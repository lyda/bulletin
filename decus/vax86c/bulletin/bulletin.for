C
C  BULLETIN.FOR, Version 10/30/86
C  Purpose: Bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Usage: Invoked by the BULLETIN command.
C  Programmer: Mark R. London
C
C  NOTES: See BULLETIN.TXT for general info.
C
	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLFILES.INC'

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE '($RMSDEF)'

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /POINT/ BULL_POINT

	COMMON /READIT/ READIT

	COMMON /PAGE/ PAGE_LENGTH

	COMMON /CTRLY/ CTRLY

	COMMON /PROMPT/ COMMAND_PROMPT
	CHARACTER*39 COMMAND_PROMPT

	COMMON /ACCESS/ READ_ONLY
	LOGICAL READ_ONLY

	COMMON /DECNET/ DECNET_PROC,ERROR_UNIT
	LOGICAL DECNET_PROC

	EXTERNAL BULLETIN_SUBCOMMANDS,LIB$GET_INPUT
	EXTERNAL BULLETIN_MAINCOMMANDS,ENABLE_CTRL_EXIT
	EXTERNAL CLI$_ABSENT,CLI$_NOCOMD

	PARAMETER PCB$M_BATCH = '4000'X
	PARAMETER PCB$M_NETWRK = '200000'X
	PARAMETER LIB$M_CLI_CTRLY = '2000000'X

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD

	CHARACTER*11 UPTIME_DATE
	CHARACTER*8 UPTIME_TIME
	CHARACTER*64 HELP_DIRECTORY

	CALL DCLEXH(%LOC(ENABLE_CTRL_EXIT))		! Declare exit handler

C
C  Check to see if CONTROL Y disabled.  If so, then never disable CONTROL Y.
C  Disabling and enabling CONTROL Y is done so that a person can not break
C  while one of the data files is opened, as that would not allow anyone
C  else to modify the files.  However, if CONTROL Y is already disabled,
C  this is not necessary, and should not be done!
C

	CALL LIB$DISABLE_CTRL(LIB$M_CLI_CTRLY,CTRLY)	! Disable CTRL-Y & -C
	CTRLY = CTRLY .AND. LIB$M_CLI_CTRLY
	CALL GETPRIV			! Check privileges
	IF (.NOT.SETPRV_PRIV()) THEN	! If no SETPRV privileges...
	   CALL CHECK_PRIV_IO(ERR)	! check privileges on output I/O
	ELSE
	   ERR = 0			! Else we don't have to check them.
	END IF
	CALL LIB$ENABLE_CTRL(CTRLY,)		! Renable CTRLY-Y & -C

	IF (ERR.EQ.1) CALL EXIT			! I/O privilege error, so exit

	CALL GETUSER(USERNAME)		! Get the process's username

	CALL CLI$GET_VALUE('$LINE',COMMAND_PROMPT)
	LEN = 1
	DO WHILE (LEN.GT.0)
	   LEN = MAX(INDEX(COMMAND_PROMPT,':'),INDEX(COMMAND_PROMPT,']'))
	   IF (LEN.GT.0) THEN
	      COMMAND_PROMPT = COMMAND_PROMPT(LEN+1:)
	   ELSE
	      DO I=TRIM(COMMAND_PROMPT),1,-1
		 IF (COMMAND_PROMPT(I:I).LT.'A'.OR.
     &			COMMAND_PROMPT(I:I).GT.'Z') THEN
		    COMMAND_PROMPT = COMMAND_PROMPT(:I-1)
		 END IF
	      END DO
	   END IF
	END DO
	COMMAND_PROMPT = COMMAND_PROMPT(1:TRIM(COMMAND_PROMPT))//'> '
	IF (COMMAND_PROMPT.EQ.'RUN> ') COMMAND_PROMPT = 'BULLETIN> '

C
C  Test for /LOGIN switch.
C  NOTE: /READ has been replaced by the SET READNEW command.
C

	CALL LIB$GET_FOREIGN(INCMD)

	IER = CLI$DCL_PARSE('BULLETIN'//INCMD,BULLETIN_MAINCOMMANDS)

	READIT = 0
	LOGIT = 0
	IF (CLI$PRESENT('LOGIN')) LOGIT = 1	! Test for /LOGIN switch.

	IF (CLI$PRESENT('CLEANUP')) THEN	! Test for /CLEANUP switch
	   CALL CLI$GET_VALUE('CLEANUP',BULL_PARAMETER,LEN) ! Get folder #
	   READ (BULL_PARAMETER,'(I<LEN>)') FOLDER_NUMBER
	   CALL SELECT_FOLDER(.FALSE.,IER)	! Select folder
	   CALL CLEANUP_BULLFILE		! Cleanup empty blocks
	   CALL EXIT				! all done with cleanup
	ELSE IF (CLI$PRESENT('BBOARD')) THEN	! Test for /BBOARD switch
	   CALL BBOARD				! look for BBOARD mail
	   CALL EXIT				! all done with BBOARD
	END IF

C
C  Delete any expired bulletins (normal or shutdown ones).
C  (NOTE: If bulletin files don't exist, they get created now by
C  OPEN_FILE_SHARED.  Also, if new format has been defined for files,
C  they get converted now.  The directory file has had it's record size
C  lengthened in the past to include more info, and the bulletin file 
C  was lengthened from 80 to 81 characters to include byte which indicated
C  start of bulletin message.  However, that scheme was removed and
C  was replaced with a 128 byte record compressed format).
C

	CALL OPEN_FILE_SHARED(2)	! Open directory file
	CALL OPEN_FILE_SHARED(1)	! Open bulletin file
	CALL CLOSE_FILE(1)
	CALL READDIR(0,IER)		! Get directory header
	IF (IER.EQ.1) THEN		! Is header present?
	   IER = COMPARE_DATE(NEWEST_EXDATE,' ') ! Yes. Any expired bulls?
	   IF (IER.EQ.0) IER = COMPARE_TIME(NEWEST_EXTIME,' ')
	   IF (SHUTDOWN.GT.0) THEN	! Do shutdown bulletins exist?
	      CALL GET_UPTIME(UPTIME_DATE,UPTIME_TIME)
	      IER1 = COMPARE_DATE(SHUTDOWN_DATE,UPTIME_DATE)
	      IF (IER1.EQ.0) IER1 = COMPARE_TIME(SHUTDOWN_TIME,UPTIME_TIME)
	      IF (IER1.LE.0) SHUTDOWN = 0
	   ELSE
	      IER1 = 1
	   END IF
	   IF (IER.LE.0.OR.IER1.LE.0) THEN
	      CALL CLOSE_FILE(2)
	      CALL OPEN_FILE(2)		! Reopen without sharing
	      CALL UPDATE 		! Need to update
	   END IF
	ELSE		! If header not there, then first time running BULLETIN
	   CALL OPEN_FILE(4)		! Create user file to be able to set
	   CALL CLOSE_FILE(4)		! defaults, privileges, etc.
	END IF
	CALL CLOSE_FILE(2)

	CALL GETSTS(STS)			! Get process status word

	IF (LOGIT.GT.0) THEN			! If BULLETIN/LOGIN then
	   IF ((STS.AND.PCB$M_BATCH).GT.0) CALL EXIT	! If BATCH, exit
	END IF

	IF ((STS.AND.PCB$M_NETWRK).GT.0) THEN
	   DECNET_PROC = .TRUE.
	   ERROR_UNIT = 5
	ELSE
	   DECNET_PROC = .FALSE.
	   ERROR_UNIT = 6
	END IF

	CALL ASSIGN_TERMINAL			! Assign terminal

C
C  Get page length for the terminal.
C

	CALL GETPAGLEN(PAGE_LENGTH)

C
C  If /LOGIN, display SYSTEM bulletins and subject of non-SYSTEM bulletins.
C

	IF (LOGIT.GT.0) THEN		! Is /LOGIN present?
	   CALL LOGIN			! Display SYSTEM bulletins
	   IF (READIT.EQ.0) CALL EXIT	! If no READNEWs not set, exit
	END IF

C
C  Update user's last read bulletin date.  If new bulletins have been
C  added since the last time bulletins have been read, position bulletin
C  pointer so that next bulletin read is the first new bulletin, and
C  alert user.  If READNEW set and no new bulletins, just exit.
C

	IF (READIT.EQ.0) THEN 			! If not in READNEW mode
	   CALL UPDATE_READ(NEW_GENERAL_BULL)	! Update last read time
	   DO FOLDER_NUMBER = 1,63
	      F_POINT = FOLDER_NUMBER/32 + 1
	      IF (BTEST(NEW_FLAG(F_POINT).AND.SET_FLAG(F_POINT),
     &		FOLDER_NUMBER)) THEN
		 CALL SELECT_FOLDER(.FALSE.,IER)
		 IF (IER) THEN
		    IF (BTEST(NEW_FLAG(F_POINT),FOLDER_NUMBER)) THEN
		     WRITE (6,'('' There are new messages in folder '',
     &			A,''.'')') FOLDER(1:TRIM(FOLDER))
		    END IF
		 ELSE				! Can't select the folder
		    CALL CHANGE_FLAG(0,1)	! then clear SET_FLAG
		 END IF
	      END IF
	   END DO
	   FOLDER_NUMBER = 0
	   CALL SELECT_FOLDER(.FALSE.,IER)
	   IF (NEW_GENERAL_BULL) THEN
	      CALL FIND_NEWEST_BULL	! See if there are new messages
	      IF (BULL_POINT.NE.-1) THEN
	       WRITE(6,'('' Type READ to read new general messages.'')')
	      ELSE
	       BULL_POINT = 0
	      END IF
	   END IF
	ELSE				! READNEW mode.
	   READ_DONE = -1
	   DO FOLDER_NUMBER = 0,63
	      F_POINT = FOLDER_NUMBER/32 + 1
	      IF (BTEST(NEW_FLAG(F_POINT).AND.SET_FLAG(F_POINT),
     &		FOLDER_NUMBER)) THEN
		 CALL SELECT_FOLDER(.FALSE.,IER)
		 IF (IER) THEN
		    IF (FOLDER_NUMBER.GT.0) CALL LOGIN_FOLDER
	            IF (BULL_POINT.NE.-1) THEN
		       SAVE_BULL_POINT = BULL_POINT
		       CALL READNEW
		       IF (BULL_POINT.NE.SAVE_BULL_POINT
     &			 .AND.READ_DONE.EQ.-1) READ_DONE = FOLDER_NUMBER
		    END IF
		 ELSE				! Can't select the folder
		    CALL CHANGE_FLAG(0,1)	! then clear SET_FLAG
		 END IF
	      END IF
	   END DO
	   IF (READ_DONE.GE.0) THEN
	      IF (READ_DONE.EQ.0) CALL UPDATE_READ(NEW_GENERAL_BULL)
	      DO FOLDER_NUMBER = 0,63
	         F_POINT = FOLDER_NUMBER/32 + 1
	         IF (BTEST(NEW_FLAG(F_POINT).AND.SET_FLAG(F_POINT),
     &		   FOLDER_NUMBER)) THEN
	 	   CALL CHANGE_FLAG(0,2)		! Clear NEW_FLAG
		 END IF
	      END DO
	   END IF
	   CALL EXIT
	END IF

C
C  The MAIN loop for processing bulletin commands.
C

	DIR_COUNT = 0	! # directory entry to continue bulletin read from
	READ_COUNT = 0	! # block that bulletin READ is to continue from
	FOLDER_COUNT = 0 ! # folder entry to continue SHOW/ALL folder from

	MAIL_STATUS = 1

	DO WHILE (1)

	   IF (MAIL_STATUS) THEN
	      CALL GET_INPUT_PROMPT(INCMD,IER,
     &		COMMAND_PROMPT(:TRIM(COMMAND_PROMPT)+1))
	   ELSE
	      CALL GET_INPUT_PROMPT(INCMD,IER,
     &		CHAR(10)//COMMAND_PROMPT(:TRIM(COMMAND_PROMPT)+1))
	      MAIL_STATUS = 0
	   END IF

	   IF (IER.EQ.-2) THEN
	      IER = RMS$_EOF
	   ELSE IF (IER.LE.0) THEN
	      IER = %LOC(CLI$_NOCOMD)
	   ELSE
	      DO WHILE (IER.GT.0.AND.INCMD(1:1).EQ.' ')
		 INCMD = INCMD(2:IER)
		 IER = IER - 1
	      END DO
	      DO WHILE (IER.GT.0.AND.
     &			INCMD(IER:IER).GE.'0'.AND.INCMD(IER:IER).LE.'9')
		 IER = IER - 1
	      END DO
	      IF (IER.EQ.0) INCMD = 'READ '//INCMD
	      IER=CLI$DCL_PARSE(INCMD,BULLETIN_SUBCOMMANDS,LIB$GET_INPUT)
	   END IF

	   IF (IER.EQ.RMS$_EOF) THEN
	      GO TO 999	! If no command, exit
	   ELSE IF (IER.EQ.%LOC(CLI$_NOCOMD)) THEN  ! If just RETURN entered
	      LEN_P = 0			! Indicate no parameter in command
	      IF (DIR_COUNT.GT.0) THEN		! If still more dir entries
		 CALL DIRECTORY(DIR_COUNT)	! continue outputting them
	      ELSE IF (FOLDER_COUNT.GT.0) THEN	! If more folder entries
		 CALL DIRECTORY_FOLDERS(FOLDER_COUNT) ! continue outputting them
	      ELSE				! Else try to read next bulletin
		 CALL READ(READ_COUNT,BULL_POINT+1)  ! or finish old one
	      END IF
	      GO TO 100				! Loop to read new command
	   ELSE IF (.NOT.IER) THEN		! If command has error
	      GO TO 100				! ask for new command
	   END IF

	   DIR_COUNT = 0			! Reinit display pointers
	   READ_COUNT = 0
	   FOLDER_COUNT = 0

	   CALL CLI$GET_VALUE('$VERB',INCMD)	! Get the VERB command
	   IF (READ_ONLY.AND.(INCMD(1:3).EQ.'ADD'.OR.INCMD(1:3).EQ.'DEL'
     &	     .OR.INCMD(1:3).EQ.'REP')) THEN	! FOLDER can only be read?
	     WRITE (6,'('' ERROR: Access to folder limited to reading.'')')
	   ELSE IF (INCMD(1:3).EQ.'ADD') THEN	! ADD bulletin command?
	     CALL ADD				! Go add bulletin
	   ELSE IF (INCMD(1:4).EQ.'BACK') THEN	! BACK command?
	     IF (BULL_POINT.LE.1) THEN
	        WRITE(6,1060)
	     ELSE
	        CALL READ(READ_COUNT,BULL_POINT-1)  ! Try to read previous bull
	     END IF
	   ELSE IF (INCMD(1:4).EQ.'COPY') THEN		! COPY command?
	     CALL MOVE(.FALSE.)
	   ELSE IF (INCMD(1:4).EQ.'CREA') THEN		! CREATE command?
	     CALL CREATE_FOLDER			! Go create the folder
	   ELSE IF (INCMD(1:4).EQ.'CURR') THEN		! CURRENT command?
	     READ_COUNT = -1		! Reread current message from beginning.
	     CALL READ(READ_COUNT,BULL_POINT)
	   ELSE IF (INCMD(1:4).EQ.'DELE') THEN 	! DELETE command?
	     CALL DELETE			! Go delete bulletin
	   ELSE IF (INCMD(1:4).EQ.'DIRE') THEN		! DIRECTORY command?
	     IF (CLI$PRESENT('FOLDER')) THEN		! /FOLDER specified?
		CALL DIRECTORY_FOLDERS(FOLDER_COUNT)	! Show all folders
	     ELSE
	        CALL DIRECTORY(DIR_COUNT)		! Show messages
	     END IF
	   ELSE IF (INCMD(1:4).EQ.'EXIT') THEN		! EXIT command?
	     GO TO 999				! Exit from program
	   ELSE IF (INCMD(1:4).EQ.'FILE') THEN		! FILE command?
	     CALL FILE				! Copy bulletin to file
	   ELSE IF (INCMD(1:4).EQ.'HELP') THEN		! HELP command?
	     IER = LIB$SYS_TRNLOG('BULL$HELP',LEN,HELP_DIRECTORY)
	     IF (IER.NE.1) THEN
		HELP_DIRECTORY = 'SYS$HELP:'
		LEN = 9
	     END IF
	     CALL HELP(HELP_DIRECTORY(1:LEN)//'BULL.HLB')	! Get help
	   ELSE IF (INCMD(1:4).EQ.'LAST') THEN		! LAST command?
	     READ_COUNT = -1
	     BULL_READ = 99999
	     CALL READ(READ_COUNT,BULL_READ)
	   ELSE IF (INCMD(1:4).EQ.'MAIL') THEN		! MAIL command?
	     CALL MAIL(MAIL_STATUS)
	   ELSE IF (INCMD(1:4).EQ.'MOVE') THEN		! MOVE command?
	     CALL MOVE(.TRUE.)
	   ELSE IF (INCMD(1:4).EQ.'NEXT') THEN		! NEXT command?
	     CALL READ(READ_COUNT,BULL_POINT+1)		! Read next bulletin
	   ELSE IF (INCMD(1:4).EQ.'PRIN') THEN		! PRINT command?
	     CALL PRINT				! Printout bulletin
	   ELSE IF (INCMD(1:4).EQ.'READ') THEN		! READ command?
	     IER = CLI$GET_VALUE('BULLETIN_NUMBER',BULL_PARAMETER,LEN_P)
	     IF (IER.NE.%LOC(CLI$_ABSENT)) THEN		! Bulletin specified?
	        DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER) BULL_READ	! Yes
		READ_COUNT = -1
		CALL READ(READ_COUNT,BULL_READ)
	     ELSE
		CALL READ(READ_COUNT,BULL_POINT+1)
	     END IF
	   ELSE IF (INCMD(1:3).EQ.'REM') THEN		! REMOVE command?
	     CALL REMOVE_FOLDER
	   ELSE IF (INCMD(1:4).EQ.'REPL') THEN		! REPLACE command?
	     CALL REPLACE				! Replace old bulletin
	   ELSE IF (INCMD(1:3).EQ.'SEA') THEN		! SEARCH command?
	     CALL SEARCH(READ_COUNT)
	   ELSE IF (INCMD(1:3).EQ.'SEL') THEN		! SELECT command?
	     CALL SELECT_FOLDER(.TRUE.,IER)
	   ELSE IF (INCMD(1:3).EQ.'SET') THEN		! SET command?
	     CALL CLI$GET_VALUE('SET_PARAM1',BULL_PARAMETER)
	     IF (BULL_PARAMETER(1:1).EQ.'B') THEN		! SET BBOARD?
		CALL SET_BBOARD(.TRUE.)
	     ELSE IF (BULL_PARAMETER(1:3).EQ.'NOB') THEN	! SET NOBBOARD?
		CALL SET_BBOARD(.FALSE.)
	     ELSE IF (BULL_PARAMETER(1:3).EQ.'NOT') THEN	! SET NOTIFY?
		IF (CLI$PRESENT('DEFAULT')) THEN
		   CALL SET_FOLDER_NOTIFY_READNEW(1,-1)
		ELSE
		   CALL CHANGE_FLAG(1,3)
		END IF
	     ELSE IF (BULL_PARAMETER(1:3).EQ.'NON') THEN	! SET NONOTIFY?
		IF (CLI$PRESENT('DEFAULT')) THEN
		   CALL SET_FOLDER_NOTIFY_READNEW(0,-1)
		ELSE
		   CALL CHANGE_FLAG(0,3)
		END IF
	     ELSE IF (BULL_PARAMETER(1:1).EQ.'R') THEN		! SET READNEW?
		IF (CLI$PRESENT('DEFAULT')) THEN
		   CALL SET_FOLDER_NOTIFY_READNEW(-1,1)
		ELSE
		   CALL CHANGE_FLAG(1,1)
		END IF
	     ELSE IF (BULL_PARAMETER(1:3).EQ.'NOR') THEN	! SET NOREADNEW?
		IF (CLI$PRESENT('DEFAULT')) THEN
		   CALL SET_FOLDER_NOTIFY_READNEW(-1,0)
		ELSE
		   CALL CHANGE_FLAG(0,1)
		END IF
	     ELSE IF (BULL_PARAMETER(1:1).EQ.'A') THEN		! SET ACCESS?
		CALL SET_ACCESS(.TRUE.)
	     ELSE IF (BULL_PARAMETER(1:3).EQ.'NOA') THEN	! SET NOACCESS?
		CALL SET_ACCESS(.FALSE.)
	     ELSE IF (BULL_PARAMETER(1:1).EQ.'F') THEN		! SET FOLDER?
		CALL SELECT_FOLDER(.TRUE.,IER)
	     ELSE IF (BULL_PARAMETER(1:1).EQ.'P') THEN	! SET PRIVILEGES?
		CALL SET_PRIV
	     END IF
	   ELSE IF (INCMD(1:4).EQ.'SHOW') THEN		! SHOW command?
	     CALL CLI$GET_VALUE('SHOW_PARAM1',BULL_PARAMETER,LEN_P)
	     IF (BULL_PARAMETER(1:1).EQ.'F') THEN	! SHOW FOLDER?
	        CALL SHOW_FOLDER
	     ELSE IF (BULL_PARAMETER(1:1).EQ.'N') THEN	! SHOW NOTIFY?
		CALL SHOW_NOTIFY
	     ELSE IF (BULL_PARAMETER(1:1).EQ.'P') THEN	! SHOW PRIVILEGES?
		CALL SHOW_PRIV
	     ELSE IF (BULL_PARAMETER(1:1).EQ.'R') THEN	! SHOW READNEW?
		CALL SHOW_READNEW
	     END IF
	   END IF

100	   CONTINUE

	END DO

999	DO FOLDER_NUMBER = 0,63
	   F_POINT = FOLDER_NUMBER/32 + 1
	   IF (BTEST(NEW_FLAG(F_POINT).AND.SET_FLAG(F_POINT),
     &		   FOLDER_NUMBER)) THEN
	      CALL CHANGE_FLAG(0,2)		! Clear NEW_FLAG
	   END IF
	END DO

	CALL EXIT


1010	FORMAT(Q,A)
1060	FORMAT(' ERROR: There are no more messages.')

	END




	SUBROUTINE ADD
C
C  SUBROUTINE ADD
C
C  FUNCTION: Adds bulletin to bulletin file.
C
	IMPLICIT INTEGER (A - Z)

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /NODE_INFO/ NODES,LOCAL_NODE_FOUND,NODE_NUM,
     &				NODE_ERROR,POINT_NODE
	CHARACTER*32 NODES(10)
	LOGICAL LOCAL_NODE_FOUND,NODE_ERROR

	COMMON /DECNET/ DECNET_PROC,ERROR_UNIT
	LOGICAL DECNET_PROC

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE '($SSDEF)'

	INCLUDE '($BRKDEF)'

	CHARACTER INEXDATE*11,INEXTIME*8
	CHARACTER*80 INDESCRIP,INPUT

	INTEGER TIMADR(2)

C
C  The largest message that can be broadcasted is dependent on system
C  and user quotas.  The following limit is 12 lines of ( 80 characters +
C  CR/LF ) + 2 bells.  This should be more than enough room, as broadcasts
C  shouldn't be too large anyway.
C

	PARAMETER BRDCST_LIMIT = 82*12 + 2
	CHARACTER*(BRDCST_LIMIT) BROADO
	CHARACTER*1 CR/13/,LF/10/,BELL/7/

	CHARACTER*80 MAILEDIT,INLINE 
    	CHARACTER PASSWORD*31,DEFAULT_USER*12

	EXTERNAL CLI$_ABSENTE

	CALL DISABLE_CTRL		! Disable CTRL-Y & -C

	ALLOW = SETPRV_PRIV()

	IER = CLI$GET_VALUE('FILESPEC',BULL_PARAMETER,LEN_P)
	IF (IER.NE.%LOC(CLI$_ABSENT)) THEN'
 	   IF (.NOT.ALLOW) THEN		! If no SETPRV privileges, remove SYSPRV
	      CALL DISABLE_PRIVS	! privileges when trying toM
	   END IF					! create new file.M
	   OPEN (UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),STATUS='OLD',READONLY,
     &		 SHARED,ERR=920,FORM='FORMATTED')	! Try opening the file
	   CALL ENABLE_PRIVS	! Reset SYSPRV privileges
	END IF 

	IER = CLI$GET_VALUE('USERNAME',DEFAULT_USER) 
	IF (.NOT.IER) DEFAULT_USER = USERNAME
	IF (DECNET_PROC) THEN		! Running via DECNET?S
	   USERNAME = DEFAULT_USERL
	   CALL CONFIRM_PRIV(USERNAME,ALLOW)T
	END IF$

	IF (FOLDER_SET.AND.			! If folder set and
     &	   (CLI$PRESENT('SYSTEM').OR.		! Is /SYSTEM switch present?
     &	    CLI$PRESENT('BROADCAST').OR.	! Is /BROADCAST switch present?C
     &	    CLI$PRESENT('SHUTDOWN').OR.		! Is /SHUTDOWN switch present?
     &	    CLI$PRESENT('NODES'))) THEN		! Decnet nodes specified? 
	   WRITE (6,'('' ERROR: Invalid parameter used with folder set.'')') 
	   RETURN
	END IF 

	IF (CLI$PRESENT('SYSTEM')) THEN		! Is /SYSTEM switch present?
	   IF (.NOT.ALLOW) THEN			! If no privileges 
	      WRITE(ERROR_UNIT,1070)		! Tell user
	      RETURN				! and abort
	   END IF
	   SYSTEM = 1				! Set system bit
	ELSEn
	   SYSTEM = 0				! Clear system bit
	END IF 

	IF (CLI$PRESENT('BROADCAST')) THEN	! Is /BROADCAST switch present?=
	   IF (.NOT.(ALLOW.OR.OPER_PRIV())) THEN	! If no privileges
	      WRITE(ERROR_UNIT,1080)		! Tell user
	      RETURN				! and abort
	   END IF
	END IFh

	IF (CLI$PRESENT('PERMANENT')) THEN	! Is /PERMANENT switch present?e
	   IF (.NOT.ALLOW.AND..NOT.FOLDER_SET) THEN	! If no privilegesb
	      WRITE(ERROR_UNIT,1081)		! Tell user
	      RETURN				! and abort
	   ELSE
	      SYSTEM = SYSTEM.OR.2		! Set permanent bit
	      INEXDATE = '5-NOV-2000'
	      INEXTIME = '00:00:00'
	   END IF
	END IF

	IF (CLI$PRESENT('SHUTDOWN')) THEN	! Is /SHUTDOWN switch present? 
	   IF (.NOT.ALLOW) THEN			! If no privilegesO
	      WRITE(ERROR_UNIT,1082)		! Tell user
	      RETURN				! and abort
	   ELSE
	      SYSTEM = SYSTEM.OR.4		! Set shutdown bitP
	      INEXDATE = '5-NOV-2000'
	      INEXTIME = '00:00:00'
	   END IF
	END IF 

	CALL GET_NODE_INFO

	IF (NODE_ERROR) GO TO 940

	IF (SYSTEM.LE.1) THEN			! Not permanent or shutdown
	   CALL GET_EXPIRED(INPUT,IER)T
	   IF (.NOT.IER) GO TO 910s
	   INEXDATE = INPUT(1:11)
	   INEXTIME = INPUT(13:20) 
	END IFE

	LENDES = 54
	DO WHILE (LENDES.GT.53)			! Do until valid descriptionE
	   WRITE(6,1050)			! Request header for bulletinI
	   CALL GET_LINE(INDESCRIP,LENDES)	! Get input line
	   IF (LENDES.LE.0) GO TO 910
	   IF (LENDES.GT.53) THEN		! If too many characters
	      WRITE(6,1060)			! tell user
	      WRITE(6,2020) INDESCRIP(1:53)	! Show how much would fit
	   END IF
	END DO_

CR
C  If file specified in ADD command, read file to obtain bulletin.
C  Else, read the bulletin from the terminal.L
CI
		
	ICOUNT = 0				! Line count for bulletin

	IF (CLI$PRESENT('EDIT')) THEN		! If /EDIT specified, then
	   LEN = 0 
	   IER = LIB$SYS_TRNLOG('MAIL$EDIT',LEN,MAILEDIT)
	   IF (IER.NE.SS$_NORMAL) MAILEDIT = 'SYS$SYSTEM:MAILEDIT'a
	   IF (LEN_P.EQ.0) THEN			! If no file param specifiedx
	      CALL LIB$SPAWN('$@'//MAILEDIT//' "" SYS$LOGIN:BULL.SCR')f
	      OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='OLD',.
     &		 DISPOSE='DELETE',ERR=920,FORM='FORMATTED')n
	      LEN_P = 1
	   ELSE
	      CLOSE (UNIT=3)u
	      CALL LIB$SPAWN('$@'//MAILEDIT//' '//BULL_PARAMETER(1:LEN_P)
     &			//' SYS$LOGIN:BULL.SCR')u
	      OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='OLD',s
     &		 DISPOSE='DELETE',ERR=920,FORM='FORMATTED')
	   END IF
	END IF_

	IF (LEN_P.GT.0) THEN			! If file param in ADD command
	   DO WHILE(1)				! Read until end of file to
	      READ (3,'(Q,A)',END=10) LEN,INPUT	! get record countE
	      IF (LEN.GT.80) GO TO 950 
	      IF (INDEX(INPUT,CHAR(9)).GT.0) THEN
	         EXTRA = 0 
	         DO I=1,LEN
	 	    IF (INPUT(I:I).EQ.CHAR(9)) THEN
		       EXTRA = EXTRA + 8 - MOD(I+EXTRA,8)x
		    END IF
	         END DO
		 IF (LEN+EXTRA.GT.80) GO TO 950 
	      END IFS
	      ICOUNT = ICOUNT + 1 + MIN(LEN,80)
	      IF (LEN.EQ.0) ICOUNT = ICOUNT + 1	! COPY_BULL writes line with.
	   END DO				! 1 space for blank line
	ELSE					! If no input file
	   OPEN (UNIT=3,STATUS='SCRATCH',FILE='SYS$LOGIN:BULL.SCR',
     &		FORM='FORMATTED')		! Sratch file to save bulletinP
	   WRITE (6,1000)		! Request bulletin input from terminal
	   LEN = 81				! Length of input line
	   DO WHILE (LEN.GE.0)			! Input until no more inputt
	      CALL GET_LINE(INPUT,LEN)		! Get input lineg
	      IF (LEN.GT.80) THEN		! Input line too longE
		 WRITE(6,'('' ERROR: Input line length > 80.  Reinput:'')')	
	      ELSE IF (LEN.GE.0) THEN		! If good input line entered
		 ICOUNT = ICOUNT + 1 + LEN	! Increment record count.
		 IF (LEN.EQ.0) ICOUNT = ICOUNT + 1
		 WRITE(3,2010) INPUT(1:LEN)	! Save line in scratch file 
	      END IFR
	   END DO
	   IF (LEN.EQ.-1) GO TO 910		! CTRL_C entered, error out
10	   IF (ICOUNT.EQ.0) GO TO 910		! No lines entered, error outA
	ENDIF

	REWIND (UNIT=3)

	IF (NODE_NUM.GT.0) THEN
	   INLINE = 'ADD'
	   IF (CLI$PRESENT('SYSTEM'))
     &	      INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)//'/SYSTEM'E
	   IF (CLI$PRESENT('BROADCAST'))C
     &	      INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)//'/BROADCAST'
	   IF (CLI$PRESENT('PERMANENT'))u
     &	      INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)//'/PERMANENT'
	   IF (CLI$PRESENT('SHUTDOWN'))
     &	      INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)//'/SHUTDOWN'f
	   IF (CLI$PRESENT('BELL'))
     &	      INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)//'/BELL' 

	   LEN_INLINE = STR$POSITION(INLINE,' ') - 1d

	   DO POINT_NODE=1,NODE_NUM	   	! Write out command to nodes=
	      INLINE = INLINE(1:LEN_INLINE)
	      SEMI = INDEX(NODES(POINT_NODE),'::')	! Look for semicolons)
	      LEN = TRIM(NODES(POINT_NODE))		! Length of node name
	      IF (SEMI.GT.0) THEN			! Are semicolon found?N
	         IF (LEN.GT.SEMI+1) THEN		! Is username found?'
	            TEMP_USER = NODES(POINT_NODE)(SEMI+2:)	! YesF
	            LEN = SEMI - 1			! Remove semicolonsC
	         ELSE					! No username found...E
		    TEMP_USER = DEFAULT_USER		! Set user to default 
	            LEN = SEMI - 1			! Remove semicolonsE
		    SEMI = 0				! Indicate no username
	         END IF
	      ELSE					! No semicolons presentO
	         TEMP_USER = DEFAULT_USER		! Set user to defaultw
	      END IFe
	      IER = 1
	      DO WHILE ((INLINE.NE.'ADD'.OR.SEMI.GT.0.OR.
     &			CLI$PRESENT('USERNAME')).AND.IER.NE.0) 
	         WRITE(6,'('' Enter password for node '',2A)')_
     &			NODES(POINT_NODE),CHAR(10)F
		 CALL GET_INPUT_NOECHO(PASSWORD)
		 IF (STR$POSITION(PASSWORD,CHAR(13)).LE.1) GO TO 910
	         OPEN (UNIT=10+NODE_NUM,NAME=NODES(POINT_NODE)(:LEN)//L
     &		   '"'//TEMP_USER(1:TRIM(TEMP_USER))//' '//
     &		   PASSWORD(1:STR$POSITION(PASSWORD,CHAR(13))-1)//'"::',
     &		   TYPE='SCRATCH',IOSTAT=IER)I
		 CLOSE (UNIT=10+NODE_NUM)E
		 IF (IER.NE.0) THENR
		    WRITE (6,'('' ERROR: Password is invalid.'')')
		 END IF 
	      END DOL
	      INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)
     &					//'/USERNAME='//TEMP_USER
	      WRITE (POINT_NODE+9,'(A)',ERR=940) INLINE
	      IF (SYSTEM.LE.1)	! If not permanent or shutdown specify dateO
     &		WRITE (POINT_NODE+9,'(A)',ERR=940) INEXDATE//' '//INEXTIME
	      WRITE (POINT_NODE+9,'(A)',ERR=940) INDESCRIP(1:LENDES)A
	      IER = 0
	      DO WHILE (IER.EQ.0)
	         READ (3,'(Q,A)',IOSTAT=IER) LEN,INPUT
		 LEN = MIN(LEN,80)
		 IF (IER.EQ.0) THENs
		    WRITE (POINT_NODE+9,'(A)',ERR=940) INPUT(1:LEN)o
		 END IF 
	      END DOi
	      WRITE (POINT_NODE+9,'(A)',ERR=940) CHAR(26)
	      READ (POINT_NODE+9,'(A)',ERR=940,END=940) INPUT
	      IF (INPUT.EQ.'END') THENr
	         WRITE (6,'('' Message successfully sent to node '',A)'))
     &				NODES(POINT_NODE)_
	      ELSEI
	         WRITE (6,'('' Error while sending message to node '',A)') 
     &				NODES(POINT_NODE)D
		 WRITE (6,'(A)') INPUT
		 GO TO 940
	      END IFP
	      REWIND (UNIT=3)
	   END DO
	END IFF

	IF (.NOT.LOCAL_NODE_FOUND) GO TO 95	! Was local node specified?

C.
C  Add bulletin to bulletin file and directory entry for to directory file..
C.

	CALL OPEN_FILE(2)			! Prepare to add dir entryR

	DESCRIP=INDESCRIP(1:LENDES)		! Description header
	EXDATE=INEXDATE				! Expiration dateD
	EXTIME=INEXTIME
	LENGTH = (ICOUNT+127)/128		! Number of recordsI
	FROM = USERNAME				! Username

	CALL OPEN_FILE(1)			! Prepare to add bulletin

	CALL READDIR(0,IER)			! Get NBLOCK 
	IF (IER.EQ.0) NBLOCK = 0		! If new file, NBLOCK is 0c

	CALL COPY_BULL(3,1,NBLOCK+1,IER)	! Add the new bulletin
	IF (IER.NE.0) GO TO 930			! Error in creating bulletino

	CALL CLOSE_FILE(1)			! Finished adding bulletin

	CALL ADD_ENTRY				! Add the new directory entry

	CALL CLOSE_FILE(2)			! Totally finished with addF

CD
C  Broadcast the bulletin if requested.i
C

	IF (CLI$PRESENT('BROADCAST')) THEN	! Should we broadcast the bull? 
	   REWIND (UNIT=3)			! Yes, rewind the input file
	   IF (CLI$PRESENT('BELL')) THEN	! Include BELL in message?
	      BROAD(1:36) =			! Say who the bulletin is from
     &		BELL//BELL//CR//LF//LF//'NEW BULLETIN FROM: '//FROMG
	      START = 37			! Start adding next line here 
	   ELSE
	      BROAD(1:34) =			! Say who the bulletin is from
     &		CR//LF//LF//'NEW BULLETIN FROM: '//FROMV
	      START = 35			! Start adding next line hereL
	   END IF
	   NBLANK = 0
	   END = 0E
	   DO WHILE (ICOUNT.GT.0) 		! Stuff bulletin into stringE
	      READ(3,'(Q,A)') LEN,INPUT		! Read input lines
	      ICOUNT = ICOUNT - LEN - 1
	      IF (LEN.EQ.0) THEN.
		 NBLANK = NBLANK + 1	! Count number of blank lines
		 ICOUNT = ICOUNT - 1	! ICOUNT counts blank line as one space
	      ELSE		! Ignore blank liness at start or end of messageI
		 IF (NBLANK.GT.0.AND.END.GT.0) THENE
	            END = START + NBLANK*2	! Check how long string will beF
	            IF (END.GT.BRDCST_LIMIT) GO TO 90	! String too long?A
		    DO I=1,NBLANK 
	               BROAD(START:START+1) = CR//LFo
		       START = START + 2
		    END DO
		 END IFl
		 NBLANK = 0I
	         END = START + LEN - 1 + 2	! Check how long string will be=
	         IF (END.GT.BRDCST_LIMIT) GO TO 90	! String too long?
	         BROAD(START:END) = CR//LF//INPUT(1:LEN)! Else add new inputT
	         START = END + 1			! Reset pointeru
	      END IFE
	   END DO
90	   IF (CLI$PRESENT('ALL')) THEN		! Should we broadcast to ALL?F
	      CALL SYS$BRKTHRUs
     &		(,BROAD(1:START-1)//CR,,%VAL(BRK$C_ALLTERMS),,,,,,,)
	   ELSE 				! Else just broadcast to users.
	      CALL SYS$BRKTHRUe
     &		(,BROAD(1:START-1)//CR,,%VAL(BRK$C_ALLUSERS),,,,,,,)
	   END IF
	END IFO

95	CLOSE (UNIT=3)			! Close the input file
	IF (DECNET_PROC) WRITE(5,'(''END'')') ! DECNET operation worked

100	CALL ENABLE_CTRL		! Enable CTRL-Y & -C
	DO I=10,NODE_NUM+9L
	   CLOSE (UNIT=I)
	END DOY
	RETURN'

910	WRITE(ERROR_UNIT,1010)
	CLOSE (UNIT=3,ERR=100)N
	GOTO 100R

920	WRITE(6,1020)
	CALL ENABLE_PRIVS		! Reset SYSPRV privilegesD
	GOTO 100L

930	WRITE (ERROR_UNIT,1025) 
	CALL CLOSE_FILE(1).
	CALL CLOSE_FILE(2)S
	CLOSE (UNIT=3) 
	GO TO 100

940	WRITE (6,1015) NODES(POINT_NODE)
	CLOSE (UNIT=3)T
	GO TO 100

950	WRITE (6,1030)
	CLOSE (UNIT=3)E
	GO TO 100

1000	FORMAT (' Enter message: End with ctrl-z, cancel with ctrl-c'))
1010	FORMAT (' No message was added.')
1015	FORMAT (' ERROR: Unable to reach node ',A)	
1020	FORMAT (' ERROR: Unable to open specified file.')
1025	FORMAT (' ERROR: Unable to add message to file.')
1030	FORMAT (' ERROR: Line length in file exceeds 80 characters.')
1050	FORMAT (' Enter description header.  Limit header to 53
     & characters.')
1060	FORMAT (' ERROR: Header > 53 characters. Header would be%
     & truncated to:')
1070	FORMAT (' ERROR: SETPRV privileges are needed for system_
     & messages.')
1080	FORMAT (' ERROR: SETPRV privileges are needed to broadcast 
     & messages.')
1081	FORMAT (' ERROR: SETPRV privileges are needed to permanent.
     & messages.')
1082	FORMAT (' ERROR: SETPRV privileges are needed to shutdown
     & messages.')
2010	FORMAT(A)
2020	FORMAT(1X,A)A

	END


	SUBROUTINE DELETE
CL
C  SUBROUTINE DELETE
C'
C  FUNCTION:  Deletes a bulletin entry from the bulletin file.
CE
	IMPLICIT INTEGER (A - Z)H

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /NODE_INFO/ NODES,LOCAL_NODE_FOUND,NODE_NUM,
     &				NODE_ERROR,POINT_NODER
	CHARACTER*32 NODES(10)	
	LOGICAL LOCAL_NODE_FOUND,NODE_ERROR

	COMMON /DECNET/ DECNET_PROC,ERROR_UNIT.
	LOGICAL DECNET_PROC

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'L

	INCLUDE 'BULLFOLDER.INC'E

	EXTERNAL CLI$_ABSENTL

	CHARACTER ANSWER*1,REMOTE_USER*12,SUBJECT*53N

	IF (CLI$PRESENT('NODES')) THEN	! Delete messages on DECNET node? 
	   CALL DELETE_NODE		! Yes...
	   RETURN
	ELSE IF (DECNET_PROC) THEN	! Is this from remote node?
	   IER = CLI$GET_VALUE('USERNAME',REMOTE_USER)E
	   IER = CLI$GET_VALUE('SUBJECT',SUBJECT,SLEN)E
	   CALL OPEN_FILE(2))
	   BULL_DELETE = 0R
	   IER = 1F
	   DO WHILE (BULL_DELETE+1.EQ.IER)C
	      BULL_DELETE = BULL_DELETE + 1
	      CALL READDIR(BULL_DELETE,IER)
	      CALL STR$UPCASE(DESCRIP,DESCRIP))
	      IF (BULL_DELETE+1.EQ.IER.AND.REMOTE_USER.EQ.FROMU
     &		   .AND.INDEX(DESCRIP,SUBJECT(:SLEN)).GT.0) THEN
		 GO TO 50L
	      END IF,
	   END DO
	   CALL CLOSE_FILE(2)		! Specified message not found,
	   WRITE(ERROR_UNIT,1030)	! so error out.
	   RETURN
	END IFE

C)
C  Get the bulletin number to be deleted.T
CC

	IER = CLI$GET_VALUE('BULLETIN_NUMBER',BULL_PARAMETER,LEN_P)
	IF (IER.NE.%LOC(CLI$_ABSENT)) THEN	! Was bulletin specified?B
	   DECODE(LEN_P,5,BULL_PARAMETER,ERR=920) BULL_DELETE	! Yes
5	   FORMAT(I<LEN_P>) 
	ELSE IF (BULL_POINT.EQ.0) THEN	! No.  Have we just read a bulletin?
	   GO TO 910			! No, then error.E
	ELSE_
	   BULL_DELETE = BULL_POINT	! Delete the file we are reading
	END IFC

	IF (BULL_DELETE.LE.0) GO TO 920

CA
C  Check to see if specified bulletin is present, and if the userL
C  is permitted to delete the bulletin. 
CW

	CALL OPEN_FILE(2)

	CALL READDIR(BULL_DELETE,IER)	! Get info for specified bulletin

	IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?
	   WRITE(ERROR_UNIT,1030)	! If not, then error out=
	   GOTO 100
	END IFO

	IF (USERNAME.NE.FROM) THEN	! If doesn't match owner of bulletin,I
	   IF ((.NOT.SETPRV_PRIV().AND..NOT.FOLDER_SET).OR.    ! Privileges orl
     &	       (.NOT.SETPRV_PRIV().AND.USERNAME.NE.FOLDER_OWNER
     &		.AND.FOLDER_SET)) THEN ! folder owner?
	      WRITE(ERROR_UNIT,1040)	! Then error out.
	      GO TO 100
	   ELSE
	      CALL CLOSE_FILE (2)
	      IF (.NOT.DECNET_PROC) THEN 
	         WRITE (6,1050)		! Make sure user wants to delete itL
	         READ (5,'(A)',IOSTAT=IER) ANSWER
	         CALL STR$UPCASE(ANSWER,ANSWER)
	         IF (ANSWER.NE.'Y') GO TO 900
	      END IFA
	      CALL OPEN_FILE(2)
	      CALL READDIR(BULL_DELETE,IER) ! Get info for specified bulletin
	      IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?U
	         WRITE(ERROR_UNIT,1030)		! If not, then error out
	         GOTO 100
	      END IF'
	   END IF
	END IFT

CI
C  Delete the bulletin directory entry.
C

50	CALL DELETE_ENTRY(BULL_DELETE)		! Delete the directory entrya

	CALL CLEANUP_DIRFILE(BULL_DELETE)	! Reorder directory fileg

	IF ((SYSTEM.AND.4).EQ.4) THEN		! Was entry shutdown bulletin?
	   CALL READDIR(0,IER)			! Get shutdown count
	   SHUTDOWN = SHUTDOWN - 1		! Decrement shutdown countB
	END IFI

	CALL UPDATE		! Somewhat a kludgey way of updating latest1
				! bulletin and expired dates.R

	IF (BULL_DELETE.LE.BULL_POINT) BULL_POINT = BULL_POINT - 12
				! Readjust where which bulletin to read next
				! if deletion causes messages to be moved.

100	CALL CLOSE_FILE(2)
	IF (DECNET_PROC) WRITE (5,'(''END'')')R
				! Tell DECNET that delete went ok.
900	RETURN

910	WRITE(6,1010)r
	GO TO 900

920	WRITE(6,1020)C
	GO TO 900

1010	FORMAT(' ERROR: You are not reading any message.') 
1020	FORMAT(' ERROR: Specified message number has incorrect format.')R
1030	FORMAT(' ERROR: Specified message was not found.') 
1040	FORMAT(' ERROR: Specified message is not owned by you.')e
1050	FORMAT(' Message is not owned by you.',
     &	       ' Are you sure you want to delete it? ',$)

	END



	SUBROUTINE DIRECTORY(DIR_COUNT)
C 
C  SUBROUTINE DIRECTORY
C 
C  FUNCTION: Display directory of messages.
CI
	IMPLICIT INTEGER (A - Z)o

	INCLUDE 'BULLDIR.INC'

	COMMON /PAGE/ PAGE_LENGTH

	DATA SCRATCH_D1/0/?

	COMMON /POINT/ BULL_POINT

	EXTERNAL CLI$_ABSENTS

	CHARACTER START_PARAMETER*4,DATETIME*23,TODAY*11)

	CALL LIB$ERASE_PAGE(1,1)		! Clear the screenC

CE
C  Directory listing is first buffered into temporary memory storage beforev
C  being outputted to the terminal.  This is to be able to quickly close the
C  directory file, and to avoid the possibility of the user holding the screen,!
C  and thus causing the directory file to stay open.  The temporary memory
C  is structured as a linked-list queue, where SCRATCH_D1 points to the header
C  of the queue.  See BULLSUBS.FOR for more description of the queue.'
CT

	CALL INIT_QUEUE(SCRATCH_D1,BULLDIR_COM)
	SCRATCH_D = SCRATCH_D1V

	CALL OPEN_FILE_SHARED(2)		! Get directory fileI

	CALL READDIR(0,IER)			! Does directory header exist?N
	IF (IER.EQ.1) THEN			! If so, there are messagesE
	   IF (DIR_COUNT.EQ.0) THEN
	      IF (CLI$PRESENT('START')) THEN	! Start number specified?e
	         IER = CLI$GET_VALUE('START',START_PARAMETER,LEN)
	         DECODE(LEN,'(I<LEN>)',START_PARAMETER) DIR_COUNT
		 IF (DIR_COUNT.GT.NBULL) THENT
		    DIR_COUNT = NBULLI
		 ELSE IF (DIR_COUNT.LT.1) THEN
		    WRITE (6,'('' ERROR: Invalid starting message.'')')W
		    CALL CLOSE_FILE(2)
		    DIR_COUNT = 0!
		    RETURN
		 END IF 
	      ELSE IF (CLI$PRESENT('SINCE')) THEN	! Date specified?
		 IER = CLI$GET_VALUE('SINCE',DATETIME)
	   	  IF (DATETIME.EQ.'TODAY') THEN		! TODAY is the default.I
	      	     IER = SYS$ASCTIM(,TODAY,,)		! Need to get date.
		     DATETIME = TODAY//' 00:00:00.0'
		  END IF
		  TEMP_COUNT = 0
		  IER = 1t
		  DO WHILE (IER.EQ.TEMP_COUNT+1)
		   TEMP_COUNT = TEMP_COUNT + 1
		   CALL READDIR(TEMP_COUNT,IER)X
		   IF (IER.NE.TEMP_COUNT+1) THEN
		     WRITE (6,'('' No messages past specified date.'')')
		     CALL CLOSE(2)
		     RETURNs
		   ELSEr
		     DIFF = COMPARE_DATE(DATETIME(1:11),DATE) 
		     IF (DIFF.EQ.0) DIFF = COMPARE_TIME(DATETIME(13:20),TIME).
		     IF (DIFF.LE.0) THEN
			DIR_COUNT = TEMP_COUNT)
			IER = IER + 1
		     END IF2
		  END IF
		 END DOo
	      ELSEu
	         DIR_COUNT = BULL_POINT
		 IF (DIR_COUNT.EQ.0) DIR_COUNT = 1
	      END IFb
	      IF (CLI$PRESENT('SINCE')) THENt
		 SBULL = DIR_COUNT
	         EBULL = DIR_COUNT + (PAGE_LENGTH - 6) - 1
	         IF (EBULL.GE.NBULL-2) EBULL = NBULLi
	      ELSE IF (NBULL-DIR_COUNT+1.LE.PAGE_LENGTH-4) THEN
	         EBULL = NBULLF
	         SBULL = NBULL - (PAGE_LENGTH-4) + 1L
	         IF (SBULL.LT.1) SBULL = 1I
	      ELSEa
	         SBULL = DIR_COUNT$
	         EBULL = DIR_COUNT + (PAGE_LENGTH - 6) - 1 
	      END IF,
	   ELSE
	      SBULL = DIR_COUNT
	      EBULL = DIR_COUNT + (PAGE_LENGTH - 6) - 1
	      IF (EBULL.GE.NBULL-2) EBULL = NBULL
	   END IF
	   DO I=SBULL,EBULL			! Copy messages from file
	      CALL READDIR(I,IER)		! Into the queue
	      CALL WRITE_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,BULLDIR_COM)
	   END DO
	ELSE	
	   NBULL = 0E
	END IF,

	CALL CLOSE_FILE(2)			! We don't need file anymore

	IF (NBULL.EQ.0) THENa
	   WRITE (6,'('' There are no messages present.'')') 
	   RETURN
	END IFA

C'
C  Directory entries are now in queue.  Output queue entries to screen. 
C 

	SCRATCH_D = SCRATCH_D1			! Init queue pointer to header

	WRITE(6,1000)				! Write header
	DO I=SBULL,EBULLC
	   CALL READ_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,BULLDIR_COM)
	   WRITE(6,2010) I,DESCRIP,FROM,DATE(1:7)//DATE(10:11)O
	END DO

	DIR_COUNT = EBULL + 1			! Update directory counter)

	IF (DIR_COUNT.GT.NBULL) THEN		! Outputted all entries?e
	   DIR_COUNT = 0			! Yes. Set counter to 0.
	ELSE
	   WRITE(6,1010)			! Else say there are moreS
	END IFR

	RETURN'

1000	FORMAT('   #',1X,'Description',43X,'From',9X,'Date',/)t
1010	FORMAT(1X,/,' Press RETURN for more...',/)u

2000	FORMAT(A53,A12,A11)
2010	FORMAT(1X,I3,1X,A53,1X,A12,1X,A9)

	END
 E

	SUBROUTINE FILE
Cn
C  SUBROUTINE FILE
C 
C  FUNCTION:  Copies a bulletin to a file.
C
	IMPLICIT INTEGER (A - Z)	

	CHARACTER INPUT*80

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	INCLUDE 'BULLDIR.INC'

	EXTERNAL CLI$_ABSENTN

	IER = CLI$GET_VALUE('FILESPEC',BULL_PARAMETER,LEN_P)U

	IF (IER.EQ.%LOC(CLI$_ABSENT)) THEN	! If no file name was specified 
	   WRITE(6,1020)		! Write error
	   RETURN			! And returnC
	END IF 

	IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been readr
	   WRITE(6,1010)		! Write error
	   RETURN			! And returnT
	END IF

	CALL OPEN_FILE_SHARED(2)L

	CALL READDIR(BULL_POINT,IER)	! Get info for specified bulletinN

	IF (IER.NE.BULL_POINT+1) THEN	! Was bulletin found?
	   WRITE(6,1030)I
	   CALL CLOSE_FILE(2)		! If not, then error out
	   RETURN
	END IF$

	CALL CLOSE_FILE(2)

	CALL OPEN_FILE_SHARED(1)	! Open BULLETIN file

	IF (.NOT.SETPRV_PRIV()) THEN		! If no SETPRV, remove SYSPRV
	   CALL DISABLE_PRIVS			! privileges when trying to
	END IF					! create new file.

	IF (CLI$PRESENT('NEW')) THEN 
	   OPEN(UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),ERR=900,E
     &		STATUS='NEW',CARRIAGECONTROL='LIST')
	ELSEI
	   OPEN(UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),ERR=900, 
     &		STATUS='UNKNOWN',CARRIAGECONTROL='LIST',ACCESS='APPEND')
	END IF:
	CALL ENABLE_PRIVS			! Reset SYSPRV privileges

	IF (CLI$PRESENT('HEADER')) THEN		! Printout header?
	   WRITE(3,1050) DESCRIP		! Output bulletin header info
	   WRITE(3,1060) FROM,DATEe
	END IF

	LEN = 81T
	DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file
	   DO WHILE (LEN.GT.0)e
	      CALL GET_BULL(I,INPUT,LEN)	
	      IF (LEN.LT.0) THEN	
		 GO TO 90 
	      ELSE IF (LEN.GT.0) THEN
	         WRITE (3,'(A)') INPUT(1:LEN)
	      END IFn
	   END DO
	   LEN = 80
	END DO 

90	CLOSE (UNIT=3)			! Bulletin copy completeds

	WRITE(6,1040) BULL_POINT,BULL_PARAMETER(1:LEN_P)R
					! Show name of file created.D
100	CALL CLOSE_FILE(1)
	RETURNH

900	WRITE(6,1000).
	CALL ENABLE_PRIVS		! Reset BYPASS privilegesE
	GO TO 100

1000	FORMAT(' ERROR: Error in opening file.') 
1010	FORMAT(' ERROR: You have not read any bulletin.')
1020	FORMAT(' ERROR: No file name was specified.')
1030	FORMAT(' ERROR: Specified bulletin was not found.')
1040	FORMAT(' Message ',I3,' written to ',A)
1050	FORMAT(/,'Description: ',A53)
1060	FORMAT('From: ',A12,' Date: ',A11,/)$

	END




	SUBROUTINE LOGIN:
C
C  SUBROUTINE LOGINT
C,
C  FUNCTION: Alerts user of new messages upon logging in.)
CE
	IMPLICIT INTEGER (A - Z)O

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'D

	INCLUDE 'BULLFOLDER.INC'S

	COMMON /READIT/ READIT

	CHARACTER TODAY*23,INPUT*80,INREAD*1 

	COMMON /PAGE/ PAGE_LENGTH

	COMMON /POINT/ BULL_POINT

	CHARACTER LOGIN_DATE_SAVE*11,LOGIN_TIME_SAVE*8t

	LOGICAL*1 CTRL_G/7/

	DATA GEN_DIR1/0/	! General directory link list header
	DATA SYS_DIR1/0/	! System directory link list header 
	DATA SYS_BUL1/0/	! System bulletin link list header

	DATA PAGE/0/T

	DATA FIRST_WRITE/.TRUE./M
	LOGICAL FIRST_WRITE

	DIMENSION H_NEW_FLAG(2),H_SET_FLAG(2)

	CALL SYS$ASCTIM(,TODAY,,)		! Get the present time

C 
C  Find user entry in BULLUSER.DAT to update information and
C  to get the last date that messages were read.
CI

	CALL OPEN_FILE_SHARED(4)		! Open user filee

	DO WHILE (REC_LOCK(IER))d
	 READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER) TEMP_USER, 
     &	  NEWEST_DATE,NEWEST_TIME,BBOARD_DATE,BBOARD_TIME,H_SET_FLAG,
     &	  H_NEW_FLAG,NOTIFY_FLAG			! Get the header
	END DO

	IF (IER.EQ.0) THEN			! Header is present.
	   DO WHILE (REC_LOCK(IER1))A
	      READ (4,FMT=USER_FMT,KEY=USERNAME,IOSTAT=IER1) USERNAME,b
     &	       LOGIN_DATE,LOGIN_TIME,READ_DATE,READ_TIME,SET_FLAG,.
     &	       NEW_FLAG,NOTIFY_FLAG		! Find if there is an entryR
	   END DO
	   IF (IER1.EQ.0) THEN			! There is a user entry	
	      REWRITE (4,FMT=USER_FMT) USERNAME,TODAY(1:11),TODAY(13:20),
     &	       READ_DATE,READ_TIME,SET_FLAG,NEW_FLAG	! Update login date)
     &	       ,NOTIFY_FLAG
	      IF ((SET_FLAG(1).OR.SET_FLAG(2)).NE.0) READIT = 1
	   ELSE
	      READ_DATE = ' 5-NOV-1956'	! No entry, so make new one
	      READ_TIME = '11:05:56'	! Fake a read date. Set to the past.
	      READIT = 1
	      WRITE (4,FMT=USER_FMT,IOSTAT=IER) USERNAME,TODAY(1:11),
     &		TODAY(13:20),READ_DATE,READ_TIME,SET_FLAG,NEW_FLAG,	
     &		NOTIFY_FLAGt
	      IF (IER.NE.0) THEN		! Error in writing to user file
		 WRITE (6,1070)			! Tell user of the error
		 CALL CLOSE_FILE(4)		! Close the user file
		 CALL EXIT			! Go away...e
	      END IFR
	      CALL CLEANUP_LOGIN		! Good time to delete dead users1
	      DIFF = -1				! Force us to look at messages
	   END IF
	   DO WHILE (REC_LOCK(IER1))/
	    READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER1) TEMP_USER,E
     &	     NEWEST_DATE,NEWEST_TIME,BBOARD_DATE,BBOARD_TIME,H_SET_FLAG,/
     &	     H_NEW_FLAG,NOTIFY_FLAG		! Reset read back to headeri
	   END DO
	END IF 

	IF (IER.EQ.0.AND.(BBOARD_DATE.NE.TODAY(1:11).OR.! Update BBOARD mail?
     &	COMPARE_TIME(TODAY(13:20),BBOARD_TIME)/60.GT.BBOARD_UPDATE)) THEN
	   REWRITE (4,FMT=USER_FMT) TEMP_USER,NEWEST_DATE,  ! Rewrite headerN
     &	    NEWEST_TIME,TODAY(1:11),TODAY(13:20),H_SET_FLAG,H_NEW_FLAG1
     &	    ,NOTIFY_FLAGn
	   CALL CLOSE_FILE(4)
	   CALL CREATE_BBOARD_PROCESS
	ELSEe
	   CALL CLOSE_FILE(4)
	   IF (IER.NE.0) CALL EXIT	! If no header, no messages+
	END IF	

	IF (IER1.EQ.0) THEN		! Skip date comparison if new entryC
CL
C  Compare and see if messages have been added since the last time
C  that the user has logged in or used the BULLETIN facility.2
C	
	   DIFF = COMPARE_DATE(LOGIN_DATE,READ_DATE) 
	   IF (DIFF.EQ.0) DIFF = COMPARE_TIME(LOGIN_TIME,READ_TIME)
	   IF (DIFF.LT.0) THEN		! If read messages since last login,
	      LOGIN_TIME = READ_TIME	! then use the read date to compareu
	      LOGIN_DATE = READ_DATE	! with the latest bulletin dateE
	   END IF			! to see if should alert user.H

	   DIFF = COMPARE_DATE(LOGIN_DATE,NEWEST_DATE)$
	   IF (DIFF.EQ.0) DIFF = COMPARE_TIME(LOGIN_TIME,NEWEST_TIME)
	END IF 

	LOGIN_TIME_SAVE = LOGIN_TIME	! These are destroyed in UPDATE_READ
	LOGIN_DATE_SAVE = LOGIN_DATE,

	IF (DIFF.GT.0) THEN
	   BULL_POINT = -1D
	   RETURN
	END IFT

C	
C  If there are new messages, look for them in BULLDIR.DAT
C  Save all new entries in the GEN_DIR file BULLCHECK.SCR so
C  that we can close BULLDIR.DAT as soon as possible.
CD

	ENTRY LOGIN_FOLDERR

	LOGIN_TIME = LOGIN_TIME_SAVET
	LOGIN_DATE = LOGIN_DATE_SAVE	

	CALL OPEN_FILE_SHARED(2)	! Yes, so go get bulletin directoryG
	NGEN = 0			! Number of general messages
	NSYS = 0			! Number of system messagesS
	CALL READDIR(0,IER)		! Get header infoT
	CALL INIT_QUEUE(GEN_DIR1,BULLDIR_COM)
	CALL INIT_QUEUE(SYS_DIR1,BULLDIR_COM)
	GEN_DIR = GEN_DIR1G
	SYS_DIR = SYS_DIR1A
	BULL_POINT = -1
	START = 1
	REVERSE = 0
	IF (CLI$PRESENT('REVERSE')) THENe
	   REVERSE = 1)
	   START = NBULL + 1 
	   IER = START + 1'
	   DIFF = 0
	   IF (IER1.NE.0) THENp
	      START = 1
	   ELSE
	     DO WHILE (START+1.EQ.IER.AND.DIFF.LE.0))
	      START = START - 1
	      IF (START.GT.0) CALL READDIR(START,IER)
	      IF (START+1.EQ.IER) THENe
	         DIFF = COMPARE_DATE(LOGIN_DATE,DATE)
	         IF (DIFF.EQ.0) DIFF = COMPARE_TIME(LOGIN_TIME,TIME) 
	      END IFt
	     END DO
	     START = START + 1i
	   END IF
	END IFs
	DO ICOUNT1 = NBULL,START,-1
	   IF (REVERSE) THENP
	      ICOUNT = NBULL + START - ICOUNT1 
	   ELSE
	      ICOUNT = ICOUNT1R
	   END IF
	   CALL READDIR(ICOUNT,IER)
	   IF (IER1.EQ.0) THEN ! Is this a totally new user?p
				  ! No. Is bulletin system or from same user?)
	      IF (.NOT.REVERSE) THEN 
	         DIFF = COMPARE_DATE(LOGIN_DATE,DATE) ! No, so compare date
	         IF (DIFF.EQ.0) DIFF = COMPARE_TIME(LOGIN_TIME,TIME)E
	         IF (DIFF.GT.0) GO TO 100
	      END IFL
	      IF (USERNAME.NE.FROM.OR.SYSTEM) THENN
		 IF (SYSTEM) THEN		! Is it system bulletin? 
		    NSYS = NSYS + 1E
		    CALL WRITE_QUEUE(%VAL(SYS_DIR),SYS_DIR,BULLDIR_COM)T
	         ELSE
		    NGEN = NGEN + 1O
		    IF (.NOT.REVERSE.OR.BULL_POINT.EQ.-1) THEN
		       BULL_POINT = ICOUNT - 1
		    END IF
		    SYSTEM = ICOUNTU
		    CALL WRITE_QUEUE(%VAL(GEN_DIR),GEN_DIR,BULLDIR_COM)$
	         END IF
	      END IFE
	   ELSE			! Totally new user, save all messages
	      IF (SYSTEM) THENe
	         NSYS = NSYS + 1L
		 CALL WRITE_QUEUE(%VAL(SYS_DIR),SYS_DIR,BULLDIR_COM)
	      ELSEt
		 NGEN = NGEN + 1
		 SYSTEM = ICOUNT	! Save bulletin number for display 
		 CALL WRITE_QUEUE(%VAL(GEN_DIR),GEN_DIR,BULLDIR_COM)
	      END IF
	   END IF
	END DO
100	CALL CLOSE_FILE(2)
	IF (FOLDER_SET) NSYS = 0
C 
C  Review new directory entries.  If there are system messages,,
C  copy the system bulletin into GEN_DIR file BULLSYS.SCR for outputting
C  to the terminal.  If there are simple messages, just output the
C  header information.
C 
	IF (NGEN.EQ.0.AND.NSYS.EQ.0) RETURN

	IF (NSYS.GT.0) THEN		! Are there any system messages?
	   IF (FIRST_WRITE) THEN 
	      PAGE = 4		! Don't erase MAIL/PASSWORD notifiese
	      FIRST_WRITE = .FALSE.	! if this is first write to screen.
	   END IF
	   WRITE (6,1026) CTRL_G	! Yep...
	   PAGE = PAGE + 1a
	   CTRL_G = 0		! Don't ring bell for non-system bulls
	   CALL OPEN_FILE_SHARED(1)
	   CALL INIT_QUEUE(SYS_BUL1,INPUT)L
	   SYS_BUL = SYS_BUL1
	   SYS_DIR = SYS_DIR1
	   DO J=1,NSYS 
	      CALL READ_QUEUE(%VAL(SYS_DIR),SYS_DIR,BULLDIR_COM)I
 	      INPUT = ' 'w
	      CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
	      LEN = 81C
	      DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin to SYS_BUL link liste
		 DO WHILE (LEN.GT.0)
		    CALL GET_BULL(I,INPUT,LEN)
		    IF (LEN.LT.0) THEN
		       CALL CLOSE_FILE(1)f
		       RETURNI
		    ELSE IF (LEN.GT.0) THEN 
		       CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
		    END IF
		 END DOO
		 LEN = 80I
	      END DON
	   END DO
	   CALL CLOSE_FILE(1)
	   SYS_BUL = SYS_BUL1
	   DO WHILE (SYS_BUL.NE.0)	! Write out the system messagesl
	      CALL READ_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)A
	      IF (SYS_BUL.NE.0) THEND
		 IF (PAGE.EQ.PAGE_LENGTH-2) THEN	! If at end of screen
		    WRITE(6,1080)	! Ask for input to proceed to next page 
		    CALL GET_INPUT_NOECHO(INREAD)	! Get terminal input
	            CALL LIB$ERASE_PAGE(1,1)		! Clear the screeni
		    WRITE(6,1065) INPUT(1:TRIM(INPUT))
		    PAGE = 1
		 ELSEU
		    WRITE(6,1060) INPUT(1:TRIM(INPUT))
		    PAGE = PAGE + 1 
		 END IF
	      END IFN
	   END DO
	   IF (NGEN.EQ.0) THENL
	      WRITE(6,'(A)')		! Write delimiting blank line
	   END IF
	   PAGE = PAGE + 1 
	END IFf
	GEN_DIR = GEN_DIR1T
	IF (NGEN.GT.0) THEN		! Are there new non-system messages?
	   LENF = TRIM(FOLDER) 
	   S1 = (80-13-LENF)/2 
	   S2 = 80-S1-13-LENF
	   IF (PAGE+5+NGEN.GT.PAGE_LENGTH.AND.PAGE.GT.0) THEN
	      WRITE(6,1080)		! Ask for input to proceed to next pageL
	      CALL GET_INPUT_NOECHO(INREAD)
	      CALL LIB$ERASE_PAGE(1,1)	! Clear the screen
	      WRITE(6,1028) 'New '//FOLDER(1:LENF)//' messages',CTRL_GO
	      PAGE = 1	
	   ELSE
	      IF (FIRST_WRITE) THEN
		 PAGE = 4		  ! Don't erase MAIL/PASSWORD notifiesl
	         FIRST_WRITE = .FALSE. ! if this is first write to screen.L
	      END IFI
	      WRITE(6,1027) 'New '//FOLDER(1:LENF)//' messages',CTRL_G 
	      PAGE = PAGE + 1
	   END IF
	   WRITE(6,1020)
	   WRITE(6,1025)(
	   PAGE = PAGE + 2 
	   DO I=1,NGEN'
	      CALL READ_QUEUE(%VAL(GEN_DIR),GEN_DIR,BULLDIR_COM)
	      IF (PAGE.EQ.PAGE_LENGTH-2) THEN	! If at end of screen
		 WRITE(6,1080)	! Ask for input to proceed to next page
		 CALL GET_INPUT_NOECHO(INREAD)
	         CALL LIB$ERASE_PAGE(1,1)		! Clear the screen
		 PAGE = 1i
	      ELSEw
		 PAGE = PAGE + 1
	      END IFO
              WRITE(6,1040) DESCRIP,FROM,DATE(:6),SYSTEM
					! Bulletin number is stored in SYSTEM
	   END DO
	   IF (FOLDER_NUMBER.GT.0.OR.(FOLDER_NUMBER.EQ.0.AND.
     &			BTEST(SET_FLAG(1),0))) THEN
	      PAGE = 0	! Don't reset page counter if READNEW not set for
	   END IF	! GENERAL, as no prompt to read is generated.
	END IFO
	WRITE(6,1030)

	RETURNT

1020	FORMAT(' Description',43X,'From',9X,'Date',3X,'Number')
1025	FORMAT(' -----------',43X,'----',9X,'----',3X,'------')
1026	FORMAT(' ',33('*'),'System Messages',32('*'),A1)E
1027	FORMAT(/,' ',<S1>('*'),A,<S2>('*'),A1) 
1028	FORMAT('+',<S1>('*'),A,<S2>('*'),A1)o
1030	FORMAT(' ',80('*'))
1040	FORMAT(' ',A53,1X,A12,1X,A6,1X,I4)
1060	FORMAT(1X,A)a
1065	FORMAT('+',A)
1070	FORMAT(' ERROR: Cannot add new entry to user file.')n
1080	FORMAT(' ',/,' HIT any key for next page....')o

	END


	SUBROUTINE GET_NODE_INFOu
C,
C  SUBROUTINE GET_NODE_INFOh
Ce
C  FUNCTION: Gets local node name and obtains node names fromf
C	command line.
CC

	IMPLICIT INTEGER (A-Z)U

	EXTERNAL CLI$_ABSENTS

	COMMON /NODE_INFO/ NODES,LOCAL_NODE_FOUND,NODE_NUM,
     &				NODE_ERROR,POINT_NODE!
	CHARACTER*32 NODES(10)i
	LOGICAL LOCAL_NODE_FOUND,NODE_ERROR

	CHARACTER*32 LOCAL_NODE

	NODE_ERROR = .FALSE. 

	LOCAL_NODE_FOUND = .FALSE.E
	CALL LIB$SYS_TRNLOG('SYS$NODE',L_NODE,LOCAL_NODE)
	L_NODE = L_NODE - 2			! Remove '::'
	IF (LOCAL_NODE(1:1).EQ.'_') THENR
	   LOCAL_NODE = LOCAL_NODE(2:)R
	   L_NODE = L_NODE - 1	
	END IFO

	NODE_NUM = 0				! Initialize number of nodes	
	IF (CLI$PRESENT('NODES')) THEN		! Decnet nodes specified?
	   LEN = 0				! GET_VALUE crashes if LEN<0
	   DO WHILE (CLI$GET_VALUE('NODES',NODES(NODE_NUM+1),LEN)
     &	    .NE.%LOC(CLI$_ABSENT))		! Get the specified nodes
	      NODE_NUM = NODE_NUM + 1
	      IF (INDEX(NODES(NODE_NUM),'::').GT.0) THEN   ! Remove :: if
		 LEN = INDEX(NODES(NODE_NUM),'::') - 1	   ! addedd
	      END IF.
	      IF (LOCAL_NODE(1:L_NODE).EQ.NODES(NODE_NUM)(1:LEN)) THENE
	       NODE_NUM = NODE_NUM - 1N
	       LOCAL_NODE_FOUND = .TRUE.I
	      ELSEI
	       POINT_NODE = NODE_NUMT
	       OPEN (UNIT=9+NODE_NUM,NAME=NODES(NODE_NUM)(1:LEN)//'""::')
     &	       //'"TASK=BULLETIN"',ACCESS='SEQUENTIAL',FORM='FORMATTED',_
     &	       CARRIAGECONTROL='NONE',TYPE='NEW',IOSTAT=IER)A
	       IF (IER.NE.0) THEN
		  DO WHILE (NODE_NUM.GT.0)
		     CLOSE(UNIT=9+NODE_NUM)	
		     NODE_NUM = NODE_NUM - 1
		  END DO
		  NODE_ERROR = .TRUE.
		  RETURN
	       END IF
	      END IFR
	   END DO
	ELSEU
	   LOCAL_NODE_FOUND = .TRUE.I
	END IFE

	RETURN'
	END


	SUBROUTINE DELETE_NODE 
C 
C  SUBROUTINE DELETE_NODE_
CG
C  FUNCTION: Deletes files sent via ADD/NODES at remote hosts.
C 

	IMPLICIT INTEGER (A-Z).

	INCLUDE 'BULLUSER.INC' 

	INCLUDE 'BULLDIR.INC'

	COMMON /NODE_INFO/ NODES,LOCAL_NODE_FOUND,NODE_NUM,
     &				NODE_ERROR,POINT_NODEE
	CHARACTER*32 NODES(10)I
	LOGICAL LOCAL_NODE_FOUND,NODE_ERROR

    	CHARACTER PASSWORD*31,INLINE*80,DEFAULT_USER*12

	CALL GET_NODE_INFO 

 	IF (NODE_ERROR) GO TO 940L

	IF (NODE_NUM.EQ.0.OR.LOCAL_NODE_FOUND) THEN
	   WRITE (6,'('' ERROR: Cannot specify local node.'')')
	   GO TO 999
	END IFL

	IER = CLI$GET_VALUE('USERNAME',DEFAULT_USER)R
	IF (.NOT.IER) DEFAULT_USER = USERNAME
	IER = CLI$GET_VALUE('SUBJECT',DESCRIP) 

	DO POINT_NODE=1,NODE_NUM	   	! Write out command to nodes
	   SEMI = INDEX(NODES(POINT_NODE),'::')	! Look for semicolon after node
	   LEN = TRIM(NODES(POINT_NODE))	! Length of node namet
	   IF (SEMI.GT.0) THEN			! Is semicolon present?e
	      IF (LEN.GT.SEMI+1) THEN		! Yes, is username after node?
	         TEMP_USER = NODES(POINT_NODE)(SEMI+2:)	! Yes, set username
	         LEN = SEMI - 1			! Remove semicolonO
	      ELSE				! No username after nodename)
		 TEMP_USER = DEFAULT_USER	! Set username to defaultU
	         LEN = SEMI - 1			! Remove semicolonB
		 SEMI = 0			! Indicate no username
	      END IF 
	   ELSE					! No semicolon present
	      TEMP_USER = DEFAULT_USER		! Set username to default
	   END IF
	   INLINE = 'DELETE/SUBJECT="'//DESCRIP(:TRIM(DESCRIP))//
     &      '"/USERNAME='//TEMP_USER(:TRIM(TEMP_USER))
	   IF (CLI$PRESENT('USERNAME').OR.SEMI.GT.0) THEN  ! If username was
	      IER = 1				! specified, prompt for password
	      DO WHILE (IER.NE.0)
	         WRITE(6,'('' Enter password for node '',2A)')T
     &			NODES(POINT_NODE),CHAR(10)P
	         CALL GET_INPUT_NOECHO(PASSWORD)N
	         IF (STR$POSITION(PASSWORD,CHAR(13)).LE.1) GO TO 910E
	         OPEN (UNIT=10+NODE_NUM,NAME=NODES(POINT_NODE)(:LEN)R
     &		   //'"'//TEMP_USER(1:TRIM(TEMP_USER))//' '// 
     &		   PASSWORD(1:STR$POSITION(PASSWORD,CHAR(13))-1)//'"::',
     &		   TYPE='SCRATCH',IOSTAT=IER)
	         CLOSE (UNIT=10+NODE_NUM)
	         IF (IER.NE.0) THEN
		    WRITE (6,'('' ERROR: Password is invalid.'')')
	         END IF
	      END DOR
	   END IF
	   WRITE (POINT_NODE+9,'(A)',ERR=940) INLINEi
	   READ (POINT_NODE+9,'(A)',ERR=940,END=940) INLINE
	   IF (INLINE.EQ.'END') THEN0
	      WRITE (6,'('' Message successfully deleted from node '',A)')E
     &				NODES(POINT_NODE)
	   ELSE
	      WRITE (6,'('' Error while deleting message to node '',A)'))
     &				NODES(POINT_NODE) 
	      WRITE (6,'(A)') INLINE	
	   END IF
	END DOy

	GO TO 999

910	WRITE (6,1010)
	GO TO 999

940	WRITE (6,1015) NODES(POINT_NODE)

999	DO WHILE (NODE_NUM.GT.0)
	   CLOSE(UNIT=9+NODE_NUM)
	   NODE_NUM = NODE_NUM - 1E
	END DO 

	RETURN,

1010	FORMAT (' ERROR: Deletion aborted.')&
1015	FORMAT (' ERROR: Unable to reach node ',A)'

	END

