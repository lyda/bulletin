From:	HENRY::IN%"MRL%PFCVAX%xx.lcs.mit.edu%csnet-relay.CSNET%relay.cs.net@RCA.COM" 19-JUN-1987 21:01
To:	"SYSTEM%UK.AC.SOTON.ESP.V1" <SYSTEM%UK.AC.SOTON.ESP.V1%nss.cs.ucl.ac.uk@xx>, 
Subj:	BULLETIN.FOR

C
C  BULLETIN.FOR, Version 6/10/87
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

	COMMON /EDIT/ EDIT_DEFAULT
	DATA EDIT_DEFAULT/.FALSE./

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

	FOLDER_FILE = FOLDER_DIRECTORY(1:TRIM(FOLDER_DIRECTORY))//FOLDER

C
C  Test for /LOGIN switch.
C  NOTE: /READ has been replaced by the SET READNEW command.
C

	CALL LIB$GET_FOREIGN(INCMD)

	IER = CLI$DCL_PARSE('BULLETIN'//INCMD,BULLETIN_MAINCOMMANDS)

	READIT = 0
	LOGIT = 0
	IF (CLI$PRESENT('LOGIN')) LOGIT = 1	! Test for /LOGIN switch.

	IF (CLI$PRESENT('EDIT')) EDIT_DEFAULT = .TRUE.	! /EDIT switch test

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

	CALL SELECT_FOLDER(.FALSE.,IER)		! Select GENERAL folder
	IF (.NOT.IER) RETURN			! If can't access, exit

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
	   DO FOLDER_NUMBER = 1,FOLDER_MAX
	      IF (TEST2(NEW_FLAG,FOLDER_NUMBER).AND.
     &		TEST2(SET_FLAG,FOLDER_NUMBER)) THEN
		 CALL SELECT_FOLDER(.FALSE.,IER)
		 IF (IER) THEN
		    IF (TEST2(NEW_FLAG,FOLDER_NUMBER)) THEN
		     IF (NBULL.GT.0) THEN
		      WRITE (6,'('' There are new messages in folder '',
     &			A,''.'')') FOLDER(1:TRIM(FOLDER))
		     ELSE
		      CALL CHANGE_FLAG(0,1)
		     END IF
		    END IF
		 ELSE				! Can't select the folder
		    CALL CHANGE_FLAG(0,2)	! then clear SET_FLAG
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
	   DO FOLDER_NUMBER = 0,FOLDER_MAX
	      IF (TEST2(NEW_FLAG,FOLDER_NUMBER).AND.
     &		TEST2(SET_FLAG,FOLDER_NUMBER)) THEN
		 CALL SELECT_FOLDER(.FALSE.,IER)
		 IF (IER) THEN
		    IF (FOLDER_NUMBER.GT.0) CALL LOGIN_FOLDER
	            IF (BULL_POINT.NE.-1) THEN
		     IF (TEST2(BRIEF_FLAG,FOLDER_NUMBER)) THEN
		      IF (FOLDER_NUMBER.GT.0) THEN
		       WRITE (6,'('' There are new messages in folder '',
     &			A,''.'')') FOLDER(1:TRIM(FOLDER))
		      END IF
		     ELSE
		       SAVE_BULL_POINT = BULL_POINT
		       CALL READNEW
		       IF (BULL_POINT.NE.SAVE_BULL_POINT
     &			 .AND.READ_DONE.EQ.-1) READ_DONE = FOLDER_NUMBER
		     END IF
		    ELSE			! If really no new messages,
		     CALL CHANGE_FLAG(0,1)	! then clear NEW_FLAG
		    END IF
		 ELSE				! Can't select the folder
		    CALL CHANGE_FLAG(0,2)	! then clear SET_FLAG
		 END IF
	      END IF
	   END DO
	   IF (READ_DONE.GE.0) THEN
	      IF (READ_DONE.EQ.0) CALL UPDATE_READ(NEW_GENERAL_BULL)
	      DO FOLDER_NUMBER = 0,FOLDER_MAX
	         IF (TEST2(NEW_FLAG,FOLDER_NUMBER).AND.
     &		   TEST2(SET_FLAG,FOLDER_NUMBER)) THEN
	 	   CALL CHANGE_FLAG(0,1)		! Clear NEW_FLAG
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
	   ELSE IF (INCMD(1:4).EQ.'FILE'.OR.
     &		    INCMD(1:4).EQ.'EXTR') THEN		! FILE command?
	     CALL FILE				! Copy bulletin to file
	   ELSE IF (INCMD(1:1).EQ.'E'.OR.
     &		    INCMD(1:4).EQ.'QUIT') THEN		! EXIT command?
	     GO TO 999				! Exit from program
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
	   ELSE IF (INCMD(1:3).EQ.'MOD') THEN		! MODIFY command?
	     CALL MODIFY_FOLDER
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
	   ELSE IF (INCMD(1:3).EQ.'REP') THEN		! REPLACE command?
	     CALL REPLACE				! Replace old bulletin
	   ELSE IF (INCMD(1:3).EQ.'RES') THEN		! RESPOND command?
	     CALL RESPOND(MAIL_STATUS)
	   ELSE IF (INCMD(1:3).EQ.'SEA') THEN		! SEARCH command?
	     CALL SEARCH(READ_COUNT)
	   ELSE IF (INCMD(1:3).EQ.'SEL') THEN		! SELECT command?
	     CALL SELECT_FOLDER(.TRUE.,IER)
	   ELSE IF (INCMD(1:3).EQ.'SET') THEN		! SET command?
	     CALL CLI$GET_VALUE('SET_PARAM1',BULL_PARAMETER)
	     IF (BULL_PARAMETER(1:2).EQ.'BB') THEN		! SET BBOARD?
		CALL SET_BBOARD(.TRUE.)
	     ELSE IF (BULL_PARAMETER(1:4).EQ.'NOBB') THEN	! SET NOBBOARD?
		CALL SET_BBOARD(.FALSE.)
	     ELSE IF (BULL_PARAMETER(1:3).EQ.'NOT') THEN	! SET NOTIFY?
		IF (CLI$PRESENT('DEFAULT')) THEN
		   CALL SET_FOLDER_DEFAULT(1,-1,-1)
		ELSE IF (CLI$PRESENT('ALL')) THEN
		   IF (SETPRV_PRIV()) THEN
		      CALL SET_FOLDER_DEFAULT(1,-2,-2)
		   ELSE
		      WRITE (6,'('' ERROR: /ALL is a privileged command.'')')
	 	   END IF
		ELSE
		   CALL CHANGE_FLAG(1,4)
		END IF
	     ELSE IF (BULL_PARAMETER(1:3).EQ.'NON') THEN	! SET NONOTIFY?
		IF (CLI$PRESENT('DEFAULT')) THEN
		   CALL SET_FOLDER_DEFAULT(0,-1,-1)
		ELSE IF (CLI$PRESENT('ALL')) THEN
		   IF (SETPRV_PRIV()) THEN
		      CALL SET_FOLDER_DEFAULT(0,-2,-2)
		   ELSE
		      WRITE (6,'('' ERROR: /ALL is a privileged command.'')')
	 	   END IF
		ELSE
		   CALL CHANGE_FLAG(0,4)
		END IF
	     ELSE IF (BULL_PARAMETER(1:1).EQ.'R') THEN		! SET READNEW?
		IF (CLI$PRESENT('DEFAULT')) THEN
		   CALL SET_FOLDER_DEFAULT(-1,1,0)
		ELSE IF (CLI$PRESENT('ALL')) THEN
		   IF (SETPRV_PRIV()) THEN
		      CALL SET_FOLDER_DEFAULT(-2,1,0)
		   ELSE
		      WRITE (6,'('' ERROR: /ALL is a privileged command.'')')
	 	   END IF
		ELSE
		   CALL CHANGE_FLAG(1,2)
		   CALL CHANGE_FLAG(0,3)
		END IF
	     ELSE IF (BULL_PARAMETER(1:3).EQ.'NOR') THEN	! SET NOREADNEW?
		IF (CLI$PRESENT('DEFAULT')) THEN
		   CALL SET_FOLDER_DEFAULT(-1,0,0)
		ELSE IF (CLI$PRESENT('ALL')) THEN
		   IF (SETPRV_PRIV()) THEN
		      CALL SET_FOLDER_DEFAULT(-2,0,0)
		   ELSE
		      WRITE (6,'('' ERROR: /ALL is a privileged command.'')')
	 	   END IF
		ELSE
		   CALL CHANGE_FLAG(0,2)
		   CALL CHANGE_FLAG(0,3)
		END IF
	     ELSE IF (BULL_PARAMETER(1:2).EQ.'BR') THEN		! SET BRIEF?
	        IF (FOLDER_NUMBER.EQ.0) THEN
	         WRITE (6,'(
     &		 '' ERROR: SET BRIEF not allowed for GENERAL folder.'')')
		ELSE
		 IF (CLI$PRESENT('DEFAULT')) THEN
		   CALL SET_FOLDER_DEFAULT(-1,1,1)
		 ELSE IF (CLI$PRESENT('ALL')) THEN
		   IF (SETPRV_PRIV()) THEN
		      CALL SET_FOLDER_DEFAULT(-2,1,1)
		   ELSE
		      WRITE (6,'('' ERROR: /ALL is a privileged command.'')')
	 	   END IF
		 ELSE
		   CALL CHANGE_FLAG(1,2)
		   CALL CHANGE_FLAG(1,3)
		 END IF
		END IF
	     ELSE IF (BULL_PARAMETER(1:4).EQ.'NOBR') THEN	! SET NOBRIEF?
	        IF (FOLDER_NUMBER.EQ.0) THEN
	         WRITE (6,'(
     &		 '' ERROR: SET NOBRIEF not allowed for GENERAL folder.'')')
		ELSE
		 IF (CLI$PRESENT('DEFAULT')) THEN
		   CALL SET_FOLDER_DEFAULT(-1,0,0)
		 ELSE IF (CLI$PRESENT('ALL')) THEN
		   IF (SETPRV_PRIV()) THEN
		      CALL SET_FOLDER_DEFAULT(-2,0,0)
		   ELSE
		      WRITE (6,'('' ERROR: /ALL is a privileged command.'')')
	 	   END IF
		 ELSE
		   CALL CHANGE_FLAG(0,2)
		   CALL CHANGE_FLAG(0,3)
		 END IF
		END IF
	     ELSE IF (BULL_PARAMETER(1:1).EQ.'A') THEN		! SET ACCESS?
		CALL SET_ACCESS(.TRUE.)
	     ELSE IF (BULL_PARAMETER(1:3).EQ.'NOA') THEN	! SET NOACCESS?
		CALL SET_ACCESS(.FALSE.)
	     ELSE IF (BULL_PARAMETER(1:1).EQ.'F') THEN		! SET FOLDER?
		CALL SELECT_FOLDER(.TRUE.,IER)
	     ELSE IF (BULL_PARAMETER(1:1).EQ.'L') THEN		! SET LOGIN?
		CALL SET_LOGIN(.TRUE.)
	     ELSE IF (BULL_PARAMETER(1:3).EQ.'NOL') THEN	! SET NOLOGIN?
		CALL SET_LOGIN(.FALSE.)
	     ELSE IF (BULL_PARAMETER(1:1).EQ.'P') THEN	! SET PRIVILEGES?
		CALL SET_PRIV
	     END IF
	   ELSE IF (INCMD(1:4).EQ.'SHOW') THEN		! SHOW command?
	     CALL CLI$GET_VALUE('SHOW_PARAM1',BULL_PARAMETER,LEN_P)
	     IF (BULL_PARAMETER(1:1).EQ.'B') THEN	! SHOW BRIEF?
		CALL SHOW_BRIEF
	     ELSE IF (BULL_PARAMETER(1:1).EQ.'F') THEN	! SHOW FOLDER?
	        CALL SHOW_FOLDER
	     ELSE IF (BULL_PARAMETER(1:2).EQ.'NE') THEN	! SHOW NEW?
		DO FOLDER_NUMBER = 0,FOLDER_MAX
	   	   IF (TEST2(NEW_FLAG,FOLDER_NUMBER).AND.
     &	       		TEST2(SET_FLAG,FOLDER_NUMBER).AND.NBULL.GT.0) THEN
		     CALL SELECT_FOLDER(.FALSE.,IER)
		     WRITE (6,'('' There are new messages in folder '',
     &			A,''.'')') FOLDER(1:TRIM(FOLDER))
		   END IF
		END DO
	     ELSE IF (BULL_PARAMETER(1:2).EQ.'NO') THEN	! SHOW NOTIFY?
		CALL SHOW_NOTIFY
	     ELSE IF (BULL_PARAMETER(1:1).EQ.'P') THEN	! SHOW PRIVILEGES?
		CALL SHOW_PRIV
	     ELSE IF (BULL_PARAMETER(1:1).EQ.'R') THEN	! SHOW READNEW?
		CALL SHOW_READNEW
	     END IF
	   END IF

100	   CONTINUE

	END DO

999	DO FOLDER_NUMBER = 0,FOLDER_MAX
	   IF (TEST2(NEW_FLAG,FOLDER_NUMBER).AND.
     &	       TEST2(SET_FLAG,FOLDER_NUMBER)) THEN
	      CALL CHANGE_FLAG(0,1)		! Clear NEW_FLAG
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

	COMMON /EDIT/ EDIT_DEFAULT
	DATA EDIT_DEFAULT/.FALSE./

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

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
	CHARACTER*(BRDCST_LIMIT) BROAD
	CHARACTER*1 CR/13/,LF/10/,BELL/7/

	CHARACTER*80 INLINE
    	CHARACTER PASSWORD*31,DEFAULT_USER*12

	EXTERNAL CLI$_ABSENT

	CALL DISABLE_CTRL		! Disable CTRL-Y & -C

	ALLOW = SETPRV_PRIV()

	IER = CLI$GET_VALUE('FILESPEC',BULL_PARAMETER,LEN_P)
	IF (IER.NE.%LOC(CLI$_ABSENT)) THEN
 	   IF (.NOT.ALLOW) THEN		! If no SETPRV privileges, remove SYSPRV
	      CALL DISABLE_PRIVS	! privileges when trying to
	   END IF					! create new file.
	   OPEN (UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),STATUS='OLD',READONLY,
     &		 SHARED,ERR=920,FORM='FORMATTED')	! Try opening the file
	   CALL ENABLE_PRIVS	! Reset SYSPRV privileges
	END IF

	IER = CLI$GET_VALUE('USERNAME',DEFAULT_USER)
	IF (.NOT.IER) DEFAULT_USER = USERNAME
	IF (DECNET_PROC) THEN		! Running via DECNET?
	   USERNAME = DEFAULT_USER
	   CALL CONFIRM_PRIV(USERNAME,ALLOW)
	END IF

	IF (FOLDER_SET.AND.			! If folder set and
     &	   (CLI$PRESENT('SYSTEM').OR.		! Is /SYSTEM switch present?
     &	    CLI$PRESENT('BROADCAST').OR.	! Is /BROADCAST switch present?
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
	ELSE
	   SYSTEM = 0				! Clear system bit
	END IF

	IF (CLI$PRESENT('BROADCAST')) THEN	! Is /BROADCAST switch present?
	   IF (.NOT.(ALLOW.OR.OPER_PRIV())) THEN	! If no privileges
	      WRITE(ERROR_UNIT,1080)		! Tell user
	      RETURN				! and abort
	   END IF
	END IF

	IF (CLI$PRESENT('PERMANENT')) THEN	! Is /PERMANENT switch present?
	   IF (.NOT.ALLOW.AND..NOT.FOLDER_SET) THEN	! If no privileges
	      WRITE(ERROR_UNIT,1081)		! Tell user
	      RETURN				! and abort
	   ELSE
	      SYSTEM = SYSTEM.OR.2		! Set permanent bit
	      INEXDATE = '5-NOV-2000'
	      INEXTIME = '00:00:00'
	   END IF
	END IF

	IF (CLI$PRESENT('SHUTDOWN')) THEN	! Is /SHUTDOWN switch present?
	   IF (.NOT.ALLOW) THEN			! If no privileges
	      WRITE(ERROR_UNIT,1082)		! Tell user
	      RETURN				! and abort
	   ELSE
	      SYSTEM = SYSTEM.OR.4		! Set shutdown bit
	      INEXDATE = '5-NOV-2000'
	      INEXTIME = '00:00:00'
	   END IF
	END IF

	IF (CLI$PRESENT('SELECT_FOLDER')) THEN
	   OLD_FOLDER_NUMBER = FOLDER_NUMBER
	   CALL SELECT_FOLDER(.TRUE.,IER)
	   IF (.NOT.IER) RETURN
	END IF

	CALL GET_NODE_INFO

	IF (NODE_ERROR) GO TO 940

	IF (SYSTEM.LE.1) THEN			! Not permanent or shutdown
	   CALL GET_EXPIRED(INPUT,IER)
	   IF (.NOT.IER) GO TO 910
	   INEXDATE = INPUT(1:11)
	   INEXTIME = INPUT(13:20)
	END IF

	LENDES = 54
	DO WHILE (LENDES.GT.53)			! Do until valid description
	   WRITE(6,1050)			! Request header for bulletin
	   CALL GET_LINE(INDESCRIP,LENDES)	! Get input line
	   IF (LENDES.LE.0) GO TO 910
	   IF (LENDES.GT.53) THEN		! If too many characters
	      WRITE(6,1060)			! tell user
	      WRITE(6,2020) INDESCRIP(1:53)	! Show how much would fit
	   END IF
	END DO

C
C  If file specified in ADD command, read file to obtain bulletin.
C  Else, read the bulletin from the terminal.
C
	
	ICOUNT = 0				! Line count for bulletin

	IF (CLI$PRESENT('EDIT').OR.EDIT_DEFAULT) THEN	! If /EDIT specified
	   IF (LEN_P.EQ.0) THEN			! If no file param specified
	      CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')
	      OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='OLD',
     &		 DISPOSE='DELETE',ERR=920,FORM='FORMATTED')
	      LEN_P = 1
	   ELSE
	      CLOSE (UNIT=3)
	      CALL MAILEDIT(BULL_PARAMETER(:LEN_P),'SYS$LOGIN:BULL.SCR')
	      OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='OLD',
     &		 DISPOSE='DELETE',ERR=920,FORM='FORMATTED')
	   END IF
	END IF

	IF (LEN_P.GT.0) THEN			! If file param in ADD command
	   DO WHILE(1)				! Read until end of file to
	      READ (3,'(Q,A)',END=10) LEN,INPUT	! get record count
	      IF (LEN.GT.80) GO TO 950
	      ICOUNT = ICOUNT + 1 + MIN(LEN,80)
	      IF (LEN.EQ.0) ICOUNT = ICOUNT + 1	! COPY_BULL writes line with
	   END DO				! 1 space for blank line
	ELSE					! If no input file
	   OPEN (UNIT=3,STATUS='SCRATCH',FILE='SYS$LOGIN:BULL.SCR',
     &		FORM='FORMATTED')		! Sratch file to save bulletin
	   WRITE (6,1000)		! Request bulletin input from terminal
	   LEN = 81				! Length of input line
	   DO WHILE (LEN.GE.0)			! Input until no more input
	      CALL GET_LINE(INPUT,LEN)		! Get input line
	      IF (LEN.GT.80) THEN		! Input line too long
		 WRITE(6,'('' ERROR: Input line length > 80.  Reinput:'')')
	      ELSE IF (LEN.GE.0) THEN		! If good input line entered
		 ICOUNT = ICOUNT + 1 + LEN	! Increment record count
		 IF (LEN.EQ.0) ICOUNT = ICOUNT + 1
		 WRITE(3,2010) INPUT(1:LEN)	! Save line in scratch file
	      END IF
	   END DO
	   IF (LEN.EQ.-1) GO TO 910		! CTRL_C entered, error out
10	   IF (ICOUNT.EQ.0) GO TO 910		! No lines entered, error out
	ENDIF

	REWIND (UNIT=3)

	IF (NODE_NUM.GT.0) THEN
	   INLINE = 'ADD'
	   IF (CLI$PRESENT('SYSTEM'))
     &	      INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)//'/SYSTEM'
	   IF (CLI$PRESENT('BROADCAST'))
     &	      INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)//'/BROADCAST'
	   IF (CLI$PRESENT('PERMANENT'))
     &	      INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)//'/PERMANENT'
	   IF (CLI$PRESENT('SHUTDOWN'))
     &	      INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)//'/SHUTDOWN'
	   IF (CLI$PRESENT('BELL'))
     &	      INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)//'/BELL'

	   LEN_INLINE = STR$POSITION(INLINE,' ') - 1

	   DO POINT_NODE=1,NODE_NUM	   	! Write out command to nodes
	      INLINE = INLINE(1:LEN_INLINE)
	      SEMI = INDEX(NODES(POINT_NODE),'::')	! Look for semicolons
	      LEN = TRIM(NODES(POINT_NODE))		! Length of node name
	      IF (SEMI.GT.0) THEN			! Are semicolon found?
	         IF (LEN.GT.SEMI+1) THEN		! Is username found?
	            TEMP_USER = NODES(POINT_NODE)(SEMI+2:)	! Yes
	            LEN = SEMI - 1			! Remove semicolons
	         ELSE					! No username found...
		    TEMP_USER = DEFAULT_USER		! Set user to default
	            LEN = SEMI - 1			! Remove semicolons
		    SEMI = 0				! Indicate no username
	         END IF
	      ELSE					! No semicolons present
	         TEMP_USER = DEFAULT_USER		! Set user to default
	      END IF
	      IER = 1
	      DO WHILE ((INLINE.NE.'ADD'.OR.SEMI.GT.0.OR.
     &			CLI$PRESENT('USERNAME')).AND.IER.NE.0)
	         WRITE(6,'('' Enter password for node '',2A)')
     &			NODES(POINT_NODE),CHAR(10)
		 CALL GET_INPUT_NOECHO(PASSWORD)
		 IF (STR$POSITION(PASSWORD,CHAR(13)).LE.1) GO TO 910
	         OPEN (UNIT=10+NODE_NUM,NAME=NODES(POINT_NODE)(:LEN)//
     &		   '"'//TEMP_USER(1:TRIM(TEMP_USER))//' '//
     &		   PASSWORD(1:STR$POSITION(PASSWORD,CHAR(13))-1)//'"::',
     &		   TYPE='SCRATCH',IOSTAT=IER)
		 CLOSE (UNIT=10+NODE_NUM)
		 IF (IER.NE.0) THEN
		    WRITE (6,'('' ERROR: Password is invalid.'')')
		 END IF
	      END DO
	      INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)
     &					//'/USERNAME='//TEMP_USER
	      WRITE (POINT_NODE+9,'(A)',ERR=940) INLINE
	      IF (SYSTEM.LE.1)	! If not permanent or shutdown specify date
     &		WRITE (POINT_NODE+9,'(A)',ERR=940) INEXDATE//' '//INEXTIME
	      WRITE (POINT_NODE+9,'(A)',ERR=940) INDESCRIP(1:LENDES)
	      IER = 0
	      DO WHILE (IER.EQ.0)
	         READ (3,'(Q,A)',IOSTAT=IER) LEN,INPUT
		 LEN = MIN(LEN,80)
		 IF (IER.EQ.0) THEN
		    WRITE (POINT_NODE+9,'(A)',ERR=940) INPUT(1:LEN)
		 END IF
	      END DO
	      WRITE (POINT_NODE+9,'(A)',ERR=940) CHAR(26)
	      READ (POINT_NODE+9,'(A)',ERR=940,END=940) INPUT
	      IF (INPUT.EQ.'END') THEN
	         WRITE (6,'('' Message successfully sent to node '',A)')
     &				NODES(POINT_NODE)
	      ELSE
	         WRITE (6,'('' Error while sending message to node '',A)')
     &				NODES(POINT_NODE)
		 WRITE (6,'(A)') INPUT
		 GO TO 940
	      END IF
	      REWIND (UNIT=3)
	   END DO
	END IF

	IF (.NOT.LOCAL_NODE_FOUND) GO TO 95	! Was local node specified?

C
C  Add bulletin to bulletin file and directory entry for to directory file.
C

	CALL OPEN_FILE(2)			! Prepare to add dir entry

	DESCRIP=INDESCRIP(1:LENDES)		! Description header
	EXDATE=INEXDATE				! Expiration date
	EXTIME=INEXTIME
	LENGTH = (ICOUNT+127)/128		! Number of records
	FROM = USERNAME				! Username

	CALL OPEN_FILE(1)			! Prepare to add bulletin

	CALL READDIR(0,IER)			! Get NBLOCK
	IF (IER.EQ.0) NBLOCK = 0		! If new file, NBLOCK is 0

	CALL COPY_BULL(3,1,NBLOCK+1,IER)	! Add the new bulletin
	IF (IER.NE.0) GO TO 930			! Error in creating bulletin

	CALL CLOSE_FILE(1)			! Finished adding bulletin

	CALL ADD_ENTRY				! Add the new directory entry

	CALL CLOSE_FILE(2)			! Totally finished with add

C
C  Broadcast the bulletin if requested.
C

	IF (CLI$PRESENT('BROADCAST')) THEN	! Should we broadcast the bull?
	   REWIND (UNIT=3)			! Yes, rewind the input file
	   IF (CLI$PRESENT('BELL')) THEN	! Include BELL in message?
	      BROAD(1:36) =			! Say who the bulletin is from
     &		BELL//BELL//CR//LF//LF//'NEW BULLETIN FROM: '//FROM
	      START = 37			! Start adding next line here
	   ELSE
	      BROAD(1:34) =			! Say who the bulletin is from
     &		CR//LF//LF//'NEW BULLETIN FROM: '//FROM
	      START = 35			! Start adding next line here
	   END IF
	   NBLANK = 0
	   END = 0
	   DO WHILE (ICOUNT.GT.0) 		! Stuff bulletin into string
	      READ(3,'(Q,A)') LEN,INPUT		! Read input line
	      ICOUNT = ICOUNT - LEN - 1
	      IF (LEN.EQ.0) THEN
		 NBLANK = NBLANK + 1	! Count number of blank lines
		 ICOUNT = ICOUNT - 1	! ICOUNT counts blank line as one space
	      ELSE		! Ignore blank liness at start or end of message
		 IF (NBLANK.GT.0.AND.END.GT.0) THEN
	            END = START + NBLANK*2	! Check how long string will be
	            IF (END.GT.BRDCST_LIMIT) GO TO 90	! String too long?
		    DO I=1,NBLANK
	               BROAD(START:START+1) = CR//LF
		       START = START + 2
		    END DO
		 END IF
		 NBLANK = 0
	         END = START + LEN - 1 + 2	! Check how long string will be
	         IF (END.GT.BRDCST_LIMIT) GO TO 90	! String too long?
	         BROAD(START:END) = CR//LF//INPUT(1:LEN)! Else add new input
	         START = END + 1			! Reset pointer
	      END IF
	   END DO
90	   IF (CLI$PRESENT('ALL')) THEN		! Should we broadcast to ALL?
	      CALL SYS$BRKTHRU
     &		(,BROAD(1:START-1)//CR,,%VAL(BRK$C_ALLTERMS),,,,,,,)
	   ELSE 				! Else just broadcast to users.
	      CALL SYS$BRKTHRU
     &		(,BROAD(1:START-1)//CR,,%VAL(BRK$C_ALLUSERS),,,,,,,)
	   END IF
	END IF

95	CLOSE (UNIT=3)			! Close the input file
	IF (DECNET_PROC) WRITE(5,'(''END'')') ! DECNET operation worked

100	CALL ENABLE_CTRL		! Enable CTRL-Y & -C
	DO I=10,NODE_NUM+9
	   CLOSE (UNIT=I)
	END DO

	IF (CLI$PRESENT('SELECT_FOLDER')) THEN
	   FOLDER_NUMBER = OLD_FOLDER_NUMBER
	   CALL SELECT_FOLDER(.TRUE.,IER)
	END IF

	RETURN

910	WRITE(ERROR_UNIT,1010)
	CLOSE (UNIT=3,ERR=100)
	GOTO 100

920	WRITE(6,1020)
	CALL ENABLE_PRIVS		! Reset SYSPRV privileges
	GOTO 100

930	WRITE (ERROR_UNIT,1025)
	CALL CLOSE_FILE(1)
	CALL CLOSE_FILE(2)
	CLOSE (UNIT=3)
	GO TO 100

940	WRITE (6,1015) NODES(POINT_NODE)
	CLOSE (UNIT=3)
	GO TO 100

950	WRITE (6,1030)
	CLOSE (UNIT=3)
	GO TO 100

1000	FORMAT (' Enter message: End with ctrl-z, cancel with ctrl-c')
1010	FORMAT (' No message was added.')
1015	FORMAT (' ERROR: Unable to reach node ',A)
1020	FORMAT (' ERROR: Unable to open specified file.')
1025	FORMAT (' ERROR: Unable to add message to file.')
1030	FORMAT (' ERROR: Line length in file exceeds 80 characters.')
1050	FORMAT (' Enter description header.  Limit header to 53
     & characters.')
1060	FORMAT (' ERROR: Header > 53 characters. Header would be
     & truncated to:')
1070	FORMAT (' ERROR: SETPRV privileges are needed for system
     & messages.')
1080	FORMAT (' ERROR: SETPRV privileges are needed to broadcast
     & messages.')
1081	FORMAT (' ERROR: SETPRV privileges are needed to permanent
     & messages.')
1082	FORMAT (' ERROR: SETPRV privileges are needed to shutdown
     & messages.')
2010	FORMAT(A)
2020	FORMAT(1X,A)

	END
