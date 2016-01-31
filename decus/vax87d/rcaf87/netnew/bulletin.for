From:	CSBVAX::MRGATE!MRL%PFC-VAX.MIT.EDU@XX.LCS.MIT.EDU@SMTP 23-DEC-1987 15:46
To:	ARISIA::EVERHART
Subj:	BULLETIN.FOR


Received: from PFC-VAX.MIT.EDU by XX.LCS.MIT.EDU via Chaosnet; 23 Dec 87 15:04-EST
Date: 23 Dec 87 15:05:37 EST
From: MRL%PFC-VAX.MIT.EDU@XX.LCS.MIT.EDU
To: EVERHART%ARISIA.DECNET@CRD.GE.COM@XX
Subject: BULLETIN.FOR

C
C  BULLETIN.FOR, Version 12/14/87
C  Purpose: Bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Usage: Invoked by the BULLETIN command.
C  Programmer: Mark R. London
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

	COMMON /PAGE/ PAGE_LENGTH,PAGING
	LOGICAL PAGING /.FALSE./

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
	COMMAND_PROMPT = COMMAND_PROMPT(:TRIM(COMMAND_PROMPT))//'> '
	IF (COMMAND_PROMPT.EQ.'RUN> ') COMMAND_PROMPT = 'BULLETIN> '

	FOLDER_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//FOLDER

C
C  Test for /LOGIN switch.
C  NOTE: /READ has been replaced by the SET READNEW command.
C

	CALL LIB$GET_FOREIGN(INCMD)

	IER = CLI$DCL_PARSE('BULLETIN '//INCMD,BULLETIN_MAINCOMMANDS)

	READIT = 0
	LOGIT = 0
	IF (CLI$PRESENT('LOGIN').OR.CLI$PRESENT('SYSTEM')) LOGIT = 1	
					! Test for /LOGIN or /SYSTEM switch.

	IF (CLI$PRESENT('EDIT')) EDIT_DEFAULT = .TRUE.	! /EDIT switch test

	CALL FIND_BULLCP			! See if BULLCP is running

	IF (CLI$PRESENT('CLEANUP')) THEN	! Test for /CLEANUP switch
	   CALL CLI$GET_VALUE('CLEANUP',BULL_PARAMETER,LEN) ! Get folder #
	   READ (BULL_PARAMETER,'(I<LEN>)') FOLDER_NUMBER
	   CALL SELECT_FOLDER(.FALSE.,IER)	! Select folder
	   CALL CLEANUP_BULLFILE		! Cleanup empty blocks
	   CALL EXIT				! all done with cleanup
	ELSE IF (CLI$PRESENT('BBOARD')) THEN	! Test for /BBOARD switch
	   CALL BBOARD				! look for BBOARD mail
	   CALL EXIT				! all done with BBOARD
	ELSE IF (CLI$PRESENT('STARTUP')) THEN	! Create bulletin control
	   CALL CREATE_BULLCP			! subprocess at startup
	ELSE IF (CLI$PRESENT('BULLCP')) THEN	! This is BULLCP, so start
	   CALL RUN_BULLCP			! doing what BULLCP does!
	END IF

	CALL GETSTS(STS)			! Get process status word

	IF (LOGIT.GT.0) THEN			! If BULLETIN/LOGIN or /SYSTEM
	   IF ((STS.AND.PCB$M_BATCH).GT.0) CALL EXIT	! If BATCH, exit
	   CALL CRELNM('SYS$INPUT','TT')	! Take input from terminal
	END IF

	IF ((STS.AND.PCB$M_NETWRK).EQ.0) THEN
	   DECNET_PROC = .FALSE.
	   ERROR_UNIT = 6

	   CALL ASSIGN_TERMINAL			! Assign terminal

	   INCMD = 'SELECT'	! Causes nearest folder name to be selected
	   CALL SELECT_FOLDER(.FALSE.,IER)	! Select GENERAL folder
	   IF (.NOT.IER) RETURN			! If can't access, exit

	   CALL DELETE_EXPIRED			! Delete expired messages

C
C  Get page length for the terminal.
C

	   CALL GETPAGLEN(PAGE_LENGTH)

	   IF (CLI$PRESENT('PAGE')) PAGING = .TRUE.

	   IF (CLI$PRESENT('SYSTEM')) THEN
	      IER = CLI$GET_VALUE('SYSTEM',BULL_PARAMETER,LEN_P)
	      IF (IER.NE.%LOC(CLI$_ABSENT)) THEN	! Days specified?
	         CALL SUBTIME(LOGIN_BTIM,BULL_PARAMETER(:LEN_P),IER)
		 IF (.NOT.IER) THEN
		    WRITE (6,'('' ERROR: Invalid parameter in /SYSTEM.'')')
		    CALL EXIT
		 END IF
	      END IF
	      CALL SHOW_SYSTEM
	      CALL EXIT
	   END IF

C
C  Get user info stored in SYS$LOGIN.  Currently, this simply stores
C  the time of the latest message read for each folder.
C

	   CALL OPEN_USERINFO

C
C  If /LOGIN, display SYSTEM bulletins and subject of non-SYSTEM bulletins.
C

	   IF (LOGIT.GT.0) THEN			! Is /LOGIN present?
	      CALL LOGIN			! Display SYSTEM bulletins
	      IF (READIT.EQ.0) CALL EXIT	! If no READNEWs not set, exit
	   END IF

C
C  If new bulletins have been added since the last time bulletins have been
C  read, position bulletin pointer so that next bulletin read is the first new
C  bulletin, and alert user.  If READNEW set and no new bulletins, just exit.
C

	   CALL NEW_MESSAGE_NOTIFICATION
	ELSE
	   DECNET_PROC = .TRUE.
	   ERROR_UNIT = 5
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
	      DO WHILE (IER.GT.0.AND.INCMD(:1).EQ.' ')
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
	   IF (READ_ONLY.AND.(INCMD(:3).EQ.'ADD'.OR.INCMD(:3).EQ.'DEL'e
     &	     .OR.INCMD(:3).EQ.'CHA'.OR.INCMD(:3).EQ.'REP')) THENe
						! FOLDER can only be read?
	     WRITE (6,'('' ERROR: Access to folder limited to reading.'')')
	   ELSE IF (INCMD(:3).EQ.'ADD') THEN	! ADD bulletin command?E
	     CALL ADD				! Go add bulletine
	   ELSE IF (INCMD(:4).EQ.'BACK') THEN	! BACK command?
	     IF (BULL_POINT.LE.1) THENn
	        WRITE(6,1060)
	     ELSE
	        CALL READ(READ_COUNT,BULL_POINT-1)  ! Try to read previous bull
	     END IF
	   ELSE IF (INCMD(:4).EQ.'CHAN') THEN		! CHANGE command?I
	     CALL REPLACE				! Replace old bulletin
	   ELSE IF (INCMD(:4).EQ.'COPY') THEN		! COPY command? 
	     CALL MOVE(.FALSE.)
	   ELSE IF (INCMD(:4).EQ.'CREA') THEN		! CREATE command?P
	     CALL CREATE_FOLDER			! Go create the folder
	   ELSE IF (INCMD(:4).EQ.'CURR') THEN		! CURRENT command?
	     READ_COUNT = -1		! Reread current message from beginning.L
	     CALL READ(READ_COUNT,BULL_POINT)
	   ELSE IF (INCMD(:4).EQ.'DELE') THEN 	! DELETE command?L
	     CALL DELETE			! Go delete bulletin
	   ELSE IF (INCMD(:4).EQ.'DIRE') THEN		! DIRECTORY command?
	     IF (CLI$PRESENT('FOLDER')) THEN		! /FOLDER specified?P
		CALL DIRECTORY_FOLDERS(FOLDER_COUNT)	! Show all folders0
	     ELSE IF (CLI$PRESENT('SELECT_FOLDER')) THEN! Folder specified?
	        CALL SELECT_FOLDER(.TRUE.,IER)		! Try to select folderP
	        IF (IER) THEN				! If successful
	           CALL DIRECTORY(DIR_COUNT)		! Show messages
		END IF
	     ELSE
	        CALL DIRECTORY(DIR_COUNT)		! Show messagess
	     END IF
	   ELSE IF (INCMD(:4).EQ.'FILE'.OR.
     &		    INCMD(:4).EQ.'EXTR') THEN		! FILE command?
	     CALL FILE				! Copy bulletin to file
	   ELSE IF (INCMD(:1).EQ.'E'.OR. 
     &		    INCMD(:4).EQ.'QUIT') THEN		! EXIT command?
	     GO TO 999				! Exit from program
	   ELSE IF (INCMD(:4).EQ.'HELP') THEN		! HELP command?A
	     IER = LIB$SYS_TRNLOG('BULL_HELP',HLEN,HELP_DIRECTORY)Y
	     IF (IER.NE.1) THEN
		HELP_DIRECTORY = 'SYS$HELP:'
		HLEN = 9
	     ELSE IF (HELP_DIRECTORY(HLEN:HLEN).NE.':'.AND.
     &		   HELP_DIRECTORY(HLEN:HLEN).NE.']') THENo
		HELP_DIRECTORY = HELP_DIRECTORY(:HLEN)//':'o
		HLEN = HLEN + 1h
	     END IF
	     CALL HELP(HELP_DIRECTORY(:HLEN)//'BULL.HLB')	! Get help 
	   ELSE IF (INCMD(:4).EQ.'LAST') THEN		! LAST command?
	     READ_COUNT = -1	
	     BULL_READ = 99999n
	     CALL READ(READ_COUNT,BULL_READ)M
	   ELSE IF (INCMD(:4).EQ.'MAIL') THEN		! MAIL command?A
	     CALL MAIL(MAIL_STATUS)
	   ELSE IF (INCMD(:3).EQ.'MOD') THEN		! MODIFY command?
	     CALL MODIFY_FOLDER
	   ELSE IF (INCMD(:4).EQ.'MOVE') THEN		! MOVE command?1
	     CALL MOVE(.TRUE.)P
	   ELSE IF (INCMD(:4).EQ.'NEXT') THEN		! NEXT command?T
	     CALL READ(READ_COUNT,BULL_POINT+1)		! Read next bulletin
	   ELSE IF (INCMD(:4).EQ.'PRIN') THEN		! PRINT command?
	     CALL PRINT				! Printout bulletin
	   ELSE IF (INCMD(:4).EQ.'READ') THEN		! READ command?T
	     IER = CLI$GET_VALUE('BULLETIN_NUMBER',BULL_PARAMETER,LEN_P)/
	     IF (IER.NE.%LOC(CLI$_ABSENT)) THEN		! Bulletin specified? 
	        DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER) BULL_READ	! Yes
		READ_COUNT = -1D
		CALL READ(READ_COUNT,BULL_READ)N
	     ELSE
		CALL READ(READ_COUNT,BULL_POINT+1)
	     END IF
	   ELSE IF (INCMD(:3).EQ.'REM') THEN		! REMOVE command?
	     CALL REMOVE_FOLDER
	   ELSE IF (INCMD(:3).EQ.'REP') THEN		! REPLY command?E
	     IF (BULL_POINT.LT.1) THENL
		WRITE (6,'('' ERROR: No bulletin currently read.'')')A
	     ELSE
	        WRITE (6,'('' Adding REPLY message with the subject:'')')
		IF (DESCRIP(:3).NE.'RE:') THEN
	           WRITE (6,'(1X,A)') 'RE: '//DESCRIP
		ELSE
	           WRITE (6,'(1X,A)') DESCRIP
		END IF
	        CALL ADDL
	     END IF
	   ELSE IF (INCMD(:3).EQ.'RES') THEN		! RESPOND command?S
	     CALL RESPOND(MAIL_STATUS)	
	   ELSE IF (INCMD(:3).EQ.'SEA') THEN		! SEARCH command?
	     CALL SEARCH(READ_COUNT)!
	   ELSE IF (INCMD(:3).EQ.'SEL') THEN		! SELECT command?
	     CALL SELECT_FOLDER(.TRUE.,IER)
	   ELSE IF (INCMD(:3).EQ.'SET') THEN		! SET command?L
	     CALL CLI$GET_VALUE('SET_PARAM1',BULL_PARAMETER) 
	     IF (BULL_PARAMETER(:2).EQ.'BB') THEN		! SET BBOARD?
		CALL SET_BBOARD(.TRUE.)r
	     ELSE IF (BULL_PARAMETER(:4).EQ.'NOBB') THEN	! SET NOBBOARD?r
		CALL SET_BBOARD(.FALSE.)
	     ELSE IF (BULL_PARAMETER(:2).EQ.'DU') THEN		! SET DUMP?
		CALL SET_FOLDER_FLAG(.TRUE.,1,'DUMP')n
	     ELSE IF (BULL_PARAMETER(:4).EQ.'NODU') THEN	! SET NODUMP?T
		CALL SET_FOLDER_FLAG(.FALSE.,1,'DUMP')
	     ELSE IF (BULL_PARAMETER(:3).EQ.'NOT') THEN	! SET NOTIFY?
		IF (CLI$PRESENT('DEFAULT')) THEN
		   CALL SET_FOLDER_DEFAULT(1,-1,-1)S
		ELSE IF (CLI$PRESENT('ALL')) THEN 
		   IF (SETPRV_PRIV()) THEN
		      CALL SET_FOLDER_DEFAULT(1,-2,-2)
		   ELSEe
		      WRITE (6,'('' ERROR: /ALL is a privileged command.'')')
	 	   END IF
		ELSE
		   CALL CHANGE_FLAG(1,4)
		END IF
	     ELSE IF (BULL_PARAMETER(:1).EQ.'E') THEN	! SET EXPIRE?
	        IER = CLI$GET_VALUE('EXPIRATION',BULL_PARAMETER,LEN_P) 
		IF (LEN_P.LE.3) THEN
	           READ (BULL_PARAMETER,'(I<LEN_P>)') LIMIT
		   CALL SET_FOLDER_EXPIRE_LIMIT(LIMIT)
		ELSE
		   WRITE (6,'('' ERROR: Invalid expiration specified.'')')
		END IF
	     ELSE IF (BULL_PARAMETER(:3).EQ.'NOE') THEN	! SET NOEXPIRE?
		CALL SET_FOLDER_EXPIRE_LIMIT(0)
	     ELSE IF (BULL_PARAMETER(:3).EQ.'NON') THEN	! SET NONOTIFY?
		IF (CLI$PRESENT('DEFAULT')) THEN
		   CALL SET_FOLDER_DEFAULT(0,-1,-1)r
		ELSE IF (CLI$PRESENT('ALL')) THEN
		   IF (SETPRV_PRIV()) THEN
		      CALL SET_FOLDER_DEFAULT(0,-2,-2)
		   ELSE
		      WRITE (6,'('' ERROR: /ALL is a privileged command.'')')O
	 	   END IF
		ELSE
		   CALL CHANGE_FLAG(0,4)
		END IF
	     ELSE IF (BULL_PARAMETER(:1).EQ.'S') THEN		! SET SHOWNEW?
	        IF (FOLDER_NUMBER.EQ.0) THEN 
	         WRITE (6,'(e
     &		 '' ERROR: SET SHOWNEW not allowed for GENERAL folder.'')') 
		ELSE IF (CLI$PRESENT('DEFAULT')) THENu
		   CALL SET_FOLDER_DEFAULT(-1,0,1)
		ELSE IF (CLI$PRESENT('ALL')) THENM
		   IF (SETPRV_PRIV()) THEN
		      CALL SET_FOLDER_DEFAULT(-2,0,1)=
		   ELSE
		      WRITE (6,'('' ERROR: /ALL is a privileged command.'')')
	 	   END IF
		ELSE
		   CALL CHANGE_FLAG(0,2)
		   CALL CHANGE_FLAG(1,3)
		END IF
	     ELSE IF (BULL_PARAMETER(:3).EQ.'NOS') THEN	! SET NOSHOWNEW? 
	        IF (FOLDER_NUMBER.EQ.0) THENr
	         WRITE (6,'(=
     &		 '' ERROR: SET NOSHOWNEW not allowed for GENERAL folder.'')')U
		ELSE IF (CLI$PRESENT('DEFAULT')) THENM
		   CALL SET_FOLDER_DEFAULT(-1,0,0)
		ELSE IF (CLI$PRESENT('ALL')) THENI
		   IF (SETPRV_PRIV()) THEN
		      CALL SET_FOLDER_DEFAULT(-2,0,0) 
		   ELSES
		      WRITE (6,'('' ERROR: /ALL is a privileged command.'')')O
	 	   END IF
		ELSE
		   CALL CHANGE_FLAG(0,2)
		   CALL CHANGE_FLAG(0,3)
		END IF
	     ELSE IF (BULL_PARAMETER(:1).EQ.'R') THEN		! SET READNEW?
		IF (CLI$PRESENT('DEFAULT')) THEN
		   CALL SET_FOLDER_DEFAULT(-1,1,0)
		ELSE IF (CLI$PRESENT('ALL')) THENI
		   IF (SETPRV_PRIV()) THEN
		      CALL SET_FOLDER_DEFAULT(-2,1,0)C
		   ELSE'
		      WRITE (6,'('' ERROR: /ALL is a privileged command.'')')$
	 	   END IF
		ELSE
		   CALL CHANGE_FLAG(1,2)
		   CALL CHANGE_FLAG(0,3)
		END IF
	     ELSE IF (BULL_PARAMETER(:3).EQ.'NOR') THEN	! SET NOREADNEW?T
		IF (CLI$PRESENT('DEFAULT')) THEN
		   CALL SET_FOLDER_DEFAULT(-1,0,0)
		ELSE IF (CLI$PRESENT('ALL')) THENl
		   IF (SETPRV_PRIV()) THEN
		      CALL SET_FOLDER_DEFAULT(-2,0,0)t
		   ELSE 
		      WRITE (6,'('' ERROR: /ALL is a privileged command.'')')L
	 	   END IF
		ELSE
		   CALL CHANGE_FLAG(0,2)
		   CALL CHANGE_FLAG(0,3)
		END IF
	     ELSE IF (BULL_PARAMETER(:2).EQ.'BR') THEN		! SET BRIEF?!
	        IF (FOLDER_NUMBER.EQ.0) THEN 
	         WRITE (6,'(t
     &		 '' ERROR: SET BRIEF not allowed for GENERAL folder.'')')r
		ELSE
		 IF (CLI$PRESENT('DEFAULT')) THENn
		   CALL SET_FOLDER_DEFAULT(-1,1,1)
		 ELSE IF (CLI$PRESENT('ALL')) THEN
		   IF (SETPRV_PRIV()) THEN
		      CALL SET_FOLDER_DEFAULT(-2,1,1)	
		   ELSEE
		      WRITE (6,'('' ERROR: /ALL is a privileged command.'')'))
	 	   END IF
		 ELSE 
		   CALL CHANGE_FLAG(1,2)
		   CALL CHANGE_FLAG(1,3)
		 END IFR
		END IF
	     ELSE IF (BULL_PARAMETER(:4).EQ.'NOBR') THEN	! SET NOBRIEF?
	        IF (FOLDER_NUMBER.EQ.0) THEN.
	         WRITE (6,'(t
     &		 '' ERROR: SET NOBRIEF not allowed for GENERAL folder.'')')D
		ELSE
		 IF (CLI$PRESENT('DEFAULT')) THENF
		   CALL SET_FOLDER_DEFAULT(-1,0,0)
		 ELSE IF (CLI$PRESENT('ALL')) THEN
		   IF (SETPRV_PRIV()) THEN
		      CALL SET_FOLDER_DEFAULT(-2,0,0)F
		   ELSEI
		      WRITE (6,'('' ERROR: /ALL is a privileged command.'')')A
	 	   END IF
		 ELSEl
		   CALL CHANGE_FLAG(0,2)
		   CALL CHANGE_FLAG(0,3)
		 END IF 
		END IF
	     ELSE IF (BULL_PARAMETER(:1).EQ.'A') THEN		! SET ACCESS?m
		CALL SET_ACCESS(.TRUE.)O
	     ELSE IF (BULL_PARAMETER(:3).EQ.'NOA') THEN	! SET NOACCESS?
		CALL SET_ACCESS(.FALSE.)
	     ELSE IF (BULL_PARAMETER(:1).EQ.'F') THEN		! SET FOLDER?
		CALL SELECT_FOLDER(.TRUE.,IER)
	     ELSE IF (BULL_PARAMETER(:1).EQ.'G') THEN		! SET GENERIC?
		CALL SET_GENERIC(.TRUE.)
	     ELSE IF (BULL_PARAMETER(:3).EQ.'NOG') THEN	! SET NOGENERIC? 
		CALL SET_GENERIC(.FALSE.)N
	     ELSE IF (BULL_PARAMETER(:1).EQ.'L') THEN		! SET LOGIN?
		CALL SET_LOGIN(.TRUE.)
	     ELSE IF (BULL_PARAMETER(:3).EQ.'NOL') THEN	! SET NOLOGIN?F
		CALL SET_LOGIN(.FALSE.)C
	     ELSE IF (BULL_PARAMETER(:2).EQ.'PR') THEN		! SET PRIVS? 
		CALL SET_PRIV	
	     ELSE IF (BULL_PARAMETER(:2).EQ.'PA') THEN		! SET PAGE?
		PAGING = .TRUE.F
		WRITE (6,'('' PAGE has been set.'')')_
	     ELSE IF (BULL_PARAMETER(:3).EQ.'NOP') THEN	! SET NOPAGE?
		PAGING = .FALSE.
		WRITE (6,'('' NOPAGE has been set.'')')m
	     END IF
	   ELSE IF (INCMD(:4).EQ.'SHOW') THEN		! SHOW command?)
	     CALL CLI$GET_VALUE('SHOW_PARAM1',BULL_PARAMETER,LEN_P)
	     IF (BULL_PARAMETER(:2).EQ.'FL') THEN	! SHOW FLAGS?
		CALL SHOW_FLAGSL
	     ELSE IF (BULL_PARAMETER(:2).EQ.'FO') THEN	! SHOW FOLDER?
	        CALL SHOW_FOLDER 
	     ELSE IF (BULL_PARAMETER(:2).EQ.'NE') THEN	! SHOW NEW?N
		SAVE_FOLDER_NUMBER = FOLDER_NUMBER
		DO FOLDER_NUMBER = 0,FOLDER_MAXL
	   	   IF (TEST2(SET_FLAG,FOLDER_NUMBER).OR.R
     &		       TEST2(BRIEF_FLAG,FOLDER_NUMBER)) THEN
		      CALL SELECT_FOLDER(.FALSE.,IER)R
		      IF (NBULL.GT.0) THEN
		        DIFF = COMPARE_BTIM(
     &			 LAST_READ_BTIM(1,FOLDER_NUMBER+1),F_NEWEST_BTIM)
		        IF (DIFF.LT.0) THEN 
		         WRITE (6,'('' There are new messages in folder ''
     &			   ,A,''.'')') FOLDER(:TRIM(FOLDER))A
			END IF
		      END IF
		   END IFD
		END DO
		FOLDER_NUMBER = SAVE_FOLDER_NUMBER
		CALL SELECT_FOLDER(.FALSE.,IER)V
	     ELSE IF (BULL_PARAMETER(:1).EQ.'P') THEN	! SHOW PRIVILEGES?I
		CALL SHOW_PRIV
	     END IF
	   ELSE IF (INCMD(:4).EQ.'UNDE') THEN		! UNDELETE command?n
	     CALL UNDELETEE
	   END IF

100	   CONTINUE	

	END DOm

999	CALL EXITP

1010	FORMAT(Q,A)
1060	FORMAT(' ERROR: There are no more messages.')

	END




	SUBROUTINE ADDE
CA
C  SUBROUTINE ADDR
CU
C  FUNCTION: Adds bulletin to bulletin file.
CA
	IMPLICIT INTEGER (A - Z)e

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /NODE_INFO/ NODES,LOCAL_NODE_FOUND,NODE_NUM,
     &				NODE_ERROR,POINT_NODEN
	CHARACTER*32 NODES(10):
	LOGICAL LOCAL_NODE_FOUND,NODE_ERROR

	COMMON /DECNET/ DECNET_PROC,ERROR_UNITD
	LOGICAL DECNET_PROC

	COMMON /EDIT/ EDIT_DEFAULTL
	DATA EDIT_DEFAULT/.FALSE./6

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'h

	INCLUDE 'BULLFOLDER.INC'C

	INCLUDE '($BRKDEF)'

	CHARACTER INEXDATE*11,INEXTIME*8/
	CHARACTER*80 INDESCRIP,INPUTR

C6
C  The largest message that can be broadcasted is dependent on systemE
C  and user quotas.  The following limit is 12 lines of ( 80 characters +_
C  CR/LF ) + 2 bells.  This should be more than enough room, as broadcasts
C  shouldn't be too large anyway. 
CC

	PARAMETER BRDCST_LIMIT = 82*12 + 2?
	CHARACTER*(BRDCST_LIMIT) BROAD,
	CHARACTER*1 CR/13/,LF/10/,BELL/7/

	CHARACTER*80 INLINE
    	CHARACTER PASSWORD*31,DEFAULT_USER*12

	EXTERNAL CLI$_ABSENTU

	CALL DISABLE_CTRL		! Disable CTRL-Y & -C

	ALLOW = SETPRV_PRIV()

	IF (CLI$PRESENT('SELECT_FOLDER')) THENO
	   OLD_FOLDER_NUMBER = FOLDER_NUMBERB
	   CALL SELECT_FOLDER(.TRUE.,IER)
	   IF (.NOT.IER) GO TO 910!
	END IF?

	IER = CLI$GET_VALUE('FILESPEC',BULL_PARAMETER,LEN_P) 
	IF (IER.NE.%LOC(CLI$_ABSENT)) THEN 
 	   IF (.NOT.ALLOW) THEN		! If no SETPRV privileges, remove SYSPRV 
	      CALL DISABLE_PRIVS	! privileges when trying to(
	   END IF					! create new file.C
	   OPEN (UNIT=3,FILE=BULL_PARAMETER(:LEN_P),STATUS='OLD',READONLY, 
     &		 SHARED,ERR=920,FORM='FORMATTED')	! Try opening the file
	   CALL ENABLE_PRIVS	! Reset SYSPRV privileges/
	END IFp

	IER = CLI$GET_VALUE('USERNAME',DEFAULT_USER)	
	IF (.NOT.IER) DEFAULT_USER = USERNAME
	IF (DECNET_PROC) THEN		! Running via DECNET?T
	   USERNAME = DEFAULT_USERG
	   CALL CONFIRM_PRIV(USERNAME,ALLOW)N
	END IFF

	IF (FOLDER_SET.AND.			! If folder set and
     &	   (CLI$PRESENT('SYSTEM').OR.		! Is /SYSTEM switch present?
     &	    CLI$PRESENT('BROADCAST').OR.	! Is /BROADCAST switch present?	
     &	    CLI$PRESENT('SHUTDOWN').OR.		! Is /SHUTDOWN switch present?
     &	    CLI$PRESENT('NODES'))) THEN		! Decnet nodes specified?(
	   WRITE (6,'('' ERROR: Invalid parameter used with folder set.'')')
	   GO TO 910O
	END IFU

	IF (CLI$PRESENT('SYSTEM')) THEN		! Is /SYSTEM switch present?
	   IF (.NOT.ALLOW) THEN			! If no privileges(
	      WRITE(ERROR_UNIT,1070)		! Tell user
	      GO TO 910				! and abortn
	   END IF
	   SYSTEM = 1				! Set system bit
	ELSE,
	   SYSTEM = 0				! Clear system bit
	END IF.

	IF (CLI$PRESENT('BROADCAST')) THEN	! Is /BROADCAST switch present? 
	   IF (.NOT.(ALLOW.OR.OPER_PRIV())) THEN	! If no privileges
	      WRITE(ERROR_UNIT,1080)		! Tell user
	      GO TO 910				! and abortC
	   END IF
	END IFT

	IF (CLI$PRESENT('PERMANENT')) THEN	! Is /PERMANENT switch present? 
	   IF (.NOT.ALLOW.AND..NOT.FOLDER_SET) THEN	! If no privileges 
	      WRITE(ERROR_UNIT,1081)		! Tell user
	      GO TO 910				! and abortL
	   ELSE IF (F_EXPIRE_LIMIT.GT.0.AND..NOT.ALLOW	! Expiration limit
     &		.AND.USERNAME.NE.FOLDER_OWNER) THEN	! is present
	      WRITE(ERROR_UNIT,1083)_
	      GO TO 910
	   ELSE
	      SYSTEM = SYSTEM.OR.2		! Set permanent bit
	      INEXDATE = '5-NOV-2000'
	      INEXTIME = '00:00:00'
	   END IF
	END IF 

	IF (CLI$PRESENT('SHUTDOWN')) THEN	! Is /SHUTDOWN switch present?I
	   IF (.NOT.ALLOW) THEN			! If no privilegesT
	      WRITE(ERROR_UNIT,1082)		! Tell user
	      GO TO 910				! and abortp
	   ELSE
	      SYSTEM = SYSTEM.OR.4		! Set shutdown bitH
	      INEXDATE = '5-NOV-2000'
	      INEXTIME = '00:00:00'
	   END IF
	END IF(

	CALL GET_NODE_INFO 

	IF (NODE_ERROR) GO TO 940

	IF (SYSTEM.LE.1) THEN			! Not permanent or shutdown
	   CALL GET_EXPIRED(INPUT,IER)I
	   IF (.NOT.IER) GO TO 910N
	   INEXDATE = INPUT(:11)E
	   INEXTIME = INPUT(13:20)	
	END IFT

	IF (INCMD(:3).EQ.'REP'.AND.TRIM(DESCRIP).GT.0) THEN
					! REPLY command and subject present?,
	   IF (DESCRIP(:4).NE.'RE: ') THEN	! Fill in subject to be_
	      INDESCRIP = 'RE: '//DESCRIP	! RE: the subject of theS
	   END IF				! message just read.
	   LENDES = TRIM(INDESCRIP)
	ELSE IF (CLI$PRESENT('SUBJECT')) THEN	! /SUBJECT specified
	   CALL CLI$GET_VALUE('SUBJECT',INDESCRIP,LENDES)
	   IF (LENDES.GT.53) THEN		! If too many characters
	      WRITE(6,1060)			! tell user
	      WRITE(6,2020) INDESCRIP(:53)	! Show how much would fitF
	      GO TO 910
	   END IF
	ELSEQ
	   LENDES = 54T
	   DO WHILE (LENDES.GT.53)		! Do until valid description 
	      WRITE(6,1050)			! Request header for bulletin
	      CALL GET_LINE(INDESCRIP,LENDES)	! Get input lineE
	      IF (LENDES.LE.0) GO TO 910R
	      IF (LENDES.GT.53) THEN		! If too many characters	
	         WRITE(6,1060)			! tell userL
	         WRITE(6,2020) INDESCRIP(:53)	! Show how much would fit
	      END IFp
	   END DO
	END IF'

C 
C  If file specified in ADD command, read file to obtain bulletin.
C  Else, read the bulletin from the terminal.B
C_
	A
	ICOUNT = 0				! Line count for bulletin

	IF (CLI$PRESENT('EDIT').OR.EDIT_DEFAULT) THEN	! If /EDIT specified	
	   IF (LEN_P.EQ.0) THEN			! If no file param specified'
	      CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')
	      OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='OLD',N
     &		 DISPOSE='DELETE',ERR=920,FORM='FORMATTED') 
	      LEN_P = 1
	   ELSE
	      CLOSE (UNIT=3) 
	      CALL MAILEDIT(BULL_PARAMETER(:LEN_P),'SYS$LOGIN:BULL.SCR')N
	      OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='OLD',F
     &		 DISPOSE='DELETE',ERR=920,FORM='FORMATTED')A
	   END IF
	END IFH

	IF (LEN_P.GT.0) THEN			! If file param in ADD command
	   DO WHILE(1)				! Read until end of file to
	      READ (3,'(Q,A)',END=10) LEN,INPUT	! get record countE
	      IF (LEN.GT.80) GO TO 950R
	      ICOUNT = ICOUNT + 1 + MIN(LEN,80)
	      IF (LEN.EQ.0) ICOUNT = ICOUNT + 1	! COPY_BULL writes line withG
	   END DO				! 1 space for blank line
	ELSE					! If no input file
	   OPEN (UNIT=3,STATUS='SCRATCH',FILE='SYS$LOGIN:BULL.SCR',
     &		FORM='FORMATTED')		! Sratch file to save bulletinN
	   WRITE (6,1000)		! Request bulletin input from terminal
	   LEN = 81				! Length of input line
	   DO WHILE (LEN.GE.0)			! Input until no more inputS
	      CALL GET_LINE(INPUT,LEN)		! Get input lineA
	      IF (LEN.GT.80) THEN		! Input line too long.
		 WRITE(6,'('' ERROR: Input line length > 80.  Reinput:'')') 
	      ELSE IF (LEN.GE.0) THEN		! If good input line entered
		 ICOUNT = ICOUNT + 1 + LEN	! Increment record count 
		 IF (LEN.EQ.0) ICOUNT = ICOUNT + 1
		 WRITE(3,2010) INPUT(:LEN)	! Save line in scratch file
	      END IF_
	   END DO
	   IF (LEN.EQ.-1) GO TO 910		! CTRL_C entered, error outG
10	   IF (ICOUNT.EQ.0) GO TO 910		! No lines entered, error out)
	ENDIF

	REWIND (UNIT=3)

	IF (NODE_NUM.GT.0) THEN
	   INLINE = 'ADD'
	   IF (CLI$PRESENT('SYSTEM'))
     &	      INLINE = INLINE(:STR$POSITION(INLINE,' ')-1)//'/SYSTEM'
	   IF (CLI$PRESENT('BROADCAST'))F
     &	      INLINE = INLINE(:STR$POSITION(INLINE,' ')-1)//'/BROADCAST'
	   IF (CLI$PRESENT('PERMANENT'))I
     &	      INLINE = INLINE(:STR$POSITION(INLINE,' ')-1)//'/PERMANENT'&
	   IF (CLI$PRESENT('SHUTDOWN'))
     &	      INLINE = INLINE(:STR$POSITION(INLINE,' ')-1)//'/SHUTDOWN'
	   IF (CLI$PRESENT('BELL'))
     &	      INLINE = INLINE(:STR$POSITION(INLINE,' ')-1)//'/BELL'

	   LEN_INLINE = STR$POSITION(INLINE,' ') - 1D

	   DO POINT_NODE=1,NODE_NUM	   	! Write out command to nodesV
	      INLINE = INLINE(:LEN_INLINE)Q
	      SEMI = INDEX(NODES(POINT_NODE),'::')	! Look for semicolons 
	      LEN = TRIM(NODES(POINT_NODE))		! Length of node name 
	      IF (SEMI.GT.0) THEN			! Are semicolon found?E
	         IF (LEN.GT.SEMI+1) THEN		! Is username found?(
	            TEMP_USER = NODES(POINT_NODE)(SEMI+2:)	! YesB
	            LEN = SEMI - 1			! Remove semicolonsO
	         ELSE					! No username found...P
		    TEMP_USER = DEFAULT_USER		! Set user to default
	            LEN = SEMI - 1			! Remove semicolons6
		    SEMI = 0				! Indicate no username
	         END IF
	      ELSE					! No semicolons presentN
	         TEMP_USER = DEFAULT_USER		! Set user to defaultO
	      END IFM
	      IER = 1
	      DO WHILE ((INLINE.NE.'ADD'.OR.SEMI.GT.0.OR.
     &			CLI$PRESENT('USERNAME')).AND.IER.NE.0).
	         WRITE(6,'('' Enter password for node '',2A)')
     &			NODES(POINT_NODE),CHAR(10)'
		 CALL GET_INPUT_NOECHO(PASSWORD)
		 IF (TRIM(PASSWORD).EQ.0) GO TO 910A
	         OPEN (UNIT=10+NODE_NUM,NAME=NODES(POINT_NODE)(:LEN)//
     &		   '"'//TEMP_USER(:TRIM(TEMP_USER))//' '//
     &		   PASSWORD(:TRIM(PASSWORD))//'"::',
     &		   TYPE='SCRATCH',IOSTAT=IER)c
		 CLOSE (UNIT=10+NODE_NUM)b
		 IF (IER.NE.0) THENo
		    WRITE (6,'('' ERROR: Password is invalid.'')')
		 END IFw
	      END DOR
	      INLINE = INLINE(:STR$POSITION(INLINE,' ')-1)L
     &					//'/USERNAME='//TEMP_USER
	      WRITE (POINT_NODE+9,'(A)',ERR=940) INLINE
	      IF (SYSTEM.LE.1)	! If not permanent or shutdown specify dateL
     &		WRITE (POINT_NODE+9,'(A)',ERR=940) INEXDATE//' '//INEXTIME
	      WRITE (POINT_NODE+9,'(A)',ERR=940) INDESCRIP(:LENDES)
	      IER = 0
	      DO WHILE (IER.EQ.0)
	         READ (3,'(Q,A)',IOSTAT=IER) LEN,INPUTR
		 LEN = MIN(LEN,80)
		 IF (IER.EQ.0) THENN
		    WRITE (POINT_NODE+9,'(A)',ERR=940) INPUT(:LEN)
		 END IFT
	      END DOT
	      WRITE (POINT_NODE+9,'(A)',ERR=940) CHAR(26)
	      READ (POINT_NODE+9,'(A)',ERR=940,END=940) INPUT
	      IF (INPUT.EQ.'END') THENF
	         WRITE (6,'('' Message successfully sent to node '',A)'),
     &				NODES(POINT_NODE)r
	      ELSE 
	         WRITE (6,'('' Error while sending message to node '',A)')I
     &				NODES(POINT_NODE),
		 WRITE (6,'(A)') INPUT
		 GO TO 940
	      END IF
	      REWIND (UNIT=3)
	   END DO
	END IFT

	IF (.NOT.LOCAL_NODE_FOUND) GO TO 95	! Was local node specified?

CE
C  Add bulletin to bulletin file and directory entry for to directory file.'
CT

	CALL OPEN_FILE(2)			! Prepare to add dir entryC

	DESCRIP=INDESCRIP(:LENDES)		! Description headere
	EXDATE=INEXDATE				! Expiration dateN
	EXTIME=INEXTIME
	LENGTH = (ICOUNT+127)/128		! Number of recordsE
	FROM = USERNAME				! Username

	CALL OPEN_FILE(1)			! Prepare to add bulletin

	CALL READDIR(0,IER)			! Get NBLOCK 
	IF (IER.EQ.0) NBLOCK = 0		! If new file, NBLOCK is 0s

	CALL COPY_BULL(3,1,NBLOCK+1,IER)	! Add the new bulletin
	IF (IER.NE.0) GO TO 930			! Error in creating bulletin

	CALL CLOSE_FILE(1)			! Finished adding bulletin

	CALL ADD_ENTRY				! Add the new directory entry

	CALL UPDATE_FOLDER			! Update info in folder file
CA
C  If user is adding message, update that user's last read time forR
C  folder, so user is not alerted of new message which is owned by user.
C 
	LAST_READ_BTIM(1,FOLDER_NUMBER+1) = F_NEWEST_BTIM(1)D
	LAST_READ_BTIM(2,FOLDER_NUMBER+1) = F_NEWEST_BTIM(2)N

	CALL CLOSE_FILE(2)			! Totally finished with addD

CT
C  Broadcast the bulletin if requested.E
CR

	IF (CLI$PRESENT('BROADCAST')) THEN	! Should we broadcast the bull? 
	   REWIND (UNIT=3)			! Yes, rewind the input file
	   IF (CLI$PRESENT('BELL')) THEN	! Include BELL in message?
	      BROAD(:36) =			! Say who the bulletin is from
     &		BELL//BELL//CR//LF//LF//'NEW BULLETIN FROM: '//FROM 
	      START = 37			! Start adding next line here0
	   ELSE
	      BROAD(:34) =			! Say who the bulletin is from
     &		CR//LF//LF//'NEW BULLETIN FROM: '//FROMW
	      START = 35			! Start adding next line hereI
	   END IF
	   NBLANK = 0
	   END = 0	
	   DO WHILE (ICOUNT.GT.0) 		! Stuff bulletin into strings
	      READ(3,'(Q,A)') LEN,INPUT		! Read input lineI
	      ICOUNT = ICOUNT - LEN - 1
	      IF (LEN.EQ.0) THENI
		 NBLANK = NBLANK + 1	! Count number of blank lines
		 ICOUNT = ICOUNT - 1	! ICOUNT counts blank line as one space
	      ELSE		! Ignore blank liness at start or end of messageE
		 IF (NBLANK.GT.0.AND.END.GT.0) THENT
	            END = START + NBLANK*2	! Check how long string will beY
	            IF (END.GT.BRDCST_LIMIT) GO TO 90	! String too long?	
		    DO I=1,NBLANKb
	               BROAD(START:START+1) = CR//LFe
		       START = START + 2
		    END DO
		 END IF
		 NBLANK = 0I
	         END = START + LEN - 1 + 2	! Check how long string will bef
	         IF (END.GT.BRDCST_LIMIT) GO TO 90	! String too long?
	         BROAD(START:END) = CR//LF//INPUT(:LEN)! Else add new input
	         START = END + 1			! Reset pointer:
	      END IFm
	   END DO
90	   IF (CLI$PRESENT('ALL')) THEN		! Should we broadcast to ALL?W
	      IF (CLI$PRESENT('CLUSTER')) THENc
	         CALL SYS$BRKTHRU(,BROAD(:START-1)//CR,,o
     &			%VAL(BRK$C_ALLTERMS),,,%VAL(BRK$M_CLUSTER),,,,)
	      ELSE 
	         CALL SYS$BRKTHRU(,BROAD(:START-1)//CR,,.
     &			%VAL(BRK$C_ALLTERMS),,,,,,,)
	      END IF(
	   ELSE 				! Else just broadcast to users.
	      IF (CLI$PRESENT('CLUSTER')) THENt
	         CALL SYS$BRKTHRU(,BROAD(:START-1)//CR,, 
     &			%VAL(BRK$C_ALLUSERS),,,%VAL(BRK$M_CLUSTER),,,,)
	      ELSEr
	         CALL SYS$BRKTHRU(,BROAD(:START-1)//CR,,N
     &			%VAL(BRK$C_ALLUSERS),,,,,,,)I
	      END IF'
	   END IF
	END IFT

95	CLOSE (UNIT=3)			! Close the input file
	IF (DECNET_PROC) WRITE(5,'(''END'')') ! DECNET operation worked

100	CALL ENABLE_CTRL		! Enable CTRL-Y & -C
	DO I=10,NODE_NUM+9R
	   CLOSE (UNIT=I)
	END DOS

	IF (CLI$PRESENT('SELECT_FOLDER')) THEN 
	   FOLDER_NUMBER = OLD_FOLDER_NUMBER(
	   CALL SELECT_FOLDER(.TRUE.,IER)
	END IF(

	RETURNS

910	WRITE(ERROR_UNIT,1010)
	CLOSE (UNIT=3,ERR=100):
	GOTO 100T

920	WRITE(6,1020)	
	CALL ENABLE_PRIVS		! Reset SYSPRV privileges 
	GOTO 100E

930	WRITE (ERROR_UNIT,1025)	
	CALL CLOSE_FILE(1)A
	CALL CLOSE_FILE(2)I
	CLOSE (UNIT=3)n
	GO TO 100

940	WRITE (6,1015) NODES(POINT_NODE)
	CLOSE (UNIT=3)c
	GO TO 100

950	WRITE (6,1030)
	CLOSE (UNIT=3) 
	GO TO 100

1000	FORMAT (' Enter message: End with ctrl-z, cancel with ctrl-c')_
1010	FORMAT (' No message was added.')
1015	FORMAT (' ERROR: Unable to reach node ',A)i
1020	FORMAT (' ERROR: Unable to open specified file.')
1025	FORMAT (' ERROR: Unable to add message to file.')
1030	FORMAT (' ERROR: Line length in file exceeds 80 characters.')
1050	FORMAT (' Enter description header.  Limit header to 53
     & characters.')
1060	FORMAT (' ERROR: Header > 53 characters. Header would bel
     & truncated to:')
1070	FORMAT (' ERROR: SETPRV privileges are needed for systeml
     & messages.')
1080	FORMAT (' ERROR: SETPRV privileges are needed to broadcaste
     & messages.')
1081	FORMAT (' ERROR: SETPRV privileges are needed to permanentO
     & messages.')
1082	FORMAT (' ERROR: SETPRV privileges are needed to shutdown
     & messages.')
1083	FORMAT (' ERROR: Folder has expiration limit.')
2010	FORMAT(A)
2020	FORMAT(1X,A)9

	END


	SUBROUTINE SUBTIME(BTIM,DAYS_BEFORE_TODAY,IER))

	IMPLICIT INTEGER (A-Z)N

	CHARACTER DAYS_BEFORE_TODAY*(*),TODAY_DATE*20

	INTEGER BTIM(2),TODAY_BTIM(2)

	IER = SYS$BINTIM(DAYS_BEFORE_TODAY,BTIM)F
	IF (.NOT.IER) RETURNT

	BTIM(1) = -BTIM(1)		! Convert to negative delta time 
	BTIM(2) = -BTIM(2)-1 

	IER = SYS$ASCTIM(TLEN,TODAY_DATE,,)
	CALL SYS$BINTIM(TODAY_DATE(:TLEN),TODAY_BTIM)

	CALL LIB$SUBX(TODAY_BTIM,BTIM,BTIM)

	RETURN)
	END
