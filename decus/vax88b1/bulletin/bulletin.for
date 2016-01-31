C
C  BULLETIN.FOR, Version 8/3/88
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

	EXTERNAL ERROR_TRAP
	EXTERNAL BULLETIN_SUBCOMMANDS,LIB$GET_INPUT
	EXTERNAL BULLETIN_MAINCOMMANDS,ENABLE_CTRL_EXIT
	EXTERNAL CLI$_ABSENT,CLI$_NOCOMD

	PARAMETER PCB$M_BATCH = '4000'X
	PARAMETER PCB$M_NETWRK = '200000'X
	PARAMETER LIB$M_CLI_CTRLY = '2000000'X

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD

	CHARACTER HELP_DIRECTORY*64,SAVE_FOLDER*25

	COMMON /EDIT/ EDIT_DEFAULT
	DATA EDIT_DEFAULT/.FALSE./

	COMMON /COMMAND_SWITCHES/ LOGIN_SWITCH,SYSTEM_SWITCH
	COMMON /COMMAND_SWITCHES/ SYSTEM_LOGIN_BTIM(2)
	COMMON /COMMAND_SWITCHES/ REVERSE_SWITCH,SEPARATE
	CHARACTER*1 SEPARATE

	CALL LIB$ESTABLISH(ERROR_TRAP)
	IF (.NOT.CLI$GET_VALUE('PROMPT',COMMAND_PROMPT,ILEN)) THEN
	   CALL LIB$GET_FOREIGN(INCMD)
	   CALL CLI$DCL_PARSE('BULLETIN '//INCMD,BULLETIN_MAINCOMMANDS)
	   CALL CLI$GET_VALUE('$LINE',COMMAND_PROMPT,ILEN)
	END IF
	CALL LIB$REVERT

	READIT = 0
	LOGIN_SWITCH = CLI$PRESENT('LOGIN')
	SYSTEM_SWITCH = CLI$PRESENT('SYSTEM')
	REVERSE_SWITCH = CLI$PRESENT('REVERSE')

	IER = LIB$SYS_TRNLOG('BULL_DISABLE',LEN_P,BULL_PARAMETER)
	IF (IER.EQ.1.AND.LEN_P.GT.0.AND..NOT.CLI$PRESENT('STOP')) THEN
	   IF (.NOT.LOGIN_SWITCH) THEN
	      WRITE (6,'('' BULLETIN temporarily disabled. Try later.'')')
	   END IF
	   CALL EXIT
	END IF

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

	I = 1				! Strip off folder name if specified
	DO WHILE (I.LE.ILEN)
	   IF (COMMAND_PROMPT(I:I).EQ.' ') THEN
	      COMMAND_PROMPT = COMMAND_PROMPT(:I-1)
	      I = ILEN + 1
	   ELSE
	      I = I + 1
	   END IF
	END DO
	ILEN = 1			! Get executable name to use as prompt
	DO WHILE (ILEN.GT.0)
	   ILEN = MAX(INDEX(COMMAND_PROMPT,':'),INDEX(COMMAND_PROMPT,']'))
	   IF (ILEN.GT.0) THEN
	      COMMAND_PROMPT = COMMAND_PROMPT(ILEN+1:)
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

	CALL CLI$GET_VALUE('SEPARATE',SEPARATE)

	IF (CLI$PRESENT('EDIT')) EDIT_DEFAULT = .TRUE.	! /EDIT switch test

	CALL FIND_BULLCP			! See if BULLCP is running

	IF (CLI$PRESENT('CLEANUP')) THEN	! Test for /CLEANUP switch
	   CALL CLI$GET_VALUE('CLEANUP',BULL_PARAMETER,LEN_P) ! Get folder #
	   READ (BULL_PARAMETER,'(I<LEN_P>)') FOLDER_NUMBER
	   CALL SELECT_FOLDER(.FALSE.,IER)	! Select folder
	   CALL CLEANUP_BULLFILE		! Cleanup empty blocks
	   CALL EXIT				! all done with cleanup
	ELSE IF (CLI$PRESENT('BBOARD')) THEN	! Test for /BBOARD switch
	   CALL BBOARD				! look for BBOARD mail
	   CALL EXIT				! all done with BBOARD
	ELSE IF (CLI$PRESENT('STARTUP').OR.	! BULLCP process control
     &	         CLI$PRESENT('STOP')) THEN
	   CALL CREATE_BULLCP
	ELSE IF (CLI$PRESENT('BULLCP')) THEN	! This is BULLCP, so start
	   CALL RUN_BULLCP			! doing what BULLCP does!
	END IF

	CALL GETSTS(STS)			! Get process status word

	IF (SYSTEM_SWITCH.OR.LOGIN_SWITCH) THEN	! If BULLETIN/LOGIN or /SYSTEM
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

	   IF (.NOT.TEST_BULLCP()) CALL DELETE_EXPIRED
						! Delete expired messages

C
C  Get page length for the terminal.
C

	   CALL GETPAGLEN(PAGE_LENGTH)

	   IF (CLI$PRESENT('PAGE')) PAGING = .TRUE.

	   IF (SYSTEM_SWITCH) THEN
	      IER = CLI$GET_VALUE('SYSTEM',BULL_PARAMETER,LEN_P)
	      IF (IER.NE.%LOC(CLI$_ABSENT)) THEN	! Days specified?
	         CALL SUBTIME(SYSTEM_LOGIN_BTIM,BULL_PARAMETER(:LEN_P),IER)
		 IF (.NOT.IER) THEN
		    WRITE (6,'('' ERROR: Invalid parameter in /SYSTEM.'')')
		    CALL EXIT
		 END IF
	      END IF
	      IF (.NOT.LOGIN_SWITCH) THEN
	         CALL MODIFY_SYSTEM_LIST(0)
		 CALL SHOW_SYSTEM
	         CALL EXIT
	      END IF
	   END IF

C
C  Get user info stored in SYS$LOGIN.  Currently, this simply stores
C  the time of the latest message read for each folder.
C

	   CALL OPEN_USERINFO

C
C  If /LOGIN, display SYSTEM bulletins and subject of non-SYSTEM bulletins.
C

	   IF (LOGIN_SWITCH.OR.SYSTEM_SWITCH) THEN	! Is /LOGIN present?
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
	INDEX_COUNT = 0

	IER = LIB$SYS_TRNLOG('BULL_HELP',HLEN,HELP_DIRECTORY)
	IF (IER.NE.1) THEN
	   HELP_DIRECTORY = 'SYS$HELP:'
	   HLEN = 9
	ELSE IF (HELP_DIRECTORY(HLEN:HLEN).NE.':'.AND.
     &		 HELP_DIRECTORY(HLEN:HLEN).NE.']') THEN
	   HELP_DIRECTORY = HELP_DIRECTORY(:HLEN)//':'
	   HLEN = HLEN + 1
	END IF

	DO WHILE (1)

	   CALL GET_INPUT_PROMPT(INCMD,IER,
     &		CHAR(10)//COMMAND_PROMPT(:TRIM(COMMAND_PROMPT)+1))

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
	      ELSE IF (INDEX_COUNT.GT.0) THEN
	         CALL FULL_DIR(INDEX_COUNT)
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
	   INDEX_COUNT = 0

	   CALL CLI$GET_VALUE('$VERB',INCMD)	! Get the VERB command
	   IF (READ_ONLY.AND.(INCMD(:3).EQ.'ADD'.OR.INCMD(:3).EQ.'DEL'
     &	     .OR.INCMD(:3).EQ.'CHA'.OR.INCMD(:3).EQ.'REP')) THEN
						! FOLDER can only be read?
	     WRITE (6,'('' ERROR: Access to folder limited to reading.'')')
	   ELSE IF (INCMD(:3).EQ.'ADD') THEN	! ADD bulletin command?
	     CALL ADD				! Go add bulletin
	   ELSE IF (INCMD(:4).EQ.'BACK') THEN	! BACK command?
	     IF (BULL_POINT.LE.1) THEN
	        WRITE(6,1060)
	     ELSE
	        CALL READ(READ_COUNT,BULL_POINT-1)  ! Try to read previous bull
	     END IF
	   ELSE IF (INCMD(:4).EQ.'CHAN') THEN		! CHANGE command?
	     CALL REPLACE				! Replace old bulletin
	   ELSE IF (INCMD(:4).EQ.'COPY') THEN		! COPY command?
	     CALL MOVE(.FALSE.)
	   ELSE IF (INCMD(:4).EQ.'CREA') THEN		! CREATE command?
	     CALL CREATE_FOLDER			! Go create the folder
	   ELSE IF (INCMD(:4).EQ.'CURR') THEN		! CURRENT command?
	     READ_COUNT = -1		! Reread current message from beginning.
	     CALL READ(READ_COUNT,BULL_POINT)
	   ELSE IF (INCMD(:4).EQ.'DELE') THEN 	! DELETE command?
	     CALL DELETE			! Go delete bulletin
	   ELSE IF (INCMD(:4).EQ.'DIRE') THEN		! DIRECTORY command?
	     IF (CLI$PRESENT('FOLDER')) THEN		! /FOLDER specified?
		CALL DIRECTORY_FOLDERS(FOLDER_COUNT)	! Show all folders
	     ELSE IF (CLI$PRESENT('SELECT_FOLDER')) THEN! Folder specified?
	        CALL SELECT_FOLDER(.TRUE.,IER)		! Try to select folder
	        IF (IER) THEN				! If successful
	           CALL DIRECTORY(DIR_COUNT)		! Show messages
		END IF
	     ELSE
	        CALL DIRECTORY(DIR_COUNT)		! Show messages
	     END IF
	   ELSE IF (INCMD(:4).EQ.'FILE'.OR.
     &		    INCMD(:4).EQ.'EXTR') THEN		! FILE command?
	     CALL FILE				! Copy bulletin to file
	   ELSE IF (INCMD(:1).EQ.'E'.OR.
     &		    INCMD(:4).EQ.'QUIT') THEN		! EXIT command?
	     GO TO 999				! Exit from program
	   ELSE IF (INCMD(:4).EQ.'HELP') THEN		! HELP command?
	     CALL HELP(HELP_DIRECTORY(:HLEN)//'BULL.HLB')	! Get help
	   ELSE IF (INCMD(:3).EQ.'IND') THEN		! INDEX command?
	     INDEX_COUNT = 1
	     CALL FULL_DIR(INDEX_COUNT)
	   ELSE IF (INCMD(:4).EQ.'LAST') THEN		! LAST command?
	     READ_COUNT = -1
	     BULL_READ = 99999
	     CALL READ(READ_COUNT,BULL_READ)
	   ELSE IF (INCMD(:4).EQ.'MAIL') THEN		! MAIL command?
	     CALL MAIL(MAIL_STATUS)
	   ELSE IF (INCMD(:3).EQ.'MOD') THEN		! MODIFY command?
	     CALL MODIFY_FOLDER
	   ELSE IF (INCMD(:4).EQ.'MOVE') THEN		! MOVE command?
	     CALL MOVE(.TRUE.)
	   ELSE IF (INCMD(:4).EQ.'NEXT') THEN		! NEXT command?
	     CALL READ(READ_COUNT,BULL_POINT+1)		! Read next bulletin
	   ELSE IF (INCMD(:4).EQ.'PRIN') THEN		! PRINT command?
	     CALL PRINT				! Printout bulletin
	   ELSE IF (INCMD(:4).EQ.'READ') THEN		! READ command?
	     IER = CLI$GET_VALUE('BULLETIN_NUMBER',BULL_PARAMETER,LEN_P)
	     IF (IER.NE.%LOC(CLI$_ABSENT)) THEN		! Bulletin specified?
	        DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER) BULL_READ	! Yes
		READ_COUNT = -1
		CALL READ(READ_COUNT,BULL_READ)
	     ELSE
		CALL READ(READ_COUNT,BULL_POINT+1)
	     END IF
	   ELSE IF (INCMD(:3).EQ.'REM') THEN		! REMOVE command?
	     CALL REMOVE_FOLDER
	   ELSE IF (INCMD(:3).EQ.'REP') THEN		! REPLY command?
	     IF (BULL_POINT.LT.1) THEN
		WRITE (6,'('' ERROR: No bulletin currently read.'')')
	     ELSE
	        WRITE (6,'('' Adding REPLY message with the subject:'')')
		CALL STR$UPCASE(BULL_PARAMETER,DESCRIP)
		IF (BULL_PARAMETER(:3).NE.'RE:') THEN
	           DESCRIP = 'RE: '//DESCRIP
		ELSE
	           DESCRIP = 'RE:'//DESCRIP(4:)
		END IF
	        WRITE (6,'(1X,A)') DESCRIP
	        CALL ADD
	     END IF
	   ELSE IF (INCMD(:3).EQ.'RES') THEN		! RESPOND command?
	     CALL RESPOND(MAIL_STATUS)
	   ELSE IF (INCMD(:3).EQ.'SEA') THEN		! SEARCH command?
	     CALL SEARCH(READ_COUNT)
	   ELSE IF (INCMD(:3).EQ.'SEL') THEN		! SELECT command?
	     CALL SELECT_FOLDER(.TRUE.,IER)
	   ELSE IF (INCMD(:3).EQ.'SET') THEN		! SET command?
	     CALL CLI$GET_VALUE('SET_PARAM1',BULL_PARAMETER)
	     IF (BULL_PARAMETER(:1).EQ.'F') THEN		! SET FOLDER?
		CALL SELECT_FOLDER(.TRUE.,IER)
	     ELSE IF (BULL_PARAMETER(:2).EQ.'PR') THEN		! SET PRIVS?
		CALL SET_PRIV
	     ELSE IF (BULL_PARAMETER(:2).EQ.'PA') THEN		! SET PAGE?
		PAGING = .TRUE.
		WRITE (6,'('' PAGE has been set.'')')
	     ELSE IF (BULL_PARAMETER(:2).EQ.'KE') THEN		! SET KEYPAD?
		CALL SET_KEYPAD
	     ELSE IF (BULL_PARAMETER(:3).EQ.'NOK') THEN		! SET NOKEYPAD?
		CALL SET_NOKEYPAD
	     ELSE IF (BULL_PARAMETER(:3).EQ.'NOP') THEN		! SET NOPAGE?
		PAGING = .FALSE.
		WRITE (6,'('' NOPAGE has been set.'')')
	     ELSE IF (FOLDER_NUMBER.EQ.-1) THEN
	        WRITE (6,'('' ERROR: Invalid command for remote folder.'')')
	     ELSE IF (BULL_PARAMETER(:2).EQ.'SY') THEN		! SET SYSTEM?
	 	CALL SET_SYSTEM(.TRUE.)
	     ELSE IF (BULL_PARAMETER(:4).EQ.'NOSY') THEN	! SET NOSYSTEM?
	 	CALL SET_SYSTEM(.FALSE.)
	     ELSE IF (BULL_PARAMETER(:2).EQ.'BB') THEN		! SET BBOARD?
		CALL SET_BBOARD(.TRUE.)
	     ELSE IF (BULL_PARAMETER(:4).EQ.'NOBB') THEN	! SET NOBBOARD?
		CALL SET_BBOARD(.FALSE.)
	     ELSE IF (BULL_PARAMETER(:2).EQ.'DU') THEN		! SET DUMP?
		CALL SET_FOLDER_FLAG(.TRUE.,1,'DUMP')
	     ELSE IF (BULL_PARAMETER(:4).EQ.'NODU') THEN	! SET NODUMP?
		CALL SET_FOLDER_FLAG(.FALSE.,1,'DUMP')
	     ELSE IF (BULL_PARAMETER(:3).EQ.'NOT') THEN	! SET NOTIFY?
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
	     ELSE IF (BULL_PARAMETER(:1).EQ.'E') THEN	! SET EXPIRE?
	        IER = CLI$GET_VALUE('EXPIRATION',BULL_PARAMETER,LEN_P)
		IF (LEN_P.LE.3) THEN
	           READ (BULL_PARAMETER,'(I<LEN_P>)') LIMIT
		   CALL SET_FOLDER_EXPIRE_LIMIT(LIMIT)
		ELSE
		   WRITE (6,'('' ERROR: Invalid expiration specified.'')')
		END IF
	     ELSE IF (BULL_PARAMETER(:4).EQ.'NODE') THEN	! SET NODE?
		CALL SET_NODE(.TRUE.)
	     ELSE IF (BULL_PARAMETER(:6).EQ.'NONODE') THEN	! SET NONODE?
		CALL SET_NODE(.FALSE.)
	     ELSE IF (BULL_PARAMETER(:3).EQ.'NOE') THEN	! SET NOEXPIRE?
		CALL SET_FOLDER_EXPIRE_LIMIT(0)
	     ELSE IF (BULL_PARAMETER(:5).EQ.'NONOT') THEN	! SET NONOTIFY?
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
	     ELSE IF (BULL_PARAMETER(:1).EQ.'S') THEN		! SET SHOWNEW?
	        IF (FOLDER_NUMBER.EQ.0) THEN
	         WRITE (6,'(
     &		 '' ERROR: SET SHOWNEW not allowed for GENERAL folder.'')')
		ELSE IF (CLI$PRESENT('DEFAULT')) THEN
		   CALL SET_FOLDER_DEFAULT(-1,0,1)
		ELSE IF (CLI$PRESENT('ALL')) THEN
		   IF (SETPRV_PRIV()) THEN
		      CALL SET_FOLDER_DEFAULT(-2,0,1)
		   ELSE
		      WRITE (6,'('' ERROR: /ALL is a privileged command.'')')
	 	   END IF
		ELSE
		   CALL CHANGE_FLAG(0,2)
		   CALL CHANGE_FLAG(1,3)
		END IF
	     ELSE IF (BULL_PARAMETER(:3).EQ.'NOS') THEN	! SET NOSHOWNEW?
	        IF (FOLDER_NUMBER.EQ.0) THEN
	         WRITE (6,'(
     &		 '' ERROR: SET NOSHOWNEW not allowed for GENERAL folder.'')')
		ELSE IF (CLI$PRESENT('DEFAULT')) THEN
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
	     ELSE IF (BULL_PARAMETER(:1).EQ.'R') THEN		! SET READNEW?
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
	     ELSE IF (BULL_PARAMETER(:3).EQ.'NOR') THEN	! SET NOREADNEW?
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
	     ELSE IF (BULL_PARAMETER(:2).EQ.'BR') THEN		! SET BRIEF?
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
	     ELSE IF (BULL_PARAMETER(:4).EQ.'NOBR') THEN	! SET NOBRIEF?
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
	     ELSE IF (BULL_PARAMETER(:1).EQ.'A') THEN		! SET ACCESS?
		CALL SET_ACCESS(.TRUE.)
	     ELSE IF (BULL_PARAMETER(:3).EQ.'NOA') THEN	! SET NOACCESS?
		CALL SET_ACCESS(.FALSE.)
	     ELSE IF (BULL_PARAMETER(:1).EQ.'G') THEN		! SET GENERIC?
		CALL SET_GENERIC(.TRUE.)
	     ELSE IF (BULL_PARAMETER(:3).EQ.'NOG') THEN	! SET NOGENERIC?
		CALL SET_GENERIC(.FALSE.)
	     ELSE IF (BULL_PARAMETER(:1).EQ.'L') THEN		! SET LOGIN?
		CALL SET_LOGIN(.TRUE.)
	     ELSE IF (BULL_PARAMETER(:3).EQ.'NOL') THEN	! SET NOLOGIN?
		CALL SET_LOGIN(.FALSE.)
	     END IF
	   ELSE IF (INCMD(:4).EQ.'SHOW') THEN		! SHOW command?
	     CALL CLI$GET_VALUE('SHOW_PARAM1',BULL_PARAMETER,LEN_P)
	     IF (BULL_PARAMETER(:2).EQ.'FL') THEN	! SHOW FLAGS?
		CALL SHOW_FLAGS
	     ELSE IF (BULL_PARAMETER(:2).EQ.'FO') THEN	! SHOW FOLDER?
	        CALL SHOW_FOLDER
	     ELSE IF (BULL_PARAMETER(:2).EQ.'KE') THEN	! SHOW KEYPAD
	        CALL SHOW_KEYPAD(HELP_DIRECTORY(:HLEN)//'BULL.HLB')
	     ELSE IF (BULL_PARAMETER(:2).EQ.'NE') THEN	! SHOW NEW?
		SAVE_FOLDER_NUMBER = FOLDER_NUMBER
		SAVE_FOLDER = FOLDER
		DO FOLDER_NUMBER = 0,FOLDER_MAX-1
	   	   IF (TEST2(SET_FLAG,FOLDER_NUMBER).OR.
     &		       TEST2(BRIEF_FLAG,FOLDER_NUMBER)) THEN
		      CALL SELECT_FOLDER(.FALSE.,IER)
		      IF (NBULL.GT.0) THEN
		        DIFF = COMPARE_BTIM(
     &			 LAST_READ_BTIM(1,FOLDER_NUMBER+1),F_NEWEST_BTIM)
		        IF (DIFF.LT.0) THEN
		         WRITE (6,'('' There are new messages in folder ''
     &			   ,A,''.'')') FOLDER(:TRIM(FOLDER))
			END IF
		      END IF
		   END IF
		END DO
		FOLDER1 = SAVE_FOLDER
		FOLDER_NUMBER = SAVE_FOLDER_NUMBER
		CALL SELECT_FOLDER(.FALSE.,IER)
	     ELSE IF (BULL_PARAMETER(:1).EQ.'P') THEN	! SHOW PRIVILEGES?
		CALL SHOW_PRIV
	     END IF
	   ELSE IF (INCMD(:4).EQ.'UNDE') THEN		! UNDELETE command?
	     CALL UNDELETE
	   END IF

100	   CONTINUE

	END DO

999	CALL EXIT

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

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	CHARACTER INEXDATE*11,INEXTIME*11
	CHARACTER*80 INDESCRIP,INPUT

	CHARACTER INLINE*80,OLD_FOLDER*25
    	CHARACTER PASSWORD*31,DEFAULT_USER*12

	EXTERNAL CLI$_ABSENT,CLI$_NEGATED

	CALL DISABLE_CTRL		! Disable CTRL-Y & -C

	ALLOW = SETPRV_PRIV()

	OLD_FOLDER_NUMBER = FOLDER_NUMBER
	OLD_FOLDER = FOLDER

	IER = CLI$GET_VALUE('FILESPEC',BULL_PARAMETER,LEN_P)
	IF (IER.NE.%LOC(CLI$_ABSENT)) THEN
 	   IF (.NOT.ALLOW) THEN		! If no SETPRV privileges, remove SYSPRV
	      CALL DISABLE_PRIVS	! privileges when trying to
	   END IF					! create new file.
	   OPEN (UNIT=3,FILE=BULL_PARAMETER(:LEN_P),STATUS='OLD',READONLY,
     &		 SHARED,ERR=920,FORM='FORMATTED')	! Try opening the file
	   CALL ENABLE_PRIVS	! Reset SYSPRV privileges
	ELSE IF (CLI$PRESENT('TEXT')) THEN
	   BULL_PARAMETER = 'SYS$LOGIN:BULL.SCR'
	   LEN_P = TRIM(BULL_PARAMETER)
	   OPEN(UNIT=3,FILE=BULL_PARAMETER(:LEN_P),IOSTAT=IER,
     &		STATUS='NEW',CARRIAGECONTROL='LIST',FORM='FORMATTED')

	   IF (IER.NE.0) THEN
	      CALL ERRSNS(IDUMMY,IER)
	      CALL SYS_GETMSG(IER)
	      GO TO 910
	   END IF

	   CALL OPEN_FILE_SHARED(1)

	   ILEN = 81
	   DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file
	      DO WHILE (ILEN.GT.0)
	         CALL GET_BULL(I,INPUT,ILEN)
	         IF (ILEN.LT.0) THEN
		    GO TO 90
	         ELSE IF (ILEN.GT.0) THEN
		    IF (CLI$PRESENT('NOINDENT')) THEN
	               WRITE (3,'(A)') INPUT(:ILEN)
		    ELSE
	               WRITE (3,'(A)') '>'//INPUT(:MIN(79,ILEN))
		       IF (ILEN.EQ.80) WRITE (3,'(A)') '>'//INPUT(80:)
		    END IF
	         END IF
	      END DO
	      ILEN = 80
	   END DO

90	   CALL CLOSE_FILE(1)
	END IF

	SELECT_FOLDERS = .FALSE.
	IF (CLI$PRESENT('SELECT_FOLDER')) THEN
	   CALL GET_FOLDER_INFO(IER)
	   IF (.NOT.IER) GO TO 910
	   SELECT_FOLDERS = .TRUE.
	ELSE
	   NODE_NUM = 1
	   NODES(1) = OLD_FOLDER
	END IF

	IER = CLI$GET_VALUE('USERNAME',DEFAULT_USER)
	IF (.NOT.IER) DEFAULT_USER = USERNAME
	IF (DECNET_PROC) THEN		! Running via DECNET?
	   USERNAME = DEFAULT_USER
	   CALL CONFIRM_PRIV(USERNAME,ALLOW)
	END IF

	IF (FOLDER_NUMBER.GT.0.AND.		! If folder set and
     &	    CLI$PRESENT('NODES')) THEN		! Decnet nodes specified?
	   WRITE (6,'('' ERROR: /NODES cannot be used with folder set.'')')
	   GO TO 910
	END IF

	IF (.NOT.BTEST(FOLDER_FLAG,2).AND.FOLDER_NUMBER.NE.0.AND.
     &	   (CLI$PRESENT('SYSTEM').OR.		! Is /SYSTEM switch present?
     &	    CLI$PRESENT('BROADCAST').OR.	! Is /BROADCAST swtich present?
     &	    CLI$PRESENT('SHUTDOWN'))) THEN	! Is /SHUTDOWN switch present?
	   WRITE (6,'('' ERROR: Folder is not a SYSTEM folder.'')')
	   GO TO 910
	END IF

	IF (CLI$PRESENT('SYSTEM')) THEN		! Is /SYSTEM switch present?
	   IF (.NOT.ALLOW) THEN			! If no privileges
	      WRITE(ERROR_UNIT,1070)		! Tell user
	      GO TO 910				! and abort
	   END IF
	   SYSTEM = 1				! Set system bit
	ELSE
	   SYSTEM = 0				! Clear system bit
	END IF

	IF (CLI$PRESENT('BROADCAST')) THEN	! Is /BROADCAST switch present?
	   IF (.NOT.(ALLOW.OR.OPER_PRIV())) THEN	! If no privileges
	      WRITE(ERROR_UNIT,1080)		! Tell user
	      GO TO 910				! and abort
	   END IF
	END IF

	IF (CLI$PRESENT('PERMANENT')) THEN	! Is /PERMANENT switch present?
	   IF (.NOT.ALLOW.AND..NOT.FOLDER_SET) THEN	! If no privileges
	      WRITE(ERROR_UNIT,1081)		! Tell user
	      GO TO 910				! and abort
	   ELSE IF (F_EXPIRE_LIMIT.GT.0.AND..NOT.ALLOW	! Expiration limit
     &		.AND.USERNAME.NE.FOLDER_OWNER) THEN	! is present
	      WRITE(ERROR_UNIT,1083)
	      GO TO 910
	   ELSE
	      SYSTEM = SYSTEM.OR.2		! Set permanent bit
	      INEXDATE = '5-NOV-2000'
	      INEXTIME = '00:00:00.00'
	   END IF
	END IF

	IF (CLI$PRESENT('SHUTDOWN')) THEN	! Is /SHUTDOWN switch present?
	   IF (.NOT.ALLOW) THEN			! If no privileges
	      WRITE(ERROR_UNIT,1082)		! Tell user
	      GO TO 910				! and abort
	   ELSE
	      SYSTEM = SYSTEM.OR.4		! Set shutdown bit
	      INEXDATE = '5-NOV-2000'
	      CALL GET_NODE_NUMBER(NODE_NUMBER,NODE_AREA)
	      WRITE (INEXTIME,'(I4)') NODE_NUMBER
	      WRITE (INEXTIME(7:),'(I4)') NODE_AREA
	      DO I=1,11
		 IF (INEXTIME(I:I).EQ.' ') INEXTIME(I:I) = '0'
	      END DO
	      INEXTIME = INEXTIME(1:2)//':'//INEXTIME(3:4)//':'//
     &			 INEXTIME(7:8)//'.'//INEXTIME(9:10)
	   END IF
	END IF

	SELECT_NODES = .FALSE.
	IF (CLI$PRESENT('NODES')) THEN
	   CALL GET_NODE_INFO
	   IF (NODE_ERROR) GO TO 940
	   SELECT_NODES = .TRUE.
	END IF

	IF (SYSTEM.LE.1) THEN			! Not permanent or shutdown
	   CALL GET_EXPIRED(INPUT,IER)
	   IF (.NOT.IER) GO TO 910
	   INEXDATE = INPUT(:11)
	   INEXTIME = INPUT(13:)
	END IF

	IF (INCMD(:3).EQ.'REP') THEN		! REPLY command?
	   INDESCRIP = DESCRIP			! Use descrption with RE:,
	   LENDES = TRIM(INDESCRIP)		! filled in by main subroutine
	ELSE IF (CLI$PRESENT('SUBJECT')) THEN	! /SUBJECT specified
	   CALL CLI$GET_VALUE('SUBJECT',INDESCRIP,LENDES)
	   IF (LENDES.GT.53) THEN		! If too many characters
	      WRITE(6,1060)			! tell user
	      WRITE(6,2020) INDESCRIP(:53)	! Show how much would fit
	      GO TO 910
	   END IF
	ELSE
	   LENDES = 54
	   DO WHILE (LENDES.GT.53)		! Do until valid description
	      WRITE(6,1050)			! Request header for bulletin
	      CALL GET_LINE(INDESCRIP,LENDES)	! Get input line
	      IF (LENDES.LE.0) GO TO 910
	      IF (LENDES.GT.53) THEN		! If too many characters
	         WRITE(6,1060)			! tell user
	         WRITE(6,2020) INDESCRIP(:53)	! Show how much would fit
	      END IF
	   END DO
	END IF

C
C  If file specified in ADD command, read file to obtain bulletin.
C  Else, read the bulletin from the terminal.
C
	
	ICOUNT = 0				! Line count for bulletin

	IF ((CLI$PRESENT('EDIT').OR.EDIT_DEFAULT).AND.	! If /EDIT specified
     &      (CLI$PRESENT('EDIT').NE.%LOC(CLI$_NEGATED))) THEN
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
	      READ (3,'(Q,A)',END=10) ILEN,INPUT! get record count
	      IF (ILEN.GT.80) GO TO 950
	      ICOUNT = ICOUNT + 1 + MIN(ILEN,80)
	      IF (ILEN.EQ.0) ICOUNT = ICOUNT + 1! COPY_BULL writes line with
	   END DO				! 1 space for blank line
	ELSE					! If no input file
	   OPEN (UNIT=3,STATUS='SCRATCH',FILE='SYS$LOGIN:BULL.SCR',
     &		FORM='FORMATTED')		! Sratch file to save bulletin
	   WRITE (6,1000)		! Request bulletin input from terminal
	   ILEN = 81				! Length of input line
	   DO WHILE (ILEN.GE.0)			! Input until no more input
	      CALL GET_LINE(INPUT,ILEN)		! Get input line
	      IF (ILEN.GT.80) THEN		! Input line too long
		 WRITE(6,'('' ERROR: Input line length > 80.  Reinput:'')')
	      ELSE IF (ILEN.GE.0) THEN		! If good input line entered
		 ICOUNT = ICOUNT + 1 + ILEN	! Increment record count
		 IF (ILEN.EQ.0) ICOUNT = ICOUNT + 1
		 WRITE(3,2010) INPUT(:ILEN)	! Save line in scratch file
	      END IF
	   END DO
	   IF (ILEN.EQ.-1) GO TO 910		! CTRL_C entered, error out
10	   IF (ICOUNT.EQ.0) GO TO 910		! No lines entered, error out
	ENDIF

	REWIND (UNIT=3)

	IF (SELECT_NODES.AND.NODE_NUM.GT.0) THEN
	   INLINE = 'ADD'
	   IF (CLI$PRESENT('SYSTEM'))
     &	      INLINE = INLINE(:STR$POSITION(INLINE,' ')-1)//'/SYSTEM'
	   IF (CLI$PRESENT('BROADCAST'))
     &	      INLINE = INLINE(:STR$POSITION(INLINE,' ')-1)//'/BROADCAST'
	   IF (CLI$PRESENT('PERMANENT'))
     &	      INLINE = INLINE(:STR$POSITION(INLINE,' ')-1)//'/PERMANENT'
	   IF (CLI$PRESENT('SHUTDOWN'))
     &	      INLINE = INLINE(:STR$POSITION(INLINE,' ')-1)//'/SHUTDOWN'
	   IF (CLI$PRESENT('BELL'))
     &	      INLINE = INLINE(:STR$POSITION(INLINE,' ')-1)//'/BELL'

	   LEN_INLINE = STR$POSITION(INLINE,' ') - 1

	   DO POINT_NODE=1,NODE_NUM	   	! Write out command to nodes
	      INLINE = INLINE(:LEN_INLINE)
	      SEMI = INDEX(NODES(POINT_NODE),'::')	! Look for semicolons
	      ILEN = TRIM(NODES(POINT_NODE))		! Length of node name
	      IF (SEMI.GT.0) THEN			! Are semicolon found?
	         IF (ILEN.GT.SEMI+1) THEN		! Is username found?
	            TEMP_USER = NODES(POINT_NODE)(SEMI+2:)	! Yes
	            ILEN = SEMI - 1			! Remove semicolons
	         ELSE					! No username found...
		    TEMP_USER = DEFAULT_USER		! Set user to default
	            ILEN = SEMI - 1			! Remove semicolons
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
		 IF (TRIM(PASSWORD).EQ.0) GO TO 910
	         OPEN (UNIT=10+NODE_NUM,NAME=NODES(POINT_NODE)(:ILEN)//
     &		   '"'//TEMP_USER(:TRIM(TEMP_USER))//' '//
     &		   PASSWORD(:TRIM(PASSWORD))//'"::',
     &		   TYPE='SCRATCH',IOSTAT=IER)
		 CLOSE (UNIT=10+NODE_NUM)
		 IF (IER.NE.0) THEN
		    WRITE (6,'('' ERROR: Password is invalid.'')')
		 END IF
	      END DO
	      INLINE = INLINE(:STR$POSITION(INLINE,' ')-1)
     &					//'/USERNAME='//TEMP_USER
	      WRITE (POINT_NODE+9,'(A)',ERR=940) INLINE
	      IF (SYSTEM.LE.1)	! If not permanent or shutdown specify date
     &		WRITE (POINT_NODE+9,'(A)',ERR=940) INEXDATE//' '//INEXTIME
	      WRITE (POINT_NODE+9,'(A)',ERR=940) INDESCRIP(:LENDES)
	      IER = 0
	      DO WHILE (IER.EQ.0)
	         READ (3,'(Q,A)',IOSTAT=IER) ILEN,INPUT
		 ILEN = MIN(ILEN,80)
		 IF (IER.EQ.0) THEN
		    WRITE (POINT_NODE+9,'(A)',ERR=940) INPUT(:ILEN)
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

	IF (SELECT_NODES.AND..NOT.LOCAL_NODE_FOUND) GO TO 95
					! Exit if local node not specified.

	IF (.NOT.SELECT_FOLDERS) THEN
	   NODE_NUM = 1				! No folders specified so just
	   NODES(1) = FOLDER			! add to select folder
	END IF


C
C  Add bulletin to bulletin file and directory entry for to directory file.
C
	BRDCST = .FALSE.

	DO I = 1,NODE_NUM

	   IF (FOLDER.NE.NODES(I)) THEN
	      FOLDER_NUMBER = -1
	      FOLDER1 = NODES(I)
	      CALL SELECT_FOLDER(.FALSE.,IER)
	   ELSE
	      IER = 1
	   END IF
	   
	   IF (IER) THEN
	      CALL OPEN_FILE(2)			! Prepare to add dir entry

	      DESCRIP=INDESCRIP(:LENDES)	! Description header
	      EXDATE=INEXDATE			! Expiration date
	      EXTIME=INEXTIME
	      LENGTH = (ICOUNT+127)/128		! Number of records
	      FROM = USERNAME			! Username

	      CALL OPEN_FILE(1)			! Prepare to add bulletin

	      CALL READDIR(0,IER)		! Get NBLOCK
	      IF (IER.EQ.0) NBLOCK = 0		! If new file, NBLOCK is 0

	      REWIND (UNIT=3)
	      CALL COPY_BULL(3,1,NBLOCK+1,IER)	! Add the new bulletin
	      IF (IER.NE.0) GO TO 930		! Error in creating bulletin
C
C  Broadcast the bulletin if requested.
C
	      IF (.NOT.BRDCST.AND.CLI$PRESENT('BROADCAST').AND.
     &		 (.NOT.REMOTE_SET.OR.FOLDER_NUMBER.GT.0)) THEN
		 CALL GET_BROADCAST_MESSAGE(CLI$PRESENT('BELL'))
		 BRDCST = .TRUE.
	         IF (.NOT.CLI$PRESENT('LOCAL')) THEN
	            CALL BROADCAST_ALL_NODES(CLI$PRESENT('ALL'),
     &			CLI$PRESENT('CLUSTER'))
		 END IF
	         CALL BROADCAST(
     &			CLI$PRESENT('ALL'),CLI$PRESENT('CLUSTER'))
	      END IF

	      CALL CLOSE_FILE(1)		! Finished adding bulletin

	      CALL ADD_ENTRY			! Add the new directory entry

	      IF (FOLDER_NUMBER.GE.0) THEN
	         CALL UPDATE_FOLDER		! Update info in folder file
C
C  If user is adding message, update that user's last read time for
C  folder, so user is not alerted of new message which is owned by user.
C
	         LAST_READ_BTIM(1,FOLDER_NUMBER+1) = F_NEWEST_BTIM(1)
	         LAST_READ_BTIM(2,FOLDER_NUMBER+1) = F_NEWEST_BTIM(2)
	      END IF

	      CALL CLOSE_FILE(2)		! Totally finished with add
	   ELSE
	      WRITE (6,'('' ERROR: Unable to add message to '',A)')
     &				NODES(I)
	   END IF
	END DO

95	CLOSE (UNIT=3)			! Close the input file
	IF (DECNET_PROC) WRITE(5,'(''END'')') ! DECNET operation worked

100	CALL ENABLE_CTRL		! Enable CTRL-Y & -C
	DO I=10,NODE_NUM+9
	   CLOSE (UNIT=I)
	END DO

	IF (FOLDER_NUMBER.NE.OLD_FOLDER_NUMBER) THEN
	   FOLDER_NUMBER = OLD_FOLDER_NUMBER
	   FOLDER1 = OLD_FOLDER
	   CALL SELECT_FOLDER(.FALSE.,IER)
	END IF

	IF (CLI$PRESENT('TEXT')) THEN
	   CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;*')
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
1083	FORMAT (' ERROR: Folder has expiration limit.')
2010	FORMAT(A)
2020	FORMAT(1X,A)

	END


	SUBROUTINE SUBTIME(BTIM,DAYS_BEFORE_TODAY,IER)

	IMPLICIT INTEGER (A-Z)

	CHARACTER DAYS_BEFORE_TODAY*(*),TODAY_DATE*23

	INTEGER BTIM(2),TODAY_BTIM(2)

	IER = SYS$BINTIM(DAYS_BEFORE_TODAY,BTIM)
	IF (.NOT.IER) RETURN

	BTIM(1) = -BTIM(1)		! Convert to negative delta time
	BTIM(2) = -BTIM(2)-1

	IER = SYS$ASCTIM(TLEN,TODAY_DATE,,)
	CALL SYS$BINTIM(TODAY_DATE(:TLEN),TODAY_BTIM)

	CALL LIB$SUBX(TODAY_BTIM,BTIM,BTIM)

	RETURN
	END



	SUBROUTINE BROADCAST_ALL_NODES(ALL,CLUSTER)

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	PARAMETER BRDCST_LIMIT = 82*12 + 2
	CHARACTER*(BRDCST_LIMIT) BMESSAGE

	COMMON /BROAD_MESSAGE/ BMESSAGE,BLENGTH

	COMMON /SYSTEM_FOLDERS/ SYSTEM_FLAG(FLONG),NODENAME
	CHARACTER NODENAME*8

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	CHARACTER*8 LOCALNODE

	IF (.NOT.TEST_BULLCP().OR.REMOTE_SET) RETURN

	CALL OPEN_FILE_SHARED(4)

	REMOTE_FOUND = .FALSE.
	TEMP_USER = ':'

	DO WHILE (.NOT.REMOTE_FOUND)
	   DO WHILE (REC_LOCK(IER))		 
	      READ (4,KEYGT=TEMP_USER,IOSTAT=IER)
     &		TEMP_USER,LOGIN_BTIM,READ_BTIM,NEW_FLAG
	   END DO
	   IF (TEMP_USER(:1).NE.':') THEN
	      CALL CLOSE(4)
	      RETURN
 	   END IF
	   REMOTE_FOUND = TEST2(NEW_FLAG,FOLDER_NUMBER)
	END DO

	CALL CLOSE (4)

	OPEN (UNIT=17,STATUS='UNKNOWN',IOSTAT=IER,RECL=256,
     &		FILE=NODENAME(:TRIM(NODENAME))//'::"TASK=BULLETIN1"')

	IF (IER.EQ.0) THEN
	   IER = 0
	   I = 1
	   DO WHILE (IER.EQ.0.AND.I.LT.BLENGTH)
	      WRITE (17,'(4A)',IOSTAT=IER)
     &		15,-1,I,BMESSAGE(I:MIN(BLENGTH,I+127))
	       I = I + 128
	   END DO
	   IF (IER.EQ.0) WRITE (17,'(7A)',IOSTAT=IER)
     &		15,BLENGTH,I,ALL,CLUSTER,FOLDER_NUMBER,FOLDER
	END IF

	CLOSE (UNIT=17)

	RETURN
	END



	INTEGER FUNCTION ERROR_TRAP

	ERROR_TRAP = 1

	RETURN
	END
