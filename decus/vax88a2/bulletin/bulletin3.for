C
C  BULLETIN3.FOR, Version 3/28/88
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Programmer: Mark R. London
C
	SUBROUTINE UPDATE
C
C  SUBROUTINE UPDATE
C
C  FUNCTION:  Searches for bulletins that have expired and deletes them.
C
C  NOTE:  Assumes directory file is already opened.
C
	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLDIR.INC'

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	CHARACTER*107 DIRLINE

	CHARACTER*11 TEMP_DATE,TEMP_EXDATE
	CHARACTER*8 TEMP_TIME,TEMP_EXTIME

	IF (TEST_BULLCP().OR.REMOTE_SET) RETURN
					! BULLCP cleans up expired bulletins

	ENTRY UPDATE_ALWAYS		! Entry to skip BULLCP test

	TEMP_EXDATE = '5-NOV-2000'  ! If a bulletin gets deleted, and there are
	TEMP_EXTIME = '00:00:00'    ! are no more bulletins, this is the value
				    ! assigned to the latest expiration date

	TEMP_DATE = '5-NOV-1956' 	! Storage for computing newest
	TEMP_TIME = '00:00:00'		! bulletin date if deletion occurs

	BULL_ENTRY = 1				! Init bulletin pointer
	UPDATE_DONE = 0			! Flag showing bull has been deleted

	DO WHILE (1)
	   CALL READDIR(BULL_ENTRY,IER)		! Get next directory entry
	   IF (IER.EQ.BULL_ENTRY) GO TO 100	! ERROR: Not found
	   IF (SYSTEM.LE.3.OR.(SHUTDOWN.EQ.0	! If not shutdown, or time
     &	     .AND.(SYSTEM.AND.4).EQ.4)) THEN	! to delete shutdowns?
	    IF ((SYSTEM.AND.4).EQ.4) THEN	! Shutdown bulletin?
	       DIFF = 0				! If so, delete it
	    ELSE
	       DIFF = COMPARE_DATE(EXDATE,' ')	! Has expiration date passed?
	       IF (DIFF.EQ.0) DIFF = COMPARE_TIME(EXTIME,' ')
	    END IF
	    IF (DIFF.LE.0) THEN			! If so then delete bulletin
	      CALL DELETE_ENTRY(BULL_ENTRY)	! Delete bulletin entry
	      IF (UPDATE_DONE.EQ.0) THEN	! If this is first deleted file
	         UPDATE_DONE = BULL_ENTRY	! store it to use for reordering
	      END IF				! directory file.
	    ELSE IF (SYSTEM.LE.3) THEN		! Expiration date hasn't passed
		! If a bulletin is deleted, we'll have to update the latest
		! expiration date. The following does that.
	      DIFF = COMPARE_DATE(EXDATE,TEMP_EXDATE)
	      IF (DIFF.LT.0.OR.(DIFF.EQ.0.AND.
     &		COMPARE_TIME(EXTIME,TEMP_EXTIME).LT.0)) THEN
	         TEMP_EXDATE = EXDATE		! If this is the latest exp
	         TEMP_EXTIME = EXTIME		! date seen so far, save it.
	      END IF
	      TEMP_DATE = DATE			! Keep date so when we quit
	      TEMP_TIME = TIME			! search, we'll have the
	    END IF				! latest bulletin date
	   ELSE
	      TEMP_DATE = DATE
	      TEMP_TIME = TIME
	   END IF
	   BULL_ENTRY = BULL_ENTRY + 1
	END DO

100	IF (UPDATE_DONE.GT.0) THEN		! Reorder directory file
	   CALL CLEANUP_DIRFILE(UPDATE_DONE)	! due to deleted entries
	END IF

	DATE = NEWEST_DATE
	TIME = NEWEST_TIME
	NEW_SHUTDOWN = SHUTDOWN
	CALL READDIR(0,IER)
	SHUTDOWN = NEW_SHUTDOWN
	NEWEST_EXDATE = TEMP_EXDATE
	NEWEST_EXTIME = TEMP_EXTIME
	NEWEST_DATE = TEMP_DATE
	NEWEST_TIME = TEMP_TIME
	CALL WRITEDIR(0,IER)
	CALL UPDATE_FOLDER
C
C  If newest message date has been changed, must change it in BULLUSER.DAT
C  and also see if it affects notification of new messages to users
C
	IF (TEMP_DATE.NE.DATE.OR.TEMP_TIME.NE.TIME) THEN
	   CALL UPDATE_LOGIN(.FALSE.)
	END IF

	RETURN

	END



	SUBROUTINE UPDATE_READ
C
C  SUBROUTINE UPDATE_READ
C
C  FUNCTION:
C	Store the latest date that user has used the BULLETIN facility.
C	If new bulletins have been added, alert user of the fact.
C

	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLDIR.INC'

	INCLUDE '($PRVDEF)'

	CHARACTER TODAY*23

	DIMENSION TODAY_BTIM(2),READ_BTIM_SAVE(2)

C
C  Update user's latest read time in his entry in BULLUSER.DAT.
C

	CALL OPEN_FILE_SHARED(4)		! Get BULLUSER.DAT file

	CALL READ_USER_FILE_HEADER(IER)

	IF (IER.NE.0) THEN			! If header not present, exit
	   CALL CLOSE_FILE(4)
	   RETURN
	ELSE IF (USERPRIV(1).EQ.-1.AND.USERPRIV(2).EQ.-1) THEN
						! If header present, but no
	   DO I=1,FLONG				! SET_FLAG and NOTIFY_FLAG
	      SET_FLAG_DEF(I) = 0		! information, write default
	      NOTIFY_FLAG_DEF(I) = 0		! flags.
	      BRIEF_FLAG_DEF(I) = 0
	   END DO
	   SET_FLAG_DEF(1) = 1
	   USERPRIV(1) = PRV$M_OPER.OR.PRV$M_CMKRNL.OR.PRV$M_SETPRV
	   USERPRIV(2) = 0
	   REWRITE (4) USER_HEADER
	END IF

	CALL SYS$ASCTIM(,TODAY,,)		! Get today's time
	CALL SYS_BINTIM(TODAY,TODAY_BTIM)

	UNLOCK 4

	CALL READ_USER_FILE_KEYNAME(USERNAME,IER1)

	IF (IER1.EQ.0) THEN			! If entry found, update it
	   READ_BTIM_SAVE(1) = READ_BTIM(1)
	   READ_BTIM_SAVE(2) = READ_BTIM(2)
	   READ_BTIM(1) = TODAY_BTIM(1)
	   READ_BTIM(2) = TODAY_BTIM(2)
	   REWRITE (4) USER_ENTRY
	   READ_BTIM(1) = READ_BTIM_SAVE(1)
	   READ_BTIM(2) = READ_BTIM_SAVE(2)
	ELSE					! If no entry create a new entry
	   NEW_FLAG(1) = 143
	   NEW_FLAG(2) = 0
	   LOGIN_BTIM(1) = TODAY_BTIM(1)
	   LOGIN_BTIM(2) = TODAY_BTIM(2)
	   READ_BTIM(1) = TODAY_BTIM(1)
	   READ_BTIM(2) = TODAY_BTIM(2)
	   CALL WRITE_USER_FILE_NEW(IER)
	END IF

	CALL CLOSE_FILE(4)			! All finished with BULLUSER

	RETURN					! to go home...

	END




	SUBROUTINE FIND_NEWEST_BULL
C
C  SUBROUTINE FIND_NEWEST_BULL
C
C	If new bulletins have been added, alert user of the fact and
C	set the next bulletin to be read to the first new bulletin.
C
C  OUTPUTS:
C	BULL_POINT  -  If -1, no new bulletins to read, else there are.
C

	IMPLICIT INTEGER (A - Z)

	COMMON /POINT/ BULL_POINT

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'

	INTEGER DIR_BTIM(2)

C
C  Now see if bulletins have been added since the user's previous
C  read time.  If they have, then search for the first new bulletin.
C  Ignore new bulletins that are owned by the user or system notices
C  that have not been added since the user has logged in.
C
	BULL_POINT = -1				! Init bulletin pointer

	CALL OPEN_FILE_SHARED(2)		! Yep, so get directory file
	CALL READDIR(0,IER)			! Get # bulletins from header
	IF (IER.EQ.1) THEN
	   CALL GET_NEWEST_MSG(LAST_READ_BTIM(1,FOLDER_NUMBER+1),START)
	   IF (START.LE.0) THEN
	      BULL_POINT = START
	      CALL CLOSE_FILE(2)
	      RETURN
	   ELSE
	      START = START + 1
	   END IF
	   DO WHILE (START.LE.NBULL.AND.(FROM.EQ.USERNAME.OR.SYSTEM))
	      IF (FROM.NE.USERNAME) THEN	! Ignore bull if owner is user
	         IF (SYSTEM) THEN		! If system bulletin
	            CALL SYS_BINTIM(DATE//' '//TIME,DIR_BTIM)
	            DIFF = COMPARE_BTIM(LOGIN_BTIM,DIR_BTIM)
		    IF (DIFF.GT.0) THEN
		       START = START + 1
	               CALL READDIR(START,IER)
		    ELSE			! SYSTEM bulletin was not seen
		       SYSTEM = 0		! so force exit to read it.
		    END IF
	         END IF
	      ELSE
		 START = START + 1
		 CALL READDIR(START,IER)
	      END IF
	   END DO
	   IF (START.LE.NBULL) BULL_POINT = START - 1
	END IF

	CALL CLOSE_FILE(2)

	RETURN
	END



	SUBROUTINE GET_EXPIRED(INPUT,IER)

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	CHARACTER*20 INPUT
	CHARACTER*23 TODAY

	DIMENSION EXTIME(2),NOW(2)

	EXTERNAL CLI$_ABSENT

	IER = SYS$ASCTIM(,TODAY,,)		! Get today's date

	IERC = CLI$GET_VALUE('EXPIRATION',INPUT,ILEN)

	PROMPT = .TRUE.

5	IF (PROMPT) THEN
	   IF (IERC.NE.%LOC(CLI$_ABSENT)) THEN	! Was value specified?
	      PROMPT = .FALSE.
	   ELSE
	      WRITE(6,1030) TODAY		! Prompt for expiration date
	      CALL GET_LINE(INPUT,ILEN)		! Get input line
	   END IF
	ELSE
	   RETURN
	END IF

	IF (ILEN.LE.0) THEN
	   IER = 0
	   RETURN
	END IF

	INPUT = INPUT(:ILEN)			! Change trailing zeros 2 spaces

	IF (INDEX(INPUT,'-').EQ.0.AND.INDEX(INPUT,':').GT.0.AND.
     &		INDEX(INPUT(:ILEN),' ').EQ.0) THEN
	   INPUT = TODAY(:INDEX(TODAY(2:),' ')+1)//INPUT
	END IF

	CALL STR$UPCASE(INPUT,INPUT)		! Convert to upper case
	IER = SYS_BINTIM(INPUT,EXTIME)
	IF (IER.NE.1) THEN			! If not able to do so
    	   WRITE(6,1040)			! tell user is wrong
	   GO TO 5
	END IF
	IER = SYS$ASCTIM(TIMLEN,INPUT,EXTIME,)
	IF (TIMLEN.EQ.16) THEN
	   CALL SYS$GETTIM(NOW)
	   CALL LIB$SUBX(NOW,EXTIME,EXTIME)
	   IER = SYS$ASCTIM(TIMLEN,INPUT,EXTIME,)
	END IF

	IF (INPUT(2:2).EQ.'-') INPUT = '0'//INPUT
	IER = COMPARE_DATE(INPUT(:11),TODAY(:11)) ! Compare date with today's
	IF (IER.GT.F_EXPIRE_LIMIT.AND.F_EXPIRE_LIMIT.GT.0.AND.
     &		.NOT.SETPRV_PRIV().AND.USERNAME.NE.FOLDER_OWNER) THEN
	   WRITE(6,1050) F_EXPIRE_LIMIT		! Expiration date > limit
	   GO TO 5
	END IF
	IF (IER.EQ.0) IER = COMPARE_TIME(INPUT(13:20),TODAY(13:20))
	IF (IER.LE.0) THEN			! If expiration date not future
	   WRITE(6,1045)			! tell user
	   GO TO 5				! and re-request date
	END IF

	IER = 1

	RETURN

1030	FORMAT(' It is ',A23,
     &'. Specify when the message should expire:',/,1x,
     &'Enter absolute time: [dd-mmm-yyyy] hh:mm:ss ',
     &'or delta time: dddd hh:mm:ss')
1040	FORMAT(' ERROR: Invalid date format specified.')
1045	FORMAT(' ERROR: Specified time has already passed.')
1050	FORMAT(' ERROR: Specified expiration period too large.
     & Limit is ',I3,' days.')

	END


	SUBROUTINE MAILEDIT(INFILE,OUTFILE)

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($SSDEF)'

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD

	EXTERNAL BULLETIN_SUBCOMMANDS

	CHARACTER*(*) INFILE,OUTFILE

	CHARACTER*80 MAIL_EDIT,OUT

	IER = SYS_TRNLNM('MAIL$EDIT',MAIL_EDIT)
	IF (IER.NE.SS$_NORMAL) MAIL_EDIT = 'SYS$SYSTEM:MAILEDIT'

	OUT = OUTFILE
	IF (TRIM(OUT).EQ.0) THEN
	   OUT = INFILE
	END IF

	IF (INDEX(MAIL_EDIT,'CALLABLE_').EQ.0) THEN
	   CALL DISABLE_PRIVS
	   CALL LIB$SPAWN('$@'//MAIL_EDIT(:TRIM(MAIL_EDIT))
     &		//' '//INFILE//' '//OUT(:TRIM(OUT)))
	   CALL ENABLE_PRIVS
	ELSE IF (INDEX(MAIL_EDIT,'EDT').GT.0) THEN
	   CALL EDT$EDIT(INFILE,OUT)
	ELSE IF (INDEX(MAIL_EDIT,'TPU').GT.0) THEN
	   CALL TPU$EDIT(INFILE,OUT)
	   IER = CLI$DCL_PARSE(INCMD,BULLETIN_SUBCOMMANDS)
		! TPU does CLI$ stuff which wipes our parsed command line
	END IF

	RETURN
	END





	SUBROUTINE CREATE_BULLCP

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($PRCDEF)'

	INCLUDE '($JPIDEF)'

	INCLUDE '($SSDEF)'

	INCLUDE 'BULLFILES.INC'

	COMMON /PRIVILEGES/ PROCPRIV(2),NEEDPRIV(2)

	CHARACTER IMAGENAME*132,ANSWER*1,PRCNAM*15

	DIMENSION SAVEPRIV(2)

	CALL DISABLE_PRIVS	! Just let real privileged people do a /STARTUP

	CALL SYS$SETPRV(%VAL(1),PROCPRIV,,SAVEPRIV)	! Enable original priv

	IF (TEST_BULLCP()) THEN
	   WRITE (6,'('' BULLCP process running.
     & Do you wish to kill it and restart a new one? '',$)')
	   READ (5,'(A)') ANSWER
	   IF (ANSWER.NE.'Y'.AND.ANSWER.NE.'y') CALL EXIT

	   WILDCARD = -1

	   CALL INIT_ITMLST	! Initialize item list
				! Now add items to list
	   CALL ADD_2_ITMLST(LEN(PRCNAM),JPI$_PRCNAM,%LOC(PRCNAM))
	   CALL ADD_2_ITMLST(4,JPI$_PID,%LOC(PID))
	   CALL END_ITMLST(GETJPI_ITMLST)	! Get address of itemlist
	   IER = 1
	   DO WHILE (IER.AND.PRCNAM(:6).NE.'BULLCP')
						! Get next interactive process
	      IER = SYS$GETJPIW(,WILDCARD,,%VAL(GETJPI_ITMLST),,,,)
						! Get next process.
	   END DO
	   IF (IER.AND.PID.NE.0) IER = SYS$DELPRC(PID,)
	   IF (.NOT.IER) THEN
	      CALL SYS_GETMSG(IER)
	      CALL EXIT
	   END IF
	END IF

	CALL GETIMAGE(IMAGENAME,ILEN)

	LEN_B = TRIM(FOLDER_DIRECTORY)

	CALL SYS$SETDFPROT('AA00'X,CUR_DEF_PROT)
		! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD:RW,GROUP:RW)
	OPEN(UNIT=11,FILE=FOLDER_DIRECTORY(:LEN_B)//'BULLCP.COM',
     &		STATUS='NEW',IOSTAT=IER,CARRIAGECONTROL='LIST')
	IF (IER.NE.0) RETURN
	WRITE(11,'(A)') '$SET NOON'
	WRITE(11,'(A)') '$B:=$'//IMAGENAME(:ILEN)
	WRITE(11,'(A)') '$LOOP:'
	WRITE(11,'(A)') '$B/BULLCP'
	WRITE(11,'(A)') '$GOTO LOOP'		! File open timed out
	CLOSE(UNIT=11)
	CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection

	IER = 0
	DO WHILE (IER.EQ.0.OR.(IER.EQ.SS$_DUPLNAM.AND.PID.GT.0))
	   IER = SYS$CREPRC(,'SYS$SYSTEM:LOGINOUT',
     &	      FOLDER_DIRECTORY(:LEN_B)//'BULLCP.COM','NL:'
     &	      ,,,,'BULLCP',%VAL(4),,,%VAL(PRC$M_NOUAF+PRC$M_DETACH))
	END DO

	IF (IER) THEN
	   OPEN(UNIT=11,FILE=FOLDER_DIRECTORY(:LEN_B)//'BULLCP.COM;-1',
     &		STATUS='OLD',IOSTAT=IER1)
	   IF (IER1.EQ.0) CLOSE(UNIT=11,STATUS='DELETE',IOSTAT=IER1)
	END IF

	CALL SYS$SETPRV(%VAL(0),SAVEPRIV,,)	! Reset privs

	CALL ENABLE_PRIVS

	IF (.NOT.IER) THEN
	   CALL SYS_GETMSG(IER)
	ELSE
	   WRITE (6,'('' Successfully created BULLCP detached process.'')')
	END IF
	CALL EXIT

	END



	SUBROUTINE FIND_BULLCP

	IMPLICIT INTEGER (A-Z)

	COMMON /BCP/ BULLCP
	LOGICAL BULLCP /.FALSE./

	CHARACTER*1 DUMMY

	IER = SYS_TRNLNM('BULL_BULLCP',DUMMY)
	IF (IER) BULLCP = .TRUE.

	RETURN
	END




	LOGICAL FUNCTION TEST_BULLCP

	IMPLICIT INTEGER (A-Z)

	COMMON /BCP/ BULLCP
	LOGICAL BULLCP

	TEST_BULLCP = BULLCP

	RETURN
	END




	SUBROUTINE RUN_BULLCP

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	COMMON /BCP/ BULLCP
	LOGICAL BULLCP

	COMMON /KNOWN_FOLDERS/ FOLDER_Q1,NUM_FOLDERS

	IF (TEST_BULLCP()) CALL EXIT	! BULLCP already running, so exit.

	BULLCP = .FALSE.		! Enable process to do BULLCP functions

	IER = SYS$CREMBX(%VAL(1),CHAN,,,,,'BULL_BULLCP')
	IF (.NOT.IER) THEN		! Can't create mailbox, so exit.
	   CALL SYS_GETMSG(IER)
	   CALL EXIT
	END IF

	IER = SYS$DELMBX(%VAL(CHAN))	! If process dies, mailbox is deleted.

	CALL REGISTER_BULLCP

	CALL START_DECNET

	DO WHILE (1)			! Loop once every 15 minutes
	   CALL GET_PROXY_ACCOUNTS	! Proxy info for incoming connectiosn
	   CALL BBOARD			! Look for BBOARD messages.
	   FOLDER_Q = FOLDER_Q1		! Init queue pointer to header
	   POINT_FOLDER = 0
	   DO WHILE (POINT_FOLDER.LT.NUM_FOLDERS)
	      POINT_FOLDER = POINT_FOLDER + 1
	      CALL SYS$SETAST(%VAL(0))
	      CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)
	      IF (FOLDER_BBOARD(:2).NE.'::') THEN
	         CALL SELECT_FOLDER(.FALSE.,IER)	! Select folder
	         IF (IER) THEN
	            IF (NEMPTY.GT.200) THEN
	               CALL CLEANUP_BULLFILE	! Cleanup empty blocks
	            END IF
	            CALL DELETE_EXPIRED		! Delete expired messages
	         END IF
	      END IF
	      CALL SYS$SETAST(%VAL(1))
	   END DO
	   CALL WAIT('15')		! Wait for 15 minutes
C
C  Look at remote folders and update local info to reflect new messages.
C  Do here after waiting in case problem with connecting to remote folder
C  which requires killing process.
C
	   FOLDER_Q = FOLDER_Q1
	   POINT_FOLDER = 0
	   DO WHILE (POINT_FOLDER.LT.NUM_FOLDERS)
	      POINT_FOLDER = POINT_FOLDER + 1
	      CALL SYS$SETAST(%VAL(0))
	      CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)
	      IF (FOLDER_BBOARD(:2).EQ.'::') THEN
	         CALL SELECT_FOLDER(.FALSE.,IER)
	      END IF
	      CALL SYS$SETAST(%VAL(1))
	   END DO
	   CALL SYS$SETAST(%VAL(0))
	   FOLDER_NUMBER = 0			! Reset to GENERAL folder
	   CALL SELECT_FOLDER(.FALSE.,IER)
	   CALL SYS$SETAST(%VAL(1))
	END DO

	RETURN
	END



	SUBROUTINE REGISTER_BULLCP

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLUSER.INC'

	COMMON /SYSTEM_FOLDERS/ SYSTEM_FLAG(FLONG),NODENAME,DUMMY(2)
	CHARACTER NODENAME*8

	CALL OPEN_FILE(4)

	DO WHILE (REC_LOCK(IER))
	   READ (4,KEY='*SYSTEM',IOSTAT=IER) 
     &		TEMP_USER,NODENAME,DUMMY,NEW_FLAG,SYSTEM_FLAG
	END DO

	IF (IER.NE.0) THEN
	   DO I=1,FLONG
	      SYSTEM_FLAG(I) = 0
	   END DO
	   CALL SET2(SYSTEM_FLAG,0)
	END IF

	CALL LIB$SYS_TRNLOG('SYS$NODE',,NODENAME)
	NODENAME = NODENAME(2:INDEX(NODENAME,':')-1)

	IF (IER.NE.0) THEN
	   WRITE (4,IOSTAT=IER)
     &		'*SYSTEM',NODENAME,DUMMY,NEW_FLAG,SYSTEM_FLAG
	ELSE
	   REWRITE (4,IOSTAT=IER)
     &		TEMP_USER,NODENAME,DUMMY,NEW_FLAG,SYSTEM_FLAG
	END IF

	CALL CLOSE_FILE(4)

	RETURN
	END





	SUBROUTINE WAIT(PARAM)
C
C SUBROUTINE WAIT
C
C FUNCTION: Waits for specified time period in minutes.
C
	IMPLICIT INTEGER (A-Z)
	INTEGER TIMADR(2)			! Buffer containing time
						! in desired system format.
	CHARACTER TIMBUF*13,PARAM*2
	DATA TIMBUF/'0 00:00:00.00'/

	DATA WAIT_EF /0/

	IF (WAIT_EF.EQ.0) CALL LIB$GET_EF(WAIT_EF)

	TIMBUF(6:7) = PARAM

	IER=SYS$BINTIM(TIMBUF,TIMADR)
	IER=SYS$SETIMR(%VAL(WAIT_EF),TIMADR,,%VAL(2))	! Set timer.
	IER=SYS$WAITFR(%VAL(WAIT_EF))		! Wait for EFN to be set.

	RETURN
	END



	SUBROUTINE WAIT_SEC(PARAM)
C
C SUBROUTINE WAIT_SEC
C
C FUNCTION: Waits for specified time period in seconds.
C
	IMPLICIT INTEGER (A-Z)
	INTEGER TIMADR(2)			! Buffer containing time
						! in desired system format.
	CHARACTER TIMBUF*13,PARAM*2
	DATA TIMBUF/'0 00:00:00.00'/
	DATA WAIT_EF /0/

	IF (WAIT_EF.EQ.0) CALL LIB$GET_EF(WAIT_EF)

	TIMBUF(9:10) = PARAM

	IER=SYS$BINTIM(TIMBUF,TIMADR)
	IER=SYS$SETIMR(%VAL(WAIT_EF),TIMADR,,%VAL(3))	! Set timer.
	IER=SYS$WAITFR(%VAL(WAIT_EF))		! Wait for EFN to be set.

	RETURN
	END




	SUBROUTINE DELETE_EXPIRED

C
C  SUBROUTINE DELETE_EXPIRED
C
C  FUNCTION:
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

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'

	CHARACTER UPTIME_DATE*11,UPTIME_TIME*8

	CALL OPEN_FILE_SHARED(2)	! Open directory file
	CALL OPEN_FILE_SHARED(1)	! Open bulletin file
	CALL CLOSE_FILE(1)
	CALL READDIR(0,IER)		! Get directory header
	IF (IER.EQ.1) THEN		! Is header present?
	   IER = COMPARE_DATE(NEWEST_EXDATE,' ') ! Yes. Any expired bulls?
	   IF (IER.EQ.0) IER = COMPARE_TIME(NEWEST_EXTIME,' ')
	   IF (SHUTDOWN.GT.0.AND.
     &		(FOLDER_NUMBER.EQ.0.OR.BTEST(FOLDER_FLAG,2))) THEN
						! Do shutdown bulletins exist?
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

	RETURN
	END




	SUBROUTINE BBOARD
C
C  SUBROUTINE BBOARD
C
C  FUNCTION: Converts mail to BBOARD into non-system bulletins.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFILES.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE '($RMSDEF)'

	COMMON /KNOWN_FOLDERS/ FOLDER_Q1,NUM_FOLDERS
	DATA FOLDER_Q1/0/

	CHARACTER*11 INEXDATE
	CHARACTER INDESCRIP*74,INFROM*74,INTO*76,INPUT*132
	CHARACTER ACCOUNT_SAVE*8,USERNAME_SAVE*12

	DIMENSION NEW_MAIL(FOLDER_MAX)

	DATA SPAWN_EF/0/

	IF (SPAWN_EF.EQ.0) CALL LIB$GET_EF(SPAWN_EF)

	CALL DISABLE_CTRL

	CALL INIT_QUEUE(FOLDER_Q1,FOLDER_COM)

	FOLDER_Q = FOLDER_Q1

	CALL SYS$SETAST(%VAL(0))
	CALL OPEN_FILE_SHARED(7)		! Get folder file

	NUM_FOLDERS = 0
	IER = 0
	DO WHILE (IER.EQ.0)			! Copy all bulletins from file
	   CALL READ_FOLDER_FILE(IER)
	   IF (IER.EQ.0) THEN
	      NUM_FOLDERS = NUM_FOLDERS + 1
	      CALL WRITE_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)
	   END IF
	END DO

	CALL CLOSE_FILE(7)			! We don't need file anymore
	CALL SYS$SETAST(%VAL(1))

	CALL SYS$SETAST(%VAL(0))
	CALL CHECK_MAIL(NEW_MAIL)
	CALL SYS$SETAST(%VAL(1))

	FOLDER_Q = FOLDER_Q1			! Init queue pointer to header

	NBBOARD_FOLDERS = 0

	POINT_FOLDER = 0

1	POINT_FOLDER = POINT_FOLDER + 1
	IF (POINT_FOLDER.GT.NUM_FOLDERS) GO TO 900

	CALL SYS$SETAST(%VAL(0))

	FOLDER_Q_SAVE = FOLDER_Q

	CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)

	IF (FOLDER_BBOARD.EQ.'NONE'.OR.
     &		FOLDER_BBOARD(:2).EQ.'::') GO TO 1

	NBBOARD_FOLDERS = NBBOARD_FOLDERS + 1

	IF (.NOT.NEW_MAIL(POINT_FOLDER)) GO TO 1
C
C  The process is set to the BBOARD uic and username in order to create
C  a spawned process that is able to read the BBOARD mail (a real kludge).
C

	CALL GETUSER(USERNAME_SAVE)		! Get present username
	CALL GETACC(ACCOUNT_SAVE)		! Get present account
	CALL GETUIC(GROUP_SAVE,USER_SAVE)	! Get present uic

	IF (TRIM(FOLDER_BBOARD).GT.0) THEN	! BBOARD name present?
	   IER = SETUSER(FOLDER_BBOARD,USERNAME_SAVE)! Set to BBOARD username
	   IF (IER.EQ.2) GO TO 910	! Can't set username. New VMS version?
	   CALL SETACC(ACCOUNTB)	! Set to BBOARD account
	   CALL SETUIC(IBCLR(GROUPB,31),IBCLR(USERB,31)) ! Set to BBOARD uic
	END IF

	LEN_B = TRIM(BBOARD_DIRECTORY)
	IER = LIB$DELETE_FILE(BBOARD_DIRECTORY(:LEN_B)//
     &		FOLDER_BBOARD(:TRIM(FOLDER_BBOARD))//'.TXT;*')
				! Delete old TXT files left due to errors

	IF (.NOT.BTEST(USERB,31).AND.(USERB.NE.0.OR.GROUPB.NE.0)) THEN
							! If normal BBOARD user
	 IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(:LEN_B)
     &		//'BOARD.COM','NL:','NL:',1,,,STATUS,SPAWN_EF)
	 CALL SYS$SETAST(%VAL(1))
	 CALL SYS$WAITFR(%VAL(SPAWN_EF))
	 CALL SYS$SETAST(%VAL(0))
	 IF (((STATUS.AND.'1FFFF'X).EQ.RMS$_FNF) .OR.
     &	 ((STATUS .AND. '1FFF0'X).EQ. (RMS$_SPL .AND. '1FFF0'X))) THEN
	   CALL SYS$SETDFPROT('AA00'X,CUR_DEF_PROT)
		! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD:RW,GROUP:RW)
	   OPEN(UNIT=11,FILE=BBOARD_DIRECTORY(:LEN_B)//'BOARD.COM',
     &		STATUS='NEW',ERR=910,CARRIAGECONTROL='LIST')
	   WRITE(11,'(A)') '$ SET PROTECT=(W:RWED)/DEFAULT'
	   WRITE(11,'(A)') '$ SET PROC/PRIV=SYSPRV'
	   WRITE(11,'(A)')
     & '$ DEFINE/USER EXTRACT_FILE '//BBOARD_DIRECTORY(:LEN_B)//
     & '''F$GETJPI("","USERNAME")'''
	   WRITE(11,'(A)') '$ MAIL'
	   WRITE(11,'(A)') 'READ'
	   WRITE(11,'(A)') 'EXTRACT/ALL/APPEND EXTRACT_FILE'
	   WRITE(11,'(A)') 'DELETE/ALL'
	   CLOSE(UNIT=11)
	   CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection
	   IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(:LEN_B)
     &			//'BOARD.COM','NL:','NL:',1,,,STATUS,SPAWN_EF)
	   CALL SYS$SETAST(%VAL(1))
	   CALL SYS$WAITFR(%VAL(SPAWN_EF))
	   CALL SYS$SETAST(%VAL(0))
	 END IF
	ELSE
	 IER = LIB$FIND_FILE(BBOARD_DIRECTORY(:LEN_B)//FOLDER_BBOARD
     &	    (:TRIM(FOLDER_BBOARD))//'.COM',INPUT,CONTEXT)
	 IF (IER) THEN
	    IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(:LEN_B)//
     &		FOLDER_BBOARD(:TRIM(FOLDER_BBOARD))//'.COM','NL:',
     &		'NL:',1,,,STATUS,SPAWN_EF)
	    CALL SYS$SETAST(%VAL(1))
	    CALL SYS$WAITFR(%VAL(SPAWN_EF))
	    CALL SYS$SETAST(%VAL(0))
	 END IF
	 IF (.NOT.IER.OR.((STATUS.AND.'1FFFF'X).EQ.RMS$_FNF) .OR.
     &	 ((STATUS .AND. '1FFF0'X).EQ. (RMS$_SPL .AND. '1FFF0'X))) THEN
	    IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(:LEN_B)//
     &	      'BOARD_SPECIAL.COM','NL:','NL:',1,,,STATUS,SPAWN_EF)
	    CALL SYS$SETAST(%VAL(1))
	    CALL SYS$WAITFR(%VAL(SPAWN_EF))
	    CALL SYS$SETAST(%VAL(0))
	 END IF
	END IF

	CALL READ_QUEUE(%VAL(FOLDER_Q_SAVE),FOLDER_Q,FOLDER_COM)

	NBULL = F_NBULL

	CALL SETACC(ACCOUNT_SAVE)		! Reset to original account
	CALL SETUSER(USERNAME_SAVE)		! Reset to original username
	CALL SETUIC(GROUP_SAVE,USER_SAVE)	! Reset to original uic

	OPEN (UNIT=3,FILE=BBOARD_DIRECTORY(:LEN_B)//FOLDER_BBOARD
     &	   (:TRIM(FOLDER_BBOARD))//'.TXT',STATUS='OLD',ERR=110)
	CALL SYS$SETAST(%VAL(1))

5	CALL SYS$SETAST(%VAL(0))

	CALL READ_QUEUE(%VAL(FOLDER_Q_SAVE),IDUMMY,FOLDER_COM)

	LEN_INPUT = 1
	DO WHILE (LEN_INPUT.GT.0)
	   READ (3,'(Q,A)',END=100) LEN_INPUT,INPUT ! Read next line from mail
	   IF (INPUT(:5).EQ.'From:') THEN
	      INFROM = INPUT(7:)		! Store username
	   ELSE IF (INPUT(:5).EQ.'Subj:') THEN
	      INDESCRIP = INPUT(7:)		! Store subject
	   ELSE IF (INPUT(:3).EQ.'To:') THEN
	      INTO = INPUT(5:)			! Store address
	   END IF
	END DO

	INTO = INTO(:TRIM(INTO))
	CALL STR$TRIM(INTO,INTO)
	FLEN = TRIM(FOLDER_BBOARD)
	IF (INDEX(INTO,FOLDER_BBOARD(:FLEN)).EQ.0.AND.
     &		INTO.NE.FOLDER_BBOARD) THEN
	   POINT_FOLDER1 = 0
 	   FOLDER_Q2 = FOLDER_Q1
	   FOLDER1_BBOARD = FOLDER_BBOARD
	   FOUND = .FALSE.
	   DO WHILE (.NOT.FOUND.AND.POINT_FOLDER1.LT.NUM_FOLDERS)
	      FOLDER_Q2_SAVE = FOLDER_Q2
	      CALL READ_QUEUE(%VAL(FOLDER_Q2),FOLDER_Q2,FOLDER1_COM)
	      FLEN = TRIM(FOLDER1_BBOARD)
	      POINT_FOLDER1 = POINT_FOLDER1 + 1
	      IF (POINT_FOLDER1.LE.NUM_FOLDERS.AND.
     &		  FOLDER1_BBOARD.NE.'NONE') THEN
	 	 IF (INTO.EQ.FOLDER1_BBOARD) THEN
		    FOUND = .TRUE.
		 ELSE
		    FIND_TO = INDEX(INTO,FOLDER1_BBOARD(:FLEN))
		    IF (FIND_TO.GT.0) THEN
		       END_TO = FLEN+FIND_TO
		       IF (TRIM(INTO).LT.END_TO.OR.
     &			    INTO(END_TO:END_TO).LT.'A'.OR.
     &			    INTO(END_TO:END_TO).GT.'Z') THEN
			  IF (FIND_TO.EQ.1) THEN
			     FOUND = .TRUE.
			  ELSE IF (INTO(FIND_TO-1:FIND_TO-1).LT.'A'.OR.
     &			           INTO(FIND_TO-1:FIND_TO-1).GT.'Z') THEN
			     FOUND = .TRUE.
			  END IF
		       END IF
		    END IF
	 	 END IF
	      END IF
	   END DO
	   IF (FOUND) THEN
	      IF (F_NBULL.NE.NBULL) CALL UPDATE_FOLDER
	      FOLDER_COM = FOLDER1_COM
	      FOLDER_Q_SAVE = FOLDER_Q2_SAVE
	   END IF
	END IF

	IF (FOLDER_NUMBER.EQ.0) THEN
	   FOLDER_SET = .FALSE.
	ELSE
	   FOLDER_SET = .TRUE.
	   FOLDER_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//
     &		FOLDER
	END IF

C
C  Add bulletin to bulletin file and directory entry to directory file.
C

	CALL OPEN_FILE(2)			! Prepare to add dir entry

	READ (3,'(Q,A)',IOSTAT=IER) LEN_INPUT,INPUT	! Read first line
	IF (IER.NE.0) GO TO 100			! If end of file, exit
	IF (LEN_INPUT.EQ.1.AND.INPUT(:1).EQ.CHAR(12)) GO TO 5
			! If line is just form feed, the message is empty

	CALL OPEN_FILE(1)			! Prepare to add bulletin

	CALL READDIR(0,IER)			! Get NBLOCK
	IF (IER.EQ.0) NBLOCK = 0		! If new file, NBLOCK is 0

	OCOUNT = NBLOCK + 1			! Initialize line count

	SPACE = INDEX(INFROM,' ') - 1		! Strip off the date
	IF (SPACE.GT.0) INFROM = INFROM(:SPACE)! From the "From:" line

	IF (TRIM(INFROM).GT.12) THEN		! Is length > allowable?
	   LEN_INFROM = TRIM(INFROM)
	   CALL STORE_BULL(6+LEN_INFROM,'From: '//INFROM(:LEN_INFROM),
     &		OCOUNT)
	   IF (INDEX(INFROM,'::').GT.0)		! Strip off node name
     &		INFROM = INFROM(INDEX(INFROM,'::')+2:)
	   I = 12		! Trim username to first non-alpha character
	   DO WHILE (I.GT.1.AND.
     &		     ((INFROM(I:I).GE.'A'.AND.INFROM(I:I).LE.'Z').OR.
     &		     (INFROM(I:I).GE.'a'.AND.INFROM(I:I).LE.'z')) )
	      I = I - 1
	   END DO
	   IF (I.GT.1) INFROM = INFROM(:I-1)
	END IF

	LEN_DESCRP = TRIM(INDESCRIP)
	IF (LEN_DESCRP.GT.53) THEN	! Is length > allowable subject length?
	   CALL STORE_BULL(6+LEN_DESCRP,'Subj: '//INDESCRIP(:LEN_DESCRP),
     &		OCOUNT)
	   INDESCRIP = INDESCRIP(:LEN_DESCRP)
	   DO I=1,LEN_DESCRP
	      IF (INDESCRIP(I:I).LT.' ') INDESCRIP(I:I) = ' '
	   END DO
	ELSE
	   DO I=1,LEN_DESCRP			! Remove control characters
	      IF (INDESCRIP(I:I).LT.' ') INDESCRIP(I:I) = ' '
	   END DO
	END IF

	ISTART = 0
	NBLANK = 0
	DO WHILE (INPUT(:1).NE.CHAR(12))	! Move text to bulletin file
	   IF (LEN_INPUT.EQ.0) THEN
	      IF (ISTART.EQ.1) THEN
		 NBLANK = NBLANK + 1
	      END IF
	   ELSE
	      ISTART = 1
	      DO I=1,NBLANK
		 CALL STORE_BULL(1,' ',OCOUNT)
	      END DO
	      NBLANK = 0
	      CALL STORE_BULL(MIN(LEN_INPUT,80),INPUT,OCOUNT)
	      IF (LEN_INPUT.GT.80) THEN		! Breakup line if > 80 chars
		 CALL STORE_BULL(MIN(LEN_INPUT,132)-80,INPUT(81:),OCOUNT)
	      END IF
	   END IF
	   READ (3,'(Q,A)',END=25) LEN_INPUT,INPUT
	END DO

25	CALL FLUSH_BULL(OCOUNT)

	CALL CLOSE_FILE(1)			! Finished adding bulletin

	DESCRIP = INDESCRIP(:53)		! Description header
	FROM = INFROM(:12)			! Username
	IF (FOLDER_BBEXPIRE.EQ.-1) THEN		! Folder has expiration time?
	   EXDATE = '5-NOV-2000'		! no, so set date far in future
	   SYSTEM = 2				! indicate permanent message
	ELSE					! Else set expiration date
	   CALL GET_EXDATE(EXDATE,FOLDER_BBEXPIRE)
	   SYSTEM = 0
	END IF
	EXTIME = '00:00:00'
	LENGTH = OCOUNT - NBLOCK		! Number of records

	CALL ADD_ENTRY				! Add the new directory entry

30	CALL CLOSE_FILE(2)			! Totally finished with add

	CALL SYS$SETAST(%VAL(1))

	GO TO 5					! See if there is more mail

100	CALL UPDATE_FOLDER

110	CLOSE (UNIT=3,STATUS='DELETE')		! Close the input file
	CALL SYS$SETAST(%VAL(1))
	GOTO 1

900	FOLDER_NUMBER = 0

	CALL OPEN_FILE_SHARED(7)
	CALL READ_FOLDER_FILE_KEYNUM(0,IER)
	CALL CLOSE_FILE(7)
	CALL ENABLE_CTRL
	FOLDER_SET = .FALSE.

	IF (NBBOARD_FOLDERS.EQ.0) THEN
	   CALL OPEN_FILE(4)
	   CALL READ_USER_FILE_HEADER(IER)
	   CALL SYS_BINTIM('5-NOV-2956 00:00:00.00',BBOARD_BTIM)
	   REWRITE (4) USER_HEADER		! Rewrite header
	   CALL CLOSE_FILE(4)
	END IF

	RETURN

910	WRITE (6,1010)
	GO TO 100

930	CLOSE (UNIT=3)
	CALL CLOSE_FILE(1)
	CALL CLOSE_FILE(2)
	WRITE (6,1030)
	GO TO 100

1010	FORMAT(' ERROR:Install program with CMKRNL privileges or relink.')
1030	FORMAT(' ERROR:Alert system programmer. Data file problems.')

	END




	SUBROUTINE CREATE_BBOARD_PROCESS

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($PRCDEF)'

	INCLUDE 'BULLFILES.INC'

	CHARACTER*132 IMAGENAME

	CALL GETIMAGE(IMAGENAME,ILEN)

	LEN_B = TRIM(BBOARD_DIRECTORY)

	OPEN(UNIT=11,FILE=BBOARD_DIRECTORY(:LEN_B)//'BULL_COMMAND.COM',
     &		STATUS='OLD',IOSTAT=IER)
	IF (IER.EQ.0) CLOSE(UNIT=11,STATUS='DELETE')

	CALL SYS$SETDFPROT('AA00'X,CUR_DEF_PROT)
		! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD:RW,GROUP:RW)
	OPEN(UNIT=11,FILE=BBOARD_DIRECTORY(:LEN_B)//'BULL_COMMAND.COM',
     &		STATUS='NEW',IOSTAT=IER,CARRIAGECONTROL='LIST')
	IF (IER.NE.0) RETURN
	WRITE(11,'(A)') '$B:=$'//IMAGENAME(:ILEN)
	WRITE(11,'(A)') '$ON ERROR THEN GOTO EXIT'
	WRITE(11,'(A)') '$ON SEVERE THEN GOTO EXIT'
	WRITE(11,'(A)') '$ON WARNING THEN GOTO EXIT'
	WRITE(11,'(A)') '$B/'//'''F$PROCESS()'''
	WRITE(11,'(A)') '$EXIT:'
	WRITE(11,'(A)') '$LOGOUT'
	CLOSE(UNIT=11)
	CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection

	IER = SYS$CREPRC(,'SYS$SYSTEM:LOGINOUT',
     &	   BBOARD_DIRECTORY(:LEN_B)//'BULL_COMMAND.COM','NL:'
     &	   ,,,,'BBOARD',%VAL(4),,,%VAL(PRC$M_NOUAF+PRC$M_DETACH))

	RETURN
	END



	SUBROUTINE GETUIC(GRP,MEM)
C
C  SUBROUTINE GETUIC(UIC)
C
C  FUNCTION:
C	To get UIC of process submitting the job.
C  OUTPUT:
C	GRP   -    Group number of UIC
C	MEM   -	   Member number of UIC
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($JPIDEF)'

	CALL INIT_ITMLST	! Initialize item list
				! Now add items to list
	CALL ADD_2_ITMLST(4,JPI$_GRP,%LOC(GRP))
	CALL ADD_2_ITMLST(4,JPI$_MEM,%LOC(MEM))
	CALL END_ITMLST(GETJPI_ITMLST)	! Get address of itemlist

	IER = SYS$GETJPIW(,,,%VAL(GETJPI_ITMLST),,,,)	! Get Info command.

	RETURN
	END




	SUBROUTINE GET_UPTIME(UPTIME_DATE,UPTIME_TIME)
C
C  SUBROUTINE GET_UPTIME
C
C  FUNCTION: Gets time of last reboot.
C

	IMPLICIT INTEGER (A-Z)

	EXTERNAL	EXE$GL_ABSTIM
	INTEGER 	UPTIME(2),SYSTIME(2),UPSINCE(2)
	CHARACTER*(*)	UPTIME_TIME,UPTIME_DATE
	CHARACTER	ASCSINCE*23

	UPTIME(1) = GET_L_VAL(EXE$GL_ABSTIM)			! Up time (sec)

	CALL LIB$EMUL(10000000,UPTIME,0,UPTIME) 		! 64 bit format
	CALL SYS$GETTIM(SYSTIME)
	CALL LIB$SUBX(SYSTIME,UPTIME,UPSINCE)
	CALL SYS$ASCTIM(,ASCSINCE,UPSINCE,)			! Up since

	UPTIME_DATE = ASCSINCE(:11)
	UPTIME_TIME = ASCSINCE(13:20)

	RETURN	
	END

	INTEGER FUNCTION GET_L_VAL(I)
	INTEGER I
	GET_L_VAL = I
	RETURN
	END



	SUBROUTINE CHECK_MAIL(NEW_MAIL)

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	COMMON /KNOWN_FOLDERS/ FOLDER_Q1,NUM_FOLDERS
	DATA FOLDER_Q1/0/

	DIMENSION NEW_MAIL(1)

	CHARACTER INPUT*35
	EQUIVALENCE (INPUT(34:),COUNT)

	FOLDER_Q = FOLDER_Q1			! so reinit queue pointer

	OPEN (UNIT=10,FILE='VMSMAIL',DEFAULTFILE='SYS$SYSTEM:VMSMAIL.DAT',
     &       ACCESS='KEYED',FORM='FORMATTED',ORGANIZATION='INDEXED',
     &       STATUS='OLD',READONLY,SHARED)

	DO I=1,NUM_FOLDERS
	   CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)

	   IF ((.NOT.BTEST(USERB,31).AND.(USERB.NE.0.OR.GROUPB.NE.0)).OR.
     &		 BTEST(GROUPB,31)) THEN	! If normal BBOARD or /VMSMAIL
	      READ(10,'(A)',KEY=FOLDER_BBOARD,IOSTAT=IER) INPUT
	      IF (IER.EQ.0.AND.COUNT.GT.0) THEN
		 NEW_MAIL(I) = .TRUE.
	      ELSE
		 NEW_MAIL(I) = .FALSE.
	      END IF
	   ELSE
	      NEW_MAIL(I) = .TRUE.
	   END IF
	END DO

	CLOSE (10)

	RETURN
	END



	SUBROUTINE GETIMAGE(IMAGNAME,ILEN)
C
C  SUBROUTINE GETIMAGE(IMAGNAME,ILEN)
C
C  FUNCTION:
C	To get image name of process.
C  OUTPUT:
C	IMAGNAME   -    Image name of process
C	ILEN	   -	Length of imagename
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($JPIDEF)'

	CHARACTER*(*) IMAGNAME

	CALL INIT_ITMLST	! Initialize item list
				! Now add items to list
	CALL ADD_2_ITMLST_WITH_RET(LEN(IMAGNAME),JPI$_IMAGNAME,
     &					%LOC(IMAGNAME),%LOC(ILEN))
	CALL END_ITMLST(GETJPI_ITMLST)	! Get address of itemlist

	IER = SYS$GETJPIW(,,,%VAL(GETJPI_ITMLST),,,,)	! Get Info command.

	RETURN
	END




	SUBROUTINE GET_NEWEST_MSG(IN_BTIM,START)

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	DIMENSION IN_BTIM(2),DIR_BTIM(2)

	IF (REMOTE_SET) THEN
	   WRITE (REMOTE_UNIT,'(3A)',IOSTAT=IER) 12,IN_BTIM(1),IN_BTIM(2)
	   IF (IER.EQ.0) THEN
	      READ (REMOTE_UNIT,'(A)',IOSTAT=IER) START
	   END IF
	ELSE
	   CALL READDIR(1,IER)
	   CALL SYS_BINTIM(DATE//' '//TIME,DIR_BTIM)
	   DIFFB = COMPARE_BTIM(IN_BTIM,DIR_BTIM)
	   IF (DIFFB.LE.0) THEN
	      START = 0
	      RETURN
	   END IF
	   CALL READDIR(NBULL,IER)
	   CALL SYS_BINTIM(DATE//' '//TIME,DIR_BTIM)
	   DIFFT = COMPARE_BTIM(IN_BTIM,DIR_BTIM)
	   IF (DIFFT.GT.0.OR.IER.EQ.NBULL) THEN
	      START = -1
	      RETURN
	   END IF
	   BOT = 0
	   TOP = NBULL + 1
	   DIFFB = 0
	   NCHECKS = 0
	   DO WHILE (DIFFB.LE.0.OR.DIFFT.GT.0)
	      START = (TOP+BOT) / 2
	      CALL READDIR(START,IER)
	      CALL SYS_BINTIM(DATE//' '//TIME,DIR_BTIM)
	      DIFFB = COMPARE_BTIM(IN_BTIM,DIR_BTIM)
	      CALL READDIR(START+1,IER)
	      CALL SYS_BINTIM(DATE//' '//TIME,DIR_BTIM)
	      DIFFT = COMPARE_BTIM(IN_BTIM,DIR_BTIM)
	      IF (DIFFB.GT.0) THEN
		 BOT = START + 1
	      ELSE
		 TOP = START
	      END IF
	      NCHECKS = NCHECKS + 1
C
C It should never happen, but test to see if can't find
C newest message, to avoid looping forever.
C
	      IF (NCHECKS.GT.NBULL) RETURN
	   END DO
	END IF

	RETURN
	END
