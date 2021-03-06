From:	HENRY::IN%"MRL%PFCVAX%xx.lcs.mit.edu%csnet-relay.CSNET%relay.cs.net@RCA.COM" 19-JUN-1987 20:59
To:	"SYSTEM%UK.AC.SOTON.ESP.V1" <SYSTEM%UK.AC.SOTON.ESP.V1%ucl-cs.arpa@xx>, 
Subj:	BULLETIN2.FOR

C
C  BULLETIN2.FOR, Version 6/16/87
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Programmer: Mark R. London
C
	SUBROUTINE REPLACE
C
C  SUBROUTINE REPLACE
C
C  FUNCTION: Replaces existing bulletin to bulletin file.
C
	IMPLICIT INTEGER (A - Z)

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /EDIT/ EDIT_DEFAULT

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	CHARACTER INEXDATE*11,INEXTIME*8
	CHARACTER INDESCRIP*80,INPUT*80,TODAY*23
	CHARACTER*1 ANSWER

	INTEGER TIMADR(2)

	EXTERNAL CLI$_ABSENT

	LOGICAL*1 DOALL

C
C  Get the bulletin number to be replaced.
C
	IF (.NOT.CLI$PRESENT('NUMBER')) THEN	! No number has been specified
	   IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been read
	      WRITE (6,1005)		! Tell user of the error
	      RETURN			! and return
	   END IF
	   NUMBER_PARAM = BULL_POINT	! Replace the bulletin we are reading
	ELSE
	   CALL CLI$GET_VALUE('NUMBER',BULL_PARAMETER,LEN_P)
	   DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER) NUMBER_PARAM
	END IF

	IF (CLI$PRESENT('SYSTEM')) THEN
	   IF (.NOT.SETPRV_PRIV()) THEN
	    WRITE (6,'(
     &	     '' ERROR: Not enough privileges to change to system.'')')
	    RETURN
	   ELSE IF (FOLDER_SET.AND.CLI$PRESENT('SYSTEM')) THEN
	    WRITE (6,'(
     &       '' ERROR: Invalid parameter used with folder set.'')')
	    RETURN
	   END IF
	END IF

	IF (CLI$PRESENT('SHUTDOWN')) THEN
	   IF (.NOT.SETPRV_PRIV()) THEN
	    WRITE (6,'(
     &	     '' ERROR: Not enough privileges to change to shutdown.'')')
	    RETURN
	   ELSE IF (FOLDER_SET.AND.CLI$PRESENT('SHUTDOWN')) THEN
	    WRITE (6,'(
     &       '' ERROR: Invalid parameter used with folder set.'')')
	    RETURN
	   END IF
	END IF

	IF (CLI$PRESENT('PERMANENT').AND.
     &      .NOT.FOLDER_SET.AND..NOT.SETPRV_PRIV()) THEN
	   WRITE (6,'(
     &	    '' ERROR: Not enough privileges to change to permanent.'')')
	   RETURN
	END IF
C
C  Check to see if specified bulletin is present, and if the user
C  is permitted to replace the bulletin.
C

	CALL OPEN_FILE_SHARED(2)

	CALL READDIR(NUMBER_PARAM,IER)	! Get info for specified bulletin

	CALL CLOSE_FILE(2)

	IF (IER.NE.NUMBER_PARAM+1) THEN	! Was bulletin found?
	   WRITE (6,1015)		! If not, tell the person
	   RETURN			! and error out
	END IF

	IF (USERNAME.NE.FROM) THEN	! If doesn't match owner of bulletin,
	   IF ((.NOT.SETPRV_PRIV().AND..NOT.FOLDER_SET).OR.    ! Privileges or
     &	       (.NOT.SETPRV_PRIV().AND.
     &		USERNAME.NE.FOLDER_OWNER.AND.FOLDER_SET)) THEN ! folder owner?
	      WRITE(6,1090)		! If not, then error out.
	      RETURN
	   ELSE
	      WRITE (6,1100)		! Make sure user wants to delete it
	      READ (5,'(A)',IOSTAT=IER) ANSWER	! Get his answer
	      CALL STR$UPCASE(ANSWER,ANSWER)	! Convert input to uppercase
	      IF (ANSWER.NE.'Y') RETURN	! If not Yes, then exit
	   END IF
	END IF

C
C  If no switches were given, replace the full bulletin
C

	DOALL = .FALSE.

	IF ((.NOT.CLI$PRESENT('EXPIRATION')).AND.
     &	   (.NOT.CLI$PRESENT('GENERAL')).AND.
     &	   (.NOT.CLI$PRESENT('SYSTEM')).AND.
     &	   (.NOT.CLI$PRESENT('HEADER')).AND.
     &	   (.NOT.CLI$PRESENT('TEXT')).AND.
     &	   (.NOT.CLI$PRESENT('SHUTDOWN')).AND.
     &	   (.NOT.CLI$PRESENT('PERMANENT'))) THEN
	   DOALL = .TRUE.
	END IF

	CALL DISABLE_CTRL			! Disable CTRL-Y & -C

	IF (CLI$PRESENT('EXPIRATION').OR.DOALL) THEN
	   CALL GET_EXPIRED(INPUT,IER)
	   IF (.NOT.IER) GO TO 910
	   INEXDATE = INPUT(1:11)
	   INEXTIME = INPUT(13:20)
	END IF

8	IF (CLI$PRESENT('HEADER').OR.DOALL) THEN
	   WRITE(6,1050)			! Request header for bulletin
	   READ(5,'(Q,A)',END=910,ERR=910) LEN,INDESCRIP
	   IF (LEN.EQ.0) GO TO 910		! If no header, don't add bull
	   IF (LEN.GT.53) THEN			! If too many characters
	      WRITE(6,1060)			! tell user
	      WRITE(6,2020) INDESCRIP(1:53)	! Show how much would fit
	      GO TO 8				! and re-request header
	   END IF
	END IF


	IF (CLI$PRESENT('TEXT').OR.DOALL) THEN
C
C  If file specified in REPLACE command, read file to obtain bulletin.
C  Else, read the bulletin from the terminal.
C
	
	  ICOUNT = 0				! Line count for bulletin
	  LAST_NOBLANK = 0			! Last line with data
	  REC1 = 1

	  IER = CLI$GET_VALUE('FILESPEC',BULL_PARAMETER,LEN_P)
	  IF (IER.NE.%LOC(CLI$_ABSENT).OR.	! If file param in ADD command
     &	    CLI$PRESENT('EDIT').OR.EDIT_DEFAULT) THEN	! or /EDIT specified

	   IF (CLI$PRESENT('EDIT').OR.EDIT_DEFAULT) THEN ! If /EDIT specified
	      IF (LEN_P.EQ.0) THEN		! If no file param specified
		 IF (.NOT.CLI$PRESENT('NEW')) THEN
	            OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='NEW',
     &		       ERR=920,FORM='FORMATTED',CARRIAGECONTROL='LIST')
	            CALL OPEN_FILE_SHARED(1)	! Prepare to copy message
		    LEN = 81
		    DO I=BLOCK,BLOCK+LENGTH-1	! Copy mesage into file
		       DO WHILE (LEN.GT.0)
			  CALL GET_BULL(I,INPUT,LEN)
			  IF (LEN.LT.0) THEN
			     GO TO 5
			  ELSE IF (LEN.GT.0) THEN
			     WRITE (3,'(A)') INPUT(1:LEN)
			  END IF
		       END DO
		       LEN = 80
		    END DO
5		    CALL CLOSE_FILE(1)
	            CLOSE (UNIT=3)		! Bulletin copy completed
		 END IF
		 CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')
	      ELSE 
		 IF (.NOT.SETPRV_PRIV()) CALL DISABLE_PRIVS
		 CALL MAILEDIT(BULL_PARAMETER(:LEN_P),'SYS$LOGIN:BULL.SCR')
	      END IF
	      IER = LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;-1')
	      OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='OLD',
     &		 DISPOSE='DELETE',ERR=920,FORM='FORMATTED')
	   ELSE IF (LEN_P.GT.0) THEN
	      IF (.NOT.SETPRV_PRIV()) CALL DISABLE_PRIVS
	      OPEN (UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),STATUS='OLD',
     &		READONLY,SHARED,ERR=920,FORM='FORMATTED') ! Try opening the file
	   END IF

	   CALL ENABLE_PRIVS			! Reset SYSPRV privileges

	   DO WHILE(1)				! Read until end of file to
	      READ (3,'(Q,A)',END=10) LEN,INPUT	! get record count
	      IF (LEN.GT.80) GO TO 950
	      CALL STR$TRIM(INPUT,INPUT,LEN)
	      IF (LEN.GT.0) THEN		! If good input line entered
		 ICOUNT = ICOUNT + LEN + 1	! Increment record count
		 LAST_NOBLANK = ICOUNT
	      ELSE IF (LEN.EQ.0) THEN
		 IF (ICOUNT.GT.0) THEN
		    ICOUNT = ICOUNT + 2		! COPY_BULL writes a line with
		 ELSE				! 1 space for a blank line.
		    REC1 = REC1 + 1
		 END IF
	      END IF
	   END DO
	  ELSE					! If no input file
	   OPEN (UNIT=3,STATUS='SCRATCH',FILE='SYS$LOGIN:BULL.SCR',
     &		FORM='FORMATTED')		! Scratch file to save bulletin
	   WRITE (6,1000)		! Request bulletin input from terminal
	   LEN = 80				! Length of input line
	   DO WHILE (LEN.GE.0)			! Input until no more input
	      CALL GET_LINE(INPUT,LEN)		! Get input line
	      IF (LEN.GT.80) THEN		! Line too long.
		 WRITE(6,'('' ERROR: Input line length > 80. Reinput::'')')
	      ELSE IF (LEN.GT.0) THEN		! If good input line entered
		 ICOUNT = ICOUNT + 1 + LEN	! Increment character count
		 WRITE(3,'(A)') INPUT(1:LEN)	! Save line in scratch file
		 LAST_NOBLANK = ICOUNT
	      ELSE IF (LEN.EQ.0.AND.ICOUNT.GT.0) THEN
		 WRITE(3,'(A)') INPUT(1:LEN)	! Save line in scratch file
		 ICOUNT = ICOUNT + 2		! COPY_BULL writes a line with
	      END IF				! 1 space for a blank line.
	   END DO
	   IF (LEN.EQ.-1) GO TO 910		! CTRL_C entered, error out
10	   ICOUNT = LAST_NOBLANK
	   IF (ICOUNT.EQ.0) GO TO 910		! No lines entered, error out
	  ENDIF

	  REWIND (UNIT=3)
	END IF

C
C  Add bulletin to bulletin file and directory entry for to directory file.
C

	CALL OPEN_FILE(2)			! Prepare to add dir entry

	INPUT = DESCRIP
	CALL READDIR(NUMBER_PARAM,IER)		! Get info for message

	IF (IER.NE.NUMBER_PARAM+1.OR.INPUT.NE.DESCRIP) THEN
				! Message disappeared in the mean time?
	   CALL CLOSE_FILE(2)
	   CLOSE (UNIT=3)
	   WRITE(6,'('' ERROR: Message file info invalidated.
     & Find message and do REPLACE again.'')')
	   GO TO 100
	END IF

	CALL READDIR(0,IER)			! Get directory header

	IF (CLI$PRESENT('TEXT').OR.DOALL) THEN	! If text has been replaced

	   LENGTH_SAVE = LENGTH			! Copy BULL modifies LENGTH

	   CALL OPEN_FILE(1)			! Prepare to add bulletin
	   ICOUNT = (ICOUNT+127)/128

	   BLOCK = NBLOCK + 1
	   BLOCK_SAVE = BLOCK
	   NEMPTY = NEMPTY + LENGTH
	   NBLOCK = NBLOCK + ICOUNT

	   CALL WRITEDIR(0,IER)

	   CALL COPY_BULL(3,REC1,BLOCK,IER)	! Replace old bulletin

	   CALL CLOSE_FILE(1)

	   CALL READDIR(NUMBER_PARAM,IER)	! Get directory entry
	   LENGTH = ICOUNT			! Update size
	   BLOCK = BLOCK_SAVE
	   CALL WRITEDIR(NUMBER_PARAM,IER)	! Write new directory entry
	ELSE
	   CALL READDIR(NUMBER_PARAM,IER)
	END IF

	IF (CLI$PRESENT('HEADER').OR.DOALL) DESCRIP=INDESCRIP(1:53)
						! Update description header
	IF (CLI$PRESENT('EXPIRATION').OR.DOALL) THEN
	   SYSTEM = IBCLR(SYSTEM,1)
	   SYSTEM = IBCLR(SYSTEM,2)
	   EXDATE=INEXDATE			! Update expiration date
	   EXTIME=INEXTIME
	   DIFF = COMPARE_DATE(EXDATE,NEWEST_EXDATE)	! Compare expiration
	   IF (DIFF.EQ.0) DIFF = COMPARE_TIME(EXTIME,NEWEST_EXTIME)
	   IF (DIFF.LT.0) THEN			! If it's oldest expiration bull
	      NEWEST_EXDATE = EXDATE		! Update the header in
	      NEWEST_EXTIME = EXTIME		! the directory file
	      CALL WRITEDIR(0,IER)
	   END IF
	ELSE IF (CLI$PRESENT('PERMANENT').AND.
     &			(.NOT.BTEST(SYSTEM,1))) THEN
	   IF (BTEST(SYSTEM,2)) THEN
	      SYSTEM = IBCLR(SYSTEM,2)
	      SHUTDOWN = SHUTDOWN - 1
	      CALL WRITEDIR(0,IER)
	   END IF
	   SYSTEM = IBSET(SYSTEM,1)
	   EXDATE = '5-NOV-2000'
	   EXTIME = '00:00:00'
	ELSE IF (CLI$PRESENT('SHUTDOWN').AND.
     &			(.NOT.BTEST(SYSTEM,2))) THEN
	   SYSTEM = IBSET(SYSTEM,2)
	   SYSTEM = IBCLR(SYSTEM,1)
	   EXDATE = '5-NOV-2000'
	   EXTIME = '00:00:00'
	   SHUTDOWN = SHUTDOWN + 1
	   CALL SYS$ASCTIM(,TODAY,,)		! Get the present time
	   SHUTDOWN_DATE = TODAY(1:11)
	   SHUTDOWN_TIME = TODAY(13:20)
	   CALL WRITEDIR(0,IER)
	END IF

	IF (CLI$PRESENT('SYSTEM')) THEN
	   SYSTEM = IBSET(SYSTEM,0)
	ELSE IF (CLI$PRESENT('GENERAL')) THEN
	   SYSTEM = IBCLR(SYSTEM,0)
	END IF

	CALL WRITEDIR(NUMBER_PARAM,IER)

	CALL CLOSE_FILE(2)		! Totally finished with replace

	CLOSE (UNIT=3)

100	CALL ENABLE_CTRL		! Enable CTRL-Y & -C
	RETURN

910	WRITE(6,1010)
	CLOSE (UNIT=3,ERR=100)
	GOTO 100

920	WRITE(6,1020)
	CALL ENABLE_PRIVS	! Reset SYSPRV privileges
	GOTO 100

950	WRITE (6,1030)
	CLOSE (UNIT=3)
	GO TO 100

1000	FORMAT (' Enter message: End with ctrl-z, cancel with ctrl-c')
1005	FORMAT (' ERROR: You are not reading any message.')
1010	FORMAT (' No message was replaced.')
1015	FORMAT (' ERROR: Specified message was not found.')
1020	FORMAT (' ERROR: Unable to open specified file.')
1030	FORMAT (' ERROR: Line length in file exceeds 80 characters.')
1050	FORMAT (' Enter description header.  Limit header to 53
     & characters.')
1060	FORMAT (' ERROR: Header > 53 characters. Header would be
     & truncated to:')
1090	FORMAT(' ERROR: Specified message is not owned by you.')
1100	FORMAT(' Message is not owned by you.',
     &	       ' Are you sure you want to replace it? ',$)
2020	FORMAT(1X,A)

	END





	SUBROUTINE RESPOND(STATUS)
C
C  SUBROUTINE RESPOND
C
C  FUNCTION: Sends a mail message in reply to a posted message.
C
C  NOTE: Modify the last SPAWN statement to specify the command
C	you use to send mail to sites other than via MAIL.
C	If you always use a different command, modify both
C	spawn commands.
C
	IMPLICIT INTEGER (A - Z)

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	CHARACTER INPUT*80,FROM_TEST*5

	INCLUDE 'BULLDIR.INC'

	IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been read
	   WRITE(6,'('' ERROR: You have not read any message.'')')
	   RETURN			! And return
	END IF

	BULL_PARAMETER = 'RE: '//DESCRIP
	IF (CLI$PRESENT('SUBJECT')) THEN
	   IER = CLI$GET_VALUE('SUBJECT',BULL_PARAMETER,LEN_P)
	   IF (LEN_P.GT.LEN(BULL_PARAMETER)-2) THEN
	      WRITE(6,'('' ERROR: Subject limit is 64 characters.'')')
	      RETURN
	   END IF
	END IF

	LEN_P = TRIM(BULL_PARAMETER)

	IF (BULL_PARAMETER(1:1).NE.'"') THEN
	   BULL_PARAMETER = '"'//BULL_PARAMETER(1:LEN_P)
	   LEN_P = LEN_P + 1
	END IF

	IF (BULL_PARAMETER(LEN_P:LEN_P).NE.'"') THEN
	   BULL_PARAMETER = BULL_PARAMETER(1:LEN_P)//'"'
	   LEN_P = LEN_P + 1
	END IF

	IF (CONFIRM_USER(FROM).EQ.0) THEN
	   CALL LIB$SPAWN('$MAIL SYS$INPUT '//FROM//'/SUBJECT='//
     &		BULL_PARAMETER,,,,,,STATUS)
	ELSE
	   FROM_TEST = ' '
	   CALL OPEN_FILE_SHARED(1)	! Prepare to copy message
	   L_INPUT = 81
	   I = BLOCK
	   DO WHILE (I.LT.BLOCK+LENGTH.AND.L_INPUT.GT.0)
	      CALL GET_BULL(I,INPUT,L_INPUT)
	      IF (L_INPUT.GT.0) THEN
		 CALL STR$UPCASE(FROM_TEST,INPUT(1:5))
		 IF (FROM_TEST.EQ.'FROM:') THEN
		    IF (INDEX(INPUT,'.').GT.0.OR.INDEX(INPUT,'@').GT.0
     &			.OR.INDEX(INPUT,'%').GT.0) THEN
		       L_INPUT = 0
		    END IF
		 END IF
	      ELSE IF (L_INPUT.EQ.0) THEN
	         L_INPUT = 80
	         I = I + 1
	      END IF
	   END DO
	   CALL CLOSE_FILE(1)
	   IF (FROM_TEST.EQ.'FROM:') THEN
	      L_B = INDEX(INPUT,'<')
	      R_B = INDEX(INPUT,'>')
	      IF (L_B.GT.0.AND.R_B.GT.0) THEN
		 INPUT = INPUT(L_B+1:R_B-1)
		 L_INPUT = R_B - 1 - L_B
	      ELSE
		 L_INPUT = TRIM(INPUT)
		 I = 6
		 DO WHILE (INPUT(I:I).EQ.' '.AND.I.GT.0)
		    I = I + 1
		    IF (I.GT.L_INPUT) I = 0
		 END DO
		 INPUT = INPUT(I:L_INPUT)
		 L_INPUT = L_INPUT - I + 1
	      END IF
	      CALL LIB$SPAWN('$CHMAIL/I '//INPUT(:L_INPUT)//
     &		'@XX/SUBJECT='//BULL_PARAMETER,,,,,,STATUS)
	   ELSE
	      WRITE (6,'('' ERROR: Cannot respond to mail.'')')
	   END IF
	END IF

	RETURN

	END


	INTEGER FUNCTION CONFIRM_USER(USERNAME)
C
C  FUNCTION CONFIRM_USER
C
C  FUNCTION: Confirms that username is valid user.
C
	IMPLICIT INTEGER (A-Z)

	CHARACTER*(*) USERNAME

	CALL OPEN_FILE_SHARED(8)

	READ (8,KEY=USERNAME,IOSTAT=CONFIRM_USER)

	CALL CLOSE_FILE(8)

	RETURN
	END



	SUBROUTINE SEARCH(READ_COUNT)
C
C  SUBROUTINE SEARCH
C
C  FUNCTION: Search for bulletin with specified string
C
	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLDIR.INC'

	COMMON/INPUT_BULL/INPUT
	CHARACTER*80 INPUT

	CHARACTER*132 SEARCH_STRING,SAVE_STRING
	DATA SEARCH_STRING /' '/, SEARCH_LEN /1/

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	CALL DISABLE_CTRL

	IF (CLI$PRESENT('START')) THEN		! Starting message specified
	   CALL CLI$GET_VALUE('START',BULL_PARAMETER,LEN_P)
	   DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER) BULL_POINT
	   BULL_POINT = BULL_POINT - 1
	END IF

	SAVE_STRING = SEARCH_STRING
	SAVE_LEN = SEARCH_LEN

	IER = CLI$GET_VALUE('SEARCH_STRING',SEARCH_STRING,SEARCH_LEN)
	
	IF (.NOT.IER) THEN			! If no search string entered
	   SEARCH_STRING = SAVE_STRING		! use saved search string
	   SEARCH_LEN = SAVE_LEN
	ELSE IF (.NOT.CLI$PRESENT('START')) THEN ! If string entered but no
	   BULL_POINT = 0			 ! starting message, use first
	END IF

	CALL STR$UPCASE(SEARCH_STRING,SEARCH_STRING)	! Make upper case

	CALL OPEN_FILE_SHARED(2)

	CALL READDIR(0,IER)

	IF (BULL_POINT+1.GT.NBULL) THEN
	   WRITE (6,'('' ERROR: No more messages.'')')
	   CALL CLOSE_FILE(2)
	   CALL ENABLE_CTRL
	   RETURN
	END IF

	CALL OPEN_FILE_SHARED(1)

	DO BULL_SEARCH = BULL_POINT+1, NBULL
	   CALL READDIR(BULL_SEARCH,IER)	! Get bulletin directory entry
	   IF (IER.EQ.BULL_SEARCH+1) THEN
	      LEN = 81
	      DO J=BLOCK,BLOCK+LENGTH-1
	         DO WHILE (LEN.GT.0)
	            CALL GET_BULL(J,INPUT,LEN)
	            CALL STR$UPCASE(INPUT,INPUT)	! Make upper case
		    IF (INDEX(INPUT,SEARCH_STRING(1:SEARCH_LEN)).GT.0) THEN
		       CALL CLOSE_FILE(1)
		       CALL CLOSE_FILE(2)
		       CALL ENABLE_CTRL
		       BULL_POINT = BULL_SEARCH - 1
	               CALL READ(READ_COUNT,BULL_POINT+1) ! Read next bulletin
		       RETURN
		    END IF
	         END DO
		 LEN = 80
	      END DO
	   END IF
	END DO

	CALL CLOSE_FILE(1)			! End of bulletin file read
	CALL CLOSE_FILE(2)

	CALL ENABLE_CTRL

	WRITE (6,'('' No messages found with given search string.'')')

	RETURN
	END





	SUBROUTINE UPDATE
C
C  SUBROUTINE UPDATE
C
C  FUNCTION:  Searches for bulletins that have expired and deletes them.
C
C  NOTE:  Assumes directory file is already opened.
C
	IMPLICIT INTEGER (A - Z)
	CHARACTER*107 DIRLINE

	INCLUDE 'BULLDIR.INC'

	CHARACTER*11 TEMP_DATE,TEMP_EXDATE
	CHARACTER*8 TEMP_TIME,TEMP_EXTIME

	TEMP_EXDATE = '5-NOV-2000'  ! If a bulletin gets deleted, and there are
	TEMP_EXTIME = '00:00:00'    ! are no more bulletins, this is the value
				    ! assigned to the latest expiration date

	TEMP_DATE = '5-NOV-1956' 	! Storage for computing newest
	TEMP_TIME = '00:00:00'		! bulletin date if deletion occurs

	CALL OPEN_FILE(1)			! Open both bulletin files

	BULL_ENTRY = 1				! Init bulletin pointer
	UPDATE_DONE = 0			! Flag showing bull has been deleted

	DO WHILE (1)
	   CALL READDIR(BULL_ENTRY,IER)		! Get next directory entry
	   IF (IER.EQ.BULL_ENTRY) GO TO 100	! ERROR: Not found
	   IF (SYSTEM.LE.1.OR.(SHUTDOWN.EQ.0	! If not permanent, or time
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
	    ELSE IF (SYSTEM.LE.1) THEN		! Expiration date hasn't passed
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
	CALL CLOSE_FILE(1)
C
C  If newest message date has been changed, must change it in BULLUSER.DAT
C  and also see if it affects notification of new messages to users
C
	IF (TEMP_DATE.NE.DATE.OR.TEMP_TIME.NE.TIME) THEN
	   CALL UPDATE_LOGIN(.FALSE.)
	END IF

	RETURN

	END



	SUBROUTINE UPDATE_READ(NEW_BULL)
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

	DIMENSION TODAY_BTIM(2)

C
C  Update user's latest read time in his entry in BULLUSER.DAT.
C

	NEW_BULL = .FALSE.

	CALL OPEN_FILE_SHARED(4)		! Get BULLUSER.DAT file

	DO WHILE (REC_LOCK(IER))
	   READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER)
     &		TEMP_USER,NEWEST_BTIM,	! Get newest bulletin
     &		BBOARD_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	END DO

	IF (IER.NE.0) THEN			! If header not present, exit
	   CALL CLOSE_FILE(4)
	   RETURN
	ELSE IF (NEW_FLAG(1).EQ.-1.AND.NEW_FLAG(2).EQ.-1) THEN
						! If header present, but no
	   DO I=1,FLONG				! SET_FLAG and NOTIFY_FLAG
	      SET_FLAG(I) = 0			! information, write default
	      NOTIFY_FLAG(I) = 0		! flags.
	      BRIEF_FLAG(I) = 0
	      NEW_FLAG(I) = 0
	   END DO
	   SET_FLAG(1) = 1
	   NEW_FLAG(1) = PRV$M_OPER.OR.PRV$M_CMKRNL.OR.PRV$M_SETPRV
	   REWRITE (4,FMT=USER_FMT)
     &		TEMP_USER,NEWEST_BTIM,BBOARD_BTIM,
     &		NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	END IF

	CALL SYS$ASCTIM(,TODAY,,)		! Get today's time
	CALL SYS$BINTIM(TODAY,TODAY_BTIM)

	DO WHILE (REC_LOCK(IER1))
	   READ (4,FMT=USER_FMT,KEY=USERNAME,IOSTAT=IER1) USERNAME,
     &	    LOGIN_BTIM,READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,
     &      NOTIFY_FLAG			! Find user's info
	END DO

	IF (IER1.EQ.0) THEN			! If entry found, update it
	   DIFF = COMPARE_BTIM(READ_BTIM,NEWEST_BTIM)
	   IF (DIFF.LE.0) NEW_BULL = .TRUE.	! If new bull set flag
C
C  No need to update read time/date if no new bulletins and no READNEW set,
C  unless new bulletin is in general folder.
C
	   TEST_NEW_BULL = 0
	   I = 0
	   DO WHILE (TEST_NEW_BULL.EQ.0.AND.I.LT.FLONG)
	      I = I + 1
	      TEST_NEW_BULL = NEW_FLAG(I).AND.SET_FLAG(I)
	   END DO
	   IF (TEST_NEW_BULL.NE.0.OR.NEW_BULL) THEN
	      REWRITE (4,FMT=USER_FMT) USERNAME,LOGIN_BTIM,TODAY_BTIM,
     &		NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	   END IF
	ELSE					! If no entry create a new entry
	   NEW_BULL = .TRUE.
	   DO I=1,FLONG
	      NEW_FLAG(I) = 'FFFFFFFF'X
	   END DO
	   WRITE (4,FMT=USER_FMT) USERNAME,TODAY_BTIM,TODAY_BTIM,
     &	    NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
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

	CHARACTER READ_DATE_TIME*20,LOGIN_DATE_TIME*20

	CALL SYS$ASCTIM(,READ_DATE_TIME,READ_BTIM,)
	CALL SYS$ASCTIM(,LOGIN_DATE_TIME,LOGIN_BTIM,)
C
C  Now see if bulletins have been added since the user's previous
C  read time.  If they have, then search for the first new bulletin.
C  Ignore new bulletins that are owned by the user or system notices
C  that have not been added since the user has logged in.
C
	BULL_POINT = -1				! Init bulletin pointer
C
C  Following stores a "possible" new bulletin.  That is, the user has
C  READNEW set, but ignored reading the bulletins.  The user then enters
C  BULLETIN, and if new bulletins are added after logging in, we want to
C  point to that bulletin.  However, if there were none added since then,
C  we want to point to the first unread one.  Thus, the first new unread
C  bulletin is stored in BULL_POSSIBLE, and the search continues for
C  new bulletins since logging in.
C
	BULL_POSSIBLE = -1

	CALL OPEN_FILE_SHARED(2)		! Yep, so get directory file
	CALL READDIR(0,IER)			! Get # bulletins from header
	IF (IER.EQ.1) THEN			! If header present
	   DO ICOUNT=1,NBULL			! Get each bulletin to compare
	      CALL READDIR(ICOUNT,IER)		! its date with last read date
	      IF (FROM.NE.USERNAME) THEN	! Ignore bull if owner is user
	         DIFF = COMPARE_DATE(READ_DATE_TIME(1:11),DATE)
	         IF (DIFF.EQ.0) 
     &			DIFF = COMPARE_TIME(READ_DATE_TIME(13:20),TIME)
	         IF (DIFF.LE.0) THEN		! If new bull or new user
		    IF (SYSTEM) THEN		! If system bulletin
		       DIFF = COMPARE_DATE(LOGIN_DATE_TIME(1:11),DATE)
		       IF (DIFF.EQ.0)
     &			DIFF = COMPARE_TIME(LOGIN_DATE_TIME(13:20),TIME)
		       IF (DIFF.LE.0) THEN	! If system bull, make it
		          BULL_POINT = ICOUNT - 1 ! the first new bull only
			  GO TO 100		! if added since user logged in
		       END IF			! else he's read it already.
		    ELSE
		       IF (TEST2(SET_FLAG,FOLDER_NUMBER)) THEN
			IF (BULL_POSSIBLE.EQ.-1) BULL_POSSIBLE = ICOUNT - 1
		        DIFF = COMPARE_BTIM(LOGIN_BTIM,READ_BTIM)
			IF (DIFF.GT.0) THEN
		         DIFF = COMPARE_DATE(LOGIN_DATE_TIME(1:11),DATE)
		         IF(DIFF.EQ.0)
     &			  DIFF=COMPARE_TIME(LOGIN_DATE_TIME(13:20),TIME)
			END IF
		       END IF
		       IF (DIFF.LE.0) THEN
		        BULL_POINT = ICOUNT - 1  ! If not system bull then
		        GO TO 100		! make it the new bull
		       END IF
		    END IF
		 END IF
	      END IF
	   END DO
	END IF

	BULL_POINT = BULL_POSSIBLE

100	CALL CLOSE_FILE(2)			! Its time for this program

	RETURN
	END



	SUBROUTINE GET_EXPIRED(INPUT,IER)

	IMPLICIT INTEGER (A-Z)

	CHARACTER*20 INPUT
	CHARACTER*23 TODAY

	DIMENSION EXTIME(2),NOW(2)

	IER = SYS$ASCTIM(,TODAY,,)		! Get today's date

5	WRITE(6,1030) TODAY			! Prompt for expiration date
	CALL GET_LINE(INPUT,LEN)		! Get input line

	IF (LEN.LE.0) THEN
	   IER = 0
	   RETURN
	END IF

	INPUT = INPUT(1:LEN)			! Change trailing zeros 2 spaces

	IF (INDEX(INPUT,'-').EQ.0.AND.INDEX(INPUT,':').GT.0.AND.
     &		INDEX(INPUT(1:LEN),' ').EQ.0) THEN
	   INPUT = TODAY(1:INDEX(TODAY(2:),' ')+1)//INPUT
	END IF

	CALL STR$UPCASE(INPUT,INPUT)		! Convert to upper case
	IER = SYS$BINTIM(INPUT,EXTIME)
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
	IER = COMPARE_DATE(INPUT(1:11),TODAY(1:11)) ! Compare date with today's
	IF (IER.EQ.0) IER = COMPARE_TIME(INPUT(13:20),TODAY(13:20))
	IF (IER.LE.0) THEN			! If expiration date not future
	   WRITE(6,1045)			! tell user
	   GO TO 5				! and re-request date
	END IF

	IER = 1

	RETURN

1030	FORMAT (' It is ',A23,
     &'. Specify when the message should expire:',/,1x,
     &'Enter absolute time: [dd-mmm-yyyy] hh:mm:ss ',
     &'or delta time: dddd hh:mm:ss')
1040	FORMAT (' ERROR: Invalid date format specified.')
1045	FORMAT (' ERROR: Specified time has already passed.')

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
	   CALL LIB$SPAWN('$@'//MAIL_EDIT(:TRIM(MAIL_EDIT))
     &		//' '//INFILE//' '//OUT(:TRIM(OUT)))
	ELSE IF (INDEX(MAIL_EDIT,'EDT').GT.0) THEN
	   CALL EDT$EDIT(INFILE,OUT)
	ELSE IF (INDEX(MAIL_EDIT,'TPU').GT.0) THEN
	   CALL TPU$EDIT(INFILE,OUT)
	   IER = CLI$DCL_PARSE(INCMD,BULLETIN_SUBCOMMANDS)
		! TPU does CLI$ stuff which wipes our parsed command line
	END IF

	RETURN
	END
