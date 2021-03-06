C
C  BULLSUB0.FOR, Version 10/1/86
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Programmer: Mark R. London
C
	SUBROUTINE MAIL(STATUS)
C
C  SUBROUTINE MAIL
C
C  FUNCTION: Sends message which you have read to user via DEC mail.
C
	IMPLICIT INTEGER (A - Z)

	CHARACTER INPUT*80

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	INCLUDE 'BULLDIR.INC'

	EXTERNAL CLI$_ABSENT

	IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been read
	   WRITE(6,'('' ERROR: You have not read any message.'')')
	   RETURN			! And return
	END IF

	CALL OPEN_FILE_SHARED(2)

	CALL READDIR(BULL_POINT,IER)	! Get info for specified bulletin

	IF (IER.NE.BULL_POINT+1) THEN	! Was bulletin found?
	   WRITE(6,'('' ERROR: Specified message was not found.'')')
	   CALL CLOSE_FILE(2)		! If not, then error out
	   RETURN
	END IF

	CALL CLOSE_FILE(2)

	OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,
     &		STATUS='NEW',CARRIAGECONTROL='LIST')

	IF (IER.NE.0) THEN
	   WRITE(6,'('' ERROR: Error in opening scratch file.'')')
	   RETURN
	END IF

	CALL OPEN_FILE_SHARED(1)	! Open BULLETIN file

	LEN = 81
	DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file
	   DO WHILE (LEN.GT.0)
	      CALL GET_BULL(I,INPUT,LEN)
	      IF (LEN.LT.0) THEN
		 GO TO 90
	      ELSE IF (LEN.GT.0) THEN
	         WRITE (3,'(A)') INPUT(1:LEN)
	      END IF
	   END DO
	   LEN = 80
	END DO

90	CLOSE (UNIT=3)			! Message copy completed

	CALL CLOSE_FILE(1)

	IF (CLI$PRESENT('SUBJECT')) THEN
	   IER = CLI$GET_VALUE('SUBJECT',DESCRIP,LEN_D)
	ELSE
	   LEN_D = TRIM(DESCRIP)
	END IF

	IER = CLI$GET_VALUE('RECIPIENTS',BULL_PARAMETER,LEN_P)

	IF (DESCRIP(1:1).NE.'"') THEN
	   DESCRIP = '"'//DESCRIP(1:LEN_D)
	   LEN_D = LEN_D + 1
	END IF

	IF (DESCRIP(LEN_D:LEN_D).NE.'"') THEN
	   DESCRIP = DESCRIP(1:LEN_D)//'"'
	   LEN_D = LEN_D + 1
	END IF
	   
	CALL LIB$SPAWN('$MAIL/SUBJECT='//DESCRIP(1:LEN_D)//
     &	   ' SYS$LOGIN:BULL.SCR '//BULL_PARAMETER(1:LEN_P),,,,,STATUS)

	CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR')

	RETURN

	END



	SUBROUTINE MOVE(DELETE_ORIGINAL)
C
C  SUBROUTINE MOVE
C
C  FUNCTION: Moves message from one folder to another.
C
	IMPLICIT INTEGER (A - Z)

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /ACCESS/ READ_ONLY
	LOGICAL READ_ONLY

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	EXTERNAL CLI$_ABSENT

	LOGICAL DELETE_ORIGINAL

	CHARACTER INPUT*80

	IF (BULL_POINT.EQ.0) THEN	! If no message has been read
	   WRITE(6,'('' ERROR: You are not reading any message.'')')
	   RETURN			! and return
	END IF

	CALL OPEN_FILE_SHARED(2)
	CALL READDIR(BULL_POINT,IER)		! Get message directory entry
	CALL CLOSE_FILE(2)
	IF (IER.NE.BULL_POINT+1) THEN		! Was message found?
	   WRITE(6,'('' ERROR: Specified message was not found.'')')
	   RETURN
	END IF

	SAVE_BULL_POINT = BULL_POINT

	OPEN (UNIT=3,STATUS='SCRATCH',FILE='SYS$LOGIN:BULL.SCR',
     &		FORM='FORMATTED')		! Scratch file to save bulletin

	CALL OPEN_FILE_SHARED(1)

	LEN = 81
	DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file
	   DO WHILE (LEN.GT.0)
	      CALL GET_BULL(I,INPUT,LEN)
	      IF (LEN.LT.0) THEN
		 GO TO 90
	      ELSE IF (LEN.GT.0) THEN
	         WRITE (3,'(A)') INPUT(1:LEN)
	      END IF
	   END DO
	   LEN = 80
	END DO

90	REWIND (UNIT=3)			! Bulletin copy completed

	CALL CLOSE_FILE(1)

	SAVE_FOLDER_NUMBER = FOLDER_NUMBER
	CALL CLI$GET_VALUE('FOLDER',FOLDER1)

	FOLDER_NUMBER = -1	! Use FOLDER as key rather than FOLDER_NUMBER
	CALL SELECT_FOLDER(.FALSE.,IER)

	IF (.NOT.IER.OR.READ_ONLY) THEN
	   WRITE (6,'('' ERROR: Cannot access specified folder.'')')
	   CLOSE (UNIT=3)
	   FOLDER_NUMBER = SAVE_FOLDER_NUMBER
	   RETURN
	END IF

C
C  Add bulletin to bulletin file and directory entry for to directory file.
C

	CALL OPEN_FILE(2)			! Prepare to add dir entry

	CALL OPEN_FILE(1)			! Prepare to add bulletin

	CALL READDIR(0,IER)			! Get NBLOCK
	IF (IER.EQ.0) NBLOCK = 0		! If new file, NBLOCK is 0

	CALL COPY_BULL(3,1,NBLOCK+1,IER)	! Add the new bulletin
	IF (IER.NE.0) THEN			! Error in creating bulletin
	   WRITE(6,'('' ERROR: Message copy aborted.'')')
	   CALL CLOSE_FILE(1)
	   CALL CLOSE_FILE(2)
	   CLOSE (UNIT=3)
	END IF

	CALL CLOSE_FILE(1)			! Finished adding bulletin

	SYSTEM = IBCLR(SYSTEM,0)		! Remove system bit
	IF (BTEST(SYSTEM,2)) THEN		! Shutdown message?
	   SYSTEM = IBCLR(SYSTEM,2)		! Remove shutdown bit
	   CALL GET_EXDATE(EXDATE,FOLDER_BBEXPIRE)
	END IF

	FROM = USERNAME				! New bulletin has new owner
	CALL ADD_ENTRY				! Add the new directory entry

	CALL CLOSE_FILE(2)			! Totally finished with add

	CLOSE (UNIT=3)			! Close the input file

	WRITE (6,'('' Message has been copied to folder '',A)')
     &		FOLDER(1:TRIM(FOLDER))//'.'

	FOLDER_NUMBER = SAVE_FOLDER_NUMBER
	CALL SELECT_FOLDER(.FALSE.,IER)

	BULL_POINT = SAVE_BULL_POINT

	IF (DELETE_ORIGINAL) CALL DELETE

	RETURN

	END





	SUBROUTINE READNEW
C
C  SUBROUTINE READNEW
C
C  FUNCTION: Displays new non-system bulletins with prompts between bulletins.
C

	IMPLICIT INTEGER (A-Z)

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'

	COMMON /POINT/ BULL_POINT

	CHARACTER INREAD*1,INPUT*80,FILE_DEF*80

	DATA LEN_FILE_DEF /0/, INREAD/0/

	LOGICAL SLOW,SLOW_TERMINAL

C
C  This subroutine is executed due to the BULLETIN/LOGIN command which is
C  normally executed by a command procedure during login.  In order to use
C  LIB$GET_INPUT, we must redefine SYS$INPUT to the terminal (temporarily
C  using user mode).
C
	IF (ICHAR(INREAD).EQ.0) THEN
	   CALL CRELNM('SYS$INPUT','TT')
	   CALL PURGE_TYPEAHEAD
	   SLOW = SLOW_TERMINAL()
	END IF

	LEN_P = 0			! Tells read subroutine there is
					! no bulletin parameter

1	WRITE(6,1000)			! Ask if want to read new bulletins

	INREAD = '0'
	TEMP_READ = 0
	DO WHILE (INREAD.GE.'0'.AND.INREAD.LE.'9')
	   CALL GET_INPUT_NOECHO(INREAD)
	   CALL STR$UPCASE(INREAD,INREAD)		! Make input upper case
	   IF (TEMP_READ.GT.0.AND.(INREAD.LT.'0'.OR.INREAD.GT.'9').AND.
     &			INREAD.NE.CHAR(13)) THEN
	      GO TO 1
	   ELSE IF (INREAD.EQ.'N'.OR.INREAD.EQ.'Q') THEN
	      IF (INREAD.EQ.'Q') THEN
	         WRITE (6,'(''+Quit'',$)')
	      ELSE
	         WRITE (6,'(''+No'',$)')
	      END IF
	      RETURN	! If NO, exit
			! Include QUIT to be consistent with next question
	   ELSE IF (INREAD.GE.'0'.AND.INREAD.LE.'9') THEN
	      TEMP_READ = TEMP_READ*10 + ICHAR(INREAD) - ICHAR('0')
	      WRITE (6,'(''+'',A1,$)') INREAD
	   END IF
	END DO

	IF (TEMP_READ.GT.0) THEN
	   IF (TEMP_READ.LT.BULL_POINT+1.OR.TEMP_READ.GT.NBULL) THEN
	      WRITE (6,'('' ERROR: Specified new message not found.'')')
	      GO TO 1
	   ELSE
	      BULL_POINT = TEMP_READ - 1
	   END IF
	END IF

	READ_COUNT = 0				! Initialize display pointer

5	CALL READ(READ_COUNT,BULL_POINT+1)	! Read next bulletin
	FILE_POINT = BULL_POINT
	IF (READ_COUNT.EQ.0) THEN		! Is full bulletin displayed?
	   CALL OPEN_FILE_SHARED(2)		! If so, see if more new bulls
10	   CALL READDIR(BULL_POINT+1,IER_POINT)
	   IF ((IER_POINT.EQ.BULL_POINT+2).AND.(SYSTEM)) THEN
	      BULL_POINT = BULL_POINT + 1	! If system bulletin, skip it.
	      GO TO 10
	   END IF
	   CALL CLOSE_FILE(2)
	END IF

12	IF (READ_COUNT.EQ.0) THEN		! Prompt user in between
	   WRITE(6,1020)			! full screens or end of bull.
	ELSE
	   IF (READ_COUNT.EQ.BLOCK) THEN
	      WRITE(6,1030) 'TEXT'
	   ELSE
	      WRITE(6,1030) 'MORE'
	   END IF
	END IF

	CALL GET_INPUT_NOECHO(INREAD)
	CALL STR$UPCASE(INREAD,INREAD)	! Convert input to upper case

	IF (INREAD.EQ.'Q') THEN		! If Q , then QUIT
	   WRITE (6,'(''+Quit'',$)')
	   RETURN
	ELSE IF (INREAD.EQ.'F') THEN	! If F then copy bulletin to file
	   WRITE (6,'(''+ '')')		! Move cursor from end of prompt line
					! to beginning of next line.
	   IF (LEN_FILE_DEF.EQ.0) THEN
	      CALL LIB$SYS_TRNLOG('SYS$LOGIN',LEN,FILE_DEF)
	      IER = LIB$FIND_FILE(FILE_DEF//'BULL.DIR',
     &			BULL_PARAMETER,CONTEXT)
	      IF (IER) THEN
		 FILE_DEF = BULL_PARAMETER(:LEN-1)//'.BULL]'
		 LEN_FILE_DEF = LEN + 5
	      ELSE
	         FILE_DEF = 'SYS$LOGIN:'
	         LEN_FILE_DEF = 10
	      END IF
	   END IF

	   LEN_FOLDER = TRIM(FOLDER)
	   CALL GET_INPUT_PROMPT(BULL_PARAMETER,LEN_P,
     &		'Name of file? (Default='//FILE_DEF(:LEN_FILE_DEF)//
     &		FOLDER(:LEN_FOLDER)//'.LIS) ')

	   IF (LEN_P.EQ.0) THEN
	      BULL_PARAMETER = FILE_DEF(:LEN_FILE_DEF)//FOLDER(:LEN_FOLDER)
     &			//'.LIS'
	      LEN_P = LEN_FILE_DEF + LEN_FOLDER + 4
	   END IF

	   BLOCK_SAVE = BLOCK
	   LENGTH_SAVE = LENGTH
	   CALL OPEN_FILE_SHARED(2)
	   CALL OPEN_FILE_SHARED(1)		! Open BULLETIN file
	   CALL READDIR(FILE_POINT,IER)
	   IF (.NOT.SETPRV_PRIV()) THEN		! If no SETPRV, remove SYSPRV
	      CALL DISABLE_PRIVS		! privileges when trying to
	   END IF				! create new file.
	   OPEN(UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),IOSTAT=IER,ERR=18,
     &		STATUS='UNKNOWN',CARRIAGECONTROL='LIST',ACCESS='APPEND')
	   WRITE(3,1050) DESCRIP		! Output bulletin header info
	   WRITE(3,1060) FROM,DATE
	   LEN = 81
	   DO I=BLOCK,BLOCK+LENGTH-1		! Copy bulletin into file
	      DO WHILE (LEN.GT.0)
	         CALL GET_BULL(I,INPUT,LEN)
		 IF (LEN.LT.0) THEN
		   GO TO 18
		 ELSE IF (LEN.GT.0) THEN
	            WRITE(3,'(A)') INPUT(1:TRIM(INPUT))
		 END IF
	      END DO
	      LEN = 80
	   END DO
	   WRITE(6,1040) BULL_PARAMETER(1:LEN_P)
						! Show name of file created.
18	   IF (IER.NE.0) THEN
	      CALL ERRSNS(IDUMMY,IER)
	      CALL SYS_GETMSG(IER)
	   END IF
	   CLOSE (UNIT=3)			! Bulletin copy completed
	   CALL CLOSE_FILE(1)
	   CALL CLOSE_FILE(2)
	   LENGTH = LENGTH_SAVE
	   BLOCK = BLOCK_SAVE
	   CALL ENABLE_PRIVS			! Reset BYPASS privileges
	   GO TO 12
	ELSE IF (INREAD.EQ.'N'.AND.READ_COUNT.GT.0) THEN
				! If NEXT and last bulletins not finished
	   READ_COUNT = 0			! Reset read bulletin counter
	   CALL OPEN_FILE_SHARED(2)		! Look for NEXT bulletin
20	   CALL READDIR(BULL_POINT+1,IER)
	   IF (IER.NE.BULL_POINT+2) THEN	! If no NEXT bulletin
	      CALL CLOSE_FILE(2)		! Exit
	      WRITE(6,1010)
	      RETURN
	   ELSE IF (SYSTEM) THEN		! Else if NEXT bulletin SYSTEM
	      BULL_POINT = BULL_POINT + 1	! Skip it
	      GO TO 20			! Look for more bulletins
	   END IF
	   CALL CLOSE_FILE(2)
	ELSE IF (IER_POINT.NE.BULL_POINT+2.AND.READ_COUNT.EQ.0) THEN
	   WRITE(6,1010)
	   RETURN
	END IF
	IF (READ_COUNT.EQ.0.AND.SLOW) READ_COUNT = -2
	GO TO 5

1000	FORMAT(' Read messages? Type N(No),Q(Quit),message
     & number, or any other key for yes: ',$)
1010	FORMAT(' No more messages.')
1020	FORMAT(1X,80('-'),/,
     &' Type Q(Quit), F(File it) or any other key for next message: ',$)
1030	FORMAT(1X,80('-'),/,' Type Q(Quit), F(File it), N(Next message),
     & or any other key for ',A4,'... ',$)
1040	FORMAT(' Message written to ',A)
1050	FORMAT(/,'Description: ',A53)
1060	FORMAT('From: ',A12,' Date: ',A20,/)

	END




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

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	CHARACTER INEXDATE*11,INEXTIME*8,MAILEDIT*80
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
     &		CLI$PRESENT('EDIT')) THEN	! or /EDIT specified

	   IF (CLI$PRESENT('EDIT')) THEN	! If /EDIT specified, then
	      IER = LIB$SYS_TRNLOG('MAIL$EDIT',LEN,MAILEDIT)
	      IF (IER.NE.SS$_NORMAL) MAILEDIT = 'SYS$SYSTEM:MAILEDIT'
	      IF (LEN_P.EQ.0) THEN		! If no file param specified
		 IF (.NOT.CLI$PRESENT('NEW')) THEN
	            OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='NEW',V
     &		       ERR=920,FORM='FORMATTED',CARRIAGECONTROL='LIST') 
	            CALL OPEN_FILE_SHARED(1)	! Prepare to copy message
		    LEN = 81
		    DO I=BLOCK,BLOCK+LENGTH-1	! Copy mesage into fileO
		       DO WHILE (LEN.GT.0)
			  CALL GET_BULL(I,INPUT,LEN)t
			  IF (LEN.LT.0) THEN
			     GO TO 5 
			  ELSE IF (LEN.GT.0) THEN
			     WRITE (3,'(A)') INPUT(1:LEN)
			  END IFR
		       END DOL
		       LEN = 80 
		    END DO
5		    CALL CLOSE_FILE(1)
	            CLOSE (UNIT=3)		! Bulletin copy completed
		 END IFe
		 CALL LIB$SPAWN('$@'//MAILEDIT//' "" SYS$LOGIN:BULL.SCR') 
	      ELSE 
		 IF (.NOT.SETPRV_PRIV()) CALL DISABLE_PRIVSC
	         CALL LIB$SPAWN('$@'//MAILEDIT//' '//BULL_PARAMETER(1:LEN_P)r
     &			//' SYS$LOGIN:BULL.SCR').
	      END IFH
	      IER = LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;-1')f
	      OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='OLD',f
     &		 DISPOSE='DELETE',ERR=920,FORM='FORMATTED')L
	   ELSE IF (LEN_P.GT.0) THENL
	      IF (.NOT.SETPRV_PRIV()) CALL DISABLE_PRIVS'
	      OPEN (UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),STATUS='OLD',
     &		READONLY,SHARED,ERR=920,FORM='FORMATTED') ! Try opening the file
	   END IF

	   CALL ENABLE_PRIVS			! Reset SYSPRV privilegesD

	   DO WHILE(1)				! Read until end of file to
	      READ (3,'(Q,A)',END=10) LEN,INPUT	! get record count
	      IF (LEN.GT.80) GO TO 950G
	      CALL STR$TRIM(INPUT,INPUT,LEN)N
	      IF (INDEX(INPUT,CHAR(9)).GT.0) THEN
	         EXTRA = 0 
	         DO I=1,LEN
	 	    IF (INPUT(I:I).EQ.CHAR(9)) THEN
		       EXTRA = EXTRA + 8 - MOD(I+EXTRA,8)C
		    END IF
	         END DO
		 IF (LEN+EXTRA.GT.80) GO TO 950C
	      END IFS
	      IF (LEN.GT.0) THEN		! If good input line enteredT
		 ICOUNT = ICOUNT + LEN + 1	! Increment record countR
		 LAST_NOBLANK = ICOUNT
	      ELSE IF (LEN.EQ.0) THEN
		 IF (ICOUNT.GT.0) THEN
		    ICOUNT = ICOUNT + 2		! COPY_BULL writes a line withC
		 ELSE				! 1 space for a blank line.
		    REC1 = REC1 + 1
		 END IF 
	      END IFL
	   END DO
	  ELSE					! If no input file
	   OPEN (UNIT=3,STATUS='SCRATCH',FILE='SYS$LOGIN:BULL.SCR',
     &		FORM='FORMATTED')		! Scratch file to save bulletin
	   WRITE (6,1000)		! Request bulletin input from terminal
	   LEN = 80				! Length of input line
	   DO WHILE (LEN.GE.0)			! Input until no more inputA
	      CALL GET_LINE(INPUT,LEN)		! Get input lineL
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
	   IF (LEN.EQ.-1) GO TO 910		! CTRL_C entered, error out?
10	   ICOUNT = LAST_NOBLANKc
	   IF (ICOUNT.EQ.0) GO TO 910		! No lines entered, error outV
	  ENDIF

	  REWIND (UNIT=3)
	END IF=

CT
C  Add bulletin to bulletin file and directory entry for to directory file.h
Cl

	CALL OPEN_FILE(2)			! Prepare to add dir entry

	INPUT = DESCRIP
	CALL READDIR(NUMBER_PARAM,IER)		! Get info for messageE

	IF (IER.NE.NUMBER_PARAM+1.OR.INPUT.NE.DESCRIP) THEN
				! Message disappeared in the mean time?S
	   CALL CLOSE_FILE(2)
	   WRITE(6,'('' ERROR: Message file info invalidated.
     & Find message and do REPLACE again.'')')
	   GO TO 100e
	END IFc

	CALL READDIR(0,IER)			! Get directory headerN

	LENGTH_SAVE = LENGTH			! Copy BULL modifies LENGTHL
	BLOCK_SAVE = BLOCKB

	IF (CLI$PRESENT('TEXT').OR.DOALL) THEN	! If text has been replacedL
	   CALL OPEN_FILE(1)			! Prepare to add bulletinN
	   ICOUNT = (ICOUNT+127)/128o
	   IF (ICOUNT.GT.LENGTH.AND.NBULL.GT.NUMBER_PARAM) THEN
	      BLOCK = NBLOCK + 1_
	      NBLOCK = NBLOCK + ICOUNT
	      BLOCK_SAVE = BLOCKi
	      NEMPTY = NEMPTY + LENGTH 
	      CALL WRITEDIR(0,IER) 
	   ELSE IF (ICOUNT.LT.LENGTH) THENn
	      NEMPTY = NEMPTY + LENGTH - ICOUNT
	      CALL WRITEDIR(0,IER)(
	   END IF
	   CALL COPY_BULL(3,REC1,BLOCK,IER)	! Replace old bulletini

	   CALL CLOSE_FILE(1)

	   IF (ICOUNT.NE.LENGTH_SAVE) THEN	! If new bull different size
	      CALL READDIR(NUMBER_PARAM,IER)	! Get directory entrya
	      LENGTH = ICOUNT			! Update size
	      BLOCK = BLOCK_SAVEL
	      CALL WRITEDIR(NUMBER_PARAM,IER)	! Write new directory entry
	   END IF
	END IFB

	CALL READDIR(NUMBER_PARAM,IER)
	IF (CLI$PRESENT('HEADER').OR.DOALL) DESCRIP=INDESCRIP(1:53)
						! Update description header 
	IF (CLI$PRESENT('EXPIRATION').OR.DOALL) THENE
	   SYSTEM = IBCLR(SYSTEM,1)
	   SYSTEM = IBCLR(SYSTEM,2)
	   EXDATE=INEXDATE			! Update expiration date
	   EXTIME=INEXTIME_
	   DIFF = COMPARE_DATE(EXDATE,NEWEST_EXDATE)	! Compare expiration
	   IF (DIFF.EQ.0) DIFF = COMPARE_TIME(EXTIME,NEWEST_EXTIME)
	   IF (DIFF.LT.0) THEN			! If it's oldest expiration bull
	      NEWEST_EXDATE = EXDATE		! Update the header inL
	      NEWEST_EXTIME = EXTIME		! the directory fileL
	      CALL WRITEDIR(0,IER)
	   END IF
	ELSE IF (CLI$PRESENT('PERMANENT').AND.
     &			(.NOT.BTEST(SYSTEM,1))) THEN:
	   IF (BTEST(SYSTEM,2)) THENi
	      SYSTEM = IBCLR(SYSTEM,2).
	      SHUTDOWN = SHUTDOWN - 1
	      CALL WRITEDIR(0,IER)R
	   END IF
	   SYSTEM = IBSET(SYSTEM,1)
	   EXDATE = '5-NOV-2000'
	   EXTIME = '00:00:00'C
	ELSE IF (CLI$PRESENT('SHUTDOWN').AND.
     &			(.NOT.BTEST(SYSTEM,2))) THEN
	   SYSTEM = IBSET(SYSTEM,2)
	   SYSTEM = IBCLR(SYSTEM,1)
	   EXDATE = '5-NOV-2000'u
	   EXTIME = '00:00:00'h
	   SHUTDOWN = SHUTDOWN + 1h
	   CALL SYS$ASCTIM(,TODAY,,)		! Get the present timeg
	   SHUTDOWN_DATE = TODAY(1:11)G
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
	RETURNT

910	WRITE(6,1010)L
	CLOSE (UNIT=3,ERR=100)A
	GOTO 100

920	WRITE(6,1020)E
	CALL ENABLE_PRIVS	! Reset SYSPRV privileges
	GOTO 100i

950	WRITE (6,1030)
	CLOSE (UNIT=3)0
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
1090	FORMAT(' ERROR: Specified message is not owned by you.')O
1100	FORMAT(' Message is not owned by you.',
     &	       ' Are you sure you want to replace it? ',$) 
2020	FORMAT(1X,A)L

	END





	SUBROUTINE READ(READ_COUNT,BULL_READ)
C
C  SUBROUTINE READ
CI
C  FUNCTION: Reads a specified bulletin.
CD
C  PARAMETER:N
C	READ_COUNT - Variable to store the record in the message fileO
C		that READ will read from.  Must be set to 0 to indicate
C		that it is the first read of the message.  If -1,
C		READ will search for the last message in the message file
C		and read that one.  If -2, just display header information.
C	BULL_READ - Message number to be read.
CG
	IMPLICIT INTEGER (A - Z)A

	COMMON /POINT/ BULL_POINT

	INCLUDE 'BULLDIR.INC'

	COMMON/INPUT_BULL/INPUT
	CHARACTER*80 INPUT	

	COMMON /READIT/ READITl

	COMMON /PAGE/ PAGE_LENGTH

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD

	DATA SCRATCH_B1/0/)

	CHARACTER TODAY*11,DATETIME*23C

	LOGICAL SINCE,PAGER

	CALL LIB$ERASE_PAGE(1,1)		! Clear screenr
	END = 0					! Nothing outputted on screen

	IF (READ_COUNT.GT.0) GO TO 100		! Skip init steps if this isE
						! not first page of bulletin

	SINCE = .FALSE.
	PAGE = .TRUE.
	IF (INCMD(1:4).EQ.'READ') THEN		! If READ command...m
	 IF (.NOT.CLI$PRESENT('PAGE')) PAGE = .FALSE.
	 IF (CLI$PRESENT('SINCE')) THEN		! was /SINCE specified?'
	   IER = CLI$GET_VALUE('SINCE',DATETIME)F
	   IF (DATETIME.EQ.'TODAY') THEN 
	      IER = SYS$ASCTIM(,TODAY,,)		! Get today's dateF
	      DATETIME = TODAY//' 00:00:00.0'
	   END IF
	   CALL OPEN_FILE_SHARED(2)
	   TEMP_READ = 0D
	   IER = 1I
	   DO WHILE (IER.EQ.TEMP_READ+1)
	      TEMP_READ = TEMP_READ + 1
	      CALL READDIR(TEMP_READ,IER)
	      IF (IER.NE.TEMP_READ+1) THENP
		 WRITE (6,'('' No messages found past specified date.'')')
		 CALL CLOSE(2)
		 RETURNR
	      ELSE
	         DIFF = COMPARE_DATE(DATETIME(1:11),DATE)  ! Compare expiration
	         IF (DIFF.EQ.0) DIFF = COMPARE_TIME(DATETIME(13:20),TIME)
	         IF (DIFF.LE.0) THENF
		    BULL_READ = TEMP_READ 
		    IER = IER + 1H
		 END IFO
	      END IF(
	   END DO
	   IER = BULL_READ + 1p
	   SINCE = .TRUE.
	 END IF
	END IF_

	IF (.NOT.SINCE) THENS
	 IF (BULL_READ.GT.0) THEN		! Valid bulletin number?
	   CALL OPEN_FILE_SHARED(2)
	   CALL READDIR(BULL_READ,IER)		! Get bulletin directory entryN
	   IF (READ_COUNT.EQ.-1.AND.IER.NE.BULL_READ+1) THEN
	      READ_COUNT = 0W
	      CALL READDIR(0,IER)
	      IF (NBULL.GT.0) THEN0
	         BULL_READ = NBULLn
	         CALL READDIR(BULL_READ,IER)T
	      ELSE 
		 IER = 0
	      END IFN
	   END IF
	   CALL CLOSE_FILE(2)
	 ELSE
	   IER = 0.
	 END IF
	END IFG

	IF (IER.NE.BULL_READ+1) THEN		! Was bulletin found?
	   WRITE(6,1030)			! If not, then error out
	   RETURN
	END IF(

	BULL_POINT = BULL_READ			! Update bulletin counter=

	WRITE(6,1040) BULL_POINT		! Output bulletin header info
	WRITE(6,1050) DESCRIP
	IF ((SYSTEM.AND.4).EQ.4) THEN		! Is entry shutdown bulletin?M
	   WRITE(6,1065) FROM,DATE,'Expires on shutdown' 
	ELSE IF ((SYSTEM.AND.2).EQ.2) THEN	! Is entry permanent bulletin?
	   WRITE(6,1065) FROM,DATE,'Permanent message'E
	ELSEL
	   WRITE(6,1060) FROM,DATE,EXDATE//' '//EXTIME 
	END IFi

Ce
C  Each page of the bulletin is buffered into temporary memory storage beforeN
C  being outputted to the terminal.  This is to be able to quickly close the
C  bulletin file, and to avoid the possibility of the user holding the screen,
C  and thus causing the bulletin file to stay open.  The temporary memoryl
C  is structured as a linked-list queue, where SCRATCH_B1 points to the header
C  of the queue.  See BULLSUBS.FOR for more description of the queue.L
CO

	IF (SCRATCH_B1.NE.0) THEN		! Is queue empty?	
	   SCRATCH_B = SCRATCH_B1		! No, set queue pointer to head)
	ELSE					! Else if queue is empty
	   CALL INIT_QUEUE(SCRATCH_B,INPUT)
	   SCRATCH_B1 = SCRATCH_B		! Init header pointerQ
	END IFO

	END = 4					! Outputted 4 lines to screen

	READ_ALREADY = 0			! Number of lines already read
						! from record.
	IF (READ_COUNT.EQ.-2) THEN		! Just output header first read
	   READ_COUNT = BLOCK
	   RETURN
	ELSEi
	   READ_COUNT = BLOCK			! Init bulletin record counterA
	END IF-

100	SCRATCH_B = SCRATCH_B1			! Init queue pointer to headern
	IF (READIT.GT.0) END = END - 2		! /READ can output 2 more lines
	DISPLAY = 0
	CALL OPEN_FILE_SHARED(1)		! Get bulletin file
	MORE_LINES = .TRUE.
	READ_REC = READ_COUNT
	IF (READ_ALREADY.EQ.0) LEN = 81
	DO WHILE (MORE_LINES.AND.READ_REC.LE.BLOCK+LENGTH-1)o
	   DO WHILE (LEN.GT.0.AND.MORE_LINES)
	      CALL GET_BULL(READ_REC,INPUT,LEN)
	      IF (LEN.LT.0) THEN		! Error, couldn't read record
		 READ_REC = BLOCK + LENGTH	! Fake end of reading fileR
		 MORE_LINES = .FALSE.O
	      ELSE IF (LEN.GT.0) THEN
	         CALL WRITE_QUEUE(%VAL(SCRATCH_B),SCRATCH_B,INPUT)0
	         READ_ALREADY = 1
		 DISPLAY = DISPLAY + 1
		 IF ((DISPLAY.EQ.PAGE_LENGTH-END-4).AND.PAGE) THEN
		    MORE_LINES = .FALSE.
		 END IFe
	      END IFO
	   END DO
	   LEN = 80
	   IF (MORE_LINES) THEN
	      READ_REC = READ_REC + 1
	      READ_ALREADY = 0h
	   END IF
	END DOW

	CALL CLOSE_FILE(1)			! End of bulletin file readR

C 
C  Bulletin page is now in temporary memory, so output to terminal.u
C  Note that if this is a /READ, the first line will have problems withM
C  the usual FORMAT statement.  It will cause a blank line to be outputted
C  at the top of the screen.  This is because of the input QIO at the 
C  end of the previous page.  The output gets confused and thinks it mustg
C  end the previous line.  To prevent that, the first line of a new page
C  in a /READ must use a different FORMAT statement to surpress the CR/LF.
Ce

	SCRATCH_B = SCRATCH_B1			! Reinit queue pointer to head
	DO I=1,DISPLAY				! Output page to terminal
	   CALL READ_QUEUE(%VAL(SCRATCH_B),SCRATCH_B,INPUT) ! Get queue recordn
	   IF (I.EQ.1.AND.READ_REC.NE.BLOCK.AND.READIT.GT.0) THEN
	      WRITE(6,2020) INPUT(1:TRIM(INPUT))	! (See above comments)
	   ELSE
	      WRITE(6,2010) INPUT(1:TRIM(INPUT))u
	   END IF
	END DO'

	READ_COUNT = READ_REC			! Update bull record counterE

	IF (READ_REC.EQ.BLOCK+LENGTH) THEN	! Last block?.
	   READ_COUNT = 0			! init bulletin record counter 
	ELSE IF (READ_REC.EQ.BLOCK+LENGTH-1.AND..NOT.MORE_LINES) THEN
		! Possibly last block since end of page could be last line
	   CALL TEST_MORE_LINES(LEN)		! More lines to read?
	   IF (LEN.GT.0) THEN			! Yes, there are still more
	      IF (READIT.EQ.0) WRITE(6,1070)	! say there is more of bulletin_
	   ELSE					! Yes, last line anyway
	      READ_COUNT = 0			! init bulletin record counter
	   END IF
	ELSE IF (READIT.EQ.0) THEN		! Else if this is not /READ
	   WRITE(6,1070)			! say there is more of bulletinl
	END IF 

	RETURN.

1030	FORMAT(' ERROR: Specified message was not found.')
1040	FORMAT('+Message number: ',I3).
1050	FORMAT(' Description: ',A53)A
1060	FORMAT(' From: ',A12,' Date: ',A11,' Expires: ',A20,/) 
1065	FORMAT(' From: ',A12,' Date: ',A11,' ',A,/)
1070	FORMAT(1X,/,' Press RETURN for more...',/)o

2000	FORMAT(A)
2010	FORMAT(1X,A)A
2020	FORMAT('+',A)

	END



	SUBROUTINE SEARCH(READ_COUNT)
C	
C  SUBROUTINE SEARCH
Ca
C  FUNCTION: Search for bulletin with specified string
Ct
	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLDIR.INC'

	COMMON/INPUT_BULL/INPUT
	CHARACTER*80 INPUT.

	CHARACTER*132 SEARCH_STRING,SAVE_STRING
	DATA SEARCH_STRING /' '/, SEARCH_LEN /1/R

	COMMON /POINT/ BULL_POINT

	CALL DISABLE_CTRL

	SAVE_STRING = SEARCH_STRING
	SAVE_LEN = SEARCH_LEN

	IER = CLI$GET_VALUE('SEARCH_STRING',SEARCH_STRING,SEARCH_LEN)
	S
	IF (.NOT.IER) THEN 
	   SEARCH_STRING = SAVE_STRINGT
	   SEARCH_LEN = SAVE_LENU
	END IFI

	CALL STR$UPCASE(SEARCH_STRING,SEARCH_STRING)	! Make upper caseT

	CALL OPEN_FILE_SHARED(2)

	CALL READDIR(0,IER)

	IF (BULL_POINT+1.GT.NBULL) THEN
	   WRITE (6,'('' ERROR: No more messages.'')')I
	   CALL CLOSE_FILE(2)
	   CALL ENABLE_CTRL
	   RETURN
	END IFN

	CALL OPEN_FILE_SHARED(1)s

	DO BULL_SEARCH = BULL_POINT+1, NBULL'
	   CALL READDIR(BULL_SEARCH,IER)	! Get bulletin directory entry
	   IF (IER.EQ.BULL_SEARCH+1) THEN
	      LEN = 81	
	      DO J=BLOCK,BLOCK+LENGTH-1
	         DO WHILE (LEN.GT.0) 
	            CALL GET_BULL(J,INPUT,LEN) 
	            CALL STR$UPCASE(INPUT,INPUT)	! Make upper case
		    IF (INDEX(INPUT,SEARCH_STRING(1:SEARCH_LEN)).GT.0) THENT
		       CALL CLOSE_FILE(1)d
		       CALL CLOSE_FILE(2)e
		       CALL ENABLE_CTRLs
		       BULL_POINT = BULL_SEARCH - 1
	               CALL READ(READ_COUNT,BULL_POINT+1) ! Read next bulletin	
		       RETURNh
		    END IF
	         END DO
		 LEN = 80E
	      END DOL
	   END IF
	END DO 

	CALL CLOSE_FILE(1)			! End of bulletin file readA
	CALL CLOSE_FILE(2)L

	CALL ENABLE_CTRLE

	WRITE (6,'('' No messages found with given search string.'')')D

	RETURNd
	END





	SUBROUTINE UPDATE
CI
C  SUBROUTINE UPDATE
C 
C  FUNCTION:  Searches for bulletins that have expired and deletes them.
CP
C  NOTE:  Assumes directory file is already opened.O
CL
	IMPLICIT INTEGER (A - Z) 
	CHARACTER*107 DIRLINE

	INCLUDE 'BULLDIR.INC'

	CHARACTER*11 TEMP_DATE,TEMP_EXDATER
	CHARACTER*8 TEMP_TIME,TEMP_EXTIME

	TEMP_EXDATE = '5-NOV-2000'  ! If a bulletin gets deleted, and there are
	TEMP_EXTIME = '00:00:00'    ! are no more bulletins, this is the value 
				    ! assigned to the latest expiration date

	TEMP_DATE = '5-NOV-1956' 	! Storage for computing newest 
	TEMP_TIME = '00:00:00'		! bulletin date if deletion occurs 

	CALL OPEN_FILE(1)			! Open both bulletin files 

	BULL_ENTRY = 1				! Init bulletin pointer
	UPDATE_DONE = 0			! Flag showing bull has been deletede

	DO WHILE (1)W
	   CALL READDIR(BULL_ENTRY,IER)		! Get next directory entry
	   IF (IER.EQ.BULL_ENTRY) GO TO 100	! ERROR: Not foundA
	   IF (SYSTEM.LE.1.OR.(SHUTDOWN.EQ.0	! If not permanent, or time	
     &	     .AND.(SYSTEM.AND.4).EQ.4)) THEN	! to delete shutdowns?
	    IF ((SYSTEM.AND.4).EQ.4) THEN	! Shutdown bulletin?'
	       DIFF = 0				! If so, delete it
	    ELSEE
	       DIFF = COMPARE_DATE(EXDATE,' ')	! Has expiration date passed? 
	       IF (DIFF.EQ.0) DIFF = COMPARE_TIME(EXTIME,' ')
	    END IF,
	    IF (DIFF.LE.0) THEN			! If so then delete bulletinN
	      CALL DELETE_ENTRY(BULL_ENTRY)	! Delete bulletin entry
	      IF (UPDATE_DONE.EQ.0) THEN	! If this is first deleted file 
	         UPDATE_DONE = BULL_ENTRY	! store it to use for reorderingE
	      END IF				! directory file.
	    ELSE IF (SYSTEM.LE.1) THEN		! Expiration date hasn't passed
		! If a bulletin is deleted, we'll have to update the latest 
		! expiration date. The following does that.P
	      DIFF = COMPARE_DATE(EXDATE,TEMP_EXDATE)
	      IF (DIFF.LT.0.OR.(DIFF.EQ.0.AND. 
     &		COMPARE_TIME(EXTIME,TEMP_EXTIME).LT.0)) THEN
	         TEMP_EXDATE = EXDATE		! If this is the latest expn
	         TEMP_EXTIME = EXTIME		! date seen so far, save it.
	      END IFN
	      TEMP_DATE = DATE			! Keep date so when we quitN
	      TEMP_TIME = TIME			! search, we'll have the
	    END IF				! latest bulletin datea
	   END IF
	   BULL_ENTRY = BULL_ENTRY + 1
	END DO 

100	IF (UPDATE_DONE.GT.0) THEN		! Reorder directory file
	   CALL CLEANUP_DIRFILE(UPDATE_DONE)	! due to deleted entries
	END IFO

	DATE = NEWEST_DATEt
	TIME = NEWEST_TIMEt
	NEW_SHUTDOWN = SHUTDOWN
	CALL READDIR(0,IER)
	SHUTDOWN = NEW_SHUTDOWN
	NEWEST_EXDATE = TEMP_EXDATE
	NEWEST_EXTIME = TEMP_EXTIME
	NEWEST_DATE = TEMP_DATE
	NEWEST_TIME = TEMP_TIME
	CALL WRITEDIR(0,IER)L
	CALL CLOSE_FILE(1)0
CH
C  If newest message date has been changed, must change it in BULLUSER.DAT
C  and also see if it affects notification of new messages to usersd
C	
	IF (TEMP_DATE.NE.DATE.OR.TEMP_TIME.NE.TIME) THENu
	   CALL UPDATE_LOGIN(.FALSE.)
	END IF 

	RETURNa

1000	FORMAT(A11,A11,A8,A4,A4)T
1020	FORMAT(A107)E

	END



	SUBROUTINE UPDATE_READ(NEW_BULL)N
C(
C  SUBROUTINE UPDATE_READc
Ci
C  FUNCTION:
C	Store the latest date that user has used the BULLETIN facility.s
C	If new bulletins have been added, alert user of the fact.9
C	

	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLUSER.INC'c

	INCLUDE 'BULLDIR.INC'

	INCLUDE '($PRVDEF)'

	CHARACTER TODAY*23F

C 
C  Update user's latest read time in his entry in BULLUSER.DAT.e
Cd

	NEW_BULL = .FALSE. 

	CALL OPEN_FILE_SHARED(4)		! Get BULLUSER.DAT file

	DO WHILE (REC_LOCK(IER))=
	   READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER)
     &		TEMP_USER,NEWEST_DATE,NEWEST_TIME,	! Get newest bulletin
     &		BBOARD_DATE,BBOARD_TIME,SET_FLAG,NEW_FLAG,NOTIFY_FLAGE
	END DO

	IF (IER.NE.0) THEN			! If header not present, exit
	   CALL CLOSE_FILE(4)
	   RETURN
	ELSE IF (NEW_FLAG(1).EQ.-1.AND.NEW_FLAG(2).EQ.-1) THENE
	   SET_FLAG(1) = 1			! If header present, but noT
	   SET_FLAG(2) = 0			! SET_FLAG and NOTIFY_FLAG
	   NOTIFY_FLAG(1) = 0			! information, write defaultx
	   NOTIFY_FLAG(2) = 0			! flags._
	   NEW_FLAG(1) = PRV$M_OPER.OR.PRV$M_CMKRNL.OR.PRV$M_SETPRV
	   NEW_FLAG(2) = 0T
	   REWRITE (4,FMT=USER_FMT)
     &		TEMP_USER,NEWEST_DATE,NEWEST_TIME,BBOARD_DATE,
     &		BBOARD_TIME,SET_FLAG,NEW_FLAG,NOTIFY_FLAG 
	END IF 

	CALL SYS$ASCTIM(,TODAY,,)		! Get today's time

	DO WHILE (REC_LOCK(IER1))
	   READ (4,FMT=USER_FMT,KEY=USERNAME,IOSTAT=IER1) USERNAME,
     &	    LOGIN_DATE,LOGIN_TIME,READ_DATE,READ_TIME,SET_FLAG,NEW_FLAG
     &      ,NOTIFY_FLAG			! Find user's info
	END DOC

	IF (IER1.EQ.0) THEN			! If entry found, update it
	   DIFF = COMPARE_DATE(READ_DATE,NEWEST_DATE)
	   IF (DIFF.EQ.0) DIFF = COMPARE_TIME(READ_TIME,NEWEST_TIME)B
	   IF (DIFF.LE.0) NEW_BULL = .TRUE.	! If new bull set flagW
Ce
C  No need to update read time/date if no new bulletins and no READNEW set,
C  unless new bulletin is in general folder.
CD
	   IF ( ((NEW_FLAG(1).AND.SET_FLAG(1)).OR.e
     &		 (NEW_FLAG(2).AND.SET_FLAG(2))).NE.0.OR.NEW_BULL) THEN
	      REWRITE (4,FMT=USER_FMT) USERNAME,LOGIN_DATE,LOGIN_TIME,T
     &		TODAY(1:11),TODAY(13:20),SET_FLAG,NEW_FLAG,NOTIFY_FLAG
	   END IF
	ELSE					! If no entry create a new entry
	   NEW_BULL = .TRUE. 
	   WRITE (4,FMT=USER_FMT) USERNAME,TODAY(1:11),TODAY(13:20),	
     &	    TODAY(1:11),TODAY(13:20),SET_FLAG,'FFFFFFFF'X,'FFFFFFFF'X,t
     &	    NOTIFY_FLAG
	END IFX

	CALL CLOSE_FILE(4)			! All finished with BULLUSER

	RETURN					! to go home... 

	END




	SUBROUTINE FIND_NEWEST_BULL
CT
C  SUBROUTINE FIND_NEWEST_BULL
CT
C	If new bulletins have been added, alert user of the fact and
C	set the next bulletin to be read to the first new bulletin.F
C 
C  OUTPUTS:S
C	BULL_POINT  -  If -1, no new bulletins to read, else there are.
CS

	IMPLICIT INTEGER (A - Z).

	COMMON /POINT/ BULL_POINT

	INCLUDE 'BULLUSER.INC'=

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'A
C=
C  Now see if bulletins have been added since the user's previous 
C  read time.  If they have, then search for the first new bulletin.
C  Ignore new bulletins that are owned by the user or system notices
C  that have not been added since the user has logged in.
C 
	BULL_POINT = -1				! Init bulletin pointerN
CG
C  Following stores a "possible" new bulletin.  That is, the user hasR
C  READNEW set, but ignored reading the bulletins.  The user then enters
C  BULLETIN, and if new bulletins are added after logging in, we want to
C  point to that bulletin.  However, if there were none added since then,(
C  we want to point to the first unread one.  Thus, the first new unread
C  bulletin is stored in BULL_POSSIBLE, and the search continues for
C  new bulletins since logging in.
Cr
	BULL_POSSIBLE = -1'

	CALL OPEN_FILE_SHARED(2)		! Yep, so get directory fileN
	CALL READDIR(0,IER)			! Get # bulletins from header
	IF (IER.EQ.1) THEN			! If header present 
	   DO ICOUNT=1,NBULL			! Get each bulletin to compare
	      CALL READDIR(ICOUNT,IER)		! its date with last read dateA
	      IF (FROM.NE.USERNAME) THEN	! Ignore bull if owner is user
	         DIFF = COMPARE_DATE(READ_DATE,DATE)c
	         IF (DIFF.EQ.0) DIFF = COMPARE_TIME(READ_TIME,TIME)
	         IF (DIFF.LE.0) THEN		! If new bull or new user
		    IF (SYSTEM) THEN		! If system bulletin
		       DIFF = COMPARE_DATE(LOGIN_DATE,DATE)$
		       IF (DIFF.EQ.0) DIFF = COMPARE_TIME(LOGIN_TIME,TIME)
		       IF (DIFF.LE.0) THEN	! If system bull, make it
		          BULL_POINT = ICOUNT - 1 ! the first new bull onlyU
			  GO TO 100		! if added since user logged inf
		       END IF			! else he's read it already.
		    ELSE
		       IF ((FOLDER_NUMBER.LE.31.AND.
     &				BTEST(SET_FLAG(1),FOLDER_NUMBER)).OR.a
     &			   (FOLDER_NUMBER.GT.31.AND.r
     &				BTEST(SET_FLAG(2),FOLDER_NUMBER-32))) THEN
			IF (BULL_POSSIBLE.EQ.-1) BULL_POSSIBLE = ICOUNT - 1
		        DIFF = COMPARE_DATE(LOGIN_DATE,READ_DATE)I
		        IF (DIFF.EQ.0)
     &			 DIFF = COMPARE_TIME(LOGIN_TIME,READ_TIME)O
			IF (DIFF.GT.0) THEN
		         DIFF = COMPARE_DATE(LOGIN_DATE,DATE)/
		         IF(DIFF.EQ.0) DIFF=COMPARE_TIME(LOGIN_TIME,TIME)A
			END IF1
		       END IF
		       IF (DIFF.LE.0) THEN
		        BULL_POINT = ICOUNT - 1  ! If not system bull then
		        GO TO 100		! make it the new bullO
		       END IFn
		    END IF
		 END IF	
	      END IFa
	   END DO
	END IFN

	BULL_POINT = BULL_POSSIBLEI

100	CALL CLOSE_FILE(2)			! Its time for this program

	RETURNI
	END



	SUBROUTINE GET_EXPIRED(INPUT,IER)

	IMPLICIT INTEGER (A-Z)S

	CHARACTER*20 INPUTE
	CHARACTER*23 TODAYN

	DIMENSION EXTIME(2),NOW(2)E

	IER = SYS$ASCTIM(,TODAY,,)		! Get today's date,

5	WRITE(6,1030) TODAY			! Prompt for expiration date
	CALL GET_LINE(INPUT,LEN)		! Get input line(

	IF (LEN.LE.0) THEN
	   IER = 0
	   RETURN
	END IFT

	INPUT = INPUT(1:LEN)			! Change trailing zeros 2 spaces

	IF (INDEX(INPUT,'-').EQ.0.AND.INDEX(INPUT,':').GT.0.AND.
     &		INDEX(INPUT(1:LEN),' ').EQ.0) THEN
	   INPUT = TODAY(1:INDEX(TODAY(2:),' ')+1)//INPUT
	END IF

	CALL STR$UPCASE(INPUT,INPUT)		! Convert to upper case
	IER = SYS$BINTIM(INPUT,EXTIME)F
	IF (IER.NE.1) THEN			! If not able to do so
    	   WRITE(6,1040)			! tell user is wrong
	   GO TO 5R
	END IF 
	IER = SYS$ASCTIM(TIMLEN,INPUT,EXTIME,)I
	IF (TIMLEN.EQ.16) THEN 
	   CALL SYS$GETTIM(NOW)
	   CALL LIB$SUBX(NOW,EXTIME,EXTIME)
	   IER = SYS$ASCTIM(TIMLEN,INPUT,EXTIME,)
	END IF 

	IF (INPUT(2:2).EQ.'-') INPUT = '0'//INPUT
	IER = COMPARE_DATE(INPUT(1:11),TODAY(1:11)) ! Compare date with today's
	IF (IER.EQ.0) IER = COMPARE_TIME(INPUT(13:20),TODAY(13:20))
	IF (IER.LE.0) THEN			! If expiration date not futureL
	   WRITE(6,1045)			! tell user 
	   GO TO 5				! and re-request date
	END IFT

	IER = 1

	RETURN=

1030	FORMAT (' It is ',A23,
     &'. Specify when the message should expire:',/,1x,E
     &'Enter absolute time: [dd-mmm-yyyy] hh:mm:ss ', 
     &'or delta time: dddd hh:mm:ss')h
1040	FORMAT (' ERROR: Invalid date format specified.')
1045	FORMAT (' ERROR: Specified time has already passed.')

	END

