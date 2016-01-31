From:	HENRY::IN%"MRL%PFCVAX%xx.lcs.mit.edu%csnet-relay.CSNET%relay.cs.net@RCA.COM" 19-JUN-1987 21:03
To:	"SYSTEM%UK.AC.SOTON.ESP.V1" <SYSTEM%UK.AC.SOTON.ESP.V1%ucl-cs.arpa@xx>, 
Subj:	BULLETIN1.FOR

C
C  BULLETIN1.FOR, Version 6/3/87
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

	CHARACTER*64 MAIL_SUBJECT

	INCLUDE 'BULLDIR.INC'

	EXTERNAL CLI$_ABSENT

	IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been read
	   WRITE(6,'('' ERROR: You have not read any message.'')')
	   RETURN			! And return
	END IF

	MAIL_SUBJECT = DESCRIP
	IF (CLI$PRESENT('SUBJECT')) THEN
	   IER = CLI$GET_VALUE('SUBJECT',MAIL_SUBJECT,LEN_D)
	   IF (LEN_D.GT.LEN(MAIL_SUBJECT)-2) THEN
	      WRITE(6,'('' ERROR: Subject limit is 64 characters.'')')
	      RETURN
	   END IF
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

	LEN_I = 81
	DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file
	   DO WHILE (LEN_I.GT.0)
	      CALL GET_BULL(I,INPUT,LEN_I)
	      IF (LEN_I.LT.0) THEN
		 GO TO 90
	      ELSE IF (LEN_I.GT.0) THEN
	         WRITE (3,'(A)') INPUT(1:LEN_I)
	      END IF
	   END DO
	   LEN_I = 80
	END DO

90	CLOSE (UNIT=3)			! Message copy completed

	CALL CLOSE_FILE(1)

	LEN_D = TRIM(MAIL_SUBJECT)
	IF (LEN_D.EQ.0) THEN
	   MAIL_SUBJECT = 'BULLETIN message.'
	   LEN_D = TRIM(MAIL_SUBJECT)
	END IF

	IF (MAIL_SUBJECT(1:1).NE.'"') THEN
	   MAIL_SUBJECT = '"'//MAIL_SUBJECT(1:LEN_D)
	   LEN_D = LEN_D + 1
	END IF

	IF (MAIL_SUBJECT(LEN_D:LEN_D).NE.'"') THEN
	   MAIL_SUBJECT = MAIL_SUBJECT(1:LEN_D)//'"'
	   LEN_D = LEN_D + 1
	END IF

	IER = CLI$GET_VALUE('RECIPIENTS',BULL_PARAMETER,LEN_P)
	   
	CALL LIB$SPAWN('$MAIL SYS$LOGIN:BULL.SCR '//BULL_PARAMETER(1:LEN_P)
     &	   //'/SUBJECT='//MAIL_SUBJECT(1:LEN_D),,,,,,STATUS)

	CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR')

	RETURN

	END



	SUBROUTINE MODIFY_FOLDER
C
C  SUBROUTINE MODIFY_FOLDER
C
C  FUNCTION: Modifies a folder's information.
C
	IMPLICIT INTEGER (A - Z)

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFILES.INC'

	INCLUDE '($SSDEF)'

	IF (FOLDER_NUMBER.EQ.0) THEN
	   WRITE (6,'('' ERROR: Cannot modify GENERAL folder.'')')
	   RETURN
	ELSE IF (FOLDER_OWNER.NE.USERNAME.AND..NOT.SETPRV_PRIV()) THEN
	   WRITE (6,'('' ERROR: No privileges to modify folder.'')')
	   RETURN
	END IF

	IF (CLI$PRESENT('NAME')) THEN
	   CALL CLI$GET_VALUE('NAME',FOLDER1,LEN_P)
	   IF (LEN_P.GT.25) THEN
	      WRITE (6,'('' ERROR: Folder name cannot be larger 
     &				than 25 characters.'')')
	      RETURN
	   END IF
	ELSE
	   FOLDER1 = FOLDER
	END IF

	IF (CLI$PRESENT('DESCRIPTION')) THEN
	   WRITE (6,'('' Enter one line description of folder.'')')
	   LEN_P = 81
	   DO WHILE (LEN_P.GT.80)
	    CALL GET_LINE(FOLDER1_DESCRIP,LEN_P)	! Get input line
	    IF (LEN_P.LE.0) THEN
	     WRITE (6,'('' ERROR: Folder modification aborted.'')')
	    ELSE IF (LEN_P.GT.80) THEN			! If too many characters
	     WRITE (6,'('' ERROR: Description must be < 80 characters.'')')
	    ELSE
	       FOLDER1_DESCRIP = FOLDER1_DESCRIP(1:LEN_P) ! End fill with spaces
	    END IF
	   END DO
	ELSE
	   FOLDER1_DESCRIP = FOLDER_DESCRIP
	END IF

	IF (CLI$PRESENT('OWNER')) THEN
	   CALL CLI$GET_VALUE('OWNER',FOLDER1_OWNER,LEN_P)
	   IF (LEN_P.GT.LEN(FOLDER1_OWNER)) THEN
	      WRITE (6,'('' ERROR: Folder owner name too long.'')')
	      RETURN
	   ELSE IF (.NOT.SETPRV_PRIV()) THEN
	      WRITE (6,'('' ERROR: No privileges to modify folder owner.'')')
	   ELSE
	      FOLDER1_OWNER = FOLDER1_OWNER(1:LEN_P)
	   END IF
	ELSE
	   FOLDER1_OWNER = FOLDER_OWNER
	END IF

	CALL OPEN_FILE(7)		! Open folder file

	IF (CLI$PRESENT('NAME')) THEN
	   READ (7,FMT=FOLDER_FMT,IOSTAT=IER,KEY=FOLDER1,KEYID=0)
					! See if folder exists
	   IF (IER.EQ.0) THEN
	      WRITE (6,'('' ERROR: Folder name already exists.'')')
	      CALL CLOSE_FILE(7)
	      RETURN
	   END IF
	END IF

	READ (7,FMT=FOLDER_FMT,IOSTAT=IER,KEY=FOLDER,KEYID=0)
     &			FOLDER,FOLDER_NUMBER,FOLDER_OWNER,
     &			FOLDER_DESCRIP,FOLDER_BBOARD,FOLDER_BBEXPIRE

	IF (IER.EQ.0) THEN
	   LEN_F = TRIM(FOLDER_DIRECTORY)
	   IER = LIB$RENAME_FILE(FOLDER_DIRECTORY(:LEN_F)//
     &		FOLDER(:TRIM(FOLDER))//'.*',FOLDER_DIRECTORY(:LEN_F)//
     &		FOLDER1(:TRIM(FOLDER1))//'.*')
	   IF (IER) THEN
	      IER = 0
	      FOLDER_FILE = FOLDER_DIRECTORY(:LEN_F)//FOLDER1
	   END IF
	END IF

	IF (IER.EQ.0) THEN
	   IF (CLI$PRESENT('OWNER')) THEN
	      CALL CHKACL
     &		(FOLDER_FILE(1:TRIM(FOLDER_FILE))//'.BULLFIL',IER)
	      IF (IER.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL)) THEN
	         CALL ADD_ACL(FOLDER1_OWNER,'R+W+C',IER)
	         CALL DEL_ACL(FOLDER_OWNER,'R+W+C',IER)
	      END IF
	   END IF
	   FOLDER = FOLDER1
	   FOLDER_OWNER = FOLDER1_OWNER
	   FOLDER_DESCRIP = FOLDER1_DESCRIP
	   DELETE (7)
	   WRITE (7,FMT=FOLDER_FMT,IOSTAT=IER) 
     &			FOLDER,FOLDER_NUMBER,FOLDER_OWNER,
     &			FOLDER_DESCRIP,FOLDER_BBOARD,FOLDER_BBEXPIRE
	   IF (IER.EQ.0) WRITE (6,'('' Folder successfully modified.'')')
	END IF

	IF (IER.NE.0) THEN
	   WRITE (6,'('' ERROR: Folder modification aborted.'')')
	END IF

	CALL CLOSE_FILE(7)

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

	CHARACTER INPUT*80,SAVE_USERNAME*12

	CHARACTER*116 BULLDIR_COM_SAVE

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

	CALL LIB$MOVC3(116,%REF(BULLDIR_COM),%REF(BULLDIR_COM_SAVE))
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

	SAVE_USERNAME = USERNAME
	IF (CLI$PRESENT('ORIGINAL')) THEN
	 IF (SETPRV_PRIV()) THEN
	  USERNAME = FROM
	 ELSE
	  WRITE (6,
     &	  '('' ERROR: You have no privileges to keep original owner.'')')
	 END IF
	END IF

	FOLDER_NUMBER = -1	! Use FOLDER as key rather than FOLDER_NUMBER
	CALL SELECT_FOLDER(.FALSE.,IER)

	IF (.NOT.IER.OR.READ_ONLY) THEN
	   WRITE (6,'('' ERROR: Cannot access specified folder.'')')
	   CLOSE (UNIT=3)
	   FOLDER_NUMBER = SAVE_FOLDER_NUMBER
	   USERNAME = SAVE_USERNAME
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

	CALL LIB$MOVC3(116,%REF(BULLDIR_COM_SAVE),%REF(BULLDIR_COM))

	SYSTEM = IBCLR(SYSTEM,0)		! Remove system bit
	IF (BTEST(SYSTEM,2)) THEN		! Shutdown message?
	   SYSTEM = IBCLR(SYSTEM,2)		! Remove shutdown bit
	   CALL GET_EXDATE(EXDATE,FOLDER_BBEXPIRE)
	ELSE IF (BTEST(SYSTEM,1).AND.FOLDER_NUMBER.EQ.0.AND.
     &		 .NOT.SETPRV_PRIV()) THEN	! Permanent?
	  WRITE (6,'('' ERROR: No privileges to add permanent message.'')')
	  WRITE (6,'('' Expiration will be '',I,'' days.'')') FOLDER_BBEXPIRE
	END IF

	FROM = USERNAME				! Specify owner
	CALL ADD_ENTRY				! Add the new directory entry

	CALL CLOSE_FILE(2)			! Totally finished with add

	CLOSE (UNIT=3)			! Close the input file

	WRITE (6,'('' Message has been copied to folder '',A)')
     &		FOLDER(1:TRIM(FOLDER))//'.'

	USERNAME = SAVE_USERNAME

	FOLDER_NUMBER = SAVE_FOLDER_NUMBER
	CALL SELECT_FOLDER(.FALSE.,IER)

	BULL_POINT = SAVE_BULL_POINT

	IF (DELETE_ORIGINAL) CALL DELETE

	RETURN

	END




	SUBROUTINE PRINT
C
C  SUBROUTINE PRINT
C
C  FUNCTION:  Print header to queue.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($SJCDEF)'

	CHARACTER*32 QUEUE

	INTEGER*2 FILE_ID(14)
	INTEGER*2 IOSB(4)
	EQUIVALENCE (IOSB(1),JBC_ERROR)

	CHARACTER*80 INPUT

	COMMON /POINT/ BULL_POINT

	INCLUDE 'BULLDIR.INC'

	IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been read
	   WRITE(6,1010)		! Write error
	   RETURN			! And return
	END IF

	CALL OPEN_FILE_SHARED(2)

	CALL READDIR(BULL_POINT,IER)	! Get info for specified bulletin

	IF (IER.NE.BULL_POINT+1) THEN	! Was bulletin found?
	   WRITE(6,1030)
	   CALL CLOSE_FILE(2)		! If not, then error out
	   RETURN
	END IF

	CALL CLOSE_FILE(2)

	CALL OPEN_FILE_SHARED(1)	! Open BULLETIN file

	IF (.NOT.SETPRV_PRIV()) THEN		! If no SETPRV, remove SYSPRV
	   CALL DISABLE_PRIVS			! privileges when trying to
	END IF					! create new file.

	OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.LIS',ERR=900,IOSTAT=IER,
     &		STATUS='NEW',CARRIAGECONTROL='LIST')

	CALL ENABLE_PRIVS

	IF (CLI$PRESENT('HEADER')) THEN		! Printout header?
	   WRITE(3,1050) DESCRIP		! Output bulletin header info
	   WRITE(3,1060) FROM,DATE
	END IF

	LEN =81
	DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file
	   DO WHILE (LEN.GT.0)
	      CALL GET_BULL(I,INPUT,LEN)
	      IF (LEN.GT.0) WRITE(3,'(A)') INPUT(1:TRIM(INPUT))
	   END DO
	   LEN = 80
	END DO

	CLOSE (UNIT=3)			! Bulletin copy completed

	CALL CLOSE_FILE(1)

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(18,SJC$_FILE_SPECIFICATION,
     &		%LOC('SYS$LOGIN:BULL.LIS'))

	IER = CLI$GET_VALUE('QUEUE',QUEUE,LEN) 		! Get queue name
	IF (LEN.EQ.0) THEN
	   QUEUE = 'SYS$PRINT'
	   LEN = 9
	END IF

	CALL ADD_2_ITMLST(LEN,SJC$_QUEUE,%LOC(QUEUE))
	CALL ADD_2_ITMLST(0,SJC$_DELETE_FILE,0)

	IF (CLI$PRESENT('NOTIFY')) THEN
	   CALL ADD_2_ITMLST(0,SJC$_NOTIFY,0)
	END IF

	IF (.NOT.SETPRV_PRIV()) THEN		! If no SETPRV, remove SYSPRV
	   CALL DISABLE_PRIVS			! privileges when trying to
	END IF					! create new file.

	CALL END_ITMLST(SJC_ITMLST)
	
	IER=SYS$SNDJBCW(,%VAL(SJC$_ENTER_FILE),,%VAL(SJC_ITMLST),IOSB,,)
	IF (IER.AND.(.NOT.JBC_ERROR)) THEN
	   CALL SYS_GETMSG(JBC_ERROR)
	   IER = LIB$DELETE_FILE('SYS$LOGIN:BULL.LIS;')
	ELSE IF (.NOT.IER) THEN
	   CALL SYS_GETMSG(IER)
	   IER = LIB$DELETE_FILE('SYS$LOGIN:BULL.LIS;')
	END IF

	CALL ENABLE_PRIVS			! Reset SYSPRV privileges

	RETURN

900	CALL ERRSNS(IDUMMY,IER)
	CALL ENABLE_PRIVS			! Reset SYSPRV privileges
	CLOSE (UNIT=3,STATUS='DELETE')
	CALL CLOSE_FILE(1)
	WRITE(6,1000)
	CALL SYS_GETMSG(IER)

	RETURN

1000	FORMAT(' ERROR: Unable to open temporary file
     & SYS$LOGIN:BULL.LIS for printing.')
1010	FORMAT(' ERROR: You have not read any message.')
1030	FORMAT(' ERROR: Specified message was not found.')
1040	FORMAT(' Message ',I4,' written to ',A)
1050	FORMAT('Description: ',A53)
1060	FORMAT('From: ',A12,' Date: ',A11,/)

	END




	SUBROUTINE READ(READ_COUNT,BULL_READ)
C
C  SUBROUTINE READ
C
C  FUNCTION: Reads a specified bulletin.
C
C  PARAMETER:
C	READ_COUNT - Variable to store the record in the message file
C		that READ will read from.  Must be set to 0 to indicate
C		that it is the first read of the message.  If -1,
C		READ will search for the last message in the message file
C		and read that one.  If -2, just display header information.
C	BULL_READ - Message number to be read.
C
	IMPLICIT INTEGER (A - Z)

	COMMON /POINT/ BULL_POINT

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'

	COMMON/INPUT_BULL/INPUT
	CHARACTER*80 INPUT

	COMMON /READIT/ READIT

	COMMON /PAGE/ PAGE_LENGTH

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD

	DATA SCRATCH_B1/0/

	CHARACTER TODAY*11,DATETIME*23

	LOGICAL SINCE,PAGE

	CALL LIB$ERASE_PAGE(1,1)		! Clear screen
	END = 0					! Nothing outputted on screen

	IF (READ_COUNT.GT.0) GO TO 100		! Skip init steps if this is
						! not first page of bulletin

	SINCE = .FALSE.
	PAGE = .TRUE.
	IF (INCMD(1:4).EQ.'READ') THEN		! If READ command...
	 IF (.NOT.CLI$PRESENT('PAGE')) PAGE = .FALSE.
	 IF (CLI$PRESENT('SINCE')) THEN		! was /SINCE specified?
	   IER = CLI$GET_VALUE('SINCE',DATETIME)
	   IF (DATETIME.EQ.'TODAY') THEN
	      IER = SYS$ASCTIM(,TODAY,,)		! Get today's date
	      DATETIME = TODAY//' 00:00:00.0'
	   END IF
	   CALL OPEN_FILE_SHARED(2)
	   TEMP_READ = 0
	   IER = 1
	   DO WHILE (IER.EQ.TEMP_READ+1)
	      TEMP_READ = TEMP_READ + 1
	      CALL READDIR(TEMP_READ,IER)
	      IF (IER.NE.TEMP_READ+1) THEN
		 WRITE (6,'('' No messages found past specified date.'')')
		 CALL CLOSE(2)
		 RETURN
	      ELSE
	         DIFF = COMPARE_DATE(DATETIME(1:11),DATE)  ! Compare expiration
	         IF (DIFF.EQ.0) DIFF = COMPARE_TIME(DATETIME(13:20),TIME)
	         IF (DIFF.LE.0) THEN
		    BULL_READ = TEMP_READ
		    IER = IER + 1
		 END IF
	      END IF
	   END DO
	   IER = BULL_READ + 1
	   SINCE = .TRUE.
	 END IF
	END IF

	IF (.NOT.SINCE) THEN
	 IF (BULL_READ.GT.0) THEN		! Valid bulletin number?
	   CALL OPEN_FILE_SHARED(2)
	   CALL READDIR(BULL_READ,IER)		! Get bulletin directory entry
	   IF (READ_COUNT.EQ.-1.AND.IER.NE.BULL_READ+1) THEN
	      READ_COUNT = 0
	      CALL READDIR(0,IER)
	      IF (NBULL.GT.0) THEN
	         BULL_READ = NBULL
	         CALL READDIR(BULL_READ,IER)
	      ELSE
		 IER = 0
	      END IF
	   END IF
	   CALL CLOSE_FILE(2)
	 ELSE
	   IER = 0
	 END IF
	END IF

	IF (IER.NE.BULL_READ+1) THEN		! Was bulletin found?
	   WRITE(6,1030)			! If not, then error out
	   RETURN
	END IF

	BULL_POINT = BULL_READ			! Update bulletin counter

	FLEN = TRIM(FOLDER)
	WRITE(6,1040) BULL_POINT,FOLDER(:FLEN)	! Output bulletin header info
	WRITE(6,1050) DESCRIP
	IF ((SYSTEM.AND.4).EQ.4) THEN		! Is entry shutdown bulletin?
	   WRITE(6,1065) FROM,DATE,'Expires on shutdown'
	ELSE IF ((SYSTEM.AND.2).EQ.2) THEN	! Is entry permanent bulletin?
	   WRITE(6,1065) FROM,DATE,'Permanent message'
	ELSE
	   WRITE(6,1060) FROM,DATE,EXDATE//' '//EXTIME
	END IF

C
C  Each page of the bulletin is buffered into temporary memory storage before
C  being outputted to the terminal.  This is to be able to quickly close the
C  bulletin file, and to avoid the possibility of the user holding the screen,
C  and thus causing the bulletin file to stay open.  The temporary memory
C  is structured as a linked-list queue, where SCRATCH_B1 points to the header
C  of the queue.  See BULLSUBS.FOR for more description of the queue.
C

	IF (SCRATCH_B1.NE.0) THEN		! Is queue empty?
	   SCRATCH_B = SCRATCH_B1		! No, set queue pointer to head
	ELSE					! Else if queue is empty
	   CALL INIT_QUEUE(SCRATCH_B,INPUT)
	   SCRATCH_B1 = SCRATCH_B		! Init header pointer
	END IF

	END = 4					! Outputted 4 lines to screen

	READ_ALREADY = 0			! Number of lines already read
						! from record.
	IF (READ_COUNT.EQ.-2) THEN		! Just output header first read
	   READ_COUNT = BLOCK
	   RETURN
	ELSE
	   READ_COUNT = BLOCK			! Init bulletin record counter
	END IF

100	SCRATCH_B = SCRATCH_B1			! Init queue pointer to header
	IF (READIT.GT.0) END = END - 2		! /READ can output 2 more lines
	DISPLAY = 0
	IF (READ_COUNT.GT.BLOCK.AND.READIT.EQ.0) THEN ! If not 1st page of READ
	   WRITE(6,1040) BULL_POINT,FOLDER(:FLEN) ! Output bulletin header info
	   END = END + 1			  ! Increase display counter
	END IF
	CALL OPEN_FILE_SHARED(1)		! Get bulletin file
	MORE_LINES = .TRUE.
	READ_REC = READ_COUNT
	IF (READ_ALREADY.EQ.0) LEN = 81
	DO WHILE (MORE_LINES.AND.READ_REC.LE.BLOCK+LENGTH-1)
	   DO WHILE (LEN.GT.0.AND.MORE_LINES)
	      CALL GET_BULL(READ_REC,INPUT,LEN)
	      IF (LEN.LT.0) THEN		! Error, couldn't read record
		 READ_REC = BLOCK + LENGTH	! Fake end of reading file
		 MORE_LINES = .FALSE.
	      ELSE IF (LEN.GT.0) THEN
		 LEN_TEMP = LEN
		 CALL CONVERT_TABS(INPUT,LEN_TEMP)
	         CALL WRITE_QUEUE(%VAL(SCRATCH_B),SCRATCH_B,INPUT)
	         READ_ALREADY = 1
		 DISPLAY = DISPLAY + 1
		 IF ((DISPLAY.EQ.PAGE_LENGTH-END-4).AND.PAGE) THEN
		    MORE_LINES = .FALSE.
		 END IF
	      END IF
	   END DO
	   LEN = 80
	   IF (MORE_LINES) THEN
	      READ_REC = READ_REC + 1
	      READ_ALREADY = 0
	   END IF
	END DO

	CALL CLOSE_FILE(1)			! End of bulletin file read

C
C  Bulletin page is now in temporary memory, so output to terminal.
C  Note that if this is a /READ, the first line will have problems with
C  the usual FORMAT statement.  It will cause a blank line to be outputted
C  at the top of the screen.  This is because of the input QIO at the
C  end of the previous page.  The output gets confused and thinks it must
C  end the previous line.  To prevent that, the first line of a new page
C  in a /READ must use a different FORMAT statement to surpress the CR/LF.
C

	SCRATCH_B = SCRATCH_B1			! Reinit queue pointer to head
	DO I=1,DISPLAY				! Output page to terminal
	   CALL READ_QUEUE(%VAL(SCRATCH_B),SCRATCH_B,INPUT) ! Get queue record
	   IF (I.EQ.1.AND.READ_REC.NE.BLOCK.AND.READIT.GT.0) THEN
	      WRITE(6,2020) INPUT(1:TRIM(INPUT))	! (See above comments)
	   ELSE
	      WRITE(6,2010) INPUT(1:TRIM(INPUT))
	   END IF
	END DO

	READ_COUNT = READ_REC			! Update bull record counter

	IF (READ_REC.EQ.BLOCK+LENGTH) THEN	! Last block?
	   READ_COUNT = 0			! init bulletin record counter
	ELSE IF (READ_REC.EQ.BLOCK+LENGTH-1.AND..NOT.MORE_LINES) THEN
		! Possibly last block since end of page could be last line
	   CALL TEST_MORE_LINES(LEN)		! More lines to read?
	   IF (LEN.GT.0) THEN			! Yes, there are still more
	      IF (READIT.EQ.0) WRITE(6,1070)	! say there is more of bulletin
	   ELSE					! Yes, last line anyway
	      READ_COUNT = 0			! init bulletin record counter
	   END IF
	ELSE IF (READIT.EQ.0) THEN		! Else if this is not /READ
	   WRITE(6,1070)			! say there is more of bulletin
	END IF

	RETURN

1030	FORMAT(' ERROR: Specified message was not found.')
1040	FORMAT('+Message number: ',I4,<60-FLEN>X,A)
1050	FORMAT(' Description: ',A53)
1060	FORMAT(' From: ',A12,' Date: ',A11,' Expires: ',A20,/)
1065	FORMAT(' From: ',A12,' Date: ',A11,' ',A,/)
1070	FORMAT(1X,/,' Press RETURN for more...',/)

2000	FORMAT(A)
2010	FORMAT(1X,A)
2020	FORMAT('+',A)

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




	SUBROUTINE SET_BBOARD(BBOARD)
C
C  SUBROUTINE SET_BBOARD
C
C  FUNCTION: Set username for BBOARD for selected folder.
C
	IMPLICIT INTEGER (A-Z)

	PARAMETER UAF$V_DISACNT = 4

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFILES.INC'

	EXTERNAL CLI$_ABSENT

	CHARACTER EXPIRE*3,INPUT_BBOARD*12

	IF (TRIM(BBOARD_DIRECTORY).EQ.0) THEN
	 WRITE(6,'('' ERROR: System programmer has disabled BBOARD.'')')
	 RETURN
	END IF

	IF (FOLDER_OWNER.EQ.USERNAME.OR.SETPRV_PRIV()) THEN

	   CALL OPEN_FILE(7)		! Open folder file
	   READ (7,FMT=FOLDER_FMT,KEY=FOLDER,KEYID=0,IOSTAT=IER)
     &		FOLDER,FOLDER_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP
     &		,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB

	   IF (BBOARD) THEN
	      IER = CLI$GET_VALUE('BB_USERNAME',INPUT_BBOARD,INPUT_LEN)
	      IF (IER.NE.%LOC(CLI$_ABSENT)) THEN
		 CALL GET_UAF
     &		   (INPUT_BBOARD,USERB,GROUPB,ACCOUNTB,FLAGS,IER)
	         IF (IER.AND..NOT.BTEST(FLAGS,UAF$V_DISACNT)) THEN ! DISUSER?
	            WRITE (6,'
     &		    ('' ERROR: BBOARD account needs DISUSER flag set.'')')
		    IER = 0
		 END IF
		 IF (IER) THEN
	          READ (7,FMT=FOLDER_FMT,KEY='GENERAL',KEYID=0,IOSTAT=IER)
     &		   FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIP
     &		   ,FOLDER1_BBOARD,FOLDER1_BBEXPIRE
		  DO WHILE ((FOLDER1_BBOARD.NE.INPUT_BBOARD.OR.
     &		     FOLDER1_NUMBER.EQ.FOLDER_NUMBER).AND.IER.EQ.0)
	           READ (7,FMT=FOLDER_FMT,IOSTAT=IER)
     &		   FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIP
     &		   ,FOLDER1_BBOARD,FOLDER1_BBEXPIRE
	          END DO
		  IF (FOLDER1_BBOARD.EQ.INPUT_BBOARD.AND.
     &		      FOLDER1_NUMBER.NE.FOLDER_NUMBER) THEN
		   WRITE (6,'(
     &		    '' ERROR: Account used by other folder.'')')
		   CALL CLOSE_FILE(7)
		   RETURN
		  ELSE
	           READ (7,FMT=FOLDER_FMT,KEY=FOLDER,KEYID=0,IOSTAT=IER)
     &		    FOLDER,FOLDER_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP
     &		    ,FOLDER_BBOARD,FOLDER_BBEXPIRE
		   FOLDER_BBOARD = INPUT_BBOARD
		   IF (CLI$PRESENT('SPECIAL')) THEN	! SPECIAL specified?
		     USERB = IBSET(USERB,31)	! Set bit to show /SPECIAL
		     IF (CLI$PRESENT('VMSMAIL')) THEN
		        GROUPB = IBSET(GROUPB,31)   ! Set bit to show /VMSMAIL
		     END IF
		   END IF
		  END IF
		 ELSE
		  CALL CLOSE_FILE(7)
		  RETURN
		 END IF
	      ELSE IF (CLI$PRESENT('SPECIAL')) THEN
	       USERB = IBSET(0,31)		! Set top bit to show /SPECIAL
	       GROUPB = 0
	       DO I=1,LEN(FOLDER_BBOARD)
		  FOLDER_BBOARD(I:I) = ' '
	       END DO
	      ELSE IF (FOLDER_BBOARD.EQ.'NONE') THEN
	       WRITE (6,'('' ERROR: No BBOARD specified for folder.'')')
	      END IF

	      IER = CLI$GET_VALUE('EXPIRATION',EXPIRE,EX_LEN)
	      IF (IER.NE.%LOC(CLI$_ABSENT)) THEN
	         IF (EX_LEN.GT.3) EX_LEN = 3
	         READ (EXPIRE,'(I<EX_LEN>)') TEMP
		 IF (TEMP.GT.BBEXPIRE_LIMIT.AND..NOT.SETPRV_PRIV()) THEN
		    WRITE (6,'('' ERROR: Expiration cannot be > '',
     &			I3,'' days.'')') BBEXPIRE_LIMIT
		    CALL CLOSE_FILE(7)
		    RETURN
		 ELSE IF (TEMP.LE.0) THEN
		    WRITE (6,'('' ERROR: Expiration must be > 0.'')')
		    CALL CLOSE_FILE(7)
		    RETURN
		 ELSE
		    FOLDER_BBEXPIRE = TEMP
		 END IF
	      ELSE IF (.NOT.CLI$PRESENT('EXPIRATION')) THEN
		 FOLDER_BBEXPIRE = -1
	      END IF
	   ELSE
	      FOLDER_BBOARD = 'NONE'
	   END IF

	   REWRITE (7,FMT=FOLDER_FMT,IOSTAT=IER)
     &		FOLDER,FOLDER_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP
     &		,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB
	   CALL CLOSE_FILE(7)
	   WRITE (6,'('' BBOARD has been modified for folder.'')')
	ELSE
	   WRITE (6,'('' You are not authorized to modify BBOARD.'')')
	END IF

	RETURN
	END
