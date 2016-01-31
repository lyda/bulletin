From:	HENRY::IN%"MRL%PFCVAX%xx.lcs.mit.edu%csnet-relay.CSNET%relay.cs.net@RCA.COM" 19-JUN-1987 20:58
To:	"SYSTEM%UK.AC.SOTON.ESP.V1" <SYSTEM%UK.AC.SOTON.ESP.V1%nss.cs.ucl.ac.uk@xx>, 
Subj:	BULLETIN0.FOR

C
C  BULLETIN0.FOR, Version 5/18/87
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Programmer: Mark R. London
C
	SUBROUTINE DELETE
C
C  SUBROUTINE DELETE
C
C  FUNCTION:  Deletes a bulletin entry from the bulletin file.
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

	EXTERNAL CLI$_ABSENT

	CHARACTER ANSWER*1,REMOTE_USER*12,SUBJECT*53

	IF (CLI$PRESENT('NODES')) THEN	! Delete messages on DECNET node?
	   CALL DELETE_NODE		! Yes...
	   RETURN
	ELSE IF (DECNET_PROC) THEN	! Is this from remote node?
	   IER = CLI$GET_VALUE('USERNAME',REMOTE_USER)
	   IER = CLI$GET_VALUE('SUBJECT',SUBJECT,SLEN)
	   CALL OPEN_FILE(2)
	   BULL_DELETE = 0
	   IER = 1
	   DO WHILE (BULL_DELETE+1.EQ.IER)
	      BULL_DELETE = BULL_DELETE + 1
	      CALL READDIR(BULL_DELETE,IER)
	      CALL STR$UPCASE(DESCRIP,DESCRIP)
	      IF (BULL_DELETE+1.EQ.IER.AND.REMOTE_USER.EQ.FROM
     &		   .AND.INDEX(DESCRIP,SUBJECT(:SLEN)).GT.0) THEN
		 GO TO 50
	      END IF
	   END DO
	   CALL CLOSE_FILE(2)		! Specified message not found,
	   WRITE(ERROR_UNIT,1030)	! so error out.
	   RETURN
	END IF

C
C  Get the bulletin number to be deleted.
C

	IER = CLI$GET_VALUE('BULLETIN_NUMBER',BULL_PARAMETER,LEN_P)
	IF (IER.NE.%LOC(CLI$_ABSENT)) THEN	! Was bulletin specified?
	   DECODE(LEN_P,5,BULL_PARAMETER,ERR=920) BULL_DELETE	! Yes
5	   FORMAT(I<LEN_P>)
	ELSE IF (BULL_POINT.EQ.0) THEN	! No.  Have we just read a bulletin?
	   GO TO 910			! No, then error.
	ELSE
	   BULL_DELETE = BULL_POINT	! Delete the file we are reading
	END IF

	IF (BULL_DELETE.LE.0) GO TO 920

C
C  Check to see if specified bulletin is present, and if the user
C  is permitted to delete the bulletin.
C

	CALL OPEN_FILE(2)

	CALL READDIR(BULL_DELETE,IER)	! Get info for specified bulletin

	IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?
	   WRITE(ERROR_UNIT,1030)	! If not, then error out
	   GOTO 100
	END IF

	IF (USERNAME.NE.FROM) THEN	! If doesn't match owner of bulletin,
	   IF ((.NOT.SETPRV_PRIV().AND..NOT.FOLDER_SET).OR.    ! Privileges or
     &	       (.NOT.SETPRV_PRIV().AND.USERNAME.NE.FOLDER_OWNER
     &		.AND.FOLDER_SET)) THEN ! folder owner?
	      WRITE(ERROR_UNIT,1040)	! Then error out.
	      GO TO 100
	   ELSE
	      CALL CLOSE_FILE (2)
	      IF (.NOT.DECNET_PROC) THEN
	         WRITE (6,1050)		! Make sure user wants to delete it
	         READ (5,'(A)',IOSTAT=IER) ANSWER
	         CALL STR$UPCASE(ANSWER,ANSWER)
	         IF (ANSWER.NE.'Y') GO TO 900
	      END IF
	      CALL OPEN_FILE(2)
	      CALL READDIR(BULL_DELETE,IER) ! Get info for specified bulletin
	      IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?
	         WRITE(ERROR_UNIT,1030)		! If not, then error out
	         GOTO 100
	      END IF
	   END IF
	END IF

C
C  Delete the bulletin directory entry.
C

50	CALL DELETE_ENTRY(BULL_DELETE)		! Delete the directory entry

	CALL CLEANUP_DIRFILE(BULL_DELETE)	! Reorder directory file

	IF ((SYSTEM.AND.4).EQ.4) THEN		! Was entry shutdown bulletin?
	   CALL READDIR(0,IER)			! Get shutdown count
	   SHUTDOWN = SHUTDOWN - 1		! Decrement shutdown count
	END IF

	CALL UPDATE		! Somewhat a kludgey way of updating latest
				! bulletin and expired dates.

	IF (BULL_DELETE.LE.BULL_POINT) BULL_POINT = BULL_POINT - 1
				! Readjust where which bulletin to read next
				! if deletion causes messages to be moved.

100	CALL CLOSE_FILE(2)
	IF (DECNET_PROC) WRITE (5,'(''END'')')
				! Tell DECNET that delete went ok.
900	RETURN

910	WRITE(6,1010)
	GO TO 900

920	WRITE(6,1020)
	GO TO 900

1010	FORMAT(' ERROR: You are not reading any message.')
1020	FORMAT(' ERROR: Specified message number has incorrect format.')
1030	FORMAT(' ERROR: Specified message was not found.')
1040	FORMAT(' ERROR: Message was not deleted. Not owned by you.')
1050	FORMAT(' Message is not owned by you.',
     &	       ' Are you sure you want to delete it? ',$)

	END



	SUBROUTINE DIRECTORY(DIR_COUNT)
C
C  SUBROUTINE DIRECTORY
C
C  FUNCTION: Display directory of messages.
C
	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLDIR.INC'

	COMMON /PAGE/ PAGE_LENGTH

	DATA SCRATCH_D1/0/

	COMMON /POINT/ BULL_POINT

	EXTERNAL CLI$_ABSENT

	CHARACTER START_PARAMETER*16,DATETIME*23,TODAY*11

	CALL LIB$ERASE_PAGE(1,1)		! Clear the screen

C
C  Directory listing is first buffered into temporary memory storage before
C  being outputted to the terminal.  This is to be able to quickly close the
C  directory file, and to avoid the possibility of the user holding the screen,
C  and thus causing the directory file to stay open.  The temporary memory
C  is structured as a linked-list queue, where SCRATCH_D1 points to the header
C  of the queue.  See BULLSUBS.FOR for more description of the queue.
C

	CALL INIT_QUEUE(SCRATCH_D1,BULLDIR_COM)
	SCRATCH_D = SCRATCH_D1

	CALL OPEN_FILE_SHARED(2)		! Get directory file

	CALL READDIR(0,IER)			! Does directory header exist?
	IF (IER.EQ.1) THEN			! If so, there are messages
	   IF (DIR_COUNT.EQ.0) THEN
	      IF (CLI$PRESENT('START')) THEN	! Start number specified?
	         IER = CLI$GET_VALUE('START',START_PARAMETER,LEN)
	         DECODE(LEN,'(I<LEN>)',START_PARAMETER) DIR_COUNT
		 IF (DIR_COUNT.GT.NBULL) THEN
		    DIR_COUNT = NBULL
		 ELSE IF (DIR_COUNT.LT.1) THEN
		    WRITE (6,'('' ERROR: Invalid starting message.'')')
		    CALL CLOSE_FILE(2)
		    DIR_COUNT = 0
		    RETURN
		 END IF
	      ELSE IF (CLI$PRESENT('SINCE')) THEN	! Date specified?
		 IER = CLI$GET_VALUE('SINCE',DATETIME)
	   	  IF (DATETIME.EQ.'TODAY') THEN		! TODAY is the default.
	      	     IER = SYS$ASCTIM(,TODAY,,)		! Need to get date.
		     DATETIME = TODAY//' 00:00:00.0'
		  END IF
		  TEMP_COUNT = 0
		  IER = 1
		  DO WHILE (IER.EQ.TEMP_COUNT+1)
		   TEMP_COUNT = TEMP_COUNT + 1
		   CALL READDIR(TEMP_COUNT,IER)
		   IF (IER.NE.TEMP_COUNT+1) THEN
		     WRITE (6,'('' No messages past specified date.'')')
		     CALL CLOSE(2)
		     RETURN
		   ELSE
		     DIFF = COMPARE_DATE(DATETIME(1:11),DATE)
		     IF (DIFF.EQ.0) DIFF = COMPARE_TIME(DATETIME(13:20),TIME)
		     IF (DIFF.LE.0) THEN
			DIR_COUNT = TEMP_COUNT
			IER = IER + 1
		     END IF
		  END IF
		 END DO
	      ELSE
	         DIR_COUNT = BULL_POINT
		 IF (DIR_COUNT.EQ.0) DIR_COUNT = 1
	      END IF
	      IF (CLI$PRESENT('SINCE')) THEN
		 SBULL = DIR_COUNT
	         EBULL = DIR_COUNT + (PAGE_LENGTH - 6) - 1
	         IF (EBULL.GE.NBULL-2) EBULL = NBULL
	      ELSE IF (NBULL-DIR_COUNT+1.LE.PAGE_LENGTH-4) THEN
	         EBULL = NBULL
	         SBULL = NBULL - (PAGE_LENGTH-4) + 1
	         IF (SBULL.LT.1) SBULL = 1
	      ELSE
	         SBULL = DIR_COUNT
	         EBULL = DIR_COUNT + (PAGE_LENGTH - 6) - 1
	      END IF
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
	   NBULL = 0
	END IF

	CALL CLOSE_FILE(2)			! We don't need file anymore

	IF (NBULL.EQ.0) THEN
	   WRITE (6,'('' There are no messages present.'')')
	   RETURN
	END IF

C
C  Directory entries are now in queue.  Output queue entries to screen.
C

	SCRATCH_D = SCRATCH_D1			! Init queue pointer to header

	WRITE(6,1000)				! Write header
	DO I=SBULL,EBULL
	   CALL READ_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,BULLDIR_COM)
	   WRITE(6,2010) I,DESCRIP(:52),FROM,DATE(1:7)//DATE(10:11)
	END DO

	DIR_COUNT = EBULL + 1			! Update directory counter

	IF (DIR_COUNT.GT.NBULL) THEN		! Outputted all entries?
	   DIR_COUNT = 0			! Yes. Set counter to 0.
	ELSE
	   WRITE(6,1010)			! Else say there are more
	END IF

	RETURN

1000	FORMAT('    #',1X,'Description',43X,'From',9X,'Date',/)
1010	FORMAT(1X,/,' Press RETURN for more...',/)

2010	FORMAT(1X,I4,1X,A52,1X,A12,1X,A9)

	END
 

	SUBROUTINE FILE
C
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

	EXTERNAL CLI$_ABSENT

	IER = CLI$GET_VALUE('FILESPEC',BULL_PARAMETER,LEN_P)

	IF (IER.EQ.%LOC(CLI$_ABSENT)) THEN	! If no file name was specified
	   WRITE(6,1020)		! Write error
	   RETURN			! And return
	END IF

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

	IF (CLI$PRESENT('NEW')) THEN
	   OPEN(UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),ERR=900,
     &		STATUS='NEW',CARRIAGECONTROL='LIST')
	ELSE
	   OPEN(UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),ERR=900,
     &		STATUS='UNKNOWN',CARRIAGECONTROL='LIST',ACCESS='APPEND')
	END IF
	CALL ENABLE_PRIVS			! Reset SYSPRV privileges

	IF (CLI$PRESENT('HEADER')) THEN		! Printout header?
	   WRITE(3,1050) DESCRIP		! Output bulletin header info
	   WRITE(3,1060) FROM,DATE
	END IF

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

90	CLOSE (UNIT=3)			! Bulletin copy completed

	WRITE(6,1040) BULL_POINT,BULL_PARAMETER(1:LEN_P)
					! Show name of file created.
100	CALL CLOSE_FILE(1)
	RETURN

900	WRITE(6,1000)
	CALL ENABLE_PRIVS		! Reset BYPASS privileges
	GO TO 100

1000	FORMAT(' ERROR: Error in opening file.')
1010	FORMAT(' ERROR: You have not read any bulletin.')
1020	FORMAT(' ERROR: No file name was specified.')
1030	FORMAT(' ERROR: Specified bulletin was not found.')
1040	FORMAT(' Message ',I4,' written to ',A)
1050	FORMAT(/,'Description: ',A53)
1060	FORMAT('From: ',A12,' Date: ',A11,/)

	END




	SUBROUTINE LOGIN
C
C  SUBROUTINE LOGIN
C
C  FUNCTION: Alerts user of new messages upon logging in.
C
	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	COMMON /READIT/ READIT

	CHARACTER TODAY*23,INPUT*80,INREAD*1

	COMMON /PAGE/ PAGE_LENGTH

	COMMON /POINT/ BULL_POINT

	COMMON /PROMPT/ COMMAND_PROMPT
	CHARACTER*39 COMMAND_PROMPT

	LOGICAL*1 CTRL_G/7/

	DATA GEN_DIR1/0/	! General directory link list header
	DATA SYS_DIR1/0/	! System directory link list header
	DATA SYS_BUL1/0/	! System bulletin link list header

	DATA PAGE/0/

	DATA FIRST_WRITE/.TRUE./
	LOGICAL FIRST_WRITE

	DIMENSION H_NEW_FLAG(FLONG),H_SET_FLAG(FLONG),H_BRIEF_FLAG(FLONG)
	DIMENSION NOLOGIN_BTIM(2),LOGIN_BTIM_SAVE(2),TODAY_BTIM(2)
	DIMENSION DIR_BTIM(2),NEW_BTIM(2)

	CHARACTER*1 SEPARATE

	CALL SYS$ASCTIM(,TODAY,,)		! Get the present time
	CALL SYS$BINTIM(TODAY,TODAY_BTIM)

	CALL SYS$BINTIM('5-NOV-2956',NOLOGIN_BTIM)
	CALL SYS$BINTIM('5-NOV-1956 11:05:56',NEW_BTIM)

C
C  Find user entry in BULLUSER.DAT to update information and
C  to get the last date that messages were read.
C

	CALL OPEN_FILE_SHARED(4)		! Open user file

	DO WHILE (REC_LOCK(IER))
	 READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER) TEMP_USER,
     &	  NEWEST_BTIM,BBOARD_BTIM,H_NEW_FLAG,H_SET_FLAG,
     &	  H_BRIEF_FLAG,NOTIFY_FLAG			! Get the header
	END DO

	IF (IER.EQ.0) THEN			! Header is present.
	   DO WHILE (REC_LOCK(IER1))
	      READ (4,FMT=USER_FMT,KEY=USERNAME,IOSTAT=IER1) USERNAME,
     &	       LOGIN_BTIM,READ_BTIM,NEW_FLAG,SET_FLAG,
     &	       BRIEF_FLAG,NOTIFY_FLAG		! Find if there is an entry
	   END DO
	   IF (COMPARE_BTIM(LOGIN_BTIM,NOLOGIN_BTIM).EQ.0) RETURN  ! DISMAIL set
	   IF (IER1.EQ.0) THEN			! There is a user entry
	      REWRITE (4,FMT=USER_FMT) USERNAME,TODAY_BTIM,
     &	       READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG	! Update login date
     &	       ,NOTIFY_FLAG
	      DO I = 1,FLONG
		 IF (SET_FLAG(I).NE.0) READIT = 1
	      END DO
	   ELSE
	      CALL CLEANUP_LOGIN		! Good time to delete dead users
	      READ_BTIM(1) = NEW_BTIM(1)		! Make new entry
	      READ_BTIM(2) = NEW_BTIM(2)
	      DO I = 1,FLONG
	         NEW_FLAG(I) = 'FFFFFFFF'X
	         SET_FLAG(I) = H_SET_FLAG(I)
	         BRIEF_FLAG(I) = H_BRIEF_FLAG(I)
	      END DO
	      CALL CHECK_DISMAIL(USERNAME,DISMAIL)
	      IF (DISMAIL.EQ.1) THEN
	       WRITE (4,FMT=USER_FMT,IOSTAT=IER) USERNAME,NOLOGIN_BTIM,
     &		READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	      ELSE
	       WRITE (4,FMT=USER_FMT,IOSTAT=IER) USERNAME,TODAY_BTIM,
     &		READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	       DO I = 1,FLONG
		 IF (SET_FLAG(I).NE.0) READIT = 1
	       END DO
	      END IF
	      IF (IER.NE.0) THEN		! Error in writing to user file
		 WRITE (6,1070)			! Tell user of the error
		 CALL CLOSE_FILE(4)		! Close the user file
		 CALL EXIT			! Go away...
	      END IF
	      IF (DISMAIL.EQ.1) RETURN		! Go away if DISMAIL set
	      DIFF = -1				! Force us to look at messages
	   END IF
	   DO WHILE (REC_LOCK(IER2))
	    READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER2) TEMP_USER,
     &	     NEWEST_BTIM,BBOARD_BTIM,H_NEW_FLAG,H_SET_FLAG,
     &	     H_BRIEF_FLAG,NOTIFY_FLAG		! Reset read back to header
	   END DO
	END IF

	IF (IER.EQ.0.AND.MINUTE_DIFF(TODAY_BTIM,BBOARD_BTIM)
     &			.GT.BBOARD_UPDATE) THEN		! Update BBOARD mail?
	   REWRITE (4,FMT=USER_FMT) TEMP_USER,NEWEST_BTIM,  ! Rewrite header
     &		TODAY_BTIM,H_NEW_FLAG,H_SET_FLAG,H_BRIEF_FLAG,NOTIFY_FLAG
	   CALL CLOSE_FILE(4)
	   CALL CREATE_BBOARD_PROCESS
	ELSE
	   CALL CLOSE_FILE(4)
	   IF (IER.NE.0) CALL EXIT	! If no header, no messages
	END IF

	IF (IER1.EQ.0) THEN		! Skip date comparison if new entry
C
C  Compare and see if messages have been added since the last time
C  that the user has logged in or used the BULLETIN facility.
C
	   DIFF = COMPARE_BTIM(LOGIN_BTIM,READ_BTIM)
	   IF (DIFF.LT.0) THEN		! If read messages since last login,
	      LOGIN_BTIM(1) = READ_BTIM(1) ! then use the read date to compare
	      LOGIN_BTIM(2) = READ_BTIM(2) ! with the latest bulletin date
	   END IF			! to see if should alert user.

	   DIFF = COMPARE_BTIM(LOGIN_BTIM,NEWEST_BTIM)
	END IF

	LOGIN_BTIM_SAVE(1) = LOGIN_BTIM(1) ! These are destroyed in UPDATE_READ
	LOGIN_BTIM_SAVE(2) = LOGIN_BTIM(2)

	IF (DIFF.GT.0) THEN
	   BULL_POINT = -1
	   RETURN
	END IF

C
C  If there are new messages, look for them in BULLDIR.DAT
C  Save all new entries in the GEN_DIR file BULLCHECK.SCR so
C  that we can close BULLDIR.DAT as soon as possible.
C

	ENTRY LOGIN_FOLDER

	LOGIN_BTIM(1) = LOGIN_BTIM_SAVE(1)
	LOGIN_BTIM(2) = LOGIN_BTIM_SAVE(2)

	CALL OPEN_FILE_SHARED(2)	! Yes, so go get bulletin directory
	NGEN = 0			! Number of general messages
	NSYS = 0			! Number of system messages
	CALL READDIR(0,IER)		! Get header info
	CALL INIT_QUEUE(GEN_DIR1,BULLDIR_COM)
	CALL INIT_QUEUE(SYS_DIR1,BULLDIR_COM)
	GEN_DIR = GEN_DIR1
	SYS_DIR = SYS_DIR1
	BULL_POINT = -1
	START = 1
	REVERSE = 0
	IF (CLI$PRESENT('REVERSE').AND.
     &		.NOT.TEST2(BRIEF_FLAG,FOLDER_NUMBER)) THEN
	   REVERSE = 1
	   START = NBULL + 1
	   IER = START + 1
	   DIFF = 0
	   IF (IER1.NE.0) THEN
	      START = 1
	   ELSE
	     DO WHILE (START+1.EQ.IER.AND.DIFF.LE.0)
	      START = START - 1
	      IF (START.GT.0) CALL READDIR(START,IER)
	      IF (START+1.EQ.IER) THEN
		 CALL SYS$BINTIM(DATE//' '//TIME,DIR_BTIM)
	         DIFF = COMPARE_BTIM(LOGIN_BTIM,DIR_BTIM)
	      END IF
	     END DO
	     START = START + 1
	   END IF
	END IF
	DO ICOUNT1 = NBULL,START,-1
	   IF (REVERSE) THEN
	      ICOUNT = NBULL + START - ICOUNT1
	   ELSE
	      ICOUNT = ICOUNT1
	   END IF
	   CALL READDIR(ICOUNT,IER)
	   IF (IER1.EQ.0) THEN ! Is this a totally new user?
				  ! No. Is bulletin system or from same user?
	      IF (.NOT.REVERSE) THEN 
		 CALL SYS$BINTIM(DATE//' '//TIME,DIR_BTIM)
	         DIFF = COMPARE_BTIM(LOGIN_BTIM,DIR_BTIM) ! No, so compare date
	         IF (DIFF.GT.0) GO TO 100
	      END IF
	      IF (USERNAME.NE.FROM.OR.SYSTEM) THEN
		 IF (SYSTEM) THEN		! Is it system bulletin? 
		    NSYS = NSYS + 1
		    CALL WRITE_QUEUE(%VAL(SYS_DIR),SYS_DIR,BULLDIR_COM)
	         ELSE
		    IF (.NOT.REVERSE.OR.BULL_POINT.EQ.-1) THEN
		       BULL_POINT = ICOUNT - 1
		       IF (TEST2(BRIEF_FLAG,FOLDER_NUMBER)) GO TO 100
		    END IF
		    NGEN = NGEN + 1
		    SYSTEM = ICOUNT
		    CALL WRITE_QUEUE(%VAL(GEN_DIR),GEN_DIR,BULLDIR_COM)
	         END IF
	      END IF
	   ELSE			! Totally new user, save all messages
	      IF (SYSTEM) THEN
	         NSYS = NSYS + 1
		 CALL WRITE_QUEUE(%VAL(SYS_DIR),SYS_DIR,BULLDIR_COM)
	      ELSE
		 SYSTEM = ICOUNT	! Save bulletin number for display
		 IF (.NOT.REVERSE.OR.BULL_POINT.EQ.-1) THEN
		    BULL_POINT = ICOUNT - 1
		    IF (TEST2(BRIEF_FLAG,FOLDER_NUMBER)) GO TO 100
		 END IF
		 NGEN = NGEN + 1
		 CALL WRITE_QUEUE(%VAL(GEN_DIR),GEN_DIR,BULLDIR_COM)
	      END IF
	   END IF
	END DO
100	CALL CLOSE_FILE(2)
	IF (FOLDER_SET) NSYS = 0
C
C  Review new directory entries.  If there are system messages,
C  copy the system bulletin into GEN_DIR file BULLSYS.SCR for outputting
C  to the terminal.  If there are simple messages, just output the
C  header information.
C
	IF (NGEN.EQ.0.AND.NSYS.EQ.0) RETURN

	IF (NSYS.GT.0) THEN		! Are there any system messages?
	   CALL CLI$GET_VALUE('SEPARATE',SEPARATE)
	   IF (FIRST_WRITE) THEN
	      PAGE = 4		! Don't erase MAIL/PASSWORD notifies
	      FIRST_WRITE = .FALSE.	! if this is first write to screen.
	   END IF
	   WRITE (6,1026) CTRL_G	! Yep...
	   PAGE = PAGE + 1
	   CTRL_G = 0		! Don't ring bell for non-system bulls
	   CALL OPEN_FILE_SHARED(1)
	   CALL INIT_QUEUE(SYS_BUL1,INPUT)
	   SYS_BUL = SYS_BUL1
	   SYS_DIR = SYS_DIR1
	   DO J=1,NSYS
	      CALL READ_QUEUE(%VAL(SYS_DIR),SYS_DIR,BULLDIR_COM)
 	      INPUT = ' '
	      CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
	      LEN = 81
	      DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin to SYS_BUL link list
		 DO WHILE (LEN.GT.0)
		    CALL GET_BULL(I,INPUT,LEN)
		    IF (LEN.LT.0) THEN
		       CALL CLOSE_FILE(1)
		       RETURN
		    ELSE IF (LEN.GT.0) THEN
		       CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
		    END IF
		 END DO
		 LEN = 80
	      END DO
	      IF (J.LT.NSYS.AND.SEPARATE.NE.' ') THEN
 	         INPUT = ' '
	         CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
		 DO I=1,80
		    INPUT(I:I) = SEPARATE
		 END DO
		 CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
	      END IF
	   END DO
	   CALL CLOSE_FILE(1)
	   SYS_BUL = SYS_BUL1
	   DO WHILE (SYS_BUL.NE.0)	! Write out the system messages
	      CALL READ_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
	      IF (SYS_BUL.NE.0) THEN
		 IF (PAGE.EQ.PAGE_LENGTH-2) THEN	! If at end of screen
		    WRITE(6,1080)	! Ask for input to proceed to next page
		    CALL GET_INPUT_NOECHO(INREAD)	! Get terminal input
	            CALL LIB$ERASE_PAGE(1,1)		! Clear the screen
		    WRITE(6,1065) INPUT(1:TRIM(INPUT))
		    PAGE = 1
		 ELSE
		    WRITE(6,1060) INPUT(1:TRIM(INPUT))
		    PAGE = PAGE + 1
		 END IF
	      END IF
	   END DO
	   IF (NGEN.EQ.0) THEN
	      WRITE(6,'(A)')		! Write delimiting blank line
	   END IF
	   PAGE = PAGE + 1
	END IF
	GEN_DIR = GEN_DIR1
	IF (NGEN.GT.0) THEN		! Are there new non-system messages?
	   LENF = TRIM(FOLDER)
	   S1 = (80-13-LENF)/2
	   S2 = 80-S1-13-LENF
	   IF (PAGE+5+NGEN.GT.PAGE_LENGTH.AND.PAGE.GT.0) THEN
	      WRITE(6,1080)		! Ask for input to proceed to next page
	      CALL GET_INPUT_NOECHO(INREAD)
	      CALL LIB$ERASE_PAGE(1,1)	! Clear the screen
	      WRITE(6,1028) 'New '//FOLDER(1:LENF)//' messages',CTRL_G
	      PAGE = 1
	   ELSE
	      IF (FIRST_WRITE) THEN
		 PAGE = 4		  ! Don't erase MAIL/PASSWORD notifies
	         FIRST_WRITE = .FALSE. ! if this is first write to screen.
	      END IF
	      WRITE(6,1027) 'New '//FOLDER(1:LENF)//' messages',CTRL_G
	      PAGE = PAGE + 1
	   END IF
	   WRITE(6,1020)
	   WRITE(6,1025)
	   PAGE = PAGE + 2
	   DO I=1,NGEN
	      CALL READ_QUEUE(%VAL(GEN_DIR),GEN_DIR,BULLDIR_COM)
	      IF (PAGE.EQ.PAGE_LENGTH-2) THEN	! If at end of screen
		 WRITE(6,1080)	! Ask for input to proceed to next page
		 CALL GET_INPUT_NOECHO(INREAD)
	         CALL LIB$ERASE_PAGE(1,1)		! Clear the screen
		 PAGE = 1
	      ELSE
		 PAGE = PAGE + 1
	      END IF
              WRITE(6,1040) DESCRIP,FROM,DATE(:6),SYSTEM
					! Bulletin number is stored in SYSTEM
	   END DO
	   IF (FOLDER_NUMBER.GT.0.OR.(FOLDER_NUMBER.EQ.0.AND.
     &			BTEST(SET_FLAG(1),0))) THEN
	      PAGE = 0	! Don't reset page counter if READNEW not set for
	   END IF	! GENERAL, as no prompt to read is generated.
	END IF
	IF (NGEN.EQ.0.OR.
     &	    READIT.NE.0.OR.COMPARE_BTIM(READ_BTIM,NEW_BTIM).NE.0) THEN
	   WRITE(6,1030)
	ELSE
	   LEN = 27 + INDEX(COMMAND_PROMPT,'>') - 1
	   S1 = (80-LEN)/2
	   S2 = 80 - S1 - LEN
	   WRITE(6,1035)
     &	     'Type '//COMMAND_PROMPT(:LEN-27)//' to read new messages.'
	END IF

	RETURN

1020	FORMAT(' Description',43X,'From',9X,'Date',3X,'Number')
1025	FORMAT(' -----------',43X,'----',9X,'----',3X,'------')
1026	FORMAT(' ',33('*'),'System Messages',32('*'),A1)
1027	FORMAT(/,' ',<S1>('*'),A,<S2>('*'),A1)
1028	FORMAT('+',<S1>('*'),A,<S2>('*'),A1)
1030	FORMAT(' ',80('*'))
1035	FORMAT(' ',<S1>('*'),A,<S2>('*'))
1040	FORMAT(' ',A53,1X,A12,1X,A6,1X,I4)
1060	FORMAT(1X,A)
1065	FORMAT('+',A)
1070	FORMAT(' ERROR: Cannot add new entry to user file.')
1080	FORMAT(' ',/,' HIT any key for next page....')

	END


	SUBROUTINE GET_NODE_INFO
C
C  SUBROUTINE GET_NODE_INFO
C
C  FUNCTION: Gets local node name and obtains node names from
C	command line.
C

	IMPLICIT INTEGER (A-Z)

	EXTERNAL CLI$_ABSENT

	COMMON /NODE_INFO/ NODES,LOCAL_NODE_FOUND,NODE_NUM,
     &				NODE_ERROR,POINT_NODE
	CHARACTER*32 NODES(10)
	LOGICAL LOCAL_NODE_FOUND,NODE_ERROR

	CHARACTER*32 LOCAL_NODE

	NODE_ERROR = .FALSE.

	LOCAL_NODE_FOUND = .FALSE.
	CALL LIB$SYS_TRNLOG('SYS$NODE',L_NODE,LOCAL_NODE)
	L_NODE = L_NODE - 2			! Remove '::'
	IF (LOCAL_NODE(1:1).EQ.'_') THEN
	   LOCAL_NODE = LOCAL_NODE(2:)
	   L_NODE = L_NODE - 1
	END IF

	NODE_NUM = 0				! Initialize number of nodes
	IF (CLI$PRESENT('NODES')) THEN		! Decnet nodes specified?
	   LEN = 0				! GET_VALUE crashes if LEN<0
	   DO WHILE (CLI$GET_VALUE('NODES',NODES(NODE_NUM+1),LEN)
     &	    .NE.%LOC(CLI$_ABSENT))		! Get the specified nodes
	      NODE_NUM = NODE_NUM + 1
	      IF (INDEX(NODES(NODE_NUM),'::').GT.0) THEN   ! Remove :: if
		 LEN = INDEX(NODES(NODE_NUM),'::') - 1	   ! addedd
	      END IF
	      IF (LOCAL_NODE(1:L_NODE).EQ.NODES(NODE_NUM)(1:LEN)) THEN
	       NODE_NUM = NODE_NUM - 1
	       LOCAL_NODE_FOUND = .TRUE.
	      ELSE
	       POINT_NODE = NODE_NUM
	       OPEN (UNIT=9+NODE_NUM,NAME=NODES(NODE_NUM)(1:LEN)//'""::'
     &	       //'"TASK=BULLETIN"',ACCESS='SEQUENTIAL',FORM='FORMATTED',
     &	       CARRIAGECONTROL='NONE',TYPE='NEW',IOSTAT=IER)
	       IF (IER.NE.0) THEN
		  DO WHILE (NODE_NUM.GT.0)
		     CLOSE(UNIT=9+NODE_NUM)
		     NODE_NUM = NODE_NUM - 1
		  END DO
		  NODE_ERROR = .TRUE.
		  RETURN
	       END IF
	      END IF
	   END DO
	ELSE
	   LOCAL_NODE_FOUND = .TRUE.
	END IF

	RETURN
	END


	SUBROUTINE DELETE_NODE
C
C  SUBROUTINE DELETE_NODE
C
C  FUNCTION: Deletes files sent via ADD/NODES at remote hosts.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLDIR.INC'

	COMMON /NODE_INFO/ NODES,LOCAL_NODE_FOUND,NODE_NUM,
     &				NODE_ERROR,POINT_NODE
	CHARACTER*32 NODES(10)
	LOGICAL LOCAL_NODE_FOUND,NODE_ERROR

    	CHARACTER PASSWORD*31,INLINE*80,DEFAULT_USER*12

	CALL GET_NODE_INFO

 	IF (NODE_ERROR) GO TO 940

	IF (NODE_NUM.EQ.0.OR.LOCAL_NODE_FOUND) THEN
	   WRITE (6,'('' ERROR: Cannot specify local node.'')')
	   GO TO 999
	END IF

	IER = CLI$GET_VALUE('USERNAME',DEFAULT_USER)
	IF (.NOT.IER) DEFAULT_USER = USERNAME
	IER = CLI$GET_VALUE('SUBJECT',DESCRIP)

	DO POINT_NODE=1,NODE_NUM	   	! Write out command to nodes
	   SEMI = INDEX(NODES(POINT_NODE),'::')	! Look for semicolon after node
	   LEN = TRIM(NODES(POINT_NODE))	! Length of node name
	   IF (SEMI.GT.0) THEN			! Is semicolon present?
	      IF (LEN.GT.SEMI+1) THEN		! Yes, is username after node?
	         TEMP_USER = NODES(POINT_NODE)(SEMI+2:)	! Yes, set username
	         LEN = SEMI - 1			! Remove semicolon
	      ELSE				! No username after nodename
		 TEMP_USER = DEFAULT_USER	! Set username to default
	         LEN = SEMI - 1			! Remove semicolon
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
	         WRITE(6,'('' Enter password for node '',2A)')
     &			NODES(POINT_NODE),CHAR(10)
	         CALL GET_INPUT_NOECHO(PASSWORD)
	         IF (STR$POSITION(PASSWORD,CHAR(13)).LE.1) GO TO 910
	         OPEN (UNIT=10+NODE_NUM,NAME=NODES(POINT_NODE)(:LEN)
     &		   //'"'//TEMP_USER(1:TRIM(TEMP_USER))//' '//
     &		   PASSWORD(1:STR$POSITION(PASSWORD,CHAR(13))-1)//'"::',
     &		   TYPE='SCRATCH',IOSTAT=IER)
	         CLOSE (UNIT=10+NODE_NUM)
	         IF (IER.NE.0) THEN
		    WRITE (6,'('' ERROR: Password is invalid.'')')
	         END IF
	      END DO
	   END IF
	   WRITE (POINT_NODE+9,'(A)',ERR=940) INLINE
	   READ (POINT_NODE+9,'(A)',ERR=940,END=940) INLINE
	   IF (INLINE.EQ.'END') THEN
	      WRITE (6,'('' Message successfully deleted from node '',A)')
     &				NODES(POINT_NODE)
	   ELSE
	      WRITE (6,'('' Error while deleting message to node '',A)')
     &				NODES(POINT_NODE)
	      WRITE (6,'(A)') INLINE
	   END IF
	END DO

	GO TO 999

910	WRITE (6,1010)
	GO TO 999

940	WRITE (6,1015) NODES(POINT_NODE)

999	DO WHILE (NODE_NUM.GT.0)
	   CLOSE(UNIT=9+NODE_NUM)
	   NODE_NUM = NODE_NUM - 1
	END DO

	RETURN

1010	FORMAT (' ERROR: Deletion aborted.')
1015	FORMAT (' ERROR: Unable to reach node ',A)

	END
