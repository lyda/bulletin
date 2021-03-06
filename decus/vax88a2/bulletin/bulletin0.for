C
C  BULLETIN0.FOR, Version 3/19/88
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Programmer: Mark R. London
C
	SUBROUTINE GET_BROADCAST_MESSAGE(RING_BELL)

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE '($BRKDEF)'

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

C
C  The largest message that can be broadcasted is dependent on system
C  and user quotas.  The following limit is 12 lines of ( 80 characters +
C  CR/LF ) + 2 bells.  This should be more than enough room, as broadcasts
C  shouldn't be too large anyway.
C

	PARAMETER CR=CHAR(13),LF=CHAR(10),BELL=CHAR(7)

	PARAMETER BRDCST_LIMIT = 82*12 + 2
	CHARACTER*(BRDCST_LIMIT) BROAD

	COMMON /BROAD_MESSAGE/ BROAD,BLENGTH

	CHARACTER*128 INPUT

	IF (RING_BELL) THEN	! Include BELL in message?
	   BROAD(:36) =			! Say who the bulletin is from
     &		BELL//BELL//CR//LF//LF//'NEW BULLETIN FROM: '//FROM
	   BLENGTH = 37			! Start adding next line here
	ELSE
	   BROAD(:34) =			! Say who the bulletin is from
     &		CR//LF//LF//'NEW BULLETIN FROM: '//FROM
	   BLENGTH = 35			! Start adding next line here
	END IF

	IF (REMOTE_SET) REWIND (UNIT=3)

	END = 0
	ILEN = 81
	DO I=NBLOCK+1,NBLOCK+LENGTH	! Copy bulletin into file
	   DO WHILE (ILEN.GT.0)
	      IF (REMOTE_SET) THEN
	         READ (3,'(Q,A)',IOSTAT=IER) ILEN,INPUT
		 IF (IER.NE.0) RETURN
	      ELSE
	         CALL GET_BULL(I,INPUT,ILEN)
	      END IF
	      IF (ILEN.LT.0) THEN	! End of bulletin?
		 RETURN
	      ELSE IF (ILEN.GT.0) THEN
	         END = BLENGTH + ILEN - 1 + 2	! Check how long string will be
	         IF (END.GT.BRDCST_LIMIT) RETURN	! String too long?
	         BROAD(BLENGTH:END) = CR//LF//INPUT(:ILEN)! Else add new input
	         BLENGTH = END + 1			! Reset pointer
	      END IF
	   END DO
	   ILEN = 80
	END DO

	RETURN

	ENTRY BROADCAST(ALL,CLUSTER)

	IF (ALL) THEN				! Should we broadcast to ALL?
	   IF (CLUSTER) THEN
	      CALL SYS$BRKTHRU(,BROAD(:BLENGTH-1)//CR,,
     &			%VAL(BRK$C_ALLTERMS),,,%VAL(BRK$M_CLUSTER),,,,)
	   ELSE
	      CALL SYS$BRKTHRU(,BROAD(:BLENGTH-1)//CR,,
     &			%VAL(BRK$C_ALLTERMS),,,,,,,)
	   END IF
	ELSE	 				! Else just broadcast to users.
	   IF (CLUSTER) THEN
	      CALL SYS$BRKTHRU(,BROAD(:BLENGTH-1)//CR,,
     &			%VAL(BRK$C_ALLUSERS),,,%VAL(BRK$M_CLUSTER),,,,)
	   ELSE
	       CALL SYS$BRKTHRU(,BROAD(:BLENGTH-1)//CR,,
     &			%VAL(BRK$C_ALLUSERS),,,,,,,)
	   END IF
	END IF

	RETURN
	END


	SUBROUTINE GET_FOLDER_INFO(IER)
C
C  SUBROUTINE GET_FOLDER_INFO
C
C  FUNCTION: Obtains & verifies folder names from command line.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	EXTERNAL CLI$_ABSENT

	COMMON /NODE_INFO/ NODES,LOCAL_NODE_FOUND,NODE_NUM,
     &				NODE_ERROR,POINT_NODE
	CHARACTER*32 NODES(10)
	LOGICAL LOCAL_NODE_FOUND,NODE_ERROR

	CHARACTER NODE_TEMP*256

	NODE_NUM = 0				! Initialize number of nodes
	DO WHILE (CLI$GET_VALUE('SELECT_FOLDER',NODE_TEMP)
     &	    .NE.%LOC(CLI$_ABSENT))		! Get the specified nodes
	   IER = SYS_TRNLNM(NODE_TEMP,NODE_TEMP)
	   DO WHILE (TRIM(NODE_TEMP).GT.0)
	      NODE_NUM = NODE_NUM + 1
	      COMMA = INDEX(NODE_TEMP,',')
	      IF (COMMA.GT.0) THEN
		 NODES(NODE_NUM) = NODE_TEMP(:COMMA-1)
		 NODE_TEMP = NODE_TEMP(COMMA+1:)
	      ELSE
		 NODES(NODE_NUM) = NODE_TEMP
		 NODE_TEMP = ' '
	      END IF
	      NLEN = TRIM(NODES(NODE_NUM))
	      IF (NODES(NODE_NUM)(NLEN-1:NLEN).EQ.'::') THEN
		 NODES(NODE_NUM) = NODES(NODE_NUM)(:NLEN)//'GENERAL'
	      END IF
	      FOLDER_NUMBER = -1
	      FOLDER1 = NODES(NODE_NUM)
	      CALL SELECT_FOLDER(.FALSE.,IER)
	      IF (.NOT.IER) THEN
		 WRITE (6,'('' Unable to access folder '',A)')
     &				NODES(NODE_NUM)
		 RETURN
	      END IF
	   END DO
	END DO

	IER = 1

	RETURN
	END






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

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	EXTERNAL CLI$_ABSENT

	CHARACTER ANSWER*1,REMOTE_USER*12,SUBJECT*53,INPUT*20

	INTEGER EXBTIM(2),NOW(2)

	IMMEDIATE = 0
	IF (CLI$PRESENT('IMMEDIATE')) IMMEDIATE = 1

	IF (CLI$PRESENT('NODES')) THEN	! Delete messages on DECNET node?
	   CALL DELETE_NODE		! Yes...
	   RETURN
	ELSE IF (DECNET_PROC) THEN	! Is this from remote node?
	   IER = CLI$GET_VALUE('USERNAME',REMOTE_USER)
	   IER = CLI$GET_VALUE('SUBJECT',SUBJECT,SLEN)
	   CALL STR$UPCASE(SUBJECT,SUBJECT)
	   CALL OPEN_FILE(2)
	   DEL_BULL = 0
	   IER = 1
	   DO WHILE (DEL_BULL+1.EQ.IER)
	      DEL_BULL = DEL_BULL + 1
	      CALL READDIR(DEL_BULL,IER)
	      CALL STR$UPCASE(DESCRIP,DESCRIP)
	      IF (DEL_BULL+1.EQ.IER.AND.REMOTE_USER.EQ.FROM
     &		   .AND.INDEX(DESCRIP,SUBJECT(:SLEN)).GT.0) THEN
	         CALL REMOVE_ENTRY(DEL_BULL,DEL_BULL,DEL_BULL,IMMEDIATE)
		 CALL CLOSE_FILE(2)
	         WRITE (5,'(''END'')')	! Tell DECNET that delete went ok.
		 RETURN
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
	   CALL GET_2_VALS(BULL_PARAMETER,LEN_P,SBULL,EBULL,IER)
	ELSE IF (BULL_POINT.EQ.0) THEN	! No.  Have we just read a bulletin?
	   WRITE(6,1010)		! No, then error.
	   RETURN
	ELSE
	   SBULL = BULL_POINT		! Delete the file we are reading
	   EBULL = SBULL
	   IER = 0
	END IF

	IF (SBULL.LE.0.OR.IER.NE.0) THEN
	   WRITE (6,1020)
	   RETURN
	ELSE IF (EBULL.GT.NBULL.AND..NOT.REMOTE_SET.AND.
     &						SBULL.NE.EBULL) THEN
	   WRITE (6,'('' Last message specified > number in folder.'')')
	   WRITE (6,'('' Do you want to delete to end of folder? '',$)')
	   READ (5,'(A)',IOSTAT=IER) ANSWER
	   CALL STR$UPCASE(ANSWER,ANSWER)
	   IF (ANSWER.NE.'Y') THEN
	      WRITE (6,'('' Deletion aborted.'')')
	      RETURN
	   ELSE
	      EBULL = NBULL
	   END IF
	END IF

C
C  Check to see if specified bulletin is present, and if the user
C  is permitted to delete the bulletin.
C

	IF (REMOTE_SET) THEN
	   IF (SBULL.NE.EBULL) THEN
	      WRITE (6,1025)
	      RETURN
	   END IF
	   IF (SBULL.NE.BULL_POINT) CALL READDIR(SBULL,IER)
	   WRITE(REMOTE_UNIT,'(4A)',IOSTAT=IER)
     &			 4,SBULL,IMMEDIATE,DESCRIP
	   IF (IER.EQ.0) THEN
	      READ(REMOTE_UNIT,'(Q,A)',IOSTAT=IER) I,FOLDER1_COM
	   END IF
	   IF (IER.EQ.0) THEN
	      IF (I.EQ.LEN(FOLDER1_COM)) THEN
	         IER = SYS$ASCTIM(,INPUT,F1_NEWEST_BTIM,)
	         NEWEST_EXDATE = INPUT(1:11)
	         NEWEST_EXTIME = INPUT(13:20)
	         NBULL = F1_NBULL
	   	 CALL UPDATE_FOLDER
	      ELSE
	  	 WRITE (6,'(1X,A)') FOLDER1_COM(:I)
	      END IF
	   ELSE
	      CALL DISCONNECT_REMOTE
	   END IF
	   RETURN
	END IF

	CALL OPEN_FILE(2)

	DO BULL_DELETE = SBULL,EBULL
	   CALL READDIR(BULL_DELETE,IER)	! Get info for bulletin

	   IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?
	      WRITE(6,1030)			! If not, then error out
	      CALL CLOSE_FILE(2)
	      RETURN
	   END IF

	   IF (USERNAME.NE.FROM) THEN	! If doesn't match owner of bulletin,
	      IF ((.NOT.SETPRV_PRIV().AND..NOT.FOLDER_SET).OR.    ! Privileges?
     &	       (.NOT.SETPRV_PRIV().AND.USERNAME.NE.FOLDER_OWNER
     &		.AND.FOLDER_SET)) THEN
	         WRITE(6,1040)		! No, then error out.
	         CALL CLOSE_FILE(2)
		 RETURN
	      ELSE IF (SBULL.EQ.EBULL) THEN
	         CALL CLOSE_FILE(2)
	         WRITE (6,1050)	! Make sure user wants to delete it
	         READ (5,'(A)',IOSTAT=IER) ANSWER
	         CALL STR$UPCASE(ANSWER,ANSWER)
	         IF (ANSWER.NE.'Y') RETURN
	         CALL OPEN_FILE(2)
	         CALL READDIR(BULL_DELETE,IER)
	         IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?
	            WRITE(6,1030)	! If not, then error out
	            CALL CLOSE_FILE(2)
		    RETURN
	         END IF
	      END IF
	   END IF

C
C  Delete the bulletin directory entry.
C
	   CALL REMOVE_ENTRY(BULL_DELETE,SBULL,EBULL,IMMEDIATE)
	END DO

	CALL CLOSE_FILE(2)
	RETURN

1010	FORMAT(' ERROR: You are not reading any message.')
1020	FORMAT(' ERROR: Specified message number has incorrect format.')
1025	FORMAT(' ERROR: Cannot delete multiple messages in remote folder.')
1030	FORMAT(' ERROR: Specified message was not found.')
1040	FORMAT(' ERROR: Message was not deleted. Not owned by you.')
1050	FORMAT(' Message is not owned by you.',
     &	       ' Are you sure you want to delete it? ',$)

	END



	SUBROUTINE REMOVE_ENTRY(BULL_DELETE,SBULL,EBULL,IMMEDIATE)

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'

	COMMON /POINT/ BULL_POINT

	CHARACTER INPUT*20

	IF (IMMEDIATE.EQ.1) THEN		! Delete it immediately

	   CALL DELETE_ENTRY(BULL_DELETE)	! Delete the directory entry

	   IF ((SYSTEM.AND.4).EQ.4) THEN	! Was entry shutdown bulletin?
	      CALL READDIR(0,IER)		! Get shutdown count
	      SHUTDOWN = SHUTDOWN - 1		! Decrement shutdown count
	   END IF
	ELSE				! Delete it eventually
C
C  Change year of expiration date of message to 100 years less,
C  to indicate that message is to be deleted.  Then, set expiration date
C  in header of folder to 15 minutes from now.  Thus, the folder will be
C  checked in 15 minutes (or more), and will delete the messages then.
C
	   IF (SYSTEM.LE.1) THEN	! General or System message
	      EXDATE = EXDATE(1:7)//'18'//EXDATE(10:)
	   ELSE				! Permanent or Shutdown
	      IF (EXDATE(2:2).EQ.'-') THEN
	         EXDATE = EXDATE(1:6)//'19'//EXDATE(9:)
	      ELSE
	         EXDATE = EXDATE(1:7)//'19'//EXDATE(10:)
	      END IF
	   END IF

	   CALL WRITEDIR(BULL_DELETE,IER)	! Update message expiration date

	   IER = SYS$BINTIM('0 0:15',EXBTIM)	! Get time 15 minutes from now
	   IER = SYS$GETTIM(NOW)
	   IER = LIB$SUBX(NOW,EXBTIM,EXBTIM)
	   IER = SYS$ASCTIM(,INPUT,EXBTIM,)

	END IF

	IF (IMMEDIATE.NE.1.AND.BULL_DELETE.EQ.EBULL) THEN
	   CALL READDIR(0,IER)			! Get header

	   NEWEST_EXDATE = INPUT(1:11)		! and store new expiration date
	   NEWEST_EXTIME = INPUT(13:20)

	   CALL WRITEDIR(0,IER)
	ELSE IF (BULL_DELETE.EQ.EBULL) THEN
	   CALL CLEANUP_DIRFILE(SBULL)	! Reorder directory file

	   CALL UPDATE_ALWAYS	! Somewhat a kludgey way of updating latest
				! bulletin and expired dates.

	   IF (SBULL.LE.BULL_POINT) THEN
	      IF (BULL_POINT.GT.EBULL) THEN
	         BULL_POINT = BULL_POINT - (EBULL - SBULL + 1)
	      ELSE
		 BULL_POINT = SBULL
	      END IF
	   END IF		! Readjust where which bulletin to read next
				! if deletion causes messages to be moved.
	END IF

	RETURN
	END





	SUBROUTINE GET_2_VALS(INPUT,ILEN,SVAL,EVAL,IER)

	IMPLICIT INTEGER (A-Z)

	CHARACTER*(*) INPUT

	DELIM = MAX(INDEX(INPUT,':'),INDEX(INPUT,'-'))

	IF (DELIM.EQ.0) THEN
	   DECODE(ILEN,'(I<ILEN>)',INPUT,IOSTAT=IER) SVAL
	   EVAL = SVAL
	ELSE
	   DECODE(DELIM-1,'(I<DELIM-1>)',INPUT,IOSTAT=IER) SVAL
	   IF (IER.EQ.0) THEN
	      ILEN = ILEN - DELIM
	      DECODE(ILEN,'(I<ILEN>)',INPUT(DELIM+1:),IOSTAT=IER) EVAL
	   END IF
	   IF (EVAL.LT.SVAL) IER = 2
	END IF

	RETURN
	END

 

	SUBROUTINE DIRECTORY(DIR_COUNT)
C
C  SUBROUTINE DIRECTORY
C
C  FUNCTION: Display directory of messages.
C
	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	COMMON /PAGE/ PAGE_LENGTH, PAGING
	LOGICAL PAGING

	DATA SCRATCH_D1/0/

	COMMON /POINT/ BULL_POINT

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	EXTERNAL CLI$_ABSENT,CLI$_NEGATED,CLI$_PRESENT

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
	         IER = CLI$GET_VALUE('START',START_PARAMETER,ILEN)
	         DECODE(ILEN,'(I<ILEN>)',START_PARAMETER) DIR_COUNT
		 IF (DIR_COUNT.GT.NBULL) THEN
		    DIR_COUNT = NBULL
		 ELSE IF (DIR_COUNT.LT.1) THEN
		    WRITE (6,'('' ERROR: Invalid starting message.'')')
		    CALL CLOSE_FILE(2)
		    DIR_COUNT = 0
		    RETURN
		 END IF
	      ELSE IF (CLI$PRESENT('SINCE').OR.(FOLDER_NUMBER.GE.0.AND.
     &				CLI$PRESENT('NEW'))) THEN
		 IF (CLI$PRESENT('SINCE')) THEN		! Was /SINCE specified?
		   IER = CLI$GET_VALUE('SINCE',DATETIME)
	   	   IF (DATETIME.EQ.'TODAY') THEN	! TODAY is the default.
	      	     IER = SYS$ASCTIM(,TODAY,,)		! Need to get date.
		     DATETIME = TODAY//' 00:00:00.0'
		   END IF
		 ELSE IF (CLI$PRESENT('NEW')) THEN	! was /NEW specified?
	   	   DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &			       F_NEWEST_BTIM)
		   IF (DIFF.GE.0) THEN
		     WRITE (6,'('' No new messages are present in'',
     &			'' folder '',A,''.'')') FOLDER(:TRIM(FOLDER))
		     CALL CLOSE_FILE(2)
		     RETURN
		   ELSE
		     CALL SYS$ASCTIM
     &			(,DATETIME,LAST_READ_BTIM(1,FOLDER_NUMBER+1),)
		   END IF
		 END IF
		 TEMP_COUNT = 0
		 IER = 1
		 DO WHILE (IER.EQ.TEMP_COUNT+1)
		   TEMP_COUNT = TEMP_COUNT + 1
		   CALL READDIR(TEMP_COUNT,IER)
		   IF (IER.NE.TEMP_COUNT+1) THEN
		     WRITE (6,'('' No messages past specified date.'')')
		     CALL CLOSE_FILE(2)
		     RETURN
		   ELSE
		     DIFF = COMPARE_DATE(DATETIME(1:11),DATE)
		     IF (DIFF.EQ.0) DIFF = COMPARE_TIME(DATETIME(13:20),TIME)
		     IF (DIFF.LT.0) THEN
			DIR_COUNT = TEMP_COUNT
			IER = IER + 1
		     END IF
		   END IF
		 END DO
	      ELSE
	         DIR_COUNT = BULL_POINT
		 IF (DIR_COUNT.EQ.0) DIR_COUNT = 1
	      END IF
	      IF (CLI$PRESENT('SINCE').OR.CLI$PRESENT('NEW')) THEN
		 SBULL = DIR_COUNT
	         EBULL = DIR_COUNT + (PAGE_LENGTH - 7) - 1
	         IF (EBULL.GE.NBULL-2) EBULL = NBULL
	      ELSE IF (NBULL-DIR_COUNT+1.LE.PAGE_LENGTH-5) THEN
	         EBULL = NBULL
	         SBULL = NBULL - (PAGE_LENGTH-5) + 1
	         IF (SBULL.LT.1) SBULL = 1
	      ELSE
	         SBULL = DIR_COUNT
	         EBULL = DIR_COUNT + (PAGE_LENGTH - 7) - 1
	      END IF
	   ELSE
	      SBULL = DIR_COUNT
	      EBULL = DIR_COUNT + (PAGE_LENGTH - 7) - 1
	      IF (EBULL.GE.NBULL-2) EBULL = NBULL
	   END IF
	   IF (.NOT.PAGING) THEN
	      EBULL = NBULL
	   END IF
	   IF (.NOT.REMOTE_SET) THEN
	      DO I=SBULL,EBULL			! Copy messages from file
	         CALL READDIR(I,IER)		! Into the queue
	         CALL WRITE_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,BULLDIR_COM)
	      END DO
	   ELSE
	      WRITE(REMOTE_UNIT,'(3A)',IOSTAT=IER) 13,SBULL,EBULL
	      IF (IER.EQ.0) THEN
		 I = SBULL
		 DO WHILE (IER.EQ.0.AND.I.LE.EBULL)
	            READ(REMOTE_UNIT,'(A)',IOSTAT=IER) BULLDIR_COM
	            CALL WRITE_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,BULLDIR_COM)
		    I = I + 1
		 END DO
	      END IF
	      IF (IER.NE.0) THEN
	         CALL CLOSE_FILE(2)
		 CALL DISCONNECT_REMOTE
		 RETURN
	      END IF
	   END IF
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

	FLEN = TRIM(FOLDER)
	WRITE(6,'(<81-FLEN>X,A)') FOLDER(:FLEN)
	WRITE(6,1000)				! Write header
	DO I=SBULL,EBULL
	   CALL READ_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,BULLDIR_COM)
	   IF (EXDATE(8:9).EQ.'18'.OR.INDEX(EXDATE,'1900').GT.0) THEN
	      WRITE(6,2010) I,DESCRIP(:52),FROM,'(DELETED)'
	   ELSE
	      WRITE(6,2010) I,DESCRIP(:52),FROM,DATE(1:7)//DATE(10:11)
	   END IF
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

	ILEN = 81
	DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file
	   DO WHILE (ILEN.GT.0)
	      CALL GET_BULL(I,INPUT,ILEN)
	      IF (ILEN.LT.0) THEN
		 GO TO 90
	      ELSE IF (ILEN.GT.0) THEN
	         WRITE (3,'(A)') INPUT(1:ILEN)
	      END IF
	   END DO
	   ILEN = 80
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

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /PAGE/ PAGE_LENGTH,PAGING
	LOGICAL PAGING

	COMMON /POINT/ BULL_POINT

	COMMON /PROMPT/ COMMAND_PROMPT
	CHARACTER*39 COMMAND_PROMPT

	COMMON /SYSTEM_FOLDERS/ SYSTEM_FLAG(FLONG),DUMMY(2)

	COMMON /COMMAND_SWITCHES/ LOGIN_SWITCH,SYSTEM_SWITCH
	COMMON /COMMAND_SWITCHES/ SYSTEM_LOGIN_BTIM(2)
	COMMON /COMMAND_SWITCHES/ REVERSE_SWITCH,SEPARATE
	CHARACTER*1 SEPARATE

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	CHARACTER TODAY*23,INPUT*80,INREAD*1

	LOGICAL*1 CTRL_G/7/

	DATA GEN_DIR1/0/	! General directory link list header
	DATA SYS_DIR1/0/	! System directory link list header
	DATA SYS_NUM1/0/	! System message number link list header
	DATA SYS_BUL1/0/	! System bulletin link list header
	DATA ALL_DIR1/0/	! Full directory link list header (for remote)

	DATA PAGE/0/

	DATA FIRST_WRITE/.TRUE./
	LOGICAL FIRST_WRITE

	DIMENSION NOLOGIN_BTIM(2),LOGIN_BTIM_SAVE(2),TODAY_BTIM(2)
	DIMENSION DIR_BTIM(2),NEW_BTIM(2)

	CALL SYS$ASCTIM(,TODAY,,)		! Get the present time
	CALL SYS_BINTIM(TODAY,TODAY_BTIM)

	CALL SYS_BINTIM('5-NOV-2956 00:00:00.00',NOLOGIN_BTIM)
	CALL SYS_BINTIM('5-NOV-1956 11:05:56',NEW_BTIM)

C
C  Find user entry in BULLUSER.DAT to update information and
C  to get the last date that messages were read.
C

	CALL OPEN_FILE_SHARED(4)

	CALL READ_USER_FILE_HEADER(IER)		! Get the header

	IF (IER.EQ.0) THEN			! Header is present.
	   UNLOCK 4
	   CALL READ_USER_FILE_KEYNAME(USERNAME,IER1)
						! Find if there is an entry
	   IF (NEW_FLAG(1).LT.143.OR.NEW_FLAG(1).GT.143) THEN
	      NEW_FLAG(2)=0		! If old version clear GENERIC value
	      NEW_FLAG(1)=143		! Set new version number
	   END IF
	   IF (COMPARE_BTIM(LOGIN_BTIM,NOLOGIN_BTIM).GE.0) RETURN  ! DISMAIL set
	   IF (IER1.EQ.0) THEN			! There is a user entry
	      LOGIN_BTIM_SAVE(1) = LOGIN_BTIM(1)
	      LOGIN_BTIM_SAVE(2) = LOGIN_BTIM(2)
	      LOGIN_BTIM(1) = TODAY_BTIM(1)
	      LOGIN_BTIM(2) = TODAY_BTIM(2)
	      REWRITE (4) USER_ENTRY
	      IF (SYSTEM_FLAG(1).NE.0.AND.SYSTEM_FLAG(1).NE.1) READIT = 1
	      DO I = 1,FLONG
		 IF (SET_FLAG(I).NE.0.OR.BRIEF_FLAG(I).NE.0.OR.
     &		    (I.GT.1.AND.SYSTEM_FLAG(I).NE.0)) READIT = 1
	      END DO
	   ELSE
	      CALL CLEANUP_LOGIN		! Good time to delete dead users
	      READ_BTIM(1) = NEW_BTIM(1)		! Make new entry
	      READ_BTIM(2) = NEW_BTIM(2)
	      DO I = 1,FLONG
	         SET_FLAG(I) = SET_FLAG_DEF(I)
	         BRIEF_FLAG(I) = BRIEF_FLAG_DEF(I)
		 NOTIFY_FLAG(I) = NOTIFY_FLAG_DEF(I)
	      END DO
	      NEW_FLAG(1) = 143
	      NEW_FLAG(2) = 0
	      CALL CHECK_DISMAIL(USERNAME,DISMAIL)
	      IF (DISMAIL.EQ.1) THEN
		 LOGIN_BTIM(1) = NOLOGIN_BTIM(1)
		 LOGIN_BTIM(2) = NOLOGIN_BTIM(2)
	         LOGIN_BTIM_SAVE(1) = LOGIN_BTIM(1)
	         LOGIN_BTIM_SAVE(2) = LOGIN_BTIM(2)
	      ELSE
	         LOGIN_BTIM_SAVE(1) = LOGIN_BTIM(1)
	         LOGIN_BTIM_SAVE(2) = LOGIN_BTIM(2)
	         LOGIN_BTIM(1) = TODAY_BTIM(1)
	         LOGIN_BTIM(2) = TODAY_BTIM(2)
	         DO I = 1,FLONG
		    IF (SET_FLAG(I).NE.0) READIT = 1
	         END DO
	      END IF
	      CALL WRITE_USER_FILE(IER)
	      IF (IER.NE.0) THEN		! Error in writing to user file
		 WRITE (6,1070)			! Tell user of the error
		 CALL CLOSE_FILE(4)		! Close the user file
		 CALL EXIT			! Go away...
	      END IF
	      IF (DISMAIL.EQ.1) RETURN		! Go away if DISMAIL set
	      DIFF = -1				! Force us to look at messages
	      CALL OPEN_FILE_SHARED(9)
	      DO I=1,FOLDER_MAX
	         LAST_READ_BTIM(1,I) = READ_BTIM(1)
	         LAST_READ_BTIM(2,I) = READ_BTIM(2)
	      END DO
	      WRITE (9,IOSTAT=IER) USERNAME,
     &		((LAST_READ_BTIM(I,J),I=1,2),J=1,FOLDER_MAX)
	      CALL CLOSE_FILE(9)
	   END IF
	   LOGIN_BTIM(1) = LOGIN_BTIM_SAVE(1)
	   LOGIN_BTIM(2) = LOGIN_BTIM_SAVE(2)
	   CALL READ_USER_FILE_HEADER(IER2)	! Reset read back to header
	END IF

	IF (IER.EQ.0.AND.MINUTE_DIFF(TODAY_BTIM,BBOARD_BTIM)
     &			.GT.BBOARD_UPDATE) THEN	! Update BBOARD mail?
	   BBOARD_BTIM(1) = TODAY_BTIM(1)
	   BBOARD_BTIM(2) = TODAY_BTIM(2)
	   REWRITE (4) USER_HEADER		! Rewrite header
	   CALL CLOSE_FILE(4)
	   IF (.NOT.TEST_BULLCP()) CALL CREATE_BBOARD_PROCESS
	ELSE
	   CALL CLOSE_FILE(4)
	   IF (IER.NE.0) CALL EXIT	! If no header, no messages
	END IF

	IF (IER1.EQ.0) THEN		! Skip date comparison if new entry
C
C  Compare and see if messages have been added since the last time
C  that the user has logged in or used the BULLETIN facility.
C
	   DIFF1 = COMPARE_BTIM(LOGIN_BTIM,READ_BTIM)
	   IF (DIFF1.LT.0) THEN		! If read messages since last login,
	      LOGIN_BTIM(1) = READ_BTIM(1) ! then use the read date to compare
	      LOGIN_BTIM(2) = READ_BTIM(2) ! with the latest bulletin date
	   END IF			! to see if should alert user.

	   IF (SYSTEM_SWITCH) THEN
	      DIFF1 = COMPARE_BTIM(SYSTEM_LOGIN_BTIM,NEWEST_BTIM)
	   ELSE
	      DIFF1 = COMPARE_BTIM(LOGIN_BTIM,NEWEST_BTIM)
	   END IF
	END IF

	LOGIN_BTIM_SAVE(1) = LOGIN_BTIM(1) ! These are destroyed in UPDATE_READ
	LOGIN_BTIM_SAVE(2) = LOGIN_BTIM(2)
	
	IF (NEW_FLAG(2).NE.0) THEN
	   CALL LIB$MOVC3(4,NEW_FLAG(2),%REF(BULL_PARAMETER))
	   CALL SUBTIME(LOGIN_BTIM,BULL_PARAMETER(1:4),IER)
	ELSE IF (DIFF1.GT.0) THEN
	   BULL_POINT = -1
	   RETURN
	END IF

C
C  If there are new messages, look for them in BULLDIR.DAT
C  Save all new entries in the GEN_DIR file BULLCHECK.SCR so
C  that we can close BULLDIR.DAT as soon as possible.
C

	ENTRY LOGIN_FOLDER

	IF (NEW_FLAG(2).EQ.0.OR.FOLDER_SET) THEN
	   LOGIN_BTIM(1) = LOGIN_BTIM_SAVE(1)
	   LOGIN_BTIM(2) = LOGIN_BTIM_SAVE(2)
	END IF

	ENTRY SHOW_SYSTEM

	JUST_SYSTEM = (.NOT.LOGIN_SWITCH.AND.SYSTEM_SWITCH).OR.
     &	   (FOLDER_NUMBER.GT.0.AND.BTEST(FOLDER_FLAG,2)
     &		.AND..NOT.TEST2(SET_FLAG,FOLDER_NUMBER)
     &		.AND..NOT.TEST2(BRIEF_FLAG,FOLDER_NUMBER))

	NGEN = 0			! Number of general messages
	NSYS = 0			! Number of system messages
	BULL_POINT = -1

	IF (IER1.NE.0.AND.FOLDER_NUMBER.GT.0) RETURN
		! Don't overwhelm new user with lots of non-general msgs

	IF (BTEST(FOLDER_FLAG,2).AND.SYSTEM_SWITCH) THEN
			! Can folder have SYSTEM messages and /SYSTEM specified?
	   LOGIN_BTIM(1) = SYSTEM_LOGIN_BTIM(1)	! Use specified login time
	   LOGIN_BTIM(2) = SYSTEM_LOGIN_BTIM(2)	! for system messages.
	END IF

	CALL OPEN_FILE_SHARED(2)	! Get bulletin directory
	IF (.NOT.REMOTE_SET) THEN
	   CALL READDIR(0,IER)		! Get header info
	ELSE
	   NBULL = F_NBULL
	END IF
	   
	CALL INIT_QUEUE(GEN_DIR1,BULLDIR_COM)
	CALL INIT_QUEUE(SYS_DIR1,BULLDIR_COM)
	CALL INIT_QUEUE(SYS_NUM1,%DESCR(ICOUNT))
	GEN_DIR = GEN_DIR1
	SYS_DIR = SYS_DIR1
	SYS_NUM = SYS_NUM1
	START = 1
	REVERSE = 0
	IF (REVERSE_SWITCH.AND.(.NOT.TEST2(SET_FLAG,FOLDER_NUMBER).OR.
     &		.NOT.TEST2(BRIEF_FLAG,FOLDER_NUMBER))) THEN
	   REVERSE = 1
	   CALL GET_NEWEST_MSG(LOGIN_BTIM,START)
	   IF (START.EQ.-1) START = NBULL
	   START = START + 1
	END IF

	IF (REMOTE_SET) THEN
	   CALL INIT_QUEUE(ALL_DIR1,BULLDIR_COM)
	   IF (REVERSE) THEN
	      WRITE(REMOTE_UNIT,'(3A)',IOSTAT=IER) 13,START,NBULL
	   ELSE
	      WRITE(REMOTE_UNIT,'(3A)',IOSTAT=IER) 13,NBULL,START
	   END IF
	   IF (IER.EQ.0) THEN
	      ALL_DIR = ALL_DIR1
	      I = START
	      DO WHILE (IER.EQ.0.AND.I.LE.NBULL)
	         READ(REMOTE_UNIT,'(A)',IOSTAT=IER) BULLDIR_COM
	         CALL WRITE_QUEUE(%VAL(ALL_DIR),ALL_DIR,BULLDIR_COM)
		 I = I + 1
	      END DO
	   END IF
	   IF (IER.NE.0) THEN
	      CALL CLOSE_FILE(2)
	      CALL DISCONNECT_REMOTE
	      RETURN
	   END IF
	   ALL_DIR = ALL_DIR1
	END IF

	DO ICOUNT1 = NBULL,START,-1
	   IF (REVERSE) THEN
	      ICOUNT = NBULL + START - ICOUNT1
	   ELSE
	      ICOUNT = ICOUNT1
	   END IF
	   IF (REMOTE_SET) THEN
	      CALL READ_QUEUE(%VAL(ALL_DIR),ALL_DIR,BULLDIR_COM)
	      IER = ICOUNT + 1
	   ELSE
	      CALL READDIR(ICOUNT,IER)
	   END IF
	   IF (IER1.EQ.0.AND.IER.EQ.ICOUNT+1) THEN ! Is this a totally new user?
				  ! No. Is bulletin system or from same user?
	      IF (.NOT.REVERSE) THEN 
		 CALL SYS_BINTIM(DATE//' '//TIME,DIR_BTIM)
	         DIFF = COMPARE_BTIM(LOGIN_BTIM,DIR_BTIM) ! No, so compare date
	         IF (DIFF.GT.0) GO TO 100
	      END IF
	      IF (.NOT.BTEST(FOLDER_FLAG,2)) SYSTEM = SYSTEM.AND.(.NOT.1)
			! Show system msg in non-system folder as general msg
	      IF (USERNAME.NE.FROM.OR.SYSTEM) THEN
		 IF (SYSTEM) THEN		! Is it system bulletin? 
		    NSYS = NSYS + 1
		    CALL WRITE_QUEUE(%VAL(SYS_DIR),SYS_DIR,BULLDIR_COM)
		    CALL WRITE_QUEUE(%VAL(SYS_NUM),SYS_NUM,%DESCR(ICOUNT))
	         ELSE IF (.NOT.JUST_SYSTEM) THEN
		    IF (SYSTEM_SWITCH) THEN
		       CALL SYS_BINTIM(DATE//' '//TIME,DIR_BTIM)
	               DIFF = COMPARE_BTIM(LOGIN_BTIM_SAVE,DIR_BTIM)
		    ELSE
		       DIFF = -1
		    END IF
		    IF (DIFF.LT.0) THEN
		       IF (.NOT.REVERSE.OR.BULL_POINT.EQ.-1) THEN
		          BULL_POINT = ICOUNT - 1
		          IF (TEST2(BRIEF_FLAG,FOLDER_NUMBER).AND.
     &			   TEST2(SET_FLAG,FOLDER_NUMBER)) GO TO 100
		       END IF
		       NGEN = NGEN + 1
		       SYSTEM = ICOUNT
		       CALL WRITE_QUEUE(%VAL(GEN_DIR),GEN_DIR,BULLDIR_COM)
		    END IF
	         END IF
	      END IF
	   ELSE IF (IER.EQ.ICOUNT+1) THEN
			! Totally new user, save only permanent system msgs
	      IF (SYSTEM.EQ.3) THEN
	         NSYS = NSYS + 1
		 CALL WRITE_QUEUE(%VAL(SYS_DIR),SYS_DIR,BULLDIR_COM)
		 CALL WRITE_QUEUE(%VAL(SYS_NUM),SYS_NUM,%DESCR(ICOUNT))
	      ELSE IF (NGEN.EQ.0) THEN	! And save only the first non-system msg
		 SYSTEM = ICOUNT	! Save bulletin number for display
		 IF (.NOT.REVERSE.OR.BULL_POINT.EQ.-1) THEN
		    BULL_POINT = ICOUNT - 1
		    IF (TEST2(BRIEF_FLAG,FOLDER_NUMBER).AND.
     &		 	TEST2(SET_FLAG,FOLDER_NUMBER)) GO TO 100
		 END IF
		 NGEN = NGEN + 1
		 CALL WRITE_QUEUE(%VAL(GEN_DIR),GEN_DIR,BULLDIR_COM)
	      END IF
	   END IF
	END DO
100	CALL CLOSE_FILE(2)
C
C  Review new directory entries.  If there are system messages,
C  copy the system bulletin into GEN_DIR file BULLSYS.SCR for outputting
C  to the terminal.  If there are simple messages, just output the
C  header information.
C
	IF (NGEN.EQ.0.AND.NSYS.EQ.0) RETURN

	IF (NSYS.GT.0) THEN		! Are there any system messages?
	   IF (FIRST_WRITE) THEN
	      PAGE = 4		! Don't erase MAIL/PASSWORD notifies
	      FIRST_WRITE = .FALSE.	! if this is first write to screen.
	   END IF
	   LENF = TRIM(FOLDER)
	   S1 = (80-(LENF+16))/2
	   S2 = 80 - S1 - (LENF + 16)
	   WRITE (6,1026) FOLDER(:LENF),CTRL_G	! Yep...
	   PAGE = PAGE + 1
	   CTRL_G = 0		! Don't ring bell for non-system bulls
	   CALL OPEN_FILE_SHARED(1)
	   CALL INIT_QUEUE(SYS_BUL1,INPUT)
	   SYS_BUL = SYS_BUL1
	   SYS_DIR = SYS_DIR1
	   SYS_NUM = SYS_NUM1
	   NSYS_LINE = 0
	   DO J=1,NSYS
	      CALL READ_QUEUE(%VAL(SYS_DIR),SYS_DIR,BULLDIR_COM)
	      IF (REMOTE_SET) THEN
	         CALL READ_QUEUE(%VAL(SYS_NUM),SYS_NUM,%DESCR(ICOUNT))
	         WRITE (REMOTE_UNIT,'(2A)',IOSTAT=IER) 5,ICOUNT
	         IF (IER.GT.0) THEN
	            CALL DISCONNECT_REMOTE
	         ELSE
	            CALL GET_REMOTE_MESSAGE(IER)
	         END IF
		 IF (IER.GT.0) THEN
		    CALL CLOSE_FILE(1)
		    RETURN
		 END IF
	      END IF
 	      INPUT = ' '
	      CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
	      NSYS_LINE = NSYS_LINE + 1
	      ILEN = 81
	      DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin to SYS_BUL link list
		 DO WHILE (ILEN.GT.0)
		    CALL GET_BULL(I,INPUT,ILEN)
		    IF (ILEN.LT.0) THEN
		       CALL CLOSE_FILE(1)
		       RETURN
		    ELSE IF (ILEN.GT.0) THEN
		       CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
		       NSYS_LINE = NSYS_LINE + 1
		    END IF
		 END DO
		 ILEN = 80
	      END DO
	      IF (J.LT.NSYS.AND.SEPARATE.NE.' ') THEN
 	         INPUT = ' '
	         CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
		 DO I=1,80
		    INPUT(I:I) = SEPARATE
		 END DO
		 CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
	         NSYS_LINE = NSYS_LINE + 2
	      END IF
	   END DO
	   CALL CLOSE_FILE(1)
	   SYS_BUL = SYS_BUL1
	   DO I = 1,NSYS_LINE	! Write out the system messages
	      CALL READ_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
	      IF (SYS_BUL.NE.0) THEN
		 IF (PAGE.EQ.PAGE_LENGTH-2.AND.PAGING) THEN
							! If at end of screen
		    WRITE(6,1080)	! Ask for input to proceed to next page
		    CALL GET_INPUT_NOECHO_PROMPT(INREAD,! Get terminal input
     &			'HIT any key for next page....')
	            CALL LIB$ERASE_PAGE(1,1)		! Clear the screen
		    PAGE = 1
		    WRITE(6,1060) '+'//INPUT(1:TRIM(INPUT))
		 ELSE
		    PAGE = PAGE + 1
		    WRITE(6,1060) ' '//INPUT(1:TRIM(INPUT))
		 END IF
	      END IF
	   END DO
	   IF (NGEN.EQ.0) THEN
	      WRITE(6,'(A)')		! Write delimiting blank line
	   END IF
	   PAGE = PAGE + 1
	END IF

	ENTRY REDISPLAY_DIRECTORY

	GEN_DIR = GEN_DIR1
	IF (NGEN.GT.0) THEN		! Are there new non-system messages?
	   LENF = TRIM(FOLDER)
	   S1 = (80-13-LENF)/2
	   S2 = 80-S1-13-LENF
	   IF (PAGE+5+NGEN.GT.PAGE_LENGTH.AND.PAGE.GT.0) THEN
	      WRITE(6,1080)		! Ask for input to proceed to next page
	      CALL GET_INPUT_NOECHO_PROMPT(INREAD,	! Get terminal input
     &			'HIT any key for next page....')
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
	   I = 0
	   DO WHILE (I.LT.NGEN)
	      I = I + 1
	      CALL READ_QUEUE(%VAL(GEN_DIR),GEN_DIR,BULLDIR_COM)
	      IF (PAGE.EQ.PAGE_LENGTH-2.AND.PAGING) THEN ! If at end of screen
		 WRITE(6,1080)	! Ask for input to proceed to next page
		 CALL GET_INPUT_NOECHO_PROMPT(INREAD,
     &		'HIT Q(Quit listing) or any other key for next page....')
	         CALL STR$UPCASE(INREAD,INREAD)
	         CALL LIB$ERASE_PAGE(1,1)		! Clear the screen
		 PAGE = 1
		 IF (INREAD.EQ.'Q') THEN
		    I = NGEN		! Quit directory listing
		    WRITE(6,'(''+Quitting directory listing.'')')
		 ELSE
		    WRITE(6,1040) '+'//DESCRIP,FROM,DATE(:6),SYSTEM
		 END IF
					! Bulletin number is stored in SYSTEM
	      ELSE
		 PAGE = PAGE + 1
		 WRITE(6,1040) ' '//DESCRIP,FROM,DATE(:6),SYSTEM
	      END IF
	   END DO
	   IF ((.NOT.FOLDER_SET.AND.BTEST(SET_FLAG(1),0).AND.DIFF1.LE.0)
     &		.OR.(FOLDER_SET.AND.TEST2(SET_FLAG,FOLDER_NUMBER))) THEN
	      PAGE = 0	! Don't reset page counter if READNEW not set,
	   END IF	! as no prompt to read is generated.
	END IF
	IF (COMPARE_BTIM(READ_BTIM,NEW_BTIM).NE.0) THEN
	   WRITE(6,1030)
	ELSE IF (NGEN.EQ.0) THEN
	   ILEN = 57 + INDEX(COMMAND_PROMPT,'>') - 1
	   S1 = (80-ILEN)/2
	   S2 = 80 - S1 - ILEN
	   WRITE(6,1035) 'The '//COMMAND_PROMPT(:ILEN-57)//
     &		'/SYSTEM command can be used to reread these messages.'
	ELSE
	   ILEN = 48 + INDEX(COMMAND_PROMPT,'>') - 1
	   S1 = (80-ILEN)/2
	   S2 = 80 - S1 - ILEN
	   WRITE(6,1035) 'The '//COMMAND_PROMPT(:ILEN-48)//
     &			' command can be used to read these messages.'
	END IF

	RETURN

1020	FORMAT(' Description',43X,'From',9X,'Date',3X,'Number')
1025	FORMAT(' -----------',43X,'----',9X,'----',3X,'------')
1026	FORMAT(' ',<S1>('*'),A,' System Messages',<S2>('*'),A1)
1027	FORMAT(/,' ',<S1>('*'),A,<S2>('*'),A1)
1028	FORMAT('+',<S1>('*'),A,<S2>('*'),A1)
1030	FORMAT(' ',80('*'))
1035	FORMAT(' ',<S1>('*'),A,<S2>('*'))
1040	FORMAT(A54,1X,A12,1X,A6,1X,I4)
1060	FORMAT(A)
1070	FORMAT(' ERROR: Cannot add new entry to user file.')
1080	FORMAT(' ',/)

	END
