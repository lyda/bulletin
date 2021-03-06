C
C  BULLETIN0.FOR, Version 4/28/91
C  Purpose: Bulletin board utility program.
C  Environment: VAX/VMS
C  Usage: Invoked by the BULLETIN command.
C  Programmer: Mark R. London
C
C  Copyright (c) 1990
C  Property of Massachusetts Institute of Technology, Cambridge MA 02139.
C  This program cannot be copied or distributed in any form for non-MIT
C  use without specific written approval of MIT Plasma Fusion Center
C  Management.
C
	SUBROUTINE DELETE_MSG
C
C  SUBROUTINE DELETE_MSG
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
 
	CHARACTER ANSWER*1,REMOTE_USER*12,SUBJECT*53
 
	INTEGER NOW(2)
 
	IMMEDIATE = 0
	IF (CLI$PRESENT('IMMEDIATE')) IMMEDIATE = 1
 
	IF (CLI$PRESENT('NODES')) THEN	! Delete messages on DECNET node?
	   CALL DELETE_NODE		! Yes...
	   RETURN
	ELSE IF (DECNET_PROC) THEN	! Is this from remote node?
	   IER = CLI$GET_VALUE('SUBJECT',SUBJECT,SLEN)
	   CALL STR$UPCASE(SUBJECT,SUBJECT)
	   CALL OPEN_BULLDIR
	   CALL READDIR(0,IER)
	   DEL_BULL = 0
	   IER = 1
	   DO WHILE (DEL_BULL+1.EQ.IER)
	      DEL_BULL = DEL_BULL + 1
	      CALL READDIR(DEL_BULL,IER)
	      CALL STR$UPCASE(DESCRIP,DESCRIP)
	      IF (DEL_BULL+1.EQ.IER.AND.USERNAME.EQ.FROM
     &		   .AND.INDEX(DESCRIP,SUBJECT(:SLEN)).GT.0) THEN
	         CALL REMOVE_ENTRY(DEL_BULL,DEL_BULL,DEL_BULL,IMMEDIATE)
		 CALL CLOSE_BULLDIR
	         WRITE (5,'(''END'')')	! Tell DECNET that delete went ok.
		 RETURN
	      END IF
	   END DO
	   CALL CLOSE_BULLDIR		! Specified message not found,
	   WRITE(ERROR_UNIT,1030)	! so error out.
	   RETURN
	END IF
 
C
C  Get the bulletin number to be deleted.
C
 
	IER = CLI$GET_VALUE('BULLETIN_NUMBER',BULL_PARAMETER,LEN_P)
	IF (IER.NE.%LOC(CLI$_ABSENT)) THEN	! Was bulletin specified?
	   CALL GET_2_VALS(BULL_PARAMETER,LEN_P,SBULL,EBULL,IER)
	ELSE IF (CLI$PRESENT('ALL')) THEN
	   SBULL = 1
	   EBULL = F_NBULL
	   IER = 0
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
	ELSE IF (EBULL.GT.F_NBULL.AND..NOT.REMOTE_SET.AND.
     &						SBULL.NE.EBULL) THEN
	   WRITE (6,'('' Last message specified > number in folder.'')')
	   WRITE (6,'('' Do you want to delete to end of folder? '',$)')
	   READ (5,'(A)',IOSTAT=IER) ANSWER
	   CALL STR$UPCASE(ANSWER,ANSWER)
	   IF (ANSWER.NE.'Y') THEN
	      WRITE (6,'('' Deletion aborted.'')')
	      RETURN
	   ELSE
	      EBULL = F_NBULL
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
	   CALL REMOTE_DELETE(SBULL,IMMEDIATE,DESCRIP,I,FOLDER1_COM,IER)
	   IF (IER.EQ.0.AND.REMOTE_SET.NE.3) THEN
	      IF (I.EQ.LEN(FOLDER1_COM)) THEN
	         IER = SYS$ASCTIM(,INPUT,F1_NEWEST_BTIM,)
	         NEWEST_EXDATE = INPUT(1:11)
	         NEWEST_EXTIME = INPUT(13:)
	         NBULL = F1_NBULL
	   	 CALL UPDATE_FOLDER
	      ELSE
	  	 WRITE (6,'(1X,A)') FOLDER1_COM(:I)
	      END IF
	   ELSE IF (IER.NE.0) THEN
	      CALL DISCONNECT_REMOTE
	   END IF
	   RETURN
	END IF
 
	CALL OPEN_BULLDIR
 
	CALL READDIR(0,IER)
 
	DO BULL_DELETE = SBULL,EBULL
	   CALL READDIR(BULL_DELETE,IER)	! Get info for bulletin
 
	   IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?
	      WRITE(6,1030)			! If not, then error out
	      CALL CLOSE_BULLDIR
	      RETURN
	   END IF
 
	   IF (USERNAME.NE.FROM) THEN	! If doesn't match owner of bulletin,
	      CALL STR$UPCASE(REMOTE_USER,FROM)
	      IF ((.NOT.SETPRV_PRIV().AND..NOT.FOLDER_SET).OR.    ! Privileges?
     &	       (.NOT.FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)
     &		.AND.FOLDER_SET)) THEN
	         WRITE(6,1040)		! No, then error out.
	         CALL CLOSE_BULLDIR
		 RETURN
	      ELSE IF (SBULL.EQ.EBULL.AND.REMOTE_USER.EQ.FROM) THEN
	         CALL CLOSE_BULLDIR
	         WRITE (6,1050)	! Make sure user wants to delete it
	         READ (5,'(A)',IOSTAT=IER) ANSWER
	         CALL STR$UPCASE(ANSWER,ANSWER)
	         IF (ANSWER.NE.'Y') RETURN
	         CALL OPEN_BULLDIR
	         CALL READDIR(BULL_DELETE,IER)
	         IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?
	            WRITE(6,1030)	! If not, then error out
	            CALL CLOSE_BULLDIR
		    RETURN
	         END IF
	      END IF
	   END IF
 
C
C  Delete the bulletin directory entry.
C
	   CALL REMOVE_ENTRY(BULL_DELETE,SBULL,EBULL,IMMEDIATE)
	END DO
 
	CALL CLOSE_BULLDIR
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
 
	INTEGER NOW(2)
 
	IF (IMMEDIATE.EQ.1) THEN		! Delete it immediately
 
	   CALL DELETE_ENTRY(BULL_DELETE)	! Delete the directory entry
 
	   IF ((SYSTEM.AND.4).EQ.4) THEN	! Was entry shutdown bulletin?
	      SHUTDOWN = SHUTDOWN - 1		! Decrement shutdown count
	   END IF
	ELSE				! Delete it eventually
C
C  Change year of expiration date of message to 100 years less,
C  to indicate that message is to be deleted.  Then, set expiration date
C  in header of folder to 15 minutes from now.  Thus, the folder will be
C  checked in 15 minutes (or more), and will delete the messages then.
C
C  NOTE: If some comic set their expiration date to > 1999, then
C  the deleted date will be set to 1899 since can't specify date <1859.
C
 
	   IF ((SYSTEM.AND.7).LE.1) THEN	! General or System message
	      EXDATE = EXDATE(1:7)//'18'//EXDATE(10:)
	      IF (EXDATE(10:10).LT.'6') EXDATE(10:11) = '99'
	   ELSE				! Permanent or Shutdown
	      IF (EXDATE(2:2).EQ.'-') THEN
	         EXDATE = EXDATE(1:6)//'19'//EXDATE(9:)
	      ELSE
	         EXDATE = EXDATE(1:7)//'19'//EXDATE(10:)
	      END IF
	   END IF
 
	   CALL WRITEDIR(BULL_DELETE,IER)	! Update message expiration date
 
	   IER = SYS$BINTIM('0 0:15',EX_BTIM)	! Get time 15 minutes from now
	   IER = SYS$GETTIM(NOW)
	   IER = LIB$SUBX(NOW,EX_BTIM,EX_BTIM)
	   IER = SYS$ASCTIM(,INPUT,EX_BTIM,)
 
	END IF
 
	IF (IMMEDIATE.NE.1.AND.BULL_DELETE.EQ.EBULL) THEN
	   CALL READDIR(0,IER)			! Get header
 
	   NEWEST_EXDATE = INPUT(1:11)		! and store new expiration date
	   NEWEST_EXTIME = INPUT(13:)
 
	   CALL WRITEDIR(0,IER)
	ELSE IF (BULL_DELETE.EQ.EBULL) THEN
	   CALL CLEANUP_DIRFILE(SBULL)	! Reorder directory file
 
	   CALL UPDATE_ALWAYS	! Somewhat a kludgey way of updating latest
				! bulletin and expired dates.
 
	   IF (SBULL.LE.BULL_POINT) THEN
	      IF (BULL_POINT.GT.EBULL) THEN
	         BULL_POINT = BULL_POINT - (EBULL - SBULL + 1)
	      ELSE
		 BULL_POINT = SBULL - 1
	      END IF
	   END IF		! Readjust where which bulletin to read next
				! if deletion causes messages to be moved.
	END IFr
 e
	RETURNn
	END
 i
 y
 o
 m
 
	SUBROUTINE GET_2_VALS(INPUT,ILEN,SVAL,EVAL,IER)
 L
	IMPLICIT INTEGER (A-Z)m
 :
	INCLUDE 'BULLFOLDER.INC'p
 g
	COMMON /POINT/ BULL_POINT
 s
	CHARACTER*(*) INPUT
 e
	DELIM = MAX(INDEX(INPUT,':'),INDEX(INPUT,'-'))a
 t
	IF (DELIM.EQ.0) THENe
	   DECODE(ILEN,'(I<ILEN>)',INPUT,IOSTAT=IER) SVAL
	   EVAL = SVALo
	ELSEl
	   DECODE(DELIM-1,'(I<DELIM-1>)',INPUT,IOSTAT=IER) SVAL
	   CALL STR$UPCASE(INPUT,INPUT)
	   IF (IER.NE.0) THEN
	      IF (INDEX('CURRENT',INPUT(:DELIM-1)).EQ.1) THEN
		 SVAL = BULL_POINT
		 IER = 0
	      END IFI
	   END IF
	   IF (IER.EQ.0) THEN
	      ILEN = ILEN - DELIM
	      DECODE(ILEN,'(I<ILEN>)',INPUT(DELIM+1:),IOSTAT=IER) EVAL_
	      IF (IER.NE.0) THENP
	         IF (INDEX('LAST',INPUT(DELIM+1:TRIM(INPUT))).EQ.1) THENE
		    EVAL = F_NBULL
		    IER = 0R
                 ELSE IF (INDEX('CURRENT',
     &                  INPUT(DELIM+1:TRIM(INPUT))).EQ.1) THEN
                    EVAL = BULL_POINT_
                    IER = 0T
                 END IFN
 	      END IF
	   END IF
	   IF (EVAL.LT.SVAL) IER = 2
	END IFN
 2
	RETURNI
	END
 =
 
  
	SUBROUTINE DIRECTORY(DIR_COUNT)
C=
C  SUBROUTINE DIRECTORY'
CE
C  FUNCTION: Display directory of messages.
C 
	IMPLICIT INTEGER (A - Z)
  
	INCLUDE 'BULLDIR.INC'
 O
	INCLUDE 'BULLUSER.INC'm
  
	INCLUDE 'BULLFOLDER.INC'A
 (
	COMMON /PAGE/ PAGE_LENGTH,PAGE_WIDTH,PAGING
	LOGICAL PAGING
  
	DATA SCRATCH_D1/0/ 
 A
	COMMON /POINT/ BULL_POINT
 L
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 )
	COMMON /TAGS/ BULL_TAG,READ_TAG
  
	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD
 D
	COMMON /CLOSE_FILES_INFO/ CLOSED_FILESQ
 R
	EXTERNAL CLI$_ABSENT,CLI$_NEGATED,CLI$_PRESENT,CLOSE_FILES(
 E
	CHARACTER START_PARAMETER*16,DATETIME*23,SEARCH_STRING*80
 E
	INTEGER TODAY(2)
 C
	CHARACTER*9 EXPIRES
  
        CHARACTER TIMBUF*13e
        DATA TIMBUF/'0 00:00:05.00'/
 
        INTEGER TIMADR(2)                       ! Buffer containing time
  
	DATA WAITEFN /0/R
 _
	IF (WAITEFN.EQ.0) CALL LIB$GET_EF(WAITEFN)I
        IER=SYS$BINTIM(TIMBUF,TIMADR)t
   d
	CALL LIB$ERASE_PAGE(1,1)		! Clear the screenN
 M
	IF (INCMD(:3).EQ.'DIR'.AND..NOT.READ_TAG) THEN_
	   SUBJECT = CLI$PRESENT('SUBJECT')
	   REPLY = CLI$PRESENT('REPLY')
	   REPLY_FIRST = REPLYR
	   SEARCH = CLI$PRESENT('SEARCH')
	   IF (.NOT.CLI$PRESENT('SELECT_FOLDER').AND.
     &		CLI$PRESENT('MARKED')) THENE
	      IF (FOLDER_NUMBER.GE.0) THEN?
		 READ_TAG = .TRUE.
		 CALL GET_FIRST_TAG(FOLDER_NUMBER,IER,BULL_POINT)L
	      ELSEe
		 WRITE (6,'('' ERROR: Cannot use /MARKED with'',
     &				'' remote folder.'')')
		 RETURNR
	      END IF 
	   END IF
	END IF 
 U
C
C  Directory listing is first buffered into temporary memory storage beforeU
C  being outputted to the terminal.  This is to be able to quickly close the
C  directory file, and to avoid the possibility of the user holding the screen,(
C  and thus causing the directory file to stay open.  The temporary memory
C  is structured as a linked-list queue, where SCRATCH_D1 points to the header
C  of the queue.  See BULLSUBS.FOR for more description of the queue. 
C 
 s
	CALL INIT_QUEUE(SCRATCH_D1,BULLDIR_ENTRY)
	SCRATCH_D = SCRATCH_D1l
  
	CALL OPEN_BULLDIR_SHARED		! Get directory file 
 F
	CALL READDIR(0,IER)			! Does directory header exist?R
	IF (IER.EQ.1.AND.NBULL.GT.0) THEN	! And are there messages?
	   IF (DIR_COUNT.EQ.0) THEN
	      EXPIRATION = CLI$PRESENT('EXPIRATION')
	      IF (CLI$PRESENT('START')) THEN	! Start number specified?(
	         IER = CLI$GET_VALUE('START',START_PARAMETER,ILEN)S
	         DECODE(ILEN,'(I<ILEN>)',START_PARAMETER) DIR_COUNT
		 IF (DIR_COUNT.GT.NBULL) THEN 
		    DIR_COUNT = NBULL 
		 ELSE IF (DIR_COUNT.LT.1) THEN
		    WRITE (6,'('' ERROR: Invalid starting message.'')') 
		    CALL CLOSE_BULLDIR
		    DIR_COUNT = 0E
		    RETURN
		 END IF 
	      ELSE IF (CLI$PRESENT('SINCE').OR.CLI$PRESENT('NEW')) THEN
		 IF (CLI$PRESENT('SINCE')) THEN		! Was /SINCE specified?
		   IER = CLI$GET_VALUE('SINCE',DATETIME)
	   	   IF (DATETIME.EQ.'TODAY') THEN	! TODAY is the default. 
	      	     IER = SYS$BINTIM('-- 00:00:00.00',TODAY) 
		     CALL GET_MSGKEY(TODAY,MSG_KEY) 
		   ELSE 
		     CALL SYS_BINTIM(DATETIME,MSG_BTIM)c
		     CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
		   END IFR
		   CALL READDIR_KEYGE(IER)
		 ELSE IF (CLI$PRESENT('NEW')) THEN	! was /NEW specified?
		   IF (REMOTE_SET.NE.3) THEN
	   	     DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &			       F_NEWEST_BTIM)
		     IF (DIFF.GE.0) THEN
		       WRITE (6,'('' No new messages are present in'',
     &			'' folder '',A,''.'')') FOLDER(:TRIM(FOLDER))
		       CALL CLOSE_BULLDIR!
		       RETURNa
		     ELSE 
		       CALL GET_MSGKEY(LAST_READ_BTIM(1,FOLDER_NUMBER+1),L
     &							MSG_KEY)E
		     END IFF
		     CALL READDIR_KEYGE(IER)
		   ELSEE
		     CALL NEWS_GET_NEWEST_MESSAGE(IER)
		     IF (IER.EQ.0) THENE
		       WRITE (6,'('' No new messages are present in'',
     &			 '' folder '',A,''.'')')o
     &			 FOLDER_NAME(:TRIM(FOLDER_NAME))	
		       RETURN 
		     END IF 
		   END IF 
		 END IF
 
		 IF (IER.EQ.0) THENn
		    WRITE (6,'('' No messages past specified date.'')')B
		    CALL CLOSE_BULLDIR
		    RETURN
		 ELSEU
		    DIR_COUNT = IER0
		 END IF 
	      ELSEe
	         DIR_COUNT = BULL_POINT
		 IF (DIR_COUNT.EQ.0) DIR_COUNT = 1
	      END IFc
 o
	      IF (CLI$PRESENT('SEARCH')) THEN
		 IER1 = CLI$GET_VALUE('SEARCH',SEARCH_STRING,SLEN)
	      ELSE IF (CLI$PRESENT('SUBJECT')) THEN
		 IER1 = CLI$GET_VALUE('SUBJECT',SEARCH_STRING,SLEN)e
	      ELSE IF (CLI$PRESENT('REPLY')) THEN
		 SEARCH_STRING = ' '
	      END IF 
 e
	      IF (READ_TAG) THEN)
	         IF (SUBJECT.OR.REPLY.OR.SEARCH) THEN
		    WRITE (6,'('' ERROR: Qualifier not valid when '',E
     &			''displaying only MARKED messages.'')')
	            SUBJECT = .FALSE.
	            REPLY = .FALSE.
	      	    SEARCH = .FALSE.
		    CALL CLOSE_BULLDIR
		    RETURN
		 END IFt
	         IF (.NOT.(CLI$PRESENT('SINCE').OR.CLI$PRESENT('NEW')
     &			.OR.CLI$PRESENT('START'))) THEN
	            DIR_COUNT = 1
		 END IFn
		 CALL READDIR(DIR_COUNT,IER1)l
		 IF (IER1.EQ.DIR_COUNT+1) IER1 = 0
		 I = 1
		 DO WHILE (I.LT.9)
		    ITEST = ICHAR(MSG_KEY(I:I))t
	            IF (ITEST.GT.0) THEN,
		       MSG_KEY(I:I) = CHAR(ITEST-1)o
		       I = 9
		    ELSE
		       I = I + 1
		    END IF
		 END DOm
	      END IF)
 n
	      IF (CLI$PRESENT('SINCE').OR.CLI$PRESENT('NEW')) THENs
		 SBULL = DIR_COUNT
	         EBULL = DIR_COUNT + (PAGE_LENGTH - 7) - 1o
	         IF (EBULL.GE.NBULL-2) EBULL = NBULL
	      ELSE IF (NBULL-DIR_COUNT+1.LE.PAGE_LENGTH-5) THEN
	         EBULL = NBULLA
	         SBULL = NBULL - (PAGE_LENGTH-5) + 10
	         IF (SBULL.LT.1) SBULL = 1 
	      ELSEe
	         SBULL = DIR_COUNTI
	         EBULL = DIR_COUNT + (PAGE_LENGTH - 7) - 1T
	      END IFX
	   ELSE IF (DIR_COUNT.EQ.-1.AND..NOT.READ_TAG) THEN
	      SUBJECT = .FALSE.
	      REPLY = .FALSE.
	      SEARCH = .FALSE.L
	      SBULL = (SBULL - 1) - ((PAGE_LENGTH - 7) - 1)
	      IF (SBULL.LT.1) SBULL = 1
	      EBULL = SBULL + (PAGE_LENGTH - 7) - 1
	      IF (NBULL-SBULL+1.LE.PAGE_LENGTH-5) THENE
	         SBULL = NBULL - (PAGE_LENGTH-5) + 1
	         EBULL = NBULLI
	         IF (SBULL.LT.1) SBULL = 1L
	      END IFL
	   ELSE IF (DIR_COUNT.EQ.-1.AND.READ_TAG) THEND
	      CALL GET_FIRST_TAG(FOLDER_NUMBER,IER,FIRST_BULL)E
	      FIRST_BULL = FIRST_BULL + 1
	      IER1 = 0)
	      IER = 0
	      FBULL = 0
	      DO WHILE (SBULL.GT.FIRST_BULL.AND.IER.EQ.0)
		 SBULL = SBULL - 1
	         CALL READDIR(SBULL,IER)e
		 IF (IER.EQ.SBULL+1) THEN	
	            CALL GET_THIS_TAG(FOLDER_NUMBER,IER,DIR_COUNT)N
		    IF (IER.EQ.0) THEN
		       IF (FBULL.EQ.0) THEN 
		          EBULL = DIR_COUNTL
		          FBULL = EBULL + 1	
		       END IFL
		       FBULL = FBULL - 1
		       IF (EBULL-FBULL.EQ.(PAGE_LENGTH-7)-1) THEN
		          IER = 1s
		       END IF 
		    ELSE
		       IER = 0
		    END IF
		 ELSE
		    IER = 1 
		 END IFI
	      END DOE
	      IF (FBULL.EQ.FIRST_BULL) THEN
		 CALL READDIR(EBULL,IER)
		 CALL GET_NEXT_TAG(FOLDER_NUMBER,IER,DIR_COUNT)R
		 DO WHILE (IER.EQ.0.AND.EBULL-FBULL.LT.(PAGE_LENGTH-7)-1))
		    CALL GET_NEXT_TAG(FOLDER_NUMBER,IER,DIR_COUNT)
		    IF (IER.EQ.0) EBULL = EBULL + 1=
		 END DOS
		 DO I=1,3E
		    CALL GET_NEXT_TAG(FOLDER_NUMBER,IER,DIR_COUNT)
		 END DOS
		 IF (IER.NE.0) EBULL = DIR_COUNT
	      END IFI
	      CALL READDIR(EBULL,IER)
	      IF (EBULL+1.NE.IER) THEN
		 EBULL = EBULL + 1
	      ELSE 
	         CALL GET_NEXT_TAG(FOLDER_NUMBER,IER,DUMMY)
		 IF (IER.NE.0) EBULL = EBULL + 1
	      END IFS
	      CALL READDIR(SBULL,IER)
	      I = 1
	      DO WHILE (I.LT.9)
		 ITEST = ICHAR(MSG_KEY(I:I))
	         IF (ITEST.GT.0) THEN
		    MSG_KEY(I:I) = CHAR(ITEST-1)
		    I = 9R
		 ELSE 
		    I = I + 1 
		 END IFE
	      END DOT
	   ELSE
	      SBULL = DIR_COUNT
	      EBULL = DIR_COUNT + (PAGE_LENGTH - 7) - 1
	      IF (EBULL.GE.NBULL-2) EBULL = NBULL
	   END IF
	   IF (.NOT.PAGING) THEN
	      EBULL = NBULL
	   END IF
	   IF (SUBJECT.OR.REPLY.OR.SEARCH) THEN
	      CONTINUEN
	   ELSE IF (.NOT.REMOTE_SET.AND..NOT.READ_TAG) THEN
	      DO I = SBULL,EBULLE
	         CALL READDIR(I,IER)		! Into the queueC
	         CALL WRITE_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,BULLDIR_ENTRY)G
	      END DOP
	   ELSE IF (READ_TAG) THENG
	      I = 0
	      DO WHILE (I.LE.EBULL.AND.IER1.EQ.0)
		 CALL GET_NEXT_TAG(FOLDER_NUMBER,IER1,DIR_COUNT)
		 IF (I.EQ.0.AND.IER1.EQ.0) THENE
		    SBULL = DIR_COUNTA
		    I = SBULLC
		 END IF2
	         CALL WRITE_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,BULLDIR_ENTRY)L
		 I = I + 1
	      END DOR
	      EBULL = I - 1
	      IF (IER1.NE.0) THEN
	         EBULL = EBULL - 1*
	      ELSEG
	         CALL GET_NEXT_TAG(FOLDER_NUMBER,IER1,DUMMY)R
		 IF (IER1.EQ.0) THEN
		    IER = 00
		    EBULL_SAVE = EBULL
		    DO I=1,2
		       IF (IER.EQ.0) THENu
	                  CALL WRITE_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,
     &							BULLDIR_ENTRY)I
			  EBULL = EBULL + 1
	                  CALL GET_NEXT_TAG(FOLDER_NUMBER,IER,DUMMY)s
		       END IFI
		    END DO
		    IF (IER.NE.0) THEN
	               CALL GET_FIRST_TAG(FOLDER_NUMBER,IER,FIRST_BULL)
	               IF (SBULL.NE.FIRST_BULL+1) EBULL = EBULL_SAVEE
		       IER1 = 1T
		    ELSE
		       EBULL = EBULL_SAVE&
		    END IF
		 END IFT
	      END IF 
	   ELSE
	      CALL REMOTE_DIRECTORY_COMMAND
     &				     (SBULL,EBULL,.FALSE.,SCRATCH_D,IER)
	      IF (IER.NE.0) THEN(
	         CALL CLOSE_BULLDIR
		 CALL DISCONNECT_REMOTEm
		 RETURN'
	      END IFR
	   END IF
	ELSE 
	   NBULL = 0I
	END IF
  
	IF (NBULL.EQ.0.OR.EBULL.LT.SBULL) THEN 
	   CALL CLOSE_BULLDIR			! We don't need file anymore 
	   WRITE (6,'('' There are no messages present.'')')
	   RETURN
	END IFd
  
Ci
C  Directory entries are now in queue.  Output queue entries to screen.h
Ci
 t
	FLEN = TRIM(FOLDER_NAME) 
	WRITE(6,'(<PAGE_WIDTH-FLEN+1>X,A)') FOLDER_NAME(:FLEN),
	IF (EXPIRATION) THENs
	   WRITE(6,1005)				! Write header 
	ELSES
	   WRITE(6,1000)				! Write headere
	END IFs
 C
	IF (.NOT.(SUBJECT.OR.REPLY.OR.SEARCH).AND.R
     &		BULL_TAG.AND..NOT.READ_TAG) THEN
	   IF (INCMD(1:3).NE.'   ') THENF
	      SCRATCH_D = SCRATCH_D1		! Init queue pointer to headerI
	      CALL READ_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,BULLDIR_ENTRY)I
	      CALL GET_THIS_OR_NEXT_TAG(FOLDER_NUMBER,IER,NEXT_TAG)
	      IF (IER.NE.0) NEXT_TAG = NBULL + 1	
	   END IF
	   SCRATCH_D = SCRATCH_D1		! Init queue pointer to header
	   DO I=SBULL,EBULL
	      SAVE_SCRATCH_D = SCRATCH_D_
	      CALL READ_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,BULLDIR_ENTRY)_
	      IF (BULL_TAG.AND.MSG_NUM.EQ.NEXT_TAG) THEN	
		 MSG_NUM = -MSG_NUM:
	         CALL WRITE_QUEUE(%VAL(SAVE_SCRATCH_D),DUMMY,BULLDIR_ENTRY)
	         CALL GET_NEXT_TAG(FOLDER_NUMBER,IER,NEXT_TAG)$
	         IF (IER.NE.0) NEXT_TAG = NBULL + 1
	      END IFE
	   END DO
	END IFW
 /
	CALL CLOSE_BULLDIR			! We don't need file anymore
 T
	SCRATCH_D = SCRATCH_D1			! Init queue pointer to header
 f
	I = SBULL
	START_SEARCH = IB
	IF (.NOT.REPLY_FIRST) START_SEARCH = I - 1E
	IF (SUBJECT.OR.REPLY.OR.SEARCH) THEN	
	   CALL OPEN_BULLDIR_SHARED
	   IF (SEARCH) CALL OPEN_BULLFIL_SHARED
	   CLOSED_FILES = .FALSE.
	END IFE
	DO WHILE (I.LE.EBULL)
	   IF (.NOT.(SUBJECT.OR.REPLY.OR.SEARCH)) THENe
	      CALL READ_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,BULLDIR_ENTRY)(
	   ELSE
	      IF (CLOSED_FILES) THEN&
		 CLOSED_FILES = .FALSE.	
		 CALL OPEN_BULLDIR_SHARED 
		 IF (SEARCH) CALL OPEN_BULLFIL_SHAREDe
	      END IF 
 	      CALL GET_SEARCH(FOUND,SEARCH_STRING,START_SEARCH,.FALSE.
     &		    ,SUBJECT,REPLY_FIRST,.FALSE.,.TRUE.)
	      IF (INCMD(1:3).NE.'   '.AND.BULL_TAG.AND.FOUND.GT.0) THEN
	         CALL GET_THIS_OR_NEXT_TAG(FOLDER_NUMBER,IER,NEXT_TAG)
	         IF (IER.NE.0) NEXT_TAG = NBULL + 1
		 CALL READDIR(FOUND,IER)
	      END IF 
	      REPLY_FIRST = .FALSE.
	      IF (FOUND.GT.0) THEN 
		 SEARCH_STRING = ' '
		 START_SEARCH = FOUNDT
	         IF (BULL_TAG.AND.MSG_NUM.EQ.NEXT_TAG) THEN
	            CALL GET_NEXT_TAG(FOLDER_NUMBER,IER,NEXT_TAG)
	            IF (IER.NE.0) NEXT_TAG = NBULL + 1'
		    CALL READDIR(FOUND,IER)	
		    MSG_NUM = -MSG_NUM
	         END IF
	      ELSE
		 I = EBULL + 1
	      END IF 
              IER = SYS$SETIMR(%VAL(WAITEFN),TIMADR,CLOSE_FILES,)
	   END IF
	   IF (I.LE.EBULL) THEN
	      CALL CONVERT_ENTRY_FROMBINC
	      IF (MSG_NUM.LT.0.OR.READ_TAG) THEN$
	         WRITE (6,'('' *'',$)')
	         IF (MSG_NUM.LT.0) MSG_NUM = -MSG_NUM
	      ELSEF
	         WRITE (6,'(''  '',$)')
	      END IF'
	      N = MAX(INT(LOG10(REAL(MSG_NUM)))+1,3)E
	      IF ((EXDATE(8:9).EQ.'18'.OR.INDEX(EXDATE,'1900').GT.0) 
     &		  .AND.REMOTE_SET.NE.3) THEN
	         WRITE(6,2010) MSG_NUM,DESCRIP(:55-N),FROM,'(DELETED)' 
	      ELSE IF (EXPIRATION) THEN
	         IF ((SYSTEM.AND.4).EQ.4) THEN		! Shutdown bulletin?O
		    EXPIRES = 'Shutdown'
	         ELSE IF ((SYSTEM.AND.2).EQ.2) THEN	! Permanent bulletin?
		    EXPIRES = 'Permanent'T
	         ELSE IF (EXDATE(8:9).EQ.'18'.AND.REMOTE_SET.EQ.3) THEN
		    EXPIRES = 'Unknown'	
	         ELSE
		    EXPIRES = EXDATE(1:7)//EXDATE(10:11)
	         END IF
	         WRITE(6,2010) MSG_NUM,DESCRIP(:55-N),FROM,EXPIRES
	      ELSEK
	         WRITE(6,2010) MSG_NUM,DESCRIP(:55-N),FROM,
     &						DATE(1:7)//DATE(10:11)
	      END IF 
	   END IF
	   I = I + 1I
	   IF (SUBJECT.OR.REPLY.OR.SEARCH) IER = SYS$CANTIM(,)=
	END DOT
  
	DIR_COUNT = MSG_NUM + 1			! Update directory counter 
  
	IF (SEARCH.OR.REPLY.OR.SUBJECT) THEN
	   IF (SEARCH) CALL CLOSE_BULLFIL
	   CALL CLOSE_BULLDIR
	   IF (FOUND.GT.0) THEN
	      DIR_COUNT = FOUND + 1
	   ELSE
	      DIR_COUNT = NBULL + 1
	   END IF
	END IFE
 e
	IF (DIR_COUNT.GT.NBULL.OR.(READ_TAG.AND.IER1.NE.0)) THEN 
						! Outputted all entries?
	   DIR_COUNT = -1			! Yes. Set counter to -1.
	ELSEG
	   WRITE(6,1010)			! Else say there are more=
	END IF
  
	RETURN 
 F
1000	FORMAT('    #',1X,'Description',43X,'From',9X,'Date',/)
1005	FORMAT('    #',1X,'Description',43X,'From',8X,'Expires',/)N
1010	FORMAT(1X,/,' Press RETURN for more...',/)N
 -
2010	FORMAT('+',I<N>,1X,A<55-N>,1X,A12,1X,A9)H
  
	END
  
  
	SUBROUTINE CLOSE_FILES 
 F
	IMPLICIT INTEGER (A-Z)
  
	COMMON /CLOSE_FILES_INFO/ CLOSED_FILESA
 R
	INQUIRE(UNIT=1,OPENED=IER)E
	IF (IER) CALL CLOSE_BULLFIL
 S
	INQUIRE(UNIT=2,OPENED=IER) 
	IF (IER) CALL CLOSE_BULLDIR
 
	CLOSED_FILES = .TRUE.
 L
	RETURN 
	END
 L
 S
 L
	SUBROUTINE GET_MSGKEY(BTIM,MSG_KEY)
 S
	IMPLICIT INTEGER (A-Z)R
 D
	INTEGER BTIM(2)
  
	CHARACTER*8 MSG_KEY,INPUT
  
	CALL LIB$MOVC3(8,BTIM(1),%REF(INPUT))
 R
	DO I=1,8	
	   MSG_KEY(I:I) = INPUT(9-I:9-I) 
	END DO.
 T
	RETURN 
	END
 B
  
 I
	SUBROUTINE FILE
C 
C  SUBROUTINE FILE
C 
C  FUNCTION:  Copies a bulletin to a file.
C 
	IMPLICIT INTEGER (A - Z)_
 G
	COMMON /POINT/ BULL_POINT
  
	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER
 S
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
  
	INCLUDE 'BULLDIR.INC'
  
	INCLUDE 'BULLFOLDER.INC',
 )
	EXTERNAL CLI$_ABSENTF
 E
	IER = CLI$GET_VALUE('BULLETIN_NUMBER',BULL_PARAMETER,LEN_P)
	IF (IER.NE.%LOC(CLI$_ABSENT)) THEN	! Was bulletin specified?,
	   CALL GET_2_VALS(BULL_PARAMETER,LEN_P,SBULL,EBULL,IER)E
	   IF (EBULL.GT.F_NBULL) EBULL = F_NBULLA
	ELSE IF (CLI$PRESENT('ALL')) THEN
	   SBULL = 1I
	   EBULL = F_NBULLC
	   IER = 0E
	ELSE IF (BULL_POINT.EQ.0) THEN	! No.  Have we just read a bulletin?
	   WRITE(6,1010)		! No, then error.
	   RETURN
	ELSET
	   SBULL = BULL_POINT
	   EBULL = SBULLI
	   IER = 0L
	END IF+
 
	IF (SBULL.LE.0.OR.IER.NE.0) THEN(
	   WRITE (6,1015)
	   RETURN
	END IF 
 L
	IER = CLI$GET_VALUE('FILESPEC',BULL_PARAMETER,LEN_P)S
 T
	IF (IER.EQ.%LOC(CLI$_ABSENT)) THEN	! If no file name was specified
	   WRITE(6,1020)		! Write error
	   RETURN			! And return 
	END IFD
 C
	CALL DISABLE_PRIVSD
 C
	IF (CLI$PRESENT('NEW')) THEN 
	   OPEN(UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),ERR=900, 
     &	      RECL=LINE_LENGTH,STATUS='NEW',CARRIAGECONTROL='LIST')
	ELSE.
	   OPEN(UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),IOSTAT=IER,
     &		RECL=LINE_LENGTH,D
     &		STATUS='OLD',CARRIAGECONTROL='LIST',ACCESS='APPEND')
	   IF (IER.NE.0) THEN
	      OPEN(UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),ERR=900,
     &	         RECL=LINE_LENGTH,STATUS='NEW',CARRIAGECONTROL='LIST')=
	   ELSE IF (CLI$PRESENT('FF')) THEN
	      WRITE (3,'(A)') CHAR(12)L
	   END IF
	END IFO
 )
	CALL ENABLE_PRIVS			! Reset SYSPRV privileges
  
	HEAD = CLI$PRESENT('HEADER')	
 D
	CALL OPEN_BULLDIR_SHAREDQ
	CALL OPEN_BULLFIL_SHARED	! Open BULLETIN file
 I
	FIRST = .TRUE.E
 D
	DO FBULL = SBULL,EBULL
	   FBULL1 = FBULL
	   CALL READDIR(FBULL,IER)	! Get info for specified bulletinA
 G
	   IF (IER.NE.FBULL+1.OR.FBULL.GT.EBULL.OR.(.NOT.CLI$PRESENT
     &		('ALL').AND.FBULL1.EQ.SBULL.AND.FBULL.NE.SBULL)) THEN 
	      IF (REMOTE_SET.NE.3.OR.FBULL1.EQ.SBULL) WRITE(6,1030) FBULL1T
	      IF (FBULL1.GT.SBULL) GO TO 100E
	      CLOSE (UNIT=3,STATUS='DELETE') 
	      CALL CLOSE_BULLFILG
	      CALL CLOSE_BULLDIR
	      RETURNI
	   ELSE IF (REMOTE_SET) THENE
	      CALL REMOTE_READ_MESSAGE(FBULL,IER1)F
	      IF (IER1.GT.0) THEN
	         CALL DISCONNECT_REMOTE
	      ELSEU
	         CALL GET_REMOTE_MESSAGE(IER1) 
	      END IF 
	      IF (IER1.NE.0) GO TO 100F
	   END IF
  
	   IF (.NOT.FIRST.AND.CLI$PRESENT('FF')) THEN
	      WRITE (3,'(A)') CHAR(12)L
	   ELSE IF (FIRST) THEN
	      FIRST = .FALSE.
	   END IF
  
	   ILEN = LINE_LENGTH + 1
  
	   CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN) 
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN 
	      IF (HEAD) WRITE(3,1060) INPUT(7:ILEN),DATE//' '//TIME(:8)
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   ELSE IF (HEAD) THEN'
	      WRITE(3,1060) FROM,DATE//' '//TIME(:8)y
	   END IF
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THENi
	      IF (HEAD) WRITE(3,1050) INPUT(7:ILEN)
	   ELSE
	      IF (HEAD) WRITE(3,1050) DESCRIP
	      IF (ILEN.GT.0) WRITE (3,'(A)') INPUT(:ILEN)
	   END IF
  
	   DO WHILE (ILEN.GT.0)		! Copy bulletin into file 
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      IF (ILEN.GT.0) WRITE (3,'(A)') INPUT(1:ILEN).
	   END DO
	END DOS
 T
100	CLOSE (UNIT=3)			! Bulletin copy completed
 
	WRITE(6,1040) BULL_PARAMETER(1:LEN_P)
					! Show name of file created.L
	CALL CLOSE_BULLFILG
	CALL CLOSE_BULLDIRX
 A
	RETURN 
  
900	WRITE(6,1000) 
	CALL ENABLE_PRIVS		! Reset BYPASS privilegesC
	RETURNn
 q
1000	FORMAT(' ERROR: Error in opening file.') 
1010	FORMAT(' ERROR: You have not read any bulletin.')
1015	FORMAT(' ERROR: Specified message number has incorrect format.')_
1020	FORMAT(' ERROR: No file name was specified.')
1030	FORMAT(' ERROR: Following bulletin was not found: ',I)R
1040	FORMAT(' Message(s) written to ',A)
1050	FORMAT('Description: ',A,/)
1060	FORMAT(/,'From: ',A,/,'Date: ',A)
 I
	END
 N
 O
 E
 I
	SUBROUTINE LOGIN_
CL
C  SUBROUTINE LOGIN 
Ce
C  FUNCTION: Alerts user of new messages upon logging in.r
C 
	IMPLICIT INTEGER (A - Z)A
 S
	INCLUDE 'BULLDIR.INC'
 _
	INCLUDE 'BULLUSER.INC' 
 
	INCLUDE 'BULLFOLDER.INC'A
 )
	COMMON /READIT/ READITL
 _
	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER
 S
	COMMON /PAGE/ PAGE_LENGTH,PAGE_WIDTH,PAGING
	LOGICAL PAGINGR
 Y
	COMMON /POINT/ BULL_POINT
  
	COMMON /PROMPT/ COMMAND_PROMPTH
	CHARACTER*39 COMMAND_PROMPT
  
	COMMON /SYSTEM_FOLDERS/ SYSTEM_FLAG(FLONG),DUMMY(2)
 	
	COMMON /COMMAND_SWITCHES/ LOGIN_SWITCH,SYSTEM_SWITCHL
	COMMON /COMMAND_SWITCHES/ SYSTEM_LOGIN_BTIM(2)A
	COMMON /COMMAND_SWITCHES/ REVERSE_SWITCH,SEPARATE
	CHARACTER*1 SEPARATER
 .
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 B
	CHARACTER TODAY*23,INREAD*1
  
	LOGICAL*1 CTRL_G/7/
 X
	DATA GEN_DIR1/0/	! General directory link list header
	DATA SYS_DIR1/0/	! System directory link list header 
	DATA SYS_NUM1/0/	! System message number link list header
	DATA SYS_BUL1/0/	! System bulletin link list header
	DATA ALL_DIR1/0/	! Full directory link list header (for remote)
  
	DATA PAGE/0/_
 T
	DATA FIRST_WRITE/.TRUE./_
	LOGICAL FIRST_WRITE
 R
	COMMON /LOGIN_BTIM/ LOGIN_BTIM_SAVE(2)R
 D
	DIMENSION NOLOGIN_BTIM(2),TODAY_BTIM(2)
	DIMENSION NEW_BTIM(2),PASSCHANGE(2),BULLCP_BTIM(2) 
	DIMENSION LOGIN_BTIM_OLD(2),LOGIN_BTIM_NEW(2)
 T
	FOLDER_NAME = 'GENERAL'
  
	CALL SYS$ASCTIM(,TODAY,,)		! Get the present time
	CALL SYS_BINTIM(TODAY,TODAY_BTIM)
 L
	CALL SYS_BINTIM('5-NOV-2956 00:00:00.00',NOLOGIN_BTIM)
	CALL SYS_BINTIM('5-NOV-1956 11:05:56',NEW_BTIM)
  
CS
C  Find user entry in BULLUSER.DAT to update information and
C  to get the last date that messages were read.
CX
 E
	CALL OPEN_BULLUSER_SHARED
 1
	CALL READ_USER_FILE_HEADER(IER)		! Get the header
  
	IF (IER.EQ.0) THEN			! Header is present.
	   UNLOCK 4
	   CALL READ_USER_FILE_KEYNAME(USERNAME,IER1)
						! Find if there is an entryu
	   IF (NEW_FLAG(1).LT.143.OR.NEW_FLAG(1).GT.143) THEN
	      NEW_FLAG(2)=0		! If old version clear GENERIC value
	      NEW_FLAG(1)=143		! Set new version number
	   END IF
	   IF (IER1.EQ.0) THEN			! There is a user entryk
	      IF (COMPARE_BTIM(LOGIN_BTIM,NOLOGIN_BTIM).GE.0) THEN0
						! DISMAIL or SET LOGIN set
		 IF (CLI$PRESENT('ALL')) THEN-
		    LOGIN_BTIM(1) = TODAY_BTIM(1) 
		    LOGIN_BTIM(2) = TODAY_BTIM(2)-
		 ELSE
		    RETURN			! Don't notify1
	         END IF
	      END IF 
	      LOGIN_BTIM_SAVE(1) = LOGIN_BTIM(1)E
	      LOGIN_BTIM_SAVE(2) = LOGIN_BTIM(2)D
	      LOGIN_BTIM(1) = TODAY_BTIM(1)
	      LOGIN_BTIM(2) = TODAY_BTIM(2)
	      REWRITE (4) USER_ENTRYE
	      IF (SYSTEM_FLAG(1).NE.0.AND.SYSTEM_FLAG(1).NE.1) READIT = 1
	      DO I = 1,FLONGT
		 IF (SET_FLAG(I).NE.0.OR.BRIEF_FLAG(I).NE.0.OR.1
     &		    (I.GT.1.AND.SYSTEM_FLAG(I).NE.0)) READIT = 1
	      END DO.
	   ELSE
	      CALL CLEANUP_LOGIN		! Good time to delete dead userss
	      READ_BTIM(1) = NEW_BTIM(1)		! Make new entryl
	      READ_BTIM(2) = NEW_BTIM(2) 
	      DO I = 1,FLONGR
	         SET_FLAG(I) = SET_FLAG_DEF(I)X
	         BRIEF_FLAG(I) = BRIEF_FLAG_DEF(I)o
		 NOTIFY_FLAG(I) = NOTIFY_FLAG_DEF(I)
	      END DOs
	      NEW_FLAG(1) = 143
	      NEW_FLAG(2) = 0
	      CALL CHECK_NEWUSER(USERNAME,DISMAIL,PASSCHANGE)
	      IF (DISMAIL.EQ.1) THENN
		 LOGIN_BTIM(1) = NOLOGIN_BTIM(1)
		 LOGIN_BTIM(2) = NOLOGIN_BTIM(2)
	         LOGIN_BTIM_SAVE(1) = LOGIN_BTIM(1)
	         LOGIN_BTIM_SAVE(2) = LOGIN_BTIM(2)
	      ELSEE
	         LOGIN_BTIM_SAVE(1) = NEW_BTIM(1)
	         LOGIN_BTIM_SAVE(2) = NEW_BTIM(2)
	         LOGIN_BTIM(1) = TODAY_BTIM(1) 
	         LOGIN_BTIM(2) = TODAY_BTIM(2)T
	         DO I = 1,FLONG
		    IF (SET_FLAG(I).NE.0) READIT = 1
	         END DO
		 IF (COMPARE_BTIM(PASSCHANGE,NEWEST_BTIM).LT.0) IER1 = 0
			! Old password change indicates user is new to BULLETIN
			! but not to system, so don't limit message viewing. 
	      END IF_
	      CALL WRITE_USER_FILE(IER)
	      IF (IER.NE.0) THEN		! Error in writing to user file
		 WRITE (6,1070)			! Tell user of the error
		 CALL CLOSE_BULLUSER		! Close the user file
		 CALL EXIT			! Go away...,
	      END IF 
	      IF (DISMAIL.EQ.1) RETURN		! Go away if DISMAIL setL
	      DIFF = -1				! Force us to look at messages
	      CALL OPEN_BULLINF_SHARED 
	      DO I=1,FOLDER_MAX
	         LAST_READ_BTIM(1,I) = READ_BTIM(1)
	         LAST_READ_BTIM(2,I) = READ_BTIM(2)
	      END DOH
	      WRITE (9,IOSTAT=IER) USERNAME,C
     &		((LAST_READ_BTIM(I,J),I=1,2),J=1,FOLDER_MAX)
	      CALL CLOSE_BULLINFi
	   END IF
	   LOGIN_BTIM(1) = LOGIN_BTIM_SAVE(1)
	   LOGIN_BTIM(2) = LOGIN_BTIM_SAVE(2)
	   CALL READ_USER_FILE_HEADER(IER2)	! Reset read back to header
	END IF(
  
	IF (IER.EQ.0.AND.MINUTE_DIFF(TODAY_BTIM,BBOARD_BTIM)G
     &			.GT.BBOARD_UPDATE) THEN	! Update BBOARD mail?
	   BBOARD_BTIM(1) = TODAY_BTIM(1)
	   BBOARD_BTIM(2) = TODAY_BTIM(2)
	   REWRITE (4) USER_HEADER		! Rewrite headerr
	   IF (.NOT.TEST_BULLCP()) CALL CREATE_PROCESS('BBOARD')T
	ELSE IF (IER.NE.0) THEN
	   CALL CLOSE_BULLUSER(
	   CALL EXIT			! If no header, no messagesN
	END IFS
 E
	IF (IER1.EQ.0) THEN		! Skip date comparison if new entryL
CA
C  Compare and see if messages have been added since the last time
C  that the user has logged in or used the BULLETIN facility..
C0
	   DIFF1 = COMPARE_BTIM(LOGIN_BTIM,READ_BTIM)
	   IF (DIFF1.LT.0) THEN		! If read messages since last login,
	      LOGIN_BTIM(1) = READ_BTIM(1) ! then use the read date to compareI
	      LOGIN_BTIM(2) = READ_BTIM(2) ! with the latest bulletin date	
	   END IF			! to see if should alert user.R
 N
	   IF (SYSTEM_SWITCH) THENB
	      DIFF1 = COMPARE_BTIM(SYSTEM_LOGIN_BTIM,NEWEST_BTIM)
	   ELSE
	      DIFF1 = COMPARE_BTIM(LOGIN_BTIM,NEWEST_BTIM)L
	   END IF
	END IFR
 D
	LOGIN_BTIM_SAVE(1) = LOGIN_BTIM(1) ! These are destroyed in UPDATE_READ
	LOGIN_BTIM_SAVE(2) = LOGIN_BTIM(2)S
	
	IF (NEW_FLAG(2).NE.0.AND.NEW_FLAG(2).NE.-1) THENS
	   CALL LIB$MOVC3(4,NEW_FLAG(2),%REF(BULL_PARAMETER))
	   CALL SUBTIME(LOGIN_BTIM,BULL_PARAMETER(1:4),IER)
	ELSE IF (DIFF1.GT.0) THEN
	   BULL_POINT = -1'
	   IF (READIT.EQ.1) THENI
	      CALL UPDATE_READ(1)
	      LOGIN_BTIM_NEW(1) = LOGIN_BTIM(1)
	      LOGIN_BTIM_NEW(2) = LOGIN_BTIM(2)
	      CALL READ_IN_FOLDERS.
	      CALL MODIFY_SYSTEM_LIST(1)C
	   END IF
	   CALL CLOSE_BULLUSERL
	   RETURN
	END IFE
  
	CALL READ_IN_FOLDERS 
	CALL MODIFY_SYSTEM_LIST(1) 
 N
	ENTRY LOGIN_FOLDERT
 R
	IF (NEW_FLAG(2).EQ.0.OR.NEW_FLAG(2).EQ.-1.OR.FOLDER_SET) THEN
	   LOGIN_BTIM(1) = LOGIN_BTIM_SAVE(1)
	   LOGIN_BTIM(2) = LOGIN_BTIM_SAVE(2)
	END IFH
 1
	IF (REMOTE_SET.EQ.1) THEN	! If system remote folder, use remote
					! info, not local login timeT
	   IF (LAST_SYS_BTIM(1,FOLDER_NUMBER+1).NE.0) THENT
	    LOGIN_BTIM(1) = LAST_SYS_BTIM(1,FOLDER_NUMBER+1)H
	    LOGIN_BTIM(2) = LAST_SYS_BTIM(2,FOLDER_NUMBER+1)1
	    LAST_SYS_BTIM(1,FOLDER_NUMBER+1) = 0
	    LAST_SYS_BTIM(2,FOLDER_NUMBER+1) = 0)
	   ELSE
	    DIFF1 = COMPARE_BTIM(LOGIN_BTIM,E
     &			LAST_READ_BTIM(1,FOLDER_NUMBER+1))D
	    IF (DIFF1.LT.0) THEN0
	      LOGIN_BTIM(1) = LAST_READ_BTIM(1,FOLDER_NUMBER+1)
	      LOGIN_BTIM(2) = LAST_READ_BTIM(2,FOLDER_NUMBER+1)
	    ELSE(
	      DIFF = MINUTE_DIFF(LOGIN_BTIM,F_NEWEST_BTIM) 
	      IF (DIFF.GE.0.AND.DIFF.LE.15) THEN  ! BULLCP updates every 15 min
	         IER = SYS$BINTIM('0 00:15',BULLCP_BTIM)A
	         BULLCP_BTIM(1) = -BULLCP_BTIM(1) ! Convert to -delta timeL
	         BULLCP_BTIM(2) = -BULLCP_BTIM(2)-1
	         CALL LIB$SUBX(LOGIN_BTIM,BULLCP_BTIM,LOGIN_BTIM)
	      END IFn
	    END IFR
	   END IF
	END IF 
 n
	ENTRY SHOW_SYSTEM
 A
	JUST_SYSTEM = (.NOT.LOGIN_SWITCH.AND.SYSTEM_SWITCH).OR.
     &	        (BTEST(FOLDER_FLAG,2)
     &		.AND..NOT.TEST_SET_FLAG(FOLDER_NUMBER)
     &		.AND..NOT.TEST_BRIEF_FLAG(FOLDER_NUMBER))o
 g
	NGEN = 0			! Number of general messages
	NSYS = 0			! Number of system messagesA
	BULL_POINT = -1
 )
	IF (IER1.NE.0.AND.FOLDER_NUMBER.GT.0) THEN
	   IF (LOGIN_SWITCH) THEN
	      IF (READIT.EQ.1) THEN
	         LOGIN_BTIM_SAVE(1) = LOGIN_BTIM(1)
	         LOGIN_BTIM_SAVE(2) = LOGIN_BTIM(2)
	         CALL UPDATE_READ(1)L
	         LOGIN_BTIM_NEW(1) = LOGIN_BTIM(1)I
	         LOGIN_BTIM_NEW(2) = LOGIN_BTIM(2)/
	      END IFC
	      CALL CLOSE_BULLUSER
	   END IF
	   RETURN	! Don't overwhelm new user with lots of non-general msgs,
	END IFL
 C
	IF (BTEST(FOLDER_FLAG,2).AND.SYSTEM_SWITCH) THENM
			! Can folder have SYSTEM messages and /SYSTEM specified? 
	   LOGIN_BTIM(1) = SYSTEM_LOGIN_BTIM(1)	! Use specified login timeC
	   LOGIN_BTIM(2) = SYSTEM_LOGIN_BTIM(2)	! for system messages.I
	END IFS
 _
	IF (LOGIN_SWITCH) THENC
	   IF (READIT.EQ.1) THENI
	      LOGIN_BTIM_OLD(1) = LOGIN_BTIM(1)
	      LOGIN_BTIM_OLD(2) = LOGIN_BTIM(2)
	      CALL UPDATE_READ(1)
	      LOGIN_BTIM_NEW(1) = LOGIN_BTIM(1)
	      LOGIN_BTIM_NEW(2) = LOGIN_BTIM(2)
	      LOGIN_BTIM(1) = LOGIN_BTIM_OLD(1)
	      LOGIN_BTIM(2) = LOGIN_BTIM_OLD(2)
	   END IF
	   CALL CLOSE_BULLUSER 
	END IFD
  
	IF (READIT.EQ.1.AND.FOLDER_NUMBER.GE.0) THEN
	   IF (LAST_SYS_BTIM(1,FOLDER_NUMBER+1).NE.0) THENf
	      DIFF1 = COMPARE_BTIM(LOGIN_BTIM, 
     &				LAST_SYS_BTIM(1,FOLDER_NUMBER+1))R
	      IF (DIFF1.LT.0) THENB
	         LOGIN_BTIM(1) = LAST_SYS_BTIM(1,FOLDER_NUMBER+1)
	         LOGIN_BTIM(2) = LAST_SYS_BTIM(2,FOLDER_NUMBER+1)
	      END IF_
	      LAST_SYS_BTIM(1,FOLDER_NUMBER+1) = LOGIN_BTIM_NEW(1)C
	      LAST_SYS_BTIM(2,FOLDER_NUMBER+1) = LOGIN_BTIM_NEW(2)I
	   END IF
 Y
	   IF (TEST2(BRIEF_FLAG,FOLDER_NUMBER)0
     &		       .AND.TEST2(SET_FLAG,FOLDER_NUMBER)) THEN1
	      IF (.NOT.TEST2(SYSTEM_FLAG,FOLDER_NUMBER)) GO TO 9999
	   END IF
	END IFn
 d
	CALL OPEN_BULLDIR_SHARED	! Get bulletin directory
	IF (.NOT.REMOTE_SET) THEN
	   CALL READDIR(0,IER)		! Get header info
	ELSE 
	   NBULL = F_NBULLI
	END IFT
	   
	CALL INIT_QUEUE(GEN_DIR1,BULLDIR_ENTRY)
	CALL INIT_QUEUE(SYS_DIR1,BULLDIR_ENTRY)
	CALL INIT_QUEUE(SYS_NUM1,%DESCR(ICOUNT))(
	GEN_DIR = GEN_DIR1.
	SYS_DIR = SYS_DIR1H
	SYS_NUM = SYS_NUM1)
	START = 1
	REVERSE = 0
	IF ((.NOT.TEST_SET_FLAG(FOLDER_NUMBER).OR. 
     &		.NOT.TEST_BRIEF_FLAG(FOLDER_NUMBER))
     &		.AND..NOT.BTEST(FOLDER_FLAG,7)) THEN
	   IF (REVERSE_SWITCH) REVERSE = 1I
	   IF (IER1.EQ.0) THENI
	      CALL GET_NEWEST_MSG(LOGIN_BTIM,START)
	      IF (START.EQ.-1) START = NBULL + 1M
	   END IF
	END IFM
  
	IF (REMOTE_SET) THENS
	   CALL INIT_QUEUE(ALL_DIR1,BULLDIR_ENTRY) 
	   ALL_DIR = ALL_DIR1
	   CALL REMOTE_DIRECTORY_COMMAND(START,NBULL,
     &					 .NOT.REVERSE,ALL_DIR,IER)
	   IF (IER.NE.0) THEN
	      CALL CLOSE_BULLDIRB
	      CALL DISCONNECT_REMOTER
	      GO TO 9999R
	   END IF
	   LAST_DIR = ALL_DIR
	   ALL_DIR = ALL_DIR1
	END IF1
  
	DO ICOUNT1 = NBULL,START,-1
	   IF (REVERSE) THENF
	      ICOUNT = NBULL + START - ICOUNT1N
	   ELSE
	      ICOUNT = ICOUNT1
	   END IF
	   IF (REMOTE_SET) THEN
	      IF (ALL_DIR.EQ.LAST_DIR) GO TO 100s
	      CALL READ_QUEUE(%VAL(ALL_DIR),ALL_DIR,BULLDIR_ENTRY) 
	      IER = ICOUNT + 1T
	   ELSE
	      CALL READDIR(ICOUNT,IER)E
	   END IF
	   IF (IER1.EQ.0.AND.IER.EQ.ICOUNT+1) THEN ! Is this a totally new user?L
	      IF (.NOT.REVERSE.AND..NOT.BTEST(FOLDER_FLAG,7)) THEN 
	         DIFF = COMPARE_BTIM(LOGIN_BTIM,MSG_BTIM) ! No, so compare date
	         IF (DIFF.GT.0) GO TO 100
	      END IFG
	      IF (.NOT.BTEST(FOLDER_FLAG,2)) SYSTEM = SYSTEM.AND.(.NOT.1)
			! Show system msg in non-system folder as general msg
	      IF (USERNAME.NE.FROM.OR.SYSTEM) THEN
				  	! Is bulletin system or from same user?
		 IF (SYSTEM) THEN	! Is it system bulletin?  
		    NSYS = NSYS + 1_
		    CALL WRITE_QUEUE(%VAL(SYS_DIR),SYS_DIR,BULLDIR_ENTRY) 
		    CALL WRITE_QUEUE(%VAL(SYS_NUM),SYS_NUM,%DESCR(ICOUNT))
	         ELSE IF (.NOT.JUST_SYSTEM) THENN
		    IF (BTEST(FOLDER_FLAG,7)) THEN
		       DIFF = COMPARE_BTIM
     &			      (LAST_READ_BTIM(1,FOLDER_NUMBER+1),MSG_BTIM)s
		    ELSE IF (.NOT.SYSTEM_SWITCH) THENL
		       DIFF = -1
		    ELSE
	               DIFF = COMPARE_BTIM(LOGIN_BTIM_SAVE,MSG_BTIM),
		    END IF
		    IF (DIFF.LT.0) THEN 
		       IF (.NOT.REVERSE.OR.BULL_POINT.EQ.-1) THEN	
		          BULL_POINT = ICOUNT - 1 
		          IF (.NOT.BTEST(FOLDER_FLAG,2).AND.
     &			   TEST_BRIEF_FLAG(FOLDER_NUMBER).AND.e
     &			   TEST_SET_FLAG(FOLDER_NUMBER)) GO TO 100=
		       END IF 
		       NGEN = NGEN + 1
		       SYSTEM = ICOUNT
		       CALL WRITE_QUEUE(%VAL(GEN_DIR),GEN_DIR,BULLDIR_ENTRY)
		    END IF
	         END IF
	      END IF_
	   ELSE IF (IER.EQ.ICOUNT+1) THEN
			! Totally new user, save only permanent system msgs
	      IF ((SYSTEM.AND.7).EQ.3.OR.
     &		  (SYSTEM.AND.BTEST(FOLDER_FLAG,7))) THENE
	         NSYS = NSYS + 1k
		 CALL WRITE_QUEUE(%VAL(SYS_DIR),SYS_DIR,BULLDIR_ENTRY)
		 CALL WRITE_QUEUE(%VAL(SYS_NUM),SYS_NUM,%DESCR(ICOUNT))U
	      ELSE IF (NGEN.EQ.0.OR.	! And save only the first non-system msg
     &		     BTEST(FOLDER_FLAG,7)) THEN ! and SET ALWAYS folder messages
		 SYSTEM = ICOUNT	! Save bulletin number for displayE
		 IF (.NOT.REVERSE.OR.BULL_POINT.EQ.-1) THEN(
		    BULL_POINT = ICOUNT - 1r
		    IF (.NOT.BTEST(FOLDER_FLAG,2).AND.
     &			TEST_BRIEF_FLAG(FOLDER_NUMBER).AND.
     &		 	TEST_SET_FLAG(FOLDER_NUMBER)) GO TO 100d
		 END IFl
		 NGEN = NGEN + 1
		 CALL WRITE_QUEUE(%VAL(GEN_DIR),GEN_DIR,BULLDIR_ENTRY)
	      END IFP
	   END IF
	END DOE
100	CALL CLOSE_BULLDIR
C0
C  Review new directory entries.  If there are system messages,)
C  copy the system bulletin into GEN_DIR file BULLSYS.SCR for outputting
C  to the terminal.  If there are simple messages, just output the
C  header information.
C
	IF (TEST_BRIEF_FLAG(FOLDER_NUMBER).AND.
     &			   TEST_SET_FLAG(FOLDER_NUMBER)) NGEN = 0
  
	IF (NGEN.EQ.0.AND.NSYS.EQ.0) GO TO 9999
 N
	IF (NSYS.GT.0) THEN		! Are there any system messages?
	   IF (FIRST_WRITE) THEN 
	      PAGE = 4		! Don't erase MAIL/PASSWORD notifiesI
	      FIRST_WRITE = .FALSE.	! if this is first write to screen.
	   END IF
	   LENF = TRIM(FOLDER_NAME)
	   S1 = (PAGE_WIDTH-(LENF+16))/2G
	   S2 = PAGE_WIDTH - S1 - (LENF + 16)
	   WRITE (6,'(''+'',A,$)') CTRL_G
	   WRITE (6,1026) FOLDER_NAME(:LENF)		! Yep...P
	   PAGE = PAGE + 1L
	   CTRL_G = 0		! Don't ring bell for non-system bulls
	   CALL OPEN_BULLFIL_SHARED
	   CALL INIT_QUEUE(SYS_BUL1,INPUT)Y
	   SYS_BUL = SYS_BUL1
	   SYS_DIR = SYS_DIR1
	   SYS_NUM = SYS_NUM1
	   NSYS_LINE = 0D
	   DO J=1,NSYSL
	      CALL READ_QUEUE(%VAL(SYS_DIR),SYS_DIR,BULLDIR_ENTRY)N
	      IF (REMOTE_SET) THEN)
	         CALL READ_QUEUE(%VAL(SYS_NUM),SYS_NUM,%DESCR(ICOUNT)))
	         WRITE (REMOTE_UNIT,'(2A)',IOSTAT=IER) 5,ICOUNT
	         IF (IER.GT.0) THEN
	            CALL DISCONNECT_REMOTE	
	         ELSE
	            CALL GET_REMOTE_MESSAGE(IER)D
	         END IF
		 IF (IER.GT.0) THEN(
		    CALL CLOSE_BULLFIL
		    GO TO 9999
		 END IF(
	      END IFT
 	      INPUT = ' '1
	      CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
	      NSYS_LINE = NSYS_LINE + 1
	      ILEN = LINE_LENGTH + 1E
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN
	         CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)G
	      END IFS
	      IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THEN
	         CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)D
	      END IFF
	      DO WHILE (ILEN.GT.0)	! Copy bulletin to SYS_BUL link list
		 CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
		 NSYS_LINE = NSYS_LINE + 1
		 CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      END DO)
	      IF (ILEN.LT.0) THEN
		 CALL CLOSE_BULLFILL
		 GO TO 9999 
	      END IF 
	      IF (J.LT.NSYS.AND.SEPARATE.NE.' ') THEN
 	         INPUT = ' '
	         CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT) 
		 DO I=1,PAGE_WIDTH
		    INPUT(I:I) = SEPARATET
		 END DOR
		 CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
	         NSYS_LINE = NSYS_LINE + 2o
	      END IFg
	   END DO
	   CALL CLOSE_BULLFIL
	   SYS_BUL = SYS_BUL1
	   ILEN = 0
	   I = 1D
	   DO WHILE (I.LE.NSYS_LINE.OR.ILEN.GT.0)  ! Write out system messages.
	      IF (ILEN.EQ.0) THEN
	         CALL READ_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
		 ILEN = TRIM(INPUT) 
		 I = I + 1
	      END IF 
	      IF (SYS_BUL.NE.0) THENI
		 IF (PAGE.EQ.PAGE_LENGTH-2.AND.PAGING) THEN_
							! If at end of screen
		    WRITE(6,1080)	! Ask for input to proceed to next pagee
		    CALL GET_INPUT_NOECHO_PROMPT(INREAD,! Get terminal input
     &			'HIT any key for next page....')M
	            WRITE (6,'(1X)')e
	            CALL LIB$ERASE_PAGE(1,1)		! Clear the screenL
		    PAGE = 1
		    IF (ILEN.LE.PAGE_WIDTH) THEN
		       WRITE(6,1060) '+'//INPUT(:ILEN)
		       ILEN = 0I
		    ELSE
		       WRITE(6,1060) '+'//INPUT(:PAGE_WIDTH)
		       INPUT = INPUT(PAGE_WIDTH+1:)(
		       ILEN = ILEN - PAGE_WIDTHN
		    END IF
		 ELSEP
		    PAGE = PAGE + 1G
		    IF (ILEN.LE.PAGE_WIDTH) THEN
		       WRITE(6,1060) ' '//INPUT(:ILEN)
		       ILEN = 0L
		    ELSE
		       WRITE(6,1060) ' '//INPUT(:PAGE_WIDTH)
		       INPUT = INPUT(PAGE_WIDTH+1:)E
		       ILEN = ILEN - PAGE_WIDTHL
		    END IF
		 END IF 
	      END IFB
	   END DO
	   IF (NGEN.EQ.0) THEN 
	      WRITE(6,'(A)')		! Write delimiting blank line
	   END IF
	   PAGE = PAGE + 1 
	END IFD
 1
	ENTRY REDISPLAY_DIRECTORY
 T
	GEN_DIR = GEN_DIR1(
	IF (NGEN.GT.0) THEN		! Are there new non-system messages?
	   LENF = TRIM(FOLDER_NAME)
	   S1 = (PAGE_WIDTH-13-LENF)/2E
	   S2 = PAGE_WIDTH-S1-13-LENF
	   IF (PAGE+5+NGEN.GT.PAGE_LENGTH.AND.PAGE.GT.0) THEN
	      WRITE(6,1080)		! Ask for input to proceed to next page0
	      CALL GET_INPUT_NOECHO_PROMPT(INREAD,	! Get terminal input
     &			'HIT any key for next page....') 
	      WRITE (6,'(1X)')E
	      CALL LIB$ERASE_PAGE(1,1)	! Clear the screen
	      WRITE (6,'(''+'',A,$)') CTRL_G
	      WRITE(6,1028) 'New '//FOLDER_NAME(1:LENF)//' messages' 
	      PAGE = 1T
	   ELSE
	      IF (FIRST_WRITE) THEN
		 PAGE = 4		  ! Don't erase MAIL/PASSWORD notifiesC
	         FIRST_WRITE = .FALSE. ! if this is first write to screen.D
	      END IF
	      WRITE (6,'(''+'',A,$)') CTRL_GR
	      WRITE(6,1027) 'New '//FOLDER_NAME(1:LENF)//' messages'N
	      PAGE = PAGE + 1
	   END IF
	   WRITE(6,1020)T
	   WRITE(6,1025) 
	   PAGE = PAGE + 2_
	   I = 0E
	   DO WHILE (I.LT.NGEN)
	      I = I + 1
	      CALL READ_QUEUE(%VAL(GEN_DIR),GEN_DIR,BULLDIR_ENTRY) 
	      CALL CONVERT_ENTRY_FROMBIND
	      N = MAX(INT(LOG10(REAL(SYSTEM)))+1,3)
	      N1 = MAX(1,6-N)
	      IF (PAGE.EQ.PAGE_LENGTH-2.AND.PAGING) THEN ! If at end of screen,
		 WRITE(6,1080)	! Ask for input to proceed to next page
		 CALL GET_INPUT_NOECHO_PROMPT(INREAD,
     &		'HIT Q(Quit listing) or any other key for next page....') 
	         CALL STR$UPCASE(INREAD,INREAD)
	         WRITE (6,'(1X)')
	         CALL LIB$ERASE_PAGE(1,1)		! Clear the screen
		 PAGE = 1R
		 IF (INREAD.EQ.'Q') THEN
		    I = NGEN		! Quit directory listing
		    WRITE(6,'(''+Quitting directory listing.'')')T
		 ELSE 
		    WRITE(6,1040) '+'//DESCRIP(:53),FROM,DATE(:6),SYSTEM
		 END IF=
					! Bulletin number is stored in SYSTEM
	      ELSE
		 PAGE = PAGE + 1
		 WRITE(6,1040) ' '//DESCRIP(:53),FROM,DATE(:6),SYSTEMe
	      END IF 
	   END DO
	   IF ((.NOT.FOLDER_SET.AND.BTEST(SET_FLAG(1),0).AND.DIFF1.LE.0)E
     &		.OR.(FOLDER_SET.AND.TEST_SET_FLAG(FOLDER_NUMBER))) THENI
	      PAGE = 0	! Don't reset page counter if READNEW not set,
	   END IF	! as no prompt to read is generated. 
	END IFg
C 
C  Instruct users how to read displayed messages if READNEW not selected.E
C
	IF (.NOT.TEST_BRIEF_FLAG(FOLDER_NUMBER).AND.
     &		TEST_SET_FLAG(FOLDER_NUMBER)) THEN
	   WRITE(6,1030)Y
	ELSE IF (NGEN.EQ.0) THENE
	   ILEN = 57 + INDEX(COMMAND_PROMPT,'>') - 1 
	   S1 = (PAGE_WIDTH-ILEN)/2
	   S2 = PAGE_WIDTH - S1 - ILEN 
	   WRITE(6,1035) 'The '//COMMAND_PROMPT(:ILEN-57)//
     &		'/SYSTEM command can be used to reread these messages.'(
	ELSEA
	   FLEN = TRIM(FOLDER_NAME)
	   IF (FOLDER_NUMBER.EQ.0) FLEN = -1C
	   ILEN = 49 + INDEX(COMMAND_PROMPT,'>') - 1 + FLEN
	   S1 = (PAGE_WIDTH-ILEN)/2
	   S2 = PAGE_WIDTH - S1 - ILENF
	   IF (FOLDER_NUMBER.EQ.0) THEN
	      WRITE(6,1035) 'The ' //COMMAND_PROMPT(:ILEN-48)//
     &		' command can be used to read these messages.'
	   ELSE
	      WRITE(6,1035) 'The '//COMMAND_PROMPT(:ILEN-49-FLEN)
     &		//' '//FOLDER_NAME(:FLEN)//M
     &		' command can be used to read these messages.'
	   END IF
	END IF 
 C
9999	IF (LOGIN_SWITCH) THENU
	   LOGIN_BTIM(1) = LOGIN_BTIM_NEW(1)
	   LOGIN_BTIM(2) = LOGIN_BTIM_NEW(2)E
	   LOGIN_BTIM_SAVE(1) = LOGIN_BTIM_OLD(1)
	   LOGIN_BTIM_SAVE(2) = LOGIN_BTIM_OLD(2)
	END IFs
	RETURNI
 (
1020	FORMAT(' Description',43X,'From',9X,'Date',3X,'Number')
1025	FORMAT(' -----------',43X,'----',9X,'----',3X,'------')
1026	FORMAT(' ',<S1>('*'),A,' System Messages',<S2>('*'))V
1027	FORMAT(/,' ',<S1>('*'),A,<S2>('*'))
1028	FORMAT('+',<S1>('*'),A,<S2>('*'))
1030	FORMAT(' ',<PAGE_WIDTH>('*'))
1035	FORMAT(' ',<S1>('*'),A,<S2>('*'))
1040	FORMAT(A<53>,2X,A12,1X,A6,<N1>X,I<N>)
1060	FORMAT(A)
1070	FORMAT(' ERROR: Cannot add new entry to user file.')-
1080	FORMAT(' ',/)
 I
	END
 N
  
	
	SUBROUTINE GET_NODE_NUMBER_OTHER(NODE_NUMBER,NODE_AREA,NODE_NAME)
 E
	IMPLICIT INTEGER (A-Z)	
 T
	INCLUDE '($SYIDEF)'
  
	CHARACTER*(*) NODE_NAME
 E
	CALL INIT_ITMLST	! Initialize item list
				! Now add items to list
	CALL ADD_2_ITMLST(4,SYI$_NODE_AREA,%LOC(NODE_AREA))
	CALL ADD_2_ITMLST(4,SYI$_NODE_NUMBER,%LOC(NODE_NUMBER))
	CALL END_ITMLST(GETSYI_ITMLST)	! Get address of itemlistR
 l
	IER = SYS$GETSYIW(,,NODE_NAME(:TRIM(NODE_NAME)),I
     &			%VAL(GETSYI_ITMLST),,,)	! Get Info command.
 n
	IF (.NOT.IER) THENT
	   WRITE (6,'('' ERROR: Specified node name not found.'')')
	   NODE_AREA = 0N
	END IF
  
	RETURN0
	END
 .
