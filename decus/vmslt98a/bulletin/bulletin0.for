C
C  BULLETIN0.FOR, Version 9/20/96
C  Purpose: Contains subroutines for the BULLETIN utility program.
C  Environment: VAX/VMS
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

	CHARACTER ANSWER*4,REMOTE_USER*12,SUBJECT*56

	IMMEDIATE = 0
	IF (CLI$PRESENT('IMMEDIATE')) THEN
	   IF (REMOTE_SET.EQ.4) THEN
	      WRITE (6,'('' IMMEDIATE not valid for news group.'')') 
	      RETURN
	   ELSE
	      IMMEDIATE = 1
	   END IF
	END IF

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
	   IF (ANSWER(:1).NE.'Y') THEN
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

	IF (REMOTE_SET.EQ.1) THEN
	   IF (SBULL.NE.EBULL) THEN
	      WRITE (6,1025)
	      RETURN
	   END IF
	   IER1 = SBULL + 1
	   IF (SBULL.NE.BULL_POINT) CALL READDIR(SBULL,IER1)
	   SUBJECT = DESCRIP
	   IER2 = 0
	   IF (IER2.EQ.0.AND.IER1.EQ.SBULL+1) CALL 
     &	    REMOTE_DELETE(SBULL,IMMEDIATE,SUBJECT,I,FOLDER1_COM,IER)     
	   IF (IER.EQ.0.AND.REMOTE_SET.LT.3) THEN
	      IF (I.EQ.LEN(FOLDER1_COM)) THEN
	         IER = SYS$ASCTIM(,INPUT,F1_NEWEST_BTIM,)
	         NEWEST_EXDATE = INPUT(:11)
	         NEWEST_EXTIME = INPUT(13:23)
	         NBULL = F1_NBULL
	   	 CALL UPDATE_FOLDER
	      ELSE
	  	 WRITE (6,'(1X,A)') FOLDER1_COM(:I)
	      END IF
	   ELSE IF (IER.NE.0) THEN
	      CALL DISCONNECT_REMOTE
	   END IF
	   RETURN
	ELSE IF (REMOTE_SET.EQ.3) THEN
	   BULL_DELETE = SBULL - 1
	   IER = 0
	   IF (CLI$PRESENT('REASON')) THEN 
	      CALL CLI$GET_VALUE('REASON',BULL_PARAMETER,LEN_P)
	   END IF
	   DO WHILE (BULL_DELETE.LT.EBULL)
	      BULL_DELETE = BULL_DELETE + 1
	      DO WHILE (BULL_DELETE+1.NE.IER)
	         CALL READDIR(BULL_DELETE,IER)	! Get info for bulletin
	         IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?
	            BULL_DELETE = BULL_DELETE + 1
	            IF (BULL_DELETE.GT.EBULL) RETURN
	            IF (EBULL.EQ.SBULL) THEN
		       WRITE(6,1030) 
	               RETURN
		    END IF
	         END IF
	      END DO
	      SUBJECT = DESCRIP
	      IF (.NOT.TEST_NEWS_OWNER().AND.SETPRV_PRIV().AND.
     &		  .NOT.CLI$PRESENT('FORCE')) THEN
 	         SUBJECT = 'CanceL'
	         IF (CLI$PRESENT('REASON')) THEN 
		    SUBJECT = SUBJECT(:6)//BULL_PARAMETER(:LEN_P)
		 END IF
	      END IF
	      CALL REMOTE_DELETE
     &			(SBULL,IMMEDIATE,SUBJECT,I,FOLDER1_COM,IER)     
	      IF (IER.NE.0) THEN
		 CALL DISCONNECT_REMOTE
	   	 RETURN
	      END IF
	   END DO
	   RETURN
	END IF

	CALL OPEN_BULLDIR

	CALL READDIR(0,IER)

	BULL_DELETE = SBULL - 1
	DO WHILE (BULL_DELETE.LT.EBULL)
	   BULL_DELETE = BULL_DELETE + 1
	   DO WHILE (BULL_DELETE+1.NE.IER)
	      CALL READDIR(BULL_DELETE,IER)	! Get info for bulletin
	      IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?
	         IF (REMOTE_SET.EQ.4) THEN
	            BULL_DELETE = BULL_DELETE + 1
	            IF (BULL_DELETE.GT.EBULL) THEN
		       CALL CLOSE_BULLDIR
		       RETURN
		    END IF
	         ELSE
	            IF (.NOT.CLI$PRESENT('ALL')) WRITE(6,1030) 
	            CALL CLOSE_BULLDIR		! If not, then error out
	            RETURN
	         END IF
	      END IF
	   END DO

	   SUBJECT = DESCRIP

	   IF (USERNAME.NE.FROM.OR.(REMOTE_SET.EQ.4.AND.
     &		.NOT.TEST_NEWS_OWNER())) THEN
	      CALL STR$UPCASE(REMOTE_USER,FROM)
	      IF ((.NOT.SETPRV_PRIV().AND..NOT.FOLDER_SET).OR.    ! Privileges?
     &	       (.NOT.FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)
     &		.AND.FOLDER_SET)) THEN
	         WRITE(6,1040)		! No, then error out.
	         CALL CLOSE_BULLDIR
		 RETURN
	      ELSE IF (SBULL.EQ.EBULL) THEN
		 IF (TRIM(FROM).EQ.1) THEN
		    CALL OPEN_BULLFIL
	            ILEN = LINE_LENGTH + 1
	            CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	            CALL CLOSE_BULLFIL
	            ASK = ILEN.EQ.0.OR.INPUT(:6).NE.'From: '
		 ELSE
		    ASK = REMOTE_USER.NE.USERNAME
		 END IF
		 IF (ASK.AND..NOT.CLI$PRESENT('FORCE')) THEN
	            CALL CLOSE_BULLDIR
	            WRITE (6,1050)	! Make sure user wants to delete it
	            READ (5,'(A)',IOSTAT=IER) ANSWER
	            CALL STR$UPCASE(ANSWER,ANSWER)
	            IF (ANSWER(:1).NE.'Y') RETURN
	            CALL OPEN_BULLDIR
	            CALL READDIR(BULL_DELETE,IER)
	            IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?
	               WRITE(6,1030)	! If not, then error out
	               CALL CLOSE_BULLDIR
		       RETURN
	            END IF
		    IF (REMOTE_SET.EQ.4) THEN 
 	               SUBJECT = 'CanceL'
		    END IF
	         END IF
	      END IF
	   END IF

C
C  Delete the bulletin directory entry.
C
	   CALL REMOVE_ENTRY(BULL_DELETE,SBULL,EBULL,IMMEDIATE)
	   IF (REMOTE_SET.EQ.4) THEN
	      IF (.NOT.CLI$PRESENT('LOCAL').AND.(TEST_NEWS_OWNER().OR.
     &		  SETPRV_PRIV())) THEN
	         CALL REMOTE_DELETE
     &		    (BULL_DELETE,IMMEDIATE,SUBJECT,I,FOLDER1_COM,IER)
	      END IF
	   END IF
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

        INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLDIR.INC'

	COMMON /POINT/ BULL_POINT

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	INTEGER NOW(2),EX(2)

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
	      IF (EXDATE(8:9).EQ.'19') EXDATE(8:9) = '18'
	      IF (EXDATE(8:9).EQ.'20') EXDATE(8:9) = '19'
	      IF (EXDATE(8:9).EQ.'18'.AND.EXDATE(10:10).LT.'6')
     &			EXDATE(10:11) = '99'
	   ELSE				! Permanent or Shutdown
	      IF (EXDATE(2:2).EQ.'-') THEN
	         EXDATE = EXDATE(:6)//'19'//EXDATE(9:)
	      ELSE
	         EXDATE = EXDATE(:7)//'19'//EXDATE(10:)
	      END IF
	   END IF

	   CALL WRITEDIR(BULL_DELETE,IER)	! Update message expiration date

	   IER = SYS$BINTIM('0 0:15',EX)	! Get time 15 minutes from now
	   IER = SYS$GETTIM(NOW)
	   IER = LIB$SUBX(NOW,EX,EX)
	   IER = SYS$ASCTIM(,INPUT,EX,)

	END IF

	IF (IMMEDIATE.NE.1.AND.BULL_DELETE.EQ.EBULL) THEN
	   CALL READDIR(0,IER)			! Get header

	   NEWEST_EXDATE = INPUT(:11)		! and store new expiration date
	   NEWEST_EXTIME = INPUT(13:23)

	   CALL WRITEDIR(0,IER)
	   IF (REMOTE_SET.EQ.4) THEN
	      CALL OPEN_BULLNEWS_SHARED
	      CALL READ_FOLDER_FILE_KEYNUM(FOLDER_NUMBER,IER)
              CALL GET_MSGKEY(NEWEST_EXBTIM,NEWS_F_EXPIRED_DATE)
	      CALL REWRITE_FOLDER_FILE(IER)
	      CALL CLOSE_BULLNEWS 
	   END IF
	ELSE IF (BULL_DELETE.EQ.EBULL) THEN
	   IF (REMOTE_SET.NE.4) CALL CLEANUP_DIRFILE(SBULL)
				! Reorder directory file

	   CALL UPDATE_ALWAYS	! Somewhat a kludgey way of updating latest
				! bulletin and expired dates.

	   IF (REMOTE_SET.NE.4.AND.SBULL.LE.BULL_POINT) THEN
	      IF (BULL_POINT.GT.EBULL) THEN
	         BULL_POINT = BULL_POINT - (EBULL - SBULL + 1)
	      ELSE
		 BULL_POINT = SBULL - 1
	      END IF
	   END IF		! Readjust where which bulletin to read next
				! if deletion causes messages to be moved.
	END IF

	RETURN
	END





	SUBROUTINE GET_2_VALS(INPUT,ILEN,SVAL,EVAL,IER)

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	COMMON /POINT/ BULL_POINT

	CHARACTER*(*) INPUT

	DELIM = MAX(INDEX(INPUT,':'),INDEX(INPUT,'-'))

	IF (DELIM.EQ.0) THEN
	   DECODE(ILEN,'(I<ILEN>)',INPUT,IOSTAT=IER) SVAL
	   EVAL = SVAL
	ELSE
	   DECODE(DELIM-1,'(I<DELIM-1>)',INPUT,IOSTAT=IER) SVAL
	   CALL STR$UPCASE(INPUT,INPUT)
	   IF (IER.NE.0) THEN
	      IF (INDEX('CURRENT',INPUT(:DELIM-1)).EQ.1) THEN
		 SVAL = BULL_POINT
		 IER = 0
	      END IF
	   END IF
	   IF (IER.EQ.0) THEN
	      ILEN = ILEN - DELIM
	      DECODE(ILEN,'(I<ILEN>)',INPUT(DELIM+1:),IOSTAT=IER) EVAL
	      IF (IER.NE.0) THEN
	         IF (INDEX('LAST',INPUT(DELIM+1:TRIM(INPUT))).EQ.1) THEN
		    EVAL = F_NBULL
		    IER = 0
                 ELSE IF (INDEX('CURRENT',
     &                  INPUT(DELIM+1:TRIM(INPUT))).EQ.1) THEN
                    EVAL = BULL_POINT
                    IER = 0
                 END IF
 	      END IF
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

	COMMON /PAGE/ PAGE_LENGTH,PAGE_WIDTH,PAGING
	LOGICAL PAGING

	DATA SCRATCH_D1/0/
	DATA EXCLUDE_D1/0/

	COMMON /POINT/ BULL_POINT

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /TAGS/ BULL_TAG,READ_TAG,BULL_NEWS_TAG

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*256 INCMD

	COMMON /CLOSE_FILES_INFO/ CLOSED_FILES

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /POST/ POSTTIME

	COMMON /NEXT/ NEXT

	COMMON /NEW_DIR/ NEW

	COMMON /BULL_USER_CUSTOM/ BULL_USER_CUSTOM

	COMMON /NEWGROUP/ NEWGROUP

	COMMON /CTRLC_FLAG/ FLAG

	COMMON /DIRMODE/ DIRMODE
	DATA DIRMODE/.FALSE./

	EXTERNAL CLI$_ABSENT,CLI$_NEGATED,CLI$_PRESENT,CLOSE_FILES
	EXTERNAL BULLETIN_SUBCOMMANDS

	CHARACTER DATETIME*24,SEARCH_STRING*80,OUTLINE*80
        CHARACTER GROUP*80,STAT*4

	INTEGER TODAY(2)

	CHARACTER*12 EXPIRES,DIR_TYPE

        INTEGER TIMADR(2)                       ! Buffer containing time

	DATA WAITEFN /0/

	NEXT = .TRUE.
	DIRMODE = .TRUE.

	CALL INIT_QUEUE(SCRATCH_F1,GROUP)

	IF (WAITEFN.EQ.0) CALL LIB$GET_EF(WAITEFN)
        IER=SYS$BINTIM('0 00:00:05.00',TIMADR)
   
	KILL = BTEST(BULL_USER_CUSTOM,1).AND.BTEST(BULL_USER_CUSTOM,3)
	IF (KILL) IER1 = 0

	FOUND = 0
	OUT = 6

	CONT = .FALSE.
	IF (INCMD(:3).EQ.'DIR') THEN
	   CONT = CLI$PRESENT('CONTINUE')
	ELSE IF (INCMD(:3).EQ.'   '.AND.NFOLDER.LT.0) THEN
	   CONT = .TRUE.
	END IF
	IF (CONT) THEN
	   CALL LIB$ERASE_PAGE(1,1)		! Clear the screen
	   SUBJECT = SUBJECT1
	   REPLY = REPLY1
	   SEARCH = SEARCH1
	   FROM_SEARCH = FROM_SEARCH1
	   SINCE = SINCE1
	   NEW = NEW1
           ANY_SEARCH = SUBJECT.OR.REPLY.OR.SEARCH.OR.FROM_SEARCH
	   IF (.NOT.ANY_SEARCH) THEN 
	      WRITE (6,'('' ERROR: No previous search to continue.'')')
	      RETURN
	   END IF
	   INCMD = ' '
	   LEN_P = 0
	   DIR_COUNT = DIR_COUNT1
	   NFOLDER = NFOLDER1
	   I = DIR_COUNT
	   IF (DIR_COUNT.EQ.-1) THEN 
	      I = SBULL - 1
	   END IF
	   GO TO 200
	END IF
	NFOLDER = 0

	IF (INCMD(:3).EQ.'DIR') THEN
	   IF (CLI$GET_VALUE('OUTPUT',BULL_PARAMETER,LEN_P)) THEN
	      OPEN(UNIT=3,FILE=BULL_PARAMETER(:LEN_P),IOSTAT=IER,
     &		DEFAULTFILE='.LIS',
     &	        RECL=LINE_LENGTH,STATUS='NEW',CARRIAGECONTROL='LIST')
	      IF (IER.NE.0) THEN
	         WRITE(6,1000) BULL_PARAMETER(:LEN_P)
		 RETURN
	      END IF
	      OUT = 3
	      INQUIRE (UNIT=3,NAME=BULL_PARAMETER)
	      WRITE (6,1040) BULL_PARAMETER(:TRIM(BULL_PARAMETER))
	   ELSE
	      CALL LIB$ERASE_PAGE(1,1)		! Clear the screen
	   END IF
	   IF (.NOT.CLI$PRESENT('SELECT_FOLDER')) THEN
	      IF (CLI$PRESENT('MARKED')) THEN
		 READ_TAG = 1 + IBSET(0,1)
	      ELSE IF (CLI$PRESENT('SEEN')) THEN
		 READ_TAG = 1 + IBSET(0,2)
	      ELSE IF (CLI$PRESENT('UNMARKED')) THEN
		 READ_TAG = 1 + IBSET(0,1) + IBSET(0,3)
	      ELSE IF (CLI$PRESENT('UNSEEN')) THEN
		 READ_TAG = 1 + IBSET(0,2) + IBSET(0,3)
	      ELSE IF (CLI$PRESENT('ALL')) THEN
		 READ_TAG = IBSET(0,1) + IBSET(0,2)
		 IF (REMOTE_SET.GE.3) THEN
		    BULL_POINT = F_START - 1
		 ELSE
		    BULL_POINT = 0
		 END IF
 	      END IF
	      IF (READ_TAG) THEN
	         IF (.NOT.(FOLDER_NUMBER.GE.0.OR.REMOTE_SET.GE.3)) THEN
		    WRITE (6,'('' ERROR: Invalid qualifier'',
     &			       '' with remote folder.'')')
		    READ_TAG = IBSET(0,1) + IBSET(0,2)
		    GO TO 9999
		 END IF
		 CALL GET_FIRST_TAG(FOLDER_NUMBER,IER,BULL_POINT)
	      END IF
	   END IF
	   SUBJECT = CLI$PRESENT('SUBJECT').OR.CLI$PRESENT('NOREPLIES')
	   REPLY = CLI$PRESENT('REPLY')
           REPLY_FIRST = REPLY
	   SEARCH = CLI$PRESENT('SEARCH')
	   FROM_SEARCH = CLI$PRESENT('FROM')
           ANY_SEARCH = SUBJECT.OR.REPLY.OR.SEARCH.OR.FROM_SEARCH
	   EXTRACTING = CLI$PRESENT('EXTRACT')
	   PRINTING = CLI$PRESENT('PRINT')
	   POSTTIME = CLI$PRESENT('POST')
	   NEW = CLI$PRESENT('NEW')
	   NEGATED = CLI$PRESENT('NEGATED')
	   IF (SEARCH) THEN
	      IER1 = CLI$GET_VALUE('SEARCH',SEARCH_STRING,SLEN)
	   ELSE IF (SUBJECT) THEN
	      IER1 = CLI$GET_VALUE('SUBJECT',SEARCH_STRING,SLEN)
           ELSE IF (FROM_SEARCH) THEN
              IER1 = CLI$GET_VALUE('FROM',SEARCH_STRING,SLEN)
	   ELSE IF (REPLY) THEN
	      SEARCH_STRING = ' '
	   ELSE IF (CLI$PRESENT('NOREPLIES')) THEN
	      SEARCH_STRING = 'RE:'
	      SLEN = 3
              NEGATED = .TRUE.
	   END IF

	   MATCH_MODE = 0
	   IF (CLI$PRESENT('MATCH')) THEN
	      CALL CLI$GET_VALUE('MATCH',BULL_PARAMETER,LEN_P)
	      IF (BULL_PARAMETER(:LEN_P).EQ.'AND') MATCH_MODE = 1
	      IF (BULL_PARAMETER(:LEN_P).EQ.'XOR') MATCH_MODE = 2
	   END IF
	ELSE
	   CALL LIB$ERASE_PAGE(1,1)		! Clear the screen
	   EXTRACTING = .FALSE.
	   PRINTING = .FALSE.
	   POSTTIME = .TRUE.
	   IF (INCMD(:3).EQ.'IND') THEN
	      SUBJECT = .FALSE.
	      REPLY = .FALSE.
              REPLY_FIRST = .FALSE.
	      SEARCH = .FALSE.
	      FROM_SEARCH = .FALSE.
              ANY_SEARCH = .FALSE.
	   ELSE
	      NEW = .FALSE.
	   END IF
	END IF
	OUTPUT = EXTRACTING.OR.PRINTING

	START = .FALSE.
	SINCE = .FALSE.
	IF (INCMD(:3).EQ.'DIR') THEN
	   IF (CLI$PRESENT('GROUP')) THEN
	      CALL INIT_QUEUE(SCRATCH_F1,GROUP)
	      SCRATCH_F = SCRATCH_F1
	      NGROUP = 0
	      DO WHILE (CLI$GET_VALUE('GROUP',GROUP)
     &	       .NE.%LOC(CLI$_ABSENT))	   	   ! Get the specified folders
	         NGROUP = NGROUP + 1
	         CALL LOWERCASE(GROUP)
	         CALL WRITE_QUEUE(%VAL(SCRATCH_F),SCRATCH_F,GROUP)
	      END DO
	      SCRATCH_F = SCRATCH_F1
	      CALL READ_QUEUE(%VAL(SCRATCH_F),SCRATCH_F,GROUP)
	      GLEN = TRIM(GROUP)
	      FEEDBACK = CLI$PRESENT('FEEDBACK')
	      NFOLDER = -1000
	      NFOLDER1 = -1000
	      SUBJECT1 = SUBJECT
	      REPLY1 = REPLY
	      SEARCH1 = SEARCH
	      FROM_SEARCH1 = FROM_SEARCH
	      I = SBULL - 1
	      SINCE = CLI$PRESENT('SINCE')
	      IF (SINCE) IER = CLI$GET_VALUE('SINCE',DATETIME)
	      SINCE1 = SINCE
	      NEW1 = NEW
	      GOTO 200
	   END IF
	END IF

C
C  Directory listing is first buffered into temporary memory storage before
C  being outputted to the terminal.  This is to be able to quickly close the
C  directory file, and to avoid the possibility of the user holding the screen,
C  and thus causing the directory file to stay open.  The temporary memory
C  is structured as a linked-list queue, where SCRATCH_D1 points to the header
C  of the queue.  See BULLSUBS.FOR for more description of the queue.
C

	CALL INIT_QUEUE(SCRATCH_D1,BULLDIR_ENTRY)
	SCRATCH_D = SCRATCH_D1
	CALL INIT_QUEUE(EXCLUDE_D1,%DESCR(I))
	EXCLUDE_D = EXCLUDE_D1
	NEXCLUDE = 0

	CALL OPEN_BULLDIR_SHARED		! Get directory file

	CALL READDIR(0,IER)			! Does directory header exist?
	NEWDIR = .FALSE.
	IF (IER.EQ.1.AND.NBULL.GT.0) THEN	! And are there messages?
	   IF (DIR_COUNT.EQ.0) THEN
	      NEWDIR = .TRUE.
	      EXPIRATION = CLI$PRESENT('EXPIRATION')
	      IF (CLI$PRESENT('START')) THEN	! Start number specified?
		 START = .TRUE.
	         IER = CLI$GET_VALUE('START',BULL_PARAMETER,LEN_P)
	         DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER) DIR_COUNT
		 IF (DIR_COUNT.LT.1) THEN
		    WRITE (6,'('' ERROR: Invalid starting message.'')')
		    CALL CLOSE_BULLDIR
		    DIR_COUNT = 0
		    GO TO 9999
		 END IF
	      ELSE IF (CLI$PRESENT('SINCE').OR.NEW) THEN
		 SINCE = CLI$PRESENT('SINCE')
	         IF (SINCE) IER = CLI$GET_VALUE('SINCE',DATETIME)
		 CALL GET_NEW_OR_SINCE(NEW,SINCE,IER,DATETIME)
		 IF (NEW.AND.IER.EQ.0) THEN	! was /NEW specified?
		    IF (REMOTE_SET.LT.3) THEN
		       WRITE (6,'('' No new messages are present in'',
     &			'' folder '',A,''.'')') FOLDER(:TRIM(FOLDER))
		       CALL CLOSE_BULLDIR
		       GO TO 9999
		    ELSE
		       WRITE (6,'('' No new messages are present in'',
     &			 '' folder '',A,''.'')')
     &			 FOLDER_NAME(:TRIM(FOLDER_NAME))
		       CALL CLOSE_BULLDIR
		       GO TO 9999
		    END IF
		 END IF

		 IF (IER.EQ.0) THEN
		    WRITE (6,'('' No messages past specified date.'')')
		    CALL CLOSE_BULLDIR
		    GO TO 9999
		 ELSE
		    DIR_COUNT = IER
		 END IF
	      ELSE
	         DIR_COUNT = BULL_POINT
		 IF (DIR_COUNT.EQ.0) DIR_COUNT = 1
	      END IF

	      IER1 = 0

	      IF (READ_TAG) THEN
	         IF (SUBJECT.OR.REPLY.OR.SEARCH.OR.FROM_SEARCH) THEN
		    WRITE (6,'('' ERROR: Qualifier not valid when '',
     &			''displaying only tagged messages.'')')
	            SUBJECT = .FALSE.
	            REPLY = .FALSE.
	      	    SEARCH = .FALSE.
	      	    FROM_SEARCH = .FALSE.
	      	    ANY_SEARCH = .FALSE.
		    CALL CLOSE_BULLDIR
		    GO TO 9999
		 END IF
	         IF (.NOT.(SINCE.OR.NEW.OR.START)) THEN
	            DIR_COUNT = 1
		 END IF
		 CALL READDIR(DIR_COUNT,IER1)
		 IF (IER1.EQ.DIR_COUNT+1) IER1 = 0
		 IF (REMOTE_SET.GE.3.OR.BTEST(READ_TAG,3)) THEN
		    MSG_NUM = DIR_COUNT-1
		 ELSE
		    CALL DECREMENT_MSG_KEY
	         END IF
	      END IF

	      IF (START.AND.DIR_COUNT.GT.NBULL) THEN
	         IF (READ_TAG) THEN
		    SBULL = NBULL + 1
		    GO TO 100
		 ELSE
		    START = .FALSE.
		    DIR_COUNT = NBULL
		 END IF
	      END IF
	      IF (SINCE.OR.NEW.OR.START) THEN
		 SBULL = DIR_COUNT
	         EBULL = DIR_COUNT + (PAGE_LENGTH - 7) - 1
	         IF (EBULL.GE.NBULL-2) EBULL = NBULL
	      ELSE
		 DIFF = 1
	         IF (REMOTE_SET.LT.3.AND.DIR_COUNT.NE.NBULL) THEN
		    CALL READDIR(DIR_COUNT,IER)
	   	    DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,
     &			       FOLDER_NUMBER+1),MSG_BTIM)
	            IF (LAST_READ_BTIM(1,FOLDER_NUMBER+1).EQ.MSG_BTIM(1)
     &		   .AND.LAST_READ_BTIM(2,FOLDER_NUMBER+1).EQ.MSG_BTIM(2))
     &			DIFF = 0
		    IF (READ_TAG) CALL DECREMENT_MSG_KEY
	         ELSE IF (DIR_COUNT.NE.F_NBULL) THEN
		    CALL NEWS_GET_NEWEST_MESSAGE(DIFF)
		    IF (DIFF.NE.0) THEN
		       DIFF = DIFF - DIR_COUNT - 1
	            ELSE
	               DIFF = 1
	            END IF
                 END IF
	         IF (DIFF.GT.0.AND.
     &		     NBULL-DIR_COUNT+1.LE.PAGE_LENGTH-5) THEN
	            EBULL = NBULL
	            SBULL = NBULL - (PAGE_LENGTH-5) + 1
	            IF (SBULL.LT.1) SBULL = 1
	         ELSE
	            SBULL = DIR_COUNT
	            EBULL = DIR_COUNT + (PAGE_LENGTH - 7) - 1
	            IF (EBULL.GE.NBULL-2) EBULL = NBULL
	         END IF
	      END IF

	      IER1 = 0
	      IF (REMOTE_SET.LT.3) F_START = 1
	      IF (DIR_COUNT.GT.F_START.AND.KILL.AND..NOT.(ANY_SEARCH.OR.START
     &		 .OR.SINCE.OR.NEW).AND.NEWDIR.AND..NOT.READ_TAG) THEN
       	         IF (REMOTE_SET.EQ.3) NEWGROUP = .TRUE.
	         I = DIR_COUNT
		 NUM = 0
	         SBULL = DIR_COUNT
	 	 DO WHILE (NUM.LT.PAGE_LENGTH-5.AND.I.LE.NBULL)
		    CALL READDIR(I,IER)
		    IF (I.EQ.NBULL) IER1 = 1
		    IF (I.EQ.DIR_COUNT.AND.I+1.NE.IER) DIR_COUNT = I + 1
		    IF (I+1.EQ.IER) THEN 
		       NUM = NUM + 1
	               IF (BTEST(SYSTEM,8)) THEN
	                  CALL WRITE_QUEUE(%VAL(EXCLUDE_D),EXCLUDE_D,
     &						%DESCR(-I))
		          NEXCLUDE = NEXCLUDE + 1
		       END IF
		    ELSE
	               CALL WRITE_QUEUE(%VAL(EXCLUDE_D),EXCLUDE_D,%DESCR(I))
		       NEXCLUDE = NEXCLUDE + 1
		    END IF
		    I = I + 1
		 END DO
		 IF (IER1.EQ.0.AND.NUM.GT.PAGE_LENGTH-7) NUM = PAGE_LENGTH - 7
	         IF (IER1.NE.0.AND.NUM.LT.PAGE_LENGTH-5.AND.
     &				SBULL.GT.F_START) THEN
	            I = SBULL - 1
		    NEXT = .FALSE.
	 	    DO WHILE (NUM.LT.PAGE_LENGTH-5.AND.I.GE.F_START)
		       CALL READDIR(I,IER)
		       IF (I.EQ.NBULL) IER1 = 1
		       IF (I+1.EQ.IER) THEN
			  NUM = NUM + 1
		          DIR_COUNT = I
	                  IF (BTEST(SYSTEM,8)) THEN
	                     CALL WRITE_QUEUE(%VAL(EXCLUDE_D),EXCLUDE_D,
     &						%DESCR(-I))
		             NEXCLUDE = NEXCLUDE + 1
		          END IF
		       ELSE
	                  CALL WRITE_QUEUE(%VAL(EXCLUDE_D),EXCLUDE_D,
     &					%DESCR(I))
		          NEXCLUDE = NEXCLUDE + 1
		       END IF
		       I = I - 1
		    END DO
		    NEXT = .TRUE.
	         END IF
	         SBULL = DIR_COUNT
	         EBULL = SBULL + NUM - 1
	      END IF
	   ELSE IF (DIR_COUNT.EQ.-1.AND..NOT.READ_TAG) THEN
	      SUBJECT = .FALSE.
	      REPLY = .FALSE.
              SEARCH = .FALSE.
              FROM_SEARCH = .FALSE.
	      SBULL = (SBULL - 1) - ((PAGE_LENGTH - 7) - 1)
	      IF (SBULL.LT.1) SBULL = 1
	      EBULL = SBULL + (PAGE_LENGTH - 7) - 1
	      IF (NBULL-SBULL+1.LE.PAGE_LENGTH-5) THEN
	         SBULL = NBULL - (PAGE_LENGTH-5) + 1
	         EBULL = NBULL
	         IF (SBULL.LT.1) SBULL = 1
	      END IF
	      IF ((REMOTE_SET.EQ.4.OR.KILL).AND.SBULL.GT.F_START) THEN
	         NUM = EBULL - SBULL + 1
	         I = EBULL
	         NEXT = .FALSE.
		 NUM1 = 0
		 EBULL = 0
	 	 DO WHILE (NUM.GT.0.AND.I.GE.F_START)
		    CALL READDIR(I,IER)
		    IF (I.EQ.NBULL) IER1 = 1
		    IF (I+1.EQ.IER) THEN
		       IF (EBULL.EQ.0) EBULL = I
		       NUM = NUM - 1
		       NUM1 = NUM1 + 1
		       SBULL = I
	               IF (BTEST(SYSTEM,8)) THEN
	                  CALL WRITE_QUEUE(%VAL(EXCLUDE_D),EXCLUDE_D,
     &						%DESCR(-I))
		          NEXCLUDE = NEXCLUDE + 1
		       END IF
		    ELSE
	               CALL WRITE_QUEUE(%VAL(EXCLUDE_D),EXCLUDE_D,%DESCR(I))
		       NEXCLUDE = NEXCLUDE + 1
	            END IF
		    IF (NUM.GT.0) I = I - 1
		 END DO
       	         IF (REMOTE_SET.EQ.3) NEWGROUP = .TRUE.
	         NEXT = .TRUE.
		 NUM = NUM1
	         IF (NUM.LE.PAGE_LENGTH-7) THEN
	            IF (IER1.EQ.0.AND.I.LE.F_START) THEN
		       I = EBULL
	               DO WHILE (I.LT.NBULL.AND.NUM.LE.PAGE_LENGTH-5)
	                  I = I + 1
	                  CALL READDIR(I,IER)
			  IF (I+1.EQ.IER) THEN
			     NUM = NUM + 1 
	               	     IF (BTEST(SYSTEM,8)) THEN
	                        CALL WRITE_QUEUE(%VAL(EXCLUDE_D),EXCLUDE_D,
     &						%DESCR(-I))
		                NEXCLUDE = NEXCLUDE + 1
		             END IF
			  ELSE
	                     CALL WRITE_QUEUE(%VAL(EXCLUDE_D),EXCLUDE_D,
     &						%DESCR(I))
		             NEXCLUDE = NEXCLUDE + 1
		          END IF
	               END DO
	               IF (NUM.GT.PAGE_LENGTH-5) NUM = PAGE_LENGTH-7
                    ELSE IF (IER1.NE.0.AND.I.GT.F_START) THEN
		       I = F_START - 1
		       J = 0
	               DO WHILE (J.LT.3.AND.I.LT.NBULL)
	                  I = I + 1
	                  CALL READDIR(I,IER)
			  IF (I+1.EQ.IER) THEN
			     J = J + 1
	                     IF (BTEST(SYSTEM,8)) THEN
	                        CALL WRITE_QUEUE(%VAL(EXCLUDE_D),EXCLUDE_D,
     &						%DESCR(-I))
		                NEXCLUDE = NEXCLUDE + 1
		             END IF
			  ELSE
	                     CALL WRITE_QUEUE(%VAL(EXCLUDE_D),EXCLUDE_D,
     &						%DESCR(I))
		             NEXCLUDE = NEXCLUDE + 1
		          END IF
	               END DO
	               IF (MSG_NUM.GE.SBULL) THEN 
			  NUM = NUM + 2 
	                  SBULL = F_START
	               END IF
	            END IF
	         END IF
	         EBULL = SBULL + NUM - 1
	      END IF
	   ELSE IF (DIR_COUNT.EQ.-1.AND.READ_TAG) THEN
100	      CALL GET_FIRST_TAG(FOLDER_NUMBER,IER,FIRST_BULL)
	      FIRST_BULL = FIRST_BULL + 1
	      DIR_COUNT = FIRST_BULL
	      IER1 = IER
	      IER = 0
	      FBULL = 0
	      EBULL = 0
	      LBULL = SBULL.GT.NBULL
	      DO WHILE (SBULL.GT.FIRST_BULL.AND.IER.EQ.0)
		 SBULL = SBULL - 1
	         CALL READDIR(SBULL,IER)
		 IF (IER.EQ.SBULL+1) THEN
	            CALL GET_THIS_TAG(FOLDER_NUMBER,IER,DIR_COUNT,DUMMY)
		    IF (IER.EQ.0) THEN
		       IF (FBULL.EQ.0) EBULL = DIR_COUNT
		       FBULL = FBULL +1
		       IF ((.NOT.LBULL.AND.FBULL.EQ.PAGE_LENGTH-7).OR.
     &			   (LBULL.AND.FBULL.EQ.PAGE_LENGTH-5)) THEN
		          IER = 1
		       END IF
		    ELSE
		       IER = 0
		    END IF
		 ELSE
		    IER = 1
		 END IF
	      END DO
	      IF (DIR_COUNT.EQ.FIRST_BULL.AND..NOT.LBULL) THEN
		 CALL READDIR(EBULL,IER)
		 IER = 0
		 DO WHILE (IER.EQ.0.AND.FBULL.LT.PAGE_LENGTH-7)
		    CALL GET_NEXT_TAG(FOLDER_NUMBER,IER,DIR_COUNT,DUMMY)
		    IF (IER.EQ.0) THEN
		       FBULL = FBULL + 1
		       EBULL = DIR_COUNT
		    END IF
		 END DO
		 DO I=1,3
		    CALL GET_NEXT_TAG(FOLDER_NUMBER,IER,DIR_COUNT,DUMMY)
		 END DO
		 IF (IER.NE.0) THEN
		    EBULL = DIR_COUNT
		    FBULL = FBULL + 2
	         END IF
	      END IF
	      CALL READDIR(EBULL,IER)
	      IF (EBULL+1.NE.IER) THEN
		 EBULL = EBULL + 1
	      ELSE
	         CALL GET_NEXT_TAG(FOLDER_NUMBER,IER,DUMMY,DUMMY1)
		 IF (IER.NE.0) EBULL = EBULL + 1
	      END IF
	      CALL READDIR(SBULL,IER)
	      IF (REMOTE_SET.GE.3.OR.BTEST(READ_TAG,3)) THEN
		 MSG_NUM = MSG_NUM-1
	      ELSE
		 CALL DECREMENT_MSG_KEY
	      END IF
	      EBULL = SBULL + FBULL - 1
	   ELSE
	      SBULL = DIR_COUNT
	      EBULL = DIR_COUNT + (PAGE_LENGTH - 7) - 1
	      IF (EBULL.GE.NBULL-2) EBULL = NBULL
	   END IF
	   IF (.NOT.PAGING.OR.OUTPUT.OR.OUT.EQ.3) EBULL = NBULL
	   IF (INCMD(:3).EQ.'DIR') THEN
	      IF (CLI$GET_VALUE('END',BULL_PARAMETER,LEN_P)) THEN
	         DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER) EBULL
	         EBULL = MIN(EBULL,NBULL)
	      END IF
	   END IF
	   IF (ANY_SEARCH) THEN
       	      IF (REMOTE_SET.EQ.3) NEWGROUP = .TRUE.
	   ELSE IF ((.NOT.REMOTE_SET.OR.KILL).AND..NOT.READ_TAG) THEN
       	      IF (REMOTE_SET.EQ.3) NEWGROUP = .TRUE.
	      EXCLUDE_D = EXCLUDE_D1
	      SEXC = NBULL + 1
	      LEXC = 0
	      DO I=1,NEXCLUDE
	         CALL READ_QUEUE(%VAL(EXCLUDE_D),EXCLUDE_D,%DESCR(J))
		 IF (J.LT.SEXC) SEXC = J
		 IF (J.GT.LEXC) LEXC = J
	      END DO
	      I1 = SBULL
	      I = SBULL
	      DO WHILE (I.LE.EBULL)
	         EXCLUDE_D = EXCLUDE_D1
		 J = 0
		 IER = I1
		 IF (I1.GE.SEXC.AND.I1.LE.LEXC) THEN 
		    N = NEXCLUDE
		    DO WHILE (N.GT.0.AND.J.EQ.0)
	               CALL READ_QUEUE(%VAL(EXCLUDE_D),EXCLUDE_D,%DESCR(J))
		       N = N - 1
		       IF (J.NE.I1.AND.J.NE.-I1) J = 0
		    END DO
	            IF (J.LE.0) THEN 
	               BULL_USER_CUSTOM = IBCLR(BULL_USER_CUSTOM,1)
		       CALL READDIR(I1,IER)
	               BULL_USER_CUSTOM = IBSET(BULL_USER_CUSTOM,1)
		       IF (J.LT.0) SYSTEM = IBSET(SYSTEM,8)
		    END IF
	         ELSE
	            CALL READDIR(I1,IER)
		 END IF
		 IF (KILL.AND.I1.EQ.NBULL) IER1 = 1
		 IF (IER.EQ.I1+1) THEN
	            CALL WRITE_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,
     &					BULLDIR_ENTRY)
		    I = I + 1
	         ELSE IF (I1.GE.NBULL) THEN
		    EBULL = I - 1
	         END IF
	         I1 = I1 + 1
	      END DO
	   ELSE IF (READ_TAG) THEN
	      I = 0
	      DO WHILE (I.LE.EBULL.AND.IER1.EQ.0)
		 CALL GET_NEXT_TAG(FOLDER_NUMBER,IER1,DIR_COUNT,TAG_TYPE)
		 IF (I.EQ.0.AND.IER1.EQ.0) THEN
		    EBULL = EBULL - SBULL + DIR_COUNT
		    SBULL = DIR_COUNT
		    I = SBULL
		 END IF
	 	 SYSTEM = SYSTEM.OR.ISHFT(TAG_TYPE,28)
	         CALL WRITE_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,BULLDIR_ENTRY)
		 I = I + 1
	      END DO
	      EBULL = I - 1
	      IF (IER1.NE.0) THEN
	         EBULL = EBULL - 1
	      ELSE
	         CALL GET_NEXT_TAG(FOLDER_NUMBER,IER1,DUMMY,TAG_TYPE)
		 IF (IER1.EQ.0) THEN
		    IER = 0
		    EBULL_SAVE = EBULL
		    DO I=1,2
		       IF (IER.EQ.0) THEN
			  SYSTEM = SYSTEM.OR.ISHFT(TAG_TYPE,28)
	                  CALL WRITE_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,
     &							BULLDIR_ENTRY)
			  EBULL = EBULL + 1
	                  CALL GET_NEXT_TAG(FOLDER_NUMBER,IER,DUMMY,
     &				TAG_TYPE)
		       END IF
		    END DO
		    IF (IER.NE.0) THEN
	               CALL GET_FIRST_TAG(FOLDER_NUMBER,IER,FIRST_BULL)
	               IF (SBULL.NE.FIRST_BULL+1) EBULL = EBULL_SAVE
		       IER1 = 1
		    ELSE
		       EBULL = EBULL_SAVE
		    END IF
		 END IF
	      END IF
	   ELSE
	      CALL REMOTE_DIRECTORY_COMMAND
     &				     (SBULL,EBULL,.FALSE.,SCRATCH_D,IER)
	      IF (IER.NE.0) THEN
	         CALL CLOSE_BULLDIR
		 CALL DISCONNECT_REMOTE
		 GO TO 9999
	      END IF
	   END IF
	ELSE
	   NBULL = 0
	END IF

	IF (NBULL.EQ.0.OR.EBULL.LT.SBULL) THEN
	   CALL CLOSE_BULLDIR			! We don't need file anymore
	   IF (READ_TAG) THEN
	      IF (BTEST(READ_TAG,1).AND.BTEST(READ_TAG,3)) THEN
		 DIR_TYPE = 'unmarked'
	      ELSE IF (BTEST(READ_TAG,2).AND.BTEST(READ_TAG,3)) THEN
		 DIR_TYPE = 'unseen'
	      ELSE IF (BTEST(READ_TAG,1)) THEN
		 DIR_TYPE = 'marked'
	      ELSE IF (BTEST(READ_TAG,2)) THEN
		 DIR_TYPE = 'seen'
	      END IF
	      WRITE (6,'('' No '',A,'' messages are present in'',
     &		     '' folder '',A,''.'')')
     &		DIR_TYPE(:TRIM(DIR_TYPE)),FOLDER_NAME(:TRIM(FOLDER_NAME))
	   ELSE
	      IF (INCMD(:3).EQ.'DIR'.AND.ANY_SEARCH) THEN
	         IF (.NOT.CLI$PRESENT('START').AND.
     &               .NOT.CLI$PRESENT('CONT')) THEN
	             WRITE (6,'('' No matches found starting search'',
     &			'' from message number '',I)') SBULL
		    DIR_COUNT = -1
		    GO TO 9999
	         END IF
	      END IF
	      WRITE (6,'('' There are no messages present.'')')
	   END IF
	   DIR_COUNT = -1
	   GO TO 9999
	END IF

C
C  Directory entries are now in queue.  Output queue entries to screen.
C

	IF (NFOLDER.EQ.0) CALL DIRECTORY_HEADER
     &		(OUTLINE,PRINTING,EXTRACTING,EXPIRATION,OUT)

	TAG = (BULL_TAG.AND.(REMOTE_SET.EQ.0.OR.REMOTE_SET.EQ.1)).OR.
     &			(BULL_NEWS_TAG.AND.REMOTE_SET.GE.3)

	IF (.NOT.ANY_SEARCH.AND.TAG.AND..NOT.READ_TAG) THEN
	   IF (INCMD(:3).NE.'   ') THEN
	      SCRATCH_D = SCRATCH_D1		! Init queue pointer to header
	      CALL READ_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,BULLDIR_ENTRY)
	      CALL GET_THIS_OR_NEXT_TAG(FOLDER_NUMBER,IER,NEXT_TAG,TAG_TYPE)
	      IF (IER.NE.0) NEXT_TAG = NBULL + 1
	   END IF
	   SCRATCH_D = SCRATCH_D1		! Init queue pointer to header
	   DO I=SBULL,EBULL
	      SAVE_SCRATCH_D = SCRATCH_D
	      CALL READ_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,BULLDIR_ENTRY)
	      IF (TAG.AND.MSG_NUM.EQ.NEXT_TAG) THEN
		 SYSTEM = SYSTEM.OR.ISHFT(TAG_TYPE,28)
	         CALL WRITE_QUEUE(%VAL(SAVE_SCRATCH_D),DUMMY,BULLDIR_ENTRY)
	         CALL GET_NEXT_TAG(FOLDER_NUMBER,IER,NEXT_TAG,TAG_TYPE)
	         IF (IER.NE.0) NEXT_TAG = NBULL + 1
	      END IF
	   END DO
	END IF

	CALL CLOSE_BULLDIR			! We don't need file anymore

	SCRATCH_D = SCRATCH_D1			! Init queue pointer to header

	I = SBULL
	START_SEARCH = I
	IF (.NOT.REPLY_FIRST) THEN
	   START_SEARCH = I - 1
	ELSE IF (.NOT.CLI$PRESENT('START')) THEN
	   START_SEARCH = BULL_POINT
	END IF
200	CLOSED = .FALSE.
	IF (ANY_SEARCH.OR.OUTPUT) THEN
	   NUM = 0
	   IF (NFOLDER.NE.-1000) THEN
	      CLOSED = .TRUE.
	      CALL OPEN_BULLDIR_SHARED
	      IF (SEARCH.OR.OUTPUT) CALL OPEN_BULLFIL_SHARED
	   END IF
	   CLOSED_FILES = .FALSE.
	   SEARCH_NUM = 1
	   REVERSE = .FALSE.
	END IF
	DO WHILE (I.LE.EBULL.AND.FOUND.GT.-3)
	   IF (.NOT.ANY_SEARCH) THEN
	      CALL READ_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,BULLDIR_ENTRY)
	   ELSE
 	      IF (NFOLDER.NE.-1000.AND.I.GE.SBULL)
     &		 CALL GET_SEARCH(FOUND,SEARCH_STRING,SEARCH_NUM,SLEN,0,
     &		    START_SEARCH,REVERSE,SUBJECT,REPLY_FIRST,.FALSE.,
     &		    .TRUE.,FROM_SEARCH,NEGATED,.FALSE.)
	      IF (INCMD(:3).NE.'   '.AND.TAG.AND.FOUND.GT.0) THEN
	         CALL GET_THIS_OR_NEXT_TAG(FOLDER_NUMBER,IER,NEXT_TAG,
     &		    TAG_TYPE)
	         IF (IER.NE.0) NEXT_TAG = NBULL + 1
		 NEXT = .FALSE.
		 CALL READDIR(FOUND,IER)
		 NEXT = .TRUE.
	      END IF
	      REPLY_FIRST = .FALSE.
	      IF (FOUND.GT.0) THEN
		 IF (NFOLDER.LT.0.AND.(I.EQ.F_START.OR.I.EQ.DIR_COUNT)) THEN
	   	    IF (FEEDBACK) CALL LIB$ERASE_PAGE(1,1)
		    CALL DIRECTORY_HEADER
     &			(OUTLINE,PRINTING,EXTRACTING,EXPIRATION,OUT)
		    DIR_COUNT = 0
		    BULL_POINT = MSG_NUM - 1
	   	    PRINT_HEADER = .TRUE.
		 END IF
		 SEARCH_STRING = ' '
		 START_SEARCH = FOUND
	         IF (TAG.AND.MSG_NUM.EQ.NEXT_TAG) THEN
	            CALL GET_NEXT_TAG(FOLDER_NUMBER,IER,NEXT_TAG,DUMMY)
	            IF (IER.NE.0) NEXT_TAG = NBULL + 1
		    NEXT = .FALSE.
		    CALL READDIR(FOUND,IER)
		    NEXT = .TRUE.
		    SYSTEM = SYSTEM.OR.ISHFT(TAG_TYPE,28)
		    TAG_TYPE = DUMMY
	         END IF
		 IF (NFOLDER.LT.0.AND..NOT.OUTPUT) THEN 
	            NUM = NUM + 1
	 	    IF (NUM.EQ.PAGE_LENGTH-6) I = EBULL + 1
		 END IF
	      ELSE IF (NFOLDER.LT.0.AND.(OUTPUT.OR.I.LE.SBULL)) THEN
		 IF (CLOSED) THEN
		    IF (SEARCH.OR.OUTPUT) CALL CLOSE_BULLFIL
		    CALL CLOSE_BULLDIR
		    CLOSED = .FALSE.
		 END IF
		 GFOUND = .FALSE.
	       	 CALL DECLARE_CTRLC_AST
		 DO WHILE (.NOT.GFOUND.AND.NGROUP.GT.0.AND.FLAG.NE.1)
	            CALL OPEN_BULLNEWS_SHARED
		    CALL READ_FOLDER_FILE_KEYNUM_TEMP(-NFOLDER,IER)
		    IF (IER.EQ.0) 
     &		       CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER)
		    DO WHILE (IER.EQ.0.AND.BTEST(FOLDER1_FLAG,10))
		       CALL READ_FOLDER_FILE_TEMP(IER)
	            END DO
		    IF (IER.NE.0) NFOLDER = 0
		    DO WHILE (.NOT.GFOUND.AND.NFOLDER.NE.0.AND.FLAG.NE.1)
	               CALL GET_NEXT_GROUP(.TRUE.,GROUP,GLEN,GFOUND,
     &				        .FALSE.,STAT,IER,.TRUE.)
		       CALL CLOSE_BULLFOLDER
		       IF (GFOUND) THEN
	      	          START_BULL = 0
		          OLD_FOLDER_NUMBER = FOLDER_NUMBER
	                  FOLDER_NUMBER = -1
		          FOLDER1 = FOLDER1_DESCRIP(
     &		          		:INDEX(FOLDER1_DESCRIP,' '))
	                  IF (F1_START.GT.0.AND.F1_START.LE.F1_NBULL) THEN
			     CALL SELECT_FOLDER(.FALSE.,IER)
			     START_SEARCH = F_START - 1
			     IF (IER.AND.(NEW.OR.SINCE)) THEN
				CALL OPEN_BULLDIR_SHARED
			        CALL GET_NEW_OR_SINCE(NEW,SINCE,IER1,DATETIME)
			        CALL CLOSE_BULLDIR
				IF (IER1.NE.0) THEN
				   START_SEARCH = IER1 - 1
				ELSE
				   IER = 0
				END IF
			     END IF
			  END IF
		          IF (.NOT.IER.OR.F1_START.EQ.0.OR.
     &			      F1_START.GT.F1_NBULL) THEN
		             FOLDER_NUMBER = OLD_FOLDER_NUMBER
		             CALL OPEN_BULLNEWS_SHARED
			     CALL READ_FOLDER_FILE_KEYNUM_TEMP(FOLDER1_NUMBER,
     &								IER)
		             CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER)
		             GFOUND = .FALSE.
			     IF (FLAG.EQ.1) CALL CLOSE_BULLFOLDER
		          ELSE
	   	             IF (FEEDBACK) WRITE (6,'('' Searching '',A)')
     &		          		FOLDER_NAME(:TRIM(FOLDER_NAME))
		             CHANGE = .TRUE.
		             NFOLDER = -FOLDER_NUMBER
			     SBULL = F_START
			     I = SBULL - 1
			     EBULL = F_NBULL			     
		          END IF
		       ELSE
		          NFOLDER = 0
		       END IF
		    END DO
		    IF (NFOLDER.EQ.0) NGROUP = NGROUP - 1
		    IF (NFOLDER.EQ.0.AND.NGROUP.GT.0) THEN
		       CALL READ_QUEUE(%VAL(SCRATCH_F),SCRATCH_F,GROUP)
		       GLEN = TRIM(GROUP)
		       NFOLDER = -1000
		    ELSE IF (NFOLDER.EQ.0) THEN 
		       WRITE (6,'('' No messages found.'')')
		    END IF
		    IF (NFOLDER.EQ.0) I = EBULL + 1
 	            IF (NFOLDER.LT.-1000) THEN 
		       CALL OPEN_BULLDIR_SHARED
	               IF (SEARCH.OR.OUTPUT) CALL OPEN_BULLFIL_SHARED
	               CLOSED = .TRUE.
	            END IF
		    NFOLDER1 = NFOLDER
	         END DO
		 IF (FLAG.EQ.1) THEN
		    WRITE (6,'('' Search aborted.'')')
		    I = EBULL + 1
		 END IF
	     	 CALL CANCEL_CTRLC_AST
	      ELSE
		 I = EBULL + 1
	      END IF
	      IER = SYS$SETIMR(%VAL(WAITEFN),TIMADR,CLOSE_FILES,)
	   END IF
	   IF (I.GE.SBULL.AND.I.LE.EBULL.AND.NFOLDER.NE.-1000) THEN
	      CALL CONVERT_ENTRY_FROMBIN_FOLDER
	      IF (BTEST(SYSTEM,30)) THEN
		 OUTLINE  = '>'
	      ELSE IF (BTEST(SYSTEM,8)) THEN
		 OUTLINE  = '#'
	      ELSE
		 OUTLINE  = ' '
	      END IF
	      IF (BTEST(SYSTEM,29)) THEN
		 OUTLINE(2:)  = '*'
	      ELSE
		 OUTLINE(2:)  = ' '
	      END IF
	      N = MAX(INT(LOG10(REAL(MSG_NUM)))+1,3)
	      IF (EXDATE(8:12).LT.'1994'.AND.REMOTE_SET.NE.3) THEN
	         WRITE(OUTLINE(3:),2010) MSG_NUM,DESCRIP(:54-N),FROM,
     &							'(DELETED)'
	      ELSE IF (EXPIRATION) THEN
	         IF (BTEST(SYSTEM,2)) THEN		! Shutdown bulletin?
		    EXPIRES = 'Shutdown'
	         ELSE IF (BTEST(SYSTEM,1)) THEN		! Permanent bulletin?
		    EXPIRES = 'Permanent'
	         ELSE IF (EXDATE(8:9).EQ.'18'.AND.REMOTE_SET.EQ.3) THEN
		    EXPIRES = 'Unknown'
	         ELSE
		    EXPIRES = EXDATE(:7)//EXDATE(10:11)
	         END IF
	         WRITE(OUTLINE(3:),2010) MSG_NUM,DESCRIP(:54-N),FROM,
     &						EXPIRES(:9)
	      ELSE
	         WRITE(OUTLINE(3:),2010) MSG_NUM,DESCRIP(:54-N),FROM,
     &						DATE(:7)//DATE(10:11)
	      END IF
	      NOTHING = .FALSE.
	      IF (OUT.EQ.6) THEN 
	         WRITE(OUT,'(1X,A)') OUTLINE
	      ELSE
	         WRITE(OUT,'(A)') OUTLINE
	      END IF
	      IF (OUTPUT) THEN
		 FOUND_MSG = .TRUE.
		 CALL SYS$SETAST(%VAL(0))
		 NEXT = .FALSE.
	         IF (PRINTING) THEN
	            CALL PRINT(MSG_NUM,CLOSED_FILES)
	         ELSE
		    CALL FILE(MSG_NUM,CLOSED_FILES,PRINT_HEADER)
	   	    PRINT_HEADER = .FALSE.
		    IF (MSG_NUM.GT.0) THEN
		       I = EBULL
	               FOUND = 0
                    ELSE
		       MSG_NUM = -MSG_NUM
	            END IF
	         END IF
		 NEXT = .TRUE.
		 CALL SYS$SETAST(%VAL(1))
	      END IF
	   END IF
	   I = I + 1
	   IF (ANY_SEARCH) IER = SYS$CANTIM(,)
	END DO

	DIR_COUNT = MSG_NUM + 1			! Update directory counter

	IF (ANY_SEARCH.OR.OUTPUT) THEN
	   IF (CLOSED) THEN
	      IF (SEARCH.OR.OUTPUT) CALL CLOSE_BULLFIL
	      CALL CLOSE_BULLDIR
	   END IF
	   IF (ANY_SEARCH) THEN
	      IF (FOUND.GT.0) THEN
	         DIR_COUNT = FOUND + 1
	      ELSE
	         DIR_COUNT = NBULL + 1
	      END IF
	   END IF
	END IF

	IF (DIR_COUNT.GT.NBULL
     &		.OR.((READ_TAG.OR.KILL).AND.IER1.NE.0)) THEN
						! Outputted all entries?
	   IF (PRINTING) THEN 
	      IF (CLI$PRESENT('NOW').AND.FOUND_MSG) THEN
	         INCMD = 'PRINT/NOW'
	         IER = CLI$DCL_PARSE(INCMD,BULLETIN_SUBCOMMANDS)
	         CALL PRINT(MSG_NUM,CLOSED_FILES)
	      END IF
	   ELSE IF (EXTRACTING.AND.FOUND_MSG) THEN
	      CALL FILE(0,CLOSED_FILES,.FALSE.)
	   END IF
	   IF (NFOLDER.LT.0) THEN 
	      IF (FLAG.EQ.1) WRITE(6,1020)
	   ELSE
	      DIR_COUNT = -1			! Yes. Set counter to -1.
	   END IF
	ELSE IF (NFOLDER.NE.0.OR.(-NFOLDER1.EQ.FOLDER_NUMBER.AND.
     &		   INCMD(:1).EQ.' ')) THEN
	   IF (FLAG.EQ.1) WRITE(6,1020)
	ELSE IF (FLAG.NE.1) THEN 
	   WRITE(6,1010)			! Else say there are more
	END IF

9999	POSTTIME = .FALSE.
	NEXT = .FALSE.
	DIRMODE = .FALSE.
	IF (NFOLDER.NE.0.OR.-NFOLDER1.EQ.FOLDER_NUMBER) THEN
	   DIR_COUNT1 = DIR_COUNT
	   IF (DIR_COUNT1.GT.NBULL) DIR_COUNT1 = -1
	END IF
	IF (OUT.EQ.3) CLOSE (UNIT=3)
	RETURN

1000	FORMAT(' ERROR: Error in opening file ',A,'.')
1010	FORMAT(1X,/,' Press RETURN for more...',/)
1020	FORMAT(1X,/,' Press RETURN for more, type SEARCH to read ',
     &		'these messages.',/)
1040	FORMAT(' Output being written to ',A,'.')

2010	FORMAT(I<N>,1X,A<54-N>,1X,A12,1X,A9)

	END


	SUBROUTINE CLOSE_FILES

	IMPLICIT INTEGER (A-Z)

	COMMON /CLOSE_FILES_INFO/ CLOSED_FILES

	INQUIRE(UNIT=1,OPENED=IER)
	IF (IER) CALL CLOSE_BULLFIL

	INQUIRE(UNIT=2,OPENED=IER)
	IF (IER) CALL CLOSE_BULLDIR

	CLOSED_FILES = .TRUE.

	RETURN
	END



	SUBROUTINE GET_MSGKEY(BTIM,MSG_KEY)

	IMPLICIT INTEGER (A-Z)

	INTEGER BTIM(2)

	CHARACTER*8 MSG_KEY,INPUT

	CALL LIB$MOVC3(8,BTIM(1),%REF(INPUT))

	DO I=1,8
	   MSG_KEY(I:I) = INPUT(9-I:9-I)
	END DO

	RETURN
	END



	SUBROUTINE FILE(FILE_NUM,OPEN_IT,PRINT_HEADER)
C
C  SUBROUTINE FILE
C
C  FUNCTION:  Copies a bulletin to a file.
C
	IMPLICIT INTEGER (A - Z)

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /READ_DISPLAY/ LINE_OFFSET

	COMMON /FILE_DIRECTORY/ FILE_DIRECTORY
	CHARACTER*64 FILE_DIRECTORY

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'

	EXTERNAL CLI$_ABSENT

	CHARACTER*128 FILENAME

	DATA OPENED /.FALSE./

	IF (CAPTIVE(1)) THEN
	   WRITE (6,'('' ERROR: Command invalid from CAPTIVE account.'')')
	   RETURN
	END IF

10	IF (FILE_NUM.EQ.0) THEN
	IF (.NOT.OPEN_IT) THEN
	   OPENED = .FALSE.
	   CLOSE (UNIT=3)
	   RETURN
	END IF
	IF (OPENED) THEN
	   CALL CLOSE_BULLFIL
	   CALL CLOSE_BULLDIR
	   CLOSE (UNIT=3)			! Bulletin copy completed
	   OPENED = .FALSE.
	   RETURN
	END IF
	IER = CLI$GET_VALUE('BULLETIN_NUMBER',BULL_PARAMETER,LEN_P)
	IF (IER.NE.%LOC(CLI$_ABSENT)) THEN	! Was bulletin specified?
	   CALL GET_2_VALS(BULL_PARAMETER,LEN_P,SBULL,EBULL,IER)
	   IF (EBULL.GT.F_NBULL) EBULL = F_NBULL
	ELSE IF (CLI$PRESENT('ALL')) THEN
	   SBULL = 1
	   EBULL = F_NBULL
	   IER = 0
	ELSE IF (BULL_POINT.EQ.0) THEN	! No.  Have we just read a bulletin?
	   WRITE(6,1010)		! No, then error.
	   RETURN
	ELSE
	   SBULL = BULL_POINT
	   EBULL = SBULL
	   IER = 0
	END IF

	IF (SBULL.LE.0.OR.IER.NE.0.OR.EBULL.LT.SBULL) THEN
	   WRITE (6,1015)
	   IF (OPENED) THEN
	      CALL CLOSE_BULLFIL
	      CALL CLOSE_BULLDIR
	      CLOSE (UNIT=3)			! Bulletin copy completed
	      OPENED = .FALSE.
	   END IF
	   WRITE (6,'(1X,A)') BULL_PARAMETER(:LEN_P)
	   RETURN
	END IF
	ELSE
	   SBULL = FILE_NUM
	   EBULL = SBULL
	END IF

	IF (.NOT.OPENED) THEN
	   IER = CLI$GET_VALUE('EXTRACT',FILENAME,LEN_F)

	   IF (.NOT.IER) THEN
	      FILENAME = FOLDER
	      DO I=1,LEN(FILENAME)
	         IF (FILENAME(I:I).EQ.'.') FILENAME(I:I) = '_'
	      END DO
	      FILENAME = FILENAME(:TRIM(FILENAME))//'.TXT'
	      LEN_F = TRIM(FILENAME)
	   END IF

	   IF (TRIM(FILE_DIRECTORY).GT.0.AND.INDEX(FILENAME,':').EQ.0
     &	       .AND.INDEX(FILENAME,'[').EQ.0) THEN
	      FILENAME = FILE_DIRECTORY(:TRIM(FILE_DIRECTORY))//FILENAME
	      LEN_F = TRIM(FILENAME)
	   END IF

	   CALL STR$UPCASE(FILENAME,FILENAME)

	   CALL DISABLE_PRIVS

	   IF (CLI$PRESENT('NEW')) THEN
	      OPEN(UNIT=3,FILE=FILENAME(:LEN_F),ERR=900,
     &	        RECL=LINE_LENGTH,STATUS='NEW',CARRIAGECONTROL='LIST')
	   ELSE
	      OPEN(UNIT=3,FILE=FILENAME(:LEN_F),IOSTAT=IER,
     &		RECL=LINE_LENGTH,
     &		STATUS='OLD',CARRIAGECONTROL='LIST',ACCESS='APPEND')
	      IF (IER.NE.0) THEN
	         OPEN(UNIT=3,FILE=FILENAME(:LEN_F),ERR=900,
     &	           RECL=LINE_LENGTH,STATUS='NEW',CARRIAGECONTROL='LIST')
	      ELSE IF (CLI$PRESENT('FF')) THEN
	         WRITE (3,'(A)') CHAR(12)
	      END IF
	   END IF

	   CALL ENABLE_PRIVS			! Reset SYSPRV privileges

	   HEAD = CLI$PRESENT('HEADER')

	   IF (OPEN_IT) THEN
	      CALL OPEN_BULLDIR_SHARED
	      CALL OPEN_BULLFIL_SHARED	! Open BULLETIN file
	   END IF
	   OPENED = .TRUE.
	   FIRST = .TRUE.
	END IF

	IF (PRINT_HEADER) THEN
	   WRITE (3,'(/,''Newsgroup: '',A)')
     &		FOLDER_NAME(:TRIM(FOLDER_NAME))
	END IF

	DO FBULL = SBULL,EBULL
	   FBULL1 = FBULL
	   CALL READDIR(FBULL,IER)	! Get info for specified bulletin

	   IF (IER.NE.FBULL+1.OR.FBULL.GT.EBULL.OR.(.NOT.CLI$PRESENT
     &		('ALL').AND.FBULL1.EQ.SBULL.AND.FBULL.NE.SBULL)) THEN
	      IF (REMOTE_SET.LT.3.OR.FBULL1.EQ.SBULL) WRITE(6,1030) FBULL1
	      IF (FBULL1.GT.SBULL) GO TO 100
	      CLOSE (UNIT=3,STATUS='DELETE')
	      OPENED = .FALSE.
	      IF (OPEN_IT) THEN
	         CALL CLOSE_BULLFIL
	         CALL CLOSE_BULLDIR
              END IF
	      RETURN
	   ELSE IF (REMOTE_SET) THEN
	      CALL REMOTE_READ_MESSAGE(FBULL,IER1)
	      IF (IER1.GT.0) THEN
	         CALL DISCONNECT_REMOTE
	      ELSE
	         CALL GET_REMOTE_MESSAGE(IER1)
	      END IF
	      IF (IER1.NE.0) GO TO 100
	   END IF

	   IF (.NOT.FIRST.AND.CLI$PRESENT('FF')) THEN
	      WRITE (3,'(A)') CHAR(12)
	   ELSE IF (FIRST) THEN
	      FIRST = .FALSE.
	   END IF

	   ILEN = LINE_LENGTH + 1

	   CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN
	      IF (HEAD) WRITE(3,1060) INPUT(7:ILEN),DATE//' '//TIME(:8)
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   ELSE IF (HEAD) THEN
	      WRITE(3,1060) FROM,DATE//' '//TIME(:8)
	   END IF
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THEN
	      IF (HEAD) WRITE(3,1050) INPUT(7:ILEN)
	   ELSE
	      IF (HEAD) WRITE(3,1050) DESCRIP
	      IF (ILEN.GT.0) WRITE (3,'(A)') INPUT(:ILEN)
	   END IF

	   DO WHILE (ILEN.GT.0)		! Copy bulletin into file
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      IF (ILEN.GT.0) WRITE (3,'(A)') INPUT(:ILEN)
	   END DO
	END DO

100     IF (FILE_NUM.GT.0) THEN
	   FILE_NUM = -FILE_NUM
	   RETURN
	END IF

	IER = OTS$CVT_L_TI(SBULL,BULL_PARAMETER,,,)
 	IF (SBULL.EQ.EBULL) THEN	! Show name of file created.
	   WRITE(6,1040)
     &	      BULL_PARAMETER(FIRST_ALPHA(BULL_PARAMETER):),
     &	      FILENAME(:LEN_F)
	ELSE
	   WRITE(6,1045)
     &	      BULL_PARAMETER(FIRST_ALPHA(BULL_PARAMETER):)
	   IER = OTS$CVT_L_TI(EBULL,BULL_PARAMETER,,,)
           WRITE(6,1046)
     &	      BULL_PARAMETER(FIRST_ALPHA(BULL_PARAMETER):),
     &	      FILENAME(:LEN_F)
	END IF

	GO TO 10

900	WRITE(6,1000) FILENAME(:LEN_F)
	CALL ENABLE_PRIVS		! Reset BYPASS privileges
	RETURN

1000	FORMAT(' ERROR: Error in opening file ',A,'.')
1010	FORMAT(' ERROR: You have not read any bulletin.')
1015	FORMAT(' ERROR: Specified message number has incorrect format:')
1030	FORMAT(' ERROR: Following bulletin was not found: ',I)
1040	FORMAT(' Message ',A,' written to ',A)
1045	FORMAT(' Messages ',A,'-',$)
1046	FORMAT('+',A,' written to ',A)
1050	FORMAT('Subj: ',A,/)
1060	FORMAT(/,'From: ',A,/,'Date: ',A)

	END



	SUBROUTINE COPY2(OUT,IN)

	CALL LIB$MOVC3(8,IN,OUT)

	RETURN
	END



	SUBROUTINE LOGIN
C
C  SUBROUTINE LOGIN
C
C  FUNCTION: Alerts user of new messages upon logging in.
C
	IMPLICIT INTEGER (A - Z)

        INCLUDE 'BULLFILES.INC'
 
	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	COMMON /READIT/ READIT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /PAGE/ PAGE_LENGTH,PAGE_WIDTH,PAGING
	LOGICAL PAGING

	COMMON /POINT/ BULL_POINT

	COMMON /PROMPT/ COMMAND_PROMPT
	CHARACTER*40 COMMAND_PROMPT

	COMMON /SYSTEM_FOLDERS/ SYSTEM_FLAG(FLONG),DUMMY(2)

	COMMON /COMMAND_SWITCHES/ LOGIN_SWITCH,SYSTEM_SWITCH
	COMMON /COMMAND_SWITCHES/ SYSTEM_LOGIN_BTIM(2)
	COMMON /COMMAND_SWITCHES/ REVERSE_SWITCH,SEPARATE
	CHARACTER*4 SEPARATE

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	CHARACTER TODAY*24,INREAD*4

	DATA CTRL_G/7/

	DATA GEN_DIR1/0/	! General directory link list header
	DATA SYS_DIR1/0/	! System directory link list header
	DATA SYS_NUM1/0/	! System message number link list header
	DATA SYS_BUL1/0/	! System bulletin link list header
	DATA ALL_DIR1/0/	! Full directory link list header (for remote)

	DATA PAGE/0/

	DATA FIRST_WRITE/.TRUE./
	LOGICAL FIRST_WRITE

	COMMON /LOGIN_BTIM/ LOGIN_BTIM_SAVE(2)

	DIMENSION NOLOGIN_BTIM(2),TODAY_BTIM(2)
	DIMENSION NEW_BTIM(2),PASSCHANGE(2),BULLCP_BTIM(2)
	DIMENSION LOGIN_BTIM_OLD(2),LOGIN_BTIM_NEW(2)

	COMMON /SHUTDOWN/ NODE_NUMBER,NODE_AREA
	COMMON /SHUTDOWN/ SHUTDOWN_FLAG(FLONG)

	FOLDER_NAME = FOLDER

	CALL SYS$ASCTIM(,TODAY,,)		! Get the present time
	CALL SYS_BINTIM(TODAY,TODAY_BTIM)

	CALL SYS_BINTIM('5-NOV-2956 00:00:00.00',NOLOGIN_BTIM)
	CALL SYS_BINTIM('5-NOV-1956 11:05:56',NEW_BTIM)

C
C  Find user entry in BULLUSER.DAT to update information and
C  to get the last date that messages were read.
C

	CALL OPEN_BULLUSER_SHARED

	CALL READ_USER_FILE_HEADER(IER)		! Get the header

	IF (IER.EQ.0) THEN			! Header is present.
	   UNLOCK 4
	   CALL READ_USER_FILE_KEYNAME(USERNAME,IER1)
						! Find if there is an entry
	   IF (NEW_FLAG(1).LT.143.OR.NEW_FLAG(1).GT.143) THEN
	      NEW_FLAG(2)=0		! If old version clear GENERIC value
	      NEW_FLAG(1)=143		! Set new version number
	   END IF
	   IF (IER1.EQ.0) THEN			! There is a user entry
	      IF (COMPARE_BTIM(LOGIN_BTIM,NOLOGIN_BTIM).GE.0) THEN
						! DISMAIL or SET LOGIN set
		 IF (CLI$PRESENT('ALL')) THEN
		    CALL COPY2(LOGIN_BTIM,TODAY_BTIM(1))
		 ELSE
		    RETURN			! Don't notify
	         END IF
	      END IF
	      CALL COPY2(LOGIN_BTIM_SAVE,LOGIN_BTIM)
	      CALL COPY2(LOGIN_BTIM,TODAY_BTIM)
	      REWRITE (4) USER_ENTRY
	      IF (SYSTEM_FLAG(1).NE.0.AND.SYSTEM_FLAG(1).NE.1) READIT = 1
	      DO I = 1,FLONG
		 IF (SET_FLAG(I).NE.0.OR.BRIEF_FLAG(I).NE.0.OR.
     &		    (I.GT.1.AND.SYSTEM_FLAG(I).NE.0)) READIT = 1
	      END DO
	   ELSE
	      CALL CLEANUP_LOGIN		! Good time to delete dead users
	      CALL COPY2(READ_BTIM,NEW_BTIM)	! Make new entry
	      DO I = 1,FLONG
	         SET_FLAG(I) = SET_FLAG_DEF(I)
	         BRIEF_FLAG(I) = BRIEF_FLAG_DEF(I)
		 NOTIFY_FLAG(I) = NOTIFY_FLAG_DEF(I)
	      END DO
	      NEW_FLAG(1) = 143
	      NEW_FLAG(2) = 0
	      CALL CHECK_NEWUSER(USERNAME,DISMAIL,PASSCHANGE)
	      IF (DISMAIL.EQ.1) THEN
		 CALL COPY2(LOGIN_BTIM,NOLOGIN_BTIM)
	         CALL COPY2(LOGIN_BTIM_SAVE,LOGIN_BTIM)
	      ELSE
	         CALL COPY2(LOGIN_BTIM_SAVE,NEW_BTIM)
		 CALL COPY2(LOGIN_BTIM,TODAY_BTIM)
	         DO I = 1,FLONG
		    IF (SET_FLAG(I).NE.0) READIT = 1
	         END DO
		 IF (COMPARE_BTIM(PASSCHANGE,NEWEST_BTIM).LT.0) IER1 = 0
			! Old password change indicates user is new to BULLETIN
			! but not to system, so don't limit message viewing.
	      END IF
	      CALL WRITE_USER_FILE(IER)
	      IF (IER.NE.0) THEN		! Error in writing to user file
		 WRITE (6,1070)			! Tell user of the error
		 CALL CLOSE_BULLUSER		! Close the user file
		 CALL EXIT			! Go away...
	      END IF
	      IF (DISMAIL.EQ.1) RETURN		! Go away if DISMAIL set
	      DIFF = -1				! Force us to look at messages
	      CALL OPEN_BULLINF_SHARED
	      DO I=1,FOLDER_MAX
	         CALL COPY2(LAST_READ_BTIM(1,I),READ_BTIM)
	      END DO
	      WRITE (9,IOSTAT=IER) USERNAME,
     &		((LAST_READ_BTIM(I,J),I=1,2),J=1,FOLDER_MAX)
	      CALL CLOSE_BULLINF
	   END IF
	   CALL COPY2(LOGIN_BTIM,LOGIN_BTIM_SAVE)
	   CALL READ_USER_FILE_HEADER(IER2)	! Reset read back to header
	END IF

	IF (IER.EQ.0.AND.MINUTE_DIFF(TODAY_BTIM,BBOARD_BTIM)
     &			.GT.BBOARD_UPDATE) THEN	! Update BBOARD mail?
	   CALL COPY2(BBOARD_BTIM,TODAY_BTIM)
	   REWRITE (4) USER_HEADER		! Rewrite header
	   IF (.NOT.TEST_BULLCP()) CALL CREATE_PROCESS('BBOARD')
	ELSE IF (IER.NE.0) THEN
	   CALL CLOSE_BULLUSER
	   CALL EXIT			! If no header, no messages
	END IF

	IF (IER1.EQ.0) THEN		! Skip date comparison if new entry
C
C  Compare and see if messages have been added since the last time
C  that the user has logged in or used the BULLETIN facility.
C
	   DIFF1 = COMPARE_BTIM(LOGIN_BTIM,READ_BTIM)
	   IF (DIFF1.LT.0) THEN		! If read messages since last login,
	      CALL COPY2(LOGIN_BTIM,READ_BTIM)
		! then use read date to compare with latest bulletin date
	   END IF			! to see if should alert user.

	   IF (SYSTEM_SWITCH) THEN
	      DIFF1 = COMPARE_BTIM(SYSTEM_LOGIN_BTIM,NEWEST_BTIM)
	   ELSE
	      DIFF1 = COMPARE_BTIM(LOGIN_BTIM,NEWEST_BTIM)
	   END IF
	END IF

	CALL COPY2(LOGIN_BTIM_SAVE,LOGIN_BTIM)  ! Destroyed in UPDATE_READ
	
	IF (NEW_FLAG(2).NE.0.AND.NEW_FLAG(2).NE.-1) THEN
	   CALL LIB$MOVC3(4,NEW_FLAG(2),%REF(BULL_PARAMETER))
	   CALL SUBTIME(LOGIN_BTIM,BULL_PARAMETER(:4),IER)
	ELSE IF (DIFF1.GT.0) THEN
	   BULL_POINT = -1
	   IF (READIT.EQ.1) THEN
	      CALL UPDATE_READ(1)
	      CALL COPY2(LOGIN_BTIM_NEW,LOGIN_BTIM)
	      CALL READ_IN_FOLDERS
	      CALL MODIFY_SYSTEM_LIST(1)
	   END IF
	   CALL CLOSE_BULLUSER
	   RETURN
	END IF

	CALL READ_IN_FOLDERS
	CALL MODIFY_SYSTEM_LIST(1)
        FOLDER_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//FOLDER

	ENTRY LOGIN_FOLDER

	IF (NEW_FLAG(2).EQ.0.OR.NEW_FLAG(2).EQ.-1.OR.FOLDER_SET) THEN
	   CALL COPY2(LOGIN_BTIM,LOGIN_BTIM_SAVE)
	END IF

	IF (REMOTE_SET.EQ.1) THEN	! If system remote folder, use remote
					! info, not local login time
	   IF (LAST_SYS_BTIM(1,FOLDER_NUMBER+1).NE.0) THEN
	    CALL COPY2(LOGIN_BTIM,LAST_SYS_BTIM(1,FOLDER_NUMBER+1))
	    LAST_SYS_BTIM(1,FOLDER_NUMBER+1) = 0
	    LAST_SYS_BTIM(2,FOLDER_NUMBER+1) = 0
	   ELSE
	    DIFF1 = COMPARE_BTIM(LOGIN_BTIM,
     &			LAST_READ_BTIM(1,FOLDER_NUMBER+1))
	    IF (DIFF1.LT.0) THEN
	      CALL COPY2(LOGIN_BTIM,LAST_READ_BTIM(1,FOLDER_NUMBER+1))
	    ELSE
	      DIFF = MINUTE_DIFF(LOGIN_BTIM,F_NEWEST_BTIM)
	      IF (DIFF.GE.0.AND.DIFF.LE.15) THEN  ! BULLCP updates every 15 min
	         IER = SYS$BINTIM('0 00:15',BULLCP_BTIM)
	         BULLCP_BTIM(1) = -BULLCP_BTIM(1) ! Convert to -delta time
	         BULLCP_BTIM(2) = -BULLCP_BTIM(2)-1
	         CALL LIB$SUBX(LOGIN_BTIM,BULLCP_BTIM,LOGIN_BTIM)
	      END IF
	    END IF
	   END IF
	END IF

	ENTRY SHOW_SYSTEM

	JUST_SYSTEM = (.NOT.LOGIN_SWITCH.AND.SYSTEM_SWITCH).OR.
     &	        (BTEST(FOLDER_FLAG,2)
     &		.AND..NOT.TEST_SET_FLAG(FOLDER_NUMBER)
     &		.AND..NOT.TEST_BRIEF_FLAG(FOLDER_NUMBER))

	NGEN = 0			! Number of general messages
	NSYS = 0			! Number of system messages
	BULL_POINT = -1

	IF (IER1.NE.0.AND.FOLDER_NUMBER.GT.0) THEN
	   IF (LOGIN_SWITCH) THEN
	      IF (READIT.EQ.1) THEN
	         CALL COPY2(LOGIN_BTIM_SAVE,LOGIN_BTIM)
	         CALL UPDATE_READ(1)
	         CALL COPY2(LOGIN_BTIM_NEW,LOGIN_BTIM)
	      END IF
	      CALL CLOSE_BULLUSER
	   END IF
	   RETURN	! Don't overwhelm new user with lots of non-general msgs
	END IF

	IF (BTEST(FOLDER_FLAG,2).AND.SYSTEM_SWITCH) THEN
			! Can folder have SYSTEM messages and /SYSTEM specified?
	   CALL COPY2(LOGIN_BTIM,SYSTEM_LOGIN_BTIM) ! Use specified login time
						    ! for system messages.
	END IF

	IF (LOGIN_SWITCH) THEN
	   IF (READIT.EQ.1) THEN
	      CALL COPY2(LOGIN_BTIM_OLD,LOGIN_BTIM)
	      CALL UPDATE_READ(1)
	      CALL COPY2(LOGIN_BTIM_NEW,LOGIN_BTIM)
	      CALL COPY2(LOGIN_BTIM,LOGIN_BTIM_OLD)
	   END IF
	   CALL CLOSE_BULLUSER
	END IF

	IF (READIT.EQ.1.AND.FOLDER_NUMBER.GE.0.AND.REMOTE_SET.LT.3) THEN
	   IF (LAST_SYS_BTIM(1,FOLDER_NUMBER+1).NE.0) THEN
	      DIFF1 = COMPARE_BTIM(LOGIN_BTIM,
     &				LAST_SYS_BTIM(1,FOLDER_NUMBER+1))
	      IF (DIFF1.LT.0) THEN
	         CALL COPY2(LOGIN_BTIM,LAST_SYS_BTIM(1,FOLDER_NUMBER+1))
	      END IF
	      CALL COPY2(LAST_SYS_BTIM(1,FOLDER_NUMBER+1),
     &			 LOGIN_BTIM_NEW)
	   END IF

	   IF (TEST2(BRIEF_FLAG,FOLDER_NUMBER)
     &		       .AND.TEST2(SET_FLAG,FOLDER_NUMBER)) THEN
	      IF (.NOT.TEST2(SYSTEM_FLAG,FOLDER_NUMBER)) GO TO 9999
	   END IF
	END IF

	CALL OPEN_BULLDIR_SHARED	! Get bulletin directory
	IF (.NOT.REMOTE_SET) THEN
	   CALL READDIR(0,IER)		! Get header info
	ELSE
	   NBULL = F_NBULL
	END IF
	   
	CALL INIT_QUEUE(GEN_DIR1,BULLDIR_ENTRY)
	CALL INIT_QUEUE(SYS_DIR1,BULLDIR_ENTRY)
	CALL INIT_QUEUE(SYS_NUM1,%DESCR(ICOUNT))
	GEN_DIR = GEN_DIR1
	SYS_DIR = SYS_DIR1
	SYS_NUM = SYS_NUM1
	START = 1
	REVERSE = 0
	IF ((.NOT.TEST_SET_FLAG(FOLDER_NUMBER).OR.
     &		.NOT.TEST_BRIEF_FLAG(FOLDER_NUMBER))
     &		.AND..NOT.BTEST(FOLDER_FLAG,7)) THEN
	   IF (REVERSE_SWITCH) REVERSE = 1
	   IF (IER1.EQ.0) THEN
	      CALL GET_NEWEST_MSG(LOGIN_BTIM,START)
	      IF (START.EQ.-1) START = NBULL + 1
	   END IF
	END IF

	IF (REMOTE_SET) THEN
	   CALL INIT_QUEUE(ALL_DIR1,BULLDIR_ENTRY)
	   ALL_DIR = ALL_DIR1
	   CALL REMOTE_DIRECTORY_COMMAND(START,NBULL,
     &					 .NOT.REVERSE,ALL_DIR,IER)
	   IF (IER.NE.0) THEN
	      CALL CLOSE_BULLDIR
	      CALL DISCONNECT_REMOTE
	      GO TO 9999
	   END IF
	   LAST_DIR = ALL_DIR
	   ALL_DIR = ALL_DIR1
	END IF

	CALL GET_NODE_NUMBER(NODE_NUMBER1,NODE_AREA1)

	DO ICOUNT1 = NBULL,START,-1
	   IF (REVERSE) THEN
	      ICOUNT = NBULL + START - ICOUNT1
	   ELSE
	      ICOUNT = ICOUNT1
	   END IF
	   IF (REMOTE_SET) THEN
	      IF (ALL_DIR.EQ.LAST_DIR) GO TO 100
	      CALL READ_QUEUE(%VAL(ALL_DIR),ALL_DIR,BULLDIR_ENTRY)
	      IER = ICOUNT + 1
	   ELSE
	      CALL READDIR(ICOUNT,IER)
	   END IF
	   IF (IER1.EQ.0.AND.IER.EQ.ICOUNT+1) THEN ! Is this a totally new user?
	      IF (.NOT.REVERSE.AND..NOT.BTEST(FOLDER_FLAG,7)) THEN 
	         DIFF = COMPARE_BTIM(LOGIN_BTIM,MSG_BTIM) ! No, so compare date
	         IF (DIFF.GT.0) GO TO 100
	      END IF
	      IGNORE = BTEST(SYSTEM,2).AND.(NODE_AREA.EQ.NODE_AREA1).AND.
     &			(NODE_NUMBER.AND.NODE_NUMBER1)
	      IF (.NOT.BTEST(FOLDER_FLAG,2)) SYSTEM = SYSTEM.AND.(.NOT.1)
			! Show system msg in non-system folder as general msg
	      IF ((USERNAME.NE.FROM.OR.SYSTEM).AND..NOT.IGNORE) THEN
				  	! Is bulletin system or from same user?
		 IF (SYSTEM) THEN	! Is it system bulletin? 
		    NSYS = NSYS + 1
		    CALL WRITE_QUEUE(%VAL(SYS_DIR),SYS_DIR,BULLDIR_ENTRY)
		    CALL WRITE_QUEUE(%VAL(SYS_NUM),SYS_NUM,%DESCR(ICOUNT))
	         ELSE IF (.NOT.JUST_SYSTEM) THEN
		    IF (BTEST(FOLDER_FLAG,7)) THEN
		       DIFF = COMPARE_BTIM
     &			      (LAST_READ_BTIM(1,FOLDER_NUMBER+1),MSG_BTIM)
		    ELSE IF (.NOT.SYSTEM_SWITCH) THEN
		       DIFF = -1
		    ELSE
	               DIFF = COMPARE_BTIM(LOGIN_BTIM_SAVE,MSG_BTIM)
		    END IF
		    IF (DIFF.LT.0) THEN
		       IF (.NOT.REVERSE.OR.BULL_POINT.EQ.-1) THEN
		          BULL_POINT = ICOUNT - 1
		          IF (.NOT.BTEST(FOLDER_FLAG,2).AND.
     &			   TEST_BRIEF_FLAG(FOLDER_NUMBER).AND.
     &			   TEST_SET_FLAG(FOLDER_NUMBER)) GO TO 100
		       END IF
		       NGEN = NGEN + 1
		       SYSTEM = ICOUNT
		       CALL WRITE_QUEUE(%VAL(GEN_DIR),GEN_DIR,BULLDIR_ENTRY)
		    END IF
	         END IF
	      END IF
	   ELSE IF (IER.EQ.ICOUNT+1) THEN
			! Totally new user, save only permanent system msgs
	      IF ((SYSTEM.AND.7).EQ.3.OR.
     &		  (SYSTEM.AND.BTEST(FOLDER_FLAG,7))) THEN
	         NSYS = NSYS + 1
		 CALL WRITE_QUEUE(%VAL(SYS_DIR),SYS_DIR,BULLDIR_ENTRY)
		 CALL WRITE_QUEUE(%VAL(SYS_NUM),SYS_NUM,%DESCR(ICOUNT))
	      ELSE IF (NGEN.EQ.0.OR.	! And save only the first non-system msg
     &		     BTEST(FOLDER_FLAG,7)) THEN ! and SET ALWAYS folder messages
		 SYSTEM = ICOUNT	! Save bulletin number for display
		 IF (.NOT.REVERSE.OR.BULL_POINT.EQ.-1) THEN
		    BULL_POINT = ICOUNT - 1
		    IF (.NOT.BTEST(FOLDER_FLAG,2).AND.
     &			TEST_BRIEF_FLAG(FOLDER_NUMBER).AND.
     &		 	TEST_SET_FLAG(FOLDER_NUMBER)) GO TO 100
		 END IF
		 NGEN = NGEN + 1
		 CALL WRITE_QUEUE(%VAL(GEN_DIR),GEN_DIR,BULLDIR_ENTRY)
	      END IF
	   END IF
	END DO
100	CALL CLOSE_BULLDIR
C
C  Review new directory entries.  If there are system messages,
C  copy the system bulletin into GEN_DIR file BULLSYS.SCR for outputting
C  to the terminal.  If there are simple messages, just output the
C  header information.
C
	IF (TEST_BRIEF_FLAG(FOLDER_NUMBER).AND.
     &			   TEST_SET_FLAG(FOLDER_NUMBER)) NGEN = 0

	IF (NGEN.EQ.0.AND.NSYS.EQ.0) GO TO 9999

	IF (NSYS.GT.0) THEN		! Are there any system messages?
	   IF (FIRST_WRITE) THEN
	      PAGE = 4		! Don't erase MAIL/PASSWORD notifies
	      FIRST_WRITE = .FALSE.	! if this is first write to screen.
	   END IF
	   LENF = TRIM(FOLDER_NAME)
	   S1 = (PAGE_WIDTH-(LENF+16))/2
	   S2 = PAGE_WIDTH - S1 - (LENF + 16)
	   WRITE (6,'(''+'',A,$)') CTRL_G
	   IF (REMOTE_SET.LT.3) THEN
	      FOLDER_NAME = FOLDER
	   ELSE
	      FOLDER_NAME = FOLDER_DESCRIP(:INDEX(FOLDER_DESCRIP,' ')-1)
	   END IF
	   WRITE (6,1026) FOLDER_NAME(:LENF)		! Yep...
	   PAGE = PAGE + 1
	   CTRL_G = 0		! Don't ring bell for non-system bulls
	   CALL OPEN_BULLFIL_SHARED
	   CALL INIT_QUEUE(SYS_BUL1,INPUT)
	   SYS_BUL = SYS_BUL1
	   SYS_DIR = SYS_DIR1
	   SYS_NUM = SYS_NUM1
	   NSYS_LINE = 0
	   DO J=1,NSYS
	      CALL READ_QUEUE(%VAL(SYS_DIR),SYS_DIR,BULLDIR_ENTRY)
	      IF (REMOTE_SET) THEN
	         CALL READ_QUEUE(%VAL(SYS_NUM),SYS_NUM,%DESCR(ICOUNT))
	         WRITE (REMOTE_UNIT,'(2A)',IOSTAT=IER) 5,ICOUNT
	         IF (IER.GT.0) THEN
	            CALL DISCONNECT_REMOTE
	         ELSE
	            CALL GET_REMOTE_MESSAGE(IER)
	         END IF
		 IF (IER.GT.0) THEN
		    CALL CLOSE_BULLFIL
		    GO TO 9999
		 END IF
	      END IF
 	      INPUT = ' '
	      CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
	      NSYS_LINE = NSYS_LINE + 1
	      ILEN = LINE_LENGTH + 1
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN
	         CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      END IF
	      IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THEN
	         CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      END IF
	      DO WHILE (ILEN.GT.0)	! Copy bulletin to SYS_BUL link list
		 CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
		 NSYS_LINE = NSYS_LINE + 1
		 CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      END DO
	      IF (ILEN.LT.0) THEN
		 CALL CLOSE_BULLFIL
		 GO TO 9999
	      END IF
	      IF (J.LT.NSYS.AND.SEPARATE.NE.' ') THEN
 	         INPUT = ' '
	         CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
		 DO I=1,PAGE_WIDTH
		    INPUT(I:I) = SEPARATE
		 END DO
		 CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
	         NSYS_LINE = NSYS_LINE + 2
	      END IF
	   END DO
	   CALL CLOSE_BULLFIL
	   SYS_BUL = SYS_BUL1
	   ILEN = 0
	   I = 1
	   WIDTH = PAGE_WIDTH
	   LEFT = .FALSE.
	   DO WHILE (I.LE.NSYS_LINE.OR.ILEN.GT.0)  ! Write out system messages
	      IF (ILEN.EQ.0) THEN
	         CALL READ_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)
		 ILEN = TRIM(INPUT)
		 I = I + 1
	      END IF
	      IF (SYS_BUL.NE.0) THEN
		 IF (PAGE.EQ.PAGE_LENGTH-2.AND.PAGING) THEN
							! If at end of screen
		    WRITE(6,1080)	! Ask for input to proceed to next page
		    CALL GET_INPUT_NOECHO_PROMPT(INREAD(:1),
     &			'HIT any key for next page....')
	            WRITE (6,'(1X)')
	            CALL LIB$ERASE_PAGE(1,1)		! Clear the screen
		    PAGE = 1
	            INREAD = '+'
		 ELSE IF (WIDTH.EQ.PAGE_WIDTH.OR.LEFT) THEN 
		    PAGE = PAGE + 1
	            INREAD = ' '
		 END IF
		 IF (LEFT) THEN
 		    WRITE(6,1050) INREAD(:1)//INPUT(:ILEN)
		    LEFT = .FALSE.
	            ILEN = 0
	            INREAD = '+'
		 ELSE IF (ILEN.LE.WIDTH) THEN
 		    WRITE(6,1060) INREAD(:1)//INPUT(:ILEN)
		    WIDTH = PAGE_WIDTH
		    ILEN = 0
		 ELSE
		    DO WHILE (WIDTH.GT.0.AND.INPUT(WIDTH:WIDTH).NE.' ')
	               WIDTH = WIDTH - 1
		    END DO
		    WRITE(6,1060) INREAD(:1)//INPUT(:WIDTH)
	            INPUT = INPUT(WIDTH+1:)
		    ILEN = ILEN - WIDTH
		    DO WHILE (INPUT(:1).EQ.' '.AND.ILEN.GT.0)
		       ILEN = ILEN - 1
		       INPUT = INPUT(2:)
	            END DO
                    IF (INPUT(ILEN:ILEN).EQ.' ') THEN
	               CONTINUE
		    ELSE IF (ALPHA(INPUT(ILEN:ILEN))) THEN
		       INPUT = INPUT(:ILEN)//' '
		       ILEN = ILEN + 1
		    ELSE
		       INPUT = INPUT(:ILEN)//'  '
		       ILEN = ILEN + 2
		    END IF
		    WIDTH = PAGE_WIDTH - ILEN
		    IF (WIDTH.GT.0) THEN
		       IF (ILEN.GT.0) LEFT = .TRUE.
		    ELSE
	               WIDTH = PAGE_WIDTH
		    END IF
		 END IF
	      END IF
	   END DO
	   IF (NGEN.EQ.0) THEN
	      WRITE (6,'(A)')		! Write delimiting blank line
	   END IF
	   PAGE = PAGE + 1
	END IF

	ENTRY REDISPLAY_DIRECTORY

	GEN_DIR = GEN_DIR1
	IF (NGEN.GT.0) THEN		! Are there new non-system messages?
	   LENF = TRIM(FOLDER_NAME)
	   S1 = (PAGE_WIDTH-13-LENF)/2
	   S2 = PAGE_WIDTH-S1-13-LENF
	   IF (PAGE+7+NGEN.GT.PAGE_LENGTH.AND.PAGE.GT.0) THEN
	      WRITE(6,1080)		! Ask for input to proceed to next page
	      CALL GET_INPUT_NOECHO_PROMPT(INREAD(:1),	! Get terminal input
     &			'HIT any key for next page....')
	      WRITE (6,'(1X)')
	      CALL LIB$ERASE_PAGE(1,1)	! Clear the screen
	      WRITE (6,'(''+'',A,$)') CTRL_G
	      WRITE(6,1028) 'New '//FOLDER_NAME(:LENF)//' messages'
	      PAGE = 1
	   ELSE
	      IF (FIRST_WRITE) THEN
		 PAGE = 4		  ! Don't erase MAIL/PASSWORD notifies
	         FIRST_WRITE = .FALSE. ! if this is first write to screen.
	      END IF
	      WRITE (6,'(''+'',A,$)') CTRL_G
	      WRITE(6,1027) 'New '//FOLDER_NAME(:LENF)//' messages'
	      PAGE = PAGE + 1
	   END IF
	   WRITE(6,1020)
	   WRITE(6,1025)
	   PAGE = PAGE + 2
	   I = 0
	   DO WHILE (I.LT.NGEN)
	      I = I + 1
	      CALL READ_QUEUE(%VAL(GEN_DIR),GEN_DIR,BULLDIR_ENTRY)
	      CALL CONVERT_ENTRY_FROMBIN_FOLDER
	      N = MAX(INT(LOG10(REAL(SYSTEM)))+1,3)
	      N1 = MAX(1,6-N)
	      IF (PAGE.EQ.PAGE_LENGTH-2.AND.PAGING) THEN ! If at end of screen
		 WRITE(6,1080)	! Ask for input to proceed to next page
		 CALL GET_INPUT_NOECHO_PROMPT(INREAD(:1),
     &		'HIT Q(Quit listing) or any other key for next page....')
	         CALL STR$UPCASE(INREAD(:1),INREAD(:1))
	         WRITE (6,'(1X)')
	         CALL LIB$ERASE_PAGE(1,1)		! Clear the screen
		 PAGE = 1
		 IF (INREAD(:1).EQ.'Q') THEN
		    I = NGEN		! Quit directory listing
		    WRITE(6,'(''+Quitting directory listing.'')')
		 ELSE
		    WRITE(6,1040) '+'//DESCRIP(:53),FROM,DATE(:6),SYSTEM
		 END IF
					! Bulletin number is stored in SYSTEM
	      ELSE
		 PAGE = PAGE + 1
		 WRITE(6,1040) ' '//DESCRIP(:53),FROM,DATE(:6),SYSTEM
	      END IF
	   END DO
	   IF ((.NOT.FOLDER_SET.AND.BTEST(SET_FLAG(1),0).AND.DIFF1.LE.0)
     &		.OR.(FOLDER_SET.AND.TEST_SET_FLAG(FOLDER_NUMBER))) THEN
	      PAGE = 0	! Don't reset page counter if READNEW not set,
	   END IF	! as no prompt to read is generated.
	END IF
C
C  Instruct users how to read displayed messages if READNEW not selected.
C
	IF (.NOT.TEST_BRIEF_FLAG(FOLDER_NUMBER).AND.
     &		TEST_SET_FLAG(FOLDER_NUMBER)) THEN
	   WRITE(6,1030)
	ELSE IF (NGEN.EQ.0) THEN
	   ILEN = 57 + INDEX(COMMAND_PROMPT,'>') - 1
	   S1 = (PAGE_WIDTH-ILEN)/2
	   S2 = PAGE_WIDTH - S1 - ILEN
	   WRITE(6,1035) 'The '//COMMAND_PROMPT(:ILEN-57)//
     &		'/SYSTEM command can be used to reread these messages.'
	   PAGE = PAGE + 1
	ELSE
	   FLEN = TRIM(FOLDER_NAME)
	   IF (FOLDER_NUMBER.EQ.0) FLEN = -1
	   ILEN = 30 + INDEX(COMMAND_PROMPT,'>') - 1 + FLEN
	   S1 = (PAGE_WIDTH-ILEN)/2
	   S2 = PAGE_WIDTH - S1 - ILEN
	   IF (FOLDER_NUMBER.EQ.0) THEN
	      WRITE(6,1035) 'Type ' //COMMAND_PROMPT(:ILEN-29)//
     &		' to read these messages.'
	   ELSE
	      WRITE(6,1035) 'Type '//COMMAND_PROMPT(:ILEN-30-FLEN)
     &		//' '//FOLDER_NAME(:FLEN)//
     &		' to read these messages.'
	   END IF
	   PAGE = PAGE + 1
	END IF

9999	IF (LOGIN_SWITCH) THEN
	   CALL COPY2(LOGIN_BTIM,LOGIN_BTIM_NEW)
	   CALL COPY2(LOGIN_BTIM_SAVE,LOGIN_BTIM_OLD)
	END IF
	RETURN

1020	FORMAT(' Description',43X,'From',9X,'Date',3X,'Number')
1025	FORMAT(' -----------',43X,'----',9X,'----',3X,'------')
1026	FORMAT(' ',<S1>('*'),A,' System Messages',<S2>('*'))
1027	FORMAT(/,' ',<S1>('*'),A,<S2>('*'))
1028	FORMAT('+',<S1>('*'),A,<S2>('*'))
1030	FORMAT(' ',<PAGE_WIDTH>('*'))
1035	FORMAT(' ',<S1>('*'),A,<S2>('*'))
1040	FORMAT(A<53>,2X,A12,1X,A6,<N1>X,I<N>)
1050	FORMAT(A,$)
1060	FORMAT(A)
1070	FORMAT(' ERROR: Cannot add new entry to user file.')
1080	FORMAT(' ',/)

	END


	

	SUBROUTINE GET_NODE_NUMBER_OTHER(NODE_NUMBER,NODE_AREA,NODE_NAME)

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($SYIDEF)'

	CHARACTER*(*) NODE_NAME

	CALL INIT_ITMLST	! Initialize item list
				! Now add items to list
	CALL ADD_2_ITMLST(4,SYI$_NODE_AREA,%LOC(NODE_AREA))
	CALL ADD_2_ITMLST(4,SYI$_NODE_NUMBER,%LOC(NODE_NUMBER))
	CALL END_ITMLST(GETSYI_ITMLST)	! Get address of itemlist

	IER = SYS$GETSYIW(,,NODE_NAME(:TRIM(NODE_NAME)),
     &			%VAL(GETSYI_ITMLST),,,)	! Get Info command.

	IF (.NOT.IER) THEN
	   WRITE (6,'('' ERROR: Specified node name not found.'')')
	   NODE_AREA = 0
	END IF

	RETURN
	END



	SUBROUTINE DIRECTORY_HEADER(OUTLINE,PRINTING,EXTRACTING,EXPIRATION,
     &		OUT)

	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /PAGE/ PAGE_LENGTH,PAGE_WIDTH,PAGING
	LOGICAL PAGING

	CHARACTER*(*) OUTLINE

	IF (REMOTE_SET.GE.3) THEN
	   WRITE (OUTLINE,'('' ['',I,''-'',I,'']'')')
     &						F_START,F_NBULL
	ELSE
	   WRITE (OUTLINE,'('' [1-'',I,'']'')') NBULL
	END IF
	DO WHILE (INDEX(OUTLINE,'- ').GT.0)
	   I = INDEX(OUTLINE,'- ')
	   OUTLINE(I+1:) = OUTLINE(I+2:)
	END DO
	DO WHILE (INDEX(OUTLINE,'[ ').GT.0)
	   I = INDEX(OUTLINE,'[ ')
	   OUTLINE(I+1:) = OUTLINE(I+2:)
	END DO
	DO WHILE (INDEX(OUTLINE,'  ').LT.TRIM(OUTLINE))
	   I = INDEX(OUTLINE,'  ')
	   OUTLINE(I:) = OUTLINE(I+1:)
	END DO
	OUTLINE = FOLDER_NAME(:TRIM(FOLDER_NAME))//OUTLINE
	BULL_PARAMETER = ' '
	IF (READ_TAG) THEN
	   IF (BTEST(READ_TAG,1)) THEN
	      BULL_PARAMETER = 'MARKED'
	   ELSE
	      BULL_PARAMETER = 'SEEN'
	   END IF
	   IF (BTEST(READ_TAG,3)) THEN
	      BULL_PARAMETER = 'UN'//BULL_PARAMETER
	   END IF
	END IF
	IF (PRINTING) THEN
	   BULL_PARAMETER = 'PRINTING '//BULL_PARAMETER
	ELSE IF (EXTRACTING) THEN
	   BULL_PARAMETER = 'EXTRACTING '//BULL_PARAMETER
	END IF

	IF (OUT.EQ.6) THEN 
           WRITE (OUT,'(''+'',A,<PAGE_WIDTH-TRIM(BULL_PARAMETER)-
     &		TRIM(OUTLINE)>X,A)')
     &		BULL_PARAMETER(:TRIM(BULL_PARAMETER)),
     &		OUTLINE(:TRIM(OUTLINE))
           IF (EXPIRATION) THEN
	      WRITE(OUT,1005) '    #'
	   ELSE
	      WRITE(OUT,1000) '    #'
	   END IF
	ELSE
           WRITE (OUT,'(A,<PAGE_WIDTH-TRIM(BULL_PARAMETER)-
     &		TRIM(OUTLINE)>X,A)')
     &		BULL_PARAMETER(:TRIM(BULL_PARAMETER)),
     &		OUTLINE(:TRIM(OUTLINE))
           IF (EXPIRATION) THEN
	      WRITE(OUT,1005) '   #'
	   ELSE
	      WRITE(OUT,1000) '   #'
	   END IF
	END IF

1000	FORMAT(A,1X,'Description',43X,'From',9X,'Date',/)
1005	FORMAT(A,1X,'Description',43X,'From',8X,'Expires',/)

	RETURN
	END



	SUBROUTINE GET_NEW_OR_SINCE(NEW,SINCE,IER,DATETIME)

	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	INTEGER TODAY(2)

	CHARACTER DATETIME*24

	IF (SINCE) THEN	 		! Was /SINCE specified?
   	   IF (DATETIME.EQ.'TODAY') THEN	! TODAY is the default.
	      	     IER = SYS$BINTIM('-- 00:00:00.00',TODAY)
	      CALL GET_MSGKEY(TODAY,MSG_KEY)
	   ELSE
	      CALL SYS_BINTIM(DATETIME,MSG_BTIM)
	      CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
	   END IF
	   CALL READDIR_KEYGE(IER)
	ELSE IF (NEW) THEN	! was /NEW specified?
	   IF (REMOTE_SET.LT.3) THEN
	      DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &		       F_NEWEST_BTIM)
	      IF (DIFF.GE.0) THEN
		 IER = 0
	         RETURN
	      ELSE
	         CALL GET_MSGKEY(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &				MSG_KEY)
	      END IF
	      CALL READDIR_KEYGE(IER)
	   ELSE
	      CALL NEWS_GET_NEWEST_MESSAGE(IER)
	   END IF
	END IF

	RETURN
	END
