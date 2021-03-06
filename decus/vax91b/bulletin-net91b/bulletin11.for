C
C  BULLETIN11.FOR, Version 8/25/91
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
	SUBROUTINE TAG(ADD_OR_DEL,TAG_TYPE)
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLUSER.INC'
 
	INCLUDE 'BULLDIR.INC'
 
	INCLUDE 'BULLFOLDER.INC'
 
	COMMON /TAGS/ BULL_TAG,READ_TAG,BULL_NEWS_TAG
	DATA BULL_TAG /.FALSE./,READ_TAG /.FALSE./,BULL_NEWS_TAG /.FALSE./
 
	COMMON /POINT/ BULL_POINT
 
	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER
 
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 
	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD
 
	CHARACTER*12 TAG_KEY
 
	EXTERNAL CLI$_ABSENT,CLI$_NEGATED
 
	IF ((.NOT.BULL_TAG.AND.REMOTE_SET.NE.3)
     &	    .OR.(.NOT.BULL_NEWS_TAG.AND.REMOTE_SET.EQ.3)) THEN
	   CALL OPEN_NEW_TAG(IER)
	   IF (.NOT.IER) RETURN
	END IF
 
	IF (REMOTE_SET.EQ.3) THEN
	   IF (NEWS_FIND_SUBSCRIBE().GT.FOLDER_MAX-1) THEN
	      WRITE (6,'('' ERROR: NEWS group is not subscribed.'')')
	      RETURN
	   END IF
	END IF
 
	IF (ADD_OR_DEL.AND.
     &		INCMD(:4).NE.'MARK'.AND.INCMD(:4).NE.'SEEN') THEN
	   CALL ADD_TAG(IER,TAG_TYPE)
	   RETURN
	END IF
 
	IF (INCMD(:4).EQ.'SEEN') THEN
	   IF (CLI$PRESENT('READ').EQ.%LOC(CLI$_NEGATED)) THEN
	      READ (13,KEYEQ=TAG_KEY(0,BULLDIR_HEADER,1),
     &		    IOSTAT=IER)
	      IF (IER.EQ.0) DELETE (UNIT=13)
	      BULL_TAG = IBCLR(BULL_TAG,1)
	      RETURN
	   END IF
	END IF
 
	IF (.NOT.CLI$PRESENT('NUMBER')) THEN
	   IF (BULL_POINT.EQ.0) THEN	! No.  Have we just read a bulletin?
	      WRITE(6,1010)		! No, then error.
	      RETURN
	   ELSE IF (ADD_OR_DEL) THEN
	      CALL ADD_TAG(IER,TAG_TYPE)
	   ELSE
	      CALL DEL_TAG(IER,TAG_TYPE)
	      IF (IER.NE.0) THEN
		 IF (TAG_TYPE.EQ.1) THEN
		    WRITE (6,'('' ERROR: Message was not marked.'')')
		 ELSE
		    WRITE (6,'('' ERROR: Message was not seen.'')')
		 END IF
	      END IF
	   END IF
	   RETURN
	END IF
 
	CALL OPEN_BULLDIR_SHARED
 
	IER1 = 0
	DO WHILE (CLI$GET_VALUE('NUMBER',BULL_PARAMETER,LEN_P)
     &	    .NE.%LOC(CLI$_ABSENT).AND.IER1.EQ.0) ! Get the specified messages
 
	   CALL GET_2_VALS(BULL_PARAMETER,LEN_P,SBULL,EBULL,IER)
 
	   IF (SBULL.LE.0.OR.IER.NE.0.OR.SBULL.GT.F_NBULL) THEN
	      WRITE (6,'(A)') 
     &	        ' ERROR: Specified message number has incorrect format.'
	      GO TO 100
	   END IF
 
	   DO MESSAGE_NUMBER = SBULL,MIN(EBULL,F_NBULL)
 
	      CALL READDIR(MESSAGE_NUMBER,IER)
	      IF (IER.NE.MESSAGE_NUMBER+1	! Was message found?
     &	          .AND.REMOTE_SET.NE.3) THEN	! Ignore if news
	         WRITE(6,1030) MESSAGE_NUMBER	! No
		 GO TO 100
	      ELSE IF (ADD_OR_DEL) THEN
	         CALL ADD_TAG(IER,TAG_TYPE)
	      ELSE
	         CALL DEL_TAG(IER,TAG_TYPE)
	      END IF
	   END DO
	END DO
 
100	IF (REMOTE_SET.EQ.3) CALL READDIR(BULL_POINT,IER)
 
	CALL CLOSE_BULLDIR
 
	RETURN
 
1010	FORMAT(' ERROR: You have not read any message.')
1030	FORMAT(' ERROR: Message was not found: ',I)
 
	END
 
 
 
	SUBROUTINE ADD_TAG(IER,TAG_TYPE)
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE '($FORIOSDEF)'
 
	INCLUDE 'BULLDIR.INC'
 
	INCLUDE 'BULLFOLDER.INC'
 
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 
	COMMON /TAGS/ BULL_TAG,READ_TAG,BULL_NEWS_TAG
 
	CHARACTER*12 TAG_KEY
 
	IF (REMOTE_SET.NE.3) THEN
	   IF (TAG_TYPE.EQ.2.AND..NOT.BTEST(BULL_TAG,1)) THEN ! No SEEN tags
	      WRITE (13,IOSTAT=IER) TAG_KEY(0,BULLDIR_HEADER,1)
	      BULL_TAG = IBSET(BULL_TAG,1)
	   END IF
	   WRITE (13,IOSTAT=IER) TAG_KEY(FOLDER_NUMBER,MSG_KEY,TAG_TYPE)
	ELSE
	   CALL ADD_NEWS_TAG(IER,TAG_TYPE)
	   RETURN
	END IF
 
	IF (IER.NE.FOR$IOS_INCKEYCHG.AND.IER.NE.0) THEN
	   WRITE (6,'('' ERROR: Unable to mark message.'')')
	   CALL ERRSNS(IDUMMY,IER1)
	   IF (IER1.EQ.0) THEN
	      WRITE (6,'('' IOSTAT error = '',I)') IER
	   ELSE
	      CALL SYS_GETMSG(IER1)
	   END IF
	ELSE
	   IER = 0
	END IF
 
	RETURN
	END
 
 
 
 
	SUBROUTINE GET_FIRST_NEWS_TAG(IER,MESSAGE)
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLUSER.INC'
 
	INCLUDE 'BULLDIR.INC'
 
	INCLUDE 'BULLFOLDER.INC'
 
	COMMON /NEWS_TAGS/ NEWS_TAG(4,2,FOLDER_MAX-1)
 
	COMMON /NEWS_MARK/ NEWS_MARK
	DIMENSION NEWS_MARK(128)
	INTEGER*2 NEWS_MARK2(256),NEWS_NUMBER,NEWS_REC
	EQUIVALENCE (NEWS_MARK(1),NEWS_MARK2(1))
	EQUIVALENCE (NEWS_MARK2(1),NEWS_NUMBER)
	EQUIVALENCE (NEWS_MARK2(2),NEWS_REC)
	EQUIVALENCE (NEWS_MARK(2),NEWS_FORMAT)
 
	COMMON /TAGS/ BULL_TAG,READ_TAG,BULL_NEWS_TAG
 
        COMMON /NEXT/ NEXT
 
	IER = 36
 
	SUBNUM = NEWS_FIND_SUBSCRIBE()
 
	IF (SUBNUM.GT.FOLDER_MAX-1) RETURN
 
	DO J=1,2
	   IF (BTEST(READ_TAG,J)) I = J
	END DO
 
	IF (NEWS_TAG(3,I,SUBNUM).EQ.0) RETURN
 
	OLD_NEXT = NEXT
 
	NEXT = .FALSE.
	J = F_START - 1
	IER1 = J
	DO WHILE (J.LE.F_NBULL.AND.J+1.NE.IER1)
	   J = J + 1
	   CALL READDIR(J,IER1)
	END DO
 
	IF (J+1.NE.IER1) THEN
	   NEXT = OLD_NEXT
	   RETURN
	END IF
 
	NEXT = .TRUE.
 
	DO MESSNUM = NEWS_TAG(1,I,SUBNUM),NEWS_TAG(2,I,SUBNUM)
	   TEST = TEST_TAG(MESSNUM,%VAL(NEWS_TAG(3,I,SUBNUM)),
     &			NEWS_TAG(1,I,SUBNUM))
	   IF (BTEST(READ_TAG,3)) TEST = .NOT.TEST
	   IF (TEST) THEN
	      HEADER = .TRUE.
	      CALL GET_NEXT_NEWS_TAG(IER,MESSNUM,HEADER,I,SUBNUM)
	      IF (IER.EQ.0) MESSAGE = MESSNUM
	      NEXT = OLD_NEXT
	      RETURN
	   END IF
	END DO
 
	NEXT = OLD_NEXT
 
	RETURN
 
	ENTRY GET_THIS_NEWS_TAG(IER,MESSAGE,TAG_TYPE)
 
	IER = 36
 
	SUBNUM = NEWS_FIND_SUBSCRIBE()
 
	IF (SUBNUM.GT.FOLDER_MAX-1) RETURN
 
	TAG_TYPE = 0
 
	DO I=1,2
	   IF ((BTEST(READ_TAG,I).OR.BTEST(READ_TAG,3))
     &	    .AND.(NEWS_TAG(3,I,SUBNUM).GT.0).AND.
     &	    (MSG_NUM.LE.NEWS_TAG(2,I,SUBNUM))) THEN
	      TEST = TEST_TAG(MSG_NUM,
     &		%VAL(NEWS_TAG(3,I,SUBNUM)),NEWS_TAG(1,I,SUBNUM))
	      IF (TEST) THEN
	         IER = 0
	         TAG_TYPE = IBSET(TAG_TYPE,I)
	      END IF
	   END IF
	END DO
 
	IF (BTEST(READ_TAG,3)) THEN
	   IF ((.NOT.BTEST(TAG_TYPE,2).OR..NOT.BTEST(READ_TAG,2)).AND.
     &	       (.NOT.BTEST(TAG_TYPE,1).OR..NOT.BTEST(READ_TAG,1))) THEN
	      IER = 0
	   ELSE
	      IER = 36
	   END IF
	END IF
 
	RETURN
 
	ENTRY GET_THIS_OR_NEXT_NEWS_TAG(NUM,IER,MESSAGE,TAG_TYPE)
 
	IER = 36
 
	SUBNUM = NEWS_FIND_SUBSCRIBE()
 
	IF (SUBNUM.GT.FOLDER_MAX-1) RETURN
 
	HEADER = .FALSE.
 
	TAG_TYPE = 0
 
	DO WHILE (IER.NE.0)
	   I = 0
	   DO J=1,2
	      IF (NEWS_TAG(3,J,SUBNUM).GT.0.AND.BTEST(READ_TAG,J)) THEN
		 IER = 36
		 MNUM = MAX(NEWS_TAG(1,J,SUBNUM),NUM)
	   	 DO WHILE (IER.NE.0.AND.MNUM.LE.NEWS_TAG(2,J,SUBNUM))
	     	    TEST = TEST_TAG(MNUM,%VAL(NEWS_TAG(3,J,SUBNUM)),
     &		  	NEWS_TAG(1,J,SUBNUM))
		    IF (BTEST(READ_TAG,3)) TEST = .NOT.TEST
		    IF (TEST) THEN
		       IER = 0
		    ELSE
		       MNUM = MNUM + 1
		    END IF
		 END DO
		 IF (IER.EQ.0) THEN
		    IF (J.EQ.1) THEN
		       MESSAGE = MNUM
		       I = 1
		    ELSE IF (I.EQ.0.OR.MESSAGE.GT.MNUM) THEN
		       MESSAGE = MNUM
		       I = 2
		    END IF
		 END IF
	      END IF
	   END DO
	   IF (I.EQ.0) RETURN
	   CALL GET_NEXT_NEWS_TAG(IER,MESSAGE,HEADER,I,SUBNUM)
	   IF (IER.EQ.0) THEN
	      IF (.NOT.BTEST(READ_TAG,3)) TAG_TYPE = IBSET(TAG_TYPE,I)
	      IF (NEWS_TAG(3,3-I,SUBNUM).GT.0.AND.
     &		  MESSAGE.LE.NEWS_TAG(2,3-I,SUBNUM).AND.
     &		  TEST_TAG(MESSAGE,%VAL(NEWS_TAG(3,3-I,SUBNUM)),
     &		  	NEWS_TAG(1,3-I,SUBNUM))) THEN
		 TAG_TYPE = IBSET(TAG_TYPE,3-I)
	      END IF
	      RETURN
	   ELSE IF (.NOT.BTEST(READ_TAG,3-I)) THEN
	      RETURN
	   END IF
	END DO
 
	RETURN
	END
 
 
 
 
	SUBROUTINE GET_NEXT_NEWS_TAG(IER,MESSNUM,HEADER,J,SUBNUM)
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLUSER.INC'
 
	INCLUDE 'BULLDIR.INC'
 
	COMMON /NEWS_TAGS/ NEWS_TAG(4,2,FOLDER_MAX-1)
 
	COMMON /TAGS/ BULL_TAG,READ_TAG,BULL_NEWS_TAG
 
        COMMON /NEXT/ NEXT
 
	IER = 36
 
	OLD_NEXT = NEXT
 
	DO WHILE (MESSNUM.LE.NEWS_TAG(2,J,SUBNUM).AND.IER.NE.0)
	   I = MAX(NEWS_TAG(1,J,SUBNUM),MESSNUM)
	   DO WHILE (IER.NE.0.AND.I.LE.NEWS_TAG(2,J,SUBNUM))
	      TEST = TEST_TAG(I,%VAL(NEWS_TAG(3,J,SUBNUM)),
     &		  NEWS_TAG(1,J,SUBNUM))
	      IF (BTEST(READ_TAG,3)) TEST = .NOT.TEST
	      IF (TEST) THEN
	         IER = 0
	         MESSNUM = I
	      ELSE
		 I = I + 1
	      END IF
	   END DO
	   IF (IER.EQ.0) THEN
	      SAVE_MESSNUM = MESSNUM
	      NEXT = .FALSE.
	      CALL READDIR(MESSNUM,IER1)
	      IF (IER1.NE.MESSNUM+1) THEN
	         NEXT = .TRUE.
	         CALL READDIR(MESSNUM,IER1)
	      END IF
	      IF (IER1.NE.MESSNUM+1) THEN
		 IER = 36
	         IF (.NOT.BTEST(READ_TAG,3)) THEN
		    CALL DEL_NEWS_TAG(J,MESSNUM,SUBNUM)
		 ELSE
		    NEXT = OLD_NEXT
		    RETURN
		 END IF
		 IF (BTEST(READ_TAG,1).AND.BTEST(READ_TAG,2)) RETURN
	      ELSE IF (MESSNUM.NE.SAVE_MESSNUM) THEN
		 IER = 36
	         IF (.NOT.BTEST(READ_TAG,3)) THEN
		    CALL DEL_NEWS_TAG(J,SAVE_MESSNUM,SUBNUM)
		 END IF
	      END IF
	   ELSE
	      MESSNUM = NEWS_TAG(2,J,SUBNUM) + 1
	   END IF
	END DO
 
	IF (IER.EQ.0.AND.HEADER) THEN
	   MESSNUM = MESSNUM - 1
	   MSG_NUM = MESSNUM
	END IF
 
	NEXT = OLD_NEXT
 
	RETURN
	END
 
 
 
 
	SUBROUTINE ADD_NEWS_TAG(IER,TAG_TYPE)
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLDIR.INC'
 
	INCLUDE 'BULLUSER.INC'
 
	INCLUDE 'BULLFOLDER.INC'
 
	COMMON /NEWS_TAGS/ NEWS_TAG(4,2,FOLDER_MAX-1)
 
	IER = 0
 
	SUBNUM = NEWS_FIND_SUBSCRIBE()
	IF (SUBNUM.GT.FOLDER_MAX-1) RETURN
 
	IF (NEWS_TAG(3,TAG_TYPE,SUBNUM).EQ.0.AND.F_NBULL.GE.F_START) THEN
	   NEWS_TAG(1,TAG_TYPE,SUBNUM) = F_START
	   NEWS_TAG(2,TAG_TYPE,SUBNUM) = F_NBULL
	   CALL LIB$GET_VM((F_NBULL-F_START)/8+1,
     &			   NEWS_TAG(3,TAG_TYPE,SUBNUM))
	   CALL ZERO_VM((F_NBULL-F_START)/8+1,
     &			%VAL(NEWS_TAG(3,TAG_TYPE,SUBNUM)))
	ELSE IF (F_NBULL.GT.NEWS_TAG(2,TAG_TYPE,SUBNUM)) THEN
	   DO I=1,2
	      IF (NEWS_TAG(1,I,SUBNUM).GT.0) THEN
	         CALL LIB$GET_VM((F_NBULL-NEWS_TAG(1,I,SUBNUM))/8+1,TEMP)
	         CALL ZERO_VM((F_NBULL-NEWS_TAG(1,I,SUBNUM))/8+1,
     &			%VAL(TEMP))
	         CALL LIB$MOVC3((NEWS_TAG(2,I,SUBNUM)-
     &			  NEWS_TAG(1,I,SUBNUM))/8+1,
     &			  %VAL(NEWS_TAG(3,I,SUBNUM)),%VAL(TEMP))
	         CALL LIB$FREE_VM((NEWS_TAG(2,I,SUBNUM)-
     &			  NEWS_TAG(1,I,SUBNUM))/8+1,
     &			  NEWS_TAG(3,I,SUBNUM))
	         NEWS_TAG(2,I,SUBNUM) = F_NBULL
	         NEWS_TAG(3,I,SUBNUM) = TEMP
	      END IF
	   END DO
	END IF
 
	CALL SET_TAG(MSG_NUM,%VAL(NEWS_TAG(3,TAG_TYPE,SUBNUM)),
     &		     NEWS_TAG(1,TAG_TYPE,SUBNUM))
	NEWS_TAG(4,TAG_TYPE,SUBNUM) = 1
		 
	RETURN
	END
 
 
 
	SUBROUTINE SET_TAG(NUM,TAGS,START)
 
	IMPLICIT INTEGER (A-Z)
 
	DIMENSION TAGS(1)
 
	I = (NUM-START)/32
	J = NUM - START - I*32
 
	TAGS(I+1) = IBSET(TAGS(I+1),J)
 
	RETURN
	END
 
 
 
	SUBROUTINE CLR_TAG(NUM,TAGS,START)
 
	IMPLICIT INTEGER (A-Z)
 
	DIMENSION TAGS(1)
 
	I = (NUM-START)/32
	J = NUM - START - I*32
 
	TAGS(I+1) = IBCLR(TAGS(I+1),J)
 
	RETURN
	END
 
 
 
	LOGICAL FUNCTION TEST_TAG(NUM,TAGS,START)
 
	IMPLICIT INTEGER (A-Z)
 
	DIMENSION TAGS(1)
 
	I = (NUM-START)/32
	J = NUM - START - I*32
 
	TEST_TAG = BTEST(TAGS(I+1),J)
 
	RETURN
	END
 
 
 
	SUBROUTINE DEL_TAG(IER,TAG_TYPE)
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLDIR.INC'
 
	INCLUDE 'BULLFOLDER.INC'
 
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 
	CHARACTER*12 TAG_KEY
 
	IER = 0
 
	IF (REMOTE_SET.EQ.3) THEN
	   SUBNUM = NEWS_FIND_SUBSCRIBE()
	   CALL DEL_NEWS_TAG(TAG_TYPE,MSG_NUM,SUBNUM)
	   RETURN
	END IF
 
	DO WHILE (REC_LOCK(IER1))
	   READ (13,KEYEQ=TAG_KEY(FOLDER_NUMBER,MSG_KEY,TAG_TYPE),
     &		 IOSTAT=IER1)
	END DO
	IF (IER1.NE.0) RETURN
 
	DELETE (UNIT=13,IOSTAT=IER1)
 
	RETURN
	END
 
 
 
	SUBROUTINE DEL_NEWS_TAG(TAG_TYPE,MSG_NUM,SUBNUM)
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLUSER.INC'
 
	COMMON /NEWS_TAGS/ NEWS_TAG(4,2,FOLDER_MAX-1)
 
	IF (MSG_NUM.LT.NEWS_TAG(1,TAG_TYPE,SUBNUM).OR.
     &	   MSG_NUM.GT.NEWS_TAG(2,TAG_TYPE,SUBNUM).OR..NOT.TEST_TAG
     &	    (MSG_NUM,%VAL(NEWS_TAG(3,TAG_TYPE,SUBNUM))
     &	    ,NEWS_TAG(1,TAG_TYPE,SUBNUM))) THEN
	   RETURN
	ELSE
	   NEWS_TAG(4,TAG_TYPE,SUBNUM) = 1
	   CALL CLR_TAG
     &		(MSG_NUM,%VAL(NEWS_TAG(3,TAG_TYPE,SUBNUM)),
     &		NEWS_TAG(1,TAG_TYPE,SUBNUM))
	END IF
 
	RETURN
	END
 
 
 
	SUBROUTINE OPEN_OLD_TAG
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE '($FORIOSDEF)'
 
	INCLUDE 'BULLUSER.INC'
 
	INCLUDE 'BULLDIR.INC'
 
	INCLUDE 'BULLFOLDER.INC'
 
	COMMON /NEWS_TAGS/ NEWS_TAG(4,2,FOLDER_MAX-1)
 
	COMMON /TAGS/ BULL_TAG,READ_TAG,BULL_NEWS_TAG
 
	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER
 
	COMMON /NEWS_MARK/ NEWS_MARK
	DIMENSION NEWS_MARK(128)
	INTEGER*2 NEWS_MARK2(256),NEWS_NUMBER,NEWS_REC
	EQUIVALENCE (NEWS_MARK(1),NEWS_MARK2(1))
	EQUIVALENCE (NEWS_MARK2(1),NEWS_NUMBER)
	EQUIVALENCE (NEWS_MARK2(2),NEWS_REC)
	EQUIVALENCE (NEWS_MARK(2),NEWS_FORMAT)
 
	CHARACTER*10 BULL_MARK_DIR
 
	CHARACTER*12 TAG_KEY
 
	IER = SYS_TRNLNM('BULL_MARK',BULL_PARAMETER)
	IF (IER) THEN
	   BULL_MARK_DIR = 'BULL_MARK:'
	ELSE
	   BULL_MARK_DIR = 'SYS$LOGIN:'
	END IF
 
	NTRIES = 0
 
	DO WHILE (FILE_LOCK(IER,IER1).AND.NTRIES.LE.30)
	   OPEN (UNIT=13,FILE=BULL_MARK_DIR//
     &	     USERNAME(:TRIM(USERNAME))//'.BULLMARK',STATUS='OLD',
     &	     ACCESS='KEYED',RECORDTYPE='FIXED',SHARED,
     &	     ORGANIZATION='INDEXED',IOSTAT=IER,
     &	     KEY=(1:12:CHARACTER))
	   NTRIES = NTRIES + 1
	END DO
 
	IF (IER.EQ.0) THEN
	   BULL_TAG = IBSET(BULL_TAG,0)
	   DO WHILE (REC_LOCK(IER1))
	      READ (13,KEY=TAG_KEY(0,BULLDIR_HEADER,1),IOSTAT=IER1)
	   END DO
	   IF (IER1.EQ.0) BULL_TAG = IBSET(BULL_TAG,1)
	END IF
 
	NTRIES = 0
 
	IF (IER.EQ.0.OR.IER.EQ.FOR$IOS_FILNOTFOU) THEN
	   DO WHILE (FILE_LOCK(IER,IER1).AND.NTRIES.LE.30)
	     OPEN (UNIT=23,FILE=BULL_MARK_DIR//
     &	        USERNAME(:TRIM(USERNAME))//'.NEWSMARK',STATUS='OLD',
     &	        ACCESS='KEYED',RECORDTYPE='FIXED',SHARED,
     &	        FORM='UNFORMATTED',ORGANIZATION='INDEXED',IOSTAT=IER,
     &	        KEY=(1:4:INTEGER))
	      NTRIES = NTRIES + 1
	   END DO
 
	   IF (IER.EQ.0) THEN
	      IF (BULL_NEWS_TAG) RETURN
	      BULL_NEWS_TAG = .TRUE.
	   END IF
	END IF
 
	IF (IER.NE.0.AND.IER.NE.FOR$IOS_FILNOTFOU) THEN
	   WRITE (6,'('' Unable to open mark file.'')')
	   IF (IER1.EQ.0) CALL ERRSNS(IDUMMY,IER1)
	   IF (IER1.EQ.0) THEN
	      WRITE (6,'('' IOSTAT error = '',I)') IER
	   ELSE
	      CALL SYS_GETMSG(IER1)
	   END IF
	   RETURN
	END IF
 
	IF (BULL_NEWS_TAG) THEN
	   OLD_NEWS_NUMBER = 0
	   FOLDER_NUMBER_SAVE = NEWS_FOLDER_NUMBER
	   CALL OPEN_BULLNEWS_SHARED
	   DO WHILE (IER.EQ.0)
	      DO WHILE (REC_LOCK(IER))
		 READ (23,IOSTAT=IER) NEWS_MARK
	      END DO
	      IF (IER.EQ.0) THEN
		 IF (NEWS_NUMBER.NE.OLD_NEWS_NUMBER) THEN
		    NEWS_FOLDER_NUMBER = NEWS_NUMBER
		    SUBNUM = NEWS_FIND_SUBSCRIBE()
		    IF (SUBNUM.GT.FOLDER_MAX-1) THEN
		       DELETE (UNIT=23)
		    ELSE
		       OLD_NEWS_NUMBER = NEWS_NUMBER
		       CALL READ_FOLDER_FILE_KEYNUM_TEMP
     &			  (NEWS_FOLDER_NUMBER,IER1)
		       IF (IER1.NE.0) THEN
			  SUBNUM = 0
		       ELSE
			  DO I=1,2
		             NEWS_TAG(1,I,SUBNUM) = F1_START
		             NEWS_TAG(2,I,SUBNUM) = F1_NBULL
		             NEWS_TAG(4,I,SUBNUM) = 0
		             CALL LIB$GET_VM((F1_NBULL-F1_START)/8+1,
     &					  NEWS_TAG(3,I,SUBNUM))
		             CALL ZERO_VM((F1_NBULL-F1_START)/8+1,
     &					%VAL(NEWS_TAG(3,I,SUBNUM)))
			  END DO
		       END IF
		    END IF
		 END IF
		 IF (NEWS_NUMBER.EQ.OLD_NEWS_NUMBER) THEN
	            IF (SUBNUM.EQ.0) THEN
		       DELETE (UNIT=23)
		    ELSE
		       IF (NEWS_REC.GT.0) THEN
			  TAG_TYPE = 1
		       ELSE
			  TAG_TYPE = 2
		       END IF
		       IF (NEWS_FORMAT.EQ.0) THEN	! 16 bit numbers
		          DO I=5,256
		             CALL SET_NEWS(INT(NEWS_MARK2(I)),SUBNUM,
     &					   TAG_TYPE)
		          END DO
		       ELSE
		          DO I=3,128
		             CALL SET_NEWS(NEWS_MARK(I),SUBNUM,TAG_TYPE)
		          END DO
		       END IF
		    END IF
		 END IF
	      END IF
	   END DO
	   NEWS_FOLDER_NUMBER = FOLDER_NUMBER_SAVE
	   CALL CLOSE_BULLNEWS
	END IF
 
	RETURN
	END
 
 
 
	SUBROUTINE SET_NEWS(NUM,SUBNUM,TAG_TYPE)
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLUSER.INC'
 
	COMMON /NEWS_TAGS/ NEWS_TAG(4,2,FOLDER_MAX-1)
 
	IF (NUM.GT.0) THEN
	   LAST_NUM = NUM
	   IF (NUM.LT.NEWS_TAG(1,TAG_TYPE,SUBNUM).OR.
     &	       NUM.GT.NEWS_TAG(2,TAG_TYPE,SUBNUM)) RETURN
	   CALL SET_TAG(NUM,%VAL(NEWS_TAG(3,TAG_TYPE,SUBNUM)),
     &		NEWS_TAG(1,TAG_TYPE,SUBNUM))
	ELSE IF (NUM.LT.0) THEN
	   IF (-NUM.LT.NEWS_TAG(1,TAG_TYPE,SUBNUM)) RETURN
	   DO J=MAX(NEWS_TAG(1,TAG_TYPE,SUBNUM),LAST_NUM+1),
     &		MIN(NEWS_TAG(2,TAG_TYPE,SUBNUM),-NUM)
	      CALL SET_TAG(J,%VAL(NEWS_TAG(3,TAG_TYPE,SUBNUM)),
     &		NEWS_TAG(1,TAG_TYPE,SUBNUM))
	   END DO
	END IF
 
	RETURN
	END
 
 
 
	SUBROUTINE OPEN_NEW_TAG(IER)
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLUSER.INC'
 
	INCLUDE 'BULLFOLDER.INC'
 
	COMMON /TAGS/ BULL_TAG,READ_TAG,BULL_NEWS_TAG
 
	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER
 
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 
	CHARACTER*10 BULL_MARK_DIR
 
	IER = SYS_TRNLNM('BULL_MARK',BULL_PARAMETER)
	IF (IER) THEN
	   BULL_MARK_DIR = 'BULL_MARK:'
	ELSE
	   BULL_MARK_DIR = 'SYS$LOGIN:'
	END IF
 
	IER1 = SYS_TRNLNM_SYSTEM('BULL_MARK',BULL_PARAMETER)
	IF (.NOT.IER1) THEN
	   IER = SYS_TRNLNM('BULL_MARK',BULL_PARAMETER)
	   CALL DISABLE_PRIVS
	   IER1 = .FALSE.
	END IF
	IF (REMOTE_SET.NE.3) THEN
	   MARKUNIT = 13
	   OPEN (UNIT=MARKUNIT,FILE=BULL_MARK_DIR//
     &	        USERNAME(:TRIM(USERNAME))//'.BULLMARK',STATUS='NEW',
     &	        ACCESS='KEYED',RECORDTYPE='FIXED',SHARED,
     &	        RECORDSIZE=3,
     &	        FORM='UNFORMATTED',ORGANIZATION='INDEXED',IOSTAT=IER,
     &	        KEY=(1:12:CHARACTER))
	ELSE
	   MARKUNIT = 23
	   OPEN (UNIT=MARKUNIT,FILE=BULL_MARK_DIR//
     &	        USERNAME(:TRIM(USERNAME))//'.NEWSMARK',STATUS='NEW',
     &	        ACCESS='KEYED',RECORDTYPE='FIXED',SHARED,
     &	        RECORDSIZE=128,
     &	        FORM='UNFORMATTED',ORGANIZATION='INDEXED',IOSTAT=IER,
     &	        KEY=(1:4:INTEGER))
	END IF
	IF (.NOT.IER1) CALL ENABLE_PRIVS
	IF (IER.NE.0) THEN
	   WRITE (6,'('' Cannot create mark file.'')')
	   CALL ERRSNS(IDUMMY,IER1)
	   IF (IER1.EQ.0) THEN
	      WRITE (6,'('' IOSTAT error = '',I)') IER
	      IER = 0
	   ELSE
	      CALL SYS_GETMSG(IER1)
	      IER = IER1
	   END IF
	ELSE
	   IF (.NOT.IER1) THEN
	      INQUIRE (UNIT=MARKUNIT,NAME=BULL_PARAMETER)
	      WRITE (6,'('' Created MARK file: '',A)')
     &		BULL_PARAMETER(:TRIM(BULL_PARAMETER))
	   END IF
	   IF (MARKUNIT.EQ.13) BULL_TAG = 1
	   IF (MARKUNIT.EQ.23) BULL_NEWS_TAG = .TRUE.
	   IER = 1
	END IF
 
	RETURN
	END
 
 
 
	CHARACTER*12 FUNCTION TAG_KEY(FOLDER_NUMBER,MSG_KEY,TAG_TYPE)
 
	IMPLICIT INTEGER (A-Z)
 
	CHARACTER*(*) MSG_KEY
 
	IF (TAG_TYPE.EQ.1) THEN
	   CALL LIB$MOVC3(4,FOLDER_NUMBER,%REF(TAG_KEY))
	ELSE
	   CALL LIB$MOVC3(4,-(1+FOLDER_NUMBER),%REF(TAG_KEY))
	END IF
 
	CALL GET_MSGKEY(%REF(MSG_KEY),TAG_KEY(5:))
 
	RETURN
	END
 
 
 
 
	SUBROUTINE GET_FIRST_TAG(FOLDER_NUMBER,IER,MESSAGE)
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLDIR.INC'
 
	COMMON /TAGS/ BULL_TAG,READ_TAG,BULL_NEWS_TAG
 
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 
	CHARACTER*12 TAG_KEY,INPUT_KEY
 
	CHARACTER*8 NEXT_MSG_KEY
 
	IF ((.NOT.BULL_TAG.AND.REMOTE_SET.NE.3)
     &	    .OR.(.NOT.BULL_NEWS_TAG.AND.REMOTE_SET.EQ.3)) THEN
	   CALL OPEN_NEW_TAG(IER)
	   IF (.NOT.IER) RETURN
	END IF
 
	IF (REMOTE_SET.EQ.3) THEN
	   CALL GET_FIRST_NEWS_TAG(IER,MESSAGE)
	   RETURN
	END IF
 
	IF (BTEST(READ_TAG,3)) THEN
	   MSG_NUM = 0
 	   CALL GET_NEXT_UNTAG(FOLDER_NUMBER,IER,MESSAGE,DUMMY)
	   IF (IER.EQ.0) THEN
	      MESSAGE = MESSAGE - 1
	      MSG_NUM = MESSAGE
	      MSG_KEY = BULLDIR_HEADER
	   END IF
	   RETURN
	END IF
 
	MSG_KEY = BULLDIR_HEADER
 
	HEADER = .TRUE.
 
	DO J=1,2
	   IF (BTEST(READ_TAG,J)) I = J
	END DO
 
	CALL CONFIRM_TAG(IER,FOLDER_NUMBER,MESSAGE,HEADER,I)
 
	RETURN
 
	ENTRY GET_THIS_TAG(FOLDER_NUMBER,IER,MESSAGE,TAG_TYPE)
 
	IF (REMOTE_SET.EQ.3) THEN
	   CALL GET_THIS_NEWS_TAG(IER,MESSAGE,TAG_TYPE)
	   RETURN
	END IF
 
	TAG_TYPE = 0
 
	DO I=1,2
	   IF (BTEST(READ_TAG,I).OR.BTEST(READ_TAG,3)) THEN
	      DO WHILE (REC_LOCK(IER))
	         READ (13,KEY=TAG_KEY(FOLDER_NUMBER,MSG_KEY,I),
     &		   IOSTAT=IER) INPUT_KEY
	      END DO
	      IF (IER.EQ.0) TAG_TYPE = IBSET(TAG_TYPE,I)
	   END IF
	END DO
 
	IF ((TAG_TYPE.NE.0.AND..NOT.BTEST(READ_TAG,3)).OR.
     &	    (BTEST(READ_TAG,3).AND.
     &	     (.NOT.BTEST(TAG_TYPE,2).OR..NOT.BTEST(READ_TAG,2)).AND.
     &	     (.NOT.BTEST(TAG_TYPE,1).OR..NOT.BTEST(READ_TAG,1)))) THEN
	   IF (IER.EQ.0) UNLOCK 13
	   IER = 0
	   MESSAGE = MSG_NUM
	ELSE
	   IER = 36
	END IF
 
	RETURN
 
	ENTRY GET_THIS_OR_NEXT_TAG(FOLDER_NUMBER,IER,MESSAGE,TAG_TYPE)
 
	MSG_NUM = MSG_NUM - 1
 
	CALL DECREMENT_MSG_KEY
 
	ENTRY GET_NEXT_TAG(FOLDER_NUMBER,IER,MESSAGE,TAG_TYPE)
 
	IF (REMOTE_SET.EQ.3) THEN
	   MSG_NUM = ABS(MSG_NUM) + 1
	   CALL GET_THIS_OR_NEXT_NEWS_TAG(MSG_NUM,IER,MESSAGE,TAG_TYPE)
	   RETURN
	END IF
 
	IER = 36
 
	HEADER = .FALSE.
 
	TAG_TYPE = 0
 
	IF (BTEST(READ_TAG,3)) THEN
	   CALL GET_NEXT_UNTAG(FOLDER_NUMBER,IER,MESSAGE,TAG_TYPE)
	   RETURN
	END IF
 
	DO WHILE (IER.NE.0)
	   I = 0
	   DO J=1,2
	      IF (BTEST(READ_TAG,J)) THEN
	         DO WHILE (REC_LOCK(IER))
	            READ (13,KEYGT=TAG_KEY(FOLDER_NUMBER,MSG_KEY,J),
     &		        IOSTAT=IER) INPUT_KEY
		 END DO
		 IF (IER.EQ.0) THEN
	            CALL LIB$MOVC3(4,%REF(INPUT_KEY),FOLDER1_NUMBER)
		    IF ((J.EQ.1.AND.FOLDER1_NUMBER.NE.FOLDER_NUMBER).OR.
     &		      (J.EQ.2.AND.FOLDER1_NUMBER.NE.-(1+FOLDER_NUMBER)))
     &		      IER = 36
		 END IF
		 IF (IER.EQ.0) THEN
		    IF (J.EQ.1) THEN
		       NEXT_MSG_KEY = INPUT_KEY(5:)
		       I = 1
		    ELSE IF (I.EQ.0.OR.COMPARE_MSG_KEY(NEXT_MSG_KEY,
     &			     INPUT_KEY(5:)).GT.0) THEN
		       I = 2
		    END IF
		 END IF
	      END IF
	   END DO
	   IF (I.EQ.0) RETURN
	   NEXT_MSG_KEY = MSG_KEY
	   CALL CONFIRM_TAG(IER,FOLDER_NUMBER,MESSAGE,HEADER,I)
	   IF (IER.EQ.0) THEN
	      TAG_TYPE = IBSET(TAG_TYPE,I)
	      DO WHILE (REC_LOCK(IER))
	         READ (13,KEY=TAG_KEY(FOLDER_NUMBER,MSG_KEY,3-I),
     &		        IOSTAT=IER) INPUT_KEY
	      END DO
	      IF (IER.EQ.0) TAG_TYPE = IBSET(TAG_TYPE,3-I)
	      IER = 0
	      RETURN
	   ELSE IF (.NOT.BTEST(READ_TAG,3-I)) THEN
	      MSG_KEY = NEXT_MSG_KEY
	      RETURN
	   ELSE
	      MSG_KEY = NEXT_MSG_KEY
	   END IF
	END DO
 
	RETURN
	END
 
 
 
	SUBROUTINE GET_NEXT_UNTAG(FN,IER,MESSAGE,TAG_TYPE)
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLFOLDER.INC'
 
	INCLUDE 'BULLDIR.INC'
 
	COMMON /TAGS/ BULL_TAG,READ_TAG,BULL_NEWS_TAG
 
	INQUIRE (UNIT=2,OPENED=CLOSE_IT)
	CLOSE_IT = .NOT.CLOSE_IT
	IF (CLOSE_IT) CALL OPEN_BULLDIR_SHARED
 
	DO MESSAGE = MSG_NUM+1,F_NBULL
	   CALL READDIR(MESSAGE,IER)
	   IF (IER.EQ.MESSAGE+1) THEN
	      CALL GET_THIS_TAG(FN,IER,DUMMY,TAG_TYPE)
	      IF (IER.EQ.0) THEN
		 IER = 0
		 IF (CLOSE_IT) CALL CLOSE_BULLDIR
		 RETURN
	      END IF
	   END IF
	END DO
 
	IER = 36
	IF (CLOSE_IT) CALL CLOSE_BULLDIR
 
	RETURN
	END
 
 
 
	INTEGER FUNCTION COMPARE_MSG_KEY(MSG_KEY1,MSG_KEY2)
 
	IMPLICIT INTEGER (A-Z)
 
	CHARACTER*8 MSG_KEY1,MSG_KEY2
 
	DIMENSION BTIM1(2),BTIM2(2)
 
	CALL GET_MSGBTIM(MSG_KEY1,BTIM1)
	CALL GET_MSGBTIM(MSG_KEY2,BTIM2)
 
	COMPARE_MSG_KEY = COMPARE_BTIM(BTIM1,BTIM2)
 
	RETURN
	END
 
 
 
 
	SUBROUTINE CONFIRM_TAG(IER,FOLDER_NUMBER,MESSAGE,HEADER,J)
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLDIR.INC'
 
	COMMON /TAGS/ BULL_TAG,READ_TAG,BULL_NEWS_TAG
 
	CHARACTER*12 TAG_KEY,INPUT_KEY
 
	DO WHILE (REC_LOCK(IER))
	   READ (13,KEYGT=TAG_KEY(FOLDER_NUMBER,MSG_KEY,J),IOSTAT=IER)
     &					INPUT_KEY
	END DO
 
	CLOSE_IT = .FALSE.
 
	DO WHILE (1)
	   IF (IER.EQ.0) THEN
	      CALL GET_MSGKEY(%REF(INPUT_KEY(5:)),MSG_KEY)
	      CALL LIB$MOVC3(4,%REF(INPUT_KEY),FOLDER1_NUMBER)
	   END IF
 
	   IF (IER.EQ.0) THEN
	      IF ((J.EQ.1.AND.FOLDER1_NUMBER.NE.FOLDER_NUMBER).OR.
     &		  (J.EQ.2.AND.FOLDER1_NUMBER.NE.-(1+FOLDER_NUMBER)))
     &		  IER = 36
	   END IF
	   IF (IER.NE.0) THEN
	      IER = 1
	      UNLOCK 13
	      IF (CLOSE_IT) CALL CLOSE_BULLDIR
	      RETURN
	   ELSE
	      CALL DECREMENT_MSG_KEY
	      CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
	      INQUIRE (UNIT=2,OPENED=IER)
	      IF (.NOT.IER) THEN
		 CALL OPEN_BULLDIR_SHARED
		 CLOSE_IT = .TRUE.
	      END IF
	      CALL READDIR_KEYGE(IER)
	      CALL GET_MSGKEY(%REF(INPUT_KEY(5:)),INPUT_KEY(5:))
	      IF (IER.NE.0.AND.MSG_KEY.EQ.INPUT_KEY(5:)) THEN
	         UNLOCK 13
		 MESSAGE = MSG_NUM
		 IF (HEADER) THEN
		    MESSAGE = MESSAGE - 1
		    MSG_NUM = MESSAGE
		    MSG_KEY = BULLDIR_HEADER
		 END IF
		 IER = 0
		 IF (CLOSE_IT) CALL CLOSE_BULLDIR
	         RETURN
	      ELSE
		 DELETE (UNIT=13)
		 IF (BTEST(READ_TAG,1).AND.BTEST(READ_TAG,2)) THEN
		    IER = 36
		    IF (CLOSE_IT) CALL CLOSE_BULLDIR
		    RETURN
		 END IF
		 DO WHILE (REC_LOCK(IER))
	            READ (13,IOSTAT=IER) INPUT_KEY
	 	 END DO
	      END IF
	   END IF
 
	END DO
 
	END
 
 
 
	SUBROUTINE CLOSE_TAG
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLUSER.INC'
 
	COMMON /NEWS_MARK/ NEWS_MARK
	DIMENSION NEWS_MARK(128)
	INTEGER*2 NEWS_MARK2(256),NEWS_NUMBER,NEWS_REC
	EQUIVALENCE (NEWS_MARK(1),NEWS_MARK2(1))
	EQUIVALENCE (NEWS_MARK2(1),NEWS_NUMBER)
	EQUIVALENCE (NEWS_MARK2(2),NEWS_REC)
	EQUIVALENCE (NEWS_MARK(2),NEWS_FORMAT)
 
	COMMON /TAGS/ BULL_TAG,READ_TAG,BULL_NEWS_TAG
 
	COMMON /NEWS_TAGS/ NEWS_TAG(4,2,FOLDER_MAX-1)
 
	TAG_OPENED = .FALSE.
 
	IF (BULL_NEWS_TAG) THEN
	   DO I=1,FOLDER_MAX-1
	      DO M=1,2
	         IF (NEWS_TAG(3,M,I).NE.0.AND.NEWS_TAG(4,M,I).EQ.1) THEN
	            IF (.NOT.TAG_OPENED) THEN
		       CALL OPEN_OLD_TAG
		       TAG_OPENED = .TRUE.
		    END IF
		    IF (M.EQ.1) THEN
		       NEWS_REC = 1
		    ELSE
		       NEWS_REC = -32767
		    END IF
	            NEWS_FORMAT = 0
		    IF (NEWS_TAG(2,M,I).GT.32767) NEWS_FORMAT = 1
		    LIMIT = 256/(NEWS_FORMAT+1)
	            NEWS_NUMBER = LAST_NEWS_READ2(1,I)
		    K = 5-NEWS_FORMAT*2
		    SET_LIST = .FALSE.
		    DO J=NEWS_TAG(1,M,I),NEWS_TAG(2,M,I)
		       IF (TEST_TAG(J,%VAL(NEWS_TAG(3,M,I)),
     &				      NEWS_TAG(1,M,I))) THEN
		          IF (.NOT.SET_LIST) THEN
		             CALL SET_NEWS_MARK(K,J)
			     LAST_SET = J
			     K = K + 1
		             SET_LIST = .TRUE.
		          END IF
		       ELSE IF (SET_LIST) THEN
		          IF (LAST_SET.NE.J-1) THEN
			     CALL SET_NEWS_MARK(K,-(J-1))
		             K = K + 1
			  END IF
		          SET_LIST = .FALSE.
		       END IF
		       IF (J.EQ.NEWS_TAG(2,M,I)) THEN
		          IF (SET_LIST.AND.LAST_SET.NE.J) THEN
			     CALL SET_NEWS_MARK(K,-J)
		             K = K + 1
		          END IF
		          DO L=K,LIMIT
			     CALL SET_NEWS_MARK(L,0)
		          END DO
		          K = LIMIT + 1
		       END IF
		       IF (K.GT.LIMIT) THEN
		          DO WHILE (REC_LOCK(IER))
		             READ (23,KEYEQ=NEWS_MARK(1),IOSTAT=IER)
		          END DO
		          IF (IER.NE.0) THEN
		             WRITE (23,IOSTAT=IER) NEWS_MARK
		          ELSE
		             REWRITE (23,IOSTAT=IER) NEWS_MARK
		          END IF
		          K = 5-NEWS_FORMAT*2
		          NEWS_REC = NEWS_REC + 1
		          IF (J.EQ.NEWS_TAG(2,M,I)) THEN
		             DO WHILE (REC_LOCK(IER))
		                READ (23,KEYEQ=NEWS_MARK(1),IOSTAT=IER)
			        IF (IER.EQ.0) THEN
			           DELETE (UNIT=23)
				   NEWS_REC = NEWS_REC + 1
				   L = REC_LOCK(IER)
			        END IF
			     END DO
		          END IF
		       END IF
		    END DO
	         END IF
	      END DO
	   END DO
	   CLOSE (UNIT=23)
	END IF
 
	RETURN
	END
 
 
	SUBROUTINE SET_NEWS_MARK(I,J)
 
	IMPLICIT INTEGER (A-Z)
 
	COMMON /NEWS_MARK/ NEWS_MARK
	DIMENSION NEWS_MARK(128)
	INTEGER*2 NEWS_MARK2(256),NEWS_NUMBER,NEWS_REC
	EQUIVALENCE (NEWS_MARK(1),NEWS_MARK2(1))
	EQUIVALENCE (NEWS_MARK2(1),NEWS_NUMBER)
	EQUIVALENCE (NEWS_MARK2(2),NEWS_REC)
	EQUIVALENCE (NEWS_MARK(2),NEWS_FORMAT)
 
	IF (NEWS_FORMAT.EQ.0) THEN
	   NEWS_MARK2(I) = J
	ELSE
	   NEWS_MARK(I) = J
	END IF
 
	RETURN
	END
 
 
 
	SUBROUTINE ZERO_VM(NUM,NEWS_TAG)
 
	IMPLICIT INTEGER (A-Z)
 
	LOGICAL*1 NEWS_TAG(1)
 
	DO I=1,NUM
	   NEWS_TAG(I) = 0
	END DO
 
	RETURN
	END
 
 
 
 
	SUBROUTINE FREE_TAGS(ISUB)
 
        IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLFOLDER.INC'
 
	INCLUDE 'BULLUSER.INC'
 
	COMMON /NEWS_TAGS/ NEWS_TAG(4,2,FOLDER_MAX-1)
	COMMON /NEWS_MARK/ NEWS_MARK
	DIMENSION NEWS_MARK(128)
	INTEGER*2 NEWS_MARK2(256),NEWS_NUMBER,NEWS_REC
	EQUIVALENCE (NEWS_MARK(1),NEWS_MARK2(1))
	EQUIVALENCE (NEWS_MARK2(1),NEWS_NUMBER)
	EQUIVALENCE (NEWS_MARK2(2),NEWS_REC)
	EQUIVALENCE (NEWS_MARK(2),NEWS_FORMAT)
 
	DO I=1,2
	   IF (NEWS_TAG(3,I,ISUB).GT.0) THEN
	      CALL LIB$FREE_VM(
     &		(NEWS_TAG(2,I,ISUB)-NEWS_TAG(1,I,ISUB))/8+1,NEWS_TAG(3,I,ISUB))
	      NEWS_TAG(3,I,ISUB) = 0
	      NEWS_NUMBER = NEWS_FOLDER_NUMBER
	      NEWS_REC = -32768
	      DO WHILE (REC_LOCK(IER))
	         READ (23,KEYGT=NEWS_MARK(1),IOSTAT=IER) NEWS_MARK
	         IF (IER.EQ.0.AND.NEWS_NUMBER.EQ.NEWS_FOLDER_NUMBER) THEN
		    DELETE (UNIT=23)
		    L = REC_LOCK(IER)
	         END IF
	      END DO
	   END IF
 
	   DO J=I,FOLDER_MAX-2
	      CALL LIB$MOVC3(16,NEWS_TAG(1,I,J+1),NEWS_TAG(1,I,J))
	   END DO
 
	   DO J=1,4
	      NEWS_TAG(J,I,FOLDER_MAX-1) = 0
	   END DO
	END DO
 
	RETURN
	END
 
 
 
 
	SUBROUTINE GET_PREVIOUS_TAG(FN,IER,BULL_READ,TAG_TYPE)
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLDIR.INC'
 
	INCLUDE 'BULLFOLDER.INC'
 
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 
	CHARACTER*8 PREV_MSG_KEY
 
	IER = 36
 
	IF (REMOTE_SET.EQ.3) THEN
	   SUBNUM = NEWS_FIND_SUBSCRIBE()
	   DO WHILE (IER.NE.0.AND.MSG_NUM.GT.F_START)
	      MSG_NUM = MSG_NUM - 1
	      CALL GET_THIS_TAG(FN,IER,MSG_NUM,TAG_TYPE)
	      IF (IER.EQ.0) THEN
		 TMP_MSG_NUM = MSG_NUM
	         CALL READDIR(TMP_MSG_NUM,IER1)
	         IF (IER1.NE.MSG_NUM+1) THEN
	            IF (.NOT.BTEST(READ_TAG,3)) THEN
		       CALL DEL_NEWS_TAG(TAG_TYPE,TMP_MSG_NUM,SUBNUM)
		    END IF
		    IER = 36
	         END IF
	      END IF
	   END DO
	   BULL_READ = MSG_NUM
	ELSE
	   IF (MSG_NUM.EQ.0) RETURN
	   SAVE_MSG_NUM = MSG_NUM
	   PREV_MSG_NUM = MSG_NUM
	   MSG_NUM = 0
	   MSG_KEY = BULLDIR_HEADER
	   IER = 0
	   DO WHILE (IER.EQ.0.AND.MSG_NUM.LT.SAVE_MSG_NUM)
	      IF (MSG_NUM.GT.0) THEN
	         PREV_MSG_KEY = MSG_KEY
	         PREV_MSG_NUM = MSG_NUM
	      END IF
	      CALL GET_NEXT_TAG(FN,IER,BULL_READ,TAG_TYPE)
	   END DO
	   IF (PREV_MSG_NUM.LT.SAVE_MSG_NUM) THEN
	      MSG_NUM = PREV_MSG_NUM
	      MSG_KEY = PREV_MSG_KEY
	      CALL GET_THIS_OR_NEXT_TAG(FN,IER,BULL_READ,TAG_TYPE)
	   ELSE
	      IER = 36
	   END IF
	END IF
 
	RETURN
	END
 
 
	SUBROUTINE DECREMENT_MSG_KEY
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLDIR.INC'
 
	I = 1
	DO WHILE (I.LT.9)
	   ITEST = ICHAR(MSG_KEY(I:I))
	   IF (ITEST.GT.0) THEN
	      MSG_KEY(I:I) = CHAR(ITEST-1)
	      I = 9
	   ELSE
	      I = I + 1
	   END IF
	END DO
 
	RETURN
	END
