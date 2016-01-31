C
C  BULLETIN5.FOR, Version 3/5/98
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
C
	SUBROUTINE SET_FOLDER_DEFAULT(NOTIFY,READNEW,BRIEF)
C
C  SUBROUTINE SET_FOLDER_DEFAULT
C
C  FUNCTION: Sets flag defaults for specified folder
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*256 INCMD

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	EXTERNAL CLI$_NEGATED

	IF (REMOTE_SET.GE.3) THEN
	   CALL SET_NEWS_FOLDER_DEFAULT(NOTIFY,READNEW,BRIEF)
	   RETURN
        ELSE IF (FOLDER_NUMBER.LT.0) THEN
	   WRITE (6,'('' ERROR: Command is invalid for this folder.'')')
	   RETURN
	END IF

	ALL = .FALSE.
	DEFAULT = 0
	NODEFAULT = 0

	IF (INCMD(:3).EQ.'SET') THEN
	   IF (.NOT.SETPRV_PRIV()) THEN
	      WRITE (6,'(
     &           '' ERROR: Privileges needed for changing defaults.'')')
	      RETURN
	   END IF
	   ALL = CLI$PRESENT('ALL')
	   DEFAULT = CLI$PRESENT('DEFAULT')
	   NODEFAULT = CLI$PRESENT('NODEFAULT')
	   CALL OPEN_BULLUSER_SHARED
	   IF (CLI$PRESENT('PERMANENT')) THEN
	      CALL SET_PERM(NOTIFY,READNEW,BRIEF)
	   ELSE IF (CLI$PRESENT('NOPERMANENT')) THEN
	      IF (NOTIFY.GE.0) CALL SET_PERM(0,-1,-1)
	      IF (READNEW.GE.0.OR.BRIEF.GE.0) CALL SET_PERM(-1,0,0)
	   END IF
	ELSE
	   CALL OPEN_BULLUSER_SHARED
	END IF

	CALL READ_USER_FILE_HEADER(IER)
	IF (NODEFAULT) THEN
	   IF (NOTIFY.NE.-1) CALL CLR2(NOTIFY_FLAG_DEF,FOLDER_NUMBER)
	   IF (READNEW.NE.-1.OR.BRIEF.NE.-1) THEN
	      CALL CLR2(SET_FLAG_DEF,FOLDER_NUMBER)
	      CALL CLR2(BRIEF_FLAG_DEF,FOLDER_NUMBER)
	   END IF
	   REWRITE(4) USER_HEADER
	ELSE IF (DEFAULT.EQ.0.OR.DEFAULT) THEN
	   IF (NOTIFY.EQ.0) CALL CLR2(NOTIFY_FLAG_DEF,FOLDER_NUMBER)
	   IF (NOTIFY.EQ.1) CALL SET2(NOTIFY_FLAG_DEF,FOLDER_NUMBER)
	   IF (READNEW.EQ.0) CALL CLR2(SET_FLAG_DEF,FOLDER_NUMBER)
	   IF (READNEW.EQ.1) CALL SET2(SET_FLAG_DEF,FOLDER_NUMBER)
	   IF (BRIEF.EQ.0) CALL CLR2(BRIEF_FLAG_DEF,FOLDER_NUMBER)
	   IF (BRIEF.EQ.1) CALL SET2(BRIEF_FLAG_DEF,FOLDER_NUMBER)
	   REWRITE(4) USER_HEADER
	END IF

	IF (ALL.OR.(BRIEF.NE.-1.AND.NOTIFY.NE.-1.AND.READNEW.NE.-1)) THEN
	   CALL READ_USER_FILE(IER)
	   DO WHILE (IER.EQ.0)
	      IF (TEMP_USER(:1).NE.'*'.AND.TEMP_USER(:1).NE.':') THEN
	         IF (NOTIFY.EQ.0) CALL CLR2(NOTIFY_FLAG,FOLDER_NUMBER)
	         IF (NOTIFY.EQ.1) CALL SET2(NOTIFY_FLAG,FOLDER_NUMBER)
	         IF (READNEW.EQ.0) CALL CLR2(SET_FLAG,FOLDER_NUMBER)
	         IF (READNEW.EQ.1) CALL SET2(SET_FLAG,FOLDER_NUMBER)
	         IF (BRIEF.EQ.0) CALL CLR2(BRIEF_FLAG,FOLDER_NUMBER)
	         IF (BRIEF.EQ.1) CALL SET2(BRIEF_FLAG,FOLDER_NUMBER)
	         REWRITE(4) TEMP_USER//USER_ENTRY(13:)
	      END IF
	      CALL READ_USER_FILE(IER)
	   END DO
	END IF

	CALL CLOSE_BULLUSER

	RETURN
	END




	SUBROUTINE READ_PERM

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	COMMON /BULL_PERM/ SET_PERM_FLAG,BRIEF_PERM_FLAG,NOTIFY_PERM_FLAG
	DIMENSION SET_PERM_FLAG(FLONG)
	DIMENSION BRIEF_PERM_FLAG(FLONG)
	DIMENSION NOTIFY_PERM_FLAG(FLONG)

	COMMON /FLAG_ACCESS/ FLAG_ACCESS

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	DO WHILE (REC_LOCK(IER))
	   READ (4,KEY='*PERM',IOSTAT=IER) TEMP_USER,
     &		SET_PERM_FLAG,BRIEF_PERM_FLAG,NOTIFY_PERM_FLAG
	END DO

	IF (IER.NE.0) THEN
	   DO I=1,FLONG
	      SET_PERM_FLAG(I) = 0
	      BRIEF_PERM_FLAG(I) = 0
	      NOTIFY_PERM_FLAG(I) = 0
	   END DO
	   BRIEF_PERM_FLAG(1) = 1	! SHOWNEW permanent for GENERAL folder
	   WRITE (4,IOSTAT=IER)
     &		'*PERM       ',
     &		SET_PERM_FLAG,BRIEF_PERM_FLAG,NOTIFY_PERM_FLAG
	   CALL READ_USER_FILE_HEADER(IER)
	   IF (.NOT.TEST2(SET_FLAG_DEF,0)) THEN
	      CALL SET2(BRIEF_FLAG_DEF,0)
	      REWRITE(4) USER_HEADER
	   END IF
	   CALL READ_USER_FILE(IER)
	   DO WHILE (IER.EQ.0)
	      IF (TEMP_USER(:1).NE.'*'.AND.TEMP_USER(:1).NE.':') THEN
		 IF (.NOT.TEST2(SET_FLAG,0)) THEN
		    CALL SET2(BRIEF_FLAG,0)
	            REWRITE(4) TEMP_USER//USER_ENTRY(13:)
	         END IF
	      END IF
	      CALL READ_USER_FILE(IER)
	   END DO
	ELSE
	   UNLOCK 4
	END IF

	RETURN

	ENTRY SET_PERM(NOTIFY,READNEW,BRIEF)

	DO WHILE (REC_LOCK(IER))
	   READ (4,KEY='*PERM',IOSTAT=IER) TEMP_USER,
     &		SET_PERM_FLAG,BRIEF_PERM_FLAG,NOTIFY_PERM_FLAG
	END DO

	IF (NOTIFY.EQ.0) CALL CLR2(NOTIFY_PERM_FLAG,FOLDER_NUMBER)
	IF (NOTIFY.EQ.1) CALL SET2(NOTIFY_PERM_FLAG,FOLDER_NUMBER)
	IF (READNEW.EQ.0) CALL CLR2(SET_PERM_FLAG,FOLDER_NUMBER)
	IF (READNEW.EQ.1) CALL SET2(SET_PERM_FLAG,FOLDER_NUMBER)
	IF (BRIEF.EQ.0) CALL CLR2(BRIEF_PERM_FLAG,FOLDER_NUMBER)
	IF (BRIEF.EQ.1) CALL SET2(BRIEF_PERM_FLAG,FOLDER_NUMBER)

	REWRITE (4,IOSTAT=IER) TEMP_USER,
     &		SET_PERM_FLAG,BRIEF_PERM_FLAG,NOTIFY_PERM_FLAG

	RETURN

	ENTRY SET_USER_FLAG(NOTIFY,READNEW,BRIEF)

	IF (.NOT.FLAG_ACCESS) THEN
	   WRITE (6,'('' ERROR: Cannot set flags for protected'',
     &	     '' folder without explicit access granted'',/,
     &	     '' via SET ACCESS.  See HELP SET ACCESS for further''
     &       '' information.'')')
	   RETURN
	END IF

	IF (REMOTE_SET.GE.3) THEN
 	   IF (REMOTE_SET.EQ.3.AND.NOTIFY.EQ.1) THEN
	      WRITE (6,'('' ERROR: NOTIFY is not valid for this folder.'')')
	      RETURN
	   END IF
	   CALL NEWS_SET_USER_FLAG(NOTIFY,READNEW,BRIEF)
	   RETURN
	END IF

	CALL OPEN_BULLUSER_SHARED

	DO WHILE (REC_LOCK(IER))
	   READ (4,KEY='*PERM',IOSTAT=IER) TEMP_USER,
     &		SET_PERM_FLAG,BRIEF_PERM_FLAG,NOTIFY_PERM_FLAG
	END DO

	CALL CLOSE_BULLUSER

	IER = .TRUE.
	IF (NOTIFY.EQ.0) THEN
 	   IF (TEST2(NOTIFY_PERM_FLAG,FOLDER_NUMBER)) THEN
	      WRITE (6,'('' ERROR: NOTIFY is permanent for this folder.'')')
	      RETURN
	   ELSE
	      CALL CHANGE_FLAG(0,4)
	   END IF
	ELSE IF (NOTIFY.EQ.1) THEN
	   CALL CHANGE_FLAG(1,4)
	   RETURN
	ELSE IF (BRIEF.EQ.0.AND.READNEW.EQ.0.AND.
     &	   (TEST2(SET_PERM_FLAG,FOLDER_NUMBER).OR.
     &	   TEST2(BRIEF_PERM_FLAG,FOLDER_NUMBER))) THEN
	   IER = .FALSE.
	ELSE IF (BRIEF.EQ.1.AND.READNEW.EQ.0.AND.
     &	   TEST2(SET_PERM_FLAG,FOLDER_NUMBER).AND.
     &	   .NOT.TEST2(BRIEF_PERM_FLAG,FOLDER_NUMBER)) THEN
	   IER = .FALSE.
	ELSE IF (BRIEF.EQ.1.AND.READNEW.EQ.1.AND.
     &	   (TEST2(SET_PERM_FLAG,FOLDER_NUMBER).XOR.
     &	   TEST2(BRIEF_PERM_FLAG,FOLDER_NUMBER)))  THEN
	   IER = .FALSE.
	END IF

	IF (IER) THEN
	   IF (READNEW.GE.0) CALL CHANGE_FLAG(READNEW,2)
	   IF (BRIEF.GE.0) CALL CHANGE_FLAG(BRIEF,3)
	ELSE
	   WRITE (6,'('' ERROR: PERMANENT flags exist for this folder.'')')
	   WRITE (6,'('' Flags will be set to those permanent settings.'')')

	   IF (TEST2(SET_PERM_FLAG,FOLDER_NUMBER)) THEN
	      CALL CHANGE_FLAG(1,2)
	   ELSE
	      CALL CHANGE_FLAG(0,2)
	   END IF

	   IF (TEST2(BRIEF_PERM_FLAG,FOLDER_NUMBER)) THEN
	      CALL CHANGE_FLAG(1,3)
	   ELSE
	      CALL CHANGE_FLAG(0,3)
	   END IF
	END IF

	RETURN
	END





	SUBROUTINE REMOVE_FOLDER
C
C  SUBROUTINE REMOVE_FOLDER
C
C  FUNCTION: Removes a bulletin folder.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLDIR.INC'

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	EXTERNAL CLI$_ABSENT

	CHARACTER RESPONSE*4,TEMP*80

	IER = CLI$GET_VALUE('REMOVE_FOLDER',FOLDER1,LEN_T) ! Get folder name

	IF (IER.EQ.%LOC(CLI$_ABSENT)) THEN
	   IF (.NOT.FOLDER_SET) THEN
	      WRITE (6,'('' ERROR: No folder specified.'')')
	      RETURN
	   ELSE
	      FOLDER1 = FOLDER
	   END IF
	ELSE IF (LEN_T.GT.44) THEN
	   WRITE(6,'('' ERROR: Folder name must be < 26 characters.'')')
	   RETURN
	END IF

	CALL GET_INPUT_PROMPT(RESPONSE,LEN,
     &   'Are you sure you want to remove folder '
     &	 //FOLDER1(:TRIM(FOLDER1))//' (Y/N with N as default): ')
	IF (RESPONSE(:1).NE.'y'.AND.RESPONSE(:1).NE.'Y') THEN
	   WRITE (6,'('' Folder was not removed.'')')
	   RETURN
	END IF

	IF (INDEX(FOLDER1,'.').GT.0) THEN
	   CALL OPEN_BULLNEWS_SHARED
	ELSE
	   CALL OPEN_BULLFOLDER
	END IF

	CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER)	! See if folder exists
	CALL SET_FOLDER_FILE(1)

	IF (IER.NE.0) THEN
	   WRITE (6,'('' ERROR: No such folder exists.'')')
	   GO TO 1000
	ELSE IF (INDEX(FOLDER1,'.').GT.0) THEN
	   CALL REMOTE_REMOVE_FOLDER(IER)
	   IF (.NOT.IER) GO TO 1000
	END IF

	IF (.NOT.FOLDER_ACCESS(USERNAME,FOLDER1_FLAG,FOLDER1_OWNER).OR.
     &	     (FOLDER1_NUMBER.EQ.0.AND.FOLDER1_BBOARD(:2).NE.'::')) THEN
	   WRITE (6,'('' ERROR: You are not able to remove the folder.'')')
	   GO TO 1000
	END IF

	TEMP = FOLDER_FILE
	FOLDER_FILE = FOLDER1_FILE

	REMOTE_SET_SAVE = REMOTE_SET
	REMOTE_SET = .FALSE.

	IF (FOLDER1_BBOARD(:2).EQ.'::'.AND.BTEST(FOLDER1_FLAG,2)) THEN
	   FLEN = TRIM(FOLDER1_BBOARD)
	   IF (INDEX(FOLDER1_BBOARD,'*').GT.0) FLEN = FLEN - 1
	   OPEN (UNIT=17,STATUS='UNKNOWN',IOSTAT=IER,
     &		RECL=256,FILE=FOLDER1_BBOARD(3:FLEN)
     &		//'::"TASK=BULLETIN1"')
	   IF (IER.EQ.0) THEN		! Deregister remote SYSTEM folder
	      IF (INDEX(FOLDER1_BBOARD,'*').GT.0) THEN
	         CALL OPEN_BULLDIR
	         CALL READDIR(0,IER)
		 IF (IER.EQ.1) FOLDER1 = BULLDIR_HEADER(13:)
		 CALL CLOSE_BULLDIR
	      END IF
	      WRITE (17,'(2A)',IOSTAT=IER) 1,FOLDER1	! Select folder
	      IF (IER.EQ.0) READ(17,'(5A)',IOSTAT=IER)	! Throw away response
	      IF (IER.EQ.0) WRITE(17,'(2A)',IOSTAT=IER) 14,0	! Deregister
	      CLOSE (UNIT=17)
	   END IF
	END IF

	TEMPSET = FOLDER_SET
	FOLDER_SET = .TRUE.
	CALL SYS$SETDFPROT('FF00'X,CUR_DEF_PROT)
		! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD,GROUP)
		! in case files don't exist and are created.
	CALL OPEN_BULLDIR			! Remove directory file
	CALL OPEN_BULLFIL			! Remove bulletin file
	CALL CLOSE_BULLFIL_DELETE
	CALL CLOSE_BULLDIR_DELETE
	CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection
	FOLDER_FILE = TEMP
	FOLDER_SET = TEMPSET

	DELETE (7)

	IF (FOLDER1_NUMBER.NE.0.OR.FOLDER1_BBOARD(:2).NE.'::') THEN
		! Test is due to bug which changes folder number to zero
	   TEMP_NUMBER = FOLDER_NUMBER
	   FOLDER_NUMBER = FOLDER1_NUMBER
	   TEMP_FLAG = FOLDER_FLAG
	   IF (BTEST(FOLDER1_FLAG,2)) THEN
	      FOLDER_FLAG = IBCLR(FOLDER1_FLAG,2)
	      CALL MODIFY_SYSTEM_LIST(0)
	   END IF
	   CALL SET_FOLDER_DEFAULT(0,0,0)
	   FOLDER_FLAG = TEMP_FLAG
	   FOLDER_NUMBER = TEMP_NUMBER
	END IF

	WRITE (6,'('' Folder removed.'')')

	IF (FOLDER.EQ.FOLDER1) THEN
	   CALL CLOSE_BULLFOLDER
	   FOLDER_SET = .FALSE.
           FOLDER_NUMBER = 0
           CALL SELECT_FOLDER(.FALSE.,IER)
           WRITE (6,'('' Resetting to '',A,'' folder.'')')
     &              FOLDER(:TRIM(FOLDER))
	   RETURN
	ELSE
	   REMOTE_SET = REMOTE_SET_SAVE
	END IF

1000	CALL CLOSE_BULLFOLDER

	RETURN

	END


	SUBROUTINE SELECT_FOLDER(OUTPUT,IER)
C
C  SUBROUTINE SELECT_FOLDER
C
C  FUNCTION: Selects the specified folder.
C
C  INPUTS:
C	OUTPUT - Specifies whether status messages are outputted.
C
C  NOTES:
C	FOLDER_NUMBER is used for selecting the folder.
C	If FOLDER_NUMBER = -1, the name stored in FOLDER1 is used.
C	If FOLDER_NUMBER = -2, the name stored in FOLDER1 is used,
C	but the folder is not selected if it is remote.
C	If the specified folder is on a remote node and does not have
C	a local entry (i.e. specified via NODENAME::FOLDERNAME), then
C	FOLDER_NUMBER is set to -1.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE '($RMSDEF)'
	INCLUDE '($SSDEF)'

	COMMON /POINT/ BULL_POINT

	COMMON /ACCESS/ READ_ONLY
	LOGICAL READ_ONLY

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*256 INCMD

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
	DATA REMOTE_SET /.FALSE./

	COMMON /SHUTDOWN/ NODE_NUMBER,NODE_AREA
	COMMON /SHUTDOWN/ SHUTDOWN_FLAG(FLONG)

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /TAGS/ BULL_TAG,READ_TAG

	COMMON /SAVE_FOLDERS/ SAVE_FOLDER_Q1,SAVE_FOLDER_NUM

	COMMON /BULL_NOTIFY/ NOTIFY_REMOTE(FLONG)

	COMMON /HEADER/ HEADER

	COMMON /READIT/ READIT

	COMMON /FLAG_ACCESS/ FLAG_ACCESS

	COMMON /BULL_USER_CUSTOM/ BULL_USER_CUSTOM
	DATA BULL_USER_CUSTOM/.FALSE./

	COMMON /LAST_FOLDER/ LAST_FOLDER_NUMBER 

	EXTERNAL CLI$_ABSENT,CLI$_NEGATED

	CHARACTER FSTATUS*4,FOLDER1_SAVE*44,NEWS_ACCESS*132

	CHARACTER*80 LOCAL_FOLDER1_DESCRIP

	DIMENSION FIRST_TIME(FLONG)	! Bit set for folder if folder has
	DATA FIRST_TIME /FLONG*0/	! been selected before this.

	DIMENSION OLD_NEWEST_BTIM(2)

	DATA LAST_NEWS_GROUP/0/

	CALL UPDATE_EXCLUDE

	CALL UPDATE_USERINFO

	COMMAND = (INCMD(:3).EQ.'ADD').OR.(INCMD(:3).EQ.'DEL').OR.
     &		  (INCMD(:3).EQ.'DIR').OR.(INCMD(:3).EQ.'IND').OR.
     &		  (INCMD(:3).EQ.'REP').OR.(INCMD(:3).EQ.'SEL').OR.
     &		  (INCMD(:3).EQ.'SET').OR.(INCMD(:3).EQ.'SEA')

	IF (.NOT.OUTPUT.OR.FOLDER_NUMBER.NE.-1.OR.COMMAND) THEN
	   IF (OUTPUT) THEN			! Get folder name
	      IER = CLI$GET_VALUE('SELECT_FOLDER',FOLDER1_NAME)
	      FOLDER1 = FOLDER1_NAME
	   END IF

	   FLEN = TRIM(FOLDER1)		! Add GENERAL after :: if no
	   IF (FLEN.GT.1) THEN		! name specified after the ::
	      IF (FOLDER1(FLEN-1:FLEN).EQ.'::') THEN
	         FOLDER1 = FOLDER1(:FLEN)//'GENERAL'
	      END IF
	   END IF

	   IF (((IER.EQ.%LOC(CLI$_ABSENT).OR.FOLDER1.EQ.'GENERAL').AND.
     &	    OUTPUT).OR.((FOLDER_NUMBER.EQ.0.OR.(FOLDER1.EQ.'GENERAL'.AND.
     &	    FOLDER_NUMBER.LE.-1)).AND..NOT.OUTPUT)) THEN ! Select GENERAL
	      FOLDER_NUMBER = 0
	      FOLDER1 = 'GENERAL'
	   END IF
	END IF

	REMOTE_TEST = 0
	REMOTE_SET_NEW = 0

	IF (SAVE_FOLDER_Q1.NE.0) THEN			! Have folder info
	   FOLDER1_COM = FOLDER_COM
	   IER = 0
	   NEWS = INDEX(FOLDER1,'.').GT.0.OR.(FOLDER1(:1).GE.'a'.AND.
     &					   FOLDER1(:1).LE.'z')
	   IF (NEWS.AND.BTEST(FOLDER1_FLAG,8)) REMOTE_SET_NEW = 4
	ELSE
	   NEWS = ((INDEX(FOLDER1,'.').GT.0.OR.(FOLDER1(:1).GE.'a'.AND.
     &		FOLDER1(:1).LE.'z')).AND.(FOLDER_NUMBER.LE.-1.OR.OUTPUT))
     &		.OR.(FOLDER_NUMBER.GT.1000.AND..NOT.OUTPUT)
	   IF (NEWS.AND.
     &		SYS_TRNLNM('BULL_NEWS_SERVER','DEFINED')) THEN
	      CALL OPEN_BULLNEWS_SHARED		! Go find folder
	      READ (7,IOSTAT=IER,KEYEQ=1000,KEYID=1) NEWS_FOLDER1_COM
              NEWS_EXPIRE_DEFAULT = NEWS_F1_EXPIRE
              NEWS_EXPIRE_LIMIT_DEFAULT = NEWS_F1_EXPIRE_LIMIT
	      IF (IER.NE.0) THEN
		 WRITE (6,'('' Fetching NEWS groups from remote node.''
     &			,''  This will take several minutes.'')')
		 WRITE (6,'('' This is the only time this will have''
     &			,'' to be done.'')')
	         CALL CLOSE_BULLFOLDER
		 FOLDER1_SAVE = FOLDER1
		 CALL NEWS_LIST
	         CALL OPEN_BULLFOLDER_SHARED
		 FOLDER1 = FOLDER1_SAVE
	      ELSE IF (NEWS_F1_COUNT.GT.LAST_NEWS_READ(1,FOLDER_MAX).AND.
     &		       OUTPUT.AND.NEWS_F1_COUNT.GT.LAST_NEWS_GROUP) THEN
		 IF (LAST_NEWS_READ(1,FOLDER_MAX).GT.1000) THEN
		    FOLDER1_SAVE = FOLDER1
	            FOLDER1_NUMBER = LAST_NEWS_READ(1,FOLDER_MAX)
	            IER = 2
		    DO WHILE (IER.EQ.2)
		       CALL READ_FOLDER_FILE_KEYNUM_GT_TEMP
     &		          (FOLDER1_NUMBER,IER)
		       IF (IER.EQ.0.AND.BTEST(FOLDER1_FLAG,10)) IER = 2
	            END DO
		    FOLDER1 = FOLDER1_SAVE
		 END IF
		 IF (LAST_NEWS_READ(1,FOLDER_MAX).GT.1000
     &			.AND.IER.EQ.0) THEN
		    WRITE (6,'('' Type NEWS/NEWGROUP to see recently'',
     &			       '' added news groups.'')')
		 ELSE
		    LAST_NEWS_READ(1,FOLDER_MAX) = NEWS_F1_COUNT
		 END IF
		 LAST_NEWS_GROUP = NEWS_F1_COUNT
		 FOLDER1_SAVE = FOLDER1
	         CALL STR$UPCASE(FOLDER1,FOLDER1)
	         CALL READ_FOLDER_FILE_KEYNAME_TEMP
     &				(FOLDER1(:INDEX(FOLDER1,'.')),IER)
	         IF (IER.EQ.0) THEN
		    IF (NEWS_F1_EXPIRE.GT.0) 
     &		       NEWS_EXPIRE_DEFAULT = NEWS_F1_EXPIRE
		    IF (NEWS_F1_EXPIRE_LIMIT.NE.0) 
     &		       NEWS_EXPIRE_LIMIT_DEFAULT = NEWS_F1_EXPIRE_LIMIT
	         END IF
	         FOLDER1 = FOLDER1_SAVE
	      END IF
	      CALL LOWERCASE(FOLDER1)
	   ELSE
	      CALL OPEN_BULLFOLDER_SHARED		! Go find folder
	   END IF

	   IF ((OUTPUT.AND.(FOLDER_NUMBER.NE.0.OR.FOLDER1.NE.'GENERAL'))
     &		.OR.FOLDER_NUMBER.LE.-1) THEN
	      REMOTE_TEST = INDEX(FOLDER1,'::')
	      IF (REMOTE_TEST.GT.0) THEN
	         FOLDER1_BBOARD = '::'//FOLDER1(:REMOTE_TEST-1)
	         FOLDER1 = FOLDER1(REMOTE_TEST+2:TRIM(FOLDER1))
	         FOLDER1_NUMBER = -1
	         IER = 0
	      ELSE IF (INCMD(:2).EQ.'SE') THEN
	         CALL READ_FOLDER_FILE_KEYNAME_TEMP
     &				(FOLDER1(:TRIM(FOLDER1)),IER)
	      ELSE
	         CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER)
	      END IF
	   ELSE
	      FOLDER1_NUMBER = FOLDER_NUMBER
	      CALL READ_FOLDER_FILE_KEYNUM_TEMP(FOLDER_NUMBER,IER)
	   END IF

	   IF (REMOTE_TEST.EQ.0.AND.IER.EQ.0) THEN
	      IF (BTEST(FOLDER1_FLAG,29)) THEN		! Error in folder flag!!
	         FOLDER1_FLAG = FOLDER1_FLAG.AND.3
	         F1_EXPIRE_LIMIT = 0
	         CALL REWRITE_FOLDER_FILE_TEMP(IER1)
	      END IF
	   END IF

	   CALL CLOSE_BULLFOLDER

	   IF (NEWS.AND.BTEST(FOLDER1_FLAG,8).AND.IER.EQ.0) THEN
	      REMOTE_SET_NEW = 4
	      CALL SYS_BINTIM('-',EX_BTIM)
	   END IF
	END IF

	IF (BTEST(FOLDER1_FLAG,9)) THEN
	   IF (OUTPUT) THEN
	       WRITE(6,'('' This news group has been disabled.'')')
	   END IF
	   IER = 2
	   RETURN
	END IF

	IF ((IER.EQ.0.OR.NEWS).AND.REMOTE_SET_NEW.NE.4.AND.
     &		FOLDER1_BBOARD(:2).EQ.'::') THEN
	   IF (FOLDER_NUMBER.EQ.-2) RETURN	! Don't allow
	   IF (IER.NE.0) FOLDER1_DESCRIP = FOLDER1_NAME
	   LOCAL_FOLDER1_FLAG = FOLDER1_FLAG
	   LOCAL_FOLDER1_DESCRIP = FOLDER1_DESCRIP
	   CALL CONNECT_REMOTE_FOLDER(READ_ONLY,IER1)
	   IF (IER1.NE.0) THEN
	      IF (OUTPUT) THEN
	         WRITE (6,'('' ERROR: Unable to select the folder.'')')
		 IF (.NOT.NEWS) THEN
		    LENB = TRIM(FOLDER1_BBOARD)
		    IF (FOLDER1_BBOARD(LENB:LENB).EQ.'*') LENB = LENB - 1
	            WRITE (6,'('' Cannot connect to node '',A,''.'')')
     &		        FOLDER1_BBOARD(3:LENB)
		 ELSE IF (.NOT.IER1) THEN
	            WRITE (6,'('' Cannot connect to remote NEWS node.'')')
	         END IF
	      END IF
	      RETURN
	   END IF
	   IF (REMOTE_TEST.GT.0) THEN	! Folder specified with "::"
	      FOLDER1 = FOLDER1_BBOARD(3:TRIM(FOLDER1_BBOARD))//'::'//
     &			FOLDER1
	      FOLDER1_NUMBER = -1
	      REMOTE_SET_NEW = 1
	   ELSE IF (NEWS) THEN
	      REMOTE_SET_NEW = 3
	      CALL OPEN_BULLNEWS_SHARED	! Update local folder information
	      IF (IER.NE.0) CALL NEWS_NEW_FOLDER
              CALL READ_FOLDER_FILE_KEYNAME(FOLDER1,IER)
	      IF ((F1_START.NE.F_START.OR.F1_NBULL.NE.F_NBULL).AND.
     &		  (F1_START.GT.0.OR.F_START.LE.F_NBULL)) THEN
	         IF (F1_NBULL.NE.F_NBULL) CALL SYS_BINTIM('-',F_NEWEST_BTIM)
	         F_COUNT = F1_COUNT
		 IF (F1_START.GT.0) THEN
		    IF (F1_NBULL.LT.F_NBULL) THEN
		       CALL NEWS_GET_NEWEST_MESSAGE(IER)
		       IF (IER-1.GE.F1_NBULL) THEN 
			  CALL NEWS_UPDATE_NEWEST_MESSAGE(F_NBULL)
		       END IF
		    ELSE
	               F_NBULL = F1_NBULL
		    END IF
	            F_START = F1_START
		 ELSE
		    F_START = F_NBULL + 1
		 END IF
	         CALL REWRITE_FOLDER_FILE(IER)
	      END IF
	      CALL CLOSE_BULLFOLDER
	   ELSE				! True remote folder
	      FOLDER1_DESCRIP = LOCAL_FOLDER1_DESCRIP	! Use local description
	      IF (BTEST(FOLDER1_FLAG,0)) THEN	! If remote folder is protected
		 LOCAL_FOLDER1_FLAG = IBSET(LOCAL_FOLDER1_FLAG,0)
	      END IF
	      FOLDER1_FLAG = LOCAL_FOLDER1_FLAG		! Use local flag info
	      REMOTE_SET_NEW = 1
	   END IF
	END IF

	IF (IER.EQ.0) THEN				! Folder found
	   FLAG1_ACCESS = .TRUE.
	   CALL SET_FOLDER_FILE(1)
	   IF (BTEST(FOLDER1_FLAG,0)) THEN		! Folder protected?
	      IF (NEWS) THEN
		 CALL CHKACL(NEWS_ACCESS(FOLDER1_DESCRIP),IER)
	      ELSE
	         CALL CHKACL
     &		 (FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL',IER)
	      END IF
	      IF (IER.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL).AND.USERNAME
     &		  .NE.FOLDER1_OWNER.AND.IER) THEN
	         IF (NEWS) THEN
	            CALL CHECK_ACCESS
     &		     (NEWS_ACCESS(FOLDER1_DESCRIP),
     &		     USERNAME,READ_ACCESS,WRITE_ACCESS)
	         ELSE
	            CALL CHECK_ACCESS
     &		     (FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL',
     &		     USERNAME,READ_ACCESS,WRITE_ACCESS)
	         END IF
	         IF (SETPRV_PRIV().AND.READIT.EQ.0) THEN
		    IF (.NOT.READ_ACCESS) FLAG1_ACCESS = .FALSE.
	            READ_ACCESS = 1
		    WRITE_ACCESS = 1
	         END IF
	         IF (.NOT.READ_ACCESS.AND..NOT.WRITE_ACCESS) THEN
		  IF (OUTPUT.AND.NEWS) THEN
	           WRITE(6,'('' You are not allowed to access news group.'')')
		  ELSE IF (NEWS) THEN
		   IF (NEWS_FIND_SUBSCRIBE().LE.FOLDER_MAX-1) THEN
		      CALL NEWS_SET_USER_FLAG(0,0,0)
		   END IF
		  ELSE IF (OUTPUT) THEN
	           WRITE(6,'('' You are not allowed to access folder.'')')
	           WRITE(6,'('' See '',A,'' if you wish to access folder.'')')
     &			FOLDER1_OWNER(:TRIM(FOLDER1_OWNER))
		  ELSE IF (TEST2(BRIEF_FLAG,FOLDER1_NUMBER).OR.
     &			 TEST2(SET_FLAG,FOLDER1_NUMBER)) THEN
		   CALL OPEN_BULLUSER_SHARED
		   CALL READ_USER_FILE_KEYNAME(USERNAME,IER)
		   CALL CLR2(BRIEF_FLAG,FOLDER1_NUMBER)
		   CALL CLR2(SET_FLAG,FOLDER1_NUMBER)
		   IF (IER.EQ.0) REWRITE (4) USER_ENTRY
		   CALL CLOSE_BULLUSER
		  END IF
		  IER = 0
		  RETURN
	         END IF
	      ELSE IF (BTEST(FOLDER1_FLAG,0).AND.(.NOT.IER.OR.
     &		  (IER.EQ.(SS$_ACLEMPTY.OR.SS$_NORMAL).AND..NOT.NEWS))
     &		  .AND.(FOLDER1_BBOARD(:2).NE.'::'.OR.NEWS)) THEN
	         IF (NEWS) THEN
	            CALL OPEN_BULLNEWS_SHARED
	         ELSE
	            CALL OPEN_BULLFOLDER
	         END IF
	         CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER1)
		 FOLDER1_FLAG = IBCLR(FOLDER1_FLAG,0)
	         CALL REWRITE_FOLDER_FILE_TEMP(IER1) 
		 CALL CLOSE_BULLFOLDER
	      ELSE IF (FOLDER1_BBOARD(:2).EQ.'::') THEN
	         IER = SS$_ACLEMPTY.OR.SS$_NORMAL
	      END IF
	   ELSE					! Folder not protected
	      IER = SS$_ACLEMPTY.OR.SS$_NORMAL	! Indicate folder selected
	   END IF

	   IF (REMOTE_SET_NEW.NE.1.AND.REMOTE_SET.EQ.1) 
     &					CLOSE(UNIT=REMOTE_UNIT)

	   REMOTE_SET = REMOTE_SET_NEW 

	   IF (IER) THEN
	      FLAG_ACCESS = FLAG1_ACCESS	! Can set flags?

	      FOLDER_COM = FOLDER1_COM		! Folder successfully set so
	      FOLDER_FILE = FOLDER1_FILE	! update folder parameters

	      IF (FOLDER_NUMBER.NE.0) THEN
		 FOLDER_SET = .TRUE.
	      ELSE
		 FOLDER_SET = .FALSE.
	      END IF

	      IF (REMOTE_SET.LT.3) THEN
		 FOLDER_NAME = FOLDER
	         HEADER = .NOT.BTEST(FOLDER_FLAG,4)
	      ELSE
		 IF (COMMAND.AND.INCMD(:3).NE.'REP'.AND.
     &		     INCMD(:3).NE.'DEL') HEADER = .FALSE.
		 FOLDER_NAME = FOLDER_DESCRIP(:INDEX(FOLDER_DESCRIP,' ')-1)
	         IF (REMOTE_SET.EQ.4) NEWS_FOLDER_COM = NEWS_FOLDER1_COM
	      END IF

	      IF (REMOTE_SET.EQ.1.AND.FOLDER_NUMBER.GE.0) THEN
	         CALL OPEN_BULLFOLDER	! Update local folder information
                 CALL READ_FOLDER_FILE_KEYNAME(FOLDER1,IER1)
	         OLD_NEWEST_BTIM(1) = F_NEWEST_BTIM(1)
      	         OLD_NEWEST_BTIM(2) = F_NEWEST_BTIM(2)
	         FOLDER_COM = FOLDER1_COM
	         CALL REWRITE_FOLDER_FILE(IER1)
	         CALL CLOSE_BULLFOLDER
	         DIFF = COMPARE_BTIM(OLD_NEWEST_BTIM,F_NEWEST_BTIM)
	         IF (DIFF.LT.0.AND.IER1.EQ.0) THEN
	            CALL READ_NOTIFY
	            IF (TEST2(NOTIFY_REMOTE,FOLDER_NUMBER)) THEN
		       CALL NOTIFY_REMOTE_USERS(OLD_NEWEST_BTIM)
	            END IF
	         END IF
	      END IF

	      IF (REMOTE_SET.EQ.0.AND..NOT.BTEST(FOLDER_FLAG,10)
     &		   .AND..NOT.BTEST(FOLDER_FLAG,11).AND.WRITE_ACCESS) THEN
	         SLIST = INDEX(FOLDER_DESCRIP,'<')
                 IF (OUTPUT.AND.INCMD(:3).NE.'DIR') THEN
                    IF (NEWS_FEED()) THEN
                       WRITE (6,'('' Use the POST command to send a '',
     &                 ''message to this folder''''s news group.'')')
		    ELSE IF (SLIST.GT.0) THEN
                       WRITE (6,'('' Use the POST command to send a '',
     &                 ''message to this folder''''s mailing list.'')')
                    END IF
                 END IF
	      END IF

	      IF (OUTPUT.AND.INCMD(:3).NE.'DIR') THEN
		 WRITE (6,'('' Folder has been set to '',A)') 
     &		    FOLDER_NAME(:TRIM(FOLDER_NAME))//'.'
	      END IF

	      IF (OUTPUT) THEN
		 IF (REMOTE_SET.EQ.3) THEN
		    BULL_POINT = F_START - 1
		    FSTATUS(:1) = 
     &			FOLDER_DESCRIP(INDEX(FOLDER_DESCRIP,' ')+1:)
		    IF (STREQ(FSTATUS(:1),'X')) THEN
		       WRITE (6,'('' WARNING: The local news server has'',
     &				  '' deactivated this group.'')')
		    ELSE IF (STREQ(FSTATUS(:1),'=')) THEN
		       WRITE (6,'('' NOTE: This group is no longer'',
     &				  '' active.  It has been replaced by:'')')
		       WRITE (6,'(1X,A)') FOLDER_DESCRIP(
     &				       INDEX(FOLDER_DESCRIP,'=')+1:)
		    END IF
		 ELSE IF (REMOTE_SET.EQ.4) THEN
		    BULL_POINT = F_START - 1 
		 ELSE
		    BULL_POINT = 0	! Reset pointer to first bulletin
	         END IF
	      ELSE IF (REMOTE_SET.EQ.3.OR.REMOTE_SET.EQ.4) THEN
		 BULL_POINT = F_START - 1
	      END IF

	      IF (IER.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL).AND.USERNAME
     &		  .NE.FOLDER_OWNER) THEN
	         IF (.NOT.WRITE_ACCESS) THEN
		   IF (OUTPUT.AND.INCMD(:3).NE.'DIR'.AND.SLIST.EQ.0) THEN
		    WRITE (6,'('' Folder only accessible for reading.'')')
		   END IF
		   READ_ONLY = .TRUE.
		 ELSE
		   READ_ONLY = .FALSE.
		 END IF
	      ELSE
		 READ_ONLY = .FALSE.
	      END IF

	      IF (FOLDER_NUMBER.GT.0.AND.REMOTE_SET.LT.3) THEN
		IF (TEST_BULLCP().GT.0.OR.REMOTE_SET) THEN
		 CALL SET2(FIRST_TIME,FOLDER_NUMBER)
		ELSE IF (.NOT.TEST2(FIRST_TIME,FOLDER_NUMBER)) THEN
	       			! If first select, look for expired messages.
		 CALL OPEN_BULLDIR
		 CALL READDIR(0,IER)	! Get header info from BULLDIR.DAT
	 	 IF (IER.EQ.1) THEN		! Is header present?
	   	    IER = COMPARE_DATE(NEWEST_EXDATE,' ') ! Yes. Any expired?
		    IF (SHUTDOWN.GT.0.AND.NODE_AREA.GT.0.AND.
     &			(FOLDER_NUMBER.EQ.0.OR.BTEST(FOLDER_FLAG,2))
     &			.AND.TEST2(SHUTDOWN_FLAG,FOLDER_NUMBER)) THEN
						! Do shutdown bulletins exist?
		       SHUTDOWN = 0
		       IER1 = -1
		    ELSE
		       IF (TEST2(SHUTDOWN_FLAG,FOLDER_NUMBER)) THEN
			  CALL UPDATE_SHUTDOWN(FOLDER_NUMBER)
		       END IF
	               IER1 = 1
		    END IF
	 	    IF (IER.LE.0.OR.IER.GT.20*356.OR.IER1.LE.0) THEN
		       CALL UPDATE	! Need to update
		    END IF
		 ELSE
		    NBULL = 0
		 END IF
		 CALL CLOSE_BULLDIR
		 CALL SET2(FIRST_TIME,FOLDER_NUMBER)
	        END IF
	      END IF

	      IF (OUTPUT) THEN
		 IF (CLI$PRESENT('MARKED')) THEN
		    READ_TAG = 1 + IBSET(0,1)
		    BULL_PARAMETER = 'MARKED'
		 ELSE IF (CLI$PRESENT('SEEN')) THEN
		    READ_TAG = 1 + IBSET(0,2)
		    BULL_PARAMETER = 'SEEN'
	         ELSE IF (CLI$PRESENT('UNMARKED').OR.CLI$PRESENT
     &		       ('MARKED').EQ.%LOC(CLI$_NEGATED)) THEN
		    READ_TAG = 1 + IBSET(0,1) + IBSET(0,3)
		    BULL_PARAMETER = 'UNMARKED'
	         ELSE IF (CLI$PRESENT('UNSEEN').OR.CLI$PRESENT
     &		       ('SEEN').EQ.%LOC(CLI$_NEGATED)) THEN
		    READ_TAG = 1 + IBSET(0,2) + IBSET(0,3)
		    BULL_PARAMETER = 'UNSEEN'
		 ELSE
		    READ_TAG = IBSET(0,1) + IBSET(0,2)
		 END IF
	         IF (READ_TAG) THEN
	            IF (FOLDER_NUMBER.GE.0) THEN
		       CALL GET_FIRST_TAG(FOLDER_NUMBER,IER,BULL_POINT)
		    ELSE
		       WRITE (6,'('' ERROR: invalid qualifier'',
     &			          '' with remote folder.'')')
		       READ_TAG = IBSET(0,1) + IBSET(0,2)
		    END IF
	         END IF
	         IF (READ_TAG.AND.INCMD(:3).NE.'DIR') THEN
		    IF (IER.EQ.0) THEN
		       WRITE(6,'('' NOTE: Only '',A,'' messages'',
     &			   '' will be shown.'')')
     &			   BULL_PARAMETER(:TRIM(BULL_PARAMETER))
		    ELSE
		       WRITE(6,'('' WARNING: No '',A,
     &			   '' messages found.'')')
     &		 	   BULL_PARAMETER(:TRIM(BULL_PARAMETER))
		    END IF
	         END IF
	      END IF

	      IF (REMOTE_SET.GE.3.AND.OUTPUT.AND..NOT.READ_TAG) THEN
		 CALL NEWS_GET_NEWEST_MESSAGE(IER)
		 IF (IER.GT.0.AND.IER.LE.F_NBULL) THEN
		    BULL_POINT = IER - 1
	            WRITE(6,'('' Type READ to read new messages.'')')
		 END IF
	      ELSE IF (FOLDER_NUMBER.NE.0.AND..NOT.READ_TAG.AND.
     &		  				REMOTE_SET.LT.3) THEN
	        IF (OUTPUT.AND.INCMD(:3).NE.'DIR') THEN
	         DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &					F_NEWEST_BTIM)
	         IF (DIFF.LT.0.AND.F_NBULL.GT.0) THEN 	! If new unread messages
		  CALL FIND_NEWEST_BULL			! See if we can find it
		  IF (BULL_POINT.NE.-1) THEN
	     	    WRITE(6,'('' Type READ to read new messages.'')')
		    NEW_COUNT = F_NBULL - BULL_POINT
		    DIG = 0
		    DO WHILE (NEW_COUNT.GT.0)
		      NEW_COUNT = NEW_COUNT / 10
		      DIG = DIG + 1
		    END DO
		    WRITE(6,'('' There are '',I<DIG>,'' new messages.'')')
     &			F_NBULL - BULL_POINT	! Alert user if new bulletins
		  ELSE
		    CALL COPY2(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &			       F_NEWEST_BTIM)
		    BULL_POINT = 0
		  END IF
		 END IF
		END IF
	      END IF
	      IER = 1
	      IF (TEST_BULLCP().NE.2) CALL CHECK_CUSTOM
	      IF (BTEST(BULL_USER_CUSTOM,2)) HEADER = .TRUE.
	   ELSE IF (OUTPUT) THEN
	      WRITE (6,'('' Cannot access specified folder.'')')
	      CALL SYS_GETMSG(IER)
	   END IF
	ELSE						! Folder not found
	   IF (OUTPUT) WRITE (6,'('' ERROR: Folder does not exist.'')')
	   IER = 0
	END IF

	LAST_FOLDER_NUMBER = FOLDER_NUMBER 

	RETURN

	END





	SUBROUTINE UPDATE_FOLDER
C
C  SUBROUTINE UPDATE_FOLDER
C
C  FUNCTION: Updates folder info due to new message.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'

	IF (FOLDER_NUMBER.LT.0) RETURN

	CALL OPEN_BULLFOLDER_SHARED			! Open folder file

	CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)

	CALL SYS_BINTIM(NEWEST_DATE//' '//NEWEST_TIME,F_NEWEST_BTIM)

	F_NBULL = NBULL

	IF (FOLDER_NUMBER.EQ.0) FOLDER_FLAG = IBSET(FOLDER_FLAG,2)

	IF (.NOT.BTEST(SYSTEM,0)) THEN 	! Is non-system message?
	   F_NEWEST_NOSYS_BTIM(1) = F_NEWEST_BTIM(1) ! If so, update latest
	   F_NEWEST_NOSYS_BTIM(2) = F_NEWEST_BTIM(2) ! system time.
	END IF

	CALL REWRITE_FOLDER_FILE(IER)

	CALL CLOSE_BULLFOLDER

	RETURN
	END



	SUBROUTINE SHOW_FOLDER
C
C  SUBROUTINE SHOW_FOLDER
C
C  FUNCTION: Shows the information on any folder.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLDIR.INC'

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*256 INCMD

	COMMON /BULL_PERM/ SET_PERM_FLAG,BRIEF_PERM_FLAG,NOTIFY_PERM_FLAG
	DIMENSION SET_PERM_FLAG(FLONG)
	DIMENSION BRIEF_PERM_FLAG(FLONG)
	DIMENSION NOTIFY_PERM_FLAG(FLONG)

	INCLUDE '($SSDEF)'

	INCLUDE '($RMSDEF)'

	EXTERNAL CLI$_ABSENT,BULLETIN_SUBCOMMANDS

	IF (INDEX(INCMD,'/A').GT.0.OR.INDEX(INCMD,'/a').GT.0) THEN
	   WRITE (6,'('' ERROR: /ALL is invalid qualifier.'')')
	   RETURN
	END IF

	IF (CLI$GET_VALUE('SHOW_FOLDER',FOLDER1).EQ.%LOC(CLI$_ABSENT))
     &	   THEN
	   FOLDER1 = FOLDER
	   IF (INDEX(FOLDER1,'.').GT.0) CALL LOWERCASE(FOLDER1)
	END IF

	IF (INDEX(FOLDER1,'::').NE.0) THEN
 	   WRITE (6,'('' ERROR: invalid command for remote folder.'')')
	   RETURN
	END IF

	IF (TEST_NEWS(FOLDER1)) THEN              
	   INCMD = 'SET NEWS '
	   IF (CLI$PRESENT('FULL')) INCMD = 'SET NEWS/FULL '
	   IF (CLI$PRESENT('SHOW_FOLDER')) INCMD = 'SET NEWS '//FOLDER1
           CALL CLI$DCL_PARSE(INCMD,BULLETIN_SUBCOMMANDS)
	   CALL SHOW_NEWS
	   RETURN
        END IF

	CALL OPEN_BULLFOLDER_SHARED

	CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER)

	IF (IER.NE.0) THEN
	   WRITE (6,'('' ERROR: Specified folder was not found.'')')
	   CALL CLOSE_BULLFOLDER
	   RETURN
	ELSE IF (FOLDER.EQ.FOLDER1) THEN
	   WRITE (6,1000) FOLDER1,FOLDER1_OWNER,
     &			FOLDER1_DESCRIP(:TRIM(FOLDER1_DESCRIP))
	ELSE
	   WRITE (6,1010) FOLDER1,FOLDER1_OWNER,
     &			FOLDER1_DESCRIP(:TRIM(FOLDER1_DESCRIP))
	END IF

	IF (CLI$PRESENT('FULL')) THEN
	   CALL SET_FOLDER_FILE(1)
	   CALL CHKACL
     &		 (FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL',IER)
	   IF (IER.EQ.(SS$_ACLEMPTY.OR.SS$_NORMAL).OR.(.NOT.IER)) THEN
	      IF (FOLDER1_BBOARD(:2).EQ.'::'.AND.	! Is folder remote
     &		BTEST(FOLDER1_FLAG,0)) THEN		! and private?
	         WRITE (6,'('' Access is limited.'')')
	      END IF
	   ELSE
	      IF (SETPRV_PRIV()) THEN
	         READ_ACCESS = 1
		 WRITE_ACCESS = 1
	      ELSE
	        CALL CHECK_ACCESS
     &		  (FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL',
     &		   USERNAME,READ_ACCESS,WRITE_ACCESS)
	      END IF
	      IF (WRITE_ACCESS)
     &	      CALL SHOWACL(FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL')
	   END IF
	   IF (FOLDER_ACCESS(USERNAME,FOLDER1_FLAG,FOLDER1_OWNER)) THEN
	      IF (FOLDER1_BBOARD(:2).EQ.'::') THEN
		 FLEN = TRIM(FOLDER1_BBOARD)
		 IF (INDEX(FOLDER1_BBOARD,'*').EQ.0) THEN
		    WRITE (6,'('' Folder is located on node '',
     &		     A,''.'')') FOLDER1_BBOARD(3:FLEN)
		 ELSE
		    CALL SET_FOLDER_FILE(1)
		    FOLDER_FILE = FOLDER1_FILE
		    REMOTE_SET_SAVE = REMOTE_SET
		    REMOTE_SET = .FALSE.
		    CALL OPEN_BULLDIR
		    CALL READDIR(0,IER)
		    CALL CLOSE_BULLDIR
		    CALL SET_FOLDER_FILE(0)
		    REMOTE_SET = REMOTE_SET_SAVE
		    WRITE (6,'('' Folder is located on node '',
     &		       A,''. Remote folder name is '',A,''.'')') 
     &		       FOLDER1_BBOARD(3:FLEN-1),
     &		       BULLDIR_HEADER(13:TRIM(BULLDIR_HEADER))
		 END IF
	      ELSE IF (FOLDER1_BBOARD(:4).NE.'NONE') THEN
		 FLEN = TRIM(FOLDER1_BBOARD)
		 IF (FLEN.GT.0) THEN
 	          WRITE (6,'('' BBOARD for folder is '',A<FLEN>,''.'')')
     &		 	FOLDER1_BBOARD(:FLEN)
		 END IF
		 IF ((USERB1.EQ.0.AND.GROUPB1.EQ.0).OR.BTEST(USERB1,31)) THEN
 		  WRITE (6,'('' BBOARD was specified with /SPECIAL.'')')
		  IF (BTEST(GROUPB1,31)) THEN
		   WRITE (6,'('' BBOARD was specified with /VMSMAIL.'')')
		  END IF
		 END IF
	      ELSE
	         WRITE (6,'('' No BBOARD has been defined.'')')
	      END IF
	      IF (FOLDER1_BBEXPIRE.GT.0) THEN
		 WRITE (6,'('' Default expiration is '',I3,'' days.'')')
     &			FOLDER1_BBEXPIRE
	      ELSE IF (FOLDER1_BBEXPIRE.EQ.-1) THEN
		 WRITE (6,'('' Default expiration is permanent.'')')
	      ELSE
		 WRITE (6,'('' No default expiration set.'')')
	      END IF
	      IF (BTEST(FOLDER1_FLAG,2)) THEN
		 WRITE (6,'('' SYSTEM has been set.'')')
	      END IF
	      IF (BTEST(FOLDER1_FLAG,1)) THEN
		 WRITE (6,'('' DUMP has been set.'')')
	      END IF
	      IF (BTEST(FOLDER1_FLAG,3)) THEN
		 WRITE (6,'('' NOPROMPT_EXPIRE has been set.'')')
	      END IF
	      IF (BTEST(FOLDER1_FLAG,4)) THEN
		 WRITE (6,'('' STRIP has been set.'')')
	      END IF
	      IF (BTEST(FOLDER1_FLAG,5)) THEN
		 WRITE (6,'('' DIGEST has been set.'')')
	      END IF
	      IF (BTEST(FOLDER1_FLAG,7)) THEN
		 WRITE (6,'('' ALWAYS has been set.'')')
	      END IF
	      IF (BTEST(FOLDER1_FLAG,10)) THEN
		 WRITE (6,'('' POST_ONLY has been set.'')')
	      ELSE IF (BTEST(FOLDER1_FLAG,11)) THEN
		 WRITE (6,'('' ADD_ONLY has been set.'')')
	      END IF
	      IF (BTEST(FOLDER1_FLAG,12)) THEN
		 WRITE (6,'('' COMPRESS has been set.'')')
	      END IF
	      IF (BTEST(FOLDER1_FLAG,14)) THEN
		 WRITE (6,'('' ANONYMOUS has been set.'')')
	      END IF
	      IF (BTEST(FOLDER1_FLAG,15)) THEN
		 WRITE (6,'('' GATEWAY has been set.'')')
	      END IF
	      IF (F1_EXPIRE_LIMIT.GT.0) THEN
		 WRITE (6,'('' EXPIRATION limit is '',I3,'' days.'')')
     &			F1_EXPIRE_LIMIT
	      END IF
	      CALL OPEN_BULLUSER_SHARED
	      CALL READ_USER_FILE_HEADER(IER)
	      CALL READ_PERM
	      PERM = .FALSE.
	      IF (TEST2(SET_FLAG_DEF,FOLDER1_NUMBER)) THEN
	       IF (TEST2(BRIEF_FLAG_DEF,FOLDER1_NUMBER)) THEN
	        IF (TEST2(BRIEF_PERM_FLAG,FOLDER1_NUMBER).AND.
     &		    TEST2(SET_PERM_FLAG,FOLDER1_NUMBER)) THEN
	         PERM = .TRUE.
		 WRITE (6,'('' Default is BRIEF, which is permanent.'')')
		ELSE
		 WRITE (6,'('' Default is BRIEF.'')')
		END IF
	       ELSE
	        IF (TEST2(SET_PERM_FLAG,FOLDER1_NUMBER).AND.
     &		    .NOT.TEST2(BRIEF_PERM_FLAG,FOLDER1_NUMBER)) THEN
	         PERM = .TRUE.
		 WRITE (6,'('' Default is READNEW, which is permanent.'')')
		ELSE
		 WRITE (6,'('' Default is READNEW.'')')
		END IF
	       END IF
	      ELSE
	       IF (TEST2(BRIEF_FLAG_DEF,FOLDER1_NUMBER)) THEN
	        IF (TEST2(BRIEF_PERM_FLAG,FOLDER1_NUMBER).AND.
     &		    .NOT.TEST2(SET_PERM_FLAG,FOLDER1_NUMBER)) THEN
	         PERM = .TRUE.
		 WRITE (6,'('' Default is SHOWNEW, which is permanent.'')')
		ELSE
		 WRITE (6,'('' Default is SHOWNEW.'')')
		END IF
	       END IF
	      END IF
	      IF (.NOT.PERM) THEN
	        IF (TEST2(BRIEF_PERM_FLAG,FOLDER1_NUMBER).AND.
     &		    TEST2(SET_PERM_FLAG,FOLDER1_NUMBER)) THEN
		   WRITE (6,'('' BRIEF is the permanent setting.'')')
	        ELSE IF (TEST2(SET_PERM_FLAG,FOLDER1_NUMBER).AND.
     &		    .NOT.TEST2(BRIEF_PERM_FLAG,FOLDER1_NUMBER)) THEN
		   WRITE (6,'('' READNEW is the permanent setting.'')')
	        ELSE IF (TEST2(BRIEF_PERM_FLAG,FOLDER1_NUMBER).AND.
     &		    .NOT.TEST2(SET_PERM_FLAG,FOLDER1_NUMBER)) THEN
		   WRITE (6,'('' SHOWNEW is the permanent setting.'')')
	        END IF
	      END IF
	      IF (TEST2(NOTIFY_FLAG_DEF,FOLDER1_NUMBER)) THEN
	        IF (TEST2(NOTIFY_PERM_FLAG,FOLDER1_NUMBER)) THEN
		 WRITE (6,'('' Default is NOTIFY, which is permanent.'')')
		ELSE
		 WRITE (6,'('' Default is NOTIFY.'')')
		END IF
	      ELSE
		 WRITE (6,'('' Default is NONOTIFY.'')')
	      END IF
	      CALL CLOSE_BULLUSER
	   END IF
	   IF (NEWS_FEED()) THEN
	      WRITE (6,'('' Last message fed by news group was: '',I)') F_LAST
	   END IF
	END IF

	CALL CLOSE_BULLFOLDER

	RETURN

1000	FORMAT(' Current folder: ',A44,' Owner: ',A12,/,
     &		' Description: ',A)
1010	FORMAT(' Folder name is: ',A44,' Owner: ',A12,/,
     &		' Description: ',A)
	END


	SUBROUTINE DIRECTORY_FOLDERS(FOLDER_COUNT)
C
C  SUBROUTINE DIRECTORY_FOLDERS
C
C  FUNCTION: Display all FOLDER entries.
C
	IMPLICIT INTEGER (A - Z)

	INCLUDE '($SSDEF)'

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLDIR.INC'

	COMMON /PAGE/ PAGE_LENGTH,PAGE_WIDTH,PAGING
	LOGICAL PAGING

	COMMON /CTRLC_FLAG/ FLAG

	COMMON /LAST_BUFFER/ OLD_BUFFER
	CHARACTER*(INPUT_LENGTH) OLD_BUFFER

	DATA SCRATCH_D1/0/

	CHARACTER FOLDER_MATCH*80,DATETIME*20,FSTATUS1*4,NEWS_ACCESS*132 

	INTEGER*2 MLEN,FLEN

	OLD_BUFFER = ' '

	IF (CLI$PRESENT('NEWS')) THEN
	   IF (SYS_TRNLNM('BULL_NEWS_SERVER','DEFINED')) THEN
	      CALL OPEN_BULLNEWS_SHARED
	   ELSE
	      WRITE (6,'('' ERROR: NEWS connection is not present.'')')
	      RETURN
	   END IF
	ELSE
	   CALL OPEN_BULLFOLDER_SHARED		! Get folder file
	END IF

	IF (FOLDER_COUNT.EQ.0) THEN
	   SUBSCRIBE = .FALSE.
	   ACTIVE = .FALSE. 
	   STORED = .FALSE. 
	   CLASS = .FALSE.
	   NEW = .FALSE.
	   PERM = .FALSE.
	   DEFA = .FALSE.
	   FOLDER_COUNT = 1			! Init folder number counter
	   NLINE = 1
	   START = .FALSE.
	   IF (.NOT.CLI$PRESENT('NEWS')) THEN
	      NEWS = .FALSE.
	      IF (CLI$PRESENT('DESCRIBE')) THEN
	         NLINE = 2	! Include folder descriptor if /DESCRIBE
	      END IF
	   ELSE
	      NEWS = .TRUE.
	      CALL READ_FOLDER_FILE_KEYNAME_TEMP('a',IER)
	      IF (IER.NE.0) THEN
		 WRITE (6,'('' Fetching NEWS groups from remote node.''
     &			,''  This will take several minutes.'')')
		 WRITE (6,'('' This is the only time this will have''
     &			,'' to be done.'')')
	         CALL CLOSE_BULLFOLDER
		 CALL NEWS_LIST
	         CALL OPEN_BULLNEWS_SHARED
	         CALL READ_FOLDER_FILE_KEYNAME_TEMP('a',IER)
	      END IF
	      COUNT = CLI$PRESENT('COUNT')
	      IF (COUNT) TOTAL_COUNT = 0
	      STORED = CLI$PRESENT('STORED')
	      SUBSCRIBE = CLI$PRESENT('SUBSCRIBE')
	      NEW = CLI$PRESENT('NEWGROUPS')
	      CLASS = CLI$PRESENT('CLASS')
	      PERM = CLI$PRESENT('PERMANENT')
	      DEFA =  CLI$PRESENT('DEFAULT')
	      IF (CLASS) THEN
	         CALL CLOSE_BULLFOLDER
	         CALL OPEN_BULLNEWS_SHARED
	      END IF
	      IF (NEW) THEN
		 NEW_NEWS = MAX(LAST_NEWS_READ(1,FOLDER_MAX),1000)
	      ELSE IF (SUBSCRIBE) THEN
		 CALL NEWS_GET_SUBSCRIBE(0,F1_COUNT)
		 SUBNUM = 1
	      ELSE IF (PERM) THEN
	      	 CALL OPEN_BULLINF_SHARED
	         DO WHILE (REC_LOCK(IER))
	            READ (9,KEY='*PERM',IOSTAT=IER) TEMP_USER,INF_REC
		 END DO
		 IF (IER.NE.0) THEN
	            DO I=1,FOLDER_MAX
	               INF_REC(1,I) = 0
	               INF_REC(2,I) = 0
	            END DO
	         END IF
		 CALL CLOSE_BULLINF
		 INUM = 1
	      ELSE IF (DEFA) THEN
	      	 CALL OPEN_BULLINF_SHARED
	         DO WHILE (REC_LOCK(IER))
	            READ (9,KEY='*DEFAULT',IOSTAT=IER) TEMP_USER,INF_REC
		 END DO
		 IF (IER.NE.0) THEN
	            DO I=1,FOLDER_MAX
	               INF_REC(1,I) = 0
	               INF_REC(2,I) = 0
	            END DO
	         END IF
		 CALL CLOSE_BULLINF
		 INUM = 1
	      ELSE
	         ACTIVE = .NOT.CLI$PRESENT('ALL')
	      END IF
	   END IF
	   IF (CLI$GET_VALUE('START',FOLDER1,FLEN)) THEN
	      IF (NEWS) CALL LOWERCASE(FOLDER1)
	      CALL READ_FOLDER_FILE_KEYNAMEGE_TEMP(FOLDER1(:FLEN),IER)
	      IF (IER.NE.0) THEN
		 WRITE (6,'('' There are no folders.'')')
	         CALL CLOSE_BULLFOLDER
		 FOLDER_COUNT = -1
		 RETURN
	      ELSE
		 START = .TRUE.
	      END IF
	   END IF
	   MATCH = CLI$GET_VALUE('MATCH_FOLDER',FOLDER_MATCH,MLEN)
	   IF (MATCH.AND.NEWS) CALL LOWERCASE(FOLDER_MATCH)
	   IF (MATCH.AND.INDEX(FOLDER_MATCH,'*').EQ.0) THEN
	      FOLDER_MATCH = '*'//FOLDER_MATCH(:MLEN)//'*'
	      MLEN = MLEN + 2
	   END IF
	ELSE IF (NEWS.AND.COUNT.AND.TOTAL_COUNT.LT.0) THEN
	   WRITE (6,'('' The total count is: '',I)') -TOTAL_COUNT
	   TOTAL_COUNT = 0
	   FOLDER_COUNT = -1
	   RETURN
	ELSE IF (SUBSCRIBE.AND.PAGING.AND.MORE) THEN
	   SUBNUM = -2
	ELSE
	   CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER)
	END IF

C
C  Folder listing is first buffered into temporary memory storage before
C  being outputted to the terminal.  This is to be able to quickly close the
C  folder file, and to avoid the possibility of the user holding the screen,
C  and thus causing the folder file to stay open.  The temporary memory
C  is structured as a linked-list queue, where SCRATCH_D1 points to the header
C  of the queue.
C
	CALL INIT_QUEUE(SCRATCH_D1,FOLDER1_COM)
	SCRATCH_D = SCRATCH_D1

	CALL DECLARE_CTRLC_AST

	NUM_FOLDER = 0
	IER = 0
	IER1 = 0
	MORE = .FALSE.
	NEWS_TEST = MATCH.OR.ACTIVE.OR.STORED
	DO WHILE (IER.EQ.0.AND.IER1.EQ.0)
	   IF (SUBSCRIBE) THEN
	      IER = 1
	      DO WHILE (SUBNUM.NE.0.AND.IER.NE.0)
	         CALL NEWS_GET_SUBSCRIBE(SUBNUM,MSGNUM)
		 IF (SUBNUM.NE.0) THEN
	            CALL READ_FOLDER_FILE_KEYNUM_TEMP(SUBNUM,IER)
		    IF (IER.NE.0) SUBNUM = -1
		 END IF
	      END DO
	      IF (SUBNUM.EQ.0) IER = 1
	   ELSE IF (PERM.OR.DEFA) THEN
	      IER = 1
	      DO WHILE (INUM.LE.FOLDER_MAX.AND.IER.NE.0)
		 IF (INF_REC2(1,INUM).NE.0) THEN
	            CALL READ_FOLDER_FILE_KEYNUM_TEMP
     &				(ZEXT(INF_REC2(1,INUM)),IER)
		 END IF
		 INUM = INUM + 1
	      END DO
	   ELSE IF (START) THEN
	      START = .FALSE.
	   ELSE IF (NEW) THEN
	      IER = 2
	      DO WHILE (IER.EQ.2)
	         CALL READ_FOLDER_FILE_KEYNUM_GT_TEMP(NEW_NEWS,IER)
		 IF (IER.EQ.0.AND.BTEST(FOLDER1_FLAG,10)) THEN
		    IER = 2
	            NEW_NEWS = FOLDER1_NUMBER
	         END IF
	      END DO
	      IF (IER.EQ.0) THEN
	         NEW_NEWS = FOLDER1_NUMBER
	      ELSE
	         CALL READ_FOLDER_FILE_KEYNAME_TEMP('a',IER2)
	         NEW_NEWS = NEWS_F1_COUNT
	      END IF
	   ELSE
	      CALL READ_FOLDER_FILE_TEMP(IER)
	      IF (CLASS) CALL LOWERCASE(FOLDER1_DESCRIP)
	      IF (CLASS) NEWS_TEST = .FALSE.
	      IF (CLASS.AND.FOLDER1.EQ.'a') IER = 2
	   END IF
	   IF (IER.EQ.0) THEN
	      IF ((INDEX(FOLDER1_BBOARD,'::').EQ.0.OR.NEWS).AND.
     &		  BTEST(FOLDER1_FLAG,0).AND..NOT.SETPRV_PRIV()) THEN
		 CALL SET_FOLDER_FILE(1)
	         IF (NEWS) THEN
	            IF (OLD_BUFFER.NE.NEWS_ACCESS(FOLDER1_DESCRIP)) THEN
	               OLD_BUFFER = NEWS_ACCESS(FOLDER1_DESCRIP)
		       CALL CHKACL(OLD_BUFFER(:TRIM(OLD_BUFFER)),IER2)
	   	       IF (IER2.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL)) THEN
	                  CALL CHECK_ACCESS(OLD_BUFFER(:TRIM(OLD_BUFFER)),
     &		           USERNAME,READ_ACCESS,-1)
                       ELSE
                          READ_ACCESS = 1
                       END IF
	            END IF
	         ELSE
	            CALL CHECK_ACCESS
     &		     (FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL',
     &		      USERNAME,READ_ACCESS,-1)
	         END IF
	      ELSE
		 READ_ACCESS = 1
	      END IF
	      IF (READ_ACCESS) THEN
	         J = INDEX(FOLDER1_DESCRIP,' ')
	         IF (J.GT.0) THEN
	            FSTATUS1(:1) = FOLDER1_DESCRIP(J+1:)
		 ELSE
	            FSTATUS1 = ' '
		    J = TRIM(FOLDER1_DESCRIP) + 1
	         END IF
		 IF (.NOT.NEWS_TEST) THEN
		    IF (NEWS.AND.CLASS) 
     &			FOLDER1_DESCRIP = FOLDER1_DESCRIP(:J-2)
	            NUM_FOLDER = NUM_FOLDER + 1
	            CALL WRITE_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,FOLDER1_COM)
		 ELSE IF ((.NOT.ACTIVE.OR.(FSTATUS1(:1).NE.'x'.AND..NOT.
     &			   BTEST(FOLDER1_FLAG,9))).AND.
     &	                  (.NOT.STORED.OR.BTEST(FOLDER1_FLAG,8)).AND.
     &			(.NOT.MATCH.OR.STR$MATCH_WILD(FOLDER1_DESCRIP
     &		        (:J-1),FOLDER_MATCH(:MLEN)))) THEN
		    GO TO 100
	         END IF
	      END IF
	      IF (PAGING.AND.NUM_FOLDER*NLINE+2.GT.PAGE_LENGTH-4) THEN
		 IER1 = 1
		 MORE = .TRUE.
	      END IF
	   END IF
	   IF (FLAG.EQ.1) IER1 = 1
	END DO

 	IF (NEWS_TEST) NEWS_TEST = .FALSE.

 	IF (FLAG.EQ.1) THEN
	   WRITE (6,'('' Listing aborted.'')')
	   FOLDER_COUNT = -1
	   CALL CANCEL_CTRLC_AST
	   CALL CLOSE_BULLFOLDER
	   RETURN
	END IF

	CALL CANCEL_CTRLC_AST
	CALL CLOSE_BULLFOLDER			! We don't need file anymore

	IF (NUM_FOLDER.EQ.0) THEN
	   WRITE (6,'('' There are no folders.'')')
	   FOLDER_COUNT = -1
	   IF (NEW) LAST_NEWS_READ(1,FOLDER_MAX) = NEW_NEWS
	   RETURN
	END IF

C
C  Folder entries are now in queue.  Output queue entries to screen.
C

     	SCRATCH_D = SCRATCH_D1			! Init queue pointer to header

100	CALL LIB$ERASE_PAGE(1,1)		! Clear the screen

	IF (.NOT.NEWS) THEN
	   WRITE (6,'(1X,''Folder'',22X,''Last message'',7X,''Messages'',
     &		2X,''Owner'',/,1X,80(''-''))')
	ELSE IF (COUNT) THEN
	   WRITE (6,'(1X,''News group'',<PAGE_WIDTH-80+39>X,6X,
     &		''First        Last  Count'',
     &		/,1X,<PAGE_WIDTH>(''-''))')
	ELSE IF (CLASS) THEN
	   WRITE (6,'(1X,''Class'',/,1X,<PAGE_WIDTH>(''-''))')
	ELSE IF (SUBSCRIBE) THEN
	   WRITE (6,'(1X,''News group'',<PAGE_WIDTH-80+39>X,1X,
     &		''First        Last   Last Read'',/,1X,<PAGE_WIDTH>(''-''))')
	ELSE
	   WRITE (6,'(1X,''News group'',<PAGE_WIDTH-80+39>X,''Status'',7X,
     &		''First        Last'',/,1X,<PAGE_WIDTH>(''-''))')
	END IF

	IF (PAGING.AND.MORE) NUM_FOLDER = NUM_FOLDER - 1

	I = 1
	DO WHILE ((I.LE.NUM_FOLDER.OR.NEWS_TEST).AND.FLAG.NE.1.AND.
     &		  FLAG.NE.100)
	   IF (.NOT.NEWS_TEST) THEN
	      CALL READ_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,FOLDER1_COM)
	      I = I + 1
	   END IF
	   IF (.NOT.NEWS) THEN
	      DIFF = COMPARE_BTIM
     &			(LAST_READ_BTIM(1,FOLDER1_NUMBER+1),F1_NEWEST_BTIM)
	      IF (F1_NBULL.GT.0) THEN
	         CALL SYS$ASCTIM(,DATETIME,F1_NEWEST_BTIM,)
	      ELSE
	         DATETIME = '      NONE'
	      END IF
	      IF (DIFF.GE.0.OR.F1_NBULL.EQ.0) THEN
	         WRITE (6,1000) ' '//FOLDER1,DATETIME(:17),F1_NBULL,
     &							FOLDER1_OWNER
	      ELSE
	         WRITE (6,1000) '*'//FOLDER1,DATETIME(:17),F1_NBULL,
     &							FOLDER1_OWNER
	      END IF
	   ELSE
	      IF (NEWS_TEST) UNLOCK 7
	      FLEN = MIN(80,PAGE_WIDTH-80+49)
	      IF (SUBSCRIBE) FLEN = MIN(81,PAGE_WIDTH-80+42)
	      J = INDEX(FOLDER1_DESCRIP,' ')
	      IF (J.GT.0) THEN
	         FSTATUS1(:1) = FOLDER1_DESCRIP(J+1:)
		 IF (FSTATUS1(:1).NE.'=') THEN
                    FOLDER1_DESCRIP = FOLDER1_DESCRIP(:J-1)
                 END IF
	      ELSE
	         FSTATUS1 = ' '
	      END IF
	      IF (BTEST(FOLDER1_FLAG,9)) FSTATUS1 = 'n'
	      IF (COUNT) THEN
		 TOTAL_COUNT = TOTAL_COUNT + F1_COUNT
	         IF (F1_START.LE.F1_NBULL) THEN
		    WRITE (6,1010) FOLDER1_DESCRIP(:FLEN),
     &					 F1_START,F1_NBULL,F1_COUNT
	         ELSE
		    WRITE (6,1010) FOLDER1_DESCRIP(:FLEN),0,0,0
		 END IF
              ELSE IF (CLASS) THEN
                 WRITE (6,1010) FOLDER1_DESCRIP(:FLEN)
	      ELSE IF (F1_START.LE.F1_NBULL) THEN
		 IF (SUBSCRIBE) THEN
                    NEWS_FOLDER1_NUMBER = FOLDER1_NUMBER
		    CALL NEWS_GET_NEWEST_MESSAGE1(NEWS_NEW)
		    IF (NEWS_NEW-1.LT.F1_NBULL.AND.F1_NBULL.GT.0) THEN
	               WRITE (6,1015) '* '//FOLDER1_DESCRIP(:FLEN-2),
     &					F1_START,F1_NBULL,NEWS_NEW-1
		    ELSE
	               WRITE (6,1015) '  '//FOLDER1_DESCRIP(:FLEN-2),
     &					F1_START,F1_NBULL,NEWS_NEW-1
		    END IF
		 ELSE
		    WRITE (6,1005) FOLDER1_DESCRIP(:FLEN),
     &					 FSTATUS1(:1),F1_START,F1_NBULL
		 END IF
	      ELSE IF (SUBSCRIBE) THEN
	         WRITE (6,1015) ' '//FOLDER1_DESCRIP(:FLEN-1),0,0,0
	      ELSE
		 WRITE (6,1005) FOLDER1_DESCRIP(:FLEN),FSTATUS1(:1),0,0
	      END IF
	   END IF
	   IF (NLINE.EQ.2) WRITE (6,'(1X,A)') FOLDER1_DESCRIP
	   LAST_DISPLAY = FOLDER1_NUMBER
	   IF (NEWS_TEST.AND.FLAG.NE.1) THEN
	      NUM_FOLDER = NUM_FOLDER + 1
	      IF (PAGING.AND.
     &		  NUM_FOLDER*NLINE+2.GE.PAGE_LENGTH-4) MORE = .TRUE.
	      CALL GET_NEXT_GROUP(MATCH,FOLDER_MATCH,MLEN,FOUND,STORED,
     &				  FSTATUS1,IER,ACTIVE)
	      MORE = MORE.AND.FOUND
	      IF (MORE) THEN
		 CALL READ_FOLDER_FILE_KEYNUM_TEMP(LAST_DISPLAY,IER)
              END IF
	      FOUND = FOUND.AND..NOT.MORE
	      IF (.NOT.FOUND.AND.FLAG.NE.1) FLAG = 100
	   END IF
	END DO

 	IF (FLAG.EQ.1) THEN
	   WRITE (6,'('' Listing aborted.'')')
	   FOLDER_COUNT = -1
	   CALL CANCEL_CTRLC_AST
	   CALL CLOSE_BULLFOLDER
	   RETURN
	END IF

	IF (NEWS_TEST) THEN
	   CALL CANCEL_CTRLC_AST
	   CALL CLOSE_BULLFOLDER
	END IF

	IF (IER.NE.0.AND..NOT.MORE) THEN	! Outputted all entries?
	   IF (NEWS.AND.COUNT) THEN
	      TOTAL_COUNT = -TOTAL_COUNT
	   ELSE
	      FOLDER_COUNT = -1			! Yes. Set counter to -1.
	   END IF
	   IF (NEW) LAST_NEWS_READ(1,FOLDER_MAX) = NEW_NEWS
	ELSE
	   WRITE(6,1100)			! Else say there are more
	   IF (NEW) LAST_NEWS_READ(1,FOLDER_MAX) = FOLDER1_NUMBER
	END IF

	RETURN

1000	FORMAT(1X,A26,2X,A17,2X,I8,2X,A12)
1005	FORMAT(1X,A<FLEN>,<PAGE_WIDTH-FLEN-29-1>X,2X,A1,4X,I10,'  ',I10)
1010	FORMAT(1X,A<FLEN>,<PAGE_WIDTH-FLEN-29-1>X,I10,2X,I10,1X,I6)
1015    FORMAT(1X,A<FLEN>,<PAGE_WIDTH-FLEN-36-1>X,2X,I10,2X,I10,2X,I10)
1100	FORMAT(1X,/,' Press RETURN for more...',/)

	END


	SUBROUTINE SET_ACCESS(ACCESS)
C
C  SUBROUTINE SET_ACCESS
C
C  FUNCTION: Set access on folder for specified ID.
C
C  PARAMETERS:
C	ACCESS  -  Logical: If .true., grant access, if .false. deny access
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE '($SSDEF)'

	INCLUDE '($RMSDEF)'

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	LOGICAL ACCESS,ALL,READONLY

	EXTERNAL CLI$_ABSENT

	CHARACTER ID*64,RESPONSE*4,NEW_NEWS_ACCESS*132

	CHARACTER INPUT*132

	IF (CLI$PRESENT('ALL')) THEN
	   ALL = .TRUE.
	ELSE
	   ALL = .FALSE.
	END IF

	IF (CLI$PRESENT('READONLY')) THEN
	   READONLY = .TRUE.
	ELSE
	   READONLY = .FALSE.
	END IF

	IF (ALL) THEN
	   IER = CLI$GET_VALUE('ACCESS_ID',FOLDER1,LEN) ! Get folder name
	ELSE
	   IER = CLI$GET_VALUE('ACCESS_FOLDER',FOLDER1,LEN) ! Get folder name
	END IF

	IF (IER.EQ.%LOC(CLI$_ABSENT)) FOLDER1 = FOLDER
	NEWS = INDEX(FOLDER1,'.').GT.0

	IF (NEWS.OR.CLI$PRESENT('CLASS')) THEN
	   IF (.NOT.CLI$PRESENT('CLASS')) THEN
	      CALL LOWERCASE(FOLDER1)
	   ELSE IF (FOLDER1(TRIM(FOLDER1):TRIM(FOLDER1)).NE.'.') THEN
	      FOLDER1 = FOLDER1(:TRIM(FOLDER1))//'.'
	   END IF
	   CALL OPEN_BULLNEWS
	ELSE
	   CALL OPEN_BULLFOLDER		! Open folder file
	END IF
	CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER)	! See if it exists
	OLD_FOLDER1_FLAG = FOLDER1_FLAG
	CALL CLOSE_BULLFOLDER

	IF (IER.NE.0) THEN
	   WRITE (6,'('' ERROR: No such folder exists.'')')
	ELSE IF (.NOT.FOLDER_ACCESS(USERNAME,FOLDER1_FLAG,FOLDER1_OWNER)) THEN
	   WRITE (6,
     &	'('' ERROR: You are not able to modify access to the folder.'')')
	ELSE IF (CLI$PRESENT('CLASS').AND..NOT.BTEST(FOLDER1_FLAG,0)) THEN
	   WRITE (6,'('' ERROR: Must use SET NEWS/CLASS/PRIVATE.'')')
	ELSE
	   CALL SET_FOLDER_FILE(1)
	   IF (NEWS) THEN
	      CALL CHKACL(NEW_NEWS_ACCESS(FOLDER1_DESCRIP),IER)
	   ELSE
	      CALL CHKACL
     &		 (FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL',IER)
	   END IF
	   IF (NEWS.AND.IER.EQ.RMS$_FNF) THEN
	     CALL SET_PROTECTION
	     OPEN (UNIT=3,FILE=NEW_NEWS_ACCESS(FOLDER1_DESCRIP),
     &			STATUS='NEW',IOSTAT=IER)
	     CLOSE (UNIT=3)
	     CALL RESET_PROTECTION
	     IF (IER.NE.0) THEN
	        WRITE (6,'('' ERROR: Access file cannot be created.'')')
	        RETURN
	     END IF
	     CALL ADD_ACL('*','NONE',IER)
	     IF (.NOT.IER) THEN
		WRITE(6,'('' Cannot modify access.'')')
		CALL SYS_GETMSG(IER)
		RETURN
	     END IF
	   ELSE IF (IER.EQ.RMS$_FNF) THEN
	     FOLDER_FILE = FOLDER1_FILE
	     REMOTE_SET_SAVE = REMOTE_SET
	     REMOTE_SET = .FALSE.
	     CALL OPEN_BULLFIL
	     CALL CLOSE_BULLFIL
	     REMOTE_SET = REMOTE_SET_SAVE
	     IER = SS$_ACLEMPTY.OR.SS$_NORMAL
	     CALL SET_FOLDER_FILE(0)
	   END IF
	   IF (IER.EQ.(SS$_ACLEMPTY.OR.SS$_NORMAL)) THEN
	     IF (.NOT.NEWS.AND.
     &		 ((ALL.AND..NOT.READONLY).OR.(.NOT.ACCESS))) THEN
	        WRITE (6,'('' ERROR: Folder is not a private folder.'')')
		RETURN
	     END IF
	     CALL GET_INPUT_PROMPT(RESPONSE,LEN,'Access is presently'
     &		//' unlimited. Do you want to change this? (Y/N): ')
	     IF (RESPONSE(:1).NE.'y'.AND.RESPONSE(:1).NE.'Y') THEN
	       WRITE (6,'('' Access was not changed.'')')
	       RETURN
	     ELSE
	       FOLDER1_FLAG = IBSET(FOLDER1_FLAG,0)
	       IF (READONLY.AND.ALL) THEN
	          CALL ADD_ACL('*','R',IER)
	       ELSE IF (.NOT.ALL) THEN
	          CALL ADD_ACL('*','NONE',IER)
	       END IF
	       IF (.NOT.NEWS) CALL ADD_ACL(FOLDER1_OWNER,'R+W+C',IER)
	       IF (ALL) THEN		! All finished, so exit
	        WRITE (6,'('' Access to folder has been modified.'')')
		GOTO 100
	       END IF
	     END IF
	   END IF

	   IF (ALL) THEN
	      IF (ACCESS) THEN
		 CALL DEL_ACL(' ','R+W',IER)
	         IF (READONLY) THEN
	            CALL ADD_ACL('*','R',IER)
	         ELSE IF (.NOT.NEWS) THEN
		    FOLDER1_FLAG = IBCLR(FOLDER1_FLAG,0)
	   	    IF (.NOT.NEWS.AND.REMOTE_SET) THEN
		       CALL SET_FOLDER_FILE(1)
		       FOLDER_FILE = FOLDER1_FILE
		       REMOTE_SET_SAVE = REMOTE_SET
		       REMOTE_SET = .FALSE.
		       CALL OPEN_BULLDIR
		       CALL OPEN_BULLFIL
		       CALL CLOSE_BULLFIL_DELETE
		       CALL CLOSE_BULLDIR_DELETE
		       REMOTE_SET = REMOTE_SET_SAVE
		       CALL SET_FOLDER_FILE(0)
		    END IF
		 END IF
	      ELSE
		 CALL DEL_ACL('*','R',IER)
	      END IF
	      IF (.NOT.IER) THEN
		 WRITE(6,'('' Cannot modify access.'')')
		 CALL SYS_GETMSG(IER)
	      END IF
	   END IF

	   DO WHILE (CLI$GET_VALUE('ACCESS_ID',INPUT,ILEN)
     &	    .NE.%LOC(CLI$_ABSENT).AND..NOT.ALL)
	      IER = SYS_TRNLNM(INPUT,INPUT)
	      IF (INPUT(:1).EQ.'@') THEN
		 ILEN = INDEX(INPUT,',') - 1
		 IF (ILEN.EQ.-1) ILEN = TRIM(INPUT)
		 OPEN (UNIT=3,STATUS='OLD',FILE=INPUT(2:ILEN),
     &			DEFAULTFILE='.DIS',IOSTAT=IER)
		 IF (IER.NE.0) THEN
		    WRITE (6,'('' ERROR: Cannot find file '',A)')
     &					INPUT(2:ILEN)
		    RETURN
		 END IF
		 READ (3,'(A)',IOSTAT=IER) INPUT
		 IF (IER.NE.0) THEN
		    CLOSE (UNIT=3)
		    INPUT = ' '
		 ELSE
		    FILE_OPEN = .TRUE.
		 END IF
	      ELSE
		 FILE_OPEN = .FALSE.
	      END IF
	      DO WHILE (TRIM(INPUT).GT.0)
	         COMMA = INDEX(INPUT,',')
		 IF (INDEX(INPUT,'[').EQ.0.AND.INDEX(INPUT,']').GT.0.AND.
     &		    ID(:1).EQ.'[') INPUT = ID(:TRIM(ID))//','//INPUT
		 IF (INPUT(:1).EQ.'['.AND.INDEX(INPUT,']').GT.0)
     &		    COMMA = INDEX(INPUT,']') + 1
		 IF (INPUT(:1).EQ.'"'.AND.INDEX(INPUT(2:),'"').GT.0)
     &		    COMMA = INDEX(INPUT(2:),'"') + 2
		 IF (INPUT(:1).EQ.'['.AND.INDEX(INPUT,']').EQ.0) COMMA = 0
	         IF (COMMA.GT.0) THEN
		    ID = INPUT(1:COMMA-1)
		    INPUT = INPUT(COMMA+1:)
	            ILEN = TRIM(ID)
	         ELSE
		    ID = INPUT
		    INPUT = ' '
	            ILEN = TRIM(ID)
	         END IF
 	         IF (.NOT.NEWS.AND.ID.EQ.FOLDER1_OWNER) THEN
	            WRITE (6,'('' ERROR: Cannot modify access'',
     &			       '' for owner of folder.'')')
		 ELSE IF (ID(:1).NE.'['.OR.INDEX(ID,']').NE.0) THEN
		    IF (ILEN.EQ.0) THEN
		       IER = SS$_IVIDENT
		    ELSE IF (ACCESS) THEN
	               IF (READONLY) THEN
	                  CALL ADD_ACL(ID,'R',IER)
		       ELSE
	                  CALL ADD_ACL(ID,'R+W',IER)
		       END IF
	            ELSE
	               CALL DEL_ACL(ID,'R+W',IER)
	               IF (.NOT.IER) CALL DEL_ACL(ID,'R',IER)
	            END IF
	            IF (.NOT.IER) THEN
		       WRITE(6,'('' Cannot modify access for '',A,
     &					''.'')') ID(:ILEN)
		       CALL SYS_GETMSG(IER)
		    ELSE
		       WRITE(6,'('' Access modified for '',A,''.'')')
     &				ID(:ILEN)
		    END IF
		 END IF
	         IF (TRIM(INPUT).EQ.0.AND.FILE_OPEN) THEN
		    READ (3,'(A)',IOSTAT=IER) INPUT
		    IF (IER.NE.0) THEN
		       CLOSE (UNIT=3)
		       INPUT = ' '
		       FILE_OPEN = .FALSE.
		    END IF
		 END IF
	      END DO
	   END DO
	   
100	   IF (OLD_FOLDER1_FLAG.NE.FOLDER1_FLAG) THEN
	      IF (NEWS) THEN
	         CALL OPEN_BULLNEWS
	      ELSE
	         CALL OPEN_BULLFOLDER
	      END IF
	      OLD_FOLDER1_FLAG = FOLDER1_FLAG
	      CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER)
	      FOLDER1_FLAG = OLD_FOLDER1_FLAG
	      CALL REWRITE_FOLDER_FILE_TEMP(IER)
	      CALL CLOSE_BULLFOLDER
	   END IF
	END IF

	RETURN

	END



	SUBROUTINE CHKACL(FILENAME,IERACL)
C
C  SUBROUTINE CHKACL
C
C  FUNCTION: Checks ACL of given file.
C
C  PARAMETERS:
C	FILENAME - Name of file to check.
C	IERACL   - Error returned for attempt to open file.
C

	IMPLICIT INTEGER (A-Z)

	CHARACTER*(*) FILENAME

	INCLUDE '($ACLDEF)'
	INCLUDE '($SSDEF)'

	CHARACTER*256 ACLENT

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(256,ACL$C_READACL,%LOC(ACLENT))
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist

	IERACL=SYS$CHANGE_ACL(,ACL$C_FILE,FILENAME,%VAL(ACL_ITMLST),,,)

	IF (IERACL.EQ.SS$_ACLEMPTY) THEN
	   IERACL = SS$_NORMAL.OR.IERACL
	END IF

	RETURN
	END



	SUBROUTINE CHECK_ACCESS(FILENAME,USERNAME,READ_ACCESS,WRITE_ACCESS)
C
C  SUBROUTINE CHECK_ACCESS
C
C  FUNCTION: Checks ACL of given file.
C
C  PARAMETERS:
C	FILENAME - Name of file to check.
C	USERNAME - Name of user to check access for.
C	READ_ACCESS - Error returned indicating read access.
C	WRITE_ACCESS - Error returned indicating write access.
C		       If initially set to -1, indicates just
C		       folder for read access.
C

	IMPLICIT INTEGER (A-Z)

	CHARACTER FILENAME*(*),USERNAME*(*),ACE*256,OUTPUT*80

	INCLUDE '($ACLDEF)'
	INCLUDE '($CHPDEF)'
	INCLUDE '($ARMDEF)'

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(4,CHP$_FLAGS,%LOC(FLAGS))
	CALL ADD_2_ITMLST(4,CHP$_ACCESS,%LOC(ACCESS))
	CALL ADD_2_ITMLST(LEN(ACE),CHP$_MATCHEDACE,%LOC(ACE))
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist

	FLAGS = 0		! Default is no access

	ACCESS = ARM$M_READ	! Check if user has read access
	READ_ACCESS=SYS$CHECK_ACCESS(ACL$C_FILE,FILENAME,USERNAME,
     &		%VAL(ACL_ITMLST))


	IF (ICHAR(ACE(:1)).NE.0) THEN
	   CALL SYS$FORMAT_ACL(ACE,,OUTPUT,,,,)
	   IF (INDEX(OUTPUT,'=*').NE.0.AND.
     &		INDEX(OUTPUT,'READ').EQ.0) READ_ACCESS = 0
	ELSE IF (ICHAR(ACE(:1)).EQ.0.AND.READ_ACCESS) THEN
	   READ_ACCESS = 0
	END IF

	IF (WRITE_ACCESS.EQ.-1) THEN	! Only check read access
	   RETURN
	ELSE IF (READ_ACCESS.EQ.0) THEN	! If no read access, then of
	   WRITE_ACCESS = 0		! course there is no write access.
	   RETURN
	END IF

	ACCESS = ARM$M_WRITE	! Check if user has write access
	WRITE_ACCESS=SYS$CHECK_ACCESS(ACL$C_FILE,FILENAME,USERNAME,
     &		%VAL(ACL_ITMLST))

	IF (ICHAR(ACE(:1)).NE.0) THEN
	   CALL SYS$FORMAT_ACL(ACE,,OUTPUT,,,,)
	   IF (INDEX(OUTPUT,'=*').NE.0.AND.
     &		INDEX(OUTPUT,'WRITE').EQ.0) WRITE_ACCESS = 0
	ELSE IF (ICHAR(ACE(:1)).EQ.0.AND.WRITE_ACCESS) THEN
	   WRITE_ACCESS = 0
	END IF

	RETURN
	END




	SUBROUTINE SHOWACL(FILENAME)
C
C  SUBROUTINE SHOWACL
C
C  FUNCTION: Shows users who are allowed to read private bulletin.
C
C  PARAMETERS:
C	FILENAME - Name of file to check.
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE '($ACLDEF)'

	CHARACTER*(*) FILENAME

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(4,ACL$C_ACLLENGTH,%LOC(ACLLENGTH))
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist

	IER = SYS$CHANGE_ACL(,ACL$C_FILE,FILENAME,%VAL(ACL_ITMLST),,,)

	CALL LIB$GET_VM(ACLLENGTH+8,ACLSTR)
	CALL MAKE_CHAR(%VAL(ACLSTR),ACLLENGTH,ACLLENGTH)

	CALL READACL(FILENAME,%VAL(ACLSTR),ACLLENGTH)

	RETURN
	END



	SUBROUTINE FOLDER_FILE_ROUTINES

	IMPLICIT INTEGER (A-Z)

	CHARACTER*(*) KEY_NAME

	INCLUDE 'BULLFOLDER.INC'

	COMMON /NEWS_OPEN/ NEWS_OPEN

	ENTRY WRITE_FOLDER_FILE(IER)

	IF (NEWS_OPEN) CALL FOLDER_TO_NEWS

	DO WHILE (REC_LOCK(IER))
	   IF (NEWS_OPEN) THEN
	      WRITE (7,IOSTAT=IER) NEWS_FOLDER_COM
	   ELSE
	      WRITE (7,IOSTAT=IER) FOLDER_COM
	   END IF
	END DO

	RETURN

        ENTRY WRITE_FOLDER_FILE_TEMP(IER)

        IF (NEWS_OPEN) CALL FOLDER1_TO_NEWS

        DO WHILE (REC_LOCK(IER))
           IF (NEWS_OPEN) THEN
              WRITE (7,IOSTAT=IER) NEWS_FOLDER1_COM
           ELSE
              WRITE (7,IOSTAT=IER) FOLDER1_COM
           END IF
        END DO

        RETURN

	ENTRY REWRITE_FOLDER_FILE(IER)

	IF (NEWS_OPEN) THEN
	   CALL FOLDER_TO_NEWS
	   REWRITE (7,IOSTAT=IER) NEWS_FOLDER_COM
	ELSE
	   REWRITE (7,IOSTAT=IER) FOLDER_COM
	END IF

	RETURN

	ENTRY REWRITE_FOLDER_FILE_TEMP(IER) 

	IF (NEWS_OPEN) THEN
	   CALL FOLDER1_TO_NEWS
	   REWRITE (7,IOSTAT=IER) NEWS_FOLDER1_COM
	ELSE
	   REWRITE (7,IOSTAT=IER) FOLDER1_COM
	END IF

	RETURN

	ENTRY READ_FOLDER_FILE(IER)

	DO WHILE (REC_LOCK(IER))
	   IF (NEWS_OPEN) THEN
	      READ (7,IOSTAT=IER) NEWS_FOLDER_COM
	   ELSE
	      READ (7,IOSTAT=IER) FOLDER_COM
	   END IF
	END DO

	IF (NEWS_OPEN.AND.IER.EQ.0) CALL NEWS_TO_FOLDER

	RETURN

	ENTRY READ_FOLDER_FILE_TEMP(IER)

	DO WHILE (REC_LOCK(IER))
	   IF (NEWS_OPEN) THEN
	      READ (7,IOSTAT=IER) NEWS_FOLDER1_COM
	   ELSE
	      READ (7,IOSTAT=IER) FOLDER1_COM
	   END IF
	END DO

	IF (NEWS_OPEN.AND.IER.EQ.0) CALL NEWS_TO_FOLDER1

	RETURN

	ENTRY READ_FOLDER_FILE_KEYNUM(KEY_NUMBER,IER)

	SAVE_FOLDER_NUMBER = FOLDER_NUMBER

	DO WHILE (REC_LOCK(IER))
	   IF (NEWS_OPEN) THEN
	      READ (7,KEY=KEY_NUMBER,KEYID=1,IOSTAT=IER) NEWS_FOLDER_COM
	   ELSE
	      READ (7,KEY=KEY_NUMBER,KEYID=1,IOSTAT=IER) FOLDER_COM
	   END IF
	END DO

	IF (NEWS_OPEN.AND.IER.EQ.0) CALL NEWS_TO_FOLDER

	FOLDER_NUMBER = SAVE_FOLDER_NUMBER

	RETURN

	ENTRY READ_FOLDER_FILE_KEYNUM_GT(KEY_NUMBER,IER)

	DO WHILE (REC_LOCK(IER))
	   IF (NEWS_OPEN) THEN
	      READ (7,KEYGT=KEY_NUMBER,KEYID=1,IOSTAT=IER) NEWS_FOLDER_COM
	   ELSE
	      READ (7,KEYGT=KEY_NUMBER,KEYID=1,IOSTAT=IER) FOLDER_COM
	   END IF
	END DO

	IF (NEWS_OPEN.AND.IER.EQ.0) CALL NEWS_TO_FOLDER

	RETURN

	ENTRY READ_FOLDER_FILE_KEYNUM_TEMP(KEY_NUMBER,IER)

	DO WHILE (REC_LOCK(IER))
	   IF (NEWS_OPEN) THEN
	      READ (7,KEY=KEY_NUMBER,KEYID=1,IOSTAT=IER) NEWS_FOLDER1_COM
	   ELSE
	      READ (7,KEY=KEY_NUMBER,KEYID=1,IOSTAT=IER) FOLDER1_COM
	   END IF
	END DO

	IF (NEWS_OPEN.AND.IER.EQ.0) CALL NEWS_TO_FOLDER1

	RETURN

	ENTRY READ_FOLDER_FILE_KEYNUM_GT_TEMP(KEY_NUMBER,IER)

	DO WHILE (REC_LOCK(IER))
	   IF (NEWS_OPEN) THEN
	      READ (7,KEYGT=KEY_NUMBER,KEYID=1,IOSTAT=IER) NEWS_FOLDER1_COM
	   ELSE
	      READ (7,KEYGT=KEY_NUMBER,KEYID=1,IOSTAT=IER) FOLDER1_COM
	   END IF
	END DO

	IF (NEWS_OPEN.AND.IER.EQ.0) CALL NEWS_TO_FOLDER1

	RETURN

	ENTRY READ_FOLDER_FILE_KEYNAME_TEMP(KEY_NAME,IER)

	DO WHILE (REC_LOCK(IER))
	   IF (NEWS_OPEN) THEN
	      READ (7,KEY=KEY_NAME,KEYID=0,IOSTAT=IER) NEWS_FOLDER1_COM
	   ELSE
	      READ (7,KEY=KEY_NAME,KEYID=0,IOSTAT=IER) FOLDER1_COM
	   END IF
	END DO

	IF (NEWS_OPEN.AND.IER.EQ.0) CALL NEWS_TO_FOLDER1

	RETURN

	ENTRY READ_FOLDER_FILE_KEYNAMEGE_TEMP(KEY_NAME,IER)

	DO WHILE (REC_LOCK(IER))
	   IF (NEWS_OPEN) THEN
	      READ (7,KEYGE=KEY_NAME,KEYID=0,IOSTAT=IER) NEWS_FOLDER1_COM
	   ELSE
	      READ (7,KEYGE=KEY_NAME,KEYID=0,IOSTAT=IER) FOLDER1_COM
	   END IF
	END DO

	IF (NEWS_OPEN.AND.IER.EQ.0) CALL NEWS_TO_FOLDER1

	RETURN

	ENTRY READ_FOLDER_FILE_KEYNAMEGT_TEMP(KEY_NAME,IER)

	DO WHILE (REC_LOCK(IER))
	   IF (NEWS_OPEN) THEN
	      READ (7,KEYGT=KEY_NAME,KEYID=0,IOSTAT=IER) NEWS_FOLDER1_COM
	   ELSE
	      READ (7,KEYGT=KEY_NAME,KEYID=0,IOSTAT=IER) FOLDER1_COM
	   END IF
	END DO

	IF (NEWS_OPEN.AND.IER.EQ.0) CALL NEWS_TO_FOLDER1

	RETURN

	ENTRY READ_FOLDER_FILE_KEYNAME(KEY_NAME,IER)

	DO WHILE (REC_LOCK(IER))
	   IF (NEWS_OPEN) THEN
	      READ (7,KEY=KEY_NAME,KEYID=0,IOSTAT=IER) NEWS_FOLDER_COM
	   ELSE
	      READ (7,KEY=KEY_NAME,KEYID=0,IOSTAT=IER) FOLDER_COM
	   END IF
	END DO

	IF (NEWS_OPEN.AND.IER.EQ.0) CALL NEWS_TO_FOLDER

	RETURN

	END


	SUBROUTINE USER_FILE_ROUTINES

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($PRVDEF)'

	INCLUDE '($FORIOSDEF)'

	CHARACTER*(*) KEY_NAME

	INCLUDE 'BULLUSER.INC'

	CHARACTER*12 SAVE_USERNAME

	ENTRY READ_USER_FILE(IER)

	SAVE_USERNAME = USERNAME

	DO WHILE (REC_LOCK(IER))
	   READ (4,IOSTAT=IER) USER_ENTRY
	END DO

	TEMP_USER = USERNAME
	USERNAME = SAVE_USERNAME

	RETURN

	ENTRY READ_USER_FILE_KEYNAME(KEY_NAME,IER)

	SAVE_USERNAME = USERNAME

	DO WHILE (REC_LOCK(IER))
	   READ (4,KEY=KEY_NAME,IOSTAT=IER) USER_ENTRY
	END DO

	USERNAME = SAVE_USERNAME
	TEMP_USER = KEY_NAME

	RETURN

	ENTRY READ_USER_FILE_HEADER(IER)

	DO WHILE (REC_LOCK(IER))
	   READ (4,KEY='            ',IOSTAT=IER) USER_HEADER
	   IF (IER.EQ.FOR$IOS_ATTACCNON) THEN
	      WRITE (4,FMT=USER_FMT,IOSTAT=IER)
     &		 USER_HEADER_KEY,NEWEST_BTIM,
     &	         BBOARD_BTIM,PRV$M_OPER.OR.PRV$M_CMKRNL.OR.
     &	         PRV$M_SETPRV,(0,I=1,FLONG*4-1)
	      IER = FOR$IOS_SPERECLOC
	   END IF
	END DO

	RETURN

	ENTRY WRITE_USER_FILE_NEW(IER)

	DO I=1,FLONG
	   SET_FLAG(I) = SET_FLAG_DEF(I)
	   BRIEF_FLAG(I) = BRIEF_FLAG_DEF(I)
	   NOTIFY_FLAG(I) = NOTIFY_FLAG_DEF(I)
	END DO

	ENTRY WRITE_USER_FILE(IER)

	DO WHILE (REC_LOCK(IER))
	   WRITE (4,IOSTAT=IER) USER_ENTRY
	END DO

	RETURN

	END



	CHARACTER*(*) FUNCTION NEW_NEWS_ACCESS(IFILE)
 
	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFILES.INC'

	CHARACTER IFILE*(*),FILE*80

        FILE = IFILE
	
	DO I=1,TRIM(FILE)
	   IF (FILE(I:I).EQ.'.') FILE(I:I) = '_'
	END DO

	FILE = FILE(:INDEX(FILE,' ')-1)
	IF (FILE(TRIM(FILE):TRIM(FILE)).EQ.'_') FILE = FILE(:TRIM(FILE)-1)

	NEW_NEWS_ACCESS = 
     &	   NEWS_DIRECTORY(:TRIM(NEWS_DIRECTORY))//FILE(:TRIM(FILE))
     &	   //'.ACCESS'

	RETURN
	END




	CHARACTER*(*) FUNCTION NEWS_ACCESS(IFILE)
 
	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFILES.INC'

	CHARACTER IFILE*(*),FILE*80

        FILE = IFILE
	
	DO I=1,TRIM(FILE)
	   IF (FILE(I:I).EQ.'.') FILE(I:I) = '_'
	END DO

	FILE = FILE(:INDEX(FILE,' ')-1)
	IF (FILE(TRIM(FILE):TRIM(FILE)).EQ.'_') FILE = FILE(:TRIM(FILE)-1)

	C = 0

	DO WHILE (TRIM(FILE).GT.0.AND..NOT.LIB$FIND_FILE(
     &	   NEWS_DIRECTORY(:TRIM(NEWS_DIRECTORY))//FILE(:TRIM(FILE))
     &	   //'.ACCESS',NEWS_ACCESS,C))
	   L = LAST_INDEX(FILE,'_')-1
	   IF (L.LE.0) THEN
	      FILE = ' '
	   ELSE
	      FILE = FILE(:L)
	   END IF
	END DO

	RETURN
	END




	INTEGER FUNCTION LAST_INDEX(INPUT,FIND)

	IMPLICIT INTEGER (A-Z)

	CHARACTER*(*) INPUT,FIND

        F = LEN(FIND)

	DO LAST_INDEX=LEN(INPUT)-F+1,F,-1
           IF (INPUT(LAST_INDEX:LAST_INDEX+F-1).EQ.FIND) RETURN
	END DO

	RETURN
	END




	SUBROUTINE GET_NEXT_GROUP(MATCH,FOLDER_MATCH,MLEN,FOUND,STORED,
     &				  STAT,IER,ACTIVE)

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE '($SSDEF)'

	COMMON /LAST_BUFFER/ OLD_BUFFER
	CHARACTER*(INPUT_LENGTH) OLD_BUFFER

	COMMON /CTRLC_FLAG/ FLAG

	CHARACTER*(*) STAT,FOLDER_MATCH

	CHARACTER NEWS_ACCESS*132

	FOUND = .FALSE.
	STAR = INDEX(FOLDER_MATCH,'*')
	ONE = STAR.EQ.0.AND.TRIM(FOLDER_MATCH).GT.0
	START = .FALSE.
	IF (STAR.GT.1)
     &	   START = FOLDER_MATCH(:STAR-1).NE.FOLDER1(:STAR-1)
	STARTNOW = START

	DO WHILE (FLAG.NE.1.AND.IER.EQ.0.AND..NOT.FOUND) 
	   IF (ONE) THEN
	      CALL READ_FOLDER_FILE_KEYNAME_TEMP
     &		(FOLDER_MATCH(:TRIM(FOLDER_MATCH)),IER)
	      FOLDER_MATCH = ' '
	   ELSE IF (STARTNOW) THEN 
	      CALL READ_FOLDER_FILE_KEYNAMEGE_TEMP
     &				       (FOLDER_MATCH(:STAR-1),IER)
	      STARTNOW = .FALSE.
	   ELSE
	      CALL READ_FOLDER_FILE_TEMP(IER)
	   END IF
	   J = INDEX(FOLDER1_DESCRIP,' ')
	   IF (J.GT.0) THEN
	      STAT(:1) = FOLDER1_DESCRIP(J+1:)
	   ELSE
	      STAT = ' '
	      J = TRIM(FOLDER1_DESCRIP) + 1
	   END IF
	   IF (IER.EQ.0.AND.(.NOT.ACTIVE.OR.(STAT(:1).NE.'x'.AND.
     &		.NOT.BTEST(FOLDER1_FLAG,9))).AND.
     &		(.NOT.STORED.OR.BTEST(FOLDER1_FLAG,8)).AND.
     &		(ONE.OR..NOT.MATCH.OR.STR$MATCH_WILD(FOLDER1_DESCRIP
     &		(:J-1),FOLDER_MATCH(:MLEN)))) THEN
	      IF (BTEST(FOLDER1_FLAG,0)) THEN
		 IF (OLD_BUFFER.NE.NEWS_ACCESS(FOLDER1_DESCRIP)) THEN
		    OLD_BUFFER = NEWS_ACCESS(FOLDER1_DESCRIP)
		    CALL CHKACL(OLD_BUFFER(:TRIM(OLD_BUFFER)),IER2)
	   	    IF (IER2.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL)) THEN
		       CALL CHECK_ACCESS(OLD_BUFFER
     &			 (:TRIM(OLD_BUFFER)),USERNAME,FOUND1,-1)
 		    ELSE
		       FOUND1 = .TRUE.
		    END IF
		 END IF
		 FOUND = FOUND1
	      ELSE
		 FOUND = .TRUE.
	      END IF
	   ELSE IF (IER.EQ.0.AND.START) THEN 
	      IF (FOLDER_MATCH(:STAR-1).NE.FOLDER1(:STAR-1)) RETURN
	   END IF
	   IF (ONE) RETURN
	END DO

	RETURN
	END
