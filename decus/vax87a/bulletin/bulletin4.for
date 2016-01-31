C
C  BULLETIN4.FOR, Version 1/27/87
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Programmer: Mark R. London
C
C  NOTE: Subroutine CHECK_ACCESS which is used to see if user has only read
C  access to a folder only works for VMS V4.4 or later.  If you have an
C  early version, modify as indicated.
C
	SUBROUTINE ADD_ACL(ID,ACCESS,IER)
C
C  SUBROUTINE ADD_ACL
C
C  FUNCTION: Adds ACL to bulletin files.
C
C  PARAMETERS:
C	ID - Character string containing identifier to add to ACL.
C	ACCESS - Character string containing access controls to give to ID.
C	IER - Return error from attempting to set ACL.
C
C  NOTE: The ID must be in the RIGHTS data base.
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	CHARACTER ACLENT*255,ID*(*),ACCESS*(*)

	INCLUDE '($ACLDEF)'

	INCLUDE '($SSDEF)'

	IER = SYS$PARSE_ACL('(IDENTIFIER='//ID//',ACCESS='
     &	   //ACCESS//')',ACLENT,,)
	IF (.NOT.IER) THEN
	   IF (IER.EQ.SS$_NOSUCHID.AND.ADDID) THEN
	      CALL GET_UAF(ID,USER,GROUP,ACCOUNT,FLAGS,IER)
	      IF (.NOT.IER) RETURN
	      IDENT = USER + ISHFT(GROUP,16)
	      IER = SYS$ADD_IDENT(ID,%VAL(IDENT),,)
	      IF (IER) THEN
	         IER = SYS$PARSE_ACL('(IDENTIFIER='//ID//',ACCESS='
     &	           //ACCESS//')',ACLENT,,)
	      END IF
	   END IF
	END IF
	IF (.NOT.IER) RETURN

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(ICHAR(ACLENT(1:1)),ACL$C_ADDACLENT,%LOC(ACLENT))
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist

	LEN = TRIM(FOLDER1_FILE)

	IER = SYS$CHANGE_ACL(,ACL$C_FILE,FOLDER1_FILE(1:LEN)//
     &		'.BULLDIR',%VAL(ACL_ITMLST),,,)
	IF (.NOT.IER) RETURN
	IER = SYS$CHANGE_ACL(,ACL$C_FILE,FOLDER1_FILE(1:LEN)//
     &		'.BULLFIL',%VAL(ACL_ITMLST),,,)
	IF (.NOT.IER) RETURN

	RETURN
	END



	SUBROUTINE DEL_ACL(ID,ACCESS,IER)
C
C  SUBROUTINE DEL_ACL
C
C  FUNCTION: Adds ACL to bulletin files.
C
C  PARAMETERS:
C	ID - Character string containing identifier to add to ACL.
C	ACCESS - Character string containing access controls to give to ID.
C	IER - Return error from attempting to set ACL.
C
C  NOTE: The ID must be in the RIGHTS data base.
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	CHARACTER ACLENT*255,ID*(*),ACCESS*(*)

	INCLUDE '($ACLDEF)'

	IF (ID.NE.' ') THEN
	   IER = SYS$PARSE_ACL('(IDENTIFIER='//ID//',ACCESS='
     &	      //ACCESS//')',ACLENT,,)
	   IF (.NOT.IER) RETURN

	   CALL INIT_ITMLST	! Initialize item list
	   CALL ADD_2_ITMLST(ICHAR(ACLENT(1:1)),ACL$C_DELACLENT,%LOC(ACLENT))
	   CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist
	ELSE
	   CALL INIT_ITMLST	! Initialize item list
	   CALL ADD_2_ITMLST(255,ACL$C_DELETEACL,%LOC(ACLENT))
	   CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist
	END IF

	LEN = TRIM(FOLDER1_FILE)

	IER = SYS$CHANGE_ACL(,ACL$C_FILE,FOLDER1_FILE(1:LEN)//
     &		'.BULLDIR',%VAL(ACL_ITMLST),,,)
	IF (.NOT.IER) RETURN
	IER = SYS$CHANGE_ACL(,ACL$C_FILE,FOLDER1_FILE(1:LEN)//
     &		'.BULLFIL',%VAL(ACL_ITMLST),,,)
	IF (.NOT.IER) RETURN

	RETURN
	END


	SUBROUTINE CREATE_FOLDER
C
C  SUBROUTINE CREATE_FOLDER
C
C  FUNCTION: Creates a new bulletin folder.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFILES.INC'

	IER = CLI$GET_VALUE('CREATE_FOLDER',FOLDER,LEN_T) ! Get folder name

	IF (LEN_T.GT.25) THEN
	   WRITE(6,'('' ERROR: Folder name must be < 26 characters.'')')
	   RETURN
	END IF

	IF (.NOT.SETPRV_PRIV().AND.	! /NOTIFY & /READNEW are privileged
     &	    (CLI$PRESENT('NOTIFY').OR.CLI$PRESENT('READNEW'))) THEN
	   WRITE (6,'(
     &      '' ERROR: No privs to change all NOTIFY or READNEW.'')')
	   RETURN
	END IF

	CALL OPEN_FILE(7)		! Open folder file
	READ (7,FMT=FOLDER_FMT,IOSTAT=IER,KEY=FOLDER,KEYID=0)
					! See if folder exists

	IF (IER.EQ.0) THEN
	   WRITE (6,'('' ERROR: Specified folder already exists.'')')
	   GO TO 1000
	END IF

	WRITE (6,'('' Enter one line description of folder.'')')

10	CALL GET_LINE(FOLDER_DESCRIP,LENDES)	! Get input line
	FOLDER_DESCRIP = FOLDER_DESCRIP(1:LENDES)	! End fill with spaces
	IF (LENDES.LE.0) GO TO 910
	IF (LENDES.GT.80) THEN			! If too many characters
	   WRITE(6,'('' ERROR: folder must be < 80 characters.'')')
	   GO TO 10
	END IF

	FOLDER_OWNER = USERNAME			! Get present username

	FOLDER_SET = .TRUE.

	CALL SYS$SETDFPROT('FF00'X,CUR_DEF_PROT)
			! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD,GROUP)

C
C  Folder file is placed in the directory FOLDER_DIRECTORY.
C  The file prefix is the name of the folder.
C

	FD_LEN = TRIM(FOLDER_DIRECTORY)
	IF (FD_LEN.EQ.0) THEN
	 WRITE (6,'('' ERROR: System programmer has disabled folders.'')')
	ELSE
	 FOLDER_FILE = FOLDER_DIRECTORY(1:FD_LEN)//FOLDER
	END IF

	OPEN (UNIT=2,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     1	 //'.BULLDIR',STATUS='NEW',
     1	 RECORDTYPE='FIXED',RECORDSIZE=115,ACCESS='DIRECT',IOSTAT=IER,
     1	 ORGANIZATION='RELATIVE',DISPOSE='KEEP',FORM='FORMATTED')

	IF (IER.NE.0) THEN
	   WRITE(6,'('' ERROR: Cannot create folder directory file.'')')
	   CALL ERRSNS(IDUMMY,IER)
	   CALL SYS_GETMSG(IER)
	   GO TO 910
	END IF

	OPEN (UNIT=1,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     1	 //'.BULLFIL',STATUS='NEW',
     1	 ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=32,
     1	 FORM='UNFORMATTED',IOSTAT=IER)

	IF (IER.NE.0) THEN
	   WRITE(6,'('' ERROR: Cannot create folder message file.'')')
	   CALL ERRSNS(IDUMMY,IER)
	   CALL SYS_GETMSG(IER)
	   GO TO 910
	END IF

	IF (CLI$PRESENT('PRIVATE').OR.CLI$PRESENT('SEMIPRIVATE')) THEN
				! Will folder have access limitations?
	   FOLDER1_FILE = FOLDER_FILE
	   CLOSE (UNIT=1)
	   CLOSE (UNIT=2)
	   IF (CLI$PRESENT('SEMIPRIVATE')) THEN
	      CALL ADD_ACL('*','R',IER)
	   ELSE
	      CALL ADD_ACL('*','NONE',IER)
	   END IF
	   CALL ADD_ACL(FOLDER_OWNER,'R+W+C',IER)
	   OPEN (UNIT=2,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     1	    //'.BULLDIR',STATUS='OLD',IOSTAT=IER1)
	   OPEN (UNIT=1,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     1	    //'.BULLFIL',STATUS='OLD',IOSTAT=IER1)
	   IF (.NOT.IER) THEN
	      WRITE(6,
     &	      '('' ERROR: Cannot create private folder using ACLs.'')')
	      CALL SYS_GETMSG(IER)
	      GO TO 910
	   END IF
	END IF

	IER = 0
	LAST_NUMBER = 1
	DO WHILE (IER.EQ.0.AND.LAST_NUMBER.LT.64)
	   READ (7,FMT=FOLDER_FMT,IOSTAT=IER,KEY=LAST_NUMBER,KEYID=1)
	   LAST_NUMBER = LAST_NUMBER + 1
	END DO

	IF (IER.EQ.0) THEN
	   WRITE (6,'('' ERROR: Limit of 63 folders has been reached.'')')
	   WRITE (6,'('' Unable to add specified folder.'')')
	   GO TO 910
	ELSE
	   FOLDER_NUMBER = LAST_NUMBER - 1
	END IF

	FOLDER_OWNER = USERNAME			! Get present username
	FOLDER_BBOARD = 'NONE'
	FOLDER_BBEXPIRE = 14

	WRITE (7,FMT=FOLDER_FMT) FOLDER,FOLDER_NUMBER,FOLDER_OWNER,
     &			FOLDER_DESCRIP,FOLDER_BBOARD,FOLDER_BBEXPIRE

	CLOSE (UNIT=1)
	CLOSE (UNIT=2)

	NOTIFY = 0
	READNEW = 0
	BRIEF = 0
	IF (CLI$PRESENT('NOTIFY')) NOTIFY = 1
	IF (CLI$PRESENT('READNEW')) READNEW = 1
	IF (CLI$PRESENT('BRIEF')) BRIEF = 1
	CALL SET_FOLDER_DEFAULT(NOTIFY,READNEW,BRIEF)

	WRITE (6,'('' Folder is now set to '',A)')
     &		FOLDER(1:TRIM(FOLDER))//'.'

	GO TO 1000

910	WRITE (6,'('' Aborting folder creation.'')')
	FOLDER_SET = .FALSE.
	CLOSE (UNIT=1,STATUS='DELETE')
	CLOSE (UNIT=2,STATUS='DELETE')

1000	CALL CLOSE_FILE(7)
	CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection

	RETURN

	END





	SUBROUTINE SET_FOLDER_DEFAULT(NOTIFY,READNEW,BRIEF)
C
C  SUBROUTINE SET_FOLDER_DEFAULT
C
C  FUNCTION: Sets NOTIFY or READNEW defaults for specified folder
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD

	IF (.NOT.SETPRV_PRIV().AND.INCMD(1:3).EQ.'SET') THEN
	   WRITE (6,'(
     &      '' ERROR: No privs to change all defaults.'')')
	   RETURN
	END IF

	CALL OPEN_FILE_SHARED(4)
	DO WHILE (REC_LOCK(IER))
	   READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER)	! Get header
     &      TEMP_USER,LOGIN_BTIM,READ_BTIM,NEW_FLAG,SET_FLAG,
     &	    BRIEF_FLAG,NOTIFY_FLAG
	END DO
	DO WHILE (IER.EQ.0)
	   IF (NOTIFY.EQ.0) CALL CLR2(NOTIFY_FLAG,FOLDER_NUMBER)
	   IF (NOTIFY.EQ.1) CALL SET2(NOTIFY_FLAG,FOLDER_NUMBER)
	   IF (READNEW.EQ.0) CALL CLR2(SET_FLAG,FOLDER_NUMBER)
	   IF (READNEW.EQ.1) CALL SET2(SET_FLAG,FOLDER_NUMBER)
	   IF (BRIEF.EQ.0) CALL CLR2(BRIEF_FLAG,FOLDER_NUMBER)
	   IF (BRIEF.EQ.1) CALL SET2(BRIEF_FLAG,FOLDER_NUMBER)
	   REWRITE(4,FMT=USER_FMT) TEMP_USER,LOGIN_BTIM,
     &		READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	   DO WHILE (REC_LOCK(IER))
	    READ (4,FMT=USER_FMT,KEYGT=TEMP_USER,IOSTAT=IER) TEMP_USER,
     &	     LOGIN_BTIM,READ_BTIM,NEW_FLAG,SET_FLAG,
     &	     BRIEF_FLAG,NOTIFY_FLAG		! Find if there is an entry
	   END DO
	   IF (TEMP_USER.NE.USER_HEADER.AND.
     &		(BRIEF.EQ.-1.OR.NOTIFY.EQ.-1.OR.READNEW.EQ.-1)) THEN
	      IER = 1		! Modify READNEW and NOTIFY for all users
	   END IF		! only during folder creation or deletion.
	END DO
	CALL CLOSE_FILE(4)

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

	INCLUDE 'BULLFILES.INC'

	EXTERNAL CLI$_ABSENT

	CHARACTER RESPONSE*1,TEMP*80

	IER = CLI$GET_VALUE('REMOVE_FOLDER',FOLDER1,LEN_T) ! Get folder name

	IF (IER.EQ.%LOC(CLI$_ABSENT)) THEN
	   IF (.NOT.FOLDER_SET) THEN
	      WRITE (6,'('' ERROR: No folder specified.'')')
	      RETURN
	   ELSE
	      FOLDER1 = FOLDER
	   END IF
	ELSE IF (LEN_T.GT.25) THEN
	   WRITE(6,'('' ERROR: Folder name must be < 26 characters.'')')
	   RETURN
	END IF

	CALL OPEN_FILE(7)		! Open folder file
	READ (7,FMT=FOLDER_FMT,KEY=FOLDER1,KEYID=0,IOSTAT=IER) FOLDER1,
     &    FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIP   ! See if it exists
	FOLDER1_FILE = FOLDER_DIRECTORY(1:TRIM(FOLDER_DIRECTORY))//
     &		FOLDER1

	IF (IER.NE.0) THEN
	   WRITE (6,'('' ERROR: No such folder exists.'')')
	   GO TO 1000
	END IF

	IF ((FOLDER1_OWNER.NE.USERNAME.AND..NOT.SETPRV_PRIV()).OR.
     &	     FOLDER1_NUMBER.EQ.0) THEN
	   WRITE (6,'('' ERROR: You are not able to remove the folder.'')')
	   GO TO 1000
	END IF

	CALL GET_INPUT_PROMPT(RESPONSE,LEN,
     &   'Are you sure you want to remove folder '
     &	 //FOLDER1(1:TRIM(FOLDER1))//' (Y/N with N as default): ')
	IF (RESPONSE.NE.'y'.AND.RESPONSE.NE.'Y') THEN
	   WRITE (6,'('' Folder was not removed.'')')
	   RETURN
	END IF

	TEMP = FOLDER_FILE
	FOLDER_FILE = FOLDER1_FILE
	TEMPSET = FOLDER_SET
	FOLDER_SET = .TRUE.
	CALL OPEN_FILE(2)			! Remove directory file
	CALL OPEN_FILE(1)			! Remove bulletin file
	CALL CLOSE_FILE_DELETE(1)
	CALL CLOSE_FILE_DELETE(2)
	FOLDER_FILE = TEMP
	FOLDER_SET = TEMPSET

	DELETE (7)

	TEMP_NUMBER = FOLDER_NUMBER
	FOLDER_NUMBER = FOLDER1_NUMBER
	CALL SET_FOLDER_DEFAULT(0,0,0)
	FOLDER_NUMBER = TEMP_NUMBER

	WRITE (6,'('' Folder removed.'')')

	IF (FOLDER.EQ.FOLDER1) FOLDER_SET = .FALSE.

1000	CALL CLOSE_FILE(7)

	RETURN

	END


	SUBROUTINE SELECT_FOLDER(OUTPUT,IER)
C
C  SUBROUTINE SELECT_FOLDER
C
C  FUNCTION: Selects the specified folder.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLFILES.INC'

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE '($RMSDEF)'
	INCLUDE '($SSDEF)'

	COMMON /POINT/ BULL_POINT

	COMMON /ACCESS/ READ_ONLY
	LOGICAL READ_ONLY

	EXTERNAL CLI$_ABSENT

	DIMENSION FIRST_TIME(2)		! Bit set for folder if folder has
	DATA FIRST_TIME /2*0/		! been selected before this.

	IF (OUTPUT) IER = CLI$GET_VALUE('SELECT_FOLDER',FOLDER1,LEN)
							! Get folder name

	CALL OPEN_FILE_SHARED(7)			! Go find folder

	IF (((IER.EQ.%LOC(CLI$_ABSENT).OR.FOLDER1.EQ.'GENERAL').AND.
     &	 OUTPUT).OR.((FOLDER_NUMBER.EQ.0.OR.(FOLDER1.EQ.'GENERAL'.AND.
     &	 FOLDER_NUMBER.EQ.-1)).AND..NOT.OUTPUT)) THEN ! Select GENERAL
	   FOLDER_NUMBER = 0
	   FOLDER_SET = .FALSE.
	   DO WHILE (REC_LOCK(IER))
	      READ (7,FMT=FOLDER_FMT,KEY=FOLDER_NUMBER,KEYID=1,IOSTAT=IER)
     &		FOLDER,FOLDER_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP
     &		,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB
	   END DO
	   IF (OUTPUT) THEN
	      WRITE (6,'('' Folder has been set to '',A)')
     &		 FOLDER(1:TRIM(FOLDER))//'.'
	      BULL_POINT = 0	! Reset bulletin pointer to first bulletin
	   END IF
	   IER = 1
	   CALL CLOSE_FILE(7)
	   READ_ONLY = .FALSE.
	ELSE
	   DO WHILE (REC_LOCK(IER))
	      IF (OUTPUT.OR.FOLDER_NUMBER.EQ.-1) THEN
	       READ (7,FMT=FOLDER_FMT,KEY=FOLDER1,KEYID=0,IOSTAT=IER)
     &		FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIP
     &		,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,USERB1,GROUPB1
	      ELSE
	       FOLDER1_NUMBER = FOLDER_NUMBER
	       READ (7,FMT=FOLDER_FMT,KEY=FOLDER_NUMBER,KEYID=1,IOSTAT=IER)
     &		FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIP
     &		,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,USERB1,GROUPB1
	      END IF
	   END DO

	   CALL CLOSE_FILE(7)

	   IF (IER.EQ.0) THEN				! Folder found
	      FOLDER1_FILE = FOLDER_DIRECTORY(1:TRIM(FOLDER_DIRECTORY))
     &		//FOLDER1
	      CALL CHKACL
     &		(FOLDER1_FILE(1:TRIM(FOLDER1_FILE))//'.BULLFIL',IER)
	      IF (IER.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL).AND.USERNAME
     &		  .NE.FOLDER1_OWNER) THEN
	        CALL CHECK_ACCESS
     &		 (FOLDER1_FILE(1:TRIM(FOLDER1_FILE))//'.BULLFIL',USERNAME,
     &		  READ_ACCESS,WRITE_ACCESS)
		IF (.NOT.READ_ACCESS.AND..NOT.WRITE_ACCESS) THEN
	         WRITE(6,'('' You are not allowed to access folder.'')')
	         WRITE(6,'('' See '',A,'' if you wish to access folder.'')')
     &			FOLDER1_OWNER(1:TRIM(FOLDER1_OWNER))
		 RETURN
		END IF
	      END IF
	      IF (IER) THEN
	       FOLDER_SET = .TRUE.

	       FOLDER = FOLDER1			! Folder successfully set
	       FOLDER_NUMBER = FOLDER1_NUMBER	! so update permanent folder
	       FOLDER_OWNER = FOLDER1_OWNER	! parameters.
	       FOLDER_DESCRIP = FOLDER1_DESCRIP
	       FOLDER_BBOARD = FOLDER1_BBOARD
	       FOLDER_BBEXPIRE = FOLDER1_BBEXPIRE
	       FOLDER_FILE = FOLDER1_FILE
	       USERB = USERB1
	       GROUPB = GROUPB1

	       IF (OUTPUT) THEN
		  WRITE (6,'('' Folder has been set to '',A)') 
     &		    FOLDER(1:LEN)//'.'
		  BULL_POINT = 0	! Reset pointer to first bulletin
	       END IF

	       IF (IER.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL).AND.USERNAME
     &		  .NE.FOLDER_OWNER) THEN
	          IF (.NOT.WRITE_ACCESS) THEN
		   IF (OUTPUT)
     &		    WRITE (6,'('' Folder only accessible for reading.'')')
		   READ_ONLY = .TRUE.
		  ELSE
		   READ_ONLY = .FALSE.
		  END IF
	       ELSE
		  READ_ONLY = .FALSE.
	       END IF

	       IF (OUTPUT.AND.	! If first select, look for expired messages.
     &			.NOT.TEST2(FIRST_TIME,FOLDER_NUMBER)) THEN
		 CALL OPEN_FILE(2)
		 CALL READDIR(0,IER)	! Get header info from BULLDIR.DAT
	 	 IF (IER.EQ.1) THEN		! Is header present?
	   	    IER = COMPARE_DATE(NEWEST_EXDATE,' ') ! Yes. Any expired?
	 	    IF (IER.LE.0) CALL UPDATE  ! Need to update
		 END IF
		 CALL CLOSE_FILE(2)
		 CALL SET2(FIRST_TIME,FOLDER_NUMBER)
	       END IF

	       IF (OUTPUT.AND.TEST2(NEW_FLAG,FOLDER_NUMBER)) THEN
		 CALL CHANGE_FLAG(0,1)
		 CALL FIND_NEWEST_BULL		! See if there are new bulletins
		 IF (BULL_POINT.NE.-1) THEN
	     	    WRITE(6,'('' Type READ to read new messages.'')')
						! Alert user if new bulletins
		 ELSE
		    BULL_POINT = 0
		 END IF
	       END IF
	       IER = 1
	      ELSE IF (OUTPUT) THEN
		WRITE (6,'('' Cannot access specified folder.'')')
		CALL SYS_GETMSG(IER)
	      END IF
	   ELSE						! Folder not found
	      IF (OUTPUT) WRITE (6,'('' ERROR: Folder does not exist.'')')
	      IER = 0
	   END IF
	END IF

	RETURN

	END



	SUBROUTINE SHOW_FOLDER
C
C  SUBROUTINE SHOW_FOLDERR
Ce
C  FUNCTION: Shows the information on any folder.r
Ce

	IMPLICIT INTEGER (A-Z)g

	INCLUDE 'BULLUSER.INC'F

	INCLUDE 'BULLFOLDER.INC'm

	INCLUDE 'BULLFILES.INC'

	INCLUDE '($SSDEF)'_

	INCLUDE '($RMSDEF)'

	EXTERNAL CLI$_ABSENT

	CALL OPEN_FILE_SHARED(7)			! Open folder file

	IF (CLI$GET_VALUE('SHOW_FOLDER',FOLDER1).NE.%LOC(CLI$_ABSENT))
     &		THEN
10	   DO WHILE (REC_LOCK(IER))
	      READ (7,FMT=FOLDER_FMT,KEY=FOLDER1,KEYID=0,IOSTAT=IER).
     &		FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIP
     &		,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,USERB,GROUPBi
	   END DO
	   FOLDER1_FILE = FOLDER_DIRECTORY(1:TRIM(FOLDER_DIRECTORY))// 
     &		FOLDER1N
	   IF (IER.NE.0) THEN
	      WRITE (6,'('' ERROR: Specified folder was not found.'')')
	      CALL CLOSE_FILE(7)R
	      RETURN(
	   ELSE
	      WRITE (6,1010) FOLDER1,FOLDER1_OWNER,
     &			FOLDER1_DESCRIP(1:TRIM(FOLDER1_DESCRIP))'
	   END IF
	ELSE IF (FOLDER_SET) THEN
	   WRITE (6,1000) FOLDER,FOLDER_OWNER,.
     &			FOLDER_DESCRIP(1:TRIM(FOLDER_DESCRIP))_
	   FOLDER1_FILE = FOLDER_FILE
	   FOLDER1_BBOARD = FOLDER_BBOARD
	   FOLDER1_BBEXPIRE = FOLDER_BBEXPIRE
	   FOLDER1_NUMBER = FOLDER_NUMBER
	ELSE)
	   FOLDER1 = 'GENERAL' 
	   GO TO 10
	END IF_

	IF (CLI$PRESENT('FULL')) THEN
	   CALL CHKACL 
     &		(FOLDER1_FILE(1:TRIM(FOLDER1_FILE))//'.BULLFIL',IER)
	   IF (IER.EQ.(SS$_ACLEMPTY.OR.SS$_NORMAL).OR.(.NOT.IER)) THEN 
	     WRITE (6,'('' Folder is not a private folder.'')')
	   ELSE
	     CALL CHECK_ACCESSA
     &		(FOLDER1_FILE(1:TRIM(FOLDER1_FILE))//'.BULLFIL',USERNAME,
     &		 READ_ACCESS,WRITE_ACCESS)
	     IF (WRITE_ACCESS) 
     &	     CALL SHOWACL(FOLDER1_FILE(1:TRIM(FOLDER1_FILE))//'.BULLFIL')
	   END IF
	   IF (SETPRV_PRIV().OR.USERNAME.EQ.FOLDER1_OWNER) THEN
	      IF (FOLDER1_BBOARD.NE.'NONE') THEN
		 LEN = TRIM(FOLDER1_BBOARD)T
		 IF (LEN.GT.0) THENE
 	          WRITE (6,'('' BBOARD for folder is '',A<LEN>,''.'')')i
     &		 	FOLDER1_BBOARD(1:LEN)D
		 END IFr
		 IF (USERB.EQ.0.AND.GROUPB.EQ.0) THENC
 		  WRITE (6,'('' BBOARD was specified with /SPECIAL.'')') 
		 END IF
		 IF (FOLDER1_BBEXPIRE.GT.0) THEN
		  WRITE (6,'('' BBOARD expiration is '',I3,'' days.'')')
     &			FOLDER1_BBEXPIREE
		 ELSE
		  WRITE (6,'('' BBOARD messages will not expire.'')')D
		 END IF*
	      ELSEU
	         WRITE (6,'('' No BBOARD has been defined.'')')
	      END IFI
	      CALL OPEN_FILE_SHARED(4) 
	      DO WHILE (REC_LOCK(IER))(
	        READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER)l
     &            TEMP_USER,LOGIN_BTIM,READ_BTIM,C
     &	          NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG 
	      END DOi
	      IF (TEST2(SET_FLAG,FOLDER1_NUMBER)) THENz
	       IF (TEST2(BRIEF_FLAG,FOLDER1_NUMBER)) THEN
		 WRITE (6,'('' Default is BRIEF.'')')C
	       ELSE
		 WRITE (6,'('' Default is READNEW.'')')T
	       END IF
	      ELSES
		 WRITE (6,'('' Default is NOREADNEW.'')')/
	      END IFU
	      IF (TEST2(NOTIFY_FLAG,FOLDER1_NUMBER)) THEN
		 WRITE (6,'('' Default is NOTIFY.'')')
	      ELSE/
		 WRITE (6,'('' Default is NONOTIFY.'')')
	      END IF)
	      CALL CLOSE_FILE(4)
	   END IF
	END IFE

	CALL CLOSE_FILE(7)T

	RETURNF

1000	FORMAT(' Current folder is: ',A25,' Owner: ',A12,
     &		' Description: ',/,1X,A)
1010	FORMAT(' Folder name is: ',A25,' Owner: ',A12,
     &		' Description: ',/,1X,A)
	END


	SUBROUTINE DIRECTORY_FOLDERS(FOLDER_COUNT)f
Ce
C  SUBROUTINE DIRECTORY_FOLDERS
C 
C  FUNCTION: Display all FOLDER entries.
Cc
	IMPLICIT INTEGER (A - Z)N

	INCLUDE 'BULLFOLDER.INC'V

	COMMON /PAGE/ PAGE_LENGTH

	DATA SCRATCH_D1/0/ 

	IF (FOLDER_COUNT.GT.0) GO TO 50		! Skip init steps if this is
						! not the 1st page of folder

Cp
C  Folder listing is first buffered into temporary memory storage before
C  being outputted to the terminal.  This is to be able to quickly close the
C  folder file, and to avoid the possibility of the user holding the screen,
C  and thus causing the folder file to stay open.  The temporary memoryI
C  is structured as a linked-list queue, where SCRATCH_D1 points to the header
C  of the queue.  See BULLSUBS.FOR for more description of the queue.D
CR

	IF (SCRATCH_D1.EQ.0) THEN		! Is queue empty?.
	   CALL LIB$GET_VM(132,SCRATCH_D)	! If so, allocated memory
	   CALL MAKE_CHAR(%VAL(SCRATCH_D),120)	! Form a character string)
	   SCRATCH_D1 = SCRATCH_D		! Init header pointerE
	ELSE					! Else queue is not empty_
	   SCRATCH_D = SCRATCH_D1		! so reinit queue pointerT
	END IF					! to the header.

	CALL OPEN_FILE_SHARED(7)		! Get folder file

	NUM_FOLDER = 0i
	IER = 0
	FOLDER1 = '                         '	! Start folder search
	DO WHILE (IER.EQ.0)			! Copy all bulletins from file 
	   DO WHILE (REC_LOCK(IER))
	      READ (7,FMT=FOLDER_FMT,KEYGT=FOLDER1,KEYID=0,IOSTAT=IER)
     &		FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIP
	   END DO
	   IF (IER.EQ.0) THEN
	      NUM_FOLDER = NUM_FOLDER + 1
	      CALL WRITE_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,FOLDER_COM),
	   END IF
	END DOT

	CALL CLOSE_FILE(7)			! We don't need file anymore

	IF (NUM_FOLDER.EQ.0) THEN
	   WRITE (6,'('' There are no folders.'')')
	   RETURN
	END IFy

C.
C  Folder entries are now in queue.  Output queue entries to screen.
C 

	SCRATCH_D = SCRATCH_D1			! Init queue pointer to header

	FOLDER_COUNT = 1			! Init folder number counter

50	CALL LIB$ERASE_PAGE(1,1)		! Clear the screen2

	DISPLAY = MIN((NUM_FOLDER-FOLDER_COUNT+1)*2,PAGE_LENGTH-4)E
			! If more entries then page size, truncate output
	DO I=FOLDER_COUNT,FOLDER_COUNT+DISPLAY/2-1
	   CALL READ_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,FOLDER_COM)C
	   WRITE (6,1000) FOLDER1,FOLDER1_OWNER,FOLDER1_DESCRIP
	   FOLDER_COUNT = FOLDER_COUNT + 1	! Update folder counterE
	END DO_

	IF (FOLDER_COUNT.GT.NUM_FOLDER) THEN	! Outputted all entries?
	   FOLDER_COUNT = 0			! Yes. Set counter to 0.,
	ELSE 
	   WRITE(6,1010)			! Else say there are more 
	END IF

	RETURND

1000	FORMAT(' Folder: ',A25,' Owner: ',A12,' Description:',/,1X,A80)
1010	FORMAT(1X,/,' Press RETURN for more...',/)L

	END


	SUBROUTINE SET_ACCESS(ACCESS)
CF
C  SUBROUTINE SET_ACCESS
C 
C  FUNCTION: Set access on folder for specified ID.F
CN
C  PARAMETERS:
C	ACCESS  -  Logical: If .true., grant access, if .false. deny accesss
C 

	IMPLICIT INTEGER (A-Z)_

	INCLUDE 'BULLFOLDER.INC'0

	INCLUDE 'BULLUSER.INC'I

	INCLUDE 'BULLFILES.INC'

	INCLUDE '($SSDEF)'A

	LOGICAL ACCESS,ALL,READONLY

	EXTERNAL CLI$_ABSENTE

	CHARACTER ID*25,RESPONSE*1T

	IF (CLI$PRESENT('ALL')) THEND
	   ALL = .TRUE.
	ELSE
	   ALL = .FALSE. 
	END IFm

	IF (CLI$PRESENT('READONLY')) THEN
	   READONLY = .TRUE.n
	ELSE 
	   READONLY = .FALSE.
	END IFO

	IER = CLI$GET_VALUE('ACCESS_FOLDER',FOLDER1,LEN) ! Get folder nameL

	IF (IER.EQ.%LOC(CLI$_ABSENT)) THENe
	   IF (.NOT.FOLDER_SET) THEN
	      WRITE (6,'('' ERROR: No folder specified.'')')O
	      RETURNB
	   ELSE
	      FOLDER1 = FOLDERE
	   END IF
	ELSE IF (LEN.GT.25) THEN
	   WRITE(6,'('' ERROR: Folder name must be < 26 characters.'')')B
	   RETURN
	END IFE

	IF (.NOT.ALL) THEN=
	   IER = CLI$GET_VALUE('ACCESS_ID',ID,LEN) 	! Get IDP
	   IF (LEN.GT.25) THEN1
	      WRITE(6,'('' ERROR: ID name must be < 26 characters.'')')
	      RETURNs
	   END IF
	END IF&

	CALL OPEN_FILE(7)		! Open folder file
	READ (7,FMT=FOLDER_FMT,KEY=FOLDER1,KEYID=0,IOSTAT=IER)
     &	 FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIP ! See if it existsD
	CALL CLOSE_FILE(7) 

	IF ((.NOT.ALL).AND.(ID.EQ.FOLDER1_OWNER)) THEN!
	 WRITE (6,'(p
     &	  '' ERROR: Cannot modify access for owner of folder.'')')F
	 RETURN
	END IF,

	IF (IER.NE.0) THENN
	   WRITE (6,'('' ERROR: No such folder exists.'')')
	ELSE IF (FOLDER1_OWNER.NE.USERNAME.AND..NOT.SETPRV_PRIV()) THEN
	   WRITE (6,'
     &	'('' ERROR: You are not able to modify access to the folder.'')')
	ELSEA
	   FOLDER1_FILE = FOLDER_DIRECTORY(1:TRIM(FOLDER_DIRECTORY))//T
     &		FOLDER1T
	   CALL CHKACL 
     &		(FOLDER1_FILE(1:TRIM(FOLDER1_FILE))//'.BULLFIL',IER)
	   IF (IER.EQ.(SS$_ACLEMPTY.OR.SS$_NORMAL)) THENL
	     IF ((ALL.AND..NOT.READONLY).OR.(.NOT.ACCESS)) THEN
	        WRITE (6,'('' ERROR: Folder is not a private folder.'')')
		RETURN
	     END IF
	     CALL GET_INPUT_PROMPT(RESPONSE,LEN,D
     &      'Folder is not private. Do you want to make it so? (Y/N): ')
	     IF (RESPONSE.NE.'y'.AND.RESPONSE.NE.'Y') THENG
	       WRITE (6,'('' Folder access was not changed.'')')F
	       RETURN
	     ELSE
	       IF (READONLY.AND.ALL) THEN
	          CALL ADD_ACL('*','R',IER)
	       ELSE
	          CALL ADD_ACL('*','NONE',IER)S
	       END IF
	       CALL ADD_ACL(FOLDER1_OWNER,'R+W+C',IER)U
	     END IF
	   END IF
	   IF (ACCESS) THEN
	      IF (.NOT.ALL) THENG
	         IF (READONLY) THEN
	            CALL ADD_ACL(ID,'R',IER)U
		 ELSET
	            CALL ADD_ACL(ID,'R+W',IER)R
		 END IFW
	      ELSEG
	         IF (READONLY) THEN
	            CALL ADD_ACL('*','R',IER)
		 ELSEO
		    CALL DEL_ACL(' ','R+W',IER)A
		 END IF&
	      END IFO
	   ELSE
	      IF (ALL) THEN
		 CALL DEL_ACL('*','R',IER)
	      ELSEd
	         CALL DEL_ACL(ID,'R+W',IER)
	         IF (.NOT.IER) CALL DEL_ACL(ID,'R',IER)
	      END IFI
	   END IF
	   IF (.NOT.IER) THEN
	      WRITE(6,'('' ERROR: Cannot modify ACL of folder files.'')')
	      CALL SYS_GETMSG(IER)o
	   ELSE
	      WRITE (6,'('' Access to folder has been modified.'')')I
	   END IF
	END IF

	RETURNE

	END



	SUBROUTINE CHKACL(FILENAME,IERACL)A
CR
C  SUBROUTINE CHKACL
CI
C  FUNCTION: Checks ACL of given file.
CL
C  PARAMETERS:
C	FILENAME - Name of file to check.E
C	IERACL   - Error returned for attempt to open file.E
C,

	IMPLICIT INTEGER (A-Z)f

	CHARACTER*(*) FILENAME

	INCLUDE '($ACLDEF)'
	INCLUDE '($SSDEF)'F

	CHARACTER*255 ACLENT,ACLSTR

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(255,ACL$C_READACL,%LOC(ACLENT))
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist

	IERACL=SYS$CHANGE_ACL(,ACL$C_FILE,FILENAME,%VAL(ACL_ITMLST),,,)

	IF (IERACL.EQ.SS$_ACLEMPTY) THENS
	   IERACL = SS$_NORMAL.OR.IERACLO
	END IFC

	RETURN(
	END



	SUBROUTINE CHECK_ACCESS(FILENAME,USERNAME,READ_ACCESS,WRITE_ACCESS)
CO
C  SUBROUTINE CHECK_ACCESS
C
C  FUNCTION: Checks ACL of given file.
CW
C  PARAMETERS:
C	FILENAME - Name of file to check. 
C	USERNAME - Name of user to check access for.
C	READ_ACCESS - Error returned indicating read access.
C	WRITE_ACCESS - Error returned indicating write access.
C,
C  NOTE: SYS$CHECK_ACCESS is only available under V4.4 or later.
C	If you have an earlier version, comment out the lines which call
C	it and set both READ_ACCESS and WRITE_ACCESS to 1, which willn
C	allow program to run, but will not allow READONLY access feature.E
CI

	IMPLICIT INTEGER (A-Z)T

	CHARACTER FILENAME*(*),USERNAME*(*)

	INCLUDE '($ACLDEF)'
	INCLUDE '($CHPDEF)'
	INCLUDE '($ARMDEF)'

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(4,CHP$_FLAGS,%LOC(FLAGS))
	CALL ADD_2_ITMLST(4,CHP$_ACCESS,%LOC(ACCESS))
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist

	FLAGS = 0		! Default is no access

	ACCESS = ARM$M_READ	! Check if user has read access
	READ_ACCESS=SYS$CHECK_ACCESS(ACL$C_FILE,FILENAME,USERNAME,.
     &		%VAL(ACL_ITMLST))I

	ACCESS = ARM$M_WRITE	! Check if user has write access
	WRITE_ACCESS=SYS$CHECK_ACCESS(ACL$C_FILE,FILENAME,USERNAME,
     &		%VAL(ACL_ITMLST))l

	RETURN
	END




	SUBROUTINE SHOWACL(FILENAME)D
CI
C  SUBROUTINE SHOWACLI
C.
C  FUNCTION: Shows users who are allowed to read private bulletin.
C'
C  PARAMETERS:
C	FILENAME - Name of file to check.U
CP
	IMPLICIT INTEGER (A-Z)R

	INCLUDE '($ACLDEF)'

	CHARACTER*(*) FILENAMEN

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(4,ACL$C_ACLLENGTH,%LOC(ACLLENGTH))s
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist

	IER = SYS$CHANGE_ACL(,ACL$C_FILE,FILENAME,%VAL(ACL_ITMLST),,,)P

	CALL LIB$GET_VM(ACLLENGTH+8,ACLSTR)
	CALL MAKE_CHAR(%VAL(ACLSTR),ACLLENGTH)1

	CALL READACL(FILENAME,%VAL(ACLSTR),ACLLENGTH)

	RETURN.
	END


	Q
	SUBROUTINE READACL(FILENAME,ACLENT,ACLLENGTH)
C.
C  SUBROUTINE READACLl
C 
C  FUNCTION: Reads the ACL of a file.O
CR
C  PARAMETERS:
C	FILENAME - Name of file to check.R
C	ACLENT - String which will be large enough to hold ACL information.O
CR
	IMPLICIT INTEGER (A-Z)E

	INCLUDE '($ACLDEF)'

	CHARACTER ACLENT*(*),OUTPUT*80,ACLSTR*255,FILENAME*(*)
	CHARACTER NOT_ID*3
	DATA NOT_ID /'=[,'/

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(ACLLENGTH,ACL$C_READACL,%LOC(ACLENT))
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist

	IER = SYS$CHANGE_ACL(,ACL$C_FILE,FILENAME,%VAL(ACL_ITMLST),,,)O

	DO ACCESS_TYPE=1,2
	 POINT = 1T
	 OUTLEN = 0
	 DO WHILE ((POINT.LT.ACLLENGTH).AND.IER)D
	   IER = SYS$FORMAT_ACL(ACLENT(POINT:POINT-1+
     &		ICHAR(ACLENT(POINT:POINT))),ACLLEN,ACLSTR,,,,)
	   IF ((ACCESS_TYPE.EQ.1.AND.INDEX(ACLSTR,'WRITE').GT.0).OR.E
     &	       (ACCESS_TYPE.EQ.2.AND.INDEX(ACLSTR,'READ)').GT.0)) THENR
	      START_ID = INDEX(ACLSTR,'=') + 1)
	      END_ID = INDEX(ACLSTR,'ACCESS') - 2
	      IF (ACLSTR(END_ID:END_ID).EQ.']') THEND
		 START_ID = END_ID - 1
		 DO WHILED
     &		   (INDEX(NOT_ID,ACLSTR(START_ID:START_ID)).EQ.0)Q
		    START_ID = START_ID - 1 
		 END DOF
		 START_ID = START_ID + 1
		 END_ID = END_ID - 1
		 IF (ACLSTR(START_ID:START_ID).EQ.'*') THENF
		    START_ID = INDEX(ACLSTR,'=') + 1
	            END_ID = INDEX(ACLSTR,'ACCESS') - 2
		 END IFD
	      END IF 
	      IF (OUTLEN.EQ.0) THEN
	         IF (ACCESS_TYPE.EQ.1) THEN
		    WRITE (6,'(I
     &		    '' These users can read and write to this folder:'')')
	         ELSE
		    WRITE (6,'(D
     &		    '' These users can only read this folder:'')')
	         END IF
		 OUTLEN = 1
	      END IF(
	      LEN = END_ID - START_ID + 1
	      IF (OUTLEN+LEN-1.GT.80) THENO
		 WRITE (6,'(1X,A)') OUTPUT(:OUTLEN-1)
		 OUTPUT = ACLSTR(START_ID:END_ID)//','
		 OUTLEN = LEN + 2S
	      ELSE IF (OUTLEN+LEN-1.EQ.80) THEN
		 WRITE (6,'(1X,A)') 
     &			OUTPUT(:OUTLEN-1)//ACLSTR(START_ID:END_ID)e
	         OUTLEN = 1
	      ELSEO
	         OUTPUT(OUTLEN:) = ACLSTR(START_ID:END_ID)//','
		 OUTLEN = OUTLEN + LEN + 1
	      END IFO
	   END IF
	   POINT = POINT + ICHAR(ACLENT(POINT:POINT))
	 END DO
	 IF (OUTLEN.GT.1) WRITE (6,'(1X,A)') OUTPUT(:OUTLEN-2)R
	END DOO

	RETURN 
	END
