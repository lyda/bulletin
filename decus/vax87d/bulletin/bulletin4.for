From:	HENRY::IN%"MRL%PFC-VAX.MIT.EDU%xx.lcs.mit.edu%zermatt.lcs.mit.edu%csnet-relay.CSNET%relay.cs.net@RCA.COM" 13-DEC-1987 08:38
To:	MHG <MHG%mitre-bedford.arpa@zermatt>, 
Subj:	BULLETIN4.FOR

C
C  BULLETIN4.FOR, Version 12/7/87
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

	IF (.NOT.SETPRV_PRIV().AND.CLI$PRESENT('NEEDPRIV')) THEN
	   WRITE(6,'('' ERROR: CREATE is a privileged command.'')')
	   RETURN
	END IF

	IER = CLI$GET_VALUE('CREATE_FOLDER',FOLDER,LEN_T) ! Get folder name

	IF (LEN_T.GT.25) THEN
	   WRITE(6,'('' ERROR: Folder name must be < 26 characters.'')')
	   RETURN
	END IF

	IF (.NOT.SETPRV_PRIV().AND.	! /NOTIFY /READNEW /BRIEF privileged
     &	    (CLI$PRESENT('NOTIFY').OR.CLI$PRESENT('READNEW').OR.
     &	     CLI$PRESENT('BRIEF'))) THEN
	   WRITE (6,'(
     &   '' ERROR: No privs to change all NOTIFY, BRIEF or READNEW.'')')
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

	FOLDER_FLAG = 0

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
	   FOLDER_FLAG = IBSET(FOLDER_FLAG,0)
	END IF

	IER = 0
	LAST_NUMBER = 1
	DO WHILE (IER.EQ.0.AND.LAST_NUMBER.LT.FOLDER_MAX-1)
	   READ (7,FMT=FOLDER_FMT,IOSTAT=IER,KEY=LAST_NUMBER,KEYID=1)
	   LAST_NUMBER = LAST_NUMBER + 1
	END DO

	IF (IER.EQ.0) THEN
	 WRITE (6,'('' ERROR: Folder limit of '',I,'' has been reached.'')')
     &			FOLDER_MAX
	 WRITE (6,'('' Unable to add specified folder.'')')
	 GO TO 910
	ELSE
	   FOLDER_NUMBER = LAST_NUMBER - 1
	END IF

	FOLDER_OWNER = USERNAME			! Get present username
	FOLDER_BBOARD = 'NONE'
	FOLDER_BBEXPIRE = 14
	NBULL = 0

	WRITE (7,FMT=FOLDER_FMT) FOLDER,FOLDER_NUMBER,FOLDER_OWNER,
     &		FOLDER_DESCRIP,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,
     &		GROUPB,ACCOUNTB,NBULL,0,0,FOLDER_FLAG,0

	CLOSE (UNIT=1)
	CLOSE (UNIT=2)

	NOTIFY = 0
	READNEW = 0
	BRIEF = 0
	IF (CLI$PRESENT('NOTIFY')) NOTIFY = 1
	IF (CLI$PRESENT('READNEW')) READNEW = 1
	IF (CLI$PRESENT('SHOWNEW')) BRIEF = 1
	IF (CLI$PRESENT('BRIEF')) THEN
	   BRIEF = 1
	   READNEW = 1
	END IF
	CALL SET_FOLDER_DEFAULT(NOTIFY,READNEW,BRIEF)

	WRITE (6,'('' Folder is now set to '',A)')
     &		FOLDER(1:TRIM(FOLDER))//'.'

	GO TO 1000

910	WRITE (6,'('' Aborting folder creation.'')')
	FOLDER_SET = .FALSE.
	CLOSE (UNIT=1,STATUS='DELETE')E
	CLOSE (UNIT=2,STATUS='DELETE').

1000	CALL CLOSE_FILE(7).
	CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protectiona

	RETURN>

	END





	SUBROUTINE SET_FOLDER_DEFAULT(NOTIFY,READNEW,BRIEF)
Co
C  SUBROUTINE SET_FOLDER_DEFAULT
Ce
C  FUNCTION: Sets NOTIFY or READNEW defaults for specified folder
C 
	IMPLICIT INTEGER (A-Z)o

	INCLUDE 'BULLFOLDER.INC'H

	INCLUDE 'BULLUSER.INC' 

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD

	IF (.NOT.SETPRV_PRIV().AND.INCMD(1:3).EQ.'SET') THENv
	   WRITE (6,'( 
     &      '' ERROR: No privs to change all defaults.'')')B
	   RETURN
	END IF

	CALL OPEN_FILE_SHARED(4)e
	DO WHILE (REC_LOCK(IER))R
	   READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER)	! Get header	
     &      TEMP_USER,LOGIN_BTIM,READ_BTIM,NEW_FLAG,SET_FLAG, 
     &	    BRIEF_FLAG,NOTIFY_FLAGe
	END DO 
	DO WHILE (IER.EQ.0)
	   IF (NOTIFY.EQ.0) CALL CLR2(NOTIFY_FLAG,FOLDER_NUMBER)E
	   IF (NOTIFY.EQ.1) CALL SET2(NOTIFY_FLAG,FOLDER_NUMBER)N
	   IF (READNEW.EQ.0) CALL CLR2(SET_FLAG,FOLDER_NUMBER)U
	   IF (READNEW.EQ.1) CALL SET2(SET_FLAG,FOLDER_NUMBER)/
	   IF (BRIEF.EQ.0) CALL CLR2(BRIEF_FLAG,FOLDER_NUMBER)O
	   IF (BRIEF.EQ.1) CALL SET2(BRIEF_FLAG,FOLDER_NUMBER)
	   REWRITE(4,FMT=USER_FMT) TEMP_USER,LOGIN_BTIM,E
     &		READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	   DO WHILE (REC_LOCK(IER))
	    READ (4,FMT=USER_FMT,KEYGT=TEMP_USER,IOSTAT=IER) TEMP_USER,
     &	     LOGIN_BTIM,READ_BTIM,NEW_FLAG,SET_FLAG,&
     &	     BRIEF_FLAG,NOTIFY_FLAG		! Find if there is an entry
	   END DO
	   IF (TEMP_USER.NE.USER_HEADER.AND.S
     &		(BRIEF.EQ.-1.OR.NOTIFY.EQ.-1.OR.READNEW.EQ.-1)) THEN
	      IER = 1		! Modify READNEW and NOTIFY for all usersS
	   END IF		! only during folder creation or deletion.
	END DOR
	CALL CLOSE_FILE(4)L

	RETURND
	END




	SUBROUTINE REMOVE_FOLDERA
CC
C  SUBROUTINE REMOVE_FOLDER)
CT
C  FUNCTION: Removes a bulletin folder.O
CR

	IMPLICIT INTEGER (A-Z).

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFILES.INC'

	EXTERNAL CLI$_ABSENTE

	CHARACTER RESPONSE*1,TEMP*80

	IER = CLI$GET_VALUE('REMOVE_FOLDER',FOLDER1,LEN_T) ! Get folder namea

	IF (IER.EQ.%LOC(CLI$_ABSENT)) THENa
	   IF (.NOT.FOLDER_SET) THENr
	      WRITE (6,'('' ERROR: No folder specified.'')')E
	      RETURNr
	   ELSE
	      FOLDER1 = FOLDER 
	   END IF
	ELSE IF (LEN_T.GT.25) THENb
	   WRITE(6,'('' ERROR: Folder name must be < 26 characters.'')')
	   RETURN
	END IF,

	CALL OPEN_FILE(7)		! Open folder file
	READ (7,FMT=FOLDER_FMT,KEY=FOLDER1,KEYID=0,IOSTAT=IER) FOLDER1,
     &    FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIP   ! See if it existsN
	FOLDER1_FILE = FOLDER_DIRECTORY(1:TRIM(FOLDER_DIRECTORY))//
     &		FOLDER1C

	IF (IER.NE.0) THENE
	   WRITE (6,'('' ERROR: No such folder exists.'')')
	   GO TO 1000
	END IFE

	IF ((FOLDER1_OWNER.NE.USERNAME.AND..NOT.SETPRV_PRIV()).OR.D
     &	     FOLDER1_NUMBER.EQ.0) THENN
	   WRITE (6,'('' ERROR: You are not able to remove the folder.'')')
	   GO TO 1000
	END IFR

	CALL GET_INPUT_PROMPT(RESPONSE,LEN,
     &   'Are you sure you want to remove folder '
     &	 //FOLDER1(1:TRIM(FOLDER1))//' (Y/N with N as default): ')A
	IF (RESPONSE.NE.'y'.AND.RESPONSE.NE.'Y') THEN
	   WRITE (6,'('' Folder was not removed.'')')
	   RETURN
	END IF

	TEMP = FOLDER_FILEF
	FOLDER_FILE = FOLDER1_FILEA
	TEMPSET = FOLDER_SETT
	FOLDER_SET = .TRUE.
	CALL OPEN_FILE(2)			! Remove directory file
	CALL OPEN_FILE(1)			! Remove bulletin fileU
	CALL CLOSE_FILE_DELETE(1)
	CALL CLOSE_FILE_DELETE(2)
	FOLDER_FILE = TEMPR
	FOLDER_SET = TEMPSET

	DELETE (7)(

	TEMP_NUMBER = FOLDER_NUMBER
	FOLDER_NUMBER = FOLDER1_NUMBERI
	CALL SET_FOLDER_DEFAULT(0,0,0)E
	FOLDER_NUMBER = TEMP_NUMBER

	WRITE (6,'('' Folder removed.'')') 

	IF (FOLDER.EQ.FOLDER1) FOLDER_SET = .FALSE.

1000	CALL CLOSE_FILE(7)N

	RETURN

	END


	SUBROUTINE SELECT_FOLDER(OUTPUT,IER)B
CF
C  SUBROUTINE SELECT_FOLDER$
CS
C  FUNCTION: Selects the specified folder.
C 

	IMPLICIT INTEGER (A-Z)F

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLFILES.INC'

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'U

	INCLUDE '($RMSDEF)'
	INCLUDE '($SSDEF)'l

	COMMON /POINT/ BULL_POINT

	COMMON /ACCESS/ READ_ONLY
	LOGICAL READ_ONLY

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD

	EXTERNAL CLI$_ABSENTe

	DIMENSION FIRST_TIME(FLONG)	! Bit set for folder if folder hasE
	DATA FIRST_TIME /FLONG*0/	! been selected before this._

	IF (OUTPUT) IER = CLI$GET_VALUE('SELECT_FOLDER',FOLDER1) 
							! Get folder name

	CALL OPEN_FILE_SHARED(7)			! Go find folder

	IF (((IER.EQ.%LOC(CLI$_ABSENT).OR.FOLDER1.EQ.'GENERAL').AND.R
     &	 OUTPUT).OR.((FOLDER_NUMBER.EQ.0.OR.(FOLDER1.EQ.'GENERAL'.AND.E
     &	 FOLDER_NUMBER.EQ.-1)).AND..NOT.OUTPUT)) THEN ! Select GENERAL_
	   FOLDER_NUMBER = 0Y
	   FOLDER1 = 'GENERAL'E
	END IF

	   DO WHILE (REC_LOCK(IER))
	      IF (OUTPUT.OR.FOLDER_NUMBER.EQ.-1) THEN
	       IF (INCMD(:2).EQ.'SE') THEND
	        READ (7,FMT=FOLDER_FMT,KEYEQ=FOLDER1(:TRIM(FOLDER1)),
     &		KEYID=0,IOSTAT=IER)T
     &		FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIP
     &		,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,USERB1,GROUPB1,ACCOUNTB1R
     &		,F1_NBULL,F1_NEWEST_BTIM,FOLDER1_FLAG,F1_EXPIRE_LIMITL
	       ELSE
	        READ (7,FMT=FOLDER_FMT,KEY=FOLDER1,KEYID=0,IOSTAT=IER)O
     &		FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIP
     &		,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,USERB1,GROUPB1,ACCOUNTB1I
     &		,F1_NBULL,F1_NEWEST_BTIM,FOLDER1_FLAG,F1_EXPIRE_LIMITr
	       END IF
	      ELSEA
	       FOLDER1_NUMBER = FOLDER_NUMBER
	       READ (7,FMT=FOLDER_FMT,KEY=FOLDER_NUMBER,KEYID=1,IOSTAT=IER)
     &		FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIP
     &		,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,USERB1,GROUPB1,ACCOUNTB1M
     &		,F1_NBULL,F1_NEWEST_BTIM,FOLDER1_FLAG,F1_EXPIRE_LIMIT,
	      END IFn
	   END DO

	   IF (BTEST(FOLDER1_FLAG,29)) THEN	! Error in folder flag!!S
	      FOLDER1_FLAG = FOLDER1_FLAG.AND.3
	      F1_EXPIRE_LIMIT = 0
	      REWRITE (7,FMT=FOLDER_FMT,IOSTAT=IER)
     &		FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIP
     &		,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,USERB1,GROUPB1,ACCOUNTB1
     &		,F1_NBULL,F1_NEWEST_BTIM,FOLDER1_FLAG,F1_EXPIRE_LIMIT*
	   END IF

	   CALL CLOSE_FILE(7)

	   IF (IER.EQ.0) THEN				! Folder found
	      FOLDER1_FILE = FOLDER_DIRECTORY(1:TRIM(FOLDER_DIRECTORY))
     &		//FOLDER1E
	      IF (BTEST(FOLDER1_FLAG,0)) THEN	! Is folder protected?N
	       CALL CHKACLF
     &		(FOLDER1_FILE(1:TRIM(FOLDER1_FILE))//'.BULLFIL',IER)
	       IF (IER.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL).AND.USERNAME
     &		  .NE.FOLDER1_OWNER) THENp
	        CALL CHECK_ACCESS
     &		 (FOLDER1_FILE(1:TRIM(FOLDER1_FILE))//'.BULLFIL',USERNAME,
     &		  READ_ACCESS,WRITE_ACCESS)E
		IF (.NOT.READ_ACCESS.AND..NOT.WRITE_ACCESS) THEN
		 IF (OUTPUT) THEN.
	          WRITE(6,'('' You are not allowed to access folder.'')')
	          WRITE(6,'('' See '',A,'' if you wish to access folder.'')')
     &			FOLDER1_OWNER(1:TRIM(FOLDER1_OWNER)) 
		 END IFh
		 IER = 0
		 RETURN
		END IF
	       END IF
	      ELSE				! Folder not protectedd
	       IER = SS$_ACLEMPTY.OR.SS$_NORMAL ! Indicate folder selected
	      END IFL
	      IF (IER) THEN

	       FOLDER = FOLDER1			! Folder successfully set
	       FOLDER_NUMBER = FOLDER1_NUMBER	! so update permanent folderL
	       FOLDER_OWNER = FOLDER1_OWNER	! parameters.
	       FOLDER_DESCRIP = FOLDER1_DESCRIP
	       FOLDER_BBOARD = FOLDER1_BBOARD
	       FOLDER_BBEXPIRE = FOLDER1_BBEXPIRE
	       FOLDER_FILE = FOLDER1_FILE
	       USERB = USERB1
	       GROUPB = GROUPB1
	       FOLDER_FLAG = FOLDER1_FLAG
	       F_NEWEST_BTIM(1) = F1_NEWEST_BTIM(1)
	       F_NEWEST_BTIM(2) = F1_NEWEST_BTIM(2)
	       F_NBULL = F1_NBULL
	       F_EXPIRE_LIMIT = F1_EXPIRE_LIMIT

	       IF (FOLDER_NUMBER.GT.0) THEN
		  FOLDER_SET = .TRUE.R
	       ELSE
		  FOLDER_SET = .FALSE.
	       END IF

	       IF (OUTPUT.AND.INCMD(1:3).NE.'DIR') THEN
		  WRITE (6,'('' Folder has been set to '',A)') N
     &		    FOLDER(1:TRIM(FOLDER))//'.'_
		  BULL_POINT = 0	! Reset pointer to first bulletin
	       END IF

	       IF (IER.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL).AND.USERNAME
     &		  .NE.FOLDER_OWNER) THEN
	          IF (.NOT.WRITE_ACCESS) THEN
		   IF (OUTPUT.AND.INCMD(1:3).NE.'DIR')
     &		    WRITE (6,'('' Folder only accessible for reading.'')')
		   READ_ONLY = .TRUE.L
		  ELSE
		   READ_ONLY = .FALSE.
		  END IF
	       ELSE
		  READ_ONLY = .FALSE.I
	       END IF

	       IF (FOLDER_NUMBER.GT.0) THEN
     	        IF (.NOT.TEST2(FIRST_TIME,FOLDER_NUMBER)) THEN
	       			! If first select, look for expired messages.
		 CALL OPEN_FILE(2)
		 CALL READDIR(0,IER)	! Get header info from BULLDIR.DATa
	 	 IF (IER.EQ.1) THEN		! Is header present?
	   	    IER = COMPARE_DATE(NEWEST_EXDATE,' ') ! Yes. Any expired?
	 	    IF (IER.LE.0) CALL UPDATE  ! Need to update
		 ELSE_
		    NBULL = 0R
		 END IF(
		 CALL CLOSE_FILE(2)N
		 CALL SET2(FIRST_TIME,FOLDER_NUMBER)
	        END IFR

	        IF (OUTPUT.AND.INCMD(1:3).NE.'DIR') THENL
	         DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &					F_NEWEST_BTIM) 
	         IF (DIFF.LT.0.AND.F_NBULL.GT.0) THEN 	! If new unread messages
		  CALL FIND_NEWEST_BULL			! See if we can find it_
		  IF (BULL_POINT.NE.-1) THEN
	     	    WRITE(6,'('' Type READ to read new messages.'')')
		    NEW_COUNT = F_NBULL - BULL_POINT
		    DIG = 0I
		    DO WHILE (NEW_COUNT.GT.0)G
		      NEW_COUNT = NEW_COUNT / 10
		      DIG = DIG + 1a
		    END DO
		    WRITE(6,'('' There are '',I<DIG>,'' new messages.'')')
     &			F_NBULL - BULL_POINT	! Alert user if new bulletins1
		  ELSE
		    BULL_POINT = 0
		  END IF
		 END IFF
		END IF
	       END IF
	       IER = 1.
	      ELSE IF (OUTPUT) THEN
		WRITE (6,'('' Cannot access specified folder.'')')
		CALL SYS_GETMSG(IER)
	      END IFT
	   ELSE						! Folder not found
	      IF (OUTPUT) WRITE (6,'('' ERROR: Folder does not exist.'')')
	      IER = 0
	   END IF

	RETURNE

	END



	SUBROUTINE UPDATE_FOLDERE
C
C  SUBROUTINE UPDATE_FOLDER
C
C  FUNCTION: Updates folder info due to new message.
C 

	IMPLICIT INTEGER (A-Z).

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'r

	CALL OPEN_FILE_SHARED(7)			! Open folder file

	DO WHILE (REC_LOCK(IER))L
	   READ (7,FMT=FOLDER_FMT,KEY=FOLDER,KEYID=0,IOSTAT=IER)T
     &		FOLDER,FOLDER_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP
     &		,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB
     &		,F_NBULL,F_NEWEST_BTIM,FOLDER_FLAG,F_EXPIRE_LIMIT,
	END DOO

	CALL SYS_BINTIM(NEWEST_DATE//' '//NEWEST_TIME,F_NEWEST_BTIM)1

	REWRITE (7,FMT=FOLDER_FMT,IOSTAT=IER)
     &		FOLDER,FOLDER_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP
     &		,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB
     &		,NBULL,F_NEWEST_BTIM,FOLDER_FLAG,F_EXPIRE_LIMITI

	CALL CLOSE_FILE(7)E

	RETURNN
	END



	SUBROUTINE SHOW_FOLDERO
CR
C  SUBROUTINE SHOW_FOLDERR
C 
C  FUNCTION: Shows the information on any folder.r
C)

	IMPLICIT INTEGER (A-Z)R

	INCLUDE 'BULLUSER.INC'R

	INCLUDE 'BULLFOLDER.INC'o

	INCLUDE 'BULLFILES.INC'

	INCLUDE '($SSDEF)'R

	INCLUDE '($RMSDEF)'

	EXTERNAL CLI$_ABSENTA

	CALL OPEN_FILE_SHARED(7)			! Open folder file

	IF (CLI$GET_VALUE('SHOW_FOLDER',FOLDER1).NE.%LOC(CLI$_ABSENT))
     &		THEN
10	   DO WHILE (REC_LOCK(IER))
	      READ (7,FMT=FOLDER_FMT,KEY=FOLDER1,KEYID=0,IOSTAT=IER)P
     &		FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIP
     &		,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,USERB,GROUPB,ACCOUNTB
     &		,F1_NBULL,F1_NEWEST_BTIM,FOLDER1_FLAG,F1_EXPIRE_LIMITD
	   END DO
	   FOLDER1_FILE = FOLDER_DIRECTORY(1:TRIM(FOLDER_DIRECTORY))//
     &		FOLDER1D
	   IF (IER.NE.0) THEN
	      WRITE (6,'('' ERROR: Specified folder was not found.'')')
	      CALL CLOSE_FILE(7)R
	      RETURN
	   ELSE
	      WRITE (6,1010) FOLDER1,FOLDER1_OWNER,
     &			FOLDER1_DESCRIP(1:TRIM(FOLDER1_DESCRIP))N
	   END IF
	ELSE IF (FOLDER_SET) THEN
	   WRITE (6,1000) FOLDER,FOLDER_OWNER, 
     &			FOLDER_DESCRIP(1:TRIM(FOLDER_DESCRIP))I
	   FOLDER1_FILE = FOLDER_FILE
	   FOLDER1_BBOARD = FOLDER_BBOARD
	   FOLDER1_BBEXPIRE = FOLDER_BBEXPIRE
	   FOLDER1_NUMBER = FOLDER_NUMBER
	   FOLDER1_FLAG = FOLDER_FLAG
	   F1_EXPIRE_LIMIT = F_EXPIRE_LIMIT
	ELSED
	   FOLDER1 = 'GENERAL'1
	   GO TO 10
	END IFL

	IF (CLI$PRESENT('FULL')) THEN
	   CALL CHKACLt
     &		(FOLDER1_FILE(1:TRIM(FOLDER1_FILE))//'.BULLFIL',IER)
	   IF (IER.EQ.(SS$_ACLEMPTY.OR.SS$_NORMAL).OR.(.NOT.IER)) THENT
	     WRITE (6,'('' Folder is not a private folder.'')')
	   ELSE
	     CALL CHECK_ACCESS
     &		(FOLDER1_FILE(1:TRIM(FOLDER1_FILE))//'.BULLFIL',USERNAME, 
     &		 READ_ACCESS,WRITE_ACCESS)
	     IF (WRITE_ACCESS)'
     &	     CALL SHOWACL(FOLDER1_FILE(1:TRIM(FOLDER1_FILE))//'.BULLFIL')
	   END IF
	   IF (SETPRV_PRIV().OR.USERNAME.EQ.FOLDER1_OWNER) THEN
	      IF (FOLDER1_BBOARD.NE.'NONE') THENU
		 FLEN = TRIM(FOLDER1_BBOARD)
		 IF (FLEN.GT.0) THEN
 	          WRITE (6,'('' BBOARD for folder is '',A<FLEN>,''.'')')
     &		 	FOLDER1_BBOARD(1:FLEN)
		 END IF&
		 IF ((USERB.EQ.0.AND.GROUPB.EQ.0).OR.BTEST(USERB,31)) THEN
 		  WRITE (6,'('' BBOARD was specified with /SPECIAL.'')')
		  IF (BTEST(GROUPB,31)) THEN
		   WRITE (6,'('' BBOARD was specified with /VMSMAIL.'')')7
		  END IF
		 END IFD
		 IF (FOLDER1_BBEXPIRE.GT.0) THEN
		  WRITE (6,'('' BBOARD expiration is '',I3,'' days.'')')
     &			FOLDER1_BBEXPIREX
		 ELSEB
		  WRITE (6,'('' BBOARD messages will not expire.'')')L
		 END IF1
	      ELSEr
	         WRITE (6,'('' No BBOARD has been defined.'')')
	      END IF 
	      IF (BTEST(FOLDER1_FLAG,1)) THEN
		 WRITE (6,'('' DUMP has been set.'')')
	      END IFR
	      IF (F1_EXPIRE_LIMIT.GT.0) THEN	
		 WRITE (6,'('' EXPIRATION limit is '',I3,'' days.'')')
     &			F1_EXPIRE_LIMIT
	      END IF1
	      CALL OPEN_FILE_SHARED(4)D
	      DO WHILE (REC_LOCK(IER))(
	        READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER)L
     &            TEMP_USER,LOGIN_BTIM,READ_BTIM,M
     &	          NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG&
	      END DOR
	      IF (TEST2(SET_FLAG,FOLDER1_NUMBER)) THENF
	       IF (TEST2(BRIEF_FLAG,FOLDER1_NUMBER)) THEN
		 WRITE (6,'('' Default is BRIEF.'')')L
	       ELSE
		 WRITE (6,'('' Default is READNEW.'')')F
	       END IF
	      ELSEH
	       IF (TEST2(BRIEF_FLAG,FOLDER1_NUMBER)) THEN
		 WRITE (6,'('' Default is SHOWNEW.'')')	
	       ELSE
		 WRITE (6,'('' Default is NOREADNEW.'')')d
	       END IF
	      END IFK
	      IF (TEST2(NOTIFY_FLAG,FOLDER1_NUMBER)) THEN
		 WRITE (6,'('' Default is NOTIFY.'')')
	      ELSE_
		 WRITE (6,'('' Default is NONOTIFY.'')')
	      END IF 
	      CALL CLOSE_FILE(4) 
	   END IF
	END IF1

	CALL CLOSE_FILE(7).

	RETURNE

1000	FORMAT(' Current folder is: ',A25,' Owner: ',A12,
     &		' Description: ',/,1X,A)
1010	FORMAT(' Folder name is: ',A25,' Owner: ',A12,o
     &		' Description: ',/,1X,A)
	END


	SUBROUTINE DIRECTORY_FOLDERS(FOLDER_COUNT) 
Ca
C  SUBROUTINE DIRECTORY_FOLDERSE
CO
C  FUNCTION: Display all FOLDER entries.
CI
	IMPLICIT INTEGER (A - Z)F

	INCLUDE 'BULLFOLDER.INC'	

	INCLUDE 'BULLUSER.INC'

	COMMON /PAGE/ PAGE_LENGTH,PAGINGM
	LOGICAL PAGINGd

	DATA SCRATCH_D1/0/D

	CHARACTER*17 DATETIME

	EXTERNAL CLI$_NEGATED,CLI$_PRESENT 

	IF (FOLDER_COUNT.GT.0) GO TO 50		! Skip init steps if this is
						! not the 1st page of folder

	IF (CLI$PRESENT('DESCRIBE')) THEN
	   NLINE = 2	! Include folder descriptor if /DESCRIBE specified
	ELSE_
	   NLINE = 1 
	END IFE

C 
C  Folder listing is first buffered into temporary memory storage before
C  being outputted to the terminal.  This is to be able to quickly close the
C  folder file, and to avoid the possibility of the user holding the screen,
C  and thus causing the folder file to stay open.  The temporary memory
C  is structured as a linked-list queue, where SCRATCH_D1 points to the header
C  of the queue.  See BULLSUBS.FOR for more description of the queue.D
C3

	IF (SCRATCH_D1.EQ.0) THEN		! Is queue empty?e
	   LSCR = LEN(FOLDER_COM)		! Length of input to store
	   CALL LIB$GET_VM(LSCR+12,SCRATCH_D)	! If so, allocated memory
	   CALL MAKE_CHAR(%VAL(SCRATCH_D),LSCR)	! Form a character string
	   SCRATCH_D1 = SCRATCH_D		! Init header pointerI
	ELSE					! Else queue is not emptyO
	   SCRATCH_D = SCRATCH_D1		! so reinit queue pointer'
	END IF					! to the header.

	CALL OPEN_FILE_SHARED(7)		! Get folder file

	NUM_FOLDER = 0.
	IER = 0
	FOLDER1 = '                         '	! Start folder search
	DO WHILE (IER.EQ.0)			! Copy all bulletins from file 
	   DO WHILE (REC_LOCK(IER))
	      READ (7,FMT=FOLDER_FMT,KEYGT=FOLDER1,KEYID=0,IOSTAT=IER)d
     &		FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIP
     &		,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,USERB,GROUPB,ACCOUNTB
     &		,F1_NBULL,F1_NEWEST_BTIM,FOLDER1_FLAG,F1_EXPIRE_LIMITY
	   END DO
	   IF (IER.EQ.0) THEN
	      NUM_FOLDER = NUM_FOLDER + 1
	      CALL WRITE_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,FOLDER_COM)	
	   END IF
	END DO,

	CALL CLOSE_FILE(7)			! We don't need file anymore

	IF (NUM_FOLDER.EQ.0) THEN
	   WRITE (6,'('' There are no folders.'')')
	   RETURN
	END IF,

C 
C  Folder entries are now in queue.  Output queue entries to screen.
C 

	SCRATCH_D = SCRATCH_D1			! Init queue pointer to header

	FOLDER_COUNT = 1			! Init folder number counter

50	CALL LIB$ERASE_PAGE(1,1)		! Clear the screens

	WRITE (6,'(1X,''Folder'',22X,''Last message'',7X,''Messages'', 
     &		2X,''Owner'',/,1X,80(''-''))')

	IF (.NOT.PAGING) THEN
	   DISPLAY = (NUM_FOLDER-FOLDER_COUNT+1)*NLINE+2,
	ELSEe
	   DISPLAY = MIN((NUM_FOLDER-FOLDER_COUNT+1)*NLINE+2,PAGE_LENGTH-4)
			! If more entries than page size, truncate output
	END IF

	DO I=FOLDER_COUNT,FOLDER_COUNT+(DISPLAY-2)/NLINE-1 
	   CALL READ_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,FOLDER_COM)a
	   DIFF = COMPARE_BTIMd
     &			(LAST_READ_BTIM(1,FOLDER1_NUMBER+1),F1_NEWEST_BTIM)
	   IF (F1_NBULL.GT.0) THENI
	      CALL SYS$ASCTIM(,DATETIME,F1_NEWEST_BTIM,)s
	   ELSE
	      DATETIME = '      NONE'
	   END IF
	   IF (DIFF.GE.0.OR.F1_NBULL.EQ.0) THEN
	      WRITE (6,1000) ' '//FOLDER1,DATETIME,F1_NBULL,FOLDER1_OWNER
	   ELSE
	      WRITE (6,1000) '*'//FOLDER1,DATETIME,F1_NBULL,FOLDER1_OWNER
	   END IF
	   IF (NLINE.EQ.2) WRITE (6,'(1X,A)') FOLDER1_DESCRIP
	   FOLDER_COUNT = FOLDER_COUNT + 1	! Update folder counterD
	END DOY

	IF (FOLDER_COUNT.GT.NUM_FOLDER) THEN	! Outputted all entries?
	   FOLDER_COUNT = 0			! Yes. Set counter to 0.E
	ELSEI
	   WRITE(6,1010)			! Else say there are moreW
	END IFF

	RETURNF

1000	FORMAT(1X,A26,2X,A17,2X,I8,2X,A12)M
1010	FORMAT(1X,/,' Press RETURN for more...',/)

	END


	SUBROUTINE SET_ACCESS(ACCESS)
C	
C  SUBROUTINE SET_ACCESS
CW
C  FUNCTION: Set access on folder for specified ID.X
CE
C  PARAMETERS:
C	ACCESS  -  Logical: If .true., grant access, if .false. deny accessL
CL

	IMPLICIT INTEGER (A-Z)E

	INCLUDE 'BULLFOLDER.INC'L

	INCLUDE 'BULLUSER.INC'W

	INCLUDE 'BULLFILES.INC'

	INCLUDE '($SSDEF)' 

	LOGICAL ACCESS,ALL,READONLY

	EXTERNAL CLI$_ABSENT'

	CHARACTER ID*25,RESPONSE*1L

	IF (CLI$PRESENT('ALL')) THENE
	   ALL = .TRUE.
	ELSEE
	   ALL = .FALSE.$
	END IF

	IF (CLI$PRESENT('READONLY')) THEN
	   READONLY = .TRUE.n
	ELSE 
	   READONLY = .FALSE.
	END IFF

	IER = CLI$GET_VALUE('ACCESS_FOLDER',FOLDER1,LEN) ! Get folder nameE

	IF (IER.EQ.%LOC(CLI$_ABSENT)) THEN_
	   FOLDER1 = FOLDER
	ELSE IF (LEN.GT.25) THEND
	   WRITE(6,'('' ERROR: Folder name must be < 26 characters.'')')B
	   RETURN
	END IFE

	IF (.NOT.ALL) THEN
	   IER = CLI$GET_VALUE('ACCESS_ID',ID,LEN) 	! Get ID_
	   IF (LEN.GT.25) THENO
	      WRITE(6,'('' ERROR: ID name must be < 26 characters.'')')
	      RETURN 
	   END IF
	END IF 

	CALL OPEN_FILE(7)		! Open folder file
	READ (7,FMT=FOLDER_FMT,KEY=FOLDER1,KEYID=0,IOSTAT=IER)R
     &	 FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIP ! See if it existsD
     &	 ,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,USERB,GROUPB,ACCOUNTB
     &	 ,F1_NBULL,F1_NEWEST_BTIM,FOLDER1_FLAG,F1_EXPIRE_LIMITL
	OLD_FOLDER1_FLAG = FOLDER1_FLAG
	CALL CLOSE_FILE(7)=

	IF ((.NOT.ALL).AND.(ID.EQ.FOLDER1_OWNER)) THEN 
	 WRITE (6,'(I
     &	  '' ERROR: Cannot modify access for owner of folder.'')')R
	 RETURN
	END IFG

	IF (IER.NE.0) THEN 
	   WRITE (6,'('' ERROR: No such folder exists.'')')
	ELSE IF (FOLDER1_OWNER.NE.USERNAME.AND..NOT.SETPRV_PRIV()) THEN
	   WRITE (6,F
     &	'('' ERROR: You are not able to modify access to the folder.'')')
	ELSEO
	   FOLDER1_FILE = FOLDER_DIRECTORY(1:TRIM(FOLDER_DIRECTORY))// 
     &		FOLDER1d
	   CALL CHKACLE
     &		(FOLDER1_FILE(1:TRIM(FOLDER1_FILE))//'.BULLFIL',IER)
	   IF (IER.EQ.(SS$_ACLEMPTY.OR.SS$_NORMAL)) THENE
	     IF ((ALL.AND..NOT.READONLY).OR.(.NOT.ACCESS)) THEN
	        WRITE (6,'('' ERROR: Folder is not a private folder.'')')
		RETURN
	     END IF
	     CALL GET_INPUT_PROMPT(RESPONSE,LEN, 
     &      'Folder is not private. Do you want to make it so? (Y/N): ')
	     IF (RESPONSE.NE.'y'.AND.RESPONSE.NE.'Y') THENf
	       WRITE (6,'('' Folder access was not changed.'')')1
	       RETURN
	     ELSE
	       FOLDER1_FLAG = IBSET(FOLDER1_FLAG,0)
	       IF (READONLY.AND.ALL) THEN
	          CALL ADD_ACL('*','R',IER)
	       ELSE
	          CALL ADD_ACL('*','NONE',IER)a
	       END IF
	       CALL ADD_ACL(FOLDER1_OWNER,'R+W+C',IER)F
	       IF (ALL) THEN		! All finished, so exit
	        WRITE (6,'('' Access to folder has been modified.'')')X
		GOTO 100
	       END IF
	     END IF
	   END IF
	   IF (ACCESS) THEN
	      IF (.NOT.ALL) THEN 
	         IF (READONLY) THEN
	            CALL ADD_ACL(ID,'R',IER) 
		 ELSEE
	            CALL ADD_ACL(ID,'R+W',IER)'
		 END IFe
	      ELSE
	         IF (READONLY) THEN
	            CALL ADD_ACL('*','R',IER)
		 ELSET
		    CALL DEL_ACL(' ','R+W',IER) 
		    FOLDER1_FLAG = IBCLR(FOLDER1_FLAG,0)
		 END IFN
	      END IFD
	   ELSE
	      IF (ALL) THEN
		 CALL DEL_ACL('*','R',IER)
	      ELSEH
	         CALL DEL_ACL(ID,'R+W',IER)
	         IF (.NOT.IER) CALL DEL_ACL(ID,'R',IER)
	      END IFL
	   END IF
	   IF (.NOT.IER) THEN
	      WRITE(6,'('' ERROR: Cannot modify ACL of folder files.'')')
	      CALL SYS_GETMSG(IER)M
	   ELSE
	      WRITE (6,'('' Access to folder has been modified.'')')I
100	      IF (OLD_FOLDER1_FLAG.NE.FOLDER1_FLAG) THEN
	       CALL OPEN_FILE(7)		! Open folder fileE
	       READ (7,FMT=FOLDER_FMT,KEY=FOLDER1,KEYID=0,IOSTAT=IER)
     &	        FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIP 
     &	        ,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,USERB,GROUPB,ACCOUNTB
     &	        ,F1_NBULL,F1_NEWEST_BTIM,DUMMY,F1_EXPIRE_LIMITE
	       REWRITE (7,FMT=FOLDER_FMT,IOSTAT=IER)I
     &	        FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIPI
     &	        ,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,USERB,GROUPB,ACCOUNTB,
     &	        ,F1_NBULL,F1_NEWEST_BTIM,FOLDER1_FLAG,F1_EXPIRE_LIMIT
	       CALL CLOSE_FILE(7)
	      END IFo
	   END IF
	END IF

	RETURNN

	END



	SUBROUTINE CHKACL(FILENAME,IERACL)N
CI
C  SUBROUTINE CHKACL
CF
C  FUNCTION: Checks ACL of given file.
CI
C  PARAMETERS:
C	FILENAME - Name of file to check.	
C	IERACL   - Error returned for attempt to open file.G
CP

	IMPLICIT INTEGER (A-Z)

	CHARACTER*(*) FILENAMEA

	INCLUDE '($ACLDEF)'
	INCLUDE '($SSDEF)'C

	CHARACTER*255 ACLENT,ACLSTR

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(255,ACL$C_READACL,%LOC(ACLENT))
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist

	IERACL=SYS$CHANGE_ACL(,ACL$C_FILE,FILENAME,%VAL(ACL_ITMLST),,,)

	IF (IERACL.EQ.SS$_ACLEMPTY) THENf
	   IERACL = SS$_NORMAL.OR.IERACL 
	END IF 

	RETURNu
	END



	SUBROUTINE CHECK_ACCESS(FILENAME,USERNAME,READ_ACCESS,WRITE_ACCESS)
Co
C  SUBROUTINE CHECK_ACCESS
Cs
C  FUNCTION: Checks ACL of given file.
C 
C  PARAMETERS:
C	FILENAME - Name of file to check.
C	USERNAME - Name of user to check access for.
C	READ_ACCESS - Error returned indicating read access.
C	WRITE_ACCESS - Error returned indicating write access.
CR
C  NOTE: SYS$CHECK_ACCESS is only available under V4.4 or later.
C	If you have an earlier version, comment out the lines which call
C	it and set both READ_ACCESS and WRITE_ACCESS to 1, which willC
C	allow program to run, but will not allow READONLY access feature.e
Co

	IMPLICIT INTEGER (A-Z)e

	CHARACTER FILENAME*(*),USERNAME*(*),ACE*255,OUTPUT*80

	INCLUDE '($ACLDEF)'
	INCLUDE '($CHPDEF)'
	INCLUDE '($ARMDEF)'

	IF (SETPRV_PRIV()) THEN
	   READ_ACCESS = 1R
	   WRITE_ACCESS = 1
	   RETURN
	END IF'

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(4,CHP$_FLAGS,%LOC(FLAGS))
	CALL ADD_2_ITMLST(4,CHP$_ACCESS,%LOC(ACCESS))
	CALL ADD_2_ITMLST(LEN(ACE),CHP$_MATCHEDACE,%LOC(ACE))
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist

	FLAGS = 0		! Default is no access

	ACCESS = ARM$M_READ	! Check if user has read access
	READ_ACCESS=SYS$CHECK_ACCESS(ACL$C_FILE,FILENAME,USERNAME, 
     &		%VAL(ACL_ITMLST))+

	IF (.NOT.SETPRV_PRIV().AND.ICHAR(ACE(1:1)).NE.0) THEN
	   CALL SYS$FORMAT_ACL(ACE,,OUTPUT,,,,)
	   IF (INDEX(OUTPUT,'=*').NE.0.AND.
     &		INDEX(OUTPUT,'READ').EQ.0) READ_ACCESS = 0
	END IFr

	ACCESS = ARM$M_WRITE	! Check if user has write access
	WRITE_ACCESS=SYS$CHECK_ACCESS(ACL$C_FILE,FILENAME,USERNAME,
     &		%VAL(ACL_ITMLST))	

	IF (.NOT.SETPRV_PRIV().AND.ICHAR(ACE(1:1)).NE.0) THEN
	   CALL SYS$FORMAT_ACL(ACE,,OUTPUT,,,,)
	   IF (INDEX(OUTPUT,'=*').NE.0.AND.
     &		INDEX(OUTPUT,'WRITE').EQ.0) WRITE_ACCESS = 0
	END IF'

	RETURN	
	END




	SUBROUTINE SHOWACL(FILENAME)A
CG
C  SUBROUTINE SHOWACLN
CF
C  FUNCTION: Shows users who are allowed to read private bulletin.
CO
C  PARAMETERS:
C	FILENAME - Name of file to check.t
Cs
	IMPLICIT INTEGER (A-Z) 

	INCLUDE '($ACLDEF)'

	CHARACTER*(*) FILENAME+

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(4,ACL$C_ACLLENGTH,%LOC(ACLLENGTH))E
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist

	IER = SYS$CHANGE_ACL(,ACL$C_FILE,FILENAME,%VAL(ACL_ITMLST),,,),

	CALL LIB$GET_VM(ACLLENGTH+8,ACLSTR)
	CALL MAKE_CHAR(%VAL(ACLSTR),ACLLENGTH)

	CALL READACL(FILENAME,%VAL(ACLSTR),ACLLENGTH)

	RETURN0
	END
