C
C  BULLETIN1.FOR, Version 9/26/89
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

	CALL OPEN_BULLDIR_SHARED

	CALL READDIR(BULL_POINT,IER)	! Get info for specified bulletin

	IF (IER.NE.BULL_POINT+1) THEN	! Was bulletin found?
	   WRITE(6,'('' ERROR: Specified message was not found.'')')
	   CALL CLOSE_BULLDIR		! If not, then error out
	   RETURN
	END IF

	CALL CLOSE_BULLDIR

	OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,
     &	   RECL=LINE_LENGTH,STATUS='NEW',CARRIAGECONTROL='LIST')

	IF (IER.NE.0) THEN
	   WRITE(6,'('' ERROR: Error in opening scratch file.'')')
	   RETURN
	END IF

	IF (CLI$PRESENT('HEADER')) THEN		! Printout header?
	   IF (EXDATE(8:9).EQ.'18'.OR.INDEX(EXDATE,'1900').GT.0) THEN
	      INPUT = 'Date:   '//DATE//' '//TIME(:5)//'   (DELETED)'
	   ELSE IF ((SYSTEM.AND.4).EQ.4) THEN	! Is entry shutdown bulletin?
	      INPUT = 'Date:   '//DATE//' '//TIME(:5)//'   Expires on shutdown'
	   ELSE IF ((SYSTEM.AND.2).EQ.2) THEN	! Is entry permanent bulletin?
	      INPUT = 'Date:   '//DATE//' '//TIME(:5)//'   Permanent'
	   ELSE
	      INPUT = 'Date:   '//DATE//' '//TIME(:5)//
     &				'   Expires:   '//EXDATE//' '//EXTIME(:5)
	   END IF
	   IF ((SYSTEM.AND.1).EQ.1) THEN		! System bulletin?
	      INPUT = INPUT(:TRIM(INPUT))//' / System'
	   END IF
	   WRITE (3,'(A)') INPUT(:TRIM(INPUT))
	END IF

	HEAD = CLI$PRESENT('HEADER')

	CALL OPEN_BULLFIL_SHARED	! Open BULLETIN file

	ILEN = LINE_LENGTH + 1

	CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN
	   IF (HEAD) WRITE(3,1060) INPUT(7:ILEN)
	   CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	ELSE IF (HEAD) THEN
	   WRITE(3,1060) FROM
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

	CLOSE (UNIT=3)			! Message copy completed

	CALL CLOSE_BULLFIL

	LEN_D = TRIM(MAIL_SUBJECT)
	IF (LEN_D.EQ.0) THEN
	   MAIL_SUBJECT = 'BULLETIN message.'
	   LEN_D = TRIM(MAIL_SUBJECT)
	END IF

	I = 1
	DO WHILE (I.LE.LEN_D)
	   IF (MAIL_SUBJECT(I:I).EQ.'"') THEN
	      IF (LEN_D.EQ.64) THEN
		 MAIL_SUBJECT(I:I) = '`'
	      ELSE
		 MAIL_SUBJECT = MAIL_SUBJECT(:I)//'"'//MAIL_SUBJECT(I+1:)
		 I = I + 1
		 LEN_D = LEN_D + 1
	      END IF
	   END IF
	   I = I + 1
	END DO

	LEN_P = 0
	DO WHILE (CLI$GET_VALUE('RECIPIENTS',BULL_PARAMETER(LEN_P+1:),I)
     &	    .NE.%LOC(CLI$_ABSENT))		! Get all the usernames
	   LEN_P = LEN_P + I + 1
	   BULL_PARAMETER(LEN_P:LEN_P) = ','
	END DO
	LEN_P = LEN_P - 1

	I = 1		! Must change all " to "" in MAIL recipients
	DO WHILE (I.LE.LEN_P)
	   IF (BULL_PARAMETER(I:I).EQ.'"') THEN
	      BULL_PARAMETER = BULL_PARAMETER(:I)//'"'//
     &					BULL_PARAMETER(I+1:)
	      I = I + 1
	      LEN_P = LEN_P + 1
	   END IF
	   I = I + 1
	END DO

	CALL DISABLE_PRIVS
	CALL LIB$SPAWN('$MAIL SYS$LOGIN:BULL.SCR '//BULL_PARAMETER(:LEN_P)
     &	   //'/SUBJECT="'//MAIL_SUBJECT(:LEN_D)//'"',,,,,,STATUS)
	CALL ENABLE_PRIVS

	CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR')

	RETURN

1050	FORMAT('Description: ',A,/)
1060	FORMAT('From: ',A)

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

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

    	CHARACTER PASSWORD*31,DEFAULT_USER*12

	IF (FOLDER_NUMBER.EQ.0) THEN
	   WRITE (6,'('' ERROR: Cannot modify GENERAL folder.'')')
	   RETURN
	ELSE IF (FOLDER_OWNER.NE.USERNAME.AND..NOT.SETPRV_PRIV()) THEN
	   WRITE (6,'('' ERROR: No privileges to modify folder.'')')
	   RETURN
	END IF

	IF (CLI$PRESENT('NAME')) THEN
	   IF (REMOTE_SET) THEN
	      WRITE (6,'('' ERROR: Cannot change name of'',
     &				'' remote folder.'')')
	      RETURN
	   ELSE
	      CALL CLI$GET_VALUE('NAME',FOLDER1,LEN_P)
	      IF (LEN_P.GT.25) THEN
	         WRITE (6,'('' ERROR: Folder name cannot be larger 
     &				than 25 characters.'')')
	         RETURN
	      END IF
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
	     RETURN
	    ELSE IF (LEN_P.GT.80) THEN			! If too many characters
	     WRITE (6,'('' ERROR: Description must be < 80 characters.'')')
	    ELSE
	       FOLDER1_DESCRIP = FOLDER1_DESCRIP(:LEN_P) ! End fill with spaces
	    END IF
	   END DO
	ELSE
	   FOLDER1_DESCRIP = FOLDER_DESCRIP
	END IF

	IF (CLI$PRESENT('OWNER')) THEN
	   CALL CLI$GET_VALUE('OWNER',FOLDER1_OWNER,LEN_P)
	   CALL GET_UAF
     &		   (FOLDER1_OWNER,USERB1,GROUPB1,ACCOUNTB1,FLAGS,IER)
	   IF (.NOT.IER) THEN
	      WRITE (6,'('' ERROR: Owner name is not valid username.'')')
	      RETURN
	   ELSE IF (LEN_P.GT.LEN(FOLDER1_OWNER)) THEN
	      WRITE (6,'('' ERROR: Folder owner name too long.'')')
	      RETURN
	   ELSE IF (.NOT.SETPRV_PRIV()) THEN
	      WRITE(6,'('' Enter password of new owner: '',A)') CHAR(10)
	      CALL GET_INPUT_NOECHO(PASSWORD)
	      IF (TRIM(PASSWORD).EQ.0) THEN
		 WRITE (6,'('' ERROR: No password entered.'')')
		 RETURN
	      END IF
	      WRITE (6,'('' Attempting to verify password name...'')')
	      OPEN (UNIT=10,NAME='SYS$NODE"'//
     &		   FOLDER1_OWNER(:TRIM(FOLDER1_OWNER))
     &		   //' '//PASSWORD(:TRIM(PASSWORD))//'"::',
     &		   TYPE='SCRATCH',IOSTAT=IER)
	      CLOSE (UNIT=10)
	      IF (IER.NE.0) THEN
		 WRITE (6,'('' ERROR: Password is invalid.'')')
		 RETURN
	      ELSE
		 WRITE (6,'('' Password was verified.'')')
	      END IF
	   ELSE
	      FOLDER1_OWNER = FOLDER1_OWNER(:LEN_P)
	   END IF
	ELSE
	   FOLDER1_OWNER = FOLDER_OWNER
	END IF

	CALL OPEN_BULLFOLDER		! Open folder file

	IF (CLI$PRESENT('NAME')) THEN
	   READ (7,IOSTAT=IER,KEY=FOLDER1,KEYID=0)
					! See if folder exists
	   IF (IER.EQ.0) THEN
	      WRITE (6,'('' ERROR: Folder name already exists.'')')
	      CALL CLOSE_BULLFOLDER
	      RETURN
	   END IF
	END IF

	CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)

	IF (IER.EQ.0.AND.CLI$PRESENT('NAME')) THEN
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
     &		(FOLDER_FILE(:TRIM(FOLDER_FILE))//'.BULLFIL',IER)
	      IF (IER.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL)) THEN
	         CALL ADD_ACL(FOLDER1_OWNER,'R+W+C',IER)
	         CALL DEL_ACL(FOLDER_OWNER,'R+W+C',IER)
	      END IF
	   END IF
	   FOLDER = FOLDER1
	   FOLDER_OWNER = FOLDER1_OWNER
	   FOLDER_DESCRIP = FOLDER1_DESCRIP
	   DELETE (7)
	   CALL WRITE_FOLDER_FILE(IER)
	   IF (IER.EQ.0) WRITE (6,'('' Folder successfully modified.'')')
	END IF

	IF (IER.NE.0) THEN
	   WRITE (6,'('' ERROR: Folder modification aborted.'')')
	END IF

	CALL CLOSE_BULLFOLDER

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

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /REMOTE_READ_MESSAGE/ SCRATCH_R1
	DATA SCRATCH_R1 /0/

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLFILES.INC'

	EXTERNAL CLI$_ABSENT

	EXTERNAL BULLETIN_SUBCOMMANDS

	LOGICAL DELETE_ORIGINAL

	CHARACTER SAVE_FOLDER*25

	IF (CLI$PRESENT('ORIGINAL').AND..NOT.SETPRV_PRIV()) THEN
	   WRITE (6,
     &	  '('' ERROR: You have no privileges to keep original owner.'')')
	END IF

	ALL = CLI$PRESENT('ALL')

	MERGE = CLI$PRESENT('MERGE')

	SAVE_BULL_POINT = BULL_POINT

	IER1 = CLI$GET_VALUE('BULLETIN_NUMBER',BULL_PARAMETER,LEN_P)
	IF (IER1.EQ.%LOC(CLI$_ABSENT).AND..NOT.ALL) THEN
	   IF (BULL_POINT.EQ.0) THEN	! If no message has been read
	      WRITE(6,'('' ERROR: You are not reading any message.'')')
	      RETURN			! and return
	   END IF

	   CALL OPEN_BULLDIR_SHARED
	   CALL READDIR(BULL_POINT,IER)		! Get message directory entry
	   IF (IER.NE.BULL_POINT+1) THEN	! Was message found?
	      WRITE(6,'('' ERROR: Specified message was not found.'')')
	      CALL CLOSE_BULLDIR
	      RETURN
	   END IF

	   NUM_COPY = 1
	ELSE
	   CALL OPEN_BULLDIR_SHARED
	   CALL READDIR(0,IER)		! Get message directory entry
	   IF (NBULL.EQ.0) THEN		! Were messages found?
	      WRITE(6,'('' ERROR: No messages were found.'')')
	      CALL CLOSE_BULLDIR
	      RETURN
	   END IF

	   IF (IER1.NE.%LOC(CLI$_ABSENT)) THEN
	      CALL GET_2_VALS(BULL_PARAMETER,LEN_P,SBULL,EBULL,IER1)
	      IF (SBULL.LE.0.OR.IER1.NE.0) THEN
	         WRITE (6,'(A)') 
     &		  ' ERROR: Specified message number has incorrect format.'
	         CALL CLOSE_BULLDIR
	         RETURN
	      ELSE
		 NUM_COPY = EBULL - SBULL + 1
		 BULL_POINT = SBULL
	      END IF
	      ALL = .TRUE.
	   ELSE IF (CLI$PRESENT('ALL')) THEN
	      NUM_COPY = NBULL
	      BULL_POINT = 1
	   END IF
	END IF

	FROM_REMOTE = REMOTE_SET

	IF (REMOTE_SET) THEN
	   OPEN (UNIT=12,FILE='REMOTE.BULLDIR',
     &	      STATUS='SCRATCH',FORM='UNFORMATTED',
     &	      RECORDTYPE='FIXED',RECORDSIZE=DIR_RECORD_LENGTH/4,
     &	      ORGANIZATION='INDEXED',IOSTAT=IER,
     &	      KEY=(9:12:INTEGER,1:8:CHARACTER),ACCESS='KEYED')
	   IF (IER.EQ.0) THEN
	      OPEN (UNIT=11,FILE='REMOTE.BULLFIL',
     &	         STATUS='SCRATCH',IOSTAT=IER,
     &	         ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=32,
     &	         FORM='UNFORMATTED')
	   END IF
	   IF (IER.EQ.0) THEN
	      CALL OPEN_BULLFIL
	      I = BULL_POINT - 1
	      CALL READDIR(I,IER)
	      IF (IER.EQ.I+1) THEN
		 IF (I.EQ.0) THEN
	            WRITE (12,IOSTAT=IER1) BULLDIR_HEADER
		 ELSE
	            WRITE (12,IOSTAT=IER1) BULLDIR_ENTRY
		 END IF
	      END IF
	      NBLOCK = 1
	      DO WHILE (I.LT.BULL_POINT+NUM_COPY-1.AND.IER.EQ.I+1)
		 I = I + 1
	         CALL READDIR(I,IER)
		 IF (IER.EQ.I+1) THEN
		    BLOCK = NBLOCK
		    CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
	            WRITE (12,IOSTAT=IER1) BULLDIR_ENTRY
	            IF (IER1.EQ.0) THEN
	               WRITE (REMOTE_UNIT,'(2A)',IOSTAT=IER1) 5,I
	               IF (IER1.GT.0) THEN
	                  CALL DISCONNECT_REMOTE
	               ELSE
	                  CALL GET_REMOTE_MESSAGE(IER1)
	               END IF
	            END IF
		    IF (IER1.EQ.0) THEN
	               SCRATCH_R = SCRATCH_R1
	               DO J=1,LENGTH
	                CALL READ_QUEUE(%VAL(SCRATCH_R),SCRATCH_R,INPUT(:128))
		        WRITE (11'NBLOCK,IOSTAT=IER1) INPUT(:128)
		        NBLOCK = NBLOCK + 1
		       END DO
		    END IF
		    IF (IER1.NE.0) I = IER
		 END IF
	      END DO
	      NUM_COPY = I - BULL_POINT + 1
	   END IF
	   CALL CLOSE_BULLFIL
	   IF (IER1.NE.0) THEN
	      WRITE(6,'('' ERROR: Copy aborted. Remote folder problem.'')')
	      CLOSE (UNIT=11)
	      CLOSE (UNIT=12)
	      CALL CLOSE_BULLDIR
	      RETURN
	   END IF
	END IF

	CALL CLOSE_BULLDIR
	   
	SAVE_FOLDER = FOLDER
	SAVE_FOLDER_NUMBER = FOLDER_NUMBER
	CALL CLI$GET_VALUE('FOLDER',FOLDER1)

	FOLDER_NUMBER = -1	! Use FOLDER as key rather than FOLDER_NUMBER
	CALL SELECT_FOLDER(.FALSE.,IER)

	IF (.NOT.IER) THEN
	   WRITE (6,'('' ERROR: Cannot access specified folder.'')')
	   FOLDER_NUMBER = SAVE_FOLDER_NUMBER
	   FOLDER = SAVE_FOLDER
	   BULL_POINT = SAVE_BULL_POINT
	   CLOSE (UNIT=11)
	   CLOSE (UNIT=12)
	   RETURN
	END IF

	IF (READ_ONLY.OR.(MERGE.AND.REMOTE_SET)) THEN
	   IF (READ_ONLY) THEN
	      WRITE (6,'('' ERROR: No access to write into folder.'')')
	   ELSE
	      WRITE (6,'('' ERROR: /MERGE invalid into remote folder.'')')
	   END IF
	   FOLDER_NUMBER = SAVE_FOLDER_NUMBER
	   FOLDER1 = SAVE_FOLDER
	   CALL SELECT_FOLDER(.FALSE.,IER1)
	   BULL_POINT = SAVE_BULL_POINT
	   CLOSE (UNIT=11)
	   CLOSE (UNIT=12)
	   RETURN
	END IF

C
C  Add bulletin to bulletin file and directory entry for to directory file.
C

	CALL OPEN_BULLDIR			! Prepare to add dir entry

	CALL OPEN_BULLFIL			! Prepare to add bulletin

	CALL READDIR(0,IER)			! Get NBLOCK
	IF (IER.EQ.0) NBLOCK = 0		! If new file, NBLOCK is 0

	FOLDER1_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))
     &		//SAVE_FOLDER

	IF (.NOT.FROM_REMOTE) THEN
	   DO WHILE (FILE_LOCK(IER,IER1))
	    OPEN (UNIT=12,FILE=FOLDER1_FILE(:TRIM(FOLDER1_FILE))
     &	      //'.BULLDIR',STATUS='OLD',FORM='UNFORMATTED',
     &	      RECORDTYPE='FIXED',RECORDSIZE=DIR_RECORD_LENGTH/4,
     &	      ORGANIZATION='INDEXED',IOSTAT=IER,
     &	      KEY=(9:12:INTEGER,1:8:CHARACTER),ACCESS='KEYED')
	   END DO

	   IF (IER.EQ.0) THEN
	      DO WHILE (FILE_LOCK(IER,IER1))
	       OPEN (UNIT=11,FILE=FOLDER1_FILE(:TRIM(FOLDER1_FILE))
     &	         //'.BULLFIL',STATUS='UNKNOWN',IOSTAT=IER,
     &	         ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=32,
     &	         FORM='UNFORMATTED')
	      END DO
	   END IF
	ELSE
	   IER= 0
	END IF

	IF (MERGE) CALL INITIALIZE_MERGE(IER)

	START_BULL_POINT = BULL_POINT

	IF (IER.EQ.0) READ (12,KEYID=0,KEY=BULL_POINT-1,IOSTAT=IER)

	DO WHILE (NUM_COPY.GT.0.AND.IER.EQ.0)
	   READ (12,IOSTAT=IER) BULLDIR_ENTRY
	   NUM_COPY = NUM_COPY - 1

	   CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
	   CALL CONVERT_ENTRY_FROMBIN

	   IF (.NOT.BTEST(FOLDER_FLAG,2).OR.	! Not system folder?
     &		 .NOT.SETPRV_PRIV()) THEN	! Or no privileges?
	      SYSTEM = IBCLR(SYSTEM,0)		! Remove system bit
	   END IF

	   IF (BTEST(SYSTEM,2).AND.		! Shutdown message?
     &	    (.NOT.BTEST(FOLDER_FLAG,2).OR.	! Not system folder?
     &		 .NOT.SETPRV_PRIV())) THEN	! Or no privileges?
	      SYSTEM = IBCLR(SYSTEM,2)		! Remove shutdown bit
	      CALL GET_EXDATE(EXDATE,FOLDER_BBEXPIRE)
	      EXTIME = '00:00:00.00'
	   ELSE IF (BTEST(SYSTEM,1).AND.FOLDER_NUMBER.EQ.0.AND.
     &		 .NOT.SETPRV_PRIV().AND..NOT.ALL) THEN	! Permanent?
	      WRITE (6,'('' ERROR: No privileges to add'',
     &				'' permanent message.'')')
	      WRITE (6,'('' Expiration will be '',I,'' days.'')')
     &				FOLDER_BBEXPIRE
	      SYSTEM = IBCLR(SYSTEM,1)
	      CALL GET_EXDATE(EXDATE,FOLDER_BBEXPIRE)
	      EXTIME = '00:00:00.00'
	   END IF

	   IF (.NOT.CLI$PRESENT('ORIGINAL')) THEN	! If not /ORIGINAL
	      FROM = USERNAME			! Specify owner
	   END IF

	   IF (REMOTE_SET) THEN
	      WRITE (REMOTE_UNIT,'(A)',IOSTAT=IER) 2
	      IF (IER.NE.0) CALL ERROR_AND_EXIT
	   END IF

	   IF (MERGE) CALL ADD_MERGE_TO(IER)

	   IF (IER.EQ.0) THEN
	      NBLOCK = NBLOCK + 1

	      DO I=BLOCK,BLOCK+LENGTH-1
	         READ (11'I,IOSTAT=IER) INPUT(:128)
	         IF (IER.EQ.0) THEN
		    CALL WRITE_BULL_FILE(NBLOCK,INPUT(:128))
	         END IF
	         NBLOCK = NBLOCK + 1
	      END DO
	   END IF

	   IF (IER.EQ.0) THEN
	      IF (MERGE) THEN
		 CALL ADD_MERGE_FROM(IER)
	      ELSE
	         CALL ADD_ENTRY		! Add the new directory entry
	      END IF
	      BULL_POINT = BULL_POINT + 1
	   END IF
	END DO

	IF (MERGE) CALL ADD_MERGE_REST(IER)

	CALL CLOSE_BULLFIL			! Finished adding bulletin

	CLOSE (UNIT=11)

	CLOSE (UNIT=12)

	IF (FOLDER_NUMBER.GE.0.AND.IER.EQ.0) THEN
	   CALL UPDATE_FOLDER			! Update folder info
C
C  If user is adding message, update that user's last read time for
C  folder, so user is not alerted of new message which is owned by user.
C
	   LAST_READ_BTIM(1,FOLDER_NUMBER+1) = F_NEWEST_BTIM(1)
	   LAST_READ_BTIM(2,FOLDER_NUMBER+1) = F_NEWEST_BTIM(2)
	END IF

	CALL CLOSE_BULLDIR			! Totally finished with add

	IF (IER.EQ.0) THEN
	   WRITE (6,'('' Successful copy to folder '',A)')
     &		FOLDER(:TRIM(FOLDER))//'.'
	   IF (MERGE) THEN
	      CALL LIB$DELETE_FILE(FOLDER_FILE(:TRIM(FOLDER_FILE))//
     &		  '.BULLDIR;-1')
	   END IF
	ELSE IF (MERGE) THEN
	   WRITE (6,'('' ERROR: Copy aborted. No files copied.'')')
	ELSE
	   WRITE (6,'('' ERROR: Copy aborted. '',I,'' files copied.'')')
     &			BULL_POINT - START_BULL_POINT
	END IF
 
	FOLDER_NUMBER = SAVE_FOLDER_NUMBER
	FOLDER1 = SAVE_FOLDER
	CALL SELECT_FOLDER(.FALSE.,IER1)

	BULL_POINT = SAVE_BULL_POINT

	IF (DELETE_ORIGINAL.AND.IER.EQ.0) THEN
	   IF (FROM_REMOTE.AND.ALL) THEN
	      WRITE (6,'('' WARNING: Original messages not deleted.'')')
	      WRITE (6,'('' Multiple deletions not possible for '',
     &			''remote folders.'')')
	   ELSE
	      IER = CLI$DCL_PARSE(INCMD,BULLETIN_SUBCOMMANDS)
	      CALL DELETE
	   END IF
	END IF

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

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	EXTERNAL CLI$_ABSENT

	CHARACTER*32 QUEUE

	INTEGER*2 FILE_ID(14)
	INTEGER*2 IOSB(4)
	EQUIVALENCE (IOSB(1),JBC_ERROR)

        CHARACTER*31 FORM_NAME

	PARAMETER FF = CHAR(12)

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
	   SBULL = BULL_POINT
	   EBULL = SBULL
	   IER = 0
	END IF

	IF (SBULL.LE.0.OR.IER.NE.0) THEN
	   WRITE (6,1015)
	   RETURN
	END IF

	IF (.NOT.SETPRV_PRIV()) THEN		! If no SETPRV, remove SYSPRV
	   CALL DISABLE_PRIVS			! privileges when trying to
	END IF					! create new file.

	OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.LIS',ERR=900,IOSTAT=IER,
     &		RECL=LINE_LENGTH,STATUS='NEW',CARRIAGECONTROL='LIST')

	CALL ENABLE_PRIVS

	CALL OPEN_BULLDIR_SHARED

	CALL OPEN_BULLFIL_SHARED

	HEAD = CLI$PRESENT('HEADER')

	DO I=SBULL,EBULL
	   CALL READDIR(I,IER)		! Get info for specified message

	   IF (IER.NE.I+1) THEN		! Was message found?
	      IF (I.EQ.SBULL) THEN	! No, were any messages found?
	         WRITE(6,1030) 		! If not, then error out
		 CLOSE (UNIT=3,STATUS='DELETE')
		 CALL CLOSE_BULLFIL
		 CALL CLOSE_BULLDIR
		 RETURN
	      END IF
	   ELSE				! Yes, message found.
	      IF (I.GT.SBULL) WRITE(3,'(A)') FF

	      ILEN = LINE_LENGTH + 1

	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN
	         IF (HEAD) THEN
		    WRITE(3,1060) INPUT(7:ILEN),DATE//' '//TIME(:8)
		 END IF
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
	         IF (ILEN.GT.0) WRITE (3,'(A)') INPUT(1:ILEN)
	      END DO
	   END IF
	END DO

	CLOSE (UNIT=3)			! Bulletin copy completed

	CALL CLOSE_BULLFIL
	CALL CLOSE_BULLDIR

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(18,SJC$_FILE_SPECIFICATION,
     &		%LOC('SYS$LOGIN:BULL.LIS'))

	IER = CLI$GET_VALUE('QUEUE',QUEUE,ILEN) 	! Get queue name
	IF (ILEN.EQ.0) THEN
	   QUEUE = 'SYS$PRINT'
	   ILEN = 9
	END IF

	CALL ADD_2_ITMLST(ILEN,SJC$_QUEUE,%LOC(QUEUE))
	CALL ADD_2_ITMLST(0,SJC$_DELETE_FILE,0)

	IF (CLI$PRESENT('NOTIFY')) THEN
	   CALL ADD_2_ITMLST(0,SJC$_NOTIFY,0)
	END IF

	IF (CLI$PRESENT('FORM')) THEN
	   IER = CLI$GET_VALUE('FORM',FORM_NAME,FORM_NAME_LEN)
	   CALL ADD_2_ITMLST(FORM_NAME_LEN,SJC$_FORM_NAME,%LOC(FORM_NAME))
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
	WRITE(6,1000)
	CALL SYS_GETMSG(IER)
	RETURN

1000	FORMAT(' ERROR: Unable to open temporary file
     & SYS$LOGIN:BULL.LIS for printing.')
1010	FORMAT(' ERROR: You have not read any message.')
1015	FORMAT(' ERROR: Specified message number has incorrect format.')
1030	FORMAT(' ERROR: Specified message was not found.')
1050	FORMAT('Description: ',A,/)
1060	FORMAT('From: ',A,/,'Date: ',A)

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

	INCLUDE 'BULLUSER.INC'

	COMMON /READIT/ READIT

	COMMON /PAGE/ PAGE_LENGTH,PAGE_WIDTH,PAGING
	LOGICAL PAGING

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD

	COMMON /READ_DISPLAY/ LINE_OFFSET

	COMMON /TAGS/ BULL_TAG,READ_TAG

	DATA SCRATCH_B1/0/

	CHARACTER TODAY*11,DATETIME*23,BUFFER*(LINE_LENGTH)
	CHARACTER SAVE_MSG_KEY*8

	LOGICAL SINCE,PAGE

	CALL LIB$ERASE_PAGE(1,1)		! Clear screen
	END = 0					! Nothing outputted on screen

	IF (READ_COUNT.GT.0) GO TO 100		! Skip init steps if this is
						! not first page of bulletin

	SINCE = .FALSE.
	PAGE = .TRUE.
	
	IF (.NOT.PAGING) PAGE = .FALSE.
	IF (INCMD(:4).EQ.'READ') THEN		! If READ command...
	 IF (CLI$PRESENT('MARKED')) THEN
	    CALL GET_FIRST_TAG(FOLDER_NUMBER,IER,BULL_POINT)
	    IF (IER.NE.0) THEN
	       WRITE (6,'('' ERROR: No marked messages found.'')')
	       RETURN
	    ELSE
	       READ_TAG = .TRUE.
	    END IF
	 END IF

	 IF (.NOT.CLI$PRESENT('PAGE')) PAGE = .FALSE.
	 IF (CLI$PRESENT('SINCE')) THEN		! was /SINCE specified?
	   IER = CLI$GET_VALUE('SINCE',DATETIME)
	   IF (DATETIME.EQ.'TODAY') THEN	! TODAY is the default.
	      IER = SYS$BINTIM('-- 00:00:00.00',TODAY)
	      CALL GET_MSGKEY(TODAY,MSG_KEY)
 	   ELSE
	      CALL SYS_BINTIM(DATETIME,MSG_BTIM)
	      CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
 	   END IF
	 ELSE IF (CLI$PRESENT('NEW')) THEN	! was /NEW specified?
	   DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &			       F_NEWEST_BTIM)
	   IF (DIFF.GE.0) THEN
	      WRITE (6,'('' No new messages are present.'')')
	      RETURN
	   ELSE
 	      CALL GET_MSGKEY(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &							MSG_KEY)
	   END IF
	 END IF
	 IF (CLI$PRESENT('SINCE').OR.CLI$PRESENT('NEW')) THEN
	   CALL OPEN_BULLDIR_SHARED
           CALL READDIR_KEYGE(IER)
	   CALL CLOSE_BULLDIR
	   IF (IER.EQ.0) THEN
	      WRITE (6,'('' No messages past specified date.'')')
	      RETURN
	   ELSE
	      BULL_READ = IER
	      IER = IER + 1
	   END IF
	   SINCE = .TRUE.
	 END IF
	END IF

	IF (READ_TAG) THEN
	 NEXT = .FALSE.
	 IF (INCMD(:4).EQ.'NEXT'.OR.INCMD.EQ.' ') THEN
	   NEXT = .TRUE.
	 ELSE IF (INCMD(:4).EQ.'READ') THEN
	   IF (.NOT.CLI$PRESENT('BULLETIN_NUMBER')) NEXT = .TRUE.
	 END IF
	 IF (INCMD(:4).EQ.'BACK') THEN
	   SAVE_MSG_KEY = MSG_KEY
	   MSG_KEY = BULLDIR_HEADER
	   I = 0
	   IER = 0
	   DO WHILE (IER.EQ.0.AND.MSG_KEY.NE.SAVE_MSG_KEY)
	      I = I + 1
	      CALL GET_NEXT_TAG(FOLDER_NUMBER,IER,BULL_READ)
	   END DO
	   IF (IER.EQ.0) THEN
	      MSG_KEY = BULLDIR_HEADER
	      DO J=1,I-1
	         CALL GET_NEXT_TAG(FOLDER_NUMBER,IER,BULL_READ)
	      END DO
	      IER = BULL_READ + 1
	   ELSE
	      IER = 0
	   END IF
	 ELSE IF (NEXT) THEN
	   IF (SINCE) THEN
	      CALL GET_THIS_OR_NEXT_TAG(FOLDER_NUMBER,IER,BULL_READ)
	   ELSE
	      IF (BULL_POINT.GT.0) THEN
	         CALL OPEN_BULLDIR_SHARED
	         CALL READDIR(BULL_POINT,IER)
	         CALL CLOSE_BULLDIR
	      ELSE
	         MSG_KEY = BULLDIR_HEADER
	      END IF
	      CALL GET_NEXT_TAG(FOLDER_NUMBER,IER,BULL_READ)
	   END IF
	   IF (IER.EQ.0) THEN
	      IER = BULL_READ + 1
	   ELSE
	      IER = 0
	   END IF
	 END IF
	END IF

	IF (.NOT.SINCE.AND.
     &	    (.NOT.READ_TAG.OR.(.NOT.NEXT.AND.INCMD(:4).NE.'BACK'))) THEN
	 IF (BULL_READ.GT.0) THEN		! Valid bulletin number?
	   CALL OPEN_BULLDIR_SHARED
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
	   CALL CLOSE_BULLDIR
	 ELSE
	   IER = 0
	 END IF
	END IF

	IF (IER.NE.BULL_READ+1) THEN		! Was bulletin found?
	   WRITE(6,1030)			! If not, then error out
	   RETURN
	END IF

	DIFF = COMPARE_BTIM(MSG_BTIM,LAST_READ_BTIM(1,FOLDER_NUMBER+1))
	IF (DIFF.GT.0) THEN
	   LAST_READ_BTIM(1,FOLDER_NUMBER+1) = MSG_BTIM(1)
	   LAST_READ_BTIM(2,FOLDER_NUMBER+1) = MSG_BTIM(2)
	END IF

	BULL_POINT = BULL_READ			! Update bulletin counter

	IF (INCMD(:4).EQ.'READ'.OR.INCMD(:4).EQ.'CURR') THEN
	   IF (CLI$PRESENT('EDIT')) THEN
	      CALL READ_EDIT
	      RETURN
	   END IF
	END IF

	FLEN = TRIM(FOLDER)
	IF (BULL_POINT.GT.F_NBULL) F_NBULL = BULL_POINT
	WRITE (INPUT,'(1X,I5,'' of '',I5)') BULL_POINT,F_NBULL
	DO WHILE (INDEX(INPUT,'  ').LT.TRIM(INPUT))
	   I = INDEX(INPUT,'  ')
	   INPUT(I:) = INPUT(I+1:)
	END DO
	I = TRIM(INPUT)
	INPUT = ' #'//INPUT(2:TRIM(INPUT))
	INPUT(PAGE_WIDTH-LEN(FOLDER):) = FOLDER(:FLEN)
	IF (READIT.GT.0) THEN
	   WRITE(6,'(A)') '+'//INPUT(:TRIM(INPUT))
	ELSE
	   WRITE(6,'(1X,A)') INPUT(:TRIM(INPUT))
	END IF

	END = 1					! Outputted 1 line to screen

	IF (EXDATE(8:9).EQ.'18'.OR.INDEX(EXDATE,'1900').GT.0) THEN
	   INPUT = 'Date:   '//DATE//' '//TIME(:5)//'   (DELETED)'
	ELSE IF ((SYSTEM.AND.4).EQ.4) THEN	! Is entry shutdown bulletin?
	   INPUT = 'Date:   '//DATE//' '//TIME(:5)//'   Expires on shutdown'
	ELSE IF ((SYSTEM.AND.2).EQ.2) THEN	! Is entry permanent bulletin?
	   INPUT = 'Date:   '//DATE//' '//TIME(:5)//'   Permanent'
	ELSE
	   INPUT = 'Date:   '//DATE//' '//TIME(:5)//
     &				'   Expires:   '//EXDATE//' '//EXTIME(:5)
	END IF
	IF ((SYSTEM.AND.1).EQ.1) THEN		! System bulletin?
	   INPUT = INPUT(:TRIM(INPUT))//' / System'
	END IF
	WRITE (6,'(1X,A)') INPUT(:TRIM(INPUT))

	END = END + 1

	CALL OPEN_BULLFIL_SHARED		! Get bulletin file
	LINE_OFFSET = 0
	CHAR_OFFSET = 0
	ILEN = LINE_LENGTH + 1
	CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN
	   INPUT = 'From:   '//INPUT(7:)
	   DO WHILE (TRIM(INPUT).GT.0)
	      I = MIN(PAGE_WIDTH,TRIM(INPUT))
	      WRITE(6,'(1X,A)') INPUT(:I)
	      INPUT = INPUT(I+1:)
	      END = END + 1
	   END DO
	   LINE_OFFSET = 1
	ELSE
	   WRITE(6,'('' From:   '',A)') FROM
	   END = END + 1
	END IF
	IF (INPUT(:6).NE.'Subj: ') THEN
	   CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	END IF
	LEN_TEMP = ILEN
	CALL CONVERT_TABS(INPUT,LEN_TEMP)
	IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THEN
	   INPUT = 'Subj:   '//INPUT(7:)
	   DO WHILE (TRIM(INPUT).GT.0)
	      I = MIN(PAGE_WIDTH,TRIM(INPUT))
	      WRITE(6,'(1X,A)') INPUT(:I)
	      INPUT = INPUT(I+1:)
	      END = END + 1
	   END DO
	   LINE_OFFSET = LINE_OFFSET + 1
	ELSE
	   IF (LINE_OFFSET.EQ.1) THEN
	      CHAR_OFFSET = 1 - PAGE_WIDTH
	      LINE_OFFSET = 2
	   END IF
	   WRITE(6,'('' Subj:   '',A)') DESCRIP
	   END = END + 1
	END IF
	IF (LINE_OFFSET.EQ.0) ILEN = LINE_LENGTH + 1
	CALL CLOSE_BULLFIL			! End of bulletin file read

	WRITE(6,'(1X)')
	IF (READIT.GT.0) WRITE(6,'(1X)')
	END = END + 1
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

	READ_ALREADY = 0			! Number of lines already read
						! from record.
	IF (READ_COUNT.EQ.-2) THEN		! Just output header first read
	   READ_COUNT = BLOCK
	   RETURN
	ELSE
	   READ_COUNT = BLOCK			! Init bulletin record counter
	END IF

	GO TO 200

100	IF (READIT.EQ.0) THEN 			! If not 1st page of READ
	   WRITE (BUFFER,'(1X,I5,'' of '',I5)') BULL_POINT,F_NBULL
	   DO WHILE (INDEX(BUFFER,'  ').LT.TRIM(BUFFER))
	      I = INDEX(BUFFER,'  ')
	      BUFFER(I:) = BUFFER(I+1:)
	   END DO
	   BUFFER = ' #'//BUFFER(2:TRIM(BUFFER))
	   BUFFER(PAGE_WIDTH-LEN(FOLDER):) = FOLDER(:FLEN)
	   WRITE(6,'(1X,A,/)') BUFFER(:TRIM(BUFFER)) ! Output header info
	   END = END + 2			! Increase display counter
	END IF

200	SCRATCH_B = SCRATCH_B1			! Init queue pointer to header
	IF (READIT.GT.0) END = END - 2		! /READ can output 2 more lines
	DISPLAY = 0

	CALL OPEN_BULLFIL_SHARED		! Get bulletin file
	MORE_LINES = .TRUE.
	DO WHILE (ILEN.GT.0.AND.MORE_LINES)
	   IF (CHAR_OFFSET.EQ.0) THEN
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      LINE_OFFSET = LINE_OFFSET + 1
	   END IF
	   IF (ILEN.LT.0) THEN		! Error, couldn't read record
	      ILEN = 0			! Fake end of reading file
	      MORE_LINES = .FALSE.
	   ELSE IF (ILEN.GT.0) THEN
	      IF (CHAR_OFFSET.EQ.0) THEN
		 LEN_TEMP = ILEN
		 CALL CONVERT_TABS(INPUT,LEN_TEMP)
		 IF (LEN_TEMP.GT.PAGE_WIDTH) THEN
		    CHAR_OFFSET = 1
		    BUFFER = INPUT(:PAGE_WIDTH)
	            CALL WRITE_QUEUE(%VAL(SCRATCH_B),SCRATCH_B,BUFFER)
		 ELSE
	            CALL WRITE_QUEUE(%VAL(SCRATCH_B),SCRATCH_B,INPUT)
		 END IF
	      ELSE
		 CHAR_OFFSET = CHAR_OFFSET + PAGE_WIDTH
		 IF (LEN_TEMP.LE.CHAR_OFFSET+PAGE_WIDTH-1) THEN
		    BUFFER = INPUT(CHAR_OFFSET:LEN_TEMP)
	            CALL WRITE_QUEUE(%VAL(SCRATCH_B),SCRATCH_B,BUFFER)
		    CHAR_OFFSET = 0
		 ELSE
		    BUFFER = INPUT(CHAR_OFFSET:CHAR_OFFSET+PAGE_WIDTH-1)
	            CALL WRITE_QUEUE(%VAL(SCRATCH_B),SCRATCH_B,BUFFER)
		 END IF
	      END IF
	      DISPLAY = DISPLAY + 1
	      IF ((DISPLAY.EQ.PAGE_LENGTH-END-4).AND.PAGE) THEN
		 MORE_LINES = .FALSE.
	      END IF
	   END IF
	END DO

	CALL CLOSE_BULLFIL			! End of bulletin file read

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
	   CALL READ_QUEUE(%VAL(SCRATCH_B),SCRATCH_B,BUFFER) ! Get queue record
	   IF (I.EQ.1.AND.READIT.GT.0) THEN
	      WRITE(6,'(A)') '+'//BUFFER(:TRIM(BUFFER))	 ! (See above comments)
	   ELSE
	      WRITE(6,'(1X,A)') BUFFER(:TRIM(BUFFER))
	   END IF
	END DO

	IF (ILEN.EQ.0) THEN			! End of message?
	   READ_COUNT = 0			! init bulletin record counter
	ELSE	! Possibly end of message since end of page could be last line
	   CALL TEST_MORE_RECORDS(BLOCK,LENGTH,IREC)
	   IF (IREC.EQ.0) THEN			! Last record?
	      CALL TEST_MORE_LINES(ILEN)	! More lines to read?
	      IF (ILEN.GT.0) THEN		! Yes, there are still more
	         IF (READIT.EQ.0) WRITE(6,1070)	! say there is more of bulletin
	      ELSE				! Yes, last line anyway
	         READ_COUNT = 0			! init bulletin record counter
	      END IF
	   ELSE IF (READIT.EQ.0) THEN		! Not last record so
	      WRITE(6,1070)			! say there is more of bulletin
	   END IF
	END IF

	RETURN

1030	FORMAT(' ERROR: Specified message was not found.')
1070	FORMAT(1X,/,' Press RETURN for more...',/)

2000	FORMAT(A)

	END





	SUBROUTINE READ_EDIT

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'

	OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,
     &		RECL=LINE_LENGTH,STATUS='NEW',CARRIAGECONTROL='LIST')

	IF (IER.NE.0) THEN
	   CALL ERRSNS(IDUMMY,IER)
	   CALL SYS_GETMSG(IER)
	   RETURN
	END IF

	CALL OPEN_BULLFIL_SHARED

	ILEN = LINE_LENGTH + 1

	CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN
	   WRITE(3,1060) INPUT(7:ILEN),DATE//' '//TIME(:8)
	   CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	ELSE
	   WRITE(3,1060) FROM,DATE//' '//TIME(:8)
	END IF
	IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THEN
	   WRITE(3,1050) INPUT(7:ILEN)
	ELSE
	   WRITE(3,1050) DESCRIP
	   IF (ILEN.GT.0) WRITE (3,'(A)') INPUT(:ILEN)
	END IF

	DO WHILE (ILEN.GT.0)		! Copy bulletin into file
	   CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   IF (ILEN.GT.0) WRITE (3,'(A)') INPUT(:ILEN)
	END DO

	CLOSE (UNIT=3)			! Bulletin copy completed
	CALL CLOSE_BULLFIL

	CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')

	CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;*')

1050	FORMAT('Description: ',A,/)
1060	FORMAT('From: ',A,' Date: ',A)

	RETURN
	END


	SUBROUTINE READNEW(REDO)
C
C  SUBROUTINE READNEW
C
C  FUNCTION: Displays new non-system bulletins with prompts between bulletins.
C

	IMPLICIT INTEGER (A-Z)

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'

	COMMON /SYSTEM_FOLDERS/ SYSTEM_FLAG(FLONG),DUMMY(2)

	COMMON /POINT/ BULL_POINT

	COMMON /READ_DISPLAY/ LINE_OFFSET

	COMMON /PAGE/ PAGE_LENGTH,PAGE_WIDTH,PAGING
	LOGICAL PAGING

	CHARACTER INREAD*1,FILE_DEF*80,NUMREAD*5

	DATA LEN_FILE_DEF /0/, INREAD/0/

	LOGICAL SLOW,SLOW_TERMINAL

	FIRST_MESSAGE = BULL_POINT

	IF (ICHAR(INREAD).EQ.0) THEN	! If calling READNEW for first time
	   SLOW = SLOW_TERMINAL()	! Check baud rate of terminal
	END IF				! to avoid gobs of output

	LEN_P = 0			! Tells read subroutine there is
					! no bulletin parameter

1	WRITE(6,1000)			! Ask if want to read new bulletins

	CALL GET_INPUT_NUM(NUMREAD,NLEN)	! Get input
	CALL STR$UPCASE(NUMREAD,NUMREAD)	! Make input upper case
	READ (NUMREAD,'(I<NLEN>)',IOSTAT=IER) TEMP_READ
	IF (IER.NE.0) THEN
	   INREAD = NUMREAD(:1)
	   IF (INREAD.EQ.'N'.OR.INREAD.EQ.'Q'.OR.INREAD.EQ.'E') THEN
	      IF (INREAD.EQ.'Q') THEN
	         WRITE (6,'(''+uit'',$)')
	      ELSE IF (INREAD.EQ.'E') THEN
	         WRITE (6,'(''+xit'',$)')
		 DO I=1,FLONG			! Just show SYSTEM folders
		    NEW_MSG(I) = NEW_MSG(I).AND.SYSTEM_FLAG(I)
		 END DO
		 DO I=1,FLONG	! Test for new messages in SYSTEM folders
		    IF (NEW_MSG(I).NE.0) RETURN
		 END DO
		 CALL EXIT
	      ELSE
	         WRITE (6,'(''+o'',$)')
	      END IF
	      RETURN	! If NO, exit
			! Include QUIT to be consistent with next question
	   ELSE
	      CALL LIB$ERASE_PAGE(1,1)
	   END IF
	END IF

3	IF (TEMP_READ.GT.0) THEN
	   IF (TEMP_READ.LT.FIRST_MESSAGE+1.OR.TEMP_READ.GT.NBULL) THEN
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
	   CALL OPEN_BULLDIR_SHARED		! If so, see if more new bulls
10	   CALL READDIR(BULL_POINT+1,IER_POINT)
	   IF ((IER_POINT.EQ.BULL_POINT+2).AND.	! If system bulletin (and system
     &	       (SYSTEM.AND.BTEST(FOLDER_FLAG,2))) THEN	! folder) then skip it.
	      BULL_POINT = BULL_POINT + 1
	      GO TO 10
	   END IF
	   CALL CLOSE_BULLDIR
	END IF

12	IF (READ_COUNT.EQ.0) THEN		! Prompt user in between
	   WRITE(6,1020)			! full screens or end of bull.
	ELSE
	   WRITE(6,1030)
	END IF

	CALL GET_INPUT_NOECHO(INREAD)
	CALL STR$UPCASE(INREAD,INREAD)	! Convert input to upper case

	IF (INREAD.EQ.'Q') THEN		! If Q , then QUIT
	   WRITE (6,'(''+Quit'',$)')
	   RETURN
	ELSE IF (INREAD.EQ.'D') THEN	! If D , then redisplay directory
	   WRITE (6,'(''+Dir'',$)')
	   REDO = .TRUE.
	   RETURN
	ELSE IF (INREAD.EQ.'F') THEN	! If F then copy bulletin to file
	   WRITE (6,'(''+ '')')		! Move cursor from end of prompt line
					! to beginning of next line.
	   IF (LEN_FILE_DEF.EQ.0) THEN
	      CALL LIB$SYS_TRNLOG('SYS$LOGIN',ILEN,FILE_DEF)
	      IER = LIB$FIND_FILE(FILE_DEF//'BULL.DIR',
     &			BULL_PARAMETER,CONTEXT)
	      IF (IER) THEN
		 FILE_DEF = BULL_PARAMETER(:ILEN-1)//'.BULL]'
		 LEN_FILE_DEF = ILEN + 5
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
	   ELSE
	      IER = LIB$SYS_TRNLOG(BULL_PARAMETER(:LEN_P),ILEN,INPUT)
	      IF (IER.NE.1.AND.INDEX(BULL_PARAMETER(:LEN_P),':').EQ.0
     &		  .AND.INDEX(BULL_PARAMETER(:LEN_P),'[').EQ.0) THEN
		 BULL_PARAMETER = FILE_DEF(:LEN_FILE_DEF)//
     &				BULL_PARAMETER(:LEN_P)
		 LEN_P = LEN_P + LEN_FILE_DEF
	      END IF
	   END IF

	   BLOCK_SAVE = BLOCK
	   LENGTH_SAVE = LENGTH
	   CALL OPEN_BULLDIR_SHARED
	   CALL OPEN_BULLFIL_SHARED		! Open BULLETIN file
	   CALL READDIR(FILE_POINT,IER)
	   IF (.NOT.SETPRV_PRIV()) THEN		! If no SETPRV, remove SYSPRV
	      CALL DISABLE_PRIVS		! privileges when trying to
	   END IF				! create new file.
	   OPEN(UNIT=3,FILE=BULL_PARAMETER(:LEN_P),IOSTAT=IER,
     &	      RECL=LINE_LENGTH,ERR=18,STATUS='UNKNOWN',
     &	      CARRIAGECONTROL='LIST',ACCESS='APPEND')
	   WRITE(3,1050) DESCRIP		! Output bulletin header info
	   WRITE(3,1060) FROM,DATE//' '//TIME(:5)
	   ILEN = LINE_LENGTH + 1
	   DO WHILE (ILEN.GT.0)
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      IF (ILEN.GT.0) WRITE(3,'(A)') INPUT(:TRIM(INPUT))
	   END DO
	   IF (ILEN.EQ.0) WRITE(6,1040) BULL_PARAMETER(:LEN_P)
						! Show name of file created.
18	   IF (IER.NE.0) THEN
	      CALL ERRSNS(IDUMMY,IER)
	      CALL SYS_GETMSG(IER)
	   END IF
	   CLOSE (UNIT=3)			! Bulletin copy completed
	   IF (READ_COUNT.GT.0) THEN		! Reposition GET_BULL routine
	      ILEN = LINE_LENGTH + 1		! in case read in progress
	      DO I=1,LINE_OFFSET		! and partial block was read.
	         CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      END DO
	   END IF
	   CALL CLOSE_BULLFIL
	   CALL CLOSE_BULLDIR
	   LENGTH = LENGTH_SAVE
	   BLOCK = BLOCK_SAVE
	   CALL ENABLE_PRIVS			! Reset BYPASS privileges
	   GO TO 12
	ELSE IF (INREAD.EQ.'N'.AND.READ_COUNT.GT.0) THEN
				! If NEXT and last bulletins not finished
	   READ_COUNT = 0			! Reset read bulletin counter
	   CALL OPEN_BULLDIR_SHARED		! Look for NEXT bulletin
20	   CALL READDIR(BULL_POINT+1,IER)
	   IF (IER.NE.BULL_POINT+2) THEN	! If no NEXT bulletin
	      CALL CLOSE_BULLDIR		! Exit
	      WRITE(6,1010)
	      RETURN
	   ELSE IF (SYSTEM.AND.BTEST(FOLDER_FLAG,2)) THEN
	      BULL_POINT = BULL_POINT + 1	! If SYSTEM bulletin, skip it
	      GO TO 20			! Look for more bulletins
	   END IF
	   CALL CLOSE_BULLDIR
	ELSE IF (INREAD.EQ.'R') THEN
	   WRITE (6,'(''+Read'')')
	   WRITE (6,'('' Enter message number: '',$)')
	   CALL GET_INPUT_NUM(NUMREAD,NLEN)	! Get input
	   CALL STR$UPCASE(NUMREAD,NUMREAD)	! Make input upper case
	   READ (NUMREAD,'(I<NLEN>)',IOSTAT=IER) TEMP_READ
	   IF (IER.NE.0.OR.TEMP_READ.LE.0) THEN
	      WRITE (6,'('' ERROR: Invalid message number specified.'')')
	      GO TO 12
	   ELSE
	      GO TO 3
	   END IF
	ELSE IF (IER_POINT.NE.BULL_POINT+2.AND.READ_COUNT.EQ.0) THEN
	   WRITE(6,1010)
	   RETURN
	END IF
	IF (READ_COUNT.EQ.0.AND.SLOW) READ_COUNT = -2
	GO TO 5

1000	FORMAT(' Read messages? Type N(No),E(Exit),message
     & number, or any other key for yes: ',$)
1010	FORMAT(' No more messages.')
1020	FORMAT(1X,<PAGE_WIDTH>('-'),/,' Type Q(Quit),
     & F(File it), D(Dir), R(Read msg #) or other for next message: ',$)
1030	FORMAT(1X,<PAGE_WIDTH>('-'),/,' Type Q(Quit), F(File), N(Next),
     & D(Dir), R(Read msg #) or other for MORE: ',$)
1040	FORMAT(' Message written to ',A)
1050	FORMAT(/,'Description: ',A53)
1060	FORMAT('From: ',A12,' Date: ',A20,/)

	END




	SUBROUTINE SET_DEFAULT_EXPIRE
C
C  SUBROUTINE SET_DEFAULT_EXPIRE
C
C  FUNCTION: Sets default expiration date.
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	CHARACTER EXPIRE*3

	IF (SETPRV_PRIV().OR.USERNAME.EQ.FOLDER_OWNER) THEN
	   IER = CLI$GET_VALUE('DEFAULT_EXPIRE',EXPIRE,EX_LEN)
	   IF (EX_LEN.GT.3) EX_LEN = 3
	   READ (EXPIRE,'(I<EX_LEN>)') TEMP

	   CALL OPEN_BULLFOLDER		! Open folder file
	   CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)
	   IF (TEMP.GT.BBEXPIRE_LIMIT.AND..NOT.SETPRV_PRIV()) THEN
	      WRITE (6,'('' ERROR: Expiration cannot be > '',
     &			I3,'' days.'')') BBEXPIRE_LIMIT
	   ELSE IF (TEMP.LT.-1) THEN
	      WRITE (6,'('' ERROR: Expiration must be > -1.'')')
	   ELSE
	      FOLDER_BBEXPIRE = TEMP
	      WRITE (6,'('' Default expiration modified.'')')
	   END IF
	   CALL REWRITE_FOLDER_FILE
	   CALL CLOSE_BULLFOLDER
	ELSE
	   WRITE (6,'('' You are not authorized to set expiration.'')')
	END IF

	RETURN
	END
