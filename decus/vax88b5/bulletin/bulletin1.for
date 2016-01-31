From:	CSBVAX::MRGATE!MRL%PFC-VAX.MIT.EDU%XX.LCS.MIT.EDU@EDDIE.MIT.EDU@SMTP 16-AUG-1988 17:05
To:	ARISIA::EVERHART
Subj:	BULLETIN1.FOR


Received: from deep-thought.mit.edu by EDDIE.MIT.EDU via Chaosnet with MAIL with sendmail-5.45/4.7 id <AA05497@EDDIE.MIT.EDU>; Tue, 16 Aug 8
8 10:38:43 EDT
Message-Id: <8808161438.AA05497@EDDIE.MIT.EDU>
Received: from PFC-VAX.MIT.EDU by DEEP-THOUGHT.MIT.EDU via Chaosnet; 16 Aug 88 10:38-EDT
Date: 16 Aug 88 10:37:58 EDT
From: MRL%PFC-VAX.MIT.EDU%XX.LCS.MIT.EDU@EDDIE.MIT.EDU
To: TENCATI@VLSI.JPL.NASA.GOV@EE, MHG@MITRE-BEDFORD.ARPA@EE,
        EVERHART%ARISIA.DECNET@GE-CRD.ARPA@EE, GAYMAN@ARI-HQ1.ARPA@EE
Subject: BULLETIN1.FOR

C
C  BULLETIN1.FOR, Version 7/13/88
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

	IF (CLI$PRESENT('HEADER')) THEN		! Printout header?
	   WRITE(3,'(A)') 'Description: '//DESCRIP ! Output bulletin header info
	   IF (EXDATE(8:9).EQ.'18'.OR.INDEX(EXDATE,'1900').GT.0) THEN
	      INPUT = 'From: '//FROM//' Date: '//DATE//' (DELETED)'
	   ELSE IF ((SYSTEM.AND.4).EQ.4) THEN	! Is entry shutdown bulletin?
	      INPUT = 'From: '//FROM//' Date: '//DATE//' Expires on shutdown'
	   ELSE IF ((SYSTEM.AND.2).EQ.2) THEN	! Is entry permanent bulletin?
	      INPUT = 'From: '//FROM//' Date: '//DATE//' Permanent'
	   ELSE
	      INPUT = 'From: '//FROM//' Date: '//DATE//' '//DATE(:5)//
     &			' Expires: '//EXDATE//' '//EXTIME(:5)
	   END IF
	   IF ((SYSTEM.AND.1).EQ.1) THEN	! System bulletin?
	      INPUT = INPUT(:TRIM(INPUT))//' / System'
	   END IF
	   WRITE (3,'(A)') INPUT
	   WRITE (3,*)
	END IF

	CALL OPEN_FILE_SHARED(1)	! Open BULLETIN file

	LEN_I = 81
	DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file
	   DO WHILE (LEN_I.GT.0)
	      CALL GET_BULL(I,INPUT,LEN_I)
	      IF (LEN_I.LT.0) THEN
		 GO TO 90
	      ELSE IF (LEN_I.GT.0) THEN
	         WRITE (3,'(A)') INPUT(:LEN_I)
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

	IER = CLI$GET_VALUE('RECIPIENTS',BULL_PARAMETER,LEN_P)

	CALL DISABLE_PRIVS
	CALL LIB$SPAWN('$MAIL SYS$LOGIN:BULL.SCR '//BULL_PARAMETER(:LEN_P)
     &	   //'/SUBJECT="'//MAIL_SUBJECT(:LEN_D)//'"',,,,,,STATUS)
	CALL ENABLE_PRIVS

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

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

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
	      WRITE (6,'('' ERROR: No privileges to modify folder owner.'')')
	   ELSE
	      FOLDER1_OWNER = FOLDER1_OWNER(:LEN_P)
	   END IF
	ELSE
	   FOLDER1_OWNER = FOLDER_OWNER
	END IF

	CALL OPEN_FILE(7)		! Open folder file

	IF (CLI$PRESENT('NAME')) THEN
	   READ (7,IOSTAT=IER,KEY=FOLDER1,KEYID=0)
					! See if folder exists
	   IF (IER.EQ.0) THEN
	      WRITE (6,'('' ERROR: Folder name already exists.'')')
	      CALL CLOSE_FILE(7)
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

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /REMOTE_READ_MESSAGE/ SCRATCH_R1
	DATA SCRATCH_R1 /0/

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'C

	INCLUDE 'BULLFOLDER.INC'i

	INCLUDE 'BULLFILES.INC'

	EXTERNAL CLI$_ABSENT

	LOGICAL DELETE_ORIGINAL

	CHARACTER INPUT*128,SAVE_FOLDER*25e

	IF (CLI$PRESENT('ORIGINAL').AND..NOT.SETPRV_PRIV()) THENe
	   WRITE (6,:
     &	  '('' ERROR: You have no privileges to keep original owner.'')')
	END IFE

	ALL = CLI$PRESENT('ALL')L

	MERGE = CLI$PRESENT('MERGE')R

	SAVE_BULL_POINT = BULL_POINTE

	IF (.NOT.ALL) THENY
	   IF (BULL_POINT.EQ.0) THEN	! If no message has been readI
	      WRITE(6,'('' ERROR: You are not reading any message.'')')
	      RETURN			! and return
	   END IF

	   CALL OPEN_FILE_SHARED(2)
	   CALL READDIR(BULL_POINT,IER)		! Get message directory entry
	   IF (IER.NE.BULL_POINT+1) THEN	! Was message found?
	      WRITE(6,'('' ERROR: Specified message was not found.'')')
	      CALL CLOSE_FILE(2)
	      RETURNI
	   END IF

	   NUM_COPY = 1
	ELSEL
	   CALL OPEN_FILE_SHARED(2)
	   CALL READDIR(0,IER)		! Get message directory entry
	   IF (NBULL.EQ.0) THEN		! Were messages found?
	      WRITE(6,'('' ERROR: No messages were found.'')') 
	      CALL CLOSE_FILE(2)a
	      RETURN 
	   END IF

	   NUM_COPY = NBULL
	   BULL_POINT = 1
	END IFJ

	FROM_REMOTE = REMOTE_SETE

	IF (REMOTE_SET) THEN 
	   OPEN (UNIT=12,FILE='REMOTE.BULLDIR',
     &	      STATUS='SCRATCH',FORM='UNFORMATTED',N
     &	      RECORDTYPE='FIXED',RECORDSIZE=DIR_RECORD_LENGTH/4,
     &	      ORGANIZATION='INDEXED',IOSTAT=IER,P
     &	      KEY=(9:12:INTEGER,1:8:CHARACTER),ACCESS='KEYED')f
	   IF (IER.EQ.0) THEN
	      OPEN (UNIT=11,FILE='REMOTE.BULLFIL',i
     &	         STATUS='SCRATCH',IOSTAT=IER,
     &	         ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=32,e
     &	         FORM='UNFORMATTED')L
	   END IF
	   IF (IER.EQ.0) THEN
	      CALL OPEN_FILE(1)
	      I = BULL_POINT - 1E
	      CALL READDIR(I,IER)
	      IF (IER.EQ.I+1) THENR
		 IF (I.EQ.0) THENo
	            WRITE (12,IOSTAT=IER1) BULLDIR_HEADER
		 ELSE 
	            WRITE (12,IOSTAT=IER1) BULLDIR_ENTRY 
		 END IF(
	      END IFo
	      NBLOCK = 1p
	      DO WHILE (I.LT.BULL_POINT+NUM_COPY-1.AND.IER.EQ.I+1)X
		 I = I + 1
	         CALL READDIR(I,IER)r
		 IF (IER.EQ.I+1) THENA
		    BLOCK = NBLOCK
		    CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)n
	            WRITE (12,IOSTAT=IER1) BULLDIR_ENTRYO
	            IF (IER1.EQ.0) THEN
	               WRITE (REMOTE_UNIT,'(2A)',IOSTAT=IER1) 5,I
	               IF (IER1.GT.0) THEN'
	                  CALL DISCONNECT_REMOTE'
	               ELSE
	                  CALL GET_REMOTE_MESSAGE(IER1)
	               END IF
	            END IFI
		    IF (IER1.EQ.0) THEN(
	               SCRATCH_R = SCRATCH_R1
	               DO J=1,LENGTHM
	                CALL READ_QUEUE(%VAL(SCRATCH_R),SCRATCH_R,INPUT) 
		        WRITE (11'NBLOCK,IOSTAT=IER1) INPUTD
		        NBLOCK = NBLOCK + 1N
		       END DOL
		    END IF
		    IF (IER1.NE.0) I = IER
		 END IFI
	      END DO
	      NUM_COPY = I - BULL_POINT + 1
	   END IF
	   CALL CLOSE_FILE(1)
	   IF (IER1.NE.0) THEN(
	      WRITE(6,'('' ERROR: Copy aborted. Remote folder problem.'')')
	      CLOSE (UNIT=11)
	      CLOSE (UNIT=12)
	      CALL CLOSE_FILE(2) 
	      RETURN
	   END IF
	END IF

	CALL CLOSE_FILE(2)S
	   
	SAVE_FOLDER = FOLDERE
	SAVE_FOLDER_NUMBER = FOLDER_NUMBERa
	CALL CLI$GET_VALUE('FOLDER',FOLDER1)E

	FOLDER_NUMBER = -1	! Use FOLDER as key rather than FOLDER_NUMBERQ
	CALL SELECT_FOLDER(.FALSE.,IER)

	IF (.NOT.IER) THEN(
	   WRITE (6,'('' ERROR: Cannot access specified folder.'')')"
	   FOLDER_NUMBER = SAVE_FOLDER_NUMBER
	   FOLDER = SAVE_FOLDER
	   BULL_POINT = SAVE_BULL_POINT
	   CLOSE (UNIT=11)=
	   CLOSE (UNIT=12)I
	   RETURN
	END IFL

	IF (READ_ONLY.OR.(MERGE.AND.REMOTE_SET)) THEN
	   IF (READ_ONLY) THEN/
	      WRITE (6,'('' ERROR: No access to write into folder.'')')
	   ELSE
	      WRITE (6,'('' ERROR: /MERGE invalid into remote folder.'')')N
	   END IF
	   FOLDER_NUMBER = SAVE_FOLDER_NUMBER
	   FOLDER1 = SAVE_FOLDERO
	   CALL SELECT_FOLDER(.FALSE.,IER1)
	   BULL_POINT = SAVE_BULL_POINT
	   CLOSE (UNIT=11)E
	   CLOSE (UNIT=12) 
	   RETURN
	END IFT

CN
C  Add bulletin to bulletin file and directory entry for to directory file.U
CS

	CALL OPEN_FILE(2)			! Prepare to add dir entry$

	CALL OPEN_FILE(1)			! Prepare to add bulletin

	CALL READDIR(0,IER)			! Get NBLOCKE
	IF (IER.EQ.0) NBLOCK = 0		! If new file, NBLOCK is 0e

	FOLDER1_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY)).
     &		//SAVE_FOLDER

	IF (.NOT.FROM_REMOTE) THENr
	   DO WHILE (FILE_LOCK(IER,IER1))
	    OPEN (UNIT=12,FILE=FOLDER1_FILE(:TRIM(FOLDER1_FILE))F
     &	      //'.BULLDIR',STATUS='OLD',FORM='UNFORMATTED',
     &	      RECORDTYPE='FIXED',RECORDSIZE=DIR_RECORD_LENGTH/4, 
     &	      ORGANIZATION='INDEXED',IOSTAT=IER,L
     &	      KEY=(9:12:INTEGER,1:8:CHARACTER),ACCESS='KEYED')R
	   END DO

	   IF (IER.EQ.0) THEN
	      DO WHILE (FILE_LOCK(IER,IER1)) 
	       OPEN (UNIT=11,FILE=FOLDER1_FILE(:TRIM(FOLDER1_FILE))
     &	         //'.BULLFIL',STATUS='UNKNOWN',IOSTAT=IER,
     &	         ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=32,E
     &	         FORM='UNFORMATTED')
	      END DOI
	   END IF
	ELSEL
	   IER= 0
	END IFe

	IF (MERGE) CALL INITIALIZE_MERGE(IER)

	IF (IER.EQ.0) READ (12,KEYID=0,KEY=BULL_POINT-1,IOSTAT=IER)

	DO WHILE (NUM_COPY.GT.0.AND.IER.EQ.0)
	   READ (12,IOSTAT=IER) BULLDIR_ENTRY
	   NUM_COPY = NUM_COPY - 1)

	   CALL GET_MSGKEY(MSG_BTIM,MSG_KEY) 
	   CALL CONVERT_ENTRY_FROMBIN

	   IF (.NOT.BTEST(FOLDER_FLAG,2).OR.	! Not system folder?
     &		 .NOT.SETPRV_PRIV()) THEN	! Or no privileges?'
	      SYSTEM = IBCLR(SYSTEM,0)		! Remove system bit
	   END IF

	   IF (BTEST(SYSTEM,2).AND.		! Shutdown message?1
     &	    (.NOT.BTEST(FOLDER_FLAG,2).OR.	! Not system folder?
     &		 .NOT.SETPRV_PRIV())) THEN	! Or no privileges?
	      SYSTEM = IBCLR(SYSTEM,2)		! Remove shutdown bit
	      CALL GET_EXDATE(EXDATE,FOLDER_BBEXPIRE)
	      EXTIME = '00:00:00.00' 
	   ELSE IF (BTEST(SYSTEM,1).AND.FOLDER_NUMBER.EQ.0.AND.
     &		 .NOT.SETPRV_PRIV().AND..NOT.ALL) THEN	! Permanent?)
	      WRITE (6,'('' ERROR: No privileges to add'',_
     &				'' permanent message.'')')
	      WRITE (6,'('' Expiration will be '',I,'' days.'')')
     &				FOLDER_BBEXPIREE
	      SYSTEM = IBCLR(SYSTEM,1)T
	      CALL GET_EXDATE(EXDATE,FOLDER_BBEXPIRE)
	      EXTIME = '00:00:00.00'H
	   END IF

	   IF (.NOT.CLI$PRESENT('ORIGINAL')) THEN	! If not /ORIGINAL 
	      FROM = USERNAME			! Specify owner
	   END IF

	   IF (REMOTE_SET) THEN
	      WRITE (REMOTE_UNIT,'(A)',IOSTAT=IER) 2N
	      IF (IER.NE.0) CALL ERROR_AND_EXIT
	   END IF

	   IF (MERGE) CALL ADD_MERGE_TO(IER)Y

	   IF (IER.EQ.0) THEN
	      NBLOCK = NBLOCK + 1

	      DO I=BLOCK,BLOCK+LENGTH-1
	         READ (11'I,IOSTAT=IER) INPUT
	         IF (IER.EQ.0) THEN
		    CALL WRITE_BULL_FILE(NBLOCK,INPUT)
	         END IF
	         NBLOCK = NBLOCK + 1
	      END DOR
	   END IF

	   IF (IER.EQ.0) THEN
	      IF (MERGE) THEN
		 CALL ADD_MERGE_FROM(IER)I
	      ELSE 
	         CALL ADD_ENTRY		! Add the new directory entry 
	      END IFF
	      BULL_POINT = BULL_POINT + 1
	   END IF
	END DO_

	IF (MERGE) CALL ADD_MERGE_REST(IER)

	CALL CLOSE_FILE(1)			! Finished adding bulletin

	CLOSE (UNIT=11)

	CLOSE (UNIT=12)

	IF (FOLDER_NUMBER.GE.0.AND.IER.EQ.0) THEN
	   CALL UPDATE_FOLDER			! Update folder infoo
Cr
C  If user is adding message, update that user's last read time forR
C  folder, so user is not alerted of new message which is owned by user.
C_
	   LAST_READ_BTIM(1,FOLDER_NUMBER+1) = F_NEWEST_BTIM(1)
	   LAST_READ_BTIM(2,FOLDER_NUMBER+1) = F_NEWEST_BTIM(2)
	END IFf

	CALL CLOSE_FILE(2)			! Totally finished with add-


	IF (IER.EQ.0) THENU
	   WRITE (6,'('' Successful copy to folder '',A)')C
     &		FOLDER(:TRIM(FOLDER))//'.'
	   IF (MERGE) THEN
	      CALL LIB$DELETE_FILE(FOLDER_FILE(:TRIM(FOLDER_FILE))//_
     &		  '.BULLDIR;-1')
	   END IF
	ELSE_
	   IF (MERGE) I = 0
	   WRITE (6,'('' ERROR: Copy aborted. '',I,'' files copied.'')')E
     &			BULL_POINT-SAVE_BULL_POINTI
	END IF
 E
	FOLDER_NUMBER = SAVE_FOLDER_NUMBERT
	FOLDER1 = SAVE_FOLDER
	CALL SELECT_FOLDER(.FALSE.,IER1)C

	BULL_POINT = SAVE_BULL_POINTE

	IF (DELETE_ORIGINAL.AND.IER.EQ.0) THEN 
	   IF (FROM_REMOTE.AND.ALL) THEN 
	      WRITE (6,'('' WARNING: Original messages not deleted.'')')L
	      WRITE (6,'('' Multiple deletions not possible for '',
     &			''remote folders.'')') 
	   ELSE
	      CALL DELETE
	   END IF
	END IFn

	RETURN 

	END




	SUBROUTINE PRINTr
Ci
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

	CHARACTER*80 INPUTe

	COMMON /POINT/ BULL_POINT

	INCLUDE 'BULLDIR.INC'

	IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been readL
	   WRITE(6,1010)		! Write error
	   RETURN			! And returnt
	END IFd

	CALL OPEN_FILE_SHARED(2).

	CALL READDIR(BULL_POINT,IER)	! Get info for specified bulletine

	IF (IER.NE.BULL_POINT+1) THEN	! Was bulletin found?
	   WRITE(6,1030)N
	   CALL CLOSE_FILE(2)		! If not, then error out
	   RETURN
	END IFO

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
	   WRITE(3,1060) FROM,DATE//' '//TIME(:5)
	END IFM

	ILEN = 81
	DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file
	   DO WHILE (ILEN.GT.0)
	      CALL GET_BULL(I,INPUT,ILEN)
	      IF (ILEN.GT.0) WRITE(3,'(A)') INPUT(:TRIM(INPUT))
	   END DO
	   ILEN = 80A
	END DOU

	CLOSE (UNIT=3)			! Bulletin copy completedI

	CALL CLOSE_FILE(1)R

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(18,SJC$_FILE_SPECIFICATION,
     &		%LOC('SYS$LOGIN:BULL.LIS')) 

	IER = CLI$GET_VALUE('QUEUE',QUEUE,ILEN) 	! Get queue name
	IF (ILEN.EQ.0) THEN
	   QUEUE = 'SYS$PRINT'M
	   ILEN = 9
	END IFW

	CALL ADD_2_ITMLST(ILEN,SJC$_QUEUE,%LOC(QUEUE))I
	CALL ADD_2_ITMLST(0,SJC$_DELETE_FILE,0)

	IF (CLI$PRESENT('NOTIFY')) THEN
	   CALL ADD_2_ITMLST(0,SJC$_NOTIFY,0)
	END IF 

	IF (.NOT.SETPRV_PRIV()) THEN		! If no SETPRV, remove SYSPRV
	   CALL DISABLE_PRIVS			! privileges when trying to
	END IF					! create new file.

	CALL END_ITMLST(SJC_ITMLST)
	 
	IER=SYS$SNDJBCW(,%VAL(SJC$_ENTER_FILE),,%VAL(SJC_ITMLST),IOSB,,) 
	IF (IER.AND.(.NOT.JBC_ERROR)) THENC
	   CALL SYS_GETMSG(JBC_ERROR)
	   IER = LIB$DELETE_FILE('SYS$LOGIN:BULL.LIS;')
	ELSE IF (.NOT.IER) THEN
	   CALL SYS_GETMSG(IER)
	   IER = LIB$DELETE_FILE('SYS$LOGIN:BULL.LIS;')
	END IFD

	CALL ENABLE_PRIVS			! Reset SYSPRV privileges

	RETURNL

900	CALL ERRSNS(IDUMMY,IER).
	CALL ENABLE_PRIVS			! Reset SYSPRV privileges
	CLOSE (UNIT=3,STATUS='DELETE') 
	CALL CLOSE_FILE(1)
	WRITE(6,1000)
	CALL SYS_GETMSG(IER)S

	RETURN

1000	FORMAT(' ERROR: Unable to open temporary file
     & SYS$LOGIN:BULL.LIS for printing.')_
1010	FORMAT(' ERROR: You have not read any message.')L
1030	FORMAT(' ERROR: Specified message was not found.') 
1040	FORMAT(' Message ',I4,' written to ',A)
1050	FORMAT('Description: ',A53)
1060	FORMAT('From: ',A12,' Date: ',A,/)e

	END




	SUBROUTINE READ(READ_COUNT,BULL_READ)
C_
C  SUBROUTINE READ
CV
C  FUNCTION: Reads a specified bulletin.
C 
C  PARAMETER:1
C	READ_COUNT - Variable to store the record in the message fileO
C		that READ will read from.  Must be set to 0 to indicate
C		that it is the first read of the message.  If -1,
C		READ will search for the last message in the message file
C		and read that one.  If -2, just display header information.
C	BULL_READ - Message number to be read.
C 
	IMPLICIT INTEGER (A - Z).

	COMMON /POINT/ BULL_POINT

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	COMMON/INPUT_BULL/INPUT
	CHARACTER*80 INPUTt

	COMMON /READIT/ READIT

	COMMON /PAGE/ PAGE_LENGTH,PAGINGt
	LOGICAL PAGING

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD

	DATA SCRATCH_B1/0/G

	CHARACTER TODAY*11,DATETIME*23 

	LOGICAL SINCE,PAGEK

	CALL LIB$ERASE_PAGE(1,1)		! Clear screenR
	END = 0					! Nothing outputted on screen

	IF (READ_COUNT.GT.0) GO TO 100		! Skip init steps if this isE
						! not first page of bulletin

	SINCE = .FALSE.
	PAGE = .TRUE.
	 
	IF (.NOT.PAGING) PAGE = .FALSE.
	IF (INCMD(:4).EQ.'READ') THEN		! If READ command...
	 IF (.NOT.CLI$PRESENT('PAGE')) PAGE = .FALSE.
	 IF (CLI$PRESENT('SINCE')) THEN		! was /SINCE specified?R
	   IER = CLI$GET_VALUE('SINCE',DATETIME)O
	   IF (DATETIME.EQ.'TODAY') THEN	! TODAY is the default.I
	      IER = SYS$BINTIM('-- 00:00:00.00',TODAY)T
	      CALL GET_MSGKEY(TODAY,MSG_KEY).
 	   ELSEA
	      CALL SYS_BINTIM(DATETIME,MSG_BTIM) 
	      CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
 	   END IF 
	 ELSE IF (CLI$PRESENT('NEW')) THEN	! was /NEW specified?F
	   DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &			       F_NEWEST_BTIM)
	   IF (DIFF.GE.0) THENP
	      WRITE (6,'('' No new messages are present.'')')
	      RETURN(
	   ELSE
 	      CALL GET_MSGKEY(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &							MSG_KEY)G
	   END IF
	 END IF
	 IF (CLI$PRESENT('SINCE').OR.CLI$PRESENT('NEW')) THEN
	   CALL OPEN_FILE_SHARED(2)
           CALL READDIR_KEYGE(IER)
	   CALL CLOSE_FILE(2)
	   IF (IER.EQ.0) THEN
	      WRITE (6,'('' No messages past specified date.'')')
	      RETURNm
	   ELSE
	      BULL_READ = IER
	      IER = IER + 1
	   END IF
	   SINCE = .TRUE.
	 END IF
	END IFN

	IF (.NOT.SINCE) THEN 
	 IF (BULL_READ.GT.0) THEN		! Valid bulletin number?
	   CALL OPEN_FILE_SHARED(2)
	   CALL READDIR(BULL_READ,IER)		! Get bulletin directory entryT
	   IF (READ_COUNT.EQ.-1.AND.IER.NE.BULL_READ+1) THENE
	      READ_COUNT = 0L
	      CALL READDIR(0,IER)
	      IF (NBULL.GT.0) THENi
	         BULL_READ = NBULL'
	         CALL READDIR(BULL_READ,IER)(
	      ELSEt
		 IER = 0
	      END IF)
	   END IF
	   CALL CLOSE_FILE(2)
	 ELSE
	   IER = 0T
	 END IF
	END IF 

	IF (IER.NE.BULL_READ+1) THEN		! Was bulletin found?
	   WRITE(6,1030)			! If not, then error out
	   RETURN
	END IFN

	DIFF = COMPARE_BTIM(MSG_BTIM,LAST_READ_BTIM(1,FOLDER_NUMBER+1))
	IF (DIFF.GT.0) THEN
	   LAST_READ_BTIM(1,FOLDER_NUMBER+1) = MSG_BTIM(1)I
	   LAST_READ_BTIM(2,FOLDER_NUMBER+1) = MSG_BTIM(2) 
	END IF

	BULL_POINT = BULL_READ			! Update bulletin counterQ

	IF (INCMD(:4).EQ.'READ'.OR.INCMD(:4).EQ.'CURR') THEN,
	   IF (CLI$PRESENT('EDIT')) THENI
	      CALL READ_EDIT 
	      RETURNQ
	   END IF
	END IFW

	FLEN = TRIM(FOLDER)
	WRITE(6,1040) BULL_POINT,FOLDER(:FLEN)	! Output bulletin header info
	WRITE(6,1050) DESCRIP
	IF (EXDATE(8:9).EQ.'18'.OR.INDEX(EXDATE,'1900').GT.0) THENI
	   WRITE(6,1060) FROM,DATE//' '//TIME(:5),'(DELETED)'
	ELSE IF ((SYSTEM.AND.4).EQ.4) THEN	! Is entry shutdown bulletin?T
	   WRITE(6,1060) FROM,DATE//' '//TIME(:5),'Expires on shutdown'
	ELSE IF ((SYSTEM.AND.2).EQ.2) THEN	! Is entry permanent bulletin?
	   WRITE(6,1060) FROM,DATE//' '//TIME(:5),'Permanent'
	ELSE0
	   WRITE(6,1060) FROM,DATE//' '//TIME(:5),d
     &				'Expires: '//EXDATE//' '//EXTIME(:5)
	END IFs
	IF ((SYSTEM.AND.1).EQ.1) THEN		! System bulletin?
	   WRITE(6,'(''+ / System'',/)')e
	ELSEe
	   WRITE(6,'(''+'',/)')
	END IF_
CB
C  Each page of the bulletin is buffered into temporary memory storage before(
C  being outputted to the terminal.  This is to be able to quickly close the
C  bulletin file, and to avoid the possibility of the user holding the screen,
C  and thus causing the bulletin file to stay open.  The temporary memory(
C  is structured as a linked-list queue, where SCRATCH_B1 points to the header
C  of the queue.  See BULLSUBS.FOR for more description of the queue.f
Cs

	IF (SCRATCH_B1.NE.0) THEN		! Is queue empty?N
	   SCRATCH_B = SCRATCH_B1		! No, set queue pointer to headE
	ELSE					! Else if queue is empty
	   CALL INIT_QUEUE(SCRATCH_B,INPUT)
	   SCRATCH_B1 = SCRATCH_B		! Init header pointerT
	END IF 

	END = 4					! Outputted 4 lines to screen

	READ_ALREADY = 0			! Number of lines already read
						! from record.
	IF (READ_COUNT.EQ.-2) THEN		! Just output header first read
	   READ_COUNT = BLOCK
	   RETURN
	ELSE 
	   READ_COUNT = BLOCK			! Init bulletin record counterN
	END IF

100	SCRATCH_B = SCRATCH_B1			! Init queue pointer to headeru
	IF (READIT.GT.0) END = END - 2		! /READ can output 2 more lines
	DISPLAY = 0
	IF (READ_COUNT.GT.BLOCK.AND.READIT.EQ.0) THEN ! If not 1st page of READ
	   WRITE(6,1040) BULL_POINT,FOLDER(:FLEN) ! Output bulletin header info
	   END = END + 1			  ! Increase display counter
	END IFi
	CALL OPEN_FILE_SHARED(1)		! Get bulletin file
	MORE_LINES = .TRUE.
	READ_REC = READ_COUNT
	IF (READ_ALREADY.EQ.0) ILEN = 81E
	DO WHILE (MORE_LINES.AND.READ_REC.LE.BLOCK+LENGTH-1)e
	   DO WHILE (ILEN.GT.0.AND.MORE_LINES)b
	      CALL GET_BULL(READ_REC,INPUT,ILEN)L
	      IF (ILEN.LT.0) THEN		! Error, couldn't read recordI
		 READ_REC = BLOCK + LENGTH	! Fake end of reading file 
		 MORE_LINES = .FALSE. 
	      ELSE IF (ILEN.GT.0) THENo
		 LEN_TEMP = ILEN
		 CALL CONVERT_TABS(INPUT,LEN_TEMP)
	         CALL WRITE_QUEUE(%VAL(SCRATCH_B),SCRATCH_B,INPUT)N
	         READ_ALREADY = 1
		 DISPLAY = DISPLAY + 1
		 IF ((DISPLAY.EQ.PAGE_LENGTH-END-4).AND.PAGE) THEN
		    MORE_LINES = .FALSE.
		 END IF)
	      END IFo
	   END DO
	   ILEN = 800
	   IF (MORE_LINES) THEN
	      READ_REC = READ_REC + 1
	      READ_ALREADY = 0)
	   END IF
	END DO 

	CALL CLOSE_FILE(1)			! End of bulletin file readi

C 
C  Bulletin page is now in temporary memory, so output to terminal. 
C  Note that if this is a /READ, the first line will have problems withA
C  the usual FORMAT statement.  It will cause a blank line to be outputted
C  at the top of the screen.  This is because of the input QIO at the1
C  end of the previous page.  The output gets confused and thinks it mustC
C  end the previous line.  To prevent that, the first line of a new page
C  in a /READ must use a different FORMAT statement to surpress the CR/LF.
C$

	SCRATCH_B = SCRATCH_B1			! Reinit queue pointer to head
	DO I=1,DISPLAY				! Output page to terminal
	   CALL READ_QUEUE(%VAL(SCRATCH_B),SCRATCH_B,INPUT) ! Get queue recordE
	   IF (I.EQ.1.AND.READ_REC.NE.BLOCK.AND.READIT.GT.0) THEN
	      WRITE(6,2020) INPUT(:TRIM(INPUT))	! (See above comments)L
	   ELSE
	      WRITE(6,2010) INPUT(:TRIM(INPUT))
	   END IF
	END DOA

	READ_COUNT = READ_REC			! Update bull record counterE

	IF (READ_REC.EQ.BLOCK+LENGTH) THEN	! Last block?E
	   READ_COUNT = 0			! init bulletin record counterH
	ELSE IF (READ_REC.EQ.BLOCK+LENGTH-1.AND..NOT.MORE_LINES) THEN
		! Possibly last block since end of page could be last line
	   CALL TEST_MORE_LINES(ILEN)		! More lines to read?.
	   IF (ILEN.GT.0) THEN			! Yes, there are still moreE
	      IF (READIT.EQ.0) WRITE(6,1070)	! say there is more of bulletin 
	   ELSE					! Yes, last line anyway
	      READ_COUNT = 0			! init bulletin record counter
	   END IF
	ELSE IF (READIT.EQ.0) THEN		! Else if this is not /READ
	   WRITE(6,1070)			! say there is more of bulletins
	END IFn

	RETURN 

1030	FORMAT(' ERROR: Specified message was not found.')(
1040	FORMAT('+Message number: ',I4,<60-FLEN>X,A)
1050	FORMAT(' Description: ',A53)O
1060	FORMAT(' From: ',A12,' Date: ',A,' ',A,$)
1070	FORMAT(1X,/,' Press RETURN for more...',/)e

2000	FORMAT(A)
2010	FORMAT(1X,A)T
2020	FORMAT('+',A)

	END




	SUBROUTINE READ_EDITa

	IMPLICIT INTEGER (A-Z)t

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'o

	CHARACTER*128 INPUT

	OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,
     &		STATUS='NEW',CARRIAGECONTROL='LIST')

	IF (IER.NE.0) THEN.
	   CALL ERRSNS(IDUMMY,IER)r
	   CALL SYS_GETMSG(IER)
	   RETURN
	END IF

	WRITE(3,1050) DESCRIP		! Output bulletin header infoI
	WRITE(3,1060) FROM,DATE//' '//TIME(:5)L

	CALL OPEN_FILE_SHARED(1)U

	ILEN = 81
	DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file
	   DO WHILE (ILEN.GT.0)
	      CALL GET_BULL(I,INPUT,ILEN)
	      IF (ILEN.LT.0) THEN
		 GO TO 90
	      ELSE IF (ILEN.GT.0) THENR
	         WRITE (3,'(A)') INPUT(:ILEN)
	      END IF 
	   END DO
	   ILEN = 80r
	END DO

90	CLOSE (UNIT=3)			! Bulletin copy completed 
	CALL CLOSE_FILE(1)T

	CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')

	CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;*')

1050	FORMAT(/,'Description: ',A53)
1060	FORMAT('From: ',A12,' Date: ',A,/) 

	RETURNR
	END


	SUBROUTINE READNEW(REDO)T
CA
C  SUBROUTINE READNEW 
C(
C  FUNCTION: Displays new non-system bulletins with prompts between bulletins.
CE

	IMPLICIT INTEGER (A-Z)I

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	INCLUDE 'BULLUSER.INC' 

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'C

	COMMON /SYSTEM_FOLDERS/ SYSTEM_FLAG(FLONG),DUMMY(2)

	COMMON /POINT/ BULL_POINT

	CHARACTER INREAD*1,INPUT*80,FILE_DEF*80,NUMREAD*5

	DATA LEN_FILE_DEF /0/, INREAD/0/A

	LOGICAL SLOW,SLOW_TERMINAL,

	FIRST_MESSAGE = BULL_POINT)

	IF (ICHAR(INREAD).EQ.0) THEN	! If calling READNEW for first timep
	   SLOW = SLOW_TERMINAL()	! Check baud rate of terminal
	END IF				! to avoid gobs of output

	LEN_P = 0			! Tells read subroutine there is
					! no bulletin parameter

1	WRITE(6,1000)			! Ask if want to read new bulletins 

	CALL GET_INPUT_NUM(NUMREAD,NLEN)	! Get inputF
	CALL STR$UPCASE(NUMREAD,NUMREAD)	! Make input upper cases
	READ (NUMREAD,'(I<NLEN>)',IOSTAT=IER) TEMP_READ
	IF (IER.NE.0) THENE
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
		 END DOD
		 CALL EXIT
	      ELSEE
	         WRITE (6,'(''+o'',$)')
	      END IF 
	      RETURN	! If NO, exit 
			! Include QUIT to be consistent with next question+
	   ELSE
	      CALL LIB$ERASE_PAGE(1,1),
	   END IF
	END IFn

3	IF (TEMP_READ.GT.0) THEN
	   IF (TEMP_READ.LT.FIRST_MESSAGE+1.OR.TEMP_READ.GT.NBULL) THEN
	      WRITE (6,'('' ERROR: Specified new message not found.'')') 
	      GO TO 1
	   ELSE
	      BULL_POINT = TEMP_READ - 1M
	   END IF
	END IFL

	READ_COUNT = 0				! Initialize display pointer

5	CALL READ(READ_COUNT,BULL_POINT+1)	! Read next bulletinF
	FILE_POINT = BULL_POINT
	IF (READ_COUNT.EQ.0) THEN		! Is full bulletin displayed?D
	   CALL OPEN_FILE_SHARED(2)		! If so, see if more new bulls
10	   CALL READDIR(BULL_POINT+1,IER_POINT)
	   IF ((IER_POINT.EQ.BULL_POINT+2).AND.	! If system bulletin (and systemT
     &	       (SYSTEM.AND.BTEST(FOLDER_FLAG,2))) THEN	! folder) then skip it..
	      BULL_POINT = BULL_POINT + 1
	      GO TO 10 
	   END IF
	   CALL CLOSE_FILE(2)
	END IFe

12	IF (READ_COUNT.EQ.0) THEN		! Prompt user in between
	   WRITE(6,1020)			! full screens or end of bull.
	ELSE/
	   WRITE(6,1030)'
	END IF

	CALL GET_INPUT_NOECHO(INREAD)
	CALL STR$UPCASE(INREAD,INREAD)	! Convert input to upper caseD

	IF (INREAD.EQ.'Q') THEN		! If Q , then QUIT
	   WRITE (6,'(''+Quit'',$)')y
	   RETURN
	ELSE IF (INREAD.EQ.'D') THEN	! If D , then redisplay directory 
	   WRITE (6,'(''+Dir'',$)')
	   REDO = .TRUE.o
	   RETURN
	ELSE IF (INREAD.EQ.'F') THEN	! If F then copy bulletin to filek
	   WRITE (6,'(''+ '')')		! Move cursor from end of prompt lineh
					! to beginning of next line.t
	   IF (LEN_FILE_DEF.EQ.0) THENs
	      CALL LIB$SYS_TRNLOG('SYS$LOGIN',LEN,FILE_DEF)
	      IER = LIB$FIND_FILE(FILE_DEF//'BULL.DIR',
     &			BULL_PARAMETER,CONTEXT)
	      IF (IER) THEN
		 FILE_DEF = BULL_PARAMETER(:LEN-1)//'.BULL]'
		 LEN_FILE_DEF = LEN + 5y
	      ELSEC
	         FILE_DEF = 'SYS$LOGIN:'o
	         LEN_FILE_DEF = 10l
	      END IFe
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
	   IF (.NOT.SETPRV_PRIV()) THEN		! If no SETPRV, remove SYSPRVa
	      CALL DISABLE_PRIVS		! privileges when trying to
	   END IF				! create new file.
	   OPEN(UNIT=3,FILE=BULL_PARAMETER(:LEN_P),IOSTAT=IER,ERR=18,
     &		STATUS='UNKNOWN',CARRIAGECONTROL='LIST',ACCESS='APPEND')
	   WRITE(3,1050) DESCRIP		! Output bulletin header info
	   WRITE(3,1060) FROM,DATE//' '//TIME(:5)
	   ILEN = 81O
	   DO I=BLOCK,BLOCK+LENGTH-1		! Copy bulletin into file
	      DO WHILE (ILEN.GT.0)I
	         CALL GET_BULL(I,INPUT,ILEN)e
		 IF (ILEN.LT.0) THEN
		   GO TO 18H
		 ELSE IF (ILEN.GT.0) THEN
	            WRITE(3,'(A)') INPUT(:TRIM(INPUT)).
		 END IF	
	      END DON
	      ILEN = 80
	   END DO
	   WRITE(6,1040) BULL_PARAMETER(:LEN_P)
						! Show name of file created.
18	   IF (IER.NE.0) THEN
	      CALL ERRSNS(IDUMMY,IER)
	      CALL SYS_GETMSG(IER)N
	   END IF
	   CLOSE (UNIT=3)			! Bulletin copy completed
	   CALL CLOSE_FILE(1)
	   CALL CLOSE_FILE(2)
	   LENGTH = LENGTH_SAVE
	   BLOCK = BLOCK_SAVE
	   CALL ENABLE_PRIVS			! Reset BYPASS privilegesL
	   GO TO 12
	ELSE IF (INREAD.EQ.'N'.AND.READ_COUNT.GT.0) THEN 
				! If NEXT and last bulletins not finishede
	   READ_COUNT = 0			! Reset read bulletin counter
	   CALL OPEN_FILE_SHARED(2)		! Look for NEXT bulletin
20	   CALL READDIR(BULL_POINT+1,IER)
	   IF (IER.NE.BULL_POINT+2) THEN	! If no NEXT bulletins
	      CALL CLOSE_FILE(2)		! Exito
	      WRITE(6,1010)
	      RETURN 
	   ELSE IF (SYSTEM.AND.BTEST(FOLDER_FLAG,2)) THEN
	      BULL_POINT = BULL_POINT + 1	! If SYSTEM bulletin, skip it
	      GO TO 20			! Look for more bulletinsr
	   END IF
	   CALL CLOSE_FILE(2)
	ELSE IF (INREAD.EQ.'R') THENt
	   WRITE (6,'(''+Read'')')	
	   WRITE (6,'('' Enter message number: '',$)')V
	   CALL GET_INPUT_NUM(NUMREAD,NLEN)	! Get input
	   CALL STR$UPCASE(NUMREAD,NUMREAD)	! Make input upper case
	   READ (NUMREAD,'(I<NLEN>)',IOSTAT=IER) TEMP_READ 
	   IF (IER.NE.0.OR.TEMP_READ.LE.0) THEN
	      WRITE (6,'('' ERROR: Invalid message number specified.'')')
	      GO TO 12r
	   ELSE
	      GO TO 3
	   END IF
	ELSE IF (IER_POINT.NE.BULL_POINT+2.AND.READ_COUNT.EQ.0) THENl
	   WRITE(6,1010)H
	   RETURN
	END IFE
	IF (READ_COUNT.EQ.0.AND.SLOW) READ_COUNT = -2
	GO TO 5

1000	FORMAT(' Read messages? Type N(No),E(Exit),messageS
     & number, or any other key for yes: ',$) 
1010	FORMAT(' No more messages.')a
1020	FORMAT(1X,80('-'),/,' Type Q(Quit),
     & F(File it), D(Dir), R(Read msg #) or other for next message: ',$)
1030	FORMAT(1X,80('-'),/,' Type Q(Quit), F(File), N(Next),
     & D(Dir), R(Read msg #) or other for MORE: ',$)
1040	FORMAT(' Message written to ',A)	
1050	FORMAT(/,'Description: ',A53)
1060	FORMAT('From: ',A12,' Date: ',A20,/):

	END
