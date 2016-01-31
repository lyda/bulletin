From:	HENRY::IN%"MRL%PFC-VAX.MIT.EDU%xx.lcs.mit.edu%zermatt.lcs.mit.edu%csnet-relay.CSNET%relay.cs.net@RCA.COM" 13-DEC-1987 08:49
To:	MHG <MHG%mitre-bedford.arpa@zermatt>, 
Subj:	BULLETIN1.FOR

C
C  BULLETIN1.FOR, Version 11/24/87
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

	CALL OPEN_FILE_SHARED(1)	! Open BULLETIN file

	LEN_I = 81
	DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file
	   DO WHILE (LEN_I.GT.0)
	      CALL GET_BULL(I,INPUT,LEN_I)
	      IF (LEN_I.LT.0) THEN
		 GO TO 90
	      ELSE IF (LEN_I.GT.0) THEN
	         WRITE (3,'(A)') INPUT(1:LEN_I)
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

	IF (MAIL_SUBJECT(1:1).NE.'"') THEN
	   MAIL_SUBJECT = '"'//MAIL_SUBJECT(1:LEN_D)
	   LEN_D = LEN_D + 1
	END IF

	IF (MAIL_SUBJECT(LEN_D:LEN_D).NE.'"') THEN
	   MAIL_SUBJECT = MAIL_SUBJECT(1:LEN_D)//'"'
	   LEN_D = LEN_D + 1
	END IF

	IER = CLI$GET_VALUE('RECIPIENTS',BULL_PARAMETER,LEN_P)
	   
	CALL LIB$SPAWN('$MAIL SYS$LOGIN:BULL.SCR '//BULL_PARAMETER(1:LEN_P)
     &	   //'/SUBJECT='//MAIL_SUBJECT(1:LEN_D),,,,,,STATUS)

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

	IF (FOLDER_NUMBER.EQ.0) THEN
	   WRITE (6,'('' ERROR: Cannot modify GENERAL folder.'')')
	   RETURN
	ELSE IF (FOLDER_OWNER.NE.USERNAME.AND..NOT.SETPRV_PRIV()) THEN
	   WRITE (6,'('' ERROR: No privileges to modify folder.'')')
	   RETURN
	END IF

	IF (CLI$PRESENT('NAME')) THEN
	   CALL CLI$GET_VALUE('NAME',FOLDER1,LEN_P)
	   IF (LEN_P.GT.25) THEN
	      WRITE (6,'('' ERROR: Folder name cannot be larger 
     &				than 25 characters.'')')
	      RETURN
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
	       FOLDER1_DESCRIP = FOLDER1_DESCRIP(1:LEN_P) ! End fill with spaces
	    END IF
	   END DO
	ELSE
	   FOLDER1_DESCRIP = FOLDER_DESCRIP
	END IF

	IF (CLI$PRESENT('OWNER')) THEN
	   CALL CLI$GET_VALUE('OWNER',FOLDER1_OWNER,LEN_P)
	   IF (LEN_P.GT.LEN(FOLDER1_OWNER)) THEN
	      WRITE (6,'('' ERROR: Folder owner name too long.'')')
	      RETURN
	   ELSE IF (.NOT.SETPRV_PRIV()) THEN
	      WRITE (6,'('' ERROR: No privileges to modify folder owner.'')')
	   ELSE
	      FOLDER1_OWNER = FOLDER1_OWNER(1:LEN_P)
	   END IF
	ELSE
	   FOLDER1_OWNER = FOLDER_OWNER
	END IF

	CALL OPEN_FILE(7)		! Open folder file

	IF (CLI$PRESENT('NAME')) THEN
	   READ (7,FMT=FOLDER_FMT,IOSTAT=IER,KEY=FOLDER1,KEYID=0)
					! See if folder exists
	   IF (IER.EQ.0) THEN
	      WRITE (6,'('' ERROR: Folder name already exists.'')')
	      CALL CLOSE_FILE(7)
	      RETURN
	   END IF
	END IF

	READ (7,FMT=FOLDER_FMT,IOSTAT=IER,KEY=FOLDER,KEYID=0)
     &			FOLDER,FOLDER_NUMBER,FOLDER_OWNER,
     &			FOLDER_DESCRIP,FOLDER_BBOARD,FOLDER_BBEXPIRE,
     &			USERB,GROUPB,ACCOUNTB,
     &			F_NBULL,F_NEWEST_BTIM,FOLDER_FLAG,F_EXPIRE_LIMIT

	IF (IER.EQ.0) THEN
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
     &		(FOLDER_FILE(1:TRIM(FOLDER_FILE))//'.BULLFIL',IER)
	      IF (IER.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL)) THEN
	         CALL ADD_ACL(FOLDER1_OWNER,'R+W+C',IER)
	         CALL DEL_ACL(FOLDER_OWNER,'R+W+C',IER)
	      END IF
	   END IF
	   FOLDER = FOLDER1
	   FOLDER_OWNER = FOLDER1_OWNER
	   FOLDER_DESCRIP = FOLDER1_DESCRIP
	   DELETE (7)
	   WRITE (7,FMT=FOLDER_FMT,IOSTAT=IER) 
     &			FOLDER,FOLDER_NUMBER,FOLDER_OWNER,
     &			FOLDER_DESCRIP,FOLDER_BBOARD,FOLDER_BBEXPIRE,
     &			USERB,GROUPB,ACCOUNTB,
     &			F_NBULL,F_NEWEST_BTIM,FOLDER_FLAG,F_EXPIRE_LIMIT
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

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	EXTERNAL CLI$_ABSENT

	LOGICAL DELETE_ORIGINAL

	CHARACTER INPUT*80,SAVE_USERNAME*12

	CHARACTER*116 BULLDIR_COM_SAVE

	IF (BULL_POINT.EQ.0) THEN	! If no message has been read
	   WRITE(6,'('' ERROR: You are not reading any message.'')')
	   RETURN			! and return
	END IF

	CALL OPEN_FILE_SHARED(2)
	CALL READDIR(BULL_POINT,IER)		! Get message directory entry
	CALL CLOSE_FILE(2)
	IF (IER.NE.BULL_POINT+1) THEN		! Was message found?
	   WRITE(6,'('' ERROR: Specified message was not found.'')')
	   RETURN
	END IF

	CALL LIB$MOVC3(116,%REF(BULLDIR_COM),%REF(BULLDIR_COM_SAVE))
	SAVE_BULL_POINT = BULL_POINT

	OPEN (UNIT=3,STATUS='SCRATCH',FILE='SYS$LOGIN:BULL.SCR',
     &		FORM='FORMATTED')		! Scratch file to save bulletin

	CALL OPEN_FILE_SHARED(1)

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

90	REWIND (UNIT=3)			! Bulletin copy completed

	CALL CLOSE_FILE(1)

	SAVE_FOLDER_NUMBER = FOLDER_NUMBER
	CALL CLI$GET_VALUE('FOLDER',FOLDER1)

	SAVE_USERNAME = USERNAME
	IF (CLI$PRESENT('ORIGINAL')) THEN
	 IF (SETPRV_PRIV()) THENU
	  USERNAME = FROM
	 ELSE
	  WRITE (6,
     &	  '('' ERROR: You have no privileges to keep original owner.'')')
	 END IF
	END IF>

	FOLDER_NUMBER = -1	! Use FOLDER as key rather than FOLDER_NUMBER 
	CALL SELECT_FOLDER(.FALSE.,IER)

	IF (.NOT.IER.OR.READ_ONLY) THEN
	   WRITE (6,'('' ERROR: Cannot access specified folder.'')') 
	   CLOSE (UNIT=3)
	   FOLDER_NUMBER = SAVE_FOLDER_NUMBER
	   USERNAME = SAVE_USERNAME
	   RETURN
	END IF 

Ct
C  Add bulletin to bulletin file and directory entry for to directory file.
CC

	CALL OPEN_FILE(2)			! Prepare to add dir entryP

	CALL OPEN_FILE(1)			! Prepare to add bulletin

	CALL READDIR(0,IER)			! Get NBLOCKU
	IF (IER.EQ.0) NBLOCK = 0		! If new file, NBLOCK is 0.

	CALL COPY_BULL(3,1,NBLOCK+1,IER)	! Add the new bulletin
	IF (IER.NE.0) THEN			! Error in creating bulletin
	   WRITE(6,'('' ERROR: Message copy aborted.'')')
	   CALL CLOSE_FILE(1)
	   CALL CLOSE_FILE(2)
	   CLOSE (UNIT=3)
	END IF,

	CALL CLOSE_FILE(1)			! Finished adding bulletin

	CALL LIB$MOVC3(116,%REF(BULLDIR_COM_SAVE),%REF(BULLDIR_COM))a

	SYSTEM = IBCLR(SYSTEM,0)		! Remove system bit
	IF (BTEST(SYSTEM,2)) THEN		! Shutdown message?U
	   SYSTEM = IBCLR(SYSTEM,2)		! Remove shutdown bitI
	   CALL GET_EXDATE(EXDATE,FOLDER_BBEXPIRE)o
	ELSE IF (BTEST(SYSTEM,1).AND.FOLDER_NUMBER.EQ.0.AND. 
     &		 .NOT.SETPRV_PRIV()) THEN	! Permanent?
	  WRITE (6,'('' ERROR: No privileges to add permanent message.'')')
	  WRITE (6,'('' Expiration will be '',I,'' days.'')') FOLDER_BBEXPIRE
	END IFL

	FROM = USERNAME				! Specify ownerR
	CALL ADD_ENTRY				! Add the new directory entry

	CALL UPDATE_FOLDER			! Update folder info
CR
C  If user is adding message, update that user's last read time for1
C  folder, so user is not alerted of new message which is owned by user.
CI
	LAST_READ_BTIM(1,FOLDER_NUMBER+1) = F_NEWEST_BTIM(1)9
	LAST_READ_BTIM(2,FOLDER_NUMBER+1) = F_NEWEST_BTIM(2)3

	CALL CLOSE_FILE(2)			! Totally finished with add 

	CLOSE (UNIT=3)			! Close the input file

	WRITE (6,'('' Message has been copied to folder '',A)')
     &		FOLDER(1:TRIM(FOLDER))//'.'.

	USERNAME = SAVE_USERNAME'

	FOLDER_NUMBER = SAVE_FOLDER_NUMBERI
	CALL SELECT_FOLDER(.FALSE.,IER)

	BULL_POINT = SAVE_BULL_POINTA

	IF (DELETE_ORIGINAL) CALL DELETED

	RETURN_

	END




	SUBROUTINE PRINTI
CU
C  SUBROUTINE PRINT.
C)
C  FUNCTION:  Print header to queue.
C:

	IMPLICIT INTEGER (A-Z)E

	INCLUDE '($SJCDEF)'

	CHARACTER*32 QUEUEI

	INTEGER*2 FILE_ID(14)
	INTEGER*2 IOSB(4)
	EQUIVALENCE (IOSB(1),JBC_ERROR)

	CHARACTER*80 INPUT_

	COMMON /POINT/ BULL_POINT

	INCLUDE 'BULLDIR.INC'

	IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been readR
	   WRITE(6,1010)		! Write error
	   RETURN			! And returnO
	END IFI

	CALL OPEN_FILE_SHARED(2)M

	CALL READDIR(BULL_POINT,IER)	! Get info for specified bulletin

	IF (IER.NE.BULL_POINT+1) THEN	! Was bulletin found?
	   WRITE(6,1030)C
	   CALL CLOSE_FILE(2)		! If not, then error out
	   RETURN
	END IFE

	CALL CLOSE_FILE(2)S

	CALL OPEN_FILE_SHARED(1)	! Open BULLETIN file

	IF (.NOT.SETPRV_PRIV()) THEN		! If no SETPRV, remove SYSPRV
	   CALL DISABLE_PRIVS			! privileges when trying to
	END IF					! create new file.

	OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.LIS',ERR=900,IOSTAT=IER,
     &		STATUS='NEW',CARRIAGECONTROL='LIST')

	CALL ENABLE_PRIVS

	IF (CLI$PRESENT('HEADER')) THEN		! Printout header?
	   WRITE(3,1050) DESCRIP		! Output bulletin header info
	   WRITE(3,1060) FROM,DATE 
	END IFR

	LEN =81
	DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file
	   DO WHILE (LEN.GT.0)T
	      CALL GET_BULL(I,INPUT,LEN)i
	      IF (LEN.GT.0) WRITE(3,'(A)') INPUT(1:TRIM(INPUT))
	   END DO
	   LEN = 80
	END DOE

	CLOSE (UNIT=3)			! Bulletin copy completedI

	CALL CLOSE_FILE(1) 

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(18,SJC$_FILE_SPECIFICATION,
     &		%LOC('SYS$LOGIN:BULL.LIS'))T

	IER = CLI$GET_VALUE('QUEUE',QUEUE,LEN) 		! Get queue name
	IF (LEN.EQ.0) THEND
	   QUEUE = 'SYS$PRINT'C
	   LEN = 9!
	END IF 

	CALL ADD_2_ITMLST(LEN,SJC$_QUEUE,%LOC(QUEUE))
	CALL ADD_2_ITMLST(0,SJC$_DELETE_FILE,0)

	IF (CLI$PRESENT('NOTIFY')) THEN
	   CALL ADD_2_ITMLST(0,SJC$_NOTIFY,0)
	END IFL

	IF (.NOT.SETPRV_PRIV()) THEN		! If no SETPRV, remove SYSPRV
	   CALL DISABLE_PRIVS			! privileges when trying to
	END IF					! create new file.

	CALL END_ITMLST(SJC_ITMLST)
	,
	IER=SYS$SNDJBCW(,%VAL(SJC$_ENTER_FILE),,%VAL(SJC_ITMLST),IOSB,,) 
	IF (IER.AND.(.NOT.JBC_ERROR)) THENL
	   CALL SYS_GETMSG(JBC_ERROR)
	   IER = LIB$DELETE_FILE('SYS$LOGIN:BULL.LIS;')
	ELSE IF (.NOT.IER) THEN
	   CALL SYS_GETMSG(IER)
	   IER = LIB$DELETE_FILE('SYS$LOGIN:BULL.LIS;')
	END IFD

	CALL ENABLE_PRIVS			! Reset SYSPRV privileges

	RETURN 

900	CALL ERRSNS(IDUMMY,IER)R
	CALL ENABLE_PRIVS			! Reset SYSPRV privileges
	CLOSE (UNIT=3,STATUS='DELETE') 
	CALL CLOSE_FILE(1)
	WRITE(6,1000)
	CALL SYS_GETMSG(IER)F

	RETURN=

1000	FORMAT(' ERROR: Unable to open temporary file
     & SYS$LOGIN:BULL.LIS for printing.')E
1010	FORMAT(' ERROR: You have not read any message.')U
1030	FORMAT(' ERROR: Specified message was not found.')E
1040	FORMAT(' Message ',I4,' written to ',A)
1050	FORMAT('Description: ',A53)
1060	FORMAT('From: ',A12,' Date: ',A11,/)F

	END




	SUBROUTINE READ(READ_COUNT,BULL_READ)
CT
C  SUBROUTINE READ
C 
C  FUNCTION: Reads a specified bulletin.
CE
C  PARAMETER:_
C	READ_COUNT - Variable to store the record in the message file)
C		that READ will read from.  Must be set to 0 to indicate
C		that it is the first read of the message.  If -1,
C		READ will search for the last message in the message file
C		and read that one.  If -2, just display header information.
C	BULL_READ - Message number to be read.
C 
	IMPLICIT INTEGER (A - Z)O

	COMMON /POINT/ BULL_POINT

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'P

	INCLUDE 'BULLUSER.INC' 

	COMMON/INPUT_BULL/INPUT
	CHARACTER*80 INPUTO

	COMMON /READIT/ READIT

	COMMON /PAGE/ PAGE_LENGTH,PAGING,
	LOGICAL PAGING

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD

	DATA SCRATCH_B1/0/F

	DIMENSION MSG_BTIM(2)

	CHARACTER TODAY*11,DATETIME*23s

	LOGICAL SINCE,PAGEE

	CALL LIB$ERASE_PAGE(1,1)		! Clear screen'
	END = 0					! Nothing outputted on screen

	IF (READ_COUNT.GT.0) GO TO 100		! Skip init steps if this isO
						! not first page of bulletin

	SINCE = .FALSE.
	PAGE = .TRUE.
	s
	IF (.NOT.PAGING) PAGE = .FALSE.
	IF (INCMD(1:4).EQ.'READ') THEN		! If READ command...U
	 IF (.NOT.CLI$PRESENT('PAGE')) PAGE = .FALSE.
	 IF (CLI$PRESENT('SINCE')) THEN		! was /SINCE specified?N
	   IER = CLI$GET_VALUE('SINCE',DATETIME).
	   IF (DATETIME.EQ.'TODAY') THEN
	      IER = SYS$ASCTIM(,TODAY,,)	! Get today's date
	      DATETIME = TODAY//' 00:00:00.0'
	   END IF
	 ELSE IF (CLI$PRESENT('NEW')) THEN	! was /NEW specified?B
	   DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &			       F_NEWEST_BTIM)
	   IF (DIFF.GE.0) THEN 
	      WRITE (6,'('' No new messages are present.'')')
	      RETURN 
	   ELSE
	      CALL SYS$ASCTIM
     &		(,DATETIME,LAST_READ_BTIM(1,FOLDER_NUMBER+1),)
	   END IF
	 END IF
	 IF (CLI$PRESENT('SINCE').OR.CLI$PRESENT('NEW')) THEN
	   CALL OPEN_FILE_SHARED(2)
	   TEMP_READ = 0
	   IER = 1O
	   DO WHILE (IER.EQ.TEMP_READ+1)D
	      TEMP_READ = TEMP_READ + 1
	      CALL READDIR(TEMP_READ,IER)
	      IF (IER.NE.TEMP_READ+1) THEN
		 WRITE (6,'('' No messages found past specified date.'')')
		 CALL CLOSE_FILE(2)D
		 RETURNN
	      ELSE=
	         DIFF = COMPARE_DATE(DATETIME(1:11),DATE)  ! Compare expiration
	         IF (DIFF.EQ.0) DIFF = COMPARE_TIME(DATETIME(13:20),TIME)
	         IF (DIFF.LT.0) THENT
		    BULL_READ = TEMP_READ'
		    IER = IER + 1 
		 END IF
	      END IF 
	   END DO
	   IER = BULL_READ + 1I
	   SINCE = .TRUE.
	   CALL CLOSE_FILE(2)
	 END IF
	END IFV

	IF (.NOT.SINCE) THENN
	 IF (BULL_READ.GT.0) THEN		! Valid bulletin number?
	   CALL OPEN_FILE_SHARED(2)
	   CALL READDIR(BULL_READ,IER)		! Get bulletin directory entryN
	   IF (READ_COUNT.EQ.-1.AND.IER.NE.BULL_READ+1) THENY
	      READ_COUNT = 0o
	      CALL READDIR(0,IER)
	      IF (NBULL.GT.0) THEN_
	         BULL_READ = NBULLe
	         CALL READDIR(BULL_READ,IER)E
	      ELSEL
		 IER = 0
	      END IFR
	   END IF
	   CALL CLOSE_FILE(2)
	 ELSE
	   IER = 0s
	 END IF
	END IF'

	IF (IER.NE.BULL_READ+1) THEN		! Was bulletin found?
	   WRITE(6,1030)			! If not, then error out
	   RETURN
	END IF 

	CALL SYS_BINTIM(DATE//' '//TIME,MSG_BTIM)
	DIFF = COMPARE_BTIM(MSG_BTIM,LAST_READ_BTIM(1,FOLDER_NUMBER+1))
	IF (DIFF.GT.0) THEN
	   LAST_READ_BTIM(1,FOLDER_NUMBER+1) = MSG_BTIM(1)L
	   LAST_READ_BTIM(2,FOLDER_NUMBER+1) = MSG_BTIM(2)C
	END IFI

	BULL_POINT = BULL_READ			! Update bulletin counter1

	FLEN = TRIM(FOLDER)
	WRITE(6,1040) BULL_POINT,FOLDER(:FLEN)	! Output bulletin header info(
	WRITE(6,1050) DESCRIP
	IF (EXDATE(8:9).EQ.'18'.OR.INDEX(EXDATE,'1900').GT.0) THEN
	   WRITE(6,1060) FROM,DATE,'(DELETED)'S
	ELSE IF ((SYSTEM.AND.4).EQ.4) THEN	! Is entry shutdown bulletin?B
	   WRITE(6,1060) FROM,DATE,'Expires on shutdown'B
	ELSE IF ((SYSTEM.AND.2).EQ.2) THEN	! Is entry permanent bulletin?
	   WRITE(6,1060) FROM,DATE,'Permanent'E
	ELSE 
	   WRITE(6,1060) FROM,DATE,'Expires: '//EXDATE//' '//EXTIME
	END IFS
	IF ((SYSTEM.AND.1).EQ.1) THEN		! System bulletin?
	   WRITE(6,'(''+ / System'',/)') 
	ELSEn
	   WRITE(6,'(''+'',/)')
	END IFi
Ce
C  Each page of the bulletin is buffered into temporary memory storage beforea
C  being outputted to the terminal.  This is to be able to quickly close the
C  bulletin file, and to avoid the possibility of the user holding the screen,
C  and thus causing the bulletin file to stay open.  The temporary memoryd
C  is structured as a linked-list queue, where SCRATCH_B1 points to the header
C  of the queue.  See BULLSUBS.FOR for more description of the queue._
CD

	IF (SCRATCH_B1.NE.0) THEN		! Is queue empty?C
	   SCRATCH_B = SCRATCH_B1		! No, set queue pointer to headN
	ELSE					! Else if queue is empty
	   CALL INIT_QUEUE(SCRATCH_B,INPUT)
	   SCRATCH_B1 = SCRATCH_B		! Init header pointer.
	END IFE

	END = 4					! Outputted 4 lines to screen

	READ_ALREADY = 0			! Number of lines already read
						! from record.
	IF (READ_COUNT.EQ.-2) THEN		! Just output header first read
	   READ_COUNT = BLOCK
	   RETURN
	ELSEB
	   READ_COUNT = BLOCK			! Init bulletin record counter
	END IFP

100	SCRATCH_B = SCRATCH_B1			! Init queue pointer to headerU
	IF (READIT.GT.0) END = END - 2		! /READ can output 2 more lines
	DISPLAY = 0
	IF (READ_COUNT.GT.BLOCK.AND.READIT.EQ.0) THEN ! If not 1st page of READ
	   WRITE(6,1040) BULL_POINT,FOLDER(:FLEN) ! Output bulletin header info
	   END = END + 1			  ! Increase display counter
	END IFO
	CALL OPEN_FILE_SHARED(1)		! Get bulletin file
	MORE_LINES = .TRUE.
	READ_REC = READ_COUNT
	IF (READ_ALREADY.EQ.0) LEN = 81
	DO WHILE (MORE_LINES.AND.READ_REC.LE.BLOCK+LENGTH-1)C
	   DO WHILE (LEN.GT.0.AND.MORE_LINES)
	      CALL GET_BULL(READ_REC,INPUT,LEN)
	      IF (LEN.LT.0) THEN		! Error, couldn't read record
		 READ_REC = BLOCK + LENGTH	! Fake end of reading filer
		 MORE_LINES = .FALSE.S
	      ELSE IF (LEN.GT.0) THEN
		 LEN_TEMP = LEN	
		 CALL CONVERT_TABS(INPUT,LEN_TEMP)
	         CALL WRITE_QUEUE(%VAL(SCRATCH_B),SCRATCH_B,INPUT)'
	         READ_ALREADY = 1
		 DISPLAY = DISPLAY + 1
		 IF ((DISPLAY.EQ.PAGE_LENGTH-END-4).AND.PAGE) THEN
		    MORE_LINES = .FALSE.
		 END IFl
	      END IFo
	   END DO
	   LEN = 80
	   IF (MORE_LINES) THEN
	      READ_REC = READ_REC + 1
	      READ_ALREADY = 0 
	   END IF
	END DOT

	CALL CLOSE_FILE(1)			! End of bulletin file readG

CW
C  Bulletin page is now in temporary memory, so output to terminal.E
C  Note that if this is a /READ, the first line will have problems with
C  the usual FORMAT statement.  It will cause a blank line to be outputted
C  at the top of the screen.  This is because of the input QIO at theV
C  end of the previous page.  The output gets confused and thinks it must 
C  end the previous line.  To prevent that, the first line of a new page
C  in a /READ must use a different FORMAT statement to surpress the CR/LF.
CO

	SCRATCH_B = SCRATCH_B1			! Reinit queue pointer to head
	DO I=1,DISPLAY				! Output page to terminal
	   CALL READ_QUEUE(%VAL(SCRATCH_B),SCRATCH_B,INPUT) ! Get queue recordo
	   IF (I.EQ.1.AND.READ_REC.NE.BLOCK.AND.READIT.GT.0) THEN
	      WRITE(6,2020) INPUT(1:TRIM(INPUT))	! (See above comments)
	   ELSE
	      WRITE(6,2010) INPUT(1:TRIM(INPUT)) 
	   END IF
	END DOR

	READ_COUNT = READ_REC			! Update bull record counterE

	IF (READ_REC.EQ.BLOCK+LENGTH) THEN	! Last block? 
	   READ_COUNT = 0			! init bulletin record counterD
	ELSE IF (READ_REC.EQ.BLOCK+LENGTH-1.AND..NOT.MORE_LINES) THEN
		! Possibly last block since end of page could be last line
	   CALL TEST_MORE_LINES(LEN)		! More lines to read?
	   IF (LEN.GT.0) THEN			! Yes, there are still more
	      IF (READIT.EQ.0) WRITE(6,1070)	! say there is more of bulletin
	   ELSE					! Yes, last line anyway
	      READ_COUNT = 0			! init bulletin record counter
	   END IF
	ELSE IF (READIT.EQ.0) THEN		! Else if this is not /READ
	   WRITE(6,1070)			! say there is more of bulletinD
	END IFn

	RETURN0

1030	FORMAT(' ERROR: Specified message was not found.')S
1040	FORMAT('+Message number: ',I4,<60-FLEN>X,A)
1050	FORMAT(' Description: ',A53)a
1060	FORMAT(' From: ',A12,' Date: ',A11,' ',A,$)
1070	FORMAT(1X,/,' Press RETURN for more...',/)e

2000	FORMAT(A)
2010	FORMAT(1X,A) 
2020	FORMAT('+',A)

	END





	SUBROUTINE READNEW(REDO) 
Cs
C  SUBROUTINE READNEWi
Cs
C  FUNCTION: Displays new non-system bulletins with prompts between bulletins.
Cd

	IMPLICIT INTEGER (A-Z)

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC''

	COMMON /POINT/ BULL_POINT

	CHARACTER INREAD*1,INPUT*80,FILE_DEF*80,NUMREAD*5

	DATA LEN_FILE_DEF /0/, INREAD/0/R

	LOGICAL SLOW,SLOW_TERMINAL/

	IF (ICHAR(INREAD).EQ.0) THEN	! If calling READNEW for first timeD
	   SLOW = SLOW_TERMINAL()	! Check baud rate of terminal
	END IF				! to avoid gobs of output

	LEN_P = 0			! Tells read subroutine there is$
					! no bulletin parameter

1	WRITE(6,1000)			! Ask if want to read new bulletinsA

	CALL GET_INPUT_NUM(NUMREAD,NLEN)	! Get inputs
	CALL STR$UPCASE(NUMREAD,NUMREAD)	! Make input upper case.
	READ (NUMREAD,'(I<NLEN>)',IOSTAT=IER) TEMP_READ
	IF (IER.NE.0) THEN.
	   INREAD = NUMREAD(1:1)c
	   IF (INREAD.EQ.'N'.OR.INREAD.EQ.'Q'.OR.INREAD.EQ.'E') THENF
	      IF (INREAD.EQ.'Q') THEN
	         WRITE (6,'(''+uit'',$)')
	      ELSE IF (INREAD.EQ.'E') THENF
	         WRITE (6,'(''+xit'',$)')
		 CALL EXIT
	      ELSE	
	         WRITE (6,'(''+o'',$)')
	      END IF:
	      RETURN	! If NO, exitI
			! Include QUIT to be consistent with next question 
	   ELSE
	      CALL LIB$ERASE_PAGE(1,1)_
	   END IF
	END IF 

	IF (TEMP_READ.GT.0) THEN(
	   IF (TEMP_READ.LT.BULL_POINT+1.OR.TEMP_READ.GT.NBULL) THENe
	      WRITE (6,'('' ERROR: Specified new message not found.'')')	
	      GO TO 1
	   ELSE
	      BULL_POINT = TEMP_READ - 1 
	   END IF
	END IFS

	READ_COUNT = 0				! Initialize display pointerO

5	CALL READ(READ_COUNT,BULL_POINT+1)	! Read next bulletinW
	FILE_POINT = BULL_POINT
	IF (READ_COUNT.EQ.0) THEN		! Is full bulletin displayed?P
	   CALL OPEN_FILE_SHARED(2)		! If so, see if more new bulls
10	   CALL READDIR(BULL_POINT+1,IER_POINT)
	   IF ((IER_POINT.EQ.BULL_POINT+2).AND.(SYSTEM)) THEN
	      BULL_POINT = BULL_POINT + 1	! If system bulletin, skip it.a
	      GO TO 10F
	   END IF
	   CALL CLOSE_FILE(2)
	END IF2

12	IF (READ_COUNT.EQ.0) THEN		! Prompt user in between
	   WRITE(6,1020)			! full screens or end of bull.
	ELSEI
	   WRITE(6,1030) 
	END IFL

	CALL GET_INPUT_NOECHO(INREAD)
	CALL STR$UPCASE(INREAD,INREAD)	! Convert input to upper caseE

	IF (INREAD.EQ.'Q') THEN		! If Q , then QUIT
	   WRITE (6,'(''+Quit'',$)')A
	   RETURN
	ELSE IF (INREAD.EQ.'D') THEN	! If D , then redisplay direcotyr 
	   WRITE (6,'(''+Dir'',$)')
	   REDO = .TRUE.Y
	   RETURN
	ELSE IF (INREAD.EQ.'F') THEN	! If F then copy bulletin to fileT
	   WRITE (6,'(''+ '')')		! Move cursor from end of prompt lineE
					! to beginning of next line.
	   IF (LEN_FILE_DEF.EQ.0) THENA
	      CALL LIB$SYS_TRNLOG('SYS$LOGIN',LEN,FILE_DEF)
	      IER = LIB$FIND_FILE(FILE_DEF//'BULL.DIR',
     &			BULL_PARAMETER,CONTEXT)
	      IF (IER) THEN
		 FILE_DEF = BULL_PARAMETER(:LEN-1)//'.BULL]'
		 LEN_FILE_DEF = LEN + 5 
	      ELSEM
	         FILE_DEF = 'SYS$LOGIN:'U
	         LEN_FILE_DEF = 10N
	      END IF_
	   END IF

	   LEN_FOLDER = TRIM(FOLDER)A
	   CALL GET_INPUT_PROMPT(BULL_PARAMETER,LEN_P,E
     &		'Name of file? (Default='//FILE_DEF(:LEN_FILE_DEF)//
     &		FOLDER(:LEN_FOLDER)//'.LIS) ')

	   IF (LEN_P.EQ.0) THEN
	      BULL_PARAMETER = FILE_DEF(:LEN_FILE_DEF)//FOLDER(:LEN_FOLDER)
     &			//'.LIS'X
	      LEN_P = LEN_FILE_DEF + LEN_FOLDER + 4
	   END IF

	   BLOCK_SAVE = BLOCK
	   LENGTH_SAVE = LENGTH
	   CALL OPEN_FILE_SHARED(2)
	   CALL OPEN_FILE_SHARED(1)		! Open BULLETIN file
	   CALL READDIR(FILE_POINT,IER)
	   IF (.NOT.SETPRV_PRIV()) THEN		! If no SETPRV, remove SYSPRVe
	      CALL DISABLE_PRIVS		! privileges when trying to
	   END IF				! create new file.
	   OPEN(UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),IOSTAT=IER,ERR=18, 
     &		STATUS='UNKNOWN',CARRIAGECONTROL='LIST',ACCESS='APPEND')
	   WRITE(3,1050) DESCRIP		! Output bulletin header info
	   WRITE(3,1060) FROM,DATEg
	   LEN = 81
	   DO I=BLOCK,BLOCK+LENGTH-1		! Copy bulletin into file
	      DO WHILE (LEN.GT.0)
	         CALL GET_BULL(I,INPUT,LEN)
		 IF (LEN.LT.0) THEN 
		   GO TO 18l
		 ELSE IF (LEN.GT.0) THEN
	            WRITE(3,'(A)') INPUT(1:TRIM(INPUT))
		 END IFw
	      END DO 
	      LEN = 80d
	   END DO
	   WRITE(6,1040) BULL_PARAMETER(1:LEN_P)t
						! Show name of file created.
18	   IF (IER.NE.0) THEN
	      CALL ERRSNS(IDUMMY,IER)
	      CALL SYS_GETMSG(IER)e
	   END IF
	   CLOSE (UNIT=3)			! Bulletin copy completed
	   CALL CLOSE_FILE(1)
	   CALL CLOSE_FILE(2)
	   LENGTH = LENGTH_SAVE
	   BLOCK = BLOCK_SAVE
	   CALL ENABLE_PRIVS			! Reset BYPASS privileges 
	   GO TO 12
	ELSE IF (INREAD.EQ.'N'.AND.READ_COUNT.GT.0) THENR
				! If NEXT and last bulletins not finisheds
	   READ_COUNT = 0			! Reset read bulletin counter
	   CALL OPEN_FILE_SHARED(2)		! Look for NEXT bulletin
20	   CALL READDIR(BULL_POINT+1,IER)
	   IF (IER.NE.BULL_POINT+2) THEN	! If no NEXT bulletinN
	      CALL CLOSE_FILE(2)		! Exito
	      WRITE(6,1010)
	      RETURNN
	   ELSE IF (SYSTEM) THEN		! Else if NEXT bulletin SYSTEM
	      BULL_POINT = BULL_POINT + 1	! Skip it
	      GO TO 20			! Look for more bulletins	
	   END IF
	   CALL CLOSE_FILE(2)
	ELSE IF (IER_POINT.NE.BULL_POINT+2.AND.READ_COUNT.EQ.0) THENE
	   WRITE(6,1010)E
	   RETURN
	END IFR
	IF (READ_COUNT.EQ.0.AND.SLOW) READ_COUNT = -2
	GO TO 5

1000	FORMAT(' Read messages? Type N(No),E(Exit),messageI
     & number, or any other key for yes: ',$)
1010	FORMAT(' No more messages.') 
1020	FORMAT(1X,80('-'),/,' Type Q(Quit),
     & F(File it), D(Dir) or any other key for next message: ',$) 
1030	FORMAT(1X,80('-'),/,' Type Q(Quit), F(File it), N(Next message),P
     & D(Dir), or other key for MORE: ',$)
1040	FORMAT(' Message written to ',A) 
1050	FORMAT(/,'Description: ',A53)
1060	FORMAT('From: ',A12,' Date: ',A20,/)D

	END




	SUBROUTINE SET_BBOARD(BBOARD)
CE
C  SUBROUTINE SET_BBOARD
CN
C  FUNCTION: Set username for BBOARD for selected folder.E
C 
	IMPLICIT INTEGER (A-Z)_

	PARAMETER UAF$V_DISACNT = 4

	INCLUDE 'BULLFOLDER.INC' 

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFILES.INC'

	EXTERNAL CLI$_ABSENT 

	CHARACTER EXPIRE*3,INPUT_BBOARD*12h

	IF (TRIM(BBOARD_DIRECTORY).EQ.0) THEN
	 WRITE(6,'('' ERROR: System programmer has disabled BBOARD.'')') 
	 RETURN
	END IFd

	IF (FOLDER_OWNER.EQ.USERNAME.OR.SETPRV_PRIV()) THEN

	   CALL OPEN_FILE(7)		! Open folder file.
	   READ (7,FMT=FOLDER_FMT,KEY=FOLDER,KEYID=0,IOSTAT=IER)p
     &		FOLDER,FOLDER_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP
     &		,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB
     &		,F_NBULL,F_NEWEST_BTIM,FOLDER_FLAG,F_EXPIRE_LIMITq

	   IF (BBOARD) THEN
	      IER = CLI$GET_VALUE('BB_USERNAME',INPUT_BBOARD,INPUT_LEN)
	      IF (IER.NE.%LOC(CLI$_ABSENT)) THENr
		 CALL GET_UAF.
     &		   (INPUT_BBOARD,USERB,GROUPB,ACCOUNTB,FLAGS,IER)I
	         IF (IER.AND..NOT.BTEST(FLAGS,UAF$V_DISACNT)) THEN ! DISUSER?
	            WRITE (6,'I
     &		    ('' ERROR: BBOARD account needs DISUSER flag set.'')')
		    IER = 0o
		 END IFI
		 IF (IER) THEN
	          READ (7,FMT=FOLDER_FMT,KEY='GENERAL',KEYID=0,IOSTAT=IER)r
     &		   FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIPE
     &		   ,FOLDER1_BBOARD,FOLDER1_BBEXPIREn
		  DO WHILE ((FOLDER1_BBOARD.NE.INPUT_BBOARD.OR.N
     &		     FOLDER1_NUMBER.EQ.FOLDER_NUMBER).AND.IER.EQ.0)s
	           READ (7,FMT=FOLDER_FMT,IOSTAT=IER)
     &		   FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,FOLDER1_DESCRIPs
     &		   ,FOLDER1_BBOARD,FOLDER1_BBEXPIRE 
	          END DOd
		  IF (FOLDER1_BBOARD.EQ.INPUT_BBOARD.AND.T
     &		      FOLDER1_NUMBER.NE.FOLDER_NUMBER) THEN	
		   WRITE (6,'(
     &		    '' ERROR: Account used by other folder.'')')
		   CALL CLOSE_FILE(7)n
		   RETURN
		  ELSE
	           READ (7,FMT=FOLDER_FMT,KEY=FOLDER,KEYID=0,IOSTAT=IER)'
     &		    FOLDER,FOLDER_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP
     &		    ,FOLDER_BBOARD,FOLDER_BBEXPIRE,DUMMY,DUMMY,DUMMY
     &		    ,F_NBULL,F_NEWEST_BTIM,FOLDER_FLAG,F_EXPIRE_LIMITB
		   FOLDER_BBOARD = INPUT_BBOARDO
		   IF (CLI$PRESENT('SPECIAL')) THEN	! SPECIAL specified?
		     USERB = IBSET(USERB,31)	! Set bit to show /SPECIALE
		     IF (CLI$PRESENT('VMSMAIL')) THENM
		        GROUPB = IBSET(GROUPB,31)   ! Set bit to show /VMSMAIL
		     END IFU
		   END IF'
		  END IF
		 ELSEU
		  CALL CLOSE_FILE(7)
		  RETURN
		 END IFE
	      ELSE IF (CLI$PRESENT('SPECIAL')) THEN
	       USERB = IBSET(0,31)		! Set top bit to show /SPECIALE
	       GROUPB = 0
	       DO I=1,LEN(FOLDER_BBOARD)W
		  FOLDER_BBOARD(I:I) = ' '
	       END DO
	      ELSE IF (FOLDER_BBOARD.EQ.'NONE') THENN
	       WRITE (6,'('' ERROR: No BBOARD specified for folder.'')')m
	      END IF(

	      IER = CLI$GET_VALUE('EXPIRATION',EXPIRE,EX_LEN)
	      IF (IER.NE.%LOC(CLI$_ABSENT)) THENL
	         IF (EX_LEN.GT.3) EX_LEN = 3p
	         READ (EXPIRE,'(I<EX_LEN>)') TEMP
		 IF (TEMP.GT.BBEXPIRE_LIMIT.AND..NOT.SETPRV_PRIV()) THEN
		    WRITE (6,'('' ERROR: Expiration cannot be > '',N
     &			I3,'' days.'')') BBEXPIRE_LIMIT
		    CALL CLOSE_FILE(7)
		    RETURN
		 ELSE IF (TEMP.LE.0) THENE
		    WRITE (6,'('' ERROR: Expiration must be > 0.'')') 
		    CALL CLOSE_FILE(7)
		    RETURN
		 ELSE)
		    FOLDER_BBEXPIRE = TEMP
		 END IF,
	      ELSE IF (.NOT.CLI$PRESENT('EXPIRATION')) THEN
		 FOLDER_BBEXPIRE = -1 
	      END IF_
	   ELSE
	      FOLDER_BBOARD = 'NONE'T
	   END IF

	   REWRITE (7,FMT=FOLDER_FMT,IOSTAT=IER)R
     &		FOLDER,FOLDER_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP
     &		,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB
     &		,F_NBULL,F_NEWEST_BTIM,FOLDER_FLAG,F_EXPIRE_LIMIT
	   CALL CLOSE_FILE(7)
	   WRITE (6,'('' BBOARD has been modified for folder.'')')N
	ELSER
	   WRITE (6,'('' You are not authorized to modify BBOARD.'')')0
	END IF 

	RETURNl
	END




	SUBROUTINE RESPOND(STATUS)(
C	
C  SUBROUTINE RESPONDw
Cl
C  FUNCTION: Sends a mail message in reply to a posted message.T
C.
C  NOTE: Modify the last SPAWN statement to specify the command+
C	you use to send mail to sites other than via MAIL.
C	If you always use a different command, modify both
C	spawn commands.H
C	
	IMPLICIT INTEGER (A - Z) 

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	CHARACTER INPUT*80,FROM_TEST*5E

	INCLUDE 'BULLDIR.INC'

	IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been reade
	   WRITE(6,'('' ERROR: You have not read any message.'')')(
	   RETURN			! And return,
	END IFi

	BULL_PARAMETER = 'RE: '//DESCRIPi
	IF (CLI$PRESENT('SUBJECT')) THENE
	   IER = CLI$GET_VALUE('SUBJECT',BULL_PARAMETER,LEN_P)e
	   IF (LEN_P.GT.LEN(BULL_PARAMETER)-2) THEN
	      WRITE(6,'('' ERROR: Subject limit is 64 characters.'')')e
	      RETURN_
	   END IF
	END IF

	LEN_P = TRIM(BULL_PARAMETER)O

	IF (BULL_PARAMETER(1:1).NE.'"') THEN_
	   BULL_PARAMETER = '"'//BULL_PARAMETER(1:LEN_P),
	   LEN_P = LEN_P + 1R
	END IF	

	IF (BULL_PARAMETER(LEN_P:LEN_P).NE.'"') THENL
	   BULL_PARAMETER = BULL_PARAMETER(1:LEN_P)//'"'E
	   LEN_P = LEN_P + 1 
	END IFE

	IF (CONFIRM_USER(FROM).EQ.0) THEN
	   CALL LIB$SPAWN('$MAIL SYS$INPUT '//FROM//'/SUBJECT='//
     &		BULL_PARAMETER,,,,,,STATUS)&
	ELSE 
	   FROM_TEST = ' 'F
	   CALL OPEN_FILE_SHARED(1)	! Prepare to copy message
	   L_INPUT = 81
	   I = BLOCKH
	   DO WHILE (I.LT.BLOCK+LENGTH.AND.L_INPUT.GT.0)/
	      CALL GET_BULL(I,INPUT,L_INPUT)X
	      IF (L_INPUT.GT.0) THEN 
		 CALL STR$UPCASE(FROM_TEST,INPUT(1:5))
		 IF (FROM_TEST.EQ.'FROM:') THENH
		    IF (INDEX(INPUT,'.').GT.0.OR.INDEX(INPUT,'@').GT.0
     &			.OR.INDEX(INPUT,'%').GT.0) THEN
		       L_INPUT = 0
		    END IF
		 END IFH
	      ELSE IF (L_INPUT.EQ.0) THEN
	         L_INPUT = 80
	         I = I + 1t
	      END IFN
	   END DO
	   CALL CLOSE_FILE(1)
	   IF (FROM_TEST.EQ.'FROM:') THEN
	      L_B = INDEX(INPUT,'<')T
	      R_B = INDEX(INPUT,'>')'
	      IF (L_B.GT.0.AND.R_B.GT.0) THEN
		 INPUT = INPUT(L_B+1:R_B-1)a
		 L_INPUT = R_B - 1 - L_B
	      ELSE 
		 L_INPUT = TRIM(INPUT)
		 I = 6
		 DO WHILE (INPUT(I:I).EQ.' '.AND.I.GT.0)
		    I = I + 1
		    IF (I.GT.L_INPUT) I = 0,
		 END DO 
		 INPUT = INPUT(I:L_INPUT)1
		 L_INPUT = L_INPUT - I + 1
	      END IFW
	      CALL LIB$SPAWN('$CHMAIL/I "'//INPUT(:L_INPUT)//
     &		'@XX"/SUBJECT='//BULL_PARAMETER,,,,,,STATUS)
	   ELSE
	      WRITE (6,'('' ERROR: Cannot respond to mail.'')')
	   END IF
	END IF

	RETURN 

	END


	INTEGER FUNCTION CONFIRM_USER(USERNAME)
C 
C  FUNCTION CONFIRM_USER
Cl
C  FUNCTION: Confirms that username is valid user.
CC
	IMPLICIT INTEGER (A-Z)=

	CHARACTER*(*) USERNAMEL

	CALL OPEN_FILE_SHARED(8)V

	READ (8,KEY=USERNAME,IOSTAT=CONFIRM_USER)

	CALL CLOSE_FILE(8).

	RETURNU
	END


