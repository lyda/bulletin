C
C  BULLETIN1.FOR, Version 4/8/98
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
	SUBROUTINE MAIL
C
C  SUBROUTINE MAIL
C
C  FUNCTION: Sends message which you have read to user via DEC mail.
C
	IMPLICIT INTEGER (A - Z)

	COMMON /POINT/ BULL_POINT

	COMMON /SENDTO/ SENDTO
	CHARACTER*256 SENDTO

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /NEWS_HEADER_INFO/ MSGNUM,SUBJECT_LINE,FROM_LINE
	CHARACTER*256 FROM_LINE,SUBJECT_LINE
	CHARACTER*12 MSGNUM

	INCLUDE 'BULLDIR.INC'

	EXTERNAL CLI$_ABSENT

	IF (BTEST(CAPTIVE(-1),1)) THEN
	   WRITE (6,'('' ERROR: MAIL invalid from DISMAIL account.'')')
	   RETURN
	END IF

	IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been read
	   WRITE(6,'('' ERROR: You have not read any message.'')')
	   RETURN			! And return
	END IF

	CALL OPEN_BULLDIR_SHARED

	CALL READDIR(BULL_POINT,IER)	! Get info for specified bulletin

	IF (IER.NE.BULL_POINT+1) THEN	! Was bulletin found?
	   WRITE(6,'('' ERROR: Specified message was not found.'')')
	   CALL CLOSE_BULLDIR		! If not, then error out
	   RETURN
	END IF

	CALL CLOSE_BULLDIR

	IF (CLI$PRESENT('EDIT')) THEN
	   CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;*')
	END IF

	OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,
     &	   RECL=LINE_LENGTH,STATUS='NEW',CARRIAGECONTROL='LIST')

	IF (IER.NE.0) THEN
	   WRITE(6,'('' ERROR: Error in opening scratch file.'')')
	   RETURN
	END IF

	IF (CLI$PRESENT('HEADER')) THEN		! Printout header?
	   IF (EXDATE(8:11).LT.'1995') THEN
	      IF (REMOTE_SET.NE.3) THEN
		 INPUT = 'Date:   '//DATE(:TRIM(DATE))//' '//TIME(:5)
     &				//'   (DELETED)'
	      ELSE
		 INPUT = 'Date:   '//DATE(:TRIM(DATE))//' '//TIME(:5)
	      END IF
	   ELSE IF ((SYSTEM.AND.4).EQ.4) THEN	! Is entry shutdown bulletin?
	      INPUT = 'Date:   '//DATE(:TRIM(DATE))//' '//TIME(:5)
     &				//'   Expires on shutdown'
	   ELSE IF ((SYSTEM.AND.2).EQ.2) THEN	! Is entry permanent bulletin?
	      INPUT = 'Date:   '//DATE(:TRIM(DATE))//' '//TIME(:5)
     &				//'   Permanent'
	   ELSE
	      INPUT = 'Date:   '//DATE(:TRIM(DATE))//' '//TIME(:5)//
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
	   SUBJECT_LINE = INPUT(7:ILEN)
	ELSE
	   IF (HEAD) WRITE(3,1050) DESCRIP
	   IF (ILEN.GT.0) WRITE (3,'(A)') INPUT(:ILEN)
	   SUBJECT_LINE = DESCRIP
	END IF

	IF (CLI$PRESENT('SUBJECT')) THEN
	   IER = CLI$GET_VALUE('SUBJECT',SUBJECT_LINE,LEN_D)
	END IF

	DO WHILE (ILEN.GT.0)		! Copy bulletin into file
	   CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   IF (ILEN.GT.0) WRITE (3,'(A)') INPUT(:ILEN)
	END DO

	CLOSE (UNIT=3)			! Message copy completed

	CALL CLOSE_BULLFIL

	LEN_D = TRIM(SUBJECT_LINE)
	IF (LEN_D.EQ.0) THEN
	   SUBJECT_LINE = 'BULLETIN message.'
	   LEN_D = TRIM(SUBJECT_LINE)
	END IF

	I = 1
	DO WHILE (I.LE.LEN_D)
	   IF (SUBJECT_LINE(I:I).EQ.'"') THEN
	      IF (LEN_D.EQ.64) THEN
		 SUBJECT_LINE(I:I) = '`'
	      ELSE
		 SUBJECT_LINE = SUBJECT_LINE(:I)//'"'//SUBJECT_LINE(I+1:)
		 I = I + 1
		 LEN_D = LEN_D + 1
	      END IF
	   END IF
	   I = I + 1
	END DO

	LEN_S = 0
	DO WHILE (CLI$GET_VALUE('RECIPIENTS',SENDTO(LEN_S+1:),I)
     &	    .NE.%LOC(CLI$_ABSENT))		! Get all the usernames
	   LEN_S = LEN_S + I + 1
	   SENDTO(LEN_S:LEN_S) = ','
	END DO
	LEN_S = LEN_S - 1

	I = 1		! Must change all " to """ in MAIL recipients
	DO WHILE (I.LE.LEN_S)
	   IF (SENDTO(I:I).EQ.'"') THEN
	      SENDTO = SENDTO(:I)//'""'//SENDTO(I+1:)
	      I = I + 2
	      LEN_S = LEN_S + 2
	   END IF
	   I = I + 1
	END DO

	IF (CLI$PRESENT('EDIT')) THEN
	   CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')
	   CONTEXT = 0
	   IER =  LIB$FIND_FILE('SYS$LOGIN:BULL.SCR',INPUT,CONTEXT)
	   VERSION = INDEX(INPUT,';') + 1
	   IF (INPUT(VERSION:VERSION).EQ.'1') THEN
	      CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;*')
	      WRITE (6,'('' ERROR: No message mailed.'')')
	      RETURN
	   END IF
	END IF

	CALL DISABLE_PRIVS
	CALL SENDMAIL('SYS$LOGIN:BULL.SCR',SENDTO(:LEN_S)
     &			,SUBJECT_LINE,STATUS)
C       CALL LIB$SPAWN('$MAIL SYS$LOGIN:BULL.SCR '//SENDTO(:LEN_S)
C     &    //'/SUBJECT="'//SUBJECT_LINE(:LEN_D)//'"',,,,,,STATUS)
C	IF (.NOT.STATUS) CALL SYS_GETMSG(STATUS)

	CALL ENABLE_PRIVS
	CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;*')

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

	CHARACTER RESPONSE*32

	IF (.NOT.FOLDER_ACCESS
     &		 (USERNAME,FOLDER_FLAG,FOLDER_OWNER)) THEN
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
	      IF (LEN_P.GT.44) THEN
		 WRITE (6,'('' ERROR: Folder name cannot be larger
     &				than 44 characters.'')')
		 RETURN
	      END IF
	   END IF
	ELSE
	   FOLDER1 = FOLDER
	END IF

	INIT_NEWSFEED = .FALSE.

	NEWSGROUP = .FALSE.
	MAILTO = 0

	IF (CLI$PRESENT('DESCRIPTION')) THEN
	   WRITE (6,'('' Enter one line description of folder.'')')
	   LENF = 81
	   DO WHILE (LENF.GT.80)
	      CALL GET_LINE(FOLDER1_DESCRIP,LENF)	! Get input line
	      IF (LENF.LE.0) THEN
		 WRITE (6,'('' ERROR: Folder modification aborted.'')')
		 RETURN
	      ELSE IF (LENF.GT.80) THEN			! If too many characters
		 WRITE (6,'('' ERROR: Description must be < 80 characters.'')')
	         RETURN
	      ELSE
		 FOLDER1_DESCRIP = FOLDER1_DESCRIP(:LENF) ! End fill with spaces
	      END IF
	   END DO
	   I = INDEX(FOLDER1_DESCRIP,'<')
	   J = INDEX(FOLDER1_DESCRIP,'>')
	   IF (I.GT.0.AND.J.GT.I.AND.(INDEX(FOLDER1_DESCRIP(I:),'@').LT.1
     &	       .OR.INDEX(FOLDER1_DESCRIP(I:),'@').GT.J-I+1).AND.
     &	       (INDEX(FOLDER1_DESCRIP(I:),'.')
     &	       .LE.J-I+1.AND.INDEX(FOLDER1_DESCRIP(I:),'.').GT.0)) THEN
	      NEWSGROUP = .TRUE.
	      WRITE (6,'('' Init news feed counter to feed '',
     &			 ''all messages in news group (Y),'')')
	      CALL GET_INPUT_PROMPT(RESPONSE,RLEN,
     &		 'or set to feed only new messages (N,default) ? ')
	      INIT_NEWSFEED = RESPONSE(:1).EQ.'y'.OR.RESPONSE(:1).EQ.'Y'
	      I = INDEX(FOLDER1_DESCRIP,'[')
	      J = INDEX(FOLDER1_DESCRIP,']')
	   END IF
	   IF (I.GT.0.AND.J.GT.I.AND.
     &	       (INDEX(FOLDER1_DESCRIP(I:),'@').GT.1.AND.
     &	       INDEX(FOLDER1_DESCRIP(I:),'@').LT.J-I+1)) THEN
	      MAILTO = 1
	   END IF
	   IF (I.GT.0.AND.J.GT.I.AND.(INDEX(FOLDER1_DESCRIP(I:),'.')
     &	       .GT.J-I+1.OR.INDEX(FOLDER1_DESCRIP(I:),'.').EQ.0)
     &	       .AND.MAILTO.EQ.0.AND..NOT.BTEST(FOLDER_FLAG,11)
     &	       .AND..NOT.BTEST(FOLDER_FLAG,10)) THEN 
	      CALL GET_INPUT_PROMPT(RESPONSE,RLEN,'Have you specified '//
     &		   'an email address in the description? (default=N) ')
	      IF (RESPONSE(:1).EQ.'y'.OR.RESPONSE(:1).EQ.'Y') MAILTO = 1
	   END IF
	   IF (MAILTO.EQ.1.AND..NOT.BTEST(FOLDER_FLAG,11).AND.
     &	       .NOT.BTEST(FOLDER_FLAG,10)) THEN 
	      WRITE (6,'('' A mailing address has been specified.'')')
	      CALL GET_INPUT_PROMPT(RESPONSE,RLEN,'Will messages be '//
     &		'sent to and received from this address? (default=N) ')
	      IF (RESPONSE(:1).EQ.'y'.OR.RESPONSE(:1).EQ.'Y') THEN
	         MAILTO = 2
		 WRITE (6,'('' SET POST_ONLY will be issued.'')')
	      ELSE
		 MAILTO = 3
		 WRITE (6,'('' SET ADD_ONLY will be issued.'')')
	      END IF
	   END IF
	ELSE
	   FOLDER1_DESCRIP = FOLDER_DESCRIP
	END IF

	IF (CLI$PRESENT('OWNER')) THEN
	   CALL CLI$GET_VALUE('OWNER',FOLDER1_OWNER,LEN_P)
	   IF (LEN_P.GT.12) THEN
	      WRITE (6,'('' ERROR: Owner name must be < 13 characters.'')')
	      RETURN
	   ELSE IF (CLI$PRESENT('ID')) THEN
	      IER = CHKPRO(FOLDER1_OWNER)
	   ELSE
	      CALL GET_UAF
     &		   (FOLDER1_OWNER,USERB1,GROUPB1,ACCOUNTB1,FLAGS,IER)
	   END IF
	   IF (.NOT.IER) THEN
	      WRITE (6,'('' ERROR: Owner name is not valid username.'')')
	      RETURN
	   ELSE IF (LEN_P.GT.LEN(FOLDER1_OWNER)) THEN
	      WRITE (6,'('' ERROR: Folder owner name too long.'')')
	      RETURN
	   ELSE IF (.NOT.SETPRV_PRIV()) THEN
	      WRITE(6,'('' Enter password of new owner: '',A)') CHAR(10)
	      CALL GET_INPUT_NOECHO(RESPONSE)
	      IF (TRIM(RESPONSE).EQ.0) THEN
		 WRITE (6,'('' ERROR: No password entered.'')')
		 RETURN
	      END IF
	      WRITE (6,'('' Attempting to verify password name...'')')
	      OPEN (UNIT=10,NAME='SYS$NODE"'//
     &		   FOLDER1_OWNER(:TRIM(FOLDER1_OWNER))
     &		   //' '//RESPONSE(:TRIM(RESPONSE))//'"::',
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
	   IF (.NOT.IER) THEN
	      I = 0
	      IER1 = LIB$FIND_FILE(FOLDER_DIRECTORY(:LEN_F)//
     &		FOLDER(:TRIM(FOLDER))//'.*',INPUT,I)
	   END IF
	   IF (IER.OR..NOT.IER1) THEN
	      FOLDER_FILE = FOLDER_DIRECTORY(:LEN_F)//FOLDER1
	      FOLDER_NAME = FOLDER1
	      IER = 0
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
	   IF (CLI$PRESENT('ID')) THEN
	      FOLDER_FLAG = IBSET(FOLDER_FLAG,6)
	   ELSE
	      FOLDER_FLAG = IBCLR(FOLDER_FLAG,6)
	   END IF
	   IF (NEWSGROUP) FOLDER_FLAG = IBSET(FOLDER_FLAG,4)
	   IF (MAILTO.EQ.2) FOLDER_FLAG = IBSET(FOLDER_FLAG,10)
	   IF (MAILTO.EQ.3) FOLDER_FLAG = IBSET(FOLDER_FLAG,11)
	   IF (INIT_NEWSFEED) THEN
	      F_LAST = 0
	   ELSE IF (NEWSGROUP) THEN
	      CALL CLOSE_BULLFOLDER
	      CALL OPEN_BULLNEWS_SHARED
	      I = INDEX(FOLDER_DESCRIP,'<') + 1
	      J = INDEX(FOLDER_DESCRIP,'>') - 1
	      CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER_DESCRIP(I:J),IER)
	      CALL CLOSE_BULLNEWS
	      CALL OPEN_BULLFOLDER
	      F_LAST = F1_NBULL
	   END IF
	   CALL WRITE_FOLDER_FILE(IER)
	   IF (IER.EQ.0) WRITE (6,'('' Folder successfully modified.'')')
	END IF

	IF (IER.NE.0) THEN
	   WRITE (6,'('' ERROR: Folder modification aborted.'')')
	END IF

	CALL CLOSE_BULLFOLDER

	RETURN
	END



	FUNCTION FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)

	IMPLICIT INTEGER (A-Z)

	CHARACTER*(*) USERNAME,FOLDER_OWNER

	IF (SETPRV_PRIV()) THEN
	   FOLDER_ACCESS = .TRUE.
	ELSE IF (BTEST(FOLDER_FLAG,6)) THEN	! If folder owner is ID
	   FOLDER_ACCESS = CHKPRO(FOLDER_OWNER)
	ELSE
	   FOLDER_ACCESS = USERNAME.EQ.FOLDER_OWNER
	END IF

	RETURN
	END



	SUBROUTINE MOVE(DELETE_ORIGINAL)
C
C  SUBROUTINE MOVE
C
C  FUNCTION: Moves message from one folder to another.
C
	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLFILES.INC'

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /ACCESS/ READ_ONLY
	LOGICAL READ_ONLY

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /REMOTE_READ_MESSAGE/ SCRATCH_R1
	DATA SCRATCH_R1 /0/

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*256 INCMD

	COMMON /HEADER/ HEADER

	COMMON /NEXT/ NEXT

	COMMON /NEWGROUP/ NEWGROUP

	COMMON /NEWS2BULL/ NEWS2BULL

	COMMON /FEED/ FEED

        COMMON /MAIL_INFO/ USE_INFROM

	COMMON /MAIN_HEADER_INFO/ INFROM,INDESCRIP,LEN_FROM,LEN_DESCRP
	COMMON /MAIN_HEADER_INFO/ INEXDATE
	CHARACTER*(INPUT_LENGTH) INFROM,INDESCRIP

	COMMON /BULLCP_NEWS/ BULLCP_NEWS

	EXTERNAL CLI$_ABSENT,BULLETIN_SUBCOMMANDS

	LOGICAL DELETE_ORIGINAL

	CHARACTER SAVE_FOLDER*44,POST_SUBJECT*256,TODAY*24
	CHARACTER SCRFILE*18

	DATA TEMP_FILE/.FALSE./

	DIMENSION BTIM(2)

	ORIGINAL = CLI$PRESENT('ORIGINAL')

	IF (ORIGINAL.AND..NOT.SETPRV_PRIV()) THEN
	   WRITE (6,'('' ERROR: You have no privileges to keep''
     &			,'' original owner.'')')
	   RETURN
	END IF

	ALL = CLI$PRESENT('ALL')

	MERGE = CLI$PRESENT('MERGE')

	SAVE_BULL_POINT = BULL_POINT

	FROM_REMOTE = REMOTE_SET
	CALL CLI$GET_VALUE('FOLDER',FOLDER1)
	IF (INDEX(FOLDER1,'.').GT.0) CALL LOWERCASE(FOLDER1)
	TO_NEWS = TEST_NEWS(FOLDER1)
	IF (.NOT.BULLCP_NEWS.AND.FOLDER.EQ.FOLDER1) THEN
	   WRITE (6,'('' ERROR: Destination cannot be same as'',
     &		      '' current location.'')')
	   RETURN
	END IF

	IER1 = CLI$GET_VALUE('BULLETIN_NUMBER',BULL_PARAMETER,LEN_P)
	IF (IER1.EQ.%LOC(CLI$_ABSENT).AND..NOT.ALL) THEN
	   IF (BULL_POINT.EQ.0) THEN	! If no message has been read
	      WRITE(6,'('' ERROR: You are not reading any message.'')')
	      RETURN			! and return
	   END IF

	   CALL OPEN_BULLDIR_SHARED
	   CALL READDIR(BULL_POINT,IER)		! Get message directory entry
	   IF (IER.NE.BULL_POINT+1.OR.BULL_POINT.NE.SAVE_BULL_POINT) THEN
	      WRITE(6,'('' ERROR: Specified message was not found.'')')
	      CALL CLOSE_BULLDIR
	      BULL_POINT = SAVE_BULL_POINT
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
	      IF (EBULL.GT.F_NBULL) EBULL = F_NBULL
	      IF (SBULL.LE.0.OR.IER1.NE.0) THEN
		 WRITE (6,'(A)')
     &		  ' ERROR: Specified message number has incorrect format.'
		 CALL CLOSE_BULLDIR
		 RETURN
	      ELSE
		 NUM_COPY = EBULL - SBULL + 1
		 BULL_POINT = SBULL
	      END IF
	      IF (NUM_COPY.GT.1) ALL = .TRUE.
	      IF (INDEX(BULL_PARAMETER,'LAST').GT.0.AND.ORIGINAL) THEN
	         NEWGROUP = .TRUE.	! Kludgey way of detecting new2bull
	         NEXT = .TRUE.		! If SBULL does not exist, will find
              ELSE			! next message after SBULL
	         SBULL1 = SBULL
	   	 CALL READDIR(SBULL,IER)
	   	 IF (IER.NE.SBULL+1.OR.SBULL.NE.SBULL1) THEN
		    WRITE(6,'('' ERROR: Specified message was not found.'')')
	      	    CALL CLOSE_BULLDIR
	      	    RETURN
	         END IF
	      END IF
	   ELSE IF (ALL) THEN
	      NUM_COPY = NBULL
	      BULL_POINT = 1
	      NEWGROUP = .TRUE.
	      NEXT = .TRUE.
	   END IF
	END IF

	IF (REMOTE_SET.OR.REMOTE_SET.EQ.4) THEN
	   IF (.NOT.TEMP_FILE) THEN
	      OPEN (UNIT=12,FILE='REMOTE.BULLDIR',
     &		 STATUS='SCRATCH',FORM='UNFORMATTED',IOSTAT=IER)
	      IF (IER.EQ.0) THEN
		 OPEN (UNIT=11,FILE='REMOTE.BULLFIL',
     &		    STATUS='SCRATCH',IOSTAT=IER,
     &		    ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=32,
     &		    FORM='UNFORMATTED')
	      END IF
	   ELSE
	      REWIND (12,IOSTAT=IER)
	   END IF
	   IF (IER.EQ.0) THEN
	      TEMP_FILE = .TRUE.
	      CALL OPEN_BULLFIL
	      CALL READDIR(0,IER)
	      I = BULL_POINT - 1
	      IER = I + 1
	      NBLOCK = 1
	      LAST = BULL_POINT+NUM_COPY-1
	      NUM_COPY = 0
	      DO WHILE (I.LT.LAST.AND.IER.EQ.I+1)
		 I = I + 1
		 CALL READDIR(I,IER)
		 IF (IER.EQ.I+1.AND.I.LE.LAST) THEN
		    CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
		    IF (REMOTE_SET) THEN
		       CALL REMOTE_READ_MESSAGE(I,IER1)
		       IF (IER1.GT.0) THEN
			  CALL DISCONNECT_REMOTE
		       ELSE
			  CALL GET_REMOTE_MESSAGE(IER1)
		       END IF
		    ELSE
		       IER1 = 0
		    END IF
		    IF (LENGTH.EQ.0) IER1 = 1	! Don't allow empty messages
		    IF (IER1.EQ.0) THEN
		       SCRATCH_R = SCRATCH_R1
		       DO J=1,LENGTH
			  IF (REMOTE_SET) THEN
			     CALL READ_QUEUE(%VAL(SCRATCH_R),
     &					SCRATCH_R,INPUT(:128))
			  ELSE
			     READ (1'BLOCK+J-1,IOSTAT=IER1) INPUT(:128)
			  END IF
			  WRITE (11'NBLOCK+J-1,IOSTAT=IER1) INPUT(:128)
		       END DO
		    END IF
		    NEWS2BULL = NEWS2BULL.AND..NOT.TO_NEWS.AND.ORIGINAL
		    IF (IER1.EQ.0.AND..NOT.NEWS2BULL) THEN
		       BLOCK = NBLOCK
		       NBLOCK = NBLOCK + LENGTH
		       WRITE (12,IOSTAT=IER1) BULLDIR_ENTRY
		    END IF
		    IF (TO_NEWS.AND.ORIGINAL) THEN
		       WRITE (12,IOSTAT=IER1) NEWS_MSGID
		    END IF
		    IF (IER1.NE.0) THEN
		       I = IER
		    ELSE IF (.NOT.NEWS2BULL) THEN
		       NUM_COPY = NUM_COPY + 1
		    END IF
		    NEWS2BULL = .FALSE.
		 END IF
	      END DO
	      CALL CLOSE_BULLFIL
	   END IF
	   IF (IER1.NE.0.OR..NOT.TEMP_FILE.OR.NUM_COPY.EQ.0) THEN
	      WRITE(6,'('' ERROR: Copy aborted. Remote folder problem.'')')
	      CLOSE (UNIT=12)
	      CLOSE (UNIT=11)
	      TEMP_FILE = .FALSE.
	      CALL CLOSE_BULLDIR
	      RETURN
	   END IF
	END IF

	CALL CLOSE_BULLDIR

	SAVE_FOLDER = FOLDER
	SAVE_FOLDER_NUMBER = FOLDER_NUMBER

	FOLDER_NUMBER = -1	! Use FOLDER as key rather than FOLDER_NUMBER
	FROM_BULL_POINT = BULL_POINT
	CALL SELECT_FOLDER(.FALSE.,IER)

	IER1 = .TRUE.

	POST_NEWS = (REMOTE_SET.EQ.4.AND..NOT.BULLCP_NEWS).OR.REMOTE_SET.EQ.3

	POST_FEED = .FALSE.
	SLIST = 0
	IF (.NOT.IER) THEN
	   WRITE (6,'('' ERROR: Cannot access specified folder.'')')
	ELSE IF (READ_ONLY.OR.(MERGE.AND.REMOTE_SET.GT.0)) THEN
	   IF (READ_ONLY) THEN
	      WRITE (6,'('' ERROR: No access to write into folder.'')')
	   ELSE
	      WRITE (6,'('' ERROR: /MERGE invalid into remote folder.'')')
	   END IF
	   IER1 = .FALSE.
	ELSE IF (REMOTE_SET.EQ.0) THEN
	   IF (.NOT.CLI$PRESENT('LOCAL').AND.NEWS_FEED()) THEN
	      SLIST = INDEX(FOLDER_DESCRIP,'<') + 1
	      FOLDER1_DESCRIP =
     &		FOLDER_DESCRIP(SLIST:INDEX(FOLDER_DESCRIP,'>')-1)
	      POST_FEED = .TRUE.
	   END IF
	   IF (NEWS_FEED()) THEN
	      SLIST = INDEX(FOLDER_DESCRIP,'[') + 1
	   ELSE
	      SLIST = INDEX(FOLDER_DESCRIP,'<') + 1
	   END IF
	   IF (SLIST.GT.1) THEN
	      IF (NEWS_FEED()) THEN
	         ELIST = INDEX(FOLDER_DESCRIP,']') - 1
	      ELSE
	         ELIST = INDEX(FOLDER_DESCRIP,'>') - 1
	      END IF
	   END IF
	   IF (CLI$PRESENT('LOCAL').AND..NOT.BULLCP_NEWS) SLIST = 0
	END IF

	IF (.NOT.IER.OR..NOT.IER1) THEN
	   FOLDER_NUMBER = SAVE_FOLDER_NUMBER
	   IF (.NOT.IER) THEN
	      FOLDER = SAVE_FOLDER
	      BULL_POINT = SAVE_BULL_POINT
	   ELSE
	      FOLDER_NUMBER = SAVE_FOLDER_NUMBER
	      FOLDER1 = SAVE_FOLDER
	      CALL SELECT_FOLDER(.FALSE.,IER1)
	   END IF
	   BULL_POINT = SAVE_BULL_POINT
	   CLOSE (UNIT=12)
	   CLOSE (UNIT=11)
	   TEMP_FILE = .FALSE.
	   RETURN
	END IF
C
C  Add bulletin to bulletin file and directory entry for to directory file.
C
	IF (POST_NEWS.OR.(POST_FEED.AND.SLIST.LE.1)) THEN
	   OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,
     &		RECL=LINE_LENGTH,STATUS='SCRATCH',CARRIAGECONTROL='LIST')
	   SCRFILE = 'SYS$LOGIN:BULL.SCR'
	END IF
	IF (.NOT.POST_NEWS) THEN
	   CALL OPEN_BULLDIR			! Prepare to add dir entry
	   IF (REMOTE_SET.EQ.4) THEN            ! In case exdate has bad date
	      IF (FOLDER_BBEXPIRE.GT.0) THEN
		 EX = FOLDER_BBEXPIRE
	      ELSE
		 EX = NEWS_EXPIRE_DEFAULT
	      END IF
	      CALL GET_EXDATE(EXDATE,EX)
	      CALL SYS$ASCTIM(,TODAY,,)		! Get the present time
	      EXTIME = TODAY(13:)
	   END IF
	   CALL OPEN_BULLFIL			! Prepare to add bulletin

	   CALL READDIR(0,IER)			! Get NBLOCK
	   IF (IER.EQ.0.AND.REMOTE_SET.LT.3) NBLOCK = 0
	END IF

	FOLDER1_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))
     &		//SAVE_FOLDER

	IF (.NOT.FROM_REMOTE.AND.FROM_REMOTE.NE.4) THEN
	   DO WHILE (FILE_LOCK(IER,IER1))
	      OPEN (UNIT=12,FILE=FOLDER1_FILE(:TRIM(FOLDER1_FILE))
     &		//'.BULLDIR',STATUS='OLD',FORM='UNFORMATTED',
     &		RECORDTYPE='FIXED',RECORDSIZE=DIR_RECORD_LENGTH/4,
     &		ORGANIZATION='INDEXED',IOSTAT=IER,
     &		KEY=(9:12:INTEGER,1:8:CHARACTER),ACCESS='KEYED')
	   END DO

	   IF (IER.EQ.0) THEN
	      DO WHILE (FILE_LOCK(IER,IER1))
		 OPEN (UNIT=11,FILE=FOLDER1_FILE(:TRIM(FOLDER1_FILE))
     &		   //'.BULLFIL',STATUS='UNKNOWN',IOSTAT=IER,
     &		   ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=32,
     &		   FORM='UNFORMATTED')
	      END DO
	   END IF
	ELSE
	   IER= 0
	END IF

	IF (REMOTE_SET.GE.3) THEN
	   SAVE_HEADER = HEADER
	   IF (CLI$PRESENT('HEADER')) THEN
	      HEADER = .TRUE.
	   ELSE
	      HEADER = .FALSE.
	   END IF
	END IF

	IF (MERGE) CALL INITIALIZE_MERGE(IER)

	START_BULL_POINT = BULL_POINT

	IF (IER.EQ.0) THEN
	   IF (FROM_REMOTE.OR.FROM_REMOTE.EQ.4) THEN
	      REWIND (12)
	   ELSE
	      READ (12,KEYID=0,KEY=FROM_BULL_POINT-1,IOSTAT=IER)
	   END IF
	END IF

	DO WHILE (NUM_COPY.GT.0.AND.IER.EQ.0)
	   READ (12,IOSTAT=IER) BULLDIR_ENTRY
	   IF ((FROM_REMOTE.OR.FROM_REMOTE.EQ.4).AND.
     &	       (TO_NEWS.AND.ORIGINAL)) THEN
	      READ (12,IOSTAT=IER) NEWS_MSGID
	   END IF
	   NUM_COPY = NUM_COPY - 1

	   CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
	   CALL CONVERT_ENTRY_FROMBIN_FOLDER

	   IF (REMOTE_SET.GE.3) SYSTEM = 0

	   IF (FROM_REMOTE.GE.3.AND.REMOTE_SET.LE.3) THEN
	      SYSTEM = 0
	      IF (FOLDER_BBEXPIRE.GT.0) THEN
		 CALL GET_EXDATE(EXDATE,FOLDER_BBEXPIRE)
	      ELSE IF (FOLDER_BBEXPIRE.EQ.-1) THEN   ! Permanent message
		 EXDATE = '5-NOV-2100'
		 SYSTEM = 2
	      ELSE IF (EX_BTIM(1).EQ.0.AND.EX_BTIM(2).EQ.0) THEN
		 CALL GET_EXDATE(EXDATE,14)
	      END IF
	   ELSE IF (REMOTE_SET.EQ.4.AND.ORIGINAL) THEN
	      IF (EX_BTIM(1).NE.0.OR.EX_BTIM(2).NE.0) THEN
		 LIMIT = NEWS_F_EXPIRE_LIMIT
		 IF (LIMIT.EQ.0) LIMIT = NEWS_EXPIRE_LIMIT_DEFAULT
		 IF (LIMIT.GT.0) THEN
		    CALL GET_EXDATE(EXDATE,LIMIT)
		    CALL SYS_BINTIM(EXDATE,BTIM)
		    IF (COMPARE_BTIM(BTIM,EX_BTIM).LT.0) THEN
		       CALL COPY2(EX_BTIM,BTIM)
		    END IF
		 END IF
		 CALL SYS$ASCTIM(,EXDATE,EX_BTIM,)
		 IF (COMPARE_DATE(EXDATE,' ').LE.0) THEN
		    IER = 0
		    GO TO 100
		 END IF
	      ELSE
		 IF (FOLDER_BBEXPIRE.GT.0) THEN
		    EX = FOLDER_BBEXPIRE
		 ELSE
		    EX = NEWS_EXPIRE_DEFAULT
		 END IF
		 IF (F_LAST.EQ.0) THEN
		    EX = EX + COMPARE_DATE(DATE,' ')
		    IF (EX.LE.0) THEN
		       IER = 0
		       GO TO 100
		    END IF
		 END IF
		 CALL GET_EXDATE(EXDATE,EX)
	      END IF
	      CALL SYS$ASCTIM(,TODAY,,)		! Get the present time
	      EXTIME = TODAY(13:)
	   END IF

	   IF (.NOT.BTEST(FOLDER_FLAG,2).OR.	! Not system folder?
     &		 .NOT.SETPRV_PRIV()) THEN	! Or no privileges?
	      SYSTEM = IBCLR(SYSTEM,0)		! Remove system bit
	   END IF

	   IF (BTEST(SYSTEM,2).AND.		! Shutdown message?
     &	    (.NOT.BTEST(FOLDER_FLAG,2).OR.	! Not system folder?
     &		 .NOT.SETPRV_PRIV())) THEN	! Or no privileges?
	      SYSTEM = IBCLR(SYSTEM,2)		! Remove shutdown bit
	      WRITE (6,'('' ERROR: No privileges to add'',
     &				'' shutdown message.'')')
	      IF (FOLDER_BBEXPIRE.GT.0) THEN
		 CALL GET_EXDATE(EXDATE,FOLDER_BBEXPIRE)
		 WRITE (6,'('' Expiration will be '',I,'' days.'')')
     &				FOLDER_BBEXPIRE
	      ELSE
		 CALL GET_EXDATE(EXDATE,14)
		 WRITE (6,'('' Expiration will be '',I,'' days.'')') 14
	      END IF
	      EXTIME = '00:00:00.00'
	   ELSE IF (BTEST(SYSTEM,1).AND.	! Permanent?
     &		F_EXPIRE_LIMIT.GT.0.AND..NOT.   ! Expiration limit present?
     &		FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)) THEN
	      WRITE (6,'('' ERROR: No privileges to add'',
     &				'' permanent message.'')')
	      WRITE (6,'('' Expiration will be '',I,'' days.'')')
     &				F_EXPIRE_LIMIT
	      SYSTEM = IBCLR(SYSTEM,1)
	      CALL GET_EXDATE(EXDATE,F_EXPIRE_LIMIT)
	      EXTIME = '00:00:00.00'
	   END IF

	   IF (.NOT.ORIGINAL) THEN	! If not /ORIGINAL
	      FROM = USERNAME		! Specify owner
	   END IF

	   IF (REMOTE_SET.EQ.1) THEN
	      WRITE (REMOTE_UNIT,'(A)',IOSTAT=IER) 2
	      IF (IER.NE.0) CALL ERROR_AND_EXIT
	   END IF

	   IF (SLIST.GT.1.OR.POST_NEWS.OR.POST_FEED) THEN
	      BLOCK_SAVE = BLOCK
	      LENGTH_SAVE = LENGTH
	      IF (SLIST.GT.1) THEN
		 OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,
     &		   RECL=LINE_LENGTH,CARRIAGECONTROL='LIST')
	         SCRFILE = 'SYS$LOGIN:BULL.SCR'
		 IF (IER.NE.0) THEN
	      	    OPEN(UNIT=3,FILE='BULL.SCR',IOSTAT=IER,
     &		       RECL=LINE_LENGTH,CARRIAGECONTROL='LIST')
	      	    SCRFILE = 'BULL.SCR'
		 END IF
	      ENDIF
	      ILEN = LINE_LENGTH + 1

	      INFROM = FROM
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN
	         INFROM = INPUT(7:)
		 CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      END IF
	      IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THEN
		 POST_SUBJECT = INPUT(7:ILEN)
	      ELSE
		 POST_SUBJECT = DESCRIP
		 IF (ILEN.GT.0) WRITE (3,'(A)') INPUT(:ILEN)
	      END IF

	      DO WHILE (ILEN.GT.0)		! Copy bulletin into file
		 CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
		 IF (ILEN.GT.0) WRITE (3,'(A)') INPUT(1:ILEN)
	      END DO

	      REWIND (UNIT=3)

	      IF (POST_NEWS.OR.POST_FEED) THEN
	         USE_INFROM = ORIGINAL
	         IF (CLI$PRESENT('LOCAL')) NEWS2BULL = .TRUE.
	         CALL NEWS_POST
     &		    (SCRFILE(:TRIM(SCRFILE)),.TRUE.,IER,POST_SUBJECT)
		 NEWS2BULL = .FALSE.
	      END IF
	      IF (SLIST.GT.1) THEN
		 CLOSE (UNIT=3)
		 USE_INFROM = ORIGINAL
		 IF (BTEST(FOLDER_FLAG,10).OR.BTEST(FOLDER_FLAG,15)) THEN
	            CALL RESPOND_MAIL(SCRFILE(:TRIM(SCRFILE)),
     &		       FOLDER_DESCRIP(SLIST:ELIST),
     &		       POST_SUBJECT(:TRIM(POST_SUBJECT)),STATUS)
		 ELSE
	            CALL RESPOND_MAIL(SCRFILE(:TRIM(SCRFILE)),
     &		       FOLDER_DESCRIP(SLIST:ELIST),
     &	   	       FOLDER(:TRIM(FOLDER))//' folder message: '//
     &		       POST_SUBJECT(:TRIM(POST_SUBJECT)),STATUS)
		 END IF
		 CALL LIB$DELETE_FILE(SCRFILE(:TRIM(SCRFILE))//';')
	      ELSE IF (POST_FEED.OR.POST_NEWS) THEN
		 REWIND (UNIT=3)
	      END IF
	      BLOCK = BLOCK_SAVE
	      LENGTH = LENGTH_SAVE
	   END IF
	   IF (.NOT.POST_NEWS) THEN
	      IF (MERGE) CALL ADD_MERGE_TO(IER)

	      IF (REMOTE_SET.EQ.4) CALL SET_BULLFIL_UPDATE

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
		    IF (.NOT.ORIGINAL) SYSTEM = IBSET(SYSTEM,4)
		    CALL ADD_ENTRY	! Add the new directory entry
		 END IF
		 BULL_POINT = BULL_POINT + 1
	      END IF
	   END IF
100	   CONTINUE
	END DO

	IF (SLIST.LT.1.AND.POST_NEWS.OR.POST_FEED) CLOSE (UNIT=3)

	IF (MERGE) CALL ADD_MERGE_REST(IER)

	IF (.NOT.POST_NEWS) CALL CLOSE_BULLFIL

	IF (.NOT.(TO_NEWS.AND.ORIGINAL.AND.TEMP_FILE)) THEN
	   CLOSE (UNIT=11)
	   CLOSE (UNIT=12)
	   TEMP_FILE = .FALSE.
	END IF

	IF (FOLDER_NUMBER.GE.0.AND.IER.EQ.0.AND..NOT.POST_NEWS
     &		.AND.FOLDER_NUMBER.LT.FOLDER_MAX) THEN
	   DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &			       F_NEWEST_BTIM)
	   CALL UPDATE_FOLDER			! Update folder info
C
C  If user is adding message, an no new messages, update last read time for
C  folder, so user is not alerted of new message which is owned by user.
C
	   IF (DIFF.GE.0) THEN
	      CALL COPY2(LAST_READ_BTIM(1,FOLDER_NUMBER+1),F_NEWEST_BTIM)
	   END IF
	END IF

	IF (.NOT.POST_NEWS) CALL CLOSE_BULLDIR	! Totally finished with add

	IF (IER.EQ.0) THEN
	   IF (TEST_BULLCP().NE.2)
     &	      WRITE (6,'('' Successful copy to folder '',A)')
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

	IF (.NOT.POST_NEWS) HEADER = SAVE_HEADER
	IF (BULLCP_NEWS) RETURN

	IF (INDEX(INCMD,' ').EQ.TRIM(INCMD)+1)
     &	   INCMD = INCMD(:TRIM(INCMD))//' '//FOLDER1

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
	      CALL DELETE_MSG
	   END IF
	END IF

	RETURN
	END




	SUBROUTINE PRINT(PRINT_NUM,OPEN_IT)
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

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*256 INCMD

	EXTERNAL CLI$_ABSENT

	CHARACTER*32 QUEUE,TEST

	INTEGER*2 IOSB(4)
	EQUIVALENCE (IOSB(1),JBC_ERROR)

	CHARACTER*32 FORM

	PARAMETER FF = CHAR(12)

	DATA FIRST /.TRUE./, CHANGED /.FALSE./

	OPENED = .FALSE.

	IF (CLI$PRESENT('NOW').AND..NOT.FIRST.AND.
     &	    INCMD(:4).EQ.'PRIN') THEN
	   WRITE (6,'('' Printing all previously queued messages.'')')
	   GO TO 200
	ELSE IF (.NOT.FIRST) THEN
	   IER = CLI$GET_VALUE('QUEUE',TEST,TLEN)
	   CHANGED = TEST(:TLEN).NE.QUEUE(:QLEN).AND.TLEN.GT.0
	   CHANGED = CHANGED.OR.CLI$PRESENT('NOTIFY').NE.NOTIFY
	   IER = CLI$GET_VALUE('FORM',TEST,FLEN)
	   CHANGED = CHANGED.OR.(TEST(:TLEN).NE.FORM(:FLEN).AND.TLEN.GT.0)
	   IF (CHANGED) THEN
	      WRITE (6,'('' Printing all previously queued messages.'')')
	      GO TO 200
	   END IF
	END IF

        IF (INCMD(:4).EQ.'PRIN') THEN
	   IF (CLI$PRESENT('CANCEL')) THEN 
	      WRITE (6,'('' Cancelling all previously queued messages.'')')
	      CLOSE (UNIT=24,DISPOSE='DELETE')
	      FIRST = .TRUE.
	      RETURN
	   END IF
	END IF

50	IF (PRINT_NUM.EQ.0) THEN
	   IER = CLI$GET_VALUE('BULLETIN_NUMBER',BULL_PARAMETER,LEN_P)
	   IF (IER.NE.%LOC(CLI$_ABSENT)) THEN	! Was bulletin specified?
	      CALL GET_2_VALS(BULL_PARAMETER,LEN_P,SBULL,EBULL,IER)
	      IF (EBULL.GT.F_NBULL) EBULL = F_NBULL
	   ELSE IF (OPENED) THEN
	      CALL CLOSE_BULLFIL
	      CALL CLOSE_BULLDIR
	      GO TO 150
	   ELSE IF (CLI$PRESENT('ALL')) THEN
	      SBULL = 1
	      EBULL = F_NBULL
	      IER = 0
	   ELSE IF (BULL_POINT.EQ.0) THEN  ! No.  Have we just read a bulletin?
	      WRITE(6,1010)		  ! No, then error.
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
	      END IF
	      WRITE (6,'(1X,A)') BULL_PARAMETER(:LEN_P)
	      RETURN
	   END IF
	ELSE
	   SBULL = PRINT_NUM
	   EBULL = SBULL
	END IF

	IF (FIRST) THEN
	   QLEN = 0
	   IER = CLI$GET_VALUE('QUEUE',QUEUE,QLEN) 	! Get queue name
	   IF (QLEN.EQ.0) THEN
	      QUEUE = 'SYS$PRINT'
	      QLEN = TRIM(QUEUE)
	   END IF

	   NOTIFY = CLI$PRESENT('NOTIFY')

	   FLEN = 0
	   IER = CLI$GET_VALUE('FORM',FORM,FLEN)	 ! Get form name

	   CALL DISABLE_PRIVS

	   OPEN(UNIT=24,FILE='SYS$LOGIN:BULL.LIS',ERR=900,IOSTAT=IER,
     &		RECL=LINE_LENGTH,STATUS='NEW',CARRIAGECONTROL='LIST')

	   CALL ENABLE_PRIVS
	END IF

	IF (OPEN_IT) THEN
	   CALL OPEN_BULLDIR_SHARED
	   CALL OPEN_BULLFIL_SHARED
	   OPENED = .TRUE.
	END IF

	HEAD = CLI$PRESENT('HEADER')

	DO I=SBULL,EBULL
	   I1 = I
	   CALL READDIR(I,IER)		! Get info for specified message
	   IF (IER.NE.I+1.OR.I.GT.EBULL.OR.(.NOT.CLI$PRESENT
     &		('ALL').AND.I1.EQ.SBULL.AND.I.NE.SBULL)) THEN
	      IF (REMOTE_SET.NE.3.OR.I1.EQ.SBULL) WRITE(6,1030) I1
	      IF (I1.GT.SBULL) GO TO 100
	      CLOSE (UNIT=24,DISPOSE='DELETE')
	      IF (OPEN_IT) THEN
		 CALL CLOSE_BULLFIL
		 CALL CLOSE_BULLDIR
	      END IF
	      RETURN
	   ELSE IF (REMOTE_SET) THEN
	      CALL REMOTE_READ_MESSAGE(I,IER1)
	      IF (IER1.GT.0) THEN
		 CALL DISCONNECT_REMOTE
	      ELSE
		 CALL GET_REMOTE_MESSAGE(IER1)
	      END IF
	      IF (IER1.NE.0) GO TO 100
	   END IF

	   IF (.NOT.FIRST) THEN
	      WRITE (24,'(A)') FF
	   ELSE
	      FIRST = .FALSE.
	   END IF

	   ILEN = LINE_LENGTH + 1

	   CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN
	      IF (HEAD) THEN
		 WRITE(24,1060) INPUT(7:ILEN),DATE//' '//TIME(:8)
	      END IF
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   ELSE IF (HEAD) THEN
	      WRITE(24,1060) FROM,DATE//' '//TIME(:8)
	   END IF
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THEN
	      IF (HEAD) WRITE(24,1050) INPUT(7:ILEN)
	   ELSE
	      IF (HEAD) WRITE(24,1050) DESCRIP
	      IF (ILEN.GT.0) WRITE (24,'(A)') INPUT(:ILEN)
	   END IF

	   DO WHILE (ILEN.GT.0)		! Copy bulletin into file
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      IF (ILEN.GT.0) WRITE (24,'(A)') INPUT(1:ILEN)
	   END DO
	END DO

100	IF (PRINT_NUM.EQ.0) THEN
	   IER = OTS$CVT_L_TI(SBULL,BULL_PARAMETER,,,)
	   IF (SBULL.EQ.EBULL) THEN
	      WRITE(6,1040)
     &		 BULL_PARAMETER(FIRST_ALPHA(BULL_PARAMETER):)
	   ELSE
	      WRITE(6,1045)
     &		 BULL_PARAMETER(FIRST_ALPHA(BULL_PARAMETER):)
	      IER = OTS$CVT_L_TI(EBULL,BULL_PARAMETER,,,)
	      WRITE(6,1046)
     &		 BULL_PARAMETER(FIRST_ALPHA(BULL_PARAMETER):)
	   END IF

1040	   FORMAT(' Message ',A,' sent to printer.')
1045	   FORMAT(' Messages ',A,$)
1046	   FORMAT('+-',A,' sent to printer.')
	   GO TO 50
	ELSE IF (OPEN_IT) THEN
	   CALL CLOSE_BULLFIL
	   CALL CLOSE_BULLDIR
	END IF

150	IF (.NOT.CLI$PRESENT('NOW').OR.INCMD(:4).NE.'PRIN') RETURN

	ENTRY PRINT_NOW

200	IF (FIRST) RETURN

	FIRST = .TRUE.

	CLOSE (UNIT=24)

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(18,SJC$_FILE_SPECIFICATION,
     &		%LOC('SYS$LOGIN:BULL.LIS'))

	CALL ADD_2_ITMLST(QLEN,SJC$_QUEUE,%LOC(QUEUE))
	CALL ADD_2_ITMLST(0,SJC$_DELETE_FILE,0)

	IF (NOTIFY) CALL ADD_2_ITMLST(0,SJC$_NOTIFY,0)

	IF (FLEN.GT.0) THEN
	   CALL ADD_2_ITMLST(FLEN,SJC$_FORM_NAME,%LOC(FORM))
	END IF

	CALL DISABLE_PRIVS

	CALL ADD_2_ITMLST(4,SJC$_ENTRY_NUMBER_OUTPUT,%LOC(JOBNUM))

	CALL END_ITMLST(SJC_ITMLST)

	IER=SYS$SNDJBCW(,%VAL(SJC$_ENTER_FILE),,%VAL(SJC_ITMLST),IOSB,,)
	IF (IER.AND.(.NOT.JBC_ERROR)) THEN
	   CALL SYS_GETMSG(JBC_ERROR)
	   IER = LIB$DELETE_FILE('SYS$LOGIN:BULL.LIS;')
	ELSE IF (.NOT.IER) THEN
	   CALL SYS_GETMSG(IER)
	   IER = LIB$DELETE_FILE('SYS$LOGIN:BULL.LIS;')
	ELSE
	   IER = OTS$CVT_L_TI(JOBNUM,BULL_PARAMETER,,,)
	   IF (IER) WRITE (6,'('' Job BULL (queue '',A,'', entry '',A,
     &	      '') started on '',A)') QUEUE(:QLEN),
     &	      BULL_PARAMETER(FIRST_ALPHA(BULL_PARAMETER):),QUEUE(:QLEN)
	END IF

	CALL ENABLE_PRIVS			! Reset SYSPRV privileges

	IF (CHANGED) THEN
	   CHANGED = .FALSE.
	   GO TO 50
	END IF

	RETURN

900	CALL ERRSNS(IDUMMY,IER)
	CALL ENABLE_PRIVS			! Reset SYSPRV privileges
	WRITE(6,1000)
	CALL SYS_GETMSG(IER)
	RETURN

1000	FORMAT(' ERROR: Unable to open temporary file
     &	 SYS$LOGIN:BULL.LIS for printing.')
1010	FORMAT(' ERROR: You have not read any message.')
1015	FORMAT(' ERROR: Specified message number has incorrect format:')
1030	FORMAT(' ERROR: Following bulletin was not found: ',I)
1050	FORMAT('Description: ',A,/)
1060	FORMAT('From: ',A,/,'Date: ',A)

	END




	SUBROUTINE READ_MSG(READ_COUNT,BULL_READ)
C
C  SUBROUTINE READ_MSG
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

	COMMON /PAGE/ PAGE_LENGTH,REAL_PAGE_WIDTH,PAGING
	LOGICAL PAGING

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*256 INCMD

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /READ_DISPLAY/ LINE_OFFSET

	COMMON /TAGS/ BULL_TAG,READ_TAG,BULL_NEWS_TAG

	COMMON /HEADER/ HEADER

	COMMON /NEXT/ NEXT
	LOGICAL NEXT /.FALSE./

	COMMON /POST/ POSTTIME

	COMMON /MSGID/ MESSAGE_ID
	CHARACTER*256 MESSAGE_ID

	COMMON /BULL_USER_CUSTOM/ BULL_USER_CUSTOM
	DATA BULL_USER_CUSTOM/.FALSE./

	COMMON /THREAD/ THREAD
	DATA THREAD /.FALSE./

	COMMON /MAIN_HEADER_INFO/ INFROM,INDESCRIP,LEN_FROM,LEN_DESCRP
	COMMON /MAIN_HEADER_INFO/ INEXDATE
	CHARACTER*(INPUT_LENGTH) INFROM,INDESCRIP

	DATA SCRATCH_B1/0/,LAST_THREAD/.FALSE./

	CHARACTER TODAY*12,DATETIME*24,BUFFER*(INPUT_LENGTH)
	CHARACTER HEADLINE*132

	LOGICAL SINCE,PAGE

	EXTERNAL CLI$_NEGATED

	FIRST = BULL_READ.LT.F_START
	KILL = BTEST(BULL_USER_CUSTOM,3)
	BULL_USER_CUSTOM = IBCLR(BULL_USER_CUSTOM,3)

	POSTTIME = .TRUE.

	CALL LIB$ERASE_PAGE(1,1)		! Clear screen
	END = 0					! Nothing outputted on screen

	IF (READ_COUNT.GT.0) GO TO 100		! Skip init steps if this is
						! not first page of bulletin

	IF (INCMD(:4).EQ.'READ'.OR.INCMD(:4).EQ.'LAST'.OR.
     &	    INCMD(:4).EQ.'BACK'.OR.INCMD(:3).EQ.'CUR'.OR.
     &	    INCMD(:4).EQ.'FIRS'.OR.INCMD(:1).EQ.'N') THEN
	   IF (CLI$PRESENT('HEADER')) THEN
	      HEADER = .TRUE.
	   ELSE IF (CLI$PRESENT('HEADER').EQ.%LOC(CLI$_NEGATED)) THEN
	      HEADER = .FALSE.
	   END IF
	   ROTC = CLI$PRESENT('ROTATE')
	END IF

	SINCE = .FALSE.
	NEW = .FALSE.
	PAGE = .TRUE.
	THREAD = .FALSE.

	IER = 0

	IF (.NOT.PAGING) PAGE = .FALSE.
	IF (INCMD(:4).EQ.'READ') THEN		! If READ command...
	   POSTTIME = CLI$PRESENT('POST')
	   THREAD = CLI$PRESENT('THREADS')
	   IF (CLI$PRESENT('MARKED')) THEN
	      READ_TAG = 1 + IBSET(0,1)
	   ELSE IF (CLI$PRESENT('SEEN')) THEN
	      READ_TAG = 1 + IBSET(0,2)
	   ELSE IF (CLI$PRESENT('UNMARKED').OR.
     &		   CLI$PRESENT('MARKED').EQ.%LOC(CLI$_NEGATED)) THEN
	      READ_TAG = 1 + IBSET(0,1) + IBSET(0,3)
	   ELSE IF (CLI$PRESENT('UNSEEN').OR.
     &		    CLI$PRESENT('SEEN').EQ.%LOC(CLI$_NEGATED)) THEN
	      READ_TAG = 1 + IBSET(0,2) + IBSET(0,3)
	   ELSE IF (CLI$PRESENT('ALL')) THEN
	      READ_TAG = IBSET(0,1) + IBSET(0,2)
	      IF (REMOTE_SET.GE.3) THEN
		 BULL_READ = F_START
	      ELSE
		 BULL_READ = 1
	      END IF
	   END IF
	   IF (READ_TAG) THEN
	      IF (.NOT.(FOLDER_NUMBER.GE.0.OR.REMOTE_SET.EQ.3)) THEN
		 WRITE (6,'('' ERROR: Invalid qualifier'',
     &			    '' with remote folder.'')')
		 READ_TAG = 1 + IBSET(0,2) + IBSET(0,3)
		 GO TO 9999
	      END IF
	      CALL GET_FIRST_TAG(FOLDER_NUMBER,IER1,BULL_POINT)
	   END IF

	   IF (.NOT.CLI$PRESENT('PAGE')) PAGE = .FALSE.
	   IF (CLI$PRESENT('SINCE').AND.
     &	       .NOT.THREAD) THEN		! was /SINCE specified?
	      IER = CLI$GET_VALUE('SINCE',DATETIME)
	      IF (DATETIME.EQ.'TODAY') THEN	! TODAY is the default.
		 IER = SYS$BINTIM('-- 00:00:00.00',TODAY)
		 CALL GET_MSGKEY(TODAY,MSG_KEY)
	      ELSE
		 CALL SYS_BINTIM(DATETIME,MSG_BTIM)
		 CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
	      END IF
	      CALL OPEN_BULLDIR_SHARED
	      CALL READDIR_KEYGE(IER)
	      CALL CLOSE_BULLDIR
	   ELSE IF (CLI$PRESENT('NEW').OR.(THREAD.AND..NOT.
     &		CLI$PRESENT('SINCE').AND..NOT.CLI$PRESENT('BULLETIN_NUMBER')
     &		.AND.(.NOT.LAST_THREAD
     &	        .OR.LAST_THREAD_NUMBER.NE.FOLDER_NUMBER)
     &		.AND.CLI$PRESENT('NEW').NE.%LOC(CLI$_NEGATED))) THEN
	      NEW = .TRUE.
	      IF (REMOTE_SET.LT.3) THEN
		 DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &				 F_NEWEST_BTIM)
		 IF (DIFF.GE.0) THEN
		    WRITE (6,'('' No new messages are present.'')')
		    GO TO 9999
		 ELSE
		    CALL GET_MSGKEY(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &							  MSG_KEY)
		 END IF
		 CALL OPEN_BULLDIR_SHARED
                 IER = 0
		 DO WHILE (IER.EQ.0)
		    CALL READDIR_KEYGE(IER)
	   	    IF (IER.NE.0.AND.BULL_TAG.AND.BTEST(BULL_TAG,1)) THEN
		       CALL GET_THIS_TAG(FOLDER_NUMBER,IER1,IER,DUMMY)
		       IF (IER1.EQ.0) THEN
	                  CALL COPY2(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &					MSG_BTIM)
		          CALL GET_MSGKEY(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &					MSG_KEY)
			  IER = 0
	               END IF
		    ELSE IF (IER.EQ.0) THEN
		       CALL CLOSE_BULLDIR
		       WRITE (6,'('' No more messages are present.'')')
		       GO TO 9999
	            END IF
		    IF (IER.NE.0.AND.THREAD.AND..NOT.BTEST(SYSTEM,8)) THEN
		       IER = 0
		    END IF
	         END DO
		 CALL CLOSE_BULLDIR
	      ELSE
                 IER = 0
		 SKIPPED_THREAD = .FALSE.
		 IF (THREAD) CALL OPEN_BULLDIR_SHARED
		 DO WHILE (IER.EQ.0)
		    IF (.NOT.SKIPPED_THREAD) THEN
		       CALL NEWS_GET_NEWEST_MESSAGE(IER)
		       BULL_READ = IER
		    END IF
		    IF ((SKIPPED_THREAD.OR.(THREAD.AND.IER.NE.0)).AND.
     &			 BULL_READ.LE.F_NBULL) THEN
	               CALL READDIR(BULL_READ,IER)
		       IER = IER - 1
		       IF (IER.NE.BULL_READ) IER = 0
		    END IF
	   	    IF (IER.NE.0.AND.((BULL_NEWS_TAG.AND.REMOTE_SET.GE.3)
     &			.OR.(BULL_TAG.AND.BTEST(BULL_TAG,1)))) THEN
		       MSG_NUM = IER
		       CALL GET_THIS_TAG(FOLDER_NUMBER,IER1,DUMMY,DUMMY)
		       IF (IER1.EQ.0) THEN
	                  IF (.NOT.SKIPPED_THREAD) THEN
		             CALL NEWS_UPDATE_NEWEST_MESSAGE(IER)
			  ELSE
			     BULL_READ = BULL_READ + 1
			  END IF
			  IER = 0
	               END IF
		    ELSE IF (IER.EQ.0) THEN
		       WRITE (6,'('' No more messages are present.'')')
		       IF (SKIPPED_THREAD) CALL CLOSE_BULLDIR
		       GO TO 9999
	            END IF
		    IF (IER.NE.0.AND.THREAD.AND..NOT.BTEST(SYSTEM,8)) THEN
		       SKIPPED_THREAD = .TRUE.
		       BULL_READ = IER + 1
		       IER = 0
		    END IF
	         END DO
	      END IF
	      IF (THREAD) CALL CLOSE_BULLDIR
	      BULL_READ = IER
	      IER = IER + 1
	   ELSE IF (THREAD) THEN
	      IF (CLI$PRESENT('SINCE')) THEN
	         IF (IER.EQ.0) THEN
		    WRITE (6,'('' No messages past specified date.'')')
		    GO TO 9999
	         ELSE
		    BULL_READ = IER - 1
		    IER = IER + 1
	         END IF
	         SINCE = .TRUE.
	      ELSE IF (CLI$PRESENT('BULLETIN_NUMBER')) THEN
		 BULL_READ = BULL_READ - 1
	      ELSE IF (LAST_THREAD_NUMBER.EQ.FOLDER_NUMBER) THEN          
	         BULL_READ = LAST_THREAD_READ
	      ELSE
	         BULL_READ = BULL_POINT - 1
	      END IF
	      CALL OPEN_BULLDIR_SHARED
	      IER = BULL_READ + 1
	      IER1 = .FALSE.
	      DO WHILE (.NOT.IER1.AND.IER.EQ.BULL_READ+1.AND.
     &			BULL_READ.LT.F_NBULL)
		 BULL_READ = BULL_READ + 1
	         CALL READDIR(BULL_READ,IER)
		 IER1 = BTEST(SYSTEM,8)
	   	 IF (IER1.AND.((BULL_NEWS_TAG.AND.REMOTE_SET.GE.3)
     &			.OR.(BULL_TAG.AND.BTEST(BULL_TAG,1)))) THEN
		    CALL GET_THIS_TAG(FOLDER_NUMBER,IER2,DUMMY,DUMMY)
		    IER1 = IER2.NE.0
		 END IF
	      END DO
	      IF (.NOT.IER1) THEN
		 WRITE (6,'('' No more messages are present.'')')
		 GO TO 9999
	      END IF
	   END IF
	END IF

	NEXT = .FALSE.
	LAST_THREAD = .FALSE.
	IF (INCMD(:1).EQ.'N'.OR.INCMD.EQ.' ') THEN
	   NEXT = .TRUE.
	ELSE IF (INCMD(:4).EQ.'READ') THEN
	   LAST_THREAD = THREAD
	   IF (THREAD) THEN
	      LAST_THREAD_READ = BULL_READ
	      LAST_THREAD_NUMBER = FOLDER_NUMBER
	   ELSE
	      IF (.NOT.SINCE.AND..NOT.NEW
     &		 .AND..NOT.CLI$PRESENT('BULLETIN_NUMBER')
     &		 .AND..NOT.CLI$PRESENT('ALL')) NEXT = .TRUE.
	   END IF
	END IF

	BULL_NOW = BULL_POINT

	OK = .TRUE.
50	IF (READ_TAG) THEN
	   IER = 0
	   IF ((INCMD(:4).EQ.'BACK'.AND.REMOTE_SET.GE.3).OR.
     &	       (INCMD(:4).EQ.'LAST'.AND.BTEST(READ_TAG,3))) THEN
	      IF (BULL_NOW.EQ.0.OR.INCMD(:4).EQ.'LAST') THEN
		 MSG_NUM = F_NBULL+1
	      ELSE
		 MSG_NUM = BULL_NOW
	      END IF
	      CALL GET_PREVIOUS_TAG(FOLDER_NUMBER,IER1,BULL_READ,DUMMY)
	      IF (IER1.EQ.0) IER = BULL_READ + 1
	   ELSE IF (INCMD(:4).EQ.'BACK') THEN
	      CALL OPEN_BULLDIR_SHARED
	      CALL GET_PREVIOUS_TAG(FOLDER_NUMBER,IER1,BULL_READ,DUMMY)
	      CALL CLOSE_BULLDIR
	      IF (IER1.EQ.0) IER = BULL_READ + 1
	   ELSE IF (INCMD(:4).EQ.'LAST') THEN
	      CALL OPEN_BULLDIR_SHARED
	      IF (BULL_NOW.GT.0) THEN
		 CALL READDIR(BULL_NOW,IER)
		 IF (IER.NE.BULL_NOW+1) THEN
		    BULL_NOW = 0
		 ELSE
		    CALL GET_THIS_OR_NEXT_TAG
     &				   (FOLDER_NUMBER,IER1,BULL_READ,DUMMY)
		    IF (IER1.NE.0) BULL_NOW = 0
		 END IF
	      END IF
	      IF (BULL_NOW.EQ.0) THEN
		 CALL GET_FIRST_TAG(FOLDER_NUMBER,IER1,BULL_READ,DUMMY)
		 IF (IER1.EQ.0) IER = BULL_READ + 1
	      END IF
	      DO WHILE (IER1.EQ.0)
		 CALL GET_NEXT_TAG(FOLDER_NUMBER,IER1,BULL_READ,DUMMY)
		 IF (IER1.EQ.0) IER = BULL_READ + 1
	      END DO
	      CALL CLOSE_BULLDIR
	   ELSE IF (INCMD(:4).EQ.'FIRS') THEN
	      CALL GET_FIRST_TAG(FOLDER_NUMBER,IER1,BULL_READ,DUMMY)
	      CALL GET_NEXT_TAG(FOLDER_NUMBER,IER1,BULL_READ,DUMMY)
	      IF (IER1.EQ.0) IER = BULL_READ + 1
	   ELSE IF (NEXT.OR.SINCE.OR.NEW) THEN
	      OLD_NEXT = NEXT
	      NEXT = .FALSE.
	      IF (NEW) MSG_NUM = BULL_READ
	      IF (.NOT.OLD_NEXT) THEN
		 CALL GET_THIS_OR_NEXT_TAG(FOLDER_NUMBER,IER1,BULL_READ,DUMMY)
	      ELSE
		 IF (REMOTE_SET.GE.3) THEN
		    MSG_NUM = BULL_NOW
		 ELSE IF (BULL_NOW.GT.0) THEN
		    CALL OPEN_BULLDIR_SHARED
		    CALL READDIR(BULL_NOW,IER)
		    CALL CLOSE_BULLDIR
		 ELSE
		    MSG_KEY = BULLDIR_HEADER
		    MSG_NUM = 0
		 END IF
		 CALL GET_NEXT_TAG(FOLDER_NUMBER,IER1,BULL_READ,DUMMY)
	      END IF
	      NEXT = OLD_NEXT
	      IF (IER1.EQ.0) THEN
		 IER = BULL_READ + 1
	      ELSE
		 IER = 0
	      END IF
	   END IF
	END IF

	IF (.NOT.SINCE.AND.(.NOT.READ_TAG.OR.(.NOT.NEXT.AND.
     &		INCMD(:4).NE.'LAST'.AND.INCMD(:4).NE.'BACK'.AND.
     &		INCMD(:4).NE.'FIRS'))) THEN
	   IF (BULL_READ.GT.0) THEN		! Valid bulletin number?
	      CALL OPEN_BULLDIR_SHARED
	      CALL READDIR(BULL_READ,IER)	! Get bulletin directory entry
	      IF (IER.NE.BULL_READ+1.AND.REMOTE_SET.GE.3
     &			   .AND.INCMD(:4).EQ.'READ') THEN
		 IF (NEW) THEN
		    NEXT = .TRUE.
		    CALL READDIR(BULL_READ,IER)
		 END IF
	      END IF
	      IF (REMOTE_SET.LT.3.AND.
     &			   READ_COUNT.EQ.-1.AND.IER.NE.BULL_READ+1) THEN
		 READ_COUNT = 0
		 IF (IER.NE.BULL_READ+1) THEN
		    CALL READDIR(0,IER)
		    IF (NBULL.GT.0) THEN
		       BULL_READ = NBULL
		       CALL READDIR(BULL_READ,IER)
		    ELSE
		       IER = 0
		    END IF
		 END IF
	      ELSE IF (READ_TAG.AND.IER.EQ.BULL_READ+1) THEN
		 CALL GET_THIS_TAG(FOLDER_NUMBER,IER1,BULL_READ,DUMMY)
		 IF (IER1.NE.0) IER = 0
	      END IF
	      CALL CLOSE_BULLDIR
	   ELSE
	      IER = 0
	   END IF
	END IF

	IF (IER.NE.BULL_READ+1) THEN		! Was bulletin found?
	   IF (REMOTE_SET.LT.3) THEN
	      WRITE(6,1030)			! If not, then error out
	   ELSE
	      WRITE(6,1040)
	   END IF
	   NEXT = .FALSE.
	   IF (.NOT.OK.AND..NOT.REMOTE_SET) CALL CLOSE_BULLFIL
	   GO TO 9999
	END IF

	SAVE_BULL_POINT = BULL_POINT
	BULL_POINT = BULL_READ			! Update bulletin counter

	IF (OK.OR.REMOTE_SET) CALL OPEN_BULLFIL_SHARED

	IF (BTEST(BULL_USER_CUSTOM,1)
     &		.AND.(FIRST.OR.NEW.OR.NEXT.OR.INCMD(:4).EQ.'BACK'.OR.
     &		INCMD(:4).EQ.'LAST'.OR.INCMD(:4).EQ.'FIRS')) THEN
	   ILEN = LINE_LENGTH + 1
	   BLOCK_SAVE = BLOCK

	   CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN
	      INFROM = INPUT(7:ILEN)
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   ELSE
	      INFROM = FROM
	   END IF
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THEN
	      INDESCRIP = INPUT(7:ILEN)
	   ELSE
	      INDESCRIP = DESCRIP
	   END IF

	   OK = INCLUDE_MSG(INFROM,INDESCRIP)
	   OK = OK.AND.(.NOT.THREAD.OR.BTEST(SYSTEM,8)) 

	   IF (.NOT.OK) THEN
	      BULL_POINT = SAVE_BULL_POINT
	      BULL_NOW = MSG_NUM
	      IF (INCMD(:4).EQ.'BACK'.OR.INCMD(:4).EQ.'LAST') THEN
	         BULL_READ = MSG_NUM - 1
	      ELSE
	         BULL_READ = MSG_NUM + 1
		 IF (INCMD(:4).EQ.'FIRS'.OR.FIRST) NEXT = .TRUE.
	      END IF
	      IF (REMOTE_SET) CALL CLOSE_BULLFIL
	      IF (REMOTE_SET.LT.3.AND..NOT.THREAD) THEN
		 DIFF = COMPARE_BTIM(MSG_BTIM,
     &				LAST_READ_BTIM(1,FOLDER_NUMBER+1))
	         IF (DIFF.GT.0) THEN
	            CALL COPY2(LAST_READ_BTIM(1,FOLDER_NUMBER+1),MSG_BTIM)
	         END IF
	      ELSE IF (.NOT.THREAD) THEN
	         CALL NEWS_UPDATE_NEWEST_MESSAGE(BULL_NOW)
              END IF
	      IF (BULL_READ.GT.F_NBULL.OR.BULL_READ.LT.F_START) THEN
		 IF (REMOTE_SET.LT.3) THEN
	            WRITE(6,1030)
	         ELSE
		    WRITE(6,1040)
		 END IF
		 NEXT = .FALSE.
	         IF (.NOT.REMOTE_SET) CALL CLOSE_BULLFIL
		 GO TO 9999
	      END IF
	      GO TO 50
	   END IF
	   BLOCK = BLOCK_SAVE
	END IF

	NEXT = .FALSE.
	IF (REMOTE_SET.LT.3.AND..NOT.THREAD) THEN
           IF (INCMD(:4).NE.'SEAR'.AND.INCMD(:3).NE.'CUR') THEN
	      DIFF = COMPARE_BTIM(MSG_BTIM,LAST_READ_BTIM(1,FOLDER_NUMBER+1))
	      IF (DIFF.GT.0) THEN
	         CALL COPY2(LAST_READ_BTIM(1,FOLDER_NUMBER+1),MSG_BTIM)
	      END IF
	   END IF
	   IF (BULL_TAG.AND.BTEST(BULL_TAG,1)) CALL ADD_TAG(IER,2)
	   IF (INCMD.EQ.'LAST'.AND..NOT.READ_TAG) THEN
	      CALL COPY2(LAST_READ_BTIM(1,FOLDER_NUMBER+1),F_NEWEST_BTIM)
	   END IF
	ELSE
	   IF (REMOTE_SET.EQ.4) MESSAGE_ID = NEWS_MSGID
	   IF (BULL_NEWS_TAG) CALL ADD_TAG(IER,2)
	   IF (.NOT.THREAD.OR.INCMD(:4).NE.'READ') THEN 
	      IF (INCMD.EQ.'LAST'.AND..NOT.READ_TAG) THEN
	         CALL NEWS_UPDATE_NEWEST_MESSAGE(F_NBULL)
	      END IF
	      IF (INCMD(:4).NE.'SEAR'.AND.INCMD(:3).NE.'CUR') THEN
	         CALL NEWS_UPDATE_NEWEST_MESSAGE(BULL_READ)
	      ELSE
	         CALL NEWS_GET_NEWEST_MESSAGE(IER)
	         IF (IER.EQ.BULL_READ)
     &		    CALL NEWS_UPDATE_NEWEST_MESSAGE(BULL_READ)
	      END IF
	   END IF
	END IF

	EDIT = .FALSE.

	PAGE_WIDTH = REAL_PAGE_WIDTH

	IF (INCMD(:1).NE.' '.AND.READIT.EQ.0) THEN
	   IF (CLI$PRESENT('EDIT')) THEN
	      OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,
     &		RECL=LINE_LENGTH,STATUS='NEW',CARRIAGECONTROL='LIST')
	      IF (IER.NE.0) THEN
		 CALL ERRSNS(IDUMMY,IER)
		 CALL SYS_GETMSG(IER)
		 GO TO 9999
	      END IF
	      EDIT = .TRUE.
	      PAGE_WIDTH = LINE_LENGTH
	      PAGE = .FALSE.
	   END IF
	END IF

	IF (BULL_POINT.GT.F_NBULL) F_NBULL = BULL_POINT

	IF (REMOTE_SET.GE.3) THEN
	   WRITE (HEADLINE,'(1X,I,'' of '',I,''-'',I)')
     &				BULL_POINT,F_START,F_NBULL
	   DO WHILE (INDEX(HEADLINE,'- ').GT.0)
	      I = INDEX(HEADLINE,'- ')
	      HEADLINE(I+1:) = HEADLINE(I+2:)
	   END DO
	ELSE
	   WRITE (HEADLINE,'(1X,I,'' of '',I)') BULL_POINT,F_NBULL
	END IF
	DO WHILE (INDEX(HEADLINE,'  ').LT.TRIM(HEADLINE))
	   I = INDEX(HEADLINE,'  ')
	   HEADLINE(I:) = HEADLINE(I+1:)
	END DO
	I = TRIM(HEADLINE)
	HEADLINE = ' #'//HEADLINE(2:TRIM(HEADLINE))
	FLEN = TRIM(FOLDER_NAME)
	HEADLINE(REAL_PAGE_WIDTH-FLEN+1:) = FOLDER_NAME(:FLEN)
	IF (READIT.GT.0) THEN
	   WRITE(6,'(A)') '+'//HEADLINE(:TRIM(HEADLINE))
	ELSE IF (EDIT) THEN
	   WRITE(3,'(A)') HEADLINE(:TRIM(HEADLINE))
	ELSE
	   WRITE(6,'(1X,A)') HEADLINE(:TRIM(HEADLINE))
	END IF

	END = 1					! Outputted 1 line to screen

	IF (EXDATE(8:11).LT.'1995') THEN
	   IF (REMOTE_SET.NE.3) THEN
	      INPUT = 'Date:   '//DATE(:TRIM(DATE))//' '//TIME(:5)
     &				//'   (DELETED)'
	   ELSE
	      INPUT = 'Date:   '//DATE(:TRIM(DATE))//' '//TIME(:5)
	   END IF
	ELSE IF ((SYSTEM.AND.4).EQ.4) THEN	! Is entry shutdown bulletin?
	   INPUT = 'Date:   '//DATE(:TRIM(DATE))//' '//TIME(:5)
     &				//'   Expires on shutdown'
	ELSE IF ((SYSTEM.AND.2).EQ.2) THEN	! Is entry permanent bulletin?
	   INPUT = 'Date:   '//DATE(:TRIM(DATE))//' '//TIME(:5)
     &				//'   Permanent'
	ELSE
	   INPUT = 'Date:   '//DATE(:TRIM(DATE))//' '//TIME(:5)//
     &				'   Expires:   '//EXDATE//' '//EXTIME(:5)
	END IF
	IF ((SYSTEM.AND.1).EQ.1) THEN		! System bulletin?
	   INPUT = INPUT(:TRIM(INPUT))//' / System'
	END IF
	IF (EDIT) THEN
	   WRITE (3,'(A)') INPUT(:TRIM(INPUT))
	ELSE
	   WRITE (6,'(1X,A)') INPUT(:TRIM(INPUT))
	END IF

	END = END + 1

	LINE_OFFSET = 0
	CHAR_OFFSET = 0
	ILEN = LINE_LENGTH + 1
	CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: '
     &	   .AND..NOT.BTEST(SYSTEM,4)) THEN
	   INPUT = 'From:   '//INPUT(7:)
	   DO WHILE (TRIM(INPUT).GT.0)
	      I = MIN(PAGE_WIDTH,TRIM(INPUT))
	      IF (EDIT) THEN
		 WRITE(3,'(A)') INPUT(:I)
	      ELSE
		 WRITE(6,'(1X,A)') INPUT(:I)
	      END IF
	      INPUT = INPUT(I+1:)
	      END = END + 1
	   END DO
	   LINE_OFFSET = 1
	ELSE
	   IF (EDIT) THEN
	      WRITE(3,'(''From:   '',A)') FROM
	   ELSE
	      WRITE(6,'('' From:   '',A)') FROM
	   END IF
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
	      IF (EDIT) THEN
		 WRITE(3,'(A)') INPUT(:I)
	      ELSE
		 WRITE(6,'(1X,A)') INPUT(:I)
	      END IF
	      INPUT = INPUT(I+1:)
	      END = END + 1
	   END DO
	   LINE_OFFSET = LINE_OFFSET + 1
	   IF (EDIT) WRITE(3,'(1X)')
	ELSE
	   END = END + 1
	   IF (EDIT) THEN
	      WRITE(3,'(''Subj:   '',A)') DESCRIP(:TRIM(DESCRIP))
	      WRITE(3,'(1X,/,A)') INPUT(:LEN_TEMP)
	   ELSE
	      WRITE(6,'('' Subj:   '',A)') DESCRIP(:TRIM(DESCRIP))
	      IF (LINE_OFFSET.EQ.1) THEN
		 CHAR_OFFSET = 1 - PAGE_WIDTH
		 LINE_OFFSET = 2
		 IF (ROTC) CALL CONVERT_ROTC(INPUT,LEN_TEMP)
	      END IF
	   END IF
	END IF
	IF (LINE_OFFSET.EQ.0) ILEN = LINE_LENGTH + 1
	CALL CLOSE_BULLFIL			! End of bulletin file read

	IF (EDIT) GO TO 200

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
	   GO TO 9999
	ELSE
	   READ_COUNT = BLOCK			! Init bulletin record counter
	END IF

	GO TO 200

100	IF (READIT.EQ.0) THEN 			! If not 1st page of READ
	   WRITE(6,'(1X,A,/)') HEADLINE(:TRIM(HEADLINE)) ! Output header info
	   END = END + 2			! Increase display counter
	END IF

	SCRATCH_B = SCRATCH_B1			! Init queue pointer to header

200	DISPLAY = 0
	IF (READIT.GT.0) END = END - 2		! /READ can output 2 more lines

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
	      IF (EDIT) THEN
		 WRITE(3,'(A)') INPUT(:ILEN)
	      ELSE IF (CHAR_OFFSET.EQ.0) THEN
		 LEN_TEMP = ILEN
		 CALL CONVERT_TABS(INPUT,LEN_TEMP)
		 IF (ROTC) CALL CONVERT_ROTC(INPUT,LEN_TEMP)
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

	IF (EDIT) THEN
	   CLOSE (UNIT=3)
	   CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')
	   CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;*')
	   READ_COUNT = 0			! init bulletin record counter
	   GO TO 9999
	END IF

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

9999	POSTTIME = .FALSE.
	IF (KILL) BULL_USER_CUSTOM = IBSET(BULL_USER_CUSTOM,3)
	RETURN

1030	FORMAT(' No more messages.')
1040	FORMAT(' Message not found.')
1070	FORMAT(1X,/,' Press RETURN for more...',/)

2000	FORMAT(A)

	END





	SUBROUTINE CONVERT_ROTC(INPUT,LEN_TEMP)

	IMPLICIT INTEGER (A-Z)

	CHARACTER INPUT*(*)

	DO I=1,LEN_TEMP
	   IF (INPUT(I:I).GE.'A'.AND.INPUT(I:I).LE.'Z') THEN
	      INPUT(I:I) = CHAR(ICHAR(INPUT(I:I)) - 13)
	      IF (INPUT(I:I).LT.'A')
     &			INPUT(I:I) = CHAR(ICHAR(INPUT(I:I)) + 26)
	   ELSE IF (INPUT(I:I).GE.'a'.AND.INPUT(I:I).LE.'z') THEN
	      INPUT(I:I) = CHAR(ICHAR(INPUT(I:I)) - 13)
	      IF (INPUT(I:I).LT.'a')
     &			INPUT(I:I) = CHAR(ICHAR(INPUT(I:I)) + 26)

	   END IF
	END DO

	RETURN
	END






	SUBROUTINE READNEW(REDO)
C
C  SUBROUTINE READNEW
C
C  FUNCTION: Displays new non-system bulletins with prompts between bulletins.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*256 INCMD

	COMMON /SYSTEM_FOLDERS/ SYSTEM_FLAG(FLONG),DUMMY(2)

	COMMON /POINT/ BULL_POINT

	COMMON /READ_DISPLAY/ LINE_OFFSET

	COMMON /PAGE/ PAGE_LENGTH,PAGE_WIDTH,PAGING
	LOGICAL PAGING

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /ACCESS/ READ_ONLY
	LOGICAL READ_ONLY

	EXTERNAL BULLETIN_SUBCOMMANDS

	CHARACTER INREAD4*4,FILE_DEF*80,NUMREAD*8
	CHARACTER INREAD*1
	EQUIVALENCE (INREAD4,INREAD)

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

5	CALL READ_MSG(READ_COUNT,BULL_POINT+1)	! Read next bulletin
	BULL_POINT_READ = BULL_POINT
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

	GO TO 12

11	IF (READ_COUNT.GT.0) THEN
	   CALL OPEN_BULLDIR_SHARED
	   CALL OPEN_BULLFIL_SHARED
	   CALL READDIR(BULL_POINT,IER)
	   ILEN = LINE_LENGTH+1
	   DO I=1,LINE_OFFSET
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   END DO
	   CALL CLOSE_BULLFIL
	   CALL CLOSE_BULLDIR
	END IF

	BULL_POINT = BULL_POINT_SAVE
	LENGTH = LENGTH_SAVE
	BLOCK = BLOCK_SAVE

12	IF (READ_COUNT.EQ.0) THEN		! Prompt user in between
	   WRITE(6,1020)			! full screens or end of bull.
	ELSE
	   WRITE(6,1030)
	END IF

	CALL GET_INPUT_NOECHO(INREAD)
	CALL STR$UPCASE(INREAD,INREAD)	! Convert input to upper case

	BLOCK_SAVE = BLOCK
	LENGTH_SAVE = LENGTH
	BULL_POINT_SAVE = BULL_POINT

	IF (INREAD.EQ.'Q') THEN		! If Q , then QUIT
	   WRITE (6,'(''+Quit'',$)')
	   RETURN
	ELSE IF (INREAD.EQ.'D') THEN	! If D , then redisplay directory
	   WRITE (6,'(''+Dir'',$)')
	   REDO = .TRUE.
	   RETURN
	ELSE IF (INREAD.EQ.'F'.AND..NOT.CAPTIVE(1)) THEN
	   				! If F then copy bulletin to file
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

	   BULL_POINT = BULL_POINT_READ
	   INCMD = 'FILE '//BULL_PARAMETER(:LEN_P)
	   IER = CLI$DCL_PARSE(INCMD,BULLETIN_SUBCOMMANDS)
	   CALL FILE(0,.TRUE.,.FALSE.)
	   GO TO 11
	ELSE IF (INREAD.EQ.'P') THEN
	   WRITE (6,'(''+P'',$)')
	   BULL_POINT = BULL_POINT_READ
	   IF (REMOTE_SET.GE.3.OR.
     &	       INDEX(FOLDER_DESCRIP,'<').GT.0) THEN
	      WRITE(6,1040)
	      CALL GET_INPUT_NOECHO(INREAD)
	      CALL STR$UPCASE(INREAD,INREAD)
	      IF (INREAD.EQ.'P') THEN
		 WRITE (6,'(''+P'',$)')
		 INCMD = 'REPLY'
	      ELSE IF (INREAD.EQ.'U') THEN
		 WRITE (6,'(''+U'',$)')
		 INCMD = 'RESPOND'
	      ELSE IF (INREAD.EQ.'B') THEN
		 WRITE (6,'(''+B'',$)')
		 INCMD = 'RESPOND/LIST'
	      ELSE
		 GO TO 11
	      END IF
	      IER = CLI$DCL_PARSE(INCMD,BULLETIN_SUBCOMMANDS)
	      CALL RESPOND
	   ELSE IF (READ_ONLY) THEN
	      WRITE (6,'(
     &		 '' ERROR: You do not write access to this folder.'')')
	   ELSE
	      INCMD = 'REPLY'
	      IER = CLI$DCL_PARSE(INCMD,BULLETIN_SUBCOMMANDS)
	      CALL REPLY
	   END IF
	   GO TO 11
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

1000	FORMAT(' Read messages? Type N(No),E(Exit),message',
     &	 ' number, or any other key for yes: ',$)
1010	FORMAT(' No more messages.')
1020	FORMAT(1X,<PAGE_WIDTH>('-'),/,' Type Q(Quit),F(File),D(Dir),',
     &	'R(Read msg #),P(Reply) or other for next message: ',$)
1030	FORMAT(1X,<PAGE_WIDTH>('-'),/,' Type Q(Quit),F(File),N(Next),',
     &	'D(Dir),R(Read msg #),P(Reply) or other for MORE: ',$)
1040	FORMAT(' Type P to post reply, U to reply to user,',
     &	' B to do both, or other to quit: ',$)

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

	IF (FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)) THEN
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
	   CALL REWRITE_FOLDER_FILE(IER)
	   CALL CLOSE_BULLFOLDER
	ELSE
	   WRITE (6,'('' You are not authorized to set expiration.'')')
	END IF

	RETURN
	END




	LOGICAL FUNCTION NEWS_FEED()

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	NEWS_FEED = .FALSE.

	SLIST = INDEX(FOLDER_DESCRIP,'<')
	IF (SLIST.GT.0) THEN
	   I = SLIST + 1
	   FLEN = TRIM(FOLDER_DESCRIP)
	   DO WHILE (I.LE.FLEN)
	      IF (FOLDER_DESCRIP(I:I).EQ.'>') THEN
		 IF (INDEX(FOLDER_DESCRIP(SLIST:I),'.').GT.0)
     &		    NEWS_FEED = .TRUE.
		 RETURN
	      ELSE IF ((FOLDER_DESCRIP(I:I).LT.'A'.OR.
     &		  FOLDER_DESCRIP(I:I).GT.'Z').AND.
     &		  FOLDER_DESCRIP(I:I).NE.':'.AND.
     &		  FOLDER_DESCRIP(I:I).NE.'@'.AND.
     &		  FOLDER_DESCRIP(I:I).NE.'%') THEN
		 I = I + 1
	      ELSE
		 I = FLEN + 2
	      END IF
	   END DO
	END IF

	RETURN
	END




	LOGICAL FUNCTION MAIL_POST()

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	IF (NEWS_FEED()) THEN
	   MAIL_POST = INDEX(FOLDER_DESCRIP,'[').GT.0
	ELSE
	   MAIL_POST = INDEX(FOLDER_DESCRIP,'<').GT.0
	END IF

	RETURN
	END
