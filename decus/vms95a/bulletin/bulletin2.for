C
C  BULLETIN2.FOR, Version 4/20/95
C  Purpose: Contains subroutines for the bulletin board utility program.
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
	SUBROUTINE SET_BBOARD(BBOARD)
C
C  SUBROUTINE SET_BBOARD
C
C  FUNCTION: Set username for BBOARD for selected folder.
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFILES.INC'

	INCLUDE '($UAIDEF)'

	EXTERNAL CLI$_ABSENT

	CHARACTER EXPIRE*4,INPUT_BBOARD*12,TODAY*24,RESPONSE*4

	IF (TRIM(BBOARD_DIRECTORY).EQ.0) THEN
	 WRITE(6,'('' ERROR: System programmer has disabled BBOARD.'')')
	 RETURN
	END IF

	IF (FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)) THEN

	   CALL OPEN_BULLFOLDER		! Open folder file
	   CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)

	   IF (FOLDER_BBOARD(:2).EQ.'::'.OR.NEWS_FEED()) THEN
	      WRITE (6,'(
     &		'' ERROR: Cannot set BBOARD for remote folder.'')')
	      CALL CLOSE_BULLFOLDER
	      RETURN
	   END IF

	   IF (BBOARD) THEN
	      IER = CLI$GET_VALUE('BB_USERNAME',INPUT_BBOARD,INPUT_LEN)
	      IF (IER.NE.%LOC(CLI$_ABSENT)) THEN
		 CALL GET_UAF
     &		   (INPUT_BBOARD,USERB,GROUPB,ACCOUNTB,FLAGS,IER1)
		 CALL CLOSE_BULLFOLDER
	         IF (IER1.AND..NOT.BTEST(FLAGS,UAI$V_DISACNT)) THEN ! DISUSER?
	            WRITE (6,'('' ERROR: '',A,
     &			'' account needs DISUSER flag set.'')')
     &			INPUT_BBOARD(:INPUT_LEN)
		    RETURN
		 ELSE IF (IER1.AND.BTEST(USERB,31)) THEN
		    WRITE (6,'('' ERROR: User number of UIC cannot '',
     &				''be greater than 7777777777.'')')
		    RETURN
		 END IF
		 CALL OPEN_BULLFOLDER
		 CALL READ_FOLDER_FILE_TEMP(IER)
		 DO WHILE ((FOLDER1_BBOARD.NE.INPUT_BBOARD.OR.
     &		     FOLDER1_NUMBER.EQ.FOLDER_NUMBER).AND.IER.EQ.0)
		   CALL READ_FOLDER_FILE_TEMP(IER)
	         END DO
		 IF (FOLDER1_BBOARD.EQ.INPUT_BBOARD.AND.
     &		      FOLDER1_NUMBER.NE.FOLDER_NUMBER) THEN
		    WRITE (6,'('' WARNING: Account used by other folder.'',
     &		     '' If you specify the same account,'')')
		    WRITE (6,'('' you must specify the address'',
     &		     '' of the mailing list in the folder description.'')')
		    CALL GET_INPUT_PROMPT(RESPONSE,RLEN,
     &                 'Do you still want to make this change? ')
		    IF (RESPONSE(:1).NE.'y'.AND.RESPONSE(:1).NE.'Y') THEN
		       CALL CLOSE_BULLFOLDER
		       WRITE (6,'('' Folder was not modified.'')')
		       RETURN
		    END IF
		 END IF
		 IF (.NOT.IER1) THEN
		    CALL CLOSE_BULLFOLDER
		    WRITE (6,'('' WARNING: '',A,'' account not in SYSUAF'',
     &		       '' file.'')') INPUT_BBOARD(:INPUT_LEN)
		    CALL GET_INPUT_PROMPT(RESPONSE,RLEN,
     &                 'Is the name a mail forwarding entry? '//
     &		       '(Y/N with N as default): ')
		    IF (RESPONSE(:1).NE.'y'.AND.RESPONSE(:1).NE.'Y') THEN
		       WRITE (6,'('' Folder was not modified.'')')
		       RETURN
		    END IF
		    CALL OPEN_BULLFOLDER
		    USERB = 1		! Fake userb/groupb, as old method of
		    GROUPB = 1		! indicating /SPECIAL used [0,0]
		 END IF
		 GROUPB1 = GROUPB
		 USERB1 = USERB
		 ACCOUNTB1 = ACCOUNTB
		 CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)
		 GROUPB = GROUPB1
		 USERB = USERB1
		 ACCOUNTB = ACCOUNTB1
		 FOLDER_BBOARD = INPUT_BBOARD
		 CALL OPEN_BULLUSER
		 CALL SYS$ASCTIM(,TODAY,,)		! Get the present time
		 CALL READ_USER_FILE_HEADER(IER)
		 CALL SYS_BINTIM(TODAY,BBOARD_BTIM)
		 REWRITE (4) USER_HEADER
		 CALL CLOSE_BULLUSER
		 IF (CLI$PRESENT('SPECIAL')) THEN	! SPECIAL specified?
		    USERB = IBSET(USERB,31)	! Set bit to show /SPECIAL
		    IF (CLI$PRESENT('VMSMAIL')) THEN
		       GROUPB = IBSET(GROUPB,31)   ! Set bit to show /VMSMAIL
		    END IF
		 END IF
	      ELSE IF (CLI$PRESENT('SPECIAL')) THEN
	         USERB = IBSET(0,31)		! Set top bit to show /SPECIAL
	         GROUPB = 0
	         DO I=1,LEN(FOLDER_BBOARD)
		    FOLDER_BBOARD(I:I) = ' '
	         END DO
	      ELSE IF (FOLDER_BBOARD(:4).EQ.'NONE') THEN
	         WRITE (6,'('' ERROR: No BBOARD specified for folder.'')')
	      END IF

	      IER = CLI$GET_VALUE('EXPIRATION',EXPIRE,EX_LEN)
	      IF (IER.NE.%LOC(CLI$_ABSENT)) THEN
	         IF (EX_LEN.GT.3) EX_LEN = 3
	         READ (EXPIRE,'(I<EX_LEN>)') TEMP
		 IF (TEMP.GT.BBEXPIRE_LIMIT.AND..NOT.SETPRV_PRIV()) THEN
		    WRITE (6,'('' ERROR: Expiration cannot be > '',
     &			I3,'' days.'')') BBEXPIRE_LIMIT
		    CALL CLOSE_BULLFOLDER
		    RETURN
		 ELSE IF (TEMP.LE.0) THEN
		    WRITE (6,'('' ERROR: Expiration must be > 0.'')')
		    CALL CLOSE_BULLFOLDER
		    RETURN
		 ELSE
		    FOLDER_BBEXPIRE = TEMP
		 END IF
	      ELSE IF (.NOT.CLI$PRESENT('EXPIRATION')) THEN
		 FOLDER_BBEXPIRE = -1
	      END IF
	   ELSE
	      FOLDER_BBOARD = 'NONE'
	   END IF

	   CALL REWRITE_FOLDER_FILE(IER)
	   CALL CLOSE_BULLFOLDER
	   WRITE (6,'('' BBOARD has been modified for folder.'')')
	ELSE
	   WRITE (6,'('' You are not authorized to modify BBOARD.'')')
	END IF

	RETURN
	END






	SUBROUTINE SET_SYSTEM(SYSTEM_SET)
C
C  SUBROUTINE SET_SYSTEM
C
C  FUNCTION: Set SYSTEM specification for selected folder.
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	IF (FOLDER_NUMBER.LT.0.OR.FOLDER_NUMBER.GE.1000) THEN
	   WRITE (6,'('' ERROR: Cannot modify for remote folder.'')')
	ELSE IF (SETPRV_PRIV()) THEN
	   CALL OPEN_BULLFOLDER		! Open folder file
	   CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)
	   IF (SYSTEM_SET) THEN
	      FOLDER_FLAG = IBSET(FOLDER_FLAG,2)
	      WRITE (6,'('' SYSTEM designation has been set.'')')
	   ELSE
	      FOLDER_FLAG = IBCLR(FOLDER_FLAG,2)
	      WRITE (6,'('' SYSTEM designation has been removed.'')')
	   END IF
	   CALL REWRITE_FOLDER_FILE(IER)
	   CALL MODIFY_SYSTEM_LIST(0)
	   CALL CLOSE_BULLFOLDER
	   CALL UPDATE_SHUTDOWN(FOLDER_NUMBER)
	ELSE
	   WRITE (6,'('' You are not authorized to modify SYSTEM.'')')
	END IF

	RETURN
	END



	SUBROUTINE MODIFY_SYSTEM_LIST(FILE_OPENED)

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /SYSTEM_FOLDERS/ SYSTEM_FLAG(FLONG),NODENAME
	CHARACTER NODENAME*8

	COMMON /SHUTDOWN/ NODE_NUMBER,NODE_AREA
	COMMON /SHUTDOWN/ SHUTDOWN_FLAG(FLONG)

	INTEGER SHUTDOWN_BTIM(FLONG),VERSION(FLONG)

	CHARACTER UPDATE*12,UPTIME*8

	INTEGER UP_BTIM(2)

	IF (.NOT.FILE_OPENED) CALL OPEN_BULLUSER

	DO WHILE (REC_LOCK(IER))
	   READ (4,KEY='*SYSTEM',IOSTAT=IER) 
     &		TEMP_USER,NODENAME,NODE_NUMBER,NODE_AREA,VERSION,
     &		SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG
	END DO

	IF (IER.NE.0.OR.VERSION(1).NE.168) THEN
	   DO I=1,FLONG
	      SYSTEM_FLAG(I) = 0
	      SHUTDOWN_FLAG(I) = 0
	   END DO
	   CALL SET2(SYSTEM_FLAG,0)
	   CALL LIB$SYS_TRNLOG('SYS$NODE',,NODENAME)
	   NODENAME = NODENAME(2:INDEX(NODENAME,':')-1)
	   SHUTDOWN_BTIM(1) = 0
	   SHUTDOWN_BTIM(2) = 0
	   NODE_NUMBER = 0
	   NODE_AREA = 0
	   IF (IER.EQ.0) THEN
	      DO WHILE (TEMP_USER(:7).EQ.'*SYSTEM'.AND.IER.EQ.0)
	         DELETE (UNIT=4)
	         DO WHILE (REC_LOCK(IER))
	           READ (4,IOSTAT=IER) TEMP_USER
		 END DO
	      END DO
	      IER = 2
	   ELSE
	      VERSION(1) = 168
	   END IF
	END IF

	IF (VERSION(1).NE.168) THEN
	   CALL CLOSE_BULLFOLDER
	   CALL OPEN_BULLFOLDER
	   NODE_AREA = 0
	   DO I=1,FLONG
	      SYSTEM_FLAG(I) = 0
	   END DO
	   IER1 = 0
	   DO WHILE (IER1.EQ.0)
	      CALL READ_FOLDER_FILE_TEMP(IER1)
	      IF (BTEST(FOLDER1_FLAG,2).AND.IER1.EQ.0) THEN
		 CALL SET2(SYSTEM_FLAG,FOLDER1_NUMBER)
	      END IF
	   END DO
	   VERSION(1) = 168
	END IF

	IF (BTEST(FOLDER_FLAG,2)) THEN
	   CALL SET2(SYSTEM_FLAG,FOLDER_NUMBER)
	ELSE
	   CALL CLR2(SYSTEM_FLAG,FOLDER_NUMBER)
	END IF

	CALL SYS_BINTIM('-',UP_BTIM)	! Get today's date
	DIFF = COMPARE_BTIM(SHUTDOWN_BTIM,UP_BTIM)
	IF (DIFF.GE.0) THEN	! Must have been in a time wrap
	   SHUTDOWN_BTIM(1) = UP_BTIM(1)
	   SHUTDOWN_BTIM(2) = UP_BTIM(2)
	END IF

	CALL GET_UPTIME(UPDATE,UPTIME)

	CALL SYS_BINTIM(UPDATE//' '//UPTIME,UP_BTIM)

	IF (NODE_AREA.EQ.0) THEN
	   IF (SHUTDOWN_BTIM(1).EQ.0) THEN
	      DIFF = -1
	   ELSE
	      DIFF = COMPARE_BTIM(SHUTDOWN_BTIM,UP_BTIM)
	   END IF
	   IF (DIFF.EQ.-1) THEN
	      CALL GET_NODE_NUMBER(NODE_NUMBER,NODE_AREA)
	      SHUTDOWN_BTIM(1) = UP_BTIM(1)
	      SHUTDOWN_BTIM(2) = UP_BTIM(2)
	      DO I=1,FLONG
		 SHUTDOWN_FLAG(I) = SYSTEM_FLAG(I)
              END DO
	   END IF
	ELSE			! Test to make sure NODE_AREA is zero
	   SEEN_FLAG = 0		! if all of SHUTDOWN_FLAG is zero
	   DO I=1,FLONG
	      IF (SHUTDOWN_FLAG(I).NE.0) SEEN_FLAG = 1
	   END DO
	   IF (SEEN_FLAG.EQ.0) NODE_AREA = 0
	END IF

	IF (IER.NE.0) THEN
	   WRITE (4,IOSTAT=IER)
     &		'*SYSTEM     ',NODENAME,NODE_NUMBER,NODE_AREA,VERSION,
     &		SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG
	ELSE
	   REWRITE (4,IOSTAT=IER)
     &		TEMP_USER,NODENAME,NODE_NUMBER,NODE_AREA,VERSION,
     &		SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG
	END IF

	CALL READ_PERM

	IF (.NOT.FILE_OPENED) THEN
	   CALL CLOSE_BULLUSER
	   IF (REMOTE_SET) THEN
	      WRITE(REMOTE_UNIT,'(3A)',IOSTAT=IER1) 14,
     &				BTEST(FOLDER_FLAG,2),NODENAME
	      IF (IER1.NE.0) THEN
	         CALL DISCONNECT_REMOTE
	         IF (.NOT.FILE_OPENED) CALL CLOSE_BULLUSER
	         RETURN
	      END IF
	   END IF
	END IF

	RETURN
	END


	
	SUBROUTINE GET_NODE_NUMBER(NODE_NUMBER,NODE_AREA)

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($SYIDEF)'

	CALL INIT_ITMLST	! Initialize item list
				! Now add items to list
	CALL ADD_2_ITMLST(4,SYI$_NODE_AREA,%LOC(NODE_AREA))
	CALL ADD_2_ITMLST(4,SYI$_NODE_NUMBER,%LOC(NODE_NUMBER))
	CALL END_ITMLST(GETSYI_ITMLST)	! Get address of itemlist

	IER = SYS$GETSYIW(,,,%VAL(GETSYI_ITMLST),,,)	! Get Info command.
C
C  NODE_AREA is set to 0 after shutdown messages are deleted.
C  If node is not part of cluster, NODE_AREA will be 0,
C  so set it to 1 as a dummy value to cause messages to be deleted.
C
	IF (NODE_AREA.EQ.0) NODE_AREA = 1

	RETURN
	END




	SUBROUTINE SET_NODE(NODE_SET)
C
C  SUBROUTINE SET_NODE
C
C  FUNCTION: Set or reset remote node specification for selected folder.
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFILES.INC'

	INCLUDE 'BULLDIR.INC'

	EXTERNAL CLI$_ABSENT

	CHARACTER RESPONSE*4,FOLDER_SAVE*44

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	IF (CLI$PRESENT('FOLDER')) THEN
	   IER = CLI$GET_VALUE('FOLDER',FOLDER1) ! Get folder name
	   FOLDER_SAVE = FOLDER
	   CALL OPEN_BULLFOLDER_SHARED		! Open folder file
	   CALL READ_FOLDER_FILE_KEYNAME(FOLDER1,IER)
	   IF (IER.EQ.0) THEN
	      IF (.NOT.FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)) THEN
		 WRITE (6,'('' ERROR: No privs to modify folder.'')')
		 IER = 1
	      END IF
	   ELSE
	      WRITE (6,'('' ERROR: Specified folder not found.'')')
	   END IF
	   IF (IER.NE.0) THEN
	      CALL READ_FOLDER_FILE_KEYNAME(FOLDER_SAVE,IER)
	      CALL CLOSE_BULLFOLDER
	      RETURN
	   END IF
	   CALL CLOSE_BULLFOLDER
	END IF

	IF (FOLDER_NUMBER.EQ.0) THEN
	   WRITE (6,'('' Cannot set remote node for the default folder.'')')
	ELSE IF (FOLDER_NUMBER.LT.0.OR.FOLDER_NUMBER.GE.1000) THEN
	   WRITE (6,'('' Cannot set remote node for this folder.'')')
	ELSE IF (FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)) THEN
	   IF (.NOT.NODE_SET) THEN
	      IF (INDEX(FOLDER_BBOARD,'*').GT.0) THEN
		 REMOTE_SET_SAVE = REMOTE_SET
		 REMOTE_SET = .FALSE.
	         FOLDER_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//
     &		     FOLDER
	         CALL OPEN_BULLDIR		! Remove directory file which
	         CALL CLOSE_BULLDIR_DELETE	! contains remote folder name
		 REMOTE_SET = REMOTE_SET_SAVE
	      END IF
	      FOLDER1_BBOARD = 'NONE'
	      WRITE (6,'('' Remote node setting has been removed.'')')
	      IF (.NOT.CLI$PRESENT('FOLDER')) REMOTE_SET = .FALSE.
	   ELSE
	      CALL GET_INPUT_PROMPT(RESPONSE,RLEN,
     &          'Are you sure you want to make folder '//
     &	        FOLDER(:TRIM(FOLDER))//
     &		' remote? (Y/N with N as default): ')
	      IF (RESPONSE(:1).NE.'y'.AND.RESPONSE(:1).NE.'Y') THEN
	        WRITE (6,'('' Folder was not modified.'')')
	        RETURN
	      END IF
	      IF (.NOT.CLI$GET_VALUE('REMOTENAME',FOLDER1)) THEN
	         FOLDER1 = FOLDER
	      END IF
	      IER = CLI$GET_VALUE('NODENAME',FOLDER1_BBOARD,FLEN)
	      FOLDER1_BBOARD = '::'//FOLDER1_BBOARD(:FLEN)
	      CALL CONNECT_REMOTE_FOLDER(READ_ONLY,IER)
	      IF (IER.NE.0) THEN
	         WRITE (6,'(
     &		  '' ERROR: Folder not accessible on remote node.'')')
	         RETURN
	      ELSE
	         WRITE (6,'('' Folder has been converted to remote.'')')
	      END IF
	      FOLDER_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//
     &		FOLDER
	      REMOTE_SET_SAVE = REMOTE_SET
	      REMOTE_SET = .FALSE.
	      CALL OPEN_BULLDIR			! Remove directory file
	      CALL OPEN_BULLFIL			! Remove bulletin file
	      CALL CLOSE_BULLFIL_DELETE
	      CALL CLOSE_BULLDIR_DELETE
	      IF (FOLDER.NE.FOLDER1) THEN	! Different remote folder name?
	         CALL OPEN_BULLDIR		! If so, put name in header
		 BULLDIR_HEADER(13:) = FOLDER1	! of directory file.
		 CALL WRITEDIR_NOCONV(0,IER)
	         CALL CLOSE_BULLDIR
	         FOLDER1_BBOARD = FOLDER1_BBOARD(:FLEN+2)//'*'
	      END IF
	      REMOTE_SET = REMOTE_SET_SAVE
	      IF (.NOT.CLI$PRESENT('FOLDER')) REMOTE_SET = .TRUE.
	   END IF
	   CALL OPEN_BULLFOLDER		! Open folder file
	   CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)
	   IF (.NOT.NODE_SET.AND.FOLDER_BBOARD(:2).EQ.'::'
     &			.AND.BTEST(FOLDER_FLAG,2)) THEN
	      OPEN (UNIT=17,STATUS='UNKNOWN',IOSTAT=IER,
     &		RECL=256,FILE=FOLDER_BBOARD(3:TRIM(FOLDER_BBOARD))
     &		//'::"TASK=BULLETIN1"')
	      IF (IER.EQ.0) THEN	! Disregister remote SYSTEM folder
		 WRITE(17,'(2A)',IOSTAT=IER) 14,0
		 CLOSE (UNIT=17)
	      END IF
	   END IF
	   FOLDER_BBOARD = FOLDER1_BBOARD
	   IF (NODE_SET) THEN
	      F_NBULL = F1_NBULL
	      F_NEWEST_BTIM(1) = F1_NEWEST_BTIM(1)
	      F_NEWEST_BTIM(2) = F1_NEWEST_BTIM(2)
	      F_NEWEST_NOSYS_BTIM(1) = F1_NEWEST_NOSYS_BTIM(1)
	      F_NEWEST_NOSYS_BTIM(2) = F1_NEWEST_NOSYS_BTIM(2)
	      FOLDER_FLAG = 0
	      F_EXPIRE_LIMIT = F1_EXPIRE_LIMIT
	   ELSE
	      F_NBULL = 0
	   END IF
	   CALL REWRITE_FOLDER_FILE(IER)
	   CALL CLOSE_BULLFOLDER
	ELSE
	   WRITE (6,'('' You are not authorized to modify NODE.'')')
	END IF

	IF (CLI$PRESENT('FOLDER')) THEN
	   CALL OPEN_BULLFOLDER_SHARED		! Open folder file
	   CALL READ_FOLDER_FILE_KEYNAME(FOLDER_SAVE,IER)
	   CALL CLOSE_BULLFOLDER
	   FOLDER_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//
     &		FOLDER
	END IF

	RETURN
	END




	SUBROUTINE RESPOND
C
C  SUBROUTINE RESPOND
C
C  FUNCTION: Sends a mail message in reply to a posted message.
C
C  NOTE: Modify the last SPAWN statement to specify the command
C	you use to send mail to sites other than via MAIL.
C	If you always use a different command, modify both
C	spawn commands.
C
	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /EDIT/ EDIT_DEFAULT
	DATA EDIT_DEFAULT/.FALSE./

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*256 INCMD

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /PAGE/ PAGE_LENGTH,PAGE_WIDTH,PAGING

	COMMON /INDESCRIP/ INDESCRIP
	CHARACTER*(INPUT_LENGTH) INDESCRIP

	CHARACTER INFROM*(INPUT_LENGTH)

	EXTERNAL CLI$_NEGATED,CLI$_ABSENT

	MSG_OWN = .FALSE.

	IF (INCMD(:4).EQ.'REPL') THEN
	   IF (REMOTE_SET.GE.3) THEN
              BULL_PARAMETER = 'news group.'
           ELSE
              BULL_PARAMETER = 'mailing list.'
           END IF
	   IF (CLI$PRESENT('ALL')) THEN
	      IF (REMOTE_SET.GE.3) THEN
                 BULL_PARAMETER = 'message owner and news group.'
              ELSE
                 BULL_PARAMETER = 'message owner and mailing list.'
              END IF
	      MSG_OWN = .TRUE.
	   END IF
	ELSE IF (INCMD(:4).EQ.'RESP') THEN
	   MSG_OWN = .TRUE.
	   BULL_PARAMETER = 'message owner.'
	   IF (CLI$PRESENT('LIST')) THEN 
	      IF (REMOTE_SET.GE.3) THEN 
	   	 BULL_PARAMETER = 'message owner and news group.'
	      ELSE
		 BULL_PARAMETER = 'message owner and mailing list.'
	      END IF

	   END IF
	ELSE IF (REMOTE_SET.GE.3) THEN 
	   BULL_PARAMETER = 'news group.'
	ELSE
	   BULL_PARAMETER = 'mailing list.'
	END IF

	LIST = INDEX(BULL_PARAMETER,'mailing list')
	IF (NEWS_FEED().AND.LIST.GT.0) THEN
	   BULL_PARAMETER(LIST:) = 'news group'//
     &				   BULL_PARAMETER(LIST+LEN('mailing list'):)
	END IF

	IF (MSG_OWN.AND.BTEST(CAPTIVE(-1),1)) THEN
           WRITE (6,'('' ERROR: MAIL invalid from DISMAIL account.'')')
           RETURN
	END IF

	WRITE (6,'('' Sending message to '',A)')
     &	   BULL_PARAMETER(:TRIM(BULL_PARAMETER))

	IF (INCMD(:4).NE.'POST') THEN
	   IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been read
	      WRITE(6,'('' ERROR: You have not read any message.'')')
	      RETURN			! And return
	   END IF

	   CALL OPEN_BULLDIR_SHARED

	   CALL READDIR(BULL_POINT,IER)	! Get info for specified bulletin

	   IF (IER.NE.BULL_POINT+1) THEN	! Was bulletin found?
	      WRITE(6,'('' ERROR: Bulletin was not found.'')')
	      CALL CLOSE_BULLDIR		! If not, then error out
	      RETURN
	   END IF

	   CALL OPEN_BULLFIL_SHARED

	   ILEN = LINE_LENGTH + 1

	   CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   END IF
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THEN
	      INDESCRIP = INPUT(7:)
	   ELSE
	      INDESCRIP = DESCRIP
	   END IF

	   CALL CLOSE_BULLFIL

	   CALL CLOSE_BULLDIR

	   IF (STREQ(INDESCRIP(:3),'RE:')) THEN
	      INDESCRIP = 'RE:'//INDESCRIP(4:)
	   ELSE
	      INDESCRIP = 'RE: '//INDESCRIP
	   END IF
	END IF

	IF (REMOTE_SET.GE.3) THEN 
	   IF (CLI$PRESENT('EXPIRATION')) THEN
	      CALL GET_EXPIRED(INPUT,IER)
	      IF (.NOT.IER) RETURN
	      EXDATE = INPUT(:11)
	      EXTIME = INPUT(13:23)
	   END IF
	END IF

	IF (CLI$PRESENT('SUBJECT')) THEN
	   IER = CLI$GET_VALUE('SUBJECT',INDESCRIP,LENDES)
	   IF (LENDES.GT.LEN(INDESCRIP)) THEN
	      WRITE(6,'('' ERROR: Subject length exceeded.'')')
	      RETURN
	   END IF
	ELSE IF (INCMD(:4).EQ.'POST') THEN
	   WRITE(6,'('' Enter subject of message:'')')
	   CALL GET_LINE(INDESCRIP,LENDES)
	   IF (LENDES.LE.0) THEN
	      WRITE(6,'('' ERROR: No subject specified.'')')
	      RETURN
	   END IF
	ELSE
	   WRITE (6,'('' Message will have the subject:'')')
	   WRITE (6,'(1X,A)') INDESCRIP(:MIN(TRIM(INDESCRIP),PAGE_WIDTH))
	END IF

	IF ((CLI$PRESENT('EDIT').OR.EDIT_DEFAULT).AND.	! If /EDIT specified
     &      (CLI$PRESENT('EDIT').NE.%LOC(CLI$_NEGATED))) THEN
	   EDIT = .TRUE.
	   CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;*')
	ELSE
	   EDIT = .FALSE.
	END IF

	TEXT = CLI$PRESENT('EXTRACT')

	LIST = CLI$PRESENT('LIST')

	ILEN = 0

	FILESPEC = CLI$GET_VALUE('FILESPEC',INPUT,ILEN)
	IF (FILESPEC.NE.%LOC(CLI$_ABSENT)) THEN
	   CALL DISABLE_PRIVS
	   OPEN (UNIT=4,FILE=INPUT(:ILEN),STATUS='OLD',READONLY,
     &		 SHARED,IOSTAT=IER,FORM='FORMATTED')
	   CALL ENABLE_PRIVS
	   IF (IER.NE.0) THEN
	      WRITE(6,'('' ERROR: Specified file cannot be opened.'')')
	      RETURN
           END IF 
	END IF

	FOUNDFILE = FILESPEC

	IF (EDIT.AND.(TEXT.OR.FILESPEC)) THEN
	   OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,
     &		RECL=LINE_LENGTH+2,STATUS='NEW',CARRIAGECONTROL='LIST')

	   IF (IER.NE.0) THEN
	      CALL ERRSNS(IDUMMY,IER)
	      CALL SYS_GETMSG(IER)
	      GO TO 900
	   END IF
	ELSE IF (TEXT.AND..NOT.EDIT) THEN
	   WRITE (6,'('' ERROR: Cannot extract text without /EDIT.'')')
	   GO TO 900
	END IF

	LENFRO = 0
	DO WHILE (CLI$GET_VALUE('CC',INPUT,ILEN)
     &	    .NE.%LOC(CLI$_ABSENT))		
	   CALL ADD_PROTOCOL(INPUT,ILEN)
	   IF (LENFRO.EQ.0) THEN
	      INFROM = INPUT(:ILEN)//','
	   ELSE
	      INFROM = INFROM(:LENFRO)//INPUT(:ILEN)//','
	   END IF
	   LENFRO = LENFRO + ILEN + 1
	END DO
	IF (LENFRO.GT.0.AND..NOT.MSG_OWN) LENFRO = LENFRO - 1

	IF ((EDIT.AND.TEXT).OR.INCMD(:4).NE.'POST') THEN
	   CALL OPEN_BULLFIL_SHARED

	   ILEN = LINE_LENGTH + 1

	   CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN
	      IF (MSG_OWN) THEN
	         ILEN = TRIM(INPUT) - 6
	         INFROM = INFROM(:LENFRO)//INPUT(7:)
	         CALL ADD_PROTOCOL(INFROM(LENFRO+1:),ILEN)
	         LENFRO = LENFRO + ILEN
	      END IF
	      IF (EDIT.AND.TEXT) THEN
		 INPUT = INPUT(7:)
		 IF (INDEX(INPUT,'%"').GT.0) THEN
		    INPUT = INPUT(INDEX(INPUT,'%"')+2:TRIM(INPUT)-1)
		 END IF
	         WRITE (3,'(A)') 'In a previous article, '//
     &			INPUT(:TRIM(INPUT))//' wrote:'
	      END IF
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   ELSE IF (MSG_OWN) THEN
	      CALL ADD_PROTOCOL(FROM,0)
	      INFROM = INFROM(:LENFRO)//FROM
	      LENFRO = TRIM(FROM) + LENFRO
	   END IF

	   IF (EDIT.AND.TEXT) THEN
	      IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THEN
	         CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      END IF
	      LEN_P = 0
	      IF (CLI$PRESENT('INDENT').NE.%LOC(CLI$_NEGATED)) THEN
	         CALL CLI$GET_VALUE('INDENT',BULL_PARAMETER,LEN_P)
	      END IF
	      DO WHILE (ILEN.GT.0)		! Copy bulletin into file
	         IF (LEN_P.EQ.0) THEN
	            WRITE (3,'(A)') INPUT(:ILEN)
	         ELSE
	            WRITE (3,'(A)') BULL_PARAMETER(:LEN_P)//INPUT(:ILEN)
	         END IF
	         CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      END DO

	      IF (FILESPEC) THEN
		 WRITE (3,'(A)') ' '
		 IER = 0
		 DO WHILE (IER.EQ.0)
		    READ (4,'(Q,A)',IOSTAT=IER) ILEN,INPUT
		    IF (IER.EQ.0) WRITE (3,'(A)') INPUT(:ILEN)
		 END DO
		 CLOSE (UNIT=4)
	         FILESPEC = .FALSE.
	      END IF

	      CLOSE (UNIT=3)			! Bulletin copy completed
	   END IF

	   CALL CLOSE_BULLFIL
	END IF

	IF (EDIT.AND.FILESPEC.AND..NOT.TEXT) THEN
	   IER = 0
	   ICOUNT = 0
	   DO WHILE (IER.EQ.0)
	      READ (4,'(Q,A)',IOSTAT=IER) ILEN,INPUT
	      IF (IER.EQ.0) THEN
		 WRITE (3,'(A)') INPUT(:ILEN)
		 ICOUNT = ICOUNT + 1
	      END IF
	   END DO
	   CLOSE (UNIT=4)
	   FILESPEC = .FALSE.
	   IF (ICOUNT.EQ.0) THEN
	      CLOSE (UNIT=3,STATUS='DELETE')
	   ELSE
	      CLOSE (UNIT=3)
	   END IF
	END IF

	IF (LIST.AND.REMOTE_SET.LT.3) THEN
	   SLIST = INDEX(FOLDER_DESCRIP,'<')
	   IF (SLIST.GT.0) THEN
	      IF (.NOT.NEWS_FEED()) THEN
		 INPUT = FOLDER_DESCRIP(SLIST+1:)
		 ILEN = INDEX(INPUT,'>') - 1
		 IF (ILEN.EQ.-1) ILEN = TRIM(INPUT)
		 INPUT = INPUT(:ILEN)
	         CALL ADD_PROTOCOL(INPUT,ILEN)
	         IF (LENFRO.GT.0.AND.INFROM(LENFRO:LENFRO).NE.',') THEN
		    INFROM = INFROM(:LENFRO)//','
		    LENFRO = LENFRO + 1
		 END IF
	         INFROM = INFROM(:LENFRO)//INPUT(:ILEN)
	         LENFRO = LENFRO + ILEN
	      ELSE
		 FOLDER1_DESCRIP = 
     &			FOLDER_DESCRIP(SLIST+1:TRIM(FOLDER_DESCRIP)-1)
		 IF (FOLDER1_DESCRIP(1:1).EQ.'@') THEN
		    WRITE(6,'('' ERROR: Multiple newsgroup feed'',
     &			'' is present.'')')
		    GO TO 900
		 END IF
	      END IF
	   ELSE
	      WRITE (6,'('' ERROR: No list address'',
     &			'' found in folder description.'')')
	      GO TO 900
	   END IF
	END IF

	I = 1		! Must change all " to "" in FROM field
	DO WHILE (I.LE.LENFRO)
	   IF (INFROM(I:I).EQ.'"') THEN
	      INFROM = INFROM(:I)//'"'//INFROM(I+1:)t
	      I = I + 1
	      LENFRO = LENFRO + 1
	   END IF
	   I = I + 1 
	END DO

	STATUS = .TRUE.

	IF (EDIT) THENn
	   CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')p
	   CONTEXT = 0
	   IER =  LIB$FIND_FILE('SYS$LOGIN:BULL.SCR',INPUT,CONTEXT)
	   IF (TEXT.OR.FOUNDFILE) THENn
	      VERSION = INDEX(INPUT,';') + 1 
	      IF (INPUT(VERSION:VERSION).EQ.'1') THEN
	         CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;*')
	      ELSEE
	         IER = 0
	      END IF 
	   ELSE IF (IER) THEN
	      IER = 0
	   END IF
	   IF (IER.EQ.0) THEN
	      CALL ADD_SIGNATURE(0,'SYS$LOGIN:BULL.SCR',FOLDER_NAME)I
	      IF ((NEWS_FEED().OR.REMOTE_SET.GE.3).AND.LIST) THEN
	         CALL NEWS_POST('SYS$LOGIN:BULL.SCR',.FALSE.,IER,
     &			INDESCRIP)O
		 STATUS = IER.EQ.0
	         IF (IER.EQ.0) THEN
		    WRITE (6,'('' Message successfully posted.'')')i
	         END IF
	      END IFE
	      IF (IER.EQ.0.AND.LENFRO.GT.0) THENR
	         CALL RESPOND_MAIL('SYS$LOGIN:BULL.SCR',INFROM,
     &			   INDESCRIP,STATUS)_
	      END IFN
	   END IF
	ELSE
	   OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,
     &		RECL=LINE_LENGTH,STATUS='NEW',CARRIAGECONTROL='LIST')o
	   IF (.NOT.FILESPEC) THEN 
	      WRITE (6,'('' Enter message: End with ctrl-z,'',B
     &			 '' cancel with ctrl-c'')')
	      ILEN = LINE_LENGTH + 1		! Length of input lineN
	      ICOUNT = 0			! Character count counter 
	      DO WHILE (ILEN.GE.0)		! Input until no more input
	         CALL GET_LINE(INPUT,ILEN)	! Get input line
	         IF (ILEN.GT.LINE_LENGTH) THEN	! Input line too longE
		    WRITE(6,'('' ERROR: Input line length > '',I,U
     &			   ''.  Reinput:'')') LINE_LENGTH
	         ELSE IF (ILEN.GE.0) THEN	! If good input line enteredH
		    ICOUNT = ICOUNT + ILEN		! Update counter
		    WRITE(3,'(A)') INPUT(:ILEN)	! Save line in scratch file 
	         END IF
	      END DON
	   ELSE
	      IER = 0
	      ICOUNT = 0E
	      DO WHILE (IER.EQ.0)
		 READ (4,'(Q,A)',IOSTAT=IER) ILEN,INPUTE
		 IF (IER.EQ.0) THENB
		    ICOUNT = ICOUNT + 1L
		    WRITE (3,'(A)') INPUT(:ILEN)
		 END IF
	      END DOB
	      CLOSE (UNIT=4)N
	      FILESPEC = .FALSE.U
	   END IF
	   IF (ILEN.EQ.-1.OR.ICOUNT.EQ.0) THEN	! CTRL_C or No lines
	      CLOSE (UNIT=3) 
	      IER = 1
	   ELSE
	      CALL ADD_SIGNATURE(3,' ',FOLDER_NAME)
	      REWIND (UNIT=3)
	      IF ((NEWS_FEED().OR.REMOTE_SET.GE.3).AND.LIST) THEN
	         CALL NEWS_POST('SYS$LOGIN:BULL.SCR',.TRUE.,IER,E
     &			   INDESCRIP)
		 STATUS = IER.EQ.0
	         IF (IER.EQ.0)  WRITE (6,'('' Message successfully posted.'')')
	      ELSE
		 IER = 0
	      END IFD
	      CLOSE (UNIT=3)(
	      IF (IER.EQ.0.AND.LENFRO.GT.0) THENR
		 CALL RESPOND_MAIL('SYS$LOGIN:BULL.SCR',INFROM, 
     &			   INDESCRIP,STATUS)E
	      END IF 
	   END IF
	END IF,
	IF (IER.NE.0) THENU
	   WRITE (6,'('' ERROR: No message added.'')')(
	   IF (.NOT.STATUS) THENT
	      CALL GET_INPUT_PROMPT(INPUT,ILEN,'Do you want to'//
     &		' save message? (Y/N with N as default): ')Y
	      IF (STREQ(INPUT(:1),'Y')) THENE
		 CALL LIB$RENAME_FILE('SYS$LOGIN:BULL.SCR',
     &				      'SYS$LOGIN:BULL.SAV') 
		 WRITE (6,'(A)') ' Message saved in SYS$LOGIN:BULL.SAV.'
	      END IF
	   END IF
	END IFk

900	IF (FILESPEC) CLOSE (UNIT=4)
	CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;*')]

	RETURN
	END



	SUBROUTINE ADD_SIGNATURE(FILEUNIT,FILENAME,FOLDER_NAME)
CC
C  SUBROUTINE ADD_SIGNATUREF
CE
C  FUNCTION: Adds signature to message being mailed/posted. 
CC
	IMPLICIT INTEGER (A-Z)D

	CHARACTER*(*) FOLDER_NAME

	CHARACTER*128 BULL_SIGNATUREO
	DATA BULL_SIGNATURE /'SYS$LOGIN:BULL_SIGNATURE.TXT'/H

	CHARACTER*256 INPUT

	IF (CLI$PRESENT('NOSIGNATURE')) RETURNU

	OPEN (UNIT=4,FILE=BULL_SIGNATURE,STATUS='OLD',READONLY,
     &		 SHARED,IOSTAT=IER,FORM='FORMATTED')

	IF (IER.NE.0) THEN 
	   OPEN (UNIT=4,FILE='BULL_SIGNATURE',STATUS='OLD',READONLY,	
     &		    SHARED,IOSTAT=IER,FORM='FORMATTED')h
	END IFI

	IF (IER.NE.0) THEND
	   OPEN (UNIT=4,FILE='MX_SIGNATURE',STATUS='OLD',READONLY, 
     &		    SHARED,IOSTAT=IER,FORM='FORMATTED')A
	END IF 

	IF (IER.NE.0) RETURN 

	IF (FILEUNIT.EQ.0) THEN
	   OPEN (UNIT=3,FILE=FILENAME,STATUS='OLD',ACCESS='APPEND',
     &		 IOSTAT=IER,FORM='FORMATTED')
	END IF 

	ICOUNT = 0R
	MATCH = .FALSE.
	DO WHILE (IER.EQ.0)
	   READ (4,'(A)',IOSTAT=IER) INPUTI
	   ILEN = TRIM(INPUT)
	   DO WHILE (.NOT.MATCH.AND.STREQ(INPUT(:6),'START ').AND.IER.EQ.0)
	      MATCH = STREQ(INPUT(7:ILEN),FOLDER_NAME(:TRIM(FOLDER_NAME)))E
	      READ (4,'(A)',IOSTAT=IER) INPUT
	      ILEN = TRIM(INPUT) 
	      IF (.NOT.MATCH) THENt
	         DO WHILE (.NOT.STREQ(INPUT(:ILEN),'END').AND.IER.EQ.0)
		    READ (4,'(A)',IOSTAT=IER) INPUTN
	            ILEN = TRIM(INPUT)	
		 END DO6
		 READ (4,'(A)',IOSTAT=IER) INPUT
	         ILEN = TRIM(INPUT)
	      END IFE
	   END DO
	   IF (IER.EQ.0) THEN
	      IF (MATCH.AND.STREQ(INPUT(:ILEN),'END')) THEN
	         MATCH = .FALSE.L
	      ELSE=
	         ICOUNT = ICOUNT + 1
	         IF (ICOUNT.EQ.1) WRITE (3,'(A)',IOSTAT=IER) ' 'I
	         WRITE (3,'(A)',IOSTAT=IER) INPUT(:ILEN) 
	      END IF 
	   END IF
	END DOi

	CLOSE (UNIT=4)
	IF (FILEUNIT.EQ.0) CLOSE (UNIT=3)

	RETURNz
	END




	LOGICAL FUNCTION STREQ(INPUT,INPUT1)D

	IMPLICIT INTEGER (A-Z)E

	CHARACTER*(*) INPUT,INPUT1B

	STREQ = .FALSE.

	IF (LEN(INPUT).NE.LEN(INPUT1)) RETURN

	DO I=1,LEN(INPUT)
	   DIFF = ABS(ICHAR(INPUT(I:I))-ICHAR(INPUT1(I:I)))
	   IF (DIFF.NE.0.AND.DIFF.NE.32) RETURN
	END DOB

	STREQ = .TRUE.U

	RETURN0
	END






	SUBROUTINE RESPOND_MAIL(FILE,SENDTO,SUBJECT,STATUS)
CS
C  SUBROUTINE RESPOND_MAIL
C 
C  FUNCTION: Sends mail to address.i
C
	IMPLICIT INTEGER (A-Z)_

	INCLUDE 'BULLUSER.INC'F

	INCLUDE 'BULLFOLDER.INC'L

	INCLUDE 'BULLDIR.INC'

	CHARACTER*(*) FILE,SENDTO,SUBJECT

	CHARACTER MAILER*128,INDESCRIP*(LINE_LENGTH)L

	PRIVS = FILE.NE.'SYS$LOGIN:BULL.SCR'W

	INDESCRIP = SUBJECT
	LENDES = TRIM(INDESCRIP))
	I = 1		! Must change all " to "" in SUBJECT field
	DO WHILE (I.LE.LENDES)S
	   IF (INDESCRIP(I:I).EQ.'"') THEN 
	      IF (LENDES.EQ.LINE_LENGTH) THEN
		 INDESCRIP(I:I) = '`'o
	      ELSEh
		 INDESCRIP = INDESCRIP(:I)//'"'D
     &				//INDESCRIP(I+1:)
		 I = I + 1
		 LENDES = LENDES + 1
	      END IFI
	   END IF
	   I = I + 1U
	END DOO

	LISTSERV = INDEX(FOLDER_DESCRIP,'LISTSERV').GT.0M
	VMSSERV = INDEX(FOLDER_DESCRIP,'VMSSERV').GT.0S

	IF (LISTSERV.OR.VMSSERV) CALL SETUSER(FOLDER_BBOARD)N
	IF (PRIVS) CALL DISABLE_PRIVS

	IF (LISTSERV) THENM
	   IF (SYS_TRNLNM('MX_NODE_NAME',MAILER)) THENS
	      REPLY_TO = .NOT.SYS_TRNLNM('MX_REPLY_TO',MAILER)2
	      IF (REPLY_TO) IER = LIB$SET_LOGICAL
     &			('MX_REPLY_TO',USERNAME(:TRIM(USERNAME)))
	   ELSE
	      REPLY_TO = .NOT.SYS_TRNLNM('PMDF_REPLY_TO',MAILER)U
	      IF (REPLY_TO) IER = LIB$SET_LOGICAL
     &			('PMDF_REPLY_TO',USERNAME(:TRIM(USERNAME)))
	   END IF
	END IF.

	LEN_SENDTO = TRIM(SENDTO)
	IF (SYS_TRNLNM('BULL_MAILER',MAILER)) THEN
	   IF (LISTSERV) THEN
	      IF (SYS_TRNLNM_SYSTEM('BULL_MAILER',MAILER)) THEN
	         CALL LIB$SPAWN('@'//MAILER(:TRIM(MAILER))//D
     &		    ' '//FILE(:TRIM(FILE))//' """'//SENDTO(:LEN_SENDTO)T
     &		    //'""" """'//INDESCRIP(:LENDES)//'""" ' 
     &		    //USERNAME(:TRIM(USERNAME)),,,,,,STATUS)
	      END IFT
	   ELSE
	      CALL LIB$SPAWN('@'//MAILER(:TRIM(MAILER))//
     &		 ' '//FILE(:TRIM(FILE))//' """'//SENDTO(:LEN_SENDTO)//
     &		 '""" """'//INDESCRIP(:LENDES)//'"""',,,,,,STATUS)
	   END IF
	ELSE
	   I = INDEX(SENDTO,'%""') + 3R
	   DO WHILE (I.LT.LEN_SENDTO-2)
	      IF (SENDTO(I:I+2).EQ.'"",'.OR.SENDTO(I:I+2).EQ.'%""') THENO
		 I = I + 3
	      ELSE IF (SENDTO(I:I+1).EQ.'""') THEN1
		 SENDTO(I:) = ''''//SENDTO(I+2:)
                 LEN_SENDTO = LEN_SENDTO - 1
              ELSE IF (SENDTO(I:I).EQ.'\'.AND.
     &                 SENDTO(I+1:I+1).NE.'\') THEN
		 SENDTO(I+1:) = '\'//SENDTO(I+1:)E
		 LEN_SENDTO = LEN_SENDTO + 1
		 I = I + 1
	      ELSE IF (SENDTO(I:I).EQ.''''.AND.
     &		       INDEX(SENDTO,'@').GT.I) THEN)
		 SENDTO(I:) = '\s'//SENDTO(I+1:)
		 LEN_SENDTO = LEN_SENDTO + 1
		 I = I + 2
	      END IFM
	      I = I + 1
	   END DO

           CALL SENDMAIL(FILE(:TRIM(FILE)),SENDTO,
     &                INDESCRIP(:LENDES),STATUS)
           IF (.NOT.STATUS) CALL SYS_GETMSG(STATUS)C
C 
C Use the following if you do not have VMS V5.3 or greater..
CT
C           CALL LIB$SPAWN('$MAIL SYS$LOGIN:BULL.SCR "'//1
C     &                SENDTO(:LEN_SENDTO)//
C     &       '" /SUBJECT="'//INDESCRIP(:LENDES)//'"',,,,,,STATUS)
	END IFN

	IF (PRIVS) CALL ENABLE_PRIVS 
	IF (LISTSERV.OR.VMSSERV) CALL SETUSER(USERNAME)
	IF (LISTSERV) THEN
	   IF (REPLY_TO) IER = LIB$DELETE_LOGICAL('PMDF_REPLY_TO') 
	   IF (REPLY_TO) IER = LIB$DELETE_LOGICAL('MX_REPLY_TO') 
	END IF 

	RETURN 
	END



	INTEGER FUNCTION CONFIRM_USER(USERNAME)
CO
C  FUNCTION CONFIRM_USER
CW
C  FUNCTION: Confirms that username is valid user.
CN
	IMPLICIT INTEGER (A-Z)

	CHARACTER*(*) USERNAME)

	CALL OPEN_SYSUAF_SHARED

	READ (8,KEY=USERNAME,IOSTAT=CONFIRM_USER)

	CALL CLOSE_SYSUAF

	RETURNY
	END





	SUBROUTINE REPLACE
CE
C  SUBROUTINE REPLACES
C=
C  FUNCTION: CHANGE command subroutine.B
CN
	IMPLICIT INTEGER (A - Z)	

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /EDIT/ EDIT_DEFAULT 

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /LAST_RECORD_WRITTEN/ OCOUNT

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC' 

	INCLUDE 'BULLFOLDER.INC'E

	CHARACTER INEXDATE*12,INEXTIME*12
	CHARACTER INDESCRIP*(INPUT_LENGTH),INFROM*(INPUT_LENGTH) 
	CHARACTER*4 ANSWERR

	CHARACTER DATE_SAVE*12,TIME_SAVE*12

	EXTERNAL CLI$_ABSENT,CLI$_NEGATED

	LOGICAL DOALL

	IF (REMOTE_SET.GE.3) THEN
	   WRITE (6,'('' Cannot CHANGE messages in this folder.'')')l
	   RETURN
	END IFL

CS
C  Get the bulletin number to be replaced.
CL

	ALL = CLI$PRESENT('ALL')D

	IER1 = CLI$GET_VALUE('NUMBER',BULL_PARAMETER,LEN_P)
	IF (IER1.EQ.%LOC(CLI$_ABSENT).AND..NOT.ALL) THEN_
	   IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been read
	      WRITE (6,1005)		! Tell user of the errore
	      RETURN			! and return
	   END IF
	   SBULL = BULL_POINT		! Replace the bulletin we are readinge
	   EBULL = SBULL 

	   CALL OPEN_BULLDIR_SHARED
	   CALL READDIR(BULL_POINT,IER)		! Get message directory entryS
	   CALL CLOSE_BULLDIR
	   IF (IER.NE.BULL_POINT+1) THEN	! Was message found?
	      WRITE(6,'('' ERROR: Specified message was not found.'')')
	      RETURNI
	   END IF
	ELSEC
	   CALL OPEN_BULLDIR_SHARED
	   CALL READDIR(0,IER)		! Get message directory entry
	   CALL CLOSE_BULLDIR
	   IF (NBULL.EQ.0) THEN		! Were messages found?
	      WRITE(6,'('' ERROR: No messages were found.'')') 
	      RETURN(
	   END IF

	   IF (IER1.NE.%LOC(CLI$_ABSENT)) THEN 
	      CALL GET_2_VALS(BULL_PARAMETER,LEN_P,SBULL,EBULL,IER1) 
	      IF (SBULL.LE.0.OR.IER1.NE.0) THEN
	         WRITE (6,'(A)') 
     &		  ' ERROR: Specified message number has incorrect format.'
	         RETURN
	      END IFi
	      ALL = .TRUE.'
	   ELSE IF (CLI$PRESENT('ALL')) THENL
	      SBULL = 1
	      EBULL = NBULL
	   END IF
	END IF)

	IF (CLI$PRESENT('SYSTEM')) THEN
	   IF (.NOT.SETPRV_PRIV()) THEN
	    WRITE (6,'(
     &	     '' ERROR: Not enough privileges to change to system.'')')A
	    RETURNF
	   ELSE IF (.NOT.BTEST(FOLDER_FLAG,2).AND.FOLDER_NUMBER.NE.0) THEN 
	    WRITE (6,'(
     &       '' ERROR: /SYSTEM cannot be set with selected folder.'')')R
	    RETURN0
	   END IF
	END IF6

	IF (CLI$PRESENT('SHUTDOWN')) THEN
	   IF (.NOT.SETPRV_PRIV()) THEN
	    WRITE (6,'(
     &	     '' ERROR: Not enough privileges to change to shutdown.'')')X
	    RETURND
	   ELSE IF (.NOT.BTEST(FOLDER_FLAG,2).AND.FOLDER_NUMBER.NE.0) THENF
	    WRITE (6,'(
     &      '' ERROR: /SHUTDOWN cannot be set with selected folder.'')')
	    RETURN 
	   ELSE IF (CLI$GET_VALUE('SHUTDOWN',BULL_PARAMETER).NE. 
     &		    %LOC(CLI$_ABSENT).AND.REMOTE_SET) THEN
	    WRITE (6,'('' ERROR: Shutdown node name not'',D
     &			    '' permitted for remote folder.'')')(
	    RETURN 
	   END IF
	END IF 

	IF (CLI$PRESENT('PERMANENT').AND.
     &		F_EXPIRE_LIMIT.GT.0.AND..NOT. ! Expiration limit present
     &		FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)) THEN
	   WRITE (6,'(e
     &	    '' ERROR: Not enough privileges to change to permanent.'')')t
	   RETURN
	END IF
C 
C  Check to see if specified bulletin is present, and if the userW
C  is permitted to replace the bulletin.
C 

	CALL OPEN_BULLDIR_SHARED

	SAME_OWNER = .TRUE.
	DO I=SBULL,EBULL'
	   CALL READDIR(I,IER)	! Get info for specified messages
	   IF (USERNAME.NE.FROM) SAME_OWNER = .FALSE. B
	END DO)
	CALL READDIR(SBULL,IER)

	CALL CLOSE_BULLDIRL

	IF (.NOT.SAME_OWNER) THEN	! If doesn't match owner of bulletin,
	   IF ((.NOT.SETPRV_PRIV().AND..NOT.FOLDER_SET).OR.	! Privileges or
     &	       (.NOT.FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)
     &		.AND.FOLDER_SET)) THEN				! folder owner?m
	      WRITE(6,1090)		! If not, then error out.F
	      RETURN(
	   ELSE
	      WRITE (6,1100)		! Make sure user wants to delete it
	      READ (5,'(A)',IOSTAT=IER) ANSWER	! Get his answer
	      CALL STR$UPCASE(ANSWER,ANSWER)	! Convert input to uppercase
	      IF (ANSWER(:1).NE.'Y') RETURN	! If not Yes, then exit
	   END IF
	END IFE

C 
C  If no switches were given, replace the full bulletinr
Cm

	DOALL = .FALSE.

	IF ((.NOT.CLI$PRESENT('EXPIRATION')).AND.
     &	   (.NOT.CLI$PRESENT('GENERAL')).AND.
     &	   (.NOT.CLI$PRESENT('SYSTEM')).AND. 
     &	   (.NOT.CLI$PRESENT('HEADER')).AND.D
     &	   (.NOT.CLI$PRESENT('SUBJECT')).AND.
     &	   (.NOT.CLI$PRESENT('TEXT')).AND. 
     &	   (.NOT.CLI$PRESENT('SHUTDOWN')).AND. 
     &	   (.NOT.CLI$PRESENT('PERMANENT'))) THEN 
	   DOALL = .TRUE.
	END IFA

	TEXT = CLI$PRESENT('TEXT').OR.CLI$PRESENT('EDIT')

	IF (SBULL.NE.EBULL.AND.(DOALL.OR.TEXT)) THEN.
	   WRITE (6,'('' ERROR: Cannot change text when replacing'',N
     &		      '' more than one messsage.'')')O
	   RETURN
	END IFA

	CALL DISABLE_CTRL			! Disable CTRL-Y & -C

	PERMANENT = .FALSE.
	IF (CLI$PRESENT('EXPIRATION').OR.DOALL) THENA
	   SYSTEM = 0
	   CALL GET_EXPIRED(INPUT,IER)E
	   PERMANENT = BTEST(SYSTEM,1)O
	   IF (.NOT.IER) GO TO 910(
	   INEXDATE = INPUT(:11)U
	   INEXTIME = INPUT(13:23)S
	END IF 

8	LENDES = 0
	IF (CLI$PRESENT('HEADER').OR.DOALL) THENT
	   WRITE(6,1050)			! Request header for bulletinO
	   READ(5,'(Q,A)',END=910,ERR=910) LENDES,INDESCRIP
	   IF (LENDES.EQ.0) GO TO 910		! If no header, don't add bull
	ELSE IF (CLI$PRESENT('SUBJECT')) THEN
	   IER = CLI$GET_VALUE('SUBJECT',INDESCRIP,LENDES)R
	END IFA

	IF (LENDES.GT.0) THEN
	   INDESCRIP = 'Subj: '//INDESCRIPh
	   LENDES = MIN(LENDES+6,LEN(INDESCRIP))I
	END IFE

	IF (SBULL.NE.EBULL) CALL OPEN_BULLDIR

	DO NUMBER=SBULL,EBULL
	 NUMBER_PARAM = NUMBERD
	 IF (SBULL.NE.EBULL) THEN
	   CALL READDIR(NUMBER_PARAM,IER)
	   IF (IER.NE.NUMBER_PARAM+1) THEN	! Couldn't find message
	      CALL CLOSE_BULLDIR
	      WRITE(6,'('' ERROR: Message '',I6,'' cannot be found.'')')P
     &			NUMBER_PARAMS
	      WRITE(6,'('' All messages up to that message were modified.'')')h
	      RETURNa
	   END IF
	 END IF

	 REC1 = 0

	 LENFROM = 0l

	 IF (LENDES.GT.0.OR.TEXT.OR.DOALL) THEN
	   OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,n
     &	     RECL=LINE_LENGTH,STATUS='SCRATCH',CARRIAGECONTROL='LIST')U

	   IF (IER.NE.0) THEN
	      CALL ERRSNS(IDUMMY,IER)
	      CALL SYS_GETMSG(IER)L
	      GO TO 910
	   END IF

	   CALL OPEN_BULLFIL_SHARED

	   REC1 = 1

	   ILEN = LINE_LENGTH + 1

	   CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)O
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN 
	      INFROM = INPUT(:ILEN)
	      LENFROM = ILENS
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   END IF
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THEND
	      IF (LENDES.EQ.0.AND..NOT.DOALL) THENC
		 INDESCRIP = INPUT(:ILEN)F
		 LENDES = ILEN
	      END IF 
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   END IF

	   DO WHILE (ILEN.GT.0)		! Copy bulletin into fileI
	      WRITE (3,'(A)') INPUT(:ILEN)
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   END DO

	   CALL CLOSE_BULLFIL

	   IF (TEXT.OR.DOALL) CLOSE(UNIT=3)
	 END IF

	 IF (TEXT.OR.DOALL) THENr
Cd
C  If file specified in REPLACE command, read file to obtain bulletin.
C  Else, read the bulletin from the terminal. 
C_
	 
	  ICOUNT = 0				! Line count for bulletin
	  LAST_NOBLANK = 0			! Last line with data 
	  REC1 = 1_

	  IER = CLI$GET_VALUE('FILESPEC',BULL_PARAMETER,LEN_P) 
	  IF (IER.NE.%LOC(CLI$_ABSENT).OR.	! If file param in ADD command
     &	    ((CLI$PRESENT('EDIT').OR.EDIT_DEFAULT).AND.	! or /EDIT specified 
     &       (CLI$PRESENT('EDIT').NE.%LOC(CLI$_NEGATED)))) THEN=

	   IF ((CLI$PRESENT('EDIT').OR.EDIT_DEFAULT).AND. ! If /EDIT specified
     &       (CLI$PRESENT('EDIT').NE.%LOC(CLI$_NEGATED))) THEN
	      IF (LEN_P.EQ.0) THEN		! If no file param specifiedl
		 IF (.NOT.CLI$PRESENT('NEW')) THEN
	            OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='NEW',A
     &		       RECL=LINE_LENGTH,
     &		       ERR=920,FORM='FORMATTED',CARRIAGECONTROL='LIST')a
	            CALL OPEN_BULLFIL_SHARED	! Prepare to copy message
		    ILEN = LINE_LENGTH + 1
		    CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)h
		    IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN 
		       CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
		    END IF
		    IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THEN_
		       CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
		    END IF
		    DO WHILE (ILEN.GT.0)	! Copy message into file'
		       WRITE (3,'(A)') INPUT(:ILEN)
		       CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
		    END DO
		    CALL CLOSE_BULLFIL
	            CLOSE (UNIT=3)		! Bulletin copy completed
		 END IFB
		 CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')
	      ELSE 
	         CALL DISABLE_PRIVS
		 CALL MAILEDIT(BULL_PARAMETER(:LEN_P),'SYS$LOGIN:BULL.SCR')
	      END IF.
	      IER = LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;-1')I
	      OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='OLD',
     &		 DISPOSE='DELETE',ERR=920,FORM='FORMATTED') 
	   ELSE IF (LEN_P.GT.0) THEN)
	      CALL DISABLE_PRIVS'
	      OPEN (UNIT=3,FILE=BULL_PARAMETER(:LEN_P),STATUS='OLD',S
     &		READONLY,SHARED,ERR=920,FORM='FORMATTED') ! Try opening the file
	   END IF

	   CALL ENABLE_PRIVS			! Reset SYSPRV privileges 

	   DO WHILE(1)				! Read until end of file to
	      READ (3,'(Q,A)',END=10) ILEN,INPUT	! get record count
	      IF (ILEN.GT.LINE_LENGTH) GO TO 950 
	      CALL STR$TRIM(INPUT,INPUT,ILEN)
	      IF (ILEN.GT.0) THEN		! If good input line entered
		 ICOUNT = ICOUNT + ILEN + 1	! Increment record count
		 LAST_NOBLANK = ICOUNT
	      ELSE IF (ILEN.EQ.0) THENR
		 IF (ICOUNT.GT.0) THEN
		    ICOUNT = ICOUNT + 2		! COPY_BULL writes a line with 
		 ELSE				! 1 space for a blank line.
		    REC1 = REC1 + 1p
		 END IF'
	      END IFR
	   END DO
	  ELSE					! If no input file
	   OPEN (UNIT=3,STATUS='NEW',FILE='SYS$LOGIN:BULL.SCR',ERR=920,
     &		 DISPOSE='DELETE',FORM='FORMATTED',RECL=LINE_LENGTH,
     &		 CARRIAGECONTROL='LIST')	! Scratch file to save bulletin
	   WRITE (6,1000)		! Request bulletin input from terminal
	   ILEN = LINE_LENGTH			! Length of input lineI
	   DO WHILE (ILEN.GE.0)			! Input until no more input
	      CALL GET_LINE(INPUT,ILEN)		! Get input line
	      IF (ILEN.GT.LINE_LENGTH) THEN	! Line too long.F
		 WRITE(6,'('' ERROR: Input line length > '',I,
     &			''. Reinput::'')') LINE_LENGTH 
	      ELSE IF (ILEN.GT.0) THEN		! If good input line entered	
		 ICOUNT = ICOUNT + 1 + ILEN	! Increment character count
		 WRITE(3,'(A)') INPUT(:ILEN)	! Save line in scratch file
		 LAST_NOBLANK = ICOUNT
	      ELSE IF (ILEN.EQ.0.AND.ICOUNT.GT.0) THEN
		 WRITE(3,'(A)') INPUT(:ILEN)	! Save line in scratch file
		 ICOUNT = ICOUNT + 2		! COPY_BULL writes a line with
	      END IF				! 1 space for a blank line.
	   END DO
	   IF (ILEN.EQ.-1) GO TO 910		! CTRL_C entered, error out
10	   ICOUNT = LAST_NOBLANKY
	   IF (ICOUNT.EQ.0) GO TO 910		! No lines entered, error outD
	  ENDIF

	 END IF

C 
C  Add bulletin to bulletin file and directory entry for to directory file.I
C

	 DATE_SAVE = DATE
	 TIME_SAVE = TIME
	 INPUT = DESCRIP 

	 IF (SBULL.EQ.EBULL) THEN
	  CALL OPEN_BULLDIR			! Prepare to add dir entryE
	  CALL READDIR(NUMBER_PARAM,IER)	! Get info for message

	  IF (IER.NE.NUMBER_PARAM+1.OR.DATE.NE.DATE_SAVE.OR.
     &	     TIME.NE.TIME_SAVE.OR.INPUT.NE.DESCRIP) THEN 
				! If message disappeared, try to find it.-
	   IF (IER.NE.NUMBER_PARAM+1) DATE = ' ''
	   NUMBER_PARAM = 0
	   IER = 1_
	   DO WHILE (IER.EQ.NUMBER_PARAM+1.AND.
     &	    (DATE.NE.DATE_SAVE.OR.TIME.NE.TIME_SAVE.OR.DESCRIP.NE.INPUT))
	      NUMBER_PARAM = NUMBER_PARAM + 1
	      CALL READDIR(NUMBER_PARAM,IER))
	   END DO

	   IF (IER.NE.NUMBER_PARAM+1) THEN	! Couldn't find messageO
	      CALL CLOSE_BULLDIRN
	      CLOSE (UNIT=3,STATUS='SAVE')
	      WRITE(6,'('' ERROR: Message has been deleted'',
     &			'' by another user.'')')"
	      IF (DOALL.OR.TEXT) THEN
		 WRITE (6,'('' New text has been saved in'',
     &				'' SYS$LOGIN:BULL.SCR.'')')v
	      END IF/
	      GO TO 100
	   END IF
	  END IF:
	 END IF

	 CALL READDIR(0,IER)			! Get directory header

	 IF (REC1.GT.0) THEN			! If text has been replaced_

	   CALL OPEN_BULLFIL			! Prepare to add bulletinO

	   BLOCK = NBLOCK + 1
	   BLOCK_SAVE = BLOCK
	   NEMPTY = NEMPTY + LENGTH

	   OBLOCK = BLOCK
	   IF (LENFROM.GT.0) THEN
	      CALL STORE_BULL(LENFROM,INFROM(:LENFROM),OBLOCK),
	   END IF
	   IF (LENDES.GT.0) THEN0
	      CALL STORE_BULL(LENDES,INDESCRIP(:LENDES),OBLOCK)
	   END IF
	   REWIND (UNIT=3)E
	   CALL COPY_BULL(3,REC1,OBLOCK,IER)	! Add the new bulletin
	   IF (IER.NE.0) THEN		! Error in creating bulletin
	      WRITE (6,'(A)') ' ERROR: Unable to replace message.'N
	      CALL CLOSE_BULLFIL 
	      CALL CLOSE_BULLDIRA
	      CLOSE (UNIT=3):
	      GO TO 100
	   END IF

	   LENGTH_SAVE = OCOUNT - BLOCK + 1
	   NBLOCK = NBLOCK + LENGTH_SAVE 

	   IF (.NOT.REMOTE_SET) CALL WRITEDIR(0,IER)I

	   CALL CLOSE_BULLFIL

	   IF (.NOT.REMOTE_SET) THENT
	    CALL READDIR(NUMBER_PARAM,IER)	! Get directory entry(
	    LENGTH = LENGTH_SAVE		! Update size
	    BLOCK = BLOCK_SAVE.
	    CALL WRITEDIR(NUMBER_PARAM,IER)	! Write new directory entry
	   END IF
	 ELSE
	   CALL READDIR(NUMBER_PARAM,IER)
	 END IF

	 IF (.NOT.REMOTE_SET) THENN

	   IF (LENDES.GT.0.OR.DOALL) THEN
	      DESCRIP=INDESCRIP(7:62)		! Update description headerN
	   END IF
	   CALL UPDATE_DIR_HEADER((CLI$PRESENT('EXPIRATION').OR.DOALL).AND.
     &		.NOT.PERMANENT,CLI$PRESENT('PERMANENT').OR.PERMANENT,I
     &		CLI$PRESENT('SHUTDOWN'),INEXDATE,INEXTIME)
	   IF (CLI$PRESENT('SYSTEM')) THEN 
	      SYSTEM = IBSET(SYSTEM,0) 
	   ELSE IF (CLI$PRESENT('GENERAL')) THENT
	      SYSTEM = IBCLR(SYSTEM,0)O
	   END IF
	   CALL WRITEDIR(NUMBER_PARAM,IER) 
	 ELSE
	   MSGTYPE = 0T
	   IF (CLI$PRESENT('SYSTEM').OR.S
     &		(BTEST(SYSTEM,0).AND..NOT.CLI$PRESENT('GENERAL'))) THEN 
	      MSGTYPE = IBSET(MSGTYPE,0)N
	   END IF
	   IF (CLI$PRESENT('PERMANENT').OR.PERMANENT) THENO
	      MSGTYPE = IBSET(MSGTYPE,1)'
	   ELSE IF (CLI$PRESENT('SHUTDOWN')) THEN
	      MSGTYPE = IBSET(MSGTYPE,2)E
	   ELSE IF ((CLI$PRESENT('EXPIRATION').OR.DOALL)I
     &		    .AND..NOT.PERMANENT) THEN
	      MSGTYPE = IBSET(MSGTYPE,3)
	   END IF
	   IF (LENDES.EQ.0.AND..NOT.DOALL) INDESCRIP(7:) = DESCRIP1
	   IF (CLI$PRESENT('EXPIRATION').OR.DOALL) THEN
	      EXDATE = INEXDATE
	      EXTIME = INEXTIME
	   END IF
	   WRITE (REMOTE_UNIT,'(7A)',IOSTAT=IER)
     &      10,DESCRIP,NUMBER_PARAM,INDESCRIP(7:62),MSGTYPE,
     &	    EXDATE,EXTIME
	   IF (IER.EQ.0) THEN
	      READ(REMOTE_UNIT,'(Q,A)',IOSTAT=IER) I,FOLDER1_COMl
	   END IF
	   IF (IER.EQ.0) THEN
	      IF (I.NE.LEN(FOLDER1_COM)) THEN
		 WRITE (6,'(1X,A)') FOLDER1_COM(:I)"
	      END IFt
	   ELSE
	      CALL DISCONNECT_REMOTEO
	   END IF
	 END IF
	END DO 

	CALL CLOSE_BULLDIR		! Totally finished with replace

	CLOSE (UNIT=3)L

100	CALL ENABLE_CTRL		! Enable CTRL-Y & -C
	RETURNF

910	WRITE(6,1010)U
	CLOSE (UNIT=3,ERR=100) 
	GOTO 100O

920	WRITE(6,1020) 
	CALL ENABLE_PRIVS	! Reset SYSPRV privileges
	GOTO 100R

950	WRITE (6,1030) LINE_LENGTH
	CLOSE (UNIT=3)T
	GO TO 100

1000	FORMAT (' Enter message: End with ctrl-z, cancel with ctrl-c') 
1005	FORMAT (' ERROR: You are not reading any message.')
1010	FORMAT (' No message was replaced.'),
1015	FORMAT (' ERROR: Specified message was not found.')
1020	FORMAT (' ERROR: Unable to open specified file.')
1030	FORMAT (' ERROR: Line length in file exceeds '',I,'' characters.') 
1050	FORMAT (' Enter description header.')
1090	FORMAT(' ERROR: Specified message is not owned by you.') 
1100	FORMAT(' Message(s) is not owned by you.',E
     &	       ' Are you sure you want to replace it? ',$)L
2020	FORMAT(1X,A) 

	END



	SUBROUTINE UPDATE_DIR_HEADER(EXPIRE,PERM,SHUT,INEXDATE,INEXTIME)I

	IMPLICIT INTEGER (A-Z)T

	INCLUDE 'BULLDIR.INC'

	EXTERNAL CLI$_ABSENTR

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*256 INCMD

	CHARACTER TODAY*24,INEXDATE*12,INEXTIME*12-

	IF (EXPIRE) THENc
	   SYSTEM = IBCLR(SYSTEM,1)
	   SYSTEM = IBCLR(SYSTEM,2)
	   EXDATE=INEXDATE			! Update expiration date
	   EXTIME=INEXTIME 
	   DIFF = COMPARE_DATE(EXDATE,NEWEST_EXDATE)	! Compare expiration
	   IF (DIFF.EQ.0) DIFF = COMPARE_TIME(EXTIME,NEWEST_EXTIME)
	   IF (DIFF.LT.0) THEN			! If it's oldest expiration bull
	      NEWEST_EXDATE = EXDATE		! Update the header inR
	      NEWEST_EXTIME = EXTIME		! the directory file0
	      CALL WRITEDIR(0,IER)n
	   END IF
	ELSE IF (PERM.AND.(.NOT.BTEST(SYSTEM,1))) THEN 
	   IF (BTEST(SYSTEM,2)) THENa
	      SYSTEM = IBCLR(SYSTEM,2) 
	      SHUTDOWN = SHUTDOWN - 1
	      CALL WRITEDIR(0,IER)O
	   END IF
	   SYSTEM = IBSET(SYSTEM,1)
	   EXDATE = '5-NOV-2000'I
	   EXTIME = '00:00:00.00'
	ELSE IF (SHUT.AND.(.NOT.BTEST(SYSTEM,2))) THEN3
	   SYSTEM = IBSET(SYSTEM,2)
	   SYSTEM = IBCLR(SYSTEM,1)
	   EXDATE = '5-NOV-2000'E
	   NODE_AREA = 0N
	   IF (INCMD(:4).EQ.'REPL') THENE
	      IF (CLI$GET_VALUE('SHUTDOWN',NODE_NAME)
     &		    .NE.%LOC(CLI$_ABSENT)) THENC
		 CALL GET_NODE_NUMBER_OTHER(NODE_NUMBER,NODE_AREA,NODE_NAME)
	         IF (NODE_AREA.EQ.0) THEN
		    WRITE (6,'('' ERROR: Shutdown node name ignored.'',U
     &		               '' Invalid node name specified.'')').
		 END IF 
	      END IF0
	   END IF
	   IF (NODE_AREA.EQ.0) CALL GET_NODE_NUMBER(NODE_NUMBER,NODE_AREA) 
	   WRITE (EXTIME,'(I4)') NODE_NUMBER 
	   WRITE (EXTIME(7:),'(I4)') NODE_AREA	
	   DO I=1,11A
	      IF (EXTIME(I:I).EQ.' ') EXTIME(I:I) = '0'
	   END DO
	   EXTIME = EXTIME(1:2)//':'//EXTIME(3:4)//':'//N
     &		    EXTIME(7:8)//'.'//EXTIME(9:10)
	   SHUTDOWN = SHUTDOWN + 1T
	   CALL SYS$ASCTIM(,TODAY,,)		! Get the present time,
	   SHUTDOWN_DATE = TODAY(:11)
	   SHUTDOWN_TIME = TODAY(13:)
	   CALL WRITEDIR(0,IER)
	END IF)

	RETURNE
	END




	SUBROUTINE SEARCH(READ_COUNT)
C
C  SUBROUTINE SEARCH
CN
C  FUNCTION: Search for bulletin with specified string
CI
	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLFOLDER.INC'	

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*256 INCMD

	CHARACTER*132 SEARCH_STRING
	DIMENSION SEARCH_LEN(10) 

	EXTERNAL CLI$_ABSENTg

	NFOLDER = 1

	IF (CLI$PRESENT('SELECT_FOLDER')) THENR
	   CALL INIT_QUEUE(SCRATCH_F1,FOLDER1_NAME)
	   SCRATCH_F = SCRATCH_F1
	   NFOLDER = 0I
	END IFX

	DO WHILE (CLI$GET_VALUE('SELECT_FOLDER',FOLDER1_NAME)
     &	    .NE.%LOC(CLI$_ABSENT))		! Get the specified folders
	   IF (TRIM(FOLDER1_NAME).EQ.0) FOLDER1_NAME = FOLDER_NAMEI
	   NFOLDER = NFOLDER + 1N
	   CALL WRITE_QUEUE(%VAL(SCRATCH_F),SCRATCH_F,FOLDER1_NAME)
	END DOR

	IF (CLI$PRESENT('SELECT_FOLDER')) SCRATCH_F = SCRATCH_F1T

	START_BULL = BULL_POINT

	IF (CLI$PRESENT('START')) THEN		! Starting message specified=
	   IF (CLI$GET_VALUE('START',BULL_PARAMETER,LEN_P)) THEN
	      DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER) START_BULLA
	   END IF
	   IF (.NOT.CLI$PRESENT('REPLY')) START_BULL = START_BULL - 1
	END IFN

	IF (CLI$PRESENT('NOREPLIES')) THENR
	   SEARCH_STRING = 'RE:'O
	   SEARCH_LEN(1) = 3L
	   SEARCH_NUM = 1
	ELSE IF (CLI$PRESENT('REPLY')) THEN
	   SEARCH_STRING = ' '.
	   SEARCH_NUM = 1
	ELSE IF (CLI$PRESENT('SEARCH_STRING')) THEN
	   SEARCH_NUM = 1
	   J = 1S
	   DO WHILE (CLI$GET_VALUE('SEARCH_STRING',SEARCH_STRING(J:),
     &	    SEARCH_LEN(SEARCH_NUM)).NE.%LOC(CLI$_ABSENT))
	      J = J + SEARCH_LEN(SEARCH_NUM)O
	      SEARCH_NUM = SEARCH_NUM + 1
	   END DO
           IF (SEARCH_NUM.GT.1) SEARCH_NUM = SEARCH_NUM - 1 
	ELSET
	   SEARCH_STRING = ' 'E
	END IF 

	MATCH_MODE = 00
	IF (CLI$PRESENT('MATCH')) THEN 
	   CALL CLI$GET_VALUE('MATCH',BULL_PARAMETER,LEN_P)
           IF (BULL_PARAMETER(:LEN_P).EQ.'AND') MATCH_MODE = 1
           IF (BULL_PARAMETER(:LEN_P).EQ.'XOR') MATCH_MODE = 2
	END IFA

	IF (NFOLDER.GT.0) FOUND = 0

	DO WHILE (NFOLDER.GT.0.AND.FOUND.LE.0)4
	   IF (.NOT.CLI$PRESENT('SELECT_FOLDER').OR.R
     &		SCRATCH_F.NE.SCRATCH_F1)  
     &	      CALL GET_SEARCH(FOUND,SEARCH_STRING,SEARCH_NUM,
     &		SEARCH_LEN,MATCH_MODE,START_BULL,I
     &		CLI$PRESENT('REVERSE'),CLI$PRESENT('SUBJECT')U
     &	        .OR.CLI$PRESENT('NOREPLIES'),
     &		CLI$PRESENT('REPLY'),.TRUE.,CLI$PRESENT('START'),D
     &		CLI$PRESENT('FROM'),CLI$PRESENT('NEGATED')
     &	        .OR.CLI$PRESENT('NOREPLIES'),MATCH_MODE)S
	   IF (FOUND.EQ.-1) THENL
	      NFOLDER = 0
	   ELSE IF (FOUND.LE.0) THENP
	      IF (.NOT.CLI$PRESENT('SELECT_FOLDER').OR.
     &		   SCRATCH_F.NE.SCRATCH_F1) NFOLDER = NFOLDER - 1C
	      IF (NFOLDER.GT.0) THENT
	         CALL READ_QUEUE(%VAL(SCRATCH_F),SCRATCH_F,FOLDER1_NAME).
		 OLD_FOLDER_NUMBER = FOLDER_NUMBER
	         FOLDER_NUMBER = -1
		 IER = 0
		 DO WHILE (.NOT.IER.AND.NFOLDER.GT.0)T
		    FOLDER1 = FOLDER1_NAME
	            CALL SELECT_FOLDER(.FALSE.,IER)
		    IF (.NOT.IER) THEN
		       FOLDER_NUMBER = OLD_FOLDER_NUMBER
		       WRITE (6,'('' ERROR: Cannot find folder '',A,
     &			  ''.'')') FOLDER1_NAME(:TRIM(FOLDER1_NAME)) 
	               CALL GET_INPUT_PROMPT(FOLDER1_NAME,ILEN,
     &		       'Type new folder name or hit RETURN to continue: ')
	               IF (ILEN.LE.0.AND.NFOLDER.GT.0) THEN
		          NFOLDER = NFOLDER - 1I
	                  CALL READ_QUEUE(%VAL(SCRATCH_F),SCRATCH_F,Y
     &				          FOLDER1_NAME)E
		       END IFT
		    END IF
		 END DOE
	      END IF2
	   END IF
	END DO)

	IF (FOUND.GT.0) THEN 
	   BULL_POINT = FOUND - 1
	   CALL READ_MSG(READ_COUNT,BULL_POINT+1) ! Read next bulletin(
	ELSE IF (FOUND.EQ.0) THEN
  	   WRITE (6,'('' No messages found with given search string.'')')
	ELSE IF (FOUND.EQ.-2) THEN 
	   WRITE (6,'('' ERROR: No more messages.'')')I
	END IFN

	RETURNA
	END




	SUBROUTINE GET_SEARCH(FOUND,SEARCH_STRING,SEARCH_NUM,SEARCH_LEN,M
     &	        MATCH_INPUT,START_BULL,REVERSE,SUBJECT,REPLY,FILES,START,
     &		FROM_SEARCH,NEGATE)I
CI
C  SUBROUTINE GET_SEARCH
CD
C  FUNCTION: Search for bulletin with specified string
C
	IMPLICIT INTEGER (A - Z)M

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC' 

	COMMON /POINT/ BULL_POINT

	COMMON /TAGS/ BULL_TAG,READ_TAG,BULL_NEWS_TAG

	COMMON /CTRLC_FLAG/ FLAG/

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	CHARACTER*(*) SEARCH_STRING

	DIMENSION SEARCH_LEN(1)
	DIMENSION MATCH_FOUND(10)

	CHARACTER*132 SAVE_STRING
	DATA SAVE_STRING/' '/

	COMMON /NEWGROUP/ NEWGROUP=

	COMMON /NEXT/ NEXT(
	LOGICAL NEXTQ

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*256 INCMD

	CHARACTER*4 SAVECMD  

	CHARACTER*56 DESCRIP1

	FOUND = -1N

	CALL DISABLE_CTRL

	CALL DECLARE_CTRLC_ASTT

	IF (TRIM(SEARCH_STRING).EQ.0) THEN:
	   IER1 = .FALSE.
	ELSEL
	   IER1 = .TRUE.D
	END IF	
	=
	IF (.NOT.IER1.AND..NOT.REPLY.AND.
     &      (SUBJECT.OR.SEARCH_MODE.NE.1)) THENI
						! If no search string enteredE
	   SEARCH_STRING = SAVE_STRING		! use saved search string
	   IF (TRIM(SAVE_STRING).EQ.0) THEN
	      WRITE (6,'('' No search string present.'')')L
	      CALL CANCEL_CTRLC_AST
	      CALL ENABLE_CTRLL
	      RETURN(
	   END IF
	   IF (STEP_BULL.EQ.-1) START_BULL = START_BULL - 2
	ELSE IF (.NOT.IER1.AND.SEARCH_MODE.EQ.1.AND..NOT.REPLY) THENb
	   SEARCH_STRING = SAVE_STRING		! use saved search string
	   IF (STEP_BULL.EQ.-1) START_BULL = START_BULL - 2

	END IFO

	IF (FILES) CALL OPEN_BULLDIR_SHARED

	CALL READDIR(0,IER)

	OLD_SEARCH_MODE = SEARCH_MODE
	OLD_MATCH_MODE = MATCH_MODE

	IF (IER1) THEN				! If string entered
	   IF (SUBJECT) THENI
	      SEARCH_MODE = 3
	   ELSE IF (FROM_SEARCH) THEN    
	      SEARCH_MODE = 4
	   ELSE
	      SEARCH_MODE = 2
	   END IF
	   NEGATED = NEGATE
	   MATCH_MODE = MATCH_INPUT
	ELSE IF (SUBJECT.AND.SEARCH_MODE.NE.3) THEN
	   SEARCH_MODE = 3$
	   NEGATED = NEGATE
	   MATCH_MODE = MATCH_INPUT
	ELSE IF (FROM_SEARCH.AND.SEARCH_MODE.NE.4) THEN
	   SEARCH_MODE = 4 
	   NEGATED = NEGATE
	   MATCH_MODE = MATCH_INPUT
	ELSE IF (REPLY) THENR
	   NEGATED = NEGATE
	   MATCH_MODE = MATCH_INPUT
	   CALL READDIR(START_BULL,IER)
	   IF (START_BULL+1.NE.IER) THEN	
	      WRITE (6,'('' ERROR: No message being read.'')')
	      IF (FILES) CALL CLOSE_BULLDIR
	      CALL CANCEL_CTRLC_AST
	      CALL ENABLE_CTRLm
	      RETURN 
	   ELSE
	      SEARCH_MODE = 1
	      SEARCH_STRING = DESCRIP
	      IF (STREQ(DESCRIP(:4),'RE: ')) SEARCH_STRING = DESCRIP(5:)E
	      SEARCH_LEN(1) = TRIM(SAVE_STRING)
	      IF (REVERSE) START_BULL = START_BULL - 2g
	   END IF
	END IF 

	SAVE_STRING = SEARCH_STRING

	CALL STR$UPCASE(SEARCH_STRING,SEARCH_STRING)	! Make upper case

	IF (IER1.OR.SEARCH_MODE.NE.OLD_SEARCH_MODE.OR.3
     &	    MATCH_MODE.NE.OLD_MATCH_MODE.OR.REVERSE.OR.REPLY) THENI
	   IF (.NOT.START.AND.SEARCH_MODE.NE.1) THEN  S
	      START_BULL = 0	! If starting message not specified, use first
	      IF (REVERSE) START_BULL = NBULL - 1  ! or last$
	   END IF
	   IF (REVERSE) THENU
	      START_BULL = MIN(START_BULL,NBULL-1)
	      END_BULL = 1E
	      STEP_BULL = -1M
	   ELSE
	      END_BULL = NBULLT
	      STEP_BULL = 1
	   END IF
	END IFE

	IF ((START_BULL+1.GT.NBULL.AND.STEP_BULL.EQ.1).OR. 
     &	    (START_BULL+1.EQ.0)) THEN
	   FOUND = -2
	   IF (FILES) CALL CLOSE_BULLDIR 
	   CALL CANCEL_CTRLC_AST 
	   CALL ENABLE_CTRL
	   RETURN
	END IFR

	IF (FILES) CALL OPEN_BULLFIL_SHARED

	NEWGROUP = .TRUE.
	NEXT = STEP_BULL.EQ.1
	SAVECMD = INCMD(:4)
	IF (STEP_BULL.EQ.-1) INCMD(:4) = 'BACK'

	SAVE_BULL_SEARCH = 0 
	BULL_SEARCH = START_BULL+1g
	DO WHILE ((STEP_BULL.EQ.1.AND.BULL_SEARCH.LE.END_BULL).OR.m
     &		(STEP_BULL.EQ.-1.AND.BULL_SEARCH.GE.MAX(1,F_START)))
	   CALL READDIR(BULL_SEARCH,IER)	! Get bulletin directory entry
	   IF (READ_TAG) THEN
	      IF (STEP_BULL.EQ.-1) THEN
		 CALL GET_THIS_TAG(FOLDER_NUMBER,IER,BULL_SEARCH,DUMMY) 
	 	 IF (IER.NE.0) THEN
		    CALL GET_PREVIOUS_TAG(FOLDER_NUMBER,IER,
     &			BULL_SEARCH,DUMMY)U
		 END IF=
	      ELSE	
		 CALL GET_THIS_OR_NEXT_TAG(FOLDER_NUMBER,IER,T
     &			BULL_SEARCH,DUMMY)'
	      END IFh
	      IF (IER.EQ.0) THEN 
		 IER = BULL_SEARCH + 1
	      ELSEt
		 GO TO 800
	      END IFN
	   END IF
	   IF (REMOTE_SET.EQ.3.AND.SAVE_BULL_SEARCH.EQ.BULL_SEARCH) GO TO 800
	   SAVE_BULL_SEARCH = BULL_SEARCH
	   IF (IER.EQ.BULL_SEARCH+1.AND.SEARCH_MODE.NE.2) THENH
	      IF (SEARCH_MODE.EQ.4) THEN'
	         CALL STR$UPCASE(DESCRIP1,FROM)
	      ELSEt
	         CALL STR$UPCASE(DESCRIP1,DESCRIP).
	      END IFo
	      IF ((SEARCH_MODE.GE.3.AND.MATCH(DESCRIP1,SEARCH_STRING,
     &		  SEARCH_LEN,SEARCH_NUM,MATCH_MODE)).OR.
     &		  (SEARCH_MODE.EQ.1.AND.(DESCRIP1.EQ.SEARCH_STRING.OR.
     &		  (INDEX(SEARCH_STRING,DESCRIP1(5:)).EQ.1.AND.
     &		   STREQ(DESCRIP1(:4),'RE: '))))) THEN
                 IF (.NOT.NEGATED) THENa
		    FOUND = BULL_SEARCH	
		    GO TO 900b
	         END IF   O
	      ELSE IF (FLAG.EQ.1) THEN!
		 WRITE (6,'('' Search aborted.'')')T
		 GO TO 900
              ELSE IF (NEGATED) THEN o
                 FOUND = BULL_SEARCH
                 GO TO 900
	      END IFi
	   END IF
	   IF (IER.EQ.BULL_SEARCH+1.AND.SEARCH_MODE.EQ.2) THEN 
	      IF (REMOTE_SET) THEN 
		 CALL REMOTE_READ_MESSAGE(BULL_SEARCH,IER)
	         IF (IER.GT.0) THEN
	            CALL DISCONNECT_REMOTEI
		    GO TO 900R
	         ELSE
	            CALL GET_REMOTE_MESSAGE(IER).
		    IF (IER.GT.0) GO TO 900 
	         END IF
	      END IF.
	      ILEN = LINE_LENGTH + 1e
              MATCHES = 0t
	      DO I=1,SEARCH_NUM
		 MATCH_FOUND(I) = .FALSE.B
	      END DO 
	      DO WHILE (ILEN.GT.0).
	         CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)R
	         CALL STR$UPCASE(INPUT,INPUT)	! Make upper case
		 J = 1
		 DO I=1,SEARCH_NUM
		    IF (.NOT.MATCH_FOUND(I)) THENN
		       MATCH_FOUND(I) = INDEX(INPUT,
     &				SEARCH_STRING(J:J+SEARCH_LEN(I)-1)).GT.0
		       IF (MATCH_FOUND(I)) MATCHES = MATCHES + 1
		    END IF
		    J = J + SEARCH_LEN(I)'
	         END DO
		 IF ((MATCHES.GT.0.AND.MATCH_MODE.EQ.0).OR. 
     &	             (MATCHES.EQ.SEARCH_NUM.AND.MATCH_MODE.EQ.1)) THENL
		    FOUND = BULL_SEARCH 
		    IF (.NOT.NEGATED) GO TO 900 
		 ELSE IF (FLAG.EQ.1) THEN 
		    WRITE (6,'('' Search aborted.'')')
		    GO TO 900G
		 END IF	
	      END DOe
	      IF (MATCHES.EQ.1.AND.MATCH_MODE.EQ.2) THENo
		 FOUND = BULL_SEARCH
		 IF (.NOT.NEGATED) GO TO 900
              END IF
              IF (NEGATED) THEN 
                 IF (FOUND.EQ.-1) THEN
                    FOUND = BULL_SEARCH,
 		    GO TO 900
		 ELSEF
		    FOUND = -1
	         END IF
	      END IF,
	   END IF
	   BULL_SEARCH = BULL_SEARCH + STEP_BULLT
	END DO 

800	FOUND = 0R

900	IF (FOUND.LE.0.AND.REMOTE_SET.EQ.3) THEN
	   NEXT = .FALSE.
	   CALL READDIR(BULL_POINT,IER)
	   CALL REMOTE_READ_MESSAGE(BULL_POINT,IER)
	   IF (IER.GT.0) THEN
	      CALL DISCONNECT_REMOTEU
	   ELSE
	      CALL GET_REMOTE_MESSAGE(IER)0
	   END IF
	END IFE
	IF (FILES) CALL CLOSE_BULLFIL		! End of bulletin file readS
	IF (FILES) CALL CLOSE_BULLDIR
	CALL CANCEL_CTRLC_AST
	CALL ENABLE_CTRLL

	NEXT = .FALSE.T
	IF (STEP_BULL.EQ.-1) INCMD(:4) = SAVECMDR

	RETURN	
	END
 e



        LOGICAL FUNCTION MATCH(INPUT,SEARCH_STRING,SEARCH_LEN,
     &                         SEARCH_NUM,MATCH_MODE)e

        IMPLICIT INTEGER (A - Z)

	DIMENSION SEARCH_LEN(1)

        CHARACTER*(*) INPUT,SEARCH_STRINGE
 T
        OLD_MATCH = .FALSE.R

	J = 1

	DO I=1,SEARCH_NUM
	   MATCH = INDEX(INPUT,SEARCH_STRING(J:J+SEARCH_LEN(I)-1)).GT.0
           IF (MATCH.AND.MATCH_MODE.EQ.0) RETURN
           IF (.NOT.MATCH.AND.MATCH_MODE.EQ.1) RETURN'
           IF (OLD_MATCH.AND.MATCH.AND.MATCH_MODE.EQ.2) THEN
              MATCH = .FALSE.S
	      RETURN 
           END IF(
	   J = J + SEARCH_LEN(I)I
	END DO'

	RETURNT
	END



	SUBROUTINE UNDELETE
CO
C  SUBROUTINE UNDELETE
CE
C  FUNCTION: Undeletes deleted message.P
C 
	IMPLICIT INTEGER (A - Z)S

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'G

	INCLUDE 'BULLFOLDER.INC'E

	EXTERNAL CLI$_ABSENT 

	IF (REMOTE_SET.EQ.3) THEN
	   WRITE (6,'('' Cannot UNDELETE messages in this folder.'')') 
	   RETURN
	END IF)
CE
C  Get the bulletin number to be undeleted.N
CF

	IER = CLI$GET_VALUE('BULLETIN_NUMBER',BULL_PARAMETER,LEN_P)
	IF (IER.NE.%LOC(CLI$_ABSENT)) THEN	! Was bulletin specified? 
	   DECODE(LEN_P,5,BULL_PARAMETER,ERR=920) BULL_DELETE	! Yes
5	   FORMAT(I<LEN_P>)T
	ELSE IF (BULL_POINT.EQ.0) THEN	! No.  Have we just read a bulletin?
	   GO TO 910			! No, then error.E
	ELSE 
	   BULL_DELETE = BULL_POINT	! Delete the file we are readingl
	END IF 

	IF (BULL_DELETE.LE.0) GO TO 920

CN
C  Check to see if specified bulletin is present, and if the userN
C  is permitted to delete the bulletin.)
CE

	CALL OPEN_BULLDIR

	CALL READDIR(BULL_DELETE,IER)	! Get info for specified bulletin

	IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?L
	   WRITE(6,1030)	! If not, then error out
	   GOTO 100
	END IFI

	IF (USERNAME.NE.FROM) THEN	! If doesn't match owner of bulletin,S
	   IF ((.NOT.SETPRV_PRIV().AND..NOT.FOLDER_SET).OR.	! Privileges or
     &	       (.NOT.FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)
     &		.AND.FOLDER_SET)) THEN				! folder owner? 
	      WRITE(6,1040)		! Then error out.,
	      GO TO 100
	   ELSE
	      CALL READDIR(BULL_DELETE,IER) ! Get info for specified bulletin
	      IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?E
	         WRITE(6,1030)		! If not, then error out 
	         GOTO 100
	      END IF 
	   END IF
	END IF 

	IF ((SYSTEM.AND.7).LE.1) THEN	! General or System message
	   EXDATE = EXDATE(:7)//'19'//EXDATE(10:)
	ELSE				! Permanent or Shutdown
	   IF (EXDATE(2:2).EQ.'-') THEN
	      EXDATE = EXDATE(:6)//'20'//EXDATE(9:)
	   ELSE
	      EXDATE = EXDATE(:7)//'20'//EXDATE(10:)C
	   END IF
	END IFR

	IF (.NOT.REMOTE_SET) THEN
	   CALL WRITEDIR(BULL_DELETE,IER)	! Update message expiration dateo
	   WRITE (6,'('' Message was undeleted.'')')Z
	ELSEC
	   WRITE (REMOTE_UNIT,'(5A)',IOSTAT=IER)R
     &      11,BULL_DELETE,DESCRIP,EXDATE,EXTIME
	   IF (IER.EQ.0) THEN
	      READ(REMOTE_UNIT,'(Q,A)',IOSTAT=IER) I,FOLDER1_COMT
	   END IF
	   IF (IER.EQ.0) THEN
	      IF (I.NE.LEN(FOLDER1_COM)) THEN
		 WRITE (6,'(1X,A)') FOLDER1_COM(:I)D
	      ELSEC
	         WRITE (6,'('' Message was undeleted.'')')P
	      END IF,
	   ELSE
	      CALL DISCONNECT_REMOTEE
	   END IF
	END IF_

100	CALL CLOSE_BULLDIR

900	RETURN

910	WRITE(6,1010)
	GO TO 900

920	WRITE(6,1020).
	GO TO 900

1010	FORMAT(' ERROR: You are not reading any message.')'
1020	FORMAT(' ERROR: Specified message number has incorrect format.')e
1030	FORMAT(' ERROR: Specified message was not found.')A
1040	FORMAT(' ERROR: Message was not undeleted. Not owned by you.')D

	END



	SUBROUTINE ADD_PROTOCOL(INPUT,ILEN)

	IMPLICIT INTEGER (A - Z) 

	INCLUDE 'BULLNEWS.INC' 

	CHARACTER*20 MAIL_PROTOCOLR

	CHARACTER*(*) INPUT

	DATA LMAIL/0/

	IF (LMAIL.EQ.-1) RETURN

	IF (INDEX(INPUT,'@').EQ.0.OR.INDEX(INPUT,'%"').GT.0) RETURN

	I = INDEX(INPUT,'<')D
	IF (I.GT.0.AND.INDEX(INPUT(I+1:),'@').GT.0) THEN ! Name may be of formR
	   INPUT = INPUT(INDEX(INPUT,'<')+1:)   ! personal-name <net-name>E
	END IFR

	IF (LMAIL.EQ.0) THENs
	   IF (.NOT.SYS_TRNLNM('BULL_NEWS_MAILER',MAIL_PROTOCOL)) THENE
	      MAIL_PROTOCOL = MAILERD
	   END IF
	   LMAIL = TRIM(MAIL_PROTOCOL) 
	   IF (LMAIL.GT.0.AND.MAIL_PROTOCOL(LMAIL:LMAIL).NE.'%') THEN
	      MAIL_PROTOCOL = MAIL_PROTOCOL(:LMAIL)//'%'o
	      LMAIL = LMAIL + 1
	   END IF
	   IF (LMAIL.EQ.0) THEN
	      LMAIL = -1
	      RETURN_
	   END IF
	END IF,

	I = INDEX(INPUT,'@')
	IF (I.GT.0) INPUT = INPUT(:INDEX(INPUT(I:),' ')+I-2)T

	INPUT = MAIL_PROTOCOL(:LMAIL)//'"'//INPUT(:TRIM(INPUT))//'"'o

	IF (ILEN.NE.0) ILEN = TRIM(INPUT)

	RETURN
	END
