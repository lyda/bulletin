C
C  BULLETIN2.FOR, Version 6/15/91
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
 
	CHARACTER EXPIRE*3,INPUT_BBOARD*12,TODAY*23,RESPONSE*1
 
	IF (TRIM(BBOARD_DIRECTORY).EQ.0) THEN
	 WRITE(6,'('' ERROR: System programmer has disabled BBOARD.'')')
	 RETURN
	END IF
 
	IF (FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)) THEN
 
	   CALL OPEN_BULLFOLDER		! Open folder file
	   CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)
 
	   IF (FOLDER_BBOARD(:2).EQ.'::') THEN
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
		    WRITE (6,'(
     &		     '' ERROR: Account used by other folder.'')')
		    CALL CLOSE_BULLFOLDER
		    RETURN
		 END IF
		 IF (.NOT.IER1) THEN
		    CALL CLOSE_BULLFOLDER
		    WRITE (6,'('' WARNING: '',A,'' account not in SYSUAF'',
     &		       '' file.'')') INPUT_BBOARD(:INPUT_LEN)
		    CALL GET_INPUT_PROMPT(RESPONSE,RLEN,
     &                 'Is the name a mail forwarding entry? '//
     &		       '(Y/N with N as default): ')
		    IF (RESPONSE.NE.'y'.AND.RESPONSE.NE.'Y') THEN
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
	      ELSE IF (FOLDER_BBOARD.EQ.'NONE') THEN
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
 
	   CALL REWRITE_FOLDER_FILE
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
 
	IF (FOLDER_NUMBER.EQ.0) THEN
	   WRITE (6,'('' ERROR: Cannot modify GENERAL folder.'')')
	ELSE IF (FOLDER_NUMBER.LT.0) THEN
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
	   CALL REWRITE_FOLDER_FILE
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
 
	CHARACTER UPDATE*11,UPTIME*8
 
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
 
	IF (REMOTE_SET) THEN
	   WRITE(REMOTE_UNIT,'(3A)',IOSTAT=IER1) 14,BTEST(FOLDER_FLAG,2),
     &				NODENAME
	   IF (IER1.NE.0) THEN
	      CALL DISCONNECT_REMOTE
	      IF (.NOT.FILE_OPENED) CALL CLOSE_BULLUSER
	      RETURN
	   END IF
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
 
	IF (.NOT.FILE_OPENED) CALL CLOSE_BULLUSER
 
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
 
	CHARACTER RESPONSE*1,FOLDER_SAVE*25
 
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
	   WRITE (6,'('' Cannot set remote node for GENERAL folder.'')')
	ELSE IF (FOLDER_NUMBER.LT.0) THEN
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
	      IF (RESPONSE.NE.'y'.AND.RESPONSE.NE.'Y') THEN
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
	   CALL REWRITE_FOLDER_FILE
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
	CHARACTER*132 INCMD
 
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 
	COMMON /PAGE/ PAGE_LENGTH,PAGE_WIDTH,PAGING
 
	COMMON /INDESCRIP/ INDESCRIP
	CHARACTER*(LINE_LENGTH) INDESCRIP
 
	CHARACTER FROM_TEST*5,INFROM*(LINE_LENGTH)
 
	EXTERNAL CLI$_NEGATED,CLI$_ABSENT
 
	MSG_OWN = .FALSE.
 
	IF (INCMD(:4).EQ.'REPLY') THEN
	   BULL_PARAMETER = 'mailing list.'
	   IF (CLI$PRESENT('ALL')) THEN
	      BULL_PARAMETER = 'message owner and mailing list.'
	      MSG_OWN = .TRUE.
	   END IF
	ELSE IF (INCMD(:4).EQ.'RESP') THEN
	   MSG_OWN = .TRUE.
	   BULL_PARAMETER = 'message owner.'
	   IF (CLI$PRESENT('LIST'))
     &			BULL_PARAMETER = 'message owner and mailing list.'
	ELSE
	   BULL_PARAMETER = 'mailing list.'
	END IF
 
	IF (MSG_OWN.AND.BTEST(CAPTIVE(),1)) THEN
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
	      LENDES = 0
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
 
	CALL DISABLE_PRIVS
 
	ILEN = 0
 
	FILESPEC = CLI$GET_VALUE('FILESPEC',INPUT,ILEN)
	IF (FILESPEC.NE.%LOC(CLI$_ABSENT)) THEN
	   OPEN (UNIT=4,FILE=INPUT(:ILEN),STATUS='OLD',READONLY,
     &		 SHARED,IOSTAT=IER,FORM='FORMATTED')
	   IF (IER.NE.0) FILESPEC = .FALSE.
	END IF
 
	IF (EDIT.AND.(TEXT.OR.FILESPEC)) THEN
	   OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,
     &		RECL=LINE_LENGTH,STATUS='NEW',CARRIAGECONTROL='LIST')
 
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
	IF (CLI$GET_VALUE('CC',INPUT,ILEN)) THEN
	   CALL ADD_PROTOCOL(INPUT,ILEN)
	   INFROM = INPUT(:ILEN)
	   LENFRO = ILEN
	   IF (MSG_OWN) THEN
	      INFROM = INFROM(:LENFRO)//','
	      LENFRO = LENFRO + 1
	   END IF
	END IF
 
	IF ((EDIT.AND.TEXT).OR.INCMD(:4).NE.'POST') THEN
	   CALL ENABLE_PRIVS
	   CALL OPEN_BULLFIL_SHARED
 
	   ILEN = LINE_LENGTH + 1
 
	   CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN
	      ILEN = TRIM(INPUT) 
	      IF (MSG_OWN) THEN
	         CALL ADD_PROTOCOL(INPUT(7:),ILEN)
	         INFROM = INFROM(:LENFRO)//INPUT(7:)
	         LENFRO = LENFRO + ILEN - 6
	      END IF
	      IF (EDIT.AND.TEXT) THEN
		 IF (INPUT(ILEN:ILEN).EQ.'"') ILEN = ILEN - 1
		 INPUT = INPUT(7:ILEN)
		 ILEN = ILEN - 6
		 IF (INDEX(INPUT,'%"').GT.0) THEN
		    INPUT = INPUT(INDEX(INPUT,'%"')+2:)
		    ILEN = TRIM(INPUT)
		 END IF
	         WRITE (3,'(A)') 'In a previous article, '//
     &			INPUT(:ILEN)//' wrote:'
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
	      DO WHILE (ILEN.GT.0)		! Copy bulletin into file
	         IF (CLI$PRESENT('NOINDENT')) THEN
	            WRITE (3,'(A)') INPUT(:ILEN)
	         ELSE
	            WRITE (3,'(A)') '>'//INPUT(:ILEN)
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
	   CALL DISABLE_PRIVS
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
 
	IF (LIST.AND.REMOTE_SET.NE.3) THEN
	   SLIST = INDEX(FOLDER_DESCRIP,'<')
	   IF (SLIST.GT.0) THEN
	      IF (REMOTE_SET.NE.4) THEN
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
	      INFROM = INFROM(:I)//'"'//INFROM(I+1:)
	      I = I + 1
	      LENFRO = LENFRO + 1
	   END IF
	   I = I + 1
	END DO
 
	LENDES = TRIM(INDESCRIP)
	I = 1		! Must change all " to "" in SUBJECT field
	DO WHILE (I.LE.LENDES)
	   IF (INDESCRIP(I:I).EQ.'"') THEN
	      IF (LENDES.EQ.LINE_LENGTH) THEN
		 INDESCRIP(I:I) = '`'
	      ELSE
		 INDESCRIP = INDESCRIP(:I)//'"'
     &				//INDESCRIP(I+1:)
		 I = I + 1
		 LENDES = LENDES + 1
	      END IF
	   END IF
	   I = I + 1
	END DO
 
	STATUS = .TRUE.
 
	IF (EDIT) THEN
	   CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')
	   CONTEXT = 0
	   IER =  LIB$FIND_FILE('SYS$LOGIN:BULL.SCR',INPUT,CONTEXT)
	   IF (TEXT) THEN
	      VERSION = INDEX(INPUT,';') + 1
	      IF (INPUT(VERSION:VERSION).EQ.'1') THEN
	         CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;*')
	      ELSE
	         IER = 0
	      END IF
	   ELSE IF (IER) THEN
	      IER = 0
	   END IF
	   IF (IER.EQ.0) THEN
	      CALL ADD_SIGNATURE(0,'SYS$LOGIN:BULL.SCR',FOLDER_NAME)
	      IF (REMOTE_SET.GE.3.AND.LIST) THEN
	         CALL NEWS_POST('SYS$LOGIN:BULL.SCR',.FALSE.,IER,
     &			INDESCRIP)
		 STATUS = IER.EQ.0
	         IF (IER.EQ.0) THEN
		    WRITE (6,'('' Message successfully posted.'')')
	         END IF
	      END IF
	      IF (IER.EQ.0.AND.LENFRO.GT.0) THEN
	         CALL RESPOND_MAIL('SYS$LOGIN:BULL.SCR',INFROM(:LENFRO),
     &			   INDESCRIP(:LENDES),STATUS)
	      END IF
	   END IF
	ELSE
	   OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,
     &		RECL=LINE_LENGTH,STATUS='NEW',CARRIAGECONTROL='LIST')
	   IF (.NOT.FILESPEC) THEN
	      WRITE (6,'('' Enter message: End with ctrl-z,'',
     &			 '' cancel with ctrl-c'')')
	      ILEN = LINE_LENGTH + 1		! Length of input line
	      ICOUNT = 0			! Character count counter
	      DO WHILE (ILEN.GE.0)		! Input until no more input
	         CALL GET_LINE(INPUT,ILEN)	! Get input line
	         IF (ILEN.GT.LINE_LENGTH) THEN	! Input line too long
		    WRITE(6,'('' ERROR: Input line length > '',I,
     &			   ''.  Reinput:'')') LINE_LENGTH
	         ELSE IF (ILEN.GE.0) THEN	! If good input line entered
		    ICOUNT = ICOUNT + ILEN		! Update counter
		    WRITE(3,'(A)') INPUT(:ILEN)	! Save line in scratch file
	         END IF
	      END DO
	   ELSE
	      IER = 0
	      ICOUNT = 0
	      DO WHILE (IER.EQ.0)
		 READ (4,'(Q,A)',IOSTAT=IER) ILEN,INPUT
		 IF (IER.EQ.0) THEN
		    ICOUNT = ICOUNT + 1
		    WRITE (3,'(A)') INPUT(:ILEN)
		 END IF
	      END DO
	      CLOSE (UNIT=4)
	      FILESPEC = .FALSE.
	   END IF
	   IF (ILEN.EQ.-1.OR.ICOUNT.EQ.0) THEN	! CTRL_C or No lines
	      CLOSE (UNIT=3)
	      IER = 1
	   ELSE
	      CALL ADD_SIGNATURE(3,' ',FOLDER_NAME)
	      REWIND (UNIT=3)
	      IF (REMOTE_SET.GE.3.AND.LIST) THEN
	         CALL NEWS_POST('SYS$LOGIN:BULL.SCR',.TRUE.,IER,
     &			   INDESCRIP)
		 STATUS = IER.EQ.0
	         IF (IER.EQ.0)  WRITE (6,'('' Message successfully posted.'')')
	      ELSE
		 IER = 0
	      END IF
	      CLOSE (UNIT=3)
	      IF (IER.EQ.0.AND.LENFRO.GT.0) THEN
		 CALL RESPOND_MAIL('SYS$LOGIN:BULL.SCR',INFROM(:LENFRO),
     &			   INDESCRIP(:LENDES),STATUS)
	      END IF
	   END IF
	END IF
	IF (IER.NE.0) THEN
	   WRITE (6,'('' ERROR: No message added.'')')
	   IF (.NOT.STATUS) THEN
	      CALL GET_INPUT_PROMPT(INPUT,ILEN,'Do you want to'//
     &		' save message? (Y/N with N as default): ')
	      IF (STREQ(INPUT(:1),'Y')) THEN
		 CALL LIB$RENAME_FILE('SYS$LOGIN:BULL.SCR',
     &				      'SYS$LOGIN:BULL.SAV')
		 WRITE (6,'(A)') ' Message saved in SYS$LOGIN:BULL.SAV.'
	      END IF
	   END IF
	END IF
 
900	CALL ENABLE_PRIVS
	IF (FILESPEC) CLOSE (UNIT=4)
	CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;*')
 
	RETURN
	END
 
 
 
	SUBROUTINE ADD_SIGNATURE(FILEUNIT,FILENAME,FOLDER_NAME)
C
C  SUBROUTINE ADD_SIGNATURE
C
C  FUNCTION: Adds signature to message being mailed/posted.
C
	IMPLICIT INTEGER (A-Z)
 
	CHARACTER*(*) FOLDER_NAME
 
	CHARACTER*128 BULL_SIGNATURE
	DATA BULL_SIGNATURE /'SYS$LOGIN:BULL_SIGNATURE.TXT'/
 
	CHARACTER*255 INPUT
 
	OPEN (UNIT=4,FILE=BULL_SIGNATURE,STATUS='OLD',READONLY,
     &		 SHARED,IOSTAT=IER,FORM='FORMATTED')
 
	IF (IER.NE.0) THEN
	   OPEN (UNIT=4,FILE='BULL_SIGNATURE',STATUS='OLD',READONLY,
     &		    SHARED,IOSTAT=IER,FORM='FORMATTED')
	END IF
 
	IF (IER.NE.0) RETURN
 
	IF (FILEUNIT.EQ.0) THEN
	   OPEN (UNIT=3,FILE=FILENAME,STATUS='OLD',ACCESS='APPEND',
     &		 IOSTAT=IER,FORM='FORMATTED')
	END IF
 
	ICOUNT = 0
	MATCH = .FALSE.
	DO WHILE (IER.EQ.0)
	   READ (4,'(A)',IOSTAT=IER) INPUT
	   ILEN = TRIM(INPUT)
	   DO WHILE (.NOT.MATCH.AND.STREQ(INPUT(:6),'START ').AND.IER.EQ.0)
	      MATCH = STREQ(INPUT(7:ILEN),FOLDER_NAME(:TRIM(FOLDER_NAME)))
	      READ (4,'(A)',IOSTAT=IER) INPUT
	      ILEN = TRIM(INPUT)
	      IF (.NOT.MATCH) THEN
	         DO WHILE (.NOT.STREQ(INPUT(:ILEN),'END').AND.IER.EQ.0)
		    READ (4,'(A)',IOSTAT=IER) INPUT
	            ILEN = TRIM(INPUT)
		 END DO
		 READ (4,'(A)',IOSTAT=IER) INPUT
	         ILEN = TRIM(INPUT)
	      END IF
	   END DO
	   IF (IER.EQ.0) THEN
	      IF (MATCH.AND.STREQ(INPUT(:ILEN),'END')) THEN
	         MATCH = .FALSE.
	      ELSE
	         ICOUNT = ICOUNT + 1
	         IF (ICOUNT.EQ.1) WRITE (3,'(A)',IOSTAT=IER) ' '
	         WRITE (3,'(A)',IOSTAT=IER) INPUT(:ILEN)
	      END IF
	   END IF
	END DO
 
	CLOSE (UNIT=4)
	IF (FILEUNIT.EQ.0) CLOSE (UNIT=3)
 
	RETURN
	END
 
 
 
 
	LOGICAL FUNCTION STREQ(INPUT,INPUT1)
 
	IMPLICIT INTEGER (A-Z)
 
	CHARACTER*(*) INPUT,INPUT1
 
	STREQ = .FALSE.
 
	IF (LEN(INPUT).NE.LEN(INPUT1)) RETURN
 
	DO I=1,LEN(INPUT)
	   DIFF = ABS(ICHAR(INPUT(I:I))-ICHAR(INPUT1(I:I)))
	   IF (DIFF.NE.0.AND.DIFF.NE.32) RETURN
	END DO
 
	STREQ = .TRUE.
 
	RETURN
	END
 
 
 
 
 
 
	SUBROUTINE RESPOND_MAIL(FILE,SENDTO,SUBJECT,STATUS)
C
C  SUBROUTINE RESPOND_MAIL
C
C  FUNCTION: Sends mail to address.
C
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLUSER.INC'
 
	INCLUDE 'BULLFOLDER.INC'
 
	CHARACTER*(*) FILE,SENDTO,SUBJECT
 
	CHARACTER MAILER*128
 
	LISTSERV = INDEX(FOLDER_DESCRIP,'LISTSERV').GT.0
 
	IF (LISTSERV) THEN
	   CALL SETUSER(FOLDER_BBOARD)
	   IF (SYS_TRNLNM('MX_NODE_NAME',MAILER)) THEN
	      REPLY_TO = .NOT.SYS_TRNLNM('MX_REPLY_TO',MAILER)
	      IF (REPLY_TO) IER = LIB$SET_LOGICAL
     &			('MX_REPLY_TO',USERNAME(:TRIM(USERNAME)))
	   ELSE
	      REPLY_TO = .NOT.SYS_TRNLNM('PMDF_REPLY_TO',MAILER)
	      IF (REPLY_TO) IER = LIB$SET_LOGICAL
     &			('PMDF_REPLY_TO',USERNAME(:TRIM(USERNAME)))
	   END IF
	END IF
 
	IF (SYS_TRNLNM('BULL_MAILER',MAILER)) THEN
	   IF (LISTSERV) THEN
	      IF (SYS_TRNLNM_SYSTEM('BULL_MAILER',MAILER)) THEN
	         CALL LIB$SPAWN('@'//MAILER(:TRIM(MAILER))//
     &		    ' SYS$LOGIN:BULL.SCR """'//SENDTO//'""" """'
     &		    //SUBJECT//'""" '
     &		    //USERNAME(:TRIM(USERNAME)),,,,,,STATUS)
	      END IF
	   ELSE
	      CALL LIB$SPAWN('@'//MAILER(:TRIM(MAILER))//
     &			     ' SYS$LOGIN:BULL.SCR """'//SENDTO//
     &			     '""" """'//SUBJECT//'"""',,,,,,STATUS)
	   END IF
	ELSE
	   CALL LIB$SPAWN('$MAIL SYS$LOGIN:BULL.SCR "'//SENDTO//
     &		      '" /SUBJECT="'//SUBJECT//'"',,,,,,STATUS)
	END IF
 
	IF (LISTSERV) THEN
	   CALL SETUSER(USERNAME)
	   IF (REPLY_TO) IER = LIB$DELETE_LOGICAL('PMDF_REPLY_TO')
	   IF (REPLY_TO) IER = LIB$DELETE_LOGICAL('MX_REPLY_TO')
	END IF
 
	RETURN
	END
 
 
 
	INTEGER FUNCTION CONFIRM_USER(USERNAME)
C
C  FUNCTION CONFIRM_USER
C
C  FUNCTION: Confirms that username is valid user.
C
	IMPLICIT INTEGER (A-Z)
 
	CHARACTER*(*) USERNAME
 
	CALL OPEN_SYSUAF_SHARED
 
	READ (8,KEY=USERNAME,IOSTAT=CONFIRM_USER)
 
	CALL CLOSE_SYSUAF
 
	RETURN
	END
 
 
 
 
 
	SUBROUTINE REPLACE
C
C  SUBROUTINE REPLACE
C
C  FUNCTION: CHANGE command subroutine.
C
	IMPLICIT INTEGER (A - Z)
 
	COMMON /POINT/ BULL_POINT
 
	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER
 
	COMMON /EDIT/ EDIT_DEFAULT
 
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 
	COMMON /LAST_RECORD_WRITTEN/ OCOUNT
 
	INCLUDE 'BULLDIR.INC'
 
	INCLUDE 'BULLUSER.INC'
 
	INCLUDE 'BULLFOLDER.INC'
 
	CHARACTER INEXDATE*11,INEXTIME*11
	CHARACTER INDESCRIP*(LINE_LENGTH),INFROM*(LINE_LENGTH)
	CHARACTER*1 ANSWER
 
	CHARACTER DATE_SAVE*11,TIME_SAVE*11
 
	INTEGER TIMADR(2)
 
	EXTERNAL CLI$_ABSENT,CLI$_NEGATED
 
	LOGICAL*1 DOALL
 
	IF (REMOTE_SET.EQ.3) THEN
	   WRITE (6,'('' Cannot CHANGE messages in this folder.'')')
	   RETURN
	END IF
 
C
C  Get the bulletin number to be replaced.
C
 
	ALL = CLI$PRESENT('ALL')
 
	IER1 = CLI$GET_VALUE('NUMBER',BULL_PARAMETER,LEN_P)
	IF (IER1.EQ.%LOC(CLI$_ABSENT).AND..NOT.ALL) THEN
	   IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been read
	      WRITE (6,1005)		! Tell user of the error
	      RETURN			! and return
	   END IF
	   SBULL = BULL_POINT		! Replace the bulletin we are reading
	   EBULL = SBULL
 
	   CALL OPEN_BULLDIR_SHARED
	   CALL READDIR(BULL_POINT,IER)		! Get message directory entry
	   CALL CLOSE_BULLDIR
	   IF (IER.NE.BULL_POINT+1) THEN	! Was message found?
	      WRITE(6,'('' ERROR: Specified message was not found.'')')
	      RETURN
	   END IF
	ELSE
	   CALL OPEN_BULLDIR_SHARED
	   CALL READDIR(0,IER)		! Get message directory entry
	   CALL CLOSE_BULLDIR
	   IF (NBULL.EQ.0) THEN		! Were messages found?
	      WRITE(6,'('' ERROR: No messages were found.'')')
	      RETURN
	   END IF
 
	   IF (IER1.NE.%LOC(CLI$_ABSENT)) THEN
	      CALL GET_2_VALS(BULL_PARAMETER,LEN_P,SBULL,EBULL,IER1)
	      IF (SBULL.LE.0.OR.IER1.NE.0) THEN
	         WRITE (6,'(A)') 
     &		  ' ERROR: Specified message number has incorrect format.'
	         RETURN
	      END IF
	      ALL = .TRUE.
	   ELSE IF (CLI$PRESENT('ALL')) THEN
	      SBULL = 1
	      EBULL = NBULL
	   END IF
	END IF
 
	IF (CLI$PRESENT('SYSTEM')) THEN
	   IF (.NOT.SETPRV_PRIV()) THEN
	    WRITE (6,'(
     &	     '' ERROR: Not enough privileges to change to system.'')')
	    RETURN
	   ELSE IF (.NOT.BTEST(FOLDER_FLAG,2).AND.FOLDER_NUMBER.NE.0) THEN
	    WRITE (6,'(
     &       '' ERROR: /SYSTEM cannot be set with selected folder.'')')
	    RETURN
	   END IF
	END IF
 
	IF (CLI$PRESENT('SHUTDOWN')) THEN
	   IF (.NOT.SETPRV_PRIV()) THEN
	    WRITE (6,'(
     &	     '' ERROR: Not enough privileges to change to shutdown.'')')
	    RETURN
	   ELSE IF (.NOT.BTEST(FOLDER_FLAG,2).AND.FOLDER_NUMBER.NE.0) THEN
	    WRITE (6,'(
     &      '' ERROR: /SHUTDOWN cannot be set with selected folder.'')')
	    RETURN
	   ELSE IF (CLI$GET_VALUE('SHUTDOWN',BULL_PARAMETER).NE.
     &		    %LOC(CLI$_ABSENT).AND.REMOTE_SET) THEN
	    WRITE (6,'('' ERROR: Shutdown node name not'',
     &			    '' permitted for remote folder.'')')
	    RETURN
	   END IF
	END IF
 
	IF (CLI$PRESENT('PERMANENT').AND.
     &		F_EXPIRE_LIMIT.GT.0.AND..NOT. ! Expiration limit present
     &		FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)) THEN
	   WRITE (6,'(
     &	    '' ERROR: Not enough privileges to change to permanent.'')')
	   RETURN
	END IF
C
C  Check to see if specified bulletin is present, and if the user
C  is permitted to replace the bulletin.
C
 
	CALL OPEN_BULLDIR_SHARED
 
	SAME_OWNER = .TRUE.
	DO I=SBULL,EBULL
	   CALL READDIR(I,IER)	! Get info for specified messages
	   IF (USERNAME.NE.FROM) SAME_OWNER = .FALSE. 
	END DO
	CALL READDIR(SBULL,IER)
 
	CALL CLOSE_BULLDIR
 
	IF (.NOT.SAME_OWNER) THEN	! If doesn't match owner of bulletin,
	   IF ((.NOT.SETPRV_PRIV().AND..NOT.FOLDER_SET).OR.	! Privileges or
     &	       (.NOT.FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)
     &		.AND.FOLDER_SET)) THEN				! folder owner?
	      WRITE(6,1090)		! If not, then error out.
	      RETURN
	   ELSE
	      WRITE (6,1100)		! Make sure user wants to delete it
	      READ (5,'(A)',IOSTAT=IER) ANSWER	! Get his answer
	      CALL STR$UPCASE(ANSWER,ANSWER)	! Convert input to uppercase
	      IF (ANSWER.NE.'Y') RETURN	! If not Yes, then exit
	   END IF
	END IF
 
C
C  If no switches were given, replace the full bulletin
C
 
	DOALL = .FALSE.
 
	TEXT = CLI$PRESENT('TEXT')
 
	IF ((.NOT.CLI$PRESENT('EXPIRATION')).AND.
     &	   (.NOT.CLI$PRESENT('GENERAL')).AND.
     &	   (.NOT.CLI$PRESENT('SYSTEM')).AND.
     &	   (.NOT.CLI$PRESENT('HEADER')).AND.
     &	   (.NOT.CLI$PRESENT('SUBJECT')).AND.
     &	   (.NOT.TEXT).AND.
     &	   (.NOT.CLI$PRESENT('SHUTDOWN')).AND.
     &	   (.NOT.CLI$PRESENT('PERMANENT'))) THEN
	   DOALL = .TRUE.
	END IF
 
	IF (SBULL.NE.EBULL.AND.(DOALL.OR.TEXT)) THEN
	   WRITE (6,'('' ERROR: Cannot change text when replacing'',
     &		      '' more than one messsage.'')')
	   RETURN
	END IF
 
	CALL DISABLE_CTRL			! Disable CTRL-Y & -C
 
	PERMANENT = .FALSE.
	IF (CLI$PRESENT('EXPIRATION').OR.DOALL) THEN
	   SYSTEM = 0
	   CALL GET_EXPIRED(INPUT,IER)
	   PERMANENT = BTEST(SYSTEM,1)
	   IF (.NOT.IER) GO TO 910
	   INEXDATE = INPUT(:11)
	   INEXTIME = INPUT(13:)
	END IF
 
8	LENDES = 0
	IF (CLI$PRESENT('HEADER').OR.DOALL) THEN
	   WRITE(6,1050)			! Request header for bulletin
	   READ(5,'(Q,A)',END=910,ERR=910) LENDES,INDESCRIP
	   IF (LENDES.EQ.0) GO TO 910		! If no header, don't add bull
	ELSE IF (CLI$PRESENT('SUBJECT')) THEN
	   IER = CLI$GET_VALUE('SUBJECT',INDESCRIP,LENDES)
	END IF
 
	IF (LENDES.GT.0) THEN
	   INDESCRIP = 'Subj: '//INDESCRIP
	   LENDES = MIN(LENDES+6,LEN(INDESCRIP))
	END IF
 
	IF (SBULL.NE.EBULL) CALL OPEN_BULLDIR
 
	DO NUMBER=SBULL,EBULL
	 NUMBER_PARAM = NUMBER
	 IF (SBULL.NE.EBULL) THEN
	   CALL READDIR(NUMBER_PARAM,IER)
	   IF (IER.NE.NUMBER_PARAM+1) THEN	! Couldn't find message
	      CALL CLOSE_BULLDIR
	      WRITE(6,'('' ERROR: Message '',I6,'' cannot be found.'')')
     &			NUMBER_PARAM
	      WRITE(6,'('' All messages up to that message were modified.'')')
	      RETURN
	   END IF
	 END IF
 
	 REC1 = 0
 
	 LENFROM = 0
 
	 IF (LENDES.GT.0.OR.TEXT.OR.DOALL) THEN
	   OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,
     &	     RECL=LINE_LENGTH,STATUS='SCRATCH',CARRIAGECONTROL='LIST')
 
	   IF (IER.NE.0) THEN
	      CALL ERRSNS(IDUMMY,IER)
	      CALL SYS_GETMSG(IER)
	      GO TO 910
	   END IF
 
	   CALL OPEN_BULLFIL_SHARED
 
	   REC1 = 1
 
	   ILEN = LINE_LENGTH + 1
 
	   CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN
	      INFROM = INPUT(:ILEN)
	      LENFROM = ILEN
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   END IF
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THEN
	      IF (LENDES.EQ.0.AND..NOT.DOALL) THEN
		 INDESCRIP = INPUT(:ILEN)
		 LENDES = ILEN
	      END IF
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   END IF
 
	   DO WHILE (ILEN.GT.0)		! Copy bulletin into file
	      WRITE (3,'(A)') INPUT(:ILEN)
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   END DO
 
	   CALL CLOSE_BULLFIL
 
	   IF (TEXT.OR.DOALL) CLOSE(UNIT=3)
	 END IF
 
	 IF (TEXT.OR.DOALL) THEN
C
C  If file specified in REPLACE command, read file to obtain bulletin.
C  Else, read the bulletin from the terminal.
C
	
	  ICOUNT = 0				! Line count for bulletin
	  LAST_NOBLANK = 0			! Last line with data
	  REC1 = 1
 
	  IER = CLI$GET_VALUE('FILESPEC',BULL_PARAMETER,LEN_P)
	  IF (IER.NE.%LOC(CLI$_ABSENT).OR.	! If file param in ADD command
     &	    ((CLI$PRESENT('EDIT').OR.EDIT_DEFAULT).AND.	! or /EDIT specified
     &       (CLI$PRESENT('EDIT').NE.%LOC(CLI$_NEGATED)))) THEN
 
	   IF ((CLI$PRESENT('EDIT').OR.EDIT_DEFAULT).AND. ! If /EDIT specified
     &       (CLI$PRESENT('EDIT').NE.%LOC(CLI$_NEGATED))) THEN
	      IF (LEN_P.EQ.0) THEN		! If no file param specified
		 IF (.NOT.CLI$PRESENT('NEW')) THEN
	            OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='NEW',
     &		       RECL=LINE_LENGTH,
     &		       ERR=920,FORM='FORMATTED',CARRIAGECONTROL='LIST')
	            CALL OPEN_BULLFIL_SHARED	! Prepare to copy message
		    ILEN = LINE_LENGTH + 1
		    CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
		    IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN
		       CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
		    END IF
		    IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THEN
		       CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
		    END IF
		    DO WHILE (ILEN.GT.0)	! Copy message into file
		       WRITE (3,'(A)') INPUT(:ILEN)
		       CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
		    END DO
		    CALL CLOSE_BULLFIL
	            CLOSE (UNIT=3)		! Bulletin copy completed
		 END IF
		 CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')
	      ELSE 
	         CALL DISABLE_PRIVS
		 CALL MAILEDIT(BULL_PARAMETER(:LEN_P),'SYS$LOGIN:BULL.SCR')
	      END IF
	      IER = LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;-1')
	      OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='OLD',
     &		 DISPOSE='DELETE',ERR=920,FORM='FORMATTED')
	   ELSE IF (LEN_P.GT.0) THEN
	      CALL DISABLE_PRIVS
	      OPEN (UNIT=3,FILE=BULL_PARAMETER(:LEN_P),STATUS='OLD',
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
	      ELSE IF (ILEN.EQ.0) THEN
		 IF (ICOUNT.GT.0) THEN
		    ICOUNT = ICOUNT + 2		! COPY_BULL writes a line with
		 ELSE				! 1 space for a blank line.
		    REC1 = REC1 + 1
		 END IF
	      END IF
	   END DO
	  ELSE					! If no input file
	   OPEN (UNIT=3,STATUS='NEW',FILE='SYS$LOGIN:BULL.SCR',ERR=920,
     &		 DISPOSE='DELETE',FORM='FORMATTED',RECL=LINE_LENGTH,
     &		 CARRIAGECONTROL='LIST')	! Scratch file to save bulletin
	   WRITE (6,1000)		! Request bulletin input from terminal
	   ILEN = LINE_LENGTH			! Length of input line
	   DO WHILE (ILEN.GE.0)			! Input until no more input
	      CALL GET_LINE(INPUT,ILEN)		! Get input line
	      IF (ILEN.GT.LINE_LENGTH) THEN	! Line too long.
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
10	   ICOUNT = LAST_NOBLANK
	   IF (ICOUNT.EQ.0) GO TO 910		! No lines entered, error out
	  ENDIF
 
	 END IF
 
C
C  Add bulletin to bulletin file and directory entry for to directory file.
C
 
	 DATE_SAVE = DATE
	 TIME_SAVE = TIME
	 INPUT = DESCRIP
 
	 IF (SBULL.EQ.EBULL) THEN
	  CALL OPEN_BULLDIR			! Prepare to add dir entry
	  CALL READDIR(NUMBER_PARAM,IER)	! Get info for message
 
	  IF (IER.NE.NUMBER_PARAM+1.OR.DATE.NE.DATE_SAVE.OR.
     &	     TIME.NE.TIME_SAVE.OR.INPUT.NE.DESCRIP) THEN
				! If message disappeared, try to find it.
	   IF (IER.NE.NUMBER_PARAM+1) DATE = ' '
	   NUMBER_PARAM = 0
	   IER = 1
	   DO WHILE (IER.EQ.NUMBER_PARAM+1.AND.
     &	    (DATE.NE.DATE_SAVE.OR.TIME.NE.TIME_SAVE.OR.DESCRIP.NE.INPUT))
	      NUMBER_PARAM = NUMBER_PARAM + 1
	      CALL READDIR(NUMBER_PARAM,IER)
	   END DO
 
	   IF (IER.NE.NUMBER_PARAM+1) THEN	! Couldn't find message
	      CALL CLOSE_BULLDIR
	      CLOSE (UNIT=3,STATUS='SAVE')
	      WRITE(6,'('' ERROR: Message has been deleted'',
     &			'' by another user.'')')
	      IF (DOALL.OR.TEXT) THEN
		 WRITE (6,'('' New text has been saved in'',
     &				'' SYS$LOGIN:BULL.SCR.'')')
	      END IF
	      GO TO 100
	   END IF
	  END IF
	 END IF
 
	 CALL READDIR(0,IER)			! Get directory header
 
	 IF (REC1.GT.0) THEN			! If text has been replaced
 
	   CALL OPEN_BULLFIL			! Prepare to add bulletin
 
	   BLOCK = NBLOCK + 1
	   BLOCK_SAVE = BLOCK
	   NEMPTY = NEMPTY + LENGTH
 
	   OBLOCK = BLOCK
	   IF (LENFROM.GT.0) THEN
	      CALL STORE_BULL(LENFROM,INFROM(:LENFROM),OBLOCK)
	   END IF
	   IF (LENDES.GT.0) THEN
	      CALL STORE_BULL(LENDES,INDESCRIP(:LENDES),OBLOCK)
	   END IF
	   REWIND (UNIT=3)
	   CALL COPY_BULL(3,REC1,OBLOCK,IER)	! Add the new bulletin
	   IF (IER.NE.0) THEN		! Error in creating bulletin
	      WRITE (6,'(A)') ' ERROR: Unable to replace message.'
	      CALL CLOSE_BULLFIL
	      CALL CLOSE_BULLDIR
	      CLOSE (UNIT=3)
	      GO TO 100
	   END IF
 
	   LENGTH_SAVE = OCOUNT - BLOCK + 1
	   NBLOCK = NBLOCK + LENGTH_SAVE
 
	   IF (.NOT.REMOTE_SET) CALL WRITEDIR(0,IER)
 
	   CALL CLOSE_BULLFIL
 
	   IF (.NOT.REMOTE_SET) THEN
	    CALL READDIR(NUMBER_PARAM,IER)	! Get directory entry
	    LENGTH = LENGTH_SAVE		! Update size
	    BLOCK = BLOCK_SAVE
	    CALL WRITEDIR(NUMBER_PARAM,IER)	! Write new directory entry
	   END IF
	 ELSE
	   CALL READDIR(NUMBER_PARAM,IER)
	 END IF
 
	 IF (.NOT.REMOTE_SET) THEN
 
	   IF (LENDES.GT.0.OR.DOALL) THEN
	      DESCRIP=INDESCRIP(7:59)		! Update description header
	   END IF
	   CALL UPDATE_DIR_HEADER((CLI$PRESENT('EXPIRATION').OR.DOALL).AND.
     &		.NOT.PERMANENT,CLI$PRESENT('PERMANENT').OR.PERMANENT,
     &		CLI$PRESENT('SHUTDOWN'),INEXDATE,INEXTIME)
	   IF (CLI$PRESENT('SYSTEM')) THEN
	      SYSTEM = IBSET(SYSTEM,0)
	   ELSE IF (CLI$PRESENT('GENERAL')) THEN
	      SYSTEM = IBCLR(SYSTEM,0)
	   END IF
	   CALL WRITEDIR(NUMBER_PARAM,IER)
	 ELSE
	   MSGTYPE = 0
	   IF (CLI$PRESENT('SYSTEM').OR.
     &		(BTEST(SYSTEM,0).AND..NOT.CLI$PRESENT('GENERAL'))) THEN
	      MSGTYPE = IBSET(MSGTYPE,0)
	   END IF
	   IF (CLI$PRESENT('PERMANENT').OR.PERMANENT) THEN
	      MSGTYPE = IBSET(MSGTYPE,1)
	   ELSE IF (CLI$PRESENT('SHUTDOWN')) THEN
	      MSGTYPE = IBSET(MSGTYPE,2)
	   ELSE IF ((CLI$PRESENT('EXPIRATION').OR.DOALL)
     &		    .AND..NOT.PERMANENT) THEN
	      MSGTYPE = IBSET(MSGTYPE,3)
	   END IF
	   IF (LENDES.EQ.0.AND..NOT.DOALL) INDESCRIP(7:) = DESCRIP
	   IF (CLI$PRESENT('EXPIRATION').OR.DOALL) THEN
	      EXDATE = INEXDATE
	      EXTIME = INEXTIME
	   END IF
	   WRITE (REMOTE_UNIT,'(7A)',IOSTAT=IER)
     &      10,DESCRIP,NUMBER_PARAM,INDESCRIP(7:59),MSGTYPE,
     &	    EXDATE,EXTIME
	   IF (IER.EQ.0) THEN
	      READ(REMOTE_UNIT,'(Q,A)',IOSTAT=IER) I,FOLDER1_COM
	   END IF
	   IF (IER.EQ.0) THEN
	      IF (I.NE.LEN(FOLDER1_COM)) THEN
		 WRITE (6,'(1X,A)') FOLDER1_COM(:I)
	      END IF
	   ELSE
	      CALL DISCONNECT_REMOTE
	   END IF
	 END IF
	END DO
 
	CALL CLOSE_BULLDIR		! Totally finished with replace
 
	CLOSE (UNIT=3)
 
100	CALL ENABLE_CTRL		! Enable CTRL-Y & -C
	RETURN
 
910	WRITE(6,1010)
	CLOSE (UNIT=3,ERR=100)
	GOTO 100
 
920	WRITE(6,1020)
	CALL ENABLE_PRIVS	! Reset SYSPRV privileges
	GOTO 100
 
950	WRITE (6,1030) LINE_LENGTH
	CLOSE (UNIT=3)
	GO TO 100
 
1000	FORMAT (' Enter message: End with ctrl-z, cancel with ctrl-c')
1005	FORMAT (' ERROR: You are not reading any message.')
1010	FORMAT (' No message was replaced.')
1015	FORMAT (' ERROR: Specified message was not found.')
1020	FORMAT (' ERROR: Unable to open specified file.')
1030	FORMAT (' ERROR: Line length in file exceeds '',I,'' characters.')
1050	FORMAT (' Enter description header.')
1090	FORMAT(' ERROR: Specified message is not owned by you.')
1100	FORMAT(' Message(s) is not owned by you.',
     &	       ' Are you sure you want to replace it? ',$)
2020	FORMAT(1X,A)
 
	END
 
 
 
	SUBROUTINE UPDATE_DIR_HEADER(EXPIRE,PERM,SHUT,INEXDATE,INEXTIME)
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLDIR.INC'
 
	EXTERNAL CLI$_ABSENT
 
	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD
 
	CHARACTER TODAY*23,INEXDATE*11,INEXTIME*11
 
	IF (EXPIRE) THEN
	   SYSTEM = IBCLR(SYSTEM,1)
	   SYSTEM = IBCLR(SYSTEM,2)
	   EXDATE=INEXDATE			! Update expiration date
	   EXTIME=INEXTIME
	   DIFF = COMPARE_DATE(EXDATE,NEWEST_EXDATE)	! Compare expiration
	   IF (DIFF.EQ.0) DIFF = COMPARE_TIME(EXTIME,NEWEST_EXTIME)
	   IF (DIFF.LT.0) THEN			! If it's oldest expiration bull
	      NEWEST_EXDATE = EXDATE		! Update the header in
	      NEWEST_EXTIME = EXTIME		! the directory file
	      CALL WRITEDIR(0,IER)
	   END IF
	ELSE IF (PERM.AND.(.NOT.BTEST(SYSTEM,1))) THEN
	   IF (BTEST(SYSTEM,2)) THEN
	      SYSTEM = IBCLR(SYSTEM,2)
	      SHUTDOWN = SHUTDOWN - 1
	      CALL WRITEDIR(0,IER)
	   END IF
	   SYSTEM = IBSET(SYSTEM,1)
	   EXDATE = '5-NOV-2000'
	   EXTIME = '00:00:00.00'
	ELSE IF (SHUT.AND.(.NOT.BTEST(SYSTEM,2))) THEN
	   SYSTEM = IBSET(SYSTEM,2)
	   SYSTEM = IBCLR(SYSTEM,1)
	   EXDATE = '5-NOV-2000'
	   NODE_AREA = 0
	   IF (INCMD(:4).EQ.'REPL') THEN
	      IF (CLI$GET_VALUE('SHUTDOWN',NODE_NAME)
     &		    .NE.%LOC(CLI$_ABSENT)) THEN
		 CALL GET_NODE_NUMBER_OTHER(NODE_NUMBER,NODE_AREA,NODE_NAME)
	         IF (NODE_AREA.EQ.0) THEN
		    WRITE (6,'('' ERROR: Shutdown node name ignored.'',
     &		               '' Invalid node name specified.'')')
		 END IF
	      END IF
	   END IF
	   IF (NODE_AREA.EQ.0) CALL GET_NODE_NUMBER(NODE_NUMBER,NODE_AREA)
	   WRITE (EXTIME,'(I4)') NODE_NUMBER
	   WRITE (EXTIME(7:),'(I4)') NODE_AREA
	   DO I=1,11
	      IF (EXTIME(I:I).EQ.' ') EXTIME(I:I) = '0'
	   END DO
	   EXTIME = EXTIME(1:2)//':'//EXTIME(3:4)//':'//
     &		    EXTIME(7:8)//'.'//EXTIME(9:10)
	   SHUTDOWN = SHUTDOWN + 1
	   CALL SYS$ASCTIM(,TODAY,,)		! Get the present time
	   SHUTDOWN_DATE = TODAY(:11)
	   SHUTDOWN_TIME = TODAY(13:)
	   CALL WRITEDIR(0,IER)
	END IF
 
	RETURN
	END
 
 
 
 
	SUBROUTINE SEARCH(READ_COUNT)
C
C  SUBROUTINE SEARCH
C
C  FUNCTION: Search for bulletin with specified string
C
	IMPLICIT INTEGER (A - Z)
 
	INCLUDE 'BULLFOLDER.INC'
 
	COMMON /POINT/ BULL_POINT
 
	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER
 
	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD
 
	CHARACTER*132 SEARCH_STRING
 
	EXTERNAL CLI$_ABSENT
 
	IF (INCMD.NE.'SEAR') NFOLDER = 1
 
	IF (CLI$PRESENT('SELECT_FOLDER')) THEN
	   CALL INIT_QUEUE(SCRATCH_F1,FOLDER1_NAME)
	   SCRATCH_F = SCRATCH_F1
	   NFOLDER = 0
	END IF
 
	DO WHILE (CLI$GET_VALUE('SELECT_FOLDER',FOLDER1_NAME)
     &	    .NE.%LOC(CLI$_ABSENT))		! Get the specified folders
	   IF (TRIM(FOLDER1_NAME).EQ.0) FOLDER1_NAME = FOLDER_NAME
	   NFOLDER = NFOLDER + 1
	   CALL WRITE_QUEUE(%VAL(SCRATCH_F),SCRATCH_F,FOLDER1_NAME)
	END DO
 
	IF (CLI$PRESENT('SELECT_FOLDER')) SCRATCH_F = SCRATCH_F1
 
	START_BULL = BULL_POINT
 
	IF (CLI$PRESENT('START')) THEN		! Starting message specified
	   CALL CLI$GET_VALUE('START',BULL_PARAMETER,LEN_P)
	   DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER) START_BULL
	   IF (.NOT.CLI$PRESENT('REPLY')) START_BULL = START_BULL - 1
	END IF
 
	IER = CLI$GET_VALUE('SEARCH_STRING',SEARCH_STRING,SEARCH_LEN)
 
	IF (NFOLDER.GT.0) FOUND = 0
 
	DO WHILE (NFOLDER.GT.0.AND.FOUND.LE.0)
	   IF (.NOT.CLI$PRESENT('SELECT_FOLDER').OR.
     &		SCRATCH_F.NE.SCRATCH_F1) 
     &	      CALL GET_SEARCH(FOUND,SEARCH_STRING,START_BULL,
     &		CLI$PRESENT('REVERSE'),CLI$PRESENT('SUBJECT'),
     &		CLI$PRESENT('REPLY'),.TRUE.,CLI$PRESENT('START'))
	   IF (FOUND.EQ.-1) THEN
	      NFOLDER = 0
	   ELSE IF (FOUND.LE.0) THEN
	      IF (.NOT.CLI$PRESENT('SELECT_FOLDER').OR.
     &		   SCRATCH_F.NE.SCRATCH_F1) NFOLDER = NFOLDER - 1
	      IF (NFOLDER.GT.0) THEN
	         CALL READ_QUEUE(%VAL(SCRATCH_F),SCRATCH_F,FOLDER1_NAME)
		 OLD_FOLDER_NUMBER = FOLDER_NUMBER
	         FOLDER_NUMBER = -1
		 IER = 0
		 DO WHILE (.NOT.IER.AND.NFOLDER.GT.0)
		    FOLDER1 = FOLDER1_NAME
	            CALL SELECT_FOLDER(.FALSE.,IER)
		    IF (.NOT.IER) THEN
		       FOLDER_NUMBER = OLD_FOLDER_NUMBER
		       WRITE (6,'('' ERROR: Cannot find folder '',A,
     &			  ''.'')') FOLDER1_NAME(:TRIM(FOLDER1_NAME))
	               CALL GET_INPUT_PROMPT(FOLDER1_NAME,ILEN,
     &		       'Type new folder name or hit RETURN to continue: ')
	               IF (ILEN.LE.0.AND.NFOLDER.GT.0) THEN
		          NFOLDER = NFOLDER - 1
	                  CALL READ_QUEUE(%VAL(SCRATCH_F),SCRATCH_F,
     &				          FOLDER1_NAME)
		       END IF
		    END IF
		 END DO
	      END IF
	   END IF
	END DO
 
	IF (FOUND.GT.0) THEN
	   BULL_POINT = FOUND - 1
	   CALL READ_MSG(READ_COUNT,BULL_POINT+1) ! Read next bulletin
	ELSE IF (FOUND.EQ.0) THEN
	   WRITE (6,'('' No messages found with given search string.'')')
	ELSE IF (FOUND.EQ.-2) THEN
	   WRITE (6,'('' ERROR: No more messages.'')')
	END IF
 
	RETURN
	END
 
 
 
 
	SUBROUTINE GET_SEARCH(FOUND,SEARCH_STRING,START_BULL,REVERSE,
     &				SUBJECT,REPLY,FILES,START)
C
C  SUBROUTINE GET_SEARCH
C
C  FUNCTION: Search for bulletin with specified string
C
	IMPLICIT INTEGER (A - Z)
 
	INCLUDE 'BULLDIR.INC'
 
	INCLUDE 'BULLFOLDER.INC'
 
	COMMON /TAGS/ BULL_TAG,READ_TAG,BULL_NEWS_TAG
 
	COMMON /CTRLC_FLAG/ FLAG
 
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 
	COMMON /NEXT/ NEXT
 
	CHARACTER*(*) SEARCH_STRING
 
	CHARACTER*132 SAVE_STRING
	DATA SAVE_STRING/' '/
 
	CHARACTER*53 DESCRIP1
 
	FOUND = -1
 
	CALL DISABLE_CTRL
 
	CALL DECLARE_CTRLC_AST
 
	IF (TRIM(SEARCH_STRING).EQ.0) THEN
	   IER1 = .FALSE.
	ELSE
	   IER1 = .TRUE.
	END IF
	
	IF (.NOT.IER1.AND..NOT.REPLY.AND.
     &      (SUBJECT.OR.SEARCH_MODE.NE.1)) THEN
						! If no search string entered
	   SEARCH_STRING = SAVE_STRING		! use saved search string
	   IF (TRIM(SAVE_STRING).EQ.0) THEN
	      WRITE (6,'('' No search string present.'')')
	      CALL CANCEL_CTRLC_AST
	      CALL ENABLE_CTRL
	      RETURN
	   END IF
	   IF (STEP_BULL.EQ.-1) START_BULL = START_BULL - 2
	ELSE IF (.NOT.IER1.AND.SEARCH_MODE.EQ.1) THEN
	   SEARCH_STRING = SAVE_STRING		! use saved search string
	END IF
 
	IF (FILES) CALL OPEN_BULLDIR_SHARED
 
	CALL READDIR(0,IER)
 
	OLD_SEARCH_MODE = SEARCH_MODE
	IF (IER1) THEN				! If string entered
	   IF (SUBJECT) THEN
	      SEARCH_MODE = 3
	   ELSE
	      SEARCH_MODE = 2
	   END IF
	ELSE IF (SUBJECT.AND.SEARCH_MODE.NE.3) THEN
	   SEARCH_MODE = 3
	ELSE IF (REPLY) THEN
	   CALL READDIR(START_BULL,IER)
	   IF (START_BULL+1.NE.IER) THEN
	      WRITE (6,'('' ERROR: No message being read.'')')
	      IF (FILES) CALL CLOSE_BULLDIR
	      CALL CANCEL_CTRLC_AST
	      CALL ENABLE_CTRL
	      RETURN
	   ELSE
	      SEARCH_MODE = 1
	      SEARCH_STRING = DESCRIP
	      IF (REVERSE) START_BULL = START_BULL - 2
	   END IF
	END IF
 
	SAVE_STRING = SEARCH_STRING
	SEARCH_LEN = TRIM(SAVE_STRING)
 
	CALL STR$UPCASE(SEARCH_STRING,SEARCH_STRING)	! Make upper case
 
	IF (IER1.OR.SEARCH_MODE.NE.OLD_SEARCH_MODE.OR.
     &	    REVERSE.OR.REPLY) THEN
	   IF (.NOT.START.AND.SEARCH_MODE.NE.1) THEN  
	      START_BULL = 0	! If starting message not specified, use first
	      IF (REVERSE) START_BULL = NBULL - 1  ! or last
	   END IF
	   IF (REVERSE) THEN
	      END_BULL = 1
	      STEP_BULL = -1
	   ELSE
	      END_BULL = NBULL
	      STEP_BULL = 1
	   END IF
	END IF
 
	IF ((START_BULL+1.GT.NBULL.AND.STEP_BULL.EQ.1).OR.
     &	    (START_BULL+1.EQ.0)) THEN
	   FOUND = -2
	   IF (FILES) CALL CLOSE_BULLDIR
	   CALL CANCEL_CTRLC_AST
	   CALL ENABLE_CTRL
	   RETURN
	END IF
 
	IF (FILES) CALL OPEN_BULLFIL_SHARED
 
	SAVE_BULL_SEARCH = 0
	DO BULL_SEARCH = START_BULL+1, END_BULL, STEP_BULL
	   CALL READDIR(BULL_SEARCH,IER)	! Get bulletin directory entry
	   IF (READ_TAG) THEN
	      IF (STEP_BULL.EQ.-1) THEN
		 CALL GET_THIS_TAG(FOLDER_NUMBER,IER,BULL_SEARCH,DUMMY)
	 	 IF (IER.NE.0) THEN
		    CALL GET_PREVIOUS_TAG(FOLDER_NUMBER,IER,
     &			BULL_SEARCH,DUMMY)
		 END IF
	      ELSE
		 CALL GET_THIS_OR_NEXT_TAG(FOLDER_NUMBER,IER,
     &			BULL_SEARCH,DUMMY)
	      END IF
	      IF (IER.EQ.0) THEN
		 IER = BULL_SEARCH + 1
	      ELSE
		 GO TO 800
	      END IF
	   END IF
	   IF (REMOTE_SET.EQ.3.AND.SAVE_BULL_SEARCH.EQ.BULL_SEARCH) GO TO 800
	   SAVE_BULL_SEARCH = BULL_SEARCH
	   IF (IER.EQ.BULL_SEARCH+1.AND.SEARCH_MODE.NE.2) THEN
	      CALL STR$UPCASE(DESCRIP1,DESCRIP)	! Make upper case
	      IF ((SEARCH_MODE.EQ.3.AND.
     &		  INDEX(DESCRIP1,SEARCH_STRING(:SEARCH_LEN)).GT.0).OR.
     &		  (SEARCH_MODE.EQ.1.AND.(DESCRIP1.EQ.SEARCH_STRING.OR.
     &		  INDEX(SEARCH_STRING,DESCRIP1(5:)).EQ.1))) THEN
		 FOUND = BULL_SEARCH
		 GO TO 900
	      ELSE IF (FLAG.EQ.1) THEN
		 WRITE (6,'('' Search aborted.'')')
		 GO TO 900
	      END IF
	   END IF
	   IF (IER.EQ.BULL_SEARCH+1.AND.SEARCH_MODE.EQ.2) THEN
	      IF (REMOTE_SET) THEN
		 CALL REMOTE_READ_MESSAGE(BULL_SEARCH,IER)
	         IF (IER.GT.0) THEN
	            CALL DISCONNECT_REMOTE
		    GO TO 900
	         ELSE
	            CALL GET_REMOTE_MESSAGE(IER)
		    IF (IER.GT.0) GO TO 900
	         END IF
	      END IF
	      ILEN = LINE_LENGTH + 1
	      DO WHILE (ILEN.GT.0)
	         CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	         CALL STR$UPCASE(INPUT,INPUT)	! Make upper case
		 IF (INDEX(INPUT,SEARCH_STRING(:SEARCH_LEN)).GT.0) THEN
		    FOUND = BULL_SEARCH
		    GO TO 900
		 ELSE IF (FLAG.EQ.1) THEN
		    WRITE (6,'('' Search aborted.'')')
		    GO TO 900
		 END IF
	      END DO
	   END IF
	END DO
 
800	FOUND = 0
 
900	IF (FILES) CALL CLOSE_BULLFIL		! End of bulletin file read
	IF (FILES) CALL CLOSE_BULLDIR
	CALL CANCEL_CTRLC_AST
	CALL ENABLE_CTRL
 
	RETURN
	END
 
 
 
 
	SUBROUTINE UNDELETE
C
C  SUBROUTINE UNDELETE
C
C  FUNCTION: Undeletes deleted message.
C
	IMPLICIT INTEGER (A - Z)
 
	COMMON /POINT/ BULL_POINT
 
	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER
 
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 
	INCLUDE 'BULLDIR.INC'
 
	INCLUDE 'BULLUSER.INC'
 
	INCLUDE 'BULLFOLDER.INC'
 
	EXTERNAL CLI$_ABSENT
 
	IF (REMOTE_SET.EQ.3) THEN
	   WRITE (6,'('' Cannot UNDELETE messages in this folder.'')')
	   RETURN
	END IF
C
C  Get the bulletin number to be undeleted.
C
 
	IER = CLI$GET_VALUE('BULLETIN_NUMBER',BULL_PARAMETER,LEN_P)
	IF (IER.NE.%LOC(CLI$_ABSENT)) THEN	! Was bulletin specified?
	   DECODE(LEN_P,5,BULL_PARAMETER,ERR=920) BULL_DELETE	! Yes
5	   FORMAT(I<LEN_P>)
	ELSE IF (BULL_POINT.EQ.0) THEN	! No.  Have we just read a bulletin?
	   GO TO 910			! No, then error.
	ELSE
	   BULL_DELETE = BULL_POINT	! Delete the file we are reading
	END IF
 
	IF (BULL_DELETE.LE.0) GO TO 920
 
C
C  Check to see if specified bulletin is present, and if the user
C  is permitted to delete the bulletin.
C
 
	CALL OPEN_BULLDIR
 
	CALL READDIR(BULL_DELETE,IER)	! Get info for specified bulletin
 
	IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?
	   WRITE(6,1030)	! If not, then error out
	   GOTO 100
	END IF
 
	IF (USERNAME.NE.FROM) THEN	! If doesn't match owner of bulletin,
	   IF ((.NOT.SETPRV_PRIV().AND..NOT.FOLDER_SET).OR.	! Privileges or
     &	       (.NOT.FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)
     &		.AND.FOLDER_SET)) THEN				! folder owner?
	      WRITE(6,1040)		! Then error out.
	      GO TO 100
	   ELSE
	      CALL READDIR(BULL_DELETE,IER) ! Get info for specified bulletin
	      IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?
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
	      EXDATE = EXDATE(:7)//'20'//EXDATE(10:)
	   END IF
	END IF
 
	IF (.NOT.REMOTE_SET) THEN
	   CALL WRITEDIR(BULL_DELETE,IER)	! Update message expiration date
	   WRITE (6,'('' Message was undeleted.'')')
	ELSE
	   WRITE (REMOTE_UNIT,'(5A)',IOSTAT=IER)
     &      11,BULL_DELETE,DESCRIP,EXDATE,EXTIME
	   IF (IER.EQ.0) THEN
	      READ(REMOTE_UNIT,'(Q,A)',IOSTAT=IER) I,FOLDER1_COM
	   END IF
	   IF (IER.EQ.0) THEN
	      IF (I.NE.LEN(FOLDER1_COM)) THEN
		 WRITE (6,'(1X,A)') FOLDER1_COM(:I)
	      ELSE
	         WRITE (6,'('' Message was undeleted.'')')
	      END IF
	   ELSE
	      CALL DISCONNECT_REMOTE
	   END IF
	END IF
 
100	CALL CLOSE_BULLDIR
 
900	RETURN
 
910	WRITE(6,1010)
	GO TO 900
 
920	WRITE(6,1020)
	GO TO 900
 
1010	FORMAT(' ERROR: You are not reading any message.')
1020	FORMAT(' ERROR: Specified message number has incorrect format.')
1030	FORMAT(' ERROR: Specified message was not found.')
1040	FORMAT(' ERROR: Message was not undeleted. Not owned by you.')
 
	END
 
 
 
	SUBROUTINE ADD_PROTOCOL(INPUT,ILEN)
 
	IMPLICIT INTEGER (A - Z)
 
	INCLUDE 'BULLNEWS.INC'
 
	CHARACTER*20 MAIL_PROTOCOL
 
	CHARACTER*(*) INPUT
 
	DATA LMAIL/0/
 
	IF (LMAIL.EQ.-1) RETURN
 
	IF (INDEX(INPUT,'@').EQ.0.OR.INDEX(INPUT,'%"').GT.0) RETURN
 
	IF (LMAIL.EQ.0) THEN
	   IF (.NOT.SYS_TRNLNM('BULL_NEWS_MAILER',MAIL_PROTOCOL)) THEN
	      MAIL_PROTOCOL = MAILER
	   END IF
	   LMAIL = TRIM(MAIL_PROTOCOL)
	   IF (LMAIL.GT.0.AND.MAIL_PROTOCOL(LMAIL:LMAIL).NE.'%') THEN
	      MAIL_PROTOCOL = MAIL_PROTOCOL(:LMAIL)//'%'
	      LMAIL = LMAIL + 1
	   END IF
	   IF (LMAIL.EQ.0) THEN
	      LMAIL = -1
	      RETURN
	   END IF
	END IF
 
	INPUT = MAIL_PROTOCOL(:LMAIL)//'"'//INPUT(:TRIM(INPUT))//'"'
 
	IF (ILEN.NE.0) ILEN = ILEN + LMAIL + 2
 
	RETURN
	END
