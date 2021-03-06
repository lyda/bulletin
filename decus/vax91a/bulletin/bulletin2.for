C
C  BULLETIN2.FOR, Version 3/20/91
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
	   SHUTDOWN_BTIM(2) = UP_BTIM(2)0
	END IFP
 o
	IF (REMOTE_SET) THENs
	   WRITE(REMOTE_UNIT,'(3A)',IOSTAT=IER1) 14,BTEST(FOLDER_FLAG,2),
     &				NODENAME
	   IF (IER1.NE.0) THEN 
	      CALL DISCONNECT_REMOTE
	      IF (.NOT.FILE_OPENED) CALL CLOSE_BULLUSER
	      RETURNo
	   END IF
	END IFg
 A
	CALL GET_UPTIME(UPDATE,UPTIME)b
 o
	CALL SYS_BINTIM(UPDATE//' '//UPTIME,UP_BTIM) 
 e
	IF (NODE_AREA.EQ.0) THENp
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
	ELSE			! Test to make sure NODE_AREA is zeroI
	   SEEN_FLAG = 0		! if all of SHUTDOWN_FLAG is zero
	   DO I=1,FLONG
	      IF (SHUTDOWN_FLAG(I).NE.0) SEEN_FLAG = 1I
	   END DO
	   IF (SEEN_FLAG.EQ.0) NODE_AREA = 0D
	END IF)
 E
	IF (IER.NE.0) THEN_
	   WRITE (4,IOSTAT=IER)
     &		'*SYSTEM     ',NODENAME,NODE_NUMBER,NODE_AREA,VERSION,
     &		SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG(
	ELSE 
	   REWRITE (4,IOSTAT=IER)
     &		TEMP_USER,NODENAME,NODE_NUMBER,NODE_AREA,VERSION,R
     &		SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG 
	END IF 
 I
	CALL READ_PERME
 B
	IF (.NOT.FILE_OPENED) CALL CLOSE_BULLUSER
 I
	RETURNC
	END
 E
 )
	E
	SUBROUTINE GET_NODE_NUMBER(NODE_NUMBER,NODE_AREA)
 O
	IMPLICIT INTEGER (A-Z)
 C
	INCLUDE '($SYIDEF)'
  
	CALL INIT_ITMLST	! Initialize item list
				! Now add items to list 
	CALL ADD_2_ITMLST(4,SYI$_NODE_AREA,%LOC(NODE_AREA))
	CALL ADD_2_ITMLST(4,SYI$_NODE_NUMBER,%LOC(NODE_NUMBER))
	CALL END_ITMLST(GETSYI_ITMLST)	! Get address of itemlistB
 )
	IER = SYS$GETSYIW(,,,%VAL(GETSYI_ITMLST),,,)	! Get Info command. 
C&
C  NODE_AREA is set to 0 after shutdown messages are deleted.
C  If node is not part of cluster, NODE_AREA will be 0,(
C  so set it to 1 as a dummy value to cause messages to be deleted.F
CE
	IF (NODE_AREA.EQ.0) NODE_AREA = 1
 0
	RETURNC
	END
 F
 E
 I
 T
	SUBROUTINE SET_NODE(NODE_SET)
CF
C  SUBROUTINE SET_NODE
CR
C  FUNCTION: Set or reset remote node specification for selected folder.
C'
	IMPLICIT INTEGER (A-Z):
 c
	INCLUDE 'BULLFOLDER.INC''
 
	INCLUDE 'BULLUSER.INC'L
 
	INCLUDE 'BULLFILES.INC'
 	
	INCLUDE 'BULLDIR.INC'
  
	EXTERNAL CLI$_ABSENT
  
	CHARACTER RESPONSE*1,FOLDER_SAVE*25
 n
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 T
	IF (CLI$PRESENT('FOLDER')) THEN
	   IER = CLI$GET_VALUE('FOLDER',FOLDER1) ! Get folder namee
	   FOLDER_SAVE = FOLDER
	   CALL OPEN_BULLFOLDER_SHARED		! Open folder file
	   CALL READ_FOLDER_FILE_KEYNAME(FOLDER1,IER)
	   IF (IER.EQ.0) THEN
	      IF (.NOT.FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)) THEN
		 WRITE (6,'('' ERROR: No privs to modify folder.'')')r
		 IER = 1
	      END IF 
	   ELSE
	      WRITE (6,'('' ERROR: Specified folder not found.'')')
	   END IF
	   IF (IER.NE.0) THEN
	      CALL READ_FOLDER_FILE_KEYNAME(FOLDER_SAVE,IER)L
	      CALL CLOSE_BULLFOLDER
	      RETURNE
	   END IF
	   CALL CLOSE_BULLFOLDERE
	END IF=
 P
	IF (FOLDER_NUMBER.EQ.0) THENE
	   WRITE (6,'('' Cannot set remote node for GENERAL folder.'')')E
	ELSE IF (FOLDER_NUMBER.LT.0) THEN
	   WRITE (6,'('' Cannot set remote node for this folder.'')')
	ELSE IF (FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)) THEN
	   IF (.NOT.NODE_SET) THEN(
	      IF (INDEX(FOLDER_BBOARD,'*').GT.0) THEN
		 REMOTE_SET_SAVE = REMOTE_SET 
		 REMOTE_SET = .FALSE.B
	         FOLDER_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//E
     &		     FOLDERS
	         CALL OPEN_BULLDIR		! Remove directory file which
	         CALL CLOSE_BULLDIR_DELETE	! contains remote folder nameE
		 REMOTE_SET = REMOTE_SET_SAVE:
	      END IF 
	      FOLDER1_BBOARD = 'NONE'
	      WRITE (6,'('' Remote node setting has been removed.'')')A
	      IF (.NOT.CLI$PRESENT('FOLDER')) REMOTE_SET = .FALSE.R
	   ELSE
	      CALL GET_INPUT_PROMPT(RESPONSE,RLEN, 
     &          'Are you sure you want to make folder '//)
     &	        FOLDER(:TRIM(FOLDER))//
     &		' remote? (Y/N with N as default): ').
	      IF (RESPONSE.NE.'y'.AND.RESPONSE.NE.'Y') THEN
	        WRITE (6,'('' Folder was not modified.'')')
	        RETURNT
	      END IFO
	      IF (.NOT.CLI$GET_VALUE('REMOTENAME',FOLDER1)) THEN
	         FOLDER1 = FOLDER
	      END IFb
	      IER = CLI$GET_VALUE('NODENAME',FOLDER1_BBOARD,FLEN)
	      FOLDER1_BBOARD = '::'//FOLDER1_BBOARD(:FLEN) 
	      CALL CONNECT_REMOTE_FOLDER(READ_ONLY,IER)
	      IF (IER.NE.0) THEN 
	         WRITE (6,'(
     &		  '' ERROR: Folder not accessible on remote node.'')')
	         RETURN
	      ELSEL
	         WRITE (6,'('' Folder has been converted to remote.'')')'
	      END IF 
	      FOLDER_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//
     &		FOLDER
	      REMOTE_SET_SAVE = REMOTE_SETB
	      REMOTE_SET = .FALSE.)
	      CALL OPEN_BULLDIR			! Remove directory file
	      CALL OPEN_BULLFIL			! Remove bulletin fileP
	      CALL CLOSE_BULLFIL_DELETE
	      CALL CLOSE_BULLDIR_DELETE
	      IF (FOLDER.NE.FOLDER1) THEN	! Different remote folder name?
	         CALL OPEN_BULLDIR		! If so, put name in header
		 BULLDIR_HEADER(13:) = FOLDER1	! of directory file. 
		 CALL WRITEDIR_NOCONV(0,IER)
	         CALL CLOSE_BULLDIR
	         FOLDER1_BBOARD = FOLDER1_BBOARD(:FLEN+2)//'*'R
	      END IF_
	      REMOTE_SET = REMOTE_SET_SAVES
	      IF (.NOT.CLI$PRESENT('FOLDER')) REMOTE_SET = .TRUE.
	   END IF
	   CALL OPEN_BULLFOLDER		! Open folder file
	   CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)
	   IF (.NOT.NODE_SET.AND.FOLDER_BBOARD(:2).EQ.'::'m
     &			.AND.BTEST(FOLDER_FLAG,2)) THEN
	      OPEN (UNIT=17,STATUS='UNKNOWN',IOSTAT=IER,L
     &		RECL=256,FILE=FOLDER_BBOARD(3:TRIM(FOLDER_BBOARD))
     &		//'::"TASK=BULLETIN1"')r
	      IF (IER.EQ.0) THEN	! Disregister remote SYSTEM folder
		 WRITE(17,'(2A)',IOSTAT=IER) 14,0I
		 CLOSE (UNIT=17)
	      END IFI
	   END IF
	   FOLDER_BBOARD = FOLDER1_BBOARD
	   IF (NODE_SET) THEN
	      F_NBULL = F1_NBULLR
	      F_NEWEST_BTIM(1) = F1_NEWEST_BTIM(1)M
	      F_NEWEST_BTIM(2) = F1_NEWEST_BTIM(2)A
	      F_NEWEST_NOSYS_BTIM(1) = F1_NEWEST_NOSYS_BTIM(1)U
	      F_NEWEST_NOSYS_BTIM(2) = F1_NEWEST_NOSYS_BTIM(2)
	      FOLDER_FLAG = 0
	      F_EXPIRE_LIMIT = F1_EXPIRE_LIMITU
	   ELSE
	      F_NBULL = 0
	   END IF
	   CALL REWRITE_FOLDER_FILE
	   CALL CLOSE_BULLFOLDERD
	ELSE 
	   WRITE (6,'('' You are not authorized to modify NODE.'')')&
	END IFE
 O
	IF (CLI$PRESENT('FOLDER')) THEN
	   CALL OPEN_BULLFOLDER_SHARED		! Open folder file
	   CALL READ_FOLDER_FILE_KEYNAME(FOLDER_SAVE,IER)
	   CALL CLOSE_BULLFOLDER 
	   FOLDER_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//O
     &		FOLDER
	END IFA
 )
	RETURNL
	END
 T
 O
 S
 N
	SUBROUTINE RESPOND(STATUS)E
CN
C  SUBROUTINE RESPONDE
C'
C  FUNCTION: Sends a mail message in reply to a posted message.E
CM
C  NOTE: Modify the last SPAWN statement to specify the commandH
C	you use to send mail to sites other than via MAIL.
C	If you always use a different command, modify both
C	spawn commands.D
C,
	IMPLICIT INTEGER (A - Z)E
 D
	COMMON /POINT/ BULL_POINT
 =
	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER
 R
	COMMON /EDIT/ EDIT_DEFAULTL
	DATA EDIT_DEFAULT/.FALSE./O
 _
	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD
  
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
  
	INCLUDE 'BULLDIR.INC'
  
	INCLUDE 'BULLFOLDER.INC'(
 1
	CHARACTER FROM_TEST*5,INFROM*(LINE_LENGTH).
 0
	EXTERNAL CLI$_NEGATED,CLI$_ABSENT
 1
	MSG_OWN = .FALSE.
 
	IF (INCMD(:4).EQ.'REPLY') THEN8
	   BULL_PARAMETER = 'mailing list.'
	   IF (CLI$PRESENT('ALL')) THEN
	      BULL_PARAMETER = 'message owner and mailing list.'R
	      MSG_OWN = .TRUE.C
	   END IF
	ELSE IF (INCMD(:4).EQ.'RESP') THEND
	   MSG_OWN = .TRUE.
	   BULL_PARAMETER = 'message owner.'H
	   IF (CLI$PRESENT('LIST'))
     &			BULL_PARAMETER = 'message owner and mailing list.')
	ELSET
	   BULL_PARAMETER = 'mailing list.'
	END IF 
 R
	WRITE (6,'('' Sending message to '',A)')T
     &	   BULL_PARAMETER(:TRIM(BULL_PARAMETER))1
 .
	IF (INCMD(:4).NE.'POST') THEN
	   IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been read
	      WRITE(6,'('' ERROR: You have not read any message.'')')
	      RETURN			! And return
	   END IF
 P
	   CALL OPEN_BULLDIR_SHARED
 A
	   CALL READDIR(BULL_POINT,IER)	! Get info for specified bulletin
  
	   IF (IER.NE.BULL_POINT+1) THEN	! Was bulletin found?
	      WRITE(6,'('' ERROR: Bulletin was not found.'')')O
	      CALL CLOSE_BULLDIR		! If not, then error outM
	      RETURN)
	   END IF
 W
	   CALL CLOSE_BULLDIR
  
	   CALL STR$UPCASE(BULL_PARAMETER,DESCRIP)M
	   IF (BULL_PARAMETER(:3).NE.'RE:') THEN
	      BULL_PARAMETER = 'RE: '//DESCRIPz
	   ELSE
	      BULL_PARAMETER = 'RE:'//DESCRIP(4:)
	   END IF
	END IFG
  
	IF (CLI$PRESENT('SUBJECT')) THENN
	   IER = CLI$GET_VALUE('SUBJECT',BULL_PARAMETER,LEN_P) 
	   IF (LEN_P.GT.LEN(BULL_PARAMETER)-2) THEN
	      WRITE(6,'('' ERROR: Subject limit is 64 characters.'')'),
	      RETURNO
	   END IF
	ELSE IF (INCMD(:4).EQ.'POST') THENA
	   WRITE(6,'('' Enter subject of message:'')')E
	   CALL GET_LINE(BULL_PARAMETER,LEN_P)R
	   IF (LEN_P.LE.0) THEN
	      LEN_P = 0
	      WRITE(6,'('' ERROR: No subject specified.'')').
	      RETURN)
	   END IF
	ELSER
	   WRITE (6,'('' Message will have the subject:'')')N
	   WRITE (6,'(1X,A)') BULL_PARAMETERI
	END IFE
 A
	IF ((CLI$PRESENT('EDIT').OR.EDIT_DEFAULT).AND.	! If /EDIT specified
     &      (CLI$PRESENT('EDIT').NE.%LOC(CLI$_NEGATED))) THEN_
	   EDIT = .TRUE._
	   CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;*')
	ELSEU
	   EDIT = .FALSE.
	END IFY
 T
	TEXT = CLI$PRESENT('EXTRACT')
 )
	LIST = CLI$PRESENT('LIST')G
 Y
	CALL DISABLE_PRIVSn
 c
	ILEN = 0&
  
	FILESPEC = CLI$GET_VALUE('FILESPEC',INPUT,ILEN)
	IF (FILESPEC.NE.%LOC(CLI$_ABSENT)) THEN
	   OPEN (UNIT=4,FILE=INPUT(:ILEN),STATUS='OLD',READONLY,l
     &		 SHARED,IOSTAT=IER,FORM='FORMATTED')
	   IF (IER.NE.0) FILESPEC = .FALSE.
	END IFE
 
	IF (EDIT.AND.(TEXT.OR.FILESPEC)) THEN
	   OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,t
     &		RECL=LINE_LENGTH,STATUS='NEW',CARRIAGECONTROL='LIST')I
 I
	   IF (IER.NE.0) THEN
	      CALL ERRSNS(IDUMMY,IER)
	      CALL SYS_GETMSG(IER)
	      GO TO 900
	   END IF
	ELSE IF (TEXT.AND..NOT.EDIT) THEN
	   WRITE (6,'('' ERROR: Cannot extract text without /EDIT.'')')
	   GO TO 900D
	END IFE
 T
	LENFRO = 0
	IF (CLI$GET_VALUE('CC',INPUT,ILEN)) THENE
	   CALL ADD_PROTOCOL(INPUT,ILEN) 
	   INFROM = INPUT(:ILEN)D
	   LENFRO = ILEN 
	   IF (MSG_OWN) THENS
	      INFROM = INFROM(:LENFRO)//','
	      LENFRO = LENFRO + 1
	   END IF
	END IF.
 0
	IF ((EDIT.AND.TEXT).OR.INCMD(:4).NE.'POST') THENE
	   CALL ENABLE_PRIVSH
	   CALL OPEN_BULLFIL_SHARED
 i
	   ILEN = LINE_LENGTH + 1
 I
	   CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN),
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THENN
	      ILEN = TRIM(INPUT) 
	      IF (EDIT.AND.TEXT) THEN
	         WRITE (3,'(A)') 'In a previous article, '//
     &			INPUT(7:ILEN)//' wrote:'L
	      END IFE
	      IF (MSG_OWN) THEN
	         CALL ADD_PROTOCOL(INPUT(7:),ILEN)n
	         INFROM = INFROM(:LENFRO)//INPUT(7:)E
	         LENFRO = LENFRO + ILEN - 6
	      END IFa
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   ELSE IF (MSG_OWN) THEN
	      CALL ADD_PROTOCOL(FROM,0)
	      INFROM = INFROM(:LENFRO)//FROMD
	      LENFRO = TRIM(FROM) + LENFROR
	   END IF
 =
	   IF (EDIT.AND.TEXT) THEN 
	      IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THEN
	         CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN) 
	      END IFL
	      DO WHILE (ILEN.GT.0)		! Copy bulletin into file
	         IF (CLI$PRESENT('NOINDENT')) THENE
	            WRITE (3,'(A)') INPUT(:ILEN)E
	         ELSE
	            WRITE (3,'(A)') '>'//INPUT(:ILEN)
	         END IF
	         CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)L
	      END DOE
  
	      IF (FILESPEC) THENC
		 WRITE (3,'(A)') ' '
		 IER = 0
		 DO WHILE (IER.EQ.0)
		    READ (4,'(Q,A)',IOSTAT=IER) ILEN,INPUT
		    IF (IER.EQ.0) WRITE (3,'(A)') INPUT(:ILEN)
		 END DO 
		 CLOSE (UNIT=4) 
	         FILESPEC = .FALSE.
	      END IF 
 N
	      CLOSE (UNIT=3)			! Bulletin copy completed)
	   END IF
 T
	   CALL CLOSE_BULLFIL
	   CALL DISABLE_PRIVS
	END IFT
 M
	IF (EDIT.AND.FILESPEC.AND..NOT.TEXT) THEN
	   IER = 0D
	   ICOUNT = 0
	   DO WHILE (IER.EQ.0)E
	      READ (4,'(Q,A)',IOSTAT=IER) ILEN,INPUT 
	      IF (IER.EQ.0) THEN 
		 WRITE (3,'(A)') INPUT(:ILEN)E
		 ICOUNT = ICOUNT + 1
	      END IFT
	   END DO
	   CLOSE (UNIT=4)
	   FILESPEC = .FALSE.
	   IF (ICOUNT.EQ.0) THENo
	      CLOSE (UNIT=3,STATUS='DELETE')L
	   ELSE
	      CLOSE (UNIT=3)h
	   END IF
	END IFr
 t
	IF (LIST.AND.REMOTE_SET.NE.3) THENE
	   SLIST = INDEX(FOLDER_DESCRIP,'<')D
	   IF (SLIST.GT.0) THEN
	      IF (REMOTE_SET.NE.4) THEN
		 INPUT = FOLDER_DESCRIP(SLIST+1:)
		 ILEN = INDEX(INPUT,'>') - 1
		 IF (ILEN.EQ.-1) ILEN = TRIM(INPUT)L
		 INPUT = INPUT(:ILEN)f
	         CALL ADD_PROTOCOL(INPUT,ILEN) 
	         IF (LENFRO.GT.0.AND.INFROM(LENFRO:LENFRO).NE.',') THEN
		    INFROM = INFROM(:LENFRO)//',' 
		    LENFRO = LENFRO + 1I
		 END IFa
	         INFROM = INFROM(:LENFRO)//INPUT(:ILEN)
	         LENFRO = LENFRO + ILEN
	      ELSEE
		 FOLDER1_DESCRIP = E
     &			FOLDER_DESCRIP(SLIST+1:TRIM(FOLDER_DESCRIP)-1)+
		 IF (FOLDER1_DESCRIP(1:1).EQ.'@') THEN
		    WRITE(6,'('' ERROR: Multiple newsgroup feed'',
     &			'' is present.'')')
		    GO TO 900L
		 END IFO
	      END IFl
	   ELSE
	      WRITE (6,'('' ERROR: No list address'',
     &			'' found in folder description.'')')'
	      GO TO 900
	   END IF
	END IF)
 H
	I = 1		! Must change all " to "" in FROM fieldI
	DO WHILE (I.LE.LENFRO)L
	   IF (INFROM(I:I).EQ.'"') THEN
	      INFROM = INFROM(:I)//'"'//INFROM(I+1:)I
	      I = I + 1
	      LENFRO = LENFRO + 1
	   END IF
	   I = I + 1I
	END DO)
 ,
	LEN_P = TRIM(BULL_PARAMETER)E
	I = 1		! Must change all " to "" in SUBJECT field
	DO WHILE (I.LE.LEN_P)
	   IF (BULL_PARAMETER(I:I).EQ.'"') THEN
	      IF (LEN_P.EQ.64) THEN
		 BULL_PARAMETER(I:I) = '`'
	      ELSET
		 BULL_PARAMETER = BULL_PARAMETER(:I)//'"'W
     &				//BULL_PARAMETER(I+1:)
		 I = I + 1
		 LEN_P = LEN_P + 1
	      END IFF
	   END IF
	   I = I + 1P
	END DO 
 1
	IF (EDIT) THEN 
	   CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')A
	   CONTEXT = 0_
	   IER =  LIB$FIND_FILE('SYS$LOGIN:BULL.SCR',INPUT,CONTEXT)
	   IF (TEXT) THEN
	      VERSION = INDEX(INPUT,';') + 1I
	      IF (INPUT(VERSION:VERSION).EQ.'1') THEN
	         CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;*')
	      ELSEM
	         IER = 0
	      END IFB
	   ELSE IF (IER) THEN
	      IER = 0
	   END IF
	   IF (IER.EQ.0) THEN
	      CALL ADD_SIGNATURE(0,'SYS$LOGIN:BULL.SCR',FOLDER_NAME)S
	      IF (REMOTE_SET.GE.3.AND.LIST) THENN
	         CALL NEWS_POST('SYS$LOGIN:BULL.SCR',.FALSE.,IER,
     &			BULL_PARAMETER)
	         IF (IER.EQ.0) THEN
		    WRITE (6,'('' Message successfully posted.'')')a
	         END IF
	      END IF
	      IF (IER.EQ.0.AND.LENFRO.GT.0) THENd
	         CALL RESPOND_MAIL('SYS$LOGIN:BULL.SCR',INFROM(:LENFRO),C
     &			   BULL_PARAMETER(:LEN_P),STATUS)
	      END IFT
	   END IF
	ELSER
	   OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,D
     &		RECL=LINE_LENGTH,STATUS='NEW',CARRIAGECONTROL='LIST')C
	   IF (.NOT.FILESPEC) THENO
	      WRITE (6,'('' Enter message: End with ctrl-z,'',U
     &			 '' cancel with ctrl-c'')')
	      ILEN = LINE_LENGTH + 1		! Length of input line
	      ICOUNT = 0			! Character count counterG
	      DO WHILE (ILEN.GE.0)		! Input until no more input
	         CALL GET_LINE(INPUT,ILEN)	! Get input line
	         IF (ILEN.GT.LINE_LENGTH) THEN	! Input line too longg
		    WRITE(6,'('' ERROR: Input line length > '',I, 
     &			   ''.  Reinput:'')') LINE_LENGTH
	         ELSE IF (ILEN.GE.0) THEN	! If good input line enteredS
		    ICOUNT = ICOUNT + ILEN		! Update counter
		    WRITE(3,'(A)') INPUT(:ILEN)	! Save line in scratch filen
	         END IF
	      END DO,
	   ELSE
	      IER = 0
	      ICOUNT = 0U
	      DO WHILE (IER.EQ.0)
		 READ (4,'(Q,A)',IOSTAT=IER) ILEN,INPUTN
		 IF (IER.EQ.0) THEN.
		    ICOUNT = ICOUNT + 1s
		    WRITE (3,'(A)') INPUT(:ILEN)
		 END IF 
	      END DOs
	      CLOSE (UNIT=4)R
	      FILESPEC = .FALSE.F
	   END IF
	   IF (ILEN.EQ.-1.OR.ICOUNT.EQ.0) THEN	! CTRL_C or No lines
	      CLOSE (UNIT=3) 
	      IER = 1
	   ELSE
	      CALL ADD_SIGNATURE(3,' ',FOLDER_NAME)
	      REWIND (UNIT=3)
	      IF (REMOTE_SET.GE.3.AND.LIST) THENL
	         CALL NEWS_POST('SYS$LOGIN:BULL.SCR',.TRUE.,IER,
     &			   BULL_PARAMETER)E
	         IF (IER.EQ.0)  WRITE (6,'('' Message successfully posted.'')')
	      ELSEN
		 IER = 0
	      END IF_
	      CLOSE (UNIT=3)C
	      IF (IER.EQ.0.AND.LENFRO.GT.0) THEN:
		 CALL RESPOND_MAIL('SYS$LOGIN:BULL.SCR',INFROM(:LENFRO),
     &			   BULL_PARAMETER(:LEN_P),STATUS)
	      END IFR
	   END IF
	END IFF
	IF (IER.NE.0) WRITE (6,'('' ERROR: No message added.'')')
 O
900	CALL ENABLE_PRIVSc
	IF (FILESPEC) CLOSE (UNIT=4)O
	CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;*')E
 
	RETURN(
	END
 t
 s
 e
	SUBROUTINE ADD_SIGNATURE(FILEUNIT,FILENAME,FOLDER_NAME)
CR
C  SUBROUTINE ADD_SIGNATURE 
C 
C  FUNCTION: Adds signature to message being mailed/posted.'
C.
	IMPLICIT INTEGER (A-Z) 
 
	CHARACTER*(*) FOLDER_NAME
 g
	CHARACTER*128 BULL_SIGNATURE
	DATA BULL_SIGNATURE /'SYS$LOGIN:BULL_SIGNATURE.TXT'/I
 (
	CHARACTER*255 INPUT
 I
	OPEN (UNIT=4,FILE=BULL_SIGNATURE,STATUS='OLD',READONLY,
     &		 SHARED,IOSTAT=IER,FORM='FORMATTED')
 D
	IF (IER.NE.0) THENL
	   OPEN (UNIT=4,FILE='BULL_SIGNATURE',STATUS='OLD',READONLY,A
     &		    SHARED,IOSTAT=IER,FORM='FORMATTED'))
	END IFS
  
	IF (IER.NE.0) RETURNY
 C
	IF (FILEUNIT.EQ.0) THEN
	   OPEN (UNIT=3,FILE=FILENAME,STATUS='OLD',ACCESS='APPEND',
     &		 IOSTAT=IER,FORM='FORMATTED')H
	END IFP
 (
	ICOUNT = 0N
	MATCH = .FALSE.
	DO WHILE (IER.EQ.0)
	   READ (4,'(A)',IOSTAT=IER) INPUTD
	   ILEN = TRIM(INPUT)
	   DO WHILE (.NOT.MATCH.AND.STREQ(INPUT(:6),'START ').AND.IER.EQ.0)
	      MATCH = STREQ(INPUT(7:ILEN),FOLDER_NAME(:TRIM(FOLDER_NAME)))L
	      READ (4,'(A)',IOSTAT=IER) INPUT
	      ILEN = TRIM(INPUT).
	      IF (.NOT.MATCH) THENS
	         DO WHILE (.NOT.STREQ(INPUT(:ILEN),'END').AND.IER.EQ.0)
		    READ (4,'(A)',IOSTAT=IER) INPUTH
	            ILEN = TRIM(INPUT)o
		 END DOe
		 READ (4,'(A)',IOSTAT=IER) INPUT
	         ILEN = TRIM(INPUT)
	      END IFV
	   END DO
	   IF (IER.EQ.0) THEN
	      IF (MATCH.AND.STREQ(INPUT(:ILEN),'END')) THEN
	         MATCH = .FALSE. 
	      ELSET
	         ICOUNT = ICOUNT + 1E
	         IF (ICOUNT.EQ.1) WRITE (3,'(A)',IOSTAT=IER) ' '.
	         WRITE (3,'(A)',IOSTAT=IER) INPUT(:ILEN)T
	      END IFE
	   END IF
	END DOL
 E
	CLOSE (UNIT=4)
	IF (FILEUNIT.EQ.0) CLOSE (UNIT=3)
  
	RETURNB
	END
 B
 K
 N
 ,
	LOGICAL FUNCTION STREQ(INPUT,INPUT1)U
 6
	IMPLICIT INTEGER (A-Z) 
 I
	CHARACTER*(*) INPUT,INPUT1 
 I
	STREQ = .FALSE.
  
	IF (LEN(INPUT).NE.LEN(INPUT1)) RETURN
 e
	DO I=1,LEN(INPUT)
	   DIFF = ABS(ICHAR(INPUT(I:I))-ICHAR(INPUT1(I:I)))
	   IF (DIFF.NE.0.AND.DIFF.NE.32) RETURN
	END DOL
 n
	STREQ = .TRUE.M
 I
	RETURNN
	END
 U
 :
 
  
  
 E
	SUBROUTINE RESPOND_MAIL(FILE,SENDTO,SUBJECT,STATUS)
C_
C  SUBROUTINE RESPOND_MAIL
CE
C  FUNCTION: Sends mail to address. 
CL
	IMPLICIT INTEGER (A-Z) 
  
	INCLUDE 'BULLUSER.INC'/
 O
	INCLUDE 'BULLFOLDER.INC'O
 +
	CHARACTER*(*) FILE,SENDTO,SUBJECT
 A
	CHARACTER MAILER*128I
 I
	LISTSERV = INDEX(FOLDER_DESCRIP,'LISTSERV').GT.0 
 L
	IF (LISTSERV) THENL
	   CALL SETUSER(FOLDER_BBOARD)L
	   IF (SYS_TRNLNM('MX_NODE_NAME',MAILER)) THENt
	      REPLY_TO = .NOT.SYS_TRNLNM('MX_REPLY_TO',MAILER) 
	      IF (REPLY_TO) IER = LIB$SET_LOGICAL
     &			('MX_REPLY_TO',USERNAME(:TRIM(USERNAME)))
	   ELSE
	      REPLY_TO = .NOT.SYS_TRNLNM('PMDF_REPLY_TO',MAILER)N
	      IF (REPLY_TO) IER = LIB$SET_LOGICAL
     &			('PMDF_REPLY_TO',USERNAME(:TRIM(USERNAME)))
	   END IF
	END IF
  
	IF (SYS_TRNLNM('BULL_MAILER',MAILER)) THEN 
	   IF (LISTSERV) THEN
	      IF (SYS_TRNLNM_SYSTEM('BULL_MAILER',MAILER)) THEN
	         CALL LIB$SPAWN('@'//MAILER(:TRIM(MAILER))//O
     &		    ' SYS$LOGIN:BULL.SCR "'//SENDTO//'" "'//SUBJECT 
     &		    //'" '//USERNAME(:TRIM(USERNAME)),,,,,,STATUS)
	      END IFE
	   ELSE
	      CALL LIB$SPAWN('@'//MAILER(:TRIM(MAILER))//
     &			     ' SYS$LOGIN:BULL.SCR "'//SENDTO//=
     &			     '" "'//SUBJECT//'"',,,,,,STATUS)
	   END IF
	ELSEP
	   CALL LIB$SPAWN('$MAIL SYS$LOGIN:BULL.SCR "'//SENDTO// 
     &		      '"/SUBJECT="'//SUBJECT//'"',,,,,,STATUS)
	END IF.
 T
	IF (LISTSERV) THENN
	   CALL SETUSER(USERNAME)
	   IF (REPLY_TO) IER = LIB$DELETE_LOGICAL('PMDF_REPLY_TO')L
	   IF (REPLY_TO) IER = LIB$DELETE_LOGICAL('MX_REPLY_TO')S
	END IFD
  
	RETURNS
	END
 H
 
  
	INTEGER FUNCTION CONFIRM_USER(USERNAME)
CE
C  FUNCTION CONFIRM_USER
C 
C  FUNCTION: Confirms that username is valid user.
CI
	IMPLICIT INTEGER (A-Z):
 N
	CHARACTER*(*) USERNAMEO
 O
	CALL OPEN_SYSUAF_SHARED
 (
	READ (8,KEY=USERNAME,IOSTAT=CONFIRM_USER)
 H
	CALL CLOSE_SYSUAF
 O
	RETURN)
	END
 	
  
 F
 =
 N
	SUBROUTINE REPLACE
C 
C  SUBROUTINE REPLACEL
CR
C  FUNCTION: CHANGE command subroutine.R
C 
	IMPLICIT INTEGER (A - Z)E
 D
	COMMON /POINT/ BULL_POINT
 C
	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER
  
	COMMON /EDIT/ EDIT_DEFAULTR
 u
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 )
	COMMON /LAST_RECORD_WRITTEN/ OCOUNT
 E
	INCLUDE 'BULLDIR.INC'
 I
	INCLUDE 'BULLUSER.INC' 
 r
	INCLUDE 'BULLFOLDER.INC'n
 l
	CHARACTER INEXDATE*11,INEXTIME*11
	CHARACTER INDESCRIP*(LINE_LENGTH),INFROM*(LINE_LENGTH) 
	CHARACTER*1 ANSWERI
 D
	CHARACTER DATE_SAVE*11,TIME_SAVE*11
 I
	INTEGER TIMADR(2)
 I
	EXTERNAL CLI$_ABSENT,CLI$_NEGATED
 
	LOGICAL*1 DOALL
  
	IF (REMOTE_SET.EQ.3) THEN
	   WRITE (6,'('' Cannot CHANGE messages in this folder.'')')T
	   RETURN
	END IFc
 g
Cl
C  Get the bulletin number to be replaced.
C_
 
	ALL = CLI$PRESENT('ALL').
 '
	IER1 = CLI$GET_VALUE('NUMBER',BULL_PARAMETER,LEN_P)
	IF (IER1.EQ.%LOC(CLI$_ABSENT).AND..NOT.ALL) THENU
	   IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been read
	      WRITE (6,1005)		! Tell user of the error 
	      RETURN			! and return
	   END IF
	   SBULL = BULL_POINT		! Replace the bulletin we are reading 
	   EBULL = SBULL 
  
	   CALL OPEN_BULLDIR_SHARED
	   CALL READDIR(BULL_POINT,IER)		! Get message directory entryX
	   CALL CLOSE_BULLDIR
	   IF (IER.NE.BULL_POINT+1) THEN	! Was message found?
	      WRITE(6,'('' ERROR: Specified message was not found.'')')
	      RETURN 
	   END IF
	ELSEI
	   CALL OPEN_BULLDIR_SHARED
	   CALL READDIR(0,IER)		! Get message directory entry
	   CALL CLOSE_BULLDIR
	   IF (NBULL.EQ.0) THEN		! Were messages found?
	      WRITE(6,'('' ERROR: No messages were found.'')')R
	      RETURN
	   END IF
 A
	   IF (IER1.NE.%LOC(CLI$_ABSENT)) THEN	
	      CALL GET_2_VALS(BULL_PARAMETER,LEN_P,SBULL,EBULL,IER1) 
	      IF (SBULL.LE.0.OR.IER1.NE.0) THEN
	         WRITE (6,'(A)') 
     &		  ' ERROR: Specified message number has incorrect format.'
	         RETURN
	      END IFS
	      ALL = .TRUE.T
	   ELSE IF (CLI$PRESENT('ALL')) THENI
	      SBULL = 1
	      EBULL = NBULL
	   END IF
	END IFH
 A
	IF (CLI$PRESENT('SYSTEM')) THEN
	   IF (.NOT.SETPRV_PRIV()) THEN
	    WRITE (6,'(
     &	     '' ERROR: Not enough privileges to change to system.'')'))
	    RETURNE
	   ELSE IF (.NOT.BTEST(FOLDER_FLAG,2).AND.FOLDER_NUMBER.NE.0) THENh
	    WRITE (6,'(
     &       '' ERROR: /SYSTEM cannot be set with selected folder.'')') 
	    RETURN(
	   END IF
	END IFu
 i
	IF (CLI$PRESENT('SHUTDOWN')) THEN
	   IF (.NOT.SETPRV_PRIV()) THEN
	    WRITE (6,'(
     &	     '' ERROR: Not enough privileges to change to shutdown.'')')G
	    RETURN 
	   ELSE IF (.NOT.BTEST(FOLDER_FLAG,2).AND.FOLDER_NUMBER.NE.0) THEN=
	    WRITE (6,'(
     &      '' ERROR: /SHUTDOWN cannot be set with selected folder.'')')
	    RETURN 
	   ELSE IF (CLI$GET_VALUE('SHUTDOWN',BULL_PARAMETER).NE.I
     &		    %LOC(CLI$_ABSENT).AND.REMOTE_SET) THEN
	    WRITE (6,'('' ERROR: Shutdown node name not'',.
     &			    '' permitted for remote folder.'')') 
	    RETURN
	   END IF
	END IFD
 s
	IF (CLI$PRESENT('PERMANENT').AND.
     &		F_EXPIRE_LIMIT.GT.0.AND..NOT. ! Expiration limit present
     &		FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)) THEN
	   WRITE (6,'( 
     &	    '' ERROR: Not enough privileges to change to permanent.'')') 
	   RETURN
	END IFL
C)
C  Check to see if specified bulletin is present, and if the user
C  is permitted to replace the bulletin.
CE
 Q
	CALL OPEN_BULLDIR_SHAREDs
 e
	SAME_OWNER = .TRUE.
	DO I=SBULL,EBULLR
	   CALL READDIR(I,IER)	! Get info for specified messagesR
	   IF (USERNAME.NE.FROM) SAME_OWNER = .FALSE. L
	END DOI
	CALL READDIR(SBULL,IER)
 
	CALL CLOSE_BULLDIRM
 R
	IF (.NOT.SAME_OWNER) THEN	! If doesn't match owner of bulletin,
	   IF ((.NOT.SETPRV_PRIV().AND..NOT.FOLDER_SET).OR.	! Privileges or
     &	       (.NOT.FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)
     &		.AND.FOLDER_SET)) THEN				! folder owner?S
	      WRITE(6,1090)		! If not, then error out.N
	      RETURNB
	   ELSE
	      WRITE (6,1100)		! Make sure user wants to delete it
	      READ (5,'(A)',IOSTAT=IER) ANSWER	! Get his answer
	      CALL STR$UPCASE(ANSWER,ANSWER)	! Convert input to uppercase
	      IF (ANSWER.NE.'Y') RETURN	! If not Yes, then exit
	   END IF
	END IFO
  
CI
C  If no switches were given, replace the full bulletinS
CE
 O
	DOALL = .FALSE.
 T
	TEXT = CLI$PRESENT('TEXT')L
  
	IF ((.NOT.CLI$PRESENT('EXPIRATION')).AND.
     &	   (.NOT.CLI$PRESENT('GENERAL')).AND.
     &	   (.NOT.CLI$PRESENT('SYSTEM')).AND.N
     &	   (.NOT.CLI$PRESENT('HEADER')).AND. 
     &	   (.NOT.CLI$PRESENT('SUBJECT')).AND.
     &	   (.NOT.TEXT).AND.
     &	   (.NOT.CLI$PRESENT('SHUTDOWN')).AND. 
     &	   (.NOT.CLI$PRESENT('PERMANENT'))) THEN 
	   DOALL = .TRUE.
	END IFU
 
	IF (SBULL.NE.EBULL.AND.(DOALL.OR.TEXT)) THENH
	   WRITE (6,'('' ERROR: Cannot change text when replacing'',R
     &		      '' more than one messsage.'')'))
	   RETURN
	END IFA
 I
	CALL DISABLE_CTRL			! Disable CTRL-Y & -C
  
	PERMANENT = .FALSE.
	IF (CLI$PRESENT('EXPIRATION').OR.DOALL) THENN
	   SYSTEM = 0
	   CALL GET_EXPIRED(INPUT,IER)T
	   PERMANENT = BTEST(SYSTEM,1) 
	   IF (.NOT.IER) GO TO 910R
	   INEXDATE = INPUT(:11)P
	   INEXTIME = INPUT(13:)N
	END IF 
 E
8	LENDES = 0
	IF (CLI$PRESENT('HEADER').OR.DOALL) THENT
	   WRITE(6,1050)			! Request header for bulletinA
	   READ(5,'(Q,A)',END=910,ERR=910) LENDES,INDESCRIP
	   IF (LENDES.EQ.0) GO TO 910		! If no header, don't add bull
	ELSE IF (CLI$PRESENT('SUBJECT')) THEN
	   IER = CLI$GET_VALUE('SUBJECT',INDESCRIP,LENDES)
	END IFE
 N
	IF (LENDES.GT.0) THEN
	   INDESCRIP = 'Subj: '//INDESCRIPD
	   LENDES = MIN(LENDES+6,LEN(INDESCRIP))N
	END IF1
 
	IF (SBULL.NE.EBULL) CALL OPEN_BULLDIR
 R
	DO NUMBER=SBULL,EBULL
	 NUMBER_PARAM = NUMBER 
	 IF (SBULL.NE.EBULL) THEN
	   CALL READDIR(NUMBER_PARAM,IER)
	   IF (IER.NE.NUMBER_PARAM+1) THEN	! Couldn't find messageF
	      CALL CLOSE_BULLDIRN
	      WRITE(6,'('' ERROR: Message '',I6,'' cannot be found.'')') 
     &			NUMBER_PARAME
	      WRITE(6,'('' All messages up to that message were modified.'')')E
	      RETURNS
	   END IF
	 END IF
 L
	 REC1 = 0
 E
	 LENFROM = 0
 C
	 IF (LENDES.GT.0.OR.TEXT.OR.DOALL) THEN
	   OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,
     &	     RECL=LINE_LENGTH,STATUS='SCRATCH',CARRIAGECONTROL='LIST'))
 .
	   IF (IER.NE.0) THEN
	      CALL ERRSNS(IDUMMY,IER)
	      CALL SYS_GETMSG(IER)X
	      GO TO 910
	   END IF
  
	   CALL OPEN_BULLFIL_SHARED
 E
	   REC1 = 1
 
	   ILEN = LINE_LENGTH + 1
 S
	   CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)M
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THENT
	      INFROM = INPUT(:ILEN)
	      LENFROM = ILENI
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   END IF
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THENN
	      IF (LENDES.EQ.0.AND..NOT.DOALL) THENE
		 INDESCRIP = INPUT(:ILEN)N
		 LENDES = ILEN
	      END IFH
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   END IF
 	
	   DO WHILE (ILEN.GT.0)		! Copy bulletin into file
	      WRITE (3,'(A)') INPUT(:ILEN)E
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   END DO
 W
	   CALL CLOSE_BULLFIL
 )
	   IF (TEXT.OR.DOALL) CLOSE(UNIT=3)
	 END IF
 =
	 IF (TEXT.OR.DOALL) THENE
C/
C  If file specified in REPLACE command, read file to obtain bulletin.
C  Else, read the bulletin from the terminal.E
C"
	S
	  ICOUNT = 0				! Line count for bulletin
	  LAST_NOBLANK = 0			! Last line with data
	  REC1 = 1_
  
	  IER = CLI$GET_VALUE('FILESPEC',BULL_PARAMETER,LEN_P)_
	  IF (IER.NE.%LOC(CLI$_ABSENT).OR.	! If file param in ADD command
     &	    ((CLI$PRESENT('EDIT').OR.EDIT_DEFAULT).AND.	! or /EDIT specified 
     &       (CLI$PRESENT('EDIT').NE.%LOC(CLI$_NEGATED)))) THENe
 
	   IF ((CLI$PRESENT('EDIT').OR.EDIT_DEFAULT).AND. ! If /EDIT specifiedN
     &       (CLI$PRESENT('EDIT').NE.%LOC(CLI$_NEGATED))) THEN
	      IF (LEN_P.EQ.0) THEN		! If no file param specified
		 IF (.NOT.CLI$PRESENT('NEW')) THEN
	            OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='NEW',I
     &		       RECL=LINE_LENGTH,
     &		       ERR=920,FORM='FORMATTED',CARRIAGECONTROL='LIST')P
	            CALL OPEN_BULLFIL_SHARED	! Prepare to copy message
		    ILEN = LINE_LENGTH + 1
		    CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)T
		    IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN'
		       CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
		    END IF
		    IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THEN)
		       CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
		    END IF
		    DO WHILE (ILEN.GT.0)	! Copy message into fileR
		       WRITE (3,'(A)') INPUT(:ILEN)C
		       CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
		    END DO
		    CALL CLOSE_BULLFIL
	            CLOSE (UNIT=3)		! Bulletin copy completed
		 END IFb
		 CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')
	      ELSE 
	         CALL DISABLE_PRIVS
		 CALL MAILEDIT(BULL_PARAMETER(:LEN_P),'SYS$LOGIN:BULL.SCR')O
	      END IF 
	      IER = LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;-1')e
	      OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='OLD',R
     &		 DISPOSE='DELETE',ERR=920,FORM='FORMATTED')I
	   ELSE IF (LEN_P.GT.0) THENr
	      CALL DISABLE_PRIVSL
	      OPEN (UNIT=3,FILE=BULL_PARAMETER(:LEN_P),STATUS='OLD',N
     &		READONLY,SHARED,ERR=920,FORM='FORMATTED') ! Try opening the file
	   END IF
 +
	   CALL ENABLE_PRIVS			! Reset SYSPRV privilegesR
 :
	   DO WHILE(1)				! Read until end of file to
	      READ (3,'(Q,A)',END=10) ILEN,INPUT	! get record count
	      IF (ILEN.GT.LINE_LENGTH) GO TO 950y
	      CALL STR$TRIM(INPUT,INPUT,ILEN)
	      IF (ILEN.GT.0) THEN		! If good input line entered
		 ICOUNT = ICOUNT + ILEN + 1	! Increment record count
		 LAST_NOBLANK = ICOUNT
	      ELSE IF (ILEN.EQ.0) THEN
		 IF (ICOUNT.GT.0) THEN
		    ICOUNT = ICOUNT + 2		! COPY_BULL writes a line with.
		 ELSE				! 1 space for a blank line.
		    REC1 = REC1 + 1O
		 END IFd
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
	      IF (ILEN.GT.LINE_LENGTH) THEN	! Line too long..
		 WRITE(6,'('' ERROR: Input line length > '',I,
     &			''. Reinput::'')') LINE_LENGTH(
	      ELSE IF (ILEN.GT.0) THEN		! If good input line enteredR
		 ICOUNT = ICOUNT + 1 + ILEN	! Increment character countR
		 WRITE(3,'(A)') INPUT(:ILEN)	! Save line in scratch file
		 LAST_NOBLANK = ICOUNT
	      ELSE IF (ILEN.EQ.0.AND.ICOUNT.GT.0) THENb
		 WRITE(3,'(A)') INPUT(:ILEN)	! Save line in scratch file
		 ICOUNT = ICOUNT + 2		! COPY_BULL writes a line with
	      END IF				! 1 space for a blank line.
	   END DO
	   IF (ILEN.EQ.-1) GO TO 910		! CTRL_C entered, error out
10	   ICOUNT = LAST_NOBLANK 
	   IF (ICOUNT.EQ.0) GO TO 910		! No lines entered, error outM
	  ENDIF
 
	 END IF
 I
CL
C  Add bulletin to bulletin file and directory entry for to directory file.E
CL
 _
	 DATE_SAVE = DATE
	 TIME_SAVE = TIME
	 INPUT = DESCRIPR
 :
	 IF (SBULL.EQ.EBULL) THEN
	  CALL OPEN_BULLDIR			! Prepare to add dir entry
	  CALL READDIR(NUMBER_PARAM,IER)	! Get info for message
 u
	  IF (IER.NE.NUMBER_PARAM+1.OR.DATE.NE.DATE_SAVE.OR.
     &	     TIME.NE.TIME_SAVE.OR.INPUT.NE.DESCRIP) THEN 
				! If message disappeared, try to find it.n
	   IF (IER.NE.NUMBER_PARAM+1) DATE = ' 'E
	   NUMBER_PARAM = 0
	   IER = 1E
	   DO WHILE (IER.EQ.NUMBER_PARAM+1.AND.
     &	    (DATE.NE.DATE_SAVE.OR.TIME.NE.TIME_SAVE.OR.DESCRIP.NE.INPUT))
	      NUMBER_PARAM = NUMBER_PARAM + 1
	      CALL READDIR(NUMBER_PARAM,IER) 
	   END DO
  
	   IF (IER.NE.NUMBER_PARAM+1) THEN	! Couldn't find message 
	      CALL CLOSE_BULLDIR	
	      CLOSE (UNIT=3,STATUS='SAVE')9
	      WRITE(6,'('' ERROR: Message has been deleted'',
     &			'' by another user.'')')e
	      IF (DOALL.OR.TEXT) THEN
		 WRITE (6,'('' New text has been saved in'',
     &				'' SYS$LOGIN:BULL.SCR.'')')R
	      END IFu
	      GO TO 100
	   END IF
	  END IF 
	 END IF
 o
	 CALL READDIR(0,IER)			! Get directory header
 I
	 IF (REC1.GT.0) THEN			! If text has been replaced
 
	   CALL OPEN_BULLFIL			! Prepare to add bulletin'
 
	   BLOCK = NBLOCK + 1
	   BLOCK_SAVE = BLOCK
	   NEMPTY = NEMPTY + LENGTH
 R
	   OBLOCK = BLOCK
	   IF (LENFROM.GT.0) THEN
	      CALL STORE_BULL(LENFROM,INFROM(:LENFROM),OBLOCK) 
	   END IF
	   IF (LENDES.GT.0) THENN
	      CALL STORE_BULL(LENDES,INDESCRIP(:LENDES),OBLOCK)
	   END IF
	   REWIND (UNIT=3).
	   CALL COPY_BULL(3,REC1,OBLOCK,IER)	! Add the new bulletin
	   IF (IER.NE.0) THEN		! Error in creating bulletin
	      WRITE (6,'(A)') ' ERROR: Unable to replace message.'g
	      CALL CLOSE_BULLFIL 
	      CALL CLOSE_BULLDIR 
	      CLOSE (UNIT=3)
	      GO TO 100
	   END IF
 R
	   LENGTH_SAVE = OCOUNT - BLOCK + 1
	   NBLOCK = NBLOCK + LENGTH_SAVEL
 T
	   IF (.NOT.REMOTE_SET) CALL WRITEDIR(0,IER)T
 R
	   CALL CLOSE_BULLFIL
 Y
	   IF (.NOT.REMOTE_SET) THENT
	    CALL READDIR(NUMBER_PARAM,IER)	! Get directory entry:
	    LENGTH = LENGTH_SAVE		! Update size
	    BLOCK = BLOCK_SAVE)
	    CALL WRITEDIR(NUMBER_PARAM,IER)	! Write new directory entry
	   END IF
	 ELSE
	   CALL READDIR(NUMBER_PARAM,IER)
	 END IF
 G
	 IF (.NOT.REMOTE_SET) THEN'
 d
	   IF (LENDES.GT.0.OR.DOALL) THEN
	      DESCRIP=INDESCRIP(7:59)		! Update description headerS
	   END IF
	   CALL UPDATE_DIR_HEADER((CLI$PRESENT('EXPIRATION').OR.DOALL).AND.
     &		.NOT.PERMANENT,CLI$PRESENT('PERMANENT').OR.PERMANENT,U
     &		CLI$PRESENT('SHUTDOWN'),INEXDATE,INEXTIME)
	   IF (CLI$PRESENT('SYSTEM')) THENF
	      SYSTEM = IBSET(SYSTEM,0)R
	   ELSE IF (CLI$PRESENT('GENERAL')) THENM
	      SYSTEM = IBCLR(SYSTEM,0) 
	   END IF
	   CALL WRITEDIR(NUMBER_PARAM,IER)E
	 ELSE
	   MSGTYPE = 0,
	   IF (CLI$PRESENT('SYSTEM').OR.&
     &		(BTEST(SYSTEM,0).AND..NOT.CLI$PRESENT('GENERAL'))) THENs
	      MSGTYPE = IBSET(MSGTYPE,0)T
	   END IF
	   IF (CLI$PRESENT('PERMANENT').OR.PERMANENT) THENC
	      MSGTYPE = IBSET(MSGTYPE,1)L
	   ELSE IF (CLI$PRESENT('SHUTDOWN')) THEN
	      MSGTYPE = IBSET(MSGTYPE,2)L
	   ELSE IF ((CLI$PRESENT('EXPIRATION').OR.DOALL)'
     &		    .AND..NOT.PERMANENT) THENC
	      MSGTYPE = IBSET(MSGTYPE,3) 
	   END IF
	   IF (LENDES.EQ.0.AND..NOT.DOALL) INDESCRIP(7:) = DESCRIP_
	   IF (CLI$PRESENT('EXPIRATION').OR.DOALL) THEN
	      EXDATE = INEXDATE
	      EXTIME = INEXTIME
	   END IF
	   WRITE (REMOTE_UNIT,'(7A)',IOSTAT=IER)T
     &      10,DESCRIP,NUMBER_PARAM,INDESCRIP(7:59),MSGTYPE,
     &	    EXDATE,EXTIME
	   IF (IER.EQ.0) THEN
	      READ(REMOTE_UNIT,'(Q,A)',IOSTAT=IER) I,FOLDER1_COME
	   END IF
	   IF (IER.EQ.0) THEN
	      IF (I.NE.LEN(FOLDER1_COM)) THEN
		 WRITE (6,'(1X,A)') FOLDER1_COM(:I) 
	      END IFU
	   ELSE
	      CALL DISCONNECT_REMOTE 
	   END IF
	 END IF
	END DO)
  
	CALL CLOSE_BULLDIR		! Totally finished with replace
 L
	CLOSE (UNIT=3) 
 _
100	CALL ENABLE_CTRL		! Enable CTRL-Y & -C
	RETURN 
 A
910	WRITE(6,1010))
	CLOSE (UNIT=3,ERR=100)C
	GOTO 100)
  
920	WRITE(6,1020)E
	CALL ENABLE_PRIVS	! Reset SYSPRV privileges
	GOTO 100o
 n
950	WRITE (6,1030) LINE_LENGTH
	CLOSE (UNIT=3)h
	GO TO 100
  
1000	FORMAT (' Enter message: End with ctrl-z, cancel with ctrl-c') 
1005	FORMAT (' ERROR: You are not reading any message.')
1010	FORMAT (' No message was replaced.')P
1015	FORMAT (' ERROR: Specified message was not found.')
1020	FORMAT (' ERROR: Unable to open specified file.')
1030	FORMAT (' ERROR: Line length in file exceeds '',I,'' characters.')N
1050	FORMAT (' Enter description header.')
1090	FORMAT(' ERROR: Specified message is not owned by you.') 
1100	FORMAT(' Message(s) is not owned by you.',%
     &	       ' Are you sure you want to replace it? ',$) 
2020	FORMAT(1X,A)f
 
	END
 .
 .
 $
	SUBROUTINE UPDATE_DIR_HEADER(EXPIRE,PERM,SHUT,INEXDATE,INEXTIME)L
 R
	IMPLICIT INTEGER (A-Z)	
  
	INCLUDE 'BULLDIR.INC'
 &
	EXTERNAL CLI$_ABSENTF
 A
	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD
 B
	CHARACTER TODAY*23,INEXDATE*11,INEXTIME*11 
 L
	IF (EXPIRE) THEN1
	   SYSTEM = IBCLR(SYSTEM,1)
	   SYSTEM = IBCLR(SYSTEM,2)
	   EXDATE=INEXDATE			! Update expiration date
	   EXTIME=INEXTIMEL
	   DIFF = COMPARE_DATE(EXDATE,NEWEST_EXDATE)	! Compare expiration
	   IF (DIFF.EQ.0) DIFF = COMPARE_TIME(EXTIME,NEWEST_EXTIME)
	   IF (DIFF.LT.0) THEN			! If it's oldest expiration bull
	      NEWEST_EXDATE = EXDATE		! Update the header in)
	      NEWEST_EXTIME = EXTIME		! the directory fileG
	      CALL WRITEDIR(0,IER)
	   END IF
	ELSE IF (PERM.AND.(.NOT.BTEST(SYSTEM,1))) THENe
	   IF (BTEST(SYSTEM,2)) THEN
	      SYSTEM = IBCLR(SYSTEM,2)S
	      SHUTDOWN = SHUTDOWN - 1
	      CALL WRITEDIR(0,IER) 
	   END IF
	   SYSTEM = IBSET(SYSTEM,1)
	   EXDATE = '5-NOV-2000' 
	   EXTIME = '00:00:00.00'
	ELSE IF (SHUT.AND.(.NOT.BTEST(SYSTEM,2))) THEN,
	   SYSTEM = IBSET(SYSTEM,2)
	   SYSTEM = IBCLR(SYSTEM,1)
	   EXDATE = '5-NOV-2000'T
	   NODE_AREA = 0 
	   IF (INCMD(:4).EQ.'REPL') THENA
	      IF (CLI$GET_VALUE('SHUTDOWN',NODE_NAME)
     &		    .NE.%LOC(CLI$_ABSENT)) THENN
		 CALL GET_NODE_NUMBER_OTHER(NODE_NUMBER,NODE_AREA,NODE_NAME)
	         IF (NODE_AREA.EQ.0) THEN
		    WRITE (6,'('' ERROR: Shutdown node name ignored.'',n
     &		               '' Invalid node name specified.'')')	
		 END IFd
	      END IFI
	   END IF
	   IF (NODE_AREA.EQ.0) CALL GET_NODE_NUMBER(NODE_NUMBER,NODE_AREA) 
	   WRITE (EXTIME,'(I4)') NODE_NUMBERi
	   WRITE (EXTIME(7:),'(I4)') NODE_AREA	
	   DO I=1,11o
	      IF (EXTIME(I:I).EQ.' ') EXTIME(I:I) = '0'
	   END DO
	   EXTIME = EXTIME(1:2)//':'//EXTIME(3:4)//':'//O
     &		    EXTIME(7:8)//'.'//EXTIME(9:10)
	   SHUTDOWN = SHUTDOWN + 1k
	   CALL SYS$ASCTIM(,TODAY,,)		! Get the present timeI
	   SHUTDOWN_DATE = TODAY(:11)
	   SHUTDOWN_TIME = TODAY(13:)
	   CALL WRITEDIR(0,IER)
	END IFS
 ,
	RETURN
	END
 D
 O
 '
 E
	SUBROUTINE SEARCH(READ_COUNT)
CT
C  SUBROUTINE SEARCH
CR
C  FUNCTION: Search for bulletin with specified string
C0
	IMPLICIT INTEGER (A - Z)t
 o
	COMMON /POINT/ BULL_POINT
 G
	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER
  
	CHARACTER*132 SEARCH_STRING
 I
	START_BULL = BULL_POINT
 
	IF (CLI$PRESENT('START')) THEN		! Starting message specifiedE
	   CALL CLI$GET_VALUE('START',BULL_PARAMETER,LEN_P)
	   DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER) START_BULL
	   IF (.NOT.CLI$PRESENT('REPLY')) START_BULL = START_BULL - 1
	END IFm
  
	IER1 = CLI$GET_VALUE('SEARCH_STRING',SEARCH_STRING,SEARCH_LEN)c
 c
	CALL GET_SEARCH(FOUND,SEARCH_STRING,START_BULL,
     &		CLI$PRESENT('REVERSE'),CLI$PRESENT('SUBJECT'),
     &		CLI$PRESENT('REPLY'),.TRUE.,CLI$PRESENT('START'))2
  
	IF (FOUND.GT.0) THEN 
	   BULL_POINT = FOUND - 1
	   CALL READ_MSG(READ_COUNT,BULL_POINT+1) ! Read next bulletin	
	ELSE IF (FOUND.EQ.0) THEN
	   WRITE (6,'('' No messages found with given search string.'')')
	ELSE IF (FOUND.EQ.-2) THEN 
	   WRITE (6,'('' ERROR: No more messages.'')')b
	END IFi
 a
	RETURNo
	END
 f
 t
 i
 t
	SUBROUTINE GET_SEARCH(FOUND,SEARCH_STRING,START_BULL,REVERSE,
     &				SUBJECT,REPLY,FILES,START)
C 
C  SUBROUTINE GET_SEARCH
C!
C  FUNCTION: Search for bulletin with specified string
CR
	IMPLICIT INTEGER (A - Z)u
  
	INCLUDE 'BULLDIR.INC'
 O
	CHARACTER*(*) SEARCH_STRING
  
	COMMON /CTRLC_FLAG/ FLAGP
 N
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
  
	CHARACTER*132 SAVE_STRING
	DATA SAVE_STRING/' '/
 E
	COMMON /NEXT/ NEXT0
  
	CHARACTER*53 DESCRIP1
 E
	FOUND = -1A
 +
	CALL DISABLE_CTRL
 E
	CALL DECLARE_CTRLC_ASTT
 _
	IF (TRIM(SEARCH_STRING).EQ.0) THENM
	   IER1 = .FALSE.
	ELSE1
	   IER1 = .TRUE.I
	END IFP
	M
	IF (.NOT.IER1.AND..NOT.REPLY.AND.
     &      (SUBJECT.OR.SEARCH_MODE.NE.1)) THEN
						! If no search string enteredO
	   SEARCH_STRING = SAVE_STRING		! use saved search string
	   IF (TRIM(SAVE_STRING).EQ.0) THEN
	      WRITE (6,'('' No search string present.'')')
	      CALL CANCEL_CTRLC_AST
	      CALL ENABLE_CTRL	
	      RETURNB
	   END IF
	   IF (STEP_BULL.EQ.-1) START_BULL = START_BULL - 2
	ELSE IF (.NOT.IER1.AND.SEARCH_MODE.EQ.1) THEN
	   SEARCH_STRING = SAVE_STRING		! use saved search string
	END IFr
 a
	IF (FILES) CALL OPEN_BULLDIR_SHARED
 r
	CALL READDIR(0,IER)
  
	OLD_SEARCH_MODE = SEARCH_MODE
	IF (IER1) THEN				! If string entered
	   IF (SUBJECT) THENO
	      SEARCH_MODE = 3
	   ELSE
	      SEARCH_MODE = 2
	   END IF
	ELSE IF (SUBJECT.AND.SEARCH_MODE.NE.3) THEN
	   SEARCH_MODE = 3C
	ELSE IF (REPLY) THENN
	   CALL READDIR(START_BULL,IER)
	   IF (START_BULL+1.NE.IER) THENC
	      WRITE (6,'('' ERROR: No message being read.'')')F
	      IF (FILES) CALL CLOSE_BULLDIR
	      CALL CANCEL_CTRLC_AST
	      CALL ENABLE_CTRLl
	      RETURN
	   ELSE
	      SEARCH_MODE = 1
	      SEARCH_STRING = DESCRIP
	      IF (REVERSE) START_BULL = START_BULL - 2 
	   END IF
	END IFT
 B
	SAVE_STRING = SEARCH_STRING
	SEARCH_LEN = TRIM(SAVE_STRING)R
 T
	CALL STR$UPCASE(SEARCH_STRING,SEARCH_STRING)	! Make upper case 
 (
	IF (IER1.OR.SEARCH_MODE.NE.OLD_SEARCH_MODE.OR.P
     &	    REVERSE.OR.REPLY) THEN 
	   IF (.NOT.START.AND.SEARCH_MODE.NE.1) THEN  K
	      START_BULL = 0	! If starting message not specified, use first
	      IF (REVERSE) START_BULL = NBULL - 1  ! or last_
	   END IF
	   IF (REVERSE) THENT
	      END_BULL = 1d
	      STEP_BULL = -1R
	   ELSE
	      END_BULL = NBULLR
	      STEP_BULL = 1
	   END IF
	END IF 
  
	IF ((START_BULL+1.GT.NBULL.AND.STEP_BULL.EQ.1).OR.O
     &	    (START_BULL+1.EQ.0)) THEN
	   FOUND = -2
	   IF (FILES) CALL CLOSE_BULLDIR&
	   CALL CANCEL_CTRLC_ASTN
	   CALL ENABLE_CTRL
	   RETURN
	END IFE
 )
	IF (FILES) CALL OPEN_BULLFIL_SHARED
 
	SAVE_BULL_SEARCH = 0T
	DO BULL_SEARCH = START_BULL+1, END_BULL, STEP_BULL
	   CALL READDIR(BULL_SEARCH,IER)	! Get bulletin directory entry
	   IF (REMOTE_SET.EQ.3.AND.SAVE_BULL_SEARCH.EQ.BULL_SEARCH) GO TO 800
	   SAVE_BULL_SEARCH = BULL_SEARCH
	   IF (IER.EQ.BULL_SEARCH+1.AND.SEARCH_MODE.NE.2) THENF
	      CALL STR$UPCASE(DESCRIP1,DESCRIP)	! Make upper case
	      IF ((SEARCH_MODE.EQ.3.AND. 
     &		  INDEX(DESCRIP1,SEARCH_STRING(:SEARCH_LEN)).GT.0).OR.
     &		  (SEARCH_MODE.EQ.1.AND.(DESCRIP1.EQ.SEARCH_STRING.OR.
     &		  INDEX(SEARCH_STRING,DESCRIP1(5:)).EQ.1))) THEN
		 FOUND = BULL_SEARCH
		 GO TO 900
	      ELSE IF (FLAG.EQ.1) THEN 
		 WRITE (6,'('' Search aborted.'')')T
		 GO TO 900
	      END IFE
	   END IF
	   IF (IER.EQ.BULL_SEARCH+1.AND.SEARCH_MODE.EQ.2) THENM
	      IF (REMOTE_SET) THEN
		 CALL REMOTE_READ_MESSAGE(BULL_SEARCH,IER)
	         IF (IER.GT.0) THEN
	            CALL DISCONNECT_REMOTE 
		    GO TO 900U
	         ELSE
	            CALL GET_REMOTE_MESSAGE(IER)(
		    IF (IER.GT.0) GO TO 900.
	         END IF
	      END IF(
	      ILEN = LINE_LENGTH + 1 
	      DO WHILE (ILEN.GT.0) 
	         CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN))
	         CALL STR$UPCASE(INPUT,INPUT)	! Make upper case
		 IF (INDEX(INPUT,SEARCH_STRING(:SEARCH_LEN)).GT.0) THENC
		    FOUND = BULL_SEARCH1
		    GO TO 900
		 ELSE IF (FLAG.EQ.1) THENT
		    WRITE (6,'('' Search aborted.'')')
		    GO TO 900S
		 END IFl
	      END DOo
	   END IF
	END DO3
 L
800	FOUND = 0O
 (
900	IF (FILES) CALL CLOSE_BULLFIL		! End of bulletin file read
	IF (FILES) CALL CLOSE_BULLDIR
	CALL CANCEL_CTRLC_AST
	CALL ENABLE_CTRLn
 e
	RETURN
	END
 A
 '
  
 s
	SUBROUTINE UNDELETE
C5
C  SUBROUTINE UNDELETE
C 
C  FUNCTION: Undeletes deleted message.E
CR
	IMPLICIT INTEGER (A - Z)f
 .
	COMMON /POINT/ BULL_POINT
 l
	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER
 p
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 a
	INCLUDE 'BULLDIR.INC'
 
	INCLUDE 'BULLUSER.INC'i
 o
	INCLUDE 'BULLFOLDER.INC' 
  
	EXTERNAL CLI$_ABSENTn
 o
	IF (REMOTE_SET.EQ.3) THEN
	   WRITE (6,'('' Cannot UNDELETE messages in this folder.'')')X
	   RETURN
	END IFT
CN
C  Get the bulletin number to be undeleted.I
CU
 '
	IER = CLI$GET_VALUE('BULLETIN_NUMBER',BULL_PARAMETER,LEN_P)
	IF (IER.NE.%LOC(CLI$_ABSENT)) THEN	! Was bulletin specified?X
	   DECODE(LEN_P,5,BULL_PARAMETER,ERR=920) BULL_DELETE	! Yes
5	   FORMAT(I<LEN_P>)M
	ELSE IF (BULL_POINT.EQ.0) THEN	! No.  Have we just read a bulletin?
	   GO TO 910			! No, then error.A
	ELSE(
	   BULL_DELETE = BULL_POINT	! Delete the file we are readingD
	END IFP
 _
	IF (BULL_DELETE.LE.0) GO TO 920
 I
CL
C  Check to see if specified bulletin is present, and if the user=
C  is permitted to delete the bulletin.N
CS
 X
	CALL OPEN_BULLDIR
 r
	CALL READDIR(BULL_DELETE,IER)	! Get info for specified bulletin
 M
	IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?M
	   WRITE(6,1030)	! If not, then error out
	   GOTO 100
	END IFT
 N
	IF (USERNAME.NE.FROM) THEN	! If doesn't match owner of bulletin,S
	   IF ((.NOT.SETPRV_PRIV().AND..NOT.FOLDER_SET).OR.	! Privileges or
     &	       (.NOT.FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)
     &		.AND.FOLDER_SET)) THEN				! folder owner?-
	      WRITE(6,1040)		! Then error out.N
	      GO TO 100
	   ELSE
	      CALL READDIR(BULL_DELETE,IER) ! Get info for specified bulletin
	      IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?E
	         WRITE(6,1030)		! If not, then error outN
	         GOTO 100
	      END IFw
	   END IF
	END IF'
 
	IF ((SYSTEM.AND.7).LE.1) THEN	! General or System message
	   EXDATE = EXDATE(:7)//'19'//EXDATE(10:)
	ELSE				! Permanent or Shutdown
	   IF (EXDATE(2:2).EQ.'-') THEN
	      EXDATE = EXDATE(:6)//'20'//EXDATE(9:)
	   ELSE
	      EXDATE = EXDATE(:7)//'20'//EXDATE(10:) 
	   END IF
	END IF 
 E
	IF (.NOT.REMOTE_SET) THEN
	   CALL WRITEDIR(BULL_DELETE,IER)	! Update message expiration dateI
	   WRITE (6,'('' Message was undeleted.'')')D
	ELSEk
	   WRITE (REMOTE_UNIT,'(5A)',IOSTAT=IER)p
     &      11,BULL_DELETE,DESCRIP,EXDATE,EXTIME
	   IF (IER.EQ.0) THEN
	      READ(REMOTE_UNIT,'(Q,A)',IOSTAT=IER) I,FOLDER1_COM
	   END IF
	   IF (IER.EQ.0) THEN
	      IF (I.NE.LEN(FOLDER1_COM)) THEN
		 WRITE (6,'(1X,A)') FOLDER1_COM(:I) 
	      ELSEi
	         WRITE (6,'('' Message was undeleted.'')')T
	      END IFG
	   ELSE
	      CALL DISCONNECT_REMOTE
	   END IF
	END IFR
 T
100	CALL CLOSE_BULLDIR
 C
900	RETURN
 S
910	WRITE(6,1010)I
	GO TO 900
 I
920	WRITE(6,1020)T
	GO TO 900
  
1010	FORMAT(' ERROR: You are not reading any message.')A
1020	FORMAT(' ERROR: Specified message number has incorrect format.')L
1030	FORMAT(' ERROR: Specified message was not found.')U
1040	FORMAT(' ERROR: Message was not undeleted. Not owned by you.')R
 ,
	END
 N
 
 
	SUBROUTINE ADD_PROTOCOL(INPUT,ILEN)
 T
	IMPLICIT INTEGER (A - Z)(
 V
	INCLUDE 'BULLNEWS.INC'C
 ,
	CHARACTER*20 MAIL_PROTOCOL)
 R
	CHARACTER*(*) INPUT
 2
	DATA LMAIL/0/
 .
	IF (LMAIL.EQ.-1) RETURN
 U
	IF (INDEX(INPUT,'@').EQ.0.OR.INDEX(INPUT,'%"').GT.0) RETURN
 l
	IF (LMAIL.EQ.0) THEN.
	   IF (.NOT.SYS_TRNLNM('BULL_NEWS_MAILER',MAIL_PROTOCOL)) THEN 
	      MAIL_PROTOCOL = MAILERQ
	   END IF
	   LMAIL = TRIM(MAIL_PROTOCOL)m
	   IF (LMAIL.GT.0.AND.MAIL_PROTOCOL(LMAIL:LMAIL).NE.'%') THEN
	      MAIL_PROTOCOL = MAIL_PROTOCOL(:LMAIL)//'%',
	      LMAIL = LMAIL + 1
	   END IF
	   IF (LMAIL.EQ.0) THEN
	      LMAIL = -1 
	      RETURNc
	   END IF
	END IFp
 f
	INPUT = MAIL_PROTOCOL(:LMAIL)//'"'//INPUT(:TRIM(INPUT))//'"'I
 N
	IF (ILEN.NE.0) ILEN = ILEN + LMAIL + 2
 M
	RETURNC
	END
