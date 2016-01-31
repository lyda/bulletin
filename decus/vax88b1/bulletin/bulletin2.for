C
C  BULLETIN2.FOR, Version 8/3/88
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Programmer: Mark R. London
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

	CHARACTER EXPIRE*3,INPUT_BBOARD*12,TODAY*23

	IF (TRIM(BBOARD_DIRECTORY).EQ.0) THEN
	 WRITE(6,'('' ERROR: System programmer has disabled BBOARD.'')')
	 RETURN
	END IF

	IF (FOLDER_OWNER.EQ.USERNAME.OR.SETPRV_PRIV()) THEN

	   CALL OPEN_FILE(7)		! Open folder file
	   CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)

	   IF (FOLDER_BBOARD(:2).EQ.'::') THEN
	      WRITE (6,'(
     &		'' ERROR: Cannot set BBOARD for remote folder.'')')
	      CALL CLOSE_FILE(7)
	      RETURN
	   END IF

	   IF (BBOARD) THEN
	      IER = CLI$GET_VALUE('BB_USERNAME',INPUT_BBOARD,INPUT_LEN)
	      IF (IER.NE.%LOC(CLI$_ABSENT)) THEN
		 CALL GET_UAF
     &		   (INPUT_BBOARD,USERB,GROUPB,ACCOUNTB,FLAGS,IER1)
		 CALL CLOSE_FILE(7)
	         IF (IER1.AND..NOT.BTEST(FLAGS,UAI$V_DISACNT)) THEN ! DISUSER?
	            WRITE (6,'
     &		    ('' ERROR: BBOARD account needs DISUSER flag set.'')')
		    RETURN
		 ELSE IF (IER1.AND.BTEST(USERB,31)) THEN
		    WRITE (6,'('' ERROR: User number of UIC cannot '',
     &				''be greater than 7777777777.'')')
		    RETURN
		 END IF
		 CALL OPEN_FILE(7)
		 CALL READ_FOLDER_FILE_TEMP(IER)
		 DO WHILE ((FOLDER1_BBOARD.NE.INPUT_BBOARD.OR.
     &		     FOLDER1_NUMBER.EQ.FOLDER_NUMBER).AND.IER.EQ.0)
		   CALL READ_FOLDER_FILE_TEMP(IER)
	         END DO
		 IF (FOLDER1_BBOARD.EQ.INPUT_BBOARD.AND.
     &		      FOLDER1_NUMBER.NE.FOLDER_NUMBER) THEN
		    WRITE (6,'(
     &		     '' ERROR: Account used by other folder.'')')
		    CALL CLOSE_FILE(7)
		    RETURN
		 END IF
		 IF (.NOT.IER1) THEN
		    WRITE (6,'('' WARNING: BBOARD account not in SYSUAF'',
     &			'' file.  Assuming mail forwarding entry.'')')
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
		 CALL OPEN_FILE(4)
		 CALL SYS$ASCTIM(,TODAY,,)		! Get the present time
		 CALL READ_USER_FILE_HEADER(IER)
		 CALL SYS_BINTIM(TODAY,BBOARD_BTIM)
		 REWRITE (4) USER_HEADER
		 CALL CLOSE_FILE(4)
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
		    CALL CLOSE_FILE(7)
		    RETURN
		 ELSE IF (TEMP.LE.0) THEN
		    WRITE (6,'('' ERROR: Expiration must be > 0.'')')
		    CALL CLOSE_FILE(7)
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
	   CALL CLOSE_FILE(7)
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
	ELSE IF (SETPRV_PRIV()) THEN
	   CALL OPEN_FILE(7)		! Open folder file
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
	   CALL CLOSE_FILE(7)
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

	INTEGER SHUTDOWN_BTIM(FLONG)

	EQUIVALENCE (SHUTDOWN_BTIM,BRIEF_FLAG)

	CHARACTER UPDATE*11,UPTIME*8

	INTEGER UP_BTIM(2)

	IF (.NOT.FILE_OPENED) CALL OPEN_FILE(4)

	DO WHILE (REC_LOCK(IER))
	   READ (4,KEY='*SYSTEM',IOSTAT=IER) 
     &		TEMP_USER,NODENAME,NODE_NUMBER,NODE_AREA,NEW_FLAG,
     &		SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG
	END DO

	IF (IER.NE.0) THEN
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
	   NEW_FLAG(1) = 152
	END IF

	IF (NEW_FLAG(1).NE.152) THEN
	   CALL CLOSE_FILE(7)
	   CALL OPEN_FILE(7)
	   NODE_AREA = 0
	   DO I=1,FLONG
	      SYSTEM_FLAG(I) = 0
	   END DO
	   IER1 = 0
	   DO WHILE (IER1.EQ.0)
	      CALL READ_FOLDER_FILE_TEMP(IER1)
	      IF (BTEST(FOLDER1_FLAG,2)) THEN
		 CALL SET2(SYSTEM_FLAG,FOLDER1_NUMBER)
	      END IF
	   END DO
	   NEW_FLAG(1) = 152
	END IF

	IF (BTEST(FOLDER_FLAG,2)) THEN
	   CALL SET2(SYSTEM_FLAG,FOLDER_NUMBER)
	ELSE
	   CALL CLR2(SYSTEM_FLAG,FOLDER_NUMBER)
	END IF

	IF (REMOTE_SET) THEN
	   WRITE(REMOTE_UNIT,'(3A)',IOSTAT=IER1) 14,BTEST(FOLDER_FLAG,2),
     &				NODENAME
	   IF (IER1.NE.0) THEN
	      CALL DISCONNECT_REMOTE
	      IF (.NOT.FILE_OPENED) CALL CLOSE_FILE(4)
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
     &			'*SYSTEM',NODENAME,NODE_NUMBER,NODE_AREA,NEW_FLAG,
     &			SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG
	ELSE
	   REWRITE (4,IOSTAT=IER)
     &			TEMP_USER,NODENAME,NODE_NUMBER,NODE_AREA,NEW_FLAG,
     &			SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG
	END IF

	IF (.NOT.FILE_OPENED) CALL CLOSE_FILE(4)

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

	EXTERNAL CLI$_ABSENT

	CHARACTER RESPONSE*1,FOLDER_SAVE*25

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	IF (CLI$PRESENT('FOLDER')) THEN
	   IER = CLI$GET_VALUE('FOLDER',FOLDER1) ! Get folder name
	   FOLDER_SAVE = FOLDER
	   CALL OPEN_FILE_SHARED(7)		! Open folder file
	   CALL READ_FOLDER_FILE_KEYNAME(FOLDER1,IER)
	   IF (IER.EQ.0) THEN
	      IF (FOLDER_OWNER.NE.USERNAME.AND..NOT.SETPRV_PRIV()) THEN
		 WRITE (6,'('' ERROR: No privs to modify folder.'')')
		 IER = 1
	      END IF
	   ELSE
	      WRITE (6,'('' ERROR: Specified folder not found.'')')
	   END IF
	   IF (IER.NE.0) THEN
	      CALL READ_FOLDER_FILE_KEYNAME(FOLDER_SAVE,IER)
	      CALL CLOSE_FILE(7)
	      RETURN
	   END IF
	   CALL CLOSE_FILE(7)
	END IF

	IF (FOLDER_NUMBER.EQ.0) THEN
	   WRITE (6,'('' Cannot set remote node for GENERAL folder.'')')
	ELSE IF (FOLDER_OWNER.EQ.USERNAME.OR.SETPRV_PRIV()) THEN
	   IF (.NOT.NODE_SET) THEN
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
	      FOLDER1 = FOLDER
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
	      CALL OPEN_FILE(2)			! Remove directory file
	      CALL OPEN_FILE(1)			! Remove bulletin file
	      CALL CLOSE_FILE_DELETE(1)
	      CALL CLOSE_FILE_DELETE(2)
	      IF (.NOT.CLI$PRESENT('FOLDER')) REMOTE_SET = .TRUE.
	   END IF
	   CALL OPEN_FILE(7)		! Open folder file
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
	   CALL CLOSE_FILE(7)
	ELSE
	   WRITE (6,'('' You are not authorized to modify NODE.'')')
	END IF

	IF (CLI$PRESENT('FOLDER')) THEN
	   CALL OPEN_FILE_SHARED(7)		! Open folder file
	   CALL READ_FOLDER_FILE_KEYNAME(FOLDER_SAVE,IER)
	   CALL CLOSE_FILE(7)
	   FOLDER_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//
     &		FOLDER
	END IF

	RETURN
	END




	SUBROUTINE RESPOND(STATUS)
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

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /EDIT/ EDIT_DEFAULT
	DATA EDIT_DEFAULT/.FALSE./

	CHARACTER INPUT*80,FROM_TEST*5

	INCLUDE 'BULLDIR.INC'

	EXTERNAL CLI$_NEGATED

	IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been read
	   WRITE(6,'('' ERROR: You have not read any message.'')')
	   RETURN			! And return
	END IF

	BULL_PARAMETER = 'RE: '//DESCRIP
	IF (CLI$PRESENT('SUBJECT')) THEN
	   IER = CLI$GET_VALUE('SUBJECT',BULL_PARAMETER,LEN_P)
	   IF (LEN_P.GT.LEN(BULL_PARAMETER)-2) THEN
	      WRITE(6,'('' ERROR: Subject limit is 64 characters.'')')
	      RETURN
	   END IF
	END IF

	LEN_P = TRIM(BULL_PARAMETER)

	IF (BULL_PARAMETER(:1).NE.'"') THEN
	   BULL_PARAMETER = '"'//BULL_PARAMETER(:LEN_P)
	   LEN_P = LEN_P + 1
	END IF

	IF (BULL_PARAMETER(LEN_P:LEN_P).NE.'"') THEN
	   BULL_PARAMETER = BULL_PARAMETER(:LEN_P)//'"'
	   LEN_P = LEN_P + 1
	END IF

	IF ((CLI$PRESENT('EDIT').OR.EDIT_DEFAULT).AND.	! If /EDIT specified
     &      (CLI$PRESENT('EDIT').NE.%LOC(CLI$_NEGATED))) THEN
	   EDIT = .TRUE.
	ELSE
	   EDIT = .FALSE.
	END IF

	IF (EDIT.AND.CLI$PRESENT('TEXT')) THEN
	   OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,
     &		STATUS='NEW',CARRIAGECONTROL='LIST')

	   IF (IER.NE.0) THEN
	      CALL ERRSNS(IDUMMY,IER)
	      CALL SYS_GETMSG(IER)
	      RETURN
	   END IF

	   CALL OPEN_FILE_SHARED(1)

	   ILEN = 81
	   DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file
	      DO WHILE (ILEN.GT.0)
	         CALL GET_BULL(I,INPUT,ILEN)
	         IF (ILEN.LT.0) THEN
		    GO TO 90
	         ELSE IF (ILEN.GT.0) THEN
		    IF (CLI$PRESENT('NOINDENT')) THEN
	               WRITE (3,'(A)') INPUT(:ILEN)
		    ELSE
	               WRITE (3,'(A)') '>'//INPUT(:MIN(79,ILEN))
		       IF (ILEN.EQ.80) WRITE (3,'(A)') '>'//INPUT(80:)
		    END IF
	         END IF
	      END DO
	      ILEN = 80
	   END DO

90	   CLOSE (UNIT=3)			! Bulletin copy completed
	   CALL CLOSE_FILE(1)
	END IF

	IF (CONFIRM_USER(FROM).EQ.0) THEN
	   CALL DISABLE_PRIVS
	   IF (EDIT) THEN
	      CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')
	      CALL LIB$SPAWN('$MAIL SYS$LOGIN:BULL.SCR '//FROM
     &		//'/SUBJECT='//BULL_PARAMETER,,,,,,STATUS)
	   ELSE
	      CALL LIB$SPAWN('$MAIL SYS$INPUT '//FROM//'/SUBJECT='//
     &		BULL_PARAMETER,,,,,,STATUS)
	   END IF
	   CALL ENABLE_PRIVS
	ELSE
	   FROM_TEST = ' '
	   CALL OPEN_FILE_SHARED(1)	! Prepare to copy message
	   L_INPUT = 81
	   I = BLOCK
	   DO WHILE (I.LT.BLOCK+LENGTH.AND.L_INPUT.GT.0)
	      CALL GET_BULL(I,INPUT,L_INPUT)
	      IF (L_INPUT.GT.0) THEN
		 CALL STR$UPCASE(FROM_TEST,INPUT(:5))
		 IF (FROM_TEST.EQ.'FROM:') THEN
		    IF (INDEX(INPUT,'.').GT.0.OR.INDEX(INPUT,'@').GT.0
     &			.OR.INDEX(INPUT,'%').GT.0) THEN
		       L_INPUT = 0
		    END IF
		 END IF
	      ELSE IF (L_INPUT.EQ.0) THEN
	         L_INPUT = 80
	         I = I + 1
	      END IF
	   END DO
	   CALL CLOSE_FILE(1)
	   IF (FROM_TEST.EQ.'FROM:') THEN
	      L_B = INDEX(INPUT,'<')
	      R_B = INDEX(INPUT,'>')
	      IF (L_B.GT.0.AND.R_B.GT.0) THEN
		 INPUT = INPUT(L_B+1:R_B-1)
		 L_INPUT = R_B - 1 - L_B
	      ELSE
		 L_INPUT = TRIM(INPUT)
		 I = 6
		 DO WHILE (INPUT(I:I).EQ.' '.AND.I.GT.0)
		    I = I + 1
		    IF (I.GT.L_INPUT) I = 0
		 END DO
		 INPUT = INPUT(I:L_INPUT)
		 L_INPUT = L_INPUT - I + 1
	      END IF
	      CALL DISABLE_PRIVS
C
C The commented lines contain modifications to interace with PMDF
C using the in% syntax - Jim Gerland 29-Dec-1987
C
C              K = INDEX (Input, '%')
C              If (K .GT. 0) Then
C                 Input = Input (K+1:L_Input)
C                 L_Input = l_Input - K
C              End If
C
	      IF (EDIT) THEN
	         CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')
C
C For PMDF, uncomment the following lines and deleted the 3 lines
C in the actual code.
C
C                 CALL LIB$SPAWN ('$MAIL SYS$LOGIN:BULL.SCR "IN%"'
C     &		//INPUT(:L_INPUT)//
C     &          '""/SUBJECT='//BULL_PARAMETER,,,,,,STATUS)
C
	         CALL LIB$SPAWN('$CHMAIL SYS$LOGIN:BULL.SCR "'
     &			//INPUT(:L_INPUT)//
     &		   '@XX"/SUBJECT='//BULL_PARAMETER,,,,,,STATUS)
	      ELSE
C
C For PMDF, uncomment the following lines and deleted the 2 lines
C in the actual code.
C
C                 CALL LIB$SPAWN ('$MAIL SYS$INPUT "IN%"'
C     &		//INPUT(:L_INPUT)//
C     &          '""/SUBJECT='//BULL_PARAMETER,,,,,,STATUS)
C
	         CALL LIB$SPAWN('$CHMAIL/I "'//INPUT(:L_INPUT)//
     &		   '@XX"/SUBJECT='//BULL_PARAMETER,,,,,,STATUS)
	      END IF
	      CALL ENABLE_PRIVS
	   ELSE
	      CALL DISABLE_PRIVS
	      IF (EDIT) THEN
	         CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')
	         CALL LIB$SPAWN('$MAIL SYS$LOGIN:BULL.SCR '//FROM
     &		//'/SUBJECT='//BULL_PARAMETER,,,,,,STATUS)
	      ELSE
	         CALL LIB$SPAWN('$MAIL SYS$INPUT '//FROM//'/SUBJECT='
     &		//BULL_PARAMETER,,,,,,STATUS)
	      END IF
	      CALL ENABLE_PRIVS
	   END IF
	END IF

	IF (EDIT) THEN
	   CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;*')
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

	CALL OPEN_FILE_SHARED(8)

	READ (8,KEY=USERNAME,IOSTAT=CONFIRM_USER)

	CALL CLOSE_FILE(8)

	RETURN
	END






	SUBROUTINE REPLACE
C
C  SUBROUTINE REPLACE
C
C  FUNCTION: Replaces existing bulletin to bulletin file.
C
	IMPLICIT INTEGER (A - Z)

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /EDIT/ EDIT_DEFAULT

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	CHARACTER INEXDATE*11,INEXTIME*11
	CHARACTER INDESCRIP*80,INPUT*80
	CHARACTER*1 ANSWER

	CHARACTER DATE_SAVE*11,TIME_SAVE*11

	INTEGER TIMADR(2)

	EXTERNAL CLI$_ABSENT,CLI$_NEGATED

	LOGICAL*1 DOALL

C
C  Get the bulletin number to be replaced.
C
	IF (.NOT.CLI$PRESENT('NUMBER')) THEN	! No number has been specified
	   IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been read
	      WRITE (6,1005)		! Tell user of the error
	      RETURN			! and return
	   END IF
	   NUMBER_PARAM = BULL_POINT	! Replace the bulletin we are reading
	ELSE
	   CALL CLI$GET_VALUE('NUMBER',BULL_PARAMETER,LEN_P)
	   DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER) NUMBER_PARAM
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
	   END IF
	END IF

	IF (CLI$PRESENT('PERMANENT').AND.
     &      .NOT.FOLDER_SET.AND..NOT.SETPRV_PRIV()) THEN
	   WRITE (6,'(
     &	    '' ERROR: Not enough privileges to change to permanent.'')')
	   RETURN
	END IF
C
C  Check to see if specified bulletin is present, and if the user
C  is permitted to replace the bulletin.
C

	CALL OPEN_FILE_SHARED(2)

	CALL READDIR(NUMBER_PARAM,IER)	! Get info for specified bulletin

	CALL CLOSE_FILE(2)

	IF (IER.NE.NUMBER_PARAM+1) THEN	! Was bulletin found?
	   WRITE (6,1015)		! If not, tell the person
	   RETURN			! and error out
	END IF

	IF (USERNAME.NE.FROM) THEN	! If doesn't match owner of bulletin,
	   IF ((.NOT.SETPRV_PRIV().AND..NOT.FOLDER_SET).OR.    ! Privileges or
     &	       (.NOT.SETPRV_PRIV().AND.
     &		USERNAME.NE.FOLDER_OWNER.AND.FOLDER_SET)) THEN ! folder owner?
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

	IF ((.NOT.CLI$PRESENT('EXPIRATION')).AND.
     &	   (.NOT.CLI$PRESENT('GENERAL')).AND.
     &	   (.NOT.CLI$PRESENT('SYSTEM')).AND.
     &	   (.NOT.CLI$PRESENT('HEADER')).AND.
     &	   (.NOT.CLI$PRESENT('SUBJECT')).AND.
     &	   (.NOT.CLI$PRESENT('TEXT')).AND.
     &	   (.NOT.CLI$PRESENT('SHUTDOWN')).AND.
     &	   (.NOT.CLI$PRESENT('PERMANENT'))) THEN
	   DOALL = .TRUE.
	END IF

	CALL DISABLE_CTRL			! Disable CTRL-Y & -C

	IF (CLI$PRESENT('EXPIRATION').OR.DOALL) THEN
	   CALL GET_EXPIRED(INPUT,IER)
	   IF (.NOT.IER) GO TO 910
	   INEXDATE = INPUT(:11)
	   INEXTIME = INPUT(13:)
	END IF

8	IF (CLI$PRESENT('HEADER').OR.DOALL) THEN
	   WRITE(6,1050)			! Request header for bulletin
	   READ(5,'(Q,A)',END=910,ERR=910) DESLEN,INDESCRIP
	   IF (DESLEN.EQ.0) GO TO 910	! If no header, don't add bull
	   IF (DESLEN.GT.53) THEN		! If too many characters
	      WRITE(6,1060)			! tell user
	      WRITE(6,2020) INDESCRIP(:53)	! Show how much would fit
	      GO TO 8			! and re-request header
	   END IF
	ELSE IF (CLI$PRESENT('SUBJECT')) THEN
	   IER = CLI$GET_VALUE('SUBJECT',INDESCRIP,DESLEN)
	   IF (DESLEN.GT.53) THEN		! If too many characters
	      WRITE(6,1060)			! tell user
	      WRITE(6,2020) INDESCRIP(:53)	! Show how much would fit
	      GO TO 910				! and abort
	   END IF
	END IF


	IF (CLI$PRESENT('TEXT').OR.DOALL) THEN
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
     &		       ERR=920,FORM='FORMATTED',CARRIAGECONTROL='LIST')
	            CALL OPEN_FILE_SHARED(1)	! Prepare to copy message
		    ILEN = 81
		    DO I=BLOCK,BLOCK+LENGTH-1	! Copy mesage into file
		       DO WHILE (ILEN.GT.0)
			  CALL GET_BULL(I,INPUT,ILEN)
			  IF (ILEN.LT.0) THEN
			     GO TO 5
			  ELSE IF (ILEN.GT.0) THEN
			     WRITE (3,'(A)') INPUT(:ILEN)
			  END IF
		       END DO
		       ILEN = 80
		    END DO
5		    CALL CLOSE_FILE(1)
	            CLOSE (UNIT=3)		! Bulletin copy completed
		 END IF
		 CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')
	      ELSE 
		 IF (.NOT.SETPRV_PRIV()) CALL DISABLE_PRIVS
		 CALL MAILEDIT(BULL_PARAMETER(:LEN_P),'SYS$LOGIN:BULL.SCR')
	      END IF
	      IER = LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;-1')
	      OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='OLD',
     &		 DISPOSE='DELETE',ERR=920,FORM='FORMATTED')
	   ELSE IF (LEN_P.GT.0) THEN
	      IF (.NOT.SETPRV_PRIV()) CALL DISABLE_PRIVS
	      OPEN (UNIT=3,FILE=BULL_PARAMETER(:LEN_P),STATUS='OLD',
     &		READONLY,SHARED,ERR=920,FORM='FORMATTED') ! Try opening the file
	   END IF

	   CALL ENABLE_PRIVS			! Reset SYSPRV privileges

	   DO WHILE(1)				! Read until end of file to
	      READ (3,'(Q,A)',END=10) ILEN,INPUT	! get record count
	      IF (ILEN.GT.80) GO TO 950
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
	   OPEN (UNIT=3,STATUS='NEW',FILE='SYS$LOGIN:BULL.SCR',
     &		 DISPOSE='DELETE',ERR=920,FORM='FORMATTED',
     &		 CARRIAGECONTROL='LIST')	! Scratch file to save bulletin
	   WRITE (6,1000)		! Request bulletin input from terminal
	   ILEN = 80				! Length of input line
	   DO WHILE (ILEN.GE.0)			! Input until no more input
	      CALL GET_LINE(INPUT,ILEN)		! Get input line
	      IF (ILEN.GT.80) THEN		! Line too long.
		 WRITE(6,'('' ERROR: Input line length > 80. Reinput::'')')
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

	  REWIND (UNIT=3)
	END IF

C
C  Add bulletin to bulletin file and directory entry for to directory file.
C

	DATE_SAVE = DATE
	TIME_SAVE = TIME
	INPUT = DESCRIP

	CALL OPEN_FILE(2)			! Prepare to add dir entry

	CALL READDIR(NUMBER_PARAM,IER)		! Get info for message

	IF (IER.NE.NUMBER_PARAM+1.OR.DATE.NE.DATE_SAVE.OR.
     &	    TIME.NE.TIME_SAVE.OR.INPUT.NE.DESCRIP) THEN
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
	      CALL CLOSE_FILE(2)
	      CLOSE (UNIT=3,STATUS='SAVE')
	      WRITE(6,'('' ERROR: Message has been deleted'',
     &			'' by another user.'')')
	      IF (DOALL.OR.CLI$PRESENT('TEXT')) THEN
		 WRITE (6,'('' New text has been saved in'',
     &				'' SYS$LOGIN:BULL.SCR.'')')
	      END IF
	      GO TO 100
	   END IF
	END IF

	CALL READDIR(0,IER)			! Get directory header

	IF (CLI$PRESENT('TEXT').OR.DOALL) THEN	! If text has been replaced

	   LENGTH_SAVE = LENGTH			! Copy BULL modifies LENGTH

	   CALL OPEN_FILE(1)			! Prepare to add bulletin
	   ICOUNT = (ICOUNT+127)/128

	   BLOCK = NBLOCK + 1
	   BLOCK_SAVE = BLOCK
	   NEMPTY = NEMPTY + LENGTH
	   NBLOCK = NBLOCK + ICOUNT

	   IF (.NOT.REMOTE_SET) CALL WRITEDIR(0,IER)

	   CALL COPY_BULL(3,REC1,BLOCK,IER)	! Replace old bulletin

	   CALL CLOSE_FILE(1)

	   IF (.NOT.REMOTE_SET) THEN
	    CALL READDIR(NUMBER_PARAM,IER)	! Get directory entry
	    LENGTH = ICOUNT			! Update size
	    BLOCK = BLOCK_SAVE
	    CALL WRITEDIR(NUMBER_PARAM,IER)	! Write new directory entry
	   END IF
	ELSE
	   CALL READDIR(NUMBER_PARAM,IER)
	END IF

	IF (.NOT.REMOTE_SET) THEN

	   IF (CLI$PRESENT('HEADER').OR.CLI$PRESENT('SUBJECT')
     &							.OR.DOALL) THEN
	      DESCRIP=INDESCRIP(:53)		! Update description header
	   END IF
	   CALL UPDATE_DIR_HEADER(CLI$PRESENT('EXPIRATION').OR.DOALL,
     &		CLI$PRESENT('PERMANENT'),CLI$PRESENT('SHUTDOWN'),
     &		INEXDATE,INEXTIME)
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
	   IF (CLI$PRESENT('PERMANENT')) THEN
	      MSGTYPE = IBSET(MSGTYPE,1)
	   ELSE IF (CLI$PRESENT('SHUTDOWN')) THEN
	      MSGTYPE = IBSET(MSGTYPE,2)
	   ELSE IF (CLI$PRESENT('EXPIRATION').OR.DOALL) THEN
	      MSGTYPE = IBSET(MSGTYPE,3)
	   END IF
	   IF (.NOT.CLI$PRESENT('HEADER').AND..NOT.
     &		CLI$PRESENT('SUBJECT').AND..NOT.DOALL) INDESCRIP = DESCRIP
	   IF (CLI$PRESENT('EXPIRATION')) THEN
	      EXDATE = INEXDATE
	      EXTIME = INEXTIME
	   END IF
	   WRITE (REMOTE_UNIT,'(7A)',IOSTAT=IER)
     &      10,DESCRIP,NUMBER_PARAM,INDESCRIP(:53),MSGTYPE,EXDATE,EXTIME
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

	CALL CLOSE_FILE(2)		! Totally finished with replace

	CLOSE (UNIT=3)

100	CALL ENABLE_CTRL		! Enable CTRL-Y & -C
	RETURN

910	WRITE(6,1010)
	CLOSE (UNIT=3,ERR=100)
	GOTO 100

920	WRITE(6,1020)
	CALL ENABLE_PRIVS	! Reset SYSPRV privileges
	GOTO 100

950	WRITE (6,1030)
	CLOSE (UNIT=3)
	GO TO 100

1000	FORMAT (' Enter message: End with ctrl-z, cancel with ctrl-c')
1005	FORMAT (' ERROR: You are not reading any message.')
1010	FORMAT (' No message was replaced.')
1015	FORMAT (' ERROR: Specified message was not found.')
1020	FORMAT (' ERROR: Unable to open specified file.')
1030	FORMAT (' ERROR: Line length in file exceeds 80 characters.')
1050	FORMAT (' Enter description header.  Limit header to 53
     & characters.')
1060	FORMAT (' ERROR: Header > 53 characters. Header would be
     & truncated to:')
1090	FORMAT(' ERROR: Specified message is not owned by you.')
1100	FORMAT(' Message is not owned by you.',
     &	       ' Are you sure you want to replace it? ',$)
2020	FORMAT(1X,A)

	END



	SUBROUTINE UPDATE_DIR_HEADER(EXPIRE,PERM,SHUT,INEXDATE,INEXTIME)

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'

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
	   CALL GET_NODE_NUMBER(NODE_NUMBER,NODE_AREA)
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

	INCLUDE 'BULLDIR.INC'

	COMMON/INPUT_BULL/INPUT
	CHARACTER*80 INPUT

	CHARACTER*132 SEARCH_STRING,SAVE_STRING
	DATA SEARCH_STRING /' '/, SEARCH_LEN /1/

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /CTRLC_FLAG/ FLAG

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	CALL DISABLE_CTRL

	IF (CLI$PRESENT('START')) THEN		! Starting message specified
	   CALL CLI$GET_VALUE('START',BULL_PARAMETER,LEN_P)
	   DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER) BULL_POINT
	   BULL_POINT = BULL_POINT - 1
	END IF

	SAVE_STRING = SEARCH_STRING
	SAVE_LEN = SEARCH_LEN

	IER = CLI$GET_VALUE('SEARCH_STRING',SEARCH_STRING,SEARCH_LEN)
	
	IF (.NOT.IER) THEN			! If no search string entered
	   SEARCH_STRING = SAVE_STRING		! use saved search string
	   SEARCH_LEN = SAVE_LEN
	ELSE IF (.NOT.CLI$PRESENT('START')) THEN ! If string entered but no
	   BULL_POINT = 0			 ! starting message, use first
	END IF

	IF (IER) SUBJECT = CLI$PRESENT('SUBJECT')

	CALL STR$UPCASE(SEARCH_STRING,SEARCH_STRING)	! Make upper case

	CALL OPEN_FILE_SHARED(2)

	CALL READDIR(0,IER)

	IF (BULL_POINT+1.GT.NBULL) THEN
	   WRITE (6,'('' ERROR: No more messages.'')')
	   CALL CLOSE_FILE(2)
	   CALL ENABLE_CTRL
	   RETURN
	END IF

	CALL OPEN_FILE_SHARED(1)

	CALL DECLARE_CTRLC_AST

	DO BULL_SEARCH = BULL_POINT+1, NBULL
	   CALL READDIR(BULL_SEARCH,IER)	! Get bulletin directory entry
	   IF (IER.EQ.BULL_SEARCH+1) THEN
	      CALL STR$UPCASE(DESCRIP,DESCRIP)	! Make upper case
	      IF (INDEX(DESCRIP,SEARCH_STRING(:SEARCH_LEN)).GT.0) THEN
		 CALL CLOSE_FILE(1)
		 CALL CLOSE_FILE(2)
		 CALL CANCEL_CTRLC_AST
		 CALL ENABLE_CTRL
		 BULL_POINT = BULL_SEARCH - 1
		 CALL READ(READ_COUNT,BULL_POINT+1) ! Read next bulletin
		 RETURN
	      END IF
	   END IF
	   IF (IER.EQ.BULL_SEARCH+1.AND..NOT.SUBJECT) THEN
	      IF (REMOTE_SET) THEN
	         WRITE (REMOTE_UNIT,'(2A)',IOSTAT=IER) 5,BULL_SEARCH
	         IF (IER.GT.0) THEN
	            CALL DISCONNECT_REMOTE
		    GO TO 900
	         ELSE
	            CALL GET_REMOTE_MESSAGE(IER)
		    IF (IER.GT.0) GO TO 900
	         END IF
	      END IF
	      ILEN = 81
	      DO J=BLOCK,BLOCK+LENGTH-1
	         DO WHILE (ILEN.GT.0)
	            CALL GET_BULL(J,INPUT,ILEN)
	            CALL STR$UPCASE(INPUT,INPUT)	! Make upper case
		    IF (INDEX(INPUT,SEARCH_STRING(:SEARCH_LEN)).GT.0) THEN
		       CALL CLOSE_FILE(1)
		       CALL CLOSE_FILE(2)
		       CALL CANCEL_CTRLC_AST
		       CALL ENABLE_CTRL
		       BULL_POINT = BULL_SEARCH - 1
	               CALL READ(READ_COUNT,BULL_POINT+1) ! Read next bulletin
		       RETURN
		    ELSE IF (FLAG.EQ.1) THEN
		       WRITE (6,'('' Search aborted.'')')
		       CALL CLOSE_FILE(1)
		       CALL CLOSE_FILE(2)
		       CALL ENABLE_CTRL
		       RETURN
		    END IF
	         END DO
		 ILEN = 80
	      END DO
	   END IF
	END DO

900	CALL CANCEL_CTRLC_AST

	CALL CLOSE_FILE(1)			! End of bulletin file read
	CALL CLOSE_FILE(2)

	CALL ENABLE_CTRL

	WRITE (6,'('' No messages found with given search string.'')')

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

	CALL OPEN_FILE(2)

	CALL READDIR(BULL_DELETE,IER)	! Get info for specified bulletin

	IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?
	   WRITE(6,1030)	! If not, then error out
	   GOTO 100
	END IF

	IF (USERNAME.NE.FROM) THEN	! If doesn't match owner of bulletin,
	   IF ((.NOT.SETPRV_PRIV().AND..NOT.FOLDER_SET).OR.    ! Privileges or
     &	       (.NOT.SETPRV_PRIV().AND.USERNAME.NE.FOLDER_OWNER
     &		.AND.FOLDER_SET)) THEN	! folder owner?
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

	IF (SYSTEM.LE.1) THEN		! General or System message
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

100	CALL CLOSE_FILE(2)

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
