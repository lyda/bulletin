From:	KBSVAX::KANE "Joseph Kane"  4-APR-1988 13:32
To:	everhart@arisia.DECNET
Subj:	forwarded mail

From uunet!rutgers.edu!Postmaster Mon Apr  4 14:24:10 1988
Received:  by kbsvax.steinmetz (1.2/1.1x Steinmetz)
	 id AA18631; Mon, 4 Apr 88 14:23:44 edt
Received: from RUTGERS.EDU by uunet.UU.NET (5.54/1.14) 
	id AA05253; Mon, 4 Apr 88 13:44:28 EDT
Received: by rutgers.edu (5.54/1.15) 
	id AB18302; Mon, 4 Apr 88 12:52:10 EDT
Date: Mon, 4 Apr 88 12:52:10 EDT
From: uunet!rutgers.edu!Postmaster (Mail Delivery Subsystem)
Subject: Returned mail: Host unknown
Message-Id: <8804041652.AB18302@rutgers.edu>
To: <MAILER-DAEMON>
Status: R
 
   ----- Transcript of session follows -----
550 <pfc-vax.mit.edu!mrl@rutgers.edu>... Host unknown
 
   ----- Unsent message follows -----
Received: by rutgers.edu (5.54/1.15) 
	id AA17180; Mon, 4 Apr 88 10:26:10 EDT
Received: from steinmetz.UUCP by uunet.UU.NET (5.54/1.14) with UUCP 
	id AA05371; Mon, 4 Apr 88 10:22:48 EDT
Received:  by kbsvax.steinmetz (1.2/1.1x Steinmetz)
	 id AA15927; Mon, 4 Apr 88 10:17:18 edt
Date:  2 Apr 88 20:16:21 EST
From: steinmetz!MAILER-DAEMON@uunet.uu.net (Mail Delivery Subsystem)
Subject: Returned mail: User unknown
Message-Id: <8804041417.AA15927@kbsvax.steinmetz>
To: MRL@pfc-vax.mit.edu
 
   ----- Transcript of session follows -----
mail11: %MAIL-E-SYNTAX, error parsing 'CRD'
550 crd.ge.com!EVERHART%ARISIA.DECNET... User unknown
 
   ----- Unsent message follows -----
Received:  by kbsvax.steinmetz (1.2/1.1x Steinmetz)
	 id AA15899; Mon, 4 Apr 88 10:17:18 edt
Received: by ge-dab.GE.COM (smail2.5)
	id AA19167; 4 Apr 88 06:30:43 EDT (Mon)
Received: by ge-rtp.GE.COM (smail2.5)
	id AA10804; 3 Apr 88 21:16:11 EST (Sun)
Received: by mcnc.mcnc.org (5.54/MCNC/10-20-87)
	id AA14692; Sat, 2 Apr 88 21:04:44 EST
From: <mcnc!rutgers.edu!pfc-vax.mit.edu!MRL>
Received: by rutgers.edu (5.54/1.15) 
	id AA00893; Sat, 2 Apr 88 20:30:57 EST
Message-Id: <8804030130.AA00893@rutgers.edu>
Received: from PFC-VAX.MIT.EDU by XX.LCS.MIT.EDU via Chaosnet; 2 Apr 88 20:17-EST
Date:  2 Apr 88 20:16:21 EST
To: xx!TENCATI@vlsi.jpl.nasa.gov, xx!MHG@mitre-bedford.arpa,
        crd.ge.com!xx!EVERHART@ARISIA.DECNET, xx!GAYMAN@ari-hq1.arpa,
        radc-softvax!xx!BACH
Subject: BULLETIN1.FOR
 
C
C  BULLETIN1.FOR, Version 3/16/88
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Programmer: Mark R. London
C
	SUBROUTINE GET_NODE_INFO
C
C  SUBROUTINE GET_NODE_INFO
C
C  FUNCTION: Gets local node name and obtains node names from
C	command line.
C
 
	IMPLICIT INTEGER (A-Z)
 
	EXTERNAL CLI$_ABSENT
 
	COMMON /NODE_INFO/ NODES,LOCAL_NODE_FOUND,NODE_NUM,
     &				NODE_ERROR,POINT_NODE
	CHARACTER*32 NODES(10)
	LOGICAL LOCAL_NODE_FOUND,NODE_ERROR
 
	CHARACTER LOCAL_NODE*32,NODE_TEMP*256
 
	NODE_ERROR = .FALSE.
 
	LOCAL_NODE_FOUND = .FALSE.
	CALL LIB$SYS_TRNLOG('SYS$NODE',L_NODE,LOCAL_NODE)
	L_NODE = L_NODE - 2			! Remove '::'
	IF (LOCAL_NODE(1:1).EQ.'_') THEN
	   LOCAL_NODE = LOCAL_NODE(2:)
	   L_NODE = L_NODE - 1
	END IF
 
	NODE_NUM = 0				! Initialize number of nodes
	IF (CLI$PRESENT('NODES')) THEN		! Decnet nodes specified?
	   DO WHILE (CLI$GET_VALUE('NODES',NODE_TEMP)
     &	    .NE.%LOC(CLI$_ABSENT))		! Get the specified nodes
	    IER = SYS_TRNLNM(NODE_TEMP,NODE_TEMP)
	    DO WHILE (TRIM(NODE_TEMP).GT.0)
	      NODE_NUM = NODE_NUM + 1
	      COMMA = INDEX(NODE_TEMP,',')
	      IF (COMMA.GT.0) THEN
		 NODES(NODE_NUM) = NODE_TEMP(1:COMMA-1)
		 NODE_TEMP = NODE_TEMP(COMMA+1:)
	      ELSE
		 NODES(NODE_NUM) = NODE_TEMP
		 NODE_TEMP = ' '
	      END IF
	      NLEN = TRIM(NODES(NODE_NUM))
	      IF (INDEX(NODES(NODE_NUM),'::').GT.0) THEN   ! Remove :: if
		 NLEN = INDEX(NODES(NODE_NUM),'::') - 1	   ! addedd
	      END IF
	      IF (LOCAL_NODE(1:L_NODE).EQ.NODES(NODE_NUM)(1:NLEN)) THEN
	       NODE_NUM = NODE_NUM - 1
	       LOCAL_NODE_FOUND = .TRUE.
	      ELSE
	       POINT_NODE = NODE_NUM
	       OPEN (UNIT=9+NODE_NUM,NAME=NODES(NODE_NUM)(1:NLEN)//'""::'
     &	       //'"TASK=BULLETIN"',ACCESS='SEQUENTIAL',FORM='FORMATTED',
     &	       CARRIAGECONTROL='NONE',TYPE='NEW',IOSTAT=IER)
	       IF (IER.NE.0) THEN
		  DO WHILE (NODE_NUM.GT.0)
		     CLOSE(UNIT=9+NODE_NUM)
		     NODE_NUM = NODE_NUM - 1
		  END DO
		  NODE_ERROR = .TRUE.
		  RETURN
	       END IF
	      END IF
	    END DO
	   END DO
	ELSE
	   LOCAL_NODE_FOUND = .TRUE.
	END IF
 
	RETURN
	END
 
 
 
 
	SUBROUTINE DELETE_NODE
C
C  SUBROUTINE DELETE_NODE
C
C  FUNCTION: Deletes files sent via ADD/NODES at remote hosts.
C
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLUSER.INC'
 
	INCLUDE 'BULLDIR.INC'
 
	COMMON /NODE_INFO/ NODES,LOCAL_NODE_FOUND,NODE_NUM,
     &				NODE_ERROR,POINT_NODE
	CHARACTER*32 NODES(10)
	LOGICAL LOCAL_NODE_FOUND,NODE_ERROR
 
    	CHARACTER PASSWORD*31,INLINE*80,DEFAULT_USER*12
 
	CALL GET_NODE_INFO
 
 	IF (NODE_ERROR) GO TO 940
 
	IF (NODE_NUM.EQ.0.OR.LOCAL_NODE_FOUND) THEN
	   WRITE (6,'('' ERROR: Cannot specify local node.'')')
	   GO TO 999
	END IF
 
	IER = CLI$GET_VALUE('USERNAME',DEFAULT_USER)
	IF (.NOT.IER) DEFAULT_USER = USERNAME
	IER = CLI$GET_VALUE('SUBJECT',DESCRIP)
 
	DO POINT_NODE=1,NODE_NUM	   	! Write out command to nodes
	   SEMI = INDEX(NODES(POINT_NODE),'::')	! Look for semicolon after node
	   NLEN = TRIM(NODES(POINT_NODE))	! Length of node name
	   IF (SEMI.GT.0) THEN			! Is semicolon present?
	      IF (NLEN.GT.SEMI+1) THEN		! Yes, is username after node?
	         TEMP_USER = NODES(POINT_NODE)(SEMI+2:)	! Yes, set username
	         NLEN = SEMI - 1		! Remove semicolon
	      ELSE				! No username after nodename
		 TEMP_USER = DEFAULT_USER	! Set username to default
	         NLEN = SEMI - 1		! Remove semicolon
		 SEMI = 0			! Indicate no username
	      END IF
	   ELSE					! No semicolon present
	      TEMP_USER = DEFAULT_USER		! Set username to default
	   END IF
	   INLINE = 'DELETE/SUBJECT="'//DESCRIP(:TRIM(DESCRIP))//
     &      '"/USERNAME='//TEMP_USER(:TRIM(TEMP_USER))
	   IF (CLI$PRESENT('USERNAME').OR.SEMI.GT.0) THEN  ! If username was
	      IER = 1				! specified, prompt for password
	      DO WHILE (IER.NE.0)
	         WRITE(6,'('' Enter password for node '',2A)')
     &			NODES(POINT_NODE),CHAR(10)
	         CALL GET_INPUT_NOECHO(PASSWORD)
	         IF (TRIM(PASSWORD).EQ.0) GO TO 910
	         OPEN (UNIT=10+NODE_NUM,NAME=NODES(POINT_NODE)(:NLEN)
     &		   //'"'//TEMP_USER(1:TRIM(TEMP_USER))//' '//
     &		   PASSWORD(1:TRIM(PASSWORD))//'"::',
     &		   TYPE='SCRATCH',IOSTAT=IER)
	         CLOSE (UNIT=10+NODE_NUM)
	         IF (IER.NE.0) THEN
		    WRITE (6,'('' ERROR: Password is invalid.'')')
	         END IF
	      END DO
	   END IF
	   WRITE (POINT_NODE+9,'(A)',ERR=940) INLINE
	   READ (POINT_NODE+9,'(A)',ERR=940,END=940) INLINE
	   IF (INLINE.EQ.'END') THEN
	      WRITE (6,'('' Message successfully deleted from node '',A)')
     &				NODES(POINT_NODE)
	   ELSE
	      WRITE (6,'('' Error while deleting message to node '',A)')
     &				NODES(POINT_NODE)
	      WRITE (6,'(A)') INLINE
	   END IF
	END DO
 
	GO TO 999
 
910	WRITE (6,1010)
	GO TO 999
 
940	WRITE (6,1015) NODES(POINT_NODE)
 
999	DO WHILE (NODE_NUM.GT.0)
	   CLOSE(UNIT=9+NODE_NUM)
	   NODE_NUM = NODE_NUM - 1
	END DO
 
	RETURN
 
1010	FORMAT (' ERROR: Deletion aborted.')
1015	FORMAT (' ERROR: Unable to reach node ',A)
 
	END
 
 
 
 
	SUBROUTINE SET_FOLDER_FLAG(SETTING,FLAG,FLAGNAME)
C
C  SUBROUTINE SET_FOLDER_FLAG
C
C  FUNCTION: Sets or clears specified flag for folder
C
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLFOLDER.INC'
 
	INCLUDE 'BULLUSER.INC'
 
	INCLUDE 'BULLFILES.INC'
 
	CHARACTER*(*) FLAGNAME
 
	IF (FOLDER_OWNER.EQ.USERNAME.OR.SETPRV_PRIV()) THEN
	   CALL OPEN_FILE(7)		! Open folder file
 
	   CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)
 
	   IF (SETTING) THEN
	      FOLDER_FLAG = IBSET(FOLDER_FLAG,FLAG)
	   ELSE
	      FOLDER_FLAG = IBCLR(FOLDER_FLAG,FLAG)
	   END IF
 r
	   CALL REWRITE_FOLDER_FILE
 1
	   CALL CLOSE_FILE(7)
  
	   WRITE (6,'(1X,A,'' has been modified for folder.'')')4
     &		FLAGNAME
	ELSEe
	   WRITE (6,'(1X,A,'' You are not authorized to modify.'')')5
     &		FLAGNAME
	END IFD
 
	RETURN 
	END
 s
 u
 .
 1
	SUBROUTINE SET_FOLDER_EXPIRE_LIMIT(LIMIT)
C
C  SUBROUTINE SET_FOLDER_EXPIRE_LIMITm
Cu
C  FUNCTION: Sets folder expiration limit.
Cs
	IMPLICIT INTEGER (A-Z)a
  
	INCLUDE 'BULLFOLDER.INC'8
 0
	INCLUDE 'BULLUSER.INC'>
 o
	INCLUDE 'BULLFILES.INC'
 
	IF (LIMIT.LT.0) THENp
	   WRITE (6,'('' ERROR: Invalid expiration length specified.'')')
	ELSE IF (FOLDER_OWNER.EQ.USERNAME.OR.SETPRV_PRIV()) THEN
	   CALL OPEN_FILE(7)		! Open folder file 
 7
	   CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)m
 e
	   F_EXPIRE_LIMIT = LIMIT
 5
	   CALL REWRITE_FOLDER_FILE
 M
	   CALL CLOSE_FILE(7)
	   WRITE (6,'('' Folder expiration date modified.'')')
	ELSE1
	   WRITE (6,'('' You are not allowed to modify folder.'')')
	END IFt
 m
	RETURNR
	END
 u
 .
 n
 (
 l
	SUBROUTINE MAIL(STATUS)
C:
C  SUBROUTINE MAIL
Ck
C  FUNCTION: Sends message which you have read to user via DEC mail.
C.
	IMPLICIT INTEGER (A - Z)s
 p
	CHARACTER INPUT*80-
 -
	COMMON /POINT/ BULL_POINT
  
	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER
 
	CHARACTER*64 MAIL_SUBJECT
 w
	INCLUDE 'BULLDIR.INC'
 v
	EXTERNAL CLI$_ABSENTS
 n
	IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been read 
	   WRITE(6,'('' ERROR: You have not read any message.'')')E
	   RETURN			! And returnt
	END IF(
 i
	MAIL_SUBJECT = DESCRIPr
	IF (CLI$PRESENT('SUBJECT')) THENy
	   IER = CLI$GET_VALUE('SUBJECT',MAIL_SUBJECT,LEN_D) 
	   IF (LEN_D.GT.LEN(MAIL_SUBJECT)-2) THEN
	      WRITE(6,'('' ERROR: Subject limit is 64 characters.'')')
	      RETURNt
	   END IF
	END IFS
 
	CALL OPEN_FILE_SHARED(2)0
 @
	CALL READDIR(BULL_POINT,IER)	! Get info for specified bulletina
 a
	IF (IER.NE.BULL_POINT+1) THEN	! Was bulletin found?
	   WRITE(6,'('' ERROR: Specified message was not found.'')'),
	   CALL CLOSE_FILE(2)		! If not, then error out
	   RETURN
	END IF
  
	CALL CLOSE_FILE(2)!
 H
	OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,
     &		STATUS='NEW',CARRIAGECONTROL='LIST')
 s
	IF (IER.NE.0) THENr
	   WRITE(6,'('' ERROR: Error in opening scratch file.'')')
	   RETURN
	END IF.
 n
	CALL OPEN_FILE_SHARED(1)	! Open BULLETIN file
 T
	LEN_I = 81I
	DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file
	   DO WHILE (LEN_I.GT.0)n
	      CALL GET_BULL(I,INPUT,LEN_I)A
	      IF (LEN_I.LT.0) THENN
		 GO TO 90O
	      ELSE IF (LEN_I.GT.0) THEN
	         WRITE (3,'(A)') INPUT(:LEN_I)N
	      END IFR
	   END DO
	   LEN_I = 80
	END DOO
 ,
90	CLOSE (UNIT=3)			! Message copy completed
 P
	CALL CLOSE_FILE(1) 
 F
	LEN_D = TRIM(MAIL_SUBJECT) 
	IF (LEN_D.EQ.0) THENY
	   MAIL_SUBJECT = 'BULLETIN message.'
	   LEN_D = TRIM(MAIL_SUBJECT)
	END IF 
 C
	IF (MAIL_SUBJECT(:1).NE.'"') THEN
	   MAIL_SUBJECT = '"'//MAIL_SUBJECT(:LEN_D)
	   LEN_D = LEN_D + 1D
	END IF	
 !
	IF (MAIL_SUBJECT(LEN_D:LEN_D).NE.'"') THENT
	   MAIL_SUBJECT = MAIL_SUBJECT(:LEN_D)//'"'
	   LEN_D = LEN_D + 1U
	END IF,
 E
	IER = CLI$GET_VALUE('RECIPIENTS',BULL_PARAMETER,LEN_P)i
 d
	CALL DISABLE_PRIVSS
	CALL LIB$SPAWN('$MAIL SYS$LOGIN:BULL.SCR '//BULL_PARAMETER(:LEN_P) 
     &	   //'/SUBJECT='//MAIL_SUBJECT(:LEN_D),,,,,,STATUS)
	CALL ENABLE_PRIVS
 A
	CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR')1
 M
	RETURN	
 D
	END
 N
 _
 P
	SUBROUTINE MODIFY_FOLDER	
CD
C  SUBROUTINE MODIFY_FOLDERN
C_
C  FUNCTION: Modifies a folder's information.N
CS
	IMPLICIT INTEGER (A - Z)D
 N
	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER
 O
	INCLUDE 'BULLFOLDER.INC'd
 
	INCLUDE 'BULLUSER.INC' 
 C
	INCLUDE 'BULLFILES.INC'
 D
	INCLUDE '($SSDEF)'
  
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 U
	IF (FOLDER_NUMBER.EQ.0) THEN 
	   WRITE (6,'('' ERROR: Cannot modify GENERAL folder.'')')E
	   RETURN
	ELSE IF (FOLDER_OWNER.NE.USERNAME.AND..NOT.SETPRV_PRIV()) THENE
	   WRITE (6,'('' ERROR: No privileges to modify folder.'')')N
	   RETURN
	END IFI
 
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
	      END IFF
	   END IF
	ELSEl
	   FOLDER1 = FOLDER
	END IF 
 t
	IF (CLI$PRESENT('DESCRIPTION')) THEN
	   WRITE (6,'('' Enter one line description of folder.'')')
	   LEN_P = 81
	   DO WHILE (LEN_P.GT.80)
	    CALL GET_LINE(FOLDER1_DESCRIP,LEN_P)	! Get input line
	    IF (LEN_P.LE.0) THEN_
	     WRITE (6,'('' ERROR: Folder modification aborted.'')')
	    ELSE IF (LEN_P.GT.80) THEN			! If too many characters
	     WRITE (6,'('' ERROR: Description must be < 80 characters.'')')
	    ELSE 
	       FOLDER1_DESCRIP = FOLDER1_DESCRIP(:LEN_P) ! End fill with spaces
	    END IFL
	   END DO
	ELSEL
	   FOLDER1_DESCRIP = FOLDER_DESCRIP
	END IFE
 I
	IF (CLI$PRESENT('OWNER')) THENR
	   CALL CLI$GET_VALUE('OWNER',FOLDER1_OWNER,LEN_P)o
	   IF (LEN_P.GT.LEN(FOLDER1_OWNER)) THENT
	      WRITE (6,'('' ERROR: Folder owner name too long.'')')
	      RETURNE
	   ELSE IF (.NOT.SETPRV_PRIV()) THENG
	      WRITE (6,'('' ERROR: No privileges to modify folder owner.'')')
	   ELSE
	      FOLDER1_OWNER = FOLDER1_OWNER(:LEN_P)
	   END IF
	ELSEI
	   FOLDER1_OWNER = FOLDER_OWNER
	END IFS
  
	CALL OPEN_FILE(7)		! Open folder file
 N
	IF (CLI$PRESENT('NAME')) THEN
	   READ (7,IOSTAT=IER,KEY=FOLDER1,KEYID=0)a
					! See if folder exists 
	   IF (IER.EQ.0) THEN
	      WRITE (6,'('' ERROR: Folder name already exists.'')')
	      CALL CLOSE_FILE(7)
	      RETURNS
	   END IF
	END IFS
 u
	CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)
 E
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
  
	IF (IER.EQ.0) THEN(
	   IF (CLI$PRESENT('OWNER')) THEN
	      CALL CHKACL
     &		(FOLDER_FILE(:TRIM(FOLDER_FILE))//'.BULLFIL',IER)N
	      IF (IER.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL)) THEN
	         CALL ADD_ACL(FOLDER1_OWNER,'R+W+C',IER)'
	         CALL DEL_ACL(FOLDER_OWNER,'R+W+C',IER)
	      END IF=
	   END IF
	   FOLDER = FOLDER1
	   FOLDER_OWNER = FOLDER1_OWNER
	   FOLDER_DESCRIP = FOLDER1_DESCRIP
	   DELETE (7)
	   CALL WRITE_FOLDER_FILE(IER)P
	   IF (IER.EQ.0) WRITE (6,'('' Folder successfully modified.'')')
	END IF9
  
	IF (IER.NE.0) THENE
	   WRITE (6,'('' ERROR: Folder modification aborted.'')')
	END IFr
 n
	CALL CLOSE_FILE(7)	
 O
	RETURN_
	END
  
 L
 
	SUBROUTINE MOVE(DELETE_ORIGINAL)l
Cn
C  SUBROUTINE MOVE
C'
C  FUNCTION: Moves message from one folder to another.
CL
	IMPLICIT INTEGER (A - Z)
 
	COMMON /POINT/ BULL_POINT
 1
	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER
 E
	COMMON /ACCESS/ READ_ONLY
	LOGICAL READ_ONLY
 O
	INCLUDE 'BULLDIR.INC'
 D
	INCLUDE 'BULLUSER.INC'0
 F
	INCLUDE 'BULLFOLDER.INC'o
 d
	EXTERNAL CLI$_ABSENTR
  
	LOGICAL DELETE_ORIGINAL
 
	CHARACTER INPUT*80,SAVE_USERNAME*12,SAVE_FOLDER*25E
 N
	CHARACTER*116 BULLDIR_COM_SAVEN
 E
	IF (BULL_POINT.EQ.0) THEN	! If no message has been read
	   WRITE(6,'('' ERROR: You are not reading any message.'')')U
	   RETURN			! and returnE
	END IFR
 C
	CALL OPEN_FILE_SHARED(2).
	CALL READDIR(BULL_POINT,IER)		! Get message directory entry
	CALL CLOSE_FILE(2)V
	IF (IER.NE.BULL_POINT+1) THEN		! Was message found?
	   WRITE(6,'('' ERROR: Specified message was not found.'')')(
	   RETURN
	END IF 
 L
	CALL LIB$MOVC3(116,%REF(BULLDIR_COM),%REF(BULLDIR_COM_SAVE))G
	SAVE_BULL_POINT = BULL_POINT 
  
	OPEN (UNIT=3,STATUS='SCRATCH',FILE='SYS$LOGIN:BULL.SCR',L
     &		FORM='FORMATTED')		! Scratch file to save bulletin
 '
	CALL OPEN_FILE_SHARED(1)S
 
	ILEN = 81
	DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file
	   DO WHILE (ILEN.GT.0)
	      CALL GET_BULL(I,INPUT,ILEN)
	      IF (ILEN.LT.0) THEN
		 GO TO 90)
	      ELSE IF (ILEN.GT.0) THENP
	         WRITE (3,'(A)') INPUT(:ILEN)
	      END IFi
	   END DO
	   ILEN = 80A
	END DO
 C
90	REWIND (UNIT=3)			! Bulletin copy completed
 >
	CALL CLOSE_FILE(1)I
 .
	SAVE_FOLDER = FOLDER.
	SAVE_FOLDER_NUMBER = FOLDER_NUMBERv
	CALL CLI$GET_VALUE('FOLDER',FOLDER1)
 S
	SAVE_USERNAME = USERNAMEA
	IF (CLI$PRESENT('ORIGINAL')) THEN
	 IF (SETPRV_PRIV()) THENr
	  USERNAME = FROM
	 ELSE
	  WRITE (6,
     &	  '('' ERROR: You have no privileges to keep original owner.'')')
	 END IF
	END IFA
 C
	FOLDER_NUMBER = -1	! Use FOLDER as key rather than FOLDER_NUMBER'
	CALL SELECT_FOLDER(.FALSE.,IER)
  
	IF (.NOT.IER.OR.READ_ONLY) THEN
	   WRITE (6,'('' ERROR: Cannot access specified folder.'')') 
	   CLOSE (UNIT=3)
	   FOLDER_NUMBER = SAVE_FOLDER_NUMBER
	   FOLDER = SAVE_FOLDER
	   USERNAME = SAVE_USERNAME
	   RETURN
	END IF-
 s
Cp
C  Add bulletin to bulletin file and directory entry for to directory file. 
CL
 R
	CALL OPEN_FILE(2)			! Prepare to add dir entryC
 A
	CALL OPEN_FILE(1)			! Prepare to add bulletin
 v
	CALL READDIR(0,IER)			! Get NBLOCKL
	IF (IER.EQ.0) NBLOCK = 0		! If new file, NBLOCK is 0 
 T
	CALL COPY_BULL(3,1,NBLOCK+1,IER)	! Add the new bulletin
	IF (IER.NE.0) THEN			! Error in creating bulletin
	   WRITE(6,'('' ERROR: Message copy aborted.'')')
	   CALL CLOSE_FILE(1)
	   CALL CLOSE_FILE(2)
	   CLOSE (UNIT=3)
	END IF_
 J
	CALL CLOSE_FILE(1)			! Finished adding bulletin
 t
	CALL LIB$MOVC3(116,%REF(BULLDIR_COM_SAVE),%REF(BULLDIR_COM))
 
	IF (.NOT.BTEST(FOLDER_FLAG,2).OR.	! Not system folder?I
     &		 .NOT.SETPRV_PRIV()) THEN	! Or no privileges?.
	   SYSTEM = IBCLR(SYSTEM,0)		! Remove system bit(
	END IFR
  
	IF (BTEST(SYSTEM,2).AND.		! Shutdown message?
     &	    (.NOT.BTEST(FOLDER_FLAG,2).OR.	! Not system folder?
     &		 .NOT.SETPRV_PRIV())) THEN	! Or no privileges?
	   SYSTEM = IBCLR(SYSTEM,2)		! Remove shutdown bitG
	   CALL GET_EXDATE(EXDATE,FOLDER_BBEXPIRE) 
	ELSE IF (BTEST(SYSTEM,1).AND.FOLDER_NUMBER.EQ.0.AND.'
     &		 .NOT.SETPRV_PRIV()) THEN	! Permanent?
	  WRITE (6,'('' ERROR: No privileges to add permanent message.'')')
	  WRITE (6,'('' Expiration will be '',I,'' days.'')') FOLDER_BBEXPIRE
	END IFI
 P
	FROM = USERNAME				! Specify ownerE
	CALL ADD_ENTRY				! Add the new directory entry
  
	IF (FOLDER_NUMBER.GE.0) THENL
	   CALL UPDATE_FOLDER			! Update folder info 
C
C  If user is adding message, update that user's last read time for 
C  folder, so user is not alerted of new message which is owned by user.
C 
	   LAST_READ_BTIM(1,FOLDER_NUMBER+1) = F_NEWEST_BTIM(1)
	   LAST_READ_BTIM(2,FOLDER_NUMBER+1) = F_NEWEST_BTIM(2)
	END IFL
 B
	CALL CLOSE_FILE(2)			! Totally finished with add 
 D
	CLOSE (UNIT=3)			! Close the input file
 )
	WRITE (6,'('' Message has been copied to folder '',A)')
     &		FOLDER(:TRIM(FOLDER))//'.'
 E
	USERNAME = SAVE_USERNAMEI
 N
	FOLDER_NUMBER = SAVE_FOLDER_NUMBER 
	FOLDER1 = SAVE_FOLDER
	CALL SELECT_FOLDER(.FALSE.,IER)
 B
	BULL_POINT = SAVE_BULL_POINT 
 /
	IF (DELETE_ORIGINAL) CALL DELETE,
 S
	RETURNC
  
	END
 I
 
 
 L
	SUBROUTINE PRINTS
CL
C  SUBROUTINE PRINT
CT
C  FUNCTION:  Print header to queue.
CO
 Y
	IMPLICIT INTEGER (A-Z)N
 O
	INCLUDE '($SJCDEF)'
 T
	CHARACTER*32 QUEUEr
 i
	INTEGER*2 FILE_ID(14)
	INTEGER*2 IOSB(4)
	EQUIVALENCE (IOSB(1),JBC_ERROR)
 R
	CHARACTER*80 INPUT 
 L
	COMMON /POINT/ BULL_POINT
 O
	INCLUDE 'BULLDIR.INC'
 U
	IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been read$
	   WRITE(6,1010)		! Write error
	   RETURN			! And return
	END IFF
 E
	CALL OPEN_FILE_SHARED(2)R
  
	CALL READDIR(BULL_POINT,IER)	! Get info for specified bulletinE
  
	IF (IER.NE.BULL_POINT+1) THEN	! Was bulletin found?
	   WRITE(6,1030)'
	   CALL CLOSE_FILE(2)		! If not, then error out
	   RETURN
	END IF
 
	CALL CLOSE_FILE(2)E
  
	CALL OPEN_FILE_SHARED(1)	! Open BULLETIN file
 '
	IF (.NOT.SETPRV_PRIV()) THEN		! If no SETPRV, remove SYSPRV
	   CALL DISABLE_PRIVS			! privileges when trying to
	END IF					! create new file.
 I
	OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.LIS',ERR=900,IOSTAT=IER,
     &		STATUS='NEW',CARRIAGECONTROL='LIST')
 e
	CALL ENABLE_PRIVS
 U
	IF (CLI$PRESENT('HEADER')) THEN		! Printout header?
	   WRITE(3,1050) DESCRIP		! Output bulletin header info
	   WRITE(3,1060) FROM,DATEn
	END IFs
 p
	ILEN = 81
	DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file
	   DO WHILE (ILEN.GT.0)
	      CALL GET_BULL(I,INPUT,ILEN)
	      IF (ILEN.GT.0) WRITE(3,'(A)') INPUT(:TRIM(INPUT))
	   END DO
	   ILEN = 80)
	END DOS
 F
	CLOSE (UNIT=3)			! Bulletin copy completeds
  
	CALL CLOSE_FILE(1)O
 D
	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(18,SJC$_FILE_SPECIFICATION,
     &		%LOC('SYS$LOGIN:BULL.LIS'))
  
	IER = CLI$GET_VALUE('QUEUE',QUEUE,ILEN) 	! Get queue name
	IF (ILEN.EQ.0) THEN
	   QUEUE = 'SYS$PRINT')
	   ILEN = 9
	END IFT
 L
	CALL ADD_2_ITMLST(ILEN,SJC$_QUEUE,%LOC(QUEUE))T
	CALL ADD_2_ITMLST(0,SJC$_DELETE_FILE,0)
 (
	IF (CLI$PRESENT('NOTIFY')) THEN
	   CALL ADD_2_ITMLST(0,SJC$_NOTIFY,0)
	END IFV
 I
	IF (.NOT.SETPRV_PRIV()) THEN		! If no SETPRV, remove SYSPRV
	   CALL DISABLE_PRIVS			! privileges when trying to
	END IF					! create new file.
 
	CALL END_ITMLST(SJC_ITMLST)
	R
	IER=SYS$SNDJBCW(,%VAL(SJC$_ENTER_FILE),,%VAL(SJC_ITMLST),IOSB,,)I
	IF (IER.AND.(.NOT.JBC_ERROR)) THEND
	   CALL SYS_GETMSG(JBC_ERROR)
	   IER = LIB$DELETE_FILE('SYS$LOGIN:BULL.LIS;')
	ELSE IF (.NOT.IER) THEN
	   CALL SYS_GETMSG(IER)
	   IER = LIB$DELETE_FILE('SYS$LOGIN:BULL.LIS;')
	END IFR
 
	CALL ENABLE_PRIVS			! Reset SYSPRV privileges
 Y
	RETURNE
 E
900	CALL ERRSNS(IDUMMY,IER)$
	CALL ENABLE_PRIVS			! Reset SYSPRV privileges
	CLOSE (UNIT=3,STATUS='DELETE')F
	CALL CLOSE_FILE(1):
	WRITE(6,1000)
	CALL SYS_GETMSG(IER)/
 *
	RETURND
 C
1000	FORMAT(' ERROR: Unable to open temporary file
     & SYS$LOGIN:BULL.LIS for printing.') 
1010	FORMAT(' ERROR: You have not read any message.') 
1030	FORMAT(' ERROR: Specified message was not found.')R
1040	FORMAT(' Message ',I4,' written to ',A)
1050	FORMAT('Description: ',A53)
1060	FORMAT('From: ',A12,' Date: ',A11,/)$
 L
	END
 S
 N
 A
  
	SUBROUTINE READ(READ_COUNT,BULL_READ)
C'
C  SUBROUTINE READ
C 
C  FUNCTION: Reads a specified bulletin.
C 
C  PARAMETER:N
C	READ_COUNT - Variable to store the record in the message file 
C		that READ will read from.  Must be set to 0 to indicate
C		that it is the first read of the message.  If -1,
C		READ will search for the last message in the message file
C		and read that one.  If -2, just display header information.
C	BULL_READ - Message number to be read.
CO
	IMPLICIT INTEGER (A - Z)
 S
	COMMON /POINT/ BULL_POINT
 l
	INCLUDE 'BULLDIR.INC'
 '
	INCLUDE 'BULLFOLDER.INC'e
 o
	INCLUDE 'BULLUSER.INC'
 
	COMMON/INPUT_BULL/INPUT
	CHARACTER*80 INPUT 
 L
	COMMON /READIT/ READITP
  
	COMMON /PAGE/ PAGE_LENGTH,PAGING 
	LOGICAL PAGINGE
 C
	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD
 U
	DATA SCRATCH_B1/0/
 C
	DIMENSION MSG_BTIM(2)
 I
	CHARACTER TODAY*11,DATETIME*23T
 A
	LOGICAL SINCE,PAGEL
 C
	CALL LIB$ERASE_PAGE(1,1)		! Clear screen,
	END = 0					! Nothing outputted on screen
 R
	IF (READ_COUNT.GT.0) GO TO 100		! Skip init steps if this ism
						! not first page of bulletin
 E
	SINCE = .FALSE.
	PAGE = .TRUE.
	.
	IF (.NOT.PAGING) PAGE = .FALSE.
	IF (INCMD(:4).EQ.'READ') THEN		! If READ command...
	 IF (.NOT.CLI$PRESENT('PAGE')) PAGE = .FALSE.
	 IF (CLI$PRESENT('SINCE')) THEN		! was /SINCE specified? 
	   IER = CLI$GET_VALUE('SINCE',DATETIME) 
	   IF (DATETIME.EQ.'TODAY') THEN'
	      IER = SYS$ASCTIM(,TODAY,,)	! Get today's date
	      DATETIME = TODAY//' 00:00:00.0'
	   END IF
	 ELSE IF (CLI$PRESENT('NEW').AND.FOLDER_NUMBER.GE.0) THEN
						! was /NEW specified?&
	   DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &			       F_NEWEST_BTIM)
	   IF (DIFF.GE.0) THENC
	      WRITE (6,'('' No new messages are present.'')')
	      RETURNC
	   ELSE
	      CALL SYS$ASCTIM
     &		(,DATETIME,LAST_READ_BTIM(1,FOLDER_NUMBER+1),)
	   END IF
	 END IF
	 IF (CLI$PRESENT('SINCE').OR.CLI$PRESENT('NEW')) THEN
	   CALL OPEN_FILE_SHARED(2)
	   TEMP_READ = 0=
	   IER = 1i
	   DO WHILE (IER.EQ.TEMP_READ+1)I
	      TEMP_READ = TEMP_READ + 1
	      CALL READDIR(TEMP_READ,IER)
	      IF (IER.NE.TEMP_READ+1) THENE
		 WRITE (6,'('' No messages found past specified date.'')')
		 CALL CLOSE_FILE(2)V
		 RETURNE
	      ELSEM
	         DIFF = COMPARE_DATE(DATETIME(:11),DATE)  ! Compare expiratione
	         IF (DIFF.EQ.0) DIFF = COMPARE_TIME(DATETIME(13:20),TIME)
	         IF (DIFF.LT.0) THENh
		    BULL_READ = TEMP_READ 
		    IER = IER + 1,
		 END IFI
	      END IFR
	   END DO
	   IER = BULL_READ + 1O
	   SINCE = .TRUE.
	   CALL CLOSE_FILE(2)
	 END IF
	END IF 
 O
	IF (.NOT.SINCE) THENE
	 IF (BULL_READ.GT.0) THEN		! Valid bulletin number?
	   CALL OPEN_FILE_SHARED(2)
	   CALL READDIR(BULL_READ,IER)		! Get bulletin directory entryf
	   IF (READ_COUNT.EQ.-1.AND.IER.NE.BULL_READ+1) THENP
	      READ_COUNT = 0C
	      CALL READDIR(0,IER)
	      IF (NBULL.GT.0) THEN
	         BULL_READ = NBULLN
	         CALL READDIR(BULL_READ,IER) 
	      ELSEO
		 IER = 0
	      END IFL
	   END IF
	   CALL CLOSE_FILE(2)
	 ELSE
	   IER = 0)
	 END IF
	END IFc
 t
	IF (IER.NE.BULL_READ+1) THEN		! Was bulletin found?
	   WRITE(6,1030)			! If not, then error out
	   RETURN
	END IFS
 U
	IF (FOLDER_NUMBER.GE.0) THENS
	   CALL SYS_BINTIM(DATE//' '//TIME,MSG_BTIM)L
	   DIFF = COMPARE_BTIM(MSG_BTIM,LAST_READ_BTIM(1,FOLDER_NUMBER+1)).
	   IF (DIFF.GT.0) THENO
	      LAST_READ_BTIM(1,FOLDER_NUMBER+1) = MSG_BTIM(1)
	      LAST_READ_BTIM(2,FOLDER_NUMBER+1) = MSG_BTIM(2)
	   END IF
	END IF
 D
	BULL_POINT = BULL_READ			! Update bulletin counterg
 
	IF (INCMD(:4).EQ.'READ'.OR.INCMD(:4).EQ.'CURR') THENf
	   IF (CLI$PRESENT('EDIT')) THEN)
	      CALL READ_EDITe
	      RETURN=
	   END IF
	END IFe
 e
	FLEN = TRIM(FOLDER)
	WRITE(6,1040) BULL_POINT,FOLDER(:FLEN)	! Output bulletin header infoO
	WRITE(6,1050) DESCRIP
	IF (EXDATE(8:9).EQ.'18'.OR.INDEX(EXDATE,'1900').GT.0) THEN(
	   WRITE(6,1060) FROM,DATE,'(DELETED)'m
	ELSE IF ((SYSTEM.AND.4).EQ.4) THEN	! Is entry shutdown bulletin?)
	   WRITE(6,1060) FROM,DATE,'Expires on shutdown'	
	ELSE IF ((SYSTEM.AND.2).EQ.2) THEN	! Is entry permanent bulletin?
	   WRITE(6,1060) FROM,DATE,'Permanent'A
	ELSET
	   WRITE(6,1060) FROM,DATE,'Expires: '//EXDATE//' '//EXTIME
	END IFa
	IF ((SYSTEM.AND.1).EQ.1) THEN		! System bulletin?
	   WRITE(6,'(''+ / System'',/)')i
	ELSEw
	   WRITE(6,'(''+'',/)')
	END IFM
CF
C  Each page of the bulletin is buffered into temporary memory storage beforeE
C  being outputted to the terminal.  This is to be able to quickly close the
C  bulletin file, and to avoid the possibility of the user holding the screen,
C  and thus causing the bulletin file to stay open.  The temporary memoryN
C  is structured as a linked-list queue, where SCRATCH_B1 points to the header
C  of the queue.  See BULLSUBS.FOR for more description of the queue.O
C 
 /
	IF (SCRATCH_B1.NE.0) THEN		! Is queue empty?R
	   SCRATCH_B = SCRATCH_B1		! No, set queue pointer to headO
	ELSE					! Else if queue is empty
	   CALL INIT_QUEUE(SCRATCH_B,INPUT)
	   SCRATCH_B1 = SCRATCH_B		! Init header pointerA
	END IFU
 r
	END = 4					! Outputted 4 lines to screen
 )
	READ_ALREADY = 0			! Number of lines already read
						! from record.
	IF (READ_COUNT.EQ.-2) THEN		! Just output header first read
	   READ_COUNT = BLOCK
	   RETURN
	ELSEr
	   READ_COUNT = BLOCK			! Init bulletin record counterd
	END IF
 D
100	SCRATCH_B = SCRATCH_B1			! Init queue pointer to headerP
	IF (READIT.GT.0) END = END - 2		! /READ can output 2 more lines
	DISPLAY = 0
	IF (READ_COUNT.GT.BLOCK.AND.READIT.EQ.0) THEN ! If not 1st page of READ
	   WRITE(6,1040) BULL_POINT,FOLDER(:FLEN) ! Output bulletin header info
	   END = END + 1			  ! Increase display counter
	END IF.
	CALL OPEN_FILE_SHARED(1)		! Get bulletin file
	MORE_LINES = .TRUE.
	READ_REC = READ_COUNT
	IF (READ_ALREADY.EQ.0) ILEN = 81w
	DO WHILE (MORE_LINES.AND.READ_REC.LE.BLOCK+LENGTH-1)9
	   DO WHILE (ILEN.GT.0.AND.MORE_LINES)I
	      CALL GET_BULL(READ_REC,INPUT,ILEN)
	      IF (ILEN.LT.0) THEN		! Error, couldn't read record 
		 READ_REC = BLOCK + LENGTH	! Fake end of reading file 
		 MORE_LINES = .FALSE.n
	      ELSE IF (ILEN.GT.0) THEN=
		 LEN_TEMP = ILEN
		 CALL CONVERT_TABS(INPUT,LEN_TEMP)
	         CALL WRITE_QUEUE(%VAL(SCRATCH_B),SCRATCH_B,INPUT) 
	         READ_ALREADY = 1
		 DISPLAY = DISPLAY + 1
		 IF ((DISPLAY.EQ.PAGE_LENGTH-END-4).AND.PAGE) THEN
		    MORE_LINES = .FALSE.
		 END IFL
	      END IFD
	   END DO
	   ILEN = 80a
	   IF (MORE_LINES) THEN
	      READ_REC = READ_REC + 1
	      READ_ALREADY = 0O
	   END IF
	END DO
 R
	CALL CLOSE_FILE(1)			! End of bulletin file reada
 
C 
C  Bulletin page is now in temporary memory, so output to terminal.L
C  Note that if this is a /READ, the first line will have problems withS
C  the usual FORMAT statement.  It will cause a blank line to be outputted
C  at the top of the screen.  This is because of the input QIO at the 
C  end of the previous page.  The output gets confused and thinks it must 
C  end the previous line.  To prevent that, the first line of a new page
C  in a /READ must use a different FORMAT statement to surpress the CR/LF.
CD
 N
	SCRATCH_B = SCRATCH_B1			! Reinit queue pointer to head
	DO I=1,DISPLAY				! Output page to terminal
	   CALL READ_QUEUE(%VAL(SCRATCH_B),SCRATCH_B,INPUT) ! Get queue recordF
	   IF (I.EQ.1.AND.READ_REC.NE.BLOCK.AND.READIT.GT.0) THEN
	      WRITE(6,2020) INPUT(:TRIM(INPUT))	! (See above comments)D
	   ELSE
	      WRITE(6,2010) INPUT(:TRIM(INPUT))
	   END IF
	END DO3
 A
	READ_COUNT = READ_REC			! Update bull record counterC
  
	IF (READ_REC.EQ.BLOCK+LENGTH) THEN	! Last block?R
	   READ_COUNT = 0			! init bulletin record counterB
	ELSE IF (READ_REC.EQ.BLOCK+LENGTH-1.AND..NOT.MORE_LINES) THEN
		! Possibly last block since end of page could be last line
	   CALL TEST_MORE_LINES(ILEN)		! More lines to read?,
	   IF (ILEN.GT.0) THEN			! Yes, there are still more:
	      IF (READIT.EQ.0) WRITE(6,1070)	! say there is more of bulletinA
	   ELSE					! Yes, last line anyway
	      READ_COUNT = 0			! init bulletin record counter
	   END IF
	ELSE IF (READIT.EQ.0) THEN		! Else if this is not /READ
	   WRITE(6,1070)			! say there is more of bulletin 
	END IFn
 a
	RETURNa
 t
1030	FORMAT(' ERROR: Specified message was not found.')s
1040	FORMAT('+Message number: ',I4,<60-FLEN>X,A)
1050	FORMAT(' Description: ',A53)s
1060	FORMAT(' From: ',A12,' Date: ',A11,' ',A,$)
1070	FORMAT(1X,/,' Press RETURN for more...',/)
 
2000	FORMAT(A)
2010	FORMAT(1X,A)C
2020	FORMAT('+',A)
 
	END
 '
 L
 D
 I
	SUBROUTINE READ_EDITL
 R
	IMPLICIT INTEGER (A-Z)_
 L
	INCLUDE 'BULLDIR.INC'
  
	INCLUDE 'BULLFOLDER.INC'I
 
	CHARACTER*128 INPUT
 E
	OPEN(UNIT=3,FILE='SYS$LOGIN:BULL.SCR',IOSTAT=IER,
     &		STATUS='NEW',CARRIAGECONTROL='LIST')
 A
	IF (IER.NE.0) THENN
	   CALL ERRSNS(IDUMMY,IER)T
	   CALL SYS_GETMSG(IER)
	   RETURN
	END IFG
 
	WRITE(3,1050) DESCRIP		! Output bulletin header info0
	WRITE(3,1060) FROM,DATE
 c
	CALL OPEN_FILE_SHARED(1)T
  
	ILEN = 81
	DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file
	   DO WHILE (ILEN.GT.0)
	      CALL GET_BULL(I,INPUT,ILEN)
	      IF (ILEN.LT.0) THEN
		 GO TO 90E
	      ELSE IF (ILEN.GT.0) THEN.
	         WRITE (3,'(A)') INPUT(:ILEN)
	      END IFF
	   END DO
	   ILEN = 80N
	END DOS
 E
90	CLOSE (UNIT=3)			! Bulletin copy completedA
	CALL CLOSE_FILE(1)E
 E
	CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')
 T
	CALL LIB$DELETE_FILE('SYS$LOGIN:BULL.SCR;*')O
 /
1050	FORMAT(/,'Description: ',A53)
1060	FORMAT('From: ',A12,' Date: ',A11,/)0
 H
	RETURN	
	END
 W
 e
	SUBROUTINE READNEW(REDO)E
CI
C  SUBROUTINE READNEWE
CU
C  FUNCTION: Displays new non-system bulletins with prompts between bulletins.
C,
 '
	IMPLICIT INTEGER (A-Z)e
 '
	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER
 E
	INCLUDE 'BULLUSER.INC'N
 E
	INCLUDE 'BULLDIR.INC'
 F
	INCLUDE 'BULLFOLDER.INC')
 .
	COMMON /SYSTEM_FOLDERS/ SYSTEM_FLAG(FLONG),DUMMY(2)
  
	COMMON /POINT/ BULL_POINT
 
	CHARACTER INREAD*1,INPUT*80,FILE_DEF*80,NUMREAD*5
 T
	DATA LEN_FILE_DEF /0/, INREAD/0/E
 R
	LOGICAL SLOW,SLOW_TERMINALE
 R
	IF (ICHAR(INREAD).EQ.0) THEN	! If calling READNEW for first timee
	   SLOW = SLOW_TERMINAL()	! Check baud rate of terminal
	END IF				! to avoid gobs of output
 1
	LEN_P = 0			! Tells read subroutine there isI
					! no bulletin parameter
 T
1	WRITE(6,1000)			! Ask if want to read new bulletins 
 L
	CALL GET_INPUT_NUM(NUMREAD,NLEN)	! Get inputD
	CALL STR$UPCASE(NUMREAD,NUMREAD)	! Make input upper case
	READ (NUMREAD,'(I<NLEN>)',IOSTAT=IER) TEMP_READ
	IF (IER.NE.0) THEN 
	   INREAD = NUMREAD(:1)
	   IF (INREAD.EQ.'N'.OR.INREAD.EQ.'Q'.OR.INREAD.EQ.'E') THENI
	      IF (INREAD.EQ.'Q') THEN
	         WRITE (6,'(''+uit'',$)')
	      ELSE IF (INREAD.EQ.'E') THENA
	         WRITE (6,'(''+xit'',$)')
		 DO I=1,FLONG			! Just show SYSTEM folders
		    NEW_MSG(I) = NEW_MSG(I).AND.SYSTEM_FLAG(I)
		 END DO 
		 DO I=1,FLONG	! Test for new messages in SYSTEM folders=
		    IF (NEW_MSG(I).NE.0) RETURN 
		 END DOF
		 CALL EXIT
	      ELSE)
	         WRITE (6,'(''+o'',$)')
	      END IF)
	      RETURN	! If NO, exit
			! Include QUIT to be consistent with next questionN
	   ELSE
	      CALL LIB$ERASE_PAGE(1,1)E
	   END IF
	END IFI
 A
	IF (TEMP_READ.GT.0) THEN 
	   IF (TEMP_READ.LT.BULL_POINT+1.OR.TEMP_READ.GT.NBULL) THEN)
	      WRITE (6,'('' ERROR: Specified new message not found.'')')+
	      GO TO 1
	   ELSE
	      BULL_POINT = TEMP_READ - 1M
	   END IF
	END IFF
 E
	READ_COUNT = 0				! Initialize display pointerl
 i
5	CALL READ(READ_COUNT,BULL_POINT+1)	! Read next bulletinU
	FILE_POINT = BULL_POINT
	IF (READ_COUNT.EQ.0) THEN		! Is full bulletin displayed?=
	   CALL OPEN_FILE_SHARED(2)		! If so, see if more new bulls
10	   CALL READDIR(BULL_POINT+1,IER_POINT)
	   IF ((IER_POINT.EQ.BULL_POINT+2).AND.	! If system bulletin (and systemX
     &	       (SYSTEM.AND.BTEST(FOLDER_FLAG,2))) THEN	! folder) then skip it.S
	      BULL_POINT = BULL_POINT + 1
	      GO TO 10)
	   END IF
	   CALL CLOSE_FILE(2)
	END IFt
 n
12	IF (READ_COUNT.EQ.0) THEN		! Prompt user in between
	   WRITE(6,1020)			! full screens or end of bull.
	ELSEE
	   WRITE(6,1030)0
	END IFD
 ,
	CALL GET_INPUT_NOECHO(INREAD)
	CALL STR$UPCASE(INREAD,INREAD)	! Convert input to upper case
  
	IF (INREAD.EQ.'Q') THEN		! If Q , then QUIT
	   WRITE (6,'(''+Quit'',$)')F
	   RETURN
	ELSE IF (INREAD.EQ.'D') THEN	! If D , then redisplay direcotyrr
	   WRITE (6,'(''+Dir'',$)')
	   REDO = .TRUE.t
	   RETURN
	ELSE IF (INREAD.EQ.'F') THEN	! If F then copy bulletin to filey
	   WRITE (6,'(''+ '')')		! Move cursor from end of prompt linen
					! to beginning of next line.m
	   IF (LEN_FILE_DEF.EQ.0) THENk
	      CALL LIB$SYS_TRNLOG('SYS$LOGIN',LEN,FILE_DEF)
	      IER = LIB$FIND_FILE(FILE_DEF//'BULL.DIR',
     &			BULL_PARAMETER,CONTEXT)
	      IF (IER) THEN
		 FILE_DEF = BULL_PARAMETER(:LEN-1)//'.BULL]'
		 LEN_FILE_DEF = LEN + 5o
	      ELSE	
	         FILE_DEF = 'SYS$LOGIN:'A
	         LEN_FILE_DEF = 10)
	      END IF1
	   END IF
  
	   LEN_FOLDER = TRIM(FOLDER)
	   CALL GET_INPUT_PROMPT(BULL_PARAMETER,LEN_P,)
     &		'Name of file? (Default='//FILE_DEF(:LEN_FILE_DEF)//
     &		FOLDER(:LEN_FOLDER)//'.LIS) ')
 !
	   IF (LEN_P.EQ.0) THEN
	      BULL_PARAMETER = FILE_DEF(:LEN_FILE_DEF)//FOLDER(:LEN_FOLDER)
     &			//'.LIS'n
	      LEN_P = LEN_FILE_DEF + LEN_FOLDER + 4
	   END IF
 !
	   BLOCK_SAVE = BLOCK
	   LENGTH_SAVE = LENGTH
	   CALL OPEN_FILE_SHARED(2)
	   CALL OPEN_FILE_SHARED(1)		! Open BULLETIN file
	   CALL READDIR(FILE_POINT,IER)
	   IF (.NOT.SETPRV_PRIV()) THEN		! If no SETPRV, remove SYSPRVO
	      CALL DISABLE_PRIVS		! privileges when trying to
	   END IF				! create new file.
	   OPEN(UNIT=3,FILE=BULL_PARAMETER(:LEN_P),IOSTAT=IER,ERR=18,
     &		STATUS='UNKNOWN',CARRIAGECONTROL='LIST',ACCESS='APPEND')
	   WRITE(3,1050) DESCRIP		! Output bulletin header info
	   WRITE(3,1060) FROM,DATEL
	   ILEN = 81C
	   DO I=BLOCK,BLOCK+LENGTH-1		! Copy bulletin into file
	      DO WHILE (ILEN.GT.0)r
	         CALL GET_BULL(I,INPUT,ILEN)F
		 IF (ILEN.LT.0) THEN
		   GO TO 18 
		 ELSE IF (ILEN.GT.0) THENL
	            WRITE(3,'(A)') INPUT(:TRIM(INPUT))R
		 END IFT
	      END DO 
	      ILEN = 80
	   END DO
	   WRITE(6,1040) BULL_PARAMETER(:LEN_P)
						! Show name of file created.
18	   IF (IER.NE.0) THEN
	      CALL ERRSNS(IDUMMY,IER)
	      CALL SYS_GETMSG(IER)	
	   END IF
	   CLOSE (UNIT=3)			! Bulletin copy completed
	   CALL CLOSE_FILE(1)
	   CALL CLOSE_FILE(2)
	   LENGTH = LENGTH_SAVE
	   BLOCK = BLOCK_SAVE
	   CALL ENABLE_PRIVS			! Reset BYPASS privilegesr
	   GO TO 12
	ELSE IF (INREAD.EQ.'N'.AND.READ_COUNT.GT.0) THENp
				! If NEXT and last bulletins not finishedA
	   READ_COUNT = 0			! Reset read bulletin counter
	   CALL OPEN_FILE_SHARED(2)		! Look for NEXT bulletin
20	   CALL READDIR(BULL_POINT+1,IER)
	   IF (IER.NE.BULL_POINT+2) THEN	! If no NEXT bulletinh
	      CALL CLOSE_FILE(2)		! Exito
	      WRITE(6,1010)
	      RETURNe
	   ELSE IF (SYSTEM.AND.BTEST(FOLDER_FLAG,2)) THEN
	      BULL_POINT = BULL_POINT + 1	! If SYSTEM bulletin, skip it
	      GO TO 20			! Look for more bulletinsR
	   END IF
	   CALL CLOSE_FILE(2)
	ELSE IF (IER_POINT.NE.BULL_POINT+2.AND.READ_COUNT.EQ.0) THENA
	   WRITE(6,1010)H
	   RETURN
	END IF!
	IF (READ_COUNT.EQ.0.AND.SLOW) READ_COUNT = -2
	GO TO 5
 R
1000	FORMAT(' Read messages? Type N(No),E(Exit),message)
     & number, or any other key for yes: ',$)E
1010	FORMAT(' No more messages.')N
1020	FORMAT(1X,80('-'),/,' Type Q(Quit),
     & F(File it), D(Dir) or any other key for next message: ',$))
1030	FORMAT(1X,80('-'),/,' Type Q(Quit), F(File it), N(Next message),e
     & D(Dir), or other key for MORE: ',$)
1040	FORMAT(' Message written to ',A)s
1050	FORMAT(/,'Description: ',A53)
1060	FORMAT('From: ',A12,' Date: ',A20,/) 
 e
	END
  
