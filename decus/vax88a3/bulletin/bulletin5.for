From:	KBSVAX::KANE "Joseph Kane"  4-APR-1988 19:35
To:	everhart@arisia.DECNET
Subj:	forwarded mail

From uunet!rutgers.edu!Postmaster Mon Apr  4 16:47:17 1988
Received:  by kbsvax.steinmetz (1.2/1.1x Steinmetz)
	 id AA20728; Mon, 4 Apr 88 16:45:44 edt
Received: from RUTGERS.EDU by uunet.UU.NET (5.54/1.14) 
	id AA08081; Mon, 4 Apr 88 15:17:06 EDT
Received: by rutgers.edu (5.54/1.15) 
	id AF20964; Mon, 4 Apr 88 15:18:34 EDT
Date: Mon, 4 Apr 88 15:18:34 EDT
From: uunet!rutgers.edu!Postmaster (Mail Delivery Subsystem)
Subject: Returned mail: Host unknown
Message-Id: <8804041918.AF20964@rutgers.edu>
To: <MAILER-DAEMON>
Status: R
 
   ----- Transcript of session follows -----
550 <pfc-vax.mit.edu!mrl@rutgers.edu>... Host unknown
 
   ----- Unsent message follows -----
Received: by rutgers.edu (5.54/1.15) 
	id AA19947; Mon, 4 Apr 88 12:30:04 EDT
Received: from steinmetz.UUCP by uunet.UU.NET (5.54/1.14) with UUCP 
	id AA02246; Mon, 4 Apr 88 12:25:29 EDT
Received:  by kbsvax.steinmetz (1.2/1.1x Steinmetz)
	 id AA16180; Mon, 4 Apr 88 10:34:04 edt
Date:  2 Apr 88 20:17:35 EST
From: steinmetz!MAILER-DAEMON@uunet.uu.net (Mail Delivery Subsystem)
Subject: Returned mail: User unknown
Message-Id: <8804041434.AA16180@kbsvax.steinmetz>
To: MRL@pfc-vax.mit.edu
 
   ----- Transcript of session follows -----
mail11: %MAIL-E-SYNTAX, error parsing 'CRD'
550 crd.ge.com!EVERHART%ARISIA.DECNET... User unknown
 
   ----- Unsent message follows -----
Received:  by kbsvax.steinmetz (1.2/1.1x Steinmetz)
	 id AA16173; Mon, 4 Apr 88 10:34:04 edt
Received: by ge-dab.GE.COM (smail2.5)
	id AA19231; 4 Apr 88 06:43:09 EDT (Mon)
Received: by ge-rtp.GE.COM (smail2.5)
	id AA10867; 3 Apr 88 21:17:55 EST (Sun)
Received: by mcnc.mcnc.org (5.54/MCNC/10-20-87)
	id AA14779; Sat, 2 Apr 88 21:17:45 EST
From: <mcnc!rutgers.edu!pfc-vax.mit.edu!MRL>
Received: by rutgers.edu (5.54/1.15) 
	id AA02126; Sat, 2 Apr 88 21:01:12 EST
Message-Id: <8804030201.AA02126@rutgers.edu>
Received: from PFC-VAX.MIT.EDU by XX.LCS.MIT.EDU via Chaosnet; 2 Apr 88 20:19-EST
Date:  2 Apr 88 20:17:35 EST
To: xx!TENCATI@vlsi.jpl.nasa.gov, xx!MHG@mitre-bedford.arpa,
        crd.ge.com!xx!EVERHART@ARISIA.DECNET, xx!GAYMAN@ari-hq1.arpa,
        radc-softvax!xx!BACH
Subject: BULLETIN5.FOR
 
C
C  BULLETIN5.FOR, Version 3/27/88
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Programmer: Mark R. London
C
	SUBROUTINE SET_FOLDER_DEFAULT(NOTIFY,READNEW,BRIEF)
C
C  SUBROUTINE SET_FOLDER_DEFAULT
C
C  FUNCTION: Sets flag defaults for specified folder
C
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLFOLDER.INC'
 
	INCLUDE 'BULLUSER.INC'
 
	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD
 
	IF (.NOT.SETPRV_PRIV().AND.INCMD(:3).EQ.'SET') THEN
	   WRITE (6,'(
     &      '' ERROR: No privs to change all defaults.'')')
	   RETURN
	END IF
 
	CALL OPEN_FILE_SHARED(4)
	CALL READ_USER_FILE_HEADER(IER)
	IF (NOTIFY.EQ.0) CALL CLR2(NOTIFY_FLAG_DEF,FOLDER_NUMBER)
	IF (NOTIFY.EQ.1) CALL SET2(NOTIFY_FLAG_DEF,FOLDER_NUMBER)
	IF (READNEW.EQ.0) CALL CLR2(SET_FLAG_DEF,FOLDER_NUMBER)
	IF (READNEW.EQ.1) CALL SET2(SET_FLAG_DEF,FOLDER_NUMBER)
	IF (BRIEF.EQ.0) CALL CLR2(BRIEF_FLAG_DEF,FOLDER_NUMBER)
	IF (BRIEF.EQ.1) CALL SET2(BRIEF_FLAG_DEF,FOLDER_NUMBER)
	REWRITE(4) USER_HEADER
 
	IF (BRIEF.NE.-1.AND.NOTIFY.NE.-1.AND.READNEW.NE.-1) THEN
	   CALL READ_USER_FILE(IER)
	   DO WHILE (IER.EQ.0)
	      IF (TEMP_USER(:1).NE.'*') THEN
	         IF (NOTIFY.EQ.0) CALL CLR2(NOTIFY_FLAG,FOLDER_NUMBER)
	         IF (NOTIFY.EQ.1) CALL SET2(NOTIFY_FLAG,FOLDER_NUMBER)
	         IF (READNEW.EQ.0) CALL CLR2(SET_FLAG,FOLDER_NUMBER)
	         IF (READNEW.EQ.1) CALL SET2(SET_FLAG,FOLDER_NUMBER)
	         IF (BRIEF.EQ.0) CALL CLR2(BRIEF_FLAG,FOLDER_NUMBER)
	         IF (BRIEF.EQ.1) CALL SET2(BRIEF_FLAG,FOLDER_NUMBER)
	         REWRITE(4) TEMP_USER//USER_ENTRY(13:)
	      END IF
	      CALL READ_USER_FILE(IER)
	   END DO
	END IF
	CALL CLOSE_FILE(4)
 
	RETURN
	END
 
 
 
 
	SUBROUTINE REMOVE_FOLDER
C
C  SUBROUTINE REMOVE_FOLDER
C
C  FUNCTION: Removes a bulletin folder.
C
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLFOLDER.INC'
 
	INCLUDE 'BULLUSER.INC'
 
	INCLUDE 'BULLFILES.INC'
 
	EXTERNAL CLI$_ABSENT
 
	CHARACTER RESPONSE*1,TEMP*80
 
	IER = CLI$GET_VALUE('REMOVE_FOLDER',FOLDER1,LEN_T) ! Get folder name
 
	IF (IER.EQ.%LOC(CLI$_ABSENT)) THEN
	   IF (.NOT.FOLDER_SET) THEN
	      WRITE (6,'('' ERROR: No folder specified.'')')
	      RETURN
	   ELSE
	      FOLDER1 = FOLDER
	   END IF
	ELSE IF (LEN_T.GT.25) THEN
	   WRITE(6,'('' ERROR: Folder name must be < 26 characters.'')')
	   RETURN
	END IF
 
	CALL GET_INPUT_PROMPT(RESPONSE,LEN,
     &   'Are you sure you want to remove folder '
     &	 //FOLDER1(:TRIM(FOLDER1))//' (Y/N with N as default): ')
	IF (RESPONSE.NE.'y'.AND.RESPONSE.NE.'Y') THEN
	   WRITE (6,'('' Folder was not removed.'')')
	   RETURN
	END IF
 
	CALL OPEN_FILE(7)				! Open folder file
	CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER)	! See if folder exists
	FOLDER1_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//
     &		FOLDER1
 
	IF (IER.NE.0) THEN
	   WRITE (6,'('' ERROR: No such folder exists.'')')
	   GO TO 1000
	END IF
 
	IF ((FOLDER1_OWNER.NE.USERNAME.AND..NOT.SETPRV_PRIV()).OR.
     &	     FOLDER1_NUMBER.EQ.0) THEN
	   WRITE (6,'('' ERROR: You are not able to remove the folder.'')')
	   GO TO 1000
	END IF
 
	IF (FOLDER1_BBOARD(:2).EQ.'::'.AND.BTEST(FOLDER1_FLAG,2)) THEN
	   OPEN (UNIT=17,STATUS='UNKNOWN',IOSTAT=IER,
     &		RECL=256,FILE=FOLDER1_BBOARD(3:TRIM(FOLDER1_BBOARD))
     &		//'::"TASK=BULLETIN1"')
	   IF (IER.EQ.0) THEN		! Disregister remote SYSTEM folder
	      WRITE(17,'(2A)',IOSTAT=IER) 14,0
	      CLOSE (UNIT=17)
	   END IF
	END IF
 
	TEMP = FOLDER_FILE
	FOLDER_FILE = FOLDER1_FILE
	TEMPSET = FOLDER_SET
	FOLDER_SET = .TRUE.
	CALL OPEN_FILE(2)			! Remove directory file
	CALL OPEN_FILE(1)			! Remove bulletin file
	CALL CLOSE_FILE_DELETE(1)
	CALL CLOSE_FILE_DELETE(2)
	FOLDER_FILE = TEMP
	FOLDER_SET = TEMPSET
 
	DELETE (7)
 
	TEMP_NUMBER = FOLDER_NUMBER
	FOLDER_NUMBER = FOLDER1_NUMBER
	CALL SET_FOLDER_DEFAULT(0,0,0)
	FOLDER_NUMBER = TEMP_NUMBER
 
	WRITE (6,'('' Folder removed.'')')
 
	IF (FOLDER.EQ.FOLDER1) FOLDER_SET = .FALSE.
 
1000	CALL CLOSE_FILE(7)
 
	RETURN
 
	END
 
 
	SUBROUTINE SELECT_FOLDER(OUTPUT,IER)
C
C  SUBROUTINE SELECT_FOLDER
C
C  FUNCTION: Selects the specified folder.
C
C  INPUTS:
C	OUTPUT - Specifies whether status messages are outputted.
C
C  NOTES:
C	FOLDER_NUMBER is used for selecting the folder.
C	If FOLDER_NUMBER = -1, the name stored in FOLDER1 is used.
C	If FOLDER_NUMBER = -2, the name stored in FOLDER1 is used,
C	but the folder is not selected if it is remote.
C	If the specified folder is on a remote node and does not have
C	a local entry (i.e. specified via NODENAME::FOLDERNAME), then
C	FOLDER_NUMBER is set to -1.
C
 
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLFOLDER.INC'
 
	INCLUDE 'BULLFILES.INC'
 
	INCLUDE 'BULLDIR.INC'
 
	INCLUDE 'BULLUSER.INC'
 
	INCLUDE '($RMSDEF)'
	INCLUDE '($SSDEF)'
 
	COMMON /POINT/ BULL_POINT
 
	COMMON /ACCESS/ READ_ONLY
	LOGICAL READ_ONLY
 
	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD
 
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
	DATA REMOTE_SET /.FALSE./
 
	EXTERNAL CLI$_ABSENT
 
	CHARACTER*80 LOCAL_FOLDER1_DESCRIP
 
	DIMENSION FIRST_TIME(FLONG)	! Bit set for folder if folder has
	DATA FIRST_TIME /FLONG*0/	! been selected before this.
 
	IF (OUTPUT) THEN			! Get folder name
	   IER = CLI$GET_VALUE('SELECT_FOLDER',FOLDER1,FLEN)
	   IF (IER.AND.FOLDER1(FLEN-1:FLEN).EQ.'::') THEN
	      FOLDER1 = FOLDER1(:FLEN)//'GENERAL'
	   END IF
	END IF
 
	CALL OPEN_FILE_SHARED(7)			! Go find folder
 
	IF (((IER.EQ.%LOC(CLI$_ABSENT).OR.FOLDER1.EQ.'GENERAL').AND.
     &	 OUTPUT).OR.((FOLDER_NUMBER.EQ.0.OR.(FOLDER1.EQ.'GENERAL'.AND.-
     &	 FOLDER_NUMBER.LE.-1)).AND..NOT.OUTPUT)) THEN ! Select GENERALn
	   FOLDER_NUMBER = 0e
	   FOLDER1 = 'GENERAL'8
	END IFd
 b
	REMOTE_TEST = 0
 .
	IF (OUTPUT.OR.FOLDER_NUMBER.LE.-1) THEN
	   REMOTE_TEST = INDEX(FOLDER1,'::')E
	   IF (REMOTE_TEST.GT.0) THEN
	      FOLDER1_BBOARD = '::'//FOLDER1(:REMOTE_TEST-1)d
	      FOLDER1 = FOLDER1(REMOTE_TEST+2:TRIM(FOLDER1)) 
	      FOLDER1_NUMBER = -1
	      IER = 0
	   ELSE IF (INCMD(:2).EQ.'SE') THEN
	      CALL READ_FOLDER_FILE_KEYNAME_TEMPt
     &				(FOLDER1(:TRIM(FOLDER1)),IER)0
	   ELSE
	      CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER)
	   END IF
	ELSEp
	   FOLDER1_NUMBER = FOLDER_NUMBER
	   CALL READ_FOLDER_FILE_KEYNUM_TEMP(FOLDER_NUMBER,IER)
	END IF 
 s
	IF (BTEST(FOLDER1_FLAG,29)) THEN	! Error in folder flag!!
	   FOLDER1_FLAG = FOLDER1_FLAG.AND.3
	   F1_EXPIRE_LIMIT = 0z
	   CALL REWRITE_FOLDER_FILE_TEMPi
	END IF
  
	CALL CLOSE_FILE(7)8
 :
	IF (IER.EQ.0.AND.FOLDER1_BBOARD(:2).EQ.'::') THEN
	   IF (FOLDER_NUMBER.EQ.-2) RETURN	! Don't allowe
	   LOCAL_FOLDER1_FLAG = FOLDER1_FLAG 
	   LOCAL_FOLDER1_DESCRIP = FOLDER1_DESCRIPl
	   CALL CONNECT_REMOTE_FOLDER(READ_ONLY,IER)u
	   IF (IER.NE.0) THEN
	      IF (OUTPUT) THENe
	         WRITE (6,'('' ERROR: Unable to connect to folder.'')')
	      END IF-
	      RETURNI
	   END IF
	   IF (REMOTE_TEST.GT.0) THEN	! Folder specified with "::".
	      FOLDER1 = FOLDER1_BBOARD(3:TRIM(FOLDER1_BBOARD))//'::'//v
     &			FOLDER1
	      FOLDER1_NUMBER = -1
	   ELSE				! True remote folder
	      FOLDER1_DESCRIP = LOCAL_FOLDER1_DESCRIP	! Use local description
	      FOLDER1_FLAG = LOCAL_FOLDER1_FLAG		! & local flag info 
	      CALL OPEN_FILE(7)		! Update local folder informationo
              CALL READ_FOLDER_FILE_KEYNAME(FOLDER1,IER)
	      FOLDER_COM = FOLDER1_COM!
	      CALL REWRITE_FOLDER_FILEy
	      CALL CLOSE_FILE(7)
	   END IF
	   REMOTE_SET = .TRUE.S
	END IFg
 d
	IF (IER.EQ.0) THEN				! Folder founde
	   FOLDER1_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))
     &		//FOLDER1a
	   IF (BTEST(FOLDER1_FLAG,0).AND.FOLDER1_BBOARD(:2).NE.'::'
     &		.AND..NOT.SETPRV_PRIV()) THEN.
				! Is folder protected and not remote?@
	      CALL CHKACL
     &		(FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL',IER)B
	      IF (IER.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL).AND.USERNAMEn
     &		  .NE.FOLDER1_OWNER) THENr
	         CALL CHECK_ACCESSF
     &		  (FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL',USERNAME,
     &		  READ_ACCESS,WRITE_ACCESS)E
	         IF (.NOT.READ_ACCESS.AND..NOT.WRITE_ACCESS) THEN
		  IF (OUTPUT) THEN
	           WRITE(6,'('' You are not allowed to access folder.'')')D
	           WRITE(6,'('' See '',A,'' if you wish to access folder.'')')
     &			FOLDER1_OWNER(:TRIM(FOLDER1_OWNER))
		  ELSE IF (TEST2(BRIEF_FLAG,FOLDER1_NUMBER).OR.
     &			 TEST2(SET_FLAG,FOLDER1_NUMBER)) THEN
		   CALL OPEN_FILE_SHARED(4)I
		   CALL READ_USER_FILE_KEYNAME(USERNAME,IER)
		   CALL CLR2(BRIEF_FLAG,FOLDER1_NUMBER)L
		   CALL CLR2(SET_FLAG,FOLDER1_NUMBER) 
		   REWRITE (4) USER_ENTRYY
		   CALL CLOSE_FILE(4)
		  END IF
		  IER = 0L
		  RETURN
	         END IF
	      END IFE
	   ELSE					! Folder not protectedM
	      IER = SS$_ACLEMPTY.OR.SS$_NORMAL	! Indicate folder selected
	   END IF
 L
	   IF (FOLDER1_BBOARD(:2).NE.'::') THEN
	      IF (REMOTE_SET) CLOSE(UNIT=REMOTE_UNIT)
	      REMOTE_SET = .FALSE.)
	   END IF
 L
	   IF (IER) THEN)
	      FOLDER_COM = FOLDER1_COM		! Folder successfully set soE
	      FOLDER_FILE = FOLDER1_FILE	! update folder parameters
 R
	      IF (FOLDER_NUMBER.NE.0) THENE
		 FOLDER_SET = .TRUE.
	      ELSE 
		 FOLDER_SET = .FALSE.L
	      END IFF
 E
	      IF (OUTPUT.AND.INCMD(:3).NE.'DIR') THEN
		 WRITE (6,'('' Folder has been set to '',A)') 
     &		    FOLDER(:TRIM(FOLDER))//'.'
		 BULL_POINT = 0	! Reset pointer to first bulletinE
	      END IF 
  
	      IF (IER.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL).AND.USERNAME 
     &		  .NE.FOLDER_OWNER) THEN
	         IF (.NOT.WRITE_ACCESS) THEN
		   IF (OUTPUT.AND.INCMD(:3).NE.'DIR')O
     &		    WRITE (6,'('' Folder only accessible for reading.'')')
		   READ_ONLY = .TRUE.o
		 ELSE
		   READ_ONLY = .FALSE.
		 END IFI
	      ELSEO
		 READ_ONLY = .FALSE.
	      END IF
 
	      IF (FOLDER_NUMBER.GT.0) THENR
		IF (TEST_BULLCP()) THENA
		 CALL SET2(FIRST_TIME,FOLDER_NUMBER)
		ELSE IF (.NOT.TEST2(FIRST_TIME,FOLDER_NUMBER)) THENn
	       			! If first select, look for expired messages.
		 CALL OPEN_FILE(2)
		 CALL READDIR(0,IER)	! Get header info from BULLDIR.DAT 
	 	 IF (IER.EQ.1) THEN		! Is header present?
	   	    IER = COMPARE_DATE(NEWEST_EXDATE,' ') ! Yes. Any expired?
	 	    IF (IER.LE.0) CALL UPDATE  ! Need to update
		 ELSEI
		    NBULL = 0_
		 END IFT
		 CALL CLOSE_FILE(2) 
		 CALL SET2(FIRST_TIME,FOLDER_NUMBER)
	        END IFD
	      END IFR
 /
	      IF (FOLDER_NUMBER.NE.0) THENR
	        IF (OUTPUT.AND.INCMD(:3).NE.'DIR') THEN
	         DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &					F_NEWEST_BTIM)n
	         IF (DIFF.LT.0.AND.F_NBULL.GT.0) THEN 	! If new unread messages
		  CALL FIND_NEWEST_BULL			! See if we can find itF
		  IF (BULL_POINT.NE.-1) THEN
	     	    WRITE(6,'('' Type READ to read new messages.'')')
		    NEW_COUNT = F_NBULL - BULL_POINT
		    DIG = 0
		    DO WHILE (NEW_COUNT.GT.0)N
		      NEW_COUNT = NEW_COUNT / 10
		      DIG = DIG + 1)
		    END DO
		    WRITE(6,'('' There are '',I<DIG>,'' new messages.'')')
     &			F_NBULL - BULL_POINT	! Alert user if new bulletins:
		  ELSE
		    BULL_POINT = 0
		  END IF
		 END IFT
		END IF
	      END IF,
	      IER = 1
	   ELSE IF (OUTPUT) THENI
	      WRITE (6,'('' Cannot access specified folder.'')')F
	      CALL SYS_GETMSG(IER)e
	   END IF
	ELSE						! Folder not foundA
	   IF (OUTPUT) WRITE (6,'('' ERROR: Folder does not exist.'')')
	   IER = 0 
	END IFL
 
	RETURNI
 =
	END
 F
 
 T
	SUBROUTINE CONNECT_REMOTE_FOLDER(READ_ONLY,IER)
CN
C  SUBROUTINE CONNECT_REMOTE_FOLDERC
C 
C  FUNCTION: Connects to folder that is located on other DECNET node.L
CL
	IMPLICIT INTEGER (A-Z)E
 I
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
	DATA REMOTE_UNIT /15/
 L
	INCLUDE 'BULLUSER.INC'R
 F
	INCLUDE 'BULLFOLDER.INC'O
 R
	CHARACTER*12 FOLDER_BBOARD_SAVE,FOLDER_OWNER_SAVE
 I
	DIMENSION DUMMY(2)o
 .
	REMOTE_UNIT = 31 - REMOTE_UNIT1
 O
	OPEN (UNIT=REMOTE_UNIT,STATUS='UNKNOWN',IOSTAT=IER,RECL=256,
     &		FILE=FOLDER1_BBOARD(3:TRIM(FOLDER1_BBOARD))R
     &		//'::"TASK=BULLETIN1"')E
 
	IF (IER.EQ.0) THENc
	   WRITE (REMOTE_UNIT,'(2A)',IOSTAT=IER) 1,FOLDER1S
	   FOLDER_OWNER_SAVE = FOLDER1_OWNERt
	   FOLDER_BBOARD_SAVE = FOLDER1_BBOARD 
	   FOLDER_NUMBER_SAVE = FOLDER1_NUMBERF
	   IF (IER.EQ.0) THEN
	      READ(REMOTE_UNIT,'(5A)',IOSTAT=IER)IER1,READ_ONLY,e
     &		DUMMY(1),DUMMY(2),FOLDER1_COM 
	   END IF
	END IFe
 d
	IF (IER.NE.0.OR..NOT.IER1) THEN
	   CLOSE (UNIT=REMOTE_UNIT)
	   REMOTE_UNIT = 31 - REMOTE_UNIT
	   IF (IER.EQ.0.AND.FOLDER_NUMBER_SAVE.GE.0) THEN
	      IF (TEST2(BRIEF_FLAG,FOLDER_NUMBER_SAVE) 
     &		  .OR.TEST2(SET_FLAG,FOLDER_NUMBER_SAVE)) THEN
	         CALL OPEN_FILE_SHARED(4)
	         CALL READ_USER_FILE_KEYNAME(USERNAME,IER)U
	         CALL CLR2(BRIEF_FLAG,FOLDER_NUMBER_SAVE)
	         CALL CLR2(SET_FLAG,FOLDER_NUMBER_SAVE)
	         REWRITE (4) USER_ENTRY
	         CALL CLOSE_FILE(4)
	      END IF
	   END IF
	   IER = 2E
	ELSET
	   FOLDER1_BBOARD = FOLDER_BBOARD_SAVE
	   FOLDER1_NUMBER = FOLDER_NUMBER_SAVER
	   FOLDER1_OWNER = FOLDER_OWNER_SAVEI
	   CLOSE (UNIT=31-REMOTE_UNIT)r
	   IF ((FOLDER_NUMBER.NE.FOLDER1_NUMBER.AND.(DUMMY(1).NE.0e
     &		.OR.DUMMY(2).NE.0)).OR.FOLDER1_NUMBER.EQ.-1) THENm
	      LAST_READ_BTIM(1,FOLDER1_NUMBER+1) = DUMMY(1)
	      LAST_READ_BTIM(2,FOLDER1_NUMBER+1) = DUMMY(2)
	   END IF
	   IER = 0E
	END IF/
 E
	RETURN 
	END
 
 D
 
 
 C
  
 N
 L
 H
	SUBROUTINE UPDATE_FOLDER
C
C  SUBROUTINE UPDATE_FOLDERT
CR
C  FUNCTION: Updates folder info due to new message.
C_
 B
	IMPLICIT INTEGER (A-Z)E
 A
	INCLUDE 'BULLDIR.INC'
 M
	INCLUDE 'BULLFOLDER.INC')
 H
	IF (FOLDER_NUMBER.LT.0) RETURNN
 E
	CALL OPEN_FILE_SHARED(7)			! Open folder file
 M
	CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)
 L
	CALL SYS_BINTIM(NEWEST_DATE//' '//NEWEST_TIME,F_NEWEST_BTIM)O
 T
	F_NBULL = NBULL
  
	IF (FOLDER_NUMBER.EQ.0) FOLDER_FLAG = IBSET(FOLDER_FLAG,2)E
 =
	CALL REWRITE_FOLDER_FILE(
 D
	CALL CLOSE_FILE(7)1
 M
	RETURN
	END
 R
 0
  
	SUBROUTINE SHOW_FOLDERS
C 
C  SUBROUTINE SHOW_FOLDERE
CI
C  FUNCTION: Shows the information on any folder.)
CR
 
	IMPLICIT INTEGER (A-Z)D
 L
	INCLUDE 'BULLUSER.INC'D
 ,
	INCLUDE 'BULLFOLDER.INC' 
 O
	INCLUDE 'BULLFILES.INC'
 
	INCLUDE '($SSDEF)'F
 _
	INCLUDE '($RMSDEF)'
 ,
	EXTERNAL CLI$_ABSENT 
 E
	CALL OPEN_FILE_SHARED(7)			! Open folder file
 
	IF (CLI$GET_VALUE('SHOW_FOLDER',FOLDER1).NE.%LOC(CLI$_ABSENT))
     &		THEN
10	   CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER)8
	   FOLDER1_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//
     &		FOLDER1Q
	   IF (IER.NE.0) THEN
	      WRITE (6,'('' ERROR: Specified folder was not found.'')')
	      CALL CLOSE_FILE(7) 
	      RETURNM
	   ELSE
	      WRITE (6,1010) FOLDER1,FOLDER1_OWNER,
     &			FOLDER1_DESCRIP(:TRIM(FOLDER1_DESCRIP))
	   END IF
	ELSE IF (FOLDER_SET) THEN
	   WRITE (6,1000) FOLDER,FOLDER_OWNER,
     &			FOLDER_DESCRIP(:TRIM(FOLDER_DESCRIP))
	   FOLDER1_FILE = FOLDER_FILE
	   FOLDER1_BBOARD = FOLDER_BBOARD
	   FOLDER1_BBEXPIRE = FOLDER_BBEXPIRE
	   FOLDER1_NUMBER = FOLDER_NUMBER
	   FOLDER1_FLAG = FOLDER_FLAG
	   F1_EXPIRE_LIMIT = F_EXPIRE_LIMIT
	ELSEs
	   FOLDER1 = 'GENERAL'F
	   GO TO 10
	END IFG
  
	IF (CLI$PRESENT('FULL')) THEN
	   CALL CHKACLp
     &		(FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL',IER)R
	   IF (IER.EQ.(SS$_ACLEMPTY.OR.SS$_NORMAL).OR.(.NOT.IER)) THEN 
	     WRITE (6,'('' Folder is not a private folder.'')')
	   ELSE
	     CALL CHECK_ACCESSS
     &		(FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL',USERNAME,
     &		 READ_ACCESS,WRITE_ACCESS)
	     IF (WRITE_ACCESS)/
     &	     CALL SHOWACL(FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL') 
	   END IF
	   IF (SETPRV_PRIV().OR.USERNAME.EQ.FOLDER1_OWNER) THEN
	      IF (FOLDER1_BBOARD(:2).EQ.'::') THENE
		 FLEN = TRIM(FOLDER1_BBOARD)
		 WRITE (6,'('' Folder is located on node '',
     &		   A<FLEN-2>,''.'')') FOLDER1_BBOARD(3:FLEN)
	      ELSE IF (FOLDER1_BBOARD.NE.'NONE') THEN
		 FLEN = TRIM(FOLDER1_BBOARD)
		 IF (FLEN.GT.0) THEN
 	          WRITE (6,'('' BBOARD for folder is '',A<FLEN>,''.'')')
     &		 	FOLDER1_BBOARD(:FLEN)N
		 END IFU
		 IF ((USERB.EQ.0.AND.GROUPB.EQ.0).OR.BTEST(USERB,31)) THEN
 		  WRITE (6,'('' BBOARD was specified with /SPECIAL.'')')o
		  IF (BTEST(GROUPB,31)) THEN
		   WRITE (6,'('' BBOARD was specified with /VMSMAIL.'')')S
		  END IF
		 END IFU
		 IF (FOLDER1_BBEXPIRE.GT.0) THEN
		  WRITE (6,'('' BBOARD expiration is '',I3,'' days.'')')
     &			FOLDER1_BBEXPIREE
		 ELSE,
		  WRITE (6,'('' BBOARD messages will not expire.'')')L
		 END IFF
	      ELSEU
	         WRITE (6,'('' No BBOARD has been defined.'')')
	      END IFF
	      IF (BTEST(FOLDER1_FLAG,2)) THEN
		 WRITE (6,'('' SYSTEM has been set.'')')
	      END IF
	      IF (BTEST(FOLDER1_FLAG,1)) THEN
		 WRITE (6,'('' DUMP has been set.'')')
	      END IFR
	      IF (F1_EXPIRE_LIMIT.GT.0) THENR
		 WRITE (6,'('' EXPIRATION limit is '',I3,'' days.'')')
     &			F1_EXPIRE_LIMIT
	      END IF)
	      CALL OPEN_FILE_SHARED(4)	
	      CALL READ_USER_FILE_HEADER(IER)
	      IF (TEST2(SET_FLAG_DEF,FOLDER1_NUMBER)) THEN
	       IF (TEST2(BRIEF_FLAG_DEF,FOLDER1_NUMBER)) THEN
		 WRITE (6,'('' Default is BRIEF.'')')F
	       ELSE
		 WRITE (6,'('' Default is READNEW.'')')3
	       END IF
	      ELSE,
	       IF (TEST2(BRIEF_FLAG_DEF,FOLDER1_NUMBER)) THEN
		 WRITE (6,'('' Default is SHOWNEW.'')')e
	       ELSE
		 WRITE (6,'('' Default is NOREADNEW.'')')I
	       END IF
	      END IFN
	      IF (TEST2(NOTIFY_FLAG_DEF,FOLDER1_NUMBER)) THEN
		 WRITE (6,'('' Default is NOTIFY.'')')
	      ELSE.
		 WRITE (6,'('' Default is NONOTIFY.'')')
	      END IFo
	      CALL CLOSE_FILE(4)'
	   END IF
	END IF=
 R
	CALL CLOSE_FILE(7)R
 _
	RETURNA
 .
1000	FORMAT(' Current folder is: ',A25,' Owner: ',A12,
     &		' Description: ',/,1X,A)
1010	FORMAT(' Folder name is: ',A25,' Owner: ',A12,L
     &		' Description: ',/,1X,A)
	END
  
 O
	SUBROUTINE DIRECTORY_FOLDERS(FOLDER_COUNT) 
C 
C  SUBROUTINE DIRECTORY_FOLDERSp
Cd
C  FUNCTION: Display all FOLDER entries.
CD
	IMPLICIT INTEGER (A - Z)o
 o
	INCLUDE 'BULLFOLDER.INC'.
 1
	INCLUDE 'BULLUSER.INC'n
 
	COMMON /PAGE/ PAGE_LENGTH,PAGINGE
	LOGICAL PAGING 
  
	DATA SCRATCH_D1/0/I
 L
	CHARACTER*17 DATETIME
  
	EXTERNAL CLI$_NEGATED,CLI$_PRESENT	
 D
	IF (FOLDER_COUNT.GT.0) GO TO 50		! Skip init steps if this is
						! not the 1st page of folder
 
	IF (CLI$PRESENT('DESCRIBE')) THEN
	   NLINE = 2	! Include folder descriptor if /DESCRIBE specified
	ELSEC
	   NLINE = 1T
	END IFM
 F
CE
C  Folder listing is first buffered into temporary memory storage before
C  being outputted to the terminal.  This is to be able to quickly close the
C  folder file, and to avoid the possibility of the user holding the screen,
C  and thus causing the folder file to stay open.  The temporary memory
C  is structured as a linked-list queue, where SCRATCH_D1 points to the header
C  of the queue.  See BULLSUBS.FOR for more description of the queue.e
C,
	CALL INIT_QUEUE(SCRATCH_D1,FOLDER1_INFO)U
	SCRATCH_D = SCRATCH_D1s
 i
	CALL OPEN_FILE_SHARED(7)		! Get folder file
 
	NUM_FOLDER = 0D
	IER = 0
	FOLDER1 = '                         '	! Start folder search
	DO WHILE (IER.EQ.0)			! Copy all bulletins from file.
	   CALL READ_FOLDER_FILE_TEMP(IER)
	   IF (IER.EQ.0) THEN
	      NUM_FOLDER = NUM_FOLDER + 1
	      CALL WRITE_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,FOLDER1_INFO)E
	   END IF
	END DO
 
	CALL CLOSE_FILE(7)			! We don't need file anymore
 D
	IF (NUM_FOLDER.EQ.0) THEN
	   WRITE (6,'('' There are no folders.'')')
	   RETURN
	END IFi
 o
Ce
C  Folder entries are now in queue.  Output queue entries to screen.
CT
 O
	SCRATCH_D = SCRATCH_D1			! Init queue pointer to header
 C
	FOLDER_COUNT = 1			! Init folder number counter
 
50	CALL LIB$ERASE_PAGE(1,1)		! Clear the screen_
 E
	WRITE (6,'(1X,''Folder'',22X,''Last message'',7X,''Messages'',
     &		2X,''Owner'',/,1X,80(''-''))')
 '
	IF (.NOT.PAGING) THEN
	   DISPLAY = (NUM_FOLDER-FOLDER_COUNT+1)*NLINE+2R
	ELSE&
	   DISPLAY = MIN((NUM_FOLDER-FOLDER_COUNT+1)*NLINE+2,PAGE_LENGTH-4)
			! If more entries than page size, truncate output
	END IFO
 R
	DO I=FOLDER_COUNT,FOLDER_COUNT+(DISPLAY-2)/NLINE-1 
	   CALL READ_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,FOLDER1_INFO)N
	   DIFF = COMPARE_BTIM,
     &			(LAST_READ_BTIM(1,FOLDER1_NUMBER+1),F1_NEWEST_BTIM)
	   IF (F1_NBULL.GT.0) THEND
	      CALL SYS$ASCTIM(,DATETIME,F1_NEWEST_BTIM,)S
	   ELSE
	      DATETIME = '      NONE'
	   END IF
	   IF (DIFF.GE.0.OR.F1_NBULL.EQ.0) THEN
	      WRITE (6,1000) ' '//FOLDER1,DATETIME,F1_NBULL,FOLDER1_OWNER
	   ELSE
	      WRITE (6,1000) '*'//FOLDER1,DATETIME,F1_NBULL,FOLDER1_OWNER
	   END IF
	   IF (NLINE.EQ.2) WRITE (6,'(1X,A)') FOLDER1_DESCRIP
	   FOLDER_COUNT = FOLDER_COUNT + 1	! Update folder counterL
	END DO_
 G
	IF (FOLDER_COUNT.GT.NUM_FOLDER) THEN	! Outputted all entries?
	   FOLDER_COUNT = 0			! Yes. Set counter to 0.
	ELSE 
	   WRITE(6,1010)			! Else say there are moreD
	END IF 
 O
	RETURNB
 =
1000	FORMAT(1X,A26,2X,A17,2X,I8,2X,A12) 
1010	FORMAT(1X,/,' Press RETURN for more...',/)T
 
	END
 F
 E
	SUBROUTINE SET_ACCESS(ACCESS)
CM
C  SUBROUTINE SET_ACCESS
C(
C  FUNCTION: Set access on folder for specified ID._
CD
C  PARAMETERS:
C	ACCESS  -  Logical: If .true., grant access, if .false. deny access(
C
  
	IMPLICIT INTEGER (A-Z)D
 /
	INCLUDE 'BULLFOLDER.INC'D
 
	INCLUDE 'BULLUSER.INC'
 B
	INCLUDE 'BULLFILES.INC'
  
	INCLUDE '($SSDEF)'L
 T
	LOGICAL ACCESS,ALL,READONLY
  
	EXTERNAL CLI$_ABSENT.
 _
	CHARACTER ID*25,RESPONSE*1E
 A
	IF (CLI$PRESENT('ALL')) THENI
	   ALL = .TRUE.
	ELSE
	   ALL = .FALSE.B
	END IFR
 R
	IF (CLI$PRESENT('READONLY')) THEN
	   READONLY = .TRUE.
	ELSEE
	   READONLY = .FALSE.
	END IF)
 L
	IER = CLI$GET_VALUE('ACCESS_FOLDER',FOLDER1,LEN) ! Get folder name
 N
	IF (IER.EQ.%LOC(CLI$_ABSENT)) THENE
	   FOLDER1 = FOLDER
	ELSE IF (LEN.GT.25) THENL
	   WRITE(6,'('' ERROR: Folder name must be < 26 characters.'')')D
	   RETURN
	END IFT
  
	IF (.NOT.ALL) THENS
	   IER = CLI$GET_VALUE('ACCESS_ID',ID,LEN) 	! Get IDr
	   IF (LEN.GT.25) THENR
	      WRITE(6,'('' ERROR: ID name must be < 26 characters.'')')
	      RETURNO
	   END IF
	END IFU
 '
	CALL OPEN_FILE(7)		! Open folder file
	CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER)	! See if it existsN
	OLD_FOLDER1_FLAG = FOLDER1_FLAG
	CALL CLOSE_FILE(7)V
 E
	IF ((.NOT.ALL).AND.(ID.EQ.FOLDER1_OWNER)) THEN 
	 WRITE (6,'( 
     &	  '' ERROR: Cannot modify access for owner of folder.'')')E
	 RETURN
	END IF(
 I
	IF (IER.NE.0) THEN/
	   WRITE (6,'('' ERROR: No such folder exists.'')')
	ELSE IF (FOLDER1_OWNER.NE.USERNAME.AND..NOT.SETPRV_PRIV()) THEN
	   WRITE (6, 
     &	'('' ERROR: You are not able to modify access to the folder.'')')
	ELSE	
	   FOLDER1_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//
     &		FOLDER1
	   CALL CHKACL)
     &		(FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL',IER)D
	   IF (IER.EQ.(SS$_ACLEMPTY.OR.SS$_NORMAL)) THENR
	     IF ((ALL.AND..NOT.READONLY).OR.(.NOT.ACCESS)) THEN
	        WRITE (6,'('' ERROR: Folder is not a private folder.'')')
		RETURN
	     END IF
	     CALL GET_INPUT_PROMPT(RESPONSE,LEN, 
     &      'Folder is not private. Do you want to make it so? (Y/N): ')
	     IF (RESPONSE.NE.'y'.AND.RESPONSE.NE.'Y') THENI
	       WRITE (6,'('' Folder access was not changed.'')')_
	       RETURN
	     ELSE
	       FOLDER1_FLAG = IBSET(FOLDER1_FLAG,0)
	       IF (READONLY.AND.ALL) THEN
	          CALL ADD_ACL('*','R',IER)
	       ELSE
	          CALL ADD_ACL('*','NONE',IER)_
	       END IF
	       CALL ADD_ACL(FOLDER1_OWNER,'R+W+C',IER)S
	       IF (ALL) THEN		! All finished, so exit
	        WRITE (6,'('' Access to folder has been modified.'')')O
		GOTO 100
	       END IF
	     END IF
	   END IF
	   IF (ACCESS) THEN
	      IF (.NOT.ALL) THEN(
	         IF (READONLY) THEN
	            CALL ADD_ACL(ID,'R',IER)R
		 ELSE3
	            CALL ADD_ACL(ID,'R+W',IER).
		 END IFN
	      ELSER
	         IF (READONLY) THEN
	            CALL ADD_ACL('*','R',IER)
		 ELSEr
		    CALL DEL_ACL(' ','R+W',IER) 
		    FOLDER1_FLAG = IBCLR(FOLDER1_FLAG,0)
		 END IF.
	      END IF.
	   ELSE
	      IF (ALL) THEN
		 CALL DEL_ACL('*','R',IER)
	      ELSEh
	         CALL DEL_ACL(ID,'R+W',IER)
	         IF (.NOT.IER) CALL DEL_ACL(ID,'R',IER)
	      END IF.
	   END IF
	   IF (.NOT.IER) THEN
	      WRITE(6,'('' ERROR: Cannot modify ACL of folder files.'')')
	      CALL SYS_GETMSG(IER)&
	   ELSE
	      WRITE (6,'('' Access to folder has been modified.'')')t
100	      IF (OLD_FOLDER1_FLAG.NE.FOLDER1_FLAG) THEN
	       CALL OPEN_FILE(7)		! Open folder file 
	       OLD_FOLDER1_FLAG = FOLDER1_FLAGA
	       CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER) 
	       FOLDER1_FLAG = OLD_FOLDER1_FLAGA
	       CALL REWRITE_FOLDER_FILE_TEMPe
	       CALL CLOSE_FILE(7)
	      END IF_
	   END IF
	END IFE
 
	RETURN(
 (
	END
 T
  
 i
	SUBROUTINE CHKACL(FILENAME,IERACL)_
CI
C  SUBROUTINE CHKACL
C
C  FUNCTION: Checks ACL of given file.
CL
C  PARAMETERS:
C	FILENAME - Name of file to check.A
C	IERACL   - Error returned for attempt to open file.L
CD
 F
	IMPLICIT INTEGER (A-Z)W
 E
	CHARACTER*(*) FILENAME.
 '
	INCLUDE '($ACLDEF)'
	INCLUDE '($SSDEF)'i
 E
	CHARACTER*255 ACLENT,ACLSTR
  
	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(255,ACL$C_READACL,%LOC(ACLENT))
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist
 O
	IERACL=SYS$CHANGE_ACL(,ACL$C_FILE,FILENAME,%VAL(ACL_ITMLST),,,)
 F
	IF (IERACL.EQ.SS$_ACLEMPTY) THEN	
	   IERACL = SS$_NORMAL.OR.IERACL)
	END IF 
 S
	RETURNI
	END
 '
 f
 t
	SUBROUTINE CHECK_ACCESS(FILENAME,USERNAME,READ_ACCESS,WRITE_ACCESS)
CF
C  SUBROUTINE CHECK_ACCESS
CE
C  FUNCTION: Checks ACL of given file.
Cn
C  PARAMETERS:
C	FILENAME - Name of file to check.t
C	USERNAME - Name of user to check access for.
C	READ_ACCESS - Error returned indicating read access.
C	WRITE_ACCESS - Error returned indicating write access.
C 
C  NOTE: SYS$CHECK_ACCESS is only available under V4.4 or later.
C	If you have an earlier version, comment out the lines which call
C	it and set both READ_ACCESS and WRITE_ACCESS to 1, which will_
C	allow program to run, but will not allow READONLY access feature.A
CR
  
	IMPLICIT INTEGER (A-Z)L
 N
	CHARACTER FILENAME*(*),USERNAME*(*),ACE*255,OUTPUT*80
 	
	INCLUDE '($ACLDEF)'
	INCLUDE '($CHPDEF)'
	INCLUDE '($ARMDEF)'
 
	IF (SETPRV_PRIV()) THEN
	   READ_ACCESS = 1 
	   WRITE_ACCESS = 1
	   RETURN
	END IFs
 i
	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(4,CHP$_FLAGS,%LOC(FLAGS))
	CALL ADD_2_ITMLST(4,CHP$_ACCESS,%LOC(ACCESS))
	CALL ADD_2_ITMLST(LEN(ACE),CHP$_MATCHEDACE,%LOC(ACE))
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist
  
	FLAGS = 0		! Default is no access
 a
	ACCESS = ARM$M_READ	! Check if user has read access
	READ_ACCESS=SYS$CHECK_ACCESS(ACL$C_FILE,FILENAME,USERNAME,D
     &		%VAL(ACL_ITMLST))o
 h
	IF (.NOT.SETPRV_PRIV().AND.ICHAR(ACE(:1)).NE.0) THENq
	   CALL SYS$FORMAT_ACL(ACE,,OUTPUT,,,,)
	   IF (INDEX(OUTPUT,'=*').NE.0.AND.
     &		INDEX(OUTPUT,'READ').EQ.0) READ_ACCESS = 0
	END IFF
 E
	ACCESS = ARM$M_WRITE	! Check if user has write access
	WRITE_ACCESS=SYS$CHECK_ACCESS(ACL$C_FILE,FILENAME,USERNAME,
     &		%VAL(ACL_ITMLST))R
 _
	IF (.NOT.SETPRV_PRIV().AND.ICHAR(ACE(:1)).NE.0) THENN
	   CALL SYS$FORMAT_ACL(ACE,,OUTPUT,,,,)
	   IF (INDEX(OUTPUT,'=*').NE.0.AND.
     &		INDEX(OUTPUT,'WRITE').EQ.0) WRITE_ACCESS = 0
	END IF 
 d
	RETURN 
	END
 o
 
 
  
	SUBROUTINE SHOWACL(FILENAME)T
C6
C  SUBROUTINE SHOWACLe
C'
C  FUNCTION: Shows users who are allowed to read private bulletin.
C 
C  PARAMETERS:
C	FILENAME - Name of file to check.=
CR
	IMPLICIT INTEGER (A-Z)i
 r
	INCLUDE '($ACLDEF)'
 O
	CHARACTER*(*) FILENAMEu
 r
	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(4,ACL$C_ACLLENGTH,%LOC(ACLLENGTH)),
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist
 1
	IER = SYS$CHANGE_ACL(,ACL$C_FILE,FILENAME,%VAL(ACL_ITMLST),,,)L
 -
	CALL LIB$GET_VM(ACLLENGTH+8,ACLSTR)
	CALL MAKE_CHAR(%VAL(ACLSTR),ACLLENGTH,ACLLENGTH)A
 L
	CALL READACL(FILENAME,%VAL(ACLSTR),ACLLENGTH)
 c
	RETURNt
	END
 O
 R
 D
	SUBROUTINE FOLDER_FILE_ROUTINES
 Y
	IMPLICIT INTEGER (A-Z)A
 U
	CHARACTER*(*) KEY_NAMEC
 ,
	INCLUDE 'BULLFOLDER.INC'C
 A
	ENTRY WRITE_FOLDER_FILE(IER)T
 1
	DO WHILE (REC_LOCK(IER))T
	   WRITE (7,IOSTAT=IER) FOLDER_COM
	END DOL
 Y
	RETURN,
 E
	ENTRY REWRITE_FOLDER_FILE
 E
	REWRITE (7) FOLDER_COM 
 N
	RETURNN
 F
	ENTRY REWRITE_FOLDER_FILE_TEMPQ
  
	REWRITE (7) FOLDER1_COM
  
	RETURN1
 T
	ENTRY READ_FOLDER_FILE(IER)
  
	DO WHILE (REC_LOCK(IER)) 
	   READ (7,IOSTAT=IER) FOLDER_COM
	END DO
  
	RETURN 
 F
	ENTRY READ_FOLDER_FILE_TEMP(IER)L
 1
	DO WHILE (REC_LOCK(IER))=
	   READ (7,IOSTAT=IER) FOLDER1_COMn
	END DOD
 _
	RETURN 
 L
	ENTRY READ_FOLDER_FILE_KEYNUM(KEY_NUMBER,IER)
 e
	SAVE_FOLDER_NUMBER = FOLDER_NUMBER 
 n
	DO WHILE (REC_LOCK(IER))T
	   READ (7,KEY=KEY_NUMBER,KEYID=1,IOSTAT=IER) FOLDER_COMR
	END DO0
 F
	FOLDER_NUMBER = SAVE_FOLDER_NUMBER0
 F
	RETURN/
 P
	ENTRY READ_FOLDER_FILE_KEYNUM_TEMP(KEY_NUMBER,IER)O
 N
	DO WHILE (REC_LOCK(IER)) 
	   READ (7,KEY=KEY_NUMBER,KEYID=1,IOSTAT=IER) FOLDER1_COM
	END DOc
 e
	RETURND
  
	ENTRY READ_FOLDER_FILE_KEYNAME_TEMP(KEY_NAME,IER)
 c
	DO WHILE (REC_LOCK(IER))(
	   READ (7,KEY=KEY_NAME,KEYID=0,IOSTAT=IER) FOLDER1_COM
	END DO
 
	RETURN'
 L
	ENTRY READ_FOLDER_FILE_KEYNAME(KEY_NAME,IER)I
 U
	DO WHILE (REC_LOCK(IER))L
	   READ (7,KEY=KEY_NAME,KEYID=0,IOSTAT=IER) FOLDER_COMA
	END DO*
 R
	RETURNE
 A
	END
 I
 E
	SUBROUTINE USER_FILE_ROUTINES
 .
	IMPLICIT INTEGER (A-Z)E
 
	CHARACTER*(*) KEY_NAMEE
 T
	INCLUDE 'BULLUSER.INC'E
 N
	CHARACTER*12 SAVE_USERNAMED
 Y
	ENTRY READ_USER_FILE(IER)
 =
	SAVE_USERNAME = USERNAMEE
 F
	DO WHILE (REC_LOCK(IER))m
	   READ (4,IOSTAT=IER) USER_ENTRY
	END DO
  
	TEMP_USER = USERNAME 
	USERNAME = SAVE_USERNAMER
 (
	RETURNR
  
	ENTRY READ_USER_FILE_KEYNAME(KEY_NAME,IER) 
 E
	SAVE_USERNAME = USERNAMEO
 L
	DO WHILE (REC_LOCK(IER))_
	   READ (4,KEY=KEY_NAME,IOSTAT=IER) USER_ENTRY.
	END DOE
 
	USERNAME = SAVE_USERNAMEI
	TEMP_USER = KEY_NAMEr
 e
	RETURN
  
	ENTRY READ_USER_FILE_HEADER(IER)'
 C
	DO WHILE (REC_LOCK(IER))l
	   READ (4,KEY='            ',IOSTAT=IER) USER_HEADER
	END DOi
 t
	RETURN
 D
	ENTRY WRITE_USER_FILE_NEW(IER)L
 L
	SET_FLAG(1) = SET_FLAG_DEF(1)
	SET_FLAG(2) = SET_FLAG_DEF(2)
	BRIEF_FLAG(1) = BRIEF_FLAG_DEF(1)
	BRIEF_FLAG(2) = BRIEF_FLAG_DEF(2)
	NOTIFY_FLAG(1) = NOTIFY_FLAG_DEF(1)
	NOTIFY_FLAG(2) = NOTIFY_FLAG_DEF(2)
  
	ENTRY WRITE_USER_FILE(IER)'
 )
	DO WHILE (REC_LOCK(IER))E
	   WRITE (4,IOSTAT=IER) USER_ENTRYN
	END DOT
 6
	RETURN&
 '
	END
 Y
 a
 n
 a
  
	SUBROUTINE SET_GENERIC(GENERIC)
C
C  SUBROUTINE SET_GENERICF
CE
C  FUNCTION: Enables or disables "GENERIC" display, i.e. displayingK
C	general bulletins continually for a certain amount of days.R
C
	IMPLICIT INTEGER (A-Z)P
 O
	INCLUDE 'BULLUSER.INC' 
 F
	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER
 E
	IF (.NOT.SETPRV_PRIV()) THENo
	   WRITE (6,'(U
     &      '' ERROR: No privs to change GENERIC.'')')
	   RETURN
	END IFl
  
	IER = CLI$GET_VALUE('USERNAME',TEMP_USER)
 )
	CALL OPEN_FILE_SHARED(4).
 .
	CALL READ_USER_FILE_KEYNAME(TEMP_USER,IER)'
  
	IF (IER.EQ.0) THEN 
	   IF (GENERIC) THEN 
	      IF (CLI$PRESENT('DAYS')) THEN
	         IER = CLI$GET_VALUE('DAYS',BULL_PARAMETER)
	         CALL LIB$MOVC3(4,%REF(BULL_PARAMETER),NEW_FLAG(2))
	      ELSEL
		 NEW_FLAG(2) = '   7'_
	      END IFF
	   ELSE
	      NEW_FLAG(2) = 0
	   END IF
	   REWRITE (4) TEMP_USER//USER_ENTRY(13:)
	ELSEt
	   WRITE (6,'('' ERROR: Specified username not found.'')')'
	END IFO
 1
	CALL CLOSE_FILE(4) 
  
	RETURN 
	END
 
  
	SUBROUTINE SET_LOGIN(LOGIN)
C.
C  SUBROUTINE SET_LOGIN(
CD
C  FUNCTION: Enables or disables bulletin display at login.
C 
	IMPLICIT INTEGER (A-Z)'
 '
	INCLUDE 'BULLUSER.INC' 
 S
	CHARACTER TODAY*23D
 Y
	DIMENSION NOLOGIN_BTIM(2)
 C
	CALL SYS$ASCTIM(,TODAY,,)		! Get the present time
 I
	IF (.NOT.SETPRV_PRIV()) THENR
	   WRITE (6,'(
     &      '' ERROR: No privs to change LOGIN.'')')
	   RETURN
	END IF(
 ,
	IER = CLI$GET_VALUE('USERNAME',TEMP_USER)
 I
	CALL OPEN_FILE_SHARED(4)(
 T
	CALL READ_USER_FILE_KEYNAME(TEMP_USER,IER).
  
	CALL SYS_BINTIM('5-NOV-2956',NOLOGIN_BTIM)E
	IF (IER.EQ.0) THEN 
	   IF (LOGIN.AND.COMPARE_BTIM(LOGIN_BTIM,NOLOGIN_BTIM).EQ.0) THEN
	      CALL SYS_BINTIM(TODAY,LOGIN_BTIM)
	   ELSE IF (.NOT.LOGIN) THEN	
	      LOGIN_BTIM(1) = NOLOGIN_BTIM(1)
	      LOGIN_BTIM(2) = NOLOGIN_BTIM(2)
	   END IF
	   REWRITE (4) TEMP_USER//USER_ENTRY(13:)
	ELSE 
	   WRITE (6,'('' ERROR: Specified username not found.'')') 
	END IFL
 =
	CALL CLOSE_FILE(4) 
  
	RETURNW
	END
 E
 I
 T
 e
  
	SUBROUTINE GET_UAF(USERNAME,USER,GROUP,ACCOUNT,FLAGS,IER)
 
	IMPLICIT INTEGER (A-Z)T
  
	PARAMETER UAF$V_DISACNT = 4, UAF$L_UIC = '24'X 
	PARAMETER UAF$L_ACCOUNT = 53T
	PARAMETER UAF$L_FLAGS = '1D4'XL
	PARAMETER INPUT_LEN = UAF$L_FLAGS + 4
  
	CHARACTER INPUT*(INPUT_LEN),USERNAME*(*),ACCOUNT*(*)o
  
	EQUIVALENCE (INPUT(UAF$L_UIC+1:),USER2)
	EQUIVALENCE (INPUT(UAF$L_UIC+3:),GROUP2)'
	EQUIVALENCE (INPUT(UAF$L_FLAGS+1:),FLAGS2)T
 2
	INTEGER*2 USER2,GROUP2L
 N
	CALL OPEN_FILE_SHARED(8) 
 t
        READ (8,KEY=USERNAME,IOSTAT=IER) INPUT
						! Move pointer to top of fileG
 a
	CALL CLOSE_FILE(8)O
 I
	IF (IER.EQ.0) THEN,
	   FLAGS = FLAGS2
	   IER = 1S
	   USER = USER2
	   GROUP = GROUP2
	   ACCOUNT = INPUT(UAF$L_ACCOUNT:UAF$L_ACCOUNT+7)
	END IFR
 R
	RETURN
	END
 t
 S
 O
	SUBROUTINE DCLEXH(EXIT_ROUTINE)
 R
	IMPLICIT INTEGER (A-Z)
 
	INTEGER*4 EXBLK(4)C
 S
	EXBLK(2) = EXIT_ROUTINE
	EXBLK(3) = 1l
	EXBLK(4) = %LOC(EXBLK(4))
 A
	CALL SYS$DCLEXH(EXBLK(1))
 	
	RETURN-
	END
 u
  
 c
 k
	SUBROUTINE FULL_DIR(INDEX_COUNT) 
Cu
C	Add INDEX command to BULLETIN, display directories of ALL 
C	folders. Added per request of a faculty member for his private
C	board. Changes to BULLETIN.FOR should be fairly obvious.
C 
C	Brian Nelson, Brian@uoft02.bitnet (or .ccnet, node 8.2)C
C 
	IMPLICIT INTEGER (A-Z)i
 w
	INCLUDE 'BULLDIR.INC'
	INCLUDE 'BULLFILES.INC'
	INCLUDE 'BULLFOLDER.INC'R
	INCLUDE 'BULLUSER.INC'A
 L
	COMMON /POINT/ BULL_POINT
 S
	DATA FOLDER_Q1/0/
 U
	BULL_POINT = 0E
 $
	IF (NUM_FOLDERS.GT.0.AND..NOT.CLI$PRESENT('RESTART')
     &		.AND.INDEX_COUNT.EQ.1) THENA
	   INDEX_COUNT = 2_
	   DIR_COUNT = 0U
	END IFI
 
	IF (INDEX_COUNT.EQ.1) THENa
	  CALL INIT_QUEUE(FOLDER_Q1,FOLDER1_COM)L
 ,
	  FOLDER_Q = FOLDER_Q1_
	  CALL OPEN_FILE_SHARED(7)		 ! Get folder fileT
 T
	  NUM_FOLDERS = 0
	  IER = 0
	  DO WHILE (IER.EQ.0)			! Copy all bulletins from filei
	    CALL READ_FOLDER_FILE_TEMP(IER)
	    IF (IER.EQ.0) THEN_
	      NUM_FOLDERS = NUM_FOLDERS + 1
	      CALL WRITE_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER1_COM)
	    END IFA
	  END DOo
 h
	  CALL CLOSE_FILE(7)			 ! We don't need file anymoreq
  
	  FOLDER_Q = FOLDER_Q1			! Init queue pointer to header
	  WRITE (6,1000)
	  WRITE (6,1020)T
	  DO J = 1,NUM_FOLDERSS
	   CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER1_COM)
	   WRITE (6,1030)
     &		FOLDER1(:15),F1_NBULL,FOLDER1_DESCRIP(:61)
	  END DO 
	  WRITE (6,1060))
	  FOLDER_Q = FOLDER_Q1			! Init queue pointer to header
	  INDEX_COUNT = 2
	  DIR_COUNT = 0
	  RETURN 
	ELSE IF (INDEX_COUNT.EQ.2) THEN
	 IF (DIR_COUNT.EQ.0) THEN
	  F1_NBULL = 0C
	  DO WHILE (NUM_FOLDERS.GT.0.AND.F1_NBULL.EQ.0)
	     NUM_FOLDERS = NUM_FOLDERS - 1
	     CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER1_COM)
	     IF (F1_NBULL.GT.0) THENl
	      FOLDER_NUMBER = -1
	      CALL SELECT_FOLDER(.FALSE.,IER)
	      IF (.NOT.IER) F1_NBULL = 0C
	     END IF
	  END DOA
 T
	  IF (F1_NBULL.EQ.0) THEN
	     WRITE (6,1050)
	     INDEX_COUNT = 0_
	     RETURN
	  END IFH
	 END IF
     ,
	 CALL DIRECTORY(DIR_COUNT))
 G
	 IF (DIR_COUNT.GT.0) RETURN
 =
	 IF (NUM_FOLDERS.GT.0) THEN
	    WRITE (6,1040)T
	 ELSE
	    INDEX_COUNT = 0
	 END IF
	END IF
 L
	RETURNA
 V
1000	FORMAT (' The following folders are present'/)I
1020	FORMAT (' Name	       Count Description'/)D
1030	FORMAT (1X,A15,I3,1X,A)
1040	FORMAT (' Type Return to continue to the next folder...')
1050	FORMAT (' End of folder search.')
1060	FORMAT (' Type Return to continue...')
  
	END
 C