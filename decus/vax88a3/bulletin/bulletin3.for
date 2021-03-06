From:	KBSVAX::KANE "Joseph Kane"  4-APR-1988 19:32
To:	everhart@arisia.DECNET
Subj:	

From uunet!rutgers.edu!Postmaster Mon Apr  4 16:45:27 1988
Received:  by kbsvax.steinmetz (1.2/1.1x Steinmetz)
	 id AA20676; Mon, 4 Apr 88 16:43:20 edt
Received: from RUTGERS.EDU by uunet.UU.NET (5.54/1.14) 
	id AA07937; Mon, 4 Apr 88 15:11:10 EDT
Received: by rutgers.edu (5.54/1.15) 
	id AD20964; Mon, 4 Apr 88 15:12:07 EDT
Date: Mon, 4 Apr 88 15:12:07 EDT
From: uunet!rutgers.edu!Postmaster (Mail Delivery Subsystem)
Subject: Returned mail: Host unknown
Message-Id: <8804041912.AD20964@rutgers.edu>
To: <MAILER-DAEMON>
Status: R
 
   ----- Transcript of session follows -----
550 <pfc-vax.mit.edu!mrl@rutgers.edu>... Host unknown
 
   ----- Unsent message follows -----
Received: by rutgers.edu (5.54/1.15) 
	id AA19854; Mon, 4 Apr 88 12:26:33 EDT
Received: from steinmetz.UUCP by uunet.UU.NET (5.54/1.14) with UUCP 
	id AA02086; Mon, 4 Apr 88 12:22:21 EDT
Received:  by kbsvax.steinmetz (1.2/1.1x Steinmetz)
	 id AA15958; Mon, 4 Apr 88 10:21:40 edt
Date:  2 Apr 88 20:16:54 EST
From: steinmetz!MAILER-DAEMON@uunet.uu.net (Mail Delivery Subsystem)
Subject: Returned mail: User unknown
Message-Id: <8804041421.AA15958@kbsvax.steinmetz>
To: MRL@pfc-vax.mit.edu
 
   ----- Transcript of session follows -----
mail11: %MAIL-E-SYNTAX, error parsing 'CRD'
550 crd.ge.com!EVERHART%ARISIA.DECNET... User unknown
 
   ----- Unsent message follows -----
Received:  by kbsvax.steinmetz (1.2/1.1x Steinmetz)
	 id AA15954; Mon, 4 Apr 88 10:21:40 edt
Received: by ge-dab.GE.COM (smail2.5)
	id AA19175; 4 Apr 88 06:30:56 EDT (Mon)
Received: by ge-rtp.GE.COM (smail2.5)
	id AA10834; 3 Apr 88 21:17:06 EST (Sun)
Received: by mcnc.mcnc.org (5.54/MCNC/10-20-87)
	id AA14746; Sat, 2 Apr 88 21:13:41 EST
From: <mcnc!rutgers.edu!pfc-vax.mit.edu!MRL>
Received: by rutgers.edu (5.54/1.15) 
	id AA01339; Sat, 2 Apr 88 20:47:04 EST
Message-Id: <8804030147.AA01339@rutgers.edu>
Received: from PFC-VAX.MIT.EDU by XX.LCS.MIT.EDU via Chaosnet; 2 Apr 88 20:18-EST
Date:  2 Apr 88 20:16:54 EST
To: xx!TENCATI@vlsi.jpl.nasa.gov, xx!MHG@mitre-bedford.arpa,
        crd.ge.com!xx!EVERHART@ARISIA.DECNET, xx!GAYMAN@ari-hq1.arpa,
        radc-softvax!xx!BACH
Subject: BULLETIN3.FOR
 
C
C  BULLETIN3.FOR, Version 4/1/88
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Programmer: Mark R. London
C
	SUBROUTINE UPDATE
C
C  SUBROUTINE UPDATE
C
C  FUNCTION:  Searches for bulletins that have expired and deletes them.
C
C  NOTE:  Assumes directory file is already opened.
C
	IMPLICIT INTEGER (A - Z)
 
	INCLUDE 'BULLDIR.INC'
 
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 
	CHARACTER*107 DIRLINE
 
	CHARACTER*11 TEMP_DATE,TEMP_EXDATE
	CHARACTER*8 TEMP_TIME,TEMP_EXTIME
 
	IF (TEST_BULLCP().OR.REMOTE_SET) RETURN
					! BULLCP cleans up expired bulletins
 
	ENTRY UPDATE_ALWAYS		! Entry to skip BULLCP test
 
	TEMP_EXDATE = '5-NOV-2000'  ! If a bulletin gets deleted, and there are
	TEMP_EXTIME = '00:00:00'    ! are no more bulletins, this is the value
				    ! assigned to the latest expiration date
 
	TEMP_DATE = '5-NOV-1956' 	! Storage for computing newest
	TEMP_TIME = '00:00:00'		! bulletin date if deletion occurs
 
	BULL_ENTRY = 1				! Init bulletin pointer
	UPDATE_DONE = 0			! Flag showing bull has been deleted
 
	DO WHILE (1)
	   CALL READDIR(BULL_ENTRY,IER)		! Get next directory entry
	   IF (IER.EQ.BULL_ENTRY) GO TO 100	! ERROR: Not found
	   IF (SYSTEM.LE.3.OR.(SHUTDOWN.EQ.0	! If not shutdown, or time
     &	     .AND.(SYSTEM.AND.4).EQ.4)) THEN	! to delete shutdowns?
	    IF ((SYSTEM.AND.4).EQ.4) THEN	! Shutdown bulletin?
	       DIFF = 0				! If so, delete it
	    ELSE
	       DIFF = COMPARE_DATE(EXDATE,' ')	! Has expiration date passed?
	       IF (DIFF.EQ.0) DIFF = COMPARE_TIME(EXTIME,' ')
	    END IF
	    IF (DIFF.LE.0) THEN			! If so then delete bulletin
	      CALL DELETE_ENTRY(BULL_ENTRY)	! Delete bulletin entry
	      IF (UPDATE_DONE.EQ.0) THEN	! If this is first deleted file
	         UPDATE_DONE = BULL_ENTRY	! store it to use for reordering
	      END IF				! directory file.
	    ELSE IF (SYSTEM.LE.3) THEN		! Expiration date hasn't passed
		! If a bulletin is deleted, we'll have to update the latest
		! expiration date. The following does that.
	      DIFF = COMPARE_DATE(EXDATE,TEMP_EXDATE)
	      IF (DIFF.LT.0.OR.(DIFF.EQ.0.AND.
     &		COMPARE_TIME(EXTIME,TEMP_EXTIME).LT.0)) THEN
	         TEMP_EXDATE = EXDATE		! If this is the latest exp
	         TEMP_EXTIME = EXTIME		! date seen so far, save it.
	      END IF
	      TEMP_DATE = DATE			! Keep date so when we quit
	      TEMP_TIME = TIME			! search, we'll have the
	    END IF				! latest bulletin date
	   ELSE
	      TEMP_DATE = DATE
	      TEMP_TIME = TIME
	   END IF
	   BULL_ENTRY = BULL_ENTRY + 1
	END DO
 
100	IF (UPDATE_DONE.GT.0) THEN		! Reorder directory file
	   CALL CLEANUP_DIRFILE(UPDATE_DONE)	! due to deleted entries
	END IF
 
	DATE = NEWEST_DATE
	TIME = NEWEST_TIME
	NEW_SHUTDOWN = SHUTDOWN
	CALL READDIR(0,IER)
	SHUTDOWN = NEW_SHUTDOWN
	NEWEST_EXDATE = TEMP_EXDATE
	NEWEST_EXTIME = TEMP_EXTIME
	NEWEST_DATE = TEMP_DATE
	NEWEST_TIME = TEMP_TIME
	CALL WRITEDIR(0,IER)
	CALL UPDATE_FOLDER
C
C  If newest message date has been changed, must change it in BULLUSER.DAT
C  and also see if it affects notification of new messages to users
C
	IF (TEMP_DATE.NE.DATE.OR.TEMP_TIME.NE.TIME) THEN
	   CALL UPDATE_LOGIN(.FALSE.)
	END IF
 
	RETURN
 
	END
 
 
 
	SUBROUTINE UPDATE_READ
C
C  SUBROUTINE UPDATE_READ
C
C  FUNCTION:
C	Store the latest date that user has used the BULLETIN facility.
C	If new bulletins have been added, alert user of the fact.
C
 
	IMPLICIT INTEGER (A - Z)
 
	INCLUDE 'BULLUSER.INC'
 
	INCLUDE 'BULLDIR.INC'
 
	INCLUDE '($PRVDEF)'
 
	CHARACTER TODAY*23
 
	DIMENSION TODAY_BTIM(2),READ_BTIM_SAVE(2)
 
C
C  Update user's latest read time in his entry in BULLUSER.DAT.
C
 
	CALL OPEN_FILE_SHARED(4)		! Get BULLUSER.DAT file
 
	CALL READ_USER_FILE_HEADER(IER)
 
	IF (IER.NE.0) THEN			! If header not present, exit
	   CALL CLOSE_FILE(4)
	   RETURN
	ELSE IF (USERPRIV(1).EQ.-1.AND.USERPRIV(2).EQ.-1) THEN
						! If header present, but no
	   DO I=1,FLONG				! SET_FLAG and NOTIFY_FLAG
	      SET_FLAG_DEF(I) = 0		! information, write default
	      NOTIFY_FLAG_DEF(I) = 0		! flags.
	      BRIEF_FLAG_DEF(I) = 0
	   END DO
	   SET_FLAG_DEF(1) = 1
	   USERPRIV(1) = PRV$M_OPER.OR.PRV$M_CMKRNL.OR.PRV$M_SETPRV
	   USERPRIV(2) = 0
	   REWRITE (4) USER_HEADER
	END IF
 
	CALL SYS$ASCTIM(,TODAY,,)		! Get today's time
	CALL SYS_BINTIM(TODAY,TODAY_BTIM)
 
	UNLOCK 4
 
	CALL READ_USER_FILE_KEYNAME(USERNAME,IER1)
 
	IF (IER1.EQ.0) THEN			! If entry found, update it
	   READ_BTIM_SAVE(1) = READ_BTIM(1)
	   READ_BTIM_SAVE(2) = READ_BTIM(2)
	   READ_BTIM(1) = TODAY_BTIM(1)
	   READ_BTIM(2) = TODAY_BTIM(2)
	   REWRITE (4) USER_ENTRY
	   READ_BTIM(1) = READ_BTIM_SAVE(1)
	   READ_BTIM(2) = READ_BTIM_SAVE(2)
	ELSE					! If no entry create a new entry
	   NEW_FLAG(1) = 143
	   NEW_FLAG(2) = 0
	   LOGIN_BTIM(1) = TODAY_BTIM(1)
	   LOGIN_BTIM(2) = TODAY_BTIM(2)
	   READ_BTIM(1) = TODAY_BTIM(1)
	   READ_BTIM(2) = TODAY_BTIM(2)
	   CALL WRITE_USER_FILE_NEW(IER)
	END IF
 
	CALL CLOSE_FILE(4)			! All finished with BULLUSER
 
	RETURN					! to go home...
 
	END
 
 
 
 
	SUBROUTINE FIND_NEWEST_BULL
C
C  SUBROUTINE FIND_NEWEST_BULL
C
C	If new bulletins have been added, alert user of the fact and
C	set the next bulletin to be read to the first new bulletin.
C
C  OUTPUTS:
C	BULL_POINT  -  If -1, no new bulletins to read, else there are.
C
 
	IMPLICIT INTEGER (A - Z)
 
	COMMON /POINT/ BULL_POINT
 
	INCLUDE 'BULLUSER.INC'
 
	INCLUDE 'BULLDIR.INC'
 
	INCLUDE 'BULLFOLDER.INC'
 
	INTEGER DIR_BTIM(2)
 
C
C  Now see if bulletins have been added since the user's previous8
C  read time.  If they have, then search for the first new bulletin.
C  Ignore new bulletins that are owned by the user or system notices
C  that have not been added since the user has logged in.t
Ce
	BULL_POINT = -1				! Init bulletin pointer4
 1
	CALL OPEN_FILE_SHARED(2)		! Yep, so get directory file 
	CALL READDIR(0,IER)			! Get # bulletins from header
	IF (IER.EQ.1) THENe
	   CALL GET_NEWEST_MSG(LAST_READ_BTIM(1,FOLDER_NUMBER+1),START)
	   IF (START.LE.0) THEN
	      BULL_POINT = STARTt
	      CALL CLOSE_FILE(2)0
	      RETURNu
	   ELSE
	      START = START + 1
	   END IF
	   DO WHILE (START.LE.NBULL.AND.(FROM.EQ.USERNAME.OR.SYSTEM))
	      IF (FROM.NE.USERNAME) THEN	! Ignore bull if owner is user
	         IF (SYSTEM) THEN		! If system bulletin
	            CALL SYS_BINTIM(DATE//' '//TIME,DIR_BTIM)
	            DIFF = COMPARE_BTIM(LOGIN_BTIM,DIR_BTIM)h
		    IF (DIFF.GT.0) THEN 
		       START = START + 1
	               CALL READDIR(START,IER)e
		    ELSE			! SYSTEM bulletin was not seent
		       SYSTEM = 0		! so force exit to read it.
		    END IF
	         END IF
	      ELSEs
		 START = START + 1
		 CALL READDIR(START,IER)
	      END IF2
	   END DO
	   IF (START.LE.NBULL) BULL_POINT = START - 1
	END IFT
 s
	CALL CLOSE_FILE(2)o
 -
	RETURNl
	END
 -
 Y
 X
	SUBROUTINE GET_EXPIRED(INPUT,IER)
 E
	IMPLICIT INTEGER (A-Z)U
  
	INCLUDE 'BULLUSER.INC's
  
	INCLUDE 'BULLFOLDER.INC'e
 d
	CHARACTER*20 INPUTz
	CHARACTER*23 TODAY
  
	DIMENSION EXTIME(2),NOW(2)2
 0
	EXTERNAL CLI$_ABSENTa
 E
	IER = SYS$ASCTIM(,TODAY,,)		! Get today's dateE
 (
	IERC = CLI$GET_VALUE('EXPIRATION',INPUT,ILEN)
 A
	PROMPT = .TRUE.
 :
5	IF (PROMPT) THEN
	   IF (IERC.NE.%LOC(CLI$_ABSENT)) THEN	! Was value specified?
	      PROMPT = .FALSE.m
	   ELSE
	      WRITE(6,1030) TODAY		! Prompt for expiration date
	      CALL GET_LINE(INPUT,ILEN)		! Get input line
	   END IF
	ELSE0
	   RETURN
	END IFr
 d
	IF (ILEN.LE.0) THEN
	   IER = 0b
	   RETURN
	END IFC
 s
	INPUT = INPUT(:ILEN)			! Change trailing zeros 2 spaces
 x
	IF (INDEX(INPUT,'-').EQ.0.AND.INDEX(INPUT,':').GT.0.AND. 
     &		INDEX(INPUT(:ILEN),' ').EQ.0) THEN
	   INPUT = TODAY(:INDEX(TODAY(2:),' ')+1)//INPUTH
	END IF:
 L
	CALL STR$UPCASE(INPUT,INPUT)		! Convert to upper case
	IER = SYS_BINTIM(INPUT,EXTIME)r
	IF (IER.NE.1) THEN			! If not able to do so
    	   WRITE(6,1040)			! tell user is wrong
	   GO TO 5n
	END IF
	IER = SYS$ASCTIM(TIMLEN,INPUT,EXTIME,)A
	IF (TIMLEN.EQ.16) THENr
	   CALL SYS$GETTIM(NOW)
	   CALL LIB$SUBX(NOW,EXTIME,EXTIME)
	   IER = SYS$ASCTIM(TIMLEN,INPUT,EXTIME,)
	END IF
 P
	IF (INPUT(2:2).EQ.'-') INPUT = '0'//INPUT
	IER = COMPARE_DATE(INPUT(:11),TODAY(:11)) ! Compare date with today's
	IF (IER.GT.F_EXPIRE_LIMIT.AND.F_EXPIRE_LIMIT.GT.0.AND.
     &		.NOT.SETPRV_PRIV().AND.USERNAME.NE.FOLDER_OWNER) THENE
	   WRITE(6,1050) F_EXPIRE_LIMIT		! Expiration date > limit
	   GO TO 5D
	END IFS
	IF (IER.EQ.0) IER = COMPARE_TIME(INPUT(13:20),TODAY(13:20))
	IF (IER.LE.0) THEN			! If expiration date not futureX
	   WRITE(6,1045)			! tell usere
	   GO TO 5				! and re-request date
	END IFn
 t
	IER = 1
 e
	RETURN 
 e
1030	FORMAT(' It is ',A23,
     &'. Specify when the message should expire:',/,1x,:
     &'Enter absolute time: [dd-mmm-yyyy] hh:mm:ss ',R
     &'or delta time: dddd hh:mm:ss')T
1040	FORMAT(' ERROR: Invalid date format specified.')
1045	FORMAT(' ERROR: Specified time has already passed.')t
1050	FORMAT(' ERROR: Specified expiration period too large.R
     & Limit is ',I3,' days.')
 R
	END
 N
 .
	SUBROUTINE MAILEDIT(INFILE,OUTFILE)
  
	IMPLICIT INTEGER (A-Z))
 E
	INCLUDE '($SSDEF)'n
 
	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD
 
	EXTERNAL BULLETIN_SUBCOMMANDS
 t
	CHARACTER*(*) INFILE,OUTFILE=
 M
	CHARACTER*80 MAIL_EDIT,OUTx
 a
	IER = SYS_TRNLNM('MAIL$EDIT',MAIL_EDIT)
	IF (IER.NE.SS$_NORMAL) MAIL_EDIT = 'SYS$SYSTEM:MAILEDIT'F
 E
	OUT = OUTFILE
	IF (TRIM(OUT).EQ.0) THEN 
	   OUT = INFILE
	END IF_
 R
	IF (INDEX(MAIL_EDIT,'CALLABLE_').EQ.0) THEN
	   CALL DISABLE_PRIVS
	   CALL LIB$SPAWN('$@'//MAIL_EDIT(:TRIM(MAIL_EDIT))
     &		//' '//INFILE//' '//OUT(:TRIM(OUT)))
	   CALL ENABLE_PRIVSy
	ELSE IF (INDEX(MAIL_EDIT,'EDT').GT.0) THENp
	   CALL EDT$EDIT(INFILE,OUT) 
	ELSE IF (INDEX(MAIL_EDIT,'TPU').GT.0) THENe
	   CALL TPU$EDIT(INFILE,OUT)t
	   IER = CLI$DCL_PARSE(INCMD,BULLETIN_SUBCOMMANDS)A
		! TPU does CLI$ stuff which wipes our parsed command lineD
	END IF&
 O
	RETURNE
	END
 E
 E
 M
 L
 )
	SUBROUTINE CREATE_BULLCPT
  
	IMPLICIT INTEGER (A-Z)l
 s
	INCLUDE '($PRCDEF)'
 T
	INCLUDE '($JPIDEF)'
  
	INCLUDE '($SSDEF)' 
 E
	INCLUDE 'BULLFILES.INC'
 T
	COMMON /PRIVILEGES/ PROCPRIV(2),NEEDPRIV(2)
 E
	CHARACTER IMAGENAME*132,ANSWER*1,PRCNAM*15I
 	
	DIMENSION SAVEPRIV(2)
  
	CALL DISABLE_PRIVS	! Just let real privileged people do a /STARTUPF
  
	CALL SYS$SETPRV(%VAL(1),PROCPRIV,,SAVEPRIV)	! Enable original priv0
 H
	IF (TEST_BULLCP()) THEN
	   WRITE (6,'('' BULLCP process running. 
     & Do you wish to kill it and restart a new one? '',$)')
	   READ (5,'(A)') ANSWERU
	   IF (ANSWER.NE.'Y'.AND.ANSWER.NE.'y') CALL EXIT
 _
	   WILDCARD = -1X
 E
	   CALL INIT_ITMLST	! Initialize item list
				! Now add items to listW
	   CALL ADD_2_ITMLST(LEN(PRCNAM),JPI$_PRCNAM,%LOC(PRCNAM))L
	   CALL ADD_2_ITMLST(4,JPI$_PID,%LOC(PID))c
	   CALL END_ITMLST(GETJPI_ITMLST)	! Get address of itemlist
	   IER = 1i
	   DO WHILE (IER.AND.PRCNAM(:6).NE.'BULLCP')_
						! Get next interactive process
	      IER = SYS$GETJPIW(,WILDCARD,,%VAL(GETJPI_ITMLST),,,,)
						! Get next process.T
	   END DO
	   IF (IER.AND.PID.NE.0) IER = SYS$DELPRC(PID,)
	   IF (.NOT.IER) THEN
	      CALL SYS_GETMSG(IER)B
	      CALL EXIT
	   END IF
	END IFh
  
	CALL GETIMAGE(IMAGENAME,ILEN)
 
	LEN_B = TRIM(FOLDER_DIRECTORY)Z
 
	CALL SYS$SETDFPROT('AA00'X,CUR_DEF_PROT)'
		! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD:RW,GROUP:RW)
	OPEN(UNIT=11,FILE=FOLDER_DIRECTORY(:LEN_B)//'BULLCP.COM',
     &		STATUS='NEW',IOSTAT=IER,CARRIAGECONTROL='LIST')T
	IF (IER.NE.0) RETURNF
	WRITE(11,'(A)') '$SET NOON'
	WRITE(11,'(A)') '$B:=$'//IMAGENAME(:ILEN)
	WRITE(11,'(A)') '$LOOP:'	
	WRITE(11,'(A)') '$B/BULLCP'
	WRITE(11,'(A)') '$GOTO LOOP'		! File open timed out
	CLOSE(UNIT=11).
	CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection 
 O
	IER = 0
	DO WHILE (IER.EQ.0.OR.(IER.EQ.SS$_DUPLNAM.AND.PID.GT.0))	
	   IER = SYS$CREPRC(,'SYS$SYSTEM:LOGINOUT',
     &	      FOLDER_DIRECTORY(:LEN_B)//'BULLCP.COM','NL:' 
     &	      ,,,,'BULLCP',%VAL(4),,,%VAL(PRC$M_NOUAF+PRC$M_DETACH))$
	END DOO
 R
	IF (IER) THEN
	   OPEN(UNIT=11,FILE=FOLDER_DIRECTORY(:LEN_B)//'BULLCP.COM;-1',
     &		STATUS='OLD',IOSTAT=IER1)'
	   IF (IER1.EQ.0) CLOSE(UNIT=11,STATUS='DELETE',IOSTAT=IER1)
	END IFD
 E
	CALL SYS$SETPRV(%VAL(0),SAVEPRIV,,)	! Reset privs
 N
	CALL ENABLE_PRIVS
 d
	IF (.NOT.IER) THEN_
	   CALL SYS_GETMSG(IER)
	ELSEB
	   WRITE (6,'('' Successfully created BULLCP detached process.'')')
	END IF=
	CALL EXIT
 
	END
 T
 4
 S
	SUBROUTINE FIND_BULLCP)
 R
	IMPLICIT INTEGER (A-Z)D
 I
	COMMON /BCP/ BULLCP
	LOGICAL BULLCP /.FALSE./ 
 a
	CHARACTER*1 DUMMY
 _
	IER = SYS_TRNLNM('BULL_BULLCP',DUMMY)
	IF (IER) BULLCP = .TRUE.M
 
	RETURNI
	END
  
 O
 _
 M
	LOGICAL FUNCTION TEST_BULLCPB
 (
	IMPLICIT INTEGER (A-Z)D
 B
	COMMON /BCP/ BULLCP
	LOGICAL BULLCPR
 
	TEST_BULLCP = BULLCPS
 I
	RETURN 
	END
 h
 w
  
 L
	SUBROUTINE RUN_BULLCP
 g
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLFOLDER.INC'W
 _
	INCLUDE 'BULLDIR.INC'
 D
	INCLUDE 'BULLUSER.INC'w
 l
	COMMON /BCP/ BULLCP
	LOGICAL BULLCPh
 a
	COMMON /KNOWN_FOLDERS/ FOLDER_Q1,NUM_FOLDERSh
 i
	IF (TEST_BULLCP()) CALL EXIT	! BULLCP already running, so exit.
 b
	BULLCP = .FALSE.		! Enable process to do BULLCP functions
 A
	IER = SYS$CREMBX(%VAL(1),CHAN,,,,,'BULL_BULLCP')E
	IF (.NOT.IER) THEN		! Can't create mailbox, so exit.U
	   CALL SYS_GETMSG(IER)
	   CALL EXITI
	END IF
 
	IER = SYS$DELMBX(%VAL(CHAN))	! If process dies, mailbox is deleted.
 a
	CALL REGISTER_BULLCPt
  
	CALL START_DECNET
 w
	DO WHILE (1)			! Loop once every 15 minutes
	   CALL GET_PROXY_ACCOUNTS	! Proxy info for incoming connectiosnc
	   CALL BBOARD			! Look for BBOARD messages.1
	   FOLDER_Q = FOLDER_Q1		! Init queue pointer to header
	   POINT_FOLDER = 0
	   DO WHILE (POINT_FOLDER.LT.NUM_FOLDERS)
	      POINT_FOLDER = POINT_FOLDER + 1
	      CALL SYS$SETAST(%VAL(0))R
	      CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)
	      IF (FOLDER_BBOARD(:2).NE.'::') THEN
	         CALL SELECT_FOLDER(.FALSE.,IER)	! Select folderT
	         IF (IER) THENW
	            IF (NEMPTY.GT.200) THEN
	               CALL CLEANUP_BULLFILE	! Cleanup empty blocks
	            END IF
	            CALL DELETE_EXPIRED		! Delete expired messages 
	         END IF
	      END IFD
	      CALL SYS$SETAST(%VAL(1))P
	   END DO
	   CALL WAIT('15')		! Wait for 15 minutes
C
C  Look at remote folders and update local info to reflect new messages.
C  Do here after waiting in case problem with connecting to remote folderx
C  which requires killing process.
C 
	   FOLDER_Q = FOLDER_Q1
	   POINT_FOLDER = 0
	   DO WHILE (POINT_FOLDER.LT.NUM_FOLDERS)
	      POINT_FOLDER = POINT_FOLDER + 1
	      CALL SYS$SETAST(%VAL(0))L
	      CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)
	      IF (FOLDER_BBOARD(:2).EQ.'::') THEN
	         CALL SELECT_FOLDER(.FALSE.,IER) 
	      END IFL
	      CALL SYS$SETAST(%VAL(1))N
	   END DO
	   CALL SYS$SETAST(%VAL(0))
	   FOLDER_NUMBER = 0			! Reset to GENERAL folder
	   CALL SELECT_FOLDER(.FALSE.,IER)d
	   CALL SYS$SETAST(%VAL(1))
	END DOE
 R
	RETURNP
	END
 
 
 O
	SUBROUTINE REGISTER_BULLCPT
 H
	IMPLICIT INTEGER (A-Z)C
 _
	INCLUDE 'BULLUSER.INC' 
 c
	COMMON /SYSTEM_FOLDERS/ SYSTEM_FLAG(FLONG),NODENAME,DUMMY(2))
	CHARACTER NODENAME*8p
 t
	CALL OPEN_FILE(4)
 E
	DO WHILE (REC_LOCK(IER))n
	   READ (4,KEY='*SYSTEM',IOSTAT=IER) 
     &		TEMP_USER,NODENAME,DUMMY,NEW_FLAG,SYSTEM_FLAG 
	END DOE
 I
	IF (IER.NE.0) THENT
	   DO I=1,FLONG
	      SYSTEM_FLAG(I) = 0x
	   END DO
	   CALL SET2(SYSTEM_FLAG,0)
	END IFT
 A
	CALL LIB$SYS_TRNLOG('SYS$NODE',,NODENAME)
	NODENAME = NODENAME(2:INDEX(NODENAME,':')-1))
 N
	IF (IER.NE.0) THENC
	   WRITE (4,IOSTAT=IER)
     &		'*SYSTEM',NODENAME,DUMMY,NEW_FLAG,SYSTEM_FLAGI
	ELSEI
	   REWRITE (4,IOSTAT=IER)
     &		TEMP_USER,NODENAME,DUMMY,NEW_FLAG,SYSTEM_FLAGw
	END IF 
 T
	CALL CLOSE_FILE(4)=
 S
	RETURNI
	END
 T
 T
 ,
 
  
	SUBROUTINE WAIT(PARAM) 
CL
C SUBROUTINE WAIT 
CA
C FUNCTION: Waits for specified time period in minutes.E
CN
	IMPLICIT INTEGER (A-Z)P
	INTEGER TIMADR(2)			! Buffer containing timeI
						! in desired system format.:
	CHARACTER TIMBUF*13,PARAM*2
	DATA TIMBUF/'0 00:00:00.00'/A
 F
	DATA WAIT_EF /0/A
 
	IF (WAIT_EF.EQ.0) CALL LIB$GET_EF(WAIT_EF)F
 E
	TIMBUF(6:7) = PARAM
 (
	IER=SYS$BINTIM(TIMBUF,TIMADR)
	IER=SYS$SETIMR(%VAL(WAIT_EF),TIMADR,,%VAL(2))	! Set timer.R
	IER=SYS$WAITFR(%VAL(WAIT_EF))		! Wait for EFN to be set.T
 	
	RETURNp
	END
 a
 n
 f
	SUBROUTINE WAIT_SEC(PARAM) 
Cl
C SUBROUTINE WAIT_SEC!
Cd
C FUNCTION: Waits for specified time period in seconds.e
C0
	IMPLICIT INTEGER (A-Z)
	INTEGER TIMADR(2)			! Buffer containing time:
						! in desired system format.e
	CHARACTER TIMBUF*13,PARAM*2
	DATA TIMBUF/'0 00:00:00.00'/m
	DATA WAIT_EF /0/(
 R
	IF (WAIT_EF.EQ.0) CALL LIB$GET_EF(WAIT_EF)F
 A
	TIMBUF(9:10) = PARAMm
 a
	IER=SYS$BINTIM(TIMBUF,TIMADR)
	IER=SYS$SETIMR(%VAL(WAIT_EF),TIMADR,,%VAL(3))	! Set timer.m
	IER=SYS$WAITFR(%VAL(WAIT_EF))		! Wait for EFN to be set.T
 F
	RETURNL
	END
 I
 I
  
 E
	SUBROUTINE DELETE_EXPIRED
 E
Cn
C  SUBROUTINE DELETE_EXPIRED
CD
C  FUNCTION:
CI
C  Delete any expired bulletins (normal or shutdown ones).
C  (NOTE: If bulletin files don't exist, they get created now by
C  OPEN_FILE_SHARED.  Also, if new format has been defined for files,S
C  they get converted now.  The directory file has had it's record sizeU
C  lengthened in the past to include more info, and the bulletin file 
C  was lengthened from 80 to 81 characters to include byte which indicated
C  start of bulletin message.  However, that scheme was removed andy
C  was replaced with a 128 byte record compressed format).
CI
 N
	IMPLICIT INTEGER (A-Z)E
 A
	INCLUDE 'BULLDIR.INC'
 
	INCLUDE 'BULLFOLDER.INC'T
 
	CHARACTER UPTIME_DATE*11,UPTIME_TIME*8B
 M
	CALL OPEN_FILE_SHARED(2)	! Open directory filea
	CALL OPEN_FILE_SHARED(1)	! Open bulletin file
	CALL CLOSE_FILE(1)
	CALL READDIR(0,IER)		! Get directory header
	IF (IER.EQ.1) THEN		! Is header present?I
	   IER = COMPARE_DATE(NEWEST_EXDATE,' ') ! Yes. Any expired bulls?.
	   IF (IER.EQ.0) IER = COMPARE_TIME(NEWEST_EXTIME,' ')E
	   IF (SHUTDOWN.GT.0.AND.
     &		(FOLDER_NUMBER.EQ.0.OR.BTEST(FOLDER_FLAG,2))) THEN
						! Do shutdown bulletins exist?
	      CALL GET_UPTIME(UPTIME_DATE,UPTIME_TIME)(
	      IER1 = COMPARE_DATE(SHUTDOWN_DATE,UPTIME_DATE) 
	      IF (IER1.EQ.0) IER1 = COMPARE_TIME(SHUTDOWN_TIME,UPTIME_TIME)
	      IF (IER1.LE.0) SHUTDOWN = 0
	   ELSE
	      IER1 = 1E
	   END IF
	   IF (IER.LE.0.OR.IER1.LE.0) THENN
	      CALL CLOSE_FILE(2)
	      CALL OPEN_FILE(2)		! Reopen without sharing
	      CALL UPDATE 		! Need to updateo
	   END IF
	ELSE		! If header not there, then first time running BULLETIN
	   CALL OPEN_FILE(4)		! Create user file to be able to setE
	   CALL CLOSE_FILE(4)		! defaults, privileges, etc.
	END IFI
	CALL CLOSE_FILE(2))
 .
	RETURN_
	END
  
  
 t
 t
	SUBROUTINE BBOARD
C 
C  SUBROUTINE BBOARD
CA
C  FUNCTION: Converts mail to BBOARD into non-system bulletins. 
C
  
	IMPLICIT INTEGER (A-Z) 
 Y
	INCLUDE 'BULLDIR.INC'
 T
	INCLUDE 'BULLFILES.INC'
 E
	INCLUDE 'BULLUSER.INC'X
 
	INCLUDE 'BULLFOLDER.INC'C
  
	INCLUDE '($RMSDEF)'
 
	COMMON /KNOWN_FOLDERS/ FOLDER_Q1,NUM_FOLDERS 
	DATA FOLDER_Q1/0/
 U
	CHARACTER*11 INEXDATE
	CHARACTER INDESCRIP*74,INFROM*74,INTO*76,INPUT*132)
	CHARACTER ACCOUNT_SAVE*8,USERNAME_SAVE*12
 '
	DIMENSION NEW_MAIL(FOLDER_MAX)'
 S
	DATA SPAWN_EF/0/T
 =
	IF (SPAWN_EF.EQ.0) CALL LIB$GET_EF(SPAWN_EF))
 '
	CALL DISABLE_CTRL
 (
	CALL INIT_QUEUE(FOLDER_Q1,FOLDER_COM)
 (
	FOLDER_Q = FOLDER_Q11
 (
	CALL SYS$SETAST(%VAL(0)),
	CALL OPEN_FILE_SHARED(7)		! Get folder file
 E
	NUM_FOLDERS = 0
	IER = 0
	DO WHILE (IER.EQ.0)			! Copy all bulletins from fileR
	   CALL READ_FOLDER_FILE(IER)
	   IF (IER.EQ.0) THEN
	      NUM_FOLDERS = NUM_FOLDERS + 1
	      CALL WRITE_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)L
	   END IF
	END DO&
  
	CALL CLOSE_FILE(7)			! We don't need file anymore
	CALL SYS$SETAST(%VAL(1))I
  
	CALL SYS$SETAST(%VAL(0))=
	CALL CHECK_MAIL(NEW_MAIL)
	CALL SYS$SETAST(%VAL(1))T
 '
	FOLDER_Q = FOLDER_Q1			! Init queue pointer to header
 =
	NBBOARD_FOLDERS = 0
 E
	POINT_FOLDER = 0Y
 E
1	POINT_FOLDER = POINT_FOLDER + 1i
	IF (POINT_FOLDER.GT.NUM_FOLDERS) GO TO 900)
 E
	CALL SYS$SETAST(%VAL(0)))
 E
	FOLDER_Q_SAVE = FOLDER_Qc
 f
	CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)
 L
	IF (FOLDER_BBOARD.EQ.'NONE'.OR.
     &		FOLDER_BBOARD(:2).EQ.'::') GO TO 1
 Z
	NBBOARD_FOLDERS = NBBOARD_FOLDERS + 1
 C
	IF (.NOT.NEW_MAIL(POINT_FOLDER)) GO TO 1I
C=
C  The process is set to the BBOARD uic and username in order to createI
C  a spawned process that is able to read the BBOARD mail (a real kludge).
CA
 D
	CALL GETUSER(USERNAME_SAVE)		! Get present username
	CALL GETACC(ACCOUNT_SAVE)		! Get present account
	CALL GETUIC(GROUP_SAVE,USER_SAVE)	! Get present uic
 
	IF (TRIM(FOLDER_BBOARD).GT.0) THEN	! BBOARD name present?
	   IER = SETUSER(FOLDER_BBOARD,USERNAME_SAVE)! Set to BBOARD username
	   IF (IER.EQ.2) GO TO 910	! Can't set username. New VMS version?
	   CALL SETACC(ACCOUNTB)	! Set to BBOARD accounti
	   CALL SETUIC(IBCLR(GROUPB,31),IBCLR(USERB,31)) ! Set to BBOARD uic
	END IFS
 E
	LEN_B = TRIM(BBOARD_DIRECTORY)'
	IER = LIB$DELETE_FILE(BBOARD_DIRECTORY(:LEN_B)// 
     &		FOLDER_BBOARD(:TRIM(FOLDER_BBOARD))//'.TXT;*')
				! Delete old TXT files left due to errorse
 d
	IF (.NOT.BTEST(USERB,31).AND.(USERB.NE.0.OR.GROUPB.NE.0)) THENT
							! If normal BBOARD user
	 IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(:LEN_B)
     &		//'BOARD.COM','NL:','NL:',1,,,STATUS,SPAWN_EF)
	 CALL SYS$SETAST(%VAL(1))
	 CALL SYS$WAITFR(%VAL(SPAWN_EF))1
	 CALL SYS$SETAST(%VAL(0))
	 IF (((STATUS.AND.'1FFFF'X).EQ.RMS$_FNF) .OR.
     &	 ((STATUS .AND. '1FFF0'X).EQ. (RMS$_SPL .AND. '1FFF0'X))) THENC
	   CALL SYS$SETDFPROT('AA00'X,CUR_DEF_PROT)
		! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD:RW,GROUP:RW)
	   OPEN(UNIT=11,FILE=BBOARD_DIRECTORY(:LEN_B)//'BOARD.COM',
     &		STATUS='NEW',ERR=910,CARRIAGECONTROL='LIST')
	   WRITE(11,'(A)') '$ SET PROTECT=(W:RWED)/DEFAULT'
	   WRITE(11,'(A)') '$ SET PROC/PRIV=SYSPRV'
	   WRITE(11,'(A)') 
     & '$ DEFINE/USER EXTRACT_FILE '//BBOARD_DIRECTORY(:LEN_B)//
     & '''F$GETJPI("","USERNAME")'''
	   WRITE(11,'(A)') '$ MAIL'
	   WRITE(11,'(A)') 'READ'
	   WRITE(11,'(A)') 'EXTRACT/ALL/APPEND EXTRACT_FILE'c
	   WRITE(11,'(A)') 'DELETE/ALL'
	   CLOSE(UNIT=11)
	   CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection
	   IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(:LEN_B)
     &			//'BOARD.COM','NL:','NL:',1,,,STATUS,SPAWN_EF)U
	   CALL SYS$SETAST(%VAL(1))
	   CALL SYS$WAITFR(%VAL(SPAWN_EF))$
	   CALL SYS$SETAST(%VAL(0))
	 END IF
	ELSEO
	 IER = LIB$FIND_FILE(BBOARD_DIRECTORY(:LEN_B)//FOLDER_BBOARD:
     &	    (:TRIM(FOLDER_BBOARD))//'.COM',INPUT,CONTEXT)
	 IF (IER) THEN 
	    IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(:LEN_B)//T
     &		FOLDER_BBOARD(:TRIM(FOLDER_BBOARD))//'.COM','NL:',
     &		'NL:',1,,,STATUS,SPAWN_EF)
	    CALL SYS$SETAST(%VAL(1))L
	    CALL SYS$WAITFR(%VAL(SPAWN_EF))
	    CALL SYS$SETAST(%VAL(0))B
	 END IF
	 IF (.NOT.IER.OR.((STATUS.AND.'1FFFF'X).EQ.RMS$_FNF) .OR.
     &	 ((STATUS .AND. '1FFF0'X).EQ. (RMS$_SPL .AND. '1FFF0'X))) THENA
	    IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(:LEN_B)// 
     &	      'BOARD_SPECIAL.COM','NL:','NL:',1,,,STATUS,SPAWN_EF)E
	    CALL SYS$SETAST(%VAL(1)),
	    CALL SYS$WAITFR(%VAL(SPAWN_EF))
	    CALL SYS$SETAST(%VAL(0)) 
	 END IF
	END IF 
 x
	CALL READ_QUEUE(%VAL(FOLDER_Q_SAVE),FOLDER_Q,FOLDER_COM)C
  
	NBULL = F_NBULL
 O
	CALL SETACC(ACCOUNT_SAVE)		! Reset to original account)
	CALL SETUSER(USERNAME_SAVE)		! Reset to original username
	CALL SETUIC(GROUP_SAVE,USER_SAVE)	! Reset to original uic
  
	OPEN (UNIT=3,FILE=BBOARD_DIRECTORY(:LEN_B)//FOLDER_BBOARD
     &	   (:TRIM(FOLDER_BBOARD))//'.TXT',STATUS='OLD',ERR=110)
	CALL SYS$SETAST(%VAL(1))
  
5	CALL SYS$SETAST(%VAL(0))
 
	CALL READ_QUEUE(%VAL(FOLDER_Q_SAVE),IDUMMY,FOLDER_COM)i
 p
	LEN_INPUT = 1
	DO WHILE (LEN_INPUT.GT.0)
	   READ (3,'(Q,A)',END=100) LEN_INPUT,INPUT ! Read next line from mails
	   IF (INPUT(:5).EQ.'From:') THEN
	      INFROM = INPUT(7:)		! Store username
	   ELSE IF (INPUT(:5).EQ.'Subj:') THEN)
	      INDESCRIP = INPUT(7:)		! Store subjectP
	   ELSE IF (INPUT(:3).EQ.'To:') THEN)
	      INTO = INPUT(5:)			! Store addressL
	   END IF
	END DOI
 S
	INTO = INTO(:TRIM(INTO)) 
	CALL STR$TRIM(INTO,INTO)	
	FLEN = TRIM(FOLDER_BBOARD)
	IF (INDEX(INTO,FOLDER_BBOARD(:FLEN)).EQ.0.AND.I
     &	 INTO.NE.FOLDER_BBOARD.AND.INDEX(INTO,'@').EQ.0) THEN
	   POINT_FOLDER1 = 0N
 	   FOLDER_Q2 = FOLDER_Q1
	   FOLDER1_BBOARD = FOLDER_BBOARD
	   FOUND = .FALSE.e
	   DO WHILE (.NOT.FOUND.AND.POINT_FOLDER1.LT.NUM_FOLDERS)
	      FOLDER_Q2_SAVE = FOLDER_Q2R
	      CALL READ_QUEUE(%VAL(FOLDER_Q2),FOLDER_Q2,FOLDER1_COM)0
	      FLEN = TRIM(FOLDER1_BBOARD)
	      POINT_FOLDER1 = POINT_FOLDER1 + 1
	      IF (POINT_FOLDER1.LE.NUM_FOLDERS.AND.
     &		  FOLDER1_BBOARD(:2).NE.'::'.AND.T
     &		  FOLDER1_BBOARD.NE.'NONE') THEN
	 	 IF (INTO.EQ.FOLDER1_BBOARD) THEN
		    FOUND = .TRUE.
		 ELSEF
		    FIND_TO = INDEX(INTO,FOLDER1_BBOARD(:FLEN))a
		    IF (FIND_TO.GT.0) THEN
		       END_TO = FLEN+FIND_TO
		       IF (TRIM(INTO).LT.END_TO.OR.S
     &			    INTO(END_TO:END_TO).LT.'A'.OR.f
     &			    INTO(END_TO:END_TO).GT.'Z') THENe
			  IF (FIND_TO.EQ.1) THENd
			     FOUND = .TRUE.
			  ELSE IF (INTO(FIND_TO-1:FIND_TO-1).LT.'A'.OR.
     &			           INTO(FIND_TO-1:FIND_TO-1).GT.'Z') THEN
			     FOUND = .TRUE.
			  END IFt
		       END IFv
		    END IF
	 	 END IF
	      END IFr
	   END DO
	   IF (FOUND) THENm
	      IF (F_NBULL.NE.NBULL) CALL UPDATE_FOLDERE
	      FOLDER_COM = FOLDER1_COM
	      FOLDER_Q_SAVE = FOLDER_Q2_SAVET
	   END IF
	END IFI
 T
	IF (FOLDER_NUMBER.EQ.0) THENR
	   FOLDER_SET = .FALSE.
	ELSEL
	   FOLDER_SET = .TRUE.p
	   FOLDER_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))// 
     &		FOLDER
	END IF 
 R
C.
C  Add bulletin to bulletin file and directory entry to directory file. 
Ce
 A
	CALL OPEN_FILE(2)			! Prepare to add dir entryR
 I
	READ (3,'(Q,A)',IOSTAT=IER) LEN_INPUT,INPUT	! Read first line
	IF (IER.NE.0) GO TO 100			! If end of file, exit	
	IF (LEN_INPUT.EQ.1.AND.INPUT(:1).EQ.CHAR(12)) GO TO 5
			! If line is just form feed, the message is empty
 U
	CALL OPEN_FILE(1)			! Prepare to add bulletin
 1
	CALL READDIR(0,IER)			! Get NBLOCKT
	IF (IER.EQ.0) NBLOCK = 0		! If new file, NBLOCK is 0 
 I
	OCOUNT = NBLOCK + 1			! Initialize line count
 )
	SPACE = INDEX(INFROM,' ') - 1		! Strip off the date
	IF (SPACE.GT.0) INFROM = INFROM(:SPACE)! From the "From:" lineu
 t
	IF (TRIM(INFROM).GT.12) THEN		! Is length > allowable?t
	   LEN_INFROM = TRIM(INFROM)O
	   CALL STORE_BULL(6+LEN_INFROM,'From: '//INFROM(:LEN_INFROM),E
     &		OCOUNT)l
	   IF (INDEX(INFROM,'::').GT.0)		! Strip off node name
     &		INFROM = INFROM(INDEX(INFROM,'::')+2:)
	   I = 12		! Trim username to first non-alpha character
	   DO WHILE (I.GT.1.AND.m
     &		     ((INFROM(I:I).GE.'A'.AND.INFROM(I:I).LE.'Z').OR.U
     &		     (INFROM(I:I).GE.'a'.AND.INFROM(I:I).LE.'z')) )S
	      I = I - 1
	   END DO
	   IF (I.GT.1) INFROM = INFROM(:I-1)
	END IF 
 O
	LEN_DESCRP = TRIM(INDESCRIP)R
	IF (LEN_DESCRP.GT.53) THEN	! Is length > allowable subject length?R
	   CALL STORE_BULL(6+LEN_DESCRP,'Subj: '//INDESCRIP(:LEN_DESCRP),
     &		OCOUNT)D
	   INDESCRIP = INDESCRIP(:LEN_DESCRP)
	   DO I=1,LEN_DESCRP 
	      IF (INDESCRIP(I:I).LT.' ') INDESCRIP(I:I) = ' '
	   END DO
	ELSEL
	   DO I=1,LEN_DESCRP			! Remove control charactersO
	      IF (INDESCRIP(I:I).LT.' ') INDESCRIP(I:I) = ' '
	   END DO
	END IFl
  
	ISTART = 0M
	NBLANK = 0
	DO WHILE (INPUT(:1).NE.CHAR(12))	! Move text to bulletin file
	   IF (LEN_INPUT.EQ.0) THEN
	      IF (ISTART.EQ.1) THEN
		 NBLANK = NBLANK + 1
	      END IF 
	   ELSE
	      ISTART = 1E
	      DO I=1,NBLANK
		 CALL STORE_BULL(1,' ',OCOUNT)
	      END DO7
	      NBLANK = 0 
	      CALL STORE_BULL(MIN(LEN_INPUT,80),INPUT,OCOUNT)
	      IF (LEN_INPUT.GT.80) THEN		! Breakup line if > 80 chars
		 CALL STORE_BULL(MIN(LEN_INPUT,132)-80,INPUT(81:),OCOUNT) 
	      END IFB
	   END IF
	   READ (3,'(Q,A)',END=25) LEN_INPUT,INPUTD
	END DOT
 L
25	CALL FLUSH_BULL(OCOUNT)
 N
	CALL CLOSE_FILE(1)			! Finished adding bulletin
 )
	DESCRIP = INDESCRIP(:53)		! Description headerQ
	FROM = INFROM(:12)			! Username
	IF (FOLDER_BBEXPIRE.EQ.-1) THEN		! Folder has expiration time?R
	   EXDATE = '5-NOV-2000'		! no, so set date far in future
	   SYSTEM = 2				! indicate permanent message
	ELSE					! Else set expiration date
	   CALL GET_EXDATE(EXDATE,FOLDER_BBEXPIRE)I
	   SYSTEM = 0
	END IFa
	EXTIME = '00:00:00'
	LENGTH = OCOUNT - NBLOCK		! Number of records
 (
	CALL ADD_ENTRY				! Add the new directory entry
 A
30	CALL CLOSE_FILE(2)			! Totally finished with addO
 S
	CALL SYS$SETAST(%VAL(1))t
 c
	GO TO 5					! See if there is more mail
 B
100	CALL UPDATE_FOLDER
  
110	CLOSE (UNIT=3,STATUS='DELETE')		! Close the input file
	CALL SYS$SETAST(%VAL(1)) 
	GOTO 1'
 e
900	FOLDER_NUMBER = 0s
 ?
	CALL OPEN_FILE_SHARED(7)	
	CALL READ_FOLDER_FILE_KEYNUM(0,IER)
	CALL CLOSE_FILE(7)I
	CALL ENABLE_CTRLe
	FOLDER_SET = .FALSE.I
 
	IF (NBBOARD_FOLDERS.EQ.0) THENY
	   CALL OPEN_FILE(4)I
	   CALL READ_USER_FILE_HEADER(IER)&
	   CALL SYS_BINTIM('5-NOV-2956 00:00:00.00',BBOARD_BTIM)e
	   REWRITE (4) USER_HEADER		! Rewrite header.
	   CALL CLOSE_FILE(4)
	END IF.
 R
	RETURNE
 )
910	WRITE (6,1010)
	GO TO 100
 s
930	CLOSE (UNIT=3)
	CALL CLOSE_FILE(1)O
	CALL CLOSE_FILE(2)/
	WRITE (6,1030),
	GO TO 100
 S
1010	FORMAT(' ERROR:Install program with CMKRNL privileges or relink.')1
1030	FORMAT(' ERROR:Alert system programmer. Data file problems.')
  
	END
  
  
 T
 S
	SUBROUTINE CREATE_BBOARD_PROCESS.
 F
	IMPLICIT INTEGER (A-Z)S
 S
	INCLUDE '($PRCDEF)'
 P
	INCLUDE 'BULLFILES.INC'
 S
	CHARACTER*132 IMAGENAME
 W
	CALL GETIMAGE(IMAGENAME,ILEN)
 B
	LEN_B = TRIM(BBOARD_DIRECTORY)M
 
	OPEN(UNIT=11,FILE=BBOARD_DIRECTORY(:LEN_B)//'BULL_COMMAND.COM',
     &		STATUS='OLD',IOSTAT=IER)
	IF (IER.EQ.0) CLOSE(UNIT=11,STATUS='DELETE')S
 '
	CALL SYS$SETDFPROT('AA00'X,CUR_DEF_PROT)E
		! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD:RW,GROUP:RW)
	OPEN(UNIT=11,FILE=BBOARD_DIRECTORY(:LEN_B)//'BULL_COMMAND.COM',
     &		STATUS='NEW',IOSTAT=IER,CARRIAGECONTROL='LIST')_
	IF (IER.NE.0) RETURNA
	WRITE(11,'(A)') '$B:=$'//IMAGENAME(:ILEN)
	WRITE(11,'(A)') '$ON ERROR THEN GOTO EXIT'l
	WRITE(11,'(A)') '$ON SEVERE THEN GOTO EXIT'
	WRITE(11,'(A)') '$ON WARNING THEN GOTO EXIT''
	WRITE(11,'(A)') '$B/'//'''F$PROCESS()'''S
	WRITE(11,'(A)') '$EXIT:'S
	WRITE(11,'(A)') '$LOGOUT'
	CLOSE(UNIT=11)(
	CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protectionO
 :
	IER = SYS$CREPRC(,'SYS$SYSTEM:LOGINOUT',O
     &	   BBOARD_DIRECTORY(:LEN_B)//'BULL_COMMAND.COM','NL:'
     &	   ,,,,'BBOARD',%VAL(4),,,%VAL(PRC$M_NOUAF+PRC$M_DETACH))
 R
	RETURNO
	END
 R
 /
 C
	SUBROUTINE GETUIC(GRP,MEM),
CT
C  SUBROUTINE GETUIC(UIC)$
CA
C  FUNCTION:
C	To get UIC of process submitting the job.L
C  OUTPUT:
C	GRP   -    Group number of UIC
C	MEM   -	   Member number of UIC$
CF
 O
	IMPLICIT INTEGER (A-Z).
 F
	INCLUDE '($JPIDEF)'
 .
	CALL INIT_ITMLST	! Initialize item list
				! Now add items to list/
	CALL ADD_2_ITMLST(4,JPI$_GRP,%LOC(GRP))
	CALL ADD_2_ITMLST(4,JPI$_MEM,%LOC(MEM))
	CALL END_ITMLST(GETJPI_ITMLST)	! Get address of itemlist 
 C
	IER = SYS$GETJPIW(,,,%VAL(GETJPI_ITMLST),,,,)	! Get Info command.
 (
	RETURNS
	END
 E
 ,
 D
 C
	SUBROUTINE GET_UPTIME(UPTIME_DATE,UPTIME_TIME)U
CS
C  SUBROUTINE GET_UPTIME
Co
C  FUNCTION: Gets time of last reboot.
C 
 o
	IMPLICIT INTEGER (A-Z)E
 C
	EXTERNAL	EXE$GL_ABSTIMR
	INTEGER 	UPTIME(2),SYSTIME(2),UPSINCE(2)E
	CHARACTER*(*)	UPTIME_TIME,UPTIME_DATE
	CHARACTER	ASCSINCE*23
 B
	UPTIME(1) = GET_L_VAL(EXE$GL_ABSTIM)			! Up time (sec)A
 )
	CALL LIB$EMUL(10000000,UPTIME,0,UPTIME) 		! 64 bit format
	CALL SYS$GETTIM(SYSTIME)D
	CALL LIB$SUBX(SYSTIME,UPTIME,UPSINCE)
	CALL SYS$ASCTIM(,ASCSINCE,UPSINCE,)			! Up sinceI
 T
	UPTIME_DATE = ASCSINCE(:11)
	UPTIME_TIME = ASCSINCE(13:20)
  
	RETURN	
	END
  
	INTEGER FUNCTION GET_L_VAL(I)
	INTEGER I
	GET_L_VAL = I
	RETURNE
	END
  
 D
 R
	SUBROUTINE CHECK_MAIL(NEW_MAIL)
  
	IMPLICIT INTEGER (A-Z):
 T
	INCLUDE 'BULLFOLDER.INC':
 	
	COMMON /KNOWN_FOLDERS/ FOLDER_Q1,NUM_FOLDERST
	DATA FOLDER_Q1/0/
 
	DIMENSION NEW_MAIL(1)
 
	CHARACTER INPUT*35B
	EQUIVALENCE (INPUT(34:),COUNT)O
 (
	FOLDER_Q = FOLDER_Q1			! so reinit queue pointerN
 N
	OPEN (UNIT=10,FILE='VMSMAIL',DEFAULTFILE='SYS$SYSTEM:VMSMAIL.DAT',L
     &       ACCESS='KEYED',FORM='FORMATTED',ORGANIZATION='INDEXED',
     &       STATUS='OLD',READONLY,SHARED)
 O
	DO I=1,NUM_FOLDERS_
	   CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)Q
 F
	   IF (((.NOT.BTEST(USERB,31).AND.(USERB.NE.0.OR.GROUPB.NE.0)).OR.I
     &		 BTEST(GROUPB,31)).AND.FOLDER_BBOARD(:2).NE.'::') THEN
					! If normal BBOARD or /VMSMAILD
	      READ(10,'(A)',KEY=FOLDER_BBOARD,IOSTAT=IER) INPUT
	      IF (IER.EQ.0.AND.COUNT.GT.0) THEN
		 NEW_MAIL(I) = .TRUE.E
	      ELSEI
		 NEW_MAIL(I) = .FALSE.
	      END IFa
	   ELSE
	      NEW_MAIL(I) = .TRUE. 
	   END IF
	END DOO
 	
	CLOSE (10)I
 N
	RETURND
	END
 
  
 	
	SUBROUTINE GETIMAGE(IMAGNAME,ILEN)
C 
C  SUBROUTINE GETIMAGE(IMAGNAME,ILEN)E
C
C  FUNCTION:
C	To get image name of process.T
C  OUTPUT:
C	IMAGNAME   -    Image name of processO
C	ILEN	   -	Length of imagename_
C1
 N
	IMPLICIT INTEGER (A-Z) 
  
	INCLUDE '($JPIDEF)'
 I
	CHARACTER*(*) IMAGNAME 
 N
	CALL INIT_ITMLST	! Initialize item list
				! Now add items to list 
	CALL ADD_2_ITMLST_WITH_RET(LEN(IMAGNAME),JPI$_IMAGNAME,
     &					%LOC(IMAGNAME),%LOC(ILEN))A
	CALL END_ITMLST(GETJPI_ITMLST)	! Get address of itemlist_
 B
	IER = SYS$GETJPIW(,,,%VAL(GETJPI_ITMLST),,,,)	! Get Info command.
 E
	RETURNO
	END
  
 O
 R
 R
	SUBROUTINE GET_NEWEST_MSG(IN_BTIM,START)	
 D
	IMPLICIT INTEGER (A-Z)A
 b
	INCLUDE 'BULLDIR.INC'
 d
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
  
	DIMENSION IN_BTIM(2),DIR_BTIM(2) 
 r
	IF (REMOTE_SET) THEN'
	   WRITE (REMOTE_UNIT,'(3A)',IOSTAT=IER) 12,IN_BTIM(1),IN_BTIM(2)
	   IF (IER.EQ.0) THEN
	      READ (REMOTE_UNIT,'(A)',IOSTAT=IER) START
	   END IF
	ELSEI
	   CALL READDIR(1,IER) 
	   CALL SYS_BINTIM(DATE//' '//TIME,DIR_BTIM)!
	   DIFFB = COMPARE_BTIM(IN_BTIM,DIR_BTIM)
	   IF (DIFFB.LE.0) THEN
	      START = 0
	      RETURN 
	   END IF
	   CALL READDIR(NBULL,IER)+
	   CALL SYS_BINTIM(DATE//' '//TIME,DIR_BTIM)X
	   DIFFT = COMPARE_BTIM(IN_BTIM,DIR_BTIM)
	   IF (DIFFT.GT.0.OR.IER.EQ.NBULL) THEN
	      START = -1
	      RETURNN
	   END IF
	   BOT = 0e
	   TOP = NBULL + 1 
	   DIFFB = 0T
	   NCHECKS = 0 
	   DO WHILE (DIFFB.LE.0.OR.DIFFT.GT.0)F
	      START = (TOP+BOT) / 2
	      CALL READDIR(START,IER)
	      CALL SYS_BINTIM(DATE//' '//TIME,DIR_BTIM)
	      DIFFB = COMPARE_BTIM(IN_BTIM,DIR_BTIM)r
	      CALL READDIR(START+1,IER)
	      CALL SYS_BINTIM(DATE//' '//TIME,DIR_BTIM)
	      DIFFT = COMPARE_BTIM(IN_BTIM,DIR_BTIM)U
	      IF (DIFFB.GT.0) THENE
		 BOT = START + 1
	      ELSE
		 TOP = START
	      END IF 
	      NCHECKS = NCHECKS + 1
C1
C It should never happen, but test to see if can't findN
C newest message, to avoid looping forever.u
Cc
	      IF (NCHECKS.GT.NBULL) RETURN_
	   END DO
	END IFS
 P
	RETURNC
	END
  
