From:	HENRY::IN%"MRL%PFCVAX%xx.lcs.mit.edu%csnet-relay.CSNET%relay.cs.net@RCA.COM" 19-JUN-1987 20:57
To:	"SYSTEM%UK.AC.SOTON.ESP.V1" <SYSTEM%UK.AC.SOTON.ESP.V1%nss.cs.ucl.ac.uk@xx>, 
Subj:	BULLETIN3.FOR

C
C  BULLETIN3.FOR, Version 6/17/87
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Programmer: Mark R. London
C
	SUBROUTINE BBOARD
C
C  SUBROUTINE BBOARD
C
C  FUNCTION: Converts mail to BBOARD into non-system bulletins.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFILES.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE '($RMSDEF)'

	CHARACTER*11 INEXDATE
	CHARACTER INDESCRIP*80,INFROM*80,INPUT*132
	CHARACTER*8 ACCOUNT

	CALL DISABLE_CTRL

	CALL OPEN_FILE_SHARED(7)

1	DO WHILE (REC_LOCK(IER))
	   READ (7,FMT=FOLDER_FMT,IOSTAT=IER)
     &		FOLDER,FOLDER_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP
     &		,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB
	END DO
	UNLOCK 7

	IF (IER.NE.0) GO TO 900
	IF (FOLDER_BBOARD.EQ.'NONE') GO TO 1
	IF (FOLDER_NUMBER.EQ.0) THEN
	   FOLDER_SET = .FALSE.
	ELSE
	   FOLDER_SET = .TRUE.
	   FOLDER_FILE = FOLDER_DIRECTORY(1:TRIM(FOLDER_DIRECTORY))//
     &		FOLDER
	END IF

	IF ((.NOT.BTEST(USERB,31).AND.(USERB.NE.0.OR.GROUPB.NE.0)).OR.
     &		 BTEST(GROUPB,31)) THEN	! If normal BBOARD or /VMSMAIL
	   CALL CHECK_MAIL(FOLDER_BBOARD,COUNT)	! Any new VMS mail?
	   IF (COUNT.EQ.0) GO TO 1		! None.
	END IF

C
C  The process is set to the BBOARD uic and username in order to create
C  a spawned process that is able to read the BBOARD mail (a real kludge).
C

	CALL GETUSER(USERNAME)		! Get present username
	CALL GETACC(ACCOUNT)		! Get present account
	CALL GETUIC(GROUP,USER)		! Get present uic

	IF (TRIM(FOLDER_BBOARD).GT.0) THEN	! BBOARD name present?
	   IER = SETUSER(FOLDER_BBOARD,USERNAME)! Set to BBOARD username
	   IF (IER.EQ.2) GO TO 910	! Can't set username. New VMS version?
	   CALL SETACC(ACCOUNTB)	! Set to BBOARD account
	   CALL SETUIC(IBCLR(GROUPB,31),IBCLR(USERB,31)) ! Set to BBOARD uic
	END IF

	LEN_B = TRIM(BBOARD_DIRECTORY)
	IER = LIB$DELETE_FILE(BBOARD_DIRECTORY(1:LEN_B)//
     &		FOLDER_BBOARD(1:TRIM(FOLDER_BBOARD))//'.TXT;*')
				! Delete old TXT files left due to errors

	IF (.NOT.BTEST(USERB,31).AND.(USERB.NE.0.OR.GROUPB.NE.0)) THEN
							! If normal BBOARD user
	 IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(1:LEN_B)
     &		//'BOARD.COM','NL:','NL:',,,,STATUS)
	 IF (((STATUS.AND.'1FFFF'X).EQ.RMS$_FNF) .OR.
     &	 ((STATUS .AND. '1FFF0'X).EQ. (RMS$_SPL .AND. '1FFF0'X))) THEN
	   CALL SYS$SETDFPROT('AA00'X,CUR_DEF_PROT)
		! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD:RW,GROUP:RW)
	   OPEN(UNIT=11,FILE=BBOARD_DIRECTORY(1:LEN_B)//'BOARD.COM',
     &		STATUS='NEW',ERR=910,CARRIAGECONTROL='LIST')
	   WRITE(11,'(A)') '$ SET PROTECT=(W:RWED)/DEFAULT'
	   WRITE(11,'(A)') '$ SET PROC/PRIV=SYSPRV'
	   WRITE(11,'(A)')
     & '$ DEFINE/USER EXTRACT_FILE '//BBOARD_DIRECTORY(1:LEN_B)//
     & '''F$GETJPI("","USERNAME")'''
	   WRITE(11,'(A)') '$ MAIL'
	   WRITE(11,'(A)') 'READ'
	   WRITE(11,'(A)') 'EXTRACT/ALL/APPEND EXTRACT_FILE'
	   WRITE(11,'(A)') 'DELETE/ALL'
	   CLOSE(UNIT=11)
	   CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection
	   IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(1:LEN_B)
     &			//'BOARD.COM','NL:','NL:',,,,STATUS)
	 END IF
	ELSE
	 IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(1:LEN_B)//FOLDER_BBOARD
     &	    (1:TRIM(FOLDER_BBOARD))//'.COM','NL:','NL:',,,,STATUS)
	 IF (((STATUS.AND.'1FFFF'X).EQ.RMS$_FNF) .OR.
     &	 ((STATUS .AND. '1FFF0'X).EQ. (RMS$_SPL .AND. '1FFF0'X))) THEN
	    IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(1:LEN_B)//
     &	      'BOARD_SPECIAL.COM','NL:','NL:',,,,STATUS)
	 END IF
	END IF
					! Create sequential mail file
	CALL SETACC(ACCOUNT)		! Reset to original account
	CALL SETUSER(USERNAME)		! Reset to original username
	CALL SETUIC(GROUP,USER)		! Reset to original uic

	OPEN (UNIT=3,FILE=BBOARD_DIRECTORY(1:LEN_B)//FOLDER_BBOARD
     &	   (1:TRIM(FOLDER_BBOARD))//'.TXT',STATUS='OLD',ERR=100)

5	LEN_INPUT = 1
	DO WHILE (LEN_INPUT.GT.0)
	   READ (3,'(Q,A)',END=100) LEN_INPUT,INPUT	! Read next line from mail
	   IF (INPUT(1:5).EQ.'From:') THEN
	      INFROM = INPUT(7:)		! Store username
	   ELSE IF (INPUT(1:5).EQ.'Subj:') THEN
	      INDESCRIP = INPUT(7:)		! Store subject
	   END IF
	END DO


C
C  Add bulletin to bulletin file and directory entry to directory file.
C

10	CALL OPEN_FILE(2)			! Prepare to add dir entry

	READ (3,'(Q,A)',IOSTAT=IER) LEN_INPUT,INPUT	! Read first line
	IF (IER.NE.0) GO TO 100			! If end of file, exit
	IF (LEN_INPUT.EQ.1.AND.INPUT(1:1).EQ.CHAR(12)) GO TO 5
			! If line is just form feed, the message is empty

	CALL OPEN_FILE(1)			! Prepare to add bulletin

	CALL READDIR(0,IER)			! Get NBLOCK
	IF (IER.EQ.0) NBLOCK = 0		! If new file, NBLOCK is 0

	OCOUNT = NBLOCK + 1			! Initialize line count

	SPACE = INDEX(INFROM,' ') - 1		! Strip off the date
	IF (SPACE.GT.0) INFROM = INFROM(1:SPACE)! From the "From:" line

	IF (TRIM(INFROM).GT.12) THEN		! Is length > allowable?
	   LEN_INFROM = TRIM(INFROM)
	   CALL STORE_BULL(6+LEN_INFROM,'From: '//INFROM(1:LEN_INFROM),
     &		OCOUNT)
	   IF (INDEX(INFROM,'::').GT.0)		! Strip off node name
     &		INFROM = INFROM(INDEX(INFROM,'::')+2:)
	   I = 12		! Trim username to first non-alpha character
	   DO WHILE (I.GT.1.AND.
     &		     ((INFROM(I:I).GE.'A'.AND.INFROM(I:I).LE.'Z').OR.
     &		     (INFROM(I:I).GE.'a'.AND.INFROM(I:I).LE.'z')) )
	      I = I - 1
	   END DO
	   IF (I.GT.1) INFROM = INFROM(1:I-1)
	END IF

	LEN_DESCRP = TRIM(INDESCRIP)
	IF (LEN_DESCRP.GT.53) THEN	! Is length > allowable subject length?
	   CALL STORE_BULL(6+LEN_DESCRP,'Subj: '//INDESCRIP(1:LEN_DESCRP),
     &		OCOUNT)
	   INDESCRIP = INDESCRIP(1:LEN_DESCRP)
	   DO I=1,LEN_DESCRP
	      IF (INDESCRIP(I:I).LT.' ') INDESCRIP(I:I) = ' '
	   END DO
	ELSE
	   DO I=1,LEN_DESCRP			! Remove control characters
	      IF (INDESCRIP(I:I).LT.' ') INDESCRIP(I:I) = ' '
	   END DO
	END IF

	ISTART = 0
	NBLANK = 0
	DO WHILE (INPUT(1:1).NE.CHAR(12))	! Move text to bulletin file
	   IF (LEN_INPUT.EQ.0) THEN
	      IF (ISTART.EQ.1) THEN
		 NBLANK = NBLANK + 1
	      END IF
	   ELSE
	      ISTART = 1
	      DO I=1,NBLANK
		 CALL STORE_BULL(1,' ',OCOUNT)
	      END DO
	      NBLANK = 0
	      CALL STORE_BULL(MIN(LEN_INPUT,80),INPUT,OCOUNT)
	      IF (LEN_INPUT.GT.80) THEN		! Breakup line if > 80 chars
		 CALL STORE_BULL(MIN(LEN_INPUT,132)-80,INPUT(81:),OCOUNT)
	      END IF
	   END IF
	   READ (3,'(Q,A)',END=25) LEN_INPUT,INPUT
	END DO

25	CALL FLUSH_BULL(OCOUNT)

	CALL CLOSE_FILE(1)			! Finished adding bulletin

	DESCRIP = INDESCRIP(1:53)		! Description header
	FROM = INFROM(1:12)			! Username
	IF (FOLDER_BBEXPIRE.EQ.-1) THEN		! Folder has expiration time?
	   EXDATE = '5-NOV-2000'		! no, so set date far in future
	   SYSTEM = 2				! indicate permanent message
	ELSE					! Else set expiration date
	   CALL GET_EXDATE(EXDATE,FOLDER_BBEXPIRE)
	   SYSTEM = 0
	END IF
	EXTIME = '00:00:00'
	LENGTH = OCOUNT - NBLOCK		! Number of records

	CALL ADD_ENTRY				! Add the new directory entry

30	CALL CLOSE_FILE(2)			! Totally finished with add

	GO TO 5					! See if there is more mail

100	CLOSE (UNIT=3,STATUS='DELETE')		! Close the input file
	GOTO 1

900	FOLDER_NUMBER = 0
	READ (7,FMT=FOLDER_FMT,IOSTAT=IER,KEY=0,KEYID=1)
     &		FOLDER,FOLDER_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP
     &		,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB
	CALL CLOSE_FILE(7)
	CALL ENABLE_CTRL
	FOLDER_SET = .FALSE.

	RETURN

910	WRITE (6,1010)
	GO TO 100

930	CLOSE (UNIT=3)
	CALL CLOSE_FILE(1)
	CALL CLOSE_FILE(2)
	WRITE (6,1030)
	GO TO 100

1010	FORMAT(' ERROR:Install program with CMKRNL privileges or relink.')
1030	FORMAT(' ERROR:Alert system programmer. Data file problems.')

	END




	SUBROUTINE CREATE_BBOARD_PROCESS

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($PRCDEF)'

	INCLUDE 'BULLFILES.INC'

	CHARACTER*132 IMAGENAME

	CALL GETIMAGE(IMAGENAME,ILEN)

	LEN_B = TRIM(BBOARD_DIRECTORY)

	OPEN(UNIT=11,FILE=BBOARD_DIRECTORY(:LEN_B)//'BULL_COMMAND.COM',
     &		STATUS='OLD',IOSTAT=IER)
	IF (IER.EQ.0) CLOSE(UNIT=11,STATUS='DELETE')

	CALL SYS$SETDFPROT('AA00'X,CUR_DEF_PROT)
		! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD:RW,GROUP:RW)
	OPEN(UNIT=11,FILE=BBOARD_DIRECTORY(:LEN_B)//'BULL_COMMAND.COM',
     &		STATUS='NEW',IOSTAT=IER,CARRIAGECONTROL='LIST')
	IF (IER.NE.0) RETURN
	WRITE(11,'(A)') '$B:=$'//IMAGENAME(:ILEN)
	WRITE(11,'(A)') '$ON ERROR THEN GOTO EXIT'
	WRITE(11,'(A)') '$ON SEVERE THEN GOTO EXIT'
	WRITE(11,'(A)') '$ON WARNING THEN GOTO EXIT'
	WRITE(11,'(A)') '$B/'//'''F$PROCESS()'''
	WRITE(11,'(A)') '$EXIT:'
	WRITE(11,'(A)') '$LOGOUT'
	CLOSE(UNIT=11)
	CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection

	IER = SYS$CREPRC(,'SYS$SYSTEM:LOGINOUT',
     &	   BBOARD_DIRECTORY(:LEN_B)//'BULL_COMMAND.COM','NL:'
     &	   ,,,,'BBOARD',%VAL(4),,,%VAL(PRC$M_NOUAF+PRC$M_DETACH))

	RETURN
	END



	SUBROUTINE GETUIC(GRP,MEM)
C
C  SUBROUTINE GETUIC(UIC)
C
C  FUNCTION:
C	To get UIC of process submitting the job.
C  OUTPUT:
C	GRP   -    Group number of UIC
C	MEM   -	   Member number of UIC
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($JPIDEF)'

	CALL INIT_ITMLST	! Initialize item list
				! Now add items to list
	CALL ADD_2_ITMLST(4,JPI$_GRP,%LOC(GRP))
	CALL ADD_2_ITMLST(4,JPI$_MEM,%LOC(MEM))
	CALL END_ITMLST(GETJPI_ITMLST)	! Get address of itemlist

	IER = SYS$GETJPIW(,,,%VAL(GETJPI_ITMLST),,,,)	! Get Info command.

	RETURN
	END




	SUBROUTINE GET_UPTIME(UPTIME_DATE,UPTIME_TIME)
C
C  SUBROUTINE GET_UPTIME
C
C  FUNCTION: Gets time of last reboot.
C

	IMPLICIT INTEGER (A-Z)

	EXTERNAL	EXE$GL_ABSTIM
	INTEGER 	UPTIME(2),SYSTIME(2),UPSINCE(2)
	CHARACTER*(*)	UPTIME_TIME,UPTIME_DATE
	CHARACTER	ASCSINCE*23

	UPTIME(1) = GET_L_VAL(EXE$GL_ABSTIM)			! Up time (sec)

	CALL LIB$EMUL(10000000,UPTIME,0,UPTIME) 		! 64 bit format
	CALL SYS$GETTIM(SYSTIME)
	CALL LIB$SUBX(SYSTIME,UPTIME,UPSINCE)
	CALL SYS$ASCTIM(,ASCSINCE,UPSINCE,)			! Up since

	UPTIME_DATE = ASCSINCE(1:11)
	UPTIME_TIME = ASCSINCE(13:20)

	RETURN	
	END

	INTEGER FUNCTION GET_L_VAL(I)
	INTEGER I
	GET_L_VAL = I
	RETURN
	END



	SUBROUTINE CHECK_MAIL(USER,NEW_MESSAGES)

	IMPLICIT INTEGER (A-Z)

	CHARACTER INPUT*35,USER*(*)
	EQUIVALENCE (INPUT(34:),COUNT)

	OPEN (UNIT=10,FILE='VMSMAIL',DEFAULTFILE='SYS$SYSTEM:VMSMAIL.DAT',
     &       ACCESS='KEYED',FORM='FORMATTED',ORGANIZATION='INDEXED',
     &       STATUS='OLD',READONLY,SHARED)
	READ(10,'(A)',KEY=USER,IOSTAT=IER) INPUT
	CLOSE (10)

	NEW_MESSAGES = COUNT

	IF (IER.NE.0) COUNT = 0

	RETURN
	END



	SUBROUTINE GETIMAGE(IMAGNAME,ILEN)
C
C  SUBROUTINE GETIMAGE(IMAGNAME,ILEN)
C
C  FUNCTION:
C	To get image name of process.
C  OUTPUT:
C	IMAGNAME   -    Image name of process
C	ILEN	   -	Length of imagename
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($JPIDEF)'

	CHARACTER*(*) IMAGNAME

	CALL INIT_ITMLST	! Initialize item list
				! Now add items to list
	CALL ADD_2_ITMLST_WITH_RET(LEN(IMAGNAME),JPI$_IMAGNAME,
     &					%LOC(IMAGNAME),%LOC(ILEN))
	CALL END_ITMLST(GETJPI_ITMLST)	! Get address of itemlist

	IER = SYS$GETJPIW(,,,%VAL(GETJPI_ITMLST),,,,)	! Get Info command.

	RETURN
	END


C
C  SUBROUTINE ITMLST_SUBS
C
C  FUNCTION:
C	A set of routines to easily create item lists.  It allows one
C  to easily create item lists without the need for declaring arrays
C  or itemlist size.  Thus, the code can be easily changed to add or
C  delete item list codes.
C
C  Here is an example of how to use the routines (prints file to a queue):
C
C	CALL INIT_ITMLST	! Initialize item list
C				! Now add items to list
C	CALL ADD_2_ITMLST(LEN,SJC$_FILE_SPECIFICATION,%LOC(FILENAME))
C	CALL ADD_2_ITMLST(9,SJC$_QUEUE,%LOC(QUEUE))
C	CALL END_ITMLST(SNDJBC_ITMLST)	! Get address of itemlist
C	IER = SYS$SNDJBCW(,%VAL(SJC$_ENTER_FILE),,%VAL(SNDJBC_ITMLST),IOSB,,)
C
C  NOTE: These routines don't presently allow return length address
C  in item list.
C
	SUBROUTINE ITMLST_SUBS

	IMPLICIT INTEGER (A-Z)

	DATA SAVE_ITMLST_ADDRESS/0/,NUM_ITEMS/0/,QUEUE_HEADER/0/

	ENTRY INIT_ITMLST

	IF (QUEUE_HEADER.EQ.0) THEN	! First time INIT_ITMLST ever called?
	   CALL LIB$GET_VM(8,QUEUE_HEADER)  ! Yes, create queue header pointer
	   CALL LIB$MOVC3(4,0,%VAL(QUEUE_HEADER))	! Zero out header
	   CALL LIB$MOVC3(4,0,%VAL(QUEUE_HEADER+4))	! Zero out header
	ELSE IF (SAVE_ITMLST_ADDRESS.GT.0) THEN	! Clean out old item list
	   CALL LIB$FREE_VM((NUM_ITEMS+1)*12,SAVE_ITMLST_ADDRESS)
	   NUM_ITEMS = 0		! Release old itemlist memory
	   SAVE_ITMLST_ADDRESS = 0
	ELSE				! ITMLST calls cannot be nested.
	   WRITE (6,'('' ERROR: INIT_ITMLST called before previous'',$)')
	   WRITE (6,'(''+ ITMLST terminated with END_ITMLST.'')')
	   CALL EXIT
	END IF

	RETURN


	ENTRY ADD_2_ITMLST(BUFLEN,CODE,BUFADR)
C
C  ITMLST entries are initially stored in a queue.  Each queue entry
C  needs 8 bytes for pointer + 12 bytes for itemlist info.
C
	CALL LIB$GET_VM(20,INPUT_ITMLST)	! Get memory for entry

	CALL STORE_ITMLST_ENTRY(%VAL(INPUT_ITMLST+8),BUFLEN,CODE,BUFADR,0)
						! Store data in itemlist format
	CALL LIB$INSQTI(%VAL(INPUT_ITMLST),%VAL(QUEUE_HEADER))
						! Insert entry into queue
	NUM_ITEMS = NUM_ITEMS + 1		! Increment item count

	RETURN


	ENTRY ADD_2_ITMLST_WITH_RET(BUFLEN,CODE,BUFADR,RETADR)
C
C  ITMLST entries are initially stored in a queue.  Each queue entry
C  needs 8 bytes for pointer + 12 bytes for itemlist info.
C
	CALL LIB$GET_VM(20,INPUT_ITMLST)	! Get memory for entry

	CALL STORE_ITMLST_ENTRY(%VAL(INPUT_ITMLST+8),BUFLEN,CODE,BUFADR,
     &							RETADR)
						! Store data in itemlist format
	CALL LIB$INSQTI(%VAL(INPUT_ITMLST),%VAL(QUEUE_HEADER))
						! Insert entry into queue
	NUM_ITEMS = NUM_ITEMS + 1		! Increment item count

	RETURN


	ENTRY END_ITMLST(ITMLST_ADDRESS)

	CALL LIB$GET_VM((NUM_ITEMS+1)*12,ITMLST_ADDRESS)
						! Get memory for itemlist
	SAVE_ITMLST_ADDRESS = ITMLST_ADDRESS	! Save address to remove memory

	DO I=1,NUM_ITEMS			! Place entries into itemlist
	   CALL LIB$REMQHI(%VAL(QUEUE_HEADER),INPUT_ITMLST)
	   CALL LIB$MOVC3(12,%VAL(INPUT_ITMLST+8),
     &		%VAL(ITMLST_ADDRESS+(I-1)*12))
	   CALL LIB$FREE_VM(20,INPUT_ITMLST)
	END DO

	CALL LIB$MOVC3(4,0,%VAL(ITMLST_ADDRESS+NUM_ITEMS*12))
					! Place terminating 0 at end of itemlist

	RETURN
	END



	SUBROUTINE STORE_ITMLST_ENTRY(INPUT_ITMLST,BUFLEN,CODE,BUFADR,
     &							RETADR)

	IMPLICIT INTEGER (A-Z)

	STRUCTURE /ITMLST/
	 UNION
	  MAP
	   INTEGER*2 BUFLEN,CODE
	   INTEGER BUFADR,RETADR
	  END MAP
	 END UNION
	END STRUCTURE

	RECORD /ITMLST/ INPUT_ITMLST(1)

	INPUT_ITMLST(1).BUFLEN = BUFLEN
	INPUT_ITMLST(1).CODE = CODE
	INPUT_ITMLST(1).BUFADR = BUFADR
	INPUT_ITMLST(1).RETADR = RETADR

	RETURN
	END


	SUBROUTINE CLEANUP_LOGIN
C
C  SUBROUTINE CLEANUP_LOGIN
C
C  FUNCTION: Removes entry in user file of user that no longer exist
C		if it creates empty space for new user.
C
	INCLUDE 'BULLUSER.INC'

	CHARACTER*12 LOGIN_USER

	CALL OPEN_FILE_SHARED(8)

	READ (4,'(A12)',IOSTAT=IER1,KEYGT=USERNAME) TEMP_USER ! Look forward one
	READ (8,KEY=TEMP_USER,IOSTAT=IER) TEMP_USER	! See if user exists

	IF (IER.NE.0.AND.IER1.EQ.0.AND.TEMP_USER.NE.USER_HEADER) THEN
	   DELETE(UNIT=4)			! Delete non-existant user
	END IF

	CALL CLOSE_FILE(8)			! All done...

	RETURN
	END


	SUBROUTINE COPY_BULL(INLUN,IBLOCK,OBLOCK,IER)
C
C  SUBROUTINE COPY_BULL
C
C  FUNCTION: To copy data to the bulletin file.
C
C  INPUT:
C	INLUN	-	Input logical unit number
C	IBLOCK	-	Input block number in input file to start at
C	OBLOCK	-	Output block number in output file to start at
C
C  OUTPUT:
C	IER	-	If error in writing to bulletin, IER will be <> 0.
C
C  NOTES:  Input file is accessed using sequential access.  This is 
C	to allow files which have variable records to be read.  The
C       bulletin file is assumed to be opened on logical unit 1.
C

	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLDIR.INC'

	CHARACTER INPUT*80

	DO I=1,IBLOCK-1
	   READ(INLUN,'(A)')
	END DO

	OCOUNT = OBLOCK
	ICOUNT = IBLOCK

	NBLANK = 0
	LENGTH = 0
	DO WHILE (1)
	   LEN = 0
	   DO WHILE (LEN.EQ.0)
	      READ(INLUN,'(Q,A)',END=100) LEN,INPUT
	      LEN = MIN(LEN,TRIM(INPUT),80)
	      IF (LEN.GT.1.AND.ICHAR(INPUT(LEN:LEN)).EQ.10) THEN
		 INPUT(LEN-1:LEN-1) = CHAR(32)	! Remove imbedded
		 INPUT(LEN:LEN) = CHAR(32)	! CR/LFs at end of file.
		 LEN = LEN - 2
	      END IF
	      IF (LEN.GT.0) THEN
		 ICOUNT = ICOUNT + 1
	      ELSE IF (LEN.EQ.0.AND.ICOUNT.GT.IBLOCK) THEN
		 NBLANK = NBLANK + 1
	      END IF
	   END DO
	   IF (NBLANK.GT.0) THEN
	      DO I=1,NBLANK
	         CALL STORE_BULL(1,' ',OCOUNT)
	      END DO
	      LENGTH = LENGTH + NBLANK*2
	      NBLANK = 0
	   END IF
	   CALL STORE_BULL(LEN,INPUT,OCOUNT)
	   LENGTH = LENGTH + LEN + 1
	END DO

100	LENGTH = (LENGTH+127)/128
	IF (LENGTH.EQ.0) THEN
	   IER = 1
	ELSE
	   IER = 0
	END IF

	CALL FLUSH_BULL(OCOUNT)

	RETURN
	END



	SUBROUTINE STORE_BULL(LEN,INPUT,OCOUNT)

	IMPLICIT INTEGER (A-Z)

	PARAMETER BRECLEN=128

	CHARACTER INPUT*(*),OUTPUT*(BRECLEN)

	DATA POINT/0/

	IF (LEN+POINT+1.GT.BRECLEN) THEN
	   IF (POINT.EQ.BRECLEN) THEN
	      WRITE (1'OCOUNT) OUTPUT(1:POINT)
	      OUTPUT = CHAR(LEN)//INPUT
	      POINT = LEN + 1
	   ELSE IF (POINT.EQ.BRECLEN-1) THEN
	      WRITE (1'OCOUNT) OUTPUT(1:POINT)//CHAR(LEN)
	      OUTPUT = INPUT
	      POINT = LEN
	   ELSE
	      WRITE (1'OCOUNT) OUTPUT(1:POINT)//CHAR(LEN)
     &		//INPUT(1:BRECLEN-1-POINT)
	      OUTPUT = INPUT(BRECLEN-POINT:)
	      POINT = LEN - (BRECLEN-1-POINT)
	   END IF
	   OCOUNT = OCOUNT + 1
	ELSE
	   OUTPUT(POINT+1:) = CHAR(LEN)//INPUT(1:LEN)
	   POINT = POINT + LEN + 1
	END IF

	RETURN

	ENTRY FLUSH_BULL(OCOUNT)

	IF (POINT.LT.BRECLEN) OUTPUT(POINT+1:POINT+1) = CHAR(0)
	WRITE (1'OCOUNT) OUTPUT
	POINT = 0

	RETURN

	END


	SUBROUTINE GET_BULL(BLOCK,INPUT,LEN)

	IMPLICIT INTEGER (A-Z)

	PARAMETER BRECLEN=128,LINE_LENGTH=80

	CHARACTER INPUT*(*),TEMP*(BRECLEN), LEFT*(BRECLEN)

	DATA POINT /1/, LEFT_LEN /0/

	IF (LEN.GT.LINE_LENGTH) THEN
	   POINT = 1
	   LEFT_LEN = 0
	END IF

	IF (POINT.EQ.1) THEN
	   DO WHILE (REC_LOCK(IER))
	      READ (1'BLOCK,IOSTAT=IER) TEMP
	   END DO
	ELSE IF (POINT.EQ.BRECLEN+1) THEN
	   LEN = 0
	   POINT = 1
	   RETURN
	END IF

	IF (IER.GT.0) THEN
	   LEN = -1
	   POINT = 1
	   LEFT_LEN = 0
	   RETURN
	END IF

	IF (LEFT_LEN.GT.0) THEN
	   LEN = ICHAR(LEFT(1:1))
	   INPUT = LEFT(2:LEN-LEFT_LEN+1)//TEMP(1:LEFT_LEN)
	   POINT = LEFT_LEN + 1
	   LEFT_LEN = 0
	ELSE
	   LEN = ICHAR(TEMP(POINT:POINT))
	   IF (LEN.GT.BRECLEN-POINT) THEN
	      LEFT = TEMP(POINT:)
	      LEFT_LEN = LEN - (BRECLEN-POINT)
	      LEN = 0
	      POINT = 1
	   ELSE IF (LEN.EQ.0) THEN
	      POINT = 1
	   ELSE
	      INPUT = TEMP(POINT+1:POINT+LEN)
	      POINT = POINT+LEN+1
	   END IF
	END IF

	RETURN

	ENTRY TEST_MORE_LINES(LEN)

	IF (POINT.EQ.BRECLEN+1) THEN
	   LEN = 0
	ELSE
	   LEN = ICHAR(TEMP(POINT:POINT))
	END IF

	RETURN

	END



	SUBROUTINE DELETE_ENTRY(BULL_ENTRY)
C
C  SUBROUTINE DELETE_ENTRY
C
C  FUNCTION:
C	To delete a directory entry.
C
C  INPUTS:
C	BULL_ENTRY  -  Bulletin entry number to delete
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'

	IF (NBULL.GT.0) THEN
	   CALL READDIR(0,IER)
	   NBULL = -NBULL
	   CALL WRITEDIR(0,IER)
	END IF

	DELETE(UNIT=2,REC=BULL_ENTRY+1)

	NEMPTY = NEMPTY + LENGTH
	CALL WRITEDIR(0,IER)

	RETURN
	END




	SUBROUTINE GET_EXDATE(EXDATE,NDAYS)
C
C  SUBROUTINE GET_EXDATE
C
C  FUNCTION:  Computes expiration date giving number of days to expire.
C
	IMPLICIT INTEGER (A-Z)

	CHARACTER*11 EXDATE

	CHARACTER*3 MONTHS(12)
	DIMENSION LENGTH(12)
	DATA MONTHS/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     &		    'OCT','NOV','DEC'/
	DATA LENGTH/31,27,31,30,31,30,31,31,30,31,30,31/

	CALL SYS$ASCTIM(,EXDATE,,)		! Get the present date

	DECODE(2,'(I2)',EXDATE(1:2)) DAY	! Get day
	DECODE(4,'(I4)',EXDATE(8:11)) YEAR	! Get year

	MONTH = 1
	DO WHILE (MONTHS(MONTH).NE.EXDATE(4:6))	! Get month
	   MONTH = MONTH + 1
	END DO

	IF (MOD(YEAR,4).EQ.0) THEN		! Correct February length
	   LENGTH(2) = 28			! if we're in a leap year
	ELSE
	   LENGTH(2) = 27
	END IF

	NUM_DAYS = NDAYS	! Put number of days into buffer variable

	DO WHILE (NUM_DAYS.GT.0)
	   IF (NUM_DAYS+DAY.GT.LENGTH(MONTH)) THEN
				! If expiration date exceeds end of month
	      NUM_DAYS = NUM_DAYS - (LENGTH(MONTH) - DAY + 1)
				! Decrement # of days by days left in month
	      DAY = 1				! Reset day to first of month
	      MONTH = MONTH + 1			! Increment month pointer
	      IF (MONTH.EQ.13) THEN		! Moved into next year?
		 MONTH = 1			! Reset month pointer
		 YEAR = YEAR + 1		! Increment year pointer
	         IF (MOD(YEAR,4).EQ.0) THEN	! Correct February length
	            LENGTH(2) = 28		! if we're in a leap year
	         ELSE
	            LENGTH(2) = 27
	         END IF
	      END IF
	   ELSE			! If expiration date is within the month
	      DAY = DAY + NUM_DAYS		! Find expiration day
	      NUM_DAYS = 0			! Force loop exit
	   END IF
	END DO

	ENCODE(2,'(I2)',EXDATE(1:2)) DAY	! Put day into new date
	ENCODE(4,'(I4)',EXDATE(8:11)) YEAR	! Put year into new date
	EXDATE(4:6) = MONTHS(MONTH)		! Put month into new date

	RETURN
	END




	SUBROUTINE GET_LINE(INPUT,LEN_INPUT)
C
C  SUBROUTINE GET_LINE
C
C  FUNCTION:
C	Gets line of input from terminal.
C
C  OUTPUTS:
C	LEN_INPUT  -  Length of input line.  If = -1, CTRLC entered.
C		      if = -2, CTRLZ entered.
C
C  NOTES:
C	Assumes terminal assigned to TERM_CHAN in common /TERM_CHAN/.
C	Also, on first call, set LEN_INPUT to 1+LENGTH OF INPUT CHARCTER
C	for initializing the CTRLC AST.
C

	IMPLICIT INTEGER (A-Z)

	LOGICAL*1 DESCRIP(8),DTYPE,CLASS
	INTEGER*2 LENGTH
	CHARACTER*(*) INPUT
	EQUIVALENCE (DESCRIP(1),LENGTH),(DESCRIP(3),DTYPE)
	EQUIVALENCE (DESCRIP(4),CLASS),(DESCRIP(5),POINTER)
	EXTERNAL IO$_SETMODE,IO$M_CTRLCAST,CTRLC_ROUTINE
	COMMON /TERM_CHAN/ TERM_CHAN

	INCLUDE '($RMSDEF)'

	COMMON /DECNET/ DECNET_PROC,ERROR_UNIT
	LOGICAL DECNET_PROC

	CHARACTER*(*) PROMPT
	LOGICAL*1 USE_PROMPT

	USE_PROMPT = .FALSE.

	GO TO 5

	ENTRY GET_INPUT_PROMPT(INPUT,LEN_INPUT,PROMPT)

	USE_PROMPT = .TRUE.

5	LIMIT = LEN(INPUT)			! Get input line size limit
	INPUT = ' '				! Clean out input buffer

C
C  Initialize CTRL-C AST with AST routine CTRLC_ROUTINE and
C  AST parameter FLAG.  When CTRLC occurs, FLAG is set to 1
C

	FLAG = 0				! Yep, init CTRL-C flag
	IO_CTRLC = %LOC(IO$_SETMODE)+%LOC(IO$M_CTRLCAST)	! Set AST code
	IER=SYS$QIOW(,%VAL(TERM_CHAN),%VAL(IO_CTRLC),,,,	! for QIO
     &	      CTRLC_ROUTINE,FLAG,,,,)		! Enable the AST

	LEN_INPUT = 0				! Nothing inputted yet

	LENGTH = 0				! Init special variable
	DTYPE = 0				! descriptor so we won't
	CLASS = 2				! run into any memory limit
	POINTER = 0				! during input.

C
C  LIB$GET_INPUT is nice way of getting input from terminal,
C  as it handles such thing as accidental wrap around to next line.
C

	IF (DECNET_PROC) THEN
	   READ (5,'(Q,A)',IOSTAT=IER) LEN_INPUT,INPUT
	   IF (IER.NE.0) LEN_INPUT = -2 
	   RETURN
	ELSE IF (USE_PROMPT) THEN
	   IER = LIB$GET_INPUT(DESCRIP,PROMPT)	! Get line from terminal
	ELSE
	   IER = LIB$GET_INPUT(DESCRIP)		! Get line from terminal
	END IF

	CALL STR$TRIM(DESCRIP,DESCRIP,LEN_INPUT)

	IF (FLAG.EQ.0) THEN			! If no CTRL-C has occurred
	   IER1 = SYS$CANCEL(%VAL(TERM_CHAN))	! Cancel CTRL-C AST
	   IF (IER.NE.RMS$_EOF) THEN		! End of input?
	      LEN_INPUT = MIN(LIMIT,LENGTH)	! No. Get length of line
	      DO I=0,LEN_INPUT-1		! Extract from descriptor
	         CALL GET_VAL(INPUT(I+1:I+1),%VAL(POINTER+I))
	      END DO
	      CALL CONVERT_TABS(INPUT,LEN_INPUT)
	      LEN_INPUT = MAX(LEN_INPUT,LENGTH)
	   ELSE
	      LEN_INPUT = -2			! If CTRL-Z, say so
	   END IF
	ELSE
	   LEN_INPUT = -1			! If CTRL-C, say so
	END IF
	RETURN
	END



	SUBROUTINE CONVERT_TABS(INPUT,LEN_INPUT)

	IMPLICIT INTEGER (A-Z)

	CHARACTER*(*) INPUT

	PARAMETER TAB = CHAR(9)

	LIMIT = LEN(INPUT)

	DO WHILE (INDEX(INPUT,TAB).GT.0.AND.LEN_INPUT.LT.LIMIT)
	   TAB_POINT = INDEX(INPUT,TAB)	! Remove tabs
	   MOVE = ((TAB_POINT-1)/8)*8 + 9
	   ADD = MOVE - TAB_POINT
	   IF (MOVE-1.LE.LIMIT) THEN
	      INPUT(MOVE:) = INPUT(TAB_POINT+1:)
	      DO I = TAB_POINT,MOVE-1
	         INPUT(I:I) = ' '
	      END DO
	      LEN_INPUT = LEN_INPUT + ADD - 1
	   ELSE
	      DO I = TAB_POINT,LIMIT
	         INPUT(I:I) = ' '
	      END DO
	      LEN_INPUT = LIMIT+1
	   END IF
	END DO

	RETURN
	END


	SUBROUTINE GET_VAL(OUTPUT,INPUT)	! Used to convert logical
	CHARACTER*(*) OUTPUT			! byte to character value
	LOGICAL*1 INPUT
	OUTPUT = CHAR(INPUT)
	RETURN
	END

	SUBROUTINE CTRLC_ROUTINE(FLAG)		! CTRL-C AST routine
	IMPLICIT INTEGER (A-Z)			! If CTRL-C, come here
	FLAG = 1				! to set flag
	RETURN
	END



	SUBROUTINE GET_INPUT_NOECHO(DATA)
C
C  SUBROUTINE GET_INPUT_NOECHO
C
C  FUNCTION: Reads data in from terminal without echoing characters.
C	     Also contains entry to assign terminal and purge
C	     type ahead buffer.
C
	IMPLICIT INTEGER (A-Z)

	CHARACTER*(*) DATA

	EXTERNAL IO$_READVBLK,IO$M_NOECHO,IO$M_PURGE,IO$M_TIMED

	COMMON /TERM_CHAN/ TERM_CHAN

	DO I=1,LEN(DATA)
	   DATA(I:I) = ' '
	END DO

	IO_READ = %LOC(IO$_READVBLK)+%LOC(IO$M_NOECHO)

	IER = SYS$QIOW(,%VAL(TERM_CHAN),%VAL(IO_READ),,,,
     &		  %VAL(%LOC(DATA)),%VAL(LEN(DATA)),,,,)

	RETURN

	ENTRY ASSIGN_TERMINAL

	IER = SYS$ASSIGN('TT',TERM_CHAN,,)	! Assign terminal

	RETURN

	ENTRY PURGE_TYPEAHEAD			! Purge type-ahead buffer

	IO_READ = %LOC(IO$_READVBLK)+%LOC(IO$M_NOECHO)+%LOC(IO$M_PURGE)
     &		+%LOC(IO$M_TIMED)

	IER = SYS$QIOW(,%VAL(TERM_CHAN),%VAL(IO_READ),,,,
     &	 %VAL(%LOC(IER)),%VAL(0),%VAL(5),,,)	! Purge type ahead buffer

	RETURN
	END





	SUBROUTINE GETPAGLEN(PAGE_LENGTH)
C
C  SUBROUTINE GETPAGLEN
C
C  FUNCTION:
C	Gets page length of the terminal.
C
C  OUTPUTS:
C	PAGE_LENGTH  -  Page length of the terminal.
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE '($DVIDEF)'

	LOGICAL*1 DEVDEPEND(4)

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(4,DVI$_DEVDEPEND,%LOC(DEVDEPEND(1)))
	CALL END_ITMLST(GETDVI_ITMLST)		! Get address of itemlist

	CALL SYS$GETDVIW(,,'TT',%VAL(GETDVI_ITMLST),,,,)

	PAGE_LENGTH = DEVDEPEND(4)

	RETURN
	END





	LOGICAL FUNCTION SLOW_TERMINAL
C
C  FUNCTION SLOW_TERMINAL
C
C  FUNCTION:
C	Indicates that terminal has a slow speed (2400 baud or less).
C
C  OUTPUTS:
C	SLOW_TERMINAL = .true. if slow, .false. if not.
C

	IMPLICIT INTEGER (A-Z)

	EXTERNAL IO$_SENSEMODE

	COMMON /TERM_CHAN/ TERM_CHAN

	COMMON CHAR_BUF(2)

	LOGICAL*1 IOSB(8)

	INCLUDE '($TTDEF)'

	IER = SYS$QIOW(,%VAL(TERM_CHAN),IO$_SENSEMODE,IOSB,,,
     &		  CHAR_BUF,%VAL(8),,,,)

	IF (IOSB(3).LE.TT$C_BAUD_2400) THEN
	   SLOW_TERMINAL = .TRUE.
	ELSE
	   SLOW_TERMINAL = .FALSE.
	END IF

	RETURN
	END




	SUBROUTINE SHOW_PRIV
C
C  SUBROUTINE SHOW_PRIV
C
C  FUNCTION:
C	To show privileges necessary for managing bulletin board.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLUSER.INC'

	INCLUDE '($PRVDEF)'

	COMMON /PRVDEF/ PRIVS
	CHARACTER*8 PRIVS(0:38)

	CALL OPEN_FILE_SHARED(4)		! Get BULLUSER.DAT file

	DO WHILE (REC_LOCK(IER))
	   READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER)
     &		TEMP_USER,NEWEST_BTIM,	! Get newest bulletin
     &		BBOARD_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	END DO

	IF (IER.EQ.0) THEN			! If header is present, exit
	   IF (NEW_FLAG(1).EQ.-1.AND.NEW_FLAG(2).EQ.-1) THEN  ! Info not present
	      CALL CLOSE_FILE(4)
	      CALL OPEN_FILE_SHARED(4)		! Get BULLUSER.DAT file
	      READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER)
     &		TEMP_USER,NEWEST_BTIM,	! Get newest bulletin
     &		BBOARD_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	      NEW_FLAG(1) = PRV$M_OPER.OR.PRV$M_CMKRNL.OR.PRV$M_SETPRV
	      NEW_FLAG(2) = 0
	      REWRITE (4,FMT=USER_FMT)
     &		TEMP_USER,NEWEST_BTIM,BBOARD_BTIM,
     &		NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	   END IF
	   WRITE (6,'('' Following privileges are needed for privileged
     & commands:'')')
	   DO I=0,38
	      IF ((I.LT.32.AND.BTEST(NEW_FLAG(1),I)).OR.
     &		  (I.GT.31.AND.BTEST(NEW_FLAG(2),I-32))) THEN
		 WRITE (6,'(1X,A)') PRIVS(I)
	      END IF
	   END DO
	ELSE
	   WRITE (6,'('' ERROR: Cannot show privileges.'')')
	END IF

	CALL CLOSE_FILE(4)			! All finished with BULLUSER

	RETURN

	END




	SUBROUTINE SET_PRIV
C
C  SUBROUTINE SET_PRIV
C
C  FUNCTION:
C	To set privileges necessary for managing bulletin board.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($PRVDEF)'

	INCLUDE 'BULLUSER.INC'

	COMMON /PRIVILEGES/ PROCPRIV(2),NEEDPRIV(2)

	COMMON /PRVDEF/ PRIVS
	CHARACTER*8 PRIVS(0:38)
	DATA PRIVS
     &	/'CMKRNL','CMEXEC','SYSNAM','GRPNAM','ALLSPOOL','DETACH',
     &  'DIAGNOSE','LOG_IO','GROUP','ACNT','PRMCEB','PRMMBX','PSWAPM',
     &	'ALTPRI','SETPRV','TMPMBX','WORLD','MOUNT','OPER','EXQUOTA',
     &	'NETMBX','VOLPRO','PHY_IO','BUGCHK','PRMGBL','SYSGBL','PFNMAP',
     &	'SHMEM','SYSPRV','BYPASS','SYSLCK','SHARE','UPGRADE','DOWNGRADE',
     &	'GRPPRV','READALL',' ',' ','SECURITY'/

	EXTERNAL CLI$_ABSENT

	DIMENSION ONPRIV(2),OFFPRIV(2)

	CHARACTER*8 INPUT_PRIV

	IF (.NOT.SETPRV_PRIV().OR..NOT.BTEST(PROCPRIV(1),PRV$V_SETPRV)) THEN
	   WRITE (6,'('' ERROR: This command requires SETPRV privileges.'')')
	   RETURN
	END IF

	OFFPRIV(1) = 0
	OFFPRIV(2) = 0
	ONPRIV(1) = 0
	ONPRIV(2) = 0

	DO WHILE (CLI$GET_VALUE('PRIVILEGES',INPUT_PRIV,LEN)
     &	    .NE.%LOC(CLI$_ABSENT))		! Get the specified nodes
	   PRIV_FOUND = -1
	   I = 0
	   DO WHILE (I.LT.39.AND.PRIV_FOUND.EQ.-1)
	      IF (INPUT_PRIV(:LEN).EQ.PRIVS(I)) PRIV_FOUND = I
	      IF (INPUT_PRIV(3:LEN).EQ.PRIVS(I)) PRIV_FOUND = I
	      I = I + 1
	   END DO
	   IF (PRIV_FOUND.EQ.-1) THEN
	      WRITE(6,'('' ERROR: Incorrectly specified privilege = '',
     &		A)') INPUT_PRIV(1:LEN)
	      RETURN
	   ELSE IF (INPUT_PRIV(1:2).EQ.'NO') THEN
	      IF (INPUT_PRIV.EQ.'NOSETPRV') THEN
	       WRITE(6,'('' ERROR: Cannot remove SETPRV privileges.'')')
	       RETURN
	      ELSE IF (PRIV_FOUND.LT.32) THEN
		 OFFPRIV(1) = IBSET(OFFPRIV(1),PRIV_FOUND)
	      ELSE
		 OFFPRIV(2) = IBSET(OFFPRIV(2),PRIV_FOUND-32)
	      END IF
	   ELSE
	      IF (PRIV_FOUND.LT.32) THEN
		 ONPRIV(1) = IBSET(ONPRIV(1),PRIV_FOUND)
	      ELSE
		 ONPRIV(2) = IBSET(ONPRIV(2),PRIV_FOUND-32)
	      END IF
	   END IF
	END DO

	CALL OPEN_FILE(4)		! Get BULLUSER.DAT file

	READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER)
     &	 TEMP_USER,NEWEST_BTIM,	! Get newest bulletin
     &	 BBOARD_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG

	IF (IER.EQ.0) THEN			! If header is present, exit
	   NEW_FLAG(1) = NEW_FLAG(1).OR.ONPRIV(1)
	   NEW_FLAG(2) = NEW_FLAG(2).OR.ONPRIV(2)
	   NEW_FLAG(1) = NEW_FLAG(1).AND.(.NOT.OFFPRIV(1))
	   NEW_FLAG(2) = NEW_FLAG(2).AND.(.NOT.OFFPRIV(2))
	   REWRITE (4,FMT=USER_FMT)
     &		TEMP_USER,NEWEST_BTIM,BBOARD_BTIM,
     &		NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	   WRITE (6,'('' Privileges successfully modified.'')')
	ELSE
	   WRITE (6,'('' ERROR: Cannot modify privileges.'')')
	END IF

	CALL CLOSE_FILE(4)			! All finished with BULLUSER

	RETURN

	END
