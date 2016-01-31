From:	KBSVAX::KANE "Joseph Kane"  4-APR-1988 19:33
To:	everhart@arisia.DECNET
Subj:	forwarded mail

From uunet!rutgers.edu!Postmaster Mon Apr  4 17:17:47 1988
Received:  by kbsvax.steinmetz (1.2/1.1x Steinmetz)
	 id AA20994; Mon, 4 Apr 88 16:59:01 edt
Received: from RUTGERS.EDU by uunet.UU.NET (5.54/1.14) 
	id AA07874; Mon, 4 Apr 88 15:08:59 EDT
Received: by rutgers.edu (5.54/1.15) 
	id AB20964; Mon, 4 Apr 88 15:10:17 EDT
Date: Mon, 4 Apr 88 15:10:17 EDT
From: uunet!rutgers.edu!Postmaster (Mail Delivery Subsystem)
Subject: Returned mail: Host unknown
Message-Id: <8804041910.AB20964@rutgers.edu>
To: <MAILER-DAEMON>
Status: R
 
   ----- Transcript of session follows -----
550 <pfc-vax.mit.edu!mrl@rutgers.edu>... Host unknown
 
   ----- Unsent message follows -----
Received: by rutgers.edu (5.54/1.15) 
	id AA19920; Mon, 4 Apr 88 12:28:50 EDT
Received: from steinmetz.UUCP by uunet.UU.NET (5.54/1.14) with UUCP 
	id AA02176; Mon, 4 Apr 88 12:23:27 EDT
Received:  by kbsvax.steinmetz (1.2/1.1x Steinmetz)
	 id AA16052; Mon, 4 Apr 88 10:25:43 edt
Date:  2 Apr 88 20:17:14 EST
From: steinmetz!MAILER-DAEMON@uunet.uu.net (Mail Delivery Subsystem)
Subject: Returned mail: User unknown
Message-Id: <8804041425.AA16052@kbsvax.steinmetz>
To: MRL@pfc-vax.mit.edu
 
   ----- Transcript of session follows -----
mail11: %MAIL-E-SYNTAX, error parsing 'CRD'
550 crd.ge.com!EVERHART%ARISIA.DECNET... User unknown
 
   ----- Unsent message follows -----
Received:  by kbsvax.steinmetz (1.2/1.1x Steinmetz)
	 id AA16049; Mon, 4 Apr 88 10:25:43 edt
Received: by ge-dab.GE.COM (smail2.5)
	id AA19227; 4 Apr 88 06:43:03 EDT (Mon)
Received: by ge-rtp.GE.COM (smail2.5)
	id AA10856; 3 Apr 88 21:17:41 EST (Sun)
Received: by mcnc.mcnc.org (5.54/MCNC/10-20-87)
	id AA14769; Sat, 2 Apr 88 21:16:22 EST
From: <mcnc!rutgers.edu!pfc-vax.mit.edu!MRL>
Received: by rutgers.edu (5.54/1.15) 
	id AA01509; Sat, 2 Apr 88 20:53:40 EST
Message-Id: <8804030153.AA01509@rutgers.edu>
Received: from PFC-VAX.MIT.EDU by XX.LCS.MIT.EDU via Chaosnet; 2 Apr 88 20:18-EST
Date:  2 Apr 88 20:17:14 EST
To: xx!TENCATI@vlsi.jpl.nasa.gov, xx!MHG@mitre-bedford.arpa,
        crd.ge.com!xx!EVERHART@ARISIA.DECNET, xx!GAYMAN@ari-hq1.arpa,
        radc-softvax!xx!BACH
Subject: BULLETIN4.FOR
 
C
C  BULLETIN4.FOR, Version 3/24/88
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Programmer: Mark R. London
C
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
 
	LOGIN_USER = USERNAME
	READ (4,IOSTAT=IER1,KEYGT=USERNAME) USER_ENTRY ! Look forward one
	TEMP_USER = USERNAME
	USERNAME = LOGIN_USER
	READ (8,KEY=TEMP_USER,IOSTAT=IER) TEMP_USER	! See if user exists
 
	IF (IER.NE.0.AND.IER1.EQ.0.AND.TEMP_USER.NE.USER_HEADER_KEY) THEN
	   DELETE(UNIT=4)			! Delete non-existant user
	   CALL OPEN_FILE(9)
	   READ (9,KEY=TEMP_USER,IOSTAT=IER)
	   IF (IER.EQ.0) DELETE(UNIT=9)
	   CALL CLOSE_FILE(9)
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
 
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 
	INCLUDE 'BULLDIR.INC'
 
	CHARACTER INPUT*80
 
	IF (REMOTE_SET) THEN
	   WRITE (REMOTE_UNIT,'(A)',IOSTAT=IER) 2
	   IF (IER.NE.0) CALL ERROR_AND_EXIT
	END IF
 
	DO I=1,IBLOCK-1
	   READ(INLUN,'(A)')
	END DO
 
	OCOUNT = OBLOCK
	ICOUNT = IBLOCK
 
	NBLANK = 0
	LENGTH = 0
	DO WHILE (1)
	   ILEN = 0
	   DO WHILE (ILEN.EQ.0)
	      READ(INLUN,'(Q,A)',END=100) ILEN,INPUT
	      ILEN = MIN(ILEN,TRIM(INPUT),80)
	      IF (ILEN.GT.1.AND.ICHAR(INPUT(ILEN:ILEN)).EQ.10) THEN
		 INPUT(ILEN-1:ILEN-1) = CHAR(32)	! Remove imbedded
		 INPUT(ILEN:ILEN) = CHAR(32)	! CR/LFs at end of file. 
		 ILEN = ILEN - 2
	      END IFm
	      IF (ILEN.GT.0) THEN
		 ICOUNT = ICOUNT + 1
	      ELSE IF (ILEN.EQ.0.AND.ICOUNT.GT.IBLOCK) THEN
		 NBLANK = NBLANK + 1
	      END IFT
	   END DO
	   IF (NBLANK.GT.0) THEN
	      DO I=1,NBLANK
	         CALL STORE_BULL(1,' ',OCOUNT)e
	      END DO
	      LENGTH = LENGTH + NBLANK*21
	      NBLANK = 04
	   END IF
	   CALL STORE_BULL(ILEN,INPUT,OCOUNT)
	   LENGTH = LENGTH + ILEN + 1
	END DOR
 r
100	LENGTH = (LENGTH+127)/128-
	IF (LENGTH.EQ.0) THEN
	   IER = 1
	ELSEI
	   IER = 0
	END IF
 
	CALL FLUSH_BULL(OCOUNT)
 i
	RETURNs
	END
 5
 p
 v
	SUBROUTINE STORE_BULL(ILEN,INPUT,OCOUNT)
 
	IMPLICIT INTEGER (A-Z) 
 l
	PARAMETER BRECLEN=128
 t
	CHARACTER INPUT*(*),OUTPUT*(BRECLEN)n
  
	DATA POINT/0/
 
	IF (ILEN+POINT+1.GT.BRECLEN) THEN
	   IF (POINT.EQ.BRECLEN) THEN
	      CALL WRITE_BULL_FILE(OCOUNT,OUTPUT(:POINT))
	      OUTPUT = CHAR(ILEN)//INPUTS
	      POINT = ILEN + 1 
	   ELSE IF (POINT.EQ.BRECLEN-1) THENr
	      CALL WRITE_BULL_FILE(OCOUNT,OUTPUT(:POINT)//CHAR(ILEN))
	      OUTPUT = INPUT
	      POINT = ILENl
	   ELSE
	      CALL WRITE_BULL_FILE(OCOUNT,OUTPUT(:POINT)//CHAR(ILEN)R
     &		//INPUT(:BRECLEN-1-POINT))
	      OUTPUT = INPUT(BRECLEN-POINT:)1
	      POINT = ILEN - (BRECLEN-1-POINT)5
	   END IF
	   OCOUNT = OCOUNT + 1.
	ELSEn
	   OUTPUT(POINT+1:) = CHAR(ILEN)//INPUT(:ILEN)
	   POINT = POINT + ILEN + 1
	END IF.
 S
	RETURN
  
	ENTRY FLUSH_BULL(OCOUNT)0
 :
	IF (POINT.LT.BRECLEN) OUTPUT(POINT+1:POINT+1) = CHAR(0)
	CALL WRITE_BULL_FILE(OCOUNT,OUTPUT)
	POINT = 0
 E
	RETURNi
 5
	END
 A
 5
	SUBROUTINE WRITE_BULL_FILE(OCOUNT,OUTPUT)
 n
	IMPLICIT INTEGER (A-Z)2
 7
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
  
	CHARACTER*(*) OUTPUTa
 i
	IF (REMOTE_SET) THENy
	   WRITE (REMOTE_UNIT,'(2A)',IOSTAT=IER) 6,OUTPUT
	ELSE3
	   WRITE (1'OCOUNT) OUTPUT0
	END IF0
 u
	RETURN>
	END
 d
 r
	SUBROUTINE GET_BULL(IBLOCK,INPUT,ILEN)C
 s
	IMPLICIT INTEGER (A-Z)
 e
	INCLUDE 'BULLDIR.INC'
 o
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 d
	COMMON /REMOTE_READ_MESSAGE/ SCRATCH_R1
	DATA SCRATCH_R1 /0/
 -
	PARAMETER BRECLEN=128,LINE_LENGTH=80H
 u
	CHARACTER INPUT*(*),TEMP*(BRECLEN), LEFT*(BRECLEN)o
 /
	DATA POINT /1/, LEFT_LEN /0/b
 t
	IF (ILEN.GT.LINE_LENGTH) THEN
	   POINT = 1 
	   LEFT_LEN = 0
	END IF1
 0
	IF (POINT.EQ.1) THENM
	   IF (REMOTE_SET) THEN
	      IF (IBLOCK.EQ.BLOCK) SCRATCH_R = SCRATCH_R1	      i
	      CALL READ_QUEUE(%VAL(SCRATCH_R),SCRATCH_R,TEMP)
	   ELSE
	      DO WHILE (REC_LOCK(IER))o
	         READ (1'IBLOCK,IOSTAT=IER) TEMPh
	      END DOn
	   END IF
	ELSE IF (POINT.EQ.BRECLEN+1) THEN
	   ILEN = 0
	   POINT = 1x
	   RETURN
	END IFh
 o
	IF (IER.GT.0) THENo
	   ILEN = -1
	   POINT = 1L
	   LEFT_LEN = 0
	   RETURN
	END IFa
 i
	IF (LEFT_LEN.GT.0) THEN
	   ILEN = ICHAR(LEFT(:1))
	   INPUT = LEFT(2:ILEN-LEFT_LEN+1)//TEMP(:LEFT_LEN)
	   POINT = LEFT_LEN + 1
	   LEFT_LEN = 0
	ELSE)
	   ILEN = ICHAR(TEMP(POINT:POINT))Y
	   IF (ILEN.GT.BRECLEN-POINT) THENL
	      LEFT = TEMP(POINT:)
	      LEFT_LEN = ILEN - (BRECLEN-POINT)
	      ILEN = 0
	      POINT = 1
	   ELSE IF (ILEN.EQ.0) THEN
	      POINT = 1
	   ELSE
	      INPUT = TEMP(POINT+1:POINT+ILEN)N
	      POINT = POINT+ILEN+1r
	   END IF
	END IFB
 T
	RETURNU
 E
	ENTRY TEST_MORE_LINES(ILEN)
  
	IF (POINT.EQ.BRECLEN+1) THEN0
	   ILEN = 0
	ELSE!
	   ILEN = ICHAR(TEMP(POINT:POINT))4
	END IFU
 _
	RETURN)
 Z
	END
 e
 r
 E
	SUBROUTINE GET_REMOTE_MESSAGE(IER)N
CC
C  SUBROUTINE GET_REMOTE_MESSAGE
CF
C  FUNCTION:
C	Gets remote message.
CS
 
	IMPLICIT INTEGER (A-Z)a
 o
	INCLUDE 'BULLDIR.INC'
 E
	CHARACTER*128 INPUT
 S
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 6
	COMMON /REMOTE_READ_MESSAGE/ SCRATCH_R1
	DATA SCRATCH_R1 /0/
 6
	IF (SCRATCH_R1.NE.0) THEN		! Is queue empty?
	   SCRATCH_R = SCRATCH_R1		! No, set queue pointer to headI
	ELSE					! Else if queue is empty
	   CALL INIT_QUEUE(SCRATCH_R,INPUT)
	   SCRATCH_R1 = SCRATCH_R		! Init header pointeri
	END IF 
 e
	ILEN = 128t
	IER = 0
	LENGTH = 0E
	DO WHILE (ILEN.GT.0.AND.IER.EQ.0)
	   READ (REMOTE_UNIT,'(Q,A)',IOSTAT=IER) ILEN,INPUT
	   IF (IER.NE.0) THEN
	      LENGTH = 0 
	      IER1 = IERr
	      CALL DISCONNECT_REMOTEU
	      IER = IER1	! IER is set to 0 by DISCONNECT_REMOTE
	   ELSE IF (ILEN.GT.0) THEN
	      CALL WRITE_QUEUE(%VAL(SCRATCH_R),SCRATCH_R,INPUT)
	      LENGTH = LENGTH + 1
	   END IF
	END DO
 
	RETURNS
	END
  
  
 t
 l
	SUBROUTINE DELETE_ENTRY(BULL_ENTRY)
C 
C  SUBROUTINE DELETE_ENTRY
C 
C  FUNCTION:
C	To delete a directory entry.
C,
C  INPUTS:
C	BULL_ENTRY  -  Bulletin entry number to delete
CY
 A
	IMPLICIT INTEGER (A-Z)C
 ,
	INCLUDE 'BULLDIR.INC'
 D
	INCLUDE 'BULLFOLDER.INC't
 i
	CHARACTER*80 INPUT$
 Q
	IF (NBULL.GT.0) THENV
	   CALL READDIR(0,IER)	
	   NBULL = -NBULL
	   CALL WRITEDIR(0,IER)
	END IF	
 n
	IF (BTEST(FOLDER_FLAG,1)) THEN
	   OPEN(UNIT=3,FILE=FOLDER_FILE//'.LOG',IOSTAT=IER,
     &		STATUS='OLD',CARRIAGECONTROL='LIST',ACCESS='APPEND')
	   IF (IER.NE.0) THEN
	      OPEN(UNIT=3,FILE=FOLDER_FILE//'.LOG',ERR=900,
     &		STATUS='NEW',CARRIAGECONTROL='LIST')
	   ELSE
	      WRITE (3,'(A)') CHAR(12)V
	   END IF
 )
	   WRITE (3,1050) DESCRIP		! Output bulletin header info
	   WRITE (3,1060) FROM,DATE
 1
	   CALL OPEN_FILE(1)E
 M
	   ILEN = 81T
	   DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file+
	      DO WHILE (ILEN.GT.0)t
	         CALL GET_BULL(I,INPUT,ILEN)R
	         IF (ILEN.LT.0) THENO
		    GO TO 90
	         ELSE IF (ILEN.GT.0) THEN
	            WRITE (3,'(A)') INPUT(:ILEN) 
	         END IF
	      END DOT
	      ILEN = 80
	   END DO
 E
90	   CLOSE (UNIT=3)			! Bulletin copy completed
 D
	   CALL CLOSE_FILE(1)
	END IFE
 
900	DELETE(UNIT=2,REC=BULL_ENTRY+1)
 I
	NEMPTY = NEMPTY + LENGTHL
	CALL WRITEDIR(0,IER)D
  
1050	FORMAT(/,'Description: ',A53)
1060	FORMAT('From: ',A12,' Date: ',A11,/)R
 R
	RETURN
	END
 S
 O
 N
 L
	SUBROUTINE GET_EXDATE(EXDATE,NDAYS)
CO
C  SUBROUTINE GET_EXDATE
Cs
C  FUNCTION:  Computes expiration date giving number of days to expire.s
Ce
	IMPLICIT INTEGER (A-Z)U
 '
	CHARACTER*11 EXDATE
 A
	CHARACTER*3 MONTHS(12)C
	DIMENSION LENGTH(12)
	DATA MONTHS/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',E
     &		    'OCT','NOV','DEC'/
	DATA LENGTH/31,27,31,30,31,30,31,31,30,31,30,31/8
 Y
	CALL SYS$ASCTIM(,EXDATE,,)		! Get the present date
 
	DECODE(2,'(I2)',EXDATE(:2)) DAY	! Get day
	DECODE(4,'(I4)',EXDATE(8:11)) YEAR	! Get year
 l
	MONTH = 1
	DO WHILE (MONTHS(MONTH).NE.EXDATE(4:6))	! Get month
	   MONTH = MONTH + 1 
	END DO.
 0
	IF (MOD(YEAR,4).EQ.0) THEN		! Correct February length
	   LENGTH(2) = 28			! if we're in a leap year
	ELSED
	   LENGTH(2) = 27
	END IFL
 N
	NUM_DAYS = NDAYS	! Put number of days into buffer variableF
 T
	DO WHILE (NUM_DAYS.GT.0)l
	   IF (NUM_DAYS+DAY.GT.LENGTH(MONTH)) THEN 
				! If expiration date exceeds end of monthm
	      NUM_DAYS = NUM_DAYS - (LENGTH(MONTH) - DAY + 1)
				! Decrement # of days by days left in month:
	      DAY = 1				! Reset day to first of monthl
	      MONTH = MONTH + 1			! Increment month pointer
	      IF (MONTH.EQ.13) THEN		! Moved into next year?h
		 MONTH = 1			! Reset month pointer
		 YEAR = YEAR + 1		! Increment year pointer
	         IF (MOD(YEAR,4).EQ.0) THEN	! Correct February length
	            LENGTH(2) = 28		! if we're in a leap year
	         ELSE
	            LENGTH(2) = 27
	         END IF
	      END IFT
	   ELSE			! If expiration date is within the month 
	      DAY = DAY + NUM_DAYS		! Find expiration day
	      NUM_DAYS = 0			! Force loop exitO
	   END IF
	END DON
  
	ENCODE(2,'(I2)',EXDATE(:2)) DAY	! Put day into new date
	ENCODE(4,'(I4)',EXDATE(8:11)) YEAR	! Put year into new date
	EXDATE(4:6) = MONTHS(MONTH)		! Put month into new date)
 )
	RETURNI
	END
 T
 A
 I
	SUBROUTINE GET_LINE(INPUT,LEN_INPUT)I
CT
C  SUBROUTINE GET_LINE
C!
C  FUNCTION:
C	Gets line of input from terminal.C
CF
C  OUTPUTS:i
C	LEN_INPUT  -  Length of input line.  If = -1, CTRLC entered.
C		      if = -2, CTRLZ entered.
CE
C  NOTES:N
C	Also, on first call, set LEN_INPUT to 1+LENGTH OF INPUT CHARCTER
C	for initializing the CTRLC AST..
CT
 
	IMPLICIT INTEGER (A-Z) 
  
	LOGICAL*1 DESCRIP(8),DTYPE,CLASS
	INTEGER*2 LENGTH 
	CHARACTER*(*) INPUT
	EQUIVALENCE (DESCRIP(1),LENGTH),(DESCRIP(3),DTYPE)O
	EQUIVALENCE (DESCRIP(4),CLASS),(DESCRIP(5),POINTER)
 1
	EXTERNAL SMG$_EOF
 G
	COMMON /DECNET/ DECNET_PROC,ERROR_UNIT 
	LOGICAL DECNET_PROC
 I
	COMMON /SMG/ KEYBOARD_ID,KEY_TABLE_ID
 U
	COMMON /CTRLC_FLAG/ FLAGE
 
	CHARACTER PROMPT*(*),NULLPROMPT*1
	LOGICAL*1 USE_PROMPT
 P
	USE_PROMPT = .FALSE.l
 P
	GO TO 5
 L
	ENTRY GET_INPUT_PROMPT(INPUT,LEN_INPUT,PROMPT)n
  
	USE_PROMPT = .TRUE.
  
5	LIMIT = LEN(INPUT)			! Get input line size limit
	INPUT = ' '				! Clean out input buffer
 T
CT
C  Initialize CTRL-C AST with AST routine CTRLC_ROUTINE andT
C  AST parameter FLAG.  When CTRLC occurs, FLAG is set to 1C
C 
 T
	CALL DECLARE_CTRLC_AST(
 I
	LEN_INPUT = 0				! Nothing inputted yet
  
	LENGTH = 0				! Init special variable
	DTYPE = 0				! descriptor so we won't
	CLASS = 2				! run into any memory limitT
	POINTER = 0				! during input.-
 N
C1
C  LIB$GET_INPUT is nice way of getting input from terminal,
C  as it handles such thing as accidental wrap around to next line.:
CN
 
	IF (DECNET_PROC) THEN
	   READ (5,'(Q,A)',IOSTAT=IER) LEN_INPUT,INPUTL
	   IF (IER.NE.0) LEN_INPUT = -2 E
	   RETURN
	ELSE IF (USE_PROMPT) THEN
	   IER = SMG$READ_COMPOSED_LINE(KEYBOARD_ID,KEY_TABLE_ID,
     &		DESCRIP,PROMPT)		! Get line from terminal with prompt
	ELSEP
	   IER = SMG$READ_COMPOSED_LINE(KEYBOARD_ID,KEY_TABLE_ID,
     &		DESCRIP,NULLPROMPT)	! Get line from terminal with no prompt
	END IF 
 M
	IF (.NOT.IER.AND.IER.NE.%LOC(SMG$_EOF)) CALL EXIT(IER)1
 O
	CALL STR$TRIM(DESCRIP,DESCRIP,LEN_INPUT)
 
	IF (FLAG.EQ.0) THEN			! If no CTRL-C has occurred
	   CALL CANCEL_CTRLC_AST		! Cancel CTRL-C AST
	   IF (IER.NE.%LOC(SMG$_EOF)) THEN	! End of input?T
	      LEN_INPUT = MIN(LIMIT,LENGTH)	! No. Get length of line1
	      DO I=0,LEN_INPUT-1		! Extract from descriptor
	         CALL GET_VAL(INPUT(I+1:I+1),%VAL(POINTER+I))
	      END DO1
	      CALL CONVERT_TABS(INPUT,LEN_INPUT)G
	      LEN_INPUT = MAX(LEN_INPUT,LENGTH)
	   ELSE
	      LEN_INPUT = -2			! If CTRL-Z, say soS
	   END IF
	ELSE 
	   LEN_INPUT = -1			! If CTRL-C, say so
	END IF 
	RETURN 
	END
 E
 A
 C
	SUBROUTINE CONVERT_TABS(INPUT,LEN_INPUT)D
 H
	IMPLICIT INTEGER (A-Z) 
  
	CHARACTER*(*) INPUT
 E
	PARAMETER TAB = CHAR(9)
  
	LIMIT = LEN(INPUT)N
 Q
	DO WHILE (INDEX(INPUT,TAB).GT.0.AND.LEN_INPUT.LT.LIMIT)
	   TAB_POINT = INDEX(INPUT,TAB)	! Remove tabs
	   MOVE = ((TAB_POINT-1)/8)*8 + 9
	   ADD = MOVE - TAB_POINT
	   IF (MOVE-1.LE.LIMIT) THEN 
	      INPUT(MOVE:) = INPUT(TAB_POINT+1:)I
	      DO I = TAB_POINT,MOVE-1
	         INPUT(I:I) = ' '
	      END DO0
	      LEN_INPUT = LEN_INPUT + ADD - 1
	   ELSE
	      DO I = TAB_POINT,LIMITE
	         INPUT(I:I) = ' '
	      END DO_
	      LEN_INPUT = LIMIT+1
	   END IF
	END DO 
  
        CALL FILTER (INPUT, LEN_INPUT)
  
	RETURNT
	END
  
 E
	SUBROUTINE FILTER (INCHAR, LENGTH)L
 N
	IMPLICIT INTEGER (A-Z)E
 r
	CHARACTER*(*) INCHAR
 
	DO I = 1,LENGTH
	   IF ((INCHAR(I:I).LT.' '.AND.
     &      INCHAR(I:I).NE.CHAR(13).AND.INCHAR(I:I).NE.CHAR(10))
     &	    .OR.INCHAR(I:I).GT.'~') INCHAR(I:I) = '.'
	END DO
 
	RETURNN
	END
 O
 M
	SUBROUTINE GET_VAL(OUTPUT,INPUT)	! Used to convert logicalN
	CHARACTER*(*) OUTPUT			! byte to character valueN
	LOGICAL*1 INPUT
	OUTPUT = CHAR(INPUT)
	RETURNA
	END
 I
	SUBROUTINE CTRLC_ROUTINE		! CTRL-C AST routineT
	IMPLICIT INTEGER (A-Z)			! If CTRL-C, come here
 
	COMMON /CTRLC_FLAG/ FLAGI
 S
	IF (FLAG.EQ.2) THEN
	   CALL LIB$PUT_OUTPUT('Bulletin aborting...') 
	   CALL SYS$CANEXH()o
	   CALL EXIT	
	END IFi
	FLAG = 1				! to set flag
	RETURN(
	END
 ,
 U
 
	SUBROUTINE DECLARE_CTRLC_ASTn
Ch
C  SUBROUTINE DECLARE_CTRLC_ASTE
C 
C  FUNCTION:
C	Declares a CTRLC ast. 
C  NOTES:A
C	Assumes terminal assigned to TERM_CHAN in common /TERM_CHAN/.T
C 
	IMPLICIT INTEGER (A-Z) 
 L
	EXTERNAL IO$_SETMODE,IO$M_CTRLCAST,CTRLC_ROUTINEE
	COMMON /TERM_CHAN/ TERM_CHAN 
  
	COMMON /CTRLC_FLAG/ FLAGE
 E
	FLAG = 0				! Init CTRL-C flag 
	IO_CTRLC = %LOC(IO$_SETMODE)+%LOC(IO$M_CTRLCAST)	! Set AST code
	IER=SYS$QIOW(,%VAL(TERM_CHAN),%VAL(IO_CTRLC),,,,	! for QIO 
     &	      CTRLC_ROUTINE,,,,,)		! Enable the AST
 
	RETURNT
  
	ENTRY CANCEL_CTRLC_ASTT
 :
	IER = SYS$CANCEL(%VAL(TERM_CHAN))
  
	FLAG = 2		! Indicates that a CTRLC will cause an exit
	IER=SYS$QIOW(,%VAL(TERM_CHAN),%VAL(IO_CTRLC),,,,	! for QION
     &	      CTRLC_ROUTINE,,,,,)		! Enable the AST
 N
	RETURN
	END
 L
 .
 T
 V
	SUBROUTINE GET_INPUT_NOECHO(DATA)
C 
C  SUBROUTINE GET_INPUT_NOECHO
CE
C  FUNCTION: Reads data in from terminal without echoing characters.
C	     Also contains entry to assign terminal.
CC
	IMPLICIT INTEGER (A-Z)E
 '
	CHARACTER*(*) DATA,PROMPT
 N
	COMMON /TERM_CHAN/ TERM_CHANR
 L
	COMMON /SMG/ KEYBOARD_ID,KEY_TABLE_ID
 R
	COMMON /CTRLC_FLAG/ FLAGL
 
	INCLUDE '($TRMDEF)'
 A
	INTEGER TERMSET(2))
  
	INTEGER MASK(4)
	DATA MASK/4*'FFFFFFFF'X/e
 r
	DATA PURGE/.TRUE./1
 )
	DO I=1,LEN(DATA) 
	   DATA(I:I) = ' 'M
	END DON
 8
	IF (PURGE) THEN
	   CALL SMG$READ_STRING(KEYBOARD_ID,DATA,,LEN(DATA),H
     &		TRM$M_TM_NOECHO.OR.TRM$M_TM_PURGE)
	   PURGE = .FALSE. 
	ELSEL
	   CALL SMG$READ_STRING(KEYBOARD_ID,DATA,,LEN(DATA),.
     &		TRM$M_TM_NOECHO)
	END IF(
 )
	RETURNL
  
	ENTRY GET_INPUT_NOECHO_PROMPT(DATA,PROMPT)E
  
	DO I=1,LEN(DATA)
	   DATA(I:I) = ' '	
	END DOi
 o
	IF (PURGE) THEN
	   CALL SMG$READ_STRING(KEYBOARD_ID,DATA,PROMPT,LEN(DATA),U
     &		TRM$M_TM_NOECHO.OR.TRM$M_TM_PURGE)
	   PURGE = .FALSE.R
	ELSE
	   CALL SMG$READ_STRING(KEYBOARD_ID,DATA,PROMPT,LEN(DATA),'
     &		TRM$M_TM_NOECHO)
	END IFD
 S
	RETURN
 
	ENTRY GET_INPUT_NUM(DATA,NLEN)Y
 
	DO I=1,LEN(DATA)E
	   DATA(I:I) = ' 'T
	END DOp
 s
	IF (PURGE) THEN
	   CALL SMG$READ_STRING(KEYBOARD_ID,DATA,,LEN(DATA), 
     &		TRM$M_TM_PURGE,,TERMSET,NLEN,TERM)
	   PURGE = .FALSE.D
	ELSEN
	   CALL SMG$READ_STRING(KEYBOARD_ID,DATA,,LEN(DATA),,,'
     &		TERMSET,NLEN,TERM)
	END IF 
 T
	IF (TERM.NE.13.AND.TERM.NE.510.AND.NLEN.EQ.0) THEN3
				! Input did not end with CR or buffer full
	   NLEN = 1
	   DATA(:1) = CHAR(TERM)2
	END IF(
 )
	RETURNt
 y
	ENTRY ASSIGN_TERMINAL
 :
	IER = SYS$ASSIGN('TT',TERM_CHAN,,)	! Assign terminal(
 T
	CALL DECLARE_CTRLC_ASTm
 h
	FLAG = 2		! Indicates that a CTRLC will cause an exit
 .
	IER = SMG$CREATE_VIRTUAL_KEYBOARD(KEYBOARD_ID,,,,20)	
 i
	IER = SMG$SET_KEYPAD_MODE(KEYBOARD_ID,0)2
  
	IER = SMG$CREATE_KEY_TABLE (KEY_TABLE_ID)
 r
	TERMSET(1) = 16
	TERMSET(2) = %LOC(MASK)
 N
	DO I=ICHAR('0'),ICHAR('9')A
	   MASK(2) = IBCLR(MASK(2),I-32)	
	END DOr
 o
	RETURNc
	END
 o
 o
 m
  
  
	SUBROUTINE GETPAGLEN(PAGE_LENGTH)
C 
C  SUBROUTINE GETPAGLEN 
Cf
C  FUNCTION:
C	Gets page length of the terminal.e
C 
C  OUTPUTS:o
C	PAGE_LENGTH  -  Page length of the terminal.
Cn
	IMPLICIT INTEGER (A-Z)N
 E
	INCLUDE '($DVIDEF)'
 n
	LOGICAL*1 DEVDEPEND(4)	
 R
	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(4,DVI$_DEVDEPEND,%LOC(DEVDEPEND(1)))H
	CALL END_ITMLST(GETDVI_ITMLST)		! Get address of itemlist
  
	CALL SYS$GETDVIW(,,'TT',%VAL(GETDVI_ITMLST),,,,)L
 T
	PAGE_LENGTH = DEVDEPEND(4)
  
	RETURNT
	END
 E
 !
  
 i
 i
	LOGICAL FUNCTION SLOW_TERMINAL 
CD
C  FUNCTION SLOW_TERMINAL 
Ci
C  FUNCTION:
C	Indicates that terminal has a slow speed (2400 baud or less).
CC
C  OUTPUTS:E
C	SLOW_TERMINAL = .true. if slow, .false. if not.(
C'
 D
	IMPLICIT INTEGER (A-Z)r
 t
	EXTERNAL IO$_SENSEMODE=
 N
	COMMON /TERM_CHAN/ TERM_CHANw
 t
	COMMON CHAR_BUF(2)D
 T
	LOGICAL*1 IOSB(8)
 E
	INCLUDE '($TTDEF)'T
 
	IER = SYS$QIOW(,%VAL(TERM_CHAN),IO$_SENSEMODE,IOSB,,,
     &		  CHAR_BUF,%VAL(8),,,,) 
 T
	IF (IOSB(3).LE.TT$C_BAUD_2400) THEN
	   SLOW_TERMINAL = .TRUE.
	ELSE	
	   SLOW_TERMINAL = .FALSE..
	END IFN
 S
	RETURNo
	END
 t
 l
 s
 L
	SUBROUTINE SHOW_PRIV 
CU
C  SUBROUTINE SHOW_PRIVi
Cg
C  FUNCTION:
C	To show privileges necessary for managing bulletin board.8
CT
 ,
	IMPLICIT INTEGER (A-Z) 
 C
	INCLUDE 'BULLUSER.INC'V
 N
	INCLUDE '($PRVDEF)'
 E
	COMMON /PRVDEF/ PRIVS
	CHARACTER*8 PRIVS(0:38)
 S
	CALL OPEN_FILE_SHARED(4)		! Get BULLUSER.DAT file
 D
	CALL READ_USER_FILE_HEADER(IER)
 C
	IF (IER.EQ.0) THEN			! If header is present, exit
	   IF (NEW_FLAG(1).EQ.-1.AND.NEW_FLAG(2).EQ.-1) THEN  ! Info not presentT
	      CALL CLOSE_FILE(4)
	      CALL OPEN_FILE(4)			! Get BULLUSER.DAT file
	      CALL READ_USER_FILE_HEADER(IER)
	      USERPRIV(1) = PRV$M_OPER.OR.PRV$M_CMKRNL.OR.PRV$M_SETPRVp
	      USERPRIV(2) = 0
	      REWRITE (4) USER_HEADER
	   END IF
	   WRITE (6,'('' Following privileges are needed for privileged
     & commands:'')')h
	   DO I=0,38,
	      IF ((I.LT.32.AND.BTEST(USERPRIV(1),I)).OR.(
     &		  (I.GT.31.AND.BTEST(USERPRIV(2),I-32))) THENN
		 WRITE (6,'(1X,A)') PRIVS(I)
	      END IF	
	   END DO
	ELSEe
	   WRITE (6,'('' ERROR: Cannot show privileges.'')')I
	END IF	
  
	CALL CLOSE_FILE(4)			! All finished with BULLUSER
  
	RETURNn
  
	END
 i
 ,
  
  
	SUBROUTINE SET_PRIV
Ca
C  SUBROUTINE SET_PRIV
Cx
C  FUNCTION:
C	To set privileges necessary for managing bulletin board.
C_
 U
	IMPLICIT INTEGER (A-Z)0
 E
	INCLUDE '($PRVDEF)'
 N
	INCLUDE 'BULLUSER.INC'H
 
	COMMON /PRIVILEGES/ PROCPRIV(2),NEEDPRIV(2)
 T
	COMMON /PRVDEF/ PRIVS
	CHARACTER*8 PRIVS(0:38)
	DATA PRIVSh
     &	/'CMKRNL','CMEXEC','SYSNAM','GRPNAM','ALLSPOOL','DETACH',
     &  'DIAGNOSE','LOG_IO','GROUP','ACNT','PRMCEB','PRMMBX','PSWAPM',
     &	'ALTPRI','SETPRV','TMPMBX','WORLD','MOUNT','OPER','EXQUOTA', 
     &	'NETMBX','VOLPRO','PHY_IO','BUGCHK','PRMGBL','SYSGBL','PFNMAP',
     &	'SHMEM','SYSPRV','BYPASS','SYSLCK','SHARE','UPGRADE','DOWNGRADE',
     &	'GRPPRV','READALL',' ',' ','SECURITY'/)
 E
	EXTERNAL CLI$_ABSENT 
 L
	DIMENSION ONPRIV(2),OFFPRIV(2).
 t
	CHARACTER*8 INPUT_PRIVD
 =
	IF (.NOT.SETPRV_PRIV().OR..NOT.BTEST(PROCPRIV(1),PRV$V_SETPRV)) THENI
	   WRITE (6,'('' ERROR: This command requires SETPRV privileges.'')')
	   RETURN
	END IFL
 I
	OFFPRIV(1) = 0P
	OFFPRIV(2) = 0L
	ONPRIV(1) = 0
	ONPRIV(2) = 0
 R
	DO WHILE (CLI$GET_VALUE('PRIVILEGES',INPUT_PRIV,LEN)!
     &	    .NE.%LOC(CLI$_ABSENT))		! Get the specified nodes
	   PRIV_FOUND = -1I
	   I = 0P
	   DO WHILE (I.LT.39.AND.PRIV_FOUND.EQ.-1)A
	      IF (INPUT_PRIV(:LEN).EQ.PRIVS(I)) PRIV_FOUND = I=
	      IF (INPUT_PRIV(3:LEN).EQ.PRIVS(I)) PRIV_FOUND = I
	      I = I + 1
	   END DO
	   IF (PRIV_FOUND.EQ.-1) THEN
	      WRITE(6,'('' ERROR: Incorrectly specified privilege = '',
     &		A)') INPUT_PRIV(:LEN) 
	      RETURNM
	   ELSE IF (INPUT_PRIV(:2).EQ.'NO') THEN 
	      IF (INPUT_PRIV.EQ.'NOSETPRV') THEN'
	       WRITE(6,'('' ERROR: Cannot remove SETPRV privileges.'')')
	       RETURN
	      ELSE IF (PRIV_FOUND.LT.32) THEN
		 OFFPRIV(1) = IBSET(OFFPRIV(1),PRIV_FOUND)
	      ELSE
		 OFFPRIV(2) = IBSET(OFFPRIV(2),PRIV_FOUND-32)U
	      END IFT
	   ELSE
	      IF (PRIV_FOUND.LT.32) THENE
		 ONPRIV(1) = IBSET(ONPRIV(1),PRIV_FOUND)
	      ELSEI
		 ONPRIV(2) = IBSET(ONPRIV(2),PRIV_FOUND-32):
	      END IF
	   END IF
	END DOI
 E
	CALL OPEN_FILE(4)		! Get BULLUSER.DAT file&
  
	CALL READ_USER_FILE_HEADER(IER)
 =
	IF (IER.EQ.0) THEN			! If header is present, exit
	   USERPRIV(1) = USERPRIV(1).OR.ONPRIV(1)
	   USERPRIV(2) = USERPRIV(2).OR.ONPRIV(2)
	   USERPRIV(1) = USERPRIV(1).AND.(.NOT.OFFPRIV(1))P
	   USERPRIV(2) = USERPRIV(2).AND.(.NOT.OFFPRIV(2))!
	   REWRITE (4) USER_HEADERI
	   WRITE (6,'('' Privileges successfully modified.'')')
	ELSE/
	   WRITE (6,'('' ERROR: Cannot modify privileges.'')')(
	END IF 
 r
	CALL CLOSE_FILE(4)			! All finished with BULLUSER
 D
	RETURNA
  
	END
  
  
 g
 R
 R
 
	SUBROUTINE ADD_ACL(ID,ACCESS,IER)
CT
C  SUBROUTINE ADD_ACLT
C 
C  FUNCTION: Adds ACL to bulletin files.
Cr
C  PARAMETERS:
C	ID - Character string containing identifier to add to ACL.
C	ACCESS - Character string containing access controls to give to ID.D
C	IER - Return error from attempting to set ACL.
CE
C  NOTE: The ID must be in the RIGHTS data base.
C 
	IMPLICIT INTEGER (A-Z)
 _
	INCLUDE 'BULLFOLDER.INC'L
 I
	CHARACTER ACLENT*255,ID*(*),ACCESS*(*)O
 %
	INCLUDE '($ACLDEF)'
 R
	INCLUDE '($SSDEF)' 
 &
	IER = SYS$PARSE_ACL('(IDENTIFIER='//ID//',ACCESS='T
     &	   //ACCESS//')',ACLENT,,)
	IF (.NOT.IER) THEN(
	   IF (IER.EQ.SS$_NOSUCHID.AND.ADDID) THENt
	      CALL GET_UAF(ID,USER,GROUP,ACCOUNT,FLAGS,IER)
	      IF (.NOT.IER) THEN	
		 CALL ERRSNS(IDUMMY,IER)
		 WRITE (6,'(
     &		    '' ERROR: Specified username cannot be verified.'')') 
		 CALL SYS_GETMSG(IER) 
	         RETURN
	      END IF
	      IDENT = USER + ISHFT(GROUP,16)m
	      IER = SYS$ADD_IDENT(ID,%VAL(IDENT),,)
	      IF (IER) THEN
	         IER = SYS$PARSE_ACL('(IDENTIFIER='//ID//',ACCESS='
     &	           //ACCESS//')',ACLENT,,)C
	      END IFO
	   END IF
	END IFE
	IF (.NOT.IER) RETURN 
 R
	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(ICHAR(ACLENT(:1)),ACL$C_ADDACLENT,%LOC(ACLENT))
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist
  
	FLEN = TRIM(FOLDER1_FILE)
 8
	IER = SYS$CHANGE_ACL(,ACL$C_FILE,FOLDER1_FILE(:FLEN)//D
     &		'.BULLDIR',%VAL(ACL_ITMLST),,,).
	IF (.NOT.IER) RETURN 
	IER = SYS$CHANGE_ACL(,ACL$C_FILE,FOLDER1_FILE(:FLEN)//R
     &		'.BULLFIL',%VAL(ACL_ITMLST),,,)N
	IF (.NOT.IER) RETURNR
 R
	RETURNE
	END
 I
 T
 E
	SUBROUTINE DEL_ACL(ID,ACCESS,IER)
CE
C  SUBROUTINE DEL_ACL 
C 
C  FUNCTION: Adds ACL to bulletin files.
CS
C  PARAMETERS:
C	ID - Character string containing identifier to add to ACL.
C	ACCESS - Character string containing access controls to give to ID.G
C	IER - Return error from attempting to set ACL.
CM
C  NOTE: The ID must be in the RIGHTS data base.
CT
	IMPLICIT INTEGER (A-Z) 
 ,
	INCLUDE 'BULLFOLDER.INC' 
 T
	CHARACTER ACLENT*255,ID*(*),ACCESS*(*)L
 G
	INCLUDE '($ACLDEF)'
 D
	IF (ID.NE.' ') THEN
	   IER = SYS$PARSE_ACL('(IDENTIFIER='//ID//',ACCESS='
     &	      //ACCESS//')',ACLENT,,)
	   IF (.NOT.IER) RETURN
 ,
	   CALL INIT_ITMLST	! Initialize item list
	   CALL ADD_2_ITMLST(ICHAR(ACLENT(:1)),ACL$C_DELACLENT,%LOC(ACLENT))o
	   CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlistC
	ELSEM
	   CALL INIT_ITMLST	! Initialize item list_
	   CALL ADD_2_ITMLST(255,ACL$C_DELETEACL,%LOC(ACLENT))n
	   CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlistI
	END IFt
  
	FLEN = TRIM(FOLDER1_FILE)
 
	IER = SYS$CHANGE_ACL(,ACL$C_FILE,FOLDER1_FILE(:FLEN)//i
     &		'.BULLDIR',%VAL(ACL_ITMLST),,,)0
	IF (.NOT.IER) RETURNT
	IER = SYS$CHANGE_ACL(,ACL$C_FILE,FOLDER1_FILE(:FLEN)//T
     &		'.BULLFIL',%VAL(ACL_ITMLST),,,)H
	IF (.NOT.IER) RETURN 
 L
	RETURN,
	END
 E
 D
 
 
	SUBROUTINE CREATE_FOLDER
C
C  SUBROUTINE CREATE_FOLDERE
CN
C  FUNCTION: Creates a new bulletin folder.N
CO
 
	IMPLICIT INTEGER (A-Z) 
 m
	INCLUDE 'BULLFOLDER.INC'	
 E
	INCLUDE 'BULLUSER.INC'f
 e
	INCLUDE 'BULLFILES.INC'
 E
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
	DATA REMOTE_SET /.FALSE./
 N
	IF (.NOT.SETPRV_PRIV().AND.CLI$PRESENT('NEEDPRIV')) THEN_
	   WRITE(6,'('' ERROR: CREATE is a privileged command.'')')
	   RETURN
	END IFt
 i
	IER = CLI$GET_VALUE('CREATE_FOLDER',FOLDER,LEN_T) ! Get folder name
 G
	IF (LEN_T.GT.25) THEN
	   WRITE(6,'('' ERROR: Folder name must be < 26 characters.'')') 
	   RETURN
	END IFW
 R
	IF (.NOT.SETPRV_PRIV().AND.	! /NOTIFY /READNEW /BRIEF privilegedd
     &	    (CLI$PRESENT('NOTIFY').OR.CLI$PRESENT('READNEW').OR..
     &	     CLI$PRESENT('BRIEF').OR.CLI$PRESENT('SYSTEM'))) THEN
	   WRITE (6,'(S
     &   '' ERROR: No privs for SYSTEM, NOTIFY, BRIEF or READNEW.'')')
	   RETURN
	END IF
 
	IF (CLI$PRESENT('NODE')) THEN	! Remote node specified?)
	   IER = CLI$GET_VALUE('NODE',FOLDER_BBOARD,LEN_B) ! Get node nameS
	   FOLDER_BBOARD = '::'//FOLDER_BBOARD(:LEN_B) 
	   FOLDER1_BBOARD = FOLDER_BBOARD
	   FOLDER1 = FOLDER
	   CALL CONNECT_REMOTE_FOLDER(READ_ONLY,IER)H
	   IF (IER.NE.0) THEN
	    WRITE (6,'('' ERROR: Folder not accessible on remote node.'')')
	    RETURNt
	   ELSE IF (CLI$PRESENT('SYSTEM').AND.Z
     &				.NOT.BTEST(FOLDER1_FLAG,2)) THEN
	    WRITE (6,'('' ERROR: /SYSTEM not allowed as remote node'',3
     &			'' is not SYSTEM folder.'')')
	    RETURN 
	   END IF
	END IFU
 _
	WRITE (6,'('' Enter one line description of folder.'')')p
 e
10	CALL GET_LINE(FOLDER_DESCRIP,LENDES)	! Get input line
	FOLDER_DESCRIP = FOLDER_DESCRIP(:LENDES)	! End fill with spaces
	IF (LENDES.LE.0) THEN
	   WRITE (6,'('' Aborting folder creation.'')')
	   RETURN
	ELSE IF (LENDES.GT.80) THEN		! If too many characters
	   WRITE(6,'('' ERROR: folder must be < 80 characters.'')')
	   GO TO 10
	END IF6
 '
	CALL OPEN_FILE(7)		! Open folder file
	READ (7,IOSTAT=IER,KEY=FOLDER,KEYID=0)I
					! See if folder existsN
 T
	IF (IER.EQ.0) THENR
	   WRITE (6,'('' ERROR: Specified folder already exists.'')')
	   GO TO 1000
	END IF)
  
	FOLDER_OWNER = USERNAME			! Get present username 
 O
	FOLDER_SET = .TRUE.
 .
	CALL SYS$SETDFPROT('FF00'X,CUR_DEF_PROT)	
			! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD,GROUP)
 
C
C  Folder file is placed in the directory FOLDER_DIRECTORY.T
C  The file prefix is the name of the folder.g
Cl
 i
	FD_LEN = TRIM(FOLDER_DIRECTORY)
	IF (FD_LEN.EQ.0) THEN
	 WRITE (6,'('' ERROR: System programmer has disabled folders.'')')O
	 GO TO 910P
	ELSE
	 FOLDER_FILE = FOLDER_DIRECTORY(:FD_LEN)//FOLDER)
	END IFR
 h
	OPEN (UNIT=2,FILE=FOLDER_FILE(:TRIM(FOLDER_FILE))
     1	 //'.BULLDIR',STATUS='NEW',
     1	 RECORDTYPE='FIXED',RECORDSIZE=115,ACCESS='DIRECT',IOSTAT=IER,T
     1	 ORGANIZATION='RELATIVE',DISPOSE='KEEP',FORM='FORMATTED')
 L
	IF (IER.NE.0) THEN'
	   WRITE(6,'('' ERROR: Cannot create folder directory file.'')')L
	   CALL ERRSNS(IDUMMY,IER)A
	   CALL SYS_GETMSG(IER)
	   GO TO 910S
	END IF)
 E
	OPEN (UNIT=1,FILE=FOLDER_FILE(:TRIM(FOLDER_FILE))
     1	 //'.BULLFIL',STATUS='NEW',
     1	 ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=32,,
     1	 FORM='UNFORMATTED',IOSTAT=IER)
 O
	IF (IER.NE.0) THENr
	   WRITE(6,'('' ERROR: Cannot create folder message file.'')') 
	   CALL ERRSNS(IDUMMY,IER)I
	   CALL SYS_GETMSG(IER)
	   GO TO 910C
	END IFL
 '
	FOLDER_FLAG = 0
 V
	IF (CLI$PRESENT('PRIVATE').OR.CLI$PRESENT('SEMIPRIVATE')) THENd
				! Will folder have access limitations?
	   FOLDER1_FILE = FOLDER_FILE
	   CLOSE (UNIT=1)
	   CLOSE (UNIT=2)
	   IF (CLI$PRESENT('SEMIPRIVATE')) THEN
	      CALL ADD_ACL('*','R',IER)
	   ELSE
	      CALL ADD_ACL('*','NONE',IER)R
	   END IF
	   CALL ADD_ACL(FOLDER_OWNER,'R+W+C',IER)
	   OPEN (UNIT=2,FILE=FOLDER_FILE(:TRIM(FOLDER_FILE))(
     1	    //'.BULLDIR',STATUS='OLD',IOSTAT=IER1).
	   OPEN (UNIT=1,FILE=FOLDER_FILE(:TRIM(FOLDER_FILE))E
     1	    //'.BULLFIL',STATUS='OLD',IOSTAT=IER1)V
	   IF (.NOT.IER) THEN
	      WRITE(6, 
     &	      '('' ERROR: Cannot create private folder using ACLs.'')')
	      CALL SYS_GETMSG(IER)R
	      GO TO 910
	   END IF
	   FOLDER_FLAG = IBSET(FOLDER_FLAG,0)
	END IFP
 _
	IER = 0
	LAST_NUMBER = 1
	DO WHILE (IER.EQ.0.AND.LAST_NUMBER.LT.FOLDER_MAX-1)
	   READ (7,IOSTAT=IER,KEY=LAST_NUMBER,KEYID=1) 
	   LAST_NUMBER = LAST_NUMBER + 1
	END DON
 L
	IF (IER.EQ.0) THEND
	 WRITE (6,'('' ERROR: Folder limit of '',I,'' has been reached.'')')	
     &			FOLDER_MAXe
	 WRITE (6,'('' Unable to add specified folder.'')')
	 GO TO 9102
	ELSER
	   FOLDER1_NUMBER = LAST_NUMBER - 1
	END IFV
 .
	IF (.NOT.CLI$PRESENT('NODE')) THEN2
	   FOLDER_BBOARD = 'NONE'
	   IF (REMOTE_SET) CLOSE (UNIT=REMOTE_UNIT)
	   REMOTE_SET = .FALSE.
	   FOLDER_BBEXPIRE = 14
	   F_NBULL = 0T
	   NBULL = 0:
	   F_NEWEST_BTIM(1) = 0
	   F_NEWEST_BTIM(2) = 0
	   F_EXPIRE_LIMIT = 0
	   FOLDER_NUMBER = FOLDER1_NUMBER
	ELSEE
	   REMOTE_SET = .TRUE.
	   CLOSE (UNIT=1,STATUS='DELETE')
	   CLOSE (UNIT=2,STATUS='DELETE')
	   FOLDER1_FLAG = FOLDER_FLAG
	   FOLDER1_DESCRIP = FOLDER_DESCRIP
	   FOLDER_COM = FOLDER1_COM
	   NBULL = F_NBULL
	END IF-
	FOLDER_OWNER = USERNAME			! Get present usernamee
  
	IF (CLI$PRESENT('SYSTEM')) THEN
	   FOLDER_FLAG = IBSET(FOLDER_FLAG,2)
	END IF 
 t
	CALL WRITE_FOLDER_FILE(IER)
	CALL MODIFY_SYSTEM_LIST
 C
	CLOSE (UNIT=1)I
	CLOSE (UNIT=2)T
 A
	NOTIFY = 0(
	READNEW = 0
	BRIEF = 0
	IF (CLI$PRESENT('NOTIFY')) NOTIFY = 1
	IF (CLI$PRESENT('READNEW')) READNEW = 1
	IF (CLI$PRESENT('SHOWNEW')) BRIEF = 1
	IF (CLI$PRESENT('BRIEF')) THEN(
	   BRIEF = 1Q
	   READNEW = 1.
	END IFE
	CALL SET_FOLDER_DEFAULT(NOTIFY,READNEW,BRIEF)
 ,
	WRITE (6,'('' Folder is now set to '',A)')S
     &		FOLDER(:TRIM(FOLDER))//'.'
 	
	GO TO 1000 
 c
910	WRITE (6,'('' Aborting folder creation.'')')
	IF (FOLDER_NUMBER.EQ.0) FOLDER_SET = .FALSE.I
	CLOSE (UNIT=1,STATUS='DELETE')R
	CLOSE (UNIT=2,STATUS='DELETE')E
 I
1000	CALL CLOSE_FILE(7)I
	CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection/
 C
	RETURN 
 &
	END
  
 /
..
 /
