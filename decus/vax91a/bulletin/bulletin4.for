C
C  BULLETIN4.FOR, Version 4/28/91
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
C  FUNCTION: Removes entry in user file of user that no longer exist.
C		It creates empty space for new user.
C
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLUSER.INC'
 
	CHARACTER*12 LOGIN_USER
 
	CHARACTER TODAY*23
 
	DIMENSION TODAY_BTIM(2)
 
	CALL SYS$ASCTIM(,TODAY,,)		! Get the present time
	CALL SYS_BINTIM(TODAY,TODAY_BTIM)
 
	CALL OPEN_SYSUAF_SHARED
 
	LOGIN_USER = USERNAME
	READ (4,IOSTAT=IER1,KEYGT=USERNAME) USER_ENTRY	! Look forward one
	TEMP_USER = USERNAME
	USERNAME = LOGIN_USER
	DO WHILE (REC_LOCK(IER))
	   READ (8,KEY=TEMP_USER,IOSTAT=IER) TEMP_USER	! See if user exists
	END DO
 
	IF (IER.NE.0.AND.IER1.EQ.0.AND.TEMP_USER.NE.USER_HEADER_KEY) THEN
				! If no UAF entry and last login was
				! more than 6 months old, delete entry
	   IF (MINUTE_DIFF(TODAY_BTIM,LOGIN_BTIM).GT.6*30*24*60) THEN
	      DELETE(UNIT=4)			! Delete non-existant user
	      CALL OPEN_BULLINF
	      READ (9,KEY=TEMP_USER,IOSTAT=IER)
	      IF (IER.EQ.0) DELETE(UNIT=9)
	      LU = TRIM(TEMP_USER)
	      TEMP_USER(LU:LU) = CHAR(ICHAR(TEMP_USER(LU:LU)).OR.128)
	      READ (9,KEY=TEMP_USER,IOSTAT=IER)
	      IF (IER.EQ.0) DELETE(UNIT=9)
	      IF (LU.GT.1) THEN
	         TEMP_USER(LU-1:LU-1) = 
     &			CHAR(128.OR.ICHAR(TEMP_USER(LU-1:LU-1)))
	      ELSE
		 TEMP_USER(2:2) = CHAR(128.OR.ICHAR(TEMP_USER(2:2)))
	      END IF
	      READ (9,KEY=TEMP_USER,IOSTAT=IER)
	      IF (IER.EQ.0) DELETE(UNIT=9)
	      CALL CLOSE_BULLINF
	   END IF
	END IF
 
	CALL CLOSE_SYSUAF			! All done...
 
	RETURN
	END
 
 
	SUBROUTINE TOTAL_CLEANUP_LOGIN
C
C  SUBROUTINE TOTAL_CLEANUP_LOGIN
C
C  FUNCTION: Removes all entries in user file of usesr that no longer exist
C
	IMPLICIT INTEGER (A-Z)
 
	INCLUDE 'BULLUSER.INC'
 
	COMMON /BULL_NOTIFY/ NOTIFY_REMOTE(FLONG)
 
	CHARACTER TODAY*23
 
	DIMENSION TODAY_BTIM(2)
 
	CALL SYS$ASCTIM(,TODAY,,)		! Get the present time
	CALL SYS_BINTIM(TODAY,TODAY_BTIM)
 
	CALL OPEN_SYSUAF_SHARED
	CALL OPEN_BULLUSER
	CALL OPEN_BULLINF
 
	TEMP_USER = USERNAME
 
	IER = 0
 
	DO WHILE (IER.EQ.0)			! Clean out BULLUSER.DAT
	   READ (4,IOSTAT=IER) USER_ENTRY
	   IF (IER.EQ.0.AND.USERNAME(:1).NE.'*'.AND.
     &	       USERNAME(:1).NE.':'.AND.
     &	       USERNAME.NE.USER_HEADER_KEY) THEN	! See if user exists
	      DO WHILE (REC_LOCK(IER))
	         READ (8,KEY=USERNAME,IOSTAT=IER)
	      END DO
	      IF (IER.NE.0) THEN 	! If no UAF entry and last login was
					! more than 6 months old, delete entry
		 IF (MINUTE_DIFF(TODAY_BTIM,LOGIN_BTIM).GT.6*30*24*60) THEN
	            DELETE (UNIT=4)
		    READ (9,KEY=USERNAME,IOSTAT=IER)
		    IF (IER.EQ.0) DELETE (UNIT=9)
	            LU = TRIM(USERNAME)
	            USERNAME(LU:LU) = CHAR(ICHAR(USERNAME(LU:LU)).OR.128)
		    READ (9,KEY=USERNAME,IOSTAT=IER)
		    IF (IER.EQ.0) DELETE (UNIT=9)
		    IF (LU.GT.1) THEN
		       USERNAME(LU-1:LU-1) = 
     &			 CHAR(128.OR.ICHAR(USERNAME(LU-1:LU-1)))
		    ELSE
		       USERNAME(2:2) = CHAR(128.OR.ICHAR(USERNAME(2:2)))
		    END IF
		    READ (9,KEY=USERNAME,IOSTAT=IER)
		    IF (IER.EQ.0) DELETE (UNIT=9)
		 END IF
		 IER = 0
	      ELSE
		 DO I=0,FOLDER_MAX-1
		    IF (TEST2(NOTIFY_FLAG,I)) THEN
		       CALL SET2(NOTIFY_REMOTE,I)
		    END IF
		 END DO
	      END IF
	   END IF
	END DO
 
	CALL CLOSE_SYSUAF			! All done...
 
	DO WHILE (REC_LOCK(IER))
	   READ (4,KEY='*NOTIFY',IOSTAT=IER) TEMP_USER
	END DO
 
	IF (IER.NE.0) THEN
	   WRITE (4,IOSTAT=IER) '*NOTIFY     ',NOTIFY_REMOTE
	ELSE
	   REWRITE (4,IOSTAT=IER) '*NOTIFY     ',NOTIFY_REMOTE
	END IF
 
	READ (9,KEYGT='            ',IOSTAT=IER) USERNAME
 
	DO WHILE (IER.EQ.0)			! Clean out BULLINF.DAT
	   LU = TRIM(USERNAME)
	   USERNAME(LU:LU) = CHAR(ICHAR(USERNAME(LU:LU)).AND.127)
	   IF (LU.GT.1) THEN
	      USERNAME(LU-1:LU-1) = 
     &			 CHAR(127.AND.ICHAR(USERNAME(LU-1:LU-1)))
	   ELSE
	      USERNAME(2:2) = CHAR(127.AND.ICHAR(USERNAME(2:2)))n
	   END IF
	   READ (4,KEYEQ=USERNAME,IOSTAT=IER)
	   IF (IER.NE.0) DELETE (UNIT=9) 
	   READ (9,IOSTAT=IER) USERNAME
	END DOr
 .
	CALL CLOSE_BULLINFr
	CALL CLOSE_BULLUSER
 t
	USERNAME = TEMP_USERi
 e
	RETURNo
	END
 b
 g
	SUBROUTINE COPY_BULL(INLUN,IBLOCK,OBLOCK,IER)
Ci
C  SUBROUTINE COPY_BULL 
C-
C  FUNCTION: To copy data to the bulletin file. 
C 
C  INPUT:o
C	INLUN	-	Input logical unit numberS
C	IBLOCK	-	Input block number in input file to start ati
C	OBLOCK	-	Output block number in output file to start ati
Cc
C  OUTPUT:
C	IER	-	If error in writing to bulletin, IER will be <> 0.
C.
C  NOTES:  Input file is accessed using sequential access.  This is 
C	to allow files which have variable records to be read.  Ther
C       bulletin file is assumed to be opened on logical unit 1.
C
 	
	IMPLICIT INTEGER (A - Z)
 A
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 L
	COMMON /LAST_RECORD_WRITTEN/ OCOUNT
 E
	INCLUDE 'BULLDIR.INC'
 I
	IF (REMOTE_SET) THENG
	   CALL REMOTE_COPY_BULL(IER)
	   IF (IER.NE.0) CALL ERROR_AND_EXITL
	END IFT
 T
	DO I=1,IBLOCK-1
	   READ(INLUN,'(A)')
	END DOI
  
	OCOUNT = OBLOCK
	ICOUNT = IBLOCK
 D
	NBLANK = 0I
	LENGTH = 0_
	DO WHILE (1)
	   ILEN = 0
	   DO WHILE (ILEN.EQ.0)
	      READ(INLUN,'(Q,A)',END=100) ILEN,INPUTe
	      ILEN = MIN(ILEN,TRIM(INPUT),LINE_LENGTH)r
	      IF (ILEN.GT.1.AND.ICHAR(INPUT(ILEN:ILEN)).EQ.10) THEN
		 INPUT(ILEN-1:ILEN-1) = CHAR(32)	! Remove imbedded
		 INPUT(ILEN:ILEN) = CHAR(32)	! CR/LFs at end of file.S
		 ILEN = ILEN - 2
	      END IFd
	      IF (ILEN.GT.0) THEN
		 IF (ICOUNT.EQ.IBLOCK) THENS
		    IF (INPUT(:6).EQ.'From: ') THENe
		       INPUT(:4) = 'FROM'E
		    END IF
		 END IFS
		 ICOUNT = ICOUNT + 1
	      ELSE IF (ILEN.EQ.0.AND.ICOUNT.GT.IBLOCK) THEN
		 NBLANK = NBLANK + 1
	      END IF 
	   END DO
	   IF (NBLANK.GT.0) THENL
	      DO I=1,NBLANK
	         CALL STORE_BULL(1,' ',OCOUNT)Y
	      END DOU
	      LENGTH = LENGTH + NBLANK*2n
	      NBLANK = 0 
	   END IF
	   CALL STORE_BULL(ILEN,INPUT,OCOUNT)
	   LENGTH = LENGTH + ILEN + 1
	END DOf
 
100	LENGTH = (LENGTH+127)/128T
	IF (LENGTH.EQ.0) THEN
	   IER = 1L
	ELSE_
	   IER = 0%
	END IF_
 L
	CALL FLUSH_BULL(OCOUNT)
 
	RETURNt
	END
 i
 t
 i
	SUBROUTINE STORE_BULL(ILEN,INPUT,OCOUNT)L
 ,
	IMPLICIT INTEGER (A-Z)	
  
	PARAMETER BRECLEN=128
 N
	CHARACTER INPUT*(*),OUTPUT*256m
  
	DATA POINT/0/
 T
	IF (ILEN+POINT+1.GT.BRECLEN) THEN
	   IF (POINT.EQ.BRECLEN) THEN
	      CALL WRITE_BULL_FILE(OCOUNT,OUTPUT(:POINT))
	      OUTPUT = CHAR(ILEN)//INPUT8
	      POINT = ILEN + 1y
	   ELSE IF (POINT.EQ.BRECLEN-1) THENE
	      CALL WRITE_BULL_FILE(OCOUNT,OUTPUT(:POINT)//CHAR(ILEN))
	      OUTPUT = INPUTT
	      POINT = ILENU
	   ELSE
	      CALL WRITE_BULL_FILE(OCOUNT,OUTPUT(:POINT)//CHAR(ILEN)L
     &		//INPUT(:BRECLEN-1-POINT))
	      OUTPUT = INPUT(BRECLEN-POINT:)i
	      POINT = ILEN - (BRECLEN-1-POINT) 
	   END IF
	   OCOUNT = OCOUNT + 1
	   DO WHILE (POINT.GE.BRECLEN)D
	      CALL WRITE_BULL_FILE(OCOUNT,OUTPUT(:BRECLEN))
	      OCOUNT = OCOUNT + 1
	      OUTPUT = OUTPUT(BRECLEN+1:)
	      POINT = POINT - BRECLEN
	   END DO
	ELSE
	   OUTPUT(POINT+1:) = CHAR(ILEN)//INPUT(:ILEN)l
	   POINT = POINT + ILEN + 1
	END IFA
 )
	RETURNM
 )
	ENTRY FLUSH_BULL(OCOUNT)L
 P
	IF (POINT.LT.BRECLEN) OUTPUT(POINT+1:POINT+1) = CHAR(0)
	CALL WRITE_BULL_FILE(OCOUNT,OUTPUT(:BRECLEN))
	POINT = 0
 O
	RETURNV
 I
	END
 R
 +
	SUBROUTINE WRITE_BULL_FILE(OCOUNT,OUTPUT)
 n
	IMPLICIT INTEGER (A-Z)R
 
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
 P
	CHARACTER*(*) OUTPUTU
 R
	IF (REMOTE_SET) THEN)
	   CALL REMOTE_WRITE_BULL_FILE(OUTPUT)C
	ELSET
	   WRITE (1'OCOUNT) OUTPUT 
	END IF 
 L
	RETURN
	END
 E
 U
	SUBROUTINE GET_BULL_LINE(SBLOCK,BLENGTH,BUFFER,ILEN)
 
	IMPLICIT INTEGER (A-Z)M
 (
	INCLUDE 'BULLDIR.INC'
 F
	CHARACTER*(*) BUFFERM
 (
	COMMON /HEADER/ HEADERM
	LOGICAL HEADER /.TRUE./
 T
	COMMON /DATE/ DATE_LINE
	CHARACTER*(LINE_LENGTH) DATE_LINE
 N
	IF (ILEN.GT.LINE_LENGTH) THEN		! First read?I
	   CALL STRIP_HEADER(BUFFER,0,IER) 
	   STRIP = .NOT.HEADER 
	   IBLOCK = SBLOCK			! Initialize pointers.
	   BULL_HEADER = .TRUE.
	   CALL GET_BULL(IBLOCK,BUFFER,ILEN)I
	   IF (ILEN.LE.0) IBLOCK = IBLOCK + 1
	   MSG_SENT = .FALSE.
	ELSE					! Else set ILEN to zeroL
	   ILEN = 0				! to request next line
	END IFC
  
	IF (MSG_SENT) THENY
	   BUFFER = ' '
	   ILEN = 1
	   MSG_SENT = .FALSE.
	   RETURN
	END IFS
 =
	DO WHILE (1)N
	   DO WHILE (ILEN.EQ.0)			! Read until line created
	      CALL GET_BULL(IBLOCK,BUFFER,ILEN)
	      IF (ILEN.LE.0) IBLOCK = IBLOCK + 1    ! Need to read new record.u
	      IF (IBLOCK.GE.SBLOCK+BLENGTH) RETURN  ! No more records.E
	   END DO
 R
	   IF (STRIP) THENn
	      IF (BULL_HEADER) THEN
		 IF (BUFFER(:5).EQ.'From:'.OR.BUFFER(:5).EQ.'Subj:') THEND
		    RETURN
		 ELSE IF (BUFFER(:13).EQ.'Message sent:') THEN
		    MSG_SENT = .TRUE.t
		    RETURN
		 ELSEP
		    BULL_HEADER = .FALSE.Y
		 END IFI
	      END IF 
	      IF (DATE_LINE.NE.' ') DATE_LINE = ' '
	      CALL STRIP_HEADER(BUFFER,ILEN,STRIP)H
	      IF (DATE_LINE.NE.' ') THEN
		 BUFFER = DATE_LINE_
		 ILEN = TRIM(DATE_LINE) 
		 MSG_SENT = .TRUE.
		 RETURN 
	      END IFH
	      IF (STRIP.OR.(.NOT.STRIP.AND.TRIM(BUFFER).EQ.0)) ILEN = 0
	   ELSE
	      RETURN
	   END IF
	END DO_
 R
	RETURNH
 1
	ENTRY TEST_MORE_RECORDS(SBLOCK,BLENGTH,IREC) 
  
	IREC = (SBLOCK+BLENGTH-1) - IBLOCK 
  
	RETURNE
	END
 T
 N
	SUBROUTINE GET_BULL(IBLOCK,BUFFER,ILEN)
CF
C  SUBROUTINE GET_BULL
C_
C  FUNCTION:  Outputs line from folder file.
C
C  INPUT:N
C	IBLOCK	-	Input block number in input file to read from.I
C
C  OUTPUT:
C	BUFFER  -	Character string containing output line.
C	ILEN	-	Length of character string.  If 0, signifies that
C			new record needs to be read, -1 signifies error.
CL
C  NOTE:  Since message file is stored as a fixed length (128) record file,S
C	  but message lines are variable, message lines may span one orY
C	  more record.  This routine takes a record and outputs as manyP
C	  lines as it can from the record.  When no more lines can beL
C	  outputted, it returns ILEN=0 requesting the calling program to
C	  increment the record counter.N
C(
	IMPLICIT INTEGER (A-Z) 
  
	INCLUDE 'BULLDIR.INC'
 
	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD
 f
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
  
	COMMON /REMOTE_READ_MESSAGE/ SCRATCH_R1
	DATA SCRATCH_R1 /0/
 I
	PARAMETER BRECLEN=128
 e
	CHARACTER BUFFER*(*),TEMP*(BRECLEN), LEFT*(LINE_LENGTH)
 t
	DATA POINT /1/, LEFT_LEN /0/D
 B
	IF (ILEN.GT.LINE_LENGTH) THEN		! First read? 
	   POINT = 1				! Initialize pointers.R
	   LEFT_LEN = 0
	END IF(
 .
	IF (POINT.EQ.1) THEN			! Need to read new line?
	   IF (INCMD(:4).EQ.'MOVE'.OR.INCMD(:4).EQ.'COPY') THEN
	      DO WHILE (REC_LOCK(IER))		! Read from file=
	         READ (11'IBLOCK,IOSTAT=IER) TEMP
	      END DO.
	   ELSE IF (REMOTE_SET) THEN		! Remote folder?&
	      IF (IBLOCK.EQ.BLOCK) SCRATCH_R = SCRATCH_R1	! Read lines 
	      CALL READ_QUEUE(%VAL(SCRATCH_R),SCRATCH_R,TEMP)	! from queue 
	   ELSE					! Local folder=
	      DO WHILE (REC_LOCK(IER))		! Read from fileI
	         READ (1'IBLOCK,IOSTAT=IER) TEMPD
	      END DO 
	   END IF
	ELSE IF (POINT.EQ.BRECLEN+1) THEN	! Read all of line
	   ILEN = 0				! so indicate need to read
	   POINT = 1				! new line to calling routine.A
	   RETURN
	END IFH
  
	IF (IER.GT.0) THEN			! Error in reading file.
	   ILEN = -1				! ILEN = -1 signifies error
	   POINT = 1R
	   LEFT_LEN = 0
	   RETURN
	END IFY
 M
	IF (LEFT_LEN.GT.0) THEN			! Part of line is left from
	   ILEN = ICHAR(LEFT(:1))		! previous record read. 
	   IF (LEFT_LEN.LE.BRECLEN) THEN	! Rest of it is in next record.B
	      BUFFER = LEFT(2:ILEN-LEFT_LEN+1)//TEMP(:LEFT_LEN) ! Output line.E
	      POINT = LEFT_LEN + 1		! Update pointers. 
	      LEFT_LEN = 0-
	   ELSE					! Rest of line is longer than
	      LEFT(ILEN-LEFT_LEN+2:) = TEMP	! a record, so store record
	      LEFT_LEN = LEFT_LEN - BRECLEN	! and request another read.
	      ILEN = 0				! Request new record read.E
	   END IF
	ELSE					! Else nothing left over.L
	   ILEN = ICHAR(TEMP(POINT:POINT))	! Get line lengthP
	   IF (ILEN.GT.BRECLEN-POINT) THEN	! If it extends to next record
	      LEFT = TEMP(POINT:)		! Store it in leftover bufferO
	      LEFT_LEN = ILEN - (BRECLEN-POINT)	! Store leftover length
	      ILEN = 0				! Request new record read
	      POINT = 1				! Update record pointer.
	   ELSE IF (ILEN.EQ.0) THEN		! Empty line signifies
	      POINT = 1				! end of message.t
	   ELSE					! Else message line fully readT
	      BUFFER = TEMP(POINT+1:POINT+ILEN)	! So output its
	      POINT = POINT+ILEN+1		! and update pointer.
	   END IF
	END IF 
  
	RETURNf
  
	ENTRY TEST_MORE_LINES(ILEN)	! Test for more lines in record.N
					! Returns length of next line.O
	IF (POINT.EQ.BRECLEN+1) THEN		! If pointer greater than
	   ILEN = 0				! record, no more lines.
	ELSE					! Else there is another line.T
	   ILEN = ICHAR(TEMP(POINT:POINT))	! Output it's length.E
	END IFT
 D
	RETURNO
 1
	END
 D
 L
 '
 '
 
 D
 I
	SUBROUTINE DELETE_ENTRY(BULL_ENTRY)
C
C  SUBROUTINE DELETE_ENTRY
C
C  FUNCTION:
C	To delete a directory entry.
CE
C  INPUTS:
C	BULL_ENTRY  -  Bulletin entry number to delete
C 
 I
	IMPLICIT INTEGER (A-Z)E
 H
	INCLUDE 'BULLDIR.INC'
 D
	INCLUDE 'BULLFOLDER.INC'.
  
	IF (NBULL.GT.0) THENL
	   CALL READDIR(0,IER) 
	   NBULL = -NBULL
	   CALL WRITEDIR(0,IER)
	END IFd
  
	IF (BTEST(FOLDER_FLAG,1)) THEN 
	   OPEN(UNIT=3,FILE=FOLDER_FILE//'.LOG',IOSTAT=IER,STATUS='OLD',S
     &		RECL=LINE_LENGTH,CARRIAGECONTROL='LIST',ACCESS='APPEND')
	   IF (IER.NE.0) THEN
	      OPEN(UNIT=3,FILE=FOLDER_FILE//'.LOG',ERR=900,
     &		RECL=LINE_LENGTH,STATUS='NEW',CARRIAGECONTROL='LIST')E
	   ELSE
	      WRITE (3,'(A)') CHAR(12)E
	   END IF
 =
	   CALL OPEN_BULLFILL
 O
	   ILEN = LINE_LENGTH + 1
 E
	   CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN) 
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THENI
	      WRITE(3,1060) INPUT(7:ILEN),DATE//' '//TIME(:8)
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   ELSE
	      WRITE(3,1060) FROM,DATE//' '//TIME(:8)
	   END IF
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THENO
	      WRITE(3,1050) INPUT(7:ILEN)
	   ELSE
	      WRITE(3,1050) DESCRIP
	      IF (ILEN.GT.0) WRITE (3,'(A)') INPUT(:ILEN)
	   END IF
  
	   DO WHILE (ILEN.GT.0)
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      IF (ILEN.GT.0) WRITE (3,'(A)') INPUT(:ILEN)
	   END DO
 U
	   CLOSE (UNIT=3)			! Bulletin copy completed
  
	   CALL CLOSE_BULLFIL
	END IFE
 
900	CALL READDIR(BULL_ENTRY,IER)
	DELETE(UNIT=2)C
 (
	NEMPTY = NEMPTY + LENGTHU
	CALL WRITEDIR(0,IER)U
  
1050	FORMAT('Description: ',A,/)
1060	FORMAT(/,'From: ',A,' Date: ',A11) 
 	
	RETURNB
	END
 P
 T
 
  
	SUBROUTINE GET_EXDATE(EXDATE,NDAYS)
C 
C  SUBROUTINE GET_EXDATE
CI
C  FUNCTION:  Computes expiration date giving number of days to expire.N
C
	IMPLICIT INTEGER (A-Z)L
 C
	CHARACTER*11 EXDATE
  
	CHARACTER*3 MONTHS(12)
	DIMENSION LENGTH(12)B
	DATA MONTHS/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',U
     &		    'OCT','NOV','DEC'/
	DATA LENGTH/31,27,31,30,31,30,31,31,30,31,30,31/
 
	CALL SYS$ASCTIM(,EXDATE,,)		! Get the present dateI
 L
	DECODE(2,'(I2)',EXDATE(:2)) DAY	! Get day
	DECODE(4,'(I4)',EXDATE(8:11)) YEAR	! Get year
 P
	MONTH = 1
	DO WHILE (MONTHS(MONTH).NE.EXDATE(4:6))	! Get month
	   MONTH = MONTH + 1
	END DO 
 E
	IF (MOD(YEAR,4).EQ.0) THEN		! Correct February length
	   LENGTH(2) = 28			! if we're in a leap year
	ELSE)
	   LENGTH(2) = 27
	END IF_
 L
	NUM_DAYS = NDAYS	! Put number of days into buffer variable 
 L
	DO WHILE (NUM_DAYS.GT.0)S
	   IF (NUM_DAYS+DAY.GT.LENGTH(MONTH)) THEN,
				! If expiration date exceeds end of monthU
	      NUM_DAYS = NUM_DAYS - (LENGTH(MONTH) - DAY + 1)
				! Decrement # of days by days left in month
	      DAY = 1				! Reset day to first of month)
	      MONTH = MONTH + 1			! Increment month pointer
	      IF (MONTH.EQ.13) THEN		! Moved into next year?I
		 MONTH = 1			! Reset month pointer
		 YEAR = YEAR + 1		! Increment year pointer
	         IF (MOD(YEAR,4).EQ.0) THEN	! Correct February length
	            LENGTH(2) = 28		! if we're in a leap year
	         ELSE
	            LENGTH(2) = 27o
	         END IF
	      END IF
	   ELSE			! If expiration date is within the month
	      DAY = DAY + NUM_DAYS		! Find expiration day
	      NUM_DAYS = 0			! Force loop exit!
	   END IF
	END DOt
 
	ENCODE(2,'(I2)',EXDATE(:2)) DAY	! Put day into new date
	ENCODE(4,'(I4)',EXDATE(8:11)) YEAR	! Put year into new date
	EXDATE(4:6) = MONTHS(MONTH)		! Put month into new dates
 
	RETURNO
	END
  
 (
 I
	SUBROUTINE GET_LINE(INPUT,LEN_INPUT)
CI
C  SUBROUTINE GET_LINE
C.
C  FUNCTION:
C	Gets line of input from terminal. 
C(
C  OUTPUTS:Q
C	LEN_INPUT  -  Length of input line.  If = -1, CTRLC entered.
C		      if = -2, CTRLZ entered.
CY
C  NOTES:I
C	Also, on first call, set LEN_INPUT to 1+LENGTH OF INPUT CHARCTER
C	for initializing the CTRLC AST.R
CH
  
	IMPLICIT INTEGER (A-Z))
 E
	LOGICAL*1 DESCRIP(8),DTYPE,CLASS 
	INTEGER*2 LENGTH
	CHARACTER*(*) INPUT
	EQUIVALENCE (DESCRIP(1),LENGTH),(DESCRIP(3),DTYPE).
	EQUIVALENCE (DESCRIP(4),CLASS),(DESCRIP(5),POINTER)
 R
        DATA LENGTH/0/,DTYPE/0/,CLASS/2/,POINTER/0/Y
 S
	EXTERNAL SMG$_EOF
 L
	COMMON /DECNET/ DECNET_PROC,ERROR_UNIT-
	LOGICAL DECNET_PROC
 E
	COMMON /SMG/ KEYBOARD_ID,KEY_TABLE_ID
 ,
	COMMON /CTRLC_FLAG/ FLAGT
  
	CHARACTER PROMPT*(*),NULLPROMPT*1
	LOGICAL*1 USE_PROMPT
 
	USE_PROMPT = .FALSE.I
 t
	GO TO 5
  
	ENTRY GET_INPUT_PROMPT(INPUT,LEN_INPUT,PROMPT)U
 R
	USE_PROMPT = .TRUE.
 a
5	LIMIT = LEN(INPUT)			! Get input line size limit
	INPUT = ' '				! Clean out input buffer
 e
Ct
C  Initialize CTRL-C AST with AST routine CTRLC_ROUTINE andi
C  AST parameter FLAG.  When CTRLC occurs, FLAG is set to 1e
Cg
 i
	CALL DECLARE_CTRLC_AST 
 e
	LEN_INPUT = 0				! Nothing inputted yet
 u
Ce
C  LIB$GET_INPUT is nice way of getting input from terminal,
C  as it handles such thing as accidental wrap around to next line.L
C0
 q
	IF (DECNET_PROC) THEN
	   READ (5,'(Q,A)',IOSTAT=IER) LEN_INPUT,INPUTI
	   IF (IER.NE.0) LEN_INPUT = -2 E
	   RETURN
	ELSE IF (USE_PROMPT) THEN
	   IER = SMG$READ_COMPOSED_LINE(KEYBOARD_ID,KEY_TABLE_ID,
     &		DESCRIP,PROMPT)		! Get line from terminal with promptC
	ELSED
	   IER = SMG$READ_COMPOSED_LINE(KEYBOARD_ID,KEY_TABLE_ID,
     &		DESCRIP,NULLPROMPT)	! Get line from terminal with no prompt,
	END IF 
 D
	IF (.NOT.IER.AND.IER.NE.%LOC(SMG$_EOF)) CALL EXIT(IER) 
 N
	CALL STR$TRIM(DESCRIP,DESCRIP,LEN_INPUT)L
 =
	IF (FLAG.EQ.0) THEN			! If no CTRL-C has occurred
	   CALL CANCEL_CTRLC_AST		! Cancel CTRL-C AST
	   IF (IER.NE.%LOC(SMG$_EOF)) THEN	! End of input?E
	      LEN_INPUT = MIN(LIMIT,LENGTH)	! No. Get length of lineE
	      DO I=0,LEN_INPUT-1		! Extract from descriptor
	         CALL GET_VAL(INPUT(I+1:I+1),%VAL(POINTER+I))
	      END DOa
	      CALL CONVERT_TABS(INPUT,LEN_INPUT)C
	      LEN_INPUT = MAX(LEN_INPUT,LENGTH)
	   ELSE
	      LEN_INPUT = -2			! If CTRL-Z, say so	
	   END IF
	ELSE
	   LEN_INPUT = -1			! If CTRL-C, say so
	END IFE
	RETURN 
	END
 
 S
 F
	SUBROUTINE CONVERT_TABS(INPUT,LEN_INPUT)n
 
	IMPLICIT INTEGER (A-Z)c
  
	CHARACTER*(*) INPUT
  
	PARAMETER TAB = CHAR(9)
 u
	LIMIT = LEN(INPUT)E
 I
	DO WHILE (INDEX(INPUT,TAB).GT.0.AND.LEN_INPUT.LT.LIMIT)
	   TAB_POINT = INDEX(INPUT,TAB)	! Remove tabs
	   MOVE = ((TAB_POINT-1)/8)*8 + 9
	   ADD = MOVE - TAB_POINT
	   IF (MOVE-1.LE.LIMIT) THENn
	      INPUT(MOVE:) = INPUT(TAB_POINT+1:)	
	      DO I = TAB_POINT,MOVE-1
	         INPUT(I:I) = ' '
	      END DO 
	      LEN_INPUT = LEN_INPUT + ADD - 1
	   ELSE
	      DO I = TAB_POINT,LIMITu
	         INPUT(I:I) = ' '
	      END DOd
	      LEN_INPUT = LIMIT+1
	   END IF
	END DO	
 R
        CALL FILTER (INPUT, LEN_INPUT)
 N
	RETURN+
	END
 P
 a
	SUBROUTINE FILTER (INCHAR, LENGTH)_
  
	IMPLICIT INTEGER (A-Z)d
 q
	CHARACTER*(*) INCHAR 
 I
	DO I = 1,LENGTH
	   IF ((INCHAR(I:I).LT.' '.AND.
     &      INCHAR(I:I).NE.CHAR(13).AND.INCHAR(I:I).NE.CHAR(10)))O
     &	    INCHAR(I:I) = '.'
	END DOG
 R
	RETURNN
	END
  
 i
	SUBROUTINE GET_VAL(OUTPUT,INPUT)	! Used to convert logical 
	CHARACTER*(*) OUTPUT			! byte to character valueR
	LOGICAL*1 INPUT
	OUTPUT = CHAR(INPUT) 
	RETURN 
	END
 q
	SUBROUTINE CTRLC_ROUTINE		! CTRL-C AST routinee
	IMPLICIT INTEGER (A-Z)			! If CTRL-C, come here
 t
	COMMON /CTRLY/ CTRLY 
 I
	COMMON /CTRLC_FLAG/ FLAGt
  
	COMMON /DEF_PROT/ ORIGINAL_DEF_PROT
 T
	IF (FLAG.EQ.2) THEN
	   CALL LIB$PUT_OUTPUT('Bulletin aborting...')T
	   CALL SYS$CANEXH()u
	   CALL SYS$SETDFPROT(ORIGINAL_DEF_PROT,)
	   CALL LIB$ENABLE_CTRL(CTRLY,)		! Enable CTRL-Y & -C
	   CALL EXITd
	END IF!
	FLAG = 1				! to set flag
	RETURNP
	END
 R
 E
 )
	SUBROUTINE DECLARE_CTRLC_ASTn
C 
C  SUBROUTINE DECLARE_CTRLC_ASTi
C.
C  FUNCTION:
C	Declares a CTRLC ast.e
C  NOTES:N
C	Assumes terminal assigned to TERM_CHAN in common /TERM_CHAN/.
CT
	IMPLICIT INTEGER (A-Z)'
 '
	EXTERNAL IO$_SETMODE,IO$M_CTRLCAST,CTRLC_ROUTINE
	COMMON /TERM_CHAN/ TERM_CHAN
 
	COMMON /CTRLC_FLAG/ FLAG 
 e
	FLAG = 0				! Init CTRL-C flagU
	IO_CTRLC = %LOC(IO$_SETMODE)+%LOC(IO$M_CTRLCAST)	! Set AST code
	IER=SYS$QIOW(,%VAL(TERM_CHAN),%VAL(IO_CTRLC),,,,	! for QIOO
     &	      CTRLC_ROUTINE,,,,,)		! Enable the AST
 R
	RETURN
  
	ENTRY CANCEL_CTRLC_ASTW
 E
	IER = SYS$CANCEL(%VAL(TERM_CHAN))
 L
	FLAG = 2		! Indicates that a CTRLC will cause an exit
	IER=SYS$QIOW(,%VAL(TERM_CHAN),%VAL(IO_CTRLC),,,,	! for QIOI
     &	      CTRLC_ROUTINE,,,,,)		! Enable the AST
 H
	RETURN 
	END
 T
 F
 =
 D
	SUBROUTINE GET_INPUT_NOECHO(DATA)
CL
C  SUBROUTINE GET_INPUT_NOECHO
CT
C  FUNCTION: Reads data in from terminal without echoing characters.
C	     Also contains entry to assign terminal.
CG
	IMPLICIT INTEGER (A-Z)B
 _
	CHARACTER*(*) DATA,PROMPT
 
	COMMON /TERM_CHAN/ TERM_CHAN.
 '
	COMMON /SMG/ KEYBOARD_ID,KEY_TABLE_ID
 7
	COMMON /CTRLC_FLAG/ FLAG
  
	COMMON /READIT/ READITC
 E
	INCLUDE '($TRMDEF)'
 E
	INTEGER TERMSET(2))
 O
	INTEGER MASK(4)
	DATA MASK/4*'FFFFFFFF'X/L
 G
	DATA PURGE/.TRUE./'
 j
	DO I=1,LEN(DATA)W
	   DATA(I:I) = ' 'I
	END DO 
 E
	IF (PURGE) THEN
	   CALL SMG$READ_STRING(KEYBOARD_ID,DATA,,LEN(DATA),U
     &		TRM$M_TM_NOECHO.OR.TRM$M_TM_PURGE)
	   PURGE = .FALSE.E
	ELSEL
	   CALL SMG$READ_STRING(KEYBOARD_ID,DATA,,LEN(DATA),T
     &		TRM$M_TM_NOECHO)
	END IFO
 U
	RETURNS
 U
	ENTRY GET_INPUT_NOECHO_PROMPT(DATA,PROMPT)A
 C
	DO I=1,LEN(DATA)I
	   DATA(I:I) = ' 'D
	END DON
 ,
	IF (PURGE) THEN
	   CALL SMG$READ_STRING(KEYBOARD_ID,DATA,PROMPT,LEN(DATA),
     &		TRM$M_TM_NOECHO.OR.TRM$M_TM_PURGE)
	   PURGE = .FALSE.D
	ELSEA
	   CALL SMG$READ_STRING(KEYBOARD_ID,DATA,PROMPT,LEN(DATA),A
     &		TRM$M_TM_NOECHO)
	END IF 
 _
	RETURNI
  
	ENTRY GET_INPUT_NUM(DATA,NLEN)a
 g
	DO I=1,LEN(DATA)s
	   DATA(I:I) = ' 'P
	END DOE
  
	IF (PURGE) THEN
	   CALL SMG$READ_STRING(KEYBOARD_ID,DATA,,LEN(DATA),N
     &		TRM$M_TM_PURGE,,TERMSET,NLEN,TERM)
	   PURGE = .FALSE.U
	ELSE'
	   CALL SMG$READ_STRING(KEYBOARD_ID,DATA,,LEN(DATA),,,,
     &		TERMSET,NLEN,TERM)
	END IF
 C
	IF (TERM.NE.13.AND.TERM.NE.510.AND.NLEN.EQ.0) THENL
				! Input did not end with CR or buffer full
	   NLEN = 1
	   DATA(:1) = CHAR(TERM)e
	END IFM
 H
	RETURN 
 L
	ENTRY ASSIGN_TERMINAL
 4
	IER = SYS$ASSIGN('TT',TERM_CHAN,,)	! Assign terminal
  
	CALL DECLARE_CTRLC_AST!
 r
	FLAG = 2		! Indicates that a CTRLC will cause an exit
 l
	IER = SMG$CREATE_VIRTUAL_KEYBOARD(KEYBOARD_ID,,,,20)M
 Y
	IER = SMG$CREATE_KEY_TABLE(KEY_TABLE_ID)v
 a
	IER = SMG$SET_KEYPAD_MODE(KEYBOARD_ID,0)F
 U
	IF (CLI$PRESENT('KEYPAD')) THEN
	   CALL SET_KEYPAD 
	ELSE IF (READIT.EQ.0) THEN 
	   CALL SET_NOKEYPADS
	END IFH
 N
	TERMSET(1) = 16
	TERMSET(2) = %LOC(MASK)
 y
	DO I=ICHAR('0'),ICHAR('9')=
	   MASK(2) = IBCLR(MASK(2),I-32))
	END DOM
 H
	RETURN+
	END
 c
 e
 m
 h
 i
	SUBROUTINE GETPAGSIZ(PAGE_LENGTH,PAGE_WIDTH)o
Cx
C  SUBROUTINE GETPAGSIZ!
Cs
C  FUNCTION:
C	Gets page size of the terminal. 
Cr
C  OUTPUTS: 
C	PAGE_LENGTH  -  Page length of the terminal.
C	PAGE_WIDTH   -  Page size of the terminal.
Cr
	IMPLICIT INTEGER (A-Z) 
 L
	INCLUDE '($DVIDEF)'
 )
	LOGICAL*1 DEVDEPEND(4)
  
	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(4,DVI$_DEVDEPEND,%LOC(DEVDEPEND(1)))n
	CALL ADD_2_ITMLST(4,DVI$_DEVBUFSIZ,%LOC(PAGE_WIDTH))i
	CALL END_ITMLST(GETDVI_ITMLST)		! Get address of itemlist
 !
	CALL SYS$GETDVIW(,,'TT',%VAL(GETDVI_ITMLST),,,,)1
 Y
	PAGE_LENGTH = ZEXT(DEVDEPEND(4))T
 :
	PAGE_WIDTH = MIN(PAGE_WIDTH,132)o
 w
	RETURN
	END
 O
 E
 
 
 
	LOGICAL FUNCTION SLOW_TERMINALN
CP
C  FUNCTION SLOW_TERMINAL_
CE
C  FUNCTION:
C	Indicates that terminal has a slow speed (2400 baud or less).E
CN
C  OUTPUTS:h
C	SLOW_TERMINAL = .true. if slow, .false. if not.i
C 
  
	IMPLICIT INTEGER (A-Z)T
 I
	EXTERNAL IO$_SENSEMODEs
 L
	COMMON /TERM_CHAN/ TERM_CHANH
 T
	COMMON CHAR_BUF(2)g
 e
	LOGICAL*1 IOSB(8)
 I
	INCLUDE '($TTDEF)')
 E
	IER = SYS$QIOW(,%VAL(TERM_CHAN),IO$_SENSEMODE,IOSB,,,
     &		  CHAR_BUF,%VAL(8),,,,)N
 (
	IF (IOSB(3).LE.TT$C_BAUD_2400) THEN
	   SLOW_TERMINAL = .TRUE.
	ELSED
	   SLOW_TERMINAL = .FALSE. 
	END IFN
 /
	RETURN0
	END
 /
 I
 R
 Y
	SUBROUTINE SHOW_PRIVF
CL
C  SUBROUTINE SHOW_PRIV_
CC
C  FUNCTION:
C	To show privileges necessary for managing bulletin board._
C
 
	IMPLICIT INTEGER (A-Z)T
  
	INCLUDE 'BULLUSER.INC'U
 R
	INCLUDE 'BULLFILES.INC'
 
	INCLUDE '($PRVDEF)'
 E
	INCLUDE '($SSDEF)'
 T
	COMMON /PRVDEF/ PRIVS
	CHARACTER*8 PRIVS(0:38)
 E
	CALL OPEN_BULLUSER_SHARED		! Get BULLUSER.DAT filep
 l
	CALL READ_USER_FILE_HEADER(IER)
 a
	IF (IER.EQ.0) THEN			! If header is present, exit
	   IF (NEW_FLAG(1).EQ.-1.AND.NEW_FLAG(2).EQ.-1) THEN  ! Info not presentc
	      CALL CLOSE_BULLUSER
	      CALL OPEN_BULLUSER			! Get BULLUSER.DAT file 
	      CALL READ_USER_FILE_HEADER(IER)
	      USERPRIV(1) = PRV$M_OPER.OR.PRV$M_CMKRNL.OR.PRV$M_SETPRVl
	      USERPRIV(2) = 0
	      REWRITE (4) USER_HEADER
	   END IF
	   WRITE (6,'('' Following privileges are needed for privileged
     & commands:'')')E
	   DO I=0,38
	      IF ((I.LT.32.AND.BTEST(USERPRIV(1),I)).OR.M
     &		  (I.GT.31.AND.BTEST(USERPRIV(2),I-32))) THEN	
		 WRITE (6,'(1X,A)') PRIVS(I)
	      END IFp
	   END DO
	ELSE 
	   WRITE (6,'('' ERROR: Cannot show privileges.'')')
	END IFE
 I
	CALL CLOSE_BULLUSER			! All finished with BULLUSER,
 E
	CALL CHKACL(BULLUSER_FILE(:TRIM(BULLUSER_FILE)),IER)L
	IF (IER.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL).AND.IER) THEN
	   CALL SHOWACL(BULLUSER_FILE(:TRIM(BULLUSER_FILE)))u
	END IF 
 L
	RETURNT
 _
	END
 n
  
 L
 A
	SUBROUTINE SET_PRIV
CM
C  SUBROUTINE SET_PRIV
Ct
C  FUNCTION:
C	To set privileges necessary for managing bulletin board.
C 
 ,
	IMPLICIT INTEGER (A-Z)o
 e
	INCLUDE '($PRVDEF)'
 G
	INCLUDE 'BULLUSER.INC'L
 I
	COMMON /PRIVILEGES/ PROCPRIV(2),NEEDPRIV(2)
 S
	COMMON /PRVDEF/ PRIVS
	CHARACTER*8 PRIVS(0:38)
	DATA PRIVS 
     &	/'CMKRNL','CMEXEC','SYSNAM','GRPNAM','ALLSPOOL','DETACH',
     &  'DIAGNOSE','LOG_IO','GROUP','ACNT','PRMCEB','PRMMBX','PSWAPM',
     &	'ALTPRI','SETPRV','TMPMBX','WORLD','MOUNT','OPER','EXQUOTA',I
     &	'NETMBX','VOLPRO','PHY_IO','BUGCHK','PRMGBL','SYSGBL','PFNMAP',
     &	'SHMEM','SYSPRV','BYPASS','SYSLCK','SHARE','UPGRADE','DOWNGRADE',
     &	'GRPPRV','READALL',' ',' ','SECURITY'/A
 !
	EXTERNAL CLI$_ABSENT,CLI$_NEGATED
 /
	DIMENSION ONPRIV(2),OFFPRIV(2)I
 
	CHARACTER*32 INPUT_PRIV
 n
	IF (.NOT.SETPRV_PRIV().OR..NOT.BTEST(PROCPRIV(1),PRV$V_SETPRV)) THENO
	   WRITE (6,'('' ERROR: This command requires SETPRV privileges.'')')
	   RETURN
	END IF 
 E
	IF (CLI$PRESENT('ID').OR.
     &		CLI$PRESENT('ID').EQ.%LOC(CLI$_NEGATED)) THEN 
	   DO WHILE (CLI$GET_VALUE('PRIVILEGES',INPUT_PRIV,PLEN)L
     &	       .NE.%LOC(CLI$_ABSENT))		! Get the IDsa
	      IF (CLI$PRESENT('ID')) THEN
		 CALL ADD_ACL(INPUT_PRIV(:PLEN),'R+C',IER)
	      ELSE 
		 CALL DEL_ACL(INPUT_PRIV(:PLEN),'R+C',IER)
	      END IF 
	      IF (.NOT.IER) CALL SYS_GETMSG(IER)I
	   END DO
	   RETURN
	END IFC
 (
	OFFPRIV(1) = 0D
	OFFPRIV(2) = 0
	ONPRIV(1) = 0
	ONPRIV(2) = 0
 U
	DO WHILE (CLI$GET_VALUE('PRIVILEGES',INPUT_PRIV,PLEN)
     &	    .NE.%LOC(CLI$_ABSENT))		! Get the privilegesT
	   PRIV_FOUND = -1R
	   I = 0D
	   DO WHILE (I.LT.39.AND.PRIV_FOUND.EQ.-1) 
	      IF (INPUT_PRIV(:PLEN).EQ.PRIVS(I)) PRIV_FOUND = I
	      IF (INPUT_PRIV(3:PLEN).EQ.PRIVS(I)) PRIV_FOUND = It
	      I = I + 1
	   END DO
	   IF (PRIV_FOUND.EQ.-1) THEN
	      WRITE(6,'('' ERROR: Incorrectly specified privilege = '',
     &		A)') INPUT_PRIV(:PLEN)
	      RETURN_
	   ELSE IF (INPUT_PRIV(:2).EQ.'NO') THEN)
	      IF (INPUT_PRIV.EQ.'NOSETPRV') THEND
	       WRITE(6,'('' ERROR: Cannot remove SETPRV privileges.'')')O
	       RETURN
	      ELSE IF (PRIV_FOUND.LT.32) THEN
		 OFFPRIV(1) = IBSET(OFFPRIV(1),PRIV_FOUND)
	      ELSES
		 OFFPRIV(2) = IBSET(OFFPRIV(2),PRIV_FOUND-32)o
	      END IF/
	   ELSE
	      IF (PRIV_FOUND.LT.32) THENA
		 ONPRIV(1) = IBSET(ONPRIV(1),PRIV_FOUND)
	      ELSE_
		 ONPRIV(2) = IBSET(ONPRIV(2),PRIV_FOUND-32) 
	      END IF	
	   END IF
	END DOU
 I
	CALL OPEN_BULLUSER		! Get BULLUSER.DAT file
  
	CALL READ_USER_FILE_HEADER(IER)
 M
	IF (IER.EQ.0) THEN			! If header is present, exit
	   USERPRIV(1) = USERPRIV(1).OR.ONPRIV(1)
	   USERPRIV(2) = USERPRIV(2).OR.ONPRIV(2)
	   USERPRIV(1) = USERPRIV(1).AND.(.NOT.OFFPRIV(1))d
	   USERPRIV(2) = USERPRIV(2).AND.(.NOT.OFFPRIV(2))(
	   REWRITE (4) USER_HEADERC
	   WRITE (6,'('' Privileges successfully modified.'')')
	ELSEA
	   WRITE (6,'('' ERROR: Cannot modify privileges.'')')N
	END IFO
 T
	CALL CLOSE_BULLUSER			! All finished with BULLUSERO
 R
	RETURN 
 f
	END
 n
 w
 o
	SUBROUTINE ADD_ACL(ID,ACCESS,IER)
Ct
C  SUBROUTINE ADD_ACLr
Ca
C  FUNCTION: Adds ACL to bulletin files.
CT
C  PARAMETERS:
C	ID - Character string containing identifier to add to ACL.
C	ACCESS - Character string containing access controls to give to ID.I
C	IER - Return error from attempting to set ACL.
CR
C  NOTE: The ID must be in the RIGHTS data base.
CF
	IMPLICIT INTEGER (A-Z)U
 '
	INCLUDE 'BULLFOLDER.INC' 
 A
	INCLUDE 'BULLFILES.INC'
 
	CHARACTER ACLENT*255,ID*(*),ACCESS*(*)G
 Y
	INCLUDE '($ACLDEF)'
 U
	INCLUDE '($SSDEF)'E
 .
	IER = SYS$PARSE_ACL('(IDENTIFIER='//ID//',ACCESS=' 
     &	   //ACCESS//')',ACLENT,,)T
	IF (.NOT.IER) THEN&
	   IF (IER.EQ.SS$_NOSUCHID.AND.ADDID.AND.
     &				INDEX(ACCESS,'C').EQ.0) THEN
	      CALL GET_UAF(ID,USER,GROUP,ACCOUNT,FLAGS,IER)
	      IF (.NOT.IER) THEN 
		 CALL ERRSNS(IDUMMY,IER)
		 WRITE (6,'(
     &		    '' ERROR: Specified username cannot be verified.'')')
		 CALL SYS_GETMSG(IER)S
	         RETURN
	      END IFO
	      IDENT = USER + ISHFT(GROUP,16)	
	      IER = SYS$ADD_IDENT(ID,%VAL(IDENT),,)
	      IF (IER) THEN
	         IER = SYS$PARSE_ACL('(IDENTIFIER='//ID//',ACCESS='
     &	           //ACCESS//')',ACLENT,,)A
	      END IFR
	   END IF
	END IFN
	IF (.NOT.IER) RETURNE
 E
	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(ICHAR(ACLENT(:1)),ACL$C_ADDACLENT,%LOC(ACLENT))
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist
 E
	IF (INDEX(ACCESS,'C').GT.0.AND.INDEX(ACCESS,'W').EQ.0) THEN
	   IER = SYS$CHANGE_ACL(,ACL$C_FILE,BULLUSER_FILE(:TRIM(
     &		   BULLUSER_FILE)),%VAL(ACL_ITMLST),,,)L
	   RETURN
	END IFG
 T
	FLEN = TRIM(FOLDER1_FILE)
 a
	IER = SYS$CHANGE_ACL(,ACL$C_FILE,FOLDER1_FILE(:FLEN)//t
     &		'.BULLDIR',%VAL(ACL_ITMLST),,,)I
	IF (.NOT.IER) RETURN_
	IER = SYS$CHANGE_ACL(,ACL$C_FILE,FOLDER1_FILE(:FLEN)//A
     &		'.BULLFIL',%VAL(ACL_ITMLST),,,)Y
	IF (.NOT.IER) RETURNF
 U
	RETURNI
	END
 '
 P
 )
	SUBROUTINE DEL_ACL(ID,ACCESS,IER)
C(
C  SUBROUTINE DEL_ACL 
CL
C  FUNCTION: Adds ACL to bulletin files.
C6
C  PARAMETERS:
C	ID - Character string containing identifier to add to ACL.
C	ACCESS - Character string containing access controls to give to ID.S
C	IER - Return error from attempting to set ACL.
CS
C  NOTE: The ID must be in the RIGHTS data base.
Ci
	IMPLICIT INTEGER (A-Z)
 O
	INCLUDE 'BULLFOLDER.INC' 
 e
	INCLUDE 'BULLFILES.INC'
 A
	CHARACTER ACLENT*255,ID*(*),ACCESS*(*)
 
	INCLUDE '($ACLDEF)'
 
	IF (ID.NE.' ') THEN
	   IER = SYS$PARSE_ACL('(IDENTIFIER='//ID//',ACCESS='
     &	      //ACCESS//')',ACLENT,,)
	   IF (.NOT.IER) RETURN
 V
	   CALL INIT_ITMLST	! Initialize item listF
	   CALL ADD_2_ITMLST(ICHAR(ACLENT(:1)),ACL$C_DELACLENT,%LOC(ACLENT))o
	   CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist)
	ELSE
	   CALL INIT_ITMLST	! Initialize item listP
	   CALL ADD_2_ITMLST(255,ACL$C_DELETEACL,%LOC(ACLENT))
	   CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlistN
	END IFW
 R
	IF (INDEX(ACCESS,'C').GT.0) THENe
	   IER = SYS$CHANGE_ACL(,ACL$C_FILE,BULLUSER_FILE(:TRIM(
     &		   BULLUSER_FILE)),%VAL(ACL_ITMLST),,,)f
	   RETURN
	END IF 
 I
	FLEN = TRIM(FOLDER1_FILE)
 T
	IER = SYS$CHANGE_ACL(,ACL$C_FILE,FOLDER1_FILE(:FLEN)//
     &		'.BULLDIR',%VAL(ACL_ITMLST),,,)I
	IF (.NOT.IER) RETURN$
	IER = SYS$CHANGE_ACL(,ACL$C_FILE,FOLDER1_FILE(:FLEN)//O
     &		'.BULLFIL',%VAL(ACL_ITMLST),,,),
	IF (.NOT.IER) RETURN.
 C
	RETURN0
	END
  
 L
 T
 I
	SUBROUTINE CREATE_FOLDERL
CT
C  SUBROUTINE CREATE_FOLDER
C
C  FUNCTION: Creates a new bulletin folder.H
CP
 F
	IMPLICIT INTEGER (A-Z)P
 _
	INCLUDE 'BULLFOLDER.INC'o
 r
	INCLUDE 'BULLUSER.INC'n
 n
	INCLUDE 'BULLFILES.INC'
 I
	INCLUDE 'BULLDIR.INC'
 
	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
	DATA REMOTE_SET /.FALSE./
 '
	IF (.NOT.SETPRV_PRIV().AND.CLI$PRESENT('NEEDPRIV')) THENA
	   WRITE(6,'('' ERROR: CREATE is a privileged command.'')')
	   RETURN
	END IF
 L
	IER = CLI$GET_VALUE('CREATE_FOLDER',FOLDER,LEN_T) ! Get folder name
 e
	IF (LEN_T.GT.25) THEN
	   WRITE(6,'('' ERROR: Folder name must be < 26 characters.'')') 
	   RETURN
	END IFR
  
	IF (.NOT.SETPRV_PRIV().AND.(CLI$PRESENT('ALWAYS').OR.
     &	     CLI$PRESENT('NOTIFY').OR.CLI$PRESENT('READNEW').OR.E
     &	     CLI$PRESENT('BRIEF').OR.CLI$PRESENT('SYSTEM'))) THEN
	   WRITE (6,'('' ERROR: Privileged qualifier specified.'')')n
	   RETURN
	END IFe
 o
	IF (CLI$PRESENT('NODE')) THEN	! Remote node specified? 
	   IER = CLI$GET_VALUE('NODE',FOLDER_BBOARD,LEN_B) ! Get node nameD
	   FOLDER_BBOARD = '::'//FOLDER_BBOARD(:LEN_B)X
	   FOLDER1_BBOARD = FOLDER_BBOARD
	   IF (.NOT.CLI$GET_VALUE('REMOTENAME',FOLDER1)) THEN
	      FOLDER1 = FOLDER
	   END IF
	   CALL CONNECT_REMOTE_FOLDER(READ_ONLY,IER)E
	   IF (IER.NE.0) THEN
	    WRITE (6,'('' ERROR: Folder not accessible on remote node.'')')
	    RETURN 
	   ELSE IF (CLI$PRESENT('SYSTEM').AND.M
     &				.NOT.BTEST(FOLDER1_FLAG,2)) THEN
	    WRITE (6,'('' ERROR: /SYSTEM not allowed as remote node'', 
     &			'' is not SYSTEM folder.'')')
	    RETURNs
	   END IF
	END IFe
  
	LENDES = 0,
	DO WHILE (LENDES.EQ.0)o
	   IF (CLI$PRESENT('DESCRIPTION')) THEN		! DESCRIPTION specified?
	      IER = CLI$GET_VALUE('DESCRIPTION',FOLDER_DESCRIP,LENDES)S
	   ELSE
	      WRITE (6,'('' Enter one line description of folder.'')')'
	      CALL GET_LINE(FOLDER_DESCRIP,LENDES)	! Get input line
	      FOLDER_DESCRIP = FOLDER_DESCRIP(:LENDES)	! End fill with spaces
	   END IF
	   IF (LENDES.LE.0) THEN
	      WRITE (6,'('' Aborting folder creation.'')')S
	      RETURN,
	   ELSE IF (LENDES.GT.80) THEN		! If too many charactersE
	      WRITE(6,'('' ERROR: folder must be < 80 characters.'')')!
	      LENDES = 0S
	   END IF
	END DO
 M
	CALL OPEN_BULLFOLDER		! Open folder fileR
	READ (7,IOSTAT=IER,KEY=FOLDER,KEYID=0)(
					! See if folder existsR
 _
	IF (IER.EQ.0) THENR
	   WRITE (6,'('' ERROR: Specified folder already exists.'')')
	   GO TO 1000
	END IFE
 I
	IF (CLI$PRESENT('OWNER')) THEN	
	   IF (.NOT.SETPRV_PRIV().AND..NOT.CLI$PRESENT('ID')) THEN(
	      WRITE (6,'('' ERROR: /OWNER requires privileges.'')')
	      CALL CLOSE_BULLFOLDER
	      RETURNI
	   ELSE
	      CALL CLI$GET_VALUE('OWNER',FOLDER1_OWNER,LEN_P)
	      IF (LEN_P.GT.12) THEN
	         WRITE (6,'('' ERROR: Folder owner name must be'',I
     &		        '' no more than 12 characters long.'')')
	         CALL CLOSE_BULLFOLDER
	         RETURN
	      ELSE IF (CLI$PRESENT('ID')) THENH
		 IER = CHKPRO(FOLDER1_OWNER)
	         IF (.NOT.IER) THEN
	            WRITE (6,'('' ERROR: ID not valid.'')')
	            CALL CLOSE_BULLFOLDER
	            RETURNF
		 END IF 
	      ELSEI
	         CALL GET_UAF
     &		   (FOLDER1_OWNER,USERB1,GROUPB1,ACCOUNTB1,FLAGS,IER))
	         IF (.NOT.IER) THEN
	            WRITE (6,'('' ERROR: Owner not valid username.'')')
	            CALL CLOSE_BULLFOLDER
	            RETURN	
		 END IFP
	      END IF 
	      FOLDER_OWNER = FOLDER1_OWNER2
	   END IF
	ELSE 
	   FOLDER_OWNER = USERNAME		! Get present usernameT
	   FOLDER1_OWNER = FOLDER_OWNER		! Save for later
	END IF 
 E
	FOLDER_SET = .TRUE.
 V
	CALL SYS$SETDFPROT('FF00'X,CUR_DEF_PROT)F
			! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD,GROUP)P
 (
CP
C  Folder file is placed in the directory FOLDER_DIRECTORY.N
C  The file prefix is the name of the folder.P
C_
 N
	FD_LEN = TRIM(FOLDER_DIRECTORY)
	IF (FD_LEN.EQ.0) THEN
	 WRITE (6,'('' ERROR: System programmer has disabled folders.'')')	
	 GO TO 910E
	ELSEi
	 FOLDER_FILE = FOLDER_DIRECTORY(:FD_LEN)//FOLDERI
	END IFT
 	
	OPEN (UNIT=2,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))R
     &	      //'.BULLDIR',STATUS='NEW',FORM='UNFORMATTED',
     &	      RECORDTYPE='FIXED',RECORDSIZE=DIR_RECORD_LENGTH/4,S
     &	      ORGANIZATION='INDEXED',IOSTAT=IER,DISPOSE='KEEP',
     &	      KEY=(9:12:INTEGER,1:8:CHARACTER),ACCESS='KEYED')d
 )
	IF (IER.NE.0) THEN 
	   WRITE(6,'('' ERROR: Cannot create folder directory file.'')') 
	   CALL ERRSNS(IDUMMY,IER)e
	   CALL SYS_GETMSG(IER)
	   GO TO 910
	END IFo
 S
	OPEN (UNIT=1,FILE=FOLDER_FILE(:TRIM(FOLDER_FILE))
     1	 //'.BULLFIL',STATUS='NEW',
     1	 ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=32,r
     1	 FORM='UNFORMATTED',IOSTAT=IER)
 	
	IF (IER.NE.0) THENr
	   WRITE(6,'('' ERROR: Cannot create folder message file.'')')r
	   CALL ERRSNS(IDUMMY,IER).
	   CALL SYS_GETMSG(IER)
	   GO TO 910T
	END IFs
 
	FOLDER_FLAG = 0
  
	IF (CLI$PRESENT('PRIVATE').OR.CLI$PRESENT('SEMIPRIVATE')) THENN
				! Will folder have access limitations?
	   FOLDER1_FILE = FOLDER_FILE
	   CLOSE (UNIT=1)
	   CLOSE (UNIT=2)
	   IF (CLI$PRESENT('SEMIPRIVATE')) THEN
	      CALL ADD_ACL('*','R',IER)
	   ELSE
	      CALL ADD_ACL('*','NONE',IER)S
	   END IF
	   CALL ADD_ACL(FOLDER_OWNER,'R+W+C',IER)
	   OPEN (UNIT=2,FILE=FOLDER_FILE(:TRIM(FOLDER_FILE))E
     1	    //'.BULLDIR',STATUS='OLD',IOSTAT=IER1)M
	   OPEN (UNIT=1,FILE=FOLDER_FILE(:TRIM(FOLDER_FILE)) 
     1	    //'.BULLFIL',STATUS='OLD',IOSTAT=IER1)M
	   IF (.NOT.IER) THEN
	      WRITE(6,O
     &	      '('' ERROR: Cannot create private folder using ACLs.'')')
	      CALL SYS_GETMSG(IER)R
	      GO TO 910
	   END IF
	   FOLDER_FLAG = IBSET(FOLDER_FLAG,0)
	END IF 
  
	IER = 0
	LAST_NUMBER = 1
	DO WHILE (IER.EQ.0.AND.LAST_NUMBER.LT.FOLDER_MAX-1)
	   READ (7,IOSTAT=IER,KEY=LAST_NUMBER,KEYID=1) 
	   LAST_NUMBER = LAST_NUMBER + 1N
	END DOL
 A
	IF (IER.EQ.0) THEN)
	 WRITE (6,'('' ERROR: Folder limit of '',I,'' has been reached.'')')X
     &			FOLDER_MAX.
	 WRITE (6,'('' Unable to add specified folder.'')')
	 GO TO 910B
	ELSE_
	   FOLDER1_NUMBER = LAST_NUMBER - 1
	END IFC
 T
	IF (.NOT.CLI$PRESENT('NODE')) THEN
	   FOLDER_BBOARD = 'NONE'
	   IF (REMOTE_SET) CLOSE (UNIT=REMOTE_UNIT)
	   REMOTE_SET = .FALSE.
	   FOLDER_BBEXPIRE = 14
	   F_NBULL = 0E
	   NBULL = 0R
	   F_NEWEST_BTIM(1) = 0
	   F_NEWEST_BTIM(2) = 0
	   F_NEWEST_NOSYS_BTIM(1) = 0
	   F_NEWEST_NOSYS_BTIM(2) = 0
	   F_EXPIRE_LIMIT = 0
	   FOLDER_NUMBER = FOLDER1_NUMBER
	ELSEE
	   CLOSE (UNIT=1,STATUS='DELETE')
	   CLOSE (UNIT=2,STATUS='DELETE')
	   IF (FOLDER1.NE.FOLDER) THEN	! Different remote folder name?t
	      REMOTE_SET = .FALSE.S
    	      CALL OPEN_BULLDIR		! If so, store name in directory file 
	      BULLDIR_HEADER(13:) = FOLDER1
	      CALL WRITEDIR_NOCONV(0,IER)
	      CALL CLOSE_BULLDIR
	      FOLDER1_BBOARD = FOLDER1_BBOARD(:LEN_B+2)//'*' 
	      FOLDER1 = FOLDER.
	   END IF
	   REMOTE_SET = .TRUE.)
	   IF (BTEST(FOLDER1_FLAG,0)) FOLDER_FLAG = IBSET(FOLDER_FLAG,0) 
	   FOLDER1_FLAG = FOLDER_FLAG
	   FOLDER1_DESCRIP = FOLDER_DESCRIP
	   FOLDER_COM = FOLDER1_COM
	   NBULL = F_NBULL
	END IFI
 _
	FOLDER_OWNER = FOLDER1_OWNER
  
	IF (CLI$PRESENT('SYSTEM')) FOLDER_FLAG = IBSET(FOLDER_FLAG,2)
	IF (CLI$PRESENT('ID')) FOLDER_FLAG = IBSET(FOLDER_FLAG,6)
	IF (CLI$PRESENT('ALWAYS')) FOLDER_FLAG = IBSET(FOLDER_FLAG,7)
 A
	CALL WRITE_FOLDER_FILE(IER)
	CALL MODIFY_SYSTEM_LIST(0)D
 M
	CLOSE (UNIT=1) 
	CLOSE (UNIT=2)e
 s
	NOTIFY = 0
	READNEW = 0
	BRIEF = 0
	IF (CLI$PRESENT('NOTIFY')) NOTIFY = 1
	IF (CLI$PRESENT('READNEW')) READNEW = 1
	IF (CLI$PRESENT('SHOWNEW')) BRIEF = 1
	IF (CLI$PRESENT('BRIEF')) THEN 
	   BRIEF = 1F
	   READNEW = 1S
	END IF_
	CALL SET_FOLDER_DEFAULT(NOTIFY,READNEW,BRIEF)
 U
	WRITE (6,'('' Folder is now set to '',A)')R
     &		FOLDER(:TRIM(FOLDER))//'.'
 L
	GO TO 1000E
 L
910	WRITE (6,'('' Aborting folder creation.'')')
	IF (FOLDER_NUMBER.EQ.0) FOLDER_SET = .FALSE.
	CLOSE (UNIT=1,STATUS='DELETE')L
	CLOSE (UNIT=2,STATUS='DELETE')L
 
1000	CALL CLOSE_BULLFOLDER
	CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protectionI
 U
	RETURNO
 R
	END
 r
 I
 U
	INTEGER FUNCTION CHKPRO(INPUT)'
CL
C 	Description:I
C		Parse given identify into binary ACL format. 
C		Call SYS$CHKPRO to check if present process has readI
C		access to an object if the object's protection is the ACL.R
C(
	IMPLICIT INTEGER (A-Z) 
 v
	CHARACTER ACL*255
	CHARACTER*(*) INPUT
 L
	INCLUDE '($CHPDEF)'
 R
	CHKPRO = SYS$PARSE_ACL('(IDENTIFIER='//INPUT(:TRIM(INPUT))//.
     &		',ACCESS=R)',ACL,,)	! Convert to ACL into binary formatt
	IF (.NOT.CHKPRO) RETURN		! Exit if can't.
 .
	FLAGS = CHP$M_READ		! Specify read access checking 
  
	CALL INIT_ITMLST		! Initialize item listE
	CALL ADD_2_ITMLST(ICHAR(ACL(:1)),CHP$_ACL,%LOC(ACL(1:1)))
	CALL ADD_2_ITMLST(4,CHP$_FLAGS,%LOC(FLAGS))
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist
 
	CHKPRO = SYS$CHKPRO(%VAL(ACL_ITMLST))	! Check if process has the 
						! rights-id assigned to it
	RETURN 
	END
