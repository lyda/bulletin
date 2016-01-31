C
C  BULLETIN4.FOR, Version 12/17/97
C  Purpose: Contains subroutines for the BULLETIN utility program.
C  Environment: VAX/VMS
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

	CHARACTER TODAY*24

	DIMENSION TODAY_BTIM(2)

	MARK = SYS_TRNLNM_SYSTEM('BULL_MARK','DEFINED')

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
	      IF (MARK) CALL LIB$DELETE_FILE('BULL_MARK:'//
     &					TEMP_USER(:LU)//'.*MARK;*')
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

	CHARACTER TODAY*24

	DIMENSION TODAY_BTIM(2)

	CALL SYS$ASCTIM(,TODAY,,)		! Get the present time
	CALL SYS_BINTIM(TODAY,TODAY_BTIM)

	MARK = SYS_TRNLNM_SYSTEM('BULL_MARK','DEFINED')

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
		    IF (MARK) CALL LIB$DELETE_FILE('BULL_MARK:'//
     &					USERNAME(:LU)//'.*MARK;*')
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
	      USERNAME(2:2) = CHAR(127.AND.ICHAR(USERNAME(2:2)))
	   END IF
	   READ (4,KEYEQ=USERNAME,IOSTAT=IER)
	   IF (IER.NE.0) DELETE (UNIT=9)
	   READ (9,IOSTAT=IER) USERNAME
	END DO

	CALL CLOSE_BULLINF
	CALL CLOSE_BULLUSER

	USERNAME = TEMP_USER

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

	COMMON /LAST_RECORD_WRITTEN/ OCOUNT

	INCLUDE 'BULLDIR.INC'

	IF (REMOTE_SET) THEN
	   CALL REMOTE_COPY_BULL(IER)
	   IF (IER.NE.0) CALL ERROR_AND_EXIT
	END IF

	DO I=1,IBLOCK-1
	   READ(INLUN,'(A)')
	END DO

	OCOUNT = OBLOCK
	ICOUNT = IBLOCK

	NBLANK = 0
	LENGTH = 0
	DO WHILE (LENGTH.GE.0)
	   ILEN = 0
	   DO WHILE (ILEN.EQ.0)
	      READ(INLUN,'(Q,A)',END=100) ILEN,INPUT
	      ILEN = MIN(ILEN,TRIM(INPUT),LINE_LENGTH)
	      IF (ILEN.GT.1.AND.ICHAR(INPUT(ILEN:ILEN)).EQ.10) THEN
		 INPUT(ILEN-1:ILEN-1) = CHAR(32)	! Remove imbedded
		 INPUT(ILEN:ILEN) = CHAR(32)	! CR/LFs at end of file.
		 ILEN = ILEN - 2
	      END IF
	      IF (ILEN.GT.0) THEN
		 IF (ICOUNT.EQ.IBLOCK) THEN
		    IF (INPUT(:6).EQ.'From: ') THEN
		       INPUT(:4) = 'FROM'
		    END IF
		 END IF
		 ICOUNT = ICOUNT + 1
	      ELSE IF (ILEN.EQ.0.AND.ICOUNT.GT.IBLOCK) THEN
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
	   CALL STORE_BULL(ILEN,INPUT,OCOUNT)
	   LENGTH = LENGTH + ILEN + 1
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




	SUBROUTINE STORE_BULL(ILEN,INPUT,OCOUNT)

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	COMMON /STORE_POINT/ POINT
	DATA POINT/-1/

	CHARACTER INPUT*(*),OUTPUT*255

	IF (POINT.EQ.-1) THEN
	   POINT = 0
	   IF (BTEST(FOLDER_FLAG,12)) CALL STORE_BULL1(0,CHAR(0),OCOUNT)
	END IF

	IF (BTEST(FOLDER_FLAG,12)) THEN
	   CALL COMPRESS(INPUT(:ILEN),OUTPUT,OLEN)
	   CALL STORE_BULL1(OLEN,OUTPUT,OCOUNT)
	ELSE
	   CALL STORE_BULL1(ILEN,INPUT,OCOUNT)
	END IF

	RETURN
	END




	SUBROUTINE STORE_BULL1(ILEN,INPUT,OCOUNT)

	IMPLICIT INTEGER (A-Z)

	PARAMETER BRECLEN=128

	CHARACTER INPUT*(*),OUTPUT*256

	COMMON /STORE_POINT/ POINT

	IF (ILEN+POINT+1.GT.BRECLEN) THEN
	   IF (POINT.EQ.BRECLEN) THEN
	      CALL WRITE_BULL_FILE(OCOUNT,OUTPUT(:POINT))
	      OUTPUT = CHAR(ILEN)//INPUT
	      POINT = ILEN + 1
	   ELSE IF (POINT.EQ.BRECLEN-1) THEN
	      CALL WRITE_BULL_FILE(OCOUNT,OUTPUT(:POINT)//CHAR(ILEN))
	      OUTPUT = INPUT
	      POINT = ILEN
	   ELSE
	      CALL WRITE_BULL_FILE(OCOUNT,OUTPUT(:POINT)//CHAR(ILEN)
     &		//INPUT(:BRECLEN-1-POINT))
	      OUTPUT = INPUT(BRECLEN-POINT:)
	      POINT = ILEN - (BRECLEN-1-POINT)
	   END IF
	   OCOUNT = OCOUNT + 1
	   DO WHILE (POINT.GE.BRECLEN)
	      CALL WRITE_BULL_FILE(OCOUNT,OUTPUT(:BRECLEN))
	      OCOUNT = OCOUNT + 1
	      OUTPUT = OUTPUT(BRECLEN+1:)
	      POINT = POINT - BRECLEN
	   END DO
	ELSE
	   OUTPUT(POINT+1:) = CHAR(ILEN)//INPUT(:ILEN)
	   POINT = POINT + ILEN + 1
	END IF

	RETURN

	ENTRY FLUSH_BULL(OCOUNT)

	IF (POINT.LT.BRECLEN) OUTPUT(POINT+1:POINT+1) = CHAR(0)
	CALL WRITE_BULL_FILE(OCOUNT,OUTPUT(:BRECLEN))
	POINT = -1

	RETURN

	END


	SUBROUTINE WRITE_BULL_FILE(OCOUNT,OUTPUT)

	IMPLICIT INTEGER (A-Z)

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	CHARACTER*(*) OUTPUT

	IF (REMOTE_SET) THEN
	   CALL REMOTE_WRITE_BULL_FILE(OUTPUT)
	ELSE
	   WRITE (1'OCOUNT) OUTPUT
	END IF

	RETURN
	END


	SUBROUTINE GET_BULL_LINE(SBLOCK,BLENGTH,BUFFER,ILEN)

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'

	CHARACTER*(*) BUFFER

	COMMON /HEADER/ HEADER
	LOGICAL HEADER /.TRUE./

	COMMON /DATE/ DATE_LINE
	CHARACTER*(INPUT_LENGTH) DATE_LINE

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /REF/ REFERENCES,LREF
	CHARACTER*256 REFERENCES

	COMMON /HEADER_QUEUE/ HEADER_Q,HEADER_Q1,NHEAD

	IF (ILEN.GT.LINE_LENGTH) THEN		! First read?
	   CALL STRIP_HEADER(' ',-1,IER)
	   STRIP = .NOT.HEADER
	   IBLOCK = SBLOCK			! Initialize pointers.
	   BULL_HEADER = .TRUE.
	   SEEN_FROM = .FALSE.
	   SEEN_SUBJ = .FALSE.
	   READ_HEAD = .FALSE.
	   CALL GET_BULL(IBLOCK,BUFFER,ILEN)
	   IF (ILEN.LE.0) IBLOCK = IBLOCK + 1
	   MSG_SENT = .FALSE.
	   FINDREF = .NOT.STRIP.AND.REMOTE_SET.EQ.4
	ELSE					! Else set ILEN to zero
	   ILEN = 0				! to request next line
	END IF

	IF (MSG_SENT) THEN
	   BUFFER = ' '
	   ILEN = 1
	   MSG_SENT = .FALSE.
	   RETURN
	END IF

	DO WHILE (ILEN.GE.0)
	   DO WHILE (ILEN.EQ.0)			! Read until line created
	      CALL GET_BULL(IBLOCK,BUFFER,ILEN)
	      IF (ILEN.LE.0) IBLOCK = IBLOCK + 1    ! Need to read new record.
	      IF (IBLOCK.GE.SBLOCK+BLENGTH) THEN    ! No more records.
	         IF (STRIP.AND..NOT.READ_HEAD.AND.NHEAD.GT.0) THEN
		     IBLOCK = SBLOCK
		     ILEN = LINE_LENGTH+1
		     CALL GET_BULL(IBLOCK,BUFFER,ILEN)
		     IF (ILEN.LE.0) IBLOCK = IBLOCK + 1
		     READS = 0
		     IF (SEEN_FROM) READS = READS + 1
		     IF (SEEN_SUBJ) READS = READS + 1
		     IF (MSG_SENT) READS = READS + 1
		     IF (READS.GT.0) THEN
		        DO I=1,READS
		           ILEN = 0
		           DO WHILE (ILEN.EQ.0)
			      CALL GET_BULL(IBLOCK,BUFFER,ILEN)
			      IF (ILEN.LE.0) IBLOCK = IBLOCK + 1
			   END DO
		        END DO
		     END IF
		     STRIP = .FALSE.
		 ELSE
		     RETURN
		 END IF
	      END IF
	   END DO

	   IF (STRIP.OR.FINDREF) THEN
	      IF (BULL_HEADER) THEN
		 IF (BUFFER(:5).EQ.'From:'.AND..NOT.SEEN_FROM) THEN
	            SEEN_FROM = .TRUE.
		    RETURN
		 ELSE IF (BUFFER(:5).EQ.'Subj:'.AND..NOT.SEEN_SUBJ) THEN
	            SEEN_SUBJ = .TRUE.
		    RETURN
		 ELSE IF (BUFFER(:13).EQ.'Message sent:') THEN
		    MSG_SENT = .TRUE.
		    RETURN
		 ELSE
		    BULL_HEADER = .FALSE.
		    IF (REMOTE_SET.EQ.4) FOUNDREF = .FALSE. 
		 END IF
	      END IF
              IF (REMOTE_SET.EQ.4.AND.ILEN.GT.12.AND.
     &			(BUFFER(1:11).EQ.'References:'.OR.
     &			 BUFFER(1:11).EQ.'Message-ID:')) THEN
	         IF (.NOT.FOUNDREF) LREF = 0
	         FOUNDREF = .TRUE.
		 IF (LREF.EQ.0) THEN
		    REFERENCES = BUFFER(13:ILEN)
		 ELSE
		    REFERENCES = REFERENCES(:LREF)//' '//
     &				BUFFER(13:ILEN)
		 END IF
		 LREF = TRIM(REFERENCES)
	      END IF
	      IF (STRIP) THEN
	         IF (DATE_LINE.NE.' ') DATE_LINE = ' '
	         CALL STRIP_HEADER(BUFFER,ILEN,STRIP)
	         IF (DATE_LINE.NE.' '.AND..NOT.MAIL_POST()) THEN
		    BUFFER = DATE_LINE
		    ILEN = TRIM(DATE_LINE)
		    MSG_SENT = .TRUE.
		    RETURN
	         END IF
	         IF (STRIP.OR.(.NOT.STRIP.AND.TRIM(BUFFER).EQ.0)) ILEN = 0
		 IF (STRIP.AND.BUFFER(:5).EQ.'From:') READ_HEAD = .TRUE.
	         IF (.NOT.STRIP.AND..NOT.READ_HEAD.AND.NHEAD.GT.0) THEN
		     IBLOCK = SBLOCK
		     ILEN = LINE_LENGTH+1
		     CALL GET_BULL(IBLOCK,BUFFER,ILEN)
		     IF (ILEN.LE.0) IBLOCK = IBLOCK + 1
		     READS = 0
		     IF (SEEN_FROM) READS = READS + 1
		     IF (SEEN_SUBJ) READS = READS + 1
		     IF (MSG_SENT) READS = READS + 1
		     IF (READS.GT.0) THEN
		        DO I=1,READS
		           ILEN = 0
		           DO WHILE (ILEN.EQ.0)
			      CALL GET_BULL(IBLOCK,BUFFER,ILEN)
			      IF (ILEN.LE.0) IBLOCK = IBLOCK + 1
			   END DO
		        END DO
		     END IF
		 END IF
	      ELSE
	         IF (.NOT.HEADER) THEN
		    CALL STRIP_HEADER(BUFFER,ILEN,STRIP)
		    STRIP = .FALSE.
	         END IF
	         IF (TRIM(BUFFER).EQ.0) THEN
		    FINDREF = .FALSE.
	            IF (.NOT.FOUNDREF) LREF = 0 
	         END IF
	         RETURN
	      END IF
	   ELSE
	      RETURN
	   END IF
	END DO

	RETURN

	ENTRY TEST_MORE_RECORDS(SBLOCK,BLENGTH,IREC)

	IREC = (SBLOCK+BLENGTH-1) - IBLOCK

	RETURN
	END


	SUBROUTINE GET_BULL(IBLOCK,BUFFER,OLEN)
C
C  SUBROUTINE GET_BULL
C
C  FUNCTION:  Outputs line from folder file.
C
C  INPUT:
C	IBLOCK	-	Input block number in input file to read from.
C
C  OUTPUT:
C	BUFFER  -	Character string containing output line.
C	OLEN	-	Length of character string.  If 0, signifies that
C			new record needs to be read, -1 signifies error.
C
C  NOTE:  Since message file is stored as a fixed length (128) record file,
C	  but message lines are variable, message lines may span one or
C	  more record.  This routine takes a record and outputs as many
C	  lines as it can from the record.  When no more lines can be
C	  outputted, it returns OLEN=0 requesting the calling program to
C	  increment the record counter.
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*256 INCMD

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /REMOTE_READ_MESSAGE/ SCRATCH_R1
	DATA SCRATCH_R1 /0/

	PARAMETER BRECLEN=128

	CHARACTER BUFFER*(*),TEMP*(BRECLEN), LEFT*(INPUT_LENGTH)

	DATA POINT /1/, LEFT_LEN /0/

	IF (OLEN.GT.LINE_LENGTH) THEN		! First read?
	   POINT = 1				! Initialize pointers.
	   LEFT_LEN = 0
	   DTYPE = 0
	END IF

	IF (POINT.EQ.1) THEN			! Need to read new line?
10	   IF (INCMD(:4).EQ.'MOVE'.OR.INCMD(:4).EQ.'COPY') THEN
	      DO WHILE (REC_LOCK(IER))		! Read from file
	         READ (11'IBLOCK,IOSTAT=IER) TEMP
	      END DO
	   ELSE IF (REMOTE_SET) THEN		! Remote folder?
	      IF (IBLOCK.EQ.BLOCK) SCRATCH_R = SCRATCH_R1	! Read lines
	      CALL READ_QUEUE(%VAL(SCRATCH_R),SCRATCH_R,TEMP)	! from queue
	      IER = 0
	   ELSE					! Local folder
	      DO WHILE (REC_LOCK(IER))		! Read from file
	         READ (1'IBLOCK,IOSTAT=IER) TEMP
	      END DO
	   END IF
	   IF (OLEN.GT.LINE_LENGTH.AND.IER.EQ.0
     &		.AND.ICHAR(TEMP(:1)).EQ.0) THEN
	      DTYPE = 1
	      POINT = POINT + 1
           END IF
	ELSE IF (POINT.EQ.BRECLEN+1) THEN	! Read all of line
	   OLEN = 0				! so indicate need to read
	   POINT = 1				! new line to calling routine.
	   RETURN
	END IF

	IF (IER.GT.0) THEN			! Error in reading file.
	   OLEN = -1				! OLEN = -1 signifies error
	   POINT = 1
	   LEFT_LEN = 0
	   RETURN
	END IF

	IF (LEFT_LEN.GT.0) THEN			! Part of line is left from
	   OLEN = ICHAR(LEFT(:1))		! previous record read.
	   IF (LEFT_LEN.LE.BRECLEN) THEN	! Rest of it is in next record.
 	      IF (DTYPE.EQ.0) THEN
	         BUFFER = LEFT(2:OLEN-LEFT_LEN+1)//
     &					TEMP(:LEFT_LEN) ! Output line.
	      ELSE
	         CALL UNCOMPRESS(LEFT(2:OLEN-LEFT_LEN+1)
     &				 //TEMP(:LEFT_LEN),BUFFER,OLEN)
 	      END IF
	      POINT = LEFT_LEN + 1		! Update pointers.
	      LEFT_LEN = 0
	   ELSE					! Rest of line is longer than
	      LEFT(OLEN-LEFT_LEN+2:) = TEMP	! a record, so store record
	      LEFT_LEN = LEFT_LEN - BRECLEN	! and request another read.
	      OLEN = 0				! Request new record read.
	   END IF
	ELSE					! Else nothing left over.
	   OLEN = ICHAR(TEMP(POINT:POINT))	! Get line length
	   IF (OLEN.GT.BRECLEN-POINT) THEN	! If it extends to next record
	      LEFT = TEMP(POINT:)		! Store it in leftover buffer
	      LEFT_LEN = OLEN - (BRECLEN-POINT)	! Store leftover length
	      OLEN = 0				! Request new record read
	      POINT = 1				! Update record pointer.
	   ELSE IF (OLEN.EQ.0) THEN		! Empty line signifies
	      POINT = 1				! end of message.
	   ELSE					! Else message line fully read
	      ILEN = OLEN
	      IF (DTYPE.EQ.0) THEN
	         BUFFER = TEMP(POINT+1:POINT+ILEN)	! So output it
	         OLEN = OLEN
	      ELSE
	         CALL UNCOMPRESS(TEMP(POINT+1:POINT+ILEN),BUFFER,OLEN)
	      END IF
	      POINT = POINT+ILEN+1		! and update pointer.
	   END IF
	END IF

	RETURN

	ENTRY TEST_MORE_LINES(OLEN)	! Test for more lines in record.
					! Returns length of next line.
	IF (POINT.EQ.BRECLEN+1) THEN		! If pointer greater than
	   OLEN = 0				! record, no more lines.
	ELSE					! Else there is another line.
	   OLEN = ICHAR(TEMP(POINT:POINT))	! Output it's length.
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

	CALL DUMP_MESSAGE()

	CALL READDIR(BULL_ENTRY,IER)
	DELETE(UNIT=2)

	NEMPTY = NEMPTY + LENGTH

	CALL WRITEDIR(0,IER)

	RETURN
	END


	SUBROUTINE DUMP_MESSAGE()
C
C  SUBROUTINE DUMP_MESSAGE
C
C  FUNCTION:
C	To delete a directory entry.
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFILES.INC'

	INCLUDE '($ACLDEF)'

	INCLUDE '($SSDEF)'

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /ACL/ ACLENT
	CHARACTER ACLENT*256

	CHARACTER DUMP_FILE*80

	IF (BTEST(FOLDER_FLAG,1)) THEN
	   DUMP_FILE = FOLDER_FILE
           IF (REMOTE_SET.EQ.4) THEN
	      DUMP_FILE = FOLDER_DESCRIP(:INDEX(FOLDER_DESCRIP,' ')-1)
	      DO I=1,TRIM(DUMP_FILE)
                 IF (DUMP_FILE(I:I).EQ.'.') DUMP_FILE(I:I) = '_'
	      END DO	
	      DUMP_FILE = NEWS_DIRECTORY(:TRIM(NEWS_DIRECTORY))//
     &				DUMP_FILE
	   END IF
	   OPEN(UNIT=3,FILE=DUMP_FILE(:TRIM(DUMP_FILE))//'.LOG',
     &		IOSTAT=IER,STATUS='OLD',
     &		RECL=LINE_LENGTH,CARRIAGECONTROL='LIST',ACCESS='APPEND')
	   IF (IER.NE.0) THEN
	      OPEN(UNIT=3,FILE=DUMP_FILE(:TRIM(DUMP_FILE))//'.LOG',
     &		IOSTAT=IER,
     &		RECL=LINE_LENGTH,STATUS='NEW',CARRIAGECONTROL='LIST')
	      IF (IER.NE.0) RETURN

	      IER = SYS$PARSE_ACL('(IDENTIFIER='//FOLDER_OWNER(
     &		:TRIM(FOLDER_OWNER))//',ACCESS=R+W+E+D+C)',ACLENT,,)
       	      IF (IER) THEN
		 CALL INIT_ITMLST	! Initialize item list
		 CALL ADD_2_ITMLST(ICHAR(ACLENT(:1)),ACL$C_ADDACLENT,
     &			%LOC(ACLENT))
		 CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist

		 IER = SYS$CHANGE_ACL(,ACL$C_FILE,DUMP_FILE(:TRIM(
     &		   DUMP_FILE))//'.LOG',%VAL(ACL_ITMLST),,,)
              END IF
	   ELSE
	      WRITE (3,'(A)') CHAR(12)
	   END IF

	   CALL OPEN_BULLFIL

	   ILEN = LINE_LENGTH + 1

	   CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'From: ') THEN
	      WRITE(3,1060) INPUT(7:ILEN),DATE//' '//TIME(:8)
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	   ELSE
	      WRITE(3,1060) FROM,DATE//' '//TIME(:8)
	   END IF
	   IF (ILEN.GT.0.AND.INPUT(:6).EQ.'Subj: ') THEN
	      WRITE(3,1050) INPUT(7:MIN(ILEN,LINE_LENGTH-3))
	   ELSE
	      WRITE(3,1050) DESCRIP
	      IF (ILEN.GT.0) WRITE (3,'(A)') INPUT(:ILEN)
	   END IF

	   DO WHILE (ILEN.GT.0)
	      CALL GET_BULL_LINE(BLOCK,LENGTH,INPUT,ILEN)
	      IF (ILEN.GT.0) WRITE (3,'(A)') INPUT(:ILEN)
	   END DO

	   CLOSE (UNIT=3)			! Bulletin copy completed

	   CALL CLOSE_BULLFIL
	END IF

1050	FORMAT('Subject: ',A,/)
1060	FORMAT(/,'From: ',A,' Date: ',A11)

	RETURN
	END



	SUBROUTINE GET_EXDATE(EXDATE,NDAYS)
C
C  SUBROUTINE GET_EXDATE
C
C  FUNCTION:  Computes expiration date giving number of days to expire.
C
	IMPLICIT INTEGER (A-Z)

	CHARACTER*12 EXDATE

	CHARACTER*3 MONTHS(12)
	DIMENSION LENGTH(12)
	DATA MONTHS/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     &		    'OCT','NOV','DEC'/
	DATA LENGTH/31,27,31,30,31,30,31,31,30,31,30,31/

	CALL SYS$ASCTIM(,EXDATE,,)		! Get the present date

	DECODE(2,'(I2)',EXDATE(:2)) DAY	! Get day
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

	CALL OTS$CVT_L_TI (DAY,EXDATE(:2),%VAL(2))
	IF (EXDATE(1:1).EQ.'0') EXDATE(1:1) = ' '
	CALL OTS$CVT_L_TI (YEAR,EXDATE(8:11),%VAL(4))

C	ENCODE(2,'(I2)',EXDATE(:2)) DAY	! Put day into new date
C  	ENCODE(4,'(I4)',EXDATE(8:11)) YEAR	! Put year into new date
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
C	Also, on first call, set LEN_INPUT to 1+LENGTH OF INPUT CHARCTER
C	for initializing the CTRLC AST.
C

	IMPLICIT INTEGER (A-Z)

	LOGICAL*1 DESCRIP(8),DTYPE,CLASS
	INTEGER*2 LENGTH
	CHARACTER*(*) INPUT
	EQUIVALENCE (DESCRIP(1),LENGTH),(DESCRIP(3),DTYPE)
	EQUIVALENCE (DESCRIP(4),CLASS),(DESCRIP(5),POINTER)

        DATA LENGTH/0/,DTYPE/0/,CLASS/2/,POINTER/0/

	EXTERNAL SMG$_EOF

	COMMON /DECNET/ DECNET_PROC,ERROR_UNIT
	LOGICAL DECNET_PROC

	COMMON /SMG/ KEYBOARD_ID,KEY_TABLE_ID

	COMMON /CTRLC_FLAG/ FLAG

	CHARACTER PROMPT*(*),NULLPROMPT*4
	LOGICAL USE_PROMPT

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

	CALL DECLARE_CTRLC_AST

	LEN_INPUT = 0				! Nothing inputted yet

C
C  LIB$GET_INPUT is nice way of getting input from terminal,
C  as it handles such thing as accidental wrap around to next line.
C

	IF (DECNET_PROC) THEN
	   READ (5,'(Q,A)',IOSTAT=IER) LEN_INPUT,INPUT
	   IF (IER.NE.0) LEN_INPUT = -2 
	   RETURN
	ELSE IF (USE_PROMPT) THEN
	   IER = SMG$READ_COMPOSED_LINE(KEYBOARD_ID,KEY_TABLE_ID,
     &		DESCRIP,PROMPT)		! Get line from terminal with prompt
	ELSE
	   IER = SMG$READ_COMPOSED_LINE(KEYBOARD_ID,KEY_TABLE_ID,
     &		DESCRIP,NULLPROMPT(:1))	! Get line from terminal with no prompt
	END IF

	IF (.NOT.IER.AND.IER.NE.%LOC(SMG$_EOF)) CALL EXIT(IER)

	CALL STR$TRIM(DESCRIP,DESCRIP,LEN_INPUT)

	IF (FLAG.EQ.0) THEN			! If no CTRL-C has occurred
	   CALL CANCEL_CTRLC_AST		! Cancel CTRL-C AST
	   IF (IER.NE.%LOC(SMG$_EOF)) THEN	! End of input?
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

        CALL FILTER (INPUT, LEN_INPUT)

	RETURN
	END


	SUBROUTINE FILTER (INCHAR, LENGTH)

	IMPLICIT INTEGER (A-Z)

	CHARACTER*(*) INCHAR

	DO I = 1,LENGTH
	   IF ((INCHAR(I:I).LT.' '.AND.
     &      INCHAR(I:I).NE.CHAR(13).AND.INCHAR(I:I).NE.CHAR(10)))
     &	    INCHAR(I:I) = '.'
	END DO

	RETURN
	END


	SUBROUTINE GET_VAL(OUTPUT,INPUT)	! Used to convert logical
	CHARACTER*(*) OUTPUT			! byte to character value
	LOGICAL*1 INPUT
	OUTPUT = CHAR(INPUT)
	RETURN
	END

	SUBROUTINE CTRLC_ROUTINE		! CTRL-C AST routine
	IMPLICIT INTEGER (A-Z)			! If CTRL-C, come here

	COMMON /CTRLY/ CTRLY

	COMMON /CTRLC_FLAG/ FLAG

	COMMON /DEF_PROT/ ORIGINAL_DEF_PROT

	IF (FLAG.EQ.2) THEN
	   CALL LIB$PUT_OUTPUT('Bulletin aborting...')
	   CALL SYS$CANEXH()
	   CALL SYS$SETDFPROT(ORIGINAL_DEF_PROT,)
	   CALL LIB$ENABLE_CTRL(CTRLY,)		! Enable CTRL-Y & -C
	   CALL EXIT
	END IF
	FLAG = 1				! to set flag
	RETURN
	END



	SUBROUTINE DECLARE_CTRLC_AST
C
C  SUBROUTINE DECLARE_CTRLC_AST
C
C  FUNCTION:
C	Declares a CTRLC ast.
C  NOTES:
C	Assumes terminal assigned to TERM_CHAN in common /TERM_CHAN/.
C
	IMPLICIT INTEGER (A-Z)

	EXTERNAL IO$_SETMODE,IO$M_CTRLCAST,CTRLC_ROUTINE
	COMMON /TERM_CHAN/ TERM_CHAN

	COMMON /CTRLC_FLAG/ FLAG

	FLAG = 0				! Init CTRL-C flag
	IO_CTRLC = %LOC(IO$_SETMODE)+%LOC(IO$M_CTRLCAST)	! Set AST code
	IER=SYS$QIOW(,%VAL(TERM_CHAN),%VAL(IO_CTRLC),,,,	! for QIO
     &	      CTRLC_ROUTINE,,,,,)		! Enable the AST

	RETURN

	ENTRY CANCEL_CTRLC_AST

	IER = SYS$CANCEL(%VAL(TERM_CHAN))

	FLAG = 2		! Indicates that a CTRLC will cause an exit
	IER=SYS$QIOW(,%VAL(TERM_CHAN),%VAL(IO_CTRLC),,,,	! for QIO
     &	      CTRLC_ROUTINE,,,,,)		! Enable the AST

	RETURN
	END




	SUBROUTINE GET_INPUT_NOECHO(DATA)
C
C  SUBROUTINE GET_INPUT_NOECHO
C
C  FUNCTION: Reads data in from terminal without echoing characters.
C	     Also contains entry to assign terminal.
C
	IMPLICIT INTEGER (A-Z)

	CHARACTER*(*) DATA,PROMPT

	COMMON /TERM_CHAN/ TERM_CHAN

	COMMON /SMG/ KEYBOARD_ID,KEY_TABLE_ID

	COMMON /CTRLC_FLAG/ FLAG

	COMMON /READIT/ READIT

	INCLUDE '($TRMDEF)'

	INTEGER TERMSET(2)

	INTEGER MASK(4)
	DATA MASK/4*'FFFFFFFF'X/

	DATA PURGE/.TRUE./

	DO I=1,LEN(DATA)
	   DATA(I:I) = ' '
	END DO

	IF (PURGE) THEN
	   CALL SMG$READ_STRING(KEYBOARD_ID,DATA,,LEN(DATA),
     &		TRM$M_TM_NOECHO.OR.TRM$M_TM_PURGE)
	   PURGE = .FALSE.
	ELSE
	   CALL SMG$READ_STRING(KEYBOARD_ID,DATA,,LEN(DATA),
     &		TRM$M_TM_NOECHO)
	END IF

	RETURN

	ENTRY GET_INPUT_NOECHO_PROMPT(DATA,PROMPT)

	DO I=1,LEN(DATA)
	   DATA(I:I) = ' '
	END DO

	IF (PURGE) THEN
	   CALL SMG$READ_STRING(KEYBOARD_ID,DATA,PROMPT,LEN(DATA),
     &		TRM$M_TM_NOECHO.OR.TRM$M_TM_PURGE)
	   PURGE = .FALSE.
	ELSE
	   CALL SMG$READ_STRING(KEYBOARD_ID,DATA,PROMPT,LEN(DATA),
     &		TRM$M_TM_NOECHO)
	END IF

	RETURN

	ENTRY GET_INPUT_NUM(DATA,NLEN)

	DO I=1,LEN(DATA)
	   DATA(I:I) = ' '
	END DO

	IF (PURGE) THEN
	   CALL SMG$READ_STRING(KEYBOARD_ID,DATA,,LEN(DATA),
     &		TRM$M_TM_PURGE,,TERMSET,NLEN,TERM)
	   PURGE = .FALSE.
	ELSE
	   CALL SMG$READ_STRING(KEYBOARD_ID,DATA,,LEN(DATA),,,
     &		TERMSET,NLEN,TERM)
	END IF

	IF (TERM.NE.13.AND.TERM.NE.510.AND.NLEN.EQ.0) THEN
				! Input did not end with CR or buffer full
	   NLEN = 1
	   DATA(:1) = CHAR(TERM)
	END IF

	RETURN

	ENTRY ASSIGN_TERMINAL

	IER = SYS$ASSIGN('TT',TERM_CHAN,,)	! Assign terminal

	CALL DECLARE_CTRLC_AST

	FLAG = 2		! Indicates that a CTRLC will cause an exit

	IER = SMG$CREATE_VIRTUAL_KEYBOARD(KEYBOARD_ID,,,,20)

	IER = SMG$CREATE_KEY_TABLE(KEY_TABLE_ID)

	IER = SMG$SET_KEYPAD_MODE(KEYBOARD_ID,0)

	IF (CLI$PRESENT('KEYPAD')) THEN
	   CALL SET_KEYPAD
	ELSE IF (READIT.EQ.0) THEN
	   CALL SET_NOKEYPAD
	END IF

	TERMSET(1) = 16
	TERMSET(2) = %LOC(MASK)

	DO I=ICHAR('0'),ICHAR('9')
	   MASK(2) = IBCLR(MASK(2),I-32)
	END DO

	RETURN
	END





	SUBROUTINE GETPAGSIZ(PAGE_LENGTH,PAGE_WIDTH)
C
C  SUBROUTINE GETPAGSIZ
C
C  FUNCTION:
C	Gets page size of the terminal.
C
C  OUTPUTS:
C	PAGE_LENGTH  -  Page length of the terminal.
C	PAGE_WIDTH   -  Page size of the terminal.
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE '($DVIDEF)'

	LOGICAL*1 DEVDEPEND(4)

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(4,DVI$_DEVDEPEND,%LOC(DEVDEPEND(1)))
	CALL ADD_2_ITMLST(4,DVI$_DEVBUFSIZ,%LOC(PAGE_WIDTH))
	CALL END_ITMLST(GETDVI_ITMLST)		! Get address of itemlist

	CALL SYS$GETDVIW(,,'TT',%VAL(GETDVI_ITMLST),,,,)

	PAGE_LENGTH = ZEXT(DEVDEPEND(4))

	PAGE_WIDTH = MIN(PAGE_WIDTH,132)

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

	IF (IOSB(3).LE.TT$C_BAUD_2400.AND.IOSB(3).NE.0) THEN
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

	INCLUDE 'BULLFILES.INC'

	INCLUDE '($PRVDEF)'

	INCLUDE '($SSDEF)'

	COMMON /PRVDEF/ PRIVS
	CHARACTER*8 PRIVS(0:38)

	CALL OPEN_BULLUSER_SHARED		! Get BULLUSER.DAT file

	CALL READ_USER_FILE_HEADER(IER)

	IF (IER.EQ.0) THEN			! If header is present, exit
	   IF (NEW_FLAG(1).EQ.-1.AND.NEW_FLAG(2).EQ.-1) THEN  ! Info not present
	      CALL CLOSE_BULLUSER
	      CALL OPEN_BULLUSER			! Get BULLUSER.DAT file
	      CALL READ_USER_FILE_HEADER(IER)
	      USERPRIV(1) = PRV$M_OPER.OR.PRV$M_CMKRNL.OR.PRV$M_SETPRV
	      USERPRIV(2) = 0
	      REWRITE (4) USER_HEADER
	   END IF
	   WRITE (6,'('' Following privileges are needed for privileged
     & commands:'')')
	   DO I=0,38
	      IF ((I.LT.32.AND.BTEST(USERPRIV(1),I)).OR.
     &		  (I.GT.31.AND.BTEST(USERPRIV(2),I-32))) THEN
		 WRITE (6,'(1X,A)') PRIVS(I)
	      END IF
	   END DO
	ELSE
	   WRITE (6,'('' ERROR: Cannot show privileges.'')')
	END IF

	CALL CLOSE_BULLUSER			! All finished with BULLUSER

	CALL CHKACL(BULLUSER_FILE(:TRIM(BULLUSER_FILE)),IER)
	IF (IER.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL).AND.IER) THEN
	   CALL SHOWACL(BULLUSER_FILE(:TRIM(BULLUSER_FILE)))
	END IF

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

	EXTERNAL CLI$_ABSENT,CLI$_NEGATED

	DIMENSION ONPRIV(2),OFFPRIV(2)

	CHARACTER*32 INPUT_PRIV

	IF (.NOT.SETPRV_PRIV().OR..NOT.BTEST(PROCPRIV(1),PRV$V_SETPRV)) THEN
	   WRITE (6,'('' ERROR: This command requires SETPRV privileges.'')')
	   RETURN
	END IF

	IF (CLI$PRESENT('ID').OR.
     &		CLI$PRESENT('ID').EQ.%LOC(CLI$_NEGATED)) THEN
	   DO WHILE (CLI$GET_VALUE('PRIVILEGES',INPUT_PRIV,PLEN)
     &	       .NE.%LOC(CLI$_ABSENT))		! Get the IDs
	      IF (CLI$PRESENT('ID')) THEN
		 CALL ADD_ACL(INPUT_PRIV(:PLEN),'R+C',IER)
	      ELSE
		 CALL DEL_ACL(INPUT_PRIV(:PLEN),'R+C',IER)
	      END IF
	      IF (.NOT.IER) CALL SYS_GETMSG(IER)
	   END DO
	   RETURN
	END IF

	OFFPRIV(1) = 0
	OFFPRIV(2) = 0
	ONPRIV(1) = 0
	ONPRIV(2) = 0

	DO WHILE (CLI$GET_VALUE('PRIVILEGES',INPUT_PRIV,PLEN)
     &	    .NE.%LOC(CLI$_ABSENT))		! Get the privileges
	   PRIV_FOUND = -1
	   I = 0
	   DO WHILE (I.LT.39.AND.PRIV_FOUND.EQ.-1)
	      IF (INPUT_PRIV(:PLEN).EQ.PRIVS(I)) PRIV_FOUND = I
	      IF (INPUT_PRIV(3:PLEN).EQ.PRIVS(I)) PRIV_FOUND = I
	      I = I + 1
	   END DO
	   IF (PRIV_FOUND.EQ.-1) THEN
	      WRITE(6,'('' ERROR: Incorrectly specified privilege = '',
     &		A)') INPUT_PRIV(:PLEN)
	      RETURN
	   ELSE IF (INPUT_PRIV(:2).EQ.'NO') THEN
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

	CALL OPEN_BULLUSER		! Get BULLUSER.DAT file

	CALL READ_USER_FILE_HEADER(IER)

	IF (IER.EQ.0) THEN			! If header is present, exit
	   USERPRIV(1) = USERPRIV(1).OR.ONPRIV(1)
	   USERPRIV(2) = USERPRIV(2).OR.ONPRIV(2)
	   USERPRIV(1) = USERPRIV(1).AND.(.NOT.OFFPRIV(1))
	   USERPRIV(2) = USERPRIV(2).AND.(.NOT.OFFPRIV(2))
	   REWRITE (4) USER_HEADER
	   WRITE (6,'('' Privileges successfully modified.'')')
	ELSE
	   WRITE (6,'('' ERROR: Cannot modify privileges.'')')
	END IF

	CALL CLOSE_BULLUSER			! All finished with BULLUSER

	RETURN

	END



	SUBROUTINE ADD_ACL(ID,ACCESS,IER)
C
C  SUBROUTINE ADD_ACL
C
C  FUNCTION: Adds ACL to bulletin files.
C
C  PARAMETERS:
C	ID - Character string containing identifier to add to ACL.
C	ACCESS - Character string containing access controls to give to ID.
C	IER - Return error from attempting to set ACL.
C
C  NOTE: The ID must be in the RIGHTS data base.
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLFILES.INC'

	COMMON /ACL/ ACLENT
	CHARACTER ACLENT*256

	CHARACTER ID*(*),ACCESS*(*),NEWS_ACCESS*132

	INCLUDE '($ACLDEF)'

	INCLUDE '($SSDEF)'

	IER = SYS$PARSE_ACL('(IDENTIFIER='//ID//',ACCESS='
     &	   //ACCESS//')',ACLENT,,)
	IF (.NOT.IER) THEN
	   IF (IER.EQ.SS$_NOSUCHID.AND.ADDID.AND.
     &				INDEX(ACCESS,'C').EQ.0) THEN
	      CALL GET_UAF(ID,USER,GROUP,ACCOUNT,FLAGS,IER)
	      IF (.NOT.IER) THEN
		 CALL ERRSNS(IDUMMY,IER)
		 WRITE (6,'(
     &		    '' ERROR: Specified username cannot be verified.'')')
		 CALL SYS_GETMSG(IER)
	         RETURN
	      END IF
	      IDENT = USER + ISHFT(GROUP,16)
	      IER = SYS$ADD_IDENT(ID,%VAL(IDENT),,)
	      IF (IER) THEN
	         IER = SYS$PARSE_ACL('(IDENTIFIER='//ID//',ACCESS='
     &	           //ACCESS//')',ACLENT,,)
	      END IF
	   END IF
	END IF
	IF (.NOT.IER) RETURN

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(ICHAR(ACLENT(:1)),ACL$C_ADDACLENT,%LOC(ACLENT))
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist

	IF (INDEX(ACCESS,'C').GT.0.AND.INDEX(ACCESS,'W').EQ.0) THEN
	   IER = SYS$CHANGE_ACL(,ACL$C_FILE,BULLUSER_FILE(:TRIM(
     &		   BULLUSER_FILE)),%VAL(ACL_ITMLST),,,)
	   RETURN
	END IF

	IF (INDEX(FOLDER1,'.').GT.0) THEN
  	   IER = SYS$CHANGE_ACL(,ACL$C_FILE,NEWS_ACCESS(FOLDER1_DESCRIP)
     &	      ,%VAL(ACL_ITMLST),,,)
        ELSE
  	   IER = SYS$CHANGE_ACL(,ACL$C_FILE,FOLDER1_FILE
     &	      (:TRIM(FOLDER1_FILE))//'.BULLFIL',%VAL(ACL_ITMLST),,,)
	END IF

	RETURN
	END



	SUBROUTINE DEL_ACL(ID,ACCESS,IER)
C
C  SUBROUTINE DEL_ACL
C
C  FUNCTION: Adds ACL to bulletin files.
C
C  PARAMETERS:
C	ID - Character string containing identifier to add to ACL.
C	ACCESS - Character string containing access controls to give to ID.
C	IER - Return error from attempting to set ACL.
C
C  NOTE: The ID must be in the RIGHTS data base.
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLFILES.INC'

	COMMON /ACL/ ACLENT
	CHARACTER ACLENT*256

	CHARACTER ID*(*),ACCESS*(*),NEWS_ACCESS*132

	INCLUDE '($ACLDEF)'

	IF (ID.NE.' ') THEN
	   IER = SYS$PARSE_ACL('(IDENTIFIER='//ID//',ACCESS='
     &	      //ACCESS//')',ACLENT,,)
	   IF (.NOT.IER) RETURN

	   CALL INIT_ITMLST	! Initialize item list
	   CALL ADD_2_ITMLST(ICHAR(ACLENT(:1)),ACL$C_DELACLENT,%LOC(ACLENT))
	   CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist
	ELSE
	   CALL INIT_ITMLST	! Initialize item list
	   CALL ADD_2_ITMLST(255,ACL$C_DELETEACL,%LOC(ACLENT))
	   CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist
	END IF

	IF (INDEX(ACCESS,'C').GT.0) THEN
	   IER = SYS$CHANGE_ACL(,ACL$C_FILE,BULLUSER_FILE(:TRIM(
     &		   BULLUSER_FILE)),%VAL(ACL_ITMLST),,,)
	   RETURN
	END IF

	IF (INDEX(FOLDER1,'.').GT.0) THEN
  	   IER = SYS$CHANGE_ACL(,ACL$C_FILE,NEWS_ACCESS(FOLDER1_DESCRIP)
     &	      ,%VAL(ACL_ITMLST),,,)
        ELSE
  	   IER = SYS$CHANGE_ACL(,ACL$C_FILE,FOLDER1_FILE
     &	      (:TRIM(FOLDER1_FILE))//'.BULLFIL',%VAL(ACL_ITMLST),,,)
	END IF

	RETURN
	END




	SUBROUTINE CREATE_FOLDER
C
C  SUBROUTINE CREATE_FOLDER
C
C  FUNCTION: Creates a new bulletin folder.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFILES.INC'

	INCLUDE 'BULLDIR.INC'

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
	DATA REMOTE_SET /.FALSE./

	COMMON /BULL_CUSTOM/ BULL_CUSTOM

	EXTERNAL CLI$_ABSENT,CLI$_NEGATED

	DIMENSION LAST(2,FOLDER_MAX)
	INTEGER*2 LAST2(4,FOLDER_MAX)
	EQUIVALENCE (LAST,LAST2)

	CHARACTER RESPONSE*4

	IF (CLI$PRESENT('NEWS')) THEN
	   CALL CREATE_NEWS_FOLDER
	   RETURN
	END IF

	IF (.NOT.SETPRV_PRIV().AND.(CLI$PRESENT('NEEDPRIV').OR.
     &		BTEST(BULL_CUSTOM,0))) THEN
	   WRITE(6,'('' ERROR: CREATE is a privileged command.'')')
	   RETURN
	END IF

	IER = CLI$GET_VALUE('CREATE_FOLDER',FOLDER,LEN_T) ! Get folder name
	CALL STR$UPCASE(FOLDER,FOLDER)

	IF (LEN_T.GT.44) THEN
	   WRITE(6,'('' ERROR: Folder name must be < 45 characters.'')')
	   RETURN
	END IF

	IF (.NOT.SETPRV_PRIV().AND.(CLI$PRESENT('ALWAYS').OR.
     &	     CLI$PRESENT('NOTIFY').OR.CLI$PRESENT('READNEW').OR.
     &	     CLI$PRESENT('BRIEF').OR.CLI$PRESENT('SYSTEM'))) THEN
	   WRITE (6,'('' ERROR: Privileged qualifier specified.'')')
	   RETURN
	END IF

	IF (CLI$PRESENT('NODE')) THEN	! Remote node specified?
	   IER = CLI$GET_VALUE('NODE',FOLDER_BBOARD,LEN_B) ! Get node name
	   FOLDER_BBOARD = '::'//FOLDER_BBOARD(:LEN_B)
	   FOLDER1_BBOARD = FOLDER_BBOARD
	   IF (.NOT.CLI$GET_VALUE('REMOTENAME',FOLDER1,LEN_P)) THEN
	      FOLDER1 = FOLDER
	   ELSE IF (LEN_P.GT.40) THEN
	      WRITE (6,'('' ERROR: REMOTENAME cannot be longer '',
     &			 ''than 40 characters.'')')
	   END IF
	   FOLDER1_NUMBER = FOLDER_MAX
	   CALL CONNECT_REMOTE_FOLDER(READ_ONLY,IER)
	   IF (IER.NE.0) THEN
	    WRITE (6,'('' ERROR: Folder not accessible on remote node.'')')
	    RETURN
	   ELSE IF (CLI$PRESENT('SYSTEM').AND.
     &				.NOT.BTEST(FOLDER1_FLAG,2)) THEN
	    WRITE (6,'('' ERROR: /SYSTEM not allowed as remote node'',
     &			'' is not SYSTEM folder.'')')
	    RETURN
	   END IF
	END IF

	LENDES = 0
	DO WHILE (LENDES.EQ.0)
	   IF (CLI$PRESENT('DESCRIPTION')) THEN		! DESCRIPTION specified?
	      IER = CLI$GET_VALUE('DESCRIPTION',FOLDER_DESCRIP,LENDES)
	   ELSE
	      WRITE (6,'('' Enter one line description of folder.'')')
	      CALL GET_LINE(FOLDER_DESCRIP,LENDES)	! Get input line
	      FOLDER_DESCRIP = FOLDER_DESCRIP(:LENDES)	! End fill with spaces
	   END IF
	   IF (LENDES.LE.0) THEN
	      WRITE (6,'('' Aborting folder creation.'')')
	      RETURN
	   ELSE IF (LENDES.GT.80) THEN		! If too many characters
	      WRITE(6,'('' ERROR: folder must be < 80 characters.'')')
	      RETURN
	   END IF
	END DO

	CALL OPEN_BULLFOLDER		! Open folder file
	READ (7,IOSTAT=IER,KEY=FOLDER,KEYID=0)
					! See if folder exists

	IF (IER.EQ.0) THEN
	   WRITE (6,'('' ERROR: Specified folder already exists.'')')
	   GO TO 1000
	END IF

	IF (CLI$PRESENT('OWNER')) THEN
	   IF (.NOT.SETPRV_PRIV().AND..NOT.CLI$PRESENT('ID')) THEN
	      WRITE (6,'('' ERROR: /OWNER requires privileges.'')')
	      CALL CLOSE_BULLFOLDER
	      RETURN
	   ELSE
	      CALL CLI$GET_VALUE('OWNER',FOLDER1_OWNER,LEN_P)
	      IF (LEN_P.GT.12) THEN
	         WRITE (6,'('' ERROR: Folder owner name must be'',
     &		        '' no more than 12 characters long.'')')
	         CALL CLOSE_BULLFOLDER
	         RETURN
	      ELSE IF (CLI$PRESENT('ID')) THEN
		 IER = CHKPRO(FOLDER1_OWNER)
	         IF (.NOT.IER) THEN
	            WRITE (6,'('' ERROR: ID not valid.'')')
	            CALL CLOSE_BULLFOLDER
	            RETURN
		 END IF
	      ELSE
	         CALL GET_UAF
     &		   (FOLDER1_OWNER,USERB1,GROUPB1,ACCOUNTB1,FLAGS,IER)
	         IF (.NOT.IER) THEN
	            WRITE (6,'('' ERROR: Owner not valid username.'')')
	            CALL CLOSE_BULLFOLDER
	            RETURN
		 END IF
	      END IF
	      FOLDER_OWNER = FOLDER1_OWNER
	   END IF
	ELSE
	   FOLDER_OWNER = USERNAME		! Get present username
	   FOLDER1_OWNER = FOLDER_OWNER		! Save for later
	END IF

	FOLDER_SET = .TRUE.

	CALL SYS$SETDFPROT('FF00'X,CUR_DEF_PROT)
			! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD,GROUP)

C
C  Folder file is placed in the directory FOLDER_DIRECTORY.
C  The file prefix is the name of the folder.
C

	FD_LEN = TRIM(FOLDER_DIRECTORY)
	IF (FD_LEN.EQ.0) THEN
	 WRITE (6,'('' ERROR: System programmer has disabled folders.'')')
	 GO TO 910
	ELSE
	 FOLDER_FILE = FOLDER_DIRECTORY(:FD_LEN)//FOLDER
	END IF

	OPEN (UNIT=2,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     &	      //'.BULLDIR',STATUS='NEW',FORM='UNFORMATTED',
     &	      RECORDTYPE='FIXED',RECORDSIZE=DIR_RECORD_LENGTH/4,
     &	      ORGANIZATION='INDEXED',IOSTAT=IER,DISPOSE='KEEP',
     &	      KEY=(9:12:INTEGER,1:8:CHARACTER),ACCESS='KEYED')

	IF (IER.NE.0) THEN
	   WRITE(6,'('' ERROR: Cannot create folder directory file.'')')
	   CALL ERRSNS(IDUMMY,IER)
	   CALL SYS_GETMSG(IER)
	   GO TO 910
	END IF

	OPEN (UNIT=1,FILE=FOLDER_FILE(:TRIM(FOLDER_FILE))
     1	 //'.BULLFIL',STATUS='NEW',
     1	 ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=32,
     1	 FORM='UNFORMATTED',IOSTAT=IER)

	IF (IER.NE.0) THEN
	   WRITE(6,'('' ERROR: Cannot create folder message file.'')')
	   CALL ERRSNS(IDUMMY,IER)
	   CALL SYS_GETMSG(IER)
	   GO TO 910
	END IF

	FOLDER_FLAG = 0

	IF (CLI$PRESENT('PRIVATE').OR.CLI$PRESENT('SEMIPRIVATE')) THEN
				! Will folder have access limitations?
	   FOLDER1_FILE = FOLDER_FILE
	   CLOSE (UNIT=1)
	   CLOSE (UNIT=2)
	   FOLDER1 = FOLDER	! Save for ADD_ACL
	   IF (CLI$PRESENT('SEMIPRIVATE')) THEN
	      CALL ADD_ACL('*','R',IER)
	   ELSE
	      CALL ADD_ACL('*','NONE',IER)
	   END IF
	   CALL ADD_ACL(FOLDER_OWNER,'R+W+C',IER)
	   OPEN (UNIT=2,FILE=FOLDER_FILE(:TRIM(FOLDER_FILE))
     1	    //'.BULLDIR',STATUS='OLD',IOSTAT=IER1)
	   OPEN (UNIT=1,FILE=FOLDER_FILE(:TRIM(FOLDER_FILE))
     1	    //'.BULLFIL',STATUS='OLD',IOSTAT=IER1)
	   IF (.NOT.IER) THEN
	      WRITE(6,
     &	      '('' ERROR: Cannot create private folder using ACLs.'')')
	      CALL SYS_GETMSG(IER)
	      GO TO 910
	   END IF
	   FOLDER_FLAG = IBSET(FOLDER_FLAG,0)
	END IF

	IER = 0
	LAST_NUMBER = 1
	DO WHILE (IER.EQ.0.AND.LAST_NUMBER.LT.FOLDER_MAX-1)
	   READ (7,IOSTAT=IER,KEY=LAST_NUMBER,KEYID=1)
	   LAST_NUMBER = LAST_NUMBER + 1
	END DO

	IF (IER.EQ.0) THEN
	 WRITE (6,'('' ERROR: Folder limit of '',I,'' has been reached.'')')
     &			FOLDER_MAX
	 WRITE (6,'('' Unable to add specified folder.'')')
	 GO TO 910
	ELSE
	   FOLDER1_NUMBER = LAST_NUMBER - 1
	END IF

	IF (.NOT.CLI$PRESENT('NODE')) THEN
	   FOLDER_BBOARD = 'NONE'
	   IF (REMOTE_SET) CLOSE (UNIT=REMOTE_UNIT)
	   REMOTE_SET = .FALSE.
	   FOLDER_BBEXPIRE = 14
	   F_NBULL = 0
	   NBULL = 0
	   F_NEWEST_BTIM(1) = 0
	   F_NEWEST_BTIM(2) = 0
	   F_NEWEST_NOSYS_BTIM(1) = 0
	   F_NEWEST_NOSYS_BTIM(2) = 0
	   F_EXPIRE_LIMIT = 0
	   FOLDER_NUMBER = FOLDER1_NUMBER
	ELSE
	   CLOSE (UNIT=1,STATUS='DELETE')
	   CLOSE (UNIT=2,STATUS='DELETE')
	   IF (FOLDER1.NE.FOLDER) THEN	! Different remote folder name?
	      REMOTE_SET = .FALSE.
    	      CALL OPEN_BULLDIR		! If so, store name in directory file
	      BULLDIR_HEADER(13:) = FOLDER1
	      CALL WRITEDIR_NOCONV(0,IER)
	      CALL CLOSE_BULLDIR
	      FOLDER1_BBOARD = FOLDER1_BBOARD(:LEN_B+2)//'*'
	      FOLDER1 = FOLDER
	   END IF
	   REMOTE_SET = .TRUE.
	   IF (BTEST(FOLDER1_FLAG,0)) FOLDER_FLAG = IBSET(FOLDER_FLAG,0)
	   FOLDER1_FLAG = FOLDER_FLAG
	   FOLDER1_DESCRIP = FOLDER_DESCRIP
	   FOLDER_COM = FOLDER1_COM
	   NBULL = F_NBULL
	END IF

	FOLDER_NAME = FOLDER
	FOLDER_OWNER = FOLDER1_OWNER

	MAILTO = 0
	I = INDEX(FOLDER_DESCRIP,'<')
	J = INDEX(FOLDER_DESCRIP,'>')
	IF (I.GT.0.AND.J.GT.I.AND.(INDEX(FOLDER_DESCRIP(I:),'@').LT.1.OR.
     &	    INDEX(FOLDER_DESCRIP(I:),'@').GT.J-I+1).AND.NEWS_FEED()) THEN
           FOLDER_FLAG = IBSET(FOLDER_FLAG,4)
	   I = INDEX(FOLDER_DESCRIP,'[')
	   J = INDEX(FOLDER_DESCRIP,']')
	END IF

	IF (I.GT.0.AND.J.GT.I.AND.
     &	       (INDEX(FOLDER_DESCRIP(I:),'@').GT.1.AND.
     &	       INDEX(FOLDER_DESCRIP(I:),'@').LT.J-I+1)) THEN
	   MAILTO = 1
	END IF

	IF (CLI$PRESENT('SYSTEM')) FOLDER_FLAG = IBSET(FOLDER_FLAG,2)
	IF (CLI$PRESENT('ID')) FOLDER_FLAG = IBSET(FOLDER_FLAG,6)
	IF (CLI$PRESENT('ALWAYS')) FOLDER_FLAG = IBSET(FOLDER_FLAG,7)
	IF (CLI$PRESENT('POST_ONLY')) FOLDER_FLAG = IBSET(FOLDER_FLAG,10)
	IF (CLI$PRESENT('ADD_ONLY')) FOLDER_FLAG = IBSET(FOLDER_FLAG,11)
	IF (CLI$PRESENT('COMPRESS')) FOLDER_FLAG = IBSET(FOLDER_FLAG,12)

	IF (I.GT.0.AND.J.GT.I.AND..NOT.NEWS_FEED().AND.MAILTO.EQ.0
     &	    .AND..NOT.BTEST(FOLDER_FLAG,11)
     &	    .AND..NOT.BTEST(FOLDER_FLAG,10)) THEN 
	   CALL GET_INPUT_PROMPT(RESPONSE,RLEN,'Have you specified '//
     &		'an email address in the description? (default=N) ')
	   IF (RESPONSE(:1).EQ.'y'.OR.RESPONSE(:1).EQ.'Y') MAILTO = 1
	END IF

	IF (MAILTO.EQ.1.AND..NOT.BTEST(FOLDER_FLAG,11).AND.
     &	    .NOT.BTEST(FOLDER_FLAG,10)) THEN 
	   WRITE (6,'('' A mailing address has been specified.'')')
	   CALL GET_INPUT_PROMPT(RESPONSE,RLEN,'Will messages be '//
     &		'sent to and received from this address? (default=N) ')
	   IF (RESPONSE(:1).EQ.'y'.OR.RESPONSE(:1).EQ.'Y') THEN
	      MAILTO = 2
	      WRITE (6,'('' SET POST_ONLY will be issued.'')')
	   ELSE
	      MAILTO = 3
	      WRITE (6,'('' SET ADD_ONLY will be issued.'')')
	   END IF
	END IF

	CALL WRITE_FOLDER_FILE(IER)
	CALL MODIFY_SYSTEM_LIST(0)

	CLOSE (UNIT=1)
	CLOSE (UNIT=2)

	NOTIFY = 0
	READNEW = 0
	BRIEF = 0
	IF (CLI$PRESENT('NOTIFY')) NOTIFY = 1
	IF (CLI$PRESENT('READNEW')) READNEW = 1
	IF (CLI$PRESENT('SHOWNEW')) BRIEF = 1
	IF (CLI$PRESENT('BRIEF')) THEN
	   BRIEF = 1
	   READNEW = 1
	END IF
	CALL SET_FOLDER_DEFAULT(NOTIFY,READNEW,BRIEF)

	WRITE (6,'('' Folder is now set to '',A)')
     &		FOLDER(:TRIM(FOLDER))//'.'

	IF (CLI$GET_VALUE('COPY',FOLDER1,FLEN).NE.%LOC(CLI$_ABSENT)) THEN
	   CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER)
	   IF (IER.NE.0) THEN
	      WRITE (6,'('' Unable to copy folder settings.'')')
	   ELSE
	      CALL OPEN_BULLINF_SHARED
	      IER = 0
	      DO WHILE (IER.EQ.0)
		 DO WHILE (REC_LOCK(IER))
		    READ (9,IOSTAT=IER) TEMP_USER,LAST
		 END DO
		 IF (IER.EQ.0) THEN
		    LU = TRIM(TEMP_USER)
		    I = MAX(LU,2) - 1
		    IF (.NOT.BTEST(ICHAR(TEMP_USER(I:I)),7)) THEN
		       LAST(1,FOLDER1_NUMBER+1) = LAST(1,FOLDER_NUMBER+1)
		       LAST(2,FOLDER1_NUMBER+1) = LAST(2,FOLDER_NUMBER+1)
		       REWRITE (9,IOSTAT=IER) TEMP_USER,LAST 
		    END IF
		 END IF
	      END DO
	      CALL CLOSE_BULLINF
	      CALL OPEN_BULLUSER_SHARED
	      CALL READ_USER_FILE_HEADER(IER)
	      CALL READ_USER_FILE(IER)
	      DO WHILE (IER.EQ.0)
	         IF (TEST2(SET_FLAG,FOLDER1_NUMBER)) THEN
		    CALL SET2(SET_FLAG,FOLDER_NUMBER)
		 ELSE
		    CALL CLR2(SET_FLAG,FOLDER_NUMBER)
		 END IF
	         IF (TEST2(BRIEF_FLAG,FOLDER1_NUMBER)) THEN
		    CALL SET2(BRIEF_FLAG,FOLDER_NUMBER)
		 ELSE
		    CALL CLR2(BRIEF_FLAG,FOLDER_NUMBER)
		 END IF
	         IF (TEST2(NOTIFY_FLAG,FOLDER1_NUMBER)) THEN
		    CALL SET2(NOTIFY_FLAG,FOLDER_NUMBER)
		 ELSE
		    CALL CLR2(NOTIFY_FLAG,FOLDER_NUMBER)
		 END IF
	         REWRITE(4) TEMP_USER//USER_ENTRY(13:)
	         CALL READ_USER_FILE(IER)
	      END DO
	      CALL CLOSE_BULLUSER
	   END IF
	END IF

	GO TO 1000

910	WRITE (6,'('' Aborting folder creation.'')')
	IF (FOLDER_NUMBER.EQ.0) FOLDER_SET = .FALSE.
	CLOSE (UNIT=1,STATUS='DELETE')
	CLOSE (UNIT=2,STATUS='DELETE')

1000	CALL CLOSE_BULLFOLDER
	CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection

	RETURN

	END



	INTEGER FUNCTION CHKPRO(INPUT)
C
C 	Description:
C		Parse given identify into binary ACL format.
C		Call SYS$CHKPRO to check if present process has read
C		access to an object if the object's protection is the ACL.
C
	IMPLICIT INTEGER (A-Z)

	CHARACTER ACL*256
	CHARACTER*(*) INPUT

	INCLUDE '($CHPDEF)'

	CHKPRO = SYS$PARSE_ACL('(IDENTIFIER='//INPUT(:TRIM(INPUT))//
     &		',ACCESS=R)',ACL,,)	! Convert to ACL into binary format
	IF (.NOT.CHKPRO) RETURN		! Exit if can't

	FLAGS = CHP$M_READ		! Specify read access checking

	CALL INIT_ITMLST		! Initialize item list
	CALL ADD_2_ITMLST(ICHAR(ACL(:1)),CHP$_ACL,%LOC(ACL(1:1)))
	CALL ADD_2_ITMLST(4,CHP$_FLAGS,%LOC(FLAGS))
	CALL END_ITMLST(ACL_ITMLST)	! Get address of itemlist

	CHKPRO = SYS$CHKPRO(%VAL(ACL_ITMLST))	! Check if process has the
						! rights-id assigned to it
	RETURN
	END




	SUBROUTINE CREATE_NEWS_FOLDER
C
C  SUBROUTINE CREATE_NEWS_FOLDER
C
C  FUNCTION: Creates a new newsgroup.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLDIR.INC'

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /EDIT/ EDIT_DEFAULT
	DATA EDIT_DEFAULT/.FALSE./

	EXTERNAL CLI$_ABSENT,CLI$_NEGATED

	EDITIT = (CLI$PRESENT('EDIT').OR.EDIT_DEFAULT).AND.
     &           (CLI$PRESENT('EDIT').NE.%LOC(CLI$_NEGATED))

	IER = CLI$GET_VALUE('CREATE_FOLDER',FOLDER1_NAME,LEN_F)
	CALL LOWERCASE(FOLDER1_NAME)

	LEN_P = 0

	IF (CLI$PRESENT('FILESPEC')) THEN
	   IER = CLI$GET_VALUE('FILESPEC',BULL_PARAMETER,LEN_P)
	   CALL DISABLE_PRIVS
	   OPEN (UNIT=3,FILE=BULL_PARAMETER(:LEN_P),STATUS='OLD',
     &		READONLY,SHARED,ERR=920,FORM='FORMATTED')
	   CALL ENABLE_PRIVS	! Reset SYSPRV privileges
	END IF
C
C  If file specified in command, read file.
C  Else, read  from the terminal.
C

	IF (EDITIT) THEN			! If /EDIT specified
	   IF (LEN_P.EQ.0) THEN			! If no file param specified
	      CALL MAILEDIT('SYS$LOGIN:BULL.SCR',' ')
	      OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='OLD',
     &		 DISPOSE='DELETE',ERR=910,FORM='FORMATTED')
	      LEN_P = 1
	   ELSE
	      CLOSE (UNIT=3)
	      CALL MAILEDIT(BULL_PARAMETER(:LEN_P),'SYS$LOGIN:BULL.SCR')
	      OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='OLD',
     &		 DISPOSE='DELETE',ERR=910,FORM='FORMATTED')
	   END IF
	ELSE IF (LEN_P.EQ.0) THEN			! If file param
	   OPEN (UNIT=3,STATUS='SCRATCH',FILE='SYS$LOGIN:BULL.SCR',
     &		FORM='FORMATTED',RECL=LINE_LENGTH) ! Temp file to save message
	   WRITE (6,1000)			! Request input from terminal
1000	   FORMAT (' Enter newsgroup description:',
     &		   ' End with ctrl-z, cancel with ctrl-c')
	   ILEN = 0
	   ICOUNT = 0
	   DO WHILE (ILEN.GE.0)			! Input until no more input
	      CALL GET_LINE(INPUT,ILEN)		! Get input line
	      IF (ILEN.GT.LINE_LENGTH) THEN	! Input line too long
		 WRITE(6,'('' ERROR: Input line length > '',I,
     &			''.  Reinput:'')') LINE_LENGTH
	      ELSE IF (ILEN.GE.0) THEN		! If good input line entered
		 WRITE(3,2010) INPUT(:ILEN)	! Save line in scratch file
2010	         FORMAT(A)
		 ICOUNT = ICOUNT + ILEN
	      END IF
	   END DO
	   IF (ILEN.EQ.-1) GO TO 910		! CTRL_C entered, error out
	   IF (ICOUNT.EQ.0) GO TO 910		! No lines entered, error out
	ENDIF

	REWIND (UNIT=3)

	CALL NEWS_POST('newgroup '//FOLDER1_NAME(:LEN_F),.TRUE.,IER,
     &		       'Adding newsgroup.')
	CLOSE (UNIT=3)

	RETURN

920	WRITE(6,1020)
1020	FORMAT (' ERROR: Unable to open specified file.')
	CALL ENABLE_PRIVS
	RETURN

910	WRITE(6,1010)
1010	FORMAT (' No news group was added.')
	CLOSE (UNIT=3)
	RETURN

	END




	SUBROUTINE INIT_COMPRESS

	IMPLICIT INTEGER (A-Z)

	CHARACTER*2 MAP(159),UNMAP(0:254)

	DATA MAP/
     &	'  ', 'e ', 'th', ' t', 's ', ' a', 'in', 't ', 'er', 'he', 'on',
     &	'at', ': ', 're', 'an', 'it', 'ti', 'n ', ' i', ' o', 'es', 'ne',
     &	'te', 'd ', ' s', 'en', 'ed', 'is', 'ic', 'y ', 'st', 'ar', 'or',
     &	', ', ' w', 'al', 'ou', 'ha', 'du', 'le', 'r ', 'nt', '.e', 'nd',
     &	'to', 'f ', 've', 'ng', 'ct', ' p', 'o ', 'me', 'om', 'of', '. ',
     &	' c', 'io', 'ri', 'ca', 'se', ' m', ' b', 'ta', 'co', 'el', 'si',
     &	'as', 'hi', 'de', ' f', 'l ', 'ec', 'll', 'ro', 'et', 'a ', ' d',
     &	'ni', ' e', 'ea', 'no', 'li', 'ch', 's.', 'ra', 'ma', 'ce', 'sc',
     &	'ns', 'g ', 'ss', 'nc', 'us', 'be', ' h', '> ', 'h ', 'ac', 'os',
     &	'ci', 'bl', 'ph', 'rt', ' r', 'ot', ' I', 'tr', 'ut', ' n', 'la',
     &	'cs', 'ly', 'pr', 'wa', 'ws', 'oo', 'pe', 'ag', 'ys', 'so', 'ie',
     &	'ur', 'un', ' (', 'po', 'fo', 'em', ' l', 'm ', 'ho', 'lo', 'wi',
     &	' T', 'e.', 'im', 'di', 'ia', '.c', 'pa', 'ge', 'ga', 'ee', 'rs',
     &	'pi', 'su', 'Th', 'il', 'ai', 'wh', 'ol', 'ul', 'gr', 'ow', 'u ',
     &	'iv', 'pl', 'ab', 'am', 'mo'/

	CHARACTER*1 A(0:127,0:127)
	CHARACTER*2 B

	CHARACTER*(*) IN,OUT
	CHARACTER*255 T

	DO I=0,127
	   DO J=0,127
	     A(J,I) = ' '
	   END DO
	END DO

	UNMAP(0) = '  '
        DO I=1,254
           UNMAP(I) = CHAR(255)//CHAR(255)
	END DO
	
	J = 1
	DO I=1,8
	   J = J + 1
	   B = MAP(J)
	   UNMAP(I) = B
	   A(ICHAR(B(:1)),ICHAR(B(2:2))) = CHAR(I)
	END DO
	DO I=10,31
	   J = J + 1
	   B = MAP(J)
	   UNMAP(I) = B
	   A(ICHAR(B(:1)),ICHAR(B(2:2))) = CHAR(I)
	END DO
	DO I=127,254
	   J = J + 1
	   B = MAP(J)
	   UNMAP(I) = B
	   A(ICHAR(B(:1)),ICHAR(B(2:2))) = CHAR(I)
	END DO

	RETURN

	ENTRY COMPRESS(IN,OUT,O)

	L = LEN(IN)
	O = 1
	K = 1
	DO WHILE (K.LT.L)
	   IF (ICHAR(IN(K:K)).GT.126) IN(K:K) = ' '
	   IF (ICHAR(IN(K+1:K+1)).GT.126) IN(K+1:K+1) = ' '
	   T(O:O) = A(ICHAR(IN(K:)),ICHAR(IN(K+1:)))
	   IF (T(O:O).NE.' ') THEN
	      K = K + 2
	      O = O + 1
	   ELSE IF (L.GT.K+2.AND.IN(K:K).EQ.IN(K+1:K+1).AND.
     &		    IN(K:K+1).EQ.IN(K+2:K+3)) THEN
	      C = 4
	      K = K + 4
	      DO WHILE (K.LE.L.AND.IN(K:K).EQ.IN(K-1:K-1))
		 C = C + 1
		 K = K + 1
	      END DO
	      T(O:O+2) = CHAR(255)//CHAR(C)//IN(K-1:K-1)
	      O = O + 3
	   ELSE IF (IN(K:K+1).EQ.'  ') THEN
	      K = K + 2
	      T(O:O) = CHAR(0)
	      O = O + 1
	   ELSE
	      T(O:O) = IN(K:K)
	      IF (ICHAR(T(O:O)).LT.9.OR.(ICHAR(T(O:O)).GT.9.AND.
     &		 ICHAR(T(O:O)).LT.32)) T(O:O) = ' '
	      K = K + 1
	      O = O + 1
	   END IF
	END DO
	IF (K.EQ.L) THEN
	   T(O:O) = IN(K:K)
	ELSE
	   O = O - 1
	END IF

	OUT = T

	RETURN

	ENTRY UNCOMPRESS(IN,OUT,O)

	L = LEN(IN)
	O = 0
	I = 1
	DO WHILE (I.LE.L)
	   J = ICHAR(IN(I:I))
	   IF (J.EQ.255) THEN
	      DO J=1,ICHAR(IN(I+1:I+1))
	         O = O + 1
	         T(O:O) = IN(I+2:I+2)
	      END DO
	      I = I + 3
           ELSE
	      B = UNMAP(J)
	      IF (B.EQ.CHAR(255)//CHAR(255)) THEN
	         O = O + 1
                 T(O:O) = IN(I:I)
	      ELSE
	         O = O + 2
	         T(O-1:O) = B
	      END IF
	      I = I + 1
	   END IF
	END DO

	OUT = T(:O)

	RETURN
	END
