	SUBROUTINE BBOARD
C
C  SUBROUTINE BBOARD
C
C  FUNCTION: Converts mail to BBOARD into non-system bulletins.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFILES.INC'

	CHARACTER*11 INEXDATE
	CHARACTER*80 INDESCRIP,INFROM,INPUT

	COMMON /CTRLY/ CTRLY

	CHARACTER*12 USERNAME

	IF (BBOARD_USER.EQ.'NONE') RETURN	! BBOARD disabled?

	CALL LIB$DISABLE_CTRL(CTRLY,)	! Disable CTRL-Y & -C

C
C  The process is set to the BBOARD uic and username in order to create
C  a spawned process that is able to read the BBOARD mail (a real kludge).
C

	CALL GETUSER(USERNAME)		! Get present username
	CALL GETUIC(GROUP,USER)		! Get present uic
	IER = SETUSER(BBOARD_USER,USERNAME)	! Set to BBOARD username
	IF (IER.EQ.2) GO TO 910		! Can't set username. New VMS version?
	READ(BBOARD_UIC(2:INDEX(BBOARD_UIC,',')-1),'(O)') GROUPB
	READ(BBOARD_UIC(INDEX(BBOARD_UIC,',')+1:INDEX(BBOARD_UIC,']')-1)
     &		,'(O)') USERB
	CALL SETUIC(GROUPB,USERB)	! Set to BBOARD uic
	IER = LIB$SPAWN('$@'//BBOARD_COMMAND,'NL:','NL:')
					! Create sequential mail file
	CALL SETUSER(USERNAME)		! Reset to original username
	CALL SETUIC(GROUP,USER)		! Reset to original uic

	OPEN (UNIT=3,FILE=BBOARD_FILE,STATUS='OLD',ERR=100)

5	LEN = 1
	DO WHILE (LEN.GT.0)
	   READ (3,'(Q,A)',END=100) LEN,INPUT	! Read next line from mail
	   IF (INPUT(1:5).EQ.'From:') THEN
	      INFROM = INPUT(7:)			! Store username
	   ELSE IF (INPUT(1:5).EQ.'Subj:') THEN
	      INDESCRIP = INPUT(7:)		! Store subject
	   END IF
	END DO


C
C  Add bulletin to bulletin file and directory entry to directory file.
C

10	CALL OPEN_FILE(2)			! Prepare to add dir entry

	READ (3,'(Q,A)',IOSTAT=IER) LEN,INPUT	! Read first line
	IF (IER.NE.0) GO TO 100			! If end of file, exit
	IF (LEN.EQ.1.AND.INPUT(1:1).EQ.CHAR(12)) GO TO 5
			! If line is just form feed, the message is empty

	CALL OPEN_FILE(1)			! Prepare to add bulletin

	CALL READDIR(0,IER)			! Get NBLOCK
	IF (IER.EQ.0) NBLOCK = 0		! If new file, NBLOCK is 0

	ICOUNT = 0				! Initialize line count

	SPACE = INDEX(INFROM,' ') - 1		! Strip off the date
	IF (SPACE.GT.0) INFROM = INFROM(1:SPACE)! From the "From:" line

	CALL STR$TRIM(INFROM,INFROM,LEN)	! Get length of From line
	IF (LEN.GT.12) THEN		! Is it > allowable username length?
	   ICOUNT = ICOUNT + 1		! If so, put From line in bulletin text
	   WRITE(1'NBLOCK+ICOUNT,'(A80)',ERR=930) 'From: '//INFROM(1:74)
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

	CALL STR$TRIM(INDESCRIP,INDESCRIP,LEN)	! Get length of Subj line
	IF (LEN.GT.53) THEN		! Is it > allowable subject length?
	   ICOUNT = ICOUNT + 1		! If so, put Subj line in bulletin text
	   WRITE(1'NBLOCK+ICOUNT,'(A80)',ERR=930) 'Subj: '//INDESCRIP(1:74)
	   I = 53			! Trim subject to first space 
	   DO WHILE (I.GT.1.AND.INDESCRIP(I:I).NE.' ')
	      I = I - 1
	   END DO
	   IF (I.GT.1) INDESCRIP = INDESCRIP(1:I-1)
	END IF

	DO WHILE (INPUT(1:1).NE.CHAR(12))	! Move text to bulletin file
	   ICOUNT = ICOUNT + 1
	   WRITE(1'NBLOCK+ICOUNT,'(A80)',ERR=930) INPUT
	   READ (3,'(A)',END=25) INPUT
	END DO

25	CLOSE (UNIT=1)				! Finished adding bulletin

	DESCRIP = INDESCRIP(1:53)		! Description header
	FROM = INFROM(1:53)			! Username
	CALL GET_EXDATE(EXDATE,7)		! Expires after a week
	LENGTH = ICOUNT				! Number of records

	CALL ADD_ENTRY				! Add the new directory entry

30	CLOSE (UNIT=2)				! Totally finished with add

	GO TO 5					! See if there is more mail

100	CLOSE (UNIT=3,STATUS='DELETE')		! Close the input file
	CALL LIB$ENABLE_CTRL(CTRLY,)	! Enable CTRL-Y & -C
	RETURN

910	WRITE (6,1010)
	GO TO 100

930	CLOSE (UNIT=3)
	CALL CLOSE_FILE(3)
	WRITE (6,1030)
	GO TO 100

1010	FORMAT (' ERROR: Install BULLETIN with CMKRNL privileges or relink.')
1030	FORMAT (' ERROR: Alert system programmer. BULLETIN file problems.')

	END



	SUBROUTINE CLEANUP_LOGIN
C
C  SUBROUTINE CLEANUP_LOGIN
C
C  FUNCTION: Removes entries in user file of users that no longer exist.
C
	CHARACTER*12 USERNAME

	OPEN (UNIT=7,FILE='SYS$SYSTEM:SYSUAF.DAT',SHARED,STATUS='OLD',
     &  ACCESS='KEYED',FORM='FORMATTED',ORGANIZATION='INDEXED',READONLY,
     &  ERR=30)

	READ (4,'(A12)',ERR=20,KEYGE='            ') USERNAME
						! Move pointer to top of file

5	READ (4,'(A12)',ERR=20) USERNAME		! Get user entry
	READ (7,'(A12)',KEY=USERNAME,ERR=10) USERNAME	! See if user exists
	GO TO 5					! If so, get next user entry
	
10	DELETE(UNIT=4)				! Delete non-existant user
	GO TO 5					! Go get next user entry

20	CLOSE (UNIT=7)				! All done...

30	RETURN
	END




	SUBROUTINE CLOSE_FILE(INPUT)
C
C  SUBROUTINE CLOSE_FILE
C
C  FUNCTION: To close out the bulletin files and enable CTRL-C & -Y
C
C  INPUT:
C	INPUT  -  Unit number of file to close out. (EXCEPT FOR 3)
C	          1 = BULLETIN.DAT
C		  2 = BULLDIR.DAT
C		  3 = Close out both 1 & 2
C		  4 = BULLUSER.DAT
C

	COMMON /CTRLY/ CTRLY

	CALL LIB$ENABLE_CTRL(CTRLY,)	! Re-enable breaks

	IF (INPUT.NE.3) THEN
	   CLOSE (UNIT=INPUT)
	ELSE
	   CLOSE (UNIT=2)
	   CLOSE (UNIT=1)
	END IF

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

	CHARACTER*80 INPUT

	IF (INLUN.GT.1) THEN
	   DO I=1,IBLOCK-1
	      READ(INLUN,1000)
	   END DO
	END IF

	OCOUNT = OBLOCK
	ICOUNT = IBLOCK

	DO WHILE (1)
	   IF (INLUN.EQ.1) THEN
	      READ(INLUN'ICOUNT,1000,ERR=100) INPUT
	      ICOUNT = ICOUNT + 1
	   ELSE
	      LEN = 0
	      DO WHILE (LEN.EQ.0)
	         READ(INLUN,'(Q,A)',END=100) LEN,INPUT
		 IF (LEN.EQ.0) THEN
		    INPUT(1:) = ' '
		    LEN = 1
	         ELSE IF (ICHAR(INPUT(LEN:LEN)).EQ.10) THEN
		    INPUT(LEN-1:LEN-1) = CHAR(32)
		    INPUT(LEN:LEN) = CHAR(32)
		    LEN = LEN - 2
	         END IF
	      END DO
	   END IF
	   WRITE(1'OCOUNT,1000,IOSTAT=IER,ERR=100) INPUT
	   OCOUNT = OCOUNT + 1
	END DO

100	RETURN

1000	FORMAT(A80)

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

	OFFSET = LENGTH
	DO I=BULL_ENTRY+1,NBULL
	   CALL READDIR(I,IER)
	   BLOCK = BLOCK - OFFSET
	   CALL WRITEDIR(I-1,IER)
	END DO

	DELETE(UNIT=2,REC=NBULL+1)

	CALL READDIR(0,IER)
	NBULL = NBULL - 1
	NBLOCK = NBLOCK - OFFSET
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

	LIMIT = LEN(INPUT)			! Get input line size limit

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

	IER = LIB$GET_INPUT(DESCRIP)		! Get line from terminal

	IF (FLAG.EQ.0) THEN			! If no CTRL-C has occurred
	   IER1 = SYS$CANCEL(%VAL(TERM_CHAN))	! Cancel CTRL-C AST
	   IF (IER.NE.RMS$_EOF) THEN		! See if CTRL-Z is in input
	      LEN_INPUT = MIN(LIMIT,LENGTH)	! Yep. Get length of line
	      DO  I=0,LEN_INPUT-1		! Extract from descriptor
	         CALL GET_VAL(INPUT(I+1:I+1),%VAL(POINTER+I))
	      END DO
	   ELSE
	      LEN_INPUT = -2			! If CTRL-Z, say so
	   END IF
	ELSE
	   LEN_INPUT = -1			! If CTRL-C, say so
	END IF
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
	PARAMETER DVI$_DEVDEPEND = 'A'X
	INTEGER ITMLST(3)
	LOGICAL*1 DEVDEPEND(4)
	ITMLST(1) = ISHFT(DVI$_DEVDEPEND,16).OR.4
	ITMLST(2) = %LOC(DEVDEPEND(1))
	ITMLST(3) = LEN
	ITMLST(4) = 0
	CALL SYS$GETDVIW(,,'TT',ITMLST,,,,)
	PAGE_LENGTH = DEVDEPEND(4)
	RETURN
	END






	SUBROUTINE GETPRIV(ALLOW)
C
C  SUBROUTINE GETPRIV
C
C  FUNCTION:
C	To check if process has SETPRV capabilities.
C  OUTPUTS:
C	ALLOW - Set to 0 if no privileges, set to 1 if privileges.
C

	IMPLICIT INTEGER (A-Z)

	INTEGER*4 ITMLST(4)		! Item list for SYS$GETJPI
C
C  ITMLST is the ITEMLIST for the SYS$GETJPI system service.
C  It has the following format:
C	ITMLST(1)	Top 16 bits = Item code (found in macro $JPIDEF
C			in SYS$LIBRARY:STARTLET.MLB).
C			Bottom 16 bits = length of buffer in bytes to
C			receive the device information.
C	ITMLST(2)	Address of buffer to receive device information.
C	ITMLST(3)	Address of buffer to receive the length of the
C			information. 0 indicates no such buffer desired.
C	  ...
C	ITMLST(N)	The last longword in an item list must be 0.
C
	DATA ITMLST/4*0/
	PARAMETER JPI$_PROCPRIV='204'X	! Item code to get JPI$_PROCPRIV
	PARAMETER PRV$M_SETPRV='4000'X	! Mask for SETPRV privileges

	ITMLST(1) = ISHFT(JPI$_PROCPRIV,16).OR.4 ! Move JPI$_PROCPRIV to upper
					! word & fill bottom word with # bytes.
	ITMLST(2) = %LOC(PROCPRIV)	! PROCPRIV is buffer to receive info.

	IER = SYS$GETJPIW(,,,ITMLST,,,,) ! Get info

	IF ((PROCPRIV.AND.PRV$M_SETPRV).NE.0) THEN
	   ALLOW = 1
	ELSE
	   ALLOW = 0
	END IF

	RETURN
	END






 
	SUBROUTINE GETUSER(USERNAME)
C
C  SUBROUTINE GETUSER
C
C  FUNCTION:
C	To get username of present process.
C  OUTPUTS:
C	USERNAME   -   Username owner of present process.
C

	IMPLICIT INTEGER (A-Z)

	INTEGER*4 ITMLST(4)		! Item list for SYS$GETJPI
C
C  ITMLST is the ITEMLIST for the SYS$GETJPI system service.
C  It has the following format:
C	ITMLST(1)	Top 16 bits = Item code (found in macro $JPIDEF
C			in SYS$LIBRARY:STARTLET.MLB).
C			Bottom 16 bits = length of buffer in bytes to
C			receive the device information.
C	ITMLST(2)	Address of buffer to receive device information.
C	ITMLST(3)	Address of buffer to receive the length of the
C			information. 0 indicates no such buffer desired.
C	  ...
C	ITMLST(N)	The last longword in an item list must be 0.
C
	DATA ITMLST/4*0/
	PARAMETER JPI$_USERNAME='202'X	! Item code to get JPI$_USERNAME
	CHARACTER*(*) USERNAME		! Limit is 12 characters

	ITMLST(1) = ISHFT(JPI$_USERNAME,16).OR.12 ! Move JPI$_USERNAME to upper
					! word & fill bottom word with # bytes.
	ITMLST(2) = %LOC(USERNAME)	! USERNAME is buffer to receive info.

	IER = SYS$GETJPIW(,,,ITMLST,,,,) ! Get info

	RETURN
	END




	SUBROUTINE GETSTS(STS)
C
C  SUBROUTINE GETSTS
C
C  FUNCTION:
C	To get status of present process. This tells if its a batch process.
C  OUTPUTS:
C	STS   -   Status word of present process.
C

	IMPLICIT INTEGER (A-Z)

	INTEGER*4 ITMLST(4)		! Item list for SYS$GETJPI
C
C  ITMLST is the ITEMLIST for the SYS$GETJPI system service.
C  It has the following format:
C	ITMLST(1)	Top 16 bits = Item code (found in macro $JPIDEF
C			in SYS$LIBRARY:STARTLET.MLB).
C			Bottom 16 bits = length of buffer in bytes to
C			receive the device information.
C	ITMLST(2)	Address of buffer to receive device information.
C	ITMLST(3)	Address of buffer to receive the length of the
C			information. 0 indicates no such buffer desired.
C	  ...
C	ITMLST(N)	The last longword in an item list must be 0.
C
	DATA ITMLST/4*0/
	PARAMETER JPI$_STS='305'X	! Item code to get JPI$_USERNAME
	INTEGER STS

	ITMLST(1) = ISHFT(JPI$_STS,16).OR.4	! Move JPI$_STS to upper
					! word & fill bottom word with # bytes.
	ITMLST(2) = %LOC(STS)		! STS is buffer to receive info.

	IER = SYS$GETJPIW(,,,ITMLST,,,,) ! Get info

	RETURN
	END




	SUBROUTINE HELP(LIBRARY)

	IMPLICIT INTEGER (A-Z)
	EXTERNAL LIB$PUT_OUTPUT,LIB$GET_INPUT
	CHARACTER*(*) LIBRARY

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	IER = CLI$GET_VALUE('HELP_TOPIC',BULL_PARAMETER,LEN_P)

	CALL LBR$OUTPUT_HELP(LIB$PUT_OUTPUT,,BULL_PARAMETER(1:LEN_P)
     &		,LIBRARY,,LIB$GET_INPUT)

	RETURNe
	END


	SUBROUTINE OPEN_FILE(INPUT)

	IMPLICIT INTEGER (A-Z)E

	INCLUDE 'BULLFILES.INC'

	INCLUDE '($FORIOSDEF)'I

	EXTERNAL BULLDIR_ERR,BULLETIN_ERR,BULLUSER_ERRN

	PARAMETER TIMEOUT = -10*1000*1000*30T
	DIMENSION TIMEBUF(2)S
	DATA TIMEBUF /TIMEOUT,-1/
	PARAMETER TIMEEFN = 1

	COMMON /CTRLY/ CTRLY$

	CALL LIB$DISABLE_CTRL(CTRLY,)	! No breaks while file is openi

	IF (INPUT.EQ.3.OR.INPUT.EQ.2) THEN 
	   IER = SYS$SETIMR(%VAL(TIMEEFN),TIMEBUF,BULLDIR_ERR,)
20	   OPEN (UNIT=2,FILE=BULLDIR_FILE,STATUS='UNKNOWN',
     1	   RECORDTYPE='FIXED',RECORDSIZE=107,ACCESS='DIRECT',ERR=20,r
     1	   ORGANIZATION='RELATIVE',DISPOSE='KEEP',FORM='FORMATTED')
	END IFI

	IF (INPUT.EQ.3.OR.INPUT.EQ.1) THENr
	   IER = SYS$SETIMR(%VAL(TIMEEFN),TIMEBUF,BULLETIN_ERR,),
10	   OPEN (UNIT=1,FILE=BULLETIN_FILE,STATUS='UNKNOWN',,
     1	   ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=80,L
     1	   FORM='FORMATTED',ERR=10)
	END IFI

	IF (INPUT.EQ.4) THENR
	   IER = SYS$SETIMR(%VAL(TIMEEFN),TIMEBUF,BULLUSER_ERR,)
30	   OPEN (UNIT=4,FILE=BULLUSER_FILE,STATUS='UNKNOWN',C
     1	   ACCESS='KEYED',RECORDTYPE='FIXED',RECORDSIZE=58,IOSTAT=IER,B
     1	   FORM='FORMATTED',ORGANIZATION='INDEXED',
     1	   KEY=(1:12:CHARACTER)),
	   IF (IER.EQ.FOR$IOS_INCRECLEN) THEN
	      CALL CONVERT_USERFILE
	      GO TO 30
	   ELSE IF (IER.NE.0) THEN	
	      GO TO 30
	   END IF
	END IF5

	IER = SYS$CANTIM(,)		! Successful, so cancel timer.

	RETURN
	END

	SUBROUTINE TIMER_ERR 

	IMPLICIT INTEGER (A-Z)i

	COMMON /CTRLY/ CTRLY 

	ENTRY BULLDIR_ERR
	WRITE (6,'('' ERROR: Unable to open BULLDIR.DAT after 30 seconds.'')')I
	GO TO 10L

	ENTRY BULLETIN_ERRi
	WRITE (6,'('' ERROR: Unable to open BULLETIN.DAT after 30 seconds.'')')
	GO TO 10E

	ENTRY BULLUSER_ERR	
	WRITE (6,'('' ERROR: Unable to open BULLUSER.DAT after 30 seconds.'')')
	GO TO 10t

10	CALL LIB$ENABLE_CTRL(CTRLY,)	! No breaks while file is open
	CALL EXIT
	END



	SUBROUTINE OPEN_FILE_SHARED(INPUT)	

	INCLUDE '($FORIOSDEF)'

	INCLUDE 'BULLFILES.INC'

	COMMON /CTRLY/ CTRLY

	CALL LIB$DISABLE_CTRL(CTRLY,)	! No breaks while file is open

	IF (INPUT.EQ.3.OR.INPUT.EQ.2) THENe
20	   OPEN (UNIT=2,FILE=BULLDIR_FILE,STATUS='OLD',
     1	   RECORDTYPE='FIXED',RECORDSIZE=107,ACCESS='DIRECT',IOSTAT=IER,m
     1	   ORGANIZATION='RELATIVE',DISPOSE='KEEP',FORM='FORMATTED',
     1	   SHARED,READONLY)

	   IF (IER.EQ.FOR$IOS_FILNOTFOU) GO TO 100n
	   IF (IER.NE.0) GO TO 20

	END IFR

	IF (INPUT.EQ.3.OR.INPUT.EQ.1) THENm
10	   OPEN (UNIT=1,FILE=BULLETIN_FILE,STATUS='OLD',A
     1	   ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=80,'
     1	   FORM='FORMATTED',IOSTAT=IER,SHARED,READONLY)

	   IF (IER.EQ.FOR$IOS_FILNOTFOU) GO TO 100 
	   IF (IER.NE.0) GO TO 10

	END IFD

	IF (INPUT.EQ.4) THEND
30	   OPEN (UNIT=4,FILE=BULLUSER_FILE,STATUS='UNKNOWN',N
     1	   ACCESS='KEYED',RECORDTYPE='FIXED',RECORDSIZE=58,IOSTAT=IER, 
     1	   FORM='FORMATTED',ORGANIZATION='INDEXED',SHARED,B
     1	   KEY=(1:12:CHARACTER))j
	   IF (IER.EQ.FOR$IOS_INCRECLEN) THEN
	      CALL CONVERT_USERFILE
	      GO TO 301
	   ELSE IF (IER.NE.0) THEN
	      GO TO 30
	   END IF
	END IF.

	RETURNC

100	CALL OPEN_FILE(INPUT)I

	RETURNH
	END



	SUBROUTINE CONVERT_USERFILE
Cu
C  SUBROUTINE CONVERT_USERFILE
C 
C  FUNCTION: Converts user file to new format which has 8 bytes added.
C=

	IMPLICIT INTEGER (A-Z)C

	INCLUDE 'BULLFILES.INC'

	CHARACTER*58 BUFFER
	DIMENSION ZERO(2)
	DATA ZERO/2*0/ 

10	OPEN (UNIT=4,FILE=BULLUSER_FILE,STATUS='UNKNOWN',
     1	   ACCESS='KEYED',RECORDTYPE='FIXED',RECORDSIZE=50,ERR=10,r
     1	   FORM='FORMATTED',ORGANIZATION='INDEXED',SHARED,e
     1	   KEY=(1:12:CHARACTER))t

	OPEN (UNIT=8,FILE=BULLUSER_FILE,STATUS='NEW',
     1	   ACCESS='KEYED',RECORDTYPE='FIXED',RECORDSIZE=58,IOSTAT=IER,u
     1	   FORM='FORMATTED',ORGANIZATION='INDEXED',
     1	   KEY=(1:12:CHARACTER))1

	DO WHILE (1)
	   READ (4,'(A50)',END=20) BUFFER
	   WRITE (8,'(A50,2A4)') BUFFER,(ZERO(I),I=1,2)
	END DOR

20	CLOSE (UNIT=4)t
	CLOSE (UNIT=8)e

	RETURN.
	END


	SUBROUTINE READDIR(BULLETIN_NUM,ICOUNT)
CE
C  SUBROUTINE READDIR
CD
C  FUNCTION: Finds the entry for the specified bulletin in the
C	directory file and returns the information for that entry.
C 
C  INPUTS:
C	BULLETIN_NUM  -  Bulletin number.  Starts with 1.E
C			 If 0, gives header info, i.e number of bulls,
C			 number of blocks in bulletin file, etc.
C  OUTPUTS:R
C	ICOUNT  -  The last record read by this routine.
CE

	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLDIR.INC'
	f
	ICOUNT = BULLETIN_NUM

	IF (ICOUNT.EQ.0) THEN
	   READ (2'1,1000,ERR=999) NEWEST_EXDATE,NEWEST_DATE,NEWEST_TIME,
     &		NBULL,NBLOCK,SHUTDOWN,SHUTDOWN_DATE,SHUTDOWN_TIME
	ELSED
	   READ(2'ICOUNT+1,1010,ERR=999)a
     &		DESCRIP,FROM,DATE,TIME,LENGTH,EXDATE,SYSTEM,BLOCKE
	END IF	

	ICOUNT = ICOUNT + 1

999	RETURN

1000	FORMAT(A11,A11,A8,A4,A4,A4,A11,A8)
1010	FORMAT(A53,A12,A11,A8,A4,A11,A4,A4)

	END


	SUBROUTINE WRITEDIR(BULLETIN_NUM,IER)
C
C  SUBROUTINE WRITEDIR
C-
C  FUNCTION: Writes the entry for the specified bulletin in the 
C	directory file. 
C=
C  INPUTS:
C	BULLETIN_NUM  -  Bulletin number.  Starts with 1.A
C			 If 0, write the header of the directory file.
C  OUTPUTS:	
C	IER - Error status from WRITE.
C)

	IMPLICIT INTEGER (A - Z))

	INCLUDE 'BULLDIR.INC'
	
	IF (BULLETIN_NUM.EQ.0) THEN
	   WRITE (2'1,1000,IOSTAT=IER) NEWEST_EXDATE,NEWEST_DATE,NEWEST_TIME,
     &		NBULL,NBLOCK,SHUTDOWN,SHUTDOWN_DATE,SHUTDOWN_TIMEt
	ELSEe
	   WRITE(2'BULLETIN_NUM+1,1010,IOSTAT=IER)n
     &		DESCRIP,FROM,DATE,TIME,LENGTH,EXDATE,SYSTEM,BLOCKi
	END IF 

	RETURN

1000	FORMAT(A11,A11,A8,A4,A4,A4,A11,A8)f
1010	FORMAT(A53,A12,A11,A8,A4,A11,A4,A4)

	END


	SUBROUTINE TRUNCATE_FILE(TRUNC_SIZE)

	IMPLICIT INTEGER (A-Z)s

	INCLUDE 'BULLFILES.INC'

	COMMON /USER_OPEN/ CHANNEL,STATUS,SIZEv

	EXTERNAL USER_OPEN$TRUNCATE

	INCLUDE '($RMSDEF)'

	COMMON /CTRLY/ CTRLYe

	CALL LIB$DISABLE_CTRL(CTRLY,)	! No breaks while file is openT

10	OPEN (UNIT=1,FILE=BULLETIN_FILE,STATUS='OLD',
     1	 RECORDTYPE='FIXED',RECORDSIZE=80,INITIALSIZE=TRUNC_SIZE,
     1	 FORM='FORMATTED',ERR=20,USEROPEN=USER_OPEN$TRUNCATE)

15	CLOSE (UNIT=1)A
	CALL LIB$ENABLE_CTRL(CTRLY,)P
	RETURN 

20	IF ((STATUS.AND.1).EQ.1.OR.STATUS.EQ.RMS$_EOF) THEN
	   GO TO 15
	ELSE 
	   GO TO 10
	END IF'

	END


	SUBROUTINE UPDATE_LOGIN
CE
C  SUBROUTINE UPDATE_LOGIN
C 
C  FUNCTION:  Updates the login file when a bulletin has been deleted.
CU
	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	CHARACTER*12 TEMP_USERE
	CHARACTER*11 TEMP_DATE,BBOARD_DATET
	CHARACTER*8 TEMP_TIME,BBOARD_TIME

	CALL OPEN_FILE(4)

	READ (4,1000,KEY='            ',ERR=10)
     &		TEMP_USER,TEMP_DATE,TEMP_TIME,BBOARD_DATE,BBOARD_TIME 
	REWRITE (4,1000)E
     & 		TEMP_USER,NEWEST_DATE,NEWEST_TIME,BBOARD_DATE,BBOARD_TIME
	CLOSE (UNIT=4) 
	RETURNi

10	WRITE (4,1000) '            ',NEWEST_DATE,NEWEST_TIME
	CLOSE (UNIT=4).
	RETURNO

1000	FORMAT(A12,A11,A8,A11,A8)

	END


 L
	SUBROUTINE ADD_ENTRYL
C 
C  SUBROUTINE ADD_ENTRYL
CI
C  FUNCTION: Enters a new directory entry in the directory file.
CD
	IMPLICIT INTEGER (A - Z)-
	
	INCLUDE 'BULLDIR.INC'
	
	CHARACTER*23 TODAY_TIME

	CALL SYS$ASCTIM(,TODAY_TIME,,) 
	DATE = TODAY_TIME(1:11)
	TIME = TODAY_TIME(13:20)E

	CALL READDIR(0,IER)
	IF (IER.EQ.1) GO TO 20n

10	NEWEST_EXDATE = DATE
	NBULL = 0
	NBLOCK = 0A
	SHUTDOWN = 0T

20	NEWEST_DATE = DATER
	NEWEST_TIME = TIMEN

	DIFF = COMPARE_DATE(NEWEST_EXDATE,EXDATE)
	IF (DIFF.GT.0) NEWEST_EXDATE = EXDATE

	NBULL = NBULL + 1
	BLOCK = NBLOCK + 1G
	NBLOCK = NBLOCK + LENGTH3

	IF ((SYSTEM.AND.4).EQ.4) THEN
	   SHUTDOWN = SHUTDOWN + 1d
	   SHUTDOWN_DATE = DATE
	   SHUTDOWN_TIME = TIME
	END IF(

	CALL WRITEDIR(0,IER)A

	CALL UPDATE_LOGIN

	CALL WRITEDIR(NBULL,IER))

	RETURN4
	END




 
	INTEGER FUNCTION COMPARE_DATE(DATE1,DATE2)Y
C,
C  FUNCTION COMPARE_DATE
Cu
C  FUCTION: Compares dates to see which is farther in future.S
C
C  INPUTS:
C	DATE1  -  First date  (dd-mm-yy)
C	DATE2  -  Second date (If is equal to ' ', then use present date)T
C  OUTPUT:
C	Returns the difference in days between the two dates.o
C	If the DATE1 is farther in the future, the output is positive,
C	else it is negative.
Ce
	IMPLICIT INTEGER (A - Z)e

	CHARACTER*(*) DATE1,DATE2
	INTEGER USER_TIME(2)o

	CALL SYS$BINTIM(DATE1,USER_TIME)!
	CALL LIB$DAY(DAY1,USER_TIME) 

	IF (DATE2.NE.' ') THENo
	   CALL SYS$BINTIM(DATE2,USER_TIME)
	ELSEm
	   CALL SYS$GETTIM(USER_TIME)
	END IFm

	CALL LIB$DAY(DAY2,USER_TIME)O

	COMPARE_DATE = DAY1 - DAY2F

	RETURNg
	END




	INTEGER FUNCTION COMPARE_TIME(TIME1,TIME2)
C 
C  FUNCTION COMPARE_TIME
CT
C  FUCTION: Compares times to see which is farther in future.x
Ca
C  INPUTS:
C	TIME1  -  First time	(hh:mm:ss)Y
C	TIME2  -  Second timea
C  OUTPUT:
C	Outputs 1 if time1 greater in future, outputs -1 if time2
C	greater in future.  If exactly the same, output 0.
Ct

	IMPLICIT INTEGER (A-Z)(
	CHARACTER*(*) TIME1,TIME2
	CHARACTER*23 TODAY_TIME
	CHARACTER*8 TIME2_TEMP 

	IF (TIME2.EQ.' ') THEN
	   CALL SYS$ASCTIM(,TODAY_TIME,,)
	   TIME2_TEMP = TODAY_TIME(13:20)
	ELSEE
	   TIME2_TEMP = TIME2
	END IFf

	COMPARE_TIME = 0

	DO J=1,7,3S
	   DO I=J,J+1
	      IF (TIME1(I:I).GT.TIME2_TEMP(I:I)) THEN
		 COMPARE_TIME = 1 
		 RETURNe
	      ELSE IF (TIME1(I:I).LT.TIME2_TEMP(I:I)) THENR
		 COMPARE_TIME = -1
		 RETURN	
	      END IFc
	   END DO
	END DO 

	RETURN 
	END

C-------------------------------------------------------------------------
CG
C  The following are subroutines to create a linked-list queue for P
C  temporary buffer storage of data that is read from files to beN
C  outputted to the terminal.  This is done so as to be able to closeC
C  the file as soon as possible.
C_
C  Each record in the queue has the following format.  The first two
C  words are used for creating a character variable.  The first word
C  contains the length of the character variable, the second contains 
C  the address.  The address is simply the address of the 3rd word ofL
C  the record.  The last word in the record contains the address of theE
C  next record.  Every time a record is written, if that record has a,
C  zero link, it adds a new record for the next write operation. 
C  Therefore, there will always be an extra record in the queue.  To
C  check for the end of the queue, the last word (link to next record)
C  is checked to see if it is zero. 
Ci
C  There are 2 seperate queues.  One for directory listings, and one  
C  for bulletins reads.  The bulletin queue is made of character
C  variables of length 80.  The directory listings contain character
C  variables of length 88.  Although BULLETIN does not use all the
C  info that is stored, (SYSTEM,BLOCK,LENGTH), that info is used byi
C  BULLCHECK.N
CP
C-------------------------------------------------------------------------

	SUBROUTINE WRITE_DIR(RECORD,NEXT)
	INTEGER RECORD(1)
	CALL WRITE_DIR_CHAR(%VAL(%LOC(RECORD)))
	NEXT = RECORD(25)
	IF (NEXT.NE.0) RETURN
	CALL LIB$GET_VM(100,NEXT)
	CALL MAKE_CHAR(%VAL(NEXT),88)
	RECORD(25) = NEXT
	RETURNT
	END

	SUBROUTINE WRITE_DIR_CHAR(SCRATCH)T
	CHARACTER*(*) SCRATCH
	INCLUDE 'BULLDIR.INC'
        WRITE(SCRATCH,1035) DESCRIP,FROM,DATE,SYSTEM,BLOCK,LENGTHI
	RETURNR
1035	FORMAT(A53,A12,A11,A4,A4,A4)O
	END

	SUBROUTINE READ_DIR(RECORD,NEXT)I
	INTEGER RECORD(1)
	CALL READ_DIR_CHAR(%VAL(%LOC(RECORD)))e
	NEXT = RECORD(25)
	RETURN
	END

	SUBROUTINE READ_DIR_CHAR(SCRATCH)
	CHARACTER*(*) SCRATCH
	INCLUDE 'BULLDIR.INC'
        READ(SCRATCH,1035) DESCRIP,FROM,DATE,SYSTEM,BLOCK,LENGTH
	RETURN 
1035	FORMAT(A53,A12,A11,A4,A4,A4)I
	END

	SUBROUTINE WRITE_BULL(RECORD,NEXT)P
	INTEGER RECORD(1)
	CALL WRITE_BULL_CHAR(%VAL(%LOC(RECORD)))S
	NEXT = RECORD(23)
	IF (NEXT.NE.0) RETURN
	CALL LIB$GET_VM(92,NEXT)
	CALL MAKE_CHAR(%VAL(NEXT),80)
	RECORD(23) = NEXT
	RETURNM
	END

	SUBROUTINE WRITE_BULL_CHAR(SCRATCH)
	CHARACTER*(*) SCRATCH
	COMMON/INPUT_BULL/INPUT
	CHARACTER*80 INPUTP
	SCRATCH = INPUT
	RETURNo
	END

	SUBROUTINE READ_BULL(RECORD,NEXT)
	INTEGER RECORD(1)
	CALL READ_BULL_CHAR(%VAL(%LOC(RECORD)))
	NEXT = RECORD(23)
	RETURN 
	END

	SUBROUTINE READ_BULL_CHAR(SCRATCH)m
	CHARACTER*(*) SCRATCH
	COMMON/INPUT_BULL/INPUT
	CHARACTER*80 INPUTs
	INPUT = SCRATCH
	RETURNh
	END


	SUBROUTINE MAKE_CHAR(IARRAY,LEN) 
	DIMENSION IARRAY(1)
	IARRAY(1) = LEN
	IARRAY(2) = %LOC(IARRAY(3))
	IARRAY(LEN/4+3) = 0
	RETURN 
	END



	SUBROUTINE CHECK_PRIV_IO(ERROR)
Cf
C  SUBROUTINE CHECK_PRIV_IOs
Cf
C  FUNCTION: Checks SYS$OUTPUT and SYS$ERROR to see if they need
C	privileges to output to.
Ce

	IMPLICIT INTEGER (A-Z)t

	DIMENSION SETPRV(2)
	DATA SETPRV/Z10000000,0/		! SYSPRV privileges

	CALL SYS$SETPRV(%VAL(0),SETPRV,,)	! Disable SYSPRV 

	OPEN (UNIT=6,FILE='SYS$OUTPUT',IOSTAT=IER,STATUS='NEW')
	CLOSE (UNIT=6,STATUS='DELETE')a

	OPEN (UNIT=4,FILE='SYS$ERROR',IOSTAT=IER1,STATUS='NEW')
	IF (IER.NE.0.OR.IER1.NE.0) THEN
	   IF (IER1.EQ.0) WRITE (4,100)
	   IF (IER.EQ.0) WRITE (6,200)%
	   ERROR = 1 
	ELSEV
	   CLOSE (UNIT=4,STATUS='DELETE')
	   ERROR = 0,
	END IF,

	CALL SYS$SETPRV(%VAL(1),SETPRV,,)	! Enable SYSPRV )

100	FORMAT(1X,'ERROR: SYS$OUTPUT cannot be opened.')
200	FORMAT(1X,'ERROR: SYS$ERROR cannot be opened.')E

	RETURN
	END



	SUBROUTINE GETUIC(GRP,MEM)T
C:
C  SUBROUTINE GETUIC(UIC)e
Cp
C  FUNCTION:
C	To get UIC of process submitting the job.p
C  OUTPUT:
C	GRP   -    Group number of UIC
C	MEM   -	   Member number of UICl
C 

	IMPLICIT INTEGER (A-Z)S

	INTEGER*4 ITMLST(7)		! Item list for SYS$GETJPI
C 
C  ITMLST is the ITEMLIST for the SYS$GETJPI system service.
C  It has the following format:	
C	ITMLST(1)	Top 16 bits = Item code (found in macro $JPIDEF 
C			in SYS$LIBRARY:STARTLET.MLB). 
C			Bottom 16 bits = length of buffer in bytes tof
C			receive the device information.	
C	ITMLST(2)	Address of buffer to receive device information.
C	ITMLST(3)	Address of buffer to receive the length of the
C			information. 0 indicates no such buffer desired.
C	  ...D
C	ITMLST(N)	The last longword in an item list must be 0.
C 
	DATA ITMLST/7*0/E
	PARAMETER JPI$_GRP='308'X	! Item code to get JPI$_GRP
	PARAMETER JPI$_MEM='307'X	! Item code to get JPI$_MEM

	ITMLST(1) = ISHFT(JPI$_GRP,16).OR.4 ! Move JPI$_GRP to upperI
					! word & fill bottom word with # bytes.
	ITMLST(2)=%LOC(GRP)		! GRP is buffer to receive info.
	ITMLST(4) = ISHFT(JPI$_MEM,16).OR.4 ! Move JPI$_MEM to upper
					! word & fill bottom word with # bytes.
	ITMLST(5)=%LOC(MEM)		! MEM is buffer to receive info.

	IER = SYS$GETJPIW(,,,ITMLST,,,,)	! Get Info command.e

	RETURNs
	END




	SUBROUTINE GET_UPTIME(UPTIME_DATE,UPTIME_TIME)t
Cl
C  SUBROUTINE GET_UPTIME
CM
C  FUNCTION: Gets time of last reboot.
Ce

	IMPLICIT INTEGER (A-Z)o

	EXTERNAL	EXE$GL_ABSTIM1
	INTEGER 	UPTIME(2),SYSTIME(2),UPSINCE(2)J
	CHARACTER*(*)	UPTIME_TIME,UPTIME_DATE
	CHARACTER	ASCSINCE*23

	UPTIME(1) = GET_L_VAL(EXE$GL_ABSTIM)			! Up time (sec)r

	CALL LIB$EMUL(10000000,UPTIME,0,UPTIME) 		! 64 bit format
	CALL SYS$GETTIM(SYSTIME)s
	CALL LIB$SUBX(SYSTIME,UPTIME,UPSINCE)
	CALL SYS$ASCTIM(,ASCSINCE,UPSINCE,)			! Up sincee

	UPTIME_DATE = ASCSINCE(1:11)t
	UPTIME_TIME = ASCSINCE(13:20)

	RETURN	
	END

	INTEGER FUNCTION GET_L_VAL(I)
	INTEGER I
	GET_L_VAL = I
	RETURN
	END


	SUBROUTINE SET_READNEW(CMD,TOPIC)
C.
C  SUBROUTINE SET_READNEWe
C
C  FUNCTION: Sets readnew for specified topic (TOPIC = 1 is general topic).s
Cf
C  INPUTS:
C	CMD    -   LOGICAL*4 value. If TRUE, set readnew. 
C		   If FALSE, clear readnew.
C	TOPIC  -   TOPIC number, corresponding to bit number.T
CA
	IMPLICIT INTEGER (A - Z)P

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'P

	LOGICAL CMD

CA
C  Find user entry in BULLUSER.DAT to update information.,
CL

	CALL OPEN_FILE_SHARED(4)		! Open user file$

	READ (4,1000,KEY=USERNAME) USERNAME,	! Read old entry
     &		LOGIN_DATE,LOGIN_TIME,READ_DATE,READ_TIME,FLAGS(

	F_POINT = TOPIC/32 + 1 
	IF (CMD) THEN
	   I = IBSET(FLAGS(F_POINT),TOPIC-1)S
	ELSE
	   I = IBCLR(FLAGS(F_POINT),TOPIC-1)L
	END IFN

	REWRITE (4,1000) USERNAME,		! Write modified entryT
     &		LOGIN_DATE,LOGIN_TIME,READ_DATE,READ_TIME,FLAGSN

	CALL CLOSE_FILE (4)
	RETURN

1000	FORMAT(A12,A11,A8,A11,A8,2A4)
	END





	SUBROUTINE CONFIRM_PRIV(USERNAME,PASSWORD,ALLOW) 
CE
C  SUBROUTINE CONFIRM_PRIV
CM
C  FUNCTION: Confirms that given username has SETPRV, and that the
C	the given password is correct.
CR
C  INPUTS:
C	USERNAME  -  Usernamer
C	PASSWORD  -  Username's password
C  OUTPUTS:'
C  	ALLOW     -  Returns 1 if correct password and SETPRV set,
C		     returns 0 if not.%
C(

	IMPLICIT INTEGER (A-Z)R

	CHARACTER*(*) USERNAME,PASSWORD

	PARAMETER UAF$Q_DEF_PRIV = '1A4'X, UAF$Q_PWD = '154'X
	PARAMETER UAF$W_SALT = '166'X, UAF$B_ENCRYPT = '168'X

	PARAMETER PRV$V_SETPRV = 'E'X

	LOGICAL*1 UAF(0:583)Y
	CHARACTER*(*) SYSUAFT
	PARAMETER (SYSUAF = 'SYS$SYSTEM:SYSUAF.DAT')=
	EQUIVALENCE (UAF(UAF$B_ENCRYPT), UAF_ENCRYPT)
	EQUIVALENCE (UAF(UAF$W_SALT), UAF_SALT)
	EQUIVALENCE (UAF(UAF$Q_PWD), UAF_PWD)
	EQUIVALENCE (UAF(UAF$Q_DEF_PRIV),UAF_DEF_PRIV)H
	CHARACTER UAF_PWD*8,HASH*8R

	CALL STR$UPCASE(PASSWORD,PASSWORD)	! Password must be upper case
	ALLOW = 0					! Set return false 
	CALL LIB$GET_LUN(LUN)				! Get LUNI
	OPEN (UNIT=LUN,FILE=SYSUAF,SHARED,READONLY,ACCESS='KEYED',
     &		FORM='UNFORMATTED',TYPE='OLD',ERR=999)	! Open UAFi
	READ (LUN,KEY=USERNAME,IOSTAT=STATUS) UAF	! Read Record
	IF (STATUS.EQ.0) THEN				! If username found3
	   IF (BTEST(UAF_DEF_PRIV,PRV$V_SETPRV)) THEN	! System privileges?E
	      CALL LGI$HPWD(HASH,PASSWORD,%VAL(UAF_ENCRYPT),)
     &		%VAL(UAF_SALT),USERNAME)		! HASH the passwordR
	      IF (HASH.EQ.UAF_PWD) ALLOW = 1		! Set return true
	   END IF					! If correct password
	END IFo
	CLOSE (UNIT=LUN)				! Close the LUN
999	CALL LIB$FREE_LUN(LUN)				! Free the LUN
	RETURN						! ReturnI
	END						! EndU



	INTEGER FUNCTION SYS_TRNLNM(INPUT,OUTPUT,ACCESS)$

	IMPLICIT INTEGER (A-Z)r

	CHARACTER*(*) INPUT,OUTPUT 

	INTEGER ITMLST(4)

        PARAMETER LNM$_STRING = '2'X

	ITMLST(1) = ISHFT(LNM$_STRING,16).OR.LEN(OUTPUT)R
	ITMLST(2) = %LOC(OUTPUT)T
	ITMLST(3) = 0

	SYS_TRNLNM = SYS$TRNLNM(,'LNM$PROCESS',INPUT,ACCESS,ITMLST)

	RETURN 
	END
