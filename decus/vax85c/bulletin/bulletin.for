C
C  BULLETIN.FOR, Version P850716
C  Purpose: Facility for reading, adding, and delete bulletins.
C  Environment: MIT PFC VAX-11/780, VMS
C  Usage: Invoked by the BULLETIN command.
C  Programmer: Mark R. London
C
C  NOTES: See BULLETIN.TXT for general info.
C
	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE '($RMSDEF)'

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /POINT/ BULL_POINT

	COMMON /READIT/ READIT

	COMMON /PAGE/ PAGE_LENGTH

	COMMON /CTRLY/ CTRLY

	COMMON /TERM_CHAN/ TERM_CHAN

	EXTERNAL BULLETIN_SUBCOMMANDS,LIB$GET_INPUT,CLI$_NOCOMD,CLI$_ABSENT
	EXTERNAL BULLETIN_MAINCOMMANDS

	PARAMETER PCB$M_BATCH = '4000'X
	PARAMETER LIB$M_CLI_CTRLY = '2000000'X

	CHARACTER*32 INLINE

	CHARACTER*11 UPTIME_DATE
	CHARACTER*8 UPTIME_TIME

C
C  Check to see if CONTROL Y disabled.  If so, then never disable CONTROL Y.
C  Disabling and enabling CONTROL Y is done so that a person can not break
C  while one of the data files is opened, as that would not allow anyone
C  else to modify the files.  However, if CONTROL Y is already disabled,
C  this is not necessary, and should not be done!
C

	CALL LIB$DISABLE_CTRL(LIB$M_CLI_CTRLY,CTRLY)	! Disable CTRL-Y & -C
	CTRLY = CTRLY .AND. LIB$M_CLI_CTRLY
	CALL GETPRIV(ALLOW)			! Check privileges
	IF (ALLOW.EQ.0) THEN		! If no SETPRV privileges...
	   CALL CHECK_PRIV_IO(ERR)	! check privileges on output I/O
	ELSE
	   ERR = 0			! Else we don't have to check them.
	END IF
	CALL LIB$ENABLE_CTRL(CTRLY,)		! Renable CTRLY-Y & -C

	IF (ERR.EQ.1) CALL EXIT			! I/O privilege error, so exit

C
C  Delete any expired bulletins (normal or shutdown ones).
C

	CALL OPEN_FILE(2)
	CALL READDIR(0,IER)		! Get header info from BULLDIR.DAT
	IF (IER.EQ.1) THEN		! Is header present?
	   IER = COMPARE_DATE(NEWEST_EXDATE,' ') ! Yes. Any expired bulls?
	   IF (SHUTDOWN.GT.0) THEN	! Do shutdown bulletins exist?
	      CALL GET_UPTIME(UPTIME_DATE,UPTIME_TIME)
	      IER1 = COMPARE_DATE(SHUTDOWN_DATE,UPTIME_DATE)
	      IF (IER1.EQ.0) IER1 = COMPARE_TIME(SHUTDOWN_TIME,UPTIME_TIME)
	      IF (IER1.LE.0) SHUTDOWN = 0
	   END IF
	   IF (IER.LE.0.OR.IER1.LE.0) CALL UPDATE  ! Need to update
	END IF
	CALL CLOSE_FILE(2)

C
C  Test for /READ & /LOGIN switches.
C

	CALL LIB$GET_FOREIGN(INLINE)

	IER = CLI$DCL_PARSE('BULLETIN'//INLINE,BULLETIN_MAINCOMMANDS)

	READIT = 0
	IF (CLI$PRESENT('READNEW')) READIT = 1	! Test for /READ switch.
	LOGIT = 0
	IF (CLI$PRESENT('LOGIN')) LOGIT = 1	! Test for /LOGIN switch.

C
C  Ignore BULLETIN/READ or BULLETIN/LOGIN if this is a batch process.
C

	IF (READIT.GT.0.OR.LOGIT.GT.0) THEN
	   CALL GETSTS(STS)			! Get process status word
	   IF ((STS.AND.PCB$M_BATCH).GT.0) CALL EXIT	! If BATCH, exit
	END IF

	IER = SYS$ASSIGN('TT',TERM_CHAN,,)	! Assign terminal
C
C  Get page length for the terminal.
C

	CALL GETPAGLEN(PAGE_LENGTH)

C
C  If /LOGIN, display SYSTEM bulletins and subject of non-SYSTEM bulletins.
C

	IF (LOGIT.GT.0) THEN		! Is /LOGIN present?
	   CALL LOGIN(READIT)		! Display SYSTEM bulletins
	   IF (READIT.EQ.0) CALL EXIT	! If not /READ, exit program
	END IF

C
C  Update user's last read bulletin date.  If new bulletins have been
C  added since the last time bulletins have been read, position bulletin
C  pointer so that next bulletin read is the first new bulletin, and
C  alert user.  If /READ switch and no new bulletins, just exit.
C

	CALL UPDATE_READ		! Bulletins added since last read?
	IF (BULL_POINT.EQ.-1) THEN	! BULL_POINT would be bulletin # -1
	   BULL_POINT = 0		! Since its -1, no new bulletins
	   IF (READIT.GT.0) CALL EXIT	! If /READ, just exit
	ELSE IF (READIT.EQ.0) THEN 	! There are new bulletins
	   WRITE(6,1000)		! Alert user of the fact
	END IF				! if not in /READ mode

	IF (READIT.GT.0) CALL READNEW	! /READ mode. READNEW exits the program

C
C  The MAIN loop for processing bulletin commands.
C

	DIR_COUNT = 0	! # directory entry to continue bulletin read from
	READ_COUNT = 0	! # block that bulletin READ is to continue from

	DO WHILE (1)

	   IER = CLI$DCL_PARSE(%VAL(0),BULLETIN_SUBCOMMANDS,LIB$GET_INPUT,
     &		LIB$GET_INPUT,'BULLETIN> ')

	   IF (IER.EQ.RMS$_EOF) THEN
	      GO TO 999	! If no command, exit
	   ELSE IF (IER.EQ.%LOC(CLI$_NOCOMD)) THEN  ! If just RETURN entered
	      LEN_P = 0			! Indicate no parameter in command
	      IF (DIR_COUNT.GT.0) THEN		! If still more dir entries
		 CALL DIRECTORY(DIR_COUNT)	! continue outputting them
	      ELSE				! Else try to read next bulletin
		 CALL READ(READ_COUNT,BULL_POINT+1)  ! or finish old one
	      END IF
	      GO TO 100				! Loop to read new command
	   ELSE IF (.NOT.IER) THEN		! If command has error
	      GO TO 100				! ask for new command
	   END IF

	   DIR_COUNT = 0			! Reinit dir and read pointers
	   READ_COUNT = 0

80	   CALL CLI$GET_VALUE('$VERB',INLINE)	! Get the VERB command
	   IF (INLINE(1:3).EQ.'ADD') THEN	! ADD bulletin command?
	     CALL ADD				! Go add bulletin
	   ELSE IF (INLINE(1:4).EQ.'BACK') THEN	! BACK command?
	     IF (BULL_POINT.LE.1) THEN
	        WRITE(6,1060)
	     ELSE
	        CALL READ(READ_COUNT,BULL_POINT-1)  ! Try to read previous bull
	     END IF
	   ELSE IF (INLINE(1:4).EQ.'DELE') THEN 	! DELETE command?
	     CALL DELETE			! Go delete bulletin
	   ELSE IF (INLINE(1:4).EQ.'DIRE') THEN		! DIRECTORY command?
	     CALL DIRECTORY(DIR_COUNT)		! Get directory of bulletins
	   ELSE IF (INLINE(1:4).EQ.'EXIT') THEN		! EXIT command?
	     CALL EXIT				! Exit from program
	   ELSE IF (INLINE(1:4).EQ.'FILE') THEN		! FILE command?
	     CALL FILE				! Copy bulletin to file
	   ELSE IF (INLINE(1:4).EQ.'HELP') THEN		! HELP command?
	     CALL HELP('BULL.HLB')		! Get help
	   ELSE IF (INLINE(1:4).EQ.'NEXT') THEN		! NEXT command?
	     CALL READ(READ_COUNT,BULL_POINT+1)		! Read next bulletin
	   ELSE IF (INLINE(1:4).EQ.'READ') THEN		! READ command?
	      IER = CLI$GET_VALUE('BULLETIN_NUMBER',BULL_PARAMETER,LEN_P)
	      IF (IER.NE.%LOC(CLI$_ABSENT)) THEN	! Bulletin specified?
	         DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER) BULL_READ	! Yes
		 CALL READ(READ_COUNT,BULL_READ)
	      ELSE
		 CALL READ(READ_COUNT,BULL_POINT+1)
	      END IF
	   ELSE IF (INLINE(1:4).EQ.'REPL') THEN		! REPLACE command?
	      CALL REPLACE			! Replace old bulletin
	   ELSE IF (INLINE(1:3).EQ.'SET') THEN	! SET command?
	      CALL CLI$GET_VALUE('SET_PARAM1',INLINE)
	      IF (INLINE(1:4).EQ.'READ') THEN	! SET READNEW?
		 CALL SET_READNEW(1,1)
	      ELSE IF (INLINE(1:4).EQ.'NORE') THEN
		 CALL SET_READNEW(0,1)
	      END IF
	   ELSE 				! Else bad parameter
	     WRITE(6,1020)			! Inform user of it
	   ENDIF

100	   CONTINUE

	END DO

999	CALL EXIT

1000	FORMAT(' Type READ to read new bulletins.')
1010	FORMAT(Q,A)
1020	FORMAT(' ERROR: Unknown command. Please retype.')
1060	FORMAT(' ERROR: There are no more bulletins.')

	END




	SUBROUTINE ADD
C
C  SUBROUTINE ADD
C
C  FUNCTION: Adds bulletin to bulletin file.
C
	IMPLICIT INTEGER (A - Z)

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE '($SSDEF)'

	CHARACTER*11 INEXDATE,TODAY
	CHARACTER*80 INDESCRIP,INPUT

	INTEGER TIMADR(2)

	DIMENSION SETPRV(2)
	DATA SETPRV/Z10000000,0/		! SYSPRV privileges

C
C  The largest message that can be broadcasted is dependent on system
C  and user quotas.  The following limit is 12 lines of ( 80 characters +
C  CR/LF ) + 2 bells.  This should be more than enough room, as broadcasts
C  shouldn't be too large anyway.
C

	PARAMETER BRDCST_LIMIT = 82*12 + 2
	CHARACTER*(BRDCST_LIMIT) BROAD
	CHARACTER*1 CR/13/,LF/10/,BELL/7/

	COMMON /TERM_CHAN/ TERM_CHAN

	COMMON /CTRLY/ CTRLY

	EXTERNAL CLI$_ABSENT
	EXTERNAL IO$_READVBLK,IO$M_NOECHO,IO$M_PURGE

	CHARACTER*80 MAILEDIT,INLINE
	CHARACTER*32 NODES(10)
    	CHARACTER PASSWORD*31,TEMPUSER*12

	CALL LIB$DISABLE_CTRL(CTRLY,)	! Disable CTRL-Y & -C

	IER = CLI$GET_VALUE('FILESPEC',BULL_PARAMETER,LEN_P)
	IF (IER.NE.%LOC(CLI$_ABSENT)) THEN
	   CALL GETPRIV(ALLOW)		! Does user have SETPRV privileges?
 	   IF (ALLOW.EQ.0) THEN			! If not, then remove SYSPRV
	      CALL SYS$SETPRV(%VAL(0),SETPRV,,)	! privileges when trying to
	   END IF					! create new file.
	   OPEN (UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),STATUS='OLD',READONLY,
     &		 SHARED,ERR=920,FORM='FORMATTED')	! Try opening the file
	   CALL SYS$SETPRV(%VAL(1),SETPRV,,)	! Reset SYSPRV privileges
	END IF

	IER = SYS_TRNLNM('SYS$NET',INLINE,1)
	IF (IER.EQ.SS$_NORMAL) THEN		! Running via DECNET?
	   IER = CLI$GET_VALUE('USERNAME',USERNAME)
	   IF (CLI$GET_VALUE('PASSWORD',PASSWORD).EQ.SS$_NORMAL) THEN
	      CALL CONFIRM_PRIV(USERNAME,PASSWORD,ALLOW)
	   END IF
	ELSE
	   CALL GETPRIV(ALLOW)			! Check privileges
	END IF

	IF (CLI$PRESENT('SYSTEM')) THEN		! Is /SYSTEM switch present?
	   IF (ALLOW.EQ.0) THEN			! If no privileges
	      WRITE(6,1070)			! Tell user
	      RETURN				! and abort
	   END IF
	   SYSTEM = 1				! Set system bit
	ELSE
	   SYSTEM = 0				! Clear system bit
	END IF

	IF (CLI$PRESENT('BROADCAST')) THEN	! Is /BROADCAST switch present?
	   IF (ALLOW.EQ.0) THEN			! If no privileges
	      WRITE(6,1080)			! Tell user
	      RETURN				! and abort
	   END IF
	END IF

	IF (CLI$PRESENT('PERMANENT')) THEN	! Is /PERMANENT switch present?
	   IF (CLI$PRESENT('SHUTDOWN')) THEN
	      WRITE(6,1083)
	      RETURN
	   ELSE IF (ALLOW.EQ.0) THEN		! If no privileges
	      WRITE(6,1081)			! Tell user
	      RETURN				! and abort
	   ELSE
	      SYSTEM = SYSTEM.OR.2		! Set permanent bit
	      INEXDATE = 'PERMANENT'
	      GO TO 8				! Skip expiration date question
	   END IF
	END IF

	IF (CLI$PRESENT('SHUTDOWN')) THEN	! Is /SHUTDOWN switch present?
	   IF (CLI$PRESENT('PERMANENT')) THEN
	      WRITE(6,1083)
	      RETURN
	   ELSE IF (ALLOW.EQ.0) THEN		! If no privileges
	      WRITE(6,1082)			! Tell user
	      RETURN				! and abort
	   ELSE
	      SYSTEM = SYSTEM.OR.4		! Set shutdown bit
	      INEXDATE = 'SHUTDOWN'
	      GO TO 8				! Skip expiration date question
	   END IF
	END IF

	NODE_NUM = 0				! Initialize number of nodes
	IF (CLI$PRESENT('NODES')) THEN		! Decnet nodes specified?
	   LEN = 0				! GET_VALUE crashes if LEN<0
	   DO WHILE (CLI$GET_VALUE('NODES',NODES(NODE_NUM+1),LEN)
     &	    .EQ.SS$_NORMAL)			! Get the specified nodes
	      NODE_NUM = NODE_NUM + 1
	      IF (NODES(NODE_NUM)(LEN-1:LEN).EQ.'::') THEN  ! Remove :: if
		 LEN = LEN - 2				    ! added
	      END IF
	      POINT_NODE = NODE_NUM
	      OPEN (UNIT=9+NODE_NUM,NAME=NODES(NODE_NUM)(1:LEN)//'""::'//
     &		'"TASK=BULLETIN"',ACCESS='SEQUENTIAL',FORM='FORMATTED',
     &		CARRIAGECONTROL='NONE',TYPE='NEW',ERR=940)
	   END DO
	END IF

5	IER = SYS$ASCTIM(,TODAY,,)		! Get today's date
	WRITE(6,1030) TODAY			! Prompt for expiration date
	CALL GET_LINE(INEXDATE,LEN)		! Get input line
	IF (LEN.LE.0) GO TO 910
	DECODE(LEN,'(I<LEN>)',INEXDATE,IOSTAT=IER) NDAYS  ! Is it # days?
	IF (IER.EQ.0) THEN			! If so,
	   IF (NDAYS.LE.0) THEN			! Is # days not in future
	      WRITE(6,1045)			! tell user
	      GO TO 5				! and re-request date
	   ELSE
	      CALL GET_EXDATE(INEXDATE,NDAYS)	! Get expiration date
	   END IF
	END IF
	IF (INEXDATE(2:2).EQ.'-') INEXDATE = '0'//INEXDATE
	CALL STR$UPCASE(INEXDATE,INEXDATE)	! Convert to upper for BINTIM
	IER = SYS$BINTIM(INEXDATE,TIMADR(1))	! Is real date?
	IF (IER.NE.1) THEN			! If not,
7    	   WRITE(6,1040)			! tell user input is wrong
	   GO TO 5				! and re-request date
	END IF
	IER = SYS$ASCTIM(,INEXDATE,TIMADR(1),)
	IER = COMPARE_DATE(INEXDATE,TODAY)	! Compare date with today's
	IF (IER.LE.0) THEN			! If expiration date not future
	   WRITE(6,1045)			! tell user
	   GO TO 5				! and re-request date
	END IF

8	WRITE(6,1050)				! Request header for bulletin
	CALL GET_LINE(INDESCRIP,LENDES)		! Get input line
	IF (LENDES.LE.0) GO TO 910
	IF (LENDES.GT.53) THEN			! If too many characters
	   WRITE(6,1060)			! tell user
	   WRITE(6,2020) INDESCRIP(1:53)	! Show how much would fit
	   GO TO 8				! and re-request header
	END IF

C
C  If file specified in ADD command, read file to obtain bulletin.
C  Else, read the bulletin from the terminal.
C
	
	ICOUNT = 0				! Line count for bulletin

	IF (CLI$PRESENT('EDIT')) THEN		! If /EDIT specified, then
	   LEN = 0
	   IER = LIB$SYS_TRNLOG('MAIL$EDIT',LEN,MAILEDIT)
	   IF (IER.NE.SS$_NORMAL) MAILEDIT = 'SYS$SYSTEM:MAILEDIT'
	   IF (LEN_P.EQ.0) THEN			! If no file param specified
	      CALL LIB$SPAWN('$@'//MAILEDIT//' "" SYS$LOGIN:BULL.SCR')
	      OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='OLD',
     &		 DISPOSE='DELETE',ERR=920,FORM='FORMATTED')
	      LEN_P = 1
	   ELSE
	      CLOSE (UNIT=3)
	      CALL LIB$SPAWN('$@'//MAILEDIT//' '//BULL_PARAMETER(1:LEN_P)
     &			//' SYS$LOGIN:BULL.SCR')
	      OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='OLD',
     &		 DISPOSE='DELETE',ERR=920,FORM='FORMATTED')
	   END IF
	END IF

	IF (LEN_P.GT.0) THEN			! If file param in ADD command
	   DO WHILE(1)				! Read until end of file to
	      READ (3,2000,END=10)		! get record count
	      ICOUNT = ICOUNT + 1
	   END DO
	ELSE					! If no input file
	   OPEN (UNIT=3,STATUS='SCRATCH',FILE='SYS$LOGIN:BULL.SCR',
     &		FORM='FORMATTED')		! Sratch file to save bulletin
	   WRITE (6,1000)		! Request bulletin input from terminal
	   LEN = 81				! Length of input line
	   DO WHILE (LEN.GE.0)			! Input until no more input
	      CALL GET_LINE(INPUT,LEN)		! Get input line
	      IF (LEN.GE.0) THEN		! If good input line entered
		 ICOUNT = ICOUNT + 1		! Increment record count
		 WRITE(3,2010) INPUT(1:LEN)	! Save line in scratch file
	      END IF
	   END DO
	   IF (LEN.EQ.-1) GO TO 910		! CTRL_C entered, error out
10	   IF (ICOUNT.EQ.0) GO TO 910		! No lines entered, error out
	ENDIF

	REWIND (UNIT=3)

	IF (NODE_NUM.GT.0) THEN
	   INLINE = 'ADD'
	   IF (CLI$PRESENT('SYSTEM'))
     &	      INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)//'/SYSTEM'
	   IF (CLI$PRESENT('BROADCAST'))
     &	      INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)//'/BROADCAST'
	   IF (CLI$PRESENT('PERMANENT'))
     &	      INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)//'/PERMANENT'
	   IF (CLI$PRESENT('SHUTDOWN'))
     &	      INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)//'/SHUTDOWN'
	   IF (CLI$PRESENT('BELL'))
     &	      INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)//'/BELL'

	   IO_READ = %LOC(IO$_READVBLK)+%LOC(IO$M_NOECHO)+%LOC(IO$M_PURGE)

	   DO POINT_NODE=1,NODE_NUM	   	! Write out command to nodes
	      WRITE (6,'('' Enter username at node '',A)') NODES(POINT_NODE)
	      WRITE (6,'('' Hit RETURN to use username of local node.'')')
	      READ (5,'(Q,A)',ERR=910,END=910) LEN,TEMPUSER
	      IF (INLINE.NE.'ADD'.OR.LEN.GT.0) THEN
	         WRITE(6,'('' Enter password for node '',2A)')
     &			NODES(POINT_NODE),CHAR(10)
	         IER=SYS$QIOW(,%VAL(TERM_CHAN),%VAL(IO_READ),,,,
     &	                %VAL(%LOC(PASSWORD)),%VAL(31),,,,)
		 INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)//'/PASSWORD='
     &		   //PASSWORD(1:STR$POSITION(PASSWORD,CHAR(13))-1)
	      END IF
	      IF (LEN.EQ.0) TEMPUSER = USERNAME
	      INLINE = INLINE(1:STR$POSITION(INLINE,' ')-1)
     &					//'/USERNAME='//TEMPUSER
	      WRITE (POINT_NODE+9,'(A)',ERR=940) INLINE
	      WRITE (POINT_NODE+9,'(A)',ERR=940) INEXDATE
	      WRITE (POINT_NODE+9,'(A)',ERR=940) INDESCRIP(1:LENDES)
	      DO I=1,ICOUNT
	         READ (3,'(Q,A)') LEN,INPUT
	         WRITE (POINT_NODE+9,'(A)',ERR=940) INPUT(1:LEN)
	      END DO
	      WRITE (6,'('' Bulletin successfully sent to node '',A)')
     &				NODES(POINT_NODE)
	      REWIND (UNIT=3)
	   END DO
	END IF

C
C  Add bulletin to bulletin file and directory entry for to directory file.
C

	CALL OPEN_FILE(2)			! Prepare to add dir entry

	DESCRIP=INDESCRIP(1:LENDES)		! Description header
	EXDATE=INEXDATE				! Expiration datee
	LENGTH = ICOUNT				! Number of records 
	FROM = USERNAME				! Username

	CALL OPEN_FILE(1)			! Prepare to add bulletin

	CALL READDIR(0,IER)			! Get NBLOCKT
	IF (IER.EQ.0) NBLOCK = 0		! If new file, NBLOCK is 0

	CALL COPY_BULL(3,1,NBLOCK+1,IER)	! Add the new bulletin
	IF (IER.NE.0) GO TO 930			! Error in creating bulletinR

	CLOSE (UNIT=1)				! Finished adding bulletin/

	CALL ADD_ENTRY				! Add the new directory entry

	CLOSE (UNIT=2)				! Totally finished with add

CR
C  Broadcast the bulletin if requested.I
CO

	IF (CLI$PRESENT('BROADCAST')) THEN	! Should we broadcast the bull?B
	   REWIND (UNIT=3)			! Yes, rewind the input file
	   IF (CLI$PRESENT('BELL')) THEN	! Include BELL in message?
	      BROAD(1:36) =			! Say who the bulletin is fromL
     &		BELL//BELL//CR//LF//LF//'NEW BULLETIN FROM: '//FROMi
	      START = 37			! Start adding next line herec
	   ELSE
	      BROAD(1:34) =			! Say who the bulletin is fromo
     &		CR//LF//LF//'NEW BULLETIN FROM: '//FROM.
	      START = 35			! Start adding next line herei
	   END IF
	   DO I=1,ICOUNT			! Stuff bulletin into string
	      READ(3,2000) LEN,INPUT		! Read input line
	      END = START + LEN - 1 + 2		! Check how long string will be)
	      IF (END.GT.BRDCST_LIMIT) GO TO 90	! If too much for string, exit.
	      BROAD(START:END) = CR//LF//INPUT(1:LEN)	! Else add new inputS
	      START = END + 1			! Reset pointer
	   END DO
90	   CALL SYS$BRDCST(BROAD(1:START-1)//CR,,,)	! Do the BROADCAST
	END IFR

	CLOSE (UNIT=3)			! Close the input file

100	CALL LIB$ENABLE_CTRL(CTRLY,)	! Enable CTRL-Y & -Cs
	DO I=10,NODE_NUM+9
	   CLOSE (UNIT=I)
	END DOD
	RETURN)

910	WRITE(6,1010)r
	CLOSE (UNIT=3,ERR=100)E
	GOTO 100!

920	WRITE(6,1020)
	CALL SYS$SETPRV(%VAL(1),SETPRV,,)	! Reset SYSPRV privileges
	GOTO 100 

930	WRITE (6,1025)
	CALL CLOSE_FILE(3)t
	CLOSE (UNIT=3) 
	GO TO 100

940	WRITE (6,1015) NODES(POINT_NODE)
	CLOSE (UNIT=3)E
	GO TO 100

1000	FORMAT (' Enter bulletin: End with ctrl-z, cancel with ctrl-c')
1010	FORMAT (' No bulletin was added.')T
1015	FORMAT (' ERROR: Unable to reach node ',A).
1020	FORMAT (' ERROR: Unable to open specified file.')
1025	FORMAT (' ERROR: Unable to add bulletin to bulletin file.')
1030	FORMAT (' Today is ',A11,
     &'. Specify when the bulletin should expire:',/,1x,
     &'Enter specific date, dd-mmm-yyyy, or number of days from today.')
1040	FORMAT (' ERROR: Invalid date format specified.')
1045	FORMAT (' ERROR: Specified date has already passed.')
1050	FORMAT (' Enter description header.  Limit header to 53
     & characters.')
1060	FORMAT (' ERROR: Header > 53 characters. Header would beF
     & truncated to:')
1070	FORMAT (' ERROR: SETPRV privileges are needed for system(
     & bulletins.')s
1080	FORMAT (' ERROR: SETPRV privileges are needed to broadcastC
     & bulletins.')N
1081	FORMAT (' ERROR: SETPRV privileges are needed to permanent 
     & bulletins.')
1082	FORMAT (' ERROR: SETPRV privileges are needed to shutdown
     & bulletins.')a
1083	FORMAT (' ERROR: Permanent and shutdown cannot be specified
     & simultaneously.')
2000	FORMAT(Q,A)
2010	FORMAT(A)
2020	FORMAT(1X,A)e

	END


	SUBROUTINE DELETE
Cn
C  SUBROUTINE DELETE
Cs
C  FUNCTION:  Deletes a bulletin entry from the bulletin file.
Cn
	IMPLICIT INTEGER (A - Z)e

	CHARACTER*107 DIRLINE

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'u

	EXTERNAL CLI$_ABSENT 

	CHARACTER*1 ANSWER 

C1
C  Get the bulletin number to be deleted.L
CX

	IER = CLI$GET_VALUE('BULLETIN_NUMBER',BULL_PARAMETER,LEN_P)
	IF (IER.NE.%LOC(CLI$_ABSENT)) THEN	! Was bulletin specified?
	   DECODE(LEN_P,5,BULL_PARAMETER,ERR=920) BULL_DELETE	! Yes
5	   FORMAT(I<LEN_P>)A
	ELSE IF (BULL_POINT.EQ.0) THEN	! No.  Have we just read a bulletin?
	   GO TO 910			! No, then error.#
	ELSEo
	   BULL_DELETE = BULL_POINT	! Delete the file we are readingl
	END IFb

Ci
C  Check to see if specified bulletin is present, and if the userA
C  is permitted to delete the bulletin.I
CT

	CALL OPEN_FILE(2)

	CALL READDIR(BULL_DELETE,IER)	! Get info for specified bulletin

	IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?O
	   WRITE(6,1030)		! If not, then error out 
	   GOTO 100
	END IF 

	IF (USERNAME.NE.FROM) THEN	! If doesn't match owner of bulletin, 
	   CALL GETPRIV(ALLOW)		! then see if owner has privileges.
	   IF (ALLOW.EQ.0) THEN		! If owner doesn't have privileges,
	      WRITE(6,1040)		! Then error out.o
	      GO TO 100
	   ELSE
	      CALL CLOSE_FILE (2)
	      WRITE (6,1050)		! Make sure user wants to delete it
	      READ (5,'(A)',IOSTAT=IER) ANSWER 
	      CALL STR$UPCASE(ANSWER,ANSWER) 
	      IF (ANSWER.NE.'Y') GO TO 100 
	      CALL OPEN_FILE(2)
	      CALL READDIR(BULL_DELETE,IER) ! Get info for specified bulletin
	      IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found? 
	         WRITE(6,1030)		! If not, then error outL
	         GOTO 100
	      END IFm
	   END IF
	END IFP

CL
C  Delete the bulletin from the bulletin file.
C 

	CALL OPEN_FILE(1)		! Open BULLETIN file

	CALL COPY_BULL(1,BLOCK+LENGTH,BLOCK,IER)! Delete the bulletin byD
						! overwriting rest of file

	CLOSE (UNIT=1) 

Cl
C  Delete the bulletin directory entry..
CR

	CALL DELETE_ENTRY(BULL_DELETE)		! Delete the directory entry!

	TRUNC_SIZE = (NBLOCK*80)/512 + 1	! Truncate bulletin file
	CALL TRUNCATE_FILE(TRUNC_SIZE)		! To remove extra space

	IF ((SYSTEM.AND.4).EQ.4) THEN		! Was entry shutdown bulletin?
	   SHUTDOWN = SHUTDOWN - 1		! Decrement shutdown countI
	END IF(

	CALL UPDATE		! Somewhat a kludgey way of updating latest.
				! bulletin and expired dates.L

100	CALL CLOSE_FILE(2)
900	RETURN

910	WRITE(6,1010)(
	GO TO 900

920	WRITE(6,1020)n
	GO TO 900

1010	FORMAT(' ERROR: You are not reading any bulletin.')
1020	FORMAT(' ERROR: Specified bulletin number has incorrect format.')
1030	FORMAT(' ERROR: Specified bulletin was not found.')
1040	FORMAT(' ERROR: Specified bulletin is not owned by you.')
1050	FORMAT(' Bulletin is not owned by you.',
     &	       ' Are you sure you want to delete it? ',$)

2000	FORMAT(A107)I

	END




	SUBROUTINE DIRECTORY(DIR_COUNT)
C 
C  SUBROUTINE DIRECTORYl
C 
C  FUNCTION: Display directory of bulletins.
CT
	IMPLICIT INTEGER (A - Z)C

	INCLUDE 'BULLDIR.INC'

	COMMON /PAGE/ PAGE_LENGTH

	DATA SCRATCH_D1/0/!

	CALL LIB$ERASE_PAGE(1,1)		! Clear the screenE

	IF (DIR_COUNT.GT.0) GO TO 50		! Skip init steps if this is
						! not the 1st page of directory 

Ce
C  Directory listing is first buffered into temporary memory storage beforeU
C  being outputted to the terminal.  This is to be able to quickly close the
C  directory file, and to avoid the possibility of the user holding the screen,
C  and thus causing the directory file to stay open.  The temporary memory
C  is structured as a linked-list queue, where SCRATCH_D1 points to the header
C  of the queue.  See BULLSUBS.FOR for more description of the queue.C
CO

	IF (SCRATCH_D1.EQ.0) THEN		! Is queue empty?U
	   CALL LIB$GET_VM(100,SCRATCH_D)	! If so, allocated memory
	   CALL MAKE_CHAR(%VAL(SCRATCH_D),88)	! Form a character string
	   SCRATCH_D1 = SCRATCH_D		! Init header pointer
	ELSE					! Else queue is not empty0
	   SCRATCH_D = SCRATCH_D1		! so reinit queue pointers
	END IF					! to the header.

	CALL OPEN_FILE_SHARED(2)		! Get directory filel

	CALL READDIR(0,IER)			! Does directory header exist?)
	IF (IER.EQ.1) THEN			! If so, there are bulletins
	   DO I=1,NBULL				! Copy all bulletins from file
	      CALL READDIR(I,IER)		! Into the queue
	      CALL WRITE_DIR(%VAL(SCRATCH_D),SCRATCH_D)
	   END DO
	END IFM

	CALL CLOSE_FILE(2)			! We don't need file anymore

CT
C  Directory entries are now in queue.  Output queue entries to screen.A
CT

	SCRATCH_D = SCRATCH_D1			! Init queue pointer to header

	DIR_COUNT = 1				! Init directory number counterL

50	DISPLAY = MIN(NBULL-DIR_COUNT+1,PAGE_LENGTH-6)E
			! If more entries then page size, truncate output
	WRITE(6,1000)				! Write header
	DO I=DIR_COUNT,DIR_COUNT+DISPLAY-1i
	   CALL READ_DIR(%VAL(SCRATCH_D),SCRATCH_D)	! Get entry from queue 
	   WRITE(6,2010) I,DESCRIP,FROM,DATE(1:7)//DATE(10:11)t
	END DO

	DIR_COUNT = DIR_COUNT + DISPLAY		! Update directory counter

	IF (DIR_COUNT.GT.NBULL) THEN		! Outputted all entries?R
	   DIR_COUNT = 0			! Yes. Set counter to 0.
	ELSEA
	   WRITE(6,1010)			! Else say there are morep
	END IF

	RETURN

1000	FORMAT('   #',1X,'DESCRIPTION',43X,'FROM',9X,'DATE',/)L
1010	FORMAT(1X,/,' Press RETURN for more...',/)_

2000	FORMAT(A53,A12,A11)
2010	FORMAT(1X,I3,1X,A53,1X,A12,1X,A9)

	END
 O

	SUBROUTINE FILE
C 
C  SUBROUTINE FILE
CA
C  FUNCTION:  Copies a bulletin to a file.
CT
	IMPLICIT INTEGER (A - Z)l
	CHARACTER*107 DIRLINE
	CHARACTER*80 INPUT 

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	INCLUDE 'BULLDIR.INC'

	EXTERNAL CLI$_ABSENT 

	DIMENSION SETPRV(2)
	DATA SETPRV/Z10000000,0/		! SYSPRV privileges

	IER = CLI$GET_VALUE('FILESPEC',BULL_PARAMETER,LEN_P)'

	IF (IER.EQ.%LOC(CLI$_ABSENT)) THEN	! If no file name was specifiedI
	   WRITE(6,1020)		! Write error
	   RETURN			! And returnR
	END IF!

	IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been readN
	   WRITE(6,1010)		! Write error
	   RETURN			! And returnN
	END IFW

	CALL OPEN_FILE_SHARED(2)8

	CALL READDIR(BULL_POINT,IER)	! Get info for specified bulletine

	IF (IER.NE.BULL_POINT+1) THEN	! Was bulletin found?
	   WRITE(6,1030)L
	   CALL CLOSE_FILE(2)		! If not, then error out
	   RETURN
	END IF 

	CALL CLOSE_FILE(2) 

	CALL OPEN_FILE_SHARED(1)	! Open BULLETIN file

	CALL GETPRIV(ALLOW)		! Does user have SETPRV privileges?D
	IF (ALLOW.EQ.0) THEN			! If not, then remove SYSPRV
	   CALL SYS$SETPRV(%VAL(0),SETPRV,,)	! privileges when trying to.
	END IF					! create new file.

	OPEN(UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),ERR=900,
     &		STATUS='NEW',CARRIAGECONTROL='LIST')
	CALL SYS$SETPRV(%VAL(1),SETPRV,,)	! Reset SYSPRV privileges

	IF (CLI$PRESENT('HEADER')) THEN		! Printout header?
	   WRITE(3,1050) DESCRIP		! Output bulletin header info
	   WRITE(3,1060) FROM,DATET
	END IFc

	DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file
	   READ(1'I,2010,ERR=100) INPUT
	   CALL STR$TRIM(INPUT,INPUT,LEN)
	   WRITE(3,2010) INPUT(1:LEN)
	END DOs

	CLOSE (UNIT=3)			! Bulletin copy completed

	WRITE(6,1040) BULL_POINT,BULL_PARAMETER(1:LEN_P)!
					! Show name of file created.	
100	CALL CLOSE_FILE(1)
	RETURN 

900	WRITE(6,1000)N
	CALL SYS$SETPRV(%VAL(1),SETPRV,,)	! Reset BYPASS privileges
	GO TO 100

1000	FORMAT(' ERROR: Error in opening file.')'
1010	FORMAT(' ERROR: You have not read any bulletin.')
1020	FORMAT(' ERROR: No file name was specified.')
1030	FORMAT(' ERROR: Specified bulletin was not found.')
1040	FORMAT(' Bulletin ',I3,' written to ',A)D
1050	FORMAT('DESCRIPTION: ',A53)
1060	FORMAT('FROM: ',A12,' DATE: ',A11,/)I

2000	FORMAT(A107)D
2010	FORMAT(A)

	END




	SUBROUTINE LOGIN(READIT)F
CD
C  SUBROUTINE LOGIN 
Ca
C  FUNCTION: Alerts user of new bulletins upon logging in.
C	Also saves latest login time, which is accessed by FINGER.
CA
	IMPLICIT INTEGER (A - Z)p

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC')

	CHARACTER*23 TODAY/

	COMMON/INPUT_BULL/INPUT
	CHARACTER*80 INPUT 

	COMMON /PAGE/ PAGE_LENGTH

	COMMON /TERM_CHAN/ TERM_CHAN(

	CHARACTER BBOARD_DATE*11,BBOARD_TIME*8	

	LOGICAL*1 CTRL_G/7/

	EXTERNAL IO$_READVBLK,IO$M_NOECHO,IO$M_PURGET

	INCLUDE '($FORIOSDEF)'e

	CALL SYS$ASCTIM(,TODAY,,)		! Get the present time

C=
C  Find user entry in BULLUSER.DAT to update information and
C  to get the last date that bulletins were read.
C 

	CALL OPEN_FILE_SHARED(4)		! Open user file!

10	READ (4,1000,KEY='            ',IOSTAT=IER)	! Get the headere
     &	  USERNAME,NEWEST_DATE,NEWEST_TIME,BBOARD_DATE,BBOARD_TIME,FLAGSL
	IF (IER.EQ.FOR$IOS_SPERECLOC) GO TO 10	! If locked record,try again
	IF (IER.EQ.0) UNLOCK 4			! If no error, unlock read

	CALL GETUSER(USERNAME)			! Get present username

	READ (4,1000,KEY=USERNAME,ERR=20,IOSTAT=IER1) USERNAME,
     &		LOGIN_DATE,LOGIN_TIME,READ_DATE,READ_TIME,FLAGS.
						! Find if there is an entryh

	REWRITE (4,1000) USERNAME,TODAY(1:11),TODAY(13:20),
     &		READ_DATE,READ_TIME,FLAGS	! Update login datee

	IF (FLAGS(1).AND.1) READIT = 1 

	GO TO 30L

20	READ_DATE = ' 5-NOV-1956'	! No entry, so make new one
	READ_TIME = '11:05:56'		! Fake a read date. Set to the past. 
	FLAGS(1) = 0p
	FLAGS(2) = 0 
	WRITE (4,1000,IOSTAT=IER) USERNAME,TODAY(1:11),TODAY(13:20), 
     &		READ_DATE,READ_TIME,FLAGSL
	IF (IER.NE.0) THEN		! Error in writing to user file
	   WRITE (6,1070)		! Tell user of the error
	   CALL CLOSE_FILE(4)		! Close the user fileW
	   CALL EXIT			! Go away...
	END IF:
	CALL CLEANUP_LOGIN		! Good time to delete dead users 
	DIFF = -1			! Force us to look at the bulletins

30	IF (IER.EQ.0.AND.(BBOARD_DATE.NE.TODAY(1:11).OR.! Look for BBOARD mail
     &	     BBOARD_TIME(1:2).NE.TODAY(13:14)) ) THEN	! when hour changes
	   READ (4,1000,KEY='            ')	! Get the header0
     &	     USERNAME,NEWEST_DATE,NEWEST_TIME,BBOARD_DATE,BBOARD_TIME,FLAGS
	   REWRITE (4,1000)			! Rewrite header=
     &	     USERNAME,NEWEST_DATE,NEWEST_TIME,TODAY(1:11),TODAY(13:20),FLAGSe
	   CALL CLOSE_FILE(4)
	   CALL BBOARD			! Convert any BBOARD mail to bulletins
	ELSE 
	   CALL CLOSE_FILE(4)
	   IF (IER.NE.0) CALL EXIT	! If no header, no bulletins
	END IF 
	IF (IER1.NE.0) GO TO 40		! Skip date comparison if new entry.

CE
C  Compare and see if bulletins have been added since the last time 
C  that the user has logged in or used the BULLETIN facility.t
Cf

	DIFF = COMPARE_DATE(LOGIN_DATE,READ_DATE)
	IF (DIFF.EQ.0) DIFF = COMPARE_TIME(LOGIN_TIME,READ_TIME)C
	IF (DIFF.LT.0) THEN		! If read bulletins since last login,
	   LOGIN_TIME = READ_TIME	! then use the read date to compare
	   LOGIN_DATE = READ_DATE	! with the latest bulletin date
	END IF				! to see if should alert user.M

	DIFF = COMPARE_DATE(LOGIN_DATE,NEWEST_DATE)
	IF (DIFF.EQ.0) DIFF = COMPARE_TIME(LOGIN_TIME,NEWEST_TIME)S

C 
C  If there are new bulletins, look for them in BULLDIR.DATE
C  Save all new entries in the SCRATCH_D file BULLCHECK.SCR so
C  that we can close BULLDIR.DAT as soon as possible.T
C(

40	IF (DIFF.LE.0) THEN		! Are there new unread bulletins? 
	   CALL OPEN_FILE_SHARED(2)	! Yes, so go get bulletin directory
	   NEW_BULLS = 0		! Number of new bulletins
	   NSYS = 0			! Number of system bulletinsD
	   CALL READDIR(0,IER)		! Get header info
	   CALL LIB$GET_VM(100,SCRATCH_D)
	   CALL MAKE_CHAR(%VAL(SCRATCH_D),88)
	   SCRATCH_D1 = SCRATCH_D
	   DO ICOUNT = NBULL,1,-1
	      CALL READDIR(ICOUNT,IER)A
	      IF (IER1.EQ.0) THEN ! Is this a totally new user?
				  ! No. Is bulletin system or from same user?p
	         DIFF = COMPARE_DATE(LOGIN_DATE,DATE) ! No, so compare date
	         IF (DIFF.EQ.0) DIFF = COMPARE_TIME(LOGIN_TIME,TIME) 
		 IF (DIFF.GT.0) GO TO 100W
		 IF (USERNAME.NE.FROM.OR.SYSTEM) THEN1
	            IF (DIFF.LE.0) THEN			 ! Is bulletin new?
			CALL WRITE_DIR(%VAL(SCRATCH_D),SCRATCH_D)
			NEW_BULLS = NEW_BULLS + 1	 ! Yep, so save it 
		        IF (SYSTEM) NSYS = NSYS + 1P
	            END IF)
		 END IF	
	      ELSE			! Totally new user, save all bulletins
		 CALL WRITE_DIR(%VAL(SCRATCH_D),SCRATCH_D)
		 NEW_BULLS = NEW_BULLS + 1
		 IF (SYSTEM) NSYS = NSYS + 1
	      END IFC
	   END DO
100	   CALL CLOSE_FILE(2) 

C 
C  Review new directory entries.  If there are system bulletins,
C  copy the system bulletin into SCRATCH_D file BULLSYS.SCR for outputting
C  to the terminal.  If there are simple bulletins, just output theW
C  header information.
CE

	   IF (NEW_BULLS.EQ.0) CALL EXITe
	   IO_READ = %LOC(IO$_READVBLK)+%LOC(IO$M_NOECHO)+%LOC(IO$M_PURGE)N
	   PAGE = 0
	   NEW_BULLS = NEW_BULLS - NSYS
	   IF (NSYS.GT.0) THEN		! Are there any system bulletins?
	      WRITE (6,1026) CTRL_G	! Yep...U
	      PAGE = PAGE + 1
	      CTRL_G = 0		! Don't ring bell for non-system bulls!
	      CALL OPEN_FILE_SHARED(1) 
	      CALL LIB$GET_VM(92,SCRATCH_B)
	      CALL MAKE_CHAR(%VAL(SCRATCH_B),80)i
	      SCRATCH_B1 = SCRATCH_BO
	      SCRATCH_D = SCRATCH_D1n
	      DO WHILE (NSYS.GT.0)	! Find which new bulls are systemC
		 CALL READ_DIR(%VAL(SCRATCH_D),SCRATCH_D)
		 IF (SYSTEM) THEN	! If it is a system bulletin
	 	    INPUT = ' '
		    CALL WRITE_BULL(%VAL(SCRATCH_B),SCRATCH_B)
		    DO I=BLOCK,BLOCK+LENGTH-1	! Copy the bulletin to SCRATCH_D
		       READ(1'I,1050,ERR=999) INPUT
		       CALL WRITE_BULL(%VAL(SCRATCH_B),SCRATCH_B) 
		    END DO
	            NSYS = NSYS - 1	! Decrement system bulletin count
		 END IFw
	      END DO 
	      CALL CLOSE_FILE(1)L
	      PAGE = 1W
	      SCRATCH_B = SCRATCH_B1 
	      DO WHILE (SCRATCH_B.NE.0)	! Write out the system bulletinsD
		 CALL READ_BULL(%VAL(SCRATCH_B),SCRATCH_B)
		 CALL STR$TRIM(INPUT,INPUT,LEN)M
	 	 IF (SCRATCH_B.NE.0) THEN
		   IF (PAGE.EQ.PAGE_LENGTH-2) THEN	! If at end of screen
		      WRITE(6,1080)	! Ask for input to proceed to next pageU
		      IER = SYS$QIOW(,%VAL(TERM_CHAN),%VAL(IO_READ),,,,h
     &		      %VAL(%LOC(INREAD)),%VAL(1),,,,).
	              CALL LIB$ERASE_PAGE(1,1)		! Clear the screen 
		      WRITE(6,1065) INPUT(1:LEN)
		      PAGE = 1
		   ELSE 
		      WRITE(6,1060) INPUT(1:LEN)
		      PAGE = PAGE + 1L
		   END IFR
		 END IF-
	      END DO 
150	      WRITE(6,1050)		! Write delimiting blank line
	   END IF
	   SCRATCH_D = SCRATCH_D1
	   IF (NEW_BULLS.GT.0) THEN	! Are there new non-system bulletins?
	      IF (PAGE.NE.0) THEN	! Yep...(
		 WRITE(6,1080)		! Ask for input to proceed to next page(
		 IER = SYS$QIOW(,%VAL(TERM_CHAN),%VAL(IO_READ),,,,
     &		      %VAL(%LOC(INREAD)),%VAL(1),,,,)2
		 CALL LIB$ERASE_PAGE(1,1)	! Clear the screen
	         WRITE(6,1028) CTRL_G
	      ELSE)
	         WRITE(6,1027) CTRL_G
	      END IF'
	      WRITE(6,1020)
	      WRITE(6,1025)
	      PAGE = 3F
	      DO WHILE (SCRATCH_D.NE.0)
	         CALL READ_DIR(%VAL(SCRATCH_D),SCRATCH_D)
	         IF (.NOT.SYSTEM.AND.SCRATCH_D.NE.0) THEN
		   IF (PAGE.EQ.PAGE_LENGTH-2) THEN	! If at end of screen
		      WRITE(6,1080)	! Ask for input to proceed to next page 
		      IER = SYS$QIOW(,%VAL(TERM_CHAN),%VAL(IO_READ),,,,a
     &		      %VAL(%LOC(INREAD)),%VAL(1),,,,))
	              CALL LIB$ERASE_PAGE(1,1)		! Clear the screen5
		      PAGE = 1
                      WRITE(6,1045) DESCRIP,FROM,DATEt
		   ELSEi
		      PAGE = PAGE + 1t
                      WRITE(6,1040) DESCRIP,FROM,DATEr
		   END IFe
		 END IFw
	      END DO&
	   END IF
	   IF (NEW_BULLS.GT.0.AND.READ_DATE.EQ.' 5-NOV-1956') THENs
	      WRITE (6,1035)	! Tell novice how to read the non-system bulls
	   ELSE
	      WRITE(6,1030)
	   END IF
	END IFF

998	RETURN

999	CALL CLOSE_FILE(1)	! Just in case bulletins gets deleted
	GO TO 998		! while we are trying to read it (unlikely) 

1000	FORMAT(A12,A11,A8,A11,A8,2A4)
1005	FORMAT(A53,A12,A11,A8,A4,A11,A4)c
1020	FORMAT(' DESCRIPTION',43X,'FROM',9X,'DATE')
1025	FORMAT(' -----------',43X,'----',9X,'----')
1026	FORMAT(' ',33('*'),'SYSTEM NOTICES',33('*'),A1)
1027	FORMAT(' ',33('*'),'NEW BULLETINS',34('*'),A1)h
1028	FORMAT('+',33('*'),'NEW BULLETINS',34('*'),A1)C
1030	FORMAT(' ',80('*'))
1035	FORMAT(' ',14('*'),
     &   'USE THE BULLETIN COMMAND TO READ THE ABOVE BULLETINS',14('*'))
1040	FORMAT(' ',A53,1X,A12,1X,A11)
1045	FORMAT(' ',A53,1X,A12,1X,A11)
1050	FORMAT(A)
1060	FORMAT(1X,A)
1065	FORMAT('+',A)
1070	FORMAT(' ERROR: Cannot add new entry to BULLETIN user file.')
1080	FORMAT(' ',/,' HIT any key for next page....'))

	END




	SUBROUTINE READ(READ_COUNT,BULL_READ)
CA
C  SUBROUTINE READ
CE
C  FUNCTION: Reads a specified bulletin.
C(
	IMPLICIT INTEGER (A - Z) 

	COMMON /POINT/ BULL_POINT

	INCLUDE 'BULLDIR.INC'

	COMMON/INPUT_BULL/INPUT
	CHARACTER*80 INPUT!

	COMMON /READIT/ READITd

	COMMON /PAGE/ PAGE_LENGTH

	DATA SCRATCH_B1/0/l

	CALL LIB$ERASE_PAGE(1,1)		! Clear screenm
	END = 0					! Nothing outputted on screen

	IF (READ_COUNT.GT.0) GO TO 100		! Skip init steps if this isc
						! not first page of bulletin

	IF (BULL_READ.GT.0) THEN		! Valid bulletin number?	
	   CALL OPEN_FILE_SHARED(2)
	   CALL READDIR(BULL_READ,IER)		! Get bulletin directory entrym
	   CALL CLOSE_FILE(2)
	ELSEA
	   IER = 0L
	END IFe

	IF (IER.NE.BULL_READ+1) THEN		! Was bulletin found?
	   WRITE(6,1030)			! If not, then error out
	   GOTO 900
	END IFe

	BULL_POINT = BULL_READ			! Update bulletin counterS

	WRITE(6,1040) BULL_POINT		! Output bulletin header info
	WRITE(6,1050) DESCRIP
	WRITE(6,1060) FROM,DATE,EXDATE 

	END = 4					! Outputted 4 lines to screen

	READ_COUNT = BLOCK			! Init bulletin record counter

CC
C  Each page of the bulletin is buffered into temporary memory storage beforeN
C  being outputted to the terminal.  This is to be able to quickly close the
C  bulletin file, and to avoid the possibility of the user holding the screen,
C  and thus causing the bulletin file to stay open.  The temporary memory)
C  is structured as a linked-list queue, where SCRATCH_B1 points to the header
C  of the queue.  See BULLSUBS.FOR for more description of the queue.l
C 

	IF (SCRATCH_B1.NE.0) THEN		! Is queue empty?L
	   SCRATCH_B = SCRATCH_B1		! No, set queue pointer to head
	ELSE					! Else if queue is empty
	   CALL LIB$GET_VM(92,SCRATCH_B)	! Allocate first recordT
	   CALL MAKE_CHAR(%VAL(SCRATCH_B),80)	! Form into character stringh
	   SCRATCH_B1 = SCRATCH_B		! Init header pointerc
	END IFu

100	SCRATCH_B = SCRATCH_B1			! Init queue pointer to headery
	IF (READIT.GT.0) END = END - 2		! /READ can output 2 more lines
	DISPLAY = MIN(LENGTH,PAGE_LENGTH-END-4)	! Figure how much can outputR
	CALL OPEN_FILE_SHARED(1)		! Get bulletin file
	DO I=READ_COUNT,READ_COUNT+DISPLAY-1	! Get page full from bulletinb
	   READ(1'I,2000,IOSTAT=IER) INPUT			! Read bulletin record
	   IF (IER.NE.0) GO TO 105n
	   CALL WRITE_BULL(%VAL(SCRATCH_B),SCRATCH_B)	! Save record in queue0
	END DO(
	GO TO 107

105	DISPLAY = I - READ_COUNT	! If read error, output only this much 
	LENGTH = DISPLAY		! This forces the bulletin read to end 

107	CALL CLOSE_FILE(1)			! End of bulletin file read

CN
C  Bulletin page is now in temporary memory, so output to terminal.C
C  Note that if this is a /READ, the first line will have problems with_
C  the usual FORMAT statement.  It will cause a blank line to be outputted
C  at the top of the screen.  This is because of the input QIO at theo
C  end of the previous page.  The output gets confused and thinks it muste
C  end the previous line.  To prevent that, the first line of a new page
C  in a /READ must use a different FORMAT statement to surpress the CR/LF.
Ce

	SCRATCH_B = SCRATCH_B1			! Reinit queue pointer to head
	DO I=READ_COUNT,READ_COUNT+DISPLAY-1	! Output page to terminal 
	   CALL READ_BULL(%VAL(SCRATCH_B),SCRATCH_B)	! Get the queue record
	   CALL STR$TRIM(INPUT,INPUT,LEN)		! Strip leading blanks
	   IF (I.EQ.READ_COUNT.AND.I.NE.BLOCK.AND.READIT.GT.0) THEN
	      WRITE(6,2020) INPUT(1:LEN)	! (See above comments)
	   ELSE
	      WRITE(6,2010) INPUT(1:LEN)h
	   END IF
	END DOC

110	READ_COUNT = READ_COUNT + DISPLAY	! Update bull record counter

	LENGTH = LENGTH - DISPLAY		! Length of remaining recordso
	IF (LENGTH.EQ.0) THEN			! If no more recordsP
	   READ_COUNT = 0			! init bulletin record counterD
	ELSE IF (READIT.EQ.0) THEN		! Else if this is not /READ
	   WRITE(6,1070)			! say there is more of bulletinU
	END IFo

900	RETURN

910	WRITE(6,1010)C
	GO TO 900

1010	FORMAT(' ERROR: You are not reading any bulletin.')
1030	FORMAT(' ERROR: Specified bulletin was not found.')
1040	FORMAT('+BULLETIN NUMBER: ',I3)
1050	FORMAT(' DESCRIPTION: ',A53)u
1060	FORMAT(' FROM: ',A12,' DATE: ',A11,' EXPIRES: ',A11,/)R
1070	FORMAT(1X,/,' Press RETURN for more...',/)C

2000	FORMAT(A)
2010	FORMAT(1X,A)o
2020	FORMAT('+',A)

	END




	SUBROUTINE READNEW6
C
C  SUBROUTINE READNEWe
Ca
C  FUNCTION: Displays new non-system bulletins with prompts between bulletins.
CN

	IMPLICIT INTEGER (A-Z)_

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	INCLUDE 'BULLDIR.INC'

	COMMON /POINT/ BULL_POINT

	COMMON /TERM_CHAN/ TERM_CHAN 

	EXTERNAL IO$_READVBLK,IO$M_NOECHO,IO$M_PURGEL

	CHARACTER*1 INREADl

	LEN_P = 0			! Tells read subroutine there isr
					! no bulletin paramter0
	IO_READ = %LOC(IO$_READVBLK)+%LOC(IO$M_NOECHO)+%LOC(IO$M_PURGE)
	WRITE(6,1000)		! Ask if want to read new bulletins
	IER=SYS$QIOW(,%VAL(TERM_CHAN),%VAL(IO_READ),,,,	! Use QIOsA
     &	   %VAL(%LOC(INREAD)),%VAL(1),,,,)	! So no prompt is needed
	CALL STR$UPCASE(INREAD,INREAD)	! Make input upper caseO
	IF (INREAD.EQ.'N') CALL EXIT		! If NO, exit

5	CALL READ(READ_COUNT,BULL_POINT+1)	! Read next bulletinU
	IF (READ_COUNT.EQ.0) THEN		! Is full bulletin displayed?P
	   CALL OPEN_FILE_SHARED(2)		! If so, see if more new bulls
10	   CALL READDIR(BULL_POINT+1,IER)
	   IF (IER.NE.BULL_POINT+2) THEN	! If no new bulls, exit.
	      CALL CLOSE_FILE(2)V
	      IF (INREAD.EQ.'N') WRITE (6,1010)
	      CALL EXIT
	   ELSE IF (SYSTEM) THEN		! If bull is system
	      BULL_POINT = BULL_POINT + 1	! If so, just skip it.t
	      GO TO 10
	   END IF
	   CALL CLOSE_FILE(2)
	END IFa

	IF (READ_COUNT.EQ.0) THEN		! Prompt user in between
	   WRITE(6,1020)			! full screens or end of bull.
	ELSE
	   WRITE(6,1030)L
	END IFR

	IER=SYS$QIOW(,%VAL(TERM_CHAN),%VAL(IO_READ),,,,	! Use QIOs1
     &	   %VAL(%LOC(INREAD)),%VAL(1),,,,)	! So no prompt is needed
	CALL STR$UPCASE(INREAD,INREAD)	! Convert input to upper caseL

	IF (INREAD.EQ.'Q') THEN		! If Q , then QUIT
	   CALL EXIT
	ELSE IF (INREAD.EQ.'N'.AND.READ_COUNT.GT.0) THENv
				! If NEXT and last bulletins not finishedn
	   READ_COUNT = 0			! Reset read bulletin counter
	   CALL OPEN_FILE_SHARED(2)		! Look for NEXT bulletin
20	   CALL READDIR(BULL_POINT+1,IER)
	   IF (IER.NE.BULL_POINT+2) THEN	! If no NEXT bulletinR
	      CALL CLOSE_FILE(2)		! Exit)
	      WRITE(6,1010)
	      CALL EXIT
	   ELSE IF (SYSTEM) THEN		! Else if NEXT bulletin SYSTEMR
	      BULL_POINT = BULL_POINT + 1	! Skip it
	      GO TO 20			! Look for more bulletins
	   END IF
	   CALL CLOSE_FILE(2)
	END IFi
	GO TO 5

1000	FORMAT(' Read new bulletins? Type N(No) or any otherU
     & key for yes',$)
1010	FORMAT(' No more messages.')(
1020	FORMAT(1X,80('-'),/,' Type Q(Quit) or any other key for
     & next message.',$)
1030	FORMAT(1X,80('-'),/,' Type Q(Quit), N(Next message), or
     & any other key for MORE... ',$) 

	END




	SUBROUTINE REPLACEB
CS
C  SUBROUTINE REPLACE0
C
C  FUNCTION: Replaces existing bulletin to bulletin file.(
CR
	IMPLICIT INTEGER (A - Z)b

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC't

	CHARACTER*11 INEXDATE
	CHARACTER*80 INDESCRIP,INPUTA
	CHARACTER*1 ANSWER:

	INTEGER TIMADR(2)

	COMMON /TERM_CHAN/ TERM_CHAND

	COMMON /CTRLY/ CTRLYI

	EXTERNAL CLI$_ABSENTT

	DIMENSION SETPRV(2)
	DATA SETPRV/Z10000000,0/		! SYSPRV privileges

	LOGICAL*1 DOALL

Co
C  Get the bulletin number to be replaced.
CP
	IF (.NOT.CLI$PRESENT('NUMBER')) THEN	! No number has been specified
	   IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been read
	      WRITE (6,1005)		! Tell user of the error
	      RETURN			! and return
	   END IF
	   NUMBER_PARAM = BULL_POINT	! Replace the bulletin we are readingA
	ELSEE
	   CALL CLI$GET_VALUE('NUMBER',BULL_PARAMETER,LEN_P))
	   DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER) NUMBER_PARAM
	END IFn

Cr
C  Check to see if specified bulletin is present, and if the usert
C  is permitted to replace the bulletin.
C 

	CALL OPEN_FILE_SHARED(2)s

	CALL READDIR(NUMBER_PARAM,IER)	! Get info for specified bulletinh

	IF (IER.NE.NUMBER_PARAM+1) THEN	! Was bulletin found?
	   WRITE (6,1015)		! If not, tell the personR
	   GOTO 100			! and error out
	END IFa

	IF (USERNAME.NE.FROM) THEN	! If doesn't match owner of bulletin,U
	   CALL GETPRIV(ALLOW)		! then see if owner has privileges.
	   IF (ALLOW.EQ.0) THEN		! If owner doesn't have privileges,T
	      WRITE(6,1090)		! Then error out.i
	      GO TO 100
	   ELSE
	      CALL CLOSE_FILE(2)	! Let go of the file
	      WRITE (6,1100)		! Make sure user wants to delete it
	      READ (5,'(A)',IOSTAT=IER) ANSWER	! Get his answer
	      CALL STR$UPCASE(ANSWER,ANSWER)	! Convert input to uppercase
	      IF (ANSWER.NE.'Y') GO TO 100	! If not Yes, then exit 
	   END IF
	END IFW

	CALL CLOSE_FILE(2))

CN
C  If no switches were given, replace the full bulletinM
CL

	DOALL = .FALSE.

	IF ((.NOT.CLI$PRESENT('EXPIRATION')).AND.
     &	   (.NOT.CLI$PRESENT('HEADER')).AND.A
     &	   (.NOT.CLI$PRESENT('TEXT'))) THEN
	   DOALL = .TRUE.
	END IFE

	CALL LIB$DISABLE_CTRL(CTRLY,)	! Disable CTRL-Y & -C

5	IF (CLI$PRESENT('EXPIRATION').OR.DOALL) THEN
	   WRITE(6,1030)			! Prompt for expiration date
	   READ(5,2000,END=910,ERR=7) LEN,INEXDATE 
	   CALL STR$UPCASE(INEXDATE,INEXDATE)	! Convert to upper for BINTIM
	   IF (LEN.EQ.0) GO TO 910)
	   IER = SYS$BINTIM(INEXDATE,TIMADR(1))	! Is date format valid?
	   IF ((IER.AND.1).NE.1) THEN		! If not,4
7	      WRITE(6,1040)			! tell user 
	      GO TO 5				! and re-request date1
	   END IF
	   IER = SYS$ASCTIM(,INEXDATE,TIMADR(1),)
	   IER = COMPARE_DATE(INEXDATE,' ')	! Compare date with today's
	   IF (IER.LE.0) THEN			! If expiration date not future
	      WRITE(6,1045)			! tell user
	      GO TO 5				! and re-request datei
	   END IF
	END IF 

8	IF (CLI$PRESENT('HEADER').OR.DOALL) THEN
	   WRITE(6,1050)			! Request header for bulletinn
	   READ(5,2000,END=910,ERR=910) LEN,INDESCRIP
	   IF (LEN.EQ.0) GO TO 910		! If no header, don't add bullM
	   IF (LEN.GT.53) THEN			! If too many characters
	      WRITE(6,1060)			! tell user
	      WRITE(6,2020) INDESCRIP(1:53)	! Show how much would fit
	      GO TO 8				! and re-request headerh
	   END IF
	END IFe


	IF (CLI$PRESENT('TEXT').OR.DOALL) THEN
CD
C  If file specified in REPLACE command, read file to obtain bulletin.
C  Else, read the bulletin from the terminal.h
C 
	 
	  ICOUNT = 0				! Line count for bulletin

	  IER = CLI$GET_VALUE('FILESPEC',BULL_PARAMETER,LEN_P)C
	  IF (IER.NE.%LOC(CLI$_ABSENT)) THEN	! If file param in ADD command

	   CALL GETPRIV(ALLOW)		! Does user have SETPRV privileges?
	   IF (ALLOW.EQ.0) THEN			! If not, then remove SYSPRVy
	     CALL SYS$SETPRV(%VAL(0),SETPRV,,)	! privileges when trying toe
	   END IF					! create new file.A

	   OPEN (UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),STATUS='OLD',READONLY,
     &		 SHARED,ERR=920,FORM='FORMATTED')	! Try opening the file

	   CALL SYS$SETPRV(%VAL(1),SETPRV,,)	! Reset SYSPRV privilegesI

	   DO WHILE(1)				! Read until end of file to
	      READ (3,2000,END=10)		! get record countp
	      ICOUNT = ICOUNT + 1
	   END DO
	  ELSE					! If no input file
	   OPEN (UNIT=3,STATUS='SCRATCH',FILE='SYS$LOGIN:BULL.SCR',
     &		FORM='FORMATTED')		! Scratch file to save bulletin
	   WRITE (6,1000)		! Request bulletin input from terminal
	   LEN = 81				! Length of input line
	   DO WHILE (LEN.GE.0)			! Input until no more input 
	      CALL GET_LINE(INPUT,LEN)		! Get input line 
	      IF (LEN.GE.0) THEN		! If good input line enterede
		 ICOUNT = ICOUNT + 1		! Increment record count
		 WRITE(3,2010) INPUT(1:LEN)	! Save line in scratch file 
	      END IF 
	   END DO
	   IF (LEN.EQ.-1) GO TO 910		! CTRL_C entered, error out
10	   IF (ICOUNT.EQ.0) GO TO 910		! No lines entered, error out
	  ENDIF

	  REWIND (UNIT=3)
	END IFD

C 
C  Add bulletin to bulletin file and directory entry for to directory file.s
Cu

	CALL OPEN_FILE(2)			! Prepare to add dir entryF

	CALL READDIR(NUMBER_PARAM,IER)		! Get info for bulletin
	CALL READDIR(0,IER)			! Get directory headerE

	IF (CLI$PRESENT('TEXT').OR.DOALL) THEN	! If text has been replacedt
	   CALL OPEN_FILE(1)			! Prepare to add bulletinT
	   IF (ICOUNT.LT.LENGTH) THEN		! If new bulletin smaller...
	      CALL COPY_BULL(3,1,BLOCK,IER)	! Replace old bulletinH
	      CALL COPY_BULL(1,BLOCK+LENGTH,BLOCK+ICOUNT,IER)
						! Move up any future bulletins
	   ELSE IF (ICOUNT.EQ.LENGTH) THEN	! If new bulletin same size 
	      CALL COPY_BULL(3,1,BLOCK,IER)	! Replace old bulletin	
	   ELSE					! If new bulletin is larger...	
	      IF (NBULL.GT.NUMBER_PARAM) THEN	! If there are future bulletins
	         DO I=NBLOCK,BLOCK+LENGTH,-1	! Move future bulletins down
	            READ (1'I,'(A80)') INPUTH
		    WRITE (1'I+ICOUNT-LENGTH,'(A80)') INPUT 
	         END DO
	      END IF_
	      CALL COPY_BULL(3,1,BLOCK,IER)	! Replace old bulletinD
	   END IF

	   CLOSE (UNIT=1)

	   IF (ICOUNT.NE.LENGTH) THEN		! If new bull different size
	      DIFF = ICOUNT - LENGTH		! Get difference in sizeS
	      CALL READDIR(NUMBER_PARAM,IER)	! Get directory entryD
	      LENGTH = ICOUNT			! Update size
	      CALL WRITEDIR(NUMBER_PARAM,IER)	! Write new directory entry
	      DO I=NUMBER_PARAM+1,NBULL		! Fix sizes of future bulletins,
		 CALL READDIR(I,IER)
		 BLOCK = BLOCK + DIFF	
		 CALL WRITEDIR(I,IER)L
	      END DOL
	      NBLOCK = NBLOCK + DIFF		! Update NBLOCK
	      IF (DIFF.LT.0) THEN		! If bulletin file smaller
		 TRUNC_SIZE = (NBLOCK*80)/512 + 1	! Truncate fileE
		 CALL TRUNCATE_FILE(TRUNC_SIZE)	
	      END IF0
	      CALL WRITEDIR(0,IER)E
	   END IF
	END IFD

	CALL READDIR(NUMBER_PARAM,IER)5
	IF (CLI$PRESENT('HEADER').OR.DOALL) DESCRIP=INDESCRIP(1:53)
						! Update description header 
	IF (CLI$PRESENT('EXPIRATION').OR.DOALL) EXDATE=INEXDATE
						! Update expiration date
	CALL WRITEDIR(NUMBER_PARAM,IER)

	DIFF = COMPARE_DATE(EXDATE,NEWEST_EXDATE)	! Compare expirationC
	IF (DIFF.LT.0) THEN		! If it's the oldest expiration bull
	   NEWEST_EXDATE = EXDATE		! Update the header in
	   CALL WRITEDIR(0,IER)		! the directory file
	END IF 

	CALL CLOSE_FILE(2)			! Totally finished with replace6

	CLOSE (UNIT=3)E

100	CALL LIB$ENABLE_CTRL(CTRLY,)	! Enable CTRL-Y & -C.
	RETURN 

910	WRITE(6,1010)L
	CLOSE (UNIT=3,ERR=100) 
	GOTO 100(

920	WRITE(6,1020)C
	CALL SYS$SETPRV(%VAL(1),SETPRV,,)	! Reset SYSPRV privileges
	GOTO 100n

1000	FORMAT (' Enter bulletin: End with ctrl-z, cancel with ctrl-c')
1005	FORMAT (' ERROR: You are not reading any bulletin.')&
1010	FORMAT (' No bulletin was replaced.')
1015	FORMAT (' ERROR: Specified bulletin was not found.')
1020	FORMAT (' ERROR: Unable to open specified file.')
1030	FORMAT (' Enter expiration date of bulletin: dd-mmm-yyyy') 
1040	FORMAT (' ERROR: Invalid date format specified.')
1045	FORMAT (' ERROR: Specified date has already passed.')
1050	FORMAT (' Enter description header.  Limit header to 53
     & characters.')
1060	FORMAT (' ERROR: Header > 53 characters. Header would be
     & truncated to:')
1090	FORMAT(' ERROR: Specified bulletin is not owned by you.')
1100	FORMAT(' Bulletin is not owned by you.',o
     &	       ' Are you sure you want to replace it? ',$)
2000	FORMAT(Q,A)
2010	FORMAT(A)
2020	FORMAT(1X,A) 

	END




	SUBROUTINE UPDATE
C5
C  SUBROUTINE UPDATE
CX
C  FUNCTION:  Searches for bulletins that have expired and deletes them.
C7
C  NOTE:  Assumes directory file is already opened.8
CR
	IMPLICIT INTEGER (A - Z)I
	CHARACTER*107 DIRLINE

	INCLUDE 'BULLDIR.INC'

	CHARACTER*11 TEMP_DATE/'5-NOV-2000'/	! Default exp date if no bulls
	CHARACTER*11 TEMP_EXDATEF
	CHARACTER*8 TEMP_TIME

	NEW_EX = 0				! Init expiration flag

	CALL OPEN_FILE(1)			! Open both bulletin files,

	BULL_ENTRY = 1				! Init bulletin pointer
	UPDATE_DONE = 0			! Flag showing bull has been deleted 

	DO WHILE (1)'
	   CALL READDIR(BULL_ENTRY,IER)		! Get next directory entry
	   IF (IER.EQ.BULL_ENTRY) GO TO 100	! ERROR: Not foundb
	   IF (SYSTEM.LE.1.OR.(SHUTDOWN.EQ.0	! If not permanent, or shutdown
     &	     .AND.(SYSTEM.AND.4).EQ.4)) THEN	! bulletin and /SHUT specified?
	    IF ((SYSTEM.AND.4).EQ.4) THEN	! Shutdown bulletin?
	       DIFF = 0				! If so, delete it
	    ELSE!
	       DIFF = COMPARE_DATE(EXDATE,' ')	! Has expiration date passed?_
	    END IFO
	    IF (DIFF.LE.0) THEN			! If so then delete bulletin 
	      CALL COPY_BULL(1,BLOCK+LENGTH,BLOCK,IER) ! Delete the bulletin by
						    ! rewriting rest of file
	      CALL DELETE_ENTRY(BULL_ENTRY)	! Delete bulletin entry
	      UPDATE_DONE = 1			! Set bulletin deleted flag
	    ELSE IF (SYSTEM.LE.1) THEN		! Expiration date hasn't passed
		! If a bulletin is deleted, we'll have to update the latestB
		! expiration date. The following does that.e
	      IF (DIFF.LT.NEW_EX.OR.NEW_EX.EQ.0) THEN
	         TEMP_EXDATE = EXDATE		! If this is the latest expA
	         NEW_EX = DIFF			! date seen so far, save it.
	      END IF 
	      BULL_ENTRY = BULL_ENTRY + 1	! Increment bulletin counterh
	      TEMP_DATE = DATE			! Keep date so when we quitf
	      TEMP_TIME = TIME			! search, we'll have the
	    END IF				! latest bulletin datee
	   ELSE
	    BULL_ENTRY = BULL_ENTRY + 1
	   END IF
	END DOr

100	DATE = NEWEST_DATE
	TIME = NEWEST_TIMEs
	CALL READDIR(0,IER)
	NEWEST_EXDATE = TEMP_EXDATE
	NEWEST_DATE = TEMP_DATE
	NEWEST_TIME = TEMP_TIME
	CALL WRITEDIR(0,IER)u
	CLOSE(UNIT=1)

	IF (UPDATE_DONE.EQ.1) THEN		! If any deletions occurred
	   TRUNC_SIZE = (NBLOCK*80)/512 + 1	! truncate bulletin file.
	   CALL TRUNCATE_FILE(TRUNC_SIZE)
	END IF 

	IF (TEMP_DATE.NE.DATE.OR.TEMP_TIME.NE.TIME) THENH
	   NEWEST_DATE = TEMP_DATE		! If the newest bulletin date
	   NEWEST_TIME = TEMP_TIME		! has been changed, it must
	   CALL UPDATE_LOGIN			! be changed in BULLUSER.DAT
	END IFC

	RETURN!

1000	FORMAT(A11,A11,A8,A4,A4)I
1020	FORMAT(A107) 

	END



	SUBROUTINE UPDATE_READn
C
C  SUBROUTINE UPDATE_READL
CT
C  FUNCTION:
C	Store the latest date that user has used the BULLETIN facility. 
C	If new bulletins have been added, alert user of the fact and
C	set the next bulletin to be read to the first new bulletin.i
Ce
C  OUTPUTS:(
C	BULL_POINT  -  If -1, no new bulletins to read, else there are.)
CS

	IMPLICIT INTEGER (A - Z)(

	COMMON /POINT/ BULL_POINT

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC' 

	CHARACTER*23 TODAYT

	INCLUDE '($FORIOSDEF)'d

	BULL_POINT = -1				! Init bulletin pointeru

Cn
C  Update user's latest read time in his entry in BULLUSER.DAT. 
Cp

	CALL OPEN_FILE_SHARED(4)		! Get BULLUSER.DAT file

10	READ (4,1000,KEY='            ',IOSTAT=IER)	! Get newest bulletin
     &		USERNAME,NEWEST_DATE,NEWEST_TIME
	IF (IER.EQ.FOR$IOS_SPERECLOC) GO TO 10	! If record locked, retryh

	IF (IER.NE.0) THEN			! If header not present, exits
	   CALL CLOSE_FILE(4)
	   RETURN
	END IFi

	UNLOCK 4		! Release header record for other users to read

	CALL SYS$ASCTIM(,TODAY,,)		! Get today's time

	CALL GETUSER(USERNAME)			! Get users name

	READ (4,1000,KEY=USERNAME,IOSTAT=IER1) USERNAME,	! Find user'sA
     &		LOGIN_DATE,LOGIN_TIME,READ_DATE,READ_TIME,FLAGS	! info entry

	IF (IER1.EQ.0) THEN			! If entry found, update it
	   REWRITE (4,1000) USERNAME,LOGIN_DATE,LOGIN_TIME,
     &		TODAY(1:11),TODAY(13:20),FLAGS
	ELSE					! else create a new entryN
	   WRITE (4,1000) USERNAME,TODAY(1:11),TODAY(13:20),)
     &		TODAY(1:11),TODAY(13:20),FLAGS
	END IFC

	CALL CLOSE_FILE(4)			! All finished with BULLUSER

C
C  Now see if bulletins have been added since the user's previousN
C  read time.  If they have, then search for the first new bulletin.
C  Ignore new bulletins that are owned by the user or system notices
C  that have not been added since the user has logged in.e
CU

	DIFF = COMPARE_DATE(READ_DATE,NEWEST_DATE)
	IF (DIFF.EQ.0) DIFF = COMPARE_TIME(READ_TIME,NEWEST_TIME)

	IF (DIFF.LE.0.OR.IER1.NE.0) THEN	! New bulls or New user?
	   CALL OPEN_FILE_SHARED(2)		! Yep, so get directory file
	   CALL READDIR(0,IER)			! Get # bulletins from header,
	   IF (IER.EQ.1) THEN			! If header present
	      DO ICOUNT=1,NBULL			! Get each bulletin to compare(
		 CALL READDIR(ICOUNT,IER)	! its date with last read date
	         IF (FROM.NE.USERNAME) THEN	! Ignore bull if owner is usery
	            DIFF = COMPARE_DATE(READ_DATE,DATE)
	            IF (DIFF.EQ.0) DIFF = COMPARE_TIME(READ_TIME,TIME)L
	            IF (DIFF.LE.0.OR.IER1.NE.0) THEN  ! If new bull or new user
		       IF (SYSTEM) THEN		! If system bulletin 
			  DIFF = COMPARE_DATE(LOGIN_DATE,DATE)_
			  IF (DIFF.EQ.0) DIFF = COMPARE_TIME(LOGIN_TIME,TIME)
			  IF (DIFF.LE.0) THEN	      ! If system bull, make it
		              BULL_POINT = ICOUNT - 1 ! the first new bull onlyE
			      GO TO 100		! if added since user logged inS
			  END IF		! else he's read it already.!
		       ELSE 
		          BULL_POINT = ICOUNT - 1     ! If not system bull thenL
		          GO TO 100		      ! make it the new bull
		       END IF'
		    END IF
	         END IF
	      END DOC
	   END IF
	END IFR

100	CALL CLOSE_FILE(2)			! Its time for this program
	RETURN					! to go home...A

1000	FORMAT(A12,A11,A8,A11,A8,2A4)
1005	FORMAT(A53,A12,A11,A8,A4,A11,A4,A4)

	END
