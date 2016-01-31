From:	HENRY::IN%"MRL%PFC-VAX.MIT.EDU%xx.lcs.mit.edu%zermatt.lcs.mit.edu%csnet-relay.CSNET%relay.cs.net@RCA.COM" 13-DEC-1987 08:52
To:	MHG <MHG%mitre-bedford.arpa@zermatt>, 
Subj:	BULLETIN2.FOR

C
C  BULLETIN2.FOR, Version 12/11/87
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Programmer: Mark R. London
C
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

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	CHARACTER INEXDATE*11,INEXTIME*8
	CHARACTER INDESCRIP*80,INPUT*80,TODAY*23
	CHARACTER*1 ANSWER

	CHARACTER DATE_SAVE*11,TIME_SAVE*8

	INTEGER TIMADR(2)

	EXTERNAL CLI$_ABSENT

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
	   ELSE IF (FOLDER_SET.AND.CLI$PRESENT('SYSTEM')) THEN
	    WRITE (6,'(
     &       '' ERROR: Invalid parameter used with folder set.'')')
	    RETURN
	   END IF
	END IF

	IF (CLI$PRESENT('SHUTDOWN')) THEN
	   IF (.NOT.SETPRV_PRIV()) THEN
	    WRITE (6,'(
     &	     '' ERROR: Not enough privileges to change to shutdown.'')')
	    RETURN
	   ELSE IF (FOLDER_SET.AND.CLI$PRESENT('SHUTDOWN')) THEN
	    WRITE (6,'(
     &       '' ERROR: Invalid parameter used with folder set.'')')
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
	   INEXDATE = INPUT(1:11)
	   INEXTIME = INPUT(13:20)
	END IF

8	IF (CLI$PRESENT('HEADER').OR.DOALL) THEN
	   WRITE(6,1050)			! Request header for bulletin
	   READ(5,'(Q,A)',END=910,ERR=910) DESLEN,INDESCRIP
	   IF (DESLEN.EQ.0) GO TO 910	! If no header, don't add bull
	   IF (DESLEN.GT.53) THEN		! If too many characters
	      WRITE(6,1060)			! tell user
	      WRITE(6,2020) INDESCRIP(1:53)	! Show how much would fit
	      GO TO 8			! and re-request header
	   END IF
	ELSE IF (CLI$PRESENT('SUBJECT')) THEN
	   IER = CLI$GET_VALUE('SUBJECT',INDESCRIP,DESLEN)
	   IF (DESLEN.GT.53) THEN		! If too many characters
	      WRITE(6,1060)			! tell user
	      WRITE(6,2020) INDESCRIP(1:53)	! Show how much would fit
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
     &	    CLI$PRESENT('EDIT').OR.EDIT_DEFAULT) THEN	! or /EDIT specified

	   IF (CLI$PRESENT('EDIT').OR.EDIT_DEFAULT) THEN ! If /EDIT specified
	      IF (LEN_P.EQ.0) THEN		! If no file param specified
		 IF (.NOT.CLI$PRESENT('NEW')) THEN
	            OPEN (UNIT=3,FILE='SYS$LOGIN:BULL.SCR',STATUS='NEW',
     &		       ERR=920,FORM='FORMATTED',CARRIAGECONTROL='LIST')
	            CALL OPEN_FILE_SHARED(1)	! Prepare to copy message
		    LEN = 81
		    DO I=BLOCK,BLOCK+LENGTH-1	! Copy mesage into file
		       DO WHILE (LEN.GT.0)
			  CALL GET_BULL(I,INPUT,LEN)
			  IF (LEN.LT.0) THEN
			     GO TO 5
			  ELSE IF (LEN.GT.0) THEN
			     WRITE (3,'(A)') INPUT(1:LEN)
			  END IF
		       END DO
		       LEN = 80
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
	      OPEN (UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),STATUS='OLD',
     &		READONLY,SHARED,ERR=920,FORM='FORMATTED') ! Try opening the file
	   END IF

	   CALL ENABLE_PRIVS			! Reset SYSPRV privileges

	   DO WHILE(1)				! Read until end of file to
	      READ (3,'(Q,A)',END=10) LEN,INPUT	! get record count
	      IF (LEN.GT.80) GO TO 950
	      CALL STR$TRIM(INPUT,INPUT,LEN)
	      IF (LEN.GT.0) THEN		! If good input line entered
		 ICOUNT = ICOUNT + LEN + 1	! Increment record count
		 LAST_NOBLANK = ICOUNT
	      ELSE IF (LEN.EQ.0) THEN
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
	   LEN = 80				! Length of input line
	   DO WHILE (LEN.GE.0)			! Input until no more input
	      CALL GET_LINE(INPUT,LEN)		! Get input line
	      IF (LEN.GT.80) THEN		! Line too long.
		 WRITE(6,'('' ERROR: Input line length > 80. Reinput::'')')
	      ELSE IF (LEN.GT.0) THEN		! If good input line entered
		 ICOUNT = ICOUNT + 1 + LEN	! Increment character count
		 WRITE(3,'(A)') INPUT(1:LEN)	! Save line in scratch file
		 LAST_NOBLANK = ICOUNT
	      ELSE IF (LEN.EQ.0.AND.ICOUNT.GT.0) THEN
		 WRITE(3,'(A)') INPUT(1:LEN)	! Save line in scratch file
		 ICOUNT = ICOUNT + 2		! COPY_BULL writes a line with
	      END IF				! 1 space for a blank line.
	   END DO
	   IF (LEN.EQ.-1) GO TO 910		! CTRL_C entered, error out.
10	   ICOUNT = LAST_NOBLANK8
	   IF (ICOUNT.EQ.0) GO TO 910		! No lines entered, error outN
	  ENDIF

	  REWIND (UNIT=3)
	END IF2

C7
C  Add bulletin to bulletin file and directory entry for to directory file. 
Cv

	DATE_SAVE = DATE-
	TIME_SAVE = TIMEo
	INPUT = DESCRIP

	CALL OPEN_FILE(2)			! Prepare to add dir entry 

	CALL READDIR(NUMBER_PARAM,IER)		! Get info for messagel

	IF (IER.NE.NUMBER_PARAM+1.OR.DATE.NE.DATE_SAVE.OR. 
     &	    TIME.NE.TIME_SAVE.OR.INPUT.NE.DESCRIP) THEN
				! If message disappeared, try to find it.E
	   IF (IER.NE.NUMBER_PARAM+1) DATE = ' ''
	   NUMBER_PARAM = 0
	   IER = 1C
	   DO WHILE (IER.EQ.NUMBER_PARAM+1.AND.
     &	    (DATE.NE.DATE_SAVE.OR.TIME.NE.TIME_SAVE.OR.DESCRIP.NE.INPUT))
	      NUMBER_PARAM = NUMBER_PARAM + 1
	      CALL READDIR(NUMBER_PARAM,IER)T
	   END DO

	   IF (IER.NE.NUMBER_PARAM+1) THEN	! Couldn't find message 
	      CALL CLOSE_FILE(2).
	      CLOSE (UNIT=3,STATUS='SAVE')e
	      WRITE(6,'('' ERROR: Message has been deleted'',
     &			'' by another user.'')')W
	      IF (DOALL.OR.CLI$PRESENT('TEXT')) THEN 
		 WRITE (6,'('' New text has been saved in'',
     &				'' SYS$LOGIN:BULL.SCR.'')')n
	      END IF
	      GO TO 100
	   END IF
	END IFR

	CALL READDIR(0,IER)			! Get directory headerN

	IF (CLI$PRESENT('TEXT').OR.DOALL) THEN	! If text has been replacedM

	   LENGTH_SAVE = LENGTH			! Copy BULL modifies LENGTH

	   CALL OPEN_FILE(1)			! Prepare to add bulletina
	   ICOUNT = (ICOUNT+127)/128R

	   BLOCK = NBLOCK + 1
	   BLOCK_SAVE = BLOCK
	   NEMPTY = NEMPTY + LENGTH
	   NBLOCK = NBLOCK + ICOUNT

	   CALL WRITEDIR(0,IER)

	   CALL COPY_BULL(3,REC1,BLOCK,IER)	! Replace old bulletinE

	   CALL CLOSE_FILE(1)

	   CALL READDIR(NUMBER_PARAM,IER)	! Get directory entry
	   LENGTH = ICOUNT			! Update sizea
	   BLOCK = BLOCK_SAVE
	   CALL WRITEDIR(NUMBER_PARAM,IER)	! Write new directory entry)
	ELSE
	   CALL READDIR(NUMBER_PARAM,IER)
	END IFl

	IF (CLI$PRESENT('HEADER').OR.CLI$PRESENT('SUBJECT').OR.DOALL) THEND
	   DESCRIP=INDESCRIP(1:53)		! Update description header
	END IF_

	IF (CLI$PRESENT('EXPIRATION').OR.DOALL) THEN
	   SYSTEM = IBCLR(SYSTEM,1)
	   SYSTEM = IBCLR(SYSTEM,2)
	   EXDATE=INEXDATE			! Update expiration date
	   EXTIME=INEXTIMEd
	   DIFF = COMPARE_DATE(EXDATE,NEWEST_EXDATE)	! Compare expiration
	   IF (DIFF.EQ.0) DIFF = COMPARE_TIME(EXTIME,NEWEST_EXTIME)
	   IF (DIFF.LT.0) THEN			! If it's oldest expiration bull
	      NEWEST_EXDATE = EXDATE		! Update the header in!
	      NEWEST_EXTIME = EXTIME		! the directory filee
	      CALL WRITEDIR(0,IER)!
	   END IF
	ELSE IF (CLI$PRESENT('PERMANENT').AND.E
     &			(.NOT.BTEST(SYSTEM,1))) THEN
	   IF (BTEST(SYSTEM,2)) THENN
	      SYSTEM = IBCLR(SYSTEM,2)l
	      SHUTDOWN = SHUTDOWN - 1
	      CALL WRITEDIR(0,IER)R
	   END IF
	   SYSTEM = IBSET(SYSTEM,1)
	   EXDATE = '5-NOV-2000'I
	   EXTIME = '00:00:00'n
	ELSE IF (CLI$PRESENT('SHUTDOWN').AND.
     &			(.NOT.BTEST(SYSTEM,2))) THEN 
	   SYSTEM = IBSET(SYSTEM,2)
	   SYSTEM = IBCLR(SYSTEM,1)
	   EXDATE = '5-NOV-2000'L
	   EXTIME = '00:00:00'R
	   SHUTDOWN = SHUTDOWN + 1e
	   CALL SYS$ASCTIM(,TODAY,,)		! Get the present timex
	   SHUTDOWN_DATE = TODAY(1:11) 
	   SHUTDOWN_TIME = TODAY(13:20)
	   CALL WRITEDIR(0,IER)
	END IF=

	IF (CLI$PRESENT('SYSTEM')) THEN
	   SYSTEM = IBSET(SYSTEM,0)
	ELSE IF (CLI$PRESENT('GENERAL')) THEN
	   SYSTEM = IBCLR(SYSTEM,0)
	END IF

	CALL WRITEDIR(NUMBER_PARAM,IER)

	CALL CLOSE_FILE(2)		! Totally finished with replace

	CLOSE (UNIT=3)L

100	CALL ENABLE_CTRL		! Enable CTRL-Y & -C
	RETURNS

910	WRITE(6,1010) 
	CLOSE (UNIT=3,ERR=100)P
	GOTO 100)

920	WRITE(6,1020)T
	CALL ENABLE_PRIVS	! Reset SYSPRV privileges
	GOTO 100 

950	WRITE (6,1030)
	CLOSE (UNIT=3).
	GO TO 100

1000	FORMAT (' Enter message: End with ctrl-z, cancel with ctrl-c')N
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
     &	       ' Are you sure you want to replace it? ',$)r
2020	FORMAT(1X,A)E

	END



	SUBROUTINE SEARCH(READ_COUNT)
CS
C  SUBROUTINE SEARCH
Ch
C  FUNCTION: Search for bulletin with specified string
CE
	IMPLICIT INTEGER (A - Z)T

	INCLUDE 'BULLDIR.INC'

	COMMON/INPUT_BULL/INPUT
	CHARACTER*80 INPUT 

	CHARACTER*132 SEARCH_STRING,SAVE_STRING
	DATA SEARCH_STRING /' '/, SEARCH_LEN /1/O

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /CTRLC_FLAG/ FLAGC

	CALL DISABLE_CTRL

	IF (CLI$PRESENT('START')) THEN		! Starting message specifiedm
	   CALL CLI$GET_VALUE('START',BULL_PARAMETER,LEN_P)
	   DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER) BULL_POINT
	   BULL_POINT = BULL_POINT - 1s
	END IF

	SAVE_STRING = SEARCH_STRING
	SAVE_LEN = SEARCH_LEN

	IER = CLI$GET_VALUE('SEARCH_STRING',SEARCH_STRING,SEARCH_LEN)
	I
	IF (.NOT.IER) THEN			! If no search string entered 
	   SEARCH_STRING = SAVE_STRING		! use saved search string
	   SEARCH_LEN = SAVE_LEN1
	ELSE IF (.NOT.CLI$PRESENT('START')) THEN ! If string entered but no
	   BULL_POINT = 0			 ! starting message, use firstL
	END IF

	CALL STR$UPCASE(SEARCH_STRING,SEARCH_STRING)	! Make upper case 

	CALL OPEN_FILE_SHARED(2)T

	CALL READDIR(0,IER)

	IF (BULL_POINT+1.GT.NBULL) THEN
	   WRITE (6,'('' ERROR: No more messages.'')') 
	   CALL CLOSE_FILE(2)
	   CALL ENABLE_CTRL
	   RETURN
	END IFe

	CALL OPEN_FILE_SHARED(1)

	CALL DECLARE_CTRLC_ASTI

	DO BULL_SEARCH = BULL_POINT+1, NBULLO
	   CALL READDIR(BULL_SEARCH,IER)	! Get bulletin directory entry
	   IF (IER.EQ.BULL_SEARCH+1) THEN
	      LEN = 81 
	      DO J=BLOCK,BLOCK+LENGTH-1
	         DO WHILE (LEN.GT.0) 
	            CALL GET_BULL(J,INPUT,LEN)'
	            CALL STR$UPCASE(INPUT,INPUT)	! Make upper case
		    IF (INDEX(INPUT,SEARCH_STRING(1:SEARCH_LEN)).GT.0) THENC
		       CALL CLOSE_FILE(1)E
		       CALL CLOSE_FILE(2)(
		       CALL CANCEL_CTRLC_AST
		       CALL ENABLE_CTRLM
		       BULL_POINT = BULL_SEARCH - 1 
	               CALL READ(READ_COUNT,BULL_POINT+1) ! Read next bulletinW
		       RETURN 
		    ELSE IF (FLAG.EQ.1) THEN
		       WRITE (6,'('' Search aborted.'')')o
		       CALL CLOSE_FILE(1)O
		       CALL CLOSE_FILE(2)M
		       CALL ENABLE_CTRLI
		       RETURN	
		    END IF
	         END DO
		 LEN = 80C
	      END DO!
	   END IF
	END DOn

	CALL CANCEL_CTRLC_AST

	CALL CLOSE_FILE(1)			! End of bulletin file read0
	CALL CLOSE_FILE(2)=

	CALL ENABLE_CTRLU

	WRITE (6,'('' No messages found with given search string.'')') 

	RETURNC
	END




	SUBROUTINE UNDELETE
C 
C  SUBROUTINE UNDELETE
C 
C  FUNCTION: Undeletes deleted message.,
CE
	IMPLICIT INTEGER (A - Z) 

	COMMON /POINT/ BULL_POINT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'i

	INCLUDE 'BULLFOLDER.INC' 

	EXTERNAL CLI$_ABSENTi

C 
C  Get the bulletin number to be undeleted. 
Cu

	IER = CLI$GET_VALUE('BULLETIN_NUMBER',BULL_PARAMETER,LEN_P)
	IF (IER.NE.%LOC(CLI$_ABSENT)) THEN	! Was bulletin specified?p
	   DECODE(LEN_P,5,BULL_PARAMETER,ERR=920) BULL_DELETE	! Yes
5	   FORMAT(I<LEN_P>) 
	ELSE IF (BULL_POINT.EQ.0) THEN	! No.  Have we just read a bulletin?
	   GO TO 910			! No, then error.S
	ELSEe
	   BULL_DELETE = BULL_POINT	! Delete the file we are reading.
	END IFI

	IF (BULL_DELETE.LE.0) GO TO 920

C(
C  Check to see if specified bulletin is present, and if the userB
C  is permitted to delete the bulletin.!
Cs

	CALL OPEN_FILE(2)

	CALL READDIR(BULL_DELETE,IER)	! Get info for specified bulletin

	IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?G
	   WRITE(6,1030)	! If not, then error out
	   GOTO 100
	END IFI

	IF (USERNAME.NE.FROM) THEN	! If doesn't match owner of bulletin,e
	   IF ((.NOT.SETPRV_PRIV().AND..NOT.FOLDER_SET).OR.    ! Privileges oro
     &	       (.NOT.SETPRV_PRIV().AND.USERNAME.NE.FOLDER_OWNER
     &		.AND.FOLDER_SET)) THEN	! folder owner?
	      WRITE(6,1040)		! Then error out.P
	      GO TO 100
	   ELSE
	      CALL READDIR(BULL_DELETE,IER) ! Get info for specified bulletin
	      IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found?A
	         WRITE(6,1030)		! If not, then error out 
	         GOTO 100
	      END IFA
	   END IF
	END IF(

	IF (SYSTEM.LE.1) THEN		! General or System messageP
	   EXDATE = EXDATE(1:7)//'19'//EXDATE(10:)
	ELSE				! Permanent or Shutdown
	   IF (EXDATE(2:2).EQ.'-') THEN
	      EXDATE = EXDATE(1:6)//'20'//EXDATE(9:)
	   ELSE
	      EXDATE = EXDATE(1:7)//'20'//EXDATE(10:)
	   END IF
	END IF6

	CALL WRITEDIR(BULL_DELETE,IER)	! Update message expiration date

	WRITE (6,'('' Message was undeleted.'')')

100	CALL CLOSE_FILE(2)

900	RETURN

910	WRITE(6,1010) 
	GO TO 900

920	WRITE(6,1020)'
	GO TO 900

1010	FORMAT(' ERROR: You are not reading any message.')D
1020	FORMAT(' ERROR: Specified message number has incorrect format.')L
1030	FORMAT(' ERROR: Specified message was not found.') 
1040	FORMAT(' ERROR: Message was not undeleted. Not owned by you.')p

	END



	SUBROUTINE UPDATE
CC
C  SUBROUTINE UPDATE
CC
C  FUNCTION:  Searches for bulletins that have expired and deletes them.
CO
C  NOTE:  Assumes directory file is already opened. 
CA
	IMPLICIT INTEGER (A - Z)E

	INCLUDE 'BULLDIR.INC'

	CHARACTER*107 DIRLINE

	CHARACTER*11 TEMP_DATE,TEMP_EXDATE!
	CHARACTER*8 TEMP_TIME,TEMP_EXTIME

	IF (TEST_BULLCP()) RETURN	! BULLCP cleans up expired bulletinsU

	ENTRY UPDATE_ALWAYS		! Entry to skip BULLCP test

	TEMP_EXDATE = '5-NOV-2000'  ! If a bulletin gets deleted, and there are
	TEMP_EXTIME = '00:00:00'    ! are no more bulletins, this is the valueU
				    ! assigned to the latest expiration date

	TEMP_DATE = '5-NOV-1956' 	! Storage for computing newest,
	TEMP_TIME = '00:00:00'		! bulletin date if deletion occursa

	BULL_ENTRY = 1				! Init bulletin pointer
	UPDATE_DONE = 0			! Flag showing bull has been deleteda

	DO WHILE (1)F
	   CALL READDIR(BULL_ENTRY,IER)		! Get next directory entry
	   IF (IER.EQ.BULL_ENTRY) GO TO 100	! ERROR: Not foundW
	   IF (SYSTEM.LE.3.OR.(SHUTDOWN.EQ.0	! If not shutdown, or time
     &	     .AND.(SYSTEM.AND.4).EQ.4)) THEN	! to delete shutdowns?
	    IF ((SYSTEM.AND.4).EQ.4) THEN	! Shutdown bulletin?	
	       DIFF = 0				! If so, delete it
	    ELSEM
	       DIFF = COMPARE_DATE(EXDATE,' ')	! Has expiration date passed?N
	       IF (DIFF.EQ.0) DIFF = COMPARE_TIME(EXTIME,' ')
	    END IFS
	    IF (DIFF.LE.0) THEN			! If so then delete bulletin0
	      CALL DELETE_ENTRY(BULL_ENTRY)	! Delete bulletin entry
	      IF (UPDATE_DONE.EQ.0) THEN	! If this is first deleted fileC
	         UPDATE_DONE = BULL_ENTRY	! store it to use for reordering 
	      END IF				! directory file.
	    ELSE IF (SYSTEM.LE.3) THEN		! Expiration date hasn't passed
		! If a bulletin is deleted, we'll have to update the latestE
		! expiration date. The following does that.M
	      DIFF = COMPARE_DATE(EXDATE,TEMP_EXDATE)
	      IF (DIFF.LT.0.OR.(DIFF.EQ.0.AND.M
     &		COMPARE_TIME(EXTIME,TEMP_EXTIME).LT.0)) THEN
	         TEMP_EXDATE = EXDATE		! If this is the latest expp
	         TEMP_EXTIME = EXTIME		! date seen so far, save it.
	      END IFR
	      TEMP_DATE = DATE			! Keep date so when we quitT
	      TEMP_TIME = TIME			! search, we'll have the
	    END IF				! latest bulletin date5
	   ELSE
	      TEMP_DATE = DATE
	      TEMP_TIME = TIME 
	   END IF
	   BULL_ENTRY = BULL_ENTRY + 1h
	END DON

100	IF (UPDATE_DONE.GT.0) THEN		! Reorder directory file
	   CALL CLEANUP_DIRFILE(UPDATE_DONE)	! due to deleted entries
	END IFe

	DATE = NEWEST_DATE.
	TIME = NEWEST_TIMER
	NEW_SHUTDOWN = SHUTDOWN
	CALL READDIR(0,IER)
	SHUTDOWN = NEW_SHUTDOWN
	NEWEST_EXDATE = TEMP_EXDATE
	NEWEST_EXTIME = TEMP_EXTIME
	NEWEST_DATE = TEMP_DATE
	NEWEST_TIME = TEMP_TIME
	CALL WRITEDIR(0,IER)R
	CALL UPDATE_FOLDERe
C 
C  If newest message date has been changed, must change it in BULLUSER.DAT
C  and also see if it affects notification of new messages to userso
C,
	IF (TEMP_DATE.NE.DATE.OR.TEMP_TIME.NE.TIME) THENi
	   CALL UPDATE_LOGIN(.FALSE.)
	END IF

	RETURNN

	END



	SUBROUTINE UPDATE_READ 
CR
C  SUBROUTINE UPDATE_READ 
C 
C  FUNCTION:
C	Store the latest date that user has used the BULLETIN facility.I
C	If new bulletins have been added, alert user of the fact.C
CA

	IMPLICIT INTEGER (A - Z)T

	INCLUDE 'BULLUSER.INC''

	INCLUDE 'BULLDIR.INC'

	INCLUDE '($PRVDEF)'

	CHARACTER TODAY*23U

	DIMENSION TODAY_BTIM(2)

C 
C  Update user's latest read time in his entry in BULLUSER.DAT.L
C

	CALL OPEN_FILE_SHARED(4)		! Get BULLUSER.DAT file

	DO WHILE (REC_LOCK(IER))L
	   READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER)
     &		TEMP_USER,NEWEST_BTIM,		! Get newest bulletinB
     &		BBOARD_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	END DOE

	IF (IER.NE.0) THEN			! If header not present, exit_
	   CALL CLOSE_FILE(4)
	   RETURN
	ELSE IF (NEW_FLAG(1).EQ.-1.AND.NEW_FLAG(2).EQ.-1) THENN
						! If header present, but no 
	   DO I=1,FLONG				! SET_FLAG and NOTIFY_FLAG
	      SET_FLAG(I) = 0			! information, write defaulto
	      NOTIFY_FLAG(I) = 0		! flags.s
	      BRIEF_FLAG(I) = 0
	      NEW_FLAG(I) = 0
	   END DO
	   SET_FLAG(1) = 1u
	   NEW_FLAG(1) = PRV$M_OPER.OR.PRV$M_CMKRNL.OR.PRV$M_SETPRV
	   REWRITE (4,FMT=USER_FMT)
     &		TEMP_USER,NEWEST_BTIM,BBOARD_BTIM,
     &		NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	END IFL

	CALL SYS$ASCTIM(,TODAY,,)		! Get today's time
	CALL SYS_BINTIM(TODAY,TODAY_BTIM)

	UNLOCK 4 

	DO WHILE (REC_LOCK(IER1))
	   READ (4,FMT=USER_FMT,KEY=USERNAME,IOSTAT=IER1) USERNAME,
     &	    LOGIN_BTIM,READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,=
     &      NOTIFY_FLAG			! Find user's info
	END DO 

	IF (IER1.EQ.0) THEN			! If entry found, update it
	   REWRITE (4,FMT=USER_FMT) USERNAME,LOGIN_BTIM,TODAY_BTIM,
     &		NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	ELSE					! If no entry create a new entry
	   NEW_FLAG(1) = 143 
	   NEW_FLAG(2) = 0T
	   WRITE (4,FMT=USER_FMT) USERNAME,TODAY_BTIM,TODAY_BTIM,
     &	    NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAGO
	END IFR

	CALL CLOSE_FILE(4)			! All finished with BULLUSER

	RETURN					! to go home...(

	END




	SUBROUTINE FIND_NEWEST_BULL
C_
C  SUBROUTINE FIND_NEWEST_BULL
C2
C	If new bulletins have been added, alert user of the fact and
C	set the next bulletin to be read to the first new bulletin.D
Cn
C  OUTPUTS:C
C	BULL_POINT  -  If -1, no new bulletins to read, else there are.
CL

	IMPLICIT INTEGER (A - Z)B

	COMMON /POINT/ BULL_POINT

	INCLUDE 'BULLUSER.INC'e

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'N

	CHARACTER READ_DATE_TIME*20,LOGIN_DATE_TIME*20T

	CALL SYS$ASCTIM(,READ_DATE_TIME,LAST_READ_BTIM(1,FOLDER_NUMBER+1),)
	CALL SYS$ASCTIM(,LOGIN_DATE_TIME,LOGIN_BTIM,)
CR
C  Now see if bulletins have been added since the user's previous'
C  read time.  If they have, then search for the first new bulletin.
C  Ignore new bulletins that are owned by the user or system notices
C  that have not been added since the user has logged in.E
C)
	BULL_POINT = -1				! Init bulletin pointeru

	CALL OPEN_FILE_SHARED(2)		! Yep, so get directory file2
	CALL READDIR(0,IER)			! Get # bulletins from header
	IF (IER.EQ.1) THEN			! If header present 
	   DO ICOUNT=1,NBULL			! Get each bulletin to compare
	      CALL READDIR(ICOUNT,IER)		! its date with last read daten
	      IF (FROM.NE.USERNAME) THEN	! Ignore bull if owner is user
	         DIFF = COMPARE_DATE(READ_DATE_TIME(1:11),DATE)
	         IF (DIFF.EQ.0) h
     &			DIFF = COMPARE_TIME(READ_DATE_TIME(13:20),TIME)
	         IF (DIFF.LT.0) THEN		! If new bull or new user
		    IF (SYSTEM) THEN		! If system bulletin
		       DIFF = COMPARE_DATE(LOGIN_DATE_TIME(1:11),DATE)
		       IF (DIFF.EQ.0)N
     &			DIFF = COMPARE_TIME(LOGIN_DATE_TIME(13:20),TIME) 
		    END IF
		    IF (DIFF.LE.0) THEN_
		       BULL_POINT = ICOUNT - 1
		       CALL CLOSE_FILE(2).
		       RETURNO
		    END IF
		 END IFF
	      END IFN
	   END DO
	END IF 

	CALL CLOSE_FILE(2)n

	RETURNP
	END



	SUBROUTINE GET_EXPIRED(INPUT,IER)

	IMPLICIT INTEGER (A-Z)f

	INCLUDE 'BULLUSER.INC' 

	INCLUDE 'BULLFOLDER.INC')

	CHARACTER*20 INPUTu
	CHARACTER*23 TODAY(

	DIMENSION EXTIME(2),NOW(2)u

	EXTERNAL CLI$_ABSENT 

	IER = SYS$ASCTIM(,TODAY,,)		! Get today's dateE

	IERC = CLI$GET_VALUE('EXPIRATION',INPUT,ILEN)

	PROMPT = .TRUE.

5	IF (PROMPT) THEN
	   IF (IERC.NE.%LOC(CLI$_ABSENT)) THEN	! Was value specified?
	      PROMPT = .FALSE./
	   ELSE
	      WRITE(6,1030) TODAY		! Prompt for expiration date
	      CALL GET_LINE(INPUT,ILEN)		! Get input line
	   END IF
	ELSEa
	   RETURN
	END IF 

	IF (ILEN.LE.0) THEN
	   IER = 0d
	   RETURN
	END IFA

	INPUT = INPUT(1:ILEN)			! Change trailing zeros 2 spaces9

	IF (INDEX(INPUT,'-').EQ.0.AND.INDEX(INPUT,':').GT.0.AND.Y
     &		INDEX(INPUT(1:ILEN),' ').EQ.0) THENA
	   INPUT = TODAY(1:INDEX(TODAY(2:),' ')+1)//INPUT
	END IF

	CALL STR$UPCASE(INPUT,INPUT)		! Convert to upper case
	IER = SYS_BINTIM(INPUT,EXTIME)n
	IF (IER.NE.1) THEN			! If not able to do so
    	   WRITE(6,1040)			! tell user is wrong
	   GO TO 5N
	END IFe
	IER = SYS$ASCTIM(TIMLEN,INPUT,EXTIME,)d
	IF (TIMLEN.EQ.16) THENT
	   CALL SYS$GETTIM(NOW)
	   CALL LIB$SUBX(NOW,EXTIME,EXTIME)
	   IER = SYS$ASCTIM(TIMLEN,INPUT,EXTIME,)
	END IFR

	IF (INPUT(2:2).EQ.'-') INPUT = '0'//INPUT
	IER = COMPARE_DATE(INPUT(1:11),TODAY(1:11)) ! Compare date with today's
	IF (IER.GT.F_EXPIRE_LIMIT.AND.F_EXPIRE_LIMIT.GT.0.AND.A
     &		.NOT.SETPRV_PRIV().AND.USERNAME.NE.FOLDER_OWNER) THEN 
	   WRITE(6,1050) F_EXPIRE_LIMIT		! Expiration date > limit'
	   GO TO 5!
	END IFo
	IF (IER.EQ.0) IER = COMPARE_TIME(INPUT(13:20),TODAY(13:20))
	IF (IER.LE.0) THEN			! If expiration date not futureo
	   WRITE(6,1045)			! tell user_
	   GO TO 5				! and re-request date
	END IFo

	IER = 1

	RETURN 

1030	FORMAT(' It is ',A23,
     &'. Specify when the message should expire:',/,1x,
     &'Enter absolute time: [dd-mmm-yyyy] hh:mm:ss ',e
     &'or delta time: dddd hh:mm:ss')B
1040	FORMAT(' ERROR: Invalid date format specified.')M
1045	FORMAT(' ERROR: Specified time has already passed.') 
1050	FORMAT(' ERROR: Specified expiration period too large. 
     & Limit is ',I3,' days.')

	END


	SUBROUTINE MAILEDIT(INFILE,OUTFILE)

	IMPLICIT INTEGER (A-Z) 

	INCLUDE '($SSDEF)'T

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*132 INCMD

	EXTERNAL BULLETIN_SUBCOMMANDS

	CHARACTER*(*) INFILE,OUTFILEI

	CHARACTER*80 MAIL_EDIT,OUTe

	IER = SYS_TRNLNM('MAIL$EDIT',MAIL_EDIT)
	IF (IER.NE.SS$_NORMAL) MAIL_EDIT = 'SYS$SYSTEM:MAILEDIT'E

	OUT = OUTFILE
	IF (TRIM(OUT).EQ.0) THEN
	   OUT = INFILE
	END IFL

	IF (INDEX(MAIL_EDIT,'CALLABLE_').EQ.0) THEN
	   CALL LIB$SPAWN('$@'//MAIL_EDIT(:TRIM(MAIL_EDIT))
     &		//' '//INFILE//' '//OUT(:TRIM(OUT)))
	ELSE IF (INDEX(MAIL_EDIT,'EDT').GT.0) THENe
	   CALL EDT$EDIT(INFILE,OUT)t
	ELSE IF (INDEX(MAIL_EDIT,'TPU').GT.0) THENO
	   CALL TPU$EDIT(INFILE,OUT)
	   IER = CLI$DCL_PARSE(INCMD,BULLETIN_SUBCOMMANDS)P
		! TPU does CLI$ stuff which wipes our parsed command lineT
	END IFE

	RETURN 
	END





	SUBROUTINE CREATE_BULLCP 

	IMPLICIT INTEGER (A-Z) 

	INCLUDE '($PRCDEF)'

	INCLUDE '($JPIDEF)'

	INCLUDE '($SSDEF)' 

	INCLUDE 'BULLFILES.INC'

	COMMON /PRIVILEGES/ PROCPRIV(2),NEEDPRIV(2)

	CHARACTER IMAGENAME*132,ANSWER*1,PRCNAM*15_

	DIMENSION SAVEPRIV(2)

	CALL DISABLE_PRIVS	! Just let real privileged people do a /STARTUP

	CALL SYS$SETPRV(%VAL(1),PROCPRIV,,SAVEPRIV)	! Enable original privN

	IF (TEST_BULLCP()) THEN
	   WRITE (6,'('' BULLCP process running.E
     & Do you wish to kill it and restart a new one? '',$)')
	   READ (5,'(A)') ANSWERW
	   IF (ANSWER.NE.'Y'.AND.ANSWER.NE.'y') CALL EXIT

	   WILDCARD = -1I

	   CALL INIT_ITMLST	! Initialize item listM
				! Now add items to listR
	   CALL ADD_2_ITMLST(LEN(PRCNAM),JPI$_PRCNAM,%LOC(PRCNAM))n
	   CALL ADD_2_ITMLST(4,JPI$_PID,%LOC(PID))a
	   CALL END_ITMLST(GETJPI_ITMLST)	! Get address of itemlist
	   IER = 1T
	   DO WHILE (IER.AND.PRCNAM(:6).NE.'BULLCP') 
						! Get next interactive process
	      IER = SYS$GETJPIW(,WILDCARD,,%VAL(GETJPI_ITMLST),,,,)
						! Get next process.
	   END DO
	   IF (IER.AND.PID.NE.0) IER = SYS$DELPRC(PID,)
	   IF (.NOT.IER) THEN
	      CALL SYS_GETMSG(IER)d
	      CALL EXIT
	   END IF
	END IFI

	CALL GETIMAGE(IMAGENAME,ILEN)

	LEN_B = TRIM(FOLDER_DIRECTORY).

	OPEN(UNIT=11,FILE=FOLDER_DIRECTORY(:LEN_B)//'BULLCP.COM',
     &		STATUS='OLD',IOSTAT=IER)
	IF (IER.EQ.0) CLOSE(UNIT=11,STATUS='DELETE')S

	CALL SYS$SETDFPROT('AA00'X,CUR_DEF_PROT)G
		! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD:RW,GROUP:RW)
	OPEN(UNIT=11,FILE=FOLDER_DIRECTORY(:LEN_B)//'BULLCP.COM',
     &		STATUS='NEW',IOSTAT=IER,CARRIAGECONTROL='LIST')E
	IF (IER.NE.0) RETURNL
	WRITE(11,'(A)') '$B:=$'//IMAGENAME(:ILEN)
	WRITE(11,'(A)') '$LOOP:'n
	WRITE(11,'(A)') '$B/BULLCP'
	WRITE(11,'(A)') '$GOTO LOOP'		! File open timed out
	CLOSE(UNIT=11)N
	CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection_

	IER = 0
	DO WHILE (IER.EQ.0.OR.(IER.EQ.SS$_DUPLNAM.AND.PID.GT.0))a
	   IER = SYS$CREPRC(,'SYS$SYSTEM:LOGINOUT',
     &	      FOLDER_DIRECTORY(:LEN_B)//'BULLCP.COM','NL:'
     &	      ,,,,'BULLCP',%VAL(4),,,%VAL(PRC$M_NOUAF+PRC$M_DETACH)).
	END DOP

	CALL SYS$SETPRV(%VAL(0),SAVEPRIV,,)	! Reset privs

	CALL ENABLE_PRIVS

	IF (.NOT.IER) THENF
	   CALL SYS_GETMSG(IER)
	ELSEI
	   WRITE (6,'('' Successfully created BULLCP detached process.'')')
	END IFD
	CALL EXIT

	END



	SUBROUTINE FIND_BULLCP)

	IMPLICIT INTEGER (A-Z)K

	COMMON /BCP/ BULLCP
	LOGICAL BULLCP /.FALSE./N

	CHARACTER*1 DUMMY

	IER = SYS_TRNLNM('BULL_BULLCP',DUMMY)
	IF (IER) BULLCP = .TRUE.o

	RETURN
	END




	LOGICAL FUNCTION TEST_BULLCPd

	IMPLICIT INTEGER (A-Z)U

	COMMON /BCP/ BULLCP
	LOGICAL BULLCP

	TEST_BULLCP = BULLCP,

	RETURNN
	END




	SUBROUTINE RUN_BULLCP

	IMPLICIT INTEGER (A-Z)L

	INCLUDE 'BULLFOLDER.INC' 

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'T

	COMMON /BCP/ BULLCP
	LOGICAL BULLCPB

	IF (TEST_BULLCP()) CALL EXIT	! BULLCP already running, so exit.

	BULLCP = .FALSE.		! Enable process to do BULLCP functions

	IER = SYS$CREMBX(%VAL(1),CHAN,,,,,'BULL_BULLCP') 
	IF (.NOT.IER) THEN		! Can't create mailbox, so exit.d
	   CALL SYS_GETMSG(IER)
	   CALL EXITe
	END IFi

	IER = SYS$DELMBX(%VAL(CHAN))	! If process dies, mailbox is deleted.

	DO WHILE (1)			! Loop once every 15 minutes
	   CALL BBOARD			! Look for BBOARD messages.M
	   DO FOLDER_NUMBER=0,FOLDER_MAX'
	      CALL SELECT_FOLDER(.FALSE.,IER)	! Select folder
	      IF (IER) THEN
	         IF (NEMPTY.GT.200) THEN_
	            CALL CLEANUP_BULLFILE	! Cleanup empty blocks(
	         END IF
	         CALL DELETE_EXPIRED		! Delete expired messages
	      END IFl
	   END DO
	   CALL WAIT('15')		! Wait for 15 minutes
	END DOI

	RETURN,
	END



	SUBROUTINE WAIT(MIN)e
C.
C SUBROUTINE WAITl
Cn
C FUNCTION: Waits for 15 minutes.t
Cn
	IMPLICIT INTEGER (A-Z)b
	PARAMETER WAITEFN=1			! Event flag to wait on.L
	INTEGER TIMADR(2)			! Buffer containing timeL
						! in desired system format.t
	CHARACTER TIMBUF*13,MIN*2
	DATA TIMBUF/'0 00:00:00.00'/f

	TIMBUF(6:7) = MIN

	IER=SYS$BINTIM(TIMBUF,TIMADR)
	IER=SYS$SETIMR(%VAL(WAITEFN),TIMADR,,)! Set timer.e
	IER=SYS$WAITFR(%VAL(WAITEFN))		! Wait for EFN to be set.d

	RETURN 
	END




	SUBROUTINE DELETE_EXPIRED

Co
C  SUBROUTINE DELETE_EXPIRED
CM
C  FUNCTION:
CT
C  Delete any expired bulletins (normal or shutdown ones).
C  (NOTE: If bulletin files don't exist, they get created now by
C  OPEN_FILE_SHARED.  Also, if new format has been defined for files,e
C  they get converted now.  The directory file has had it's record size 
C  lengthened in the past to include more info, and the bulletin file 
C  was lengthened from 80 to 81 characters to include byte which indicated
C  start of bulletin message.  However, that scheme was removed andF
C  was replaced with a 128 byte record compressed format).
CS

	IMPLICIT INTEGER (A-Z)D

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC' 

	CALL OPEN_FILE_SHARED(2)	! Open directory fileU
	CALL OPEN_FILE_SHARED(1)	! Open bulletin file
	CALL CLOSE_FILE(1)
	CALL READDIR(0,IER)		! Get directory header
	IF (IER.EQ.1) THEN		! Is header present?G
	   IER = COMPARE_DATE(NEWEST_EXDATE,' ') ! Yes. Any expired bulls?
	   IF (IER.EQ.0) IER = COMPARE_TIME(NEWEST_EXTIME,' ')L
	   IF (SHUTDOWN.GT.0.AND.FOLDER_NUMBER.EQ.0) THEN
						! Do shutdown bulletins exist?
	      CALL GET_UPTIME(UPTIME_DATE,UPTIME_TIME)
	      IER1 = COMPARE_DATE(SHUTDOWN_DATE,UPTIME_DATE) 
	      IF (IER1.EQ.0) IER1 = COMPARE_TIME(SHUTDOWN_TIME,UPTIME_TIME)
	      IF (IER1.LE.0) SHUTDOWN = 0
	   ELSE
	      IER1 = 1 
	   END IF
	   IF (IER.LE.0.OR.IER1.LE.0) THEN.
	      CALL CLOSE_FILE(2)T
	      CALL OPEN_FILE(2)		! Reopen without sharing
	      CALL UPDATE 		! Need to update,
	   END IF
	ELSE		! If header not there, then first time running BULLETIN
	   CALL OPEN_FILE(4)		! Create user file to be able to set	
	   CALL CLOSE_FILE(4)		! defaults, privileges, etc.
	END IFs
	CALL CLOSE_FILE(2)

	RETURNI
	END

