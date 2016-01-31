C
C  BULLETIN6.FOR, Version 1/27/87
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Programmer: Mark R. London
C
	SUBROUTINE UPDATE_LOGIN(ADD_BULL)
C
C  SUBROUTINE UPDATE_LOGIN
C
C  FUNCTION:  Updates the login file when a bulletin has been deleted
C	or added.
C
	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE '($BRKDEF)'

	DIMENSION READ_BTIM_SAVE(2),TEMP_BTIM(2)

	CHARACTER*160 OUTPUT
	CHARACTER*1 CR/13/,LF/10/,BELL/7/

	DIMENSION SAVE_NEW_FLAG(2)

C
C  We want to keep the last read date for comparison when selecting new
C  folders, so save it for later restoring.
C

	READ_BTIM_SAVE(1) = READ_BTIM(1)
	READ_BTIM_SAVE(2) = READ_BTIM(2)

	CALL OPEN_FILE_SHARED(4)

C
C  Newest date/time in user file only applies to general bulletins.
C  This was present before adding folder capability.
C  We set flags in user entry to show new folder added for folder bulletins.
C  However, the newest bulletin for each folder is not continually updated,
C  As it is only used when comparing to the last bulletin read time, and to
C  store this for each folder would be too expensive.
C

	DO WHILE (REC_LOCK(IER))
	   READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER)
     &		TEMP_USER,TEMP_BTIM,BBOARD_BTIM,NEW_FLAG,
     &	        SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	END DO

	IF (IER.NE.0) THEN
	   CALL CLOSE_FILE(4)
	   RETURN
	ELSE IF (FOLDER_NUMBER.EQ.0) THEN
	   CALL SYS$BINTIM(NEWEST_DATE//' '//NEWEST_TIME,NEWEST_BTIM)
	   REWRITE (4,FMT=USER_FMT) TEMP_USER,NEWEST_BTIM,BBOARD_BTIM,
     & 		NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	END IF

C
C  Set flags in all user entries that have SET READNEW on the particular
C  folder to indicate that a new bulletin is present for the particular folder.
C  Also send broadcast if notify flag set.
C
	OUTPUT = BELL//CR//LF//LF//'New bulletin added to folder '//
     &	 FOLDER(1:TRIM(FOLDER))//'. From: '//FROM(1:TRIM(FROM))//
     &   CR//LF//'Description: '//DESCRIP(1:TRIM(DESCRIP))

	IF (.NOT.ADD_BULL) THEN
	   SAVE_NEW_FLAG(1) = NEW_FLAG(1)
	   SAVE_NEW_FLAG(2) = NEW_FLAG(2)
	END IF

	F_POINT = FOLDER_NUMBER/32 + 1
	IER = 0
	DO WHILE (IER.EQ.0)
	   DO WHILE (REC_LOCK(IER))
	    READ (4,FMT=USER_FMT,IOSTAT=IER) TEMP_USER,LOGIN_BTIM,
     &		READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	   END DO
	   SAVE_FLAG = NEW_FLAG(F_POINT)
 	   IF ((IER.EQ.0).AND.(TEMP_USER.NE.FROM.OR..NOT.ADD_BULL)) THEN
	      IF (ADD_BULL) THEN
	       CALL SET2(NEW_FLAG,FOLDER_NUMBER)
	       IF (TEST2(NOTIFY_FLAG,FOLDER_NUMBER)) THEN
	         CALL SYS$BRKTHRU(,OUTPUT(1:TRIM(OUTPUT))//CR,TEMP_USER,
     &		   %VAL(BRK$C_USERNAME),,,,,,,)
	       END IF
	      ELSE
	       DIFF = COMPARE_BTIM(NEWEST_BTIM,READ_BTIM)
	       IF (DIFF.LT.0) THEN
		  CALL CLR2(NEW_FLAG,FOLDER_NUMBER)
		  IF (TEMP_USER.EQ.USERNAME) THEN
		     SAVE_NEW_FLAG(F_POINT) = NEW_FLAG(F_POINT)
		  END IF
	       END IF
	      END IF
	      IF (SAVE_FLAG.NE.NEW_FLAG(F_POINT)) THEN
	         REWRITE (4,FMT=USER_FMT) TEMP_USER,LOGIN_BTIM,
     &	          READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	      END IF
	   END IF
	END DO

	NEW_FLAG(1) = SAVE_NEW_FLAG(1)
	NEW_FLAG(2) = SAVE_NEW_FLAG(2)

	DO WHILE (REC_LOCK(IER))
	   READ (4,FMT=USER_FMT,KEY=USERNAME,IOSTAT=IER) USERNAME,
     &	     LOGIN_BTIM,READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,
     &	     NOTIFY_FLAG
		! Reobtain present values as calling programs still uses them
	END DO

	READ_BTIM(1) = READ_BTIM_SAVE(1)
	READ_BTIM(2) = READ_BTIM_SAVE(2)

	CALL CLOSE_FILE(4)

	RETURN

	END




 
	SUBROUTINE ADD_ENTRY
C
C  SUBROUTINE ADD_ENTRY
C
C  FUNCTION: Enters a new directory entry in the directory file.
C
	IMPLICIT INTEGER (A - Z)
	
	INCLUDE 'BULLDIR.INC'
	
	CHARACTER*23 TODAY_TIME

	CALL SYS$ASCTIM(,TODAY_TIME,,)
	DATE = TODAY_TIME(1:11)
	TIME = TODAY_TIME(13:20)

	CALL READDIR(0,IER)

	IF (IER.NE.1) THEN
	   NEWEST_EXDATE = '5-NOV-2000'
	   NEWEST_EXTIME = '00:00:00'
	   NBULL = 0
	   NBLOCK = 0
	   SHUTDOWN = 0
	   NEMPTY = 0
	END IF

	NEWEST_DATE = DATE
	NEWEST_TIME = TIME

	DIFF = COMPARE_DATE(NEWEST_EXDATE,EXDATE)
	IF (DIFF.GT.0) THEN
	   NEWEST_EXDATE = EXDATE
	   NEWEST_EXTIME = EXTIME
	ELSE IF (DIFF.EQ.0) THEN
	   DIFF = COMPARE_TIME(NEWEST_EXTIME,EXTIME)
	   IF (DIFF.GT.0) NEWEST_EXTIME = EXTIME
	END IF

	NBULL = NBULL + 1
	BLOCK = NBLOCK + 1
	NBLOCK = NBLOCK + LENGTH

	IF ((SYSTEM.AND.4).EQ.4) THEN
	   SHUTDOWN = SHUTDOWN + 1
	   SHUTDOWN_DATE = DATE
	   SHUTDOWN_TIME = TIME
	END IF

	CALL UPDATE_LOGIN(.TRUE.)

	CALL WRITEDIR(NBULL,IER)

	CALL WRITEDIR(0,IER)

	RETURN
	END




	INTEGER FUNCTION COMPARE_BTIM(BTIM1,BTIM2)
C
C  FUNCTION COMPARE_BTIM
C
C  FUCTION: Compares times in binary format to see which is farther in future.
C
C  INPUTS:
C	BTIM1  -  First time in binary format
C	BTIM2  -  Second time in binary format
C  OUTPUT:
C	Returns +1 if first time is farther in future
C	Returns -1 if second time is farther in future
C	Returns 0 if equal time
C
	IMPLICIT INTEGER (A - Z)

	DIMENSION BTIM1(2),BTIM2(2),DIFF(2)

	CALL LIB$SUBX(BTIM1,BTIM2,DIFF)

	IF (DIFF(2).LT.0) THEN
	   COMPARE_BTIM = -1
	ELSE IF (DIFF(2).GT.0) THEN
	   COMPARE_BTIM = +1
	ELSE
	   IF (DIFF(1).LT.0) THEN
	      COMPARE_BTIM = -1
	   ELSE IF (DIFF(1).GT.0) THEN
	      COMPARE_BTIM = +1
	   ELSE
	      COMPARE_BTIM = 0
	   END IF
	END IF

	RETURN
	END





	INTEGER FUNCTION MINUTE_DIFF(DATE2,DATE1)
C
C  FUNCTION MINUTE_DIFF
C
C  FUNCTION: Finds difference in minutes between 2 binary times.
C
C
	IMPLICIT INTEGER (A-Z)

	DIMENSION DATE1(2),DATE2(2)

	CALL LIB$DAY(DAYS1,DATE1,MSECS1)
	CALL LIB$DAY(DAYS2,DATE2,MSECS2)

	MINUTE_DIFF = (DAYS2-DAYS1)*3600 + (MSECS2-MSECS1)/6000

	RETURN
	END





 
	INTEGER FUNCTION COMPARE_DATE(DATE1,DATE2)
C
C  FUNCTION COMPARE_DATE
C
C  FUCTION: Compares dates to see which is farther in future.
C
C  INPUTS:
C	DATE1  -  First date  (dd-mm-yy)
C	DATE2  -  Second date (If is equal to ' ', then use present date)
C  OUTPUT:
C	Returns the difference in days between the two dates.
C	If the DATE1 is farther in the future, the output is positive,
C	else it is negative.
C
	IMPLICIT INTEGER (A - Z)

	CHARACTER*(*) DATE1,DATE2
	INTEGER USER_TIME(2)

	CALL SYS$BINTIM(DATE1,USER_TIME)
	CALL LIB$DAY(DAY1,USER_TIME)

	IF (DATE2.NE.' ') THEN
	   CALL SYS$BINTIM(DATE2,USER_TIME)
	ELSE
	   CALL SYS$GETTIM(USER_TIME)
	END IF

	CALL LIB$DAY(DAY2,USER_TIME)

	COMPARE_DATE = DAY1 - DAY2

	RETURN
	END




	INTEGER FUNCTION COMPARE_TIME(TIME1,TIME2)
C
C  FUNCTION COMPARE_TIME
C
C  FUCTION: Compares times to see which is farther in future.
C
C  INPUTS:
C	TIME1  -  First time	(hh:mm:ss)
C	TIME2  -  Second time
C  OUTPUT:
C	Outputs (TIME1-TIME2) in seconds.  Thus, if TIME1 is further
C	in the future, outputs positive number, else negative.
C

	IMPLICIT INTEGER (A-Z)
	CHARACTER*(*) TIME1,TIME2
	CHARACTER*23 TODAY_TIME
	CHARACTER*8 TEMP2

	IF (TIME2.EQ.' ') THEN
	   CALL SYS$ASCTIM(,TODAY_TIME,,)
	   TEMP2 = TODAY_TIME(13:20)
	ELSE
	   TEMP2 = TIME2
	END IF

	COMPARE_TIME = 3600*10*(ICHAR(TIME1(1:1))-ICHAR(TEMP2(1:1)))
     &		         +3600*(ICHAR(TIME1(2:2))-ICHAR(TEMP2(2:2)))
     &		        +60*10*(ICHAR(TIME1(4:4))-ICHAR(TEMP2(4:4)))
     &		           +60*(ICHAR(TIME1(5:5))-ICHAR(TEMP2(5:5)))
     &		           +10*(ICHAR(TIME1(7:7))-ICHAR(TEMP2(7:7)))
     &		              +(ICHAR(TIME1(8:8))-ICHAR(TEMP2(8:8)))

	RETURN
	END

C-------------------------------------------------------------------------
C
C  The following are subroutines to create a linked-list queue for 
C  temporary buffer storage of data that is read from files to be
C  outputted to the terminal.  This is done so as to be able to close
C  the file as soon as possible.
C
C  Each record in the queue has the following format.  The first two
C  words are used for creating a character variable.  The first word
C  contains the length of the character variable, the second contains
C  the address.  The address is simply the address of the 3rd word of
C  the record.  The last word in the record contains the address of the
C  next record.  Every time a record is written, if that record has a
C  zero link, it adds a new record for the next write operation. 
C  Therefore, there will always be an extra record in the queue.  To
C  check for the end of the queue, the last word (link to next record)
C  is checked to see if it is zero. 
C
C-------------------------------------------------------------------------
	SUBROUTINE INIT_QUEUE(HEADER,DATA)
	CHARACTER*(*) DATA
	IF (HEADER.NE.0) RETURN		! Queue already initialized
	LENGTH = LEN(DATA)
	CALL LIB$GET_VM(LENGTH+12,HEADER)
	CALL MAKE_CHAR(%VAL(HEADER),LENGTH)
	RETURN
	END


	SUBROUTINE WRITE_QUEUE(RECORD,NEXT,DATA)
	INTEGER RECORD(1)
	CHARACTER*(*) DATA
	LENGTH = LEN(DATA)
	CALL COPY_CHAR(LENGTH,DATA,%VAL(%LOC(RECORD)))
	NEXT = RECORD((LENGTH+12)/4)
	IF (NEXT.NE.0) RETURN
	CALL LIB$GET_VM(LENGTH+12,NEXT)
	CALL MAKE_CHAR(%VAL(NEXT),LENGTH)
	RECORD((LENGTH+12)/4) = NEXT
	RETURN
	END

	SUBROUTINE READ_QUEUE(RECORD,NEXT,DATA)
	CHARACTER*(*) DATA
	INTEGER RECORD(1)
	LENGTH = LEN(DATA)
	CALL COPY_CHAR(LENGTH,%VAL(%LOC(RECORD)),DATA)
	NEXT = RECORD((LENGTH+12)/4)
	RETURN
	END

	SUBROUTINE COPY_CHAR(LENGTH,INCHAR,OUTCHAR)
	CHARACTER*(*) INCHAR,OUTCHAR
	OUTCHAR = INCHAR(:LENGTH)
	RETURN
	END

	SUBROUTINE MAKE_CHAR(IARRAY,LEN)
	DIMENSION IARRAY(1)
	IARRAY(1) = LEN
	IARRAY(2) = %LOC(IARRAY(3))
	IARRAY(LEN/4+3) = 0
	RETURN
	END



	SUBROUTINE DISABLE_PRIVS
C
C  SUBROUTINE DISABLE_PRIVS
C
C  FUNCTION: Disable SYSPRV privileges.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($PRVDEF)'

	COMMON /PRIVS/ SETPRV
	DIMENSION SETPRV(2)

	SETPRV(1) = 0
	SETPRV(1) = IBSET(SETPRV(1),PRV$V_SYSPRV)
	SETPRV(1) = IBSET(SETPRV(1),PRV$V_WORLD)
	SETPRV(1) = IBSET(SETPRV(1),PRV$V_OPER)

	CALL SYS$SETPRV(%VAL(0),SETPRV,,)	! Disable SYSPRV 

	RETURN
	END



	SUBROUTINE ENABLE_PRIVS
C
C  SUBROUTINE ENABLE_PRIVS
C
C  FUNCTION: Enable SYSPRV privileges.
C

	IMPLICIT INTEGER (A-Z)

	COMMON /PRIVS/ SETPRV
	DIMENSION SETPRV(2)

	CALL SYS$SETPRV(%VAL(1),SETPRV,,)	! Enable SYSPRV 

	RETURN
	END



	SUBROUTINE CHECK_PRIV_IO(ERROR)
C
C  SUBROUTINE CHECK_PRIV_IO
C
C  FUNCTION: Checks SYS$OUTPUT and SYS$ERROR to see if they need
C	privileges to output to.
C

	IMPLICIT INTEGER (A-Z)

	CALL DISABLE_PRIVS			! Disable SYSPRV 

	OPEN (UNIT=6,FILE='SYS$OUTPUT',IOSTAT=IER,STATUS='NEW')
	CLOSE (UNIT=6,STATUS='DELETE')

	OPEN (UNIT=4,FILE='SYS$ERROR',IOSTAT=IER1,STATUS='NEW')
	IF (IER.NE.0.OR.IER1.NE.0) THEN
	   IF (IER1.EQ.0) WRITE (4,100)
	   IF (IER.EQ.0) WRITE (6,200)
	   ERROR = 1
	ELSE
	   CLOSE (UNIT=4,STATUS='DELETE')
	   ERROR = 0
	END IF

	CALL ENABLE_PRIVS			! Enable SYSPRV 

100	FORMAT(1X,'ERROR: SYS$OUTPUT cannot be opened.')
200	FORMAT(1X,'ERROR: SYS$ERROR cannot be opened.')

	RETURN
	END


	SUBROUTINE CHANGE_FLAG(CMD,FLAG)
C
C  SUBROUTINE CHANGE_FLAG
C
C  FUNCTION: Sets flags for specified folder.
C
C  INPUTS:
C	CMD    -   LOGICAL*4 value. If TRUE, set flag. 
C		   If FALSE, clear flag.
C	FLAG	-  If 1, modify NEW_FLAG, if 2, modify SET_FLAG
C		   If 3, modify BRIEF_FLAG, 4, modify NOTIFY_FLAG
C
	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	DIMENSION FLAGS(2,4)
	EQUIVALENCE (NEW_FLAG(1),FLAGS(1,1))

	LOGICAL CMD

	CHARACTER*23 TODAY
	DIMENSION READ_BTIM_SAVE(2)

C
C  Find user entry in BULLUSER.DAT to update information.
C

	CALL OPEN_FILE_SHARED(4)		! Open user file

	READ_BTIM_SAVE(1) = READ_BTIM(1)
	READ_BTIM_SAVE(2) = READ_BTIM(2)

	DO WHILE (REC_LOCK(IER))		! Read old entry
	 READ (4,FMT=USER_FMT,KEY=USERNAME,IOSTAT=IER) USERNAME,
     &	  LOGIN_BTIM,READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	END DO

	IF (IER.GT.0) THEN 		! No entry (how did this happen??)
	   CALL SYS$ASCTIM(,TODAY,,)
	   CALL SYS$BINTIM(TODAY,LOGIN_BTIM)
	   CALL SYS$BINTIM('5-NOV-1956 11:05:56',READ_BTIM)	! Fake new entry
	   READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER) TEMP_USER
     &	     NEWEST_BTIM,BBOARD_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,
     &	     NOTIFY_FLAG
	   IF (CMD) THEN
	      CALL SET2(FLAGS(1,FLAG),FOLDER_NUMBER)
	   ELSE
	      CALL CLR2(FLAGS(1,FLAG),FOLDER_NUMBER)
	   END IF
	   WRITE (4,FMT=USER_FMT,IOSTAT=IER) USERNAME,LOGIN_BTIM,
     &	    READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	ELSE
	   IF (CMD) THEN
	      CALL SET2(FLAGS(1,FLAG),FOLDER_NUMBER)
	   ELSE
	      CALL CLR2(FLAGS(1,FLAG),FOLDER_NUMBER)
	   END IF
	   REWRITE (4,FMT=USER_FMT,IOSTAT=IER) USERNAME,  ! Write modified entry
     &	     LOGIN_BTIM,READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,
     &	     NOTIFY_FLAG
	   READ_BTIM(1) = READ_BTIM_SAVE(1)
	   READ_BTIM(2) = READ_BTIM_SAVE(2)
	END IF

	CALL CLOSE_FILE (4)
	RETURN

	END





	SUBROUTINE CONFIRM_PRIV(USERNAME,ALLOW)
C
C  SUBROUTINE CONFIRM_PRIV
C
C  FUNCTION: Confirms that given username has SETPRV.
C
C  INPUTS:
C	USERNAME  -  Username
C  OUTPUTS:
C  	ALLOW     -  Returns 1 if account has SETPRV.
C		     returns 0 if account has no SETPRV.
C

	IMPLICIT INTEGER (A-Z)

	CHARACTER*(*) USERNAME

	INCLUDE '($PRVDEF)'

	PARAMETER UAF$Q_DEF_PRIV = '1A4'X

	LOGICAL*1 UAF(0:583)
	EQUIVALENCE (UAF(UAF$Q_DEF_PRIV),UAF_DEF_PRIV)

	CALL OPEN_FILE_SHARED(8)
	ALLOW = 0					! Set return false
	READ (8,KEY=USERNAME,IOSTAT=STATUS) UAF		! Read Record
	IF (STATUS.EQ.0) THEN				! If username found
	   IF (BTEST(UAF_DEF_PRIV,PRV$V_SETPRV).OR.	! SETPRV or CMRKNL
     &	       BTEST(UAF_DEF_PRIV,PRV$V_CMKRNL)) THEN	! privileges?
	      ALLOW = 1					! Yep
	   END IF
	END IF
	CALL CLOSE_FILE(8)
	RETURN						! Return
	END						! End





	SUBROUTINE CHECK_DISMAIL(USERNAME,DISMAIL)
C
C  SUBROUTINE CHECK_DISMAIL
C
C  FUNCTION: Checks that given username has DISMAIL.
C
C  INPUTS:
C	USERNAME  -  Username
C  OUTPUTS:
C  	DISMAIL     -  Returns 1 if account has DISMAIL.
C		       returns 0 if account has no DISMAIL.
C

	IMPLICIT INTEGER (A-Z)

	CHARACTER*(*) USERNAME

	PARAMETER UAF$V_DISMAIL = '7'X
	PARAMETER UAF$L_FLAGS = '1D4'X

	LOGICAL*1 UAF(0:583)
	EQUIVALENCE (UAF(UAF$L_FLAGS),UAF_L_FLAGS)

	CALL OPEN_FILE_SHARED(8)
	DISMAIL = 0					! Set return false
	READ (8,KEY=USERNAME,IOSTAT=STATUS) UAF		! Read Record
	IF (STATUS.EQ.0) THEN				! If username found
	   IF (BTEST(UAF_L_FLAGS,UAF$V_DISMAIL)) THEN	! DISMAIL SET?
	      DISMAIL = 1				! Yep
	   END IF
	END IF
	CALL CLOSE_FILE(8)
	RETURN						! Return
	END						! End



	INTEGER FUNCTION SYS_TRNLNM(INPUT,OUTPUT,ACCESS)

	IMPLICIT INTEGER (A-Z)

	CHARACTER*(*) INPUT,OUTPUT

        PARAMETER LNM$_STRING = '2'X

	CALL INIT_ITMLST	! Initialize item list
	CALL ADD_2_ITMLST(LEN(OUTPUT),LNM$_STRING,%LOC(OUTPUT))
	CALL END_ITMLST(TRNLNM_ITMLST)	! Get address of itemlist

	SYS_TRNLNM = SYS$TRNLNM(,'LNM$PROCESS',INPUT,ACCESS,
     &		%VAL(TRNLNM_ITMLST))

	RETURN
	END



	INTEGER FUNCTION FILE_LOCK(IER,IER1)

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($RMSDEF)'

	DATA INIT /.TRUE./

	IF (INIT) THEN
	   FILE_LOCK = 1
	   INIT = .FALSE.
	ELSE
	   IF (IER.GT.0) THEN
	      CALL ERRSNS(IDUMMY,IER1)
	      IF (IER1.EQ.RMS$_FLK) THEN
	         FILE_LOCK = 1
	      ELSE
	         FILE_LOCK = 0
	         INIT = .TRUE.
	      END IF
	   ELSE
	      FILE_LOCK = 0
	      IER1 = 0
	      INIT = .TRUE.
	   END IF
	END IF

	RETURN
	END



	SUBROUTINE ENABLE_CTRL

	IMPLICIT INTEGER (A-Z)

	COMMON /CTRLY/ CTRLY

	COMMON /CTRL_LEVEL/ LEVEL

	QUIT = 1

	ENTRY ENABLE_CTRL_EXIT

	QUIT = QUIT.AND.1		! If called via entry, QUIT = 0o
	LEVEL = LEVEL - 1

	IF (LEVEL.LT.0.AND.QUIT.EQ.1) THENg
	   WRITE (6,'('' ERROR: Error in CTRL.'')')
	END IFr

	IF (LEVEL.EQ.0.OR.QUIT.EQ.0) THEN
	   CALL LIB$ENABLE_CTRL(CTRLY,)	! Enable CTRL-Y & -CN
	END IFF

	IF (QUIT.EQ.0) CALL EXITi
	QUIT = 0			! Reinitialize

	RETURN	
	END


	SUBROUTINE DISABLE_CTRL

	IMPLICIT INTEGER (A-Z).

	COMMON /CTRLY/ CTRLYR

	COMMON /CTRL_LEVEL/ LEVEL
	DATA LEVEL /0/E

	IF (LEVEL.EQ.0) CALL LIB$DISABLE_CTRL(CTRLY,)
	LEVEL = LEVEL + 1

	RETURNU
	END




	SUBROUTINE CLEANUP_BULLFILE
CN
C  SUBROUTINE CLEANUP_BULLFILE
C 
C  FUNCTION:  Searches for empty space in bulletin file and deletes it.l
Cs
	IMPLICIT INTEGER (A - Z)o

	INCLUDE 'BULLFILES.INC'

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'

	CHARACTER FILENAME*132,INPUT*128

	CALL OPEN_FILE(2)

	CALL READDIR(0,IER)

	IF (FOLDER_SET) THEN 
	   FILENAME = FOLDER_FILE(1:TRIM(FOLDER_FILE))//'.BULLFIL' 
	ELSEn
	   FILENAME = BULLETIN_FILE
	END IFr

	IF (NEMPTY.GT.0) THEN

	 IER = LIB$RENAME_FILE(FILENAME,FILENAME(1:TRIM(FILENAME))//';2')
				! Old file name to version number 2 

	 IF (.NOT.IER) RETURN

	 OPEN (UNIT=11,FILE=FILENAME(1:TRIM(FILENAME))//';1',
     1	      STATUS='UNKNOWN',IOSTAT=IER,
     1	      RECORDTYPE='FIXED',RECORDSIZE=32,
     1	      FORM='UNFORMATTED')
				! Compressed version is number 1

	 CALL OPEN_FILE(1)			! Open bulletin file

	 NBLOCK = 0

	 DO I=1,NBULL				! Copy bulletins to new file
	   CALL READDIR(I,IER)N
	   ICOUNT = BLOCK
	   DO J=1,LENGTH/
	      NBLOCK = NBLOCK + 1
	      READ(1'ICOUNT) INPUTM
	      WRITE(11) INPUT
	      ICOUNT = ICOUNT + 1
	   END DO
	 END DO

	 CALL CLOSE_FILE(1)
	 CLOSE (UNIT=11) 

	 NEMPTY = -1		! Copying done, but not directory updating.
	 CALL WRITEDIR(0,IER)
	END IFe

	IER = LIB$DELETE_FILE(FILENAME(1:TRIM(FILENAME))//';2')
				! Can safely delete old file, since NEMPTY = -1F

	NBLOCK = 0		! Update directory entry pointers
	DO I=1,NBULLO
	   CALL READDIR(I,IER)1
	   BLOCK = NBLOCK + 1
	   CALL WRITEDIR(I,IER)
	   NBLOCK = NBLOCK + LENGTH
	END DOD

	READ (2'1,1000,IOSTAT=IER)	! Read directory headerA
     &		NEWEST_EXDATE,NEWEST_EXTIME,NEWEST_DATE,NEWEST_TIME,
     &		NBULL,NBLOCK,SHUTDOWN,SHUTDOWN_DATE,SHUTDOWN_TIME,NEMPTY
		! NOTE: Can't use READDIR since it'll call CLEANUP_BULLFILE_

	NEMPTY = 0	
	CALL WRITEDIR(0,IER)		! Update header to show no empty spaces

	CALL CLOSE_FILE(2)G

1000	FORMAT(A11,A8,A11,A8,A4,A4,A4,A11,A8,A4).

	RETURNT
	END




	SUBROUTINE CLEANUP_DIRFILE(DELETE_ENTRY)L
CT
C  SUBROUTINE CLEANUP_DIRFILE 
C 
C  FUNCTION:  Reorder directory file after deletions.A
C	      Is called either directly after a deletion, or is 
C	      called if it is detected that a deletion was not fully
C	      completed due to the fact that the deleting process 
C	      was abnormally terminated.
CE
	IMPLICIT INTEGER (A - Z) 

	INCLUDE 'BULLFILES.INC'

	INCLUDE 'BULLDIR.INC'

	NBULL = -NBULL		! Negative # Bulls signals deletion in progress
	MOVE_TO = 0		! Moving directory entries starting here
	MOVE_FROM = 0		! Moving directory entries from here
	I = DELETE_ENTRY	! Start search point for first deleted entries
	DO WHILE (MOVE_TO.EQ.0.AND.I.LE.NBULL)D
	   CALL READDIR(I,IER)V
	   IF (IER.NE.I+1) THEN	! Have we found a deleted entry?L
	      MOVE_TO = I	! If so, start moving entries to here
	      J=I+1		! Search for next entry in fileR
	      DO WHILE (MOVE_FROM.EQ.0.AND.J.LE.NBULL) 
		 CALL READDIR(J,IER)
		 IF (IER.EQ.J+1) MOVE_FROM = J
		 J = J + 1
	      END DOD
	      IF (MOVE_FROM.EQ.0) THEN	! There are no more entriesD
		 NBULL = I - 1		! so just update number of bulletins
		 CALL WRITEDIR(0,IER)N
		 RETURN
	      END IFT
	      LENGTH = -LENGTH		! Indicate starting point by writingn
	      CALL WRITEDIR(I,IER)	! next entry into deleted entryC
	      FIRST_DELETE = I		! with negative length
	      MOVE_FROM = MOVE_FROM + 1	! Set up pointers to move rest of
	      MOVE_TO = MOVE_TO + 1	! the entries
	   ELSE IF (LENGTH.LT.0) THEN	! If negative length found, deletionT
	      FIRST_DELETE = I		! was previously in progress 
	      J = I			! Try to find where entry came from
	      DO WHILE (MOVE_FROM.EQ.0.AND.J.LE.NBULL)P
		 BLOCK_SAVE = BLOCKE
		 K = J + 1		! Search for duplicate entries
		 DO WHILE (MOVE_FROM.EQ.0.AND.K.LE.NBULL) 
		    CALL READDIR(K,IER)I
		    IF (IER.EQ.K+1) THEN
		       IF (BLOCK_SAVE.EQ.BLOCK) THEN
			  MOVE_TO=J+1
			  MOVE_FROM=K+1
		       ELSEO
			  K = K + 1
		       END IFK
		    END IF
		 END DOM
		 J = J + 1		! If no duplicate entry found for this
		 CALL READDIR(J,IER)	! entry, see if one exists for any
	      END DO			! of the other entries
	   END IF
	   I = I + 1R
	END DOI

	IF (I.LE.NBULL) THEN		! Move reset of entries if necessaryB
	   IF (MOVE_FROM.GT.0) THEN
	      DO J=MOVE_FROM,NBULL 
	         CALL READDIR(J,IER) 
		 IF (IER.EQ.J+1) THEN	! Skip any other deleted entries
		    CALL WRITEDIR(MOVE_TO,IER)
		    MOVE_TO = MOVE_TO + 1b
		 END IFt
	      END DO	
	   END IF
	   DO J=MOVE_TO,NBULL		! Delete empty records at end of file 
	      DELETE(UNIT=2,REC=J+1,IOSTAT=IER)
	   END DO
	   NBULL = MOVE_TO - 1		! Update # bulletin count
	   CALL READDIR(FIRST_DELETE,IER)
	   LENGTH = -LENGTH		! Fix entry which has negative length=
	   CALL WRITEDIR(FIRST_DELETE,IER) 
	END IF_

	CALL WRITEDIR(0,IER)F

	RETURNT
	END


	SUBROUTINE SHOW_FLAGS
C 
C  SUBROUTINE SHOW_FLAGS
C
C  FUNCTION: Show READNEW and NOTIFY flags.C
CA
	IMPLICIT INTEGER (A - Z)E

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC' 

	INCLUDE 'BULLFOLDER.INC'

	LOGICAL SKIP,FLAG_NOTIFY,FLAG_READNEW,FLAG_BRIEFe
	DATA SKIP /.FALSE./

	ENTRY SHOW_BRIEF
	IF (.NOT.SKIP) THEN
	   FLAG_BRIEF = .TRUE.T
	   FLAG_NOTIFY = .FALSE.$
	   FLAG_READNEW =.FALSE.C
	   SKIP = .TRUE.A
	END IF2

	ENTRY SHOW_NOTIFY
	IF (.NOT.SKIP) THEN
	   FLAG_BRIEF = .FALSE.
	   FLAG_NOTIFY = .TRUE.
	   FLAG_READNEW =.FALSE.A
	   SKIP = .TRUE.
	END IFT

	ENTRY SHOW_READNEW 
	IF (.NOT.SKIP) THEN
	   FLAG_BRIEF = .FALSE.
	   FLAG_NOTIFY = .FALSE.:
	   FLAG_READNEW =.TRUE.
	   SKIP = .TRUE. 
	END IFd

	SKIP = .FALSE.t

C,
C  Find user entry in BULLUSER.DAT to obtain flags.i
Cr

	CALL OPEN_FILE_SHARED(4)		! Open user fileD

	DO WHILE (REC_LOCK(IER))		! Read old entrys
	 READ (4,FMT=USER_FMT,KEY=USERNAME,IOSTAT=IER) USERNAME,Z
     &	  LOGIN_BTIM,READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	END DO1

	WRITE (6,'('' For the selected folder '',A,$)') FOLDER(1:TRIM(FOLDER)) 

	IF (FLAG_READNEW) THENR
	   IF (TEST2(SET_FLAG,FOLDER_NUMBER).AND.
     &		(.NOT.TEST2(BRIEF_FLAG,FOLDER_NUMBER))) THEN
	      WRITE (6,'(''+, READNEW is set.'')')
	   ELSE
	      WRITE (6,'(''+, READNEW is not set.'')')F
	   END IF
	ELSE IF (FLAG_NOTIFY) THENp
	   IF (TEST2(NOTIFY_FLAG,FOLDER_NUMBER)) THEN
	      WRITE (6,'(''+, NOTIFY is set.'')')
	   ELSE
	      WRITE (6,'(''+, NOTIFY is not set.'')')
	   END IF
	ELSE IF (FLAG_BRIEF) THEN
	   IF (TEST2(BRIEF_FLAG,FOLDER_NUMBER)) THENu
	      WRITE (6,'(''+, BRIEF is set.'')')E
	   ELSE
	      WRITE (6,'(''+, BRIEF is not set.'')')_
	   END IF
	END IFM

	CALL CLOSE_FILE(4))

	RETURN 
	END


	SUBROUTINE SET2(FLAG,NUMBER) 

	IMPLICIT INTEGER (A-Z)

	INTEGER FLAG(2)

	F_POINT = NUMBER/32 + 1
	FLAG(F_POINT) = IBSET(FLAG(F_POINT),NUMBER-32*(F_POINT-1)) 

	RETURN(
	END


	SUBROUTINE CLR2(FLAG,NUMBER) 

	IMPLICIT INTEGER (A-Z)I

	INTEGER FLAG(2)

	F_POINT = NUMBER/32 + 1
	FLAG(F_POINT) = IBCLR(FLAG(F_POINT),NUMBER-32*(F_POINT-1)) 

	RETURN(
	END



	LOGICAL FUNCTION TEST2(FLAG,NUMBER)

	IMPLICIT INTEGER (A-Z)A

	INTEGER FLAG(2)

	F_POINT = NUMBER/32 + 1
	TEST2 = BTEST(FLAG(F_POINT),NUMBER-32*(F_POINT-1))-

	RETURN
	END
