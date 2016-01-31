C
C  BULLETIN3.FOR, Version 12/4/97
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
	SUBROUTINE UPDATE
C
C  SUBROUTINE UPDATE
C
C  FUNCTION:  Searches for bulletins that have expired and deletes them.
C
C  NOTE:  Assumes directory file is already opened.
C
	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLDIR.INC'

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /SHUTDOWN/ NODE_NUMBER,NODE_AREA
	COMMON /SHUTDOWN/ SHUTDOWN_FLAG(FLONG)

	CHARACTER*12 TEMP_DATE,TEMP_EXDATE,TEMP_NOSYSDATE
	CHARACTER*12 TEMP_TIME,TEMP_EXTIME,TEMP_NOSYSTIME

	IF (REMOTE_SET.AND.
     &		NODE_AREA.GT.0.AND.BTEST(FOLDER_FLAG,2)) THEN
	   CALL UPDATE_SHUTDOWN(FOLDER_NUMBER)
	END IF

	IF (TEST_BULLCP().OR.REMOTE_SET) RETURN
					! BULLCP cleans up expired bulletins

	ENTRY UPDATE_ALWAYS		! Entry to skip BULLCP test

	TEMP_EXDATE = '5-NOV-2100'  ! If a bulletin gets deleted, and there are
	TEMP_EXTIME = '00:00:00.00' ! are no more bulletins, this is the value
				    ! assigned to the latest expiration date

	TEMP_DATE = '5-NOV-1956' 	! Storage for computing newest
	TEMP_TIME = '00:00:00.00'	! bulletin date if deletion occurs

	TEMP_NOSYSDATE = '5-NOV-1956' 	! Storage for computing newest
	TEMP_NOSYSTIME = '00:00:00.00'	! non-system bulletin date

	BULL_ENTRY = 1				! Init bulletin pointer
	UPDATE_DONE = 0			! Flag showing bull has been deleted

	NEW_SHUTDOWN = 0
	OLD_SHUTDOWN = SHUTDOWN

	DO WHILE (UPDATE_DONE.GE.0)
	   CALL READDIR(BULL_ENTRY,IER)		! Get next directory entry
	   IF (IER.EQ.BULL_ENTRY) GO TO 100	! ERROR: Not found
	   IF ((SYSTEM.AND.7).LE.3.OR.(OLD_SHUTDOWN.EQ.0
						! If not shutdown, or time
     &	     .AND.(SYSTEM.AND.4).EQ.4)) THEN	! to delete shutdowns?
	    IF ((SYSTEM.AND.4).EQ.4) THEN	! Shutdown bulletin?
	       IF (NODE_AREA.GT.0) THEN
	          READ (EXTIME(1:2),'(I2)') I
	          READ (EXTIME(4:5),'(I2)') NODE_NUMBER_MSG
		  NODE_NUMBER_MSG = NODE_NUMBER_MSG + I*60
	          READ (EXTIME(7:8),'(I2)') I
	          READ (EXTIME(10:11),'(I2)') NODE_AREA_MSG
		  NODE_AREA_MSG = NODE_AREA_MSG + I*60
	          IF (NODE_NUMBER_MSG.EQ.NODE_NUMBER.AND.
     &		      NODE_AREA_MSG.EQ.NODE_AREA) THEN
		     DIFF = 0
		  ELSE
		     DIFF = 1
	             NEW_SHUTDOWN = NEW_SHUTDOWN + 1
		  END IF
	       END IF
	    ELSE
	       DIFF = COMPARE_DATE(EXDATE,' ')	! Has expiration date passed?
	       IF (DIFF.EQ.0) DIFF = COMPARE_TIME(EXTIME,' ')
	    END IF
	    IF (DIFF.LE.0) THEN			! If so then delete bulletin
	      CALL DELETE_ENTRY(BULL_ENTRY)	! Delete bulletin entry
	      IF (UPDATE_DONE.EQ.0) THEN	! If this is first deleted file
	         UPDATE_DONE = BULL_ENTRY	! store it to use for reordering
	      END IF				! directory file.
	    ELSE IF ((SYSTEM.AND.7).LE.3) THEN	! Expiration date hasn't passed
		! If a bulletin is deleted, we'll have to update the latest
		! expiration date. The following does that.
	      DIFF = COMPARE_DATE(EXDATE,TEMP_EXDATE)
	      IF (DIFF.LT.0.OR.(DIFF.EQ.0.AND.
     &		COMPARE_TIME(EXTIME,TEMP_EXTIME).LT.0)) THEN
	         TEMP_EXDATE = EXDATE		! If this is the latest exp
	         TEMP_EXTIME = EXTIME		! date seen so far, save it.
	      END IF
	      TEMP_DATE = DATE			! Keep date after search
	      TEMP_TIME = TIME			! we have the last message date
	      IF (.NOT.BTEST(SYSTEM,0)) THEN
		 TEMP_NOSYSDATE = DATE
		 TEMP_NOSYSTIME = TIME
	      END IF
	    END IF
	   ELSE
	      TEMP_DATE = DATE
	      TEMP_TIME = TIME
	      IF (.NOT.BTEST(SYSTEM,0)) THEN
		 TEMP_NOSYSDATE = DATE
		 TEMP_NOSYSTIME = TIME
	      END IF
	   END IF
	   BULL_ENTRY = BULL_ENTRY + 1
	END DO

100	IF (UPDATE_DONE.GT.0) THEN		! Reorder directory file
	   CALL CLEANUP_DIRFILE(UPDATE_DONE)	! due to deleted entries
	END IF

	DATE = NEWEST_DATE
	TIME = NEWEST_TIME
	CALL READDIR(0,IER)
	SHUTDOWN = NEW_SHUTDOWN
	NEWEST_EXDATE = TEMP_EXDATE
	DIFF = COMPARE_DATE(NEWEST_EXDATE,' ')
	IF (DIFF.GT.20*356) NEWEST_EXDATE = '5-NOV-2100'
	NEWEST_EXTIME = TEMP_EXTIME
	NEWEST_DATE = TEMP_DATE
	NEWEST_TIME = TEMP_TIME
	CALL WRITEDIR(0,IER)
	SYSTEM = 0			! Updating last non-system date/time
	NEWEST_DATE = TEMP_NOSYSDATE
	NEWEST_TIME = TEMP_NOSYSTIME
	CALL UPDATE_FOLDER
	SYSTEM = 1			! Now update latest date/time
	NEWEST_DATE = TEMP_DATE
	NEWEST_TIME = TEMP_TIME
	CALL UPDATE_FOLDER

	IF (NODE_AREA.GT.0.AND.BTEST(FOLDER_FLAG,2)) THEN ! Shutdowns deleted?
	   CALL UPDATE_SHUTDOWN(FOLDER_NUMBER)		  ! Save that info
	END IF

C
C  If newest message date has been changed, must change it in BULLUSER.DAT
C  and also see if it affects notification of new messages to users
C
	IF (TEMP_DATE.NE.DATE.OR.TEMP_TIME.NE.TIME) THEN
	   CALL UPDATE_LOGIN(.FALSE.)
	END IF

	RETURN

	END



	SUBROUTINE UPDATE_READ(USERFILE_OPEN)
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

	CHARACTER TODAY*24

	DIMENSION TODAY_BTIM(2),READ_BTIM_SAVE(2)

	LOGICAL MODIFY_SYSTEM /.TRUE./

C
C  Update user's latest read time in his entry in BULLUSER.DAT.
C
	IF (.NOT.USERFILE_OPEN) THEN
	   CALL OPEN_BULLUSER_SHARED		! Get BULLUSER.DAT file
	END IF

	CALL READ_USER_FILE_HEADER(IER)

	IF (IER.NE.0) THEN			! If header not present, exit
	   IF (.NOT.USERFILE_OPEN) CALL CLOSE_BULLUSER
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
	   IF (USERFILE_OPEN.EQ.0) THEN
	      READ_BTIM_SAVE(1) = READ_BTIM(1)
	      READ_BTIM_SAVE(2) = READ_BTIM(2)
	      READ_BTIM(1) = TODAY_BTIM(1)
	      READ_BTIM(2) = TODAY_BTIM(2)
	      REWRITE (4) USER_ENTRY
     	      READ_BTIM(1) = READ_BTIM_SAVE(1)
	      READ_BTIM(2) = READ_BTIM_SAVE(2)
	   END IF
	ELSE					! If no entry create a new entry
	   NEW_FLAG(1) = 143
	   NEW_FLAG(2) = 0
	   LOGIN_BTIM(1) = TODAY_BTIM(1)
	   LOGIN_BTIM(2) = TODAY_BTIM(2)
	   READ_BTIM(1) = TODAY_BTIM(1)
	   READ_BTIM(2) = TODAY_BTIM(2)
	   CALL WRITE_USER_FILE_NEW(IER)
	END IF

	IF (MODIFY_SYSTEM) THEN
	   CALL MODIFY_SYSTEM_LIST(1)
	   MODIFY_SYSTEM = .FALSE.
	END IF

	IF (.NOT.USERFILE_OPEN) THEN
	   CALL CLOSE_BULLUSER			! All finished with BULLUSER
	END IF

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
C  Now see if bulletins have been added since the user's previous
C  read time.  If they have, then search for the first new bulletin.
C  Ignore new bulletins that are owned by the user or system notices
C  that have not been added since the user has logged in.
C
	BULL_POINT = -1				! Init bulletin pointer

	CALL OPEN_BULLDIR_SHARED		! Yep, so get directory file
	CALL READDIR(0,IER)			! Get # bulletins from header
	IF (IER.EQ.1) THEN
	   CALL GET_NEWEST_MSG(LAST_READ_BTIM(1,FOLDER_NUMBER+1),START)
	   IF (START.LE.0) THEN
	      BULL_POINT = START
	      CALL CLOSE_BULLDIR
	      RETURN
	   END IF
	   DO WHILE (START.LE.NBULL.AND.(FROM.EQ.USERNAME.OR.SYSTEM))
	      IF (FROM.NE.USERNAME) THEN	! Ignore bull if owner is user
	         IF (SYSTEM) THEN		! If system bulletin
	            CALL SYS_BINTIM(DATE//' '//TIME,DIR_BTIM)
	            DIFF = COMPARE_BTIM(LOGIN_BTIM,DIR_BTIM)
		    IF (DIFF.GT.0) THEN
		       START = START + 1
	               CALL READDIR(START,IER)
		    ELSE			! SYSTEM bulletin was not seen
		       SYSTEM = 0		! so force exit to read it.
		    END IF
	         END IF
	      ELSE
		 START = START + 1
		 CALL READDIR(START,IER)
		 IF (IER.NE.START+1) START = NBULL + 1
	      END IF
	   END DO
	   IF (START.LE.NBULL) BULL_POINT = START - 1
	END IF

	CALL CLOSE_BULLDIR

	RETURN
	END



	SUBROUTINE GET_EXPIRED(EXPDAT,IER)

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLDIR.INC'

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	CHARACTER*24 EXPDAT
	CHARACTER*24 TODAY

	DIMENSION EXTIME_BIN(2),NOW(2)

	EXTERNAL CLI$_ABSENT

	IER = SYS$ASCTIM(,TODAY,,)		! Get today's date

	IERC = CLI$GET_VALUE('EXPIRATION',EXPDAT,ILEN)

	PROMPT = .TRUE.

	EXPIRE_LIMIT = F_EXPIRE_LIMIT
	IF (REMOTE_SET.EQ.4.AND.EXPIRE_LIMIT.EQ.0)
     &	   EXPIRE_LIMIT = NEWS_EXPIRE_LIMIT_DEFAULT

5	IF (PROMPT) THEN
	   IF (IERC.NE.%LOC(CLI$_ABSENT)) THEN	! Was value specified?
	      PROMPT = .FALSE.
	   ELSE
	      DEFAULT_EXPIRE = FOLDER_BBEXPIRE
	      IF (REMOTE_SET.EQ.4.AND.DEFAULT_EXPIRE.EQ.0)
     &		 DEFAULT_EXPIRE = NEWS_EXPIRE_DEFAULT
	      IF ((DEFAULT_EXPIRE.GT.EXPIRE_LIMIT.OR.DEFAULT_EXPIRE
     &		  .EQ.0).AND.EXPIRE_LIMIT.GT.0.AND..NOT.
     &	          FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)) THEN
		 DEFAULT_EXPIRE = F_EXPIRE_LIMIT
	      END IF
	      IF (BTEST(FOLDER_FLAG,3).OR.
     &		  REMOTE_SET.EQ.4) THEN		! NOPROMPT was set
		 IF (DEFAULT_EXPIRE.LE.0) THEN		! If no expiration date
	            SYSTEM = SYSTEM.OR.2		! make permanent
	            EXPDAT = '5-NOV-2100 00:00:00.00'
		 ELSE					! Else set expiration
		    CALL GET_EXDATE(EXPDAT,DEFAULT_EXPIRE)
		    EXPDAT = EXPDAT(:TRIM(EXPDAT))//' 00:00:00.00'
		 END IF
		 ILEN = TRIM(EXPDAT)
	      ELSE
		 IF (DEFAULT_EXPIRE.EQ.0) THEN	! Get expiration date
	            WRITE(6,1030) TODAY(:INDEX(TODAY,'.')-4)
		 ELSE IF (DEFAULT_EXPIRE.EQ.-1) THEN
	            WRITE(6,1031) TODAY(:INDEX(TODAY,'.')-4)
		 ELSE
	            WRITE(6,1032) TODAY(:INDEX(TODAY,'.')-4),
     &					DEFAULT_EXPIRE
		 END IF
		 WRITE (6,1035)
	         CALL GET_LINE(EXPDAT,ILEN)	! Get EXPDAT line
		 IF (ILEN.EQ.0.AND.DEFAULT_EXPIRE.NE.0) THEN
		    IF (DEFAULT_EXPIRE.EQ.-1) THEN
		       EXPDAT = '5-NOV-2100 00:00:00.00'
		       SYSTEM = IBSET(SYSTEM,1)	! Indicate permanent message
		    ELSE
		       CALL GET_EXDATE(EXPDAT,DEFAULT_EXPIRE)
		       EXPDAT = EXPDAT(:TRIM(EXPDAT))//' 00:00:00.00'
		    END IF
		    ILEN = TRIM(EXPDAT)
		 END IF
	      END IF
	   END IF
	ELSE
	   RETURN
	END IF

	IF (ILEN.LE.0) THEN
	   IER = 0
	   RETURN
	END IF

	EXPDAT = EXPDAT(:ILEN)			! Change trailing zeros 2 spaces

	IF (INDEX(EXPDAT,'-').EQ.0.AND.INDEX(EXPDAT,':').GT.0.AND.
     &		INDEX(EXPDAT(:ILEN),' ').EQ.0) THEN	! Only time specified?
	   EXPDAT = TODAY(:INDEX(TODAY(2:),' ')+1)//EXPDAT	! Add date
	ELSE IF (INDEX(EXPDAT(6:),'-').EQ.0.AND.		! Date specified
     &			INDEX(EXPDAT,'-').GT.0) THEN	! but no year?
	   SPACE = INDEX(EXPDAT,' ') - 1			! Add year
	   IF (SPACE.EQ.-1) SPACE = TRIM(EXPDAT) 
	   YEAR = INDEX(TODAY(6:),'-')
	   EXPDAT = EXPDAT(:SPACE)//TODAY(5+YEAR:9+YEAR)//EXPDAT(SPACE+1:)
	END IF

	CALL STR$UPCASE(EXPDAT,EXPDAT)		! Convert to upper case
	IER = SYS_BINTIM(EXPDAT,EXTIME_BIN)
	IF (IER.NE.1) THEN			! If not able to do so
    	   WRITE(6,1040)			! tell user is wrong
	   IER = 0				! Set error for return value
	   GO TO 5				! Re-request date (if prompting)
	END IF
	IER = SYS$ASCTIM(TIMLEN,EXPDAT,EXTIME_BIN,)
	IF (TIMLEN.EQ.16) THEN
	   CALL SYS$GETTIM(NOW)
	   CALL LIB$SUBX(NOW,EXTIME_BIN,EXTIME_BIN)
	   IER = SYS$ASCTIM(TIMLEN,EXPDAT,EXTIME_BIN,)
	END IF

	IF (EXPDAT(2:2).EQ.'-') EXPDAT = '0'//EXPDAT
	IER = COMPARE_DATE(EXPDAT(:11),TODAY(:11)) ! Compare date with today's
	IF (IER.GT.EXPIRE_LIMIT.AND.EXPIRE_LIMIT.GT.0.AND.
     &	    .NOT.FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)) THEN
	   WRITE(6,1050) EXPIRE_LIMIT		! Expiration date > limit
	   IER = 0				! Set error for return value
	   GO TO 5				! Re-request date (if prompting)
	END IF
	IF (IER.EQ.0) IER = COMPARE_TIME(EXPDAT(13:23),TODAY(13:23))
	IF (IER.LE.0) THEN			! If expiration date not future
	   WRITE(6,1045)			! tell user
	   IER = 0				! Set error for return value
	   GO TO 5				! Re-request date (if prompting)
	END IF

	IF (PROMPT) THEN
	   IF (BTEST(SYSTEM,1)) THEN		! Permanent message
	      WRITE (6,'('' Message will be permanent.'')')
	   ELSE
	      WRITE (6,'('' Expiration date will be '',A,''.'')')
     &		EXPDAT(:TRIM(EXPDAT))
	   END IF
	END IF

	IER = 1

	RETURN

1030	FORMAT(' It is ',A,'. Specify when message expires.')
1031	FORMAT(' It is ',A,'. Specify when message expires.',
     &		' Default is permanent.')
1032	FORMAT(' It is ',A,'. Specify when message expires.',
     &		' Default is ',I3,' days.')
1035    Format(' Enter absolute time: [dd-mmm-yyyy] hh:mm:ss ',
     &		'or delta time: dddd hh:mm:ss')
1040	FORMAT(' ERROR: Invalid date format specified.')
1045	FORMAT(' ERROR: Specified time has already passed.')
1050	FORMAT(' ERROR: Specified expiration period too large.'
     &		' Limit is ',I3,' days.')

	END


	SUBROUTINE MAILEDIT(INFILE,OUTFILE)

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($SSDEF)'

	INCLUDE 'BULLUSER.INC'

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*256 INCMD

	EXTERNAL BULLETIN_SUBCOMMANDS

	CHARACTER*(*) INFILE,OUTFILE

	CHARACTER*80 MAIL_EDIT,OUT
	DATA MAIL_EDIT /' '/

	CHARACTER*132 INPUT

	CHARACTER*256 SPAWN_COMMAND

	EXTERNAL ERROR_TRAP

	IF (CAPTIVE(2)) THEN
	   WRITE (6,'('' ERROR: /EDIT not allowed from CAPTIVE account.'')')
	   RETURN
	END IF

	IF (MAIL_EDIT.EQ.' ') THEN
	  IF (.NOT.SYS_TRNLNM('MAIL$EDIT',MAIL_EDIT)) THEN
	    OPEN (UNIT=10,FILE='VMSMAIL_PROFILE',
     &	     DEFAULTFILE='SYS$SYSTEM:VMSMAIL_PROFILE.DATA',
     &       ACCESS='KEYED',FORM='FORMATTED',ORGANIZATION='INDEXED',
     &       STATUS='OLD',READONLY,SHARED,IOSTAT=IER)
	    IF (IER.EQ.0) THEN
	      DO WHILE (REC_LOCK(IER))
	         READ(10,'(A)',KEY=USERNAME,IOSTAT=IER) INPUT
	      END DO
	      CLOSE (UNIT=10)
	      IF (IER.EQ.0) THEN
		 INPUT = INPUT(32:)
		 DO WHILE (TRIM(INPUT).GT.0)
		    IF (ICHAR(INPUT(1:1)).EQ.8) THEN
		       MAIL_EDIT = 'CALLABLE_'//INPUT(5:4+ICHAR(INPUT(3:3)))
		       INPUT = ' '
		    ELSE
		       INPUT = INPUT(ICHAR(INPUT(3:3))+5:)
	            END IF
	         END DO
	      END IF
	    END IF
	  END IF
	  CALL STR$UPCASE(MAIL_EDIT,MAIL_EDIT)
	END IF

	OUT = OUTFILE
	IF (TRIM(OUT).EQ.0) THEN
	   OUT = INFILE
	END IF

	CALL DISABLE_PRIVS
	CALL DECLARE_CTRLC_AST
	IF (TRIM(MAIL_EDIT).GT.0
     &		.AND.INDEX(MAIL_EDIT,'CALLABLE_').EQ.0) THEN
	   IF (MAIL_EDIT(:1).EQ.'@') MAIL_EDIT = MAIL_EDIT(2:)
	   IF (OUT.EQ.INFILE) THEN
	      SPAWN_COMMAND = '$@'//MAIL_EDIT(:TRIM(MAIL_EDIT))
     &		//' "" '//OUT(:TRIM(OUT))
	   ELSE
	      SPAWN_COMMAND = '$@'//MAIL_EDIT(:TRIM(MAIL_EDIT))
     &		//' '//INFILE//' '//OUT(:TRIM(OUT))
	   END IF
	   CALL LIB$SPAWN(SPAWN_COMMAND)
	ELSE
	   IF (TRIM(MAIL_EDIT).EQ.0) MAIL_EDIT = 'CALLABLE_EDT'

C           The string CALLABLE_ was found.  Extract the editor name from the
C           string and look up the entry point in the shareable image.

	   N = INDEX(MAIL_EDIT,'_')+1
	   IER = LIB$FIND_IMAGE_SYMBOL(
     &		MAIL_EDIT(N:TRIM(MAIL_EDIT))//'SHR',
     &		MAIL_EDIT(N:TRIM(MAIL_EDIT))//'$EDIT',ENTRYADDR)
	   IF (IER) THEN
	      CONTEXT = 0
	      IER1 = LIB$FIND_FILE(INFILE,INPUT,CONTEXT)
	      IF (MAIL_EDIT.EQ.'CALLABLE_EDT') THEN
	         IF (.NOT.IER1) THEN
	            CALL EDT$EDIT('NL:',OUT)
	         ELSE
	            CALL EDT$EDIT(INFILE,OUT)
	         END IF
	      ELSE
                 IF (.NOT.IER1) THEN
                    CALL EDITMESSAGE(%VAL(ENTRYADDR),' ',OUT)
                 ELSE
                    CALL EDITMESSAGE(%VAL(ENTRYADDR),INFILE,OUT)
                 END IF
	      END IF
	      CALL LIB$ESTABLISH(ERROR_TRAP)
	      IER = CLI$DCL_PARSE(INCMD,BULLETIN_SUBCOMMANDS)
              CALL LIB$REVERT
	   ELSE
	      WRITE(6,'('' Could not activate editor.'')')
	   END IF
        END IF
	CALL CANCEL_CTRLC_AST
	CALL ENABLE_PRIVS

	RETURN
	END



        SUBROUTINE EDITMESSAGE(EDITOR,INFILE,OUTFILE)

        CHARACTER*(*) INFILE,OUTFILE

        EXTERNAL EDITOR

        CALL EDITOR(INFILE,OUTFILE)

        RETURN
        END



	SUBROUTINE CREATE_BULLCP

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($PRCDEF)'

	INCLUDE '($SSDEF)'

	INCLUDE '($PRVDEF)'

	INCLUDE 'BULLFILES.INC'

	COMMON /REALPROC/ REALPROCPRIV(2)

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	LOGICAL*1 QUOTA(32)

	DIMENSION IMAGEPRIV(2)

	CHARACTER IMAGENAME*132,ANSWER*4

	IF (.NOT.SETPRV_PRIV()) THEN
	   WRITE (6,'('' ERROR: You do not have the privileges '',
     &			''to execute the command.'')')
	   CALL EXIT
	END IF

	JUST_STOP = CLI$PRESENT('STOP')

	IF (JUST_STOP.AND..NOT.BTEST(REALPROCPRIV(1),PRV$V_SETPRV)) THEN
	   WRITE (6,'('' ERROR: You need SETPRV to execute /STOP.'')')
	   CALL EXIT
	ELSE IF (.NOT.JUST_STOP.AND.
     &			.NOT.BTEST(REALPROCPRIV(1),PRV$V_SYSNAM)) THEN
	   CALL SYS$SETPRV(,,,IMAGEPRIV)
	   IF (.NOT.BTEST(IMAGEPRIV(1),PRV$V_SYSNAM)) THEN
	      WRITE (6,'('' ERROR: This new version of BULLETIN'',
     &			'' needs to be installed with SYSNAM.'')')
	      CALL EXIT
	   END IF
	END IF

	IF (TEST_BULLCP()) THEN
	   IF (.NOT.JUST_STOP) THEN
	      WRITE (6,'('' BULLCP process running.
     & Do you wish to kill it and restart a new one? '',$)')
	      READ (5,'(A)') ANSWER(:1)
	      IF (ANSWER(:1).NE.'Y'.AND.ANSWER(:1).NE.'y') CALL EXIT
	   END IF

	   CALL DELPRC('BULLCP',IER)

	   IF (.NOT.IER) THEN
	      CALL SYS_GETMSG(IER)
	      CALL EXIT
	   ELSE IF (JUST_STOP) THEN
	      WRITE (6,'('' BULLCP process has been terminated.'')')
	      CALL EXIT
	   END IF
	ELSE IF (JUST_STOP) THEN
	   WRITE (6,'('' BULLCP is not presently running.'')')
	   CALL EXIT
	END IF

	CALL GETIMAGE(IMAGENAME,ILEN)

	LEN_B = TRIM(FOLDER_DIRECTORY)

	CALL SYS$SETDFPROT('AA00'X,CUR_DEF_PROT)
		! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD:RW,GROUP:RW)
C
C  Generate a new BULLCP.COM each time.  This is done in case the BULLETIN
C  executeable is moved, or a new version of BULLETIN is being installed that
C  has changes to BULLCP.COM.  (It's also a security risk to execute the old
C  copy, as someone might have been able to write into that directory and
C  replace BULLCP.COM, and the command procedure is executed under the
C  SYSTEM account, so it has all privileges.)
C
	OPEN(UNIT=11,FILE=FOLDER_DIRECTORY(:LEN_B)//'BULLCP.COM',
     &		STATUS='NEW',IOSTAT=IER,CARRIAGECONTROL='LIST')
	IF (IER.NE.0) RETURN
	WRITE(11,'(A)') '$SET NOON'
	WRITE(11,'(A)') '$B:=$'//IMAGENAME(:ILEN)
	WRITE(11,'(A)') '$LOOP:'
	WRITE(11,'(A)') '$PURGE '//FOLDER_DIRECTORY(:LEN_B)//'BULLCP.LOG'
	WRITE(11,'(A)') '$DEF/USER SYS$OUTPUT '
     &				//FOLDER_DIRECTORY(:LEN_B)//'BULLCP.LOG'
	WRITE(11,'(A)') '$DEF/USER SYS$ERROR '
     &				//FOLDER_DIRECTORY(:LEN_B)//'BULLCP.ERR'
	WRITE(11,'(A)') '$B/BULLCP'
	WRITE(11,'(A)') '$WAIT 00:01:00'
	WRITE(11,'(A)') '$GOTO LOOP'		! File open timed out
	CLOSE(UNIT=11)
	CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection

	CALL GETQUOTA(QUOTA,1)

	IER = 0
	DO WHILE (IER.EQ.0.OR.IER.EQ.SS$_DUPLNAM)
	   IER = SYS$CREPRC(,'SYS$SYSTEM:LOGINOUT',FOLDER_DIRECTORY(:LEN_B)
     &		//'BULLCP.COM','NL:',,,QUOTA,'BULLCP',%VAL(4),
     &		,,%VAL(PRC$M_NOUAF+PRC$M_DETACH))
	END DO

	IF (IER) THEN
	   OPEN(UNIT=11,FILE=FOLDER_DIRECTORY(:LEN_B)//'BULLCP.COM;-1',
     &		STATUS='OLD',IOSTAT=IER1)
	   IF (IER1.EQ.0) CLOSE(UNIT=11,STATUS='DELETE',IOSTAT=IER1)
	END IF

	IF (.NOT.IER) THEN
	   CALL SYS_GETMSG(IER)
	ELSE
	   IF (CONFIRM_USER('DECNET').NE.0) THEN
	      WRITE (6,'('' WARNING: Account with username DECNET'',
     &				'' does not exist.'')')
	      WRITE (6,'('' BULLCP will be owned by present account.'')')
	   END IF
	   WRITE (6,'('' Successfully created BULLCP detached process.'')')
	END IF
	CALL EXIT

	END






	SUBROUTINE FIND_BULLCP

	IMPLICIT INTEGER (A-Z)

	COMMON /BCP/ BULLCP
	DATA BULLCP /0/

	CHARACTER*80 TEMP

	IER = SYS_TRNLNM('BULL_BULLCP',TEMP)
	IF (IER.AND.TEMP.NE.'IGNORE') BULLCP = 1

	RETURN
	END




	LOGICAL FUNCTION TEST_BULLCP

	IMPLICIT INTEGER (A-Z)

	COMMON /BCP/ BULLCP
	LOGICAL BULLCP

	TEST_BULLCP = BULLCP

	RETURN
	END




	SUBROUTINE RUN_BULLCP

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFILES.INC'

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLUSER.INC'

	COMMON /BCP/ BULLCP
	LOGICAL BULLCP

	COMMON /KNOWN_FOLDERS/ FOLDER_Q1,NUM_FOLDERS

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /BBOARD_LOOP/ BBOARD_LOOP

	CHARACTER*24 OLD_TIME,NEW_TIME

        COMMON /MAIN_FOLDER_DIRECTORY/ FOLDER1_DIRECTORY
	CHARACTER*80 FOLDER1_DIRECTORY

	COMMON /SHUTDOWN/ NODE_NUMBER,NODE_AREA
	COMMON /SHUTDOWN/ SHUTDOWN_FLAG(FLONG)

	IF (TEST_BULLCP()) CALL EXIT	! BULLCP already running, so exit.

	CALL LIB$DATE_TIME(OLD_TIME)

	BULLCP = 2			! Enable process to do BULLCP functions

	IER = SYS$CREMBX(%VAL(1),CHAN,,,,,'BULL_BULLCP')
	IF (.NOT.IER) THEN		! Can't create mailbox, so exit.
	   CALL SYS_GETMSG(IER)
	   CALL EXIT
	END IF

	IER = SYS$DELMBX(%VAL(CHAN))	! If process dies, mailbox is deleted.

	CALL REGISTER_BULLCP

	CALL SET_REMOTE_SYSTEM

	CALL START_DECNET

	BBOARD_LOOP = 0
	NEWS_LOOP = 0
	NOW = SYS_TRNLNM('BULL_NEWS_CLEANUP','DEFINED')

	DO WHILE (NEWS_LOOP.GE.0)		! Loop once every 15 minutes
	   CALL SYS$SETAST(%VAL(0))

	   UPDATEBBOARD = 1
	   IF (SYS_TRNLNM('BULL_BBOARD_UPDATE',BULL_PARAMETER)) THEN
	      LEN_P = TRIM(BULL_PARAMETER)
	      DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER,IOSTAT=IER)
     &		  UPDATEBBOARD
	      IF (IER.EQ.0) UPDATEBBOARD = (UPDATEBBOARD+14) / 15
	   END IF

	   UPDATENEWS = 4
	   IF (SYS_TRNLNM('BULL_NEWS_UPDATE',BULL_PARAMETER)) THEN
	      LEN_P = TRIM(BULL_PARAMETER)
	      DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER,IOSTAT=IER)
     &		  UPDATENEWS
	      IF (IER.EQ.0) UPDATENEWS = (UPDATENEWS+14) / 15
	   END IF

	   CALL LIB$DATE_TIME(NEW_TIME)
	   CALL GET_PROXY_ACCOUNTS	! Proxy info for incoming connections
	   FOLDER1_DIRECTORY = FOLDER_DIRECTORY
	   CALL SYS$SETAST(%VAL(1))

           IF (.NOT.NOW) NOW = INDEX(NEW_TIME,' 03:').NE.0.AND.
     &                     INDEX(OLD_TIME,' 03:').EQ.0
	   IER = 1
	   DO WHILE (IER)
	      CALL BBOARD			! Look for BBOARD messages.
	      FOLDER_Q = FOLDER_Q1		! Init queue pointer to header
	      POINT_FOLDER = 0
	      DO WHILE (POINT_FOLDER.LT.NUM_FOLDERS)
		 POINT_FOLDER = POINT_FOLDER + 1
		 CALL SYS$SETAST(%VAL(0))
		 CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)
		 IF (FOLDER_BBOARD(:2).NE.'::') THEN
		    CALL SELECT_FOLDER(.FALSE.,IER)	! Select folder
		    IF (IER) THEN
		       CALL DELETE_EXPIRED	! Delete expired messages
		       IF (NOW) THEN	! Do empty block cleanup at 3 a.m.
			  IF (NEMPTY.GT.200) THEN
			     CALL CLEANUP_BULLFILE	! Cleanup empty blocks
			  END IF
		       END IF
		    END IF
		 END IF
	         IF (TEST2(SHUTDOWN_FLAG,FOLDER_NUMBER)) THEN
		    CALL UPDATE_SHUTDOWN(FOLDER_NUMBER)
	         END IF
		 CALL SYS$SETAST(%VAL(1))
	      END DO
	      IF (NOW) THEN  ! Cleanup deleted users from files at 3 a.m.
	         CALL SYS$SETAST(%VAL(0))
	         CALL TOTAL_CLEANUP_LOGIN
	         CALL SYS$SETAST(%VAL(1))
	      END IF
	      CALL SYS$SETAST(%VAL(0))
	      CALL REGISTER_BULLCP
	      IER1 = 1
	      DO WHILE (IER1)
	         IER = SYS_TRNLNM_SYSTEM_INDEX('BULL_DIR_LIST',
     &			   FOLDER_DIRECTORY)
	         IF (IER.AND.FOLDER_DIRECTORY.EQ.FOLDER1_DIRECTORY) THEN 
		    IER1 = 1
	         ELSE
		    IER1 = 0
		 END IF
      	      END DO
	      IF (IER) CALL ADD_DIRECTORIES
	      CALL SYS$SETAST(%VAL(1))
	   END DO

           CALL SYS$SETAST(%VAL(0)) 
	   FOLDER_DIRECTORY = FOLDER1_DIRECTORY
           CALL ADD_DIRECTORIES
           CALL SYS$SETAST(%VAL(1))

	   BBOARD_LOOP = BBOARD_LOOP + 1
	   IF (BBOARD_LOOP.EQ.UPDATEBBOARD) BBOARD_LOOP = 0

	   CALL SYS$SETAST(%VAL(0))
	   IF (SYS_TRNLNM('BULL_NEWS_SERVER','DEFINED').AND.
     &	      (NEWS_LOOP.EQ.0.OR.NOW)) THEN
	      IF (NOW) THEN
	         CALL CREATE_PROCESS('BULLCP NEWS1')
	      ELSE
	         CALL CREATE_PROCESS('BULLCP NEWS')
	      END IF
           END IF
	   CALL SYS$SETAST(%VAL(1))

	   C = 0
	   IF (LIB$FIND_FILE(FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))
     &			//'*.SMTP',INPUT,C)) THEN
	      CALL CREATE_PROCESS('BULLCP SMTP')
	   END IF

	   NOW = .FALSE.

	   NEWS_LOOP = NEWS_LOOP + 1
	   IF (NEWS_LOOP.EQ.UPDATENEWS) NEWS_LOOP = 0

	   OLD_TIME = NEW_TIME
	   CALL HIBER('15')		! Wait for 15 minutes
C
C  Look at remote folders and update local info to reflect new messages.
C  Do here after waiting in case problem with connecting to remote folder
C  which requires killing process.
C

	   FOLDER_Q = FOLDER_Q1
	   POINT_FOLDER = 0
	   DO WHILE (POINT_FOLDER.LT.NUM_FOLDERS)
	      POINT_FOLDER = POINT_FOLDER + 1
	      CALL SYS$SETAST(%VAL(0))
	      CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)
	      IF (FOLDER_BBOARD(:2).EQ.'::') THEN
		 CALL SELECT_FOLDER(.FALSE.,IER)
	      END IF
	      CALL SYS$SETAST(%VAL(1))
	   END DO
	   CALL SYS$SETAST(%VAL(0))
	   FOLDER_NUMBER = 0			! Reset to GENERAL folder
	   CALL SELECT_FOLDER(.FALSE.,IER)
	   CALL SYS$SETAST(%VAL(1))
	END DO

	RETURN
	END





	SUBROUTINE SET_REMOTE_SYSTEM

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /SYSTEM_FOLDERS/ SYSTEM_FLAG(FLONG),NODENAME
	CHARACTER NODENAME*8

	DIMENSION NEW_SYSTEM_FLAG(FLONG)

	CALL LIB$SYS_TRNLOG('SYS$NODE',,NODENAME)
	NODENAME = NODENAME(2:INDEX(NODENAME,':')-1)

 	CALL OPEN_BULLFOLDER_SHARED

	IER = 0
	DO WHILE (IER.EQ.0)
	   CALL READ_FOLDER_FILE(IER)
	   IF (BTEST(FOLDER_FLAG,2))
     &		CALL SET2(NEW_SYSTEM_FLAG,FOLDER_NUMBER)
	   IF (FOLDER_BBOARD(:2).EQ.'::'.AND.BTEST(FOLDER_FLAG,2)
     &		.AND.IER.EQ.0) THEN
	      CALL CLOSE_BULLFOLDER
	      CALL SETUSER(FOLDER_OWNER)
	      CALL SELECT_FOLDER(.FALSE.,IER1)
	      IF (IER1) THEN
	         WRITE(REMOTE_UNIT,'(3A)',IOSTAT=IER1) 14,
     &			BTEST(FOLDER_FLAG,2),NODENAME
	      END IF
	      CALL SETUSER(USERNAME)
 	      CALL OPEN_BULLFOLDER_SHARED
	      CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)
	   END IF
	END DO

	CALL CLOSE_BULLFOLDER

	FOLDER_NUMBER = 0			! Reset to GENERAL folder
	CALL SELECT_FOLDER(.FALSE.,IER)

	FOLDER1_FLAG = FOLDER_FLAG
	DO FOLDER_NUMBER=0,FOLDER_MAX-1
	   IF (TEST2(SYSTEM_FLAG,FOLDER_NUMBER).AND..NOT.
     &	       TEST2(NEW_SYSTEM_FLAG,FOLDER_NUMBER)) THEN
	      FOLDER_FLAG = IBCLR(FOLDER_FLAG,2)
	      CALL MODIFY_SYSTEM_LIST(0)
	   END IF
	END DO
	FOLDER_FLAG = FOLDER1_FLAG
	FOLDER_NUMBER = 0

	RETURN
	END




	SUBROUTINE REGISTER_BULLCP

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLUSER.INC'

	INTEGER SHUTDOWN_BTIM(FLONG)

	EQUIVALENCE (SHUTDOWN_BTIM,BRIEF_FLAG)

	COMMON /SYSTEM_FOLDERS/ SYSTEM_FLAG(FLONG),NODENAME
	CHARACTER NODENAME*8

	COMMON /SHUTDOWN/ NODE_NUMBER,NODE_AREA
	COMMON /SHUTDOWN/ SHUTDOWN_FLAG(FLONG)

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	CHARACTER REGNODE*8

	CALL OPEN_BULLUSER

	DO WHILE (REC_LOCK(IER))
	   READ (4,KEY='*SYSTEM',IOSTAT=IER) 
     &		TEMP_USER,REGNODE,NODE_NUMBER,NODE_AREA,NEW_FLAG,
     &		SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG
	END DO

	CALL LIB$SYS_TRNLOG('SYS$NODE',,NODENAME)
	NODENAME = NODENAME(2:INDEX(NODENAME,':')-1)

	IF (IER.NE.0) THEN
	   DO I=1,FLONG
	      SYSTEM_FLAG(I) = 0
	      SHUTDOWN_FLAG(I) = 0
	   END DO
	   CALL SET2(SYSTEM_FLAG,0)
	   NODE_AREA = 0
	   WRITE (4,IOSTAT=IER)
     &		'*SYSTEM     ',NODENAME,NODE_NUMBER,NODE_AREA,NEW_FLAG,
     &		SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG
	   CALL CLOSE_BULLUSER
	ELSE
	   DO I=1,FLONG
	      SHUTDOWN_FLAG(I) = SYSTEM_FLAG(I)
	   END DO
	   REWRITE (4,IOSTAT=IER)
     &		TEMP_USER,NODENAME,NODE_NUMBER,NODE_AREA,NEW_FLAG,
     &		SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG
           IF (NODENAME.EQ.REGNODE) THEN
	      CALL CLOSE_BULLUSER
	      RETURN
	   END IF
	   TEMP_USER = ':'
	   DO WHILE (TEMP_USER(:1).EQ.':')
	      DO WHILE (REC_LOCK(IER))		 
	         READ (4,KEYGT=TEMP_USER,IOSTAT=IER)
     &		   TEMP_USER,LOGIN_BTIM,READ_BTIM,NEW_FLAG,USERNAME
		 TEMP_USER = TEMP_USER(:TRIM(TEMP_USER))
	      END DO
	      IF (TEMP_USER(:1).NE.':'.OR.IER.NE.0) THEN
		 CALL CLOSE_BULLUSER
		 RETURN
	      END IF
	      OPEN (UNIT=REMOTE_UNIT,STATUS='UNKNOWN',IOSTAT=IER,RECL=256,
     &		FILE=TEMP_USER(2:LEN(TEMP_USER))//'::"TASK=BULLETIN1"')

	      IF (IER.NE.0) THEN
		 CALL ERRSNS(IDUMMY,IDUMMY,INODE)
	         IF (INODE.EQ.%LOC(SS$_NOSUCHNODE).OR.
     &		     INODE.EQ.%LOC(SS$_NOSUCHOBJ).OR.INODE.EQ.0) THEN
		    DELETE (4)
		 END IF
	      ELSE
		 WRITE (REMOTE_UNIT,'(3A)',IOSTAT=IER)
     &			16,REGNODE,NODENAME
	      END IF
	      CLOSE (UNIT=REMOTE_UNIT)
	   END DO
	END IF

	RETURN
	END





	SUBROUTINE UPDATE_SHUTDOWN(FOLDER_NUMBER)

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLUSER.INC'

	INTEGER SHUTDOWN_BTIM(FLONG)

	EQUIVALENCE (SHUTDOWN_BTIM,BRIEF_FLAG)

	COMMON /SYSTEM_FOLDERS/ SYSTEM_FLAG(FLONG),NODENAME
	CHARACTER NODENAME*8

	COMMON /SHUTDOWN/ NODE_NUMBER,NODE_AREA
	COMMON /SHUTDOWN/ SHUTDOWN_FLAG(FLONG)

	CALL OPEN_BULLUSER

	DO WHILE (REC_LOCK(IER))
	   READ (4,KEY='*SYSTEM',IOSTAT=IER) 
     &		TEMP_USER,NODENAME,NODE_NUMBER,NODE_AREA,NEW_FLAG,
     &		SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG
	END DO

	CALL CLR2(SHUTDOWN_FLAG,FOLDER_NUMBER)

	SEEN_FLAG = 0
	DO I=1,FLONG
	   IF (SHUTDOWN_FLAG(I).NE.0) SEEN_FLAG = 1
	END DO
	IF (SEEN_FLAG.EQ.0) NODE_AREA = 0	! All done with that node

	IF (IER.NE.0) THEN
	   WRITE (4,IOSTAT=IER)
     &		'*SYSTEM     ',NODENAME,NODE_NUMBER,NODE_AREA,NEW_FLAG,
     &		SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG
	ELSE
	   REWRITE (4,IOSTAT=IER)
     &		TEMP_USER,NODENAME,NODE_NUMBER,NODE_AREA,NEW_FLAG,
     &		SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG
	END IF

	CALL CLOSE_BULLUSER

	RETURN
	END





	SUBROUTINE HIBER(MIN)
C
C SUBROUTINE HIBER
C
C FUNCTION: Waits for specified time period in minutes.
C
	IMPLICIT INTEGER (A-Z)
	INTEGER TIMADR(2)			! Buffer containing time
						! in desired system format.
	CHARACTER MIN*(*)

	IER=SYS$BINTIM('0 00:'//MIN//':00.00',TIMADR)
	IER=SYS$SCHDWK(,,TIMADR,)		! Set timer.
	IER=SYS$HIBER()

	RETURN
	END



	SUBROUTINE WAIT_SEC(PARAM)
C
C SUBROUTINE WAIT_SEC
C
C FUNCTION: Waits for specified time period in seconds.
C
	IMPLICIT INTEGER (A-Z)
	INTEGER TIMADR(2)			! Buffer containing time
						! in desired system format.
	CHARACTER PARAM*(*)
	DATA WAIT_EF /0/

	IF (WAIT_EF.EQ.0) CALL LIB$GET_EF(WAIT_EF)

	IER=SYS$BINTIM('0 00:00:'//PARAM//'.00',TIMADR)
	IER=SYS$SETIMR(%VAL(WAIT_EF),TIMADR,,%VAL(3))	! Set timer.
	IER=SYS$WAITFR(%VAL(WAIT_EF))		! Wait for EFN to be set.

	RETURN
	END



	SUBROUTINE DELETE_EXPIRED_NEWS(NOW)
C
C  SUBROUTINE DELETE_EXPIRED_NEWS
C
C  FUNCTION:
C
C  Delete any expired message in local news folders.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

 	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFILES.INC'

	COMMON /NEXT/ NEXT

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

        COMMON /BULLFIL/ BULLFIL

        COMMON /NEWSDIR_FILE/ BULLNEWSDIR_FILE
        CHARACTER*80 BULLNEWSDIR_FILE

	COMMON /NEWSLIST/ NEWSLIST

	COMMON /DIRLIST/ DIRLIST

	CHARACTER*4 GET_VMS_VERSION

	INTEGER TODAY(2),NEXT_EX_BTIM(2),NO_EXPIRE(2)

	CHARACTER*8 TODAY_KEY,TEMP
	CHARACTER ASCTIME*24

	IF (.NOT.SYS_TRNLNM('BULL_NEWS_SERVER','DEFINED')) RETURN

	IF (NOW) THEN 
       	   IER = SYS$SETPRN('BULL NEWS1')
	   IF (.NOT.IER) CALL EXIT
	   IER = SYS$SETPRN('BULL NEWS')
	   IF (.NOT.IER) CALL EXIT
	END IF

	FOLDER_NUMBER = 1000

	FOLDER_FILE = NEWS_DIRECTORY(:TRIM(NEWS_DIRECTORY)-1)//'.]'

	CALL OPEN_BULLNEWS_SHARED

	DO WHILE (REC_LOCK(IER))
	   READ (7,IOSTAT=IER,KEYEQ=1000,KEYID=1)
	END DO

	IF (IER.NE.0) THEN
	   CALL CLOSE_BULLNEWS
	   RETURN
	END IF

	CALL SYS_BINTIM('-',TODAY)
	
	CALL GET_MSGKEY(TODAY,TODAY_KEY)

	REMOTE_SET = 4
	
C
C	A bug keeps messing up the last expired date key so that stored
C	news groups do not get found.  Someday when this is fixed, we can
C	add code like this:
C
C	INPUT = GET_VMS_VERSION()
C	IF (INPUT(:2).EQ.'V5'.OR.INPUT(:2).EQ.'V4') VMSOLD = .TRUE..AND.NOW

	DO WHILE (IER.EQ.0)
	   CALL SYS_BINTIM('5-NOV-2956 00:00:00.00',NEXT_EX_BTIM)

	   DO WHILE (REC_LOCK(IER).OR.(NOW.AND.IER.EQ.0.AND.
     &		(.NOT.BTEST(NEWS_F_FLAG,8).OR.
     &		 COMPARE_BTIM(TODAY,NEWEST_EXBTIM).LT.0)))
	      IF (NOW) THEN
	         READ (7,IOSTAT=IER) NEWS_FOLDER_COM
	         CALL GET_MSGKEY(%REF(NEWS_F_EXPIRED_DATE),
     &				 %DESCR(NEWEST_EXBTIM))
	      ELSE
	         READ (7,IOSTAT=IER,KEYLE=TODAY_KEY,KEYID=3) NEWS_FOLDER_COM
	      END IF
	   END DO
	   CALL NEWS_TO_FOLDER

	   UNLOCK 7

	   IF (IER.NE.0) THEN
	      CALL CLOSE_BULLNEWS
	      GO TO 1000
           END IF

	   IF (.NOT.BTEST(FOLDER_FLAG,8)) GO TO 900

	   CALL OPEN_BULLDIR_SHARED

	   CALL GET_MSGKEY(%REF(NEWS_F_EXPIRED_DATE),%DESCR(NEWEST_EXBTIM))

	   DEL_COUNT = 0
	   NDEL = -1
	   DS = .FALSE.
	   DN = .FALSE.
	   CALL READ_FIRST_EXPIRED(NDEL)
	   DO WHILE (NDEL.GT.0)
	      DIFF = COMPARE_BTIM(TODAY,EX_BTIM)
	      IF (DIFF.GT.0) THEN
	         IF (NDEL.EQ.F_START) DS = .TRUE.
	         IF (NDEL.EQ.F_NBULL) DN = .TRUE.	
                 IF (NDEL.GT.NEWS_F_END) THEN
	            CALL READ_NEXT_EXPIRED(NDEL)
		 ELSE IF (COMPARE_BTIM(EX_BTIM,NEWEST_EXBTIM).GE.0) THEN
      		    DEL_COUNT = DEL_COUNT + 1
	            CALL READ_NEXT_EXPIRED(NDEL)
	         ELSE IF (EXDATE(8:11).LT.'1995') THEN	! Deleted manually?
	            IF (LENGTH.GT.0) CALL DUMP_MESSAGE()
		    CALL SYS$ASCTIM(,ASCTIME,TODAY,)
	            EXDATE = ASCTIME(:11) 
		    EXTIME = ASCTIME(13:23)
		    LENGTH = 0
	      	    CALL WRITEDIR(MSG_NUM,IER)
		    DEL_COUNT = 0
	            CALL READ_FIRST_EXPIRED(NDEL)
		 ELSE
                    CALL READ_NEXT_EXPIRED(NDEL) 
	         END IF
	      ELSE
		 CALL COPY2(NEXT_EX_BTIM,EX_BTIM)
	         IF (F_COUNT.LE.DEL_COUNT.OR.NDEL.GT.NEWS_F_END) THEN
		   CALL SYS_BINTIM('5-NOV-2956 00:00:00.00',NEXT_EX_BTIM)
		 END IF
		 NDEL = 0
                 UNLOCK 2
 	      END IF
	   END DO
	   CALL READ_FOLDER_FILE_KEYNAME(NEWS_FOLDER,IER)
	   NEXT = .FALSE.
	   CALL READDIR(F_START,IER)
	   IF (DS.OR.F_START.EQ.IER) THEN
	      IER = 0
	      NEXT = .TRUE.
              I = F_START
              DO WHILE (F_NBULL.GE.I.AND.IER.EQ.0)
		 I = I + 1
		 IF (I.LE.F_NBULL) CALL READDIR(I,IER)
		 IF (COMPARE_BTIM(EX_BTIM,NEXT_EX_BTIM).LT.0) IER = 0
	      END DO
              F_START = I
	      NEXT = .FALSE.
	   END IF
	   CALL READDIR(F_NBULL,IER)
	   IF (F_START.LT.F_NBULL.AND.(DN.OR.F_NBULL.EQ.IER)) THEN
	      I = F_NBULL
	      IER = I
	      DO WHILE (I.GE.F_START.AND.IER.EQ.I)
		 I = I - 1
		 IF (I.GE.F_START) CALL READDIR(I,IER)
		 IF (COMPARE_BTIM(EX_BTIM,NEXT_EX_BTIM).LT.0) IER = I
	      END DO
	      IF (I.GE.F_START) CALL COPY2(F_NEWEST_BTIM,MSG_BTIM)
	      IF (I.NE.IER) F_NBULL = I
	   END IF
	   F_COUNT = MAX(0,F_COUNT - DEL_COUNT)
	   CALL GET_MSGKEY(TODAY,NEWS_F_EXPIRED_DATE)
	   FOLDER_FLAG = IBSET(FOLDER_FLAG,13)
	   CALL REWRITE_FOLDER_FILE(IER)
	   IF (IER.EQ.0) THEN
    	      CALL READ_FIRST_EXPIRED(NDEL)
	      DO WHILE (NDEL.GT.0.AND.NDEL.LE.NEWS_F_END.AND.
     &		COMPARE_BTIM(EX_BTIM,NEXT_EX_BTIM).LT.0)
	         IF (LENGTH.GT.0) CALL DUMP_MESSAGE()
	         DELETE (UNIT=2)
    	         CALL READ_FIRST_EXPIRED(NDEL)
	      END DO
	      CALL READ_FOLDER_FILE_KEYNAME(NEWS_FOLDER,IER)
	      FOLDER_FLAG = IBCLR(FOLDER_FLAG,13)
	      CALL GET_MSGKEY(NEXT_EX_BTIM,NEWS_F_EXPIRED_DATE)
	      CALL REWRITE_FOLDER_FILE(IER)
	   END IF
           CALL CLOSE_BULLDIR
	END DO

900     CALL CLOSE_BULLNEWS

1000	IF (NOW.OR.IER.EQ.0) THEN
	   BULLNEWSDIR_FILE = FOLDER_FILE(:MINGT0(INDEX(FOLDER_FILE,'.]'),
     &		INDEX(FOLDER_FILE,'.BULLNEWS')))//'BULLNEWSDIR.*]'//'*.'
	   INPUT = GET_VMS_VERSION()
	   CALL LIB$DAY_OF_WEEK(TODAY,DAY)
	   IF (DAY.NE.7) THEN
	      IER = SYS_TRNLNM('BULL_NEWS_CLEANUP','DEFINED')
	      IF (IER) THEN
		 DAY = 7
	         CALL DELLNM('BULL_NEWS_CLEANUP')
	      END IF
	   END IF
	   IF (INPUT(:2).NE.'V5'.AND.INPUT(:2).NE.'V4') THEN
	      CONTEXT = 0
	      DO WHILE (LIB$FIND_FILE(BULLNEWSDIR_FILE,INPUT,CONTEXT))
	         IER = CONV$RECLAIM(INPUT(:TRIM(INPUT)))
	      END DO 
	   ELSE IF (DAY.EQ.7) THEN
	      REMOTE_SET = 4
	      DIRLIST = .TRUE.
	      NEWSLIST = .TRUE.
	      CALL OPEN_BULLNEWS_SHARED
	      CALL READ_FOLDER_FILE_KEYNUM(1000,IER)
	      CALL READ_FOLDER_FILE(IER)
	      CALL SYS_BINTIM('5-NOV-2956 00:00:00.00',NO_EXPIRE)
	      DO WHILE (IER.EQ.0)
	         UNLOCK 7
		 CALL GET_MSGKEY(%REF(NEWS_F_EXPIRED_DATE),
     &				 %DESCR(NEWEST_EXBTIM))
		 IF (BTEST(FOLDER_FLAG,8)) THEN
		    CALL SET_BULLNEWSDIR_FILE(FOLDER_NUMBER)
		    C = 0
	            IF (LIB$FIND_FILE(BULLNEWSDIR_FILE
     &			(:TRIM(BULLNEWSDIR_FILE))//';1',INPUT,C)) THEN
                       IER = LIB$DELETE_FILE(
     &			BULLNEWSDIR_FILE(:TRIM(BULLNEWSDIR_FILE))//';2')
		    ELSE
		       IER = LIB$RENAME_FILE(
     &			BULLNEWSDIR_FILE(:TRIM(BULLNEWSDIR_FILE)),'*.*;1')
		    END IF
		    CALL OPEN_BULLDIR_SHARED
		    OPEN (UNIT=9,FILE=BULLNEWSDIR_FILE,SHARED,
     &	             BUFFERCOUNT=127,
     &		     INITIALSIZE=(NEWSDIR_RECORD_LENGTH*F_COUNT)/512,
     &	             STATUS='NEW',FORM='UNFORMATTED',DISPOSE='DELETE',
     &	             RECORDSIZE=NEWSDIR_RECORD_LENGTH/4,IOSTAT=IER,
     &	             ORGANIZATION='INDEXED',RECORDTYPE='FIXED',
     &	             KEY=(1:4:INTEGER,5:12:CHARACTER,13:20:CHARACTER,
     &	             57:64:CHARACTER),ACCESS='KEYED')
		    IF (IER.NE.0) THEN
		       CALL CLOSE_BULLDIR
		       CALL CLOSE_BULLNEWS
		       RETURN
		    END IF
		    DO WHILE (IER.EQ.0)
		       DO WHILE (REC_LOCK(IER))
		          READ (2,IOSTAT=IER) NEWSDIR_ENTRY
		       END DO
		       IF (IER.EQ.0) THEN
			  WRITE (9,IOSTAT=IER) NEWSDIR_ENTRY
			  IF (IER.NE.0) THEN
			     CALL CLOSE_BULLDIR
			     CALL CLOSE_BULLNEWS
		             RETURN
			  ELSE
	     		     CALL GET_MSGKEY(%REF(NEWS_EX_BTIM_KEY),
     &					     %DESCR(EX_BTIM))
			     IF (COMPARE_BTIM(EX_BTIM,NEWEST_EXBTIM).LT.0)
     &			        CALL COPY2(NEWEST_EXBTIM,EX_BTIM)
		          END IF
		       END IF
		    END DO
		    CLOSE (UNIT=9,DISPOSE='KEEP')
		    CALL CLOSE_BULLDIR_DELETE
		    CALL GET_MSGKEY(NEWEST_EXBTIM,TEMP)
		    IF (TEMP.NE.NEWS_F_EXPIRED_DATE) THEN 
		       CALL READ_FOLDER_FILE_KEYNUM(FOLDER_NUMBER,IER)
		       NEWS_F_EXPIRED_DATE = TEMP
	               CALL REWRITE_FOLDER_FILE(IER)
		       CALL READ_FOLDER_FILE_KEYNUM(FOLDER_NUMBER,IER)
		    END IF
		    IER = LIB$RENAME_FILE(
     &			BULLNEWSDIR_FILE(:TRIM(BULLNEWSDIR_FILE)),'*.*;1')
		 ELSE
		    IF (NO_EXPIRE(1).NE.NEWEST_EXBTIM(1).OR.
     & 			NO_EXPIRE(2).NE.NEWEST_EXBTIM(2)) THEN
		       CALL GET_MSGKEY(NO_EXPIRE,NEWS_F_EXPIRED_DATE)
	               CALL REWRITE_FOLDER_FILE(IER)
		       CALL OPEN_BULLDIR
		       CALL CLOSE_BULLDIR_DELETE
		    ELSE 
		       CALL SET_BULLNEWSDIR_FILE(FOLDER_NUMBER)
		    END IF
		    IER = 1
		    DO WHILE (IER)
                      IER = LIB$DELETE_FILE(
     &			 BULLNEWSDIR_FILE(:TRIM(BULLNEWSDIR_FILE))//';')
	            END DO
		 END IF
	         CALL READ_FOLDER_FILE(IER)
	      END DO
	      CALL CLOSE_BULLNEWS
	   END IF
	   DIRLIST = .FALSE.
	   NEWSLIST = .TRUE.
	   CALL COPY2(EX_BTIM,TODAY)
           BULLFIL = 0
	   IER = .TRUE.
	   DO WHILE (IER)
	      IER = SYS_BINTIM('1 00:00',DAY)
	      IER = LIB$ADDX(EX_BTIM,DAY,EX_BTIM)
	      CALL SET_BULLFIL
	      IER = LIB$DELETE_FILE(FOLDER_FILE(:TRIM(FOLDER_FILE))//
     &			'.BULLFIL;')
	      IER = INDEX(FOLDER_FILE,']1JAN').EQ.0
	   END DO
	   J = INDEX(FOLDER_FILE,']')
	   DECODE(2,'(I2)',FOLDER_FILE(J-2:J-1),IOSTAT=IER) YEAR
	   IF (IER.EQ.0) THEN 
	      DO I=1,10
	         YEAR = YEAR - 1
	         IF (YEAR.EQ.-1) YEAR = 99
	         ENCODE(2,'(I2)',FOLDER_FILE(J-2:J-1),IOSTAT=IER) YEAR
                 IF (IER.EQ.0) IER = LIB$DELETE_FILE(FOLDER_FILE(:J)
     &                  //'*.*;*')
	      END DO
	   END IF
           IER = LIB$DELETE_FILE(NEWS_DIRECTORY(:TRIM(NEWS_DIRECTORY))
     &                  //'BULLNEWS*.DIR;*')
	END IF

	RETURN
	END



	SUBROUTINE DELETE_EXPIRED
C
C  SUBROUTINE DELETE_EXPIRED
C
C  FUNCTION:
C
C  Delete any expired bulletins (normal or shutdown ones).
C  (NOTE: If bulletin files don't exist, they get created now by
C  OPEN_FILE_SHARED.  Also, if new format has been defined for files,
C  they get converted now.  The directory file has had it's record size
C  lengthened in the past to include more info, and the bulletin file 
C  was lengthened from 80 to 81 characters to include byte which indicated
C  start of bulletin message.  However, that scheme was removed and
C  was replaced with a 128 byte record compressed format).
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLUSER.INC'

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'

	COMMON /SHUTDOWN/ NODE_NUMBER,NODE_AREA
	COMMON /SHUTDOWN/ SHUTDOWN_FLAG(FLONG)

	CALL OPEN_BULLDIR_SHARED	! Open directory file
	CALL OPEN_BULLFIL_SHARED	! Open bulletin file
	CALL CLOSE_BULLFIL
	CALL READDIR(0,IER)		! Get directory header
	IF (IER.EQ.1) THEN		! Is header present?
	   IER = COMPARE_DATE(NEWEST_EXDATE,' ') ! Yes. Any expired bulls?
	   IF (IER.GT.20*356) IER = -1	! Check if latest expiration date valid.
	   IF (IER.EQ.0) IER = COMPARE_TIME(NEWEST_EXTIME,' ')
	   IF (SHUTDOWN.GT.0.AND.NODE_AREA.GT.0.AND.
     &		(FOLDER_NUMBER.EQ.0.OR.BTEST(FOLDER_FLAG,2)).AND.
     &		TEST2(SHUTDOWN_FLAG,FOLDER_NUMBER)) THEN
			! Do shutdown messages exist and need to be checked?
	      SHUTDOWN = 0
	      IER1 = -1
	   ELSE
	      IF (TEST2(SHUTDOWN_FLAG,FOLDER_NUMBER)) THEN
		 CALL UPDATE_SHUTDOWN(FOLDER_NUMBER)
	      END IF
	      IER1 = 1
	   END IF
	   IF (IER.LE.0.OR.IER1.LE.0) THEN
	      CALL CLOSE_BULLDIR
	      CALL OPEN_BULLDIR		! Reopen without sharing
	      CALL UPDATE 		! Need to update
	   END IF
	ELSE		! If header not there, then first time running BULLETIN
	   IF (FOLDER_NUMBER.EQ.0) THEN
	      CALL OPEN_BULLUSER	! Create user file to be able to set
	      CALL CLOSE_BULLUSER	! defaults, privileges, etc.
	   END IF
           IF (TEST2(SHUTDOWN_FLAG,FOLDER_NUMBER)) THEN
              CALL UPDATE_SHUTDOWN(FOLDER_NUMBER)
           END IF
	END IF
	CALL CLOSE_BULLDIR

	RETURN
	END




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

	COMMON /KNOWN_FOLDERS/ FOLDER_Q1,NUM_FOLDERS
	DATA FOLDER_Q1/0/

	COMMON /BBOARD_LOOP/ BBOARD_LOOP

	COMMON /MAIL_INFO/ USE_INFROM

	COMMON /HEADER_QUEUE/ HEADER_Q,HEADER_Q1,NHEAD

	CHARACTER*12 INEXDATE
	CHARACTER INDESCRIP*(INPUT_LENGTH),INFROM*(INPUT_LENGTH),INTO*76
	CHARACTER ACCOUNT_SAVE*8,USERNAME_SAVE*12
	CHARACTER F_BBOARD*64,BBOARD_NAME*64

	DIMENSION NEW_MAIL(FOLDER_MAX)

	DATA SPAWN_EF/0/

	CALL SYS$SETAST(%VAL(0))

	IF (SPAWN_EF.EQ.0) CALL LIB$GET_EF(SPAWN_EF)

	CALL DISABLE_CTRL

	CALL INIT_QUEUE(FOLDER_Q1,FOLDER_COM)

	FOLDER_Q = FOLDER_Q1

	CALL OPEN_BULLFOLDER_SHARED		! Get folder file

	NUM_FOLDERS = 0
	IER = 0
	DO WHILE (IER.EQ.0)			! Copy all bulletins from file
	   CALL READ_FOLDER_FILE(IER)
	   IF (IER.EQ.0) THEN
	      NUM_FOLDERS = NUM_FOLDERS + 1
	      CALL WRITE_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)
	   END IF
	END DO

	CALL CLOSE_BULLFOLDER			! We don't need file anymore
	CALL SYS$SETAST(%VAL(1))

	IF (TEST_BULLCP().EQ.2.AND.BBOARD_LOOP.NE.0) GO TO 900

	CALL SYS$SETAST(%VAL(0))
	CALL CHECK_MAIL(NEW_MAIL)
	CALL SYS$SETAST(%VAL(1))

	FOLDER_Q = FOLDER_Q1			! Init queue pointer to header

	NBBOARD_FOLDERS = 0

	POINT_FOLDER = 0

1	POINT_FOLDER = POINT_FOLDER + 1
	IF (POINT_FOLDER.GT.NUM_FOLDERS) GO TO 900

	CALL SYS$SETAST(%VAL(0))

	FOLDER_Q_SAVE = FOLDER_Q

	CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)

	IF (FOLDER_BBOARD(:4).EQ.'NONE'.OR.
     &		FOLDER_BBOARD(:2).EQ.'::') GO TO 1

	NBBOARD_FOLDERS = NBBOARD_FOLDERS + 1

	IF (.NOT.NEW_MAIL(POINT_FOLDER)) GO TO 1
C
C  The process is set to the BBOARD uic and username in order to create
C  a spawned process that is able to read the BBOARD mail (a real kludge).
C

	CALL GETUSER(USERNAME_SAVE)		! Get present username
	CALL GETACC(ACCOUNT_SAVE)		! Get present account
	CALL GETUIC(GROUP_SAVE,USER_SAVE)	! Get present uic

	IF (TRIM(FOLDER_BBOARD).GT.0) THEN	! BBOARD name present?
	   IER = SETUSER(FOLDER_BBOARD,USERNAME_SAVE)! Set to BBOARD username
	   IF (IER.EQ.2) GO TO 910	! Can't set username. New VMS version?
	   CALL SETACC(ACCOUNTB)	! Set to BBOARD account
	   CALL SETUIC(IBCLR(GROUPB,31),IBCLR(USERB,31)) ! Set to BBOARD uic
	END IF

	LEN_B = TRIM(BBOARD_DIRECTORY)
	IER = LIB$DELETE_FILE(BBOARD_DIRECTORY(:LEN_B)//
     &		FOLDER_BBOARD(:TRIM(FOLDER_BBOARD))//'.TXT;*')
				! Delete old TXT files left due to errors

	IF (.NOT.BTEST(USERB,31).AND.(USERB.NE.0.OR.GROUPB.NE.0)) THEN
	      						! If normal BBOARD user
	   IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(:LEN_B)
     &		  //'READ_BOARD.COM','NL:','NL:',1,,,STATUS,SPAWN_EF)
	   CALL SYS$SETAST(%VAL(1))
	   IF (IER) CALL SYS$WAITFR(%VAL(SPAWN_EF))
	   CALL SYS$SETAST(%VAL(0))
	   IF (((STATUS.AND.'1FFFF'X).EQ.RMS$_FNF) .OR.
     &	    ((STATUS .AND. '1FFF0'X).EQ. (RMS$_SPL .AND. '1FFF0'X))) THEN
	      CALL SYS$SETDFPROT('AA00'X,CUR_DEF_PROT)
	      	! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD:RW,GROUP:RW)
	      OPEN(UNIT=11,FILE=BBOARD_DIRECTORY(:LEN_B)//'READ_BOARD.COM',
     &		   STATUS='NEW',ERR=910,CARRIAGECONTROL='LIST')
	      WRITE(11,'(A)') '$ SET PROTECT=(W:RWED)/DEFAULT'
	      WRITE(11,'(A)') '$ SET PROC/PRIV=SYSPRV'
	      WRITE(11,'(A)')
     &	       '$ DEFINE/USER EXTRACT_FILE '//BBOARD_DIRECTORY(:LEN_B)//
     &	       '''F$GETJPI("","USERNAME")'''
	      WRITE(11,'(A)') '$ MAIL'
	      WRITE(11,'(A)') 'SELECT MAIL'
	      WRITE(11,'(A)') 'READ'
	      WRITE(11,'(A)') 'EXTRACT/ALL/APPEND EXTRACT_FILE'
	      WRITE(11,'(A)') 'DELETE/ALL'
	      WRITE(11,'(A)') 'READ/NEW'
	      WRITE(11,'(A)') 'EXTRACT/ALL/APPEND EXTRACT_FILE'
	      WRITE(11,'(A)') 'DELETE/ALL'
	      WRITE(11,'(A)') 'SELECT/NEW'
	      CLOSE(UNIT=11)
	      CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection
	      IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(:LEN_B)
     &		   //'READ_BOARD.COM','NL:','NL:',1,,,STATUS,SPAWN_EF)
	      CALL SYS$SETAST(%VAL(1))
	      IF (IER) CALL SYS$WAITFR(%VAL(SPAWN_EF))
	      CALL SYS$SETAST(%VAL(0))
	   END IF
	ELSE
	   CONTEXT = 0
	   IER = LIB$FIND_FILE(BBOARD_DIRECTORY(:LEN_B)//FOLDER_BBOARD
     &	      (:TRIM(FOLDER_BBOARD))//'.COM',INPUT,CONTEXT)
	   IF (IER) THEN
	      IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(:LEN_B)//
     &		  FOLDER_BBOARD(:TRIM(FOLDER_BBOARD))//'.COM','NL:',
     &		  'NL:',1,,,STATUS,SPAWN_EF)
	      CALL SYS$SETAST(%VAL(1))
	      IF (IER) CALL SYS$WAITFR(%VAL(SPAWN_EF))
	      CALL SYS$SETAST(%VAL(0))
	   END IF
	   IF (.NOT.IER.OR.((STATUS.AND.'1FFFF'X).EQ.RMS$_FNF) .OR.
     &	    ((STATUS .AND. '1FFF0'X).EQ. (RMS$_SPL .AND. '1FFF0'X))) THEN
	      IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(:LEN_B)//
     &		'BOARD_SPECIAL.COM','NL:','NL:',1,,,STATUS,SPAWN_EF)
	      CALL SYS$SETAST(%VAL(1))
	      IF (IER) CALL SYS$WAITFR(%VAL(SPAWN_EF))
	      CALL SYS$SETAST(%VAL(0))
	   END IF
	END IF

	CALL READ_QUEUE(%VAL(FOLDER_Q_SAVE),FOLDER_Q,FOLDER_COM)

	NBULL = F_NBULL

	CALL SETACC(ACCOUNT_SAVE)		! Reset to original account
	CALL SETUSER(USERNAME_SAVE)		! Reset to original username
	CALL SETUIC(GROUP_SAVE,USER_SAVE)	! Reset to original uic

	OPEN (UNIT=14,FILE=BBOARD_DIRECTORY(:LEN_B)//FOLDER_BBOARD
     &	   (:TRIM(FOLDER_BBOARD))//'.TXT',STATUS='OLD',ERR=100)
	READ (14,'(Q,A)',END=100) LEN_INPUT,INPUT ! Read first line
	CALL SYS$SETAST(%VAL(1))

5	CALL SYS$SETAST(%VAL(0))

	CALL READ_QUEUE(%VAL(FOLDER_Q_SAVE),IDUMMY,FOLDER_COM)

	DO WHILE (LEN_INPUT.GT.0)
	   IF (INPUT(:5).EQ.'From:') THEN
	      INFROM = INPUT(7:)		! Store username
	   ELSE IF (INPUT(:5).EQ.'Subj:') THEN
	      INDESCRIP = INPUT(7:)		! Store subject
	   ELSE IF (INPUT(:3).EQ.'To:') THEN
	      INTO = INPUT(5:)			! Store address
	   END IF
	   READ (14,'(Q,A)',END=100) LEN_INPUT,INPUT ! Read next line from mail
	END DO

	INTO = INTO(:TRIM(INTO))
	CALL STR$TRIM(INTO,INTO)
	CALL STR$UPCASE(INTO,INTO)
	FLEN = TRIM(FOLDER_BBOARD)

	IER = 0
	CALL STRIP_HEADER(' ',-1,STRIP)
	STRIP = .TRUE.
	DO WHILE (IER.EQ.0.AND.STRIP)
	   READ (14,'(Q,A)',IOSTAT=IER) LEN_INPUT,INPUT
	   IF (IER.EQ.0) THEN
	      CALL STRIP_HEADER(INPUT,LEN_INPUT,STRIP)
	      IF (STRIP.AND.INPUT(:5).EQ.'From:') INFROM = ' '
	   END IF
	END DO

C
C  If more than one folder has same BBOARD account, don't use the 
C  To: line to determine which folder to put the mail message in.
C
	POINT_FOLDER1 = 0
	FOLDER_Q2 = FOLDER_Q1
	DUP = .FALSE.
	DO WHILE (.NOT.DUP.AND.POINT_FOLDER1.LT.NUM_FOLDERS)
	   CALL READ_QUEUE(%VAL(FOLDER_Q2),FOLDER_Q2,FOLDER1_COM)
	   POINT_FOLDER1 = POINT_FOLDER1 + 1
	   DUP = FOLDER.NE.FOLDER1.AND.FOLDER_BBOARD.EQ.FOLDER1_BBOARD
	END DO
	IF (DUP.OR..NOT.DETECT_BBOARD(INTO,FOLDER_BBOARD(:FLEN))) THEN
	   FOUND = .FALSE.
	   J = 0
	   IF (DUP) J = 1
	   DO WHILE (J.LT.2.AND..NOT.FOUND)
	      J = J + 1
	      POINT_FOLDER1 = 0
	      FOLDER_Q2 = FOLDER_Q1
	      FOUND = .FALSE.
	      DO WHILE (.NOT.FOUND.AND.POINT_FOLDER1.LT.NUM_FOLDERS)
		 CALL READ_QUEUE(%VAL(FOLDER_Q2),FOLDER_Q2,FOLDER1_COM)
		 POINT_FOLDER1 = POINT_FOLDER1 + 1
		 IF (POINT_FOLDER1.LE.NUM_FOLDERS.AND.
     &		     FOLDER1_BBOARD(:2).NE.'::'.AND.
     &		     FOLDER1_BBOARD(:4).NE.'NONE') THEN
		    IF (J.EQ.1) THEN
		       F_BBOARD = FOLDER1_BBOARD
		       FOUND = INTO.EQ.F_BBOARD
		    ELSE
		       F_BBOARD = BBOARD_NAME(FOLDER1_BBOARD,FOLDER1_DESCRIP)
		       FOUND = DETECT_BBOARD(INTO,F_BBOARD(:FLEN))
		    END IF
		    FLEN = TRIM(F_BBOARD)
		    IF (.NOT.FOUND.AND.NHEAD.GT.1) THEN
		       HEADER_Q = HEADER_Q1
		       I = 1
		       DO WHILE (I.LT.NHEAD.AND..NOT.FOUND)
			  CALL READ_QUEUE(%VAL(HEADER_Q),HEADER_Q,INPUT)
			  FOUND = DETECT_BBOARD(INPUT,F_BBOARD(:FLEN))
			  I = I + 1
		       END DO
		    END IF
		 END IF
	      END DO
	   END DO
	   IF (FOUND) FOLDER_COM = FOLDER1_COM
	END IF

	NUMHEAD = 0
	IF (NHEAD.GT.0) NUMHEAD = NHEAD + 1

	IF (NUMHEAD.GT.0) THEN
	   HEADER_Q = HEADER_Q1
	   CALL READ_QUEUE(%VAL(HEADER_Q),HEADER_Q,INPUT)
	   LEN_INPUT = TRIM(INPUT)
	   NUMHEAD = NUMHEAD - 1
	END IF

	DO WHILE (LEN_INPUT.EQ.1.AND.INPUT(:1).EQ.CHAR(12).AND.IER.EQ.0)
	   READ (14,'(Q,A)',IOSTAT=IER) LEN_INPUT,INPUT
	   IF (INPUT(:5).EQ.'From:') GO TO 5
	END DO		! If line is just form feed, the message is empty
	IF (IER.NE.0) GO TO 100				! If end of file, exit

	EFROM = 2
	I = TRIM(INFROM)
	DO WHILE (EFROM.GT.0.AND.I.GT.0)		! Strip off the date
	   IF (INFROM(I:I).EQ.' ') EFROM = EFROM - 1	! From the "From:" line
	   I = I - 1
	END DO
	IF (I.GT.0) INFROM = INFROM(:I)

	FOLDER_NAME = FOLDER			! For broadcasts

	SAVE_Q = HEADER_Q
	SAVE_Q1 = HEADER_Q1
	NHEAD1 = NHEAD
	HEADER_Q1 = 0
		! INIT_MESSAGE_ADD_BBOARD reinits header so save it
	CALL INIT_MESSAGE_ADD_BBOARD(INFROM,INDESCRIP,IER)

	ISTART = 0
	NBLANK = 0
	IER = 0
	DO WHILE (IER.EQ.0)		! Move text to bulletin file
	   IF (LEN_INPUT.EQ.0) THEN
	      IF (ISTART.EQ.1) THEN
		 NBLANK = NBLANK + 1
	      END IF
	   ELSE
	      ISTART = 1
	      DO I=1,NBLANK
		 CALL WRITE_MESSAGE_LINE(' ')
	      END DO
	      NBLANK = 0
	      CALL WRITE_MESSAGE_LINE(INPUT)
	   END IF
	   IF (NUMHEAD.EQ.0) THEN
	      READ (14,'(Q,A)',IOSTAT=IER) LEN_INPUT,INPUT
	   ELSE IF (NUMHEAD.EQ.1) THEN
	      INPUT = ' '
	      LEN_INPUT = 1
	      NUMHEAD = NUMHEAD - 1
	   ELSE
	      CALL READ_QUEUE(%VAL(SAVE_Q),SAVE_Q,INPUT)
	      LEN_INPUT = TRIM(INPUT)
	      NUMHEAD = NUMHEAD - 1
	   END IF
	   IF (LEN_INPUT.EQ.1.AND.INPUT(:1).EQ.CHAR(12)) THEN
	      DO WHILE (LEN_INPUT.EQ.1.AND.INPUT(:1).EQ.CHAR(12)
     &			.AND.IER.EQ.0)
		 READ (14,'(Q,A)',IOSTAT=IER) LEN_INPUT,INPUT
	      END DO
	      IF (IER.EQ.0.AND.INPUT(:5).EQ.'From:') THEN
		 IER = 1
	      ELSE
		 NBLANK = NBLANK + 1
	      END IF
	   END IF
	END DO

	USE_INFROM = .TRUE.

	NHEAD = NHEAD1
	HEADER_Q1 = SAVE_Q1

	CALL FINISH_MESSAGE_ADD			! Totally finished with add

	CALL SYS$SETAST(%VAL(1))

	GO TO 5					! See if there is more mail

100	CLOSE (UNIT=14,STATUS='DELETE')		! Close the input file
	CALL SYS$SETAST(%VAL(1))
	GO TO 1

900	CALL SYS$SETAST(%VAL(0))

	FOLDER_NUMBER = 0
	CALL OPEN_BULLFOLDER_SHARED
	CALL READ_FOLDER_FILE_KEYNUM(0,IER)
	CALL CLOSE_BULLFOLDER
	CALL ENABLE_CTRL
	FOLDER_SET = .FALSE.

	IF (NBBOARD_FOLDERS.EQ.0) THEN
	   CALL OPEN_BULLUSER
	   CALL READ_USER_FILE_HEADER(IER)
	   CALL SYS_BINTIM('5-NOV-2956 00:00:00.00',BBOARD_BTIM)
	   REWRITE (4) USER_HEADER		! Rewrite header
	   CALL CLOSE_BULLUSER
	END IF
	CALL SYS$SETAST(%VAL(1))

	CALL SYS$SETAST(%VAL(0))
	IF (SYS_TRNLNM('BULL_NEWS_SERVER','DEFINED')) THEN
	   CALL SYS$SETAST(%VAL(1))
	   IF (.NOT.TEST_BULLCP().AND.TEST_BULLCP().NE.2)
     &		CALL NEWS2BULL(.FALSE.)
	END IF
	CALL SYS$SETAST(%VAL(1))

	RETURN

910	WRITE (6,1010)
	GO TO 100

1010	FORMAT(' ERROR:Install program with CMKRNL privileges or relink.')

	END




	LOGICAL FUNCTION DETECT_BBOARD(INPUT,BBOARD)

	IMPLICIT INTEGER (A-Z)

	CHARACTER*(*) INPUT,BBOARD

	DETECT_BBOARD = .TRUE.

	LEN_BBOARD = LEN(BBOARD) - 1
	LEN_INPUT = TRIM(INPUT)

	DO I=1,LEN_INPUT-LEN_BBOARD
	   IF (.NOT.STREQ(INPUT(:4),'Subj').AND.
     &		STREQ(INPUT(I:I+LEN_BBOARD),BBOARD).AND.
     &		(I.EQ.1.OR..NOT.ALPHA(INPUT(I-1:I-1))).AND.
     &		(I.EQ.LEN_INPUT-LEN_BBOARD.OR.
     &		(INDEX('@%!',INPUT(I+LEN_BBOARD+1:I+LEN_BBOARD+1)).GT.0
     &		.AND.(I.EQ.1.OR.(INPUT(I-1:I-1).NE.
     &		INPUT(I+LEN_BBOARD+1:I+LEN_BBOARD+1).AND.
     &		(INPUT(I-1:I-1).NE.'('.OR.
     &		INPUT(I+LEN_BBOARD+1:I+LEN_BBOARD+1).NE.' ')))))) RETURN
	END DO

	DETECT_BBOARD = .FALSE.

	RETURN
	END



	LOGICAL FUNCTION ALPHA(IN)

	CHARACTER*(*) IN

	ALPHA = (ICHAR(IN).GE.ICHAR('A').AND.ICHAR(IN).LE.ICHAR('Z'))
     &	    .OR.(ICHAR(IN).GE.ICHAR('a').AND.ICHAR(IN).LE.ICHAR('z'))

	RETURN
	END



	CHARACTER*(*) FUNCTION BBOARD_NAME(FOLDER_BBOARD,FOLDER_DESCRIP)

	CHARACTER*(*) FOLDER_BBOARD,FOLDER_DESCRIP

	BBOARD_NAME = FOLDER_BBOARD

	I = INDEX(FOLDER_DESCRIP,'<')
	IF (I.EQ.0) RETURN

	BBOARD_NAME = FOLDER_DESCRIP(I+1:)

	I = INDEX(BBOARD_NAME,'%"')
	IF (I.GT.0) BBOARD_NAME = BBOARD_NAME(I+2:)

	I = INDEX(BBOARD_NAME,'!')
	DO WHILE (I.GT.0)
	   BBOARD_NAME = BBOARD_NAME(I+1:)
	   I = INDEX(BBOARD_NAME,'!')
	END DO

	I = INDEX(BBOARD_NAME,'>')
	IF (I.GT.0) BBOARD_NAME = BBOARD_NAME(:I-1)
	I = INDEX(BBOARD_NAME,'@')
	IF (I.GT.0) BBOARD_NAME = BBOARD_NAME(:I-1)
	I = INDEX(BBOARD_NAME,'%')
	IF (I.GT.0) BBOARD_NAME = BBOARD_NAME(:I-1)

	RETURN
	END




	SUBROUTINE CREATE_PROCESS(COMMAND)

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($PRCDEF)'

	INCLUDE 'BULLFILES.INC'

	COMMON /PRIVILEGES/ PROCPRIV(2),NEEDPRIV(2)

	LOGICAL*1 QUOTA(32)

	CHARACTER*132 IMAGENAME

	CHARACTER*(*) COMMAND

	CALL GETIMAGE(IMAGENAME,ILEN)

	LEN_B = TRIM(BBOARD_DIRECTORY)

	IER = 0
	DO WHILE (IER.EQ.0)
	   OPEN(UNIT=11,FILE=BBOARD_DIRECTORY(:LEN_B)//'BULL_COMMAND.COM',
     &		STATUS='OLD',IOSTAT=IER)
	   IF (IER.EQ.0) CLOSE(UNIT=11,STATUS='DELETE')
	END DO

	CALL SYS$SETDFPROT('AA00'X,CUR_DEF_PROT)
		! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD:RW,GROUP:RW)
	OPEN(UNIT=11,FILE=BBOARD_DIRECTORY(:LEN_B)//'BULL_COMMAND.COM',
     &		STATUS='NEW',IOSTAT=IER,CARRIAGECONTROL='LIST')
	IF (IER.NE.0) RETURN
	IF (INDEX(IMAGENAME,';').GT.0) ILEN = INDEX(IMAGENAME,';')
	WRITE(11,'(A)') '$B:=$'//IMAGENAME(:ILEN)
	WRITE(11,'(A)') '$ON ERROR THEN GOTO EXIT'
	WRITE(11,'(A)') '$ON SEVERE THEN GOTO EXIT'
	WRITE(11,'(A)') '$ON WARNING THEN GOTO EXIT'
	WRITE(11,'(A)') '$B/'//'''F$PROCESS()'''
	WRITE(11,'(A)') '$EXIT:'
	WRITE(11,'(A)') '$LOGOUT'
	CLOSE(UNIT=11)
	CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection

	DEL = .FALSE.
	IER = .FALSE.

	CALL GETQUOTA(QUOTA,0)

	DO WHILE (.NOT.IER)
	   IER = SYS$CREPRC(,'SYS$SYSTEM:LOGINOUT',
     &	    BBOARD_DIRECTORY(:LEN_B)//'BULL_COMMAND.COM','NL:',,
     &	    PROCPRIV,QUOTA,COMMAND(:TRIM(COMMAND))
     &	    ,%VAL(4),,,%VAL(PRC$M_NOUAF+PRC$M_DETACH))
	   IF (.NOT.IER.AND..NOT.DEL) THEN
	      CALL DELPRC('BULLCP NEWS',DEL)
	      IER = .NOT.DEL
	   ELSE
	      IER = .TRUE.
	   END IF
	END DO

	RETURN
	END




	SUBROUTINE GETQUOTA(QUOTA,CLI)
C
C  SUBROUTINE GETQUOTA
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE '($PQLDEF)'

	INCLUDE '($JPIDEF)'
                              
	LOGICAL*1 QUOTA(32)

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	CALL INIT_ITMLST	! Initialize item list
				! Now add items to list
	CALL ADD_2_ITMLST(4,JPI$_DFWSCNT,%LOC(WSDEFAULT))
	CALL ADD_2_ITMLST(4,JPI$_WSEXTENT,%LOC(WSEXTENT))
	CALL ADD_2_ITMLST(4,JPI$_WSQUOTA,%LOC(WSQUOTA))
	CALL ADD_2_ITMLST(4,JPI$_BYTLM,%LOC(BYTLM))
	CALL ADD_2_ITMLST(4,JPI$_ENQLM,%LOC(ENQLM))
	CALL END_ITMLST(GETJPI_ITMLST)	! Get address of itemlist

	IER = SYS$GETJPIW(,,,%VAL(GETJPI_ITMLST),,,,)	! Get Info command.

	I = 1
	IF (CLI) THEN
	   IF (CLI$GET_VALUE('PGFLQUOTA',BULL_PARAMETER,LEN_P)) THEN 
	      DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER) PGFLQUOTA
	      QUOTA(1) = PQL$_PGFLQUOTA
	      CALL LIB$MOVC3(4,PGFLQUOTA,QUOTA((I-1)*5+2))
	      I = I + 1
	   END IF
	   IF (CLI$GET_VALUE('WSEXTENT',BULL_PARAMETER,LEN_P)) THEN
	      DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER) WSEXTENT
	   END IF
	END IF
 	QUOTA((I-1)*5+1) = PQL$_WSEXTENT
	CALL LIB$MOVC3(4,WSEXTENT,QUOTA((I-1)*5+2)) 
	I = I + 1
	QUOTA((I-1)*5+1) = PQL$_WSQUOTA
	CALL LIB$MOVC3(4,WSQUOTA,QUOTA((I-1)*5+2))
	I = I + 1
	QUOTA((I-1)*5+1) = PQL$_WSDEFAULT
	CALL LIB$MOVC3(4,WSDEFAULT,QUOTA((I-1)*5+2))
	I = I + 1
	QUOTA((I-1)*5+1) = PQL$_BYTLM
	CALL LIB$MOVC3(4,BYTLM,QUOTA((I-1)*5+2))
	I = I + 1
	QUOTA((I-1)*5+1) = PQL$_ENQLM
	CALL LIB$MOVC3(4,ENQLM,QUOTA((I-1)*5+2))
	I = I + 1
	QUOTA((I-1)*5+1) = PQL$_LISTEND
	CALL LIB$MOVC3(4,0,QUOTA((I-1)*5+2))

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

	INCLUDE '($SYIDEF)'

	INTEGER 	UPTIME(2)
	CHARACTER*(*)	UPTIME_TIME,UPTIME_DATE
	CHARACTER	ASCSINCE*24

	CALL INIT_ITMLST
	CALL ADD_2_ITMLST(8,SYI$_BOOTTIME,%LOC(UPTIME))
	CALL END_ITMLST(GETSYI_ITMLST)

	IER = SYS$GETSYI(,,,%VAL(GETSYI_ITMLST),,,)

	CALL SYS$ASCTIM(,ASCSINCE,UPTIME,)

	UPTIME_DATE = ASCSINCE(:11)
	UPTIME_TIME = ASCSINCE(13:23)

	RETURN	
	END



	CHARACTER*4 FUNCTION GET_VMS_VERSION
C
C  FUNCTION GET_VMS_VERSION
C
C  FUNCTION: Gets VMS version 
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($SYIDEF)'

	CHARACTER VERSION*4

	CALL INIT_ITMLST
	CALL ADD_2_ITMLST(4,SYI$_NODE_SWVERS,%LOC(VERSION))
	CALL END_ITMLST(GETSYI_ITMLST)

	IER = SYS$GETSYI(,,,%VAL(GETSYI_ITMLST),,,)

	GET_VMS_VERSION = VERSION

	RETURN	
	END



	INTEGER FUNCTION GET_L_VAL(I)
	INTEGER I
	GET_L_VAL = I
	RETURN
	END



	SUBROUTINE CHECK_MAIL(NEW_MAIL)

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	COMMON /KNOWN_FOLDERS/ FOLDER_Q1,NUM_FOLDERS
	DATA FOLDER_Q1/0/

	DIMENSION NEW_MAIL(1)

	CHARACTER INPUT*132

	INTEGER*2 COUNT

	FOLDER_Q = FOLDER_Q1			! so reinit queue pointer

	OPEN (UNIT=10,FILE='VMSMAIL_PROFILE',
     &	     DEFAULTFILE='SYS$SYSTEM:VMSMAIL_PROFILE.DATA',
     &       ACCESS='KEYED',FORM='FORMATTED',ORGANIZATION='INDEXED',
     &       STATUS='OLD',READONLY,SHARED,IOSTAT=IER)

	DO I=1,NUM_FOLDERS
	   CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)

	   IF (((.NOT.BTEST(USERB,31).AND.(USERB.NE.0.OR.GROUPB.NE.0)).OR.
     &		 BTEST(GROUPB,31)).AND.FOLDER_BBOARD(:2).NE.'::'.AND.
     &		 FOLDER_BBOARD(:4).NE.'NONE') THEN
						! If normal BBOARD or /VMSMAIL
	      DO WHILE (REC_LOCK(IER1))
	         READ(10,'(A)',KEY=FOLDER_BBOARD,IOSTAT=IER1) INPUT
	      END DO
	      COUNT = 0
	      IF (IER1.EQ.0) THEN
		 INPUT = INPUT(32:)
		 DO WHILE (TRIM(INPUT).GT.0)
		    IF (ICHAR(INPUT(1:1)).EQ.1) THEN
		       CALL LIB$MOVC3(2,%REF(INPUT(5:)),COUNT)
		       INPUT = ' '
		    ELSE
		       INPUT = INPUT(ICHAR(INPUT(3:3))+5:)
	            END IF
	         END DO
	      END IF
	      IF (IER1.EQ.0.AND.(COUNT.GT.0.OR.IER.NE.0)) THEN
		 NEW_MAIL(I) = .TRUE.
	      ELSE
		 NEW_MAIL(I) = .FALSE.
	      END IF
	   ELSE
	      NEW_MAIL(I) = .TRUE.
	   END IF
	END DO

	CLOSE (10)

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




	SUBROUTINE GET_NEWEST_MSG(IN_BTIM,START)

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	DIMENSION IN_BTIM(2)

	IF (REMOTE_SET) THEN
	   CALL REMOTE_GET_NEWEST_MSG(IN_BTIM,START)
	ELSE
	   CALL GET_MSGKEY(IN_BTIM,MSG_KEY)
	   CALL READDIR_KEYGE(START)
	   IF (START.EQ.0) THEN
	      START = -1
	   END IF
	END IF

	RETURN
	END



	SUBROUTINE NOTIFY_REMOTE_USERS(IN_BTIM)

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	DIMENSION IN_BTIM(2)

	CALL GET_MSGKEY(IN_BTIM,MSG_KEY)
	CALL READDIR_KEYGE(START)

	IF (START.EQ.0) RETURN

	CALL OPEN_BULLUSER_SHARED

	IER = START + 1
	DO WHILE (START+1.EQ.IER)
	   IF (.NOT.BTEST(SYSTEM,3)) CALL NOTIFY_USERS(0)
	   START = START + 1
	   CALL READDIR(START,IER)
	END DO

	CALL CLOSE_BULLDIR

	RETURN
	END





	SUBROUTINE READ_NOTIFY

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	COMMON /BULL_NOTIFY/ NOTIFY_REMOTE(FLONG)

	CALL OPEN_BULLUSER_SHARED

	DO WHILE (REC_LOCK(IER))
	   READ (4,KEY='*NOTIFY',IOSTAT=IER) TEMP_USER,NOTIFY_REMOTE
	END DO

	IF (IER.NE.0) THEN
	   DO I=1,FLONG
	      NOTIFY_REMOTE(I) = 0
	   END DO
	   WRITE (4,IOSTAT=IER) '*NOTIFY     ',NOTIFY_REMOTE
	END IF

	CALL CLOSE_BULLDIR

	RETURN
	END



	SUBROUTINE DELPRC(DELNAM,IER)

	IMPLICIT INTEGER (A-Z)

	INCLUDE '($JPIDEF)'

	CHARACTER*(*) DELNAM

	DATA OBIO/0/,OCPU/0/,ODIO/0/

	CHARACTER PRCNAM*16

	TEST = 'BULLCP NEWS'.EQ.DELNAM

	WILDCARD = -1

	CALL INIT_ITMLST	! Initialize item list
				! Now add items to list
	
	CALL ADD_2_ITMLST(LEN(PRCNAM),JPI$_PRCNAM,%LOC(PRCNAM))
	CALL ADD_2_ITMLST(4,JPI$_PID,%LOC(PID))
	IF (TEST) THEN 
           CALL ADD_2_ITMLST(4,JPI$_BUFIO,%LOC(BIO))
           CALL ADD_2_ITMLST(4,JPI$_CPUTIM,%LOC(CPU))
           CALL ADD_2_ITMLST(4,JPI$_DIRIO,%LOC(DIO))
	END IF
 	CALL END_ITMLST(GETJPI_ITMLST)	! Get address of itemlist
	IER = SYS$GETJPIW(,,DELNAM(:LEN(DELNAM)),%VAL(GETJPI_ITMLST),,,,)
	IF (.NOT.IER) THEN 
	   IER = 1
	   DO WHILE (IER.AND.PRCNAM(:LEN(DELNAM)).NE.DELNAM)
						! Get next interactive process
	      IER = SYS$GETJPIW(,WILDCARD,,%VAL(GETJPI_ITMLST),,,,)
						! Get next process.
	   END DO
	END IF
	IF (IER.AND.PID.NE.0) THEN 
	   IF (TEST.AND.
     &		(BIO.GT.OBIO.OR.DIO.GT.ODIO.OR.CPU.GT.OCPU+10)) THEN
	      OBIO = BIO
	      ODIO = DIO
	      OCPU = CPU
	      IER = 0
	      RETURN
	   END IF
	   IER = SYS$DELPRC(PID,)
	   IF (IER.AND.TEST) THEN
              OBIO = 0
              ODIO = 0
              OCPU = 0  
	   END IF
	END IF
	RETURN
	END
