C
C  BULLETIN3.FOR, Version 6/1/89
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

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLUSER.INC'

        INCLUDE 'BULLDIR.INC'

        COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

        COMMON /SHUTDOWN/ NODE_NUMBER,NODE_AREA
        COMMON /SHUTDOWN/ SHUTDOWN_FLAG(FLONG)

        CHARACTER*107 DIRLINE

        CHARACTER*11 TEMP_DATE,TEMP_EXDATE,TEMP_NOSYSDATE
        CHARACTER*11 TEMP_TIME,TEMP_EXTIME,TEMP_NOSYSTIME

        IF (REMOTE_SET.AND.
     &          NODE_AREA.GT.0.AND.BTEST(FOLDER_FLAG,2)) THEN
           CALL UPDATE_SHUTDOWN(FOLDER_NUMBER)
        END IF

        IF (TEST_BULLCP().OR.REMOTE_SET) RETURN
                                        ! BULLCP cleans up expired bulletins

        ENTRY UPDATE_ALWAYS             ! Entry to skip BULLCP test

        TEMP_EXDATE = '5-NOV-2000'  ! If a bulletin gets deleted, and there are
        TEMP_EXTIME = '00:00:00.00' ! are no more bulletins, this is the value
                                    ! assigned to the latest expiration date

        TEMP_DATE = '5-NOV-1956'        ! Storage for computing newest
        TEMP_TIME = '00:00:00.00'       ! bulletin date if deletion occurs

        TEMP_NOSYSDATE = '5-NOV-1956'   ! Storage for computing newest
        TEMP_NOSYSTIME = '00:00:00.00'  ! non-system bulletin date

        BULL_ENTRY = 1                          ! Init bulletin pointer
        UPDATE_DONE = 0                 ! Flag showing bull has been deleted

        NEW_SHUTDOWN = 0
        OLD_SHUTDOWN = SHUTDOWN

        DO WHILE (1)
           CALL READDIR(BULL_ENTRY,IER)         ! Get next directory entry
           IF (IER.EQ.BULL_ENTRY) GO TO 100     ! ERROR: Not found
           IF (SYSTEM.LE.3.OR.(OLD_SHUTDOWN.EQ.0! If not shutdown, or time
     &       .AND.(SYSTEM.AND.4).EQ.4)) THEN    ! to delete shutdowns?
            IF ((SYSTEM.AND.4).EQ.4) THEN       ! Shutdown bulletin?
               IF (NODE_AREA.GT.0) THEN
                  EXTIME(3:4) = EXTIME(4:5)
                  READ (EXTIME(1:4),'(I4)') NODE_NUMBER_MSG
                  EXTIME(9:10) = EXTIME(10:11)
                  READ (EXTIME(7:10),'(I4)') NODE_AREA_MSG
                  IF (NODE_NUMBER_MSG.EQ.NODE_NUMBER.AND.
     &                NODE_AREA_MSG.EQ.NODE_AREA) THEN
                     DIFF = 0
                  ELSE
                     DIFF = 1
                  END IF
               ELSE
                  DIFF = 1
               END IF
               IF (DIFF.EQ.1) NEW_SHUTDOWN = NEW_SHUTDOWN + 1
            ELSE
               DIFF = COMPARE_DATE(EXDATE,' ')  ! Has expiration date passed?
               IF (DIFF.EQ.0) DIFF = COMPARE_TIME(EXTIME,' ')
            END IF
            IF (DIFF.LE.0) THEN                 ! If so then delete bulletin
              CALL DELETE_ENTRY(BULL_ENTRY)     ! Delete bulletin entry
              IF (UPDATE_DONE.EQ.0) THEN        ! If this is first deleted file
                 UPDATE_DONE = BULL_ENTRY       ! store it to use for reordering
              END IF                            ! directory file.
            ELSE IF (SYSTEM.LE.3) THEN          ! Expiration date hasn't passed
                ! If a bulletin is deleted, we'll have to update the latest
                ! expiration date. The following does that.
              DIFF = COMPARE_DATE(EXDATE,TEMP_EXDATE)
              IF (DIFF.LT.0.OR.(DIFF.EQ.0.AND.
     &          COMPARE_TIME(EXTIME,TEMP_EXTIME).LT.0)) THEN
                 TEMP_EXDATE = EXDATE           ! If this is the latest exp
                 TEMP_EXTIME = EXTIME           ! date seen so far, save it.
              END IF
              TEMP_DATE = DATE                  ! Keep date after search
              TEMP_TIME = TIME                  ! we have the last message date
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

100     IF (UPDATE_DONE.GT.0) THEN              ! Reorder directory file
           CALL CLEANUP_DIRFILE(UPDATE_DONE)    ! due to deleted entries
        END IF

        DATE = NEWEST_DATE
        TIME = NEWEST_TIME
        CALL READDIR(0,IER)
        SHUTDOWN = NEW_SHUTDOWN
        NEWEST_EXDATE = TEMP_EXDATE
        DIFF = COMPARE_DATE(NEWEST_EXDATE,' ')
        IF (DIFF.GT.20*356) NEWEST_EXDATE = '5-NOV-2000'
        NEWEST_EXTIME = TEMP_EXTIME
        NEWEST_DATE = TEMP_DATE
        NEWEST_TIME = TEMP_TIME
        CALL WRITEDIR(0,IER)
        SYSTEM = 0                      ! Updating last non-system date/time
        NEWEST_DATE = TEMP_NOSYSDATE
        NEWEST_TIME = TEMP_NOSYSTIME
        CALL UPDATE_FOLDER
        SYSTEM = 1                      ! Now update latest date/time
        NEWEST_DATE = TEMP_DATE
        NEWEST_TIME = TEMP_TIME
        CALL UPDATE_FOLDER

        IF (NODE_AREA.GT.0.AND.BTEST(FOLDER_FLAG,2)) THEN ! Shutdowns deleted?
           CALL UPDATE_SHUTDOWN(FOLDER_NUMBER)            ! Save that info
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



        SUBROUTINE UPDATE_READ
C
C  SUBROUTINE UPDATE_READ
C
C  FUNCTION:
C       Store the latest date that user has used the BULLETIN facility.
C       If new bulletins have been added, alert user of the fact.
C

        IMPLICIT INTEGER (A - Z)

        INCLUDE 'BULLUSER.INC'

        INCLUDE 'BULLDIR.INC'

        INCLUDE '($PRVDEF)'

        CHARACTER TODAY*23

        DIMENSION TODAY_BTIM(2),READ_BTIM_SAVE(2)

        LOGICAL MODIFY_SYSTEM /.TRUE./

C
C  Update user's latest read time in his entry in BULLUSER.DAT.
C

        CALL OPEN_BULLUSER_SHARED               ! Get BULLUSER.DAT file

        CALL READ_USER_FILE_HEADER(IER)

        IF (IER.NE.0) THEN                      ! If header not present, exit
           CALL CLOSE_BULLUSER
           RETURN
        ELSE IF (USERPRIV(1).EQ.-1.AND.USERPRIV(2).EQ.-1) THEN
                                                ! If header present, but no
           DO I=1,FLONG                         ! SET_FLAG and NOTIFY_FLAG
              SET_FLAG_DEF(I) = 0               ! information, write default
              NOTIFY_FLAG_DEF(I) = 0            ! flags.
              BRIEF_FLAG_DEF(I) = 0
           END DO
           SET_FLAG_DEF(1) = 1
           USERPRIV(1) = PRV$M_OPER.OR.PRV$M_CMKRNL.OR.PRV$M_SETPRV
           USERPRIV(2) = 0
           REWRITE (4) USER_HEADER
        END IF

        CALL SYS$ASCTIM(,TODAY,,)               ! Get today's time
        CALL SYS_BINTIM(TODAY,TODAY_BTIM)

        UNLOCK 4

        CALL READ_USER_FILE_KEYNAME(USERNAME,IER1)

        IF (IER1.EQ.0) THEN                     ! If entry found, update it
           READ_BTIM_SAVE(1) = READ_BTIM(1)
           READ_BTIM_SAVE(2) = READ_BTIM(2)
           READ_BTIM(1) = TODAY_BTIM(1)
           READ_BTIM(2) = TODAY_BTIM(2)
           REWRITE (4) USER_ENTRY
           READ_BTIM(1) = READ_BTIM_SAVE(1)
           READ_BTIM(2) = READ_BTIM_SAVE(2)
        ELSE                                    ! If no entry create a new entry
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

        CALL CLOSE_BULLUSER                     ! All finished with BULLUSER

        RETURN                                  ! to go home...

        END




        SUBROUTINE FIND_NEWEST_BULL
C
C  SUBROUTINE FIND_NEWEST_BULL
C
C       If new bulletins have been added, alert user of the fact and
C       set the next bulletin to be read to the first new bulletin.
C
C  OUTPUTS:
C       BULL_POINT  -  If -1, no new bulletins to read, else there are.
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
        BULL_POINT = -1                         ! Init bulletin pointer

        CALL OPEN_BULLDIR_SHARED                ! Yep, so get directory file
        CALL READDIR(0,IER)                     ! Get # bulletins from header
        IF (IER.EQ.1) THEN
           CALL GET_NEWEST_MSG(LAST_READ_BTIM(1,FOLDER_NUMBER+1),START)
           IF (START.LE.0) THEN
              BULL_POINT = START
              CALL CLOSE_BULLDIR
              RETURN
           END IF
           DO WHILE (START.LE.NBULL.AND.(FROM.EQ.USERNAME.OR.SYSTEM))
              IF (FROM.NE.USERNAME) THEN        ! Ignore bull if owner is user
                 IF (SYSTEM) THEN               ! If system bulletin
                    CALL SYS_BINTIM(DATE//' '//TIME,DIR_BTIM)
                    DIFF = COMPARE_BTIM(LOGIN_BTIM,DIR_BTIM)
                    IF (DIFF.GT.0) THEN
                       START = START + 1
                       CALL READDIR(START,IER)
                    ELSE                        ! SYSTEM bulletin was not seen
                       SYSTEM = 0               ! so force exit to read it.
                    END IF
                 END IF
              ELSE
                 START = START + 1
                 CALL READDIR(START,IER)
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

        CHARACTER*23 EXPDAT
        CHARACTER*23 TODAY

        DIMENSION EXTIME(2),NOW(2)

        EXTERNAL CLI$_ABSENT

        IER = SYS$ASCTIM(,TODAY,,)              ! Get today's date

        IERC = CLI$GET_VALUE('EXPIRATION',EXPDAT,ILEN)

        PROMPT = .TRUE.

5       IF (PROMPT) THEN
           IF (IERC.NE.%LOC(CLI$_ABSENT)) THEN  ! Was value specified?
              PROMPT = .FALSE.
           ELSE
              DEFAULT_EXPIRE = FOLDER_BBEXPIRE
              IF ((DEFAULT_EXPIRE.GT.F_EXPIRE_LIMIT.OR.DEFAULT_EXPIRE
     &            .EQ.0).AND.F_EXPIRE_LIMIT.GT.0.AND.
     &           .NOT.SETPRV_PRIV().AND.USERNAME.NE.FOLDER_OWNER) THEN
                 DEFAULT_EXPIRE = F_EXPIRE_LIMIT
              END IF
              IF (BTEST(FOLDER_FLAG,3)) THEN            ! NOPROMPT was set
                 IF (DEFAULT_EXPIRE.LE.0) THEN          ! If no expiration date
                    SYSTEM = SYSTEM.OR.2                ! make permanent
                    EXPDAT = '5-NOV-2000 00:00:00.00'
                 ELSE                                   ! Else set expiration
                    CALL GET_EXDATE(EXPDAT,DEFAULT_EXPIRE)
                    EXPDAT = EXPDAT(:TRIM(EXPDAT))//' 00:00:00.00'
                 END IF
                 ILEN = TRIM(EXPDAT)
              ELSE
                 IF (DEFAULT_EXPIRE.EQ.0) THEN  ! Get expiration date
                    WRITE(6,1030) TODAY(:INDEX(TODAY,'.')-4)
                 ELSE IF (DEFAULT_EXPIRE.EQ.-1) THEN
                    WRITE(6,1031) TODAY(:INDEX(TODAY,'.')-4)
                 ELSE
                    WRITE(6,1032) TODAY(:INDEX(TODAY,'.')-4),
     &                                  DEFAULT_EXPIRE
                 END IF
                 WRITE (6,1035)
                 CALL GET_LINE(EXPDAT,ILEN)     ! Get EXPDAT line
                 IF (ILEN.EQ.0.AND.DEFAULT_EXPIRE.NE.0) THEN
                    IF (DEFAULT_EXPIRE.EQ.-1) THEN
                       EXPDAT = '5-NOV-2000 00:00:00.00'
                       SYSTEM = IBSET(SYSTEM,1) ! Indicate permanent message
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

        EXPDAT = EXPDAT(:ILEN)                  ! Change trailing zeros 2 spaces

        IF (INDEX(EXPDAT,'-').EQ.0.AND.INDEX(EXPDAT,':').GT.0.AND.
     &          INDEX(EXPDAT(:ILEN),' ').EQ.0) THEN     ! Only time specified?
           EXPDAT = TODAY(:INDEX(TODAY(2:),' ')+1)//EXPDAT      ! Add date
        ELSE IF (INDEX(EXPDAT(6:),'-').EQ.0.AND.                ! Date specified
     &                  INDEX(EXPDAT,'-').GT.0) THEN    ! but no year?
           SPACE = INDEX(EXPDAT,' ') - 1                        ! Add year
           IF (SPACE.EQ.-1) SPACE = TRIM(EXPDAT) 
           YEAR = INDEX(TODAY(6:),'-')
           EXPDAT = EXPDAT(:SPACE)//TODAY(5+YEAR:9+YEAR)//EXPDAT(SPACE+1:)
        END IF

        CALL STR$UPCASE(EXPDAT,EXPDAT)          ! Convert to upper case
        IER = SYS_BINTIM(EXPDAT,EXTIME)
        IF (IER.NE.1) THEN                      ! If not able to do so
           WRITE(6,1040)                        ! tell user is wrong
           IER = 0                              ! Set error for return value
           GO TO 5                              ! Re-request date (if prompting)
        END IF
        IER = SYS$ASCTIM(TIMLEN,EXPDAT,EXTIME,)
        IF (TIMLEN.EQ.16) THEN
           CALL SYS$GETTIM(NOW)
           CALL LIB$SUBX(NOW,EXTIME,EXTIME)
           IER = SYS$ASCTIM(TIMLEN,EXPDAT,EXTIME,)
        END IF

        IF (EXPDAT(2:2).EQ.'-') EXPDAT = '0'//EXPDAT
        IER = COMPARE_DATE(EXPDAT(:11),TODAY(:11)) ! Compare date with today's
        IF (IER.GT.F_EXPIRE_LIMIT.AND.F_EXPIRE_LIMIT.GT.0.AND.
     &          .NOT.SETPRV_PRIV().AND.USERNAME.NE.FOLDER_OWNER) THEN
           WRITE(6,1050) F_EXPIRE_LIMIT         ! Expiration date > limit
           IER = 0                              ! Set error for return value
           GO TO 5                              ! Re-request date (if prompting)
        END IF
        IF (IER.EQ.0) IER = COMPARE_TIME(EXPDAT(13:),TODAY(13:))
        IF (IER.LE.0) THEN                      ! If expiration date not future
           WRITE(6,1045)                        ! tell user
           IER = 0                              ! Set error for return value
           GO TO 5                              ! Re-request date (if prompting)
        END IF

        IF (PROMPT) THEN
           IF (BTEST(SYSTEM,1)) THEN            ! Permanent message
              WRITE (6,'('' Message will be permanent.'')')
           ELSE
              WRITE (6,'('' Expiration date will be '',A,''.'')')
     &          EXPDAT(:TRIM(EXPDAT))
           END IF
        END IF

        IER = 1

        RETURN

1030    FORMAT(' It is ',A,'. Specify when message expires.')
1031    FORMAT(' It is ',A,'. Specify when message expires.',
     &          ' Default is permanent.')
1032    FORMAT(' It is ',A,'. Specify when message expires.',
     &          ' Default is ',I3,' days.')
1035    Format(' Enter absolute time: [dd-mmm-yyyy] hh:mm:ss ',
     &          'or delta time: dddd hh:mm:ss')
1040    FORMAT(' ERROR: Invalid date format specified.')
1045    FORMAT(' ERROR: Specified time has already passed.')
1050    FORMAT(' ERROR: Specified expiration period too large.'
     &          ' Limit is ',I3,' days.')

        END


        SUBROUTINE MAILEDIT(INFILE,OUTFILE)

        IMPLICIT INTEGER (A-Z)

        INCLUDE '($SSDEF)'

        COMMON /COMMAND_LINE/ INCMD
        CHARACTER*132 INCMD

        EXTERNAL BULLETIN_SUBCOMMANDS

        CHARACTER*(*) INFILE,OUTFILE

        CHARACTER*80 MAIL_EDIT,OUT

        IER = SYS_TRNLNM('MAIL$EDIT',MAIL_EDIT)

        OUT = OUTFILE
        IF (TRIM(OUT).EQ.0) THEN
           OUT = INFILE
        END IF

        IF (INDEX(MAIL_EDIT,'CALLABLE_').EQ.0.AND.
     &                          IER.EQ.SS$_NORMAL) THEN
           CALL DISABLE_PRIVS
           IF (OUT.EQ.INFILE) THEN
              CALL LIB$SPAWN('$@'//MAIL_EDIT(:TRIM(MAIL_EDIT))
     &          //' "" '//OUT(:TRIM(OUT)))
           ELSE
              CALL LIB$SPAWN('$@'//MAIL_EDIT(:TRIM(MAIL_EDIT))
     &          //' '//INFILE//' '//OUT(:TRIM(OUT)))
           END IF
           CALL ENABLE_PRIVS
        ELSE IF (INDEX(MAIL_EDIT,'EDT').GT.0.OR.
     &                          IER.NE.SS$_NORMAL) THEN
           CALL EDT$EDIT(INFILE,OUT)
        ELSE IF (INDEX(MAIL_EDIT,'TPU').GT.0) THEN
           CONTEXT = 0
           IER = LIB$FIND_FILE(INFILE,MAIL_EDIT,CONTEXT)
           IF (.NOT.IER) THEN
              CALL TPU$EDIT(' ',OUT)
           ELSE
              CALL TPU$EDIT(INFILE,OUT)
           END IF
           IER = CLI$DCL_PARSE(INCMD,BULLETIN_SUBCOMMANDS)
                ! TPU does CLI$ stuff which wipes our parsed command line
        END IF

        RETURN
        END





        SUBROUTINE CREATE_BULLCP

        IMPLICIT INTEGER (A-Z)

        INCLUDE '($PRCDEF)'

        INCLUDE '($JPIDEF)'

        INCLUDE '($SSDEF)'

        INCLUDE '($PRVDEF)'

        INCLUDE 'BULLFILES.INC'

        COMMON /REALPROC/ REALPROCPRIV(2)

        DIMENSION IMAGEPRIV(2)

        CHARACTER IMAGENAME*132,ANSWER*1,PRCNAM*15

        IF (.NOT.SETPRV_PRIV()) THEN
           WRITE (6,'('' ERROR: You do not have the privileges '',
     &                  ''to execute the command.'')')
           CALL EXIT
        END IF

        JUST_STOP = CLI$PRESENT('STOP')

        IF (JUST_STOP.AND..NOT.BTEST(REALPROCPRIV(1),PRV$V_SETPRV)) THEN
           WRITE (6,'('' ERROR: You need SETPRV to execute /STOP.'')')
           CALL EXIT
        ELSE IF (.NOT.JUST_STOP.AND.
     &                  .NOT.BTEST(REALPROCPRIV(1),PRV$V_SYSNAM)) THEN
           CALL SYS$SETPRV(,,,IMAGEPRIV)
           IF (.NOT.BTEST(IMAGEPRIV(1),PRV$V_SYSNAM)) THEN
              WRITE (6,'('' ERROR: This new version of BULLETIN'',
     &                  '' needs to be installed with SYSNAM.'')')
              CALL EXIT
           END IF
        END IF

        IF (TEST_BULLCP()) THEN
           IF (.NOT.JUST_STOP) THEN
              WRITE (6,'('' BULLCP process running.
     & Do you wish to kill it and restart a new one? '',$)')
              READ (5,'(A)') ANSWER
              IF (ANSWER.NE.'Y'.AND.ANSWER.NE.'y') CALL EXIT
           END IF

           WILDCARD = -1

           CALL INIT_ITMLST     ! Initialize item list
                                ! Now add items to list
           CALL ADD_2_ITMLST(LEN(PRCNAM),JPI$_PRCNAM,%LOC(PRCNAM))
           CALL ADD_2_ITMLST(4,JPI$_PID,%LOC(PID))
           CALL END_ITMLST(GETJPI_ITMLST)       ! Get address of itemlist
           IER = 1
           DO WHILE (IER.AND.PRCNAM(:6).NE.'BULLCP')
                                                ! Get next interactive process
              IER = SYS$GETJPIW(,WILDCARD,,%VAL(GETJPI_ITMLST),,,,)
                                                ! Get next process.
           END DO
           IF (IER.AND.PID.NE.0) IER = SYS$DELPRC(PID,)
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
        OPEN(UNIT=11,FILE=FOLDER_DIRECTORY(:LEN_B)//'BULLCP.COM',
     &          STATUS='NEW',IOSTAT=IER,CARRIAGECONTROL='LIST')
        IF (IER.NE.0) RETURN
        WRITE(11,'(A)') '$SET NOON'
        WRITE(11,'(A)') '$B:=$'//IMAGENAME(:ILEN)
        WRITE(11,'(A)') '$LOOP:'
        WRITE(11,'(A)') '$PURGE '//FOLDER_DIRECTORY(:LEN_B)//'BULLCP.LOG'
        WRITE(11,'(A)') '$DEF/USER SYS$OUTPUT '
     &                          //FOLDER_DIRECTORY(:LEN_B)//'BULLCP.LOG'
        WRITE(11,'(A)') '$DEF/USER SYS$ERROR '
     &                          //FOLDER_DIRECTORY(:LEN_B)//'BULLCP.ERR'
        WRITE(11,'(A)') '$B/BULLCP'
        WRITE(11,'(A)') '$WAIT 00:01:00'
        WRITE(11,'(A)') '$GOTO LOOP'            ! File open timed out
        CLOSE(UNIT=11)
        CALL SYS$SETDFPROT(CUR_DEF_PROT,)       ! Reset default protection

        IER = 0
        DO WHILE (IER.EQ.0.OR.(IER.EQ.SS$_DUPLNAM.AND.PID.GT.0))
           IER = SYS$CREPRC(,'SYS$SYSTEM:LOGINOUT',
     &        FOLDER_DIRECTORY(:LEN_B)//'BULLCP.COM','NL:'
     &        ,,,,'BULLCP',%VAL(4),,,%VAL(PRC$M_NOUAF+PRC$M_DETACH))
        END DO

        IF (IER) THEN
           OPEN(UNIT=11,FILE=FOLDER_DIRECTORY(:LEN_B)//'BULLCP.COM;-1',
     &          STATUS='OLD',IOSTAT=IER1)
           IF (IER1.EQ.0) CLOSE(UNIT=11,STATUS='DELETE',IOSTAT=IER1)
        END IF

        IF (.NOT.IER) THEN
           CALL SYS_GETMSG(IER)
        ELSE
           IF (CONFIRM_USER('DECNET').NE.0) THEN
              WRITE (6,'('' WARNING: Account with username DECNET'',
     &                          '' does not exist.'')')
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

        CHARACTER*1 DUMMY

        IER = SYS_TRNLNM('BULL_BULLCP',DUMMY)
        IF (IER) BULLCP = 1

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

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLDIR.INC'

        INCLUDE 'BULLUSER.INC'

        COMMON /BCP/ BULLCP
        LOGICAL BULLCP

        COMMON /KNOWN_FOLDERS/ FOLDER_Q1,NUM_FOLDERS

        CHARACTER*23 OLD_TIME,NEW_TIME

        IF (TEST_BULLCP()) CALL EXIT    ! BULLCP already running, so exit.

        CALL LIB$DATE_TIME(OLD_TIME)

        BULLCP = 2                      ! Enable process to do BULLCP functions

        IER = SYS$CREMBX(%VAL(1),CHAN,,,,,'BULL_BULLCP')
        IF (.NOT.IER) THEN              ! Can't create mailbox, so exit.
           CALL SYS_GETMSG(IER)
           CALL EXIT
        END IF

        IER = SYS$DELMBX(%VAL(CHAN))    ! If process dies, mailbox is deleted.

        CALL REGISTER_BULLCP

        CALL SET_REMOTE_SYSTEM

        CALL START_DECNET

        DO WHILE (1)                    ! Loop once every 15 minutes
           CALL SYS$SETAST(%VAL(0))
           CALL LIB$DATE_TIME(NEW_TIME)
           CALL GET_PROXY_ACCOUNTS      ! Proxy info for incoming connections
           CALL SYS$SETAST(%VAL(1))
           CALL BBOARD                  ! Look for BBOARD messages.
           FOLDER_Q = FOLDER_Q1         ! Init queue pointer to header
           POINT_FOLDER = 0
           DO WHILE (POINT_FOLDER.LT.NUM_FOLDERS)
              POINT_FOLDER = POINT_FOLDER + 1
              CALL SYS$SETAST(%VAL(0))
              CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)
              IF (FOLDER_BBOARD(:2).NE.'::') THEN
                 CALL SELECT_FOLDER(.FALSE.,IER)        ! Select folder
                 IF (IER) THEN
                    CALL DELETE_EXPIRED         ! Delete expired messages
                    IF (INDEX(NEW_TIME,' 03:').NE.0.AND.   ! Do empty block
     &                  INDEX(OLD_TIME,' 03:').EQ.0) THEN  ! cleanup at 3 a.m.
                       IF (NEMPTY.GT.200) THEN
                          CALL CLEANUP_BULLFILE ! Cleanup empty blocks
                       END IF
                    END IF
                 END IF
              END IF
              CALL SYS$SETAST(%VAL(1))
           END DO

           IF (INDEX(NEW_TIME,' 03:').NE.0.AND.   ! Cleanup deleted users from
     &         INDEX(OLD_TIME,' 03:').EQ.0) THEN  ! data files at 3 a.m.
              CALL SYS$SETAST(%VAL(0))
              CALL TOTAL_CLEANUP_LOGIN
              CALL SYS$SETAST(%VAL(1))
           END IF

           OLD_TIME = NEW_TIME
           CALL WAIT('15')              ! Wait for 15 minutes
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
           FOLDER_NUMBER = 0                    ! Reset to GENERAL folder
           CALL SELECT_FOLDER(.FALSE.,IER)
           CALL REGISTER_BULLCP
           CALL SYS$SETAST(%VAL(1))
        END DO

        RETURN
        END




        SUBROUTINE SET_REMOTE_SYSTEM

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLFOLDER.INC'

        COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

        CHARACTER NODENAME*8

        CALL LIB$SYS_TRNLOG('SYS$NODE',,NODENAME)
        NODENAME = NODENAME(2:INDEX(NODENAME,':')-1)

        CALL OPEN_BULLFOLDER_SHARED

        IER = 0
        DO WHILE (IER.EQ.0)
           CALL READ_FOLDER_FILE(IER)
           IF (FOLDER_BBOARD(:2).EQ.'::'.AND.BTEST(FOLDER_FLAG,2)
     &          .AND.IER.EQ.0) THEN
              CALL SELECT_FOLDER(.FALSE.,IER1)
              IF (IER1) THEN
                 WRITE(REMOTE_UNIT,'(3A)',IOSTAT=IER1) 14,
     &                  BTEST(FOLDER_FLAG,2),NODENAME
              END IF
           END IF
        END DO

        CALL CLOSE_BULLFOLDER

        FOLDER_NUMBER = 0                       ! Reset to GENERAL folder
        CALL SELECT_FOLDER(.FALSE.,IER)

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

        CALL OPEN_BULLUSER

        DO WHILE (REC_LOCK(IER))
           READ (4,KEY='*SYSTEM',IOSTAT=IER) 
     &          TEMP_USER,NODENAME,NODE_NUMBER,NODE_AREA,NEW_FLAG,
     &          SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG
        END DO

        IF (IER.NE.0) THEN
           DO I=1,FLONG
              SYSTEM_FLAG(I) = 0
              SHUTDOWN_FLAG(I) = 0
           END DO
           CALL SET2(SYSTEM_FLAG,0)
           NODE_AREA = 0
        END IF

        CALL LIB$SYS_TRNLOG('SYS$NODE',,NODENAME)
        NODENAME = NODENAME(2:INDEX(NODENAME,':')-1)

        DO I=1,FLONG
           SHUTDOWN_FLAG(I) = SYSTEM_FLAG(I)
        END DO

        IF (IER.NE.0) THEN
           WRITE (4,IOSTAT=IER)
     &          '*SYSTEM     ',NODENAME,NODE_NUMBER,NODE_AREA,NEW_FLAG,
     &          SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG
        ELSE
           REWRITE (4,IOSTAT=IER)
     &          TEMP_USER,NODENAME,NODE_NUMBER,NODE_AREA,NEW_FLAG,
     &          SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG
        END IF

        CALL CLOSE_BULLUSER

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
     &          TEMP_USER,NODENAME,NODE_NUMBER,NODE_AREA,NEW_FLAG,
     &          SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG
        END DO

        CALL CLR2(SHUTDOWN_FLAG,FOLDER_NUMBER)

        SEEN_FLAG = 0
        DO I=1,FLONG
           IF (SHUTDOWN_FLAG(I).NE.0) SEEN_FLAG = 1
        END DO
        IF (SEEN_FLAG.EQ.0) NODE_AREA = 0       ! All done with that node

        IF (IER.NE.0) THEN
           WRITE (4,IOSTAT=IER)
     &          '*SYSTEM     ',NODENAME,NODE_NUMBER,NODE_AREA,NEW_FLAG,
     &          SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG
        ELSE
           REWRITE (4,IOSTAT=IER)
     &          TEMP_USER,NODENAME,NODE_NUMBER,NODE_AREA,NEW_FLAG,
     &          SYSTEM_FLAG,SHUTDOWN_BTIM,SHUTDOWN_FLAG
        END IF

        CALL CLOSE_BULLUSER

        RETURN
        END





        SUBROUTINE WAIT(PARAM)
C
C SUBROUTINE WAIT
C
C FUNCTION: Waits for specified time period in minutes.
C
        IMPLICIT INTEGER (A-Z)
        INTEGER TIMADR(2)                       ! Buffer containing time
                                                ! in desired system format.
        CHARACTER TIMBUF*13,PARAM*2
        DATA TIMBUF/'0 00:00:00.00'/

        DATA WAIT_EF /0/

        IF (WAIT_EF.EQ.0) CALL LIB$GET_EF(WAIT_EF)

        TIMBUF(6:7) = PARAM

        IER=SYS$BINTIM(TIMBUF,TIMADR)
        IER=SYS$SETIMR(%VAL(WAIT_EF),TIMADR,,%VAL(2))   ! Set timer.
        IER=SYS$WAITFR(%VAL(WAIT_EF))           ! Wait for EFN to be set.

        RETURN
        END



        SUBROUTINE WAIT_SEC(PARAM)
C
C SUBROUTINE WAIT_SEC
C
C FUNCTION: Waits for specified time period in seconds.
C
        IMPLICIT INTEGER (A-Z)
        INTEGER TIMADR(2)                       ! Buffer containing time
                                                ! in desired system format.
        CHARACTER TIMBUF*13,PARAM*2
        DATA TIMBUF/'0 00:00:00.00'/
        DATA WAIT_EF /0/

        IF (WAIT_EF.EQ.0) CALL LIB$GET_EF(WAIT_EF)

        TIMBUF(9:10) = PARAM

        IER=SYS$BINTIM(TIMBUF,TIMADR)
        IER=SYS$SETIMR(%VAL(WAIT_EF),TIMADR,,%VAL(3))   ! Set timer.
        IER=SYS$WAITFR(%VAL(WAIT_EF))           ! Wait for EFN to be set.

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

        CHARACTER UPTIME_DATE*11,UPTIME_TIME*11

        CALL OPEN_BULLDIR_SHARED        ! Open directory file
        CALL OPEN_BULLFIL_SHARED        ! Open bulletin file
        CALL CLOSE_BULLFIL
        CALL READDIR(0,IER)             ! Get directory header
        IF (IER.EQ.1) THEN              ! Is header present?
           IER = COMPARE_DATE(NEWEST_EXDATE,' ') ! Yes. Any expired bulls?
           IF (IER.GT.20*356) IER = -1  ! Check if latest expiration date valid.
           IF (IER.EQ.0) IER = COMPARE_TIME(NEWEST_EXTIME,' ')
           IF (SHUTDOWN.GT.0.AND.NODE_AREA.GT.0.AND.
     &          (FOLDER_NUMBER.EQ.0.OR.BTEST(FOLDER_FLAG,2)).AND.
     &          TEST2(SHUTDOWN_FLAG,FOLDER_NUMBER)) THEN
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
              CALL OPEN_BULLDIR         ! Reopen without sharing
              CALL UPDATE               ! Need to update
           END IF
        ELSE            ! If header not there, then first time running BULLETIN
           CALL OPEN_BULLUSER           ! Create user file to be able to set
           CALL CLOSE_BULLUSER          ! defaults, privileges, etc.
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

        CHARACTER*11 INEXDATE
        CHARACTER INDESCRIP*(LINE_LENGTH),INFROM*(LINE_LENGTH),INTO*76
        CHARACTER ACCOUNT_SAVE*8,USERNAME_SAVE*12

        DIMENSION NEW_MAIL(FOLDER_MAX)

        DATA SPAWN_EF/0/

        CALL SYS$SETAST(%VAL(0))

        IF (SPAWN_EF.EQ.0) CALL LIB$GET_EF(SPAWN_EF)

        CALL DISABLE_CTRL

        CALL INIT_QUEUE(FOLDER_Q1,FOLDER_COM)

        FOLDER_Q = FOLDER_Q1

        CALL OPEN_BULLFOLDER_SHARED             ! Get folder file

        NUM_FOLDERS = 0
        IER = 0
        DO WHILE (IER.EQ.0)                     ! Copy all bulletins from file
           CALL READ_FOLDER_FILE(IER)
           IF (IER.EQ.0) THEN
              NUM_FOLDERS = NUM_FOLDERS + 1
              CALL WRITE_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)
           END IF
        END DO

        CALL CLOSE_BULLFOLDER                   ! We don't need file anymore
        CALL SYS$SETAST(%VAL(1))

        CALL SYS$SETAST(%VAL(0))
        CALL CHECK_MAIL(NEW_MAIL)
        CALL SYS$SETAST(%VAL(1))

        FOLDER_Q = FOLDER_Q1                    ! Init queue pointer to header

        NBBOARD_FOLDERS = 0

        POINT_FOLDER = 0

1       POINT_FOLDER = POINT_FOLDER + 1
        IF (POINT_FOLDER.GT.NUM_FOLDERS) GO TO 900

        CALL SYS$SETAST(%VAL(0))

        FOLDER_Q_SAVE = FOLDER_Q

        CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)

        IF (FOLDER_BBOARD.EQ.'NONE'.OR.
     &          FOLDER_BBOARD(:2).EQ.'::') GO TO 1

        NBBOARD_FOLDERS = NBBOARD_FOLDERS + 1

        IF (.NOT.NEW_MAIL(POINT_FOLDER)) GO TO 1
C
C  The process is set to the BBOARD uic and username in order to create
C  a spawned process that is able to read the BBOARD mail (a real kludge).
C

        CALL GETUSER(USERNAME_SAVE)             ! Get present username
        CALL GETACC(ACCOUNT_SAVE)               ! Get present account
        CALL GETUIC(GROUP_SAVE,USER_SAVE)       ! Get present uic

        IF (TRIM(FOLDER_BBOARD).GT.0) THEN      ! BBOARD name present?
           IER = SETUSER(FOLDER_BBOARD,USERNAME_SAVE)! Set to BBOARD username
           IF (IER.EQ.2) GO TO 910      ! Can't set username. New VMS version?
           CALL SETACC(ACCOUNTB)        ! Set to BBOARD account
           CALL SETUIC(IBCLR(GROUPB,31),IBCLR(USERB,31)) ! Set to BBOARD uic
        END IF

        LEN_B = TRIM(BBOARD_DIRECTORY)
        IER = LIB$DELETE_FILE(BBOARD_DIRECTORY(:LEN_B)//
     &          FOLDER_BBOARD(:TRIM(FOLDER_BBOARD))//'.TXT;*')
                                ! Delete old TXT files left due to errors

        IF (.NOT.BTEST(USERB,31).AND.(USERB.NE.0.OR.GROUPB.NE.0)) THEN
                                                        ! If normal BBOARD user
         IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(:LEN_B)
     &          //'READ_BOARD.COM','NL:','NL:',1,,,STATUS,SPAWN_EF)
         CALL SYS$SETAST(%VAL(1))
         IF (IER) CALL SYS$WAITFR(%VAL(SPAWN_EF))
         CALL SYS$SETAST(%VAL(0))
         IF (((STATUS.AND.'1FFFF'X).EQ.RMS$_FNF) .OR.
     &   ((STATUS .AND. '1FFF0'X).EQ. (RMS$_SPL .AND. '1FFF0'X))) THEN
           CALL SYS$SETDFPROT('AA00'X,CUR_DEF_PROT)
                ! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD:RW,GROUP:RW)
           OPEN(UNIT=11,FILE=BBOARD_DIRECTORY(:LEN_B)//'READ_BOARD.COM',
     &          STATUS='NEW',ERR=910,CARRIAGECONTROL='LIST')
           WRITE(11,'(A)') '$ SET PROTECT=(W:RWED)/DEFAULT'
           WRITE(11,'(A)') '$ SET PROC/PRIV=SYSPRV'
           WRITE(11,'(A)')
     & '$ DEFINE/USER EXTRACT_FILE '//BBOARD_DIRECTORY(:LEN_B)//
     & '''F$GETJPI("","USERNAME")'''
           WRITE(11,'(A)') '$ MAIL'
           WRITE(11,'(A)') 'READ'
           WRITE(11,'(A)') 'EXTRACT/ALL/APPEND EXTRACT_FILE'
           WRITE(11,'(A)') 'DELETE/ALL'
           WRITE(11,'(A)') 'SELECT/NEW'
           CLOSE(UNIT=11)
           CALL SYS$SETDFPROT(CUR_DEF_PROT,)    ! Reset default protection
           IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(:LEN_B)
     &          //'READ_BOARD.COM','NL:','NL:',1,,,STATUS,SPAWN_EF)
           CALL SYS$SETAST(%VAL(1))
           IF (IER) CALL SYS$WAITFR(%VAL(SPAWN_EF))
           CALL SYS$SETAST(%VAL(0))
         END IF
        ELSE
         CONTEXT = 0
         IER = LIB$FIND_FILE(BBOARD_DIRECTORY(:LEN_B)//FOLDER_BBOARD
     &      (:TRIM(FOLDER_BBOARD))//'.COM',INPUT,CONTEXT)
         IF (IER) THEN
            IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(:LEN_B)//
     &          FOLDER_BBOARD(:TRIM(FOLDER_BBOARD))//'.COM','NL:',
     &          'NL:',1,,,STATUS,SPAWN_EF)
            CALL SYS$SETAST(%VAL(1))
            IF (IER) CALL SYS$WAITFR(%VAL(SPAWN_EF))
            CALL SYS$SETAST(%VAL(0))
         END IF
         IF (.NOT.IER.OR.((STATUS.AND.'1FFFF'X).EQ.RMS$_FNF) .OR.
     &   ((STATUS .AND. '1FFF0'X).EQ. (RMS$_SPL .AND. '1FFF0'X))) THEN
            IER = LIB$SPAWN('$@'//BBOARD_DIRECTORY(:LEN_B)//
     &        'BOARD_SPECIAL.COM','NL:','NL:',1,,,STATUS,SPAWN_EF)
            CALL SYS$SETAST(%VAL(1))
            IF (IER) CALL SYS$WAITFR(%VAL(SPAWN_EF))
            CALL SYS$SETAST(%VAL(0))
         END IF
        END IF

        CALL READ_QUEUE(%VAL(FOLDER_Q_SAVE),FOLDER_Q,FOLDER_COM)

        NBULL = F_NBULL

        CALL SETACC(ACCOUNT_SAVE)               ! Reset to original account
        CALL SETUSER(USERNAME_SAVE)             ! Reset to original username
        CALL SETUIC(GROUP_SAVE,USER_SAVE)       ! Reset to original uic

        OPEN (UNIT=3,FILE=BBOARD_DIRECTORY(:LEN_B)//FOLDER_BBOARD
     &     (:TRIM(FOLDER_BBOARD))//'.TXT',STATUS='OLD',ERR=100)
        READ (3,'(Q,A)',END=100) LEN_INPUT,INPUT ! Read first line
        CALL SYS$SETAST(%VAL(1))

5       CALL SYS$SETAST(%VAL(0))

        CALL READ_QUEUE(%VAL(FOLDER_Q_SAVE),IDUMMY,FOLDER_COM)

        DO WHILE (LEN_INPUT.GT.0)
           IF (INPUT(:5).EQ.'From:') THEN
              INFROM = INPUT(7:)                ! Store username
           ELSE IF (INPUT(:5).EQ.'Subj:') THEN
              INDESCRIP = INPUT(7:)             ! Store subject
           ELSE IF (INPUT(:3).EQ.'To:') THEN
              INTO = INPUT(5:)                  ! Store address
           END IF
           READ (3,'(Q,A)',END=100) LEN_INPUT,INPUT ! Read next line from mail
        END DO

        INTO = INTO(:TRIM(INTO))
        CALL STR$TRIM(INTO,INTO)
        CALL STR$UPCASE(INTO,INTO)
        FLEN = TRIM(FOLDER_BBOARD)
        IF (INDEX(INTO,FOLDER_BBOARD(:FLEN)).EQ.0.AND.
     &   INTO.NE.FOLDER_BBOARD.AND.INDEX(INTO,'@').EQ.0) THEN
           POINT_FOLDER1 = 0
           FOLDER_Q2 = FOLDER_Q1
           FOLDER1_BBOARD = FOLDER_BBOARD
           FOUND = .FALSE.
           DO WHILE (.NOT.FOUND.AND.POINT_FOLDER1.LT.NUM_FOLDERS)
              FOLDER_Q2_SAVE = FOLDER_Q2
              CALL READ_QUEUE(%VAL(FOLDER_Q2),FOLDER_Q2,FOLDER1_COM)
              FLEN = TRIM(FOLDER1_BBOARD)
              POINT_FOLDER1 = POINT_FOLDER1 + 1
              IF (POINT_FOLDER1.LE.NUM_FOLDERS.AND.
     &            FOLDER1_BBOARD(:2).NE.'::'.AND.
     &            FOLDER1_BBOARD.NE.'NONE') THEN
                 IF (INTO.EQ.FOLDER1_BBOARD) THEN
                    FOUND = .TRUE.
                 ELSE
                    FIND_TO = INDEX(INTO,FOLDER1_BBOARD(:FLEN))
                    IF (FIND_TO.GT.0) THEN
                       END_TO = FLEN+FIND_TO
                       IF (TRIM(INTO).LT.END_TO.OR.
     &                      INTO(END_TO:END_TO).LT.'A'.OR.
     &                      INTO(END_TO:END_TO).GT.'Z') THEN
                          IF (FIND_TO.EQ.1) THEN
                             FOUND = .TRUE.
                          ELSE IF (INTO(FIND_TO-1:FIND_TO-1).LT.'A'.OR.
     &                             INTO(FIND_TO-1:FIND_TO-1).GT.'Z') THEN
                             FOUND = .TRUE.
                          END IF
                       END IF
                    END IF
                 END IF
              END IF
           END DO
           IF (FOUND) THEN
              FOLDER_COM = FOLDER1_COM
              FOLDER_Q_SAVE = FOLDER_Q2_SAVE
           END IF
        END IF

        READ (3,'(Q,A)',IOSTAT=IER) LEN_INPUT,INPUT     ! Read first line
        DO WHILE (LEN_INPUT.EQ.1.AND.INPUT(:1).EQ.CHAR(12).AND.IER.EQ.0)
           READ (3,'(Q,A)',IOSTAT=IER) LEN_INPUT,INPUT
           IF (INPUT(:5).EQ.'From:') GO TO 5
        END DO          ! If line is just form feed, the message is empty
        IF (IER.NE.0) GO TO 100                         ! If end of file, exit

        EFROM = 2
        I = TRIM(INFROM)
        DO WHILE (EFROM.GT.0.AND.I.GT.0)                ! Strip off the date
          IF (INFROM(I:I).EQ.' ') EFROM = EFROM - 1     ! From the "From:" line
          I = I - 1
        END DO
        IF (I.GT.0) INFROM = INFROM(:I)

        CALL INIT_MESSAGE_ADD_BBOARD(INFROM,INDESCRIP,IER)

        ISTART = 0
        NBLANK = 0
        IER = 0
        DO WHILE (IER.EQ.0)             ! Move text to bulletin file
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
           READ (3,'(Q,A)',IOSTAT=IER) LEN_INPUT,INPUT
           IF (LEN_INPUT.EQ.1.AND.INPUT(:1).EQ.CHAR(12)) THEN
              DO WHILE (LEN_INPUT.EQ.1.AND.INPUT(:1).EQ.CHAR(12)
     &                  .AND.IER.EQ.0)
                 READ (3,'(Q,A)',IOSTAT=IER) LEN_INPUT,INPUT
              END DO
              IF (IER.EQ.0.AND.INPUT(:5).EQ.'From:') THEN
                 IER = 1
              ELSE
                 NBLANK = NBLANK + 1
              END IF
           END IF
        END DO

        CALL FINISH_MESSAGE_ADD                 ! Totally finished with add

        CALL SYS$SETAST(%VAL(1))

        GO TO 5                                 ! See if there is more mail

100     CLOSE (UNIT=3,STATUS='DELETE')          ! Close the input file
        CALL SYS$SETAST(%VAL(1))
        GO TO 1

900     CALL SYS$SETAST(%VAL(0))

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
           REWRITE (4) USER_HEADER              ! Rewrite header
           CALL CLOSE_BULLUSER
        END IF

        CALL SYS$SETAST(%VAL(1))

        RETURN

910     WRITE (6,1010)
        GO TO 100

930     CLOSE (UNIT=3)
        CALL CLOSE_BULLFIL
        CALL CLOSE_BULLDIR
        WRITE (6,1030)
        GO TO 100

1010    FORMAT(' ERROR:Install program with CMKRNL privileges or relink.')
1030    FORMAT(' ERROR:Alert system programmer. Data file problems.')

        END




        SUBROUTINE CREATE_BBOARD_PROCESS

        IMPLICIT INTEGER (A-Z)

        INCLUDE '($PRCDEF)'

        INCLUDE 'BULLFILES.INC'

        COMMON /PRIVILEGES/ PROCPRIV(2),NEEDPRIV(2)

        CHARACTER*132 IMAGENAME

        CALL GETIMAGE(IMAGENAME,ILEN)

        LEN_B = TRIM(BBOARD_DIRECTORY)

        OPEN(UNIT=11,FILE=BBOARD_DIRECTORY(:LEN_B)//'BULL_COMMAND.COM',
     &          STATUS='OLD',IOSTAT=IER)
        IF (IER.EQ.0) CLOSE(UNIT=11,STATUS='DELETE')

        CALL SYS$SETDFPROT('AA00'X,CUR_DEF_PROT)
                ! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD:RW,GROUP:RW)
        OPEN(UNIT=11,FILE=BBOARD_DIRECTORY(:LEN_B)//'BULL_COMMAND.COM',
     &          STATUS='NEW',IOSTAT=IER,CARRIAGECONTROL='LIST')
        IF (IER.NE.0) RETURN
        WRITE(11,'(A)') '$B:=$'//IMAGENAME(:ILEN)
        WRITE(11,'(A)') '$ON ERROR THEN GOTO EXIT'
        WRITE(11,'(A)') '$ON SEVERE THEN GOTO EXIT'
        WRITE(11,'(A)') '$ON WARNING THEN GOTO EXIT'
        WRITE(11,'(A)') '$B/'//'''F$PROCESS()'''
        WRITE(11,'(A)') '$EXIT:'
        WRITE(11,'(A)') '$LOGOUT'
        CLOSE(UNIT=11)
        CALL SYS$SETDFPROT(CUR_DEF_PROT,)       ! Reset default protection

        IER = SYS$CREPRC(,'SYS$SYSTEM:LOGINOUT',
     &   BBOARD_DIRECTORY(:LEN_B)//'BULL_COMMAND.COM','NL:',,
     &   PROCPRIV,,'BBOARD',%VAL(4),,,%VAL(PRC$M_NOUAF+PRC$M_DETACH))

        RETURN
        END



        SUBROUTINE GETUIC(GRP,MEM)
C
C  SUBROUTINE GETUIC(UIC)
C
C  FUNCTION:
C       To get UIC of process submitting the job.
C  OUTPUT:
C       GRP   -    Group number of UIC
C       MEM   -    Member number of UIC
C

        IMPLICIT INTEGER (A-Z)

        INCLUDE '($JPIDEF)'

        CALL INIT_ITMLST        ! Initialize item list
                                ! Now add items to list
        CALL ADD_2_ITMLST(4,JPI$_GRP,%LOC(GRP))
        CALL ADD_2_ITMLST(4,JPI$_MEM,%LOC(MEM))
        CALL END_ITMLST(GETJPI_ITMLST)  ! Get address of itemlist

        IER = SYS$GETJPIW(,,,%VAL(GETJPI_ITMLST),,,,)   ! Get Info command.

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

        INTEGER         UPTIME(2)
        CHARACTER*(*)   UPTIME_TIME,UPTIME_DATE
        CHARACTER       ASCSINCE*23

        CALL INIT_ITMLST
        CALL ADD_2_ITMLST(8,SYI$_BOOTTIME,%LOC(UPTIME))
        CALL END_ITMLST(GETSYI_ITMLST)

        IER = SYS$GETSYI(,,,%VAL(GETSYI_ITMLST),,,)

        CALL SYS$ASCTIM(,ASCSINCE,UPTIME,)

        UPTIME_DATE = ASCSINCE(:11)
        UPTIME_TIME = ASCSINCE(13:)

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

        CHARACTER INPUT*37,FILENAME*132

        INTEGER*2 COUNT

        FOLDER_Q = FOLDER_Q1                    ! so reinit queue pointer

        OPEN (UNIT=10,FILE='VMSMAIL_PROFILE',
     &       DEFAULTFILE='SYS$SYSTEM:VMSMAIL_PROFILE.DATA',
     &       ACCESS='KEYED',FORM='FORMATTED',ORGANIZATION='INDEXED',
     &       STATUS='OLD',READONLY,SHARED,IOSTAT=IER)
        OFFSET = 36

        IF (IER.NE.0) THEN
           OPEN (UNIT=10,FILE='VMSMAIL',
     &       DEFAULTFILE='SYS$SYSTEM:VMSMAIL.DAT',
     &       ACCESS='KEYED',FORM='FORMATTED',ORGANIZATION='INDEXED',
     &       STATUS='OLD',READONLY,SHARED,IOSTAT=IER)
           OFFSET = 34
        END IF

        DO I=1,NUM_FOLDERS
           CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)

           IF (((.NOT.BTEST(USERB,31).AND.(USERB.NE.0.OR.GROUPB.NE.0)).OR.
     &           BTEST(GROUPB,31)).AND.FOLDER_BBOARD(:2).NE.'::') THEN
                                        ! If normal BBOARD or /VMSMAIL
              READ(10,'(A)',KEY=FOLDER_BBOARD,IOSTAT=IER1) INPUT
              CALL LIB$MOVC3(2,%REF(INPUT(OFFSET:)),COUNT)
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
C       To get image name of process.
C  OUTPUT:
C       IMAGNAME   -    Image name of process
C       ILEN       -    Length of imagename
C

        IMPLICIT INTEGER (A-Z)

        INCLUDE '($JPIDEF)'

        CHARACTER*(*) IMAGNAME

        CALL INIT_ITMLST        ! Initialize item list
                                ! Now add items to list
        CALL ADD_2_ITMLST_WITH_RET(LEN(IMAGNAME),JPI$_IMAGNAME,
     &                                  %LOC(IMAGNAME),%LOC(ILEN))
        CALL END_ITMLST(GETJPI_ITMLST)  ! Get address of itemlist

        IER = SYS$GETJPIW(,,,%VAL(GETJPI_ITMLST),,,,)   ! Get Info command.

        RETURN
        END




        SUBROUTINE GET_NEWEST_MSG(IN_BTIM,START)

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLDIR.INC'

        COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

        DIMENSION IN_BTIM(2)

        IF (REMOTE_SET) THEN
           WRITE (REMOTE_UNIT,'(3A)',IOSTAT=IER) 12,IN_BTIM(1),IN_BTIM(2)
           IF (IER.EQ.0) THEN
              READ (REMOTE_UNIT,'(A)',IOSTAT=IER) START
           END IF
        ELSE
           CALL GET_MSGKEY(IN_BTIM,MSG_KEY)
           CALL READDIR_KEYGE(START)
           IF (START.EQ.0) THEN
              START = -1
           END IF
        END IF

        RETURN
        END
