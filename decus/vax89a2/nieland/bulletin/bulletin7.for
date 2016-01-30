C
C  BULLETIN7.FOR, Version 4/16/89
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Programmer: Mark R. London
C
        SUBROUTINE UPDATE_LOGIN(ADD_BULL)
C
C  SUBROUTINE UPDATE_LOGIN
C
C  FUNCTION:  Updates the login file when a bulletin has been deleted
C       or added.
C
        IMPLICIT INTEGER (A - Z)

        INCLUDE 'BULLDIR.INC'

        INCLUDE 'BULLUSER.INC'

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE '($BRKDEF)'

        INCLUDE '($SSDEF)'

        DIMENSION READ_BTIM_SAVE(2),TEMP_BTIM(2)

        CHARACTER OUTPUT*160,TERMINAL*7,FLAGS*1
        CHARACTER*1 CR/13/,LF/10/,BELL/7/

C
C  We want to keep the last read date for comparison when selecting new
C  folders, so save it for later restoring.
C

        READ_BTIM_SAVE(1) = READ_BTIM(1)
        READ_BTIM_SAVE(2) = READ_BTIM(2)

        CALL OPEN_BULLUSER_SHARED

C
C  Newest date/time in user file only applies to general bulletins.
C  This was present before adding folder capability.
C  We set flags in user entry to show new folder added for folder bulletins.
C  However, the newest bulletin for each folder is not continually updated,
C  As it is only used when comparing to the last bulletin read time, and to
C  store this for each folder would be too expensive.
C

        TEMP_BTIM(1) = NEWEST_BTIM(1)
        TEMP_BTIM(2) = NEWEST_BTIM(2)
        CALL READ_USER_FILE_HEADER(IER)
        NEWEST_BTIM(1) = TEMP_BTIM(1)
        NEWEST_BTIM(2) = TEMP_BTIM(2)

        IF (IER.NE.0) THEN
           CALL CLOSE_BULLUSER
           RETURN
        ELSE IF (FOLDER_NUMBER.EQ.0) THEN
           CALL SYS_BINTIM(NEWEST_DATE//' '//NEWEST_TIME,NEWEST_BTIM)
           REWRITE (4,IOSTAT=IER) USER_HEADER
        END IF

        IF (ADD_BULL.AND.FOLDER_NUMBER.GE.0) THEN       ! Message added?
           IF (FOLDER_NUMBER.GT.0) THEN         ! Folder private?
              CALL CHKACL
     &          (FOLDER_FILE(1:TRIM(FOLDER_FILE))//'.BULLFIL',IER)
              IF (IER.EQ.(SS$_ACLEMPTY.OR.SS$_NORMAL)) THEN
                 CHECK_ACL = 0
              ELSE
                 CHECK_ACL = 1
              END IF
           ELSE
               CHECK_ACL = 0
           END IF

           OUTPUT = BELL//CR//LF//LF//
     &          'New bulletin added to folder '//FOLDER(1:TRIM(FOLDER))
     &          //'. From: '//FROM(1:TRIM(FROM))//CR//LF//
     &          'Description: '//DESCRIP(1:TRIM(DESCRIP))

           IER = SYS_TRNLNM('BULL_SYSTEM_FLAGS',FLAGS)
           IF (.NOT.IER) THEN
              IER = SYS_TRNLNM('MAIL$SYSTEM_FLAGS',FLAGS)
           END IF

           FLAG = 0
           BFLAG = 0

           IF (IER) THEN
              READ (FLAGS(:1),'(I1)',IOSTAT=IER) FLAG
              IF (BTEST(FLAG,1).AND.IER.EQ.0) THEN      ! Node part of cluster?
                 CALL OPEN_BULLNOTIFY_SHARED            ! Yes, get notify list.
                 DO WHILE (REC_LOCK(IER1))              ! Any entries?
                    READ (10,IOSTAT=IER1) TEMP_USER
                 END DO
                 IF (IER1.NE.0) THEN                    ! No entries.
                    CALL READ_USER_FILE(IER)            ! Create entries from
                    DO WHILE (IER.EQ.0)                 ! user file.
                       IF (TEMP_USER(:1).NE.':'.AND.TEMP_USER(:1).NE.'*'
     &                     .AND.TEST2(NOTIFY_FLAG,FOLDER_NUMBER)) THEN
                          WRITE (10) TEMP_USER
                       END IF
                       CALL READ_USER_FILE(IER)
                    END DO
                    DO WHILE (REC_LOCK(IER1))           ! Reset to first entry.
                       READ (10,KEYGT='            ',IOSTAT=IER1)
     &                          TEMP_USER
                    END DO
                 END IF

                 BFLAG = BRK$M_CLUSTER                  ! Broadcast to all nodes

                 IF (TEST2(NOTIFY_FLAG_DEF,FOLDER_NUMBER).AND.  ! If /ALL then
     &               TEMP_USER.EQ.'*'.AND.IER1.EQ.0) THEN       ! notify all.
                    CALL SYS$BRKTHRU(,OUTPUT(1:TRIM(OUTPUT))//CR,
     &               ,%VAL(BRK$C_ALLUSERS),,,%VAL(BFLAG),,,,)
                    IER1 = 1            ! Don't have to loop through notify list
                 END IF
              END IF
           END IF

           DO WHILE ((BFLAG.EQ.0.AND.GETUSERS(TEMP_USER,TERMINAL)).OR.
     &               (BFLAG.NE.0.AND.IER1.EQ.0))
              CALL READ_USER_FILE_KEYNAME(TEMP_USER,IER)
              IF (IER.EQ.0.AND.TEMP_USER.NE.FROM.AND.
     &            TEST2(NOTIFY_FLAG,FOLDER_NUMBER)) THEN
                 IF (CHECK_ACL) THEN
                    CALL CHECK_ACCESS
     &                  (FOLDER_FILE(1:TRIM(FOLDER_FILE))//'.BULLFIL',
     &                  TEMP_USER,IER,WRITE_ACCESS)
                 ELSE
                    IER = 1
                 END IF
                 IF (IER) THEN
                    IF (BFLAG.EQ.0) THEN
                        CALL SYS$BRKTHRU(,OUTPUT(1:TRIM(OUTPUT))//CR,
     &                   TERMINAL(:TRIM(TERMINAL)),%VAL(BRK$C_DEVICE)
     &                   ,,,%VAL(BFLAG),,,,)
                    ELSE
                        CALL SYS$BRKTHRU(,OUTPUT(1:TRIM(OUTPUT))//CR,
     &                   TEMP_USER(:TRIM(TEMP_USER)),%VAL(BRK$C_USERNAME)
     &                   ,,,%VAL(BFLAG),,,,)
                    END IF
                 ELSE
                    CALL CLR2(NOTIFY_FLAG,FOLDER_NUMBER)
                    REWRITE (4,IOSTAT=IER) TEMP_USER//USER_ENTRY(13:)
                 END IF
              ELSE IF (IER.NE.0.AND.BFLAG.NE.0) THEN
                 DELETE (UNIT=10)
              END IF
              IF (BFLAG.NE.0) THEN
                 DO WHILE (REC_LOCK(IER1))
                    READ (10,IOSTAT=IER1) TEMP_USER
                 END DO
              END IF
           END DO
           IF (BFLAG.NE.0) CALL CLOSE_BULLNOTIFY
        END IF

        CALL READ_USER_FILE_KEYNAME(USERNAME,IER)
                ! Reobtain present values as calling programs still uses them

        READ_BTIM(1) = READ_BTIM_SAVE(1)
        READ_BTIM(2) = READ_BTIM_SAVE(2)

        CALL CLOSE_BULLUSER

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

        INCLUDE 'BULLFOLDER.INC'

        COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
        
        CHARACTER TODAY_TIME*32

        COMMON /COMMAND_LINE/ INCMD
        CHARACTER*132 INCMD

        IF (REMOTE_SET) THEN
           LOCAL = .TRUE.
           IF (INCMD(:3).EQ.'ADD') LOCAL = CLI$PRESENT('LOCAL')
           IF (LOCAL) THEN
              WRITE (REMOTE_UNIT,'(9A)',IOSTAT=IER)
     &                  3,DESCRIP,EXDATE,EXTIME,SYSTEM,0,0,0,0
           ELSE
              WRITE (REMOTE_UNIT,'(9A)',IOSTAT=IER)
     &          3,DESCRIP,EXDATE,EXTIME,SYSTEM,CLI$PRESENT('BROADCAST'),
     &          CLI$PRESENT('BELL'),CLI$PRESENT('ALL'),
     &          CLI$PRESENT('CLUSTER')
           END IF
           IF (IER.EQ.0) THEN
              READ(REMOTE_UNIT,'(Q,A)',IOSTAT=IER) I,FOLDER1_COM
           END IF
           IF (IER.EQ.0) THEN
              IF (I.EQ.LEN(FOLDER1_COM)) THEN
                 IER = SYS$ASCTIM(,TODAY_TIME,F1_NEWEST_BTIM,)
                 NEWEST_DATE = TODAY_TIME(1:11)
                 NEWEST_TIME = TODAY_TIME(13:)
                 NBULL = F1_NBULL
                 CALL UPDATE_FOLDER
              ELSE
                 WRITE (6,'(1X,A)') FOLDER1_COM(:I)
              END IF
           ELSE
              CALL DISCONNECT_REMOTE
           END IF
           CALL UPDATE_LOGIN(.TRUE.)
           RETURN
        END IF

        CALL SYS$ASCTIM(,TODAY_TIME,,)
        DATE = TODAY_TIME(1:11)
        TIME = TODAY_TIME(13:)

        CALL READDIR(0,IER)

        IF (IER.NE.1) THEN
           NEWEST_EXDATE = '5-NOV-2000'
           NEWEST_EXTIME = '00:00:00.00'
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
C       BTIM1  -  First time in binary format
C       BTIM2  -  Second time in binary format
C  OUTPUT:
C       Returns +1 if first time is farther in future
C       Returns -1 if second time is farther in future
C       Returns 0 if equal time
C
        IMPLICIT INTEGER (A - Z)

        DIMENSION BTIM1(2),BTIM2(2),DIFF(2)

        CALL LIB$SUBX(BTIM1,BTIM2,DIFF)

        IF (DIFF(2).LT.0) THEN
           COMPARE_BTIM = -1
        ELSE IF (DIFF(2).GE.0) THEN
           COMPARE_BTIM = +1
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
C       DATE1  -  First date  (dd-mm-yy)
C       DATE2  -  Second date (If is equal to ' ', then use present date)
C  OUTPUT:
C       Returns the difference in days between the two dates.
C       If the DATE1 is farther in the future, the output is positive,
C       else it is negative.
C
        IMPLICIT INTEGER (A - Z)

        CHARACTER*(*) DATE1,DATE2
        INTEGER USER_TIME(2)

        CALL SYS_BINTIM(DATE1,USER_TIME)

        CALL VERIFY_DATE(USER_TIME)
C
C  LIB$DAY crashes if date invalid, which happened once due to an unknown
C  hardware or software error which created a date very far in the future.
C
        CALL LIB$DAY(DAY1,USER_TIME)

        IF (DATE2.NE.' ') THEN
           CALL SYS_BINTIM(DATE2,USER_TIME)
           CALL VERIFY_DATE(USER_TIME)
        ELSE
           CALL SYS$GETTIM(USER_TIME)
        END IF

        CALL LIB$DAY(DAY2,USER_TIME)

        COMPARE_DATE = DAY1 - DAY2

        RETURN
        END



        SUBROUTINE VERIFY_DATE(BTIM)

        IMPLICIT INTEGER (A-Z)

        DIMENSION BTIM(2),TEMP(2)

        CALL SYS_BINTIM(' 5-NOV-2011 00:00:00.00',TEMP)

        IER = COMPARE_BTIM(BTIM,TEMP)

        IF (IER.GT.0) THEN              ! Date invalid
           BTIM(1) = TEMP(1)
           BTIM(2) = TEMP(2)
        END IF

        CALL SYS_BINTIM(' 5-NOV-1955 00:00:00.00',TEMP)

        IER = COMPARE_BTIM(BTIM,TEMP)

        IF (IER.LT.0) THEN              ! Date invalid
           BTIM(1) = TEMP(1)
           BTIM(2) = TEMP(2)
        END IF

        RETURN
        END



        INTEGER FUNCTION COMPARE_TIME(TIME1,TIME2)
C
C  FUNCTION COMPARE_TIME
C
C  FUCTION: Compares times to see which is farther in future.
C
C  INPUTS:
C       TIME1  -  First time    (hh:mm:ss.xx)
C       TIME2  -  Second time
C  OUTPUT:
C       Outputs (TIME1-TIME2) in seconds.  Thus, if TIME1 is further
C       in the future, outputs positive number, else negative.
C

        IMPLICIT INTEGER (A-Z)
        CHARACTER*(*) TIME1,TIME2
        CHARACTER*23 TODAY_TIME
        CHARACTER*11 TEMP2

        IF (TIME2.EQ.' ') THEN
           CALL SYS$ASCTIM(,TODAY_TIME,,)
           TEMP2 = TODAY_TIME(13:)
        ELSE
           TEMP2 = TIME2
        END IF

        COMPARE_TIME = 3600*10*(ICHAR(TIME1(1:1))-ICHAR(TEMP2(1:1)))
     &                   +3600*(ICHAR(TIME1(2:2))-ICHAR(TEMP2(2:2)))
     &                  +60*10*(ICHAR(TIME1(4:4))-ICHAR(TEMP2(4:4)))
     &                     +60*(ICHAR(TIME1(5:5))-ICHAR(TEMP2(5:5)))
     &                     +10*(ICHAR(TIME1(7:7))-ICHAR(TEMP2(7:7)))
     &                        +(ICHAR(TIME1(8:8))-ICHAR(TEMP2(8:8)))

        IF (COMPARE_TIME.EQ.0) THEN
           COMPARE_TIME = 10*(ICHAR(TIME1(10:10))-ICHAR(TEMP2(10:10)))
     &                      +(ICHAR(TIME1(11:11))-ICHAR(TEMP2(11:11)))
           IF (COMPARE_TIME.GT.0) THEN
              COMPARE_TIME = 1
           ELSE IF (COMPARE_TIME.LT.0) THEN
              COMPARE_TIME = -1
           END IF
        END IF

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
        INTEGER HEADER
        IF (HEADER.NE.0) RETURN         ! Queue already initialized
        LENGTH = LEN(DATA)
        IF (MOD(LENGTH,4).NE.0) LENGTH = LENGTH + 4 - MOD(LENGTH,4)
        CALL LIB$GET_VM(LENGTH+12,HEADER)
        CALL MAKE_CHAR(%VAL(HEADER),LEN(DATA),LENGTH)
        RETURN
        END


        SUBROUTINE WRITE_QUEUE(RECORD,NEXT,DATA)
        INTEGER RECORD(1)
        CHARACTER*(*) DATA
        LENGTH = RECORD(1)
        CALL COPY_CHAR(LENGTH,DATA,%VAL(%LOC(RECORD)))
        IF (MOD(LENGTH,4).NE.0) LENGTH = LENGTH + 4 - MOD(LENGTH,4)
        NEXT = RECORD((LENGTH+12)/4)
        IF (NEXT.NE.0) RETURN
        CALL LIB$GET_VM(LENGTH+12,NEXT)
        CALL MAKE_CHAR(%VAL(NEXT),RECORD(1),LENGTH)
        RECORD((LENGTH+12)/4) = NEXT
        RETURN
        END

        SUBROUTINE READ_QUEUE(RECORD,NEXT,DATA)
        CHARACTER*(*) DATA
        INTEGER RECORD(1)
        LENGTH = RECORD(1)
        CALL COPY_CHAR(LENGTH,%VAL(%LOC(RECORD)),DATA)
        IF (MOD(LENGTH,4).NE.0) LENGTH = LENGTH + 4 - MOD(LENGTH,4)
        NEXT = RECORD((LENGTH+12)/4)
        RETURN
        END

        SUBROUTINE COPY_CHAR(LENGTH,INCHAR,OUTCHAR)
        CHARACTER*(*) INCHAR,OUTCHAR
        OUTCHAR = INCHAR(:LENGTH)
        RETURN
        END

        SUBROUTINE MAKE_CHAR(IARRAY,CHAR_LEN,REAL_LEN)
        IMPLICIT INTEGER (A-Z)
        DIMENSION IARRAY(1)
        IARRAY(1) = CHAR_LEN
        IARRAY(2) = %LOC(IARRAY(3))
        IARRAY(REAL_LEN/4+3) = 0
        RETURN
        END



        SUBROUTINE DISABLE_PRIVS
C
C  SUBROUTINE DISABLE_PRIVS
C
C  FUNCTION: Disable image high privileges.
C

        IMPLICIT INTEGER (A-Z)

        INCLUDE '($PRVDEF)'

        COMMON /PRIVS/ SETPRV,PRV_DEPTH
        DIMENSION SETPRV(2)

        DATA PRV_DEPTH /0/

        COMMON /REALPROC/ REALPROCPRIV(2)

        PRV_DEPTH = PRV_DEPTH + 1

        IF (PRV_DEPTH.GT.1) RETURN

        CALL SYS$SETPRV(%VAL(0),,,SETPRV)       ! Get privileges

        SETPRV(1) = SETPRV(1).AND..NOT.REALPROCPRIV(1)

        CALL SYS$SETPRV(%VAL(0),SETPRV,,)       ! Disable installed privs

        RETURN
        END



        SUBROUTINE ENABLE_PRIVS
C
C  SUBROUTINE ENABLE_PRIVS
C
C  FUNCTION: Enable image high privileges.
C

        IMPLICIT INTEGER (A-Z)

        COMMON /PRIVS/ SETPRV,PRV_DEPTH
        DIMENSION SETPRV(2)

        PRV_DEPTH = PRV_DEPTH - 1

        IF (PRV_DEPTH.GT.1) RETURN

        CALL SYS$SETPRV(%VAL(1),SETPRV,,)       ! Enable image privs

        RETURN
        END



        SUBROUTINE CHECK_PRIV_IO(ERROR)
C
C  SUBROUTINE CHECK_PRIV_IO
C
C  FUNCTION: Checks SYS$OUTPUT and SYS$ERROR to see if they need
C       privileges to output to.
C

        IMPLICIT INTEGER (A-Z)

        CALL DISABLE_PRIVS                      ! Disable SYSPRV 

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

        CALL ENABLE_PRIVS                       ! Enable SYSPRV 

100     FORMAT(1X,'ERROR: SYS$OUTPUT cannot be opened.')
200     FORMAT(1X,'ERROR: SYS$ERROR cannot be opened.')

        RETURN
        END


        SUBROUTINE CHANGE_FLAG(CMD,FLAG)
C
C  SUBROUTINE CHANGE_FLAG
C
C  FUNCTION: Sets flags for specified folder.
C
C  INPUTS:
C       CMD    -   LOGICAL*4 value. If TRUE, set flag. 
C                  If FALSE, clear flag.
C       FLAG    -  If 1, modify NEW_FLAG, if 2, modify SET_FLAG
C                  If 3, modify BRIEF_FLAG, 4, modify NOTIFY_FLAG
C
        IMPLICIT INTEGER (A - Z)

        INCLUDE 'BULLDIR.INC'

        INCLUDE 'BULLUSER.INC'

        INCLUDE 'BULLFOLDER.INC'

        DIMENSION FLAGS(FLONG,4)
        EQUIVALENCE (NEW_FLAG(1),FLAGS(1,1))

        LOGICAL CMD

        DIMENSION READ_BTIM_SAVE(2)

        DATA CHANGE_FOLDER /.FALSE./

        IF (CLI$PRESENT('FOLDER')) THEN
           IER = CLI$GET_VALUE('FOLDER',FOLDER1)
           IF (IER) THEN
              FOLDER_NUMBER_SAVE = FOLDER_NUMBER
              CALL OPEN_BULLFOLDER_SHARED
              CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER)
              CALL CLOSE_BULLFOLDER
              IF (IER.NE.0) THEN
                 WRITE (6,'('' ERROR: No such folder found.'')')
                 RETURN
              END IF
           END IF
           FOLDER_NUMBER = FOLDER1_NUMBER
           CHANGE_FOLDER = .TRUE.
        END IF

C
C  Find user entry in BULLUSER.DAT to update information.
C

        ENTRY CHANGE_FLAG_NOCMD(CMD,FLAG)

        CALL OPEN_BULLUSER_SHARED               ! Open user file

        READ_BTIM_SAVE(1) = READ_BTIM(1)
        READ_BTIM_SAVE(2) = READ_BTIM(2)

        CALL READ_USER_FILE_KEYNAME(USERNAME,IER)       ! Read old entry

        IF (IER.GT.0) THEN              ! No entry (how did this happen??)
           CALL SYS_BINTIM('-',LOGIN_BTIM)      ! Get today's today
           CALL SYS_BINTIM('5-NOV-1956 11:05:56',READ_BTIM)     ! Fake new entry
           CALL READ_USER_FILE_HEADER(IER)
           IF (CMD) THEN
              CALL SET2(FLAGS(1,FLAG),FOLDER_NUMBER)
           ELSE
              CALL CLR2(FLAGS(1,FLAG),FOLDER_NUMBER)
           END IF
           NEW_FLAG(1) = 143
           NEW_FLAG(2) = 0
           CALL WRITE_USER_FILE_NEW(IER)
        ELSE
           IF (CMD) THEN
              CALL SET2(FLAGS(1,FLAG),FOLDER_NUMBER)
           ELSE
              CALL CLR2(FLAGS(1,FLAG),FOLDER_NUMBER)
           END IF
           NEW_FLAG(1) = 143
           REWRITE (4,IOSTAT=IER) USER_ENTRY
           READ_BTIM(1) = READ_BTIM_SAVE(1)
           READ_BTIM(2) = READ_BTIM_SAVE(2)
        END IF

        CALL CLOSE_FILE (4)

        IF (FLAG.EQ.4) THEN                     ! If notify, see if cluster
           IER = SYS_TRNLNM('BULL_SYSTEM_FLAGS',TEMP_USER)
           IF (.NOT.IER) THEN
              IER = SYS_TRNLNM('MAIL$SYSTEM_FLAGS',TEMP_USER)
           END IF
           READ (TEMP_USER(:1),'(I1)',IOSTAT=IER) BFLAG
           IF (BTEST(BFLAG,1).AND.IER.EQ.0) THEN
              CALL OPEN_BULLNOTIFY_SHARED
              DO WHILE (REC_LOCK(IER))
                 READ (10,IOSTAT=IER) TEMP_USER
              END DO
              IF (TEMP_USER.NE.'*') THEN
                 IF (CMD) THEN
                    WRITE (10,IOSTAT=IER) USERNAME
                 ELSE
                    DO WHILE (REC_LOCK(IER))
                       READ (10,KEY=USERNAME,IOSTAT=IER)
                    END DO
                    IF (IER.EQ.0) DELETE (UNIT=10)
                 END IF
              END IF
              CALL CLOSE_BULLNOTIFY
           END IF
        END IF

        IF (CHANGE_FOLDER) THEN
           FOLDER_NUMBER = FOLDER_NUMBER_SAVE
           CHANGE_FOLDER = .FALSE.
        END IF

        RETURN

        END




        SUBROUTINE SET_VERSION
C
C  SUBROUTINE SET_VERSION
C
C  FUNCTION: Sets version number.
C
        IMPLICIT INTEGER (A - Z)

        INCLUDE 'BULLDIR.INC'

        INCLUDE 'BULLUSER.INC'

        INCLUDE 'BULLFOLDER.INC'

        DIMENSION FLAGS(FLONG,4)
        EQUIVALENCE (NEW_FLAG(1),FLAGS(1,1))

        LOGICAL CMD

        DIMENSION READ_BTIM_SAVE(2)

C
C  Find user entry in BULLUSER.DAT to update information.
C

        CALL OPEN_BULLUSER_SHARED               ! Open user file

        READ_BTIM_SAVE(1) = READ_BTIM(1)
        READ_BTIM_SAVE(2) = READ_BTIM(2)

        CALL READ_USER_FILE_KEYNAME(USERNAME,IER)       ! Read old entry

        IF (IER.EQ.0) THEN
           NEW_FLAG(1) = 143
           REWRITE (4,IOSTAT=IER) USER_ENTRY  ! Write modified entry
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
C       USERNAME  -  Username
C  OUTPUTS:
C       ALLOW     -  Returns 1 if account has SETPRV.
C                    returns 0 if account has no SETPRV.
C

        IMPLICIT INTEGER (A-Z)

        CHARACTER*(*) USERNAME

        INCLUDE '($PRVDEF)'

        INCLUDE '($UAIDEF)'

        INTEGER DEF_PRIV(2)

        CALL INIT_ITMLST
        CALL ADD_2_ITMLST(8,UAI$_DEF_PRIV,%LOC(DEF_PRIV))
        CALL END_ITMLST(GETUAI_ITMLST)

        ALLOW = 0                                       ! Set return false
        IER = SYS$GETUAI(,,USERNAME,%VAL(GETUAI_ITMLST),,,)     ! Read Record
        IF (IER) THEN                                   ! If username found
           IF (BTEST(DEF_PRIV(1),PRV$V_SETPRV).OR.      ! SETPRV or CMRKNL
     &         BTEST(DEF_PRIV(1),PRV$V_CMKRNL)) THEN    ! privileges?
              ALLOW = 1                                 ! Yep
           END IF
        END IF

        RETURN                                          ! Return
        END                                             ! End





        SUBROUTINE CHECK_NEWUSER(USERNAME,DISMAIL,PASSCHANGE)
C
C  SUBROUTINE CHECK_NEWUSER
C
C  FUNCTION: Checks flags for a new: Whether DISMAIL is set,
C               and what the last password change was.
C
C  INPUTS:
C       USERNAME  -  Username
C  OUTPUTS:
C       DISMAIL     -  Returns 1 if account has DISMAIL.
C                      returns 0 if account has no DISMAIL.
C       PASSCHANGE  -  Date of last password change.
C

        IMPLICIT INTEGER (A-Z)

        CHARACTER*(*) USERNAME

        INTEGER PASSCHANGE(2)

        INCLUDE '($UAIDEF)'

        CALL INIT_ITMLST
        CALL ADD_2_ITMLST(4,UAI$_FLAGS,%LOC(FLAGS))
        CALL ADD_2_ITMLST(8,UAI$_PWD_DATE,%LOC(PASSCHANGE))
        CALL END_ITMLST(GETUAI_ITMLST)

        DISMAIL = 0                                     ! Set return false
        IER = SYS$GETUAI(,,USERNAME,%VAL(GETUAI_ITMLST),,,)     ! Read Record
        IF (IER) THEN                                   ! If username found
           IF (BTEST(FLAGS,UAI$V_NOMAIL)) THEN          ! DISMAIL SET?
              DISMAIL = 1                               ! Yep
           END IF
        END IF

        RETURN                                          ! Return
        END                                             ! End



        INTEGER FUNCTION SYS_TRNLNM(INPUT,OUTPUT)

        IMPLICIT INTEGER (A-Z)

        CHARACTER*(*) INPUT,OUTPUT

        PARAMETER LNM$_STRING = '2'X

        CALL INIT_ITMLST        ! Initialize item list
        CALL ADD_2_ITMLST_WITH_RET
     &          (LEN(OUTPUT),LNM$_STRING,%LOC(OUTPUT),%LOC(OLEN))
        CALL END_ITMLST(TRNLNM_ITMLST)  ! Get address of itemlist

        SYS_TRNLNM = SYS$TRNLNM(,'LNM$FILE_DEV',INPUT(:TRIM(INPUT)),,
     &          %VAL(TRNLNM_ITMLST))

        IF (SYS_TRNLNM) OUTPUT = OUTPUT(:OLEN)

        RETURN
        END




        INTEGER FUNCTION SYS_TRNLNM_SYSTEM(INPUT,OUTPUT)

        IMPLICIT INTEGER (A-Z)

        CHARACTER*(*) INPUT,OUTPUT

        PARAMETER LNM$_STRING = '2'X

        CALL INIT_ITMLST        ! Initialize item list
        CALL ADD_2_ITMLST_WITH_RET
     &          (LEN(OUTPUT),LNM$_STRING,%LOC(OUTPUT),%LOC(OLEN))
        CALL END_ITMLST(TRNLNM_ITMLST)  ! Get address of itemlist

        SYS_TRNLNM_SYSTEM = SYS$TRNLNM(,'LNM$SYSTEM',
     &          INPUT(:TRIM(INPUT)),,%VAL(TRNLNM_ITMLST))

        IF (SYS_TRNLNM_SYSTEM) OUTPUT = OUTPUT(:OLEN)

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
                 CALL WAIT_SEC('01')
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

        COMMON /DEF_PROT/ ORIGINAL_DEF_PROT

        QUIT = 1

        ENTRY ENABLE_CTRL_EXIT

        QUIT = QUIT.AND.1               ! If called via entry, QUIT = 0
        IF (QUIT.EQ.1) LEVEL = LEVEL - 1

        IF (LEVEL.LT.0.AND.QUIT.EQ.1) THEN
           WRITE (6,'('' ERROR: Error in CTRL.'')')
        END IF

        IF (LEVEL.EQ.0.OR.QUIT.EQ.0) THEN
           CALL LIB$ENABLE_CTRL(CTRLY,) ! Enable CTRL-Y & -C
        END IF

        IF (QUIT.EQ.0) THEN
           CALL UPDATE_USERINFO
           CALL SYS$SETDFPROT(ORIGINAL_DEF_PROT,)
           CALL EXIT
        END IF
        QUIT = 0                        ! Reinitialize

        RETURN
        END


        SUBROUTINE DISABLE_CTRL

        IMPLICIT INTEGER (A-Z)

        COMMON /CTRLY/ CTRLY

        COMMON /CTRL_LEVEL/ LEVEL
        DATA LEVEL /0/

        IF (LEVEL.EQ.0) CALL LIB$DISABLE_CTRL(CTRLY,)
        LEVEL = LEVEL + 1

        RETURN
        END




        SUBROUTINE CLEANUP_BULLFILE
C
C  SUBROUTINE CLEANUP_BULLFILE
C
C  FUNCTION:  Searches for empty space in bulletin file and deletes it.
C
        IMPLICIT INTEGER (A - Z)

        INCLUDE 'BULLFILES.INC'

        INCLUDE 'BULLDIR.INC'

        INCLUDE 'BULLFOLDER.INC'

        CHARACTER FILENAME*132,BUFFER*128

        CALL OPEN_BULLDIR_SHARED

C
C  NOTE: Can't use READDIR for reading header since it'll spawn a 
C  BULL/CLEANUP.  (Fooey).
C

        DO WHILE (REC_LOCK(IER))
           READ (2,KEYID=0,KEY=HEADER_KEY,IOSTAT=IER) BULLDIR_HEADER
        END DO

        IF (NEMPTY.EQ.0) THEN           ! No cleanup necessary
         CALL CLOSE_BULLDIR
         RETURN
        ELSE IF (NEMPTY.GT.0) THEN

         OPEN (UNIT=11,FILE=FOLDER_FILE(:TRIM(FOLDER_FILE))//'.TMPFIL',
     1        STATUS='UNKNOWN',IOSTAT=IER,DISPOSE='DELETE',
     1        RECORDTYPE='FIXED',RECORDSIZE=32,
     1        FORM='UNFORMATTED',INITIALSIZE=((NBLOCK-NEMPTY)*128)/512)
                                ! Compressed version is number 1

         IF (IER.NE.0) THEN
            OPEN (UNIT=11,
     1        FILE=FOLDER_FILE(:TRIM(FOLDER_FILE))//'.TMPFIL',
     1        STATUS='UNKNOWN',IOSTAT=IER,DISPOSE='DELETE',
     1        RECORDTYPE='FIXED',RECORDSIZE=32,
     1        FORM='UNFORMATTED')
            IF (IER.NE.0) THEN
               CALL CLOSE_BULLDIR
               RETURN
            END IF
         END IF

         CALL OPEN_BULLFIL_SHARED               ! Open bulletin file

         NBLOCK = 0

         DO I=1,NBULL                           ! Copy bulletins to new file
           CALL READDIR(I,IER)
           ICOUNT = BLOCK
           DO J=1,LENGTH
              NBLOCK = NBLOCK + 1
              DO WHILE (REC_LOCK(IER1))
                 READ(1'ICOUNT,IOSTAT=IER1) BUFFER
              END DO
              IF (IER1.NE.0) THEN               ! This file is corrupt
                 NBLOCK = NBLOCK - 1
                 NBULL = I - 1
                 GO TO 100
              END IF
              WRITE(11) BUFFER
              ICOUNT = ICOUNT + 1
           END DO
         END DO

100      CALL CLOSE_BULLFIL
        ELSE IF (NEMPTY.EQ.-1) THEN
         CALL CLOSE_BULLDIR
         CALL OPEN_BULLDIR      ! Open with no sharing
         IER = LIB$RENAME_FILE(FOLDER_FILE(:TRIM(FOLDER_FILE))//'.TMPFIL',
     &                          '*.BULLFIL')
         IER = 1
         DO WHILE (IER)
            IER = LIB$DELETE_FILE(FOLDER_FILE(:TRIM(FOLDER_FILE))//
     &                          '.BULLFIL;-1')
         END DO
         IER = LIB$RENAME_FILE(FOLDER_FILE(:TRIM(FOLDER_FILE))//'.TMPDIR',
     &                          '*.BULLDIR')
         CALL CLOSE_BULLDIR_DELETE
         IER = 1
         DO WHILE (IER)
            IER = LIB$DELETE_FILE(FOLDER_FILE(:TRIM(FOLDER_FILE))//
     &                          '.BULLDIR;-1')
         END DO
         IER = LIB$RENAME_FILE(FOLDER_FILE(:TRIM(FOLDER_FILE))//'.BULL*',
     &                          '*.*;1')
         RETURN
        END IF

        OPEN (UNIT=12,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     &        //'.TMPDIR',STATUS='UNKNOWN',FORM='UNFORMATTED',
     &        RECORDTYPE='FIXED',RECORDSIZE=DIR_RECORD_LENGTH/4,
     &        ORGANIZATION='INDEXED',IOSTAT=IER,DISPOSE='DELETE',
     &        KEY=(9:12:INTEGER,1:8:CHARACTER),ACCESS='KEYED',
     &        INITIALSIZE=(((NBULL+1)*DIR_RECORD_LENGTH)/512)+1 )

        IF (IER.NE.0) THEN
           OPEN (UNIT=12,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     &        //'.TMPDIR',STATUS='UNKNOWN',FORM='UNFORMATTED',
     &        RECORDTYPE='FIXED',RECORDSIZE=DIR_RECORD_LENGTH/4,
     &        ORGANIZATION='INDEXED',IOSTAT=IER,DISPOSE='DELETE',
     &        KEY=(9:12:INTEGER,1:8:CHARACTER),ACCESS='KEYED')
            IF (IER.NE.0) THEN
               CLOSE (UNIT=11)
               CALL CLOSE_BULLDIR
               RETURN
            END IF
        END IF

        NEMPTY = 0
        WRITE (12,IOSTAT=IER) BULLDIR_HEADER    ! Write directory header

        NBLOCK = 0              ! Update directory entry pointers
        DO I=1,NBULL
           CALL READDIR(I,IER)
           BLOCK = NBLOCK + 1
           CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
           WRITE (12,IOSTAT=IER) BULLDIR_ENTRY
           NBLOCK = NBLOCK + LENGTH
        END DO

        CLOSE (UNIT=12,STATUS='KEEP')
        CLOSE (UNIT=11,STATUS='KEEP')

        CALL CLOSE_BULLDIR
        CALL OPEN_BULLDIR       ! Open with no sharing

        NEMPTY = -1             ! Copying done, indicate that in case of crash
        WRITE (2,IOSTAT=IER) BULLDIR_HEADER ! Write new directory header

        IER = LIB$RENAME_FILE(FOLDER_FILE(:TRIM(FOLDER_FILE))//'.TMPFIL',
     &                          '*.BULLFIL')
        IER = 1
        DO WHILE (IER)
           IER = LIB$DELETE_FILE(FOLDER_FILE(:TRIM(FOLDER_FILE))//
     &                          '.BULLFIL;-1')
        END DO
        IER = LIB$RENAME_FILE(FOLDER_FILE(:TRIM(FOLDER_FILE))//'.TMPDIR',
     &                          '*.BULLDIR')
        CALL CLOSE_BULLDIR_DELETE
        IER = 1
        DO WHILE (IER)
           IER = LIB$DELETE_FILE(FOLDER_FILE(:TRIM(FOLDER_FILE))//
     &                          '.BULLDIR;-1')
        END DO
        IER = LIB$RENAME_FILE(FOLDER_FILE(:TRIM(FOLDER_FILE))//'.BULL*',
     &                          '*.*;1')

        RETURN
        END




        SUBROUTINE CLEANUP_DIRFILE(DELETE_ENTRY)
C
C  SUBROUTINE CLEANUP_DIRFILE
C
C  FUNCTION:  Reorder directory file after deletions.
C             Is called either directly after a deletion, or is
C             called if it is detected that a deletion was not fully
C             completed due to the fact that the deleting process
C             was abnormally terminated.
C
        IMPLICIT INTEGER (A - Z)

        INCLUDE 'BULLFILES.INC'

        INCLUDE 'BULLDIR.INC'

        CHARACTER*(DIR_RECORD_LENGTH) BULLDIR_ENTRY_SAVE

        CHARACTER*11 DATE_SAVE,EXDATE_SAVE
        CHARACTER*11 TIME_SAVE,EXTIME_SAVE

        BULLDIR_ENTRY_SAVE = BULLDIR_ENTRY
        DATE_SAVE = DATE
        TIME_SAVE = TIME
        EXDATE_SAVE = EXDATE
        EXTIME_SAVE = EXTIME

        NBULL = -NBULL          ! Negative # Bulls signals deletion in progress
        MOVE_TO = 0             ! Moving directory entries starting here
        MOVE_FROM = 0           ! Moving directory entries from here
        I = DELETE_ENTRY        ! Start search point for first deleted entries
        DO WHILE (MOVE_TO.EQ.0.AND.I.LE.NBULL)
           CALL READDIR(I,IER)
           IF (IER.NE.I+1) THEN ! Have we found a deleted entry?
              MOVE_TO = I       ! If so, start moving entries to here
              J=I+1             ! Search for next entry in file
              DO WHILE (MOVE_FROM.EQ.0.AND.J.LE.NBULL)
                 CALL READDIR(J,IER)
                 IF (IER.EQ.J+1) MOVE_FROM = J
                 J = J + 1
              END DO
              IF (MOVE_FROM.EQ.0) THEN  ! There are no more entries
                 NBULL = I - 1          ! so just update number of bulletins
                 CALL WRITEDIR(0,IER)
                 RETURN
              END IF
              LENGTH = -LENGTH          ! Indicate starting point by writing
              CALL WRITEDIR(I,IER)      ! next entry into deleted entry
              FIRST_DELETE = I          ! with negative length
              MOVE_FROM = MOVE_FROM + 1 ! Set up pointers to move rest of
              MOVE_TO = MOVE_TO + 1     ! the entries
           ELSE IF (LENGTH.LT.0) THEN   ! If negative length found, deletion
              FIRST_DELETE = I          ! was previously in progress
              J = I                     ! Try to find where entry came from
              CALL INIT_QUEUE(ENTRY_Q1,BULLDIR_ENTRY)
              ENTRY_Q = ENTRY_Q1
              DO K=J,NBULL
                 CALL READDIR(K,IER)
                 IF (IER.EQ.K+1) THEN
                    CALL WRITE_QUEUE(%VAL(ENTRY_Q),ENTRY_Q,BULLDIR_ENTRY)
                 END IF
              END DO
              ENTRY_QLAST = ENTRY_Q
              ENTRY_Q2 = ENTRY_Q1
              DO WHILE (MOVE_FROM.EQ.0.AND.ENTRY_Q2.NE.ENTRY_QLAST)
                 CALL READ_QUEUE(%VAL(ENTRY_Q2),ENTRY_Q,BULLDIR_ENTRY)
                 ENTRY_Q2 = ENTRY_Q
                 BLOCK_SAVE = BLOCK
                 MSG_NUM_SAVE = MSG_NUM
                 DO WHILE (MOVE_FROM.EQ.0.AND.ENTRY_Q.NE.ENTRY_QLAST)
                                                ! Search for duplicate entries
                    CALL READ_QUEUE(%VAL(ENTRY_Q),ENTRY_Q,BULLDIR_ENTRY)
                    IF (BLOCK_SAVE.EQ.BLOCK) THEN
                       MOVE_TO = MSG_NUM_SAVE + 1
                       MOVE_FROM = MSG_NUM + 1
                    END IF
                 END DO
                                        ! If no duplicate entry found for this
                                        ! entry, see if one exists for any
              END DO                    ! of the other entries
           END IF
           I = I + 1
        END DO

        IF (I.LE.NBULL) THEN            ! Move reset of entries if necessary
           IF (MOVE_FROM.GT.0) THEN
              DO J=MOVE_FROM,NBULL
                 CALL READDIR(J,IER)
                 IF (IER.EQ.J+1) THEN   ! Skip any other deleted entries
                    CALL WRITEDIR(MOVE_TO,IER)
                    MOVE_TO = MOVE_TO + 1
                 END IF
              END DO
           END IF
           DO J=MOVE_TO,NBULL           ! Delete empty records at end of file
              CALL READDIR(J,IER)
              DELETE(UNIT=2,IOSTAT=IER)
           END DO
           NBULL = MOVE_TO - 1          ! Update # bulletin count
        END IF

        CALL READDIR(FIRST_DELETE,IER)
        IF (IER.EQ.FIRST_DELETE+1.AND.LENGTH.LT.0) THEN
           LENGTH = -LENGTH             ! Fix entry which has negative length
           CALL WRITEDIR(FIRST_DELETE,IER)
        END IF

        CALL WRITEDIR(0,IER)

        BULLDIR_ENTRY = BULLDIR_ENTRY_SAVE
        DATE = DATE_SAVE
        TIME = TIME_SAVE
        EXDATE = EXDATE_SAVE
        EXTIME = EXTIME_SAVE

        RETURN
        END


        SUBROUTINE SHOW_FLAGS
C
C  SUBROUTINE SHOW_FLAGS
C
C  FUNCTION: Show user flags.
C
        IMPLICIT INTEGER (A - Z)

        INCLUDE 'BULLDIR.INC'

        INCLUDE 'BULLUSER.INC'

        INCLUDE 'BULLFOLDER.INC'

C
C  Find user entry in BULLUSER.DAT to obtain flags.
C

        CALL OPEN_BULLUSER_SHARED               ! Open user file

        CALL READ_USER_FILE_KEYNAME(USERNAME,IER)       ! Read old entry

        WRITE (6,'('' For the selected folder '',A)') FOLDER(1:TRIM(FOLDER))
        
        IF (TEST2(NOTIFY_FLAG,FOLDER_NUMBER)) THEN
           WRITE (6,'('' NOTIFY is set.'')')
        END IF

        IF (TEST2(SET_FLAG,FOLDER_NUMBER).AND.
     &     (.NOT.TEST2(BRIEF_FLAG,FOLDER_NUMBER))) THEN
           WRITE (6,'('' READNEW is set.'')')
        ELSE IF (TEST2(BRIEF_FLAG,FOLDER_NUMBER).AND.
     &         TEST2(SET_FLAG,FOLDER_NUMBER)) THEN
           WRITE (6,'('' BRIEF is set.'')')
        ELSE IF (TEST2(BRIEF_FLAG,FOLDER_NUMBER).AND.
     &         .NOT.TEST2(SET_FLAG,FOLDER_NUMBER)) THEN
           WRITE (6,'('' SHOWNEW is set.'')')
        ELSE IF (.NOT.TEST2(NOTIFY_FLAG,FOLDER_NUMBER)) THEN
           WRITE (6,'('' No flags are set.'')')
        END IF

        CALL CLOSE_BULLUSER

        RETURN
        END


        SUBROUTINE SET2(FLAG,NUMBER)

        IMPLICIT INTEGER (A-Z)

        INTEGER FLAG(2)

        F_POINT = NUMBER/32 + 1
        FLAG(F_POINT) = IBSET(FLAG(F_POINT),NUMBER-32*(F_POINT-1))

        RETURN
        END


        SUBROUTINE CLR2(FLAG,NUMBER)

        IMPLICIT INTEGER (A-Z)

        INTEGER FLAG(3)

        F_POINT = NUMBER/32 + 1
        FLAG(F_POINT) = IBCLR(FLAG(F_POINT),NUMBER-32*(F_POINT-1))

        RETURN
        END



        LOGICAL FUNCTION TEST2(FLAG,NUMBER)

        IMPLICIT INTEGER (A-Z)

        INTEGER FLAG(3)

        F_POINT = NUMBER/32 + 1
        TEST2 = BTEST(FLAG(F_POINT),NUMBER-32*(F_POINT-1))

        RETURN
        END




        INTEGER FUNCTION GETUSERS(USERNAME,TERMINAL)
C
C  FUNCTION GETUSERS
C
C  FUNCTION:
C       To get names of all users that are logged in.
C

        IMPLICIT INTEGER (A-Z)

        INCLUDE '($JPIDEF)'

        CHARACTER USERNAME*(*),TERMINAL*(*)

        DATA WILDCARD /-1/

        CALL INIT_ITMLST        ! Initialize item list
                                ! Now add items to list
        CALL ADD_2_ITMLST(LEN(USERNAME),JPI$_USERNAME,%LOC(USERNAME))
        CALL ADD_2_ITMLST(LEN(TERMINAL),JPI$_TERMINAL,%LOC(TERMINAL))
        CALL END_ITMLST(GETJPI_ITMLST)  ! Get address of itemlist

        IER = 1
        TERMINAL(1:1) = CHAR(0)
        DO WHILE (IER.AND.TERMINAL(1:1).EQ.CHAR(0))
                                                ! Get next interactive process
           IER = SYS$GETJPIW(,WILDCARD,,%VAL(GETJPI_ITMLST),,,,)
                                                ! Get next process.
        END DO

        IF (.NOT.IER) WILDCARD = -1

        GETUSERS = IER

        RETURN
        END





        SUBROUTINE OPEN_USERINFO
C
C  SUBROUTINE OPEN_USERINFO
C
C  FUNCTION:  Opens the file in SYS$LOGIN which contains user information.
C
        IMPLICIT INTEGER (A - Z)

        INCLUDE 'BULLUSER.INC'

        COMMON /USERINFO/ USERINFO_READ
        DATA USERINFO_READ /.FALSE./

        CALL OPEN_BULLINF_SHARED

        READ (9,KEY=USERNAME,IOSTAT=IER) USERNAME,
     &    ((LAST_READ_BTIM(1,I),LAST_READ_BTIM(2,I)),I=1,FOLDER_MAX)

        IF (IER.NE.0.AND.TEST_BULLCP().EQ.2     ! Is this BULLCP process?
     &      .AND.CONFIRM_USER(USERNAME).NE.0) THEN      ! Not real user?
           USERNAME = 'DECNET'
           READ (9,KEY=USERNAME,IOSTAT=IER) USERNAME,
     &       ((LAST_READ_BTIM(1,I),LAST_READ_BTIM(2,I)),I=1,FOLDER_MAX)
        END IF

        IF (IER.NE.0) THEN
           OPEN (UNIT=10,FILE='SYS$LOGIN:BULLETIN.INF',STATUS='OLD',
     &        RECORDTYPE='FIXED',FORM='UNFORMATTED',IOSTAT=IER)
           INQUIRE(UNIT=10,RECORDSIZE=INF_SIZE)
           IF (IER.EQ.0) THEN
              READ (10)
     &    ((LAST_READ_BTIM(1,I),LAST_READ_BTIM(2,I)),I=1,INF_SIZE/2)
              CLOSE (UNIT=10,STATUS='DELETE')
           ELSE
              CALL OPEN_BULLUSER_SHARED         ! Get BULLUSER.DAT file
              CALL READ_USER_FILE_KEYNAME(USERNAME,IER)  ! Find user's info
              CALL CLOSE_BULLUSER
              IF (IER.NE.0.AND.TEST_BULLCP().EQ.2) THEN ! BULLCP process?
                 CALL SYS_BINTIM('-',LOGIN_BTIM)        ! Get today's date
                 CALL SYS_BINTIM('5-NOV-1956 11:05:56',READ_BTIM)
                 CALL READ_USER_FILE_HEADER(IER)
                 NEW_FLAG(1) = 143
                 NEW_FLAG(2) = 0
                 CALL WRITE_USER_FILE_NEW(IER)
              END IF
              IF (IER.EQ.0) THEN
                 DO I=1,FOLDER_MAX
                    LAST_READ_BTIM(1,I) = READ_BTIM(1)
                    LAST_READ_BTIM(2,I) = READ_BTIM(2)
                 END DO
              END IF
           END IF
           IF (IER.EQ.0) WRITE (9,IOSTAT=IER) USERNAME,
     &    ((LAST_READ_BTIM(1,I),LAST_READ_BTIM(2,I)),I=1,FOLDER_MAX)
        END IF

        CALL CLOSE_BULLINF

        USERINFO_READ = .TRUE.

        RETURN
        END



        SUBROUTINE UPDATE_USERINFO
C
C  SUBROUTINE UPDATE_USERINFO
C
C  FUNCTION:  Updates the latest message read times for each folder.
C
        IMPLICIT INTEGER (A - Z)

        COMMON /USERINFO/ USERINFO_READ

        INCLUDE 'BULLUSER.INC'

        IF (.NOT.USERINFO_READ) RETURN

        CALL OPEN_BULLINF_SHARED

        READ (9,KEY=USERNAME,IOSTAT=IER)
        IF (IER.EQ.0) REWRITE (9,IOSTAT=IER) USERNAME,
     &    ((LAST_READ_BTIM(1,I),LAST_READ_BTIM(2,I)),I=1,FOLDER_MAX)

        CALL CLOSE_BULLINF

        RETURN
        END


        INTEGER FUNCTION SYS_BINTIM(TIME,BTIM)

        IMPLICIT INTEGER (A-Z)

        INTEGER BTIM(2)

        CHARACTER*(*) TIME

        IF (TRIM(TIME).EQ.20) THEN
           SYS_BINTIM = SYS$BINTIM(TIME//'.00',BTIM)
        ELSE
           SYS_BINTIM = SYS$BINTIM(TIME,BTIM)
        END IF

        RETURN
        END




        SUBROUTINE NEW_MESSAGE_NOTIFICATION
C
C  SUBROUTINE NEW_MESSAGE_NOTIFICATION
C
C  FUNCTION:
C
C  Update user's last read bulletin date.  If new bulletins have been
C  added since the last time bulletins have been read, position bulletin
C  pointer so that next bulletin read is the first new bulletin, and
C  alert user.  If READNEW set and no new bulletins, just exit.
C

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLUSER.INC'

        COMMON /READIT/ READIT

        COMMON /POINT/ BULL_POINT

        COMMON /SYSTEM_FOLDERS/ SYSTEM_FLAG(FLONG),DUMMY(2)

        COMMON /COMMAND_SWITCHES/ LOGIN_SWITCH,SYSTEM_SWITCH
        COMMON /COMMAND_SWITCHES/ SYSTEM_LOGIN_BTIM(2)
        COMMON /COMMAND_SWITCHES/ REVERSE_SWITCH,SEPARATE
        CHARACTER*1 SEPARATE

        COMMON /SHUTDOWN/ NODE_NUMBER,NODE_AREA
        COMMON /SHUTDOWN/ SHUTDOWN_FLAG(FLONG)

        DIMENSION LOGIN_BTIM_SAVE(2)

        LOGIN_BTIM_SAVE(1) = LOGIN_BTIM(1)
        LOGIN_BTIM_SAVE(2) = LOGIN_BTIM(2)
        CALL UPDATE_READ                        ! Update login time

        IF (CLI$PRESENT('SELECT_FOLDER')) THEN
           CALL SELECT_FOLDER(.TRUE.,IER)
           IF (IER) RETURN
        END IF

        CALL INIT_QUEUE(FOLDER_Q1,FOLDER_COM)
        FOLDER_Q = FOLDER_Q1

        CALL OPEN_BULLFOLDER_SHARED             ! Go find folders

        DO FOLDER_NUMBER = 0,FOLDER_MAX-1
           CALL CLR2(NEW_MSG,FOLDER_NUMBER)     ! Clear new message flag
           IF (.NOT.TEST_BULLCP().AND.NODE_AREA.GT.0.AND.READIT.EQ.1
     &         .AND.TEST2(SHUTDOWN_FLAG,FOLDER_NUMBER)) THEN
              CALL SET2(NEW_MSG,FOLDER_NUMBER)
              CALL WRITE_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)
           ELSE IF ((NEW_FLAG(1).LT.142.OR.NEW_FLAG(1).GT.143).AND.
     &        TEST2(BRIEF_FLAG,FOLDER_NUMBER).AND.
     &        .NOT.TEST2(SET_FLAG,FOLDER_NUMBER)) THEN
              CALL CHANGE_FLAG_NOCMD(0,3)
              CALL SET_VERSION
           ELSE IF (TEST2(SET_FLAG,FOLDER_NUMBER).OR.
     &          TEST2(BRIEF_FLAG,FOLDER_NUMBER).OR.
     &          (FOLDER_NUMBER.GT.0.AND.
     &          TEST2(SYSTEM_FLAG,FOLDER_NUMBER).AND.READIT.EQ.1)) THEN
              CALL READ_FOLDER_FILE_KEYNUM(FOLDER_NUMBER,IER)
C
C  Unknown problem caused system folder flag in folder file to disappear
C  so this tests to see if the flag has disappeared and resets if needed.
C
              IF (TEST2(SYSTEM_FLAG,FOLDER_NUMBER).AND.
     &            .NOT.BTEST(FOLDER_FLAG,2).AND.IER.EQ.0) THEN
                 FOLDER_FLAG = IBSET(FOLDER_FLAG,2)
                 CALL REWRITE_FOLDER_FILE
              END IF
              IF (IER.NE.0) THEN
                 CALL CHANGE_FLAG_NOCMD(0,2)
                 CALL CHANGE_FLAG_NOCMD(0,3)
                 CALL CHANGE_FLAG_NOCMD(0,4)
                 IF (TEST2(SYSTEM_FLAG,FOLDER_NUMBER)) THEN
                    FOLDER_FLAG = 0
                    CALL MODIFY_SYSTEM_LIST(0)
                 END IF
              ELSE IF (READIT.EQ.1.AND.SYSTEM_SWITCH.AND.
     &          TEST2(SYSTEM_FLAG,FOLDER_NUMBER)) THEN
                 DIFF = COMPARE_BTIM(SYSTEM_LOGIN_BTIM,
     &                                  F_NEWEST_BTIM)
              ELSE
                 DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &                                  F_NEWEST_BTIM)
                 IF (DIFF.LT.0.AND.READIT.EQ.1) THEN
                    DIFF = COMPARE_BTIM(LOGIN_BTIM_SAVE,F_NEWEST_BTIM)
                    IF (FOLDER_BBOARD(:2).EQ.'::'.AND.DIFF.GE.0) THEN
                        IER = MINUTE_DIFF(LOGIN_BTIM_SAVE,F_NEWEST_BTIM)
                        IF (IER.LE.15) DIFF = -1
                    END IF
                 END IF
              END IF
              IF (DIFF.LT.0.AND.F_NBULL.GT.0) THEN  ! If new unread messages
                 CALL SET2(NEW_MSG,FOLDER_NUMBER)   ! Set new message flag
                 CALL WRITE_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)
              END IF
           END IF
        END DO

        CALL CLOSE_BULLFOLDER

        FOLDER_Q = FOLDER_Q1

        IF (READIT.EQ.0) THEN                   ! If not in READNEW mode
           IF (TEST2(NEW_MSG,0)) THEN
              CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)
           END IF
           NEW_MESS = .FALSE.
           DO FOLDER_NUMBER = 1,FOLDER_MAX-1
              IF (TEST2(NEW_MSG,FOLDER_NUMBER)) THEN
                 CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)
                 DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &                                  F_NEWEST_BTIM)
                 IF (DIFF.LT.0) THEN            ! Are there unread messages?
                    DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &                                  F_NEWEST_NOSYS_BTIM)
                    IF (DIFF.GT.0) THEN         ! Unread non-system messages?
                       DIFF = COMPARE_BTIM(LOGIN_BTIM,F_NEWEST_BTIM)
                                                ! No. Unread system messages?
                       IF (DIFF.GT.0) THEN      ! No, update last read time.
                          LAST_READ_BTIM(1,FOLDER_NUMBER+1) =
     &                                          F_NEWEST_BTIM(1)
                          LAST_READ_BTIM(2,FOLDER_NUMBER+1) =
     &                                          F_NEWEST_BTIM(2)
                       END IF
                    END IF
                    IF (DIFF.LT.0) THEN
                       WRITE (6,'('' There are new messages in '',
     &                     ''folder '',A,''.'',$)') FOLDER(1:TRIM(FOLDER))
                       NEW_MESS = .TRUE.
                    END IF
                 END IF
              END IF
           END DO
           IF (NEW_MESS) THEN
              WRITE (6,'('' Type SELECT followed by foldername to'',
     &                   '' read above messages.'')')
           END IF
           FOLDER_NUMBER = 0
           CALL SELECT_FOLDER(.FALSE.,IER)
           DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &                          F_NEWEST_BTIM)
           IF (DIFF.LT.0.AND.F_NBULL.GT.0) THEN
              CALL FIND_NEWEST_BULL     ! See if there are new messages
              IF (BULL_POINT.NE.-1) THEN
                WRITE(6,'('' Type READ to read new GENERAL messages.'')')
                NEW_COUNT = F_NBULL - BULL_POINT
                DIG = 0
                DO WHILE (NEW_COUNT.GT.0)
                   NEW_COUNT = NEW_COUNT / 10
                   DIG = DIG + 1
                END DO
                WRITE(6,'('' There are '',I<DIG>,'' new messages.'')')
     &                  F_NBULL - BULL_POINT    ! Alert user if new bulletins
              ELSE
                BULL_POINT = 0
                LAST_READ_BTIM(1,FOLDER_NUMBER+1) = F_NEWEST_BTIM(1)
                LAST_READ_BTIM(2,FOLDER_NUMBER+1) = F_NEWEST_BTIM(2)
              END IF
           END IF
        ELSE                            ! READNEW mode.
           DO FOLDER_NUMBER = 0,FOLDER_MAX-1
              IF (TEST2(NEW_MSG,FOLDER_NUMBER)) THEN
                 CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER_COM)
                 CALL SELECT_FOLDER(.FALSE.,IER)
                 IF (IER) THEN
                   IF (SYSTEM_SWITCH.AND.
     &                  TEST2(SYSTEM_FLAG,FOLDER_NUMBER)) THEN
                    DIFF = COMPARE_BTIM(SYSTEM_LOGIN_BTIM,F_NEWEST_BTIM)
                   ELSE
                    DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &                                  F_NEWEST_BTIM)
                   END IF
                   IF (DIFF.LT.0) THEN
                    IF (FOLDER_NUMBER.GT.0) CALL LOGIN_FOLDER
                    IF (BULL_POINT.NE.-1) THEN
                     IF (TEST2(BRIEF_FLAG,FOLDER_NUMBER).AND.
     &                   TEST2(SET_FLAG,FOLDER_NUMBER)) THEN
                      IF (FOLDER_NUMBER.GT.0) THEN
                       WRITE (6,'('' There are new messages in folder '',
     &                  A,''.'')') FOLDER(1:TRIM(FOLDER))
                      END IF
                     ELSE IF (FOLDER_NUMBER.EQ.0.OR.
     &                  .NOT.TEST2(BRIEF_FLAG,FOLDER_NUMBER)) THEN
                       SAVE_BULL_POINT = BULL_POINT
                       REDO = .TRUE.
                       DO WHILE (REDO)
                          REDO = .FALSE.
                          CALL READNEW(REDO)
                          IF (REDO) CALL REDISPLAY_DIRECTORY
                          BULL_POINT = SAVE_BULL_POINT
                       END DO
                     END IF
                    END IF
                   END IF
                 END IF
              END IF
           END DO
           CALL EXIT
        END IF

        RETURN
        END




        SUBROUTINE DISCONNECT_REMOTE

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLFOLDER.INC'

        WRITE (6,'('' ERROR: Connection to remote folder disconnected.'')')

        FOLDER_NUMBER = -1
        FOLDER1 = 'GENERAL'

        CALL SELECT_FOLDER(.FALSE.,IER)

        WRITE (6,'('' Resetting to GENERAL folder.'')')

        RETURN
        END
