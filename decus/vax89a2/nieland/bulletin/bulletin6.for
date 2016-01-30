C
C  BULLETIN6.FOR, Version 5/1/89
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Programmer: Mark R. London
C
        SUBROUTINE CLOSE_FILE
C
C  SUBROUTINE CLOSE_FILE
C
C  FUNCTION: To close out the bulletin files and enable CTRL-C & -Y
C
        DATA LUN /0/

        ENTRY CLOSE_BULLNOTIFY
        LUN = LUN + 1                   ! Unit = 10

        ENTRY CLOSE_BULLINF
        LUN = LUN + 1                   ! Unit = 9

        ENTRY CLOSE_SYSUAF
        LUN = LUN + 1                   ! Unit = 8

        ENTRY CLOSE_BULLFOLDER
        LUN = LUN + 3                   ! Unit = 7

        ENTRY CLOSE_BULLUSER
        LUN = LUN + 2                   ! Unit = 4

        ENTRY CLOSE_BULLDIR
        LUN = LUN + 1                   ! Unit = 2

        ENTRY CLOSE_BULLFIL
        LUN = LUN + 1                   ! Unit = 1

        CALL ENABLE_CTRL

        CLOSE (UNIT=LUN)

        LUN = 0

        RETURN
        END


        SUBROUTINE CLOSE_FILE_DELETE

        IMPLICIT INTEGER (A-Z)

        DATA LUN /0/

        ENTRY CLOSE_BULLNOTIFY_DELETE
        LUN = LUN + 8                   ! Unit = 10

        ENTRY CLOSE_BULLDIR_DELETE
        LUN = LUN + 1                   ! Unit = 2

        ENTRY CLOSE_BULLFIL_DELETE
        LUN = LUN + 1                   ! Unit = 1

        CALL ENABLE_CTRL

        CLOSE (UNIT=LUN,STATUS='DELETE')

        LUN = 0

        RETURN
        END


        SUBROUTINE OPEN_FILE(UNIT)

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLFILES.INC'

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLUSER.INC'

        INCLUDE 'BULLDIR.INC'

        INCLUDE '($FORIOSDEF)'

        INCLUDE '($PRVDEF)'

        COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

        COMMON /DIR_POSITION/ DIR_NUM

        DATA LUN /0/

        LUN = UNIT - 10                 ! 10 gets added to LUN

        ENTRY OPEN_BULLNOTIFY
        LUN = LUN + 1                   ! Unit = 10

        ENTRY OPEN_BULLINF
        LUN = LUN + 1                   ! Unit = 9

        ENTRY OPEN_SYSUAF
        LUN = LUN + 1                   ! Unit = 8

        ENTRY OPEN_BULLFOLDER
        LUN = LUN + 3                   ! Unit = 7

        ENTRY OPEN_BULLUSER
        LUN = LUN + 2                   ! Unit = 4

        ENTRY OPEN_BULLDIR
        LUN = LUN + 1                   ! Unit = 2

        ENTRY OPEN_BULLFIL
        LUN = LUN + 1                   ! Unit = 1

        IER = 0

        NTRIES = 0

        CALL DISABLE_CTRL               ! No breaks while file is open

        IF (LUN.EQ.2.AND..NOT.REMOTE_SET) THEN
           DO WHILE (FILE_LOCK(IER,IER1))

            OPEN (UNIT=2,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     &        //'.BULLDIR',STATUS='OLD',FORM='UNFORMATTED',
     &        RECORDTYPE='FIXED',RECORDSIZE=DIR_RECORD_LENGTH/4,
     &        ORGANIZATION='INDEXED',IOSTAT=IER,
     &        KEY=(9:12:INTEGER,1:8:CHARACTER),ACCESS='KEYED')

            IF (IER.EQ.FOR$IOS_FILNOTFOU) THEN
               OPEN (UNIT=2,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     &          //'.BULLDIR',STATUS='NEW',FORM='UNFORMATTED',
     &          RECORDTYPE='FIXED',RECORDSIZE=DIR_RECORD_LENGTH/4,
     &          ORGANIZATION='INDEXED',IOSTAT=IER,DISPOSE='KEEP',
     &          KEY=(9:12:INTEGER,1:8:CHARACTER),ACCESS='KEYED')
            ELSE IF (IER.EQ.0) THEN
               INQUIRE(UNIT=2,RECORDSIZE=ASK_SIZE)
               IF (ASK_SIZE.NE.DIR_RECORD_LENGTH/4) THEN
                  CLOSE (UNIT=2)
                  IDUMMY = FILE_LOCK(IER,IER1)  ! Avoid breaking out of DO loop
                  CALL CONVERT_BULLFILES
                  NTRIES = 0
               END IF
            ELSE IF (IER.EQ.FOR$IOS_INCFILORG) THEN
               IDUMMY = FILE_LOCK(IER,IER1)     ! Avoid breaking out of DO loop
               CALL CONVERT_BULLDIRS
               NTRIES = 0
            END IF
            NTRIES = NTRIES + 1
            IF (NTRIES.GT.30) CALL TIMER_ERR(LUN)
           END DO
           DIR_NUM = -1
        END IF

        IF (LUN.EQ.1.AND..NOT.REMOTE_SET) THEN
           DO WHILE (FILE_LOCK(IER,IER1))
            OPEN (UNIT=1,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     &        //'.BULLFIL',STATUS='UNKNOWN',IOSTAT=IER,
     &        ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=32,
     &        FORM='UNFORMATTED')
            IF (IER.EQ.FOR$IOS_INCRECLEN) THEN
               IDUMMY = FILE_LOCK(IER,IER1)     ! Avoid breaking out of DO loop
               CALL CONVERT_BULLFILE
               NTRIES = 0
            END IF
            NTRIES = NTRIES + 1
            IF (NTRIES.GT.30) CALL TIMER_ERR(LUN)
           END DO
        END IF

        IF (LUN.EQ.4) THEN
           DO WHILE (FILE_LOCK(IER,IER1))
            OPEN (UNIT=4,FILE=BULLUSER_FILE,STATUS='OLD',
     &       ACCESS='KEYED',RECORDTYPE='FIXED',RECORDSIZE=7+FLONG*4,
     &       ORGANIZATION='INDEXED',IOSTAT=IER,
     &       KEY=(1:12:CHARACTER))
            IF (IER.EQ.FOR$IOS_FILNOTFOU) THEN
             OPEN (UNIT=4,FILE=BULLUSER_FILE,STATUS='UNKNOWN',
     &        ACCESS='KEYED',RECORDTYPE='FIXED',RECORDSIZE=28+FLONG*16,
     &        FORM='FORMATTED',ORGANIZATION='INDEXED',IOSTAT=IER,
     &        KEY=(1:12:CHARACTER))
             WRITE (4,FMT=USER_FMT) USER_HEADER_KEY,NEWEST_BTIM,
     &        BBOARD_BTIM,PRV$M_OPER.OR.PRV$M_CMKRNL.OR.
     &        PRV$M_SETPRV,(0,I=1,FLONG*4-1)
             CLOSE (UNIT=4)
             IDUMMY = FILE_LOCK(IER,IER1)
            ELSE IF (IER.EQ.FOR$IOS_INCRECLEN) THEN
             IDUMMY = FILE_LOCK(IER,IER1)
             CALL CONVERT_USERFILE
             NTRIES = 0
            END IF
            NTRIES = NTRIES + 1
            IF (NTRIES.GT.30) CALL TIMER_ERR(LUN)
           END DO
        END IF

        IF (LUN.EQ.7) THEN
           DO WHILE (FILE_LOCK(IER,IER1))
            OPEN (UNIT=7,FILE=BULLFOLDER_FILE,STATUS='OLD',
     &       ACCESS='KEYED',RECORDTYPE='FIXED',
     &       ORGANIZATION='INDEXED',IOSTAT=IER,
     &       KEY=(1:25:CHARACTER,26:29:INTEGER))
            IF (IER.EQ.FOR$IOS_FILNOTFOU) THEN
              FOLDER1 = 'GENERAL'
              FOLDER1_OWNER = 'SYSTEM'
              FOLDER1_DESCRIP = 'Default general bulletin folder.'
              FOLDER1_BBOARD = 'NONE'
              FOLDER1_BBEXPIRE = 14
              NBULL = 0
              OPEN (UNIT=7,FILE=BULLFOLDER_FILE,STATUS='UNKNOWN',
     &          ACCESS='KEYED',RECORDTYPE='FIXED',
     &          RECORDSIZE=FOLDER_RECORD,
     &          FORM='FORMATTED',ORGANIZATION='INDEXED',IOSTAT=IER2,
     &          KEY=(1:25:CHARACTER,26:29:INTEGER))
              WRITE (7,FMT=FOLDER_FMT,IOSTAT=IER2)
     &          FOLDER1,0,FOLDER1_OWNER,FOLDER1_DESCRIP
     &          ,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,USERB,GROUPB,ACCOUNTB
     &          ,NBULL,F_NEWEST_BTIM,4,0,F_NEWEST_NOSYS_BTIM
                                                ! 4 means system folder
              CLOSE (UNIT=7)
              IDUMMY = FILE_LOCK(IER,IER1)      ! Avoid breaking out of DO loop
            END IF
            NTRIES = NTRIES + 1
            IF (NTRIES.GT.30) CALL TIMER_ERR(LUN)
           END DO
        END IF

        IF (LUN.EQ.9) THEN
           DO WHILE (FILE_LOCK(IER,IER1))
            OPEN (UNIT=9,FILE=BULLINF_FILE,STATUS='UNKNOWN',
     &       ACCESS='KEYED',RECORDTYPE='FIXED',RECORDSIZE=FOLDER_MAX*2+3,
     &       IOSTAT=IER,ORGANIZATION='INDEXED',
     &       KEY=(1:12:CHARACTER))
             IF (IER.EQ.FOR$IOS_INCRECLEN) THEN
               IDUMMY = FILE_LOCK(IER,IER1)     ! Avoid breaking out of DO loop
               CALL CONVERT_INFFILE
               NTRIES = 0
             END IF
             NTRIES = 0
             IF (NTRIES.GT.30) CALL TIMER_ERR(LUN)
           END DO
        END IF

        IF (LUN.EQ.10) THEN
           DO WHILE (FILE_LOCK(IER,IER1))
              OPEN (UNIT=10,STATUS='UNKNOWN',IOSTAT=IER,
     &           ACCESS='KEYED',RECORDTYPE='FIXED',RECORDSIZE=3,
     &           ORGANIZATION='INDEXED',KEY=(1:12:CHARACTER),
     &           FORM='UNFORMATTED',
     &           FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))//'.NOTIFY')
              NTRIES = NTRIES + 1
              IF (NTRIES.GT.30) CALL TIMER_ERR(LUN)
           END DO
        END IF

        IF (IER.NE.0) THEN
           WRITE (6,'(
     &      '' Cannot open file in OPEN_FILE, unit = '',I)') LUN
           IF (IER1.EQ.0) CALL ERRSNS(IDUMMY,IER1)
           IF (IER1.EQ.0) THEN
              WRITE (6,'('' IOSTAT error = '',I)') IER
           ELSE
              CALL SYS_GETMSG(IER1)
           END IF
           CALL ENABLE_CTRL_EXIT        ! Enable CTRL-Y & -C & EXIT
        END IF

        LUN = 0

        RETURN
        END



        SUBROUTINE TIMER_ERR(UNIT)

        IMPLICIT INTEGER (A-Z)

        CHARACTER*14 NAMES(6)
        DATA NAMES/'directory','message','BULLUSER.DAT','BULLFOLDER.DAT',
     &                  'BULLINF.DAT','notify'/
        INTEGER NAME(10)
        DATA NAME/1,2,0,3,0,0,4,0,5,6/

        IF (TEST_BULLCP().NE.2) THEN    ! If BULLCP process, don't log error
           WRITE(6,'('' ERROR: Unable to open '',A,
     &               '' file after 30 secs.'')')
     &                  NAMES(NAME(UNIT))(:TRIM(NAMES(NAME(UNIT))))
           WRITE (6,'('' Please try again later.'')')
        END IF

        CALL ENABLE_CTRL_EXIT           ! No breaks while file is open
        END



        SUBROUTINE OPEN_FILE_SHARED

        IMPLICIT INTEGER (A-Z)

        INCLUDE '($FORIOSDEF)'

        INCLUDE 'BULLFILES.INC'

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLUSER.INC'

        INCLUDE 'BULLDIR.INC'

        COMMON /POINT/ BULL_POINT

        COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

        COMMON /DIR_POSITION/ DIR_NUM

        EXTERNAL LNM_MODE_EXEC,ENABLE_CTRL_EXIT
C
C  The following 2 files were used prior to V1.1.
C
        CHARACTER*80 BULLDIR_FILE /'BULL_DIR:BULLDIR.DAT'/
        CHARACTER*80 BULLETIN_FILE /'BULL_DIR:BULLETIN.DAT'/

        CHARACTER*25 SAVE_FOLDER
        DATA SAVE_BLOCK/-1/

        DATA LUN /0/

        ENTRY OPEN_BULLNOTIFY_SHARED
        LUN = LUN + 1                   ! Unit = 10

        ENTRY OPEN_BULLINF_SHARED
        LUN = LUN + 1                   ! Unit = 9

        ENTRY OPEN_SYSUAF_SHARED
        LUN = LUN + 1                   ! Unit = 8

        ENTRY OPEN_BULLFOLDER_SHARED
        LUN = LUN + 3                   ! Unit = 7

        ENTRY OPEN_BULLUSER_SHARED
        LUN = LUN + 2                   ! Unit = 4

        ENTRY OPEN_BULLDIR_SHARED
        LUN = LUN + 1                   ! Unit = 2

        ENTRY OPEN_BULLFIL_SHARED
        LUN = LUN + 1                   ! Unit = 1

        IER = 0

        NTRIES = 0

        CALL DISABLE_CTRL

        IF (LUN.EQ.2.AND..NOT.REMOTE_SET) THEN
           DO WHILE (FILE_LOCK(IER,IER1))

            OPEN (UNIT=2,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     &        //'.BULLDIR',STATUS='OLD',FORM='UNFORMATTED',
     &        RECORDTYPE='FIXED',RECORDSIZE=DIR_RECORD_LENGTH/4,
     &        ORGANIZATION='INDEXED',IOSTAT=IER,SHARED,
     &        KEY=(9:12:INTEGER,1:8:CHARACTER),ACCESS='KEYED')
            IF (IER.EQ.FOR$IOS_FILNOTFOU.AND.(FOLDER_NUMBER.EQ.0
     &          .OR.FOLDER.EQ.'GENERAL')) THEN
               IER2 = LIB$RENAME_FILE(BULLETIN_FILE,'GENERAL.BULLFIL')
               IER2 = LIB$RENAME_FILE(BULLDIR_FILE,'GENERAL.BULLDIR')
               IF (IER2) IDUMMY = FILE_LOCK(IER,IER1) ! Don't break out of loop
            ELSE IF (IER.EQ.0) THEN
               INQUIRE(UNIT=2,RECORDSIZE=ASK_SIZE)
               IF (ASK_SIZE.NE.DIR_RECORD_LENGTH/4) THEN
                  CLOSE (UNIT=2)
                  IDUMMY = FILE_LOCK(IER,IER1)  ! Avoid breaking out of DO loop
                  CALL CONVERT_BULLFILES
                  NTRIES = 0
               END IF
            ELSE IF (IER.EQ.FOR$IOS_INCFILORG) THEN
               IDUMMY = FILE_LOCK(IER,IER1)     ! Avoid breaking out of DO loop
               CALL CONVERT_BULLDIRS
               NTRIES = 0
            END IF
            NTRIES = NTRIES + 1
            IF (NTRIES.GT.30) CALL ENABLE_CTRL_EXIT
           END DO
           DIR_NUM = -1
        END IF

        IF (LUN.EQ.1.AND.REMOTE_SET.AND.(SAVE_BLOCK.NE.BLOCK.OR.
     &          SAVE_FOLDER.NE.FOLDER)) THEN
           WRITE (REMOTE_UNIT,'(2A)',IOSTAT=IER) 5,BULL_POINT
           IF (IER.GT.0) THEN
              CALL ERROR_AND_EXIT
           ELSE
              SAVE_BLOCK = BLOCK
              SAVE_FOLDER = FOLDER
              CALL GET_REMOTE_MESSAGE(IER)
              IER = 0
           END IF
        ELSE IF (LUN.EQ.1.AND..NOT.REMOTE_SET) THEN
           DO WHILE (FILE_LOCK(IER,IER1))
            OPEN (UNIT=1,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     &        //'.BULLFIL',STATUS='OLD',
     &        ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=32,
     &        FORM='UNFORMATTED',IOSTAT=IER,SHARED)
            IF (IER.EQ.FOR$IOS_INCRECLEN) THEN
               IDUMMY = FILE_LOCK(IER,IER1)     ! Avoid breaking out of DO loop
               CALL CONVERT_BULLFILE
               NTRIES = 0
            END IF
            NTRIES = NTRIES + 1
            IF (NTRIES.GT.30) CALL ENABLE_CTRL_EXIT
           END DO
        END IF

        IF (LUN.EQ.4) THEN
           DO WHILE (FILE_LOCK(IER,IER1))
            OPEN (UNIT=4,FILE=BULLUSER_FILE,STATUS='OLD',
     &      ACCESS='KEYED',RECORDTYPE='FIXED',RECORDSIZE=7+FLONG*4,
     &      IOSTAT=IER,ORGANIZATION='INDEXED',SHARED,
     &      KEY=(1:12:CHARACTER))
            IF (IER.EQ.FOR$IOS_INCRECLEN) THEN
               IDUMMY = FILE_LOCK(IER,IER1)
               CALL CONVERT_USERFILE
               NTRIES = 0
            END IF
            NTRIES = NTRIES + 1
            IF (NTRIES.GT.30) CALL ENABLE_CTRL_EXIT
           END DO
        END IF

        IF (LUN.EQ.7) THEN
           DO WHILE (FILE_LOCK(IER,IER1))
            OPEN (UNIT=7,FILE=BULLFOLDER_FILE,STATUS='OLD',
     &      ACCESS='KEYED',RECORDTYPE='FIXED',
     &      IOSTAT=IER,ORGANIZATION='INDEXED',SHARED,
     &      KEY=(1:25:CHARACTER,26:29:INTEGER))

            IF (IER.EQ.0) THEN
               INQUIRE(UNIT=7,RECORDSIZE=ASK_SIZE)
               IF (ASK_SIZE.NE.FOLDER_RECORD/4) THEN
                  CLOSE (UNIT=7)
                  IDUMMY = FILE_LOCK(IER,IER1)
                  CALL CONVERT_BULLFOLDER(ASK_SIZE)
                  NTRIES = 0
               END IF
            END IF
            NTRIES = NTRIES + 1
            IF (NTRIES.GT.30) CALL ENABLE_CTRL_EXIT
           END DO
        END IF

        IF (LUN.EQ.8) THEN
           DO WHILE (FILE_LOCK(IER,IER1))
            OPEN (UNIT=8,FILE='SYSUAF',DEFAULTFILE='SYS$SYSTEM:SYSUAF.DAT',
     &       ACCESS='KEYED',FORM='UNFORMATTED',ORGANIZATION='INDEXED',
     &       STATUS='OLD',READONLY,IOSTAT=IER,SHARED,
     &       USEROPEN=LNM_MODE_EXEC)
           END DO
        END IF

        IF (LUN.EQ.9) THEN
           DO WHILE (FILE_LOCK(IER,IER1))
             OPEN (UNIT=9,FILE=BULLINF_FILE,STATUS='OLD',
     &        ACCESS='KEYED',RECORDTYPE='FIXED',
     &        RECORDSIZE=FOLDER_MAX*2+3,
     &        IOSTAT=IER,ORGANIZATION='INDEXED',SHARED,
     &        KEY=(1:12:CHARACTER))
             IF (IER.EQ.FOR$IOS_INCRECLEN) THEN
               IDUMMY = FILE_LOCK(IER,IER1)     ! Avoid breaking out of DO loop
               CALL CONVERT_INFFILE
               NTRIES = 0
             END IF
             NTRIES = NTRIES + 1
             IF (NTRIES.GT.30) CALL ENABLE_CTRL_EXIT
           END DO
        END IF

        IF (LUN.EQ.10) THEN
           DO WHILE (FILE_LOCK(IER,IER1))
              OPEN (UNIT=10,STATUS='OLD',IOSTAT=IER,
     &           ACCESS='KEYED',RECORDTYPE='FIXED',RECORDSIZE=3,
     &           ORGANIZATION='INDEXED',KEY=(1:12:CHARACTER),
     &           FORM='UNFORMATTED',
     &           FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))//'.NOTIFY')
              NTRIES = NTRIES + 1
              IF (NTRIES.GT.30) CALL ENABLE_CTRL_EXIT
           END DO
        END IF

        IF (IER.EQ.FOR$IOS_FILNOTFOU.AND.LUN.NE.8) THEN
           CALL SYS$SETDFPROT('FF00'X,CUR_DEF_PROT)
                ! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD,GROUP)
           CALL OPEN_FILE(LUN)
           CALL SYS$SETDFPROT(CUR_DEF_PROT,)    ! Reset default protection
        ELSE IF (IER.NE.0) THEN
           WRITE (6,'(
     &      '' Cannot open file in OPEN_FILE_SHARE, unit = '',I)') LUN
           IF (IER1.EQ.0) CALL ERRSNS(IDUMMY,IER1)
           IF (IER1.EQ.0) THEN
              WRITE (6,'('' IOSTAT error = '',I)') IER
           ELSE
              CALL SYS_GETMSG(IER1)
           END IF
           CALL ENABLE_CTRL_EXIT
        END IF

        LUN = 0

        RETURN
        END





        SUBROUTINE CONVERT_BULLDIRS

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLDIR.INC'

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLFILES.INC'

        CHARACTER BUFFER*115

        WRITE (6,'('' Converting data files to new format. Please wait.'')')

        CALL SYS$SETDFPROT('FF00'X,CUR_DEF_PROT)
                ! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD,GROUP)

        OPEN (UNIT=2,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     &        //'.BULLDIR',STATUS='OLD',FORM='UNFORMATTED',
     &        RECORDTYPE='FIXED',ACCESS='DIRECT',
     &        ORGANIZATION='RELATIVE',DISPOSE='KEEP',
     &        IOSTAT=IER)

        IF (IER.NE.0) GO TO 900 ! No BULLDIR file found.

        READ (2'1,IOSTAT=IER1) BUFFER

        CALL LIB$MOVC3(4,%REF(BUFFER(39:)),NBULL)

        OPEN (UNIT=9,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     &        //'.BULLDIR',STATUS='NEW',FORM='UNFORMATTED',
     &        RECORDTYPE='FIXED',RECORDSIZE=DIR_RECORD_LENGTH/4,
     &        ORGANIZATION='INDEXED',IOSTAT=IER,DISPOSE='DELETE',
     &        KEY=(9:12:INTEGER,1:8:CHARACTER),ACCESS='KEYED',
     &        INITIALSIZE=(((NBULL+1)*DIR_RECORD_LENGTH)/512)+5 )

        IF (IER.NE.0) THEN
           OPEN (UNIT=9,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     &        //'.BULLDIR',STATUS='NEW',FORM='UNFORMATTED',
     &        RECORDTYPE='FIXED',RECORDSIZE=DIR_RECORD_LENGTH/4,
     &        ORGANIZATION='INDEXED',IOSTAT=IER,DISPOSE='DELETE',
     &        KEY=(9:12:INTEGER,1:8:CHARACTER),ACCESS='KEYED')
        END IF

        IF (IER1.NE.0) GO TO 800

        CALL SYS_BINTIM(BUFFER(1:11)//' '//BUFFER(12:19),NEWEST_EXBTIM)
        CALL SYS_BINTIM(BUFFER(20:30)//' '//BUFFER(31:38),NEWEST_MSGBTIM)
        BULLDIR_HEADER(29:40) = BUFFER(39:)
        CALL SYS_BINTIM(BUFFER(51:61)//' '//BUFFER(62:69),SHUTDOWN_BTIM)
        BULLDIR_HEADER(49:52) = BUFFER(70:)
        IF (IER.EQ.0) WRITE (9,IOSTAT=IER) BULLDIR_HEADER

        ICOUNT = 2
        DO WHILE (IER.EQ.0)
           READ (2'ICOUNT,IOSTAT=IER) BUFFER
           IF (IER.EQ.0) THEN
              MSG_NUM = ICOUNT - 1
              DESCRIP = BUFFER(1:)
              FROM = BUFFER(54:)
              BULLDIR_ENTRY(78:81) = BUFFER(85:)
              BULLDIR_ENTRY(90:97) = BUFFER(108:)
              CALL SYS_BINTIM(BUFFER(89:99)//' '//BUFFER(100:107),EX_BTIM)
              CALL SYS_BINTIM(BUFFER(66:76)//' '//BUFFER(77:84),MSG_BTIM)
              CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
              WRITE (9,IOSTAT=IER) BULLDIR_ENTRY
              ICOUNT = ICOUNT + 1
           END IF
        END DO

800     CLOSE (UNIT=9,DISPOSE='KEEP')
        CLOSE (UNIT=2)

900     CALL SYS$SETDFPROT(CUR_DEF_PROT,)       ! Reset default protection

        RETURN

        END



        SUBROUTINE CONVERT_BULLFILES
C
C  SUBROUTINE CONVERT_BULLFILES
C
C  FUNCTION: Converts bulletin files to new format file.
C       Add expiration time to directory file, add extra byte to bulletin
C       file to show where each bulletin starts (for redunancy sake in
C       case crash occurs).
C

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLDIR.INC'

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLFILES.INC'

        CHARACTER*81 BUFFER

        WRITE (6,'('' Converting data files to new format. Please wait.'')')

        OPEN (UNIT=9,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     &        //'.BULLDIR',STATUS='OLD',
     &        RECORDTYPE='FIXED',RECORDSIZE=107,ACCESS='DIRECT',
     &        ORGANIZATION='RELATIVE',DISPOSE='KEEP',FORM='FORMATTED',
     &        SHARED,READONLY,IOSTAT=IER)

        IF (IER.NE.0) CALL ERROR_AND_EXIT               ! Error.  Why?

        OPEN (UNIT=10,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     &        //'.BULLFIL',STATUS='OLD',
     &        RECORDTYPE='FIXED',RECORDSIZE=80,
     &        FORM='FORMATTED',IOSTAT=IER,SHARED,READONLY)

        IF (IER.NE.0) CALL ERROR_AND_EXIT               ! Error.  Why?

        CALL SYS$SETDFPROT('FF00'X,CUR_DEF_PROT)
                ! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD,GROUP)

        OPEN (UNIT=1,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     &        //'.BULLFIL',STATUS='NEW',IOSTAT=IER,
     &        ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=81,
     &        FORM='FORMATTED')

        OPEN (UNIT=2,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     &        //'.BULLDIR',STATUS='NEW',FORM='UNFORMATTED',
     &        RECORDTYPE='FIXED',RECORDSIZE=DIR_RECORD_LENGTH/4,
     &        ORGANIZATION='INDEXED',IOSTAT=IER,DISPOSE='KEEP',
     &        KEY=(9:12:INTEGER,1:8:CHARACTER),ACCESS='KEYED')

        NEWEST_EXTIME = '00:00:00.00'
        READ (9'1,1000,IOSTAT=IER) 
     &          NEWEST_EXDATE,NEWEST_DATE,NEWEST_TIME(:8),
     &          NBULL,NBLOCK,SHUTDOWN,SHUTDOWN_DATE,SHUTDOWN_TIME(:8)
        NEMPTY = 0
        IF (IER.EQ.0) CALL WRITEDIR(0,IER1)

        EXTIME = '00:00:00.00'
        ICOUNT = 2
        DO WHILE (IER.EQ.0)
           READ(9'ICOUNT,1010,IOSTAT=IER)
     &          DESCRIP,FROM,DATE,TIME(:8),LENGTH,EXDATE,SYSTEM,BLOCK
           IF (IER.EQ.0) THEN
              READ(10,'(A)') BUFFER
              WRITE(1,'(A)') BUFFER(1:80)//CHAR(1)
              DO I=2,LENGTH
                 READ(10,'(A)') BUFFER
                 WRITE(1,'(A)') BUFFER
              END DO
              CALL WRITEDIR(ICOUNT-1,IER1)
              ICOUNT = ICOUNT + 1
           END IF
        END DO

        CLOSE (UNIT=9)
        CLOSE (UNIT=2)
        CLOSE (UNIT=10)
        CLOSE (UNIT=1)

        CALL SYS$SETDFPROT(CUR_DEF_PROT,)       ! Reset default protection
        RETURN

1000    FORMAT(A11,A11,A8,A4,A4,A4,A11,A8)
1010    FORMAT(A53,A12,A11,A8,A4,A11,A4,A4)

        END

        SUBROUTINE CONVERT_BULLFILE
C
C  SUBROUTINE CONVERT_BULLFILE
C
C  FUNCTION: Converts bulletin data file to new format file.
C
C  NOTE: CONVERT_BULLFILES converts from 80 to 81 byte length.
C        This converts from 81 byte length to 128 compressed format.
C

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLDIR.INC'

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLFILES.INC'

        CHARACTER*80 BUFFER,NEW_FILE

        WRITE (6,'('' Converting data files to new format. Please wait.'')')

        CALL CLOSE_BULLDIR

        CALL SYS$SETDFPROT('FF00'X,CUR_DEF_PROT)
                ! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD,GROUP)

        CALL OPEN_BULLFOLDER

100     READ (7,FMT=FOLDER_FMT,ERR=200)
     &          FOLDER,FOLDER_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP
     &          ,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB

        FOLDER_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))
     &          //FOLDER(:TRIM(FOLDER))
        NEW_FILE = FOLDER_FILE(:TRIM(FOLDER_FILE))//'.BULLFILOLD'
        OPEN (UNIT=10,FILE=FOLDER_FILE(:TRIM(FOLDER_FILE))//'.BULLFIL'
     &        ,STATUS='OLD',
     &        RECORDTYPE='FIXED',RECORDSIZE=81,ACCESS='DIRECT',
     &        FORM='FORMATTED',IOSTAT=IER,SHARED,READONLY)

        IF (IER.NE.0) CALL ERROR_AND_EXIT               ! Error.  Why?

        OPEN (UNIT=1,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))
     &     //'.BULLFIL',STATUS='NEW',IOSTAT=IER,
     &     ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=32,
     &     FORM='UNFORMATTED')
        IER = LIB$RENAME_FILE(FOLDER_FILE(:TRIM(FOLDER_FILE))
     &          //'.BULLFIL;-1',NEW_FILE)

        CALL OPEN_BULLDIR

        CALL READDIR(0,IER)

        IF (IER.EQ.1) THEN
         NBLOCK = 0
         DO I=1,NBULL
           CALL READDIR(I,IER)
           NBLOCK = NBLOCK + 1
           SBLOCK = NBLOCK
           DO J=BLOCK,LENGTH+BLOCK-1
              READ(10'J,'(A)') BUFFER
              ILEN = TRIM(BUFFER)
              IF (ILEN.EQ.0) ILEN = 1
              CALL STORE_BULL(ILEN,BUFFER,NBLOCK)
           END DO
           CALL FLUSH_BULL(NBLOCK)
           LENGTH = NBLOCK - SBLOCK + 1
           BLOCK = SBLOCK
           CALL WRITEDIR(I,IER)
         END DO

         NEMPTY = 0
         CALL WRITEDIR(0,IER)
        END IF

        CLOSE (UNIT=10)
        CLOSE (UNIT=1)

        CALL CLOSE_BULLDIR
        GOTO 100

200     CALL OPEN_BULLDIR_SHARED

        CALL SYS$SETDFPROT(CUR_DEF_PROT,)       ! Reset default protection

        RETURN

        END



        SUBROUTINE CONVERT_BULLFOLDER(ASK_SIZE)
C
C  SUBROUTINE CONVERT_BULLFOLDER
C
C  FUNCTION: Converts bulletin folder file to new format.
C
        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLDIR.INC'

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLFILES.INC'

        INCLUDE '($SSDEF)'

        CHARACTER*80 NEW_FILE

        WRITE (6,'('' Converting data files to new format. Please wait.'')')

        CALL SYS$SETDFPROT('FF00'X,CUR_DEF_PROT)

        EODIR = MAX(INDEX(BULLFOLDER_FILE,':'),INDEX(BULLFOLDER_FILE,']'))
        SUFFIX = INDEX(BULLFOLDER_FILE(EODIR:),'.') + EODIR - 1
        NEW_FILE = BULLFOLDER_FILE(:SUFFIX)//'OLD'
        IER = LIB$RENAME_FILE(BULLFOLDER_FILE,NEW_FILE)

        DO WHILE (FILE_LOCK(IER,IER1))
           OPEN (UNIT=7,FILE=NEW_FILE,STATUS='OLD',
     &          ACCESS='KEYED',RECORDTYPE='FIXED',
     &          FORM='FORMATTED',ORGANIZATION='INDEXED',IOSTAT=IER,
     &          KEY=(1:25:CHARACTER,26:29:INTEGER))
        END DO

        IF (IER.NE.0) CALL ERROR_AND_EXIT               ! Error.  Why?

        OPEN (UNIT=9,FILE=BULLFOLDER_FILE,STATUS='NEW',
     &          ACCESS='KEYED',RECORDTYPE='FIXED',
     &          RECORDSIZE=FOLDER_RECORD,
     &          FORM='FORMATTED',ORGANIZATION='INDEXED',IOSTAT=IER,
     &          KEY=(1:25:CHARACTER,26:29:INTEGER),DISPOSE='DELETE')

        IF (IER.NE.0) CALL ERROR_AND_EXIT               ! Error.  Why?

        IF (ASK_SIZE.EQ.173/4) THEN
         F_NUMBER = 0
         DO WHILE (IER.EQ.0)
           READ (7,FMT='(A25,A4,A12,A80,A12,3A4,A8,5A4)',
     &                  KEYGE=F_NUMBER,KEYID=1,IOSTAT=IER)
     &          FOLDER,F_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP
     &          ,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB
     &          ,F_NBULL,F_NEWEST_BTIM,FOLDER_FLAG,FOLDER_SET
           IF (IER.EQ.0) THEN
              WRITE (9,FMT=FOLDER_FMT,IOSTAT=IER)
     &          FOLDER,F_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP
     &          ,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB
     &          ,F_NBULL,F_NEWEST_BTIM,FOLDER_FLAG,FOLDER_SET
     &          ,F_NEWEST_BTIM
              F_NUMBER = F_NUMBER + 1
           END IF
         END DO
        ELSE
         F_NUMBER = 0
         DO WHILE (IER.EQ.0)
           READ (7,FMT='(A25,A4,A12,A80,A12,3A4,A8)',
     &                  KEYGE=F_NUMBER,KEYID=1,IOSTAT=IER)
     &          FOLDER,F_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP
     &          ,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB
           IF (IER.EQ.0) THEN
              FOLDER_FLAG = 0
              IF (F_NUMBER.EQ.0) FOLDER_FLAG = IBSET(FOLDER_FLAG,2)
              FOLDER_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))
     &          //FOLDER(:TRIM(FOLDER))
              CALL CHKACL
     &          (FOLDER_FILE(1:TRIM(FOLDER_FILE))//'.BULLFIL',IER)
              IF (IER.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL)) THEN
                 FOLDER_FLAG = IBSET(FOLDER_FLAG,0)
              END IF
              CALL OPEN_BULLDIR_SHARED
              CALL READDIR(0,IER)
              IF (NEWEST_DATE.EQ.'5-NOV-1956 ') THEN
                 IF (NBULL.GT.0) THEN
                    CALL READDIR(NBULL,IER)
                    NEWEST_DATE = DATE
                    NEWEST_TIME = TIME
                    CALL WRITEDIR(0,IER)
                 END IF
              END IF
              CALL SYS_BINTIM(NEWEST_DATE//' '//NEWEST_TIME,F_NEWEST_BTIM)
              WRITE (9,FMT=FOLDER_FMT,IOSTAT=IER)
     &          FOLDER,F_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP
     &          ,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB
     &          ,NBULL,F_NEWEST_BTIM,FOLDER_FLAG,0,F_NEWEST_BTIM
              CALL CLOSE_BULLDIR
              F_NUMBER = F_NUMBER + 1
           END IF
         END DO
        END IF

        CLOSE (UNIT=7)
        CLOSE (UNIT=9,STATUS='SAVE')

        CALL SYS$SETDFPROT(CUR_DEF_PROT,)       ! Reset default protection

        IER = LIB$DELETE_FILE(BBOARD_DIRECTORY(:TRIM(BBOARD_DIRECTORY))
     &          //'BOARD.COM;*')        ! BULLETIN$ is referenced in old file

        RETURN
        END

        SUBROUTINE CONVERT_USERFILE
C
C  SUBROUTINE CONVERT_USERFILE
C
C  FUNCTION: Converts user file to new format which has 8 bytes added.
C

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLFILES.INC'

        INCLUDE 'BULLUSER.INC'

        CHARACTER BUFFER*74,NEW_FILE*80

        CHARACTER*11 LOGIN_DATE,READ_DATE
        CHARACTER*8 LOGIN_TIME,READ_TIME

        WRITE (6,'('' Converting data files to new format. Please wait.'')')

        EODIR = MAX(INDEX(BULLUSER_FILE,':'),INDEX(BULLUSER_FILE,']'))
        SUFFIX = INDEX(BULLUSER_FILE(EODIR:),'.') + EODIR - 1
        NEW_FILE = BULLUSER_FILE(:SUFFIX)//'OLD'
        IER = LIB$RENAME_FILE(BULLUSER_FILE,NEW_FILE)

        OPEN (UNIT=9,FILE=NEW_FILE,STATUS='OLD',
     &       ACCESS='KEYED',RECORDTYPE='FIXED',
     &       FORM='FORMATTED',ORGANIZATION='INDEXED',IOSTAT=IER,
     &       KEY=(1:12:CHARACTER))
        INQUIRE (UNIT=9,RECORDSIZE=RECL)

        IF ((RECL-28)/16.GT.FLONG) THEN
           WRITE (6,'('' ERROR: Old data files have more folders'',
     &                '' than was specified with BULLUSER.INC.'')')
           WRITE (6,'('' Recompile with correct FOLDER_MAX.'')')
           IER = LIB$RENAME_FILE(NEW_FILE,BULLUSER_FILE)
           IF (USERNAME.EQ.'DECNET') THEN
              CALL SYS$DELPRC(,)
           ELSE
              CALL SYS$CANEXH()
              CALL EXIT
           END IF
        END IF

        IF (IER.EQ.0) THEN
           CALL SYS$SETDFPROT('FF00'X,CUR_DEF_PROT)
                ! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD,GROUP)
           OPEN (UNIT=4,FILE=BULLUSER_FILE,STATUS='NEW',
     &      ACCESS='KEYED',RECORDTYPE='FIXED',RECORDSIZE=28+FLONG*16,
     &      FORM='FORMATTED',ORGANIZATION='INDEXED',IOSTAT=IER,
     &      KEY=(1:12:CHARACTER))
        END IF

        IF (IER.NE.0) THEN
           WRITE (6,'('' Cannot convert user file.'')')
           IF (IER1.EQ.0) CALL ERRSNS(IDUMMY,IER1)
           CALL SYS_GETMSG(IER1)
           CALL SYS$SETDFPROT(CUR_DEF_PROT,)    ! Reset default protection
           CALL ENABLE_CTRL_EXIT
        END IF

        DO I=1,FLONG
           NEW_FLAG(I) = 'FFFFFFFF'X
           NOTIFY_FLAG(I) = 0
           BRIEF_FLAG(I) = 0
           SET_FLAG(I) = 0
        END DO

        IF (RECL.EQ.42.OR.RECL.EQ.50.OR.RECL.EQ.58.OR.RECL.EQ.66.OR.
     &          RECL.EQ.74) THEN                ! Old format
           IF (RECL.LE.58) RECL = 50
           IER = 0
           DO WHILE (IER.EQ.0)
              READ (9,'(A<RECL>)',IOSTAT=IER) BUFFER
              IF (IER.EQ.0) THEN
                TEMP_USER = BUFFER(1:12)
                LOGIN_DATE = BUFFER(13:23)
                LOGIN_TIME = BUFFER(24:31)
                READ_DATE = BUFFER(32:42)
                READ_TIME = BUFFER(43:50)
                IF (RECL.EQ.58)
     &            CALL LIB$MOVC3(8,%REF(BUFFER(51:)),SET_FLAG(1))
                IF (RECL.EQ.66)
     &            CALL LIB$MOVC3(8,%REF(BUFFER(59:)),NEW_FLAG(1))
                IF (RECL.EQ.74)
     &            CALL LIB$MOVC3(8,%REF(BUFFER(67:)),NOTIFY_FLAG(1))
                CALL SYS_BINTIM(LOGIN_DATE//' '//LOGIN_TIME,LOGIN_BTIM)
                CALL SYS_BINTIM(READ_DATE//' '//READ_TIME,READ_BTIM)
                WRITE (4,FMT=USER_FMT) TEMP_USER,LOGIN_BTIM,
     &          READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
            END IF
           END DO
           IF (RECL.LT.66) THEN
             READ (4,KEY=USER_HEADER_KEY,FMT=USER_FMT) TEMP_USER,
     &          LOGIN_BTIM,
     &          READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
             NEW_FLAG(1) = PRV$M_OPER.OR.PRV$M_CMKRNL.OR.PRV$M_SETPRV
             WRITE (4,FMT=USER_FMT) TEMP_USER,LOGIN_BTIM,
     &          READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
           END IF
        ELSE                                    ! Folder maxmimum increase
           OFLONG = (RECL - 28) / 16            ! Old  #longwords/flag
           DO WHILE (IER.EQ.0)
            READ (9,FMT='(A12,<4+OFLONG*4>A4)',IOSTAT=IER) 
     &       TEMP_USER,LOGIN_BTIM,READ_BTIM,
     &       (NEW_FLAG(I),I=1,OFLONG),(SET_FLAG(I),I=1,OFLONG),
     &       (BRIEF_FLAG(I),I=1,OFLONG),(NOTIFY_FLAG(I),I=1,OFLONG)
            IF (IER.EQ.0) THEN
             WRITE (4,FMT=USER_FMT) TEMP_USER,LOGIN_BTIM,
     &          READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
            END IF
           END DO
        END IF

        IER = 0

        CLOSE (UNIT=9)
        CLOSE (UNIT=4)

        CALL SYS$SETDFPROT(CUR_DEF_PROT,)       ! Reset default protection

        RETURN
        END


        SUBROUTINE READDIR(BULLETIN_NUM,ICOUNT)
C
C  SUBROUTINE READDIR
C
C  FUNCTION: Finds the entry for the specified bulletin in the
C       directory file and returns the information for that entry.
C
C  INPUTS:
C       BULLETIN_NUM  -  Bulletin number.  Starts with 1.
C                        If 0, gives header info, i.e number of bulls,
C                        number of blocks in bulletin file, etc.
C  OUTPUTS:
C       ICOUNT  -  The last record read by this routine.
C

        IMPLICIT INTEGER (A - Z)

        INCLUDE 'BULLDIR.INC'

        INCLUDE 'BULLFOLDER.INC'

        COMMON /PROMPT/ COMMAND_PROMPT
        CHARACTER*39 COMMAND_PROMPT

        COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

        COMMON /DIR_POSITION/ DIR_NUM

        CHARACTER*3 CFOLDER_NUMBER

        ICOUNT = BULLETIN_NUM

        IF (ICOUNT.EQ.0) THEN
           IF (.NOT.REMOTE_SET) THEN
              DO WHILE (REC_LOCK(IER))
                READ (2,KEYID=0,KEY=0,IOSTAT=IER) BULLDIR_HEADER
              END DO
              IF (IER.EQ.0) THEN
                 CALL CONVERT_HEADER_FROMBIN
                 DIR_NUM = 0
              END IF
           ELSE
              WRITE (REMOTE_UNIT,'(2A)',IOSTAT=IER) 8,0
              IF (IER.EQ.0) THEN
                 READ (REMOTE_UNIT,'(2A)',IOSTAT=IER) ICOUNT,BULLDIR_HEADER
              END IF
              IF (IER.GT.0) THEN
                 CALL ERROR_AND_EXIT
              ELSE
                 CALL CONVERT_HEADER_FROMBIN
                 RETURN
              END IF
           END IF
           IF (IER.EQ.0) THEN
              IF (NBULL.LT.0) THEN      ! This indicates bulletin deletion
                                        ! was incomplete.
                 CALL CLOSE_BULLDIR
                 CALL OPEN_BULLDIR
                 CALL CLEANUP_DIRFILE(1)
                 CALL UPDATE_FOLDER
              END IF
              IF (NEMPTY.EQ.'    ') NEMPTY = 0
C
C  Check to see if cleanup of empty file space is necessary, which is
C  defined here as being 50 blocks (200 128byte records).  Also check
C  to see if cleanup was in progress but didn't properly finish.
C
              IF (NEMPTY.GT.200.AND.TEST_BULLCP().EQ.0) THEN
                 WRITE (CFOLDER_NUMBER,'(I3)') FOLDER_NUMBER
                 IER1 = LIB$SPAWN('$'//COMMAND_PROMPT(1:INDEX(
     &            COMMAND_PROMPT,'>')-1)//'/CLEANUP='//CFOLDER_NUMBER,
     &            'NL:','NL:',1,'BULL_CLEANUP')
              ELSE IF (NEMPTY.EQ.-1) THEN
                 CALL CLEANUP_BULLFILE
              END IF
           END IF
        ELSE
           IF (.NOT.REMOTE_SET) THEN
              DO WHILE (REC_LOCK(IER))
                 IF (DIR_NUM.EQ.ICOUNT-1) THEN
                    READ(2,IOSTAT=IER) BULLDIR_ENTRY
                    IF (MSG_NUM.NE.ICOUNT) IER = 36
                 ELSE
                    READ(2,KEYID=0,KEY=ICOUNT,IOSTAT=IER) BULLDIR_ENTRY
                 END IF
              END DO
              IF (IER.EQ.0) THEN
                 CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
                 CALL CONVERT_ENTRY_FROMBIN
                 DIR_NUM = MSG_NUM
              ELSE
                 DIR_NUM = -1
              END IF
           ELSE
              WRITE (REMOTE_UNIT,'(2A)',IOSTAT=IER) 8,ICOUNT
              IF (IER.EQ.0) THEN
                 READ (REMOTE_UNIT,'(2A)',IOSTAT=IER) ICOUNT,BULLDIR_ENTRY
              END IF
              IF (IER.GT.0) THEN
                 CALL ERROR_AND_EXIT
              ELSE
                 CALL CONVERT_ENTRY_FROMBIN
                 RETURN
              END IF
           END IF
        END IF

        IF (IER.EQ.0) ICOUNT = ICOUNT + 1

        UNLOCK 2

        RETURN

        END





        SUBROUTINE READDIR_KEYGE(IER)
C
C  SUBROUTINE READDIR_KEYGE
C
C  FUNCTION: Finds the entry for the specified bulletin in the
C       directory file corresponding to or later than the date specified.
C
C  INPUTS:
C       MSG_KEY - Message key (passed via BULLDIR.INC common block).
C  OUTPUTS:
C       IER  -  If not 0, no entry found.  Else contains message number.
C

        IMPLICIT INTEGER (A - Z)

        INCLUDE 'BULLDIR.INC'

        COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

        COMMON /DIR_POSITION/ DIR_NUM

        IF (.NOT.REMOTE_SET) THEN
           DO WHILE (REC_LOCK(IER))
              READ(2,KEYID=1,KEYGT=MSG_KEY,IOSTAT=IER) BULLDIR_ENTRY
           END DO
           IF (IER.EQ.0) THEN
              IER = MSG_NUM
              CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
              CALL CONVERT_ENTRY_FROMBIN
              DIR_NUM = MSG_NUM
           ELSE
              IER = 0
              DIR_NUM = -1
           END IF
           UNLOCK 2
        ELSE
           WRITE (REMOTE_UNIT,'(3A)',IOSTAT=IER) 8,-1,MSG_KEY
           IF (IER.EQ.0) THEN
              READ (REMOTE_UNIT,'(2A)',IOSTAT=IER) ICOUNT,BULLDIR_ENTRY
           END IF
           IF (IER.GT.0) THEN
              CALL ERROR_AND_EXIT
           ELSE
              IER = MSG_NUM
              CALL CONVERT_ENTRY_FROMBIN
           END IF
        END IF

        RETURN

        END



        SUBROUTINE CONVERT_HEADER_FROMBIN

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLDIR.INC'

        CHARACTER*23 DATETIME

        CALL SYS$ASCTIM(,DATETIME,NEWEST_EXBTIM,)

        NEWEST_EXDATE = DATETIME
        NEWEST_EXTIME = DATETIME(13:)

        CALL SYS$ASCTIM(,DATETIME,NEWEST_MSGBTIM,)

        NEWEST_DATE = DATETIME
        NEWEST_TIME = DATETIME(13:)

        CALL SYS$ASCTIM(,DATETIME,SHUTDOWN_BTIM,)

        SHUTDOWN_DATE = DATETIME
        SHUTDOWN_TIME = DATETIME(13:)

        RETURN
        END



        SUBROUTINE CONVERT_ENTRY_FROMBIN

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLDIR.INC'

        CHARACTER*23 DATETIME

        CALL SYS$ASCTIM(,DATETIME,EX_BTIM,)

        EXDATE = DATETIME
        EXTIME = DATETIME(13:)

        CALL SYS$ASCTIM(,DATETIME,MSG_BTIM,)

        DATE = DATETIME
        TIME = DATETIME(13:)

        RETURN
        END





        SUBROUTINE WRITEDIR(BULLETIN_NUM,IER)
C
C  SUBROUTINE WRITEDIR
C
C  FUNCTION: Writes the entry for the specified bulletin in the
C       directory file.
C
C  INPUTS:
C       BULLETIN_NUM  -  Bulletin number.  Starts with 1.
C                        If 0, write the header of the directory file.
C  OUTPUTS:
C       IER - Error status from WRITE.
C

        IMPLICIT INTEGER (A - Z)

        COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

        COMMON /DIR_POSITION/ DIR_NUM

        INCLUDE 'BULLDIR.INC'

        CONV = .TRUE.

        GO TO 10

        ENTRY WRITEDIR_NOCONV(BULLETIN_NUM,IER)

        CONV = .FALSE.
        
10      IF (BULLETIN_NUM.EQ.0) THEN
           IF (CONV) CALL CONVERT_HEADER_TOBIN
           IF (REMOTE_SET) THEN
              WRITE(REMOTE_UNIT,'(3A)',IOSTAT=IER)9,0,BULLDIR_HEADER
           ELSE
              IER = -1
              IF (DIR_NUM.EQ.0) THEN
                 REWRITE (2,IOSTAT=IER) BULLDIR_HEADER
              END IF
              IF (IER.NE.0) THEN
                 READ (2,KEYID=0,KEY=0,IOSTAT=IER)
                 IF (IER.EQ.0) THEN
                    REWRITE (2,IOSTAT=IER) BULLDIR_HEADER
                 END IF
              END IF
              IF (IER.NE.0) THEN
                 WRITE (2,IOSTAT=IER) BULLDIR_HEADER
              END IF
           END IF
        ELSE
           IF (CONV) CALL CONVERT_ENTRY_TOBIN
           MSG_NUM = BULLETIN_NUM
           IF (REMOTE_SET) THEN
              WRITE(REMOTE_UNIT,'(3A)',IOSTAT=IER)9,BULLETIN_NUM,BULLDIR_ENTRY
           ELSE
              IER = -1
              IF (DIR_NUM.EQ.MSG_NUM) THEN
                 REWRITE (2,IOSTAT=IER) BULLDIR_ENTRY
              END IF
              IF (IER.NE.0) THEN
                 READ (2,KEYID=0,KEY=BULLETIN_NUM,IOSTAT=IER)
                 IF (IER.EQ.0) THEN
                    REWRITE (2,IOSTAT=IER) BULLDIR_ENTRY
                 ELSE
                    WRITE (2,IOSTAT=IER) BULLDIR_ENTRY
                 END IF
              END IF
           END IF
        END IF

        IF (REMOTE_SET.AND.IER.GT.0) CALL ERROR_AND_EXIT

        DIR_NUM = -1

        RETURN

        END



        SUBROUTINE CONVERT_HEADER_TOBIN

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLDIR.INC'

        CALL SYS_BINTIM(NEWEST_EXDATE//' '//NEWEST_EXTIME,NEWEST_EXBTIM)

        CALL SYS_BINTIM(NEWEST_DATE//' '//NEWEST_TIME,NEWEST_MSGBTIM)

        CALL SYS_BINTIM(SHUTDOWN_DATE//' '//SHUTDOWN_TIME,SHUTDOWN_BTIM)

        RETURN
        END



        SUBROUTINE CONVERT_ENTRY_TOBIN

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLDIR.INC'

        CALL SYS_BINTIM(EXDATE//' '//EXTIME,EX_BTIM)

        CALL SYS_BINTIM(DATE//' '//TIME,MSG_BTIM)

        CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)

        RETURN
        END




        SUBROUTINE READACL(FILENAME,ACLENT,ACLLENGTH)
C
C  SUBROUTINE READACL
C
C  FUNCTION: Reads the ACL of a file.
C
C  PARAMETERS:
C       FILENAME - Name of file to check.
C       ACLENT - String which will be large enough to hold ACL information.
C
        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLFILES.INC'

        INCLUDE '($ACLDEF)'

        CHARACTER ACLENT*(*),OUTPUT*80,ACLSTR*255,FILENAME*(*)
        CHARACTER NOT_ID*3
        DATA NOT_ID /'=[,'/

        CALL INIT_ITMLST        ! Initialize item list
        CALL ADD_2_ITMLST(ACLLENGTH,ACL$C_READACL,%LOC(ACLENT))
        CALL END_ITMLST(ACL_ITMLST)     ! Get address of itemlist

        IER = SYS$CHANGE_ACL(,ACL$C_FILE,FILENAME,%VAL(ACL_ITMLST),,,)

        DO ACC_TYPE=1,2
         POINT = 1
         OUTLEN = 0
         DO WHILE ((POINT.LT.ACLLENGTH).AND.IER)
           IER = SYS$FORMAT_ACL(ACLENT(POINT:POINT-1+
     &          ICHAR(ACLENT(POINT:POINT))),ACLLEN,ACLSTR,,,,)
           AC = INDEX(ACLSTR,',ACCESS')
           IF ((ACC_TYPE.EQ.1.AND.INDEX(ACLSTR(AC:),'WRITE').GT.0).OR.
     &         (ACC_TYPE.EQ.2.AND.INDEX(ACLSTR(AC:),'READ').GT.0)) THEN
              START_ID = INDEX(ACLSTR,'=') + 1
              END_ID = INDEX(ACLSTR,',ACCESS') - 1
              IF (ACLSTR(END_ID:END_ID).EQ.']') THEN
                 START_ID = END_ID - 1
                 DO WHILE
     &             (INDEX(NOT_ID,ACLSTR(START_ID:START_ID)).EQ.0)
                    START_ID = START_ID - 1
                 END DO
                 START_ID = START_ID + 1
                 END_ID = END_ID - 1
                 IF (ACLSTR(START_ID:START_ID).EQ.'*') THEN
                    START_ID = INDEX(ACLSTR,'=') + 1
                    END_ID = INDEX(ACLSTR,'ACCESS') - 2
                 END IF
              END IF
              IF (OUTLEN.EQ.0) THEN
                IF (FILENAME.NE.BULLUSER_FILE) THEN
                 IF (ACC_TYPE.EQ.1) THEN
                    WRITE (6,'(
     &              '' These users can read and write to this folder:'')')
                 ELSE
                    WRITE (6,'(
     &              '' These users can only read this folder:'')')
                 END IF
                ELSE
                 WRITE (6,'('' The following are rights identifiers'',
     &                  '' which will give privileges.'')')
                END IF
                OUTLEN = 1
              END IF
              IDLEN = END_ID - START_ID + 1
              IF (OUTLEN+IDLEN-1.GT.80) THEN
                 WRITE (6,'(1X,A)') OUTPUT(:OUTLEN-1)
                 OUTPUT = ACLSTR(START_ID:END_ID)//','
                 OUTLEN = IDLEN + 2
              ELSE IF (OUTLEN+IDLEN-1.EQ.80) THEN
                 WRITE (6,'(1X,A)') 
     &                  OUTPUT(:OUTLEN-1)//ACLSTR(START_ID:END_ID)
                 OUTLEN = 1
              ELSE
                 OUTPUT(OUTLEN:) = ACLSTR(START_ID:END_ID)//','
                 OUTLEN = OUTLEN + IDLEN + 1
              END IF
           END IF
           POINT = POINT + ICHAR(ACLENT(POINT:POINT))
         END DO
         IF (OUTLEN.GT.1) WRITE (6,'(1X,A)') OUTPUT(:OUTLEN-2)
        END DO

        RETURN
        END




        SUBROUTINE CONVERT_INFFILE

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLUSER.INC'

        INCLUDE 'BULLFILES.INC'

        OPEN (UNIT=10,FILE=BULLINF_FILE,STATUS='OLD',
     &     ACCESS='KEYED',RECORDTYPE='FIXED',
     &     IOSTAT=IER,ORGANIZATION='INDEXED',
     &     KEY=(1:12:CHARACTER))

        INQUIRE (UNIT=10,RECORDSIZE=RECL)

        IF ((RECL-28)/16.GT.FLONG) THEN
           WRITE (6,'('' ERROR: Old data files have more folders'',
     &                '' than was specified with BULLUSER.INC.'')')
           WRITE (6,'('' Recompile with correct FOLDER_MAX.'')')
           IF (USERNAME.EQ.'DECNET') THEN
              CALL SYS$DELPRC(,)
           ELSE
              CALL SYS$CANEXH()
              CALL EXIT
           END IF
        END IF

        RECL = RECL/8

        OPEN (UNIT=9,FILE=BULLINF_FILE,STATUS='NEW',
     &     ACCESS='KEYED',RECORDTYPE='FIXED',RECORDSIZE=FOLDER_MAX*2+3,
     &     IOSTAT=IER,ORGANIZATION='INDEXED',
     &     KEY=(1:12:CHARACTER))

        DO WHILE (IER.EQ.0)
         READ (10,IOSTAT=IER) TEMP_USER,((LAST_READ_BTIM(J,I),J=1,2),I=1,RECL)
         IF (IER.EQ.0) WRITE (9) TEMP_USER,
     &                  ((LAST_READ_BTIM(J,I),J=1,2),I=1,FOLDER_MAX)
        END DO

        CLOSE (UNIT=10,STATUS='DELETE')

        CLOSE (UNIT=9)

        RETURN
        END


        SUBROUTINE ERROR_AND_EXIT

        IMPLICIT INTEGER (A-Z)
        
        CALL ERRSNS(IDUMMY,IER)
        CALL SYS_GETMSG(IER)
        CALL ENABLE_CTRL_EXIT

        RETURN
        END

