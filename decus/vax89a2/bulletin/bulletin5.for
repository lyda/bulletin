C
C  BULLETIN5.FOR, Version 5/16/89
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Programmer: Mark R. London
C
        SUBROUTINE SET_FOLDER_DEFAULT(NOTIFY,READNEW,BRIEF)
C
C  SUBROUTINE SET_FOLDER_DEFAULT
C
C  FUNCTION: Sets flag defaults for specified folder
C
        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLUSER.INC'

        COMMON /COMMAND_LINE/ INCMD
        CHARACTER*132 INCMD

        EXTERNAL CLI$_NEGATED

        IF (.NOT.SETPRV_PRIV().AND.INCMD(:3).EQ.'SET') THEN
           WRITE (6,'(
     &      '' ERROR: No privs to change all defaults.'')')
           RETURN
        END IF

        CALL OPEN_BULLUSER_SHARED
        CALL READ_USER_FILE_HEADER(IER)
        IF (NOTIFY.EQ.0) CALL CLR2(NOTIFY_FLAG_DEF,FOLDER_NUMBER)
        IF (NOTIFY.EQ.1) CALL SET2(NOTIFY_FLAG_DEF,FOLDER_NUMBER)
        IF (READNEW.EQ.0) CALL CLR2(SET_FLAG_DEF,FOLDER_NUMBER)
        IF (READNEW.EQ.1) CALL SET2(SET_FLAG_DEF,FOLDER_NUMBER)
        IF (BRIEF.EQ.0) CALL CLR2(BRIEF_FLAG_DEF,FOLDER_NUMBER)
        IF (BRIEF.EQ.1) CALL SET2(BRIEF_FLAG_DEF,FOLDER_NUMBER)
        REWRITE(4) USER_HEADER

        FLAG = 0
        IER = SYS_TRNLNM('BULL_SYSTEM_FLAGS',TEMP_USER)
        IF (.NOT.IER) THEN
           IER = SYS_TRNLNM('MAIL$SYSTEM_FLAGS',TEMP_USER)
        END IF
        READ (TEMP_USER(:1),'(I1)',IOSTAT=IER) FLAG

        IF (NOTIFY.EQ.1.AND.BTEST(FLAG,1).AND.
     &          CLI$PRESENT('CLUSTER').EQ.%LOC(CLI$_NEGATED)) THEN
           CALL OPEN_BULLNOTIFY
           READ (10,KEY='*',IOSTAT=IER)
           IF (IER.EQ.0) DELETE (UNIT=10)
           FLAG = -1
        END IF

        IF (BRIEF.NE.-1.AND.NOTIFY.NE.-1.AND.READNEW.NE.-1) THEN
           CALL READ_USER_FILE(IER)
           DO WHILE (IER.EQ.0)
              IF (TEMP_USER(:1).NE.'*'.AND.TEMP_USER(:1).NE.':') THEN
                 IF (NOTIFY.EQ.0) CALL CLR2(NOTIFY_FLAG,FOLDER_NUMBER)
                 IF (NOTIFY.EQ.1) CALL SET2(NOTIFY_FLAG,FOLDER_NUMBER)
                 IF (READNEW.EQ.0) CALL CLR2(SET_FLAG,FOLDER_NUMBER)
                 IF (READNEW.EQ.1) CALL SET2(SET_FLAG,FOLDER_NUMBER)
                 IF (BRIEF.EQ.0) CALL CLR2(BRIEF_FLAG,FOLDER_NUMBER)
                 IF (BRIEF.EQ.1) CALL SET2(BRIEF_FLAG,FOLDER_NUMBER)
                 REWRITE(4) TEMP_USER//USER_ENTRY(13:)
                 IF (FLAG.EQ.-1) WRITE (10,IOSTAT=IER) TEMP_USER
              END IF
              CALL READ_USER_FILE(IER)
           END DO
        END IF

        IF (FLAG.EQ.-1) THEN
           CALL CLOSE_BULLNOTIFY
        ELSE IF (NOTIFY.EQ.1.AND.BTEST(FLAG,1).AND.
     &       CLI$PRESENT('CLUSTER').NE.%LOC(CLI$_NEGATED)) THEN
           WRITE (6,'('' NOTE: In a cluster, /ALL or /DEFAULT '',
     &            ''causes all users to be notified.'')')
           WRITE (6,'('' They will not be able to disable this.'',
     &            '' See HELP SET NOTIFY for more info.'')')
           CALL OPEN_BULLNOTIFY
           CALL CLOSE_BULLNOTIFY_DELETE
           CALL OPEN_BULLNOTIFY
           WRITE (10) '*           '
           CALL CLOSE_BULLNOTIFY
        ELSE IF (NOTIFY.EQ.0.AND.BTEST(FLAG,1)) THEN
           CALL OPEN_BULLNOTIFY
           READ (10,IOSTAT=IER) TEMP_USER
           IF ((IER.EQ.0.AND.TEMP_USER.EQ.'*').OR.
     &          (BRIEF.NE.-1.AND.READNEW.NE.-1)) THEN
              CALL CLOSE_BULLNOTIFY_DELETE
           ELSE
              CALL CLOSE_BULLNOTIFY
           END IF
        END IF

        CALL CLOSE_BULLUSER

        RETURN
        END




        SUBROUTINE REMOVE_FOLDER
C
C  SUBROUTINE REMOVE_FOLDER
C
C  FUNCTION: Removes a bulletin folder.
C

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLUSER.INC'

        INCLUDE 'BULLFILES.INC'

        INCLUDE 'BULLDIR.INC'

        COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

        EXTERNAL CLI$_ABSENT

        CHARACTER RESPONSE*1,TEMP*80

        IER = CLI$GET_VALUE('REMOVE_FOLDER',FOLDER1,LEN_T) ! Get folder name

        IF (IER.EQ.%LOC(CLI$_ABSENT)) THEN
           IF (.NOT.FOLDER_SET) THEN
              WRITE (6,'('' ERROR: No folder specified.'')')
              RETURN
           ELSE
              FOLDER1 = FOLDER
           END IF
        ELSE IF (LEN_T.GT.25) THEN
           WRITE(6,'('' ERROR: Folder name must be < 26 characters.'')')
           RETURN
        END IF

        CALL GET_INPUT_PROMPT(RESPONSE,LEN,
     &   'Are you sure you want to remove folder '
     &   //FOLDER1(:TRIM(FOLDER1))//' (Y/N with N as default): ')
        IF (RESPONSE.NE.'y'.AND.RESPONSE.NE.'Y') THEN
           WRITE (6,'('' Folder was not removed.'')')
           RETURN
        END IF

        CALL OPEN_BULLFOLDER                            ! Open folder file
        CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER) ! See if folder exists
        FOLDER1_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//
     &          FOLDER1

        IF (IER.NE.0) THEN
           WRITE (6,'('' ERROR: No such folder exists.'')')
           GO TO 1000
        END IF

        IF ((FOLDER1_OWNER.NE.USERNAME.AND..NOT.SETPRV_PRIV()).OR.
     &       FOLDER1_NUMBER.EQ.0) THEN
           WRITE (6,'('' ERROR: You are not able to remove the folder.'')')
           GO TO 1000
        END IF

        TEMP = FOLDER_FILE
        FOLDER_FILE = FOLDER1_FILE

        REMOTE_SET_SAVE = REMOTE_SET
        REMOTE_SET = .FALSE.

        IF (FOLDER1_BBOARD(:2).EQ.'::'.AND.BTEST(FOLDER1_FLAG,2)) THEN
           FLEN = TRIM(FOLDER1_BBOARD)
           IF (INDEX(FOLDER1_BBOARD,'*').GT.0) FLEN = FLEN - 1
           OPEN (UNIT=17,STATUS='UNKNOWN',IOSTAT=IER,
     &          RECL=256,FILE=FOLDER1_BBOARD(3:FLEN)
     &          //'::"TASK=BULLETIN1"')
           IF (IER.EQ.0) THEN           ! Deregister remote SYSTEM folder
              IF (INDEX(FOLDER1_BBOARD,'*').GT.0) THEN
                 CALL OPEN_BULLDIR
                 CALL READDIR(0,IER)
                 IF (IER.EQ.1) FOLDER1 = BULLDIR_HEADER(13:)
                 CALL CLOSE_BULLDIR
              END IF
              WRITE (17,'(2A)',IOSTAT=IER) 1,FOLDER1    ! Select folder
              IF (IER.EQ.0) READ(17,'(5A)',IOSTAT=IER)  ! Throw away response
              IF (IER.EQ.0) WRITE(17,'(2A)',IOSTAT=IER) 14,0    ! Deregister
              CLOSE (UNIT=17)
           END IF
        END IF

        TEMPSET = FOLDER_SET
        FOLDER_SET = .TRUE.
        CALL SYS$SETDFPROT('FF00'X,CUR_DEF_PROT)
                ! Set protection to (SYSTEM:RWED,OWNER:RWED,WORLD,GROUP)
                ! in case files don't exist and are created.
        CALL OPEN_BULLDIR                       ! Remove directory file
        CALL OPEN_BULLFIL                       ! Remove bulletin file
        CALL OPEN_BULLNOTIFY
        CALL CLOSE_BULLNOTIFY_DELETE
        CALL CLOSE_BULLFIL_DELETE
        CALL CLOSE_BULLDIR_DELETE
        CALL SYS$SETDFPROT(CUR_DEF_PROT,)       ! Reset default protection
        FOLDER_FILE = TEMP
        FOLDER_SET = TEMPSET

        DELETE (7)

        TEMP_NUMBER = FOLDER_NUMBER
        FOLDER_NUMBER = FOLDER1_NUMBER
        CALL SET_FOLDER_DEFAULT(0,0,0)
        FOLDER_NUMBER = TEMP_NUMBER

        WRITE (6,'('' Folder removed.'')')

        IF (FOLDER.EQ.FOLDER1) THEN
           FOLDER_SET = .FALSE.
        ELSE
           REMOTE_SET = REMOTE_SET_SAVE
        END IF

1000    CALL CLOSE_BULLFOLDER

        RETURN

        END


        SUBROUTINE SELECT_FOLDER(OUTPUT,IER)
C
C  SUBROUTINE SELECT_FOLDER
C
C  FUNCTION: Selects the specified folder.
C
C  INPUTS:
C       OUTPUT - Specifies whether status messages are outputted.
C
C  NOTES:
C       FOLDER_NUMBER is used for selecting the folder.
C       If FOLDER_NUMBER = -1, the name stored in FOLDER1 is used.
C       If FOLDER_NUMBER = -2, the name stored in FOLDER1 is used,
C       but the folder is not selected if it is remote.
C       If the specified folder is on a remote node and does not have
C       a local entry (i.e. specified via NODENAME::FOLDERNAME), then
C       FOLDER_NUMBER is set to -1.
C

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLFILES.INC'

        INCLUDE 'BULLDIR.INC'

        INCLUDE 'BULLUSER.INC'

        INCLUDE '($RMSDEF)'
        INCLUDE '($SSDEF)'

        COMMON /POINT/ BULL_POINT

        COMMON /ACCESS/ READ_ONLY
        LOGICAL READ_ONLY

        COMMON /COMMAND_LINE/ INCMD
        CHARACTER*132 INCMD

        COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
        DATA REMOTE_SET /.FALSE./

        COMMON /SHUTDOWN/ NODE_NUMBER,NODE_AREA
        COMMON /SHUTDOWN/ SHUTDOWN_FLAG(FLONG)

        COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
        CHARACTER*64 BULL_PARAMETER

        COMMON /TAGS/ BULL_TAG,READ_TAG

        EXTERNAL CLI$_ABSENT

        CHARACTER*80 LOCAL_FOLDER1_DESCRIP

        DIMENSION FIRST_TIME(FLONG)     ! Bit set for folder if folder has
        DATA FIRST_TIME /FLONG*0/       ! been selected before this.

        COMMAND = (INCMD(:3).EQ.'ADD').OR.(INCMD(:3).EQ.'DEL').OR.
     &            (INCMD(:3).EQ.'DIR').OR.(INCMD(:3).EQ.'IND').OR.
     &            (INCMD(:3).EQ.'REP').OR.(INCMD(:3).EQ.'SEL').OR.
     &            (INCMD(:3).EQ.'SET')

        IF (.NOT.OUTPUT.OR.FOLDER_NUMBER.NE.-1.OR.COMMAND) THEN
           IF (OUTPUT) THEN                     ! Get folder name
              IER = CLI$GET_VALUE('SELECT_FOLDER',FOLDER1)
           END IF

           FLEN = TRIM(FOLDER1)         ! Add GENERAL after :: if no
           IF (FLEN.GT.1) THEN          ! name specified after the ::
              IF (FOLDER1(FLEN-1:FLEN).EQ.'::') THEN
                 FOLDER1 = FOLDER1(:FLEN)//'GENERAL'
              END IF
           END IF

           IF (((IER.EQ.%LOC(CLI$_ABSENT).OR.FOLDER1.EQ.'GENERAL').AND.
     &      OUTPUT).OR.((FOLDER_NUMBER.EQ.0.OR.(FOLDER1.EQ.'GENERAL'.AND.
     &      FOLDER_NUMBER.LE.-1)).AND..NOT.OUTPUT)) THEN ! Select GENERAL
              FOLDER_NUMBER = 0
              FOLDER1 = 'GENERAL'
           END IF
        END IF

        CALL OPEN_BULLFOLDER_SHARED                     ! Go find folder

        REMOTE_TEST = 0

        IF (OUTPUT.OR.FOLDER_NUMBER.LE.-1) THEN
           REMOTE_TEST = INDEX(FOLDER1,'::')
           IF (REMOTE_TEST.GT.0) THEN
              FOLDER1_BBOARD = '::'//FOLDER1(:REMOTE_TEST-1)
              FOLDER1 = FOLDER1(REMOTE_TEST+2:TRIM(FOLDER1))
              FOLDER1_NUMBER = -1
              IER = 0
           ELSE IF (INCMD(:2).EQ.'SE') THEN
              CALL READ_FOLDER_FILE_KEYNAME_TEMP
     &                          (FOLDER1(:TRIM(FOLDER1)),IER)
           ELSE
              CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER)
           END IF
        ELSE
           FOLDER1_NUMBER = FOLDER_NUMBER
           CALL READ_FOLDER_FILE_KEYNUM_TEMP(FOLDER_NUMBER,IER)
        END IF

        IF (BTEST(FOLDER1_FLAG,29)) THEN        ! Error in folder flag!!
           FOLDER1_FLAG = FOLDER1_FLAG.AND.3
           F1_EXPIRE_LIMIT = 0
           CALL REWRITE_FOLDER_FILE_TEMP
        END IF

        CALL CLOSE_BULLFOLDER

        IF (IER.EQ.0.AND.FOLDER1_BBOARD(:2).EQ.'::') THEN
           IF (FOLDER_NUMBER.EQ.-2) RETURN      ! Don't allow
           LOCAL_FOLDER1_FLAG = FOLDER1_FLAG
           LOCAL_FOLDER1_DESCRIP = FOLDER1_DESCRIP
           CALL CONNECT_REMOTE_FOLDER(READ_ONLY,IER)
           IF (IER.NE.0) THEN
              IF (OUTPUT) THEN
                 WRITE (6,'('' ERROR: Unable to connect to folder.'')')
              END IF
              RETURN
           END IF
           IF (REMOTE_TEST.GT.0) THEN   ! Folder specified with "::"
              FOLDER1 = FOLDER1_BBOARD(3:TRIM(FOLDER1_BBOARD))//'::'//
     &                  FOLDER1
              FOLDER1_NUMBER = -1
           ELSE                         ! True remote folder
              FOLDER1_DESCRIP = LOCAL_FOLDER1_DESCRIP   ! Use local description
              IF (BTEST(FOLDER1_FLAG,0)) THEN   ! Copy remote folder protection
                 LOCAL_FOLDER1_FLAG = IBSET(LOCAL_FOLDER1_FLAG,0)
              ELSE
                 LOCAL_FOLDER1_FLAG = IBCLR(LOCAL_FOLDER1_FLAG,0)
              END IF
              FOLDER1_FLAG = LOCAL_FOLDER1_FLAG         ! Use local flag info
              CALL OPEN_BULLFOLDER      ! Update local folder information
              CALL READ_FOLDER_FILE_KEYNAME(FOLDER1,IER)
              FOLDER_COM = FOLDER1_COM
              CALL REWRITE_FOLDER_FILE
              CALL CLOSE_BULLFOLDER
           END IF
           REMOTE_SET = .TRUE.
        END IF

        IF (IER.EQ.0) THEN                              ! Folder found
           FOLDER1_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))
     &          //FOLDER1
           IF (BTEST(FOLDER1_FLAG,0).AND.FOLDER1_BBOARD(:2).NE.'::'
     &          .AND..NOT.SETPRV_PRIV()) THEN
                                ! Is folder protected and not remote?
              CALL CHKACL
     &          (FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL',IER)
              IF (IER.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL).AND.USERNAME
     &            .NE.FOLDER1_OWNER) THEN
                 IF (SETPRV_PRIV()) THEN
                    READ_ACCESS = 1
                    WRITE_ACCESS = 1
                 ELSE
                    CALL CHECK_ACCESS
     &               (FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL',
     &                USERNAME,READ_ACCESS,WRITE_ACCESS)
                 END IF
                 IF (.NOT.READ_ACCESS.AND..NOT.WRITE_ACCESS) THEN
                  IF (OUTPUT) THEN
                   WRITE(6,'('' You are not allowed to access folder.'')')
                   WRITE(6,'('' See '',A,'' if you wish to access folder.'')')
     &                  FOLDER1_OWNER(:TRIM(FOLDER1_OWNER))
                  ELSE IF (TEST2(BRIEF_FLAG,FOLDER1_NUMBER).OR.
     &                   TEST2(SET_FLAG,FOLDER1_NUMBER)) THEN
                   CALL OPEN_BULLUSER_SHARED
                   CALL READ_USER_FILE_KEYNAME(USERNAME,IER)
                   CALL CLR2(BRIEF_FLAG,FOLDER1_NUMBER)
                   CALL CLR2(SET_FLAG,FOLDER1_NUMBER)
                   IF (IER.EQ.0) REWRITE (4) USER_ENTRY
                   CALL CLOSE_BULLUSER
                  END IF
                  IER = 0
                  RETURN
                 END IF
              ELSE IF (BTEST(FOLDER1_FLAG,0).AND.
     &            IER.EQ.(SS$_ACLEMPTY.OR.SS$_NORMAL)) THEN
                 CALL OPEN_BULLFOLDER
                 CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER1)
                 FOLDER1_FLAG = IBCLR(FOLDER1_FLAG,0)
                 CALL REWRITE_FOLDER_FILE_TEMP
                 CALL CLOSE_BULLFOLDER
              END IF
           ELSE                                 ! Folder not protected
              IER = SS$_ACLEMPTY.OR.SS$_NORMAL  ! Indicate folder selected
           END IF

           IF (FOLDER1_BBOARD(:2).NE.'::') THEN
              IF (REMOTE_SET) CLOSE(UNIT=REMOTE_UNIT)
              REMOTE_SET = .FALSE.
           END IF

           IF (IER) THEN
              FOLDER_COM = FOLDER1_COM          ! Folder successfully set so
              FOLDER_FILE = FOLDER1_FILE        ! update folder parameters

              IF (FOLDER_NUMBER.NE.0) THEN
                 FOLDER_SET = .TRUE.
              ELSE
                 FOLDER_SET = .FALSE.
              END IF

              IF (OUTPUT.AND.INCMD(:3).NE.'DIR') THEN
                 WRITE (6,'('' Folder has been set to '',A)') 
     &              FOLDER(:TRIM(FOLDER))//'.'
                 BULL_POINT = 0 ! Reset pointer to first bulletin
              END IF

              IF (IER.NE.(SS$_ACLEMPTY.OR.SS$_NORMAL).AND.USERNAME
     &            .NE.FOLDER_OWNER) THEN
                 IF (.NOT.WRITE_ACCESS) THEN
                   IF (OUTPUT.AND.INCMD(:3).NE.'DIR')
     &              WRITE (6,'('' Folder only accessible for reading.'')')
                   READ_ONLY = .TRUE.
                 ELSE
                   READ_ONLY = .FALSE.
                 END IF
              ELSE
                 READ_ONLY = .FALSE.
              END IF

              IF (FOLDER_NUMBER.GT.0) THEN
                IF (TEST_BULLCP()) THEN
                 CALL SET2(FIRST_TIME,FOLDER_NUMBER)
                ELSE IF (.NOT.TEST2(FIRST_TIME,FOLDER_NUMBER)) THEN
                                ! If first select, look for expired messages.
                 CALL OPEN_BULLDIR
                 CALL READDIR(0,IER)    ! Get header info from BULLDIR.DAT
                 IF (IER.EQ.1) THEN             ! Is header present?
                    IER = COMPARE_DATE(NEWEST_EXDATE,' ') ! Yes. Any expired?
                    IF (SHUTDOWN.GT.0.AND.NODE_AREA.GT.0.AND.
     &                  (FOLDER_NUMBER.EQ.0.OR.BTEST(FOLDER_FLAG,2))
     &                  .AND.TEST2(SHUTDOWN_FLAG,FOLDER_NUMBER)) THEN
                                                ! Do shutdown bulletins exist?
                       SHUTDOWN = 0
                       IER1 = -1
                    ELSE
                       IF (TEST2(SHUTDOWN_FLAG,FOLDER_NUMBER)) THEN
                          CALL UPDATE_SHUTDOWN(FOLDER_NUMBER)
                       END IF
                       IER1 = 1
                    END IF
                    IF (IER.LE.0.OR.IER.GT.20*356.OR.IER1.LE.0) THEN
                       CALL UPDATE      ! Need to update
                    END IF
                 ELSE
                    NBULL = 0
                 END IF
                 CALL CLOSE_BULLDIR
                 CALL SET2(FIRST_TIME,FOLDER_NUMBER)
                END IF
              END IF

              IF (OUTPUT) THEN
               IF (FOLDER_NUMBER.GE.0.AND.CLI$PRESENT('MARKED')) THEN
                 READ_TAG = .TRUE.
                 CALL GET_FIRST_TAG(FOLDER_NUMBER,IER,BULL_POINT)
                 IF (INCMD(:3).NE.'DIR') THEN
                   IF (IER.EQ.0) THEN
                    WRITE(6,'('' NOTE: Only marked messages'',
     &                                  '' will be shown.'')')
                   ELSE
                    WRITE(6,'('' ERROR: No marked messages found.'')')
                   END IF
                 END IF
               ELSE
                 READ_TAG = .FALSE.
               END IF
              END IF

              IF (FOLDER_NUMBER.NE.0.AND..NOT.READ_TAG) THEN
                IF (OUTPUT.AND.INCMD(:3).NE.'DIR') THEN
                 DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),
     &                                  F_NEWEST_BTIM)
                 IF (DIFF.LT.0.AND.F_NBULL.GT.0) THEN   ! If new unread messages
                  CALL FIND_NEWEST_BULL                 ! See if we can find it
                  IF (BULL_POINT.NE.-1) THEN
                    WRITE(6,'('' Type READ to read new messages.'')')
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
                  END IF
                 END IF
                END IF
              END IF
              IER = 1
           ELSE IF (OUTPUT) THEN
              WRITE (6,'('' Cannot access specified folder.'')')
              CALL SYS_GETMSG(IER)
           END IF
        ELSE                                            ! Folder not found
           IF (OUTPUT) WRITE (6,'('' ERROR: Folder does not exist.'')')
           IER = 0
        END IF

        RETURN

        END



        SUBROUTINE CONNECT_REMOTE_FOLDER(READ_ONLY,IER)
C
C  SUBROUTINE CONNECT_REMOTE_FOLDER
C
C  FUNCTION: Connects to folder that is located on other DECNET node.
C
        IMPLICIT INTEGER (A-Z)

        COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT
        DATA REMOTE_UNIT /15/

        COMMON /COMMAND_SWITCHES/ LOGIN_SWITCH,SYSTEM_SWITCH
        COMMON /COMMAND_SWITCHES/ SYSTEM_LOGIN_BTIM(2)
        COMMON /COMMAND_SWITCHES/ REVERSE_SWITCH,SEPARATE
        CHARACTER*1 SEPARATE

        INCLUDE 'BULLUSER.INC'

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLDIR.INC'

        INCLUDE 'BULLFILES.INC'

        CHARACTER*12 FOLDER_BBOARD_SAVE,FOLDER_OWNER_SAVE
        CHARACTER*25 FOLDER_SAVE

        DIMENSION DUMMY(2)

        REMOTE_UNIT = 31 - REMOTE_UNIT

        SAME = .TRUE.
        LEN_BBOARD = TRIM(FOLDER1_BBOARD)
        IF (INDEX(FOLDER1_BBOARD,'*').GT.0) THEN  ! Remote folder name different
           SAME = .FALSE.                         ! from local?  Yes.
           LEN_BBOARD = LEN_BBOARD - 1
        END IF

        OPEN (UNIT=REMOTE_UNIT,STATUS='UNKNOWN',IOSTAT=IER,RECL=256,
     &          FILE=FOLDER1_BBOARD(3:LEN_BBOARD)//'::"TASK=BULLETIN1"')

        IF (IER.EQ.0) THEN
           IF (.NOT.SAME) THEN
              FOLDER1_FILE = FOLDER_FILE
              FOLDER_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))
     &          //FOLDER1
              REMOTE_SET_SAVE = REMOTE_SET
              REMOTE_SET = .FALSE.
              CALL OPEN_BULLDIR
              CALL READDIR(0,IER)
              CALL CLOSE_BULLDIR
              REMOTE_SET = REMOTE_SET_SAVE
              FOLDER_FILE = FOLDER1_FILE
              FOLDER_SAVE = FOLDER1
              FOLDER1 = BULLDIR_HEADER(13:)
           END IF
           WRITE (REMOTE_UNIT,'(2A)',IOSTAT=IER) 1,FOLDER1
           FOLDER_OWNER_SAVE = FOLDER1_OWNER
           FOLDER_BBOARD_SAVE = FOLDER1_BBOARD
           FOLDER_NUMBER_SAVE = FOLDER1_NUMBER
           IF (IER.EQ.0) THEN
              READ(REMOTE_UNIT,'(5A)',IOSTAT=IER)IER1,READ_ONLY,
     &          DUMMY(1),DUMMY(2),FOLDER1_COM
           END IF
           IF (.NOT.SAME) FOLDER1 = FOLDER_SAVE
        END IF

        IF (IER.NE.0.OR..NOT.IER1) THEN
           CLOSE (UNIT=REMOTE_UNIT)
           REMOTE_UNIT = 31 - REMOTE_UNIT
           IF (IER.EQ.0.AND.FOLDER_NUMBER_SAVE.GE.0) THEN
              IF (TEST2(BRIEF_FLAG,FOLDER_NUMBER_SAVE)
     &            .OR.TEST2(SET_FLAG,FOLDER_NUMBER_SAVE)) THEN
                 CALL OPEN_BULLUSER_SHARED
                 CALL READ_USER_FILE_KEYNAME(USERNAME,IER)
                 CALL CLR2(BRIEF_FLAG,FOLDER_NUMBER_SAVE)
                 CALL CLR2(SET_FLAG,FOLDER_NUMBER_SAVE)
                 IF (IER.EQ.0) REWRITE (4) USER_ENTRY
                 CALL CLOSE_BULLUSER
              END IF
           END IF
           IER = 2
        ELSE
           FOLDER1_BBOARD = FOLDER_BBOARD_SAVE
           FOLDER1_NUMBER = FOLDER_NUMBER_SAVE
           FOLDER1_OWNER = FOLDER_OWNER_SAVE
           CLOSE (UNIT=31-REMOTE_UNIT)
C
C  If remote folder has returned a last read time for the folder,
C  and if in /LOGIN mode, or last selected folder was a different
C  folder, or folder specified with "::", then update last read time.
C
           IF (((FOLDER_NUMBER.NE.FOLDER1_NUMBER.OR.LOGIN_SWITCH)
     &          .AND.(DUMMY(1).NE.0.OR.DUMMY(2).NE.0))
     &          .OR.FOLDER1_NUMBER.EQ.-1) THEN
              LAST_READ_BTIM(1,FOLDER1_NUMBER+1) = DUMMY(1)
              LAST_READ_BTIM(2,FOLDER1_NUMBER+1) = DUMMY(2)
           END IF
           IER = 0
        END IF

        RETURN
        END









        SUBROUTINE UPDATE_FOLDER
C
C  SUBROUTINE UPDATE_FOLDER
C
C  FUNCTION: Updates folder info due to new message.
C

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLDIR.INC'

        INCLUDE 'BULLFOLDER.INC'

        IF (FOLDER_NUMBER.LT.0) RETURN

        CALL OPEN_BULLFOLDER_SHARED                     ! Open folder file

        CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)

        CALL SYS_BINTIM(NEWEST_DATE//' '//NEWEST_TIME,F_NEWEST_BTIM)

        F_NBULL = NBULL

        IF (FOLDER_NUMBER.EQ.0) FOLDER_FLAG = IBSET(FOLDER_FLAG,2)

        IF (.NOT.BTEST(SYSTEM,0)) THEN  ! Is non-system message?
           F_NEWEST_NOSYS_BTIM(1) = F_NEWEST_BTIM(1) ! If so, update latest
           F_NEWEST_NOSYS_BTIM(2) = F_NEWEST_BTIM(2) ! system time.
        END IF

        CALL REWRITE_FOLDER_FILE

        CALL CLOSE_BULLFOLDER

        RETURN
        END



        SUBROUTINE SHOW_FOLDER
C
C  SUBROUTINE SHOW_FOLDER
C
C  FUNCTION: Shows the information on any folder.
C

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLUSER.INC'

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLFILES.INC'

        INCLUDE 'BULLDIR.INC'

        COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

        INCLUDE '($SSDEF)'

        INCLUDE '($RMSDEF)'

        EXTERNAL CLI$_ABSENT

        IF (CLI$GET_VALUE('SHOW_FOLDER',FOLDER1).EQ.%LOC(CLI$_ABSENT))
     &          FOLDER1 = FOLDER

        IF (INDEX(FOLDER1,'::').NE.0) THEN
           WRITE (6,'('' ERROR: Invalid command for remote folder.'')')
           RETURN
        END IF

        CALL OPEN_BULLFOLDER_SHARED                     ! Open folder file

        CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER)
        FOLDER1_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//
     &          FOLDER1
        IF (IER.NE.0) THEN
           WRITE (6,'('' ERROR: Specified folder was not found.'')')
           CALL CLOSE_BULLFOLDER
           RETURN
        ELSE IF (FOLDER.EQ.FOLDER1) THEN
           WRITE (6,1000) FOLDER1,FOLDER1_OWNER,
     &                  FOLDER1_DESCRIP(:TRIM(FOLDER1_DESCRIP))
        ELSE
           WRITE (6,1010) FOLDER1,FOLDER1_OWNER,
     &                  FOLDER1_DESCRIP(:TRIM(FOLDER1_DESCRIP))
        END IF

        IF (CLI$PRESENT('FULL')) THEN
           CALL CHKACL
     &          (FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL',IER)
           IF (IER.EQ.(SS$_ACLEMPTY.OR.SS$_NORMAL).OR.(.NOT.IER)) THEN
              IF (FOLDER1_BBOARD(:2).EQ.'::'.AND.       ! Is folder remote
     &          BTEST(FOLDER1_FLAG,0)) THEN             ! and private?
                 WRITE (6,'('' Folder is a private folder.'')')
              ELSE
                 WRITE (6,'('' Folder is not a private folder.'')')
              END IF
           ELSE
              IF (SETPRV_PRIV()) THEN
                 READ_ACCESS = 1
                 WRITE_ACCESS = 1
              ELSE
                CALL CHECK_ACCESS
     &            (FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL',
     &             USERNAME,READ_ACCESS,WRITE_ACCESS)
              END IF
              IF (WRITE_ACCESS)
     &        CALL SHOWACL(FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL')
           END IF
           IF (SETPRV_PRIV().OR.USERNAME.EQ.FOLDER1_OWNER) THEN
              IF (FOLDER1_BBOARD(:2).EQ.'::') THEN
                 FLEN = TRIM(FOLDER1_BBOARD)
                 IF (INDEX(FOLDER1_BBOARD,'*').EQ.0) THEN
                    WRITE (6,'('' Folder is located on node '',
     &               A,''.'')') FOLDER1_BBOARD(3:FLEN)
                 ELSE
                    FOLDER1_FILE = FOLDER_FILE
                    FOLDER_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))
     &                  //FOLDER1
                    REMOTE_SET_SAVE = REMOTE_SET
                    REMOTE_SET = .FALSE.
                    CALL OPEN_BULLDIR
                    CALL READDIR(0,IER)
                    CALL CLOSE_BULLDIR
                    REMOTE_SET = REMOTE_SET_SAVE
                    WRITE (6,'('' Folder is located on node '',
     &                 A,''. Remote folder name is '',A,''.'')') 
     &                 FOLDER1_BBOARD(3:FLEN-1),
     &                 BULLDIR_HEADER(13:TRIM(BULLDIR_HEADER))
                 END IF
              ELSE IF (FOLDER1_BBOARD.NE.'NONE') THEN
                 FLEN = TRIM(FOLDER1_BBOARD)
                 IF (FLEN.GT.0) THEN
                  WRITE (6,'('' BBOARD for folder is '',A<FLEN>,''.'')')
     &                  FOLDER1_BBOARD(:FLEN)
                 END IF
                 IF ((USERB1.EQ.0.AND.GROUPB1.EQ.0).OR.BTEST(USERB1,31)) THEN
                  WRITE (6,'('' BBOARD was specified with /SPECIAL.'')')
                  IF (BTEST(GROUPB1,31)) THEN
                   WRITE (6,'('' BBOARD was specified with /VMSMAIL.'')')
                  END IF
                 END IF
              ELSE
                 WRITE (6,'('' No BBOARD has been defined.'')')
              END IF
              IF (FOLDER1_BBEXPIRE.GT.0) THEN
                 WRITE (6,'('' Default expiration is '',I3,'' days.'')')
     &                  FOLDER1_BBEXPIRE
              ELSE IF (FOLDER1_BBEXPIRE.EQ.-1) THEN
                 WRITE (6,'('' Default expiration is permanent.'')')
              ELSE
                 WRITE (6,'('' No default expiration set.'')')
              END IF
              IF (BTEST(FOLDER1_FLAG,2)) THEN
                 WRITE (6,'('' SYSTEM has been set.'')')
              END IF
              IF (BTEST(FOLDER1_FLAG,1)) THEN
                 WRITE (6,'('' DUMP has been set.'')')
              END IF
              IF (BTEST(FOLDER1_FLAG,3)) THEN
                 WRITE (6,'('' NOPROMPT_EXPIRE has been set.'')')
              END IF
              IF (BTEST(FOLDER1_FLAG,4)) THEN
                 WRITE (6,'('' STRIP has been set.'')')
              END IF
              IF (BTEST(FOLDER1_FLAG,5)) THEN
                 WRITE (6,'('' DIGEST has been set.'')')
              END IF
              IF (F1_EXPIRE_LIMIT.GT.0) THEN
                 WRITE (6,'('' EXPIRATION limit is '',I3,'' days.'')')
     &                  F1_EXPIRE_LIMIT
              END IF
              CALL OPEN_BULLUSER_SHARED
              CALL READ_USER_FILE_HEADER(IER)
              IF (TEST2(SET_FLAG_DEF,FOLDER1_NUMBER)) THEN
               IF (TEST2(BRIEF_FLAG_DEF,FOLDER1_NUMBER)) THEN
                 WRITE (6,'('' Default is BRIEF.'')')
               ELSE
                 WRITE (6,'('' Default is READNEW.'')')
               END IF
              ELSE
               IF (TEST2(BRIEF_FLAG_DEF,FOLDER1_NUMBER)) THEN
                 WRITE (6,'('' Default is SHOWNEW.'')')
               ELSE
                 WRITE (6,'('' Default is NOREADNEW.'')')
               END IF
              END IF
              IF (TEST2(NOTIFY_FLAG_DEF,FOLDER1_NUMBER)) THEN
                 WRITE (6,'('' Default is NOTIFY.'')')
              ELSE
                 WRITE (6,'('' Default is NONOTIFY.'')')
              END IF
              CALL CLOSE_BULLUSER
           END IF
        END IF

        CALL CLOSE_BULLFOLDER

        RETURN

1000    FORMAT(' Current folder is: ',A25,' Owner: ',A12,
     &          ' Description: ',/,1X,A)
1010    FORMAT(' Folder name is: ',A25,' Owner: ',A12,
     &          ' Description: ',/,1X,A)
        END


        SUBROUTINE DIRECTORY_FOLDERS(FOLDER_COUNT)
C
C  SUBROUTINE DIRECTORY_FOLDERS
C
C  FUNCTION: Display all FOLDER entries.
C
        IMPLICIT INTEGER (A - Z)

        INCLUDE 'BULLFILES.INC'

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLUSER.INC'

        COMMON /PAGE/ PAGE_LENGTH,PAGE_WIDTH,PAGING
        LOGICAL PAGING

        DATA SCRATCH_D1/0/

        CHARACTER*17 DATETIME

        IF (FOLDER_COUNT.GT.0) GO TO 50         ! Skip init steps if this is
                                                ! not the 1st page of folder

        IF (CLI$PRESENT('DESCRIBE')) THEN
           NLINE = 2    ! Include folder descriptor if /DESCRIBE specified
        ELSE
           NLINE = 1
        END IF

C
C  Folder listing is first buffered into temporary memory storage before
C  being outputted to the terminal.  This is to be able to quickly close the
C  folder file, and to avoid the possibility of the user holding the screen,
C  and thus causing the folder file to stay open.  The temporary memory
C  is structured as a linked-list queue, where SCRATCH_D1 points to the header
C  of the queue.  See BULLSUBS.FOR for more description of the queue.
C
        CALL INIT_QUEUE(SCRATCH_D1,FOLDER1_COM)
        SCRATCH_D = SCRATCH_D1

        CALL OPEN_BULLFOLDER_SHARED             ! Get folder file

        NUM_FOLDER = 0
        IER = 0
        FOLDER1 = '                         '   ! Start folder search
        DO WHILE (IER.EQ.0)                     ! Copy all bulletins from file
           CALL READ_FOLDER_FILE_TEMP(IER)
           IF (IER.EQ.0) THEN
              IF (BTEST(FOLDER1_FLAG,0).AND..NOT.SETPRV_PRIV()) THEN
                 FOLDER1_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))
     &                                  //FOLDER1
                 CALL CHECK_ACCESS
     &            (FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL',
     &             USERNAME,READ_ACCESS,-1)
              ELSE
                 READ_ACCESS = 1
              END IF
              IF (READ_ACCESS) THEN
                 NUM_FOLDER = NUM_FOLDER + 1
                 CALL WRITE_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,FOLDER1_COM)
              END IF
           END IF
        END DO

        CALL CLOSE_BULLFOLDER                   ! We don't need file anymore

        IF (NUM_FOLDER.EQ.0) THEN
           WRITE (6,'('' There are no folders.'')')
           RETURN
        END IF

C
C  Folder entries are now in queue.  Output queue entries to screen.
C

        SCRATCH_D = SCRATCH_D1                  ! Init queue pointer to header

        FOLDER_COUNT = 1                        ! Init folder number counter

50      CALL LIB$ERASE_PAGE(1,1)                ! Clear the screen

        WRITE (6,'(1X,''Folder'',22X,''Last message'',7X,''Messages'',
     &          2X,''Owner'',/,1X,80(''-''))')

        IF (.NOT.PAGING) THEN
           DISPLAY = (NUM_FOLDER-FOLDER_COUNT+1)*NLINE+2
        ELSE
           DISPLAY = MIN((NUM_FOLDER-FOLDER_COUNT+1)*NLINE+2,PAGE_LENGTH-4)
                        ! If more entries than page size, truncate output
        END IF

        DO I=FOLDER_COUNT,FOLDER_COUNT+(DISPLAY-2)/NLINE-1
           CALL READ_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,FOLDER1_COM)
           DIFF = COMPARE_BTIM
     &                  (LAST_READ_BTIM(1,FOLDER1_NUMBER+1),F1_NEWEST_BTIM)
           IF (F1_NBULL.GT.0) THEN
              CALL SYS$ASCTIM(,DATETIME,F1_NEWEST_BTIM,)
           ELSE
              DATETIME = '      NONE'
           END IF
           IF (DIFF.GE.0.OR.F1_NBULL.EQ.0) THEN
              WRITE (6,1000) ' '//FOLDER1,DATETIME,F1_NBULL,FOLDER1_OWNER
           ELSE
              WRITE (6,1000) '*'//FOLDER1,DATETIME,F1_NBULL,FOLDER1_OWNER
           END IF
           IF (NLINE.EQ.2) WRITE (6,'(1X,A)') FOLDER1_DESCRIP
           FOLDER_COUNT = FOLDER_COUNT + 1      ! Update folder counter
        END DO

        IF (FOLDER_COUNT.GT.NUM_FOLDER) THEN    ! Outputted all entries?
           FOLDER_COUNT = 0                     ! Yes. Set counter to 0.
        ELSE
           WRITE(6,1010)                        ! Else say there are more
        END IF

        RETURN

1000    FORMAT(1X,A26,2X,A17,2X,I8,2X,A12)
1010    FORMAT(1X,/,' Press RETURN for more...',/)

        END


        SUBROUTINE SET_ACCESS(ACCESS)
C
C  SUBROUTINE SET_ACCESS
C
C  FUNCTION: Set access on folder for specified ID.
C
C  PARAMETERS:
C       ACCESS  -  Logical: If .true., grant access, if .false. deny access
C

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLUSER.INC'

        INCLUDE 'BULLFILES.INC'

        INCLUDE '($SSDEF)'

        LOGICAL ACCESS,ALL,READONLY

        EXTERNAL CLI$_ABSENT

        CHARACTER ID*64,RESPONSE*1

        CHARACTER INPUT*132

        IF (CLI$PRESENT('ALL')) THEN
           ALL = .TRUE.
        ELSE
           ALL = .FALSE.
        END IF

        IF (CLI$PRESENT('READONLY')) THEN
           READONLY = .TRUE.
        ELSE
           READONLY = .FALSE.
        END IF

        IER = CLI$GET_VALUE('ACCESS_FOLDER',FOLDER1,LEN) ! Get folder name

        IF (IER.EQ.%LOC(CLI$_ABSENT)) THEN
           FOLDER1 = FOLDER
        ELSE IF (LEN.GT.25) THEN
           WRITE(6,'('' ERROR: Folder name must be < 26 characters.'')')
           RETURN
        END IF

        CALL OPEN_BULLFOLDER            ! Open folder file
        CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER) ! See if it exists
        OLD_FOLDER1_FLAG = FOLDER1_FLAG
        CALL CLOSE_BULLFOLDER

        IF (IER.NE.0) THEN
           WRITE (6,'('' ERROR: No such folder exists.'')')
        ELSE IF (FOLDER1_OWNER.NE.USERNAME.AND..NOT.SETPRV_PRIV()) THEN
           WRITE (6,
     &  '('' ERROR: You are not able to modify access to the folder.'')')
        ELSE
           FOLDER1_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//
     &          FOLDER1
           CALL CHKACL
     &          (FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL',IER)
           IF (IER.EQ.(SS$_ACLEMPTY.OR.SS$_NORMAL)) THEN
             IF ((ALL.AND..NOT.READONLY).OR.(.NOT.ACCESS)) THEN
                WRITE (6,'('' ERROR: Folder is not a private folder.'')')
                RETURN
             END IF
             CALL GET_INPUT_PROMPT(RESPONSE,LEN,
     &      'Folder is not private. Do you want to make it so? (Y/N): ')
             IF (RESPONSE.NE.'y'.AND.RESPONSE.NE.'Y') THEN
               WRITE (6,'('' Folder access was not changed.'')')
               RETURN
             ELSE
               FOLDER1_FLAG = IBSET(FOLDER1_FLAG,0)
               IF (READONLY.AND.ALL) THEN
                  CALL ADD_ACL('*','R',IER)
               ELSE
                  CALL ADD_ACL('*','NONE',IER)
               END IF
               CALL ADD_ACL(FOLDER1_OWNER,'R+W+C',IER)
               IF (ALL) THEN            ! All finished, so exit
                WRITE (6,'('' Access to folder has been modified.'')')
                GOTO 100
               END IF
             END IF
           END IF

           IF (ALL) THEN
              IF (ACCESS) THEN
                 CALL DEL_ACL(' ','R+W',IER)
                 IF (READONLY) THEN
                    CALL ADD_ACL('*','R',IER)
                 ELSE
                    FOLDER1_FLAG = IBCLR(FOLDER1_FLAG,0)
                 END IF
              ELSE
                 CALL DEL_ACL('*','R',IER)
              END IF
              IF (.NOT.IER) THEN
                 WRITE(6,'('' Cannot modify access.'')')
                 CALL SYS_GETMSG(IER)
              END IF
           END IF

           DO WHILE (CLI$GET_VALUE('ACCESS_ID',INPUT,ILEN)
     &      .NE.%LOC(CLI$_ABSENT).AND..NOT.ALL)
              IER = SYS_TRNLNM(INPUT,INPUT)
              IF (INPUT(:1).EQ.'@') THEN
                 ILEN = INDEX(INPUT,',') - 1
                 IF (ILEN.EQ.-1) ILEN = TRIM(INPUT)
                 OPEN (UNIT=3,STATUS='OLD',FILE=INPUT(2:ILEN),
     &                  DEFAULTFILE='.DIS',IOSTAT=IER)
                 IF (IER.NE.0) THEN
                    WRITE (6,'('' ERROR: Cannot find file '',A)')
     &                                  INPUT(2:ILEN)
                    RETURN
                 END IF
                 READ (3,'(A)',IOSTAT=IER) INPUT
                 IF (IER.NE.0) THEN
                    CLOSE (UNIT=3)
                    INPUT = ' '
                 ELSE
                    FILE_OPEN = .TRUE.
                 END IF
              ELSE
                 FILE_OPEN = .FALSE.
              END IF
              DO WHILE (TRIM(INPUT).GT.0)
                 COMMA = INDEX(INPUT,',')
                 IF (INPUT(:1).EQ.'[') COMMA = INDEX(INPUT,']') + 1
                 IF (INPUT(:1).EQ.'"') COMMA = INDEX(INPUT(2:),'"') + 2
                 IF (COMMA.GT.0) THEN
                    ID = INPUT(1:COMMA-1)
                    INPUT = INPUT(COMMA+1:)
                 ELSE
                    ID = INPUT
                    INPUT = ' '
                 END IF
                 ILEN = TRIM(ID)
                 IF (ID.EQ.FOLDER1_OWNER) THEN
                    WRITE (6,'('' ERROR: Cannot modify access'',
     &                         '' for owner of folder.'')')
                 ELSE
                    IF (ACCESS) THEN
                       IF (READONLY) THEN
                          CALL ADD_ACL(ID,'R',IER)
                       ELSE
                          CALL ADD_ACL(ID,'R+W',IER)
                       END IF
                    ELSE
                       CALL DEL_ACL(ID,'R+W',IER)
                       IF (.NOT.IER) CALL DEL_ACL(ID,'R',IER)
                    END IF
                    IF (.NOT.IER) THEN
                       WRITE(6,'('' Cannot modify access for '',A,
     &                                  ''.'')') ID(:ILEN)
                       CALL SYS_GETMSG(IER)
                    ELSE
                       WRITE(6,'('' Access modified for '',A,''.'')')
     &                          ID(:ILEN)
                    END IF
                 END IF
                 IF (TRIM(INPUT).EQ.0.AND.FILE_OPEN) THEN
                    READ (3,'(A)',IOSTAT=IER) INPUT
                    IF (IER.NE.0) THEN
                       CLOSE (UNIT=3)
                       INPUT = ' '
                       FILE_OPEN = .FALSE.
                    END IF
                 END IF
              END DO
           END DO
           
100        IF (OLD_FOLDER1_FLAG.NE.FOLDER1_FLAG) THEN
              CALL OPEN_BULLFOLDER              ! Open folder file
              OLD_FOLDER1_FLAG = FOLDER1_FLAG
              CALL READ_FOLDER_FILE_KEYNAME_TEMP(FOLDER1,IER)
              FOLDER1_FLAG = OLD_FOLDER1_FLAG
              CALL REWRITE_FOLDER_FILE_TEMP
              CALL CLOSE_BULLFOLDER
           END IF
        END IF

        RETURN

        END



        SUBROUTINE CHKACL(FILENAME,IERACL)
C
C  SUBROUTINE CHKACL
C
C  FUNCTION: Checks ACL of given file.
C
C  PARAMETERS:
C       FILENAME - Name of file to check.
C       IERACL   - Error returned for attempt to open file.
C

        IMPLICIT INTEGER (A-Z)

        CHARACTER*(*) FILENAME

        INCLUDE '($ACLDEF)'
        INCLUDE '($SSDEF)'

        CHARACTER*255 ACLENT,ACLSTR

        CALL INIT_ITMLST        ! Initialize item list
        CALL ADD_2_ITMLST(255,ACL$C_READACL,%LOC(ACLENT))
        CALL END_ITMLST(ACL_ITMLST)     ! Get address of itemlist

        IERACL=SYS$CHANGE_ACL(,ACL$C_FILE,FILENAME,%VAL(ACL_ITMLST),,,)

        IF (IERACL.EQ.SS$_ACLEMPTY) THEN
           IERACL = SS$_NORMAL.OR.IERACL
        END IF

        RETURN
        END



        SUBROUTINE CHECK_ACCESS(FILENAME,USERNAME,READ_ACCESS,WRITE_ACCESS)
C
C  SUBROUTINE CHECK_ACCESS
C
C  FUNCTION: Checks ACL of given file.
C
C  PARAMETERS:
C       FILENAME - Name of file to check.
C       USERNAME - Name of user to check access for.
C       READ_ACCESS - Error returned indicating read access.
C       WRITE_ACCESS - Error returned indicating write access.
C                      If initially set to -1, indicates just
C                      folder for read access.
C

        IMPLICIT INTEGER (A-Z)

        CHARACTER FILENAME*(*),USERNAME*(*),ACE*255,OUTPUT*80

        INCLUDE '($ACLDEF)'
        INCLUDE '($CHPDEF)'
        INCLUDE '($ARMDEF)'

        CALL INIT_ITMLST        ! Initialize item list
        CALL ADD_2_ITMLST(4,CHP$_FLAGS,%LOC(FLAGS))
        CALL ADD_2_ITMLST(4,CHP$_ACCESS,%LOC(ACCESS))
        CALL ADD_2_ITMLST(LEN(ACE),CHP$_MATCHEDACE,%LOC(ACE))
        CALL END_ITMLST(ACL_ITMLST)     ! Get address of itemlist

        FLAGS = 0               ! Default is no access

        ACCESS = ARM$M_READ     ! Check if user has read access
        READ_ACCESS=SYS$CHECK_ACCESS(ACL$C_FILE,FILENAME,USERNAME,
     &          %VAL(ACL_ITMLST))

        IF (.NOT.SETPRV_PRIV().AND.ICHAR(ACE(:1)).NE.0) THEN
           CALL SYS$FORMAT_ACL(ACE,,OUTPUT,,,,)
           IF (INDEX(OUTPUT,'=*').NE.0.AND.
     &          INDEX(OUTPUT,'READ').EQ.0) READ_ACCESS = 0
        ELSE IF (ICHAR(ACE(:1)).EQ.0.AND.READ_ACCESS) THEN
           READ_ACCESS = 0
        END IF

        IF (WRITE_ACCESS.EQ.-1) THEN    ! Only check read access
           RETURN
        ELSE IF (READ_ACCESS.EQ.0) THEN ! If no read access, then of
           WRITE_ACCESS = 0             ! course there is no write access.
           RETURN
        END IF

        ACCESS = ARM$M_WRITE    ! Check if user has write access
        WRITE_ACCESS=SYS$CHECK_ACCESS(ACL$C_FILE,FILENAME,USERNAME,
     &          %VAL(ACL_ITMLST))

        IF (.NOT.SETPRV_PRIV().AND.ICHAR(ACE(:1)).NE.0) THEN
           CALL SYS$FORMAT_ACL(ACE,,OUTPUT,,,,)
           IF (INDEX(OUTPUT,'=*').NE.0.AND.
     &          INDEX(OUTPUT,'WRITE').EQ.0) WRITE_ACCESS = 0
        END IF

        RETURN
        END




        SUBROUTINE SHOWACL(FILENAME)
C
C  SUBROUTINE SHOWACL
C
C  FUNCTION: Shows users who are allowed to read private bulletin.
C
C  PARAMETERS:
C       FILENAME - Name of file to check.
C
        IMPLICIT INTEGER (A-Z)

        INCLUDE '($ACLDEF)'

        CHARACTER*(*) FILENAME

        CALL INIT_ITMLST        ! Initialize item list
        CALL ADD_2_ITMLST(4,ACL$C_ACLLENGTH,%LOC(ACLLENGTH))
        CALL END_ITMLST(ACL_ITMLST)     ! Get address of itemlist

        IER = SYS$CHANGE_ACL(,ACL$C_FILE,FILENAME,%VAL(ACL_ITMLST),,,)

        CALL LIB$GET_VM(ACLLENGTH+8,ACLSTR)
        CALL MAKE_CHAR(%VAL(ACLSTR),ACLLENGTH,ACLLENGTH)

        CALL READACL(FILENAME,%VAL(ACLSTR),ACLLENGTH)

        RETURN
        END



        SUBROUTINE FOLDER_FILE_ROUTINES

        IMPLICIT INTEGER (A-Z)

        CHARACTER*(*) KEY_NAME

        INCLUDE 'BULLFOLDER.INC'

        ENTRY WRITE_FOLDER_FILE(IER)

        DO WHILE (REC_LOCK(IER))
           WRITE (7,IOSTAT=IER) FOLDER_COM
        END DO

        RETURN

        ENTRY REWRITE_FOLDER_FILE

        REWRITE (7) FOLDER_COM

        RETURN

        ENTRY REWRITE_FOLDER_FILE_TEMP

        REWRITE (7) FOLDER1_COM

        RETURN

        ENTRY READ_FOLDER_FILE(IER)

        DO WHILE (REC_LOCK(IER))
           READ (7,IOSTAT=IER) FOLDER_COM
        END DO

        RETURN

        ENTRY READ_FOLDER_FILE_TEMP(IER)

        DO WHILE (REC_LOCK(IER))
           READ (7,IOSTAT=IER) FOLDER1_COM
        END DO

        RETURN

        ENTRY READ_FOLDER_FILE_KEYNUM(KEY_NUMBER,IER)

        SAVE_FOLDER_NUMBER = FOLDER_NUMBER

        DO WHILE (REC_LOCK(IER))
           READ (7,KEY=KEY_NUMBER,KEYID=1,IOSTAT=IER) FOLDER_COM
        END DO

        FOLDER_NUMBER = SAVE_FOLDER_NUMBER

        RETURN

        ENTRY READ_FOLDER_FILE_KEYNUM_TEMP(KEY_NUMBER,IER)

        DO WHILE (REC_LOCK(IER))
           READ (7,KEY=KEY_NUMBER,KEYID=1,IOSTAT=IER) FOLDER1_COM
        END DO

        RETURN

        ENTRY READ_FOLDER_FILE_KEYNAME_TEMP(KEY_NAME,IER)

        DO WHILE (REC_LOCK(IER))
           READ (7,KEY=KEY_NAME,KEYID=0,IOSTAT=IER) FOLDER1_COM
        END DO

        RETURN

        ENTRY READ_FOLDER_FILE_KEYNAME(KEY_NAME,IER)

        DO WHILE (REC_LOCK(IER))
           READ (7,KEY=KEY_NAME,KEYID=0,IOSTAT=IER) FOLDER_COM
        END DO

        RETURN

        END


        SUBROUTINE USER_FILE_ROUTINES

        IMPLICIT INTEGER (A-Z)

        CHARACTER*(*) KEY_NAME

        INCLUDE 'BULLUSER.INC'

        CHARACTER*12 SAVE_USERNAME

        ENTRY READ_USER_FILE(IER)

        SAVE_USERNAME = USERNAME

        DO WHILE (REC_LOCK(IER))
           READ (4,IOSTAT=IER) USER_ENTRY
        END DO

        TEMP_USER = USERNAME
        USERNAME = SAVE_USERNAME

        RETURN

        ENTRY READ_USER_FILE_KEYNAME(KEY_NAME,IER)

        SAVE_USERNAME = USERNAME

        DO WHILE (REC_LOCK(IER))
           READ (4,KEY=KEY_NAME,IOSTAT=IER) USER_ENTRY
        END DO

        USERNAME = SAVE_USERNAME
        TEMP_USER = KEY_NAME

        RETURN

        ENTRY READ_USER_FILE_HEADER(IER)

        DO WHILE (REC_LOCK(IER))
           READ (4,KEY='            ',IOSTAT=IER) USER_HEADER
        END DO

        RETURN

        ENTRY WRITE_USER_FILE_NEW(IER)

        SET_FLAG(1) = SET_FLAG_DEF(1)
        SET_FLAG(2) = SET_FLAG_DEF(2)
        BRIEF_FLAG(1) = BRIEF_FLAG_DEF(1)
        BRIEF_FLAG(2) = BRIEF_FLAG_DEF(2)
        NOTIFY_FLAG(1) = NOTIFY_FLAG_DEF(1)
        NOTIFY_FLAG(2) = NOTIFY_FLAG_DEF(2)

        ENTRY WRITE_USER_FILE(IER)

        DO WHILE (REC_LOCK(IER))
           WRITE (4,IOSTAT=IER) USER_ENTRY
        END DO

        RETURN

        END





        SUBROUTINE SET_GENERIC(GENERIC)
C
C  SUBROUTINE SET_GENERIC
C
C  FUNCTION: Enables or disables "GENERIC" display, i.e. displaying
C       general bulletins continually for a certain amount of days.
C
        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLUSER.INC'

        COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
        CHARACTER*64 BULL_PARAMETER

        IF (.NOT.SETPRV_PRIV()) THEN
           WRITE (6,'(
     &      '' ERROR: No privs to change GENERIC.'')')
           RETURN
        END IF

        IER = CLI$GET_VALUE('USERNAME',TEMP_USER)

        CALL OPEN_BULLUSER_SHARED

        CALL READ_USER_FILE_KEYNAME(TEMP_USER,IER)

        IF (IER.EQ.0) THEN
           IF (GENERIC) THEN
              IF (CLI$PRESENT('DAYS')) THEN
                 IER = CLI$GET_VALUE('DAYS',BULL_PARAMETER)
                 CALL LIB$MOVC3(4,%REF(BULL_PARAMETER),NEW_FLAG(2))
              ELSE
                 NEW_FLAG(2) = '   7'
              END IF
           ELSE
              NEW_FLAG(2) = 0
           END IF
           REWRITE (4) TEMP_USER//USER_ENTRY(13:)
        ELSE
           WRITE (6,'('' ERROR: Specified username not found.'')')
        END IF

        CALL CLOSE_BULLUSER

        RETURN
        END


        SUBROUTINE SET_LOGIN(LOGIN)
C
C  SUBROUTINE SET_LOGIN
C
C  FUNCTION: Enables or disables bulletin display at login.
C
        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLUSER.INC'

        CHARACTER TODAY*23

        DIMENSION NOLOGIN_BTIM(2)

        CALL SYS$ASCTIM(,TODAY,,)               ! Get the present time

        IF (.NOT.SETPRV_PRIV()) THEN
           WRITE (6,'(
     &      '' ERROR: No privs to change LOGIN.'')')
           RETURN
        END IF

        IER = CLI$GET_VALUE('USERNAME',TEMP_USER)

        CALL OPEN_BULLUSER_SHARED

        CALL READ_USER_FILE_KEYNAME(TEMP_USER,IER)

        CALL SYS_BINTIM('5-NOV-2956 00:00:00.00',NOLOGIN_BTIM)
        IF (IER.EQ.0) THEN
           IF (LOGIN.AND.COMPARE_BTIM(LOGIN_BTIM,NOLOGIN_BTIM).GE.0) THEN
              CALL SYS_BINTIM(TODAY,LOGIN_BTIM)
           ELSE IF (.NOT.LOGIN) THEN
              LOGIN_BTIM(1) = NOLOGIN_BTIM(1)
              LOGIN_BTIM(2) = NOLOGIN_BTIM(2)
           END IF
           REWRITE (4) TEMP_USER//USER_ENTRY(13:)
        ELSE
           WRITE (6,'('' ERROR: Specified username not found.'')')
        END IF

        CALL CLOSE_BULLUSER

        RETURN
        END





        SUBROUTINE GET_UAF(USERNAME,USER,GROUP,ACCOUNT,FLAGS,IER)

        IMPLICIT INTEGER (A-Z)

        CHARACTER USERNAME*(*),ACCOUNT*(*)

        INCLUDE '($UAIDEF)'

        INTEGER*2 UIC(2)

        CALL INIT_ITMLST
        CALL ADD_2_ITMLST(4,UAI$_FLAGS,%LOC(FLAGS))
        CALL ADD_2_ITMLST(LEN(ACCOUNT),UAI$_ACCOUNT,%LOC(ACCOUNT))
        CALL ADD_2_ITMLST(4,UAI$_UIC,%LOC(UIC))
        CALL END_ITMLST(GETUAI_ITMLST)

        IER = SYS$GETUAI(,,USERNAME,%VAL(GETUAI_ITMLST),,,)

        USER = UIC(1)
        GROUP = UIC(2)

        RETURN
        END



        SUBROUTINE DCLEXH(EXIT_ROUTINE)

        IMPLICIT INTEGER (A-Z)

        INTEGER*4 EXBLK(4)

        EXBLK(2) = EXIT_ROUTINE
        EXBLK(3) = 1
        EXBLK(4) = %LOC(EXBLK(4))

        CALL SYS$DCLEXH(EXBLK(1))

        RETURN
        END
