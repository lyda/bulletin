C
C  BULLETIN9.FOR, Version 6/1/89
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: MIT PFC VAX-11/780, VMS
C  Programmer: Mark R. London
C
        SUBROUTINE HELP(LIBRARY)

        IMPLICIT INTEGER (A-Z)

        CHARACTER*(*) LIBRARY

        COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
        CHARACTER*64 BULL_PARAMETER

        IER = CLI$GET_VALUE('HELP_FOLDER',BULL_PARAMETER,LEN_P)
        IF (.NOT.IER) BULL_PARAMETER = ' '

        CALL OUTPUT_HELP(BULL_PARAMETER(1:LEN_P),LIBRARY)

        RETURN
        END




        SUBROUTINE GET_NODE_INFO
C
C  SUBROUTINE GET_NODE_INFO
C
C  FUNCTION: Gets local node name and obtains node names from
C       command line.
C

        IMPLICIT INTEGER (A-Z)

        EXTERNAL CLI$_ABSENT

        COMMON /NODE_INFO/ NODES,LOCAL_NODE_FOUND,NODE_NUM,
     &                          NODE_ERROR,POINT_NODE
        CHARACTER*32 NODES(10)
        LOGICAL LOCAL_NODE_FOUND,NODE_ERROR

        CHARACTER LOCAL_NODE*32,NODE_TEMP*256

        NODE_ERROR = .FALSE.

        LOCAL_NODE_FOUND = .FALSE.
        CALL LIB$SYS_TRNLOG('SYS$NODE',L_NODE,LOCAL_NODE)
        L_NODE = L_NODE - 2                     ! Remove '::'
        IF (LOCAL_NODE(1:1).EQ.'_') THEN
           LOCAL_NODE = LOCAL_NODE(2:)
           L_NODE = L_NODE - 1
        END IF

        NODE_NUM = 0                            ! Initialize number of nodes
        IF (CLI$PRESENT('NODES')) THEN          ! Decnet nodes specified?
           DO WHILE (CLI$GET_VALUE('NODES',NODE_TEMP)
     &      .NE.%LOC(CLI$_ABSENT))              ! Get the specified nodes
            IER = SYS_TRNLNM(NODE_TEMP,NODE_TEMP)
            DO WHILE (TRIM(NODE_TEMP).GT.0)
              NODE_NUM = NODE_NUM + 1
              COMMA = INDEX(NODE_TEMP,',')
              IF (COMMA.GT.0) THEN
                 NODES(NODE_NUM) = NODE_TEMP(1:COMMA-1)
                 NODE_TEMP = NODE_TEMP(COMMA+1:)
              ELSE
                 NODES(NODE_NUM) = NODE_TEMP
                 NODE_TEMP = ' '
              END IF
              NLEN = TRIM(NODES(NODE_NUM))
              IF (INDEX(NODES(NODE_NUM),'::').GT.0) THEN   ! Remove :: if
                 NLEN = INDEX(NODES(NODE_NUM),'::') - 1    ! addedd
              END IF
              IF (LOCAL_NODE(1:L_NODE).EQ.NODES(NODE_NUM)(1:NLEN)) THEN
               NODE_NUM = NODE_NUM - 1
               LOCAL_NODE_FOUND = .TRUE.
              ELSE
               POINT_NODE = NODE_NUM
               OPEN (UNIT=9+NODE_NUM,NAME=NODES(NODE_NUM)(1:NLEN)//'""::'
     &         //'"TASK=BULLETIN"',ACCESS='SEQUENTIAL',FORM='FORMATTED',
     &         CARRIAGECONTROL='NONE',TYPE='NEW',IOSTAT=IER)
               IF (IER.NE.0) THEN
                  DO WHILE (NODE_NUM.GT.0)
                     CLOSE(UNIT=9+NODE_NUM)
                     NODE_NUM = NODE_NUM - 1
                  END DO
                  NODE_ERROR = .TRUE.
                  RETURN
               END IF
              END IF
            END DO
           END DO
        ELSE
           LOCAL_NODE_FOUND = .TRUE.
        END IF

        RETURN
        END




        SUBROUTINE DELETE_NODE
C
C  SUBROUTINE DELETE_NODE
C
C  FUNCTION: Deletes files sent via ADD/NODES at remote hosts.
C

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLUSER.INC'

        INCLUDE 'BULLDIR.INC'

        COMMON /NODE_INFO/ NODES,LOCAL_NODE_FOUND,NODE_NUM,
     &                          NODE_ERROR,POINT_NODE
        CHARACTER*32 NODES(10)
        LOGICAL LOCAL_NODE_FOUND,NODE_ERROR

        CHARACTER PASSWORD*31,INLINE*80,DEFAULT_USER*12

        CALL GET_NODE_INFO

        IF (NODE_ERROR) GO TO 940

        IF (NODE_NUM.EQ.0.OR.LOCAL_NODE_FOUND) THEN
           WRITE (6,'('' ERROR: Cannot specify local node.'')')
           GO TO 999
        END IF

        IER = CLI$GET_VALUE('USERNAME',DEFAULT_USER)
        IF (.NOT.IER) DEFAULT_USER = USERNAME
        IER = CLI$GET_VALUE('SUBJECT',DESCRIP)

        DO POINT_NODE=1,NODE_NUM                ! Write out command to nodes
           SEMI = INDEX(NODES(POINT_NODE),'::') ! Look for semicolon after node
           NLEN = TRIM(NODES(POINT_NODE))       ! Length of node name
           IF (SEMI.GT.0) THEN                  ! Is semicolon present?
              IF (NLEN.GT.SEMI+1) THEN          ! Yes, is username after node?
                 TEMP_USER = NODES(POINT_NODE)(SEMI+2:) ! Yes, set username
                 NLEN = SEMI - 1                ! Remove semicolon
              ELSE                              ! No username after nodename
                 TEMP_USER = DEFAULT_USER       ! Set username to default
                 NLEN = SEMI - 1                ! Remove semicolon
                 SEMI = 0                       ! Indicate no username
              END IF
           ELSE                                 ! No semicolon present
              TEMP_USER = DEFAULT_USER          ! Set username to default
           END IF
           INLINE = 'DELETE/SUBJECT="'//DESCRIP(:TRIM(DESCRIP))//
     &      '"/USERNAME='//TEMP_USER(:TRIM(TEMP_USER))
           IF (CLI$PRESENT('USERNAME').OR.SEMI.GT.0) THEN  ! If username was
              IER = 1                           ! specified, prompt for password
              DO WHILE (IER.NE.0)
                 WRITE(6,'('' Enter password for node '',2A)')
     &                  NODES(POINT_NODE),CHAR(10)
                 CALL GET_INPUT_NOECHO(PASSWORD)
                 IF (TRIM(PASSWORD).EQ.0) GO TO 910
                 OPEN (UNIT=10+NODE_NUM,NAME=NODES(POINT_NODE)(:NLEN)
     &             //'"'//TEMP_USER(1:TRIM(TEMP_USER))//' '//
     &             PASSWORD(1:TRIM(PASSWORD))//'"::',
     &             TYPE='SCRATCH',IOSTAT=IER)
                 CLOSE (UNIT=10+NODE_NUM)
                 IF (IER.NE.0) THEN
                    WRITE (6,'('' ERROR: Password is invalid.'')')
                 END IF
              END DO
           END IF
           WRITE (POINT_NODE+9,'(A)',ERR=940) INLINE
           READ (POINT_NODE+9,'(A)',ERR=940,END=940) INLINE
           IF (INLINE.EQ.'END') THEN
              WRITE (6,'('' Message successfully deleted from node '',A)')
     &                          NODES(POINT_NODE)
           ELSE
              WRITE (6,'('' Error while deleting message to node '',A)')
     &                          NODES(POINT_NODE)
              WRITE (6,'(A)') INLINE
           END IF
        END DO

        GO TO 999

910     WRITE (6,1010)
        GO TO 999

940     WRITE (6,1015) NODES(POINT_NODE)

999     DO WHILE (NODE_NUM.GT.0)
           CLOSE(UNIT=9+NODE_NUM)
           NODE_NUM = NODE_NUM - 1
        END DO

        RETURN

1010    FORMAT (' ERROR: Deletion aborted.')
1015    FORMAT (' ERROR: Unable to reach node ',A)

        END




        SUBROUTINE SET_FOLDER_FLAG(SETTING,FLAG,FLAGNAME)
C
C  SUBROUTINE SET_FOLDER_FLAG
C
C  FUNCTION: Sets or clears specified flag for folder
C
        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLUSER.INC'

        INCLUDE 'BULLFILES.INC'

        CHARACTER*(*) FLAGNAME

        IF (FOLDER_OWNER.EQ.USERNAME.OR.SETPRV_PRIV()) THEN
           CALL OPEN_BULLFOLDER         ! Open folder file

           CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)

           IF (SETTING) THEN
              FOLDER_FLAG = IBSET(FOLDER_FLAG,FLAG)
           ELSE
              FOLDER_FLAG = IBCLR(FOLDER_FLAG,FLAG)
           END IF

           CALL REWRITE_FOLDER_FILE

           CALL CLOSE_BULLFOLDER

           WRITE (6,'(1X,A,'' has been modified for folder.'')')
     &          FLAGNAME
        ELSE
           WRITE (6,'(1X,'' You are not authorized to modify '',A)')
     &          FLAGNAME//'.'
        END IF

        RETURN
        END




        SUBROUTINE SET_FOLDER_EXPIRE_LIMIT(LIMIT)
C
C  SUBROUTINE SET_FOLDER_EXPIRE_LIMIT
C
C  FUNCTION: Sets folder expiration limit.
C
        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLUSER.INC'

        INCLUDE 'BULLFILES.INC'

        IF (LIMIT.LT.0) THEN
           WRITE (6,'('' ERROR: Invalid expiration length specified.'')')
        ELSE IF (FOLDER_OWNER.EQ.USERNAME.OR.SETPRV_PRIV()) THEN
           CALL OPEN_BULLFOLDER         ! Open folder file

           CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)

           F_EXPIRE_LIMIT = LIMIT

           CALL REWRITE_FOLDER_FILE

           CALL CLOSE_BULLFOLDER
           WRITE (6,'('' Folder expiration date modified.'')')
        ELSE
           WRITE (6,'('' You are not allowed to modify folder.'')')
        END IF

        RETURN
        END





        SUBROUTINE MERGE

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLDIR.INC'

        CHARACTER*(DIR_RECORD_LENGTH) BULLDIR_ENTRY_SAVE

        ENTRY INITIALIZE_MERGE(IER1)

        DO WHILE (FILE_LOCK(IER1,IER2))
           OPEN (UNIT=13,FILE=FOLDER_FILE(:TRIM(FOLDER_FILE))
     &          //'.TMPDIR',STATUS='NEW',FORM='UNFORMATTED',
     &          RECORDTYPE='FIXED',RECORDSIZE=DIR_RECORD_LENGTH/4,
     &          ORGANIZATION='INDEXED',IOSTAT=IER,DISPOSE='DELETE',
     &          KEY=(9:12:INTEGER,1:8:CHARACTER),ACCESS='KEYED')
        END DO

        IF (IER1.NE.0) RETURN

        NBULL = 0

        WRITE(13,IOSTAT=IER1) BULLDIR_HEADER
        CALL CONVERT_HEADER_FROMBIN

        TO_POINTER = 1

        RETURN

        ENTRY ADD_MERGE_TO(IER1)
 
        IER1 = 0

        DO WHILE (IER1.EQ.0)

           BULLDIR_ENTRY_SAVE = BULLDIR_ENTRY

           CALL READDIR(TO_POINTER,IER)

           DIFF = COMPARE_BTIM(%REF(BULLDIR_ENTRY_SAVE),MSG_BTIM)
           IF (DIFF.LT.0.OR.TO_POINTER+1.NE.IER) THEN
              BULLDIR_ENTRY = BULLDIR_ENTRY_SAVE
              CALL CONVERT_ENTRY_FROMBIN
              RETURN
           END IF

           NBULL = NBULL + 1
           MSG_NUM = NBULL

           CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
           WRITE(13,IOSTAT=IER1) BULLDIR_ENTRY

           NEWEST_DATE = DATE
           NEWEST_TIME = TIME

           TO_POINTER = TO_POINTER + 1

           BULLDIR_ENTRY = BULLDIR_ENTRY_SAVE
        END DO

        CLOSE (UNIT=13)

        RETURN

        ENTRY ADD_MERGE_FROM(IER1)

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

        IF ((SYSTEM.AND.4).EQ.4) THEN
           SHUTDOWN = SHUTDOWN + 1
           SHUTDOWN_DATE = DATE
           SHUTDOWN_TIME = TIME
        END IF

        BLOCK = NBLOCK - LENGTH

        NBULL = NBULL + 1
        MSG_NUM = NBULL

        CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
        WRITE(13,IOSTAT=IER1) BULLDIR_ENTRY

        RETURN

        ENTRY ADD_MERGE_REST(IER1)

        CALL UPDATE_LOGIN(.TRUE.)

        DO WHILE (IER1.EQ.0)

           CALL READDIR(TO_POINTER,IER)
           IF (TO_POINTER+1.NE.IER) THEN
              READ (13,KEYID=0,KEY=0,IOSTAT=IER1)
              CALL CONVERT_HEADER_TOBIN
              REWRITE(13,IOSTAT=IER1) BULLDIR_HEADER
              IF (IER1.EQ.0) THEN
                 CLOSE (UNIT=13,DISPOSE='KEEP')
                 CALL LIB$RENAME_FILE(FOLDER_FILE(:TRIM(FOLDER_FILE))//
     &            '.TMPDIR',FOLDER_FILE(:TRIM(FOLDER_FILE))//'.BULLDIR')
              ELSE
                 CLOSE (UNIT=13)
              END IF
              RETURN
           END IF

           NBULL = NBULL + 1
           MSG_NUM = NBULL

           CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
           WRITE(13,IOSTAT=IER1) BULLDIR_ENTRY

           NEWEST_DATE = DATE
           NEWEST_TIME = TIME

           TO_POINTER = TO_POINTER + 1
        END DO

        CLOSE (UNIT=13)

        RETURN
        END




        SUBROUTINE SET_NOKEYPAD

        IMPLICIT INTEGER (A-Z)

        COMMON /SMG/ KEYBOARD_ID,KEY_TABLE_ID

        INCLUDE '($SMGDEF)'

        TERM = SMG$M_KEY_TERMINATE

        IER = SMG$SET_KEYPAD_MODE(KEYBOARD_ID,0)

        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PF2',,TERM,'SET KEYPAD',)

        RETURN
        END





        SUBROUTINE SET_KEYPAD

        IMPLICIT INTEGER (A-Z)

        COMMON /SMG/ KEYBOARD_ID,KEY_TABLE_ID

        INCLUDE '($SMGDEF)'

        TERM = SMG$M_KEY_TERMINATE

        IER = SMG$SET_KEYPAD_MODE(KEYBOARD_ID,1)

        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PF1',,,,'GOLD')
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PF2',,TERM,'HELP',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PF2','GOLD',TERM,'SET NOKEYPAD',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PF3',,,'EXTRACT ',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PF3','GOLD',,'FILE ',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PF4',,TERM,'SHOW KEYPAD',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PF4','GOLD',TERM,
     &          'SHOW KEYPAD/PRINT',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP0',,TERM,
     &          'SHOW FOLDER/FULL',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP0','GOLD',TERM,'SHOW FLAGS',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP1',,TERM,'BACK',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP1','GOLD',TERM,'NEXT',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP2',,TERM,'PRINT',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP2','GOLD',TERM,'PRINT/NONOTIFY',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP3',,TERM,'DIR',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP3','GOLD',TERM,'DIR/FOLDER',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP4',,TERM,'CURRENT',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP4','GOLD',TERM,'CURRENT/EDIT ',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP5',,TERM,'RESPOND',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP5','GOLD',TERM,'RESP/EDIT/TEXT',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP6',,TERM,'LAST',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP7',,TERM,'ADD',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP7','GOLD',TERM,'ADD/EDIT',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP8',,TERM,'REPLY',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP8','GOLD',TERM,'REPL/EDIT/TEXT',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP9',,TERM,'MAIL',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP9','GOLD',TERM,'MAIL/NOHEAD',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'MINUS',,TERM,'READ/NEW',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'MINUS','GOLD',TERM,'SHOW NEW',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'COMMA',,TERM,'DIR/NEW',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'COMMA','GOLD',TERM,'INDEX',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PERIOD',,TERM,'DELETE',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PERIOD','GOLD',TERM,'UNDELETE',)
        IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'ENTER','GOLD',,'SELECT ',)

        RETURN
        END



        SUBROUTINE SHOW_KEYPAD(LIBRARY)

        IMPLICIT INTEGER (A-Z)
        EXTERNAL LIB$PUT_OUTPUT,PRINT_OUTPUT
        CHARACTER*(*) LIBRARY

        INCLUDE '($HLPDEF)'

        IF (CLI$PRESENT('PRINT')) THEN
           OPEN (UNIT=8,STATUS='NEW',FILE='SYS$PRINT:KEYPAD.DAT',
     &                  IOSTAT=IER)
           IF (IER.NE.0) THEN
              WRITE (6,'('' ERROR WHILE OPENING FILE TO PRINTER.'')')
           ELSE
              CALL LBR$OUTPUT_HELP(PRINT_OUTPUT,,'KEYPAD'
     &          ,LIBRARY,HLP$M_HELP)
              CLOSE (UNIT=8)
           END IF
        ELSE
           CALL LBR$OUTPUT_HELP(LIB$PUT_OUTPUT,,'KEYPAD'
     &          ,LIBRARY,HLP$M_HELP)
        END IF

        RETURN
        END

        INTEGER FUNCTION PRINT_OUTPUT(INPUT)
        IMPLICIT INTEGER (A-Z)
        CHARACTER*(*) INPUT
        WRITE (8,'(1X,A)',IOSTAT=IER) INPUT(:TRIM(INPUT))
        IF (IER.EQ.0) PRINT_OUTPUT = 1
        RETURN
        END



        SUBROUTINE OUTPUT_HELP(PARAMETER,LIBRARY)
C
C  SUBROUTINE OUTPUT_HELP
C
C  FUNCTION:
C       To create interactive help session.  Prompting is enabled.
C  INPUTS:
C       PARAMETER - Character string. Optional input parameter
C                   containing a list of help keys.
C       LIBRARY   - Character string. Name of help library.
C
        IMPLICIT INTEGER (A-Z)

        INCLUDE '($LBRDEF)'

        COMMON /HELP/ HELP_PAGE,DISPLAY_ID,HELP_INPUT,HELP_INPUT_LEN
        COMMON /HELP/ NEED_ERASE,KEYBOARD_ID,KEY_TABLE_ID
        CHARACTER*80 HELP_INPUT

        COMMON /LEVELS/ KEY,KEYL,NKEY,OLD_NKEY,EXACT
        CHARACTER*20 KEY(10)
        DIMENSION KEYL(10)

        COMMON /PAGE/ PAGE_LENGTH,PAGE_WIDTH,PAGING

        EXTERNAL PUT_OUTPUT

        CHARACTER*(*) LIBRARY,PARAMETER

        CHARACTER*80 PROMPT

        DATA DISPLAY_ID/0/,KEYBOARD_ID/0/

        IER = SMG$CREATE_PASTEBOARD(PASTEBOARD_ID)      ! Initialize terminal
        IF (DISPLAY_ID.EQ.0) THEN
           IER = SMG$CREATE_VIRTUAL_DISPLAY(PAGE_LENGTH,
     &                                  PAGE_WIDTH,DISPLAY_ID)
        END IF
        IER = SMG$PASTE_VIRTUAL_DISPLAY(DISPLAY_ID,PASTEBOARD_ID,1,1)

        IF (KEYBOARD_ID.EQ.0) THEN
           IER = SMG$CREATE_VIRTUAL_KEYBOARD(KEYBOARD_ID,,,,20)
           IER = SMG$CREATE_KEY_TABLE(KEY_TABLE_ID)
        END IF

        CALL STR$TRIM(HELP_INPUT,PARAMETER,HELP_INPUT_LEN)      ! Trim input

        CALL LBR$INI_CONTROL(LINDEX,LBR$C_READ)         ! Init library read
        CALL LBR$OPEN(LINDEX,LIBRARY)                   ! Specify library name

        DO I=1,10                                       ! Initialize key lengths
           KEYL(I) = 0
        END DO

        NKEY = 0                                        ! Number of help keys

        DO WHILE (1)            ! Do until CTRL-Z entered or no more keys

           HELP_PAGE = 0                                ! Init line counter
           NEED_ERASE = .TRUE.                          ! Need to erase screen

           OLD_NKEY = NKEY                              ! Save old key count
           EXACT = .TRUE.                               ! Exact key match

           DO WHILE (NKEY.LT.10.AND.HELP_INPUT_LEN.GT.0.AND.
     &                                     HELP_INPUT(:1).NE.'?')
                                                        ! Break input into keys
              NKEY = NKEY + 1                           ! Increment key counter

              DO WHILE (HELP_INPUT(1:1).EQ.' '.AND.HELP_INPUT_LEN.GT.0)
                 HELP_INPUT = HELP_INPUT(2:HELP_INPUT_LEN)      ! Strip spaces
                 HELP_INPUT_LEN = HELP_INPUT_LEN - 1    ! at start of input
              END DO

              NEXT_KEY = 2

              DO WHILE (NEXT_KEY.LE.HELP_INPUT_LEN              ! Search for
     &            .AND.HELP_INPUT(NEXT_KEY:NEXT_KEY).NE.' '     ! space or
     &            .AND.HELP_INPUT(NEXT_KEY:NEXT_KEY).NE.'/')    ! backslash
                 NEXT_KEY = NEXT_KEY + 1        ! indicating start of next key
              END DO

              IF (NEXT_KEY.GT.HELP_INPUT_LEN) THEN      ! Found the last key
                 KEY(NKEY) = HELP_INPUT(:HELP_INPUT_LEN)        ! Key string
                 KEYL(NKEY) = HELP_INPUT_LEN                    ! Key length
                 HELP_INPUT_LEN = 0
              ELSE                                      ! Found the next key
                 KEY(NKEY) = HELP_INPUT(:NEXT_KEY-1)
                 HELP_INPUT = HELP_INPUT(NEXT_KEY:HELP_INPUT_LEN)
                 KEYL(NKEY) = NEXT_KEY - 1
                 HELP_INPUT_LEN = HELP_INPUT_LEN - NEXT_KEY + 1
              END IF
           END DO
           HELP_INPUT_LEN = 0
           IER = LBR$GET_HELP(LINDEX,,PUT_OUTPUT,,      ! Display help
     &             KEY(1)(:KEYL(1)),KEY(2)(:KEYL(2)),
     &             KEY(3)(:KEYL(3)),KEY(4)(:KEYL(4)),KEY(5)(:KEYL(5)),
     &             KEY(6)(:KEYL(6)),KEY(7)(:KEYL(7)),KEY(8)(:KEYL(8)),
     &             KEY(9)(:KEYL(9)),KEY(10)(:KEYL(10)))

           IF (IER.EQ.0.AND.HELP_INPUT_LEN.GT.0) IER = 1
                ! IER = 0 special case means input given to full screen prompt

           IF (KEY(NKEY).EQ.'*'.OR..NOT.EXACT) THEN     ! If not exact match
              DO I=OLD_NKEY+1,NKEY                      ! then don't update
                 KEYL(I) = 0                            ! new keys
              END DO
              NKEY = OLD_NKEY
           END IF

           DO WHILE (HELP_INPUT_LEN.EQ.0.AND.IER.AND.NKEY.GE.0)
              IF (NKEY.EQ.0) THEN       ! If top level, prompt for topic
                 IER = SMG$READ_COMPOSED_LINE(KEYBOARD_ID,KEY_TABLE_ID,
     &             HELP_INPUT,'Topic? ',HELP_INPUT_LEN)
              ELSE                      ! If not top level, prompt for subtopic
                 LPROMPT = 0            ! Create subtopic prompt line
                 DO I=1,NKEY            ! Put spaces in between keys
                    PROMPT = PROMPT(:LPROMPT)//KEY(I)(:KEYL(I))//' '
                    LPROMPT = LPROMPT + KEYL(I) + 1
                 END DO
                 PROMPT = PROMPT(:LPROMPT)//'Subtopic? '
                 LPROMPT = LPROMPT + 10
                 IER = SMG$READ_COMPOSED_LINE(KEYBOARD_ID,KEY_TABLE_ID,
     &             HELP_INPUT,PROMPT(:LPROMPT),HELP_INPUT_LEN)
              END IF
              CALL STR$TRIM(HELP_INPUT,HELP_INPUT,HELP_INPUT_LEN)
              IF (IER.AND.HELP_INPUT_LEN.EQ.0) THEN     ! If RETURN entered
                 KEYL(NKEY) = 0                         ! Back up one key level
                 NKEY = NKEY - 1
              END IF
           END DO

           IF (.NOT.IER.OR.NKEY.LT.0) THEN      ! If CTRL-Z above top level,
              CALL LBR$CLOSE(LINDEX)            ! then close library,
              CALL SMG$UNPASTE_VIRTUAL_DISPLAY(DISPLAY_ID,PASTEBOARD_ID)
                                                ! remove virtual display
              RETURN                            ! and end help session.
           END IF

        END DO

        END



        INTEGER FUNCTION PUT_OUTPUT(INPUT,INFO,DATA,LEVEL)
C
C  FUNCTION PUT_OUTPUT
C
C  FUNCTION:
C       Output routine for input from LBR$GET_HELP.  Displays
C       help text on terminal with full screen prompting.
C  INPUTS:
C       INPUT - Character string.  Line of input text.
C       INFO  - Longword.  Contains help flag bits.
C       DATA  - Longword.  Not presently used.
C       LEVEL - Longword.  Contains current key level.
C
        IMPLICIT INTEGER (A-Z)

        INCLUDE '($HLPDEF)'

        COMMON /LEVELS/ KEY,KEYL,NKEY,OLD_NKEY,EXACT
        CHARACTER*20 KEY(10)
        DIMENSION KEYL(10)

        COMMON /HELP/ HELP_PAGE,DISPLAY_ID,HELP_INPUT,HELP_INPUT_LEN
        COMMON /HELP/ NEED_ERASE,KEYBOARD_ID,KEY_TABLE_ID
        CHARACTER*80 HELP_INPUT

        COMMON /PAGE/ PAGE_LENGTH,PAGE_WIDTH,PAGING

        CHARACTER INPUT*(*)

        CHARACTER SPACES*20
        DATA SPACES /' '/

        IF ((INFO.AND.HLP$M_NOHLPTXT).NE.0) THEN        ! Key cannot be found
           NEED_ERASE = .FALSE.                         ! Don't erase screen
           IF (HELP_PAGE.EQ.0) THEN             ! If first line of help text
              DO I=OLD_NKEY+1,NKEY              ! remove any new keys that
                 KEYL(I) = 0                    ! were inputted, as they are
              END DO                            ! not valid, as no match
              NKEY = OLD_NKEY                   ! could be found.
           END IF
        ELSE IF ((INFO.AND.HLP$M_KEYNAMLIN).NE.0.AND.NKEY.GT.0.AND.
     &           LEVEL.GT.OLD_NKEY.AND.KEY(NKEY)(:KEYL(NKEY)).NE.'*'.AND.
     &           %LOC(INPUT).NE.0) THEN         ! If text contains key names
                        ! Update if not wildcard search and they are new keys
           IF (KEYL(LEVEL).GT.0) THEN           ! If key already updated
              EXACT = .FALSE.           ! Must be more than one match possible
           END IF                       ! so indicate not exact match.
           START_KEY = 1                ! String preceeding spaces.
           DO WHILE (INPUT(START_KEY:START_KEY).EQ.' ')
              START_KEY = START_KEY + 1
           END DO
           KEY(LEVEL) = INPUT(START_KEY:)                       ! Store new key
           CALL STR$TRIM(KEY(LEVEL),KEY(LEVEL),KEYL(LEVEL))     ! & key length
        ELSE IF (HELP_PAGE.EQ.0) THEN           ! If first line of text,
           DO I=OLD_NKEY+1,NKEY                 ! remove any new keys that
              KEYL(I) = 0                       ! were just inputted, allowing
           END DO                               ! this routine to fill them.
        END IF

        IF (NEED_ERASE) THEN                    ! Need to erase screen?
           IER = SMG$ERASE_DISPLAY(DISPLAY_ID)  ! i.e. start of new topic.
           NEED_ERASE = .FALSE.
        END IF

        HELP_PAGE = HELP_PAGE + 1               ! Increment screen counter
        IF (PAGING.AND.HELP_PAGE.GT.PAGE_LENGTH-2) THEN         ! End of page?
           HELP_PAGE = 0                        ! Reinitialize screen counter
           CALL LIB$PUT_OUTPUT(' ')     ! Skip line and prompt for next screen
           IER = SMG$READ_COMPOSED_LINE(KEYBOARD_ID,KEY_TABLE_ID,
     &          HELP_INPUT,'Press RETURN to continue ... ',HELP_INPUT_LEN)
           CALL STR$TRIM(HELP_INPUT,HELP_INPUT,HELP_INPUT_LEN)  ! Trim input
           IF (.NOT.IER.OR.HELP_INPUT_LEN.GT.0) THEN    ! CTRL-Z or Text input?
              EXACT = .TRUE.    ! If more than one match was found and being
                                ! displayed, text input specifies that the
                                ! current displayed match is desired.
              PUT_OUTPUT = 0    ! Stop any more of current help display.
           ELSE                                 ! Else if RETURN entered
              IER = SMG$ERASE_DISPLAY(DISPLAY_ID)       ! Erase display
              NSPACES = LEVEL*2         ! Number of spaces to indent output
              IF ((INFO.AND.HLP$M_KEYNAMLIN).NE.0) NSPACES = NSPACES - 2
                ! Key name lines are indented 2 less than help description.
              IF (NSPACES.GT.0) THEN    ! Add spaces if present to output
                 PUT_OUTPUT =  LIB$PUT_OUTPUT(SPACES(:NSPACES)//INPUT)
              ELSE                      ! Else just output text.
                 PUT_OUTPUT =  LIB$PUT_OUTPUT(INPUT)
              END IF
              HELP_PAGE = 1             ! Increment page counter.
           END IF
        ELSE                            ! Else if not end of page
           NSPACES = LEVEL*2            ! Just output text line
           IF ((INFO.AND.HLP$M_KEYNAMLIN).NE.0) NSPACES = NSPACES - 2
           IF (NSPACES.GT.0) THEN
              PUT_OUTPUT = LIB$PUT_OUTPUT(SPACES(:NSPACES)//INPUT)
           ELSE
              PUT_OUTPUT = LIB$PUT_OUTPUT(INPUT)
           END IF
        END IF

        RETURN
        END




        SUBROUTINE SHOW_VERSION

        IMPLICIT INTEGER (A-Z)

        CHARACTER VERSION*10,DATE*23

        CALL READ_HEADER(VERSION,DATE)

        WRITE (6,'(A)') ' BULLETIN Version '//VERSION(:TRIM(VERSION))

        WRITE (6,'(A)') ' Linked on '//DATE(:TRIM(DATE))

        RETURN
        END






        SUBROUTINE TAG(ADD_OR_DEL)

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLDIR.INC'

        COMMON /TAGS/ BULL_TAG,READ_TAG
        DATA BULL_TAG /.FALSE./,READ_TAG /.FALSE./

        COMMON /POINT/ BULL_POINT

        COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
        CHARACTER*64 BULL_PARAMETER

        EXTERNAL CLI$_ABSENT

        IF (.NOT.BULL_TAG) THEN
           CALL OPEN_NEW_TAG(IER)
           IF (.NOT.IER) RETURN
        END IF

        IF (.NOT.CLI$PRESENT('NUMBER')) THEN
           IF (BULL_POINT.EQ.0) THEN    ! No.  Have we just read a bulletin?
              WRITE(6,1010)             ! No, then error.
              RETURN
           ELSE IF (ADD_OR_DEL) THEN
              CALL ADD_TAG(IER)
           ELSE
              CALL DEL_TAG(IER)
              IF (IER.NE.0) THEN
                 WRITE (6,'('' ERROR: Message was not marked.'')')
              END IF
           END IF
           RETURN
        END IF

        CALL OPEN_BULLDIR_SHARED

        IER1 = 0
        DO WHILE (CLI$GET_VALUE('NUMBER',BULL_PARAMETER,LEN_P)
     &      .NE.%LOC(CLI$_ABSENT).AND.IER1.EQ.0) ! Get the specified messages

           DECODE(LEN_P,'(I<LEN_P>)',BULL_PARAMETER) MESSAGE_NUMBER

           CALL READDIR(MESSAGE_NUMBER,IER)     ! Get info for bulletin

           IF (IER.NE.MESSAGE_NUMBER+1) THEN    ! Was bulletin found?
              WRITE(6,1030)                     ! If not, then error out
           ELSE IF (ADD_OR_DEL) THEN
              CALL ADD_TAG(IER1)
           ELSE
              CALL DEL_TAG(IER)
              IF (IER.NE.0) THEN
                 WRITE (6,'('' ERROR: Message '',I,
     &                          '' was not marked.'')') MESSAGE_NUMBER
              END IF
           END IF
        END DO

        CALL CLOSE_BULLDIR

        RETURN

1010    FORMAT(' ERROR: You have not read any message.')
1030    FORMAT(' ERROR: Message was not found.')

        END



        SUBROUTINE ADD_TAG(IER)

        IMPLICIT INTEGER (A-Z)

        INCLUDE '($FORIOSDEF)'

        INCLUDE 'BULLDIR.INC'

        INCLUDE 'BULLFOLDER.INC'

        CHARACTER*12 TAG_KEY

        WRITE (13,IOSTAT=IER) TAG_KEY(FOLDER_NUMBER,MSG_KEY)

        IF (IER.EQ.FOR$IOS_INCKEYCHG) THEN
           WRITE (6,'('' Message was already marked.'')')
        ELSE IF (IER.NE.0) THEN
           WRITE (6,'('' ERROR: Unable to add mark.'')')
           CALL ERRSNS(IDUMMY,IER1)
           IF (IER1.EQ.0) THEN
              WRITE (6,'('' IOSTAT error = '',I)') IER
           ELSE
              CALL SYS_GETMSG(IER1)
           END IF
        END IF

        RETURN
        END




        SUBROUTINE DEL_TAG(IER)

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLDIR.INC'

        INCLUDE 'BULLFOLDER.INC'

        CHARACTER*12 TAG_KEY

        DO WHILE (REC_LOCK(IER))
           READ (13,KEYEQ=TAG_KEY(FOLDER_NUMBER,MSG_KEY),IOSTAT=IER)
        END DO
        IF (IER.NE.0) RETURN

        DELETE (UNIT=13,IOSTAT=IER)

        IF (IER.NE.0) THEN
           WRITE (6,'('' ERROR: Unable to delete mark.'')')
           CALL ERRSNS(IDUMMY,IER1)
           IF (IER1.EQ.0) THEN
              WRITE (6,'('' IOSTAT error = '',I)') IER
           ELSE
              CALL SYS_GETMSG(IER1)
           END IF
        END IF

        RETURN
        END





        SUBROUTINE OPEN_OLD_TAG

        IMPLICIT INTEGER (A-Z)

        INCLUDE '($FORIOSDEF)'

        INCLUDE 'BULLUSER.INC'

        COMMON /TAGS/ BULL_TAG,READ_TAG

        COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
        CHARACTER*64 BULL_PARAMETER

        IER = SYS_TRNLNM('BULL_MARK',BULL_PARAMETER)
        IF (.NOT.IER) RETURN

        NTRIES = 0

        DO WHILE (FILE_LOCK(IER,IER1).AND.NTRIES.LE.30)
           OPEN (UNIT=13,FILE='BULL_MARK:'//
     &       USERNAME(:TRIM(USERNAME))//'.BULLMARK',STATUS='OLD',
     &       ACCESS='KEYED',RECORDTYPE='FIXED',SHARED,
     &       ORGANIZATION='INDEXED',IOSTAT=IER,
     &       KEY=(1:12:CHARACTER))
           NTRIES = NTRIES + 1
        END DO

        IF (IER.NE.0.AND.IER.NE.FOR$IOS_FILNOTFOU) THEN
           WRITE (6,'('' Unable to open mark file.'')')
           IF (IER1.EQ.0) CALL ERRSNS(IDUMMY,IER1)
           IF (IER1.EQ.0) THEN
              WRITE (6,'('' IOSTAT error = '',I)') IER
           ELSE
              CALL SYS_GETMSG(IER1)
           END IF
           RETURN
        END IF

        IF (IER.EQ.0) BULL_TAG = .TRUE.

        RETURN
        END




        SUBROUTINE OPEN_NEW_TAG(IER)

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLUSER.INC'

        COMMON /TAGS/ BULL_TAG,READ_TAG

        COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
        CHARACTER*64 BULL_PARAMETER

        CHARACTER*64 BULL_MARK

        IER = SYS_TRNLNM('BULL_MARK',BULL_MARK)
        IF (.NOT.IER) THEN
           WRITE (6,'('' ERROR: BULL_MARK must be defined.'',
     &          ''  See HELP MARK.'')')
           RETURN
        ELSE
           IER1 = SYS_TRNLNM_SYSTEM('BULL_MARK',BULL_PARAMETER)
           IF (.NOT.IER1.OR.BULL_MARK.NE.BULL_PARAMETER) THEN
              IER = SYS_TRNLNM('BULL_MARK',BULL_PARAMETER)
              CALL DISABLE_PRIVS
              IER1 = 0
           END IF
           OPEN (UNIT=13,FILE='BULL_MARK:'//
     &          USERNAME(:TRIM(USERNAME))//'.BULLMARK',STATUS='NEW',
     &          ACCESS='KEYED',RECORDTYPE='FIXED',SHARED,
     &          RECORDSIZE=3,
     &          FORM='UNFORMATTED',ORGANIZATION='INDEXED',IOSTAT=IER,
     &          KEY=(1:12:CHARACTER))
           IF (.NOT.IER1) CALL ENABLE_PRIVS
           IF (IER.NE.0) THEN
              WRITE (6,'('' Cannot create mark file.'')')
              CALL ERRSNS(IDUMMY,IER1)
              IF (IER1.EQ.0) THEN
                 WRITE (6,'('' IOSTAT error = '',I)') IER
                 IER = 0
              ELSE
                 CALL SYS_GETMSG(IER1)
                 IER = IER1
              END IF
           ELSE
              BULL_TAG = .TRUE.
              IER = 1
           END IF
        END IF

        RETURN
        END



        CHARACTER*12 FUNCTION TAG_KEY(FOLDER_NUMBER,MSG_KEY)

        IMPLICIT INTEGER (A-Z)

        CHARACTER*(*) MSG_KEY

        CALL LIB$MOVC3(4,FOLDER_NUMBER,%REF(TAG_KEY))

        CALL GET_MSGKEY(%REF(MSG_KEY),TAG_KEY(5:))

        RETURN
        END




        SUBROUTINE GET_FIRST_TAG(FOLDER_NUMBER,IER,MESSAGE)

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLDIR.INC'

        COMMON /TAGS/ BULL_TAG,READ_TAG

        CHARACTER*12 TAG_KEY,INPUT_KEY

        IF (.NOT.BULL_TAG) THEN
           CALL OPEN_NEW_TAG(IER)
           IF (.NOT.IER) RETURN
        END IF

        MSG_KEY = BULLDIR_HEADER

        HEADER = .TRUE.
        GO TO 10

        ENTRY GET_THIS_OR_NEXT_TAG(FOLDER_NUMBER,IER,MESSAGE)

        I = 1
        DO WHILE (I.LT.9)
           ITEST = ICHAR(MSG_KEY(I:I))
           IF (ITEST.GT.0) THEN
              MSG_KEY(I:I) = CHAR(ITEST-1)
              I = 9
           ELSE
              I = I + 1
           END IF
        END DO

        ENTRY GET_NEXT_TAG(FOLDER_NUMBER,IER,MESSAGE)

        HEADER = .FALSE.

10      DO WHILE (1)
           DO WHILE (REC_LOCK(IER))
              READ (13,KEYGT=TAG_KEY(FOLDER_NUMBER,MSG_KEY),IOSTAT=IER)
     &                                  INPUT_KEY
           END DO

           IF (IER.EQ.0) THEN
              CALL GET_MSGKEY(%REF(INPUT_KEY(5:)),MSG_KEY)
              CALL LIB$MOVC3(4,%REF(INPUT_KEY),FOLDER1_NUMBER)
           END IF

           IF (FOLDER1_NUMBER.NE.FOLDER_NUMBER.OR.IER.NE.0) THEN
              IER = 1
              UNLOCK 13
              RETURN
           ELSE
              I = 1
              DO WHILE (I.LT.9)
                 ITEST = ICHAR(MSG_KEY(I:I))
                 IF (ITEST.GT.0) THEN
                    MSG_KEY(I:I) = CHAR(ITEST-1)
                    I = 9
                 ELSE
                    I = I + 1
                 END IF
              END DO
              CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
              CALL OPEN_BULLDIR
              CALL READDIR_KEYGE(IER)
              CALL CLOSE_BULLDIR
              CALL GET_MSGKEY(%REF(INPUT_KEY(5:)),INPUT_KEY(5:))
              IF (IER.NE.0.AND.MSG_KEY.EQ.INPUT_KEY(5:)) THEN
                 UNLOCK 13
                 MESSAGE = MSG_NUM
                 IF (HEADER) THEN
                    MESSAGE = MESSAGE - 1
                    MSG_KEY = BULLDIR_HEADER
                 END IF
                 IER = 0
                 RETURN
              ELSE
                 DELETE (UNIT=13)
                 IER = 1
              END IF
           END IF

        END DO

        END






        SUBROUTINE FULL_DIR(INDEX_COUNT)
C
C       Add INDEX command to BULLETIN, display directories of ALL
C       folders. Added per request of a faculty member for his private
C       board. Changes to BULLETIN.FOR should be fairly obvious.
C
C       Brian Nelson, Brian@uoft02.bitnet (or .ccnet, node 8.2)
C
        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLDIR.INC'
        INCLUDE 'BULLFILES.INC'
        INCLUDE 'BULLFOLDER.INC'
        INCLUDE 'BULLUSER.INC'

        COMMON /POINT/ BULL_POINT

        COMMON /TAGS/ BULL_TAG,READ_TAG

        DATA FOLDER_Q1/0/

        BULL_POINT = 0

        IF (NUM_FOLDERS.GT.0.AND..NOT.CLI$PRESENT('RESTART')
     &          .AND.INDEX_COUNT.EQ.1) THEN
           INDEX_COUNT = 2
           DIR_COUNT = 0
        END IF

        IF (INDEX_COUNT.EQ.1) THEN
          CALL INIT_QUEUE(FOLDER_Q1,FOLDER1_COM)

          FOLDER_Q = FOLDER_Q1
          CALL OPEN_BULLFOLDER_SHARED            ! Get folder file

          NUM_FOLDERS = 0
          IER = 0
          DO WHILE (IER.EQ.0)                   ! Copy all bulletins from file
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
                 NUM_FOLDERS = NUM_FOLDERS + 1
                 CALL WRITE_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER1_COM)
              END IF
            END IF
          END DO

          CALL CLOSE_BULLFOLDER                  ! We don't need file anymore

          FOLDER_Q = FOLDER_Q1                  ! Init queue pointer to header
          WRITE (6,1000)
          WRITE (6,1020)
          DO J = 1,NUM_FOLDERS
           CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER1_COM)
           WRITE (6,1030) FOLDER1(:15),F1_NBULL,
     &          FOLDER1_DESCRIP(:MIN(TRIM(FOLDER1_DESCRIP),59))
          END DO
          WRITE (6,1060)
          FOLDER_Q = FOLDER_Q1                  ! Init queue pointer to header
          INDEX_COUNT = 2
          DIR_COUNT = 0
          READ_TAG = .FALSE.
          IF (CLI$PRESENT('MARKED')) READ_TAG = .TRUE.
          RETURN
        ELSE IF (INDEX_COUNT.EQ.2) THEN
         IF (DIR_COUNT.EQ.0) THEN
          F1_NBULL = 0
          DO WHILE (NUM_FOLDERS.GT.0.AND.F1_NBULL.EQ.0)
             NUM_FOLDERS = NUM_FOLDERS - 1
             CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER1_COM)
             IF (F1_NBULL.GT.0) THEN
              FOLDER_NUMBER = -1
              CALL SELECT_FOLDER(.FALSE.,IER)
              IF (.NOT.IER) F1_NBULL = 0
             END IF
          END DO

          IF (F1_NBULL.EQ.0) THEN
             WRITE (6,1050)
             INDEX_COUNT = 0
             RETURN
          END IF
         END IF
     
         IF (READ_TAG) THEN
            CALL GET_FIRST_TAG(FOLDER_NUMBER,IER,BULL_POINT)
         END IF

         CALL DIRECTORY(DIR_COUNT)
         IF (DIR_COUNT.GT.0) RETURN

         IF (NUM_FOLDERS.GT.0) THEN
            WRITE (6,1040)
         ELSE
            INDEX_COUNT = 0
         END IF
        END IF

        RETURN

1000    FORMAT (' The following folders are present'/)
1020    FORMAT (' Name         Count Description'/)
1030    FORMAT (1X,A15,I5,1X,A)
1040    FORMAT (' Type Return to continue to the next folder...')
1050    FORMAT (' End of folder search.')
1060    FORMAT (' Type Return to continue...')

        END




        SUBROUTINE SHOW_USER
C
C  SUBROUTINE SHOW_USER
C
C  FUNCTION: Shows information for specified users.
C
        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLUSER.INC'

        DIMENSION NOLOGIN_BTIM(2)

        CHARACTER*17 DATETIME

        ALL = CLI$PRESENT('NOLOGIN').OR.CLI$PRESENT('ALL')
     &                          .OR.CLI$PRESENT('LOGIN')
        IF (.NOT.ALL) THEN
           IER = CLI$GET_VALUE('USERNAME',TEMP_USER)
           IF (.NOT.IER) TEMP_USER = USERNAME
        END IF

        IF (.NOT.SETPRV_PRIV().AND.(ALL.OR.USERNAME.NE.TEMP_USER)) THEN
           WRITE (6,'('' ERROR: No privs to user command.'')')
           RETURN
        END IF

        CALL SYS_BINTIM('5-NOV-2956 00:00:00.00',NOLOGIN_BTIM)

        CALL OPEN_BULLUSER_SHARED

        IF (.NOT.ALL) THEN
           CALL READ_USER_FILE_KEYNAME(TEMP_USER,IER)
           IF (IER.EQ.0) THEN
              IF (COMPARE_BTIM(LOGIN_BTIM,NOLOGIN_BTIM).GE.0) THEN
                 WRITE (6,'('' NOLOGIN set for specified user.'')')
              ELSE
                 CALL SYS$ASCTIM(,DATETIME,LOGIN_BTIM,)
                 WRITE (6,'('' User last logged in at '',A,''.'')')
     &                                          DATETIME
              END IF
           ELSE
              WRITE (6,'('' Entry for specified user not found.'')')
           END IF
        ELSE
           CALL READ_USER_FILE(IER)
           DO WHILE (IER.EQ.0)
              CALL READ_USER_FILE(IER)
              IF (IER.EQ.0.AND.TEMP_USER(:1).NE.':'.AND.
     &                          TEMP_USER(:1).NE.'*') THEN
                 IER1 = COMPARE_BTIM(LOGIN_BTIM,NOLOGIN_BTIM)
                 IF (.NOT.CLI$PRESENT('LOGIN').AND.IER1.GE.0) THEN
                    WRITE (6,'('' NOLOGIN set for '',A,''.'')')
     &                                  TEMP_USER(:TRIM(TEMP_USER))
                 ELSE IF (.NOT.CLI$PRESENT('NOLOGIN').AND.IER1.LT.0) THEN
                    CALL SYS$ASCTIM(,DATETIME,LOGIN_BTIM,)
                    WRITE (6,'(1X,A,'' last logged in at '',A,''.'')')
     &                          TEMP_USER(:TRIM(TEMP_USER)),DATETIME
                 END IF
              END IF
           END DO
        END IF

        CALL CLOSE_BULLUSER

        RETURN
        END



        SUBROUTINE INIT_MESSAGE_ADD(IN_FOLDER,IN_FROM,IN_DESCRIP,IER)
C
C  SUBROUTINE INIT_MESSAGE_ADD
C
C  FUNCTION:  Opens specified folder in order to add message.
C
C  INPUTS:
C       IN_FOLDER  - Character string containing folder name
C       IN_FROM    - Character string containing name of owner of message.
C                    If empty, the default is the owner of the process.
C       IN_DESCRIP - Character string containing subject of message.
C                    If empty, the message is searched for a line
C                    which starts with "Subj:" or "Subject:".
C  OUTPUTS:
C       IER - Error status.  True if properly connected to folder.
C               False if folder not found.
C
        IMPLICIT INTEGER (A - Z)

        INCLUDE 'BULLFILES.INC'

        INCLUDE 'BULLFOLDER.INC'

        INCLUDE 'BULLDIR.INC'

        COMMON /BCP/ BULLCP
        LOGICAL BULLCP

        COMMON /MAIL_PROTOCOL/ PROTOCOL,LPRO
        CHARACTER*12 PROTOCOL
        DATA LPRO/0/

        COMMON /DIGEST/ LDESCR,FIRST_BREAK

        CHARACTER*(*) IN_FOLDER,IN_FROM,IN_DESCRIP

        COMMON /MAIN_HEADER_INFO/ INFROM,INDESCRIP,LEN_FROM,LEN_DESCRP
        CHARACTER*(LINE_LENGTH) INFROM,INDESCRIP

        COMMON /TEXT_PRESENT/ TEXT

        BULLCP = 1                      ! Inhibit folder cleanup subprocess

        CALL OPEN_BULLFOLDER                    ! Get folder file

        CALL READ_FOLDER_FILE_KEYNAME(IN_FOLDER(:TRIM(IN_FOLDER)),IER)

        CALL CLOSE_BULLFOLDER

        IF (IER.NE.0) THEN
           CALL ERRSNS(IDUMMY,IER)
           RETURN
        ELSE
           IER = 1
        END IF

        ENTRY INIT_MESSAGE_ADD_BBOARD(IN_FROM,IN_DESCRIP,IER)

        TEXT = .FALSE.                  ! No text written, as of yet

        FIRST_BREAK = .TRUE.

        IF (FOLDER_NUMBER.EQ.0) THEN    ! If GENERAL folder
           FOLDER_SET = .FALSE.         ! indicate it
        ELSE                            ! Else it's another folder
           FOLDER_SET = .TRUE.          ! indicate it
        END IF

        FOLDER_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//
     &          FOLDER                  ! set folder file names

        ENTRY INIT_MESSAGE_ADD_DIGEST(IN_FROM,IN_DESCRIP,IER)

        CALL OPEN_BULLDIR               ! Open directory file

        CALL OPEN_BULLFIL               ! Open data file

        CALL READDIR(0,IER1)            ! Get NBLOCK
        IF (IER1.EQ.0) NBLOCK = 0       ! If new file, NBLOCK is 0

        NBLOCK = NBLOCK + 1
        LENGTH = NBLOCK                 ! Initialize line count

        LEN_FROM = TRIM(IN_FROM)
        IF (LEN_FROM.EQ.0) THEN
           CALL GETUSER(FROM)
           INFROM = FROM
           LEN_FROM = TRIM(INFROM)
        ELSE
           INFROM = IN_FROM
           IF (.NOT.BTEST(FOLDER_FLAG,5)) THEN
              CALL STORE_FROM(INFROM,LEN_FROM)
           ELSE IF (INDEX(INFROM,'%"').GT.0) THEN       ! Store any protocol
              LPRO = INDEX(INFROM,'%"') + 1
              PROTOCOL = INFROM(:LPRO)
           END IF
        END IF

        LEN_DESCRP = TRIM(IN_DESCRIP)
        IF (LEN_DESCRP.GT.0) THEN
           INDESCRIP = IN_DESCRIP
           IF (.NOT.BTEST(FOLDER_FLAG,5)) THEN
              CALL STORE_DESCRP(INDESCRIP,LEN_DESCRP)
           END IF
        ELSE
           DESCRIP = ' '
        END IF

        CALL STRIP_HEADER(INPUT,0,IER1)

        RETURN
        END




        SUBROUTINE WRITE_MESSAGE_LINE(BUFFER)
C
C  SUBROUTINE WRITE_MESSAGE_LINE
C
C  FUNCTION:  Writes one line of message into folder.
C
C  INPUTS:
C       BUFFER - Character string containing line to be put into message.
C

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLDIR.INC'

        INCLUDE 'BULLFOLDER.INC'

        COMMON /MAIL_PROTOCOL/ PROTOCOL,LPRO
        CHARACTER*12 PROTOCOL

        COMMON /MAIN_HEADER_INFO/ INFROM,INDESCRIP,LEN_FROM,LEN_DESCRP
        CHARACTER*(LINE_LENGTH) INFROM,INDESCRIP

        COMMON /DIGEST/ LDESCR,FIRST_BREAK
        DATA FIRST_BREAK/.TRUE./

        COMMON /STRIP_HEADER/ STRIP
        DATA STRIP/.TRUE./

        COMMON /TEXT_PRESENT/ TEXT

        CHARACTER*(*) BUFFER

        LEN_BUFFER = TRIM(BUFFER)

        IF (BTEST(FOLDER_FLAG,5)) THEN
           IF (INDEX(BUFFER,'-------------').EQ.1) THEN
              BREAK = .TRUE.
              DO I=1,LEN_BUFFER
                 IF (BUFFER(I:I).NE.'-') BREAK = .FALSE.
              END DO
           ELSE
              BREAK = .FALSE.
           END IF
           IF (BREAK) THEN
              IF (.NOT.FIRST_BREAK) THEN
                 CALL FINISH_MESSAGE_ADD
                 CALL INIT_MESSAGE_ADD_DIGEST(INFROM,INDESCRIP,IER)
              ELSE
                 FIRST_BREAK = .FALSE.
              END IF
              LFROM = 0
              LDESCR = 0
              RETURN
           ELSE IF (.NOT.FIRST_BREAK) THEN
              IF (LDESCR.EQ.0) THEN
                 IF (BUFFER(:9).EQ.'Subject: ') THEN
                    LDESCR = LEN_BUFFER - 9
                    CALL STORE_DESCRP(BUFFER(10:),LDESCR)
                    IF (LFROM.EQ.0) THEN
                       LFROM = LEN_FROM
                       CALL STORE_FROM(INFROM,LFROM)
                    END IF
                 ELSE IF (BUFFER(:6).EQ.'From: ') THEN
                    LFROM = LEN_BUFFER - 6
                    IF (LPRO.GT.0) THEN
                       LFROM = LFROM + LPRO + 1
                       CALL STORE_FROM(PROTOCOL(:LPRO)//
     &                  BUFFER(7:LEN_BUFFER)//'"',LFROM)
                    ELSE
                       CALL STORE_FROM(BUFFER(7:),LFROM)
                    END IF
                 END IF
                 RETURN
              END IF
           ELSE
              RETURN
           END IF
        END IF

        IF (LEN_BUFFER.EQ.0) THEN               ! If empty line
           IF (.NOT.STRIP) THEN
              CALL STORE_BULL(1,' ',NBLOCK)     ! just store one space
           ELSE
              STRIP = .FALSE.
           END IF
        ELSE
           IF (LEN_DESCRP.EQ.0) THEN
              IF (BUFFER(:9).EQ.'Subject: ') THEN
                 DESCRIP = BUFFER(INDEX(BUFFER,' ')+1:)
                 LEN_DESCRP = LEN_BUFFER
              END IF
           END IF
           IF (STRIP) THEN
              CALL STRIP_HEADER(BUFFER,LEN_BUFFER,IER)
              IF (IER) THEN
                 RETURN
              ELSE
                 STRIP = .FALSE.
              END IF
           END IF
           CALL STORE_BULL(MIN(LEN_BUFFER,LINE_LENGTH),BUFFER,NBLOCK)
           TEXT = .TRUE.
        END IF

        RETURN
        END




        SUBROUTINE FINISH_MESSAGE_ADD
C
C  SUBROUTINE FINISH_MESSAGE_ADD
C
C  FUNCTION:  Writes message entry into directory file and closes folder
C
C  NOTE:  Only should be run if INIT_MESSAGE_ADD was successful.
C

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLDIR.INC'

        INCLUDE 'BULLFOLDER.INC'

        COMMON /DIGEST/ LDESCR,FIRST_BREAK

        COMMON /STRIP_HEADER/ STRIP

        COMMON /TEXT_PRESENT/ TEXT

        STRIP = .TRUE.                          ! Reset strip flag

        CALL FLUSH_BULL(NBLOCK)

        CALL CLOSE_BULLFIL                      ! Finished adding bulletin

        IF ((BTEST(FOLDER_FLAG,5).AND.LDESCR.EQ.0).OR.  ! End of digest msg
     &                          .NOT.TEXT) THEN ! or no message text found
           CALL CLOSE_BULLDIR                   ! then don't add message entry
           RETURN
        END IF

        IF (FOLDER_BBEXPIRE.EQ.-1) THEN         ! Folder has expiration time?
           EXDATE = '5-NOV-2000'                ! no, so set date far in future
           SYSTEM = 2                           ! indicate permanent message
        ELSE                                    ! Else set expiration date
           CALL GET_EXDATE(EXDATE,FOLDER_BBEXPIRE)
           SYSTEM = 0
        END IF
        EXTIME = '00:00:00.00'

        LENGTH = NBLOCK - LENGTH + 1            ! Number of records

        CALL ADD_ENTRY                          ! Add the new directory entry

        CALL CLOSE_BULLDIR                      ! Totally finished with add

        CALL UPDATE_FOLDER

        RETURN
        END



        SUBROUTINE STORE_FROM(IFROM,LEN_INFROM)

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLDIR.INC'

        CHARACTER*(*) IFROM

        CHARACTER*(LINE_LENGTH) INFROM

        INFROM = IFROM

        CALL STORE_BULL(6+LEN_INFROM,'From: '//INFROM(:LEN_INFROM),
     &          NBLOCK)

        IF (INDEX(INFROM,'%"').GT.0)            ! Strip off protocol program
     &          INFROM = INFROM(INDEX(INFROM,'%"')+2:)

        IF (INDEX(INFROM,'::').GT.0)            ! Strip off node name
     &          INFROM = INFROM(INDEX(INFROM,'::')+2:)  ! I.e. HOST::USER

        DO WHILE (INDEX(INFROM,'!').GT.0.AND.   ! Unix address go backwards.
     &          INDEX(INFROM,'!').LT.INDEX(INFROM,'@'))
           INFROM = INFROM(INDEX(INFROM,'!')+1:)        ! I.e. host!user
        END DO

        IF (INDEX(INFROM,'<').GT.0) THEN        ! Name may be of form
           INFROM = INFROM(INDEX(INFROM,'<'):)  ! personal-name <net-name>
        END IF

        IF (INDEX(INFROM,'(').GT.0.AND.         ! personal-name (net-name)
     &          INDEX(INFROM,'@').GT.INDEX(INFROM,'(')) THEN
           INFROM = INFROM(INDEX(INFROM,'(')+1:)
        END IF

        I = 1   ! Trim username to start at first alpha character
        DO WHILE (I.LE.LEN_INFROM.AND.(INFROM(I:I).EQ.' '.OR.
     &          INFROM(I:I).EQ.'%'.OR.INFROM(I:I).EQ.'.'.OR.
     &          INFROM(I:I).EQ.'@'.OR.INFROM(I:I).EQ.'<'.OR.
     &          INFROM(I:I).EQ.'"'))
           I = I + 1
        END DO
        INFROM = INFROM(I:)

        I = 1           ! Trim username to end at a alpha character
        DO WHILE (I.LE.12.AND.INFROM(I:I).NE.' '.AND.
     &          INFROM(I:I).NE.'%'.AND.INFROM(I:I).NE.'.'.AND.
     &          INFROM(I:I).NE.'@'.AND.INFROM(I:I).NE.'<'.AND.
     &          INFROM(I:I).NE.'"')
           I = I + 1
        END DO
        FROM = INFROM(:I-1)

        DO J=2,I-1
           IF ((FROM(J:J).GE.'A'.AND.FROM(J:J).LE.'Z').AND.
     &         ((FROM(J-1:J-1).GE.'A'.AND.FROM(J-1:J-1).LE.'Z').OR.
     &          (FROM(J-1:J-1).GE.'a'.AND.FROM(J-1:J-1).LE.'z'))) THEN
              FROM(J:J) = CHAR(ICHAR(FROM(J:J))-ICHAR('A')+ICHAR('a'))
           END IF
        END DO

        RETURN
        END




        SUBROUTINE STORE_DESCRP(INDESCRIP,LEN_DESCRP)

        IMPLICIT INTEGER (A-Z)

        INCLUDE 'BULLDIR.INC'

        CHARACTER*(*) INDESCRIP

        DO WHILE (LEN_DESCRP.GT.0.AND.INDESCRIP(:1).EQ.' ')
           INDESCRIP = INDESCRIP(2:)
           LEN_DESCRP = LEN_DESCRP - 1
        END DO

        DO I=1,LEN_DESCRP                       ! Remove control characters
           IF (INDESCRIP(I:I).LT.' ') INDESCRIP(I:I) = ' '
        END DO
        IF (LEN_DESCRP.GT.LEN(DESCRIP)) THEN
                                ! Is length > allowable subject length?
           CALL STORE_BULL(6+LEN_DESCRP,'Subj: '//
     &          INDESCRIP(:LEN_DESCRP),NBLOCK)
        END IF

        DESCRIP = INDESCRIP(:MIN(LEN_DESCRP,LEN(DESCRIP)))

        RETURN
        END




        SUBROUTINE STRIP_HEADER(BUFFER,BLEN,IER)
C
C  SUBROUTINE STRIP_HEADER
C
C  FUNCTION:  Indicates whether line is part of mail message header.
C
C  INPUTS:
C       BUFFER  - Character string containing input line of message.
C       BLEN    - Length of character string.  If = 0, initialize subroutine.
C
C  OUTPUTS:
C       IER     - If true, line should be stripped.  Else, end of header.
C
        IMPLICIT INTEGER (A - Z)

        CHARACTER*(*) BUFFER

        INCLUDE 'BULLFOLDER.INC'

        IF (.NOT.BTEST(FOLDER_FLAG,4).OR.TRIM(BUFFER).EQ.0) THEN
                        ! If STRIP not set for folder or empty line
           IER = .FALSE.
           CONT_LINE = .FALSE.
           RETURN
        END IF

        IF (BLEN.EQ.0) CONT_LINE = .FALSE.

        IER = .TRUE.

        IF (CONT_LINE.AND.(BUFFER(:1).EQ.' '.OR.   ! If line is continuation
     &          BUFFER(:1).EQ.CHAR(9))) RETURN     ! of previous header line

        I = 1
        DO WHILE (I.LE.BLEN.AND.BUFFER(I:I).NE.' ')
           IF (BUFFER(I:I).EQ.':') THEN ! Header line found
              CONT_LINE = .TRUE.        ! Next line might be continuation
              RETURN
           ELSE
              I = I + 1
           END IF
        END DO

        IER = .FALSE.
        CONT_LINE = .FALSE.

        RETURN
        END
