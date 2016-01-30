C
C  BULLETIN9.FOR, Version 4/8/98
C  Purpose: Contains subroutines for the bulletin board utility program.
C  Environment: VAX/VMS
C  Usage: Invoked by the BULLETIN command.
C  Programmer: Mark R. London
C
C  Copyright (c) 1990
C  Property of Massachusetts Institute of Technology, Cambridge MA 02139.
C  This program cannot be copied or distributed in any form for non-MIT
C  use without specific written approval of MIT Plasma Fusion Center
C  Management.
C
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
     &				NODE_ERROR,POINT_NODE
	CHARACTER*32 NODES(10)
	LOGICAL LOCAL_NODE_FOUND,NODE_ERROR

    	CHARACTER INLINE*80

	CALL GET_NODE_INFO

 	IF (NODE_ERROR) GO TO 940

	IF (NODE_NUM.EQ.0.OR.LOCAL_NODE_FOUND) THEN
	   WRITE (6,'('' ERROR: Cannot specify local node.'')')
	   GO TO 999
	END IF

	IER = CLI$GET_VALUE('SUBJECT',DESCRIP)

	DO POINT_NODE=1,NODE_NUM	   	! Write out command to nodes
	   NLEN = TRIM(NODES(POINT_NODE))	! Length of node name
	   INLINE = 'DELETE/SUBJECT="'//DESCRIP(:TRIM(DESCRIP))
	   WRITE (POINT_NODE+9,'(A)',ERR=940) INLINE
	   READ (POINT_NODE+9,'(A)',ERR=940,END=940) INLINE
	   IF (INLINE.EQ.'END') THEN
	      WRITE (6,'('' Message successfully deleted from node '',A)')
     &				NODES(POINT_NODE)
	   ELSE
	      WRITE (6,'('' Error while deleting message to node '',A)')
     &				NODES(POINT_NODE)
	      WRITE (6,'(A)') INLINE
	   END IF
	END DO

	GO TO 999

940	WRITE (6,1015) NODES(POINT_NODE)

999	DO WHILE (NODE_NUM.GT.0)
	   CLOSE(UNIT=9+NODE_NUM)
	   NODE_NUM = NODE_NUM - 1
	END DO

	RETURN

1015	FORMAT (' ERROR: Unable to reach node ',A)

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

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	CHARACTER*(*) FLAGNAME

	IF (REMOTE_SET.EQ.3.OR.(REMOTE_SET.EQ.4.AND.FLAG.NE.1)) THEN
	   WRITE (6,'('' ERROR: Command invalid for folder.'')')
	ELSE IF ((FLAG.EQ.7.OR.FLAG.EQ.14).AND..NOT.SETPRV_PRIV()) THEN
	   WRITE (6,'('' ERROR: Privileges required for this command.'')')
	ELSE IF (FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)) THEN
           IF (REMOTE_SET.NE.4) THEN
	      CALL OPEN_BULLFOLDER		! Open folder file
	   ELSE
	      CALL OPEN_BULLNEWS_SHARED
	   END IF

	   CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)

	   IF (SETTING) THEN
	      FOLDER_FLAG = IBSET(FOLDER_FLAG,FLAG)
	   ELSE
	      FOLDER_FLAG = IBCLR(FOLDER_FLAG,FLAG)
	   END IF

	   CALL REWRITE_FOLDER_FILE(IER)

	   CALL CLOSE_BULLFOLDER

	   WRITE (6,'(1X,A,'' has been modified for folder.'')')
     &		FLAGNAME
	ELSE
	   WRITE (6,'(1X,'' You are not authorized to modify '',A)')
     &		FLAGNAME//'.'
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

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	IF (REMOTE_SET.EQ.3) THEN
	   WRITE (6,'('' ERROR: Command invalid for folder. '')')
	ELSE IF (LIMIT.LT.0) THEN
	   WRITE (6,'('' ERROR: Invalid expiration length specified.'')')
	ELSE IF (FOLDER_ACCESS(USERNAME,FOLDER_FLAG,FOLDER_OWNER)) THEN
           IF (REMOTE_SET.NE.4) THEN
	      CALL OPEN_BULLFOLDER		! Open folder file
	   ELSE
	      CALL OPEN_BULLNEWS_SHARED
	   END IF

	   CALL READ_FOLDER_FILE_KEYNAME(FOLDER,IER)

	   F_EXPIRE_LIMIT = LIMIT

	   CALL REWRITE_FOLDER_FILE(IER)

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
	   OPEN (UNIT=24,FILE=FOLDER_FILE(:TRIM(FOLDER_FILE))
     &		//'.TMPDIR',STATUS='NEW',FORM='UNFORMATTED',
     &		RECORDTYPE='FIXED',RECORDSIZE=DIR_RECORD_LENGTH/4,
     &		ORGANIZATION='INDEXED',IOSTAT=IER,DISPOSE='DELETE',
     &		KEY=(9:12:INTEGER,1:8:CHARACTER),ACCESS='KEYED')
	END DO

	IF (IER1.NE.0) RETURN

	NBULL = 0

	WRITE(24,IOSTAT=IER1) BULLDIR_HEADER
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
	   WRITE(24,IOSTAT=IER1) BULLDIR_ENTRY

	   NEWEST_DATE = DATE
	   NEWEST_TIME = TIME

	   TO_POINTER = TO_POINTER + 1

	   BULLDIR_ENTRY = BULLDIR_ENTRY_SAVE
	END DO

	CLOSE (UNIT=24)

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
	WRITE(24,IOSTAT=IER1) BULLDIR_ENTRY

	RETURN

	ENTRY ADD_MERGE_REST(IER1)

	CALL UPDATE_LOGIN(.TRUE.)

	DO WHILE (IER1.EQ.0)

	   CALL READDIR(TO_POINTER,IER)
	   IF (TO_POINTER+1.NE.IER) THEN
	      READ (24,KEYID=0,KEY=0,IOSTAT=IER1)
	      CALL CONVERT_HEADER_TOBIN
	      REWRITE(24,IOSTAT=IER1) BULLDIR_HEADER
	      IF (IER1.EQ.0) THEN
	         CLOSE (UNIT=24,DISPOSE='KEEP')
	         CALL LIB$RENAME_FILE(FOLDER_FILE(:TRIM(FOLDER_FILE))//
     &		  '.TMPDIR',FOLDER_FILE(:TRIM(FOLDER_FILE))//'.BULLDIR')
	      ELSE
		 CLOSE (UNIT=24)
	      END IF
	      RETURN
	   END IF

	   NBULL = NBULL + 1
	   MSG_NUM = NBULL

	   CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
	   WRITE(24,IOSTAT=IER1) BULLDIR_ENTRY

	   NEWEST_DATE = DATE
	   NEWEST_TIME = TIME

	   TO_POINTER = TO_POINTER + 1
	END DO

	CLOSE (UNIT=24)

	RETURN
	END




	SUBROUTINE SET_NOKEYPAD

	IMPLICIT INTEGER (A-Z)

	COMMON /SMG/ KEYBOARD_ID,KEY_TABLE_ID

	COMMON /KEYPAD/ KEYPAD_MODE

	INCLUDE '($SMGDEF)'

	KEYPAD_MODE = 0

	TERM = SMG$M_KEY_TERMINATE

	IER = SMG$SET_KEYPAD_MODE(KEYBOARD_ID,0)

	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PF2',,TERM,'SET KEYPAD',)

	RETURN
	END





	SUBROUTINE SET_KEYPAD

	IMPLICIT INTEGER (A-Z)

	COMMON /SMG/ KEYBOARD_ID,KEY_TABLE_ID

	COMMON /KEYPAD/ KEYPAD_MODE

        COMMON /KEYLOAD/ LOAD_KEY

	INCLUDE '($SMGDEF)'

	KEYPAD_MODE = 1

	TERM = SMG$M_KEY_TERMINATE

	IER = SMG$SET_KEYPAD_MODE(KEYBOARD_ID,1)

	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PF1',,,,'GOLD')
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PF2',,TERM,'HELP',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PF2','GOLD',TERM,'SET NOKEYPAD',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PF3',,,'EXTRACT ',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PF3','GOLD',,'FILE ',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PF4',,TERM,'SHOW KEYPAD',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PF4','GOLD',TERM,
     &		'SHOW KEYPAD/PRINT',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP0',,TERM,
     &		'SHOW FOLDER/FULL',)
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
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP5','GOLD',TERM,'RESP/EDIT/EXT',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP6',,TERM,'LAST',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP7',,TERM,'ADD',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP7','GOLD',TERM,'ADD/EDIT',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP8',,TERM,'REPLY',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP8','GOLD',TERM,'REPL/EDIT/EXT',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP9',,TERM,'MAIL',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'KP9','GOLD',TERM,'MAIL/NOHEAD',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'MINUS',,TERM,'READ/NEW',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'MINUS','GOLD',TERM,'SHOW NEW',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'COMMA',,TERM,'DIR/NEW',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'COMMA','GOLD',TERM,'INDEX',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PERIOD',,TERM,'DELETE',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'PERIOD','GOLD',TERM,'UNDELETE',)
	IER = SMG$ADD_KEY_DEF(KEY_TABLE_ID,'ENTER','GOLD',,'SELECT ',)

        LOAD_KEY = SMG$LOAD_KEY_DEFS(KEY_TABLE_ID,'BULL_INIT',
     &				'SYS$LOGIN:BULL.INI',1)

	RETURN
	END



	SUBROUTINE SHOW_KEYPAD(LIBRARY)

	IMPLICIT INTEGER (A-Z)
	EXTERNAL LIB$PUT_OUTPUT,PRINT_OUTPUT
	CHARACTER*(*) LIBRARY

	COMMON /SMG/ KEYBOARD_ID,KEY_TABLE_ID

        COMMON /KEYLOAD/ LOAD_KEY

	COMMON /PAGE/ PAGE_LENGTH,PAGE_WIDTH,PAGING

	INCLUDE '($HLPDEF)'

	CHARACTER KEY*10,EQU*50,ST*20,IFS*20

	OUT = 6

	IF (CLI$PRESENT('PRINT')) THEN
	   OPEN (UNIT=8,STATUS='NEW',FILE='SYS$LOGIN:KEYPAD.DAT',
     &			IOSTAT=IER)
	   IF (IER.NE.0) THEN
	      WRITE (6,'('' ERROR WHILE OPENING FILE TO PRINTER.'')')
	      RETURN
	   END IF
	   OUT = 8
	END IF      

	IF (CLI$GET_VALUE('SHOW_KEY',KEY,I)) THEN
	   DO WHILE (CLI$GET_VALUE('STATE',IFS,J))
              IER = SMG$GET_KEY_DEF(
     &			KEY_TABLE_ID,KEY(:I),IFS(:J),ATT,EQU,ST)
              WRITE (OUT,'(3X,A,$)') KEY(:TRIM(KEY))//' = '//'"'//
     &                                  EQU(:TRIM(EQU))//'"'
	      IF (TRIM(ST).GT.0) THEN
	         WRITE (OUT,'(A,$)') '+ '//'state='//ST(:TRIM(ST))
	      END IF
              IF (TRIM(IFS).GT.0.AND.IFS.NE.'DEFAULT') THEN
                 WRITE (OUT,'(A,$)') '+ '//'ifstate='//IFS(:TRIM(IFS))
              END IF
	      WRITE (OUT,'(A)') '+'
	   END DO
	   RETURN
 	ELSE IF (LOAD_KEY) THEN
	   C = 0
	   IER = 1
	   WRITE (OUT,'(1X,A)') 'Keypad definitions:'
	   L = 1
	   DO WHILE (IER)
	      IER = SMG$LIST_KEY_DEFS(KEY_TABLE_ID,C,KEY,IFS,ATT,EQU,ST)
	      IF (IER) THEN
		 WRITE (OUT,'(3X,A,$)') KEY(:TRIM(KEY))//' = '//'"'//
     &					EQU(:TRIM(EQU))//'"'
	         IF (TRIM(ST).GT.0) THEN
		    WRITE (OUT,'(A,$)') '+ '//'state='//ST(:TRIM(ST))
		 END IF
                 IF (TRIM(IFS).GT.0.AND.IFS.NE.'DEFAULT') THEN
                    WRITE (OUT,'(A,$)') '+ '//'ifstate='
     &						//IFS(:TRIM(IFS))
                 END IF
		 WRITE (OUT,'(A)') '+'
 		 L = L + 1
                 IF (PAGING.AND.L.EQ.PAGE_LENGTH-1.AND.OUT.EQ.6) THEN
                    L = 0                        ! Reinitialize screen counter
	            CALL LIB$PUT_OUTPUT(' ')
		    CALL GET_INPUT_NOECHO_PROMPT(
     &				KEY(:1),'Press key to continue ... ')
             	    IER = LIB$ERASE_PAGE(1,1)         ! Erase display
		 END IF
	      END IF
	   END DO
           IF (OUT.EQ.8) CLOSE (UNIT=8,DISP='PRINT/DELETE')
	   RETURN
	END IF

	IF (OUT.EQ.8) THEN
	   CALL LBR$OUTPUT_HELP(PRINT_OUTPUT,,'KEYPAD'
     &		,LIBRARY,HLP$M_HELP)
	   CLOSE (UNIT=8,DISP='PRINT/DELETE')
	ELSE
	   CALL LBR$OUTPUT_HELP(LIB$PUT_OUTPUT,,'KEYPAD'
     &		,LIBRARY,HLP$M_HELP)
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
C	To create interactive help session.  Prompting is enabled.
C  INPUTS:
C	PARAMETER - Character string. Optional input parameter
C		    containing a list of help keys.
C	LIBRARY   - Character string. Name of help library.
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE '($LBRDEF)'

	COMMON /HELP/ HELP_PAGE,HELP_INPUT,HELP_INPUT_LEN
	COMMON /HELP/ NEED_ERASE,KEYBOARD_ID,KEY_TABLE_ID,OTHERINFO
	CHARACTER*80 HELP_INPUT

	COMMON /LEVELS/ KEY,KEYL,NKEY,OLD_NKEY,EXACT
	CHARACTER*20 KEY(10)
	DIMENSION KEYL(10)

	EXTERNAL PUT_OUTPUT

	CHARACTER*(*) LIBRARY,PARAMETER

	CHARACTER*80 PROMPT

	DATA KEYBOARD_ID/0/

	IF (KEYBOARD_ID.EQ.0) THEN
	   IER = SMG$CREATE_VIRTUAL_KEYBOARD(KEYBOARD_ID,,,,20)
	   IER = SMG$CREATE_KEY_TABLE(KEY_TABLE_ID)
	END IF

	CALL STR$TRIM(HELP_INPUT,PARAMETER,HELP_INPUT_LEN)	! Trim input

	CALL LBR$INI_CONTROL(LINDEX,LBR$C_READ)		! Init library read
	CALL LBR$OPEN(LINDEX,LIBRARY)			! Specify library name

	DO I=1,10					! Initialize key lengths
	   KEYL(I) = 0
	END DO

	NKEY = 0					! Number of help keys

	DO WHILE (NKEY.GE.0)	! Do until CTRL-Z entered or no more keys

	   HELP_PAGE = 0				! Init line counter
	   NEED_ERASE = .TRUE.				! Need to erase screen

	   OLD_NKEY = NKEY				! Save old key count
	   EXACT = .TRUE.				! Exact key match

	   DO WHILE (NKEY.LT.10.AND.HELP_INPUT_LEN.GT.0.AND.
     &					   HELP_INPUT(:1).NE.'?')
							! Break input into keys
	      NKEY = NKEY + 1				! Increment key counter

	      DO WHILE (HELP_INPUT(1:1).EQ.' '.AND.HELP_INPUT_LEN.GT.0)
		 HELP_INPUT = HELP_INPUT(2:HELP_INPUT_LEN)	! Strip spaces
		 HELP_INPUT_LEN = HELP_INPUT_LEN - 1	! at start of input
	      END DO

	      NEXT_KEY = 2

	      DO WHILE (NEXT_KEY.LE.HELP_INPUT_LEN		! Search for
     &		  .AND.HELP_INPUT(NEXT_KEY:NEXT_KEY).NE.' '	! space or
     &		  .AND.HELP_INPUT(NEXT_KEY:NEXT_KEY).NE.'/')	! backslash
		 NEXT_KEY = NEXT_KEY + 1	! indicating start of next key
	      END DO

	      IF (NEXT_KEY.GT.HELP_INPUT_LEN) THEN	! Found the last key
		 KEY(NKEY) = HELP_INPUT(:HELP_INPUT_LEN)	! Key string
		 KEYL(NKEY) = HELP_INPUT_LEN			! Key length
		 HELP_INPUT_LEN = 0
	      ELSE					! Found the next key
		 KEY(NKEY) = HELP_INPUT(:NEXT_KEY-1)
		 HELP_INPUT = HELP_INPUT(NEXT_KEY:HELP_INPUT_LEN)
		 KEYL(NKEY) = NEXT_KEY - 1
		 HELP_INPUT_LEN = HELP_INPUT_LEN - NEXT_KEY + 1
	      END IF
	   END DO
	   HELP_INPUT_LEN = 0
	   IER = LBR$GET_HELP(LINDEX,,PUT_OUTPUT,,	! Display help
     &		   KEY(1)(:KEYL(1)),KEY(2)(:KEYL(2)),
     &		   KEY(3)(:KEYL(3)),KEY(4)(:KEYL(4)),KEY(5)(:KEYL(5)),
     &		   KEY(6)(:KEYL(6)),KEY(7)(:KEYL(7)),KEY(8)(:KEYL(8)),
     &		   KEY(9)(:KEYL(9)),KEY(10)(:KEYL(10)))

	   IF (IER.EQ.0.AND.HELP_INPUT_LEN.GT.0) IER = 1
		! IER = 0 special case means input given to full screen prompt

	   IF (KEY(NKEY).EQ.'*'.OR..NOT.EXACT) THEN	! If not exact match
	      DO I=OLD_NKEY+1,NKEY			! then don't update
		 KEYL(I) = 0				! new keys
	      END DO
	      NKEY = OLD_NKEY
	   END IF

	   IF (IER.AND.NKEY.GT.0.AND.OTHERINFO.EQ.0) THEN ! No subtopics?
	      KEYL(NKEY) = 0				! Back up one key level
	      NKEY = NKEY - 1
	   END IF

	   DO WHILE (HELP_INPUT_LEN.EQ.0.AND.IER.AND.NKEY.GE.0)
	      IF (NKEY.EQ.0) THEN	! If top level, prompt for topic
	         IER = SMG$READ_COMPOSED_LINE(KEYBOARD_ID,KEY_TABLE_ID,
     &		   HELP_INPUT,'Topic? ',HELP_INPUT_LEN)
	      ELSE			! If not top level, prompt for subtopic
		 LPROMPT = 0		! Create subtopic prompt line
		 DO I=1,NKEY		! Put spaces in between keys
		    PROMPT = PROMPT(:LPROMPT)//KEY(I)(:KEYL(I))//' '
		    LPROMPT = LPROMPT + KEYL(I) + 1
		 END DO
		 PROMPT = PROMPT(:LPROMPT)//'Subtopic? '
		 LPROMPT = LPROMPT + 10
	         IER = SMG$READ_COMPOSED_LINE(KEYBOARD_ID,KEY_TABLE_ID,
     &		   HELP_INPUT,PROMPT(:LPROMPT),HELP_INPUT_LEN)
	      END IF
	      CALL STR$TRIM(HELP_INPUT,HELP_INPUT,HELP_INPUT_LEN)
	      IF (IER.AND.HELP_INPUT_LEN.EQ.0) THEN	! If RETURN entered
		 KEYL(NKEY) = 0				! Back up one key level
		 NKEY = NKEY - 1
	      END IF
	   END DO

	   IF (.NOT.IER.OR.NKEY.LT.0) THEN	! If CTRL-Z above top level,
	      CALL LIB$PUT_OUTPUT(' ')		! Skip line
	      CALL LBR$CLOSE(LINDEX)		! then close library,
	      RETURN				! and end help session.
	   END IF

	END DO

	END



	INTEGER FUNCTION PUT_OUTPUT(INPUT,INFO,DATA,LEVEL)
C
C  FUNCTION PUT_OUTPUT
C
C  FUNCTION:
C	Output routine for input from LBR$GET_HELP.  Displays
C	help text on terminal with full screen prompting.
C  INPUTS:
C	INPUT - Character string.  Line of input text.
C	INFO  - Longword.  Contains help flag bits.
C	DATA  - Longword.  Not presently used.
C	LEVEL - Longword.  Contains current key level.
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE '($HLPDEF)'

	COMMON /LEVELS/ KEY,KEYL,NKEY,OLD_NKEY,EXACT
	CHARACTER*20 KEY(10)
	DIMENSION KEYL(10)

	COMMON /HELP/ HELP_PAGE,HELP_INPUT,HELP_INPUT_LEN
	COMMON /HELP/ NEED_ERASE,KEYBOARD_ID,KEY_TABLE_ID,OTHERINFO
	CHARACTER*80 HELP_INPUT

	COMMON /PAGE/ PAGE_LENGTH,PAGE_WIDTH,PAGING

	CHARACTER INPUT*(*)

	CHARACTER SPACES*20
	DATA SPACES /' '/

	OTHERINFO = INFO.AND.HLP$M_OTHERINFO

	IF ((INFO.AND.HLP$M_NOHLPTXT).NE.0) THEN	! Key cannot be found
	   NEED_ERASE = .FALSE.				! Don't erase screen
	   IF (HELP_PAGE.EQ.0) THEN		! If first line of help text
	      DO I=OLD_NKEY+1,NKEY		! remove any new keys that
		 KEYL(I) = 0			! were inputted, as they are
	      END DO				! not valid, as no match
	      NKEY = OLD_NKEY			! could be found.
	   END IF
	ELSE IF ((INFO.AND.HLP$M_KEYNAMLIN).NE.0.AND.NKEY.GT.0.AND.
     &		 LEVEL.GT.OLD_NKEY.AND.KEY(NKEY)(:KEYL(NKEY)).NE.'*'.AND.
     &		 %LOC(INPUT).NE.0) THEN		! If text contains key names
			! Update if not wildcard search and they are new keys
	   IF (KEYL(LEVEL).GT.0) THEN		! If key already updated
	      EXACT = .FALSE.		! Must be more than one match possible
	   END IF			! so indicate not exact match.
	   START_KEY = 1		! String preceeding spaces.
	   DO WHILE (INPUT(START_KEY:START_KEY).EQ.' ')
	      START_KEY = START_KEY + 1
	   END DO
	   KEY(LEVEL) = INPUT(START_KEY:)			! Store new key
	   CALL STR$TRIM(KEY(LEVEL),KEY(LEVEL),KEYL(LEVEL))	! & key length
	ELSE IF (HELP_PAGE.EQ.0) THEN		! If first line of text,
	   DO I=OLD_NKEY+1,NKEY			! remove any new keys that
	      KEYL(I) = 0			! were just inputted, allowing
	   END DO				! this routine to fill them.
	END IF

	IF (NEED_ERASE) THEN			! Need to erase screen?
	   IER = LIB$ERASE_PAGE(1,1)		! i.e. start of new topic.
	   NEED_ERASE = .FALSE.
	END IF

	HELP_PAGE = HELP_PAGE + 1		! Increment screen counter
	IF (PAGING.AND.HELP_PAGE.GT.PAGE_LENGTH-2) THEN		! End of page?
	   HELP_PAGE = 0			! Reinitialize screen counter
	   CALL LIB$PUT_OUTPUT(' ')	! Skip line and prompt for next screen
	   IER = SMG$READ_COMPOSED_LINE(KEYBOARD_ID,KEY_TABLE_ID,
     &		HELP_INPUT,'Press RETURN to continue ... ',HELP_INPUT_LEN)
	   CALL STR$TRIM(HELP_INPUT,HELP_INPUT,HELP_INPUT_LEN)	! Trim input
	   IF (.NOT.IER.OR.HELP_INPUT_LEN.GT.0) THEN	! CTRL-Z or Text input?
	      EXACT = .TRUE.	! If more than one match was found and being
				! displayed, text input specifies that the
				! current displayed match is desired.
	      PUT_OUTPUT = 0	! Stop any more of current help display.
	   ELSE					! Else if RETURN entered
	      IER = LIB$ERASE_PAGE(1,1)		! Erase display
	      NSPACES = LEVEL*2		! Number of spaces to indent output
	      IF ((INFO.AND.HLP$M_KEYNAMLIN).NE.0) NSPACES = NSPACES - 2
		! Key name lines are indented 2 less than help description.
	      IF (NSPACES.GT.0) THEN	! Add spaces if present to output
		 PUT_OUTPUT =  LIB$PUT_OUTPUT(SPACES(:NSPACES)//INPUT)
	      ELSE			! Else just output text.
		 PUT_OUTPUT =  LIB$PUT_OUTPUT(INPUT)
	      END IF
	      HELP_PAGE = 1		! Increment page counter.
	   END IF
	ELSE				! Else if not end of page
	   NSPACES = LEVEL*2		! Just output text line
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

	INCLUDE 'BULLUSER.INC'

	CHARACTER VERSION*12,DATE*24

	INTEGER BTIM(2)

	CALL READ_HEADER(VERSION,DATE)

	WRITE (6,'(A)') ' BULLETIN Version '//VERSION(:TRIM(VERSION))

	WRITE (6,'(A)') ' Linked on '//DATE(:TRIM(DATE))

	RETURN

	ENTRY SHOW_NEW_VERSION

	CALL READ_HEADER(VERSION,DATE)

	IER = SYS$BINTIM(DATE(:TRIM(DATE)),BTIM)
	IF (.NOT.IER) RETURN

C	IF (COMPARE_BTIM(READ_BTIM,BTIM).LT.0) THEN
C	   WRITE (6,'(A)') ' A new BULLETIN executable has been '//
C     &			   'installed since your last use.'
C	   WRITE (6,'(A)') 
C     &		' Type HELP NEW_FEATURES for help on any new features.'
C	END IF

	RETURN
	END




	SUBROUTINE FULL_DIR
C
C	Add INDEX command to BULLETIN, display directories of ALL
C	folders.
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'
	INCLUDE 'BULLFILES.INC'
	INCLUDE 'BULLFOLDER.INC'
	INCLUDE 'BULLUSER.INC'

	COMMON /POINT/ BULL_POINT

	COMMON /TAGS/ BULL_TAG,READ_TAG

        COMMON /NEW_DIR/ NEW

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /PAGE/ PAGE_LENGTH,PAGE_WIDTH,PAGING
	LOGICAL PAGING

	DATA FOLDER_Q1/0/

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*256 INCMD

	COMMON /COUNT/
     &     DIR_COUNT,	   ! # directory entry to continue bulletin read from
     &     READ_COUNT,	   ! # block that bulletin READ is to continue from
     &     FOLDER_COUNT,   ! # folder entry to continue SHOW/ALL folder from
     &	   INDEX_COUNT

	CHARACTER NEWS_ACCESS*132,DATETIME*20

	EXTERNAL BULLETIN_SUBCOMMANDS

	IF (NUM_FOLDERS.GT.0.AND..NOT.CLI$PRESENT('RESTART')
     &		.AND.INDEX_COUNT.EQ.1) THEN
	   INDEX_COUNT = 2
	   DIR_COUNT = 0
	END IF

	IF (INDEX_COUNT.EQ.1) THEN
	  CALL INIT_QUEUE(FOLDER_Q1,FOLDER1_COM)

	  FOLDER_Q = FOLDER_Q1

	  SET = CLI$PRESENT('SET')
	  NEW = CLI$PRESENT('NEW')
	  INEW = NEW

	  IREAD_TAG = IBSET(0,1) + IBSET(0,2)
	  IF (CLI$PRESENT('MARKED')) THEN
	     IREAD_TAG = 1 + IBSET(0,1)
	  ELSE IF (CLI$PRESENT('SEEN')) THEN
	     IREAD_TAG = 1 + IBSET(0,2)
	  ELSE IF (CLI$PRESENT('UNMARKED').OR.CLI$PRESENT
     &		   ('MARKED').EQ.%LOC(CLI$_NEGATED)) THEN
	     IREAD_TAG = 1 + IBSET(0,1) + IBSET(0,3)
	  ELSE IF (CLI$PRESENT('UNSEEN').OR.CLI$PRESENT
     &		   ('SEEN').EQ.%LOC(CLI$_NEGATED)) THEN
	     IREAD_TAG = 1 + IBSET(0,2) + IBSET(0,3)
 	  END IF

	  NEW = NEW.AND..NOT.IREAD_TAG

	  SUBSCRIBE = CLI$PRESENT('SUBSCRIBE')
	  IF (SUBSCRIBE) THEN
	     CALL NEWS_GET_SUBSCRIBE(0,F1_COUNT)
	     SUBNUM = 1
	     CALL OPEN_BULLNEWS_SHARED
	  ELSE
	     CALL OPEN_BULLFOLDER_SHARED
	  END IF

	  NUM_FOLDERS = 0
	  IER = 0
	  DO WHILE (IER.EQ.0)			! Copy all bulletins from file
	    IF (SUBSCRIBE) THEN
	       IER = 1
	       DO WHILE (SUBNUM.NE.0.AND.IER.NE.0)
	          CALL NEWS_GET_SUBSCRIBE(SUBNUM,MSGNUM)
		  IF (SUBNUM.NE.0) THEN
	             CALL READ_FOLDER_FILE_KEYNUM_TEMP(SUBNUM,IER)
		     IF (IER.NE.0) THEN
			SUBNUM = -1
		     ELSE IF (NEW.AND.(MSGNUM.GE.F1_NBULL.OR.F1_NBULL
     &			  .EQ.0.OR.F1_START.GT.F1_NBULL)) THEN
		        IER = 1
	             END IF
		  END IF
	       END DO
	       IF (SUBNUM.EQ.0) IER = 1
	    ELSE
	       FOUND = .FALSE.
	       DO WHILE (.NOT.FOUND.AND.IER.EQ.0)
	          CALL READ_FOLDER_FILE_TEMP(IER)
	          IF (IER.EQ.0) THEN
		     IF (.NOT.SET.OR.TEST2(SET_FLAG,FOLDER1_NUMBER)
     &			.OR.TEST2(BRIEF_FLAG,FOLDER1_NUMBER)) THEN
			FOUND = .NOT.NEW.OR.COMPARE_BTIM(LAST_READ_BTIM
     &			   (1,FOLDER1_NUMBER+1),F1_NEWEST_BTIM).LT.0
                     END IF
	          END IF
	       END DO
	    END IF
	    IF (IER.EQ.0) THEN
	      IF (BTEST(FOLDER1_FLAG,0).AND..NOT.SETPRV_PRIV()) THEN
		 FOLDER1_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))
     &					//FOLDER1
	         IF (SUBSCRIBE) THEN
	            CALL CHECK_ACCESS
     &		     (NEWS_ACCESS(FOLDER_DESCRIP),
     &		      USERNAME,READ_ACCESS,-1)
	         ELSE
	            CALL CHECK_ACCESS
     &		     (FOLDER1_FILE(:TRIM(FOLDER1_FILE))//'.BULLFIL',
     &		      USERNAME,READ_ACCESS,-1)
	         END IF
	      ELSE
		 READ_ACCESS = 1
	      END IF
	      IF (READ_ACCESS) THEN
	         NUM_FOLDERS = NUM_FOLDERS + 1
	         CALL WRITE_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER1_COM)
	      END IF
	    END IF
	  END DO

	  CALL CLOSE_BULLFOLDER			! We don't need file anymore

	  FOLDER_Q = FOLDER_Q1			! Init queue pointer to header
	  IF (NEW) THEN
	     WRITE (6,1010)
	  ELSE
	     WRITE (6,1000)
	  END IF
	  IF (.NOT.SUBSCRIBE) THEN
	     WRITE (6,'(1X,''Folder'',22X,''Last message'',7X,''Messages'',
     &		2X,''Owner'',/,1X,80(''-''))')
	  ELSE
	     WRITE (6,'(1X,''News group'',<PAGE_WIDTH-80+39>X,1X,
     &		''First        Last   Last Read'',/,1X,<PAGE_WIDTH>(''-''))')
	  END IF
          NUM_FOLDER = 0
	  IF (SUBSCRIBE) FLEN = MIN(81,PAGE_WIDTH-80+42)
	  DO I = 1,NUM_FOLDERS
	   CALL READ_QUEUE(%VAL(FOLDER_Q),FOLDER_Q,FOLDER1_COM)
	   IF (SUBSCRIBE) THEN
	      J = INDEX(FOLDER1_DESCRIP,' ')
	      IF (J.GT.0.AND.FOLDER1_DESCRIP(J+1:J+1).NE.'=') THEN
                 FOLDER1_DESCRIP = FOLDER1_DESCRIP(:J-1)
              END IF
	      IF (F1_START.LE.F1_NBULL) THEN
                 NEWS_FOLDER1_NUMBER = FOLDER1_NUMBER
		 CALL NEWS_GET_NEWEST_MESSAGE1(NEWS_NEW)
	         WRITE (6,1015) '*'//FOLDER1_DESCRIP(:FLEN-1),
     &					F1_START,F1_NBULL,NEWS_NEW-1
              ELSE
	         WRITE (6,1015) ' '//FOLDER1_DESCRIP(:FLEN-1),0,0,0
	      END IF
	   ELSE
	      IF (F1_NBULL.GT.0) THEN
	         CALL SYS$ASCTIM(,DATETIME,F1_NEWEST_BTIM,)
	      ELSE
	         DATETIME = '      NONE'
	      END IF
	      WRITE (6,1030) FOLDER1,DATETIME(:17),F1_NBULL,
     &							FOLDER1_OWNER
	   END IF
	   NUM_FOLDER = NUM_FOLDER + 1
	   IF (I.NE.NUM_FOLDERS.AND.PAGING.AND.((NUM_FOLDER+6.EQ.PAGE_LENGTH
     &		.AND.I.EQ.NUM_FOLDER).OR.(NUM_FOLDER+2.EQ.PAGE_LENGTH.AND.
     &		I.NE.NUM_FOLDER))) THEN
	      NUM_FOLDER = 0
	      WRITE(6,1080)	! Ask for input to proceed to next page
	      CALL GET_INPUT_NOECHO_PROMPT(DATETIME(:1),
     &			'HIT any key for next page....')
	   END IF
	  END DO
	  IF (NUM_FOLDERS.EQ.0) THEN
	     WRITE (6,1050)
	     INDEX_COUNT = 0
	     RETURN
	  END IF
	  WRITE (6,1060)
	  FOLDER_Q = FOLDER_Q1			! Init queue pointer to header
	  INDEX_COUNT = 2
	  DIR_COUNT = 0
	  RETURN
	ELSE IF (INDEX_COUNT.EQ.2) THEN
	 READ_TAG = IREAD_TAG
	 IF (DIR_COUNT.LE.0) THEN
	  F1_NBULL = 0
	  DIR_COUNT = 0
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
	 ELSE IF (INEW) THEN
	    NEW = INEW
            IF (REMOTE_SET.GE.3) THEN
	       CALL NEWS_GET_NEWEST_MESSAGE(IER)
	       IF (IER.GT.0.AND.IER.LE.F_NBULL) BULL_POINT = IER - 1	
	    ELSE
	       CALL FIND_NEWEST_BULL
	    END IF
	 END IF

	 IF (INCMD(:4).NE.'INDE') THEN 
	    IER = CLI$DCL_PARSE('INDEX',BULLETIN_SUBCOMMANDS)
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

1000	FORMAT (' The following folders are present'/)
1010	FORMAT (' The following folders with new messages are present'/)
1015    FORMAT(1X,A<FLEN>,<PAGE_WIDTH-FLEN-36-1>X,2X,I10,2X,I10,2X,I10)
1030	FORMAT(1X,A26,2X,A17,2X,I8,2X,A12)
1035	FORMAT (1X,A,1X,I6)
1040	FORMAT (' Type Return to continue to the next folder...')
1050	FORMAT (' End of folder search.')
1060	FORMAT (' Type Return to continue...')
1080	FORMAT(' ',/)

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

	INCLUDE 'BULLDIR.INC'

	COMMON /POINT/ BULL_POINT

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

	COMMON /CTRLC_FLAG/ FLAG

	DIMENSION NOLOGIN_BTIM(2),START_BTIM(2)

	CHARACTER DATETIME*17

	DIMENSION LAST(2,FOLDER_MAX)
	INTEGER*2 LAST2(4,FOLDER_MAX)
	EQUIVALENCE (LAST,LAST2)

	ALL = CLI$PRESENT('NOLOGIN').OR.CLI$PRESENT('ALL')
     &				.OR.CLI$PRESENT('LOGIN')

	SETPRV = SETPRV_PRIV()		! SETPRV_PRIV rewrites TEMP_USER

	IF (.NOT.ALL) THEN
	   IER = CLI$GET_VALUE('USERNAME',TEMP_USER)
	   IF (.NOT.IER) TEMP_USER = USERNAME
	END IF

	IF (.NOT.SETPRV.AND.(ALL.OR.USERNAME.NE.TEMP_USER)) THEN
	   WRITE (6,'('' ERROR: No privs to use command.'')')
	   RETURN
	END IF

	CALL SYS_BINTIM('5-NOV-2956 00:00:00.00',NOLOGIN_BTIM)

	FOLDER_PRESENT = CLI$PRESENT('FOLDER')

	IF (FOLDER_PRESENT) THEN
	   IER = CLI$GET_VALUE('FOLDER',FOLDER1_NAME)
	   IF (.NOT.IER) FOLDER1_NAME = FOLDER_NAME
	   NEWS = INDEX(FOLDER1_NAME,'.').GT.0.OR.(FOLDER1_NAME(:1)
     &			.GE.'a'.AND.FOLDER1_NAME(:1).LE.'z')
           IF (.NOT.NEWS) THEN
	      CALL OPEN_BULLFOLDER_SHARED
	   ELSE
	      CALL OPEN_BULLNEWS_SHARED
	      CALL LOWERCASE(FOLDER1_NAME)
	   END IF
	   CALL READ_FOLDER_FILE_KEYNAME_TEMP
     &				(FOLDER1_NAME(:TRIM(FOLDER1_NAME)),IER)
	   CALL CLOSE_BULLFOLDER
	   IF (IER.NE.0) THEN
	      WRITE (6,'('' ERROR: Folder not found.'')')
	      RETURN
	   END IF
	END IF

	SINCE = CLI$PRESENT('SINCE').OR.CLI$PRESENT('START')
	IF (CLI$GET_VALUE('SINCE',BULL_PARAMETER,LEN_P)) THEN
	   IF (.NOT.NEWS) THEN
	      IER = SYS_BINTIM(BULL_PARAMETER,START_BTIM)
	      IF (.NOT.IER) THEN
	         WRITE (6,'('' ERROR: Invalid date specified.'')')
		 RETURN
	      END IF
	   ELSE
	      WRITE (6,'('' ERROR: /SINCE not valid with NEWS group.'')')
	      RETURN
	   END IF
	ELSE IF (CLI$GET_VALUE('START',BULL_PARAMETER,LEN_P)) THEN
	   IF (NEWS) THEN
	      IER = OTS$CVT_TI_L(BULL_PARAMETER(:LEN_P),
     &				    STARTMSG,,%VAL(1))
	      IF (.NOT.IER) THEN
		 WRITE (6,'('' ERROR: Invalid number specified.'')')
		 RETURN
	      END IF
	   ELSE
	      WRITE (6,'('' ERROR: /START not valid with folder.'')')
	      RETURN
	   END IF
	ELSE IF (SINCE) THEN
	   IF (BULL_POINT.EQ.0) THEN
	      WRITE (6,'('' ERROR: No current message.'')')
	      RETURN
	   ELSE IF (NEWS) THEN
	      STARTMSG = BULL_POINT
	   ELSE
	      START_BTIM(1) = MSG_BTIM(1)
	      START_BTIM(2) = MSG_BTIM(2)
	   END IF
	ELSE IF (.NOT.NEWS) THEN
	   CALL SYS_BINTIM('6-NOV-1956 00:00:00.00',START_BTIM)
	ELSE
	   STARTMSG = 1
	END IF

	CALL DISABLE_CTRL
	CALL DECLARE_CTRLC_AST
	IF (FOLDER_PRESENT) THEN
	   CALL OPEN_BULLINF_SHARED
	   IER = 0
	   DO WHILE (IER.EQ.0.AND.FLAG.NE.1)
	      IF (ALL) THEN
	         DO WHILE (REC_LOCK(IER))
	            READ (9,IOSTAT=IER) TEMP_USER,LAST
	         END DO
	      ELSE
		 IF (NEWS) THEN
		    LU = TRIM(TEMP_USER)
		    TEMP_USER(LU:LU) = CHAR(128.OR.ICHAR(TEMP_USER(LU:LU)))
		    IF (LU.GT.1) THEN
		       TEMP_USER(LU-1:LU-1) =
     &			        CHAR(128.OR.ICHAR(TEMP_USER(LU-1:LU-1)))
		    ELSE
		       TEMP_USER(2:2) = CHAR(128.OR.ICHAR(TEMP_USER(2:2)))
	 	    END IF
	 	 END IF
	         DO WHILE (REC_LOCK(IER))
	            READ (9,KEY=TEMP_USER,IOSTAT=IER) TEMP_USER,LAST
	         END DO
	      END IF 
	      UNLOCK 9
	      IF (IER.EQ.0) THEN
	         LU = TRIM(TEMP_USER)
		 I = MAX(LU,2)
		 DO WHILE (I.GT.0.AND..NOT.BTEST(ICHAR(TEMP_USER(I:I)),7))
		    I = I - 1
		 END DO
		 IF (NEWS.AND.I.GE.LU.AND.I.NE.1.AND.
     &		     BTEST(ICHAR(TEMP_USER(I-1:I-1)),7)) THEN
		    TEMP_USER(I:I) = CHAR(ICHAR(TEMP_USER(I:I)).AND.127)
		    TEMP_USER(I-1:I-1) =
     &				 CHAR(ICHAR(TEMP_USER(I-1:I-1)).AND.127)
		    I = 0
		    NEWSMSG = 1
		    DO WHILE (LAST2(1,NEWSMSG).NE.NEWS_FOLDER1_NUMBER
     &					.AND.NEWSMSG.LE.FOLDER_MAX)
		       NEWSMSG = NEWSMSG + 1
	            END DO
		    IF (NEWSMSG.LE.FOLDER_MAX) THEN
		       FOUND = LAST(2,NEWSMSG).GE.STARTMSG
		    ELSE
		       FOUND = .FALSE.
		    END IF
		 ELSE IF (.NOT.NEWS.AND.I.EQ.0) THEN
		    FOUND = COMPARE_BTIM
     &			(START_BTIM,LAST(1,FOLDER1_NUMBER+1)).LE.0
		 ELSE
		    FOUND = .FALSE.
		 END IF
		 IF (FOUND.AND.NEWS) THEN
	            WRITE (6,'(1X,A,'' latest message read '',
     &			I<LOG10(REAL(LAST(2,NEWSMSG)))+1>,''.'')')
     &			TEMP_USER(:TRIM(TEMP_USER)),LAST(2,NEWSMSG)
		 ELSE IF (FOUND) THEN
		    CALL SYS$ASCTIM(,DATETIME,LAST(1,FOLDER1_NUMBER+1),)
	            WRITE (6,'(1X,A,'' latest message read '',A,''.'')')
     &				TEMP_USER(:TRIM(TEMP_USER)),DATETIME
		 ELSE IF (.NOT.ALL) THEN
	            WRITE (6,'('' User has never read or not subscribed'',
     &			'' to specified folder.'')')
	         END IF
	      END IF
	      IF (.NOT.ALL) THEN
		 IF (IER.NE.0) THEN
		    WRITE (6,'('' User info does not exist.'')')
		 END IF
		 IER = 2
	      END IF
	   END DO
	   CALL CLOSE_BULLINF
	ELSE IF (.NOT.ALL) THEN
	   CALL OPEN_BULLUSER_SHARED
	   CALL READ_USER_FILE_KEYNAME(TEMP_USER,IER)
	   IF (IER.EQ.0) THEN
	      IF (COMPARE_BTIM(LOGIN_BTIM,NOLOGIN_BTIM).GE.0) THEN
	         WRITE (6,'('' NOLOGIN set for specified user.'')')
	      ELSE
	         CALL SYS$ASCTIM(,DATETIME,LOGIN_BTIM,)
	         WRITE (6,'('' User last logged in at '',A,''.'')')
     &						DATETIME
	      END IF
	   ELSE
	      WRITE (6,'('' Entry for specified user not found.'')')
	   END IF
	   CALL CLOSE_BULLUSER
	ELSE
	   CALL OPEN_BULLUSER_SHARED
	   CALL READ_USER_FILE(IER)
	   DO WHILE (IER.EQ.0.AND.FLAG.NE.1)
	      CALL READ_USER_FILE(IER)
	      IF (IER.EQ.0.AND.TEMP_USER(:1).NE.':'.AND.
     &				TEMP_USER(:1).NE.'*') THEN
		 IER1 = COMPARE_BTIM(LOGIN_BTIM,NOLOGIN_BTIM)
		 IF (.NOT.CLI$PRESENT('LOGIN').AND.IER1.GE.0) THEN
	            WRITE (6,'('' NOLOGIN set for '',A,''.'')')
     &					TEMP_USER(:TRIM(TEMP_USER))
		 ELSE IF (.NOT.CLI$PRESENT('NOLOGIN').AND.IER1.LT.0.AND.
     &			  COMPARE_BTIM(START_BTIM,LOGIN_BTIM).LE.0) THEN
	            CALL SYS$ASCTIM(,DATETIME,LOGIN_BTIM,)
	            WRITE (6,'(1X,A,'' last logged in at '',A,''.'')')
     &				TEMP_USER(:TRIM(TEMP_USER)),DATETIME
		 END IF
	      END IF
	   END DO
	   CALL CLOSE_BULLUSER
	END IF
	CALL CANCEL_CTRLC_AST
	CALL ENABLE_CTRL

	RETURN
	END




	SUBROUTINE INIT_MESSAGE_ADD(IN_FOLDER,IN_FROM,IN_DESCRIP,IER)
C
C  SUBROUTINE INIT_MESSAGE_ADD
C
C  FUNCTION:  Opens specified folder in order to add message.
C
C  INPUTS:
C	IN_FOLDER  - Character string containing folder name
C	IN_FROM	   - Character string containing name of owner of message.
C		     If empty, the message is searched for either a
C		     Reply-to: field or a From: field.  If none, then
C		     the owner of the process is used.  If IN_FROM
C		     ends with a %, it is assumed that it is simply
C		     the prefix that should be when responding to the
C		     address via MAIL.  I.e. the PMDF interface sends
C		     IN%, so when the From: field is found, the message
C		     owner becomes IN%"from-address".
C	IN_DESCRIP - Character string containing subject of message.
C		     If empty, the message is searched for a line
C		     which starts with "Subj:" or "Subject:".
C  OUTPUTS:
C	IER - Error status.  True if properly connected to folder.
C		False if folder not found.
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
	COMMON /MAIN_HEADER_INFO/ INEXDATE
	CHARACTER*(INPUT_LENGTH) INFROM,INDESCRIP

	COMMON /TEXT_PRESENT/ TEXT

	COMMON /SAVE_IN/ SAVE_IN_DESCRIP,SAVE_IN_FROM
	CHARACTER*(INPUT_LENGTH) SAVE_IN_DESCRIP,SAVE_IN_FROM

	COMMON /LAST_BUFFER/ OLD_BUFFER
	CHARACTER*(INPUT_LENGTH) OLD_BUFFER

	COMMON /OLD_BUFFER/ OLD_BUFFER_FROM,OLD_BUFFER_SUBJ
	DATA OLD_BUFFER_FROM /.FALSE./, OLD_BUFFER_SUBJ /.FALSE./

	COMMON /SCRTYPE/ SCRTYPE,SCRNAME
	CHARACTER*132 SCRNAME
	DATA SCRTYPE/-1/

	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P
	CHARACTER*64 BULL_PARAMETER

        COMMON /MAIN_FOLDER_DIRECTORY/ FOLDER1_DIRECTORY
        CHARACTER*80 FOLDER1_DIRECTORY

	IER = LIB$SYS_TRNLOG('BULL_DISABLE',LEN_P,BULL_PARAMETER)
	IF (IER.EQ.1.AND.LEN_P.GT.0
     &	    .AND.BULL_PARAMETER(:LEN_P).NE.'ENABLE') CALL SYS$DELPRC(,)

	BULLCP = 1			! Inhibit folder cleanup subprocess

	CALL CHECK_DIR_ACCESS()

	CALL INIT_COMPRESS

	FOLDER1_DIRECTORY = FOLDER_DIRECTORY

	IER = 1
	DO WHILE (IER.NE.0)
	   CALL OPEN_BULLFOLDER			! Get folder file

	   CALL READ_FOLDER_FILE_KEYNAME(IN_FOLDER(:TRIM(IN_FOLDER)),IER)

	   CALL CLOSE_BULLFOLDER

	   IF (IER.NE.0) THEN
	      IER1 = 1
	      DO WHILE (IER1)
	         IER2 = SYS_TRNLNM_SYSTEM_INDEX('BULL_DIR_LIST',
     &			   FOLDER_DIRECTORY)
	         IF (IER2.AND.FOLDER_DIRECTORY.EQ.FOLDER1_DIRECTORY) THEN 
		    IER1 = 1
	         ELSE
		    IER1 = 0
		 END IF
      	      END DO
	      IF (IER2) THEN
		 CALL ADD_DIRECTORIES
	      ELSE
		 CALL ERRSNS(IDUMMY,IER)
	         RETURN
	      END IF
	   END IF
	END DO
	IER = 1

        FOLDER_NAME = FOLDER

	ENTRY INIT_MESSAGE_ADD_BBOARD(IN_FROM,IN_DESCRIP,IER)

	LDESCR = 0

	TEXT = .FALSE.			! No text written, as of yet

	FIRST_BREAK = .TRUE.

	IF (FOLDER_NUMBER.EQ.0) THEN	! If GENERAL folder
	   FOLDER_SET = .FALSE.		! indicate it
	ELSE				! Else it's another folder
	   FOLDER_SET = .TRUE.		! indicate it
	END IF

	FOLDER_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//
     &		FOLDER			! set folder file names

	ENTRY INIT_MESSAGE_ADD_DIGEST(IN_FROM,IN_DESCRIP,IER)

	CALL OPEN_BULLDIR		! Open directory file

	CALL OPEN_BULLFIL		! Open data file

	CALL READDIR(0,IER1)		! Get NBLOCK
	IF (IER1.EQ.0) NBLOCK = 0	! If new file, NBLOCK is 0

	NBLOCK = NBLOCK + 1
	LENGTH = NBLOCK			! Initialize line count
	IF (NEWS_FEED()) THEN
	   CALL STRIP_HEADER(' ',-1,IER)
	END IF

	LEN_FROM = TRIM(IN_FROM)

	IF (IN_FROM(LEN_FROM:LEN_FROM).EQ.'%') THEN	! Just protocol
	   PROTOCOL = IN_FROM(:LEN_FROM)//'"'
	   LPRO = LEN_FROM + 1
	   LEN_FROM = 0
	END IF

	MAIL = BTEST(FOLDER_FLAG,11).AND.INDEX(FOLDER_DESCRIP,'<').GT.0
	IF (MAIL) THEN 
	   SCRNAME = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//'BULL.MAIL'
	   OPEN (UNIT=3,DISPOSE='DELETE',FILE=SCRNAME,
     &	    FORM='FORMATTED',RECL=LINE_LENGTH,IOSTAT=IER1,STATUS='NEW')
	   SCRTYPE = 0
	   SAVE_IN_DESCRIP = IN_DESCRIP
	   SAVE_IN_FROM = ' '
	ELSE IF (NEWS_FEED().OR.LEN_FROM.EQ.0
     &		.OR.(BTEST(FOLDER_FLAG,5).AND.FIRST_BREAK)) THEN
	   SCRTYPE = 0
	   SCRNAME = 'SYS$LOGIN:BULL.SCR'
	   OPEN (UNIT=3,DISPOSE='DELETE',FILE=SCRNAME,
     &	    FORM='FORMATTED',RECL=LINE_LENGTH,IOSTAT=IER1,STATUS='NEW')
	   IF (IER1.NE.0) THEN
	      SCRNAME = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))//'BULL.SCR'
	      OPEN (UNIT=3,DISPOSE='DELETE',FILE=SCRNAME,
     &		FORM='FORMATTED',RECL=LINE_LENGTH,STATUS='NEW')
	   END IF
	   SAVE_IN_DESCRIP = IN_DESCRIP
	   SAVE_IN_FROM = ' '
	END IF

	IF (LEN_FROM.GT.0) THEN
	   INFROM = IN_FROM
	   IF (.NOT.BTEST(FOLDER_FLAG,5)) THEN
	      CALL STORE_FROM(INFROM,LEN_FROM)
	   ELSE	IF (INDEX(INFROM,'%"').GT.0) THEN	! Store any protocol
	      LPRO = INDEX(INFROM,'%"') + 1
	      PROTOCOL = INFROM(:LPRO)
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
	END IF

	OLD_BUFFER = ' '

	OLD_BUFFER_SUBJ = .FALSE.
	OLD_BUFFER_FROM = .FALSE.

	INEXDATE = .FALSE.

	RETURN
	END



	SUBROUTINE WRITEOUT_STORED

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLDIR.INC'

	COMMON /MAIN_HEADER_INFO/ INFROM,INDESCRIP,LEN_FROM,LEN_DESCRP
	COMMON /MAIN_HEADER_INFO/ INEXDATE
	CHARACTER*(INPUT_LENGTH) INFROM,INDESCRIP

	COMMON /STORED/ STORED

	CHARACTER*256 BUFFER

	REWIND (UNIT=3)

      	IER = 0
	DO WHILE (IER.EQ.0)
	   READ (3,'(A)',IOSTAT=IER) BUFFER
	   IF (IER.EQ.0) THEN
	      CALL WRITE_MESSAGE_LINE(BUFFER)
	   END IF
	END DO

	IF (BTEST(FOLDER_FLAG,5)) RETURN
	IF (.NOT.NEWS_FEED().AND.
     &	    .NOT.BTEST(FOLDER_FLAG,11).AND..NOT.STORED) CLOSE (UNIT=3)
        IF (BTEST(FOLDER_FLAG,11)) REWIND (UNIT=3)

	RETURN
	END



	SUBROUTINE WRITE_MESSAGE_LINE(BUFFER)
C
C  SUBROUTINE WRITE_MESSAGE_LINE
C
C  FUNCTION:  Writes one line of message into folder.
C
C  INPUTS:
C	BUFFER - Character string containing line to be put into message.
C

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'

	COMMON /MAIL_PROTOCOL/ PROTOCOL,LPRO
	CHARACTER*12 PROTOCOL

	COMMON /MAIN_HEADER_INFO/ INFROM,INDESCRIP,LEN_FROM,LEN_DESCRP
	COMMON /MAIN_HEADER_INFO/ INEXDATE
	CHARACTER*(INPUT_LENGTH) INFROM,INDESCRIP

	COMMON /DIGEST/ LDESCR,FIRST_BREAK
	DATA FIRST_BREAK/.TRUE./

	COMMON /TEXT_PRESENT/ TEXT

	COMMON /SAVE_IN/ SAVE_IN_DESCRIP,SAVE_IN_FROM
	CHARACTER*(INPUT_LENGTH) SAVE_IN_DESCRIP,SAVE_IN_FROM

	CHARACTER*(*) BUFFER

	COMMON /LAST_BUFFER/ OLD_BUFFER
	CHARACTER*(INPUT_LENGTH) OLD_BUFFER

	COMMON /OLD_BUFFER/ OLD_BUFFER_FROM,OLD_BUFFER_SUBJ

	COMMON /DATE/ DATE_LINE
	CHARACTER*(INPUT_LENGTH) DATE_LINE

	CHARACTER*24 TODAY

	COMMON /STORED/ STORED
	DATA STORED /.FALSE./ 

	LEN_BUFFER = TRIM(BUFFER)

	IF (LEN_FROM.EQ.0) THEN
	   WRITE (3,'(A)') BUFFER(:MIN(LEN_BUFFER,LINE_LENGTH))
	   IF (LEN_BUFFER.GT.LINE_LENGTH) THEN
	      WRITE (3,'(A)') ' '//BUFFER(LINE_LENGTH+1:LEN_BUFFER)
	   END IF
	   IF (OLD_BUFFER_FROM.AND.(BUFFER(:1).EQ.' '.OR.
     &		BUFFER(:1).EQ.CHAR(9)).AND.LEN_BUFFER.GT.1) THEN
	      SAVE_IN_FROM = 
     &		SAVE_IN_FROM(:TRIM(SAVE_IN_FROM))//BUFFER(:LEN_BUFFER)
	      RETURN
	   ELSE IF (OLD_BUFFER_SUBJ.AND.(BUFFER(:1).EQ.' '.OR.
     &		BUFFER(:1).EQ.CHAR(9)).AND.LEN_BUFFER.GT.1) THEN
	      INDESCRIP = 
     &		INDESCRIP(:TRIM(INDESCRIP))//BUFFER(:LEN_BUFFER)
	      LDESCR = LDESCR + LEN_BUFFER
	      RETURN
	   ELSE IF (BUFFER(:5).EQ.'From:'.AND.SAVE_IN_FROM.EQ.' ') THEN
	      IF (LEN_BUFFER.GE.7) SAVE_IN_FROM = BUFFER(7:)
	      OLD_BUFFER_FROM = .TRUE.
	      OLD_BUFFER_SUBJ = .FALSE.
	      RETURN
	   ELSE IF (BUFFER(:9).EQ.'Subject: ') THEN
	      LDESCR = LEN_BUFFER - 9
	      INDESCRIP = BUFFER(10:)
	      OLD_BUFFER_SUBJ = .TRUE.
	      OLD_BUFFER_FROM = .FALSE.
	      RETURN
	   ELSE IF (BUFFER(:9).EQ.'Reply-To:') THEN
	      IF (LEN_BUFFER.GE.11) SAVE_IN_FROM = BUFFER(11:)
	      OLD_BUFFER_FROM = .TRUE.
	      OLD_BUFFER_SUBJ = .FALSE.
	      RETURN
	   ELSE IF (LEN_BUFFER.EQ.0) THEN
	      IF (SAVE_IN_FROM.EQ.' ') CALL GETUSER(SAVE_IN_FROM)
	      LEN_FROM = TRIM(SAVE_IN_FROM)
	      IF (LEN_FROM.GT.0) THEN
		 OLD_BUFFER_FROM = .FALSE.
		 INFROM = SAVE_IN_FROM
		 IF (.NOT.BTEST(FOLDER_FLAG,5)) THEN
		    CALL STORE_FROM(INFROM,LEN_FROM)
		 ELSE IF (INDEX(INFROM,'%"').GT.0) THEN
		    LPRO = INDEX(INFROM,'%"') + 1
		    PROTOCOL = INFROM(:LPRO)
		 END IF
		 IF (LDESCR.GT.0) THEN
		    LEN_DESCRP = LDESCR
	            CALL STORE_DESCRP(INDESCRIP,LEN_DESCRP)
		 ELSE
	            LEN_DESCRP = TRIM(SAVE_IN_DESCRIP)
	            IF (LEN_DESCRP.GT.0) THEN
	               INDESCRIP = SAVE_IN_DESCRIP
	               IF (.NOT.BTEST(FOLDER_FLAG,5)) THEN
	                  CALL STORE_DESCRP(INDESCRIP,LEN_DESCRP)
	               END IF
	            ELSE
		       INDESCRIP = ' '
		       DESCRIP = ' '
		    END IF
		 END IF
	         STORED = .TRUE.
	         IF (.NOT.BTEST(FOLDER_FLAG,5)) CALL WRITEOUT_STORED
	         STORED = .FALSE.
	      END IF
	   END IF
	   OLD_BUFFER_FROM = .FALSE.
	   OLD_BUFFER_SUBJ = .FALSE.
	   RETURN
	END IF
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
                 CLOSE (UNIT=3)
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
		    IF (LFROM.LE.0) THEN
		       LFROM = TRIM(SAVE_IN_FROM)
		       IF (LPRO.GT.0) THEN
		          LFROM = LFROM + LPRO + 1
	                  CALL STORE_FROM(PROTOCOL(:LPRO)//
     &			   SAVE_IN_FROM//'"',LFROM)
		       ELSE
	                  CALL STORE_FROM(SAVE_IN_FROM,LFROM)
		       END IF
		    ELSE IF (LPRO.GT.0) THEN
		       LFROM = LFROM + LPRO + 1
	               CALL STORE_FROM(PROTOCOL(:LPRO)//
     &			BUFFER(7:LEN_BUFFER)//'"',LFROM)
		    ELSE
	               CALL STORE_FROM(BUFFER(7:),LFROM)
		    END IF
		 END IF
		 RETURN
	      END IF
	   ELSE
	      IF (LEN_BUFFER.GT.0) THEN
		 IF (.NOT.TEXT) THEN
	            IF (.NOT.NEWS_FEED()) THEN
		       TEXT = .TRUE.
		    ELSE
	               CALL STRIP_HEADER(BUFFER,TRIM(BUFFER),IER)
		       TEXT = .NOT.IER
		    END IF
		 END IF
		 IF (TEXT) THEN
	            WRITE (3,'(A)') BUFFER(:MIN(LEN_BUFFER,LINE_LENGTH))
		 END IF
	      ELSE
		 IF (TEXT) WRITE (3,'(A)') ' '
	      END IF
	      RETURN
	   END IF
	END IF

	IF (LEN_BUFFER.EQ.0) THEN		! If empty line
	   IF (.NOT.STORED.AND.
     &	       ((TEXT.AND.NEWS_FEED()).OR.BTEST(FOLDER_FLAG,11))) THEN
	      WRITE (3,'(A)') ' '
	      CALL STRIP_HEADER(' ',-1,IER)
	   END IF
	   CALL STORE_BULL(1,' ',NBLOCK)
	ELSE
	   IF (LEN_DESCRP.EQ.0) THEN
	      IF (BUFFER(:9).EQ.'Subject: ') THEN
		 DESCRIP = BUFFER(INDEX(BUFFER,' ')+1:)
		 LEN_DESCRP = LEN_BUFFER
	      END IF
	   END IF
	   IF (.NOT.INEXDATE) THEN
	      IF (BUFFER(:9).EQ.'Expires: '.OR.
     &		  BUFFER(:11).EQ.'X-Expires: ') THEN
		 I = INDEX(BUFFER,' ')+1
		 NODATE = .FALSE.
		 DO J=I,LEN_BUFFER
		    IF (BUFFER(J:J).EQ.','.OR.BUFFER(J:J).EQ.'-') THEN
		       BUFFER(J:J) = ' '
		    END IF
		 END DO
	         CALL STR$UPCASE(BUFFER(I:),BUFFER(I:))
		 NODATE = .TRUE.
		 I = INDEX(BUFFER,' ')+1
		 EXDATE(3:3) = '-'
		 EXDATE(7:7) = '-'
		 DO WHILE (I.LE.LEN_BUFFER)
		    IF (BUFFER(I:I).GE.'0'.AND.BUFFER(I:I).LE.'9') THEN
		       IF (NODATE) THEN
			  IF (INDEX(BUFFER(I:),' ').EQ.2) THEN
			     EXDATE(1:2) = '0'//BUFFER(I:I)
			     I = I + 1
			  ELSE
			     EXDATE(1:2) = BUFFER(I:I+1)
			     I = I + 2
			  END IF
			  NODATE = .FALSE.
		       ELSE
			  IF (LEN_BUFFER-I.EQ.1.OR.
     &			     INDEX(BUFFER(I:),' ').EQ.3) THEN   ! No century?
			     IER = SYS$ASCTIM(,TODAY,,)	   ! Get today's date
			     YEAR = INDEX(TODAY(6:),'-')
			     EXDATE(8:) = TODAY(6+YEAR:7+YEAR)//BUFFER(I:I+1)
			     I = I + 2
			  ELSE
			     EXDATE(8:) = BUFFER(I:I+3)
			     I = I + 4
			  END IF
		       END IF
		    ELSE IF (BUFFER(I:I).GE.'A'.AND.BUFFER(I:I).LE.'Z') THEN
		       EXDATE(4:6) = BUFFER(I:I+2)
		       I = I + 3
		    ELSE
		       I = I + 1
		    END IF
		 END DO
		 INEXDATE = .TRUE.
	      END IF
	   END IF
	   CALL STORE_BULL(MIN(LEN_BUFFER,LINE_LENGTH),BUFFER,NBLOCK)
	   IF (NEWS_FEED().AND..NOT.TEXT) THEN
	      CALL STRIP_HEADER(BUFFER,TRIM(BUFFER),IER)
	      TEXT = .NOT.IER
	   ELSE
	      TEXT = .TRUE.
           END IF
	   IF (.NOT.STORED.AND.
     &	       ((TEXT.AND.NEWS_FEED()).OR.BTEST(FOLDER_FLAG,11))) THEN
	      WRITE (3,'(A)') BUFFER(:MIN(LEN_BUFFER,LINE_LENGTH))
	   END IF
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

	INCLUDE 'BULLUSER.INC'

	COMMON /DIGEST/ LDESCR,FIRST_BREAK

	COMMON /SCRTYPE/ SCRTYPE,SCRNAME
	CHARACTER*132 SCRNAME

	COMMON /TEXT_PRESENT/ TEXT

	COMMON /MAIN_HEADER_INFO/ INFROM,INDESCRIP,LEN_FROM,LEN_DESCRP
	COMMON /MAIN_HEADER_INFO/ INEXDATE
	CHARACTER*(INPUT_LENGTH) INFROM,INDESCRIP

	COMMON /SAVE_IN/ SAVE_IN_DESCRIP,SAVE_IN_FROM
	CHARACTER*(INPUT_LENGTH) SAVE_IN_DESCRIP,SAVE_IN_FROM

	CHARACTER*24 TODAY

	CHARACTER USER_SAVE*12,PROC_SAVE*12

	DIMENSION BIN_EXTIME(2)

	IF (TEXT.AND.BTEST(FOLDER_FLAG,5).AND.FIRST_BREAK) THEN
	   IF (LEN_FROM.GT.0) THEN
	      CALL STORE_FROM(INFROM,LEN_FROM)
	   ELSE
	      CALL GETUSER(FROM)
	      INFROM = FROM
	      LEN_FROM = TRIM(INFROM)
	   END IF
	   IF (LEN_DESCRP.GT.0) THEN
	      CALL STORE_DESCRP(INDESCRIP,LEN_DESCRP)
	   END IF
	   LDESCRP = 1
	   FIRST_BREAK = .FALSE.
	   CALL WRITEOUT_STORED
	   CLOSE (UNIT=3)
	ELSE IF (LEN_FROM.EQ.0) THEN
	   CALL GETUSER(FROM)
	   INFROM = FROM
	   LEN_FROM = TRIM(INFROM)
	   LEN_DESCRP = TRIM(SAVE_IN_DESCRIP)
	   IF (LEN_DESCRP.GT.0) THEN
	      INDESCRIP = SAVE_IN_DESCRIP
	      IF (.NOT.BTEST(FOLDER_FLAG,5)) THEN
	         CALL STORE_DESCRP(INDESCRIP,LEN_DESCRP)
	      END IF
	   ELSE
	      INDESCRIP = ' '
	      DESCRIP = ' '
	   END IF
	   CALL WRITEOUT_STORED
	END IF

	CALL FLUSH_BULL(NBLOCK)

	CALL CLOSE_BULLFIL			! Finished adding bulletin

	IF ((BTEST(FOLDER_FLAG,5).AND.LDESCR.EQ.0).OR.	! End of digest msg
     &				.NOT.TEXT) THEN	! or no message text found
	   CALL CLOSE_BULLDIR			! then don't add message entry
	   IF (SCRTYPE.EQ.0) THEN
	      CLOSE (UNIT=3)
              SCRTYPE = -1
	   END IF
	   RETURN
	END IF

	EXTIME = '00:00:00.00'
	IF (INEXDATE) THEN
	   IER = SYS_BINTIM(EXDATE//' '//EXTIME,BIN_EXTIME)
	   IF (IER) THEN			! If good date format
	      IER = SYS$ASCTIM(,TODAY,,)	! Get today's date
	      IER = COMPARE_DATE(EXDATE,TODAY(:11)) ! Compare date with today's
	      IF ((IER.GT.F_EXPIRE_LIMIT.AND.F_EXPIRE_LIMIT.GT.0) ! Too great?
     &		.OR.IER.LE.0) THEN		! or expiration date not future
	         INEXDATE = .FALSE.		! Don't use it
	      END IF
	   ELSE
	      INEXDATE = .FALSE.		! Don't use it
	   END IF
	END IF

	IF (.NOT.INEXDATE) THEN
	   IF (FOLDER_BBEXPIRE.EQ.-1) THEN	! Folder has expiration time?
	      EXDATE = '5-NOV-2100'		! no, so set date far in future
	      SYSTEM = 2			! indicate permanent message
	   ELSE					! Else set expiration date
	      CALL GET_EXDATE(EXDATE,FOLDER_BBEXPIRE)
	      SYSTEM = 0
	   END IF
	END IF

	LENGTH = NBLOCK - LENGTH + 1		! Number of records

	CALL ADD_ENTRY				! Add the new directory entry

	CALL CLOSE_BULLDIR			! Totally finished with add

	CALL UPDATE_FOLDER

	IF (SCRTYPE.EQ.0.AND.NEWS_FEED()) THEN
	   FOLDER1_DESCRIP = FOLDER_DESCRIP(INDEX(FOLDER_DESCRIP,'<')+1:)
	   FOLDER1_DESCRIP = FOLDER1_DESCRIP(:INDEX(FOLDER1_DESCRIP,'>')-1)
	   CALL GET_MSGKEY(MSG_BTIM,MSG_KEY)
      	   CALL NEWS_POST('ignore',.TRUE.,IER,INDESCRIP)
	   IF (.NOT.BTEST(FOLDER_FLAG,11)) CLOSE (UNIT=3)
	END IF

	IF (BTEST(FOLDER_FLAG,11).AND.SCRTYPE.EQ.0) THEN
	   IF (NEWS_FEED()) THEN
	      SLIST = INDEX(FOLDER_DESCRIP,'[')
	   ELSE
	      SLIST = INDEX(FOLDER_DESCRIP,'<')
	   END IF
	   IF (SLIST.GT.0) THEN
	      INPUT = FOLDER_DESCRIP(SLIST+1:)
	      IF (NEWS_FEED()) THEN
	         ILEN = INDEX(INPUT,']') - 1
	      ELSE
	         ILEN = INDEX(INPUT,'>') - 1
	      END IF
	      IF (ILEN.EQ.-1) ILEN = TRIM(INPUT)
	      INPUT = INPUT(:ILEN)
              CALL ADD_PROTOCOL(INPUT,ILEN)
	      CLOSE (UNIT=3,STATUS='SAVE')
	      CALL GETUSER(PROC_SAVE)
              USER_SAVE = USERNAME
	      USERNAME = FOLDER
	      IF (CONFIRM_USER(USERNAME).EQ.0) THEN
	        CALL SETUSER(USERNAME)
	      END IF
	      IF (SYS_TRNLNM('MX_NODE_NAME','DEFINED')) THEN
	         IER = LIB$SET_LOGICAL
     &			('MX_REPLY_TO',INFROM(:TRIM(INFROM)))
	      ELSE IF (SYS_TRNLNM('PMDF_ROOT','DEFINED')) THEN 
	         IER = LIB$SET_LOGICAL
     &			('PMDF_REPLY_TO',INFROM(:TRIM(INFROM)))
	      ELSE IF (FOLDER1_BBOARD(:4).NE.'NONE') THEN
		 USERNAME = FOLDER_BBOARD
	      END IF
	      IF (.NOT.BTEST(FOLDER_FLAG,15)) THEN 
	         CALL RESPOND_MAIL(SCRNAME,INPUT,
     &	   	    FOLDER(:TRIM(FOLDER))//' folder message: '//
     &	   	    INDESCRIP(:LEN_DESCRP),STATUS)
	      ELSE
	         CALL RESPOND_MAIL(SCRNAME,INPUT,
     &	   	    INDESCRIP(:LEN_DESCRP),STATUS)
	      END IF
              CALL LIB$DELETE_FILE(SCRNAME(:TRIM(SCRNAME))//';*')
	      CALL SETUSER(PROC_SAVE)
	      USERNAME = USER_SAVE
	   ELSE
	      CLOSE (UNIT=3)
	   END IF
	ELSE IF (SCRTYPE.EQ.0) THEN
	   CLOSE (UNIT=3)
	END IF

	CALL STRIP_HEADER(' ',-1,IER)

	SCRTYPE = -1

	RETURN
	END




	SUBROUTINE STORE_FROM(IFROM,LEN_INFROM)

	IMPLICIT INTEGER (A-Z)

	COMMON /MAIL_PROTOCOL/ PROTOCOL,LPRO
	CHARACTER*12 PROTOCOL

	INCLUDE 'BULLDIR.INC'

	CHARACTER*(*) IFROM

	CHARACTER*(INPUT_LENGTH) INFROM

	INFROM = IFROM

	IF (LPRO.GT.0) THEN			! Protocol present?
	   I = INDEX(INFROM,'%"') + 2		! Make usable for VMS MAIL
	   IF (I.EQ.2) THEN
	      INFROM = PROTOCOL(:LPRO)//INFROM(:LEN_INFROM)//'"'
	      I = LPRO + 1
	      LEN_INFROM = LEN_INFROM + LPRO + 1
	   END IF
	   DO WHILE (I.LT.LEN_INFROM)
	      IF (INFROM(I:I).EQ.'"') THEN
		 INFROM(I:I) = ''''
	      ELSE IF (INFROM(I:I).EQ.'\') THEN
		 INFROM(I+1:) = '\'//INFROM(I+1:)
		 LEN_INFROM = LEN_INFROM + 1
		 I = I + 1
	      ELSE IF (INFROM(I:I).EQ.''''.AND.
     &		       INDEX(INFROM,'@').GT.I) THEN
		 INFROM(I:) = '\s'//INFROM(I+1:)
		 LEN_INFROM = LEN_INFROM + 1
		 I = I + 2
	      END IF
	      I = I + 1
	   END DO
	END IF

	DO I=1,LEN_INFROM			! Remove control characters
	   IF (INFROM(I:I).LT.' ') INFROM(I:I) = ' '
	END DO

	DO WHILE (LEN_INFROM.GT.0.AND.INFROM(:1).EQ.' ')
	   INFROM = INFROM(2:)
	   LEN_INFROM = LEN_INFROM - 1
	END DO

	TWO_SPACE = INDEX(INFROM,'  ')
	DO WHILE (TWO_SPACE.GT.0.AND.TWO_SPACE.LT.LEN_INFROM)
	   INFROM = INFROM(:TWO_SPACE)//INFROM(TWO_SPACE+2:)
	   LEN_INFROM = LEN_INFROM - 1
	   TWO_SPACE = INDEX(INFROM,'  ')
	END DO

C	IF (.NOT.NEWS_FEED()) THEN
           CALL STORE_BULL(6+LEN_INFROM,'From: '//INFROM(:LEN_INFROM),
     &		NBLOCK)
C	END IF

	IF (INDEX(INFROM,'%"').GT.0)		! Strip off protocol program
     &		INFROM = INFROM(INDEX(INFROM,'%"')+2:)

	IF (INDEX(INFROM,'::').GT.0)		! Strip off node name
     &		INFROM = INFROM(INDEX(INFROM,'::')+2:)	! I.e. HOST::USER

	CALL GET_FROM(FROM,INFROM,LEN_INFROM)

	RETURN
	END


	SUBROUTINE GET_FROM(FROM,INFROM1,LEN_INFROM)

	IMPLICIT INTEGER (A-Z)

	CHARACTER*(*) INFROM1,FROM

	CHARACTER*256 INFROM

	INFROM = INFROM1

	DO WHILE (INDEX(INFROM,'!').GT.0.AND.	! Unix address go backwards.
     &		INDEX(INFROM,'!').LT.INDEX(INFROM,'@'))
	   INFROM = INFROM(INDEX(INFROM,'!')+1:)	! I.e. host!user
	END DO

	I = INDEX(INFROM,'<')
	IF (I.GT.0.AND.INDEX(INFROM(I+1:),'@').GT.0) THEN ! Name may be of form
	   INFROM = INFROM(INDEX(INFROM,'<')+1:)   ! personal-name <net-name>
	END IF

	I = INDEX(INFROM,'(')
	IF (I.GT.0.AND.INDEX(INFROM(I+1:),'@').GT.0) THEN ! Name may be of form
	   INFROM = INFROM(INDEX(INFROM,'(')+1:)   ! personal-name (net-name)
	END IF

	I = 1	! Trim username to start at first alpha character
	DO WHILE (I.LE.LEN_INFROM.AND.(INFROM(I:I).EQ.' '.OR.
     &		INFROM(I:I).EQ.'%'.OR.INFROM(I:I).EQ.'.'.OR.
     &		INFROM(I:I).EQ.'@'.OR.INFROM(I:I).EQ.'<'.OR.
     &		INFROM(I:I).EQ.'\'.OR.INFROM(I:I).LE.' '.OR.
     &		INFROM(I:I).GE.CHAR(127).OR.
     &		INFROM(I:I).EQ.'"'.OR.INFROM(I:I).EQ.''''))
	   I = I + 1
	END DO
	INFROM = INFROM(I:)
	J = LEN_INFROM - I + 1

	I = 1		! Trim username to end at a alpha character
	DO WHILE (I.LE.J.AND.INFROM(I:I).NE.' '.AND.
     &		INFROM(I:I).NE.'%'.AND.
     &		INFROM(I:I).NE.'@'.AND.INFROM(I:I).NE.'<'.AND.
     &		INFROM(I:I).NE.'\'.AND.INFROM(I:I).GT.' '.AND.
     &		INFROM(I:I).LT.CHAR(127).AND.
     &		INFROM(I:I).NE.'"'.AND.INFROM(I:I).NE.'''')
	   I = I + 1
	END DO
	FROM = INFROM(:I-1)

	DO J=2,TRIM(FROM)
	   IF ((FROM(J:J).GE.'A'.AND.FROM(J:J).LE.'Z').AND.
     &	       ((FROM(J-1:J-1).GE.'A'.AND.FROM(J-1:J-1).LE.'Z').OR.
     &	        (FROM(J-1:J-1).GE.'a'.AND.FROM(J-1:J-1).LE.'z'))) THEN
	      FROM(J:J) = CHAR(ICHAR(FROM(J:J))-ICHAR('A')+ICHAR('a'))
	   END IF
	END DO

	RETURN
	END




	SUBROUTINE STORE_DESCRP(INDESCRIP,LEN_DESCRP)

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLDIR.INC'

	CHARACTER*(*) INDESCRIP

	CALL CONVERT_TABS(INDESCRIP,LEN_DESCRP)

	DO I=1,LEN_DESCRP			! Remove control characters
	   IF (INDESCRIP(I:I).LT.' '.OR.ICHAR(INDESCRIP(I:I)).GT.126)
     &			INDESCRIP(I:I) = ' '
	END DO

	DO WHILE (LEN_DESCRP.GT.0.AND.INDESCRIP(:1).EQ.' ')
	   INDESCRIP = INDESCRIP(2:)
	   LEN_DESCRP = LEN_DESCRP - 1
	END DO

	IF (LEN_DESCRP.GT.LEN(DESCRIP)) THEN
C	IF (LEN_DESCRP.GT.LEN(DESCRIP).AND..NOT.NEWS_FEED()) THEN
				! Is length > allowable subject length?
	   CALL STORE_BULL(6+LEN_DESCRP,'Subj: '//
     &		INDESCRIP(:LEN_DESCRP),NBLOCK)
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
C	BUFFER	- Character string containing input line of message.
C	BLEN	- Length of character string.  If = 0, initialize subroutine.
C
C  OUTPUTS:
C	IER	- If true, line should be stripped.  Else, end of header.
C
	IMPLICIT INTEGER (A - Z)

	INCLUDE 'BULLDIR.INC'

	INCLUDE 'BULLFOLDER.INC'

	COMMON /DATE/ DATE_LINE
	CHARACTER*(INPUT_LENGTH) DATE_LINE

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	COMMON /NEWSGROUPS/ NEWSGROUPS
	CHARACTER*256 NEWSGROUPS

	COMMON /HEADER_QUEUE/ HEADER_Q,HEADER_Q1,NHEAD
	DATA HEADER_Q1/0/

	CHARACTER*(*) BUFFER

	IF (TRIM(BUFFER).EQ.0) THEN
			! If STRIP not set for folder or empty line
	   IER = .FALSE.
	   CONT_LINE = .FALSE.
	   LAST_NEWSGROUPS = .FALSE.
	   CALL INIT_QUEUE(HEADER_Q1,INPUT)
	   IF (BLEN.EQ.-1) THEN
	      CALL INIT_QUEUE(HEADER_Q1,INPUT)
	      HEADER_Q = HEADER_Q1
	      NHEAD = 0
	   END IF
	   RETURN
	END IF

	IF (BLEN.EQ.0) THEN
	   DATE_LINE = ' '
	   CONT_LINE = .FALSE.
	   LAST_NEWSGROUPS = .FALSE.
	END IF

	IER = .TRUE.

	IF (CONT_LINE.AND.(BUFFER(:1).EQ.' '.OR.   ! If line is continuation
     &		BUFFER(:1).EQ.CHAR(9))) THEN	   ! of previous header line
	   IF (LAST_NEWSGROUPS) THEN
	      NEWSGROUPS = NEWSGROUPS(:TRIM(NEWSGROUPS))//BUFFER(2:)
	   END IF
	   CALL WRITE_QUEUE(%VAL(HEADER_Q),HEADER_Q,BUFFER)
	   NHEAD = NHEAD + 1
	   RETURN
	END IF

	I = 1
	DO WHILE (I.LE.BLEN.AND.BUFFER(I:I).NE.' ')
	   IF (BUFFER(I:I).EQ.':') THEN	! Header line found
	      CONT_LINE = .TRUE.	! Next line might be continuation
	      LAST_NEWSGROUPS = .FALSE.
	      IF (REMOTE_SET.LT.3.AND.BUFFER(:5).EQ.'Date:') THEN
		 DATE_LINE = 'Message sent'//BUFFER(5:BLEN)
		 IF (DATE_LINE(TRIM(DATE_LINE):).NE.'.') THEN
		    DATE_LINE(TRIM(DATE_LINE)+1:) = '.'
	         END IF
	      ELSE IF (BUFFER(:11).EQ.'Newsgroups:') THEN
		 NEWSGROUPS = BUFFER(13:)
		 LAST_NEWSGROUPS = .TRUE.
	      END IF
	      CALL WRITE_QUEUE(%VAL(HEADER_Q),HEADER_Q,BUFFER)
	      NHEAD = NHEAD + 1
	      RETURN
	   ELSE
	      I = I + 1
	   END IF
	END DO

	IER = .FALSE.
	CONT_LINE = .FALSE.
	LAST_NEWSGROUPS = .FALSE.

	RETURN
	END




	SUBROUTINE SET_NEWS_FOLDER_DEFAULT(NOTIFY,READNEW,BRIEF)
C
C  SUBROUTINE SET_NEWS_FOLDER_DEFAULT
C
C  FUNCTION: Sets flag defaults for specified news group
C	Note: If NOTIFY READNEW and BRIEF = 0, it is either news 
C	group removal or SET SUBSCRIBE command.
C
	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	COMMON /COMMAND_LINE/ INCMD
	CHARACTER*256 INCMD

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	EXTERNAL CLI$_NEGATED

	ALL = .FALSE.
	DEFAULT = 1
        NODEFAULT = 0
	SUB = ABS(BRIEF)+ABS(NOTIFY)+ABS(READNEW).EQ.0

	IF (NOTIFY.EQ.1.AND.REMOTE_SET.EQ.3) THEN
	   WRITE (6,'('' ERROR: /NOTIFY is invalid with non-stored'',
     &		  '' news group.'')')
           RETURN
	END IF

	IF (INCMD(:3).EQ.'SET') THEN
	   IF (.NOT.SETPRV_PRIV()) THEN
	      WRITE (6,'(
     &           '' ERROR: Privileges needed for changing defaults.'')')
	      RETURN
	   END IF
	   ALL = CLI$PRESENT('ALL')
	   DEFAULT = CLI$PRESENT('DEFAULT')
	   NODEFAULT = CLI$PRESENT('NODEFAULT')
	   CALL OPEN_BULLNEWS_SHARED
	   CALL OPEN_BULLINF_SHARED
	   IF (CLI$PRESENT('NOPERMANENT').OR.CLI$PRESENT('PERMANENT').OR.
     &	     	(SUB.AND.(NODEFAULT.OR.CLI$PRESENT('NOPERMANENT')))) THEN
              DO WHILE (REC_LOCK(IER1))
                 READ (9,KEY='*PERM',IOSTAT=IER1) TEMP_USER,INF_REC
	      END DO
	      IF (IER1.NE.0) THEN
                 DO I=1,FOLDER_MAX
                    INF_REC(1,I) = 0
                    INF_REC(2,I) = 0
                 END DO
              END IF

              IF (CLI$PRESENT('PERMANENT')) THEN
                 CALL SET_NEWS_FLAG(IER,NOTIFY,READNEW,BRIEF)
		 IF (SUB) DEFAULT = 1
              ELSE IF (CLI$PRESENT('NOPERMANENT').OR.NODEFAULT) THEN
                 IF (NOTIFY.GE.0) CALL SET_NEWS_FLAG(IER,0,-1,-1)
                 IF (READNEW.GE.0.OR.BRIEF.GE.0)
     &	 		CALL SET_NEWS_FLAG(IER,-1,0,0)
		 IF (SUB)
     &			CALL SET_NEWS_FLAG(IER,-1,-1,-1)
              END IF
              IF (.NOT.IER) THEN
                 CALL CLOSE_BULLNEWS
                 CALL CLOSE_BULLINF
                 RETURN
              END IF
              IF (IER1.EQ.0) THEN
                 REWRITE (9,IOSTAT=IER) TEMP_USER,INF_REC
              ELSE
                 WRITE (9,IOSTAT=IER) '*PERM       ',INF_REC
              END IF
           END IF	
	ELSE
 	   CALL OPEN_BULLNEWS_SHARED
 	   CALL OPEN_BULLINF_SHARED
	END IF

        DO WHILE (REC_LOCK(IER1))
           READ (9,KEY='*DEFAULT',IOSTAT=IER1) TEMP_USER,INF_REC
	END DO
	IF (IER1.NE.0) THEN
	   DO I=1,FOLDER_MAX
	      INF_REC(1,I) = 0
	      INF_REC(2,I) = 0
	   END DO
	END IF
        IF (NODEFAULT.AND.SUB) THEN
	   NOTIFY = -1
	   READNEW = -1
	   BRIEF = -1
	END IF
  	IF (DEFAULT.OR.NODEFAULT) THEN
	   IF (NODEFAULT.AND..NOT.SUB) THEN
	      IF (NOTIFY.NE.-1) CALL SET_NEWS_FLAG(IER,0,-1,-1)
	      IF (READNEW.NE.-1.OR.BRIEF.NE.-1)
     &		CALL SET_NEWS_FLAG(IER,-1,0,0)
	   ELSE
	      CALL SET_NEWS_FLAG(IER,NOTIFY,READNEW,BRIEF)
	   END IF
           IF (.NOT.IER) THEN
              CALL CLOSE_BULLNEWS
              CALL CLOSE_BULLINF
              RETURN
           END IF
	   IF (IER1.EQ.0) THEN
	      REWRITE (9,IOSTAT=IER) TEMP_USER,INF_REC
	   ELSE
	      WRITE (9,IOSTAT=IER) '*DEFAULT    ',INF_REC
	   END IF
	END IF
	IF ((ALL.OR.(SUB.AND.INCMD(:3).NE.'SET')).AND.IER.EQ.0) THEN
	   CALL OPEN_BULLUSER_SHARED
	   CALL READ_USER_FILE_HEADER(IER)
	   CALL READ_USER_FILE(IER)
	   DO WHILE (IER.EQ.0)
	      IF (TEMP_USER(:1).NE.'*'.AND.TEMP_USER(:1).NE.':') THEN
	         LU = TRIM(TEMP_USER)
	         TEMP_USER(LU:LU) = CHAR(128.OR.ICHAR(TEMP_USER(LU:LU)))
	         IF (LU.GT.1) THEN
	            TEMP_USER(LU-1:LU-1) =
     &			CHAR(128.OR.ICHAR(TEMP_USER(LU-1:LU-1)))
	         ELSE
	            TEMP_USER(2:2) = CHAR(128.OR.ICHAR(TEMP_USER(2:2)))
	         END IF
	         DO WHILE (REC_LOCK(IER1))
                    READ (9,KEY=TEMP_USER,IOSTAT=IER1) TEMP_USER,INF_REC
	         END DO
       	         IF (IER1.NE.0) THEN
	            DO I=1,FOLDER_MAX
	               INF_REC(1,I) = 0
	               INF_REC(2,I) = 0
	            END DO
	         END IF
	         CALL SET_NEWS_FLAG(IER,NOTIFY,READNEW,BRIEF)
		 IF (IER1.EQ.0) THEN
		    REWRITE (9,IOSTAT=IER) TEMP_USER,INF_REC
		 ELSE
	            WRITE (9,IOSTAT=IER) TEMP_USER,INF_REC
		 END IF
	      END IF
	      CALL READ_USER_FILE(IER)
 	   END DO
	   CALL CLOSE_BULLUSER
	END IF

	CALL CLOSE_BULLNEWS
	CALL CLOSE_BULLINF

	RETURN
	END




	SUBROUTINE READ_INF_REC

	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLUSER.INC'

	COMMON /REMOTE_FOLDER/ REMOTE_SET,REMOTE_UNIT

	CALL OPEN_BULLINF_SHARED
        DO WHILE (REC_LOCK(IER1))
           READ (9,KEY='*PERM',IOSTAT=IER1) TEMP_USER,INF_REC
	END DO
	IF (IER1.NE.0) THEN
           DO I=1,FOLDER_MAX
              INF_REC(1,I) = 0
              INF_REC(2,I) = 0
           END DO
        END IF
	CALL CLOSE_BULLINF

	RETURN

	ENTRY SET_NEWS_FLAG(IER,NOTIFY,READNEW,BRIEF)

	I = 1
	DO WHILE (INF_REC2(1,I).NE.NEWS_FOLDER_NUMBER.AND.
     &		INF_REC2(1,I).NE.0.AND.I.LE.FOLDER_MAX-1)
	   I = I + 1
	END DO

	IF (I.GT.FOLDER_MAX-1.AND.TEMP_USER(:1).EQ.'*') THEN
	   WRITE (6,'('' ERROR: You have '',
     &		    '' reached the news folder limit of '',I,''.'')')
     &		    FOLDER_MAX-1
	   IER = 0
	   RETURN
	END IF

	IF (INF_REC2(1,I).EQ.NEWS_FOLDER_NUMBER) THEN
	   IF (NOTIFY.EQ.1) INF_REC2(2,I) = IBSET(INF_REC2(2,I),13)
	   IF (NOTIFY.EQ.0) INF_REC2(2,I) = IBCLR(INF_REC2(2,I),13)
	   IF (READNEW.EQ.1) INF_REC2(2,I) = IBSET(INF_REC2(2,I),14)
	   IF (READNEW.EQ.0) INF_REC2(2,I) = IBCLR(INF_REC2(2,I),14)
	   IF (BRIEF.EQ.1) INF_REC2(2,I) = IBSET(INF_REC2(2,I),15)
	   IF (BRIEF.EQ.0) INF_REC2(2,I) = IBCLR(INF_REC2(2,I),15)
  	   IF (NOTIFY+READNEW+BRIEF.EQ.-3) THEN
	      DO J=I,FOLDER_MAX-2
	         CALL COPY2(INF_REC(1,J),INF_REC(1,J+1))
              END DO
	   END IF
	   IER = 1
	   RETURN
	END IF

	IF (NOTIFY+READNEW+BRIEF.EQ.-3) RETURN

	DO J=I,1,-1
	   IF (J.GT.1) THEN
	      CALL READ_FOLDER_FILE_KEYNUM_TEMP(
     &	         ZEXT(INF_REC2(1,J-1)),IER)
	      IF (FOLDER_DESCRIP.LT.FOLDER1_DESCRIP) THEN
	         CALL COPY2(INF_REC(1,J),INF_REC(1,J-1))
	      END IF
	   END IF
	   IF (FOLDER_DESCRIP.GT.FOLDER1_DESCRIP.OR.J.EQ.1) THEN
	      INF_REC2(1,J) = NEWS_FOLDER_NUMBER
	      IF (F_START.LE.F_NBULL) THEN
		 INF_REC2(2,J) = MIN(8191,F_NBULL-(F_START-1))
		 INF_REC(2,J) = F_START - 1
	      ELSE
		 INF_REC2(2,J) = 0
		 INF_REC(2,J) = F_NBULL
	      END IF
	      IF (NOTIFY.EQ.1) INF_REC2(2,I) = IBSET(INF_REC2(2,I),13)
	      IF (NOTIFY.EQ.0) INF_REC2(2,I) = IBCLR(INF_REC2(2,I),13)
	      IF (READNEW.EQ.1) INF_REC2(2,I) = IBSET(INF_REC2(2,I),14)
	      IF (READNEW.EQ.0) INF_REC2(2,I) = IBCLR(INF_REC2(2,I),14)
	      IF (BRIEF.EQ.1) INF_REC2(2,I) = IBSET(INF_REC2(2,I),15)
	      IF (BRIEF.EQ.0) INF_REC2(2,I) = IBCLR(INF_REC2(2,I),15)
	      IER = 1
	      RETURN
	   END IF
	END DO

	RETURN
	END
