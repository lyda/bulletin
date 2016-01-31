$set nover
$copy sys$input BOARD_DIGEST.COM
$deck
$!
$! BOARD_DIGEST.COM
$!
$! Command file invoked by folder associated with a BBOARD which is
$! is specified with /SPECIAL.  It will convert "digest" mail and
$! split it into separate messages.  This type of mail is used in
$! certain Arpanet mailing lists, such as TEXHAX and INFO-MAC.
$!
$ FF[0,8] = 12                  ! Define a form feed character
$ SET PROTECT=(W:RWED)/DEFAULT
$ SET PROC/PRIV=SYSPRV
$ USER := 'F$GETJPI("","USERNAME")
$ EXTRACT_FILE = "BULL_DIR:" + "''USER'" + ".TXT"
$ DEFINE/USER EXTRACT_FILE BULL_DIR:'USER'
$ MAIL
READ
EXTRACT EXTRACT_FILE
DELETE
$ OPEN/READ INPUT 'EXTRACT_FILE'
$ OPEN/WRITE OUTPUT 'EXTRACT_FILE'
$ READ INPUT FROM_USER
$AGAIN:
$ READ/END=ERROR INPUT BUFFER
$ IF F$EXTRACT(0,3,BUFFER) .NES. "To:" THEN GOTO SKIP
$ USER = F$EXTRACT(4,F$LEN(BUFFER),BUFFER)
$ GOTO AGAIN1
$SKIP:
$ IF F$EXTRACT(0,15,BUFFER) .NES. "---------------" THEN GOTO AGAIN
$AGAIN1:
$ READ/END=ERROR INPUT BUFFER
$ IF F$EXTRACT(0,15,BUFFER) .NES. "---------------" THEN GOTO AGAIN1
$ FROM = " "
$ SUBJ = " "
$NEXT:
$ READ/END=EXIT INPUT BUFFER
$FROM:
$ IF F$EXTRACT(0,5,BUFFER) .NES. "From:" THEN GOTO SUBJECT
$ FROM = BUFFER 
$ GOTO NEXT
$SUBJECT:
$ IF F$EXTRACT(0,8,BUFFER) .NES. "Subject:" THEN GOTO NEXT
$ SUBJ = BUFFER - "Subject:"
$F2:
$ IF F$LENGTH(SUBJ) .EQ. 0 THEN GOTO WRITE
$ IF F$EXTRACT(0,1,SUBJ) .NES. " " THEN GOTO WRITE
$ SUBJ = F$EXTRACT(1,F$LENGTH(SUBJ),SUBJ)
$ GOTO F2
$WRITE:
$ WRITE OUTPUT FROM_USER
                                ! Write From: + TAB + USERNAME
$ WRITE OUTPUT "To:     " + USER
                                ! Write To: + TAB + BBOARDUSERNAME
$ WRITE OUTPUT "Subj:   " + SUBJ
                                ! Write Subject: + TAB + mail subject
$ WRITE OUTPUT ""               ! Write one blank line
$ IF FROM .NES. " " THEN WRITE OUTPUT FROM
$READ:
$ READ/END=EXIT/ERR=EXIT INPUT BUFFER
$ IF F$EXTRACT(0,15,BUFFER) .EQS. "---------------" THEN GOTO READ1
$ WRITE OUTPUT BUFFER
$ GOTO READ
$READ1:
$ READ/END=EXIT/ERR=EXIT INPUT BUFFER
$ IF F$LOCATE(":",BUFFER) .EQ. F$LENGTH(BUFFER) THEN GOTO READ1
$ WRITE OUTPUT FF
$ FROM = " "
$ SUBJ = " "
$ GOTO FROM
$EXIT:
$ CLOSE INPUT
$ CLOSE OUTPUT
$ PUR 'EXTRACT_FILE'
$ EXIT
$ERROR:
$ CLOSE INPUT
$ CLOSE OUTPUT
$ DELETE 'EXTRACT_FILE';
$eod 
$copy sys$input BOARD_SPECIAL.COM
$deck
$!
$! BOARD_SPECIAL.COM
$!
$! Command file invoked by folder associated with a BBOARD which is
$! is specified with /SPECIAL.  This can be used to convert data to
$! a message via a different means than the VMS mail.  This is done by
$! converting the data to look like output created by the MAIL utility,
$! which appears as follows:
$!
$!      First line is 0 length line.
$!      Second line is "From:" followed by TAB followed by incoming username
$!      Third line is "To:" followed by TAB followed by BBOARD username
$!      Fourth line is "Subj:" followed by TAB followed by subject
$!      The message text then follows.
$!      Message is ended by a line containing a FORM FEED.
$!
$! This command file should be put in the BBOARD_DIRECTORY as specified
$! in BULLFILES.INC.  You can also have several different types of special
$! procedures.  To accomplish this, rename the file to the BBOARD username.
$! i.e. if you specify SET BBOARD FOO/SPECIAL, you could name the file
$! FOO.COM and it will execute that rather than BOARD_SPECIAL.COM.
$!
$! The following routine is the one we use to convert mail from a non-DEC
$! mail network.  The output from this mail is written into a file which
$! is slightly different from the type outputted by MAIL.
$!
$! (NOTE: A username in the SET BBOARD command need only be specified if
$! the process which reads the mail requires that the process be owned by
$! a specific user, which is the case for this sample, and for that matter
$! when reading VMS MAIL.  If this is not required, you do not have to
$! specify a username.)
$!
$ USERNAME := 'F$GETJPI("","USERNAME")'         ! This trims trailing spaces
$ IF F$SEARCH("MFE_TELL_FILES:"+USERNAME+".MAI") .EQS. "" THEN EXIT
$ SET DEFAULT BULL_DIR: ! BULLETIN looks for text in BBOARD directory
$ SET PROTECT=(W:RWED)/DEFAULT
$ IF F$SEARCH("MFEMSG.MAI") .NES. "" THEN -
  DELETE MFEMSG.MAI;*           ! Delete any leftover output files.
$ MSG := $MFE_TELL: MESSAGE
$ DEFINE/USER SYS$COMMAND SYS$INPUT
$ MSG                           ! Read MFENET mail
copy * MFEMSG
delete *
exit
$ FF[0,8] = 12                  ! Define a form feed character
$ OPEN/READ/ERROR=EXIT INPUT MFEMSG.MAI
$ OUTNAME = USERNAME+".TXT"     ! Output file will be 'USERNAME'.TXT
$ OPEN/WRITE OUTPUT 'OUTNAME'
$ READ/END=END INPUT DATA               ! Skip first line in MSG output
$HEADER:
$ FROM = ""
$ SUBJ = ""
$ MFEMAIL = "T"
$NEXTHEADER:
$ IF (FROM.NES."") .AND. (SUBJ.NES."") THEN GOTO SKIPHEADER
$ READ/END=END INPUT DATA               ! Read header line in MSG output
$ IF DATA .EQS. "" THEN GOTO SKIPHEADER ! Missing From or Subj ??
$ IF FROM .NES. "" THEN GOTO SKIPFROM
$ IF F$LOCATE("From: ",DATA) .NES. 0 THEN GOTO 10$
$ MFEMAIL = "F"
$ FROM= F$EXTRACT(6,F$LENGTH(DATA),DATA)
$ GOTO NEXTHEADER
$10$:
$ IF F$LOCATE("Reply-to: ",DATA) .NES. 0 THEN GOTO 20$
$ MFEMAIL = "F"
$ FROM= F$EXTRACT(10,F$LENGTH(DATA),DATA)
$ GOTO NEXTHEADER
$20$:
$ IF F$LOCATE("From ",DATA) .NES. 0 THEN GOTO SKIPFROM
$ FROM= F$EXTRACT(5,F$LENGTH(DATA),DATA)
$ GOTO NEXTHEADER
$SKIPFROM:
$ IF SUBJ .NES. "" THEN GOTO SKIPSUBJ
$ IF F$LOCATE("Subject",DATA) .NES. 0 THEN GOTO SKIPSUBJ
$ SUBJ= F$EXTRACT(F$LOCATE(": ",DATA)+2,F$LENGTH(DATA),DATA)
$ GOTO NEXTHEADER
$SKIPSUBJ:
$ GOTO NEXTHEADER
$SKIPHEADER:
$ WRITE OUTPUT "From:   " + FROM
                                ! Write From: + TAB + USERNAME
$ WRITE OUTPUT "To:     " + USERNAME
                                ! Write To: + TAB + BBOARDUSERNAME
$ WRITE OUTPUT "Subj:   " + SUBJ
                                ! Write Subject: + TAB + mail subject
$ WRITE OUTPUT ""               ! Write one blank line
$ IF (DATA.EQS."") .OR. MFEMAIL THEN GOTO SKIPBLANKS
$50$:
$ READ/END=END INPUT DATA               ! Skip rest of main header
$ IF DATA .NES. "" THEN GOTO 50$
$60$:
$ READ/END=END INPUT DATA               ! Skip all of secondary header
$ IF DATA .NES. "" THEN GOTO 60$
$SKIPBLANKS:
$ READ/END=END INPUT DATA               ! Skip all blanks
$ IF DATA .EQS. "" THEN GOTO SKIPBLANKS
$NEXT:                          ! Read and write message text
$ WRITE OUTPUT DATA
$ IF DATA .EQS. FF THEN GOTO HEADER
                        ! Multiple messages are seperated by form feeds
$ READ/END=END INPUT DATA
$ GOTO NEXT
$END:
$ CLOSE INPUT
$ CLOSE OUTPUT
$ DELETE MFEMSG.MAI;
$EXIT:
$ EXIT
$eod 
$copy sys$input BULLCOM.CLD
$deck
!
! BULLCOM.CLD
!
! VERSION 5/26/89
!
        MODULE BULLETIN_SUBCOMMANDS

        DEFINE VERB ADD
                PARAMETER P1, LABEL=FILESPEC, VALUE(TYPE=$FILE)
                QUALIFIER ALL, NONNEGATABLE
                QUALIFIER BELL, NONNEGATABLE
                QUALIFIER BROADCAST, NONNEGATABLE
                DISALLOW NOT BROADCAST AND ALL
                DISALLOW NOT BROADCAST AND BELL
                QUALIFIER CLUSTER, DEFAULT
                QUALIFIER EDIT, NEGATABLE
                QUALIFIER EXPIRATION, NONNEGATABLE, VALUE
                QUALIFIER FOLDER, LABEL=SELECT_FOLDER, VALUE(REQUIRED,LIST)
                QUALIFIER NODES, LABEL=NODES, VALUE(REQUIRED,LIST)
                NONNEGATABLE
                QUALIFIER LOCAL, NONNEGATABLE
                DISALLOW LOCAL AND NOT BROADCAST
                DISALLOW NODES AND SELECT_FOLDER
                QUALIFIER NOINDENT, NONNEGATABLE
                DISALLOW NOINDENT AND NOT TEXT
                QUALIFIER PERMANENT, NONNEGATABLE
                QUALIFIER SHUTDOWN, NONNEGATABLE
                DISALLOW PERMANENT AND SHUTDOWN
                QUALIFIER SUBJECT, NONNEGATABLE, VALUE(REQUIRED)
                QUALIFIER SYSTEM, NONNEGATABLE
                QUALIFIER TEXT, NONNEGATABLE
                DISALLOW TEXT AND NOT EDIT
                DISALLOW TEXT AND FILESPEC
                QUALIFIER USERNAME, LABEL=USERNAME, VALUE(REQUIRED)
                NONNEGATABLE
        DEFINE VERB BACK
        DEFINE VERB CHANGE
                PARAMETER P1, LABEL=FILESPEC, VALUE(TYPE=$FILE)
                QUALIFIER EDIT, NEGATABLE
                QUALIFIER EXPIRATION, NONNEGATABLE, VALUE
                QUALIFIER GENERAL, NONNEGATABLE
                QUALIFIER HEADER, NONNEGATABLE
                QUALIFIER SUBJECT, NONNEGATABLE, VALUE(REQUIRED)
                QUALIFIER NEW,NONNEGATABLE
                QUALIFIER NUMBER, VALUE(TYPE=$NUMBER,REQUIRED)
                QUALIFIER PERMANENT, NONNEGATABLE
                QUALIFIER SHUTDOWN, NONNEGATABLE
                QUALIFIER SYSTEM,NONNEGATABLE
                QUALIFIER TEXT, NONNEGATABLE
                DISALLOW NEW AND NOT EDIT
                DISALLOW SYSTEM AND GENERAL
                DISALLOW PERMANENT AND SHUTDOWN
                DISALLOW PERMANENT AND EXPIRATION
                DISALLOW SHUTDOWN AND EXPIRATION
                DISALLOW SUBJECT AND HEADER
        DEFINE VERB COPY
                PARAMETER P1, LABEL=FOLDER, PROMPT="Folder"
                        VALUE(REQUIRED)
                PARAMETER P2, LABEL=BULLETIN_NUMBER, VALUE(TYPE=$FILE)
                QUALIFIER ALL
                QUALIFIER MERGE
                QUALIFIER ORIGINAL
                DISALLOW ALL AND BULLETIN_NUMBER
        DEFINE VERB CREATE
                QUALIFIER BRIEF, NONNEGATABLE
                QUALIFIER DESCRIPTION, NONNEGATABLE, VALUE(REQUIRED)
!
! Make the following qualifier DEFAULT if you want CREATE to be
! a privileged command.  NOTE: Make sure that BULL_DIR:BULLUSER.DAT
! has the following protection:  (RWED,RWED,,)
!
                QUALIFIER NEEDPRIV, NONNEGATABLE
                QUALIFIER NODE, NONNEGATABLE, VALUE(REQUIRED)
                QUALIFIER NOTIFY, NONNEGATABLE
                QUALIFIER OWNER, NONNEGATABLE, VALUE(REQUIRED)
                QUALIFIER PRIVATE, NONNEGATABLE
                QUALIFIER READNEW, NONNEGATABLE
                QUALIFIER REMOTENAME, NONNEGATABLE, VALUE(REQUIRED)
                QUALIFIER SEMIPRIVATE, NONNEGATABLE
                QUALIFIER SHOWNEW, NONNEGATABLE
                QUALIFIER SYSTEM, NONNEGATABLE
                PARAMETER P1, LABEL=CREATE_FOLDER, PROMPT="Folder"
                        VALUE(REQUIRED)
                DISALLOW PRIVATE AND SEMIPRIVATE
                DISALLOW BRIEF AND READNEW
                DISALLOW SHOWNEW AND READNEW
                DISALLOW BRIEF AND SHOWNEW
                DISALLOW NODE AND (NOTIFY OR PRIVATE OR SEMIPRIVATE)
        DEFINE VERB CURRENT
                QUALIFIER EDIT
        DEFINE VERB DELETE
                PARAMETER P1, LABEL=BULLETIN_NUMBER, VALUE(TYPE=$FILE)
                QUALIFIER ALL
                QUALIFIER IMMEDIATE,NONNEGATABLE
                QUALIFIER FOLDER, LABEL=SELECT_FOLDER, VALUE(REQUIRED,LIST)
                QUALIFIER NODES, LABEL=NODES, VALUE(REQUIRED,LIST)
                QUALIFIER USERNAME, LABEL=USERNAME, VALUE(REQUIRED)
                QUALIFIER SUBJECT, VALUE(REQUIRED)
                DISALLOW NOT SUBJECT AND (NODES OR SELECT_FOLDER)
                DISALLOW NODES AND SELECT_FOLDER
        DEFINE VERB DIRECTORY
                PARAMETER P1, LABEL=SELECT_FOLDER
                QUALIFIER FOLDER, SYNTAX=DIRECTORY_FOLDER, NONNEGATABLE
                QUALIFIER NEW
                QUALIFIER START, VALUE(REQUIRED,TYPE=$NUMBER), NONNEGATABLE
                QUALIFIER SINCE,VALUE(DEFAULT="TODAY",TYPE=$DATETIME)
                QUALIFIER MARKED, NONNEGATABLE
                DISALLOW (NEW AND SINCE) OR (START AND NEW) OR (START AND SINCE)
        DEFINE SYNTAX DIRECTORY_FOLDER
                QUALIFIER DESCRIBE
                QUALIFIER FOLDER, DEFAULT
        DEFINE VERB E                           ! EXIT command.
        DEFINE VERB EX                          ! EXIT command.
        DEFINE VERB EXIT                        ! EXIT command.
        DEFINE VERB EXTRACT
                PARAMETER P1, LABEL=FILESPEC, VALUE(TYPE=$FILE,REQUIRED),
                        PROMPT="File"
                PARAMETER P2, LABEL=BULLETIN_NUMBER, VALUE(TYPE=$FILE)
                QUALIFIER ALL
                QUALIFIER HEADER, DEFAULT
                QUALIFIER NEW, NONNEGATABLE
                DISALLOW ALL AND BULLETIN_NUMBER
        DEFINE VERB FILE
                PARAMETER P1, LABEL=FILESPEC, VALUE(TYPE=$FILE,REQUIRED),
                        PROMPT="File"
                PARAMETER P2, LABEL=BULLETIN_NUMBER, VALUE(TYPE=$FILE)
                QUALIFIER ALL
                QUALIFIER HEADER, DEFAULT
                QUALIFIER NEW, NONNEGATABLE
                DISALLOW ALL AND BULLETIN_NUMBER
        DEFINE VERB HELP
                PARAMETER P1, LABEL=HELP_FOLDER, VALUE(TYPE=$REST_OF_LINE)
        DEFINE VERB INDEX
                PARAMETER P1, LABEL=SELECT_FOLDER
                QUALIFIER MARKED
                QUALIFIER FOLDER, SYNTAX=DIRECTORY_FOLDER, NONNEGATABLE
                QUALIFIER NEW
                QUALIFIER RESTART
                QUALIFIER START, VALUE(REQUIRED,TYPE=$NUMBER), NONNEGATABLE
                QUALIFIER SINCE,VALUE(DEFAULT="TODAY",TYPE=$DATETIME)
                DISALLOW (NEW AND SINCE) OR (START AND NEW) OR (START AND SINCE)
        DEFINE VERB LAST
        DEFINE VERB MAIL
                PARAMETER P1, LABEL=RECIPIENTS, PROMPT="Recipients"
                VALUE(REQUIRED,IMPCAT,LIST)
                QUALIFIER HEADER, DEFAULT
                QUALIFIER SUBJECT, VALUE(REQUIRED)
        DEFINE VERB MODIFY
                QUALIFIER DESCRIPTION
                QUALIFIER NAME, VALUE(REQUIRED)
                QUALIFIER OWNER, VALUE(REQUIRED)
        DEFINE VERB MOVE
                PARAMETER P1, LABEL=FOLDER, PROMPT="Folder"
                        VALUE(REQUIRED)
                PARAMETER P2, LABEL=BULLETIN_NUMBER, VALUE(TYPE=$FILE)
                QUALIFIER ALL
                QUALIFIER MERGE
                QUALIFIER NODES
                QUALIFIER ORIGINAL
                QUALIFIER IMMEDIATE,NONNEGATABLE,DEFAULT
                DISALLOW ALL AND BULLETIN_NUMBER
                DISALLOW FOLDER AND NODES
        DEFINE VERB NEXT
        DEFINE VERB POST
                QUALIFIER CC, VALUE(LIST,REQUIRED)
                QUALIFIER LIST, DEFAULT
                QUALIFIER SUBJECT, VALUE(REQUIRED)
                QUALIFIER NOINDENT, NONNEGATABLE
                DISALLOW NOINDENT AND NOT TEXT
                QUALIFIER TEXT
                QUALIFIER EDIT
                DISALLOW TEXT AND NOT EDIT
        DEFINE VERB PRINT
                PARAMETER P1, LABEL=BULLETIN_NUMBER, VALUE(TYPE=$FILE)
                QUALIFIER HEADER, DEFAULT
                QUALIFIER NOTIFY, DEFAULT
                QUALIFIER QUEUE, VALUE(DEFAULT=SYS$PRINT), NONNEGATABLE
                QUALIFIER FORM, VALUE, NONNEGATABLE
                QUALIFIER ALL
                DISALLOW ALL AND BULLETIN_NUMBER
        DEFINE VERB QUIT
        DEFINE VERB READ
                PARAMETER P1, LABEL=BULLETIN_NUMBER, VALUE(TYPE=$NUMBER)
                QUALIFIER EDIT
                QUALIFIER MARKED, NONNEGATABLE
                QUALIFIER NEW
                QUALIFIER PAGE, DEFAULT
                QUALIFIER SINCE,VALUE(DEFAULT="TODAY",TYPE=$DATETIME)
                DISALLOW NEW AND SINCE
        DEFINE VERB REPLY
                PARAMETER P1, LABEL=FILESPEC, VALUE(TYPE=$FILE)
                QUALIFIER ALL, NONNEGATABLE
                QUALIFIER BELL, NONNEGATABLE
                QUALIFIER BROADCAST, NONNEGATABLE
                DISALLOW NOT BROADCAST AND ALL
                DISALLOW NOT BROADCAST AND BELL
                QUALIFIER CLUSTER, DEFAULT
                QUALIFIER EDIT, NEGATABLE
                QUALIFIER EXPIRATION, NONNEGATABLE, VALUE
                QUALIFIER FOLDER, LABEL=SELECT_FOLDER, VALUE(REQUIRED,LIST)
                QUALIFIER NODES, LABEL=NODES, VALUE(REQUIRED,LIST)
                NONNEGATABLE
                QUALIFIER LOCAL
                DISALLOW LOCAL AND NOT BROADCAST
                DISALLOW NODES AND SELECT_FOLDER
                QUALIFIER NOINDENT, NONNEGATABLE
                DISALLOW NOINDENT AND NOT TEXT
                QUALIFIER PERMANENT, NONNEGATABLE
                QUALIFIER SHUTDOWN, NONNEGATABLE
                DISALLOW PERMANENT AND SHUTDOWN
                QUALIFIER SUBJECT, NONNEGATABLE, VALUE(REQUIRED)
                QUALIFIER SYSTEM, NONNEGATABLE
                QUALIFIER TEXT, NONNEGATABLE
                DISALLOW TEXT AND NOT EDIT
                DISALLOW TEXT AND FILESPEC
                QUALIFIER USERNAME, LABEL=USERNAME, VALUE(REQUIRED)
                NONNEGATABLE
        DEFINE VERB REMOVE
                PARAMETER P1, LABEL=REMOVE_FOLDER, PROMPT="Folder"
                        VALUE(REQUIRED)
        DEFINE VERB RESPOND
                QUALIFIER CC, VALUE(LIST,REQUIRED)
                QUALIFIER LIST
                QUALIFIER SUBJECT, VALUE(REQUIRED)
                QUALIFIER NOINDENT, NONNEGATABLE
                DISALLOW NOINDENT AND NOT TEXT
                QUALIFIER TEXT
                QUALIFIER EDIT
                DISALLOW TEXT AND NOT EDIT
        DEFINE VERB SEARCH
                PARAMETER P1, LABEL=SEARCH_STRING
                QUALIFIER START, VALUE(TYPE=$NUMBER,REQUIRED)
                QUALIFIER SUBJECT
        DEFINE VERB SELECT
                PARAMETER P1, LABEL=SELECT_FOLDER
                QUALIFIER MARKED, NONNEGATABLE
        DEFINE VERB SET
                PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SET_OPTIONS)
                QUALIFIER ID
        DEFINE TYPE SET_OPTIONS
                KEYWORD NODE, SYNTAX=SET_NODE
                KEYWORD NONODE, SYNTAX = SET_NONODE
                KEYWORD EXPIRE_LIMIT, SYNTAX=SET_EXPIRE
                KEYWORD NOEXPIRE_LIMIT
                KEYWORD GENERIC, SYNTAX=SET_GENERIC
                KEYWORD NOGENERIC, SYNTAX=SET_GENERIC
                KEYWORD LOGIN, SYNTAX=SET_LOGIN
                KEYWORD NOLOGIN, SYNTAX=SET_LOGIN
                KEYWORD NOBBOARD
                KEYWORD BBOARD, SYNTAX=SET_BBOARD
                KEYWORD NOBRIEF, SYNTAX=SET_NOFLAGS
                KEYWORD BRIEF, SYNTAX=SET_FLAGS
                KEYWORD NOSHOWNEW, SYNTAX=SET_NOFLAGS
                KEYWORD SHOWNEW, SYNTAX=SET_FLAGS
                KEYWORD NOREADNEW, SYNTAX=SET_NOFLAGS
                KEYWORD READNEW, SYNTAX=SET_FLAGS
                KEYWORD ACCESS, SYNTAX=SET_ACCESS
                KEYWORD NOACCESS, SYNTAX=SET_NOACCESS
                KEYWORD FOLDER, SYNTAX=SET_FOLDER
                KEYWORD NOTIFY, SYNTAX=SET_FLAGS
                KEYWORD NONOTIFY, SYNTAX=SET_NOFLAGS
                KEYWORD PRIVILEGES, SYNTAX=SET_PRIVILEGES
                KEYWORD DUMP
                KEYWORD NODUMP
                KEYWORD PAGE
                KEYWORD NOPAGE
                KEYWORD SYSTEM
                KEYWORD NOSYSTEM
                KEYWORD KEYPAD
                KEYWORD NOKEYPAD
                KEYWORD PROMPT_EXPIRE
                KEYWORD NOPROMPT_EXPIRE
                KEYWORD DEFAULT_EXPIRE, SYNTAX=SET_DEFAULT_EXPIRE
                KEYWORD STRIP
                KEYWORD NOSTRIP
                KEYWORD DIGEST
                KEYWORD NODIGEST
        DEFINE SYNTAX SET_NODE
                PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SET_OPTIONS)
                PARAMETER P2, LABEL=NODENAME, VALUE(REQUIRED)
                PARAMETER P3, LABEL=REMOTENAME
                QUALIFIER FOLDER, VALUE(REQUIRED)
        DEFINE SYNTAX SET_NONODE
                QUALIFIER FOLDER, VALUE(REQUIRED)
        DEFINE SYNTAX SET_EXPIRE
                PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SET_OPTIONS)
                PARAMETER P2, LABEL=EXPIRATION, VALUE(TYPE=$NUMBER,REQUIRED)
        DEFINE SYNTAX SET_GENERIC
                PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SET_OPTIONS)
                PARAMETER P2, LABEL=USERNAME, VALUE(REQUIRED)
                QUALIFIER DAYS,VALUE(TYPE=$NUMBER,DEFAULT="7"),DEFAULT
        DEFINE SYNTAX SET_LOGIN
                PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SET_OPTIONS)
                PARAMETER P2, LABEL=USERNAME, VALUE(REQUIRED)
        DEFINE SYNTAX SET_FLAGS
                PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SET_OPTIONS)
                QUALIFIER DEFAULT, NONNEGATABLE
                QUALIFIER ALL, NONNEGATABLE
                QUALIFIER CLUSTER, DEFAULT
                QUALIFIER FOLDER, VALUE(REQUIRED)
                DISALLOW NOT ALL AND NOT DEFAULT AND CLUSTER
                DISALLOW ALL AND DEFAULT
        DEFINE SYNTAX SET_NOFLAGS
                PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SET_OPTIONS)
                QUALIFIER DEFAULT, NONNEGATABLE
                QUALIFIER ALL, NONNEGATABLE
                QUALIFIER FOLDER, VALUE(REQUIRED)
                DISALLOW ALL AND DEFAULT
        DEFINE SYNTAX SET_BBOARD
                PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SET_OPTIONS)
                PARAMETER P2, LABEL=BB_USERNAME
                QUALIFIER EXPIRATION, VALUE(TYPE=$NUMBER)
                        LABEL=EXPIRATION, DEFAULT
                QUALIFIER SPECIAL, NONNEGATABLE
                QUALIFIER VMSMAIL, NONNEGATABLE
                DISALLOW VMSMAIL AND NOT SPECIAL
                DISALLOW VMSMAIL AND NOT BB_USERNAME
        DEFINE SYNTAX SET_FOLDER
                PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SET_OPTIONS)
                PARAMETER P2, LABEL=SELECT_FOLDER
                QUALIFIER MARKED, NONNEGATABLE
        DEFINE SYNTAX SET_NOACCESS
                PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SET_OPTIONS)
                PARAMETER P2, LABEL=ACCESS_ID, VALUE(LIST)
                PARAMETER P3, LABEL=ACCESS_FOLDER
                QUALIFIER ALL, NONNEGATABLE
                QUALIFIER READONLY, NONNEGATABLE
                DISALLOW NOT ALL AND NOT ACCESS_ID
                DISALLOW ALL AND NOT READONLY
        DEFINE SYNTAX SET_ACCESS
                PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SET_OPTIONS)
                PARAMETER P2, LABEL=ACCESS_ID, VALUE(LIST)
                PARAMETER P3, LABEL=ACCESS_FOLDER
                QUALIFIER READONLY, NONNEGATABLE
                QUALIFIER ALL, NONNEGATABLE
                DISALLOW NOT ALL AND NOT ACCESS_ID
        DEFINE SYNTAX SET_PRIVILEGES
                PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SET_OPTIONS)
                PARAMETER P2, LABEL=PRIVILEGES, PROMPT="Privileges"
                VALUE (REQUIRED,LIST)
        DEFINE SYNTAX SET_DEFAULT_EXPIRE
                PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SET_OPTIONS)
                PARAMETER P2, LABEL=DEFAULT_EXPIRE, VALUE(TYPE=$NUMBER,REQUIRED)
        DEFINE VERB SHOW
                PARAMETER P1, LABEL=SHOW_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SHOW_OPTIONS)
!
! The following are defined to allow qualifiers to be specified
! directly after the SHOW command, i.e. SHOW/FULL FOLDER.
! Otherwise, the CLI routines will reject the command, because it
! first attempts to process the qualifier before process the parameter,
! so it has no information the qualifiers are valid.
!
                QUALIFIER FULL, SYNTAX=SHOW_FOLDER_FULL, NONNEGATABLE
                QUALIFIER ALL, SYNTAX=SHOW_USER
                QUALIFIER LOGIN, SYNTAX=SHOW_USER
                QUALIFIER NOLOGIN, SYNTAX=SHOW_USER
                QUALIFIER PRINT, SYNTAX=SHOW_KEYPAD_PRINT
        DEFINE TYPE SHOW_OPTIONS
                KEYWORD FOLDER, SYNTAX=SHOW_FOLDER
                KEYWORD NEW, SYNTAX=SHOW_FLAGS
                KEYWORD PRIVILEGES, SYNTAX=SHOW_FLAGS
                KEYWORD FLAGS, SYNTAX=SHOW_FLAGS
                KEYWORD KEYPAD, SYNTAX=SHOW_KEYPAD
                KEYWORD USER, SYNTAX=SHOW_USER
                KEYWORD VERSION
        DEFINE SYNTAX SHOW_FLAGS
                PARAMETER P1, LABEL=SHOW_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SHOW_OPTIONS)
        DEFINE SYNTAX SHOW_KEYPAD
                PARAMETER P1, LABEL=SHOW_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SHOW_OPTIONS)
                QUALIFIER PRINT
        DEFINE SYNTAX SHOW_KEYPAD_PRINT
                PARAMETER P1, LABEL=SHOW_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SHOW_OPTIONS)
                QUALIFIER PRINT,DEFAULT
        DEFINE SYNTAX SHOW_FOLDER
                PARAMETER P1, LABEL=SHOW_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SHOW_OPTIONS)
                PARAMETER P2, LABEL=SHOW_FOLDER
        DEFINE SYNTAX SHOW_USER
                PARAMETER P1, LABEL=SHOW_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SHOW_OPTIONS)
                PARAMETER P2, LABEL=USERNAME
                QUALIFIER ALL
                QUALIFIER LOGIN
                QUALIFIER NOLOGIN
                DISALLOW (NOLOGIN OR LOGIN OR ALL) AND USERNAME
                DISALLOW (LOGIN AND NOLOGIN)
        DEFINE SYNTAX SHOW_FOLDER_FULL
                QUALIFIER FULL, DEFAULT
                PARAMETER P1, LABEL=SHOW_PARAM1, PROMPT="What"
                        VALUE(REQUIRED, TYPE=SHOW_OPTIONS)
                PARAMETER P2, LABEL=SHOW_FOLDER
        DEFINE VERB MARK
                PARAMETER P1, LABEL=NUMBER, VALUE(LIST,TYPE=$NUMBER)
        DEFINE VERB SPAWN
                PARAMETER P1, LABEL=COMMAND, VALUE(TYPE=$REST_OF_LINE)
        DEFINE VERB UNMARK
                PARAMETER P1, LABEL=NUMBER, VALUE(LIST,TYPE=$NUMBER)
        DEFINE VERB UNDELETE
                PARAMETER P1, LABEL=BULLETIN_NUMBER, VALUE(TYPE=$FILE)
$eod 
$copy sys$input BULLETIN.CLD
$deck
!
!  This file is the CLD file used to define a command to execute
!  BULLETIN by using CDU, which adds the command  to the command table.
!  The alternative is to define a symbol to execute BULLETIN.
!  Either way will work, and it is up to the user's to decide which
!  method to work.  (If you don't know which, you probably should use
!  the default symbol method.)
!

Define Verb BULLETIN
  Image BULL_DIR:BULLETIN
  Parameter P1, Label = SELECT_FOLDER
  Qualifier BBOARD
  Qualifier BULLCP
  Qualifier CLEANUP, Value (Required)
  Qualifier EDIT
  Qualifier KEYPAD
  Qualifier LOGIN
  Qualifier MARKED
  Qualifier PAGE, Default
  Qualifier PROMPT, Value (Default = "BULLETIN"), Default
  Qualifier READNEW
  Qualifier REVERSE
  !
  ! The following line causes a line to be outputted separating system notices.
  ! The line consists of a line of all "-"s, i.e.:
  !--------------------------------------------------------------------------
  ! If you want a different character to be used, simply put in the desired one
  ! in the following line.  If you want to disable the feature, remove the
  ! Default at the end of the line.  (Don't remove the whole line!)
  !
  Qualifier SEPARATE, Value (Default = "-"), Default
  Qualifier STARTUP
  Qualifier STOP
  Qualifier SYSTEM, Value (Type = $NUMBER, Default = "7")
$eod 
$copy sys$input BULLETIN.COM
$deck
$ DEFINE SYS$INPUT SYS$NET
$ BULLETIN
$eod 
$copy sys$input BULLMAIN.CLD
$deck
        MODULE BULLETIN_MAINCOMMANDS
        DEFINE VERB BULLETIN
                PARAMETER P1, LABEL=SELECT_FOLDER
                QUALIFIER BBOARD
                QUALIFIER BULLCP
                QUALIFIER CLEANUP, LABEL=CLEANUP, VALUE(REQUIRED)
                QUALIFIER EDIT
                QUALIFIER KEYPAD
                QUALIFIER LOGIN
                QUALIFIER MARKED
                QUALIFIER PAGE, DEFAULT
                QUALIFIER READNEW
                QUALIFIER REVERSE
!
! The following line causes a line to be outputted separating system notices.
! The line consists of a line of all "-"s, i.e.:
!--------------------------------------------------------------------------
! If you want a different character to be used, simply put in the desired one
! in the following line.  If you want to disable the feature, remove the
! DEFAULT at the end of the line.  (Don't remove the whole line!)
!
                QUALIFIER SEPARATE, VALUE(DEFAULT="-"), DEFAULT
                QUALIFIER STARTUP
                QUALIFIER STOP
                QUALIFIER SYSTEM, VALUE(TYPE=$NUMBER,DEFAULT="7")
$eod 
$copy sys$input BULLSTART.COM
$deck
$ RUN SYS$SYSTEM:INSTALL
BULL_DIR:BULLETIN/SHAR/OPEN/HEAD/-
PRIV=(OPER,SYSPRV,CMKRNL,WORLD,DETACH,PRMMBX,SYSNAM)
/EXIT
$ BULL*ETIN :== $BULL_DIR:BULLETIN
$ BULLETIN/STARTUP
$eod 
$copy sys$input CREATE.COM
$deck
$ FORTRAN/EXTEND BULLETIN
$ FORTRAN/EXTEND BULLETIN0
$ FORTRAN/EXTEND BULLETIN1
$ FORTRAN/EXTEND BULLETIN2
$ FORTRAN/EXTEND BULLETIN3
$ FORTRAN/EXTEND BULLETIN4
$ FORTRAN/EXTEND BULLETIN5
$ FORTRAN/EXTEND BULLETIN6
$ FORTRAN/EXTEND BULLETIN7
$ FORTRAN/EXTEND BULLETIN8
$ FORTRAN/EXTEND BULLETIN9
$ MAC ALLMACS
$ SET COMMAND/OBJ BULLCOM
$ SET COMMAND/OBJ BULLMAIN
$ IF F$SEARCH("BULL.OLB") .NES. "" THEN DELETE BULL.OLB;
$ IF F$SEARCH("BULL.OLB") .EQS. "" THEN LIB/CREATE BULL
$ LIB BULL *.OBJ;
$ DELETE *.OBJ;*
$ @BULLETIN.LNK
$eod 
$copy sys$input DCLREMOTE.COM
$deck
$! DCL procedure to execute DCL commands passed over Decnet on a remote system.
$! Commands sent by the command procedure REMOTE.COM on the local system are
$! are received by this procedure on the remote node.
$! This procedure is usually a DECNET OBJECT with task name DCLREMOTE and
$! normally resides in the default DECNET account.  To install as an object,
$! enter NCP, and then use the command:
$!              NCP> SET OBJECT DCLREMOTE FILE file-spec NUM 0
$! where file-spec includes the disk, directory, and file name of the file.
$! If DCLREMOTE is not installed as an object, the logical name DCLREMOTE can
$! be defined to point at it.  
$!
$! Alternativley, DCLREMOTE.COM could be placed in the directory of the user's
$! proxy login on the remote system.
$!
$! WARNING: An EXIT command must not be passed as a command to execute at this
$! procedure level or the link will hang.
$!
$ SET NOON
$ N = 0
$AGAIN:
$ N = N + 1
$ IF N .GE. 5 THEN GOTO DONE
$ OPEN/WRITE/READ/ERR=AGAIN NET SYS$NET
$ DEFINE /NOLOG SYS$OUTPUT NET
$ DEFINE /NOLOG SYS$ERROR NET
$NEXT_CMD:
$  READ /ERR=DONE NET COMMAND
$  'COMMAND'
$  WRITE/ERR=DONE SYS$OUTPUT "COMMAND$DONE ''$STATUS'"
$  GOTO NEXT_CMD
$DONE:
$ CLOSE NET
$eod 
$copy sys$input INSTALL.COM
$deck
$ COPY BULLETIN.EXE BULL_DIR:
$ RUN SYS$SYSTEM:INSTALL
BULL_DIR:BULLETIN/DEL
BULL_DIR:BULLETIN/SHAR/OPEN/HEAD/-
PRIV=(OPER,SYSPRV,CMKRNL,WORLD,DETACH,PRMMBX,SYSNAM)
/EXIT
$!
$! NOTE: BULLETIN requires a separate help library. If you do not wish
$! the library to be placed in SYS$HELP, modify the following lines and
$! define the logical name BULL_HELP to be the help library directory, i.e.
$!      $ DEFINE/SYSTEM BULL_HELP SYSD$:[NEWDIRECTORY]
$! The above line should be placed in BULLSTART.COM to be executed after
$! every system reboot.
$!
$ IF F$SEARCH("SYS$HELP:BULL.HLB") .NES. "" THEN LIB/DELETE=*/HELP SYS$HELP:BULL
$ IF F$SEARCH("SYS$HELP:BULL.HLB") .EQS. "" THEN LIB/CREATE/HELP SYS$HELP:BULL
$ LIB/HELP SYS$HELP:BULL BULLCOMS1,BULLCOMS2
$ LIB/HELP SYS$HELP:HELPLIB BULLETIN
$eod 
$copy sys$input INSTALL_REMOTE.COM
$deck
$!
$! INSTALL_REMOTE.COM
$! VERSION 5/25/88
$!
$! DESCRIPTION:
$! Command procedure to easily install BULLETIN.EXE on several nodes.
$!
$! INPUTS:
$! The following parameters can be added to the command line.  They
$! should be placed on the command line which executes this command
$! procedure, separated by spaces.  I.e. @INSTALL_REMOTE.COM OLD COPY TEST
$!
$! OLD  - Specifies that the present version of BULLETIN is 1.51 or earlier.
$! COPY - Specifies that the executable is to be copied to the nodes.
$! TEST - Specifies that all the nodes are to be checked to see if they
$!        are up before beginning the intallation.
$!
$! NOTES:
$!      ***PLEASE READ ALL COMMENTS BEFORE RUNNING THIS***
$! This calls REMOTE.COM which is also included with the installation.
$!
$! DCLREMOTE.COM must be properly installed on all nodes.
$! See comments at the beginning of that file for instructions.
$! Also, you need to have a proxy login with privileges on those nodes.
$! This procedure assumes that the BULLETIN executable on each node is
$! located in the BULL_DIR directory.  The new executable should be copied
$! to that directory before running this procedure, or the COPY option
$! should be used.
$!
$! If the present version of BULLETIN is 1.51 or earlier, it does not have
$! the ability of setting BULL_DISABLE to disable BULLETIN, so you should
$! use the OLD parameter when running this procedure.
$!
$! INSTRUCTIONS FOR SPECIFYING THE NODES AT YOUR SITE:
$! Place the nodes where bulletin is to be reinstalled in variable NODES.
$! Place the nodes where the executable is to be copied to in COPY_NODES.
$! Place nodes where BULLCP is running in BULLCP_NODES.
$!
$ NODES = "ALCVAX,NERUS,ANANSI,MOLVAX,LAURIE,CANDLE,KLYPSO,DOME" +-
",ARVON,LARAN,ORYANA,PALDAR,MOTHRA,TARNA,DARIUS"
$ COPY_NODES = "NERUS,LAURIE,ARVON"
$ BULLCP_NODES = "NERUS,LAURIE,ARVON"
$!
$ NODES = NODES + ","
$ COPY_NODES = COPY_NODES + ","
$ BULLCP_NODES = BULLCP_NODES + ","
$!
$! Check for any parameters passed to the command procedure.
$!
$ PARAMETER = P1 + P2 + P3
$ OLD = 0
$ IF F$LOCATE("OLD",PARAMETER) .NE. F$LENGTH(PARAMETER) THEN OLD = 1
$ TEST = 0
$ IF F$LOCATE("TEST",PARAMETER) .NE. F$LENGTH(PARAMETER) THEN TEST = 1
$ COPYB = 0
$ IF F$LOCATE("COPY",PARAMETER) .NE. F$LENGTH(PARAMETER) THEN COPYB = 1
$!
$! If TEST requested, see if nodes are accessible.
$!
$ IF .NOT. TEST THEN GOTO END_TEST
$BEGIN_TEST:
$ NODES1 = NODES
$TEST:
$ IF F$LEN(NODES1) .EQ. 0 THEN GOTO END_TEST
$ NODE = F$EXTRACT(0,F$LOCATE(",",NODES1),NODES1)
$ NODES1 = NODES1 - NODE - ","
$ @REMOTE 'NODE' END
$ GOTO TEST
$END_TEST:
$!
$! If COPY requested, copy executable to nodes.
$!
$ IF .NOT. COPYB THEN GOTO END_COPY
$COPY:
$ IF F$LEN(COPY_NODES) .EQ. 0 THEN GOTO END_COPY
$ NODE = F$EXTRACT(0,F$LOCATE(",",COPY_NODES),COPY_NODES)
$ COPY_NODES = COPY_NODES - NODE - ","
$ COPY BULLETIN.EXE 'NODE'::BULL_DIR:
$ GOTO COPY
$END_COPY:
$!
$! The procedure now goes to each node and disables bulletin and kills
$! the BULLCP process if present.  NOTE: If version is < 1.51, we assume
$! that BULLCP is running under SYSTEM account.  This is not necessary
$! for older versions where the BULLETIN/STOP command can be used.
$! If BULLCP is not running under the SYSTEM account for version 1.51
$! or less, you will have to kill them manually before running this!
$!
$BEGIN_DISABLE:
$ NODES1 = NODES
$DISABLE:
$ IF F$LEN(NODES1) .EQ. 0 THEN GOTO END_DISABLE
$ NODE = F$EXTRACT(0,F$LOCATE(",",NODES1),NODES1)
$ NODES1 = NODES1 - NODE - ","
$ @REMOTE 'NODE' CONTINUE SET PROC/PRIV=ALL
$ IF F$LOCATE(","+NODE+",",","+BULLCP_NODES) .EQ. -
 F$LENGTH(","+BULLCP_NODES) THEN GOTO SKIP_STOP_BULLCP
$ IF OLD THEN @REMOTE 'NODE' CONTINUE SET UIC [SYSTEM]
$ IF OLD THEN @REMOTE 'NODE' CONTINUE STOP BULLCP
$ IF .NOT. OLD THEN @REMOTE 'NODE' CONTINUE BULLETIN := $BULL_DIR:BULLETIN
$ IF .NOT. OLD THEN @REMOTE 'NODE' CONTINUE BULLETIN/STOP
$SKIP_STOP_BULLCP:
$ @REMOTE 'NODE' CONTINUE INS := $SYS$SYSTEM:INSTALL
$ IF OLD THEN @REMOTE 'NODE' END INS BULL_DIR:BULLETIN/DELETE
$ IF .NOT. OLD THEN @REMOTE 'NODE' END DEF/SYSTEM BULL_DISABLE DISABLE
$ GOTO DISABLE
$END_DISABLE:
$!
$! The procedure now installs the new BULLETIN.
$!
$ NODES1 = NODES
$INSTALL:
$ IF F$LEN(NODES1) .EQ. 0 THEN EXIT
$ NODE = F$EXTRACT(0,F$LOCATE(",",NODES1),NODES1)
$ NODES1 = NODES1 - NODE - ","
$ @REMOTE 'NODE' CONTINUE SET PROC/PRIV=ALL
$ @REMOTE 'NODE' CONTINUE INS := $SYS$SYSTEM:INSTALL
$ @REMOTE 'NODE' CONTINUE BULLETIN := $BULL_DIR:BULLETIN
$ IF OLD THEN @REMOTE 'NODE' CONTINUE INS BULL_DIR:BULLETIN/SHAR-
/OPEN/HEAD/PRIV=(OPER,SYSPRV,CMKRNL,WORLD,DETACH,PRMMBX,SYSNAM)
$ IF .NOT. OLD THEN @REMOTE 'NODE' CONTINUE INS BULL_DIR:BULLETIN/REPLACE
$ IF .NOT. OLD THEN @REMOTE 'NODE' CONTINUE DEASS/SYSTEM BULL_DISABLE
$ IF F$LOCATE(","+NODE+",",","+BULLCP_NODES) .EQ. -
 F$LENGTH(","+BULLCP_NODES) THEN GOTO SKIP_START_BULLCP
$ @REMOTE 'NODE' CONTINUE SET UIC [SYSTEM]
$ @REMOTE 'NODE' CONTINUE BULLETIN := $BULL_DIR:BULLETIN"
$ @REMOTE 'NODE' CONTINUE BULLETIN/START
$SKIP_START_BULLCP:
$ @REMOTE 'NODE' END CONTINUE
$ GOTO INSTALL
$eod 
$copy sys$input INSTRUCT.COM
$deck
$ BULLETIN
ADD/PERMANENT/SYSTEM INSTRUCT.TXT
INFO ON HOW TO USE THE BULLETIN UTILITY.
ADD/PERMANENT NONSYSTEM.TXT
INFO ON BEING PROMPTED TO READ NON-SYSTEM BULLETINS.
EXIT
$eod 
$copy sys$input LOGIN.COM
$deck
$!
$! The following line defines the BULLETIN command.
$!
$ BULL*ETIN :== $BULL_DIR:BULLETIN
$!
$! Note: The command prompt when executing the utility is named after
$! the executable image.  Thus, as it is presently set up, the prompt
$! will be "BULLETIN>".  DO NOT make the command that executes the
$! image different from the image name, or certain things will break.
$!
$! If you would rather define the BULLETIN command using CDU rather than
$! defining it using a symbol, use the BULLETIN.CLD file to do so.
$!
$! The following line causes new messages to be displayed upon logging in.
$!
$ BULLETIN/LOGIN/REVERSE
$!
$! If you wish bulletins to be displayed starting with
$! the newest rather the oldest, omit the /REVERSE qualifier.
$! Note that for totally new users, only permanent system messages and
$! the first non-system general message is displayed (which, if you ran
$! INSTURCT.COM, would describe what a non-system message is).
$! This is done so as to avoid overwhelming a new user with lots of
$! messages upon logging in for the first time.
$!
$eod 
$copy sys$input MAKEFILE.
$deck
# Makefile for BULLETIN

Bulletin : Bulletin.Exe Bull.Hlb

Bulletin.Exe : Bull.Olb
   Link /NoTrace Bull.Olb/Lib /Inc=Bulletin$Main,Sys$System:Sys.Stb/Sel -
        /NoUserlib /Exe=Bulletin.Exe

Bull.Olb : Bulletin.Obj Bulletin0.Obj Bulletin1.Obj Bulletin2.Obj  \
           Bulletin3.Obj Bulletin4.Obj Bulletin5.Obj Bulletin6.Obj \
           Bulletin7.Obj Bulletin8.Obj Bulletin9.Obj \
           Bullcom.Obj Bullmain.Obj Allmacs.Obj
   Library /Create Bull.Olb *.Obj
   Purge /Log *.Obj,*.Exe

Bulletin.Obj : Bulletin.For Bullfiles.Inc Bulldir.Inc Bullfolder.Inc \
               Bulluser.Inc
   Fortran /Extend /NoList Bulletin.For

Bulletin0.Obj : Bulletin0.For Bulldir.Inc Bulluser.Inc Bullfolder.Inc \
                Bullfiles.Inc
   Fortran /Extend /NoList Bulletin0.For

Bulletin1.Obj : Bulletin1.For Bulldir.Inc Bullfolder.Inc Bulluser.Inc \
                Bullfiles.Inc
   Fortran /Extend /NoList Bulletin1.For

Bulletin2.Obj : Bulletin2.For Bulldir.Inc Bulluser.Inc Bullfolder.Inc \
                Bullfiles.Inc
   Fortran /Extend /NoList Bulletin2.For

Bulletin3.Obj : Bulletin3.For Bulldir.Inc Bullfolder.Inc Bulluser.Inc \
                Bullfiles.Inc
   Fortran /Extend /NoList Bulletin3.For

Bulletin4.Obj : Bulletin4.For Bullfolder.Inc Bulluser.Inc Bullfiles.Inc \
                Bulldir.Inc
   Fortran /Extend /NoList Bulletin4.For

Bulletin5.Obj : Bulletin5.For Bulldir.Inc Bulluser.Inc Bullfolder.Inc \
                Bullfiles.Inc
   Fortran /Extend /NoList Bulletin5.For

Bulletin6.Obj : Bulletin6.For Bulldir.Inc Bulluser.Inc Bullfolder.Inc \
                Bullfiles.Inc
   Fortran /Extend /NoList Bulletin6.For

Bulletin7.Obj : Bulletin7.For Bulldir.Inc Bulluser.Inc Bullfolder.Inc \
                Bullfiles.Inc
   Fortran /Extend /NoList Bulletin7.For

Bulletin8.Obj : Bulletin8.For Bulldir.Inc Bulluser.Inc Bullfolder.Inc \
                Bullfiles.Inc
   Fortran /Extend /NoList Bulletin8.For

Bulletin9.Obj : Bulletin9.For Bulldir.Inc Bulluser.Inc Bullfolder.Inc \
                Bullfiles.Inc
   Fortran /Extend /NoList Bulletin9.For

Allmacs.Obj : Allmacs.mar
   Macro   /NoList Allmacs.Mar

Bullcom.Obj : Bullcom.cld
   Set Command /Obj Bullcom.Cld

Bullmain.Obj : Bullmain.cld
   Set Command /Obj Bullmain.Cld

Bull.Hlb : Bullcoms1.Hlp Bullcoms2.Hlp
   Library /Create /Help Bull.Hlb Bullcoms1.Hlp, Bullcoms2.Hlp
   Purge Bull.Hlb
*.hlb :
        lib/help/cre $*

$eod 
$copy sys$input REMOTE.COM
$deck
$! FILE: REMOTE.COM     VERSION 1.3     EDIT 880513 - CAK
$! DCL procedure to execute DCL commands on a remote decnet node.
$! The remote DECNET object DCLREMOTE.COM must be defined as a known type 0 
$! object on the remote node or the file must be in the login directory
$! of the account used on the remote system. Or the logical name DCLREMOTE
$! can be defined to point at the object.
$!
$! Usage:       REM*OTE :== @SYS$MANAGER:REMOTE [P1] [P2] ...
$!
$! P1 - Node name commands are to be executed on, including any access control.
$!      If no access control is specified then a proxy login is attempted.
$!      The you do not have an account on the remote system then the default
$!      DECNET account is used.
$! P2 - DCL command to execute on the remote system. Optional.
$! P3-P8 Additional parameters passed to the command (so quotes aren't needed)
$
$ ON WARNING THEN GOTO ERROR
$ ON CONTROL_Y THEN GOTO ERROR
$ COMMAND := 'P2' 'P3' 'P4' 'P5' 'P6' 'P7' 'P8'
$ IF P2 .EQS. "CONTINUE" THEN COMMAND = COMMAND - "CONTINUE"
$ IF P2 .EQS. "END" THEN COMMAND = COMMAND - "END"
$ NEXT_CMD = "NEXT_CMD"
$ IF P2 .NES. "" THEN NEXT_CMD = "DONE"
$ P1 = P1 - "::"
$ 
$ IF F$LOG ("NET") .EQS. "" THEN GOTO OPEN_LINK
$ IF P2 .EQS. "CONTINUE" THEN GOTO NEXT_CMD
$ IF P2 .EQS. "END" THEN GOTO NEXT_CMD
$OPEN_LINK:
$ WRITE SYS$OUTPUT "Establishing DECNET link to node ''P1'..."
$ OPEN/WRITE/READ NET 'P1'::"TASK=DCLREMOTE"
$
$NEXT_CMD:
$ IF P2 .EQS. "" THEN READ /ERR=ERROR/PROMPT="''P1'> " SYS$COMMAND COMMAND
$ IF F$EDIT(F$EXTR(0,1,COMMAND),"UPCASE") .EQS. "E" THEN GOTO DONE
$ WRITE NET COMMAND
$LOOP:
$   READ/ERR=ERROR/TIME_OUT=10 NET LINE
$   IF F$EXTR (0,12,LINE) .EQS. "COMMAND$DONE" THEN GOTO 'NEXT_CMD'
$   WRITE SYS$OUTPUT LINE
$   GOTO LOOP
$DONE:
$ IF P2 .EQS. "CONTINUE" THEN EXIT
$ IF F$LOG ("NET") .NES. "" THEN CLOSE NET
$ EXIT
$ERROR:
$ IF F$LOG ("NET") .NES. "" THEN CLOSE NET
$ STOP
$eod 