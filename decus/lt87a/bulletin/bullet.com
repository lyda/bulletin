From:	HENRY::IN%"MRL%PFCVAX%xx.lcs.mit.edu%csnet-relay.CSNET%relay.cs.net@RCA.COM" 19-JUN-1987 21:00
To:	"SYSTEM%UK.AC.SOTON.ESP.V1" <SYSTEM%UK.AC.SOTON.ESP.V1%nss.cs.ucl.ac.uk@xx>, 
Subj:	BULLET.COM

$set nover
$copy sys$input AAAREADME.TXT
$deck
The following are instructions for creating and installing the BULLETIN
utility. None of the command procedures included here are sophisticated, so it
is likely that several modifications will have to be made by the installer.
The installer should enable all privileges before installation. 

1) CREATE.COM
   This will compile and link the BULLETIN sources. Also, there are several
   INCLUDE files for the fortran sources (.INC files). BULLFILES.INC must first
   be modified before this procedure is run. It contains the names of data files
   which BULLETIN creates. It also includes specifications of directories used
   by the FOLDER and BBOARD features. (In relation to the FOLDER feature, you
   can restrict FOLDER creation to privileged users.  See BULLCOM.CLD).
   You should also look at BULLFOLDER.INC, as there may be some parameters in
   that you may or may not want to modify.

   (NOTE: If you are simply receiving the objects, you should use the procedure
   CREATE_NOFORT.COM.  The objects have have been compiled to use the directory
   BULLETIN$ for all data files.  You should define this as a system logical
   name pointing to the directory which you plan to use, i.e. $ DEFINE/SYSTEM
   BULLETIN$ USRD$:[BULLETIN] .  You should also include this definition in
   BULLSTART.COM, which is mentioned below.)

   NOTE 1: If you elect to have folders with the BBOARD feature that receives
   messages from outside networks, and wish the RESPOND command to be able
   to send messages to the originators of these messages, you must modify
   the subroutine RESPOND in BULLETIN2.FOR in order to specify the mail
   utility which you use to send mail over those networks.

   NOTE 2: The maximum number of folders for this distribution is 96 folders.
   If you wish to increase this, modify BULLUSER.INC and recompile the sources.
   When the new executable is run, it will create a new BULLUSER.DAT data file
   and rename the old one to BULLUSER.OLD.  You cannot reduce the number of
   folders.

2) INSTALL.COM
   The following procedure copies the executable image to SYS$SYSTEM and
   installs it with certain privileges.  It also installs the necessary
   help files in SYS$HELP.  (BULLETIN help file is installed into the
   system help library HELPLIB.HLB.  If you don't wish this done, delete
   or modify the appropriate line in the procedure.  Also, the help
   library for the BULLETIN program, BULL.HLB, can be moved to a different
   directory other than SYS$HELP.  If this is done, the system logical name
   BULL$HELP should be defined to be the directory where the library is
   to be found.)

3) LOGIN.COM
   This contains the comands that should be executed at login time
   by SYS$MANAGER:SYLOGIN.COM.  It defines the BULLETIN commands.
   It also executes the command BULLETIN/LOGIN in order to notify
   the user of new messages.  NOTE: If you wish the utility to be a
   different name than BULLETIN, you should modify this procedure.
   The prompt which the utility uses is named after image executable.
   If you want messages displayed upon logging in starting from
   oldest to newest (rather than newest to oldest), add /REVERSE to
   the BULLETIN/LOGIN command.  Also note that users with the DISMAIL
   flag setting in the authorization file will not be notified of
   new emssages.  See help on the SET LOGIN command within the BULLETIN
   utility for more information on this.

4) BULLSTART.COM
   This procedure contains the commands that should be executed after
   a system startup.  It should be executed by SYS$MANAGER:SYSTARTUP.COM.
   It simply installs the BULLETIN utility with correct privileges.

5) BULLETIN.COM
   If one wants the feature of using BULLETIN between DECNET nodes,
   this file must be put in each node's DECNET default user's directory
   (usually [DECNET]).  Once this is done, the /NODE qualifer for the
   ADD command can be used.
   NOTE:  Privileged functions such as /SYSTEM will work on other nodes
   if you have an account on the other node with appropriate privileges.
   You will be prompted for the password for the account on the remote node.

6) INSTRUCT.COM
   This procedure adds 2 permanent messages which give a very brief
   description about the BULLETIN utility, and how to turn off optional
   prompting of non-system messages (via SET NOREADNEW).

7) BOARD_SPECIAL.COM
   This command procedure describes and illustrates how to use the
   SET BBOARD/SPECIAL feature.  This feature allows the use of BBOARD
   where the input does not come from VMS MAIL.  For example, this could
   be used in the case where mail from a non-DEC network is not stored
   in the VMS MAIL.  Another example is BOARD_DIGEST.COM.  This file
   takes mail messages from "digest" type mailing lists and splits them
   into separate BULLETIN messages for easier reading.
$eod 
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
$ FF[0,8] = 12			! Define a form feed character
$ SET PROTECT=(W:RWED)/DEFAULT
$ SET PROC/PRIV=SYSPRV
$ USER := 'F$GETJPI("","USERNAME")
$ EXTRACT_FILE = "BULLETIN$:" + "''USER'" + ".TXT"
$ DEFINE/USER EXTRACT_FILE BULLETIN$:'USER'
$ MAIL
READ
EXTRACT EXTRACT_FILE
DELETE
$ OPEN/READ INPUT 'EXTRACT_FILE'
$ OPEN/WRITE OUTPUT 'EXTRACT_FILE'
$ READ INPUT FROM_USER
$AGAIN:
$ READ/END=ERROR INPUT BUFFER
$ IF F$EXTRACT(0,15,BUFFER) .NES. "---------------" THEN GOTO AGAIN
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
$ WRITE OUTPUT "To:	" + USER
				! Write To: + TAB + BBOARDUSERNAME
$ WRITE OUTPUT "Subj:	" + SUBJ
				! Write Subject: + TAB + mail subject
$ WRITE OUTPUT ""		! Write one blank line
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
$!	First line is 0 length line.
$!	Second line is "From:" followed by TAB followed by incoming username
$!	Third line is "To:" followed by TAB followed by BBOARD username
$!	The message text then follows.
$!	Message is ended by a line containing a FORM FEED.
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
$ USERNAME := 'F$GETJPI("","USERNAME")'		! This trims trailing spaces
$ IF F$SEARCH("MFE_TELL_FILES:"+USERNAME+".MAI") .EQS. "" THEN EXIT
$ SET DEFAULT BULLETIN$:	! BULLETIN looks for text in BBOARD directory
$ SET PROTECT=(W:RWED)/DEFAULT
$ IF F$SEARCH("MFEMSG.MAI") .NES. "" THEN -
  DELETE MFEMSG.MAI;*		! Delete any leftover output files.
$ MSG := $MFE_TELL: MESSAGE
$ DEFINE/USER SYS$COMMAND SYS$INPUT
$ MSG				! Read MFENET mail
copy * MFEMSG
delete *
exit
$ FF[0,8] = 12			! Define a form feed character
$ OPEN/READ/ERROR=EXIT INPUT MFEMSG.MAI
$ OUTNAME = USERNAME+".TXT"	! Output file will be 'USERNAME'.TXT
$ OPEN/WRITE OUTPUT 'OUTNAME'
$ READ/END=END INPUT DATA		! Skip first line in MSG output
$HEADER:
$ FROM = ""
$ SUBJ = ""
$ MFEMAIL = "T"
$NEXTHEADER:
$ IF (FROM.NES."") .AND. (SUBJ.NES."") THEN GOTO SKIPHEADER
$ READ/END=END INPUT DATA		! Read header line in MSG output
$ IF DATA .EQS. "" THEN GOTO SKIPHEADER	! Missing From or Subj ??
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
$ WRITE OUTPUT "From:	" + FROM
				! Write From: + TAB + USERNAME
$ WRITE OUTPUT "To:	" + USERNAME
				! Write To: + TAB + BBOARDUSERNAME
$ WRITE OUTPUT "Subj:	" + SUBJ
				! Write Subject: + TAB + mail subject
$ WRITE OUTPUT ""		! Write one blank line
$ IF (DATA.EQS."") .OR. MFEMAIL THEN GOTO SKIPBLANKS
$50$:
$ READ/END=END INPUT DATA		! Skip rest of main header
$ IF DATA .NES. "" THEN GOTO 50$
$60$:
$ READ/END=END INPUT DATA		! Skip all of secondary header
$ IF DATA .NES. "" THEN GOTO 60$
$SKIPBLANKS:
$ READ/END=END INPUT DATA		! Skip all blanks
$ IF DATA .EQS. "" THEN GOTO SKIPBLANKS
$NEXT:				! Read and write message text
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
! VERSION 6/16/87
!
 	MODULE BULLETIN_SUBCOMMANDS

	DEFINE VERB ADD
		PARAMETER P1, LABEL=FILESPEC, VALUE(TYPE=$FILE)
		QUALIFIER ALL, NONNEGATABLE
		QUALIFIER BELL, NONNEGATABLE
		QUALIFIER BROADCAST, NONNEGATABLE
		DISALLOW NOT BROADCAST AND ALL
		DISALLOW NOT BROADCAST AND BELL
		QUALIFIER EDIT, NONNEGATABLE
		QUALIFIER FOLDER, LABEL=SELECT_FOLDER, VALUE(REQUIRED)
		QUALIFIER NODES, LABEL=NODES, VALUE(REQUIRED,LIST)
		NONNEGATABLE
		QUALIFIER PERMANENT, NONNEGATABLE
		QUALIFIER SHUTDOWN, NONNEGATABLE
		DISALLOW PERMANENT AND SHUTDOWN
		QUALIFIER SYSTEM, NONNEGATABLE
		QUALIFIER USERNAME, LABEL=USERNAME, VALUE(REQUIRED)
		NONNEGATABLE
	DEFINE VERB BACK
	DEFINE VERB COPY
		PARAMETER P1, LABEL=FOLDER, PROMPT="Folder"
			VALUE(REQUIRED)
		QUALIFIER BULLETIN_NUMBER
		QUALIFIER ORIGINAL
		DISALLOW FOLDER AND BULLETIN_NUMBER
	DEFINE VERB CREATE
		QUALIFIER BRIEF, NONNEGATABLE
!
! Make the following qualifier DEFAULT if you want CREATE to be
! a privileged command.
!
		QUALIFIER NEEDPRIV, NONNEGATABLE
		QUALIFIER NOTIFY, NONNEGATABLE
		QUALIFIER PRIVATE, NONNEGATABLE
		QUALIFIER READNEW, NONNEGATABLE
		QUALIFIER SEMIPRIVATE, NONNEGATABLE
		PARAMETER P1, LABEL=CREATE_FOLDER, PROMPT="Folder"
			VALUE(REQUIRED)
		DISALLOW PRIVATE AND SEMIPRIVATE
		DISALLOW BRIEF AND READNEW
	DEFINE VERB CURRENT
	DEFINE VERB DELETE
		PARAMETER P1, LABEL=BULLETIN_NUMBER, VALUE(TYPE=$FILE)
		QUALIFIER NODES, LABEL=NODES, VALUE(REQUIRED,LIST)
		QUALIFIER USERNAME, LABEL=USERNAME, VALUE(REQUIRED)
		QUALIFIER SUBJECT, VALUE(REQUIRED)
		DISALLOW NOT SUBJECT AND NODES
	DEFINE VERB DIRECTORY
		QUALIFIER FOLDER, SYNTAX=DIRECTORY_FOLDER, NONNEGATABLE
		QUALIFIER START, VALUE(REQUIRED,TYPE=$NUMBER), NONNEGATABLE
		QUALIFIER SINCE,VALUE(DEFAULT="TODAY",TYPE=$DATETIME)
		DISALLOW START AND SINCE
	DEFINE SYNTAX DIRECTORY_FOLDER
		QUALIFIER FOLDER, DEFAULT
	DEFINE VERB E				! EXIT command.
	DEFINE VERB EX				! EXIT command.
	DEFINE VERB EXIT			! EXIT command.
	DEFINE VERB EXTRACT
		PARAMETER P1, LABEL=FILESPEC, VALUE(TYPE=$FILE,REQUIRED),
			PROMPT="File"
		QUALIFIER HEADER, DEFAULT
		QUALIFIER NEW, NONNEGATABLE
	DEFINE VERB FILE
		PARAMETER P1, LABEL=FILESPEC, VALUE(TYPE=$FILE,REQUIRED),
			PROMPT="File"
		QUALIFIER HEADER, DEFAULT
		QUALIFIER NEW, NONNEGATABLE
	DEFINE VERB HELP
		PARAMETER P1, LABEL=HELP_FOLDER, VALUE(TYPE=$REST_OF_LINE)
	DEFINE VERB LAST
	DEFINE VERB MAIL
		PARAMETER P1, LABEL=RECIPIENTS, PROMPT="Recipients"
		VALUE(REQUIRED,TYPE=$REST_OF_LINE)
		QUALIFIER SUBJECT, VALUE(REQUIRED)
	DEFINE VERB MODIFY
		QUALIFIER DESCRIPTION
		QUALIFIER NAME, VALUE(REQUIRED)
		QUALIFIER OWNER, VALUE(REQUIRED)
	DEFINE VERB MOVE
		PARAMETER P1, LABEL=FOLDER, PROMPT="Folder"
			VALUE(REQUIRED)
		QUALIFIER BULLETIN_NUMBER
		QUALIFIER NODES
		QUALIFIER ORIGINAL
		DISALLOW FOLDER AND BULLETIN_NUMBER
		DISALLOW FOLDER AND NODES
	DEFINE VERB NEXT
	DEFINE VERB PRINT
		QUALIFIER HEADER, DEFAULT
		QUALIFIER NOTIFY, DEFAULT
		QUALIFIER QUEUE, VALUE(DEFAULT=SYS$PRINT), NONNEGATABLE
	DEFINE VERB QUIT
	DEFINE VERB READ
		PARAMETER P1, LABEL=BULLETIN_NUMBER, VALUE(TYPE=$NUMBER)
		QUALIFIER PAGE, DEFAULT
		QUALIFIER SINCE,VALUE(DEFAULT="TODAY",TYPE=$DATETIME)
	DEFINE VERB REPLACE
		PARAMETER P1, LABEL=FILESPEC, VALUE(TYPE=$FILE)
		QUALIFIER EDIT, NONNEGATABLE
		QUALIFIER EXPIRATION, NONNEGATABLE
		QUALIFIER GENERAL, NONNEGATABLE
		QUALIFIER HEADER, NONNEGATABLE
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
	DEFINE VERB REMOVE
		PARAMETER P1, LABEL=REMOVE_FOLDER, PROMPT="Folder"
			VALUE(REQUIRED)
	DEFINE VERB RESPOND
		QUALIFIER SUBJECT, VALUE(REQUIRED)
	DEFINE VERB SEARCH
		PARAMETER P1, LABEL=SEARCH_STRING
		QUALIFIER START, VALUE(TYPE=$NUMBER,REQUIRED)
	DEFINE VERB SELECT
		PARAMETER P1, LABEL=SELECT_FOLDER
	DEFINE VERB SET
		PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
			VALUE(REQUIRED, TYPE=SET_OPTIONS)
	DEFINE TYPE SET_OPTIONS
		KEYWORD LOGIN, SYNTAX=SET_LOGIN
		KEYWORD NOLOGIN, SYNTAX=SET_LOGIN
		KEYWORD NOBBOARD
		KEYWORD BBOARD, SYNTAX=SET_BBOARD
		KEYWORD NOBRIEF, SYNTAX=SET_FLAGS
		KEYWORD BRIEF, SYNTAX=SET_FLAGS
		KEYWORD NOREADNEW, SYNTAX=SET_FLAGS
		KEYWORD READNEW, SYNTAX=SET_FLAGS
		KEYWORD ACCESS, SYNTAX=SET_ACCESS
		KEYWORD NOACCESS, SYNTAX=SET_NOACCESS
		KEYWORD FOLDER, SYNTAX=SET_FOLDER
		KEYWORD NOTIFY, SYNTAX=SET_FLAGS
		KEYWORD NONOTIFY, SYNTAX=SET_FLAGS
		KEYWORD PRIVILEGES, SYNTAX=SET_PRIVILEGES
	DEFINE SYNTAX SET_LOGIN
		PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
			VALUE(REQUIRED, TYPE=SET_OPTIONS)
		PARAMETER P2, LABEL=USERNAME, VALUE(REQUIRED)
	DEFINE SYNTAX SET_FLAGS
		PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
			VALUE(REQUIRED, TYPE=SET_OPTIONS)
		QUALIFIER DEFAULT, NONNEGATABLE
		QUALIFIER ALL, NONNEGATABLE
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
	DEFINE SYNTAX SET_NOACCESS
		PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
			VALUE(REQUIRED, TYPE=SET_OPTIONS)
		PARAMETER P2, LABEL=ACCESS_ID
		PARAMETER P3, LABEL=ACCESS_FOLDER
		QUALIFIER ALL, NONNEGATABLE
		QUALIFIER READONLY, NONNEGATABLE
		DISALLOW NOT ALL AND NOT ACCESS_ID
		DISALLOW ALL AND NOT READONLY
	DEFINE SYNTAX SET_ACCESS
		PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
			VALUE(REQUIRED, TYPE=SET_OPTIONS)
		PARAMETER P2, LABEL=ACCESS_ID
		PARAMETER P3, LABEL=ACCESS_FOLDER
		QUALIFIER READONLY, NONNEGATABLE
		QUALIFIER ALL, NONNEGATABLE
		DISALLOW NOT ALL AND NOT ACCESS_ID
	DEFINE SYNTAX SET_PRIVILEGES
		PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
			VALUE(REQUIRED, TYPE=SET_OPTIONS)
		PARAMETER P2, LABEL=PRIVILEGES, PROMPT="Privileges"
		VALUE (REQUIRED,LIST)
	DEFINE VERB SHOW
		PARAMETER P1, LABEL=SHOW_PARAM1, PROMPT="What"
			VALUE(REQUIRED, TYPE=SHOW_OPTIONS)
		QUALIFIER FULL, SYNTAX=SHOW_FOLDER_FULL, NONNEGATABLE
	DEFINE TYPE SHOW_OPTIONS
		KEYWORD BRIEF, SYNTAX=SHOW_FLAGS
		KEYWORD FOLDER, SYNTAX=SHOW_FOLDER
		KEYWORD NOTIFY, SYNTAX=SHOW_FLAGS
		KEYWORD NEW, SYNTAX=SHOW_FLAGS
		KEYWORD PRIVILEGES, SYNTAX=SHOW_FLAGS
		KEYWORD READNEW, SYNTAX=SHOW_FLAGS
	DEFINE SYNTAX SHOW_FLAGS
		PARAMETER P1, LABEL=SHOW_PARAM1, PROMPT="What"
			VALUE(REQUIRED, TYPE=SHOW_OPTIONS)
	DEFINE SYNTAX SHOW_FOLDER
		PARAMETER P1, LABEL=SHOW_PARAM1, PROMPT="What"
			VALUE(REQUIRED, TYPE=SHOW_OPTIONS)
		PARAMETER P2, LABEL=SHOW_FOLDER
	DEFINE SYNTAX SHOW_FOLDER_FULL
		QUALIFIER FULL, DEFAULT
		PARAMETER P1, LABEL=SHOW_PARAM1, PROMPT="What"
			VALUE(REQUIRED, TYPE=SHOW_OPTIONS)
		PARAMETER P2, LABEL=SHOW_FOLDER
$eod 
$copy sys$input BULLDIR.INC
$deck
	COMMON /BULL_DIR/ DESCRIP,FROM,DATE,TIME,LENGTH,EXDATE,EXTIME
     &	,SYSTEM,BLOCK,NEWEST_EXDATE,NEWEST_EXTIME,NEWEST_DATE,NEWEST_TIME
     &	,NBULL,NBLOCK,SHUTDOWN,SHUTDOWN_DATE,SHUTDOWN_TIME,NEMPTY
	CHARACTER*53 DESCRIP
	CHARACTER*12 FROM
	CHARACTER*11 DATE,EXDATE,NEWEST_EXDATE,NEWEST_DATE,SHUTDOWN_DATE
	CHARACTER*8 TIME,EXTIME,NEWEST_EXTIME,NEWEST_TIME,SHUTDOWN_TIME
	LOGICAL SYSTEM

	CHARACTER*116 BULLDIR_COM		! This value + 12 must be
	EQUIVALENCE (DESCRIP,BULLDIR_COM)	! divisable by 4
$eod 
$copy sys$input BULLETIN.COM
$deck
$ DEFINE SYS$INPUT SYS$NET
$ BULLETIN
$eod 
$copy sys$input BULLETIN.HLP
$deck
1 BULLETIN
Invokes the PFC BULLETIN Utility.  This utility is used for reading,
adding and deleting message.  Users are notified at login time that new
messages have been added and the topics of those messages are
displayed.  Reading of those messages is optional. (Use the command SET
READNEW while in BULLETIN for setting automatic reading.)  Privileged
users can add system bulletins that are displayed in full at login
time.  These messages are also saved, and can be read by BULLETIN. 
Messages are automatically deleted after a specified expiration date,
or they can manually be deleted by either the submitter of the message
or a privileged user. 

 Format:

      BULLETIN

BULLETIN has an interactive help available while using the utility.
Type HELP after invoking the BULLETIN command.
2 Description
The BULLETIN utility is a utility to display messages to users when
logging in.  Users are notified of messages only once.  They're not
forced into reading them every time they log in.  Submitting and
reading messages is easy to do via a utility similar to the VMS MAIL
utility. Privileged users can create messages which are displayed in
full. (known as SYSTEM messages).  Non-privileged users may be able to
create non-SYSTEM messages (unless your system manager has disabled the
feature), but only topics are displayed at login. 

Folders can be created so that messages pertaining to a single topic
can be placed together.  Folders can be made private so that reading
and writing is limited to only users or groups who are granted access.
Alternatively, folders can be made semi-private in that everyone is
allowed to read them but write access is limited.

When new non-system messages are displayed, an optional feature which a
user may enable will cause BULLETIN to ask whether the user wishes to
read the new bulletins. The user can then read the messages (with the
ability to write any of the messages to a file). A user can enable the
notification and prompting of new messages feature on a folder per
folder basis.  However, the exception is messages submitted to the
default GENERAL folder.  Users are always notified at login of new
bulletins in this folder, but can disable the prompting.  This is to
give non-privileged users some ability to force a notification of an
important message. 

Messages have expiration dates and times, and are deleted automatically.
Expiration dates and times can be specified in absolute or delta
notation. Privileged users can specify "SHUTDOWN" messages, i.e.
messages that get deleted after a system shutdown has occurred. 
"PERMANENT" messages can also be created which never expire. 

Privileged users can broadcast their message (to either all users or
all terminals).

A user can select, on a folder per folder basis, to have a message
broadcast to their terminal immediately notifying them when a new
message has been added. 

An optional "Bulletin Board" feature allows messages to be created by
users of other systems connected via networks.  A username can be
assigned to a folder, and any mail sent to that user is converted to
messages and stored in that folder.  This feature originally was
designed to duplicate the message board feature that exists on some
Arpanet sites.  However, with the addition of folders, another possible
use is to assign an Arpanet mailing list to a folder. For example, one
could have an INFOVAX folder associated with an INFOVAX username, and
have INFO-VAX mail sent to INFOVAX.  Users could then read the mailing
list in that folder, rather than having INFO-VAX sent to each user.
Optionally, the input for the bulletin board can be directed to be taken
from any source other than VMS MAIL.  This might be useful if incoming
mail is stored in a different place other than VMS MAIL.

There is a feature which allows adding GENERAL non-system and system
messages to other DECNET nodes from within the BULLETIN the utility (see
the ADD command).  All information about the message, such as expiration
date, are transferred to the host, thus making it more flexible than the
BBOARD method of adding messages.  Deletion of messages is also
possible across DECNET.

Messages can be either sent to a file, to a print queue, or mailed to
another user.
2 /EDIT
Specifies that all ADD or REPLACE commands within BULLETIN will select
the editor for inputting text.
$eod 
$copy sys$input BULLETIN.LNK
$deck
$ LINK/NOTRACE BULLETIN,BULLETIN0,BULLETIN1,BULLETIN2,BULLETIN3,-
BULLETIN4,BULLETIN5,BULLETIN6,-
BULLCOM,BULLMAIN,ALLMACS,SYS$SYSTEM:SYS.STB/SEL
$eod 
$copy sys$input BULLFILES.INC
$deck
C
C  THE FIRST 2 FILES ARE FILES CREATED AND USED BY BULLETIN.
C  SPECIFY THE DEVICE/DIRECTORY IN WHICH YOU DESIRE THAT THEY BE KEPT.
C
C  FOLDER_DIRECTORY IS THE DIRECTORY THAT FILES FOR FOLDERS THAT
C  ARE CREATED ARE KEPT IN.  IF YOU WISH TO PREVENT FOLDER CREATION,
C  YOU SHOULD MODIFY BULLCOM.CLD TO MAKE THE CREATE COMMAND A PRIVILEGED
C  COMMAND (OR SIMPLY REMOVE THE LINES WHICH DEFINE THE CREATE COMMAND).
C
C  BBOARD_DIRECTORY IS THE SCRATCH AREA USED BY BBOARD WHEN EXTRACTING
C  MAIL.  IF IT IS UNDEFINED, BBOARD WILL NOT BE ABLE TO BE USED.
C  NOTE THAT EITHER THIS DIRECTORY MUST BE GIVEN WORLD READ/WRITE ACCESS,
C  OR THE BBOARD ACCOUNTS MUST BE GIVEN SYSPRV PRIVILEGES TO BE ABLE
C  TO WRITE INTO THIS DIRECTORY.  ALSO, FOR BBOARD TO WORK, MAKE SURE
C  THAT THE SUBPROCESS LIMIT FOR USERS IS AT LEAST 2.  YOU ALSO MAY HAVE
C  TO INCREASE SOME SUBPROCESS SYSTEM PARAMETERS: PQL_DPGFLQUOTA AND
C  PQL_DWSQUOTA MAY HAVE TO BE CHANGED. (10000 AND 500 ARE TYPICAL).
C
	COMMON /FILES/ BULLDIR_FILE,BULLETIN_FILE,BULLUSER_FILE
	COMMON /FILES/ BULLFOLDER_FILE,FOLDER_DIRECTORY,BBOARD_DIRECTORY
	CHARACTER*80 BULLUSER_FILE /'BULLETIN$:BULLUSER.DAT'/
	CHARACTER*80 BULLFOLDER_FILE /'BULLETIN$:BULLFOLDER.DAT'/
	CHARACTER*80 FOLDER_DIRECTORY /'BULLETIN$:'/
	CHARACTER*80 BBOARD_DIRECTORY /'BULLETIN$:'/
C
C  THE FOLLOWING 2 FILES ARE OBSOLETE AS OF V1.1 AND NO LONGER HAVE TO
C  BE SPECIFIED.  BULLETIN NOW TREATS THE GENERAL FOLDER AS ANY OTHER
C  FOLDER.  NEW USERS SHOULD JUST LEAVE THEM ALONE.  HOWEVER, USERS
C  USING OLDER VERSIONS STILL HAVE TO SPECIFY THEM IN ORDER THAT
C  BULLETIN KNOWS THE NAMES IN ORDER TO RENAME THEM.
C
	CHARACTER*80 BULLDIR_FILE /'BULLETIN$:BULLDIR.DAT'/
	CHARACTER*80 BULLETIN_FILE /'BULLETIN$:BULLETIN.DAT'/
$eod 
$copy sys$input BULLFOLDER.INC
$deck
!
!  The following 2 parameters can be modified if desired before compilation.
!
	PARAMETER BBEXPIRE_LIMIT = 30	! Maxmimum time limit in days that
					! BBOARDS can be set to.
	PARAMETER BBOARD_UPDATE = 15	! Number of minutes between checks
					! for new BBOARD mail. (Note: Check
					! only occurs via BULLETIN/LOGIN.
					! Check is forced via BULLETIN/BBOARD).
	PARAMETER ADDID = .TRUE.	! Allows users who are not in the
					! rights data base to be added
					! according to uic number.

	PARAMETER FOLDER_FMT = '(A25,A4,A12,A80,A12,3A4,A8)'
	PARAMETER FOLDER_RECORD = 153

	COMMON /BULL_FOLDER/ FOLDER_SET,FOLDER_OWNER,
     &		FOLDER_DESCRIP,FOLDER,FOLDER_NUMBER,FOLDER_FILE,
     &		FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB
	LOGICAL FOLDER_SET
	DATA FOLDER_SET /.FALSE./, FOLDER/'GENERAL'/
	CHARACTER FOLDER_OWNER*12,FOLDER*25,ACCOUNTB*8
	CHARACTER FOLDER_FILE*80,FOLDER_DESCRIP*80,FOLDER_BBOARD*12

	COMMON /BULL_FOLDER1/ FOLDER1_OWNER,FOLDER1_DESCRIP,
     &		FOLDER1,FOLDER1_NUMBER,FOLDER1_FILE,
     &		FOLDER1_BBOARD,FOLDER1_BBEXPIRE
	CHARACTER FOLDER1_OWNER*12,FOLDER1*25
	CHARACTER FOLDER1_FILE*80,FOLDER1_DESCRIP*80,FOLDER1_BBOARD*12

	CHARACTER*120 FOLDER_COM
	EQUIVALENCE (FOLDER1_OWNER,FOLDER_COM)
$eod 
$copy sys$input BULLMAIN.CLD
$deck
	MODULE BULLETIN_MAINCOMMANDS
	DEFINE VERB BULLETIN
		QUALIFIER BBOARD
		QUALIFIER CLEANUP, LABEL=CLEANUP, VALUE(REQUIRED)
		QUALIFIER EDIT
		QUALIFIER LOGIN
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
$eod 
$copy sys$input BULLSTART.COM
$deck
$ RUN SYS$SYSTEM:INSTALL
SYS$SYSTEM:BULLETIN/SHAR/OPEN/HEAD/PRIV=(OPER,SYSPRV,CMKRNL,WORLD,DETACH,PRMMBX)
/EXIT
$eod 
$copy sys$input BULLUSER.INC
$deck
!
! The parameter FOLDER_MAX should be changed to increase the maximum number
! of folders available.  Due to storage via longwords, the maximum number
! available is always a multiple of 32.  Thus, it will probably make sense
! to specify a multiple of 32 for FOLDER_MAX, as that it what really will be
! the capacity.  Note that the default general folder counts as a folder also,
! so that if you specify 64, you will be able to create 63 folders on your own.
!
	PARAMETER FOLDER_MAX = 96
	PARAMETER FLONG = (FOLDER_MAX + 31)/ 32

	PARAMETER USER_FMT = '(A12,<4+FLONG*4>A4)'
	PARAMETER USER_HEADER = '            '

	COMMON /HEADER_INFO/ TEMP_USER,BBOARD_BTIM,NEWEST_BTIM
	CHARACTER TEMP_USER*12
	DIMENSION BBOARD_BTIM(2),NEWEST_BTIM(2)

	COMMON /BULL_USER/ USERNAME,LOGIN_BTIM,READ_BTIM,
     &		NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	CHARACTER*12 USERNAME
	DIMENSION LOGIN_BTIM(2),READ_BTIM(2)
	DIMENSION NEW_FLAG(FLONG)   ! Bit set indicates new message in folder
	DIMENSION SET_FLAG(FLONG)   ! Bit set indicates READNEW set for folder
	DIMENSION BRIEF_FLAG(FLONG) ! Bit set indicates READNEW/BRIEF set
	DIMENSION NOTIFY_FLAG(FLONG)! Bit set indicates to broadcast
				    ! notification when new bulletin is added.
$eod 
$copy sys$input BULL_COMMAND.COM
$deck
$B:=$PFCVAX$DBC1:[MRL.BULLETIN]BULLETIN.EXE;13
$ON ERROR THEN GOTO EXIT
$ON SEVERE THEN GOTO EXIT
$ON WARNING THEN GOTO EXIT
$B/'F$PROCESS()'
$EXIT:
$LOGOUT
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
$ MAC ALLMACS
$ SET COMMAND/OBJ BULLCOM
$ SET COMMAND/OBJ BULLMAIN
$ @BULLETIN.LNK
$eod 
$copy sys$input CREATE_NOFORT.COM
$deck
$!
$! CREATE_NOFORT.COM
$! Command procedure to create bulletin executable without fortran compiler.
$!
$ RUN ASC2BIN
BULLETIN.ASC
BULLETIN.BAK
$ BACKUP BULLETIN.BAK/SAVE */NEW
$ RUN ASC2BIN
BULLSUB0.ASC
BULLSUB0.BAK
$ BACKUP BULLSUB0.BAK/SAVE */NEW
$ RUN ASC2BIN
BULLSUB1.ASC
BULLSUB1.BAK
$ BACKUP BULLSUB1.BAK/SAVE */NEW
$ RUN ASC2BIN
BULLSUB2.ASC
BULLSUB2.BAK
$ BACKUP BULLSUB2.BAK/SAVE */NEW
$ RUN ASC2BIN
BULLSUB3.ASC
BULLSUB3.BAK
$ BACKUP BULLSUB3.BAK/SAVE */NEW
$ MAC ALLMACS
$ SET COMMAND/OBJ BULLCOM
$ SET COMMAND/OBJ BULLMAIN
$ @BULLETIN.LNK
$ WRITE SYS$OUTPUT "You can now delete all the .BAK and .ASC files."
$eod 
$copy sys$input INSTALL.COM
$deck
$ COPY BULLETIN.EXE SYS$SYSTEM:
$ RUN SYS$SYSTEM:INSTALL
SYS$SYSTEM:BULLETIN/DEL
SYS$SYSTEM:BULLETIN/SHAR/OPEN/HEAD/PRIV=(OPER,SYSPRV,CMKRNL,WORLD,DETACH,PRMMBX)
/EXIT
$!
$! NOTE: BULLETIN requires a separate help library. If you do not wish
$! the library to be placed in SYS$HELP, modify the following lines and
$! define the logical name BULL$HELP to be the help library directory, i.e.
$!	$ DEFINE/SYSTEM BULL$HELP SYSD$:[NEWDIRECTORY]
$! The above line should be placed in BULLSTART.COM to be executed after
$! every system reboot.
$!
$ IF F$SEARCH("SYS$HELP:BULL.HLB") .NES. "" THEN LIB/DELETE=*/HELP SYS$HELP:BULL
$ IF F$SEARCH("SYS$HELP:BULL.HLB") .EQS. "" THEN LIB/CREATE/HELP SYS$HELP:BULL
$ LIB/HELP SYS$HELP:BULL BULLCOMS
$ LIB/HELP SYS$HELP:HELPLIB BULLETIN
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
$copy sys$input INSTRUCT.TXT
$deck
This message is being displayed by the BULLETIN facility.  This is a non-DEC
facility, so it is not described in the manuals.  Messages can be submitted by
using the BULLETIN command.  System messages, such as this one, are displayed
in full, but can only be entered by privileged users.  Non-system messages can
be entered by anyone, but only their topics will be displayed at login time,
and will be prompted to optionally read them.  (This prompting feature can be
disabled).  All bulletins can be reread at any time unless they are deleted or
expire.  For more information, see the on-line help (via HELP BULLETIN). 
$eod 
$copy sys$input LOGIN.COM
$deck
$!
$! Note: The command prompt when executing the utility is named after
$! the executable image.  Thus, as it is presently set up, the prompt
$! will be "BULLETIN>".  DO NOT make the command that executes the
$! image different from the image name, or certain things will break.
$! If you wish bulletins to be displayed upon logging in starting with
$! oldest rather than newest, change BULLETIN/LOGIN to BULLETIN/LOGIN/REVERSE. 
$!
$ BULL*ETIN :== $SYS$SYSTEM:BULLETIN
$ BULLETIN/LOGIN
$eod 
$copy sys$input NONSYSTEM.TXT
$deck
Non-system bulletins (such as this) can be submitted by any user.  Users are
alerted at login time that new non-system bulletins have been added, but only
their topics are listed.  Optionally, users can be prompted at login time to
see if they wish to read the bulletins.  When reading the bulletins in this
manner, the bulletins can optionally be written to a file.  If you have the
subdirectory [.BULL] created, BULLETIN will use that directory as the default
directory to write the file into.

A user can disable this prompting featuring by using BULLETIN as follows: 

$ BULLETIN
BULLETIN> SET NOREADNEW
BULLETIN> EXIT

Afterwords, the user will only be alerted of the bulletins, and will have to
use the BULLETIN utility in order to read the messages.
$eod 
