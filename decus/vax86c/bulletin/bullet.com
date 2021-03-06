$set nover
$copy sys$input AAAREADME.TXT
$deck
The following are instructions for creating the BULLETIN executable and 
installation of the utility.  A brief explanation of how the internals
of the BULLETIN utility works can be found in BULLETIN.TXT .  None of
the command procedures included here are sophisticated, so it is likely
that several modifications will have to be made by the installer.
The installer should enable all privileges before installation.

1) CREATE.COM
   This will compile and link the BULLETIN sources. Also, there are several
   INCLUDE files for the fortran sources (.INC files). BULLFILES.INC must first
   be modified before this procedure is run. It contains the names of data files
   which BULLETIN creates. It also includes specifications of directories used
   by the FOLDER and BBOARD features. There are also some parameters in
   BULLFOLDER.INC which you may or may not want to modify. (If you are simply
   receiving the objects, ignore this command procedure.  Use the procedure
   CREATE_NOFORT.COM.  The objects have have been compiled to use the directory
   BULLETIN$ for all data files.  You should define this as a system logical
   name pointing to the directory which you plan to use, i.e. $ DEFINE/SYSTEM
   BULLETIN$ USRD$:[BULLETIN] .  You should also include this definition in
   BULLSTART.COM, which is mentioned below.)

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
   the user of new bulletins.  NOTE: If you wish the utility to be a
   different name than BULLETIN, you should modify this procedure.
   The prompt which the utility uses is named after image executable.
   Also, if you want bulletins displayed upon logging in starting from
   oldest to newest (rather than newest to oldest), add /REVERSE to
   the BULLETIN/LOGIN command.

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
   This procedure adds 2 permanent bulletins which give a very brief
   description about the BULLETIN utility, and how to turn off optional
   prompting of non-system bulletins (via SET NOREADNEW).

7) BOARD_SPECIAL.COM
   This command procedure describes and illustrates how to use the
   SET BBOARD/SPECIAL feature.  This feature allows the use of BBOARD
   where the input does not come from VMS MAIL.  For example, this could
   be used in the case where mail from a non-DEC network is not stored
   in the VMS MAIL.
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
$ FF[0,8] = 12			! Define a form feed character
$ SET DEFAULT BULLETIN$:	! BULLETIN looks for text in BBOARD directory
$ DELETE MFENET.MSG;*		! Delete any leftover output files.
$ OUTNAME := 'F$GETJPI("","USERNAME")'
$ IF OUTNAME .NES. "INFOMFE" THEN GOTO END	! Did user remember to SET BBOARD
				! to user MFE in BULLETIN?  If not, exit.
$ DEFINE/USER SYS$COMMAND SYS$INPUT
$ MSG				! Read MFENET mail
copy * mfenet.msg
delete *
exit
$ OPEN/READ/ERROR=EXIT INPUT MFENET.MSG
$ OUTNAME := 'F$GETJPI("","USERNAME")'
$ OUTNAME := 'OUTNAME'".TXT"	! Output file will be 'USERNAME'.TXT
$ OPEN/WRITE OUTPUT 'OUTNAME'
$ READ INPUT DATA		! Skip first line in MFENET output
$HEADER:
$ READ INPUT DATA		! Read FROM line in MFENET output
$ DATA := 'F$EXTRACT(5,F$LENGTH(DATA),DATA)
$ LEN = F$LOCATE(" ",DATA)
$ IF LEN .GT. 12 THEN LEN = 12
$ WRITE OUTPUT "From:	" + "''F$EXTRACT(0,LEN,DATA)'"
				! Write From: + TAB + USERNAME
$ WRITE OUTPUT "To:	" + "''F$GETJPI("","USERNAME")'"
				! Write To: + TAB + BBOARDUSERNAME
$ READ INPUT DATA
$ WRITE OUTPUT "Subj:	" +-	! Write Subject: + TAB + mail subject
"''F$EXTRACT(F$LOCATE(": ",DATA)+2,F$LENGTH(DATA),DATA)'"
$NEXT:
$ READ/END=END INPUT DATA	! Read and write message text
$ WRITE OUTPUT DATA
$ IF DATA .EQS. FF THEN GOTO HEADER
			! Multiple messages are seperated by form feeds
$ GOTO NEXT
$END:
$ CLOSE INPUT
$ CLOSE OUTPUT
$ DELETE MFENET.MSG;
$EXIT:
$eod 
$copy sys$input BULLCOM.CLD
$deck
 	MODULE BULLETIN_SUBCOMMANDS

	DEFINE VERB ADD
		PARAMETER P1, LABEL=FILESPEC, VALUE(TYPE=$FILE)
		QUALIFIER ALL, NONNEGATABLE
		QUALIFIER BELL, NONNEGATABLE
		QUALIFIER BROADCAST, NONNEGATABLE
		DISALLOW NOT BROADCAST AND ALL
		DISALLOW NOT BROADCAST AND BELL
		QUALIFIER EDIT, NONNEGATABLE
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
		DISALLOW FOLDER AND BULLETIN_NUMBER
	DEFINE VERB CREATE
		QUALIFIER NOTIFY, NONNEGATABLE
		QUALIFIER PRIVATE, NONNEGATABLE
		QUALIFIER READNEW, NONNEGATABLE
		QUALIFIER SEMIPRIVATE, NONNEGATABLE
		PARAMETER P1, LABEL=CREATE_FOLDER, PROMPT="Folder"
			VALUE(REQUIRED)
		DISALLOW PRIVATE AND SEMIPRIVATE
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
	DEFINE VERB EXIT
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
	DEFINE VERB MOVE
		PARAMETER P1, LABEL=FOLDER, PROMPT="Folder"
			VALUE(REQUIRED)
		QUALIFIER BULLETIN_NUMBER
		QUALIFIER NODES
		DISALLOW FOLDER AND BULLETIN_NUMBER
		DISALLOW FOLDER AND NODES
	DEFINE VERB NEXT
	DEFINE VERB PRINT
		QUALIFIER HEADER, DEFAULT
		QUALIFIER NOTIFY, DEFAULT
		QUALIFIER QUEUE, VALUE(DEFAULT=SYS$PRINT), NONNEGATABLE
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
	DEFINE VERB SEARCH
		PARAMETER P1, LABEL=SEARCH_STRING
	DEFINE VERB SELECT
		PARAMETER P1, LABEL=SELECT_FOLDER
	DEFINE VERB SET
		PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
			VALUE(REQUIRED, TYPE=SET_OPTIONS)
	DEFINE TYPE SET_OPTIONS
		KEYWORD NOBBOARD
		KEYWORD BBOARD, SYNTAX=SET_BBOARD
		KEYWORD NOREADNEW, SYNTAX=SET_NOTIFY_READNEW
		KEYWORD READNEW, SYNTAX=SET_NOTIFY_READNEW
		KEYWORD ACCESS, SYNTAX=SET_ACCESS
		KEYWORD NOACCESS, SYNTAX=SET_NOACCESS
		KEYWORD FOLDER, SYNTAX=SET_FOLDER
		KEYWORD NOTIFY, SYNTAX=SET_NOTIFY_READNEW
		KEYWORD NONOTIFY, SYNTAX=SET_NOTIFY_READNEW
		KEYWORD PRIVILEGES, SYNTAX=SET_PRIVILEGES
	DEFINE SYNTAX SET_NOTIFY_READNEW
		PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
			VALUE(REQUIRED, TYPE=SET_OPTIONS)
		QUALIFIER DEFAULT, NONNEGATABLE
	DEFINE SYNTAX SET_BBOARD
		PARAMETER P1, LABEL=SET_PARAM1, PROMPT="What"
			VALUE(REQUIRED, TYPE=SET_OPTIONS)
		PARAMETER P2, LABEL=BB_USERNAME
		QUALIFIER EXPIRATION, VALUE(TYPE=$NUMBER)
			LABEL=EXPIRATION, DEFAULT
		QUALIFIER SPECIAL, NONNEGATABLE
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
		KEYWORD FOLDER, SYNTAX=SHOW_FOLDER
		KEYWORD NOTIFY, SYNTAX=SHOW_FLAGS
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
$copy sys$input BULLCOMS.HLP
$deck
1 ADD
Adds a message to the specified folder.  A file can be specified which
contains the message.  Otherwise, BULLETIN will prompt for the text.
BULLETIN will ask for an expiration date and a header to contain the
topic of the message.

  Format:
    ADD [file-name]

All the qualifiers except for /EDIT and /NODES are restricted to users
with SETPRV privileges.
2 /ALL
This option is restricted to privileged users.  It is used in conjunction
with the /BROADCAST qualifier.  If specified, all terminals are sent the
message.  Otherwise, only users are sent the message.
2 /BELL
This option is restricted to privileged users.  It is used in conjunction 
with the /BROADCAST qualifier.  If specified, the bell is rung on the 
terminals when the message is broadcasted.
2 /BROADCAST
This option is restricted to privileged users.  If specified, message
is both stored and broadcasted to all users logged in at the time.
See also /ALL and /BELL.
2 /EDIT
Determines whether or not the editor is invoked to edit the message
you are adding.
2 /NODES=(nodes[,...])
Specifies to send the message to the listed DECNET nodes.  The BULLETIN
utility must be installed properly on the other nodes.  You can specify
a different username to use at the other nodes by either using the
USERNAME qualifier, or by specifying the nodename with 2 semi-colons
followed by the username, i.e. nodename::username.  If you specify a
username, you will be prompted for the password of the account on the
other nodes.
2 /PERMANENT
This option is restricted to privileged users for the GENERAL folder,
but available to all in other folders.  If specified, message will be a
permanent message and will never expire.
2 /SHUTDOWN
This option is restricted to privileged users.  If specified, message
will be automatically deleted after a computer shutdown has occurred.
This option is restricted to the general message file.
2 /SYSTEM
This option is restricted to privileged users.  If specified, message
is both saved in the general folder and displayed in full as a system
message when a user logs in.  System messages should be as brief as possible
to avoid the possibility that system messages could scroll off the screen.
2 /USERNAME
Specifies username to be used at remote DECNET nodes when adding messages
to DECNET nodes via the /NODE qualifier.
1 BACK
Displays the message preceding the current message.
1 BULLETIN
The BULLETIN utility permits a user to create a message for reading by
all users.  Users are notified upon logging in that new messages have
been added, and what the topic of the messages are.  Actual reading of
the messages is optional. (See the command SET READNEW for info on
automatic reading.)  Messages are automatically deleted when their
expiration date has passed.
1 COPY
Copies a message to another folder  without  deleting  it  from  the
current folder.

  Format:

    COPY folder-name
1 CREATE
Creates a folder of messages.  This is similar to the folders in the VMS
MAIL utility.  Folders are often created so that messages of a similar
topic are grouped seperately, or to restrict reading of certain messages
to specified users.  Once created, that message is automatically selected.
(see information on SELECT command).  The commands that can be used to
modify the folder's characteristics are: SET ACCESS, SET BBOARD, REMOVE.

  Format:p
    CREATE folder-name

The folder-name is limited to 25 letters and must not include spaces orh
characters that are also invalid in filenames (this is because the folder.
is stored in a file name created with the folder name). 
2 /NOTIFY 
Specifies that all users automatically have NOTIFY set for this folder.F
Only a privileged user can use this qualifier.  (See HELP SET NOTIFY for
more information.)
2 /PRIVATE
Specifies that the folder can only be accessed by users who have been 
granted access via the SET ACCESS command.  Note: This option uses ACLss
and users who are granted access must be entered into the Rights Data Base.s
If the RDB does not exist on your system, you will need this to be created
by a privileged user.  If the user is not in the RDB, this program wills
automatically enter the user into it (unless this feature was disabled s
during the compilation of this program).
2 /READNEW
Specifies that all users automatically have READNEW set for this folder.
Only a privileged user can use this qualifier.  (See HELP SET READNEW for 
more information.)
2 /SEMIPRIVATE
Similar to /PRIVATE, except that the folder is restricted only withT
respect to adding or modifying messages.  All users can read the folder.
1 CURRENTh

Displays the beginning of the message you are currently reading.  If
you  are  reading  a long message and want to display the first part
of the message again, you can enter the CURRENT command.

  Format: 

    CURRENTe
1 DELETE
Deletes the specified message.  If no message is specified, the currentY
message is deleted.  Only the original owner or a privileged user can 
delete a message.e

  Format:e
    DELETE [message-number]f

The message's relative number is found by the DIRECTORY command.
2 /NODES=(nodes[,...])
Specifies to delete the message at the listed DECNET nodes.  The BULLETINl
utility must be installed properly on the other nodes.  You can specifyf
a different username to use at the other nodes by either using the
USERNAME qualifier, or by specifying the nodename with 2 semi-colons
followed by the username, i.e. nodename::username.  If you specify a
username, you will be prompted for the password of the account on theo
other nodes.  The /SUBJECT must be specified to identify the specificI
message that is to be deleted.
2 /SUBJECT=subject
Specifies the subject of the bulletin to be deleted at a remote DECNET
node.  The DECNET node must be specified with the /NODE qualifier.
The specified subject need not be the exact subject of the message.B
It can be a substring of the subject.  This is in case you have forgottenn
the exact subject that was specified.  Case is not critical either. 
You will be notified if the deletion was successful.
2 /USERNAMEa
Specifies username to be used at remote DECNET nodes when deleting messagess
on other DECNET nodes via the /NODE qualifier.
1 DIRECTORYc
Lists a summary of the messages.  The message number, submitter's name,p
date, and subject of each message is displayed.e
2 /FOLDERS
Lists the available message folders.
2 /SINCE=datel
Displays a listing of all the messages created on or after the
specified date.  If no date is specified, the default is TODAY.t
2 /START=start-point
Indicates the first message number you want to display.  For example,e
to  display  all the messages beginning with number three, enter the
command line DIRECTORY/START=3.  Not valid with /FOLDER.
1 EXIT
Exits the BULLETIN program.e
1 FILE
Copies the current message to the named file.  The file-name parameter
is required.  If the file exists, the message is appended to the file.

  Format:!
    FILE file-name
2 /HEADERa

 /[NO]HEADER

Controls whether a header containing the owner, subject, and date of the  
message is written in the file.  The default is to write the header.
1 Folders 
All messages are divided into separate folders.  The default folder is
GENERAL, in which also is stored SYSTEM messages.  New folders can bet
created by any user.  As an example, the following creates a folder forw
GAMES related messages: 

BULLETIN> CREATE GAMES
Enter a one line description of folder.
GAMESm

To see the list of available folders, use DIRECTORY/FOLDERS.  To select 
a specific folder, use the SELECT command.  

If a user selects a folder and enters the SET READNEW command, thatl
user will be alerted of topics of new messages at login time, and will  
then be given the option of reading them.  Note, however, that the display
of topics of new GENERAL folders is not controlled by this command, ande
that READNEW is the default for the GENERAL folder. Additionally, a 
user can be immediately alerted when a new message has been added to a
folder by the SET NOTIFY command. 

A folder can be restricted to only certain users, if desired.  This is !
done by specifying CREATE/PRIVATE.  Afterwards, access to the folder is 
controller by the creator by the SET [NO]ACCESS command.
1 HELP
To obtain help on any topic, type:

	HELP  topic
1 LAST

Displays the last message in the current folder.

  Format: 
       LAST 
1 MOVE
Moves a message to another  folder and deletes it from  the  current
folder. 

  Format: 

    MOVE folder-name
1 MAIL
Invokes the VAX/VMS Personal Mail Utility (MAIL) to send the message
which you are reading to the specified recipients.

  Format:'

    MAIL recipient-nameF

The input for the recipient name is exactly the same format as used by
the MAIL utility.x
2 /SUBJECT

 /SUBJECT=text

Specifies the subject of the message for the heading.   If  the  text
consists of more than one word, enclose the text in quotation marks (").

If you omit this qualifier, the description of the message will be used.
as the subject.E
1 NEXT
Skips to the next message and displays it.  This is useful when paging
through the messages and you encounter a particularly long message
that you would like to skip over.
1 PRINT 
Queues a copy of the message you are currently reading (or have just
read)  for  printing. The PRINT command can take optional qualifiers.

   Format:

       PRINT
2 /HEADERS

 /[NO]HEADER

Controls whether a header containing the owner, subject, and date of the 	
message is printed at the beginning. The default is to write the header.
2 /NOTIFYT

 /[NO]NOTIFY

Indicates that you will be notified by a broadcast message  when  theT
file or files have been printed.  If /NONOTIFY is specified, there
is no notification.  The default is /NOTIFY.
2 /QUEUE=queue-name 
The name of the queue to which a message is to be sent.  If the /QUEUE
qualifier  is  not  specified,  the message is queued to SYS$PRINT. 
1 READ
Displays the specified message.  If you do not specify a message, then
the first time you enter the command, the first message in the folder will
be displayed.  However, if there are new messages, the first new message
will be displayed.  Each time you enter the command, the next page, or ifE
there are no more pages, the next message will be displayed. L

  Format:E
    READ [message-number]	

The message's relative number is found by the DIRECTORY command.
If you specify a number greater than the number of messages in the
folder, the last message in the folder will be displayed. 

NOTE: The READ command can be abbreviated by omitting the READ command,M
i.e. typing the command "2" is equivalent to "READ 2", and simply hittingF
the <RETURN> key is equivalent to "READ".T
2 /PAGEA

 /[NO]PAGE

Specifies that the display of the message will pause when it reaches the
end of the page.  If /NOPAGE is specified, the whole message will be
displayed.  This is useful for terminals that can store more than oneL
screenful at a time, and that have a remote printer that can then printE
the contents of the terminal's memory.
2 /SINCE=date 
Specifies to read the first message created on or after the specifiedR
date.  If no date is specified, the default is TODAY.B
1 REMOVE
Removes a folder.  Only the owner of a folder or a privileged	
user can remove the folder.E

  Format:R
    REMOVE folder-name
1 REPLACE,
Replaces or modifies existing stored message.  This is for changing part
or all of a message without causing users who have already seen theB
message to be notified of it a second time.  If the text of the messageI
is to be changed, a file can be specified which contains the text.
Otherwise, you will be promptted for the text.  The expiration info andP
header can also be changed.  If neither no qualifiers are added to the
command, it is assumed the whole message will be replaced.

  Format:R
    REPLACE [file-name]	
2 /EDIT 
Determines whether or not the editor is invoked to edit the messageO
you are replacing.  The old message text is read into the editor unless
a file-name or /NEW is specified.E
2 /EXPIRATIONN
Specifies that the message expiration date is to be replaced.N
2 /GENERAL
Specifies that the message is to be converted from a SYSTEM message to
a GENERAL message.  This only applies to the GENERAL folder.
2 /HEADERA
Specifies that the message header is to be replaced.
2 /NEW
If the editor is to be used for replacing the text of the message,
NEW specifies not to read in the old message text, and that a totally(
new text is to be read in.
2 /NUMBER=nB
Specifies the message number to be replaced.  If this qualifier is A
omitted, the message that is presently being read will be replaced.E
2 /PERMANENT
Specifies that the message is to be made permanent.M
2 /SHUTDOWNA
Specifies that the message is to expire after the next computerN
shutdown.  This only applies to general or system messages.D
2 /SYSTEM
Specifies that the message is to be made a SYSTEM message.  This is aI
privileged command and only applies to the GENERAL folder.
2 /TEXT
Specifies that the message text is to be replaced.
1 SEARCH
Searches the currently selected folder for  the  message  containing
the first occurrence of the specified text string.

   Format:

       SEARCH [search-string]T

message searches for the given search-string  in  the  currently
selected  folder.   The  search  starts  from  the  beginning of the
messages in  the  current  folder.   If  a  "search-string"  is  not
specified,  a  search  is  made for the previously specified string,
starting after the message you are currently reading (or  have  just
read).
1 SELECT
Selects a folder of messages.  See HELP Folders for a description of a
folder.  Once a folder has been selected, all commands, i.e. DIRECTORY,T
READ, etc. will apply only to those messages.  Use the CREATE command to
create a folder. Use the DIRECTORY/FOLDER command to see the list of
folders that have been created. 

 Format:

     SELECT [folder-name]A

Omitting the folder name will select the default general messages.
1 SETE
The SET command is used with other  commands  to  define  or  change
characteristics  of  the  BULLETIN  Utility.

  Format:E

    SET option
2 ACCESS
Controls access to a private folder.  A private folder can only be R
selected by users who have been granted access.  Only the owner of that 
folder is allowed to grant access.

  Format:A

    SET [NO]ACCESS id [folder-name],

The parameter "id" is the id in the system Rights Database to whichA
access is being affected.  For more infomation concerning usage of
private folders, see HELP CREATE /PRIVATE.
3 /ALL
Specifies that access to the folder is granted to all users, in otherA
words the folder is made no longer private.  /ALL is specified inP
place of the id name after the SET ACCESS command:
    SET ACCESS /ALL [folder-name]
3 /READE
Specifies that access to the folder will be limited to being able to
read the messages.
2 BBOARD
Specifies a username to be used as a BBOARD destination.  Mail which ist
sent to that user are converted into messages.  This command will applyS
to the selected folder, and each folder can have it's own BBOARD.  If	
no folder is selected, the general message file is modified.  Only P
privileged users or owners of the folders can set BBOARD. Note: TheA
specified account must have the DISUSER flag specified in the system
authorization file, and it either must be given SYSTEM privileges, orO
the scratch bboard_directory (specified when compiling BULLETIN) mustG
have world rwed protection.W

  Format:P

    SET BBOARD [username]T
3 /[NO]EXPIRATION=days
Specifies the number of days the message created by the BBOARD is to be
retained.  The default is 14 days.  The highest limit that can beF
specified is 30 days.  This can be overridden by a user with privileges.
If /NOEXPIRATION is specified, messages will become permanent.
3 /SPECIAL
Specifies that the input for incoming mail is not the normal VMS MAIL.
Specifying a username is optional.  To remove this feature, you must
either SET NOBBOARD, or SET BBOARD and specify a username.  SeeU
installation notes for how to use this feature.s
2 FOLDER
Select a folder of messages.  Identical to the SELECT command.  See help
on that command for more information. 

  Format:c

    SET FOLDER [folder-name]
2 NOTIFY
Specifies whether you will be notified via a broadcast message when ap
message is added to the selected folder.

  Format:/

    SET [NO]NOTIFY
3 /DEFAULT
Specifies that the SET [NO]NOTIFY command be applied to all users fort
the specified folder.  This is a privileged qualifier.  It will only
affect new users.j
2 PRIVILEGES
Specifies the privileges that are necessary to use privileged commands.n
Use the SHOW PRIVILEGES command to see what privileges are presently set.e
This is a privileged command.i

  Format:

    SET PRIVILEGES privilege-listl

Privilege-list is the list of privileges seperated by commas. 
To remove a privilege, specify the privilege preceeded by "NO".s
2 READNEWr
Controls whether you will be prompted upon logging in if you wish to read 
new non-system or folder messages (if any exist).  The default is that you
are prompted.  In order to apply this to a specific folder, first select
the folder (using the SELECT command), and then enter the READNEW command.
For messages in folders other than the GENERAL folder, both prompting 
and display of topics of new messages are controlled by this command.e

  Format:h

    SET [NO]READNEWR
3 /DEFAULT
Specifies that the SET [NO]READNEW command be applied to all users for
the specified folder.  This is a privileged qualifier.  It will only
affect new users
1 SHOW
The SHOW command displays information about certain characteristics.
2 FOLDER
Shows information about a folder of messages.  Owner and description are
shown.  If the folder name is omitted, and a folder has been selected via
the SELECT command, information about that folder is shown.f

  Format:e

    SHOW FOLDER [folder-name]r
3 /FULL 
Controls whether the access list and the BBOARD information for thet
folder is displayed.  This infomation is only those who have access to
that message.e
2 NOTIFY
Shows whether NOTIFY has been set for this folder. (See HELP SET NOTIFY). 
2 PRIVILEGES
Shows the privileges necessary to use privileged commands.
2 READNEW
Shows whether READNEW has been set for this folder. (See HELP SET READNEW).N
$eod y
$copy sys$input BULLDIR.INCe
$decko
	COMMON /BULL_DIR/ DESCRIP,FROM,DATE,TIME,LENGTH,EXDATE,EXTIME
     &	,SYSTEM,BLOCK,NEWEST_EXDATE,NEWEST_EXTIME,NEWEST_DATE,NEWEST_TIME
     &	,NBULL,NBLOCK,SHUTDOWN,SHUTDOWN_DATE,SHUTDOWN_TIME,NEMPTY
	CHARACTER*53 DESCRIPu
	CHARACTER*12 FROM
	CHARACTER*11 DATE,EXDATE,NEWEST_EXDATE,NEWEST_DATE,SHUTDOWN_DATEa
	CHARACTER*8 TIME,EXTIME,NEWEST_EXTIME,NEWEST_TIME,SHUTDOWN_TIME
	LOGICAL SYSTEMe

	CHARACTER*116 BULLDIR_COM		! This value + 12 must be
	EQUIVALENCE (DESCRIP,BULLDIR_COM)	! divisable by 4l
$eod h
$copy sys$input BULLETIN.COM
$deckF
$ DEFINE SYS$INPUT SYS$NET
$ BULLETIN
$eod s
$copy sys$input BULLETIN.HLP
$deck 
1 BULLETIN
Invokes the PFC BULLETIN Utility.  This utility is used for reading, addingg
and deleting bulletins.  Any user can submit a bulletin.  Users areh
notified at login time that new bulletins have been added and the topics ofA
those bulletins are displayed.  Reading of those bulletins is optional.o
(Use the command SET READNEW while in BULLETIN for setting automatic
reading.)  Privileged users can add system bulletins that are displayed
in full at login time.  These bulletins are also saved, and can be readc
by BULLETIN.  Bulletins are automatically deleted after a specified
expiration date, or they can manually be deleted by either the submitter
of the bulletin or a privileged user. 

 Format:

      BULLETIN

BULLETIN has an interactive help available while using the utility.m
Type HELP after invoking the BULLETIN command.
2 Descriptionc
The BULLETIN utility is a utility to display bulletins to users when
logging in.  Users are notified of bulletins only once.  They're not
forced into reading them every time they log in.  Submitting and reading
bulletins is easy to do via a utility similar to the VMS MAIL utility. m
Privileged users can create bulletins which are displayed in full.
(known as SYSTEM bulletins).  Non-privileged users can create non-SYSTEM
bulletins, but only topics are displayed at login.

Folders can be created so that bulletins pertaining to a single topice
can be placed together.  Folders can be made private so that reading
and writing is limited to only users or groups who are granted access.
Alternatively, folders can be made semi-private in that everyone is 
allowed to read them but write access is limited.m

When new non-system bulletins are displayed, an optional feature which a
user may enable will cause BULLETIN to ask whether the user wishes tod
read the new bulletins. The user can then read the bulletins (with the
ability to write any of the bulletins to a file). A user can enable theb
notification and prompting of new bulletins feature on a folder pern
folder basis.  However, the exception is bulletins submitted to thes
default GENERAL folder.  Users are always notified at login of new
bulletins in this folder, but can disable the prompting.  This is to giveo
non-privileged users some ability to force a notification of an importantd
message. 2

Bulletins have expiration dates and times, and are deleted automatically.i
Expiration dates and times can be specified in absolute or delta
notation. Privileged users can specify "SHUTDOWN" bulletins, i.e.y
bulletins that get deleted after a system shutdown has occurred. 
"PERMANENT" bulletins can also be created which never expire. 

Privileged users can broadcast their bulletin (to either all users or.
all terminals).u

A user can select, on a folder per folder basis, to have a message
broadcast to their terminal immediately notifying them when a newc
bulletin has been added. d

An optional "Bulletin Board" feature allows bulletins to be created by
users of other systems connected via networks.  A username can be 
assigned to a folder, and any mail sent to that user is converted to
bulletins and stored in that folder.  This feature originally was 
designed to duplicate the bulletin board feature that exists on some
Arpanet sites.  However, with the addition of folders, another possibleE
use is to assign an Arpanet mailing list to a folder. For example, one
could have an INFOVAX folder associated with an INFOVAX username, andA
have INFO-VAX mail sent to INFOVAX.  Users could then read the mailing
list in that folder, rather than having INFO-VAX sent to each user.h
Optionally, the input for the bulletin board can be directed to be taken
from any source other than VMS MAIL.  This might be useful if incoming
mail is stored in a different place other than VMS MAIL.

There is a feature which allows adding GENERAL non-system and system
bulletins to other DECNET nodes from within the BULLETIN the utility (seee
the ADD command).  All information about the message, such as expiration
date, are transferred to the host, thus making it more flexible than the
BBOARD method of adding bulletins.  Deletion of bulletins is alsoi
possible across DECNET.S

Bulletins can be either sent to a file, to a print queue, or mailed to
another user.e
$eod f
$copy sys$input BULLETIN.LNK
$deck
$ LINK/NOTRACE BULLETIN,BULLSUB0,BULLSUB1,BULLSUB2,BULLSUB3,-r
BULLCOM,BULLMAIN,ALLMACS,SYS$SYSTEM:SYS.STB/SELs
$eod I
$copy sys$input BULLETIN.TXT
$deckf
This file describes the general operation of the BULLETIN utility.

BULLETIN uses the following files to store its data: BULLETIN.DAT, BULLDIR.DAT,s
BULLUSER.DAT, & BULLFOLDER.DAT.  Also, each folder has it's own corresponding,
equivalent of BULLETIN.DAT and BULLDIR.DAT, although they are named with the
folder name as the prefix, and the suffixes of BULLFIL and BULLDIR. 
These files are opened with the shared attribute as much as possible to allowb
simultaneous operations on the files.  However, when a bulletin is added ort
deleted, the file cannot be shared, as this might cause the file to be
corrupted.  Because of this problem, files are closed as soon as possible so
that it may be quickly opened for adding and deleting files. During read
operations, the information is passed to temporary storage, the file is closed, 
and then the information is sent to the terminal. This avoids a possible
problem where the terminal output is stopped by the user, therefore delaying
the closing of the file.  Also, the use of CTRL-Y & CTRL-C is disabled while
the file is opened to avoid lockout problems. 

BULLETIN.DAT stores the actual bulletins in a fixed 81 character length file. 
Bulletins are store sequentially datewise.  New bulletins are appended to the=
end of the file.  When a bulletin is deleted, all the following bulletins are
moved up in the file to remove the gap, and the file is then truncated to
remove the unused space.  Each line is limited to 80 characters, with the 81st
character reservered to indicate the first line of each bulletin message.f
This is reduntant information since BULLDIR.DAT also stores this information.a
This is done to provide a means to recover from corrupted files due to a
crash.

BULLDIR.DAT is a fixed record length file storing directory entries for each
bulletin in BULLETIN.DAT. Each entry contains the header information, length,]
and starting record position in BULLETIN.DAT.  The first line of BULLDIR.DAT ise
a header containing the date of the next expiration that will occur, the date 
of the latest sumbitted bulletin, the number of bulletins, and the total sizem
of BULLETIN.DAT.  The last two numbers make it easier to add bulletins. Thee
directory entries then follow, again stored sequentially datewise. e

BULLUSER.DAT is a relative indexed file, where the keyword is the username ofu
the user.  Each entry contains the latest time that the user logged in, plus
the latest time that the BULLETIN utility was used to read bulletins.  A headeri
entry with a blank username stores the latest bulletin date.  The informationH
in this file is used for checking to see if the user should be alerted to newa
bulletins or not.h

BULLFOLDER.DAT is a relative indexed file storing information about all thel
folders.  It has 2 keywords, the folder number and the folder name.s
$eod e
$copy sys$input BULLFILES.INCI
$deck 
Cm
C  THE FIRST 4 FILES ARE FILES CREATED AND USED BY BULLETIN.
C  SEE BULLETIN.TXT FOR MORE INFORMATION. SPECIFY THE DEVICE/DIRECTORY
C  IN WHICH YOU DESIRE THAT THEY BE KEPT. 
Co
C  FOLDER_DIRECTORY IS THE DIRECTORY THAT FILES FOR FOLDERS THAT
C  ARE CREATED ARE KEPT IN.  IF IT IS UNDEFINED, FOLDERS WILL NOTt
C  BE ABLE TO BE CREATED./
C]
C  BBOARD_DIRECTORY IS THE SCRATCH AREA USED BY BBOARD WHEN EXTRACTING
C  MAIL.  IF IT IS UNDEFINED, BBOARD WILL NOT BE ABLE TO BE USED.a
C  NOTE THAT EITHER THIS DIRECTORY MUST BE GIVEN WORLD READ/WRITE ACCESS,a
C  OR THE BBOARD ACCOUNTS MUST BE GIVEN SYSPRV PRIVILEGES TO BE ABLE
C  TO WRITE INTO THIS DIRECTORY.  ALSO, FOR BBOARD TO WORK, MAKE SUREc
C  THAT THE SUBPROCESS LIMIT FOR USERS IS AT LEAST 2. 
Cc
C  NOTE: DELETED SPACE IS PERIODICALLY RECLAIMED AUTOMATICALLY IN ALLE
C  FILES EXCEPT FOR BULLUSER.DAT, AS EMPTY SPACE IN THAT FILE IS VERYc
C  SLOWLY ACCUMULATED.  EMPTY SPACE CAN BE RECLAIMED BY THE FOLLOWING,
C  VMS COMMAND:  $ CONVERT BULLUSER.DAT BULLUSER.DAT
C  DOING THIS ABOUT ONCE A YEAR IS PROBABLY GOOD ENOUGH.  HOWEVER, IFe
C  YOU HAVE PERIODS OF HIGH TURNOVER OF USERS, I.E. AT THE END OF Ah
C  SCHOOL YEAR, YOU SHOULD SCHEDULE IT TO BE DONE AT THAT TIME. 
Cc
	COMMON /FILES/ BULLDIR_FILE,BULLETIN_FILE,BULLUSER_FILE
	COMMON /FILES/ BULLFOLDER_FILE,FOLDER_DIRECTORY,BBOARD_DIRECTORYI
	CHARACTER*80 BULLDIR_FILE /'BULLETIN$:BULLDIR.DAT'/
	CHARACTER*80 BULLETIN_FILE /'BULLETIN$:BULLETIN.DAT'/
	CHARACTER*80 BULLUSER_FILE /'BULLETIN$:BULLUSER.DAT'/
	CHARACTER*80 BULLFOLDER_FILE /'BULLETIN$:BULLFOLDER.DAT'/
	CHARACTER*80 FOLDER_DIRECTORY /'BULLETIN$:'/t
	CHARACTER*80 BBOARD_DIRECTORY /'BULLETIN$:'/e
$eod E
$copy sys$input BULLFOLDER.INC
$decks
!e
!  The following 2 parameters can be modified if desired before compilation.
!o
	PARAMETER BBEXPIRE_LIMIT = 30	! Maxmimum time limit in days thatl
					! BBOARDS can be set to..
	PARAMETER BBOARD_UPDATE = 15	! Number of minutes between checks
					! for new BBOARD mail. (Note: Check
					! only occurs via BULLETIN/LOGIN.
					! Check is forced via BULLETIN/BBOARD).
	PARAMETER ADDID = .TRUE.	! Allows users who are not in theB
					! rights data base to be addedr
					! according to uic number.

	PARAMETER FOLDER_FMT = '(A25,A4,A12,A80,A12,3A4,A8)' 
	PARAMETER FOLDER_RECORD = 153

	COMMON /BULL_FOLDER/ FOLDER_SET,FOLDER_OWNER,
     &		FOLDER_DESCRIP,FOLDER,FOLDER_NUMBER,FOLDER_FILE,
     &		FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTBr
	LOGICAL FOLDER_SET 
	DATA FOLDER_SET /.FALSE./, FOLDER/'GENERAL'/a
	CHARACTER FOLDER_OWNER*12,FOLDER*25,ACCOUNTB*8a
	CHARACTER FOLDER_FILE*80,FOLDER_DESCRIP*80,FOLDER_BBOARD*12

	COMMON /BULL_FOLDER1/ FOLDER1_OWNER,FOLDER1_DESCRIP,r
     &		FOLDER1,FOLDER1_NUMBER,FOLDER1_FILE,
     &		FOLDER1_BBOARD,FOLDER1_BBEXPIREo
	CHARACTER FOLDER1_OWNER*12,FOLDER1*25
	CHARACTER FOLDER1_FILE*80,FOLDER1_DESCRIP*80,FOLDER1_BBOARD*12g

	CHARACTER*120 FOLDER_COMc
	EQUIVALENCE (FOLDER1_OWNER,FOLDER_COM) 
$eod  
$copy sys$input BULLMAIN.CLD
$decks
	MODULE BULLETIN_MAINCOMMANDS 
	DEFINE VERB BULLETIN 
		QUALIFIER BBOARD
		QUALIFIER CLEANUP, LABEL=CLEANUP, VALUE(REQUIRED),
		QUALIFIER LOGINe
		QUALIFIER READNEWe
		QUALIFIER REVERSEe
$eod 
$copy sys$input BULLSTART.COMd
$decke
$ RUN SYS$SYSTEM:INSTALL
SYS$SYSTEM:BULLETIN/SHAR/OPEN/HEAD/PRIV=(OPER,SYSPRV,CMKRNL,WORLD,DETACH,PRMMBX)
/EXITC
$eod 
$copy sys$input BULLUSER.INC
$decke
	PARAMETER USER_FMT = '(A12,A11,A8,A11,A8,6A4)'e
	PARAMETER USER_HEADER = '            'e

	COMMON /HEADER_INFO/ TEMP_USER,BBOARD_DATE,BBOARD_TIME
	CHARACTER TEMP_USER*12,BBOARD_DATE*11,BBOARD_TIME*8

	COMMON /BULL_USER/ USERNAME,LOGIN_DATE,LOGIN_TIME,READ_DATE,m
     &		READ_TIME,SET_FLAG,NEW_FLAG,NOTIFY_FLAG 
	CHARACTER*12 USERNAME
	CHARACTER*11 LOGIN_DATE,READ_DATE
	CHARACTER*8 LOGIN_TIME,READ_TIME
	DIMENSION SET_FLAG(2)	! Bit set indicates READNEW set for folderR
	DIMENSION NEW_FLAG(2)	! Bit set indicates new bulletin in folderf
	DIMENSION NOTIFY_FLAG(2)! Bit set indicates to broadcast notification
				! when new bulletin is added.a
$eod i
$copy sys$input BULL_COMMAND.COM
$decke
$B:=$PFCVAX$DBC1:[LONDON.BULLETIN.NEW]BULLETIN.EXE
$ON ERROR THEN GOTO EXIT
$ON SEVERE THEN GOTO EXIT 
$ON WARNING THEN GOTO EXIT
$B/'F$PROCESS()'
$EXIT:
$LOGOUTs
$eod d
$copy sys$input CREATE.COM
$deck 
$ FORTRAN/EXTEND BULLETINi
$ FORTRAN/EXTEND BULLSUB0P
$ FORTRAN/EXTEND BULLSUB1r
$ FORTRAN/EXTEND BULLSUB2 
$ FORTRAN/EXTEND BULLSUB3a
$ MAC ALLMACS
$ SET COMMAND/OBJ BULLCOMe
$ SET COMMAND/OBJ BULLMAIN
$ @BULLETIN.LNKd
$eod s
$copy sys$input CREATE_NOFORT.COMa
$decke
$!
$! CREATE_NOFORT.COM
$! Command procedure to create bulletin executable without fortran compiler.
$!
$ RUN ASC2BINo
BULLETIN.ASC
BULLETIN.BAK
$ BACKUP BULLETIN.BAK/SAVE */NEW
$ RUN ASC2BINe
BULLSUB0.ASC
BULLSUB0.BAK
$ BACKUP BULLSUB0.BAK/SAVE */NEW
$ RUN ASC2BINs
BULLSUB1.ASC
BULLSUB1.BAK
$ BACKUP BULLSUB1.BAK/SAVE */NEW
$ RUN ASC2BINh
BULLSUB2.ASC
BULLSUB2.BAK
$ BACKUP BULLSUB2.BAK/SAVE */NEW
$ RUN ASC2BINe
BULLSUB3.ASC
BULLSUB3.BAK
$ BACKUP BULLSUB3.BAK/SAVE */NEW
$ MAC ALLMACSe
$ SET COMMAND/OBJ BULLCOMm
$ SET COMMAND/OBJ BULLMAIN
$ @BULLETIN.LNKt
$ WRITE SYS$OUTPUT "You can now delete all the .BAK and .ASC files."
$eod r
$copy sys$input INSTALL.COMb
$deckB
$ COPY BULLETIN.EXE SYS$SYSTEM:f
$ RUN SYS$SYSTEM:INSTALL
SYS$SYSTEM:BULLETIN/DELp
SYS$SYSTEM:BULLETIN/SHAR/OPEN/HEAD/PRIV=(OPER,SYSPRV,CMKRNL,WORLD,DETACH,PRMMBX)
/EXITi
$!
$! NOTE: BULLETIN requires a separate help library. If you do not wish
$! the library to be placed in SYS$HELP, modify the following lines andm
$! define the logical name BULL$HELP to be the help library directory, i.e. 
$!	$ DEFINE/SYSTEM BULL$HELP SYSD$:[NEWDIRECTORY]n
$! The above line should be placed in BULLSTART.COM to be executed after
$! every system reboot. 
$!
$ IF F$SEARCH("SYS$HELP:BULL.HLB") .NES. "" THEN LIB/DELETE=*/HELP SYS$HELP:BULL
$ IF F$SEARCH("SYS$HELP:BULL.HLB") .EQS. "" THEN LIB/CREATE/HELP SYS$HELP:BULL
$ LIB/HELP SYS$HELP:BULL BULLCOMSd
$ LIB/HELP SYS$HELP:HELPLIB BULLETIN
$eod  
$copy sys$input INSTRUCT.COM
$decke
$ BULLETIN
ADD/PERMANENT/SYSTEM INSTRUCT.TXT 
INFO ON HOW TO USE THE BULLETIN UTILITY.
ADD/PERMANENT NONSYSTEM.TXT 
INFO ON BEING PROMPTED TO READ NON-SYSTEM BULLETINS.
EXIT
$eod l
$copy sys$input INSTRUCT.TXT
$decke
This message is being displayed by the BULLETIN facility.  This is a non-DEC
facility, so it is not described in the manuals.  Messages can be submitted by
using the BULLETIN command.  System messages, such as this one, are displayeds
in full, but can only be entered by privileged users.  Non-system messages can
be entered by anyone, but only their topics will be displayed at login time,
and will be prompted to optionally read them.  (This prompting feature can be 
disabled).  All bulletins can be reread at any time unless they are deleted or
expire.  For more information, see the on-line help (via HELP BULLETIN). g
$eod o
$copy sys$input LOGIN.COML
$deck,
$!
$! Note: The command prompt when executing the utility is named afterh
$! the executable image.  Thus, as it is presently set up, the prompte
$! will be "BULLETIN>".  DO NOT make the command that executes the
$! image different from the image name, or certain things will break.t
$! If you wish bulletins to be displayed upon logging in starting with
$! oldest rather than newest, change BULLETIN/LOGIN to BULLETIN/LOGIN/REVERSE. a
$!
$ BULL*ETIN :== $SYS$SYSTEM:BULLETIN
$ BULLETIN/LOGIN
$eod l
$copy sys$input NONSYSTEM.TXT 
$deckc
Non-system bulletins (such as this) can be submitted by any user.  Users are
alerted at login time that new non-system bulletins have been added, but onlyn
their topics are listed.  Optionally, users can be prompted at login time to
see if they wish to read the bulletins.  When reading the bulletins in this 
manner, the bulletins can optionally be written to a file.  If you have thes
subdirectory [.BULL] created, BULLETIN will use that directory as the defaultt
directory to write the file into.D

A user can disable this prompting featuring by using BULLETIN as follows: 

$ BULLETIN
BULLETIN> SET NOREADNEW
BULLETIN> EXIT

Afterwords, the user will only be alerted of the bulletins, and will have to
use the BULLETIN utility in order to read the messages. 
$eod u
