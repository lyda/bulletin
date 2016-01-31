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
   BULL_HELP should be defined to be the directory where the library is
   to be found.)

3) LOGIN.COM
   This contains the commands that should be executed at login time
   by SYS$MANAGER:SYLOGIN.COM.  It defines the BULLETIN commands.
   It also executes the command BULLETIN/LOGIN in order to notify
   the user of new messages.  NOTE: If you wish the utility to be a
   different name than BULLETIN, you should modify this procedure.
   The prompt which the utility uses is named after image executable.
   If you want messages displayed upon logging in starting from
   oldest to newest (rather than newest to oldest), add /REVERSE to
   the BULLETIN/LOGIN command.  (WARNING: Finding the newest unread
   message is quicker than finding the oldest unread message.  This
   is not a problem if the number of messages is small.  However,
   if you plan on having lots of messages, and your system is heavily
   loaded, you may want to avoid /REVERSE.  Trial & error is the only
   way to find out if this is a problem!) Note that users with the DISMAIL
   flag setting in the authorization file will not be notified of
   new messages.  See help on the SET LOGIN command within the BULLETIN
   utility for more information on this.

   If you want SYSTEM messages, i.e. messages which are displayed in full
   when logging in, to be continually displayed for a period of time rather
   than just once, you should add the /SYSTEM= qualifier.  This is documented
   in BULLETIN.HLP, although there it is referred to only with respect to
   a user wanting to review system messages.  It can be added with /LOGIN.
   (This is a new feature added as of Version 1.5).

4) BULLSTART.COM
   This procedure contains the commands that should be executed after
   a system startup.  It should be executed by SYS$MANAGER:SYSTARTUP.COM.
   It installs the BULLETIN utility with correct privileges.  It also
   includes the command BULLETIN/STARTUP.  This starts up a detached process
   with the name BULLCP.  It periodically check for expire messages, cleanup
   empty space in files, and converts BBOARD mail to messages.  It also allows
   other DECNET nodes to share it's folders.  If you don't want this feature
   and don't plan on having multiple folders or make use of BBOARD, you could
   eliminate this command if you like.

   If you are installing BULLETIN on a cluster and plan to have the bulletin
   files be shared between all of the cluster nodes, you only need to have
   this process running on one node. On all other nodes, the system logical
   name BULL_BULLCP should be defined (to anything you want) so as to notify
   BULLETIN that BULLCP is running. (On the local node where BULLCP is running,
   this logical name is automatically defined.)

5) BULLETIN.COM
   If one wants BULLETIN to be able to send messages to other DECNET
   node's GENERAL folder, but wants to avoid running the process created
   by BULLETIN/STARTUP on this node, another method exists.  This is the
   "older" (and slower) method.  BULLETIN.COM must be put in each node's
   DECNET default user's directory (usually [DECNET]).  Once this is done,
   the /NODE qualifier for the ADD & DELETE commands can be used.
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

   To use this feature, place the special command procedure into the
   bulletin file directory using the name BOARD_SPECIAL.COM.  If you want
   to have several different special procedure, you should name the command
   procedure after the username specified by the SET BBOARD command.
$eod 
$copy sys$input BULLDIR.INC
$deck
	PARAMETER DIR_RECORD_LENGTH = 115

	COMMON /BULL_DIR/ DESCRIP,FROM,DATE,TIME,LENGTH,EXDATE,EXTIME
     &	,SYSTEM,BLOCK,NEWEST_EXDATE,NEWEST_EXTIME,NEWEST_DATE,NEWEST_TIME
     &	,NBULL,NBLOCK,SHUTDOWN,SHUTDOWN_DATE,SHUTDOWN_TIME,NEMPTY
	CHARACTER*53 DESCRIP
	CHARACTER*12 FROM
	CHARACTER*11 DATE,EXDATE,NEWEST_EXDATE,NEWEST_DATE,SHUTDOWN_DATE
	CHARACTER*8 TIME,EXTIME,NEWEST_EXTIME,NEWEST_TIME,SHUTDOWN_TIME
	LOGICAL SYSTEM

	CHARACTER*(DIR_RECORD_LENGTH) BULLDIR_ENTRY
	EQUIVALENCE (DESCRIP,BULLDIR_ENTRY)

	CHARACTER*(DIR_RECORD_LENGTH) BULLDIR_HEADER
	EQUIVALENCE (NEWEST_EXDATE,BULLDIR_HEADER)

	CHARACTER*116 BULLDIR_COM		! This value + 12 must be
	EQUIVALENCE (DESCRIP,BULLDIR_COM)	! divisable by 4
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
2 /PAGE
 /[NO]PAGE

Specifies whether BULLETIN will stop outputting when it displays a full
screen or not.  /PAGE is the default.  If /NOPAGE is specified, any
output will continue until it finishes.  This is useful if you have a
terminal which can store several screenfuls of display in it's memory.
2 /STARTUP
Starts up a detached process which will periodically check for expired
messages, cleanup empty space in files, and convert BBOARD mail to
messages.  This is recommended to avoid delays when invoking BULLETIN.
It will create a process with the name BULLCP.  For clusters, this
need be done only on one node.  On all other nodes, the system logical
name BULL_BULLCP should be defined (to anything) in order that BULLETIN
is aware that it is running on another node. (On the local node where
BULLCP is running, this logical name is automatically defined.)
2 /SYSTEM
   /SYSTEM=[days]

Displays system messages that have been recently added.  The default is
to show the messages that were added during the last 7 days.  This can
be modified by specifying the number of days as the parameter.
This command is useful for easily redisplaying system messages that
might have been missed upon logging in (or were broadcasted but were
erased from the screen.)
$eod 
$copy sys$input BULLETIN.LNK
$deck
$ LINK/NOTRACE BULLETIN,BULLETIN0,BULLETIN1,BULLETIN2,BULLETIN3,-
BULLETIN4,BULLETIN5,BULLETIN6,BULLETIN7,BULLETIN8,-
BULLCOM,BULLMAIN,ALLMACS,SYS$SYSTEM:SYS.STB/SEL/NOUSERLIB
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
C  NOTE THAT EITHER THE BBOARD ACCOUNT MUST HAVE ACCESS TO THIS DIRECTORY,
C  OR THE BBOARD ACCOUNTS MUST BE GIVEN SYSPRV PRIVILEGES TO BE ABLE
C  TO WRITE INTO THIS DIRECTORY.  ALSO, FOR BBOARD TO WORK, MAKE SURE
C  THAT THE SUBPROCESS LIMIT FOR USERS IS AT LEAST 2.  YOU ALSO MAY HAVE
C  TO INCREASE SOME SUBPROCESS SYSTEM PARAMETERS: PQL_DPGFLQUOTA AND
C  PQL_DWSQUOTA MAY HAVE TO BE CHANGED. (10000 AND 500 ARE TYPICAL).
C  (NOTE: ACCESS CAN BE GIVEN TO THE DIRECTORY FOR THE BBOARD ACCOUNT USING
C  ACLS, I.E. " SET ACL/ACL=(ID=bboard,ACCESS=R+W)/OBJ=FILE directory.DIR")
C
	COMMON /FILES/ BULLDIR_FILE,BULLETIN_FILE,BULLUSER_FILE
	COMMON /FILES/ BULLFOLDER_FILE,FOLDER_DIRECTORY,BBOARD_DIRECTORY
	COMMON /FILES/ BULLINF_FILE
	CHARACTER*80 BULLUSER_FILE /'BULL_DIR:BULLUSER.DAT'/
	CHARACTER*80 BULLFOLDER_FILE /'BULL_DIR:BULLFOLDER.DAT'/
	CHARACTER*80 BULLINF_FILE /'BULL_DIR:BULLINF.DAT'/
	CHARACTER*80 FOLDER_DIRECTORY /'BULL_DIR:'/
	CHARACTER*80 BBOARD_DIRECTORY /'BULL_DIR:'/
C
C  THE FOLLOWING 2 FILES ARE OBSOLETE AS OF V1.1 AND NO LONGER HAVE TO
C  BE SPECIFIED.  BULLETIN NOW TREATS THE GENERAL FOLDER AS ANY OTHER
C  FOLDER.  NEW USERS SHOULD JUST LEAVE THEM ALONE.  HOWEVER, USERS
C  USING OLDER VERSIONS STILL HAVE TO SPECIFY THEM IN ORDER THAT
C  BULLETIN KNOWS THE NAMES IN ORDER TO RENAME THEM.
C
	CHARACTER*80 BULLDIR_FILE /'BULL_DIR:BULLDIR.DAT'/
	CHARACTER*80 BULLETIN_FILE /'BULL_DIR:BULLETIN.DAT'/
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

	PARAMETER FOLDER_FMT = '(A25,A4,A12,A80,A12,3A4,A8,5A4)'
	PARAMETER FOLDER_RECORD = 173

	COMMON /BULL_FOLDER/ FOLDER,FOLDER_NUMBER,FOLDER_OWNER,
     &		FOLDER_DESCRIP,FOLDER_BBOARD,FOLDER_BBEXPIRE,
     &		USERB,GROUPB,ACCOUNTB,
     &		F_NBULL,F_NEWEST_BTIM,FOLDER_FLAG,F_EXPIRE_LIMIT,
     &		FOLDER_FILE,FOLDER_SET
	INTEGER F_NEWEST_BTIM(2)
	LOGICAL FOLDER_SET
	DATA FOLDER_SET /.FALSE./, FOLDER/'GENERAL'/
	CHARACTER FOLDER_OWNER*12,FOLDER*25,ACCOUNTB*8
	CHARACTER FOLDER_FILE*80,FOLDER_DESCRIP*80,FOLDER_BBOARD*12

	CHARACTER*(FOLDER_RECORD) FOLDER_COM
	EQUIVALENCE (FOLDER,FOLDER_COM)

	CHARACTER*20 REMOTE_FOLDER_COM
	EQUIVALENCE (F_NBULL,REMOTE_FOLDER_COM)

	COMMON /BULL_FOLDER1/ FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,
     &		FOLDER1_DESCRIP,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,
     &		USERB1,GROUPB1,ACCOUNTB1,
     &		F1_NBULL,F1_NEWEST_BTIM,FOLDER1_FLAG,F1_EXPIRE_LIMIT,
     &		FOLDER1_FILE
	CHARACTER FOLDER1_OWNER*12,FOLDER1*25,ACCOUNTB1*8
	CHARACTER FOLDER1_FILE*80,FOLDER1_DESCRIP*80,FOLDER1_BBOARD*12
	INTEGER F1_NEWEST_BTIM(2)

	CHARACTER*(FOLDER_RECORD) FOLDER1_COM
	EQUIVALENCE (FOLDER1,FOLDER1_COM)

	CHARACTER*20 REMOTE_FOLDER_COM1
	EQUIVALENCE (F1_NBULL,REMOTE_FOLDER_COM1)
C
C Following is used for folder directory display. Must be multiple of 4.
C The next time the the folder file is modified, it should be made a
C multiple of 4, and then this variable can be deleted.
C
	CHARACTER*240 FOLDER1_INFO
	EQUIVALENCE (FOLDER1_INFO,FOLDER1_COM)
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

	PARAMETER USER_RECORD_LENGTH = 28 + FLONG*16
	PARAMETER USER_FMT = '(A12,<4+FLONG*4>A4)'
	PARAMETER USER_HEADER_KEY = '            '

	COMMON /HEADER_INFO/ TEMP_USER,BBOARD_BTIM,NEWEST_BTIM,USERPRIV
	COMMON /HEADER_INFO/ SET_FLAG_DEF,BRIEF_FLAG_DEF
	COMMON /HEADER_INFO/ NOTIFY_FLAG_DEF
	CHARACTER TEMP_USER*12
	DIMENSION BBOARD_BTIM(2),NEWEST_BTIM(2),USERPRIV(FLONG)
	DIMENSION SET_FLAG_DEF(FLONG),BRIEF_FLAG_DEF(FLONG)
	DIMENSION NOTIFY_FLAG_DEF(FLONG)

	COMMON /BULL_USER/ USERNAME,LOGIN_BTIM,READ_BTIM,
     &		NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	CHARACTER*12 USERNAME
	DIMENSION LOGIN_BTIM(2),READ_BTIM(2)
	DIMENSION NEW_FLAG(FLONG)   ! Bit set indicates new message in folder
	DIMENSION SET_FLAG(FLONG)   ! Bit set indicates READNEW set for folder
	DIMENSION BRIEF_FLAG(FLONG) ! Bit set indicates READNEW/BRIEF set
	DIMENSION NOTIFY_FLAG(FLONG)! Bit set indicates to broadcast
				    ! notification when new bulletin is added.

	CHARACTER*(USER_RECORD_LENGTH) USER_ENTRY,USER_HEADER
	EQUIVALENCE (USER_ENTRY,USERNAME)
	EQUIVALENCE (USER_HEADER,TEMP_USER)

	COMMON /FOLDER_TIMES/ LAST_READ_BTIM(2,0:FOLDER_MAX)
	   ! Last read times for each folder as stored in SYS$LOGIN:BULLETIN.INF

	COMMON /NEW_MESSAGES/ NEW_MSG
	DIMENSION NEW_MSG(FLONG)   ! Flag showing new messages detected
$eod 
$copy sys$input HANDOUT.TXT
$deck
               Introduction to BULLETIN on the Vax
                                                  2/88 AW

PUBLISHED BY THE DREW UNIVERSITY ACADEMIC COMPUTER CENTER. MAY BE
COPIED WITH WRITING CREDIT GIVEN TO DREW UNIVERSITY.

BULLETIN was written for the Public Domain by Mark London at MIT.

     The BULLETIN utility permits a user to create messages for
reading by other users.  Users may be notified upon logging on
that new messages have been added, and what the topic of the
messages are.  Actual reading of the messages is optional.  (See
the command SET READNEW for info on automatic reading.)  Messages
are automatically deleted when their expiration data has passed.
     The program runs like VAX mail.  The different interest
groups or BULLETIN boards are implemented in the form of
'Folders', just like a filing cabinet.  A Folder contain various
messages on the same general topic.  A message is a piece of text
written by a user or staff person and added to a particular
folder.  All users are not permitted to submit messages to all
folders.

     A message consists of an expiration date, a subject line
and the text of the message.  BULLETIN will prompt the user for
these things when a message is being added.

     Several different folders are currently defined to
BULLETIN.  The General Folders will be used by Computer Center
Staff to post messages of general interest concerning the VAX to
the user community.  If something is of an important nature, it
will be posted in the General folder as a 'System' message.
This is a special message type.  It will be displayed to each
user  as they log in the first time after that message was
posted.  This will be done automatically by BULLETIN on login.
Once a particular system message has been displayed, it will not
be displayed for that user on subsequent logins.

Folders

     Different folders have been created to contain messages on
different topics.  Folders may be public, semi-private, or
private.  The majority of the folders will be public.  However a
few will be semi-private, which will mean that all users may
read messages in the folder but not all will be able to post to
it.  Currently, there are several folders defined:

GENERAL -- system messages

PUBLIC_ANNOUNCEMENTS -- Can be used by anyone to post messages
of interest to the public

On Beta:
AIDE STATION -- Private folder for Computer Center Employees

In addition on Alpha there are folders that receive electronic
magazines, such as:
NETMONTH --  The monthly magazine of BITNET information.
RISKS -- Identifying the risks involved in using computers.
INFOIBMPC -- Information about the IBM personal computers.
INFOVAX -- Information on the Digital VAX.
PROGRAMMING_JOURNALS-Includes MINIX, UNIX and C, Modula-2 and
Prolog journals
watch for new ones being added.

Using BULLETIN

     BULLETIN is invoked by type the command 'BULLETIN' (or BULL,
for short) at the '$' prompt.  BULLETIN will display its prompt
'BULLETIN>'. Help is available from DCL command level ($) or from
within the BULLETIN program itself by typing the word 'HELP'.  To
leave the BULLETIN program, type 'EXIT'.

To see what is there

     In order to see message and folders, on can use the
'Directory' command. Upon entering BULLETIN, the user is place
in the General folder.  If the user wishes to see which folders
exist, the directory/folders command is used. for example:
typing:

     BULLETIN> directory/folders

will make a display like:

      Folder                       Owner
     *GENERAL                      SYSTEM
     *PUBLIC_ANNOUNCEMENTS         BBEYER
      NETMONTH                     BITNET
     *VAX_SIG                      BBEYER

An asterisk (*) next to the folder name indicates you have unread
messages in that folder.

The command 'DIRECTORY/FOLDERS/DESCRIBE' would list all available
folders, along with a brief description of each.

     To switch from one folder to another folder, the user may
execute the 'SELECT' command.  For example, the following
command would show what a user would do to switch to the folder
called PUBLIC_ANNOUNCEMENTS:

BULLETIN> SELECT PUBLIC_ANNOUNCEMENTS

and BULLETIN would respond:
     Folder has been set to PUBLIC_ANNOUNCEMENTS

     Now the user may get a list of the messages in this folder
by issuing the directory command with no qualifiers.
This command, for example:
BULLETIN> DIRECTORY
would have bulletin respond:

 #     Description               From                  Date
 1     CHRISTMAS PARTY           oleksiak              26-JUN-88
 2     Learning about BULLETIN   oleksiak              26-JUN-87
 3     VAX MAIL                  LLLOYD                01-Jan-87

     The command 'DIR/NEW' will list just unread messages.


Reading messages

     In order to read messages in a folder, the user may type
the read command or he/she may simply type the number of the
message he wishes to read.  The message numbers can be acquired
by doing the 'DIRECTORY' command.  If the user hits a carriage
return with no input whatsoever,  BULLETIN will type the first
message in the folder, or if there are new messages present, it
will type the first new message in the folder.

     If a folder contains the above messages (as seen by the
'Directory' command) then these messages can be read by:

BULLETIN> READ
and BULLETIN would respond:

Message number:  1                       PUBLIC_ANNOUNCEMENTS
Description: CHRISTMAS PARTY
Date:  26-JUN-1988 8:08:40   Expires:  1-JAN-1989 08:08:40

...Body of message.....

     Should the user only wish to see message number 3, he can
enter the 'READ' command with the message number as a parameter.
for example:

BULLETIN> READ 3

     There are three other useful commands that can be used at
the 'BULLETIN>' prompt when reading messages. These are:

BACK - Read the message preceding the message currently being
read.

CURRENT - Start reading the current message at the top.  This is
useful for someone who is reading a message and wishes to reread
it from the beginning.

NEXT - Start reading from the beginning of the next message.
This is handy if the user is reading a very long message and
wants to skip to the next one.

Saving the interesting stuff.

     If the user sees something which he/she wants a copy of,
the extract command can be use to write an ASCII copy of the
message into a file.  This command works on the current message
being read.  It requires the name of the file into which to save
the message.  If the file name is not given, the user will be
prompted for it.  For example:

BULLETIN>  Read 2

********** Message on Screen ********

A person could then type
BULLETIN> extract
file:  FV.TXT
BULLETIN>

BULLETIN has now saved the contents of message number 2 into the
file name 'FV.txt'.
     If the file to which the user is writing already exists,
BULLETIN will append the message to the file.  The user can
force BULLETIN to write a new file containing only the message
being saved by using the '/new' qualifier in the 'extract'
command.  These messages can then be sent to other users, or
downloaded for use in Wordperfect.  (See "Mail on the Vax", or
"Transferring a file between a PC and the VAX").

This command may be useful if you wish to transfer the message to
your PC, perhaps using a BITNET journal message as a reference in
a paper. Once the file is saved, you can transfer it to a PC by
following the instructions in the handout 'Transferring files
from the PC to the VAX of from the VAX to a PC".

Adding messages
     A user may add a message to a folder by selecting the
folder and then using the 'ADD' command.  This is provided that
the user is adding the message to a public folder.  The user has
the option of giving the 'ADD' command and typing a message using
the VAX editor or uploading a message from your PC (see
documentation), or add a message you have extracted from VAX
mail.  BULLETIN will prompt for the expiration date and subject
line.  It will then add the text of the file as the body of the
message. To add a message that is stored in a file (from MAIL or
from your PC, for example) type:

          ADD filename

If the user does not specify a file name, he/she will be
prompted to enter the body of the message.  The user may also
use the EDT text editor by issuing the command with the
'/EDIT'option.

For example:
BULLETIN> sel PUBLIC_ANNOUNCEMENTS
          folder has been set to PUBLIC_ANNOUNCEMENTS
BULLETIN> ADD MESS.TXT

IT IS 10-JUL-1988 12:41:06.15.  SPECIFY WHEN THE MESSAGE SHOULD
EXPIRE:  ENTER ABsolute TIME:  <DD-MMM-YYYY]HH:MM:SS OR DELTA
TIME: DDD HH:MM:SS

A user then type the date of expiration and press the 'return'
button.  The time input may be ignored. For example, typing:
20-JUL-1988 or type "10" - for ten days in the future.

BULLETIN responds:
ENTER DESCRIPTION HEADER.  LIMIT HEADER TO 53 CHARACTERS.

Now the user may enter the subject of the message.

BULLETIN>

The above session adds the text in the file 'mess.txt' as the
next message in the PUBLIC_ANNOUNCEMENTS Folder.  The message
will be deleted automatically on the 20th of July as requested
by the user adding the message.

Asking BULLETIN to notify you of new messages upon logging in.

     If the user wishes to get notification on login when new
messages are in a folder, he should use the 'READNEW' option.
This command does not force the reader to reading new messages,
only gives notification.  To do this, 'SELECT' each folder you
are interested in and do a 'SET READNEW' command while set to
that folder.

Example:

BULLETIN> Select PUBLIC_ANNOUNCEMENTS
folder has been set to PUBLIC_ANNOUNCEMENTS
BULLETIN> SET READNEW

Alternately, you may type SET SHOWNEW. This will just display a
message notifying you that there are new messages.

Mailing a BULLETIN message

     A user may directly mail another user a message found in the
BULLETIN.  While reading the message that he/she desires to send,
at the 'BULLETIN>' type 'MAIL'.  The Vax will then ask to whom
you wish to send the information too.

Check the BULLETIN DISCUSSION folder on ALPHA for new additions.
If you have comments or questions about BULLETIN, leave them
there.
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
