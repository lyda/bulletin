$set nover
$copy/log sys$input AAAREADME.TXT
$deck
The following are instructions for creating and installing the BULLETIN
utility. None of the command procedures included here are sophisticated, so it
is likely that several modifications will have to be made by the installer.
The installer should enable all privileges before installation.

Once installation is complete, it is suggested that the installer enter
BULLETIN and read HELP FOLDERS to see the options available when creating
or modifying folders.  BULLETIN creates a default folder called GENERAL
which is a SYSTEM folder (allows messages to be posted which are displayed
in full when people login.)  This folder can be modified (name changed,
SYSTEM setting removed, etc.), but it will remain the default folder
which is selected when BULLETIN is entered, and it cannot be deleted.

One of the main uses of BULLETIN, besides storage of messages that are manually
entered by users, is storage of messages from network mailing lists.  This is
done by using the BBOARD feature, which is enabled using the SET BBOARD command
inside BULLETIN.  The alternative method is for mail messages to be written
directly by a mailing program by calling internal BULLETIN routines.  Such a
a program has been written for the popular mail utilities PMDF and MX.  If you
wish to do so for another utility, read the text file WRITEMSG.TXT.  I would be
glad to include any such programs with my distribution if you think such a
program would be of use to other users.

Responding to mail which is added via the BBOARD feature is done using
VMS MAIL.  The name of the mail protocol to use for responding by mail
can be either hardcoded by putting in BULLNEWS.INC, or by defining the
system logical name BULL_NEWS_MAILER, i.e. DEFINE BULL_NEWS_MAILER "MX%".

If for some reason this is inappropriate, you can define BULL_MAILER
to point to a command procedure, and which will be run instead of VMS MAIL.
The parameters passed to this procedure are P1 = username and P2 = subject.

1) CREATE.COM
   This will compile and link the BULLETIN sources. Also, there are several
   INCLUDE files for the fortran sources (.INC files). BULLETIN will create it's
   data files in the directory pointed to by the logical name BULL_DIR.  If you
   elect not to use this definition, BULLFILES.INC should be modified.
   Note that after this procedure compiles the sources, it puts the objects
   into an object library, and then deletes all the OBJ files in the directory.

   CREATE.COM will automatically determine if you are running on an alpha
   rather than a vax and will issue the appropriate commands for that cpu.
   Of course, separate executables are needed for the two cpus, so if your 
   site has both, you will have to run this procedure separately on each.

   NOTE 1: If you plan on using the USENET NEWS reader capability of BULLETIN,
   read NEWS.TXT for installation instructions before compiling.

   NOTE 2: The maximum number of folders for this distribution is 96 folders.
   If you wish to increase this, modify BULLUSER.INC and recompile the sources.
   When the new executable is run, it will create a new BULLUSER.DAT data file
   and rename the old one to BULLUSER.OLD.  You cannot reduce the number of
   folders.

2) INSTALL.COM
   The following procedure copies the executable image to BULL_DIR and
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
   the BULLETIN/LOGIN command.  Note that users with the DISMAIL
   flag setting in the authorization file will not be notified of
   new messages.  See help on the SET LOGIN command within the BULLETIN
   utility for more information on this.  Also, please note that when
   a brand new user to the system logins, to avoid overwhelming the new
   user with lots of messages, only PERMANENT SYSTEM messages are displayed.

   If you want SYSTEM messages, i.e. messages which are displayed in full
   when logging in, to be continually displayed for a period of time rather
   than just once, you should add the /SYSTEM= qualifier.  This is documented
   in BULLETIN.HLP, although there it is referred to only with respect to
   a user wanting to review system messages.  It can be added with /LOGIN.

   DECWINDOWS users should note the following: Both SYLOGIN and LOGIN are
   executed twice, once before the terminal is actually created, while
   SYS$OUTPUT is still a mailbox, the other time after the terminal is
   created.  To avoid this, place the following code in both procedure. 
   It causes them to execute only when the output is a terminal. This code
   also helps to allow programs to be placed in LOGIN.COM that prompt for
   terminal input.  BULLETIN does this if you select READNEW mode for
   displaying messages when logging in, as READNEW mode will ask you if
   you want to display the messages text.  Attempts to read terminal input
   under DECWINDOWS when SYS$OUTPUT is still a mailbox will cause DECTERM
   creation to fail.  (This problem is fixed under MOTIF).

   $ IF F$LOCATE("_TW",F$GETJPI("","PRCNAM")) .NE. 0 THEN GOTO START
   $ IF "''F$MODE()'" .NES. "INTERACTIVE" THEN GOTO START
   $ IF F$GETDVI("SYS$OUTPUT","TRM") THEN GOTO START
   $ GOTO FINISH
   $START:
   .
   .
   body of SYLOGIN.COM (including BULLETIN command)
   .
   .
   $FINISH:
   $ EXIT

4) BULLSTART.COM
   This procedure contains the commands that should be executed after
   a system startup.  It should be executed by SYS$MANAGER:SYSTARTUP.COM.
   It installs the BULLETIN utility with correct privileges.  It also
   includes the command BULLETIN/STARTUP.  This starts up a detached process
   with the name BULLCP.  It periodically checks for expire messages,cleanups
   empty space in files, and converts BBOARD mail to messages.  It also allows
   other DECNET nodes to share it's folders.  If you don't want this feature
   and don't plan on having multiple folders or make use of BBOARD, you could
   eliminate this command if you like.  However, it is highly recommended that
   you create this process to avoid extra overhead when users login.  NOTE:
   BULLCP normally is created so it is owned by the DECNET account.  If that
   account does not exist, BULLCP will be owned by the account that issues
   the BULLETIN/START command.  In that case, access via other DECNET nodes
   will not be available.

   If you are installing BULLETIN on a cluster and plan to have the bulletin
   files be shared between all of the cluster nodes, you only need to have
   this process running on one node. On all other nodes, the system logical
   name BULL_BULLCP should be defined (to anything you want) so as to notify
   BULLETIN that BULLCP is running. (On the local node where BULLCP is running,
   this logical name is automatically defined.)

   The system logical name BULL_CUSTOM can be defined to enable several 
   features.  It is equated to a hex number string.  
	Bit 0 set = need privileges to create folder.
	    1 set = captive account can write files.
	    2 set = captive account can use editor. 
   
   If you want to have more than one database, you can do so by redefining 
   BULL_DIR to another directory.  However, only directories that are 
   defined in the list of equivalence names pointed to by the system logical 
   name BULL_DIR_LIST are allowed.  For example:

	DEFINE/SYSTEM BULL_DIR_LIST SITE$ROOT:[SYSEXE],USER1:[MRL]

   Then BULL_DIR can be defined as SITE$ROOT:[SYSEXE] or USER1:[MRL].  
   BULL_DIR_LIST must be defined on all nodes in a cluster.
   
   The use of the MARK command to mark messages require that a file be
   created for each user which saves the marked info.  That file file is
   stored in the directory pointed to by the logical name BULL_MARK.  You can
   either let users who want to use this command define it themselves, or
   you can define it for them, i.e. DEFINE/SYSTEM BULL_MARK SYS$LOGIN.

5) INSTRUCT.COM
   This procedure adds 2 permanent messages which give a very brief
   description about the BULLETIN utility, and how to turn off optional
   prompting of non-system messages (via SET NOREADNEW).

6) BOARD_SPECIAL.COM
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

7) UPGRADE.COM
   This procedure is used to upgrade to a new version of BULLETIN.
   See comments for instructions.

8) MASTER.COM
   If you are using PMDF, and want to use the BBOARD option, a set of
   routines are included which will allow PMDF to write message directly
   into folders, which is a much more effecient way of doing it than
   the normal BBOARD method of using VMS MAIL.  Read PMDF.TXT for how
   to do this.

9) OPTIMIZE_RMS.COM
   This routine optimizes index files.  To run, type @OPTIMIZE_RMS.COM
   followed by the filename.  If you omit the filename, it will prompt
   you to allow you to turn off or on several different types of RMS
   compression.  The default is to turn on all types of compression.
   The optimization will cause the file to be compressed.

   If you use the NEWS feature, it is suggest that you run this procedure
   on BULLNEWS.DAT after it is created.  Compressing that file greatly speeds
   up the NEWS update process.  If you are tight on space, and have been
   running BULLETIN for a long time, it might also be useful to compress
   BULLINF.DAT if that file is very large.  However, compressing that (or
   the other BULLETIN data files) don't appear to save any execution time,
   unlike BULLNEWS.DAT.
$eod 
$copy/log sys$input BULLDIR.INC
$deck
	PARAMETER DIR_RECORD_LENGTH = (100/4)*4

	COMMON /BULL_DIR/ MSG_BTIM,MSG_NUM,DESCRIP,FROM,LENGTH,EX_BTIM
     &	,SYSTEM,BLOCK,HEADER_BTIM,HEADER_NUM,NEWEST_EXBTIM,NEWEST_MSGBTIM
     &	,NBULL,NBLOCK,SHUTDOWN,SHUTDOWN_BTIM,NEMPTY
     &	,DATE,TIME,EXDATE,EXTIME,NEWEST_EXDATE,NEWEST_EXTIME
     &  ,NEWEST_DATE,NEWEST_TIME,SHUTDOWN_DATE,SHUTDOWN_TIME
	CHARACTER*56 DESCRIP
	CHARACTER*12 FROM
	LOGICAL SYSTEM

	CHARACTER*12 DATE,EXDATE,NEWEST_EXDATE,NEWEST_DATE,SHUTDOWN_DATE
	CHARACTER*12 TIME,EXTIME,NEWEST_EXTIME,NEWEST_TIME,SHUTDOWN_TIME

	INTEGER MSG_BTIM(2),EX_BTIM(2),HEADER_BTIM(2)
	INTEGER NEWEST_EXBTIM(2),NEWEST_MSGBTIM(2),SHUTDOWN_BTIM(2)

	CHARACTER*(DIR_RECORD_LENGTH) BULLDIR_ENTRY
	EQUIVALENCE (MSG_BTIM,BULLDIR_ENTRY)

	CHARACTER*52 BULLDIR_HEADER
	EQUIVALENCE (HEADER_BTIM,BULLDIR_HEADER)

	DATA HEADER_BTIM/0,0/,HEADER_NUM/0/

	CHARACTER MSG_KEY*8

	EQUIVALENCE (MSG_BTIM,MSG_KEY)

	PARAMETER LINE_LENGTH=255
	PARAMETER INPUT_LENGTH=256

	COMMON /INPUT_BUFFER/ INPUT
	CHARACTER INPUT*(INPUT_LENGTH)

	PARAMETER NEWSDIR_RECORD_LENGTH = 140

	COMMON /NEWS_DIR/ NEWS_MSG_NUM,NEWS_MSG_BTIM_KEY
     &  ,NEWS_EX_BTIM_KEY,NEWS_MSGID,NEWS_POST_KEY,NEWS_BLOCK
     &  ,NEWS_LENGTH,NEWS_DESCRIP,NEWS_FROM
     &  ,NEWS_HEADER_KEY,NEWS_NEWEST_MSG_BTIM_KEY
     &  ,NEWS_HEADER_FOLDER,NEWS_NEWEST_EX_BTIM_KEY,NEWS_HEADER_NUM
     &	,NEWS_NBULL
	CHARACTER*36 NEWS_MSGID
	CHARACTER*56 NEWS_DESCRIP
	CHARACTER*12 NEWS_FROM
	CHARACTER*8 NEWS_POST_KEY

	CHARACTER*8 NEWS_MSG_BTIM_KEY,NEWS_EX_BTIM_KEY
	CHARACTER*8 NEWS_NEWEST_MSG_BTIM_KEY,NEWS_NEWEST_EX_BTIM_KEY
	INTEGER NEWS_POST_BTIM(2)

	CHARACTER*(NEWSDIR_RECORD_LENGTH) NEWSDIR_ENTRY
	EQUIVALENCE (NEWS_MSG_NUM,NEWSDIR_ENTRY)

	CHARACTER*64 NEWS_HEADER_FOLDER
	CHARACTER*(NEWSDIR_RECORD_LENGTH) NEWSDIR_HEADER
	EQUIVALENCE (NEWS_HEADER_KEY,NEWSDIR_HEADER)
$eod 
$copy/log sys$input BULLETIN.HLP
$deck
1 BULLETIN
Invokes  the  PFC  BULLETIN  Utility.  This utility is used for reading,
adding and deleting message.  Users are notified at login time that new
messages have been added and the topics of those messages are displayed.
Reading of those messages is optional.  (Use  the  command  SET  READNEW
while  in BULLETIN for setting automatic reading.)  Privileged users can
add system bulletins that are displayed in full at  login  time.   These
messages  are  also  saved,  and  can be read by BULLETIN.  Messages are
automatically deleted after a specified expiration  date,  or  they  can
manually  be  deleted  by  either  the  submitter  of  the  message or a
privileged user.

 Format:

      BULLETIN [foldername or bulletin interactive command]

BULLETIN has an interactive help  available  while  using  the  utility.
Type HELP after invoking the BULLETIN command.

If so configured, BULLETIN can also read USENET NEWS.
2 Description

The  BULLETIN  utility  is  a  utility to display messages to users when
logging in.  Users are notified of  messages  only  once.   They're  not
forced into reading them every time they log in.  Submitting and reading
messages is easy to do via a utility similar to the  VMS  MAIL  utility.
Privileged users can create messages which are displayed in full. (known
as SYSTEM  messages).   Non-privileged  users  may  be  able  to  create
non-SYSTEM  messages  (unless  your  system  manager  has  disabled  the
feature), but only topics are displayed at login.

Folders can be created so that messages pertaining to a single topic can
be  placed  together.   Folders  can be made private so that reading and
writing is limited to only users  or  groups  who  are  granted  access.
Alternatively,  folders  can  be  made  semi-private in that everyone is
allowed to read them but write access is limited.

When new non-system messages are displayed, an optional feature which  a
user  may  enable  will cause BULLETIN to ask whether the user wishes to
read the new bulletins. The user can then read the  messages  (with  the
ability  to  write any of the messages to a file). A user can enable the
notification and prompting of new  messages  feature  on  a  folder  per
folder  basis.   However,  the  exception  is  messages submitted to the
default GENERAL folder.  Users are  always  notified  at  login  of  new
bulletins  in  this  folder,  but can disable the prompting.  This is to
give non-privileged users some ability to force  a  notification  of  an
important message.

Messages have expiration dates and times, and are deleted automatically.
Expiration dates and  times  can  be  specified  in  absolute  or  delta
notation.   Privileged  users  can  specify  "SHUTDOWN"  messages,  i.e.
messages  that  get  deleted  after  a  system  shutdown  has  occurred.
"PERMANENT" messages can also be created which never expire.

Privileged users can broadcast their message (to either all users or all
terminals).

A user can select, on a folder per  folder  basis,  to  have  a  message
broadcast  to  their  terminal  immediately  notifying  them  when a new
message has been added.

An optional "Bulletin Board" feature allows messages to  be  created  by
users  of  other  systems  connected  via  networks.   A username can be
assigned to a folder, and any mail sent to that  user  is  converted  to
messages  and  stored  in  that  folder.   This  feature  originally was
designed to duplicate the message board  feature  that  exists  on  some
Arpanet  sites.  However, with the addition of folders, another possible
use is to assign an Arpanet mailing list to a folder. For  example,  one
could  have  an  INFOVAX folder associated with an INFOVAX username, and
have INFO-VAX mail sent to INFOVAX.  Users could then read  the  mailing
list  in  that  folder,  rather  than having INFO-VAX sent to each user.
Optionally, the input for the bulletin board can be directed to be taken
from  any  source other than VMS MAIL.  This might be useful if incoming
mail is stored in a different place other than VMS MAIL.

Messages can be either sent to a file, to a print queue,  or  mailed  to
another user.

BULLETIN  can  also  act a USENET NEWS reader if the appropriate network
software is available to interact with.  See the installation notes  for
more detail.
2 Parameters
The  parameter  following  the  BULLETIN  command  is interpreted as the
folder name which should be selected, rather than  the  default  GENERAL
folder.   If  the  parameter is specified with quotes ("parameter"), the
parameter is  interpreted  as  an  interactive  BULLETIN  command,  i.e.
commands  which are entered once BULLETIN is executed, i.e. "DIRECTORY",
"ADD", etc.  BULLETIN will exit immediately after entering that command,
rather than prompting for another command.  More than one command can be
specified by separating the  commands  with  semi-colons,  i.e.  "SELECT
DATA;DIR".  If the last command ends with a semi-colon, then BULLETIN 
will not exit, but instead will enter the standard interactive mode and
prompt the user for commands.
2 /EDIT
Specifies that all ADD or REPLACE commands within BULLETIN will select
the editor for inputting text.
2 /KEYPAD
 /[NO]KEYPAD
Specifies that keypad mode is to be set on, such that the keypad keys
correspond to BULLETIN commands.  The default is /KEYPAD.
2 /LIBRARY
 /LIBRARY=library

Specifies the library of folders to use.  By default there is  only  one
library.  However, the system manager may decide to create more than one
library, with each library saved in a different directory.  Use the SHOW
LIBRARY/ALL command within BULLETIN to see if there are other libraries. 
2 /PAGE
 /[NO]PAGE

Specifies  whether BULLETIN will stop outputting when it displays a full
screen or not.  /PAGE is the default.   If  /NOPAGE  is  specified,  any
output  will  continue  until it finishes.  This is useful if you have a
terminal which can store several screenfuls of display in its memory.
2 /PGFLQUOTA
   /PGFLQUOTA=pages

Used if you want to specify the page file quota for the BULLCP process.
2 /STARTUP
Starts up a detached process which will periodically check for expired
messages, cleanup empty space in files, and convert BBOARD mail to
messages.  This is recommended to avoid delays when invoking BULLETIN.
It will create a process with the name BULLCP.  For clusters, this
need be done only on one node.  On all other nodes, the system logical
name BULL_BULLCP should be defined (to anything) in order that BULLETIN
is aware that it is running on another node. (On the local node where
BULLCP is running, this logical name is automatically defined.)
2 /STOP
Stops the BULLCP process without restarting a new one.  (See /STARTUP
for information on the BULLCP process.)
2 /SYSTEM
   /SYSTEM=[days]

Displays system messages that have been recently added.  The default is
to show the messages that were added during the last 7 days.  This can
be modified by specifying the number of days as the parameter.
This command is useful for easily redisplaying system messages that
might have been missed upon logging in (or were broadcasted but were
erased from the screen.)
2 /WIDTH
   /WIDTH=page_width

Specifies the terminal width for display purposes.  This is used if your
startup procedure is configured such that BULLETIN/LOGIN is executed before
the terminal type is known, and the default width is larger than what the
terminal type actually is.  I.e. the default width might be 132, but the
real width is 80.  In that case, you should add /WIDTH=80 to BULLETIN/LOGIN.
2 /WSEXTENT
   /WSEXTENT=pages

Used if you want to specify the working set limit for the BULLCP process.
$eod 
$copy/log sys$input BULLETIN.LNK
$deck
$ ULIB = "NONE"
$ IF F$TRNLNM("MULTINET_SOCKET_LIBRARY") .NES. "" THEN GOTO LINK
$ IF F$TRNLNM("TWG$TCP") .EQS. "" THEN GOTO LINK
$ ULIB = "PROCESS"
$ DEFINE/USER LNK$LIBRARY TWG$TCP:[NETDIST.LIB]LIBNET
$ DEFINE/USER LNK$LIBRARY_1 TWG$TCP:[NETDIST.LIB]LIBNETACC
$ DEFINE/USER LNK$LIBRARY_2 TWG$TCP:[NETDIST.LIB]LIBNET
$LINK:
$ IF F$GETSYI("HW_MODEL") .GT. 1023 THEN GOTO ALINK
$ LINK/NOTRACE BULL/LIB/INC=BULLETIN$MAIN,SYS$SYSTEM:SYS.STB/SEL-
        /USERLIB='ULIB'/EXE=BULLETIN,SYS$INPUT/OPT
SYS$SHARE:VAXCRTL/SHARE
ID="V2.3"
$ EXIT
$ALINK:
$ LINK/NOTRACE/NONATIVE_ONLY BULL/LIB/INC=BULLETIN$MAIN/SYSEXE-
       /USERLIB='ULIB'/EXE=BULLETIN,SYS$SHARE:VAXCRTL/LIB,SYS$INPUT/OPT
ID="V2.3"
$eod 
$copy/log sys$input BULLFILES.INC
$deck
C
C  FOLDER_DIRECTORY IS THE DIRECTORY THAT FILES FOR FOLDERS THAT
C  ARE CREATED ARE KEPT IN.  IF YOU WISH TO PREVENT FOLDER CREATION,
C  YOU SHOULD MODIFY BULLCOM.CLD TO MAKE THE CREATE COMMAND A PRIVILEGED
C  COMMAND (OR SIMPLY REMOVE THE LINES WHICH DEFINE THE CREATE COMMAND).
C
C  BBOARD_DIRECTORY IS THE SCRATCH AREA USED BY BBOARD WHEN EXTRACTING
C  MAIL.  IF IT IS UNDEFINED, BBOARD WILL NOT BE ABLE TO BE USED.
C  NOTE THAT EITHER THE BBOARD ACCOUNTS MUST HAVE ACCESS TO THIS DIRECTORY,
C  OR THE BBOARD ACCOUNTS MUST BE GIVEN SYSPRV PRIVILEGES TO BE ABLE
C  TO WRITE INTO THIS DIRECTORY.  ALSO, FOR BBOARD TO WORK, MAKE SURE
C  THAT THE SUBPROCESS LIMIT FOR USERS IS AT LEAST 2.  YOU WILL ALSO HAVE
C  TO INCREASE THE FOLLOWING SYSTEM PARAMETERS WHICH AFFECT DETACHED PROCESES:
C  PQL_DPGFLQUOTA = 15000, PQL_DWSQUOTA = 500, & PQL_DFILLM = 30.
C  (NOTE: ACCESS CAN BE GIVEN TO THE DIRECTORY FOR THE BBOARD ACCOUNTS USING
C  ACLS, I.E. " SET ACL/ACL=(ID=bboard,ACCESS=R+W)/OBJ=FILE directory.DIR")
C
	COMMON /FILES/ BULLFOLDER_FILE,FOLDER_DIRECTORY,BBOARD_DIRECTORY
	COMMON /FILES/ BULLUSER_FILE,BULLINF_FILE,NEWS_DIRECTORY
	COMMON /FILES/ BULLNEWS_FILE
	CHARACTER*80 FOLDER_DIRECTORY /'BULL_DIR:'/
	CHARACTER*80 BBOARD_DIRECTORY /'BULL_DIR:'/
C
C  NOTE: THE FOLLOWING FILE ARE STORED IN THE FOLDER_DIRECTORY BY DEFAULT.
C  YOU CAN CHANGE THIS BY ADDING A DIRECTORY NAME TO THE FILE NAME.
C
	CHARACTER*80 BULLUSER_FILE /'BULLUSER.DAT'/	! Stores user login time
							! & folder flag settings
	CHARACTER*80 BULLFOLDER_FILE /'BULLFOLDER.DAT'/	! Stores folder data
	CHARACTER*80 BULLINF_FILE /'BULLINF.DAT'/	! Stores times of last
							! read messages of users
	CHARACTER*80 BULLNEWS_FILE /'BULLNEWS.DAT'/	! Stores news group data
C
C  THE FOLLOWING IS THE DIRECTORY THAT IS USED TO STORE LOCAL NEWS GROUPS,
C  I.E. NEWS GROUPS THAT ARE COPIED FROM THE NEWS SERVER AND SAVED LOCALLY.
C  BULLETIN WILL CREATE SUBDIRECTORIES IN THIS DIRECTORY AND THE FILES WILL
C  BE STORED IN THOSE SUBDIRECTORIES.
C
	CHARACTER*80 NEWS_DIRECTORY /'BULL_DIR:'/
$eod 
$copy/log sys$input BULLFOLDER.INC
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
					! NOT APPLICABLE IF BULLCP IS RUNNING.
	PARAMETER ADDID = .TRUE.	! Allows users who are not in the
					! rights data base to be added
					! according to uic number.

	PARAMETER FOLDER_FMT = '(A44,A4,A8,A12,A80,A12,3A4,A8,10A4)'
	PARAMETER FOLDER_RECORD = 220	! Must be multiple of 4

	COMMON /BULL_FOLDER/ FOLDER,FOLDER_NUMBER,FOLDER_CREATED_DATE,
     &		FOLDER_OWNER,
     &		FOLDER_DESCRIP,FOLDER_BBOARD,FOLDER_BBEXPIRE,
     &		USERB,GROUPB,ACCOUNTB,
     &		F_NBULL,F_NEWEST_BTIM,FOLDER_FLAG,F_EXPIRE_LIMIT,
     &		F_NEWEST_NOSYS_BTIM,F_START,F_COUNT,F_LAST,
     &		FOLDER_FILE,FOLDER_SET,FOLDER_NAME
	INTEGER F_NEWEST_BTIM(2)
	INTEGER F_NEWEST_NOSYS_BTIM(2)
	LOGICAL FOLDER_SET
	DATA FOLDER_SET /.FALSE./, FOLDER/'GENERAL'/
	CHARACTER FOLDER_OWNER*12,FOLDER*44,ACCOUNTB*8,FOLDER_NAME*80
	CHARACTER FOLDER_FILE*80,FOLDER_DESCRIP*80,FOLDER_BBOARD*12
	CHARACTER FOLDER_CREATED_DATE*8

	CHARACTER*(FOLDER_RECORD) FOLDER_COM
	EQUIVALENCE (FOLDER,FOLDER_COM)

	COMMON /BULL_FOLDER1/ FOLDER1,FOLDER1_NUMBER,FOLDER1_CREATED_DATE,
     &		FOLDER1_OWNER,
     &		FOLDER1_DESCRIP,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,
     &		USERB1,GROUPB1,ACCOUNTB1,
     &		F1_NBULL,F1_NEWEST_BTIM,FOLDER1_FLAG,F1_EXPIRE_LIMIT,
     &		F1_NEWEST_NOSYS_BTIM,F1_START,F1_COUNT,F1_LAST,
     &		FOLDER1_FILE,FOLDER1_SET,FOLDER1_NAME
	CHARACTER FOLDER1_OWNER*12,FOLDER1*44,ACCOUNTB1*8,FOLDER1_NAME*80
	CHARACTER FOLDER1_FILE*80,FOLDER1_DESCRIP*80,FOLDER1_BBOARD*12
	CHARACTER FOLDER1_CREATED_DATE*8
	INTEGER F1_NEWEST_BTIM(2)
	INTEGER F1_NEWEST_NOSYS_BTIM(2)

	CHARACTER*(FOLDER_RECORD) FOLDER1_COM
	EQUIVALENCE (FOLDER1,FOLDER1_COM)

	PARAMETER NEWS_FOLDER_FMT = '(A44,A4,2A8,A36,11A4)'
	PARAMETER NEWS_FOLDER_RECORD = 144	! Must be multiple of 4

	COMMON /NEWS_FOLDER/ NEWS_FOLDER,NEWS_FOLDER_NUMBER,
     &		NEWS_F_CREATED_DATE,NEWS_F_EXPIRED_DATE,
     &		NEWS_FOLDER_DESCRIP,NEWS_F_START,NEWS_F_COUNT,
     &		NEWS_F_NBULL,NEWS_F_NEWEST_BTIM,NEWS_F_LAST,
     &		NEWS_F_FLAG,NEWS_F_EXPIRE,NEWS_F_FIRST,
     &		NEWS_F_EXPIRE_LIMIT,NEWS_F_END 
	INTEGER NEWS_F_NEWEST_BTIM(2)
	CHARACTER NEWS_FOLDER*44
	CHARACTER NEWS_FOLDER_DESCRIP*36
	CHARACTER*8 NEWS_F_CREATED_DATE,NEWS_F_EXPIRED_DATE

	CHARACTER*(NEWS_FOLDER_RECORD) NEWS_FOLDER_COM
	EQUIVALENCE (NEWS_FOLDER,NEWS_FOLDER_COM)

        COMMON /NEWS_FOLDER_DEFAULT/ NEWS_FLAG_DEFAULT,
     &	        NEWS_EXPIRE_DEFAULT,NEWS_EXPIRE_LIMIT_DEFAULT

	COMMON /NEWS_FOLDER1/ NEWS_FOLDER1,NEWS_FOLDER1_NUMBER,
     &		NEWS_F1_CREATED_DATE,NEWS_F1_EXPIRED_DATE,
     &		NEWS_FOLDER1_DESCRIP,NEWS_F1_START,NEWS_F1_COUNT,
     &		NEWS_F1_NBULL,NEWS_F1_NEWEST_BTIM,NEWS_F1_LAST,
     &		NEWS_F1_FLAG,NEWS_F1_EXPIRE,NEWS_F1_FIRST,
     &	        NEWS_F1_EXPIRE_LIMIT,NEWS_F1_END
	INTEGER NEWS_F1_NEWEST_BTIM(2)
	CHARACTER NEWS_FOLDER1*44
	CHARACTER NEWS_FOLDER1_DESCRIP*36
	CHARACTER*8 NEWS_F1_CREATED_DATE,NEWS_F1_EXPIRED_DATE

	CHARACTER*(NEWS_FOLDER_RECORD) NEWS_FOLDER1_COM
	EQUIVALENCE (NEWS_FOLDER1,NEWS_FOLDER1_COM)
$eod 
$copy/log sys$input BULLNEWS.INC
$deck
	COMMON /NEWS_DEFAULTS/ ORGANIZATION,MAILER

	CHARACTER*132 ORGANIZATION
	DATA ORGANIZATION /'MIT PLASMA FUSION CENTER'/

	CHARACTER*12 MAILER
	DATA MAILER /'IN%'/
$eod 
$copy/log sys$input BULLUSER.INC
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
	DIMENSION NEW_FLAG(FLONG)   ! Used to indicate new message in folder
				    ! Now NEW_FLAG(2) contains SET GENERIC days
	DIMENSION SET_FLAG(FLONG)   ! Bit set indicates READNEW set for folder
	DIMENSION BRIEF_FLAG(FLONG) ! Bit set indicates READNEW/BRIEF set
	DIMENSION NOTIFY_FLAG(FLONG)! Bit set indicates to broadcast
				    ! notification when new bulletin is added.

	CHARACTER*(USER_RECORD_LENGTH) USER_ENTRY,USER_HEADER
	EQUIVALENCE (USER_ENTRY,USERNAME)
	EQUIVALENCE (USER_HEADER,TEMP_USER)

	COMMON /FOLDER_TIMES/ LAST_READ_BTIM(2,0:FOLDER_MAX)
	   ! Must start with 0 to store info for folder specified with ::
	COMMON /SYS_FOLDER_TIMES/ LAST_SYS_BTIM(2,FOLDER_MAX)
	   ! Last read times for each folder as stored in BULL_DIR:BULLINF.DAT
	COMMON /NEWS_TIMES/ LAST_NEWS_READ(2,FOLDER_MAX)
	INTEGER*2 LAST_NEWS_READ2(4,FOLDER_MAX)
	EQUIVALENCE (LAST_NEWS_READ2(1,1),LAST_NEWS_READ(1,1))
	   ! Last read times for each folder as stored in BULL_DIR:BULLINF.DAT

	COMMON /INF_REC/ INF_REC(2,FOLDER_MAX)
	INTEGER*2 INF_REC2(4,FOLDER_MAX)
	EQUIVALENCE (INF_REC2(1,1), INF_REC(1,1))

	COMMON /NEW_MESSAGES/ NEW_MSG
	DIMENSION NEW_MSG(FLONG)   ! Flag showing new messages detected
$eod 
$copy/log sys$input BULL_NEWS.C
$deck
#include <string.h>
#include <descrip.h>
#include <stdio.h>
#include "sys$library:iodef.h"

#if MULTINET

#include "multinet_root:[multinet.include.sys]types.h"
#include "multinet_root:[multinet.include.sys]socket.h"
#include "multinet_root:[multinet.include.netinet]in.h"
#include "multinet_root:[multinet.include.arpa]inet.h"
#include "multinet_root:[multinet.include]netdb.h"
#include "multinet_root:[multinet.include]errno.h"
#include "multinet_root:[multinet.include.vms]inetiodef.h"

static char inet[7] = "INET0:";
$DESCRIPTOR(inet_d,inet);

static struct dns {
	unsigned char function;
	unsigned char call_code;
	short zeros;
	short length;
	char string[512];
} buf1, buf2;

struct  sockaddr_un {
        short   sun_family;             /* AF_UNIX */
        char    sun_path[109];          /* path name (gag) */
};
#else

#if UCX

#include <ucx$inetdef.h>

struct sockaddr {
  short inet_family;
  short inet_port;
  int inet_adrs;
  char bklb[8];
  };

struct itlist { int lgth; struct sockaddr *hst; };

static short sck_parm[2];
static struct sockaddr local_host, remote_host;
struct itlist lhst_adrs, rhst_adrs;

static char ucxdev[11] = "UCX$DEVICE";
$DESCRIPTOR(ucxdev_d,ucxdev);

static int addr_buff;

#define htons(x) ((unsigned short)((x<<8)|(x>>8)))

#else

#if TWG

#include <types.h>
#include <socket.h>
#include <netdb.h>
#include <in.h>
#include <inetiodef.h>

static char inet[6] = "INET:";
$DESCRIPTOR(inet_d,inet);

#else

#define CMU 1
static char ip[4] = "IP:";
$DESCRIPTOR(ip_d,ip);

#endif

#endif

#endif

static char task[20];
$DESCRIPTOR(task_d,task);

static int s,s1;

static struct iosb {
	short status;
	short size;
	int info;
} iosb;

#define TCP 0
#define DECNET 1

static int mode = TCP;

#if MULTINET

#include <lib$routines>
#include <stdarg.h>
#ifdef __ALPHA
unsigned int __VA_COUNT_BUILTIN(void);
#define va_count(count)		(count = __VA_COUNT_BUILTIN())
#else
#ifdef VAXC
#define va_count(n) vaxc$va_count(&n)
extern int vaxc$va_count();
#else
#define va_count(n) decc$va_count(&n)
extern int decc$va_count();
#endif
#endif

static int FindRoutine(struct dsc$descriptor *image,
		       struct dsc$descriptor *routine, int (**rtn)());

int inet_ntoa1(int *arg1)
{
  static $DESCRIPTOR(image,"MULTINET_SOCKET_LIBRARY");
  static $DESCRIPTOR(routine,"inet_ntoa");
  int arglist[255];
  int i;
  static int status;
  static int (*rtn)() = 0;
  va_list ap;
  va_count(arglist[0]);
  va_start(ap, arg1);
  arglist[1] = *arg1;
  for (i=1;i<arglist[0];i++)
    arglist[i+1] = va_arg(ap, int);
  if (!rtn)
  {
    status = FindRoutine((struct dsc$descriptor *)&image,
			 (struct dsc$descriptor *)&routine,&rtn);
    if (!(status & 1))
      rtn = (int (*)())1;
  }
  if ((int)rtn != 1)
    status = lib$callg(arglist,rtn);
  return status;
}

int gethostname1(int arg1,int arg2)
{
  static $DESCRIPTOR(image,"MULTINET_SOCKET_LIBRARY");
  static $DESCRIPTOR(routine,"gethostname");
  int arglist[255];
  int i;
  static int status;
  static int (*rtn)() = 0;
  va_list ap;
  va_count(arglist[0]);
  va_start(ap, arg1);
  arglist[1] = arg1;
  for (i=1;i<arglist[0];i++)
    arglist[i+1] = va_arg(ap, int);
  if (!rtn)
  {
    status = FindRoutine((struct dsc$descriptor *)&image,
			 (struct dsc$descriptor *)&routine,&rtn);
    if (!(status & 1))
      rtn = (int (*)())1;
  }
  if ((int)rtn != 1)
    status = lib$callg(arglist,rtn);
  return status;
}

int htons1(int arg1)
{
  static $DESCRIPTOR(image,"MULTINET_SOCKET_LIBRARY");
  static $DESCRIPTOR(routine,"htons");
  int arglist[255];
  int i;
  static int status;
  static int (*rtn)() = 0;
  va_list ap;
  va_count(arglist[0]);
  va_start(ap, arg1);
  arglist[1] = arg1;
  for (i=1;i<arglist[0];i++)
    arglist[i+1] = va_arg(ap, int);
  if (!rtn)
  {
    status = FindRoutine((struct dsc$descriptor *)&image,
			 (struct dsc$descriptor *)&routine,&rtn);
    if (!(status & 1))
      rtn = (int (*)())1;
  }
  if ((int)rtn != 1)
    status = lib$callg(arglist,rtn);
  return status;
}

int gethostbyname1(int arg1)
{
  static $DESCRIPTOR(image,"MULTINET_SOCKET_LIBRARY");
  static $DESCRIPTOR(routine,"gethostbyname");
  int arglist[255];
  int i;
  static int status;
  static int (*rtn)() = 0;
  va_list ap;
  va_count(arglist[0]);
  va_start(ap, arg1);
  arglist[1] = arg1;
  for (i=1;i<arglist[0];i++)
    arglist[i+1] = va_arg(ap, int);
  if (!rtn)
  {
    status = FindRoutine((struct dsc$descriptor *)&image,
			 (struct dsc$descriptor *)&routine,&rtn);
    if (!(status & 1))
      rtn = (int (*)())1;
  }
  if ((int)rtn != 1)
    status = lib$callg(arglist,rtn);
  return status;
}

static int FindRoutine(struct dsc$descriptor *image,
		       struct dsc$descriptor *routine, int (**rtn)())
{
  lib$establish(lib$sig_to_ret);
  return lib$find_image_symbol(image,routine,rtn);
}
#endif

news_get_chan()
{return(s);}

news_set_chan(i)
int *i;
{s = *i;}

news_disconnect()
{
#if UCX
	sys$cancel(s);
	sys$qiow(0,s,IO$_DEACCESS,0,0,0,0,0,0,0,0,0);
#endif
	sys$dassgn(s);
}

#if MULTINET || TWG

static struct hostent *hp, *hp1;
static struct sockaddr_in sin;

#endif

int *node,*node1;

news_gethost()
{
	/*
	 *  Get the IP address of the NEWS host.
	 *  As of MULTINET 3.0, cannot be done at AST level
	 *  so can't do in NEWS_ASSIGN(), as BULLCP calls it at
	 *  AST level if the decnet gateway feature is used.
	 */
#if TWG
	struct hostent *gethostbyname();
#else
#if MULTINET
#endif
#endif

	node = getenv("BULL_NEWS_SERVER");
	if (!node) return(0);
	if (!strchr(node,'.')) return(1); 

#if TWG
	hp = gethostbyname(node);
#else
#if MULTINET
	hp = gethostbyname1(node);
#endif
#endif
	return(1);
}

news_assign()
{
	int n;

	if (!strchr(node,'.')) {
	   strcpy(&task[0],node);
	   n = strlen(node);
	   strcpy(&task[n],"::\"TASK=NNTP\"");
	   task_d.dsc$w_length = 13 + n;
	   if (!(sys$assign(&task_d,&s,0,0) & 1)) return(0);
	   mode = DECNET;
	   return(1);
	}
#if MULTINET || TWG
	/*
	 *  Create a "sockaddr_in" structure which describes the remote
	 *  IP address we want to send to (from gethostbyname()).
	 */

        if (!hp) {
          int h[4],i;
          if (sscanf(node,"%d.%d.%d.%d",&h[0],&h[1],&h[2],&h[3]) == 4) {
            for (i=0;i<4;i++) if (h[i] < 0 || h[i] > 255) return(0);
	    sin.sin_addr.s_addr = (h[3]<<24)+(h[2]<<16)+(h[1]<<8)+(h[0]);
	  } else
	    return(0);
	  sin.sin_family = AF_INET;
	}
        else {
 	  sin.sin_family = hp->h_addrtype;
	  memcpy(&sin.sin_addr, hp->h_addr, hp->h_length);
        }
#if TWG
	sin.sin_port = htons(119);
#else
	sin.sin_port = htons1(119);
#endif

	/*
	 *  Create an IP-family socket on which to make the connection
	 */

	if (!(sys$assign(&inet_d,&s,0,0) & 1)) return(0);
#else
#if UCX
         if (!(sys$assign(&ucxdev_d,&s,0,0) & 1)) return(0);
	{
           short retlen;
	   struct dsc$descriptor host_name
		= {strlen(node),DSC$K_CLASS_S,DSC$K_DTYPE_T,node};
	   int comm = INETACP$C_TRANS * 256 + INETACP_FUNC$C_GETHOSTBYNAME;
	   struct dsc$descriptor command
		= {4,DSC$K_CLASS_S, DSC$K_DTYPE_T,&comm};
	   struct dsc$descriptor host_ad
		= {4,DSC$K_CLASS_S, DSC$K_DTYPE_T,&addr_buff};
	   struct iosb nam_iosb;

           if (!(sys$qiow(0,s,IO$_ACPCONTROL,&nam_iosb,0,0,
                       &command,&host_name,&retlen,&host_ad,0,0) & 1)
               || !(nam_iosb.status & 1)) {
              sys$dassgn(s);
	      return(0);
	   }
	}
#else
	if (!(sys$assign(&ip_d,&s,0,0) & 1)) return(0);
#endif
#endif
	return(1);
}

struct iosb accept_iosb;

nntp_listen(listen_chan)
int *listen_chan;
{
#if MULTINET
	struct sockaddr_in sin;
	struct iosb accept_iosb;

	if (!(sys$assign(&inet_d,listen_chan,0,0) & 1)) return(0);

	/*
	 *  Create an IP-family socket on which to listen for connections
	 */
	if (!(sys$qiow(0,*listen_chan,IO$_SOCKET,&accept_iosb,0,0,AF_INET,
	    SOCK_STREAM,0,0,0,0) & 1) || !(accept_iosb.status & 1)) {
	   sys$dassgn(*listen_chan);
	   return(0);
	}

	/*
	 *  Create a "sockaddr_in" structure which describes the port we
	 *  want to listen to. Address INADDR_ANY means we will accept
	 *  connections to any of our local IP addresses.
	 */

	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = INADDR_ANY;
	sin.sin_port = htons1(119);

	/*
	 *  Bind to that address...
	 */

	if (!(sys$qiow(0,*listen_chan,IO$_BIND,&accept_iosb,0,0,
	   &sin,sizeof(sin),0,0,0,0) & 1) || !(accept_iosb.status & 1)) {
	   sys$dassgn(*listen_chan);
	   return(0);
	}

 	/*
	 *  Declare to the kernel that we want to listen for connections
	 *  on this port, and that the kernel may queue up to five such
	 *  connections for us.
	 */

	if (!(sys$qiow(0,*listen_chan,IO$_LISTEN,&accept_iosb,0,0,5,
	    0,0,0,0,0) & 1) || !(accept_iosb.status & 1)) {
	   sys$dassgn(*listen_chan);
	   return(0);
	}

	return(1);
#else
	return(0);
#endif
}

nntp_accept_wait(listen_chan,listen_ast,listen_iosb)
int *listen_chan,*listen_ast,*listen_iosb;
{
#if MULTINET                                            
	if (!(sys$qio(0,*listen_chan,IO$_ACCEPT_WAIT,listen_iosb,listen_ast,
	    0,0,0,0,0,0,0) & 1)) {
	   sys$dassgn(*listen_chan);
	   return(0);
	}

	return(1);
#endif
}
 
nntp_accept(listen_chan,accept_chan,accept_iosb)
int *listen_chan,*accept_chan;
struct iosb *accept_iosb;
{
#if MULTINET
	struct sockaddr_in sin;
	FILE *fp;
	char buf[128];
	char *cp, *h;
	int s;
	struct sockaddr_un sun = {AF_UNIX};

	*accept_chan = -1;

	    /*
	     *	Call accept to accept a new connection. This 'peels'
	     *	a connection off of the original socket and returns to us
	     *	a new channel to the connection. We could now close
	     *	down the original socket if we didn't want to handle
	     *	more connections.
	     */
	if (!(sys$assign(&inet_d,accept_chan,0,0) & 1)) return(0);

	if (!(sys$qiow(0,*accept_chan,IO$_ACCEPT,accept_iosb,0,0,
	   &sin,sizeof(sin),*listen_chan,0,0,0) & 1)
	   || !(accept_iosb->status & 1)) return(0);

	fp = fopen("BULL_TCP_NEWS_GATEWAY", "r");
	if (!fp) return(1);

	/* A non-official way of getting ip name at ast level */

	if (!(sys$assign(&inet_d,&s,0,0) & 1)) return(0);
	if (!(sys$qiow(0,s,IO$_SOCKET,&iosb,0,0,AF_UNIX,
	    SOCK_STREAM,0,0,0,0) & 1) || !(iosb.status & 1))
	    {printf("1 iosb.status = %d\n",iosb.status);sys$dassgn(s);return(0);} 

	strcpy(sun.sun_path,"DNS");
	if (!(sys$qiow(0,s,IO$_CONNECT,&iosb,0,0,&sun,sizeof(sun),0,0,0,0) & 1)
	    || !(iosb.status & 1)) {printf("2 iosb.status = %d\n",iosb.status);sys$dassgn(s);return(0);} 

/*	buf1.function = 1;	/* gethostbyname */
	buf1.function = 2;	/* gethostbyaddr */
	buf1.call_code = 0;
	buf1.length = strlen(inet_ntoa1((int)(&sin.sin_addr)));
 	strcpy(buf1.string,inet_ntoa1((int)(&sin.sin_addr)));

	if (!(sys$qiow(0,s,IO$_WRITEVBLK,&iosb,0,0,&buf1,
					sizeof(buf1),0,0,0,0) & 1)
	    || !(iosb.status & 1)) {printf("3 iosb.status = %d\n",iosb.status);sys$dassgn(s);return(0);} 

	if (!(sys$qiow(0,s,IO$_READVBLK,&iosb,0,0,&buf2,
					sizeof(buf2),0,0,0,0) & 1)
	    || !(iosb.status & 1)) {printf("4 iosb.status = %d\n",iosb.status);sys$dassgn(s);return(0);} 

	printf("5 iosb.status = %d\n",iosb.status);sys$dassgn(s);
	buf2.string[buf2.length] = 0;
	for (cp=buf2.string; *cp; cp++) *cp = tolower(*cp);

	while (fgets(buf, sizeof(buf), fp)) {
	    for (cp=buf; *cp != '\n'; cp++) *cp = tolower(*cp);
	    *cp = 0;
	    for (cp=buf; *cp == ' ' || *cp == '\t'; cp++);
	    if (*cp == '\n' || *cp == '#') continue;
	    if (!strcmp(buf2.string,cp)) return (1);
	    if (*cp == '.' && strstr(buf2.string,cp)) return (1);
	}
	(void) fclose(fp);

	return (0);
#endif
}

news_socket()
{
	if (mode == DECNET) return (1);

#if MULTINET || TWG
	if (!(sys$qiow(0,s,IO$_SOCKET,&iosb,0,0,sin.sin_family,
	    SOCK_STREAM,0,0,0,0) & 1) || !(iosb.status & 1)) {
	   sys$dassgn(s);
	   return(0);
	}
#endif
#if UCX
	sck_parm[0] = INET$C_TCP;
	sck_parm[1] = INET_PROTYP$C_STREAM;
	local_host.inet_family = INET$C_AF_INET;
	local_host.inet_port = 0;
	local_host.inet_adrs = INET$C_INADDR_ANY;
	lhst_adrs.lgth = sizeof local_host;
	lhst_adrs.hst = &local_host;
	if (!(sys$qiow(0,s,IO$_SETMODE,&iosb,0,0,&sck_parm,0,
	   &lhst_adrs,0,0,0) & 1) || !(iosb.status & 1)) {
           sys$qiow(0,s,IO$_DEACCESS|IO$M_SHUTDOWN,&iosb,0,0,0,0,0,
						UCX$C_DSC_ALL,0,0);
	   sys$dassgn(s);
	   return(0);
	}
#endif

	return(1);
}

news_socket_bullcp(efn,biosb,astadr,astprm)
int *biosb,*astadr,*astprm,*efn;
{
	if (mode == DECNET) return (1);

#if MULTINET || TWG
	if (!(sys$qio(*efn,s,IO$_SOCKET,biosb,astadr,*astprm,sin.sin_family,
	    SOCK_STREAM,0,0,0,0) & 1) ) return(0);
#else
#if UCX
	sck_parm[0] = INET$C_TCP;
	sck_parm[1] = INET_PROTYP$C_STREAM;
	local_host.inet_family = INET$C_AF_INET;
	local_host.inet_port = 0;
	local_host.inet_adrs = INET$C_INADDR_ANY;
	lhst_adrs.lgth = sizeof local_host;
	lhst_adrs.hst = &local_host;
	if (!(sys$qio(0,s,IO$_SETMODE,biosb,astadr,*astprm,&sck_parm,0,
	   &lhst_adrs,0,0,0) & 1) ) return(0);
#else
	return(-1);
#endif
#endif

	return(1);
}

news_create()
{
	if (mode == DECNET) return (1);

#if MULTINET || TWG

	/*
	 *  Do a psuedo-connect to that address. This tells the kernel that
	 *  anything written on this socket gets sent to this destination. It
	 *  also binds us to a local port number (random, but that is ok).
	 */

	if (!(sys$qiow(0,s,IO$_CONNECT,&iosb,0,0,&sin,sizeof(sin),0,0,0,0) & 1)
	    || !(iosb.status & 1)) {
	   sys$dassgn(s);
	   return(0);
	}
#else
#if UCX 
        remote_host.inet_family = INET$C_AF_INET;
        remote_host.inet_port = htons(119);
	remote_host.inet_adrs = addr_buff;
	rhst_adrs.lgth = sizeof remote_host;
	rhst_adrs.hst = &remote_host;
	if (!(sys$qiow(0,s,IO$_ACCESS,&iosb,0,0,0,0,&rhst_adrs,0,0,0) & 1)
	    || !(iosb.status & 1)) {
           sys$qiow(0,s,IO$_DEACCESS|IO$M_SHUTDOWN,&iosb,0,0,0,0,0,
						UCX$C_DSC_ALL,0,0);
	   sys$dassgn(s);
	   return(0);
	}
#else
	if (!(sys$qiow(0,s,IO$_CREATE,&iosb,0,0,node,119,0,1,0,300) & 1)
	    || !(iosb.status & 1)) {
	   sys$dassgn(s);
	   return(0);
	}
#endif
#endif

	return(1);
}

news_create_bullcp(efn,biosb,astadr,astprm)
int *biosb,*astadr,*astprm,*efn;
{
	if (mode == DECNET) return (1);

#if MULTINET || TWG

	/*
	 *  Do a psuedo-connect to that address. This tells the kernel that
	 *  anything written on this socket gets sent to this destination. It
	 *  also binds us to a local port number (random, but that is ok).
	 */

	if (!(sys$qio(*efn,s,IO$_CONNECT,biosb,astadr
		,*astprm,&sin,sizeof(sin),0,0,0,0) & 1)) return(0);
#else
#if UCX
        remote_host.inet_family = INET$C_AF_INET;
        remote_host.inet_port = htons(119);
	remote_host.inet_adrs = addr_buff;
	rhst_adrs.lgth = sizeof remote_host;
	rhst_adrs.hst = &remote_host;
	if (!(sys$qio(*efn,s,IO$_ACCESS,biosb,astadr,*astprm,0,
		0,&rhst_adrs,0,0,0) & 1)) return(0);
#else
	if (!(sys$qio(*efn,s,IO$_CREATE,biosb,astadr,*astprm,node,
		119,0,1,0,300) & 1))
	   return(0);
#endif
#endif

	return(1);
}

news_connect()
{
	if (!news_gethost()) return(0);
	if (!news_assign()) return(0);
	if (!news_socket()) return(0);
	return(news_create());
}

news_write_packet(buf)

struct dsc$descriptor_s *buf;
{
	static int n,len;

	len = buf->dsc$w_length;
#if CMU
	if (!(sys$qiow(0,s,IO$_WRITEVBLK,&iosb,0,0,buf->dsc$a_pointer,
					len,0,!mode,0,0) & 1)
	    || !(iosb.status & 1)) return(0);
#else
	if (!(sys$qiow(0,s,IO$_WRITEVBLK,&iosb,0,0,buf->dsc$a_pointer,
					len,0,0,0,0) & 1)
	    || !(iosb.status & 1)) return(0);
#endif

	return(1);
}

news_write_packet_bullcp(efn,biosb,astadr,astprm,buf,len)
int *biosb,*astadr,*astprm,*efn,*buf,*len;
{
#if CMU
	if (!(sys$qio(*efn,s,IO$_WRITEVBLK,biosb,astadr,*astprm,buf,
					*len,0,!mode,0,0) & 1)) return(0);
#else
	if (!(sys$qio(*efn,s,IO$_WRITEVBLK,biosb,astadr,*astprm,buf,
					*len,0,0,0,0) & 1)) return(0);
#endif

	return(1);
}

news_read_packet(buf)
struct dsc$descriptor_s *buf;
{
	static int n,len;

	len = buf->dsc$w_length;
	if (!(sys$qiow(0,s,IO$_READVBLK,&iosb,0,0,buf->dsc$a_pointer,
					len,0,0,0,0) & 1)
	    || !(iosb.status & 1)) return(0);
	n = iosb.size;

	return(n);
}

news_gethostname(buf)

struct dsc$descriptor_s *buf;
{
	if (mode == DECNET) return (-1);
#if TWG
	return(gethostname(buf->dsc$a_pointer, buf->dsc$w_length));
#else
#if MULTINET
	return(gethostname1(buf->dsc$a_pointer, buf->dsc$w_length));
#else
	return(-1);
#endif
#endif
}


smtp_assign()
{
	int n;

#if MULTINET || TWG
	/*
	 *  Create a "sockaddr_in" structure which describes the remote
	 *  IP address we want to send to (from gethostbyname()).
	 */

        if (!hp1) {
          int h[4],i;
          if (sscanf(node1,"%d.%d.%d.%d",&h[0],&h[1],&h[2],&h[3]) == 4) {
            for (i=0;i<4;i++) if (h[i] < 0 || h[i] > 255) return(0);
	    sin.sin_addr.s_addr = (h[3]<<24)+(h[2]<<16)+(h[1]<<8)+(h[0]);
	  } else
	    return(0);
	  sin.sin_family = AF_INET;
	}
        else {
 	  sin.sin_family = hp1->h_addrtype;
	  memcpy(&sin.sin_addr, hp1->h_addr, hp1->h_length);
        }
#if TWG
	sin.sin_port = htons(25);
#else
	sin.sin_port = htons1(25);
#endif

	/*
	 *  Create an IP-family socket on which to make the connection
	 */

	if (!(sys$assign(&inet_d,&s1,0,0) & 1)) return(0);
#else
#if UCX
         if (!(sys$assign(&ucxdev_d,&s1,0,0) & 1)) return(0);
	{
           short retlen;
	   struct dsc$descriptor host_name
		= {strlen(node1),DSC$K_CLASS_S,DSC$K_DTYPE_T,node1};
	   int comm = INETACP$C_TRANS * 256 + INETACP_FUNC$C_GETHOSTBYNAME;
	   struct dsc$descriptor command
		= {4,DSC$K_CLASS_S, DSC$K_DTYPE_T,&comm};
	   struct dsc$descriptor host_ad
		= {4,DSC$K_CLASS_S, DSC$K_DTYPE_T,&addr_buff};
	   struct iosb nam_iosb;

           if (!(sys$qiow(0,s1,IO$_ACPCONTROL,&nam_iosb,0,0,
                       &command,&host_name,&retlen,&host_ad,0,0) & 1)
               || !(nam_iosb.status & 1)) {
              sys$dassgn(s);
	      return(0);
	   }
	}
#else
	if (!(sys$assign(&ip_d,&s1,0,0) & 1)) return(0);
#endif
#endif
	return(1);
}


smtp_create()
{
#if MULTINET || TWG

	/*
	 *  Do a psuedo-connect to that address. This tells the kernel that
	 *  anything written on this socket gets sent to this destination. It
	 *  also binds us to a local port number (random, but that is ok).
	 */

	if (!(sys$qiow(0,s1,IO$_CONNECT,&iosb,0,0,&sin,sizeof(sin),0,0,0,0) & 1)
	    || !(iosb.status & 1)) {
	   sys$dassgn(s1);
	   return(0);
	}
#else
#if UCX 
        remote_host.inet_family = INET$C_AF_INET;
        remote_host.inet_port = htons(25);
	remote_host.inet_adrs = addr_buff;
	rhst_adrs.lgth = sizeof remote_host;
	rhst_adrs.hst = &remote_host;
	if (!(sys$qiow(0,s1,IO$_ACCESS,&iosb,0,0,0,0,&rhst_adrs,0,0,0) & 1)
	    || !(iosb.status & 1)) {
           sys$qiow(0,s1,IO$_DEACCESS|IO$M_SHUTDOWN,&iosb,0,0,0,0,0,
						UCX$C_DSC_ALL,0,0);
	   sys$dassgn(s1);
	   return(0);
	}
#else
	if (!(sys$qiow(0,s1,IO$_CREATE,&iosb,0,0,node1,25,0,1,0,300) & 1)
	    || !(iosb.status & 1)) {
	   sys$dassgn(s1);
	   return(0);
	}
#endif
#endif

	return(1);
}

smtp_disconnect()
{
#if UCX
	sys$cancel(s1);
	sys$qiow(0,s1,IO$_DEACCESS,0,0,0,0,0,0,0,0,0);
#endif
	sys$dassgn(s1);
}


smtp_connect()
{
	if (!smtp_gethost()) return(0);
	if (!smtp_assign()) return(0);
	if (!smtp_socket()) return(0);
	return(smtp_create());
}

char node2[132];

smtp_gethost()
{
	/*
	 *  Get the IP address of the SMTP host.
	 *  As of MULTINET 3.0, cannot be done at AST level
	 *  so can't do in SMTP_ASSIGN(), as BULLCP calls it at
	 *  AST level if the decnet gateway feature is used.
	 */
#if TWG
	struct hostent *gethostbyname();
#else
#if MULTINET
#endif
#endif

	node1 = getenv("BULL_SMTP_SERVER");
	if (!node1) {
#if TWG
	   gethostname(node2,132);
#else
#if MULTINET
	   gethostname1(node2,132);
#endif
#endif
	   node1 = node2;
	}

#if TWG
	hp1 = gethostbyname(node1);
#else
#if MULTINET
	hp1 = gethostbyname1(node1);
#endif
#endif
	return(1);
}



smtp_write_packet(buf)
struct dsc$descriptor_s *buf;
{
	static int n,len;

	len = buf->dsc$w_length;
#if CMU
	if (!(sys$qiow(0,s1,IO$_WRITEVBLK,&iosb,0,0,buf->dsc$a_pointer,
					len,0,!mode,0,0) & 1)
	    || !(iosb.status & 1)) return(0);
#else
	if (!(sys$qiow(0,s1,IO$_WRITEVBLK,&iosb,0,0,buf->dsc$a_pointer,
					len,0,0,0,0) & 1)
	    || !(iosb.status & 1)) return(0);
#endif

	return(1);
}


smtp_read_packet(buf)
struct dsc$descriptor_s *buf;
{
	static int n,len;

	len = buf->dsc$w_length;
	if (!(sys$qiow(0,s1,IO$_READVBLK,&iosb,0,0,buf->dsc$a_pointer,
					len,0,0,0,0) & 1)
	    || !(iosb.status & 1)) return(0);
	n = iosb.size;

	return(n);
}


smtp_socket()
{

#if MULTINET || TWG
	if (!(sys$qiow(0,s1,IO$_SOCKET,&iosb,0,0,sin.sin_family,
	    SOCK_STREAM,0,0,0,0) & 1) || !(iosb.status & 1)) {
	   sys$dassgn(s1);
	   return(0);
	}
#endif
#if UCX
	sck_parm[0] = INET$C_TCP;
	sck_parm[1] = INET_PROTYP$C_STREAM;
	local_host.inet_family = INET$C_AF_INET;
	local_host.inet_port = 0;
	local_host.inet_adrs = INET$C_INADDR_ANY;
	lhst_adrs.lgth = sizeof local_host;
	lhst_adrs.hst = &local_host;
	if (!(sys$qiow(0,s1,IO$_SETMODE,&iosb,0,0,&sck_parm,0,
	   &lhst_adrs,0,0,0) & 1) || !(iosb.status & 1)) {
           sys$qiow(0,s1,IO$_DEACCESS|IO$M_SHUTDOWN,&iosb,0,0,0,0,0,
						UCX$C_DSC_ALL,0,0);
	   sys$dassgn(s1);
	   return(0);
	}
#endif

	return(1);
}
$eod 
$copy/log sys$input CHANGES.TXT
$deck
Added the SET GATEWAY command to change the how the subject lines looks in
messages which are sent to an email address associated with a folder.  2/25/98

Many bugs were fixed, mainly with respect to the news-email-folder gateway.

V2.3

Added the ability to create a folder that can post and read to a news group,
and can send and read messages via email.  See NEWS_TO_FOLDER.TXT for more
info.  4/18/97

Added the ability for BULLETIN to see news groups from secondary news groups
(see NEWS.TXT).  4/18/97

V 2.25

Changes to make it easier to click on news group name to select them:  You can
enter a news group name at the prompt and it will select it (without typing
SELECT).  News groups displayed without period at end when BULLETIN lists the
ones that have new messages.  NEWS/SUBS has space between * and news group
when denoting which ones have new messages.  3/21/96

V 2.24

When replying to a message in a news group that has been crossposted, give the
user  the  option  of  just  posting  the reply to the news group in which the
message is being read rather than  all  the  news  groups  in  the  crosspost.
11/20/95

Messages added to folders and stored news groups are now marked as being  read
so that the person does not see them when doing a READ/NEW.  11/20/95

Removed INCLUDE command and replaced it with the THREAD command.    This
allows  you  to  save  and  easily  read new messages in threads via the
READ/THREADS command.  11/13/95

V 2.23

Update newest read message when switching folders.  Previously this was done
only when you exited BULLETIN, which would cause problems if you ran BULLETIN
simultaneously from two different logins.  9/12/95

Added /LIMIT to EXCLUDE and also the SET EXLIMIT command.   These  allow
excludes to be specified so that they expire after a specified amount of
days.  The SHOW EXCLUDE now will show the last time a message was found 
that matched the exclude.  8/16/95

Added /GROUP qualifier to DIRECTORY command to allow  specifying  groups
to search using wild cards names.  8/3/95

Added /GROUP qualifier to SEARCH command to allow specifying  groups  to
search using wild cards names.  6/20/95

V 2.22

Added SHOW EXCLUDE and INCLUDE commands.  5/17/95

Fixed bugs relating to messages with an expiration year past 1999.  5/17/95

Fixed EXCLUDE/FROM as it did not work.   Also  fixed  EXCLUDE/DISABLE/ALL  and
INCLUDE/DISABLE/ALL, as in some cases they would not work.3/31/95 

V 2.21

Modified the file format for stored news group to reduce disk space usage.
11/29/94

Added SET LIBRARY command and also /LIBRARY qualifier on the command line to
allow switching between different directories which contain different sets of
folders.  11/29/94

Added /HEADER to POST when posting to news groups to allow adding headers to
the message.  11/9/94

Added /CANCEL to PRINT command which cancels previous print commands.
10/28/94

V 2.20

Added /FOLLOWUP to the POST command.  10/12/94

The FILE command no longer requires a file name, but will create a file
name from the folder's name.  5/25/94

Allow logical names to be specified in POST/GROUP.  5/12/94

Added SET FILE_DIRECTORY command.  5/12/94

Added /PERMANENT and /DEFAULT qualifiers to NEWS command.  4/28/94

Added SET SUBSCRIBE command to allow setting default or permanent news groups. 
4/26/94

Added code to mail rejected posting for a stored news group to poster.  4/6/94

Optimized newsgroup list upgrade to reduce  disk  I/O  which  greatly  reduces
elapsed time for slow or fragmented disks.  4/5/94

Modified /EDIT so EDT error no longer shows "no file found" message.  4/5/94

Added SET NAME command to copy settings, used if username is changed.  3/28/94

Modified SEARCH command to avoid updating new message counter (in order to be
able to follow a thread and still use READ/NEW later).  3/26/94

Added /INDENT=string to allow different indentation string.  3/12/94

Added ability to allow BULLCP to gateway for NEWS access via TCP (for MULTINET
only).  2/24/94

Fix FROM header for news groups messages that have an address which continues
on a 2nd line.  12/17/93

V 2.19

Add /MATCH qualifier to SEARCH command, and allow more than 1 string to be
specified.  12/2/93

Fixed the qualifer /CC when posting or resonding to messages.  It was supposed
to be able to send to more than one user, but actually was sending to only the
first user specified.  7/17/93

Fixed alpha related problems.  7/16/93

Fixed problem with responding to addresses of form: name <address>.  7/2/93

Fixed shutdown bugs.  6/6/93

Fixed /PRINT and /EXTRACT in DIRECTORY when used with a remote news group.
5/29/93

System messages which have longer lines than the terminal page width will have
their text left justified rather than simply wrapped.  5/28/93

Added SET [NO]EXCLUDE command to be  able  to  ignore  any  excludes  or
includes that have been specified for that folder.  5/20/93

V 2.18

Added /FULL to EXCLUDE  and  INCLUDE  command  to  make  it  affect  all
commands, such as directory listings.  5/13/93

Fixed bug which displayed wrong foldername for notification broadcasts for
messages added to bboard folders with digest set.  5/13/93

Fixed bug which caused FOR003.DAT files to appear in DECNET directory due to
BBOARD folder which has digest set.  5/10/93

Fixed problem with inserting correct time when posting to news group.  5/5/93

Fixed problem with BULL_DIR_LIST usage.  5/5/93

Fixed shutdown problems.  5/3/93

Fixed new executable message.  5/3/93

Fixed bugs which caused FOR00x.DAT files to appear in DECNET directory during
access from remote nodes.  4/29/93

V 2.17

Modified to work for ALPHA cpus.  4/5/93

Speeded up DIRECTORY listing.  3/18/93

Fixed FORWARD command from truncating subject lengths > 64.  3/18/93

V 2.16

Add code which causes nodename of remote folders to automatically be updated
when the bulletin data files of the node containing the remote folders are
moved to a different node.  3/12/93

Fix incorrect display of NEWS/SUBSCRIBE/COUNT.  3/6/93

In batch mode, paging is now automatically turned off and page width set to 80.
3/5/93

Fixed problem with shutdown messages not being deleted.  3/4/93

V 2.15

Code that converts data files if FOLDER_MAX is increased did not work.  2/27/93

NEWS/SUBS now shows last read message.  INDEX now shows listing similar to
DIR/FOLDER and NEWS. 2/27/93

A user can make /HEADER be made the default for a folder or news group by adding
a line to the user's customization file.  (See HELP custom) 2/21/93

Personal names which are set in VMS MAIL are now automatically added to the from
address when posting to news groups.  2/15/93

Fixed bug which caused only partial storage of specified local news groups.
2/5/93

Fixed bug that caused privilege error and crash to occur after a non-privileged
user posted a message to a folder which had an associated mailing list.  2/5/93

V 2.14

Added SET ANONYMOUS command so that all messages added to a folder will have
the username ANONYMOUS rather than the actual username.  2/1/93

Added /EXTRACT qualifie to DIRECTORY command.  1/31/93

Added notification of new executable and possible new features.  1/26/93

Dump log files are now created with acl for folder owner to be able to delete
it.  1/26/93

V 2.13

Fixed bug in BBOARD code that corrupts file length.  1/15/93

Fixed notification messages that showed wrong folder name.  1/15/93

Added /[NO]HEADER and /ROTATE to NEXT (help said they were there, but they
weren't).  1/15/93

Added RESET command.  1/9/93

Fixed bug in posting to stored news group by non-privileged users.  12/28/92

V 2.12

Fixed SET ACCESS /ALL which broke due to changes in V 2.11.  12/28/92

Fixed problem with reply posting to stored news group not posting to proper
group.  12/28/92

Added code to allow setting access to news group or class of news groups.
Added /PRIVATE switch to SET NEWS.  Added /CLASS to SET ACCESS.  12/26/92

Fixed bug in code that does copying from news group to folder.  12/26/92

Added INCLUDE and EXCLUDE commands which allow avoiding reading messages based
on subject and address headers.  12/15/92

Fixed bug which caused folder corruption.  12/15/92

V 2.11

Added SET NEWS command.  Used for setting a news group or a class of news
groups to be stored on disk for quicker access by users (rather than being 
read by users directly from the server).  Can also disable access to a group. 
Users can set NOTIFY on stored groups.  11/5/92
                                       
NEWS command now by default shows only groups which are active.  Can show all
groups with /ALL command.  /STORED and /COUNT are new qualifiers.  11/5/92

Stored news groups are stored with data compression.  Normal folders can also be
stored that way if set with the SET COMPRESS command.  11/5/92

INDEX command modified to make it more useful.  /NEW is now the default, and now
only shows folders or groups that have new messages.  /SET added to show only
folders which have READNEW, SHOWNEW, or BRIEF set, and /SET is the default.
11/5/92

Fixed bug with BROADCAST routines which could cause BULLCP to go into MWAST
state.  11/5/92

Modified code which adds BBOARD messages to speed it up when multiple messages
are being added.  11/5/92
                                                 
Folder names can now be up to 44 letters long.  11/5/92

BULL_BBOARD_UPDATE and BULL_NEWS_UPDATE are now continuously translated by
BULLCP so that they can be changed dynamically.  11/5/92

BULLCP now is created with reasonable working quotas rather than PQL_ defaults
which are usually way too low.  11/5/92

Changed all variables to be long word multiples in order to be more ALPHA
compliant (and maybe faster because of it?).  11/5/92

/ROTATE added for read commands to allow reading messages encoded in ROT-13
coding.  This is used by some news groups to display messages which could be
taken as being offensive (i.e. rec.humor.funny).  11/5/92

Fixed many minor bugs that no one mentioned, so I won't either.  11/5/92

Fixed ADD/BROADCAST/EDIT not working with TPU.  8/13/92

V 2.10

Allow non-digest messages to be added to a folder which has DIGEST set.  8/6/92

Added ADD_ONLY attribute.  If a mailing address is present, when messages are
added to a folder, they will also be mailed to the address.  Users are
prevented from using the POST command.  Instead, the ADD command will be used
if the POST command is entered.  One use for this is a local board which is
also distributed to non-local users.  8/1/92

Added POST_ONLY attribute  This causes the ADD command to mail messages to the
mailing address if it is present, rather than add it to the folder.  8/1/92

Fixed several shutdown bugs.  7/23/92

Fixed PMDF broken by V2.09.  6/16/92

Added system logical name BULL_CUSTOM.  It is equated to a hex number string.  
Bit 0 set = need privileges to create folder, 1 set = captive account can
write files, 2 set = captive account can use editor.  5/25/92

V 2.09

Allow having more than one database by redefining BULL_DIR.  However, only 
directories that are defined in the list of equivalence names pointed to by
the system logical name BULL_DIR_LIST are allowed.  See AAREADME.TXT
for more info.  5/10/92

GENERAL folder can now be renamed or modified (not deleted).  4/22/92

/FROM, /NOREPLIES, & /NEGATED added to SEARCH and DIRECTORY commands.  3/18/92

Mail routines now use MAIL$ calls for outgoing mail for faster execution.  
3/15/92

Changing keypad definitions using initialization file now possible.  3/12/92

Subscribed news groups are now listed in alphabetical order.  3/7/92

V 2.08

Fixed bug which caused missing news groups. See NEWS.TXT for info.  2/25/92

Allow setting local protection on remote folders.  12/12/91

Fixed bug with creation of folder files.  If they were deleted after the folder
was created, the files that would be created by BULLETIN to replace them (when
the folder is selected) would be created with the wrong protection.  12/12/91

Fix problem with MULTINET V3.0 and DECNET/NEWS gateway feature.  BULLCP will
hang without this fix if there is an attempt to read news via it.  12/9/91

Fix bug that causes incorrect time on news postings after the first post. 
Display time when reading news messages in local rather than GMT time.  12/8/91

Add 30 second timeout for connecting to nameserver for news.  Can be increased
up to 99 seconds via defining BULL_NEWS_TIMER.  12/3/91

Allow list of numbers when specifying message numbers for PRINT and FILE
commands.  11/27/91

Fixed bugs in BBOARD code:  Messages with lines > 255 characters would not be
included.  Subject line not correctly extracted if next line was simply a
To:. (relink PMDF driver if using PMDF for patch to take affect).  11/27/91

V 2.07

NEWS listing now shows the status of the news group, i.e. active, inactive,
moderated, or renamed.  10/23/91

Fixed PRINT command so that if a print qualifier (i.e. /QUEUE) is specified,
it will cause any pending print jobs to be printed if the qualifier for the
pending jobs is different.  10/23/91

Added /NOSIGNATURE qualifier for POST & RESPOND commands.  10/21/91

Fixed error in POST & RESPOND command.  If a file was specified on the command
line, and /EDIT was specified, the file would be sent even if the user quit out
of the edit, rather than exitting (i.e. outputting a file).  10/21/91

Fixed REPLY option in READNEW, as it was possible for users with only read
access to a folder to be able to add REPLY messages.  10/10/91

Add REPLY option to READNEW feature when reading messages.  Also, really fix
the REPLY command, as mentioned in V2.06.  8/11/91

V 2.06

Added code to keep track of which messages have been read a per message basis. 
Added SEEN & UNSEEN commands.  Added /SEEN, /UNSEEN, and /UNMARKED to
DIRECTORY, INDEX, READ, and SELECT commands.  Modified directory listing to
indicate which messages have been SEEN.  7/31/91 

Added /NOW to PRINT command.  Messages no longer have to be printed one message
at a time.  It now works identical to VMS MAIL.  7/31/91

Added code to NEWS users when new groups have been created.  User will be
alerted when selecting a news group that new groups are present, and will be
instructed to type NEWS/NEWGROUP in order to see them.  7/31/91

Added /PRINT to DIRECTORY command to allow printing of messages which are found
by using the DIRECTORY command.  7/31/91

Modified directory listing display so that the first and last message in the
folder are now displayed at the top.  Fixed bug which truncated very large news
group names.  7/31/91

Added FIRST command to read first message found in folder.  7/31/91

Modified REPLY command for folders associated with mailing lists, so that the
reply message to the mailing list rather than adding a local message.  7/31/91

Modified code to correctly store subject headers from BBOARD mail which are
more than one line long.  Previously, the subject would be truncated.  6/18/91

V 2.05

The MARK code was modified to work with NEWS folders.  6/3/91

Added /FOLDER=(folder,[...]) to the SEARCH command to allow searching more than
one folder at a time.  6/13/91

NEWS/SUBSCRIBED listing was fixed.  If the list could not fit on a single page,
a folder was skipped when the next page was shown.  6/3/91

INDEX was fixed.  If it was used with the qualifiers /NEW or /MARK, and the
directory listing of a folder was displayed, and then RETURN is entered to 
skip to the next folder, the directory display of the next folder would be
incorrect.  6/3/91 

Fixed broadcast bug.  If a message was added with /BROADCAST to a remote folder
from a node in a cluster which was not the node that BULLCP was running on. 
The broadcast would appear twice on the cluster.  5/24/91

Added code to alert user if message too large to be fully broadcasted.  5/24/91

Added code to avoid erroneous notifications of new messages for an empty NEWS
group.  Unlike a similar fix in V2.03 which was due to a bug, this fix may not
affect all sites, as it depends on the behavior of the server.  5/22/91 

Fixed NEWS to FOLDER feed.  A recent change broke it.  5/22/91

Added /EDIT qualifier for MAIL.  5/20/91

Added /HEADER qualifier for LAST, BACK, and CURRENT commands.  5/19/91

Added TWG (Wollongong) interface for NEWS.  5/18/91

Fixed bug which truncated subject headers of messages created when using REPLY
and RESPOND to messages which have long subject lines.  5/12/91

V2.04

Added ALWAYS attribute for folders.  Any SYSTEM messages in a folder in which
ALWAYS has been set will be displayed every time a user logs in, rather than
just once.  Also, non-SYSTEM messages will be displayed continuously (via
whatever mode is set, i.e. READNEW, SHOWNEW, or BRIEF) until it is actually
read.  4/29/91 

Added capability of controlling the time between updates for BBOARD and NEWS in
BULLCP by defining the logical names BULL_BBOARD_UPDATE or BULL_NEWS_UPDATE to
the number of minutes of desired time in minutes. 4/27/91

Added /GROUPS= qualifier to all commands which post to NEWS groups. 4/26/91

Fixed bug which prevented SET SHOWNEW or READNEW from working with subscribed
news group folders. 4/25/91

V2.03

Added /FOLDER to SHOW USER in order to show the latest message that a user
has read in the specified folder.  Also added /SINCE and /START (the former
for real folders, the latter for news groups).  4/11/91

Fixed logic so that defining BULL_NEWS_ORGANIZATION will override the
definition defined in BULLNEWS.INC.  4/10/91

Fixed SEARCH command, as it broke in V2.02 when /EDIT was added to read
message commands.  There is a missing QUALIFIER EDIT in BULLCOM.CLD for the
SEARCH verb.  /EDIT now works with SEARCH.  4/9/91

Fixed bug in BULLCP which prevented the DECNET/INTERNET NEWS gateway software
from working with UCX.  4/9/91 

Fixed bug caused by V2.00 which caused incorrect listing of message during
BULL/LOGIN for remote folders.  4/3/91

Fixed bugs which caused erroneous new message notifications for subscribed
NEWS groups that were empty.	3/27/91

V 2.02

Include BBOARD support for MX (courtesy of goathunter@wkuvx1.bitnet).

Changed BBOARD algorithm so that it is now possible to have only one real
BBOARD account, and have all the others be VMS MAIL forwarding entries.
See HELP SET BBOARD MORE_INFO for more info (it's been updated).

Added hook to allow postings from BULLETIN to a LISTSERV mailing list to use
the BBOARD account from it was subscribed to.  See HELP SET BBOARD LISTSERV.

Fixed many bugs in POST, REPLY, and RESPOND.

Fixed /ALL for COPY, PRINT, and EXTRACT when using NEWS groups.

Included RMS optimizer procedure for indexed files to optimize BULLNEWS.DAT
to speed up NEWS updates.  Can be used on other files (in particular
BULLINF.DAT) in order to save space.

Add /EDIT to BACK, NEXT, LAST, and when entering message number.

Modify ADD/REPLY command to local (non-NEWS) folders so if there are new
messages present, it doesn't reset the newest message count.  Previously,
adding a message would reset the user's last read message date to that message
in order to avoid notifying the user of new messages due to the user's own
message. 

Fixed code so that when reading new messages, and if READ/EDIT or DELETE/IMMED-
IATE IS entered, a carriage return will read the next new message.  Previously
the wrong message would be displayed.

V 2.01

Fixed many bugs associated with USENET NEWS reading feature.

Added UCX interface for NEWS.

Added signature file for POST and RESPOND messages.

Added capability to specify file name for POST, REPLY, and RESPOND.

Added the line "In a previous message, <message-owner> wrote:" to the
beginning of a message when /EXTRACT is specified

Added hook for network mail to run command procedure rather then using
VMS MAIL.  BULL_MAILER can be defined to point to the procedure, and it
is called with the username and subject as the parameters.

V 2.00

Added USENET NEWS reading feature.

V 1.93

Fixed bug which wouldn't allow a permanent message to be added by a
non-privileged user in a remote folder (the folder had been setup to allow
permanent messages from non-privileged users, of course). 

Fixed bug which causes the DELETE command not to delete a SHUTDOWN message
without the use of /IMMEDIATE.

Fixed the algorithm which prevented duplicate notification of messages in
remote folders on different nodes, as duplication was still possible.

V 1.92

Fixed bug which causes BULLCP to loop when trying to cleanup a folder which
has more than 127 identifiers granted access to a folder.  Also correct
SHOW FOLDER/FULL, which had a similar problem when trying to display the
identifiers.

Fix PMDF interface to recognize to recognize PMDF_PROTOCOL.

V 1.91

Disallow SPAWN command for CAPTIVE account.

Fix MAIL command to correctly allow passing addresses with quotes, i.e.
IN%"""MRL@NERUS.PFC.MIT.EDU""".

V 1.90

SET NOTIFY now works for remote folders.

Avoid generating notification message due to SET NOTIFY flag if the message
was broadcasted when added using ADD/BROADCAST.

Bug in DIR/SINCE for remote folders fixed.  If no new messages were present,
it would incorrectly show messages.

Added /FF to EXTRACT command to seperate messages in the file with form feeds.

Allow specifying CURRENT and LAST when specifying a range of messages for
commands that accept a range, i.e. EXTRACT 1-CURRENT, CURRENT-LAST, etc.

Open folder files with READONLY when not writing to them in order to avoid
changing modification date, which results in unnecessary backups.

Modify HELP so that it won't prompt for Subtopic is there is none.

Prevent screen from being erased after exiting HELP.

Fix bug which causes CREATE/NOTIFY to crash.

SET NOTIFY/CLUSTER has been removed.  As of VMS V5.2, it is possible to obtain
the list of users logged in to all nodes of a cluster, so this qualifier is no
long necessary.  NOTE: You can delete all the BULL_DIR:*.NOTIFY files, as they
are no longer used.

BULLETIN now will use the editor specified by the SET EDITOR command within
MAIL for editing messages.

Typing BACK after typing a DIRECTORY command will now show the previous 
DIRECTORY display entries rather than reading the previous message.

Several bugs related to the MARK command were fixed.  Also the software has been
optimized so that scanning for MARKed messages should take less time.

/EXPIRATION added to DIRECTORY command to show expiration rather than creation
date of messages.

Any BULLETIN interactive command can be executed at DCL level by typing
BULLETIN "command" or BULLETIN "command1;command2;etc.".

The CHANGE command has been modified so a range of message can be specified,
i.e. /NUMBER=1-10.  Also, the code incorrectly misinterpreted /TEXT as meaning
to extract the old text message, whereas it should have meant that only the
text was to be changed.  This prevented a user from specifying that only the
text should be changed if that user didn't have editing enabled.  This has been
fixed.  To eliminate confusing, the /TEXT qualifier on the ADD command has been
removed (previously it was a synonym for /EXTRACT). 

SHOW FOLDER/FULL display of access IDs was fixed to correctly display UICs.

Removed security hole which occurs if you are using the old method of accessing
a remote node via /NODES (it would have required looking a the sources to find,
which one installer did and was worried about).  Because of this, if you use
this old method (i.e. via BULLETIN.COM), the object BULLETIN must be installed
in the NCP database pointing to the file BULLETIN.COM, i.e. the command
"MCR NCP SET BULLETIN FILE directory:BULLETIN.COM NUMBER 0" must be executed
during the system startup.

Fixed bug in /LOGIN display when erasing page if terminal is hardcopy.  No
page would be erased (of course), and the next line outputted would start where
the previous line left off, rather than starting on a new line.

Added BULLETIN/WIDTH=page_width for users who have BULLETIN/LOGIN in their
login procedure before the terminal is known, and whose default page width is
larger (i.e. 132) than what the terminals are (i.e. 80).

Added BULLETIN/PGFLQUOTA and /WSEXTENT in order to set those quotas for the
BULLCP process.

Added ATTACH command.

Modify SET STRIP so that it saves the date that the message was sent and
leaves it at the to of the message.

BULLETIN will search BBOARD message headers for a line that starts with
"Expires:" or "X-Expires:", followed by a date (DD MMM YYYY or similar).  It if
finds that line, it will use that date as the expiration date of the message. 

Added /REPLY to SEARCH command.  Modified so that it's possible to abort out of
a /SUBJECT or /REPLY search using CTRL-C (previous possible only if searching
the text for a string.  Also, if you hit CTRL-C at the wrong time, BULLETIN
would abort totally rather than just aborting the search). 

Added /SEARCH= /SUBJ= and /REPLY to the DIRECTORY command.  Basically this is
combining the DIRECTORY and SEARCH commands.

Fixed design flaw which allowed the following to occur:  If a folder is a
remote system folder, when BULLETIN/LOGIN was executed, the same messages might
be displayed on both the local and remote nodes.  BULLETIN now will know that
the user has seen the message on one node and will not display it if that user
logs in on the other node.

Optimized code which caused slow display of new messages when executing
BULLETIN/LOGIN without /REVERSE for a remote folder.

Added /PERMANENT to SET NOTIFY, SHOWNEW, BRIEF, and READNEW.  The affect is
that users will not be allowed to change the setting.  The main intent here
was to allow the removal ofthe permanent setting of SHOWNEW from the GENERAL
folder.

Fixed bug which would cause a SYSTEM message not to be shown if SET BRIEF was
selected for that folder, and a non-SYSTEM message was also present.

Added SET CONTINUOUS_BRIEF.  This causes the SET BRIEF setting to show that
there are unread new messages every time BULLETIN/LOGIN is executed, rather
than just the one time.  The BRIEF notification code has also been optimized
so that it'll take less time to notify you of new messages.

A major bug was fixed which was introduced in previous mods to speed up
BULLETIN/LOGIN.  The effect is that no notifications will appear for certain
folders via BULLETIN/LOGIN.  This would only happen if a folder was removed at
some time.
$eod 
$copy/log sys$input DEBUG.TXT
$deck
$eod 
$copy/log sys$input HANDOUT.TXT
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
$copy/log sys$input INSTRUCT.TXT
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
$copy/log sys$input NEWS.TXT
$deck
BULLETIN has the capability to read and post messages  to  USENET  NEWS  in  a
client  mode.  I realize that there are many NEWS readers, some with much more
elegant interfaces.  However, I elected to modify BULLETIN for  the  following
reason:   We have many decnet nodes, but only several are internet nodes.  Our
only access  to  a  news  server  was  via  internet.    In  order  for  those
non-internet  nodes  to read USENET, the only method that seemed available was
to run a NEWS server program on one of our own internet nodes so that it could
be accessible via decnet.  I did not want to do that, as that requires storing
the news groups on disk, and I do not have the room for that.   I  thus  added
the  ability  in  BULLETIN  (actually  BULLCP) so that it acts as as a gateway
between decnet and tcp for NEWS.  This method does not  require  spawning  any
processes,  since the detached process BULLCP is always present, so the access
is very fast.  Also, since BULLETIN uses a shared database to  store  info  on
the  NEWS  groups and periodically updates it, there is no need for that to be
done when a user accesses the NEWS groups.  Several other NEWS readers do this
when you run them, which is why they take a long time to start up.  It is also
possible to feed NEWS groups into  a  "real"  BULLETIN  folder,  so  that  the
messages are saved on disk (see the file NEWS_TO_FOLDER.TXT). 

Presently, BULLETIN can be used with  either  UCX,  MULTINET,  or  CMU  TCP/IP
packages  (and of course DECNET) for reading NEWS.  Support for other packages
can be added if I can find sites willing to beta test the  interface  for  me.
The  source  for  the  TCP  interface  is in C rather than FORTRAN because the
MULTINET include files are in C. However, if you do not have C, I will be glad
to send the object for it (or to even possibly rewrite the code in FORTRAN). 

The instructions for installation are as follows.  Define BULL_NEWS_SERVER  to
be a system logical name pointing to either your internet or decnet NEWS node.
If it is decnet, simply specify the decnet node name, i.e. 

	$ DEFINE/SYSTEM BULL_NEWS_SERVER NERUS

BULLETIN decides to use DECNET rather than TCP access based on the node  name.
If it does not have any periods in it, then it assumes it is a DECNET node. 

In our cluster, we usually have one node which is an internet  node,  and  the
rest  non-internet  nodes.    If  you have a similar situation, you'll have to
create a startup procedure that defines BULL_NEWS_SERVER to  be  the  internet
news  server  address  only  on  the  node (or nodes) on the cluster that have
actually internet access.  The other nodes will have BULL_NEWS_SERVER  defined
as the decnet node name that BULLCP is running on in the cluster.  (Of course,
BULLCP will have to be running on a node with internet access.)

NOTE: If you want to disable the gateway feature, then before starting BULLCP,
define the logical name: 

	$ DEFINE/SYSTEM BULL_NO_NEWS_GATEWAY "TRUE"

Defining this will only shut off the gateway.  BULLETIN will still be  allowed
to read NEWS from the local node as long as BULL_NEWS_SERVER is defined. 

You can also specify that BULLCP is only to act as a NEWS gateway.  This is to
allow  adding the news gateway to an INTERNET site that you have DECNET access
to, but which does not want to make use of any of the other BULLETIN features.
You would specify the following command before starting BULLCP: 

	$ DEFINE/SYSTEM BULL_NEWS_GATEWAY_ONLY "TRUE"

It is also possible for BULLETIN to access news groups  that  are  located  on
other  news  servers than the one specified by BULL_NEWS_SERVER.  This is done
by creating the  file  BULL_DIR:BULL_ALT_NEWS.LIS  and  adding  a  line  which
containsthe  name  of  the  news group and the news server where it's located.
The format is news-group-name:news-server-name.

In order to post messages, BULLETIN needs to know the internet nodename of the
local host.  This is done automatically for nodes running MULTINET.  For other
nodes, BULLETIN attempts to  translate  the  logical  name  ARPANET_HOST_NAME,
INTERNET_HOST_NAME, and MX_NODE_NAME.  If you are on a DECNET node that is not
on INTERNET (and is not part of a cluster which has an INTERNET address),  but
you are accessing NEWS via DECNET, you can specify the hostname as follows: 

     $ DEFINE/SYSTEM INTERNET_HOST_NAME "%localhost@internet-address"

Where "localhost" is your local decnet hostname, and "internet-address" is the
internet address of the gateway node. 

The local time zone is detected by looking at  the  following  logical  names:
LISP$TIME_ZONE,  MULTINET_TIMEZONE,  or  PMDF_TIMEZONE.    (LISP$TIME_ZONE  is
defined if you have LISP installed.)

The name of the organization is included in the header of  the  NEWS  message.
This can be anything, but usually is the company or university name.  This can
be hardcoded into the source by putting in BULLNEWS.INC, or  by  defining  the
system logical name BULL_NEWS_ORGANIZATION. 

The name of the mail protocol to use for responding by mail to  NEWS  messages
can  also  be  either hardcoded by putting in BULLNEWS.INC, or by defining the
system logical name BULL_NEWS_MAILER. 

After installing the new BULLETIN, execute the command NEWS, which asks for  a
list  of  all the news groups.  Because this is the first time it is executed,
it will cause a load of all the remote news groups  into  a  local  data  base
(BULL_DIR:BULLNEWS.DAT). This will take several minutes to do.  It is the only
time that this load will be  done  interactively.    Afterwards,  BULLCP  will
periodically  update  the data base.  For this reason, it is highly recommeded
that BULLCP be installed. BULLCP will update NEWS every hour.  If you want  to
change  this frequency, define the logical name BULL_NEWS_UPDATE to the number
of minutes in between updates, i.e. DEFINE/SYSTEM BULL_NEWS_UPDATE "30" for 30
minutes.    NOTE:  BULLCP  will create a subprocess BULLCP NEWS which does the
update.  You can watch how long it takes for this to run in order to determine
if you want to change the update period). 

Due to long news group names recently created, you must do the following, or
else those news groups will not be seen.  After BULLNEWS.DAT is created, do
the following:

$ ANAL/RMS/FDL/OUT=FIX.FDL BULL_DIR:BULLNEWS.DAT

Edit FIX.FDL and find the first line which says
	DUPLICATES no
and change it to
	DUPLICATES yes

Then type:

$ CONVERT BULL_DIR:BULLNEWS.DAT BULL_DIR:BULLNEWS.DAT/FDL=FIX.FDL

Also, it is suggested that you run OPTIMIZE_RMS.COM on it, as it will cause the
file to be compressed and will allow updates to run much faster (factor of 5 or
more). 

Never delete BULLNEWS.DAT.  There is no reason to ever  do  so,  and  it  will
cause subscribed users to be subscribed to the wrong news groups. 

WARNING: One user discovered that his server (using bnews?) had  a  bug  which
caused  the updates to cause bogus "new messages" notifications for subscribed
NEWS group when entering BULLETIN.    If  you  experience  this  problem,  try
defining  the  system  logical name BULL_SPECIAL_NEWS_UPDATE.  This will cause
the update to use a different algorithm which should  eliminate  the  problem,
although it requires much more time to execute. 

If you have any problems or questions, please let me know.
									MRL
$eod 
$copy/log sys$input NEWS_TO_FOLDER.TXT
$deck
It is possible to automatically have messages from a news group be fed into  a
real  folder, and visa versa.  This allows BULLETIN messages to be shared with
a news group, thus giving access to such messages to people who  do  not  have
access  to  BULLETIN.   This can also be combined with email access for people
who have neither BULLETIN nor news group access. 

To associate a folder with a news group, place the name of the news group into
the  folder  description  surrounded  by  <>, i.e. <misc.test>.  It must be in
lower case.  (Other text is allowed in the description, i.e. "THIS IS  A  TEST
FOLDER  <misc.test>".)  When messages are added the folder, they are also sent
to the news group, and new messages from the group are posted  to  the  folder
(via  the  BULLCP  process  which wakes up on a periodic basis).  Whenever you
modify the folder description and specify the news group  name,  you  will  be
prompted  as  to  whether  you  want  to initializee the news group counter to
either load all the messages present in the news group, or to load  only  news
messages that appear there. 

Messages posted to the news group from such a folder have a  special  line  in
their  header  indicating  they were sent from that node using bulletin.  When
bulletin feeds new postings back to the folder, it will see that  header  line
and  also see that it was posted from that node, and thus will avoid adding it
to the folder (and thus creating a duplicate message).  Note that messages are
added  to  the  news  group  via  the  BULLCP process, so they will not appear
immediately after being added to the folder. 

If you are modifying an existing folder to make  it  associated  with  a  news
group, and if you want to copy any existing messages in the folder to the news
group, you can do that by using the command COPY/ORIGINAL/LOCAL and specifying
the  news  group  as the destination.  /ORIGINAL will cause the original owner
names and dates to be preserved (note, however, that dates older than 14  days
in  the  past are not usually accepted by the news server, so dates older than
that will be changed to the present date).  /LOCAL  will  create  the  special
header  line in the messages to prevent BULLCP from adding those messages back
to the folder from the news group.  /ALL can be  specified  to  copy  all  the
messages in the folder. 

If you create a remote folder that points to a folder that has  a  news  group
associated  with  it,  you  do  not  have  to  specify  the  news group in the
description of the remote node.  However, on the  node  on  which  the  folder
actually  exists,  the  news  group  has be a stored news group, i.e. you must
issue the command SET NEWS/STORED for that news group. 

When a news group is specified for a folder, it is still possible to associate
an  email  address  for  the messages to be sent to in the folder description.
You do this by using [] to enclose the address rather than the usual <>,  i.e.
[mrl@pfc.mit.edu]  See  help in BULLETIN on the command CREATE/DESCRIPTION for
more info about associated email addresses.  Note that  these  features  allow
you to create a full gateway between a news group and a mailing list. 
$eod 
$copy/log sys$input NONSYSTEM.TXT
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
$copy/log sys$input WRITEMSG.TXT
$deck
BULLETIN contains subroutines for writing a message directly to a folder.  This
would be useful for someone who is using the BBOARD feature, but wants to avoid
the extra overhead of having the message sent to an account as MAIL, and then
have BULLCP read the mail.  It is better if the network mail could be written
directly to the folder bypassing VMS MAIL, as it reduces a lot of cpu overhead.

Call INIT_MESSAGE_ADD to initiate a message addition.
Call WRITE_MESSAGE_LINE to write individual message lines.
Call FINISH_MESSAGE_ADD to complete a message addition.

Calling formats:

	CALL INIT_MESSAGE_ADD(IN_FOLDER,IN_FROM,IN_DESCRIP,IER)
C
C  INPUTS:
C	IN_FOLDER  - Character string containing folder name
C	IN_FROM	   - Character string containing name of owner of message.
C		     If empty, the default is the owner of the process.
C	IN_DESCRIP - Character string containing subject of message.
C		     If empty, the message is searched for a line
C		     which starts with "Subj:" or "Subject:".
C  OUTPUTS:
C	IER - Error status.  True if properly connected to folder.
C		False if folder not found.
C

	CALL WRITE_MESSAGE_LINE(BUFFER)
C
C  INPUTS:
C	BUFFER - Character string containing line to be put into message.
C

	CALL FINISH_MESSAGE_ADD
C
C  NOTE:  Only should be run if INIT_MESSAGE_ADD was successful.
C
$eod 
