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
   defined in the list of equivalence names pointed to by the system logical v
   name BULL_DIR_LIST are allowed.  For example:

	DEFINE/SYSTEM BULL_DIR_LIST SITE$ROOT:[SYSEXE],USER1:[MRL]L

   Then BULL_DIR can be defined as SITE$ROOT:[SYSEXE] or USER1:[MRL].  a
   BULL_DIR_LIST must be defined on all nodes in a cluster. 
   d
   The use of the MARK command to mark messages require that a file be
   created for each user which saves the marked info.  That file file is
   stored in the directory pointed to by the logical name BULL_MARK.  You canh
   either let users who want to use this command define it themselves, ore
   you can define it for them, i.e. DEFINE/SYSTEM BULL_MARK SYS$LOGIN.

5) INSTRUCT.COMf
   This procedure adds 2 permanent messages which give a very brief
   description about the BULLETIN utility, and how to turn off optionalc
   prompting of non-system messages (via SET NOREADNEW).

6) BOARD_SPECIAL.COM
   This command procedure describes and illustrates how to use the
   SET BBOARD/SPECIAL feature.  This feature allows the use of BBOARD 
   where the input does not come from VMS MAIL.  For example, this could
   be used in the case where mail from a non-DEC network is not stored
   in the VMS MAIL.  Another example is BOARD_DIGEST.COM.  This file
   takes mail messages from "digest" type mailing lists and splits themr
   into separate BULLETIN messages for easier reading.

   To use this feature, place the special command procedure into the
   bulletin file directory using the name BOARD_SPECIAL.COM.  If you want 
   to have several different special procedure, you should name the command 
   procedure after the username specified by the SET BBOARD command.

7) UPGRADE.COM
   This procedure is used to upgrade to a new version of BULLETIN.
   See comments for instructions.m

8) MASTER.COMR
   If you are using PMDF, and want to use the BBOARD option, a set ofa
   routines are included which will allow PMDF to write message directly
   into folders, which is a much more effecient way of doing it than
   the normal BBOARD method of using VMS MAIL.  Read PMDF.TXT for how 
   to do this.

9) OPTIMIZE_RMS.COMN
   This routine optimizes index files.  To run, type @OPTIMIZE_RMS.COM
   followed by the filename.  If you omit the filename, it will prompt
   you to allow you to turn off or on several different types of RMS
   compression.  The default is to turn on all types of compression.
   The optimization will cause the file to be compressed.e

   If you use the NEWS feature, it is suggest that you run this procedurer
   on BULLNEWS.DAT after it is created.  Compressing that file greatly speedsf
   up the NEWS update process.  If you are tight on space, and have been
   running BULLETIN for a long time, it might also be useful to compress
   BULLINF.DAT if that file is very large.  However, compressing that (ori
   the other BULLETIN data files) don't appear to save any execution time,
   unlike BULLNEWS.DAT.a
$eod m
$copy/log sys$input BULLDIR.INCo
$deckt
	PARAMETER DIR_RECORD_LENGTH = (100/4)*4

	COMMON /BULL_DIR/ MSG_BTIM,MSG_NUM,DESCRIP,FROM,LENGTH,EX_BTIMe
     &	,SYSTEM,BLOCK,HEADER_BTIM,HEADER_NUM,NEWEST_EXBTIM,NEWEST_MSGBTIM
     &	,NBULL,NBLOCK,SHUTDOWN,SHUTDOWN_BTIM,NEMPTY
     &	,DATE,TIME,EXDATE,EXTIME,NEWEST_EXDATE,NEWEST_EXTIMEH
     &  ,NEWEST_DATE,NEWEST_TIME,SHUTDOWN_DATE,SHUTDOWN_TIME
	CHARACTER*56 DESCRIPn
	CHARACTER*12 FROM
	LOGICAL SYSTEMl

	CHARACTER*12 DATE,EXDATE,NEWEST_EXDATE,NEWEST_DATE,SHUTDOWN_DATEt
	CHARACTER*12 TIME,EXTIME,NEWEST_EXTIME,NEWEST_TIME,SHUTDOWN_TIMEg

	INTEGER MSG_BTIM(2),EX_BTIM(2),HEADER_BTIM(2)
	INTEGER NEWEST_EXBTIM(2),NEWEST_MSGBTIM(2),SHUTDOWN_BTIM(2)

	CHARACTER*(DIR_RECORD_LENGTH) BULLDIR_ENTRY
	EQUIVALENCE (MSG_BTIM,BULLDIR_ENTRY)$

	CHARACTER*52 BULLDIR_HEADER
	EQUIVALENCE (HEADER_BTIM,BULLDIR_HEADER)e

	DATA HEADER_BTIM/0,0/,HEADER_NUM/0/

	CHARACTER MSG_KEY*8

	EQUIVALENCE (MSG_BTIM,MSG_KEY)h

	PARAMETER LINE_LENGTH=255
	PARAMETER INPUT_LENGTH=256o

	COMMON /INPUT_BUFFER/ INPUT
	CHARACTER INPUT*(INPUT_LENGTH)i

	PARAMETER NEWSDIR_RECORD_LENGTH = 180  

	COMMON /NEWS_DIR/ NEWS_MSG_KEY,NEWS_MSG_BTIM_KEY,NEWS_MSGID
     &  ,NEWS_EX_BTIM_KEY,NEWS_POST_BTIM,NEWS_BLOCK,NEWS_LENGTHt
     &  ,NEWS_DESCRIP,NEWS_FROMt
     &  ,NEWS_HEADER_KEY,NEWS_NEWEST_MSG_BTIM_KEYa
     &  ,NEWS_HEADER_FOLDER,NEWS_NEWEST_EX_BTIM_KEY,NEWS_HEADER_NUMp
     &	,NEWS_NBULL
	CHARACTER*64 NEWS_MSGID
	CHARACTER*56 NEWS_DESCRIP
	CHARACTER*12 NEWS_FROMa
	CHARACTER*8 NEWS_MSG_KEY,NEWS_HEADER_KEYt

	CHARACTER*12 NEWS_MSG_BTIM_KEY,NEWS_EX_BTIM_KEY
	CHARACTER*12 NEWS_NEWEST_MSG_BTIM_KEY,NEWS_NEWEST_EX_BTIM_KEY
	INTEGER NEWS_POST_BTIM(2)

	CHARACTER*(NEWSDIR_RECORD_LENGTH) NEWSDIR_ENTRY
	EQUIVALENCE (NEWS_MSG_KEY,NEWSDIR_ENTRY)a

	CHARACTER*64 NEWS_HEADER_FOLDER
	CHARACTER*(NEWSDIR_RECORD_LENGTH) NEWSDIR_HEADERE
	EQUIVALENCE (NEWS_HEADER_KEY,NEWSDIR_HEADER)T
$eod  
$copy/log sys$input BULLETIN.HLP
$deckh
1 BULLETIN
Invokes  the  PFC  BULLETIN  Utility.  This utility is used for reading,
adding and deleting message.  Users are notified at login time that newI
messages have been added and the topics of those messages are displayed.
Reading of those messages is optional.  (Use  the  command  SET  READNEW
while  in BULLETIN for setting automatic reading.)  Privileged users can
add system bulletins that are displayed in full at  login  time.   These
messages  are  also  saved,  and  can be read by BULLETIN.  Messages are
automatically deleted after a specified expiration  date,  or  they  can
manually  be  deleted  by  either  the  submitter  of  the  message or a
privileged user.

 Format:

      BULLETIN [foldername or bulletin interactive command]C

BULLETIN has an interactive help  available  while  using  the  utility.
Type HELP after invoking the BULLETIN command.

If so configured, BULLETIN can also read USENET NEWS.E
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
terminals).o

A user can select, on a folder per  folder  basis,  to  have  a  message
broadcast  to  their  terminal  immediately  notifying  them  when a new
message has been added.h

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
prompt the user for commands.h

NOTE:  Depending on how the BULLETIN command is defined,  triple  quotes
rather than single quotes may be required.
2 /EDITt
Specifies that all ADD or REPLACE commands within BULLETIN will select
the editor for inputting text.
2 /KEYPADy
 /[NO]KEYPAD
Specifies that keypad mode is to be set on, such that the keypad keys 
correspond to BULLETIN commands.  The default is /KEYPAD.T
2 /PAGEt
 /[NO]PAGE

Specifies  whether BULLETIN will stop outputting when it displays a full
screen or not.  /PAGE is the default.   If  /NOPAGE  is  specified,  any
output  will  continue  until it finishes.  This is useful if you have a
terminal which can store several screenfuls of display in its memory.t
2 /PGFLQUOTA
   /PGFLQUOTA=pagesg

Used if you want to specify the page file quota for the BULLCP process.D
2 /STARTUP
Starts up a detached process which will periodically check for expired
messages, cleanup empty space in files, and convert BBOARD mail to
messages.  This is recommended to avoid delays when invoking BULLETIN.
It will create a process with the name BULLCP.  For clusters, this
need be done only on one node.  On all other nodes, the system logical
name BULL_BULLCP should be defined (to anything) in order that BULLETINN
is aware that it is running on another node. (On the local node whereM
BULLCP is running, this logical name is automatically defined.)
2 /STOPR
Stops the BULLCP process without restarting a new one.  (See /STARTUPE
for information on the BULLCP process.)N
2 /SYSTEMA
   /SYSTEM=[days]M

Displays system messages that have been recently added.  The default is2
to show the messages that were added during the last 7 days.  This can
be modified by specifying the number of days as the parameter.
This command is useful for easily redisplaying system messages thatV
might have been missed upon logging in (or were broadcasted but were
erased from the screen.)
2 /WIDTH
   /WIDTH=page_width

Specifies the terminal width for display purposes.  This is used if your
startup procedure is configured such that BULLETIN/LOGIN is executed beforeD
the terminal type is known, and the default width is larger than what theE
terminal type actually is.  I.e. the default width might be 132, but the
real width is 80.  In that case, you should add /WIDTH=80 to BULLETIN/LOGIN.
2 /WSEXTENT
   /WSEXTENT=pages

Used if you want to specify the working set limit for the BULLCP process.T
$eod E
$copy/log sys$input BULLETIN.LNK
$deckA
$ ULIB = "NONE"O
$ IF F$TRNLNM("TWG$TCP") .EQS. "" THEN GOTO LINK
$ ULIB = "PROCESS"
$ DEFINE/USER LNK$LIBRARY TWG$TCP:[NETDIST.LIB]LIBNET_
$ DEFINE/USER LNK$LIBRARY_1 TWG$TCP:[NETDIST.LIB]LIBNETACC
$ DEFINE/USER LNK$LIBRARY_2 TWG$TCP:[NETDIST.LIB]LIBNET
$LINK:
$ LINK/NOTRACE BULL/LIB/INC=BULLETIN$MAIN,SYS$SYSTEM:SYS.STB/SEL-D
        /USERLIB='ULIB'/EXE=BULLETIN,SYS$INPUT/OPT
SYS$SHARE:VAXCRTL/SHAREA
ID="V2.13"
$eod D
$copy/log sys$input BULLFILES.INCU
$deckH
C
C  FOLDER_DIRECTORY IS THE DIRECTORY THAT FILES FOR FOLDERS THAT
C  ARE CREATED ARE KEPT IN.  IF YOU WISH TO PREVENT FOLDER CREATION,
C  YOU SHOULD MODIFY BULLCOM.CLD TO MAKE THE CREATE COMMAND A PRIVILEGED
C  COMMAND (OR SIMPLY REMOVE THE LINES WHICH DEFINE THE CREATE COMMAND).
C 
C  BBOARD_DIRECTORY IS THE SCRATCH AREA USED BY BBOARD WHEN EXTRACTING
C  MAIL.  IF IT IS UNDEFINED, BBOARD WILL NOT BE ABLE TO BE USED.e
C  NOTE THAT EITHER THE BBOARD ACCOUNTS MUST HAVE ACCESS TO THIS DIRECTORY,r
C  OR THE BBOARD ACCOUNTS MUST BE GIVEN SYSPRV PRIVILEGES TO BE ABLE
C  TO WRITE INTO THIS DIRECTORY.  ALSO, FOR BBOARD TO WORK, MAKE SUREt
C  THAT THE SUBPROCESS LIMIT FOR USERS IS AT LEAST 2.  YOU WILL ALSO HAVE 
C  TO INCREASE THE FOLLOWING SYSTEM PARAMETERS WHICH AFFECT DETACHED PROCESES:
C  PQL_DPGFLQUOTA = 15000, PQL_DWSQUOTA = 500, & PQL_DFILLM = 30. 
C  (NOTE: ACCESS CAN BE GIVEN TO THE DIRECTORY FOR THE BBOARD ACCOUNTS USING
C  ACLS, I.E. " SET ACL/ACL=(ID=bboard,ACCESS=R+W)/OBJ=FILE directory.DIR")a
Ce
	COMMON /FILES/ BULLFOLDER_FILE,FOLDER_DIRECTORY,BBOARD_DIRECTORY 
	COMMON /FILES/ BULLUSER_FILE,BULLINF_FILE,NEWS_DIRECTORYt
	COMMON /FILES/ BULLNEWSDIR_FILE,BULLNEWS_FILE
	CHARACTER*80 FOLDER_DIRECTORY /'BULL_DIR:'/
	CHARACTER*80 BBOARD_DIRECTORY /'BULL_DIR:'/
C 
C  NOTE: THE FOLLOWING FILE ARE STORED IN THE FOLDER_DIRECTORY BY DEFAULT.
C  YOU CAN CHANGE THIS BY ADDING A DIRECTORY NAME TO THE FILE NAME.l
C 
	CHARACTER*80 BULLUSER_FILE /'BULLUSER.DAT'/	! Stores user login timer
							! & folder flag settings 
	CHARACTER*80 BULLFOLDER_FILE /'BULLFOLDER.DAT'/	! Stores folder datal
	CHARACTER*80 BULLINF_FILE /'BULLINF.DAT'/	! Stores times of last
							! read messages of users 
	CHARACTER*80 BULLNEWS_FILE /'BULLNEWS.DAT'/	! Stores news group datae
	CHARACTER*80 BULLNEWSDIR_FILE /'BULLNEWSDIR.DAT'/
				! Directory listing for LOCAL news groupsn
Cw
C  THE FOLLOWING IS THE DIRECTORY THAT IS USED TO STORE LOCAL NEWS GROUPS,
C  I.E. NEWS GROUPS THAT ARE COPIED FROM THE NEWS SERVER AND SAVED LOCALLY.b
C  BULLETIN WILL CREATE SUBDIRECTORIES IN THIS DIRECTORY AND THE FILES WILLe
C  BE STORED IN THOSE SUBDIRECTORIES. 
C 
	CHARACTER*80 NEWS_DIRECTORY /'BULL_DIR:'/
$eod a
$copy/log sys$input BULLFOLDER.INC
$decka
!.
!  The following 2 parameters can be modified if desired before compilation.
!r
	PARAMETER BBEXPIRE_LIMIT = 30	! Maxmimum time limit in days thatt
					! BBOARDS can be set to.h
	PARAMETER BBOARD_UPDATE = 15	! Number of minutes between checks
					! for new BBOARD mail. (Note: Check
					! only occurs via BULLETIN/LOGIN.
					! Check is forced via BULLETIN/BBOARD).
					! NOT APPLICABLE IF BULLCP IS RUNNING.f
	PARAMETER ADDID = .TRUE.	! Allows users who are not in thec
					! rights data base to be added.
					! according to uic number. 

	PARAMETER FOLDER_FMT = '(A44,A4,A8,A12,A80,A12,3A4,A8,10A4)'c
	PARAMETER FOLDER_RECORD = 220	! Must be multiple of 4

	COMMON /BULL_FOLDER/ FOLDER,FOLDER_NUMBER,FOLDER_CREATED_DATE,l
     &		FOLDER_OWNER,c
     &		FOLDER_DESCRIP,FOLDER_BBOARD,FOLDER_BBEXPIRE,e
     &		USERB,GROUPB,ACCOUNTB,
     &		F_NBULL,F_NEWEST_BTIM,FOLDER_FLAG,F_EXPIRE_LIMIT,n
     &		F_NEWEST_NOSYS_BTIM,F_START,F_COUNT,F_LAST,w
     &		FOLDER_FILE,FOLDER_SET,FOLDER_NAME
	INTEGER F_NEWEST_BTIM(2)i
	INTEGER F_NEWEST_NOSYS_BTIM(2)
	LOGICAL FOLDER_SET 
	DATA FOLDER_SET /.FALSE./, FOLDER/'GENERAL'/ 
	CHARACTER FOLDER_OWNER*12,FOLDER*44,ACCOUNTB*8,FOLDER_NAME*80
	CHARACTER FOLDER_FILE*80,FOLDER_DESCRIP*80,FOLDER_BBOARD*12
	CHARACTER FOLDER_CREATED_DATE*8

	CHARACTER*(FOLDER_RECORD) FOLDER_COMl
	EQUIVALENCE (FOLDER,FOLDER_COM)

	COMMON /BULL_FOLDER1/ FOLDER1,FOLDER1_NUMBER,FOLDER1_CREATED_DATE,h
     &		FOLDER1_OWNER,
     &		FOLDER1_DESCRIP,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,
     &		USERB1,GROUPB1,ACCOUNTB1,e
     &		F1_NBULL,F1_NEWEST_BTIM,FOLDER1_FLAG,F1_EXPIRE_LIMIT,i
     &		F1_NEWEST_NOSYS_BTIM,F1_START,F1_COUNT,F1_LAST, 
     &		FOLDER1_FILE,FOLDER1_SET,FOLDER1_NAME
	CHARACTER FOLDER1_OWNER*12,FOLDER1*44,ACCOUNTB1*8,FOLDER1_NAME*80
	CHARACTER FOLDER1_FILE*80,FOLDER1_DESCRIP*80,FOLDER1_BBOARD*12
	CHARACTER FOLDER1_CREATED_DATE*8l
	INTEGER F1_NEWEST_BTIM(2)
	INTEGER F1_NEWEST_NOSYS_BTIM(2)

	CHARACTER*(FOLDER_RECORD) FOLDER1_COM
	EQUIVALENCE (FOLDER1,FOLDER1_COM)

	PARAMETER NEWS_FOLDER_FMT = '(A44,A4,2A8,A36,11A4)'
	PARAMETER NEWS_FOLDER_RECORD = 144	! Must be multiple of 4 

	COMMON /NEWS_FOLDER/ NEWS_FOLDER,NEWS_FOLDER_NUMBER,m
     &		NEWS_F_CREATED_DATE,NEWS_F_EXPIRED_DATE,
     &		NEWS_FOLDER_DESCRIP,NEWS_F_START,NEWS_F_COUNT,
     &		NEWS_F_NBULL,NEWS_F_NEWEST_BTIM,NEWS_F_LAST,
     &		NEWS_F_FLAG,NEWS_F_EXPIRE,NEWS_F_FIRST,.
     &		NEWS_F_EXPIRE_LIMIT,NEWS_F_END I
	INTEGER NEWS_F_NEWEST_BTIM(2)
	CHARACTER NEWS_FOLDER*44l
	CHARACTER NEWS_FOLDER_DESCRIP*36a
	CHARACTER*8 NEWS_F_CREATED_DATE,NEWS_F_EXPIRED_DATE

	CHARACTER*(NEWS_FOLDER_RECORD) NEWS_FOLDER_COMt
	EQUIVALENCE (NEWS_FOLDER,NEWS_FOLDER_COM)

        COMMON /NEWS_FOLDER_DEFAULT/ NEWS_FLAG_DEFAULT,c
     &	        NEWS_EXPIRE_DEFAULT,NEWS_EXPIRE_LIMIT_DEFAULT

	COMMON /NEWS_FOLDER1/ NEWS_FOLDER1,NEWS_FOLDER1_NUMBER,
     &		NEWS_F1_CREATED_DATE,NEWS_F1_EXPIRED_DATE,
     &		NEWS_FOLDER1_DESCRIP,NEWS_F1_START,NEWS_F1_COUNT, 
     &		NEWS_F1_NBULL,NEWS_F1_NEWEST_BTIM,NEWS_F1_LAST, 
     &		NEWS_F1_FLAG,NEWS_F1_EXPIRE,NEWS_F1_FIRST,
     &	        NEWS_F1_EXPIRE_LIMIT,NEWS_F1_ENDf
	INTEGER NEWS_F1_NEWEST_BTIM(2) 
	CHARACTER NEWS_FOLDER1*44
	CHARACTER NEWS_FOLDER1_DESCRIP*36
	CHARACTER*8 NEWS_F1_CREATED_DATE,NEWS_F1_EXPIRED_DATE

	CHARACTER*(NEWS_FOLDER_RECORD) NEWS_FOLDER1_COM
	EQUIVALENCE (NEWS_FOLDER1,NEWS_FOLDER1_COM)
$eod t
$copy/log sys$input BULLNEWS.INC
$deckp
	COMMON /NEWS_DEFAULTS/ ORGANIZATION,MAILERi

	CHARACTER*132 ORGANIZATIONl
	DATA ORGANIZATION /'MIT PLASMA FUSION CENTER'/ 

	CHARACTER*12 MAILER
	DATA MAILER /'IN%'/
$eod s
$copy/log sys$input BULLUSER.INC
$decku
! 
! The parameter FOLDER_MAX should be changed to increase the maximum number 
! of folders available.  Due to storage via longwords, the maximum number 
! available is always a multiple of 32.  Thus, it will probably make sense
! to specify a multiple of 32 for FOLDER_MAX, as that it what really will be
! the capacity.  Note that the default general folder counts as a folder also,
! so that if you specify 64, you will be able to create 63 folders on your own.a
!t
	PARAMETER FOLDER_MAX = 96
	PARAMETER FLONG = (FOLDER_MAX + 31)/ 32

	PARAMETER USER_RECORD_LENGTH = 28 + FLONG*16i
	PARAMETER USER_FMT = '(A12,<4+FLONG*4>A4)'t
	PARAMETER USER_HEADER_KEY = '            'o

	COMMON /HEADER_INFO/ TEMP_USER,BBOARD_BTIM,NEWEST_BTIM,USERPRIV
	COMMON /HEADER_INFO/ SET_FLAG_DEF,BRIEF_FLAG_DEFt
	COMMON /HEADER_INFO/ NOTIFY_FLAG_DEF 
	CHARACTER TEMP_USER*12d
	DIMENSION BBOARD_BTIM(2),NEWEST_BTIM(2),USERPRIV(FLONG)
	DIMENSION SET_FLAG_DEF(FLONG),BRIEF_FLAG_DEF(FLONG)
	DIMENSION NOTIFY_FLAG_DEF(FLONG)s

	COMMON /BULL_USER/ USERNAME,LOGIN_BTIM,READ_BTIM,
     &		NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	CHARACTER*12 USERNAME
	DIMENSION LOGIN_BTIM(2),READ_BTIM(2)m
	DIMENSION NEW_FLAG(FLONG)   ! Used to indicate new message in folderr
				    ! Now NEW_FLAG(2) contains SET GENERIC daysb
	DIMENSION SET_FLAG(FLONG)   ! Bit set indicates READNEW set for folderw
	DIMENSION BRIEF_FLAG(FLONG) ! Bit set indicates READNEW/BRIEF set
	DIMENSION NOTIFY_FLAG(FLONG)! Bit set indicates to broadcast 
				    ! notification when new bulletin is added.

	CHARACTER*(USER_RECORD_LENGTH) USER_ENTRY,USER_HEADER
	EQUIVALENCE (USER_ENTRY,USERNAME)
	EQUIVALENCE (USER_HEADER,TEMP_USER)

	COMMON /FOLDER_TIMES/ LAST_READ_BTIM(2,0:FOLDER_MAX)"
	   ! Must start with 0 to store info for folder specified with ::
	COMMON /SYS_FOLDER_TIMES/ LAST_SYS_BTIM(2,FOLDER_MAX)
	   ! Last read times for each folder as stored in BULL_DIR:BULLINF.DATT
	COMMON /NEWS_TIMES/ LAST_NEWS_READ(2,FOLDER_MAX)U
	INTEGER*2 LAST_NEWS_READ2(4,FOLDER_MAX)
	EQUIVALENCE (LAST_NEWS_READ2(1,1),LAST_NEWS_READ(1,1)):
	   ! Last read times for each folder as stored in BULL_DIR:BULLINF.DATd

	COMMON /NEW_MESSAGES/ NEW_MSG
	DIMENSION NEW_MSG(FLONG)   ! Flag showing new messages detected
$eod  
$copy/log sys$input BULL_NEWS.CO
$deckY
#include <descrip.h>
#include "sys$library:iodef.h"

#if MULTINET

#include "multinet_root:[multinet.include.sys]types.h"
#include "multinet_root:[multinet.include.sys]socket.h"E
#include "multinet_root:[multinet.include.netinet]in.h"F
#include "multinet_root:[multinet.include]netdb.h"
#include "multinet_root:[multinet.include]errno.h"
#include "multinet_root:[multinet.include.vms]inetiodef.h"

static char inet[7] = "INET0:";R
$DESCRIPTOR(inet_d,inet);L

#elseB

#if UCXK

#include <ucx$inetdef.h>

struct sockaddr {E
  short inet_family;
  short inet_port;
  int inet_adrs;
  char bklb[8];A
  };

struct itlist { int lgth; struct sockaddr *hst; };

static short sck_parm[2];&
static struct sockaddr local_host, remote_host; 
struct itlist lhst_adrs, rhst_adrs;N

static char ucxdev[11] = "UCX$DEVICE";
$DESCRIPTOR(ucxdev_d,ucxdev);e

static int addr_buff; 

#define htons(x) ((unsigned short)((x<<8)|(x>>8)))

#elseM

#if TWG 

#include <types.h>
#include <socket.h>
#include <netdb.h>
#include <in.h>N
#include <inetiodef.h>

static char inet[6] = "INET:";
$DESCRIPTOR(inet_d,inet);Y

#elseI

#define CMU 1:
static char ip[4] = "IP:";
$DESCRIPTOR(ip_d,ip);O

#endif

#endif

#endif

static char task[20];C
$DESCRIPTOR(task_d,task);l

static int s;8

static struct iosb {
	short status;
	short size;
	int info;
} iosb;e

#define TCP 0
#define DECNET 1

static int mode = TCP;

news_get_chan()r
{return(s);}

news_set_chan(i)
int *i;F
{s = *i;}o

news_disconnect()	
{	
#if UCXe
	sys$cancel(s);
	sys$qiow(0,s,IO$_DEACCESS,0,0,0,0,0,0,0,0,0);
#endif
	sys$dassgn(s);C
}A

#if MULTINET || TWGE

static struct hostent *hp;
static struct sockaddr_in sin;

#endif

int *node;

news_gethost()
{ 
	/*I
	 *  Get the IP address of the NEWS host. 
	 *  As of MULTINET 3.0, cannot be done at AST level
	 *  so can't do in NEWS_ASSIGN(), as BULLCP calls it at
	 *  AST level if the decnet gateway feature is used.S
	 */
#if TWG || (MULTINET && defined(__GNUC__))
	struct hostent *gethostbyname(); 
#elseu
#if MULTINET
	struct hostent *GETHOSTBYNAME1();
#endif
#endif

	node = getenv("BULL_NEWS_SERVER");.
	if (!node) return(0);
	if (!strchr(node,'.')) return(1);  

#if TWG || (MULTINET && defined(__GNUC__))
	hp = gethostbyname(node);
#elser
#if MULTINET
	hp = GETHOSTBYNAME1(node);B
#endif
#endif
	return(1);	
}o

news_assign()L
{I
	int n;

	if (!strchr(node,'.')) {L
	   strcpy(&task[0],node);
	   n = strlen(node);R
	   strcpy(&task[n],"::\"TASK=NNTP\"");o
	   task_d.dsc$w_length = 13 + n;!
	   if (!(sys$assign(&task_d,&s,0,0) & 1)) return(0); 
	   mode = DECNET;
	   return(1);
	}
#if MULTINET || TWGA
	/*1
	 *  Create a "sockaddr_in" structure which describes the remote
	 *  IP address we want to send to (from gethostbyname()).
	 */

        if (!hp) {
          int h[4],i;P
          if (sscanf(node,"%d.%d.%d.%d",&h[0],&h[1],&h[2],&h[3]) == 4) {
            for (i=0;i<4;i++) if (h[i] < 0 || h[i] > 255) return(0);
	    sin.sin_addr.s_addr = (h[3]<<24)+(h[2]<<16)+(h[1]<<8)+(h[0]);
	  } else
	    return(0);_
	  sin.sin_family = AF_INET;
	}
        else {
 	  sin.sin_family = hp->h_addrtype;
	  memcpy(&sin.sin_addr, hp->h_addr, hp->h_length);F
        }C
#if TWG || (MULTINET && defined(__GNUC__))
	sin.sin_port = htons(119);R
#else*
	sin.sin_port = HTONS1(119);
#endif

	/*A
	 *  Create an IP-family socket on which to make the connection)
	 */

	if (!(sys$assign(&inet_d,&s,0,0) & 1)) return(0);
#elseA
#if UCX 
        if (!(sys$assign(&ucxdev_d,&s,0,0) & 1)) return(0);L
	{
           short retlen;
	   struct dsc$descriptor host_nameL
		= {strlen(node),DSC$K_CLASS_S,DSC$K_DTYPE_T,node};
	   int comm = INETACP$C_TRANS * 256 + INETACP_FUNC$C_GETHOSTBYNAME;
	   struct dsc$descriptor commandA
		= {4,DSC$K_CLASS_S, DSC$K_DTYPE_T,&comm};F
	   struct dsc$descriptor host_adF
		= {4,DSC$K_CLASS_S, DSC$K_DTYPE_T,&addr_buff};
	   struct iosb nam_iosb;*

           if (!(sys$qiow(0,s,IO$_ACPCONTROL,&nam_iosb,0,0,)
                       &command,&host_name,&retlen,&host_ad,0,0) & 1)R
               || !(nam_iosb.status & 1)) {A
              sys$dassgn(s);
	      return(0); 
	   }u
	}
#elsep
	if (!(sys$assign(&ip_d,&s,0,0) & 1)) return(0);
#endif
#endif
	return(1);S
}C

news_socket()F
{P
	if (mode == DECNET) return (1);

#if MULTINET || TWG_
	if (!(sys$qiow(0,s,IO$_SOCKET,&iosb,0,0,sin.sin_family,
	    SOCK_STREAM,0,0,0,0) & 1) || !(iosb.status & 1)) { 
	   sys$dassgn(s);
	   return(0);
	}
#endif
#if UCXE
	sck_parm[0] = INET$C_TCP;
	sck_parm[1] = INET_PROTYP$C_STREAM;
	local_host.inet_family = INET$C_AF_INET;E
	local_host.inet_port = 0;
	local_host.inet_adrs = INET$C_INADDR_ANY;
	lhst_adrs.lgth = sizeof local_host;
	lhst_adrs.hst = &local_host;F
	if (!(sys$qiow(0,s,IO$_SETMODE,&iosb,0,0,&sck_parm,0,
	   &lhst_adrs,0,0,0) & 1) || !(iosb.status & 1)) {N
           sys$qiow(0,s,IO$_DEACCESS|IO$M_SHUTDOWN,&iosb,0,0,0,0,0,E
						UCX$C_DSC_ALL,0,0);,
	   sys$dassgn(s);
	   return(0);
	}
#endif

	return(1); 
}&

news_socket_bullcp(efn,biosb,astadr,astprm)T
int *biosb,*astadr,*astprm,*efn;
{R
	if (mode == DECNET) return (1);

#if MULTINET || TWGS
	if (!(sys$qio(*efn,s,IO$_SOCKET,biosb,astadr,*astprm,sin.sin_family,
	    SOCK_STREAM,0,0,0,0) & 1) ) return(0);R
#else_
#if UCXD
	sck_parm[0] = INET$C_TCP;
	sck_parm[1] = INET_PROTYP$C_STREAM;
	local_host.inet_family = INET$C_AF_INET;W
	local_host.inet_port = 0;
	local_host.inet_adrs = INET$C_INADDR_ANY;
	lhst_adrs.lgth = sizeof local_host;
	lhst_adrs.hst = &local_host;l
	if (!(sys$qio(0,s,IO$_SETMODE,biosb,astadr,*astprm,&sck_parm,0,
	   &lhst_adrs,0,0,0) & 1) ) return(0);c
#else 
	return(-1);
#endif
#endif

	return(1);a
}e

news_create()l
{e
	if (mode == DECNET) return (1);

#if MULTINET || TWGa

	/* 
	 *  Do a psuedo-connect to that address. This tells the kernel that
	 *  anything written on this socket gets sent to this destination. It
	 *  also binds us to a local port number (random, but that is ok). 
	 */

	if (!(sys$qiow(0,s,IO$_CONNECT,&iosb,0,0,&sin,sizeof(sin),0,0,0,0) & 1)
	    || !(iosb.status & 1)) {e
	   sys$dassgn(s);
	   return(0);
	}
#elseP
#if UCX 
        remote_host.inet_family = INET$C_AF_INET;)
        remote_host.inet_port = htons(119);F
	remote_host.inet_adrs = addr_buff;,
	rhst_adrs.lgth = sizeof remote_host;R
	rhst_adrs.hst = &remote_host;
	if (!(sys$qiow(0,s,IO$_ACCESS,&iosb,0,0,0,0,&rhst_adrs,0,0,0) & 1)H
	    || !(iosb.status & 1)) {F
           sys$qiow(0,s,IO$_DEACCESS|IO$M_SHUTDOWN,&iosb,0,0,0,0,0,R
						UCX$C_DSC_ALL,0,0);M
	   sys$dassgn(s);
	   return(0);
	}
#elseS
	if (!(sys$qiow(0,s,IO$_CREATE,&iosb,0,0,node,119,0,1,0,300) & 1)F
	    || !(iosb.status & 1)) { 
	   sys$dassgn(s);
	   return(0);
	}
#endif
#endif

	return(1);T
}_

news_create_bullcp(efn,biosb,astadr,astprm)B
int *biosb,*astadr,*astprm,*efn;
{F
	if (mode == DECNET) return (1);

#if MULTINET || TWG	

	/* 
	 *  Do a psuedo-connect to that address. This tells the kernel that
	 *  anything written on this socket gets sent to this destination. It
	 *  also binds us to a local port number (random, but that is ok).O
	 */

	if (!(sys$qio(*efn,s,IO$_CONNECT,biosb,astadr
		,*astprm,&sin,sizeof(sin),0,0,0,0) & 1)) return(0);C
#elseG
#if UCXE
        remote_host.inet_family = INET$C_AF_INET;)
        remote_host.inet_port = htons(119);M
	remote_host.inet_adrs = addr_buff;:
	rhst_adrs.lgth = sizeof remote_host;o
	rhst_adrs.hst = &remote_host;
	if (!(sys$qio(*efn,s,IO$_ACCESS,biosb,astadr,*astprm,0,
		0,&rhst_adrs,0,0,0) & 1)) return(0);
#elses
	if (!(sys$qio(*efn,s,IO$_CREATE,biosb,astadr,*astprm,node,E
		119,0,1,0,300) & 1))
	   return(0);
#endif
#endif

	return(1);V
}N

news_connect()
{,
	if (!news_gethost()) return(0);
	if (!news_assign()) return(0);s
	if (!news_socket()) return(0);
	return(news_create());W
}G

news_write_packet(buf)

struct dsc$descriptor_s *buf;e
{t
	static int n,len;

	len = buf->dsc$w_length;c
#if CMUu
	if (!(sys$qiow(0,s,IO$_WRITEVBLK,&iosb,0,0,buf->dsc$a_pointer,
					len,0,!mode,0,0) & 1)
	    || !(iosb.status & 1)) return(0);
#elser
	if (!(sys$qiow(0,s,IO$_WRITEVBLK,&iosb,0,0,buf->dsc$a_pointer,l
					len,0,0,0,0) & 1)
	    || !(iosb.status & 1)) return(0);
#endif

	return(1);u
}"

news_write_packet_bullcp(efn,biosb,astadr,astprm,buf,len)r
int *biosb,*astadr,*astprm,*efn,*buf,*len;
{c
#if CMUt
	if (!(sys$qio(*efn,s,IO$_WRITEVBLK,biosb,astadr,*astprm,buf,C
					*len,0,!mode,0,0) & 1)) return(0);s
#else 
	if (!(sys$qio(*efn,s,IO$_WRITEVBLK,biosb,astadr,*astprm,buf,
					*len,0,0,0,0) & 1)) return(0);s
#endif

	return(1);c
}d

news_read_packet(buf)t
struct dsc$descriptor_s *buf;o
{d
	static int n,len;

	len = buf->dsc$w_length;a
	if (!(sys$qiow(0,s,IO$_READVBLK,&iosb,0,0,buf->dsc$a_pointer,
					len,0,0,0,0) & 1)
	    || !(iosb.status & 1)) return(0);
	n = iosb.size;e

	return(n);|
}>

news_gethostname(buf) 

struct dsc$descriptor_s *buf;<
{k
	if (mode == DECNET) return (-1);e
#if TWG || (MULTINET && defined(__GNUC__))
	return(gethostname(buf->dsc$a_pointer, buf->dsc$w_length));
#elsen
#if MULTINET
	return(GETHOSTNAME1(buf->dsc$a_pointer, buf->dsc$w_length));d
#elsee
	return(-1);
#endif
#endif
}D
$eod O
$copy/log sys$input HANDOUT.TXT8
$deckt
               Introduction to BULLETIN on the Vax
                                                  2/88 AWt

PUBLISHED BY THE DREW UNIVERSITY ACADEMIC COMPUTER CENTER. MAY BE(
COPIED WITH WRITING CREDIT GIVEN TO DREW UNIVERSITY.

BULLETIN was written for the Public Domain by Mark London at MIT.0

     The BULLETIN utility permits a user to create messages fors
reading by other users.  Users may be notified upon logging on
that new messages have been added, and what the topic of the
messages are.  Actual reading of the messages is optional.  (See
the command SET READNEW for info on automatic reading.)  Messages
are automatically deleted when their expiration data has passed.
     The program runs like VAX mail.  The different interest
groups or BULLETIN boards are implemented in the form of
'Folders', just like a filing cabinet.  A Folder contain various
messages on the same general topic.  A message is a piece of text
written by a user or staff person and added to a particularm
folder.  All users are not permitted to submit messages to all
folders.

     A message consists of an expiration date, a subject linec
and the text of the message.  BULLETIN will prompt the user for)
these things when a message is being added.

     Several different folders are currently defined toa
BULLETIN.  The General Folders will be used by Computer Center
Staff to post messages of general interest concerning the VAX to
the user community.  If something is of an important nature, itn
will be posted in the General folder as a 'System' message. 
This is a special message type.  It will be displayed to each0
user  as they log in the first time after that message was
posted.  This will be done automatically by BULLETIN on login.
Once a particular system message has been displayed, it will not
be displayed for that user on subsequent logins.

Folders 

     Different folders have been created to contain messages ond
different topics.  Folders may be public, semi-private, or
private.  The majority of the folders will be public.  However a
few will be semi-private, which will mean that all users may
read messages in the folder but not all will be able to post toi
it.  Currently, there are several folders defined:

GENERAL -- system messages

PUBLIC_ANNOUNCEMENTS -- Can be used by anyone to post messages
of interest to the publicm

On Beta:
AIDE STATION -- Private folder for Computer Center Employees

In addition on Alpha there are folders that receive electronic
magazines, such as:A
NETMONTH --  The monthly magazine of BITNET information.
RISKS -- Identifying the risks involved in using computers.,
INFOIBMPC -- Information about the IBM personal computers.
INFOVAX -- Information on the Digital VAX.
PROGRAMMING_JOURNALS-Includes MINIX, UNIX and C, Modula-2 and,
Prolog journals 
watch for new ones being added. 

Using BULLETIN

     BULLETIN is invoked by type the command 'BULLETIN' (or BULL,s
for short) at the '$' prompt.  BULLETIN will display its prompt
'BULLETIN>'. Help is available from DCL command level ($) or fromU
within the BULLETIN program itself by typing the word 'HELP'.  Tom
leave the BULLETIN program, type 'EXIT'.

To see what is there

     In order to see message and folders, on can use the
'Directory' command. Upon entering BULLETIN, the user is place
in the General folder.  If the user wishes to see which folders 
exist, the directory/folders command is used. for example:
typing:e

     BULLETIN> directory/folders

will make a display like:,

      Folder                       Owner
     *GENERAL                      SYSTEM 
     *PUBLIC_ANNOUNCEMENTS         BBEYER$
      NETMONTH                     BITNETS
     *VAX_SIG                      BBEYERn

An asterisk (*) next to the folder name indicates you have unreada
messages in that folder.

The command 'DIRECTORY/FOLDERS/DESCRIBE' would list all availableU
folders, along with a brief description of each.

     To switch from one folder to another folder, the user may
execute the 'SELECT' command.  For example, the followingC
command would show what a user would do to switch to the folder 
called PUBLIC_ANNOUNCEMENTS:

BULLETIN> SELECT PUBLIC_ANNOUNCEMENTS 

and BULLETIN would respond:l
     Folder has been set to PUBLIC_ANNOUNCEMENTS

     Now the user may get a list of the messages in this folderr
by issuing the directory command with no qualifiers.
This command, for example:
BULLETIN> DIRECTORYe
would have bulletin respond:

 #     Description               From                  Date 
 1     CHRISTMAS PARTY           oleksiak              26-JUN-88
 2     Learning about BULLETIN   oleksiak              26-JUN-87
 3     VAX MAIL                  LLLOYD                01-Jan-87

     The command 'DIR/NEW' will list just unread messages.


Reading messages

     In order to read messages in a folder, the user may typee
the read command or he/she may simply type the number of the
message he wishes to read.  The message numbers can be acquiredF
by doing the 'DIRECTORY' command.  If the user hits a carriage
return with no input whatsoever,  BULLETIN will type the first
message in the folder, or if there are new messages present, ito
will type the first new message in the folder.

     If a folder contains the above messages (as seen by the
'Directory' command) then these messages can be read by:

BULLETIN> READ
and BULLETIN would respond:,

Message number:  1                       PUBLIC_ANNOUNCEMENTS;
Description: CHRISTMAS PARTY
Date:  26-JUN-1988 8:08:40   Expires:  1-JAN-1989 08:08:40

...Body of message.....*

     Should the user only wish to see message number 3, he can
enter the 'READ' command with the message number as a parameter.
for example:

BULLETIN> READ 3

     There are three other useful commands that can be used at
the 'BULLETIN>' prompt when reading messages. These are:

BACK - Read the message preceding the message currently beinga
read.*

CURRENT - Start reading the current message at the top.  This is
useful for someone who is reading a message and wishes to reread
it from the beginning.

NEXT - Start reading from the beginning of the next message.
This is handy if the user is reading a very long message and
wants to skip to the next one.

Saving the interesting stuff.s

     If the user sees something which he/she wants a copy of,E
the extract command can be use to write an ASCII copy of the
message into a file.  This command works on the current message
being read.  It requires the name of the file into which to save
the message.  If the file name is not given, the user will beG
prompted for it.  For example:

BULLETIN>  Read 2 

********** Message on Screen ********b

A person could then type
BULLETIN> extract,
file:  FV.TXT,
BULLETIN>u

BULLETIN has now saved the contents of message number 2 into the
file name 'FV.txt'.e
     If the file to which the user is writing already exists,i
BULLETIN will append the message to the file.  The user cane
force BULLETIN to write a new file containing only the message
being saved by using the '/new' qualifier in the 'extract'
command.  These messages can then be sent to other users, or
downloaded for use in Wordperfect.  (See "Mail on the Vax", or
"Transferring a file between a PC and the VAX").

This command may be useful if you wish to transfer the message to)
your PC, perhaps using a BITNET journal message as a reference inc
a paper. Once the file is saved, you can transfer it to a PC by_
following the instructions in the handout 'Transferring files_
from the PC to the VAX of from the VAX to a PC".

Adding messages0
     A user may add a message to a folder by selecting the
folder and then using the 'ADD' command.  This is provided that)
the user is adding the message to a public folder.  The user has
the option of giving the 'ADD' command and typing a message using 
the VAX editor or uploading a message from your PC (sees
documentation), or add a message you have extracted from VAX
mail.  BULLETIN will prompt for the expiration date and subjecto
line.  It will then add the text of the file as the body of the 
message. To add a message that is stored in a file (from MAIL or
from your PC, for example) type:

          ADD filename

If the user does not specify a file name, he/she will be
prompted to enter the body of the message.  The user may alsoe
use the EDT text editor by issuing the command with thee
'/EDIT'option.

For example:
BULLETIN> sel PUBLIC_ANNOUNCEMENTS
          folder has been set to PUBLIC_ANNOUNCEMENTSt
BULLETIN> ADD MESS.TXT

IT IS 10-JUL-1988 12:41:06.15.  SPECIFY WHEN THE MESSAGE SHOULDi
EXPIRE:  ENTER ABsolute TIME:  <DD-MMM-YYYY]HH:MM:SS OR DELTAp
TIME: DDD HH:MM:SS

A user then type the date of expiration and press the 'return'
button.  The time input may be ignored. For example, typing:
20-JUL-1988 or type "10" - for ten days in the future.

BULLETIN responds:
ENTER DESCRIPTION HEADER.  LIMIT HEADER TO 53 CHARACTERS.e

Now the user may enter the subject of the message.

BULLETIN>l

The above session adds the text in the file 'mess.txt' as the 
next message in the PUBLIC_ANNOUNCEMENTS Folder.  The messagen
will be deleted automatically on the 20th of July as requested
by the user adding the message.i

Asking BULLETIN to notify you of new messages upon logging in.

     If the user wishes to get notification on login when newt
messages are in a folder, he should use the 'READNEW' option.A
This command does not force the reader to reading new messages,,
only gives notification.  To do this, 'SELECT' each folder you
are interested in and do a 'SET READNEW' command while set toc
that folder.

Example:

BULLETIN> Select PUBLIC_ANNOUNCEMENTSt
folder has been set to PUBLIC_ANNOUNCEMENTST
BULLETIN> SET READNEWr

Alternately, you may type SET SHOWNEW. This will just display ad
message notifying you that there are new messages.

Mailing a BULLETIN message

     A user may directly mail another user a message found in the 
BULLETIN.  While reading the message that he/she desires to send,l
at the 'BULLETIN>' type 'MAIL'.  The Vax will then ask to whom
you wish to send the information too.t

Check the BULLETIN DISCUSSION folder on ALPHA for new additions.
If you have comments or questions about BULLETIN, leave them
there.
$eod C
$copy/log sys$input INSTRUCT.TXT
$decke
This message is being displayed by the BULLETIN facility.  This is a non-DEC
facility, so it is not described in the manuals.  Messages can be submitted by
using the BULLETIN command.  System messages, such as this one, are displayedT
in full, but can only be entered by privileged users.  Non-system messages can
be entered by anyone, but only their topics will be displayed at login time,
and will be prompted to optionally read them.  (This prompting feature can bel
disabled).  All bulletins can be reread at any time unless they are deleted or
expire.  For more information, see the on-line help (via HELP BULLETIN). h
$eod  
$copy/log sys$input NEWS.TXT
$decky
BULLETIN has the capability to read and post messages to USENET NEWS in af
client mode.  News groups can also be stored on disk.  Selected groups ore
set of groups which are commonly read can be selected to be stored, thus makingo
reading of such groups much faster than having to access them over a network.L
Note that since the number of groups is well over 2000 makes it unreasonable at 
most sites to store them all.f

BULLETIN (actually BULLCP) can act as as a gateway between decnet and tcp fors
NEWS, which allows decnet nodes without tcp access to be able to access a tcp 
news server.  This method does not require spawning any processes, since the
detached process BULLCP is always present, so the access is very fast.

Since BULLETIN uses a shared database to store info on the NEWS groups and
periodically updates it, there is no need for that to be done when a userS
accesses the NEWS groups.  Several other NEWS readers do this when you run
them, which is why they take a long time to start up.d

It is also possible to feed NEWS groups into a "real" BULLETIN folder.
 
Presently, BULLETIN can be used with either UCX, MULTINET, or CMU TCP/IP
packages (and of course DECNET) for reading NEWS.  Support for other packagesw
can be added if I can find sites willing to beta test the interface for me. 
The source for the TCP interface is in C rather than FORTRAN because the
MULTINET include files are in C. However, if you do not have C, I will be glad
to send the object for it (or to even possibly rewrite the code in FORTRAN).  

The instructions for installation are as follows.  Define BULL_NEWS_SERVER
to be a system logical name pointing to either your internet or decnet NEWS 
node.  If it is decnet, simply specify the decnet node name, i.e. 

	$ DEFINE/SYSTEM BULL_NEWS_SERVER NERUSd

BULLETIN decides to use DECNET rather than TCP access based on the node name.h
If it does not have any periods in it, then it assumes it is a DECNET node.e

If you have a cluster where one node is an internet node, and the rest
non-internet nodes, you'll have to create a startup procedure that defines
BULL_NEWS_SERVER to be the internet news server address only on the node (or
nodes) on the cluster that have actually internet access.  The other nodes will
have BULL_NEWS_SERVER defined as the decnet node name that BULLCP is running onn
in the cluster.  (Of course, BULLCP will have to be running on a node with
internet access.)g
                          
NOTE: If you want to disable the gateway feature, then before starting BULLCP,
define the logical name:

	$ DEFINE/SYSTEM BULL_NO_NEWS_GATEWAY "TRUE"

Defining this will only shut off the gateway.  BULLETIN will still be alloweda
to read NEWS from the local node as long as BULL_NEWS_SERVER is defined.

You can also specify that BULLCP is only to act as a NEWS gateway.  This is to
allow adding the news gateway to an INTERNET site that you have DECNET accessn
to, but which does not want to make use of any of the other BULLETIN features.
You would specify the following command before starting BULLCP:e

	$ DEFINE/SYSTEM BULL_NEWS_GATEWAY_ONLY "TRUE"

In order to post messages, BULLETIN needs to know the internet nodename of
the local host.  This is done automatically for nodes running MULTINET.  For
other nodes, BULLETIN attempts to translate the logical name ARPANET_HOST_NAME,s
INTERNET_HOST_NAME, and MX_NODE_NAME.  If you are on a DECNET node that is not
on INTERNET (and is not part of a cluster which has an INTERNET address), butd
you are accessing NEWS via DECNET, you can specify the hostname as follows:f

     $ DEFINE/SYSTEM INTERNET_HOST_NAME "%localhost@internet-address"

Where "localhost" is your local decnet hostname, and "internet-address" is the
internet address of the gateway node..

The local time zone is detected by looking at the following logical names:
LISP$TIME_ZONE, MULTINET_TIMEZONE, or PMDF_TIMEZONE.  (LISP$TIME_ZONE is
defined if you have LISP installed.)

The name of the organization is included in the header of the NEWS message.c
This can be anything, but usually is the company or university name.  This
can be hardcoded into the source by putting in BULLNEWS.INC, or by definingn
the system logical name BULL_NEWS_ORGANIZATION. 

The name of the mail protocol to use for responding by mail to NEWS messages
can also be either hardcoded by putting in BULLNEWS.INC, or by defining theT
system logical name BULL_NEWS_MAILER.

After installing the new BULLETIN, execute the command NEWS, which asks for an
list of all the news groups.  Because this is the first time it is executed, ite
will cause a load of all the remote news groups into a local data base
(BULL_DIR:BULLNEWS.DAT). This will take several minutes to do.  It is the only
time that this load will be done interactively.  Afterwards, BULLCP will
periodically update the data base. BULLCP will update NEWS every hour.  If you
want to change this frequency, define the logical name BULL_NEWS_UPDATE to the
number of minutes in between updates, i.e. DEFINE/SYSTEM BULL_NEWS_UPDATE "30"
for 30 minutes.  NOTE: BULLCP will create a subprocess BULLCP NEWS which does 
the update.  You can watch how long it takes for this to run in order to
determine if you want to change the update period).  If you ever want to force
NEWS to be updated, simply restart BULLCP.

It is suggested that you run OPTIMIZE_RMS.COM on BULLNEWS.DAT, as it will causes
the file to be compressed and will allow updates to run much faster (factor of
5 or more).                                                       

Never delete BULLNEWS.DAT.  There is no reason to ever do so, and it will causeA
subscribed users to be subscribed to the wrong news groups.:

WARNING: One user discovered that his server (using bnews?) had a bug which 
caused the updates to cause bogus "new messages" notifications for subscribed:
NEWS group when entering BULLETIN.  If you experience this problem, tryo
defining the system logical name BULL_SPECIAL_NEWS_UPDATE.  This will causes
the update to use a different algorithm which should eliminate the problem,d
although it requires much more time to execute.m

News groups can be specified as being stored on disk via the SET NEWS command. a
See the online help for more info.  After converting such groups, when BULLCPg
wakes up, it will start the storing process.  This can take a long time if you
have a lot of groups.  An index file pointing to the stored messages is createds
and called BULL_DIR:BULLNEWSDIR.DAT.  After the storage process is complete youe
should consider running OPTIMIZE_RMS.COM on it (and anytime after you convert ae
sizable amount of groups).
                                              
It is possible to automatically have news messages to be fed into a real
folder. Place the name of the news group into the folder description surroundedE
by <>, i.e. <misc.test>.  It must be in lower case.  (Other text is allowed in
the description, i.e. "THIS IS A TEST FOLDER <misc.test>".)i

If you have any problems or questions, please let me know.
									MRL
P.s.
	If you do not know what USENET NEWS, it's basically news messages which
are passed between nodes.  Originally it was limited to USENET, but that is no
longer the case.  Unlike internet mailing lists which use MAIL to send the
messages to individuals, NEWS messages are not sent via MAIL.  They are passed
between nodes using a special protocol, NNTP.  Users must use a NEWS readern
package to read them.  However, it is possible to read NEWS remotely over am
network, and therefore avoiding having to actually store the messages.
BULLETIN is setup to be used mainly in this client mode, i.e. it can readn
messages on another node via TCP or DECNET.  This is useful, since the numbera
of NEWS groups total over 1000, the disk space required for storage is veryp
high.  If you are interested in finding a server node that would allow you tos
read NEWS, and do not know of one (i.e. a USENET node), I know of no officialH
way of doing so.  However, one suggestion was to try connecting to BBN.COM via
ANONYMOUS FTP and look through the directory uumap/comp.mail.maps to find as
USENET node near you to contact.  
$eod e
$copy/log sys$input NONSYSTEM.TXTr
$deckn
Non-system bulletins (such as this) can be submitted by any user.  Users are
alerted at login time that new non-system bulletins have been added, but onlyr
their topics are listed.  Optionally, users can be prompted at login time to
see if they wish to read the bulletins.  When reading the bulletins in this 
manner, the bulletins can optionally be written to a file.  If you have theo
subdirectory [.BULL] created, BULLETIN will use that directory as the defaulte
directory to write the file into.C

A user can disable this prompting featuring by using BULLETIN as follows: 

$ BULLETIN
BULLETIN> SET NOREADNEWr
BULLETIN> EXIT

Afterwords, the user will only be alerted of the bulletins, and will have to
use the BULLETIN utility in order to read the messages.y
$eod 
$copy/log sys$input WRITEMSG.TXT
$deckt
BULLETIN contains subroutines for writing a message directly to a folder.  Thisl
would be useful for someone who is using the BBOARD feature, but wants to avoidP
the extra overhead of having the message sent to an account as MAIL, and thenw
have BULLCP read the mail.  It is better if the network mail could be written
directly to the folder bypassing VMS MAIL, as it reduces a lot of cpu overhead.T

Call INIT_MESSAGE_ADD to initiate a message addition. 
Call WRITE_MESSAGE_LINE to write individual message lines.
Call FINISH_MESSAGE_ADD to complete a message addition.i

Calling formats:

	CALL INIT_MESSAGE_ADD(IN_FOLDER,IN_FROM,IN_DESCRIP,IER)
Co
C  INPUTS:
C	IN_FOLDER  - Character string containing folder name
C	IN_FROM	   - Character string containing name of owner of message.
C		     If empty, the default is the owner of the process.
C	IN_DESCRIP - Character string containing subject of message.
C		     If empty, the message is searched for a line
C		     which starts with "Subj:" or "Subject:".
C  OUTPUTS: 
C	IER - Error status.  True if properly connected to folder.
C		False if folder not found.L
CW

	CALL WRITE_MESSAGE_LINE(BUFFER)
C 
C  INPUTS:
C	BUFFER - Character string containing line to be put into message.c
Cs

	CALL FINISH_MESSAGE_ADD
CB
C  NOTE:  Only should be run if INIT_MESSAGE_ADD was successful.
Cg
$eod n
