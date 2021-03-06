$set nover
$copy/log sys$input AAAREADME.TXT
$deck
The following are instructions for creating and installing the BULLETIN
utility. None of the command procedures included here are sophisticated, so it
is likely that several modifications will have to be made by the installer.
The installer should enable all privileges before installation.
 
Once installation is complete, it is suggested that the installer enter
BULLETIN and read HELP FOLDERS to see the options available when creating
or modifying folders.
 
One of the main uses of BULLETIN, besides storage of messages that are manually
entered by users, is storage of messages from network mailing lists.  This is
done by using the BBOARD feature, which is enabled using the SET BBOARD command
inside BULLETIN.  The alternative method is for mail messages to be written
directly by a mailing program by calling internal BULLETIN routines.  Such a
a program has been written for the popular mail utility PMDF.  If you wish to
do so for another utility, read the text file WRITEMSG.TXT.  I would be glad to
include any such programs with my distribution if you think such a program
would be of use to other users.
 
Responding to mail which is either added via the BBOARD feature is done using
VMS MAIL.  If for some reason this is inappropriate, you can define BULL_MAILER
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
 
4) BULLSTART.COM
   This procedure contains the commands that should be executed after
   a system startup.  It should be executed by SYS$MANAGER:SYSTARTUP.COM.
   It installs the BULLETIN utility with correct privileges.  It also
   includes the command BULLETIN/STARTUP.  This starts up a detached process
   with the name BULLCP.  It periodically check for expire messages, cleanup
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
 
7) INSTALL_REMOTE.COM
   This procedure, in conjunction with REMOTE.COM and DCLREMOTE.COM allows
   a user to install new versions of BULLETIN on several DECNET nodes from
   a single node, rather than having to login to each node.  This is
   especially useful when a new version modifies the format of one of the
   data file.  Older versions of BULLETIN will not run with newer formats
   and will either issue error statements when run, or may cause major
   problems by attempting to change the files back to the old format.
   (NOTE: Don't attempt to use this if different nodes are running
   different versions of VMS, i.e. V4 and V5, as they require different
   linked executables.)
 
8) MASTER.COM
   If you are using PMDF, and want to use the BBOARD option, a set ofh
   routines are included which will allow PMDF to write message directly
   into folders, which is a much more effecient way of doing it than
   the normal BBOARD method of using VMS MAIL.  Read PMDF.TXT for howe
   to do this.
 n
9) OPTIMIZE_RMS.COM 
   This routine optimizes index files.  To run, type @OPTIMIZE_RMS.COM
   followed by the filename.  If you omit the filename, it will prompt
   you to allow you to turn off or on several different types of RMS
   compression.  The default is to turn on all types of compression.
   The optimization will cause the file to be compressed.f
  
   If you use the NEWS feature, it is suggest that you run this procedures
   on BULLNEWS.DAT after it is created.  Compression that file greatly speedsd
   up the NEWS update process.  If you are tight on space, and have been
   running BULLETIN for a long time, it might also be useful to compress
   BULLINF.DAT if that file is very large.  However, compressing that (orr
   the other BULLETIN data files) don't appear to save any execution time,
   unlike BULLNEWS.DAT.i
 o
10) BULLETIN.COM
   If one wants BULLETIN to be able to send messages to other DECNET
   node's GENERAL folder, but wants to avoid running the process created
   by BULLETIN/STARTUP on this node, another method exists.  This is the
   "older" (and slower) method.  BULLETIN.COM must be put in each node's
   DECNET default user's directory (usually [DECNET]).  Once this is done,
   the /NODE qualifier for the ADD & DELETE commands can be used.l
   The object BULLETIN pointing to BULLETIN.COM must be added to the NCP
   database, i.e. the command
	MCR NCP SET OBJ BULLETIN FILE directory:BULLETIN.COM number 0
   must be executed at startup time on the remote node.L
   NOTE:  Privileged functions such as /SYSTEM will work on other nodess
   if you have an account on the other node with appropriate privileges.
$eod l
$copy/log sys$input BULLDIR.INC
$deck 
	PARAMETER DIR_RECORD_LENGTH = ((97+3)/4)*4 
 d
	COMMON /BULL_DIR/ MSG_BTIM,MSG_NUM,DESCRIP,FROM,LENGTH,EX_BTIMt
     &	,SYSTEM,BLOCK,HEADER_BTIM,HEADER_NUM,NEWEST_EXBTIM,NEWEST_MSGBTIM
     &	,NBULL,NBLOCK,SHUTDOWN,SHUTDOWN_BTIM,NEMPTY
     &	,DATE,TIME,EXDATE,EXTIME,NEWEST_EXDATE,NEWEST_EXTIMEc
     &  ,NEWEST_DATE,NEWEST_TIME,SHUTDOWN_DATE,SHUTDOWN_TIME
	CHARACTER*53 DESCRIP
	CHARACTER*12 FROM
	LOGICAL SYSTEM.
 .
	CHARACTER*11 DATE,EXDATE,NEWEST_EXDATE,NEWEST_DATE,SHUTDOWN_DATEM
	CHARACTER*11 TIME,EXTIME,NEWEST_EXTIME,NEWEST_TIME,SHUTDOWN_TIMET
 a
	INTEGER MSG_BTIM(2),EX_BTIM(2),HEADER_BTIM(2)
	INTEGER NEWEST_EXBTIM(2),NEWEST_MSGBTIM(2),SHUTDOWN_BTIM(2)
 I
	CHARACTER*(DIR_RECORD_LENGTH) BULLDIR_ENTRY
	EQUIVALENCE (MSG_BTIM,BULLDIR_ENTRY) 
 h
	CHARACTER*52 BULLDIR_HEADER
	EQUIVALENCE (HEADER_BTIM,BULLDIR_HEADER)A
 ,
	DATA HEADER_BTIM/0,0/,HEADER_NUM/0/
 r
	CHARACTER MSG_KEY*8
 o
	EQUIVALENCE (MSG_BTIM,MSG_KEY)o
 r
	PARAMETER LINE_LENGTH=255
 e
	COMMON /INPUT_BUFFER/ INPUT
	CHARACTER INPUT*(LINE_LENGTH)
$eod d
$copy/log sys$input BULLETIN.HLP
$deckf
1 BULLETIN
Invokes  the  PFC  BULLETIN  Utility.  This utility is used for reading,
adding and deleting message.  Users are notified at login time that  new
messages have been added and the topics of those messages are displayed.
Reading of those messages is optional.  (Use  the  command  SET  READNEW
while  in BULLETIN for setting automatic reading.)  Privileged users can
add system bulletins that are displayed in full at  login  time.   These
messages  are  also  saved,  and  can be read by BULLETIN.  Messages are
automatically deleted after a specified expiration  date,  or  they  can
manually  be  deleted  by  either  the  submitter  of  the  message or a
privileged user.
 o
 Format:
  
      BULLETIN [foldername or bulletin interactive command]B
 E
BULLETIN has an interactive help  available  while  using  the  utility.
Type HELP after invoking the BULLETIN command.
2 Descriptionm
  
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
allowed to read them but write access is limited.m
  
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
terminals).f
 e
A user can select, on a folder per  folder  basis,  to  have  a  message
broadcast  to  their  terminal  immediately  notifying  them  when a new
message has been added. 
 s
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
 g
Messages can be either sent to a file, to a print queue,  or  mailed  to
another user.e
 o
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
DATA;DIR".
 
NOTE:  Depending on how the BULLETIN command is defined,  triple  quotes
rather than single quotes may be required.
2 /EDITi
Specifies that all ADD or REPLACE commands within BULLETIN will select
the editor for inputting text.
2 /KEYPADa
 /[NO]KEYPAD
Specifies that keypad mode is to be set on, such that the keypad keys 
correspond to BULLETIN commands.  The default is /KEYPAD.t
2 /PAGEi
 /[NO]PAGE
 .
Specifies  whether BULLETIN will stop outputting when it displays a full
screen or not.  /PAGE is the default.   If  /NOPAGE  is  specified,  any
output  will  continue  until it finishes.  This is useful if you have a
terminal which can store several screenfuls of display in its memory.o
2 /PGFLQUOTA
   /PGFLQUOTA=pagesE
 f
Used if you want to specify the page file quota for the BULLCP process.t
2 /STARTUP
Starts up a detached process which will periodically check for expired
messages, cleanup empty space in files, and convert BBOARD mail to
messages.  This is recommended to avoid delays when invoking BULLETIN.
It will create a process with the name BULLCP.  For clusters, this
need be done only on one node.  On all other nodes, the system logical
name BULL_BULLCP should be defined (to anything) in order that BULLETINa
is aware that it is running on another node. (On the local node wheret
BULLCP is running, this logical name is automatically defined.) 
2 /STOP,
Stops the BULLCP process without restarting a new one.  (See /STARTUPB
for information on the BULLCP process.) 
2 /SYSTEMa
   /SYSTEM=[days]y
 s
Displays system messages that have been recently added.  The default is&
to show the messages that were added during the last 7 days.  This can
be modified by specifying the number of days as the parameter.
This command is useful for easily redisplaying system messages thatm
might have been missed upon logging in (or were broadcasted but were
erased from the screen.)
2 /WIDTH
   /WIDTH=page_width
 i
Specifies the terminal width for display purposes.  This is used if your
startup procedure is configured such that BULLETIN/LOGIN is executed before(
the terminal type is known, and the default width is larger than what theX
terminal type actually is.  I.e. the default width might be 132, but the
real width is 80.  In that case, you should add /WIDTH=80 to BULLETIN/LOGIN.
2 /WSEXTENT,
   /WSEXTENT=pages
 T
Used if you want to specify the working set limit for the BULLCP process.T
$eod E
$copy/log sys$input BULLETIN.LNK
$deckE
$ LINK/NOTRACE BULL/LIB/INC=BULLETIN$MAIN,SYS$SYSTEM:SYS.STB/SEL/NOUSERLIB-C
	/EXE=BULLETIN,SYS$INPUT/OPT
SYS$SHARE:VAXCRTL/SHAREO
ID="V2.04"
$eod G
$copy/log sys$input BULLFILES.INCI
$deckI
CG
C  FOLDER_DIRECTORY IS THE DIRECTORY THAT FILES FOR FOLDERS THAT
C  ARE CREATED ARE KEPT IN.  IF YOU WISH TO PREVENT FOLDER CREATION,
C  YOU SHOULD MODIFY BULLCOM.CLD TO MAKE THE CREATE COMMAND A PRIVILEGED
C  COMMAND (OR SIMPLY REMOVE THE LINES WHICH DEFINE THE CREATE COMMAND).
CY
C  BBOARD_DIRECTORY IS THE SCRATCH AREA USED BY BBOARD WHEN EXTRACTING
C  MAIL.  IF IT IS UNDEFINED, BBOARD WILL NOT BE ABLE TO BE USED.
C  NOTE THAT EITHER THE BBOARD ACCOUNTS MUST HAVE ACCESS TO THIS DIRECTORY,F
C  OR THE BBOARD ACCOUNTS MUST BE GIVEN SYSPRV PRIVILEGES TO BE ABLE
C  TO WRITE INTO THIS DIRECTORY.  ALSO, FOR BBOARD TO WORK, MAKE SUREh
C  THAT THE SUBPROCESS LIMIT FOR USERS IS AT LEAST 2.  YOU WILL ALSO HAVEt
C  TO INCREASE THE FOLLOWING SYSTEM PARAMETERS WHICH AFFECT DETACHED PROCESES:
C  PQL_DPGFLQUOTA = 10000, PQL_DWSQUOTA = 500, & PQL_DFILLM = 30.b
C  (NOTE: ACCESS CAN BE GIVEN TO THE DIRECTORY FOR THE BBOARD ACCOUNTS USING
C  ACLS, I.E. " SET ACL/ACL=(ID=bboard,ACCESS=R+W)/OBJ=FILE directory.DIR")e
Cf
	COMMON /FILES/ BULLFOLDER_FILE,FOLDER_DIRECTORY,BBOARD_DIRECTORYe
	COMMON /FILES/ BULLUSER_FILE,BULLINF_FILE
	CHARACTER*80 FOLDER_DIRECTORY /'BULL_DIR:'/
	CHARACTER*80 BBOARD_DIRECTORY /'BULL_DIR:'/
Ci
C  NOTE: THE FOLLOWING DEFINITIONS ASSUME THAT BULL_DIR IS USED.  IF ITn
C  IS NOT, THEN THEY SHOULD ALSO BE CHANGED.
CL
	CHARACTER*80 BULLUSER_FILE /'BULL_DIR:BULLUSER.DAT'/i
	CHARACTER*80 BULLFOLDER_FILE /'BULL_DIR:BULLFOLDER.DAT'/n
	CHARACTER*80 BULLINF_FILE /'BULL_DIR:BULLINF.DAT'/ 
	CHARACTER*80 BULLNEWS_FILE /'BULL_DIR:BULLNEWS.DAT'/l
$eod  
$copy/log sys$input BULLFOLDER.INC
$deck 
!v
!  The following 2 parameters can be modified if desired before compilation.
!s
	PARAMETER BBEXPIRE_LIMIT = 30	! Maxmimum time limit in days thato
					! BBOARDS can be set to.b
	PARAMETER BBOARD_UPDATE = 15	! Number of minutes between checks
					! for new BBOARD mail. (Note: Check
					! only occurs via BULLETIN/LOGIN.
					! Check is forced via BULLETIN/BBOARD).
					! NOT APPLICABLE IF BULLCP IS RUNNING.r
	PARAMETER ADDID = .TRUE.	! Allows users who are not in the 
					! rights data base to be added 
					! according to uic number.n
 v
	PARAMETER FOLDER_FMT = '(A25,A4,A12,A80,A12,3A4,A8,7A4)' 
	PARAMETER FOLDER_RECORD = 184	! Must be multiple of 4
 
	COMMON /BULL_FOLDER/ FOLDER,FOLDER_NUMBER,FOLDER_OWNER,
     &		FOLDER_DESCRIP,FOLDER_BBOARD,FOLDER_BBEXPIRE,t
     &		USERB,GROUPB,ACCOUNTB,
     &		F_NBULL,F_NEWEST_BTIM,FOLDER_FLAG,F_EXPIRE_LIMIT,g
     &		F_NEWEST_NOSYS_BTIM,FILLER,y
     &		FOLDER_FILE,FOLDER_SET,FOLDER_NAME
	INTEGER F_NEWEST_BTIM(2)o
	INTEGER F_NEWEST_NOSYS_BTIM(2) 
	LOGICAL FOLDER_SETo
	DATA FOLDER_SET /.FALSE./, FOLDER/'GENERAL'/s
	CHARACTER FOLDER_OWNER*12,FOLDER*25,ACCOUNTB*8,FOLDER_NAME*80
	CHARACTER FOLDER_FILE*80,FOLDER_DESCRIP*80,FOLDER_BBOARD*12
 u
	EQUIVALENCE (FOLDER_BBOARD(3:),F_START)
	EQUIVALENCE (FOLDER_BBOARD(7:),F_END)
 o
	CHARACTER*(FOLDER_RECORD) FOLDER_COM 
	EQUIVALENCE (FOLDER,FOLDER_COM)
 n
	COMMON /BULL_FOLDER1/ FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,
     &		FOLDER1_DESCRIP,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,
     &		USERB1,GROUPB1,ACCOUNTB1,c
     &		F1_NBULL,F1_NEWEST_BTIM,FOLDER1_FLAG,F1_EXPIRE_LIMIT,d
     &		F1_NEWEST_NOSYS_BTIM,FILLER1, 
     &		FOLDER1_FILE,FOLDER1_NAMEn
	CHARACTER FOLDER1_OWNER*12,FOLDER1*25,ACCOUNTB1*8,FOLDER1_NAME*80
	CHARACTER FOLDER1_FILE*80,FOLDER1_DESCRIP*80,FOLDER1_BBOARD*12
	INTEGER F1_NEWEST_BTIM(2)
	INTEGER F1_NEWEST_NOSYS_BTIM(2)
 m
	EQUIVALENCE (FOLDER1_BBOARD(3:),F1_START)
	EQUIVALENCE (FOLDER1_BBOARD(7:),F1_END)
  
	CHARACTER*(FOLDER_RECORD) FOLDER1_COM
	EQUIVALENCE (FOLDER1,FOLDER1_COM)
 b
	PARAMETER NEWS_FOLDER_FMT = '(A25,A4,A55,A12,3A4)' 
	PARAMETER NEWS_FOLDER_RECORD = 108	! Must be multiple of 4 
  
	COMMON /NEWS_FOLDER/ NEWS_FOLDER,NEWS_FOLDER_NUMBER,n
     &		NEWS_FOLDER_DESCRIP,NEWS_FOLDER_BBOARD,g
     &		NEWS_F_NBULL,NEWS_F_NEWEST_BTIMs
	INTEGER NEWS_F_NEWEST_BTIM(2)
	CHARACTER NEWS_FOLDER*25 
	CHARACTER NEWS_FOLDER_DESCRIP*55,NEWS_FOLDER_BBOARD*12
  
	EQUIVALENCE (NEWS_FOLDER_BBOARD(3:),NEWS_F_START)
	EQUIVALENCE (NEWS_FOLDER_BBOARD(7:),NEWS_F_END)
 o
	CHARACTER*(NEWS_FOLDER_RECORD) NEWS_FOLDER_COMA
	EQUIVALENCE (NEWS_FOLDER,NEWS_FOLDER_COM)
  
	COMMON /NEWS_FOLDER1/ NEWS_FOLDER1,NEWS_FOLDER1_NUMBER,
     &		NEWS_FOLDER1_DESCRIP,NEWS_FOLDER1_BBOARD, 
     &		NEWS_F1_NBULL,NEWS_F1_NEWEST_BTIMe
	INTEGER NEWS_F1_NEWEST_BTIM(2)h
	CHARACTER NEWS_FOLDER1*25
	CHARACTER NEWS_FOLDER1_DESCRIP*55,NEWS_FOLDER1_BBOARD*12e
 h
	EQUIVALENCE (NEWS_FOLDER1_BBOARD(3:),NEWS_F1_START)
	EQUIVALENCE (NEWS_FOLDER1_BBOARD(7:),NEWS_F1_END)
 o
	CHARACTER*(NEWS_FOLDER_RECORD) NEWS_FOLDER1_COM
	EQUIVALENCE (NEWS_FOLDER1,NEWS_FOLDER1_COM)
$eod t
$copy/log sys$input BULLNEWS.INC
$deckf
	COMMON /NEWS_DEFAULTS/ ORGANIZATION,MAILERr
 o
	CHARACTER*132 ORGANIZATIONn
	DATA ORGANIZATION /'MIT PLASMA FUSION CENTER'/d
  
	CHARACTER*10 MAILER
	DATA MAILER /'IN%'/
$eod r
$copy/log sys$input BULLUSER.INC
$deckq
!e
! The parameter FOLDER_MAX should be changed to increase the maximum numberN
! of folders available.  Due to storage via longwords, the maximum numbere
! available is always a multiple of 32.  Thus, it will probably make sense
! to specify a multiple of 32 for FOLDER_MAX, as that it what really will be
! the capacity.  Note that the default general folder counts as a folder also,
! so that if you specify 64, you will be able to create 63 folders on your own.p
! 
	PARAMETER FOLDER_MAX = 96
	PARAMETER FLONG = (FOLDER_MAX + 31)/ 32
 h
	PARAMETER USER_RECORD_LENGTH = 28 + FLONG*16w
	PARAMETER USER_FMT = '(A12,<4+FLONG*4>A4)' 
	PARAMETER USER_HEADER_KEY = '            ' 
 e
	COMMON /HEADER_INFO/ TEMP_USER,BBOARD_BTIM,NEWEST_BTIM,USERPRIV
	COMMON /HEADER_INFO/ SET_FLAG_DEF,BRIEF_FLAG_DEF/
	COMMON /HEADER_INFO/ NOTIFY_FLAG_DEFI
	CHARACTER TEMP_USER*12e
	DIMENSION BBOARD_BTIM(2),NEWEST_BTIM(2),USERPRIV(FLONG)
	DIMENSION SET_FLAG_DEF(FLONG),BRIEF_FLAG_DEF(FLONG)
	DIMENSION NOTIFY_FLAG_DEF(FLONG) 
 f
	COMMON /BULL_USER/ USERNAME,LOGIN_BTIM,READ_BTIM,
     &		NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
	CHARACTER*12 USERNAME
	DIMENSION LOGIN_BTIM(2),READ_BTIM(2)e
	DIMENSION NEW_FLAG(FLONG)   ! Used to indicate new message in folderp
				    ! Now NEW_FLAG(2) contains SET GENERIC dayss
	DIMENSION SET_FLAG(FLONG)   ! Bit set indicates READNEW set for folder 
	DIMENSION BRIEF_FLAG(FLONG) ! Bit set indicates READNEW/BRIEF set
	DIMENSION NOTIFY_FLAG(FLONG)! Bit set indicates to broadcaste
				    ! notification when new bulletin is added.
 y
	CHARACTER*(USER_RECORD_LENGTH) USER_ENTRY,USER_HEADER
	EQUIVALENCE (USER_ENTRY,USERNAME)
	EQUIVALENCE (USER_HEADER,TEMP_USER)
 (
	COMMON /FOLDER_TIMES/ LAST_READ_BTIM(2,0:FOLDER_MAX)a
	   ! Must start with 0 to store info for folder specified with ::
	COMMON /SYS_FOLDER_TIMES/ LAST_SYS_BTIM(2,FOLDER_MAX)
	   ! Last read times for each folder as stored in BULL_DIR:BULLINF.DATi
	COMMON /NEWS_TIMES/ LAST_NEWS_READ(2,FOLDER_MAX)d
	INTEGER*2 LAST_NEWS_READ2(4,FOLDER_MAX)
	EQUIVALENCE (LAST_NEWS_READ2(1,1),LAST_NEWS_READ(1,1))f
	   ! Last read times for each folder as stored in BULL_DIR:BULLINF.DATs
 l
	COMMON /NEW_MESSAGES/ NEW_MSG
	DIMENSION NEW_MSG(FLONG)   ! Flag showing new messages detected
$eod e
$copy/log sys$input BULL_NEWS.Cn
$deckW
#include <stdio.h>
#include <descrip.h>
#include <iodef.h>
 d
#if MULTINET
  
#include "multinet_root:[multinet.include.sys]types.h"
#include "multinet_root:[multinet.include.sys]socket.h"t
#include "multinet_root:[multinet.include.netinet]in.h"h
#include "multinet_root:[multinet.include]netdb.h"
#include "multinet_root:[multinet.include]errno.h"
#include "multinet_root:[multinet.include.vms]inetiodef.h"
 ,
static char inet[7] = "INET0:"; 
$DESCRIPTOR(inet_d,inet);k
  
#elsei
 o
#if UCXL
 p
#include <ucx$inetdef.h>
 y
struct sockaddr {K
  short inet_family;
  short inet_port;
  int inet_adrs;
  char bklb[8];U
  };
 
struct itlist { int lgth; struct sockaddr *hst; };
 
static short sck_parm[2];o
static struct sockaddr local_host, remote_host;_
struct itlist lhst_adrs, rhst_adrs; 
  
static char ucxdev[11] = "UCX$DEVICE";
$DESCRIPTOR(ucxdev_d,ucxdev);R
 E
static int addr_buff;O
 Y
#define htons(x) ((unsigned short)((x<<8)|(x>>8)))
  
#elseD
 R
#define CMU 1T
static char ip[4] = "IP:";
$DESCRIPTOR(ip_d,ip);B
 R
#endif
  
#endif
 T
static char task[20];W
$DESCRIPTOR(task_d,task);F
  
static int s;B
 D
static struct iosb {
	short status;
	short size;
	int info;
} iosb;U
 H
#define TCP 0H
#define DECNET 1
  
static int mode = TCP;
  
news_get_chan()I
{return(s);}
 
news_set_chan(i)
int *i;T
{s = *i;} 
  
news_disconnect()E
{R
#if UCXA
	sys$cancel(s);L
	sys$qiow(0,s,IO$_DEACCESS,0,0,0,0,0,0,0,0,0);
#endif
	sys$dassgn(s);L
}I
 S
#if MULTINET
	W
static struct hostent *hp;
static struct sockaddr_in sin;
 D
#endif
 0
int *node;
  
news_assign()E
{C
	int n;B
#if MULTINET
	struct hostent *GETHOSTBYNAME1();
#endif
	node = getenv("BULL_NEWS_SERVER");A
	if (!node) return(0);
	if (!strchr(node,'.')) {F
	   strcpy(&task[0],node);
	   n = strlen(node);T
	   strcpy(&task[n],"::\"TASK=NNTP\"");N
	   task_d.dsc$w_length = 13 + n;O
	   if (!(sys$assign(&task_d,&s,0,0) & 1)) return(0);_
	   mode = DECNET;
	   return(1);
	}
#if MULTINET
	/*B
	 *  Get the IP address of the NEWS host.H
	 */
  
	hp = GETHOSTBYNAME1(node);T
	if (hp == NULL) return(0);R
 L
	/*.
	 *  Create an IP-family socket on which to make the connectionn
	 */
 T
	if (!(sys$assign(&inet_d,&s,0,0) & 1)) return(0);
#else 
#if UCXF
        if (!(sys$assign(&ucxdev_d,&s,0,0) & 1)) return(0);U
	{
           short retlen;
	   struct dsc$descriptor host_namef
		= {strlen(node),DSC$K_CLASS_S,DSC$K_DTYPE_T,node};
	   int comm = INETACP$C_TRANS * 256 + INETACP_FUNC$C_GETHOSTBYNAME;
	   struct dsc$descriptor commandP
		= {4,DSC$K_CLASS_S, DSC$K_DTYPE_T,&comm};
	   struct dsc$descriptor host_adC
		= {4,DSC$K_CLASS_S, DSC$K_DTYPE_T,&addr_buff};
	   struct iosb nam_iosb;E
 /
           if (!(sys$qiow(0,s,IO$_ACPCONTROL,&nam_iosb,0,0,M
                       &command,&host_name,&retlen,&host_ad,0,0) & 1)a
               || !(nam_iosb.status & 1)) { 
              sys$dassgn(s);
	      return(0);2
	   }2
	}
#else)
	if (!(sys$assign(&ip_d,&s,0,0) & 1)) return(0);
#endif
#endif
	return(1);O
}R
 O
news_socket()B
{F
	if (mode == DECNET) return (1);
 F
#if MULTINET
	if (!(sys$qiow(0,s,IO$_SOCKET,&iosb,0,0,hp->h_addrtype,
	    SOCK_STREAM,0,0,0,0) & 1) || !(iosb.status & 1)) {&
	   sys$dassgn(s);
	   return(0);
	}
#endif
#if UCXR
	sck_parm[0] = INET$C_TCP;
	sck_parm[1] = INET_PROTYP$C_STREAM;
	local_host.inet_family = INET$C_AF_INET;O
	local_host.inet_port = 0;
	local_host.inet_adrs = INET$C_INADDR_ANY;
	lhst_adrs.lgth = sizeof local_host;
	lhst_adrs.hst = &local_host;R
	if (!(sys$qiow(0,s,IO$_SETMODE,&iosb,0,0,&sck_parm,0,
	   &lhst_adrs,0,0,0) & 1) || !(iosb.status & 1)) {
           sys$qiow(0,s,IO$_DEACCESS|IO$M_SHUTDOWN,&iosb,0,0,0,0,0,E
						UCX$C_DSC_ALL,0,0);O
	   sys$dassgn(s);
	   return(0);
	}
#endif
 &
	return(1);C
},
 D
news_socket_bullcp(efn,biosb,astadr,astprm)G
int *biosb,*astadr,*astprm,*efn;
{F
	if (mode == DECNET) return (1);
 L
#if MULTINET
	if (!(sys$qio(*efn,s,IO$_SOCKET,biosb,astadr,*astprm,hp->h_addrtype,C
	    SOCK_STREAM,0,0,0,0) & 1) ) return(0);1
#elseE
#if UCX0
	sck_parm[0] = INET$C_TCP;
	sck_parm[1] = INET_PROTYP$C_STREAM;
	local_host.inet_family = INET$C_AF_INET;E
	local_host.inet_port = 0;
	local_host.inet_adrs = INET$C_INADDR_ANY;
	lhst_adrs.lgth = sizeof local_host;
	lhst_adrs.hst = &local_host;F
	if (!(sys$qio(0,s,IO$_SETMODE,biosb,astadr,*astprm,&sck_parm,0,
	   &lhst_adrs,0,0,0) & 1) ) return(0);P
#elseR
	return(-1);
#endif
#endif
 b
	return(1); 
}
 
news_create()O
{R
	if (mode == DECNET) return (1);
  
#if MULTINET
 _
	/*I
	 *  Create a "sockaddr_in" structure which describes the remote
	 *  IP address we want to send to (from gethostbyname()) andA
	 *  the remote NNTP port number (from getservbyname()).
	 */
 N
	sin.sin_family = hp->h_addrtype;T
	BCOPY(hp->h_addr, &sin.sin_addr, hp->h_length);
	sin.sin_port = HTONS(119);L
 _
	/*D
	 *  Do a psuedo-connect to that address. This tells the kernel that
	 *  anything written on this socket gets sent to this destination. It
	 *  also binds us to a local port number (random, but that is ok).E
	 */
 
	if (!(sys$qiow(0,s,IO$_CONNECT,&iosb,0,0,&sin,sizeof(sin),0,0,0,0) & 1)
	    || !(iosb.status & 1)) {D
	   sys$dassgn(s);
	   return(0);
	}
#elseB
#if UCX 
        remote_host.inet_family = INET$C_AF_INET;:
        remote_host.inet_port = htons(119);E
	remote_host.inet_adrs = addr_buff;(
	rhst_adrs.lgth = sizeof remote_host;
	rhst_adrs.hst = &remote_host;
	if (!(sys$qiow(0,s,IO$_ACCESS,&iosb,0,0,0,0,&rhst_adrs,0,0,0) & 1)R
	    || !(iosb.status & 1)) {I
           sys$qiow(0,s,IO$_DEACCESS|IO$M_SHUTDOWN,&iosb,0,0,0,0,0, 
						UCX$C_DSC_ALL,0,0);p
	   sys$dassgn(s);
	   return(0);
	}
#else 
	if (!(sys$qiow(0,s,IO$_CREATE,&iosb,0,0,node,119,0,1,0,300) & 1)e
	    || !(iosb.status & 1)) {e
	   sys$dassgn(s);
	   return(0);
	}
#endif
#endif
  
	return(1);u
}p
 o
news_create_bullcp(efn,biosb,astadr,astprm)o
int *biosb,*astadr,*astprm,*efn;
{M
	if (mode == DECNET) return (1);
  
#if MULTINET
 t
	/*t
	 *  Create a "sockaddr_in" structure which describes the remote
	 *  IP address we want to send to (from gethostbyname()) ando
	 *  the remote NNTP port number (from getservbyname()).
	 */
 X
	sin.sin_family = hp->h_addrtype;O
	BCOPY(hp->h_addr, &sin.sin_addr, hp->h_length);
	sin.sin_port = HTONS(119);M
 R
	/*_
	 *  Do a psuedo-connect to that address. This tells the kernel that
	 *  anything written on this socket gets sent to this destination. It
	 *  also binds us to a local port number (random, but that is ok).R
	 */
 M
	if (!(sys$qio(*efn,s,IO$_CONNECT,biosb,astadr
		,*astprm,&sin,sizeof(sin),0,0,0,0) & 1)) return(0);N
#elseM
#if UCXT
        remote_host.inet_family = INET$C_AF_INET;A
        remote_host.inet_port = htons(119);T
	remote_host.inet_adrs = addr_buff;T
	rhst_adrs.lgth = sizeof remote_host;)
	rhst_adrs.hst = &remote_host;
	if (!(sys$qio(*efn,s,IO$_ACCESS,biosb,astadr,*astprm,0,
		0,&rhst_adrs,0,0,0) & 1)) return(0);
#elsey
	if (!(sys$qio(*efn,s,IO$_CREATE,biosb,astadr,*astprm,node,s
		119,0,1,0,300) & 1))
	   return(0);
#endif
#endif
 c
	return(1);B
}F
 t
news_connect()
{_
	if (!news_assign()) return(0); 
	if (!news_socket()) return(0);n
	return(news_create());e
}
 
news_write_packet(buf)
 G
struct dsc$descriptor_s *buf;U
{L
	static int n,len;
 M
	len = buf->dsc$w_length;E
#if CMUE
	if (!(sys$qiow(0,s,IO$_WRITEVBLK,&iosb,0,0,buf->dsc$a_pointer, 
					len,0,!mode,0,0) & 1)
	    || !(iosb.status & 1)) return(0);
#elseS
	if (!(sys$qiow(0,s,IO$_WRITEVBLK,&iosb,0,0,buf->dsc$a_pointer,e
					len,0,0,0,0) & 1)
	    || !(iosb.status & 1)) return(0);
#endif
 L
	return(1);(
}O
 R
news_write_packet_bullcp(efn,biosb,astadr,astprm,buf,len)N
int *biosb,*astadr,*astprm,*efn,*buf,*len;
{
#if CMUt
	if (!(sys$qio(*efn,s,IO$_WRITEVBLK,biosb,astadr,*astprm,buf,
					*len,0,!mode,0,0) & 1)) return(0);N
#elseW
	if (!(sys$qio(*efn,s,IO$_WRITEVBLK,biosb,astadr,*astprm,buf,p
					*len,0,0,0,0) & 1)) return(0);
#endif
 t
	return(1);u
}<
 c
news_read_packet(buf).
struct dsc$descriptor_s *buf;u
{"
	static int n,len;
 .
	len = buf->dsc$w_length;u
	if (!(sys$qiow(0,s,IO$_READVBLK,&iosb,0,0,buf->dsc$a_pointer,
					len,0,0,0,0) & 1)
	    || !(iosb.status & 1)) return(0);
	n = iosb.size;n
 d
	return(n);i
}u
 "
news_gethostname(buf).
 l
struct dsc$descriptor_s *buf;t
{o
	if (mode == DECNET) return (-1);"
#if MULTINET
	return(GETHOSTNAME(buf->dsc$a_pointer, buf->dsc$w_length));
#else
	return(-1);
#endif
}c
$eod e
$copy/log sys$input HANDOUT.TXTh
$deckt
               Introduction to BULLETIN on the Vax
                                                  2/88 AWc
 d
PUBLISHED BY THE DREW UNIVERSITY ACADEMIC COMPUTER CENTER. MAY BEl
COPIED WITH WRITING CREDIT GIVEN TO DREW UNIVERSITY.
 d
BULLETIN was written for the Public Domain by Mark London at MIT.v
 u
     The BULLETIN utility permits a user to create messages forg
reading by other users.  Users may be notified upon logging on
that new messages have been added, and what the topic of the
messages are.  Actual reading of the messages is optional.  (See
the command SET READNEW for info on automatic reading.)  Messagest
are automatically deleted when their expiration data has passed.
     The program runs like VAX mail.  The different interest
groups or BULLETIN boards are implemented in the form of
'Folders', just like a filing cabinet.  A Folder contain various
messages on the same general topic.  A message is a piece of textU
written by a user or staff person and added to a particularr
folder.  All users are not permitted to submit messages to all
folders.
 U
     A message consists of an expiration date, a subject linee
and the text of the message.  BULLETIN will prompt the user ford
these things when a message is being added. 
 t
     Several different folders are currently defined to 
BULLETIN.  The General Folders will be used by Computer Center
Staff to post messages of general interest concerning the VAX to
the user community.  If something is of an important nature, it 
will be posted in the General folder as a 'System' message.(
This is a special message type.  It will be displayed to eache
user  as they log in the first time after that message was
posted.  This will be done automatically by BULLETIN on login.
Once a particular system message has been displayed, it will not
be displayed for that user on subsequent logins.
 e
FoldersL
 _
     Different folders have been created to contain messages onE
different topics.  Folders may be public, semi-private, or
private.  The majority of the folders will be public.  However a
few will be semi-private, which will mean that all users may
read messages in the folder but not all will be able to post to(
it.  Currently, there are several folders defined:
  
GENERAL -- system messages
 &
PUBLIC_ANNOUNCEMENTS -- Can be used by anyone to post messages
of interest to the publics
 
On Beta:
AIDE STATION -- Private folder for Computer Center Employees
 )
In addition on Alpha there are folders that receive electronic
magazines, such as:D
NETMONTH --  The monthly magazine of BITNET information.
RISKS -- Identifying the risks involved in using computers.0
INFOIBMPC -- Information about the IBM personal computers.
INFOVAX -- Information on the Digital VAX.
PROGRAMMING_JOURNALS-Includes MINIX, UNIX and C, Modula-2 ands
Prolog journalsN
watch for new ones being added.p
  
Using BULLETIN
 n
     BULLETIN is invoked by type the command 'BULLETIN' (or BULL,
for short) at the '$' prompt.  BULLETIN will display its prompti
'BULLETIN>'. Help is available from DCL command level ($) or from 
within the BULLETIN program itself by typing the word 'HELP'.  To0
leave the BULLETIN program, type 'EXIT'.
 $
To see what is there
 ;
     In order to see message and folders, on can use the
'Directory' command. Upon entering BULLETIN, the user is place
in the General folder.  If the user wishes to see which foldersq
exist, the directory/folders command is used. for example:
typing:_
 E
     BULLETIN> directory/folders
 E
will make a display like:I
 $
      Folder                       Owner
     *GENERAL                      SYSTEMT
     *PUBLIC_ANNOUNCEMENTS         BBEYER.
      NETMONTH                     BITNETl
     *VAX_SIG                      BBEYER&
 a
An asterisk (*) next to the folder name indicates you have unreadp
messages in that folder.
  
The command 'DIRECTORY/FOLDERS/DESCRIBE' would list all availabler
folders, along with a brief description of each.
 E
     To switch from one folder to another folder, the user may
execute the 'SELECT' command.  For example, the followings
command would show what a user would do to switch to the folder 
called PUBLIC_ANNOUNCEMENTS:
 a
BULLETIN> SELECT PUBLIC_ANNOUNCEMENTS>
 d
and BULLETIN would respond:&
     Folder has been set to PUBLIC_ANNOUNCEMENTS
 1
     Now the user may get a list of the messages in this folderl
by issuing the directory command with no qualifiers.
This command, for example:
BULLETIN> DIRECTORYi
would have bulletin respond:
 n
 #     Description               From                  DateN
 1     CHRISTMAS PARTY           oleksiak              26-JUN-88
 2     Learning about BULLETIN   oleksiak              26-JUN-87
 3     VAX MAIL                  LLLOYD                01-Jan-87
 e
     The command 'DIR/NEW' will list just unread messages.
 u
 (
Reading messages
 i
     In order to read messages in a folder, the user may typeq
the read command or he/she may simply type the number of the
message he wishes to read.  The message numbers can be acquiredI
by doing the 'DIRECTORY' command.  If the user hits a carriage
return with no input whatsoever,  BULLETIN will type the first
message in the folder, or if there are new messages present, ita
will type the first new message in the folder.
 }
     If a folder contains the above messages (as seen by the
'Directory' command) then these messages can be read by:
 
BULLETIN> READ
and BULLETIN would respond: 
 T
Message number:  1                       PUBLIC_ANNOUNCEMENTSc
Description: CHRISTMAS PARTY
Date:  26-JUN-1988 8:08:40   Expires:  1-JAN-1989 08:08:40
 m
...Body of message.....m
 t
     Should the user only wish to see message number 3, he can
enter the 'READ' command with the message number as a parameter.
for example:
 /
BULLETIN> READ 3
 c
     There are three other useful commands that can be used at
the 'BULLETIN>' prompt when reading messages. These are:
  
BACK - Read the message preceding the message currently being)
read./
 M
CURRENT - Start reading the current message at the top.  This is
useful for someone who is reading a message and wishes to reread
it from the beginning.
 N
NEXT - Start reading from the beginning of the next message.
This is handy if the user is reading a very long message and
wants to skip to the next one.
 e
Saving the interesting stuff.,
 O
     If the user sees something which he/she wants a copy of,e
the extract command can be use to write an ASCII copy of the
message into a file.  This command works on the current message
being read.  It requires the name of the file into which to save
the message.  If the file name is not given, the user will be(
prompted for it.  For example:
 e
BULLETIN>  Read 2t
 t
********** Message on Screen ********n
 ,
A person could then type
BULLETIN> extractE
file:  FV.TXTo
BULLETIN>R
 V
BULLETIN has now saved the contents of message number 2 into the
file name 'FV.txt'. 
     If the file to which the user is writing already exists,,
BULLETIN will append the message to the file.  The user cano
force BULLETIN to write a new file containing only the message
being saved by using the '/new' qualifier in the 'extract'
command.  These messages can then be sent to other users, or
downloaded for use in Wordperfect.  (See "Mail on the Vax", or
"Transferring a file between a PC and the VAX").
 s
This command may be useful if you wish to transfer the message to,
your PC, perhaps using a BITNET journal message as a reference in_
a paper. Once the file is saved, you can transfer it to a PC bye
following the instructions in the handout 'Transferring filesD
from the PC to the VAX of from the VAX to a PC".
  
Adding messageso
     A user may add a message to a folder by selecting the
folder and then using the 'ADD' command.  This is provided thatb
the user is adding the message to a public folder.  The user has
the option of giving the 'ADD' command and typing a message usingt
the VAX editor or uploading a message from your PC (see.
documentation), or add a message you have extracted from VAX
mail.  BULLETIN will prompt for the expiration date and subjectd
line.  It will then add the text of the file as the body of theB
message. To add a message that is stored in a file (from MAIL or
from your PC, for example) type:
 n
          ADD filename
 u
If the user does not specify a file name, he/she will be
prompted to enter the body of the message.  The user may alsoo
use the EDT text editor by issuing the command with the 
'/EDIT'option.
 g
For example:
BULLETIN> sel PUBLIC_ANNOUNCEMENTS
          folder has been set to PUBLIC_ANNOUNCEMENTS 
BULLETIN> ADD MESS.TXT
 u
IT IS 10-JUL-1988 12:41:06.15.  SPECIFY WHEN THE MESSAGE SHOULDT
EXPIRE:  ENTER ABsolute TIME:  <DD-MMM-YYYY]HH:MM:SS OR DELTAo
TIME: DDD HH:MM:SS
 m
A user then type the date of expiration and press the 'return'
button.  The time input may be ignored. For example, typing:
20-JUL-1988 or type "10" - for ten days in the future.
 f
BULLETIN responds:
ENTER DESCRIPTION HEADER.  LIMIT HEADER TO 53 CHARACTERS.m
 m
Now the user may enter the subject of the message.
 f
BULLETIN>i
 d
The above session adds the text in the file 'mess.txt' as thel
next message in the PUBLIC_ANNOUNCEMENTS Folder.  The messaged
will be deleted automatically on the 20th of July as requested
by the user adding the message.s
 l
Asking BULLETIN to notify you of new messages upon logging in.
 e
     If the user wishes to get notification on login when news
messages are in a folder, he should use the 'READNEW' option.d
This command does not force the reader to reading new messages,i
only gives notification.  To do this, 'SELECT' each folder you
are interested in and do a 'SET READNEW' command while set tob
that folder.
 i
Example:
 r
BULLETIN> Select PUBLIC_ANNOUNCEMENTSe
folder has been set to PUBLIC_ANNOUNCEMENTS 
BULLETIN> SET READNEW
 d
Alternately, you may type SET SHOWNEW. This will just display as
message notifying you that there are new messages.
 i
Mailing a BULLETIN message
 o
     A user may directly mail another user a message found in thei
BULLETIN.  While reading the message that he/she desires to send,e
at the 'BULLETIN>' type 'MAIL'.  The Vax will then ask to whom
you wish to send the information too. 
 s
Check the BULLETIN DISCUSSION folder on ALPHA for new additions.
If you have comments or questions about BULLETIN, leave them
there.
$eod r
$copy/log sys$input INSTRUCT.TXT
$decks
This message is being displayed by the BULLETIN facility.  This is a non-DEC
facility, so it is not described in the manuals.  Messages can be submitted by
using the BULLETIN command.  System messages, such as this one, are displayeda
in full, but can only be entered by privileged users.  Non-system messages can
be entered by anyone, but only their topics will be displayed at login time,
and will be prompted to optionally read them.  (This prompting feature can bev
disabled).  All bulletins can be reread at any time unless they are deleted or
expire.  For more information, see the on-line help (via HELP BULLETIN). o
$eod e
$copy/log sys$input NEWS.TXT
$deck 
BULLETIN now has the capability to read and post messages to USENET NEWS in ae
client mode.  I realize that there are many NEWS readers, some with much mores
elegant interfaces.  However, I elected to modify BULLETIN for the following
reason:  We have many decnet nodes, but only several are internet nodes.  Oure
only access to a news server was via internet.  In order for those
non-internet nodes to read USENET, the only method that seemed available was to 
run a NEWS server program on one of our own internet nodes so that it couldU
be accessible via decnet.  I did not want to do that, as that requires storing
the news groups on disk, and I do not have the room for that.  I thus added ther
ability in BULLETIN (actually BULLCP) so that it acts as as a gateway betweenR
decnet and tcp for NEWS.  This method does not require spawning any processes,
since the detached process BULLCP is always present, so the access is very
fast.  Also, since BULLETIN uses a shared database to store info on the NEWS
groups and periodically updates it, there is no need for that to be done when aN
user accesses the NEWS groups.  Several other NEWS readers do this when you runo
them, which is why they take a long time to start up.  It is also possible toa
feed NEWS groups into a "real" BULLETIN folder, so that the messages are saved
on disk.
 i
Presently, BULLETIN can be used with either UCX, MULTINET, or CMU TCP/IP
packages (and of course DECNET) for reading NEWS.  Support for other packages1
can be added if I can find sites willing to beta test the interface for me. 
The source for the TCP interface is in C rather than FORTRAN because the
MULTINET include files are in C. However, if you do not have C, I will be glad
to send the object for it (or to even possibly rewrite the code in FORTRAN).  
 s
The instructions for installation are as follows.  Define BULL_NEWS_SERVER
to be a system logical name pointing to either your internet or decnet NEWSe
node.  If it is decnet, simply specify the decnet node name, i.e.a
 a
	$ DEFINE/SYSTEM BULL_NEWS_SERVER NERUSL
 N
BULLETIN decides to use DECNET rather than TCP access based on the node name.n
If it does not have any periods in it, then it assumes it is a DECNET node.c
 a
In our cluster, we usually have one node which is an internet node, and theg
rest non-internet nodes.  If you have a similar situation, you'll have toe
create a startup procedure that defines BULL_NEWS_SERVER to be the internetR
news server address only on the node (or nodes) on the cluster that have
actually internet access.  The other nodes will have BULL_NEWS_SERVER defined 
as the decnet node name that BULLCP is running on in the cluster.  (Of course,
BULLCP will have to be running on a node with internet access.)o
 r
NOTE: If you want to disable the gateway feature, then before starting BULLCP,
define the logical name:
 a
	$ DEFINE/SYSTEM BULL_NO_NEWS_GATEWAY "FALSE"e
 )
Defining this will only shut off the gateway.  BULLETIN will still be allowed
to read NEWS from the local node as long as BULL_NEWS_SERVER is defined.
  
In order to post messages, BULLETIN needs to know the internet nodename of
the local host.  This is done automatically for nodes running MULTINET.  For
other nodes, BULLETIN attempts to translate the logical name ARPANET_HOST_NAME,o
INTERNET_HOST_NAME, and MX_NODE_NAME.
  
The local time zone is detected by looking at the following logical names:
LISP$TIME_ZONE, MULTINET_TIMEZONE, or PMDF_TIMEZONE.  (LISP$TIME_ZONE is
defined if you have LISP installed.)
 h
The name of the organization is included in the header of the NEWS message..
This can be anything, but usually is the company or university name.  This
can be hardcoded into the source by putting in BULLNEWS.INC, or by definingL
the system logical name BULL_NEWS_ORGANIZATION.f
 s
The name of the mail protocol to use for responding by mail to NEWS messages
can also be either hardcoded by putting in BULLNEWS.INC, or by defining the 
system logical name BULL_NEWS_MAILER. 
 e
After installing the new BULLETIN, execute the command NEWS, which asks for an
list of all the news groups.  Because this is the first time it is executed, itn
will cause a load of all the remote news groups into a local data base
(BULL_DIR:BULLNEWS.DAT). This will take several minutes to do.  It is the only
time that this load will be done interactively.  Afterwards, BULLCP will
periodically update the data base.  For this reason, it is highly recommeded
that BULLCP be installed. BULLCP will update NEWS every hour.  If you want tom
change this frequency, define the logical name BULL_NEWS_UPDATE to the number 
of minutes in between updates, i.e. DEFINE/SYSTEM BULL_NEWS_UPDATE "30" for 30
minutes.  (NOTE: BULLCP will create a subprocess BULLCP NEWS which does thef
update.  You can watch how long it takes for this to run in order to determine
if you want to change the update period). After BULLNEWS.DAT is created, it is
suggested that you run OPTIMIZE_RMS.COM on it, as it will cause the file to be
compressed and will allow updates to run much faster (factor of 5 or more). 
 a
If you have any problems or questions, please let me know.
									MRL
P.s.
	If you do not know what USENET NEWS, it's basically news messages which
are passed between nodes.  Originally it was limited to USENET, but that is no
longer the case.  Unlike internet mailing lists which use MAIL to send the
messages to individuals, NEWS messages are not sent via MAIL.  They are passed
between nodes using a special protocol, NNTP.  Users must use a NEWS reader 
package to read them.  However, it is possible to read NEWS remotely over aO
network, and therefore avoiding having to actually store the messages.
BULLETIN is setup to be used mainly in this client mode, i.e. it can read 
messages on another node via TCP or DECNET.  This is useful, since the number1
of NEWS groups total over 1000, the disk space required for storage is very 
high.  If you are interested in finding a server node that would allow you tor
read NEWS, and do not know of one (i.e. a USENET node), I know of no officialx
way of doing so.  However, one suggestion was to try connecting to BBN.COM via
ANONYMOUS FTP and look through the directory uumap/comp.mail.maps to find at
USENET node near you to contact. s
$eod s
$copy/log sys$input NONSYSTEM.TXTm
$deck 
Non-system bulletins (such as this) can be submitted by any user.  Users are
alerted at login time that new non-system bulletins have been added, but onlym
their topics are listed.  Optionally, users can be prompted at login time to
see if they wish to read the bulletins.  When reading the bulletins in thisE
manner, the bulletins can optionally be written to a file.  If you have thet
subdirectory [.BULL] created, BULLETIN will use that directory as the default 
directory to write the file into.m
 t
A user can disable this prompting featuring by using BULLETIN as follows: 
 e
$ BULLETIN
BULLETIN> SET NOREADNEWN
BULLETIN> EXIT
 A
Afterwords, the user will only be alerted of the bulletins, and will have to
use the BULLETIN utility in order to read the messages.e
$eod T
$copy/log sys$input WRITEMSG.TXT
$deckt
BULLETIN contains subroutines for writing a message directly to a folder.  This 
would be useful for someone who is using the BBOARD feature, but wants to avoidT
the extra overhead of having the message sent to an account as MAIL, and thenm
have BULLCP read the mail.  It is better if the network mail could be writteny
directly to the folder bypassing VMS MAIL, as it reduces a lot of cpu overhead. 
 L
Call INIT_MESSAGE_ADD to initiate a message addition.i
Call WRITE_MESSAGE_LINE to write individual message lines.
Call FINISH_MESSAGE_ADD to complete a message addition.n
 t
Calling formats:
 i
	CALL INIT_MESSAGE_ADD(IN_FOLDER,IN_FROM,IN_DESCRIP,IER)
Ca
C  INPUTS:
C	IN_FOLDER  - Character string containing folder name
C	IN_FROM	   - Character string containing name of owner of message.
C		     If empty, the default is the owner of the process.
C	IN_DESCRIP - Character string containing subject of message.
C		     If empty, the message is searched for a line
C		     which starts with "Subj:" or "Subject:".
C  OUTPUTS:s
C	IER - Error status.  True if properly connected to folder.
C		False if folder not found.l
Ci
 
	CALL WRITE_MESSAGE_LINE(BUFFER)
Cu
C  INPUTS:
C	BUFFER - Character string containing line to be put into message.t
Ce
  
	CALL FINISH_MESSAGE_ADD
Cn
C  NOTE:  Only should be run if INIT_MESSAGE_ADD was successful.
C 
$eod  
