BULLETIN
Bulletin is a utility which is a message repository, permitting
use like VMS MAIL, but where VMS MAIL is one to one, Bulletin is
one to many. Messages are organized into messages and folders so a
user can search the folders at will. Messages can be set to expire
after some number of days, or can be permanent.

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
