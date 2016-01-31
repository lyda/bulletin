$! DCL procedure to execute DCL commands passed over Decnet on a remote system.
$! Commands sent by the command procedure REMOTE.COM on the local system are
$! are received by this procedure on the remote node.
$! This procedure is usually a DECNET OBJECT with task name DCLREMOTE and
$! normally resides in the default DECNET account.  To install as an object,
$! enter NCP, and then use the command:
$!		NCP> SET OBJECT DCLREMOTE FILE file-spec NUM 0
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
