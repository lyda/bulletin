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
