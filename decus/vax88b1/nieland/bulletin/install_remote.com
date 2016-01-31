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
$! OLD 	- Specifies that the present version of BULLETIN is 1.51 or earlier.
$! COPY - Specifies that the executable is to be copied to the nodes.
$! TEST - Specifies that all the nodes are to be checked to see if they
$!	  are up before beginning the intallation.
$!
$! NOTES:
$! 	***PLEASE READ ALL COMMENTS BEFORE RUNNING THIS***
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
$ NODES = "ALCVAX,NERUS,ANANSI,KLEIN,MOLVAX,LAURIE,CANDLE,KLYPSO,DOME" +-
",ARVON,LARAN,ORYANA,PALDAR,MOTHRA,TARNA,DARIUS"
$ COPY_NODES = "NERUS,KLEIN,MOLVAX,LAURIE,ARVON"
$ BULLCP_NODES = "NERUS,KLEIN,MOLVAX,LAURIE,ARVON"
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
/OPEN/HEAD/PRIV=(OPER,SYSPRV,CMKRNL,WORLD,DETACH,PRMMBX)
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
