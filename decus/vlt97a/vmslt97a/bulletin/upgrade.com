$!
$!  Normally, new versions of BULLETIN don't require any special
$!  installation except to link and install the new executable (and
$!  possibly relink PMDF or MX interfaces if any changes affect them).
$!  However, when there is a change to the data file format, you should run
$!  the following procedure.         
$!
$!  This is a sample upgrade procedure.  You will have to modify references
$!  to the directory where the new executables are stored, which are marked
$!  with ***. You will also have to change the references to the procedures
$!  that link either PMDF or MX.  These procedures usually replace the old
$!  executable, but that should not be done until BULLETIN has been disabled
$!  (by defining the logical name BULL_DISABLE).  If you run this procedure
$!  with a parameter (i.e. @UPGRADE LINK), it will call those linking
$!  procedures. Alternatively, you could define BULL_DISABLE and run those
$!  procedures manually.  Or, you could change them so they don't replace the
$!  old procedures and run them manually, and simply have this procedure
$!  replace them.  Whatever you find more convenient.
$!  
$!  This procedure should be run on all stand alone nodes or boot nodes of
$!  clusters on which bulletin is installed. After running upgrade.com, run
$!  restart.com to restart bulletin.  If you don't make use of remote
$!  folders, you can run restart.com immediately after upgrade.com rather
$!  than waiting to install the new version on all nodes.  Otherwise, you
$!  should try to run this procedure simultaneously on all clusters and then
$!  wait until it finishes on all nodes before running restart.com. 
$!  Otherwise, remote folder access attempts will fail.                 
$!  
$ SET PROCESS/PRIVILEGE=ALL
$ COPY ALCVAX::USER1:[MRL.BULLETIN]BULLETIN.EXE BULL_DIR:              ! *** 
$ COPY ALCVAX::USER1:[MRL.BULLETIN]BULL.HLB SITE$ROOT:[SYSHLP]         ! *** 
$ PMDF = F$TRNLNM("PMDF_ROOT")
$ MX = F$TRNLNM("MX_EXE")
$ BULL/STOP
$ IF PMDF .NES. "" THEN PMDF = F$SEARCH("PMDF_ROOT:[EXE]BULLETIN_MASTER.EXE")
$ IF PMDF .NES. "" THEN DELETE/NOCONFIRM PMDF_ROOT:[EXE]BULLETIN_MASTER.EXE;*
$ IF MX .NES. "" THEN MX = F$SEARCH("MX_EXE:MX_BULL.EXE")
$ IF MX .NES. "" THEN DELETE/NOCONFIRM MX_EXE:MX_BULL.EXE;*
$ MCR SYSMAN
SET ENV/CL
SET PROF/PRIV=ALL
DO DEFINE BULL_DISABLE/SYSTEM "DISABLE"
DO MCR INSTALL BULL_DIR:BULLETIN/REPLACE
$ IF P1 .NES. "" .AND. PMDF .NES. "" THEN @USER1:[MRL.BULLETIN]LINK_PMDF ! ***
$ IF P1 .NES. "" .AND. MX .NES. "" THEN @USER1:[MRL.BULLETIN]LINK_MX	 ! ***
$ DEFINE BULL_DISABLE "ENABLE"		! Enable it just for this process.
$ BULLETIN
$ IF F$TRNLNM("BULL_NEWS_SERVER") .NES. "" THEN BULLETIN MISC.TEST
$ IF PMDF .NES. "" THEN-
   COPY ALCVAX::PMDF_ROOT:[EXE]BULLETIN_MASTER.EXE PMDF_ROOT:[EXE]	! ***
$ IF MX .NES. "" THEN-
   COPY ALCVAX::MX_EXE:MX_BULL.EXE MX_EXE:                 		! ***
$ DEASSIGN BULL_DISABLE
