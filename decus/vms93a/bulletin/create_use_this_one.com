$ FQ = ""
$ IF F$GETSYI("HW_MODEL") .GT. 1023 THEN FQ = "/SEPARATE_COMPILATION"
$ IF F$GETSYI("VP_MASK") .NE. 0 THEN FQ = FQ + "/NOHPO"
$ FORTRAN/EXTEND/nocheck'FQ' BULLETIN
$ FORTRAN/EXTEND/nocheck'FQ' BULLETIN0
$ FORTRAN/EXTEND/nocheck'FQ' BULLETIN1
$ FORTRAN/EXTEND/nocheck'FQ' BULLETIN2
$ FORTRAN/EXTEND/nocheck'FQ' BULLETIN3
$ FORTRAN/EXTEND/nocheck'FQ' BULLETIN4
$ FORTRAN/EXTEND/nocheck'FQ' BULLETIN5
$ FORTRAN/EXTEND/nocheck'FQ' BULLETIN6
$ FORTRAN/EXTEND/nocheck'FQ' BULLETIN7
$ FORTRAN/EXTEND/nocheck'FQ' BULLETIN8
$ FORTRAN/EXTEND/nocheck'FQ' BULLETIN9
$ FORTRAN/EXTEND/nocheck'FQ' BULLETIN10
$ FORTRAN/EXTEND/nocheck'FQ' BULLETIN11
$ MACro ALLMACS
$ SET COMMAND/OBJ BULLCOM
$ SET COMMAND/OBJ BULLMAIN
$ CCQ = ""
$ IF F$GETSYI("HW_MODEL") .GT. 1023 THEN CCQ = "/STAN=VAX"
$ ON WARNING THEN GOTO DUMMY
$ IF F$TRNLNM("MULTINET_SOCKET_LIBRARY") .NES. "" THEN GOTO MULTI
$ IF F$TRNLNM("TWG$TCP") .EQS. "" THEN GOTO MULTI
$ DEFINE VAXC$INCLUDE TWG$TCP:[NETDIST.INCLUDE],-
                      TWG$TCP:[NETDIST.INCLUDE.SYS],-
                      TWG$TCP:[NETDIST.INCLUDE.VMS],-
                      TWG$TCP:[NETDIST.INCLUDE.NETINET],-
                      TWG$TCP:[NETDIST.INCLUDE.ARPA],-
                      SYS$LIBRARY
$ CC'CCQ' BULL_NEWS/DEFINE=(TWG=1)
$ GOTO LINK
$MULTI:
$ IF F$TRNLNM("MULTINET_SOCKET_LIBRARY") .EQS. "" THEN GOTO UCX
$ CC'CCQ' BULL_NEWS/DEFINE=(MULTINET=1)
$ GOTO LINK
$UCX:
$ IF F$TRNLNM("UCX$DEVICE") .EQS. "" THEN GOTO CMU
$ CC'CCQ' BULL_NEWS/DEFINE=(UCX=1)
$ GOTO LINK
$CMU:
$ CC'CCQ' BULL_NEWS
$ GOTO LINK
$DUMMY:
$ WRITE SYS$OUTPUT "There is no C compiler available for the NEWS software."
$ WRITE SYS$OUTPUT "BULLETIN will be assembled without that feature."
$ FORTRAN BULL_NEWSDUMMY
$LINK:
$ on error then exit
$ IF F$SEARCH("BULL_DIR:READ_BOARD.COM") .NES. "" THEN-
  DELETE BULL_DIR:READ_BOARD.COM;*
$ IF F$SEARCH("BULL.OLB") .NES. "" THEN DELETE BULL.OLB;*
$ IF F$SEARCH("BULL.OLB") .EQS. "" THEN LIBRARY/CREATE BULL
$ LIBRARY BULL *.OBJ;
$ DELETE *.OBJ;*
$ @BULLETIN.LNK