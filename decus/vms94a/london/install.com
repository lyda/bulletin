$ IF F$TRN("BULL_DIR") .EQS. ""
$ THEN
$   WRITE SYS$OUTPUT "ERROR: BULL_DIR has not been defined yet."
$   WRITE SYS$OUTPUT "Assign BULL_DIR as a logical name or modify this"
$   WRITE SYS$OUTPUT "command procedure to point to the appropriate directory."
$   EXIT
$ ENDIF
$ COPY BULLETIN.EXE BULL_DIR:
$ RUN SYS$SYSTEM:INSTALL
BULL_DIR:BULLETIN/DEL
BULL_DIR:BULLETIN/SHAR/OPEN/HEAD/-
PRIV=(OPER,SYSPRV,CMKRNL,WORLD,DETACH,PRMMBX,SYSNAM)
/EXIT
$!
$! NOTE: BULLETIN requires a separate help library. If you do not wish
$! the library to be placed in SYS$HELP, modify the following lines and
$! define the logical name BULL_HELP to be the help library directory, i.e.
$!	$ DEFINE/SYSTEM BULL_HELP SYSD$:[NEWDIRECTORY]
$! The above line should be placed in BULLSTART.COM to be executed after
$! every system reboot.
$!
$ IF F$SEARCH("SYS$HELP:BULL.HLB") .NES. "" THEN LIBRARY/DELETE=*/HELP SYS$HELP:BULL
$ IF F$SEARCH("SYS$HELP:BULL.HLB") .EQS. "" THEN LIBRARY/CREATE/HELP SYS$HELP:BULL
$ LIBRARY/HELP SYS$HELP:BULL BULLCOMS1,BULLCOMS2
$ LIBRARY/HELP SYS$HELP:HELPLIB BULLETIN
