$ COPY BULLETIN.EXE SYS$SYSTEM:
$ SET FILE SYS$SYSTEM:BULLETIN.EXE/OWN=[1,4]
$ RUN SYS$SYSTEM:INSTALL
SYS$SYSTEM:BULLETIN/SHARE/OPEN/HEAD/PRIV=(OPER,SYSPRV,CMKRNL)
/EXIT
$ LIB/CREATE/HELP SYS$HELP:BULL
$ LIB/HELP SYS$HELP:BULL BULLCOMS
$ LIB/HELP SYS$HELP:HELPLIB BULLETIN
