 $ RUN SYS$SYSTEM:INSTALLN BULL_DIR:BULLETIN/SHAR/OPEN/HEAD/PRIV=(OPER,SYSPRV,CMKRNL,WORLD,DETACH,PRMMBX) /EXIT " $ BULL*ETIN :== $BULL_DIR:BULLETIN $ BULLETIN/STARTUP