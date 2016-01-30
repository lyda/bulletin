d From:	HENRY::IN%"MRL%PFCVAX%xx.lcs.mit.edu%csnet-relay.CSNET%relay.cs.net@RCA.COM" 19-JUN-1987 20:58L To:	"SYSTEM%UK.AC.SOTON.ESP.V1" <SYSTEM%UK.AC.SOTON.ESP.V1%ucl-cs.arpa@xx>,  Subj:	BULLETIN5.FOR    C ! C  BULLETIN5.FOR, Version 5/17/87 H C  Purpose: Contains subroutines for the bulletin board utility program.' C  Environment: MIT PFC VAX-11/780, VMS  C  Programmer: Mark R. London  C  	SUBROUTINE SET_LOGIN(LOGIN) C  C  SUBROUTINE SET_LOGIN  C ; C  FUNCTION: Enables or disables bulletin display at login.  C  	IMPLICIT INTEGER (A-Z)    	INCLUDE 'BULLUSER.INC'    	CHARACTER TODAY*23    	DIMENSION NOLOGIN_BTIM(2)  2 	CALL SYS$ASCTIM(,TODAY,,)		! Get the present time   	IF (.NOT.SETPRV_PRIV()) THEN  	   WRITE (6,'( 4      &      '' ERROR: No privs to change LOGIN.'')')
 	   RETURN 	END IF   * 	IER = CLI$GET_VALUE('USERNAME',TEMP_USER)   	CALL OPEN_FILE_SHARED(4)    	DO WHILE (REC_LOCK(IER)) ? 	   READ (4,FMT=USER_FMT,KEYEQ=TEMP_USER,IOSTAT=IER) TEMP_USER, 3      &	     LOGIN_BTIM,READ_BTIM,NEW_FLAG,SET_FLAG, ?      &	     BRIEF_FLAG,NOTIFY_FLAG		! Find if there is an entry  	END DO   + 	CALL SYS$BINTIM('5-NOV-2956',NOLOGIN_BTIM)  	IF (IER.EQ.0) THEN B 	   IF (LOGIN.AND.COMPARE_BTIM(LOGIN_BTIM,NOLOGIN_BTIM).EQ.0) THEN( 	      CALL SYS$BINTIM(TODAY,LOGIN_BTIM) 	   ELSE IF (.NOT.LOGIN) THEN & 	      LOGIN_BTIM(1) = NOLOGIN_BTIM(1)& 	      LOGIN_BTIM(2) = NOLOGIN_BTIM(2)
 	   END IF2 	   REWRITE (4,FMT=USER_FMT) TEMP_USER,LOGIN_BTIM,@      &	       READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG 	ELSE ; 	   WRITE (6,'('' ERROR: Specified username not found.'')')  	END IF    	CALL CLOSE_FILE(4)    	RETURN  	END        : 	SUBROUTINE GET_UAF(USERNAME,USER,GROUP,ACCOUNT,FLAGS,IER)   	IMPLICIT INTEGER (A-Z)   / 	PARAMETER UAF$V_DISACNT = 4, UAF$L_UIC = '24'X  	PARAMETER UAF$L_ACCOUNT = 53  	PARAMETER UAF$L_FLAGS = '1D4'X & 	PARAMETER INPUT_LEN = UAF$L_FLAGS + 4  5 	CHARACTER INPUT*(INPUT_LEN),USERNAME*(*),ACCOUNT*(*)   ( 	EQUIVALENCE (INPUT(UAF$L_UIC+1:),USER2)) 	EQUIVALENCE (INPUT(UAF$L_UIC+3:),GROUP2) + 	EQUIVALENCE (INPUT(UAF$L_FLAGS+1:),FLAGS2)    	INTEGER*2 USER2,GROUP2    	CALL OPEN_FILE_SHARED(8)   .         READ (8,KEY=USERNAME,IOSTAT=IER) INPUT# 						! Move pointer to top of file    	CALL CLOSE_FILE(8)    	IF (IER.NE.0) THEN  	   CALL ERRSNS(IDUMMY,IER)  	   WRITE (6,'( A      &		    '' ERROR: Specified username cannot be verified.'')')  	   CALL SYS_GETMSG(IER) 	ELSE  	   FLAGS = FLAGS2 	   IER = 1  	   USER = USER2 	   GROUP = GROUP22 	   ACCOUNT = INPUT(UAF$L_ACCOUNT:UAF$L_ACCOUNT+7) 	END IF    	RETURN  	END        	SUBROUTINE DCLEXH(EXIT_ROUTINE)   	IMPLICIT INTEGER (A-Z)    	INTEGER*4 EXBLK(4)    	EXBLK(2) = EXIT_ROUTINE 	EXBLK(3) = 1  	EXBLK(4) = %LOC(EXBLK(4))   	CALL SYS$DCLEXH(EXBLK(1))   	RETURN  	END          	SUBROUTINE CRELNM(INPUT,OUTPUT) 	  	IMPLICIT INTEGER (A-Z)    	INCLUDE '($PSLDEF)'   	INCLUDE '($LNMDEF)'   	CHARACTER*(*) INPUT,OUTPUT    	CALL INIT_ITMLST 8 	CALL ADD_2_ITMLST(LEN(OUTPUT),LNM$_STRING,%LOC(OUTPUT)) 	CALL END_ITMLST(CRELNM_ITMLST)   2 	IER = SYS$CRELNM(,'LNM$PROCESS',INPUT,PSL$C_USER,      &		%VAL(CRELNM_ITMLST))   	RETURN  	END       	SUBROUTINE GETPRIV  C  C  SUBROUTINE GETPRIV  C  C  FUNCTION: C	To get process privileges. C  OUTPUTS:   C	PROCPRIV - Returned privileges C    	IMPLICIT INTEGER (A-Z)   , 	COMMON /PRIVILEGES/ PROCPRIV(2),NEEDPRIV(2)   	INCLUDE '($JPIDEF)'  ( 	CALL INIT_ITMLST	! Initialize item list2 	CALL ADD_2_ITMLST(8,JPI$_PROCPRIV,%LOC(PROCPRIV))9 	CALL END_ITMLST(GETJPI_ITMLST)	! Get address of itemlist   9 	IER = SYS$GETJPIW(,,,%VAL(GETJPI_ITMLST),,,,) ! Get info    	RETURN  	END         	LOGICAL FUNCTION SETPRV_PRIV  	IMPLICIT INTEGER (A-Z)   , 	COMMON /PRIVILEGES/ PROCPRIV(2),NEEDPRIV(2) 	DATA NEEDPRIV/0,0/    	INCLUDE '($PRVDEF)'   	INCLUDE 'BULLUSER.INC'   0 	IF (NEEDPRIV(1).EQ.0.AND.NEEDPRIV(2).EQ.0) THEN5 	   CALL OPEN_FILE_SHARED(4)		! Get BULLUSER.DAT file  	   DO WHILE (REC_LOCK(IER))7 	      READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER) 4      &		TEMP_USER,NEWEST_BTIM,	! Get newest bulletin<      &		BBOARD_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
 	   END DO 	   CALL CLOSE_FILE(4) 	   NEEDPRIV(1) = NEW_FLAG(1)  	   NEEDPRIV(2) = NEW_FLAG(2)  	END IF   + 	IF ((PROCPRIV(1).AND.NEEDPRIV(1)).GT.0.OR. 3      &	    (PROCPRIV(2).AND.NEEDPRIV(2)).GT.0) THEN  	   SETPRV_PRIV = .TRUE. 	ELSE  	   SETPRV_PRIV = .FALSE.  	END IF    	RETURN  	END       	LOGICAL FUNCTION OPER_PRIV  	IMPLICIT INTEGER (A-Z) , 	COMMON /PRIVILEGES/ PROCPRIV(2),NEEDPRIV(2) 	INCLUDE '($PRVDEF)'* 	OPER_PRIV = BTEST(PROCPRIV(1),PRV$V_OPER) 	RETURN  	END          	SUBROUTINE GETUSER(USERNAME)  C  C  SUBROUTINE GETUSER  C  C  FUNCTION:% C	To get username of present process.  C  OUTPUTS: 3 C	USERNAME   -   Username owner of present process.  C    	IMPLICIT INTEGER (A-Z)   1 	CHARACTER*(*) USERNAME		! Limit is 12 characters    	INCLUDE '($JPIDEF)'  ( 	CALL INIT_ITMLST	! Initialize item list> 	CALL ADD_2_ITMLST(LEN(USERNAME),JPI$_USERNAME,%LOC(USERNAME))9 	CALL END_ITMLST(GETJPI_ITMLST)	! Get address of itemlist   9 	IER = SYS$GETJPIW(,,,%VAL(GETJPI_ITMLST),,,,) ! Get info    	RETURN  	END       	SUBROUTINE GETACC(ACCOUNT)  C  C  SUBROUTINE GETACC C  C  FUNCTION:$ C	To get account of present process. C  OUTPUTS: 1 C	ACCOUNT   -   ACCOUNT owner of present process.  C    	IMPLICIT INTEGER (A-Z)   0 	CHARACTER*(*) ACCOUNT		! Limit is 12 characters   	INCLUDE '($JPIDEF)'  ( 	CALL INIT_ITMLST	! Initialize item list; 	CALL ADD_2_ITMLST(LEN(ACCOUNT),JPI$_ACCOUNT,%LOC(ACCOUNT)) 9 	CALL END_ITMLST(GETJPI_ITMLST)	! Get address of itemlist   9 	IER = SYS$GETJPIW(,,,%VAL(GETJPI_ITMLST),,,,) ! Get info    	RETURN  	END         	SUBROUTINE GETSTS(STS)  C  C  SUBROUTINE GETSTS C  C  FUNCTION:F C	To get status of present process. This tells if its a batch process. C  OUTPUTS: + C	STS   -   Status word of present process.  C    	IMPLICIT INTEGER (A-Z)    	INCLUDE '($JPIDEF)'  ( 	CALL INIT_ITMLST	! Initialize item list( 	CALL ADD_2_ITMLST(4,JPI$_STS,%LOC(STS))9 	CALL END_ITMLST(GETJPI_ITMLST)	! Get address of itemlist   9 	IER = SYS$GETJPIW(,,,%VAL(GETJPI_ITMLST),,,,) ! Get info    	RETURN  	END         	SUBROUTINE HELP(LIBRARY)    	IMPLICIT INTEGER (A-Z) & 	EXTERNAL LIB$PUT_OUTPUT,LIB$GET_INPUT 	CHARACTER*(*) LIBRARY  & 	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P 	CHARACTER*64 BULL_PARAMETER  8 	IER = CLI$GET_VALUE('HELP_FOLDER',BULL_PARAMETER,LEN_P)  = 	CALL LBR$OUTPUT_HELP(LIB$PUT_OUTPUT,,BULL_PARAMETER(1:LEN_P)        &		,LIBRARY,,LIB$GET_INPUT)   	RETURN  	END      , 	INTEGER FUNCTION LNM_MODE_EXEC(FAB,RAB,LUN)   	IMPLICIT INTEGER (A-Z)    	INCLUDE '($FABDEF)' 	INCLUDE '($RABDEF)'   	RECORD /FABDEF/ FAB 	RECORD /RABDEF/ RAB  , 	FAB.FAB$B_ACMODES = ISHFT(1,FAB$V_LNM_MODE)   	STATUS = SYS$OPEN(FAB) & 	IF (STATUS) STATUS = SYS$CONNECT(RAB)   	LNM_MODE_EXEC = STATUS    	END       	INTEGER FUNCTION REC_LOCK(IER)    	INCLUDE '($FORIOSDEF)'    	DATA INIT /.TRUE./    	IF (INIT) THEN  	   REC_LOCK = 1 	   INIT = .FALSE. 	ELSE & 	   IF (IER.EQ.FOR$IOS_SPERECLOC) THEN 	      REC_LOCK = 1  	   ELSE 	      REC_LOCK = 0  	      INIT = .TRUE.
 	   END IF 	END IF    	RETURN  	END   	INTEGER FUNCTION TRIM(INPUT)  	CHARACTER*(*) INPUT  	CALL STR$TRIM(INPUT,INPUT,TRIM) 	RETURN  	END   	SUBROUTINE SYS_GETMSG(IER)    	IMPLICIT INTEGER (A-Z)    	CHARACTER*80 MESSAGE   " 	CALL LIB$SYS_GETMSG(IER,,MESSAGE) 	WRITE (6,'(A)') MESSAGE   	RETURN  	END       C ! C  BULLSUB3.FOR, Version 12/18/86 H C  Purpose: Contains subroutines for the bulletin board utility program.' C  Environment: MIT PFC VAX-11/780, VMS  C  Programmer: Mark R. London  C  	SUBROUTINE CLOSE_FILE(INPUT)  C  C  SUBROUTINE CLOSE_FILE C C C  FUNCTION: To close out the bulletin files and enable CTRL-C & -Y  C 	 C  INPUT: - C	INPUT  -  Unit number of file to close out.  C	          1 = BULLETIN.DAT C		  2 = BULLDIR.DAT C		  4 = BULLUSER.DAT  C		  7 = BULLFOLDER.DAT  C		  8 = SYS$SYSTEM:SYSUAF.DAT C    	CALL ENABLE_CTRL    	CLOSE (UNIT=INPUT)    	RETURN  	END    $ 	SUBROUTINE CLOSE_FILE_DELETE(INPUT)   	IMPLICIT INTEGER (A-Z)    	CALL ENABLE_CTRL   # 	CLOSE (UNIT=INPUT,STATUS='DELETE')    	RETURN  	END     	SUBROUTINE OPEN_FILE(INPUT)   	IMPLICIT INTEGER (A-Z)    	INCLUDE 'BULLFILES.INC'   	INCLUDE 'BULLFOLDER.INC'    	INCLUDE 'BULLUSER.INC'    	INCLUDE '($FORIOSDEF)'    	INCLUDE '($PRVDEF)'  > 	EXTERNAL BULLDIR_ERR,BULLETIN_ERR,BULLUSER_ERR,BULLFOLDER_ERR  % 	PARAMETER TIMEOUT = -10*1000*1000*30  	DIMENSION TIMEBUF(2) & 	DATA TIMEBUF /TIMEOUT,-1/, TIMEEFN/0/  + 	IF (TIMEEFN.EQ.0) CALL LIB$GET_EF(TIMEEFN)   2 	CALL DISABLE_CTRL		! No breaks while file is open   	IF (INPUT.EQ.2) THEN 8 	   IER = SYS$SETIMR(%VAL(TIMEEFN),TIMEBUF,BULLDIR_ERR,)" 	   DO WHILE (FILE_LOCK(IER,IER1))7 	    OPEN (UNIT=2,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE)) 6      1	      //'.BULLDIR',STATUS='UNKNOWN',IOSTAT=IER,?      1	      RECORDTYPE='FIXED',RECORDSIZE=115,ACCESS='DIRECT', E      1	      ORGANIZATION='RELATIVE',DISPOSE='KEEP',FORM='FORMATTED') > 	    IF (IER.EQ.FOR$IOS_FILNOTFOU.AND.FOLDER_NUMBER.EQ.0) THEN> 	       IER2 = LIB$RENAME_FILE(BULLDIR_FILE,'GENERAL.BULLDIR')H 	       IF (IER2) IDUMMY = FILE_LOCK(IER,IER1) ! Don't break out of loop, 	    ELSE IF (IER.EQ.FOR$IOS_INCRECLEN) THEND 	       IDUMMY = FILE_LOCK(IER,IER1)	! Avoid breaking out of DO loop 	       CALL CONVERT_BULLFILES 	    END IF 
 	   END DO 	END IF    	IF (INPUT.EQ.1) THEN 9 	   IER = SYS$SETIMR(%VAL(TIMEEFN),TIMEBUF,BULLETIN_ERR,) " 	   DO WHILE (FILE_LOCK(IER,IER1))7 	    OPEN (UNIT=1,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE)) 6      1	      //'.BULLFIL',STATUS='UNKNOWN',IOSTAT=IER,>      1	      ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=32,       1	      FORM='UNFORMATTED')> 	    IF (IER.EQ.FOR$IOS_FILNOTFOU.AND.FOLDER_NUMBER.EQ.0) THEN? 	       IER2 = LIB$RENAME_FILE(BULLETIN_FILE,'GENERAL.BULLFIL') H 	       IF (IER2) IDUMMY = FILE_LOCK(IER,IER1) ! Don't break out of loop, 	    ELSE IF (IER.EQ.FOR$IOS_INCRECLEN) THEND 	       IDUMMY = FILE_LOCK(IER,IER1)	! Avoid breaking out of DO loop 	       CALL CONVERT_BULLFILE  	    END IF 
 	   END DO 	END IF    	IF (INPUT.EQ.4) THEN 9 	   IER = SYS$SETIMR(%VAL(TIMEEFN),TIMEBUF,BULLUSER_ERR,) " 	   DO WHILE (FILE_LOCK(IER,IER1))2 	    OPEN (UNIT=4,FILE=BULLUSER_FILE,STATUS='OLD',E      1	     ACCESS='KEYED',RECORDTYPE='FIXED',RECORDSIZE=28+FLONG*16, ?      1	     FORM='FORMATTED',ORGANIZATION='INDEXED',IOSTAT=IER, !      1	     KEY=(1:12:CHARACTER)) ' 	    IF (IER.EQ.FOR$IOS_FILNOTFOU) THEN 7 	     OPEN (UNIT=4,FILE=BULLUSER_FILE,STATUS='UNKNOWN', F      1	      ACCESS='KEYED',RECORDTYPE='FIXED',RECORDSIZE=28+FLONG*16,@      1	      FORM='FORMATTED',ORGANIZATION='INDEXED',IOSTAT=IER,"      1	      KEY=(1:12:CHARACTER))5 	     WRITE (4,FMT=USER_FMT) USER_HEADER,NEWEST_BTIM, 7      1	      BBOARD_BTIM,PRV$M_OPER.OR.PRV$M_CMKRNL.OR. +      1	      PRV$M_SETPRV,(0,I=1,FLONG*4-1) , 	    ELSE IF (IER.EQ.FOR$IOS_INCRECLEN) THEN# 	      IDUMMY = FILE_LOCK(IER,IER1)  	      CALL CONVERT_USERFILE 	    END IF 
 	   END DO 	END IF    	IF (INPUT.EQ.7) THEN ; 	   IER = SYS$SETIMR(%VAL(TIMEEFN),TIMEBUF,BULLFOLDER_ERR,) " 	   DO WHILE (FILE_LOCK(IER,IER1))4 	    OPEN (UNIT=7,FILE=BULLFOLDER_FILE,STATUS='OLD',.      1	     ACCESS='KEYED',RECORDTYPE='FIXED',%      1	     RECORDSIZE=FOLDER_RECORD, ?      1	     FORM='FORMATTED',ORGANIZATION='INDEXED',IOSTAT=IER, /      1	     KEY=(1:25:CHARACTER,26:29:INTEGER)) 
 	   END DO& 	   IF (IER.EQ.FOR$IOS_FILNOTFOU) THEN 	      FOLDER1 = 'GENERAL' 	      FOLDER1_OWNER = 'SYSTEM' ; 	      FOLDER1_DESCRIP = 'Default general bulletin folder.'  	      FOLDER1_BBOARD = 'NONE' 	      FOLDER1_BBEXPIRE = 14: 	      OPEN (UNIT=7,FILE=BULLFOLDER_FILE,STATUS='UNKNOWN',1      1	        ACCESS='KEYED',RECORDTYPE='FIXED', (      1	        RECORDSIZE=FOLDER_RECORD,B      1	        FORM='FORMATTED',ORGANIZATION='INDEXED',IOSTAT=IER,2      1	        KEY=(1:25:CHARACTER,26:29:INTEGER))+ 	      WRITE (7,FMT=FOLDER_FMT,IOSTAT=IER1) /      &		FOLDER1,0,FOLDER1_OWNER,FOLDER1_DESCRIP >      &		,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,USERB,GROUPB,ACCOUNTB
 	   END IF 	END IF    	IF (IER.NE.0) THEN 2 	   WRITE (6,'('' Cannot open unit = '',I)') INPUT+ 	   IF (IER1.EQ.0) CALL ERRSNS(IDUMMY,IER1)  	   CALL SYS_GETMSG(IER1) 5 	   CALL ENABLE_CTRL_EXIT	! Enable CTRL-Y & -C & EXIT  	END IF   4 	IER = SYS$CANTIM(,)		! Successful, so cancel timer.   	RETURN  	END   	SUBROUTINE TIMER_ERR    	IMPLICIT INTEGER (A-Z)    	ENTRY BULLDIR_ERRF 	WRITE(6,'('' ERROR: Unable to open directory file after 30 secs.'')')	 	GO TO 10    	ENTRY BULLETIN_ERR D 	WRITE(6,'('' ERROR: Unable to open message file after 30 secs.'')')	 	GO TO 10    	ENTRY BULLUSER_ERR D 	WRITE(6,'('' ERROR: Unable to open BULLUSER.DAT after 30 secs.'')')	 	GO TO 10    	ENTRY BULLFOLDER_ERR F 	WRITE(6,'('' ERROR: Unable to open BULLFOLDER.DAT after 30 secs.'')')	 	GO TO 10   8 10	CALL ENABLE_CTRL_EXIT		! No breaks while file is open 	END      # 	SUBROUTINE OPEN_FILE_SHARED(INPUT)    	IMPLICIT INTEGER (A-Z)    	INCLUDE '($FORIOSDEF)'    	INCLUDE 'BULLFILES.INC'   	INCLUDE 'BULLFOLDER.INC'    	INCLUDE 'BULLUSER.INC'    	EXTERNAL LNM_MODE_EXEC    	CALL DISABLE_CTRL   	IF (INPUT.EQ.2) THEN " 	   DO WHILE (FILE_LOCK(IER,IER1))7 	    OPEN (UNIT=2,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE)) '      1	      //'.BULLDIR',STATUS='OLD', ?      1	      RECORDTYPE='FIXED',RECORDSIZE=115,ACCESS='DIRECT', E      1	      ORGANIZATION='RELATIVE',DISPOSE='KEEP',FORM='FORMATTED', (      1	      SHARED,READONLY,IOSTAT=IER)> 	    IF (IER.EQ.FOR$IOS_FILNOTFOU.AND.FOLDER_NUMBER.EQ.0) THEN> 	       IER2 = LIB$RENAME_FILE(BULLDIR_FILE,'GENERAL.BULLDIR')H 	       IF (IER2) IDUMMY = FILE_LOCK(IER,IER1) ! Don't break out of loop, 	    ELSE IF (IER.EQ.FOR$IOS_INCRECLEN) THEND 	       IDUMMY = FILE_LOCK(IER,IER1)	! Avoid breaking out of DO loop 	       CALL CONVERT_BULLFILES 	    END IF 
 	   END DO 	END IF    	IF (INPUT.EQ.1) THEN " 	   DO WHILE (FILE_LOCK(IER,IER1))7 	    OPEN (UNIT=1,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE)) '      1	      //'.BULLFIL',STATUS='OLD', >      1	      ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=32,;      1	      FORM='UNFORMATTED',IOSTAT=IER,SHARED,READONLY) > 	    IF (IER.EQ.FOR$IOS_FILNOTFOU.AND.FOLDER_NUMBER.EQ.0) THEN? 	       IER2 = LIB$RENAME_FILE(BULLETIN_FILE,'GENERAL.BULLFIL') H 	       IF (IER2) IDUMMY = FILE_LOCK(IER,IER1) ! Don't break out of loop, 	    ELSE IF (IER.EQ.FOR$IOS_INCRECLEN) THEND 	       IDUMMY = FILE_LOCK(IER,IER1)	! Avoid breaking out of DO loop 	       CALL CONVERT_BULLFILE  	    END IF 
 	   END DO 	END IF    	IF (INPUT.EQ.4) THEN " 	   DO WHILE (FILE_LOCK(IER,IER1))2 	    OPEN (UNIT=4,FILE=BULLUSER_FILE,STATUS='OLD',D      1	    ACCESS='KEYED',RECORDTYPE='FIXED',RECORDSIZE=28+FLONG*16,E      1	    IOSTAT=IER,FORM='FORMATTED',ORGANIZATION='INDEXED',SHARED,        1	    KEY=(1:12:CHARACTER))' 	    IF (IER.EQ.FOR$IOS_INCRECLEN) THEN $ 	       IDUMMY = FILE_LOCK(IER,IER1) 	       CALL CONVERT_USERFILE  	    END IF 
 	   END DO 	END IF    	IF (INPUT.EQ.7) THEN " 	   DO WHILE (FILE_LOCK(IER,IER1))4 	    OPEN (UNIT=7,FILE=BULLFOLDER_FILE,STATUS='OLD',-      1	    ACCESS='KEYED',RECORDTYPE='FIXED', /      1	    RECORDSIZE=FOLDER_RECORD,IOSTAT=IER, :      1	    FORM='FORMATTED',ORGANIZATION='INDEXED',SHARED,.      1	    KEY=(1:25:CHARACTER,26:29:INTEGER))
 	   END DO 	END IF    	IF (INPUT.EQ.8) THEN " 	   DO WHILE (FILE_LOCK(IER,IER1))D 	    OPEN (UNIT=8,FILE='SYSUAF',DEFAULTFILE='SYS$SYSTEM:SYSUAF.DAT',F      &       ACCESS='KEYED',FORM='UNFORMATTED',ORGANIZATION='INDEXED',5      &       STATUS='OLD',READONLY,IOSTAT=IER,SHARED, #      &	     USEROPEN=LNM_MODE_EXEC) 
 	   END DO 	END IF   2 	IF (IER.EQ.FOR$IOS_FILNOTFOU.AND.INPUT.NE.8) THEN, 	   CALL SYS$SETDFPROT('FF00'X,CUR_DEF_PROT)9 			! Set protection to (SYSTEM:RWE,OWNER:RWE,WORLD,GROUP)  	   CALL OPEN_FILE(INPUT) @ 	   CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection 	ELSE IF (IER.NE.0) THEN2 	   WRITE (6,'('' Cannot open unit = '',I)') INPUT+ 	   IF (IER1.EQ.0) CALL ERRSNS(IDUMMY,IER1)  	   CALL SYS_GETMSG(IER1)  	   CALL ENABLE_CTRL_EXIT  	END IF    	RETURN  	END         	SUBROUTINE CONVERT_BULLFILES  C  C  SUBROUTINE CONVERT_BULLFILES  C 8 C  FUNCTION: Converts bulletin files to new format file.C C	Add expiration time to directory file, add extra byte to bulletin @ C	file to show where each bulletin starts (for redunancy sake in C	case crash occurs).  C    	IMPLICIT INTEGER (A-Z)    	INCLUDE 'BULLDIR.INC'   	INCLUDE 'BULLFOLDER.INC'    	INCLUDE 'BULLFILES.INC'   	CHARACTER*81 INPUT,NEW_FILE  E 	WRITE (6,'('' Converting data files to new format. Please wait.'')')   3 	OPEN (UNIT=9,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE)) '      1	      //'.BULLDIR',STATUS='OLD', ?      1	      RECORDTYPE='FIXED',RECORDSIZE=107,ACCESS='DIRECT', E      1	      ORGANIZATION='RELATIVE',DISPOSE='KEEP',FORM='FORMATTED', (      1	      SHARED,READONLY,IOSTAT=IER)  % 	IF (IER.NE.0) THEN				! Error.  Why?  	   CALL ERRSNS(IDUMMY,IER)  	   CALL SYS_GETMSG(IER) 	   CALL EXIT  	END IF   4 	OPEN (UNIT=10,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE))'      1	      //'.BULLFIL',STATUS='OLD', .      1	      RECORDTYPE='FIXED',RECORDSIZE=80,9      1	      FORM='FORMATTED',IOSTAT=IER,SHARED,READONLY)   % 	IF (IER.NE.0) THEN				! Error.  Why?  	   CALL ERRSNS(IDUMMY,IER)  	   CALL SYS_GETMSG(IER) 	   CALL EXIT  	END IF   ) 	CALL SYS$SETDFPROT('FF00'X,CUR_DEF_PROT) 9 			! Set protection to (SYSTEM:RWE,OWNER:RWE,WORLD,GROUP)   3 	OPEN (UNIT=1,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE)) 2      1	      //'.BULLFIL',STATUS='NEW',IOSTAT=IER,>      1	      ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=81,      1	      FORM='FORMATTED')  3 	OPEN (UNIT=2,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE)) '      1	      //'.BULLDIR',STATUS='NEW', ?      1	      RECORDTYPE='FIXED',RECORDSIZE=115,ACCESS='DIRECT', E      1	      ORGANIZATION='RELATIVE',DISPOSE='KEEP',FORM='FORMATTED',       1	      IOSTAT=IER)   	NEWEST_EXTIME = '00:00:00'  	READ (9'1,1000,IOSTAT=IER) .      &		NEWEST_EXDATE,NEWEST_DATE,NEWEST_TIME,9      &		NBULL,NBLOCK,SHUTDOWN,SHUTDOWN_DATE,SHUTDOWN_TIME  	NEMPTY = 0 $ 	IF (IER.EQ.0) CALL WRITEDIR(0,IER1)   	EXTIME = '00:00:00' 	ICOUNT = 2  	DO WHILE (IER.EQ.0)" 	   READ(9'ICOUNT,1010,IOSTAT=IER)9      &		DESCRIP,FROM,DATE,TIME,LENGTH,EXDATE,SYSTEM,BLOCK  	   IF (IER.EQ.0) THEN 	      READ(10,'(A)') INPUT * 	      WRITE(1,'(A)') INPUT(1:80)//CHAR(1) 	      DO I=2,LENGTH 	         READ(10,'(A)') INPUT 	         WRITE(1,'(A)') INPUT 	      END DO # 	      CALL WRITEDIR(ICOUNT-1,IER1)  	      ICOUNT = ICOUNT + 1
 	   END IF 	END DO    	CLOSE (UNIT=9)  	CLOSE (UNIT=2)  	CLOSE (UNIT=10) 	CLOSE (UNIT=1)   = 	CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection  	RETURN   ' 1000	FORMAT(A11,A11,A8,A4,A4,A4,A11,A8) ( 1010	FORMAT(A53,A12,A11,A8,A4,A11,A4,A4)   	END   	SUBROUTINE CONVERT_BULLFILE C  C  SUBROUTINE CONVERT_BULLFILE C < C  FUNCTION: Converts bulletin data file to new format file. C > C  NOTE: CONVERT_BULLFILES converts from 80 to 81 byte length.> C	 This converts from 81 byte length to 128 compressed format. C    	IMPLICIT INTEGER (A-Z)    	INCLUDE 'BULLDIR.INC'   	INCLUDE 'BULLFOLDER.INC'    	INCLUDE 'BULLFILES.INC'   	CHARACTER*80 INPUT,NEW_FILE  E 	WRITE (6,'('' Converting data files to new format. Please wait.'')')    	CALL CLOSE_FILE(2)   ) 	CALL SYS$SETDFPROT('FF00'X,CUR_DEF_PROT) 9 			! Set protection to (SYSTEM:RWE,OWNER:RWE,WORLD,GROUP)    	CALL OPEN_FILE(7)   100	DO WHILE (REC_LOCK(IER))# 	   READ (7,FMT=FOLDER_FMT,ERR=200) 8      &		FOLDER,FOLDER_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP<      &		,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB 	END DO   8 	FOLDER_FILE = FOLDER_DIRECTORY(:TRIM(FOLDER_DIRECTORY))      &		//FOLDER(:TRIM(FOLDER)) : 	NEW_FILE = FOLDER_FILE(:TRIM(FOLDER_FILE))//'.BULLFILOLD'? 	OPEN (UNIT=10,FILE=FOLDER_FILE(:TRIM(FOLDER_FILE))//'.BULLFIL'       1	      ,STATUS='OLD', >      1	      RECORDTYPE='FIXED',RECORDSIZE=81,ACCESS='DIRECT',9      1	      FORM='FORMATTED',IOSTAT=IER,SHARED,READONLY)   % 	IF (IER.NE.0) THEN				! Error.  Why?  	   CALL ERRSNS(IDUMMY,IER)  	   CALL SYS_GETMSG(IER) 	   CALL EXIT  	END IF   3 	OPEN (UNIT=1,FILE=FOLDER_FILE(1:TRIM(FOLDER_FILE)) /      1	   //'.BULLFIL',STATUS='NEW',IOSTAT=IER, ;      1	   ACCESS='DIRECT',RECORDTYPE='FIXED',RECORDSIZE=32,       1	   FORM='UNFORMATTED') 6 	IER = LIB$RENAME_FILE(FOLDER_FILE(:TRIM(FOLDER_FILE))!      &		//'.BULLFIL;-1',NEW_FILE)    	CALL OPEN_FILE(2)   	CALL READDIR(0,IER)   	IF (IER.EQ.1) THEN  	 NBLOCK = 0 	 DO I=1,NBULL 	   CALL READDIR(I,IER)  	   NBLOCK = NBLOCK + 1  	   SBLOCK = NBLOCK  	   DO J=BLOCK,LENGTH+BLOCK-1  	      READ(10'J,'(A)') INPUT  	      LEN = TRIM(INPUT) 	      IF (LEN.EQ.0) LEN = 1( 	      CALL STORE_BULL(LEN,INPUT,NBLOCK)
 	   END DO 	   CALL FLUSH_BULL(NBLOCK)   	   LENGTH = NBLOCK - SBLOCK + 1 	   BLOCK = SBLOCK 	   CALL WRITEDIR(I,IER) 	 END DO   	 NEMPTY = 0 	 CALL WRITEDIR(0,IER) 	END IF    	CLOSE (UNIT=10) 	CLOSE (UNIT=1)    	CALL CLOSE_FILE(2) 	 	GOTO 100    200	CALL OPEN_FILE_SHARED(2)  = 	CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection    	RETURN    	END   	SUBROUTINE CONVERT_USERFILE C  C  SUBROUTINE CONVERT_USERFILE C F C  FUNCTION: Converts user file to new format which has 8 bytes added. C    	IMPLICIT INTEGER (A-Z)    	INCLUDE 'BULLFILES.INC'   	INCLUDE 'BULLUSER.INC'     	CHARACTER BUFFER*74,NEW_FILE*80  " 	CHARACTER*11 LOGIN_DATE,READ_DATE! 	CHARACTER*8 LOGIN_TIME,READ_TIME   E 	WRITE (6,'('' Converting data files to new format. Please wait.'')')   ? 	EODIR = MAX(INDEX(BULLUSER_FILE,':'),INDEX(BULLUSER_FILE,']')) 6 	SUFFIX = INDEX(BULLUSER_FILE(EODIR:),'.') + EODIR - 1) 	NEW_FILE = BULLUSER_FILE(:SUFFIX)//'OLD' . 	IER = LIB$RENAME_FILE(BULLUSER_FILE,NEW_FILE)  ) 	OPEN (UNIT=9,FILE=NEW_FILE,STATUS='OLD', .      1	     ACCESS='KEYED',RECORDTYPE='FIXED',?      1	     FORM='FORMATTED',ORGANIZATION='INDEXED',IOSTAT=IER, !      1	     KEY=(1:12:CHARACTER)) ! 	INQUIRE (UNIT=9,RECORDSIZE=RECL)    	IF (IER.EQ.0) THEN , 	   CALL SYS$SETDFPROT('FF00'X,CUR_DEF_PROT)9 			! Set protection to (SYSTEM:RWE,OWNER:RWE,WORLD,GROUP) 1 	   OPEN (UNIT=4,FILE=BULLUSER_FILE,STATUS='NEW', D      1	    ACCESS='KEYED',RECORDTYPE='FIXED',RECORDSIZE=28+FLONG*16,>      1	    FORM='FORMATTED',ORGANIZATION='INDEXED',IOSTAT=IER,       1	    KEY=(1:12:CHARACTER)) 	END IF    	IF (IER.NE.0) THEN 0 	   WRITE (6,'('' Cannot convert user file.'')')+ 	   IF (IER1.EQ.0) CALL ERRSNS(IDUMMY,IER1)  	   CALL SYS_GETMSG(IER1) @ 	   CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection 	   CALL ENABLE_CTRL_EXIT  	END IF    	DO I=1,FLONG  	   NEW_FLAG(I) = 'FFFFFFFF'X  	   NOTIFY_FLAG(I) = 0 	   BRIEF_FLAG(I) = 0  	   SET_FLAG(I) = 0  	END DO   = 	IF (RECL.EQ.42.OR.RECL.EQ.50.OR.RECL.EQ.58.OR.RECL.EQ.66.OR. &      &		RECL.EQ.74) THEN		! Old format 	   IF (RECL.LE.58) RECL = 50  	   IER = 0  	   DO WHILE (IER.EQ.0) - 	      READ (9,'(A<RECL>)',IOSTAT=IER) BUFFER  	      IF (IER.EQ.0) THEN  		TEMP_USER = BUFFER(1:12)# 	        LOGIN_DATE = BUFFER(13:23) # 	        LOGIN_TIME = BUFFER(24:31) " 	        READ_DATE = BUFFER(32:42)" 	        READ_TIME = BUFFER(43:50) 	        IF (RECL.EQ.58)9      &		  CALL LIB$MOVC3(8,%REF(BUFFER(51:)),SET_FLAG(1))  	        IF (RECL.EQ.66)9      &		  CALL LIB$MOVC3(8,%REF(BUFFER(59:)),NEW_FLAG(1))  	        IF (RECL.EQ.74)<      &		  CALL LIB$MOVC3(8,%REF(BUFFER(67:)),NOTIFY_FLAG(1))@ 	        CALL SYS$BINTIM(LOGIN_DATE//' '//LOGIN_TIME,LOGIN_BTIM)= 	        CALL SYS$BINTIM(READ_DATE//' '//READ_TIME,READ_BTIM) 5 	        WRITE (4,FMT=USER_FMT) TEMP_USER,LOGIN_BTIM, :      &		READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG 	    END IF 
 	   END DO 	   IF (RECL.LT.66) THENA 	     READ (4,KEY=USER_HEADER,FMT=USER_FMT) TEMP_USER,LOGIN_BTIM, :      &		READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG> 	     NEW_FLAG(1) = PRV$M_OPER.OR.PRV$M_CMKRNL.OR.PRV$M_SETPRV2 	     WRITE (4,FMT=USER_FMT) TEMP_USER,LOGIN_BTIM,:      &		READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
 	   END IF$ 	ELSE					! Folder maxmimum increase5 	   OFLONG = (RECL - 28) / 16		! Old  #longwords/flag  	   DO WHILE (IER.EQ.0) 4 	    READ (9,FMT='(A12,<4+OFLONG*4>A4)',IOSTAT=IER) +      &	     TEMP_USER,LOGIN_BTIM,READ_BTIM, >      &	     (NEW_FLAG(I),I=1,OFLONG),(SET_FLAG(I),I=1,OFLONG),B      &	     (BRIEF_FLAG(I),I=1,OFLONG),(NOTIFY_FLAG(I),I=1,OFLONG) 	    IF (IER.EQ.0) THEN 2 	     WRITE (4,FMT=USER_FMT) TEMP_USER,LOGIN_BTIM,:      &		READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG 	    END IF 
 	   END DO 	END IF    	IER = 0   	CLOSE (UNIT=9)  	CLOSE (UNIT=4)   = 	CALL SYS$SETDFPROT(CUR_DEF_PROT,)	! Reset default protection    	RETURN  	END    ( 	SUBROUTINE READDIR(BULLETIN_NUM,ICOUNT) C  C  SUBROUTINE READDIR  C > C  FUNCTION: Finds the entry for the specified bulletin in the< C	directory file and returns the information for that entry. C 
 C  INPUTS:3 C	BULLETIN_NUM  -  Bulletin number.  Starts with 1. 2 C			 If 0, gives header info, i.e number of bulls,, C			 number of blocks in bulletin file, etc. C  OUTPUTS: 2 C	ICOUNT  -  The last record read by this routine. C    	IMPLICIT INTEGER (A - Z)    	INCLUDE 'BULLDIR.INC'   	INCLUDE 'BULLFOLDER.INC'    	COMMON /PROMPT/ COMMAND_PROMPT  	CHARACTER*39 COMMAND_PROMPT   	CHARACTER*2 CFOLDER_NUMBER    	ICOUNT = BULLETIN_NUM   	IF (ICOUNT.EQ.0) THEN 	   DO WHILE (REC_LOCK(IER))  	    READ (2'1,1000,IOSTAT=IER) <      &		NEWEST_EXDATE,NEWEST_EXTIME,NEWEST_DATE,NEWEST_TIME,@      &		NBULL,NBLOCK,SHUTDOWN,SHUTDOWN_DATE,SHUTDOWN_TIME,NEMPTY
 	   END DO 	   IF (IER.EQ.0) THEN> 	      IF (NBULL.LT.0) THEN	! This indicates bulletin deletion 					! was incomplete. 		 CALL CLOSE_FILE(2)  		 CALL OPEN_FILE(2) 		 CALL CLEANUP_DIRFILE(1) 	      END IF ' 	      IF (NEMPTY.EQ.'    ') NEMPTY = 0  C E C  Check to see if cleanup of empty file space is necessary, which is E C  defined here as being 50 blocks (200 128byte records).  Also check @ C  to see if cleanup was in progress but didn't properly finish. C  	      IF (NEMPTY.GT.200) THEN. 		 WRITE (CFOLDER_NUMBER,'(I2)') FOLDER_NUMBER7 	         IER1 = LIB$SPAWN('$'//COMMAND_PROMPT(1:INDEX( >      &		  COMMAND_PROMPT,'>')-1)//'/CLEANUP='//CFOLDER_NUMBER,'      &		  'NL:','NL:',1,'BULL_CLEANUP') " 	      ELSE IF (NEMPTY.EQ.-1) THEN 		 CALL CLEANUP_BULLFILE 	      END IF 
 	   END IF 	ELSE  	   DO WHILE (REC_LOCK(IER))% 	    READ(2'ICOUNT+1,1010,IOSTAT=IER) D      &	     DESCRIP,FROM,DATE,TIME,LENGTH,EXDATE,EXTIME,SYSTEM,BLOCK
 	   END DO 	END IF   " 	IF (IER.EQ.0) ICOUNT = ICOUNT + 1   	RETURN   - 1000	FORMAT(A11,A8,A11,A8,A4,A4,A4,A11,A8,A4) + 1010	FORMAT(A53,A12,A11,A8,A4,A11,A8,A4,A4)    	END    & 	SUBROUTINE WRITEDIR(BULLETIN_NUM,IER) C  C  SUBROUTINE WRITEDIR C ? C  FUNCTION: Writes the entry for the specified bulletin in the  C	directory file.  C 
 C  INPUTS:3 C	BULLETIN_NUM  -  Bulletin number.  Starts with 1. 2 C			 If 0, write the header of the directory file. C  OUTPUTS:   C	IER - Error status from WRITE. C    	IMPLICIT INTEGER (A - Z)    	INCLUDE 'BULLDIR.INC' 	  	IF (BULLETIN_NUM.EQ.0) THEN< 	   WRITE (2'1,1000,IOSTAT=IER) NEWEST_EXDATE,NEWEST_EXTIME,#      &	    NEWEST_DATE,NEWEST_TIME, C      &	    NBULL,NBLOCK,SHUTDOWN,SHUTDOWN_DATE,SHUTDOWN_TIME,NEMPTY  	ELSE + 	   WRITE(2'BULLETIN_NUM+1,1010,IOSTAT=IER) C      &	    DESCRIP,FROM,DATE,TIME,LENGTH,EXDATE,EXTIME,SYSTEM,BLOCK  	END IF    	RETURN   - 1000	FORMAT(A11,A8,A11,A8,A4,A4,A4,A11,A8,A4) + 1010	FORMAT(A53,A12,A11,A8,A4,A11,A8,A4,A4)    	END