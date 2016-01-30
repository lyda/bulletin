m From:	HENRY::IN%"MRL%PFC-VAX.MIT.EDU%xx.lcs.mit.edu%csnet-relay.CSNET%relay.cs.net@RCA.COM" 29-OCT-1987 03:53 N To:	"EVERHART%ARISIA%RCA.COM" <EVERHART%ARISIA%RCA.COM%csnet-relay.csnet@xx>,  Subj:	BULLETIN0.FOR    C " C  BULLETIN0.FOR, Version 10/30/87H C  Purpose: Contains subroutines for the bulletin board utility program.' C  Environment: MIT PFC VAX-11/780, VMS  C  Programmer: Mark R. London  C  	SUBROUTINE DELETE C  C  SUBROUTINE DELETE C > C  FUNCTION:  Deletes a bulletin entry from the bulletin file. C  	IMPLICIT INTEGER (A - Z)    	COMMON /POINT/ BULL_POINT  & 	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P 	CHARACTER*64 BULL_PARAMETER  4 	COMMON /NODE_INFO/ NODES,LOCAL_NODE_FOUND,NODE_NUM,      &				NODE_ERROR,POINT_NODE  	CHARACTER*32 NODES(10) $ 	LOGICAL LOCAL_NODE_FOUND,NODE_ERROR  ' 	COMMON /DECNET/ DECNET_PROC,ERROR_UNIT  	LOGICAL DECNET_PROC   	INCLUDE 'BULLDIR.INC'   	INCLUDE 'BULLUSER.INC'    	INCLUDE 'BULLFOLDER.INC'    	EXTERNAL CLI$_ABSENT   6 	CHARACTER ANSWER*1,REMOTE_USER*12,SUBJECT*53,INPUT*20   	INTEGER EXBTIM(2),NOW(2)   A 	IF (CLI$PRESENT('NODES')) THEN	! Delete messages on DECNET node?  	   CALL DELETE_NODE		! Yes...
 	   RETURN7 	ELSE IF (DECNET_PROC) THEN	! Is this from remote node? / 	   IER = CLI$GET_VALUE('USERNAME',REMOTE_USER) / 	   IER = CLI$GET_VALUE('SUBJECT',SUBJECT,SLEN)  	   CALL OPEN_FILE(2)  	   BULL_DELETE = 0  	   IER = 1 # 	   DO WHILE (BULL_DELETE+1.EQ.IER) $ 	      BULL_DELETE = BULL_DELETE + 1$ 	      CALL READDIR(BULL_DELETE,IER)' 	      CALL STR$UPCASE(DESCRIP,DESCRIP) 7 	      IF (BULL_DELETE+1.EQ.IER.AND.REMOTE_USER.EQ.FROM 8      &		   .AND.INDEX(DESCRIP,SUBJECT(:SLEN)).GT.0) THEN 		 GO TO 50  	      END IF 
 	   END DO6 	   CALL CLOSE_FILE(2)		! Specified message not found,* 	   WRITE(ERROR_UNIT,1030)	! so error out.
 	   RETURN 	END IF    C ) C  Get the bulletin number to be deleted.  C   < 	IER = CLI$GET_VALUE('BULLETIN_NUMBER',BULL_PARAMETER,LEN_P)= 	IF (IER.NE.%LOC(CLI$_ABSENT)) THEN	! Was bulletin specified? < 	   DECODE(LEN_P,5,BULL_PARAMETER,ERR=920) BULL_DELETE	! Yes 5	   FORMAT(I<LEN_P>) D 	ELSE IF (BULL_POINT.EQ.0) THEN	! No.  Have we just read a bulletin?! 	   GO TO 910			! No, then error.  	ELSE = 	   BULL_DELETE = BULL_POINT	! Delete the file we are reading  	END IF     	IF (BULL_DELETE.LE.0) GO TO 920   C A C  Check to see if specified bulletin is present, and if the user ' C  is permitted to delete the bulletin.  C    	CALL OPEN_FILE(2)  @ 	CALL READDIR(BULL_DELETE,IER)	! Get info for specified bulletin  5 	IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found? 3 	   WRITE(ERROR_UNIT,1030)	! If not, then error out  	   GOTO 100 	END IF   A 	IF (USERNAME.NE.FROM) THEN	! If doesn't match owner of bulletin, G 	   IF ((.NOT.SETPRV_PRIV().AND..NOT.FOLDER_SET).OR.    ! Privileges or >      &	       (.NOT.SETPRV_PRIV().AND.USERNAME.NE.FOLDER_OWNER.      &		.AND.FOLDER_SET)) THEN ! folder owner?/ 	      WRITE(ERROR_UNIT,1040)	! Then error out.  	      GO TO 100 	   ELSE 	      CALL CLOSE_FILE (2)! 	      IF (.NOT.DECNET_PROC) THEN = 	         WRITE (6,1050)		! Make sure user wants to delete it * 	         READ (5,'(A)',IOSTAT=IER) ANSWER( 	         CALL STR$UPCASE(ANSWER,ANSWER)& 	         IF (ANSWER.NE.'Y') GO TO 900 	      END IF  	      CALL OPEN_FILE(2)F 	      CALL READDIR(BULL_DELETE,IER) ! Get info for specified bulletin; 	      IF (IER.NE.BULL_DELETE+1) THEN	! Was bulletin found? : 	         WRITE(ERROR_UNIT,1030)		! If not, then error out 	         GOTO 100 	      END IF 
 	   END IF 	END IF    C ' C  Delete the bulletin directory entry.  C   = 50	IF (CLI$PRESENT('IMMEDIATE')) THEN	! Delete it immediately ? 	   CALL DELETE_ENTRY(BULL_DELETE)	! Delete the directory entry   > 	   CALL CLEANUP_DIRFILE(BULL_DELETE)	! Reorder directory file  @ 	   IF ((SYSTEM.AND.4).EQ.4) THEN	! Was entry shutdown bulletin?0 	      CALL READDIR(0,IER)		! Get shutdown count: 	      SHUTDOWN = SHUTDOWN - 1		! Decrement shutdown count
 	   END IF  B 	   CALL UPDATE_ALWAYS	! Somewhat a kludgey way of updating latest! 				! bulletin and expired dates.   > 	   IF (BULL_DELETE.LE.BULL_POINT) BULL_POINT = BULL_POINT - 10 				! Readjust where which bulletin to read next. 				! if deletion causes messages to be moved. 	ELSE				! Delete it eventually  C ? C  Change year of expiration date of message to 100 years less, H C  to indicate that message is to be deleted.  Then, set expiration dateH C  in header of folder to 15 minutes from now.  Thus, the folder will beF C  checked in 15 minutes (or more), and will delete the messages then. C 5 	   IF (SYSTEM.LE.1) THEN	! General or System message . 	      EXDATE = EXDATE(1:7)//'18'//EXDATE(10:)# 	   ELSE				! Permanent or Shutdown # 	      IF (EXDATE(2:2).EQ.'-') THEN 0 	         EXDATE = EXDATE(1:6)//'19'//EXDATE(9:) 	      ELSE 1 	         EXDATE = EXDATE(1:7)//'19'//EXDATE(10:)  	      END IF 
 	   END IF  C 	   CALL WRITEDIR(BULL_DELETE,IER)	! Update message expiration date   D 	   IER = SYS$BINTIM('0 0:15',EXBTIM)	! Get time 15 minutes from now 	   IER = SYS$GETTIM(NOW) % 	   IER = LIB$SUBX(NOW,EXBTIM,EXBTIM) $ 	   IER = SYS$ASCTIM(,INPUT,EXBTIM,)  & 	   CALL READDIR(0,IER)			! Get header  @ 	   NEWEST_EXDATE = INPUT(1:11)		! and store new expiration date  	   NEWEST_EXTIME = INPUT(13:20)   	   CALL WRITEDIR(0,IER) 	END IF    100	CALL CLOSE_FILE(2)' 	IF (DECNET_PROC) WRITE (5,'(''END'')') & 				! Tell DECNET that delete went ok.
 900	RETURN   910	WRITE(6,1010) 
 	GO TO 900   920	WRITE(6,1020) 
 	GO TO 900  7 1010	FORMAT(' ERROR: You are not reading any message.') E 1020	FORMAT(' ERROR: Specified message number has incorrect format.') 7 1030	FORMAT(' ERROR: Specified message was not found.') A 1040	FORMAT(' ERROR: Message was not deleted. Not owned by you.') , 1050	FORMAT(' Message is not owned by you.',8      &	       ' Are you sure you want to delete it? ',$)   	END        	SUBROUTINE DIRECTORY(DIR_COUNT) C  C  SUBROUTINE DIRECTORY  C + C  FUNCTION: Display directory of messages.  C  	IMPLICIT INTEGER (A - Z)    	INCLUDE 'BULLDIR.INC'   	INCLUDE 'BULLUSER.INC'    	INCLUDE 'BULLFOLDER.INC'   " 	COMMON /PAGE/ PAGE_LENGTH, PAGING 	LOGICAL PAGING    	DATA SCRATCH_D1/0/    	COMMON /POINT/ BULL_POINT  / 	EXTERNAL CLI$_ABSENT,CLI$_NEGATED,CLI$_PRESENT   2 	CHARACTER START_PARAMETER*16,DATETIME*23,TODAY*11  - 	CALL LIB$ERASE_PAGE(1,1)		! Clear the screen    C K C  Directory listing is first buffered into temporary memory storage before L C  being outputted to the terminal.  This is to be able to quickly close theO C  directory file, and to avoid the possibility of the user holding the screen, J C  and thus causing the directory file to stay open.  The temporary memoryN C  is structured as a linked-list queue, where SCRATCH_D1 points to the headerE C  of the queue.  See BULLSUBS.FOR for more description of the queue.  C   ( 	CALL INIT_QUEUE(SCRATCH_D1,BULLDIR_COM) 	SCRATCH_D = SCRATCH_D1   / 	CALL OPEN_FILE_SHARED(2)		! Get directory file   5 	CALL READDIR(0,IER)			! Does directory header exist? 1 	IF (IER.EQ.1) THEN			! If so, there are messages  	   IF (DIR_COUNT.EQ.0) THEN? 	      IF (CLI$PRESENT('START')) THEN	! Start number specified? : 	         IER = CLI$GET_VALUE('START',START_PARAMETER,LEN): 	         DECODE(LEN,'(I<LEN>)',START_PARAMETER) DIR_COUNT 		 IF (DIR_COUNT.GT.NBULL) THEN  		    DIR_COUNT = NBULL   		 ELSE IF (DIR_COUNT.LT.1) THEN9 		    WRITE (6,'('' ERROR: Invalid starting message.'')')  		    CALL CLOSE_FILE(2) 		    DIR_COUNT = 0  		    RETURN	 		 END IF @ 	      ELSE IF (CLI$PRESENT('SINCE').OR.CLI$PRESENT('NEW')) THEN: 		 IF (CLI$PRESENT('SINCE')) THEN		! Was /SINCE specified?* 		   IER = CLI$GET_VALUE('SINCE',DATETIME)= 	   	   IF (DATETIME.EQ.'TODAY') THEN	! TODAY is the default. < 	      	     IER = SYS$ASCTIM(,TODAY,,)		! Need to get date.& 		     DATETIME = TODAY//' 00:00:00.0' 		   END IF : 		 ELSE IF (CLI$PRESENT('NEW')) THEN	! was /NEW specified?> 	   	   DIFF = COMPARE_BTIM(LAST_READ_BTIM(1,FOLDER_NUMBER+1),      &			       F_NEWEST_BTIM) 		   IF (DIFF.GE.0) THEN6 		     WRITE (6,'('' No new messages are present.'')') 		     CALL CLOSE_FILE(2)  		     RETURN		 		   ELSEM 		     CALL SYS$ASCTIM7      &			(,DATETIME,LAST_READ_BTIM(1,FOLDER_NUMBER+1),)1 		   END IFo	 		 END IF% 		 TEMP_COUNT = 0E
 		 IER = 1! 		 DO WHILE (IER.EQ.TEMP_COUNT+1)u  		   TEMP_COUNT = TEMP_COUNT + 1! 		   CALL READDIR(TEMP_COUNT,IER)o" 		   IF (IER.NE.TEMP_COUNT+1) THEN: 		     WRITE (6,'('' No messages past specified date.'')') 		     CALL CLOSE_FILE(2)M 		     RETURN 	 		   ELSET/ 		     DIFF = COMPARE_DATE(DATETIME(1:11),DATE)T? 		     IF (DIFF.EQ.0) DIFF = COMPARE_TIME(DATETIME(13:20),TIME)I 		     IF (DIFF.LT.0) THEN 			DIR_COUNT = TEMP_COUNTC 			IER = IER + 1 		     END IFN 		   END IFR	 		 END DOR 	      ELSEM  	         DIR_COUNT = BULL_POINT$ 		 IF (DIR_COUNT.EQ.0) DIR_COUNT = 1 	      END IFA; 	      IF (CLI$PRESENT('SINCE').OR.CLI$PRESENT('NEW')) THEN  		 SBULL = DIR_COUNT3 	         EBULL = DIR_COUNT + (PAGE_LENGTH - 6) - 1'- 	         IF (EBULL.GE.NBULL-2) EBULL = NBULLC8 	      ELSE IF (NBULL-DIR_COUNT+1.LE.PAGE_LENGTH-4) THEN 	         EBULL = NBULL*- 	         SBULL = NBULL - (PAGE_LENGTH-4) + 1W# 	         IF (SBULL.LT.1) SBULL = 1E 	      ELSEs 	         SBULL = DIR_COUNTA3 	         EBULL = DIR_COUNT + (PAGE_LENGTH - 6) - 1T 	      END IFs 	   ELSE 	      SBULL = DIR_COUNT0 	      EBULL = DIR_COUNT + (PAGE_LENGTH - 6) - 1* 	      IF (EBULL.GE.NBULL-2) EBULL = NBULL
 	   END IF 	   IF (.NOT.PAGING) THEN  	      EBULL = NBULL
 	   END IF0 	   DO I=SBULL,EBULL			! Copy messages from file, 	      CALL READDIR(I,IER)		! Into the queue> 	      CALL WRITE_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,BULLDIR_COM)
 	   END DO 	ELSEE 	   NBULL = 0  	END IF   2 	CALL CLOSE_FILE(2)			! We don't need file anymore   	IF (NBULL.EQ.0) THEN 5 	   WRITE (6,'('' There are no messages present.'')')o
 	   RETURN 	END IFR   CNG C  Directory entries are now in queue.  Output queue entries to screen.  Cb  8 	SCRATCH_D = SCRATCH_D1			! Init queue pointer to header    	WRITE(6,1000)				! Write header 	DO I=SBULL,EBULLT: 	   CALL READ_QUEUE(%VAL(SCRATCH_D),SCRATCH_D,BULLDIR_COM)> 	   IF (EXDATE(8:9).EQ.'18'.OR.INDEX(EXDATE,'1900').GT.0) THEN4 	      WRITE(6,2010) I,DESCRIP(:52),FROM,'(DELETED)' 	   ELSE? 	      WRITE(6,2010) I,DESCRIP(:52),FROM,DATE(1:7)//DATE(10:11) 
 	   END IF 	END DOh  3 	DIR_COUNT = EBULL + 1			! Update directory counter.  7 	IF (DIR_COUNT.GT.NBULL) THEN		! Outputted all entries?s, 	   DIR_COUNT = 0			! Yes. Set counter to 0. 	ELSE - 	   WRITE(6,1010)			! Else say there are moreL 	END IF(   	RETURN,  < 1000	FORMAT('    #',1X,'Description',43X,'From',9X,'Date',/)/ 1010	FORMAT(1X,/,' Press RETURN for more...',/)_  & 2010	FORMAT(1X,I4,1X,A52,1X,A12,1X,A9)   	END  E   	SUBROUTINE FILE C. C  SUBROUTINE FILE Cn* C  FUNCTION:  Copies a bulletin to a file. CP 	IMPLICIT INTEGER (A - Z)E   	CHARACTER INPUT*80o   	COMMON /POINT/ BULL_POINT  & 	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P 	CHARACTER*64 BULL_PARAMETER   	INCLUDE 'BULLDIR.INC'   	EXTERNAL CLI$_ABSENTe  5 	IER = CLI$GET_VALUE('FILESPEC',BULL_PARAMETER,LEN_P)_  C 	IF (IER.EQ.%LOC(CLI$_ABSENT)) THEN	! If no file name was specified	  	   WRITE(6,1020)		! Write error 	   RETURN			! And return, 	END IFR  9 	IF (BULL_POINT.EQ.0) THEN	! If no bulletin has been read   	   WRITE(6,1010)		! Write error 	   RETURN			! And returnN 	END IF    	CALL OPEN_FILE_SHARED(2)T  ? 	CALL READDIR(BULL_POINT,IER)	! Get info for specified bulletinL  4 	IF (IER.NE.BULL_POINT+1) THEN	! Was bulletin found? 	   WRITE(6,1030)n0 	   CALL CLOSE_FILE(2)		! If not, then error out
 	   RETURN 	END IFD   	CALL CLOSE_FILE(2)h  . 	CALL OPEN_FILE_SHARED(1)	! Open BULLETIN file  < 	IF (.NOT.SETPRV_PRIV()) THEN		! If no SETPRV, remove SYSPRV4 	   CALL DISABLE_PRIVS			! privileges when trying to 	END IF					! create new file.   	IF (CLI$PRESENT('NEW')) THEN 5 	   OPEN(UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),ERR=900,b,      &		STATUS='NEW',CARRIAGECONTROL='LIST') 	ELSEc5 	   OPEN(UNIT=3,FILE=BULL_PARAMETER(1:LEN_P),ERR=900,o@      &		STATUS='UNKNOWN',CARRIAGECONTROL='LIST',ACCESS='APPEND') 	END IFu. 	CALL ENABLE_PRIVS			! Reset SYSPRV privileges  4 	IF (CLI$PRESENT('HEADER')) THEN		! Printout header?8 	   WRITE(3,1050) DESCRIP		! Output bulletin header info 	   WRITE(3,1060) FROM,DATEs 	END IF   	 	LEN = 81 4 	DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin into file 	   DO WHILE (LEN.GT.0)a! 	      CALL GET_BULL(I,INPUT,LEN)a 	      IF (LEN.LT.0) THENl 		 GO TO 90s 	      ELSE IF (LEN.GT.0) THEN& 	         WRITE (3,'(A)') INPUT(1:LEN) 	      END IFl
 	   END DO 	   LEN = 80 	END DOm  - 90	CLOSE (UNIT=3)			! Bulletin copy completed   1 	WRITE(6,1040) BULL_POINT,BULL_PARAMETER(1:LEN_P) ! 					! Show name of file created.D 100	CALL CLOSE_FILE(1) 	RETURN    900	WRITE(6,1000)I- 	CALL ENABLE_PRIVS		! Reset BYPASS privileges=
 	GO TO 100  - 1000	FORMAT(' ERROR: Error in opening file.')E6 1010	FORMAT(' ERROR: You have not read any bulletin.')2 1020	FORMAT(' ERROR: No file name was specified.')8 1030	FORMAT(' ERROR: Specified bulletin was not found.'), 1040	FORMAT(' Message ',I4,' written to ',A)" 1050	FORMAT(/,'Description: ',A53)) 1060	FORMAT('From: ',A12,' Date: ',A11,/)S   	END         	SUBROUTINE LOGINA CR C  SUBROUTINE LOGINr C 9 C  FUNCTION: Alerts user of new messages upon logging in.  Ce 	IMPLICIT INTEGER (A - Z)(   	INCLUDE 'BULLDIR.INC'   	INCLUDE 'BULLUSER.INC'A   	INCLUDE 'BULLFOLDER.INC'_   	COMMON /READIT/ READIT   & 	COMMON /BULLPAR/ BULL_PARAMETER,LEN_P 	CHARACTER*64 BULL_PARAMETER  ! 	COMMON /PAGE/ PAGE_LENGTH,PAGING  	LOGICAL PAGING0   	COMMON /POINT/ BULL_POINT   	COMMON /PROMPT/ COMMAND_PROMPT( 	CHARACTER*39 COMMAND_PROMPT  % 	CHARACTER TODAY*23,INPUT*80,INREAD*1(   	LOGICAL*1 CTRL_G/7/  6 	DATA GEN_DIR1/0/	! General directory link list header5 	DATA SYS_DIR1/0/	! System directory link list headers4 	DATA SYS_BUL1/0/	! System bulletin link list header   	DATA PAGE/0/t   	DATA FIRST_WRITE/.TRUE./T 	LOGICAL FIRST_WRITE  B 	DIMENSION H_NEW_FLAG(FLONG),H_SET_FLAG(FLONG),H_BRIEF_FLAG(FLONG); 	DIMENSION NOLOGIN_BTIM(2),LOGIN_BTIM_SAVE(2),TODAY_BTIM(2)L" 	DIMENSION DIR_BTIM(2),NEW_BTIM(2)   	CHARACTER*1 SEPARATEI  2 	CALL SYS$ASCTIM(,TODAY,,)		! Get the present time" 	CALL SYS_BINTIM(TODAY,TODAY_BTIM)  + 	CALL SYS_BINTIM('5-NOV-2956',NOLOGIN_BTIM)N0 	CALL SYS_BINTIM('5-NOV-1956 11:05:56',NEW_BTIM)   CT< C  Find user entry in BULLUSER.DAT to update information and0 C  to get the last date that messages were read. Cu  + 	CALL OPEN_FILE_SHARED(4)		! Open user file    	DO WHILE (REC_LOCK(IER))n= 	 READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER) TEMP_USER,,7      &	  NEWEST_BTIM,BBOARD_BTIM,H_NEW_FLAG,H_SET_FLAG,e4      &	  H_BRIEF_FLAG,NOTIFY_FLAG			! Get the header 	END DO   * 	IF (IER.EQ.0) THEN			! Header is present. 	   UNLOCK 4 	   DO WHILE (REC_LOCK(IER1))e? 	      READ (4,FMT=USER_FMT,KEY=USERNAME,IOSTAT=IER1) USERNAME,t5      &	       LOGIN_BTIM,READ_BTIM,NEW_FLAG,SET_FLAG,OA      &	       BRIEF_FLAG,NOTIFY_FLAG		! Find if there is an entryr
 	   END DO6 	   IF (NEW_FLAG(1).LT.143.OR.NEW_FLAG(1).GT.143) THEN: 	      NEW_FLAG(2)=0		! If old version clear GENERIC value0 	      NEW_FLAG(1)=143		! Set new version number
 	   END IFI 	   IF (COMPARE_BTIM(LOGIN_BTIM,NOLOGIN_BTIM).EQ.0) RETURN  ! DISMAIL setA1 	   IF (IER1.EQ.0) THEN			! There is a user entry_4 	      REWRITE (4,FMT=USER_FMT) USERNAME,TODAY_BTIM,H      &	       READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG	! Update login date      &	       ,NOTIFY_FLAG 	      DO I = 1,FLONG # 		 IF (SET_FLAG(I).NE.0) READIT = 1  	      END DOU 	   ELSE; 	      CALL CLEANUP_LOGIN		! Good time to delete dead usersW3 	      READ_BTIM(1) = NEW_BTIM(1)		! Make new entryI! 	      READ_BTIM(2) = NEW_BTIM(2)V 	      DO I = 1,FLONG % 	         SET_FLAG(I) = H_SET_FLAG(I) ) 	         BRIEF_FLAG(I) = H_BRIEF_FLAG(I)S 	      END DOY 	      NEW_FLAG(1) = 143 	      NEW_FLAG(2) = 0+ 	      CALL CHECK_DISMAIL(USERNAME,DISMAIL)$ 	      IF (DISMAIL.EQ.1) THEN @ 	       WRITE (4,FMT=USER_FMT,IOSTAT=IER) USERNAME,NOLOGIN_BTIM,:      &		READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG 	      ELSE > 	       WRITE (4,FMT=USER_FMT,IOSTAT=IER) USERNAME,TODAY_BTIM,:      &		READ_BTIM,NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG 	       DO I = 1,FLONG# 		 IF (SET_FLAG(I).NE.0) READIT = 11 	       END DO 	      END IFT: 	      IF (IER.NE.0) THEN		! Error in writing to user file, 		 WRITE (6,1070)			! Tell user of the error, 		 CALL CLOSE_FILE(4)		! Close the user file 		 CALL EXIT			! Go away...E 	      END IFs9 	      IF (DISMAIL.EQ.1) RETURN		! Go away if DISMAIL set 2 	      DIFF = -1				! Force us to look at messages
 	   END IF 	   DO WHILE (REC_LOCK(IER2))QA 	    READ (4,FMT=USER_FMT,KEY=USER_HEADER,IOSTAT=IER2) TEMP_USER,0:      &	     NEWEST_BTIM,BBOARD_BTIM,H_NEW_FLAG,H_SET_FLAG,A      &	     H_BRIEF_FLAG,NOTIFY_FLAG		! Reset read back to headerN
 	   END DO 	END IF   5 	IF (IER.EQ.0.AND.MINUTE_DIFF(TODAY_BTIM,BBOARD_BTIM) 7      &			.GT.BBOARD_UPDATE) THEN		! Update BBOARD mail?UE 	   REWRITE (4,FMT=USER_FMT) TEMP_USER,NEWEST_BTIM,  ! Rewrite header A      &		TODAY_BTIM,H_NEW_FLAG,H_SET_FLAG,H_BRIEF_FLAG,NOTIFY_FLAG- 	   CALL CLOSE_FILE(4)6 	   IF (.NOT.TEST_BULLCP()) CALL CREATE_BBOARD_PROCESS 	ELSE  	   CALL CLOSE_FILE(4)7 	   IF (IER.NE.0) CALL EXIT	! If no header, no messages  	END IF   9 	IF (IER1.EQ.0) THEN		! Skip date comparison if new entry  C B C  Compare and see if messages have been added since the last time= C  that the user has logged in or used the BULLETIN facility.E CL. 	   DIFF1 = COMPARE_BTIM(LOGIN_BTIM,READ_BTIM)> 	   IF (DIFF1.LT.0) THEN		! If read messages since last login,G 	      LOGIN_BTIM(1) = READ_BTIM(1) ! then use the read date to compare C 	      LOGIN_BTIM(2) = READ_BTIM(2) ! with the latest bulletin date + 	   END IF			! to see if should alert user.S  0 	   DIFF1 = COMPARE_BTIM(LOGIN_BTIM,NEWEST_BTIM) 	END IFT  H 	LOGIN_BTIM_SAVE(1) = LOGIN_BTIM(1) ! These are destroyed in UPDATE_READ# 	LOGIN_BTIM_SAVE(2) = LOGIN_BTIM(2)e 	w 	IF (NEW_FLAG(2).NE.0) THENi6 	   CALL LIB$MOVC3(4,NEW_FLAG(2),%REF(BULL_PARAMETER))4 	   CALL SUBTIME(LOGIN_BTIM,BULL_PARAMETER(1:4),IER) 	ELSE IF (DIFF1.GT.0) THEN 	   BULL_POINT = -1L
 	   RETURN 	END IF,   CI: C  If there are new messages, look for them in BULLDIR.DAT< C  Save all new entries in the GEN_DIR file BULLCHECK.SCR so5 C  that we can close BULLDIR.DAT as soon as possible.D C(   	ENTRY LOGIN_FOLDER   ) 	IF (NEW_FLAG(2).EQ.0.OR.FOLDER_SET) THEN!& 	   LOGIN_BTIM(1) = LOGIN_BTIM_SAVE(1)& 	   LOGIN_BTIM(2) = LOGIN_BTIM_SAVE(2) 	END IF    	ENTRY SHOW_SYSTEM  ( 	NGEN = 0			! Number of general messages' 	NSYS = 0			! Number of system messages( 	BULL_POINT = -1  - 	IF (IER1.NE.0.AND.FOLDER_NUMBER.GT.0) RETURN': 		! Don't overwhelm new user with lots of non-general msgs  2 	CALL OPEN_FILE_SHARED(2)	! Get bulletin directory' 	CALL READDIR(0,IER)		! Get header infoE& 	CALL INIT_QUEUE(GEN_DIR1,BULLDIR_COM)& 	CALL INIT_QUEUE(SYS_DIR1,BULLDIR_COM) 	GEN_DIR = GEN_DIR18 	SYS_DIR = SYS_DIR1 
 	START = 1 	REVERSE = 0  	IF (CLI$PRESENT('REVERSE').AND./      &		(.NOT.TEST2(SET_FLAG,FOLDER_NUMBER).OR. 3      &		.NOT.TEST2(BRIEF_FLAG,FOLDER_NUMBER))) THENE 	   REVERSE = 1T 	   START = NBULL + 1E 	   IER = START + 1T 	   DIFF = 0 	   IF (IER1.NE.0) THEN  	      START = 1 	   ELSE- 	     DO WHILE (START+1.EQ.IER.AND.DIFF.LE.0)  	      START = START - 1. 	      IF (START.GT.0) CALL READDIR(START,IER) 	      IF (START+1.EQ.IER) THENd, 		 CALL SYS_BINTIM(DATE//' '//TIME,DIR_BTIM)2 	         DIFF = COMPARE_BTIM(LOGIN_BTIM,DIR_BTIM) 	      END IFi 	     END DO 	     START = START + 1W
 	   END IF 	END IF  	DO ICOUNT1 = NBULL,START,-1 	   IF (REVERSE) THENh' 	      ICOUNT = NBULL + START - ICOUNT1L 	   ELSE 	      ICOUNT = ICOUNT1_
 	   END IF 	   CALL READDIR(ICOUNT,IER)5 	   IF (IER1.EQ.0) THEN ! Is this a totally new user? 1 				  ! No. Is bulletin system or from same user?  	      IF (.NOT.REVERSE) THEN , 		 CALL SYS_BINTIM(DATE//' '//TIME,DIR_BTIM)H 	         DIFF = COMPARE_BTIM(LOGIN_BTIM,DIR_BTIM) ! No, so compare date" 	         IF (DIFF.GT.0) GO TO 100 	      END IFA+ 	      IF (USERNAME.NE.FROM.OR.SYSTEM) THENN. 		 IF (SYSTEM) THEN		! Is it system bulletin?  		    NSYS = NSYS + 1P9 		    CALL WRITE_QUEUE(%VAL(SYS_DIR),SYS_DIR,BULLDIR_COM)R3 	         ELSE IF (.NOT.CLI$PRESENT('SYSTEM')) THENI0 		    IF (.NOT.REVERSE.OR.BULL_POINT.EQ.-1) THEN  		       BULL_POINT = ICOUNT - 11 		       IF (TEST2(BRIEF_FLAG,FOLDER_NUMBER).AND. 4      &			   TEST2(SET_FLAG,FOLDER_NUMBER)) GO TO 100 		    END IF 		    NGEN = NGEN + 1E 		    SYSTEM = ICOUNT 9 		    CALL WRITE_QUEUE(%VAL(GEN_DIR),GEN_DIR,BULLDIR_COM): 	         END IF 	      END IF = 	   ELSE		! Totally new user, save only Permanent system msgso 	      IF (SYSTEM.EQ.3) THEN 	         NSYS = NSYS + 1N6 		 CALL WRITE_QUEUE(%VAL(SYS_DIR),SYS_DIR,BULLDIR_COM)H 	      ELSE IF (NGEN.EQ.0) THEN	! And save only the first non-system msg5 		 SYSTEM = ICOUNT	! Save bulletin number for displayr- 		 IF (.NOT.REVERSE.OR.BULL_POINT.EQ.-1) THENh 		    BULL_POINT = ICOUNT - 10. 		    IF (TEST2(BRIEF_FLAG,FOLDER_NUMBER).AND.2      &		 	TEST2(SET_FLAG,FOLDER_NUMBER)) GO TO 100	 		 END IF0 		 NGEN = NGEN + 16 		 CALL WRITE_QUEUE(%VAL(GEN_DIR),GEN_DIR,BULLDIR_COM) 	      END IFF
 	   END IF 	END DOA 100	CALL CLOSE_FILE(2) 	IF (FOLDER_SET) NSYS = 0S CO? C  Review new directory entries.  If there are system messages,oH C  copy the system bulletin into GEN_DIR file BULLSYS.SCR for outputtingB C  to the terminal.  If there are simple messages, just output the C  header information. CL$ 	IF (NGEN.EQ.0.AND.NSYS.EQ.0) RETURN  6 	IF (NSYS.GT.0) THEN		! Are there any system messages?+ 	   CALL CLI$GET_VALUE('SEPARATE',SEPARATE)T 	   IF (FIRST_WRITE) THEND5 	      PAGE = 4		! Don't erase MAIL/PASSWORD notifiesD@ 	      FIRST_WRITE = .FALSE.	! if this is first write to screen.
 	   END IF" 	   WRITE (6,1026) CTRL_G	! Yep... 	   PAGE = PAGE + 1d6 	   CTRL_G = 0		! Don't ring bell for non-system bulls 	   CALL OPEN_FILE_SHARED(1)# 	   CALL INIT_QUEUE(SYS_BUL1,INPUT)U 	   SYS_BUL = SYS_BUL1 	   SYS_DIR = SYS_DIR1 	   DO J=1,NSYSF9 	      CALL READ_QUEUE(%VAL(SYS_DIR),SYS_DIR,BULLDIR_COM)L  	      INPUT = ' 'A4 	      CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT) 	      LEN = 81IE 	      DO I=BLOCK,BLOCK+LENGTH-1	! Copy bulletin to SYS_BUL link listM 		 DO WHILE (LEN.GT.0)  		    CALL GET_BULL(I,INPUT,LEN) 		    IF (LEN.LT.0) THEN 		       CALL CLOSE_FILE(1)I 		       RETURN  		    ELSE IF (LEN.GT.0) THENp6 		       CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT) 		    END IF	 		 END DOL 		 LEN = 80H 	      END DO . 	      IF (J.LT.NSYS.AND.SEPARATE.NE.' ') THEN  	         INPUT = ' '7 	         CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)O 		 DO I=1,80 		    INPUT(I:I) = SEPARATEH	 		 END DO,0 		 CALL WRITE_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT) 	      END IF	
 	   END DO 	   CALL CLOSE_FILE(1) 	   SYS_BUL = SYS_BUL1; 	   DO WHILE (SYS_BUL.NE.0)	! Write out the system messages 3 	      CALL READ_QUEUE(%VAL(SYS_BUL),SYS_BUL,INPUT)A 	      IF (SYS_BUL.NE.0) THENE- 		 IF (PAGE.EQ.PAGE_LENGTH-2.AND.PAGING) THENr 							! If at end of screen; 		    WRITE(6,1080)	! Ask for input to proceed to next page	> 		    CALL GET_INPUT_NOECHO_PROMPT(INREAD,! Get terminal input)      &			'HIT any key for next page....')(9 	            CALL LIB$ERASE_PAGE(1,1)		! Clear the screenI 		    PAGE = 1- 		    WRITE(6,1060) '+'//INPUT(1:TRIM(INPUT))  		 ELSEE 		    PAGE = PAGE + 1A- 		    WRITE(6,1060) ' '//INPUT(1:TRIM(INPUT))G	 		 END IFR 	      END IFt
 	   END DO 	   IF (NGEN.EQ.0) THENL4 	      WRITE(6,'(A)')		! Write delimiting blank line
 	   END IF 	   PAGE = PAGE + 1L 	END IF    	ENTRY REDISPLAY_DIRECTORY   	GEN_DIR = GEN_DIR1W: 	IF (NGEN.GT.0) THEN		! Are there new non-system messages? 	   LENF = TRIM(FOLDER)M 	   S1 = (80-13-LENF)/2N 	   S2 = 80-S1-13-LENF6 	   IF (PAGE+5+NGEN.GT.PAGE_LENGTH.AND.PAGE.GT.0) THEN= 	      WRITE(6,1080)		! Ask for input to proceed to next pageA@ 	      CALL GET_INPUT_NOECHO_PROMPT(INREAD,	! Get terminal input)      &			'HIT any key for next page....')U2 	      CALL LIB$ERASE_PAGE(1,1)	! Clear the screen? 	      WRITE(6,1028) 'New '//FOLDER(1:LENF)//' messages',CTRL_G  	      PAGE = 1F 	   ELSE 	      IF (FIRST_WRITE) THEN3 		 PAGE = 4		  ! Don't erase MAIL/PASSWORD notifiesNC 	         FIRST_WRITE = .FALSE. ! if this is first write to screen.  	      END IF ? 	      WRITE(6,1027) 'New '//FOLDER(1:LENF)//' messages',CTRL_G  	      PAGE = PAGE + 1
 	   END IF 	   WRITE(6,1020)r 	   WRITE(6,1025)I 	   PAGE = PAGE + 2e	 	   I = 0C 	   DO WHILE (I.LT.NGEN) 	      I = I + 19 	      CALL READ_QUEUE(%VAL(GEN_DIR),GEN_DIR,BULLDIR_COM) G 	      IF (PAGE.EQ.PAGE_LENGTH-2.AND.PAGING) THEN ! If at end of screenE8 		 WRITE(6,1080)	! Ask for input to proceed to next page' 		 CALL GET_INPUT_NOECHO_PROMPT(INREAD, A      &		'HIT Q(Quit listing) or any other key for next page....')_( 	         CALL STR$UPCASE(INREAD,INREAD)6 	         CALL LIB$ERASE_PAGE(1,1)		! Clear the screen 		 PAGE = 1M 		 IF (INREAD.EQ.'Q') THEN( 		    I = NGEN		! Quit directory listing3 		    WRITE(6,'(''+Quitting directory listing.'')')M 		 ELSEi5 		    WRITE(6,1040) '+'//DESCRIP,FROM,DATE(:6),SYSTEMB	 		 END IFO* 					! Bulletin number is stored in SYSTEM 	      ELSEL 		 PAGE = PAGE + 12 		 WRITE(6,1040) ' '//DESCRIP,FROM,DATE(:6),SYSTEM 	      END IFL
 	   END DOA 	   IF ((.NOT.FOLDER_SET.AND.BTEST(SET_FLAG(1),0).AND.DIFF1.LE.0)t@      &		.OR.(FOLDER_SET.AND.TEST2(SET_FLAG,FOLDER_NUMBER))) THEN> 	      PAGE = 0	! Don't reset page counter if READNEW not set,/ 	   END IF	! as no prompt to read is generated.  	END IFT0 	IF (COMPARE_BTIM(READ_BTIM,NEW_BTIM).NE.0) THEN 	   WRITE(6,1030)s 	ELSE IF (NGEN.EQ.0) THENL, 	   LEN = 57 + INDEX(COMMAND_PROMPT,'>') - 1 	   S1 = (80-LEN)/2  	   S2 = 80 - S1 - LEN3 	   WRITE(6,1035) 'The '//COMMAND_PROMPT(:LEN-57)//F?      &		'/SYSTEM command can be used to reread these messages.'G 	ELSE,, 	   LEN = 48 + INDEX(COMMAND_PROMPT,'>') - 1 	   S1 = (80-LEN)/2s 	   S2 = 80 - S1 - LEN3 	   WRITE(6,1035) 'The '//COMMAND_PROMPT(:LEN-48)// 7      &			' command can be used to read these messages.') 	END IFL   	RETURN)  < 1020	FORMAT(' Description',43X,'From',9X,'Date',3X,'Number')< 1025	FORMAT(' -----------',43X,'----',9X,'----',3X,'------')5 1026	FORMAT(' ',33('*'),'System Messages',32('*'),A1)B+ 1027	FORMAT(/,' ',<S1>('*'),A,<S2>('*'),A1)N) 1028	FORMAT('+',<S1>('*'),A,<S2>('*'),A1)c 1030	FORMAT(' ',80('*'))& 1035	FORMAT(' ',<S1>('*'),A,<S2>('*'))# 1040	FORMAT(A54,1X,A12,1X,A6,1X,I4)E 1060	FORMAT(A)9 1070	FORMAT(' ERROR: Cannot add new entry to user file.')_ 1080	FORMAT(' ',/)   	END     	SUBROUTINE GET_NODE_INFO	 Cu C  SUBROUTINE GET_NODE_INFOY C = C  FUNCTION: Gets local node name and obtains node names from. C	command line.U CR   	IMPLICIT INTEGER (A-Z)v   	EXTERNAL CLI$_ABSENTs  4 	COMMON /NODE_INFO/ NODES,LOCAL_NODE_FOUND,NODE_NUM,      &				NODE_ERROR,POINT_NODE( 	CHARACTER*32 NODES(10)E$ 	LOGICAL LOCAL_NODE_FOUND,NODE_ERROR  & 	CHARACTER LOCAL_NODE*32,NODE_TEMP*256   	NODE_ERROR = .FALSE.S   	LOCAL_NODE_FOUND = .FALSE. 2 	CALL LIB$SYS_TRNLOG('SYS$NODE',L_NODE,LOCAL_NODE)$ 	L_NODE = L_NODE - 2			! Remove '::'! 	IF (LOCAL_NODE(1:1).EQ.'_') THENA 	   LOCAL_NODE = LOCAL_NODE(2:)E 	   L_NODE = L_NODE - 1L 	END IF   - 	NODE_NUM = 0				! Initialize number of nodes.: 	IF (CLI$PRESENT('NODES')) THEN		! Decnet nodes specified?+ 	   LEN = 0				! GET_VALUE crashes if LEN<0T. 	   DO WHILE (CLI$GET_VALUE('NODES',NODE_TEMP)<      &	    .NE.%LOC(CLI$_ABSENT))		! Get the specified nodes* 	    IER = SYS_TRNLNM(NODE_TEMP,NODE_TEMP)$ 	    DO WHILE (TRIM(NODE_TEMP).GT.0) 	      NODE_NUM = NODE_NUM + 1# 	      COMMA = INDEX(NODE_TEMP,',')  	      IF (COMMA.GT.0) THEN1) 		 NODES(NODE_NUM) = NODE_TEMP(1:COMMA-1)B" 		 NODE_TEMP = NODE_TEMP(COMMA+1:) 	      ELSE  		 NODES(NODE_NUM) = NODE_TEMP 		 NODE_TEMP = ' ' 	      END IF.# 	      NLEN = TRIM(NODES(NODE_NUM)) B 	      IF (INDEX(NODES(NODE_NUM),'::').GT.0) THEN   ! Remove :: if5 		 NLEN = INDEX(NODES(NODE_NUM),'::') - 1	   ! addeddR 	      END IF @ 	      IF (LOCAL_NODE(1:L_NODE).EQ.NODES(NODE_NUM)(1:NLEN)) THEN 	       NODE_NUM = NODE_NUM - 10! 	       LOCAL_NODE_FOUND = .TRUE.M 	      ELSES 	       POINT_NODE = NODE_NUMEB 	       OPEN (UNIT=9+NODE_NUM,NAME=NODES(NODE_NUM)(1:NLEN)//'""::'G      &	       //'"TASK=BULLETIN"',ACCESS='SEQUENTIAL',FORM='FORMATTED',$;      &	       CARRIAGECONTROL='NONE',TYPE='NEW',IOSTAT=IER)T 	       IF (IER.NE.0) THEN 		  DO WHILE (NODE_NUM.GT.0) 		     CLOSE(UNIT=9+NODE_NUM)E 		     NODE_NUM = NODE_NUM - 1
 		  END DO 		  NODE_ERROR = .TRUE. 
 		  RETURN 	       END IF 	      END IFM 	    END DO	
 	   END DO 	ELSEE 	   LOCAL_NODE_FOUND = .TRUE.C 	END IF    	RETURNF 	END     	SUBROUTINE DELETE_NODEl Ce C  SUBROUTINE DELETE_NODE  Ct> C  FUNCTION: Deletes files sent via ADD/NODES at remote hosts. C    	IMPLICIT INTEGER (A-Z)S   	INCLUDE 'BULLUSER.INC'    	INCLUDE 'BULLDIR.INC'  4 	COMMON /NODE_INFO/ NODES,LOCAL_NODE_FOUND,NODE_NUM,      &				NODE_ERROR,POINT_NODE  	CHARACTER*32 NODES(10).$ 	LOGICAL LOCAL_NODE_FOUND,NODE_ERROR  4     	CHARACTER PASSWORD*31,INLINE*80,DEFAULT_USER*12   	CALL GET_NODE_INFO     	IF (NODE_ERROR) GO TO 940U  , 	IF (NODE_NUM.EQ.0.OR.LOCAL_NODE_FOUND) THEN8 	   WRITE (6,'('' ERROR: Cannot specify local node.'')') 	   GO TO 999  	END IFF  - 	IER = CLI$GET_VALUE('USERNAME',DEFAULT_USER)E& 	IF (.NOT.IER) DEFAULT_USER = USERNAME' 	IER = CLI$GET_VALUE('SUBJECT',DESCRIP)o  : 	DO POINT_NODE=1,NODE_NUM	   	! Write out command to nodesH 	   SEMI = INDEX(NODES(POINT_NODE),'::')	! Look for semicolon after node8 	   NLEN = TRIM(NODES(POINT_NODE))	! Length of node name1 	   IF (SEMI.GT.0) THEN			! Is semicolon present? ? 	      IF (NLEN.GT.SEMI+1) THEN		! Yes, is username after node?TD 	         TEMP_USER = NODES(POINT_NODE)(SEMI+2:)	! Yes, set username- 	         NLEN = SEMI - 1		! Remove semicoloni+ 	      ELSE				! No username after nodename 5 		 TEMP_USER = DEFAULT_USER	! Set username to defaultd- 	         NLEN = SEMI - 1		! Remove semicolont$ 		 SEMI = 0			! Indicate no username 	      END IF_# 	   ELSE					! No semicolon present : 	      TEMP_USER = DEFAULT_USER		! Set username to default
 	   END IF: 	   INLINE = 'DELETE/SUBJECT="'//DESCRIP(:TRIM(DESCRIP))//6      &      '"/USERNAME='//TEMP_USER(:TRIM(TEMP_USER))E 	   IF (CLI$PRESENT('USERNAME').OR.SEMI.GT.0) THEN  ! If username wasY2 	      IER = 1				! specified, prompt for password 	      DO WHILE (IER.NE.0)7 	         WRITE(6,'('' Enter password for node '',2A)') #      &			NODES(POINT_NODE),CHAR(10)T) 	         CALL GET_INPUT_NOECHO(PASSWORD)L, 	         IF (TRIM(PASSWORD).EQ.0) GO TO 910> 	         OPEN (UNIT=10+NODE_NUM,NAME=NODES(POINT_NODE)(:NLEN)5      &		   //'"'//TEMP_USER(1:TRIM(TEMP_USER))//' '//E-      &		   PASSWORD(1:TRIM(PASSWORD))//'"::',,%      &		   TYPE='SCRATCH',IOSTAT=IER)D" 	         CLOSE (UNIT=10+NODE_NUM) 	         IF (IER.NE.0) THEN4 		    WRITE (6,'('' ERROR: Password is invalid.'')') 	         END IF 	      END DO.
 	   END IF- 	   WRITE (POINT_NODE+9,'(A)',ERR=940) INLINEE4 	   READ (POINT_NODE+9,'(A)',ERR=940,END=940) INLINE 	   IF (INLINE.EQ.'END') THEN_C 	      WRITE (6,'('' Message successfully deleted from node '',A)')(      &				NODES(POINT_NODE)o 	   ELSEA 	      WRITE (6,'('' Error while deleting message to node '',A)')i      &				NODES(POINT_NODE)o 	      WRITE (6,'(A)') INLINE 
 	   END IF 	END DO1  
 	GO TO 999   910	WRITE (6,1010)
 	GO TO 999  $ 940	WRITE (6,1015) NODES(POINT_NODE)   999	DO WHILE (NODE_NUM.GT.0) 	   CLOSE(UNIT=9+NODE_NUM) 	   NODE_NUM = NODE_NUM - 1D 	END DO    	RETURN   ) 1010	FORMAT (' ERROR: Deletion aborted.')W/ 1015	FORMAT (' ERROR: Unable to reach node ',A)    	END        2 	SUBROUTINE SET_FOLDER_FLAG(SETTING,FLAG,FLAGNAME) C  C  SUBROUTINE SET_FOLDER_FLAG. C05 C  FUNCTION: Sets or clears specified flag for folder= CI 	IMPLICIT INTEGER (A-Z)1   	INCLUDE 'BULLFOLDER.INC'-   	INCLUDE 'BULLUSER.INC'T   	INCLUDE 'BULLFILES.INC'   	CHARACTER*(*) FLAGNAME	  4 	IF (FOLDER_OWNER.EQ.USERNAME.OR.SETPRV_PRIV()) THEN) 	   CALL OPEN_FILE(7)		! Open folder filel  9 	   READ (7,FMT=FOLDER_FMT,KEY=FOLDER,KEYID=0,IOSTAT=IER)L8      &		FOLDER,FOLDER_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP<      &		,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB9      &		,F_NBULL,F_NEWEST_BTIM,FOLDER_FLAG,F_EXPIRE_LIMIT    	   IF (SETTING) THENO, 	      FOLDER_FLAG = IBSET(FOLDER_FLAG,FLAG) 	   ELSE, 	      FOLDER_FLAG = IBCLR(FOLDER_FLAG,FLAG)
 	   END IF  ) 	   REWRITE (7,FMT=FOLDER_FMT,IOSTAT=IER)R8      &		FOLDER,FOLDER_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP<      &		,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB9      &		,F_NBULL,F_NEWEST_BTIM,FOLDER_FLAG,F_EXPIRE_LIMITE   	   CALL CLOSE_FILE(7)  9 	   WRITE (6,'(1X,A,'' has been modified for folder.'')')E      &		FLAGNAME 	ELSE = 	   WRITE (6,'(1X,A,'' You are not authorized to modify.'')')L      &		FLAGNAME 	END IFN   	RETURN  	END        * 	SUBROUTINE SET_FOLDER_EXPIRE_LIMIT(LIMIT) C_% C  SUBROUTINE SET_FOLDER_EXPIRE_LIMITA C * C  FUNCTION: Sets folder expiration limit. C  	IMPLICIT INTEGER (A-Z)I   	INCLUDE 'BULLFOLDER.INC'    	INCLUDE 'BULLUSER.INC'l   	INCLUDE 'BULLFILES.INC'   	IF (LIMIT.LT.0) THEN.B 	   WRITE (6,'('' ERROR: Invalid expiration length specified.'')')9 	ELSE IF (FOLDER_OWNER.EQ.USERNAME.OR.SETPRV_PRIV()) THENM) 	   CALL OPEN_FILE(7)		! Open folder fileE  9 	   READ (7,FMT=FOLDER_FMT,KEY=FOLDER,KEYID=0,IOSTAT=IER) 8      &		FOLDER,FOLDER_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP<      &		,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB9      &		,F_NBULL,F_NEWEST_BTIM,FOLDER_FLAG,F_EXPIRE_LIMITc   	   F_EXPIRE_LIMIT = LIMIT  ) 	   REWRITE (7,FMT=FOLDER_FMT,IOSTAT=IER) 8      &		FOLDER,FOLDER_NUMBER,FOLDER_OWNER,FOLDER_DESCRIP<      &		,FOLDER_BBOARD,FOLDER_BBEXPIRE,USERB,GROUPB,ACCOUNTB9      &		,F_NBULL,F_NEWEST_BTIM,FOLDER_FLAG,F_EXPIRE_LIMIT    	   CALL CLOSE_FILE(7)7 	   WRITE (6,'('' Folder expiration date modified.'')')	 	ELSEM< 	   WRITE (6,'('' You are not allowed to modify folder.'')') 	END IF+   	RETURNA 	END