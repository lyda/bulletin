C
C  THE FIRST 2 FILES ARE FILES CREATED AND USED BY BULLETIN.
C  SPECIFY THE DEVICE/DIRECTORY IN WHICH YOU DESIRE THAT THEY BE KEPT.
C
C  FOLDER_DIRECTORY IS THE DIRECTORY THAT FILES FOR FOLDERS THAT
C  ARE CREATED ARE KEPT IN.  IF YOU WISH TO PREVENT FOLDER CREATION,
C  YOU SHOULD MODIFY BULLCOM.CLD TO MAKE THE CREATE COMMAND A PRIVILEGED
C  COMMAND (OR SIMPLY REMOVE THE LINES WHICH DEFINE THE CREATE COMMAND).
C
C  BBOARD_DIRECTORY IS THE SCRATCH AREA USED BY BBOARD WHEN EXTRACTING
C  MAIL.  IF IT IS UNDEFINED, BBOARD WILL NOT BE ABLE TO BE USED.
C  NOTE THAT EITHER THE BBOARD ACCOUNT MUST HAVE ACCESS TO THIS DIRECTORY,
C  OR THE BBOARD ACCOUNTS MUST BE GIVEN SYSPRV PRIVILEGES TO BE ABLE
C  TO WRITE INTO THIS DIRECTORY.  ALSO, FOR BBOARD TO WORK, MAKE SURE
C  THAT THE SUBPROCESS LIMIT FOR USERS IS AT LEAST 2.  YOU ALSO MAY HAVE
C  TO INCREASE SOME SUBPROCESS SYSTEM PARAMETERS: PQL_DPGFLQUOTA AND
C  PQL_DWSQUOTA MAY HAVE TO BE CHANGED. (10000 AND 500 ARE TYPICAL).
C  (NOTE: ACCESS CAN BE GIVEN TO THE DIRECTORY FOR THE BBOARD ACCOUNT USING
C  ACLS, I.E. " SET ACL/ACL=(ID=bboard,ACCESS=R+W)/OBJ=FILE directory.DIR")
C
	COMMON /FILES/ BULLDIR_FILE,BULLETIN_FILE,BULLUSER_FILE
	COMMON /FILES/ BULLFOLDER_FILE,FOLDER_DIRECTORY,BBOARD_DIRECTORY
	COMMON /FILES/ BULLINF_FILE
	CHARACTER*80 BULLUSER_FILE /'BULL_DIR:BULLUSER.DAT'/
	CHARACTER*80 BULLFOLDER_FILE /'BULL_DIR:BULLFOLDER.DAT'/
	CHARACTER*80 BULLINF_FILE /'BULL_DIR:BULLINF.DAT'/
	CHARACTER*80 FOLDER_DIRECTORY /'BULL_DIR:'/
	CHARACTER*80 BBOARD_DIRECTORY /'BULL_DIR:'/
C
C  THE FOLLOWING 2 FILES ARE OBSOLETE AS OF V1.1 AND NO LONGER HAVE TO
C  BE SPECIFIED.  BULLETIN NOW TREATS THE GENERAL FOLDER AS ANY OTHER
C  FOLDER.  NEW USERS SHOULD JUST LEAVE THEM ALONE.  HOWEVER, USERS
C  USING OLDER VERSIONS STILL HAVE TO SPECIFY THEM IN ORDER THAT
C  BULLETIN KNOWS THE NAMES IN ORDER TO RENAME THEM.
C
	CHARACTER*80 BULLDIR_FILE /'BULL_DIR:BULLDIR.DAT'/
	CHARACTER*80 BULLETIN_FILE /'BULL_DIR:BULLETIN.DAT'/
