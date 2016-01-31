!
!  The following 2 parameters can be modified if desired before compilation.
!
        PARAMETER BBEXPIRE_LIMIT = 30   ! Maxmimum time limit in days that
                                        ! BBOARDS can be set to.
        PARAMETER BBOARD_UPDATE = 15    ! Number of minutes between checks
                                        ! for new BBOARD mail. (Note: Check
                                        ! only occurs via BULLETIN/LOGIN.
                                        ! Check is forced via BULLETIN/BBOARD).
                                        ! NOT APPLICABLE IF BULLCP IS RUNNING.
        PARAMETER ADDID = .TRUE.        ! Allows users who are not in the
                                        ! rights data base to be added
                                        ! according to uic number.

        PARAMETER FOLDER_FMT = '(A25,A4,A12,A80,A12,3A4,A8,7A4)'
        PARAMETER FOLDER_RECORD = 184   ! Must be multiple of 4

        COMMON /BULL_FOLDER/ FOLDER,FOLDER_NUMBER,FOLDER_OWNER,
     &          FOLDER_DESCRIP,FOLDER_BBOARD,FOLDER_BBEXPIRE,
     &          USERB,GROUPB,ACCOUNTB,
     &          F_NBULL,F_NEWEST_BTIM,FOLDER_FLAG,F_EXPIRE_LIMIT,
     &          F_NEWEST_NOSYS_BTIM,FILLER,
     &          FOLDER_FILE,FOLDER_SET
        INTEGER F_NEWEST_BTIM(2)
        INTEGER F_NEWEST_NOSYS_BTIM(2)
        LOGICAL FOLDER_SET
        DATA FOLDER_SET /.FALSE./, FOLDER/'GENERAL'/
        CHARACTER FOLDER_OWNER*12,FOLDER*25,ACCOUNTB*8
        CHARACTER FOLDER_FILE*80,FOLDER_DESCRIP*80,FOLDER_BBOARD*12

        CHARACTER*(FOLDER_RECORD) FOLDER_COM
        EQUIVALENCE (FOLDER,FOLDER_COM)

        COMMON /BULL_FOLDER1/ FOLDER1,FOLDER1_NUMBER,FOLDER1_OWNER,
     &          FOLDER1_DESCRIP,FOLDER1_BBOARD,FOLDER1_BBEXPIRE,
     &          USERB1,GROUPB1,ACCOUNTB1,
     &          F1_NBULL,F1_NEWEST_BTIM,FOLDER1_FLAG,F1_EXPIRE_LIMIT,
     &          F1_NEWEST_NOSYS_BTIM,FILLER1,
     &          FOLDER1_FILE
        CHARACTER FOLDER1_OWNER*12,FOLDER1*25,ACCOUNTB1*8
        CHARACTER FOLDER1_FILE*80,FOLDER1_DESCRIP*80,FOLDER1_BBOARD*12
        INTEGER F1_NEWEST_BTIM(2)
        INTEGER F1_NEWEST_NOSYS_BTIM(2)

        CHARACTER*(FOLDER_RECORD) FOLDER1_COM
        EQUIVALENCE (FOLDER1,FOLDER1_COM)
