!
! The parameter FOLDER_MAX should be changed to increase the maximum number
! of folders available.  Due to storage via longwords, the maximum number
! available is always a multiple of 32.  Thus, it will probably make sense
! to specify a multiple of 32 for FOLDER_MAX, as that it what really will be
! the capacity.  Note that the default general folder counts as a folder also,
! so that if you specify 64, you will be able to create 63 folders on your own.
!
        PARAMETER FOLDER_MAX = 96
        PARAMETER FLONG = (FOLDER_MAX + 31)/ 32

        PARAMETER USER_RECORD_LENGTH = 28 + FLONG*16
        PARAMETER USER_FMT = '(A12,<4+FLONG*4>A4)'
        PARAMETER USER_HEADER_KEY = '            '

        COMMON /HEADER_INFO/ TEMP_USER,BBOARD_BTIM,NEWEST_BTIM,USERPRIV
        COMMON /HEADER_INFO/ SET_FLAG_DEF,BRIEF_FLAG_DEF
        COMMON /HEADER_INFO/ NOTIFY_FLAG_DEF
        CHARACTER TEMP_USER*12
        DIMENSION BBOARD_BTIM(2),NEWEST_BTIM(2),USERPRIV(FLONG)
        DIMENSION SET_FLAG_DEF(FLONG),BRIEF_FLAG_DEF(FLONG)
        DIMENSION NOTIFY_FLAG_DEF(FLONG)

        COMMON /BULL_USER/ USERNAME,LOGIN_BTIM,READ_BTIM,
     &          NEW_FLAG,SET_FLAG,BRIEF_FLAG,NOTIFY_FLAG
        CHARACTER*12 USERNAME
        DIMENSION LOGIN_BTIM(2),READ_BTIM(2)
        DIMENSION NEW_FLAG(FLONG)   ! Bit set indicates new message in folder
        DIMENSION SET_FLAG(FLONG)   ! Bit set indicates READNEW set for folder
        DIMENSION BRIEF_FLAG(FLONG) ! Bit set indicates READNEW/BRIEF set
        DIMENSION NOTIFY_FLAG(FLONG)! Bit set indicates to broadcast
                                    ! notification when new bulletin is added.

        CHARACTER*(USER_RECORD_LENGTH) USER_ENTRY,USER_HEADER
        EQUIVALENCE (USER_ENTRY,USERNAME)
        EQUIVALENCE (USER_HEADER,TEMP_USER)

        COMMON /FOLDER_TIMES/ LAST_READ_BTIM(2,0:FOLDER_MAX)
           ! Last read times for each folder as stored in BULL_DIR:BULLINF.DAT

        COMMON /NEW_MESSAGES/ NEW_MSG
        DIMENSION NEW_MSG(FLONG)   ! Flag showing new messages detected
