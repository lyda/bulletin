	IMPLICIT INTEGER (A-Z)

	INCLUDE 'BULLFOLDER.INC'

	INCLUDE 'BULLFILES.INC'

	OPEN (UNIT=5,FILE='BULLNEWS.DAT',STATUS='NEW',
     &	        ACCESS='KEYED',RECORDTYPE='FIXED',
     &	        RECORDSIZE=NEWS_FOLDER_RECORD/4,
     &	        ORGANIZATION='INDEXED',IOSTAT=IER,
     &	        KEY=(1:44:CHARACTER,45:48:INTEGER,49:56:CHARACTER,
     &          57:64:CHARACTER:DESCENDING))
	BULLNEWS_FILE  = 'BULL_DIR:BULLNEWS.DAT'
	CALL OPEN_BULLNEWS_SHARED       ! Open folder file

	DO WHILE (IER.EQ.0)
		CALL READ_FOLDER_FILE_TEMP(IER)
              	IF (IER.EQ.0) WRITE (5,IOSTAT=IER) NEWS_FOLDER1_COM
	END DO

	TYPE *,FOLDER1
	END
