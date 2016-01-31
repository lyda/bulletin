$!
$! BOARD_SPECIAL.COM
$!
$! Command file invoked by folder associated with a BBOARD which is
$! is specified with /SPECIAL.  This can be used to convert data to
$! a message via a different means than the VMS mail.  This is done by
$! converting the data to look like output created by the MAIL utility,
$! which appears as follows:
$!
$!	First line is 0 length line.
$!	Second line is "From:" followed by TAB followed by incoming username
$!	Third line is "To:" followed by TAB followed by BBOARD username
$!	Fourth line is "Subj:" followed by TAB followed by subject
$!	The message text then follows.
$!	Message is ended by a line containing a FORM FEED.
$!
$! This command file should be put in the BBOARD_DIRECTORY as specified
$! in BULLFILES.INC.  You can also have several different types of special
$! procedures.  To accomplish this, rename the file to the BBOARD username.
$! i.e. if you specify SET BBOARD FOO/SPECIAL, you could name the file
$! FOO.COM and it will execute that rather than BOARD_SPECIAL.COM.
$!
$! The following routine is the one we use to convert mail from a non-DEC
$! mail network.  The output from this mail is written into a file which
$! is slightly different from the type outputted by MAIL.
$!
$! (NOTE: A username in the SET BBOARD command need only be specified if
$! the process which reads the mail requires that the process be owned by
$! a specific user, which is the case for this sample, and for that matter
$! when reading VMS MAIL.  If this is not required, you do not have to
$! specify a username.)
$!
$ USERNAME := 'F$GETJPI("","USERNAME")'		! This trims trailing spaces
$ IF F$SEARCH("MFE_TELL_FILES:"+USERNAME+".MAI") .EQS. "" THEN EXIT
$ SET DEFAULT BULL_DIR:	! BULLETIN looks for text in BBOARD directory
$ SET PROTECT=(W:RWED)/DEFAULT
$ IF F$SEARCH("MFEMSG.MAI") .NES. "" THEN -
  DELETE MFEMSG.MAI;*		! Delete any leftover output files.
$ MSG := $MFE_TELL: MESSAGE
$ DEFINE/USER SYS$COMMAND SYS$INPUT
$ MSG				! Read MFENET mail
copy * MFEMSG
delete *
exit
$ FF[0,8] = 12			! Define a form feed character
$ OPEN/READ/ERROR=EXIT INPUT MFEMSG.MAI
$ OUTNAME = USERNAME+".TXT"	! Output file will be 'USERNAME'.TXT
$ OPEN/WRITE OUTPUT 'OUTNAME'
$ READ/END=END INPUT DATA		! Skip first line in MSG output
$HEADER:
$ FROM = ""
$ SUBJ = ""
$ MFEMAIL = "T"
$NEXTHEADER:
$ IF (FROM.NES."") .AND. (SUBJ.NES."") THEN GOTO SKIPHEADER
$ READ/END=END INPUT DATA		! Read header line in MSG output
$ IF DATA .EQS. "" THEN GOTO SKIPHEADER	! Missing From or Subj ??
$ IF FROM .NES. "" THEN GOTO SKIPFROM
$ IF F$LOCATE("From: ",DATA) .NES. 0 THEN GOTO 10$
$ MFEMAIL = "F"
$ FROM= F$EXTRACT(6,F$LENGTH(DATA),DATA)
$ GOTO NEXTHEADER
$10$:
$ IF F$LOCATE("Reply-to: ",DATA) .NES. 0 THEN GOTO 20$
$ MFEMAIL = "F"
$ FROM= F$EXTRACT(10,F$LENGTH(DATA),DATA)
$ GOTO NEXTHEADER
$20$:
$ IF F$LOCATE("From ",DATA) .NES. 0 THEN GOTO SKIPFROM
$ FROM= F$EXTRACT(5,F$LENGTH(DATA),DATA)
$ GOTO NEXTHEADER
$SKIPFROM:
$ IF SUBJ .NES. "" THEN GOTO SKIPSUBJ
$ IF F$LOCATE("Subject",DATA) .NES. 0 THEN GOTO SKIPSUBJ
$ SUBJ= F$EXTRACT(F$LOCATE(": ",DATA)+2,F$LENGTH(DATA),DATA)
$ GOTO NEXTHEADER
$SKIPSUBJ:
$ GOTO NEXTHEADER
$SKIPHEADER:
$ WRITE OUTPUT "From:	" + FROM
				! Write From: + TAB + USERNAME
$ WRITE OUTPUT "To:	" + USERNAME
				! Write To: + TAB + BBOARDUSERNAME
$ WRITE OUTPUT "Subj:	" + SUBJ
				! Write Subject: + TAB + mail subject
$ WRITE OUTPUT ""		! Write one blank line
$ IF (DATA.EQS."") .OR. MFEMAIL THEN GOTO SKIPBLANKS
$50$:
$ READ/END=END INPUT DATA		! Skip rest of main header
$ IF DATA .NES. "" THEN GOTO 50$
$60$:
$ READ/END=END INPUT DATA		! Skip all of secondary header
$ IF DATA .NES. "" THEN GOTO 60$
$SKIPBLANKS:
$ READ/END=END INPUT DATA		! Skip all blanks
$ IF DATA .EQS. "" THEN GOTO SKIPBLANKS
$NEXT:				! Read and write message text
$ WRITE OUTPUT DATA
$ IF DATA .EQS. FF THEN GOTO HEADER
			! Multiple messages are seperated by form feeds
$ READ/END=END INPUT DATA
$ GOTO NEXT
$END:
$ CLOSE INPUT
$ CLOSE OUTPUT
$ DELETE MFEMSG.MAI;
$EXIT:
$ EXIT
