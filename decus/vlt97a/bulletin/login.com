$!
$! The following line defines the BULLETIN command.
$!
$ BULL*ETIN :== $BULL_DIR:BULLETIN
$!
$! Note: The command prompt when executing the utility is named after
$! the executable image.  Thus, as it is presently set up, the prompt
$! will be "BULLETIN>".  DO NOT make the command that executes the
$! image different from the image name, or certain things will break.
$!
$! If you would rather define the BULLETIN command using CDU rather than
$! defining it using a symbol, use the BULLETIN.CLD file to do so.
$!
$! If you want to have more than one BULLETIN database, replace BULL_DIR
$! with the actual directory to allow redefining BULL_DIR.  
$!
$! The following line causes new messages to be displayed upon logging in.
$!
$ BULLETIN/LOGIN/REVERSE
$!
$! If you wish bulletins to be displayed starting with
$! the newest rather the oldest, omit the /REVERSE qualifier.
$! Note that for totally new users, only permanent system messages and
$! the first non-system general message is displayed (which, if you ran
$! INSTURCT.COM, would describe what a non-system message is).
$! This is done so as to avoid overwhelming a new user with lots of
$! messages upon logging in for the first time.
$! Users who have DISMAIL enabled in the authorzation table will automatically
$! be set to "NOLOGIN" (see HELP SET NOLOGIN).  If you wish to disable this
$! feature, add /ALL to the /LOGIN command.
$!
