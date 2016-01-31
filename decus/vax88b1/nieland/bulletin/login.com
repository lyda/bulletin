$!
$! Note: The command prompt when executing the utility is named after
$! the executable image.  Thus, as it is presently set up, the prompt
$! will be "BULLETIN>".  DO NOT make the command that executes the
$! image different from the image name, or certain things will break.
$! If you wish bulletins to be displayed upon logging in starting with
$! oldest rather than newest, change BULLETIN/LOGIN to BULLETIN/LOGIN/REVERSE.
$!
$! If you would rather define the BULLETIN command using CDU rather than
$! defining it using a symbol, use the BULLETIN.CLD file to do so.
$!
$ BULL*ETIN :== $BULL_DIR:BULLETIN
$ BULLETIN/LOGIN
