$!
$! Note: The command prompt when executing the utility is named after
$! the executable image.  Thus, as it is presently set up, the prompt
$! will be "BULLETIN>".  DO NOT make the command that executes the
$! image different from the image name, or certain things will break.
$! If you wish bulletins to be displayed upon logging in starting with
$! oldest rather than newest, change BULLETIN/LOGIN to BULLETIN/LOGIN/REVERSE. 
$!
$ BULL*ETIN :== $SYS$SYSTEM:BULLETIN
$ BULLETIN/LOGIN
