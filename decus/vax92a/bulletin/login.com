 $!3 $! The following line defines the BULLETIN command.  $!" $ BULL*ETIN :== $BULL_DIR:BULLETIN $!E $! Note: The command prompt when executing the utility is named after E $! the executable image.  Thus, as it is presently set up, the prompt B $! will be "BULLETIN>".  DO NOT make the command that executes theE $! image different from the image name, or certain things will break.  $!H $! If you would rather define the BULLETIN command using CDU rather thanB $! defining it using a symbol, use the BULLETIN.CLD file to do so. $!H $! If you want to have more than one BULLETIN database, replace BULL_DIR: $! with the actual directory to allow redefining BULL_DIR. $!J $! The following line causes new messages to be displayed upon logging in. $! $ BULLETIN/LOGIN/REVERSE $!6 $! If you wish bulletins to be displayed starting with= $! the newest rather the oldest, omit the /REVERSE qualifier. F $! Note that for totally new users, only permanent system messages andG $! the first non-system general message is displayed (which, if you ran > $! INSTURCT.COM, would describe what a non-system message is).C $! This is done so as to avoid overwhelming a new user with lots of / $! messages upon logging in for the first time. N $! Users who have DISMAIL enabled in the authorzation table will automaticallyK $! be set to "NOLOGIN" (see HELP SET NOLOGIN).  If you wish to disable this + $! feature, add /ALL to the /LOGIN command.  $!