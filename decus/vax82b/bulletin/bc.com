$ save_verify = 'f$verify("NO")
$ if p1 .eqs. "" then p1 := "*"
$ on error then goto done
$ on control_y then goto done
$ temp := "SYS$DISK"
$ mgrdir := "''f$logical(temp)'''f$directory()'"
$ set def sys$sysdisk:[sysexe]
$ open/write tfile temp.bcm
$ write tfile "$ run authorize"
$ write tfile "list ''p1'"
$ close tfile
$ @temp.bcm
$ del temp.bcm;0
$ open/read uaflis sysuaf.lis
$ read/end=eol uaflis line
$ loop:
$   read/end=eol uaflis line
$   user := "''f$extract(21,12,line)'"
$   account := "''f$extract(44,8,line)'"
$   if account .eqs. "        " then goto loop
$   deff := "''f$extract(63,1000,line)'"
$   write sys$output "''user' ''deff'"
$   set def 'deff'
$   run sys$system:bullcheck
$   goto loop
$ eol:
$   close uaflis
$ done:
$   set def sys$sysdisk:[sysexe]
$   delete sysuaf.lis;0
$   set def 'mgrdir'
$   if save_verify then set verify
