$ ! MASTER.COM - Initiate delivery of messages queued on a channel
$ !
$ ! Modification history and parameter definitions are at the end of this file.
$ !
$ set noon
$ !
$ ! Clean up and set up channel name, if on hold just exit
$ !
$ channel_name = f$edit(p1, "COLLAPSE,LOWERCASE")
$ hold_list = "," + f$edit(f$logical("PMDF_HOLD"), "COLLAPSE,LOWERCASE") + ","
$ if f$locate("," + channel_name + ",", hold_list) .lt. -
     f$length(hold_list) then exit
$ define/process pmdf_channel "''channel_name'"
$ !
$ ! Save state information, set up environment properly
$ !
$ save_directory = f$environment("DEFAULT")
$ set default pmdf_root:[queue]
$ save_protection = f$environment("PROTECTION")
$ set protection=(s:rwed,o:rwed,g,w)/default
$ save_privileges = f$setprv("NOSHARE")
$ !
$ if f$logical("PMDF_DEBUG") .eqs. "" then on control_y then goto out
$ !
$ ! Create listing of messages queued on this channel.
$ !
$ if p3 .eqs. "" then p3 = "1-JAN-1970"
$ dirlst_file = "pmdf_root:[log]" + channel_name + "_master_dirlst_" + -
  F$GETJPI ("", "PID") + ".tmp"
$ define/process outbound 'dirlst_file'
$ directory/noheader/notrailer/column=1/since="''p3'"/output='dirlst_file' -
  pmdf_root:[queue]'channel_name'_*.%%;*
$ !
$ ! Determine whether or not connection should really be made
$ !
$ if p2 .nes. "POLL" .and. -
     f$file_attributes(dirlst_file, "ALQ") .eq. 0 then goto out1
$ !
$ ! Handle various channels specially
$ !
$ if channel_name .eqs. "l" then goto local_channel
$ if channel_name .eqs. "d" then goto DECnet_compatibility_channel
$ if channel_name .eqs. "directory" then goto dir_channel
$ if f$extract(0,5,channel_name) .eqs. "anje_"  then goto BITNET_channel
$ if f$extract(0,4,channel_name) .eqs. "bit_"   then goto BITNET_channel
$ if f$extract(0,5,channel_name) .eqs. "bull_"  then goto BULLETIN_channel
$ if f$extract(0,3,channel_name) .eqs. "cn_"    then goto CN_channel
$ if f$extract(0,5,channel_name) .eqs. "ctcp_"  then goto CTCP_channel
$ if f$extract(0,3,channel_name) .eqs. "dn_"    then goto DECnet_channel
$ if f$extract(0,6,channel_name) .eqs. "dsmtp_" then goto DSMTP_channel
$ if f$extract(0,5,channel_name) .eqs. "etcp_"  then goto ETCP_channel
$ if f$extract(0,5,channel_name) .eqs. "ftcp_"  then goto FTCP_channel
$ if f$extract(0,4,channel_name) .eqs. "ker_"   then goto KER_channel
$ if f$extract(0,5,channel_name) .eqs. "mail_"  then goto MAIL_channel
$ if f$extract(0,5,channel_name) .eqs. "mtcp_"  then goto MTCP_channel
$ if f$extract(0,5,channel_name) .eqs. "px25_"  then goto PX25_channel
$ if f$extract(0,4,channel_name) .eqs. "tcp_"   then goto TCP_channel
$ if f$extract(0,5,channel_name) .eqs. "test_"  then goto TEST_channel
$ if f$extract(0,5,channel_name) .eqs. "uucp_"  then goto UUCP_channel
$ if f$extract(0,5,channel_name) .eqs. "wtcp_"  then goto WTCP_channel
$ if f$extract(0,6,channel_name) .eqs. "xsmtp_" then goto XSMTP_channel
$ !
$ ! This must be a PhoneNet channel (the default); set up and use MASTER
$ !  Read the list of valid connection types for each channel.
$ !
$ cnt = f$integer("0")
$ open/read/error=regular_master pmdf_data pmdf_root:[table]phone_list.dat
$       list_loop:
$               read/end=eof_list pmdf_data line
$ !  Ignore comment lines.
$               if (f$extract (0, 1, line) .eqs. "!") then -
                        goto list_loop
$               line = f$edit (line, "COMPRESS,LOWERCASE")
$ !  Get the channel name from the line read.
$               chan = f$extract (0, f$locate(" ", line), line)
$               if (chan .nes. channel_name) then -
$                       goto list_loop
$ !  Get the connection name
$               name = f$edit(f$extract(f$locate(" ",line),255,line),"COLLAPSE")
$ !  If none, then ignore the line
$               if name .eqs. "" then -
                        goto list_loop
$ !  Found at least one to try.
$               cnt = cnt + 1
$               @pmdf_root:[exe]all_master.com 'name'
$               define PMDF_DEVICE TT
$ !
$ ! Define other logical names
$ !
$ define/user script             pmdf_root:[table.'channel_name']'name'_script.
$ define/user ph_current_message pmdf_root:[log]'channel_name'_master_curmsg.tmp
$ define/user option_file        pmdf_root:[table]'channel_name'_option.
$ define/user di_transcript      pmdf_root:[log]di_'channel_name'_master.trn
$ define/user ph_logfile         pmdf_root:[log]ph_'channel_name'_master.log
$ define/user di_errfile         pmdf_root:[log]di_'channel_name'_master.log
$ !
$ !   This check attempts to verify that we are in fact the owner process of
$ !   the device, TT.  If the device is sharable, then we ignore the
$ !   owner.
$ !
$ if (f$getdvi("TT","pid") .nes. f$getjpi(0,"pid")) .and. -
     (f$getdvi("TT","shr") .eqs. "FALSE") then -
        goto list_loop
$ !
$ !  Run master to deliver the mail
$ !
$ run pmdf_root:[exe]master
$ exit_stat = $status
$ !
$ ! Activate optional cleanup script to reset terminal/modem
$ !
$ if f$search("pmdf_root:[exe]''name'_cleanup.com") .nes. "" then -
     @pmdf_root:[exe]'name'_cleanup.com 'exit_stat'
$ deallocate TT
$ deassign TT
$ deassign PMDF_DEVICE
$ !
$ !  If master does not exit normally, then try a different connection.
$ !
$ if exit_stat .ne. 1 then goto list_loop
$ eof_list:
$ close pmdf_data
$ !
$ !  If we found at least one connection type for this channel, then skip
$ !  the attempt to use the conventional mechanism.
$ !
$ if cnt .gt. 0 then goto out_phonenet
$ !
$ regular_master:
$ @pmdf_root:[exe]'channel_name'_master.com
$ define PMDF_DEVICE TT
$ !
$ !  Define logical names
$ !
$ define/user script             pmdf_root:[table]'channel_name'_script.
$ define/user ph_current_message pmdf_root:[log]'channel_name'_master_curmsg.tmp
$ define/user option_file        pmdf_root:[table]'channel_name'_option.
$ define/user di_transcript      pmdf_root:[log]di_'channel_name'_master.trn
$ define/user ph_logfile         pmdf_root:[log]ph_'channel_name'_master.log
$ define/user di_errfile         pmdf_root:[log]di_'channel_name'_master.log
$ !
$ run pmdf_root:[exe]master
$ exit_stat = $status
$ !
$ !  Activate optional cleanup script to reset terminal/modem
$ !
$ if f$search("''channel_name'_cleanup.com") .nes. "" then -
     @pmdf_root:[exe]'channel_name'_cleanup.com 'exit_stat'
$ deallocate TT
$ deassign TT
$ deassign PMDF_DEVICE
$ !
$ out_phonenet:
$ if P4 .eqs. "POST" then wait 00:00:30
$ goto out1
$ !
$ ! Directory channel
$ !
$ dir_channel:
$ !
$ run pmdf_root:[exe]dir_master
$ goto out1
$ !
$ ! This is a DECnet channel; set up and use DN_MASTER
$ !
$ DECnet_channel:
$ !
$ ! Define other logical names
$ !
$ node_name = f$edit(channel_name - "dn_", "UPCASE")
$ define/user ph_current_message pmdf_root:[log]'channel_name'_master_curmsg.tmp
$ define/user option_file        pmdf_root:[table]'channel_name'_option.
$ define/user di_transcript      pmdf_root:[log]di_'channel_name'_master.trn
$ define/user ph_logfile         pmdf_root:[log]ph_'channel_name'_master.log
$ define/user di_errfile         pmdf_root:[log]di_'channel_name'_master.log
$ define/user pmdf_node          "''node_name'::""PMDF="""
$ !
$ run pmdf_root:[exe]dn_master
$ goto out1
$ !
$ ! This is a BITNET channel; use BN_MASTER
$ !
$ BITNET_channel:
$ !
$ if channel_name .eqs. "bit_gateway" then goto BITNET_gateway
$ run pmdf_root:[exe]bn_master
$ goto out1
$ !
$ ! This is the BITNET gateway channel; use BN_GATEWAY
$ !
$ BITNET_gateway:
$ !
$ run pmdf_root:[exe]bn_gateway
$ goto out1
$ !
$ ! This is a BULLETIN channel; use BULLETIN_MASTER
$ !
$ BULLETIN_channel:
$ !
$ run pmdf_root:[exe]bulletin_master
$ goto out1
$ !
$ ! This is a Tektronix TCP channel; use TCP_MASTER
$ !
$ TCP_channel:
$ !
$ run pmdf_root:[exe]tcp_master
$ goto out1
$ !
$ ! This is a CMU/Tektronix TCP channel; use CTCP_MASTER
$ !
$ CTCP_channel:
$ !
$ run pmdf_root:[exe]ctcp_master
$ goto out1
$ !
$ ! This is a Wollongong TCP channel; use WTCP_MASTER
$ !
$ WTCP_channel:
$ !
$ ! Define other logical names
$ !
$ run pmdf_root:[exe]wtcp_master
$ goto out1
$ !
$ ! This is a MultiNet TCP channel; use MTCP_MASTER
$ !
$ MTCP_channel:
$ !
$ run pmdf_root:[exe]mtcp_master
$ goto out1
$ !
$ ! This is a Excelan TCP channel; use ETCP_MASTER
$ !
$ ETCP_channel:
$ !
$ run pmdf_root:[exe]etcp_master
$ goto out1
$ !
$ ! This is an NRC Fusion TCP channel; use FTCP_MASTER
$ !
$ FTCP_channel:
$ !
$ run pmdf_root:[exe]ftcp_master
$ goto out1
$ !
$ CN_channel:
$ !
$ ! Define other logical names
$ !
$ define/user script             pmdf_root:[table]'channel_name'_script.
$ ! following may vary: should point to cnio's group
$ define/table=lnm$process_directory lnm$temporary_mailbox lnm$group_000277
$ !
$ run/nodeb'p5' pmdf_root:[exe]cn_smtp_master
$ goto out1
$ !
$ KER_channel:
$ !
$ ! kermit protocol is slave only. If we get here there has been a mistake.
$ ! however we will just exit and no harm done.
$ goto out1
$ !
$ ! This is a PhoneNet X25 channel; set up and use PX25_MASTER
$ !
$ PX25_channel:
$ !
$ ! Define other logical names
$ !
$ define/user ph_current_message pmdf_root:[log]'channel_name'_master_curmsg.tmp
$ define/user option_file        pmdf_root:[table]'channel_name'_option.
$ define/user di_transcript      pmdf_root:[log]'channel_name'_di_master.trn
$ define/user ph_logfile         pmdf_root:[log]'channel_name'_ph_master.log
$ define/user di_errfile         pmdf_root:[log]'channel_name'_di_master.log
$ !
$ run pmdf_root:[exe]PX25_master
$ goto out1
$ !
$ ! This is a DEC/Shell channel; set up and use UUCP_MASTER
$ !
$ UUCP_channel:
$ !
$ ! Define other logical names
$ !
$ uucp_to_host = channel_name - "uucp_"
$ define/user uucp_to_host       "''uucp_to_host'"
$ define/user uucp_current_message -
  pmdf_root:[log]'channel_name'_master_curmsg.tmp
$ define/user uucp_logfile       pmdf_root:[log]'channel_name'_master.logfile
$ !
$ run pmdf_root:[exe]UUCP_master
$ uupoll = "$shell$:[usr.lib.uucp]uupoll"
$ uupoll 'uucp_to_host'
$ goto out1
$ !
$ ! This is a X.25 SMTP channel; set up and use XSMTP_MASTER
$ !
$ XSMTP_channel:
$ !
$ run pmdf_root:[exe]xsmtp_master
$ goto out1
$ !
$ ! This is a DECNET SMTP channel; set up and use DSMTP_MASTER
$ !
$ DSMTP_channel:
$ !
$ run pmdf_root:[exe]dsmtp_master
$ goto out1
$ !
$ ! Handle delivery on the local channel, MAIL_ channels, and
$ ! the DECnet compatibility channel
$ !
$ MAIL_channel:
$ local_channel:
$ DECnet_compatibility_channel:
$ open/read queue_file 'dirlst_file'
$ local_loop:
$   read/end=exit_local_loop/error=exit_local_loop  queue_file file_to_process
$   priv_list = f$setprv("SYSPRV, DETACH")
$   mail/protocol=pmdf_mailshr 'file_to_process'
$   priv_list = f$setprv(priv_list)
$ goto local_loop
$ !
$ exit_local_loop:
$ close queue_file
$ goto out1
$ !
$ ! This is a SMTP test channel, use TEST_SMTP_MASTER
$ !
$ TEST_channel:
$ !
$ ! Typically some form of redirection is needed here...
$ deassign sys$input
$ run pmdf_root:[exe]test_smtp_master
$ goto out1
$ !
$ out1:
$ delete 'dirlst_file';*
$ !
$ ! Common exit point - clean up things first
$ !
$ out:
$ if f$logical("OUTBOUND") .nes. "" then deassign/process outbound
$ if f$logical("PMDF_CHANNEL") .nes. "" then deassign/process pmdf_channel
$ if f$logical("PMDF_DATA") .nes. "" then close pmdf_data
$ if f$logical("PMDF_DEVICE") .eqs. "" then goto restore
$ deallocate TT
$ deassign TT
$ deassign PMDF_DEVICE
$ restore:
$ !
$ ! Restore saved stuff
$ !
$ set protection=('save_protection')/default
$ set default 'save_directory'
$ set process/priv=('save_privileges')
$ !
$ exit
$ !
$ ! Modification history:
$ !
$ ! This version by Ned Freed, 20-Jul-1986
$ !
$ ! Modified by Gregg Wonderly to allow multiple connections for each channel
$ !   10-Oct-1986.
$ ! Some additions by Ned Freed 30-Oct-86.
$ ! Added CMU/Tektronix TCP channel (CTCP) /Kevin Carosso 6-Mar-1987
$ ! Added Multinet TCP channel (MTCP) /Ned Freed 10-Mar-1987
$ ! Added directory save/restore /Ned Freed 1-Jun-1987
$ ! Added Excelan TCP channel (ETCP) /Ned Freed 9-Jul-1987
$ ! Added MAIL, CNIO, KERMIT channel /Bob Smart 4-Jul-1987
$ ! Added Warwick Jackson's PhoneNet X25 support /Ned Freed 5-Sep-87
$ ! Added X25 SMTP channel SX25_ /Goeran Bengtsson, Mats Sundvall 24-Jul-87
$ ! Added NRC Fusion TCP channel (FTCP) /Kevin Carosso 12-Jan-1988
$ ! Added a variant of Randy McGee's code to put a list of channels on hold
$ !   /Ned Freed 9-Feb-1988
$ ! Made this procedure save and restore a little more state information
$ !   than it used to, including default protection and privileges. Also
$ !   moved a bunch of the logical name assignments around to eliminate
$ !   redundant code all over the place. /Ned Freed 10-Feb-1988
$ ! Modified to allow P3 date/time paramter. /Ned Freed 23-Feb-1988
$ ! Added support for Dennis Boylan's UUCP channel. /Ned Freed 28-Mar-1988
$ ! Added Robert Smart's directory channel. /Ned Freed 21-Apr-1988
$ ! Added support for Warwick Jackson's SMTP over X.25 and SMTP over
$ !   DECnet channels. /Ned Freed 26-May-1988
$ ! Added P4 and P5 parameters. /Ned Freed 10-Jun-1988
$ ! Added code to call the TEST_SMTP_MASTER for testing. /Ned Freed 1-Jul-1988
$ ! Added preliminary support for ANJE. /Ned Freed 7-Jul-1988
$ ! Removed extra dispatch for WTCP_ channel. /Ned Freed 3-Sep-1988
$ ! Added dispatch for BULL_ channel. /Ned Freed 28-Nov-1988
$ ! Cleaned up error recovered and emergency exit -- close PHONE_LIST.DAT
$ !   file when aborting. /Ned Freed 13-Dec-1988
$ ! Additional error recovery cleanup -- use PMDF_DEVICE instead of TT to
$ !   allow deallocation on an abort. /Ned Freed 14-Dec-1988
$ !
$ ! Parameters:
$ !
$ !   P1 - Name of the channel whose messages are to be delivered.
$ !   P2 - Activity type. If P2 .eqs. "POLL", establish the connection
$ !        unconditionally, otherwise only establish the connection if
$ !        messages are waiting in the queue.
$ !   P3 - Earliest possible date/time for message(s). Messages older than
$ !        this time are not processed.
$ !   P4 - Environment. P4 .eqs. "POST" if MASTER is being called from the
$ !        POST.COM procedure or some other procedure that invokes MASTER
$ !        more than once. This parameter is used to insert delays before
$ !        returning if hardware needs time to reset.
$ !   P5 - Parameter reserved for channel-specific uses.
