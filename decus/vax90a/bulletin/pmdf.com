$set nover
$copy sys$input BULLETIN_MASTER.PAS
$deck
%INCLUDE '[-]ATTRIB.INC'
PROGRAM bulletin_master (output, outbound,
                         %INCLUDE '[-]APFILES.INC',
                         %INCLUDE '[-]MMFILES.INC',
                         %INCLUDE '[-]QUFILES.INC');

(*******************************************************************)
(*                                                                 *)
(*      Authors:   Ned Freed (ned@ymir.bitnet)                     *)
(*                 Mark London (mrl%mit.mfenet@nmfecc.arpa)        *)
(*                 8/18/88                                         *)
(*                                                                 *)
(*******************************************************************)

  CONST
       %INCLUDE '[-]UTILCONST.INC'
       %INCLUDE '[-]OSCONST.INC'
       %INCLUDE '[-]APCONST.INC'
       %INCLUDE '[-]MMCONST.INC'
       %INCLUDE '[-]HECONST.INC'
       %INCLUDE '[-]LOGCONST.INC'
       %INCLUDE '[-]SYCONST.INC'

  TYPE
       %INCLUDE '[-]UTILTYPE.INC'
       %INCLUDE '[-]OSTYPE.INC'
       %INCLUDE '[-]APTYPE.INC'
       %INCLUDE '[-]SYTYPE.INC'
       %INCLUDE '[-]MMTYPE.INC'
       %INCLUDE '[-]HETYPE.INC'
       %INCLUDE '[-]LOGTYPE.INC'

  string = varying [alfa_size] of char;

  VAR
       %INCLUDE '[-]UTILVAR.INC'
       %INCLUDE '[-]OSVAR.INC'
       %INCLUDE '[-]APVAR.INC'
       %INCLUDE '[-]QUVAR.INC'
       %INCLUDE '[-]MMVAR.INC'
       %INCLUDE '[-]HEVAR.INC'
       %INCLUDE '[-]LOGVAR.INC'

       outbound : text;

  (* Place to store the channel we are servicing *)
   mail_channel : mm_channel_ptr := nil;

  (* MM status control flag *)

  mm_status          : (uninitialized, initialized, sending) := uninitialized;

  filename       : vstring;

  %INCLUDE '[-]UTILDEF.INC'
  %INCLUDE '[-]OSDEF.INC'
  %INCLUDE '[-]APDEF.INC'
  %INCLUDE '[-]HEDEF.INC'
  %INCLUDE '[-]LOGDEF.INC'
  %INCLUDE '[-]MMDEF.INC'
  %INCLUDE '[-]QUDEF.INC'

  (* Declare interface routines to BULLETIN *)

  procedure INIT_MESSAGE_ADD (
    in_folder : [class_s] packed array [l1..u1 : integer] of char;
    in_from : [class_s] packed array [l2..u2 : integer] of char;
    in_descrip : [class_s] packed array [l3..u3 : integer] of char;
    var ier : boolean); extern;

  procedure WRITE_MESSAGE_LINE (
    in_line : [class_s] packed array [l1..u1 : integer] of char); extern;

  procedure FINISH_MESSAGE_ADD; extern;

  PROCEDURE warn_master (message : varying [len1] of char);

    BEGIN (* warn_master *)
      writeln;
      os_write_datetime (output);
      writeln (message);
      END; (* warn_master *)

  (* abort program. *)

  PROCEDURE abort_master (message : varying [len1] of char);

    BEGIN (* abort_master *)
      warn_master (message);
      halt;
      END; (* abort_master *)

(* activate_mm fires up the MM package and performs related startup chores. *)

function activate_mm (is_master : boolean) : rp_replyval;

var
  mm_init_reply : rp_replyval; found : boolean; mail_chan_text : ch_chancode;
  stat : integer;

  (* Place to store the protocol that we are providing/servicing *)
  protocol_name : varying [10] of char;

begin (* activate_mm *)
  (* Set up the name of the protocol we are servicing/providing *)
  stat := $TRNLOG (lognam := 'PMDF_PROTOCOL',
                   rslbuf := protocol_name.body,
                   rsllen := protocol_name.length);
  if (not odd (stat)) or (stat = SS$_NOTRAN) then protocol_name := 'IN%';
  mm_status := initialized;
  mm_init_reply := mm_init;
  mail_chan_text := '            ';
  stat := $TRNLOG (lognam := 'PMDF_CHANNEL', rslbuf := mail_chan_text);
  if (not odd (stat)) or (stat = SS$_NOTRAN) then
    mail_chan_text := 'l           ';
  if rp_isgood (mm_init_reply) then begin
    mail_channel := mm_lookup_channel (mail_chan_text);
    if mail_channel = nil then mail_channel := mm_local_channel;
  end else mail_channel := mm_local_channel;
  activate_mm := mm_init_reply;
end; (* activate_mm *)

  (* initialize outbound, mm_ and qu_ *)

  PROCEDURE init;

    VAR fnam : vstring;
        i : integer;

    BEGIN (* init *)
      os_jacket_access := true;
      (* Initialize subroutine packages *)
      IF rp_isbad (activate_mm (false)) THEN
        abort_master ('Can''t initialize MM_ routines');
      IF rp_isbad (qu_init) THEN
        abort_master ('Can''t initialize QU_ routines');
      fnam.length := 0;
      IF NOT os_open_file (outbound, fnam, exclusive_read) THEN
        abort_master ('Can''t open outbound file');
      END; (* init *)


procedure return_bad_messages (var bad_address : vstring);

label
  100;

var
  line : vstring;
  bigline : bigvstring; result : rp_bufstruct;
  pmdfenvelopefrom : vstring;
  temp_line : vstringlptr;

  procedure try_something (rp_error : integer; routine : string);

  begin (* try_something *)
    if rp_isbad (rp_error) then begin
      mm_wkill; mm_status := initialized; goto 100;
    end;
  end; (* try_something *)

begin (* return_bad_messages *)
  if mm_status = uninitialized then
    try_something (activate_mm (false), 'mm_init');
  mm_status := sending;
  try_something (mm_sbinit, 'mm_sbinit');
  initstring (line, 'postmaster@                             ', 11);
  catvstring (line, mm_local_channel^.official_hostname);
  try_something (mm_winit (mail_channel^.chancode, line), 'mm_winit');
  initstring (line,
              'postmaster                              ', 10);
  try_something (mm_wadr (mail_channel^.official_hostname,
                            line), 'mm_wadr');
  try_something (mm_rrply (result), 'mm_rrply');
  try_something (result.rp_val, 'mm_rrply structure return');
  try_something (mm_waend, 'mm_waend');
  initstring (line, 'From: PMDF Mail Server <Postmaster@     ', 35);
  catvstring (line, mm_local_channel^.official_hostname);
  catchar (line, '>');
  catchar (line, chr (chr_lf));
  try_something (mm_wtxt (line), 'mm_wtxt');
  initstring (line, 'To: Postmaster                          ', 14);
  catchar (line, chr (chr_lf));
  try_something (mm_wtxt (line), 'mm_wtxt');
  initstring (line, 'Subject: Undeliverable mail             ', 27);
  catchar (line, chr (chr_lf));
  try_something (mm_wtxt (line), 'mm_wtxt');
  initstring (line, 'Date:                                   ', 6);
  os_cnvtdate (line);
  catchar (line, chr (chr_lf));
  try_something (mm_wtxt (line), 'mm_wtxt');
  line.length := 1; line.body[1] := chr (chr_lf);
  try_something (mm_wtxt (line), 'mm_wtxt');
  initstring (line, 'The message could not be delivered to:  ', 38);
  catchar (line, chr (chr_lf));
  try_something (mm_wtxt (line), 'mm_wtxt');
  line.length := 1; line.body[1] := chr (chr_lf);
  try_something (mm_wtxt (line), 'mm_wtxt');
  initstring (line, 'Addressee:                              ', 11);
  catvstring (line, bad_address);
  catchar (line, chr (chr_lf));
  try_something (mm_wtxt (line), 'mm_wtxt');
  initstring (line, 'Reason: No such bulletin folder.        ', 32);
  catchar (line, chr (chr_lf));
  try_something (mm_wtxt (line), 'mm_wtxt');
  line.length := 1; line.body[1] := chr (chr_lf);
  try_something (mm_wtxt (line), 'mm_wtxt');
  initstring (line, '----------------------------------------', 40);
  catchar (line, chr (chr_lf));
  catchar (line, chr (chr_lf));
  try_something (mm_wtxt (line), 'mm_wtxt');
  try_something (qu_rkill, 'qu_rkill');
  try_something (qu_rinit (filename, pmdfenvelopefrom), 'qu_rinit');
  while rp_isgood (qu_radr (line)) do begin end;
  while rp_isgood (qu_rtxt (bigline)) do
    try_something (mm_bigwtxt (bigline), 'mm_wtxt');
  mm_status := initialized;
  try_something (mm_wtend, 'mm_wtend');
  try_something (mm_rrply (result), 'mm_rrply');
  try_something (result.rp_val, 'mm_rrply structure return');
100:
end; (* return_bad_messages *)

  (* submit messages to BULLETIN *)

  PROCEDURE dosubmit;

    VAR fromaddr, toaddr, tombox, name : vstring;
        retval : rp_replyval;
        line : bigvstring;
        ier, done : boolean;
        i : integer;

    BEGIN (* dosubmit *)
      WHILE NOT eof (outbound) DO BEGIN
        readvstring (outbound, filename, 0);
        IF rp_isgood (qu_rinit (filename, fromaddr)) THEN BEGIN
          done := false;
          FOR i := 1 TO fromaddr.length DO
            fromaddr.body[i] := upper_case (fromaddr.body[i]);
          IF rp_isgood (qu_radr (toaddr)) THEN BEGIN
            REPEAT
              retval := qu_radr (name);
              UNTIL rp_isbad (retval);
            mm_parse_address (toaddr, name, tombox, TRUE, FALSE, 0);
            FOR i := 1 TO tombox.length DO
              tombox.body[i] := upper_case (tombox.body[i]);
            INIT_MESSAGE_ADD (substr (tombox.body, 1, tombox.length),
                              'IN%',' ', ier);
(* The parameter with 'IN%', causes bulletin to search for the From line: *)
(*                            substr (fromaddr.body, 1, fromaddr.length), *)
            IF ier THEN BEGIN
              WHILE rp_isgood (qu_rtxt (line)) DO BEGIN
                IF line.length > 0 THEN line.length := pred (line.length);
                WRITE_MESSAGE_LINE (substr (line.body, 1, line.length));
                END; (* while *)
              FINISH_MESSAGE_ADD;
              done := true;
            END ELSE BEGIN
	      warn_master ('Error opening folder ' +
                              substr (tombox.body, 1, tombox.length));
	      return_bad_messages(tombox);
              done := true;
            END;
	  END
          ELSE warn_master ('Can''t read To: address in file ' +
                            substr (filename.body, 1, filename.length));
          if done then qu_rend else qu_rkill;
          END
        ELSE warn_master ('Can''t open queue file ' +
                          substr (filename.body, 1, filename.length));
        END; (* while *)
      END; (* dosubmit *)

  BEGIN (* bulletin_master *)
    init;
    dosubmit;
    mm_end (true);
    qu_end;
    END. (* bulletin_master *)
$eod 
$copy sys$input MASTER.COM
$deck
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
$eod 
$copy sys$input PMDF.TXT
$deck
BULLETIN_MASTER.PAS and MASTER.COM are the files you need to run a BULLETIN
channel. Put BULLETIN_MASTER.PAS in a subdirectory of PMDF_ROOT:[SRC] (I use
the directory PMDF_ROOT:[SRC.BULLETIN]). Compile it there and then link it as
follows: 

    LINK BULLETIN_MASTER,[EXE]PMDFLIB/LIB,BULL_SOURCE:BULL/LIB,

and put the .EXE in PMDF_ROOT:[EXE]. Put the new MASTER.COM in PMDF_ROOT:[EXE].
NOTE: Check your MASTER.COM, as the latest version of PMDF contains the code
necessary to check for bulletin mail.  However, it will not necessary have the
latest copy of BULLETIN_MASTER.PAS.

You then need a channel definition like the following in your configuration
file PMDF.CNF:

    bull_local single logging
    BULLETIN-DAEMON

And a rewrite rule of the form:

    BULLETIN                          $U%BULLETIN@BULLETIN-DAEMON

Then you put an alias in your ALIASES. file for each mailing list you want to
process this way. I have the following:

    info-vax: info-vax@bulletin
    tex-hax: tex-hax@bulletin
    xmailer-list: xmailer@bulletin
    mail-l: mail-l@bulletin
    jnet-l: jnet-l@bulletin
    policy-l: policy-l@bulletin
    future-l: future-l@bulletin
    mon-l: mon-l@bulletin
    ug-l: ug-l@bulletin

Then mail sent to info-vax@localhost will be routed to a folder called
info-vax. In general, an alias of the form

    a : b@bulletin

will route mail sent to a@localhost to folder b in BULLETIN.

NOTE: If you have BBOARD set for a folder that you convert to be delivered
directly to PMDF, remember to do a SET NOBBOARD for that folders.  After
doing so, restart BULLCP using BULLETIN/START.
$eod 
