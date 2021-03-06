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
(*                 Winter 1988                                     *)
(*                                                                 *)
(*******************************************************************)

  CONST
       %INCLUDE '[-]UTILCONST.INC'
       %INCLUDE '[-]OSCONST.INC'
       %INCLUDE '[-]APCONST.INC'
       %INCLUDE '[-]MMCONST.INC'
       %INCLUDE '[-]HECONST.INC'
       %INCLUDE '[-]LOGCONST.INC'

  TYPE
       %INCLUDE '[-]UTILTYPE.INC'
       %INCLUDE '[-]OSTYPE.INC'
       %INCLUDE '[-]APTYPE.INC'
       %INCLUDE '[-]MMTYPE.INC'
       %INCLUDE '[-]HETYPE.INC'
       %INCLUDE '[-]LOGTYPE.INC'

  VAR
       %INCLUDE '[-]UTILVAR.INC'
       %INCLUDE '[-]OSVAR.INC'
       %INCLUDE '[-]APVAR.INC'
       %INCLUDE '[-]QUVAR.INC'
       %INCLUDE '[-]MMVAR.INC'
       %INCLUDE '[-]HEVAR.INC'
       %INCLUDE '[-]LOGVAR.INC'

       outbound : text;

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

  (* initialize outbound, mm_ and qu_ *)

  PROCEDURE init;

    VAR fnam : vstring;
        i : integer;

    BEGIN (* init *)
      os_jacket_access := true;
      (* Initialize subroutine packages *)
      IF rp_isbad (mm_init) THEN
        abort_master ('Can''t initialize MM_ routines');
      IF rp_isbad (qu_init) THEN
        abort_master ('Can''t initialize QU_ routines');
      fnam.length := 0;
      IF NOT os_open_file (outbound, fnam, exclusive_read) THEN
        abort_master ('Can''t open outbound file');
      END; (* init *)

(* pmdf_to_vms_backward is used to convert a PMDF From: address into something
   that VMS MAIL will like. *)

procedure pmdf_to_vms_backward (var addressee : vstring);

var
  buffer, dummy : vstring; i,stat : integer;

  (* Place to store the protocol that we are providing/servicing *)
  protocol_name : varying [10] of char;

begin (* pmdf_to_vms_backward *)
  (* Set up the name of the protocol we are servicing/providing *)
  stat := $TRNLOG (lognam := 'PMDF_PROTOCOL',
                   rslbuf := protocol_name.body,
                   rsllen := protocol_name.length);
  if (not odd (stat)) or (stat = SS$_NOTRAN) then protocol_name := 'IN%';
  copyvstring (buffer, addressee);
  addressee.length := 0;
  for i := 1 to protocol_name.length do catchar (addressee, protocol_name[i]);
  catchar (addressee, '"');
  for i := 1 to buffer.length do begin
    case buffer.body[i] of
      '''' : begin
               catchar (addressee, '\'); catchar (addressee, 's');
             end;
      '"'  : catchar (addressee, '''');
      '\'  : begin
               catchar (addressee, '\'); catchar (addressee, '\');
             end;
      otherwise catchar (addressee, buffer.body[i]);
    end; (* case *)
  end; (* for *)
  catchar (addressee, '"');
end; (* pmdf_to_vms_backward *)

  (* submit messages to BULLETIN *)

  PROCEDURE dosubmit;

    VAR filename, fromaddr, toaddr, tombox, name : vstring;
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
            pmdf_to_vms_backward (fromaddr);
            INIT_MESSAGE_ADD (substr (tombox.body, 1, tombox.length),
                              substr (fromaddr.body, 1, fromaddr.length),
                              ' ', ier);
            IF ier THEN BEGIN
              WHILE rp_isgood (qu_rtxt (line)) DO BEGIN
                IF line.length > 0 THEN line.length := pred (line.length);
                WRITE_MESSAGE_LINE (substr (line.body, 1, line.length));
                END; (* while *)
              FINISH_MESSAGE_ADD;
              done := true;
              END
            ELSE warn_master ('Error opening folder ' +
                              substr (tombox.body, 1, tombox.length));
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
$ if channel_name .eqs. "directory" then goto dir_channelN
$ if f$extract(0,5,channel_name) .eqs. "anje_"  then goto BITNET_channel
$ if f$extract(0,4,channel_name) .eqs. "bit_"   then goto BITNET_channel
$ if f$extract(0,5,channel_name) .eqs. "bull_"  then goto BULLETIN_channel
$ if f$extract(0,3,channel_name) .eqs. "cn_"    then goto CN_channel
$ if f$extract(0,5,channel_name) .eqs. "ctcp_"  then goto CTCP_channel
$ if f$extract(0,3,channel_name) .eqs. "dn_"    then goto DECnet_channel
$ if f$extract(0,6,channel_name) .eqs. "dsmtp_" then goto DSMTP_channel 
$ if f$extract(0,5,channel_name) .eqs. "etcp_"  then goto ETCP_channel
$ if f$extract(0,5,channel_name) .eqs. "ftcp_"  then goto FTCP_channel
$ if f$extract(0,4,channel_name) .eqs. "ker_"   then goto KER_channel%
$ if f$extract(0,5,channel_name) .eqs. "mail_"  then goto MAIL_channel
$ if f$extract(0,5,channel_name) .eqs. "mtcp_"  then goto MTCP_channel
$ if f$extract(0,5,channel_name) .eqs. "px25_"  then goto PX25_channel
$ if f$extract(0,4,channel_name) .eqs. "tcp_"   then goto TCP_channelN
$ if f$extract(0,5,channel_name) .eqs. "test_"  then goto TEST_channel
$ if f$extract(0,5,channel_name) .eqs. "uucp_"  then goto UUCP_channel
$ if f$extract(0,5,channel_name) .eqs. "wtcp_"  then goto WTCP_channel
$ if f$extract(0,6,channel_name) .eqs. "xsmtp_" then goto XSMTP_channelI
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
$               chan = f$extract (0, f$locate(" ", line), line)a
$               if (chan .nes. channel_name) then -d
$                       goto list_loop
$ !  Get the connection name
$               name = f$edit(f$extract(f$locate(" ",line),255,line),"COLLAPSE")
$ !  If none, then ignore the line
$               if name .eqs. "" then -s
                        goto list_loop
$ !  Found at least one to try.a
$               cnt = cnt + 1E
$               @pmdf_root:[exe]all_master.com 'name' 
$               define PMDF_DEVICE TT*
$ !
$ ! Define other logical names
$ ! 
$ define/user script             pmdf_root:[table.'channel_name']'name'_script. 
$ define/user ph_current_message pmdf_root:[log]'channel_name'_master_curmsg.tmp
$ define/user option_file        pmdf_root:[table]'channel_name'_option.
$ define/user di_transcript      pmdf_root:[log]di_'channel_name'_master.trn
$ define/user ph_logfile         pmdf_root:[log]ph_'channel_name'_master.log
$ define/user di_errfile         pmdf_root:[log]di_'channel_name'_master.log
$ !C
$ !   This check attempts to verify that we are in fact the owner process of
$ !   the device, TT.  If the device is sharable, then we ignore the
$ !   owner.
$ !
$ if (f$getdvi("TT","pid") .nes. f$getjpi(0,"pid")) .and. -
     (f$getdvi("TT","shr") .eqs. "FALSE") then -
        goto list_loop
$ ! 
$ !  Run master to deliver the maili
$ !
$ run pmdf_root:[exe]master1
$ exit_stat = $status*
$ !_
$ ! Activate optional cleanup script to reset terminal/modem
$ !v
$ if f$search("pmdf_root:[exe]''name'_cleanup.com") .nes. "" then - 
     @pmdf_root:[exe]'name'_cleanup.com 'exit_stat' 
$ deallocate TTe
$ deassign TTa
$ deassign PMDF_DEVICE
$ !s
$ !  If master does not exit normally, then try a different connection. 
$ !e
$ if exit_stat .ne. 1 then goto list_loop 
$ eof_list: 
$ close pmdf_datah
$ !a
$ !  If we found at least one connection type for this channel, then skip 
$ !  the attempt to use the conventional mechanism.i
$ !
$ if cnt .gt. 0 then goto out_phonenet
$ !d
$ regular_master:h
$ @pmdf_root:[exe]'channel_name'_master.com 
$ define PMDF_DEVICE TTe
$ !'
$ !  Define logical names 
$ ! 
$ define/user script             pmdf_root:[table]'channel_name'_script.
$ define/user ph_current_message pmdf_root:[log]'channel_name'_master_curmsg.tmp
$ define/user option_file        pmdf_root:[table]'channel_name'_option.
$ define/user di_transcript      pmdf_root:[log]di_'channel_name'_master.trn
$ define/user ph_logfile         pmdf_root:[log]ph_'channel_name'_master.log
$ define/user di_errfile         pmdf_root:[log]di_'channel_name'_master.log
$ ! 
$ run pmdf_root:[exe]masterb
$ exit_stat = $status 
$ !o
$ !  Activate optional cleanup script to reset terminal/modem;
$ ! 
$ if f$search("''channel_name'_cleanup.com") .nes. "" then -
     @pmdf_root:[exe]'channel_name'_cleanup.com 'exit_stat'n
$ deallocate TT 
$ deassign TT[
$ deassign PMDF_DEVICE
$ !d
$ out_phonenet: 
$ if P4 .eqs. "POST" then wait 00:00:30N
$ goto out1 
$ !T
$ ! Directory channel:
$ !r
$ dir_channel:
$ ! 
$ run pmdf_root:[exe]dir_master 
$ goto out1p
$ !a
$ ! This is a DECnet channel; set up and use DN_MASTER
$ !O
$ DECnet_channel:l
$ ! 
$ ! Define other logical names
$ !p
$ node_name = f$edit(channel_name - "dn_", "UPCASE")
$ define/user ph_current_message pmdf_root:[log]'channel_name'_master_curmsg.tmp
$ define/user option_file        pmdf_root:[table]'channel_name'_option.
$ define/user di_transcript      pmdf_root:[log]di_'channel_name'_master.trn
$ define/user ph_logfile         pmdf_root:[log]ph_'channel_name'_master.log
$ define/user di_errfile         pmdf_root:[log]di_'channel_name'_master.log
$ define/user pmdf_node          "''node_name'::""PMDF="""
$ ! 
$ run pmdf_root:[exe]dn_master
$ goto out1I
$ !S
$ ! This is a BITNET channel; use BN_MASTER 
$ ! 
$ BITNET_channel: 
$ !E
$ if channel_name .eqs. "bit_gateway" then goto BITNET_gateway
$ run pmdf_root:[exe]bn_master
$ goto out1)
$ ! 
$ ! This is the BITNET gateway channel; use BN_GATEWAY
$ !a
$ BITNET_gateway:
$ ! 
$ run pmdf_root:[exe]bn_gatewaye
$ goto out1 
$ !a
$ ! This is a BULLETIN channel; use BULLETIN_MASTERr
$ !
$ BULLETIN_channel: 
$ !L
$ run pmdf_root:[exe]bulletin_master
$ goto out1 
$ ! 
$ ! This is a Tektronix TCP channel; use TCP_MASTERt
$ !
$ TCP_channel:
$ ! 
$ run pmdf_root:[exe]tcp_master
$ goto out1l
$ !_
$ ! This is a CMU/Tektronix TCP channel; use CTCP_MASTER
$ ! 
$ CTCP_channel:.
$ !u
$ run pmdf_root:[exe]ctcp_master
$ goto out1R
$ !
$ ! This is a Wollongong TCP channel; use WTCP_MASTERq
$ ! 
$ WTCP_channel:
$ ! 
$ ! Define other logical names
$ !f
$ run pmdf_root:[exe]wtcp_master
$ goto out1 
$ !o
$ ! This is a MultiNet TCP channel; use MTCP_MASTER 
$ !j
$ MTCP_channel: 
$ !e
$ run pmdf_root:[exe]mtcp_master
$ goto out1o
$ !s
$ ! This is a Excelan TCP channel; use ETCP_MASTER
$ !A
$ ETCP_channel:f
$ !t
$ run pmdf_root:[exe]etcp_master
$ goto out1
$ !f
$ ! This is an NRC Fusion TCP channel; use FTCP_MASTER
$ !'
$ FTCP_channel:
$ !
$ run pmdf_root:[exe]ftcp_master
$ goto out1p
$ !l
$ CN_channel:e
$ !c
$ ! Define other logical names
$ !e
$ define/user script             pmdf_root:[table]'channel_name'_script.
$ ! following may vary: should point to cnio's group
$ define/table=lnm$process_directory lnm$temporary_mailbox lnm$group_000277q
$ ! 
$ run/nodeb'p5' pmdf_root:[exe]cn_smtp_mastera
$ goto out1 
$ !g
$ KER_channel:
$ !n
$ ! kermit protocol is slave only. If we get here there has been a mistake.o
$ ! however we will just exit and no harm done.
$ goto out1"
$ !D
$ ! This is a PhoneNet X25 channel; set up and use PX25_MASTER
$ !o
$ PX25_channel:c
$ !=
$ ! Define other logical names
$ !-
$ define/user ph_current_message pmdf_root:[log]'channel_name'_master_curmsg.tmp
$ define/user option_file        pmdf_root:[table]'channel_name'_option.
$ define/user di_transcript      pmdf_root:[log]'channel_name'_di_master.trn
$ define/user ph_logfile         pmdf_root:[log]'channel_name'_ph_master.log
$ define/user di_errfile         pmdf_root:[log]'channel_name'_di_master.log
$ !c
$ run pmdf_root:[exe]PX25_master
$ goto out1n
$ !
$ ! This is a DEC/Shell channel; set up and use UUCP_MASTERN
$ !a
$ UUCP_channel:a
$ !4
$ ! Define other logical names
$ !t
$ uucp_to_host = channel_name - "uucp_"n
$ define/user uucp_to_host       "''uucp_to_host'"
$ define/user uucp_current_message -
  pmdf_root:[log]'channel_name'_master_curmsg.tmpc
$ define/user uucp_logfile       pmdf_root:[log]'channel_name'_master.logfilen
$ !.
$ run pmdf_root:[exe]UUCP_master
$ uupoll = "$shell$:[usr.lib.uucp]uupoll".
$ uupoll 'uucp_to_host'_
$ goto out1f
$ !t
$ ! This is a X.25 SMTP channel; set up and use XSMTP_MASTER
$ !f
$ XSMTP_channel:
$ !m
$ run pmdf_root:[exe]xsmtp_mastera
$ goto out1e
$ !t
$ ! This is a DECNET SMTP channel; set up and use DSMTP_MASTER
$ !a
$ DSMTP_channel:
$ !q
$ run pmdf_root:[exe]dsmtp_master
$ goto out1t
$ !c
$ ! Handle delivery on the local channel, MAIL_ channels, anda
$ ! the DECnet compatibility channel
$ !t
$ MAIL_channel: 
$ local_channel:
$ DECnet_compatibility_channel:g
$ open/read queue_file 'dirlst_file'
$ local_loop:q
$   read/end=exit_local_loop/error=exit_local_loop  queue_file file_to_process
$   priv_list = f$setprv("SYSPRV, DETACH")
$   mail/protocol=pmdf_mailshr 'file_to_process'
$   priv_list = f$setprv(priv_list))
$ goto local_loopn
$ ! 
$ exit_local_loop:
$ close queue_file
$ goto out1n
$ !t
$ ! This is a SMTP test channel, use TEST_SMTP_MASTERo
$ !i
$ TEST_channel:s
$ !e
$ ! Typically some form of redirection is needed here...
$ deassign sys$input
$ run pmdf_root:[exe]test_smtp_master
$ goto out1l
$ !
$ out1: 
$ delete 'dirlst_file';*
$ !t
$ ! Common exit point - clean up things first 
$ !f
$ out:
$ if f$logical("OUTBOUND") .nes. "" then deassign/process outbound
$ if f$logical("PMDF_CHANNEL") .nes. "" then deassign/process pmdf_channel
$ if f$logical("PMDF_DATA") .nes. "" then close pmdf_datan
$ if f$logical("PMDF_DEVICE") .eqs. "" then goto restore
$ deallocate TT.
$ deassign TTt
$ deassign PMDF_DEVICE
$ restore:
$ !_
$ ! Restore saved stufft
$ !a
$ set protection=('save_protection')/default
$ set default 'save_directory'
$ set process/priv=('save_privileges')
$ ! 
$ exit
$ ! 
$ ! Modification history: 
$ ! 
$ ! This version by Ned Freed, 20-Jul-1986
$ !s
$ ! Modified by Gregg Wonderly to allow multiple connections for each channel]
$ !   10-Oct-1986.
$ ! Some additions by Ned Freed 30-Oct-86.
$ ! Added CMU/Tektronix TCP channel (CTCP) /Kevin Carosso 6-Mar-1987
$ ! Added Multinet TCP channel (MTCP) /Ned Freed 10-Mar-1987
$ ! Added directory save/restore /Ned Freed 1-Jun-1987
$ ! Added Excelan TCP channel (ETCP) /Ned Freed 9-Jul-1987
$ ! Added MAIL, CNIO, KERMIT channel /Bob Smart 4-Jul-1987
$ ! Added Warwick Jackson's PhoneNet X25 support /Ned Freed 5-Sep-87
$ ! Added X25 SMTP channel SX25_ /Goeran Bengtsson, Mats Sundvall 24-Jul-87e
$ ! Added NRC Fusion TCP channel (FTCP) /Kevin Carosso 12-Jan-1988
$ ! Added a variant of Randy McGee's code to put a list of channels on hold 
$ !   /Ned Freed 9-Feb-1988e
$ ! Made this procedure save and restore a little more state information
$ !   than it used to, including default protection and privileges. Also
$ !   moved a bunch of the logical name assignments around to eliminatel
$ !   redundant code all over the place. /Ned Freed 10-Feb-1988 
$ ! Modified to allow P3 date/time paramter. /Ned Freed 23-Feb-1988l
$ ! Added support for Dennis Boylan's UUCP channel. /Ned Freed 28-Mar-1988
$ ! Added Robert Smart's directory channel. /Ned Freed 21-Apr-1988
$ ! Added support for Warwick Jackson's SMTP over X.25 and SMTP over
$ !   DECnet channels. /Ned Freed 26-May-1988 
$ ! Added P4 and P5 parameters. /Ned Freed 10-Jun-1988
$ ! Added code to call the TEST_SMTP_MASTER for testing. /Ned Freed 1-Jul-1988
$ ! Added preliminary support for ANJE. /Ned Freed 7-Jul-1988 
$ ! Removed extra dispatch for WTCP_ channel. /Ned Freed 3-Sep-1988_
$ ! Added dispatch for BULL_ channel. /Ned Freed 28-Nov-1988
$ ! Cleaned up error recovered and emergency exit -- close PHONE_LIST.DATe
$ !   file when aborting. /Ned Freed 13-Dec-1988
$ ! Additional error recovery cleanup -- use PMDF_DEVICE instead of TT tot
$ !   allow deallocation on an abort. /Ned Freed 14-Dec-1988
$ ! 
$ ! Parameters:a
$ !c
$ !   P1 - Name of the channel whose messages are to be delivered.
$ !   P2 - Activity type. If P2 .eqs. "POLL", establish the connection
$ !        unconditionally, otherwise only establish the connection if
$ !        messages are waiting in the queue. 
$ !   P3 - Earliest possible date/time for message(s). Messages older than
$ !        this time are not processed./
$ !   P4 - Environment. P4 .eqs. "POST" if MASTER is being called from the
$ !        POST.COM procedure or some other procedure that invokes MASTERT
$ !        more than once. This parameter is used to insert delays before 
$ !        returning if hardware needs time to reset. 
$ !   P5 - Parameter reserved for channel-specific uses.
$eod :
$copy sys$input PMDF.TXT
$deck 
BULLETIN_MASTER.PAS and MASTER.COM are the files you need to run a BULLETINa
channel. Put BULLETIN_MASTER.PAS in a subdirectory of PMDF_ROOT:[SRC] (I use
the directory PMDF_ROOT:[SRC.BULLETIN]). Compile it there and then link it asa
follows: m

    LINK BULLETIN_MASTER,[EXE]PMDFLIB/LIB,BULL_SOURCE:BULL/LIB,]

and put the .EXE in PMDF_ROOT:[EXE]. Put the new MASTER.COM in PMDF_ROOT:[EXE]._

You then need a channel definition like the following in your configurationa
file PMDF.CNF:

    bull_local single master logging
    BULLETIN-DAEMONa

And a rewrite rule of the form:n

    BULLETIN                          $U%BULLETIN@BULLETIN-DAEMON_

Then you put an alias in your ALIASES. file for each mailing list you want to 
process this way. I have the following: 

    info-vax: info-vax@bulletinT
    tex-hax: tex-hax@bulletinb
    xmailer-list: xmailer@bulletin
    mail-l: mail-l@bulletinn
    jnet-l: jnet-l@bulletinI
    policy-l: policy-l@bulletinr
    future-l: future-l@bulletin 
    mon-l: mon-l@bulletinT
    ug-l: ug-l@bulletinM

Then mail sent to info-vax@localhost will be routed to a folder called
info-vax. In general, an alias of the form

    a : b@bulletin

will route mail sent to a@localhost to folder b in BULLETIN.

NOTE: If you have BBOARD set for a folder that you convert to be delivered
directly to PMDF, remember to do a SET NOBBOARD for that folders.  After
doing so, restart BULLCP using BULLETIN/START.
$eod n
