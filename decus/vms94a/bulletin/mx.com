$set nover
$copy/log sys$input BUILD_MX_BULL.COM
$deck
$ save_verify = 'f$verify(0)'
$!
$!  Command file to build MX_BULL (MX SITE transport for BULLETIN)
$!
$ say := write sys$output
$ if f$trnlnm("BULL_SOURCE") .eqs. ""
$ then	say "BULL_SOURCE logical not defined; must point to BULL.OLB directory"
$	exit
$ endif
$ say "Compiling MX_BULL...."
$ cc mx_bull
$ say "Linking MX_BULL...."
$ link/notrace mx_bull,bull_source:BULL.OLB/LIB,sys$input/option
SYS$SHARE:VAXCRTL.EXE/SHARE
$ say "Build of MX_BULL.EXE completed"
$ exit f$verify(save_verify).or.1
$eod 
$copy/log sys$input MX_BULL.C
$deck
#module MX_BULL "01-001"
/*
 *
 *  Program:	MX_BULL
 *
 *  Author:	Hunter Goatley
 *		Academic Computing, STH 226
 *		Western Kentucky University
 *		Bowling Green, KY 42101
 *		goathunter@wkuvx1.bitnet
 *		502-745-5251
 *
 *  Date:	March 8, 1991
 *
 *  Functional description:
 *
 *	This program serves as an MX SITE transport to transfer incoming
 *	mail files to UALR's BULLETIN.
 *
 *	The MX_SITE delivery agent takes messages routed to a SITE path and
 *	feeds them into a subprocess that executes a command procedure named
 *	MX_EXE:SITE_DELIVER.COM.  There are three parameters passed to the
 *	the command procedure:
 *
 *		P1	- The name of a temporary file containing the message
 *			  text, including all of the RFC822 headers
 *			  (corresponding to the DATA part of an SMTP
 *			  transaction).
 *		P2	- The name of a temporary file containing a list of
 *			  a messages recipients, which corresponds to the
 *			  RCPT_TO addresses of an SMTP transaction.
 *		P3	- The RFC822 address of the sender of the message,
 *			  which corresponds to the MAIL FROM address of an
 *			  SMTP transaction.
 *
 *	This program expects the same parameters, except that the third
 *	parameter is optional.  If the third parameter is omitted, BULLETIN
 *	will scan the RFC822 headers in the message for a "From:" line.
 *	If the third parameter is specified, it is expected to be a file
 *	specification.  It is assumed that SITE_DELIVER.COM has written the
 *	address to this file.
 *
 *	The logical MX_BULLETIN_POSTMASTER can be defined as a local
 *	username to receive error notices.  If BULLETIN returns an error
 *	while trying to add a message, and the MX_BULLETIN_POSTMASTER
 *	is defined as a valid local username, the message will be mailed
 *	to that user for further handling.
 *
 *	MX_BULLETIN_POSTMASTER must be defined system-wide in executive mode:
 *
 *		$ DEFINE/SYS/EXEC MX_BULLETIN_POSTMASTER GOATHUNTER
 *
 *  Modification history:
 *
 *	01-001		Hunter Goatley		14-MAR-1991 14:41
 *		Added scan_for_from_line, which scans the message's RFC822
 *		headers for the "From:" line.  General cleanup on a few
 *		routines.  MX_BULL now provides an RESPOND-able address in
 *		BULLETIN.
 *
 *	01-000		Hunter Goatley		 8-MAR-1991 07:20
 *		Genesis.
 *
 */

/*  Include all needed structures and constants  */

#include descrip
#include lib$routines
#include libdef
#include lnmdef
#include maildef
#include rms
#include ssdef
#include str$routines
#include string

/* Declare the external BULLETIN routines that we call */

unsigned long int INIT_MESSAGE_ADD();
unsigned long int WRITE_MESSAGE_LINE();
unsigned long int FINISH_MESSAGE_ADD();

/* Define some macros to make things a little easier */

#define rms_get(rab) ((rms_status = SYS$GET(rab)))
#define err_exit(stat) {traceerr(stat); return(stat);}
#define vms_errchk2() if(!(vms_status&1)) err_exit(vms_status);
#define vms_errchk(func) {vms_status=func; vms_errchk2();}

#define tracemsg(msg) if (trace) printf("MX_BULL: %s\n",msg);
#define traceerr(msg) if (trace) printf("MX_BULL: Error status %%X%08x\n",msg);

/* Define some global variables to make things easy */

struct FAB msgfab;				/* FAB for message text */
struct RAB msgrab;				/* RAB for message text */
struct FAB rcptfab;				/* FAB for recipients file */
struct RAB rcptrab;				/* RAB for recipients file */
struct FAB fromfab;				/* FAB for FROM file */
struct RAB fromrab;				/* RAB for FROM file */
char msgbuf[512];				/* Input buffer for msgrab */
char rcptbuf[512];				/* Input buffer for rcptrab */
char frombuf[512];				/* Input buffer for frombuf */
short trace;
unsigned long int rms_status;			/* Status of RMS calls */
unsigned long int vms_status;			/* Status of other calls */

static $DESCRIPTOR(lnm_table,"LNM$SYSTEM_TABLE");

#define itmlstend {0,0,0,0}			/* An empty item list */
typedef struct itmlst				/* An item list structure */
{
  short buffer_length;
  short item_code;
  long buffer_address;
  long return_length_address;
} ITMLST;

ITMLST
  nulllist[] = {itmlstend};

ITMLST
  address_itmlst[] = {				/* MAIL$SEND_ADD_ADDRESS */
	{0, MAIL$_SEND_USERNAME, 0, 0},
	itmlstend},
  bodypart_itmlst[] = {				/* MAIL$SEND_ADD_BODYPART */
	{0, MAIL$_SEND_RECORD, 0, 0},
	itmlstend},
  attribute_itmlst[] = {			/* MAIL$SEND_ADD_ATTRIBUTE */
	{0, MAIL$_SEND_TO_LINE, 0, 0},
	{0, MAIL$_SEND_FROM_LINE, 0, 0},
	{0, MAIL$_SEND_SUBJECT, 0, 0},
	itmlstend}
  ;

ITMLST
  trnlnm_itmlst[] = {				/* $TRNLNM item list */
	{0, LNM$_STRING, 0, 0},
	itmlstend}
  ;


/*
 *
 *  Function:	open_file_rms
 *
 *  Functional description:
 *
 *	This routine opens a sequential text file in VMS "normal text" file
 *	format.  It uses RMS to open the file.
 *
 *  Inputs:
 *
 *	infab	- Address of the input FAB
 *	inrab	- Address of the input RAB
 *	buff	- Address of the input buffer
 *	filename - Address of the filename to open (ASCIZ)
 *
 *  Outputs:
 *
 *	fab and rab are modified if file is opened.
 *
 *  Returns:
 *
 *	RMS status
 *
 */
unsigned long int
open_file_rms (struct FAB *infab, struct RAB *inrab, char *buff, char *filename)
{
    unsigned long int rms_status;

    *infab = cc$rms_fab;			/* Initialize the FAB */
    *inrab = cc$rms_rab;			/* Initialize the RAB */
    infab->fab$b_fns = strlen(filename);	/* Set filename length */
    infab->fab$l_fna = filename;		/* Set filename address */
    infab->fab$b_fac = FAB$M_GET;		/* GET access only */
    infab->fab$b_shr = FAB$M_SHRGET+FAB$M_SHRPUT+FAB$M_SHRUPD;
    inrab->rab$l_fab = infab;			/* Let RAB point to FAB */
    inrab->rab$b_rac = RAB$C_SEQ;		/* Sequential file access */
    inrab->rab$w_usz = 512;			/* Record size is 512 bytes */
    inrab->rab$l_ubf = buff;			/* Read to this buffer */

    rms_status = SYS$OPEN (infab);		/* Open the file */
    if (!(rms_status & 1))			/* If an error occurs, return */
	return (rms_status);			/* ... a status */
    rms_status = SYS$CONNECT (inrab);		/* Connect the RAB */
    return (rms_status);			/* Return the RMS status */
}

/*
 *
 *  Function:	init_sdesc
 *
 *  Functional description:
 *
 *	Initialize a static string descriptor.
 *
 *  Inputs:
 *
 *	sdesc	- Address of the descriptor to initialize
 *		  (of type struct dsc$descriptor_s)
 *	string	- Address of null-terminated string the descriptor describes
 *
 *  Outputs:
 *
 *	sdesc	- Descriptor passed as sdesc is initialized
 *
 */
void
init_sdesc (struct dsc$descriptor_s *sdesc, char *string)
{
    sdesc->dsc$w_length = strlen(string);	/* Set the length	*/
    sdesc->dsc$b_dtype = DSC$K_DTYPE_T;		/* Type is text		*/
    sdesc->dsc$b_class = DSC$K_CLASS_S;		/* Class is static	*/
    sdesc->dsc$a_pointer = string;		/* Point to the string	*/
}

/*
 *
 *  Function:	add_to_bulletin_folder
 *
 *  Functional description:
 *
 *	Adds a message to a BULLETIN folder by calling the external
 *	BULLETIN routines INIT_MESSAGE_ADD, WRITE_MESSAGE_LINE, and
 *	FINISH_MESSAGE_ADD.
 *
 *	The following constants are (may be) passed to INIT_MESSAGE_ADD:
 *
 *		Subject = "" 	Causes BULLETIN to scan RFC822 headers for
 *				a "Subject:" or "Subj:" line
 *		From = "MX%"	Causes BULLETIN to scan RFC822 headers for
 *				a "Reply-to:" or "From:" line
 *
 *  Inputs:
 *
 *	filerab	- Address of the message file's RAB
 *	folder	- Address of a string descriptor for the name of the folder
 *	from	- Address of a string descriptor for the "From:" address
 *
 *  Outputs:
 *
 *	None.
 *
 *  Returns:
 *
 *	unsigned long int - RMS status of call to INIT_MESSAGE_ADD
 *
 */
unsigned long int
add_to_bulletin_folder(struct RAB *filerab, void *folder, void *from)
{
    unsigned long int bull_status;	/* Status from INIT_MESSAGE_ADD */
    struct dsc$descriptor_s msg_line;	/* Descriptor for a line of the msg */
    static $DESCRIPTOR(subject,"");	/* Subject is "" */

    /* Call BULLETIN routine to initialize adding the message */

    INIT_MESSAGE_ADD (folder, from, &subject, &bull_status);

    if (!(bull_status & 1)){					/* Error? */
	return(bull_status);
    }

    /*	Loop reading message lines until end-of-file.  For each line read,
	create a string descriptor for it and call the BULLETIN routine to
	add the line. */

    while (rms_get(filerab) != RMS$_EOF){		/* Loop until EOF */
	filerab->rab$l_rbf[filerab->rab$w_rsz] = 0;	/* End byte = NULL */
	init_sdesc(&msg_line, filerab->rab$l_rbf);	/* Now build desc. */
	WRITE_MESSAGE_LINE (&msg_line);			/* Add to BULLETIN */
    }

    FINISH_MESSAGE_ADD();		/* Call BULLETIN routine to finish */

    tracemsg("Message added to folder");
    return(SS$_NORMAL);			/* Return success to caller */
}


/*
 *
 *  Function:	scan_for_from_line
 *
 *  Functional description:
 *
 *	The routine scans the message's RFC822 headers for the "From:" line.
 *	It parses out the address by extracting the <address>.
 *
 *	This routine was necessary because letting BULLETIN find the "From:"
 *	line was resulting in a non-RESPONDable address for MX.  For example,
 *	BULLETIN was creating:
 *
 *		From: MX%"Hunter Goatley, WKU <goathunter@WKUVX1.BITNET>"
 *
 *	but MX needs
 *
 *		From: MX%"<goathunter@WKUVX1.BITNET>"
 *
 *  Inputs:
 *
 *	filerab	- Address of the message file's RAB
 *
 *  Outputs:
 *
 *	final_from - Address of a character buffer to receive the final address
 *
 *  Returns:
 *
 *	unsigned long int - binary success/failure status
 *
 *  Side effects:
 *
 *	The message file is rewound so that subsequent GETs start at the
 *	beginning of the message.
 *
 */
unsigned long int
scan_for_from_line(struct RAB *filerab, char *final_from)
{
    unsigned long int scan_status;	/* Status from INIT_MESSAGE_ADD */
    struct dsc$descriptor_s msg_line;	/* Descriptor for a line of the msg */
    char whole_from_line[512];		/* The assembled "From:" line */
    char *filebuffer;			/* Pointer to the input buffer */
    int i, j, x;			/* Work variables */

    scan_status = SS$_NORMAL;			/* Assume success */
    whole_from_line[0] = '\0';			/* Initialize work buffer */

    /*	Loop reading message lines until end-of-file or first null line,
	which should signal the end of the RFC822 header.  For each line read,
	check to see if we've located the "From:" line.
    */

    filebuffer = filerab->rab$l_ubf;			/* Init buffer ptr */
    while ((rms_get(filerab) != RMS$_EOF) &&		/* Loop until EOF */
	   ((x = filerab->rab$w_rsz) != 0)){		/* or null record */
	filebuffer[x] = '\0';				/* Set NULL byte */
	if (strncmp(filebuffer,"From:",5)==0){		/* Is it the "From:"? */

	   /* Found "From:" line */
	   tracemsg("Found \042From:\042 line in RFC822 header");
	   strcpy(whole_from_line,filebuffer);		/* Copy to work buff */

	   /* The "From:" line may actually be split over several lines.
	      In such cases, the remaining lines are indented by 6 spaces.
	      To handle this, loop reading records until one is read that
	      doesn't begin with a blank.  As each record is read, it is
	      trimmed and tacked on to whole_from_line, so we end up with
	      the entire "From:" line in one buffer.  */

	   while((rms_get(filerab) != RMS$_EOF) &&	/* Read rest of From: */
		 (filebuffer[0] == ' ')){		/* ... line */
	      for (i = 0; filebuffer[i] == ' '; ++i);	/* Step over blanks */
	      strcat(whole_from_line,&filebuffer[i]);	/* Tack it on end */
	   }

	   /* Now have the whole "From:" line in whole_from_line.  Since
	      the real address is enclosed in "<>", look for it by
	      searching for the last "<" and reading up to the ">".  */

	   i = strrchr(whole_from_line,'<');		/* Find last "<" */
	   if (i != 0){					/* Found it.... */
		j = strchr(i,'>');			/* Find last ">" */
	        j = j-i+1;				/* Calc addr length */
	   }
	   else{
		j = strlen(whole_from_line)-6;		/* Don't count From: */
		i = &whole_from_line + 6;		/* in string length */
	   }
	   if (j < 0){					/* If neg., error */
		tracemsg("Error - unable to locate from address");
		strcpy(final_from,"");			/* Return null string */
		scan_status = 0;			/* Set error status */
	   }
	   else {
		tracemsg("Found sender's address in RFC822 header");
		strncpy(final_from, i, j);		/* Copy to caller */
	   }	
	}
    }

    SYS$REWIND(filerab);		/* Rewind the file to the beginning */
    return(scan_status);		/* Return success to caller */
}


/*
 *
 *  Function:	forward_to_postmaster
 *
 *  Functional description:
 *
 *	If an error occurs trying to write a message to a BULLETIN folder,
 *	this routine is called to forward the message to the local
 *	postmaster.
 *
 *  Inputs:
 *
 *	filerab	- Address of the message file's RAB
 *	folder	- Address of a string descriptor for the name of the folder
 *	from	- Address of a string descriptor for the "From:" address
 *	status	- Address of longword containing the BULLETIN error code
 *
 *  Outputs:
 *
 *	None.
 *
 *  Returns:
 *
 *	unsigned long int - binary status of call to INIT_MESSAGE_ADD
 *
 *  Side effects:
 *
 *	The message file is rewound so that subsequent calls to this routine
 *	can be made (in case the message is to be written to several folders).
 *
 */
unsigned long int
forward_to_postmaster(struct RAB *filerab, void *folder, void *from, int status)
{
    struct dsc$descriptor_s msg_line;	/* Descriptor for a line of the msg */
    struct dsc$descriptor_s subject;
    char subject_buf[256];
    char postmaster[256];   int postmaster_len;
    char status_msg_buf[256];   int status_msg_len;
    struct dsc$descriptor_s status_msg;
    static $DESCRIPTOR(faostr,"Failed BULLETIN message for folder !AS");
    static $DESCRIPTOR(MXBULL,"MX->SITE (BULLETIN delivery)");
    static $DESCRIPTOR(postmaster_lnm,"MX_BULLETIN_POSTMASTER");
    int send_context = 0;  int x;  int y;

    static char *error_msgs[] = {
	{"Error delivering message to BULLETIN folder.  BULLETIN error status:"},
	{""},
	{""},
	{"Original message text follows:"},
	{"--------------------------------------------------"}
    };

    trnlnm_itmlst[0].buffer_length = 255;
    trnlnm_itmlst[0].buffer_address = &postmaster;
    trnlnm_itmlst[0].return_length_address = &postmaster_len;

    SYS$TRNLNM( 0, &lnm_table, &postmaster_lnm, 0, trnlnm_itmlst);
    if (postmaster_len == 0)		/* If logical is not defined, */
	return(SS$_NORMAL);		/* then pretend it worked     */

    tracemsg("Forwarding message to local postmaster....");
    subject.dsc$w_length = 255;
    subject.dsc$a_pointer = &subject_buf;
    SYS$FAO(&faostr, &subject, &subject, folder);	/* Format the subject */

    address_itmlst[0].buffer_length = postmaster_len;		   /* To: */
    address_itmlst[0].buffer_address = &postmaster;		   /* To: */
    attribute_itmlst[0].buffer_length = postmaster_len;		   /* To: */
    attribute_itmlst[0].buffer_address = &postmaster;		   /* To: */
    attribute_itmlst[1].buffer_length = MXBULL.dsc$w_length;	   /* From: */
    attribute_itmlst[1].buffer_address = MXBULL.dsc$a_pointer;	   /* From: */
    attribute_itmlst[2].buffer_length = subject.dsc$w_length;	   /* Subject:*/
    attribute_itmlst[2].buffer_address = subject.dsc$a_pointer;	   /* Subject:*/

    vms_errchk(mail$send_begin(&send_context, &nulllist, &nulllist));
    vms_errchk(mail$send_add_address(&send_context, &address_itmlst,
			&nulllist));
    vms_errchk(mail$send_add_attribute(&send_context, &attribute_itmlst,
			&nulllist));

    for (x = 0; x < 5; x++){
	bodypart_itmlst[0].buffer_length = strlen(error_msgs[x]);
	bodypart_itmlst[0].buffer_address = error_msgs[x];
	vms_errchk(mail$send_add_bodypart(&send_context,
		&bodypart_itmlst, &nulllist));
	if (x == 1){
	  status_msg.dsc$w_length = 256;
	  status_msg.dsc$b_dtype = DSC$K_DTYPE_T;
	  status_msg.dsc$b_class = DSC$K_CLASS_S;
	  status_msg.dsc$a_pointer = &status_msg_buf;
	  y = SYS$GETMSG (status, &status_msg, &status_msg, 15, 0);
	  if (!(y & 1))
	     sprintf(status_msg_buf,"Error code is %%X%08x",status);
	  else
	     status_msg_buf[status_msg.dsc$w_length] = '\0';
	  bodypart_itmlst[0].buffer_length = strlen(status_msg_buf);
	  bodypart_itmlst[0].buffer_address = &status_msg_buf;
	  vms_errchk(mail$send_add_bodypart(&send_context,&bodypart_itmlst,
		&nulllist));
	}
    }

    while (rms_get(filerab) != RMS$_EOF){		/* Loop until EOF */
	bodypart_itmlst[0].buffer_length = filerab->rab$w_rsz;
	bodypart_itmlst[0].buffer_address = filerab->rab$l_rbf;
	vms_errchk(mail$send_add_bodypart(&send_context,
		&bodypart_itmlst, &nulllist));
    }

    vms_errchk(mail$send_message(&send_context, &nulllist, &nulllist));
    vms_errchk(mail$send_end(&send_context, &nulllist, &nulllist));

    tracemsg("Message forwarded to postmaster....");
}


/*
 *
 *  Function:	log_accounting
 *
 *  Functional description:
 *
 *	This routine will write an accounting record for the message.
 *
 *  Inputs:
 *
 *	folder	- Address of a string descriptor for the name of the folder
 *	from	- Address of a string descriptor for the "From:" address
 *	status	- Address of longword containing the BULLETIN error code
 *
 *  Outputs:
 *
 *	None.
 *
 *  Returns:
 *
 *	unsigned long int - RMS status
 *
 */
unsigned long int
log_accounting(void *folder, void *from, int bull_status)
{
    struct FAB accfab;
    struct RAB accrab;
    static $DESCRIPTOR(MX_BULL_ACCNTNG,"MX_BULLETIN_ACCNTNG");
    static $DESCRIPTOR(faostr,
	"!%D MX_BULL: FOLDER=\042!AS\042, ORIGIN=\042!AS\042, STATUS=%X!XL");
    char outbufbuf[256];
    struct dsc$descriptor_s outbuf = {256, DSC$K_DTYPE_T, DSC$K_CLASS_S,
		 &outbufbuf};

    int status;
    static char bullacc[] = "MX_BULLETIN_ACC";
    static char bullaccdef[] = "MX_SITE_DIR:.DAT";

    status = SYS$TRNLNM( 0, &lnm_table, &MX_BULL_ACCNTNG, 0, 0);
    if (!(status & 1))
	return(SS$_NORMAL);

    tracemsg("Writing accounting information to accounting log....");
    accfab = cc$rms_fab;
    accrab = cc$rms_rab;
    accfab.fab$b_fns = strlen(bullacc);		/* Set filename length */
    accfab.fab$l_fna = &bullacc;		/* Set filename address */
    accfab.fab$b_dns = strlen(bullaccdef);	/* Set filename length */
    accfab.fab$l_dna = &bullaccdef;		/* Set filename address */
    accfab.fab$b_fac = FAB$M_PUT;		/* PUT access only */
    accfab.fab$b_shr = FAB$M_SHRGET+FAB$M_SHRPUT+FAB$M_SHRUPD;
    accfab.fab$b_rfm = FAB$C_VAR;		/* Variable length records */
    accfab.fab$b_rat = FAB$M_CR;		/* Normal "text" rat */
    accrab.rab$l_fab = &accfab;			/* Let RAB point to FAB */
    accrab.rab$b_rac = RAB$C_SEQ;		/* Sequential file access */

    status = SYS$OPEN (&accfab);		/* Try to open the file */
    if (status & 1)				/* Success? */
	accrab.rab$l_rop = RAB$M_EOF;		/* Set to EOF */
    else					/* Couldn't open, so create */
	status = SYS$CREATE (&accfab);		/* ... a new one */
    if (status & 1){				/* If either was OK... */
	status = SYS$CONNECT (&accrab);		/* Connect the RAB */
	if (status == RMS$_EOF)			/* RMS$_EOF status is OK */
	   status = RMS$_NORMAL;		/* Change it to NORMAL */
	if (!(status & 1)){			/* If any error occurred */
	   tracemsg("Unable to open accounting file");
	   traceerr(status);
	   SYS$CLOSE (&accfab);			/* Close the file */
	   return(status);			/* And return the error */
	}
    }
    else
	return(status);

    SYS$FAO(&faostr, &outbuf, &outbuf, 0, folder, from, bull_status);
    accrab.rab$w_rsz = outbuf.dsc$w_length;
    accrab.rab$l_rbf = outbuf.dsc$a_pointer;
    SYS$PUT (&accrab);
    SYS$CLOSE (&accfab);
}

/*
 * 
 *  Main routine
 *
 */
main(int argc, char *argv[])
{
  struct dsc$descriptor_s folder;	/* Descriptor for the folder name */
  struct dsc$descriptor_s from_user;	/* Descriptor for "From:" line */
  static $DESCRIPTOR(MX_SITE_DEBUG,"MX_SITE_DEBUG");

  char *from_line;			/* Pointer to dynamic "From:" buffer */
  char *folder_name;			/* Pointer to folder name in rcptbuf */
  char *atsign;				/* Pointer to "@" in rcptbuf */
  int  x;				/* Work variable */
  unsigned long int bull_status;	/* Status from add_to_bulletin_folder */

  --argc;				/* Don't count the program name */
  if ((argc != 2) && (argc != 3)) {	/* If too many or too few args, */
    exit(LIB$_WRONUMARG);		/* ...  exit with error status  */
  }

  vms_status = SYS$TRNLNM( 0, &lnm_table, &MX_SITE_DEBUG, 0, 0);
  if (vms_status & 1)
    trace = 1;
  else
    trace = 0;

  /*  Open all input files  */

  tracemsg("Opening message file....");
  vms_errchk(open_file_rms (&msgfab, &msgrab, &msgbuf, argv[1]));
  tracemsg("Opening recipients file....");
  vms_errchk(open_file_rms (&rcptfab, &rcptrab, &rcptbuf, argv[2]));

  if (argc == 2){
     tracemsg("Using sender address from RFC822 headers....");
     scan_for_from_line(&msgrab, &frombuf);
  }
  else {
     tracemsg("Opening sender address file....");
     vms_errchk(open_file_rms (&fromfab, &fromrab, &frombuf, argv[3]));

     tracemsg("Reading sender address from file....");
     rms_get(&fromrab);			/* Read the from line */
     if (!(rms_status & 1))		/* Exit if an error occurred */
	err_exit(rms_status);

     /* Set the end of the record read, then initialize the descriptor for it */
     frombuf[fromrab.rab$w_rsz] = 0;

     SYS$CLOSE(&fromfab);
  }						/* End of "if (argc == 2)"... */

  /* frombuf now has the sender's address in it */

  if (strlen(frombuf) == 0) {
	tracemsg("Unable to find sender's address, using MX%");
	init_sdesc(&from_user, "MX%");
  }
  else{

     /* Now add the MX% prefix and the double quotes */
     from_line = malloc(4 + strlen(frombuf) + 1 + 1);	/* Allocate memory */

     /* Make the string repliable through MX by adding MX%"" to it */
     strcpy(from_line,"MX%\042");
     strcat(from_line,frombuf);
     strcat(from_line,"\042");
     if (trace)
	printf("MX_BULL: Sender's address is %s\n", from_line);
     init_sdesc (&from_user, from_line);	/* Create a string descriptor */
  }
  /*
    Read through all the recipients, writing the message to all BULLETIN
    folders (identified by checking for @BULLETIN in the address).
  */
  rms_get(&rcptrab);				/* Read a recipient */
  while ((rms_status & 1) & (rms_status != RMS$_EOF)){
     tracemsg("Looking for BULLETIN folder....");
     folder_name = &rcptbuf;			/* Point to receipt buffer */
     if (folder_name[0] == '<'){		/* If line begins with "<" */
	++folder_name;				/*  bump over it and check */
	atsign = strchr(rcptbuf,'@');		/*  for a "@"		   */
	if (atsign != 0){			/* If "@" was found,	   */
	  if (strncmp(atsign,"@BULLETIN",9)==0){/* Is it @BULLETIN?	   */
	    x = atsign - folder_name;		/* Length of folder name   */
	    folder_name[x] = 0;			/* Terminate folder name   */
	    init_sdesc (&folder, folder_name);	/* Initialize descriptor   */
	    str$upcase(&folder, &folder);	/* Convert to uppercase    */
	    if (trace)
		printf("MX_BULL: Found BULLETIN folder \042%s\042....\n",
			folder_name);
	    tracemsg("Adding message to BULLETIN folder....");
	    bull_status = add_to_bulletin_folder (&msgrab, &folder, &from_user);
	    if (!(bull_status & 1)){
		 traceerr(bull_status);
		 vms_errchk(forward_to_postmaster(&msgrab, &folder, &from_user,
				bull_status));
	    }
	    log_accounting(&folder, &from_user, bull_status);
	    SYS$REWIND(&msgrab);	/* Rewind the file for next folder */L

	  }
	}
      }
      rms_get(&rcptrab);		/* Read next recipient */e
  }u


  /* Close the RMS files */r

  SYS$CLOSE(&msgfab);  SYS$CLOSE(&rcptfab);f

  tracemsg("BULLETIN message processed");y
  exit(SS$_NORMAL);		/* Always return success */

}L
$eod t
$copy/log sys$input MX_BULL.TXTm
$deckM
                                    MX_BULL_
                             An MX SITE transport.
                                March 14, 1991

MX_BULL is a transport between MX and BULLETIN, a VMS bulletin board program
by Mark London at MIT.  It is designed to be called as an MX SITE transport,
letting MX write messages into BULLETIN folders as they are processed, instead
of routing the messages to MAIL.MAI files for each folder.

The following files make up the MX_BULL distribution:-

   BUILD_MX_BULL.COM		Command procedure to build MX_BULL.EXE
   MX_BULL.C			VAX C source code for MX_BULL
   MX_BULL.TXT			This file
   MX_BULL_SITE_DELIVER.COM	SITE_DELIVER.COM for MX_BULL

The current version is 01-001.


WHAT IS BULLETIN?h
----------------- 
BULLETIN is a VMS bulletin board written by Mark London at MIT that allows
multiple users to access a common message base.  Messages are divided into
folders, which work much like VMS Mail folders.  Using MX_BULL, messages can
be routed from Internet/Bitnet mailing lists directly to BULLETIN folders,
allowing all (or some) users on a system to access the mailing lists without
individual subscriptions.  This can cut down on the number of incoming
Bitnet/Internet mail messages significantly, since only one copy of a message 
need be sent to a site.t

BULLETIN can be found on a number of the DECUS VAX SIG tapes, including theS
Fall 1990 tapes.  It can also be retrieved by sending a mail message to 
BULLETIN@NERUS.PFC.MIT.EDU.  The body of the message must contain one of
the following commands:n

        SEND ALL        Sends all bulletin files..
        SEND filename   Sends the specified file.e
        BUGS            Sends a list of the latest bug fixes.D
        HELP or INFO    Sends a brief description of BULLETIN.


BUILDING MX_BULL.EXE
--------------------
MX_BULL is written in VAX C and can be compiled by executing BUILD_MX_BULL.COM.

MX_BULL must be linked with the BULLETIN object library, BULL.OLB.  Thei
build procedure for MX_BULL expects the logical BULL_SOURCE to point to theo
BULLETIN library.  You must define this logical (or edit the .COM file)m
before building MX_BULL.


INSTALLING MX_BULL
------------------
To install MX_BULL, perform the following steps:

1.  Using MCP, define a path named BULLETIN as a SITE transport:

	MCP> DEFINE PATH "BULLETIN" SITE 

2.  Using MCP, define a rewrite rule early in the list (this should actually
    be done using CONFIG.MCP so that the order is correct):L

	MCP> DEFINE REWRITE_RULE "<{folder}@BULLETIN>" "<{folder}@BULLETIN>"*

3.  If you don't have a SITE transport already defined, simply copyu
    MX_BULL_SITE_DELIVER.COM to MX_EXE:SITE_DELIVER.COM.

    If you do have a SITE transport defined, you'll need to merge the MX_BULLe
    stuff into the existing MX_EXE:SITE_DELIVER.COM.

4.  Reset the MX routers by using MCP RESET/ALL, or shutting down MX and
    restarting it.

Once these steps have been completed, MX_BULL is set up to begin deliveringc
messages to BULLETIN.t


ROUTING MESSAGES TO BULLETIN
----------------------------
Messages are routed to BULLETIN folders by addressing mail tod
MX%"folder@BULLETIN", where "folder" is the name of the target BULLETINm
folder.  For example, the following commands would send a message from VMS
Mail to the BULLETIN folder GENERAL (on the local system):

	$ MAIL)
	MAIL> SENDU
	To:     MX%"GENERAL@BULLETIN"
	Subj:   This is a test....a
	.....

The message is sent to the MX router, which in turn sends it to the MX SITEu
agent, since the @BULLETIN path was defined as a SITE path.f

To facilitate the automatic delivery of messages to BULLETIN folders, youi
should set up forwarding addresses for each of the BULLETIN folders:

	MAIL> SET FORWARD/USER=GENERAL MX%"""GENERAL@BULLETIN"""/
	MAIL> SET FORWARD/USER=MX-LIST MX%"""MX-LIST@BULLETIN"""t

Mail addressed to GENERAL or MX-LIST will automatically be forwarded to/
BULLETIN via MX_BULL.d

To subscribe to a Bitnet/Internet mailing list and have the messages delivered
to BULLETIN, use MX's MLFAKE to send a subscription request on behalf of the
BULLETIN folder.  For example, the user to specify would be:

	MLFAKE/USER=MX-LIST ..../

(Alternatively, you could create a dummy account named MX-LIST (or whatever 
the list name is) that exists only long enough to send the request via MAIL.)u

Once added to the lists, incoming mail addressed to MX-LIST will get forwarded
to MX%"MX-LIST@BULLETIN", which will invoke MX_BULL.  For example, an incoming
message to my local BULLETIN folder would be addressed to:

	MX-LIST@WKUVX1.bitnet

Since I have MX-LIST forwarded to MX%"MX-LIST@BULLETIN", the message is routed
to the BULLETIN folder.M

To try to illustrate the process, assume the node is WKUVX1.bitnet.  We've
subscribed a fake local user, INFO-VAX, to the MX mailing list; mail forwardingm
has been set up for INFO-VAX to send it to MX%"INFO-VAX@BULLETIN".  When maild
arrives addressed to INFO-VAX@WKUVX1.BITNET, the MX Router passes the messagee
to the Local agent, which discovers that the mail is forwarded tos
MX%"INFO-VAX@BULLETIN".  The message is then sent back to the Router, which 
finds that BULLETIN is defined as a SITE path, so the message is passed to
MX->SITE, which in turn calls MX_BULL.


MX_BULL ACCOUNTING AND DEBUGGING
--------------------------------
MX_BULL accounting is enabled with the system logical MX_BULLETIN_ACCNTNG:

	$ DEFINE/SYS/EXEC MX_BULLETIN_ACCNTNG TRUEf

This will cause MX_BULL to create MX_SITE_DIR:MX_BULLETIN_ACC.DAT.  The;
logical MX_BULLETIN_ACC can be defined system-wide to change the name of the
file: 

	$ DEFINE/SYS/EXEC MX_BULLETIN_ACC LOCALDISK:[DIR]MX_BULL.ACCOUNTING

To generate debugging logs in MX_SITE_DIR:, define the system logicalf
MX_SITE_DEBUG.


ERRORS WRITING TO BULLETIN
--------------------------
By default, MX_BULL_SITE_DELIVER.COM always returns success to the MX SITE
agent.  This was done to avoid bouncing network mail back to a mailing list.
In order to be notified in case of problems writing the message to BULLETIN,
you can define a system logical MX_BULLETIN_POSTMASTER to be a local
username to receive failed MX_BULL transactions:

	$ DEFINE/SYS/EXEC MX_BULLETIN_POSTMASTER GOATHUNTER

If BULLETIN returns an error, MX_BULL will forward the message (via the 
callable VMS Mail interface) to GOATHUNTER.	


BULLETIN AND "From:" ADDRESSES
------------------------------
If you use the return address supplied by the MX SITE agent, the return addressr
for BULLETIN messages will look something like the following:t

	From: MX%"@WKUVX1.BITNET:I-AMIGA@UBVM.BITNET"

By default, MX_BULL_SITE_DELIVER.COM is set up to ignore the sender's address.
If you want to use the MX SITE-supplied address, simply modify the following
line in MX_BULL_SITE_DELIVER.COM:s

	$ USE_SITE_FROM = 0	!Change to 1 to use MX sender's address

If the sender's address is ignored (again, the default), MX_BULL will search
the RFC822 headers in the message for the "From:" line.  It then pulls out
the sender's address in a format suitable for using the RESPOND command in
BULLETIN.  This lets users easily RESPOND to the sender of a message, or
POST a message to the list itself.

Note: MX_BULL just uses the address it's given.  Some addresses are gatewayedW
to death, leaving a bad address on the "From:" line.  This frequently happensa
with messages coming via UUCP through Internet to Bitnet, etc.


AUTHOR INFORMATION
------------------
MX_BULL was written by:b

	Hunter Goatley, VMS Systems Programmer, WKU

	E-mail: goathunter@wkuvx1.bitnety
	Voice:	502-745-5251

	U.S. Mail:	Academic Computing, STH 226s
			Western Kentucky University
			Bowling Green, KY 42101
$eod t
$copy/log sys$input MX_BULL_SITE_DELIVER.COM
$decka
$!
$!  SITE_DELIVER.COM for MX_BULL
$!
$!  Author:	Hunter Goatley, goathunter@wkuvx1.bitnet
$!  Date:	March 11, 1991
$!
$!  By default, MX_BULL will tell BULLETIN to search the RFC822 headersb
$!  in the message for a "Reply-to:" or "From:" line.  If you want MX_BULL
$!  to use the P3 as the "From:" line, simply set USE_SITE_FROM to 1.u
$!
$ USE_SITE_FROM = 0				!Change to 1 to use P3i
$ mxbull :== $mx_exe:mx_bull.exe
$!
$ set noon
$ if f$trnlnm("SYS$SCRATCH").eqs."" then define SYS$SCRATCH MX_SITE_DIR:
$ if USE_SITE_FROM				!Use P3 as "From:"?(
$ then	create mx_site_dir:sitesender.addr;	!If so, write it out to a file/
$	open/append tmp mx_site_dir:sitesender.addr;	!... to make sure DCL
$	write tmp p3				!... doesn't mess it upn
$	close tmp				!...t
$	mxbull 'p1' 'p2' mx_site_dir:sitesender.addr
$	delete/nolog mx_site_dir:sitesender.addr;s
$ else	mxbull 'p1' 'p2'			!Just let BULLETIN find "From:"b
$ endifb
$ exit 1	!Always return success 
$eod N
