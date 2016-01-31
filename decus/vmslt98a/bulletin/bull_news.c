#include <string.h>
#include <descrip.h>
#include <stdio.h>
#include "sys$library:iodef.h"

#if MULTINET

#include "multinet_root:[multinet.include.sys]types.h"
#include "multinet_root:[multinet.include.sys]socket.h"
#include "multinet_root:[multinet.include.netinet]in.h"
#include "multinet_root:[multinet.include.arpa]inet.h"
#include "multinet_root:[multinet.include]netdb.h"
#include "multinet_root:[multinet.include]errno.h"
#include "multinet_root:[multinet.include.vms]inetiodef.h"

static char inet[7] = "INET0:";
$DESCRIPTOR(inet_d,inet);

static struct dns {
	unsigned char function;
	unsigned char call_code;
	short zeros;
	short length;
	char string[512];
} buf1, buf2;

struct  sockaddr_un {
        short   sun_family;             /* AF_UNIX */
        char    sun_path[109];          /* path name (gag) */
};
#else

#if UCX

#include <ucx$inetdef.h>

struct sockaddr {
  short inet_family;
  short inet_port;
  int inet_adrs;
  char bklb[8];
  };

struct itlist { int lgth; struct sockaddr *hst; };

static short sck_parm[2];
static struct sockaddr local_host, remote_host;
struct itlist lhst_adrs, rhst_adrs;

static char ucxdev[11] = "UCX$DEVICE";
$DESCRIPTOR(ucxdev_d,ucxdev);

static int addr_buff;

#define htons(x) ((unsigned short)((x<<8)|(x>>8)))

#else

#if TWG

#include <types.h>
#include <socket.h>
#include <netdb.h>
#include <in.h>
#include <inetiodef.h>

static char inet[6] = "INET:";
$DESCRIPTOR(inet_d,inet);

#else

#define CMU 1
static char ip[4] = "IP:";
$DESCRIPTOR(ip_d,ip);

#endif

#endif

#endif

static char task[20];
$DESCRIPTOR(task_d,task);

static int s;

static struct iosb {
	short status;
	short size;
	int info;
} iosb;

#define TCP 0
#define DECNET 1

static int mode = TCP;

#if MULTINET

#include <lib$routines>
#include <stdarg.h>
#ifdef __ALPHA
unsigned int __VA_COUNT_BUILTIN(void);
#define va_count(count)		(count = __VA_COUNT_BUILTIN())
#else
#ifdef VAXC
#define va_count(n) vaxc$va_count(&n)
extern int vaxc$va_count();
#else
#define va_count(n) decc$va_count(&n)
extern int decc$va_count();
#endif
#endif

static int FindRoutine(struct dsc$descriptor *image,
		       struct dsc$descriptor *routine, int (**rtn)());

int inet_ntoa1(int *arg1)
{
  static $DESCRIPTOR(image,"MULTINET_SOCKET_LIBRARY");
  static $DESCRIPTOR(routine,"inet_ntoa");
  int arglist[255];
  int i;
  static int status;
  static int (*rtn)() = 0;
  va_list ap;
  va_count(arglist[0]);
  va_start(ap, arg1);
  arglist[1] = *arg1;
  for (i=1;i<arglist[0];i++)
    arglist[i+1] = va_arg(ap, int);
  if (!rtn)
  {
    status = FindRoutine((struct dsc$descriptor *)&image,
			 (struct dsc$descriptor *)&routine,&rtn);
    if (!(status & 1))
      rtn = (int (*)())1;
  }
  if ((int)rtn != 1)
    status = lib$callg(arglist,rtn);
  return status;
}

int gethostname1(int arg1,int arg2)
{
  static $DESCRIPTOR(image,"MULTINET_SOCKET_LIBRARY");
  static $DESCRIPTOR(routine,"gethostname");
  int arglist[255];
  int i;
  static int status;
  static int (*rtn)() = 0;
  va_list ap;
  va_count(arglist[0]);
  va_start(ap, arg1);
  arglist[1] = arg1;
  for (i=1;i<arglist[0];i++)
    arglist[i+1] = va_arg(ap, int);
  if (!rtn)
  {
    status = FindRoutine((struct dsc$descriptor *)&image,
			 (struct dsc$descriptor *)&routine,&rtn);
    if (!(status & 1))
      rtn = (int (*)())1;
  }
  if ((int)rtn != 1)
    status = lib$callg(arglist,rtn);
  return status;
}

int htons1(int arg1)
{
  static $DESCRIPTOR(image,"MULTINET_SOCKET_LIBRARY");
  static $DESCRIPTOR(routine,"htons");
  int arglist[255];
  int i;
  static int status;
  static int (*rtn)() = 0;
  va_list ap;
  va_count(arglist[0]);
  va_start(ap, arg1);
  arglist[1] = arg1;
  for (i=1;i<arglist[0];i++)
    arglist[i+1] = va_arg(ap, int);
  if (!rtn)
  {
    status = FindRoutine((struct dsc$descriptor *)&image,
			 (struct dsc$descriptor *)&routine,&rtn);
    if (!(status & 1))
      rtn = (int (*)())1;
  }
  if ((int)rtn != 1)
    status = lib$callg(arglist,rtn);
  return status;
}

int gethostbyname1(int arg1)
{
  static $DESCRIPTOR(image,"MULTINET_SOCKET_LIBRARY");
  static $DESCRIPTOR(routine,"gethostbyname");
  int arglist[255];
  int i;
  static int status;
  static int (*rtn)() = 0;
  va_list ap;
  va_count(arglist[0]);
  va_start(ap, arg1);
  arglist[1] = arg1;
  for (i=1;i<arglist[0];i++)
    arglist[i+1] = va_arg(ap, int);
  if (!rtn)
  {
    status = FindRoutine((struct dsc$descriptor *)&image,
			 (struct dsc$descriptor *)&routine,&rtn);
    if (!(status & 1))
      rtn = (int (*)())1;
  }
  if ((int)rtn != 1)
    status = lib$callg(arglist,rtn);
  return status;
}

static int FindRoutine(struct dsc$descriptor *image,
		       struct dsc$descriptor *routine, int (**rtn)())
{
  lib$establish(lib$sig_to_ret);
  return lib$find_image_symbol(image,routine,rtn);
}
#endif

news_get_chan()
{return(s);}

news_set_chan(i)
int *i;
{s = *i;}

news_disconnect()
{
#if UCX
	sys$cancel(s);
	sys$qiow(0,s,IO$_DEACCESS,0,0,0,0,0,0,0,0,0);
#endif
	sys$dassgn(s);
}

#if MULTINET || TWG

static struct hostent *hp;
static struct sockaddr_in sin;

#endif

int *node;

news_gethost()
{
	/*
	 *  Get the IP address of the NEWS host.
	 *  As of MULTINET 3.0, cannot be done at AST level
	 *  so can't do in NEWS_ASSIGN(), as BULLCP calls it at
	 *  AST level if the decnet gateway feature is used.
	 */
#if TWG
	struct hostent *gethostbyname();
#else
#if MULTINET
#endif
#endif

	node = getenv("BULL_NEWS_SERVER");
	if (!node) return(0);
	if (!strchr(node,'.')) return(1); 

#if TWG
	hp = gethostbyname(node);
#else
#if MULTINET
	hp = gethostbyname1(node);
#endif
#endif
	return(1);
}

news_assign()
{
	int n;

	if (!strchr(node,'.')) {
	   strcpy(&task[0],node);
	   n = strlen(node);
	   strcpy(&task[n],"::\"TASK=NNTP\"");
	   task_d.dsc$w_length = 13 + n;
	   if (!(sys$assign(&task_d,&s,0,0) & 1)) return(0);
	   mode = DECNET;
	   return(1);
	}
#if MULTINET || TWG
	/*
	 *  Create a "sockaddr_in" structure which describes the remote
	 *  IP address we want to send to (from gethostbyname()).
	 */

        if (!hp) {
          int h[4],i;
          if (sscanf(node,"%d.%d.%d.%d",&h[0],&h[1],&h[2],&h[3]) == 4) {
            for (i=0;i<4;i++) if (h[i] < 0 || h[i] > 255) return(0);
	    sin.sin_addr.s_addr = (h[3]<<24)+(h[2]<<16)+(h[1]<<8)+(h[0]);
	  } else
	    return(0);
	  sin.sin_family = AF_INET;
	}
        else {
 	  sin.sin_family = hp->h_addrtype;
	  memcpy(&sin.sin_addr, hp->h_addr, hp->h_length);
        }
#if TWG
	sin.sin_port = htons(119);
#else
	sin.sin_port = htons1(119);
#endif

	/*
	 *  Create an IP-family socket on which to make the connection
	 */

	if (!(sys$assign(&inet_d,&s,0,0) & 1)) return(0);
#else
#if UCX
         if (!(sys$assign(&ucxdev_d,&s,0,0) & 1)) return(0);
	{
           short retlen;
	   struct dsc$descriptor host_name
		= {strlen(node),DSC$K_CLASS_S,DSC$K_DTYPE_T,node};
	   int comm = INETACP$C_TRANS * 256 + INETACP_FUNC$C_GETHOSTBYNAME;
	   struct dsc$descriptor command
		= {4,DSC$K_CLASS_S, DSC$K_DTYPE_T,&comm};
	   struct dsc$descriptor host_ad
		= {4,DSC$K_CLASS_S, DSC$K_DTYPE_T,&addr_buff};
	   struct iosb nam_iosb;

           if (!(sys$qiow(0,s,IO$_ACPCONTROL,&nam_iosb,0,0,
                       &command,&host_name,&retlen,&host_ad,0,0) & 1)
               || !(nam_iosb.status & 1)) {
              sys$dassgn(s);
	      return(0);
	   }
	}
#else
	if (!(sys$assign(&ip_d,&s,0,0) & 1)) return(0);
#endif
#endif
	return(1);
}

struct iosb accept_iosb;

nntp_listen(listen_chan)
int *listen_chan;
{
#if MULTINET
	struct sockaddr_in sin;
	struct iosb accept_iosb;

	if (!(sys$assign(&inet_d,listen_chan,0,0) & 1)) return(0);

	/*
	 *  Create an IP-family socket on which to listen for connections
	 */
	if (!(sys$qiow(0,*listen_chan,IO$_SOCKET,&accept_iosb,0,0,AF_INET,
	    SOCK_STREAM,0,0,0,0) & 1) || !(accept_iosb.status & 1)) {
	   sys$dassgn(*listen_chan);
	   return(0);
	}

	/*
	 *  Create a "sockaddr_in" structure which describes the port we
	 *  want to listen to. Address INADDR_ANY means we will accept
	 *  connections to any of our local IP addresses.
	 */

	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = INADDR_ANY;
	sin.sin_port = htons1(119);

	/*
	 *  Bind to that address...
	 */

	if (!(sys$qiow(0,*listen_chan,IO$_BIND,&accept_iosb,0,0,
	   &sin,sizeof(sin),0,0,0,0) & 1) || !(accept_iosb.status & 1)) {
	   sys$dassgn(*listen_chan);
	   return(0);
	}

 	/*
	 *  Declare to the kernel that we want to listen for connections
	 *  on this port, and that the kernel may queue up to five such
	 *  connections for us.
	 */

	if (!(sys$qiow(0,*listen_chan,IO$_LISTEN,&accept_iosb,0,0,5,
	    0,0,0,0,0) & 1) || !(accept_iosb.status & 1)) {
	   sys$dassgn(*listen_chan);
	   return(0);
	}

	return(1);
#else
	return(0);
#endif
}

nntp_accept_wait(listen_chan,listen_ast,listen_iosb)
int *listen_chan,*listen_ast,*listen_iosb;
{
#if MULTINET                                            
	if (!(sys$qio(0,*listen_chan,IO$_ACCEPT_WAIT,listen_iosb,listen_ast,
	    0,0,0,0,0,0,0) & 1)) {
	   sys$dassgn(*listen_chan);
	   return(0);
	}

	return(1);
#endif
}
 
nntp_accept(listen_chan,accept_chan,accept_iosb)
int *listen_chan,*accept_chan;
struct iosb *accept_iosb;
{
#if MULTINET
	struct sockaddr_in sin;
	FILE *fp;
	char buf[128];
	char *cp, *h;
	int s;
	struct sockaddr_un sun = {AF_UNIX};

	*accept_chan = -1;

	    /*
	     *	Call accept to accept a new connection. This 'peels'
	     *	a connection off of the original socket and returns to us
	     *	a new channel to the connection. We could now close
	     *	down the original socket if we didn't want to handle
	     *	more connections.
	     */
	if (!(sys$assign(&inet_d,accept_chan,0,0) & 1)) return(0);

	if (!(sys$qiow(0,*accept_chan,IO$_ACCEPT,accept_iosb,0,0,
	   &sin,sizeof(sin),*listen_chan,0,0,0) & 1)
	   || !(accept_iosb->status & 1)) return(0);

	fp = fopen("BULL_TCP_NEWS_GATEWAY", "r");
	if (!fp) return(1);

	/* A non-official way of getting ip name at ast level */

	if (!(sys$assign(&inet_d,&s,0,0) & 1)) return(0);
	if (!(sys$qiow(0,s,IO$_SOCKET,&iosb,0,0,AF_UNIX,
	    SOCK_STREAM,0,0,0,0) & 1) || !(iosb.status & 1))
	    {printf("1 iosb.status = %d\n",iosb.status);sys$dassgn(s);return(0);} 

	strcpy(sun.sun_path,"DNS");
	if (!(sys$qiow(0,s,IO$_CONNECT,&iosb,0,0,&sun,sizeof(sun),0,0,0,0) & 1)
	    || !(iosb.status & 1)) {printf("2 iosb.status = %d\n",iosb.status);sys$dassgn(s);return(0);} 

/*	buf1.function = 1;	/* gethostbyname */
	buf1.function = 2;	/* gethostbyaddr */
	buf1.call_code = 0;
	buf1.length = strlen(inet_ntoa1((int)(&sin.sin_addr)));
 	strcpy(buf1.string,inet_ntoa1((int)(&sin.sin_addr)));

	if (!(sys$qiow(0,s,IO$_WRITEVBLK,&iosb,0,0,&buf1,
					sizeof(buf1),0,0,0,0) & 1)
	    || !(iosb.status & 1)) {printf("3 iosb.status = %d\n",iosb.status);sys$dassgn(s);return(0);} 

	if (!(sys$qiow(0,s,IO$_READVBLK,&iosb,0,0,&buf2,
					sizeof(buf2),0,0,0,0) & 1)
	    || !(iosb.status & 1)) {printf("4 iosb.status = %d\n",iosb.status);sys$dassgn(s);return(0);} 

	printf("5 iosb.status = %d\n",iosb.status);sys$dassgn(s);
	buf2.string[buf2.length] = 0;
	for (cp=buf2.string; *cp; cp++) *cp = tolower(*cp);

	while (fgets(buf, sizeof(buf), fp)) {
	    for (cp=buf; *cp != '\n'; cp++) *cp = tolower(*cp);
	    *cp = 0;
	    for (cp=buf; *cp == ' ' || *cp == '\t'; cp++);
	    if (*cp == '\n' || *cp == '#') continue;
	    if (!strcmp(buf2.string,cp)) return (1);
	    if (*cp == '.' && strstr(buf2.string,cp)) return (1);
	}
	(void) fclose(fp);

	return (0);
#endif
}

news_socket()
{
	if (mode == DECNET) return (1);

#if MULTINET || TWG
	if (!(sys$qiow(0,s,IO$_SOCKET,&iosb,0,0,sin.sin_family,
	    SOCK_STREAM,0,0,0,0) & 1) || !(iosb.status & 1)) {
	   sys$dassgn(s);
	   return(0);
	}
#endif
#if UCX
	sck_parm[0] = INET$C_TCP;
	sck_parm[1] = INET_PROTYP$C_STREAM;
	local_host.inet_family = INET$C_AF_INET;
	local_host.inet_port = 0;
	local_host.inet_adrs = INET$C_INADDR_ANY;
	lhst_adrs.lgth = sizeof local_host;
	lhst_adrs.hst = &local_host;
	if (!(sys$qiow(0,s,IO$_SETMODE,&iosb,0,0,&sck_parm,0,
	   &lhst_adrs,0,0,0) & 1) || !(iosb.status & 1)) {
           sys$qiow(0,s,IO$_DEACCESS|IO$M_SHUTDOWN,&iosb,0,0,0,0,0,
						UCX$C_DSC_ALL,0,0);
	   sys$dassgn(s);
	   return(0);
	}
#endif

	return(1);
}

news_socket_bullcp(efn,biosb,astadr,astprm)
int *biosb,*astadr,*astprm,*efn;
{
	if (mode == DECNET) return (1);

#if MULTINET || TWG
	if (!(sys$qio(*efn,s,IO$_SOCKET,biosb,astadr,*astprm,sin.sin_family,
	    SOCK_STREAM,0,0,0,0) & 1) ) return(0);
#else
#if UCX
	sck_parm[0] = INET$C_TCP;
	sck_parm[1] = INET_PROTYP$C_STREAM;
	local_host.inet_family = INET$C_AF_INET;
	local_host.inet_port = 0;
	local_host.inet_adrs = INET$C_INADDR_ANY;
	lhst_adrs.lgth = sizeof local_host;
	lhst_adrs.hst = &local_host;
	if (!(sys$qio(0,s,IO$_SETMODE,biosb,astadr,*astprm,&sck_parm,0,
	   &lhst_adrs,0,0,0) & 1) ) return(0);
#else
	return(-1);
#endif
#endif

	return(1);
}

news_create()
{
	if (mode == DECNET) return (1);

#if MULTINET || TWG

	/*
	 *  Do a psuedo-connect to that address. This tells the kernel that
	 *  anything written on this socket gets sent to this destination. It
	 *  also binds us to a local port number (random, but that is ok).
	 */

	if (!(sys$qiow(0,s,IO$_CONNECT,&iosb,0,0,&sin,sizeof(sin),0,0,0,0) & 1)
	    || !(iosb.status & 1)) {
	   sys$dassgn(s);
	   return(0);
	}
#else
#if UCX 
        remote_host.inet_family = INET$C_AF_INET;
        remote_host.inet_port = htons(119);
	remote_host.inet_adrs = addr_buff;
	rhst_adrs.lgth = sizeof remote_host;
	rhst_adrs.hst = &remote_host;
	if (!(sys$qiow(0,s,IO$_ACCESS,&iosb,0,0,0,0,&rhst_adrs,0,0,0) & 1)
	    || !(iosb.status & 1)) {
           sys$qiow(0,s,IO$_DEACCESS|IO$M_SHUTDOWN,&iosb,0,0,0,0,0,
						UCX$C_DSC_ALL,0,0);
	   sys$dassgn(s);
	   return(0);
	}
#else
	if (!(sys$qiow(0,s,IO$_CREATE,&iosb,0,0,node,119,0,1,0,300) & 1)
	    || !(iosb.status & 1)) {
	   sys$dassgn(s);
	   return(0);
	}
#endif
#endif

	return(1);
}

news_create_bullcp(efn,biosb,astadr,astprm)
int *biosb,*astadr,*astprm,*efn;
{
	if (mode == DECNET) return (1);

#if MULTINET || TWG

	/*
	 *  Do a psuedo-connect to that address. This tells the kernel that
	 *  anything written on this socket gets sent to this destination. It
	 *  also binds us to a local port number (random, but that is ok).
	 */

	if (!(sys$qio(*efn,s,IO$_CONNECT,biosb,astadr
		,*astprm,&sin,sizeof(sin),0,0,0,0) & 1)) return(0);
#else
#if UCX
        remote_host.inet_family = INET$C_AF_INET;
        remote_host.inet_port = htons(119);
	remote_host.inet_adrs = addr_buff;
	rhst_adrs.lgth = sizeof remote_host;
	rhst_adrs.hst = &remote_host;
	if (!(sys$qio(*efn,s,IO$_ACCESS,biosb,astadr,*astprm,0,
		0,&rhst_adrs,0,0,0) & 1)) return(0);
#else
	if (!(sys$qio(*efn,s,IO$_CREATE,biosb,astadr,*astprm,node,
		119,0,1,0,300) & 1))
	   return(0);
#endif
#endif

	return(1);
}

news_connect()
{
	if (!news_gethost()) return(0);
	if (!news_assign()) return(0);
	if (!news_socket()) return(0);
	return(news_create());
}

news_write_packet(buf)

struct dsc$descriptor_s *buf;
{
	static int n,len;

	len = buf->dsc$w_length;
#if CMU
	if (!(sys$qiow(0,s,IO$_WRITEVBLK,&iosb,0,0,buf->dsc$a_pointer,
					len,0,!mode,0,0) & 1)
	    || !(iosb.status & 1)) return(0);
#else
	if (!(sys$qiow(0,s,IO$_WRITEVBLK,&iosb,0,0,buf->dsc$a_pointer,
					len,0,0,0,0) & 1)
	    || !(iosb.status & 1)) return(0);
#endif

	return(1);
}

news_write_packet_bullcp(efn,biosb,astadr,astprm,buf,len)
int *biosb,*astadr,*astprm,*efn,*buf,*len;
{
#if CMU
	if (!(sys$qio(*efn,s,IO$_WRITEVBLK,biosb,astadr,*astprm,buf,
					*len,0,!mode,0,0) & 1)) return(0);
#else
	if (!(sys$qio(*efn,s,IO$_WRITEVBLK,biosb,astadr,*astprm,buf,
					*len,0,0,0,0) & 1)) return(0);
#endif

	return(1);
}

news_read_packet(buf)
struct dsc$descriptor_s *buf;
{
	static int n,len;

	len = buf->dsc$w_length;
	if (!(sys$qiow(0,s,IO$_READVBLK,&iosb,0,0,buf->dsc$a_pointer,
					len,0,0,0,0) & 1)
	    || !(iosb.status & 1)) return(0);
	n = iosb.size;

	return(n);
}

news_gethostname(buf)

struct dsc$descriptor_s *buf;
{
	if (mode == DECNET) return (-1);
#if TWG
	return(gethostname(buf->dsc$a_pointer, buf->dsc$w_length));
#else
#if MULTINET
	return(gethostname1(buf->dsc$a_pointer, buf->dsc$w_length));
#else
	return(-1);
#endif
#endif
}
