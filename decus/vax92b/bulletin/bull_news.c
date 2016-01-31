#include <descrip.h>
#include "sys$library:iodef.h"

#if MULTINET

#include "multinet_root:[multinet.include.sys]types.h"
#include "multinet_root:[multinet.include.sys]socket.h"
#include "multinet_root:[multinet.include.netinet]in.h"
#include "multinet_root:[multinet.include]netdb.h"
#include "multinet_root:[multinet.include]errno.h"
#include "multinet_root:[multinet.include.vms]inetiodef.h"

static char inet[7] = "INET0:";
$DESCRIPTOR(inet_d,inet);

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
#if TWG || (MULTINET && defined(__GNUC__))
	struct hostent *gethostbyname();
#else
#if MULTINET
	struct hostent *GETHOSTBYNAME1();
#endif
#endif

	node = getenv("BULL_NEWS_SERVER");
	if (!node) return(0);
	if (!strchr(node,'.')) return(1);

#if TWG || (MULTINET && defined(__GNUC__))
	hp = gethostbyname(node);
#else
#if MULTINET
	hp = GETHOSTBYNAME1(node);
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
#if TWG || (MULTINET && defined(__GNUC__))
	sin.sin_port = htons(119);
#else
	sin.sin_port = HTONS1(119);
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
#if TWG || (MULTINET && defined(__GNUC__))
	return(gethostname(buf->dsc$a_pointer, buf->dsc$w_length));
#else
#if MULTINET
	return(GETHOSTNAME1(buf->dsc$a_pointer, buf->dsc$w_length));
#else
	return(-1);
#endif
#endif
}
