unit WebSockT;   {WebSock Type Definitions (applicable to 16 bit Delphi only)}

(*
Copyright (c) 1995-2002 HREF Tools Corp. 

Permission is hereby granted, on 12-Aug-2002, free of charge, to any person 
obtaining a copy of this software (the "Software"), to deal in the Software 
without restriction, including without limitation the rights to use, copy, 
modify, merge, publish, distribute, sublicense, and/or sell copies of the 
Software, and to permit persons to whom the Software is furnished to do so, 
subject to the following conditions:

The above copyright notice and this permission notice shall be included in 
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN 
THE SOFTWARE.

*)

{loosely based on:
 Winsock.PAS © 1994, Randy Bratton
 Sockets.PAS © 1995 Gary T. Desrosiers

 Really and truly completely redone and made into a pretty complete and capable 
 Delphi component by Michael Ax, Nov 1995

 reconciled with 32bit winsock (few changes luckily) and extended
 to be compatible with both 16 and 32 bit unit! March 1996
 
 This unit was used with webmail.pas (TWebMail component in WebHub) to provide 
 16-bit support, 1995-2002.  This unit has been removed from WebHub, Aug 2002.
}


interface


{$IFDEF WIN32}
uses Windows;
{$ELSE}
uses WinTypes;
{$ENDIF}

{-----------------------------------------------------------------}

type
(*
 * Basic system type definitions, taken from the BSD file sys/types.h.
 *)
  u_char  = char;
  u_short = word;  (* in Borland C++, int and short are both 16-bits RMB *)
  u_int   = word;
  u_long  = longint;

(*
 * Other basic types needed for the C to Pascal translation.  RMB
 *)
  PPChar = ^PChar;  (* used with char FAR * FAR * xxx   RMB *)
	PInteger = ^Integer;


(*
 * The new type to be used in all
 * instances which refer to sockets.
 *
 * Must be renamed from SOCKET as there is a function called
 * socket().  RMB
 *)
	TSocket = u_int;
  pSocket = ^TSocket;

(*
 * Select uses arrays of SOCKETs.  These macros manipulate such
 * arrays.  FD_SETSIZE may be defined by the user before including
 * this file, but the default here should be >= 64.
 *
 * CAVEAT IMPLEMENTOR and USER: THESE MACROS AND TYPES MUST BE
 * INCLUDED IN WINSOCK.H EXACTLY AS SHOWN HERE.
 *)

const
	FD_SETSIZE     =   64;

type
	FD_Set = record
		fd_count : u_short;   {number in use/set}
		fd_array : array[0..FD_SETSIZE-1] of TSocket;
	end;
  pFD_Set = ^fd_Set;


(*
 * Structure used in select() call, taken from the BSD file sys/time.h.
 *)
type
  Timeval = record
    tv_sec: longint;         (* seconds *)
    tv_usec: longint;        (* and milliseconds *)
        end;
  PTimeval = ^Timeval;

(*
 * Operations on timevals.
 *
 * NB: timercmp does not work for >= or <=.
 *)
(*
**  DEFINES (macros) for timerisset, timercmp, and timerclear
**  not implemented.  RMB
*)

(*
 * Commands for ioctlsocket(),  taken from the BSD file fcntl.h.
 *
 *
 * Ioctl's have the command encoded in the lower word,
 * and the size of any in or out parameters in the upper
 * word.  The high 2 bits of the upper word are used
 * to encode the in/out status of the parameter; for now
 * we restrict parameters to at most 128 bytes.
 *)
const
  IOCPARM_MASK =   $07f;            (* parameters must be < 128 bytes *)
  IOC_VOID     =   $020000000;      (* no parameters *)
  IOC_OUT      =   $040000000;      (* copy out parameters *)
  IOC_IN       =   $080000000;      (* copy in parameters *)
  IOC_INOUT    =   (IOC_IN or IOC_OUT);

(*
**  DEFINES (macros) for _IO, _IOR, _IOW, FIONREAD, FIONBIO, FIOASYNC,
**    SIOCSHIWAT, SIOCGHIWAT, SIOCSLOWAT, SIOCGLOWAT, SIOCATMARK
**    not implemented.  RMB
*)



(*
 * Internet address (old style... should be updated)
 *)
type
	SunB = record
		s_b1,
    s_b2,
    s_b3,
    s_b4 : u_char;
	end;

	SunW = record
		s_w1,
    s_w2 : u_short;
	end;

	TInAddr = record
		case integer of
			0 : (S_un_b : SunB);
			1 : (S_un_w : SunW);
			2 : (S_addr : u_long);
		end;
	PInAddr = ^TInAddr;


(*
 * Definitions of bits in internet address integers.
 * On subnets, the decomposition of addresses to host and net parts
 * is done according to subnet mask, not the masks here.
 *)
const
  IN_CLASSA_NET       =    $ff000000;
  IN_CLASSA_NSHIFT    =    24;
  IN_CLASSA_HOST      =    $00ffffff;
  IN_CLASSA_MAX       =    128;

  IN_CLASSB_NET       =    $ffff0000;
  IN_CLASSB_NSHIFT    =    16;
  IN_CLASSB_HOST      =    $0000ffff;
  IN_CLASSB_MAX       =    65536;

  IN_CLASSC_NET       =    $ffffff00;
  IN_CLASSC_NSHIFT    =    8;
  IN_CLASSC_HOST      =    $000000ff;

  INADDR_ANY          =    $000000000;
  INADDR_LOOPBACK     =    $7f000001;
  INADDR_BROADCAST    =    $ffffffff;
  INADDR_NONE         =    $ffffffff;


(*
 * Socket address, internet style.
 *)
type
	TSockAddrIn = record
		sin_family : integer;
		sin_port : u_short;
		sin_addr : TInAddr;
		sin_zero : array[0..7] of char;
	end;
	PSockAddrIn = ^TSockAddrIn;


(*
 * Structures returned by network data base library, taken from the
 * BSD file netdb.h.  All addresses are supplied in host order, and
 * returned in network order (suitable for use in system calls).
 *)

type
  HostEnt = record
    h_name : PChar;           (* official name of host *)
    h_aliases: PPChar;        (* alias list *)
    h_addrtype: integer;      (* host address type *)
    h_length :  integer;      (* length of address *)
    Case integer of
    0: (h_addr_list: PPChar); (* list of addresses *)
    1: (h_addr: ^pInAddr);
  	end;
	PHostEnt = ^HostEnt;

type
  NetEnt = record
    n_name : PChar;           (* official name of net *)
    n_aliases : PPChar;       (* alias list *)
    n_addrtype : integer;     (* net address type *)
    n_net : u_long;           (* network $ *)
    end;
  PNetEnt = ^NetEnt;

type
  ServEnt = record
    s_name : PChar;           (* official service name *)
    s_aliases : PPChar;       (* alias list *)
    s_port : integer;         (* port $ *)
    s_proto : PChar;          (* protocol to use *)
    end;
	PServEnt = ^ServEnt;

type
	ProtoEnt = record
    p_name : PChar;           (* official protocol name *)
    p_aliases : PPChar;       (* alias list *)
    p_proto : integer;        (* protocol $ *)
    end;
	PProtoEnt = ^ProtoEnt;

(*
 * Constants and procedures defined by the internet system,
 * Per RFC 790, September 1981, taken from the BSD file netinet/in.h.
 *)

(*
 * Protocols
 *)
const
  IPPROTO_IP          =    0;               (* dummy for IP *)
  IPPROTO_ICMP        =    1;               (* control message protocol *)
  IPPROTO_GGP         =    2;               (* gateway^2 (deprecated) *)
  IPPROTO_TCP         =    6;               (* tcp *)
  IPPROTO_PUP         =    12;              (* pup *)
  IPPROTO_UDP         =    17;              (* user datagram protocol *)
  IPPROTO_IDP         =    22;              (* xns idp *)
  IPPROTO_ND          =    77;              (* UNOFFICIAL net disk proto *)

  IPPROTO_RAW         =    255;             (* raw IP packet *)
  IPPROTO_MAX         =    256;

(*
 * Port/socket numbers: network standard functions
 *)
	IPPORT_ECHO    =   7;
	IPPORT_DISCARD =   9;
	IPPORT_SYSTAT  =   11;
	IPPORT_DAYTIME =   13;
	IPPORT_NETSTAT =   15;
	IPPORT_FTP     =   21;
	IPPORT_TELNET  =   23;
	IPPORT_SMTP    =   25;
	IPPORT_TIMESERVER  =  37;
	IPPORT_NAMESERVER  =  42;
	IPPORT_WHOIS       =  43;
	IPPORT_MTP         =  57;

(*
 * Port/socket numbers: host specific functions
 *)
	IPPORT_TFTP        =  69;
	IPPORT_RJE         =  77;
	IPPORT_FINGER      =  79;
	IPPORT_TTYLINK     =  87;
	IPPORT_SUPDUP      =  95;

(*
 * UNIX TCP sockets
 *)
	IPPORT_EXECSERVER  =  512;
	IPPORT_LOGINSERVER =  513;
	IPPORT_CMDSERVER   =  514;
	IPPORT_EFSSERVER   =  520;

(*
 * UNIX UDP sockets
 *)
	IPPORT_BIFFUDP     =  512;
	IPPORT_WHOSERVER   =  513;
	IPPORT_ROUTESERVER =  520;

    (* 520+1 also used *)

(*
 * Ports < IPPORT_RESERVED are reserved for
 * privileged processes (e.g. root).
 *)
	IPPORT_RESERVED    =  1024;

(*
 * Link numbers
 *)
	IMPLINK_IP         =  155;
	IMPLINK_LOWEXPER   =  156;
	IMPLINK_HIGHEXPER  =  158;



const
	WSADESCRIPTION_LEN     =   256;
	WSASYS_STATUS_LEN      =   128;

type
	TWSAData = record
		wVersion : word;
		wHighVersion : word;
		szDescription : array[0..WSADESCRIPTION_LEN] of char;
		szSystemStatus : array[0..WSASYS_STATUS_LEN] of char;
		iMaxSockets : integer;
		iMaxUdpDg : integer;
		lpVendorInfo : PChar;
    end;
	pWSAData = ^TWSAData;


(*
 * Options for use with [gs]etsockopt at the IP level.
 *)
const
	IP_OPTIONS				= 1;    (* set/get IP per-packet options *)

(*
 * Definitions related to sockets: types, address families, options,
 * taken from the BSD file sys/socket.h.
 *)

(*
 * This is used instead of -1, since the
 * SOCKET type is unsigned.
 *)
const
	INVALID_SOCKET = TSocket(not 0);
	SOCKET_ERROR   = -1;

const
(*
 * Types
 *)
  SOCK_STREAM     = 1;               (* stream socket *)
  SOCK_DGRAM      = 2;               (* datagram socket *)
  SOCK_RAW        = 3;               (* raw-protocol interface *)
  SOCK_RDM        = 4;               (* reliably-delivered message *)
  SOCK_SEQPACKET  = 5;               (* sequenced packet stream *)

(*
 * Option flags per-socket.
 *)
  SO_DEBUG        = $0001;          (* turn on debugging info recording *)
  SO_ACCEPTCONN   = $0002;          (* socket has had listen() *)
  SO_REUSEADDR    = $0004;          (* allow local address reuse *)
  SO_KEEPALIVE    = $0008;          (* keep connections alive *)
  SO_DONTROUTE    = $0010;          (* just use interface addresses *)
  SO_BROADCAST    = $0020;          (* permit sending of broadcast msgs *)
  SO_USELOOPBACK  = $0040;          (* bypass hardware when possible *)
  SO_LINGER       = $0080;          (* linger on close if data present *)
  SO_OOBINLINE    = $0100;          (* leave received OOB data in line *)

	SO_DONTLINGER  =   $ff7f;

(*
 * Additional options.
 *)
  SO_SNDBUF       = $1001;          (* send buffer size *)
  SO_RCVBUF       = $1002;          (* receive buffer size *)
  SO_SNDLOWAT     = $1003;          (* send low-water mark *)
  SO_RCVLOWAT     = $1004;          (* receive low-water mark *)
  SO_SNDTIMEO     = $1005;          (* send timeout *)
  SO_RCVTIMEO     = $1006;          (* receive timeout *)
  SO_ERROR        = $1007;          (* get error status and clear *)
  SO_TYPE         = $1008;          (* get socket type *)

(*
 * TCP options.
 *)
  TCP_NODELAY     = $0001;

(*
 * Address families.
 *)
  AF_UNSPEC       = 0;               (* unspecified *)
  AF_UNIX         = 1;               (* local to host (pipes, portals) *)
  AF_INET         = 2;               (* internetwork: UDP, TCP, etc. *)
  AF_IMPLINK      = 3;               (* arpanet imp addresses *)
  AF_PUP          = 4;               (* pup protocols: e.g. BSP *)
  AF_CHAOS        = 5;               (* mit CHAOS protocols *)
  AF_NS           = 6;               (* XEROX NS protocols *)
  AF_ISO          = 7;               (* ISO protocols *)
  AF_OSI          = AF_ISO;          (* OSI is ISO *)
  AF_ECMA         = 8;               (* european computer manufacturers *)
  AF_DATAKIT      = 9;               (* datakit protocols *)
  AF_CCITT        = 10;              (* CCITT protocols, X.25 etc *)
  AF_SNA          = 11;              (* IBM SNA *)
  AF_DECnet       = 12;              (* DECnet *)
  AF_DLI          = 13;              (* Direct data link interface *)
  AF_LAT          = 14;              (* LAT *)
  AF_HYLINK       = 15;              (* NSC Hyperchannel *)
  AF_APPLETALK    = 16;              (* AppleTalk *)
  AF_NETBIOS      = 17;              (* NetBios-style addresses *)

  AF_MAX          = 18;


(*
 * Structure used by kernel to store most
 * addresses.
 *)
type
  SockAddr = record
    sa_family : u_short;            (* address family *)
    sa_data : array[0..13] of char; (* up to 14 bytes of direct address *)
    end;
  pSockAddr = ^SockAddr;

	SockProto = record
    sp_family : u_short;              (* address family *)
    sp_protocol : u_short;            (* protocol *)
	end;
  pSockProto = ^SockProto;

(*
 * Protocol families, same as address families for now.
 *)
const

  PF_UNSPEC       = AF_UNSPEC;
  PF_UNIX         = AF_UNIX;
  PF_INET         = AF_INET;
  PF_IMPLINK      = AF_IMPLINK;
  PF_PUP          = AF_PUP;
  PF_CHAOS        = AF_CHAOS;
  PF_NS           = AF_NS;
  PF_ISO          = AF_ISO;
  PF_OSI          = AF_OSI;
  PF_ECMA         = AF_ECMA;
  PF_DATAKIT      = AF_DATAKIT;
  PF_CCITT        = AF_CCITT;
  PF_SNA          = AF_SNA;
  PF_DECnet       = AF_DECnet;
  PF_DLI          = AF_DLI;
  PF_LAT          = AF_LAT;
  PF_HYLINK       = AF_HYLINK;
  PF_APPLETALK    = AF_APPLETALK;

  PF_MAX          = AF_MAX;


(*
 * Structure used for manipulating linger option.
 *)
type
	Linger = record
		l_onoff : boolean; {was u_short RMB}
		l_linger : u_short;
	end;
	pLinger = ^Linger;


(*
 * Level number for (get/set)sockopt() to apply to socket itself.
 *)
const
  SOL_SOCKET     = -1; {was $ffff  RMB}  (* options for socket level *)

(*
 * Maximum queue length specifiable by listen.
 *)
const
  SOMAXCONN       = 5;

  MSG_OOB         = $1;             (* process out-of-band data *)
  MSG_PEEK        = $2;             (* peek at incoming message *)
  MSG_DONTROUTE   = $4;             (* send without using routing tables *)

  MSG_MAXIOVLEN   = 16;

(*
 * Define constant based on rfc883, used by gethostbyxxxx() calls.
 *)
const
  MAXGETHOSTSTRUCT        = 1024;

(*
 * Define flags to be used with the WSAAsyncSelect() call.
 *)
const
	FD_READ            =  $01;
	FD_WRITE           =  $02;
	FD_OOB             =  $04;
	FD_ACCEPT          =  $08;
	FD_CONNECT         =  $10;
	FD_CLOSE           =  $20;

(*
 * All Windows Sockets error constants are biased by WSABASEERR from
 * the "normal"
 *)
const
	WSABASEERR         =  10000;

(*
 * Windows Sockets definitions of regular Berkeley error constants
 *)
const
	WSAEINTR           =  (WSABASEERR + 4);
	WSAEBADF           =  (WSABASEERR + 9);
	WSAEFAULT          =  (WSABASEERR + 14);
	WSAEINVAL          =  (WSABASEERR + 22);
	WSAEMFILE          =  (WSABASEERR + 24);

	WSAEWOULDBLOCK     =  (WSABASEERR + 35);
	WSAEINPROGRESS     =  (WSABASEERR + 36);
	WSAEALREADY        =  (WSABASEERR + 37);
	WSAENOTSOCK        =  (WSABASEERR + 38);
	WSAEDESTADDRREQ    =  (WSABASEERR + 39);
	WSAEMSGSIZE        =  (WSABASEERR + 40);
	WSAEPROTOTYPE      =  (WSABASEERR + 41);
	WSAENOPROTOOPT     =  (WSABASEERR + 42);
	WSAEPROTONOSUPPORT =  (WSABASEERR + 43);
	WSAESOCKTNOSUPPORT =  (WSABASEERR + 44);
	WSAEOPNOTSUPP      =  (WSABASEERR + 45);
	WSAEPFNOSUPPORT    =  (WSABASEERR + 46);
	WSAEAFNOSUPPORT    =  (WSABASEERR + 47);
	WSAEADDRINUSE      =  (WSABASEERR + 48);
	WSAEADDRNOTAVAIL   =  (WSABASEERR + 49);
	WSAENETDOWN        =  (WSABASEERR + 50);
	WSAENETUNREACH     =  (WSABASEERR + 51);
	WSAENETRESET       =  (WSABASEERR + 52);
	WSAECONNABORTED    =  (WSABASEERR + 53);
	WSAECONNRESET      =  (WSABASEERR + 54);
	WSAENOBUFS         =  (WSABASEERR + 55);
	WSAEISCONN         =  (WSABASEERR + 56);
	WSAENOTCONN        =  (WSABASEERR + 57);
	WSAESHUTDOWN       =  (WSABASEERR + 58);
	WSAETOOMANYREFS    =  (WSABASEERR + 59);
	WSAETIMEDOUT       =  (WSABASEERR + 60);
	WSAECONNREFUSED    =  (WSABASEERR + 61);
	WSAELOOP           =  (WSABASEERR + 62);
	WSAENAMETOOLONG    =  (WSABASEERR + 63);
	WSAEHOSTDOWN       =  (WSABASEERR + 64);
	WSAEHOSTUNREACH    =  (WSABASEERR + 65);
	WSAENOTEMPTY       =  (WSABASEERR + 66);
	WSAEPROCLIM        =  (WSABASEERR + 67);
	WSAEUSERS          =  (WSABASEERR + 68);
	WSAEDQUOT          =  (WSABASEERR + 69);
	WSAESTALE          =  (WSABASEERR + 70);
	WSAEREMOTE         =  (WSABASEERR + 71);

(*
 * Extended Windows Sockets error constant definitions
 *)
const
	WSASYSNOTREADY     =  (WSABASEERR + 91);
	WSAVERNOTSUPPORTED =  (WSABASEERR + 92);
	WSANOTINITIALISED  =  (WSABASEERR + 93);

(*
 * Error return codes from gethostbyname() and gethostbyaddr()
 * (when using the resolver). Note that these errors are
 * retrieved via WSAGetLastError() and must therefore follow
 * the rules for avoiding clashes with error numbers from
 * specific implementations or language run-time systems.
 * For this reason the codes are based at WSABASEERR+1001.
 * Note also that [WSA]NO_ADDRESS is defined only for
 * compatibility purposes.
 *)

const
  WSAHOST_NOT_FOUND       = (WSABASEERR+1001);(* Authoritative Answer: Host not found *)
  WSATRY_AGAIN            = (WSABASEERR+1002);(* Non-Authoritative: Host not found, or SERVERFAIL *)
  WSANO_RECOVERY          = (WSABASEERR+1003);(* Non recoverable errors, FORMERR, REFUSED, NOTIMP *)
  WSANO_DATA              = (WSABASEERR+1004);(* Valid name, no data record of requested type *)
  WSANO_ADDRESS           = WSANO_DATA;(* no address, look for MX record *)

  TRY_AGAIN               = WSATRY_AGAIN;
  HOST_NOT_FOUND          = WSAHOST_NOT_FOUND;
  NO_RECOVERY             = WSANO_RECOVERY;
  NO_DATA                 = WSANO_DATA;
  NO_ADDRESS              = WSANO_ADDRESS;


(*
 * Windows Sockets errors redefined as regular Berkeley error constants
 *)
const
  EWOULDBLOCK             = WSAEWOULDBLOCK;
  EINPROGRESS             = WSAEINPROGRESS;
  EALREADY                = WSAEALREADY;
  ENOTSOCK                = WSAENOTSOCK;
  EDESTADDRREQ            = WSAEDESTADDRREQ;
  EMSGSIZE                = WSAEMSGSIZE;
  EPROTOTYPE              = WSAEPROTOTYPE;
  ENOPROTOOPT             = WSAENOPROTOOPT;
  EPROTONOSUPPORT         = WSAEPROTONOSUPPORT;
  ESOCKTNOSUPPORT         = WSAESOCKTNOSUPPORT;
  EOPNOTSUPP              = WSAEOPNOTSUPP;
  EPFNOSUPPORT            = WSAEPFNOSUPPORT;
  EAFNOSUPPORT            = WSAEAFNOSUPPORT;
  EADDRINUSE              = WSAEADDRINUSE;
  EADDRNOTAVAIL           = WSAEADDRNOTAVAIL;
  ENETDOWN                = WSAENETDOWN;
  ENETUNREACH             = WSAENETUNREACH;
  ENETRESET               = WSAENETRESET;
  ECONNABORTED            = WSAECONNABORTED;
  ECONNRESET              = WSAECONNRESET;
  ENOBUFS                 = WSAENOBUFS;
  EISCONN                 = WSAEISCONN;
  ENOTCONN                = WSAENOTCONN;
  ESHUTDOWN               = WSAESHUTDOWN;
  ETOOMANYREFS            = WSAETOOMANYREFS;
  ETIMEDOUT               = WSAETIMEDOUT;
  ECONNREFUSED            = WSAECONNREFUSED;
  ELOOP                   = WSAELOOP;
  ENAMETOOLONG            = WSAENAMETOOLONG;
  EHOSTDOWN               = WSAEHOSTDOWN;
  EHOSTUNREACH            = WSAEHOSTUNREACH;
  ENOTEMPTY               = WSAENOTEMPTY;
  EPROCLIM                = WSAEPROCLIM;
  EUSERS                  = WSAEUSERS;
  EDQUOT                  = WSAEDQUOT;
  ESTALE                  = WSAESTALE;
  EREMOTE                 = WSAEREMOTE;

{-----------------------------------------------------------------}
type (* Socket function prototypes *)
  TwAccept= function(s : TSocket; addr : PSockaddr; addrlen : PInteger) : TSocket;
  TwBind= function(s : TSocket; addr : Psockaddr; namelen : integer) : integer;
  TwClosesocket= function(s : TSocket) : integer;
  TwConnect= function(s : TSocket; name : Psockaddr; namelen : integer) : integer;
  TwGetpeername= function(s : TSocket; name : Psockaddr; namelen : Pinteger) : integer;
  TwGetsockname= function(s : TSocket; name : Psockaddr; namelen : Pinteger) : integer;
  TwGetsockopt= function(s : TSocket; level, optname : integer; optval : PChar; optlen : integer) : integer;
  TwHtonl= function(hostlong : u_long) : u_long;
  TwHtons= function(hostshort : u_short) : u_short;
  TwInet_addr= function(cp : PChar) : u_long; {PTInAddr;}  { in_addr }
  TwInet_ntoa= function(inaddr : TInAddr) : PChar;
  TwIoctlsocket= function(s : TSocket; cmd : longint; argp : u_long) : integer;
  TwListen= function(s : TSocket; backlog : integer) : integer;
  TwNtohl= function(netlong : u_long) : u_long;
  TwNtohs= function(netshort : u_short) : u_short;
  TwRecv= function(s : TSocket; buf : PChar; len, flags : integer) : integer;
  TwRecvfrom= function(s : TSocket; buf : PChar; len, flags : integer; from : sockaddr; fromlen : integer) : integer;
  TwSelect= function(nfds : integer; readfds, writefds, exceptfds : fd_set; timeout : timeval) : longint;
  TwSend= function(s : TSocket; buf : PChar; len, flags : integer) : integer;
  TwSendto= function(s : TSocket; buf : PChar; len, flags : integer; addrto : sockaddr; tolen : integer) : integer;
  TwSetsockopt= function(s : TSocket; level, optname : integer; optval : PChar; optlen : integer) : integer;
  TwShutdown= function(s : TSocket; how : integer) : integer;
  TwSocket= function(af, struct, protocol : integer) : TSocket;

type (* Database function prototypes *)
  TwGethostbyaddr= function(addr : PChar; len, struct : integer) : PHostEnt; { hostent }
  TwGethostbyname= function(name : PChar) : PHostEnt; { hostent }
  TwGetprotobyname= function(name : PChar) : PProtoEnt; { protoent }
  TwGetprotobynumber= function(proto : integer) : PProtoEnt; { protoent }
  TwGetservbyname= function(name, proto : PChar) : PServEnt; { servent }
  TwGetservbyport= function(port : integer; proto : PChar) : PServEnt; { servent }

  TwGethostname= function(name : PChar; len : integer) : integer;

type (* Microsoft Windows Extension function prototypes *)
  TwWSAAsyncSelect= function(s : TSocket; HWindow : HWND; wMsg : u_int; lEvent : longint) : integer;
  TwWSAAsyncGetHostByAddr= function(HWindow : HWND; wMsg : u_int; addr : PChar; len, struct : integer;
                                       buf : PChar; buflen : integer) : THandle;
  TwWSAAsyncGetHostByName= function(HWindow : HWND; wMsg : u_int; name, buf : PChar; buflen : integer) : THandle;
  TwWSAAsyncGetProtoByNumber= function(HWindow : HWND; wMsg : u_int; number : integer;
                                       buf : PChar; buflen : integer) : THandle;
  TwWSAAsyncGetProtoByName= function(HWindow : HWND; wMsg : u_int; name, buf : PChar; buflen : integer) : THandle;
  TwWSAAsyncGetServByPort= function( HWindow : HWND; wMsg, port : u_int; proto, buf : PChar;
                                       buflen : integer) : THandle;
  TwWSAAsyncGetServByName= function(HWindow : HWND; wMsg : u_int; name, proto, buf : PChar;
                                       buflen : integer) : THandle;
  TwWSACancelAsyncRequest= function(hAsyncTaskHandle : THandle) : integer;

  TwWSASetBlockingHook= function(lpBlockFunc : TFarProc) : TFarProc;
  TwWSAUnhookBlockingHook= function: integer;
  TwWSAGetLastError= function: integer;
  TwWSASetLastError= procedure(iError : integer);
  TwWSACancelBlockingCall= function: integer;
  TwWSAIsBlocking= function: BOOL;
  TwWSAStartup= function(wVersionRequired : word; lpWSData : pWSAData) : integer;
  TwWSACleanup= function: integer;
  TwWSAFDIsSet= function(s: TSocket; aset: PFd_Set): integer;

{-------------------------------------------------------------------------}

implementation

{-------------------------------------------------------------------------}

end.

