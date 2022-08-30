{*******************************************************}
{               MiTeC Common Routines                   }
{               Windows Sockets 2 API                   }
{                                                       }
{          Copyright (c) 2015-2018 Michal Mutl          }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.Inc}

unit MiTeC_Ws2_32;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, WinAPI.WinSock,
     {$ELSE}
     Windows, SysUtils, Winsock,
     {$ENDIF}
     MiTeC_Windows;

const
  AF_INET6     = 23; // Internetwork Version 6
  NI_MAXHOST = 1025; // Max size of a fully-qualified domain name

  INET_ADDRSTRLEN  = 16; // Max size of numeric form of IPv4 address
  INET6_ADDRSTRLEN = 46; // Max size of numeric form of IPv6 address

  NI_NOFQDN       =   $1  ;  // Only return nodename portion for local hosts.
  NI_NUMERICHOST  =   $2  ;  // Return numeric form of the host's address.
  NI_NAMEREQD     =   $4  ;  // Error if the host's name not in DNS.
  NI_NUMERICSERV  =   $8  ;  // Return numeric form of the service (port #).
  NI_DGRAM        =   $10 ;  // Service is a datagram service.

  _SS_MAXSIZE   = 128;               // Maximum size.
  _SS_ALIGNSIZE = SizeOf(Int64);  // Desired alignment.
  _SS_PAD1SIZE = _SS_ALIGNSIZE - SizeOf(short);
  _SS_PAD2SIZE = _SS_MAXSIZE - (SizeOf(short) + _SS_PAD1SIZE + _SS_ALIGNSIZE);

  AI_PASSIVE            = $00000001;   // Socket address will be used in bind() call
  AI_CANONNAME          = $00000002;   // Return canonical name in first ai_canonname
  AI_NUMERICHOST        = $00000004;   // Nodename must be a numeric address string
  AI_NUMERICSERV        = $00000008;  // Servicename must be a numeric port number
  AI_ALL                = $00000100;  // Query both IP6 and IP4 with AI_V4MAPPED
  AI_ADDRCONFIG         = $00000400;  // Resolution only if global address configured
  AI_V4MAPPED           = $00000800;  // On v6 failure, query v4 and convert to V4MAPPED format (Vista or later)
  AI_NON_AUTHORITATIVE  = $00004000;  // LUP_NON_AUTHORITATIVE  (Vista or later)
  AI_SECURE             = $00008000;  // LUP_SECURE  (Vista or later and applies only to NS_EMAIL namespace.)
  AI_RETURN_PREFERRED_NAMES = $00010000;  // LUP_RETURN_PREFERRED_NAMES (Vista or later and applies only to NS_EMAIL namespace.)
  AI_FQDN                   = $00020000;  // Return the FQDN in ai_canonname  (Windows 7 or later)
  AI_FILESERVER             = $00040000;  // Resolving fileserver name resolution (Windows 7 or later)
  AI_DISABLE_IDN_ENCODING = $00080000;  // Disable Internationalized Domain Names handling


type
  TIPv6Byte = array [0..15] of Byte;

  {$if not defined(RAD5PLUS) and not defined(FPC)}
  ULONG_PTR = LongWord;
  {$ifend}
  SIZE_T = ULONG_PTR;

  in6_addr = record
    case Integer of
      0: (Byte: TIPv6Byte);
      1: (Word: array[0..7] of u_short);
      2: (s6_bytes: array [0..15] of u_char);
      3: (s6_addr: array [0..15] of u_char);
      4: (s6_words: array[0..7] of u_short);
  end;
  TIn6Addr = in6_addr;
  PIn6Addr = ^in6_addr;

  ipv6_mreq = record
    ipv6mr_multiaddr: in6_addr;  // IPv6 multicast address
    ipv6mr_interface: Cardinal;  // Interface index
  end;
  TIpV6MReq = ipv6_mreq;
  PIpV6MReq = ^ipv6_mreq;

type
  in_addr6 = record
    s6_addr: array [0..15] of u_char; // IPv6 address
  end;
  TInAddr6 = in_addr6;
  PInAddr6 = ^in_addr6;

// Old IPv6 socket address structure (retained for sockaddr_gen definition below)

type
  sockaddr_in6_old = record
    sin6_family: short;    // AF_INET6
    sin6_port: u_short;    // Transport level port number
    sin6_flowinfo: u_long; // IPv6 flow information
    sin6_addr: in6_addr;   // IPv6 address
  end;
  TSockAddrIn6Old = sockaddr_in6_old;
  PSockAddrIn6Old = ^sockaddr_in6_old;

// IPv6 socket address structure, RFC 2553

  SOCKADDR_IN6 = record
    sin6_family: short;    // AF_INET6
    sin6_port: u_short;    // Transport level port number
    sin6_flowinfo: u_long; // IPv6 flow information
    sin6_addr: in6_addr;   // IPv6 address
    sin6_scope_id: u_long; // set of interfaces for a scope
  end;
  PSOCKADDR_IN6 = ^SOCKADDR_IN6;
  LPSOCKADDR_IN6 = ^SOCKADDR_IN6;
  TSockAddrIn6 = SOCKADDR_IN6;
  PSockAddrIn6 = LPSOCKADDR_IN6;

  LPADDRINFO = ^addrinfo;
  addrinfo = record
    ai_flags: Integer;       // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST
    ai_family: Integer;      // PF_xxx
    ai_socktype: Integer;    // SOCK_xxx
    ai_protocol: Integer;    // 0 or IPPROTO_xxx for IPv4 and IPv6
    ai_addrlen: size_t;  // Length of ai_addr
    ai_canonname: PChar; // Canonical name for nodename
    ai_addr: PSockAddr;  // Binary address
    ai_next: LPADDRINFO;  // Next structure in linked list
  end;
  TAddrInfo = addrinfo;
  PAddrInfo = LPADDRINFO;

  LPSOCKADDR = ^sockaddr_in;

  sockaddr_storage = record
    ss_family: short;               // Address family.
    __ss_pad1: array [0.._SS_PAD1SIZE - 1] of AnsiChar;  // 6 byte pad, this is to make
                                   // implementation specific pad up to
                                   // alignment field that follows explicit
                                   // in the data structure.
    __ss_align: Int64;            // Field to force desired structure.
    __ss_pad2: array [0.._SS_PAD2SIZE - 1] of AnsiChar;  // 112 byte pad to achieve desired size;
                                   // _SS_MAXSIZE value minus size of
                                   // ss_family, __ss_pad1, and
                                   // __ss_align fields is 112.
  end;
  TSockAddrStorage = sockaddr_storage;
  PSockAddrStorage = ^sockaddr_storage;

  PSOCKADDR_STORAGE = ^sockaddr_storage;
  LPSOCKADDR_STORAGE = ^sockaddr_storage;

  LPSOCKET_ADDRESS = ^SOCKET_ADDRESS;
  PSOCKET_ADDRESS = ^SOCKET_ADDRESS;
  _SOCKET_ADDRESS = record
    lpSockaddr: LPSOCKADDR_STORAGE;//LPSOCKADDR;
    iSockaddrLength: Integer;
  end;
  SOCKET_ADDRESS = _SOCKET_ADDRESS;
  TSocketAddress = SOCKET_ADDRESS;
  PSocketAddress = PSOCKET_ADDRESS;

const
  FD_MAX_EVENTS                   = 10;
  FD_ALL_EVENTS                   = (1 shl FD_MAX_EVENTS) - 1;

type
  _WSANETWORKEVENTS = record
    lNetworkEvents: Longint;
    iErrorCode: array [0..FD_MAX_EVENTS - 1] of Integer;
  end;
  WSANETWORKEVENTS = _WSANETWORKEVENTS;
  LPWSANETWORKEVENTS = ^WSANETWORKEVENTS;
  TWsaNetworkEvents = WSANETWORKEVENTS;
  PWsaNetworkEvents = LPWSANETWORKEVENTS;

const
  MAX_PROTOCOL_CHAIN = 7;
  BASE_PROTOCOL    = 1;
  LAYERED_PROTOCOL = 0;

type
  _WSAPROTOCOLCHAIN = record
    ChainLen: Integer;                            // the length of the chain,
                                                  // length = 0 means layered protocol,
                                                  // length = 1 means base protocol,
                                                  // length > 1 means protocol chain
    ChainEntries: array [0..MAX_PROTOCOL_CHAIN - 1] of DWORD; // a list of dwCatalogEntryIds
  end;
  WSAPROTOCOLCHAIN = _WSAPROTOCOLCHAIN;
  LPWSAPROTOCOLCHAIN = ^WSAPROTOCOLCHAIN;
  TWsaProtocolChain = WSAPROTOCOLCHAIN;
  PWsaProtocolChain = LPWSAPROTOCOLCHAIN;

const
  WSAPROTOCOL_LEN = 255;

type
  _WSAPROTOCOL_INFOA = record
    dwServiceFlags1: DWORD;
    dwServiceFlags2: DWORD;
    dwServiceFlags3: DWORD;
    dwServiceFlags4: DWORD;
    dwProviderFlags: DWORD;
    ProviderId: TGUID;
    dwCatalogEntryId: DWORD;
    ProtocolChain: WSAPROTOCOLCHAIN;
    iVersion: Integer;
    iAddressFamily: Integer;
    iMaxSockAddr: Integer;
    iMinSockAddr: Integer;
    iSocketType: Integer;
    iProtocol: Integer;
    iProtocolMaxOffset: Integer;
    iNetworkByteOrder: Integer;
    iSecurityScheme: Integer;
    dwMessageSize: DWORD;
    dwProviderReserved: DWORD;
    szProtocol: array [0..WSAPROTOCOL_LEN] of AnsiChar;
  end;
  WSAPROTOCOL_INFOA = _WSAPROTOCOL_INFOA;
  LPWSAPROTOCOL_INFOA = ^WSAPROTOCOL_INFOA;
  TWsaProtocolInfoA = WSAPROTOCOL_INFOA;
  PWsaProtocolInfoA = LPWSAPROTOCOL_INFOA;

  _WSAPROTOCOL_INFOW = record
    dwServiceFlags1: DWORD;
    dwServiceFlags2: DWORD;
    dwServiceFlags3: DWORD;
    dwServiceFlags4: DWORD;
    dwProviderFlags: DWORD;
    ProviderId: TGUID;
    dwCatalogEntryId: DWORD;
    ProtocolChain: WSAPROTOCOLCHAIN;
    iVersion: Integer;
    iAddressFamily: Integer;
    iMaxSockAddr: Integer;
    iMinSockAddr: Integer;
    iSocketType: Integer;
    iProtocol: Integer;
    iProtocolMaxOffset: Integer;
    iNetworkByteOrder: Integer;
    iSecurityScheme: Integer;
    dwMessageSize: DWORD;
    dwProviderReserved: DWORD;
    szProtocol: array [0..WSAPROTOCOL_LEN] of WideChar;
  end;
  WSAPROTOCOL_INFOW = _WSAPROTOCOL_INFOW;
  LPWSAPROTOCOL_INFOW = ^WSAPROTOCOL_INFOW;
  TWsaProtocolInfoW = WSAPROTOCOL_INFOW;
  PWsaProtocolInfoW = LPWSAPROTOCOL_INFOW;

  {$IFDEF UNICODE}
  WSAPROTOCOL_INFO = WSAPROTOCOL_INFOW;
  LPWSAPROTOCOL_INFO = LPWSAPROTOCOL_INFOW;
  TWsaProtocolInfo = TWsaProtocolInfoW;
  PWsaProtocolInfo = PWsaProtocolInfoW;
  {$ELSE}
  WSAPROTOCOL_INFO = WSAPROTOCOL_INFOA;
  LPWSAPROTOCOL_INFO = LPWSAPROTOCOL_INFOA;
  TWsaProtocolInfo = TWsaProtocolInfoA;
  PWsaProtocolInfo = PWsaProtocolInfoA;
  {$ENDIF UNICODE}


  Tgetnameinfo = function (sa: PSockAddr; salen: integer; host: PChar; hostlen: DWORD; serv: PChar; servlen: DWORD; flags: Integer): Integer; stdcall;
  Tgetaddrinfo = function (nodename, servname: PChar; hints: PAddrInfo; var res: PAddrInfo): Integer; stdcall;
  Tfreeaddrinfo = procedure (ai: PAddrInfo); stdcall;
  TInetNtop = function(Family: Integer; pAddr: Pointer; pStringBuf: {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; StringBufSize: SIZE_T): {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; stdcall;
  TInetPton = function(Family: Integer; pStringBuf: {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; pAddr: Pointer): integer; stdcall;
  {TGetAddrInfoExA = function(pName : PAnsiChar; pServiceName : PAnsiChar;
    const dwNameSpace: DWord; lpNspId : LPGUID; hints : PADDRINFOEXA;
    ppResult : PADDRINFOEXA; timeout : Ptimeval; lpOverlapped : LPWSAOVERLAPPED;
    lpCompletionRoutine : LPLOOKUPSERVICE_COMPLETION_ROUTINE;
    var lpNameHandle : THandle) : Integer; stdcall;
  TGetAddrInfoExW = function(pName : PWideChar; pServiceName : PWideChar;
    const dwNameSpace: DWord; lpNspId : LPGUID;hints : PADDRINFOEXW;
    ppResult : PADDRINFOEXW; timeout : Ptimeval; lpOverlapped : LPWSAOVERLAPPED;
    lpCompletionRoutine : LPLOOKUPSERVICE_COMPLETION_ROUTINE;
    var lpNameHandle : THandle) : Integer; stdcall;}

  TWSAAddressToString = function(lpsaAddress: LPSOCKADDR; dwAddressLength: DWORD;
                                 lpProtocolInfo: {$IFDEF UNICODE}LPWSAPROTOCOL_INFOW{$ELSE}LPWSAPROTOCOL_INFOA{$ENDIF}; lpszAddressString: {$IFDEF UNICODE}LPWSTR{$ELSE}LPSTR{$ENDIF};
                                 var lpdwAddressStringLength: DWORD): Integer; stdcall;
  TWSAStringToAddress = function(AddressString: {$IFDEF UNICODE}LPWSTR{$ELSE}LPSTR{$ENDIF}; AddressFamily: Integer;
                                  lpProtocolInfo: {$IFDEF UNICODE}LPWSAPROTOCOL_INFOW{$ELSE}LPWSAPROTOCOL_INFOA{$ENDIF}; lpAddress: LPSOCKADDR;
                                  var lpAddressLength: Integer): Integer; stdcall;
var
  getnameinfo: Tgetnameinfo = nil;
  getaddrinfo: Tgetaddrinfo = nil;
  freeaddrinfo: Tfreeaddrinfo = nil;
  InetNtop: TInetNtop = nil;
  InetPton: TInetPton = nil;

  WSAAddressToString: TWSAAddressToString = nil;
  WSAStringToAddress: TWSAStringToAddress = nil;

implementation

procedure InitWS2;
var
  h: THandle;
begin
  h:=GetModuleHandle('ws2_32.dll');
  if h=0 then
    Exit;
  {$IFDEF UNICODE}
  @getnameinfo:=GetProcAddress(h,PAnsiChar('GetNameInfoW'));
  @getaddrinfo:=GetProcAddress(h,PAnsiChar('GetAddrInfoW'));
  @freeaddrinfo:=GetProcAddress(h,PAnsiChar('FreeAddrInfoW'));
  @InetNtop:=GetProcAddress(h,PAnsiChar('InetNtopW'));
  @InetPton:=GetProcAddress(h,PAnsiChar('InetPtonW'));
  @WSAAddressToString:=GetProcAddress(h,PAnsiChar('WSAAddressToStringW'));
  @WSAStringToAddress:=GetProcAddress(h,PAnsiChar('WSAStringToAddressW'));
  {$ELSE}
  @getnameinfo:=GetProcAddress(h,PAnsiChar('getnameinfo'));
  @getaddrinfo:=GetProcAddress(h,PAnsiChar('getaddrinfo'));
  @freeaddrinfo:=GetProcAddress(h,PAnsiChar('freeaddrinfo'));
  @InetNtop:=GetProcAddress(h,PAnsiChar('InetNtopA'));
  @InetPton:=GetProcAddress(h,PAnsiChar('InetPtonA'));
  @WSAAddressToString:=GetProcAddress(h,PAnsiChar('WSAAddressToStringA'));
  @WSAStringToAddress:=GetProcAddress(h,PAnsiChar('WSAStringToAddressA'));
  {$ENDIF}
end;

initialization
  InitWS2;
end.

