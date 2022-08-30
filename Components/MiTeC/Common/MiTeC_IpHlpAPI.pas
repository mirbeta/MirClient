{*******************************************************}
{               MiTeC Common Routines                   }
{                   IP Helper API                       }
{                                                       }
{                                                       }
{         Copyright (c) 1997-2019 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_IpHlpAPI;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, WinAPI.WinSock,
     {$ELSE}
     Windows, SysUtils, WinSock,
     {$ENDIF}
     MiTeC_Ws2_32, MiTeC_Windows;

type
  time_t = Longint;

  {$if not defined(RAD5PLUS) and not defined(FPC)}
  ULONGLONG=INT64;
  {$ifend}

const
  MAX_ADAPTER_DESCRIPTION_LENGTH = 128;
  MAX_ADAPTER_NAME_LENGTH        = 256;
  MAX_ADAPTER_ADDRESS_LENGTH     = 8;
  DEFAULT_MINIMUM_ENTITIES       = 32;
  MAX_HOSTNAME_LEN               = 128;
  MAX_DOMAIN_NAME_LEN            = 128;
  MAX_SCOPE_ID_LEN               = 256;
  MAX_INTERFACE_NAME_LEN = 256;
  MAX_TRANSPORT_NAME_LEN = 40;
  MAX_MEDIA_NAME         = 16;
  MAX_PORT_NAME          = 16;
  MAX_DEVICE_NAME        = 128;
  MAX_PHONE_NUMBER_LEN   = 128;
  MAX_DEVICETYPE_NAME    = 16;
  MAXLEN_IFDESCR  = 256;

  BROADCAST_NODETYPE    = 1;
  PEER_TO_PEER_NODETYPE = 2;
  MIXED_NODETYPE        = 4;
  HYBRID_NODETYPE       = 8;

  MIB_IF_OTHER_ADAPTERTYPE      = 1;        // other
  MIB_IF_ETHERNET_ADAPTERTYPE   = 6;        // Ethernet interface
  MIB_IF_TOKEN_RING_ADAPTERTYPE = 9;        // Token ring interface
  MIB_IF_FDDI_ADAPTERTYPE       = 15;       // Fiber Distributed Data Interface
  MIB_IF_PPP_ADAPTERTYPE        = 23;       // Point-to-Point Protocol interface
  MIB_IF_LOOPBACK_ADAPTERTYPE   = 24;       // Software loopback
  MIB_IF_ATM_ADAPTERTYPE        = 37;       // Asynchronous Transfer Mode interface
  MIB_IF_TYPE_IEEE80211         = 71;       // IEEE 802.11 wireless network interface
  MIB_IF_TYPE_TUNNEL            = 131;      // A tunnel type encapsulation network interface.
  MIB_IF_TYPE_IEEE1394          = 144;      // IEEE 1394 (Firewire) high performance serial bus network interface
  MIB_IF_TYPE_IEEE80216_WMAN    = 237;      // A mobile broadband interface for WiMax devices.
  MIB_IF_TYPE_WWANPP            = 243;      // A mobile broadband interface for GSM-based devices.
  MIB_IF_TYPE_WWANPP2           = 244;      // An mobile broadband interface for CDMA-based devices.

  MIB_IF_OPER_STATUS_NON_OPERATIONAL     = 0;
  MIB_IF_OPER_STATUS_UNREACHABLE         = 1;
  MIB_IF_OPER_STATUS_DISCONNECTED        = 2;
  MIB_IF_OPER_STATUS_CONNECTING          = 3;
  MIB_IF_OPER_STATUS_CONNECTED           = 4;
  MIB_IF_OPER_STATUS_OPERATIONAL         = 5;

  MIB_IF_ADMIN_STATUS_UP         = 1;
  MIB_IF_ADMIN_STATUS_DOWN       = 2;
  MIB_IF_ADMIN_STATUS_TESTING    = 3;

  ANY_SIZE      = 1;

  IP_ADAPTER_DDNS_ENABLED            = $01;
  IP_ADAPTER_REGISTER_ADAPTER_SUFFIX = $02;
  IP_ADAPTER_DHCP_ENABLED            = $04;
  IP_ADAPTER_RECEIVE_ONLY            = $08;
  IP_ADAPTER_NO_MULTICAST            = $10;
  IP_ADAPTER_IPV6_OTHER_STATEFUL_CONFIG = $20;
  IP_ADAPTER_NETBIOS_OVER_TCPIP_ENABLED = $0040;
  IP_ADAPTER_IPV4_ENABLED = $0080;
  IP_ADAPTER_IPV6_ENABLED = $0100;
  IP_ADAPTER_IPV6_MANAGE_ADDRESS_CONFIG = $0200;

  IF_MAX_STRING_SIZE = 256;
  IF_MAX_PHYS_ADDRESS_LENGTH = 32;

  MIB_IPADDR_PRIMARY = $0001;  //Primary IP address
  MIB_IPADDR_DYNAMIC = $0004;  //Dynamic IP address
  MIB_IPADDR_DISCONNECTED = $0008; //Address is on disconnected interface
  MIB_IPADDR_DELETED = $0040;  //Address is being deleted
  MIB_IPADDR_TRANSIENT = $0080;  //Transient address

  MAX_DHCPV6_DUID_LENGTH = 130;
  MAX_DNS_SUFFIX_STRING_LENGTH = 256;


  GAA_FLAG_SKIP_UNICAST = $0001; //Do not return unicast addresses.
  GAA_FLAG_SKIP_ANYCAST = $0002; //Do not return IPv6 anycast addresses.
  GAA_FLAG_SKIP_MULTICAST = $0004; //Do not return multicast addresses.
  GAA_FLAG_SKIP_DNS_SERVER = $0008; //Do not return addresses of DNS servers.
  GAA_FLAG_INCLUDE_PREFIX = $0010; //Return a list of IP address prefixes on this adapter. When this flag is set, IP address prefixes are returned for both IPv6 and IPv4 addresses.
                                   // This flag is supported on Windows XP with SP1 and later.
  GAA_FLAG_SKIP_FRIENDLY_NAME = $0020; //Do not return the adapter friendly name.
  GAA_FLAG_INCLUDE_WINS_INFO = $0040; //Return addresses of Windows Internet Name Service (WINS) servers.
                                      //This flag is supported on Windows Vista and later.
  GAA_FLAG_INCLUDE_GATEWAYS = $0080; //Return the addresses of default gateways.
                                     //This flag is supported on Windows Vista and later.
  GAA_FLAG_INCLUDE_ALL_INTERFACES = $0100; //Return addresses for all NDIS interfaces.
                                           //This flag is supported on Windows Vista and later.
  GAA_FLAG_INCLUDE_ALL_COMPARTMENTS = $0200; //Return addresses in all routing compartments.
                                             //This flag is not currently supported and reserved for future use.
  GAA_FLAG_INCLUDE_TUNNEL_BINDINGORDER = $0400; //Return the adapter addresses sorted in tunnel binding order. This flag is supported on Windows Vista and later.

  TCP_TABLE_OWNER_PID_ALL = 5;
  UDP_TABLE_OWNER_PID = 1;

  MIB_TCP_STATE_CLOSED = 1;
  MIB_TCP_STATE_LISTEN = 2;
  MIB_TCP_STATE_SYN_SENT = 3;
  MIB_TCP_STATE_SYN_RCVD = 4;
  MIB_TCP_STATE_ESTAB = 5;
  MIB_TCP_STATE_FIN_WAIT1 = 6;
  MIB_TCP_STATE_FIN_WAIT2 = 7;
  MIB_TCP_STATE_CLOSE_WAIT = 8;
  MIB_TCP_STATE_CLOSING = 9;
  MIB_TCP_STATE_LAST_ACK = 10;
  MIB_TCP_STATE_TIME_WAIT = 11;
  MIB_TCP_STATE_DELETE_TCB = 12;

type
  TPHYS_ADDRESS = array[1..MAX_ADAPTER_ADDRESS_LENGTH] of Byte;

  PIP_ADAPTER_INDEX_MAP = ^TIP_ADAPTER_INDEX_MAP;
  TIP_ADAPTER_INDEX_MAP = record
    Index: DWORD;                                   // adapter index
    Name: array[1..MAX_ADAPTER_NAME_LENGTH] of WCHAR;      // name of the adapter
  end;

  PIP_INTERFACE_INFO = ^TIP_INTERFACE_INFO;
  TIP_INTERFACE_INFO = record
    NumAdapters: integer;                           // number of adapters in array
    Adapter: array[0..ANY_SIZE - 1] of TIP_ADAPTER_INDEX_MAP;  // adapter indices and names
  end;

  PMIB_IPNETROW = ^TMIB_IPNETROW;
  TMIB_IPNETROW = packed record
    dwIndex: DWORD;
    dwPhysAddrLen: DWORD;
    bPhysAddr: TPHYS_ADDRESS;
    dwAddr: DWORD;
    dwType: DWORD;
  end;
  //
  PMIB_IPNETTABLE = ^TMIB_IPNETTABLE;
  TMIB_IPNETTABLE = packed record
    dwNumEntries: DWORD;
    Table: array[0..ANY_SIZE - 1] of TMIB_IPNETROW;
  end;

  PMIB_IPADDRROW = ^TMIB_IPADDRROW;
  TMIB_IPADDRROW = packed record
    dwAddr: DWORD;
    dwIndex: DWORD;
    dwMask: DWORD;
    dwBCastAddr: DWORD;
    dwReasmSize: DWORD;
    Unused1,
    wType: WORD;
  end;
  //
  PMIB_IPADDRTABLE = ^TMIB_IPADDRTABLE;
  TMIB_IPADDRTABLE = packed record
    dwNumEntries: DWORD;
    Table: array[0..ANY_SIZE - 1] of TMIB_IPADDRROW;
  end;

  PMIB_TCPSTATS = ^TMIB_TCPSTATS;
  TMIB_TCPSTATS = record
    dwRtoAlgorithm:  DWORD; // time-out algorithm
    dwRtoMin:        DWORD; // minimum time-out
    dwRtoMax:        DWORD; // maximum time-out
    dwMaxConn:       DWORD; // maximum connections
    dwActiveOpens:   DWORD; // active opens
    dwPassiveOpens:  DWORD; // passive opens
    dwAttemptFails:  DWORD; // failed attempts
    dwEstabResets:   DWORD; // established connections reset
    dwCurrEstab:     DWORD; // established connections
    dwInSegs:        DWORD; // segments received
    dwOutSegs:       DWORD; // segment sent
    dwRetransSegs:   DWORD; // segments retransmitted
    dwInErrs:        DWORD; // incoming errors
    dwOutRsts:       DWORD; // outgoing resets
    dwNumConns:      DWORD; // cumulative connections
  end;

  PMIB_IFROW = ^TMIB_IFROW;
  TMIB_IFROW = record
    wszName: array[1..MAX_INTERFACE_NAME_LEN] of WCHAR;
    dwIndex:                                     DWORD;  // index of the interface
    dwType:                                      DWORD;  // type of interface
    dwMtu:                                       DWORD;  // max transmission unit
    dwSpeed:                                     DWORD;  // speed of the interface
    dwPhysAddrLen:                               DWORD;  // length of physical address
    bPhysAddr:      TPHYS_ADDRESS;   // physical address of adapter
    dwAdminStatus:                               DWORD;  // administrative status
    dwOperStatus:                                DWORD;  // operational status
    dwLastChange:                                DWORD;  // last time operational status changed
    dwInOctets:                                  DWORD;  // octets received
    dwInUcastPkts:                               DWORD;  // unicast packets received
    dwInNUcastPkts:                              DWORD;  // non-unicast packets received
    dwInDiscards:                                DWORD;  // received packets discarded
    dwInErrors:                                  DWORD;  // erroneous packets received
    dwInUnknownProtos:                           DWORD;  // unknown protocol packets received
    dwOutOctets:                                 DWORD;  // octets sent
    dwOutUcastPkts:                              DWORD;  // unicast packets sent
    dwOutNUcastPkts:                             DWORD;  // non-unicast packets sent
    dwOutDiscards:                               DWORD;  // outgoing packets discarded
    dwOutErrors:                                 DWORD;  // erroneous packets sent
    dwOutQLen:                                   DWORD;  // output queue length
    dwDescrLen:                                  DWORD;  // length of bDescr member
    bDescr:          array[1..MAXLEN_IFDESCR] of BYTE;   // interface description
  end;

  PMIB_IFTABLE = ^TMIB_IFTABLE;
  TMIB_IFTABLE = record
    dwNumEntries: DWORD;                 // number of entries in table
    Table: array[0..0] of TMIB_IFROW;    // array of interface entries
  end;

  PNET_LUID = ^NET_LUID;
  NET_LUID = {$IFDEF RAD9PLUS}ULONG64{$ELSE}int64{$ENDIF};

  NET_IFINDEX = ULONG;

  NDIS_MEDIUM = (
    NdisMedium802_3,
    NdisMedium802_5,
    NdisMediumFddi,
    NdisMediumWan,
    NdisMediumLocalTalk,
    NdisMediumDix, // defined for convenience, not a real medium
    NdisMediumArcnetRaw,
    NdisMediumArcnet878_2,
    NdisMediumAtm,
    NdisMediumWirelessWan,
    NdisMediumIrda,
    NdisMediumBpc,
    NdisMediumCoWan,
    NdisMedium1394,
    NdisMediumInfiniBand,
    NdisMediumMax // Not a real medium, defined as an upper-bound
  );

  NDIS_PHYSICAL_MEDIUM = (
    NdisPhysicalMediumUnspecified,
    NdisPhysicalMediumWirelessLan,
    NdisPhysicalMediumCableModem,
    NdisPhysicalMediumPhoneLine,
    NdisPhysicalMediumPowerLine,
    NdisPhysicalMediumDSL, // includes ADSL and UADSL (G.Lite)
    NdisPhysicalMediumFibreChannel,
    NdisPhysicalMedium1394,
    NdisPhysicalMediumWirelessWan,
    NdisPhysicalMediumNative802_11,
    NdisPhysicalMediumBluetooth,
    NdisPhysicalMediumInfiniband,
    NdisPhysicalMediumWiMax,
    NdisPhysicalMediumUWB,
    NdisPhysicalMedium802_3,
    NdisPhysicalMedium802_5,
    NdisPhysicalMediumIrda,
    NdisPhysicalMediumWiredWAN,
    NdisPhysicalMediumWiredCoWan,
    NdisPhysicalMediumOther,
    NdisPhysicalMediumMax // Not a real physical type, defined as an upper-bound
  );

  NET_IF_ACCESS_TYPE = (
    NET_IF_ACCESS_LOOPBACK = 1,
    NET_IF_ACCESS_BROADCAST = 2,
    NET_IF_ACCESS_POINT_TO_POINT = 3,
    NET_IF_ACCESS_POINT_TO_MULTI_POINT = 4,
    NET_IF_ACCESS_MAXIMUM = 5
  );

  TUNNEL_TYPE = (
    TUNNEL_TYPE_NONE = 0,
    TUNNEL_TYPE_OTHER = 1,
    TUNNEL_TYPE_DIRECT = 2,
    TUNNEL_TYPE_6TO4 = 11,
    TUNNEL_TYPE_ISATAP = 13,
    TUNNEL_TYPE_TEREDO = 14
  );

  NET_IF_DIRECTION_TYPE = (
    NET_IF_DIRECTION_SENDRECEIVE,
    NET_IF_DIRECTION_SENDONLY,
    NET_IF_DIRECTION_RECEIVEONLY,
    NET_IF_DIRECTION_MAXIMUM
  );

  NET_IF_ADMIN_STATUS = (
    NET_IF_ADMIN_STATUS_UP = 1,
    NET_IF_ADMIN_STATUS_DOWN = 2,
    NET_IF_ADMIN_STATUS_TESTING = 3
  );

  IF_OPER_STATUS = (
    IfOperStatusUp,
    IfOperStatusDown,
    IfOperStatusTesting,
    IfOperStatusUnknown,
    IfOperStatusDormant,
    IfOperStatusNotPresent,
    IfOperStatusLowerLayerDown
  );
  TIfOperStatus = IF_OPER_STATUS;

  NET_IF_MEDIA_CONNECT_STATE = (
    MediaConnectStateUnknown,
    MediaConnectStateConnected,
    MediaConnectStateDisconnected
  );

  NET_IF_CONNECTION_TYPE = (
    NET_IF_CONNECTION_DEDICATED = 1,
    NET_IF_CONNECTION_PASSIVE = 2,
    NET_IF_CONNECTION_DEMAND = 3,
    NET_IF_CONNECTION_MAXIMUM = 4
  );
{$Z-}

  TInterfaceAndOperStatusFlags = (
    HardwareInterface = $1,
    FilterInterface = $2,
    ConnectorPresent = $4,
    NotAuthenticated = $8,
    NotMediaConnected = $16,
    Paused = $32,
    LowPower = $64,
    EndPointInterface = $128
  );

  PMIB_IF_ROW2 = ^MIB_IF_ROW2;
  _MIB_IF_ROW2 = record
    InterfaceLuid: NET_LUID;
    InterfaceIndex: NET_IFINDEX;
    InterfaceGuid: TGUID;
    Alias:array[0..256] of WCHAR;
    Description:array[0..256] of WCHAR;
    PhysicalAddressLength: ULONG;
    PhysicalAddress: array[0..31] of UCHAR;
    PermanentPhysicalAddress:array[0..31] of UCHAR;
    Mtu: ULONG;
    Typ: ULONG;
    TunnelType: ULONG;//TUNNEL_TYPE;
    MediaType: ULONG;//NDIS_MEDIUM;
    PhysicalMediumType: ULONG;//NDIS_PHYSICAL_MEDIUM;
    AccessType: ULONG;//NET_IF_ACCESS_TYPE;
    DirectionType: ULONG;//NET_IF_DIRECTION_TYPE;
    InterfaceAndOperStatusFlags: Cardinal; { TInterfaceAndOperStatusFlags }
    OperStatus: ULONG;//IF_OPER_STATUS;
    AdminStatus: ULONG;//NET_IF_ADMIN_STATUS;
    MediaConnectState: ULONG;//NET_IF_MEDIA_CONNECT_STATE;
    NetworkGuid: TGUID;
    ConnectionType: ULONG;//NET_IF_CONNECTION_TYPE;
    TransmitLinkSpeed: ULONG64;
    ReceiveLinkSpeed: ULONG64;
    InOctets: ULONG64;
    InUcastPkts: ULONG64;
    InNUcastPkts: ULONG64;
    InDiscards: ULONG64;
    InErrors: ULONG64;
    InUnknownProtos: ULONG64;
    InUcastOctets: ULONG64;
    InMulticastOctets: ULONG64;
    InBroadcastOctets: ULONG64;
    OutOctets: ULONG64;
    OutUcastPkts: ULONG64;
    OutNUcastPkts: ULONG64;
    OutDiscards: ULONG64;
    OutErrors: ULONG64;
    OutUcastOctets: ULONG64;
    OutMulticastOctets: ULONG64;
    OutBroadcastOctets: ULONG64;
    OutQLen: ULONG64;
  end;
  MIB_IF_ROW2 = _MIB_IF_ROW2;
  TMIB_IFROW2 = MIB_IF_ROW2;
  PMIB_IFROW2 = ^TMIB_IFROW2;

  PMIB_IF_TABLE2 = ^MIB_IF_TABLE2;
  _MIB_IF_TABLE2 = packed record
    NumEntries: Cardinal;
    Table: array[0..ANY_SIZE - 1] of _MIB_IF_ROW2;
  end;
  MIB_IF_TABLE2 = _MIB_IF_TABLE2;
  TMIB_IFTABLE2 = MIB_IF_TABLE2;
  PMIB_IFTABLE2 = ^TMIB_IFTABLE2;

  PIP_MASK_STRING = ^IP_MASK_STRING;
  IP_ADDRESS_STRING = record
    S: array [0..15] of AnsiChar;
  end;
  PIP_ADDRESS_STRING = ^IP_ADDRESS_STRING;
  IP_MASK_STRING = IP_ADDRESS_STRING;
  TIpAddressString = IP_ADDRESS_STRING;
  PIpAddressString = PIP_MASK_STRING;

  PIP_ADDR_STRING = ^IP_ADDR_STRING;
  _IP_ADDR_STRING = record
    Next: PIP_ADDR_STRING;
    IpAddress: IP_ADDRESS_STRING;
    IpMask: IP_MASK_STRING;
    Context: DWORD;
  end;
  IP_ADDR_STRING = _IP_ADDR_STRING;
  TIpAddrString = IP_ADDR_STRING;
  PIpAddrString = PIP_ADDR_STRING;

  PIP_ADAPTER_INFO = ^IP_ADAPTER_INFO;
  _IP_ADAPTER_INFO = record
    Next: PIP_ADAPTER_INFO;
    ComboIndex: DWORD;
    AdapterName: array [0..MAX_ADAPTER_NAME_LENGTH + 3] of AnsiChar;
    Description: array [0..MAX_ADAPTER_DESCRIPTION_LENGTH + 3] of AnsiChar;
    AddressLength: DWORD;
    Address: array [0..MAX_ADAPTER_ADDRESS_LENGTH - 1] of BYTE;
    Index: DWORD;
    Type_: DWORD;
    DhcpEnabled: DWORD;
    CurrentIpAddress: PIP_ADDR_STRING;
    IpAddressList: IP_ADDR_STRING;
    GatewayList: IP_ADDR_STRING;
    DhcpServer: IP_ADDR_STRING;
    HaveWins: BOOL;
    PrimaryWinsServer: IP_ADDR_STRING;
    SecondaryWinsServer: IP_ADDR_STRING;
    LeaseObtained: time_t;
    LeaseExpires: time_t;
  end;
  IP_ADAPTER_INFO = _IP_ADAPTER_INFO;
  TIpAdapterInfo = IP_ADAPTER_INFO;
  PIpAdapterInfo = PIP_ADAPTER_INFO;

  PIP_PER_ADAPTER_INFO = ^IP_PER_ADAPTER_INFO;
  _IP_PER_ADAPTER_INFO = record
    AutoconfigEnabled: DWORD;
    AutoconfigActive: DWORD;
    CurrentDnsServer: PIP_ADDR_STRING;
    DnsServerList: IP_ADDR_STRING;
  end;
  IP_PER_ADAPTER_INFO = _IP_PER_ADAPTER_INFO;
  TIpPerAdapterInfo = IP_PER_ADAPTER_INFO;
  PIpPerAdapterInfo = PIP_PER_ADAPTER_INFO;

  PFIXED_INFO = ^FIXED_INFO;
  FIXED_INFO = record
    HostName: array [0..MAX_HOSTNAME_LEN + 3] of AnsiChar;
    DomainName: array[0..MAX_DOMAIN_NAME_LEN + 3] of AnsiChar;
    CurrentDnsServer: PIP_ADDR_STRING;
    DnsServerList: IP_ADDR_STRING;
    NodeType: DWORD;
    ScopeId: array [0..MAX_SCOPE_ID_LEN + 3] of AnsiChar;
    EnableRouting: DWORD;
    EnableProxy: DWORD;
    EnableDns: DWORD;
  end;
  TFixedInfo = FIXED_INFO;
  PFixedInfo = PFIXED_INFO;

  PMIB_TCPROW = ^TMIB_TCPROW;
  TMIB_TCPROW = packed record
    dwState: DWORD;
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
    dwRemoteAddr: DWORD;
    dwRemotePort: DWORD;
  end;

  PMIB_TCPTABLE = ^TMIB_TCPTABLE;
  TMIB_TCPTABLE = packed record
    dwNumEntries: DWORD;
    Table: array[0..0] of TMIB_TCPROW;
  end;

  PMIB_UDPROW = ^TMIB_UDPROW;
  TMIB_UDPROW = packed record
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
  end;

  PMIB_UDPTABLE = ^TMIB_UDPTABLE;
  TMIB_UDPTABLE = packed record
    dwNumEntries: DWORD;
    Table: array[0..ANY_SIZE - 1] of TMIB_UDPROW;
  end;

  PMIB_UDPSTATS = ^TMIB_UDPSTATS;
  TMIB_UDPSTATS = packed record
    dwInDatagrams: DWORD;
    dwNoPorts: DWORD;
    dwInErrors: DWORD;
    dwOutDatagrams: DWORD;
    dwNumAddrs: DWORD;
  end;

  PMIB_IPSTATS = ^TMIB_IPSTATS;
  TMIB_IPSTATS = packed record
    dwForwarding: DWORD;
    dwDefaultTTL: DWORD;
    dwInReceives: DWORD;
    dwInHdrErrors: DWORD;
    dwInAddrErrors: DWORD;
    dwForwDatagrams: DWORD;
    dwInUnknownProtos: DWORD;
    dwInDiscards: DWORD;
    dwInDelivers: DWORD;
    dwOutRequests: DWORD;
    dwRoutingDiscards: DWORD;
    dwOutDiscards: DWORD;
    dwOutNoRoutes: DWORD;
    dwReasmTimeOut: DWORD;
    dwReasmReqds: DWORD;
    dwReasmOKs: DWORD;
    dwReasmFails: DWORD;
    dwFragOKs: DWORD;
    dwFragFails: DWORD;
    dwFragCreates: DWORD;
    dwNumIf: DWORD;
    dwNumAddr: DWORD;
    dwNumRoutes: DWORD;
  end;

  PMIB_TCPROW_EX = ^TMIB_TCPROW_EX ;
  TMIB_TCPROW_EX = packed record
    dwState: DWORD;
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
    dwRemoteAddr: DWORD;
    dwRemotePort: DWORD;
    dwProcessID: DWORD;
  end;

  PMIB_TCPTABLE_EX = ^TMIB_TCPTABLE_EX;
  TMIB_TCPTABLE_EX = packed record
    dwNumEntries: Integer;
    Table: array [0..0] of TMIB_TCPROW_EX;
  end;

  PMIB_UDPROW_EX = ^TMIB_UDPROW_EX;
  TMIB_UDPROW_EX = packed record
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
    dwProcessID: DWORD;
  end;

  PMIB_UDPTABLE_EX = ^TMIB_UDPTABLE_EX;
  TMIB_UDPTABLE_EX = packed record
      dwNumEntries: Integer;
      Table: array [0..0] of TMIB_UDPROW_EX;
  end;

  PIP_ADAPTER_DNS_SERVER_ADDRESS = ^_IP_ADAPTER_DNS_SERVER_ADDRESS;
  _IP_ADAPTER_DNS_SERVER_ADDRESS = record
    Union: record
      case Integer of
        0: (
          Alignment: ULONGLONG);
        1: (
          Length: ULONG;
          Reserved: DWORD);
    end;
    Next: PIP_ADAPTER_DNS_SERVER_ADDRESS;
    Address: SOCKET_ADDRESS;
  end;
  IP_ADAPTER_DNS_SERVER_ADDRESS = _IP_ADAPTER_DNS_SERVER_ADDRESS;
  TIpAdapterDnsServerAddress = IP_ADAPTER_DNS_SERVER_ADDRESS;
  PIpAdapterDnsServerAddress = PIP_ADAPTER_DNS_SERVER_ADDRESS;

  PIP_ADAPTER_PREFIX = ^IP_ADAPTER_PREFIX;
  _IP_ADAPTER_PREFIX = record
    Union: record
    case Integer of
      0: (
        Alignment: ULONGLONG);
      1: (
        Length: ULONG;
        Flags: DWORD);
    end;
    Next: PIP_ADAPTER_PREFIX;
    Address: SOCKET_ADDRESS;
    PrefixLength: ULONG;
  end;
  IP_ADAPTER_PREFIX = _IP_ADAPTER_PREFIX;
  TIpAdapterPrefix = IP_ADAPTER_PREFIX;
  PIpAdapterPrefix = PIP_ADAPTER_PREFIX;

  IP_PREFIX_ORIGIN = (
    IpPrefixOriginOther,
    IpPrefixOriginManual,
    IpPrefixOriginWellKnown,
    IpPrefixOriginDhcp,
    IpPrefixOriginRouterAdvertisement);
  TIpPrefixOrigin = IP_PREFIX_ORIGIN;

  IP_SUFFIX_ORIGIN = (
    IpSuffixOriginOther,
    IpSuffixOriginManual,
    IpSuffixOriginWellKnown,
    IpSuffixOriginDhcp,
    IpSuffixOriginLinkLayerAddress,
    IpSuffixOriginRandom);
  TIpSuffixOrigin = IP_SUFFIX_ORIGIN;

  IP_DAD_STATE = (
    IpDadStateInvalid,
    IpDadStateTentative,
    IpDadStateDuplicate,
    IpDadStateDeprecated,
    IpDadStatePreferred);
  TIpDadState = IP_DAD_STATE;

  PIP_ADAPTER_UNICAST_ADDRESS = ^_IP_ADAPTER_UNICAST_ADDRESS;
  _IP_ADAPTER_UNICAST_ADDRESS = record
    Union: record
      case Integer of
        0: (
          Alignment: ULONGLONG);
        1: (
          Length: ULONG;
          Flags: DWORD);
    end;
    Next: PIP_ADAPTER_UNICAST_ADDRESS;
    Address: SOCKET_ADDRESS;

    PrefixOrigin: IP_PREFIX_ORIGIN;
    SuffixOrigin: IP_SUFFIX_ORIGIN;
    DadState: IP_DAD_STATE;

    ValidLifetime: ULONG;
    PreferredLifetime: ULONG;
    LeaseLifetime: ULONG;
  end;
  IP_ADAPTER_UNICAST_ADDRESS = _IP_ADAPTER_UNICAST_ADDRESS;
  TIpAdapterUnicastAddress = IP_ADAPTER_UNICAST_ADDRESS;
  PIpAdapterUnicastAddress = PIP_ADAPTER_UNICAST_ADDRESS;

  PIP_ADAPTER_ANYCAST_ADDRESS = ^_IP_ADAPTER_ANYCAST_ADDRESS;
  _IP_ADAPTER_ANYCAST_ADDRESS = record
    Union: record
      case Integer of
        0: (
          Alignment: ULONGLONG);
        1: (
          Length: ULONG;
          Flags: DWORD);
    end;
    Next: PIP_ADAPTER_ANYCAST_ADDRESS;
    Address: SOCKET_ADDRESS;
  end;
  IP_ADAPTER_ANYCAST_ADDRESS = _IP_ADAPTER_ANYCAST_ADDRESS;
  TIpAdapterAnycaseAddress = IP_ADAPTER_ANYCAST_ADDRESS;
  PIpAdapterAnycaseAddress = PIP_ADAPTER_ANYCAST_ADDRESS;

  PIP_ADAPTER_MULTICAST_ADDRESS = ^_IP_ADAPTER_MULTICAST_ADDRESS;
  _IP_ADAPTER_MULTICAST_ADDRESS = record
    Union: record
      case Integer of
        0: (
          Alignment: ULONGLONG);
        1: (
          Length: ULONG;
          Flags: DWORD);
    end;
    Next: PIP_ADAPTER_MULTICAST_ADDRESS;
    Address: SOCKET_ADDRESS;
  end;
  IP_ADAPTER_MULTICAST_ADDRESS = _IP_ADAPTER_MULTICAST_ADDRESS;
  TIpAdapterMulticastAddress = IP_ADAPTER_MULTICAST_ADDRESS;
  PIpAdapterMulticastAddress = PIP_ADAPTER_MULTICAST_ADDRESS;

  PIP_ADAPTER_WINS_SERVER_ADDRESS_LH = ^_IP_ADAPTER_WINS_SERVER_ADDRESS_LH;
  _IP_ADAPTER_WINS_SERVER_ADDRESS_LH = record
    Union: record
      case Integer of
        0: (
          Alignment: ULONGLONG);
        1: (
          Length: ULONG;
          Flags: DWORD);
    end;
    Next: PIP_ADAPTER_WINS_SERVER_ADDRESS_LH;
    Address: SOCKET_ADDRESS;
  end;
  IP_ADAPTER_WINS_SERVER_ADDRESS_LH = _IP_ADAPTER_WINS_SERVER_ADDRESS_LH;
  TIpAdapterWINSServerAddressLh = IP_ADAPTER_WINS_SERVER_ADDRESS_LH;
  PIpAdapterWINSServerAddressLh = PIP_ADAPTER_WINS_SERVER_ADDRESS_LH;

  PIP_ADAPTER_GATEWAY_ADDRESS_LH = ^_IP_ADAPTER_GATEWAY_ADDRESS_LH;
  _IP_ADAPTER_GATEWAY_ADDRESS_LH = record
    Union: record
      case Integer of
        0: (
          Alignment: ULONGLONG);
        1: (
          Length: ULONG;
          Flags: DWORD);
    end;
    Next: PIP_ADAPTER_GATEWAY_ADDRESS_LH;
    Address: SOCKET_ADDRESS;
  end;
  IP_ADAPTER_GATEWAY_ADDRESS_LH = _IP_ADAPTER_GATEWAY_ADDRESS_LH;
  TIpAdapterGatewayAddressLh = IP_ADAPTER_GATEWAY_ADDRESS_LH;
  PIpAdapterGatewayAddressLh = PIP_ADAPTER_GATEWAY_ADDRESS_LH;

  PIP_ADAPTER_DNS_SUFFIX = ^_IP_ADAPTER_DNS_SUFFIX;
  _IP_ADAPTER_DNS_SUFFIX = record
    Next: PIP_ADAPTER_DNS_SUFFIX;
    Str: array[0..MAX_DNS_SUFFIX_STRING_LENGTH-1] of WCHAR;
  end;
  IP_ADAPTER_DNS_SUFFIX = _IP_ADAPTER_DNS_SUFFIX;
  TIpAdapterDNSSuffix = IP_ADAPTER_DNS_SUFFIX;
  PIpAdapterDNSSuffix = PIP_ADAPTER_DNS_SUFFIX;

  PIP_ADAPTER_ADDRESSES = ^_IP_ADAPTER_ADDRESSES;
  _IP_ADAPTER_ADDRESSES = record
    Union: record
      case Integer of
        0: (
          Alignment: ULONGLONG);
        1: (
          Length: ULONG;
          IfIndex: DWORD);
    end;
    Next: PIP_ADAPTER_ADDRESSES;
    AdapterName: PAnsiChar;
    FirstUnicastAddress: PIP_ADAPTER_UNICAST_ADDRESS;
    FirstAnycastAddress: PIP_ADAPTER_ANYCAST_ADDRESS;
    FirstMulticastAddress: PIP_ADAPTER_MULTICAST_ADDRESS;
    FirstDnsServerAddress: PIP_ADAPTER_DNS_SERVER_ADDRESS;
    DnsSuffix: PWCHAR;
    Description: PWCHAR;
    FriendlyName: PWCHAR;
    PhysicalAddress: array [0..MAX_ADAPTER_ADDRESS_LENGTH - 1] of BYTE;
    PhysicalAddressLength: DWORD;
    Flags: DWORD;
    Mtu: DWORD;
    IfType: DWORD;
    OperStatus: IF_OPER_STATUS;
    Ipv6IfIndex: DWORD;
    ZoneIndices: array [0..15] of DWORD;
    FirstPrefix: PIP_ADAPTER_PREFIX;
    TransmitLinkSpeed: {$IFDEF RAD9PLUS}ULONG64{$ELSE}int64{$ENDIF};
    ReceiveLinkSpeed: {$IFDEF RAD9PLUS}ULONG64{$ELSE}int64{$ENDIF};
    FirstWinsServerAddress: PIP_ADAPTER_WINS_SERVER_ADDRESS_LH;
    FirstGatewayAddress: PIP_ADAPTER_GATEWAY_ADDRESS_LH;
    Ipv4Metric: ULONG;
    Ipv6Metric: ULONG;
    Luid: NET_LUID;
    Dhcpv4Server: SOCKET_ADDRESS;
    CompartmentId: Cardinal;
    NetworkGuid: TGUID;
    ConnectionType: ULONG;
    TunnelType: ULONG;
    Dhcpv6Server: SOCKET_ADDRESS;
    Dhcpv6ClientDuid: array[0..MAX_DHCPV6_DUID_LENGTH-1] of Byte;
    Dhcpv6ClientDuidLength: ULONG;
    Dhcpv6Iaid: ULONG;
    FirstDnsSuffix: PIP_ADAPTER_DNS_SUFFIX;
  end;
  IP_ADAPTER_ADDRESSES = _IP_ADAPTER_ADDRESSES;
  TIpAdapterAddresses = IP_ADAPTER_ADDRESSES;
  PIpAdapterAddresses = PIP_ADAPTER_ADDRESSES;

  PMIB_IPINTERFACE_ROW = ^MIB_IPINTERFACE_ROW;
  _MIB_IPINTERFACE_ROW = record
    Family: ULONG;
    InterfaceLuid: NET_LUID;
    InterfaceIndex: NET_IFINDEX;
    MaxReassemblySize,
    InterfaceIdentifier,
    MinRouterAdvertisementInterval,
    MaxRouterAdvertisementInterval: Cardinal;
    AdvertisingEnabled,
    ForwardingEnabled,
    WeakHostSend,
    WeakHostReceive,
    UseAutomaticMetric,
    UseNeighborUnreachabilityDetection,
    ManagedAddressConfigurationSupported,
    OtherStatefulConfigurationSupported,
    AdvertiseDefaultRoute: ByteBool;
    RouterDiscoveryBehavior: ULONG;
    DadTransmits,
    BaseReachableTime,
    RetransmitTime,
    PathMtuDiscoveryTimeout: Cardinal;
    LinkLocalAddressBehavior: ULONG;
    LinkLocalAddressTimeout,
    ZoneIndices: array[0..0] of ULONG;
    SitePrefixLength,
    Metric,
    NlMtu: Cardinal;
    Connected,
    SupportsWakeUpPatterns,
    SupportsNeighborDiscovery,
    SupportsRouterDiscovery: ByteBool;
    ReachableTime: Cardinal;
    TransmitOffload: ULONG;
    ReceiveOffload: ULONG;
    DisableDefaultRoutes: ByteBool;
  end;
  MIB_IPINTERFACE_ROW = _MIB_IPINTERFACE_ROW;
  TMibIpInterfaceRow = MIB_IPINTERFACE_ROW;
  PMibIpInterfaceRow = PMIB_IPINTERFACE_ROW;

  TCP_TABLE_CLASS = Integer;

  MIB_TCPROW_OWNER_PID = packed record
    dwState     : DWORD;
    dwLocalAddr : DWORD;
    dwLocalPort : DWORD;
    dwRemoteAddr: DWORD;
    dwRemotePort: DWORD;
    dwOwningPid : DWORD;
  end;

  TMibTcpRowOwnerPid = MIB_TCPROW_OWNER_PID;
  PMibTcpRowOwnerPid = ^TMibTcpRowOwnerPid;

  PMIB_TCPTABLE_OWNER_PID  = ^MIB_TCPTABLE_OWNER_PID;
  MIB_TCPTABLE_OWNER_PID = packed record
   dwNumEntries: DWORD;
   table: array [0..ANY_SIZE - 1] of MIB_TCPROW_OWNER_PID;
  end;

  MIB_TCP6ROW_OWNER_PID = packed record
    ucLocalAddr: TIPv6Byte;
    dwLocalScopeId: DWORD;
    dwLocalPort: DWORD;
    ucRemoteAddr: TIPv6Byte;
    dwRemoteScopeId: DWORD;
    dwRemotePort: DWORD;
    dwState: DWORD;
    dwOwningPid: DWORD;
  end;

  TMibTcp6RowOwnerPid = MIB_TCP6ROW_OWNER_PID;
  PMibTcp6RowOwnerPid = ^TMibTcp6RowOwnerPid;

  PMIB_TCP6TABLE_OWNER_PID  = ^MIB_TCP6TABLE_OWNER_PID;
  MIB_TCP6TABLE_OWNER_PID = packed record
   dwNumEntries: DWORD;
   table: array [0..ANY_SIZE - 1] of MIB_TCP6ROW_OWNER_PID;
  end;

  UDP_TABLE_CLASS = Integer;

  MIB_UDPROW_OWNER_PID = packed record
    dwLocalAddr : DWORD;
    dwLocalPort : DWORD;
    dwOwningPid : DWORD;
  end;

  TMibUdpRowOwnerPid = MIB_UDPROW_OWNER_PID;
  PMibUdpRowOwnerPid = ^TMibUdpRowOwnerPid;

  PMIB_UDPTABLE_OWNER_PID  = ^MIB_UDPTABLE_OWNER_PID;
  MIB_UDPTABLE_OWNER_PID = packed record
   dwNumEntries: DWORD;
   table: array [0..ANY_SIZE - 1] of MIB_UDPROW_OWNER_PID;
  end;

  MIB_UDP6ROW_OWNER_PID = packed record
    ucLocalAddr: TIPv6Byte;
    dwLocalScopeId: DWORD;
    dwLocalPort: DWORD;
    dwOwningPid : DWORD;
  end;

  TMibUdp6RowOwnerPid = MIB_UDP6ROW_OWNER_PID;
  PMibUdp6RowOwnerPid = ^TMibUdp6RowOwnerPid;

  PMIB_UDP6TABLE_OWNER_PID  = ^MIB_UDP6TABLE_OWNER_PID;
  MIB_UDP6TABLE_OWNER_PID = packed record
   dwNumEntries: DWORD;
   table: array [0..ANY_SIZE - 1] of MIB_UDP6ROW_OWNER_PID;
  end;

  IP_OPTION_INFORMATION = record
    Ttl: UCHAR;          // Time To Live
    Tos: UCHAR;          // Type Of Service
    Flags: UCHAR;        // IP header flags
    OptionsSize: UCHAR;  // Size in bytes of options data
    OptionsData: PUCHAR; // Pointer to options data
  end;
  PIP_OPTION_INFORMATION = ^IP_OPTION_INFORMATION;
  TIpOptionInformation = IP_OPTION_INFORMATION;
  PIpOptionInformation = PIP_OPTION_INFORMATION;

  ICMP_ECHO_REPLY = record
    Address: Cardinal;    // Replying address
    Status: Cardinal;        // Reply IP_STATUS
    RoundTripTime: Cardinal; // RTT in milliseconds
    DataSize: Word;     // Reply data size in bytes
    Reserved: Word;     // Reserved for system use
    Data: Pointer;         // Pointer to the reply data
    Options: ip_option_information; // Reply options
  end;
  PICMP_ECHO_REPLY = ^ICMP_ECHO_REPLY;
  TIcmpEchoReply = ICMP_ECHO_REPLY;
  PIcmpEchoReply = PICMP_ECHO_REPLY;

var
  IpHlpAPIHandle: THandle = 0;
  UnloadIpHlpAPI: Boolean;

  GetAdaptersInfo: function (pAdapterInfo: PIP_ADAPTER_INFO; var pOutBufLen: ULONG): DWORD; stdcall = nil;
  GetNetworkParams: function (pFixedInfo: PFIXED_INFO; var pOutBufLen: ULONG): DWORD; stdcall = nil;
  GetInterfaceInfo: function (Buffer: PIP_INTERFACE_INFO; var BufferSize: DWORD): DWORD; stdcall = nil;
  GetIfTable: function (IfTable: PMIB_IFTABLE; var TableSize: DWORD; OrderByIfIndex: boolean): DWORD; stdcall = nil;
  GetIfEntry: function (var IfEntry: TMIB_IFROW): DWORD; stdcall = nil;
  GetIfTable2: function (IfTable: PMIB_IFTABLE2): DWORD; stdcall = nil;
  GetIfEntry2: function (var IfEntry: TMIB_IFROW2): DWORD; stdcall = nil;
  GetNumberOfInterfaces: function (var pdwNumIf: DWORD): DWORD; stdcall = nil;
  GetAdapterIndex: function (AdapterName: LPWSTR; var IfIndex: ULONG): DWORD; stdcall = nil;
  GetIpNetTable: function (pIpNetTable: PMIB_IPNETTABLE; pdwSize: PULONG;  bOrder: BOOL ): DWORD; stdcall = nil;
  GetIpAddrTable: function (pIpAddrTable: PMIB_IPADDRTABLE; pdwSize: PULONG; bOrder: BOOL ): DWORD; stdcall = nil;
  GetTcpTable: function (pTCPTable: PMIB_TCPTABLE; pDWSize: PDWORD; bOrder: BOOL ): DWORD; stdcall = nil;
  GetTcpStatistics: function (pStats: PMIB_TCPSTATS): DWORD; stdcall = nil;
  GetUdpTable: function (pUdpTable: PMIB_UDPTABLE; pDWSize: PDWORD; bOrder: BOOL ): DWORD; stdcall = nil;
  GetUdpStatistics: function (pStats: PMIB_UDPSTATS): DWORD; stdcall = nil;
  GetIpStatistics: function (pStats: PMIB_IPSTATS): DWORD; stdcall = nil;
  SendARP: function(DestIp: DWORD; srcIP: DWORD; pMacAddr: pointer; PhyAddrLen: Pointer): DWORD; stdcall = nil;
  GetBestInterface: function (dwDestAddr: Cardinal; var pdwBestIfIndex: DWORD): DWORD; stdcall = nil;
  GetAdaptersAddresses: function(Family: ULONG; Flags: DWORD; Reserved: Pointer;
          pAdapterAddresses: PIP_ADAPTER_ADDRESSES; pOutBufLen: PULONG): DWORD; stdcall = nil;
  ConvertInterfaceIndexToLuid: function(InterfaceIndex: NET_IFINDEX; InterfaceLuid: PNET_LUID): Cardinal; stdcall = nil;
  ConvertInterfaceLuidToAlias: function(const InterfaceLuid: PNET_LUID; InterfaceAlias: PWideChar; Length: SIZE_T): Cardinal; stdcall = nil;
  GetIpInterfaceEntry: function(var Row: MIB_IPINTERFACE_ROW): Cardinal; stdcall = nil;

  AllocateAndGetTcpExTableFromStack: procedure (var pTCPTableEx: PMIB_TCPTABLE_EX;
        bOrder: Bool; Heap: THandle; Zero, Flags: DWORD); stdcall = nil;
  AllocateAndGetUdpExTableFromStack: procedure (var pUdpTableEx: PMIB_UDPTABLE_EX;
        bOrder: Bool; Heap: THandle; Zero, Flags: DWORD); stdcall = nil;
  NotifyAddrChange: function (AHandle: PCardinal; AOverlapped: POverlapped): DWORD; stdcall = nil;
  GetExtendedTcpTable: function (pTcpTable: Pointer; dwSize: PDWORD; bOrder: BOOL; ulAf: ULONG; TableClass: TCP_TABLE_CLASS; Reserved: ULONG): DWord; stdcall = nil;
  GetExtendedUdpTable: function (pUdpTable: Pointer; dwSize: PDWORD; bOrder: BOOL; ulAf: ULONG; TableClass: UDP_TABLE_CLASS; Reserved: ULONG): DWord; stdcall = nil;


function InitIpHlpAPI: Boolean;
procedure FreeIpHlpAPI;

function GetIntfTypeStr(AType: DWORD): string;
function GetIntfStatStr(AStat: DWORD): string;
function GetIntfAdminStr(AStat: DWORD): string;

implementation

const
  IpHlpAPI_DLL = 'iphlpapi.dll';

function InitIpHlpAPI: Boolean;
begin
  IpHlpAPIHandle:=GetModuleHandle(IpHlpAPI_DLL);
  UnloadIpHlpAPI:=IpHlpAPIHandle=0;
  if IpHlpAPIHandle=0 then
    IpHlpAPIHandle:=LoadLibrary(IpHlpAPI_DLL);
  if IpHlpAPIHandle<>0 then begin
    @GetAdaptersInfo:=GetProcAddress(IpHlpAPIHandle,'GetAdaptersInfo');
    @GetNetworkParams:=GetProcAddress(IpHlpAPIHandle,'GetNetworkParams');
    @GetInterfaceInfo:=GetProcAddress(IpHlpAPIHandle,'GetInterfaceInfo');
    @GetIfTable:=GetProcAddress(IpHlpAPIHandle,'GetIfTable');
    @GetIfEntry:=GetProcAddress(IpHlpAPIHandle,'GetIfEntry');
    @GetIfTable2:=GetProcAddress(IpHlpAPIHandle,'GetIfTable2');
    @GetIfEntry2:=GetProcAddress(IpHlpAPIHandle,'GetIfEntry2');
    @GetNumberOfInterfaces:=GetProcAddress(IpHlpAPIHandle,'GetNumberOfInterfaces');
    @GetAdapterIndex:=GetProcAddress(IpHlpAPIHandle,'GetAdapterIndex');
    @GetIpNetTable:=GetProcAddress(IpHlpAPIHandle,'GetIpNetTable');
    @GetIpAddrTable:=GetProcAddress(IpHlpAPIHandle,'GetIpAddrTable');
    @SendARP:=GetProcAddress(IpHlpAPIHandle,'SendARP');
    @GetBestInterface:=GetProcAddress(IpHlpAPIHandle,'GetBestInterface');
    @GetAdaptersAddresses:=GetProcAddress(IpHlpAPIHandle,'GetAdaptersAddresses');
    @ConvertInterfaceIndexToLuid:=GetProcAddress(IpHlpAPIHandle,'ConvertInterfaceIndexToLuid');
    @ConvertInterfaceLuidToAlias:=GetProcAddress(IpHlpAPIHandle,'ConvertInterfaceLuidToAlias');
    @GetIpInterfaceEntry:=GetProcAddress(IpHlpAPIHandle,'GetIpInterfaceEntry');

    @AllocateAndGetTcpExTableFromStack:=GetProcAddress(IpHlpAPIHandle,'AllocateAndGetTcpExTableFromStack');
    @AllocateAndGetUdpExTableFromStack:=GetProcAddress(IpHlpAPIHandle,'AllocateAndGetUdpExTableFromStack');

    @NotifyAddrChange:=GetProcAddress(IpHlpAPIHandle,'NotifyAddrChange');

    @GetExtendedTcpTable:=GetProcAddress(IpHlpAPIHandle,'GetExtendedTcpTable');
    @GetExtendedUdpTable:=GetProcAddress(IpHlpAPIHandle,'GetExtendedUdpTable');
  end;
  Result:=(IpHlpAPIHandle<>0) and Assigned(GetAdaptersInfo);
end;

procedure FreeIpHlpAPI;
begin
  if (IpHlpAPIHandle<>0) and UnloadIpHlpAPI then begin
    if not FreeLibrary(IpHlpAPIHandle) then
      raise Exception.Create(Format('Unload Error: %s - 0x%x',[IpHlpAPI_DLL,GetModuleHandle(IpHlpAPI_DLL)]))
    else
      IpHlpAPIHandle:=0;
  end;
end;

function GetIntfTypeStr;
begin
  Result:='';
  case aType of
    MIB_IF_OTHER_ADAPTERTYPE: Result:='Other';
    MIB_IF_ETHERNET_ADAPTERTYPE: Result:='Ethernet';
    MIB_IF_TOKEN_RING_ADAPTERTYPE: Result:='TokenRing';
    MIB_IF_FDDI_ADAPTERTYPE: Result:='Fiber Distributed Data Interface';
    MIB_IF_PPP_ADAPTERTYPE: Result:='Point-to-Point Protocol';
    MIB_IF_LOOPBACK_ADAPTERTYPE: Result:='Loopback';
    MIB_IF_ATM_ADAPTERTYPE: Result:='Asynchronous Transfer Mode';
    MIB_IF_TYPE_IEEE80211: Result:='IEEE 802.11 wireless';
    MIB_IF_TYPE_TUNNEL: Result:='Tunnel encapsulation';
    MIB_IF_TYPE_IEEE1394: Result:='IEEE 1394 (Firewire) high performance serial bus';
    MIB_IF_TYPE_IEEE80216_WMAN: Result:='Mobile broadband for WiMax';
    MIB_IF_TYPE_WWANPP: Result:='Mobile broadband for GSM';
    MIB_IF_TYPE_WWANPP2: Result:='Mobile broadband for CDMA';
  end;
end;

function GetIntfStatStr;
begin
  case AStat of
    MIB_IF_OPER_STATUS_NON_OPERATIONAL: Result:='Non-operational';
    MIB_IF_OPER_STATUS_UNREACHABLE: Result:='Unreachable';
    MIB_IF_OPER_STATUS_DISCONNECTED: Result:='Disconnected';
    MIB_IF_OPER_STATUS_CONNECTING: Result:='Connecting';
    MIB_IF_OPER_STATUS_CONNECTED: Result:='Connected';
    MIB_IF_OPER_STATUS_OPERATIONAL: Result:='Operational';
  end;
end;

function GetIntfAdminStr;
begin
  case AStat of
    MIB_IF_ADMIN_STATUS_UP: Result:='Up';
    MIB_IF_ADMIN_STATUS_DOWN: Result:='Down';
    MIB_IF_ADMIN_STATUS_TESTING: Result:='Testing';
  end;
end;

initialization
finalization
end.

