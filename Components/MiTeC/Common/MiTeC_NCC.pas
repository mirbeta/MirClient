{*******************************************************}
{                                                       }
{            Network Configuration Class                }
{                                                       }
{                                                       }
{        Copyright (c) 2016-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_NCC;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.WinSock,
     {$ELSE}
     Windows, SysUtils, Classes, WinSock,
     {$ENDIF}
     MiTeC_Windows, MiTeC_NetUtils;

type
  TNodeType = (ntUnknown, ntBroadcast, ntPeerToPeer, ntMixed, ntHybrid);

  TAdapterType = (atOther, atEthernet, atTokenRing, atFDDI, atPPP, atLoopback,
                  atATM, atIEEE80211, atTunnel, atIEEE1394, atIEEE80216_WMAN,
                  atWWANPP, atWWANPP2);

  TAdapterOperationalStatus = (aosNonOperational, aosUnreachable, aosDisconnected, aosConnecting, aosConnected, aosOperational);

  TAdapterAdminStatus = (aasUnknown, aasUp, aasDown, aasTesting);

  TAdapter = record
    Description,
    Name,
    Alias,
    MACAddress: string;
    MaxSpeed: UInt64;
    MTU: Cardinal;
    Typ: TAdapterType;
    AdminStatus: TAdapterAdminStatus;
    OperStatus: TAdapterOperationalStatus;
    DHCPEnabled,
    DNSEnabled,
    HaveWINS,
    NETBIOSEnabled,
    IPv4Enabled,
    IPv6Enabled: boolean;
    IPv4Address,
    IPv6Address,
    Gateways_v4,
    Gateways_v6,
    DHCPServers_v4,
    DHCPServers_v6,
    PrimaryWINS_v4,
    PrimaryWINS_v6,
    SecondaryWINS,
    DNSServers_v4,
    DNSServers_v6: TStringList;
    DNSSuffix: string;
    IntfIdx: Cardinal;
    _Typ,
    _Status: Cardinal;
    _ImageIndex: Integer;
    _Mark: Boolean;
  end;

  TAdapterList = array of TAdapter;

  TAddressTypes = (atPrimary, atDynamic, atDisconnected, atDeleted, atTransient);

  TAddressType = set of TAddressTypes;

  TAddressRecord = record
    IP,
    Mask: string;
    Physical: string;
    IntfIdx: Cardinal;
    Typ: TAddressType;
    ARPType: Cardinal;
  end;

  TAddressTable = array of TAddressRecord;

  TNetworkConfiguration = class
  private
    FAdapters: TAdapterList;
    FAT,
    FARP: TAddressTable;
    FProxy: boolean;
    FRouting: boolean;
    FDNS: boolean;
    FHost: string;
    FDomain: string;
    FDNSSuf: string;
    FDNSList4,FDNSList6,FIL: TStrings;
    FNode: TNodeType;
    FDHCPScope: string;
    FBII: Cardinal;
    FIPData: TIPData;
    FRefreshing: boolean;
    FIA: boolean;
    procedure ClearList;
    function GetAdapter(Index: Word): TAdapter;
    function GetAdapterCount: Word;
    function Add(ARecord: TAdapter): Integer;
    function GetAddrRec(Index: Word): TAddressRecord;
    function GetAddrRecCount: Word;
    function GetCurrentIP: string;
    function GetCurrentMAC: string;
    function GetARPRec(Index: Word): TAddressRecord;
    function GetARPRecCount: Word;
    function GetCurrentNet: string;
    function GetAdapterImageIndex(Index: Word): integer;
    function GetAdapterMark(Index: Word): boolean;
    procedure SetAdapterMark(Index: Word; const Value: boolean);
    procedure SetAdapterImageIndex(Index: Word; const Value: integer);
    function GetCurrentAdapterName: string;
  public
    class function GetSubnetMask(AIP: string): string;
    class function SetIP(AIntfIdx: Cardinal; AStatic: Boolean; AIPv4,AMask,AGateway: string): Int64;
    class function SetDNS(AIntfIdx: Cardinal; AStatic: Boolean; APreferred,AAlternate: string): Int64;
    class function IsPortOpen(const AIPv4: string; APort: Word; ATimeout: Cardinal = INFINITE): Boolean;
    class function Ping(const AIPv4: string; ATTL, ARetries: byte; ATimeout: Cardinal): integer;
    class function InternetAccessAvailable: boolean;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure RefreshData;
    function FindAdapter(AName: string): Integer; overload;
    function FindAdapter(AIntfIdx: Cardinal): Integer; overload;
    function FindAddress(AIP: string): Integer; overload;
    function FindAddress(AIntfIdx: Cardinal): Integer; overload;
    function FindFirstOperational: Integer;
    function FindFirstOperationalIntfIdx: Cardinal;
    procedure GetTraffic(AIntfIdx: Cardinal; var AIn,AOut: UInt64);
    property Refreshing: boolean read FRefreshing;
    property Adapter[Index: Word]: TAdapter read GetAdapter;
    property AdapterImageIndex[Index: Word]: integer read GetAdapterImageIndex write SetAdapterImageIndex;
    property AdapterMark[Index: Word]: boolean read GetAdapterMark write SetAdapterMark;
    property AdapterCount: Word read GetAdapterCount;
    property AddressRecord[Index: Word]: TAddressRecord read GetAddrRec;
    property AdressRecordCount: Word read GetAddrRecCount;
    property ARPRecord[Index: Word]: TAddressRecord read GetARPRec;
    property ARPRecordCount: Word read GetARPRecCount;
    property HostName: string read FHost;
    property DomainName: string read FDomain;
    property ProxyEnabled: boolean read FProxy;
    property RoutingEnabled: boolean read FRouting;
    property DNSEnabled: boolean read FDNS;
    property PrimaryDNSSuffix: string read FDNSSuf;
    property DHCPScopeName: string read FDHCPScope;
    property DNSServers_v4: TStrings read FDNSList4;
    property DNSServers_v6: TStrings read FDNSList6;
    property NodeType: TNodeType read FNode;
    property BestInterfaceIdx: Cardinal read FBII;
    property CurrentIP: string read GetCurrentIP;
    property CurrentMAC: string read GetCurrentMAC;
    property CurrentNetwork: string read GetCurrentNet;
    property CurrentAdapterName: string read GetCurrentAdapterName;
    property InternetAccess: boolean read FIA write FIA;
  end;

const
  NodeTypes: array[TNodeType] of string = ('Unknown','Broadcast','Peer-To-Peer','Mixed','Hybrid');

  AdapterTypes: array[TAdapterType] of string = ('Other',
                                                 'Ethernet',
                                                 'Token Ring',
                                                 'FDDI',
                                                 'PPP',
                                                 'Loopback',
                                                 'ATM',
                                                 'IEEE 802.11 wireless',
                                                 'Tunnel encapsulation',
                                                 'IEEE 1394 (Firewire) high performance serial bus',
                                                 'Mobile broadband for WiMax',
                                                 'Mobile broadband for GSM',
                                                 'Mobile broadband for CDMA');

  AdapterOperationalStatus: array[TAdapterOperationalStatus] of string = ('Non Operational',
                                                                           'Unreachable',
                                                                           'Disconnected',
                                                                           'Connecting',
                                                                           'Connected',
                                                                           'Operational');

implementation

uses {$IFDEF RAD9PLUS}
     WinAPI.ActiveX, System.Win.ComObj, System.Variants, WinApi.WinInet,
     {$ELSE}
     ActiveX, ComObj, Variants, WinInet,
     {$ENDIF}
     MiTeC_IPTypes, MiTeC_IpHlpAPI, MiTeC_Ws2_32, MiTeC_IcmpAPI;

function Swap32(const Figure: Integer): Integer;
var
  ByteArray: array [1..4] of Byte absolute Figure;
begin
  Result:=ByteArray[1]*$1000000+ByteArray[2]*$10000+ByteArray[3]*$100+ByteArray[4];
end;

function TNetworkConfiguration.Add(ARecord: TAdapter): Integer;
begin
  SetLength(FAdapters,Length(FAdapters)+1);
  Result:=High(FAdapters);
  FAdapters[Result]:=ARecord;
end;

procedure TNetworkConfiguration.Clear;
begin
  ClearList;
  Finalize(FAT);
  Finalize(FARP);
  FProxy:=False;
  FRouting:=False;
  FDNS:=False;
  FHost:='';
  FDomain:='';
  FDNSList4.Clear;
  FDNSList6.Clear;
  FNode:=ntUnknown;
  FDHCPScope:='';
  FDNSSuf:='';
  FBII:=0;
  FIL.Clear;
end;

procedure TNetworkConfiguration.ClearList;
var
  i: Integer;
begin
  for i:=0 to High(FAdapters) do begin
    FAdapters[i].IPv4Address.Free;
    FAdapters[i].IPv6Address.Free;
    FAdapters[i].Gateways_v4.Free;
    FAdapters[i].Gateways_v6.Free;
    FAdapters[i].DHCPServers_v4.Free;
    FAdapters[i].DHCPServers_v6.Free;
    FAdapters[i].PrimaryWINS_v4.Free;
    FAdapters[i].PrimaryWINS_v6.Free;
    FAdapters[i].SecondaryWINS.Free;
    FAdapters[i].DNSServers_v4.Free;
    FAdapters[i].DNSServers_v6.Free;
    Finalize(FAdapters[i]);
  end;
  Finalize(FAdapters);
end;

constructor TNetworkConfiguration.Create;
begin
  Finalize(FIPData);
  FDNSList4:=TStringList.Create;
  FDNSList6:=TStringList.Create;
  FIL:=TStringList.Create;
  InitIpHlpAPI;
  FRefreshing:=False;
  Clear;
end;

destructor TNetworkConfiguration.Destroy;
begin
  Clear;
  FDNSList4.Free;
  FDNSList6.Free;
  FIL.Free;
  inherited;
end;

function TNetworkConfiguration.FindAdapter(AName: string): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(FAdapters) do
    if SameText(FAdapters[i].Name,AName) then begin
      Result:=i;
      Break;
    end;
end;

function TNetworkConfiguration.FindAdapter(AIntfIdx: Cardinal): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(FAdapters) do
    if (FAdapters[i].IntfIdx=AIntfIdx) then begin
      Result:=i;
      Break;
    end;
end;

function TNetworkConfiguration.FindAddress(AIP: string): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(FAT) do
    if SameText(FAT[i].IP,AIP) then begin
      Result:=i;
      Break;
    end;
end;

function TNetworkConfiguration.FindAddress(AIntfIdx: Cardinal): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(FAT) do
    if (FAT[i].IntfIdx=AIntfIdx) then begin
      Result:=i;
      Break;
    end;
end;

function TNetworkConfiguration.FindFirstOperational: Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(FAdapters) do
    if (FAdapters[i].OperStatus=aosOperational) then begin
      Result:=i;
      Break;
    end;
  if Result=-1 then
    for i:=0 to High(FAdapters) do
      if (FAdapters[i].IPv4Address.Count>0) and (StrToIPv4(FAdapters[i].IPv4Address[0]).Value>0) then begin
        Result:=i;
        Break;
      end;
end;

function TNetworkConfiguration.FindFirstOperationalIntfIdx: Cardinal;
var
  idx: Integer;
begin
  Result:=0;
  idx:=FindFirstOperational;
  if idx>-1 then
    Result:=FAdapters[idx].IntfIdx;
end;

function TNetworkConfiguration.GetAdapter(Index: Word): TAdapter;
begin
  Result:=FAdapters[Index];
end;

function TNetworkConfiguration.GetAdapterCount: Word;
begin
  Result:=Length(FAdapters);
end;

function TNetworkConfiguration.GetAdapterImageIndex(Index: Word): integer;
begin
  Result:=FAdapters[Index]._ImageIndex;
end;

function TNetworkConfiguration.GetAdapterMark(Index: Word): boolean;
begin
  Result:=FAdapters[Index]._Mark;
end;

function TNetworkConfiguration.GetAddrRec(Index: Word): TAddressRecord;
begin
  Result:=FAT[Index];
end;

function TNetworkConfiguration.GetAddrRecCount: Word;
begin
  Result:=Length(FAT);
end;

function TNetworkConfiguration.GetARPRec(Index: Word): TAddressRecord;
begin
  Result:=FARP[Index];
end;

function TNetworkConfiguration.GetARPRecCount: Word;
begin
  Result:=Length(FARP);
end;

function TNetworkConfiguration.GetCurrentAdapterName: string;
var
  r,n: Cardinal;
  idx: integer;
begin
  Result:='';
  n:=0;
  r:=GetBestInterface(inet_addr(PAnsiChar('8.8.8.8')),n);
  if Length(FAT)=0 then
    RefreshData;
  if r=NO_ERROR then
    FBII:=n
  else
    FBII:=FindFirstOperationalIntfIdx;
  idx:=FindAdapter(FBII);
  if idx>-1 then
    Result:=FAdapters[idx].Name;
end;

function TNetworkConfiguration.GetCurrentIP: string;
var
  r,n: Cardinal;
  idx: integer;
begin
  Result:='';
  n:=0;
  r:=GetBestInterface(inet_addr(PAnsiChar('8.8.8.8')),n);
  if Length(FAT)=0 then
    RefreshData;
  if r=NO_ERROR then
    FBII:=n
  else
    FBII:=FindFirstOperationalIntfIdx;
  idx:=FindAddress(FBII);
  if idx>-1 then
    Result:=FAT[idx].IP;
end;

function TNetworkConfiguration.GetCurrentMAC: string;
var
  r,n: Cardinal;
  idx: integer;
begin
  Result:='';
  n:=0;
  r:=GetBestInterface(inet_addr(PAnsiChar('8.8.8.8')),n);
  if Length(FAT)=0 then
    RefreshData;
  if r=NO_ERROR then
    FBII:=n
  else
    FBII:=FindFirstOperationalIntfIdx;
  idx:=FindAdapter(FBII);
  if idx>-1 then
    Result:=FAdapters[idx].MACAddress;
end;

function TNetworkConfiguration.GetCurrentNet: string;
var
  r,n: Cardinal;
  idx: integer;
begin
  Result:='';
  n:=0;
  r:=GetBestInterface(inet_addr(PAnsiChar('8.8.8.8')),n);
  if Length(FAT)=0 then
    RefreshData;
  if r=NO_ERROR then
    FBII:=n
  else
    FBII:=FindFirstOperationalIntfIdx;
  idx:=FindAdapter(FBII);
  if idx>-1 then
    Result:=FAdapters[idx].Alias;
end;

class function TNetworkConfiguration.GetSubnetMask(AIP: string): string;
var
  ip: TIPv4;
begin
  Result:='0.0.0.0';
  ip:=StrToIPv4(AIP);
  if ip.A in [1..126] then
    Result:='255.0.0.0'
  else if ip.A in [128..191] then
    Result:='255.255.0.0'
  else if ip.A in [192..223] then
    Result:='255.255.255.0';
end;

procedure TNetworkConfiguration.GetTraffic(AIntfIdx: Cardinal; var AIn,
  AOut: UInt64);
var
  Entry: TMIB_IFROW;
  Entry2: TMIB_IFROW2;
begin
  AIn:=0;
  AOut:=0;
  FillChar(Entry,SizeOf(Entry),0);
  Entry.dwIndex:=AIntfIdx;
  FillChar(Entry2,SizeOf(Entry2),0);
  Entry2.InterfaceIndex:=AIntfIdx;
  if Assigned(GetIfEntry2) and (GetIfEntry2(Entry2)=0) then begin
    AIn:=Entry2.InOctets;
    AOut:=Entry2.OutOctets;
  end else if GetIfEntry(Entry)=0 then begin
    AIn:=Entry.dwInOctets;
    AOut:=Entry.dwOutOctets;
  end;
end;

class function TNetworkConfiguration.InternetAccessAvailable: boolean;
var
  f: Cardinal;
  hs,hu: HInternet;
begin
  //Result:=InternetGetConnectedState(@f,0);
  //if Result and (f and INTERNET_CONNECTION_CONFIGURED>0) then begin
    hs:=InternetOpen('test',INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);
    try
      hu:=InternetOpenURL(hs,'http://www.google.com',nil,0,0,0);
      Result:=hu<>nil;
      InternetCloseHandle(hu);
    finally
      InternetCloseHandle(hs);
    end;
  {end else
    Result:=False;}
end;

class function TNetworkConfiguration.IsPortOpen(const AIPv4: string; APort: Word; ATimeout: Cardinal = INFINITE): Boolean;
var
  wsData: TWSAData;
  hSocket: TSocket;
  iVal: Integer;
  iRes: Integer;
  SockAddr: TSockAddr;
  WrSet: TFDSet;
  TimeVal: TTimeVal;
begin
  Result:=False;
  WSAStartup($0101, wsData);
  try
    hSocket:=Socket(AF_INET,SOCK_STREAM,IPPROTO_TCP);
    if hSocket=INVALID_SOCKET then
      Exit;
    try
      iVal:=1;
      if ATimeout<>INFINITE then
        if IOCtlSocket(hSocket,FIONBIO,iVal)=SOCKET_ERROR then
          Exit;
      FillChar(SockAddr,sizeof(SockAddr),0);
      SockAddr.sin_family:=AF_INET;
      SockAddr.sin_port:=htons(APort);
      SockAddr.sin_addr:=IPv4ToInAddr(StrToIPv4(AIPv4));
      iRes:=Connect(hSocket,SockAddr,sizeof(SockAddr));
      if iRes<>SOCKET_ERROR then begin
        if ATimeout=INFINITE then
          Result:=True
        else
          Exit;
      end;
      if ATimeout<>INFINITE then begin
        iRes:=WSAGetLastError;
        if iRes<>WSAEWOULDBLOCK then
          Exit;
        TimeVal.tv_sec:=ATimeout;
        TimeVal.tv_usec:=0;
        FD_ZERO(WrSet);
        FD_SET(hSocket,WrSet);
        Result:=Select(0,nil,@WrSet,nil,@TimeVal)=1;
      end;
    finally
      Shutdown(hSocket,SD_BOTH);
      CloseSocket(hSocket);
    end;
  finally
    WSACleanup;
  end;
end;

class function TNetworkConfiguration.Ping(const AIPv4: string; ATTL, ARetries: byte; ATimeout: Cardinal): integer;
var
  ICMPFile: THandle;
  ReplySize,NumResponses: Cardinal;
  SendData: array[0..31] of AnsiChar;
  ReplyBuffer: PICMP_ECHO_REPLY;
  ioi: TIpOptionInformation;
  i,j: Byte;
  t: Cardinal;
begin
  Result:=-1;
  if ARetries<1 then
    ARetries:=1;
  t:=0;
  j:=0;

  SendData:='1234567890ABCDEFGHIJKLMNOPQRSTUV';
  ioi.Ttl:=ATTL;
  ioi.Tos:=0;
  ioi.Flags:=0;
  ioi.OptionsSize:=0;
  ioi.OptionsData:=nil;
  IcmpFile:=IcmpCreateFile;
  if IcmpFile<>INVALID_HANDLE_VALUE then
    try
      ReplySize:=SizeOf(ICMP_ECHO_REPLY)+SizeOf(SendData)+8;
      ReplyBuffer:=AllocMem(ReplySize);
      try
        for i:=0 to ARetries-1 do begin
          ZeroMemory(ReplyBuffer,ReplySize);
          NumResponses:=IcmpSendEcho(IcmpFile,IPv4ToInAddr(StrToIPv4(AIPv4)),@SendData,SizeOf(SendData),@ioi,ReplyBuffer,ReplySize,ATimeout);
          if (NumResponses<>0) and (ReplyBuffer.Status=0) then begin
            Inc(t,ReplyBuffer.RoundTripTime);
            inc(j);
            Result:=0;
          end else if Result=-1 then
            Result:=-integer(ReplyBuffer.Status);
        end;
        if (Result=0) and (j>0) then
          Result:=Round(t/j)
        else
          Result:=-1;
      finally
        FreeMem(ReplyBuffer);
      end;
    finally
      IcmpCloseHandle(IcmpFile);
    end
end;

procedure TNetworkConfiguration.RefreshData;
var
  aim: TIP_ADAPTER_INDEX_MAP;
  ai, aiInitPtr: PIP_ADAPTER_INFO;
  lastip,ip: PIP_ADDR_STRING;
  aa,aaInitPtr: PIP_ADAPTER_ADDRESSES;
  lastdns,dns: PIP_ADAPTER_DNS_SERVER_ADDRESS;
  gw,lastgw: PIP_ADAPTER_GATEWAY_ADDRESS_LH;
  wins,lastwins: PIP_ADAPTER_WINS_SERVER_ADDRESS_LH;
  fua,lastfua: PIP_ADAPTER_UNICAST_ADDRESS;
  Entry: TMIB_IFROW;
  Entry2: TMIB_IFROW2;
  IPAdrRow: TMIB_IPADDRROW;
  IPNetRow: TMIB_IPNETROW;
  np: PFixedInfo;
  r,j,n: Cardinal;
  Size: ulong;
  A: TAdapter;
  s: string;
  pBuf: PAnsiChar;
  idx,i,k: Integer;
  luid: NET_LUID;
  alias: array[0..IF_MAX_STRING_SIZE] of char;
  sa4: sockaddr_in;
  sa6: TSockAddrIn6;
  buf: array[0..255] of char;
begin
  if FRefreshing then
    Exit;
  FIA:=InternetAccessAvailable;
  Clear;
  if InitIpHlpAPI then begin
    FRefreshing:=True;
    try
      Size:=SizeOf(TFixedInfo);
      np:=Allocmem(size);
      try
        r:=GetNetworkparams(np,Size);
        while r=ERROR_BUFFER_OVERFLOW do begin
          Reallocmem(np,size);
          r:=GetNetworkparams(np,Size);
        end;

        if r=ERROR_SUCCESS then begin
          case np^.NodeType of
            BROADCAST_NODETYPE: FNode:=ntBroadcast;
            PEER_TO_PEER_NODETYPE: FNode:=ntPeerToPeer;
            MIXED_NODETYPE: FNode:=ntMixed;
            HYBRID_NODETYPE: FNode:=ntHybrid;
            else FNode:=ntUnknown;
          end;
          FDHCPScope:=string(np^.ScopeId);
          if Assigned(np^.CurrentDnsServer) then
            FDNSSuf:=string(np^.CurrentDnsServer.IPAddress.S)
          else
            FDNSSuf:='';

          ip:=@(np^.DnsServerList);
          FDNSList4.Clear;
          repeat
            s:=string(ip^.IPAddress.s);
            FDNSList4.Add(s);
            ip:=ip.Next;
          until (not Assigned(ip)) or (s=string(ip^.IPAddress.s));

          FHost:=string(np^.HostName);
          FDomain:=string(np^.DomainName);
          FProxy:=Boolean(np^.EnableProxy);
          FRouting:=Boolean(np^.EnableRouting);
          FDNS:=Boolean(np^.EnableDns);
        end;
      finally
        if Assigned(np) then
          FreeMem(np);
      end;

      size:=SizeOf(TIP_INTERFACE_INFO);
      pBuf:=AllocMem(size);
      try
        r:=GetInterfaceInfo(PIP_INTERFACE_INFO(pBuf),size);
        while(r=ERROR_INSUFFICIENT_BUFFER) do begin
          size:=Size+SizeOf(TIP_INTERFACE_INFO);
          ReallocMem(pBuf,size);
          r:=GetInterfaceInfo(PIP_INTERFACE_INFO(pBuf),size);
        end;
        if(r=ERROR_SUCCESS) then begin
          n:=PIP_INTERFACE_INFO(pBuf).NumAdapters;
          if n>0 then begin
            Inc(pBuf,SizeOf(integer));
            for j:=1 to n do begin
              aim:=PIP_ADAPTER_INDEX_MAP(pBuf)^;
              s:=aim.Name;
              s:=Trim(s);
              idx:=Pos('}',s);
              if idx>0 then
                SetLength(s,idx);
              FIL.Add(Format('%s=%d',[s,aim.Index]));
              Inc(pBuf,SizeOf(Integer)+SizeOf(TIP_ADAPTER_INDEX_MAP));
            end;
          end;
        end;
      finally
        Dec(pBuf,SizeOf(Integer)+n*(SizeOf(Integer)+SizeOf(TIP_ADAPTER_INDEX_MAP)));
        try FreeMem(pBuf) except end;
      end;

      size:=SizeOf(IP_ADAPTER_INFO);
      aiInitPtr:=AllocMem(size);
      try
        r:=GetAdaptersInfo(aiInitPtr,size);
        while(r=ERROR_BUFFER_OVERFLOW) do begin
          size:=Size+SizeOf(IP_ADAPTER_INFO);
          ReallocMem(aiInitPtr,size);
          r:=GetAdaptersInfo(aiInitPtr,size);
        end;
        ai:=aiInitPtr;
        if(r=ERROR_SUCCESS) then begin
          while assigned(ai) do begin
            FillChar(A,SizeOf(A),0);
            A.IPv4Address:=TStringList.Create;
            A.IPv6Address:=TStringList.Create;
            A.Gateways_v4:=TStringList.Create;
            A.Gateways_v6:=TStringList.Create;
            A.DHCPServers_v4:=TStringList.Create;
            A.DHCPServers_v6:=TStringList.Create;
            A.PrimaryWINS_v4:=TStringList.Create;
            A.PrimaryWINS_v6:=TStringList.Create;
            A.SecondaryWINS:=TStringList.Create;
            A.DNSServers_v4:=TStringList.Create;
            A.DNSServers_v6:=TStringList.Create;

            A.Name:=Trim(string(ai^.AdapterName));
            A.Description:=Trim(string(ai^.Description));
            A._Typ:=ai^.Type_;
            case ai^.Type_ of
              MIB_IF_OTHER_ADAPTERTYPE: A.Typ:=atOther;
              MIB_IF_ETHERNET_ADAPTERTYPE: A.Typ:=atEthernet;
              MIB_IF_TOKEN_RING_ADAPTERTYPE: A.Typ:=atTokenRing;
              MIB_IF_FDDI_ADAPTERTYPE: A.Typ:=atFDDI;
              MIB_IF_PPP_ADAPTERTYPE: A.Typ:=atPPP;
              MIB_IF_LOOPBACK_ADAPTERTYPE: A.Typ:=atLoopback;
              MIB_IF_ATM_ADAPTERTYPE: A.Typ:=atATM;
              MIB_IF_TYPE_IEEE80211: A.Typ:=atIEEE80211;
              MIB_IF_TYPE_TUNNEL: A.Typ:=atTunnel;
              MIB_IF_TYPE_IEEE1394: A.Typ:=atIEEE1394;
              MIB_IF_TYPE_IEEE80216_WMAN: A.Typ:=atIEEE80216_WMAN;
              MIB_IF_TYPE_WWANPP: A.Typ:=atWWANPP;
              MIB_IF_TYPE_WWANPP2: A.Typ:=atWWANPP2;
            end;
            s:='';
            if ai^.AddressLength>0 then begin
              for j:=0 to ai^.AddressLength-1 do
                s:=s+Format('%2.2x:',[ai^.Address[j]]);
              SetLength(s,Length(s)-1);
            end;
            A.MACAddress:=s;
            A.DHCPEnabled:=Boolean(ai^.DhcpEnabled);
            A.HaveWINS:=Boolean(ai^.HaveWins);

            ip:=@(ai^.IpAddressList);
            repeat
              lastip:=ip;
              A.IPv4Address.Add(Format('%s=%s',[string(ip^.IPAddress.s),string(ip^.IpMask.s)]));
              ip:=ip.Next;
            until not Assigned(ip) or (lastip=ip);

            ip:=@(ai^.GatewayList);
            repeat
              lastip:=ip;
              A.Gateways_v4.Add(Format('%s=%s',[string(ip^.IPAddress.s),string(ip^.IpMask.s)]));
              ip:=ip.Next;
            until not Assigned(ip) or (lastip=ip);

            ip:=@(ai^.DhcpServer);
            repeat
              lastip:=ip;
              A.DHCPServers_v4.Add(Format('%s=%s',[string(ip^.IPAddress.s),string(ip^.IpMask.s)]));
              ip:=ip.Next;
            until not Assigned(ip) or (lastip=ip);

            ip:=@(ai^.PrimaryWinsServer);
            repeat
              lastip:=ip;
              A.PrimaryWINS_v4.Add(Format('%s=%s',[string(ip^.IPAddress.s),string(ip^.IpMask.s)]));
              ip:=ip.Next;
            until not Assigned(ip) or (lastip=ip);

            ip:=@(ai^.SecondaryWinsServer);
            repeat
              lastip:=ip;
              A.SecondaryWINS.Add(Format('%s=%s',[string(ip^.IPAddress.s),string(ip^.IpMask.s)]));
              ip:=ip.Next;
            until not Assigned(ip) or (lastip=ip);

            s:='';
            for k:=0 to A.IPv4Address.Count-1 do
              for i:=0 to High(FIPData) do
                if SameText(A.IPv4Address.Names[k],FIPData[i].IP4) then
                  s:=s+FIPData[i].IP6+',';
            SetLength(s,Length(s)-1);
            A.IPv6Address.CommaText:=s;

            FillChar(Entry,SizeOf(Entry),0);
            Entry.dwIndex:=ai.Index;
            if GetIfEntry(Entry)=0 then begin
              A.Name:=Entry.wszName;
              A.Name:=Trim(A.Name);
              A.MaxSpeed:=Entry.dwSpeed;
              A.MTU:=Entry.dwMTU;
              A.AdminStatus:=TAdapterAdminStatus(Entry.dwAdminStatus);
              A._Status:=Entry.dwOperStatus;
              A.OperStatus:=TAdapterOperationalStatus(Entry.dwOperStatus);
            end;

            FillChar(Entry2,SizeOf(Entry2),0);
            Entry2.InterfaceIndex:=ai.Index;
            if Assigned(GetIfEntry2) and (GetIfEntry2(Entry2)=0) then begin
              A.Name:=Entry2.Description;
              A.Name:=Trim(A.Name);
              A.Alias:=Entry2.Alias;
              A.Alias:=Trim(A.Alias);
              A.MaxSpeed:=Entry2.TransmitLinkSpeed;
              A.MTU:=Entry2.Mtu;
            end;

            if A.Name='' then
              A.Name:=A.Description;

            idx:=FIL.IndexOfName(A.Name);
            if idx>-1 then
              A.IntfIdx:=StrToInt(FIL.ValueFromIndex[idx])
            else
              A.IntfIdx:=ai.Index;

            if A.Alias='' then begin
              if Assigned(ConvertInterfaceIndexToLuid) and (ConvertInterfaceIndexToLuid(A.IntfIdx,@luid)=0) then
                if ConvertInterfaceLuidToAlias(@luid,@alias,SizeOf(alias))=0 then
                  A.Alias:=alias;
            end;

            if A.Typ in [atOther, atEthernet, atTokenRing, atFDDI, atPPP, atLoopback, atATM, atIEEE80211, atTunnel, atIEEE1394, atIEEE80216_WMAN, atWWANPP, atWWANPP2] then
              Add(A);
            ai:=ai.Next;
          end;
        end;
      finally
        if Assigned(aiInitPtr) then
          FreeMem(aiInitPtr);
      end;

      if Assigned(GetAdaptersAddresses) then begin
        size:=SizeOf(IP_ADAPTER_ADDRESSES);
        aaInitPtr:=AllocMem(size);
        try
          r:=GetAdaptersAddresses(AF_INET,GAA_FLAG_INCLUDE_PREFIX,nil,aaInitPtr,@size);
          while(r=ERROR_BUFFER_OVERFLOW) do begin
            size:=Size+SizeOf(IP_ADAPTER_ADDRESSES);
            ReallocMem(aaInitPtr,size);
            r:=GetAdaptersAddresses(AF_INET,GAA_FLAG_INCLUDE_PREFIX,nil,aaInitPtr,@size);
          end;
          aa:=aaInitPtr;
          if(r=ERROR_SUCCESS) then begin
            while assigned(aa) do begin
              idx:=FindAdapter(aa.Union.IfIndex);
              if idx>-1 then begin
                FAdapters[idx].DNSEnabled:=aa.Flags and IP_ADAPTER_DDNS_ENABLED>0;
                FAdapters[idx].NETBIOSEnabled:=aa.Flags and IP_ADAPTER_NETBIOS_OVER_TCPIP_ENABLED>0;
                FAdapters[idx].IPv4Enabled:=aa.Flags and IP_ADAPTER_IPV4_ENABLED>0;
                FAdapters[idx].IPv6Enabled:=aa.Flags and IP_ADAPTER_IPV6_ENABLED>0;
                FAdapters[idx].DNSSuffix:=aa.DnsSuffix;
                dns:=aa^.FirstDnsServerAddress;
                lastdns:=nil;
                while Assigned(dns) and (lastdns<>dns) do begin
                  lastdns:=dns;
                  Move(dns.Address.lpSockaddr^,sa4,sizeof(sa4));
                  s:=IPv4ToStr(InAddrToIPv4(sa4.sin_addr));
                  FAdapters[idx].DNSServers_v4.Add(s);
                  FDNSList4.Add(s);
                  dns:=dns.Next;
                end;
                FAdapters[idx].DNSSuffix:=aa.DnsSuffix;
              end;
              aa:=aa.Next;
            end;
          end;

          r:=GetAdaptersAddresses(AF_INET6,GAA_FLAG_INCLUDE_PREFIX,nil,aaInitPtr,@size);
          while(r=ERROR_BUFFER_OVERFLOW) do begin
            size:=Size+SizeOf(IP_ADAPTER_ADDRESSES);
            ReallocMem(aaInitPtr,size);
            r:=GetAdaptersAddresses(AF_INET6,GAA_FLAG_INCLUDE_PREFIX,nil,aaInitPtr,@size);
          end;
          aa:=aaInitPtr;
          if(r=ERROR_SUCCESS) then begin
            while assigned(aa) do begin
              idx:=FindAdapter(aa.Union.IfIndex);
              if idx>-1 then begin
                fua:=aa^.FirstUnicastAddress;
                lastfua:=nil;
                while Assigned(fua) and (lastfua<>fua) do begin
                  lastfua:=fua;
                  Move(fua.Address.lpSockaddr^,sa6,sizeof(sa6));
                  if Assigned(InetNtop) then begin
                    ZeroMemory(@buf,sizeof(buf));
                    InetNtop(AF_INET6,@sa6.sin6_addr,@buf,sizeof(buf));
                    s:=string(buf);
                  end else
                    s:=IPv6ToStr(InAddrToIPv6(sa6.sin6_addr));
                  FAdapters[idx].IPv6Address.Add(s);
                  fua:=fua.Next;
                end;

                dns:=aa^.FirstDnsServerAddress;
                lastdns:=nil;
                while Assigned(dns) and (lastdns<>dns) do begin
                  lastdns:=dns;
                  Move(dns.Address.lpSockaddr^,sa6,sizeof(sa6));
                  if Assigned(InetNtop) then begin
                    ZeroMemory(@buf,sizeof(buf));
                    InetNtop(AF_INET6,@sa6.sin6_addr,@buf,sizeof(buf));
                    s:=string(buf);
                  end else
                    s:=IPv6ToStr(InAddrToIPv6(sa6.sin6_addr));
                  FAdapters[idx].DNSServers_v6.Add(s);
                  FDNSList6.Add(s);
                  dns:=dns.Next;
                end;

                gw:=aa^.FirstGatewayAddress;
                lastgw:=nil;
                while Assigned(gw) and (lastgw<>gw) do begin
                  lastgw:=gw;
                  Move(gw.Address.lpSockaddr^,sa6,sizeof(sa6));
                  if Assigned(InetNtop) then begin
                    ZeroMemory(@buf,sizeof(buf));
                    InetNtop(AF_INET6,@sa6.sin6_addr,@buf,sizeof(buf));
                    s:=string(buf);
                  end else
                    s:=IPv6ToStr(InAddrToIPv6(sa6.sin6_addr));
                  FAdapters[idx].Gateways_v6.Add(s);
                  gw:=gw.Next;
                end;

                wins:=aa^.FirstWinsServerAddress;
                lastwins:=nil;
                while Assigned(wins) and (lastwins<>wins) do begin
                  lastwins:=wins;
                  Move(wins.Address.lpSockaddr^,sa6,sizeof(sa6));
                  if Assigned(InetNtop) then begin
                    ZeroMemory(@buf,sizeof(buf));
                    InetNtop(AF_INET6,@sa6.sin6_addr,@buf,sizeof(buf));
                    s:=string(buf);
                  end else
                    s:=IPv6ToStr(InAddrToIPv6(sa6.sin6_addr));
                  FAdapters[idx].PrimaryWINS_v4.Add(s);
                  wins:=wins.Next;
                end;

                if Assigned(aa.Dhcpv6Server.lpSockaddr) then begin
                  Move(aa.Dhcpv6Server.lpSockaddr^,sa6,sizeof(sa6));
                  if Assigned(InetNtop) then begin
                    ZeroMemory(@buf,sizeof(buf));
                    InetNtop(AF_INET6,@sa6.sin6_addr,@buf,sizeof(buf));
                    s:=string(buf);
                  end else
                    s:=IPv6ToStr(InAddrToIPv6(sa6.sin6_addr));
                  FAdapters[idx].DHCPServers_v6.Add(s);
                end;
              end;
              aa:=aa.Next;
            end;
          end;
        finally
          if Assigned(aaInitPtr) then
            FreeMem(aaInitPtr);
        end;
      end;

      size:=0;
      r:=GetIPAddrTable(nil,@size,False);
      if r=ERROR_NO_DATA then
        Exit;
      GetMem(pBuf,size);
      n:=0;
      try
        r:=GetIpAddrTable(PMIB_IPADDRTABLE(pBuf),@Size,False);
        if r=NO_ERROR then begin
          n:=PMIB_IPADDRTABLE(pBuf)^.dwNumEntries;
          if n>0 then begin
            Inc(pBuf,SizeOf(Cardinal));
            SetLength(FAT,n);
            for j:=1 to n do begin
              IPAdrRow:=PMIB_IPADDRROW(PBuf)^;
              FAT[j-1].IP:=IPv4ToStr(CardinalToIPv4(Swap32(IPAdrRow.dwAddr)));
              FAT[j-1].Mask:=IPv4ToStr(CardinalToIPv4(Swap32(IPAdrRow.dwMask)));
              FAT[j-1].IntfIdx:=IPAdrRow.dwIndex;
              FAT[j-1].Typ:=[];
              if IPAdrRow.wType and MIB_IPADDR_PRIMARY>0 then
                Include(FAT[j-1].Typ,atPrimary);
              if IPAdrRow.wType and MIB_IPADDR_DYNAMIC>0 then
                Include(FAT[j-1].Typ,atDynamic);
                if IPAdrRow.wType and MIB_IPADDR_DISCONNECTED>0 then
                Include(FAT[j-1].Typ,atDisconnected);
                if IPAdrRow.wType and MIB_IPADDR_DELETED>0 then
                Include(FAT[j-1].Typ,atDeleted);
                if IPAdrRow.wType and MIB_IPADDR_TRANSIENT>0 then
                Include(FAT[j-1].Typ,atTransient);
              Inc(pBuf,SizeOf(IPAdrRow));
            end;
          end;
        end;
      finally
        Dec(pBuf,SizeOf(Cardinal)+n*SizeOf(IPAdrRow));
        try FreeMem(pBuf) except end;
      end;

      Size:=0;
      r:=GetIPNetTable(nil,@Size,False);
      if r=ERROR_NO_DATA then
        Exit;
      GetMem(pBuf,Size);
      n:=0;
      try
        r:=GetIpNetTable(PMIB_IPNETTABLE(pBuf),@Size,False);
        if r=NO_ERROR then begin
          n:=PMIB_IPNETTABLE(pBuf)^.dwNumEntries;
          if n>0 then begin
            Inc(pBuf,SizeOf(Cardinal));
            SetLength(FARP,n);
            for j:=1 to n do begin
              IPNetRow:=PMIB_IPNETROW(PBuf)^;
              FARP[j-1].IP:=IPv4ToStr(CardinalToIPv4(Swap32(IPNetRow.dwAddr)));
              FARP[j-1].Mask:='255.255.255.0';
              FARP[j-1].IntfIdx:=IPNetRow.dwIndex;
              FARP[j-1].Typ:=[];
              FARP[j-1].ARPType:=IPNetRow.dwType;
              s:='';
              if IPNetRow.dwPhysAddrLen>0 then begin
                for i:=0 to IPNetRow.dwPhysAddrLen-1 do
                  s:=s+Format('%2.2x:',[IPNetRow.bPhysAddr[i]]);
                SetLength(s,Length(s)-1);
              end;
              FARP[j-1].Physical:=s;
              Inc(pBuf,SizeOf(IPAdrRow));
            end;
          end;
        end;
      finally
        Dec(pBuf,SizeOf(Cardinal)+n*SizeOf(IPNetRow));
        try FreeMem(pBuf) except end;
      end;

      r:=GetBestInterface(inet_addr(PAnsiChar('8.8.8.8')),n);
      if r=NO_ERROR then
        FBII:=n
      else
        FBII:=FindFirstOperationalIntfIdx;
    finally
      FRefreshing:=False;
    end;
  end;
end;

procedure TNetworkConfiguration.SetAdapterImageIndex(Index: Word;
  const Value: integer);
begin
  FAdapters[Index]._ImageIndex:=Value;
end;

procedure TNetworkConfiguration.SetAdapterMark(Index: Word;
  const Value: boolean);
begin
  FAdapters[Index]._Mark:=Value;
end;

class function TNetworkConfiguration.SetDNS(AIntfIdx: Cardinal; AStatic: Boolean;
  APreferred, AAlternate: string): Int64;
const
  WbemUser='';
  WbemPassword='';
  WbemComputer='localhost';
  wbemFlagForwardOnly=$00000020;
var
  FSWbemLocator,
  FWMIService,
  FWbemObjectSet,
  FWbemObject,
  FOutParams,
  vDNS: OLEVariant;
  oEnum: IEnumVariant;
  iValue: LongWord;
begin
  FSWbemLocator:=CreateOleObject('WbemScripting.SWbemLocator');
  FWMIService:=FSWbemLocator.ConnectServer(WbemComputer,'root\CIMV2',WbemUser,WbemPassword);
  FWbemObjectSet:=FWMIService.ExecQuery(Format('SELECT * FROM Win32_NetworkAdapterConfiguration WHERE InterfaceIndex=%d',[AIntfIdx]),'WQL',wbemFlagForwardOnly);
  oEnum:=IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;
  if oEnum.Next(1,FWbemObject,iValue)=0 then begin
    if AStatic then begin
      if Length(AAlternate)>3 then
        vDNS:=VarArrayCreate([0,1],varVariant)
      else
        vDNS:=VarArrayCreate([0,0],varVariant);
      vDNS[0]:=APreferred;
      if Length(AAlternate)>3 then
        vDNS[1]:=AAlternate;
      FOutParams:=FWbemObject.SetDNSServerSearchOrder(vDNS);
      Result:=FOutParams;
    end else begin
      FOutParams:=FWbemObject.SetDNSServerSearchOrder(null);
      Result:=FOutParams;
    end;
  end else
    Result:=-1;
end;

class function TNetworkConfiguration.SetIP(AIntfIdx: Cardinal; AStatic: Boolean; AIPv4,AMask,AGateway: string): Int64;
const
  WbemUser='';
  WbemPassword='';
  WbemComputer='localhost';
  wbemFlagForwardOnly=$00000020;
var
  FSWbemLocator,
  FWMIService,
  FWbemObjectSet,
  FWbemObject,
  FOutParams,
  vIpAddress,
  vGateways,
  vMask: OLEVariant;
  oEnum: IEnumVariant;
  iValue: LongWord;
begin
  FSWbemLocator:=CreateOleObject('WbemScripting.SWbemLocator');
  FWMIService:=FSWbemLocator.ConnectServer(WbemComputer,'root\CIMV2',WbemUser,WbemPassword);
  FWbemObjectSet:=FWMIService.ExecQuery(Format('SELECT * FROM Win32_NetworkAdapterConfiguration WHERE InterfaceIndex=%d',[AIntfIdx]),'WQL',wbemFlagForwardOnly);
  oEnum:=IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;
  if oEnum.Next(1,FWbemObject,iValue)=0 then begin
    if AStatic then begin
      vIpAddress:=VarArrayCreate([0,0],varVariant);
      vIpAddress[0]:=AIPv4;
      vMask:=VarArrayCreate([0,0],varVariant);
      vMask[0]:=AMask;
      FOutParams:=FWbemObject.EnableStatic(vIpAddress,vMask);
      Result:=FOutParams;
      vGateways:=VarArrayCreate([0,0],varVariant);
      vGateways[0]:=AGateway;
      FOutParams:=FWbemObject.SetGateways(vGateways);
      if Result=0 then
        Result:=FOutParams;
    end else begin
      FOutParams:=FWbemObject.EnableDHCP;
      Result:=FOutParams;
    end;
  end else
    Result:=-1;
end;

end.
