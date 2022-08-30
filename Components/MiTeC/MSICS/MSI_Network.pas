{*******************************************************}
{       MiTeC System Information Component Suite        }
{               Network Detection Part                  }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_Network;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.WinSock,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, WinSock, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MSI_Common, MSI_Defs, MiTeC_IpHlpAPI, MiTeC_NetBIOS, MiTeC_Shares, MiTeC_Ws2_32;

const
  StorageFolderName = 'Network';
  TCPIP_StorageFolderName = 'TCPIP';
  Winsock_StorageFolderName = 'Winsock';
  Shares_StorageFolderName = 'Shares';
  Sessions_StorageFolderName = 'Sessions';
  OpenFiles_StorageFolderName = 'OpenFiles';
  Connections_StorageFolderName = 'Connections';

type
  TConnectionList = array of TConnectionRecord;
  TShareList = array of TNTShareRecord;
  TOpenFileList = array of TOpenFileRecord;
  TSessionList = array of TNTSessionRecord;

  TMiTeC_NetResources =  class(TMiTeC_Component)
  private
    FConns: TConnections;
    FShares: TNTShares;
    FConnList: TConnectionList;
    FShareList: TShareList;
    FSessionList: TSessionList;
    FFileList: TOpenFileList;
    function GetConn(Index: Cardinal): TConnectionRecord;
    function GetConnCount: Cardinal;
    function GetOpenFile(Index: Cardinal): TOpenFileRecord;
    function GetOpenFileCount: Cardinal;
    function GetSession(Index: Cardinal): TNTSessionRecord;
    function GetSessionCount: Cardinal;
    function GetShare(Index: Cardinal): TNTShareRecord;
    function GetShareCount: Cardinal;

    procedure SaveConnsToStorage(const AFilename: string; ACodeStream: TCodeStreamProcedure = nil);
    procedure SaveSharesToStorage(const AFilename: string; ACodeStream: TCodeStreamProcedure = nil);
    procedure SaveSessionsToStorage(const AFilename: string; ACodeStream: TCodeStreamProcedure = nil);
    procedure SaveFilesToStorage(const AFilename: string; ACodeStream: TCodeStreamProcedure = nil);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;

    property ConnectionCount: Cardinal read GetConnCount;
    property Connections[Index: Cardinal]: TConnectionRecord read GetConn;

    property ShareCount: Cardinal read GetShareCount;
    property Shares[Index: Cardinal]: TNTShareRecord read GetShare;

    property SessionCount: Cardinal read GetSessionCount;
    property Sessions[Index: Cardinal]: TNTSessionRecord read GetSession;

    property OpenFileCount: Cardinal read GetOpenFileCount;
    property OpenFiles[Index: Cardinal]: TOpenFileRecord read GetOpenFile;
  end;

  TMiTeC_Winsock = class(TMiTeC_Component)
  private
    FDesc: string;
    FStat: string;
    FMajVer: word;
    FMinVer: word;
  public
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
  published
    property Description: string read FDesc stored False;
    property MajorVersion: word read FMajVer stored False;
    property MinorVersion: word read FMinVer stored False;
    property Status: string read FStat stored False;
  end;

  TAdapterType = (atOther, atEthernet, atTokenRing, atFDDI, atPPP, atLoopback, atATM, atIEEE80211, atTunnel, atIEEE1394, atIEEE80216_WMAN, atWWANPP, atWWANPP2);

  TAdapter = record
    Name,
    Alias,
    AdapterName,
    Address: string;
    MaxSpeed,
    MTU: Cardinal;
    Typ: TAdapterType;
    AdminStatus,
    OperStatus: Cardinal;
    EnableDHCP,
    HaveWINS,
    DNSEnabled,
    NETBIOSEnabled,
    IPv4Enabled,
    IPv6Enabled: Boolean;
    IPAddress,
    IPAddressMask,
    Gateway_IPAddress,
    Gateway_IPAddressMask,
    DHCP_IPAddress,
    DHCP_IPAddressMask,
    PrimaryWINS_IPAddress,
    PrimaryWINS_IPAddressMask,
    SecondaryWINS_IPAddress,
    SecondaryWINS_IPAddressMask: TStringList;
    DNSServers: TStringList;

    IPv6Address,
    Gateway_IPv6,
    DHCP_IPv6,
    PrimaryWINS_IPv6,
    DNSServers_IPv6: TStringList;

    DNSSuffix: string;
    IntfIdx: Cardinal;
  end;

  TAdapterList = array of TAdapter;

  TAddressRecord = record
    IP,
    Mask: string;
    IntfIdx: Cardinal;
    Typ: Word;
  end;

  TAddressTable = array of TAddressRecord;

  TNodeType = (ntUnknown, ntBroadcast, ntPeerToPeer, ntMixed, ntHybrid);

  TMiTeC_TCPIP = class(TMiTeC_Component)
  private
    FAdapters: TAdapterList;
    FAT: TAddressTable;
    FProxy: boolean;
    FRouting: boolean;
    FDNS: boolean;
    FHost: string;
    FDomain: string;
    FDNSSuf: string;
    FDNSList,
    FIL,
    FDNSSuffix: TStrings;
    FNode: TNodeType;
    FDHCPScope: string;
    FBII: Cardinal;
    procedure ClearList;
    function GetAdapter(Index: Word): TAdapter;
    function GetAdapterCount: Word;
    function Add(ARecord: TAdapter): Integer;
    function GetAddrRec(Index: Word): TAddressRecord;
    function GetAddrRecCount: Word;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
    function FindAdapter(AName: string): Integer; overload;
    function FindAdapter(AIntfIdx: Cardinal): Integer; overload;
    function FindAddress(AIP: string): Integer; overload;
    function FindAddress(AIntfIdx: Cardinal): Integer; overload;
    function GetSubnetMask(AIP: string): string;

    property Adapter[Index: Word]: TAdapter read GetAdapter;
    property AddressRecord[Index: Word]: TAddressRecord read GetAddrRec;
  published
    property AdapterCount: Word read GetAdapterCount stored False;
    property AddressRecordCount: Word read GetAddrRecCount stored False;
    property HostName: string read FHost stored False;
    property DomainName: string read FDomain stored False;
    property EnableProxy: boolean read FProxy stored False;
    property EnableRouting: boolean read FRouting stored False;
    property EnableDNS: boolean read FDNS stored False;
    property PrimaryDNSSuffix: string read FDNSSuf stored False;
    property DHCPScopeName: string read FDHCPScope stored False;
    property DNSServers: TStrings read FDNSList stored False;
    property DNSSuffixes: TStrings read FDNSSuffix stored False;
    property NodeType: TNodeType read FNode stored False;
    property BestInterfaceIdx: Cardinal read FBII stored False;
  end;

  TMiTeC_Network = class(TMiTeC_Component)
  private
    FVirtAdapter,FPhysAdapter: TStrings;
    FWinsock: TMiTeC_Winsock;
    FIPAddress: TStrings;
    FMACAddress: TStrings;
    FCli: TStrings;
    FServ: TStrings;
    FProto: TStrings;
    FTCPIP: TMiTeC_TCPIP;
    FNetResources: TMiTeC_NetResources;
    //FDevices: TMiTeC_Devices;
    function GetLocalIP :string;
  protected
    procedure SetHeaderReader(const Value: THeaderReader); override;
    procedure SetHeaderWriter(const Value: THeaderWriter); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
  published
    //property MiTeC_Devices: TMiTeC_Devices read FDevices write FDevices;
    property IPAddresses: TStrings read FIPAddress stored false;
    property MACAddresses: TStrings read FMACAddress stored False;
    property PhysicalAdapters :TStrings read FPhysAdapter stored False;
    property VirtualAdapters :TStrings read FVirtAdapter stored false;
    property Protocols :TStrings read FProto stored False;
    property Services :TStrings read FServ stored False;
    property Clients :TStrings read FCli stored False;
    property WinSock: TMiTeC_Winsock read FWinsock stored false;
    property Resources: TMiTeC_NetResources read FNetResources stored False;
    property TCPIP: TMiTeC_TCPIP read FTCPIP stored False;
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

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry,
     {$ELSE}
     Registry,
     {$ENDIF}
     MiTeC_StrUtils, MiTeC_Routines, MiTeC_IPTypes, MiTeC_CfgMgrSetupAPI, MiTeC_RegUtils;

{ TMiTeC_Winsock }


procedure TMiTeC_Winsock.Clear;
begin

end;

function TMiTeC_Winsock.LoadFromStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Clear;
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    Result:=False;
    try
      Sub:=SS.OpenSubStorage(Winsock_StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;
    if Sub<>nil then
    try
      strm:=Sub.OpenStream(strm_Props,STG_READ_INSTORAGE,False);
      if strm<>nil then
        try
          sl:=TStringList.Create;
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            Self.FMajVer:=ReadIntProperty(sl,'MajorVersion');
            Self.FMinVer:=ReadIntProperty(sl,'MinorVersion');
            Self.FDesc:=ReadStrProperty(sl,'Description');
            Self.FStat:=ReadStrProperty(sl,'Status');
            Result:=True;
            SetDataAvail(True);
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;
    finally
      if Sub<>nil then
        Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_Winsock.RefreshData;
var
  GInitData :TWSADATA;
begin
  inherited;
  Clear;
  if wsastartup($101,GInitData)=0 then begin
    FDesc:=string(GInitData.szDescription);
    FStat:=string(GInitData.szSystemStatus);
    FMajVer:=Hi(GInitData.wHighVersion);
    FMinVer:=Lo(GInitData.wHighVersion);
    wsacleanup;
  end else
    FStat:='Winsock cannot be initialized.';
  SetDataAvail(True);  
end;

procedure TMiTeC_Winsock.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
begin
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(Winsock_StorageFolderName);
    Sub:=SS.OpenSubStorage(Winsock_StorageFolderName,STG_OPEN,True);
    try
      sl:=TStringList.Create;
      try
        WriteIntProperty(sl,'MajorVersion',Self.MajorVersion);
        WriteIntProperty(sl,'MinorVersion',Self.MinorVersion);
        WriteStrProperty(sl,'Description',Self.Description);
        WriteStrProperty(sl,'Status',Self.Status);

        strm:=Sub.OpenStream(strm_Props,STG_OPEN,True);
        try
          SaveToEncodedStream(sl,strm,ACodeStream);
        finally
          strm.Free;
        end;
      finally
        sl.Free;
      end;
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

{ TMiTeC_Network }

function TMiTeC_Network.GetLocalIP: string;
type
  TaPInAddr = array [0..255] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe  :PHostEnt;
  pptr :PaPInAddr;
  Buffer :array [0..63] of ansichar;
  i :integer;
  GInitData :TWSADATA;
begin
  wsastartup($101,GInitData);
  result:='';
  GetHostName(Buffer,SizeOf(Buffer));
  phe:=GetHostByName(buffer);
  if not assigned(phe) then
    exit;
  pptr:=PaPInAddr(Phe^.h_addr_list);
  i:=0;
  while pptr^[I]<>nil do begin
    result:=Result+IPv4ToStr(InAddrToIPv4(pptr^[I]^))+',';
    inc(i);
  end;
  Delete(Result,Length(Result),1);
  wsacleanup;
end;

function TMiTeC_Network.LoadFromStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
begin
  inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Clear;
  {$B+}
  Result:=TCPIP.LoadFromStorage(AFilename,AReadHeader,ACodeStream)
          and Winsock.LoadFromStorage(AFilename,AReadHeader,ACodeStream)
          and Resources.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  {$B-}
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    Result:=False;
    try
      Sub:=SS.OpenSubStorage(StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;
    if Sub<>nil then
    try
      strm:=Sub.OpenStream(strm_Props,STG_READ_INSTORAGE,False);
      if strm<>nil then
        try
          sl:=TStringList.Create;
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            Self.FPhysAdapter.CommaText:=ReadStrProperty(sl,'PhysicalAdapters');
            Self.FVirtAdapter.CommaText:=ReadStrProperty(sl,'VirtualAdapters');
            Self.FIPAddress.CommaText:=ReadStrProperty(sl,'IPAdress');
            Self.FMACAddress.CommaText:=ReadStrProperty(sl,'MACAddress');
            Self.FProto.CommaText:=ReadStrProperty(sl,'Protocols');
            Self.FServ.CommaText:=ReadStrProperty(sl,'Services');
            Self.FCli.CommaText:=ReadStrProperty(sl,'Clients');
            Result:=True;
            SetDataAvail(True);
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;
    finally
      if Sub<>nil then
        Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_Network.RefreshData;
var
  i: integer;
  s,ck,dv,li,nm,rlk,rlv: string;
  sl: TStringList;
  guid: TGUID;
  did: TSPDeviceInterfaceData;
  pdidd: PSPDeviceInterfaceDetailData;
  dinfo: TSPDevInfoData;
  hdev: HDEVINFO;
  le,n: Cardinal;
const
  rkNetworkNT = {HKEY_LOCAL_MACHINE}'\SOFTWARE\Microsoft\Windows NT\CurrentVersion\NetworkCards';
  rkNetwork2K = {HKEY_LOCAL_MACHINE}'\SYSTEM\CurrentControlSet\Control\Network';
  rkClass = {HKEY_LOCAL_MACHINE}'\SYSTEM\CurrentControlSet\Control\Class';

  rvNetworkNT = 'Description';

  rvProtoClass = 'NetTrans';
  rvServClass = 'NetService';
  rvCliClass = 'NetClient';

procedure GetComponents(AGroup: string; AList: TStrings);
var
  i,j,p: Integer;
  kl1,kl2,vl: TStringList;
  s,d,f: string;
begin
  with OpenRegistryReadOnly do begin
    kl1:=TStringList.Create;
    kl2:=TStringList.Create;
    vl:=TStringList.Create;
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rkClass,False) then begin
        GetKeyNames(kl1);
        CloseKey;
        for i:=0 to kl1.Count-1 do begin
          if OpenKey(rkClass+'\'+kl1[i],False) then begin
            s:='';
            if ValueExists('Class') then
              s:=ReadString('Class');
            CloseKey;
            if not SameText(s,AGroup) then
              Continue;
            if OpenKey(rkNetwork2K+'\'+kl1[i],False) then begin
              GetKeyNames(kl2);
              CloseKey;
              for j:=0 to kl2.Count-1 do begin
                if OpenKey(rkNetwork2K+'\'+kl1[i]+'\'+kl2[j],False) then begin
                  if ValueExists(rvNetworkNT) then begin
                    d:=ReadString(rvNetworkNT);
                    p:=Pos(';',d);
                    if p>0 then
                      d:=Copy(d,p+1,255)
                    else begin
                      if Pos('@',d)=1 then
                        Delete(d,1,1);
                      p:=Pos(',',d);
                      if p>0 then begin
                        f:=ExpandEnvVars(Copy(d,1,p-1));
                        try
                          p:=StrToInt(Copy(d,p+2,255));
                        except
                          p:=0;
                        end;
                        d:=LoadResourceString(f,p);
                      end;
                    end;
                    if d='' then
                      d:=GetFileDesc(f);
                    if d='' then
                      d:=ExtractFilename(d);
                    AList.Add(d);
                  end;
                  CloseKey;
                end;
              end;
            end;
            Break;
          end;
        end;
      end;
    finally
      kl1.Free;
      kl2.Free;
      vl.Free;
      Free;
    end;
  end;
end;

begin
  inherited;

  Clear;
  {
  s:='';
  if Assigned(FDevices) then
    with FDevices do begin
      if not (Owner is TMiTeC_Component) or not FDevices.DataAvailable then
        RefreshData;
      for i:=0 to DeviceCount-1 do begin
        s:=Devices[i].Name;
        if Devices[i].DeviceClass=dcNet then begin
          if (Devices[i].ResourceListKey<>'') and (Devices[i].Location<>'') then
            FPhysAdapter.Add(s)
          else
            FVirtAdapter.Add(s)
        end;
      end;
    end;
  }

  guid:=GUID_DEVINTERFACE_NET;
  hdev:=SetupDiGetClassDevs(@guid,nil,0,DIGCF_PRESENT or DIGCF_DEVICEINTERFACE);
  if (INVALID_HANDLE_VALUE<>THandle(hdev)) then begin
    try
      for i:=0 to 99 do begin
        FillChar(did,SizeOf(did),0);
        did.cbSize:=SizeOf(did);
        if (SetupDiEnumDeviceInterfaces(hdev,nil,guid,i,did)) then begin
          n:=0;
          SetupDiGetDeviceInterfaceDetail(hdev,@did,nil,0,n,nil);
          le:=GetLastError;
          if (le=ERROR_INSUFFICIENT_BUFFER) then begin
            n:=n;
            pdidd:=AllocMem(n);
            try
              pdidd.cbSize:=SizeOf(TSPDeviceInterfaceDetailData);
              dinfo.cbSize:=sizeof(TSPDevInfoData);
              if (SetupDiGetDeviceInterfaceDetail(hdev,@did,pdidd,n,n,@dinfo)) then begin
                s:=PChar(@(pdidd.DevicePath));
                if (Trim(s)<>'') and (Trim(s)<>'\') then begin
                  nm:=GetString(hdev,dinfo,SPDRP_DEVICEDESC);
                  li:=GetString(hdev,dinfo,SPDRP_LOCATION_INFORMATION);
                  GetResourceListLocation('\SYSTEM\CurrentControlSet\Enum\'+FastStringReplace(Copy(s,5,Pos('{',s)-5),'#','\'),rlk,rlv);
                  if (rlk<>'') and (li<>'') then
                    FPhysAdapter.Add(nm)
                  else
                    FVirtAdapter.Add(nm);
                end;
              end;
            finally
              FreeMem(pdidd);
            end;
          end;
        end else begin
          le:=GetLastError;
          if le=ERROR_NO_MORE_ITEMS then
            Break;
        end;
      end;
    finally
      SetupDiDestroyDeviceInfoList(hdev);
    end;
  end;

  Resources.RefreshData;
  Winsock.RefreshData;
  TCPIP.RefreshData;

  with OpenRegistryReadOnly do begin
    sl:=TStringList.Create;
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rkNetworkNT,False) then begin
        GetKeyNames(sl);
        CloseKey;
        for i:=0 to sl.Count-1 do
          if OpenKey(rkNetworkNT+'\'+sl[i],False) then begin
            s:=ReadString(rvNetworkNT);
            if FPhysAdapter.IndexOf(s)=-1 then
              FPhysAdapter.Add(s);
            Closekey;
          end;
      end;
    finally
      sl.Free;
      Free;
    end;
  end;

  FIPAddress.CommaText:=GetLocalIP;

  ck:=rkNetwork2K;
  dv:=rvNetworkNT;
  GetComponents(rvProtoClass,FProto);
  GetComponents(rvServClass,FServ);
  GetComponents(rvCliClass,FCli);

  for i:=0 to TCPIP.AdapterCount-1 do
    if (TCPIP.Adapter[i].OperStatus=MIB_IF_OPER_STATUS_OPERATIONAL) then
      FMACAddress.Add(TCPIP.Adapter[i].Address);

  //NB_GetMACAddresses(Machinename,FMACAddress);

  SetDataAvail(True);
end;

procedure TMiTeC_Network.Clear;
begin
  FPhysAdapter.Clear;
  FVirtAdapter.Clear;
  FIPAddress.Clear;
  FMACAddress.Clear;
  FCli.Clear;
  FServ.Clear;
  FProto.Clear;
  Resources.Clear;
  Winsock.Clear;
  TCPIP.ClearList;
end;

constructor TMiTeC_Network.Create;
begin
  inherited Create(AOwner);
  FNetResources:=TMiTeC_NetResources.Create(Self);
  FNetResources.Name:='NetResources';
  FWinsock:=TMiTeC_Winsock.Create(Self);
  FWinsock.Name:='Winsock';
  FTCPIP:=TMiTeC_TCPIP.Create(Self);
  FTCPIP.Name:='TCPIP';

  FVirtAdapter:=TStringList.Create;
  FPhysAdapter:=TStringList.Create;
  FIPAddress:=TStringList.Create;
  FMACAddress:=TStringList.Create;
  FProto:=TStringList.Create;
  FServ:=TStringList.Create;
  FCli:=TStringList.Create;
end;

destructor TMiTeC_Network.Destroy;
begin
  FNetResources.Free;
  FWinsock.Destroy;
  FTCPIP.Destroy;
  FVirtAdapter.Destroy;
  FPhysAdapter.Destroy;
  FMACAddress.Destroy;
  FIPAddress.Destroy;
  FProto.Destroy;
  FCli.Destroy;
  FServ.Destroy;
  inherited;
end;

procedure TMiTeC_Network.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
begin
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Resources.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Winsock.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  TCPIP.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(StorageFolderName);
    Sub:=SS.OpenSubStorage(StorageFolderName,STG_OPEN,True);
    try
      sl:=TStringList.Create;
      try
        WriteStrProperty(sl,'PhysicalAdapters',Self.PhysicalAdapters.CommaText);
        WriteStrProperty(sl,'VirtualAdapters',Self.VirtualAdapters.CommaText);
        WriteStrProperty(sl,'IPAdress',Self.IPAddresses.CommaText);
        WriteStrProperty(sl,'MACAddress',Self.MACAddresses.CommaText);
        WriteStrProperty(sl,'Protocols',Self.Protocols.CommaText);
        WriteStrProperty(sl,'Services',Self.Services.CommaText);
        WriteStrProperty(sl,'Clients',Self.Clients.CommaText);

        strm:=Sub.OpenStream(strm_Props,STG_OPEN,True);
        try
          SaveToEncodedStream(sl,strm,ACodeStream);
        finally
          strm.Free;
        end;
      finally
        sl.Free;
      end;
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_Network.SetHeaderReader(const Value: THeaderReader);
begin
  inherited;
  FNetResources.OnReadHeader:=Value;
  FWinsock.OnReadHeader:=Value;
  FTCPIP.OnReadHeader:=Value;
end;

procedure TMiTeC_Network.SetHeaderWriter(const Value: THeaderWriter);
begin
  inherited;
  FNetResources.OnWriteHeader:=Value;
  FWinsock.OnWriteHeader:=Value;
  FTCPIP.OnWriteHeader:=Value;
end;

{ TMiTeC_TCPIP }

function TMiTeC_TCPIP.Add(ARecord: TAdapter): Integer;
begin
  SetLength(FAdapters,Length(FAdapters)+1);
  Result:=High(FAdapters);
  FAdapters[Result]:=ARecord;
end;

procedure TMiTeC_TCPIP.Clear;
begin
  ClearList;
  Finalize(FAT);
  FProxy:=False;
  FRouting:=False;
  FDNS:=False;
  FHost:='';
  FDomain:='';
  FDNSList.Clear;
  FNode:=ntUnknown;
  FDHCPScope:='';
  FDNSSuf:='';
  FBII:=0;
  FIL.Clear;
end;

procedure TMiTeC_TCPIP.ClearList;
var
  i: Integer;
begin
  for i:=0 to High(FAdapters) do begin
    FAdapters[i].IPAddress.Free;
    FAdapters[i].IPAddressMask.Free;
    FAdapters[i].Gateway_IPAddress.Free;
    FAdapters[i].Gateway_IPAddressMask.Free;
    FAdapters[i].DHCP_IPAddress.Free;
    FAdapters[i].DHCP_IPAddressMask.Free;
    FAdapters[i].PrimaryWINS_IPAddress.Free;
    FAdapters[i].PrimaryWINS_IPAddressMask.Free;
    FAdapters[i].SecondaryWINS_IPAddress.Free;
    FAdapters[i].SecondaryWINS_IPAddressMask.Free;
    FAdapters[i].DNSServers.Free;

    FAdapters[i].IPv6Address.Free;
    FAdapters[i].Gateway_IPv6.Free;
    FAdapters[i].DHCP_IPv6.Free;
    FAdapters[i].PrimaryWINS_IPv6.Free;
    FAdapters[i].DNSServers_IPv6.Free;

    Finalize(FAdapters[i]);
  end;
  Finalize(FAdapters);
end;

constructor TMiTeC_TCPIP.Create;
begin
  inherited Create(AOwner);
  FDNSList:=TStringList.Create;
  FDNSSuffix:=TStringList.Create;
  FIL:=TStringList.Create;
end;

destructor TMiTeC_TCPIP.Destroy;
begin
  ClearList;
  FDNSList.Destroy;
  FDNSSuffix.Destroy;
  FIL.Free;
  inherited;
end;

function TMiTeC_TCPIP.FindAdapter(AIntfIdx: Cardinal): Integer;
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

function TMiTeC_TCPIP.FindAdapter(AName: string): Integer;
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

function TMiTeC_TCPIP.GetAdapter(Index: Word): TAdapter;
begin
  Result:=FAdapters[Index];
end;

function TMiTeC_TCPIP.GetAdapterCount: Word;
begin
  Result:=Length(FAdapters);
end;

function TMiTeC_TCPIP.GetAddrRec(Index: Word): TAddressRecord;
begin
  Result:=FAT[Index];
end;

function TMiTeC_TCPIP.GetAddrRecCount: Word;
begin
  Result:=Length(FAT);
end;

function TMiTeC_TCPIP.GetSubnetMask(AIP: string): string;
var
  c: Byte;
begin
  Result:='0.0.0.0';
  c:=StrToIntDef(Copy(AIP,1,Pos('.',AIP)-1),0);
  if c in [1..126] then
    Result:='255.0.0.0'
  else if c in [128..191] then
    Result:='255.255.0.0'
  else if c in [192..223] then
    Result:='255.255.255.0';
end;

function TMiTeC_TCPIP.LoadFromStorage;
var
  stg: IStorage;
  SS, Sub: TStructuredStorage;

function ReadAdapterFromStream(AIndex: Cardinal): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
  A: TAdapter;
begin
  Result:=False;
  try strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        A.IPAddress:=TStringList.Create;
        A.IPAddressMask:=TStringList.Create;
        A.Gateway_IPAddress:=TStringList.Create;
        A.Gateway_IPAddressMask:=TStringList.Create;
        A.DHCP_IPAddress:=TStringList.Create;
        A.DHCP_IPAddressMask:=TStringList.Create;
        A.PrimaryWINS_IPAddress:=TStringList.Create;
        A.PrimaryWINS_IPAddressMask:=TStringList.Create;
        A.SecondaryWINS_IPAddress:=TStringList.Create;
        A.SecondaryWINS_IPAddressMask:=TStringList.Create;
        A.DNSServers:=TStringList.Create;

        A.IPv6Address:=TStringList.Create;
        A.Gateway_IPv6:=TStringList.Create;
        A.DHCP_IPv6:=TStringList.Create;
        A.PrimaryWINS_IPv6:=TStringList.Create;
        A.DNSServers_IPv6:=TStringList.Create;

        A.Name:=ReadStrProperty(sl,'Name');
        A.AdapterName:=ReadStrProperty(sl,'AdapterName');
        A.Alias:=ReadStrProperty(sl,'Alias');
        A.Address:=ReadStrProperty(sl,'Address');
        A.MaxSpeed:=ReadIntProperty(sl,'MaxSpeed');
        A.MTU:=ReadIntProperty(sl,'MTU');
        A.OperStatus:=ReadIntProperty(sl,'OperStatus');
        A.AdminStatus:=ReadIntProperty(sl,'AdminStatus');
        A.Typ:=TAdapterType(ReadIntProperty(sl,'Typ'));
        A.EnableDHCP:=ReadIntProperty(sl,'EnableDHCP')=1;
        A.HaveWINS:=ReadIntProperty(sl,'HaveWINS')=1;
        A.DNSEnabled:=ReadIntProperty(sl,'DNSEnabled')=1;
        A.IPv4Enabled:=ReadIntProperty(sl,'IPv4Enabled')=1;
        A.IPv6Enabled:=ReadIntProperty(sl,'IPv6Enabled')=1;
        A.IPAddress.CommaText:=ReadStrProperty(sl,'IPAddress');
        A.IPAddressMask.CommaText:=ReadStrProperty(sl,'IPAddressMask');
        A.Gateway_IPAddress.CommaText:=ReadStrProperty(sl,'Gateway_IPAddress');
        A.Gateway_IPAddressMask.CommaText:=ReadStrProperty(sl,'Gateway_IPAddressMask');
        A.DHCP_IPAddress.CommaText:=ReadStrProperty(sl,'DHCP_IPAddress');
        A.DHCP_IPAddressMask.CommaText:=ReadStrProperty(sl,'DHCP_IPAddressMask');
        A.PrimaryWINS_IPAddress.CommaText:=ReadStrProperty(sl,'PrimaryWINS_IPAddress');
        A.PrimaryWINS_IPAddressMask.CommaText:=ReadStrProperty(sl,'PrimaryWINS_IPAddressMask');
        A.SecondaryWINS_IPAddress.CommaText:=ReadStrProperty(sl,'SecondaryWINS_IPAddress');
        A.SecondaryWINS_IPAddressMask.CommaText:=ReadStrProperty(sl,'SecondaryWINS_IPAddressMask');
        A.DNSServers.CommaText:=ReadStrProperty(sl,'DNSServers');

        A.IPv6Address.CommaText:=ReadStrProperty(sl,'IPv6Address');
        A.Gateway_IPv6.CommaText:=ReadStrProperty(sl,'Gateway_IPv6');
        A.DHCP_IPv6.CommaText:=ReadStrProperty(sl,'DHCP_IPv6');
        A.PrimaryWINS_IPv6.CommaText:=ReadStrProperty(sl,'PrimaryWINS_IPv6');
        A.DNSServers_IPv6.CommaText:=ReadStrProperty(sl,'DNSServers_IPv6');

        A.DNSSuffix:=ReadStrProperty(sl,'DNSSuffix');
        A.IntfIdx:=ReadIntProperty(sl,'InterfaceIndex');
        Add(A);
        Result:=True;
        SetDataAvail(True);
      finally
        sl.Free;
      end;
    finally
      strm.Free;
    end;
end;

function ReadAddressRecordFromStream(AIndex: Cardinal): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
  A: TAddressRecord;
begin
  Result:=False;
  try strm:=Sub.OpenStream(Format(strm_IP,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        A.IP:=ReadStrProperty(sl,'IP');
        A.Mask:=ReadStrProperty(sl,'Mask');
        A.IntfIdx:=ReadIntProperty(sl,'InterfaceIndex');
        A.Typ:=ReadIntProperty(sl,'Typ');
        SetLength(FAT,Length(FAT)+1);
        FAT[High(FAT)]:=A;
        Result:=True;
        SetDataAvail(True);
      finally
        sl.Free;
      end;
    finally
      strm.Free;
    end;
end;

var
  i: Integer;
  strm: TStorageStream;
  sl: TStringList;
begin
  i:=0;
  Clear;
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    try
      Sub:=SS.OpenSubStorage(TCPIP_StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;
    try
      if Sub<>nil then begin
        strm:=Sub.OpenStream(strm_Props,STG_READ_INSTORAGE,False);
        if strm<>nil then
          try
            sl:=TStringList.Create;
            try
              LoadFromEncodedStream(strm,sl,ACodeStream);
              Self.FHost:=ReadStrProperty(sl,'HostName');
              Self.FDomain:=ReadStrProperty(sl,'DomainName');
              Self.FDNSSuf:=ReadStrProperty(sl,'PrimaryDNSSuffix');
              Self.FDNSList.CommaText:=ReadStrProperty(sl,'DNSServers');
              Self.FDNSSuffix.CommaText:=ReadStrProperty(sl,'DNSSuffixes');
              Self.FDHCPScope:=ReadStrProperty(sl,'DHCPScopeName');
              Self.FNode:=TNodeType(ReadIntProperty(sl,'NodeType'));
              Self.FProxy:=ReadIntProperty(sl,'EnableProxy')=1;
              Self.FRouting:=ReadIntProperty(sl,'EnableRouting')=1;
              Self.FDNS:=ReadIntProperty(sl,'EnableDNS')=1;
              Self.FBII:=ReadIntProperty(sl,'BestInterfaceIndex');
              Result:=True;
              SetDataAvail(True);
            finally
              sl.Free;
            end;
          finally
            strm.Free;
          end;

          i:=0;
          while ReadAddressRecordFromStream(i) do
            Inc(i);

          i:=0;
          while ReadAdapterFromStream(i) do
            Inc(i);
        end;
        Result:=Result or (i>0);
      finally
        Sub.Free;
      end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_TCPIP.RefreshData;
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
  IPAdrRow: TMIB_IPADDRROW;
  np: PFixedInfo;
  r,j,n: Cardinal;
  Size: ulong;
  A: TAdapter;
  s: string;
  pBuf,pb: PAnsiChar;
  idx: Integer;
  luid: NET_LUID;
  alias: array[0..IF_MAX_STRING_SIZE] of char;
  sa4: sockaddr_in;
  sa6: TSockAddrIn6;
  buf: array[0..255] of char;
const
  rkTCPIP = {HKEY_LOCAL_MACHINE\}'SYSTEM\CurrentControlSet\Services\Tcpip\Parameters';
  rvSL = 'SearchList';
begin
  inherited;
  Clear;
  if InitIpHlpAPI then begin
    size:=SizeOf(TIP_INTERFACE_INFO);
    pBuf:=AllocMem(size);
    try
      r:=GetInterfaceInfo(PIP_INTERFACE_INFO(pBuf),size);
      while(r=ERROR_INSUFFICIENT_BUFFER) do begin
        size:=Size+SizeOf(TIP_INTERFACE_INFO);
        ReallocMem(pBuf,size);
        r:=GetInterfaceInfo(PIP_INTERFACE_INFO(pBuf),size);
      end;
      pb:=pBuf;
      if(r=ERROR_SUCCESS) then begin
        n:=PIP_INTERFACE_INFO(pb).NumAdapters;
        if n>0 then begin
          Inc(pb,SizeOf(integer));
          for j:=1 to n do begin
            aim:=PIP_ADAPTER_INDEX_MAP(pb)^;
            s:={$IFNDEF UNICODE}WideToAnsi{$ENDIF}(Trim(WideString(aim.Name)));
            idx:=Pos('}',s);
            if idx>0 then
              SetLength(s,idx);
            FIL.Add(Format('%s=%d',[s,aim.Index]));
            Inc(pb,SizeOf(Integer)+SizeOf(TIP_ADAPTER_INDEX_MAP));
          end;
        end;
      end;
    finally
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
          A.IPAddress:=TStringList.Create;
          A.IPAddressMask:=TStringList.Create;
          A.Gateway_IPAddress:=TStringList.Create;
          A.Gateway_IPAddressMask:=TStringList.Create;
          A.DHCP_IPAddress:=TStringList.Create;
          A.DHCP_IPAddressMask:=TStringList.Create;
          A.PrimaryWINS_IPAddress:=TStringList.Create;
          A.PrimaryWINS_IPAddressMask:=TStringList.Create;
          A.SecondaryWINS_IPAddress:=TStringList.Create;
          A.SecondaryWINS_IPAddressMask:=TStringList.Create;
          A.DNSServers:=TStringList.Create;

          A.IPv6Address:=TStringList.Create;
          A.Gateway_IPv6:=TStringList.Create;
          A.DHCP_IPv6:=TStringList.Create;
          A.PrimaryWINS_IPv6:=TStringList.Create;
          A.DNSServers_IPv6:=TStringList.Create;

          A.AdapterName:=Trim(string(ai^.AdapterName));
          A.Name:=Trim(string(ai^.Description));
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
          A.Address:=s;
          A.EnableDHCP:=Boolean(ai^.DhcpEnabled);
          A.HaveWINS:=Boolean(ai^.HaveWins);

          ip:=@(ai^.IpAddressList);
          repeat
            lastip:=ip;
            if string(ip^.IpAddress.s)<>'' then begin
              A.IPAddress.Add(string(ip^.IpAddress.s));
              A.IPAddressMask.Add(string(ip^.IpMask.s));
            end;
            ip:=ip.Next;
          until not Assigned(ip) or (lastip=ip);

          ip:=@(ai^.GatewayList);
          repeat
            lastip:=ip;
            if string(ip^.IpAddress.s)<>'' then begin
              A.Gateway_IPAddress.Add(string(ip^.IpAddress.s));
              A.Gateway_IPAddressMask.Add(string(ip^.IpMask.s));
            end;
            ip:=ip.Next;
          until not Assigned(ip) or (lastip=ip);

          ip:=@(ai^.DhcpServer);
          repeat
            lastip:=ip;
            if string(ip^.IpAddress.s)<>'' then begin
              A.DHCP_IPAddress.Add(string(ip^.IpAddress.s));
              A.DHCP_IPAddressMask.Add(string(ip^.IpMask.s));
            end;
            ip:=ip.Next;
          until not Assigned(ip) or (lastip=ip);

          ip:=@(ai^.PrimaryWinsServer);
          repeat
            lastip:=ip;
            if string(ip^.IpAddress.s)<>'' then begin
              A.PrimaryWINS_IPAddress.Add(string(ip^.IpAddress.s));
              A.PrimaryWINS_IPAddressMask.Add(string(ip^.IpMask.s));
            end;
            ip:=ip.Next;
          until not Assigned(ip) or (lastip=ip);

          ip:=@(ai^.SecondaryWinsServer);
          repeat
            lastip:=ip;
            if string(ip^.IpAddress.s)<>'' then begin
              A.SecondaryWINS_IPAddress.Add(string(ip^.IpAddress.s));
              A.SecondaryWINS_IPAddressMask.Add(string(ip^.IpMask.s));
            end;
            ip:=ip.Next;
          until not Assigned(ip) or (lastip=ip);

          FillChar(Entry,SizeOf(Entry),0);
          Entry.dwIndex:=ai.Index;
          if GetIfEntry(Entry)=0 then begin
            A.AdapterName:={$IFNDEF UNICODE}WideToAnsi{$ENDIF}(Trim(WideString(Entry.wszName)));
            A.MaxSpeed:=Entry.dwSpeed;
            A.MTU:=Entry.dwMTU;
            A.AdminStatus:=Entry.dwAdminStatus;
            A.OperStatus:=Entry.dwOperStatus;
          end;

          idx:=FIL.IndexOfName(A.Name);
          if idx>-1 then
            A.IntfIdx:=StrToInt(FIL.ValueFromIndex[idx])
          else
            A.IntfIdx:=ai.Index;

          if Assigned(ConvertInterfaceIndexToLuid) and (ConvertInterfaceIndexToLuid(A.IntfIdx,@luid)=0) then begin
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
        r:=GetAdaptersAddresses(AF_INET,GAA_FLAG_INCLUDE_PREFIX or GAA_FLAG_INCLUDE_ALL_INTERFACES,nil,aaInitPtr,@size);
        while(r=ERROR_BUFFER_OVERFLOW) do begin
          size:=Size+SizeOf(IP_ADAPTER_ADDRESSES);
          ReallocMem(aaInitPtr,size);
          r:=GetAdaptersAddresses(AF_INET,GAA_FLAG_INCLUDE_PREFIX or GAA_FLAG_INCLUDE_ALL_INTERFACES,nil,aaInitPtr,@size);
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
                FAdapters[idx].DNSServers.Add(s);
                dns:=dns.Next;
              end;
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
                FAdapters[idx].DNSServers_IPv6.Add(s);
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
                FAdapters[idx].Gateway_IPv6.Add(s);
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
                FAdapters[idx].PrimaryWINS_IPv6.Add(s);
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
                FAdapters[idx].DHCP_IPv6.Add(s);
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
          FDNSSuf:=string(np^.CurrentDnsServer.IpAddress.S)
        else
          FDNSSuf:='';

        ip:=@(np^.DnsServerList);
        FDNSList.Clear;
        repeat
          s:=string(ip^.IpAddress.s);
          FDNSList.Add(s);
          ip:=ip.Next;
        until (not Assigned(ip)) or (s=string(ip^.IpAddress.s));

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

    size:=0;
    r:=GetIPAddrTable(nil,@size,False);
    if r=ERROR_NO_DATA then
      Exit;
    GetMem(pBuf,size);
    n:=0;
    try
      r:=GetIpAddrTable(PMIB_IPADDRTABLE(pBuf),@Size,False);
      pb:=pBuf;
      if r=NO_ERROR then begin
        n:=PMIB_IPADDRTABLE(pb)^.dwNumEntries;
        if n>0 then begin
          Inc(pb,SizeOf(Cardinal));
          SetLength(FAT,n);
          for j:=1 to n do begin
            IPAdrRow:=PMIB_IPADDRROW(pb)^;
            FAT[j-1].IP:=IPv4ToStr(InAddrToIPV4(TInAddr(IPAdrRow.dwAddr)));
            FAT[j-1].Mask:=IPv4ToStr(InAddrToIPV4(TInAddr(IPAdrRow.dwMask)));
            FAT[j-1].IntfIdx:=IPAdrRow.dwIndex;
            FAT[j-1].Typ:=IPAdrRow.wType;
            Inc(pb,SizeOf(IPAdrRow));
          end;
        end;
      end;
    finally
      try FreeMem(pBuf) except end;
    end;

    if Assigned(GetBestInterface) then begin
      r:=GetBestInterface(inet_addr(PAnsiChar('8.8.8.8')),n);
      if r=NO_ERROR then
        FBII:=n;
    end;

    FDNSSuffix.Clear;
    s:=ReadRegistryString(HKEY_LOCAL_MACHINE,rkTCPIP,rvSL);
    while Pos(',',s) <> 0 do begin
      FDNSSuffix.Add(Copy(s,1,Pos(',',s)-1));
      Delete(s,1,Pos(',',s));
    end;
    FDNSSuffix.Add(s);
  end;
  SetDataAvail(True);
end;

procedure TMiTeC_TCPIP.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

procedure WriteAdapterToStream(AIndex: Cardinal);
var
  strm: TStorageStream;
  sl: TStringList;
begin
    sl:=TStringList.Create;
    try
      WriteStrProperty(sl,'Name',Self.Adapter[AIndex].Name);
      WriteStrProperty(sl,'AdapterName',Self.Adapter[AIndex].AdapterName);
      WriteStrProperty(sl,'Alias',Self.Adapter[AIndex].Alias);
      WriteStrProperty(sl,'Address',Self.Adapter[AIndex].Address);
      WriteIntProperty(sl,'MaxSpeed',integer(Self.Adapter[AIndex].MaxSpeed));
      WriteIntProperty(sl,'MTU',integer(Self.Adapter[AIndex].MTU));
      WriteIntProperty(sl,'AdminStatus',integer(Self.Adapter[AIndex].AdminStatus));
      WriteIntProperty(sl,'OperStatus',integer(Self.Adapter[AIndex].OperStatus));
      WriteStrProperty(sl,'IPAddress',Self.Adapter[AIndex].IPAddress.CommaText);
      WriteStrProperty(sl,'IPAddressMask',Self.Adapter[AIndex].IPAddressMask.CommaText);
      WriteStrProperty(sl,'Gateway_IPAddress',Self.Adapter[AIndex].Gateway_IPAddress.CommaText);
      WriteStrProperty(sl,'Gateway_IPAddressMask',Self.Adapter[AIndex].Gateway_IPAddressMask.CommaText);
      WriteStrProperty(sl,'DHCP_IPAddress',Self.Adapter[AIndex].DHCP_IPAddress.CommaText);
      WriteStrProperty(sl,'DHCP_IPAddressMask',Self.Adapter[AIndex].DHCP_IPAddressMask.CommaText);
      WriteStrProperty(sl,'PrimaryWINS_IPAddress',Self.Adapter[AIndex].PrimaryWINS_IPAddress.CommaText);
      WriteStrProperty(sl,'PrimaryWINS_IPAddressMask',Self.Adapter[AIndex].PrimaryWINS_IPAddressMask.CommaText);
      WriteStrProperty(sl,'SecondaryWINS_IPAddress',Self.Adapter[AIndex].SecondaryWINS_IPAddress.CommaText);
      WriteStrProperty(sl,'SecondaryWINS_IPAddressMask',Self.Adapter[AIndex].SecondaryWINS_IPAddressMask.CommaText);
      WriteStrProperty(sl,'DNSServers',Self.Adapter[AIndex].DNSServers.CommaText);

      WriteStrProperty(sl,'IPv6Address',Self.Adapter[AIndex].IPv6Address.CommaText);
      WriteStrProperty(sl,'Gateway_IPv6',Self.Adapter[AIndex].Gateway_IPv6.CommaText);
      WriteStrProperty(sl,'DHCP_IPv6',Self.Adapter[AIndex].DHCP_IPv6.CommaText);
      WriteStrProperty(sl,'PrimaryWINS_IPv6',Self.Adapter[AIndex].PrimaryWINS_IPv6.CommaText);
      WriteStrProperty(sl,'DNSServers_IPv6',Self.Adapter[AIndex].DNSServers_IPv6.CommaText);

      WriteStrProperty(sl,'DNSSuffix',Self.Adapter[AIndex].DNSSuffix);
      WriteIntProperty(sl,'Typ',integer(Self.Adapter[AIndex].Typ));
      WriteIntProperty(sl,'EnableDHCP',integer(Self.Adapter[AIndex].EnableDHCP));
      WriteIntProperty(sl,'HaveWINS',integer(Self.Adapter[AIndex].HaveWINS));
      WriteIntProperty(sl,'DNSEnabled',integer(Self.Adapter[AIndex].DNSEnabled));
      WriteIntProperty(sl,'IPv4Enabled',integer(Self.Adapter[AIndex].IPv4Enabled));
      WriteIntProperty(sl,'IPv6Enabled',integer(Self.Adapter[AIndex].IPv6Enabled));
      WriteIntProperty(sl,'InterfaceIndex',integer(Self.Adapter[AIndex].IntfIdx));
      strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_OPEN,True);
      try
        SaveToEncodedStream(sl,strm,ACodeStream);
      finally
        strm.Free;
      end;
    finally
      sl.Free;
    end;
end;

procedure WriteAddressRecordToStream(AIndex: Cardinal);
var
  strm: TStorageStream;
  sl: TStringList;
begin
    sl:=TStringList.Create;
    try
      WriteStrProperty(sl,'IP',Self.FAT[AIndex].IP);
      WriteStrProperty(sl,'Mask',Self.FAT[AIndex].Mask);
      WriteIntProperty(sl,'InterfaceIndex',Self.FAT[AIndex].IntfIdx);
      WriteIntProperty(sl,'Typ',Self.FAT[AIndex].Typ);
      strm:=Sub.OpenStream(Format(strm_IP,[AIndex]),STG_OPEN,True);
      try
        SaveToEncodedStream(sl,strm,ACodeStream);
      finally
        strm.Free;
      end;
    finally
      sl.Free;
    end;
end;

var
  i: Integer;
  strm: TStorageStream;
  sl: TStringList;
begin
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(TCPIP_StorageFolderName);
    Sub:=SS.OpenSubStorage(TCPIP_StorageFolderName,STG_OPEN,True);
    try
      sl:=TStringList.Create;
      try
        WriteStrProperty(sl,'HostName',Self.HostName);
        WriteStrProperty(sl,'DomainName',Self.DomainName);
        WriteStrProperty(sl,'PrimaryDNSSuffix',Self.PrimaryDNSSuffix);
        WriteStrProperty(sl,'DHCPScopeName',Self.DHCPScopeName);
        WriteStrProperty(sl,'DNSServers',Self.DNSServers.CommaText);
        WriteStrProperty(sl,'DNSSuffixes',Self.DNSSuffixes.CommaText);
        WriteIntProperty(sl,'NodeType',integer(Self.NodeType));
        WriteIntProperty(sl,'EnableProxy',integer(Self.EnableProxy));
        WriteIntProperty(sl,'EnableRouting',integer(Self.EnableRouting));
        WriteIntProperty(sl,'EnableDNS',integer(Self.EnableDNS));
        WriteIntProperty(sl,'BestInterfaceIndex',Self.FBII);

        strm:=Sub.OpenStream(strm_Props,STG_OPEN,True);
        try
          SaveToEncodedStream(sl,strm,ACodeStream);
        finally
          strm.Free;
        end;
      finally
        sl.Free;
      end;

      for i:=0 to Self.AdapterCount-1 do
        WriteAdapterToStream(i);

      for i:=0 to Self.AddressRecordCount-1 do
        WriteAddressRecordToStream(i);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

function TMiTeC_TCPIP.FindAddress(AIP: string): Integer;
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

function TMiTeC_TCPIP.FindAddress(AIntfIdx: Cardinal): Integer;
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

{ TMiTeC_NetResources }

procedure TMiTeC_NetResources.Clear;
begin
  Finalize(FShareList);
  Finalize(FConnList);
  Finalize(FSessionList);
  Finalize(FFileList);
end;

constructor TMiTeC_NetResources.Create(AOwner: TComponent);
begin
  inherited;
  FConns:=TConnections.Create;
  FShares:=TNTShares.Create;
end;

destructor TMiTeC_NetResources.Destroy;
begin
  Finalize(FShareList);
  Finalize(FConnList);
  Finalize(FSessionList);
  Finalize(FFileList);
  FConns.Free;
  FShares.Free;
  inherited;
end;

function TMiTeC_NetResources.GetConn(Index: Cardinal): TConnectionRecord;
begin
  try Result:=FConnList[Index] except end;
end;

function TMiTeC_NetResources.GetConnCount: Cardinal;
begin
  Result:=Length(FConnList);
end;

function TMiTeC_NetResources.GetOpenFile(Index: Cardinal): TOpenFileRecord;
begin
  try Result:=FFileList[Index] except end;
end;

function TMiTeC_NetResources.GetOpenFileCount: Cardinal;
begin
  Result:=Length(FFileList);
end;

function TMiTeC_NetResources.GetSession(Index: Cardinal): TNTSessionRecord;
begin
  try Result:=FSessionList[Index] except end;
end;

function TMiTeC_NetResources.GetSessionCount: Cardinal;
begin
  Result:=Length(FSessionList);
end;

function TMiTeC_NetResources.GetShare(Index: Cardinal): TNTShareRecord;
begin
  try Result:=FShareList[Index] except end;
end;

function TMiTeC_NetResources.GetShareCount: Cardinal;
begin
  Result:=Length(FShareList);
end;

function TMiTeC_NetResources.LoadFromStorage;
var
  stg: IStorage;
  SS, Sub: TStructuredStorage;

function ReadSharesFromStream(AIndex: Cardinal): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
  r: TNTShareRecord;
begin
  Result:=False;
      try strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
      if strm<>nil then
        try
          sl:=TStringList.Create;
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            r.Name:=ReadStrProperty(sl,'Name');
            r.Comment:=ReadStrProperty(sl,'Comment');
            r.ShareType:=TShareType(ReadIntProperty(sl,'Type'));
            r.Permissions:=ReadIntProperty(sl,'Permissions');
            r.MaxUserCount:=ReadIntProperty(sl,'MaxUserCount');
            r.CurUserCount:=ReadIntProperty(sl,'CurUserCount');
            r.SecurityDesc:=ReadIntProperty(sl,'SecurityDesc')=1;
            r.Path:=ReadStrProperty(sl,'Path');
            r.Password:=ReadStrProperty(sl,'Password');
            SetLength(FShareList,Length(FShareList)+1);
            FShareList[High(FShareList)]:=r;
            Result:=True;
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;
end;

function ReadSessionsFromStream(AIndex: Cardinal): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
  r: TNTSessionRecord;
begin
  Result:=False;
      try strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
      if strm<>nil then
        try
          sl:=TStringList.Create;
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            r.Name:=ReadStrProperty(sl,'Name');
            r.UserName:=ReadStrProperty(sl,'Username');
            r.SesiType:=ReadStrProperty(sl,'Type');
            r.Transport:=ReadStrProperty(sl,'Transport');
            r.ConnectedTime:=ReadIntProperty(sl,'ConnTime');
            r.IdleTime:=ReadIntProperty(sl,'IdleTime');
            r.OpenFiles:=ReadIntProperty(sl,'Files');
            r.Guest:=ReadIntProperty(sl,'Guest')=1;
            SetLength(FSessionList,Length(FSessionList)+1);
            FSessionList[High(FSessionList)]:=r;
            Result:=True;
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;
end;

function ReadFilesFromStream(AIndex: Cardinal): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
  r: TOpenFileRecord;
begin
  Result:=False;
      try strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
      if strm<>nil then
        try
          sl:=TStringList.Create;
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            r.Name:=ReadStrProperty(sl,'Name');
            r.UserName:=ReadStrProperty(sl,'Username');
            r.Sharename:=ReadStrProperty(sl,'Sharename');
            r.Locks:=ReadIntProperty(sl,'Locks');
            r.Mode:=ReadIntProperty(sl,'Mode');
            r.ID:=ReadIntProperty(sl,'ID');
            SetLength(FFileList,Length(FFileList)+1);
            FFileList[High(FFileList)]:=r;
            Result:=True;
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;
end;

function ReadConnsFromStream(AIndex: Cardinal): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
  r: TConnectionRecord;
begin
  Result:=False;
      try strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
      if strm<>nil then
        try
          sl:=TStringList.Create;
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            r.Name:=ReadStrProperty(sl,'Name');
            r.UserName:=ReadStrProperty(sl,'Username');
            r.ID:=ReadIntProperty(sl,'ID');
            r.ConnType:=TShareType(ReadIntProperty(sl,'Type'));
            r.Time:=ReadIntProperty(sl,'Time');
            r.OpenFiles:=ReadIntProperty(sl,'Files');
            r.Users:=ReadIntProperty(sl,'Users');
            SetLength(FConnList,Length(FConnList)+1);
            FConnList[High(FConnList)]:=r;
            Result:=True;
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;
end;

var
  i: Integer;
begin
  Clear;
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    try
      Sub:=SS.OpenSubStorage(Shares_StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;
    if Sub<>nil then
      try
        i:=0;
        while ReadSharesFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        Sub.Free;
      end;

    try
      Sub:=SS.OpenSubStorage(Sessions_StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;
    if Sub<>nil then
      try
        i:=0;
        while ReadSessionsFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        Sub.Free;
      end;

    try
      Sub:=SS.OpenSubStorage(OpenFiles_StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;
    if Sub<>nil then
      try
        i:=0;
        while ReadFilesFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        Sub.Free;
      end;

    try
      Sub:=SS.OpenSubStorage(Connections_StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;
    if Sub<>nil then
      try
        i:=0;
        while ReadConnsFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        Sub.Free;
      end;


  finally
    SS.Free;
  end;
end;

procedure TMiTeC_NetResources.RefreshData;
var
  i: Integer;
begin
  inherited;
  Clear;

  try
    FShares.RefreshShares;
    SetLength(FShareList,FShares.ShareCount);
    for i:=0 to FShares.ShareCount-1 do
      FShareList[i]:=FShares.Shares[i]^;

    FShares.RefreshSessions;
    SetLength(FSessionList,FShares.SessionCount);
    for i:=0 to FShares.SessionCount-1 do
      FSessionList[i]:=FShares.Sessions[i]^;
    FShares.RefreshOpenFiles;
    SetLength(FFileList,FShares.OpenFileCount);
    for i:=0 to FShares.OpenFileCount-1 do
      FFileList[i]:=FShares.OpenFiles[i]^;
    {$IFDEF WIN32}
    for i:=0 to FShares.ShareCount-1 do begin
      FConns.Qualifier:=FShares.Shares[i].Name;
      FConns.Refresh(i=0);
    end;
    {$ENDIF}
    SetLength(FConnList,FConns.ConnectionCount);
    for i:=0 to FConns.ConnectionCount-1 do
      FConnList[i]:=FConns.Connections[i]^;
  except

  end;
end;

procedure TMiTeC_NetResources.SaveConnsToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

procedure WriteToStream(AIndex: Cardinal);
var
  strm: TStorageStream;
  sl: TStringList;
begin
    sl:=TStringList.Create;
    try
      WriteStrProperty(sl,'Name',Self.FConnList[AIndex].Name);
      WriteStrProperty(sl,'Username',Self.FConnList[AIndex].UserName);
      WriteIntProperty(sl,'ID',integer(Self.FConnList[AIndex].ID));
      WriteIntProperty(sl,'Type',integer(Self.FConnList[AIndex].ConnType));
      WriteIntProperty(sl,'Time',integer(Self.FConnList[AIndex].Time));
      WriteIntProperty(sl,'Files',integer(Self.FConnList[AIndex].OpenFiles));
      WriteIntProperty(sl,'Users',integer(Self.FConnList[AIndex].Users));
      strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_OPEN,True);
      try
        SaveToEncodedStream(sl,strm,ACodeStream);
      finally
        strm.Free;
      end;
    finally
      sl.Free;
    end;
end;

var
  i: Integer;
begin
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(Connections_StorageFolderName);
    Sub:=SS.OpenSubStorage(Connections_StorageFolderName,STG_OPEN,True);
    try
      for i:=0 to High(FConnList) do
        WriteToStream(i);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_NetResources.SaveFilesToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

procedure WriteToStream(AName: string; AIndex: Cardinal);
var
  strm: TStorageStream;
  sl: TStringList;
begin
    sl:=TStringList.Create;
    try
      WriteStrProperty(sl,'Name',Self.FFileList[AIndex].Name);
      WriteStrProperty(sl,'Username',Self.FFileList[AIndex].UserName);
      WriteStrProperty(sl,'Sharename',Self.FFileList[AIndex].Sharename);
      WriteIntProperty(sl,'Locks',integer(Self.FFileList[AIndex].Locks));
      WriteIntProperty(sl,'Mode',integer(Self.FFileList[AIndex].Mode));
      WriteIntProperty(sl,'ID',integer(Self.FFileList[AIndex].ID));
      strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_OPEN,True);
      try
        SaveToEncodedStream(sl,strm,ACodeStream);
      finally
        strm.Free;
      end;
    finally
      sl.Free;
    end;
end;

var
  i: Integer;
begin
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(OpenFiles_StorageFolderName);
    Sub:=SS.OpenSubStorage(OpenFiles_StorageFolderName,STG_OPEN,True);
    try
      for i:=0 to High(FFileList) do
        WriteToStream(IntToStr(i),i);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_NetResources.SaveSessionsToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

procedure WriteToStream(AIndex: Cardinal);
var
  strm: TStorageStream;
  sl: TStringList;
begin
    sl:=TStringList.Create;
    try
      WriteStrProperty(sl,'Name',Self.FSessionList[AIndex].Name);
      WriteStrProperty(sl,'Username',Self.FSessionList[AIndex].UserName);
      WriteStrProperty(sl,'Type',Self.FSessionList[AIndex].SesiType);
      WriteStrProperty(sl,'Transport',Self.FSessionList[AIndex].Transport);
      WriteIntProperty(sl,'Files',integer(Self.FSessionList[AIndex].OpenFiles));
      WriteIntProperty(sl,'ConnTime',integer(Self.FSessionList[AIndex].ConnectedTime));
      WriteIntProperty(sl,'IdleTime',integer(Self.FSessionList[AIndex].IdleTime));
      WriteIntProperty(sl,'Guest',integer(Self.FSessionList[AIndex].Guest));
      strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_OPEN,True);
      try
        SaveToEncodedStream(sl,strm,ACodeStream);
      finally
        strm.Free;
      end;
    finally
      sl.Free;
    end;
end;

var
  i: Integer;
begin
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(Sessions_StorageFolderName);
    Sub:=SS.OpenSubStorage(Sessions_StorageFolderName,STG_OPEN,True);
    try
      for i:=0 to High(FSessionList) do
        WriteToStream(i);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_NetResources.SaveSharesToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

procedure WriteToStream(AIndex: Cardinal);
var
  strm: TStorageStream;
  sl: TStringList;
begin
    sl:=TStringList.Create;
    try
      WriteStrProperty(sl,'Name',Self.FShareList[AIndex].Name);
      WriteStrProperty(sl,'Comment',Self.FShareList[AIndex].Comment);
      WriteStrProperty(sl,'Path',Self.FShareList[AIndex].Path);
      WriteStrProperty(sl,'Password',Self.FShareList[AIndex].Password);
      WriteIntProperty(sl,'Type',integer(Self.FShareList[AIndex].ShareType));
      WriteIntProperty(sl,'Permissions',integer(Self.FShareList[AIndex].Permissions));
      WriteIntProperty(sl,'MaxUserCount',integer(Self.FShareList[AIndex].MaxUserCount));
      WriteIntProperty(sl,'CurUserCount',integer(Self.FShareList[AIndex].CurUserCount));
      WriteIntProperty(sl,'SecurityDesc',integer(Self.FShareList[AIndex].SecurityDesc));
      strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_OPEN,True);
      try
        SaveToEncodedStream(sl,strm,ACodeStream);
      finally
        strm.Free;
      end;
    finally
      sl.Free;
    end;
end;

var
  i: Integer;
begin
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(Shares_StorageFolderName);
    Sub:=SS.OpenSubStorage(Shares_StorageFolderName,STG_OPEN,True);
    try
      for i:=0 to High(FShareList) do
        WriteToStream(i);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_NetResources.SaveToStorage;
begin
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  SaveConnsToStorage(AFilename,ACodeStream);
  SaveSharesToStorage(AFilename,ACodeStream);
  SaveSessionsToStorage(AFilename,ACodeSTream);
  SaveFilesToStorage(AFilename,ACodeStream);
end;

end.
