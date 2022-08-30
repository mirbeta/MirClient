{$INCLUDE Compilers.inc}

{*******************************************************}
{               MiTeC Common Routines                   }
{                  Network routines                     }
{                                                       }
{          Copyright (c) 1997-2019 Michal Mutl          }
{                                                       }
{*******************************************************}

unit MiTeC_NetUtils;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.WinSvc, WinAPI.WinSock,
     {$ELSE}
     Windows, SysUtils, Classes, {$IFDEF FPC}JwaWinSvc{$ELSE}WinSvc{$ENDIF}, WinSock,
     {$ENDIF}
     MiTeC_NetBIOS, MiTeC_IpHlpAPI, MiTeC_NetAPI32, MiTeC_IcmpApi, MiTeC_IPTypes,
     MiTeC_AdvApi, MiTeC_Lists;

type
  TSharedResource = record
    Name,
    Comment: string;
    Typ: Cardinal;
  end;

  TSharedResources = array of TSharedResource;

  TOpenFile = record
    Name,
    UserName,
    Sharename: string;
    Locks: Cardinal;
    Mode: Cardinal;
    ID: Cardinal;
  end;

  TOpenFiles = array of TOpenFile;

  TSessionRecord = record
    Name: string;
    UserName: string;
    Typ: string;
    OpenFiles: Cardinal;
    ConnectedTime: Cardinal;
    IdleTime: Cardinal;
    Guest: Boolean;
    Transport: string;
  end;

  TSessions = array of TSessionRecord;

  TServiceRecord = record
    Name: string;
    Startup,
    DisplayName,
    CmdLine: string;
    Status: Cardinal;
    Typ: TServiceType;
  end;

  TServices = array of TServiceRecord;

  TNetObject = record
    Timestamp: TDateTime;
    IPv4: TIPv4;
    IPv6: TIPv6;
    Name,
    MachineName,
    Domain,
    User,
    OS,
    IPv4Address,
    IPv6Address,
    MACAddress,
    MACVendor,
    Desc,
    Model,
    BIOS,
    CPU,
    UUID,
    IDNumber: string;
    SNMP,
    WMI,
    ALive: Boolean;
    PingTime: integer;
    ARPTime: int64;
    NBTime: int64;
    PSTime: int64;
    ElapsedTime: int64;
    RemoteTime: TDatetime;
    SystemUpTime: Int64;
    Modified: Boolean;
    Flag: Boolean;
    Flags: Cardinal;
    CPUCount: Byte;
    CPUFreq: Cardinal;
    Memory: Int64;
    InstallDate: TDatetime;
    TCPPorts,
    Contact,
    Location,
    Vendor: string;
    procedure Clear;
    function UpdateFrom(ARecord: TNetObject): Boolean;
  end;

  TIPInfo = record
    HostName: string;
    Name: string;
    IP4: string;
    IP6: string;
  end;
  TIPData = array of TIPInfo;

function AccountExists(AMachine,AName: string): Integer;
procedure DeleteAccount(AName: string);
function CreateAccount(AName,APwd: string; AHidden: Boolean = False): Integer;
function GetHostID(Data: TNetObject; NameFirst: Boolean = False): string;
function GetObjectHostName(Data: TNetObject): string;
function GetFirstIP(IP: string; const IPMask: string = ''): string;
function MAC2Str(Addr: TPHYS_ADDRESS; Size: integer): string;
function GetIP(const AName: string): string;
procedure GetIpInfo(const AHostName: string; var AData: TIPData; AAddrFamily: integer = AF_UNSPEC);
function GetMAC_NetBIOS(AName: string): string;
function GetMAC_ARP(const AIP: TIPv4; var AElapsedTime: int64): String;
function GetMAC_ARPTable(AIP: string; ARefreshARPTable: Boolean = False): string;
function GetMAC_AddrTable(AIP: string; ARefreshTable: Boolean = False): string;
function GetHostByAddr(IP: string): string; overload;
function GetHostByAddr(IP: TIPv4): string; overload;
function GetNameByAddr(IP: string): string;
procedure NBLookup(AHost: string; var AName,ADomain,AUser,AMAC: string);
procedure GetARPTable(List: TThreadStringList; AClear: Boolean = True);
procedure GetAddrTable(List: TThreadStringList; AClear: Boolean = True);
function UseIPC(AHost,ADomain,AUser,APwd: string): Cardinal;
function DelIPC(AHost: string): Cardinal;
function ShareExists(AHost,AShare: string): Boolean;
function ShareAccess(AHost,ADomain,AShare,AUser,APwd: string): Boolean;
function ShareAccessEx(APath: string): Boolean;
function GetSharePath(AHost,AShare: string): string;
function AddShare(const AShare,ADomain,AUser,APwd: string): Boolean;
function TimeDetect(AHost: string; var AUpTime: int64): TDateTime;
function UserDetect(AHost: string): string;
function ShareDetect(AName: string; var AList: TSharedResources): Cardinal;
function OpenFilesDetect(AName: string; var AList: TOpenFiles): Cardinal;
function SessionDetect(AName: string; var AList: TSessions): Cardinal;
function ServiceDetect(AName: string; var AList: TServices): Cardinal;
procedure GetWkstaInfo(AHost: string; ALevel: Cardinal; var Name,Domain: string; var OSMajor,OSMinor,OSPlatform: Cardinal);
procedure GetWkstaUsers(AHost: string; var Users: string);
procedure GetServerInfo(AHost: string; var Comment: string; var OSMajor,OSMinor,OSPlatform,Flags: Cardinal);
procedure DetectNetObject(const AName: string; var Data: TNetObject;
                          ABasic: Boolean = True;
                          ANetBIOS: Boolean = False;
                          AARP: Boolean = False;
                          ARegistry: Boolean = False);
function CopyData(Source,Dest: TNetObject; AForceSource: Boolean = False): TNetObject;
function CopyDataNew(Source,Dest: TNetObject): TNetObject;
function GetOSNameFromVer(Platf,Major,Minor,Flags: Cardinal): string;
function GetOSFromRegistry(AHost: string): string;
function GetMachineNameFromRegistry(AHost: string): string;
function ConnectIPC(AHost,AUser,APwd: string): integer;
procedure DisconnectIPC(AHost: string);
function GetDataFromRegistry(AHost: string; var AOS,AMachine,ACPU,ABIOS,AModel,AVendor: string; var AID: TDateTime): Boolean;
function FormatCPUName(const AName: string): string;
function FormatOSName(const AName: string): string;
procedure GetDomainServerList(const ADomain: string; AList: TStringList);

function Ping(const AIP: string; ATTL,ARetries: byte; ATimeout: Cardinal): integer; overload;
function Ping(const AIP: TIPv4; ATTL,ARetries: byte; ATimeout: Cardinal): integer; overload;

var
  ARPTable, AddrTable: TThreadStringList;
  RegConOK: Boolean;


implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry, System.AnsiStrings, System.DateUtils,
     {$ELSE}
     Registry, DateUtils,
     {$ENDIF}
     MiTeC_NTSecAPI, MiTeC_Datetime, MiTeC_StrUtils, MiTeC_Ws2_32;

function StripSpaces(ASource: string): string;
var
  l,c,i: Integer;
begin
  c:=0;
  l:=Length(ASource);
  Result:='';
  for i:=1 to l do
    if ASource[i]=' ' then begin
      Inc(c);
      if c<2 then
        Result:=Result+ASource[i];
    end else begin
      Result:=Result+ASource[i];
      c:=0;
    end;
end;

function AccountExists;
var
  ui: Pointer;
  WUser,WMachine: WideString;
begin
  WUser:=AName;
  WMachine:=AMachine;
  Result:=NetUserGetInfo(PWideChar(WMachine),PWideChar(WUser),0,ui);
  if Assigned(ui) then
    NetAPIBufferFree(ui);
end;

function CreateAccount(AName,APwd: string; AHidden: Boolean = False): Integer;
var
 WUser, WPwd: WideString;
 usri1: USER_INFO_1;
 lgmi3: LOCALGROUP_MEMBERS_INFO_3;
 parm_err,ec: DWORD;
const
  rk = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon\SpecialAccounts\UserList\';
begin
  Result:=0;
  {if not InitNetAPI then begin
    Result:=GetLastError;
    Exit;
  end;}
  try
    WUser:=Aname;
    WPwd:=APwd;
    usri1.usri1_name:=PWideChar(WUser);
    usri1.usri1_password:=PWideChar(WPwd);
    usri1.usri1_password_age:=0;
    usri1.usri1_priv:=1;
    usri1.usri1_home_dir:='';
    usri1.usri1_comment:='';
    usri1.usri1_flags:=$10201;
    usri1.usri1_script_path:='';
    ec:=NetUserAdd(nil,1,@usri1,parm_err);
    if (ec<>NERR_UserExists) and (ec<>NERR_Success) then
      Exit;
    lgmi3.lgrmi3_domainandname:=PWideChar(WUser);
    ec:=NetLocalGroupAddMembers(nil,'Administrators',3,lgmi3,1);
    if ec<>NERR_SUCCESS then begin
      Result:=GetLastError;
      Exit;
    end;

    if AHidden then
      with TRegistry.Create do
        try
          Rootkey:=HKEY_LOCAL_MACHINE;
          if Openkey(rk,True) then begin
            WriteInteger(AName,0);
            CloseKey;
          end;
        finally
          Free;
        end;

  finally

  end;
end;

procedure DeleteAccount(AName: string);
var
  i: Integer;
  WName: WideString;
const
  rk = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon\SpecialAccounts\UserList\';
begin
  i:=Pos('\',AName);
  if i>0 then
    AName:=Copy(AName,i+1,255);

  with TRegistry.Create do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if Openkey(rk,false) then begin
        DeleteValue(AName);
        CloseKey;
      end;
    finally
      Free;
    end;

  WName:=AName;

  NetUserDel(nil,PWideChar(WName));
end;

function GetHostID;
begin
  Result:='';
  if NameFirst then begin
    if Data.MachineName<>'' then
      Result:=Data.MachineName
    else
      Result:=GetFirstIP(data.IPv4Address);
  end else
    if (Data.IPv4Address<>'') {and (Pos(',',data.IPv4Address)=0)} then
      Result:=GetFirstIP(data.IPv4Address)
    else
      Result:=Data.MachineName;
end;

function GetObjectHostName(Data: TNetObject): string;
begin
  Result:='';
  if Data.Name<>'' then
    Result:=Data.Name
  else
    Result:=GetFirstIP(data.IPv4Address);
end;

function GetHostByAddr(IP: TIPv4): string;
var
  WSData: TWSAData;
  SockAddrIn: TSockAddrIn;
  HostEnt: PHostEnt;
begin
  Result:='';
  if WSAStartup($101, WSData) = 0 then begin
    SockAddrIn.sin_addr.S_addr:=IPv4ToInAddr(IP).S_addr;

    HostEnt:={$IFDEF RAD9PLUS}winAPI.WinSock{$ELSE}WinSock{$ENDIF}.GetHostByAddr(@SockAddrIn.sin_addr.S_addr,
               SizeOf(SockAddrIn.sin_addr.S_addr), AF_INET);
    if HostEnt <> nil then
      Result:=string(Hostent^.h_name);
    WSACleanup;
  end;
end;

function GetHostByAddr(IP: string): string;
begin
  Result:=GetHostByAddr(StrToIPv4(IP));
end;

function GetNameByAddr(IP: string): string;
var
  WSData: TWSAData;
  SockAddrIn: TSockAddrIn;
  s: string;
  r: Integer;
begin
  Result:='';
  if not Assigned(GetNameInfo) then
    Exit;
  if WSAStartup(MAKEWORD(2,2),WSData)<>0 then
    Exit;
  try
    SockAddrIn.sin_family:=AF_INET;
    SockAddrIn.sin_addr.s_addr:=inet_addr(PAnsiChar({$IFDEF UNICODE}WideToAnsi{$ENDIF}(IP)));
    r:=GetNameInfo(@SockAddrIn,SizeOf(SockAddrIn),PChar(s),NI_MAXHOST,nil,0,0);
    if r=0 then
      Result:=s;
  finally
    WSACleanup;
  end;
end;

procedure NBLookup(AHost: string; var AName,ADomain,AUser,AMAC: string);
var
  AdapterStatus: TAdapterFullStatus;
  i: Integer;
  l1,l2,l3: Boolean;
  s: string;
begin
  AName:='';
  ADomain:='';
  AUser:='';
  AMAC:='';
  l1:=False;
  l2:=False;
  l3:=False;
  AdapterStatus:=NB_GetAdapterStatus(AHost);
  if AdapterStatus.Adapter.rev_major>0 then begin
    AMAC:=AdapterToString(AdapterStatus.Adapter.adapter_address);
    for i:=0 to AdapterStatus.Adapter.name_count-1 do begin
      s:=Trim(GetStrFromBuf(AdapterStatus.Names[i].Name,SizeOf(TNBName)));
      if not l1 and (AdapterStatus.Names[i].NameSt and GROUP_NAME=GROUP_NAME) then begin
        ADomain:=s;
        l1:=True;
      end;
      if not l2 and (AdapterStatus.Names[i].NameSt=4) then begin
        AName:=s;
        l2:=True;
      end;
      if not l3 and (AdapterStatus.Names[i].NameSt and DUPLICATE=0) and not SameText(AName,s) and not SameText(ADomain,s) then begin
        AUser:=s;
        l3:=True;
      end;
    end;
  end;
end;

function GetFirstIP(IP: string; const IPMask: string = ''): string;
var
  i: Integer;
  sl: TStringList;
begin
  Result:='';
  sl:=TStringList.Create;
  try
    sl.CommaText:=IP;
    if sl.Count=0 then
      Exit;
    if IPMask='' then
      Result:=sl[0]
    else
      for i:=0 to sl.Count-1 do
        if Pos(IPMask,sl[i])=1 then begin
          Result:=sl[i];
          Break;
        end;
  finally
    sl.Free;
  end;
end;

function MAC2Str(Addr: TPHYS_ADDRESS; Size: integer): string;
var
  i: integer;
begin
  Result:='';
  if Size=0 then
    Exit;
  for i:=1 to Size do
    Result:=Result+IntToHex(Addr[i],2)+'-';
  SetLength(Result,Length(Result)-1);
  if SameText(Result,'00-00-00-00-00-00') then
    Result:='';
end;

function GetIP(const AName: string): string;
type
  TaPInAddr = array [0..255] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe  :PHostEnt;
  pptr :PaPInAddr;
  Buffer :ansistring;//array [0..63] of ansichar;
  i :integer;
  GInitData :TWSADATA;
begin
  result:='';
  if Trim(AName)='' then
    Exit;
  wsastartup($101,GInitData);
  try
    Buffer:={$IFDEF UNICODE}WideToAnsi{$ENDIF}(AName);
    phe:=GetHostByName(PAnsiChar(buffer));
    if not assigned(phe) then
      exit;
    pptr:=PaPInAddr(Phe^.h_addr_list);
    i:=0;
    while pptr^[I]<>nil do begin
      result:=Result+IPv4ToStr(InAddrToIPv4(pptr^[I]^))+',';
      inc(i);
    end;
    Delete(Result,Length(Result),1);
  finally
    wsacleanup;
  end;
end;

procedure GetIpInfo(const AHostName: string; var AData: TIPData; AAddrFamily: integer = AF_UNSPEC);
var
  nameRet: PAnsiChar;
  ptr: Pointer;
  Hints: TAddrInfo;
  AddrInfo: PAddrInfo;
  NextInfo: PAddrInfo;
  RetVal: Integer;
  ip,Name: string;
  namelen: Cardinal;
  r: TIPInfo;
  WSData: TWSAData;
  i,j,idx: Integer;
  ipdata: TIPData;
begin
  Finalize(AData);
  if not Assigned(GetAddrInfo) then
    Exit;

  if WSAStartup(MAKEWORD(2,2),WSData)<>0 then
    Exit;

  try
  FillChar(Hints, SizeOf(Hints), 0);
  Hints.ai_family:=AAddrFamily;
  Hints.ai_socktype:=SOCK_STREAM;
  Hints.ai_protocol:=IPPROTO_TCP;
  Hints.ai_flags:=0;

  AddrInfo:=nil;
  RetVal:=GetAddrInfo(PChar(AHostName),nil,@Hints,AddrInfo);
  if RetVal=0 then
    try
      NextInfo:=AddrInfo;
      while NextInfo <> nil do begin
        ZeroMemory(@r,SizeOf(r));
        r.HostName:=AHostName;
        if (NextInfo.ai_family = AF_INET) or (NextInfo.ai_family = AF_INET6) then begin
          if (r.Name='') then begin
            SetLength(Name,NI_MAXHOST);
            RetVal:=GetNameInfo(NextInfo.ai_addr,NextInfo.ai_addrlen,PChar(Name),NI_MAXHOST,nil,0,0);
            if (RetVal<>0) then begin
              NextInfo:=NextInfo.ai_next;
              continue;
            end;
            r.Name:=PChar(Name);
          end;
          NameRet:=nil;
          case NextInfo.ai_family of
            AF_INET: begin
              namelen:=INET_ADDRSTRLEN;
              SetLength(ip,namelen);
              ip:=IPv4ToStr(InAddrToIPv4(NextInfo.ai_addr.sin_addr));
              nameRet:=PAnsiChar(ip);
            end;
            AF_INET6: if Assigned(inetntop) then begin
              ptr:=LPSOCKADDR(NextInfo.ai_addr);
              namelen:=INET6_ADDRSTRLEN;
              SetLength(ip,namelen);
              WSAAddressToString(LPSOCKADDR(ptr),NextInfo.ai_addrlen,nil,PChar(ip),namelen);
              ip:=PChar(ip);
              if ip<>'' then
                NameRet:=PAnsiChar(ip);
            end;
          end;

          if (nameRet=nil) then begin
            NextInfo:=NextInfo.ai_next;
            continue;
          end;

          case NextInfo.ai_family of
            AF_INET6: r.IP6:=ip;
            AF_INET: r.IP4:=ip;
          end;

          SetLength(ipdata,Length(ipdata)+1);
          ipdata[High(ipdata)]:=r;
        end;
        NextInfo:=NextInfo.ai_next;
      end;
      for i:=0 to High(ipdata) do
        if ipdata[i].IP4<>'' then begin
          SetLength(AData,Length(AData)+1);
          AData[High(AData)]:=ipdata[i];
        end;

      for i:=0 to High(ipdata) do
        if ipdata[i].IP6<>'' then begin
          idx:=-1;
          for j:=0 to High(AData) do begin
            if SameText(ipdata[i].Name,AData[j].Name) then begin
              if (idx=-1) then begin
                idx:=j;
                if (AData[j].IP6='') then
                  Break;
              end;
              if (idx<>j) then
                idx:=j;
            end;
          end;
          if idx>-1 then
            AData[idx].IP6:=AData[idx].IP6+ipdata[i].IP6+',';
        end;

      for i:=0 to High(AData) do
        SetLength(AData[i].IP6,Length(AData[i].IP6)-1);
    finally
      FreeAddrInfo(AddrInfo);
    end;
    finally
      WSACleanup;
    end;
end;

function GetMAC_NetBIOS(AName: string): string;
var
  sl: TStringList;
begin
  Result:='';
  sl:=TStringList.Create;
  try
    NB_GetMACAddresses(AName,sl);
    Result:=sl.CommaText;
  finally
    sl.Free;
  end;
end;

function GetMAC_ARP(const AIP: TIPv4; var AElapsedTime: int64): string;
const
  HexChars: array[0..15] of AnsiChar = '0123456789ABCDEF';
  Sep: AnsiChar = '-';
var
  dwRemoteIP: DWORD;
  PhyAddrLen: Longword;
  pMacAddr: array [0..7] of byte;
  I: integer;
  P: PAnsiChar;
  a: AnsiString;
  et: Int64;
begin
  a:='';
  AElapsedTime:=-1;
  if not Assigned(SendARP) then
    Exit;
  dwremoteIP:=IPv4ToInAddr(AIP).S_addr;
  if dwremoteIP<>0 then begin
    PhyAddrLen:=8;
    et:=GetTickCount;
    if SendARP(dwremoteIP,dwremoteIP,@pMacAddr,@PhyAddrLen)=NO_ERROR then begin
      if PhyAddrLen=6 then begin
        AElapsedTime:=GetTickCount-et;
        SetLength(a,17);
        P:=pointer(a);
        for i:=0 to 5 do begin
          P[0]:=HexChars[pMacAddr[i] shr 4];
          P[1]:=HexChars[pMacAddr[i] and $F];
          P[2]:=Sep;
          inc(P,3);
        end;
      end;
    end;
  end;
  Result:=string(a);
end;


procedure GetARPTable(List: TThreadStringList; AClear: Boolean = True);
var
  IPNetRow: TMIB_IPNETROW;
  TableSize, NumEntries, ec: Cardinal;
  i: integer;
  pBuf: PAnsiChar;
  sl: TStringList;
begin
  if not Assigned(GetIPNetTable) then
    Exit;
  if AClear then
    List.Clear;
  TableSize:=0;
  ec:=GetIPNetTable(nil,@TableSize,False);
  if ec=ERROR_NO_DATA then
    Exit;
  GetMem(pBuf,TableSize);
  NumEntries:=0;
  sl:=TStringList.Create;
  try
    ec:=GetIpNetTable(PMIB_IPNETTABLE(pBuf),@TableSize,False);
    if ec=NO_ERROR then begin
      NumEntries:=PMIB_IPNETTABLE(pBuf)^.dwNumEntries;
      if NumEntries>0 then begin
        Inc(pBuf,SizeOf(Cardinal));
        for i:=1 to NumEntries do begin
          IPNetRow:=PMIB_IPNETROW(PBuf)^;
          with IPNetRow do
            if dwType<>2 then
              sl.Add(Format('%s=%s',[IPv4ToStr(InAddrToIPv4(TInAddr(dwAddr))),MAC2Str(bPhysAddr,dwPhysAddrLen)]));
          Inc(pBuf,SizeOf(IPNetRow));
        end;
      end;
    end;
    List.AddStrings(sl);
  finally
    sl.Free;
    Dec(pBuf,SizeOf(Cardinal)+NumEntries*SizeOf(IPNetRow));
    try FreeMem(pBuf) except end;
  end;
end;

procedure GetAddrTable(List: TThreadStringList; AClear: Boolean = True);
var
  IPAdrRow: TMIB_IPADDRROW;
  TableSize, NumEntries, ec, Size, n: Cardinal;
  i,r,j: integer;
  pBuf: PAnsiChar;
  ai,aiInitPtr: PIP_ADAPTER_INFO;
  s: string;
  sl: TStringList;
begin
  if not Assigned(GetIPAddrTable) then
    Exit;
  if AClear then
    List.Clear;
  TableSize:=0;
  ec:=GetIPAddrTable(nil,@TableSize,False);
  if ec=ERROR_NO_DATA then
    Exit;
  GetMem(pBuf,TableSize);
  NumEntries:=0;
  sl:=TStringList.Create;
  try
    ec:=GetIpAddrTable(PMIB_IPADDRTABLE(pBuf),@TableSize,False);
    if ec=NO_ERROR then begin
      NumEntries:=PMIB_IPADDRTABLE(pBuf)^.dwNumEntries;
      if NumEntries>0 then begin
        Inc(pBuf,SizeOf(Cardinal));
        for i:=1 to NumEntries do begin
          IPAdrRow:=PMIB_IPADDRROW(PBuf)^;
          with IPAdrRow do
            sl.Add(Format('%s=*%d',[IPv4ToStr(InAddrToIPv4(TInAddr(dwAddr))),dwIndex]));
          Inc(pBuf,SizeOf(IPAdrRow));
        end;
      end;
    end;
  finally
    Dec(pBuf,SizeOf(Cardinal)+NumEntries*SizeOf(IPAdrRow));
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
    if(r=ERROR_SUCCESS) then
      while assigned(ai) do begin
        s:='';
        if ai^.AddressLength>0 then begin
          for j:=0 to ai^.AddressLength-1 do
            s:=s+Format('%2.2x-',[ai^.Address[j]]);
          SetLength(s,Length(s)-1);
        end;
        for i:=0 to sl.Count-1 do begin
          n:=StrToIntDef(Copy(sl.ValueFromIndex[i],2,255),-1);
          if (n=ai.Index) then begin
            sl.ValueFromIndex[i]:=s;
            Break;
          end;
        end;
        ai:=ai.Next;
      end;
  finally
    i:=0;
    while i<sl.Count do
      if Pos('*',sl[i])>0 then
        sl.Delete(i)
      else
        Inc(i);

    if Assigned(aiInitPtr) then
      FreeMem(aiInitPtr);

    List.AddStrings(sl);
    sl.Free;
  end;
end;

function UseIPC(AHost,ADomain,AUser,APwd: string): Cardinal;
var
  ui: TUseInfo2;
  wh,wd,wu,wp: WideString;
  pe: Cardinal;
begin
  wh:=AHost;
  wd:=ADomain;
  wu:=AUser;
  wp:=APwd;
  ui.ui2_local:=nil;
  ui.ui2_remote:=PWideChar('\\'+wh+'\IPC$');
  ui.ui2_username:=PWideChar(wu);
  ui.ui2_password:=PWideChar(wp);
  ui.ui2_domainname:=PWideChar(wd);
  ui.ui2_asg_type:=USE_WILDCARD;//USE_IPC;
  Result:=NetUseAdd(nil,2,@ui,@pe);
  if Result<>NERR_SUCCESS then begin
    wu:='Guest';
    ui.ui2_username:=PWideChar(wu);
    ui.ui2_password:=nil;
    Result:=NetUseAdd(nil,2,@ui,@pe);
  end;
end;

function DelIPC(AHost: string): Cardinal;
var
  wh: WideString;
begin
  wh:=AHost;
  Result:=NetUseDel(nil,PWideChar('\\'+wh+'\IPC$'),USE_LOTS_OF_FORCE);
end;

function ShareExists;
var
  wh,ws: WideString;
  d: Cardinal;
begin
  wh:='\\'+AHost;
  ws:=AShare;
  Result:=NetShareCheck(PWideChar(wh),PWideChar(ws),@d)=NERR_SUCCESS;
end;

function ShareAccess;
var
  ui: TUseInfo2;
  wh,wd,wu,wp,ws: WideString;
  ec,pe: Cardinal;
begin
  wh:=AHost;
  wd:=ADomain;
  wu:=AUser;
  wp:=APwd;
  ws:=AShare;
  ui.ui2_local:=nil;
  ui.ui2_remote:=PWideChar('\\'+wh+'\'+ws);
  ui.ui2_username:=PWideChar(wu);
  ui.ui2_password:=PWideChar(wp);
  ui.ui2_domainname:=PWideChar(wd);
  ui.ui2_asg_type:=USE_DISKDEV;
  ec:=NetUseAdd(nil,2,@ui,@pe);
  Result:=ec=NERR_SUCCESS;
  NetUseDel(nil,PWideChar('\\'+wh+'\'+ws),USE_LOTS_OF_FORCE);
end;

function ShareAccessEx;
var
  s: string;
const
  cText = 'ABCD';
begin
  s:=IncludeTrailingPathDelimiter(APath)+'test.txt';
  try
    with TFileSTream.Create(s,fmCreate or fmShareExclusive) do
      try
        Write(cText,Length(cText));
      finally
        Free;
      end;
    Result:=DeleteFile(s);
  except
    Result:=False;
  end;
end;

function GetSharePath;
var
  pBuf: Pointer;
  wd,ws: WideString;
  p,n: Integer;
  s: string;
begin
  Result:='';
  wd:=Format('\\%s',[AHost]);
  p:=Pos('\',AShare);
  s:='';
  if p>0 then begin
    ws:=Copy(AShare,1,p-1);
    s:=Copy(AShare,p+1,255);
  end else
    ws:=AShare;
  n:=NetShareGetInfo(PWideChar(wd),PWideChar(ws),2,pBuf);
  if n=ERROR_SUCCESS then
    Result:=IncludeTrailingPathdelimiter(WideCharToString(PSHARE_INFO_2(pBuf)^.shi2_path))+s;
  if Assigned(pBuf) then
    NetApiBufferFree(pBuf);
end;

function AddShare;
var
  ui: TUseInfo2;
  wd,wu,wp,ws: WideString;
  ec,pe: Cardinal;
begin
  wd:=ADomain;
  wu:=AUser;
  wp:=APwd;
  ws:=AShare;
  ui.ui2_local:=nil;
  ui.ui2_remote:=PWideChar(ws);
  ui.ui2_username:=PWideChar(wu);
  ui.ui2_password:=PWideChar(wp);
  ui.ui2_domainname:=PWideChar(wd);
  ui.ui2_asg_type:=USE_DISKDEV;
  ec:=NetUseAdd(nil,2,@ui,@pe);
  Result:=ec=NERR_SUCCESS;
end;

function TimeDetect(AHost: string; var AUpTime: int64): TDateTime;
var
  ec: Cardinal;
  tod: Pointer;
  h: WideString;
  st,lst: SYSTEMTIME;
begin
  Result:=0;
  AUptime:=0;
  h:='\\'+AHost;
  ec:=NetRemoteTOD(PWideChar(h),tod);
  if ec=NERR_SUCCESS then
     with PTIME_OF_DAY_INFO(tod)^ do begin
       AUpTime:=tod_msecs;
       st.wYear:=tod_year;
       st.wMonth:=tod_month;
       st.wDayOfWeek:=tod_weekday;
       st.wDay:=tod_day;
       st.wHour:=tod_hours;
       st.wMinute:=tod_mins;
       st.wSecond:=tod_secs;
       st.wMilliseconds:=0;
       if SystemTimeToTzSpecificLocalTime(nil,st,lst) then
         Result:=SystemTimeToDateTime(lst);
     end;
  NetAPIBufferFree(tod);
end;

function UserDetect(AHost: string): string;
var
  User,Domain: array[0..1024] of Char;
  kn: array[0..MAX_PATH] of Char;
  i,n,usz,dsz,idx,ec: Cardinal;
  lwt: FILETIME;
  uk: HKEY;
  sid: PSID;
  sidType: SID_NAME_USE;
  Authority: SID_IDENTIFIER_AUTHORITY;
  sac: BYTE;
  Auth,Rev: Cardinal;
  SubAuth: array[0..7] of Cardinal;

  function GetVals(ASource: string): integer;
  var
    i,j,l,k: integer;
    s: string;
  begin
    Delete(ASource,1,2);
    j:=Pos('-',ASource);
    s:=Copy(ASource,1,j-1);
    Val(s,Rev,k);
    Delete(ASource,1,j);
    j:=pos('-',ASource);
    s:=Copy(ASource,1,j-1);
    Val('$'+s,Auth,k);
    Delete(ASource,1,j);
    i:=2;
    ASource:=ASource+'-';
    for l:=0 to 7 do begin
      j:=pos('-',ASource);
      if j>0 then begin
        s:=Copy(ASource,1,j-1);
        Val(s,SubAuth[l],k);
        Delete(ASource,1,j);
        inc(i);
      end else
        Break;
    end;
    Result:=i;
  end;

begin
  Result:='';
  Rev:=0;
  Auth:=0;
  if AHost<>'' then begin
    uk:=0;
    ec:=RegConnectRegistry(PChar(Format('\\%s',[AHost])),HKEY_USERS,uk);
  end else
    ec:=RegOpenKey(HKEY_USERS,nil,uk);
  if (ec<>ERROR_SUCCESS) then
    Exit;
  idx:=0;
  n:=SizeOf(kn);
  while (RegEnumKeyEx(uk,idx,kn,n,nil,nil,nil,@lwt)=ERROR_SUCCESS) do begin
    if not SameText(string(kn),'.default') and (Pos('Classes',string(kn))=0) then begin
      sac:=GetVals(kn);
      if (sac>=3) then begin
        sac:=sac-2;
        if (sac<2) then
          sac:=2;
        Authority.Value[5]:=PByte(@Auth)^;
        authority.Value[4]:=PByte(Cardinal(@Auth)+1)^;
        authority.Value[3]:=PByte(Cardinal(@Auth)+2)^;
        authority.Value[2]:=PByte(Cardinal(@Auth)+3)^;
        authority.Value[1]:=0;
        authority.Value[0]:=0;
        sid:=nil;
        usz:=1024;
        dsz:=1024;
        if AllocateAndInitializeSid(authority,sac,SubAuth[0],SubAuth[1],SubAuth[2],SubAuth[3],SubAuth[4],SubAuth[5],SubAuth[6],SubAuth[7],sid) then begin
          i:=0;
          repeat
            if LookupAccountSid(PChar(AHost),sid,User,usz,Domain,dsz,sidType) then
              Result:=User;
            ec:=GetLastError;
            Inc(i);
          until (string(User)<>'') or (ec=ERROR_NONE_MAPPED) or (i>10);
        end;
        if Assigned(sid) then
          FreeSid(sid);
      end;
    end;
    n:=SizeOf(kn);
    Inc(idx);
  end;
  RegCloseKey(uk);
end;

function GetMAC_ARPTable(AIP: string; ARefreshARPTable: Boolean = False): string;
var
  i,c: Integer;
begin
  Result:='';
  if (ARPTable.Count=0) or ARefreshARPTable then
    GetARPTable(ARPTable);
  c:=ARPTable.Count;
  ARPTable.Lock;
  try
    for i:=0 to c-1 do
      if Pos(ARPTable.Stringlist.Names[i]+'.',AIP+'.')>0 then begin
        Result:=ARPTable.StringList.ValueFromIndex[i];
        Break;
      end;
  finally
    ARPTable.Unlock;
  end;
end;

function GetMAC_AddrTable(AIP: string; ARefreshTable: Boolean = False): string;
var
  i,c: Integer;
begin
  Result:='';
  c:=AddrTable.Count;
  if (c=0) or ARefreshTable then
    GetAddrTable(AddrTable);
  c:=AddrTable.Count;
  AddrTable.Lock;
  try
    for i:=0 to c-1 do
      if Pos(AddrTable.StringList.Names[i]+'.',AIP+'.')>0 then begin
        Result:=AddrTable.StringList.ValueFromIndex[i];
        Break;
      end;
  finally
    AddrTable.Unlock;
  end;
end;

function ShareDetect(AName: string; var AList: TSharedResources): Cardinal;
var
  pTmpBuf, pBuf: PSHARE_INFO_1;
  nStatus: NET_API_STATUS;
  i,dwPrefMaxLen: Cardinal;
  dwEntriesRead: Cardinal;
  dwTotalEntries: Cardinal;
  dwResumeHandle: Cardinal;
  Loop: Boolean;
  Buffer: array[0..255] of WideChar;
begin
  Result:=0;
  Finalize(AList);

  pBuf:=nil;
  Loop:=True;
  dwPrefMaxLen:=$FFFFFFFF;
  dwEntriesRead:=0;
  dwTotalEntries:=0;
  dwResumeHandle:=0;
  while Loop do begin
    nStatus:=NetShareEnum(StringToWideChar(AName,Buffer,256),1,
                          Pointer(pBuf),dwPrefMaxLen,dwEntriesRead,dwTotalEntries,dwResumeHandle);
    if (nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA) then begin
      pTmpBuf:=pBuf;
      i:=0;
      repeat
        with pTmpBuf^ do begin
          SetLength(AList,Length(Alist)+1);
          with AList[High(AList)] do begin
            Name:=WideCharToString(shi1_netname);
            Comment:=WideCharToString(shi1_remark);
            Typ:=shi1_type and $0000000f;
          end;
        end;
        pTmpBuf:=PSHARE_INFO_1(PAnsiChar(pTmpBuf)+SizeOf(SHARE_INFO_1));
        Inc(i);
      until i>=dwEntriesRead;
      if Assigned(pBuf) then
        NetApiBufferFree(pBuf);
      if nStatus=ERROR_SUCCESS then
        Loop:=False;
      dwResumeHandle:=dwEntriesRead+1;
    end else begin
      Result:=GetLastError;
      Loop:=False;
    end;
  end;
end;

function OpenFilesDetect(AName: string; var AList: TOpenFiles): Cardinal;
var
  pTmpBuf, pBuf: PFILE_INFO_3;
  nStatus: NET_API_STATUS;
  i: Cardinal;
  dwPrefMaxLen: Cardinal;
  dwEntriesRead: Cardinal;
  dwTotalEntries: Cardinal;
  dwResumeHandle: Cardinal;
  Loop: Boolean;
  Buffer: array[0..256] of WideChar;
  ofr: TOpenFile;
begin
  Result:=0;
  Finalize(AList);
  pBuf:=nil;
  Loop:=True;
  dwPrefMaxLen:=$FFFFFFFF;
  dwEntriesRead:=0;
  dwTotalEntries:=0;
  dwResumeHandle:=0;
  while Loop do begin
    nStatus:=NetFileEnum(StringToWideChar(AName,Buffer,256),nil,nil,3,
                          Pointer(pBuf),
                          dwPrefMaxLen, dwEntriesRead, dwTotalEntries, dwResumeHandle);
    if ((nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA)) and (dwEntriesRead>0) then begin
      pTmpBuf:=pBuf;
      for i:=0 to dwEntriesRead-1 do begin
        ZeroMemory(@ofr,SizeOf(ofr));
        ofr.Name:=WideCharToString(pTmpBuf^.fi3_pathname);
        ofr.UserName:=WideCharToString(pTmpBuf^.fi3_username);
        ofr.Locks:=pTmpBuf^.fi3_num_locks;
        ofr.Mode:=pTmpBuf^.fi3_permissions;
        ofr.ID:=pTmpBuf^.fi3_id;
        SetLength(AList,Length(AList)+1);
        AList[High(Alist)]:=ofr;
        pTmpBuf:=PFILE_INFO_3(PAnsiChar(pTmpBuf)+SizeOf(FILE_INFO_3));
      end;
      if Assigned(pBuf) then
        NetApiBufferFree(pBuf);
      if nStatus=ERROR_SUCCESS then
        Loop:=False;
      dwResumeHandle:=dwEntriesRead+1;
    end else begin
      Result:=GetLastError;
      Loop:=False;
    end;
  end;
end;

function SessionDetect(AName: string; var AList: TSessions): Cardinal;
var
  pTmpBuf, pBuf: PSESSION_INFO_502;
  nStatus: NET_API_STATUS;
  i: Cardinal;
  dwPrefMaxLen: Cardinal;
  dwEntriesRead: Cardinal;
  dwTotalEntries: Cardinal;
  dwResumeHandle: Cardinal;
  Loop: Boolean;
  Buffer: array[0..256] of WideChar;
  r: TSessionRecord;
begin
  Result:=0;
  Finalize(AList);
  pBuf:=nil;
  Loop:=True;
  dwPrefMaxLen:=$FFFFFFFF;
  dwEntriesRead:=0;
  dwTotalEntries:=0;
  dwResumeHandle:=0;
  while Loop do begin
    nStatus:=NetSessionEnum(StringToWideChar(AName,Buffer,256),nil,nil,502,
                          Pointer(pBuf),
                          dwPrefMaxLen, dwEntriesRead, dwTotalEntries, dwResumeHandle);

    if ((nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA)) and (dwEntriesRead>0) then begin
      pTmpBuf:=pBuf;
      for i:=0 to dwEntriesRead-1 do begin
        ZeroMemory(@r,SizeOf(r));
        r.Name:=WideCharToString(pTmpBuf^.sesi502_cname);
        r.UserName:=WideCharToString(pTmpBuf^.sesi502_username);
        r.OpenFiles:=pTmpBuf^.sesi502_num_opens;
        r.Typ:=WideCharToString(pTmpBuf^.sesi502_cltype_name);
        r.ConnectedTime:=pTmpBuf^.sesi502_time;
        r.IdleTime:=pTmpBuf^.sesi502_idle_time;
        r.Guest:=pTmpBuf^.sesi502_user_flags=SESS_GUEST;
        r.Transport:=WideCharToString(pTmpBuf^.sesi502_transport);
        SetLength(AList,Length(AList)+1);
        AList[High(Alist)]:=r;
        pTmpBuf:=PSESSION_INFO_502(PAnsiChar(pTmpBuf)+SizeOf(SESSION_INFO_502));
      end;
      if Assigned(pBuf) then
        NetApiBufferFree(pBuf);
      if nStatus=ERROR_SUCCESS then
        Loop:=False;
      dwResumeHandle:=dwEntriesRead+1;
    end else begin
      Result:=GetLastError;
      Loop:=False;
    end;
  end;
end;

function ServiceDetect(AName: string; var AList: TServices): Cardinal;
{$IFDEF FPC}
const
    SERVICE_TYPE_ALL = (SERVICE_WIN32 or SERVICE_ADAPTER or SERVICE_DRIVER or SERVICE_INTERACTIVE_PROCESS);
{$ENDIF}
var
  sl: TStringList;
  i: Integer;
  qsp :TServiceConfig;
  st,n,im: string;
  s: Cardinal;
begin
  Finalize(AList);
  sl:=TStringList.Create;
  try
    Result:=ServiceGetList(AName,SERVICE_TYPE_ALL,SERVICE_STATE_ALL,sl);
    for i:=0 to sl.Count-1 do begin
      n:=ServiceGetKeyName(sl[i],AName);
      s:=ServiceGetStatus(n,AName);
      ZeroMemory(@qsp,SizeOf(qsp));
      st:='?';
      if ServiceGetConfig(AName,n,qsp) then begin
        st:=cSvcStartup[qsp.dwStartType];
        im:=StringReplace(qsp.BinaryPathName,'"','',[rfReplaceAll,rfIgnoreCase]);
      end;
      SetLength(AList,Length(Alist)+1);
      with AList[High(AList)] do begin
        case qsp.dwServiceType of
          SERVICE_KERNEL_DRIVER       :Typ:=svcKernelDriver;
          SERVICE_FILE_SYSTEM_DRIVER  :Typ:=svcFileSystemDriver;
          SERVICE_ADAPTER             :Typ:=svcAdapter;
          SERVICE_RECOGNIZER_DRIVER   :Typ:=svcRecognizerDriver;
          SERVICE_WIN32_OWN_PROCESS   :Typ:=svcOwnProcess;
          SERVICE_WIN32_SHARE_PROCESS :Typ:=svcSharedProcess;
          SERVICE_INTERACTIVE_PROCESS :Typ:=svcDesktopInteractiveProcess;
          SERVICE_INTERACTIVE_OWN_PROCESS :Typ:=svcOwnInteractiveProcess;
          SERVICE_INTERACTIVE_SHARE_PROCESS :Typ:=svcShareInteractiveProcess;
          else Typ:=svcUnknown;
        end;
        Name:=n;
        Status:=s;
        Startup:=st;
        Displayname:=sl[i];
        CmdLine:=im;
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure GetWkstaInfo;
var
 wksta: Pointer;
 ec: cardinal;
 p: PWideChar;
begin
  Domain:='';
  OSMajor:=0;
  OSMinor:=0;
  OSPlatform:=0;
  Name:='';
  {if not InitNetAPI then
    Exit;}
  try
    if (Pos('\\',AHost)=0) then begin
      if Trim(AHost)='' then
        AHost:='\\.'
      else
        AHost:='\\'+AHost;
    end;

    if Trim(AHost)='' then
      ec:=NetWkstaGetInfo(nil,ALevel,wksta)
    else begin
      p:=Allocmem(2*(Length(AHost)+1));
      StringToWideChar(AHost,p,2*(Length(AHost)+1));
      ec:=NetWkstaGetInfo(p,ALevel,wksta);
      Freemem(p);
    end;
    if (ec<>NERR_Success) then
      Exit;
    case ALevel of
      100: with PWKSTA_INFO_100(wksta)^ do begin
             Name:=WideCharToString(wksi100_computername);
             Domain:=WideCharToString(wksi100_langroup);
             OSMajor:=wksi100_ver_major;
             OSMinor:=wksi100_ver_minor;
             OSPlatform:=wksi100_platform_id;
           end;
      102: with PWKSTA_INFO_102(wksta)^ do begin
             Name:=WideCharToString(wksi102_computername);
             Domain:=WideCharToString(wksi102_langroup);
             OSMajor:=wksi102_ver_major;
             OSMinor:=wksi102_ver_minor;
             OSPlatform:=wksi102_platform_id;
           end;
    end;
  finally
    NetApiBufferFree(wksta);
  end;
end;

procedure GetWkstaUsers;
var
  Buffer, tmpBuffer: Pointer;
  PrefMaxLen,
  Resume_Handle,
  EntriesRead,
  TotalEntries: Cardinal;
  i, Size: Integer;
  PSrvr: PWideChar;
  s: string;
  r: Integer;
begin
  PSrvr:=nil;
  try
    Users:='';

    if (Pos('\\',AHost)=0) then begin
      if Trim(AHost)='' then
        AHost:='\\.'
      else
        AHost:='\\'+AHost;
    end;
    Size:=Length(AHost);
    GetMem(PSrvr,Size*SizeOf(WideChar)+1);
    StringToWideChar(AHost,PSrvr,Size+1);

    PrefMaxLen:=DWORD(-1);
    EntriesRead:=0;
    TotalEntries:=0;
    Resume_Handle:=0;
    Buffer:=nil;

    r:=NetWkstaUserEnum(PSrvr,1,@Buffer,PrefMaxLen,@EntriesRead,@TotalEntries,@Resume_Handle);
    if r=S_OK then begin
      tmpBuffer:=Pointer(DWORD(Buffer));
      for i:=0 to EntriesRead-1 do begin
        s:=WKSTA_USER_INFO_1(tmpBuffer^).wkui1_username;
        if (Pos('$',s)=0) and (Pos('__',s)=0) and (Pos(s+',',Users)=0)  then
          Users:=Users+s+',';
        tmpBuffer:=Pointer(DWORD(tmpBuffer)+SizeOf(WKSTA_USER_INFO_1));
      end;
      Setlength(Users,Length(Users)-1);
    end;
  finally
    NetApiBufferFree(Buffer);
    FreeMem(PSrvr);
  end;
end;


procedure GetServerInfo;
var
 server: Pointer;
 ec: cardinal;
 p: PWideChar;
begin
  OSMajor:=0;
  OSMinor:=0;
  OSPlatform:=0;
  Comment:='';
  {if not InitNetAPI then
    Exit;}
  try
    if (Pos('\\',AHost)=0) then begin
      if Trim(AHost)='' then
        AHost:='\\.'
      else
        AHost:='\\'+AHost;
    end;

    if Trim(AHost)='' then
      ec:=NetServerGetInfo(nil,101,server)
    else begin
      p:=Allocmem(2*(Length(AHost)+1));
      StringToWideChar(AHost,p,2*(Length(AHost)+1));
      ec:=NetServerGetInfo(p,101,server);
      Freemem(p);
    end;
    if (ec<>NERR_Success) then
      Exit;
    with PSERVER_INFO_101(server)^ do begin
      OSMajor:=sv101_version_major;
      OSMinor:=sv101_version_minor;
      OSPlatform:=sv101_platform_id;
      Comment:=sv101_comment;
      Flags:=sv101_type;
    end;
  finally
    NetApiBufferFree(server);
  end;
end;

procedure DetectNetObject(const AName: string; var Data: TNetObject;
                          ABasic: Boolean = True;
                          ANetBIOS: Boolean = False;
                          AARP: Boolean = False;
                          ARegistry: Boolean = False);
var
  osmaj,osmin,osp: Cardinal;
  s,d,u,os,cpu,m,hn: string;
  dt: TDateTime;
  p: Integer;
  ut: Int64;
begin
  if VarIsIPv4(AName) then
    data.IPv4Address:=AName;
  if data.IPv4Address='' then begin
    data.IPv4Address:=GetIP(AName);
    if data.IPv4Address<>'' then
      data.IPv4:=StrToIPv4(GetFirstIP(data.IPv4Address));
  end;
  if ANetBIOS then begin
    NBLookup(AName,m,d,u,s);
    data.Name:=m;
    data.Domain:=d;
    data.User:=u;
    if not SameText(s,'00-00-00-00-00-00') then
      data.MACAddress:=s;
  end;
  if data.IPv4Address<>'' then begin
    s:=MiTeC_NetUtils.GetHostByAddr(data.IPv4);
    if (s<>'') then
      data.Name:=s;
  end;
  if data.IPv4Address<>'' then begin
    if data.MACAddress='' then
      data.MACAddress:=GetMAC_ARPTable(GetFirstIP(data.IPv4Address));
    if data.MACAddress='' then
      data.MACAddress:=GetMAC_AddrTable(GetFirstIP(data.IPv4Address));
  end;
  if ((data.PingTime=-1) or (data.MACAddress='')) and AARP then begin
    s:=GetMAC_ARP(data.IPv4,data.ARPTime);
    if (s<>'') then
      data.MACAddress:=s;
  end;
  data.MACAddress:=FastStringReplace(data.MACAddress,','+GetFirstIP(data.MACAddress),'');
  data.ALive:=(data.IPv4Address<>'') and ((data.PingTime>-1) or (data.ARPTime>-1) or (data.NBTime>-1) or (data.Name<>''));

  p:=Pos('.',data.Name);
  if p>0 then
    data.MachineName:=Copy(data.Name,1,p-1)
  else
    data.MachineName:=data.Name;

  if data.ALive and not ABasic then begin
    try
      hn:=GetHostID(data);
      GetServerInfo(hn,s,osmaj,osmin,osp,data.Flags);
      if data.Desc='' then
        data.Desc:=s;
      if data.OS='' then
        data.OS:=GetOSNameFromVer(osp,osmaj,osmin,data.Flags);

      GetWkstaInfo(hn,100,m,s,osmaj,osmin,osp);
      if (s<>'') and not SameText(data.Name,s) then
        data.Domain:=s;
      if data.OS='' then
        data.OS:=GetOSNameFromVer(osp,osmaj,osmin,0);

      GetWkstaUsers(hn,u);
      if (u<>'') then
        data.User:=u;

      if ARegistry then begin
        GetDataFromRegistry(hn,os,m,cpu,data.BIOS,data.Model,data.Vendor,data.InstallDate);
        if os<>'' then
          data.OS:=FormatOSName(os);
        if data.User='' then
          data.User:=UserDetect(hn);
      end;

      if (data.CPU='') and (cpu<>'') then begin
        data.CPU:=FormatCPUName(cpu);
        data.CPUCount:=1;
      end;

      dt:=TimeDetect(hn,ut);
      if dt>0 then
        data.RemoteTime:=dt;
      if ut>0 then
        data.SystemUpTime:=ut;

      if (data.Name='') or (m<>'') then
        data.MachineName:=m;

      data.MachineName:=Uppercase(data.MachineName{$IFDEF BDS3PLUS},loUserLocale{$ENDIF});
    finally
    end;
  end;
end;

function CopyData(Source,Dest: TNetObject; AForceSource: Boolean = False): TNetObject;
var
  m: Boolean;
begin
  m:=False;

  if //(not SameText(Dest.Name,Source.Name) and (Source.Name<>'')) or
     (not SameText(Dest.MACAddress,Source.MACAddress) and (Source.MACAddress<>'')) then
   ZeroMemory(@Dest,sizeof(Dest));

  if AForceSource or ((Dest.Name='') and (Source.Name<>'')) then begin
    Dest.Name:=Source.Name;
    m:=True;
  end;
  if AForceSource or ((Dest.MachineName='') and (Source.MachineName<>'')) then begin
    Dest.MachineName:=Source.MachineName;
    m:=True;
  end;
  if AForceSource or ((Dest.Domain='') and (Source.Domain<>'')) then begin
    Dest.Domain:=Source.Domain;
    m:=True;
  end;
  if AForceSource or ((Dest.User='') and (Source.User<>'')) then begin
    Dest.User:=Source.User;
    m:=True;
  end;
  if AForceSource or ((Dest.OS='') and (Source.OS<>'')) then begin
    Dest.OS:=Source.OS;
    m:=True;
  end;
  if AForceSource or ((Dest.IPv4Address='') and (Source.IPv4Address<>'')) then begin
    Dest.IPv4Address:=Source.IPv4Address;
    m:=True;
  end;
  if AForceSource or ((Dest.IPv6Address='') and (Source.IPv6Address<>'')) then begin
    Dest.IPv6Address:=Source.IPv6Address;
    m:=True;
  end;
  if AForceSource or ((Dest.MACAddress='') and (Source.MACAddress<>'')) then begin
    Dest.MACAddress:=Source.MACAddress;
    m:=True;
  end;
  if AForceSource or ((Dest.MACVendor='') and (Source.MACVendor<>'')) then begin
    Dest.MACVendor:=Source.MACVendor;
    //m:=True;
  end;
  if AForceSource or ((Dest.Desc='') and (Source.Desc<>'')) then begin
    Dest.Desc:=Source.Desc;
    m:=True;
  end;
  if (Dest.PingTime=0) then begin
    Dest.PingTime:=Source.PingTime;
    //m:=True;
  end;
  if AForceSource or ((Dest.BIOS='') and (Source.BIOS<>'')) then begin
    Dest.BIOS:=Source.BIOS;
    m:=True;
  end;
  if AForceSource or ((Dest.Model='') and (Source.Model<>'')) then begin
    Dest.Model:=Source.Model;
    m:=True;
  end;
  if AForceSource or ((Dest.CPU='') and (Source.CPU<>'')) then begin
    Dest.CPU:=Source.CPU;
    m:=True;
  end;
  if AForceSource or ((Dest.UUID='') and (Source.UUID<>'')) then begin
    Dest.UUID:=Source.UUID;
    m:=True;
  end;
  if AForceSource or ((Dest.IDNumber='') and (Source.IDNumber<>'')) then begin
    Dest.IDNumber:=Source.IDNumber;
    m:=True;
  end;
  if AForceSource or ((Dest.Memory<>Source.Memory) and (Source.Memory<>0)) then begin
    Dest.Memory:=Source.Memory;
    m:=True;
  end;
  if AForceSource or ((Dest.CPUCount<>Source.CPUCount) and (Source.CPUCount<>0)) then begin
    Dest.CPUCount:=Source.CPUCount;
    m:=True;
  end;
  if AForceSource or ((Dest.CPUFreq<>Source.CPUFreq) and (Source.CPUFreq<>0)) then begin
    Dest.CPUFreq:=Source.CPUFreq;
    m:=True;
  end;
  Dest.SNMP:=Source.SNMP;
  Dest.WMI:=Source.WMI;
  Dest.ALive:=Source.ALive;
  try
    if (Dest.ElapsedTime<>Source.ElapsedTime) and (Source.ElapsedTime>0) then begin
      Dest.ElapsedTime:=Source.ElapsedTime;
    //m:=True;
    end;
  except
    Dest.ElapsedTime:=0;
  end;
  if not SameDatetime(Dest.RemoteTime,Source.RemoteTime) and (Source.RemoteTime<>0) then begin
    Dest.RemoteTime:=Source.RemoteTime;
    //m:=True;
  end;
  if AForceSource or (Dest.InstallDate<>Source.InstallDate) and (Source.InstallDate>30000) then begin
    Dest.InstallDate:=Source.InstallDate;
    m:=True;
  end;
  if (Dest.SystemUpTime<>Source.SystemUpTime) and (Source.SystemUpTime<>0) then begin
    Dest.SystemUpTime:=Source.SystemUpTime;
    //m:=True;
  end;
  if AForceSource or ((Dest.Flags<>Source.Flags) and (Source.Flags<>0)) then begin
    Dest.Flags:=Source.Flags;
    m:=True;
  end;
  if AForceSource or ((Dest.TCPPorts='') and (Source.TCPPorts<>'')) then begin
    Dest.TCPPorts:=Source.TCPPorts;
    m:=True;
  end;
  Dest.Modified:=Dest.Modified or m;
  Result:=Dest;
end;

function CopyDataNew(Source,Dest: TNetObject): TNetObject;
var
  m: Boolean;
begin
  m:=False;
  if ((Dest.Name='') or (Source.Name<>'')) then begin
    Dest.Name:=Source.Name;
    m:=True;
  end;
  if ((Dest.Domain='') or (Source.Domain<>'')) then begin
    Dest.Domain:=Source.Domain;
    m:=True;
  end;
  if ((Dest.User='') or (Source.User<>'')) then begin
    Dest.User:=Source.User;
    m:=True;
  end;
  if ((Dest.OS='') or (Source.OS<>'')) then begin
    Dest.OS:=Source.OS;
    m:=True;
  end;
  if ((Dest.IPv4Address='') or (Source.IPv4Address<>'')) then begin
    Dest.IPv4Address:=Source.IPv4Address;
    m:=True;
  end;
  if ((Dest.IPv6Address='') or (Source.IPv6Address<>'')) then begin
    Dest.IPv6Address:=Source.IPv6Address;
    m:=True;
  end;
  if ((Dest.MACAddress='') or (Source.MACAddress<>'')) then begin
    Dest.MACAddress:=Source.MACAddress;
    m:=True;
  end;
  if ((Dest.MACVendor='') or (Source.MACVendor<>'')) then begin
    Dest.MACVendor:=Source.MACVendor;
    //m:=True;
  end;
  if ((Dest.Desc='') or (Source.Desc<>'')) then begin
    Dest.Desc:=Source.Desc;
    m:=True;
  end;
  if (Dest.PingTime=0) then begin
    Dest.PingTime:=Source.PingTime;
    //m:=True;
  end;
  if ((Dest.BIOS='') or (Source.BIOS<>'')) then begin
    Dest.BIOS:=Source.BIOS;
    m:=True;
  end;
  if ((Dest.Model='') or (Source.Model<>'')) then begin
    Dest.Model:=Source.Model;
    m:=True;
  end;
  if ((Dest.CPU='') or (Source.CPU<>'')) then begin
    Dest.CPU:=Source.CPU;
    m:=True;
  end;
  if ((Dest.UUID='') or (Source.UUID<>'')) then begin
    Dest.UUID:=Source.UUID;
    m:=True;
  end;
  if ((Dest.IDNumber='') or (Source.IDNumber<>'')) then begin
    Dest.IDNumber:=Source.IDNumber;
    m:=True;
  end;
  if ((Dest.Memory=0) or (Source.Memory<>0)) then begin
    Dest.Memory:=Source.Memory;
    m:=True;
  end;
  if ((Dest.CPUCount=0) or (Source.CPUCount<>0)) then begin
    Dest.CPUCount:=Source.CPUCount;
    m:=True;
  end;
  if ((Dest.CPUFreq<>Source.CPUFreq) or (Source.CPUFreq<>0)) then begin
    Dest.CPUFreq:=Source.CPUFreq;
    m:=True;
  end;
  Dest.SNMP:=Source.SNMP;
  Dest.WMI:=Source.WMI;
  Dest.ALive:=Source.ALive;
  try
    if (Dest.ElapsedTime=0) or (Source.ElapsedTime>0) then begin
      Dest.ElapsedTime:=Source.ElapsedTime;
    //m:=True;
    end;
  except
    Dest.ElapsedTime:=0;
  end;
  if (Dest.RemoteTime=0) or (Source.RemoteTime<>0) then begin
    Dest.RemoteTime:=Source.RemoteTime;
    //m:=True;
  end;
  if (Dest.InstallDate=0) or (Source.InstallDate>30000) then begin
    Dest.InstallDate:=Source.InstallDate;
    m:=True;
  end;
  if (Dest.SystemUpTime<>0) or (Source.SystemUpTime<>0) then begin
    Dest.SystemUpTime:=Source.SystemUpTime;
    //m:=True;
  end;
  if ((Dest.Flags=0) or (Source.Flags<>0)) then begin
    Dest.Flags:=Source.Flags;
    m:=True;
  end;
  if ((Dest.TCPPorts='') and (Source.TCPPorts<>'')) then begin
    Dest.TCPPorts:=Source.TCPPorts;
    m:=True;
  end;
  Dest.Modified:=Dest.Modified or m;
  Result:=Dest;
end;

function GetOSNameFromVer;
begin
  Result:='';
  if Platf=500 then begin
    if (Major=10) then begin
      if Flags and SV_TYPE_SERVER_NT<>0 then
        Result:='Windows Server 2016'
      else
        Result:='Windows 10'
    end else if (Major=6) and (Minor=3) then begin
      if Flags and SV_TYPE_SERVER_NT<>0 then
        Result:='Windows Server 2012 R2'
      else
        Result:='Windows 8.1'
    end else if (Major=6) and (Minor=2) then begin
      if Flags and SV_TYPE_SERVER_NT<>0 then
        Result:='Windows Server 2012'
      else
        Result:='Windows 8'
    end else if (Major=6) and (Minor=1) then begin
      if Flags and SV_TYPE_SERVER_NT<>0 then
        Result:='Windows Server 2008 R2'
      else
        Result:='Windows 7'
    end else if (Major=6) and (Minor=0) then begin
      if Flags and SV_TYPE_SERVER_NT<>0 then
        Result:='Windows Server 2008'
      else
        Result:='Windows Vista'
    end else if (Major=5) and (Minor=2) then begin
      if Flags and SV_TYPE_SERVER_NT<>0 then
        Result:='Windows Server 2003 R2'
      else
        Result:='Windows XP Pro x64'
    end else if (Major=5) and (Minor=1) then begin
      if Flags and SV_TYPE_SERVER_NT<>0 then
        Result:='Windows Server 2003'
      else
        Result:='Windows XP'
    end else if (Major=5) and  (Minor=0) then begin
      if Flags and SV_TYPE_SERVER_NT<>0 then
        Result:='Windows Server 2000'
      else
        Result:='Windows 2000';
    end else if (Major=4) then begin
      if Flags and SV_TYPE_XENIX_SERVER=0 then
        Result:='Windows NT4'
      else
        Result:='Unix/Linux';
    end else begin
      if Flags and SV_TYPE_XENIX_SERVER=0 then
        Result:=Format('Windows %d.%d',[Major,Minor])
      else
        Result:='Unix/Linux';
    end;
  end else if Platf=400 then begin
    if (Major=4) and (Minor=0) then
      Result:='Windows 9x/ME'
    else if (Major=4) and (Minor=10) then
      Result:='Windows 98'
    else if (Major=4) and (Minor=90) then
      Result:='Windows ME'
    else
      Result:=Format('OS/2 %d.%d',[Major,Minor])
  end else if Platf=300 then
    Result:=Format('DOS %d.%d',[Major,Minor])
  else if Platf=600 then
    Result:=Format('OSF %d.%d',[Major,Minor])
  else if Platf=700 then
    Result:=Format('VMS %d.%d',[Major,Minor]);
end;

function GetOSFromRegistry;
var
  hkLM,hkWin: HKEY;
  Buffer: array[0..255] of Char;
  n: Cardinal;
begin
  n:=SizeOf(Buffer);
  Result:='';
  if RegConnectRegistry(PChar('\\'+AHost),HKEY_LOCAL_MACHINE,hkLM)<>ERROR_SUCCESS then
    Exit;
  if RegOpenKeyEx(hkLM,'SOFTWARE\Microsoft\Windows NT\CurrentVersion',0,KEY_READ,hkWin)=ERROR_SUCCESS then begin
    RegQueryValueEx(hkWin,'ProductName',nil,nil,PBYTE(@Buffer),@n);
    RegCloseKey(hkWin);
    if (n>0) then
      Result:=Trim(string(Buffer));
    Result:=StringReplace(Result,'Microsoft ','',[rfIgnorecase]);
  end;
  RegCloseKey(hkLM);
end;

function GetMachineNameFromRegistry;
var
  hkLM,hkWin: HKEY;
  Buffer: array[0..255] of Char;
  n: Cardinal;
begin
  n:=SizeOf(Buffer);
  Result:='';
  if RegConnectRegistry(PChar('\\'+AHost),HKEY_LOCAL_MACHINE,hkLM)<>ERROR_SUCCESS then
    Exit;
  if RegOpenKeyEx(hkLM,'SYSTEM\CurrentControlSet\Control\ComputerName\ActiveComputerName',0,KEY_READ,hkWin)=ERROR_SUCCESS then begin
    RegQueryValueEx(hkWin,'ComputerName',nil,nil,PBYTE(@Buffer),@n);
    RegCloseKey(hkWin);
    Result:=string(Buffer);
  end;
  RegCloseKey(hkLM);
end;

function GetDataFromRegistry(AHost: string; var AOS,AMachine,ACPU,ABIOS,AModel,AVendor: string; var AID: TDateTime): Boolean;
var
  hkLM,hk,hk1,hk2: HKEY;
  Buffer: array[0..255] of Char;
  i,j,n: Cardinal;
  s1,s2: string;
begin
  n:=SizeOf(Buffer);
  Result:=False;
  AMachine:='';
  AID:=0;
  AOS:='';
  ACPU:='';
  ABIOS:='';
  AModel:='';
  AVendor:='';
  if RegConnectRegistry(PChar('\\'+AHost),HKEY_LOCAL_MACHINE,hkLM)<>ERROR_SUCCESS then
    Exit;
  if RegOpenKeyEx(hkLM,'SYSTEM\CurrentControlSet\Control\ComputerName\ActiveComputerName',0,KEY_READ,hk)=ERROR_SUCCESS then begin
    RegQueryValueEx(hk,'ComputerName',nil,nil,PBYTE(@Buffer),@n);
    RegCloseKey(hk);
    AMachine:=string(Buffer);
    Result:=True;
  end;
  if RegOpenKeyEx(hkLM,'SOFTWARE\Microsoft\Windows NT\CurrentVersion',0,KEY_READ,hk)=ERROR_SUCCESS then begin
    j:=0;
    n:=sizeof(Cardinal);
    RegQueryValueEx(hk,'InstallDate',nil,nil,@j,@n);
    if (n>0) then
      AID:=UTCToDateTime(j);
    Buffer[0]:=#0;
    n:=SizeOf(Buffer);
    RegQueryValueEx(hk,'ProductName',nil,nil,PBYTE(@Buffer),@n);
    if (n>0) then
      AOS:=Trim(string(Buffer));
    Buffer[0]:=#0;
    n:=SizeOf(Buffer);
    RegQueryValueEx(hk,'CSDVersion',nil,nil,PBYTE(@Buffer),@n);
    RegCloseKey(hk);
    if (n>0) then
      AOS:=Trim(AOS+' '+string(Buffer));
    Result:=True;
  end;
  if (AOS='') and (RegOpenKeyEx(hkLM,'SOFTWARE\Microsoft\Windows\CurrentVersion',0,KEY_READ,hk)=ERROR_SUCCESS) then begin
    Buffer[0]:=#0;
    n:=SizeOf(Buffer);
    RegQueryValueEx(hk,'Version',nil,nil,PBYTE(@Buffer),@n);
    if (n>0) then
      AOS:=Trim(string(Buffer));
    Buffer[0]:=#0;
    n:=SizeOf(Buffer);
    RegQueryValueEx(hk,'SubVersionNumber',nil,nil,PBYTE(@Buffer),@n);
    RegCloseKey(hk);
    if (n>0) then
      AOS:=Trim(AOS+' '+string(Buffer));
    Result:=True;
  end;
  if RegOpenKeyEx(hkLM,'HARDWARE\DESCRIPTION\System\CentralProcessor\0',0,KEY_READ,hk)=ERROR_SUCCESS then begin
    Buffer[0]:=#0;
    n:=SizeOf(Buffer);
    RegQueryValueEx(hk,'ProcessorNameString',nil,nil,PBYTE(@Buffer),@n);
    if (n>0) then
      ACPU:=Trim(string(Buffer));
    if ACPU='' then begin
      Buffer[0]:=#0;
      n:=SizeOf(Buffer);
      RegQueryValueEx(hk,'Identifier',nil,nil,PBYTE(@Buffer),@n);
      RegCloseKey(hk);
      if (n>0) then
        ACPU:=Trim(string(Buffer));
    end;
    Result:=True;
  end;
  if RegOpenKeyEx(hkLM,'HARDWARE\DESCRIPTION\System\BIOS',0,KEY_READ,hk)=ERROR_SUCCESS then begin
    Buffer[0]:=#0;
    n:=SizeOf(Buffer);
    RegQueryValueEx(hk,'BIOSVendor',nil,nil,PBYTE(@Buffer),@n);
    if (n>0) then
      ABIOS:=Trim(string(Buffer));
    Buffer[0]:=#0;
    n:=SizeOf(Buffer);
    RegQueryValueEx(hk,'BIOSVersion',nil,nil,PBYTE(@Buffer),@n);
    if (n>0) then
      ABIOS:=ABIOS+' '+Trim(string(Buffer));
    Buffer[0]:=#0;
    n:=SizeOf(Buffer);
    RegQueryValueEx(hk,'SystemProductName',nil,nil,PBYTE(@Buffer),@n);
    if (n>0) then
      AModel:=Trim(string(Buffer));
    Buffer[0]:=#0;
    n:=SizeOf(Buffer);
    RegQueryValueEx(hk,'SystemManufacturer',nil,nil,PBYTE(@Buffer),@n);
    if (n>0) then
      AVendor:=Trim(string(Buffer));
    Result:=True;
  end;
  if (ACPU='') and (RegOpenKeyEx(hkLM,'SYSTEM\CurrentControlSet\Enum\ACPI',0,KEY_READ,hk)=ERROR_SUCCESS) then begin
    i:=0;
    while (RegEnumKeyEx(hk,i,@Buffer,n,nil,nil,nil,nil)=ERROR_SUCCESS) and (ACPU='') do begin
      s1:=string(Buffer);
      Buffer[0]:=#0;
      n:=SizeOf(Buffer);
      if RegOpenKeyEx(hk,PChar(s1),0,KEY_READ,hk1)=ERROR_SUCCESS then begin
        j:=0;
        while (RegEnumKeyEx(hk1,j,@Buffer,n,nil,nil,nil,nil)=ERROR_SUCCESS) and (ACPU='') do begin
          s2:=string(Buffer);
          Buffer[0]:=#0;
          n:=SizeOf(Buffer);
          if RegOpenKeyEx(hk1,PChar(s2),0,KEY_READ,hk2)=ERROR_SUCCESS then begin
            Buffer[0]:=#0;
            n:=SizeOf(Buffer);
            RegQueryValueEx(hk2,'Class',nil,nil,PBYTE(@Buffer),@n);
            if SameText(string(Buffer),'Processor') then begin
              n:=SizeOf(Buffer);
              Buffer[0]:=#0;
              RegQueryValueEx(hk2,'FriendlyName',nil,nil,PBYTE(@Buffer),@n);
              ACPU:=string(Buffer);
              Break;
            end;
            n:=SizeOf(Buffer);
          end;
          RegCloseKey(hk2);
          Inc(j);
        end;
      end;
      RegCloseKey(hk1);
      Inc(i);
    end;
    RegCloseKey(hk);
    Result:=True;
  end;
  RegCloseKey(hkLM);
end;

function ConnectIPC(AHost,AUser,APwd: string): integer;
var
  nr: TNetResource;
begin
  nr.dwScope:=RESOURCE_GLOBALNET;
  nr.dwType:=RESOURCETYPE_ANY;
  nr.dwUsage:=RESOURCEUSAGE_CONNECTABLE;
  nr.dwDisplayType:=RESOURCEDISPLAYTYPE_GENERIC;
  nr.lpRemoteName:=PChar(Format('\\%s\IPC$',[AHost]));
  nr.lpLocalName:=nil;
  nr.lpProvider:=nil;
  Result:=WNetAddConnection2(nr,PChar(APwd),PChar(AUser),0);
end;

procedure DisconnectIPC(AHost: string);
begin
  WNetCancelConnection2(PChar(Format('\\%s\IPC$',[AHost])),0,True);
end;

function FormatCPUName(const AName: string): string;
begin
  Result:=AName;
  Result:=StringReplace(Result,'(R)','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'(TM)',' ',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Genuine','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Procesor','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Processor','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Technology','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'CPU','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'@','',[rfReplaceAll,rfIgnoreCase]);
  {i:=Pos('CPU',Result);
  if i>0 then
    Delete(Result,i,255);
  i:=Pos('processor',Result);
  if i>0 then
    Delete(Result,i,255);
  i:=Pos('@',Result);
  if i>0 then
    Delete(Result,i,255);}
  Result:=Trim(Result);
  Result:=StripSpaces(Result);
end;

function FormatOSName(const AName: string): string;
begin
  Result:=AName;
  Result:=StringReplace(Result,'Service Pack ','SP',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Standard','Std',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Professional','Pro',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Enterprise','Ent',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Edition','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'(R)','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'NULL','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'®','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'(TM)',' ',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Microsoft','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Seven','7',[rfReplaceAll,rfIgnoreCase]);
  Result:=Trim(StripSpaces(Result));
end;

procedure GetDomainServerList(const ADomain: string; AList: TStringList);
var
  pBuffer: Pointer;
  pWork: PSERVER_INFO_100;
  dwEntriesRead,h,dwTotalEntries: Cardinal;
  i: integer;
  dwResult: NET_API_STATUS;
begin
  AList.Clear;

  dwResult:=NetServerEnum(nil,100,pBuffer,Cardinal(MAX_PREFERRED_LENGTH),
                            dwEntriesRead,dwTotalEntries,SV_TYPE_DOMAIN_ENUM,
                            PWideChar(ADomain),h);

  if dwResult=NERR_SUCCESS then begin
    try
      pWork:=pBuffer;
      for i:=1 to dwEntriesRead do begin
        AList.Add(pWork.sv100_name);
        inc(pWork);
      end;
    finally
      NetApiBufferFree(pBuffer);
    end;
  end;
end;

function Ping(const AIP: TIPv4; ATTL,ARetries: byte; ATimeout: Cardinal): integer;
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
          NumResponses:=IcmpSendEcho(IcmpFile,IPv4ToInAddr(AIP),@SendData,SizeOf(SendData),@ioi,ReplyBuffer,ReplySize,ATimeout);
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

function Ping(const AIP: string; ATTL,ARetries: byte; ATimeout: Cardinal): integer;
begin
  Result:=Ping(StrToIPv4(AIP),ATTL,ARetries,ATimeout);
end;

{ TNetObject }

procedure TNetObject.Clear;
begin
  ResetMemory(Self,SizeOf(Self));
  PingTime:=-1;
  ARPTime:=-1;
  NBTime:=-1;
  PSTime:=-1;
end;

function TNetObject.UpdateFrom(ARecord: TNetObject): Boolean;
begin
  Result:=False;
  Self.Timestamp:=ARecord.Timestamp;
  Self.ElapsedTime:=ARecord.ElapsedTime;
  Self.SNMP:=ARecord.SNMP;
  Self.WMI:=ARecord.WMI;
  Self.ALive:=ARecord.ALive;
  Self.Flag:=ARecord.Flag;
  Self.PingTime:=ARecord.PingTime;
  Self.ARPTime:=ARecord.ARPTime;
  Self.NBTime:=ARecord.NBTime;
  Self.PSTime:=ARecord.PSTime;
  if (ARecord.IPv4.Value>0) and not SameIPv4(Self.IPv4,ARecord.IPv4) then begin
    Self.IPv4:=ARecord.IPv4;
    Self.IPv4Address:=ARecord.IPv4Address;
    Result:=True;
  end;
  if not IPv6IsNull(ARecord.IPv6) and not SameIPv6(Self.IPv6,ARecord.IPv6) then begin
    Self.IPv6:=ARecord.IPv6;
    Self.IPv6Address:=ARecord.IPv6Address;
    Result:=True;
  end;
  if (ARecord.Name<>'') and not SameText(Self.Name,ARecord.Name) then begin
    Self.Name:=ARecord.Name;
    Result:=True;
  end;
  if (ARecord.MachineName<>'') and not SameText(Self.MachineName,ARecord.MachineName) then begin
    Self.MachineName:=ARecord.MachineName;
    Result:=True;
  end;
  if (ARecord.Domain<>'') and not SameText(Self.Domain,ARecord.Domain) then begin
    Self.Domain:=ARecord.Domain;
    Result:=True;
  end;
  if (ARecord.User<>'') and not SameText(Self.User,ARecord.User) then begin
    Self.User:=ARecord.User;
    Result:=True;
  end;
  if (ARecord.OS<>'') and not SameText(Self.OS,ARecord.OS) then begin
    Self.OS:=ARecord.OS;
    Result:=True;
  end;
  if (ARecord.MACAddress<>'') and not SameText(Self.MACAddress,ARecord.MACAddress) then begin
    Self.MACAddress:=ARecord.MACAddress;
    Result:=True;
  end;
  if (ARecord.MACVendor<>'') and not SameText(Self.MACVendor,ARecord.MACVendor) then begin
    Self.MACVendor:=ARecord.MACVendor;
    Result:=True;
  end;
  if (ARecord.Desc<>'') and not SameText(Self.Desc,ARecord.Desc) then begin
    Self.Desc:=ARecord.Desc;
    Result:=True;
  end;
  if (ARecord.Model<>'') and not SameText(Self.Model,ARecord.Model) then begin
    Self.Model:=ARecord.Model;
    Result:=True;
  end;
  if (ARecord.BIOS<>'') and not SameText(Self.BIOS,ARecord.BIOS) then begin
    Self.BIOS:=ARecord.BIOS;
    Result:=True;
  end;
  if (ARecord.CPU<>'') and not SameText(Self.CPU,ARecord.CPU) then begin
    Self.CPU:=ARecord.CPU;
    Result:=True;
  end;
  if (ARecord.UUID<>'') and not SameText(Self.UUID,ARecord.UUID) then begin
    Self.UUID:=ARecord.UUID;
    Result:=True;
  end;
  if (ARecord.IDNumber<>'') and not SameText(Self.IDNumber,ARecord.IDNumber) then begin
    Self.IDNumber:=ARecord.IDNumber;
    Result:=True;
  end;
  if (ARecord.InstallDate<>0) and not SameDatetime(Self.InstallDate,ARecord.InstallDate) then begin
    Self.InstallDate:=ARecord.InstallDate;
    Result:=True;
  end;
  if (Self.SystemUpTime=0) and (ARecord.SystemUpTime>0) then
    Result:=True;
  if (ARecord.SystemUpTime>0) then
    Self.SystemUpTime:=ARecord.SystemUpTime;
  if (Self.RemoteTime=0) and (ARecord.RemoteTime>0) then
    Result:=True;
  if (ARecord.RemoteTime>0) then
    Self.RemoteTime:=ARecord.RemoteTime;
  if (ARecord.Flags>0) and (Self.Flags<>ARecord.Flags) then begin
    Self.Flags:=ARecord.Flags;
    Result:=True;
  end;
  if (ARecord.CPUCount>0) and (Self.CPUCount<>ARecord.CPUCount) then begin
    Self.CPUCount:=ARecord.CPUCount;
    Result:=True;
  end;
  if (ARecord.CPUFreq>0) and (Self.CPUFreq<>ARecord.CPUFreq) then begin
    Self.CPUFreq:=ARecord.CPUFreq;
    Result:=True;
  end;
  if (ARecord.Memory>0) and (Self.Memory<>ARecord.Memory) then begin
    Self.Memory:=ARecord.Memory;
    Result:=True;
  end;
  if (ARecord.TCPPorts<>'') and not SameText(Self.TCPPorts,ARecord.TCPPorts) then begin
    Self.TCPPorts:=ARecord.TCPPorts;
    Result:=True;
  end;
  if (ARecord.Contact<>'') and not SameText(Self.Contact,ARecord.Contact) then begin
    Self.Contact:=ARecord.Contact;
    Result:=True;
  end;
  if (ARecord.Location<>'') and not SameText(Self.Location,ARecord.Location) then begin
    Self.Location:=ARecord.Location;
    Result:=True;
  end;
  if (ARecord.Vendor<>'') and not SameText(Self.Vendor,ARecord.Vendor) then begin
    Self.Vendor:=ARecord.Vendor;
    Result:=True;
  end;
end;

initialization
  ARPTable:=TThreadStringList.Create;
  AddrTable:=TThreadStringList.Create;
finalization
  ARPTable.Free;
  AddrTable.Free;
end.
