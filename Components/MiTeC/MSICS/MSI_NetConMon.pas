{*******************************************************}
{       MiTeC System Information Component Suite        }
{        Network Connection Monitor Thread              }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}


{$INCLUDE Compilers.inc}


unit MSI_NetConMon;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, System.SyncObjs,
     {$ELSE}
     Windows, SysUtils, Classes, SyncObjs,
     {$ENDIF}
     MSI_Defs, MiTeC_Routines, MiTeC_IpHlpAPI, MiTeC_Ws2_32;

const
  AF_INET = 2;
  AF_INET6 = 23;


type
  TNetworkAddress = record
    IPv4: Cardinal;
    IPv6: TIPv6Byte;
  end;

  TNetConRecord = record
    PID: Cardinal;
    ProcessName,
    LocalAddressString,
    RemoteAddressString,
    LocalHostname,
    RemoteHostname: string;
    LocalAddress,
    RemoteAddress: TNetworkAddress;
    LocalPort,
    RemotePort: Cardinal;
    Protocol: string;
    Typ: integer;
    State: Cardinal;
    VersionInfo: TVersionInfo;
    _ImageIndex: integer;
  end;
  PNetConRecord = ^TNetConRecord;

  TNetConnections = array of TNetConRecord;

  TNetConMonThread = class;

  TNetConMonNotifyEvent = procedure(Sender: TNetConMonThread) of object;

  TNetConMonThread = class(TThread)
  {$IFDEF BDS3PLUS}
  strict private
    class var FInstance: TNetConMonThread;
    class var InternalLock: TCriticalSection;
  {$ENDIF}
  private
    FOnInterval: TNetConMonNotifyEvent;
    FInterval: Cardinal;
    FList: TList;
    FAutoSuspend: Boolean;
    procedure DoSync;
    function GetInterval: Cardinal;
    function GetOnInterval: TNetConMonNotifyEvent;
    procedure SetInterval(const Value: Cardinal);
    procedure SetOnInterval(const Value: TNetConMonNotifyEvent);

    procedure RefreshData;
    function GetRecCount: Integer;
    function GetAutoSuspend: Boolean;
    procedure SetAutoSuspend(const Value: Boolean);
  protected
    constructor CreateNetConMon;
    procedure Execute; override;
  public
    class function Create: TNetConMonThread;
    destructor Destroy; override;

    procedure GetRecord(AIndex: Integer; var ARecord: TNetConRecord);
    procedure GetList(AList: TList);
    procedure Clear;
    procedure SetProcess(AIndex: integer; const AName: string; AVersionInfo: TVersionInfo; AImageIndex: integer);
    procedure SetLocalHostname(AIndex: integer; const AName: string);
    procedure SetRemoteHostname(AIndex: integer; const AName: string);
    procedure GetProcessConnections(APID: Cardinal; var AList: TNetConnections);

    property AutoSuspend: Boolean read GetAutoSuspend write SetAutoSuspend;
    property RecordCount: Integer read GetRecCount;
    property Interval: Cardinal read GetInterval write SetInterval;
    property OnInterval: TNetConMonNotifyEvent read GetOnInterval write SetOnInterval;
  end;

  function IsAddressNull(AAddress: TNetworkAddress): Boolean;
  procedure ZeroNetAddress(var A: TNetworkAddress);

implementation

uses {$IFDEF RAD9PLUS}
     {$IFDEF TRIAL}VCL.Dialogs,{$ENDIF} WinApi.WinSock,
     {$ELSE}
     {$IFDEF TRIAL}Dialogs,{$ENDIF} Winsock,
     {$ENDIF}
     MiTeC_NativeDefs, MiTeC_NativeAPI, MiTeC_IPTypes;

{$IFNDEF BDS3PLUS}
var
  InternalLock: TCriticalSection;
  FInstance: TNetConMonThread = nil;
{$ENDIF}

function IsAddressNull(AAddress: TNetworkAddress): Boolean;
var
  i,c: Integer;
begin
  c:=0;
  for i:=0 to High(AAddress.IPv6) do
    c:=c+AAddress.IPv6[i];
  Result:=(AAddress.IPv4=0) and (c=0);
end;

procedure ZeroNetAddress(var A: TNetworkAddress);
var
  i: Integer;
begin
  A.IPv4:=0;
  for i:=0 to High(A.IPv6) do
    A.IPv6[i]:=0;
end;

{ TNetConMonThread }

procedure TNetConMonThread.Clear;
begin
  InternalLock.Enter;
  try
    FList.Clear;
  finally
    InternalLock.Leave;
  end;
end;

class function TNetConMonThread.Create: TNetConMonThread;
begin
  {$IFDEF TRIAL}
  if not RunFromIDE then
    MessageDlg('TNetConMonThread'+sLineBreak+cCompName+sLineBreak+cCopyright,mtInformation,[mbOK],0);
  {$ENDIF}
  if not Assigned(InternalLock) then
    InternalLock:=TCriticalSection.Create;
  InternalLock.Enter;
  try
    if (FInstance=nil) then
      FInstance:=CreateNetConMon;
    Result:=FInstance;
  finally
    InternalLock.Leave;
  end;
end;

constructor TNetConMonThread.CreateNetConMon;
begin
  inherited Create(True);
  FList:=TList.Create;
  FreeOnTerminate:=False;
  FAutoSuspend:=False;
  FInterval:=1000;
end;

destructor TNetConMonThread.Destroy;
var
  i: Integer;
begin
  FOnInterval:=nil;
  for i:=0 to FList.Count-1 do
    Dispose(PNetConRecord(FList[i]));
  FList.Clear;
  FList.Free;
  FreeAndNil(InternalLock);
  inherited;
  FInstance:=nil;
end;

procedure TNetConMonThread.DoSync;
begin
  if Assigned(FOnInterval) then
    FOnInterval(Self);
end;

procedure TNetConMonThread.Execute;
var
  se: TSimpleEvent;
begin
  se:=TSimpleEvent.Create{$IFDEF BDS35PLUS}(nil,False,False,''){$ENDIF};
  try
    while not Terminated do begin
      RefreshData;


      if Assigned(FOnInterval) and not Terminated then
        Synchronize(DoSync);

      if not Terminated then begin
        if FAutoSuspend and not Suspended then
          Suspended:=True
        else
          se.WaitFor(FInterval);
      end;
    end;
  finally
    se.Free;
  end;
end;

function TNetConMonThread.GetAutoSuspend: Boolean;
begin
  InternalLock.Enter;
  try
    Result:=FAutoSuspend;
  finally
    InternalLock.Leave;
  end;
end;

function TNetConMonThread.GetInterval: Cardinal;
begin
  InternalLock.Enter;
  try
    Result:=FInterval;
  finally
    InternalLock.Leave;
  end;
end;

procedure TNetConMonThread.GetList(AList: TList);
var
  i: Integer;
  p: PNetConRecord;
begin
  InternalLock.Enter;
  try
    AList.Clear;
    AList.Capacity:=FList.Capacity;
    for i:=0 to FList.Count-1 do begin
      new(p);
      p^:=PNetConRecord(FList[i])^;
      AList.Add(p);
    end;
  finally
    InternalLock.Leave;
  end;
end;

function TNetConMonThread.GetOnInterval: TNetConMonNotifyEvent;
begin
  InternalLock.Enter;
  try
    Result:=FOnInterval;
  finally
    InternalLock.Leave;
  end;
end;

procedure TNetConMonThread.GetProcessConnections(APID: Cardinal;
  var AList: TNetConnections);
var
  r: PNetConRecord;
begin
  InternalLock.Enter;
  try
    Finalize(AList);
    for r in FList do
      if r.PID=APID then begin
        SetLength(AList,Length(AList)+1);
        AList[High(AList)]:=r^;
      end;
  finally
    InternalLock.Leave;
  end;
end;

function TNetConMonThread.GetRecCount: Integer;
begin
  InternalLock.Enter;
  try
    Result:=FList.Count;
  finally
    InternalLock.Leave;
  end;
end;

procedure TNetConMonThread.GetRecord(AIndex: Integer;
  var ARecord: TNetConRecord);
begin
  InternalLock.Enter;
  try
    ResetMemory(ARecord,SizeOf(ARecord));
    if AIndex<FList.Count then
      ARecord:=PNetConRecord(FList[AIndex])^;
  finally
    InternalLock.Leave;
  end;
end;

procedure TNetConMonThread.RefreshData;
var
  buf: Pointer;
  n: Cardinal;
  i: Integer;
  pTcpTable: PMIB_TCPTABLE_OWNER_PID;
  pUdpTable: PMIB_UDPTABLE_OWNER_PID;
  pTcp6Table: PMIB_TCP6TABLE_OWNER_PID;
  pUdp6Table: PMIB_UDP6TABLE_OWNER_PID;
  r: PNetConRecord;
  IpAddr: in_addr;
  p: array[0..46] of char;
begin
  InternalLock.Enter;
  try
    for i:=0 to FList.Count-1 do
      Dispose(PNetConRecord(FList[i]));
    FList.Clear;

    n:=0;
    GetExtendedTcpTable(nil,@n,False,AF_INET,TCP_TABLE_OWNER_PID_ALL,0);
    buf:=AllocMem(n);
    try
      if GetExtendedTcpTable(buf,@n,True,AF_INET,TCP_TABLE_OWNER_PID_ALL,0)=NO_ERROR then begin
        pTcpTable:=buf;
        for i:=0 to pTcpTable.dwNumEntries-1 do begin
          new(r);
          r.PID:=pTcpTable.Table[i].dwOwningPid;
          r.LocalPort:=ntohs(pTcpTable.Table[i].dwLocalPort);
          r.RemotePort:=ntohs(pTcpTable.Table[i].dwRemotePort);
          r.LocalAddress.IPv4:=pTcpTable.Table[i].dwLocalAddr;
          IpAddr.S_addr:=pTcpTable.Table[i].dwLocalAddr;
          r.LocalAddressString:=IPv4ToStr(InAddrToIPv4(IpAddr));
          r.RemoteAddress.IPv4:=pTcpTable.Table[i].dwRemoteAddr;
          IpAddr.S_addr:=pTcpTable.Table[i].dwRemoteAddr;
          if pTcpTable.Table[i].dwRemoteAddr>0 then
            r.RemoteAddressString:=IPv4ToStr(InAddrToIPv4(IpAddr))
          else
            r.RemoteAddressString:='';
          r.Protocol:='TCP';
          r.Typ:=AF_INET;
          r.State:=pTcpTable.Table[i].dwState;
          FList.Add(r);
        end;
      end;
    finally
      Freemem(buf);
    end;

    n:=0;
    GetExtendedTcpTable(nil,@n,False,AF_INET6,TCP_TABLE_OWNER_PID_ALL,0);
    buf:=AllocMem(n);
    try
      if GetExtendedTcpTable(buf,@n,True,AF_INET6,TCP_TABLE_OWNER_PID_ALL,0)=NO_ERROR then begin
        pTcp6Table:=buf;
        for i:=0 to pTcp6Table.dwNumEntries-1 do begin
          new(r);
          r.PID:=pTcp6Table.Table[i].dwOwningPid;
          r.LocalPort:=ntohs(pTcp6Table.Table[i].dwLocalPort);
          r.RemotePort:=ntohs(pTcp6Table.Table[i].dwRemotePort);
          r.LocalAddress.IPv6:=pTcp6Table.Table[i].ucLocalAddr;
          r.RemoteAddress.IPv6:=pTcp6Table.Table[i].ucRemoteAddr;
          if Assigned(@RtlIpv6AddressToString) then begin
            RtlIpv6AddressToString(@pTcp6Table.Table[i].ucLocalAddr,@p);
            r.LocalAddressString:=string(p);
            RtlIpv6AddressToString(@pTcp6Table.Table[i].ucRemoteAddr,@p);
            r.RemoteAddressString:=string(p);
          end;
          r.Protocol:='TCP';
          r.Typ:=AF_INET6;
          r.State:=pTcp6Table.Table[i].dwState;
          FList.Add(r);
        end;
      end;
    finally
      Freemem(buf);
    end;

    n:=0;
    GetExtendedUdpTable(nil,@n,False,AF_INET,UDP_TABLE_OWNER_PID,0);
    buf:=AllocMem(n);
    try
      if GetExtendedUdpTable(buf,@n,True,AF_INET,UDP_TABLE_OWNER_PID,0)=NO_ERROR then begin
        pUdpTable:=buf;
        for i:=0 to pUdpTable.dwNumEntries-1 do begin
          new(r);
          r.LocalAddress.IPv4:=pUdpTable.Table[i].dwLocalAddr;
          IpAddr.S_addr:=pUdpTable.Table[i].dwLocalAddr;
          r.LocalAddressString:=IPv4ToStr(InAddrToIPv4(IpAddr));
          r.PID:=pUdpTable.Table[i].dwOwningPid;
          r.LocalPort:=ntohs(pUdpTable.Table[i].dwLocalPort);
          r.Protocol:='UDP';
          r.Typ:=AF_INET;
          r.RemoteAddressString:='';
          r.RemotePort:=0;
          r.State:=0;
          ZeroNetAddress(r.RemoteAddress);
          FList.Add(r);
        end;
      end;
    finally
      Freemem(buf);
    end;

    n:=0;
    GetExtendedUdpTable(nil,@n,False,AF_INET6,UDP_TABLE_OWNER_PID,0);
    buf:=AllocMem(n);
    try
      if GetExtendedUdpTable(buf,@n,True,AF_INET6,UDP_TABLE_OWNER_PID,0)=NO_ERROR then begin
        pUdp6Table:=buf;
        for i:=0 to pUdp6Table.dwNumEntries-1 do begin
          new(r);
          if Assigned(@RtlIpv6AddressToString) then begin
            RtlIpv6AddressToString(@pUdp6Table.Table[i].ucLocalAddr,@p);
            r.LocalAddressString:=string(p);
          end;
          r.PID:=pUdp6Table.Table[i].dwOwningPid;
          r.LocalAddress.IPv6:=pUdp6Table.Table[i].ucLocalAddr;
          r.LocalPort:=ntohs(pUdp6Table.Table[i].dwLocalPort);
          r.Protocol:='UDP';
          r.Typ:=AF_INET6;
          r.RemoteAddressString:='';
          r.RemotePort:=0;
          ZeroNetAddress(r.RemoteAddress);
          r.State:=0;
          FList.Add(r);
        end;
      end;
    finally
      Freemem(buf);
    end;
  finally
    InternalLock.Leave;
  end;
end;

procedure TNetConMonThread.SetAutoSuspend(const Value: Boolean);
begin
  InternalLock.Enter;
  try
    FAutoSuspend:=Value;
  finally
    InternalLock.Leave;
  end;
end;

procedure TNetConMonThread.SetInterval(const Value: Cardinal);
begin
  InternalLock.Enter;
  try
    FInterval:=Value;
  finally
    InternalLock.Leave;
  end;
end;

procedure TNetConMonThread.SetLocalHostname(AIndex: integer;
  const AName: string);
begin
  InternalLock.Enter;
  try
    if AIndex<FList.Count then
      PNetConRecord(FList[AIndex])^.LocalHostname:=AName;
  finally
    InternalLock.Leave;
  end;
end;

procedure TNetConMonThread.SetOnInterval(const Value: TNetConMonNotifyEvent);
begin
  InternalLock.Enter;
  try
    FOnInterval:=Value;
  finally
    InternalLock.Leave;
  end;
end;

procedure TNetConMonThread.SetProcess(AIndex: integer; const AName: string; AVersionInfo: TVersionInfo; AImageIndex: integer);
begin
  InternalLock.Enter;
  try
    if AIndex<FList.Count then begin
      PNetConRecord(FList[AIndex])^.ProcessName:=AName;
      PNetConRecord(FList[AIndex])^.VersionInfo:=AVersionInfo;
      PNetConRecord(FList[AIndex])^._ImageIndex:=AImageIndex;
    end;
  finally
    InternalLock.Leave;
  end;
end;

procedure TNetConMonThread.SetRemoteHostname(AIndex: integer;
  const AName: string);
begin
  InternalLock.Enter;
  try
    if AIndex<FList.Count then
      PNetConRecord(FList[AIndex])^.RemoteHostname:=AName;
  finally
    InternalLock.Leave;
  end;
end;

initialization
  InitIpHlpAPI;
end.

