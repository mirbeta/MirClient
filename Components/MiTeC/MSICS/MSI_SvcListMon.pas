{*******************************************************}
{       MiTeC System Information Component Suite        }
{              Service List Monitor Thread              }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}


{$INCLUDE Compilers.inc}


unit MSI_SvcListMon;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, System.SyncObjs,
     {$ELSE}
     Windows, SysUtils, Classes, SyncObjs,
     {$ENDIF}
     MSI_Defs, MiTeC_Routines, MiTeC_AdvApi;

type
  TServiceTypes = set of TServiceType;

const
  stAll = [svcUnknown, svcKernelDriver, svcFileSystemDriver, svcAdapter, svcRecognizerDriver,
           svcOwnProcess, svcSharedProcess, svcDesktopInteractiveProcess,
           svcOwnInteractiveProcess, svcShareInteractiveProcess];
  stServices = [svcOwnProcess, svcSharedProcess, svcOwnInteractiveProcess, svcShareInteractiveProcess];
  stDrivers = [svcKernelDriver, svcFileSystemDriver, svcAdapter, svcRecognizerDriver];


type
  TServiceRecord = record
    DependOnService,
    DisplayName,
    Name,
    CmdLine,
    ImageName,
    ObjectName,
    Group,
    Description: String;
    StartUp: integer;
    Delayed,
    TriggerStart: Boolean;
    Status: Cardinal;
    Tag,
    ErrCtrl: Integer;
    PID :Cardinal;
    ControlsAccepted: Cardinal;
    Size: Int64;
    _Typ: Cardinal;
    Typ: TServiceType;
    VersionInfo: TVersionInfo;
    New: Boolean;

    _Exists: Boolean;
    _ImageIndex: integer;
    _ForceRefresh: Boolean;
  end;
  PServiceRecord = ^TServiceRecord;

  TSvcListMonThread = class;

  TSvcListMonNotifyEvent = procedure(Sender: TSvcListMonThread) of object;

  TSvcListMonThread = class(TThread)
  private
    InternalLock: TCriticalSection;
    FOnInterval: TSvcListMonNotifyEvent;
    FInterval: Cardinal;
    FList: TList;
    FAutoSuspend: Boolean;
    FTypes: TServiceTypes;
    FII: integer;
    FSD: string;
    procedure DoSync;
    function GetInterval: Cardinal;
    function GetOnInterval: TSvcListMonNotifyEvent;
    procedure SetInterval(const Value: Cardinal);
    procedure SetOnInterval(const Value: TSvcListMonNotifyEvent);

    procedure RefreshData;
    function GetRecCount: Integer;
    function GetTypes: TServiceTypes;
    procedure SetTypes(const Value: TServiceTypes);
    function GetAutoSuspend: Boolean;
    procedure SetAutoSuspend(const Value: Boolean);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetServiceRefresh(AIndex: integer);

    procedure GetRecord(AIndex: Integer; var ARecord: TServiceRecord);
    procedure GetRecordByName(const AName: string; var ARecord: TServiceRecord);
    procedure GetRecordByDisplayName(const AName: string; var ARecord: TServiceRecord);
    procedure GetServiceDependants(const AName: string; AList: TStringList);
    procedure GetList(AList: TList);
    procedure GetServicesByPID(APID: Cardinal; AList: TStringList);
    procedure Clear;
    function GetRecordCount(ATypes: TServiceTypes): Integer;

    property AutoSuspend: Boolean read GetAutoSuspend write SetAutoSuspend;
    property RecordCount: Integer read GetRecCount;
    property Types: TServiceTypes read GetTypes write SetTypes;
    property Interval: Cardinal read GetInterval write SetInterval;
    property OnInterval: TSvcListMonNotifyEvent read GetOnInterval write SetOnInterval;
  end;

implementation

uses {$IFDEF RAD9PLUS}
     {$IFDEF TRIAL}VCL.Dialogs,{$ENDIF}Winapi.WinSvc, WinApi.ShellApi,
     {$ELSE}
     {$IFDEF TRIAL}Dialogs,{$ENDIF}{$IFDEF FPC}JwaWinSvc{$ELSE}WinSvc{$ENDIF}, ShellApi,
     {$ENDIF}
     MiTeC_StrUtils, MiTeC_Datetime;

{ TSvcListMonThread }

procedure TSvcListMonThread.Clear;
var
  i: Integer;
begin
  {$IFDEF TRIAL}
  if not RunFromIDE then
    MessageDlg('TSvcListMonThread'+sLineBreak+cCompName+sLineBreak+cCopyright,mtInformation,[mbOK],0);
  {$ENDIF}
  InternalLock.Enter;
  try
    for i:=0 to FList.Count-1 do
      Dispose(PServiceRecord(FList[i]));
    FList.Clear;
  finally
    InternalLock.Leave;
  end;
end;

constructor TSvcListMonThread.Create;
var
  ShInfo: TSHFileInfo;
begin
  inherited Create(True);
  InternalLock:=TCriticalSection.Create;
  FList:=TList.Create;
  FTypes:=stAll;
  FreeOnTerminate:=False;
  FAutoSuspend:=False;
  FInterval:=1000;
  FII:=0;
  FSD:=GetWinDir;
  if SHGetFileInfo(PChar(IncludeTrailingPathDelimiter(GetSysDir)+'svchost.exe'),0,ShInfo,SizeOf(ShInfo),SHGFI_SYSICONINDEX)>0 then
    FII:=shInfo.iIcon;
end;

destructor TSvcListMonThread.Destroy;
var
  i: Integer;
begin
  FOnInterval:=nil;
  if not Terminated then
    Terminate;
  while not Terminated do
    Sleep(100);
  for i:=0 to FList.Count-1 do
    Dispose(PServiceRecord(FList[i]));
  FList.Clear;
  FList.Free;
  FreeAndNil(InternalLock);
  inherited;
end;

procedure TSvcListMonThread.DoSync;
begin
  if Assigned(FOnInterval) then
    FOnInterval(Self);
end;

procedure TSvcListMonThread.Execute;
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

function TSvcListMonThread.GetAutoSuspend: Boolean;
begin
  InternalLock.Enter;
  try
    Result:=FAutoSuspend;
  finally
    InternalLock.Leave;
  end;
end;

function TSvcListMonThread.GetInterval: Cardinal;
begin
  InternalLock.Enter;
  try
    Result:=FInterval;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSvcListMonThread.GetList(AList: TList);
var
  i: Integer;
  p: PServiceRecord;
begin
  InternalLock.Enter;
  try
    for i:=0 to AList.Count-1 do
      Dispose(PServiceRecord(AList[i]));
    AList.Clear;
    AList.Capacity:=FList.Capacity;
    for i:=0 to FList.Count-1 do begin
      new(p);
      p^:=PServiceRecord(FList[i])^;
      AList.Add(p);
    end;
  finally
    InternalLock.Leave;
  end;
end;

function TSvcListMonThread.GetOnInterval: TSvcListMonNotifyEvent;
begin
  InternalLock.Enter;
  try
    Result:=FOnInterval;
  finally
    InternalLock.Leave;
  end;
end;

function TSvcListMonThread.GetRecCount: Integer;
begin
  InternalLock.Enter;
  try
    Result:=FList.Count;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSvcListMonThread.GetRecord(AIndex: Integer;
  var ARecord: TServiceRecord);
begin
  InternalLock.Enter;
  try
    ResetMemory(ARecord,SizeOf(ARecord));
    if AIndex<FList.Count then
      ARecord:=PServiceRecord(FList[AIndex])^;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSvcListMonThread.GetRecordByDisplayName(const AName: string;
  var ARecord: TServiceRecord);
var
  i: Integer;
begin
  InternalLock.Enter;
  try
    ResetMemory(ARecord,SizeOf(ARecord));
    for i:=0 to FList.Count-1 do
      if SameText(PServiceRecord(FList[i])^.DisplayName,AName) then begin
        ARecord:=PServiceRecord(FList[i])^;
        Break;
      end;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSvcListMonThread.GetRecordByName(const AName: string; var ARecord: TServiceRecord);
var
  i: Integer;
begin
  InternalLock.Enter;
  try
    ResetMemory(ARecord,SizeOf(ARecord));
    for i:=0 to FList.Count-1 do
      if SameText(PServiceRecord(FList[i])^.Name,AName) then begin
        ARecord:=PServiceRecord(FList[i])^;
        Break;
      end;
  finally
    InternalLock.Leave;
  end;
end;

function TSvcListMonThread.GetRecordCount(ATypes: TServiceTypes): Integer;
var
  r: PServiceRecord;
begin
  InternalLock.Enter;
  try
    Result:=0;
    for r in FList do
      if r.Typ in ATypes then
        Inc(Result);
  finally
    InternalLock.Leave;
  end;
end;

procedure TSvcListMonThread.GetServiceDependants(const AName: string;
  AList: TStringList);
var
  i: Integer;
  s: string;
begin
  s:=UpperCase(AName);
  AList.Clear;
  for i:=0 to FList.Count-1 do
    if Pos(s,UpperCase(PServiceRecord(FList[i])^.DependOnService))>0 then
      AList.Add(PServiceRecord(FList[i])^.Name+'='+PServiceRecord(FList[i])^.DisplayName);
end;

procedure TSvcListMonThread.GetServicesByPID(APID: Cardinal;
  AList: TStringList);
var
  i: Integer;
begin
  InternalLock.Enter;
  try
    AList.Clear;
    for i:=0 to FList.Count-1 do begin
      if PServiceRecord(FList[i])^.PID=APID then
        AList.Add(Format('%s=%s',[PServiceRecord(FList[i])^.Name,PServiceRecord(FList[i])^.DisplayName]));
    end;
  finally
    InternalLock.Leave;
  end;
end;

function TSvcListMonThread.GetTypes: TServiceTypes;
begin
  InternalLock.Enter;
  try
    Result:=FTypes;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSvcListMonThread.RefreshData;
const
  cnMaxServices = 4096;
type
  PSvcArray = ^TSvcArray;
  TSvcArray = array[0..511] of TEnumServiceStatus;
var
  i,j,idx: Integer;
  pqsc: {$IFDEF RAD9PLUS}LPQuery_Service_ConfigW{$ELSE}PQueryServiceConfig{$ENDIF};
  ssa: TSvcArray;
  r: PServiceRecord;
  schm,sh: THandle;
  b,c,rh,m: Cardinal;
  p,q: PChar;
  ssp: PByte;
  s: string;
  st: Cardinal;
  ShInfo: TSHFileInfo;
  ForceRefresh: Boolean;
  ss: TServiceStatus;
begin
  InternalLock.Enter;
  try
    for i:=0 to FList.Count-1 do
      PServiceRecord(FList[i])._Exists:=False;

    st:=0;
    if svcKernelDriver in Ftypes then
      st:=st or SERVICE_KERNEL_DRIVER;
    if svcFileSystemDriver in FTypes then
      st:=st or SERVICE_FILE_SYSTEM_DRIVER;
    if svcAdapter in Ftypes then
      st:=st or SERVICE_ADAPTER;
    if svcRecognizerDriver in FTypes then
      st:=st or SERVICE_RECOGNIZER_DRIVER;
    if svcOwnProcess in FTypes then
      st:=st or SERVICE_WIN32_OWN_PROCESS;
    if svcSharedProcess in FTypes then
      st:=st or SERVICE_WIN32_SHARE_PROCESS;
    if svcDesktopInteractiveProcess in FTypes then
      st:=st or SERVICE_INTERACTIVE_PROCESS;
    if svcOwnInteractiveProcess in FTypes then
      st:=st or SERVICE_INTERACTIVE_OWN_PROCESS;
    if svcShareInteractiveProcess in FTypes then
      st:=st or SERVICE_INTERACTIVE_SHARE_PROCESS;
    schm:=OpenSCManager(nil,nil,GENERIC_READ);
    if (schm>0) then
      try
        rh:=0;
        while True do begin
          EnumServicesStatus(schm,st,SERVICE_STATE_ALL,{$IFDEF FPC}@{$ENDIF}ssa[0],SizeOf(ssa),b,c,rh);
          for i:=0 to c-1 do begin
            s:=ssa[i].lpServiceName;
            idx:=-1;
            for j:=0 to FList.Count-1 do begin
              if SameText(PServiceRecord(FList[j]).Name,s) then begin
                idx:=j;
                Break;
              end;
            end;
            if idx=-1 then begin
              new(r);
              r.New:=True;
              r._ImageIndex:=FII;
              FList.Add(r);
              idx:=FList.Count-1;
            end else
              PServiceRecord(FList[idx])^.New:=False;
            with PServiceRecord(FList[idx])^ do begin
              _Exists:=True;
              Name:=s;
              DisplayName:=ssa[i].lpDisplayName;
              Name:=ssa[i].lpServiceName;
              ForceRefresh:=_ForceRefresh or (Status<>ssa[i].ServiceStatus.dwCurrentState) or (ImageName='');
              _ForceRefresh:=False;
              Status:=ssa[i].ServiceStatus.dwCurrentState;

              if ForceRefresh then begin
                sh:=OpenService(schm,ssa[i].lpServiceName,SERVICE_QUERY_CONFIG or SERVICE_QUERY_STATUS);
                if (sh>0) then
                  try
                    ssp:=nil;
                    if Assigned(@QueryServiceConfig2) then begin
                      m:=$100;
                      ssp:=AllocMem(m);
                      try
                        if not QueryServiceConfig2(sh,SERVICE_CONFIG_DESCRIPTION,ssp,m,{$IFNDEF FPC}@{$ENDIF}m) then begin
                          if m>0 then
                            ReallocMem(ssp,m);
                          QueryServiceConfig2(sh,SERVICE_CONFIG_DESCRIPTION,ssp,m,{$IFNDEF FPC}@{$ENDIF}m)
                        end;
                        if m>0 then
                          Description:=string(PChar(PAnsiChar(ssp)+SizeOf(NativeUInt)));
                      finally
                        FreeMem(ssp);
                      end;

                      m:=SizeOf(TServiceDelayedAutoStartInfo);
                      ssp:=AllocMem(m);
                      try
                        if not QueryServiceConfig2(sh,SERVICE_CONFIG_DELAYED_AUTO_START_INFO,ssp,m,{$IFNDEF FPC}@{$ENDIF}m) then begin
                          if m>0 then
                            ReallocMem(ssp,m);
                          QueryServiceConfig2(sh,SERVICE_CONFIG_DELAYED_AUTO_START_INFO,ssp,m,{$IFNDEF FPC}@{$ENDIF}m)
                        end;
                        if m>0 then
                          Delayed:=PServiceDelayedAutoStartInfo(ssp).fDelayedAutostart;
                      finally
                        FreeMem(ssp);
                      end;

                      m:=SizeOf(TServiceTriggerInfo);
                      ssp:=AllocMem(m);
                      try
                        if not QueryServiceConfig2(sh,SERVICE_CONFIG_TRIGGER_INFO,ssp,m,{$IFNDEF FPC}@{$ENDIF}m) then begin
                          if m>0 then
                            ReallocMem(ssp,m);
                          QueryServiceConfig2(sh,SERVICE_CONFIG_TRIGGER_INFO,ssp,m,{$IFNDEF FPC}@{$ENDIF}m)
                        end;
                        if m>0 then
                          TriggerStart:=PServiceTriggerInfo(ssp).cTriggers>0;
                      finally
                        FreeMem(ssp);
                      end;
                    end;

                    QueryServiceConfig(sh,nil,0,m);
                    pqsc:=Allocmem(m);
                    try
                      if QueryServiceConfig(sh,pqsc,m,m) then begin
                        _Typ:=pqsc^.dwServiceType;
                        case pqsc^.dwServiceType of
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
                        StartUp:=pqsc^.dwStartType;
                        ErrCtrl:=pqsc^.dwErrorControl;
                        CmdLine:=pqsc^.lpBinaryPathName;
                        Group:=pqsc^.lpLoadOrderGroup;
                        Tag:=pqsc^.dwTagId;
                        ObjectName:=pqsc^.lpServiceStartName;

                        if QueryServiceStatus(sh,ss) then
                          ControlsAccepted:=ss.dwControlsAccepted;

                        QueryServiceStatusEx(sh,SC_STATUS_PROCESS_INFO,nil,0,m);
                        ssp:=AllocMem(m);
                        try
                          if QueryServiceStatusEx(sh,SC_STATUS_PROCESS_INFO,ssp,m,m) then begin
                            PID:=LPSERVICE_STATUS_PROCESS(ssp).dwProcessId;
                          end;
                        finally
                          Freemem(ssp);
                        end;

                        DependOnService:='';
                        p:=pqsc^.lpDependencies;
                        if p<>nil then
                          while p^<>#0 do begin
                            q:=p;
                            while p^<>#0 do
                              Inc(p);
                            if p>q then
                              DependOnService:=DependOnService+Copy(q,1,p-q)+',';
                            Inc(p);
                          end;
                        SetLength(DependOnService,Length(DependOnService)-1);

                        CmdLine:=FastStringReplace(CmdLine,'\SystemRoot\',FSD);
                        if PosText('system32\',CmdLine)=1 then
                          CmdLine:=FSD+CmdLine;
                        if (ImageName='') or (ForceRefresh) then begin
                          if FileExistsEx(DequoteStr(CmdLine)) then
                            ImageName:=DequoteStr(CmdLine)
                          else
                            ImageName:=ExtractImageName(CmdLine);

                          if ImageName<>'' then begin
                            GetFileVerInfo(ImageName,VersionInfo);
                            Size:=GetFileSize(ImageName);
                            if Size<0 then
                              Size:=0;
                          end;
                          if (SHGetFileInfo(PChar(ImageName),0,ShInfo,SizeOf(ShInfo),SHGFI_SYSICONINDEX)>0) then
                            _ImageIndex:=shInfo.iIcon;
                          if ImageName='' then
                            ImageName:=' ';
                        end;
                      end;
                    finally
                      Freemem(pqsc);
                    end;
                  finally
                    CloseServiceHandle(sh);
                  end;
              end;
            end;
          end;
          if b=0 then
            Break;
        end;
      finally
        CloseServiceHandle(schm);
      end;

    i:=0;
    while i<FList.Count do
      if not PServiceRecord(FList[i])._Exists then begin
        Dispose(PServiceRecord(FList[i]));
        FList.Delete(i);
      end else
        Inc(i);
    FList.Capacity:=FList.Count;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSvcListMonThread.SetAutoSuspend(const Value: Boolean);
begin
  InternalLock.Enter;
  try
    FAutoSuspend:=Value;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSvcListMonThread.SetInterval(const Value: Cardinal);
begin
  InternalLock.Enter;
  try
    FInterval:=Value;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSvcListMonThread.SetOnInterval(const Value: TSvcListMonNotifyEvent);
begin
  InternalLock.Enter;
  try
    FOnInterval:=Value;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSvcListMonThread.SetServiceRefresh(AIndex: integer);
begin
  InternalLock.Enter;
  try
    if AIndex<FList.Count then
      PServiceRecord(FList[AIndex])^._ForceRefresh:=True;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSvcListMonThread.SetTypes(const Value: TServiceTypes);
begin
  InternalLock.Enter;
  try
    FTypes:=Value;
  finally
    InternalLock.Leave;
  end;
end;

initialization

end.




