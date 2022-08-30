{*******************************************************}
{               MiTeC Common Routines                   }
{                   Advanced API                        }
{                                                       }
{                                                       }
{         Copyright (c) by 1997-2019 Michal Mutl        }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_AdvAPI;

{$ALIGN ON}
{$MINENUMSIZE 4}

interface

uses {$IFDEF RAD9PLUS}
     System.Classes, WinAPI.Windows, System.SysUtils, WinAPI.WinSvc;
     {$ELSE}
     Classes, Windows, SysUtils, {$IFDEF FPC}jwawinsvc{$ELSE}WinSvc{$ENDIF};
     {$ENDIF}

const
  SERVICE_CONTROL_TIMEOUT = 20000; //ms

type
  WTSSESSION_NOTIFICATION = record
    cbSize: DWORD;
    dwSessionId: DWORD;
  end;
  PWTSSESSION_NOTIFICATION = ^WTSSESSION_NOTIFICATION;
  TWtsSessionNotification = WTSSESSION_NOTIFICATION;
  PWtsSessionNotification = PWTSSESSION_NOTIFICATION;

  {$IFNDEF RAD7PLUS}
  SC_STATUS_TYPE= (
    SC_STATUS_PROCESS_INFO = 0
  );
  {$ENDIF}

  {$IFNDEF RAD9PLUS}
  LPSERVICE_STATUS_PROCESS = ^SERVICE_STATUS_PROCESS;
  SERVICE_STATUS_PROCESS = record
    dwServiceType:             DWORD;
    dwCurrentState:            DWORD;
    dwControlsAccepted:        DWORD;
    dwWin32ExitCode:           DWORD;
    dwServiceSpecificExitCode: DWORD;
    dwCheckPoint:              DWORD;
    dwWaitHint:                DWORD;
    dwProcessId:               DWORD;
    dwServiceFlags:            DWORD;
  end;

  PSERVICE_TRIGGER_SPECIFIC_DATA_ITEM = ^SERVICE_TRIGGER_SPECIFIC_DATA_ITEM;
  SERVICE_TRIGGER_SPECIFIC_DATA_ITEM = record
    dwDataType:   DWORD;
    cbData:       DWORD;
    pData:        PBYTE;
  end;

  PSERVICE_TRIGGER = ^SERVICE_TRIGGER;
  SERVICE_TRIGGER = record
    dwTriggerType:              DWORD;
    dwAction:                   DWORD;
    pTriggerSubtype:            PGUID;
    cDataItems:                 DWORD;
    pDataItems:  PSERVICE_TRIGGER_SPECIFIC_DATA_ITEM;
  end;

  PSERVICE_TRIGGER_INFO = ^SERVICE_TRIGGER_INFO;
  SERVICE_TRIGGER_INFO = record
    cTriggers: DWORD;
    pTriggers: PSERVICE_TRIGGER;
    pReserved: PBYTE;
  end;
  {$ENDIF}

  LPSERVICE_DELAYED_AUTO_START_INFO = ^SERVICE_DELAYED_AUTO_START_INFO;
  SERVICE_DELAYED_AUTO_START_INFO = record
    fDelayedAutostart: Boolean;
    a1,a2,a3: Byte;
  end;

  TServiceDelayedAutoStartInfo = SERVICE_DELAYED_AUTO_START_INFO;
  TServiceTriggerInfo = SERVICE_TRIGGER_INFO;

  PServiceDelayedAutoStartInfo = LPSERVICE_DELAYED_AUTO_START_INFO;
  PServiceTriggerInfo = PSERVICE_TRIGGER_INFO;

  TServiceType = (svcUnknown, svcKernelDriver, svcFileSystemDriver, svcAdapter, svcRecognizerDriver,
                  svcOwnProcess, svcSharedProcess, svcDesktopInteractiveProcess,
                  svcOwnInteractiveProcess, svcShareInteractiveProcess);

  THandlerFunctionEx = function(dwControl, dwEventType: DWORD; lpEventData, lpContext: Pointer): DWORD; stdcall;

  TRegisterServiceCtrlHandlerEx = function (lpServiceName: PChar; lpHandlerProc: THandlerFunctionEx; lpContext: Pointer): THandle; stdcall;

  TQueryServiceStatusEx = function (hService: SC_HANDLE; InfoLevel: SC_STATUS_TYPE; lpBuffer: PByte;
                                    cbBufSize: DWORD; var pcbBytesNeeded: DWORD): BOOL; stdcall;
  TQueryServiceConfig2 = function (hService: SC_HANDLE; dwInfoLevel: DWORD; lpBuffer: PBYTE; cbBufSize: DWORD; pcbBytesNeeded: PDWORD): BOOL; stdcall;

  TChangeServiceConfig2 = function(hService: SC_HANDLE; dwInfoLevel: DWORD; lpInfo: Pointer): Bool; stdcall;

  TServiceConfig = record
    dwServiceType: DWORD;
    dwStartType: DWORD;
    dwErrorControl: DWORD;
    BinaryPathName: string;
    LoadOrderGroup: string;
    dwTagId: DWORD;
    Dependencies: string;
    ServiceStartName: string;
    DisplayName: string;
  end;

function ServiceGetStatus(const AService :string; const AMachine: string = '') :Cardinal;
function ServiceRunning(const AService :string; const AMachine: string = '') :boolean;
function ServiceStopped(const AService :string; const AMachine: string = '') :boolean;
function ServiceStart(const AService: string; const AArgs :string = ''; ACheck: Boolean = False; const AMachine: string = ''): boolean;
function ServiceStop(const AService :string; ADependants: TStringlist; ACheck: Boolean = False; const AMachine: string = ''): boolean;
function ServiceRestart(const AService: string; ADependants: TStringlist; const AMachine: string = ''): Boolean;
function ServicePause(const AService :string; ACheck: Boolean = False; const AMachine: string = ''): boolean;
function ServiceContinue(const AService :string; ACheck: Boolean = False; const AMachine: string = ''): Boolean;
function ServiceGetKeyName(const AServiceDispName :string; const AMachine: string = '') :string;
function ServiceGetDisplayName(const AServiceKeyName :string; const AMachine: string = '') :string;
function InstallService(const AServiceName, ADisplayName, AFileName: string; AStart: Boolean = false): boolean;
function UninstallService(const AService :string) :Boolean;

function ServiceGetList(const AMachine :string; AServiceType,AServiceState :Cardinal; AList :TStrings): Cardinal;
function ServiceGetConfig(const AMachine,AService :string; var QSC: TServiceConfig) :boolean;

procedure ServiceGetDependants(const AMachine, AService: string; AOnlyRunning, ADisplayNames: boolean; AList: TStringList);


const
  SERVICE_CONTROL_SESSIONCHANGE = $0000000E;

  SERVICE_ACCEPT_SESSIONCHANGE   = $00000080;

  SERVICE_DELETE = $00010000;

  SERVICE_INTERACTIVE_OWN_PROCESS   = $00000110;

  SERVICE_INTERACTIVE_SHARE_PROCESS = $00000120;

  SERVICE_ADAPTER               = $00000004;
  SERVICE_RECOGNIZER_DRIVER     = $00000008;

  SERVICE_CONFIG_DESCRIPTION              = 1;
  SERVICE_CONFIG_FAILURE_ACTIONS          = 2;
  SERVICE_CONFIG_DELAYED_AUTO_START_INFO  = 3;
  SERVICE_CONFIG_FAILURE_ACTIONS_FLAG     = 4;
  SERVICE_CONFIG_SERVICE_SID_INFO         = 5;
  SERVICE_CONFIG_REQUIRED_PRIVILEGES_INFO = 6;
  SERVICE_CONFIG_PRESHUTDOWN_INFO         = 7;
  SERVICE_CONFIG_TRIGGER_INFO             = 8;
  SERVICE_CONFIG_PREFERRED_NODE           = 9;

  cSvcStartup: array[SERVICE_BOOT_START..SERVICE_DISABLED] of string = (
     'Boot',
     'System',
     'Automatic',
     'Manual',
     'Disabled');

  cSvcStatus: array[SERVICE_STOPPED..SERVICE_PAUSED] of string = (
      'Stopped',
      'Start/Pending',
      'Stop/Pending',
      'Running',
      'Continue/Pending',
      'Pause/Pending',
      'Paused');

  cSvcErrorControl: array[0..1] of string = (
      'Ignore',
      'Normal');

  cSvcType: array[TServiceType] of string = (
      'Unknown',
      'Kernel Driver',
      'File System Driver',
      'Adapter',
      'Recognizer Driver',
      'Own Process',
      'Shared Process',
      'Desktop Interactive Process',
      'Own Interactive Process',
      'Share Interactive Process');

var
  RegisterServiceCtrlHandlerEx: TRegisterServiceCtrlHandlerEx = nil;
  QueryServiceStatusEx: TQueryServiceStatusEx = nil;
  QueryServiceConfig2: TQueryServiceConfig2 = nil;
  ChangeServiceConfig2: TChangeServiceConfig2 = nil;

implementation

uses {$IFDEF RAD12PLUS}System.AnsiStrings,{$ENDIF}
  MiTeC_StrUtils, MiTeC_Windows;

function ServiceGetStatus(const AService :string; const AMachine: string = '') : Cardinal;
var
  schm,schs: SC_Handle;
  ss: TServiceStatus;
  dwStat: Cardinal;
begin
  dwStat:=Cardinal(-1);
  schm:=OpenSCManager(PChar(AMachine),nil,SC_MANAGER_CONNECT);
  if (schm>0) then
    try
      schs:=OpenService(schm,PChar(AService),SERVICE_QUERY_STATUS);
      if (schs>0) then
        try
          if(QueryServiceStatus(schs,ss)) then
            dwStat:=ss.dwCurrentState;
        finally
          CloseServiceHandle(schs);
        end;
    finally
      CloseServiceHandle(schm);
    end;
  Result:=dwStat;
end;


function ServiceRunning(const AService :string; const AMachine: string = '') : boolean;
begin
  Result:=SERVICE_RUNNING=ServiceGetStatus(AMachine, AService );
end;

function ServiceStopped(const AService :string; const AMachine: string = '') : boolean;
begin
  Result:=SERVICE_STOPPED=ServiceGetStatus(AMachine, AService );
end;

function ServiceStart(const AService: string; const AArgs :string = ''; ACheck: Boolean = False; const AMachine: string = ''): boolean;
var
  schm, schs: SC_Handle;
  ss: TServiceStatus;
  psTemp: PChar;
  cp,n,wc,wh: Cardinal;
  t: UInt64;
  i: integer;
begin
  Result:=False;
  psTemp:=nil;
  ss.dwCurrentState:=Cardinal(-1);
  schm:=OpenSCManager(PChar(AMachine),nil,SC_MANAGER_CONNECT);
  if (schm>0) then begin
    schs:=OpenService(schm,PChar(AService),SERVICE_START or SERVICE_QUERY_STATUS);
    if (schs>0) then begin
      n:=0;
      if AArgs<>'' then begin
        {$IFDEF UNICODE}
        psTemp:=StrAlloc((Length(AArgs)+1)*2);
        {$ELSE}
        psTemp:=StrAlloc(Length(AArgs)+1);
        {$ENDIF}
        strpcopy(psTemp,AArgs);
        n:=Length(AArgs)-Length(TrimAll(AArgs));
        if n=0 then
          n:=1;
      end;
      Result:=StartService(schs,n,psTemp);
      if ACheck then
        if QueryServiceStatus(schs,ss) then begin
          t:=GetTickCount64;
          while (SERVICE_RUNNING<>ss.dwCurrentState) do begin
            cp:=ss.dwCheckPoint;
            if ss.dwWaitHint<1000 then
              wh:=ss.dwWaitHint
            else
              wh:=1000;
            wc:=ss.dwWaitHint div 1000;
            i:=0;
            repeat
              Sleep(wh);
              inc(i);
              QueryServiceStatus(schs,ss);
              if (SERVICE_RUNNING=ss.dwCurrentState) then
                Break;
            until i>=wc;
            if (SERVICE_RUNNING<>ss.dwCurrentState) then
              if not QueryServiceStatus(schs,ss) or (ss.dwCheckPoint<cp) or (GetTickCount64-t>SERVICE_CONTROL_TIMEOUT) then
                Break;
          end;
          Result:=SERVICE_RUNNING=ss.dwCurrentState;
        end;
      CloseServiceHandle(schs);
    end;
    CloseServiceHandle(schm);
  end;
  if psTemp<>nil then
    StrDispose(psTemp);
end;

function _ServiceStop(const AService :string; ACheck: Boolean = False; AMachine: string = ''): boolean;
var
  schm, schs: SC_Handle;
  ss: TServiceStatus;
  cp,wh,wc: Cardinal;
  t: UInt64;
  i: integer;
begin
  Result:=False;
  schm:=OpenSCManager(PChar(AMachine),nil,SC_MANAGER_CONNECT);
  if (schm>0) then begin
    schs:=OpenService(schm,PChar(AService),SERVICE_STOP or SERVICE_QUERY_STATUS);
    if (schs>0)then begin
      Result:=ControlService(schs,SERVICE_CONTROL_STOP,ss);
      if ACheck then
        if ACheck then
        if QueryServiceStatus(schs,ss) then begin
          t:=GetTickCount64;
          while (SERVICE_STOPPED<>ss.dwCurrentState) do begin
            cp:=ss.dwCheckPoint;
            if ss.dwWaitHint<1000 then
              wh:=ss.dwWaitHint
            else
              wh:=1000;
            wc:=ss.dwWaitHint div 1000;
            i:=0;
            repeat
              Sleep(wh);
              inc(i);
              QueryServiceStatus(schs,ss);
              if (SERVICE_STOPPED=ss.dwCurrentState) then
                Break;
            until i>=wc;
            if (SERVICE_STOPPED<>ss.dwCurrentState) then
              if not QueryServiceStatus(schs,ss) or (ss.dwCheckPoint<cp) or (GetTickCount64-t>SERVICE_CONTROL_TIMEOUT) then
                Break;
          end;
          Result:=SERVICE_STOPPED=ss.dwCurrentState;
        end;
      CloseServiceHandle(schs);
    end;
    CloseServiceHandle(schm);
  end;
end;

function ServiceStop(const AService :string; ADependants: TStringlist; ACheck: Boolean = False; const AMachine: string = ''): boolean;
var
  i: integer;
begin
  Result:=True;
  ADependants.Add(AService);
  for i:=0 to ADependants.Count-1 do
    Result:=Result and _ServiceStop(ADependants[i],ACheck,AMachine);
end;

function ServiceRestart(const AService: string; ADependants: TStringlist; const AMachine: string = ''): Boolean;
var
  i: integer;
begin
  Result:=True;
  ADependants.Add(AService);
  for i:=0 to ADependants.Count-1 do
    _ServiceStop(ADependants[i],True,AMachine);
  for i:=ADependants.Count-1 downto 0 do
    Result:=Result and ServiceStart(ADependants[i],'',True);
end;

function ServicePause(const AService :string; ACheck: Boolean = False; const AMachine: string = ''): boolean;
var
  schm, schs: SC_Handle;
  ss: TServiceStatus;
  cp: Cardinal;
begin
  schm:=OpenSCManager(PChar(AMachine),nil,SC_MANAGER_CONNECT);
  if(schm > 0)then  begin
    schs:=OpenService(schm,PChar(AService),SERVICE_CONTROL_PAUSE or SERVICE_QUERY_STATUS);
    if(schs > 0)then  begin
      if(ControlService(schs,SERVICE_CONTROL_PAUSE,ss))then
        if(QueryServiceStatus(schs,ss))then
          if ACheck then
            while(SERVICE_PAUSED<>ss.dwCurrentState)do begin
              cp:=ss.dwCheckPoint;
              Sleep(ss.dwWaitHint);
              if(not QueryServiceStatus(schs,ss))then
                break;
              if(ss.dwCheckPoint<cp)then
                break;
            end;
      CloseServiceHandle(schs);
    end;
    CloseServiceHandle(schm);
  end;
  Result:=SERVICE_PAUSED=ss.dwCurrentState;
end;

function ServiceContinue(const AService :string; ACheck: Boolean = False; const AMachine: string = ''): Boolean;
var
  schm, schs: SC_Handle;
  ss: TServiceStatus;
  cp: Cardinal;
begin
  schm:=OpenSCManager(PChar(AMachine),nil,SC_MANAGER_CONNECT);
  if (schm>0) then begin
    schs:=OpenService(schm,PChar(AService),SERVICE_CONTROL_CONTINUE or SERVICE_QUERY_STATUS);
    if(schs > 0)then  begin
      if(ControlService(schs,SERVICE_CONTROL_CONTINUE,ss))then
        if(QueryServiceStatus(schs,ss))then
          if ACheck then
            while(SERVICE_RUNNING<>ss.dwCurrentState)do begin
              cp:=ss.dwCheckPoint;
              Sleep(ss.dwWaitHint);
              if(not QueryServiceStatus(schs,ss))then
                break;
              if(ss.dwCheckPoint<cp)then
                break;
            end;
      CloseServiceHandle(schs);
    end;
    CloseServiceHandle(schm);
  end;
  Result:=SERVICE_RUNNING=ss.dwCurrentState;
end;

function ServiceGetKeyName(const AServiceDispName :string; const AMachine: string = '') : string;
var
  schm: SC_Handle;
  nMaxNameLen: cardinal;
  pAServiceName: PChar;
begin
  Result:='';
  nMaxNameLen:=255;
  schm:=OpenSCManager(PChar(AMachine),nil,SC_MANAGER_CONNECT);
  if (schm>0) then
    try
      pAServiceName:=StrAlloc(nMaxNameLen+1);
      if (pAServiceName<>nil) then
        try
          if GetServiceKeyName(schm,PChar(AServiceDispName),pAServiceName,nMaxNameLen) then begin
            pAServiceName[nMaxNameLen]:=#0;
            Result:=pAServiceName;
          end;
        finally
          StrDispose(pAServiceName);
        end;
    finally
      CloseServiceHandle(schm);
    end;
end;

function ServiceGetDisplayName(const AServiceKeyName :string; const AMachine: string = '') : string;
var
  schm : SC_Handle;
  nMaxNameLen   : cardinal;
  pAServiceName : PChar;
begin
  Result:='';
  nMaxNameLen:=255;
  schm:=OpenSCManager(PChar(AMachine),nil,SC_MANAGER_CONNECT);
  if (schm>0) then
    try
      pAServiceName:=StrAlloc(nMaxNameLen+1);
      try
        if GetServiceDisplayName(schm,PChar(AServiceKeyName),pAServiceName,nMaxNameLen) then begin
          pAServiceName[nMaxNameLen]:=#0;
          Result:=pAServiceName;
        end;
      finally
        StrDispose(pAServiceName);
      end;
    finally
      CloseServiceHandle(schm);
    end;
end;

function ServiceGetList(const AMachine :string; AServiceType,AServiceState :Cardinal; AList :TStrings): Cardinal;
const
  cnMaxServices = 4096;
type
  PSvcArray = ^TSvcArray;
  TSvcArray = array[0..511] of TEnumServiceStatus;
var
  j : integer;
  schm : SC_Handle;
  nBytesNeeded, nServices, nResumeHandle : Cardinal;
  ssa: TSvcArray;
begin
  Result:=0;
  schm:=OpenSCManager(PChar(AMachine),nil,{SC_MANAGER_ALL_ACCESS}GENERIC_READ);
  if(schm>0) then
    try
      nResumeHandle:=0;
      while True do begin
        EnumServicesStatus(schm,AServiceType,AServiceState,{$IFDEF FPC}@{$ENDIF}ssa[0],SizeOf(ssa),nBytesNeeded,nServices,nResumeHandle );
        for j:=0 to nServices-1 do
          AList.Add(string(ssa[j].lpDisplayName));
        if nBytesNeeded = 0 then
          Break;
      end;
    finally
      CloseServiceHandle(schm);
    end
  else
    Result:=GetLastError;
end;

function ServiceGetConfig(const AMachine,AService :string; var QSC: TServiceConfig) : boolean;
var
  schm,schs :SC_Handle;
  d :Cardinal;
  {$IFDEF RAD9PLUS}
  pqsc: LPQuery_Service_ConfigW;
  {$ELSE}
  pqsc: PQueryServiceConfig;
  {$ENDIF}
  p,q: PChar;
begin
  result:=false;
  schm:=OpenSCManager(PChar(AMachine),nil,SC_MANAGER_CONNECT);
  if (schm>0) then
    try
      schs:=OpenService(schm,PChar(AService),SERVICE_QUERY_CONFIG);
      if (schs>0) then
        try
          QueryServiceConfig(schs,nil,0,d);
          pqsc:=Allocmem(d);
          try
            if QueryServiceConfig(schs,pqsc,d,d) then begin
              qsc.dwServiceType:=pqsc^.dwServiceType;
              qsc.dwStartType:=pqsc^.dwStartType;
              qsc.dwErrorControl:=pqsc^.dwErrorControl;
              qsc.BinaryPathName:=pqsc^.lpBinaryPathName;
              qsc.LoadOrderGroup:=pqsc^.lpLoadOrderGroup;
              qsc.dwTagId:=pqsc^.dwTagId;
              qsc.Dependencies:='';
              qsc.ServiceStartName:=pqsc^.lpServiceStartName;
              qsc.DisplayName:=pqsc^.lpDisplayName;
              p:=pqsc^.lpDependencies;
              if p<>nil then
                while p^<>#0 do begin
                  q:=p;
                  while p^<>#0 do
                    Inc(p);
                  if p>q then begin
                    if qsc.Dependencies<>'' then
                      qsc.Dependencies:=qsc.Dependencies+' ';
                    qsc.Dependencies:=qsc.Dependencies+Copy(q,1,p-q);
                  end;
                  Inc(p);
                end;
              Result:=True;
            end;
          finally
            Freemem(pqsc);
          end;
        finally
          CloseServiceHandle(schs);
        end;
    finally
      CloseServiceHandle(schm);
    end;
end;

function InstallService(const AServiceName, ADisplayName, AFileName: string; AStart: Boolean = false): boolean;
var
  SCManager: SC_HANDLE;
  Service: SC_HANDLE;
  Args: pchar;
begin
  Result:=False;
  SCManager:=OpenSCManager(nil, nil, SC_MANAGER_CREATE_SERVICE);
  if SCManager=0 then
    Exit;
  try
    Service:=CreateService(SCManager,PChar(AServiceName),PChar(ADisplayName),
      SERVICE_START,SERVICE_WIN32_OWN_PROCESS,SERVICE_AUTO_START,SERVICE_ERROR_IGNORE,PChar(AFileName),nil,nil,nil,nil,nil);
    Result:=Service<>0;
    Args:=nil;
    if Result and AStart then
      Result:=StartService(Service,0,Args);
    CloseServiceHandle(Service);
  finally
    CloseServiceHandle(SCManager);
  end;
end;

function UninstallService(const AService :string) : Boolean;
var
  schm,sh: SC_Handle;
begin
  Result:=False;
  schm:=OpenSCManager(nil,nil,SC_MANAGER_CONNECT);
  if (schm>0) then
    try
      sh:=OpenService(schm,PChar(AService),SERVICE_DELETE);
      if (sh>0) then
        try
          Result:=DeleteService(sh);
        finally
          CloseServiceHandle(sh);
        end;
    finally
      CloseServiceHandle(schm);
    end;
end;

procedure ServiceGetDependants(const AMachine, AService: string; AOnlyRunning, ADisplayNames: boolean; AList: TStringList);
const
  cnMaxServices = 4096;
type
  PSvcArray = ^TSvcArray;
  TSvcArray = array[0..511] of TEnumServiceStatus;
var
  i: integer;
  schm,sh: THandle;
  n,c,h,m: Cardinal;
  ssa: TSvcArray;
  pqsc: {$IFDEF RAD9PLUS}LPQuery_Service_ConfigW{$ELSE}PQueryServiceConfig{$ENDIF};
  s: string;
  q,p: PChar;
  ss: TServiceStatus;
  sl,tl: TStringList;

procedure GetDependants(const AServiceName: string);
var
  sn: string;
  i: integer;
  p: array[0..254] of char;
  d: Cardinal;
begin
  if tl.IndexOf(AServiceName)>-1 then
    Exit;
  tl.Add(AServiceName);
  sn:=UpperCase(AServiceName);
  for i:=0 to sl.Count-1 do
    if Pos(sn,UpperCase(sl.ValueFromIndex[i]))>0 then begin
      if AOnlyRunning then begin
        ss.dwCurrentState:=0;
        sh:=OpenService(schm,PChar(sl.Names[i]),SERVICE_QUERY_STATUS);
        if (sh>0) then begin
          if not QueryServiceStatus(sh,ss) then
            ss.dwCurrentState:=0;
          CloseServiceHandle(sh);
        end;
      end;
      if not AOnlyRunning or (ss.dwCurrentState=SERVICE_RUNNING) then begin
        GetDependants(sl.Names[i]);
        d:=sizeof(p);
        ZeroMemory(@p,d);
        if ADisplayNames then
          GetServiceDisplayName(schm,PChar(sl.Names[i]),@p,d);
        if string(p)<>'' then
          AList.Add(string(p))
        else
          AList.Add(sl.Names[i]);
      end;
    end;
end;

begin
  AList.Clear;
  schm:=OpenSCManager(PChar(AMachine),nil,GENERIC_READ);
  if(schm>0) then begin
    sl:=TStringList.Create;
    tl:=TStringList.Create;
    try
      h:=0;
      while True do begin
        EnumServicesStatus(schm,SERVICE_TYPE_ALL,SERVICE_STATE_ALL,{$IFDEF FPC}@{$ENDIF}ssa[0],SizeOf(ssa),n,c,h);
        for i:=0 to c-1 do begin
          s:='';
          sh:=OpenService(schm,ssa[i].lpServiceName,SERVICE_QUERY_CONFIG or SERVICE_QUERY_STATUS);
          if (sh>0) then
            try
              QueryServiceConfig(sh,nil,0,m);
              pqsc:=Allocmem(m);
              try
                if QueryServiceConfig(sh,pqsc,m,m) then begin
                  p:=pqsc^.lpDependencies;
                  if p<>nil then
                    while p^<>#0 do begin
                      q:=p;
                      while p^<>#0 do
                        Inc(p);
                      if p>q then
                        s:=s+Copy(q,1,p-q)+',';
                      Inc(p);
                    end;
                  SetLength(s,Length(s)-1);
                end;
              finally
                Freemem(pqsc);
              end;
            finally
              CloseServiceHandle(sh);
            end;
          sl.Add(string(ssa[i].lpServiceName)+'='+s);
        end;
        if n=0 then
          Break;
      end;
      GetDependants(AService);
    finally
      sl.Free;
      tl.Free;
      CloseServiceHandle(schm);
    end;
  end;
end;

var
  ADVAPIHandle :THandle;
initialization
  ADVAPIHandle:=GetModuleHandle('advapi32.dll');
  if ADVAPIHandle=0 then
    ADVAPIHandle:=LoadLibrary('advapi32.dll');
  if ADVAPIHandle<>0 then begin
    {$IFDEF UNICODE}
    RegisterServiceCtrlHandlerEx:=GetProcAddress(ADVAPIHandle,PChar('RegisterServiceCtrlHandlerExW'));
    QueryServiceConfig2:=GetProcAddress(ADVAPIHandle,'QueryServiceConfig2W');
    ChangeServiceConfig2:=GetProcAddress(ADVAPIHandle,'ChangeServiceConfig2W');
    {$ELSE}
    RegisterServiceCtrlHandlerEx:=GetProcAddress(ADVAPIHandle,PChar('RegisterServiceCtrlHandlerExA'));
    QueryServiceConfig2:=GetProcAddress(ADVAPIHandle,'QueryServiceConfig2A');
    ChangeServiceConfig2:=GetProcAddress(ADVAPIHandle,'ChangeServiceConfig2A');
    {$ENDIF}
    {$IFNDEF RAD7PLUS}
    QueryServiceStatusEx:=GetProcAddress(ADVAPIHandle,PChar('QueryServiceStatusEx'));
    {$ENDIF}
  end;
end.

