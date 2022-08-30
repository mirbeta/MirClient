{*******************************************************}
{       MiTeC System Information Component Suite        }
{                Process Monitor                        }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}


{$INCLUDE Compilers.inc}


unit MSI_ProcMon;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, VCL.ExtCtrls, System.SyncObjs,
     {$ELSE}
     Windows, SysUtils, Classes, ExtCtrls, SyncObjs,
     {$ENDIF}
     MiTeC_Windows, MSI_Defs, MiTeC_NativeDefs, MiTeC_Routines, MiTeC_PSAPI;

type
  TThreadRecord = record
    ID: Cardinal;
    Handle: THandle;
    StartAddress: NativeUInt;
    ContextSwitchCount: Cardinal;
    State: Cardinal;
    WaitReason: Cardinal;
    Priority: KPRIORITY;
    BasePriority: integer;
    CPUUsage,
    MaxCPUUsage: Double;
    KernelTime,
    UserTime: Int64;
    WaitTime: Cardinal;
    CreateTime: TDateTime;
    Cycles: UInt64;
    Text: string;
    StartAddressString: string;
    _Period: UInt64;
    _Exists: Boolean;
  end;
  PThreadRecord = ^TThreadRecord;

  TThreadHistoryRecord = record
    ID: Cardinal;
    CreateTime,
    TerminateTime: TDateTime;
    MaxCPUUsage: Double;
    StartAddressString: string;
    Text: string;
  end;
  PThreadHistoryRecord = ^TThreadHistoryRecord;

  TModuleRecord = record
    Name,
    ImageName: String;
    ImageSize: Cardinal;
    EntryPoint: Cardinal;
    BaseAddress: NativeUInt;
    VersionInfo: TVersionInfo;
    Size: Int64;
    _Exists: Boolean;
  end;
  PModuleRecord = ^TModuleRecord;

  TWindowRecord = TWindowInfo;
  PWindowRecord = ^TWindowRecord;

  TProcMonThread = class;

  TProcMonNotifyEvent = procedure(Sender: TProcMonThread) of object;

  TProcMonThread = class(TThread)
  private
    FPeriod: UInt64;
    FOnInterval: TProcMonNotifyEvent;
    FCPUCount: Byte;
    FInterval: Cardinal;
    FCPUUsage: Double;
    FCycles: UInt64;
    FHandle: THandle;
    FThreadList,
    FThreadHistoryList: TList;
    FLock: TCriticalSection;
    FPID: Cardinal;
    FIORead,
    FIOWrite,
    FIOOther: Int64;
    FProcessExists,
    FInit: Boolean;
    FImageName, FName: string;
    FWinCount,
    FBits,
    FCPCount: Cardinal;
    FSPI: TSystemProcessInformation;
    FSaveTH,FSaveTHOnlyDebug: boolean;
    FModList: TList;
    FHandleList: TList;
    FIsSuspended: Boolean;
    FSHA1: string;
    FCIPB: UInt64;
    //FPMC: TProcessMemoryCountersEx;
    Buffer: Pointer;
    BufferSize: Cardinal;
    procedure SetInterval(const Value: Cardinal);
    procedure RefreshData;
    procedure DoSync;
    function GetCPUUsage: Double;
    function GetThreadUsage(AID: Cardinal): Double;
    function FindThread(AID: Cardinal): Integer;
    function GetPriority: Cardinal;
    function GetBasePriority: Cardinal;
    function GetTotalHandleCount: Cardinal;
    function GetWinCount: Cardinal;
    function GetThreadCount: Cardinal;
    function GetPID: Cardinal;
    procedure SetPID(const Value: Cardinal);
    function GetInterval: Cardinal;
    function GetOnInterval: TProcMonNotifyEvent;
    procedure SetOnInterval(const Value: TProcMonNotifyEvent);
    function GetImageName: string;
    function GetProcessExists: Boolean;
    function GetIORead: Int64;
    function GetIOWrite: Int64;
    function GetCreateTime: TDateTime;
    function GetIOOther: Int64;
    function GetSession: Cardinal;
    function GetKernelTime: int64;
    function GetUserTime: int64;
    function GetBits: Cardinal;
    function GetProcHandle: THandle;
    function GetMemory(const Index: Integer): SIZE_T;
    function GetIO(const Index: Integer): Int64;
    function GetName: string;
    function GetParentPID: Cardinal;
    function GetTHCount: Cardinal;
    function GetSaveTH: boolean;
    procedure SetSaveTH(const Value: boolean);
    function GetSaveTHOnlyDebug: boolean;
    procedure SetSaveTHOnlyDebug(const Value: boolean);
    function GetCPCount: Cardinal;
    function GetModuleCount: Cardinal;
    function GetIsSuspended: Boolean;
    function GetHC(const Index: Integer): Integer;
    function GetSHA1: string;
    function GetChildInstancesPrivateBytes: UInt64;
    function GetAffinity: NativeUInt;
    function GetHandleCount: Cardinal;
    //function GetMemoryoryCounters: TProcessMemoryCountersEx;
  protected
    procedure Execute; override;
    procedure ClearValues;
  public
    constructor Create(APID: Cardinal);
    destructor Destroy; override;

    class function GetProcessImageNameByPID(APID: Cardinal): string;
    class function GetProcessImageNameByHandle(AHandle: THandle): string;

    class function GetProcessMemoryCountersByPID(APID: Cardinal): TProcessMemoryCountersEx;
    class function GetProcessmemoryCountersByHandle(AHandle: THandle): TProcessMemoryCountersEx;

    class procedure RefreshHandles(APID: Cardinal; AHandle: THandle; AList: TList);
    class procedure RefreshModules(APID: Cardinal; AList: TList);
    class procedure RefreshWindows(APID: Cardinal; AOnlyVisible: Boolean; AList: TList);
    class procedure RefreshEnvironment(APID: Cardinal; AHandle: THandle; AList: TStringList);

    procedure GetModuleRecord(AIndex: Integer; var ARecord: TModuleRecord);

    procedure GetHandleRecord(AIndex: Integer; var ARecord: THandleRecord);

    procedure GetThreadRecord(AIndex: Integer; var ARecord: TThreadRecord);
    procedure GetThreadRecordByID(AID: Cardinal; var ARecord: TThreadRecord);

    procedure GetThreadHistoryRecord(AIndex: Integer; var ARecord: TThreadHistoryRecord);
    procedure ClearThreadHistory;

    procedure SetThreadText(AID: Cardinal; const AText: string);
    procedure SaveThreadTexts(const AFilename: string; ANoLock: Boolean = False);
    procedure LoadThreadTexts(const AFilename: string; ANoLock: Boolean = False);

    property ProcessExists: Boolean read GetProcessExists;
    property SessionID: Cardinal read GetSession;
    property ParentPID: Cardinal read GetParentPID;
    property Name: string read GetName;
    property ImageName: string read GetImageName;
    property CreateTime: TDateTime read GetCreateTime;
    property KernelTime: int64 read GetKernelTime;
    property UserTime: int64 read GetUserTime;
    property CPUUsage: Double read GetCPUUsage;
    property Priority: Cardinal read GetPriority;
    property BasePriority: Cardinal read GetBasePriority;
    property Affinity: NativeUInt read GetAffinity;
    property IsSuspended: Boolean read GetIsSuspended;
    property ModuleCount: Cardinal read GetModuleCount;
    property ThreadCount: Cardinal read GetThreadCount;
    property TotalHandleCount: Cardinal read GetTotalHandleCount;
    property HandleCount: Cardinal read GetHandleCount;
    property WindowCount: Cardinal read GetWinCount;
    property ThreadUsage[AID: Cardinal]: Double read GetThreadUsage;
    property CurrentIORead: Int64 read GetIORead;
    property CurrentIOWrite: Int64 read GetIOWrite;
    property CurrentIOOther: Int64 read GetIOOther;
    property TotalIORead: Int64 Index 0 read GetIO;
    property TotalIOWrite: Int64 Index 1 read GetIO;
    property TotalIOOther: Int64 Index 2 read GetIO;
    property TotalIOReadCnt: Int64 Index 3 read GetIO;
    property TotalIOWriteCnt: Int64 Index 4 read GetIO;
    property TotalIOOtherCnt: Int64 Index 5 read GetIO;
    property WorkingSetSize: SIZE_T Index 0 read GetMemory;
    property PeakWorkingSetSize: SIZE_T Index 1 read GetMemory;
    property PageFileUsage: SIZE_T Index 2 read GetMemory;
    property PeakPageFileUsage: SIZE_T Index 3 read GetMemory;
    property VirtualSize: SIZE_T Index 4 read GetMemory;
    property PeakVirtualSize: SIZE_T Index 5 read GetMemory;
    property PageFaultCount: SIZE_T Index 6 read GetMemory;
    property QuotaPeakPagedPoolUsage: SIZE_T Index 7 read GetMemory;
    property QuotaPagedPoolUsage: SIZE_T Index 8 read GetMemory;
    property QuotaPeakNonPagedPoolUsage: SIZE_T Index 9 read GetMemory;
    property QuotaNonPagedPoolUsage: SIZE_T Index 10 read GetMemory;
    property PrivatePageCount: SIZE_T Index 11 read GetMemory;
    //property PeakPrivatePageCount: SIZE_T Index 12 read GetMemory;
    //property MemoryCounters: TProcessMemoryCountersEx read GetMemoryoryCounters;
    property ChildInstancesPrivateBytes: UInt64 read GetChildInstancesPrivateBytes;
    property GDIHandleCount: Integer Index 0 read GetHC;
    property UserHandleCount: Integer Index 1 read GetHC;
    property Bits: Cardinal read GetBits;
    property ProcessHandle: THandle read GetProcHandle;
    property ThreadHistoryCount: Cardinal read GetTHCount;
    property ChildProcCount: Cardinal read GetCPCount;
    property SHA1: string read GetSHA1;

    property ProcessID: Cardinal read GetPID write SetPID;
    property Interval: Cardinal read GetInterval write SetInterval;
    property SaveThreadHistory: boolean read GetSaveTH write SetSaveTH;
    property SaveOnlyThreadsWithDebug: boolean read GetSaveTHOnlyDebug write SetSaveTHOnlyDebug;
    property OnInterval: TProcMonNotifyEvent read GetOnInterval write SetOnInterval;
  end;

implementation

uses {$IFDEF RAD9PLUS}
     Winapi.TlHelp32, System.DateUtils, {$IFDEF TRIAL}VCL.Dialogs,{$ENDIF}System.AnsiStrings,
     {$ELSE}
     {$IFDEF FPC}JwaTlHelp32{$ELSE}TlHelp32{$ENDIF}, DateUtils, {$IFDEF TRIAL}Dialogs,{$ENDIF}
     {$ENDIF}
     MiTeC_NativeAPI, MiTeC_Datetime, MiTeC_StrUtils, MiTeC_WinCrypt;

type
  TWinEnumParam = record
    OnlyVisible: Boolean;
    PID: Cardinal;
    List: TList;
    Count: Integer;
  end;
  PWinEnumParam = ^TWinEnumParam;

function EnumChildProc(Wnd: HWND; AParam: LPARAM): Boolean; stdcall;
var
  WPID: Cardinal;
  wep: PWinEnumParam;
  w: PWindowRecord;
begin
  wep:=PWinEnumParam(AParam);
  GetWindowThreadProcessId(Wnd,WPID);
  if (wep.PID=WPID) and (not wep.OnlyVisible or IsWindowVisible(Wnd)) then begin
    if wep.Count=wep.List.Capacity then
      wep.List.Capacity:=wep.List.Capacity+100;
    new(w);
    w^:=GetWindowInfo(Wnd,True);
    wep.List.Add(w);
    Inc(wep.Count);
  end;
  Result:=Wnd<>0;
end;

function EnumWindowsProc(Wnd: HWND; AParam: LPARAM): Boolean; stdcall;
var
  WPID: Cardinal;
  wep: PWinEnumParam;
  w: PWindowRecord;
begin
  wep:=PWinEnumParam(AParam);
  GetWindowThreadProcessId(Wnd,WPID);
  if (wep.PID=WPID) and (not wep.OnlyVisible or IsWindowVisible(Wnd)) then begin
    if (wep.List.Count>0) and (wep.Count=wep.List.Capacity) then
      wep.List.Capacity:=wep.List.Capacity+100;
    new(w);
    w^:=GetWindowInfo(Wnd,True);
    wep.List.Add(w);
    Inc(wep.Count);
  end;
  EnumChildWindows(Wnd,@EnumChildProc,AParam);
  Result:=(Wnd<>0);
end;

{ TProcMonThread }

procedure TProcMonThread.ClearThreadHistory;
var
  i: Integer;
begin
  FLock.Enter;
  try
    for i:=0 to FThreadHistoryList.Count-1 do
      Dispose(PThreadHistoryRecord(FThreadHistoryList[i]));
    FThreadHistoryList.Clear;
  finally
    FLock.Leave;
  end;
end;

procedure TProcMonThread.ClearValues;
var
  i: integer;
begin
  for i:=0 to FModList.Count-1 do
    Dispose(PModuleRecord(FModList[i]));
  FModList.Clear;
  for i:=0 to FHandleList.Count-1 do
    Dispose(PHandleRecord(FHandleList[i]));
  FHandleList.Clear;
  for i:=0 to FThreadList.Count-1 do begin
    CloseHandle(PThreadRecord(FThreadList[i])^.Handle);
    Dispose(PThreadRecord(FThreadList[i]));
  end;
  FThreadList.Clear;
  for i:=0 to FThreadHistoryList.Count-1 do
    Dispose(PThreadHistoryRecord(FThreadHistoryList[i]));
  FThreadHistoryList.Clear;
  FInit:=True;
  FCycles:=0;
  FIORead:=0;
  FIOWrite:=0;
  FIOOther:=0;
  FWinCount:=0;
  FImageName:='';
  FName:='';
  CloseHandle(FHandle);
  FHandle:=0;
  ResetMemory(FSPI,SizeOf(FSPI));
  FBits:=0;
end;

constructor TProcMonThread.Create;
begin
  {$IFDEF TRIAL}
  if not RunFromIDE then
    MessageDlg('TProcMonThread'+sLineBreak+cCompName+sLineBreak+cCopyright,mtInformation,[mbOK],0);
  {$ENDIF}
  inherited Create(True);
  FreeOnTerminate:=False;

  FLock:=TCriticalSection.Create;
  FThreadList:=TList.Create;
  FThreadHistoryList:=TList.Create;
  FModList:=TList.Create;
  FHandleList:=TList.Create;

  FSaveTH:=False;
  FSaveTHOnlyDebug:=True;
  FProcessExists:=False;
  FInterval:=1000;
  FCPUCount:=NativeGetCPUCount;
  ClearValues;

  ProcessID:=APID;
end;

destructor TProcMonThread.Destroy;
begin
  FOnInterval:=nil;
  if not Terminated then
    Terminate;
  while not Terminated do
    Sleep(100);
  ClearValues;
  FThreadList.Free;
  FThreadHistoryList.Free;
  FModList.Free;
  FHandleList.Free;
  FLock.Free;
  inherited;
end;

procedure TProcMonThread.DoSync;
begin
  if Assigned(FOnInterval) then
    FOnInterval(Self);
end;

procedure TProcMonThread.Execute;
var
  se: TSimpleEvent;
begin
  FThreadList.Clear;
  FThreadHistoryList.Clear;
  se:=TSimpleEvent.Create{$IFDEF BDS35PLUS}(nil,False,False,''){$ENDIF};
  BufferSize:=SizeOf(TSystemProcessInformation);
  Buffer:=AllocMem(BufferSize);
  try
    FPeriod:=GetTickCount64-10;
    while not Terminated do begin
      RefreshData;

      if Assigned(FOnInterval) and not Terminated then
        Synchronize(DoSync);

      if not Terminated then
        se.WaitFor(FInterval);
    end;
  finally
    Freemem(Buffer);
    se.Free;
  end;
end;

function TProcMonThread.FindThread(AID: Cardinal): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to FThreadList.Count-1 do
    if AID=PThreadRecord(FThreadList[i]).ID then begin
      Result:=i;
      Break;
    end;
end;

function TProcMonThread.GetBits: Cardinal;
begin
  FLock.Enter;
  try
    Result:=FBits;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetCPCount: Cardinal;
begin
  FLock.Enter;
  try
    Result:=FCPCount;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetCPUUsage: Double;
begin
  FLock.Enter;
  try
    Result:=FCPUUsage;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetCreateTime: TDateTime;
begin
  FLock.Enter;
  try
    Result:=FileTimeToDateTime(TFileTime(FSPI.CreateTime),True);
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetTotalHandleCount: Cardinal;
begin
  FLock.Enter;
  try
    Result:=FSPI.HandleCount;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetHandleCount: Cardinal;
begin
  FLock.Enter;
  try
    Result:=FHandleList.Count;
  finally
    FLock.Leave;
  end;
end;

procedure TProcMonThread.GetHandleRecord(AIndex: Integer;
  var ARecord: THandleRecord);
begin
  FLock.Enter;
  try
    ResetMemory(ARecord,SizeOf(ARecord));
    if AIndex<FHandleList.Count then
      ARecord:=PHandleRecord(FHandleList[AIndex])^;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetHC(const Index: Integer): Integer;
begin
  Result:=GetGuiResources(FHandle,Index);
end;

function TProcMonThread.GetImageName: string;
begin
  FLock.Enter;
  try
    Result:=FImageName;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetInterval: Cardinal;
begin
  FLock.Enter;
  try
    Result:=FInterval;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetIO(const Index: Integer): Int64;
begin
  FLock.Enter;
  try
    Result:=0;
    case Index of
      0: Result:=FSPI.IoCounters.ReadTransferCount.QuadPart;
      1: Result:=FSPI.IoCounters.WriteTransferCount.QuadPart;
      2: Result:=FSPI.IoCounters.OtherTransferCount.QuadPart;
      3: Result:=FSPI.IoCounters.ReadOperationCount.QuadPart;
      4: Result:=FSPI.IoCounters.WriteOperationCount.QuadPart;
      5: Result:=FSPI.IoCounters.OtherOperationCount.QuadPart;
    end;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetIOOther: Int64;
begin
  FLock.Enter;
  try
    Result:=FIOOther;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetIORead: Int64;
begin
  FLock.Enter;
  try
    Result:=FIORead;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetIOWrite: Int64;
begin
  FLock.Enter;
  try
    Result:=FIOWrite;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetIsSuspended: Boolean;
begin
  FLock.Enter;
  try
    Result:=FIsSuspended;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetKernelTime: int64;
begin
  FLock.Enter;
  try
    Result:=FSPI.KernelTime.QuadPart;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetMemory(const Index: Integer): SIZE_T;
begin
  FLock.Enter;
  try
    Result:=0;
    case Index of
      0: Result:=FSPI.VmCounters.WorkingSetSize;
      1: Result:=FSPI.VmCounters.PeakWorkingSetSize;
      2: Result:=FSPI.VmCounters.PagefileUsage;
      3: Result:=FSPI.VmCounters.PeakPagefileUsage;
      4: Result:=FSPI.VmCounters.VirtualSize;
      5: Result:=FSPI.VmCounters.PeakVirtualSize;
      6: Result:=FSPI.VmCounters.PageFaultCount;
      7: Result:=FSPI.VmCounters.QuotaPeakPagedPoolUsage;
      8: Result:=FSPI.VmCounters.QuotaPagedPoolUsage;
      9: Result:=FSPI.VmCounters.QuotaPeakNonPagedPoolUsage;
      10: Result:=FSPI.VmCounters.QuotaNonPagedPoolUsage;
      11: Result:=FSPI.VmCounters.PrivatePageCount;
      //12: Result:=FSPI.VmCounters.PeakPrivatePageCount;
    end;
  finally
    FLock.Leave;
  end;
end;

{function TProcMonThread.GetMemoryoryCounters: TProcessMemoryCountersEx;
begin
  FLock.Enter;
  try
    Result:=FPMC;
  finally
    FLock.Leave;
  end;
end;}

function TProcMonThread.GetModuleCount: Cardinal;
begin
  FLock.Enter;
  try
    Result:=FModList.Count;
  finally
    FLock.Leave;
  end;
end;

procedure TProcMonThread.GetModuleRecord(AIndex: Integer;
  var ARecord: TModuleRecord);
begin
  FLock.Enter;
  try
    ResetMemory(ARecord,SizeOf(ARecord));
    if AIndex<FModList.Count then
      ARecord:=PModuleRecord(FModList[AIndex])^;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetName: string;
begin
  FLock.Enter;
  try
    Result:=FName;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetOnInterval: TProcMonNotifyEvent;
begin
  FLock.Enter;
  try
    Result:=FOnInterval;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetParentPID: Cardinal;
begin
  FLock.Enter;
  try
    Result:=FSPI.InheritedFromProcessId;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetPID: Cardinal;
begin
  FLock.Enter;
  try
    Result:=FPID;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetAffinity: NativeUInt;
var
  sa: NativeUInt;
begin
  GetProcessAffinityMask(FHandle,Result,sa);
end;

function TProcMonThread.GetBasePriority: Cardinal;
begin
  FLock.Enter;
  try
    Result:=FSPI.BasePriority;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetChildInstancesPrivateBytes: UInt64;
begin
  FLock.Enter;
  try
    Result:=FCIPB;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetPriority: Cardinal;
begin
  Result:=GetPriorityClass(FHandle);
end;

function TProcMonThread.GetProcessExists: Boolean;
begin
  FLock.Enter;
  try
    Result:=FProcessExists;
  finally
    FLock.Leave;
  end;
end;

class function TProcMonThread.GetProcessImageNameByHandle(
  AHandle: THandle): string;
var
  c: {$IFDEF NATIVEINT}NativeUInt{$ELSE}Cardinal{$ENDIF};
  pbi: PROCESS_BASIC_INFORMATION;
  pb: TPEB;
  ib: TProcessParameters;
  pwc: PWideChar;
  n,j: Cardinal;
  Buf: array[0..MAX_PATH] of char;
begin
  Result:='';
  c:=0;
  if (AHandle<>0) and (AHandle<>INVALID_HANDLE_VALUE) then
    try
      n:=SizeOf(Buf);
      if Assigned(QueryFullProcessImageName) and (QueryFullProcessImageName(AHandle,0,@Buf,@n)>0) then
        Result:=Buf;
      if (Result='') and (NtQueryInformationProcess(AHandle,ProcessBasicInformation,@pbi,SizeOf(pbi),@c)=0) then
        if ReadProcessMemory(AHandle,pbi.PebBaseAddress,@pb,SizeOf(pb),c) then
          if ReadProcessMemory(AHandle,Pointer(pb.ProcessParameters),@ib,SizeOf(ib),c) then begin
            pwc:=AllocMem(MAX_PATH+1);
            try
              if ReadProcessMemory(AHandle,Pointer(ib.ImagePathName.Buffer),pwc,MAX_PATH,c) then
                Result:=WideCharToString(pwc);
            finally
              FreeMem(pwc);
            end;
          end;
      ResetMemory(Buf,SizeOf(Buf));
      c:=0;
      if (Result='') and (NtQueryInformationProcess(AHandle,ProcessImageFileName,@Buf,SizeOf(Buf),@c)=0) then begin
        Result:=Trim(Copy(string(Buf),5,c));
        if Result<>'' then
          for j:=0 to High(DosDevices) do
            if Pos(DosDevices[j].DiskDosQuery,Result)=1 then begin
              Result:=StringReplace(Result,DosDevices[j].DiskDosQuery,DosDevices[j].DiskLabel,[rfIgnoreCase]);
              Break;
            end;
          end;
  finally
  end;
end;

class function TProcMonThread.GetProcessImageNameByPID(APID: Cardinal): string;
var
  h: THandle;
begin
  h:=GetProcessHandle(APID);
  if (h<>0) and (h<>INVALID_HANDLE_VALUE) then
    try
      GetProcessImageNameByHandle(h);
    finally
      CloseHandle(h);
    end;
end;

class function TProcMonThread.GetProcessMemoryCountersByPID(APID: Cardinal): TProcessMemoryCountersEx;
var
  h: THandle;
begin
  ResetMemory(Result,sizeof(Result));
  h:=GetProcessHandle(APID);
  if (h<>0) and (h<>INVALID_HANDLE_VALUE) then
    try
      Result:=GetProcessMemoryCountersByHandle(h);
    finally
      CloseHandle(h);
    end;
end;

class function TProcMonThread.GetProcessMemoryCountersByHandle(AHandle: THandle): TProcessMemoryCountersEx;
var
  cb: Cardinal;
begin
  cb:=SizeOf(Result);
  ResetMemory(Result,cb);
  GetProcessMemoryInfo(AHandle,@REsult,cb);
end;

function TProcMonThread.GetProcHandle: THandle;
begin
  FLock.Enter;
  try
    Result:=FHandle;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetSaveTH: boolean;
begin
  FLock.Enter;
  try
    Result:=FSaveTH;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetSaveTHOnlyDebug: boolean;
begin
  FLock.Enter;
  try
    Result:=FSaveTHOnlyDebug;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetSession: Cardinal;
begin
  FLock.Enter;
  try
    Result:=FSPI.SessionId;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetSHA1: string;
begin
  FLock.Enter;
  try
    Result:=FSHA1;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetThreadUsage(AID: Cardinal): Double;
var
  idx: Integer;
begin
  Result:=0;
  FLock.Enter;
  try
    idx:=FindThread(AID);
    if idx>-1 then
      Result:=PThreadRecord(FThreadList[idx]).CPUUsage;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetUserTime: int64;
begin
  FLock.Enter;
  try
    Result:=FSPI.UserTime.QuadPart;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetWinCount: Cardinal;
begin
  FLock.Enter;
  try
    Result:=FWinCount;
  finally
    FLock.Leave;
  end;
end;

procedure TProcMonThread.LoadThreadTexts(const AFilename: string;
  ANoLock: Boolean);
var
  i,idx: Integer;
  sl: TStringList;
begin
  if not FileExists(AFilename) then
    Exit;
  if not ANoLock then
    FLock.Enter;
  try
    sl:=TStringList.Create;
    try
      try sl.LoadFromFile(AFilename) except end;
      for i:=0 to FThreadList.Count-1 do begin
        idx:=sl.IndexOfName(IntToStr(PThreadRecord(FThreadList[i])^.ID));
        if (idx>-1) and (sl.ValueFromIndex[idx]<>'') then
          PThreadRecord(FThreadList[i])^.Text:=sl.ValueFromIndex[idx];
        end;
    finally
      sl.Free;
    end;
  finally
    if not ANoLock then
      FLock.Leave;
  end;
end;

procedure TProcMonThread.GetThreadRecord(AIndex: integer;
  var ARecord: TThreadRecord);
begin
  FLock.Enter;
  try
    ResetMemory(ARecord,SizeOf(ARecord));
    if AIndex<FThreadList.Count then
      ARecord:=PThreadRecord(FThreadList[AIndex])^;
  finally
    FLock.Leave;
  end;
end;

procedure TProcMonThread.GetThreadRecordByID(AID: Cardinal;
  var ARecord: TThreadRecord);
var
  i: integer;
begin
  FLock.Enter;
  try
    ResetMemory(ARecord,sizeof(ARecord));
    for i:=0 to FThreadList.Count-1 do
      if PThreadRecord(FThreadList[i])^.ID=AID then begin
        ARecord:=PThreadRecord(FThreadList[i])^;
        Break;
      end;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetTHCount: Cardinal;
begin
  FLock.Enter;
  try
    Result:=FThreadHistoryList.Count;
  finally
    FLock.Leave;
  end;
end;

function TProcMonThread.GetThreadCount: Cardinal;
begin
  FLock.Enter;
  try
    Result:=FSPI.ThreadCount;
  finally
    FLock.Leave;
  end;
end;

procedure TProcMonThread.GetThreadHistoryRecord(AIndex: Integer;
  var ARecord: TThreadHistoryRecord);
begin
  FLock.Enter;
  try
    ResetMemory(ARecord,SizeOf(ARecord));
    if AIndex<FThreadHistoryList.Count then
      ARecord:=PThreadHistoryRecord(FThreadHistoryList[AIndex])^;
  finally
    FLock.Leave;
  end;
end;

procedure TProcMonThread.RefreshData;
var
  status: Cardinal;
  pspi: PSystemProcessInformation;
  i,j,idx: Integer;
  br,p :Cardinal;
  r: PThreadRecord;
  c: UInt64;
  pe: Boolean;
  IsWow64: LongBool;
  hr: PThreadHistoryRecord;
  ok: bool;
  prd: UInt64;
  m: PModuleRecord;
  h: THandle;
begin
  FLock.Enter;
  try
    FCPCount:=0;
    FCIPB:=0;
    pe:=False;

    RefreshModules(FPID,FModList);
    RefreshHandles(FPID,FHandle,FHandleList);

    for i:=0 to FThreadList.Count-1 do
      PThreadRecord(FThreadList[i])._Exists:=False;

    status:=NtQuerySystemInformation(SystemProcessInformation,Buffer,BufferSize,@br);
    while (status=STATUS_BUFFER_OVERFLOW) or (status=STATUS_INFO_LENGTH_MISMATCH) do begin
      BufferSize:=br;
      ReallocMem(Buffer,BufferSize);
      status:=NtQuerySystemInformation(SystemProcessInformation,Buffer,BufferSize,@br);
    end;
    if status=STATUS_SUCCESS then begin
      pspi:=PSystemProcessInformation(Buffer);
      repeat
        if pspi.InheritedFromProcessId=FPID then begin
          Inc(FCPCount);
          if SameText(FName,string(pspi.ProcessName.Buffer)) then begin
            h:=GetProcessHandle(pspi.ProcessId);
            //if (h=0) or (h=INVALID_HANDLE_VALUE) then
              FCIPB:=FCIPB+pspi.VmCounters.PrivatePageCount
            {else begin
              FPMC:=GetProcessMemoryCountersByHandle(h);
              CloseHandle(h);
              FCIPB:=FCIPB+FPMC.PrivateUsage;
            end};
          end;
        end;
        if (pspi.ProcessId=FPID) then begin
          pe:=True;
          FWinCount:=GetWindowCount(FPID);
          ok:=False;
          prd:=GetTickCount64-FPeriod;
          if prd=0 then
            prd:=10;
          if not IsServer and Assigned(QueryProcessCycleTime) then
            ok:=QueryProcessCycleTime(FHandle,@c);
          if ok then
            FCPUUsage:=(c-FCycles)/(prd*100000)
          else begin
            c:=pspi.KernelTime.QuadPart+pspi.UserTime.QuadPart;
            FCPUUsage:=(((c-FCycles)/prd)/100)/FCPUCount;
          end;
          if (FCPUUsage>100) or (FCPUUsage<0) or (FCycles=0) then
            FCPUUsage:=0;
          FCycles:=c;
          FPeriod:=GetTickCount64;
          if not FInit then begin
            FIORead:=pspi.IOCounters.ReadTransferCount.QuadPart-FSPI.IOCounters.ReadTransferCount.QuadPart;
            FIOWrite:=pspi.IOCounters.WriteTransferCount.QuadPart-FSPI.IOCounters.WriteTransferCount.QuadPart;
            FIOOther:=pspi.IOCounters.OtherTransferCount.QuadPart-FSPI.IOCounters.OtherTransferCount.QuadPart;
          end;
          FName:={$IFDEF UNICODE}string{$ELSE}WideToAnsi{$ENDIF}(pspi.ProcessName.Buffer);
          if FImageName='' then
            FImageName:=FName;
          FSPI:=pspi^;
          //FPMC:=GetProcessMemoryCountersByHandle(FHandle);

          FIsSuspended:=True;
          for i:=0 to pspi.ThreadCount-1 do begin
            if (pspi.Threads[i].State<>5{Cardinal(StateWait)}) or (pspi.Threads[i].WaitReason<>5{Cardinal(Suspended)}) then
              FIsSuspended:=False;
            idx:=-1;
            for j:=0 to FThreadList.Count-1 do begin
              if FPID<>0 then
                p:=pspi.Threads[i].ClientId.UniqueThread
              else
                p:=i;
              if (PThreadRecord(FThreadList[j]).ID=p) then begin
                idx:=j;
                Break;
              end;
            end;
            if idx=-1 then begin
              new(r);
              if FPID<>0 then
                r.ID:=pspi.Threads[i].ClientId.UniqueThread
              else
                r.ID:=FThreadList.Count;
              r.MaxCPUUsage:=0;
              r.CPUUsage:=0;
              r.Cycles:=0;
              r._Period:=GetTickCount64-10;
              r.Handle:=GetThreadHandle(pspi.Threads[i].ClientId.UniqueThread);
              NtQueryInformationThread(r.Handle,ThreadQuerySetWin32StartAddress,@r.StartAddress,SizeOf(PVOID),nil);
              r.StartAddressString:=Format('0x%x',[r.StartAddress]);
              for j:=0 to FModList.Count-1 do begin
                m:=PModuleRecord(FModList[j]);
                if (r.StartAddress>=m.BaseAddress) and (r.StartAddress<=m.BaseAddress+m.ImageSize) then begin
                  r.StartAddressString:=Format('%s+0x%x',[m.Name,r.StartAddress-m.BaseAddress]);
                  Break;
                end;
              end;

              FThreadList.Add(r);
              idx:=FThreadList.Count-1;
            end;
            with PThreadRecord(FThreadList[idx])^ do begin
              _Exists:=True;
              State:=pspi.Threads[i].State;
              ContextSwitchCount:=pspi.Threads[i].ContextSwitchCount;
              WaitReason:=pspi.Threads[i].WaitReason;
              Priority:=pspi.Threads[i].Priority;
              BasePriority:=pspi.Threads[i].BasePriority;
              KernelTime:=pspi.Threads[i].KernelTime.QuadPart;
              UserTime:=pspi.Threads[i].UserTime.QuadPart;
              WaitTime:=pspi.Threads[i].WaitTime;
              CreateTime:=FileTimeToDateTime(TFiletime(pspi.Threads[i].CreateTime),True);

              ok:=false;
              prd:=GetTickCount64-_Period;
              if prd=0 then
                prd:=10;
              if not IsServer and Assigned(QueryThreadCycleTime) and (Handle<>INVALID_HANDLE_VALUE) and (Handle<>0) then
                ok:=QueryThreadCycleTime(Handle,@c);
              if not ok then begin
                c:=pspi.Threads[i].KernelTime.QuadPart+pspi.Threads[i].UserTime.QuadPart;
                CPUUsage:=(((c-Cycles)/prd)/100)/FCPUCount;
              end else
                CPUUsage:=(c-Cycles)/(prd*100000);
              if (CPUUsage>100) or (CPUUsage<0) or (Cycles=0) then
                CPUUsage:=0;
              if CPUUsage>MaxCPUUsage then
                MaxCPUUsage:=CPUUsage;
              Cycles:=c;
              _Period:=GetTickCount64;
            end;
          end;

          if (FBits=0) then begin
            if (FHandle<>INVALID_HANDLE_VALUE) and (FHandle<>0) then begin
              if Is64 then begin
                IsWow64:=False;
                if Assigned(IsWow64Process) then
                  IsWow64Process(Handle,IsWow64);
                if not IsWow64 then
                  FBits:=64
                else
                  FBits:=32;
              end else
                FBits:=32;
            end;

            if (FBits=0) and not FIsSuspended then begin
              if Is64 then
                FBits:=64
              else
                FBits:=32;
            end;
          end;

          FInit:=False;
          //Break;
        end;

        if pspi^.NextEntryDelta=0 then
          Break;
        pspi:=PSystemProcessInformation(PAnsiChar(pspi)+pspi^.NextEntryDelta);
      until False;
    end;
    i:=0;
    while i<FThreadList.Count do
      if not PThreadRecord(FThreadList[i])._Exists then begin
        if FSaveTH and (not FSaveTHOnlyDebug or (PThreadRecord(FThreadList[i])^.Text<>'')) then begin
          new(hr);
          hr.ID:=PThreadRecord(FThreadList[i])^.ID;
          hr.CreateTime:=PThreadRecord(FThreadList[i])^.CreateTime;
          hr.TerminateTime:=IncSecond(Now,-1);
          hr.MaxCPUUsage:=PThreadRecord(FThreadList[i])^.MaxCPUUsage;
          hr.Text:=PThreadRecord(FThreadList[i])^.Text;
          hr.StartAddressString:=PThreadRecord(FThreadList[i])^.StartAddressString;
          FThreadHistoryList.Add(hr);
        end;
        CloseHandle(PThreadRecord(FThreadList[i])^.Handle);
        Dispose(PThreadRecord(FThreadList[i]));
        FThreadList.Delete(i);
      end else
        Inc(i);
    FThreadList.Capacity:=FThreadList.Count;
    FProcessExists:=pe;
  finally
    FLock.Leave;
  end;
end;

class procedure TProcMonThread.RefreshEnvironment(APID: Cardinal; AHandle: THandle; AList: TStringList);
var
  h: Boolean;
  n: {$IFDEF NATIVEINT}NativeUint{$ELSE}Cardinal{$ENDIF};
  pbi: PROCESS_BASIC_INFORMATION;
  pb: TPEB;
  ib: TProcessParameters;
  s: string;
  mbi: TMemoryBasicInformation;
  envbuf: TBytes;
  br :Cardinal;
  p: PWideChar;
begin
  AList.Clear;
  h:=False;
  if AHandle=0 then begin
    AHandle:=GetProcessHandle(APID{,PROCESS_DUP_HANDLE});
    h:=True;
  end;
  if (AHandle=0) or (AHandle=INVALID_HANDLE_VALUE) then
    Exit;
  try
    n:=0;
    s:='';
    if NtQueryInformationProcess(AHandle,ProcessBasicInformation,@pbi,SizeOf(pbi),@n)=0 then begin
      if ReadProcessMemory(AHandle,pbi.PebBaseAddress,@pb,SizeOf(pb),n) then
        if ReadProcessMemory(AHandle,Pointer(pb.ProcessParameters),@ib,SizeOf(ib),n) then begin
          if VirtualQueryEx(AHandle,ib.Environment,mbi,SizeOf(mbi))=0 then
            br:=(mbi.RegionSize-(ULONG_PTR(ib.Environment)-ULONG_PTR(mbi.BaseAddress)))
          else
            br:=ib.EnvironmentSize;
          if (br=0) or (br>1048576) then begin
            if (os<=osXP) then
              br:=(mbi.RegionSize-(ULONG_PTR(ib.Environment)-ULONG_PTR(mbi.BaseAddress)))
            else
              br:=MAXWORD;
          end;
          repeat
            SetLength(envbuf,br);
            if ReadProcessMemory(AHandle,Pointer(ib.Environment),@envbuf[0],br,n) and (n>0) then
              Break;
            br:=br div 2;
          until (br<256);
          if (n>0) then begin
            p:=PWideChar(@envbuf[0]);
            while p^<> #0 do begin
              if p[0]<>'=' then
                s:=s+{$IFNDEF UNICODE}WideCharToString{$ENDIF}(p)+sLineBreak;
              Inc(p,Length(WideString(p))+1);
            end;
            s:=Trim(s);
          end;
        end;
    end;
    AList.Text:=s;
  finally
    if h then
      CloseHandle(AHandle);
  end;
end;

class procedure TProcMonThread.RefreshHandles(APID: Cardinal; AHandle: THandle; AList: TList);
var
  sht: TSystemHandleType;
  Buffer, p: Pointer;
  br,status,sz,c: Cardinal;
  pshi: PSystemHandleTableEntryInfo;
  param: TGetHandleInfoThreadParam;
  oh,th: THandle;
  r: PHandleRecord;
  i,idx: integer;
  h: Boolean;
  {$IFNDEF UNICODE}buf: array[0..MAX_PATH-1] of AnsiChar;{$ENDIF}
  {$IFDEF FPC}tid: NativeUint;{$ENDIF}
begin
  h:=False;
  if AHandle=0 then begin
    AHandle:=GetProcessHandle(APID,PROCESS_DUP_HANDLE);
    h:=True;
  end;
  if (AHandle=0) or (AHandle=INVALID_HANDLE_VALUE) then
    Exit;
  try
    for i:=0 to AList.Count-1 do
      PHandleRecord(AList[i])._Exists:=False;

    sz:=SizeOf(TSystemHandleInformation);
    Buffer:=AllocMem(sz);
    try
      status:=NtQuerySystemInformation(SystemHandleInformation,Buffer,sz,@br);
      while (status=STATUS_BUFFER_OVERFLOW) or (status=STATUS_INFO_LENGTH_MISMATCH) do begin
        sz:=br;
        ReallocMem(Buffer,sz);
        status:=NtQuerySystemInformation(SystemHandleInformation,Buffer,sz,@br);
      end;
      if status=STATUS_SUCCESS then begin
        c:=0;
        pshi:=PSystemHandleTableEntryInfo(PAnsiChar(Buffer)+{$IFDEF WIN64}2*{$ENDIF}SizeOf(DWORD));
        p:=AllocMem(sz);
        try
          repeat
            if (pshi^.UniqueProcessId=APID) then begin
              idx:=-1;
              for i:=0 to AList.Count-1 do
                if (PHandleRecord(AList[i]).Handle=pshi^.HandleValue) then begin
                  idx:=i;
                  r:=PHandleRecord(AList[idx]);
                  Break;
                end;
              if idx=-1 then begin
                new(r);
                ZeroMemory(r,SizeOf(r^));
              end;
              r._Exists:=True;
              if (idx=-1) or (r.Typ=integer(OB_TYPE_FILE)) then begin
                r.PID:=pshi^.UniqueProcessId;
                r.Handle:=pshi^.HandleValue;
                r.Access:=pshi^.GrantedAccess;
                r.Typ:=pshi^.ObjectTypeIndex;
                r.FilePos:=-1;
                status:=0;
                if APID<>GetCurrentProcessID then begin
                  status:=NtDuplicateObject(AHandle,r.Handle,GetCurrentProcess,@oh,0,0,DUPLICATE_SAME_ACCESS);
                  if status<>STATUS_SUCCESS then
                    status:=NtDuplicateObject(AHandle,r.Handle,GetCurrentProcess,@oh,0,0,0);
                end else
                  oh:=r.Handle;
                if status=STATUS_SUCCESS then begin
                  NtQueryObject(oh,ObjectTypeInformation,nil,0,@sz);
                  ReallocMem(p,sz);
                  status:=NtQueryObject(oh,ObjectTypeInformation,p,sz,nil);
                  if status=STATUS_SUCCESS then begin
                    {$IFDEF WIN64}
                    r.TypeName:=PWideChar(PAnsiChar(p)+SizeOf(TObjectTypeInformation));
                    {$ELSE}
                    r.Typename:=PObjectTypeInformation(p).Name.Buffer;
                    {$ENDIF}
                    for sht:=Low(TSystemHandleType) to High(TSystemHandleType) do
                      if SameText(cSystemHandleType[sht],r.Typename) then
                        r.Typ:=Integer(sht);
                  end else
                    if r.Typ>integer(High(TSystemHandleType)) then
                      r.Typename:=Format('Unknown type %d',[r.Typ])
                    else
                      r.Typename:=cSystemhandleType[TSystemHandleType(r.Typ)];
                  ZeroMemory(p,sz);
                  case TSystemHandleType(r.Typ) of
                    OB_TYPE_PROCESS: begin
                      sz:=SizeOf(TProcessBasicInformation);
                      ReAllocMem(p,sz);
                      status:=NtQueryInformationProcess(oh,ProcessBasicInformation,p,sz,nil);
                      if status=STATUS_SUCCESS then
                        r.Name:=Format('PID: %d',[PProcessBasicInformation(p)^.UniqueProcessID]);
                    end;
                    OB_TYPE_THREAD: begin
                      sz:=SizeOf(TThreadBasicInformation);
                      ReAllocMem(p,sz);
                      status:=NtQueryInformationThread(oh,ThreadBasicInformation,p,sz,nil);
                      if status=STATUS_SUCCESS then
                        r.Name:=Format('TID: %d (PID: %d)',[PThreadBasicInformation(p)^.ClientID.UniqueThread,PThreadBasicInformation(p)^.ClientID.UniqueProcess]);
                    end;
                    OB_TYPE_FILE: begin
                      ResetMemory(param,SizeOf(param));
                      param.Handle:=oh;
                      param.Typ:=TSystemHandleType(r.Typ);
                      th:=BeginThread(nil,0,GetHandleInfoThreadExecute,@param,0,{$IFDEF FPC}tid{$ELSE}sz{$ENDIF});
                      try
                        if (WaitForSingleObject(th,200)=WAIT_TIMEOUT) then
                          TerminateThread(th,0);
                      finally
                        CloseHandle(th);
                      end;
                      if idx=-1 then begin
                        r.Name:=string(param.FileName);
                        if r.Name<>'' then
                          r.Name:=KernelNameToFilename(r.Name);
                      end;
                      if param.FilePos.QuadPart>-1 then
                        r.FilePos:=param.FilePos.QuadPart;
                    end else begin
                      ResetMemory(param,SizeOf(param));
                      param.Handle:=oh;
                      param.Typ:=TSystemHandleType(r.Typ);
                      th:=BeginThread(nil,0,GetHandleInfoThreadExecute,@param,0,{$IFDEF FPC}tid{$ELSE}sz{$ENDIF});
                      try
                        if (WaitForSingleObject(th,200)=WAIT_TIMEOUT) then
                          TerminateThread(th,0);
                      finally
                        CloseHandle(th);
                      end;
                      r.Name:=string(param.FileName);
                      if r.Name<>'' then begin
                        if (r.Typ<>integer(OB_TYPE_KEY)) and (PosText('\REGISTRY\',r.Name)=1) then begin
                          r.Typ:=integer(OB_TYPE_KEY);
                          r.Typename:=cSystemhandleType[TSystemHandleType(r.Typ)];
                        end;
                        if r.Typ=integer(OB_TYPE_KEY) then begin
                          r.Name:=StringReplace(r.Name,'\REGISTRY\MACHINE','HKLM',[rfIgnoreCase]);
                          r.Name:=StringReplace(r.Name,'\REGISTRY\USER','HKU',[rfIgnoreCase]);
                        end;
                      end;
                    end;
                  end;

                  if APID<>GetCurrentProcessID then
                    NtClose(oh);
                end;

                if (idx=-1) and (r.Name<>'') then
                  AList.Add(r);
              end;

            end;
            Inc(c);
            if c>PDWORD(Buffer)^ then
              Break;
            pshi:=PSystemHandleTableEntryInfo(PAnsiChar(pshi)+SizeOf(pshi^));
          until pshi=nil;
        finally
          Freemem(p);
        end;
      end;
    finally
      Freemem(Buffer);
    end;
  finally
    if h then
      CloseHandle(AHandle);
  end;

  i:=0;
  while i<AList.Count do
    if not PHandleRecord(AList[i])._Exists then
      AList.Delete(i)
    else
      Inc(i);
  AList.Capacity:=AList.Count;
end;

class procedure TProcMonThread.RefreshModules(APID: Cardinal; AList: TList);
const
  TH32CS_SNAPMODULE32   = $00000010;
var
  i,idx: integer;
  ms: THandle;
  me32: TMODULEENTRY32;
  ok: Boolean;
  r: PModuleRecord;
  s: string;
  ba: NativeUInt;
begin
  try
    for i:=0 to AList.Count-1 do
      PModuleRecord(AList[i])._Exists:=False;

    ms:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE or TH32CS_SNAPMODULE32,APID);
    if (ms<>INVALID_HANDLE_VALUE) then
      try
        me32.dwSize:=sizeof(TMODULEENTRY32);
        ok:=Module32First(ms,me32);
        while ok do begin
          ba:=PNativeUInt(@(me32.modBaseAddr))^;
          if PosText(Format('%x',[ba]),s)=0 then begin
            idx:=-1;
            for i:=0 to AList.Count-1 do
              if (PModuleRecord(AList[i]).BaseAddress=ba) then begin
                idx:=i;
                Break;
              end;
            if idx=-1 then begin
              new(r);
              ZeroMemory(r,SizeOf(r^));
              r.Name:=string(me32.szModule);
              r.ImageName:=string(me32.szExePath);
              if not FileExists(r.ImageName) then
                r.ImageName:=StringReplace(r.ImageName,'\??\','',[rfIgnoreCase]);
              if not FileExists(r.ImageName) then
                r.ImageName:=ExpandEnvVars(r.ImageName);
              if not FileExists(r.ImageName) then
                r.ImageName:=ExpandFilename(FileSearch(r.ImageName,GetWinSysDir));
              r.BaseAddress:=ba;
              r.ImageSize:=me32.modBaseSize;
              if FileExists(r.ImageName) then
                GetFileVerInfo(r.ImageName,r.VersionInfo);
              r.Size:=GetFileSize(r.ImageName);
              if r.Size<0 then
                r.Size:=0;
              AList.Add(r);
              idx:=AList.Count-1;
            end;
            if idx<>-1 then
              PModuleRecord(AList[idx])^._Exists:=True;
            s:=s+Format('%x,',[ba]);
          end;
          ok:=Module32Next(ms,me32);
        end;
      finally
        CloseHandle(ms);
      end
    {else begin
      ph:=GetProcessHandle(APID);
      if (ph<>INVALID_HANDLE_VALUE) and (ph<>0) then
        try
          if Assigned(EnumProcessModulesEx) and EnumProcessModulesEx(ph,nil,0,n,LIST_MODULES_ALL) then begin
            hm:=AllocMem(n);
            ok:=EnumProcessModulesEx(ph,hm,n,n,LIST_MODULES_ALL);
          end else begin
            EnumProcessModules(ph,nil,0,n);
            hm:=AllocMem(n);
            ok:=EnumProcessModules(ph,hm,n,n);
          end;
          if ok then begin
            c:=n div SizeOf(HMODULE);
          end;
        finally
          CloseHandle(ph);
        end;
    end};

    i:=0;
    while i<AList.Count do
      if not PModuleRecord(AList[i])._Exists then
        AList.Delete(i)
      else
        Inc(i);
    AList.Capacity:=AList.Count;
  finally
  end;
end;

class procedure TProcMonThread.RefreshWindows(APID: Cardinal; AOnlyVisible: Boolean; AList: TList);
var
  wep: PWinEnumParam;
begin
  new(wep);
  try
    AList.Clear;
    wep.PID:=APID;
    wep.List:=AList;
    wep.OnlyVisible:=AOnlyVisible;
    wep.Count:=0;
    EnumWindows(@EnumWindowsProc,lParam(Integer(wep)));
  finally
    dispose(wep);
  end;
end;

procedure TProcMonThread.SaveThreadTexts(const AFilename: string;
  ANoLock: Boolean);
var
  i: Integer;
  sl: TStringList;
begin
  if not ANoLock then
    FLock.Enter;
  try
    sl:=TStringList.Create;
    try
      for i:=0 to FThreadList.Count-1 do
        sl.Add(Format('%d=%s',[PThreadRecord(FThreadList[i])^.ID,PThreadRecord(FThreadList[i])^.Text]));
      try
        sl.SaveToFile(AFilename)
      except
        sleep(100);
        try sl.SaveToFile(AFilename) except end;
      end;
    finally
      sl.Free;
    end;
  finally
    if not ANoLock then
      FLock.Leave;
  end;
end;

procedure TProcMonThread.SetInterval(const Value: Cardinal);
begin
  FLock.Enter;
  try
    if Value>250 then
      FInterval:=250
    else
      FInterval:=Value;
  finally
    FLock.Leave;
  end;
end;

procedure TProcMonThread.SetOnInterval(const Value: TProcMonNotifyEvent);
begin
  FLock.Enter;
  try
    FOnInterval:=Value;
  finally
    FLock.Leave;
  end;
end;

procedure TProcMonThread.SetPID(const Value: Cardinal);
begin
  FLock.Enter;
  try
    if Value<>FPID then begin
      FProcessExists:=True;
      ClearValues;
      FPID:=Value;
      FHandle:=GetProcessHandle(FPID,PROCESS_DUP_HANDLE);
      FImageName:=GetProcessImageNameByHandle(FHandle);
      if FileExists(FImageName) then
        FSHA1:=BytesToHEX(CreateFileHash(FImageName));
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TProcMonThread.SetSaveTH(const Value: boolean);
begin
  FLock.Enter;
  try
    FSaveTH:=Value;
  finally
    FLock.Leave;
  end;
end;

procedure TProcMonThread.SetSaveTHOnlyDebug(const Value: boolean);
begin
  FLock.Enter;
  try
    FSaveTHOnlyDebug:=Value;
  finally
    FLock.Leave;
  end;
end;

procedure TProcMonThread.SetThreadText(AID: Cardinal; const AText: string);
var
  idx: Integer;
begin
  FLock.Enter;
  try
    idx:=FindThread(AID);
    if (idx=-1) then
      RefreshData;
    idx:=FindThread(AID);
    if idx>-1 then
      PThreadRecord(FThreadList[idx]).Text:=AText;
  finally
    FLock.Leave;
  end;
end;

initialization
  InitNativeAPI;
end.


