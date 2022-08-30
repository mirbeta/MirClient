{*******************************************************}
{                                                       }
{       MiTeC System Information Component Suite        }
{                System Monitor                         }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MSI_SysMon;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, System.SyncObjs,
     {$ELSE}
     Windows, SysUtils, Classes, ExtCtrls, SyncObjs,
     {$ENDIF}
     MiTeC_Windows, MSI_Defs, MiTeC_NativeDefs, MiTeC_Routines;

type
  TUsages = array of int64;

  TCPUUsages = array of double;

  TSysMonThread = class;

  TSysMonNotifyEvent = procedure(Sender: TSysMonThread) of object;

  TSysMonThread = class(TThread)
  {$IFDEF BDS3PLUS}
  strict private
    class var FInstance: TSysMonThread;
    class var InternalLock: TCriticalSection;
  {$ENDIF}
  private
    FPeriod: UInt64;
    FOnInterval: TSysMonNotifyEvent;
    FValues, FLastValues: TUsages;
    FUsages,FClocks,FMaxClocks: TCPUUsages;
    FCPUCount,FCPUPackageCount,FCPUCoreCount: Byte;
    FInterval: Cardinal;
    FValue: double;
    FPS: TSystemPowerStatus;
    FTL: TStringlist;
    procedure SetInterval(const Value: Cardinal);
    procedure ReadCPUUsage;
    function GetLogCPUUsage(Index: Byte): double;
    procedure DoSync;
    function GetCPUUsage: Double;
    function GetInterval: Cardinal;
    function GetOnInterval: TSysMonNotifyEvent;
    procedure SetOnInterval(const Value: TSysMonNotifyEvent);
    function GetCPUCoreCount: Byte;
    function GetCPUCoreFreq(Index: Byte): integer;
    function GetCPUCoreMaxFreq(Index: Byte): integer;
    function GetPowerStatus: Boolean;
    function GetBatRem: Byte;
    function GetBatRemSec: int64;
    function GetBatStatus: Cardinal;
  protected
    constructor CreateSysMon;
    procedure Execute; override;
  public
    class function Create: TSysMonThread;
    destructor Destroy; override;

    class function InstallDate: TDateTime;
    class function BootTime: TDateTime;
    class function LastShutdown: TDateTime;
    class function LogicalCPUCount: Byte;

    class function SessionID: Cardinal;

    class function MemoryLoad: Cardinal;
    class function InstalledMemory: Int64;
    class function TotalPhysMemory: Int64;
    class function FreePhysMemory: Int64;
    class function LimitCommitCharge: int64;
    class function CurrentCommitCharge: Int64;
    class function TotalVirtualMemory: Int64;
    class function FreeVirtualMemory: Int64;
    class function SystemCache: Int64;

    class function TotalPageFile: int64;
    class function FreePageFile: Int64;

    class function ThreadCount: Cardinal;
    class function ProcessCount: Cardinal;
    class function HandleCount: Cardinal;

    class function SystemDisk: string;

    class function DiskFreeSpace(const ADisk: string): Int64;
    class function DiskUserFreeSpace(const ADisk: string): Int64;
    class function DiskTotalSpace(const ADisk: string): Int64;
    class function DiskFileSystem(const ADisk: string): string;

    class function CPUName: string;
    class function SystemProductName: string;
    class function BaseBoardName: string;
    class function BIOS: string;

    procedure SetThreadText(AID: Cardinal; const AText: string);
    function GetThreadText(AID: Cardinal): string;
    procedure RemoveThreadText(AID: Cardinal);
    procedure SaveThreadTexts(const AFilename: string; ANoLock: Boolean = False);

    property LogicalCPUUsage[Index: Byte]: double read GetLogCPUUsage;
    property CPUCoreFreq[Index: Byte]: integer read GetCPUCoreFreq;
    property CPUCoreMaxFreq[Index: Byte]: integer read GetCPUCoreMaxFreq;
    property CPUUsage: Double read GetCPUUsage;
    property CPUCoreCount: Byte read GetCPUCoreCount;

    property ACPower: boolean read GetPowerStatus;
    property BatteryRemaining: Byte read GetBatRem;
    property BatterySecondsRemaining: int64 read GetBatRemSec;
    property BatteryStatus: Cardinal read GetBatStatus;

    property Interval: Cardinal read GetInterval write SetInterval;
    property OnInterval: TSysMonNotifyEvent read GetOnInterval write SetOnInterval;
  end;


implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry, {$IFDEF TRIAL}VCL.Dialogs,{$ENDIF}
     {$ELSE}
     Registry, {$IFDEF TRIAL}Dialogs,{$ENDIF}
     {$ENDIF}
     MiTeC_NativeAPI, MiTeC_PowrProf, MiTeC_Datetime, MiTeC_PsAPI, MiTeC_StrUtils,
     MiTeC_RegUtils, MiTeC_WMI, MiTeC_WbemScripting_TLB;

{$IFNDEF BDS3PLUS}
var
  InternalLock: TCriticalSection;
  FInstance: TSysMonThread = nil;
{$ENDIF}

function NativeGetCPUData(var Values: TUsages): Integer;
var
  n: cardinal;
  i: integer;
  spt: Pointer;
  sbi: TSystemBasicInformation;
begin
  try
    Result:=NtQuerySystemInformation(SystemBasicInformation,@sbi,SizeOf(sbi),nil);
    if Result<>STATUS_SUCCESS then
      n:=1
    else
      n:=sbi.NumberOfProcessors;
    spt:=AllocMem(n*(SizeOf(TSystemProcessorTimes)+4));
    Result:=NtQuerySystemInformation(SystemProcessorPerformanceInformation,spt,n*(SizeOf(TSystemProcessorTimes)+4),@Result);
    for i:=0 to n-1  do
      with PSystemProcessorTimes(PAnsiChar(spt)+i*(sizeof(TSystemProcessorTimes)+4))^ do
        Values[i]:=IdleTime.QuadPart;
    FreeMem(spt);
  except
    Result:=GetLastError;
  end;
end;

{ TSysMonThread }

class function TSysMonThread.BaseBoardName: string;
var
  s: string;
begin
  s:='';
  Result:='';
  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey('HARDWARE\DESCRIPTION\System\BIOS',False) then begin
        if ValueExists('BaseBoardManufacturer') then
          Result:=Result+ReadString('BaseBoardManufacturer');
        if ValueExists('BaseBoardProduct') then
          Result:=Result+' '+ReadString('BaseBoardProduct');
        if ValueExists('BaseBoardVersion') then begin
          s:=ReadString('BaseBoardVersion');
          if not SameText(s,'N/A') and (s<>'') then
            Result:=Result+' '+s;
        end;
        Result:=Trim(Result);
        CloseKey;
      end;
    finally
      Free;
    end;
end;

class function TSysMonThread.BIOS: string;
begin
  Result:='';
  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey('HARDWARE\DESCRIPTION\System\BIOS',False) then begin
        if ValueExists('BIOSVendor') then
          Result:=Result+ReadString('BIOSVendor');
        if ValueExists('BIOSVersion') then
          Result:=Result+' '+ReadString('BIOSVersion');
        if ValueExists('BIOSReleaseDate') then
          Result:=Result+' '+ReadString('BIOSReleaseDate');
        Result:=Trim(Result);
        CloseKey;
      end;
    finally
      Free;
    end;
end;

class function TSysMonThread.BootTime: TDateTime;
begin
  Result:=Now-(GetTickCount64/1000)/(24*3600);
end;

function FormatCPUName(const AName: string): string;
begin
  Result:=AName;
  Result:=FastStringReplace(Result,'(R)','');
  Result:=FastStringReplace(Result,'(TM)',' ');
  Result:=FastStringReplace(Result,'Genuine','');
  Result:=FastStringReplace(Result,'Procesor','');
  Result:=FastStringReplace(Result,'Processor','');
  Result:=FastStringReplace(Result,'Technology','');
  Result:=FastStringReplace(Result,'CPU','');
  Result:=FastStringReplace(Result,'@','-');
  Result:=Trim(Result);
  Result:=StripSpaces(Result);
end;

class function TSysMonThread.CPUName: string;
begin
  Result:='';
  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey('HARDWARE\DESCRIPTION\System\CentralProcessor\0',False) then begin
        if ValueExists('ProcessorNameString') then
          Result:=ReadString('ProcessorNameString')
        else if ValueExists('Identifier') then
          Result:=ReadString('Identifier');
        Result:=StripSpaces(Result);
        Result:=FastStringReplace(Result,'@','-');
        CloseKey;
      end;
    finally
      Free;
    end;
end;

class function TSysMonThread.Create: TSysMonThread;
begin
  if not Assigned(InternalLock) then
    InternalLock:=TCriticalSection.Create;
  InternalLock.Enter;
  try
    if (FInstance=nil) then
      FInstance:=CreateSysMon;
    Result:=FInstance;
  finally
    InternalLock.Leave;
  end;
end;

constructor TSysMonThread.CreateSysMon;
begin
  inherited Create(True);
  FreeOnTerminate:=False;
  FInterval:=1000;
  GetCPUTopology(FCPUPackageCount,FCPUCoreCount,FCPUCount);
  FCPUCount:=NativeGetCPUCount;
  SetLength(FClocks,FCPUCoreCount);
  SetLength(FMaxClocks,FCPUCoreCount);
  SetLength(FUsages,FCPUCount);
  SetLength(FLastValues,FCPUCount);
  SetLength(FValues,FCPUCount);
  FTL:=TStringList.Create;
  SetThreadText(ThreadID,ClassName);
  SetThreadText(MainThreadID,'Main thread');
end;

destructor TSysMonThread.Destroy;
begin
  FOnInterval:=nil;
  Finalize(FClocks);
  Finalize(FMaxClocks);
  Finalize(FUsages);
  Finalize(FValues);
  Finalize(FLastValues);
  FreeAndNil(InternalLock);
  FTL.Clear;
  FTL.Free;
  inherited;
  FInstance:=nil;
end;

class function TSysMonThread.DiskFileSystem(const ADisk: string): string;
var
  sn,mcl,fsf: Cardinal;
  vn,fsn: array[0..MAX_PATH] of char;
begin
  Result:='';
  if GetVolumeInformation(PChar(ADisk),vn,SizeOf(vn),@sn,mcl,fsf,fsn,SizeOf(fsn)) then
    Result:=string(fsn);
end;

class function TSysMonThread.DiskFreeSpace(const ADisk: string): Int64;
var
  t,f,tf: Int64;
begin
  Result:=0;
  if GetDiskFreeSpaceEx(PChar(ADisk),f,t,@tf) then
    Result:=tf;
end;

class function TSysMonThread.DiskTotalSpace(const ADisk: string): Int64;
var
  t,f,tf: Int64;
begin
  Result:=0;
  if GetDiskFreeSpaceEx(PChar(ADisk),f,t,@tf) then
    Result:=t;
end;

class function TSysMonThread.DiskUserFreeSpace(const ADisk: string): Int64;
var
  t,f,tf: Int64;
begin
  Result:=0;
  if GetDiskFreeSpaceEx(PChar(ADisk),f,t,@tf) then
    Result:=f;
end;

procedure TSysMonThread.DoSync;
begin
  if Assigned(FOnInterval) then
    FOnInterval(Self);
end;

procedure TSysMonThread.Execute;
var
  a: double;
  i,c,n,j,r: integer;
  se: TSimpleEvent;
  ppi: Pointer;
  pi: TProcessorPowerInformation;
begin
  se:=TSimpleEvent.Create{$IFDEF BDS35PLUS}(nil,False,False,''){$ENDIF};
  try
    FPeriod:=GetTickCount64-10;
    while not Terminated do begin
      GetSystemPowerStatus(FPS);

      ReadCPUUsage;
      a:=0;
      for i:=0 to High(FUsages) do
        a:=a+FUsages[i];
      FValue:=a/Length(FUsages);
      if (FValue>100) or (FValue<0) then
        FValue:=0;

      c:=FCPUCount;
      ppi:=nil;
      if Assigned(CallNtPowerInformation) then begin
        ppi:=AllocMem(c*SizeOf(TProcessorPowerInformation));
        try
          r:=CallNtPowerInformation(ProcessorInformation,nil,0,ppi,c*SizeOf(TProcessorPowerInformation));
          while (r<>STATUS_SUCCESS) do begin
            Inc(c);
            ReallocMem(ppi,c*SizeOf(TProcessorPowerInformation));
            r:=CallNtPowerInformation(ProcessorInformation,nil,0,ppi,c*SizeOf(TProcessorPowerInformation));
          end;
          if (c>1) and Odd(c) then
            Dec(c);
          n:=c div FCPUCoreCount;
          if n=0 then
            n:=1;
          j:=0;
          i:=0;
          while i<c do begin
            pi:=PProcessorPowerInformation(PProcessorPowerInformation(PAnsiChar(ppi)+i*SizeOf(TProcessorPowerInformation)))^;
            FMaxClocks[j]:=pi.MaxMhz;
            FClocks[j]:=pi.CurrentMhz;
            Inc(j);
            inc(i,n);
          end;
        finally
          FreeMem(ppi);
        end;
      end;

      if Assigned(FOnInterval) and not Terminated then
        Synchronize(DoSync);

      if not Terminated then
        se.WaitFor(FInterval);
    end;
  finally
    se.Free;
  end;
end;

class function TSysMonThread.CurrentCommitCharge: Int64;
var
  MSEX: TMemoryStatusEx;
  MS: TMemoryStatus;
begin
  if Assigned(GlobalMemoryStatusEx_) then begin
    ResetMemory(MSEX,SizeOf(MSEX));
    MSEX.dwLength:=SizeOf(MSEX);
    GlobalMemoryStatusEx_(@MSEX);
    Result:=MSEX.ullTotalPageFile-MSEX.ullAvailPageFile;
  end else begin
    ResetMemory(MS,SizeOf(MS));
    MS.dwLength:=SizeOf(MS);
    GlobalMemoryStatus(MS);
    Result:=MS.dwTotalPageFile-MS.dwAvailPageFile;
  end;
end;

class function TSysMonThread.FreePageFile: Int64;
var
  wmiServices: ISWbemServices;
  wmi: TInstances;
begin
  if not WMIConnect('','','',Rootnamespace,wmiServices) then
    Exit;
  try
    WMICommand(wmiServices,'Win32_PageFileUsage',wmi);
    Result:=GetFileSize(GetInstancePropertyValue(wmi,'Name'))-StrToIntDef(GetInstancePropertyValue(wmi,'CurrentUsage'),0);
  finally
    WMIDisconnect(wmiServices);
    Finalize(wmi);
  end;
end;

class function TSysMonThread.FreePhysMemory: Int64;
var
  MSEX: TMemoryStatusEx;
  MS: TMemoryStatus;
begin
  if Assigned(GlobalMemoryStatusEx_) then begin
    ResetMemory(MSEX,SizeOf(MSEX));
    MSEX.dwLength:=SizeOf(MSEX);
    GlobalMemoryStatusEx_(@MSEX);
    Result:=MSEX.ullAvailPhys;
  end else begin
    ResetMemory(MS,SizeOf(MS));
    MS.dwLength:=SizeOf(MS);
    GlobalMemoryStatus(MS);
    Result:=MS.dwAvailPhys;
  end;
end;

class function TSysMonThread.FreeVirtualMemory: Int64;
var
  MSEX: TMemoryStatusEx;
  MS: TMemoryStatus;
begin
  if Assigned(GlobalMemoryStatusEx_) then begin
    ResetMemory(MSEX,SizeOf(MSEX));
    MSEX.dwLength:=SizeOf(MSEX);
    GlobalMemoryStatusEx_(@MSEX);
    Result:=MSEX.ullAvailVirtual;
  end else begin
    ResetMemory(MS,SizeOf(MS));
    MS.dwLength:=SizeOf(MS);
    GlobalMemoryStatus(MS);
    Result:=MS.dwAvailVirtual;
  end;
end;

function TSysMonThread.GetBatRem: Byte;
begin
  InternalLock.Enter;
  try
    Result:=FPS.BatteryLifePercent;
  finally
    InternalLock.Leave;
  end;
end;

function TSysMonThread.GetBatRemSec: int64;
begin
  InternalLock.Enter;
  try
    Result:=FPS.BatteryLifeTime;
  finally
    InternalLock.Leave;
  end;
end;

function TSysMonThread.GetBatStatus: Cardinal;
begin
  InternalLock.Enter;
  try
    Result:=FPS.BatteryFlag;
  finally
    InternalLock.Leave;
  end;
end;

function TSysMonThread.GetCPUCoreCount: Byte;
begin
  InternalLock.Enter;
  try
    Result:=FCPUCoreCount;
  finally
    InternalLock.Leave;
  end;
end;

function TSysMonThread.GetCPUCoreFreq(Index: Byte): integer;
begin
  InternalLock.Enter;
  try
    Result:=Round(FClocks[Index]);
  finally
    InternalLock.Leave;
  end;
end;

function TSysMonThread.GetCPUCoreMaxFreq(Index: Byte): integer;
begin
  InternalLock.Enter;
  try
    Result:=Round(FMaxClocks[Index]);
  finally
    InternalLock.Leave;
  end;
end;

function TSysMonThread.GetCPUUsage: Double;
begin
  InternalLock.Enter;
  try
    Result:=FValue;
  finally
    InternalLock.Leave;
  end;
end;

function TSysMonThread.GetInterval: Cardinal;
begin
  InternalLock.Enter;
  try
    Result:=FInterval;
  finally
    InternalLock.Leave;
  end;
end;

function TSysMonThread.GetLogCPUUsage(Index: Byte): double;
begin
  InternalLock.Enter;
  try
    Result:=FUsages[Index];
  finally
    InternalLock.Leave;
  end;
end;

function TSysMonThread.GetOnInterval: TSysMonNotifyEvent;
begin
  InternalLock.Enter;
  try
    Result:=FOnInterval;
  finally
    InternalLock.Leave;
  end;
end;

function TSysMonThread.GetPowerStatus: boolean;
begin
  InternalLock.Enter;
  try
    Result:=FPS.ACLineStatus<>0;
  finally
    InternalLock.Leave;
  end;
end;

function TSysMonThread.GetThreadText(AID: Cardinal): string;
var
  idx: Integer;
begin
  InternalLock.Enter;
  try
    Result:='';
    idx:=FTL.IndexOfName(IntToStr(AID));
    if (idx>-1) then
      Result:=FTL.ValueFromIndex[idx];
  finally
    InternalLock.Leave;
  end;
end;

class function TSysMonThread.HandleCount: Cardinal;
var
  pi: TPerformanceInformation;
begin
  Result:=0;
  if not Assigned(GetPerformanceInfo) or not GetPerformanceInfo(@pi,SizeOf(pi)) then
    Exit;
  Result:=pi.HandleCount;
end;

class function TSysMonThread.InstallDate: TDateTime;
const
  rvInstalldate = 'FirstInstallDateTime';
  rvInstalldateNT = 'InstallDate';
  SecsPerDay = 24*60*60;
var
  p: PAnsiChar;
  n: Cardinal;
  ft: TFiletime;
begin
  Result:=0;
  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey('\SOFTWARE\Microsoft\Windows NT\CurrentVersion',False) then begin
        if ValueExists(rvInstallDate) then begin
          try
            n:=ReadInteger(rvInstallDate);
          except
            n:=GetDataSize(rvInstallDate);
            p:=AllocMem(n);
            try
              ReadBinarydata(rvInstallDate,p^,n);
              Move(p[0],n,n);
            finally
              FreeMem(p);
            end;
          end;
          DosDateTimeToFileTime(HiWord(n),LoWord(n),ft);
          Result:=FileTimeTodateTime(ft);
        end;
        if ValueExists(rvInstallDateNT) then begin
          n:=ReadInteger(rvInstallDateNT);
          Result:=Int(Encodedate(1970,1,1));
          Result:=((Result*SecsPerDay)+n)/SecsPerDay;
        end;
      end;
    finally
      Free;
    end;
end;

class function TSysMonThread.InstalledMemory: Int64;
const
  i = 1073741824;
var
  e,m: Double;
  t: Int64;
begin
  e:=2;
  m:=i;
  t:=TotalPhysMemory;
  while Round(m)<t do begin
    m:=e*i;
    e:=e+1;
  end;
  Result:=Round(m);
end;

class function TSysMonThread.LastShutdown: TDateTime;
var
  rki: TRegKeyInfo;
begin
  Result:=0;
  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey('SYSTEM\CurrentControlSet\Control\Windows',False) then begin
        GetKeyInfo(rki);
        Result:=UTCToLocalDatetime({$IFNDEF FPC}FileTimeToDateTime{$ENDIF}(rki.FileTime));
      end;
    finally
      Free;
    end;
end;

class function TSysMonThread.LogicalCPUCount: Byte;
begin
  Result:=NativeGetCPUCount;
end;

class function TSysMonThread.MemoryLoad: Cardinal;
var
  MSEX: TMemoryStatusEx;
  MS: TMemoryStatus;
begin
  if Assigned(GlobalMemoryStatusEx_) then begin
    ResetMemory(MSEX,SizeOf(MSEX));
    MSEX.dwLength:=SizeOf(MSEX);
    GlobalMemoryStatusEx_(@MSEX);
    Result:=MSEX.dwMemoryLoad;
  end else begin
    ResetMemory(MS,SizeOf(MS));
    MS.dwLength:=SizeOf(MS);
    GlobalMemoryStatus(MS);
    Result:=MS.dwMemoryLoad;
  end;
end;

class function TSysMonThread.ProcessCount: Cardinal;
var
  pi: TPerformanceInformation;
begin
  Result:=0;
  if not Assigned(GetPerformanceInfo) or not GetPerformanceInfo(@pi,SizeOf(pi)) then
    Exit;
  Result:=pi.ProcessCount;
end;

procedure TSysMonThread.ReadCPUUsage;
var
  i: integer;
begin
  InternalLock.Enter;
  try
   for i:=0 to High(FValues) do
      FLastValues[i]:=FValues[i];

    NativeGetCPUData(FValues);
    FPeriod:=GetTickCount64-FPeriod;
    for i:=0 to High(FValues) do
      try
        FUsages[i]:=100-((FValues[i]-FLastValues[i])/FPeriod)/100;
        if FUsages[i]<0 then
          FUsages[i]:=0;
      except
        FUsages[i]:=0;
      end;
    FPeriod:=GetTickCount64;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSysMonThread.RemoveThreadText(AID: Cardinal);
var
  idx: Integer;
begin
  InternalLock.Enter;
  try
    idx:=FTL.IndexOfName(IntToStr(AID));
    if (idx>-1) then
      FTL.Delete(idx);
  finally
    InternalLock.Leave;
  end;
end;

procedure TSysMonThread.SaveThreadTexts(const AFilename: string;
  ANoLock: Boolean);
begin
  if Terminated then
    Exit;
  if not ANoLock then
    InternalLock.Enter;
  try
    try
      FTL.SaveToFile(AFilename)
    except
      sleep(100);
      try FTL.SaveToFile(AFilename) except end;
    end;
  finally
    if not ANoLock then
      InternalLock.Leave;
  end;
end;

class function TSysMonThread.SessionID: Cardinal;
begin
  ProcessIdToSessionId(GetCurrentProcessID,Result);
end;

procedure TSysMonThread.SetInterval(const Value: Cardinal);
begin
  InternalLock.Enter;
  try
    FInterval:=Value;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSysMonThread.SetOnInterval(const Value: TSysMonNotifyEvent);
begin
  InternalLock.Enter;
  try
    FOnInterval:=Value;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSysMonThread.SetThreadText(AID: Cardinal; const AText: string);
var
  idx: Integer;
begin
  InternalLock.Enter;
  try
    idx:=FTL.IndexOfName(IntToStr(AID));
    if (idx=-1) then
      FTL.Add(Format('%d=%s',[AID,AText]))
    else
      FTL[idx]:=Format('%d=%s',[AID,AText]);
  finally
    InternalLock.Leave;
  end;
end;

class function TSysMonThread.SystemCache: Int64;
var
  pi: TPerformanceInformation;
begin
  Result:=0;
  if not Assigned(GetPerformanceInfo) or not GetPerformanceInfo(@pi,SizeOf(pi)) then
    Exit;
  Result:=Int64(pi.SystemCache)*Int64(pi.PageSize);
end;

class function TSysMonThread.SystemDisk: string;
begin
  Result:=Copy(GetSysDir,1,2);
end;

class function TSysMonThread.SystemProductName: string;
var
  f,m: string;
begin
  m:='';
  f:='';
  Result:='';
  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey('HARDWARE\DESCRIPTION\System\BIOS',False) then begin
        if ValueExists('SystemProductName') then
          Result:=ReadString('SystemProductName');
        if ValueExists('SystemFamily') then
          f:=ReadString('SystemFamily');
        if ValueExists('SystemManufacturer') then
          m:=ReadString('SystemManufacturer');
        if (Pos(f,Result)=0) and (Pos('=',f)=0) then
          Result:=f+' '+Result;
        if Pos(m,Result)=0 then
          Result:=m+' '+Result;
        CloseKey;
      end;
    finally
      Free;
    end;
end;

class function TSysMonThread.ThreadCount: Cardinal;
var
  pi: TPerformanceInformation;
begin
  Result:=0;
  if not Assigned(GetPerformanceInfo) or not GetPerformanceInfo(@pi,SizeOf(pi)) then
    Exit;
  Result:=pi.ThreadCount;
end;

class function TSysMonThread.LimitCommitCharge: int64;
var
  MSEX: TMemoryStatusEx;
  MS: TMemoryStatus;
begin
  if Assigned(GlobalMemoryStatusEx_) then begin
    ResetMemory(MSEX,SizeOf(MSEX));
    MSEX.dwLength:=SizeOf(MSEX);
    GlobalMemoryStatusEx_(@MSEX);
    Result:=MSEX.ullTotalPageFile;
  end else begin
    ResetMemory(MS,SizeOf(MS));
    MS.dwLength:=SizeOf(MS);
    GlobalMemoryStatus(MS);
    Result:=MS.dwTotalPageFile;
  end;
end;

class function TSysMonThread.TotalPageFile: int64;
var
  wmiServices: ISWbemServices;
  wmi: TInstances;
begin
  if not WMIConnect('','','',Rootnamespace,wmiServices) then
    Exit;
  try
    WMICommand(wmiServices,'Win32_PageFileUsage',wmi);
    Result:=GetFileSize(GetInstancePropertyValue(wmi,'Name'));
  finally
    WMIDisconnect(wmiServices);
    Finalize(wmi);
  end;
end;

class function TSysMonThread.TotalPhysMemory: Int64;
var
  MSEX: TMemoryStatusEx;
  MS: TMemoryStatus;
begin
  if Assigned(GlobalMemoryStatusEx_) then begin
    ResetMemory(MSEX,SizeOf(MSEX));
    MSEX.dwLength:=SizeOf(MSEX);
    GlobalMemoryStatusEx_(@MSEX);
    Result:=MSEX.ullTotalPhys;
  end else begin
    ResetMemory(MS,SizeOf(MS));
    MS.dwLength:=SizeOf(MS);
    GlobalMemoryStatus(MS);
    Result:=MS.dwTotalPhys;
  end;
end;

class function TSysMonThread.TotalVirtualMemory: Int64;
var
  MSEX: TMemoryStatusEx;
  MS: TMemoryStatus;
begin
  if Assigned(GlobalMemoryStatusEx_) then begin
    ResetMemory(MSEX,SizeOf(MSEX));
    MSEX.dwLength:=SizeOf(MSEX);
    GlobalMemoryStatusEx_(@MSEX);
    Result:=MSEX.ullTotalVirtual;
  end else begin
    ResetMemory(MS,SizeOf(MS));
    MS.dwLength:=SizeOf(MS);
    GlobalMemoryStatus(MS);
    Result:=MS.dwTotalVirtual;
  end;
end;

initialization
  {$IFDEF TRIAL}
  if not RunFromIDE then
    MessageDlg('TSysMonThread'+sLineBreak+cCompName+sLineBreak+cCopyright,mtInformation,[mbOK],0);
  {$ENDIF}
  InitNativeAPI;
  InitPPAAPI;
finalization
end.
