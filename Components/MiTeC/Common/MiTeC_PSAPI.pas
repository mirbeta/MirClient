{*******************************************************}
{               MiTeC Common Routines                   }
{            Windows Process Status Helper              }
{                                                       }
{                                                       }
{         Copyright (c) 1997-2019 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_PSAPI;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.ShellAPI,
     {$ELSE}
     Windows, SysUtils, Classes, ShellAPI,
     {$ENDIF}
     MiTeC_Windows;

const
  LIST_MODULES_DEFAULT = 0;
  LIST_MODULES_32BIT   = 1;
  LIST_MODULES_64BIT   = 2;
  LIST_MODULES_ALL     = 3;

type
  PHInst = ^HInst;
  TModuleInfo = record
    lpBaseOfDll: pointer;
    SizeOfImage: Integer;
    EntryPoint: pointer;
  end;

  TPSAPIWsWatchInformation = record
    FaultingPc: pointer;
    FaultingVa: pointer;
  end;

  TProcessMemoryCounters = record
    cb: Cardinal;
    PageFaultCount: Cardinal;
    PeakWorkingSetSize: SIZE_T;
    WorkingSetSize: SIZE_T;
    QuotaPeakPagedPoolUsage: SIZE_T;
    QuotaPagedPoolUsage: SIZE_T;
    QuotaPeakNonPagedPoolUsage: SIZE_T;
    QuotaNonPagedPoolUsage: SIZE_T;
    PagefileUsage: SIZE_T;
    PeakPagefileUsage: SIZE_T;
  end;
  PProcessMemoryCounters = ^TProcessMemoryCounters;

  TProcessMemoryCountersEx = record
    cb: Cardinal;
    PageFaultCount: Cardinal;
    PeakWorkingSetSize: SIZE_T;
    WorkingSetSize: SIZE_T;
    QuotaPeakPagedPoolUsage: SIZE_T;
    QuotaPagedPoolUsage: SIZE_T;
    QuotaPeakNonPagedPoolUsage: SIZE_T;
    QuotaNonPagedPoolUsage: SIZE_T;
    PagefileUsage: SIZE_T;
    PeakPagefileUsage: SIZE_T;
    PrivateUsage: SIZE_T;
  end;
  PProcessMemoryCountersEx = ^TProcessMemoryCountersEx;

  _PERFORMANCE_INFORMATION = record
    cb: DWORD;
    CommitTotal: SIZE_T;
    CommitLimit: SIZE_T;
    CommitPeak: SIZE_T;
    PhysicalTotal: SIZE_T;
    PhysicalAvailable: SIZE_T;
    SystemCache: SIZE_T;
    KernelTotal: SIZE_T;
    KernelPaged: SIZE_T;
    KernelNonpaged: SIZE_T;
    PageSize: SIZE_T;
    HandleCount: DWORD;
    ProcessCount: DWORD;
    ThreadCount: DWORD;
  end;
  PERFORMANCE_INFORMATION = _PERFORMANCE_INFORMATION;
  PPERFORMANCE_INFORMATION = ^PERFORMANCE_INFORMATION;
  TPerformanceInformation = PERFORMANCE_INFORMATION;
  PPerformanceInformation = PPERFORMANCE_INFORMATION;

  PSAPI_WORKING_SET_BLOCK = record
    Flags: ULONG_PTR;
  end;

  PSAPI_WORKING_SET_INFORMATION = record
    NumberOfEntries: ULONG_PTR;
    WorkingSetInfo: array[0..0] of PSAPI_WORKING_SET_BLOCK;
  end;

  PSAPI_WORKING_SET_EX_BLOCK = record
    Flags: ULONG_PTR;
  end;

  PSAPI_WORKING_SET_EX_INFORMATION = record
    VirtualAddress: PVOID;
    VirtualAttributes: PSAPI_WORKING_SET_EX_BLOCK;
  end;

  TaskEnumProcEx = function(threadID: Cardinal; hMod16: WORD; hTask16: WORD; modName: PChar; fileName: PChar; param: Cardinal): BOOL; stdcall;

  function InitPSAPI: Boolean;
  procedure FreePSAPI;

  function InitVDM: Boolean;
  procedure FreeVDM;

type
  TVDMEnumTaskWOWEx = function (pid: Cardinal; callback: TaskEnumProcEx; param: Cardinal): Integer; stdcall;

  TEnumProcesses = function (pidList: PInteger; cb: Integer; var cbNeeded: Cardinal): boolean; stdcall;
  TEnumProcessModules = function (hProcess: THandle; moduleList: PHInst; cb: Integer; var cbNeeded: Cardinal): boolean; stdcall;
  TEnumProcessModulesEx = function (hProcess: THandle; moduleList: PHInst; cb: Integer; var cbNeeded: Cardinal; dwFilterFlag: Cardinal): boolean; stdcall;
  TGetModuleBaseName = function (hProcess: THandle; module: HInst; BaseName: PChar; size: Integer): Integer; stdcall;
  TGetModuleFileNameEx = function (hProcess: THandle; module: HInst; FileName: PChar; size: Integer): Integer; stdcall;
  TGetModuleInformation = function (hProcess: THandle; module: HInst; var info: TModuleInfo; size: Integer): boolean; stdcall;
  TEmptyWorkingSet = function (hProcess: THandle): boolean; stdcall;
  TQueryWorkingSet = function (hProcess: THandle; var pv; size: Integer): boolean; stdcall;
  TQueryWorkingSetEx = function (hProcess: THandle; var pv; size: Integer): boolean; stdcall;
  TInitializeProcessForWsWatch = function (hProcess: THandle): boolean; stdcall;
  TGetWsChanges = function (hProcess: THandle; var WatchInfo: TPSAPIWsWatchInformation; size: Integer): boolean; stdcall;
  TGetMappedFileName = function (hProcess: THandle; pv: pointer; FileName: PChar; size: Integer): Integer; stdcall;
  TEnumDeviceDrivers = function (ImageBase: PInteger; cb: Cardinal; var cbNeeded: Cardinal): boolean; stdcall;
  TGetDeviceDriverBaseName = function (ImageBase: Integer; BaseName: PChar; size: Cardinal): Integer; stdcall;
  TGetDeviceDriverFileName = function (ImageBase: Integer; FileName: PChar; size: Cardinal): Integer; stdcall;
  TGetProcessMemoryInfo = function (hProcess: THandle; ProcessMemoryCounters: PProcessMemoryCounters; size: Integer): boolean; stdcall;
  TGetPerformanceInfo = function (pPerformanceInformation: PPERFORMANCE_INFORMATION; cb: DWORD): BOOL; stdcall;

var
  modulelist :PHInst;
  PSAPILoaded :Boolean;

  VDMEnumTaskWOWEx :TVDMEnumTaskWOWEx = nil;

  EnumProcesses: TEnumProcesses = nil;
  EnumProcessModules: TEnumProcessModules = nil;
  EnumProcessModulesEx: TEnumProcessModulesEx = nil;
  GetModuleBaseName: TGetModuleBaseName = nil;
  GetModuleFileNameEx: TGetModuleFileNameEx = nil;
  GetModuleInformation: TGetModuleInformation = nil;
  EmptyWorkingSet: TEmptyWorkingSet = nil;
  QueryWorkingSet: TQueryWorkingSet = nil;
  QueryWorkingSetEx: TQueryWorkingSetEx = nil;
  InitializeProcessForWsWatch: TInitializeProcessForWsWatch = nil;
  GetWsChanges: TGetWsChanges = nil;
  GetMappedFileName: TGetMappedFileName = nil;
  EnumDeviceDrivers: TEnumDeviceDrivers = nil;
  GetDeviceDriverBaseName: TGetDeviceDriverBaseName = nil;
  GetDeviceDriverFileName: TGetDeviceDriverFileName = nil;
  GetProcessMemoryInfo: TGetProcessMemoryInfo = nil;
  GetPerformanceInfo: TGetPerformanceInfo = nil;


implementation

const
  PSAPI_DLL = 'psapi.dll';
  VDM_DLL = 'vdmdbg.dll';

var
  PSAPIHandle, VDMHandle: THandle;
  UnloadPSAPI, UnloadVDM: Boolean;

function InitPSAPI: Boolean;
var
  KernelHandle: THandle;
begin
  KernelHandle:=GetModuleHandle('kernel32.dll');
  PSAPIHandle:=GetModuleHandle(PSAPI_DLL);
  UnloadPSAPI:=PSAPIHandle=0;
  if PSAPIHandle = 0 then
    PSAPIHandle:=LoadLibrary(psapi_dll);
  if PSAPIHandle<>0 then begin
    @EnumProcesses:=GetProcAddress(PSAPIHandle,PChar('EnumProcesses'));
    @EnumProcessModules:=GetProcAddress(PSAPIHandle,PChar('EnumProcessModules'));
    @EnumProcessModulesEx:=GetProcAddress(PSAPIHandle,PChar('EnumProcessModulesEx'));
    if not Assigned(EnumProcessModulesEx) then
      @EnumProcessModulesEx:=GetProcAddress(KernelHandle,PChar('EnumProcessModulesEx'));
    {$IFDEF UNICODE}
    @GetModuleBaseName:=GetProcAddress(PSAPIHandle,PChar('GetModuleBaseNameW'));
    @GetModuleFileNameEx:=GetProcAddress(PSAPIHandle,PChar('GetModuleFileNameExW'));
    @GetMappedFileName:=GetProcAddress(PSAPIHandle,PChar('GetMappedFileNameW'));
    @GetDeviceDriverBaseName:=GetProcAddress(PSAPIHandle,PChar('GetDeviceDriverBaseNameW'));
    @GetDeviceDriverFileName:=GetProcAddress(PSAPIHandle,PChar('GetDeviceDriverFileNameW'));
    {$ELSE}
    @GetModuleBaseName:=GetProcAddress(PSAPIHandle,PChar('GetModuleBaseNameA'));
    @GetModuleFileNameEx:=GetProcAddress(PSAPIHandle,PChar('GetModuleFileNameExA'));
    @GetMappedFileName:=GetProcAddress(PSAPIHandle,PChar('GetMappedFileNameA'));
    @GetDeviceDriverBaseName:=GetProcAddress(PSAPIHandle,PChar('GetDeviceDriverBaseNameA'));
    @GetDeviceDriverFileName:=GetProcAddress(PSAPIHandle,PChar('GetDeviceDriverFileNameA'));
    {$ENDIF}
    @GetModuleInformation:=GetProcAddress(PSAPIHandle,PChar('GetModuleInformation'));
    @EmptyWorkingSet:=GetProcAddress(PSAPIHandle,PChar('EmptyWorkingSet'));
    @QueryWorkingSet:=GetProcAddress(PSAPIHandle,PChar('QueryWorkingSet'));
    @QueryWorkingSetEx:=GetProcAddress(PSAPIHandle,PChar('QueryWorkingSetEx'));
    @InitializeProcessForWsWatch:=GetProcAddress(PSAPIHandle,PChar('InitializeProcessForWsWatch'));
    @GetWsChanges:=GetProcAddress(PSAPIHandle,PChar('GetWsChanges'));
    @EnumDeviceDrivers:=GetProcAddress(PSAPIHandle,PChar('EnumDeviceDrivers'));
    @GetProcessMemoryInfo:=GetProcAddress(PSAPIHandle,PChar('GetProcessMemoryInfo'));
    @GetPerformanceInfo:=GetProcAddress(PSAPIHandle,PChar('GetPerformanceInfo'));
    if not Assigned(GetPerformanceInfo) then
      @GetPerformanceInfo:=GetProcAddress(PSAPIHandle,PChar('K32GetPerformanceInfo'));
  end;
  result:=(PSAPIHandle<>0) and Assigned(EnumProcesses);
end;

procedure FreePSAPI;
begin
  if (PSAPIHandle<>0) and UnloadPSAPI then begin
    if not FreeLibrary(PSAPIHandle) then
      raise Exception.Create(Format('Unload Error: %s - 0x%x',[PSAPI_DLL,GetModuleHandle(PSAPI_DLL)]))
    else
      PSAPIHandle:=0;
  end;
end;

function InitVDM: Boolean;
begin
  VDMHandle:=GetModuleHandle(VDM_DLL);
  UnloadVDM:=VDMHandle=0;
  if VDMHandle = 0 then
    VDMHandle:=loadlibrary(VDM_DLL);
  if VDMHandle<>0 then begin
    @VDMEnumTaskWOWEx:=GetProcAddress(VDMHandle,PChar('VDMEnumTaskWOWEx'));
  end;
  result:=(VDMHandle<>0) and Assigned(VDMEnumTaskWOWEx);
end;

procedure FreeVDM;
begin
  if (VDMHandle<>0) and UnloadVDM then begin
    if not FreeLibrary(VDMHandle) then
      raise Exception.Create(Format('Unload Error: %s - 0x%x',[VDM_DLL,GetModuleHandle(VDM_DLL)]))
    else
      VDMHandle:=0;
  end;
end;

initialization
  InitPSAPI;
finalization
  FreePSAPI;
  FreeVDM;
end.
