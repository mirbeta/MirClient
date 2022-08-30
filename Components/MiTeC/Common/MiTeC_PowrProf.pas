{*******************************************************}
{               MiTeC Common Routines                   }
{             Power Policy Applicator API               }
{                                                       }
{         Copyright (c) 1997-2014 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_PowrProf;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils;
     {$ELSE}
     Windows, SysUtils;
     {$ENDIF}

const
  STATUS_BUFFER_TOO_SMALL = integer($C0000023);
  STATUS_ACCESS_DENIED = integer($C0000022);
  STATUS_INVALID_PARAMETER = integer($C000000D);

type
  _SYSTEM_POWER_STATE = (
    PowerSystemUnspecified,
    PowerSystemWorking,
    PowerSystemSleeping1,
    PowerSystemSleeping2,
    PowerSystemSleeping3,
    PowerSystemHibernate,
    PowerSystemShutdown,
    PowerSystemMaximum);
  SYSTEM_POWER_STATE = _SYSTEM_POWER_STATE;
  PSYSTEM_POWER_STATE = ^SYSTEM_POWER_STATE;
  TSystemPowerState = SYSTEM_POWER_STATE;
  PSystemPowerState = PSYSTEM_POWER_STATE;

  POWER_INFORMATION_LEVEL = (
    SystemPowerPolicyAc,
    SystemPowerPolicyDc,
    VerifySystemPolicyAc,
    VerifySystemPolicyDc,
    SystemPowerCapabilities,
    SystemBatteryState,
    SystemPowerStateHandler,
    ProcessorStateHandler,
    SystemPowerPolicyCurrent,
    AdministratorPowerPolicy,
    SystemReserveHiberFile,
    ProcessorInformation,
    SystemPowerInformation,
    ProcessorStateHandler2,
    LastWakeTime,                                   // Compare with KeQueryInterruptTime()
    LastSleepTime,                                  // Compare with KeQueryInterruptTime()
    SystemExecutionState,
    SystemPowerStateNotifyHandler,
    ProcessorPowerPolicyAc,
    ProcessorPowerPolicyDc,
    VerifyProcessorPowerPolicyAc,
    VerifyProcessorPowerPolicyDc,
    ProcessorPowerPolicyCurrent,
    SystemPowerStateLogging,
    SystemPowerLoggingEntry);
  TPowerInformationLevel = POWER_INFORMATION_LEVEL;

const
  POWER_SYSTEM_MAXIMUM = 7;

type
  POWER_ACTION = (
    PowerActionNone,
    PowerActionReserved,
    PowerActionSleep,
    PowerActionHibernate,
    PowerActionShutdown,
    PowerActionShutdownReset,
    PowerActionShutdownOff,
    PowerActionWarmEject);
  PPOWER_ACTION = ^POWER_ACTION;
  TPowerAction = POWER_ACTION;
  PPowerAction = PPOWER_ACTION;

  PPOWER_ACTION_POLICY = ^POWER_ACTION_POLICY;
  POWER_ACTION_POLICY = record
    Action: POWER_ACTION;
    Flags: DWORD;
    EventCode: DWORD;
  end;
  TPowerActionPolicy = POWER_ACTION_POLICY;
  PPowerActionPolicy = PPOWER_ACTION_POLICY;

// POWER_ACTION_POLICY->Flags:

const
  POWER_ACTION_QUERY_ALLOWED  = $00000001;
  POWER_ACTION_UI_ALLOWED     = $00000002;
  POWER_ACTION_OVERRIDE_APPS  = $00000004;
  POWER_ACTION_LIGHTEST_FIRST = $10000000;
  POWER_ACTION_LOCK_CONSOLE   = $20000000;
  POWER_ACTION_DISABLE_WAKES  = $40000000;
  POWER_ACTION_CRITICAL       = DWORD($80000000);

// POWER_ACTION_POLICY->EventCode flags

  POWER_LEVEL_USER_NOTIFY_TEXT  = $00000001;
  POWER_LEVEL_USER_NOTIFY_SOUND = $00000002;
  POWER_LEVEL_USER_NOTIFY_EXEC  = $00000004;
  POWER_USER_NOTIFY_BUTTON      = $00000008;
  POWER_USER_NOTIFY_SHUTDOWN    = $00000010;
  POWER_FORCE_TRIGGER_RESET     = DWORD($80000000);

  NUM_DISCHARGE_POLICIES    = 4;
  DISCHARGE_POLICY_CRITICAL = 0;
  DISCHARGE_POLICY_LOW      = 1;

// Throttling policies

  PO_THROTTLE_NONE     = 0;
  PO_THROTTLE_CONSTANT = 1;
  PO_THROTTLE_DEGRADE  = 2;
  PO_THROTTLE_ADAPTIVE = 3;
  PO_THROTTLE_MAXIMUM  = 4; // not a policy, just a limit

type
  PSYSTEM_POWER_LEVEL = ^SYSTEM_POWER_LEVEL;
  SYSTEM_POWER_LEVEL = record
    Enable: BOOLEAN;
    Spare: array [0..3 - 1] of BYTE;
    BatteryLevel: DWORD;
    PowerPolicy: POWER_ACTION_POLICY;
    MinSystemState: SYSTEM_POWER_STATE;
  end;
  TSystemPowerLevel = SYSTEM_POWER_LEVEL;
  PSystemPowerLevel = PSYSTEM_POWER_LEVEL;

  PGLOBAL_MACHINE_POWER_POLICY = ^GLOBAL_MACHINE_POWER_POLICY;
  _GLOBAL_MACHINE_POWER_POLICY = record
    Revision: ULONG;
    LidOpenWakeAc: SYSTEM_POWER_STATE;
    LidOpenWakeDc: SYSTEM_POWER_STATE;
    BroadcastCapacityResolution: ULONG;
  end;
  GLOBAL_MACHINE_POWER_POLICY = _GLOBAL_MACHINE_POWER_POLICY;
  TGlobalMachinePowerPolicy = GLOBAL_MACHINE_POWER_POLICY;
  PGlobalMachinePowerPolicy = PGLOBAL_MACHINE_POWER_POLICY;

  PGLOBAL_USER_POWER_POLICY = ^GLOBAL_USER_POWER_POLICY;
  _GLOBAL_USER_POWER_POLICY = record
    Revision: ULONG;
    PowerButtonAc: POWER_ACTION_POLICY;
    PowerButtonDc: POWER_ACTION_POLICY;
    SleepButtonAc: POWER_ACTION_POLICY;
    SleepButtonDc: POWER_ACTION_POLICY;
    LidCloseAc: POWER_ACTION_POLICY;
    LidCloseDc: POWER_ACTION_POLICY;
    DischargePolicy: array [0..NUM_DISCHARGE_POLICIES - 1] of SYSTEM_POWER_LEVEL;
    GlobalFlags: ULONG;
  end;
  GLOBAL_USER_POWER_POLICY = _GLOBAL_USER_POWER_POLICY;
  TGlobalUserPowerPolicy = GLOBAL_USER_POWER_POLICY;
  PGlobalUserPowerPolicy = PGLOBAL_USER_POWER_POLICY;

// Structure to manage global power policies at the user level. This structure
// contains data which is common across all power policy profiles.

  PGLOBAL_POWER_POLICY = ^GLOBAL_POWER_POLICY;
  _GLOBAL_POWER_POLICY = record
    user: GLOBAL_USER_POWER_POLICY;
    mach: GLOBAL_MACHINE_POWER_POLICY;
  end;
  GLOBAL_POWER_POLICY = _GLOBAL_POWER_POLICY;
  TGlobalPowerPolicy = GLOBAL_POWER_POLICY;
  PGlobalPowerPolicy = PGLOBAL_POWER_POLICY;

// Registry storage structures for the POWER_POLICY data. There are three
// structures, MACHINE_POWER_POLICY, MACHINE_PROCESSOR_POWER_POLICY and USER_POWER_POLICY. the
// MACHINE_POWER_POLICY stores per machine data for which there is no UI.
// USER_POWER_POLICY stores the per user data.

  PMACHINE_POWER_POLICY = ^MACHINE_POWER_POLICY;
  _MACHINE_POWER_POLICY = record
    Revision: ULONG; // 1
    // meaning of power action "sleep"
    MinSleepAc: SYSTEM_POWER_STATE;
    MinSleepDc: SYSTEM_POWER_STATE;
    ReducedLatencySleepAc: SYSTEM_POWER_STATE;
    ReducedLatencySleepDc: SYSTEM_POWER_STATE;
    // parameters for dozing
    DozeTimeoutAc: ULONG;
    DozeTimeoutDc: ULONG;
    DozeS4TimeoutAc: ULONG;
    DozeS4TimeoutDc: ULONG;
    // processor policies
    MinThrottleAc: UCHAR;
    MinThrottleDc: UCHAR;
    pad1: array [0..1] of UCHAR;
    OverThrottledAc: POWER_ACTION_POLICY;
    OverThrottledDc: POWER_ACTION_POLICY;
  end;
  MACHINE_POWER_POLICY = _MACHINE_POWER_POLICY;
  TMachinePowerPolicy = MACHINE_POWER_POLICY;
  PMachinePowerPolicy = PMACHINE_POWER_POLICY;

  // processor power policy state

  PPROCESSOR_POWER_POLICY_INFO = ^PROCESSOR_POWER_POLICY_INFO;
  _PROCESSOR_POWER_POLICY_INFO = record
    // Time based information (will be converted to kernel units)
    TimeCheck: DWORD; // in US
    DemoteLimit: DWORD; // in US
    PromoteLimit: DWORD; // in US
    // Percentage based information
    DemotePercent: BYTE;
    PromotePercent: BYTE;
    Spare: array [0..1] of BYTE;
    // Flags
    Flags: DWORD;
    //DWORD                   AllowDemotion:1;
    //DWORD                   AllowPromotion:1;
    //DWORD                   Reserved:30;
  end;
  PROCESSOR_POWER_POLICY_INFO = _PROCESSOR_POWER_POLICY_INFO;
  TProcessorPowerPolicyInfo = PROCESSOR_POWER_POLICY_INFO;
  PProcessorPowerPolicyInfo = PPROCESSOR_POWER_POLICY_INFO;

// processor power policy

  PPROCESSOR_POWER_POLICY = ^PROCESSOR_POWER_POLICY;
  _PROCESSOR_POWER_POLICY = record
    Revision: DWORD; // 1
    // Dynamic Throttling Policy
    DynamicThrottle: BYTE;
    Spare: array [0..2] of BYTE;
    // Flags
    Reserved: DWORD;
    //DWORD                       DisableCStates:1;
    //DWORD                       Reserved:31;

    // System policy information
    // The Array is last, in case it needs to be grown and the structure
    // revision incremented.
    PolicyCount: DWORD;
    Policy: array [0..2] of PROCESSOR_POWER_POLICY_INFO;
  end;
  PROCESSOR_POWER_POLICY = _PROCESSOR_POWER_POLICY;
  TProcessorPowerPolicy = PROCESSOR_POWER_POLICY;
  PProcessorPowerPolicy = PPROCESSOR_POWER_POLICY;

  PMACHINE_PROCESSOR_POWER_POLICY = ^MACHINE_PROCESSOR_POWER_POLICY;
  _MACHINE_PROCESSOR_POWER_POLICY = record
    Revision: ULONG; // 1
    ProcessorPolicyAc: PROCESSOR_POWER_POLICY;
    ProcessorPolicyDc: PROCESSOR_POWER_POLICY;
  end;
  MACHINE_PROCESSOR_POWER_POLICY = _MACHINE_PROCESSOR_POWER_POLICY;
  TMachineProcessorPowerPolicy = MACHINE_PROCESSOR_POWER_POLICY;
  PMachineProcessorPowerPolicy = PMACHINE_PROCESSOR_POWER_POLICY;

  PUSER_POWER_POLICY = ^USER_POWER_POLICY;
  _USER_POWER_POLICY = record
    Revision: ULONG; // 1
    // "system idle" detection
    IdleAc: POWER_ACTION_POLICY;
    IdleDc: POWER_ACTION_POLICY;
    IdleTimeoutAc: ULONG;
    IdleTimeoutDc: ULONG;
    IdleSensitivityAc: UCHAR;
    IdleSensitivityDc: UCHAR;
    // Throttling Policy
    ThrottlePolicyAc: UCHAR;
    ThrottlePolicyDc: UCHAR;
    // meaning of power action "sleep"
    MaxSleepAc: SYSTEM_POWER_STATE;
    MaxSleepDc: SYSTEM_POWER_STATE;
    // For future use
    Reserved: array [0..1] of ULONG;
    // video policies
    VideoTimeoutAc: ULONG;
    VideoTimeoutDc: ULONG;
    // hard disk policies
    SpindownTimeoutAc: ULONG;
    SpindownTimeoutDc: ULONG;
    // processor policies
    OptimizeForPowerAc: ByteBool;
    OptimizeForPowerDc: ByteBool;
    FanThrottleToleranceAc: UCHAR;
    FanThrottleToleranceDc: UCHAR;
    ForcedThrottleAc: UCHAR;
    ForcedThrottleDc: UCHAR;
  end;
  USER_POWER_POLICY = _USER_POWER_POLICY;
  TUserPowerPolicy = USER_POWER_POLICY;
  PUserPowerPolicy = PUSER_POWER_POLICY;

// Structure to manage power policies at the user level. This structure
// contains data which is unique across power policy profiles.

  PPOWER_POLICY = ^POWER_POLICY;
  _POWER_POLICY = record
    user: USER_POWER_POLICY;
    mach: MACHINE_POWER_POLICY;
  end;
  POWER_POLICY = _POWER_POLICY;
  TPowerPolicy = POWER_POLICY;
  PPowerPolicy = PPOWER_POLICY;

// Constants for GlobalFlags

const
  EnableSysTrayBatteryMeter = $01;
  EnableMultiBatteryDisplay = $02;
  EnablePasswordLogon       = $04;
  EnableWakeOnRing          = $08;
  EnableVideoDimDisplay     = $10;


// This constant is passed as a uiID to WritePwrScheme.

  NEWSCHEME = UINT(-1);

type

// System power manager capabilities

  BATTERY_REPORTING_SCALE = record
    Granularity: DWORD;
    Capacity: DWORD;
  end;
  PBATTERY_REPORTING_SCALE = ^BATTERY_REPORTING_SCALE;
  TBatteryReportingScale = BATTERY_REPORTING_SCALE;
  PBatteryReportingScale = PBATTERY_REPORTING_SCALE;

// administrator power policy overrides

  PSYSTEM_POWER_CAPABILITIES = ^SYSTEM_POWER_CAPABILITIES;
  SYSTEM_POWER_CAPABILITIES = record
    // Misc supported system features
    PowerButtonPresent: BOOLEAN;
    SleepButtonPresent: BOOLEAN;
    LidPresent: BOOLEAN;
    SystemS1: BOOLEAN;
    SystemS2: BOOLEAN;
    SystemS3: BOOLEAN;
    SystemS4: BOOLEAN; // hibernate
    SystemS5: BOOLEAN; // off
    HiberFilePresent: BOOLEAN;
    FullWake: BOOLEAN;
    VideoDimPresent: BOOLEAN;
    ApmPresent: BOOLEAN;
    UpsPresent: BOOLEAN;
    // Processors
    ThermalControl: BOOLEAN;
    ProcessorThrottle: BOOLEAN;
    ProcessorMinThrottle: BYTE;
    ProcessorMaxThrottle: BYTE;
    spare2: array [0..4 - 1] of BYTE;
    // Disk
    DiskSpinDown: BOOLEAN;
    spare3: array [0..8 - 1] of BYTE;
    // System Battery
    SystemBatteriesPresent: BOOLEAN;
    BatteriesAreShortTerm: BOOLEAN;
    BatteryScale: array [0..3 - 1] of BATTERY_REPORTING_SCALE;
    // Wake
    AcOnLineWake: SYSTEM_POWER_STATE;
    SoftLidWake: SYSTEM_POWER_STATE;
    RtcWake: SYSTEM_POWER_STATE;
    MinDeviceWakeState: SYSTEM_POWER_STATE; // note this may change on driver load
    DefaultLowLatencyWake: SYSTEM_POWER_STATE;
  end;
  TSystemPowerCapabilities = SYSTEM_POWER_CAPABILITIES;
  PSystemPowerCapabilities = PSYSTEM_POWER_CAPABILITIES;

  PADMINISTRATOR_POWER_POLICY = ^ADMINISTRATOR_POWER_POLICY;
  _ADMINISTRATOR_POWER_POLICY = record
    // meaning of power action "sleep"
    MinSleep: SYSTEM_POWER_STATE;
    MaxSleep: SYSTEM_POWER_STATE;
    // video policies
    MinVideoTimeout: DWORD;
    MaxVideoTimeout: DWORD;
    // disk policies
    MinSpindownTimeout: DWORD;
    MaxSpindownTimeout: DWORD;
  end;
  ADMINISTRATOR_POWER_POLICY = _ADMINISTRATOR_POWER_POLICY;
  TAdministratorPowerPolicy = ADMINISTRATOR_POWER_POLICY;
  PAdministratorPowerPolicy = PADMINISTRATOR_POWER_POLICY;

  PPROCESSOR_POWER_INFORMATION = ^PROCESSOR_POWER_INFORMATION;
  _PROCESSOR_POWER_INFORMATION = record
    Number,
    MaxMhz,
    CurrentMhz,
    MhzLimit,
    MaxIdleState,
    CurrentIdleState: ULONG;
  end;
  PROCESSOR_POWER_INFORMATION = _PROCESSOR_POWER_INFORMATION;
  TProcessorPowerInformation = PROCESSOR_POWER_INFORMATION;
  PProcessorPowerInformation = PPROCESSOR_POWER_INFORMATION;

  PSYSTEM_BATTERY_STATE = ^SYSTEM_BATTERY_STATE;
  SYSTEM_BATTERY_STATE = record
    AcOnLine: BOOLEAN;
    BatteryPresent: BOOLEAN;
    Charging: BOOLEAN;
    Discharging: BOOLEAN;
    Spare1: array [0..3] of BOOLEAN;
    MaxCapacity: DWORD;
    RemainingCapacity: DWORD;
    Rate: DWORD;
    EstimatedTime: DWORD;
    DefaultAlert1: DWORD;
    DefaultAlert2: DWORD;
  end;
  TSystemBatteryState = SYSTEM_BATTERY_STATE;
  PSystemBatteryState = PSYSTEM_BATTERY_STATE;

  // Prototype for EnumPwrSchemes callback proceedures.

const
  BATTERY_POWER_ON_LINE = $00000001;
  BATTERY_DISCHARGING   = $00000002;
  BATTERY_CHARGING      = $00000004;
  BATTERY_CRITICAL      = $00000008;

// BATTERY_STATUS Constant
// BATTERY_UNKNOWN_CAPACITY defined above for IOCTL_BATTERY_QUERY_INFORMATION


  BATTERY_UNKNOWN_VOLTAGE = DWORD($FFFFFFFF);
  BATTERY_UNKNOWN_RATE    = DWORD($80000000);
  BATTERY_UNKNOWN_TIME    = DWORD($FFFFFFFF);

type
  PWRSCHEMESENUMPROC = function(uiIndex: UINT; dwName: DWORD; sName: LPWSTR; dwDesc: DWORD; sDesc: LPWSTR; pp: PPOWER_POLICY; lParam: LPARAM): ByteBool; stdcall;
  PFNNTINITIATEPWRACTION = function(pPowerAction: POWER_ACTION; SystemPowerState: SYSTEM_POWER_STATE; u: ULONG; b: ByteBool): ByteBool; stdcall;

  TGetPwrDiskSpindownRange = function (var RangeMax, RangeMin: UINT): ByteBool; stdcall;
  TEnumPwrSchemes = function (lpfnPwrSchemesEnumProc: PWRSCHEMESENUMPROC; lParam: LPARAM): ByteBool; stdcall;
  TReadGlobalPwrPolicy = function (var pGlobalPowerPolicy: GLOBAL_POWER_POLICY): ByteBool; stdcall;
  TReadPwrScheme = function (uiID: UINT; var pPowerPolicy: POWER_POLICY): ByteBool; stdcall;
  TWritePwrScheme = function (puiID: PUINT; lpszName, lpszDescription: LPWSTR; const pPowerPolicy: POWER_POLICY): ByteBool; stdcall;
  TWriteGlobalPwrPolicy = function (const pGlobalPowerPolicy: GLOBAL_POWER_POLICY): ByteBool; stdcall;
  TDeletePwrScheme = function (uiIndex: UINT): ByteBool; stdcall;
  TGetActivePwrScheme = function (var puiID: UINT): ByteBool; stdcall;
  TSetActivePwrScheme = function (uiID: UINT; pGlobalPowerPolicy: PGLOBAL_POWER_POLICY; pPowerPolicy: PPOWER_POLICY): ByteBool; stdcall;
  TGetPwrCapabilities = function (var lpSystemPowerCapabilities: SYSTEM_POWER_CAPABILITIES): ByteBool; stdcall;
  TIsPwrSuspendAllowed = function : ByteBool; stdcall;
  TIsPwrHibernateAllowed = function : ByteBool; stdcall;
  TIsPwrShutdownAllowed = function : ByteBool; stdcall;
  TIsAdminOverrideActive = function (pAdministratorPowerPolicy: PADMINISTRATOR_POWER_POLICY): ByteBool; stdcall;
  TSetSuspendState = function (Hibernate, ForceCritical, DisableWakeEvent: ByteBool): ByteBool; stdcall;
  TGetCurrentPowerPolicies = function (pGlobalPowerPolicy: PGLOBAL_POWER_POLICY; pPowerPolicy: PPOWER_POLICY): ByteBool; stdcall;
  TCanUserWritePwrScheme = function : ByteBool; stdcall;
  TReadProcessorPwrScheme = function (uiID: UINT; var pMachineProcessorPowerPolicy: MACHINE_PROCESSOR_POWER_POLICY): ByteBool; stdcall;
  TWriteProcessorPwrScheme = function (uiID: UINT; const pMachineProcessorPowerPolicy: MACHINE_PROCESSOR_POWER_POLICY): ByteBool; stdcall;
  TValidatePowerPolicies = function (GlobalPolicy: PGLOBAL_POWER_POLICY; Policy: PPOWER_POLICY): ByteBool; stdcall;
  TCallNtPowerInformation = function (InformationLeveL: POWER_INFORMATION_LEVEL; lpInputBuffer: Pointer; nInputBufferSize: ULONG; lpOutputBuffer: Pointer; nOutputBufferSize: ULONG): integer; stdcall;

var
  PPAHandle: THandle = 0;
  UnloadPPA: Boolean;

  GetPwrDiskSpindownRange: TGetPwrDiskSpindownRange = nil;
  EnumPwrSchemes: TEnumPwrSchemes = nil;
  ReadGlobalPwrPolicy: TReadGlobalPwrPolicy = nil;
  ReadPwrScheme: TReadPwrScheme = nil;
  WritePwrScheme: TWritePwrScheme = nil;
  DeletePwrScheme: TDeletePwrScheme = nil;
  GetActivePwrScheme: TGetActivePwrScheme = nil;
  SetActivePwrScheme: TSetActivePwrScheme = nil;
  GetPwrCapabilities: TGetPwrCapabilities = nil;
  IsPwrSuspendAllowed: TIsPwrSuspendAllowed = nil;
  IsPwrHibernateAllowed: TIsPwrHibernateAllowed = nil;
  IsPwrShutdownAllowed: TIsPwrShutdownAllowed = nil;
  IsAdminOverrideActive: TIsAdminOverrideActive = nil;
  SetSuspendState: TSetSuspendState = nil;
  GetCurrentPowerPolicies: TGetCurrentPowerPolicies = nil;
  CanUserWritePwrScheme: TCanUserWritePwrScheme = nil;
  ReadProcessorPwrScheme: TReadProcessorPwrScheme = nil;
  WriteProcessorPwrScheme: TWriteProcessorPwrScheme = nil;
  ValidatePowerPolicies: TValidatePowerPolicies = nil;
  CallNtPowerInformation: TCallNtPowerInformation = nil;

function InitPPAAPI: boolean;
procedure FreePPAAPI;

const
  PPA_DLL = 'powrprof.dll';

implementation

function InitPPAAPI;
begin
  PPAHandle:=GetModuleHandle(PPA_DLL);
  UnloadPPA:=PPAHandle=0;
  if PPAHandle=0 then
    PPAHandle:=LoadLibrary(PPA_DLL);
  if (PPAHandle<>0) and not Assigned(IsPwrShutdownAllowed) then begin
    @GetPwrDiskSpindownRange:=GetProcAddress(PPAHandle,'GetPwrDiskSpindownRange');
    @EnumPwrSchemes:=GetProcAddress(PPAHandle,'EnumPwrSchemes');
    @ReadGlobalPwrPolicy:=GetProcAddress(PPAHandle,'ReadGlobalPwrPolicy');
    @ReadPwrScheme:=GetProcAddress(PPAHandle,'ReadPwrScheme');
    @WritePwrScheme:=GetProcAddress(PPAHandle,'WritePwrScheme');
    @DeletePwrScheme:=GetProcAddress(PPAHandle,'DeletePwrScheme');
    @GetActivePwrScheme:=GetProcAddress(PPAHandle,'GetActivePwrScheme');
    @ReadProcessorPwrScheme:=GetProcAddress(PPAHandle,'ReadProcessorPwrScheme');
    @SetActivePwrScheme:=GetProcAddress(PPAHandle,'SetActivePwrScheme');
    @GetPwrCapabilities:=GetProcAddress(PPAHandle,'GetPwrCapabilities');
    @IsPwrHibernateAllowed:=GetProcAddress(PPAHandle,'IsPwrHibernateAllowed');
    @IsPwrSuspendAllowed:=GetProcAddress(PPAHandle,'IsPwrSuspendAllowed');
    @IsPwrShutdownAllowed:=GetProcAddress(PPAHandle,'IsPwrShutdownAllowed');
    @IsAdminOverrideActive:=GetProcAddress(PPAHandle,'IsAdminOverrideActive');
    @SetSuspendState:=GetProcAddress(PPAHandle,'SetSuspendState');
    @GetCurrentPowerPolicies:=GetProcAddress(PPAHandle,'GetCurrentPowerPolicies');
    @CanUserWritePwrScheme:=GetProcAddress(PPAHandle,'CanUserWritePwrScheme');
    @WriteProcessorPwrScheme:=GetProcAddress(PPAHandle,'WriteProcessorPwrScheme');
    @ValidatePowerPolicies:=GetProcAddress(PPAHandle,'ValidatePowerPolicies');
    @CallNtPowerInformation:=GetProcAddress(PPAHandle,'CallNtPowerInformation');
  end;
  Result:=(PPAHandle<>0) and Assigned(IsPwrShutdownAllowed);
end;

procedure FreePPAAPI;
begin
  if (PPAHandle<>0) and UnloadPPA then begin
    if not FreeLibrary(PPAHandle) then
      raise Exception.Create(Format('Unload Error: %s - 0x%x',[PPA_DLL,GetModuleHandle(PPA_DLL)]))
    else
      PPAHandle:=0;
  end;
end;


initialization
finalization
  FreePPAAPI;
end.


