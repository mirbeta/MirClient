{*******************************************************}
{               MiTeC Common Routines                   }
{             NT Native API Definitions                 }
{                                                       }
{                                                       }
{       Copyright (c) 1997-2019 Michal Mutl             }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_NativeDefs;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, WinAPI.WinSock,
     {$ELSE}
     Windows, WinSock,
     {$ENDIF}
     MiTeC_Windows;

{$INCLUDE MiTeC_NTStatus.inc}

type
  UNICODE_STRING = record
    Length,
    MaximumLength: WORD;
    Buffer: LPWSTR;
  end;
  TUnicodeString = UNICODE_STRING;
  PUNICODE_STRING = ^UNICODE_STRING;

  {$IFDEF WIN64}{$Z4}{$ENDIF}
  SYSTEM_INFORMATION_CLASS = (
    SystemBasicInformation, // q: SYSTEM_BASIC_INFORMATION
    SystemProcessorInformation, // q: SYSTEM_PROCESSOR_INFORMATION
    SystemPerformanceInformation, // q: SYSTEM_PERFORMANCE_INFORMATION
    SystemTimeOfDayInformation, // q: SYSTEM_TIMEOFDAY_INFORMATION
    SystemPathInformation, // not implemented
    SystemProcessInformation, // q: SYSTEM_PROCESS_INFORMATION
    SystemCallCountInformation, // q: SYSTEM_CALL_COUNT_INFORMATION
    SystemDeviceInformation, // q: SYSTEM_DEVICE_INFORMATION
    SystemProcessorPerformanceInformation, // q: SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION
    SystemFlagsInformation, // q: SYSTEM_FLAGS_INFORMATION
    SystemCallTimeInformation, // not implemented // 10
    SystemModuleInformation, // q: RTL_PROCESS_MODULES
    SystemLocksInformation,
    SystemStackTraceInformation,
    SystemPagedPoolInformation, // not implemented
    SystemNonPagedPoolInformation, // not implemented
    SystemHandleInformation, // q: SYSTEM_HANDLE_INFORMATION
    SystemObjectInformation, // q: SYSTEM_OBJECTTYPE_INFORMATION mixed with SYSTEM_OBJECT_INFORMATION
    SystemPageFileInformation, // q: SYSTEM_PAGEFILE_INFORMATION
    SystemVdmInstemulInformation, // q
    SystemVdmBopInformation, // not implemented // 20
    SystemFileCacheInformation, // q: SYSTEM_FILECACHE_INFORMATION; s (requires SeIncreaseQuotaPrivilege) (info for WorkingSetTypeSystemCache)
    SystemPoolTagInformation, // q: SYSTEM_POOLTAG_INFORMATION
    SystemInterruptInformation, // q: SYSTEM_INTERRUPT_INFORMATION
    SystemDpcBehaviorInformation, // q: SYSTEM_DPC_BEHAVIOR_INFORMATION; s: SYSTEM_DPC_BEHAVIOR_INFORMATION (requires SeLoadDriverPrivilege)
    SystemFullMemoryInformation, // not implemented
    SystemLoadGdiDriverInformation, // s (kernel-mode only)
    SystemUnloadGdiDriverInformation, // s (kernel-mode only)
    SystemTimeAdjustmentInformation, // q: SYSTEM_QUERY_TIME_ADJUST_INFORMATION; s: SYSTEM_SET_TIME_ADJUST_INFORMATION (requires SeSystemtimePrivilege)
    SystemSummaryMemoryInformation, // not implemented
    SystemMirrorMemoryInformation, // s (requires license value "Kernel-MemoryMirroringSupported") (requires SeShutdownPrivilege) // 30
    SystemPerformanceTraceInformation, // s
    SystemObsolete0, // not implemented
    SystemExceptionInformation, // q: SYSTEM_EXCEPTION_INFORMATION
    SystemCrashDumpStateInformation, // s (requires SeDebugPrivilege)
    SystemKernelDebuggerInformation, // q: SYSTEM_KERNEL_DEBUGGER_INFORMATION
    SystemContextSwitchInformation, // q: SYSTEM_CONTEXT_SWITCH_INFORMATION
    SystemRegistryQuotaInformation, // q: SYSTEM_REGISTRY_QUOTA_INFORMATION; s (requires SeIncreaseQuotaPrivilege)
    SystemExtendServiceTableInformation, // s (requires SeLoadDriverPrivilege) // loads win32k only
    SystemPrioritySeperation, // s (requires SeTcbPrivilege)
    SystemVerifierAddDriverInformation, // s (requires SeDebugPrivilege) // 40
    SystemVerifierRemoveDriverInformation, // s (requires SeDebugPrivilege)
    SystemProcessorIdleInformation, // q: SYSTEM_PROCESSOR_IDLE_INFORMATION
    SystemLegacyDriverInformation, // q: SYSTEM_LEGACY_DRIVER_INFORMATION
    SystemCurrentTimeZoneInformation, // q
    SystemLookasideInformation, // q: SYSTEM_LOOKASIDE_INFORMATION
    SystemTimeSlipNotification, // s (requires SeSystemtimePrivilege)
    SystemSessionCreate, // not implemented
    SystemSessionDetach, // not implemented
    SystemSessionInformation, // not implemented
    SystemRangeStartInformation, // q // 50
    SystemVerifierInformation, // q: SYSTEM_VERIFIER_INFORMATION; s (requires SeDebugPrivilege)
    SystemVerifierThunkExtend, // s (kernel-mode only)
    SystemSessionProcessInformation, // q: SYSTEM_SESSION_PROCESS_INFORMATION
    SystemLoadGdiDriverInSystemSpace, // s (kernel-mode only) (same as SystemLoadGdiDriverInformation)
    SystemNumaProcessorMap, // q
    SystemPrefetcherInformation, // q: PREFETCHER_INFORMATION; s: PREFETCHER_INFORMATION // PfSnQueryPrefetcherInformation
    SystemExtendedProcessInformation, // q: SYSTEM_PROCESS_INFORMATION
    SystemRecommendedSharedDataAlignment, // q
    SystemComPlusPackage, // q; s
    SystemNumaAvailableMemory, // 60
    SystemProcessorPowerInformation, // q: SYSTEM_PROCESSOR_POWER_INFORMATION
    SystemEmulationBasicInformation, // q
    SystemEmulationProcessorInformation,
    SystemExtendedHandleInformation, // q: SYSTEM_HANDLE_INFORMATION_EX
    SystemLostDelayedWriteInformation, // q: ULONG
    SystemBigPoolInformation, // q: SYSTEM_BIGPOOL_INFORMATION
    SystemSessionPoolTagInformation, // q: SYSTEM_SESSION_POOLTAG_INFORMATION
    SystemSessionMappedViewInformation, // q: SYSTEM_SESSION_MAPPED_VIEW_INFORMATION
    SystemHotpatchInformation, // q; s
    SystemObjectSecurityMode, // q // 70
    SystemWatchdogTimerHandler, // s (kernel-mode only)
    SystemWatchdogTimerInformation, // q (kernel-mode only); s (kernel-mode only)
    SystemLogicalProcessorInformation, // q: SYSTEM_LOGICAL_PROCESSOR_INFORMATION
    SystemWow64SharedInformationObsolete, // not implemented
    SystemRegisterFirmwareTableInformationHandler, // s (kernel-mode only)
    SystemFirmwareTableInformation, // not implemented
    SystemModuleInformationEx, // q: RTL_PROCESS_MODULE_INFORMATION_EX
    SystemVerifierTriageInformation, // not implemented
    SystemSuperfetchInformation, // q: SUPERFETCH_INFORMATION; s: SUPERFETCH_INFORMATION // PfQuerySuperfetchInformation
    SystemMemoryListInformation, // q: SYSTEM_MEMORY_LIST_INFORMATION; s: SYSTEM_MEMORY_LIST_COMMAND (requires SeProfileSingleProcessPrivilege) // 80
    SystemFileCacheInformationEx, // q: SYSTEM_FILECACHE_INFORMATION; s (requires SeIncreaseQuotaPrivilege) (same as SystemFileCacheInformation)
    SystemThreadPriorityClientIdInformation, // s: SYSTEM_THREAD_CID_PRIORITY_INFORMATION (requires SeIncreaseBasePriorityPrivilege)
    SystemProcessorIdleCycleTimeInformation, // q: SYSTEM_PROCESSOR_IDLE_CYCLE_TIME_INFORMATION[]
    SystemVerifierCancellationInformation, // not implemented // name:wow64:whNT32QuerySystemVerifierCancellationInformation
    SystemProcessorPowerInformationEx, // not implemented
    SystemRefTraceInformation, // q; s // ObQueryRefTraceInformation
    SystemSpecialPoolInformation, // q; s (requires SeDebugPrivilege) // MmSpecialPoolTag, then MmSpecialPoolCatchOverruns != 0
    SystemProcessIdInformation, // q: SYSTEM_PROCESS_ID_INFORMATION
    SystemErrorPortInformation, // s (requires SeTcbPrivilege)
    SystemBootEnvironmentInformation, // q: SYSTEM_BOOT_ENVIRONMENT_INFORMATION // 90
    SystemHypervisorInformation, // q; s (kernel-mode only)
    SystemVerifierInformationEx, // q; s
    SystemTimeZoneInformation, // s (requires SeTimeZonePrivilege)
    SystemImageFileExecutionOptionsInformation, // s: SYSTEM_IMAGE_FILE_EXECUTION_OPTIONS_INFORMATION (requires SeTcbPrivilege)
    SystemCoverageInformation, // q; s // name:wow64:whNT32QuerySystemCoverageInformation; ExpCovQueryInformation
    SystemPrefetchPatchInformation, // not implemented
    SystemVerifierFaultsInformation, // s (requires SeDebugPrivilege)
    SystemSystemPartitionInformation, // q: SYSTEM_SYSTEM_PARTITION_INFORMATION
    SystemSystemDiskInformation, // q: SYSTEM_SYSTEM_DISK_INFORMATION
    SystemProcessorPerformanceDistribution, // q: SYSTEM_PROCESSOR_PERFORMANCE_DISTRIBUTION // 100
    SystemNumaProximityNodeInformation, // q
    SystemDynamicTimeZoneInformation, // q; s (requires SeTimeZonePrivilege)
    SystemCodeIntegrityInformation, // q // SeCodeIntegrityQueryInformation
    SystemProcessorMicrocodeUpdateInformation, // s
    SystemProcessorBrandString, // q // HaliQuerySystemInformation -> HalpGetProcessorBrandString, info class 23
    SystemVirtualAddressInformation, // q: SYSTEM_VA_LIST_INFORMATION[]; s: SYSTEM_VA_LIST_INFORMATION[] (requires SeIncreaseQuotaPrivilege) // MmQuerySystemVaInformation
    SystemLogicalProcessorAndGroupInformation, // q: SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX // since WIN7 // KeQueryLogicalProcessorRelationship
    SystemProcessorCycleTimeInformation, // q: SYSTEM_PROCESSOR_CYCLE_TIME_INFORMATION[]
    SystemStoreInformation, // q; s // SmQueryStoreInformation
    SystemRegistryAppendString, // s: SYSTEM_REGISTRY_APPEND_STRING_PARAMETERS // 110
    SystemAitSamplingValue, // s: ULONG (requires SeProfileSingleProcessPrivilege)
    SystemVhdBootInformation, // q: SYSTEM_VHD_BOOT_INFORMATION
    SystemCpuQuotaInformation, // q; s // PsQueryCpuQuotaInformation
    SystemNativeBasicInformation, // not implemented
    SystemSpare1, // not implemented
    SystemLowPriorityIoInformation, // q: SYSTEM_LOW_PRIORITY_IO_INFORMATION
    SystemTpmBootEntropyInformation, // q: TPM_BOOT_ENTROPY_NT_RESULT // ExQueryTpmBootEntropyInformation
    SystemVerifierCountersInformation, // q: SYSTEM_VERIFIER_COUNTERS_INFORMATION
    SystemPagedPoolInformationEx, // q: SYSTEM_FILECACHE_INFORMATION; s (requires SeIncreaseQuotaPrivilege) (info for WorkingSetTypePagedPool)
    SystemSystemPtesInformationEx, // q: SYSTEM_FILECACHE_INFORMATION; s (requires SeIncreaseQuotaPrivilege) (info for WorkingSetTypeSystemPtes) // 120
    SystemNodeDistanceInformation, // q
    SystemAcpiAuditInformation, // q: SYSTEM_ACPI_AUDIT_INFORMATION // HaliQuerySystemInformation -> HalpAuditQueryResults, info class 26
    SystemBasicPerformanceInformation, // q: SYSTEM_BASIC_PERFORMANCE_INFORMATION // name:wow64:whNtQuerySystemInformation_SystemBasicPerformanceInformation
    SystemQueryPerformanceCounterInformation, // q: SYSTEM_QUERY_PERFORMANCE_COUNTER_INFORMATION // since WIN7 SP1
    SystemSessionBigPoolInformation, // since WIN8
    SystemBootGraphicsInformation,
    SystemScrubPhysicalMemoryInformation,
    SystemBadPageInformation,
    SystemProcessorProfileControlArea,
    SystemCombinePhysicalMemoryInformation, // 130
    SystemEntropyInterruptTimingCallback,
    SystemConsoleInformation,
    SystemPlatformBinaryInformation,
    SystemThrottleNotificationInformation,
    SystemHypervisorProcessorCountInformation,
    SystemDeviceDataInformation,
    SystemDeviceDataEnumerationInformation,
    SystemMemoryTopologyInformation,
    SystemMemoryChannelInformation,
    SystemBootLogoInformation, // 140
    SystemProcessorPerformanceInformationEx, // q: SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION_EX // since WINBLUE
    SystemSpare0,
    SystemSecureBootPolicyInformation,
    SystemPageFileInformationEx, // q: SYSTEM_PAGEFILE_INFORMATION_EX
    SystemSecureBootInformation,
    SystemEntropyInterruptTimingRawInformation,
    SystemPortableWorkspaceEfiLauncherInformation,
    SystemFullProcessInformation, // q: SYSTEM_PROCESS_INFORMATION with SYSTEM_PROCESS_INFORMATION_EXTENSION (requires admin)
    SystemKernelDebuggerInformationEx, // q: SYSTEM_KERNEL_DEBUGGER_INFORMATION_EX
    SystemBootMetadataInformation, // 150
    SystemSoftRebootInformation,
    SystemElamCertificateInformation,
    SystemOfflineDumpConfigInformation,
    SystemProcessorFeaturesInformation, // q: SYSTEM_PROCESSOR_FEATURES_INFORMATION
    SystemRegistryReconciliationInformation,
    SystemEdidInformation,
    SystemManufacturingInformation, // q: SYSTEM_MANUFACTURING_INFORMATION // since THRESHOLD
    SystemEnergyEstimationConfigInformation, // q: SYSTEM_ENERGY_ESTIMATION_CONFIG_INFORMATION
    SystemHypervisorDetailInformation, // q: SYSTEM_HYPERVISOR_DETAIL_INFORMATION
    SystemProcessorCycleStatsInformation, // q: SYSTEM_PROCESSOR_CYCLE_STATS_INFORMATION // 160
    SystemVmGenerationCountInformation,
    SystemTrustedPlatformModuleInformation, // q: SYSTEM_TPM_INFORMATION
    SystemKernelDebuggerFlags,
    SystemCodeIntegrityPolicyInformation,
    SystemIsolatedUserModeInformation,
    SystemHardwareSecurityTestInterfaceResultsInformation,
    SystemSingleModuleInformation, // q: SYSTEM_SINGLE_MODULE_INFORMATION
    SystemAllowedCpuSetsInformation,
    SystemDmaProtectionInformation, // q: SYSTEM_DMA_PROTECTION_INFORMATION
    SystemInterruptCpuSetsInformation,
    SystemSecureBootPolicyFullInformation,
    SystemCodeIntegrityPolicyFullInformation,
    SystemAffinitizedInterruptProcessorInformation,
    SystemRootSiloInformation, // q: SYSTEM_ROOT_SILO_INFORMATION
    SystemCpuSetInformation, // q: SYSTEM_CPU_SET_INFORMATION // since THRESHOLD2
    SystemCpuSetTagInformation, // q: SYSTEM_CPU_SET_TAG_INFORMATION
    SystemWin32WerStartCallout,
    SystemSecureKernelProfileInformation,
    MaxSystemInfoClass
  );

  TSystemInformationClass = SYSTEM_INFORMATION_CLASS;

  TProcessTimes = record
    CreateTime,
    ExitTime,
    KernelTime,
    UserTime: LARGE_INTEGER;
  end;

  OBJECT_INFORMATION_CLASS = (
    ObjectBasicInformation,
    ObjectNameInformation,
    ObjectTypeInformation,
    ObjectAllTypesInformation,
    ObjectHandleInformation);
  TObjectInformationClass = OBJECT_INFORMATION_CLASS;

  OBJECT_NAME_INFORMATION = UNICODE_STRING;
  POBJECT_NAME_INFORMATION = ^OBJECT_NAME_INFORMATION;
  TObjectNameInformation = OBJECT_NAME_INFORMATION;
  PObjectNameInformation = ^TObjectNameInformation;

  SYSTEM_PROCESSOR_TIMES = packed record
    IdleTime: LARGE_INTEGER;
    KernelTime: LARGE_INTEGER;
    UserTime: LARGE_INTEGER;
    DpcTime: LARGE_INTEGER;
    InterruptTime: LARGE_INTEGER;
    InterruptCount: ULONG;
  end;
  TSystemProcessorTimes = SYSTEM_PROCESSOR_TIMES;
  PSystemProcessorTimes = ^TSystemProcessorTimes;

  CLIENT_ID = record
    UniqueProcess: ULONG_PTR;
    UniqueThread: ULONG_PTR;
  end;
  PCLIENT_ID = ^CLIENT_ID;
  TClientID = CLIENT_ID;
  PClientID = PCLIENT_ID;

  KPRIORITY = LONG;

  KWAIT_REASON = (
    Executive,
    FreePage,
    PageIn,
    PoolAllocation,
    DelayExecution,
    Suspended,
    UserRequest,
    WrExecutive,
    WrFreePage,
    WrPageIn,
    WrPoolAllocation,
    WrDelayExecution,
    WrSuspended,
    WrUserRequest,
    WrEventPair,
    WrQueue,
    WrLpcReceive,
    WrLpcReply,
    WrVirtualMemory,
    WrPageOut,
    WrRendezvous,
    WrKeyedEvent,
    WrTerminated,
    WrProcessInSwap,
    WrCpuRateControl,
    WrCalloutStack,
    WrKernel,
    WrResource,
    WrPushLock,
    WrMutex,
    WrQuantumEnd,
    WrDispatchInt,
    WrPreempted,
    WrYieldExecution,
    WrFastMutex,
    WrGuardedMutex,
    WrRundown,
    WrAlertByThreadId,
    WrDeferredPreempt,
    WrPhysicalFault,
    MaximumWaitReason);
  TKWaitReason = KWAIT_REASON;

  THREAD_STATE = (
    StateInitialized,
    StateReady,
    StateRunning,
    StateStandby,
    StateTerminated,
    StateWait,
    StateTransition,
    StateDeferredReady,
    StateGateWaitObsolete,
    StateWaitingForProcessInSwap,
    StateMaximumThreadState);
  TThreadState = THREAD_STATE;

  SYSTEM_THREAD_INFORMATION = record
    KernelTime: LARGE_INTEGER;
    UserTime: LARGE_INTEGER;
    CreateTime: LARGE_INTEGER;
    WaitTime: ULONG;
    StartAddress: PVOID;
    ClientId: CLIENT_ID;
    Priority: KPRIORITY;
    BasePriority: LONG;
    ContextSwitchCount: ULONG;
    State: ULONG;
    WaitReason: ULONG;
  end;
  TSystemThreadInformation = SYSTEM_THREAD_INFORMATION;
  PSystemThreadInformation = ^TSystemThreadInformation;

  VM_COUNTERS = record
    PeakVirtualSize: SIZE_T;
    VirtualSize: SIZE_T;
    PageFaultCount: ULONG;
    PeakWorkingSetSize: SIZE_T;
    WorkingSetSize: SIZE_T;
    QuotaPeakPagedPoolUsage: SIZE_T;
    QuotaPagedPoolUsage: SIZE_T;
    QuotaPeakNonPagedPoolUsage: SIZE_T;
    QuotaNonPagedPoolUsage: SIZE_T;
    PagefileUsage: SIZE_T;
    PeakPagefileUsage: SIZE_T;
    PrivatePageCount: SIZE_T;
  end;
  TVMCounters = VM_COUNTERS;
  PVMCounters = ^TVMCounters;

  IO_COUNTERSEX  = record
    ReadOperationCount: LARGE_INTEGER;
    WriteOperationCount: LARGE_INTEGER;
    OtherOperationCount: LARGE_INTEGER;
    ReadTransferCount: LARGE_INTEGER;
    WriteTransferCount: LARGE_INTEGER;
    OtherTransferCount: LARGE_INTEGER;
  end;
  TIOCounters = IO_COUNTERSEX;
  PIoCounters = ^TIoCounters;

const
  NonPagedPool = 0;
  PagedPool = 1;
  NonPagedPoolMustSucceed = 2;
  DontUseThisType = 3;
  NonPagedPoolCacheAligned = 4;
  PagedPoolCacheAligned = 5;
  NonPagedPoolCacheAlignedMustS = 6;
  MaxPoolType = 7;
  NonPagedPoolSession = 32;
  PagedPoolSession = NonPagedPoolSession + 1;
  NonPagedPoolMustSucceedSession = PagedPoolSession + 1;
  DontUseThisTypeSession = NonPagedPoolMustSucceedSession + 1;
  NonPagedPoolCacheAlignedSession = DontUseThisTypeSession + 1;
  PagedPoolCacheAlignedSession = NonPagedPoolCacheAlignedSession + 1;
  NonPagedPoolCacheAlignedMustSSession = PagedPoolCacheAlignedSession + 1;

type
  POOL_TYPE = NonPagedPool..NonPagedPoolCacheAlignedMustSSession;

  _SYSTEM_BASIC_INFORMATION = record // Information Class 0
    Reserved: ULONG;
	  TimerResolution: ULONG;
  	PageSize: ULONG;
	  NumberOfPhysicalPages: ULONG;
  	LowestPhysicalPageNumber: ULONG;
	  HighestPhysicalPageNumber: ULONG;
  	AllocationGranularity: ULONG;
	  MinimumUserModeAddress: ULONG_PTR;
  	MaximumUserModeAddress: ULONG_PTR;
	  ActiveProcessorsAffinityMask: KAFFINITY;
  	NumberOfProcessors: CCHAR;
  end;

  SYSTEM_BASIC_INFORMATION = _SYSTEM_BASIC_INFORMATION;
  PSYSTEM_BASIC_INFORMATION = ^SYSTEM_BASIC_INFORMATION;
  TSystemBasicInformation = SYSTEM_BASIC_INFORMATION;
  PSystemBasicInformation = ^TSystemBasicInformation;

  _SYSTEM_PROCESSOR_INFORMATION = record // Information Class 1
    ProcessorArchitecture: USHORT;
    ProcessorLevel: USHORT;
    ProcessorRevision: USHORT;
    Unknown: USHORT;
    FeatureBits: ULONG;
  end;
  SYSTEM_PROCESSOR_INFORMATION = _SYSTEM_PROCESSOR_INFORMATION;
  PSYSTEM_PROCESSOR_INFORMATION = ^SYSTEM_PROCESSOR_INFORMATION;
  TSystemProcessorInformation = SYSTEM_PROCESSOR_INFORMATION;
  PSystemProcessorInformation = ^TSystemProcessorInformation;

  SYSTEM_PROCESS_INFORMATION = record
    NextEntryDelta: ULONG;
    ThreadCount: ULONG;
    WorkingSetPrivateSize: LARGE_INTEGER;
    HardFaultCount: ULONG;
    NumberOfThreadsHighWatermark: ULONG;
    CycleTime: ULONGLONG;
    CreateTime: LARGE_INTEGER;
    UserTime: LARGE_INTEGER;
    KernelTime: LARGE_INTEGER;
    ProcessName: UNICODE_STRING;
    BasePriority: KPRIORITY;
    ProcessId,
    InheritedFromProcessId: HANDLE;
    HandleCount: ULONG;
    SessionId: ULONG;
    UniqueProcessKey: ULONG_PTR;
    VmCounters: VM_COUNTERS;
    IoCounters: IO_COUNTERSEX;  // Windows 2000 only
    Threads: array [0..255] of SYSTEM_THREAD_INFORMATION;
  end;
  TSystemProcessInformation = SYSTEM_PROCESS_INFORMATION;
  PSystemProcessInformation = ^TSystemProcessInformation;

  SYSTEM_MODULE = record
    Reserved: array[0..{$IFDEF WIN64}15{$ELSE}7{$ENDIF}] of Byte;
    ImageBase: NativeUInt;
    ImageSize: Cardinal;
    Flags: Cardinal;
    LoadOrderIndex: USHORT;
    //InitOrderIndex: USHORT;
    LoadCount: USHORT;
    //OffsetToFileName: USHORT;
    FullPathName: array [0..255] of AnsiChar;
  end;
  PSYSTEM_MODULE = ^SYSTEM_MODULE;
  TSystemModule = SYSTEM_MODULE;
  PSystemModule = ^SYSTEM_MODULE;

  SYSTEM_MODULE_INFORMATION = record
    Count: NativeUInt;
    Modules: array[0..0] of SYSTEM_MODULE;
  end;
  TSystemModuleInformation = SYSTEM_MODULE_INFORMATION;
  PSystemModuleInformation = ^TSystemModuleInformation;

  SYSTEM_HANDLE_TYPE = (
		OB_TYPE_UNKNOWN,
    OB_TYPE_TYPE,
    OB_TYPE_DIRECTORY,
    OB_TYPE_SYMBOLIC_LINK,
    OB_TYPE_TOKEN,
    OB_TYPE_PROCESS,
    OB_TYPE_THREAD,
    OB_TYPE_JOB,
    OB_TYPE_DEBUG_OBJECT,
    OB_TYPE_EVENT,
    OB_TYPE_EVENT_PAIR,
    OB_TYPE_MUTANT,
    OB_TYPE_CALLBACK,
    OB_TYPE_SEMAPHORE,
    OB_TYPE_TIMER,
    OB_TYPE_PROFILE,
    OB_TYPE_KEYED_EVENT,
    OB_TYPE_WINDOWS_STATION,
    OB_TYPE_DESKTOP,
    OB_TYPE_SECTION,
    OB_TYPE_KEY,
    OB_TYPE_PORT,
    OB_TYPE_WAITABLE_PORT,
    OB_TYPE_ADAPTER,
    OB_TYPE_CONTROLLER,
    OB_TYPE_DEVICE,
    OB_TYPE_DRIVER,
    OB_TYPE_IOCOMPLETION,
    OB_TYPE_FILE,
    OB_TYPE_WMIGUID);
  TSystemHandleType = SYSTEM_HANDLE_TYPE;

  TSystemHandleTypes = set of TSystemHandleType;

  PSystemHandleTableEntryInfo32 = ^TSystemHandleTableEntryInfo32;
  TSystemHandleTableEntryInfo32 = packed record
    UniqueProcessId      : WORD;  // 0x00
    CreatorBackTraceIndex: WORD;  // 0x02
    ObjectTypeIndex      : BYTE;   // 0x04
    HandleAttributes     : BYTE;   // 0x05
    HandleValue          : WORD;  // 0x06
    Object_              : Pointer;  // 0x08
    GrantedAccess        : DWORD;  // 0x0C
  end;                              // 0x10

  PSystemHandleTableEntryInfo64 = ^TSystemHandleTableEntryInfo64;
  TSystemHandleTableEntryInfo64 = packed record
    UniqueProcessId      : WORD;  // 0x00
    CreatorBackTraceIndex: WORD;  // 0x02
    ObjectTypeIndex      : BYTE;   // 0x04
    HandleAttributes     : BYTE;   // 0x05
    HandleValue          : WORD;  // 0x06
    Object_              : Pointer;  // 0x08
    GrantedAccess        : DWORD;  // 0x10
    __alignment_14       : DWORD;  // 0x14
  end;                              // 0x18

  PSystemHandleInformation32 = ^TSystemHandleInformation32;
  TSystemHandleInformation32 = packed record
    NumberOfHandles: DWORD;                                         // 0x00
    Handles        : array [0..0] of TSystemHandleTableEntryInfo32;  // 0x04
  end;                                                               // 0x14

  PSystemHandleInformation64 = ^TSystemHandleInformation64;
  TSystemHandleInformation64 = packed record
    NumberOfHandles: DWORD;                                         // 0x00
    __alignment_04 : DWORD;                                         // 0x04
    Handles        : array [0..0] of TSystemHandleTableEntryInfo64;  // 0x08
  end;                                                               // 0x20

  {$IFDEF WIN64}
  TSystemHandleTableEntryInfo = TSystemHandleTableEntryInfo64;
  TSystemHandleInformation = TSystemHandleInformation64;
  {$ELSE}
  TSystemHandleTableEntryInfo = TSystemHandleTableEntryInfo32;
  TSystemHandleInformation = TSystemHandleInformation32;
  {$ENDIF}

  PSystemHandleTableEntryInfo = ^TSystemHandleTableEntryInfo;
  PSystemHandleInformation = ^TSystemHandleInformation;

  OBJECT_BASIC_INFORMATION = record
    Attributes: ULONG;
    GrantedAccess: ACCESS_MASK;
    HandleCount: ULONG;
    PointerCount: ULONG;
    PagedPoolUsage: ULONG;
    NonPagedPoolUsage: ULONG;
    Reserved: array [0..2] of ULONG;
    NameInformationLength: ULONG;
    TypeInformationLength: ULONG;
    SecurityDescriptorLength: ULONG;
    CreateTime: LARGE_INTEGER;
  end;
  TObjectBasicInformation = OBJECT_BASIC_INFORMATION;
  PObjectBasicInformation = ^TObjectBasicInformation;

  OBJECT_TYPE_INFORMATION = record
    Name: UNICODE_STRING;
    TotalNumberOfObjects,
    TotalNumberOfHandles,
    TotalPagedPoolUsage,
    TotalNonPagedPoolUsage,
    TotalNamePoolUsage,
    TotalHandleTableUsage,
    HighWaterNumberOfObjects,
    HighWaterNumberOfHandles,
    HighWaterPagedPoolUsage,
    HighWaterNonPagedPoolUsage,
    HighWaterNamePoolUsage,
    HighWaterHandleTableUsage,
    InvalidAttributes: ULONG;
    GenericMapping: GENERIC_MAPPING;
    ValidAccess: ULONG;
    SecurityRequired,
    MaintainHandleCount: Boolean;
    TypeIndex: Byte;
    ReservedByte: Byte;
    PoolType,
    DefaultPagedPoolCharge,
    DefaultNonPagedPoolCharge: ULONG;
  end;
  TObjectTypeInformation = OBJECT_TYPE_INFORMATION;
  PObjectTypeInformation = ^TObjectTypeInformation;

  PROCESSINFOCLASS = (
    ProcessBasicInformation = 0, // 0, q: PROCESS_BASIC_INFORMATION, PROCESS_EXTENDED_BASIC_INFORMATION
    ProcessQuotaLimits, // qs: QUOTA_LIMITS, QUOTA_LIMITS_EX
    ProcessIoCounters, // q: IO_COUNTERS
    ProcessVmCounters, // q: VM_COUNTERS, VM_COUNTERS_EX
    ProcessTimes, // q: KERNEL_USER_TIMES
    ProcessBasePriority, // s: KPRIORITY
    ProcessRaisePriority, // s: ULONG
    ProcessDebugPort, // q: HANDLE
    ProcessExceptionPort, // s: HANDLE
    ProcessAccessToken, // s: PROCESS_ACCESS_TOKEN
    ProcessLdtInformation, // 10
    ProcessLdtSize,
    ProcessDefaultHardErrorMode, // qs: ULONG
    ProcessIoPortHandlers, // (kernel-mode only)
    ProcessPooledUsageAndLimits, // q: POOLED_USAGE_AND_LIMITS
    ProcessWorkingSetWatch, // q: PROCESS_WS_WATCH_INFORMATION[]; s: void
    ProcessUserModeIOPL,
    ProcessEnableAlignmentFaultFixup, // s: BOOLEAN
    ProcessPriorityClass, // qs: PROCESS_PRIORITY_CLASS
    ProcessWx86Information,
    ProcessHandleCount, // 20, q: ULONG, PROCESS_HANDLE_INFORMATION
    ProcessAffinityMask, // s: KAFFINITY
    ProcessPriorityBoost, // qs: ULONG
    ProcessDeviceMap, // qs: PROCESS_DEVICEMAP_INFORMATION, PROCESS_DEVICEMAP_INFORMATION_EX
    ProcessSessionInformation, // q: PROCESS_SESSION_INFORMATION
    ProcessForegroundInformation, // s: PROCESS_FOREGROUND_BACKGROUND
    ProcessWow64Information, // q: ULONG_PTR
    ProcessImageFileName, // q: UNICODE_STRING
    ProcessLUIDDeviceMapsEnabled, // q: ULONG
    ProcessBreakOnTermination, // qs: ULONG
    ProcessDebugObjectHandle, // 30, q: HANDLE
    ProcessDebugFlags, // qs: ULONG
    ProcessHandleTracing, // q: PROCESS_HANDLE_TRACING_QUERY; s: size 0 disables, otherwise enables
    ProcessIoPriority, // qs: ULONG
    ProcessExecuteFlags, // qs: ULONG
    ProcessResourceManagement,
    ProcessCookie, // q: ULONG
    ProcessImageInformation, // q: SECTION_IMAGE_INFORMATION
    ProcessCycleTime, // q: PROCESS_CYCLE_TIME_INFORMATION
    ProcessPagePriority, // q: ULONG
    ProcessInstrumentationCallback, // 40
    ProcessThreadStackAllocation, // s: PROCESS_STACK_ALLOCATION_INFORMATION, PROCESS_STACK_ALLOCATION_INFORMATION_EX
    ProcessWorkingSetWatchEx, // q: PROCESS_WS_WATCH_INFORMATION_EX[]
    ProcessImageFileNameWin32, // q: UNICODE_STRING
    ProcessImageFileMapping, // q: HANDLE (input)
    ProcessAffinityUpdateMode, // qs: PROCESS_AFFINITY_UPDATE_MODE
    ProcessMemoryAllocationMode, // qs: PROCESS_MEMORY_ALLOCATION_MODE
    ProcessGroupInformation, // q: USHORT[]
    ProcessTokenVirtualizationEnabled, // s: ULONG
    ProcessConsoleHostProcess, // q: ULONG_PTR
    ProcessWindowInformation, // 50, q: PROCESS_WINDOW_INFORMATION
    ProcessHandleInformation, // q: PROCESS_HANDLE_SNAPSHOT_INFORMATION // since WIN8
    ProcessMitigationPolicy, // s: PROCESS_MITIGATION_POLICY_INFORMATION
    ProcessDynamicFunctionTableInformation,
    ProcessHandleCheckingMode,
    ProcessKeepAliveCount, // q: PROCESS_KEEPALIVE_COUNT_INFORMATION
    ProcessRevokeFileHandles, // s: PROCESS_REVOKE_FILE_HANDLES_INFORMATION
    MaxProcessInfoClass);
  TProcessInfoClass = PROCESSINFOCLASS;

  PLIST_ENTRY = ^_LIST_ENTRY;

  _LDR_DATA_TABLE_ENTRY = record // not packed!
    case Integer of
  (*   *)0: (
  (*000*)InLoadOrderLinks: LIST_ENTRY
        );
  (*   *)1: (
  (*000*)InMemoryOrderLinks: LIST_ENTRY
        );
  (*   *)2: (
  (*000*)InInitializationOrderLinks: LIST_ENTRY;
  (*008*)DllBase: Pointer;
  (*00c*)EntryPoint: Pointer;
  (*010*)SizeOfImage: ULONG;
  (*014*)FullDllName: UNICODE_STRING;
  (*01c*)BaseDllName: UNICODE_STRING;
  (*024*)Flags: ULONG;
  (*028*)LoadCount: Word;
  (*02a*)TlsIndex: Word;
  (*02c*)HashLinks: LIST_ENTRY;
  (*034*)SectionPointer: Pointer;
  (*038*)CheckSum: ULONG;
  (*03C*)TimeDateStamp: ULONG;
  (*040*)LoadedImports: Pointer;
  (*044*)EntryPointActivationContext: Pointer; // PACTIVATION_CONTEXT
  (*048*)PatchInformation: Pointer;
        )
  end;
  LDR_DATA_TABLE_ENTRY = _LDR_DATA_TABLE_ENTRY;
  PLDR_DATA_TABLE_ENTRY = ^_LDR_DATA_TABLE_ENTRY;
  PPLDR_DATA_TABLE_ENTRY = ^PLDR_DATA_TABLE_ENTRY;
  TLdrDataTableEntry = _LDR_DATA_TABLE_ENTRY;
  PLdrDataTableEntry = ^_LDR_DATA_TABLE_ENTRY;

  _PEB_LDR_DATA = record
    Length: ULONG;
    Initialized: BOOLEAN;
    SsHandle: HANDLE;
    InLoadOrderModuleList: LIST_ENTRY;
    InMemoryOrderModuleList: LIST_ENTRY;
    InInitializationOrderModuleList: LIST_ENTRY;
    EntryInProgress: PVOID;
    ShutdownInProgress: BOOLEAN;
    ShutdownThreadId: HANDLE;
  end;
  PEB_LDR_DATA = _PEB_LDR_DATA;
  PPEB_LDR_DATA = ^_PEB_LDR_DATA;
  PPPEB_LDR_DATA = ^PPEB_LDR_DATA;
  TPebLdrData = _PEB_LDR_DATA;
  PPebLdrData = ^_PEB_LDR_DATA;

  PString = ^TString;
  _STRING = record
    Length: USHORT;
    MaximumLength: USHORT;
    Buffer: PCHAR;
  end;
  TString = _STRING;

  _RTL_DRIVE_LETTER_CURDIR = record
    Flags: Word;
    Length: Word;
    TimeStamp: ULONG;
    DosPath: _STRING;
  end;
  RTL_DRIVE_LETTER_CURDIR = _RTL_DRIVE_LETTER_CURDIR;
  PRTL_DRIVE_LETTER_CURDIR = ^_RTL_DRIVE_LETTER_CURDIR;
  PPRTL_DRIVE_LETTER_CURDIR = ^PRTL_DRIVE_LETTER_CURDIR;
  TRtlDriveLetterCurdir = _RTL_DRIVE_LETTER_CURDIR;
  PRtlDriveLetterCurdir = ^_RTL_DRIVE_LETTER_CURDIR;

  _CURDIR = record // not packed!
    DosPath: UNICODE_STRING;
    Handle: HANDLE;
  end;
  CURDIR = _CURDIR;
  PCURDIR = ^_CURDIR;
  PPCURDIR = ^PCURDIR;
  TCurdir = _CURDIR;

  _RTL_USER_PROCESS_PARAMETERS = record
    MaximumLength: ULONG;
    Length: ULONG;
    Flags: ULONG; // Bit 0: all pointers normalized
    DebugFlags: ULONG;
    ConsoleHandle: HANDLE;
    ConsoleFlags: ULONG;
    StandardInput: HANDLE;
    StandardOutput: HANDLE;
    StandardError: HANDLE;
    CurrentDirectory: CURDIR;
    DllPath: UNICODE_STRING;
    ImagePathName: UNICODE_STRING;
    CommandLine: UNICODE_STRING;
    Environment: PVOID;
    StartingX: ULONG;
    StartingY: ULONG;
    CountX: ULONG;
    CountY: ULONG;
    CountCharsX: ULONG;
    CountCharsY: ULONG;
    FillAttribute: ULONG;
    WindowFlags: ULONG;
    ShowWindowFlags: ULONG;
    WindowTitle: UNICODE_STRING;
    DesktopInfo: UNICODE_STRING;
    ShellInfo: UNICODE_STRING;
    RuntimeData: UNICODE_STRING;
    CurrentDirectories: array[0..31] of RTL_DRIVE_LETTER_CURDIR;
    EnvironmentSize: ULONG;
    EnvironmentVersion: ULONG;
    PackageDependencyData: PVOID;
    ProcessGroupId: ULONG;
    LoaderThreads: ULONG;
  end;
  RTL_USER_PROCESS_PARAMETERS = _RTL_USER_PROCESS_PARAMETERS;
  PRTL_USER_PROCESS_PARAMETERS = ^_RTL_USER_PROCESS_PARAMETERS;
  PPRTL_USER_PROCESS_PARAMETERS = ^PRTL_USER_PROCESS_PARAMETERS;
  TRtlUserProcessParameters = _RTL_USER_PROCESS_PARAMETERS;
  PRtlUserProcessParameters = ^_RTL_USER_PROCESS_PARAMETERS;
  TProcessParameters = _RTL_USER_PROCESS_PARAMETERS;
  PProcessParameters = ^_RTL_USER_PROCESS_PARAMETERS;

  PPEB_FREE_BLOCK = ^_PEB_FREE_BLOCK;
  _PEB_FREE_BLOCK = record // not packed!
  (*000*)Next: PPEB_FREE_BLOCK;
  (*004*)Size: ULONG;
  end;
  PEB_FREE_BLOCK = _PEB_FREE_BLOCK;
  PPPEB_FREE_BLOCK = ^PPEB_FREE_BLOCK;
  TPebFreeBlock = _PEB_FREE_BLOCK;
  PPebFreeBlock = ^_PEB_FREE_BLOCK;

  _SYSTEM_STRINGS = record // not packed!
  (*000*)SystemRoot: UNICODE_STRING; // %SystemRoot%
  (*008*)System32Root: UNICODE_STRING; // %SystemRoot%\System32
  (*010*)BaseNamedObjects: UNICODE_STRING; // \BaseNamedObjects
  end;
  SYSTEM_STRINGS = _SYSTEM_STRINGS;
  PSYSTEM_STRINGS = ^_SYSTEM_STRINGS;
  PPSYSTEM_STRINGS = ^PSYSTEM_STRINGS;
  TSystemStrings = _SYSTEM_STRINGS;
  PSystemStrings = ^_SYSTEM_STRINGS;

  _TEXT_INFO = record // not packed!
  (*000*)Reserved: Pointer;
  (*004*)SystemStrings: PSYSTEM_STRINGS;
  end;
  TEXT_INFO = _TEXT_INFO;
  PTEXT_INFO = ^_TEXT_INFO;
  PPTEXT_INFO = ^PTEXT_INFO;
  TTextInfo = _TEXT_INFO;
  PTextInfo = ^_TEXT_INFO;

  {$IFDEF FPC}
  PRTLCriticalSection = ^TRTLCriticalSection;
  PRTLCriticalSectionDebug = ^TRTLCriticalSectionDebug;
  _RTL_CRITICAL_SECTION_DEBUG = record
    Type_18: Word;
    CreatorBackTraceIndex: Word;
    CriticalSection: PRTLCriticalSection;
    ProcessLocksList: TListEntry;
    EntryCount: DWORD;
    ContentionCount: DWORD;
    Spare: array[0..1] of DWORD;
  end;
  TRTLCriticalSectionDebug = _RTL_CRITICAL_SECTION_DEBUG;
  RTL_CRITICAL_SECTION_DEBUG = _RTL_CRITICAL_SECTION_DEBUG;

  _RTL_CRITICAL_SECTION = record
    DebugInfo: PRTLCriticalSectionDebug;
    LockCount: Longint;
    RecursionCount: Longint;
    OwningThread: THandle;
    LockSemaphore: THandle;
    Reserved: ULONG_PTR;
  end;
  TRTLCriticalSection = _RTL_CRITICAL_SECTION;
  RTL_CRITICAL_SECTION = _RTL_CRITICAL_SECTION;
  {$ENDIF}

  PRTL_CRITICAL_SECTION = ^RTL_CRITICAL_SECTION;

  {$IFNDEF WIN64}
  GDI_HANDLE_BUFFER = array[0..33] of ULONG;
  {$ELSE}
  GDI_HANDLE_BUFFER = array[0..59] of ULONG;
  {$ENDIF}

  _PEB = record
    InheritedAddressSpace: BOOLEAN;
    ReadImageFileExecOptions: BOOLEAN;
    BeingDebugged: BOOLEAN;
    BitField: BOOLEAN;
    Mutant: HANDLE;
    ImageBaseAddress: Pointer;
    Ldr: PPEB_LDR_DATA;
    ProcessParameters: PRTL_USER_PROCESS_PARAMETERS;
    SubSystemData: Pointer;
    ProcessHeap: Pointer;
    FastPebLock: PRTL_CRITICAL_SECTION;
    AtlThunkSListPtr: PVOID;
    IFEOKey: PVOID;
    CrossProcessFlags: ULONG;
    KernelCallbackTable: PVOID;
    SystemReserved: ULONG;
    AtlThunkSListPtr32: ULONG;
    ApiSetMap: PVOID;
    TlsExpansionCounter: ULONG;
    TlsBitmap: PVOID;
    TlsBitmapBits: array[0..1] of ULONG;
    ReadOnlySharedMemoryBase: PVOID;
    HotpatchInformation: PVOID;
    ReadOnlyStaticServerData: PPVOID;
    AnsiCodePageData: PVOID;
    OemCodePageData: PVOID;
    UnicodeCaseTableData: PVOID;
    NumberOfProcessors: ULONG;
    NtGlobalFlag: ULONG;
    CriticalSectionTimeout: LARGE_INTEGER;
    HeapSegmentReserve: SIZE_T;
    HeapSegmentCommit: SIZE_T;
    HeapDeCommitTotalFreeThreshold: SIZE_T;
    HeapDeCommitFreeBlockThreshold: SIZE_T;
    NumberOfHeaps: ULONG;
    MaximumNumberOfHeaps: ULONG;
    ProcessHeaps: PPVOID;
    GdiSharedHandleTable: PVOID;
    ProcessStarterHelper: PVOID;
    GdiDCAttributeList: ULONG;
    LoaderLock: PRTL_CRITICAL_SECTION;
    OSMajorVersion: ULONG;
    OSMinorVersion: ULONG;
    OSBuildNumber: USHORT;
    OSCSDVersion: USHORT;
    OSPlatformId: ULONG;
    ImageSubsystem: ULONG;
    ImageSubsystemMajorVersion: ULONG;
    ImageSubsystemMinorVersion: ULONG;
    ImageProcessAffinityMask: ULONG_PTR;
    GdiHandleBuffer: GDI_HANDLE_BUFFER;
    PostProcessInitRoutine: PVOID;
    TlsExpansionBitmap: PVOID;
    TlsExpansionBitmapBits: array[0..31] of ULONG;
    SessionId: ULONG;
    AppCompatFlags: ULARGE_INTEGER;
    AppCompatFlagsUser: ULARGE_INTEGER;
    pShimData: PVOID;
    AppCompatInfo: PVOID;
    CSDVersion: UNICODE_STRING;
    ActivationContextData: PVOID;
    ProcessAssemblyStorageMap: PVOID;
    SystemDefaultActivationContextData: PVOID;
    SystemAssemblyStorageMap: PVOID;
    MinimumStackCommit: SIZE_T;
    FlsCallback: PPVOID;
    FlsListHead: LIST_ENTRY;
    FlsBitmap: PVOID;
    FlsBitmapBits: array[0..Round(128/(sizeof(ULONG)*8))-1] of ULONG;
    FlsHighIndex: ULONG;
    WerRegistrationData: PVOID;
    WerShipAssertPtr: PVOID;
    pContextData: PVOID;
    pImageHeaderHash: PVOID;
    TracingFlags: ULONG;
    CsrServerReadOnlySharedMemoryBase: ULONGLONG;
  end;

  PEB = _PEB;
  TPEB = PEB;
  PPEB = ^PEB;

  PROCESS_BASIC_INFORMATION = record
    ExitStatus: NTSTATUS;
    PebBaseAddress: PPEB;
    AffinityMask: KAFFINITY;
    BasePriority: KPRIORITY;
    UniqueProcessId: ULONG_PTR;
    InheritedFromUniqueProcessId: ULONG_PTR;
  end;

  TProcessBasicInformation = PROCESS_BASIC_INFORMATION;
  PProcessBasicInformation = ^TProcessBasicInformation;

  PPROCESS_PARAMETERS = ^PROCESS_PARAMETERS;
  PROCESS_PARAMETERS = record
    AllocationSize: Cardinal;
    Size: Cardinal;
    Flags: Cardinal;
    Reserved: Cardinal;
    Console: Cardinal;
    ProcessGroup: Cardinal;
    hStdInput: THandle;
    hStdOutput: THandle;
    hStdError: THandle;
    CurrentDir: WideString;
    CurrentDirectoryHandle: THandle;
    LoadSearchPath: UNICODE_STRING;
    ImageName: UNICODE_STRING;
    CommandLine: UNICODE_STRING;
    Enviroment: LPWSTR;
    dwX: Cardinal;
    dwY: Cardinal;
    dwXSize: Cardinal;
    dwYSize: Cardinal;
    dwXCountChars: Cardinal;
    dwYCountChars: Cardinal;
    dwFillAttributes: Cardinal;
    dwFlags: Cardinal;
    wShowWindow: Cardinal;
    WindowTitle: UNICODE_STRING;
    Desktop: UNICODE_STRING;
    Reserved1: UNICODE_STRING;
    Reserved2: UNICODE_STRING;
  end;

  MODULE_HEADER = record
    Unknown: array[0..1] of Cardinal;
    LoadOrder: LIST_ENTRY;
    MemOrder: LIST_ENTRY;
    InitOrder: LIST_ENTRY;
  end;

  PPROCESS_MODULE_INFO = ^PROCESS_MODULE_INFO;
  PROCESS_MODULE_INFO = record
    Size: cardinal;
    ModuleHeader: MODULE_HEADER;
  end;

  PPROCESS_SESSION_INFORMATION = ^PROCESS_SESSION_INFORMATION;
  PROCESS_SESSION_INFORMATION = record
    SessionId: Cardinal;
  end;
  TProcessSessionInformation = PROCESS_SESSION_INFORMATION;
  PProcessSessionInformation = ^TProcessSessionInformation;

  PRTL_BITMAP = ^RTL_BITMAP;
  RTL_BITMAP = record
    SizeOfBitMap: cardinal; //* Number of bits in the bitmap */
    BitMapBuffer: PByte; //* Bitmap data, assumed sized to a ULONG_PTR boundary */
  end;

  THREADINFOCLASS = (
    ThreadBasicInformation, // q: THREAD_BASIC_INFORMATION
    ThreadTimes, // q: KERNEL_USER_TIMES
    ThreadPriority, // s: KPRIORITY
    ThreadBasePriority, // s: LONG
    ThreadAffinityMask, // s: KAFFINITY
    ThreadImpersonationToken, // s: HANDLE
    ThreadDescriptorTableEntry, // q: DESCRIPTOR_TABLE_ENTRY (or WOW64_DESCRIPTOR_TABLE_ENTRY)
    ThreadEnableAlignmentFaultFixup, // s: BOOLEAN
    ThreadEventPair,
    ThreadQuerySetWin32StartAddress, // q: PVOID
    ThreadZeroTlsCell, // 10
    ThreadPerformanceCount, // q: LARGE_INTEGER
    ThreadAmILastThread, // q: ULONG
    ThreadIdealProcessor, // s: ULONG
    ThreadPriorityBoost, // qs: ULONG
    ThreadSetTlsArrayAddress,
    ThreadIsIoPending, // q: ULONG
    ThreadHideFromDebugger, // s: void
    ThreadBreakOnTermination, // qs: ULONG
    ThreadSwitchLegacyState,
    ThreadIsTerminated, // q: ULONG // 20
    ThreadLastSystemCall, // q: THREAD_LAST_SYSCALL_INFORMATION
    ThreadIoPriority, // qs: IO_PRIORITY_HINT
    ThreadCycleTime, // q: THREAD_CYCLE_TIME_INFORMATION
    ThreadPagePriority, // q: ULONG
    ThreadActualBasePriority,
    ThreadTebInformation, // q: THREAD_TEB_INFORMATION (requires THREAD_GET_CONTEXT + THREAD_SET_CONTEXT)
    ThreadCSwitchMon,
    ThreadCSwitchPmu,
    ThreadWow64Context, // q: WOW64_CONTEXT
    ThreadGroupInformation, // q: GROUP_AFFINITY // 30
    ThreadUmsInformation,
    ThreadCounterProfiling,
    ThreadIdealProcessorEx, // q: PROCESSOR_NUMBER
    ThreadCpuAccountingInformation, // since WIN8
    ThreadSuspendCount, // since WINBLUE
    ThreadHeterogeneousCpuPolicy, // q: KHETERO_CPU_POLICY // since THRESHOLD
    ThreadContainerId, // q: GUID
    ThreadNameInformation,
    ThreadSelectedCpuSets,
    ThreadSystemThreadInformation, // q: SYSTEM_THREAD_INFORMATION // 40
    ThreadActualGroupAffinity, // since THRESHOLD2
    MaxThreadInfoClass);
  TThreadInfoClass = THREADINFOCLASS;

  THREAD_BASIC_INFORMATION = record
    ExitStatus: NTSTATUS;
    TebBaseAddress: ULONG_PTR;
    ClientId: CLIENT_ID;
    AffinityMask: KAFFINITY;
    Priority: KPRIORITY;
    BasePriority: KPRIORITY;
  end;
  TThreadBasicInformation = THREAD_BASIC_INFORMATION;
  PThreadBasicInformation = ^TThreadBasicInformation;

  PNT_TIB = ^_NT_TIB;
  _NT_TIB = record
    ExceptionList: Pointer; // ^_EXCEPTION_REGISTRATION_RECORD
    StackBase,
      StackLimit,
      SubSystemTib: Pointer;
    case Integer of
      0: (
        FiberData: Pointer
        );
      1: (
        Version: ULONG;
        ArbitraryUserPointer: Pointer;
        Self: PNT_TIB;
        )
  end;
  NT_TIB = _NT_TIB;
  PPNT_TIB = ^PNT_TIB;

  _ACTIVATION_CONTEXT = record
    cbSize: ULONG;
    dwFlags: DWORD;
    lpSource: LPCWSTR;
    wProcessorArchitecture: USHORT;
    wLangId: LANGID;
    lpAssemblyDirectory: LPCTSTR;
    lpResourceName: LPCTSTR;
    lpApplicationName: LPCTSTR;
    hModule: HMODULE;
  end;
  ACTIVATION_CONTEXT = _ACTIVATION_CONTEXT;
  PACTIVATION_CONTEXT = ^_ACTIVATION_CONTEXT;
  PPACTIVATION_CONTEXT = ^PACTIVATION_CONTEXT;

  PRTL_ACTIVATION_CONTEXT_STACK_FRAME = ^_RTL_ACTIVATION_CONTEXT_STACK_FRAME;
  _RTL_ACTIVATION_CONTEXT_STACK_FRAME = record
    Previous: PRTL_ACTIVATION_CONTEXT_STACK_FRAME;
    ActivationContext: PACTIVATION_CONTEXT;
    Flags: ULONG;
  end;
  RTL_ACTIVATION_CONTEXT_STACK_FRAME = _RTL_ACTIVATION_CONTEXT_STACK_FRAME;
  PPRTL_ACTIVATION_CONTEXT_STACK_FRAME = ^PRTL_ACTIVATION_CONTEXT_STACK_FRAME;

  _ACTIVATION_CONTEXT_STACK = record
    Flags: ULONG;
    NextCookieSequenceNumber: ULONG;
    ActiveFrame: PRTL_ACTIVATION_CONTEXT_STACK_FRAME;
    FrameListCache: LIST_ENTRY;
  end;
  ACTIVATION_CONTEXT_STACK = _ACTIVATION_CONTEXT_STACK;
  PACTIVATION_CONTEXT_STACK = ^_ACTIVATION_CONTEXT_STACK;
  PPACTIVATION_CONTEXT_STACK = ^PACTIVATION_CONTEXT_STACK;

  _GDI_TEB_BATCH = record
    Offset: ULONG;
    HDC: HANDLE;
    Buffer: array[0..309] of ULONG;
  end;
  GDI_TEB_BATCH = _GDI_TEB_BATCH;
  PGDI_TEB_BATCH = ^_GDI_TEB_BATCH;
  PPGDI_TEB_BATCH = ^PGDI_TEB_BATCH;

  _TEB_ACTIVE_FRAME_CONTEXT = record
    Flags: ULONG;
    FrameName: PAnsiChar;
  end;
  TEB_ACTIVE_FRAME_CONTEXT = _TEB_ACTIVE_FRAME_CONTEXT;
  PTEB_ACTIVE_FRAME_CONTEXT = ^_TEB_ACTIVE_FRAME_CONTEXT;
  PPTEB_ACTIVE_FRAME_CONTEXT = ^PTEB_ACTIVE_FRAME_CONTEXT;

  PTEB_ACTIVE_FRAME = ^_TEB_ACTIVE_FRAME;
  _TEB_ACTIVE_FRAME = record
    Flags: ULONG;
    Previous: PTEB_ACTIVE_FRAME;
    Context: PTEB_ACTIVE_FRAME_CONTEXT;
  end;
  TEB_ACTIVE_FRAME = _TEB_ACTIVE_FRAME;
  PPTEB_ACTIVE_FRAME = ^PTEB_ACTIVE_FRAME;

  _PROCESSOR_NUMBER = record
    Group: WORD;
    Number: BYTE;
    Reserved: Byte;
  end;
  PROCESSOR_NUMBER = _PROCESSOR_NUMBER;
  PPROCESSOR_NUMBER = ^_PROCESSOR_NUMBER;

  _TEB = record
    NtTib: NT_TIB;
    EnvironmentPointer: PVOID;
    ClientId: CLIENT_ID;
    ActiveRpcHandle: PVOID;
    ThreadLocalStoragePointer: PVOID;
    Peb: PPEB;
    LastErrorValue: ULONG;
    CountOfOwnedCriticalSections: ULONG;
    CsrClientThread: PVOID;
    Win32ThreadInfo: PVOID;
    User32Reserved: array[0..25] of ULONG;
    UserReserved: array[0..4] of ULONG;
    WOW32Reserved: PVOID;
    CurrentLocale: LCID;
    FpSoftwareStatusRegister: ULONG;
    SystemReserved1: array[0..53] of PVOID;
    ExceptionCode: LONG;
    ActivationContextStack: PVOID; //ACTIVATION_CONTEXT_STACK;
    {$IFDEF WIN64}
    SpareBytes1: array[0..23] of UCHAR;
    {$ELSE}
    SpareBytes1: array[0..35] of UCHAR;
    {$ENDIF}
    TxFsContext: ULONG;
    GdiTebBatch: GDI_TEB_BATCH;
    RealClientId: CLIENT_ID;
    GdiCachedProcessHandle: HANDLE;
    GdiClientPID: ULONG;
    GdiClientTID: ULONG;
    GdiThreadLocalInfo: PVOID;
    Win32ClientInfo: array[0..61] of ULONG_PTR;
    glDispatchTable: array[0..232] of PVOID;
    glReserved1: array[0..28] of ULONG_PTR;
    glReserved2: PVOID;
    glSectionInfo: PVOID;
    glSection: PVOID;
    glTable: PVOID;
    glCurrentRC: PVOID;
    glContext: PVOID;
    LastStatusValue: NTSTATUS;
    StaticUnicodeString: UNICODE_STRING;
    StaticUnicodeBuffer: array[0..MAX_PATH] of WCHAR;
    DeallocationStack: PVOID;
    TlsSlots: array[0..63] of PVOID;
    TlsLinks: LIST_ENTRY;
    Vdm: PVOID;
    ReservedForNtRpc: PVOID;
    DbgSsReserved: array[0..1] of PVOID;
    HardErrorMode: ULONG;
    {$IFDEF WIN64}
    Instrumentation: array[0..10] of PVOID;
    {$ELSE}
    Instrumentation: array[0..8] of PVOID;
    {$ENDIF}
    ActivityId: TGUID;
    SubProcessTag: PVOID;
    EtwLocalData: PVOID;
    EtwTraceData: PVOID;
    WinSockData: PVOID;
    GdiBatchCount: ULONG;
    CurrentIdealProcessor: PROCESSOR_NUMBER;
    GuaranteedStackBytes: ULONG;
    ReservedForPerf: PVOID;
    ReservedForOle: PVOID;
    WaitingOnLoaderLock: ULONG;
    SavedPriorityState: PVOID;
    SoftPatchPtr1: ULONG_PTR;
    ThreadPoolData: PVOID;
    TlsExpansionSlots: PPVOID;
    {$IFDEF WIN64}
    DeallocationBStore: PVOID;
    BStoreLimit: PVOID;
    {$ENDIF}
    MuiGeneration: ULONG;
    IsImpersonating: ULONG;
    NlsCache: PVOID;
    pShimData: PVOID;
    HeapVirtualAffinity: ULONG;
    CurrentTransactionHandle: HANDLE;
    ActiveFrame: PTEB_ACTIVE_FRAME;
    FlsData: PVOID;
    PreferredLanguages: PVOID;
    UserPrefLanguages: PVOID;
    MergedPrefLanguages: PVOID;
    MuiImpersonation: ULONG;
    CrossTebFlags: USHORT;
    SameTebFlags: USHORT;
    TxnScopeEnterCallback: PVOID;
    TxnScopeExitCallback: PVOID;
    TxnScopeContext: PVOID;
    LockCount: ULONG;
    SpareUlong0: ULONG;
    ResourceRetValue: PVOID;
    ReservedForWdf: PVOID;
  end;
  TEB = _TEB;
  PTEB = ^_TEB;
  PPTEB = ^PTEB;

type
  PTOKEN_USER = ^TOKEN_USER;
  {$EXTERNALSYM PTOKEN_USER}
  _TOKEN_USER = record
    User: SID_AND_ATTRIBUTES;
  end;
  {$EXTERNALSYM _TOKEN_USER}
  TOKEN_USER = _TOKEN_USER;
  {$EXTERNALSYM TOKEN_USER}
  TTokenUser = TOKEN_USER;
  PTokenUser = PTOKEN_USER;

  PTOKEN_OWNER = ^TOKEN_OWNER;
  {$EXTERNALSYM PTOKEN_OWNER}
  _TOKEN_OWNER = record
    Owner: PSID;
  end;
  {$EXTERNALSYM _TOKEN_OWNER}
  TOKEN_OWNER = _TOKEN_OWNER;
  {$EXTERNALSYM TOKEN_OWNER}
  TTokenOwner = TOKEN_OWNER;
  PTokenOwner = PTOKEN_OWNER;

const
  TOKEN_SOURCE_LENGTH = 8;
  {$EXTERNALSYM TOKEN_SOURCE_LENGTH}

type
  PTOKEN_SOURCE = ^TOKEN_SOURCE;
  {$EXTERNALSYM PTOKEN_SOURCE}
  _TOKEN_SOURCE = record
    SourceName: array [0..TOKEN_SOURCE_LENGTH - 1] of ANSICHAR;
    SourceIdentifier: TLUID;
  end;
  {$EXTERNALSYM _TOKEN_SOURCE}
  TOKEN_SOURCE = _TOKEN_SOURCE;
  {$EXTERNALSYM TOKEN_SOURCE}
  TTokenSource = TOKEN_SOURCE;
  PTokenSource = PTOKEN_SOURCE;

  PTOKEN_PRIMARY_GROUP = ^TOKEN_PRIMARY_GROUP;
  {$EXTERNALSYM PTOKEN_PRIMARY_GROUP}
  _TOKEN_PRIMARY_GROUP = record
    PrimaryGroup: PSID;
  end;
  {$EXTERNALSYM _TOKEN_PRIMARY_GROUP}
  TOKEN_PRIMARY_GROUP = _TOKEN_PRIMARY_GROUP;
  {$EXTERNALSYM TOKEN_PRIMARY_GROUP}
  TTokenPrimaryGroup = TOKEN_PRIMARY_GROUP;
  PTokenPrimaryGroup = PTOKEN_PRIMARY_GROUP;

  PTOKEN_DEFAULT_DACL = ^TOKEN_DEFAULT_DACL;
  {$EXTERNALSYM PTOKEN_DEFAULT_DACL}
  _TOKEN_DEFAULT_DACL = record
    DefaultDacl: PACL;
  end;
  {$EXTERNALSYM _TOKEN_DEFAULT_DACL}
  TOKEN_DEFAULT_DACL = _TOKEN_DEFAULT_DACL;
  {$EXTERNALSYM TOKEN_DEFAULT_DACL}
  TTokenDefaultDacl = TOKEN_DEFAULT_DACL;
  PTokenDefaultDacl = PTOKEN_DEFAULT_DACL;

  _TOKEN_GROUPS_AND_PRIVILEGES = record
    SidCount: DWORD;
    SidLength: DWORD;
    Sids: PSIDANDATTRIBUTES;
    RestrictedSidCount: DWORD;
    RestrictedSidLength: DWORD;
    RestrictedSids: PSIDANDATTRIBUTES;
    PrivilegeCount: DWORD;
    PrivilegeLength: DWORD;
    Privileges: PLUIDANDATTRIBUTES;
    AuthenticationId: TLUID;
  end;
  {$EXTERNALSYM _TOKEN_GROUPS_AND_PRIVILEGES}
  TOKEN_GROUPS_AND_PRIVILEGES = _TOKEN_GROUPS_AND_PRIVILEGES;
  {$EXTERNALSYM TOKEN_GROUPS_AND_PRIVILEGES}
  PTOKEN_GROUPS_AND_PRIVILEGES = ^TOKEN_GROUPS_AND_PRIVILEGES;
  {$EXTERNALSYM PTOKEN_GROUPS_AND_PRIVILEGES}
  TTokenGroupsAndPrivileges = TOKEN_GROUPS_AND_PRIVILEGES;
  PTokenGroupsAndPrivileges = PTOKEN_GROUPS_AND_PRIVILEGES;

  OBJECT_ATTRIBUTES = packed record
     Length: Cardinal;
     RootDirectory: THandle;
     ObjectName: PUnicode_String;
     Attributes: Cardinal;
     SecurityDescriptor: Pointer;// Points to type SECURITY_DESCRIPTOR
     SecurityQualityOfService: Pointer;// Points to type SECURITY_QUALITY_OF_SERVICE
  end;
  TObjectAttributes = OBJECT_ATTRIBUTES;
  POBJECT_ATTRIBUTES = ^OBJECT_ATTRIBUTES;
  PObjectAttributes = POBJECT_ATTRIBUTES;

  PLARGE_INTEGER = ^LARGE_INTEGER;

  IO_STATUS_BLOCK = record
    //union {
    Status: ULONG_PTR;
    //    PVOID Pointer;
    //}
    Information: Cardinal;
  end;
  TIOStatusBlock = IO_STATUS_BLOCK;
  PIOStatusBlock = ^TIOStatusBlock;
  PIO_STATUS_BLOCK = ^IO_STATUS_BLOCK;

  FILE_INFORMATION_CLASS = (
    FileFiller0,
    FileDirectoryInformation,     // 1
    FileFullDirectoryInformation, // 2
    FileBothDirectoryInformation, // 3
    FileBasicInformation,         // 4  wdm
    FileStandardInformation,      // 5  wdm
    FileInternalInformation,      // 6
    FileEaInformation,            // 7
    FileAccessInformation,        // 8
    FileNameInformation,          // 9
    FileRenameInformation,        // 10
    FileLinkInformation,          // 11
    FileNamesInformation,         // 12
    FileDispositionInformation,   // 13
    FilePositionInformation,      // 14 wdm
    FileFullEaInformation,        // 15
    FileModeInformation,          // 16
    FileAlignmentInformation,     // 17
    FileAllInformation,           // 18
    FileAllocationInformation,    // 19
    FileEndOfFileInformation,     // 20 wdm
    FileAlternateNameInformation, // 21
    FileStreamInformation,        // 22
    FilePipeInformation,          // 23
    FilePipeLocalInformation,     // 24
    FilePipeRemoteInformation,    // 25
    FileMailslotQueryInformation, // 26
    FileMailslotSetInformation,   // 27
    FileCompressionInformation,   // 28
    FileObjectIdInformation,      // 29
    FileCompletionInformation,    // 30
    FileMoveClusterInformation,   // 31
    FileQuotaInformation,         // 32
    FileReparsePointInformation,  // 33
    FileNetworkOpenInformation,   // 34
    FileAttributeTagInformation,  // 35
    FileTrackingInformation,      // 36
    FileMaximumInformation);
  TFileInformationClass = FILE_INFORMATION_CLASS;


  PIO_APC_ROUTINE = procedure (ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; Reserved: ULONG); stdcall;

  _DEBUG_CONTROL_CODE = (
     // In the following five different versions of Windows NT, both
    nothing,
    SysDbgGetTraceInformation,// = 1,
    SysDbgSetInternalBreakpoint,// = 2,
    SysDbgSetSpecialCall,// = 3,
    SysDbgClearSpecialCalls ,// 4,
    SysDbgQuerySpecialCalls ,// 5,

    // The following is the new NT 5.1 (Windows XP)

    SysDbgDbgBreakPointWithStatus ,// 6,
    // Access KdVersionBlock
    SysDbgSysGetVersion ,// 7,
    // Space from the kernel to user space copy, or copy from the user space to user space
    // User space but not copy from the kernel space
    SysDbgCopyMemoryChunks_0 ,// 8,
    // SysDbgReadVirtualMemory ,// 8,

    // User space from the kernel space to copy, or copy from the user space to user space
    //But not copy from the kernel space to user space
    SysDbgCopyMemoryChunks_1 ,// 9,
    //SysDbgWriteVirtualMemory ,// 9,

    //Copy from the physical address space to users, not kernel space wrote
    SysDbgCopyMemoryChunks_2 ,// 10,
    //SysDbgReadVirtualMemory ,// 10,

    //Copy from the user to physical address space, can not read kernel space
    SysDbgCopyMemoryChunks_3 ,// 11,
    //SysDbgWriteVirtualMemory ,// 11,

    //Read-write processor related control block
    SysDbgSysReadControlSpace ,// 12,
    SysDbgSysWriteControlSpace ,// 13,

    //Read and write ports
    SysDbgSysReadIoSpace ,// 14,
    SysDbgSysWriteIoSpace ,// 15,

    //Call RDMSR @ 4 respectively, and _ WRMSR @ 12
    SysDbgSysReadMsr ,// 16,
    SysDbgSysWriteMsr ,// 17,

    //Read and write data bus
    SysDbgSysReadBusData ,// 18,
    SysDbgSysWriteBusData ,// 19,

    SysDbgSysCheckLowMemory ,// 20,

    //The following is the new NT 5.2 (Windows Server 2003)

    //Were called _ KdEnableDebugger @ 0 and _ KdDisableDebugger @ 0
    SysDbgEnableDebugger ,// 21,
    SysDbgDisableDebugger ,// 22,

    //Access and set up some of the variables related to debugging
    SysDbgGetAutoEnableOnEvent ,// 23,
    SysDbgSetAutoEnableOnEvent ,// 24,
    SysDbgGetPitchDebugger ,// 25,
    SysDbgSetDbgPrintBufferSize ,// 26,
    SysDbgGetIgnoreUmExceptions ,// 27,
    SysDbgSetIgnoreUmExceptions // 28
    );

  DEBUG_CONTROL_CODE = _DEBUG_CONTROL_CODE;
  TDebugControlCode = DEBUG_CONTROL_CODE;

{  _DBGKD_GET_VERSION64 = record
    MajorVersion: USHORT;
    MinorVersion: USHORT;
    ProtocolVersion: USHORT;
    Flags: USHORT;
    MachineType: USHORT;
    MaxPacketType: UCHAR;
    MaxStateChange: UCHAR;
    MaxManipulate: UCHAR;
    Simulation: UCHAR;
    Unused: array[0..0] of USHORT;
    KernBase: ULONG64;
    PsLoadedModuleList: ULONG64;
    DebuggerDataList: ULONG64;
  end;
  DBGKD_GET_VERSION64 = _DBGKD_GET_VERSION64;
  PDBGKD_GET_VERSION64 = ^DBGKD_GET_VERSION64;}

  MSR_STRUCT = record
    MsrNum: Cardinal; // MSR number
    NotUsed: Cardinal; // Never accessed by the kernel
    MsrLo: Cardinal; // IN (write) or OUT (read): Low 32 bits of MSR
    MsrHi: Cardinal; // IN (write) or OUT (read): High 32 bits of MSR
  end;

  IO_STRUCT = record
  	IoAddr: Cardinal;			// IN: Aligned to NumBytes,I/O address
	  Reserved1: Cardinal;		// Never accessed by the kernel
  	pBuffer: Pointer;			// IN (write) or OUT (read): Ptr to buffer
	  NumBytes: Cardinal;			// IN: # bytes to read/write. Only use 1, 2, or 4.
	  Reserved4: Cardinal;		// Must be 1
  	Reserved5: Cardinal;		// Must be 0
	  Reserved6: Cardinal;		// Must be 1
  	Reserved7: Cardinal;		// Never accessed by the kernel
  end;

  PFILE_NAME_INFORMATION = ^FILE_NAME_INFORMATION;
  FILE_NAME_INFORMATION = packed record
    FileNameLength: ULONG;
    FileName: array [0..MAX_PATH - 1] of WideChar;
  end;
  TFileNameInformation = FILE_NAME_INFORMATION;
  PFileNameInformation = ^TFileNameInformation;

  PFILE_POSITION_INFORMATION = ^FILE_POSITION_INFORMATION;
  FILE_POSITION_INFORMATION = packed record
    CurrentByteOffset: LARGE_INTEGER;
  end;
  TFilePositionInformation = FILE_POSITION_INFORMATION;
  PFilePositionInformation = ^TFilePositionInformation;

  _SECTION_IMAGE_INFORMATION = record // Information Class 1
    EntryPoint: PVOID;
    Unknown1: ULONG;
    StackReserve: ULONG;
    StackCommit: ULONG;
    Subsystem: ULONG;
    MinorSubsystemVersion: USHORT;
    MajorSubsystemVersion: USHORT;
    Unknown2: ULONG;
    Characteristics: ULONG;
    ImageNumber: USHORT;
    Executable: ByteBool;
    Unknown3: UCHAR;
    Unknown4: array[0..2] of ULONG;
  end;
  SECTION_IMAGE_INFORMATION = _SECTION_IMAGE_INFORMATION;
  PSECTION_IMAGE_INFORMATION = ^SECTION_IMAGE_INFORMATION;
  TSectionImageInformation = SECTION_IMAGE_INFORMATION;
  PSectionImageInformation = TSectionImageInformation;

  _RTL_PROCESS_INFORMATION = record
    Size: ULONG;
    hProcess: HANDLE;
    hThread: HANDLE;
    ClientId: CLIENT_ID;
    ImageInfo: SECTION_IMAGE_INFORMATION;
  end;
  RTL_PROCESS_INFORMATION = _RTL_PROCESS_INFORMATION;
  PRTL_PROCESS_INFORMATION = ^RTL_PROCESS_INFORMATION;
  TRtlProcessInformation = RTL_PROCESS_INFORMATION;
  PRtlProcessInformation = ^RTL_PROCESS_INFORMATION;

  PIn6Addr = ^in6_addr;
  in6_addr = record
    case Integer of
      0: (Byte: array [0..15] of uchar);
      1: (Word: array[0..7] of ushort);
  end;
  TIn6Addr = in6_addr;

  PGetHandleInfoThreadParam = ^TGetHandleInfoThreadParam;
  TGetHandleInfoThreadParam = record
    Typ: SYSTEM_HANDLE_TYPE;
    Handle: THandle;
    FilePos: LARGE_INTEGER;
    FileName: array [0..MAX_PATH - 1] of AnsiChar;
  end;

  THandleRecord = record
    PID: Cardinal;
    Handle: WORD;
    Typ: Byte;
    Access: Cardinal;
    Address: Cardinal;
    Name,
    TypeName: string;
    FilePos: Int64;
    _Exists: boolean;
    _ProcessName: string;
  end;
  PHandleRecord = ^THandleRecord;

const
  ViewShare = 1;
  ViewUnmap = 2;

type
  SECTION_INHERIT = ViewShare..ViewUnmap;

  TNativeQueryInformationToken = function(TokenHandle: THandle;
                                          TokenInformationClass: TTokenInformationClass;
                                          TokenInformation :Pointer;
                                          TokenInformationLength :Cardinal;
                                          ReturnLength :PCardinal): NTSTATUS; stdcall;

  TNativeOpenProcessToken = function(ProcessHandle: THandle;
                                     DesiredAccess: Cardinal;
                                     TokenHandle: PHandle): NTSTATUS; stdcall;

  TNativeOpenProcess = function(ProcessHandle: PHandle;
                                DesiredAccess: Cardinal;
                                ObjectAttributes: PObjectAttributes;
                                ClientId: PClientID): NTSTATUS; stdcall;

  TNativeOpenSection = function(SectionHandle: PHandle;
                                DesiredAccess: Cardinal;
                                ObjectAttributes: PObjectAttributes): NTSTATUS; stdcall;

  TNativeClose = function(Handle: THandle): NTSTATUS; stdcall;

  TNativeQuerySystemInformation = function(SystemInformationClass: TSystemInformationClass;
                                           SystemInformation: Pointer;
                                           SystemInformationLength: ULONG;
                                           ReturnLength: PULONG): NTSTATUS; stdcall;
  TNativeCreateSection = function(var SectionHandle: THANDLE;
                                  DesiredAccess: ACCESS_MASK;
                                  ObjectAttributes: POBJECT_ATTRIBUTES;
                                  SectionSize: PLARGE_INTEGER;
                                  Protect: Cardinal; Attributes: Cardinal;
                                  FileHandle: THANDLE): NTSTATUS; stdcall;
  TNativeMapViewOfSection = function(SectionHandle: THANDLE;
                                     ProcessHandle: THANDLE;
                                     BaseAddress: PPointer;
                                     ZeroBits: Cardinal;
                                     CommitSize: Cardinal;
                                     SectionOffset: PLARGE_INTEGER;
                                     ViewSize: PCardinal;
                                     InheritDisposition: SECTION_INHERIT;
                                     AllocationType: Cardinal;
                                     Protect: Cardinal): NTSTATUS; stdcall;
  TNativeUnmapViewOfSection = function(ProcessHandle: THANDLE; BaseAddress: Pointer): NTSTATUS; stdcall;
  TNativeOpenFile = function(FileHandle: PHANDLE;
                             DesiredAccess: ACCESS_MASK;
                             ObjectAttributes: POBJECTATTRIBUTES;
                             IoStatusBlock: PIOSTATUSBLOCK;
                             ShareAccess: Cardinal;
                             OpenOptions: Cardinal): NTSTATUS; stdcall;
  TNativeCreateFile = function (FileHandle: PHANDLE;
                                DesiredAccess: ACCESS_MASK;
                                ObjectAttributes: POBJECTATTRIBUTES;
                                IoStatusBlock: PIOSTATUSBLOCK;
                                AllocationSize: PLARGE_INTEGER;
                                FileAttributes: Cardinal;
                                ShareAccess: Cardinal;
                                CreateDisposition: Cardinal;
                                CreateOptions: Cardinal;
                                EaBuffer: Pointer;
                                EaLength: Cardinal): NTSTATUS; stdcall;
  TNativeQueryObject = function (ObjectHandle: THANDLE;
                                 ObjectInformationClass:
                                 OBJECT_INFORMATION_CLASS;
                                 ObjectInformation: PVOID;
                                 ObjectInformationLength: ULONG;
                                 ReturnLength: PULONG): NTSTATUS; stdcall;
  TNativeQueryInformationProcess = function (ProcessHandle: HANDLE;
                                             ProcessInformationClass: PROCESSINFOCLASS;
                                             ProcessInformation: PVOID;
                                             ProcessInformationLength: ULONG;
                                             ReturnLength: PULONG): NTSTATUS; stdcall;
  TNativeQueryInformationThread = function(ThreadHandle: HANDLE;
                                           ThreadInformationClass: THREADINFOCLASS;
                                           ThreadInformation: PVOID;
                                           ThreadInformationLength: ULONG;
                                           ReturnLength: PULONG): NTSTATUS; stdcall;
  TNativeQueryInformationFile = function(FileHandle: HANDLE;
                                         IoStatusBlock: PIO_STATUS_BLOCK;
                                         FileInformation: PVOID;
                                         FileInformationLength: ULONG;
                                         FileInformationClass: FILE_INFORMATION_CLASS): NTSTATUS; stdcall;
  TNativeDuplicateObject = function(SourceProcessHandle: HANDLE;
                                    SourceHandle: HANDLE;
                                    TargetProcessHandle: HANDLE;
                                    TargetHandle: PHANDLE;
                                    DesiredAccess: ACCESS_MASK;
                                    Attributes: ULONG;
                                    Options: ULONG): NTSTATUS; stdcall;

  TNativeCreateToken = function(TokenHandle:PHANDLE;
                                DesiredAccess: ACCESS_MASK;
                                ObjectAttributes: POBJECTATTRIBUTES;
                                Type_: TTOKENTYPE;
                                AuthenticationId: PLUID;
                                ExpirationTime: PLARGE_INTEGER;
                                User: PTOKEN_USER;
                                Groups: PTOKENGROUPS;
                                Privileges: PTOKENPRIVILEGES;
                                Owner: PTOKENOWNER;
                                PrimaryGroup: PTOKEN_PRIMARY_GROUP;
                                DefaultDacl: PTOKEN_DEFAULT_DACL;
                                Source: PTOKEN_SOURCE): NTSTATUS; stdcall;

  TNativeDeviceIoControlFile = function(FileHandle: HANDLE;
                                        Event: HANDLE;
                                        ApcRoutine: PIO_APC_ROUTINE;
                                        ApcContext: PVOID;
                                        IoStatusBlock: PIO_STATUS_BLOCK;
                                        IoControlCode: ULONG;
                                        InputBuffer: PVOID;
                                        InputBufferLength: ULONG;
                                        OutputBuffer: PVOID;
                                        OutputBufferLength: ULONG): NTSTATUS; stdcall;

  TNativeSystemDebugControl = function(ControlCode : DEBUG_CONTROL_CODE;
                                       InputBuffer : PVOID;
                                       InputBufferLength : ULONG;
                                       OutputBuffer : PVOID;
                                       OutputBufferLength : ULONG;
                                       ReturnLength : PULONG
                                       ): NTSTATUS; stdcall;

  TNativeCreateProcess = function(var ProcessHandle : Cardinal;
                                       DesiredAccess: ACCESS_MASK;
                                       ObjectAttributes: POBJECT_ATTRIBUTES;
                                       InheritFromProcessHandle: Cardinal;
                                       InheritHandles: Cardinal;
                                       SectionHandle: Cardinal;
                                       DebugPort: Cardinal;
                                       ExceptionPort: Cardinal): NTSTATUS; stdcall;

  TNativeCreateProcessEx = function(var ProcessHandle : Cardinal;
                                   DesiredAccess: ACCESS_MASK;
                                   ObjectAttributes: POBJECT_ATTRIBUTES;
                                   InheritFromProcessHandle: Cardinal;
                                   InheritHandles: Cardinal;
                                   SectionHandle: Cardinal;
                                   DebugPort: Cardinal;
                                   ExceptionPort: Cardinal;
                                   dwSaferFlags: Cardinal): NTSTATUS; stdcall;

  TNativeTerminateProcess = function (processHandle, exitCode: Cardinal) : NTSTATUS; stdcall;

  TRtlCreateUserProcess = function (ImageFileName: PUNICODE_STRING; Attributes: ULONG; ProcessParameters: PRTL_USER_PROCESS_PARAMETERS; ProcessSecurityDescriptor: PSECURITY_DESCRIPTOR; ThreadSecurityDescriptor: PSECURITY_DESCRIPTOR;
                                    ParentProcess: HANDLE; InheritHandles: BOOLEAN; DebugPort: HANDLE; ExceptionPort: HANDLE; ProcessInfo: PRTL_PROCESS_INFORMATION): NTSTATUS; stdcall;
  TRtlCreateProcessParameters = function (ProcessParameters: PPRTL_USER_PROCESS_PARAMETERS; ImageFile: PUNICODE_STRING; DllPath: PUNICODE_STRING; CurrentDirectory: PUNICODE_STRING; CommandLine: PUNICODE_STRING; CreationFlags: ULONG;
                                            WindowTitle: PUNICODE_STRING; Desktop: PUNICODE_STRING; Reserved: PUNICODE_STRING; Reserved2: PUNICODE_STRING): NTSTATUS; stdcall;
  TRtlAdjustPrivilege = function(Privilege: ULONG; Enable: Boolean; CurrentThread: Boolean; Enabled: PBOOLEAN): NTSTATUS; stdcall;

  TNtOpenThread = function(ThreadHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; ClientId: PCLIENT_ID): NTSTATUS; stdcall;

  TRtlIpv4AddressToString = function(Addr: PInAddr; S: {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}): {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; stdcall;
  TRtlIpv6AddressToString = function(Addr: PIn6Addr; S: {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}): {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; stdcall;

  TNtReadVirtualMemory = function(ProcessHandle: Handle; BaseAddress: PVOID; Buffer: PVOID; BufferLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;

  TRtlGetCompressionWorkSpaceSize = function(CompressionFormatAndEngine: ULONG; CompressBufferWorkSpaceSize, CompressFragmentWorkSpaceSize : PULONG): Cardinal; stdcall;
  TRtlCompressBuffer = function(CompressionFormatAndEngine: ULONG; SourceBuffer: Pointer; SourceBufferLength: ULONG; DestinationBuffer: Pointer; DestinationBufferLength: ULONG;
                           SourceChunkSize: ULONG; pDestinationSize: PULONG; WorkspaceBuffer: Pointer): Cardinal; stdcall;
  TRtlDeCompressBuffer = function(CompressionFormatAndEngine:ULONG; DestinationBuffer: Pointer; DestinationBufferLength: ULONG; SourceBuffer: Pointer; SourceBufferLength: ULONG;
                             pDestinationSize: PULONG): Cardinal; stdcall;
  TRtlDeCompressBufferEx = function(CompressionFormatAndEngine:ULONG; DestinationBuffer: Pointer; DestinationBufferLength: ULONG; SourceBuffer: Pointer; SourceBufferLength: ULONG;
                             pDestinationSize: PULONG; WorkSpace: PVOID): Cardinal; stdcall;
  TNtSuspendProcess = function(ProcessHandle: THandle): NTSTATUS; stdcall;
  TNtResumeProcess = function(ProcessHandle: THandle): NTSTATUS; stdcall;

const
  NTDLL_DLL = 'NTDLL.DLL';

  FILE_SUPERSEDE = $00000000;
  FILE_OPEN = $00000001;
  FILE_CREATE = $00000002;
  FILE_OPEN_IF = $00000003;
  FILE_OVERWRITE = $00000004;
  FILE_OVERWRITE_IF = $00000005;
  FILE_MAXIMUM_DISPOSITION = $00000005;

  FILE_SUPERSEDED = $00000000;
  FILE_OPENED = $00000001;
  FILE_CREATED = $00000002;
  FILE_OVERWRITTEN = $00000003;
  FILE_EXISTS = $00000004;
  FILE_DOES_NOT_EXIST = $00000005;
  FILE_READ_DATA            = $0001; // file & pipe

  //Valid values for the Attributes field
  OBJ_INHERIT = $00000002;
  OBJ_PERMANENT = $00000010;
  OBJ_EXCLUSIVE = $00000020;
  OBJ_CASE_INSENSITIVE = $00000040;
  OBJ_OPENIF = $00000080;
  OBJ_OPENLINK = $00000100;
  OBJ_VALID_ATTRIBUTES = $000001F2;

  PAGE_NOACCESS          = $01;
  PAGE_READONLY          = $02;
  PAGE_READWRITE         = $04;
  PAGE_WRITECOPY         = $08;
  PAGE_EXECUTE           = $10;
  PAGE_EXECUTE_READ      = $20;
  PAGE_EXECUTE_READWRITE = $40;
  PAGE_EXECUTE_WRITECOPY = $80;
  PAGE_GUARD             = $100;
  PAGE_NOCACHE           = $200;
  PAGE_WRITECOMBINE      = $400;

  SECTION_QUERY       = $0001;
  SECTION_MAP_WRITE   = $0002;
  SECTION_MAP_READ    = $0004;
  SECTION_MAP_EXECUTE = $0008;
  SECTION_EXTEND_SIZE = $0010;
  SECTION_ALL_ACCESS = (STANDARD_RIGHTS_REQUIRED or SECTION_QUERY or
    SECTION_MAP_WRITE or SECTION_MAP_READ or SECTION_MAP_EXECUTE or
    SECTION_EXTEND_SIZE);

  SEC_FILE               = $800000;
  SEC_IMAGE              = $1000000;
  SEC_RESERVE            = $4000000;
  SEC_COMMIT             = Cardinal($8000000);
  SEC_NOCACHE            = $10000000;

  UNICODE_MAX_PATH = MAX_PATH *sizeof(WCHAR);

  COMPRESSION_FORMAT_NONE         = $00000000;
  COMPRESSION_FORMAT_DEFAULT      = $00000001;
  COMPRESSION_FORMAT_LZNT1        = $00000002;
  COMPRESSION_FORMAT_XPRESS       = $00000003; //NS3
  COMPRESSION_FORMAT_XPRESS_HUFF  = $00000004;
  COMPRESSION_FORMAT_NS15         = $0000000F;
  COMPRESSION_FORMAT_SPARSE       = $00004000;

  COMPRESSION_ENGINE_STANDARD     = $00000000;
  COMPRESSION_ENGINE_MAXIMUM      = $00000100;
  COMPRESSION_ENGINE_HIBER        = $00000200;

implementation

end.
