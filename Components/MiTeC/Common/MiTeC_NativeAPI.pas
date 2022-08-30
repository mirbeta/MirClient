{*******************************************************}
{               MiTeC Common Routines                   }
{                   NT Native API                       }
{                                                       }
{                                                       }
{       Copyright (c) 1997-2019 Michal Mutl             }
{                                                       }
{*******************************************************}


{$INCLUDE Compilers.inc}

unit MiTeC_NativeAPI;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils,
     {$ELSE}
     Windows, SysUtils,
     {$ENDIF}
     MiTeC_Windows, MiTeC_NativeDefs;

var
  NTDLLHandle: THandle = 0;
  UnloadNTDLL: Boolean;

  NtOpenSection: TNativeOpenSection = nil;
  NtClose: TNativeClose = nil;
  NtQueryInformationToken: TNativeQueryInformationToken = nil;
  NtOpenProcessToken: TNativeOpenProcessToken = nil;
  NtOpenProcess: TNativeOpenProcess = nil;
  NtQuerySystemInformation: TNativeQuerySystemInformation = nil;
  NtCreateSection: TNativeCreateSection = nil;
  NtMapViewOfSection: TNativeMapViewOfSection = nil;
  NtUnmapViewOfSection: TNativeUnmapViewOfSection = nil;
  NtCreateFile: TNativeCreateFile = nil;
  NtOpenFile: TNativeOpenFile = nil;
  NtQueryObject: TNativeQueryObject = nil;
  NtQueryInformationProcess: TNativeQueryInformationProcess = nil;
  NtQueryInformationThread: TNativeQueryInformationThread = nil;
  NtQueryInformationFile: TNativeQueryInformationFile = nil;
  NtDuplicateObject: TNativeDuplicateObject = nil;
  NtCreateToken: TNativeCreateToken = nil;
  NtDeviceIoControlFile: TNativeDeviceIoControlFile = nil;
  NtSystemDebugControl: TNativeSystemDebugControl = nil;
  NtCreateProcess: TNativeCreateProcess = nil;
  NtTerminateProcess: TNativeTerminateProcess = nil;
  RtlAdjustPrivilege: TRtlAdjustPrivilege = nil;
  NtOpenThread: TNtOpenThread = nil;
  RtlIpv4AddressToString: TRtlIpv4AddressToString = nil;
  RtlIpv6AddressToString: TRtlIpv6AddressToString = nil;
  NtReadVirtualMemory: TNtReadVirtualMemory = nil;
  RtlGetCompressionWorkSpaceSize: TRtlGetCompressionWorkSpaceSize = nil;
  RtlDeCompressBuffer: TRtlDeCompressBuffer = nil;
  RtlDeCompressBufferEx: TRtlDeCompressBufferEx = nil;
  RtlCompressBuffer: TRtlCompressBuffer = nil;
  NtSuspendProcess: TNtSuspendProcess = nil;
  NtResumeProcess: TNtResumeProcess = nil;

function GetHandleInfoThreadExecute(Data: Pointer): integer;

function InitNativeAPI: boolean;
procedure FreeNativeAPI;

function NativeGetCPUCount: Byte;

const
  cKWaitReason: array[TKWaitReason] of string = (
    'Executive',
    'FreePage',
    'PageIn',
    'PoolAllocation',
    'DelayExecution',
    'Suspended',
    'UserRequest',
    'WrExecutive',
    'WrFreePage',
    'WrPageIn',
    'WrPoolAllocation',
    'WrDelayExecution',
    'WrSuspended',
    'WrUserRequest',
    'WrEventPair',
    'WrQueue',
    'WrLpcReceive',
    'WrLpcReply',
    'WrVirtualMemory',
    'WrPageOut',
    'WrRendezvous',
    'WrKeyedEvent',
    'WrTerminated',
    'WrProcessInSwap',
    'WrCpuRateControl',
    'WrCalloutStack',
    'WrKernel',
    'WrResource',
    'WrPushLock',
    'WrMutex',
    'WrQuantumEnd',
    'WrDispatchInt',
    'WrPreempted',
    'WrYieldExecution',
    'WrFastMutex',
    'WrGuardedMutex',
    'WrRundown',
    'WrAlertByThreadId',
    'WrDeferredPreempt',
    'WrPhysicalFault',
    'MaximumWaitReason');

  cThreadState: array[TThreadState] of string = (
    'Initialized',
    'Ready',
    'Running',
    'Standby',
    'Terminated',
    'Wait',
    'Transition',
    'DeferredReady',
    'GateWaitObsolete',
    'WaitingForProcessInSwap',
    'MaximumThreadState');

  cSystemHandleType: array[TSystemHandleType] of string = (
    'Unknown',
		'Type',
		'Directory',
		'SymbolicLink',
		'Token',
		'Process',
		'Thread',
		'Job',
    'DebugObject',
		'Event',
		'EventPair',
		'Mutant',
		'Callback',
		'Semaphore',
		'Timer',
		'Profile',
    'KeyedEvent',
		'WindowStation',
		'Desktop',
		'Section',
		'Key',
		'Port',
		'WaitablePort',
		'Adapter',
		'Controller',
		'Device',
		'Driver',
		'IOCompletion',
    'File',
    'WMIGUID');

//function CreateSystemToken: THandle;

implementation

//uses MiTeC_NtSecAPI, MiTeC_Routines;

function GetHandleInfoThreadExecute(Data: Pointer): integer;
var
  res: NTSTATUS;
  dwReturn: DWORD;
  FileNameInfo: FILE_NAME_INFORMATION;
  FilePosInfo: TFilePositionInformation;
  CurrentByteOffset: LARGE_INTEGER;
  ObjectNameInfo: TObjectNameInformation;
  IoStatusBlock: IO_STATUS_BLOCK;
  pThreadParam: TGetHandleInfoThreadParam;
begin
  Result:=-1;
  pThreadParam:=PGetHandleInfoThreadParam(Data)^;
  pThreadParam.FilePos.QuadPart:=0;
  dwReturn:=SizeOf(ObjectNameInfo)+MAX_PATH;
  res:=NtQueryObject(pThreadParam.Handle,ObjectNameInformation,@ObjectNameInfo,dwReturn,@dwReturn);
  if res=STATUS_SUCCESS then begin
    WideCharToMultiByte(CP_ACP,0,ObjectNameInfo.Buffer,ObjectNameInfo.Length div 2,@pThreadParam.FileName[0],MAX_PATH,nil,nil);
    Result:=0;
  end else if pThreadParam.Typ=OB_TYPE_FILE then begin
    ZeroMemory(@FileNameInfo,SizeOf(FILE_NAME_INFORMATION));
    res:=NtQueryInformationFile(pThreadParam.Handle,@IoStatusBlock,@FileNameInfo,SizeOf(FileNameInfo),FileNameInformation);
    if res=STATUS_SUCCESS then begin
      Result:=0;
      WideCharToMultiByte(CP_ACP,0,@FileNameInfo.FileName[0],IoStatusBlock.Information,@pThreadParam.FileName[0],MAX_PATH,nil,nil);
    end;
  end;

  if pThreadParam.Typ=OB_TYPE_FILE then begin
    res:=NtQueryInformationFile(pThreadParam.Handle,@IoStatusBlock,@FilePosInfo,SizeOf(FilePosInfo),FilePositionInformation);
    if res=STATUS_SUCCESS then
      pThreadParam.FilePos:=FilePosInfo.CurrentByteOffset;
  end;

  PGetHandleInfoThreadParam(Data)^:=pThreadParam;
  ExitThread(Result);
end;


function InitNativeAPI;
begin
  NTDLLHandle:=GetModuleHandle(NTDLL_DLL);
  UnloadNTDLL:=NTDLLHandle=0;
  if NTDLLHandle=0 then
    NTDLLHandle:=LoadLibrary(NTDLL_DLL);
  if (NTDLLHandle<>0) and not Assigned(NtQueryObject) then begin
    @NtQueryInformationToken:=GetProcAddress(NTDLLHandle,'NtQueryInformationToken');
    @NtOpenProcessToken:=GetProcAddress(NTDLLHandle,'NtOpenProcessToken');
    @NtOpenSection:=GetProcAddress(NTDLLHandle,'NtOpenSection');
    @NtClose:=GetProcAddress(NTDLLHandle,'NtClose');
    @NtOpenProcess:=GetProcAddress(NTDLLHandle,'NtOpenProcess');
    @NtCreateProcess:=GetProcAddress(NTDLLHandle,'NtCreateProcess');
    @NtQuerySystemInformation:=GetProcAddress(NTDLLHandle,'NtQuerySystemInformation');
    @NtCreateSection:=GetProcAddress(NTDLLHandle,'NtCreateSection');
    @NtCreateToken:=GetProcAddress(NTDLLHandle,'NtCreateToken');
    @NtMapViewOfSection:=GetProcAddress(NTDLLHandle,'NtMapViewOfSection');
    @NtUnmapViewOfSection:=GetProcAddress(NTDLLHandle,'NtUnmapViewOfSection');
    @NtOpenFile:=GetProcAddress(NTDLLHandle,'NtOpenFile');
    @NtCreateFile:=GetProcAddress(NTDLLHandle,'NtCreateFile');
    @NtQueryObject:=GetProcAddress(NTDLLHandle,'NtQueryObject');
    @NtQueryInformationProcess:=GetProcAddress(NTDLLHandle,'NtQueryInformationProcess');
    @NtQueryInformationThread:=GetProcAddress(NTDLLHandle,'NtQueryInformationThread');
    @NtQueryInformationFile:=GetProcAddress(NTDLLHandle,'NtQueryInformationFile');
    @NtDuplicateObject:=GetProcAddress(NTDLLHandle,'NtDuplicateObject');
    @NtDeviceIoControlFile:=GetProcAddress(NTDLLHandle,'NtDeviceIoControlFile');
    @NtSystemDebugControl:=GetProcAddress(NTDLLHandle,'ZwSystemDebugControl');
    @NtTerminateProcess:=GetProcAddress(NTDLLHandle,'ZwTerminateProcess');
    @RtlAdjustPrivilege:=GetProcAddress(NTDLLHandle,'RtlAdjustPrivilege');
    @NtOpenThread:=GetProcAddress(NTDLLHandle,'NtOpenThread');
    @RtlIpv4AddressToString:=GetProcAddress(NTDLLHandle,{$IFDEF UNICODE}'RtlIpv4AddressToStringW'{$ELSE}'RtlIpv4AddressToStringA'{$ENDIF});
    @RtlIpv6AddressToString:=GetProcAddress(NTDLLHandle,{$IFDEF UNICODE}'RtlIpv6AddressToStringW'{$ELSE}'RtlIpv6AddressToStringA'{$ENDIF});
    @NtReadVirtualMemory:=GetProcAddress(NTDLLHandle,'NtReadVirtualMemory');
    @RtlGetCompressionWorkSpaceSize:=GetProcAddress(NTDLLHandle,'RtlGetCompressionWorkSpaceSize');
    @RtlCompressBuffer:=GetProcAddress(NTDLLHandle,'RtlCompressBuffer');
    @RtlDeCompressBuffer:=GetProcAddress(NTDLLHandle,'RtlDecompressBuffer');
    @RtlDeCompressBufferEx:=GetProcAddress(NTDLLHandle,'RtlDecompressBufferEx');
    @NtSuspendProcess:=GetProcAddress(NTDLLHandle,'NtSuspendProcess');
    @NtResumeProcess:=GetProcAddress(NTDLLHandle,'NtResumeProcess');
  end;
  Result:=(NTDLLHandle<>0) and Assigned(NtQueryObject);
end;

procedure FreeNativeAPI;
begin
  if (NTDLLHandle<>0) and UnloadNTDLL then begin
    if not FreeLibrary(NTDLLHandle) then
      raise Exception.Create(Format('Unload Error: %s - 0x%x',[NTDLL_DLL,GetModuleHandle(NTDLL_DLL)]))
    else
      NTDLLHandle:=0;
  end;
end;

function NativeGetCPUCount: Byte;
var
  sbi: TSystemBasicInformation;
  r: NTSTATUS;
begin
  r:=NtQuerySystemInformation(SystemBasicInformation,@sbi,SizeOf(sbi),nil);
  if r<>STATUS_SUCCESS then
    Result:=1
  else
    Result:=sbi.NumberOfProcessors;
end;

initialization
  InitNativeAPI;
finalization
  FreeNativeAPI;
end.
