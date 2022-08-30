unit uThreadEx;

interface

uses
  PSAPI, Tlhelp32, Windows, Classes, SysUtils, Graphics;

type
  NTStatus = Longint;
  LONG = Integer;
  PVOID = Pointer;
  KPRIORITY = LONG;
  TPDWord = ^DWORD;

  PUNICODE_STRING = ^TNtUnicodeString;
  _UNICODE_STRING = packed record
    Length: Word;
    MaximumLength: Word;
    Buffer: PWideChar;
  end;
  UNICODE_STRING = _UNICODE_STRING;
  TNtUnicodeString = _UNICODE_STRING;

  PCLIENT_ID = ^TClientId;
  _CLIENT_ID = packed record
    UniqueProcess: THandle;
    UniqueThread: THandle;
  end;
  TClientId = _CLIENT_ID;
  CLIENT_ID = _CLIENT_ID;

  _THREAD_STATE =
    (
    StateInitialized,
    StateReady,
    StateRunning,
    StateStandby,
    StateTerminated,
    StateWait,
    StateTransition,
    StateUnknown
    );
  THREAD_STATE = _THREAD_STATE;

  _THREADINFOCLASS = (
    ThreadBasicInformation,
    ThreadTimes,
    ThreadPriority,
    ThreadBasePriority,
    ThreadAffinityMask,
    ThreadImpersonationToken,
    ThreadDescriptorTableEntry,
    ThreadEnableAlignmentFaultFixup,
    ThreadEventPair_Reusable,
    ThreadQuerySetWin32StartAddress,
    ThreadZeroTlsCell,
    ThreadPerformanceCount,
    ThreadAmILastThread,
    ThreadIdealProcessor,
    ThreadPriorityBoost,
    ThreadSetTlsArrayAddress,
    ThreadIsIoPending,
    ThreadHideFromDebugger,
    ThreadBreakOnTermination,
    MaxThreadInfoClass
    );
  THREADINFOCLASS = _THREADINFOCLASS;

  _THREAD_BASIC_INFORMATION = packed record //    Information    Class    0
    ExitStatus: Longint;
    TebBaseAddress: Pointer;
    ClientId: CLIENT_ID;
    AffinityMask: Longint;
    Priority: Longint;
    BasePriority: Longint;
  end;
  THREAD_BASIC_INFORMATION = _THREAD_BASIC_INFORMATION;
  PTHREAD_BASIC_INFORMATION = ^_THREAD_BASIC_INFORMATION;

  PSYSTEM_INFORMATION_CLASS = ^TSystemInformationClass;
  _SYSTEM_INFORMATION_CLASS = (
    SystemBasicInformation,
    SystemProcessorInformation,
    SystemPerformanceInformation,
    SystemTimeOfDayInformation,
    SystemPathInformation,
    SystemProcessInformation
    );
  TSystemInformationClass = _SYSTEM_INFORMATION_CLASS;

  PIO_COUNTERSEX = ^TIoCountersex;
  _IO_COUNTERSEX = packed record
    ReadOperationCount: LARGE_INTEGER;
    WriteOperationCount: LARGE_INTEGER;
    OtherOperationCount: LARGE_INTEGER;
    ReadTransferCount: LARGE_INTEGER;
    WriteTransferCount: LARGE_INTEGER;
    OtherTransferCount: LARGE_INTEGER;
  end;
  TIoCountersex = _IO_COUNTERSEX;
  IO_COUNTERSEX = _IO_COUNTERSEX;

  PSYSTEM_THREAD_INFORMATION = ^TSystemThreadInfo;
  _SYSTEM_THREAD_INFORMATION = packed record
    KernelTime: LARGE_INTEGER;
    UserTime: LARGE_INTEGER;
    CreateTime: LARGE_INTEGER;
    WaitTime: DWORD;
    pStartAddress: PVOID;
    Cid: CLIENT_ID;
    Priority: DWORD;
    BasePriority: DWORD;
    ContextSwitches: DWORD;
    ThreadState: DWORD;
    WaitReason: DWORD;
    uReserved01: DWORD;
  end;
  TSystemThreadInfo = _SYSTEM_THREAD_INFORMATION;
  SYSTEM_THREAD_INFORMATION = _SYSTEM_THREAD_INFORMATION;

  PVM_COUNTERS = ^TVmCounters;
  _VM_COUNTERS = packed record
    uPeakVirtualSize: ULONG;
    uVirtualSize: ULONG;
    uPageFaultCount: ULONG;
    uPeakWorkingSetSize: ULONG;
    uWorkingSetSize: ULONG;
    uQuotaPeakPagedPoolUsage: ULONG;
    uQuotaPagedPoolUsage: ULONG;
    uQuotaPeakNonPagedPoolUsage: ULONG;
    uQuotaNonPagedPoolUsage: ULONG;
    uPagefileUsage: ULONG;
    uPeakPagefileUsage: ULONG;
  end;
  TVmCounters = _VM_COUNTERS;
  VM_COUNTERS = _VM_COUNTERS;

  PSYSTEM_PROCESS = ^TSystemProcess;
  _SYSTEM_PROCESS = packed record
    uNext: DWORD;
    ThreadCount: DWORD;
    Reserved01: LARGE_INTEGER;
    Reserved02: LARGE_INTEGER;
    Reserved03: LARGE_INTEGER;
    CreateTime: LARGE_INTEGER;
    UserTime: LARGE_INTEGER;
    KernelTime: LARGE_INTEGER;
    usName: UNICODE_STRING;
    BasePriority: DWORD;
    UniqueProcessId: DWORD;
    InheritedFromUniqueProcessId: DWORD;
    HandleCount: DWORD;
    SessionId: DWORD;
    Reserved08: DWORD;
    VmCounters: VM_COUNTERS;
    CommitCharge: DWORD;
  end;
  TSystemProcess = _SYSTEM_PROCESS;
  SYSTEM_PROCESS = _SYSTEM_PROCESS;

  PSYSTEM_PROCESS_NT4 = ^TSystemProcessNt4;
  _SYSTEM_PROCESS_NT4 = packed record
    Process: SYSTEM_PROCESS;
    Threads: array[0..0] of SYSTEM_THREAD_INFORMATION;
  end;
  TSystemProcessNt4 = _SYSTEM_PROCESS_NT4;
  SYSTEM_PROCESS_NT4 = _SYSTEM_PROCESS_NT4;

  PSYSTEM_PROCESS_NT5 = ^TSystemProcessNt5;
  _SYSTEM_PROCESS_NT5 = packed record
    Process: SYSTEM_PROCESS;
    IoCounters: IO_COUNTERSEX;
    Threads: array[0..0] of SYSTEM_THREAD_INFORMATION;
  end;
  TSystemProcessNt5 = _SYSTEM_PROCESS_NT5;
  SYSTEM_PROCESS_NT5 = _SYSTEM_PROCESS_NT5;

  PSYSTEM_PROCESS_INFORMATION = ^TSystemProcessInformation;
  _SYSTEM_PROCESS_INFORMATION = packed record
    case Integer of
      0: (Process_NT4: SYSTEM_PROCESS_NT4);
      1: (Process_NT5: SYSTEM_PROCESS_NT5);
  end;
  TSystemProcessInformation = _SYSTEM_PROCESS_INFORMATION;
  SYSTEM_PROCESS_INFORMATION = _SYSTEM_PROCESS_INFORMATION;

const
  STATUS_INFO_LENGTH_MISMATCH = NTStatus($C0000004);

function ZwQuerySystemInformation(ASystemInformationClass: UINT; ASystemInformation: Pointer; ASystemInformationLength: ULONG; AReturnLength: PULONG): NTStatus; stdcall; external 'ntdll.dll' name 'ZwQuerySystemInformation';

//function CheckThreadSuspend(PID: DWORD): BOOL;

implementation

uses
  ClMain;

function StrCompW(Str1, Str2: PWideChar): Integer;
asm
  PUSH EDI
  PUSH ESI
  MOV EDI, EDX
  MOV ESI, EAX
  MOV ECX, 0FFFFFFFFH
  XOR EAX, EAX
  REPNE SCASW
  NOT ECX
  MOV EDI, EDX
  XOR EDX, EDX
  REPE CMPSW
  MOV AX, [ESI - 2]
  MOV DX, [EDI - 2]
  SUB EAX, EDX
  POP ESI
  POP EDI
end;

(*
function CheckThreadSuspend(PID: DWORD): BOOL;
var
  spi, crt                  : PSYSTEM_PROCESS_INFORMATION;
  size, ModCount            : DWORD;
  j, Offset_ProcessTables   : Integer;
  //modName                   : array[0..MAX_PATH] of Char;
  //modhand                   : array[0..1024 - 1] of hModule;
  //thProcess                 : THandle;
begin
  Result := FALSE;
  if (ZwQuerySystemInformation(5, nil, 0, @size) = STATUS_INFO_LENGTH_MISMATCH) and (size > 0) then begin
    spi := AllocMem(size);
    if ZwQuerySystemInformation(5, spi, size, @size) = 0 then begin
      crt := spi;
      Offset_ProcessTables := 0;
      repeat
        crt := Pointer(DWORD(spi) + Offset_ProcessTables);
        if crt^.Process_NT5.Process.UniqueProcessId = GetCurrentProcessId then begin
          for j := 0 to crt^.Process_NT5.Process.ThreadCount - 1 do begin

            {thProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, FALSE, DWORD(crt^.Process_NT5.Threads[j].Cid.UniqueProcess));

            PSAPI.EnumProcessModules(thProcess, @modhand, SizeOf(modhand), ModCount);
            for i := 0 to (ModCount div SizeOf(DWORD)) - 1 do begin
            GetModuleFileNameEx(thProcess, modhand[i], modName, SizeOf(modName));
            ClMain.DebugOutStr(Format('线程ID %d %s', [crt^.Process_NT5.Threads[j].Cid.UniqueThread, modName]));
            end;}

            //ClMain.DebugOutStr(Format('线程ID %d', [crt^.Process_NT5.Threads[j].Cid.UniqueThread {WideCharTOString(crt^.Process_NT5.Process.usName.Buffer)}]));

            if (crt^.Process_NT5.Threads[j].Cid.UniqueThread = PID) then begin
              //DScreen.AddChatBoardString('WaitReason: ' + IntToStr(crt^.Process_NT5.Threads[j].WaitReason ), clYellow, clRed); // the thread is suspended
              //DScreen.AddChatBoardString('ThreadState: ' + IntToStr(crt^.Process_NT5.Threads[j].ThreadState), clYellow, clRed); // the thread is suspended

              {if crt^.Process_NT5.Threads[j].WaitReason = 5 then
                DScreen.AddChatBoardString('suspended...', clYellow, clRed) // the thread is suspended
              else
                DScreen.AddChatBoardString('not suspended...', clYellow, clRed); // the thread is not suspended}
              Break;
            end;
          end;
          Break;
        end;
        Offset_ProcessTables := Offset_ProcessTables + crt^.Process_NT5.Process.uNext;
      until (crt^.Process_NT5.Process.uNext = 0);

    end else begin
      ;                                 //HandleError;
    end;
    FreeMem(spi);
  end else begin
    ;                                   //HandleError;
  end;
end;
*)

end.

