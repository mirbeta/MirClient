{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2010 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnHardWareInfo;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �����
* ��Ԫ���ƣ�Ӳ����Ϣ��Ԫ
* ��Ԫ���ߣ�SkyJacker
*           LiuXiao
*           Yock
*           Bahamut
* ��    ע��Ӳ����Ϣ��Ԫ��Ŀǰֻʵ�ֻ�ȡ��ˡ���CPUϵͳ��ָ��CPU�����к���ռ����
*           �Լ����� BIOS �� ID.
* ����ƽ̨��WindowsXP sp2 + Delphi 6.0 up2
* ���ݲ��ԣ�Win2000/XP + Delphi 5��6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* ��Ԫ��ʶ��$Id: CnHardWareInfo.pas 418 2010-02-08 04:53:54Z zhoujingyu $
* �޸ļ�¼��2008.08.01 V1.3
*               ���� Bahamut �Ļ�ȡ BIOS ID �Ĺ��̣���ֻ֧��С���� BIOS
*           2008.04.12 V1.2
*               LiuXiao ����� CPU �����������Ķ�ȡ���Ƿ�֧�� cpuid ָ�������к�
*               �����ԣ���л Yock��
*           2008.01.12 V1.1
*               LiuXiao ����� CPU ռ���ʵĶ�ȡ
*           2007.01.23 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, Windows, SysUtils, ExtCtrls;
  
type
  TCnCPUIdFormat = (ifContinuous, ifDashed);
  {* CPU���к�����Ϣ����ʾ��ʽ
   |<PRE>
     ifContinuous:  -������
     ifDashed:      -ʹ�÷ָ��'-'�ָ�
   |</PRE>
  }

  TCnCpuId = class(TPersistent)
  {CPU ��Ϣ��}
  private
    FTimer: TTimer;
    FCPUCount: Integer;
    FCPUIds: TStrings;
    FCPUInfos: TStrings;
    FSupportCpuIds: TList;
    FSupportCpuSns: TList;
    FCPUOems: TStrings;
    FCPUIdFormat: TCnCPUIdFormat;
    FCPUUsageRead: Boolean;
    FCPUUsage: array[0..255] of Integer; // �ܲ��ᳬ�� 256 �� CPU �ɣ�
    FCurCnt, FLastCnt: array[0..255] of Integer;
    FAverageCPUUsage: Integer;
    function GetFirstCPUId: string;
    function GetCPUId(Index: Integer): string;
    procedure SetCPUIdFormat(ACPUIdFormat: TCnCPUIdFormat);
    function GetAverageCPUUsage: Integer;
    function GetCPUUsage(Index: Integer): Integer;
    function GetFirstCPUUsage: Integer;

    function RefreshCPUUsages: Cardinal; // ֻ����ʱ����
    procedure CpuUsageTimer(Sender: TObject);
    function GetCPUOem(Index: Integer): string;
    function GetFirstCPUOem: string;
    function GetSupportCPUId(Index: Integer): Boolean;
    function GetSupportCPUSn(Index: Integer): Boolean;
    function GetFirstSupportCPUId: Boolean;
    function GetFirstSupportCPUSn: Boolean;
    function GetCPUInfoString(Index: Integer): string;
    function GetFirstCPUInfoString: string;
  public
    constructor Create;
    {* ���캯�������� FCPUIds ������ ReadCPUId}
    destructor Destroy; override;

    procedure ReadCPUId;
    {* ������� CPU �ں˵����кź�������Ϣ������������б�}

    property CPUIdFormat: TCnCPUIdFormat read FCPUIdFormat write SetCPUIdFormat;
    {* CPU ���к���ʾ��ʽ}
    property CPUCount: Integer read FCPUCount;
    {* ϵͳ�� CPU ������}
    property FirstCPUId: string read GetFirstCPUId;
    {* ��ȡ�׸� CPU �� ID�����ڵ� CPU ϵͳ}
    property FirstCPUInfoString: string read GetFirstCPUInfoString;
    {* ��ȡ�׸� CPU ����Ϣ�ַ��������ڵ� CPU ϵͳ}
    property FirstCPUOem: string read GetFirstCPUOem;
    {* ��ȡ�׸� CPU ���������̣����ڵ� CPU ϵͳ}
    property FirstSupportCPUId: Boolean read GetFirstSupportCPUId;
    {* ��ȡ�׸� CPU �Ƿ�֧�� CPUID ָ����ڵ� CPU ϵͳ}
    property FirstSupportCPUSn: Boolean read GetFirstSupportCPUSn;
    {* ��ȡ�׸� CPU �Ƿ�֧�ֶ�ȡ CPU ���кţ����ڵ� CPU ϵͳ}
    property SupportCPUId[Index: Integer]: Boolean read GetSupportCPUId;
    {* ��ȡָ�� CPU �Ƿ�֧�� CPUID ָ��}
    property SupportCPUSn[Index: Integer]: Boolean read GetSupportCPUSn;
    {* ��ȡָ�� CPU �Ƿ�֧�ֶ�ȡ CPU ���к�}
    property CPUId[Index: Integer]: string read GetCPUId;
    {* ���ָ�� CPU �����кš����� Index �� 0 ��ʼ��
       ��Ҫ˵�����ǣ����кźܶ� CPU ����ֹ��ȡ����˴�����ȫ 0����������������
       CPU ���кŶ�ȡ���ܶ�����¶���������ġ���Ϣ�ַ�����������}
    property CPUInfoString[Index: Integer]: string read GetCPUInfoString;
    {* ���ָ�� CPU ����Ϣ�ַ��������� Index �� 0 ��ʼ��
       ����Ϣ�ַ��������� CPU ��һЩ����˵������Ψһ���ܶ�����±����� CPU ID��}
    property CPUOem[Index: Integer]: string read GetCPUOem;
     {* ���ָ�� CPU ���������̡����� Index �� 0 ��ʼ}
    property CPUUsage[Index: Integer]: Integer read GetCPUUsage;
    {* ���ָ�� CPU ��ռ���ʣ�0 �� 100
       ��Ҫ˵�����ǣ������� NT ϵͳ�ϲ��ö�ʱ������� CPU ��æ�������ټ��������
       ����ڸ�ʵ������δ�������ʱ���õ��� CPU ռ���ʿ�����������ͬ��
    }
    property AverageCPUUsage: Integer read GetAverageCPUUsage;
    {* ���ƽ�� CPU ռ���ʣ�0 �� 100}
    property FirstCPUUsage: Integer read GetFirstCPUUsage;
    {* ����׸� CPU ��ռ���ʣ�0 �� 100�����ڵ� CPU ϵͳ}
  end;

function CnGetBiosID: string;
{* ��� BIOS �� ID��ֻ֧��С���� BIOS�����Ҿ�ʽ���������ڲ��淶���޷���ȡ ID}

implementation

const
  BiosOffset: array[0..2] of DWORD = ($6577, $7196, $7550);

type
  PUNICODE_STRING = ^TUNICODE_STRING;
  _UNICODE_STRING = record
    Length: Word;
    MaximumLength: Word;
    Buffer: PWChar;
  end;
  TUNICODE_STRING = _UNICODE_STRING;

  POBJECT_ATTRIBUTES = ^TOBJECT_ATTRIBUTES;
  _OBJECT_ATTRIBUTES = record
    Length: ULONG;
    RootDirectory: THandle;
    ObjectName: PUNICODE_STRING;
    Attributes: ULONG;
    SecurityDescriptor: Pointer;
    SecurityQualityOfService: Pointer;
  end;
  TOBJECT_ATTRIBUTES = _OBJECT_ATTRIBUTES;

  PLARGE_INTEGER = ^LARGE_INTEGER;
  PPByte = ^PByte;

  _SYSTEM_BASIC_INFORMATION = record
    Unknown: ULONG;
    MaximumIncrement: ULONG;
    PhysicalPageSize: ULONG;
    NumberOfPhysicalPages: ULONG;
    LowestPhysicalPage: ULONG;
    HighestPhysicalPage: ULONG;
    AllocationGranularity: ULONG;
    LowestUserAddress: ULONG;
    HighestUserAddress: ULONG;
    ActiveProcessors: ULONG;
    NumberProcessors: UCHAR;
  end;
  SYSTEM_BASIC_INFORMATION = _SYSTEM_BASIC_INFORMATION;
  PSYSTEM_BASIC_INFORMATION = ^SYSTEM_BASIC_INFORMATION;
  TSystemBasicInformation = SYSTEM_BASIC_INFORMATION;
  PSystemBasicInformation = ^TSystemBasicInformation;

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
  
  SYSTEM_INFORMATION_CLASS = (
          SystemBasicInformation,
          SystemProcessorInformation,
          SystemPerformanceInformation,
          SystemTimeOfDayInformation,
          SystemNotImplemented1,
          SystemProcessesAndThreadsInformation,
          SystemCallCounts,
          SystemConfigurationInformation,
          SystemProcessorTimes,
          SystemGlobalFlag,
          SystemNotImplemented2,
          SystemModuleInformation,
          SystemLockInformation,
          SystemNotImplemented3,
          SystemNotImplemented4,
          SystemNotImplemented5,
          SystemHandleInformation,
          SystemObjectInformation,
          SystemPagefileInformation,
          SystemInstructionEmulationCounts,
          SystemInvalidInfoClass1,
          SystemCacheInformation,
          SystemPoolTagInformation,
          SystemProcessorStatistics,
          SystemDpcInformation,
          SystemNotImplemented6,
          SystemLoadImage,
          SystemUnloadImage,
          SystemTimeAdjustment,
          SystemNotImplemented7,
          SystemNotImplemented8,
          SystemNotImplemented9,
          SystemCrashDumpInformation,
          SystemExceptionInformation,
          SystemCrashDumpStateInformation,
          SystemKernelDebuggerInformation,
          SystemContextSwitchInformation,
          SystemRegistryQuotaInformation,
          SystemLoadAndCallImage,
          SystemPrioritySeparation,
          SystemNotImplemented10,
          SystemNotImplemented11,
          SystemInvalidInfoClass2,
          SystemInvalidInfoClass3,
          SystemTimeZoneInformation,
          SystemLookasideInformation,
          SystemSetTimeSlipEvent,
          SystemCreateSession,
          SystemDeleteSession,
          SystemInvalidInfoClass4,
          SystemRangeStartInformation,
          SystemVerifierInformation,
          SystemAddVerifier,
          SystemSessionProcessesInformation);
  TSystemInformationClass = SYSTEM_INFORMATION_CLASS;
  
  TNativeQuerySystemInformation = function(SystemInformationClass:
    TSystemInformationClass; SystemInformation: Pointer; SystemInformationLength:
    Cardinal; ReturnLength: PDWORD): Cardinal; stdcall;

  TZwOpenSection = function (var hWnd: THandle; dwMask: DWORD; PObject: POBJECT_ATTRIBUTES): DWORD; stdcall;

  TZwMapViewOfSection = function (hWnd: THandle; ViewHandle: THandle; PBaseAddr: Pointer;
    dwLength: ULONG; dwAllocLen: ULONG; PRealAddr: PLARGE_INTEGER; PReadLen: PDWORD;
    dwInherite: DWORD; dwAllocType: ULONG; dwProtectType: ULONG): DWORD; stdcall;

  TZwUnmapViewOfSection = function (hWnd: THandle; PBaseAddr: Pointer): DWORD; stdcall;

const
  STATUS_SUCCESS = $00000000;

var
  NtDllHandle: THandle = 0;
  NtDllNeedFree: Boolean = False;

  NtQuerySystemInformation: TNativeQuerySystemInformation = nil;

  ZwOpenSection: TZwOpenSection = nil;

  ZwMapViewOfSection: TZwMapViewOfSection = nil;

  ZwUnmapViewOfSection: TZwUnmapViewOfSection = nil;

function GetNtNativeAPIs: Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    NtDllHandle := GetModuleHandle('NTDLL.DLL');
    if NtDllHandle = 0 then
    begin
      NtDllHandle := LoadLibrary('NTDLL.DLL');
      NtDllNeedFree := NtDllHandle <> 0;
    end;

    if NtDllHandle <> 0 then
    begin
//      @NtQueryInformationToken:=GetProcAddress(NtDllHandle,'NtQueryInformationToken');
//      @NtOpenProcessToken := GetProcAddress(NtDllHandle,'NtOpenProcessToken');
//      @NtOpenSection := GetProcAddress(NtDllHandle,'NtOpenSection');
//      @NtClose := GetProcAddress(NtDllHandle,'NtClose');
//      @NtOpenProcess := GetProcAddress(NtDllHandle,'NtOpenProcess');
      @NtQuerySystemInformation := GetProcAddress(NtDllHandle, 'NtQuerySystemInformation');
//      @NtCreateSection := GetProcAddress(NtDllHandle,'NtCreateSection');
//      @NtCreateToken := GetProcAddress(NtDllHandle,'NtCreateToken');
//      @NtMapViewOfSection := GetProcAddress(NtDllHandle,'NtMapViewOfSection');
//      @NtUnmapViewOfSection := GetProcAddress(NtDllHandle,'NtUnmapViewOfSection');
//      @NtOpenFile := GetProcAddress(NtDllHandle,'NtOpenFile');
//      @NtCreateFile := GetProcAddress(NtDllHandle,'NtCreateFile');
//      @NtQueryObject := GetProcAddress(NtDllHandle,'NtQueryObject');
//      @NtQueryInformationProcess := GetProcAddress(NtDllHandle,'NtQueryInformationProcess');
//      @NtQueryInformationThread := GetProcAddress(NtDllHandle,'NtQueryInformationThread');
//      @NtQueryInformationFile := GetProcAddress(NtDllHandle,'NtQueryInformationFile');
//      @NtDuplicateObject := GetProcAddress(NtDllHandle,'NtDuplicateObject');
//      @NtDeviceIoControlFile := GetProcAddress(NtDllHandle,'NtDeviceIoControlFile');

      @ZwOpenSection := GetProcAddress(NtDllHandle, 'ZwOpenSection');
      @ZwMapViewOfSection := GetProcAddress(NtDllHandle, 'ZwMapViewOfSection');
      @ZwUnmapViewOfSection := GetProcAddress(NtDllHandle, 'ZwUnmapViewOfSection');
    end;
  end;
  Result := NtDllHandle <> 0;
end;

procedure FreeNtNativeAPIs;
begin
  if (NtDllHandle <> 0) and NtDllNeedFree then
  begin
    FreeLibrary(NtDllHandle);
    NtDllHandle := 0;
  end;
end;

constructor TCnCpuId.Create;
begin
  FSupportCpuIds := TList.Create;
  FSupportCpuSns := TList.Create;
  FCPUIds := TStringList.Create;
  FCPUInfos := TStringList.Create;
  FCPUOems := TStringList.Create;
  FCPUIdFormat := ifContinuous;
  ReadCPUId;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := 1000;
  FTimer.OnTimer := CpuUsageTimer;
  FTimer.Enabled := True;
  RefreshCPUUsages;

  if Win32Platform = VER_PLATFORM_WIN32_NT then // NT ����Ҫ�����ж�
    FCPUUsageRead := False;
end;

destructor TCnCpuId.Destroy;
begin
  FTimer.Free;
  FCPUOems.Free;
  FCPUInfos.Free;
  FCPUIds.Free;
  FSupportCpuSns.Free;
  FSupportCpuIds.Free;
end;

// ��ȡ���� CPU �����к�
procedure TCnCpuId.ReadCPUId;
const
  SCN_CPUID_BIT = $200000; //CPU ID λ���
var
  I: Integer;
  Mask: Integer;
  CurrProc: THandle;
  SysInfo: TSystemInfo;
  ProcessAffinityOld: Cardinal;
  ProcessAffinity: Cardinal;
  SystemAffinity: Cardinal;

  function GetCnCpuIdSupport: Boolean;
  asm
    PUSHFD   //������ֱ�Ӵ�ȡ������ͨ����ջ
    POP     EAX
    MOV     EDX, EAX
    XOR     EAX, SCN_CPUID_BIT
    PUSH    EAX
    POPFD
    PUSHFD
    POP     EAX
    XOR     EAX,EDX  // ��� ID λ�Ƿ���Ӱ��
    JZ      @exit    // CPUID ��Ч
    MOV     AL, 1  // CPUID ��Ч
  @exit:
  end;

  // ��ȡ CPU ��Ϣ�ַ���
  function GetCnCPUInfoString: string;
  const
    cnIFContinuous = '%.8x%.8x%.8x%.8x';
    cnIFDashed = '%.8x-%.8x-%.8x-%.8x';
  var
    iEax,iEbx,iEcx,iEdx: Integer;
  begin
    asm
      push ebx
      push ecx
      push edx
      mov  eax, $1
      dw $A20F      //CPUID
      mov iEax, eax
      mov iEbx, ebx
      mov iEcx, ecx
      mov iEdx, edx
      pop edx
      pop ecx
      pop ebx
    end;
    if FCPUIdFormat = ifContinuous then
      Result := Format(cnIFContinuous, [iEax, iEbx, iEcx, iEdx])
    else
      Result := Format(cnIFDashed, [iEax, iEbx, iEcx, iEdx])
  end;

  // ��ȡ CPU ���к�
  function GetCnCPUID: string;
  const
    SCnIFContinuous = '%.4x%.4x%.4x%.4x%.4x%.4x';
    SCnIFDashed = '%.4x-%.4x-%.4x-%.4x-%.4x-%.4x';
  var
    SFmt: string;
    iEax, iEcx, iEdx, iEdx1: Integer;
  begin
    asm
      push ebx
      push ecx
      push edx
      mov  eax, $1
      dw $A20F      //CPUID
      mov iEax, eax
      mov iEdx1, edx
      mov eax, $3
      dw $A20F
      mov iEcx, ecx
      mov iEdx, edx
      pop edx
      pop ecx
      pop ebx
    end;

    if FCPUIdFormat=ifContinuous then
      SFmt := SCnIFContinuous
    else
      SFmt := SCnIFDashed;

    if iEdx1 and (1 shr 18) = 0 then // Cpu ���кŲ��ܶ�������ȫ0
    begin
      Result := Format(SFmt, [0, 0, 0, 0, 0, 0]);
      FSupportCpuSns.Add(nil); // �� False
    end
    else
    begin
      FSupportCpuSns.Add(Pointer(True));
      Result := Format(SFmt,
        [(iEax and $FFFF0000) shr 16, iEax and $FFFF,
         (iEcx and $FFFF0000) shr 16, iEcx and $FFFF,
         (iEdx and $FFFF0000) shr 16, iEdx and $FFFF]);
    end;
  end;

  // ��� CPU OEM ������
  function GetCnCPUOem: string;
  var
    iEax, iEbx, iEcx, iEdx: Integer;
  begin
    asm
      push ebx
      push ecx
      push edx
      mov  eax, $0
      dw $A20F      //CPUID
      mov iEax, eax
      mov iEbx, ebx
      mov iEcx, ecx
      mov iEdx, edx
      pop edx
      pop ecx
      pop ebx
    end;
    SetLength(Result, 3 * SizeOf(Integer));
    CopyMemory(@Result[1], @iEbx, SizeOf(Integer));
    CopyMemory(@Result[1 + SizeOf(Integer)], @iEdx, SizeOf(Integer));
    CopyMemory(@Result[1 + 2 * SizeOf(Integer)], @iEcx, SizeOf(Integer));
  end;
begin
  FCPUCount := 0;
  FSupportCpuIds.Clear;
  FSupportCpuSns.Clear;
  FCPUIds.Clear;
  FCPUOems.Clear;
  
  // ��ȡ CPU ����
  GetSystemInfo(SysInfo);
  FCPUCount := SysInfo.dwNumberOfProcessors;

  // ��ȡ���� CPU �����к�
  Mask := $1;
  CurrProc := GetCurrentProcess;
  if not GetProcessAffinityMask(CurrProc, ProcessAffinityOld, SystemAffinity) then
    Exit;

  try
    for I := 0 to FCpuCount - 1 do
    begin
      ProcessAffinity := Mask shl I;
      if not SetProcessAffinityMask(CurrProc, ProcessAffinity) then
        Break;

      FSupportCpuIds.Add(Pointer(GetCnCpuIdSupport));
      if FSupportCpuIds[FSupportCpuIds.Count - 1] <> nil then
      begin
        FCPUIds.Add(GetCnCPUID);
        FCPUInfos.Add(GetCnCPUInfoString);
        FCPUOems.Add(GetCnCPUOem);
      end
      else
      begin
        FCPUIds.Add('');
        FCPUInfos.Add('');
        FCPUOems.Add('');      	
      end;
    end;
  finally
    //�ָ�Ĭ��
    SetProcessAffinityMask(CurrProc, ProcessAffinityOld);
  end;
end;

procedure TCnCpuId.SetCPUIdFormat(ACPUIdFormat: TCnCPUIdFormat);
begin
  if FCPUIdFormat <> ACPUIdFormat then
  begin
    FCPUIdFormat := ACPUIdFormat;
    ReadCPUId;
  end;
end;

// ��õ� CPU ϵͳ ID
function TCnCpuId.GetFirstCPUId: string;
begin
  if FCPUIds.Count > 0 then
    Result := FCPUIds.Strings[0];
end;

// �õ��ڼ��� CPU �����к�
function TCnCpuId.GetCPUId(Index: Integer): string;
begin
  Result := '';
  // ��֤ FCPUIds �����ĺϷ���
  if (Index < 0) or (Index > FCPUIds.Count - 1) then
    Exit;

  Result := FCPUIds.Strings[Index];
end;

function TCnCpuId.GetAverageCPUUsage: Integer;
begin
  if not FCPUUsageRead then
    Result := -1
  else
    Result := FAverageCPUUsage;
end;

function TCnCpuId.GetCPUUsage(Index: Integer): Integer;
begin
  if not FCPUUsageRead or (Index > FCPUCount - 1) then
    Result := -1
  else
    Result := FCPUUsage[Index];
end;

function TCnCpuId.GetFirstCPUUsage: Integer;
begin
  if FCPUUsageRead and (FCPUCount > 0) then
    Result := FCPUUsage[0]
  else
    Result := -1;
end;

function TCnCpuId.RefreshCPUUsages: Cardinal;
var
  CpuCnt: Cardinal;
  I: integer;
  Spt: Pointer;
  Sbi: TSystemBasicInformation;

  dwType, cbData: Cardinal;
  hOpen: HKEY;
  Buffer: Cardinal;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    for I := 0 to FCPUCount - 1 do // �����ֵ
      FLastCnt[I] := FCurCnt[I];

    if NtQuerySystemInformation(SystemBasicInformation, @Sbi, SizeOf(Sbi), nil)
      <> STATUS_SUCCESS then
      CpuCnt := 1
    else
      CpuCnt := Sbi.NumberProcessors;

    Spt := AllocMem(CpuCnt * (SizeOf(TSystemProcessorTimes) + 4));
    NtQuerySystemInformation(SystemProcessorTimes, Spt, CpuCnt * (SizeOf(TSystemProcessorTimes) + 4), @Result);

    for I := 0 to CpuCnt - 1  do
    begin
      with PSystemProcessorTimes(PChar(Spt) + I * (sizeof(TSystemProcessorTimes) + 4))^ do
        FCurCnt[I] := IdleTime.QuadPart;
        
      // ���������
      try
        FCPUUsage[I] := Round((10000000 - (FCurCnt[I] - FLastCnt[I]) / (FTimer.Interval / 1000)) / 10000000 * 100);
      except
        FCPUUsage[I] := 0;
      end;
    end;
    FreeMem(spt);
  end
  else
  begin
    if RegOpenKeyEx(HKEY_DYN_DATA,'PerfStats\StatData', 0, KEY_READ, hOpen) = ERROR_SUCCESS then
    begin
      cbData:=sizeof(Cardinal);
      if RegQueryValueEx(hOpen, 'KERNEL\CPUUsage', nil, @dwType, PBYTE(@Buffer),
        @cbData) = ERROR_SUCCESS then
        FCPUUsage[0] := Buffer;
      RegCloseKey(hOpen);
    end
    else
      FCPUUsage[0] := -1;
  end;

  FCPUUsageRead := True;
end;

procedure TCnCpuId.CpuUsageTimer(Sender: TObject);
var
  I: Integer;
begin
  RefreshCPUUsages;
  
  FAverageCPUUsage := 0;
  for I := 0 to FCPUCount - 1 do
  begin
    if FCPUUsage[I] <> -1 then
      FAverageCPUUsage := FAverageCPUUsage + FCPUUsage[I];
  end;

  if FCPUCount > 0 then
    FAverageCPUUsage := Round(FAverageCPUUsage / FCPUCount)
  else
    FAverageCPUUsage := -1;
end;

function TCnCpuId.GetCPUOem(Index: Integer): string;
begin
  Result := '';
  // ��֤ FCPUIds �����ĺϷ���
  if (Index < 0) or (Index > FCPUOems.Count - 1) then
    Exit;

  Result := FCPUOems.Strings[Index];
end;

function TCnCpuId.GetFirstCPUOem: string;
begin
  if FCPUOems.Count > 0 then
    Result := FCPUOems.Strings[0];
end;

function TCnCpuId.GetSupportCPUId(Index: Integer): Boolean;
begin
  Result := False;
  // ��֤ FSupportCpuIds �����ĺϷ���
  if (Index < 0) or (Index > FSupportCpuIds.Count - 1) then
    Exit;

  Result := Boolean(FSupportCpuIds[Index]);
end;

function TCnCpuId.GetSupportCPUSn(Index: Integer): Boolean;
begin
  Result := False;
  // ��֤ FSupportCpuIds �����ĺϷ���
  if (Index < 0) or (Index > FSupportCpuSns.Count - 1) then
    Exit;

  Result := Boolean(FSupportCpuSns[Index]);
end;

function TCnCpuId.GetFirstSupportCPUId: Boolean;
begin
  Result := False;
  if FSupportCpuIds.Count > 0 then
    Result := Boolean(FSupportCpuIds[0]);
end;

function TCnCpuId.GetFirstSupportCPUSn: Boolean;
begin
  Result := False;
  if FSupportCpuSns.Count > 0 then
    Result := Boolean(FSupportCpuSns[0]);
end;

function TCnCpuId.GetCPUInfoString(Index: Integer): string;
begin
  Result := '';
  if (Index < 0) or (Index > FCPUInfos.Count - 1) then
    Exit;

  Result := FCPUInfos.Strings[Index];
end;

function TCnCpuId.GetFirstCPUInfoString: string;
begin
  if FCPUInfos.Count > 0 then
    Result := FCPUInfos[0];
end;

function FindAwardBios(var BiosAddr: PByte): UINT;
var
  ABiosAddr: PByte;
  szBiosData: array [0..127] of Char;
  Len: Integer;
  Loop: Byte;
begin
  Result:= 0;
  ABiosAddr:= PByte(DWORD(BiosAddr) + $EC71);

  CopyMemory(@szBiosData[0], ABiosAddr, 127);
  szBiosData[127]:= #0;

  Len:= StrLen(PChar(@szBiosData[0]));
  if (Len <= 0) or (Len >= 128) then
    Exit;

  //AWard:         07/08/2002-i845G-ITE8712-JF69VD0CC-00
  //Phoenix-Award: 03/12/2002-sis645-p4s333
  if (szBiosData[2] <> '/') or (szBiosData[5] <> '/') then
    Exit;

  Loop:= 0;
  while szBiosData[Loop] <> #0 do
  begin
    if (szBiosData[Loop] < ' ') or (szBiosData[Loop] >= Chr(127)) then
      Break;
    Inc(Loop);
  end;

  if szBiosData[Loop] = #0 then
  begin
    BiosAddr:= ABiosAddr;
    Result:= Len;
  end;
end;

function FindAmiBios(var BiosAddr: PByte): UINT;
var
  ABiosAddr: PByte;
  szBiosData: array [0..127] of Char;
  Len: Integer;
  Loop: Byte;
begin
  Result:= 0;
  ABiosAddr:= PByte(DWORD(BiosAddr) + $F478);

  CopyMemory(@szBiosData[0], ABiosAddr, 127);
  szBiosData[127]:= #0;

  Len:= StrLen(PChar(@szBiosData[0]));
  if (Len <= 0) or (Len >= 128) then
    Exit;

  // Example: "AMI: 51-2300-000000-00101111-030199-"
  if (szBiosData[2] <> '-') or (szBiosData[7] <> '-') then
    Exit;

  Loop:= 0;
  while szBiosData[Loop] <> #0 do
  begin
    if (szBiosData[Loop] < ' ') or (szBiosData[Loop] >= Chr(127)) then
      Break;
    Inc(Loop);
  end;

  if szBiosData[Loop] = #0 then
  begin
    BiosAddr:= ABiosAddr;
    Result:= Len;
  end;
end;

function FindPhoenixBios(var BiosAddr: PByte): UINT;
var
  ABiosAddr: PByte;
  szBiosData: array [0..127] of Char;
  Len: Integer;
  I, Loop: Byte;
begin
  for I := 0 to 2 do
  begin
    ABiosAddr:= PByte(DWORD(BiosAddr) + BiosOffset[I]);
    CopyMemory(@szBiosData[0], ABiosAddr, 127);
    szBiosData[127]:= #0;
    Len:= StrLen(PChar(@szBiosData[0]));
    if (Len <= 0) or (Len >= 128) then
      Continue;

    // Example: Phoenix "NITELT0.86B.0044.P11.9910111055"
    if (szBiosData[7] <> '.') or (szBiosData[11] <> '.') then
      Continue;

    Loop:= 0;
    while szBiosData[Loop] <> #0 do
    begin
      if (szBiosData[Loop] < ' ') or (szBiosData[Loop] >= Chr(127)) then
        Break;
      Inc(Loop);
    end;

    if szBiosData[Loop] = #0 then
    begin
      BiosAddr:= ABiosAddr;
      Result:= Len;
      Exit;
    end;
  end;
  Result:= 0;
end;

function CnGetBiosID: string;
var
  Size: DWORD;
  RealAddr: LARGE_INTEGER;
  Path: PWCHAR;
  BaseAddr: DWORD;
  UniString: TUNICODE_STRING;
  Obj: TOBJECT_ATTRIBUTES;
  hSection: THandle;
  PBiosSerial: PByte;
  uBiosSerialLen: UINT;
  szSystemInfo: array [0..4095] of Char;
  ReturnLen: UINT;
begin
  FillChar(szSystemInfo, 4096, 0);
  ReturnLen:= 0;

  RealAddr.LowPart:= $000F0000;
  RealAddr.HighPart:= $00000000;
  Size:= $FFFF;
  Path:= '\device\physicalmemory';
  BaseAddr:= 0;
  UniString.Buffer:= Path;
  UniString.Length:= $2C;
  UniString.MaximumLength:= $2E;

  Obj.Attributes:= 64;
  Obj.Length:= 24;
  Obj.ObjectName:= @UniString;
  Obj.RootDirectory:= 0;
  Obj.SecurityDescriptor:= nil;
  Obj.SecurityQualityOfService:= nil;

  //���ú������������ڴ����ӳ��
  if (ZwOpenSection(hSection, 4, @Obj) = 0) and
     (ZwMapViewOfSection(hSection, $FFFFFFFF, @BaseAddr, 0, $FFFF, @RealAddr, @Size, 1, 0, 2) = 0) then
  begin
    //ִ�к���ڵ�ǰ���̵Ŀռ俪��һ��64k�Ŀռ䣬����f000:0000��f000:ffff��������ӳ�䵽����
    //ӳ��Ļ�ַ��BaseAddr����,���ӳ�䲻������,Ӧ����ZwUnmapViewOfSection�Ͽ�ӳ��
    PBiosSerial:= PByte(BaseAddr);
    uBiosSerialLen:= FindAwardBios(PBiosSerial);
    if uBiosSerialLen = 0 then
    begin
      uBiosSerialLen:= FindAmiBios(PBiosSerial);
      if uBiosSerialLen = 0 then
        uBiosSerialLen:= FindPhoenixBios(PBiosSerial);
    end;

    if uBiosSerialLen <> 0 then
    begin
      CopyMemory(Pointer(szSystemInfo + ReturnLen), PBiosSerial, uBiosSerialLen);
      Inc(ReturnLen, uBiosSerialLen);
    end;
    ZwUnmapViewOfSection($FFFFFFFF, Pointer(BaseAddr));
  end;

  if ReturnLen <> 0 then
  begin
    SetLength(Result, ReturnLen);
    MoveMemory(@Result[1], @szSystemInfo[0], ReturnLen);
  end;
end;

initialization
  GetNtNativeAPIs;

finalization
  FreeNtNativeAPIs;

end.
