{*******************************************************}
{       MiTeC System Information Component Suite        }
{              Process List Monitor Thread              }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}


{$INCLUDE Compilers.inc}


unit MSI_ProcListMon;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, System.SyncObjs,
     {$ELSE}
     Windows, SysUtils, Classes, SyncObjs,
     {$ENDIF}
     MiTeC_Windows, MSI_Defs, MiTeC_NativeDefs, MiTeC_NativeAPI, MiTeC_Routines, MiTeC_PSAPI;

type
  TProcessPerformanceRecord = record
    CPUUsage,
    IOReadUsage,
    IOWriteUsage,
    IOOtherUsage: double;
    CPUTime,
    Cycles: UInt64;
    _Period: UInt64;
    _IORead,
    _IOWrite,
    _IOOther: Int64;
  end;

  TProcessRecord = record
    PID,
    SessionID :Cardinal;
    Handle: THandle;
    Name,
    ImageName,
    DomainName,
    UserName,
    CommandLine,
    Environment,
    ParentImage,
    ParentCmd: string;
    Priority,
    BasePriority,
    ParentPID,
    HandleCount,
    GDIHandleCount,
    USERHandleCount,
    ThreadCount: Cardinal;
    Affinity: NativeUInt;
    //PMCounters: TProcessMemoryCountersEx;
    VMCounters: TVMCounters;
    IOCounters: TIOCounters;
    CPUTimes: TProcessTimes;
    Privileges: TPrivilegeList;
    Groups: TTokenGroupList;
    Bits: Cardinal;
    CreationTime: TDateTime;
    VersionInfo: TVersionInfo;
    Performance: TProcessPerformanceRecord;
    IsSuspended: Boolean;
    System: Boolean;
    Elevation: Cardinal;
    New: Boolean;
    SHA1: string;
    _Exists: Boolean;
    _ImageIndex: integer;
  end;
  PProcessRecord = ^TProcessRecord;

  TProcListMonThread = class;

  TProcListMonNotifyEvent = procedure(Sender: TProcListMonThread) of object;

  TProcListMonThread = class(TThread)
  private
    InternalLock: TCriticalSection;
    FOnInterval: TProcListMonNotifyEvent;
    FInterval: Cardinal;
    FList: TList;
    FCPUCount: Byte;
    FAutoSuspend: Boolean;
    FMaxCPUPID,
    FMaxMemPID,
    FMaxIOPID: Cardinal;
    FII: integer;
    Buffer: Pointer;
    BufferSize: Cardinal;
    procedure DoSync;
    function GetInterval: Cardinal;
    function GetOnInterval: TProcListMonNotifyEvent;
    procedure SetInterval(const Value: Cardinal);
    procedure SetOnInterval(const Value: TProcListMonNotifyEvent);
    function GetRecCount: Integer;
    function GetMaxCPUPID: Cardinal;
    function GetAutoSuspend: Boolean;
    procedure SetAutoSuspend(const Value: Boolean);
    function GetMaxMemPID: Cardinal;
    procedure GetParentProps(APID: Cardinal; var AImageName, ACmdline: string);
    function GetMaxIOPID: Cardinal;
  protected
    procedure RefreshData;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetRecord(AIndex: Integer; var ARecord: TProcessRecord);
    procedure GetRecordByPID(APID: Cardinal; var ARecord: TProcessRecord);
    procedure GetChildProcesses(APID: Cardinal; AList: TStringList);
    procedure GetList(AList: TList);
    procedure Clear;

    property MaxCPUUsageProcessID: Cardinal read GetMaxCPUPID;
    property MaxMemUsageProcessID: Cardinal read GetMaxMemPID;
    property MaxIOUsageProcessID: Cardinal read GetMaxIOPID;
    property AutoSuspend: Boolean read GetAutoSuspend write SetAutoSuspend;
    property RecordCount: Integer read GetRecCount;
    property Interval: Cardinal read GetInterval write SetInterval;
    property OnInterval: TProcListMonNotifyEvent read GetOnInterval write SetOnInterval;
  end;

implementation

uses {$IFDEF RAD9PLUS}
     {$IFDEF TRIAL}VCL.Dialogs,{$ENDIF}WinApi.ShellAPI, WinAPI.TlHelp32,
     {$ELSE}
     {$IFDEF TRIAL}Dialogs,{$ENDIF}ShellAPI, {$IFDEF FPC}JwaTlHelp32{$ELSE}TlHelp32{$ENDIF},
     {$ENDIF}
     MiTeC_Datetime, MiTeC_StrUtils, MiTeC_WinCrypt;

const
  rsSystemIdle = '[System Idle Process]';
  rsSystem = 'System';

{ TProcListMonThread }

procedure TProcListMonThread.Clear;
var
  i: integer;
begin
  {$IFDEF TRIAL}
  if not RunFromIDE then
    MessageDlg('TProcListMonThread'+sLineBreak+cCompName+sLineBreak+cCopyright,mtInformation,[mbOK],0);
  {$ENDIF}
  InternalLock.Enter;
  try
    for i:=0 to FList.Count-1 do begin
      CloseHandle(PProcessRecord(FList[i])^.Handle);
      Dispose(PProcessRecord(FList[i]));
    end;
    FList.Clear;
  finally
    InternalLock.Leave;
  end;
end;

constructor TProcListMonThread.Create;
var
  ShInfo: TSHFileInfo;
begin
  inherited Create(True);
  InternalLock:=TCriticalSection.Create;
  FCPUCount:=NativeGetCPUCount;
  FList:=TList.Create;
  FAutoSuspend:=False;
  FMaxCPUPID:=0;
  FMaxMemPID:=0;
  FMaxIOPID:=0;
  FreeOnTerminate:=False;
  FInterval:=1000;
  FII:=0;
  if SHGetFileInfo(PChar(IncludeTrailingPathDelimiter(GetSysDir)+'svchost.exe'),0,ShInfo,SizeOf(ShInfo),SHGFI_SYSICONINDEX)>0 then
    FII:=shInfo.iIcon;
end;

destructor TProcListMonThread.Destroy;
begin
  FOnInterval:=nil;
  if not Terminated then
    Terminate;
  while not Terminated do
    Sleep(100);
  Clear;
  FList.Free;
  FreeAndNil(InternalLock);
  inherited;
end;

procedure TProcListMonThread.DoSync;
begin
  if Assigned(FOnInterval) then
    FOnInterval(Self);
end;

procedure TProcListMonThread.Execute;
var
  se: TSimpleEvent;
begin
  se:=TSimpleEvent.Create{$IFDEF BDS35PLUS}(nil,False,False,''){$ENDIF};
  BufferSize:=SizeOf(TSystemProcessInformation);
  Buffer:=AllocMem(BufferSize);
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
    Freemem(Buffer);
    se.Free;
  end;
end;

function TProcListMonThread.GetAutoSuspend: Boolean;
begin
  InternalLock.Enter;
  try
    Result:=FAutoSuspend;
  finally
    InternalLock.Leave;
  end;
end;

procedure TProcListMonThread.GetChildProcesses(APID: Cardinal;
  AList: TStringList);
var
  i: Integer;
begin
  InternalLock.Enter;
  try
    AList.Clear;
    for i:=0 to FList.Count-1 do begin
      if PProcessRecord(FList[i])^.ParentPID=APID then
        AList.Add(Format('%d=%s',[PProcessRecord(FList[i])^.PID,PProcessRecord(FList[i])^.ImageName]))
      else if AList.IndexOfName(IntToStr(PProcessRecord(FList[i])^.ParentPID))>-1 then
        AList.Add(Format('%d=%s',[PProcessRecord(FList[i])^.PID,PProcessRecord(FList[i])^.ImageName]));
    end;
  finally
    InternalLock.Leave;
  end;
end;

function TProcListMonThread.GetInterval: Cardinal;
begin
  InternalLock.Enter;
  try
    Result:=FInterval;
  finally
    InternalLock.Leave;
  end;
end;

procedure TProcListMonThread.GetList(AList: TList);
var
  i: Integer;
  p: PProcessRecord;
begin
  InternalLock.Enter;
  try
    for i:=0 to AList.Count-1 do
      Dispose(PProcessRecord(AList[i]));
    AList.Clear;
    AList.Capacity:=FList.Capacity;
    for i:=0 to FList.Count-1 do begin
      new(p);
      p^:=PProcessRecord(FList[i])^;
      AList.Add(p);
    end;
  finally
    InternalLock.Leave;
  end;
end;

function TProcListMonThread.GetMaxCPUPID: Cardinal;
begin
  InternalLock.Enter;
  try
    Result:=FMaxCPUPID;
  finally
    InternalLock.Leave;
  end;
end;

function TProcListMonThread.GetMaxIOPID: Cardinal;
begin
  InternalLock.Enter;
  try
    Result:=FMaxIOPID;
  finally
    InternalLock.Leave;
  end;
end;

function TProcListMonThread.GetMaxMemPID: Cardinal;
begin
  InternalLock.Enter;
  try
    Result:=FMaxMemPID;
  finally
    InternalLock.Leave;
  end;
end;

procedure TProcListMonThread.GetParentProps(APID: Cardinal; var AImageName, ACmdline: string);
var
  i: Integer;
begin
  AImageName:='';
  ACmdLine:='';
  for i:=0 to FList.Count-1 do
    if PProcessRecord(FList[i]).PID=APID then begin
      ACmdLine:=PProcessRecord(FList[i]).CommandLine;
      AImageName:=PProcessRecord(FList[i]).ImageName;
      Break;
    end;
end;

function TProcListMonThread.GetOnInterval: TProcListMonNotifyEvent;
begin
  InternalLock.Enter;
  try
    Result:=FOnInterval;
  finally
    InternalLock.Leave;
  end;
end;

function TProcListMonThread.GetRecCount: Integer;
begin
  InternalLock.Enter;
  try
    Result:=FList.Count;
  finally
    InternalLock.Leave;
  end;
end;

procedure TProcListMonThread.GetRecord(AIndex: Integer;
  var ARecord: TProcessRecord);
begin
  InternalLock.Enter;
  try
    ResetMemory(ARecord,SizeOf(ARecord));
    if AIndex<FList.Count then
      ARecord:=PProcessRecord(FList[AIndex])^;
  finally
    InternalLock.Leave;
  end;
end;

procedure TProcListMonThread.GetRecordByPID(APID: Cardinal;
  var ARecord: TProcessRecord);
var
  i: Integer;
begin
  InternalLock.Enter;
  try
    ResetMemory(ARecord,SizeOf(ARecord));
    for i:=0 to FList.Count-1 do
      if PProcessRecord(FList[i])^.PID=APID then begin
        ARecord:=PProcessRecord(FList[i])^;
        Break;
      end;
  finally
    InternalLock.Leave;
  end;
end;

procedure TProcListMonThread.RefreshData;
var
  status: Cardinal;
  pspi: PSystemProcessInformation;
  i,idx: Integer;
  br :Cardinal;
  n: {$IFDEF NATIVEINT}NativeUint{$ELSE}Cardinal{$ENDIF};
  r: PProcessRecord;
  Wow64: LongBool;
  Buf: array[0..MAX_PATH] of char;
  pbi: PROCESS_BASIC_INFORMATION;
  pb: TPEB;
  ib: TProcessParameters;
  pwc,p: PWideChar;
  mc,mm,mio,io: double;
  s: string;
  ShInfo: TSHFileInfo;
  ok: bool;
  c,ct: UInt64;
  cpu: double;
  mbi: TMemoryBasicInformation;
  envbuf: TBytes;
  prd: UInt64;
  sa: NativeUInt;
begin
  InternalLock.Enter;
  try
    FMAXCPUPID:=0;
    FMAXMemPID:=0;
    mc:=0;
    mm:=0;
    mio:=0;
    for i:=0 to FList.Count-1 do
      PProcessRecord(FList[i])._Exists:=False;
    status:=NtQuerySystemInformation(SystemProcessInformation,Buffer,BufferSize,@br);
    while (status=STATUS_BUFFER_OVERFLOW) or (status=STATUS_INFO_LENGTH_MISMATCH) do begin
      BufferSize:=br;
      ReallocMem(Buffer,BufferSize);
      status:=NtQuerySystemInformation(SystemProcessInformation,Buffer,BufferSize,@br);
    end;
    if status=STATUS_SUCCESS then begin
      pspi:=PSystemProcessInformation(Buffer);
      repeat
        idx:=-1;
        for i:=0 to FList.Count-1 do
          if (PProcessRecord(FList[i]).PID=pspi.ProcessId) then begin
            idx:=i;
            Break;
          end;
        if idx=-1 then begin
          new(r);
          ZeroMemory(r,SizeOf(TProcessRecord));
          r.Handle:=GetProcessHandle(pspi.ProcessId);
          r.New:=True;
          r._ImageIndex:=FII;
          r.Performance._Period:=GetTickCount64-10;
          r.Performance._IORead:=pspi.IOCounters.ReadTransferCount.QuadPart;
          r.Performance._IOWrite:=pspi.IOCounters.WriteTransferCount.QuadPart;
          r.Performance._IOOther:=pspi.IOCounters.OtherTransferCount.QuadPart;
          FList.Add(r);
          idx:=FList.Count-1;
        end else
          PProcessRecord(FList[idx])^.New:=False;
        with PProcessRecord(FList[idx])^ do begin
          _Exists:=True;
          PID:=pspi.ProcessId;
          SessionID:=pspi.SessionId;
          BasePriority:=pspi.BasePriority;
          ParentPID:=pspi.InheritedFromProcessId;
          if ParentImage='' then
            GetParentProps(ParentPID,ParentImage,ParentCmd);
          ThreadCount:=pspi.ThreadCount;
          HandleCount:=pspi.HandleCount;
          CPUTimes.CreateTime:=pspi.CreateTime;
          CPUTimes.UserTime:=pspi.UserTime;
          CPUTimes.KernelTime:=pspi.KernelTime;
          CreationTime:=FileTimeToDateTime(TFileTime(pspi.CreateTime),True);
          VMCounters:=pspi.VmCounters;
          IOCounters:=pspi.IoCounters;

          GDIHandleCount:=GetGuiResources(Handle,GR_GDIOBJECTS);
          USERHandleCount:=GetGuiResources(Handle,GR_USEROBJECTS);
          //PMCounters:=GetProcessMemoryCounters(Handle);

          prd:=GetTickCount64-Performance._Period;
          ok:=false;
          ct:=pspi.KernelTime.QuadPart+pspi.UserTime.QuadPart;
          if not IsServer and (Handle<>INVALID_HANDLE_VALUE) and (Handle<>0) and Assigned(QueryProcessCycleTime) then
            ok:=QueryProcessCycleTime(Handle,@c);

          if ok then
            cpu:=(c-Performance.Cycles)/(prd*100000)
          else
            cpu:=(((ct-Performance.CPUTime)/prd)/100)/FCPUCount;
          if (Performance.Cycles=0) and (Performance.CPUTime=0) then
            cpu:=0
          else if (cpu>100) or (cpu<0) then
            cpu:=Performance.CPUUsage;

          Performance._Period:=GetTickCount64;
          Performance.CPUUsage:=cpu;
          Performance.CPUTime:=ct;

          Performance.IOReadUsage:=pspi.IOCounters.ReadTransferCount.QuadPart-Performance._IORead;
          Performance.IOWriteUsage:=pspi.IOCounters.WriteTransferCount.QuadPart-Performance._IOWrite;
          Performance.IOOtherUsage:=pspi.IOCounters.OtherTransferCount.QuadPart-Performance._IOOther;
          Performance.Cycles:=c;
          Performance._IORead:=pspi.IOCounters.ReadTransferCount.QuadPart;
          Performance._IOWrite:=pspi.IOCounters.WriteTransferCount.QuadPart;
          Performance._IOOther:=pspi.IOCounters.OtherTransferCount.QuadPart;

          io:=Performance.IOReadUsage+Performance.IOWriteUsage+Performance.IOOtherUsage;
          if pspi.ProcessId>0 then begin
            if Performance.CPUUsage>mc then begin
              mc:=Performance.CPUUsage;
              FMaxCPUPID:=pspi.ProcessId;
            end;
            if pspi.VmCounters.PrivatePageCount>mm then begin
              mm:=pspi.VmCounters.PrivatePageCount;
              FMaxMemPID:=pspi.ProcessId;
            end;
            if io>mio then begin
              mio:=io;
              FMaxIOPID:=pspi.ProcessId;
            end;
          end;

          IsSuspended:=True;
          for i:=0 to pspi.ThreadCount-1 do
            if (pspi.Threads[i].State<>5{Cardinal(StateWait)}) or (pspi.Threads[i].WaitReason<>5{Cardinal(Suspended)}) then begin
              IsSuspended:=False;
              Break;
            end;

          if PID=0 then
            s:=rsSystemIdle
          else if PID=4 then begin
            s:=rsSystem;
            ImageName:=ExpandFilename(FileSearch('ntoskrnl.exe',GetWinSysDir));
          end else
            s:={$IFDEF UNICODE}string{$ELSE}WideToAnsi{$ENDIF}(pspi.ProcessName.Buffer);

          if (Name='') or not SameText(Name,s) then begin

            Name:=s;

            if (Handle<>INVALID_HANDLE_VALUE) and (Handle<>0) then
              try
                Priority:=GetPriorityClass(Handle);
                GetProcessAffinityMask(Handle,Affinity,sa);
                if (Bits=0) then begin
                  if Is64 then begin
                    Wow64:=False;
                    if Assigned(IsWow64Process) and IsWow64Process(Handle,Wow64) then begin
                      if not Wow64 then
                        Bits:=64
                      else
                        Bits:=32;
                    end;
                  end else
                    Bits:=32;
                end;

                {ZeroMemory(@Buf,SizeOf(Buf));
                if GetModuleFileNameEx(Handle,0,@Buf,SizeOf(Buf))>0 then begin
                  ImageName:=Buf;
                  SetLength(ImageName,StrLen(PChar(ImageName)));
                  ImageName:=StringReplace(ImageName,'\??\','',[]);
                end;}

                if ImageName='' then begin
                  n:=SizeOf(Buf);
                  if Assigned(QueryFullProcessImageName) then
                    if QueryFullProcessImageName(Handle,0,@Buf,@n)>0 then
                      ImageName:=Buf;
                end;

                if ImageName='' then begin
                  ResetMemory(Buf,SizeOf(Buf));
                  n:=0;
                  if NtQueryInformationProcess(Handle,ProcessImageFileName,@Buf,SizeOf(Buf),@n)=0 then begin
                    ImageName:=Trim(Copy(string(Buf),5,n));
                    if ImageName<>'' then
                      for i:=0 to High(DosDevices) do
                        if Pos(DosDevices[i].DiskDosQuery,ImageName)=1 then begin
                          ImageName:=StringReplace(ImageName,DosDevices[i].DiskDosQuery,DosDevices[i].DiskLabel,[rfIgnoreCase]);
                          Break;
                        end;
                  end;
                end;
                if ImageName='' then
                  ImageName:=ExpandFilename(FileSearch(Name,GetWinSysDir+ExtractFilePath(ImageName)));
                if (ImageName<>'') and not FileExists(ImageName) then
                  ImageName:=ExpandEnvVars(ImageName);

                n:=0;

                if NtQueryInformationProcess(Handle,ProcessBasicInformation,@pbi,SizeOf(pbi),@n)=0 then begin
                  if ReadProcessMemory(Handle,pbi.PebBaseAddress,@pb,SizeOf(pb),n) then
                    if ReadProcessMemory(Handle,Pointer(pb.ProcessParameters),@ib,SizeOf(ib),n) then begin
                      if VirtualQueryEx(Handle,ib.Environment,mbi,SizeOf(mbi))=0 then
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
                        if ReadProcessMemory(Handle,Pointer(ib.Environment),@envbuf[0],br,n) and (n>0) then
                          Break;
                        br:=br div 2;
                      until (br<256);
                      if (n>0) then begin
                        p:=PWideChar(@envbuf[0]);
                        while p^<> #0 do begin
                          if p[0]<>'=' then
                            Environment:=Environment+{$IFNDEF UNICODE}WideCharToString{$ENDIF}(p)+sLineBreak;
                          Inc(p,Length(WideString(p))+1);
                        end;
                        Environment:=Trim(Environment);
                      end;

                      pwc:=AllocMem(ib.CommandLine.MaximumLength);
                      try
                        if ReadProcessMemory(Handle,Pointer(ib.CommandLine.Buffer),pwc,ib.CommandLine.MaximumLength,n) then
                          CommandLine:=WideCharToString(pwc);
                        if ImageName='' then begin
                          if ib.CommandLine.MaximumLength<ib.ImagePathName.MaximumLength then
                            ReallocMem(pwc,ib.ImagePathName.MaximumLength);
                          if ReadProcessMemory(Handle,Pointer(ib.ImagePathName.Buffer),pwc,ib.ImagePathName.MaximumLength,n) then
                            ImageName:=WideCharToString(pwc);
                        end;
                      finally
                        FreeMem(pwc);
                      end;
                    end;
                end;

                if CommandLine='' then
                  CommandLine:=ImageName;

                GetProcessUserName(Handle,Username,DomainName);
                GetProcessPrivileges(Handle,Privileges,Elevation);
                GetProcessGroups(Handle,Groups);
                if (PID>4) and (SHGetFileInfo(PChar(ImageName),0,ShInfo,SizeOf(ShInfo),SHGFI_SYSICONINDEX)>0) and not IsSuspended then
                  _ImageIndex:=shInfo.iIcon;

              finally

              end;

            if (Bits=0) and not IsSuspended then begin
              if Is64 then
                Bits:=64
              else
                Bits:=32;
            end;

            GetFileVerInfo(ImageName,VersionInfo);
            SHA1:=BytesToHEX(CreateFileHash(ImageName));

            System:=SameText(DomainName,'NT AUTHORITY') or
                      //SameText(DomainName,'Window Manager') or
                      SameText(Username,'SYSTEM') or
                      SameText(Username,'LOCAL SERVICE') or
                      SameText(Username,'NETWORK SERVICE') or
                      ((Username='') and (SessionId<2)) or
                      (PID<5);
          end;
        end;
        if pspi^.NextEntryDelta=0 then
          Break;
        pspi:=PSystemProcessInformation(PAnsiChar(pspi)+pspi^.NextEntryDelta);
      until False;
    end;

    i:=0;
    while i<FList.Count do
      if not PProcessRecord(FList[i])._Exists then begin
        CloseHandle(PProcessRecord(FList[i]).Handle);
        Dispose(PProcessRecord(FList[i]));
        FList.Delete(i);
      end else
        Inc(i);
    FList.Capacity:=FList.Count;
  finally
    InternalLock.Leave;
  end;
end;

procedure TProcListMonThread.SetAutoSuspend(const Value: Boolean);
begin
  InternalLock.Enter;
  try
    FAutoSuspend:=Value;
  finally
    InternalLock.Leave;
  end;
end;

procedure TProcListMonThread.SetInterval(const Value: Cardinal);
begin
  InternalLock.Enter;
  try
    FInterval:=Value;
  finally
    InternalLock.Leave;
  end;
end;

procedure TProcListMonThread.SetOnInterval(const Value: TProcListMonNotifyEvent);
begin
  InternalLock.Enter;
  try
    FOnInterval:=Value;
  finally
    InternalLock.Leave;
  end;
end;

initialization
  InitNativeAPI;
end.
