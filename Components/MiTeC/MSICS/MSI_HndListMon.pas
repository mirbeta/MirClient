{*******************************************************}
{        System Information Component Suite             }
{              Handle List Monitor Thread               }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}


{$INCLUDE Compilers.inc}


unit MSI_HndListMon;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, System.SyncObjs,
     {$ELSE}
     Windows, SysUtils, Classes, SyncObjs,
     {$ENDIF}
     MiTeC_Windows, MSI_Defs, MiTeC_NativeDefs, MiTeC_NativeAPI, MiTeC_Routines;

type
  THndListMonThread = class;

  THndListMonNotifyEvent = procedure(Sender: THndListMonThread) of object;

  THndListMonThread = class(TThread)
  private
    InternalLock: TCriticalSection;
    FOnCompleted: THndListMonNotifyEvent;
    FCompleted: Cardinal;
    FList: TList;
    FAutoSuspend: Boolean;
    FTypes: TSystemHandleTypes;
    procedure DoSync;
    function GetCompleted: Cardinal;
    function GetOnCompleted: THndListMonNotifyEvent;
    procedure SetCompleted(const Value: Cardinal);
    procedure SetOnCompleted(const Value: THndListMonNotifyEvent);
    function GetRecCount: Integer;
    function GetAutoSuspend: Boolean;
    procedure SetAutoSuspend(const Value: Boolean);
    function GetTypes: TSystemHandleTypes;
    procedure SetTypes(const Value: TSystemHandleTypes);
  protected
    procedure RefreshData;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetRecord(AIndex: Integer; var ARecord: THandleRecord);
    procedure GetRecordByHandle(AHandle: Word; var ARecord: THandleRecord);
    procedure GetList(AList: TList);
    procedure Clear;

    property Types: TSystemHandleTypes read GetTypes write SetTypes;
    property AutoSuspend: Boolean read GetAutoSuspend write SetAutoSuspend;
    property RecordCount: Integer read GetRecCount;
    property Completed: Cardinal read GetCompleted write SetCompleted;
    property OnCompleted: THndListMonNotifyEvent read GetOnCompleted write SetOnCompleted;
  end;

implementation

uses {$IFDEF RAD9PLUS}
     {$IFDEF TRIAL}VCL.Dialogs,{$ENDIF}System.Math, WinApi.ShellAPI, WinAPI.TlHelp32,
     {$ELSE}
     {$IFDEF TRIAL}Dialogs,{$ENDIF}Math, ShellAPI, {$IFDEF FPC}JwaTlHelp32{$ELSE}TlHelp32{$ENDIF},
     {$ENDIF}
     MiTeC_Datetime, MiTeC_StrUtils;

function GetProcessName(APID: Cardinal): string;
var
  ps: THandle;
  pe32: TProcessEntry32;
  ok: Boolean;
begin
  Result:='';
  if APID=0 then begin
    Result:='System Idle Process';
    Exit;
  end;
  if APID=4 then begin
    Result:='System';
    Exit;
  end;
  ps:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  if (ps<>INVALID_HANDLE_VALUE) then
    try
      pe32.dwSize:=sizeof(TPROCESSENTRY32);
      ok:=Process32First(ps,pe32);
      while ok do begin
        if pe32.th32ProcessID=APID then begin
          Result:=pe32.szExeFile;
          Break;
        end;
        ok:=Process32Next(ps,pe32);
      end;
    finally
      CloseHandle(ps);
    end;
end;

{ THndListMonThread }

procedure THndListMonThread.Clear;
var
  i: integer;
begin
  InternalLock.Enter;
  try
    for i:=0 to FList.Count-1 do
      Dispose(PHandleRecord(FList[i]));
    FList.Clear;
  finally
    InternalLock.Leave;
  end;
end;

constructor THndListMonThread.Create;
begin
  {$IFDEF TRIAL}
  if not RunFromIDE then
    MessageDlg(PChar('THndListMonThread'+sLineBreak+cCompName+sLineBreak+cCopyright),mtInformation,[mbOK],0);
  {$ENDIF}
  inherited Create(True);
  InternalLock:=TCriticalSection.Create;
  FList:=TList.Create;
  FAutoSuspend:=False;
  FreeOnTerminate:=False;
  FTypes:=[OB_TYPE_FILE];
  FCompleted:=1000;
end;

destructor THndListMonThread.Destroy;
begin
  FOnCompleted:=nil;
  Clear;
  FList.Free;
  FreeAndNil(InternalLock);
  inherited;
end;

procedure THndListMonThread.DoSync;
begin
  if Assigned(FOnCompleted) then
    FOnCompleted(Self);
end;

procedure THndListMonThread.Execute;
begin
  try
    while not Terminated do begin
      RefreshData;

      if Assigned(FOnCompleted) and not Terminated then
        Synchronize(DoSync);

      if not Terminated then begin
        if FAutoSuspend and not Suspended  then
          Suspended:=True
      end;
    end;
  except
  end;
end;

function THndListMonThread.GetAutoSuspend: Boolean;
begin
  InternalLock.Enter;
  try
    Result:=FAutoSuspend;
  finally
    InternalLock.Leave;
  end;
end;

function THndListMonThread.GetCompleted: Cardinal;
begin
  InternalLock.Enter;
  try
    Result:=FCompleted;
  finally
    InternalLock.Leave;
  end;
end;

procedure THndListMonThread.GetList(AList: TList);
var
  i: Integer;
  p: PHandleRecord;
begin
  InternalLock.Enter;
  try
    for i:=0 to AList.Count-1 do
      Dispose(PHandleRecord(AList[i]));
    AList.Clear;
    AList.Capacity:=FList.Capacity;
    for i:=0 to FList.Count-1 do begin
      new(p);
      p^:=PHandleRecord(FList[i])^;
      AList.Add(p);
    end;
  finally
    InternalLock.Leave;
  end;
end;

function THndListMonThread.GetOnCompleted: THndListMonNotifyEvent;
begin
  InternalLock.Enter;
  try
    Result:=FOnCompleted;
  finally
    InternalLock.Leave;
  end;
end;

function THndListMonThread.GetRecCount: Integer;
begin
  InternalLock.Enter;
  try
    Result:=FList.Count;
  finally
    InternalLock.Leave;
  end;
end;

procedure THndListMonThread.GetRecord(AIndex: Integer;
  var ARecord: THandleRecord);
begin
  InternalLock.Enter;
  try
    ResetMemory(ARecord,SizeOf(ARecord));
    if AIndex<FList.Count then
      ARecord:=PHandleRecord(FList[AIndex])^;
  finally
    InternalLock.Leave;
  end;
end;

procedure THndListMonThread.GetRecordByHandle(AHandle: Word;
  var ARecord: THandleRecord);
var
  i: Integer;
begin
  InternalLock.Enter;
  try
    ResetMemory(ARecord,SizeOf(ARecord));
    for i:=0 to FList.Count-1 do
      if PHandleRecord(FList[i])^.Handle=AHandle then begin
        ARecord:=PHandleRecord(FList[i])^;
        Break;
      end;
  finally
    InternalLock.Leave;
  end;
end;

function THndListMonThread.GetTypes: TSystemHandleTypes;
begin
  InternalLock.Enter;
  try
    Result:=FTypes;
  finally
    InternalLock.Leave;
  end;
end;

procedure THndListMonThread.RefreshData;
var
  sht: TSystemHandleType;
  Buffer, p: Pointer;
  pid,br,status,sz,c: Cardinal;
  pshi: PSystemHandleTableEntryInfo;
  h,oh,th: THandle;
  r: THandleRecord;
  pr: PHandleRecord;
  param: TGetHandleInfoThreadParam;
  {$IFNDEF UNICODE}buf: array[0..MAX_PATH-1] of AnsiChar;{$ENDIF}
  {$IFDEF FPC}tid: NativeUint;{$ENDIF}
begin
  pid:=0;
  h:=0;
  InternalLock.Enter;
  try

    Clear;

    Buffer:=nil;
    sz:=$100;
    Buffer:=AllocMem(sz*SizeOf(TSystemHandleInformation));
    try
      status:=NtQuerySystemInformation(SystemHandleInformation,Buffer,sz,@br);
      while (status=STATUS_BUFFER_OVERFLOW) or (status=STATUS_INFO_LENGTH_MISMATCH) do begin
        sz:=Max(br,sz*2);
        ReallocMem(Buffer,sz*SizeOf(TSystemHandleInformation));
        status:=NtQuerySystemInformation(SystemHandleInformation,Buffer,sz*SizeOf(TSystemHandleInformation),@br);
      end;
      if status=STATUS_SUCCESS then begin
        c:=0;
        pshi:=PSystemHandleTableEntryInfo(PAnsiChar(Buffer)+{$IFDEF WIN64}2*{$ENDIF}SizeOf(DWORD));
        p:=AllocMem(sz);
        try
          repeat
            if (pid<>pshi^.UniqueProcessId) then begin
              if (h<>0) and (h<>INVALID_HANDLE_VALUE) then
                CloseHandle(h);
              h:=GetProcessHandle(pshi^.UniqueProcessId,PROCESS_DUP_HANDLE);
              pid:=pshi^.UniqueProcessId;
            end;
            try
              if (h<>0) and (h<>INVALID_HANDLE_VALUE) then begin
                ResetMemory(r,SizeOf(r));
                r._Exists:=True;
                r.PID:=pshi^.UniqueProcessId;
                r.Handle:=pshi^.HandleValue;
                r.Access:=pshi^.GrantedAccess;
                r.Typ:=pshi^.ObjectTypeIndex;;
                status:=0;
                if pshi^.UniqueProcessId<>GetCurrentProcessID then begin
                  status:=NtDuplicateObject(h,r.Handle,GetCurrentProcess,@oh,0,0,DUPLICATE_SAME_ACCESS);
                  if status<>STATUS_SUCCESS then
                    status:=NtDuplicateObject(h,r.Handle,GetCurrentProcess,@oh,0,0,0);
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
                  if TSystemHandleType(r.Typ) in FTypes then begin
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
                        th:=0;
                        try
                          th:=BeginThread(nil,0,GetHandleInfoThreadExecute,@param,0,{$IFDEF FPC}tid{$ELSE}sz{$ENDIF});
                          try
                            if (WaitForSingleObject(th,200)=WAIT_TIMEOUT) then
                              TerminateThread(th,0);
                          finally
                            CloseHandle(th);
                          end;
                        except
                          TerminateThread(th,0);
                          CloseHandle(th);
                          r.Name:='';
                        end;
                        r.Name:=string(param.FileName);
                        if r.Name<>'' then
                          r.Name:=KernelNameToFilename(r.Name);
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
                  end;

                  if pshi^.UniqueProcessId<>GetCurrentProcessID then
                    NtClose(oh);
                end;

                if (r.Name<>'') then begin
                  r._ProcessName:=GetProcessName(r.PID);
                  new(pr);
                  pr^:=r;
                  FList.Add(pr);
                end;
              end;

              Inc(c);
              if c>PDWORD(Buffer)^ then
                Break;
              pshi:=PSystemHandleTableEntryInfo(PAnsiChar(pshi)+SizeOf(pshi^));
            finally
            end;
            if Terminated then
              Break;
          until pshi=nil;
        finally
          Freemem(p);
        end;
      end;
    finally
      if (h<>0) and (h<>INVALID_HANDLE_VALUE) then
        CloseHandle(h);
      Freemem(Buffer);
    end;
  finally
    InternalLock.Leave;
  end;
end;

procedure THndListMonThread.SetAutoSuspend(const Value: Boolean);
begin
  InternalLock.Enter;
  try
    FAutoSuspend:=Value;
  finally
    InternalLock.Leave;
  end;
end;

procedure THndListMonThread.SetCompleted(const Value: Cardinal);
begin
  InternalLock.Enter;
  try
    FCompleted:=Value;
  finally
    InternalLock.Leave;
  end;
end;

procedure THndListMonThread.SetOnCompleted(const Value: THndListMonNotifyEvent);
begin
  InternalLock.Enter;
  try
    FOnCompleted:=Value;
  finally
    InternalLock.Leave;
  end;
end;

procedure THndListMonThread.SetTypes(const Value: TSystemHandleTypes);
begin
  InternalLock.Enter;
  try
    FTypes:=Value;
  finally
    InternalLock.Leave;
  end;
end;

initialization
  InitNativeAPI;
end.
