{*******************************************************}
{       MiTeC System Information Component Suite        }
{         System Module List Monitor Thread             }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}


{$INCLUDE Compilers.inc}


unit MSI_SysModListMon;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, System.SyncObjs,
     {$ELSE}
     Windows, SysUtils, Classes, SyncObjs,
     {$ENDIF}
     MSI_Defs, MiTeC_NativeDefs, MiTeC_NativeAPI, MiTeC_Routines;

type
  TSystemModuleRecord = record
    Name: string;
    LoadOrder: Word;
    LoadCount: Word;
    Flags: Cardinal;
    Base: NativeUInt;
    Size: Cardinal;
    VersionInfo: TVersionInfo;

    _Exists: Boolean;
  end;
  PSystemModuleRecord = ^TSystemModuleRecord;

  TSysModListMonThread = class;

  TSysModListMonNotifyEvent = procedure(Sender: TSysModListMonThread) of object;

  TSysModListMonThread = class(TThread)
  private
    InternalLock: TCriticalSection;
    FOnInterval: TSysModListMonNotifyEvent;
    FInterval: Cardinal;
    FList: TList;
    FAutoSuspend: Boolean;
    procedure DoSync;
    function GetInterval: Cardinal;
    function GetOnInterval: TSysModListMonNotifyEvent;
    procedure SetInterval(const Value: Cardinal);
    procedure SetOnInterval(const Value: TSysModListMonNotifyEvent);
    function GetRecCount: Integer;
    function GetAutoSuspend: Boolean;
    procedure SetAutoSuspend(const Value: Boolean);
  protected
    procedure RefreshData;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetRecord(AIndex: Integer; var ARecord: TSystemModuleRecord);
    procedure GetRecordByName(const AName: string; var ARecord: TSystemModuleRecord);
    procedure GetList(AList: TList);
    procedure Clear;

    property AutoSuspend: Boolean read GetAutoSuspend write SetAutoSuspend;
    property RecordCount: Integer read GetRecCount;
    property Interval: Cardinal read GetInterval write SetInterval;
    property OnInterval: TSysModListMonNotifyEvent read GetOnInterval write SetOnInterval;
  end;

implementation

uses {$IFDEF RAD9PLUS}
     {$IFDEF TRIAL}VCL.Dialogs,{$ENDIF}System.Math,
     {$ELSE}
     {$IFDEF TRIAL}Dialogs,{$ENDIF}Math,
     {$ENDIF}
     MiTeC_Datetime;

{ TSysModListMonThread }

procedure TSysModListMonThread.Clear;
begin
  InternalLock.Enter;
  try
    FList.Clear;
  finally
    InternalLock.Leave;
  end;
end;

constructor TSysModListMonThread.Create;
begin
  {$IFDEF TRIAL}
  if not RunFromIDE then
    MessageDlg('TSysModListMonThread'+sLineBreak+cCompName+sLineBreak+cCopyright,mtInformation,[mbOK],0);
  {$ENDIF}
  inherited Create(True);
  InternalLock:=TCriticalSection.Create;
  FList:=TList.Create;
  FAutoSuspend:=False;
  FreeOnTerminate:=False;
  FInterval:=1000;
end;

destructor TSysModListMonThread.Destroy;
var
  i: Integer;
begin
  FOnInterval:=nil;
  for i:=0 to FList.Count-1 do
    Dispose(PSystemModuleRecord(FList[i]));
  FList.Free;
  FreeAndNil(InternalLock);
  inherited;
end;

procedure TSysModListMonThread.DoSync;
begin
  if Assigned(FOnInterval) then
    FOnInterval(Self);
end;

procedure TSysModListMonThread.Execute;
var
  se: TSimpleEvent;
begin
  se:=TSimpleEvent.Create{$IFDEF BDS35PLUS}(nil,False,False,''){$ENDIF};
  try
    while not Terminated do begin
      RefreshData;

      if Assigned(FOnInterval) and not Terminated then
        Synchronize(DoSync);

      if not Terminated then
        se.WaitFor(FInterval);
    end;
  finally
    se.Free;
  end;
end;

function TSysModListMonThread.GetAutoSuspend: Boolean;
begin
  InternalLock.Enter;
  try
    Result:=FAutoSuspend;
  finally
    InternalLock.Leave;
  end;
end;

function TSysModListMonThread.GetInterval: Cardinal;
begin
  InternalLock.Enter;
  try
    Result:=FInterval;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSysModListMonThread.GetList(AList: TList);
var
  i: Integer;
  p: PSystemModuleRecord;
begin
  InternalLock.Enter;
  try
    for i:=0 to AList.Count-1 do
      Dispose(PSystemModuleRecord(AList[i]));
    AList.Clear;
    AList.Capacity:=FList.Capacity;
    for i:=0 to FList.Count-1 do begin
      new(p);
      p^:=PSystemModuleRecord(FList[i])^;
      AList.Add(p);
    end;
  finally
    InternalLock.Leave;
  end;
end;

function TSysModListMonThread.GetOnInterval: TSysModListMonNotifyEvent;
begin
  InternalLock.Enter;
  try
    Result:=FOnInterval;
  finally
    InternalLock.Leave;
  end;
end;

function TSysModListMonThread.GetRecCount: Integer;
begin
  InternalLock.Enter;
  try
    Result:=FList.Count;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSysModListMonThread.GetRecord(AIndex: Integer;
  var ARecord: TSystemModuleRecord);
begin
  InternalLock.Enter;
  try
    ResetMemory(ARecord,sizeof(ARecord));
    if AIndex<FList.Count then
      ARecord:=PSystemModuleRecord(FList[AIndex])^;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSysModListMonThread.GetRecordByName(const AName: string;
  var ARecord: TSystemModuleRecord);
var
  i: integer;
begin
  InternalLock.Enter;
  try
    ResetMemory(ARecord,SizeOf(ARecord));
    for i:=0 to FList.Count-1 do
      if SameText(PSystemModuleRecord(FList[i])^.Name,AName) then begin
        ARecord:=PSystemModuleRecord(FList[i])^;
        Break;
      end;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSysModListMonThread.RefreshData;
var
  i,j,idx: Integer;
  Buffer: Pointer;
  n,status,br: Cardinal;
  psmi: PSystemModuleInformation;
  s: string;
  r: PSystemModuleRecord;
begin
  InternalLock.Enter;
  try
    for i:=0 to FList.Count-1 do
      PSystemModuleRecord(FList[i])._Exists:=False;

    Buffer:=nil;
    n:=32768;
    Buffer:=AllocMem(n);
    try
      status:=NtQuerySystemInformation(SystemModuleInformation,Buffer,n,@br);
      while (status=STATUS_BUFFER_OVERFLOW) or (status=STATUS_INFO_LENGTH_MISMATCH) do begin
        n:=Max(br,n+1024);
        ReallocMem(Buffer,n);
        status:=NtQuerySystemInformation(SystemModuleInformation,Buffer,n,@br);
      end;
      if status=STATUS_SUCCESS then begin
        psmi:=PSystemModuleInformation(Buffer);
        n:=psmi^.Count;
        for i:=0 to n-1 do begin
          s:=string(PAnsiChar(@psmi^.Modules[0].FullPathName));
          idx:=-1;
          for j:=0 to FList.Count-1 do begin
            if SameText(PSystemModuleRecord(FList[j]).Name,s) then begin
              idx:=j;
              Break;
            end;
          end;
          if idx=-1 then begin
            new(r);
            FList.Add(r);
            idx:=FList.Count-1;
          end;
          with PSystemModuleRecord(FList[idx])^ do begin
            _Exists:=True;
            s:=StringReplace(s,'\??\','',[]);
            s:=ExpandEnvVars(s);
            if s[1]='\' then
              s:=Copy(GetWinDir,1,2)+s;
            Size:=psmi^.Modules[0].ImageSize;
            Flags:=psmi^.Modules[0].Flags;
            Base:=psmi^.Modules[0].ImageBase;
            LoadOrder:=psmi^.Modules[0].LoadOrderIndex;
            LoadCount:=psmi^.Modules[0].LoadCount;
            if not SameText(Name,s) then begin
              Name:=s;
              GetFileVerInfo(Name,VersionInfo);
            end;
          end;
          psmi:=PSystemModuleInformation(PAnsiChar(psmi)+SizeOf(TSystemModule));
        end;
      end;
    finally
      Freemem(Buffer);
    end;

    i:=0;
    while i<FList.Count do
      if not PSystemModuleRecord(FList[i])._Exists then begin
        FList.Delete(i);
        Dispose(PSystemModuleRecord(FList[i]));
      end else
        Inc(i);
    FList.Capacity:=FList.Count;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSysModListMonThread.SetAutoSuspend(const Value: Boolean);
begin
  InternalLock.Enter;
  try
    FAutoSuspend:=Value;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSysModListMonThread.SetInterval(const Value: Cardinal);
begin
  InternalLock.Enter;
  try
    FInterval:=Value;
  finally
    InternalLock.Leave;
  end;
end;

procedure TSysModListMonThread.SetOnInterval(const Value: TSysModListMonNotifyEvent);
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

