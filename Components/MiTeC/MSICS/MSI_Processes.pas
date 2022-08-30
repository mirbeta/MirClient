{*******************************************************}
{       MiTeC System Information Component Suite        }
{              Process Detection Part                   }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}


{$INCLUDE Compilers.inc}


unit MSI_Processes;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MSI_Common, MSI_Defs, MSI_ProcListMon, MSI_SvcListMon, MiTeC_AdvApi,
     MiTeC_Routines, MiTeC_NativeDefs, MSI_HndListMon;

const
  StorageFolderName = 'ProcessList';
  ServiceList_StorageFolderName = 'ServiceList';
  HandleList_StorageFolderName = 'HandleList';

type
  TDetectionType = (dtProcesses, dtServices, dtHandles);

  TDetectionRange = set of TDetectionType;

const
  drDetectAll = [dtProcesses, dtServices, dtHandles];

type
  TMiTeC_ProcessList = class(TMiTeC_Component)
  private
    FPLM: TProcListMonThread;
    FSLM: TSvcListMonThread;
    FHLM: THndListMonThread;
    FPL,FSL,FHL: TList;
    FDR: TDetectionRange;
    FHT: TSystemHandleTypes;
    function GetProcess(Index: Integer): TProcessRecord;
    function GetProcessCount: integer;
    function GetService(Index: Integer): TServiceRecord;
    function GetServiceByName(Name: string): TServiceRecord;
    function GetServiceCount: integer;
    function GetHandle(Index: Integer): THandleRecord;
    function GetHandleCount: integer;
    function GetProcessByPID(APID: Cardinal): TProcessRecord;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;

    property Processes[Index: Integer]: TProcessRecord read GetProcess;
    property Services[Index: Integer]: TServiceRecord read GetService;
    property Handles[Index: Integer]: THandleRecord read GetHandle;

    property ProcessByPID[APID: Cardinal]: TProcessRecord read GetProcessByPID;
    property ServiceName[Name: string]: TServiceRecord read GetServiceByName;
  published
    property ProcessCount: integer read GetProcessCount stored False;
    property ServiceCount: integer read GetServiceCount stored False;
    property HandleCount: integer read GetHandleCount stored False;

    property DetectionRange: TDetectionRange read FDR Write FDR;
    property HandleTypes: TSystemHandleTypes read FHT Write FHT;
  end;

implementation

uses {$IFDEF RAD9PLUS}
     System.SyncObjs;
     {$ELSE}
     SyncObjs;
     {$ENDIF}

{ TMiTeC_ProcessList }

procedure TMiTeC_ProcessList.Clear;
var
  i: Integer;
begin
  for i:=0 to FPL.Count-1 do
    if Assigned(FPL[i]) then begin
      Dispose(PProcessRecord(FPL[i]));
    end;
  FPL.Clear;
  for i:=0 to FSL.Count-1 do
    if Assigned(FSL[i]) then
      Dispose(PServiceRecord(FSL[i]));
  FSL.Clear;
  for i:=0 to FHL.Count-1 do
    if Assigned(FHL[i]) then
      Dispose(PHandleRecord(FHL[i]));
  FHL.Clear;
end;

constructor TMiTeC_ProcessList.Create;
begin
  inherited Create(AOwner);
  DetectionRange:=[dtProcesses, dtServices];
  FHT:=[OB_TYPE_FILE];
  FPL:=TList.Create;
  FSL:=TList.Create;
  FHL:=TList.Create;
  FPLM:=nil;
  FSLM:=nil;
  FHLM:=nil;
end;

destructor TMiTeC_ProcessList.Destroy;
begin
  if Assigned(FPLM) then
    FreeAndNil(FPLM);
  if Assigned(FSLM) then
    FreeAndNil(FSLM);
  if Assigned(FHLM) then
    FreeAndNil(FHLM);
  Clear;
  FSL.Free;
  FPL.Free;
  FHL.Free;
  inherited;
end;

function TMiTeC_ProcessList.Gethandle(Index: Integer): THandleRecord;
begin
  Result:=PHandleRecord(FHL[Index])^;
end;

function TMiTeC_ProcessList.GetHandleCount: integer;
begin
  Result:=FHL.Count;
end;

function TMiTeC_ProcessList.GetProcess(Index: Integer): TProcessRecord;
begin
  Result:=PProcessRecord(FPL[Index])^;
end;

function TMiTeC_ProcessList.GetProcessByPID(APID: Cardinal): TProcessRecord;
var
  i: Integer;
begin
  ResetMemory(Result,SizeOf(Result));
  for i:=0 to FPL.Count-1 do
    if (PProcessRecord(FPL[i])^.PID=APID) then begin
      Result:=PProcessRecord(FPL[i])^;
      Break;
    end;
end;

function TMiTeC_ProcessList.GetProcessCount: integer;
begin
  Result:=FPL.Count;
end;

function TMiTeC_ProcessList.GetService(Index: Integer): TServiceRecord;
begin
  Result:=PServiceRecord(FSL[Index])^;
end;

function TMiTeC_ProcessList.GetServiceByName(Name: string): TServiceRecord;
var
  i: Integer;
begin
  ResetMemory(Result,SizeOf(Result));
  for i:=0 to FSL.Count-1 do
    if SameText(PServiceRecord(FSL[i])^.Name,Name) then begin
      Result:=PServiceRecord(FSL[i])^;
      Break;
    end;
end;

function TMiTeC_ProcessList.GetServiceCount: integer;
begin
  Result:=FSL.Count;
end;

procedure TMiTeC_ProcessList.RefreshData;
var
  se: TSimpleEvent;
begin
  if not Assigned(FPLM) then begin
    FPLM:=TProcListMonThread.Create;
    FPLM.AutoSuspend:=True;
  end;

  if not Assigned(FSLM) then begin
    FSLM:=TSvcListMonThread.Create;
    FSLM.Types:=stAll;
    FSLM.AutoSuspend:=True;
  end;

  if not Assigned(FHLM) then begin
    FHLM:=THndListMonThread.Create;
    FHLM.AutoSuspend:=True;
  end;
  FHLM.Types:=FHT;

  inherited;
  if dtProcesses in FDR then
    FPLM.Suspended:=False;
  if dtServices in FDR then
    FSLM.Suspended:=False;
  if dtHandles in FDR then
    FHLM.Suspended:=False;
  se:=TSimpleEvent.Create{$IFDEF BDS35PLUS}(nil,False,False,''){$ENDIF};
  try
    while not FPLM.Suspended or not FSLM.Suspended or not FHLM.Suspended do
      se.WaitFor(1000);
  finally
    se.Free;
  end;
  FPLM.GetList(FPL);
  FSLM.GetList(FSL);
  FHLM.GetList(FHL);
  FPLM.Clear;
  FSLM.Clear;
  FHLM.Clear;
  SetDataAvail(True);
end;

procedure TMiTeC_ProcessList.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

procedure WriteProcessToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
    sl:=TStringList.Create;
    try
      WriteIntProperty(sl,'PID',Self.Processes[AIndex].PID);
      WriteStrProperty(sl,'Name',Self.Processes[AIndex].Name);
      WriteStrProperty(sl,'ImageName',Self.Processes[AIndex].ImageName);
      WriteStrProperty(sl,'CommandLine',Self.Processes[AIndex].CommandLine);
      WriteStrProperty(sl,'UserName',Self.Processes[AIndex].UserName);
      WriteIntProperty(sl,'Priority',Self.Processes[AIndex].Priority);
      WriteIntProperty(sl,'ParentPID',Self.Processes[AIndex].ParentPID);
      WriteIntProperty(sl,'HandleCount',Self.Processes[AIndex].HandleCount);
      WriteIntProperty(sl,'ThreadCount',Self.Processes[AIndex].ThreadCount);
      WriteIntProperty(sl,'CreateTime',Self.Processes[AIndex].CPUTimes.CreateTime.QuadPart);
      WriteIntProperty(sl,'UserTime',Self.Processes[AIndex].CPUTimes.UserTime.QuadPart);
      WriteIntProperty(sl,'KernelTime',Self.Processes[AIndex].CPUTimes.KernelTime.QuadPart);
      WriteIntProperty(sl,'Bits',Self.Processes[AIndex].Bits);
      WriteIntProperty(sl,'WorkingSet',Self.Processes[AIndex].VMCounters.WorkingSetSize);
      WriteIntProperty(sl,'PeakWorkingSet',Self.Processes[AIndex].VMCounters.PeakWorkingSetSize);
      WriteIntProperty(sl,'PageFileUsage',Self.Processes[AIndex].VMCounters.PagefileUsage);
      WriteIntProperty(sl,'PeakPageFileUsage',Self.Processes[AIndex].VMCounters.PeakPagefileUsage);
      WriteDtProperty(sl,'CreationTime',Self.Processes[AIndex].CreationTime);
      strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_OPEN,True);
      try
        SaveToEncodedStream(sl,strm,ACodeStream);
      finally
        strm.Free;
      end;
    finally
      sl.Free;
    end;
end;

procedure WriteServiceToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
    sl:=TStringList.Create;
    try
      WriteStrProperty(sl,'Name',Self.Services[AIndex].Name);
      WriteStrProperty(sl,'DisplayName',Self.Services[AIndex].DisplayName);
      WriteStrProperty(sl,'ImageName',Self.Services[AIndex].ImageName);
      WriteStrProperty(sl,'ObjectName',Self.Services[AIndex].ObjectName);
      WriteStrProperty(sl,'Description',Self.Services[AIndex].Description);
      WriteStrProperty(sl,'Group',Self.Services[AIndex].Group);
      WriteStrProperty(sl,'DependOnService',Self.Services[AIndex].DependOnService);
      WriteIntProperty(sl,'Startup',Self.Services[AIndex].StartUp);
      WriteIntProperty(sl,'Status',Self.Services[AIndex].Status);
      WriteIntProperty(sl,'Tag',Self.Services[AIndex].Tag);
      WriteIntProperty(sl,'ErrCtrl',Self.Services[AIndex].ErrCtrl);
      WriteIntProperty(sl,'Typ',integer(Self.Services[AIndex].Typ));
      WriteIntProperty(sl,'PID',Self.Services[AIndex].PID);
      strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_OPEN,True);
      try
        SaveToEncodedStream(sl,strm,ACodeStream);
      finally
        strm.Free;
      end;
    finally
      sl.Free;
    end;
end;

procedure WriteHandleToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
    sl:=TStringList.Create;
    try
      WriteStrProperty(sl,'Name',Self.Handles[AIndex].Name);
      WriteIntProperty(sl,'Handle',Self.Handles[AIndex].Handle);
      WriteIntProperty(sl,'Type',Self.Handles[AIndex].Typ);
      WriteIntProperty(sl,'PID',Self.Handles[AIndex].PID);
      strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_OPEN,True);
      try
        SaveToEncodedStream(sl,strm,ACodeStream);
      finally
        strm.Free;
      end;
    finally
      sl.Free;
    end;
end;

var
  i: Integer;
begin
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(StorageFolderName);
    Sub:=SS.OpenSubStorage(StorageFolderName,STG_OPEN,True);
    try
      for i:=0 to Self.ProcessCount-1 do
        WriteProcessToStream(i);
    finally
      Sub.Free;
    end;
    SS.DeleteElement(ServiceList_StorageFolderName);
    Sub:=SS.OpenSubStorage(ServiceList_StorageFolderName,STG_OPEN,True);
    try
      for i:=0 to Self.ServiceCount-1 do
        WriteServiceToStream(i);
    finally
      Sub.Free;
    end;
    SS.DeleteElement(HandleList_StorageFolderName);
    Sub:=SS.OpenSubStorage(HandleList_StorageFolderName,STG_OPEN,True);
    try
      for i:=0 to Self.HandleCount-1 do
        WriteHandleToStream(i);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

function TMiTeC_ProcessList.LoadFromStorage;
var
  stg: IStorage;
  SS, Sub: TStructuredStorage;

function ReadProcessFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
  p: PProcessRecord;
begin
  Result:=False;
  try strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        new(p);
        p.PID:=ReadIntProperty(sl,'PID');
        p.Name:=ReadStrProperty(sl,'Name');
        p.ImageName:=ReadStrProperty(sl,'ImageName');
        p.CommandLine:=ReadStrProperty(sl,'CommandLine');
        p.UserName:=ReadStrProperty(sl,'UserName');
        p.Priority:=ReadIntProperty(sl,'Priority');
        p.ParentPID:=ReadIntProperty(sl,'ParentPID');
        p.HandleCount:=ReadIntProperty(sl,'HandleCount');
        p.ThreadCount:=ReadIntProperty(sl,'ThreadCount');
        p.CPUTimes.CreateTime.QuadPart:=ReadIntProperty(sl,'CreateTime');
        p.CPUTimes.UserTime.QuadPart:=ReadIntProperty(sl,'UserTime');
        p.CPUTimes.KernelTime.QuadPart:=ReadIntProperty(sl,'KernelTime');
        if sl.IndexOfName('Is64')>-1 then begin
          if ReadIntProperty(sl,'Is64')=1 then
            p.Bits:=64
          else
            p.Bits:=32;
        end else
          p.Bits:=ReadIntProperty(sl,'Bits');
        p.VMCounters.WorkingSetSize:=ReadIntProperty(sl,'WorkingSet');
        p.VMCounters.PeakWorkingSetSize:=ReadIntProperty(sl,'PeakWorkingSet');
        p.VMCounters.PagefileUsage:=ReadIntProperty(sl,'PageFileUsage');
        p.VMCounters.PeakPagefileUsage:=ReadIntProperty(sl,'PeakPageFileUsage');
        p.CreationTime:=ReadDtProperty(sl,'CreationTime');
        FPL.Add(p);
        Result:=True;
        SetDataAvail(True);
      finally
        sl.Free;
      end;
    finally
      strm.Free;
    end;
end;

function ReadServiceFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
  p: PServiceRecord;
begin
  Result:=False;
  try strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        new(p);
        p.Name:=ReadStrProperty(sl,'Name');
        p.DisplayName:=ReadStrProperty(sl,'DisplayName');
        p.ImageName:=ReadStrProperty(sl,'ImageName');
        p.ObjectName:=ReadStrProperty(sl,'ObjectName');
        p.Description:=ReadStrProperty(sl,'Description');
        p.Group:=ReadStrProperty(sl,'Group');
        p.DependOnService:=ReadStrProperty(sl,'DependOnService');
        p.StartUp:=ReadIntProperty(sl,'Startup');
        p.Status:=ReadIntProperty(sl,'Status');
        p.Tag:=ReadIntProperty(sl,'Tag');
        p.ErrCtrl:=ReadIntProperty(sl,'ErrCtrl');
        p.PID:=ReadIntProperty(sl,'PID');
        p.Typ:=TServiceType(ReadIntProperty(sl,'Typ'));
        FSL.Add(p);
        Result:=True;
        SetDataAvail(True);
      finally
        sl.Free;
      end;
    finally
      strm.Free;
    end;
end;

function ReadHandleFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
  p: PHandleRecord;
begin
  Result:=False;
  try strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        new(p);
        p.Name:=ReadStrProperty(sl,'Name');
        p.Handle:=ReadIntProperty(sl,'Handle');
        p.Typ:=ReadIntProperty(sl,'Type');
        p.PID:=ReadIntProperty(sl,'PID');
        FHL.Add(p);
        Result:=True;
        SetDataAvail(True);
      finally
        sl.Free;
      end;
    finally
      strm.Free;
    end;
end;

var
  i: Integer;
begin
  Clear;
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    try
      Sub:=SS.OpenSubStorage(StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;
    try
      i:=0;
      while ReadProcessFromStream(i) do
        Inc(i);
      Result:=Result or (i>0);
    finally
      if Assigned(Sub) then
        Sub.Free;
    end;
    try
      Sub:=SS.OpenSubStorage(ServiceList_StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;
    try
      i:=0;
      while ReadServiceFromStream(i) do
        Inc(i);
      Result:=Result or (i>0);
    finally
      if Assigned(Sub) then
        Sub.Free;
    end;
    try
      Sub:=SS.OpenSubStorage(HandleList_StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;
    try
      i:=0;
      while ReadHandleFromStream(i) do
        Inc(i);
      Result:=Result or (i>0);
    finally
      if Assigned(Sub) then
        Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

end.
