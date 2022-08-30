{*******************************************************}
{               MiTeC Common Routines                   }
{             Performance Library for NT                }
{                                                       }
{          Copyright (c) 1997-2016 Michal Mutl          }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_PerfLibNT;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, System.Variants,
     {$ELSE}
     Variants, Windows, SysUtils, Classes,
     {$ENDIF}
     MiTeC_WinPerf;

//resourcestring
const
  SPERFNUMHEX_BadSize = 'Bad Size (PERF_NUMBER_HEX)';
  SPERFNUMDEC_BadSize = 'Bad Size (PERF_NUMBER_DEC)';
  SPERFNUMDEC1K_BadSize = 'Bad Size (PERF_NUMBER_DEC1000)';
  SPERFCNTR_BadSize = 'Bad Size (PERF_COUNTER)';
  SPERFCNTRRATE_BadSize = 'Bad Size (PERF_COUNTER_RATE)';
  SPERFCNTRBASE_BadSize = 'Bad Size (PERF_COUNTER_BASE)';
  SPERFCNTRELAPS_BadSize = 'Bad Size (PERF_COUNTER_ELAPSED)';
  SUnknownType = 'Cannot display data';
  SPERFTYPETEXT_BadData = 'Bad Data (PERF_TYPE_TEXT)';

  SPerSec = '/sec';
  SPercent = ' %';
  SSecs = ' secs';
  SFrac = 'Frac';
  SElapsed = 'Elapsed';

  SNoname = '<noname>';

  Timer100N = 10000000;
  Timer1S = 1000;

type
  TPerfLibNT = class;

  TPerfObject = class;

  TDetailLevel = (Novice,Advanced,Expert,Wizard);

  TPerfCounter = class(TObject)
  private
    FPerfObject: TPerfObject;

    FName: string;
    FDescription: string;
    FCounterOffset: DWORD;
    FDefaultScale: DWORD;
    FCounterSize: DWORD;
    FDetailLevel: TDetailLevel;
    FCounterType: DWORD;
    FIndex: DWORD;

    function GetData(InstanceIndex: DWORD): PAnsiChar;
    function GetDataStr(InstanceIndex: DWORD): string;
    function GetDataStrEx(InstanceIndex: DWORD): string;
    function GetDataNum(InstanceIndex: DWORD): Double;

    function GetValue(AInstance: Cardinal): Variant;
  protected
    property CounterOffset: DWORD read FCounterOffset;
    property Index: DWORD read FIndex;
  public
    constructor Create(AIndex: DWORD; APerfObject: TPerfObject; APerfCntr: PPERF_COUNTER_DEFINITION);
    destructor Destroy; override;

    property Name: string read FName;
    property Description: string read FDescription;
    property DefaultScale: DWORD read FDefaultScale;
    property DetailLevel: TDetailLevel read FDetailLevel;
    property CounterType: DWORD read FCounterType;
    property CounterSize: DWORD read FCounterSize;
    property ParentObject: TPerfObject read FPerfObject;

    property AsData[InstanceIndex: DWORD]: PAnsiChar read GetData;
    property AsNumber[InstanceIndex: DWORD]: Double read GetDataNum;
    property AsString[InstanceIndex: DWORD]: string read GetDataStr;
    property AsStringEx[InstanceIndex: DWORD]: string read GetDataStrEx;
  end;

  PPerfInstance = ^TPerfInstance;
  TPerfInstance = record
    Name: string;
    ID: Cardinal;
    Index: integer;
  end;

  TPerfObject = class(TObject)
  private
    FCounters: TStringList;

    FPerfLib: TPerfLibNT;
    FName: string;
    FDescription: string;
    FCodePage: DWORD;
    FCounterCount: integer;
    FDefaultCounter: integer;
    FDetailLevel: TDetailLevel;
    FPerfObj: PPERF_OBJECT_TYPE;
    FPerfTime: LARGE_INTEGER;
    FPerfFreq: LARGE_INTEGER;
    FIndex: DWORD;
    function GetCounter(Index: Integer): TPerfCounter;
    function GetInstance(Index: Integer): TPerfInstance;
    function GetInstanceCount: integer;
  protected
    property Index: DWORD read FIndex;
    property PerfLib: TPerfLibNT read FPerfLib;
    property PerfObj: PPERF_OBJECT_TYPE read FPerfObj;
  public
    constructor Create(AIndex: DWORD; APerfLib: TPerfLibNT; APerfObj: PPERF_OBJECT_TYPE);
    destructor Destroy; override;

    function GetCntrIndexByName(AName: string): integer;
    function GetInstIndexByName(AName: string): integer;
    //function GetInstIndexByID(AID: DWORD): integer;}

    property Name: string read FName;
    property Description: string read FDescription;
    property DetailLevel: TDetailLevel read FDetailLevel;
    property CounterCount: integer read FCounterCount;
    property DefaultCounter: integer read FDefaultCounter;
    property InstanceCount: integer read GetInstanceCount;
    property CodePage: DWORD read FCodePage;
    property PerfTime: LARGE_INTEGER read FPerfTime;
    property PerfFreq: LARGE_INTEGER read FPerfFreq;

    property Counters[Index: Integer]: TPerfCounter read GetCounter;
    property Instances[Index: Integer]: TPerfInstance read GetInstance;
  end;

  TPerfLibNT = class(TPersistent)
  private
    FPerfData: PPERF_DATA_BLOCK;

    FCounters,
    FHelps,
    FObjects: TStringList;
    FPerfTime: LARGE_INTEGER;
    FPerfFreq: LARGE_INTEGER;
    FPerfTime100nsec: LARGE_INTEGER;
    FRevision: DWORD;
    FVersion: DWORD;
    FMachine: string;

    FRegLM, FRegPD: HKEY;

    procedure GetNameStrings;
    procedure ReadObjects;
    function GetObject(Index: integer): TPerfObject;
    function GetObjectCount: integer;
    procedure SetMachine(const Value: string);
  protected
    function GetCounterData(ObjectIndex,InstanceIndex,CounterIndex: DWORD): PAnsiChar;
    property Helps: TStringList read FHelps;
  public
    constructor Create;
    destructor Destroy; override;

    procedure TakeSnapshot;
    procedure RefreshData;
    function GetIndexByName(AName: string): integer;
    procedure ClearSnapShot;

    property Machine: string read FMachine write SetMachine;

    property Names: TStringList read FCounters;
    property PerfTime: LARGE_INTEGER read FPerfTime;
    property PerfFreq: LARGE_INTEGER read FPerfFreq;
    property PerfTime100nsec: LARGE_INTEGER read FPerfTime100nsec;
    property Version: DWORD read FVersion;
    property Revision: DWORD read FRevision;
    property ObjectCount: integer read GetObjectCount;
    property Objects[Index: integer]: TPerfObject read GetObject;
  end;


  function GetDetailLevelStr(ALevel: TDetailLevel): string;

implementation

const
  rkPerfLib = {HKLM\}'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Perflib';
    rvLastCounter = 'LastCounter';
  rkPerfLib009 = {HKLM\}'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Perflib\009';
    rvCounter = 'Counter';
    rvHelp = 'Help';
  rkGlobal = 'Global';

function FirstObject(APerfData: PPERF_DATA_BLOCK): PPERF_OBJECT_TYPE;
begin
  Result:=PPERF_OBJECT_TYPE(PANSICHAR(APerfData)+APerfData^.HeaderLength);
end;

function NextObject(APerfObj: PPERF_OBJECT_TYPE): PPERF_OBJECT_TYPE;
begin
  Result:=PPERF_OBJECT_TYPE(PANSICHAR(APerfObj)+APerfObj^.TotalByteLength);
end;

function FirstInstance(APerfObj: PPERF_OBJECT_TYPE): PPERF_INSTANCE_DEFINITION;
begin
  Result:=PPERF_INSTANCE_DEFINITION(PANSICHAR(APerfObj)+APerfObj^.DefinitionLength);
end;

function NextInstance(APerfInst: PPERF_INSTANCE_DEFINITION): PPERF_INSTANCE_DEFINITION;
var
  PerfCntrBlk: PPERF_COUNTER_BLOCK;
begin
  PerfCntrBlk:=PPERF_COUNTER_BLOCK(PANSICHAR(APerfInst)+APerfInst^.ByteLength);
  result:=PPERF_INSTANCE_DEFINITION(PANSICHAR(PerfCntrBlk)+PerfCntrBlk^.ByteLength);
end;

function FirstCounter(APerfObj: PPERF_OBJECT_TYPE): PPERF_COUNTER_DEFINITION;
begin
  Result:=PPERF_COUNTER_DEFINITION(PANSICHAR(APerfObj)+APerfObj^.HeaderLength);
end;

function NextCounter(APerfCntr: PPERF_COUNTER_DEFINITION): PPERF_COUNTER_DEFINITION;
begin
  Result:=PPERF_COUNTER_DEFINITION(PANSICHAR(APerfCntr)+APerfCntr^.ByteLength);
end;

function GetDetailLevelStr(ALevel: TDetailLevel): string;
begin
  case ALevel of
    Novice: Result:='Novice';
    Advanced: Result:='Advanced';
    Expert: Result:='Expert';
    Wizard: Result:='Wizard';
  end;
end;

{ TPerfLibNT }

procedure TPerfLibNT.ClearSnapShot;
begin
  while FObjects.Count>0 do begin
    FObjects.Objects[FObjects.Count-1].Free;
    FObjects.Delete(FObjects.Count-1);
  end;
  FCounters.Clear;
  FHelps.Clear;
  ReallocMem(FPerfData,MaxWord);
  ZeroMemory(FPerfData,MaxWord);
end;

constructor TPerfLibNT.Create;
begin
  FCounters:=TStringList.Create;
  FHelps:=TStringList.Create;
  FObjects:=TStringList.Create;
  FPerfData:=AllocMem(MaxWord);
end;

destructor TPerfLibNT.Destroy;
begin
  {while FCounters.Count>0 do begin
    Dispose(PInteger(FCounters.Objects[FCounters.Count-1]));
    FCounters.Delete(FCounters.Count-1);
  end;
  while FHelps.Count>0 do begin
    Dispose(PInteger(FHelps.Objects[FHelps.Count-1]));
    FHelps.Delete(FHelps.Count-1);
  end;}
  while FObjects.Count>0 do begin
    FObjects.Objects[FObjects.Count-1].Free;
    FObjects.Delete(FObjects.Count-1);
  end;
  FCounters.Free;
  FObjects.Free;
  FHelps.Free;
  FreeMem(FPerfData);
  inherited;
end;

function TPerfLibNT.GetIndexByName(AName: string): integer;
begin
  Result:=FObjects.IndexOf(AName);
end;

procedure TPerfLibNT.GetNameStrings;
var
  hkPerfLib,
  hkPerfLib009: HKEY;
  dwBufferSize,
  dwBuffer,
  dwMaxValueLen: DWORD;
  lpNameStrings: PChar;
  p: integer;
  szID,
  szName: string;
begin
  {while FCounters.Count>0 do begin
    Dispose(PInteger(FCounters.Objects[FCounters.Count-1]));
    FCounters.Delete(FCounters.Count-1);
  end;
  while FHelps.Count>0 do begin
    Dispose(PInteger(FHelps.Objects[FHelps.Count-1]));
    FHelps.Delete(FHelps.Count-1);
  end;}
  FCounters.Clear;
  FHelps.Clear;
  RegOpenKeyEx(FRegLM,rkPerfLib,0,KEY_READ,hkPerflib);
  dwBufferSize:=sizeof(dwBuffer);
  RegQueryValueEx(hkPerflib,rvLastCounter,nil,nil,PBYTE(@dwBuffer),@dwBufferSize);
  RegCloseKey(hkPerflib);
  RegOpenKeyEx(FRegLM,rkPerfLib009,0,KEY_READ,hkPerflib009);
  RegQueryInfoKey(hkPerflib009,nil,nil,nil,nil,nil,nil,nil,nil,@dwMaxValueLen,nil,nil);
  dwBuffer:=dwMaxValueLen+1;
  lpNameStrings:=AllocMem(dwBuffer*sizeof(char));

  RegQueryValueEx(hkPerflib009,rvCounter,nil,nil,PBYTE(lpNameStrings),@dwBuffer);
  p:=0;
  repeat
    szID:=lpNameStrings+p;
    Inc(p,Length(szID)+1);
    szName:=lpNameStrings+p;
    Inc(p,Length(szName)+1);
    if szID<>'' then
      FCounters.AddObject(szName,TObject(StrToInt(szID)));
  until lpNameStrings[p]=#0;

  RegQueryInfoKey(hkPerflib009,nil,nil,nil,nil,nil,nil,nil,nil,@dwMaxValueLen,nil,nil);
  dwBuffer:=dwMaxValueLen+1;
  ReallocMem(lpNameStrings,dwBuffer*sizeof(char));
  RegQueryValueEx(hkPerflib009,rvHelp,nil,nil,PBYTE(lpNameStrings),@dwBuffer);

  p:=0;
  repeat
    szID:=lpNameStrings+p;
    Inc(p,Length(szID)+1);
    szName:=lpNameStrings+p;
    Inc(p,Length(szName)+1);
    if szID<>'' then
      FHelps.AddObject(szName,TObject(StrToInt(szID)));
  until lpNameStrings[p]=#0;
  FreeMem(lpNameStrings);
  RegCloseKey(hkPerflib009);
end;

function TPerfLibNT.GetObject(Index: integer): TPerfObject;
begin
  if Index<FObjects.Count then
    Result:=TPerfObject(FObjects.Objects[Index])
  else
    Result:=nil;
end;

function TPerfLibNT.GetObjectCount: integer;
begin
  Result:=FPerfData^.NumObjectTypes;
end;

function TPerfLibNT.GetCounterData;
var
  po: PPERF_OBJECT_TYPE;
  pc: PPERF_COUNTER_DEFINITION;
  pi: PPERF_INSTANCE_DEFINITION;
  i: DWORD;
begin
  po:=FirstObject(FPerfData);
  if ObjectIndex>0 then
    for i:=0 to ObjectIndex-1 do
      po:=NextObject(po);
  pc:=FirstCounter(po);
  if CounterIndex>0 then
    for i:=0 to CounterIndex-1 do
      pc:=NextCounter(pc);
  if Integer(po^.NumInstances)>0 then begin
    pi:=FirstInstance(po);
    if InstanceIndex>0 then
      for i:=0 to InstanceIndex-1 do
        pi:=NextInstance(pi);
    Result:=PAnsiChar(pi)+pi^.ByteLength+pc^.CounterOffset;
  end else begin
    Result:=PAnsiChar(po)+po^.DefinitionLength+pc^.CounterOffset;
  end;
end;

procedure TPerfLibNT.ReadObjects;
var
  PerfObj: PPERF_OBJECT_TYPE;
  PO: TPerfObject;
  i: integer;
begin
  while FObjects.Count>0 do begin
    FObjects.Objects[FObjects.Count-1].Free;
    FObjects.Delete(FObjects.Count-1);
  end;
  PerfObj:=FirstObject(FPerfData);
  for i:=0 to FPerfData^.NumObjectTypes-1 do begin
    PO:=TPerfObject.Create(FObjects.Count,Self,PerfObj);
    FObjects.AddObject(PO.Name,PO);
    PerfObj:=NextObject(PerfObj);
  end;
end;

procedure TPerfLibNT.RefreshData;
begin
  ClearSnapShot;
  if RegConnectRegistry(PChar(FMachine),HKEY_LOCAL_MACHINE,FRegLM)<>ERROR_SUCCESS then
    Exit;
  if RegConnectRegistry(PChar(FMachine),HKEY_PERFORMANCE_DATA,FRegPD)<>ERROR_SUCCESS then
    Exit;
  try
    GetNameStrings;
    TakeSnapShot;
    ReadObjects;
  finally
    RegCloseKey(FRegLM);
    RegCloseKey(FRegPD);
  end;
end;

procedure TPerfLibNT.SetMachine(const Value: string);
begin
  FMachine:=Value;
  {if FMachine='' then
    FMachine:='\\.'
  else
    if Pos('\\',FMachine)=0 then
      FMachine:='\\'+FMachine;}
end;

procedure TPerfLibNT.TakeSnapshot;
var
  BufferSize: DWORD;
const
  BYTEINCREMENT = 4096;
begin
  BufferSize:=MaxWord;
  ReallocMem(FPerfData,BufferSize);
  while RegQueryValueEx(FRegPD,rkGlobal,nil,nil,PBYTE(FPerfData),@BufferSize)=ERROR_MORE_DATA do begin
    BufferSize:=BufferSize+BYTEINCREMENT;
    ReallocMem(FPerfData,BufferSize);
  end;
  FPerfTime:=FPerfData^.PerfTime;
  FPerfFreq:=FPerfData^.PerfFreq;
  FPerfTime100nsec:=FPerfData^.PerfTime100nSec;
  FVersion:=FPerfData^.Version;
  FRevision:=FPerfData^.Revision;
end;

{ TPerfObject }

constructor TPerfObject.Create;
var
  PerfCntr: PPERF_COUNTER_DEFINITION;
  //PtrToCntr: PPERF_COUNTER_BLOCK;
  i :integer;
  PC: TPerfCounter;
begin
  FPerfLib:=APerfLib;
  FPerfObj:=APerfObj;
  FName:='';
  FDescription:='';
  FIndex:=AIndex;
  i:=FPerfLib.Names.IndexOfObject(TObject(APerfObj^.ObjectNameTitleIndex));
  if i>-1 then
    FName:=FPerfLib.Names[i];
  i:=FPerfLib.Helps.IndexOfObject(TObject(APerfObj^.ObjectHelpTitleIndex));
  if i>-1 then
    FDescription:=FPerfLib.Helps[i];
  FCodePage:=APerfObj^.CodePage;
  FCounterCount:=APerfObj^.NumCounters;
  FDefaultCounter:=APerfObj^.DefaultCounter;
  FPerfTime:=APerfObj^.PerfTime;
  FPerfFreq:=APerfObj^.PerfFreq;
  case APerfObj^.DetailLevel of
    PERF_DETAIL_NOVICE: FDetailLevel:=Novice;
    PERF_DETAIL_ADVANCED: FDetailLevel:=Advanced;
    PERF_DETAIL_EXPERT: FDetailLevel:=Expert;
    PERF_DETAIL_WIZARD: FDetailLevel:=Wizard;
  end;
  FCounters:=TStringList.Create;

  PerfCntr:=FirstCounter(APerfObj);
  //PtrToCntr:=PPERF_COUNTER_BLOCK(PChar(APerfObj)+APerfObj^.DefinitionLength);
  if CounterCount>0 then
    for i:=0 to CounterCount-1 do begin
      PC:=TPerfCounter.Create(FCounters.Count,Self,PerfCntr);
      FCounters.AddObject(PC.Name,PC);
      PerfCntr:=NextCounter(PerfCntr);
    end;
end;

destructor TPerfObject.Destroy;
begin
  while FCounters.Count>0 do begin
    FCounters.Objects[FCounters.Count-1].Free;
    FCounters.Delete(FCounters.Count-1);
  end;
  FCounters.Free;
  inherited;
end;

function TPerfObject.GetCounter(Index: Integer): TPerfCounter;
begin
  if Index<FCounters.Count then
    Result:=TPerfCounter(FCounters.Objects[Index])
  else
    Result:=nil;
end;

function TPerfObject.GetCntrIndexByName(AName: string): integer;
begin
  Result:=FCounters.IndexOf(AName);
end;

function TPerfObject.GetInstance(Index: Integer): TPerfInstance;
var
  pi: PPERF_INSTANCE_DEFINITION;
  j: Integer;
  i: Cardinal;
begin
  if Index<InstanceCount then begin
    pi:=FirstInstance(FPerfObj);
    if Index>0 then
      for j:=0 to Index-1 do
        pi:=NextInstance(pi);
    Result.Name:='';
    Result.Index:=Index;
    for i:=0 to pi^.NameLength-1 do
      if PAnsiChar(pi)[pi^.NameOffset+i]<>#0 then
        Result.Name:=Result.Name+string(PAnsiChar(pi)[pi^.NameOffset+i]);
    Result.ID:=pi^.UniqueID;
  end;
end;

{function TPerfObject.GetInstIndexByID(AID: DWORD): integer;
var
  i: integer;
begin
  Result:=-1;
end;}

function TPerfObject.GetInstIndexByName(AName: string): integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to Self.InstanceCount-1 do
    if SameText(AName,Self.Instances[i].Name) then begin
      Result:=i;
      Break;
    end;
end;

function TPerfObject.GetInstanceCount: integer;
var
  po: PPERF_OBJECT_TYPE;
  i: integer;
begin
  po:=FirstObject(FPerfLib.FPerfData);
  if Index>0 then
    for i:=0 to Index-1 do
      po:=NextObject(po);
  Result:=po^.NumInstances;
end;

{ TPerfCounter }


constructor TPerfCounter.Create;
var
  idx: integer;
begin
  FPerfObject:=APerfObject;
  FIndex:=AIndex;
  idx:=FPerfObject.PerfLib.Names.IndexOfObject(TObject(APerfCntr^.CounterNameTitleIndex));
  if idx>-1 then
    FName:=FPerfObject.PerfLib.Names[idx]
  else
    FName:=SNoname;
  idx:=FPerfObject.PerfLib.Helps.IndexOfObject(TObject(APerfCntr^.CounterHelpTitleIndex));
  if idx>-1 then
    FDescription:=FPerfObject.PerfLib.Helps[idx]
  else
    FDescription:='';
  FCounterOffset:=APerfCntr^.CounterOffset;
  FDefaultScale:=APerfCntr^.DefaultScale;
  FCounterSize:=APerfCntr^.CounterSize;
  FCounterType:=APerfCntr^.CounterType;
  case APerfCntr^.DetailLevel of
    PERF_DETAIL_NOVICE: FDetailLevel:=Novice;
    PERF_DETAIL_ADVANCED: FDetailLevel:=Advanced;
    PERF_DETAIL_EXPERT: FDetailLevel:=Expert;
    PERF_DETAIL_WIZARD: FDetailLevel:=Wizard;
  end;
end;

destructor TPerfCounter.Destroy;
begin

  inherited;
end;

function TPerfCounter.GetData;
begin
  Result:=FPerfObject.PerfLib.GetCounterData(FPerfObject.Index,InstanceIndex,Self.Index);
end;

function TPerfCounter.GetDataNum(InstanceIndex: DWORD): Double;
begin
  try Result:=GetValue(InstanceIndex) except Result:=0 end;
end;

function TPerfCounter.GetDataStr;
begin
  try
    Result:=VarToStr(GetValue(InstanceIndex));
  except
    Result:='';
  end;
end;

function TPerfCounter.GetDataStrEx;
begin
  try
    Result:=VarToStr(GetValue(InstanceIndex));
    case CounterType and PERF_DISPLAY_MASK of
      PERF_DISPLAY_PER_SEC: result:=Result+' /sec';
      PERF_DISPLAY_PERCENT: result:=result+' %';
      PERF_DISPLAY_SECONDS: result:=result+' secs';
    end;
  except
    Result:='';
  end;
end;

function TPerfCounter.GetValue;
var
  data0, data1 : PAnsiChar;
  c : DWORD;

  function GetTextResult (data : PAnsiChar) : variant;
  var
    w : WideString;
    s : string;
    len : DWORD;
  begin
    len := PDWORD (data)^;
    Inc (data, sizeof (DWORD));
    if (c and PERF_TEXT_ASCII) = PERF_TEXT_ASCII then
    begin
      SetLength (s, len);
      Move (data^, s [1], len);
      result := s
    end
    else
    begin
      SetLength (w, len);
      Move (data^, w [1], len);
      result := w
    end
  end;

  function GetNumberResult (c : Integer; data : PAnsiChar) : variant;
  var
    d : double;
  begin
    if (c and PERF_TYPE_ZERO) = PERF_TYPE_ZERO then
      result := 0
    else
      if ((c and PERF_TYPE_ZERO) = PERF_TYPE_NUMBER) or (c and PERF_TYPE_ZERO = PERF_TYPE_COUNTER) then
      begin
        case (c and PERF_SIZE_VARIABLE_LEN) of
          PERF_SIZE_DWORD :
            result := PInteger (data)^;
          PERF_SIZE_LARGE :
            begin
              d := PInt64 (data)^;
              result := d
            end;

          else
            result := NULL
        end;

        if ((c and PERF_TYPE_ZERO) = PERF_TYPE_NUMBER) and (result <> NULL) then
          case (c and PERF_NUMBER_MASK) of
            PERF_NUMBER_DECIMAL:;
            PERF_NUMBER_DEC_1000: result := result / 1000;
            PERF_NUMBER_HEX :
              if (c and PERF_SIZE_VARIABLE_LEN) = PERF_SIZE_DWORD then
                result := IntToHex (PInteger (data)^, 8)
              else
                result := IntToHex (PInt64 (data)^, 16)
          end
      end
      else
        result := NULL
  end;

  procedure GetCounterResult;
  var
    x0, y0, x1, y1 : variant;
    d0, d1 : double;
    tempP : PAnsiChar;
    tempCH : TPerfCounter;
  begin
    x1 := GetNumberResult (c, data1);
    if (c and (PERF_DELTA_COUNTER or PERF_DELTA_BASE)) <> 0 then
    begin
      x0 := GetNumberResult (c, data0);
      x1 := x1 - x0;

      if (c and PERF_DELTA_BASE) <> 0 then
      begin
        tempP:=Self.ParentObject.FPerfLib.GetCounterData(Self.ParentObject.Index,AInstance,Self.Index+1);
        tempCH:=Self.ParentObject.Counters[Self.Index+1];
        if (tempCH.CounterType and PERF_COUNTER_MASK) = PERF_COUNTER_BASE then
          y1 := GetNumberResult (tempCH.CounterType, tempP);

        tempP:=Self.ParentObject.FPerfLib.GetCounterData(Self.ParentObject.Index,AInstance,Self.Index+1);
        tempCH:=Self.ParentObject.Counters[Self.Index+1];
        if (tempCH.CounterType and PERF_COUNTER_MASK) = PERF_COUNTER_BASE then
          y0 := GetNumberResult (tempCH.CounterType, tempP);

        y1 := y1 - y0;
      end
    end
    else
      if (c and PERF_COUNTER_MASK) = PERF_COUNTER_FRACTION then
      begin
        tempP:=Self.ParentObject.FPerfLib.GetCounterData(Self.ParentObject.Index,AInstance,Self.Index+1);
        tempCH:=Self.ParentObject.Counters[Self.Index+1];
        if (tempCH.CounterType and PERF_COUNTER_MASK) = PERF_COUNTER_BASE then
          y1 := GetNumberResult (tempCH.CounterType, tempP);
      end;

    case (c and PERF_COUNTER_MASK) of
      PERF_COUNTER_RATE :
        begin
          case (c and PERF_TIMEBASE_MASK) of
            PERF_TIMER_TICK  : d1 := Self.ParentObject.FPerfLib.PerfTime.QuadPart;
            PERF_TIMER_100NS : d1 := Self.ParentObject.FPerfLib.PerfTime100nSec.QuadPart;
            PERF_OBJECT_TIMER: d1 := Self.ParentObject.PerfTime.QuadPart;
            else
              d1 := 0
          end;

          y1 := d1;

          if (c and PERF_DELTA_COUNTER) <> 0 then
          begin
            case (c and PERF_TIMEBASE_MASK) of
              PERF_TIMER_TICK  : d0 := Self.ParentObject.FPerfLib.PerfTime.QuadPart;
              PERF_TIMER_100NS : d0 := Self.ParentObject.FPerfLib.PerfTime100nSec.QuadPart;
              PERF_OBJECT_TIMER: d0 := Self.ParentObject.PerfTime.QuadPart;
              else
                d0 := 0
            end;

            y0 := d0;
            y1 := y1 - y0;
          end;

          if y1 = 0 then
            x1 := 0
          else
            x1 := x1 / y1;

        end;
      PERF_COUNTER_ELAPSED:
        begin
          d1 := 0;
          d0 := 1;
          case (c and PERF_TIMEBASE_MASK) of
            PERF_TIMER_TICK  : d1 := Self.ParentObject.FPerfLib.PerfTime.QuadPart;
            PERF_TIMER_100NS : d1 := Self.ParentObject.FPerfLib.PerfTime100nSec.QuadPart;
            PERF_OBJECT_TIMER:
              begin
                d1 := Self.ParentObject.PerfTime.QuadPart;
                d0 := Self.ParentObject.PerfFreq.QuadPart;
              end;
          end;

          y1 := d1;
          x1 := (y1 - x1) / d0;
        end;

      PERF_COUNTER_BASE :;

      PERF_COUNTER_FRACTION :
        begin
          if (x1 = 0) or (y1 = 0) then
            x1 := 0
          else
            x1 := x1 / y1;
        end;

      PERF_COUNTER_QUEUELEN :
        begin
          case (c and PERF_TIMEBASE_MASK) of
            PERF_TIMER_TICK  : d1 := Self.ParentObject.FPerfLib.PerfTime.QuadPart;
            PERF_TIMER_100NS : d1 := Self.ParentObject.FPerfLib.PerfTime100nSec.QuadPart;
            PERF_OBJECT_TIMER: d1 := Self.ParentObject.PerfTime.QuadPart;
            else
              d1 := 0
          end;

          y1 := d1;

          if (c and PERF_DELTA_COUNTER) <> 0 then
          begin
            case (c and PERF_TIMEBASE_MASK) of
              PERF_TIMER_TICK  : d0 := Self.ParentObject.FPerfLib.PerfTime.QuadPart;
              PERF_TIMER_100NS : d0 := Self.ParentObject.FPerfLib.PerfTime100nSec.QuadPart;
              PERF_OBJECT_TIMER: d0 := Self.ParentObject.PerfTime.QuadPart;
              else
                d0 := 0
            end;

            y0 := d0;
            y1 := y1 - y0;
          end;

          if y1 = 0 then
            x1 := 0
          else
            x1 := x1 / y1;
        end

    end;

    if (c and PERF_INVERSE_COUNTER) <> 0 then
      x1 := 1 - x1;

    case c and PERF_DISPLAY_MASK of
      PERF_DISPLAY_PERCENT :
        x1 := x1 * 100;
      PERF_DISPLAY_SECONDS :
        begin
//          d0 := fSnapshot1.fPerfDataBlock^.PerfFreq;
//          x1 := x1 / d0;
        end;
      PERF_DISPLAY_PER_SEC :
        begin
          d0 := Self.ParentObject.PerfLib.PerfFreq.QuadPart;
          x1 := x1 * d0;
        end;
    end;

    result:=x1;
  end;

begin
  result:=NULL;
  try
    data1:=Self.ParentObject.FPerfLib.GetCounterData(Self.ParentObject.Index,AInstance,Self.Index);
    c:=Self.CounterType;
    case c and PERF_TYPE_ZERO of
      PERF_TYPE_NUMBER,
      PERF_TYPE_ZERO: result:=GetNumberResult(c,data1);
      PERF_TYPE_COUNTER: begin
        data0:=Self.ParentObject.FPerfLib.GetCounterData(Self.ParentObject.Index,AInstance,Self.Index);
        GetCounterResult;
      end;
      PERF_TYPE_TEXT: result:=GetTextResult(data1);
    end;
  except
  end
end;

end.
