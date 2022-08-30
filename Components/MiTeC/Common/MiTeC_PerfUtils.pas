{*******************************************************}
{               MiTeC Common Routines                   }
{             Performance Library Utils                 }
{                                                       }
{          Copyright (c) 1997-2016 Michal Mutl          }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_PerfUtils;

interface

uses{$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils,
     {$ELSE}
     Windows, SysUtils,
     {$ENDIF}
     MiTeC_WinPerf;

function GetCounterBlock(AObj: PPerfObjectType): PPerfCounterBlock; overload;
function GetCounterBlock(AInstance: PPerfInstanceDefinition): PPerfCounterBlock; overload;
function GetCounterDataAddress(AObj: PPerfObjectType; ACounter: PPerfCounterDefinition;
  AInstance: PPerfInstanceDefinition = nil): Pointer; overload;
function GetCounterDataAddress(AObj: PPerfObjectType; ACounter, AInstance: Integer): Pointer; overload;
function GetCounter(AObj: PPerfObjectType; AIndex: Integer): PPerfCounterDefinition;
function GetCounterByNameIndex(AObj: PPerfObjectType; ANameIndex: Cardinal): PPerfCounterDefinition;
function GetCounterValue32(AObj: PPerfObjectType; ACounter: PPerfCounterDefinition;
  AInstance: PPerfInstanceDefinition = nil): Cardinal;
function GetCounterValue64(AObj: PPerfObjectType; ACounter: PPerfCounterDefinition;
  AInstance: PPerfInstanceDefinition = nil): UInt64;
function GetCounterValueText(AObj: PPerfObjectType; ACounter: PPerfCounterDefinition;
  AInstance: PPerfInstanceDefinition = nil): PChar;
function GetCounterValueWideText(AObj: PPerfObjectType; ACounter: PPerfCounterDefinition;
  AInstance: PPerfInstanceDefinition = nil): PWideChar;
function GetFirstCounter(AObj: PPerfObjectType): PPerfCounterDefinition;
function GetFirstInstance(AObj: PPerfObjectType): PPerfInstanceDefinition;
function GetFirstObject(AData: PPerfDataBlock): PPerfObjectType; overload;
function GetFirstObject(AHeader: PPerfLibHeader): PPerfObjectType; overload;
function GetInstance(AObj: PPerfObjectType; aIndex: Integer): PPerfInstanceDefinition;
function GetInstanceName(AInstance: PPerfInstanceDefinition): PWideChar;
function GetNextCounter(ACounter: PPerfCounterDefinition): PPerfCounterDefinition;
function GetNextInstance(AInstance: PPerfInstanceDefinition): PPerfInstanceDefinition;
function GetNextObject(AObj: PPerfObjectType): PPerfObjectType;
function GetObjectSize(AObj: PPerfObjectType): Cardinal;
function GetObject(AData: PPerfDataBlock; AIndex: Integer): PPerfObjectType; overload;
function GetObject(AHeader: PPerfLibHeader; AIndex: Integer): PPerfObjectType; overload;
function GetObjectByNameIndex(AData: PPerfDataBlock; ANameIndex: Cardinal): PPerfObjectType; overload;
function GetObjectByNameIndex(AHeader: PPerfLibHeader; ANameIndex: Cardinal): PPerfObjectType; overload;
function GetPerformanceData(const ARegValue: string): PPerfDataBlock;
function GetProcessInstance(AObj: PPerfObjectType; AProcessID: Cardinal): PPerfInstanceDefinition;
function GetSimpleCounterValue32(AObjIndex, ACtrIndex: Integer): Cardinal;
function GetSimpleCounterValue64(AObjIndex, ACtrIndex: Integer): UInt64;

function GetProcessName(AProcessID: Cardinal): WideString;
function GetProcessPercentProcessorTime(AProcessID: Cardinal; AData1, AData2: PPerfDataBlock;
  AProcessorCount: Integer = -1): Double;
function GetProcessPrivateBytes(AProcessID: Cardinal): UInt64;
function GetProcessThreadCount(AProcessID: Cardinal): Cardinal;
function GetProcessVirtualBytes(AProcessID: Cardinal): UInt64;
function GetProcessorCount: Integer;
function GetSystemProcessCount: Cardinal;
function GetSystemUpTime: TDateTime;

var
  PerfFrequency: Int64 = 0;

const
  // perfdisk.dll
  ObjPhysicalDisk = 234;
  ObjLogicalDisk = 236;
  // perfnet.dll
  ObjBrowser = 52;
  ObjRedirector = 262;
  ObjServer = 330;
  ObjServerWorkQueues = 1300;
  // perfos.dll
  ObjSystem = 2;
    CtrProcesses = 248;
    CtrSystemUpTime = 674;
  ObjMemory = 4;
  ObjCache = 86;
  ObjProcessor = 238;
  ObjObjects = 260;
  ObjPagingFile = 700;
  // perfproc.dll
  ObjProcess = 230;
    CtrPercentProcessorTime = 6;
    CtrVirtualBytes = 174;
    CtrPrivateBytes = 186;
    CtrThreadCount = 680;
    CtrIDProcess = 784;
  ObjThread = 232;
  ObjProcessAddressSpace = 786;
  ObjImage = 740;
  ObjThreadDetails = 816;
  ObjFullImage = 1408;
  ObjJobObject = 1500;
  ObjJobObjectDetails = 1548;
  ObjHeap = 1760;
  // winspool.drv
  ObjPrintQueue = 1450;
  // tapiperf.dll
  ObjTelephony = 1150;
  // perfctrs.dll
  ObjNBTConnection = 502;
  ObjNetworkInterface = 510;
  ObjIP = 546;
  ObjICMP = 582;
  ObjTCP = 638;
  ObjUDP = 658;

implementation

function GetCounterBlock(AObj: PPerfObjectType): PPerfCounterBlock;
begin
  if Assigned(AObj) and (AObj^.NumInstances=PERF_NO_INSTANCES) then
    {$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(Result):={$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(AObj)+SizeOf(TPerfObjectType)+(AObj^.NumCounters * SizeOf(TPerfCounterDefinition))
  else
    Result:=nil;
end;

function GetCounterBlock(AInstance: PPerfInstanceDefinition): PPerfCounterBlock;
begin
  if Assigned(AInstance) then
    {$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(Result):={$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(AInstance)+AInstance^.ByteLength
  else
    Result:=nil;
end;

function GetCounterDataAddress(AObj: PPerfObjectType; ACounter: PPerfCounterDefinition;
  AInstance: PPerfInstanceDefinition=nil): Pointer;
var
  Block: PPerfCounterBlock;
begin
  Result:=nil;
  if not Assigned(AObj) or not Assigned(ACounter) then
    Exit;

  if AObj^.NumInstances=PERF_NO_INSTANCES then
    Block:=GetCounterBlock(AObj)
  else
  begin
    if not Assigned(AInstance) then
      Exit;

    Block:=GetCounterBlock(AInstance);
  end;

  if not Assigned(Block) then
    Exit;

  {$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(Result):={$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(Block)+ACounter^.CounterOffset;
end;

function GetCounterDataAddress(AObj: PPerfObjectType; ACounter, AInstance: Integer): Pointer;
begin
  Result:=nil;
  if not Assigned(AObj) or (ACounter<0) or (Cardinal(ACounter)>AObj^.NumCounters-1) then
    Exit;

  if AObj^.NumInstances=PERF_NO_INSTANCES then
  begin
    if AInstance <> -1 then
      Exit;
  end
  else
  begin
    if (AInstance>0) or (AInstance>AObj^.NumInstances-1) then
      Exit;
  end;

  Result:=GetCounterDataAddress(AObj, GetCounter(AObj, ACounter), GetInstance(AObj, AInstance));
end;

function GetCounter(AObj: PPerfObjectType; AIndex: Integer): PPerfCounterDefinition;
var
  I: Integer;
begin
  if Assigned(AObj) and (AIndex >= 0) and (Cardinal(AIndex) <= AObj^.NumCounters-1) then begin
    Result:=GetFirstCounter(AObj);
    if not Assigned(Result) then
      Exit;

    for I:=0 to AIndex-1 do begin
      Result:=GetNextCounter(Result);
      if not Assigned(Result) then
        Exit;
    end;
  end else
    Result:=nil;
end;

function GetCounterByNameIndex(AObj: PPerfObjectType; ANameIndex: Cardinal): PPerfCounterDefinition;
var
  Counter: PPerfCounterDefinition;
  I: Integer;
begin
  Result:=nil;

  Counter:=GetFirstCounter(AObj);
  for I:=0 to AObj^.NumCounters-1 do begin
    if not Assigned(Counter) then
      Exit;

    if Counter^.CounterNameTitleIndex=ANameIndex then begin
      Result:=Counter;
      Break;
    end;

    Counter:=GetNextCounter(Counter);
  end;
end;

function GetCounterValue32(AObj: PPerfObjectType; ACounter: PPerfCounterDefinition;
  AInstance: PPerfInstanceDefinition=nil): Cardinal;
var
  DataAddr: Pointer;
begin
  Result:=0;

  DataAddr:=GetCounterDataAddress(AObj, ACounter, AInstance);
  if not Assigned(DataAddr) then
    Exit;

  if ACounter^.CounterType and $00000300=PERF_SIZE_DWORD then // 32-bit value
    case ACounter^.CounterType and $00000C00 of // counter type
      PERF_TYPE_NUMBER, PERF_TYPE_COUNTER:
        Result:=PCardinal(DataAddr)^;
    end;
end;

function GetCounterValue64(AObj: PPerfObjectType; ACounter: PPerfCounterDefinition;
  AInstance: PPerfInstanceDefinition=nil): UInt64;
var
  DataAddr: Pointer;
begin
  Result:=0;

  DataAddr:=GetCounterDataAddress(AObj, ACounter, AInstance);
  if not Assigned(DataAddr) then
    Exit;

  if ACounter^.CounterType and $00000300=PERF_SIZE_LARGE then // 64-bit value
    case ACounter^.CounterType and $00000C00 of // counter type
      PERF_TYPE_NUMBER, PERF_TYPE_COUNTER:
        Result:=Uint64(PInt64(DataAddr)^);
    end;
end;

function GetCounterValueText(AObj: PPerfObjectType; ACounter: PPerfCounterDefinition;
  AInstance: PPerfInstanceDefinition=nil): PChar;
var
  DataAddr: Pointer;
begin
  Result:=nil;

  DataAddr:=GetCounterDataAddress(AObj, ACounter, AInstance);
  if not Assigned(DataAddr) then
    Exit;

  if ACounter^.CounterType and $00000300=PERF_SIZE_VARIABLE_LEN then // variable-length value
    if (ACounter^.CounterType and $00000C00=PERF_TYPE_TEXT) and
      (ACounter^.CounterType and $00010000=PERF_TEXT_ASCII) then
      Result:=PChar(DataAddr);
end;

function GetCounterValueWideText(AObj: PPerfObjectType; ACounter: PPerfCounterDefinition;
  AInstance: PPerfInstanceDefinition=nil): PWideChar;
var
  DataAddr: Pointer;
begin
  Result:=nil;

  DataAddr:=GetCounterDataAddress(AObj, ACounter, AInstance);
  if not Assigned(DataAddr) then
    Exit;

  if ACounter^.CounterType and $00000300=PERF_SIZE_VARIABLE_LEN then // variable-length value
    if (ACounter^.CounterType and $00000C00=PERF_TYPE_TEXT) and
      (ACounter^.CounterType and $00010000=PERF_TEXT_UNICODE) then
      Result:=PWideChar(DataAddr);
end;

function GetFirstCounter(AObj: PPerfObjectType): PPerfCounterDefinition;
begin
  if Assigned(AObj) then
    {$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(Result):={$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(AObj)+AObj^.HeaderLength
  else
    Result:=nil;
end;

function GetFirstInstance(AObj: PPerfObjectType): PPerfInstanceDefinition;
begin
  if not Assigned(AObj) or (AObj^.NumInstances=PERF_NO_INSTANCES) then
    Result:=nil
  else
    {$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(Result):={$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(AObj)+SizeOf(TPerfObjectType)+(AObj^.NumCounters * SizeOf(TPerfCounterDefinition));
end;

function GetFirstObject(AData: PPerfDataBlock): PPerfObjectType; overload;
begin
  if Assigned(AData) then
    {$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(Result):={$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(AData)+AData^.HeaderLength
  else
    Result:=nil;
end;

function GetFirstObject(AHeader: PPerfLibHeader): PPerfObjectType; overload;
begin
  if Assigned(AHeader) then
    {$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(Result):={$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(AHeader)+SizeOf(TPerfLibHeader)
  else
    Result:=nil;
end;

function GetInstance(AObj: PPerfObjectType; AIndex: Integer): PPerfInstanceDefinition;
var
  I: Integer;
begin
  if Assigned(AObj) and (AIndex>=0) and (AIndex<=AObj^.NumInstances-1) then
  begin
    Result:=GetFirstInstance(AObj);
    if not Assigned(Result) then
      Exit;

    for I:=0 to AIndex-1 do
    begin
      Result:=GetNextInstance(Result);
      if not Assigned(Result) then
        Exit;
    end;
  end
  else
    Result:=nil;
end;

function GetInstanceName(AInstance: PPerfInstanceDefinition): PWideChar;
begin
  if Assigned(AInstance) then
    {$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(Result):={$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(AInstance)+AInstance^.NameOffset
  else
    Result:=nil;
end;

function GetNextCounter(ACounter: PPerfCounterDefinition): PPerfCounterDefinition;
begin
  if Assigned(ACounter) then
    {$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(Result):={$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(ACounter)+ACounter^.ByteLength
  else
    Result:=nil;
end;

function GetNextInstance(AInstance: PPerfInstanceDefinition): PPerfInstanceDefinition;
var
  Block: PPerfCounterBlock;
begin
  Block:=GetCounterBlock(AInstance);
  if Assigned(Block) then
    {$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(Result):={$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(Block)+Block^.ByteLength
  else
    Result:=nil;
end;

function GetNextObject(AObj: PPerfObjectType): PPerfObjectType;
begin
  if Assigned(AObj) then
    {$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(Result):={$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(AObj)+AObj^.TotalByteLength
  else
    Result:=nil;
end;

function GetObjectSize(AObj: PPerfObjectType): Cardinal;
var
  I: Integer;
  Instance: PPerfInstanceDefinition;
begin
  Result:=0;

  if Assigned(AObj) then
  begin
    if AObj^.NumInstances=PERF_NO_INSTANCES then
      Result:=AObj^.TotalByteLength
    else
    begin
      Instance:=GetFirstInstance(AObj);
      if not Assigned(Instance) then
        Exit;

      for I:=0 to AObj^.NumInstances-1 do
      begin
        Instance:=GetNextInstance(Instance);
        if not Assigned(Instance) then
          Exit;
      end;

      Result:=Cardinal(Instance)-Cardinal(AObj);
    end;
  end;
end;

function GetObject(AData: PPerfDataBlock; AIndex: Integer): PPerfObjectType;
var
  I: Integer;
begin
  if Assigned(AData) and (AIndex>=0) and (Cardinal(AIndex)<=AData^.NumObjectTypes-1) then
  begin
    Result:=GetFirstObject(AData);
    if not Assigned(Result) then
      Exit;

    for I:=0 to AIndex-1 do
    begin
      Result:=GetNextObject(Result);
      if not Assigned(Result) then
        Exit;
    end;
  end
  else
    Result:=nil;
end;

function GetObject(AHeader: PPerfLibHeader; AIndex: Integer): PPerfObjectType;
var
  I: Integer;
begin
  if Assigned(AHeader) and (AIndex>=0) then begin
    Result:=GetFirstObject(AHeader);
    if not Assigned(Result) then
      Exit;

    for I:=0 to AIndex-1 do begin
      Result:=GetNextObject(Result);
      if not Assigned(Result) then
        Exit;
    end;
  end
  else
    Result:=nil;
end;

function GetObjectByNameIndex(AData: PPerfDataBlock; ANameIndex: Cardinal): PPerfObjectType;
var
  AObj: PPerfObjectType;
  I: Integer;
begin
  Result:=nil;

  AObj:=GetFirstObject(AData);
  for I:=0 to AData^.NumObjectTypes-1 do begin
    if not Assigned(AObj) then
      Exit;

    if AObj^.ObjectNameTitleIndex=ANameIndex then begin
      Result:=AObj;
      Break;
    end;

    AObj:=GetNextObject(AObj);
  end;
end;

function GetObjectByNameIndex(AHeader: PPerfLibHeader; ANameIndex: Cardinal): PPerfObjectType; overload;
var
  AObj: PPerfObjectType;
  I: Integer;
begin
  Result:=nil;

  AObj:=GetFirstObject(AHeader);
  for I:=0 to AHeader^.ObjectCount-1 do begin
    if not Assigned(AObj) then
      Exit;

    if AObj^.ObjectNameTitleIndex=ANameIndex then begin
      Result:=AObj;
      Break;
    end;

    AObj:=GetNextObject(AObj);
  end;
end;

function GetPerformanceData(const ARegValue: string): PPerfDataBlock;
const
  BufSizeInc = 4096;
var
  BufSize, RetVal: Cardinal;
begin
  BufSize:=BufSizeInc;
  Result:=AllocMem(BufSize);
  try
    RetVal:=RegQueryValueEx(HKEY_PERFORMANCE_DATA, PChar(ARegValue), nil, nil, PByte(Result), @BufSize);
    try
      repeat
        case RetVal of
          ERROR_SUCCESS:
            Break;
          ERROR_MORE_DATA:
            begin
              Inc(BufSize, BufSizeInc);
              ReallocMem(Result, BufSize);
              RetVal:=RegQueryValueEx(HKEY_PERFORMANCE_DATA, PChar(ARegValue), nil, nil, PByte(Result), @BufSize);
            end;
          else
            RaiseLastOSError;
        end;
      until False;
    finally
      RegCloseKey(HKEY_PERFORMANCE_DATA);
    end;
  except
    FreeMem(Result);
    raise;
  end;
end;

function GetProcessInstance(AObj: PPerfObjectType; AProcessID: Cardinal): PPerfInstanceDefinition;
var
  Counter: PPerfCounterDefinition;
  Instance: PPerfInstanceDefinition;
  Block: PPerfCounterBlock;
  I: Integer;
begin
  Result:=nil;

  Counter:=GetCounterByNameIndex(AObj, CtrIDProcess);
  if not Assigned(Counter) then
    Exit;

  Instance:=GetFirstInstance(AObj);
  for I:=0 to AObj^.NumInstances-1 do begin
    Block:=GetCounterBlock(Instance);
    if not Assigned(Block) then
      Exit;

    if PCardinal(Cardinal(Block)+Counter^.CounterOffset)^=AProcessID then begin
      Result:=Instance;
      Break;
    end;

    Instance:=GetNextInstance(Instance);
  end;
end;

function GetSimpleCounterValue32(AObjIndex, ACtrIndex: Integer): Cardinal;
var
  Data: PPerfDataBlock;
  AObj: PPerfObjectType;
  Counter: PPerfCounterDefinition;
begin
  Result:=0;

  Data:=GetPerformanceData(IntToStr(AObjIndex));
  try
    AObj:=GetObjectByNameIndex(Data, AObjIndex);
    if not Assigned(AObj) then
      Exit;

    Counter:=GetCounterByNameIndex(AObj, ACtrIndex);
    if not Assigned(Counter) then
      Exit;

    Result:=GetCounterValue32(AObj, Counter);
  finally
    FreeMem(Data);
  end;
end;

function GetSimpleCounterValue64(AObjIndex, ACtrIndex: Integer): UInt64;
var
  Data: PPerfDataBlock;
  AObj: PPerfObjectType;
  Counter: PPerfCounterDefinition;
begin
  Result:=0;

  Data:=GetPerformanceData(IntToStr(AObjIndex));
  try
    AObj:=GetObjectByNameIndex(Data, AObjIndex);
    if not Assigned(AObj) then
      Exit;

    Counter:=GetCounterByNameIndex(AObj, ACtrIndex);
    if not Assigned(Counter) then
      Exit;

    Result:=GetCounterValue64(AObj, Counter);
  finally
    FreeMem(Data);
  end;
end;

function GetProcessName(AProcessID: Cardinal): WideString;
var
  Data: PPerfDataBlock;
  AObj: PPerfObjectType;
  Instance: PPerfInstanceDefinition;
begin
  Result:='';

  Data:=GetPerformanceData(IntToStr(ObjProcess));
  try
    AObj:=GetObjectByNameIndex(Data, ObjProcess);
    if not Assigned(AObj) then
      Exit;

    Instance:=GetProcessInstance(AObj, AProcessID);
    if not Assigned(Instance) then
      Exit;

    Result:=GetInstanceName(Instance);
  finally
    FreeMem(Data);
  end;
end;

function GetProcessPercentProcessorTime(AProcessID: Cardinal; AData1, AData2: PPerfDataBlock;
  AProcessorCount: Integer): Double;
var
  Value1, Value2: UInt64;

  function GetValue(Data: PPerfDataBlock): UInt64;
  var
    AObj: PPerfObjectType;
    Instance: PPerfInstanceDefinition;
    Counter: PPerfCounterDefinition;
  begin
    Result:=0;

    AObj:=GetObjectByNameIndex(Data, ObjProcess);
    if not Assigned(AObj) then
      Exit;
    Counter:=GetCounterByNameIndex(AObj, CtrPercentProcessorTime);
    if not Assigned(Counter) then
      Exit;
    Instance:=GetProcessInstance(AObj, AProcessID);
    if not Assigned(Instance) then
      Exit;

    Result:=GetCounterValue64(AObj, Counter, Instance);
  end;
begin
  if AProcessorCount=-1 then
    AProcessorCount:=GetProcessorCount;

  Value1:=GetValue(AData1);
  Value2:=GetValue(AData2);

  Result:=100*(Value2-Value1)/(AData2^.PerfTime100nSec.QuadPart-AData1^.PerfTime100nSec.QuadPart)/AProcessorCount;
end;

function GetProcessPrivateBytes(AProcessID: Cardinal): UInt64;
var
  Data: PPerfDataBlock;
  AObj: PPerfObjectType;
  Instance: PPerfInstanceDefinition;
  Counter: PPerfCounterDefinition;
begin
  Result:=0;

  Data:=GetPerformanceData(IntToStr(ObjProcess));
  try
    AObj:=GetObjectByNameIndex(Data, ObjProcess);
    if not Assigned(AObj) then
      Exit;

    Counter:=GetCounterByNameIndex(AObj, CtrPrivateBytes);
    if not Assigned(Counter) then
      Exit;

    Instance:=GetProcessInstance(AObj, AProcessID);
    if not Assigned(Instance) then
      Exit;

    Result:=GetCounterValue64(AObj, Counter, Instance);
  finally
    FreeMem(Data);
  end;
end;

function GetProcessThreadCount(AProcessID: Cardinal): Cardinal;
var
  Data: PPerfDataBlock;
  AObj: PPerfObjectType;
  Instance: PPerfInstanceDefinition;
  Counter: PPerfCounterDefinition;
begin
  Result:=0;

  Data:=GetPerformanceData(IntToStr(ObjProcess));
  try
    AObj:=GetObjectByNameIndex(Data, ObjProcess);
    if not Assigned(AObj) then
      Exit;

    Counter:=GetCounterByNameIndex(AObj, CtrThreadCount);
    if not Assigned(Counter) then
      Exit;

    Instance:=GetProcessInstance(AObj, AProcessID);
    if not Assigned(Instance) then
      Exit;

    Result:=GetCounterValue32(AObj, Counter, Instance);
  finally
    FreeMem(Data);
  end;
end;

function GetProcessVirtualBytes(AProcessID: Cardinal): UInt64;
var
  Data: PPerfDataBlock;
  AObj: PPerfObjectType;
  Instance: PPerfInstanceDefinition;
  Counter: PPerfCounterDefinition;
begin
  Result:=0;

  Data:=GetPerformanceData(IntToStr(ObjProcess));
  try
    AObj:=GetObjectByNameIndex(Data, ObjProcess);
    if not Assigned(AObj) then
      Exit;

    Counter:=GetCounterByNameIndex(AObj, CtrVirtualBytes);
    if not Assigned(Counter) then
      Exit;

    Instance:=GetProcessInstance(AObj, AProcessID);
    if not Assigned(Instance) then
      Exit;

    Result:=GetCounterValue64(AObj, Counter, Instance);
  finally
    FreeMem(Data);
  end;
end;

function GetProcessorCount: Integer;
var
  Data: PPerfDataBlock;
  AObj: PPerfObjectType;
begin
  Result:=-1;

  Data:=GetPerformanceData(IntToStr(ObjProcessor));
  try
    AObj:=GetFirstObject(Data);
    if not Assigned(AObj) then
      Exit;

    Result:=AObj^.NumInstances;
    if Result>1 then // disregard the additional '_Total' instance
      Dec(Result);
  finally
    FreeMem(Data);
  end;
end;

function GetSystemProcessCount: Cardinal;
begin
  Result:=GetSimpleCounterValue32(ObjSystem, CtrProcesses);
end;

function GetSystemUpTime: TDateTime;
const
  SecsPerDay = 60 * 60 * 24;
var
  Data: PPerfDataBlock;
  AObj: PPerfObjectType;
  Counter: PPerfCounterDefinition;
  SecsStartup: UInt64;
begin
  Result:=0;

  Data:=GetPerformanceData(IntToStr(ObjSystem));
  try
    AObj:=GetObjectByNameIndex(Data, ObjSystem);
    if not Assigned(AObj) then
      Exit;

    Counter:=GetCounterByNameIndex(AObj, CtrSystemUpTime);
    if not Assigned(Counter) then
      Exit;

    SecsStartup:=GetCounterValue64(AObj, Counter);
    // subtract from snapshot time and divide by base frequency and number of seconds per day
    // to get a TDateTime representation
    Result:=(UInt64(AObj^.PerfTime.QuadPart)-SecsStartup)/Uint64(AObj^.PerfFreq.QuadPart)/SecsPerDay;
  finally
    FreeMem(Data);
  end;
end;

initialization
  QueryPerformanceFrequency(PerfFrequency);

finalization

end.
