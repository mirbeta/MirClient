{*******************************************************}
{                MiTeC Common Routines                  }
{                  WMI routines                         }
{                                                       }
{         Copyright (c) 1997-2019 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_WMI;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.Messages, System.Variants,
     {$ELSE}
     Windows, SysUtils, Classes, Variants, Messages,
     {$ENDIF}
     {$IFDEF FPC}MiTeC_FPC_WbemScripting_TLB{$ELSE}MiTeC_WbemScripting_TLB{$ENDIF};

const
  RootNameSpace = 'root\CIMV2';
  SCNameSpace = 'root\SecurityCenter';
  SC2NameSpace = 'root\SecurityCenter2';
  ArrayDelimiter = '|';

type
  TInstanceProperty = record
    Name,
    Value: string;
  end;

  TInstanceProperties = array of TInstanceProperty;

  TInstances = array of TInstanceProperties;

function GetInstancePropertyValue(AList: TInstances; APropertyName: string; AInstanceIndex: Integer = 0; AArrayIndex: Integer = 0): string;
function WmiGetPropStr(wmiProp: ISWbemProperty): string;
function WmiGetTypeStr(wmiProp: ISWbemProperty): string;
function WmiConnect(AMachine, AUser, APwd, ARoot: string; var AServices: ISWbemServices): Boolean;
procedure WmiDisconnect(var AServices: ISWbemServices);
function WmiCommand(AServices: ISWbemServices; AQuery: string; var AList: TInstances): Integer;
procedure WmiSaveToStorage(AFolderName, AFilename: string; AWMI: TInstances);
procedure WMIRemoteExecute(AMachine,AUser,APwd,ACommandline: string; var APID: integer);
function WMIGetCommandLine(APID: Cardinal): string;

implementation

uses {$IFDEF RAD9PLUS}
     WinAPI.ActiveX, System.Win.ComObj,
     {$ELSE}
     ActiveX, ComObj,
     {$ENDIF}
     MiTeC_SS, MiTeC_StrUtils;

function GetInstancePropertyValue;
var
  i: Integer;
begin
  Result:='';
  if not Assigned(AList) then
    Exit;
  try
    for i:=0 to High(AList[AInstanceIndex]) do
      if SameText(APropertyName,AList[AInstanceIndex][i].Name) then begin
        Result:=AList[AInstanceIndex][i].Value;
        if Pos(ArrayDelimiter,Result)>0 then
          Result:=ExtractWord(AArrayIndex+1,Result,[ArrayDelimiter]);
        Result:=Trim(Result);
        Break;
      end;
  except
  end;
end;

function WmiGetTypeStr(wmiProp: ISWbemProperty): string;
begin
  Result:='<unknown>';
  case wmiProp.CIMType of
    wbemCimtypeSint8: Result:='sint8';
    wbemCimtypeUint8: Result:='uint8';
    wbemCimtypeSint16: Result:='sint16';
    wbemCimtypeUint16: Result:='uint16';
    wbemCimtypeSint32: Result:='sint32';
    wbemCimtypeUint32: Result:='uint32';
    wbemCimtypeSint64: Result:='sint64';
    wbemCimtypeReal32: Result:='real32';
    wbemCimtypeReal64: Result:='real64';
    wbemCimtypeBoolean: Result:='boolean';
    wbemCimtypeString: Result:='string';
    wbemCimtypeUint64: Result:='uint64';
    wbemCimtypeDatetime: Result:='datetime';
    wbemCimtypeReference: Result:='reference';
    wbemCimtypeChar16: Result:='char16';
    wbemCimtypeObject: Result:='object';
  end;
  if wmiProp.IsArray then
    Result:='array of '+Result;
end;

function WmiGetPropStr(wmiProp: ISWbemProperty): string;
var
  i: integer;
begin
  Result:='';
  if VarIsNull(wmiProp.Get_Value) then
    Result:='NULL'
  else begin
    case wmiProp.CIMType of
      wbemCimtypeSint8, wbemCimtypeUint8, wbemCimtypeSint16,
      wbemCimtypeUint16, wbemCimtypeSint32, wbemCimtypeUint32,
      wbemCimtypeSint64:
        if VarIsArray(wmiProp.Get_Value) then begin
          for i:=0 to VarArrayHighBound(wmiProp.Get_Value, 1) do begin
            if i>0 then
              Result:=Result+';' ;
            Result:=Result+IntToStr(wmiProp.Get_Value[i]) ;
          end ;
        end else
          Result:=IntToStr(wmiProp.Get_Value);
      wbemCimtypeReal32, wbemCimtypeReal64:
        Result:=FloatToStr(wmiProp.Get_Value);
      wbemCimtypeBoolean:
        if wmiProp.Get_Value then
          Result:='True'
        else
          Result:='False';
      wbemCimtypeString, wbemCimtypeUint64:
        if VarIsArray(wmiProp.Get_Value) then begin
          for i:=0 to VarArrayHighBound(wmiProp.Get_Value,1) do begin
            if i>0 then
              Result:=Result+ArrayDelimiter;
            Result:=Result+wmiProp.Get_Value[i];
          end;
          SetLength(Result,Length(Result)-1);
        end else
          Result:=wmiProp.Get_Value;
      wbemCimtypeDatetime:
        Result:=wmiProp.Get_Value;
      wbemCimtypeReference:
        Result:=wmiProp.Get_Value;
      wbemCimtypeChar16:
        Result:='<16-bit character>';
      wbemCimtypeObject:
        Result:='<CIM Object>';
    end;
  end;
end;

function WmiConnect(AMachine, AUser, APwd, ARoot: string; var AServices: ISWbemServices): Boolean;
var
  Locator: {$IFDEF FPC}ISWbemLocator{$ELSE}TSWbemLocator{$ENDIF};
begin
  Result:=False;
  try
    Locator:={$IFDEF FPC}CoSWbemLocator.Create{$ELSE}TSWbemLocator.Create(nil){$ENDIF};
    try
      if CoInitialize(nil) in [S_OK, S_FALSE] then begin
        AServices:=Locator.ConnectServer(AMachine,ARoot,AUser,APwd,'','',0,nil);
        Result:=True;
      end;
    finally
      {$IFNDEF FPC}
      if Assigned(Locator) then
        FreeAndNil(Locator);
      {$ENDIF}
    end;
  except
  end;
end;

function WmiCommand(AServices: ISWbemServices; AQuery: string; var AList: TInstances): Integer;
var
  i,j: Integer;
  wmiObjectSet: ISWbemObjectSet;
  wmiObject: ISWbemObject;
  wmiProp: ISWbemProperty;
  propSet: ISWbemPropertySet;
  v,v1: OleVariant;
  n,n1: LongWord;
  propEnum,Enum: IEnumVariant;
begin
  Result:=-1;
  Finalize(AList);
  if not Assigned(AServices) then
    Exit;

  if Pos('SELECT',Uppercase(AQuery))=1 then
    wmiObjectSet:=AServices.ExecQuery(AQuery,'WQL',wbemFlagReturnImmediately,nil)
  else
    wmiObjectSet:=AServices.InstancesOf(AQuery,wbemFlagReturnImmediately or wbemQueryFlagShallow,nil);
  Result:=wmiObjectSet.Count;
  if Result=0 then
    Exit;
  SetLength(AList,Result);
  Enum:=(wmiObjectSet._NewEnum) as IEnumVariant;
  i:=0;
  while (Enum.Next(1,v,n)=S_OK) do begin
    wmiObject:=IUnknown(v) as SWBemObject;
    propSet:=wmiObject.Properties_;
    Setlength(AList[i],propSet.Count);
    propEnum:=(propSet._NewEnum) as IEnumVariant;
    j:=0;
    while (propEnum.Next(1,v1,n1)=S_OK) do begin
      wmiProp:=IUnknown(v1) as SWBemProperty;
      AList[i][j].Name:=wmiProp.Name;
      Alist[i][j].Value:=WmiGetPropStr(wmiProp);
      Inc(j);
      VarClear(v1);
    end;
    Inc(i);
    VarClear(v);
  end;
end;

procedure WMIDisconnect(var AServices: ISWbemServices);
begin
  try
    AServices:=nil;
    //CoUninitialize;
  except
  end;
end;

procedure WmiSaveToStorage;
var
  stg: IStorage;
  SS, Sub: TStructuredStorage;

procedure WriteToPS(AIndex: Integer);
var
  S: TStructuredStorage;
  SPS: TStoragePropertySet;
  i: integer;
begin
  S:=Sub.OpenSubStorage(IntToStr(AIndex),STG_OPEN,True);
  SPS:=S.OpenPropertySet(StringToGUID(FMTID_DocSummaryInformation),STG_OPEN,True);
  try
    for i:=0 to High(AWMI[AIndex]) do
      WritestringProperty(SPS._IPropertyStorage,AWMI[AIndex][i].Name,AWMI[AIndex][i].Value);
  finally
    SPS.Free;
    S.Free;
  end;
end;

var
  i: Integer;
begin
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(AFolderName);
    Sub:=SS.OpenSubStorage(AFolderName,STG_OPEN,True);
    try
      for i:=0 to High(AWMI) do begin
        WriteToPS(i);
      end;
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure WMIRemoteExecute(AMachine,AUser,APwd,ACommandline: string; var APID: integer);
var
  Locator: ISWbemLocator;
  Services: ISWbemServices;
  obj: OLEVariant;
  objProcess: OLEVariant;
  objConfig: OLEVariant;
begin
  Locator:=CoSWbemLocator.Create;
  try
    if Locator<>nil then begin
      Services:=Locator.ConnectServer(AMachine,'root\cimv2',AUser,APwd,'','',wbemConnectFlagUseMaxWait,nil);
      if Services<>nil then begin
        Services.Security_.ImpersonationLevel:=wbemImpersonationLevelImpersonate;
        obj:=Services.Get('Win32_ProcessStartup',0,nil);
        objConfig:=obj.SpawnInstance_;
        objConfig.WinstationDesktop:='Winsta0\default';
        objConfig.ShowWindow:=SW_SHOWDEFAULT;
        //objConfig.PriorityClass:=128;
        objProcess:=Services.Get('Win32_Process',0,nil);
        objProcess.Create(ACommandLine,null,objConfig,APID);
      end;
    end;
  finally
    Services:=nil;
    Locator:=nil;
  end;
end;

function WMIGetCommandLine(APID: Cardinal): string;
var
  wmi: TInstances;
  Services: ISWbemServices;
begin
  Result:='';
  if WMIConnect('','','',Rootnamespace,Services) then begin
    if WMICommand(Services,Format('select * from Win32_Process where ProcessId=%d',[APID]),wmi)>0 then begin
      Result:=GetInstancePropertyValue(wmi,'CommandLine');
      if SameText(Result,'null') then
        Result:=GetInstancePropertyValue(wmi,'ExecutablePath');
      if SameText(Result,'null') then
        Result:='';
      Finalize(wmi);
    end;
    WMIDisconnect(Services);
  end;
end;

end.



