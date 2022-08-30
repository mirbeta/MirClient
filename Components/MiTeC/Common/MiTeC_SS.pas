{*******************************************************}
{                MiTeC Common Routines                  }
{             Structured Storage Interface              }
{                                                       }
{                                                       }
{       Copyright (c) 1997-2019 Michal Mutl             }
{                                                       }
{*******************************************************}

{$I Compilers.inc}

unit MiTeC_SS;

interface
uses {$IFDEF RAD9PLUS}
     System.Win.ComObj, WinAPI.ActiveX, System.SysUtils, WinAPI.Windows, System.Classes,
     {$ELSE}
     ComObj, ActiveX, SysUtils, Windows, Classes,
     {$ENDIF}
     {$IFNDEF UNICODE}
     MiTeC_WideStrings,
     {$ENDIF}
     MiTeC_Datetime;

const
  ole32 = 'ole32.dll';
  oleaut32 = 'oleaut32.dll';
  olepro32 = 'olepro32.dll';

  STG_READ_INSTORAGE = STGM_DIRECT or STGM_READ or STGM_SHARE_EXCLUSIVE;
  STG_READ_STORAGE_FILE = STGM_DIRECT or STGM_READ or STGM_SHARE_DENY_WRITE;
  STG_CREATE_OPEN = STGM_DIRECT or STGM_CREATE or STGM_READWRITE or STGM_SHARE_EXCLUSIVE;
  STG_OPEN = STGM_DIRECT or STGM_READWRITE or STGM_SHARE_EXCLUSIVE;

  STGC_CONSOLIDATE = 8;

  ChunkSize: Longint = 8192;

  NULL_GUID = '{00000000-0000-0000-0000-000000000000}';
  FMTID_SummaryInformation = '{F29F85E0-4FF9-1068-AB91-08002B27B3D9}';
  FMTID_DocSummaryInformation = '{D5CDD502-2E9C-101B-9397-08002B2CF9AE}';
  FMTID_UserDefinedProperties = '{D5CDD505-2E9C-101B-9397-08002B2CF9AE}';
  FMTID_ImageSummaryInformation = '{6444048F-4C8B-11D1-8B70-080036B11A03}';
  FMTID_InternetSite = '{000214A1-0000-0000-C000-000000000046}';
  FMTID_Music = '{56A3372E-CE9C-11D2-9F0E-006097C686F6}';
  FMTID_Audio = '{64440490-4C8B-11D1-8B70-080036B11A03}';
  FMTID_Video = '{64440491-4C8B-11D1-8B70-080036B11A03}';
  FMTID_MediaFile = '{64440492-4C8B-11D1-8B70-080036B11A03}';
  IID_IPropertySetStorage = '{0000013A-0000-0000-C000-000000000046}';
  IID_IStorage: TGUID  = '{0000000B-0000-0000-C000-000000000046}';


// SummaryInformation
  PIDSI_TITLE  = $00000002;
  PIDSI_SUBJECT  = $00000003;
  PIDSI_AUTHOR  = $00000004;
  PIDSI_KEYWORDS  = $00000005;
  PIDSI_COMMENTS  = $00000006;
  PIDSI_TEMPLATE  = $00000007;
  PIDSI_LASTAUTHOR  = $00000008;
  PIDSI_REVNUMBER  = $00000009;
  PIDSI_EDITTIME  = $0000000A;
  PIDSI_LASTPRINTED  = $0000000B;
  PIDSI_CREATE_DTM  = $0000000C;
  PIDSI_LASTSAVE_DTM  = $0000000D;
  PIDSI_PAGECOUNT = $0000000E;
  PIDSI_WORDCOUNT = $0000000F;
  PIDSI_CHARCOUNT = $00000010;
  PIDSI_THUMBNAIL  = $00000011;
  PIDSI_APPNAME  = $00000012;
  PIDSI_SECURITY  = $00000013;

// DocSummaryInformation
  PIDDSI_CATEGORY  = $00000002;
  PIDDSI_PRESFORMAT  = $00000003;
  PIDDSI_BYTECOUNT  = $00000004;
  PIDDSI_LINECOUNT  = $00000005;
  PIDDSI_PARCOUNT  = $00000006;
  PIDDSI_SLIDECOUNT  = $00000007;
  PIDDSI_NOTECOUNT  = $00000008;
  PIDDSI_HIDDENCOUNT  = $00000009;
  PIDDSI_MMCLIPCOUNT  = $0000000A;
  PIDDSI_SCALE  = $0000000B;
  PIDDSI_HEADINGPAIR  = $0000000C;
  PIDDSI_DOCPARTS  = $0000000D;
  PIDDSI_MANAGER  = $0000000E;
  PIDDSI_COMPANY  = $0000000F;
  PIDDSI_LINKSDIRTY  = $00000010;

//{$EXTERNALSYM StgCreatePropSetStg}
//function StgCreatePropSetStg(stg: IStorage; reserved: Cardinal; out stgCreated: IPropertySetStorage): HResult; stdcall;

const
  STGFMT_STORAGE  = 0;
  STGFMT_FILE     = 3;
  STGFMT_ANY      = 4;
  STGFMT_DOCFILE  = 5;

type
  PStgOptions = ^TStgOptions;
  tagSTGOPTIONS = record
    usVersion: Byte;
    reserved: Byte;
    ulSectorSize: DWORD;
    pwcsTemplateFile: POleStr;
  end;
  TStgOptions = tagSTGOPTIONS;

type
  TCodeStreamProcedure = procedure(InStream, OutStream: TStream);
  THeaderWriter = procedure(const AFilename: string; AFormat: Integer; const AComment: string) of object;
  THeaderReader = procedure(const AFilename: string; AHeader: Pointer) of object;

  TPropSet = record
               Name: widestring;
               Props: TStatPropSetStg;
             end;
  TElement = record
               Name: widestring;
               Props: TStatStg;
             end;
  TElements = array of TElement;
  TPropSets = array of TPropSet;

  TStorageStream = class;

  TStoragePropertySet = class;

  TStructuredStorage = class
    FLockCount: integer;
    FStorage: IStorage;
    FParent: TStructuredStorage;
    FAutoFree: Boolean;
    FElements: TElements;
    FPropSets: TPropSets;
  private
    procedure GetPropSets;
    procedure GetElements;
    function GetRoot: boolean;
    function GetElemCount: Cardinal;
    function GetElement(Index: Integer): TElement;
    function GetPropSet(Index: Integer): TPropSet;
    function GetPropSetCount: Cardinal;
    function GetElementByName(Name: string): TElement;
    function GetPropSetByName(Name: string): TPropSet;
    procedure CalculateSize(var Size: Int64);
  public
    constructor Create(AParent: TStructuredStorage; AStorage: IStorage);
    destructor Destroy; override;

    function ElementExists(AName: widestring) : Boolean;
    function CreateSubStorage(AName: widestring; AMode: Integer): TStructuredStorage;
    function OpenSubStorage(AName: widestring; AMode: Integer; CanCreate: Boolean): TStructuredStorage;
    function CreateStream(AName: widestring; AMode: Integer): TStorageStream;
    function OpenStream(AName: widestring; AMode: Integer; CanCreate: Boolean): TStorageStream;
    function CreatePropertySet(AName: string; FmtID, ClassID: TGUID; AFlags: integer; AMode: Integer): TStoragePropertySet;
    function OpenPropertySet(FmtID: TGUID; AMode: Integer; CanCreate: Boolean; AName: string=''): TStoragePropertySet;
    function DeleteElement(AName: widestring): Boolean;
    procedure Close;
    function GetSize: Int64;

    property _IStorage: IStorage read FStorage;
    property Parent: TStructuredStorage read FParent;
    property IsRoot: boolean read GetRoot;
    property Elements[Index: Integer]: TElement read GetElement;
    property PropertySets[Index: Integer]: TPropSet read GetPropSet;
    property ElementByName[Name: string]: TElement read GetElementByName;
    property PropertySetByName[Name: string]: TPropSet read GetPropSetByName;
    property ElementCount: Cardinal read GetElemCount;
    property PropertySetCount: Cardinal read GetPropSetCount;
    property AutoFree: boolean read FAutoFree write FAutoFree;
  end;

  TStorageStream = class(TStream)
  private
    FStream: IStream;
    FParent: TStructuredStorage;
    procedure SetSize64(const NewSize: Int64);
  protected
    procedure SetSize(const NewSize: Int64); override;
    procedure SetSize(NewSize: Longint); override;
    function GetPosition : {$IFDEF DXE8PLUS}uint64{$ELSE}{$IFDEF FPC}QWORD{$ELSE}int64{$ENDIF}{$ENDIF};
    procedure SetPosition(NewPosition: {$IFDEF DXE8PLUS}uint64{$ELSE}{$IFDEF FPC}QWORD{$ELSE}int64{$ENDIF}{$ENDIF});
    function GetSize: int64; override;
  public
    constructor Create(var Stream: IStream; var Storage: TStructuredStorage);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure WriteInteger(Value: Int64);
    function ReadInteger: int64;
    procedure WriteString(Value: string);
    function ReadString: string;
    procedure WriteBoolean(Value: Boolean);
    function ReadBoolean: Boolean;
    procedure LoadFromFile(FileName : string);
    procedure SaveToFile(FileName : string);
    function Clone: TStorageStream;
    procedure Clear;

    property _IStream: IStream read FStream;
    property Position: {$IFDEF DXE8PLUS}uint64{$ELSE}{$IFDEF FPC}QWORD{$ELSE}int64{$ENDIF}{$ENDIF} read GetPosition write SetPosition;
    property Size: Int64 read GetSize write SetSize64;
    property Parent: TStructuredStorage read FParent;
  end;

  TStoragePropertySet = class
  private
    FProps: array of TPropVariant;
    FPS: IPropertyStorage;
    FParent: TStructuredStorage;
    function GetProp(Index: Integer): TPropVariant;
    function GetPropCount: Cardinal;
    //procedure EnumProps;
    function GetStat: STATPROPSETSTG;
  public
    constructor Create(var PropertyStorage: IPropertyStorage; var Storage: TStructuredStorage);
    destructor Destroy; override;

    property _IPropertyStorage: IPropertyStorage read FPS;
    property Parent: TStructuredStorage read FParent;
    property PropertyCount: Cardinal read GetPropCount;
    property Props[Index: Integer]: TPropVariant read GetProp;
    property Stat: STATPROPSETSTG read GetStat;
  end;

procedure WriteStringProperty(PropStg: IPropertyStorage; AName,AValue: string); overload;
procedure WriteDoubleProperty(PropStg: IPropertyStorage; AName: string; AValue: Double); overload;
procedure WriteIntegerProperty(PropStg: IPropertyStorage; AName: string; AValue: Int64); overload;
procedure WriteDateTimeProperty(PropStg: IPropertyStorage; AName: string; AValue: TDatetime); overload;

function ReadStringProperty(PropStg: IPropertyStorage; AName: string): string; overload;
function ReadDoubleProperty(PropStg: IPropertyStorage; AName: string): Double; overload;
function ReadIntegerProperty(PropStg: IPropertyStorage; AName: string): Int64; overload;
function ReadDatetimeProperty(PropStg: IPropertyStorage; AName: string; CorrectToLocal: Boolean = false): TDateTime; overload;

procedure WriteStringProperty(PropStg: IPropertyStorage; AID: Cardinal; AValue: string); overload;
procedure WriteDoubleProperty(PropStg: IPropertyStorage; AID: Cardinal; AValue: Double); overload;
procedure WriteIntegerProperty(PropStg: IPropertyStorage; AID: Cardinal; AValue: Int64); overload;
procedure WriteDateTimeProperty(PropStg: IPropertyStorage; AID: Cardinal; AValue: TDatetime); overload;

function ReadStringProperty(PropStg: IPropertyStorage; AID: Cardinal): string; overload;
function ReadDoubleProperty(PropStg: IPropertyStorage; AID: Cardinal): Double; overload;
function ReadIntegerProperty(PropStg: IPropertyStorage; AID: Cardinal): Int64; overload;
function ReadDatetimeProperty(PropStg: IPropertyStorage; AID: Cardinal; CorrectToLocal: Boolean = False): TDateTime; overload;

function ReadIntProperty(AList: TStrings; AName: string): Int64;
function ReadStrProperty(AList: TStrings; AName: string): string;
function ReadDblProperty(AList: TStrings; AName: string): Double;
function ReadDtProperty(AList: TStrings; AName: string): TDatetime;

procedure WriteIntProperty(AList: TStrings; AName: string; AValue: Int64);
procedure WriteStrProperty(AList: TStrings; AName: string; AValue: string);
procedure WriteDblProperty(AList: TStrings; AName: string; AValue: Double);
procedure WriteDtProperty(AList: TStrings; AName: string; AValue: TDateTime);

procedure LoadFromEncodedStream(AStream: TStream; AList: TStrings; ACodeStream: TCodeStreamProcedure); overload;
procedure LoadFromEncodedStream(AStream: TStream; AOutStream: TStream; ACodeStream: TCodeStreamProcedure); overload;
procedure SaveToEncodedStream(AList: TStrings; AStream: TStream; ACodeStream: TCodeStreamProcedure); overload;
procedure SaveToEncodedStream(AInStream: TStream; AStream: TStream; ACodeStream: TCodeStreamProcedure); overload;

procedure ExtractStructure(APath: widestring; var Filename: widestring; Dirs: {$IFDEF UNICODE} TStrings {$ELSE} TWideStrings {$ENDIF});
function CheckNames(ANames: string): Integer;
function CorrectName(const AName: string): string;
function PropVarTypeString(v: TVarType): string;
function PropVarToStr(v: TPropVariant): string;
procedure StrToPropVar(s: string; var v: TPropVariant);
function PropIDToString(FmtID: TGUID; ID: Cardinal): string;
function FmtIDToString(FmtID: TGUID): string;

function StorageFolderExists(AFilename, ASection: string): Boolean;
function StorageStreamExists(AFilename, AName: string): Boolean;
procedure SaveFileToStorage(AStorageFilename, AFolder, AStream, AFilename: string; ACodeStream: TCodeStreamProcedure = nil);
procedure LoadStreamFromStorage(AStorageFilename, AFolder, AStream: string; AData: TStrings; ACodeStream: TCodeStreamProcedure = nil); overload;
procedure LoadStreamFromStorage(AStorageFilename, AFolder, AStream: string; AOutStream: TStream; ACodeStream: TCodeStreamProcedure = nil); overload;
procedure SaveStreamToStorage(AStorageFilename, AFolder, AStream: string; AData: TStrings; ACodeStream: TCodeStreamProcedure = nil; ADelete: boolean = True); overload;
procedure SaveStreamToStorage(AStorageFilename, AFolder, AStream: string; AData: TStream; ACodeStream: TCodeStreamProcedure = nil; ADelete: boolean = True); overload;
procedure LoadStreamFromStorageAndSave(AStorageFilename, AFolder, AStream, ASaveFilename: string; ACodeStream: TCodeStreamProcedure = nil);
procedure DeleteStorageStream(AFilename,AFolder,AName: string);


function StgCreateStorageEx(pwcsName: POleStr; grfMode: Longint; StgFmt: Longint; grfAttrs: DWORD; pStgOptions: PStgOptions;
                            reserved2: Pointer; riid: TIID; out ppObjectOpen: IStorage): HRESULT; stdcall;
function StgOpenStorageEx(const pwcsName: POleStr; grfMode: LongInt; stgfmt: DWORD; grfAttrs: DWORD; pStgOptions: Pointer; reserved2: Pointer; riid: PGUID; out stgOpen: IStorage): HResult;

function PropVariantClear(pvar: PPropVariant): HResult; stdcall;
function PropVariantInit(pvar: PPropVariant): HResult; stdcall;

function IsEmptyStream(AFilename,AFolder,AName: string): Boolean;

procedure GetFolderStreamNames(AFilename,AFolder: string; AList: TStrings; AIncludeSize: Boolean = False);
function IsFolderEmpty(AFilename,AFolder: string): boolean;

function IsValidStgFile(const AFilename: string): boolean;
procedure ConsolidateFile(const AFilename: string);

implementation

uses {$IFDEF RAD12PLUS}System.AnsiStrings,{$ENDIF}MiTeC_StrUtils;

function StgCreateStorageEx; external ole32 name 'StgCreateStorageEx';
function StgOpenStorageEx; external ole32 name 'StgOpenStorageEx';

function PropVariantClear; external ole32 name 'PropVariantClear';
function PropVariantInit; external ole32 name 'PropVariantInit';

procedure WriteStringProperty(PropStg: IPropertyStorage; AName,AValue: string);
var
  l: integer;
  ps: TPropSpec;
  pv: TPropVariant;
begin
  ps.ulKind:=PRSPEC_LPWSTR;
  l:=Length(AName)+1;
  ps.lpwstr:=AllocMem(l*2);
  ps.lpwstr:=StringToWideChar(AName,ps.lpwstr,l);
  pv.vt:=VT_LPWSTR;
  l:=Length(AValue)+1;
  pv.pwszVal:=AllocMem(l*2);
  pv.pwszVal:=StringToWideChar(AValue,pv.pwszVal,l);
  try
    OleCheck(PropStg.WriteMultiple(1,@ps,@pv,PID_FIRST_USABLE));
  finally
    Freemem(ps.lpwstr);
    Freemem(pv.pwszVal);
  end;
end;

procedure WriteDoubleProperty(PropStg: IPropertyStorage; AName: string; AValue: Double);
var
  l: integer;
  ps: TPropSpec;
  pv: TPropVariant;
begin
  ps.ulKind:=PRSPEC_LPWSTR;
  l:=Length(AName)+1;
  ps.lpwstr:=AllocMem(l*2);
  ps.lpwstr:=StringToWideChar(AName,ps.lpwstr,l);
  pv.vt:=VT_R8;
  pv.dblVal:=AValue;
  try
    OleCheck(PropStg.WriteMultiple(1,@ps,@pv,PID_FIRST_USABLE));
  finally
    Freemem(ps.lpwstr);
  end;
end;

procedure WriteIntegerProperty(PropStg: IPropertyStorage; AName: string; AValue: int64);
var
  l: integer;
  ps: TPropSpec;
  pv: TPropVariant;
begin
  ps.ulKind:=PRSPEC_LPWSTR;
  l:=Length(AName)+1;
  ps.lpwstr:=AllocMem(l*2);
  ps.lpwstr:=StringToWideChar(AName,ps.lpwstr,l);
  pv.vt:=VT_I8;
  pv.hVal.QuadPart:=AValue;
  try
    OleCheck(PropStg.WriteMultiple(1,@ps,@pv,PID_FIRST_USABLE));
  finally
    Freemem(ps.lpwstr);
  end;
end;

procedure WriteDatetimeProperty(PropStg: IPropertyStorage; AName: string; AValue: TDatetime);
var
  l: integer;
  ps: TPropSpec;
  pv: TPropVariant;
begin
  ps.ulKind:=PRSPEC_LPWSTR;
  l:=Length(AName)+1;
  ps.lpwstr:=AllocMem(l*2);
  ps.lpwstr:=StringToWideChar(AName,ps.lpwstr,l);
  pv.vt:=VT_FILETIME;
  pv.filetime:=DateTimeToFiletime(AValue);
  try
    OleCheck(PropStg.WriteMultiple(1,@ps,@pv,PID_FIRST_USABLE));
  finally
    Freemem(ps.lpwstr);
  end;
end;

procedure WriteStringProperty(PropStg: IPropertyStorage; AID: Cardinal; AValue: string);
var
  l: Integer;
  ps: TPropSpec;
  pv: TPropVariant;
begin
  ps.ulKind:=PRSPEC_PROPID;
  ps.propid:=AID;
  pv.vt:=VT_LPWSTR;
  l:=Length(AValue)+1;
  pv.pwszVal:=AllocMem(l*2);
  pv.pwszVal:=StringToWideChar(AValue,pv.pwszVal,l);
  try
    OleCheck(PropStg.WriteMultiple(1,@ps,@pv,PID_FIRST_USABLE));
  finally
    Freemem(pv.pwszVal);
  end;
end;

procedure WriteDoubleProperty(PropStg: IPropertyStorage; AID: Cardinal; AValue: Double);
var
  ps: TPropSpec;
  pv: TPropVariant;
begin
  ps.ulKind:=PRSPEC_PROPID;
  ps.propid:=AID;
  pv.vt:=VT_R8;
  pv.dblVal:=AValue;
  try
    OleCheck(PropStg.WriteMultiple(1,@ps,@pv,PID_FIRST_USABLE));
  finally
  end;
end;

procedure WriteIntegerProperty(PropStg: IPropertyStorage; AID: Cardinal; AValue: int64);
var
  ps: TPropSpec;
  pv: TPropVariant;
begin
  ps.ulKind:=PRSPEC_PROPID;
  ps.propid:=AID;
  pv.vt:=VT_I8;
  pv.hVal.QuadPart:=AValue;
  try
    OleCheck(PropStg.WriteMultiple(1,@ps,@pv,PID_FIRST_USABLE));
  finally
  end;
end;

procedure WriteDatetimeProperty(PropStg: IPropertyStorage; AID: Cardinal; AValue:TDatetime);
var
  ps: TPropSpec;
  pv: TPropVariant;
begin
  ps.ulKind:=PRSPEC_PROPID;
  ps.propid:=AID;
  pv.vt:=VT_FILETIME;
  pv.filetime:=DateTimeToFiletime(AValue);
  try
    OleCheck(PropStg.WriteMultiple(1,@ps,@pv,PID_FIRST_USABLE));
  finally
  end;
end;

function ReadStringProperty(PropStg: IPropertyStorage; AName: string): string;
var
  l: integer;
  ps: TPropSpec;
  pv: TPropVariant;
begin
  Result:='';
  if PropStg=nil then
    Exit;
  ps.ulKind:=PRSPEC_LPWSTR;
  l:=Length(AName)+1;
  ps.lpwstr:=AllocMem(l*2);
  ps.lpwstr:=StringToWideChar(AName,ps.lpwstr,l);
  try
    if PropStg.ReadMultiple(1,@ps,@pv)=S_OK then begin
      Result:=WideCharToString(pv.pwszVal);
      PropVariantClear(@pv);
    end;
  finally
    Freemem(ps.lpwstr);
  end;
end;

function ReadDoubleProperty(PropStg: IPropertyStorage; AName: string): Double;
var
  l: integer;
  ps: TPropSpec;
  pv: TPropVariant;
begin
  Result:=0;
  if PropStg=nil then
    Exit;
  ps.ulKind:=PRSPEC_LPWSTR;
  l:=Length(AName)+1;
  ps.lpwstr:=AllocMem(l*2);
  ps.lpwstr:=StringToWideChar(AName,ps.lpwstr,l);
  try
    if PropStg.ReadMultiple(1,@ps,@pv)=S_OK then begin
      Result:=pv.dblVal;
      PropVariantClear(@pv);
    end;
  finally
    Freemem(ps.lpwstr);
  end;
end;

function ReadIntegerProperty(PropStg: IPropertyStorage; AName: string): int64;
var
  l: integer;
  ps: TPropSpec;
  pv: TPropVariant;
begin
  Result:=0;
  if PropStg=nil then
    Exit;
  ps.ulKind:=PRSPEC_LPWSTR;
  l:=Length(AName)+1;
  ps.lpwstr:=AllocMem(l*2);
  ps.lpwstr:=StringToWideChar(AName,ps.lpwstr,l);
  try
    if PropStg.ReadMultiple(1,@ps,@pv)=S_OK then begin
      Result:=pv.hVal.QuadPart;
      PropVariantClear(@pv);
    end;
  finally
    Freemem(ps.lpwstr);
  end;
end;

function ReadDatetimeProperty(PropStg: IPropertyStorage; AName: string; CorrectToLocal: Boolean = False): TDatetime;
var
  l: integer;
  ps: TPropSpec;
  pv: TPropVariant;
begin
  Result:=0;
  if PropStg=nil then
    Exit;
  ps.ulKind:=PRSPEC_LPWSTR;
  l:=Length(AName)+1;
  ps.lpwstr:=AllocMem(l*2);
  ps.lpwstr:=StringToWideChar(AName,ps.lpwstr,l);
  try
    if PropStg.ReadMultiple(1,@ps,@pv)=S_OK then begin
      Result:=FileTimeToDatetime(pv.filetime,CorrectToLocal);
      PropVariantClear(@pv);
    end;
  finally
    Freemem(ps.lpwstr);
  end;
end;

function ReadStringProperty(PropStg: IPropertyStorage; AID: cardinal): string;
var
  ps: TPropSpec;
  pv: TPropVariant;
begin
  Result:='';
  if PropStg=nil then
    Exit;
  ps.ulKind:=PRSPEC_PROPID;
  ps.propid:=AID;
  try
    if PropStg.ReadMultiple(1,@ps,@pv)=S_OK then begin
      Result:=WideCharToString(pv.pwszVal);
      PropVariantClear(@pv);
    end;
  finally
  end;
end;

function ReadDoubleProperty(PropStg: IPropertyStorage; AID: cardinal): Double;
var
  ps: TPropSpec;
  pv: TPropVariant;
begin
  Result:=0;
  if PropStg=nil then
    Exit;
  ps.ulKind:=PRSPEC_PROPID;
  ps.propid:=AID;
  try
    if PropStg.ReadMultiple(1,@ps,@pv)=S_OK then begin
      Result:=pv.dblVal;
      PropVariantClear(@pv);
    end;
  finally
  end;
end;

function ReadIntegerProperty(PropStg: IPropertyStorage; AID: cardinal): int64;
var
  ps: TPropSpec;
  pv: TPropVariant;
begin
  Result:=0;
  if PropStg=nil then
    Exit;
  ps.ulKind:=PRSPEC_PROPID;
  ps.propid:=AID;
  try
    if PropStg.ReadMultiple(1,@ps,@pv)=S_OK then begin
      Result:=pv.hVal.QuadPart;
      PropVariantClear(@pv);
    end;
  finally
  end;
end;

function ReadDatetimeProperty(PropStg: IPropertyStorage; AID: cardinal; CorrectToLocal: Boolean = False): TDatetime;
var
  ps: TPropSpec;
  pv: TPropVariant;
begin
  Result:=0;
  if PropStg=nil then
    Exit;
  ps.ulKind:=PRSPEC_PROPID;
  ps.propid:=AID;
  try
    if PropStg.ReadMultiple(1,@ps,@pv)=S_OK then begin
      Result:=FileTimeToDatetime(pv.filetime,CorrectToLocal);
      PropVariantClear(@pv);
    end;
  finally
  end;
end;

function FmtIDToString;
begin
  if SameText(GUIDToString(fmtid),FMTID_SummaryInformation) then
    Result:='Summary Information'
  else if SameText(GUIDToString(fmtid),FMTID_DocSummaryInformation) then
    Result:='Document Summary Information'
  else if SameText(GUIDToString(fmtid),FMTID_UserDefinedProperties) then
    Result:='User Defined Properties'
  else if SameText(GUIDToString(fmtid),FMTID_ImageSummaryInformation) then
    Result:='Image Summary Information'
  else if SameText(GUIDToString(fmtid),FMTID_InternetSite) then
    Result:='Internet Site'
  else if SameText(GUIDToString(fmtid),FMTID_Music) then
    Result:='Music'
  else if SameText(GUIDToString(fmtid),FMTID_Audio) then
    Result:='Audio'
  else if SameText(GUIDToString(fmtid),FMTID_Video) then
    Result:='Video'
  else if SameText(GUIDToString(fmtid),FMTID_MediaFile) then
    Result:='MediaFile'
  else
    Result:=GUIDToString(fmtid);
end;

function PropIDToString(FmtID: TGUID; ID: Cardinal): string;
begin
  Result:='';
  if SameText(GUIDToString(FmtId),FMTID_SummaryInformation) then
    case ID of
      PIDSI_TITLE: Result:='Title';
      PIDSI_SUBJECT: Result:='Subject';
      PIDSI_AUTHOR: Result:='Author';
      PIDSI_KEYWORDS: Result:='Keywords';
      PIDSI_COMMENTS: Result:='Comments';
      PIDSI_TEMPLATE: Result:='Template';
      PIDSI_LASTAUTHOR: Result:='Last saved by';
      PIDSI_REVNUMBER: Result:='Revision number';
      PIDSI_EDITTIME: Result:='Total editing time';
      PIDSI_LASTPRINTED: Result:='Last printed';
      PIDSI_CREATE_DTM: Result:='Create time/date';
      PIDSI_LASTSAVE_DTM: Result:='Last saved time/date';
      PIDSI_PAGECOUNT: Result:='Number of pages';
      PIDSI_WORDCOUNT: Result:='Number of words';
      PIDSI_CHARCOUNT: Result:='Number of characters';
      PIDSI_THUMBNAIL: Result:='Thumbnail';
      PIDSI_APPNAME: Result:='Name of creating application';
      PIDSI_SECURITY: Result:='Security';
    end
  else if SameText(GUIDToString(FmtId),FMTID_DocSummaryInformation) then
    case id of
      PIDDSI_CATEGORY: Result:='Category';
      PIDDSI_PRESFORMAT: Result:='Presentation target';
      PIDDSI_BYTECOUNT: Result:='Bytes';
      PIDDSI_LINECOUNT: Result:='Lines';
      PIDDSI_PARCOUNT: Result:='Paragraphs';
      PIDDSI_SLIDECOUNT: Result:='Slides';
      PIDDSI_NOTECOUNT: Result:='Notes';
      PIDDSI_HIDDENCOUNT: Result:='Hidden slides';
      PIDDSI_MMCLIPCOUNT: Result:='MM Clips';
      PIDDSI_SCALE: Result:='Scale crop';
      PIDDSI_HEADINGPAIR: Result:='Heading pairs';
      PIDDSI_DOCPARTS: Result:='Titlesoft parts';
      PIDDSI_MANAGER: Result:='Manager';
      PIDDSI_COMPANY: Result:='Company';
      PIDDSI_LINKSDIRTY: Result:='Links up to date';
    end
  else if SameText(GUIDToString(FmtId),FMTID_ImageSummaryInformation) then
    case id of
      13: Result:='Image Dimensions';
      5: Result:='Image Res X';
      6: Result:='Image Res Y';
    end
end;

function PropVarTypeString;
begin
  case v of
    VT_EMPTY: Result:='nothing';
    VT_NULL: Result:='null';
    VT_I2: Result:='2 byte signed int';
    VT_I4: Result:='4 byte signed int';
    VT_R4: Result:='4 byte real';
    VT_R8: Result:='8 byte real';
    VT_CY: Result:='currency';
    VT_DATE: Result:='date';
    VT_BSTR: Result:='binary string';
    VT_DISPATCH: Result:='idispatch far*';
    VT_ERROR:Result:='scode';
    VT_BOOL: Result:='true=-1/false=0';
    VT_VARIANT: Result:='variant far*';
    VT_UNKNOWN: Result:='IUnknown FAR*';
    VT_DECIMAL: Result:='16 byte fixed point';
    VT_I1: Result:='signed char';
    VT_UI1: Result:='unsigned char';
    VT_UI2: Result:='unsigned short';
    VT_UI4: Result:='unsigned long';
    VT_I8: Result:='signed 64-bit int';
    VT_UI8: Result:='unsigned 64-bit int';
    VT_INT: Result:='signed machine int';
    VT_UINT: Result:='unsigned machine int';
    VT_VOID: Result:='c-style void';
    VT_HRESULT: Result:='hresult';
    VT_PTR: Result:='pointer type';
    VT_SAFEARRAY: Result:='safearray*';
    VT_CARRAY: Result:='c-style array';
    VT_USERDEFINED: Result:='user defined type';
    VT_LPSTR: Result:='null terminated string';
    VT_LPWSTR: Result:='wide null terminated string';
    {$IFDEF BDS3PLUS}
    VT_INT_PTR: Result:='signed machine register size width';
    VT_UINT_PTR: Result:='unsigned machine register size width';
    {$ENDIF}
    VT_FILETIME: Result:='filetime';
    VT_BLOB: Result:='length prefixed bytes';
    VT_STREAM: Result:='stream';
    VT_STORAGE: Result:='storage';
    VT_STREAMED_OBJECT: Result:='stream contains an object';
    VT_STORED_OBJECT: Result:='storage contains an object';
    VT_BLOB_OBJECT: Result:='blob contains an object';
    VT_CF: Result:='clipboard format';
    VT_CLSID: Result:='class id';
    VT_VECTOR: Result:='Simple counted array';
    VT_ARRAY: Result:='safearray*';
    VT_BYREF: Result:='byref';
  end;
end;

function PropVarToStr;
begin
  case v.vt of
    VT_EMPTY: Result:='';
    VT_NULL: Result:='(null)';
    VT_I2: Result:=IntToStr(v.iVal);
    VT_I4: Result:=IntToStr(v.lVal);
    VT_R4: Result:=FloatToStr(v.fltVal);
    VT_R8: Result:=FloatToStr(v.dblVal);
    VT_CY: Result:=Format('%m',[v.cyVal]);
    VT_DATE: Result:=DateTimeToStr(v.date);
    VT_BSTR: Result:=WideCharToString(v.bstrVal);
    VT_DISPATCH: Result:='(IDispatch)';
    VT_ERROR: Result:=IntToStr(v.scode);
    VT_BOOL: Result:=IntToStr(Integer(v.bool));
    VT_VARIANT: Result:='(VARIANT FAR*)';
    VT_UNKNOWN: Result:='(IUnknown FAR*)';
    VT_DECIMAL: Result:=IntToStr(v.lVal);
    VT_I1: Result:=IntToStr(v.bVal);
    VT_UI1: Result:=IntToStr(v.iVal);
    VT_UI2: Result:=IntToStr(v.uiVal);
    VT_UI4: Result:=IntToStr(v.ulVal);
    VT_I8: Result:=IntToStr(Int64(v.hVal));
    VT_UI8: Result:=IntToStr(Int64(v.uhVal));
    VT_INT: Result:=IntToStr(v.lVal);
    VT_UINT: Result:=IntToStr(v.ulVal);
    VT_VOID: Result:='(C style void)';
    VT_HRESULT: Result:=IntToStr(v.ulVal);
    VT_PTR: Result:='(pointer type)';
    VT_SAFEARRAY: Result:='(SAFEARRAY*)';
    VT_CARRAY: Result:='(C style array)';
    VT_USERDEFINED: Result:='(user defined type)';
    VT_LPSTR: Result:=string(v.pszVal);
    VT_LPWSTR: Result:=WideCharToString(v.pwszVal);
    {$IFDEF BDS3PLUS}
    VT_INT_PTR: Result:='(signed machine register size width)';
    VT_UINT_PTR: Result:='(unsigned machine register size width)';
    {$ENDIF}
    VT_FILETIME: Result:=DateTimeToStr(FileTimeToDateTime(v.filetime));
    VT_BLOB: Result:='(Length prefixed bytes)';
    VT_STREAM: Result:='(Stream)';
    VT_STORAGE: Result:='(Storage)';
    VT_STREAMED_OBJECT: Result:='(Stream contains an object)';
    VT_STORED_OBJECT: Result:='(Storage contains an object)';
    VT_BLOB_OBJECT: Result:='(Blob contains an object)';
    VT_CF: Result:='(Clipboard format)';
    VT_CLSID: Result:=GUIDToString(v.puuid^);
    VT_VECTOR: Result:='(Simple counted array)';
    VT_ARRAY: Result:='(SAFEARRAY*)';
    VT_BYREF: Result:='(BYREF)';
  end;
end;

procedure StrToPropVar;
begin
  case v.vt of
    VT_I2: v.iVal:=StrToInt(s);
    VT_I4: v.lVal:=StrToInt(s);
    VT_R4: v.fltVal:=StrToFloat(s);
    VT_R8: v.dblVal:=StrToFloat(s);
    VT_CY: v.cyVal:=StrToFloat(s);
    VT_DATE: v.date:=StrToDatetime(s);
    VT_BSTR: begin
      v.bstrVal:=AllocMem((Length(s)+1)*2);
      v.bstrVal:=StringToWideChar(s,v.bstrVal,Length(s)+1);
    end;
    VT_ERROR: v.scode:=StrToInt(s);
    VT_BOOL: v.bool:=WordBool(StrToInt(s));
    //VT_VARIANT:
    //VT_UNKNOWN:
    VT_DECIMAL: v.lVal:=StrToInt(s);
    VT_I1: v.bVal:=StrToInt(s);
    VT_UI1: v.iVal:=Byte(StrToInt(s));
    VT_UI2: v.uiVal:=Word(StrToInt(s));
    VT_UI4: v.ulVal:=Cardinal(StrToInt(s));
    VT_I8: v.hVal.QuadPart:=StrToInt(s);
    VT_UI8: v.uhVal.QuadPart:=StrToInt(s);
    VT_INT: v.lVal:=StrToInt(s);
    VT_UINT: v.ulVal:=Cardinal(StrToInt(s));
    //VT_VOID:
    VT_HRESULT: v.ulVal:=Cardinal(StrToInt(s));
    //VT_PTR:
    //VT_SAFEARRAY:
    //VT_CARRAY:
    //VT_USERDEFINED:
    VT_LPSTR: begin
      v.pszVal:=Allocmem(Length(s)+1);
      v.pszVal:={$IFDEF RAD12PLUS}System.AnsiStrings.{$ENDIF}StrPCopy(v.pszVal,{$IFDEF UNICODE}WideToAnsi{$ENDIF}(s));
    end;
    VT_LPWSTR: begin
      v.pwszVal:=AllocMem((Length(s)+1)*2);
      v.pwszVal:=StringToWideChar(s,v.pwszVal,Length(s)+1);
    end;
    //VT_INT_PTR:
    //VT_UINT_PTR:
    VT_FILETIME: v.filetime:=DateTimeToFileTime(StrToDateTime(s));
    //VT_BLOB:
    //VT_STREAM:
    //VT_STORAGE:
    //VT_STREAMED_OBJECT:
    //VT_STORED_OBJECT:
    //VT_BLOB_OBJECT:
    //VT_CF:
    VT_CLSID: v.puuid^:=StringToGUID(s);
    //VT_VECTOR:
    //VT_ARRAY:
    //VT_BYREF:
  end;
end;

procedure ExtractStructure;
var
  s: widestring;
  p: integer;
begin
  s:=APath;
  if s='' then
    Exit;
  if Pos('\',s)=1 then
    Delete(s,1,1);
  p:=Pos('\',s);
  while p>0 do begin
    Dirs.Add(Copy(s,1,p-1));
    s:=Copy(s,p+1,Length(s)-p);
    p:=Pos('\',s);
  end;
  Dirs.Add(s);
  FileName:=Dirs[Dirs.Count-1];
  Dirs.Delete(Dirs.Count-1);
end;

function CheckNames(ANames: string): Integer;
var
  i: integer;
const
  ic: string = '!:/';
begin
  Result:=0;
  ANames:=Trim(ANames);
  if Length(ANames)>31 then
    Result:=1
  else if ANames='' then
    Result:=2
  else
    for i:=1 to Length(ic) do
      if Pos(ic[i],ANames)>0 then begin
        Result:=3;
        Break;
       end;
end;

function CorrectName;
begin
  Result:=Trim(AName);
  if Length(Result)>31 then
    Result:=Copy(Result,1,31);
  Result:=StringReplace(Result,':','_',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'!','_',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'/','_',[rfReplaceAll,rfIgnoreCase]);
end;

{ TStorageStream }

procedure TStorageStream.Clear;
begin
  Self.Position:=0;
  Self.Size:=0;
end;

function TStorageStream.Clone: TStorageStream;
var
  IOUT : IStream;
begin
  OleCheck(FStream.Clone(IOUT));
  Result:=TStorageStream.Create(IOUT,FParent);
end;

procedure TStorageStream.LoadFromFile(FileName : string);
var
  CopyBuffer: Pointer;
  BytesCopied: Longint;
  n: integer;
  Source: Integer;
begin
  Self.Position:=0;
  Self.Size:=0;
  if not Assigned(FStream) then
    Exit;
  GetMem(CopyBuffer,ChunkSize);
  try
    Source:=FileOpen(FileName,fmShareDenyWrite);
    Self.Size:=GetFileSize(Source,nil);
    if Source<0 then
      raise EFOpenError.CreateFmt('Cannot open file %s',[FileName]);
    try
      repeat
        BytesCopied:=FileRead(Source,CopyBuffer^,ChunkSize);
        if BytesCopied>0 then
          OleCheck(FStream.Write(CopyBuffer,BytesCopied,@n));
      until BytesCopied<ChunkSize;
    finally
      FileClose(Source);
    end;
  finally
    FreeMem(CopyBuffer,ChunkSize);
  end;
end;

procedure TStorageStream.SaveToFile(FileName: string);
var
  CopyBuffer: Pointer;
  BytesCopied: Longint;
  stat: TStatSTG;
  F: TFileStream;
  p: {$IFDEF DXE8PLUS}uint64{$ELSE}{$IFDEF FPC}QWORD{$ELSE}int64{$ENDIF}{$ENDIF};
begin
  if not Assigned(FStream) then
    Exit;
  GetMem(CopyBuffer,ChunkSize);
  try
    if FileExists(Filename) then
      {$IFDEF RAD9PLUS}System.SysUtils{$ELSE}SysUtils{$ENDIF}.DeleteFile(FileName);
    F:=TFileStream.Create(FileName,fmCreate or fmShareDenyWrite);
    FStream.Stat(stat,1);
    //Self.Size:=stat.cbSize;
    FStream.Seek(0,STREAM_SEEK_SET,p);
    try
      repeat
        OleCheck(FStream.Read(CopyBuffer,ChunkSize,@BytesCopied));
        if BytesCopied>0 then
          F.Write(CopyBuffer^,BytesCopied);
        until BytesCopied<ChunkSize;
    finally
      F.Free;
    end;
  finally
    FreeMem(CopyBuffer,ChunkSize);
  end;
end;

procedure TStorageStream.SetSize(NewSize: longint);
begin
  if not Assigned(FStream) then
    Exit;
  OleCheck(FStream.SetSize(NewSize));
end;

procedure TStorageStream.SetSize64(const NewSize: Int64);
begin
  if not Assigned(FStream) then
    Exit;
  OleCheck(FStream.SetSize(NewSize));
end;

procedure TStorageStream.SetSize(const NewSize: Int64);
begin
  if not Assigned(FStream) then
    Exit;
  OleCheck(FStream.SetSize(NewSize));
end;

function TStorageStream.GetPosition: {$IFDEF DXE8PLUS}uint64{$ELSE}{$IFDEF FPC}QWORD{$ELSE}int64{$ENDIF}{$ENDIF};
begin
  if not Assigned(FStream) then
    Exit;
  OleCheck(FStream.Seek(0,STREAM_SEEK_CUR,Result));
end;

procedure TStorageStream.SetPosition(NewPosition: {$IFDEF DXE8PLUS}uint64{$ELSE}{$IFDEF FPC}QWORD{$ELSE}int64{$ENDIF}{$ENDIF});
var
  p: {$IFDEF DXE8PLUS}uint64{$ELSE}{$IFDEF FPC}QWORD{$ELSE}int64{$ENDIF}{$ENDIF};
begin
  if not Assigned(FStream) then
    Exit;
  OleCheck(FStream.Seek(NewPosition,STREAM_SEEK_SET,P));
end;

function TStorageStream.GetSize: int64;
var
  stat: TStatStg;
begin
  Result:=0;
  if not Assigned(FStream) then
    Exit;
  OleCheck(FStream.Stat(stat,1));
  Result:=stat.cbSize;
end;

constructor TStorageStream.Create;
begin
  inherited Create;
  FStream:=Stream;
  FParent:=Storage;
  if FParent<>nil then
    Inc(FParent.FLockCount);
end;

destructor TStorageStream.Destroy;
begin
  FStream.Commit(STGC_DEFAULT or STGC_ONLYIFCURRENT);
  Finalize(FStream);
  if FParent<>nil then
    FParent.Close;
  inherited;
end;

function TStorageStream.Read(var Buffer; Count: Longint): Longint;
begin
  if not Assigned(FStream) then
    Exit;
  OleCheck(FStream.Read(@Buffer, Count, @Result));
end;

function TStorageStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  p: {$IFDEF DXE8PLUS}uint64{$ELSE}{$IFDEF FPC}QWORD{$ELSE}int64{$ENDIF}{$ENDIF};
begin
  Result:=0;
  if not Assigned(FStream) then
    Exit;
  OleCheck(FStream.Seek(Offset,Word(Origin),p));
  Result:=p
end;

function TStorageStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  r: {$IFDEF DXE8PLUS}uint64{$ELSE}{$IFDEF FPC}QWORD{$ELSE}int64{$ENDIF}{$ENDIF};
begin
  Result:=0;
  if not Assigned(FStream) then
    Exit;
  OleCheck(FStream.Seek(Offset,Word(Origin),r));
  Result:=r;
end;

function TStorageStream.Write(const Buffer; Count: Longint): Longint;
begin
  if not Assigned(FStream) then
    Exit;
  OleCheck(FStream.Write(@Buffer,Count,@Result));
end;

procedure TStorageStream.WriteInteger(Value: Int64);
begin
  Write(Value,Sizeof(Int64));
end;

function TStorageStream.ReadInteger: int64;
begin
  read(Result,Sizeof(Int64));
end;

procedure TStorageStream.WriteString(Value: string);
var
  Len,Size: Integer;
begin
  if not Assigned(FStream) then
    Exit;
  Len:=Length(Value);
  OleCheck(FStream.Write(@Len,SizeOf(integer),@Size));
  if (Size<>SizeOf(integer)) then
    Exit;
  Size:=0;
  OleCheck(FStream.Write(PChar(Value),Len,@Size));
end;

function TStorageStream.ReadString : string;
var
  Len: integer;
  Size: cardinal;
  p: PChar;
begin
  if not Assigned(FStream) then
    Exit;
  OleCheck(FStream.Read(@Len,SizeOf(integer),@Size));
  if (Size=SizeOf(integer)) then begin
    Size:=0;
    GetMem(p,Len+1);
    try
       OleCheck(FStream.read(p,Len,@Size));
       if Size>0 then begin
         Result:=string(p);
         SetLength(Result,Size);
       end;
    finally
      FreeMem(p,Len+1);
    end;
  end;
end;

procedure TStorageStream.WriteBoolean(Value: Boolean);
var
  v: Integer;
  Size: Cardinal;
begin
  if not Assigned(FStream) then
    Exit;
  v:=Integer(Value);
  OleCheck(FStream.Write(@v,SizeOf(integer),@Size));
end;

function TStorageStream.ReadBoolean : Boolean;
var
  v: integer;
  n: Cardinal;
begin
  Result:=False;
  if not Assigned(FStream) then
    Exit;
  OleCheck(FStream.Read(@v,SizeOf(integer),@n));
  Result:=Boolean(v);
end;

{ TStructuredStorage }

procedure TStructuredStorage.CalculateSize(var Size: Int64);
var
  i: Integer;
  sub: TStructuredStorage;
begin
  for i:=0 to ElementCount-1 do
    if Elements[i].Props.dwType=STGTY_STREAM then
      Size:=Size+Elements[i].Props.cbSize
    else if Elements[i].Props.dwType=STGTY_STORAGE then begin
      sub:=OpenSubStorage(Elements[i].Name,STG_READ_INSTORAGE,False);
      sub.CalculateSize(Size);
    end;
end;

procedure TStructuredStorage.Close;
begin
  if FLockCount>0 then
    Dec(FLockCount);
  if FLockCount<=0 then begin
    FStorage.Commit({STGC_CONSOLIDATE or} STGC_DEFAULT or STGC_ONLYIFCURRENT);
    if AutoFree then
      Destroy;
  end;
end;

constructor TStructuredStorage.Create(AParent: TStructuredStorage;
  AStorage: IStorage);
begin
  inherited Create;
  FParent:=AParent;
  FStorage:=AStorage;
  Inc(FLockCount);
  if FParent<>nil then
    Inc(FParent.FLockCount);
  AutoFree:=not(AParent=nil);
  GetElements;
  GetPropSets;
end;

function TStructuredStorage.CreatePropertySet;
var
  PropStg: IPropertyStorage;
  pid: TPropID;
  os: POleStr;
begin
  OleCheck((FStorage as IPropertySetStorage).Create(FmtID,ClassID,AFlags,AMode,PropStg));
  //OleCheck(StgCreatePropSetStg(FStorage,0,PropSetStg));
  //OleCheck(PropSetStg.Create(FmtID,ClassID,AFlags,AMode,PropStg));
  Result:=TStoragePropertySet.Create(PropStg,Self);
  if Trim(AName)<>'' then begin
    pid:=PID_DICTIONARY;
    os:=Allocmem((Length(AName)+1)*2);
    os:=StringToWidechar(AName,os,(Length(AName)+1));
    try
      OleCheck(PropStg.WritePropertyNames(1,@pid,@os));
    finally
      Freemem(os);
    end;
  end;
  GetElements;
end;

function TStructuredStorage.CreateStream(AName: widestring;
  AMode: Integer): TStorageStream;
var
  PS: TStructuredStorage;
  Stg: IStorage;
  s: IStream;
  FileName: widestring;
  i: integer;
  dl: {$IFDEF UNICODE} TStringList {$ELSE} TWideStrings {$ENDIF};
begin
  Result:=nil;
  if AName='' then
    Exit;
  PS:=Self;
  dl:={$IFDEF UNICODE}TStringList{$ELSE} TWideStrings{$ENDIF}.Create;
  try
    ExtractStructure(AName,FileName,dl);
    for i:=0 to dl.Count-1 do begin
      OleCheck(PS.FStorage.CreateStorage(PWideChar(dl[i]),AMode,0,0,Stg));
      //OleCheck(PS.FStorage.SetElementTimes(PWideChar(WideString(dl[i])),DateTimeToFileTime(now),DateTimeToFileTime(now),DateTimeToFileTime(now)));
      PS:=TStructuredStorage.Create(PS,Stg);
    end;
    OleCheck(PS.FStorage.CreateStream(PWideChar(FileName),AMode,0,0,s));
    //OleCheck(PS.FStorage.SetElementTimes(PWideChar(WideString(FileName)),DateTimeToFileTime(now),DateTimeToFileTime(now),DateTimeToFileTime(now)));
    Result:=TStorageStream.Create(s,PS);
  finally
    dl.Free;
  end;
  GetElements;
end;

function TStructuredStorage.CreateSubStorage(AName: widestring;
  AMode: Integer): TStructuredStorage;
var
  PS: TStructuredStorage;
  Stg: IStorage;
  FileName: widestring;
  i: integer;
  dl: {$IFDEF UNICODE}TStringList{$ELSE} TWideStrings{$ENDIF};
begin
  Result:=nil;
  if AName='' then
    Exit;
  PS:=Self;
  dl:={$IFDEF UNICODE}TStringList{$ELSE} TWideStrings{$ENDIF}.Create;
  try
    ExtractStructure(AName,FileName,dl);
    for i:=0 to dl.Count-1 do begin
      OleCheck(PS.FStorage.CreateStorage(PWideChar(dl[i]),AMode,0,0,Stg));
      PS:=TStructuredStorage.Create(PS,Stg);
    end;
    OleCheck(PS.FStorage.CreateStorage(PWideChar(Filename),AMode,0,0,Stg));
    PS:=TStructuredStorage.Create(PS,Stg);
    Result:=PS;
  finally
    dl.Free;
  end;
  GetElements;
end;

function TStructuredStorage.DeleteElement(AName: widestring): Boolean;
var
  strm: TStorageStream;
  sub: TStructuredStorage;
begin
  Result:=False;
  if not ElementExists(Aname) then
    Exit;
  try
    if ElementByName[AName].Props.dwType=STGTY_STREAM then begin
      strm:=OpenStream(AName,STG_OPEN,False);
      strm.Clear;
      strm.Free;
    end else if ElementByName[AName].Props.dwType=STGTY_STORAGE then begin
      sub:=OpenSubStorage(AName,STG_OPEN,False);
      while (sub.ElementCount>0) do
        sub.DeleteElement(sub.Elements[0].Name);
      sub.Free;
    end;

    FStorage.DestroyElement(PWideChar(Aname));
    FStorage.Commit(STGC_DEFAULT or {STGC_CONSOLIDATE or} STGC_ONLYIFCURRENT);
  except

  end;
  GetElements;
end;

destructor TStructuredStorage.Destroy;
begin
  if Assigned(FStorage) then
    FStorage.Commit({STGC_CONSOLIDATE or} STGC_DEFAULT or STGC_ONLYIFCURRENT);
  Finalize(FElements);
  Finalize(FPropSets);
  Finalize(FStorage);
  if FParent<>nil then
    FParent.Close;
  inherited;
end;

function TStructuredStorage.ElementExists(AName: widestring): Boolean;
var
  i: Integer;
begin
  Result:=False;
  for i:=0 to High(FElements) do
    if SameText(AName,FElements[i].Name) then begin
      Result:=True;
      Break;
    end;
end;

function TStructuredStorage.GetElemCount: Cardinal;
begin
  Result:=Length(FElements);
end;

function TStructuredStorage.GetElement(Index: Integer): TElement;
begin
  Result:=FElements[Index];
end;

function TStructuredStorage.GetElementByName(Name: string): TElement;
var
  i: Integer;
begin
  Zeromemory(@Result,sizeof(Result));
  for i:=0 to High(FElements) do
    if SameText(FElements[i].Name,Name) then begin
      Result:=FElements[i];
      Break;
    end;
end;

procedure TStructuredStorage.GetElements;
const
  Max = 10;
var
  n,i: cardinal;
  ess: IEnumStatStg;
  buf,elems: PSTATSTG;
begin
  Finalize(FElements);
  OleCheck(FStorage.EnumElements(0,nil,0,ess));
  n:=Max;
  repeat
    elems:=Allocmem(SizeOf(TSTATSTG)*Max);
    buf:=elems;
    ess.Next(Max,elems^,@n);
    if n>0 then begin
      for i:=0 to n-1 do begin
        SetLength(FElements,Length(FElements)+1);
        with FElements[High(FElements)] do begin
          Props:=elems^;
          Name:=elems^.pwcsName;
        end;
        CoTaskMemFree(elems^.pwcsName);
        elems:=PSTATSTG(PAnsiChar(elems)+SizeOf(TSTATSTG));
      end;
    end;
    Freemem(buf);
  until n<>Max;
  Finalize(ess);
end;

function TStructuredStorage.GetPropSet(Index: Integer): TPropSet;
begin
  Result:=FPropSets[Index];
end;

function TStructuredStorage.GetPropSetByName(Name: string): TPropSet;
var
  i: Integer;
begin
  Zeromemory(@Result,sizeof(Result));
  for i:=0 to High(FpropSets) do
    if SameText(FpropSets[i].Name,Name) then begin
      Result:=FpropSets[i];
      Break;
    end;
end;

function TStructuredStorage.GetPropSetCount: Cardinal;
begin
  Result:=Length(FPropSets);
end;

procedure TStructuredStorage.GetPropSets;
var
  PropSetStg: IPropertySetStorage;
  PropStg: IPropertyStorage;
  ppEnum: IEnumSTATPROPSETSTG;
  psstat: STATPROPSETSTG;
  hr: HRESULT;
  pid: Cardinal;
  os: POLEStr;
begin
  Finalize(FPropSets);

  PropSetStg:=FStorage as IPropertySetStorage;
  OleCheck(PropSetStg.Enum(ppEnum));
  repeat
    hr:=ppEnum.Next(1,psstat,nil);
    if (hr=S_OK) then begin
      try
        OleCheck(PropSetStg.Open(psstat.fmtid,STG_READ_INSTORAGE,PropStg));
        pid:=PID_DICTIONARY;
        SetLength(FPropSets,Length(FPropSets)+1);
        with FPropSets[High(FPropSets)] do begin
          Props:=psstat;
          PropStg.ReadPropertyNames(1,@pid,@os);
          try
            Name:=os;
          finally
            CoTaskMemFree(os);
          end;
        end;
      except

      end;
    end;
  until (hr<>S_OK);
  Finalize(ppEnum);
  Finalize(PropStg);
end;

function TStructuredStorage.GetRoot: boolean;
begin
  Result:=FParent=nil;
end;

function TStructuredStorage.GetSize;
var
  s: Int64;
begin
  s:=0;
  CalculateSize(s);
  Result:=s;
end;  

function TStructuredStorage.OpenPropertySet;
var
  PropSetStg: IPropertySetStorage;
  PropStg: IPropertyStorage;
begin
  Result:=nil;
  PropSetStg:=FStorage as IPropertySetStorage;
  PropSetStg.Open(FmtID,AMode,PropStg);
  if (PropStg=nil) and CanCreate then
    Result:=CreatePropertySet(AName,FmtID,StringToGUID(NULL_GUID),PROPSETFLAG_DEFAULT,AMode or STGM_CREATE)
  else
    if PropStg<>nil then
      Result:=TStoragePropertySet.Create(PropStg,Self);
  GetElements;    
end;

function TStructuredStorage.OpenStream(AName: widestring; AMode: Integer;
  CanCreate: Boolean): TStorageStream;
var
  s: IStream;
  Stg: IStorage;
  FileName: widestring;
  i: integer;
  dl: {$IFDEF UNICODE}TStringList{$ELSE}TWideStrings{$ENDIF};
  PS,PS1: TStructuredStorage;
  r: Boolean;
begin
  r:=False;
  Result:=nil;
  if (AName='') then
    Exit;
  dl:={$IFDEF UNICODE}TStringList{$ELSE} TWideStrings{$ENDIF}.Create;
  try
    ExtractStructure(AName,FileName,dl);
    if dl.Count>0 then begin
      PS:=Self;
      for i:=0 to dl.Count-1 do begin
        if not PS.ElementExists(dl[i]) and CanCreate then begin
          OleCheck(PS.FStorage.CreateStorage(PWideChar(dl[i]),AMode or STGM_CREATE,0,0,Stg));
          r:=True;
        end else
          OleCheck(PS.FStorage.OpenStorage(PWideChar(dl[i]),nil,AMode,nil,Longint(nil),Stg));
        PS1:=PS;
        PS:=TStructuredStorage.Create(PS1,Stg);
        if PS1<>Self then
          PS1.Close;
      end;
      if not PS.ElementExists(Filename) then begin
        if CanCreate then begin
          OleCheck(PS.FStorage.CreateStream(PWideChar(FileName),AMode,0,0,s));
          r:=True;
          OleCheck(PS.FStorage.OpenStream(PWideChar(FileName),nil,AMode,0,s));
        end;
      end else
        OleCheck(PS.FStorage.OpenStream(PWideChar(FileName),nil,AMode,0,s));
      if s<>nil then
        Result:=TStorageStream.Create(s,PS);
      if PS<>Self then
        PS.Close;
    end else begin
      if not ElementExists(Filename) and CanCreate then begin
        OleCheck(FStorage.CreateStream(PWideChar(FileName),AMode,0,0,s));
        r:=True;
      end;
      try
        OleCheck(FStorage.OpenStream(PWideChar(FileName),nil,AMode,0,s));
        Result:=TStorageStream.Create(s,Self);
      except
      end;
    end;
  finally
    dl.Free;
  end;
  if r then
    GetElements;
end;

function TStructuredStorage.OpenSubStorage(AName: widestring; AMode: Integer;
  CanCreate: Boolean): TStructuredStorage;
var
  PS,PS1: TStructuredStorage;
  Stg: IStorage;
  FileName: widestring;
  i: integer;
  dl: {$IFDEF UNICODE} TStringList {$ELSE} TWideStrings {$ENDIF};
  r: Boolean;
begin
  PS1:=nil;
  r:=False;
  Result:=nil;
  if AName='' then
    Exit;
  dl:={$IFDEF UNICODE}TStringList{$ELSE} TWideStrings{$ENDIF}.Create;
  try
    ExtractStructure(AName,FileName,dl);
    if dl.Count>0 then begin
      PS:=Self;
      for i:=0 to dl.Count-1 do begin
        if not PS.ElementExists(dl[i]) and CanCreate then begin
          OleCheck(PS.FStorage.CreateStorage(PWideChar(dl[i]),AMode or STGM_CREATE,0,0,Stg));
          r:=True;
        end else
          OleCheck(PS.FStorage.OpenStorage(PWideChar(dl[i]),nil,AMode,nil,Longint(nil),Stg));
        PS1:=PS;
        PS:=TStructuredStorage.Create(PS1,Stg);
        if PS1<>Self then
          PS1.Close;
      end;
      if not PS.ElementExists(Filename) then begin
        if CanCreate then begin
          OleCheck(PS.FStorage.CreateStorage(PWideChar(FileName),AMode or STGM_CREATE,0,0,Stg));
          r:=True;
        end else begin
          if PS1<>Self then
            PS1.Close;
          PS.Close;
          Exit;
        end;
      end else
        OleCheck(PS.FStorage.OpenStorage(PWideChar(FileName),nil,AMode,nil,Longint(nil),Stg));
      PS1:=PS;
      PS:=TStructuredStorage.Create(PS1,Stg);
      if PS1<>Self then
        PS1.Close;
      Result:=PS;
    end else begin
      if not ElementExists(Filename) then begin
        if CanCreate then begin
          OleCheck(FStorage.CreateStorage(PWideChar(Filename),AMode or STGM_CREATE,0,0,Stg));
          r:=True;
        end else begin
          Exit;
        end;
      end else
        OleCheck(FStorage.OpenStorage(PWideChar(FileName),nil,AMode,nil,Longint(nil),Stg));
      Result:=TStructuredStorage.Create(Self,Stg);
    end;
  finally
    dl.Free;
  end;
  if r then
    GetElements;
end;

{ TStoragePropertySet }

constructor TStoragePropertySet.Create;
begin
  inherited Create;
  FPS:=PropertyStorage;
  FParent:=Storage;
  if FParent<>nil then
    Inc(FParent.FLockCount);
end;

destructor TStoragePropertySet.Destroy;
begin
  FPS.Commit(STGC_DEFAULT or STGC_ONLYIFCURRENT);
  Finalize(FProps);
  Finalize(FPS);
  if FParent<>nil then
    FParent.Close;
  inherited;
end;

{procedure TStoragePropertySet.EnumProps;
var
  propEnum: IEnumSTATPROPSTG;
  propstat: STATPROPSTG;
  PropSpec: TPropSpec;
  PropVariant: TPropVariant;
  hr: HRESULT;
begin
  Finalize(FProps);
  OleCheck(FPS.Enum(propEnum));
  repeat
    hr:=propEnum.Next(1,propstat,nil);
    if hr=S_OK then begin
      PropSpec.propid:=propstat.propid;
      PropSpec.ulKind:=PRSPEC_PROPID;
      hr:=FPS.ReadMultiple(1,@PropSpec,@PropVariant);
      if hr=S_OK then begin
        SetLength(FProps,Length(FProps)+1);
        FProps[High(FProps)]:=PropVariant;
        PropVariantClear(@PropVariant);
      end;
    end;
  until hr<>S_OK;
end;}

function TStoragePropertySet.GetProp(Index: Integer): TPropVariant;
begin
  Result:=FProps[Index];
end;

function TStoragePropertySet.GetPropCount: Cardinal;
begin
  Result:=Length(FProps);
end;

function TStoragePropertySet.GetStat: STATPROPSETSTG;
begin
  FPS.Stat(@Result);
end;

function ReadIntProperty(AList: TStrings; AName: string): Int64;
begin
  Result:=StrToInt64Def(AList.Values[AName],0);
end;

function ReadStrProperty(AList: TStrings; AName: string): string;
begin
  Result:=AList.Values[AName];
end;

function ReadDblProperty(AList: TStrings; AName: string): Double;
var
  ds: char;
begin
  ds:={$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator;
  {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:='.';
  Result:=StrToFloatDef(AList.Values[AName],0);
  {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:=ds;
end;

function ReadDtProperty(AList: TStrings; AName: string): TDatetime;
begin
  Result:=ReadDblProperty(Alist,AName);
end;

procedure WriteIntProperty(AList: TStrings; AName: string; AValue: Int64);
begin
  AList.Add(Format('%s=%d',[AName,AValue]));
end;

procedure WriteStrProperty(AList: TStrings; AName: string; AValue: string);
begin
  AList.Add(Format('%s=%s',[AName,AValue]));
end;

procedure WriteDblProperty(AList: TStrings; AName: string; AValue: Double);
var
  ds: char;
begin
  ds:={$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator;
  {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:='.';
  AList.Add(Format('%s=%1.9f',[AName,AValue]));
  {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:=ds;
end;

procedure WriteDtProperty(AList: TStrings; AName: string; AValue: TDateTime);
begin
  WriteDblProperty(AList,AName,AValue);
end;

procedure LoadFromEncodedStream(AStream: TStream; AList: TStrings; ACodeStream: TCodeStreamProcedure);
var
  ms: TMemoryStream;
begin
  AList.Clear;
  if AStream.Size=0 then
    Exit;
  AStream.Position:=0;
  ms:=TMemoryStream.Create;
  try
    if Assigned(ACodeStream) then
      try
        ACodeStream(AStream,ms)
      except
        AStream.Position:=0;
        ms.LoadFromStream(AStream);
      end
    else
      ms.LoadFromStream(AStream);
    try
      AList.LoadFromStream(ms);
    except
    end;
  finally
    ms.Free;
  end;
end;

procedure LoadFromEncodedStream(AStream: TStream; AOutStream: TStream; ACodeStream: TCodeStreamProcedure); overload;
begin
  if AStream.Size=0 then
    Exit;
  AOutStream.Position:=0;
  AOutStream.Size:=0;
  AStream.Position:=0;
  try
    if Assigned(ACodeStream) then
      try
        ACodeStream(AStream,AOutStream)
      except
        AOutStream.Position:=0;
        AOutStream.Size:=0;
        AStream.Position:=0;
        AOutStream.CopyFrom(AStream,AStream.Size);
      end
    else
      AOutStream.CopyFrom(AStream,AStream.Size);
  finally
    AOutStream.Position:=0;
  end;
end;

procedure SaveToEncodedStream(AList: TStrings; AStream: TStream; ACodeStream: TCodeStreamProcedure);
var
  ms: TMemoryStream;
begin
  ms:=TMemoryStream.Create;
  try
    AList.SaveToStream(ms);
    if Assigned(ACodeStream) then
      ACodeStream(ms,AStream)
    else
      ms.SaveToStream(AStream);
    AStream.Position:=0;
  finally
    ms.Free;
  end;
end;

procedure SaveToEncodedStream(AInStream: TStream; AStream: TStream; ACodeStream: TCodeStreamProcedure);
var
  ms: TMemoryStream;
begin
  ms:=TMemoryStream.Create;
  try
    ms.Position:=0;
    AInSTream.Position:=0;
    ms.CopyFrom(AInSTream,AInStream.Size);
    if Assigned(ACodeStream) then
      ACodeStream(ms,AStream)
    else
      ms.SaveToStream(AStream);
    AStream.Position:=0;
    AInSTream.Position:=0;
  finally
    ms.Free;
  end;
end;

function StorageFolderExists(AFilename, ASection: string): Boolean;
var
  stg: IStorage;
  SS,Sub: TStructuredStorage;
begin
  Result:=False;
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,0,stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    try
      Sub:=SS.OpenSubStorage(ASection,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;
    Result:=Sub<>nil;
    if Sub<>nil then
      Sub.Free;
  finally
    SS.Free;
  end;
end;

function StorageStreamExists(AFilename, AName: string): Boolean;
var
  stg: IStorage;
  SS: TStructuredStorage;
  strm: TStorageStream;
begin
  Result:=False;
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    try
      strm:=SS.OpenStream(Aname,STG_READ_INSTORAGE,False);
    except
      strm:=nil;
    end;
    Result:=Assigned(strm) {and (Strm.Size<>0)};
    if strm<>nil then
      strm.Free;
  finally
    SS.Free;
  end;
end;

procedure DeleteStorageStream;
var
  stg: IStorage;
  Sub,SS: TStructuredStorage;
begin
  OleCheck(StgOpenStorage(PWideChar(WideString(AFilename)),nil,STG_OPEN{STGM_TRANSACTED or STGM_NOSCRATCH or STGM_SHARE_EXCLUSIVE or STGM_READWRITE},nil,Longint(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  Sub:=SS.OpenSubStorage(AFolder,STG_OPEN{STGM_TRANSACTED or STGM_SHARE_EXCLUSIVE or STGM_READWRITE},False);
  if Sub=nil then
    Sub:=SS;
  try
    Sub.DeleteElement(AName);
  finally
    Sub.Free;
    try
      SS.Free;
    except
    end;
  end;
end;

procedure SaveFileToStorage(AStorageFilename, AFolder, AStream, AFilename: string; ACodeStream: TCodeStreamProcedure = nil);
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  fs: TFileStream;
begin
  Sub:=nil;
  //try
    fs:=TFileStream.Create(AFilename,fmOpenRead or fmShareDenyNone);
  //except
    //fs:=nil;
  //end;
  try
    if (fs=nil) or (fs.Size=0) then
      Exit;
    if StgIsStorageFile(PWideChar(WideString(AStorageFileName)))<>S_OK then
      OleCheck(StgCreateDocFile(PWideChar(WideString(AStorageFileName)),STG_CREATE_OPEN,0,stg))
    else
      OleCheck(StgOpenStorage(PWideChar(WideString(AStorageFilename)),nil,STG_OPEN{STGM_TRANSACTED or STGM_NOSCRATCH or STGM_SHARE_EXCLUSIVE or STGM_READWRITE},nil,LongInt(nil),stg));
    SS:=TStructuredStorage.Create(nil,stg);
    try
      Sub:=SS.OpenSubStorage(AFolder,STG_OPEN{STGM_TRANSACTED or STGM_SHARE_EXCLUSIVE or STGM_READWRITE},True);
      if Sub=nil then
        Sub:=SS;
      Sub.DeleteElement(AStream);
      strm:=Sub.OpenStream(AStream,STG_OPEN,True);
      try
        SaveToEncodedStream(fs,strm,ACodeStream);
      finally
        strm.Free;
      end;
    finally
      if Assigned(Sub) then
        Sub.Free;
      try
        SS.Free;
      except
      end;
    end;
  finally
    if fs<>nil then
      fs.Free;
  end;
end;

procedure SaveStreamToStorage(AStorageFilename, AFolder, AStream: string; AData: TStrings; ACodeStream: TCodeStreamProcedure = nil; ADelete: boolean = True);
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
begin
  Sub:=nil;
  if StgIsStorageFile(PWideChar(WideString(AStorageFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AStorageFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AStorageFilename)),nil,STG_OPEN{STGM_TRANSACTED or STGM_NOSCRATCH or STGM_SHARE_EXCLUSIVE or STGM_READWRITE},nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    if (AFolder<>'') and not SameText('\',AFolder) then
      Sub:=SS.OpenSubStorage(AFolder,STG_OPEN{STGM_TRANSACTED or STGM_SHARE_EXCLUSIVE or STGM_READWRITE},True)
    else
      Sub:=SS;
    if ADelete then
      Sub.DeleteElement(AStream);
    strm:=Sub.OpenStream(AStream,STG_OPEN,True);
    try
      SaveToEncodedStream(AData,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    if Assigned(Sub) then
      Sub.Free;
    try
      SS.Free;
    except
    end;
  end;
end;

procedure SaveStreamToStorage(AStorageFilename, AFolder, AStream: string; AData: TStream; ACodeStream: TCodeStreamProcedure = nil; ADelete: boolean = True);
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
begin
  Sub:=nil;
  if StgIsStorageFile(PWideChar(WideString(AStorageFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AStorageFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AStorageFilename)),nil,STG_OPEN{STGM_TRANSACTED or STGM_NOSCRATCH or STGM_SHARE_EXCLUSIVE or STGM_READWRITE},nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    if (AFolder<>'') and not SameText('\',AFolder) then
      Sub:=SS.OpenSubStorage(AFolder,STG_OPEN{STGM_TRANSACTED or STGM_SHARE_EXCLUSIVE or STGM_READWRITE},True)
    else
      Sub:=SS;
    if ADelete then
      Sub.DeleteElement(AStream);
    strm:=Sub.OpenStream(AStream,STG_OPEN,True);
    try
      SaveToEncodedStream(AData,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    if Assigned(Sub) then
      Sub.Free;
    try
      SS.Free;
    except
    end;
  end;
end;

procedure LoadStreamFromStorage(AStorageFilename, AFolder, AStream: string; AData: TStrings; ACodeStream: TCodeStreamProcedure = nil);
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
begin
  Sub:=nil;
  AData.Clear;
  if StgIsStorageFile(PWideChar(WideString(AStorageFilename)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AStorageFilename)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    if (AFolder<>'') and not SameText('\',AFolder) then
      try
        Sub:=SS.OpenSubStorage(AFolder,STG_READ_INSTORAGE,False);
      except
        Sub:=nil;
      end
    else
      Sub:=SS;
    if Sub<>nil then begin
      if not Sub.ElementExists(AStream) then
        Exit;
      strm:=Sub.OpenStream(AStream,STG_READ_INSTORAGE,False);
      if strm=nil then
        Exit;
      try
        LoadFromEncodedStream(strm,AData,ACodeStream);
      finally
        strm.Free;
      end;
    end;
  finally
    if Sub<>nil then
      Sub.Free;
    try SS.Free except end;
  end;
end;

procedure LoadStreamFromStorage(AStorageFilename, AFolder, AStream: string; AOutStream: TStream; ACodeStream: TCodeStreamProcedure = nil);
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
begin
  Sub:=nil;
  AOutStream.Size:=0;
  AOutStream.Position:=0;
  if StgIsStorageFile(PWideChar(WideString(AStorageFilename)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AStorageFilename)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    if (AFolder<>'') and not SameText('\',AFolder) then
      try
        Sub:=SS.OpenSubStorage(ExcludeTrailingPathDelimiter(AFolder),STG_READ_INSTORAGE,False);
      except
        Sub:=nil;
      end
    else
      Sub:=SS;
    if Sub<>nil then begin
      if not Sub.ElementExists(AStream) then
        Exit;
      strm:=Sub.OpenStream(AStream,STG_READ_INSTORAGE,False);
      if strm=nil then
        Exit;
      try
        LoadFromEncodedStream(strm,AOutStream,ACodeStream);
        AOutStream.Position:=0;
      finally
        strm.Free;
      end;
    end;
  finally
    if Sub<>nil then
      Sub.Free;
    try SS.Free except end;
  end;
end;

procedure LoadStreamFromStorageAndSave;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  ms: TMemoryStream;
begin
  Sub:=nil;
  if StgIsStorageFile(PWideChar(WideString(AStorageFilename)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AStorageFilename)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    if (AFolder<>'') and not SameText('\',AFolder) then
      try
        Sub:=SS.OpenSubStorage(ExcludeTrailingPathDelimiter(AFolder),STG_READ_INSTORAGE,False);
      except
        Sub:=nil;
      end
    else
      Sub:=SS;
    if Sub<>nil then begin
      if not Sub.ElementExists(AStream) then
        Exit;
      strm:=Sub.OpenStream(AStream,STG_READ_INSTORAGE,False);
      if strm=nil then
        Exit;
      try
        ms:=TMemoryStream.Create;
        try
          LoadFromEncodedStream(strm,ms,ACodeSTream);
          ms.SaveToFile(Asavefilename);
        finally
          ms.Free;
        end;
      finally
        strm.Free;
      end;
    end;
  finally
    if Sub<>nil then
      Sub.Free;
    try SS.Free except end;
  end;
end;

function IsEmptyStream;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
begin
  Sub:=nil;
  Result:=True;
  if StgIsStorageFile(PWideChar(WideString(AFilename)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFilename)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    if (AFolder<>'') and not SameText('\',AFolder) then
      try
        Sub:=SS.OpenSubStorage(AFolder,STG_READ_INSTORAGE,False);
      except
        Sub:=nil;
      end
    else
      Sub:=SS;
    if Sub<>nil then begin
      if not Sub.ElementExists(AName) then
        Exit;
      strm:=Sub.OpenStream(AName,STG_READ_INSTORAGE,False);
      if strm=nil then
        Exit;
      try
        Result:=strm.Size=0;
      finally
        strm.Free;
      end;
    end;
  finally
    if Sub<>nil then
      Sub.Free;
    try SS.Free except end;
  end;
end;

procedure GetFolderStreamNames(AFilename,AFolder: string; AList: TStrings; AIncludeSize: Boolean = False);
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  i: Integer;
begin
  Sub:=nil;
  AList.Clear;
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFilename)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    Sub:=SS.OpenSubStorage(AFolder,STG_OPEN,True);
    if Sub=nil then
      Sub:=SS;
    for i:=0 to Sub.ElementCount-1 do
      if Sub.Elements[i].Props.dwType=STGTY_STREAM then begin
        if AIncludeSize then
          AList.Add(Format('%s=%d',[Sub.Elements[i].Name,Sub.Elements[i].Props.cbSize]))
        else
          AList.Add(Sub.Elements[i].Name);
      end;
  finally
    if Assigned(Sub) then
      Sub.Free;
    try
      SS.Free;
    except
    end;
  end;
end;

function IsFolderEmpty(AFilename,AFolder: string): Boolean;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
begin
  Sub:=nil;
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFilename)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    Sub:=SS.OpenSubStorage(AFolder,STG_OPEN,True);
    if Sub=nil then
      Sub:=SS;
    Result:=Sub.ElementCount=0;
  finally
    if Assigned(Sub) then
      Sub.Free;
    try
      SS.Free;
    except
    end;
  end;
end;

function IsValidStgFile(const AFilename: string): boolean;
var
  stg: IStorage;
begin
  Result:=False;
  if StgIsStorageFile(PWideChar(WideString(AFileName)))=S_OK then
    try
      StgOpenStorage(PWideChar(WideString(AFilename)),nil,STG_OPEN,nil,LongInt(nil),stg);
      if Assigned(stg) then begin
        stg.Commit(STGC_DEFAULT or STGC_ONLYIFCURRENT);
        Result:=True;
      end;
    except
    end;
end;

procedure ConsolidateFile(const AFilename: string);
var
  stg: IStorage;
begin
  if StgIsStorageFile(PWideChar(WideString(AFileName)))=S_OK then
    try
      StgOpenStorage(PWideChar(WideString(AFilename)),nil,STGM_WRITE or STGM_TRANSACTED or STGM_SHARE_EXCLUSIVE,nil,LongInt(nil),stg);
      //StgOpenStorageEx(PWideChar(WideString(AFilename)),STGM_READWRITE or STGM_TRANSACTED or STGM_SHARE_EXCLUSIVE,STGFMT_ANY,0,nil,nil,@IID_IStorage,stg);
      if Assigned(stg) then
        stg.Commit(STGC_DEFAULT or STGC_ONLYIFCURRENT or STGC_CONSOLIDATE);
    except
    end;
end;

end.

