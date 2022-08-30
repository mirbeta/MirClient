{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE       }
{   PROGRAM ONLY.                                                    }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}
unit cxStorage;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Registry, SysUtils, Classes, TypInfo, IniFiles, dxCore,
  cxClasses, cxLibraryStrs, cxGeometry;

type
  { IcxStoredObject }

  IcxStoredObject = interface
  ['{79A05009-CAC3-47E8-B454-F6F3D91F495D}']
    function GetObjectName: string;
    function GetProperties(AProperties: TStrings): Boolean;
    procedure GetPropertyValue(const AName: string; var AValue: Variant);
    procedure SetPropertyValue(const AName: string; const AValue: Variant);
  end;

  { IcxStoredObject2 }

  IcxStoredObject2 = interface
  ['{770DF537-C44D-4B54-A226-4195ED234F9E}']
    function CanDelete: Boolean;
  end;

  { IcxStoredParent }

  IcxStoredParent = interface
  ['{6AF48CD0-3A0B-4BEC-AC88-5D323432A686}']
    function CreateChild(const AObjectName, AClassName: string): TObject;
    procedure DeleteChild(const AObjectName: string; AObject: TObject);
    procedure GetChildren(AChildren: TStringList);
  end;

  { IcxStoredParent2 }

  IcxStoredParent2 = interface
  ['{337024FA-3C2E-4870-97D3-1467DAB60E2E}']
    procedure StoringBegin;
    procedure StoringEnd;
  end;

  EcxStorage = class(EdxException);
  EcxHexStringConvertError = class(EdxException);
  TcxStorageMode = (smChildrenCreating, smChildrenDeleting, smSavePublishedClassProperties);
  TcxStorageModes = set of TcxStorageMode;
  TcxStorage = class;
  TcxCustomReader = class;
  TcxCustomWriter = class;
  TcxCustomReaderClass = class of TcxCustomReader;
  TcxCustomWriterClass = class of TcxCustomWriter;
  TcxGetStorageModesEvent = function: TcxStorageModes of object;
  TcxTestClassPropertyEvent = function(const AName: string; AObject: TObject): Boolean of object;
  TcxGetComponentByNameEvent = function(const AName: string): TComponent of object;
  TcxGetUseInterfaceOnlyEvent = function: Boolean of object;

  TcxGetStoredPropertiesEvent = procedure(Sender: TObject; AProperties: TStrings) of object;
  TcxGetStoredPropertyValueEvent = procedure(Sender: TObject; const AName: string; var AValue: Variant) of object;
  TcxInitStoredObjectEvent = procedure(Sender: TObject; AObject: TObject) of object;
  TcxSetStoredPropertyValueEvent = procedure(Sender: TObject; const AName: string; const AValue: Variant) of object;
  TcxSetStoredPropertyValueErrorEvent = function(Sender: TcxStorage; const AName: string; const AValue: Variant): Boolean of object;

  { TcxStorageType }

  TcxStorageType = (stIniFile, stRegistry, stStream);

  { TcxStorage }

  TcxStorage = class
  strict private const
    sPropertySourceDPI = 'SourceDPI';
  strict private
    FModes: TcxStorageModes;
    FNamePrefix: string;
    FObjectNamePrefix: string;
    FReCreate: Boolean;
    FSaveComponentPropertiesByName: Boolean;
    FStorageName: string;
    FStoredDPI: Integer;
    FStoredObject: TObject;
    FStoredObjectPrevDPI: Integer;
    FStream: TStream;
    FUseInterfaceOnly: Boolean;

    FOnGetComponentByName: TcxGetComponentByNameEvent;
    FOnGetStorageModes: TcxGetStorageModesEvent;
    FOnGetUseInterfaceOnly: TcxGetUseInterfaceOnlyEvent;
    FOnSetStoredPropertyValueError: TcxSetStoredPropertyValueErrorEvent;
    FOnTestClassProperty: TcxTestClassPropertyEvent;

    function CreateChild(const AObjectName, AClassName: string): TObject;
    procedure CreateChildrenNames(AChildren: TStringList);
    procedure DeleteChild(const AObjectName: string; AObject: TObject);
    procedure GetAllPublishedClassProperties(AProperties: TStrings);
    procedure GetAllPublishedProperties(AProperties: TStrings);
    procedure GetChildren(AChildren: TStringList);
    function GetClassProperty(const AName: string): TObject;
    function GetComponentByName(const AName: string): TComponent;
    function GetObjectName(AObject: TObject): string;
    procedure GetProperties(AProperties: TStrings);
    function GetPropertyValue(const AName: string): Variant;
    function GetStorageModes: TcxStorageModes;
    function GetUseInterfaceOnly: Boolean;
    procedure ScaleStoredObjectForPPI(ATargetPPI: Integer);
    procedure SetPropertyValue(const AName: string; AValue: Variant);
    procedure StoringBegin;
    procedure StoringEnd;
    function TestClassProperty(const AName: string; AObject: TObject): Boolean;
  protected
    procedure ClearObjectData(AWriter: TcxCustomWriter);
    function GetFullObjectName(const ADefaultObjectName: string = ''): string;
    procedure InternalCheckChildrenOrder(AChildrenNames, AChildrenClassNames: TStringList);
    procedure InternalRestoreFrom(AReader: TcxCustomReader; const ADefaultObjectName: string = ''); virtual;
    procedure InternalStoreTo(AWriter: TcxCustomWriter; const ADefaultObjectName: string = ''); virtual;
    procedure SetStoredObject(AObject: TObject);
  public
    constructor Create(const AStorageName: string; AStorageStream: TStream); overload;
    constructor Create(const AStorageName: string); overload;
    constructor Create(AStream: TStream); overload;
    procedure RestoreFrom(AObject: TObject; AReaderClass: TcxCustomReaderClass); virtual;
    procedure RestoreWithExistingReader(AObject: TObject; AReader: TcxCustomReader); virtual;
    procedure RestoreFromIni(AObject: TObject);
    procedure RestoreFromRegistry(AObject: TObject);
    procedure RestoreFromStream(AObject: TObject);
    procedure StoreTo(AObject: TObject; AWriterClass: TcxCustomWriterClass); virtual;
    procedure StoreWithExistingWriter(AObject: TObject; AWriter: TcxCustomWriter); virtual;
    procedure StoreToIni(AObject: TObject);
    procedure StoreToRegistry(AObject: TObject);
    procedure StoreToStream(AObject: TObject);

    property NamePrefix: string read FNamePrefix write FNamePrefix;
    property Modes: TcxStorageModes read FModes write FModes;
    property ReCreate: Boolean read FReCreate write FReCreate;
    property SaveComponentPropertiesByName: Boolean read FSaveComponentPropertiesByName write FSaveComponentPropertiesByName;
    property StoredDPI: Integer read FStoredDPI;
    property StoredObject: TObject read FStoredObject;
    property StorageName: string read FStorageName write FStorageName;
    property UseInterfaceOnly: Boolean read FUseInterfaceOnly write FUseInterfaceOnly;
    property OnGetComponentByName: TcxGetComponentByNameEvent read FOnGetComponentByName write FOnGetComponentByName;
    property OnGetStorageModes: TcxGetStorageModesEvent read FOnGetStorageModes write FOnGetStorageModes;
    property OnGetUseInterfaceOnly: TcxGetUseInterfaceOnlyEvent read FOnGetUseInterfaceOnly write FOnGetUseInterfaceOnly;
    property OnTestClassProperty: TcxTestClassPropertyEvent read FOnTestClassProperty write FOnTestClassProperty;
    property OnSetStoredPropertyValueError: TcxSetStoredPropertyValueErrorEvent read FOnSetStoredPropertyValueError write FOnSetStoredPropertyValueError;
  end;

  { TcxCustomReader }

  TcxCustomReader = class
  private
    FStorageVersion: Integer;
    FStorageName: string;
    FStorageStream: TStream;
  protected
    function CanRead: Boolean; virtual;
    procedure DoSetStream(AStream: TStream); virtual;
    procedure BeginRead; virtual;
    procedure EndRead; virtual;
  public
    constructor Create(const AStorageName: string); overload; virtual;
    constructor Create(const AStorageName: string; AStorageStream: TStream); overload; virtual;
    procedure ReadProperties(const AObjectName, AClassName: string; AProperties: TStrings); virtual;
    function ReadProperty(const AObjectName, AClassName, AName: string): Variant; virtual;
    procedure ReadChildren(const AObjectName, AClassName: string; AChildrenNames,
        AChildrenClassNames: TStrings); virtual;
    procedure SetStream(AStream: TStream);

    property StorageName: string read FStorageName;
    property StorageStream: TStream read FStorageStream;
  end;

  { TcxCustomWriter }

  TcxCustomWriter = class
  protected
    FReCreate: Boolean;
    FStorageName: string;
    FStorageStream: TStream;
    function CanWrite: Boolean; virtual;
    procedure ClearObjectData(const AObjectFullName, AClassName: string); virtual;
    procedure DoSetStream(AStream: TStream); virtual;
    procedure BeginWrite; virtual;
    procedure EndWrite; virtual;
  public
    constructor Create(const AStorageName: string; AReCreate: Boolean = True); overload; virtual;
    constructor Create(const AStorageName: string; AStorageStream: TStream; AReCreate: Boolean = True); overload; virtual;
    procedure BeginWriteObject(const AObjectName, AClassName: string); virtual;
    procedure EndWriteObject(const AObjectName, AClassName: string); virtual;
    procedure SetStream(AStream: TStream);
    procedure WriteProperty(const AObjectName, AClassName, AName: string; AValue: Variant); virtual;

    property ReCreate: Boolean read FReCreate write FReCreate;
    property StorageName: string read FStorageName;
    property StorageStream: TStream read FStorageStream;
  end;

  { TcxRegistryReader }

  TcxRegistryReader = class(TcxCustomReader)
  private
    FRegistry: TRegistry;
  protected
    property Registry: TRegistry read FRegistry;
  public
    constructor Create(const AStorageName: string; AStorageStream: TStream); override;
    destructor Destroy; override;
    procedure ReadProperties(const AObjectName, AClassName: string; AProperties: TStrings); override;
    function ReadProperty(const AObjectName, AClassName, AName: string): Variant; override;
    procedure ReadChildren(const AObjectName, AClassName: string; AChildrenNames, AChildrenClassNames: TStrings); override;
  end;

  { TcxRegistryWriter }

  TcxRegistryWriter = class(TcxCustomWriter)
  private
    FRegistry: TRegistry;
    FRootKeyCreated: Boolean;
    FRootKeyOpened: Boolean;
    procedure CreateRootKey;
    function GetKeyName(const AObjectName: string): string;
  protected
    procedure ClearObjectData(const AObjectFullName, AClassName: string); override;

    property Registry: TRegistry read FRegistry;
  public
    constructor Create(const AStorageName: string; AStream: TStream; AReCreate: Boolean = True); overload; override;
    destructor Destroy; override;
    procedure BeginWriteObject(const AObjectName, AClassName: string); override;
    procedure EndWriteObject(const AObjectName, AClassName: string); override;
    procedure WriteProperty(const AObjectName, AClassName, AName: string; AValue: Variant); override;
  end;

  { TcxIniFileReader }

  TcxIniFileReader = class(TcxCustomReader)
  private
    FIniFile: TMemIniFile;
    FPathList: TStringList;
    FObjectNameList: TStringList;
    FClassNameList: TStringList;
    function DecodeString(S: string): string;
    procedure CreateLists;
    procedure GetSectionDetail(const ASection: string; var APath, AObjectName, AClassName: string);
  protected
    procedure BeginRead; override;
    function CanRead: Boolean; override;

    property IniFile: TMemIniFile read FIniFile;
  public
    constructor Create(const AStorageName: string; AStorageStream: TStream); override;
    destructor Destroy; override;
    procedure ReadProperties(const AObjectName, AClassName: string; AProperties: TStrings); override;
    function ReadProperty(const AObjectName, AClassName, AName: string): Variant; override;
    procedure ReadChildren(const AObjectName, AClassName: string; AChildrenNames,
        AChildrenClassNames: TStrings); override;
  end;

  { TcxIniFileWriter }

  TcxIniFileWriter = class(TcxCustomWriter)
  private const
    cxIniFileStorageVersion = 2;
  private
    FIniFile: TMemIniFile;

    function EncodeString(const S: string): string;
  protected
    procedure BeginWrite; override;
    procedure ClearObjectData(const AObjectFullName, AClassName: string); override;
    procedure EndWrite; override;

    property IniFile: TMemIniFile read FIniFile;
  public
    constructor Create(const AStorageName: string; AStream: TStream; AReCreate: Boolean = True); overload; override;
    destructor Destroy; override;
    procedure BeginWriteObject(const AObjectName, AClassName: string); override;
    procedure WriteProperty(const AObjectName, AClassName, AName: string; AValue: Variant); override;
  end;

type
  TcxStreamObjectData = class;
  TcxStreamPropertyData = class;

  { TcxStreamReader }

  TcxStreamReader = class(TcxCustomReader)
  private
    FCurrentObject: TcxStreamObjectData;
    FCurrentObjectFullName: string;
    FRootObject: TcxStreamObjectData;
    FReader: TReader;
    function GetObject(const AObjectFullName: string): TcxStreamObjectData;
    function GetProperty(AObject: TcxStreamObjectData; const AName: string): TcxStreamPropertyData;
    function InternalGetObject(const AObjectName: string; AParents: TStrings): TcxStreamObjectData;
  protected
    function CanRead: Boolean; override;
    procedure DoSetStream(AStream: TStream); override;
    procedure BeginRead; override;
  public
    destructor Destroy; override;
    procedure Read;
    procedure ReadProperties(const AObjectName, AClassName: string; AProperties: TStrings); override;
    function ReadProperty(const AObjectName, AClassName, AName: string): Variant; override;
    procedure ReadChildren(const AObjectName, AClassName: string; AChildrenNames,
        AChildrenClassNames: TStrings); override;
  end;

  { TcxStreamWriter }

  TcxStreamWriter = class(TcxCustomWriter)
  private
    FCurrentObject: TcxStreamObjectData;
    FRootObject: TcxStreamObjectData;
    FWriter: TWriter;
    procedure CreateObject(const AObjectName, AClassName: string; AParents: TStrings);
  protected
    function CanWrite: Boolean; override;
    procedure DoSetStream(AStream: TStream); override;
    procedure EndWrite; override;
  public
    destructor Destroy; override;
    procedure BeginWriteObject(const AObjectName, AClassName: string); override;
    procedure Write;
    procedure WriteProperty(const AObjectName, AClassName, AName: string; AValue: Variant); override;
  end;

  { TcxStreamPropertyData }

  TcxStreamPropertyData = class
  private
    FName: string;
    FValue: Variant;
    procedure ReadValue(AReader: TReader);
    procedure WriteValue(AWriter: TWriter);
  public
    constructor Create(const AName: string; const AValue: Variant);
    procedure Read(AReader: TReader);
    procedure Write(AWriter: TWriter);
    property Name: string read FName;
    property Value: Variant read FValue;
  end;

  { TcxStreamObjectData }

  TcxStreamObjectData = class
  private
    FClassName: string;
    FChildren: TList;
    FName: string;
    FProperties: TList;
    procedure Clear;
    function GetChildCount: Integer;
    function GetChildren(AIndex: Integer): TcxStreamObjectData;
    function GetProperties(AIndex: Integer): TcxStreamPropertyData;
    function GetPropertyCount: Integer;
  public
    constructor Create(const AName, AClassName: string);
    destructor Destroy; override;
    procedure AddChild(AChild: TcxStreamObjectData);
    procedure AddProperty(AProperty: TcxStreamPropertyData);
    procedure Read(AReader: TReader);
    procedure Write(AWriter: TWriter);
    property ChildCount: Integer read GetChildCount;
    property Children[AIndex: Integer]: TcxStreamObjectData read GetChildren;
    property Name: string read FName;
    property ClassName_: string read FClassName;
    property Properties[AIndex: Integer]: TcxStreamPropertyData read GetProperties;
    property PropertyCount: Integer read GetPropertyCount;
  end;

const
  cxBufferSize: Integer = 500000;

  cxStreamBoolean = 1;
  cxStreamChar = 2;
  cxStreamCurrency = 3;
  cxStreamDate = 4;
  cxStreamFloat = 5;
  cxStreamInteger = 6;
  cxStreamSingle = 7;
  cxStreamString = 8;
  cxStreamWideString = 9;

function cxQuotedStr(const S: string; AQuote: Char = '"'): string;
function cxDequotedStr(const S: string): string;
function cxIsQuotedStr(const S: string; AQuote: Char = '"'): Boolean;
function StreamToString(AStream: TStream): AnsiString;
procedure StringToStream(AValue: AnsiString; AStream: TStream);
function StringToHexString(const ABuffer: AnsiString): AnsiString;
function HexStringToString(const AHexString: AnsiString): AnsiString;
function StringToBoolean(const AString: string): Boolean;
function EnumerationToString(const AValue: Integer; const AEnumNames: array of string): string;
function StringToEnumeration(const AValue: string; const AEnumNames: array of string): Integer;
function SetToString(const ASet; ASize: Integer; const AEnumNames: array of string): string;
procedure StringToSet(AString: string; var ASet; ASize: Integer; const AEnumNames: array of string);

implementation

uses
  Controls, cxControls, dxDPIAwareUtils;

type
  TControlAccess = class(TControl);

function CompareChildrenNamesAsNumbers(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := StrToIntDef(List.Strings[Index1], -1) - StrToIntDef(List.Strings[Index2], -1);
end;

function cxQuotedStr(const S: string; AQuote: Char = '"'): string;
begin
  Result := AQuote + S + AQuote;
end;

function cxDequotedStr(const S: string): string;
begin
  Result := Copy(S, 2, Length(S) - 2);
end;

function cxIsQuotedStr(const S: string; AQuote: Char = '"'): Boolean;
begin
  Result := (Length(S) >= 2) and (S[1] = AQuote) and (S[Length(S)] = AQuote);
end;

function StreamToString(AStream: TStream): AnsiString;
begin
  if (AStream = nil) or (AStream.Size = 0) then
  begin
    Result := '';
    Exit;
  end;
  AStream.Position := 0;
  ReadStringFromStream(AStream, Result);
  Result := 'Hex:' + StringToHexString(Result);
end;

procedure StringToStream(AValue: AnsiString; AStream: TStream);
begin
  if (AStream = nil) or (Length(AValue) < 6) then Exit;
  Delete(AValue, 1, 4);
  if Length(AValue) > 0 then
  begin
    AValue := HexStringToString(AValue);
    WriteStringToStream(AStream, AValue);
  end;
end;

function StringToHexString(const ABuffer: AnsiString): AnsiString;
begin
  Result := dxBinToHex(ABuffer);
end;

function HexStringToString(const AHexString: AnsiString): AnsiString;
begin
  Result := dxHexToBin(AHexString);
end;

function StringToBoolean(const AString: string): Boolean;
begin
  if UpperCase(AString) = 'TRUE' then
    Result := True
  else if UpperCase(AString) = 'FALSE' then
    Result := False
  else
    raise EPropertyConvertError.Create('');
end;

function EnumerationToString(const AValue: Integer; const AEnumNames: array of string): string;
begin
  if (AValue >= Low(AEnumNames)) and (AValue <= High(AEnumNames)) then
    Result := AEnumNames[AValue]
  else
    raise EPropertyConvertError.Create('');
end;

function StringToEnumeration(const AValue: string; const AEnumNames: array of string): Integer;
var
  I: Integer;
begin
  for I := Low(AEnumNames) to High(AEnumNames) do
    if SameText(AValue, AEnumNames[I]) then
    begin
      Result := I;
      Exit;
    end;
  raise EPropertyConvertError.Create('');
end;

function SetToString(const ASet; ASize: Integer; const AEnumNames: array of string): string;
var
  AInt: Integer;
  I: Integer;
begin
  AInt := Integer(ASet);
  if ASize < SizeOf(Integer) then
    AInt := AInt and (1 shl (ASize * 8) - 1);
  Result := '';
  for I := 0 to SizeOf(Integer) * 8 - 1 do
  begin
    if AInt and 1 <> 0 then
    begin
      if I > High(AEnumNames) then
        raise EPropertyConvertError.Create('');
      if Result <> '' then
        Result := Result + ',';
      Result := Result + AEnumNames[I];
    end;
    AInt := AInt shr 1;
  end;
  Result := cxQuotedStr(Result, ']');
end;

procedure StringToSet(AString: string; var ASet; ASize: Integer; const AEnumNames: array of string);

  function FindEnum(const AStr: string): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := Low(AEnumNames) to High(AEnumNames) do
      if SameText(AStr, AEnumNames[I]) then
      begin
        Result := I;
        Break;
      end;
  end;

var
  AInt: Integer;

  procedure AddBit(const AStr: string);
  var
    AIndex: Integer;
  begin
    AIndex := FindEnum(AStr);
    if AIndex <> -1 then
    begin
      AIndex := 1 shl AIndex;
      AInt := AInt or AIndex;
    end;
  end;

var
  I: Integer;
  AEnumString: string;
begin
  if cxIsQuotedStr(AString, '[') then
  begin
    AInt := 0;
    AEnumString := '';
    AString := cxDequotedStr(AString);
    for I := 1 to Length(AString) do
    begin
      if AString[I] = ',' then
      begin
        AddBit(AEnumString);
        AEnumString := '';
      end
      else
        AEnumString := AEnumString + AString[I];
    end;
    if AEnumString <> '' then
      AddBit(AEnumString);
    Move(AInt, ASet, ASize);
  end
  else
    raise EPropertyConvertError.Create('');
end;

function GetRegistryKey(const AKey: string): DWORD;
const
  AKeyMap: array [0..6] of string = ('HKCR', 'HKCU', 'HKLM', 'HKU', 'HKPD', 'HKCC', 'HKDD');
var
  I: DWORD;
begin
  Result := 0;
  for I := Low(AKeyMap) to High(AKeyMap) do
    if AKey = AKeyMap[I] then
    begin
      Result := HKEY_CLASSES_ROOT + I;
      Break;
    end;
end;

function FindRegistryKey(const AKeyPath: string): DWORD;
var
  ADelimiterPos: Integer;
begin
  Result := 0;
  ADelimiterPos := Pos('\', AKeyPath);
  if ADelimiterPos > 0 then
    Result := GetRegistryKey(Copy(AKeyPath, 1, ADelimiterPos - 1));
end;

function GenRegistryPath(const ARoot: string): string;
var
  ADelimiterPos: Integer;
begin
  Result := ARoot;
  if Length(Result) > 0 then
  begin
    ADelimiterPos := Pos('\', ARoot);
    if ADelimiterPos <> 1 then
    begin
      if GetRegistryKey(Copy(ARoot, 1, ADelimiterPos - 1)) > 0 then
        Result := Copy(ARoot, ADelimiterPos, MaxInt)
      else
        Result := '\' + Result;
    end;
  end;
end;

function DateTimeOrStr(const AValue: string): Variant;
var
  ADateTimeValue: TDateTime;
begin
  if TryStrToDateTime(AValue, ADateTimeValue) then
    Result := ADateTimeValue
  else
    Result := AValue;
end;

procedure ExtractObjectFullName(const AObjectFullName: string; AParents: TStrings; var AObjectName: string);
var
  I: Integer;
  AName: string;
begin
  if AParents <> nil then
  begin
    AObjectName := '';
    AName := '';
    for I := 1 to Length(AObjectFullName) do
    begin
      if AObjectFullName[I] = '/' then
      begin
        AParents.Add(AName);
        AName := '';
      end
      else
        AName := AName + AObjectFullName[I];
    end;
    AObjectName := AName;
  end;
end;

function IsStringValue(const AValue: string): Boolean;
begin
  Result := cxIsQuotedStr(AValue);
end;

{ TcxStorage }

constructor TcxStorage.Create(const AStorageName: string; AStorageStream: TStream);
begin
  inherited Create;
  FStorageName := AStorageName;
  FStream := AStorageStream;
  FReCreate := True;
end;

constructor TcxStorage.Create(const AStorageName: string);
begin
  Create(AStorageName, nil);
end;

constructor TcxStorage.Create(AStream: TStream);
begin
  Create('', AStream);
end;

procedure TcxStorage.RestoreFrom(AObject: TObject; AReaderClass: TcxCustomReaderClass);
var
  AReader: TcxCustomReader;
begin
  AReader := AReaderClass.Create(StorageName, FStream);
  try
    RestoreWithExistingReader(AObject, AReader);
  finally
    AReader.Free;
  end;
end;

procedure TcxStorage.RestoreWithExistingReader(AObject: TObject; AReader: TcxCustomReader);
var
  AOriginalParentSize: TPoint;
begin
  if (AReader <> nil) and AReader.CanRead then
  begin
    SetStoredObject(AObject);
    AReader.BeginRead;
    try
      AOriginalParentSize := TcxControlHelper.GetOriginalParentSize(AObject);
      InternalRestoreFrom(AReader);
      if TcxControlHelper.GetCurrentDPIFromControl(AObject) <> StoredDPI then
        TcxControlHelper.SetOriginalParentSize(AObject, AOriginalParentSize);
    finally
      AReader.EndRead;
    end;
  end;
end;

procedure TcxStorage.RestoreFromIni(AObject: TObject);
begin
  RestoreFrom(AObject, TcxIniFileReader);
end;

procedure TcxStorage.RestoreFromRegistry(AObject: TObject);
begin
  RestoreFrom(AObject, TcxRegistryReader);
end;

procedure TcxStorage.RestoreFromStream(AObject: TObject);
begin
  RestoreFrom(AObject, TcxStreamReader);
end;

procedure TcxStorage.StoreTo(AObject: TObject; AWriterClass: TcxCustomWriterClass);
var
  AWriter: TcxCustomWriter;
begin
  AWriter := AWriterClass.Create(StorageName, FStream, ReCreate);
  try
    StoreWithExistingWriter(AObject, AWriter);
  finally
    AWriter.Free;
  end;
end;

procedure TcxStorage.StoreWithExistingWriter(AObject: TObject; AWriter: TcxCustomWriter);
begin
  if (AWriter <> nil) and AWriter.CanWrite then
  begin
    SetStoredObject(AObject);
    AWriter.BeginWrite;
    try
      ClearObjectData(AWriter);
      InternalStoreTo(AWriter);
    finally
      AWriter.EndWrite;
    end;
  end;
end;

procedure TcxStorage.StoreToIni(AObject: TObject);
begin
  StoreTo(AObject, TcxIniFileWriter);
end;

procedure TcxStorage.StoreToRegistry(AObject: TObject);
begin
  StoreTo(AObject, TcxRegistryWriter);
end;

procedure TcxStorage.StoreToStream(AObject: TObject);
begin
  StoreTo(AObject, TcxStreamWriter);
end;

function TcxStorage.CreateChild(const AObjectName, AClassName: string): TObject;
var
  AInterface: IcxStoredParent;
begin
  Result := nil;
  if Supports(FStoredObject, IcxStoredParent, AInterface) then
    Result := AInterface.CreateChild(AObjectName, AClassName);
  if Result = nil then
  begin
    if FStoredObject is TCollection then
      Result := (FStoredObject as TCollection).Add
  end;
end;

procedure TcxStorage.CreateChildrenNames(AChildren: TStringList);
var
  I: Integer;
begin
  for I := 0 to AChildren.Count - 1 do
    if AChildren[I] = '' then
      AChildren[I] := GetObjectName(AChildren.Objects[I]);
end;

procedure TcxStorage.DeleteChild(const AObjectName: string; AObject: TObject);
var
  AInterface: IcxStoredParent;
  AInterface2: IcxStoredObject2;
begin
  if Supports(FStoredObject, IcxStoredParent, AInterface) and
      (not Supports(AObject, IcxStoredObject2, AInterface2) or AInterface2.CanDelete) then
    AInterface.DeleteChild(AObjectName, AObject)
  else
    if FStoredObject is TCollection then
      AObject.Free;
end;

procedure TcxStorage.GetAllPublishedClassProperties(AProperties: TStrings);
var
  APropList: PPropList;
  ATypeInfo: PTypeInfo;
  ATypeData: PTypeData;
  I: Integer;
begin
  ATypeInfo := FStoredObject.ClassInfo;
  if ATypeInfo = nil then
    Exit;
  ATypeData := GetTypeData(ATypeInfo);

  if ATypeData.PropCount > 0 then
  begin
    GetMem(APropList, SizeOf(PPropInfo) * ATypeData.PropCount);
    try
      GetPropInfos(ATypeInfo, APropList);
      for I := 0 to ATypeData.PropCount - 1 do
        if APropList[I].PropType^.Kind = tkClass then
          AProperties.Add(dxShortStringToString(APropList[I].Name));
    finally
      FreeMem(APropList, SizeOf(PPropInfo) * ATypeData.PropCount);
    end;
  end;
end;

procedure TcxStorage.GetAllPublishedProperties(AProperties: TStrings);
var
  APropList: PPropList;
  ATypeInfo: PTypeInfo;
  ATypeData: PTypeData;
  I: Integer;
begin
  ATypeInfo := FStoredObject.ClassInfo;
  if ATypeInfo = nil then
    Exit;
  ATypeData := GetTypeData(ATypeInfo);

  if ATypeData.PropCount > 0 then
  begin
    GetMem(APropList, SizeOf(PPropInfo) * ATypeData.PropCount);
    try
      GetPropInfos(ATypeInfo, APropList);
      for I := 0 to ATypeData.PropCount - 1 do
        if APropList[I].PropType^.Kind <> tkMethod then
          AProperties.Add(dxShortStringToString(APropList[I].Name));
    finally
      FreeMem(APropList, SizeOf(PPropInfo) * ATypeData.PropCount);
    end;
  end;
end;

procedure TcxStorage.GetChildren(AChildren: TStringList);
var
  AInterface: IcxStoredParent;
  I: Integer;
  AClassProperties: TStringList;
  AClassProperty: TObject;
begin
  if Supports(FStoredObject, IcxStoredParent, AInterface) then
    AInterface.GetChildren(AChildren);
  if smSavePublishedClassProperties in GetStorageModes then
  begin
    AClassProperties := TStringList.Create;
    try
      if (FStoredObject is TCollection) and not Supports(FStoredObject, IcxStoredParent) then
      begin
        for I := 0 to TCollection(FStoredObject).Count - 1 do
          AChildren.AddObject(IntToStr(I), TCollection(FStoredObject).Items[I]);
      end;
      GetAllPublishedClassProperties(AClassProperties);
      for I := 0 to AClassProperties.Count - 1 do
      begin
        AClassProperty := GetClassProperty(AClassProperties[I]);
        if AClassProperty <> nil then
          if TestClassProperty(AClassProperties[I], AClassProperty) then
            AChildren.AddObject(AClassProperties[I], AClassProperty);
      end;
    finally
      AClassProperties.Free;
    end;
  end;
end;

function TcxStorage.GetClassProperty(const AName: string): TObject;
var
  ATypeInfo: PTypeInfo;
  APropInfo: PPropInfo;
begin
  Result := nil;
  ATypeInfo := FStoredObject.ClassInfo;
  if ATypeInfo = nil then
    Exit;
  APropInfo := GetPropInfo(ATypeInfo, AName);
  if APropInfo <> nil then
    if APropInfo^.PropType^.Kind = tkClass then
      Result := GetObjectProp(FStoredObject, APropInfo);
end;

function TcxStorage.GetComponentByName(const AName: string): TComponent;
begin
  if Assigned(FOnGetComponentByName) then
    Result := FOnGetComponentByName(AName)
  else
    Result := nil;
end;

function TcxStorage.GetObjectName(AObject: TObject): string;
var
  AInterface: IcxStoredObject;
  AObj: TObject;
begin
  if AObject <> nil then
    AObj := AObject
  else
    AObj := FStoredObject;

  if Supports(AObj, IcxStoredObject, AInterface) then
    Result := AInterface.GetObjectName
  else
    if AObj is TComponent then
      Result := (AObj as TComponent).Name
    else
      Result := 'Object';
end;

procedure TcxStorage.GetProperties(AProperties: TStrings);
var
  AInterface: IcxStoredObject;
begin
  if Supports(FStoredObject, IcxStoredObject, AInterface) then
  begin
    if not AInterface.GetProperties(AProperties) then
      GetAllPublishedProperties(AProperties);
  end
  else
    GetAllPublishedProperties(AProperties);
end;

function TcxStorage.GetPropertyValue(const AName: string): Variant;

  procedure GetPropertyValueByInterface;
  var
    AInterface: IcxStoredObject;
  begin
    if Supports(FStoredObject, IcxStoredObject, AInterface) then
      AInterface.GetPropertyValue(AName, Result);
  end;

  procedure BooleanVariantToStringVariant(var AValue: Variant);
  begin
    if VarType(AValue) = varBoolean then
    begin
      if AValue then
        AValue := 'True'
      else
        AValue := 'False';
    end;
  end;

var
  ATypeInfo: PTypeInfo;
  APropInfo: PPropInfo;
  AObject: TObject;
begin
  Result := Null;
  if not GetUseInterfaceOnly then
  begin
    ATypeInfo := FStoredObject.ClassInfo;
    if ATypeInfo <> nil then
    begin
      APropInfo := GetPropInfo(ATypeInfo, AName);
      if APropInfo <> nil then
      begin
        case APropInfo^.PropType^.Kind of
          tkInteger, tkChar, tkWChar:
            Result := GetOrdProp(FStoredObject, APropInfo);
          tkEnumeration:
            Result := GetEnumProp(FStoredObject, APropInfo);
          tkFloat:
            Result := GetFloatProp(FStoredObject, APropInfo);
          tkSet:
            Result := GetSetProp(FStoredObject, APropInfo, True);
          tkVariant:
            Result := GetVariantProp(FStoredObject, APropInfo);
          tkString, tkLString, tkUString:
            Result := GetStrProp(FStoredObject, APropInfo);
          tkWString:
            Result := GetWideStrProp(FStoredObject, APropInfo);
          tkInt64:
            Result := GetInt64Prop(FStoredObject, APropInfo);
          tkClass:
          begin
            if FSaveComponentPropertiesByName then
            begin
              AObject := GetObjectProp(FStoredObject, APropInfo);
              if AObject is TComponent then
                Result := (AObject as TComponent).Name;
            end
            else
              GetPropertyValueByInterface;
          end;
          else
            GetPropertyValueByInterface;
        end;
      end
      else
        GetPropertyValueByInterface;
    end
    else
      GetPropertyValueByInterface;
  end
  else
    GetPropertyValueByInterface;
  BooleanVariantToStringVariant(Result);
end;

function TcxStorage.GetStorageModes: TcxStorageModes;
begin
  if Assigned(FOnGetStorageModes) then
    Result := FOnGetStorageModes
  else
    Result := FModes;
end;

function TcxStorage.GetUseInterfaceOnly: Boolean;
begin
  if Assigned(FOnGetUseInterfaceOnly) then
    Result := FOnGetUseInterfaceOnly
  else
    Result := FUseInterfaceOnly;
end;

procedure TcxStorage.ClearObjectData(AWriter: TcxCustomWriter);
var
  AObjectFullName: string;
begin
  AObjectFullName := GetFullObjectName;
  AWriter.ClearObjectData(AObjectFullName, FStoredObject.ClassName);
end;

function TcxStorage.GetFullObjectName(const ADefaultObjectName: string = ''): string;
var
  AObjectName: string;
begin
  if ADefaultObjectName <> '' then
    AObjectName := ADefaultObjectName
  else
    AObjectName := GetObjectName(nil);
  if FNamePrefix <> '' then
    AObjectName := FNamePrefix + '.' + AObjectName;
  Result := FObjectNamePrefix + AObjectName;
end;

procedure TcxStorage.InternalCheckChildrenOrder(AChildrenNames, AChildrenClassNames: TStringList);
var
  AIndex: Integer;
  APrevChildrenClassNames: TStringList;
  APrevChildrenNames: TStringList;
  I: Integer;
begin
  if (FStoredObject is TCollection) and not Supports(FStoredObject, IcxStoredParent) then
  begin
    APrevChildrenClassNames := TStringList.Create;
    APrevChildrenNames := TStringList.Create;
    try
      APrevChildrenClassNames.Assign(AChildrenClassNames);
      APrevChildrenNames.Assign(AChildrenNames);

      AChildrenNames.CustomSort(CompareChildrenNamesAsNumbers);

      AChildrenClassNames.BeginUpdate;
      try
        AChildrenClassNames.Clear;
        AChildrenClassNames.Capacity := APrevChildrenClassNames.Capacity;
        for I := 0 to AChildrenNames.Count - 1 do
        begin
          AIndex := APrevChildrenNames.IndexOf(AChildrenNames[I]);
          AChildrenClassNames.AddObject(APrevChildrenClassNames[AIndex], APrevChildrenClassNames.Objects[AIndex]);
        end;
      finally
        AChildrenClassNames.EndUpdate;
      end;
    finally
      APrevChildrenClassNames.Free;
      APrevChildrenNames.Free;
    end;
  end;
end;

procedure TcxStorage.InternalRestoreFrom(AReader: TcxCustomReader; const ADefaultObjectName: string = '');
var
  AChildObject: TObject;
  AChildObjectName: string;
  AChildren: TStringList;
  AChildrenClassNames: TStringList;
  AChildrenNames: TStringList;
  AIndex: Integer;
  AObjectFullName: string;
  APropertyName: string;
  AProperties: TStringList;
  AStorage: TcxStorage;
  AValue: Variant;
  I: Integer;
begin
  StoringBegin;
  try
    AProperties := TStringList.Create;
    AChildrenNames := TStringList.Create;
    AChildrenClassNames := TStringList.Create;
    try
      AObjectFullName := GetFullObjectName(ADefaultObjectName);

      AReader.ReadProperties(AObjectFullName, FStoredObject.ClassName, AProperties);

      AValue := AReader.ReadProperty(AObjectFullName, FStoredObject.ClassName, sPropertySourceDPI);
      if VarIsNumeric(AValue) then
      begin
        FStoredDPI := AValue;
        ScaleStoredObjectForPPI(AValue);
      end;

      for I := 0 to AProperties.Count - 1 do
      begin
        APropertyName := AProperties[I];
        if APropertyName = '' then
          Continue;
        AValue := AReader.ReadProperty(AObjectFullName, FStoredObject.ClassName, APropertyName);
        if not VarIsNull(AValue) then
        try
          SetPropertyValue(APropertyName, AValue);
        except
          on EAbort do
          else
            if not (Assigned(FOnSetStoredPropertyValueError) and FOnSetStoredPropertyValueError(Self, APropertyName, AValue)) then
              raise;
        end;
      end;

      AReader.ReadChildren(AObjectFullName, FStoredObject.ClassName, AChildrenNames, AChildrenClassNames);
      InternalCheckChildrenOrder(AChildrenNames, AChildrenClassNames);

      AChildren := TStringList.Create;
      try
        GetChildren(AChildren);
        CreateChildrenNames(AChildren);
        for I := 0 to AChildrenNames.Count - 1 do
        begin
          AIndex := AChildren.IndexOf(AChildrenNames[I]);
          if AIndex >= 0 then
          begin
            AChildObject := AChildren.Objects[AIndex];
            AChildObjectName := AChildren[AIndex];
            AChildren.Delete(AIndex);
          end
          else
          begin
            if smChildrenCreating in GetStorageModes then
            begin
              AChildObject := CreateChild(AChildrenNames[I], AChildrenClassNames[I]);
              AChildObjectName := AChildrenNames[I];
            end
            else
              AChildObject := nil;
          end;

          if AChildObject <> nil then
          begin
            AStorage := TcxStorage.Create('');
            AStorage.FObjectNamePrefix := AObjectFullName + '/';
            AStorage.FNamePrefix := '';
            AStorage.FStoredObject := AChildObject;
            AStorage.OnTestClassProperty := FOnTestClassProperty;
            AStorage.OnGetComponentByName := FOnGetComponentByName;
            AStorage.OnSetStoredPropertyValueError := FOnSetStoredPropertyValueError;
            AStorage.FModes := Modes;
            AStorage.SaveComponentPropertiesByName := FSaveComponentPropertiesByName;
            try
              AStorage.InternalRestoreFrom(AReader, AChildObjectName);
            finally
              AStorage.Free;
            end;
          end;
        end;

        if smChildrenDeleting in GetStorageModes then
        begin
          if AChildren.Count > 0 then
          begin
            for I := 0 to AChildren.Count - 1 do
              DeleteChild(AChildren[I], AChildren.Objects[I]);
          end;
        end;
      finally
        AChildren.Free;
      end;
    finally
      AProperties.Free;
      AChildrenNames.Free;
      AChildrenClassNames.Free;
    end;
  finally
    StoringEnd;
  end;
end;

procedure TcxStorage.InternalStoreTo(AWriter: TcxCustomWriter; const ADefaultObjectName: string = '');
var
  AChildren: TStringList;
  AObjectFullName: string;
  AProperties: TStringList;
  APropertyValue: Variant;
  AStorage: TcxStorage;
  I: Integer;
begin
  AProperties := TStringList.Create;
  try
    StoringBegin;
    try
      AObjectFullName := GetFullObjectName(ADefaultObjectName);
      AWriter.BeginWriteObject(AObjectFullName, FStoredObject.ClassName);

      if FStoredObjectPrevDPI > 0 then
        AWriter.WriteProperty(AObjectFullName, FStoredObject.ClassName, sPropertySourceDPI, FStoredObjectPrevDPI);

      GetProperties(AProperties);
      for I := 0 to AProperties.Count - 1 do
      begin
        APropertyValue := GetPropertyValue(AProperties[I]);
        if not (VarIsEmpty(APropertyValue) or VarIsNull(APropertyValue)) then
          AWriter.WriteProperty(AObjectFullName, FStoredObject.ClassName, AProperties[I], APropertyValue);
      end;

      AChildren := TStringList.Create;
      try
        GetChildren(AChildren);
        for I := 0 to AChildren.Count - 1 do
        begin
          AStorage := TcxStorage.Create('');
          AStorage.FObjectNamePrefix := AObjectFullName + '/';
          AStorage.FNamePrefix := '';
          AStorage.FStoredObject := AChildren.Objects[I];
          AStorage.OnTestClassProperty := FOnTestClassProperty;
          AStorage.OnGetComponentByName := FOnGetComponentByName;
          AStorage.Modes := Modes;
          AStorage.SaveComponentPropertiesByName := FSaveComponentPropertiesByName;
          try
            AStorage.InternalStoreTo(AWriter, AChildren[I]);
          finally
            AStorage.Free;
          end;
        end;
      finally
        AChildren.Free;
      end;
      AWriter.EndWriteObject(AObjectFullName, FStoredObject.ClassName);
    finally
      StoringEnd;
    end;
  finally
    AProperties.Free;
  end;
end;

procedure TcxStorage.ScaleStoredObjectForPPI(ATargetPPI: Integer);
var
  AScaleFactor: IdxScaleFactor;
begin
  if (ATargetPPI > dxMinDPI) and
    (not Supports(FStoredObject, IdxScaleFactor, AScaleFactor) or (AScaleFactor.Value.TargetDPI <> ATargetPPI)) then
      TcxControlHelper.ScaleForPPI(FStoredObject, ATargetPPI);
end;

procedure TcxStorage.SetPropertyValue(const AName: string; AValue: Variant);

  procedure SetPropertyValueByInterface;
  var
    AInterface: IcxStoredObject;
  begin
    if Supports(FStoredObject, IcxStoredObject, AInterface) then
      AInterface.SetPropertyValue(AName, AValue);
  end;

var
  ATypeInfo: PTypeInfo;
  APropInfo: PPropInfo;
  AInt64: Int64;
  AClass: TClass;
  AComponent: TComponent;
begin
  if not VarIsNull(AValue) then
  begin
    if not GetUseInterfaceOnly then
    begin
      ATypeInfo := FStoredObject.ClassInfo;
      if ATypeInfo <> nil then
      begin
        APropInfo := GetPropInfo(ATypeInfo, AName);
        if APropInfo <> nil then
        begin
          case APropInfo^.PropType^.Kind of
            tkInteger, tkChar, tkWChar:
              SetOrdProp(FStoredObject, APropInfo, AValue);
            tkEnumeration:
              SetEnumProp(FStoredObject, APropInfo, AValue);
            tkFloat:
              SetFloatProp(FStoredObject, APropInfo, AValue);
            tkString, tkLString, tkUString:
              SetStrProp(FStoredObject, AName, VarToStr(AValue));
            tkWString:
              SetWideStrProp(FStoredObject, APropInfo, AValue);
            tkInt64:
            begin
              AInt64 := AValue;
              SetInt64Prop(FStoredObject, APropInfo, AInt64);
            end;
            tkSet:
              SetSetProp(FStoredObject, APropInfo, AValue);
            tkVariant:
              SetVariantProp(FStoredObject, APropInfo, AValue);
            tkClass:
            begin
              if FSaveComponentPropertiesByName then
              begin
                if VarIsStr(AValue) then
                begin
                  AClass := GetObjectPropClass(FStoredObject, APropInfo);
                  if (AClass = TComponent) or (AClass.InheritsFrom(TComponent)) then
                  begin
                    AComponent := GetComponentByName(VarToStr(AValue));
                    if AComponent <> nil then
                      SetObjectProp(FStoredObject, APropInfo, AComponent);
                  end
                  else
                    SetPropertyValueByInterface;
                end
                else
                 SetPropertyValueByInterface;
              end
              else
                SetPropertyValueByInterface;
            end;
            else
              SetPropertyValueByInterface;
          end;
        end
        else
          SetPropertyValueByInterface;
      end
      else
        SetPropertyValueByInterface;
    end
    else
      SetPropertyValueByInterface;
  end;
end;

procedure TcxStorage.SetStoredObject(AObject: TObject);
begin
  FStoredObject := AObject;
end;

procedure TcxStorage.StoringBegin;
var
  AIntf: IcxStoredParent2;
  AScaleFactor: IdxScaleFactor;
begin
  if Supports(FStoredObject, IdxScaleFactor, AScaleFactor) then
    FStoredObjectPrevDPI := AScaleFactor.Value.TargetDPI
  else
    FStoredObjectPrevDPI := 0;

  if Supports(FStoredObject, IcxStoredParent2, AIntf) then
    AIntf.StoringBegin;
end;

procedure TcxStorage.StoringEnd;
var
  AIntf: IcxStoredParent2;
begin
  ScaleStoredObjectForPPI(FStoredObjectPrevDPI);
  if Supports(FStoredObject, IcxStoredParent2, AIntf) then
    AIntf.StoringEnd;
end;

function TcxStorage.TestClassProperty(const AName: string; AObject: TObject): Boolean;
begin
  if Assigned(FOnTestClassProperty) then
    Result := FOnTestClassProperty(AName, AObject)
  else
    Result := True;
end;

{ TcxCustomReader }

constructor TcxCustomReader.Create(const AStorageName: string);
begin
  Create(AStorageName, nil);
end;

constructor TcxCustomReader.Create(const AStorageName: string; AStorageStream: TStream);
begin
  inherited Create;
  FStorageName := AStorageName;
  SetStream(AStorageStream);
end;

procedure TcxCustomReader.ReadChildren(const AObjectName, AClassName: string;
  AChildrenNames, AChildrenClassNames: TStrings);
begin
end;

procedure TcxCustomReader.ReadProperties(const AObjectName, AClassName: string; AProperties: TStrings);
begin
end;

function TcxCustomReader.ReadProperty(const AObjectName, AClassName, AName: string): Variant;
begin
  Result := Null;
end;

procedure TcxCustomReader.SetStream(AStream: TStream);
begin
  if FStorageStream <> AStream then
    DoSetStream(AStream);
end;

function TcxCustomReader.CanRead: Boolean;
begin
  Result := True;
end;

procedure TcxCustomReader.DoSetStream(AStream: TStream);
begin
  FStorageStream := AStream;
end;

procedure TcxCustomReader.BeginRead;
begin

end;

procedure TcxCustomReader.EndRead;
begin

end;

{ TcxCustomWriter }

constructor TcxCustomWriter.Create(const AStorageName: string; AReCreate: Boolean);
begin
  Create(AStorageName, nil, AReCreate);
end;

constructor TcxCustomWriter.Create(const AStorageName: string; AStorageStream: TStream; AReCreate: Boolean = True);
begin
  inherited Create;
  FStorageName := AStorageName;
  FReCreate := AReCreate;
  SetStream(AStorageStream);
end;

procedure TcxCustomWriter.BeginWriteObject(const AObjectName, AClassName: string);
begin
end;

procedure TcxCustomWriter.EndWriteObject(const AObjectName, AClassName: string);
begin
end;

procedure TcxCustomWriter.SetStream(AStream: TStream);
begin
  if FStorageStream <> AStream then
    DoSetStream(AStream);
end;

procedure TcxCustomWriter.WriteProperty(const AObjectName, AClassName, AName: string; AValue: Variant);
begin
end;

function TcxCustomWriter.CanWrite: Boolean;
begin
  Result := True;
end;

procedure TcxCustomWriter.ClearObjectData(const AObjectFullName, AClassName: string);
begin
end;

procedure TcxCustomWriter.DoSetStream(AStream: TStream);
begin
  FStorageStream := AStream;
end;

procedure TcxCustomWriter.BeginWrite;
begin
end;

procedure TcxCustomWriter.EndWrite;
begin
end;

{ TcxStreamReader }

destructor TcxStreamReader.Destroy;
begin
  FreeAndNil(FReader);
  FreeAndNil(FRootObject);
  inherited Destroy;
end;

procedure TcxStreamReader.Read;
begin
  FRootObject.Free;
  FRootObject := TcxStreamObjectData.Create('', '');

  if (FStorageStream <> nil) and (FReader.Position < FStorageStream.Size) then
    FRootObject.Read(FReader);
end;

procedure TcxStreamReader.ReadChildren(const AObjectName, AClassName: string;
  AChildrenNames, AChildrenClassNames: TStrings);
var
  I: Integer;
  AObject: TcxStreamObjectData;
begin
  AObject := GetObject(AObjectName);
  if AObject <> nil then
  begin
    for I := 0 to AObject.ChildCount - 1 do
    begin
      AChildrenNames.Add(AObject.Children[I].Name);
      AChildrenClassNames.Add(AObject.Children[I].ClassName_);
    end;
  end;
end;

procedure TcxStreamReader.ReadProperties(const AObjectName, AClassName: string; AProperties: TStrings);
var
  AObject: TcxStreamObjectData;
  I: Integer;
begin
  AObject := GetObject(AObjectName);
  if AObject <> nil then
  begin
    for I := 0 to AObject.PropertyCount - 1 do
      AProperties.Add(AObject.Properties[I].Name);
  end;
end;

function TcxStreamReader.ReadProperty(const AObjectName, AClassName, AName: string): Variant;
var
  AProperty: TcxStreamPropertyData;
begin
  AProperty := GetProperty(GetObject(AObjectName), AName);
  if AProperty <> nil then
    Result := AProperty.Value
  else
    Result := Null;
end;

function TcxStreamReader.CanRead: Boolean;
begin
  Result := (FStorageStream <> nil) and (FStorageStream.Size > 0);
end;

procedure TcxStreamReader.DoSetStream(AStream: TStream);
begin
  inherited;
  FReader.Free;
  FReader := TReader.Create(AStream, cxBufferSize);
end;

procedure TcxStreamReader.BeginRead;
begin
  Read;
end;

function TcxStreamReader.GetObject(const AObjectFullName: string): TcxStreamObjectData;
var
  AObjectName: string;
  AParents: TStringList;
begin
  if AObjectFullName = FCurrentObjectFullName then
    Result := FCurrentObject
  else
  begin
    AParents := TStringList.Create;
    try
      ExtractObjectFullName(AObjectFullName, AParents, AObjectName);
      Result := InternalGetObject(AObjectName, AParents);
      if Result <> nil then
      begin
        FCurrentObjectFullName := AObjectFullName;
        FCurrentObject := Result;
      end;
    finally
      AParents.Free;
    end;
  end;
end;

function TcxStreamReader.GetProperty(AObject: TcxStreamObjectData; const AName: string): TcxStreamPropertyData;
var
  I: Integer;
begin
  Result := nil;
  if AObject <> nil then
    for I := 0 to AObject.PropertyCount - 1 do
    begin
      if AObject.Properties[I].Name = AName then
        Exit(AObject.Properties[I]);
    end;
end;

function TcxStreamReader.InternalGetObject(const AObjectName: string; AParents: TStrings): TcxStreamObjectData;
var
  I, J: Integer;
  AObject: TcxStreamObjectData;
begin
  AParents.Add(AObjectName);
  AObject := FRootObject;
  for I := 1 to AParents.Count - 1 do
  begin
    for J := 0 to AObject.ChildCount - 1 do
    begin
      if AParents[I] = AObject.Children[J].Name then
      begin
        AObject := AObject.Children[J];
        Break;
      end;
    end;
  end;

  if AObject.Name = AObjectName then
    Result := AObject
  else
    Result := nil;
end;

{ TcxStreamWriter }

destructor TcxStreamWriter.Destroy;
begin
  FreeAndNil(FWriter);
  FreeAndNil(FRootObject);
  inherited Destroy;
end;

procedure TcxStreamWriter.BeginWriteObject(const AObjectName, AClassName: string);
var
  AName: string;
  AParents: TStringList;
begin
  AParents := TStringList.Create;
  try
    ExtractObjectFullName(AObjectName, AParents, AName);
    CreateObject(AName, AClassName, AParents);
  finally
    AParents.Free;
  end;
end;

procedure TcxStreamWriter.Write;
begin
  if FRootObject <> nil then
    FRootObject.Write(FWriter);
  FRootObject.Free;
  FRootObject := nil;
  FCurrentObject := nil;
end;

procedure TcxStreamWriter.WriteProperty(const AObjectName, AClassName, AName: string; AValue: Variant);
begin
  if FCurrentObject <> nil then
    FCurrentObject.AddProperty(TcxStreamPropertyData.Create(AName, AValue));
end;

function TcxStreamWriter.CanWrite: Boolean;
begin
  Result := FStorageStream <> nil;
end;

procedure TcxStreamWriter.DoSetStream(AStream: TStream);
begin
  inherited;
  FWriter.Free;
  FWriter := TWriter.Create(FStorageStream, cxBufferSize);
end;

procedure TcxStreamWriter.EndWrite;
begin
   Write;
end;

procedure TcxStreamWriter.CreateObject(const AObjectName, AClassName: string; AParents: TStrings);
var
  I, J: Integer;
  AObject: TcxStreamObjectData;
  ANewObject: TcxStreamObjectData;
begin
  if (FRootObject = nil) and (FCurrentObject = nil) then
  begin
    if AParents.Count = 0 then
    begin
      FRootObject := TcxStreamObjectData.Create(AObjectName, AClassName);
      FCurrentObject := FRootObject;
    end;
  end
  else
  begin
    AObject := FRootObject;
    for I := 1 to AParents.Count - 1 do
    begin
      for J := 0 to AObject.ChildCount - 1 do
      begin
        if AParents[I] = AObject.Children[J].Name then
        begin
          AObject := AObject.Children[J];
          Break;
        end;
      end;
    end;
    ANewObject := TcxStreamObjectData.Create(AObjectName, AClassName);
    FCurrentObject := ANewObject;
    AObject.AddChild(ANewObject);
  end;
end;

{ TcxRegistryReader }

constructor TcxRegistryReader.Create(const AStorageName: string; AStorageStream: TStream);
begin
  inherited Create(AStorageName, AStorageStream);

  FRegistry := TRegistry.Create(KEY_READ);
  if FindRegistryKey(StorageName) <> 0 then
    FRegistry.RootKey := FindRegistryKey(StorageName);
  if not Registry.OpenKey(GenRegistryPath(StorageName), False) then
  //  raise ERegistryException.CreateFmt(cxGetResourceString(@scxCantOpenRegistryKey), [StorageName]);
end;

destructor TcxRegistryReader.Destroy;
begin
  FreeAndNil(FRegistry);

  inherited Destroy;
end;

procedure TcxRegistryReader.ReadChildren(const AObjectName, AClassName: string;
  AChildrenNames, AChildrenClassNames: TStrings);
var
  I: Integer;
  APath: string;
begin
  Registry.GetKeyNames(AChildrenNames);
  for I := 0 to AChildrenNames.Count - 1 do
    if AChildrenNames[I] = '[ClassName]' then
    begin
      AChildrenNames.Delete(I);
      Break;
    end;

  APath := Registry.CurrentPath;
  for I := 0 to AChildrenNames.Count - 1 do
  begin
    Registry.OpenKey(AChildrenNames[I] + '\[ClassName]', False);
    AChildrenClassNames.Add(Registry.ReadString('ClassName'));
    Registry.CloseKey;
    Registry.OpenKey(APath, False);
  end;
end;

procedure TcxRegistryReader.ReadProperties(const AObjectName, AClassName: string; AProperties: TStrings);
var
  AName: string;
  AParents: TStringList;
  ANewPath: string;
  I: Integer;
begin
  AParents := TStringList.Create;
  try
    ExtractObjectFullName(AObjectName, AParents, AName);
    ANewPath := GenRegistryPath(StorageName);
    for I := 0 to AParents.Count - 1 do
      ANewPath := ANewPath + '\' + AParents[I];
    if Registry.OpenKey(ANewPath + '\' + AName, False) then
      Registry.GetValueNames(AProperties);
  finally
    AParents.Free;
  end;
end;

function TcxRegistryReader.ReadProperty(const AObjectName, AClassName, AName: string): Variant;
var
  AValue: string;
  ARealValue: Double;
  ACode: Integer;
begin
  case Registry.GetDataType(AName) of
    rdString, rdExpandString:
    begin
      AValue := Registry.ReadString(AName);

      if IsStringValue(AValue) then
        Result := cxDequotedStr(AValue)
      else
      begin
        Val(AValue, ARealValue, ACode);
        if ACode = 0 then
          Result := ARealValue
        else
          Result := DateTimeOrStr(AValue);
      end;
    end;
    rdInteger:
      Result := Registry.ReadInteger(AName);
    rdBinary:
      Result := Registry.ReadFloat(AName);
    else
      Result := Null;
  end;
end;

{ TcxRegistryWriter }

constructor TcxRegistryWriter.Create(const AStorageName: string; AStream: TStream; AReCreate: Boolean = True);
begin
  inherited Create(AStorageName, AStream, AReCreate);

  FRegistry := TRegistry.Create;
  if FindRegistryKey(StorageName) <> 0 then
    FRegistry.RootKey := FindRegistryKey(StorageName);

  if FReCreate and (StorageName <> '') then
    Registry.DeleteKey(GenRegistryPath(StorageName));

  FRootKeyCreated := Registry.KeyExists(GenRegistryPath(StorageName));
  FRootKeyOpened := False;
end;

destructor TcxRegistryWriter.Destroy;
begin
  FreeAndNil(FRegistry);

  inherited Destroy;
end;

procedure TcxRegistryWriter.BeginWriteObject(const AObjectName, AClassName: string);
var
  AKeyName, APath: string;
  AResult: Boolean;
begin
  CreateRootKey;
  AKeyName := GetKeyName(AObjectName);
  AResult := FRegistry.OpenKey(AKeyName, True);
  APath := Registry.CurrentPath;
  if AResult then
  begin
    AResult := Registry.OpenKey('[ClassName]', True);
    if AResult then
    begin
      Registry.WriteString('ClassName', AClassName);
      Registry.CloseKey;
    end;
  end;
  AResult := AResult and Registry.OpenKey(APath, False);
  if not AResult then
    raise ERegistryException.CreateFmt(scxErrorStoreObject, [AObjectName]);
end;

procedure TcxRegistryWriter.EndWriteObject(const AObjectName, AClassName: string);
var
  AName: string;
  AParents: TStringList;
  ANewKey: string;
  I: Integer;
begin
  Registry.CloseKey;

  AParents := TStringList.Create;
  try
    ExtractObjectFullName(AObjectName, AParents, AName);
    ANewKey := GenRegistryPath(StorageName);
    for I := 0 to AParents.Count - 1 do
      ANewKey := ANewKey + '\' + AParents[I];
    Registry.OpenKey(ANewKey, False);
  finally
    AParents.Free;
  end;
end;

procedure TcxRegistryWriter.WriteProperty(const AObjectName, AClassName, AName: string; AValue: Variant);
begin
  case VarType(AValue) of
    // CLR: varDecimal TODO
    varInt64, varLongWord, varWord, varShortInt,
    varInteger, varSmallInt, varByte:
      Registry.WriteInteger(AName, AValue);
    varSingle, varDouble:
      Registry.WriteFloat(AName, AValue);
    varCurrency:
      Registry.WriteCurrency(AName, AValue);
    varUString, varString, varOleStr:
      Registry.WriteString(AName, cxQuotedStr(AValue));
    varDate:
      Registry.WriteDateTime(AName, AValue);
    varBoolean:
      Registry.WriteBool(AName, AValue);
  end;
end;

procedure TcxRegistryWriter.ClearObjectData(const AObjectFullName, AClassName: string);
var
  AKeyName: string;
begin
  CreateRootKey;
  AKeyName := GetKeyName(AObjectFullName);
  if (AKeyName <> '') and Registry.KeyExists(AKeyName) and not Registry.DeleteKey(AKeyName) then
    raise ERegistryException.CreateFmt(scxErrorStoreObject, [AObjectFullName]);
end;

procedure TcxRegistryWriter.CreateRootKey;
begin
  if not FRootKeyCreated then
  begin
    if not Registry.CreateKey(GenRegistryPath(StorageName)) then
      raise ERegistryException.CreateFmt(cxGetResourceString(@scxCantCreateRegistryKey), [StorageName]);
    FRootKeyCreated := True;
  end;

  if not FRootKeyOpened then
  begin
    if not Registry.OpenKey(GenRegistryPath(StorageName), False) then
      raise ERegistryException.CreateFmt(cxGetResourceString(@scxCantOpenRegistryKey), [StorageName]);

    FRootKeyOpened := True;
  end;
end;

function TcxRegistryWriter.GetKeyName(const AObjectName: string): string;
var
  AParents: TStringList;
begin
  AParents := TStringList.Create;
  try
    ExtractObjectFullName(AObjectName, AParents, Result);
  finally
    AParents.Free;
  end;
end;

{ TcxIniFileReader }

constructor TcxIniFileReader.Create(const AStorageName: string; AStorageStream: TStream);
begin
  inherited Create(AStorageName, AStorageStream);

  FIniFile := TMemIniFile.Create(StorageName);
end;

destructor TcxIniFileReader.Destroy;
begin
  FreeAndNil(FIniFile);
  FreeAndNil(FPathList);
  FreeAndNil(FObjectNameList);
  FreeAndNil(FClassNameList);

  inherited Destroy;
end;

procedure TcxIniFileReader.ReadChildren(const AObjectName, AClassName: string;
  AChildrenNames, AChildrenClassNames: TStrings);
var
  I: Integer;
  AParentPath: string;
begin
  CreateLists;

  if AObjectName <> '' then
    AParentPath := UpperCase(AObjectName) + '/'
  else
    AParentPath := UpperCase(AObjectName);

  for I := 0 to FPathList.Count - 1 do
  begin
    if FPathList[I] = AParentPath then
    begin
      AChildrenNames.Add(FObjectNameList[I]);
      AChildrenClassNames.Add(FClassNameList[I]);
    end;
  end;
end;

procedure TcxIniFileReader.ReadProperties(const AObjectName, AClassName: string; AProperties: TStrings);
var
  ASectionName: string;
begin
  ASectionName := AObjectName + ': ' + AClassName;
  IniFile.ReadSection(ASectionName, AProperties);
end;

function TcxIniFileReader.ReadProperty(const AObjectName, AClassName, AName: string): Variant;
var
  ASectionName: string;
  AValue: string;
  AIntegerValue: Integer;
  ARealValue: Double;
  ACode: Integer;
begin
  ASectionName := AObjectName + ': ' + AClassName;
  AValue := IniFile.ReadString(ASectionName, AName, '');

  if IsStringValue(AValue) then
    Result := DecodeString(AValue)
  else
  begin
    Val(AValue, AIntegerValue, ACode);
    if ACode = 0 then
      Result := AIntegerValue
    else
    begin
      Val(AValue, ARealValue, ACode);
      if ACode = 0 then
        Result := ARealValue
      else
        Result := DateTimeOrStr(AValue);
    end;
  end;
end;

procedure TcxIniFileReader.BeginRead;
begin
  FStorageVersion := IniFile.ReadInteger('Main', 'Version', -1);
end;

function TcxIniFileReader.CanRead: Boolean;
begin
  Result := FileExists(StorageName);
end;

function TcxIniFileReader.DecodeString(S: string): string;

  function DecodeStringV2: string;
  var
    I: Integer;
  begin
    Result := '';
    I := 1;
    while I <= Length(S)  do
    begin
      if S[I] = '\' then
      begin
        Inc(I);
        if I <= Length(S) then
        begin
          if S[I] = 'n' then
            Result := Result + #13#10
          else if S[I] = 't' then
            Result := Result + #9
          else
            Result := Result + '\';
        end;
      end
      else
        Result := Result + S[I];
      Inc(I);
    end;
  end;

  function DecodeStringV0: string;
  begin
    Result := S;
    Result := StringReplace(Result, ' \n', #13#10, [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '\\n', '\n', [rfReplaceAll, rfIgnoreCase]);
  end;

begin
  S := cxDequotedStr(S);
  case FStorageVersion of
    1, 2: Result := DecodeStringV2;
    0: Result := DecodeStringV0;
  else //-1
    Result := S;
  end;
end;

procedure TcxIniFileReader.CreateLists;
var
  ASectionList: TStringList;
  I: Integer;
  APath: string;
  AObjectName: string;
  AClassName: string;
begin
  if (FPathList = nil) or (FObjectNameList = nil) or (FClassNameList = nil) then
  begin
    FPathList := TStringList.Create;
    FObjectNameList := TStringList.Create;
    FClassNameList := TStringList.Create;
    ASectionList := TStringList.Create;
    try
      IniFile.ReadSections(ASectionList);
      for I := 0 to ASectionList.Count - 1 do
      begin
        GetSectionDetail(ASectionList[I], APath, AObjectName, AClassName);
        FPathList.Add(UpperCase(APath));
        FObjectNameList.Add(AObjectName);
        FClassNameList.Add(AClassName);
      end;
    finally
      ASectionList.Free;
    end;
  end;
end;

procedure TcxIniFileReader.GetSectionDetail(const ASection: string; var APath, AObjectName, AClassName: string);
var
  I: Integer;
  AName: string;
begin
  AName := '';
  APath := '';
  AObjectName := '';
  AClassName := '';

  for I := 1 to Length(ASection) do
    if ASection[I] = '/' then
    begin
      APath := APath + AName + '/';
      AName := '';
    end
    else
      if ASection[I] = ':' then
      begin
        AObjectName := AName;
        AName := '';
      end
      else
        AName := AName + ASection[I];
  AClassName := Trim(AName);
end;

{ TcxIniFileWriter }

constructor TcxIniFileWriter.Create(const AStorageName: string; AStream: TStream; AReCreate: Boolean = True);
begin
  inherited Create(AStorageName, AStream, AReCreate);

  FIniFile := TMemIniFile.Create(StorageName);
  FIniFile.Encoding := TEncoding.UTF8;
  if FReCreate then
    FIniFile.Clear;
  FIniFile.CaseSensitive := False;
end;

destructor TcxIniFileWriter.Destroy;
begin
  FreeAndNil(FIniFile);
  inherited Destroy;
end;

procedure TcxIniFileWriter.BeginWriteObject(const AObjectName, AClassName: string);
begin
  IniFile.WriteString(AObjectName + ': ' + AClassName, '', '');
end;

procedure TcxIniFileWriter.WriteProperty(const AObjectName, AClassName, AName: string;
  AValue: Variant);
var
  ASectionName: string;
begin
  ASectionName := AObjectName + ': ' + AClassName;
  case VarType(AValue) of
    varShortInt, varWord, varLongWord, varInt64,
    varSmallInt, varInteger, varByte:
      IniFile.WriteInteger(ASectionName, AName, AValue);
    varSingle, varDouble, varCurrency:
      IniFile.WriteFloat(ASectionName, AName, AValue);
    varUString, varString, varOleStr:
      IniFile.WriteString(ASectionName, AName, EncodeString(AValue));
    varDate:
      IniFile.WriteDateTime(ASectionName, AName, AValue);
  end;
end;

procedure TcxIniFileWriter.BeginWrite;
begin
  IniFile.WriteInteger('Main', 'Version', cxIniFileStorageVersion);
end;

procedure TcxIniFileWriter.ClearObjectData(const AObjectFullName, AClassName: string);
var
  ASectionName, AObjectName: string;
  ASections: TStringList;
  I: Integer;
begin
  if AObjectFullName = '' then
    Exit;
  ASections := TStringList.Create;
  try
    IniFile.ReadSections(ASections);
    AObjectName := UpperCase(AObjectFullName);
    for I := 0 to ASections.Count - 1 do
    begin
      ASectionName := UpperCase(ASections[I]);
      if Pos(AObjectName, ASectionName) = 1 then
      begin
        if (Length(ASectionName) > Length(AObjectName)) and (dxCharInSet(ASectionName[Length(AObjectName) + 1], [':', '/'])) then
          IniFile.EraseSection(ASectionName);
      end;
    end;
  finally
    ASections.Free;
  end;
end;

procedure TcxIniFileWriter.EndWrite;
begin
  IniFile.UpdateFile;
end;

function TcxIniFileWriter.EncodeString(const S: string): string;
begin
  Result := StringReplace(S, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, #13#10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);
  Result := cxQuotedStr(Result);
end;

{ TcxStreamPropertyData }

constructor TcxStreamPropertyData.Create(const AName: string; const AValue: Variant);
begin
  inherited Create;
  FName := AName;
  FValue := AValue;
end;

procedure TcxStreamPropertyData.Read(AReader: TReader);
begin
  FName := AReader.ReadString;
  ReadValue(AReader);
end;

procedure TcxStreamPropertyData.Write(AWriter: TWriter);
begin
  AWriter.WriteString(FName);
  WriteValue(AWriter);
end;

procedure TcxStreamPropertyData.ReadValue(AReader: TReader);
var
  AStreamType: Integer;
begin
  AStreamType := AReader.ReadInteger;
  case AStreamType of
    cxStreamBoolean:
      FValue := AReader.ReadBoolean;
    cxStreamChar:
      FValue := Byte(AReader.ReadChar);
    cxStreamCurrency:
      FValue := AReader.ReadCurrency;
    cxStreamDate:
      FValue := AReader.ReadDate;
    cxStreamFloat:
      FValue := AReader.ReadFloat;
    cxStreamInteger:
      FValue := AReader.ReadInteger;
    cxStreamSingle:
      FValue := AReader.ReadSingle;
    cxStreamString:
      FValue := AReader.ReadString;
    cxStreamWideString:
    {$IFDEF DELPHI17}
      FValue := AReader.ReadString;
    {$ELSE}
      FValue := AReader.ReadWideString;
    {$ENDIF}
  end;
end;

procedure TcxStreamPropertyData.WriteValue(AWriter: TWriter);
begin
  case VarType(FValue) of
    varShortInt, varWord, varLongWord, varInt64,
    varSmallInt, varInteger:
      begin
        AWriter.WriteInteger(cxStreamInteger);
        AWriter.WriteInteger(FValue);
      end;
    varSingle:
      begin
        AWriter.WriteInteger(cxStreamSingle);
        AWriter.WriteSingle(FValue);
      end;
    varDouble:
      begin
        AWriter.WriteInteger(cxStreamFloat);
        AWriter.WriteFloat(FValue);
      end;
    varCurrency:
      begin
        AWriter.WriteInteger(cxStreamCurrency);
        AWriter.WriteCurrency(FValue);
      end;
    varDate:
      begin
        AWriter.WriteInteger(cxStreamDate);
        AWriter.WriteDate(FValue);
      end;
    varUString, varOleStr:
      begin
        AWriter.WriteInteger(cxStreamWideString);
      {$IFDEF DELPHI17}
        AWriter.WriteString(FValue);
      {$ELSE}
        AWriter.WriteWideString(FValue);
      {$ENDIF}
      end;
    varBoolean:
      begin
        AWriter.WriteInteger(cxStreamBoolean);
        AWriter.WriteBoolean(FValue);
      end;
    varByte:
      begin
        AWriter.WriteInteger(cxStreamChar);
        AWriter.WriteChar(Char(Byte(FValue)));
      end;
    varString:
      begin
        AWriter.WriteInteger(cxStreamString);
        AWriter.WriteString(FValue);
      end;
  end;
end;

{ TcxStreamObjectData }

constructor TcxStreamObjectData.Create(const AName, AClassName: string);
begin
  inherited Create;
  FName := AName;
  FClassName := AClassName;
  FChildren := TList.Create;
  FProperties := TList.Create;
end;

destructor TcxStreamObjectData.Destroy;
begin
  Clear;
  FreeAndNil(FChildren);
  FreeAndNil(FProperties);
  inherited Destroy;
end;

procedure TcxStreamObjectData.Clear;
var
  I: Integer;
begin
  for I := 0 to FProperties.Count - 1 do
    TcxStreamPropertyData(FProperties[I]).Free;
  FProperties.Clear;

  for I := 0 to FChildren.Count - 1 do
    TcxStreamObjectData(FChildren[I]).Free;
  FChildren.Clear;
end;

procedure TcxStreamObjectData.AddChild(AChild: TcxStreamObjectData);
begin
  FChildren.Add(AChild);
end;

procedure TcxStreamObjectData.AddProperty(AProperty: TcxStreamPropertyData);
begin
  FProperties.Add(AProperty);
end;

procedure TcxStreamObjectData.Read(AReader: TReader);
var
  ACount: Integer;
  I: Integer;
begin
  with AReader do
  begin
    FName := ReadString;
    FClassName := ReadString;
    ACount := ReadInteger;
    for I := 0 to ACount - 1 do
    begin
      AddProperty(TcxStreamPropertyData.Create('', Null));
      TcxStreamPropertyData(FProperties.Last).Read(AReader);
    end;
    ACount := ReadInteger;
    for I := 0 to ACount - 1 do
    begin
      AddChild(TcxStreamObjectData.Create('', ''));
      TcxStreamObjectData(FChildren.Last).Read(AReader);
    end;
  end;
end;

procedure TcxStreamObjectData.Write(AWriter: TWriter);
var
  I: Integer;
begin
  with AWriter do
  begin
    WriteString(FName);
    WriteString(FClassName);
    WriteInteger(PropertyCount);
    for I := 0 to PropertyCount - 1 do
      Properties[I].Write(AWriter);
    WriteInteger(ChildCount);
    for I := 0 to ChildCount - 1 do
      Children[I].Write(AWriter);
  end;
end;

function TcxStreamObjectData.GetChildCount: Integer;
begin
  Result := FChildren.Count;
end;

function TcxStreamObjectData.GetChildren(AIndex: Integer): TcxStreamObjectData;
begin
  Result := TcxStreamObjectData(FChildren[AIndex]);
end;

function TcxStreamObjectData.GetProperties(AIndex: Integer): TcxStreamPropertyData;
begin
  Result := TcxStreamPropertyData(FProperties[AIndex]);
end;

function TcxStreamObjectData.GetPropertyCount: Integer;
begin
  Result := FProperties.Count;
end;

end.
