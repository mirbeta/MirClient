{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEntityMapping Framework                           }
{                                                                    }
{           Copyright (c) 2016-2019 Developer Express Inc.           }
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
{   (DCU, OBJ, DLL, DPU, SO, ETC.) ARE CONFIDENTIAL AND PROPRIETARY  }
{   TRADE SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER }
{   IS LICENSED TO DISTRIBUTE THE EXPRESSENTITYMAPPING FRAMEWORK     }
{   AS PART OF AN EXECUTABLE PROGRAM ONLY.                           }
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

unit dxEMF.DB.Model;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  SysUtils, Generics.Defaults, Generics.Collections, TypInfo, FmtBcd,
  dxCoreClasses,
  dxEMF.Types;

type

 { TdxDBColumn }

  TdxDBColumn = class
  strict private
    FColumnType: TdxDBColumnType;
  public
    constructor Create(const AName: string; AIsKey: Boolean; const ADBTypeName: string; ASize: Integer; AType: TdxDBColumnType);
    class function GetColumnType(AType: PTypeInfo; ASuppressExceptionOnUnknown: Boolean = False): TdxDBColumnType; static;
    property ColumnType: TdxDBColumnType read FColumnType;
  public
    Name: string;
    Size: Integer;
    IsKey: Boolean;
    IsIdentity: Boolean;
    IsNullable: Boolean;
    DBTypeName: string;
    DefaultValue: Variant;
    Generator: TdxGenerator;
  end;

  { TdxDBColumnComparer }

  TdxDBColumnComparer = class(TCustomComparer<TdxDBColumn>)
  protected
    function Compare(const ALeft, ARight: TdxDBColumn): Integer; override;
    function Equals(const ALeft, ARight: TdxDBColumn): Boolean; override;
    function GetHashCode(const AValue: TdxDBColumn): Integer; override;
  end;

  { TdxDBColumnCollection }

  TdxDBColumnCollection = class(TObjectList<TdxDBColumn>)
  public
    function GetNames: TArray<string>;
  end;

  { TdxDBTableMultiColumnGadget }

  TdxDBTableMultiColumnGadget = class abstract
  strict private
    FName: string;
    FColumns: TList<string>;
  protected
    procedure SetName(const AValue: string);
  public
    constructor Create; overload;
    constructor Create(const AColumns: array of string); overload;
    constructor Create(const AColumns: array of TdxDBColumn); overload;
    constructor Create(AColumns: TEnumerable<TdxDBColumn>); overload;
    constructor Create(AColumns: TEnumerable<string>); overload;
    destructor Destroy; override;
    procedure AddColumns(const AColumns: array of TdxDBColumn); overload;
    procedure AddColumns(AColumns: TEnumerable<TdxDBColumn>); overload;
    procedure AddColumns(AColumns: TEnumerable<string>); overload;
    property Name: string read FName;
    property Columns: TList<string> read FColumns;
  end;

 { TdxDBIndex }

  TdxDBIndex = class(TdxDBTableMultiColumnGadget)
  strict private
    FIsUnique: Boolean;
  public
    constructor Create(const AName: string; const AColumns: array of string; AIsUnique: Boolean); overload;
    constructor Create(const AColumns: array of string; AIsUnique: Boolean); overload;
    constructor Create(const AName: string; const AColumns: TArray<TdxDBColumn>; AIsUnique: Boolean); overload;
    constructor Create(const AColumns: array of TdxDBColumn; AIsUnique: Boolean); overload;
    constructor Create(AColumns: TEnumerable<TdxDBColumn>; AIsUnique: Boolean); overload;
    constructor Create(AColumns: TEnumerable<string>; AIsUnique: Boolean); overload;
    function Clone: TdxDBIndex;
    property IsUnique: Boolean read FIsUnique;
  end;

  TdxDBIndexCollection = TObjectList<TdxDBIndex>;

  { TdxDBPrimaryKey }

  TdxDBPrimaryKey = class(TdxDBIndex);

  { TdxDBForeignKey }

  TdxDBForeignKey = class(TdxDBTableMultiColumnGadget)
  strict private
    FPrimaryKeyTable: string;
    FPrimaryKeyTableKeyColumns: TList<string>;
  public
    constructor Create(const APrimaryKeyTable: string); overload;
    constructor Create(const AColumns: array of string; const APrimaryKeyTable: string;
      const APrimaryKeyTableKeyColumns: array of string); overload;
    constructor Create(const AColumns: array of TdxDBColumn; const APrimaryKeyTable: string;
      const APrimaryKeyTableKeyColumns: array of string); overload;
    constructor Create(const AColumns: array of TdxDBColumn; const APrimaryKeyTable: string); overload;
    destructor Destroy; override;
    property PrimaryKeyTable: string read FPrimaryKeyTable;
    property PrimaryKeyTableKeyColumns: TList<string> read FPrimaryKeyTableKeyColumns;
  end;

  TdxDBForeignKeyCollection = TObjectList<TdxDBForeignKey>;


  { TdxDBTable }

  TdxDBTable = class
  strict private
    FName: string;
    FColumns: TdxDBColumnCollection;
    FIndexes: TdxDBIndexCollection;
    FPrimaryKey: TdxDBPrimaryKey;
    FForeignKeys: TdxDBForeignKeyCollection;
    FIsView: Boolean;
    function GetHasPrimaryKey: Boolean;
    function GetSequenceName: string;
  public
    constructor Create; overload;
    constructor Create(const AName: string); overload;
    destructor Destroy; override;

    function AreGadgetsEqual(AExistingKey: TdxDBTableMultiColumnGadget; AForeignKey: TdxDBTableMultiColumnGadget): Boolean;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    function ToString: string; override;
    function GetColumn(const AColumnName: string): TdxDBColumn;
    function IsForeignKeyIncluded(AForeignKey: TdxDBForeignKey): Boolean;
    function IsIndexIncluded(AIndex: TdxDBIndex): Boolean;
    procedure AddColumn(AColumn: TdxDBColumn);
    procedure AddIndex(AIndex: TdxDBIndex);
    procedure AddForeignKey(AForeignKey: TdxDBForeignKey);

    property Name: string read FName;
    property Columns: TdxDBColumnCollection read FColumns;
    property Indexes: TdxDBIndexCollection read FIndexes;
    property PrimaryKey: TdxDBPrimaryKey read FPrimaryKey write FPrimaryKey;
    property ForeignKeys: TdxDBForeignKeyCollection read FForeignKeys;
    property HasPrimaryKey: Boolean read GetHasPrimaryKey;
    property SequenceName: string read GetSequenceName;
    property IsView: Boolean read FIsView write FIsView;
  end;

  { TdxDBStoredProcedure }

  TdxDBStoredProcedure = class
  public
  end;

implementation

uses
  dxCore, dxHash,
  dxEMF.Utils;

{ TdxDBColumn }

constructor TdxDBColumn.Create(const AName: string; AIsKey: Boolean; const ADBTypeName: string; ASize: Integer; AType: TdxDBColumnType);
begin
  inherited Create;
  IsKey := AIsKey;
  Name := AName;
  DBTypeName := ADBTypeName;
  Size := ASize;
  FColumnType := AType;
end;

{$WARN NO_RETVAL OFF}
class function TdxDBColumn.GetColumnType(AType: PTypeInfo; ASuppressExceptionOnUnknown: Boolean): TdxDBColumnType;
var
  ATypeData: PTypeData;
  AUnderlyingTypeInfo: PTypeInfo;
begin
  ATypeData := GetTypeData(AType);
  case AType.Kind of
    tkInteger,
    tkEnumeration:
      case ATypeData.OrdType of
        otSByte:
          Result := TdxDBColumnType.SByte;
        otUByte:
          case AType.Kind of
            tkInteger:
              Result := TdxDBColumnType.Byte;
            tkEnumeration:
              if AType = TypeInfo(Boolean) then
                Result := TdxDBColumnType.Boolean
              else
                Result := TdxDBColumnType.Byte;
          end;
        otSWord:
          Result := TdxDBColumnType.Int16;
        otUWord:
          Result := TdxDBColumnType.UInt16;
        otSLong:
          Result := TdxDBColumnType.Int32;
        otULong:
          Result := TdxDBColumnType.UInt32;
      end;
    tkChar, tkWChar:
      Result := TdxDBColumnType.Char;
    tkFloat:
      case ATypeData.FloatType of
        TFloatType.ftSingle:
          Result := TdxDBColumnType.Single;
        TFloatType.ftDouble, TFloatType.ftExtended, TFloatType.ftComp:
          begin
            if (AType = TypeInfo(TDateTime)) or (AType = TypeInfo(TDate)) or (AType = TypeInfo(TTime)) then
              Result := TdxDBColumnType.DateTime
            else
              Result := TdxDBColumnType.Double;
          end;
        TFloatType.ftCurr:
          Result := TdxDBColumnType.Decimal;
      end;
    tkInt64:
      if ATypeData.MinInt64Value = 0 then
        Result := TdxDBColumnType.UInt64
      else
        Result := TdxDBColumnType.Int64;
    tkString, tkLString, tkWString, tkUString:
      Result := TdxDBColumnType.String;
    tkRecord{$IFDEF DELPHI103}, tkMRecord{$ENDIF}:
      begin
        if AType = TypeInfo(TGUID) then
          Result := TdxDBColumnType.Guid
        else
          if AType = TypeInfo(TBCD) then
            Result := TdxDBColumnType.Decimal
          else
            if IsNullableType(AType) then
            begin
              TryGetUnderlyingType(AType, AUnderlyingTypeInfo);
              Result := GetColumnType(AUnderlyingTypeInfo, ASuppressExceptionOnUnknown);
            end
            else
              Result := TdxDBColumnType.Unknown;
      end;
    tkDynArray, tkArray:
      Result := TdxDBColumnType.ByteArray;
    else
      Result := TdxDBColumnType.Unknown;
  end;
end;
{$WARN NO_RETVAL DEFAULT}

{ TdxDBColumnComparer }

function TdxDBColumnComparer.Compare(const ALeft, ARight: TdxDBColumn): Integer;
var
  L, R: string;
  ALen, ALenDiff: Integer;
begin
  L := AnsiLowerCase(ALeft.Name);
  R := AnsiLowerCase(ARight.Name);
  ALen := Length(L);
  ALenDiff := ALen - Length(R);
  if Length(R) < ALen then
    ALen := Length(R);
  Result := BinaryCompare(PChar(L), PChar(R), ALen * SizeOf(Char));
  if Result = 0 then
    Result := ALenDiff;
end;

function TdxDBColumnComparer.Equals(const ALeft, ARight: TdxDBColumn): Boolean;
begin
  Result := SameText(ALeft.Name, ARight.Name);
end;

function TdxDBColumnComparer.GetHashCode(const AValue: TdxDBColumn): Integer;
begin
  Result := dxElfHash(AValue.Name);
end;

{ TdxDBColumnCollection }

function TdxDBColumnCollection.GetNames: TArray<string>;
var
  AList: TList<string>;
  AColumn: TdxDBColumn;
begin
  AList := TList<string>.Create;
  try
    AList.Capacity := Count;
    for AColumn in Self do
      if AColumn <> nil then
        AList.Add(AColumn.Name);
    Result := AList.ToArray;
  finally
    AList.Free;
  end;
end;

{ TdxDBTableMultiColumnGadget }

constructor TdxDBTableMultiColumnGadget.Create;
begin
  inherited Create;
  FColumns := TList<string>.Create;
end;

constructor TdxDBTableMultiColumnGadget.Create(const AColumns: array of string);
begin
  Create;
  FColumns.AddRange(AColumns);
end;

constructor TdxDBTableMultiColumnGadget.Create(AColumns: TEnumerable<TdxDBColumn>);
begin
  Create;
  AddColumns(AColumns);
end;

constructor TdxDBTableMultiColumnGadget.Create(const AColumns: array of TdxDBColumn);
begin
  Create;
  AddColumns(AColumns);
end;

destructor TdxDBTableMultiColumnGadget.Destroy;
begin
  FreeAndNil(FColumns);
  inherited Destroy;
end;

procedure TdxDBTableMultiColumnGadget.AddColumns(const AColumns: array of TdxDBColumn);
var
  AColumn: TdxDBColumn;
begin
  for AColumn in AColumns do
    FColumns.Add(AColumn.Name);
end;

procedure TdxDBTableMultiColumnGadget.AddColumns(AColumns: TEnumerable<TdxDBColumn>);
var
  AColumn: TdxDBColumn;
begin
  for AColumn in AColumns do
    FColumns.Add(AColumn.Name);
end;

procedure TdxDBTableMultiColumnGadget.AddColumns(AColumns: TEnumerable<string>);
begin
  FColumns.AddRange(AColumns);
end;

constructor TdxDBTableMultiColumnGadget.Create(AColumns: TEnumerable<string>);
begin
  Create;
  FColumns.AddRange(AColumns);
end;

procedure TdxDBTableMultiColumnGadget.SetName(const AValue: string);
begin
  FName := AValue;
end;

{ TdxDBIndex }

constructor TdxDBIndex.Create(const AColumns: array of string; AIsUnique: Boolean);
begin
  inherited Create(AColumns);
  FIsUnique := AIsUnique;
end;

constructor TdxDBIndex.Create(const AName: string; const AColumns: array of string; AIsUnique: Boolean);
begin
  inherited Create(AColumns);
  SetName(AName);
  FIsUnique := AIsUnique;
end;

constructor TdxDBIndex.Create(const AColumns: array of TdxDBColumn; AIsUnique: Boolean);
begin
  inherited Create(AColumns);
  FIsUnique := AIsUnique;
end;

constructor TdxDBIndex.Create(const AName: string; const AColumns: TArray<TdxDBColumn>; AIsUnique: Boolean);
begin
  inherited Create(AColumns);
  SetName(AName);
  FIsUnique := AIsUnique;
end;

constructor TdxDBIndex.Create(AColumns: TEnumerable<TdxDBColumn>; AIsUnique: Boolean);
begin
  inherited Create(AColumns);
  FIsUnique := AIsUnique;
end;

function TdxDBIndex.Clone: TdxDBIndex;
begin
  Result := TdxDBIndex.Create(Name, Columns.ToArray, IsUnique);
end;

constructor TdxDBIndex.Create(AColumns: TEnumerable<string>; AIsUnique: Boolean);
begin
  inherited Create(AColumns);
  FIsUnique := AIsUnique;
end;

{ TdxDBForeignKey }

constructor TdxDBForeignKey.Create(const AColumns: array of string; const APrimaryKeyTable: string;
  const APrimaryKeyTableKeyColumns: array of string);
begin
  inherited Create(AColumns);
  FPrimaryKeyTableKeyColumns := TList<string>.Create;
  FPrimaryKeyTable := APrimaryKeyTable;
  FPrimaryKeyTableKeyColumns.AddRange(APrimaryKeyTableKeyColumns);
end;

constructor TdxDBForeignKey.Create(const AColumns: array of TdxDBColumn; const APrimaryKeyTable: string;
  const APrimaryKeyTableKeyColumns: array of string);
begin
  inherited Create(AColumns);
  FPrimaryKeyTableKeyColumns := TList<string>.Create;
  FPrimaryKeyTable := APrimaryKeyTable;
  FPrimaryKeyTableKeyColumns.AddRange(APrimaryKeyTableKeyColumns);
end;

constructor TdxDBForeignKey.Create(const AColumns: array of TdxDBColumn; const APrimaryKeyTable: string);
var
  AColumn: TdxDBColumn;
begin
  inherited Create(AColumns);
  FPrimaryKeyTableKeyColumns := TList<string>.Create;
  FPrimaryKeyTable := APrimaryKeyTable;
  for AColumn in AColumns do
    FPrimaryKeyTableKeyColumns.Add(AColumn.Name);
end;

constructor TdxDBForeignKey.Create(const APrimaryKeyTable: string);
begin
  inherited Create;
  FPrimaryKeyTableKeyColumns := TList<string>.Create;
  FPrimaryKeyTable := APrimaryKeyTable;
end;

destructor TdxDBForeignKey.Destroy;
begin
  FreeAndNil(FPrimaryKeyTableKeyColumns);
  inherited Destroy;;
end;


{ TdxDBTable }

constructor TdxDBTable.Create;
begin
  inherited Create;
  FColumns := TdxDBColumnCollection.Create;
  FIndexes := TdxDBIndexCollection.Create;
  FForeignKeys := TdxDBForeignKeyCollection.Create;
end;

constructor TdxDBTable.Create(const AName: string);
begin
  Create;
  FName := AName;
end;

destructor TdxDBTable.Destroy;
begin
  FreeAndNil(FColumns);
  FreeAndNil(FIndexes);
  FreeAndNil(FPrimaryKey);
  FreeAndNil(FForeignKeys);
  inherited;
end;

function TdxDBTable.AreGadgetsEqual(AExistingKey: TdxDBTableMultiColumnGadget; AForeignKey: TdxDBTableMultiColumnGadget): Boolean;
var
  I: Integer;
begin
  if (AExistingKey.Name <> AForeignKey.Name) or (AExistingKey.Columns.Count <> AForeignKey.Columns.Count) then
    Exit(False);
  for I := 0 to AExistingKey.Columns.Count - 1 do
  begin
    if AExistingKey.Columns[I] <> AForeignKey.Columns[I] then
      Exit(False);
  end;
  Result := True;
end;

function TdxDBTable.Equals(AObject: TObject): Boolean;
var
  AAnother: TdxDBTable;
begin
  AAnother := Safe<TdxDBTable>.Cast(AObject);
  if AAnother = nil then
    Result := False
  else
    Result := Name = AAnother.Name;
end;

function TdxDBTable.GetColumn(const AColumnName: string): TdxDBColumn;
var
  I: Integer;
  AColumn: TdxDBColumn;
begin
  for I := 0 to Columns.Count - 1 do
  begin
    AColumn := Columns[I];
    if AColumn.Name = AColumnName then
      Exit(AColumn);
  end;
  Result := nil;
end;

function TdxDBTable.GetHashCode: Integer;
begin
  if Name = '' then
    Result := 0
  else
    Result := GetValueHash(Name);
end;

function TdxDBTable.GetHasPrimaryKey: Boolean;
begin
  Result := (FPrimaryKey <> nil) and (FPrimaryKey.Columns.Count > 0);
end;

function TdxDBTable.GetSequenceName: string;
var
  AKey: TdxDBColumn;
begin
  if PrimaryKey <> nil then
  begin
    AKey := GetColumn(PrimaryKey.Columns[0]);
    Result := AKey.Generator.SequenceName;
  end
  else
    Result := '';
end;

function TdxDBTable.IsForeignKeyIncluded(AForeignKey: TdxDBForeignKey): Boolean;
var
  AKey: TdxDBForeignKey;
begin
  for AKey in ForeignKeys do
    if AreGadgetsEqual(AKey, AForeignKey) then
      Exit(True);
  Result := False;
end;

function TdxDBTable.IsIndexIncluded(AIndex: TdxDBIndex): Boolean;
var
  AKey: TdxDBIndex;
begin
  for AKey in Indexes do
    if AreGadgetsEqual(AKey, AIndex) then
      Exit(True);
  Result := False;
end;

function TdxDBTable.ToString: string;
begin
  Result := Name;
end;

procedure TdxDBTable.AddColumn(AColumn: TdxDBColumn);
begin
  FColumns.Add(AColumn);
end;

procedure TdxDBTable.AddIndex(AIndex: TdxDBIndex);
begin
  FIndexes.Add(AIndex);
end;

procedure TdxDBTable.AddForeignKey(AForeignKey: TdxDBForeignKey);
begin
  FForeignKeys.Add(AForeignKey);
end;

end.
