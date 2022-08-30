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

unit dxEMF.DB.SQLite;

{$HPPEMIT '#pragma link "dxEMF.DB.SQLite"'}

interface

{$I dxEMF.inc}
{$I cxVer.inc}

uses
  SysUtils, Classes, Generics.Collections, DB, Rtti,
  dxEMF.DB.Model,
  dxEMF.Types,
  dxEMF.Utils,
  dxEMF.DB.Criteria,
  dxEMF.DB.Query,
  dxEMF.DB.SQLConnectionProvider;

type

  { TdxSQLiteConnectionProvider }

  TdxSQLiteConnectionProvider = class(TdxSQLConnectionProvider)
  strict private const
    StopChars: array [0 .. 1] of Char = ('_', '%');
  strict private
    class var
      FConnectionParameters: TDictionary<string, TdxConnectionParameterValues>;
      FDBTypes: TDictionary<string, TdxDBColumnType>;
    class destructor Destroy;
    class function GetConnectionParameters: TDictionary<string, TdxConnectionParameterValues>; static;
    class procedure PopulateConnectionParameters; static; inline;
    class procedure PopulateDBTypes; static; inline;
    function GetDataForTables(const ATables: TArray<TdxDBTable>; AFilter: TdxSQLiteConnectionProvider.TTableFilter;
      const AQueryText: string): TDataSet;
    function GetTypeFromString(ATypeName: string; out ASize: Integer): TdxDBColumnType;
  protected
    class function GetDBTypes: TDictionary<string, TdxDBColumnType>; override;
    function ConvertToDBParameter(const AParameterValue: TValue): TValue; override;
    function CollectTablesToCreate(const ATables: TArray<TdxDBTable>): TArray<TdxDBTable>; override;
    function GetBraceJoin: Boolean; override;
    function GetMaximumStringSize: Integer; override;
    function GetSafeNameTableMaxLength: Integer; override;
    function GetSQLCreateColumnTypeForBoolean(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForSByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForChar(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForDecimal(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForDouble(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForSingle(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForUInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForUInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForUInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForString(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForDateTime(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForGuid(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForByteArray(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;

    procedure GetColumns(ATable: TdxDBTable);
    procedure GetPrimaryKey(ATable: TdxDBTable);
    procedure GetIndexes(ATable: TdxDBTable);
    procedure GetForeignKeys(ATable: TdxDBTable);
    function NeedsIndexForForeignKey: Boolean; override;

    function GetIdentity(ASQL: TdxQuery): Int64; override;
  public
    class function GetDBEngine: TdxDBEngine; override;

    procedure CreateDatabase(const ADatabaseName: string; const AValues: array of string); override;
    procedure CreateTable(ATable: TdxDBTable); override;
    procedure CreateColumn(ATable: TdxDBTable; AColumn: TdxDBColumn); override;
    procedure CreatePrimaryKey(ATable: TdxDBTable); override;
    procedure CreateForeignKey(ATable: TdxDBTable; AForeignKey: TdxDBForeignKey); override;
    function GetConnectionParameter(const AConnectionType: TdxConnectionType; AParameter: TdxConnectionParameter): string; override;
    function GetParameterName(const AParameter: IdxOperandValue; AIndex: Integer; var ACreateParameter: Boolean): string; override;
    function GetSQLCreateColumnFullAttributes(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetStorageTableNames(AIncludeViews: Boolean): TArray<string>; override;
    procedure GetTableSchema(ATable: TdxDBTable; ACheckIndexes: Boolean; ACheckForeignKeys: Boolean); override;
    function FormatColumn(const AColumnName: string): string; override;
    function FormatColumn(const AColumnName: string; const ATableAlias: string): string; override;
    function FormatConstraint(const AConstraintName: string): string; override;
    function FormatFunction(AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<string>): string; override;
    function FormatFunction(AProcessParameter: TdxProcessParameter; AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<TValue>): string; override;
    function FormatTable(const ASchemaName: string; const ATableName: string): string; override;
    function FormatTable(const ASchemaName: string; const ATableName: string; const ATableAlias: string): string; override;
    function FormatBinary(AOperatorType: TdxBinaryOperatorType; const ALeftOperand, ARightOperand: string): string; override;
    function FormatSelect(const ASelectClause, AFromClause, AWhereClause, AOrderByClause, AGroupByClause, AHavingClause: string;
      ASkipSelectedRecords: Integer; ATopSelectedRecords: Integer): string; override;
    function FormatDelete(const ATableName, AWhereClause: string): string; override;
    function FormatInsertDefaultValues(ATable: TdxDBTable): string; override;
    function FormatInsert(const ATableName, AFieldNames, AValuesClause: string): string; override;
    function FormatUpdate(const ATableName, ASetClause, AWhereClause: string): string; override;
    function SupportsNativeSkipTake: Boolean; override;
    class property ConnectionParameters: TDictionary<string, TdxConnectionParameterValues> read GetConnectionParameters;
  end;

implementation

uses
  Math, StrUtils, TypInfo,
  dxCore, dxStringHelper,
  dxEMF.DB.Utils;

{ TdxSQLiteConnectionProvider }

class procedure TdxSQLiteConnectionProvider.PopulateConnectionParameters;
begin
  FConnectionParameters.AddOrSetValue(TdxConnectionTypes.ADO, TdxSQLConnectionProvider.TConnectionParameters.Create(['Data Source']));
  FConnectionParameters.AddOrSetValue(TdxConnectionTypes.FireDAC, TdxSQLConnectionProvider.TConnectionParameters.Create(['Database']));
end;

class procedure TdxSQLiteConnectionProvider.PopulateDBTypes;
begin
  FDBTypes.Add('bigint', TdxDBColumnType.Int64);
  FDBTypes.Add('integer', TdxDBColumnType.Int32);
  FDBTypes.Add('int', TdxDBColumnType.Int32);
  FDBTypes.Add('boolean', TdxDBColumnType.Boolean);
  FDBTypes.Add('bool', TdxDBColumnType.Boolean);
  FDBTypes.Add('bit', TdxDBColumnType.Boolean);
  FDBTypes.Add('tinyint', TdxDBColumnType.Byte);
  FDBTypes.Add('text', TdxDBColumnType.String);
  FDBTypes.Add('date', TdxDBColumnType.DateTime);
  FDBTypes.Add('time', TdxDBColumnType.DateTime);
  FDBTypes.Add('datetime', TdxDBColumnType.DateTime);
  FDBTypes.Add('money', TdxDBColumnType.Decimal);
  FDBTypes.Add('real', TdxDBColumnType.Double);
  FDBTypes.Add('double precision', TdxDBColumnType.Double);
  FDBTypes.Add('double', TdxDBColumnType.Double);
  FDBTypes.Add('float', TdxDBColumnType.Single);
  FDBTypes.Add('smallint', TdxDBColumnType.Int16);
  FDBTypes.Add('blob', TdxDBColumnType.ByteArray);
  FDBTypes.Add('image', TdxDBColumnType.ByteArray);
  FDBTypes.Add('numeric', TdxDBColumnType.Decimal);
  FDBTypes.Add('nvarchar', TdxDBColumnType.String);
  FDBTypes.Add('varchar', TdxDBColumnType.String);
  FDBTypes.Add('char', TdxDBColumnType.String);
  FDBTypes.Add('nchar', TdxDBColumnType.String);
end;

function TdxSQLiteConnectionProvider.ConvertToDBParameter(const AParameterValue: TValue): TValue;
begin
  if AParameterValue.IsType<TGUID> then
    Result := Copy(GUIDToString(AParameterValue.AsType<TGUID>), 2, 36)
  else
    Result := inherited ConvertToDBParameter(AParameterValue);
end;

procedure TdxSQLiteConnectionProvider.CreateColumn(ATable: TdxDBTable; AColumn: TdxDBColumn);
var
  ADataType: string;
begin
  ADataType := GetSQLCreateColumnFullAttributes(ATable, AColumn);
  if not AColumn.IsNullable then
    ADataType := ADataType + ' default ''''';
  ExecuteSQLSchemaUpdate('Column', AColumn.Name, ATable.Name,
    Format('alter table %s add %s %s',
      [FormatTableSafe(ATable), FormatColumnSafe(AColumn.Name), ADataType]));
end;

procedure TdxSQLiteConnectionProvider.CreateDatabase(const ADatabaseName: string; const AValues: array of string);
begin
  ConnectionVendor.Open;
end;

procedure TdxSQLiteConnectionProvider.CreateForeignKey(ATable: TdxDBTable; AForeignKey: TdxDBForeignKey);
begin
end;

procedure TdxSQLiteConnectionProvider.CreatePrimaryKey(ATable: TdxDBTable);
begin
end;

class function TdxSQLiteConnectionProvider.GetDBEngine: TdxDBEngine;
begin
  Result := TdxDBEngines.SQLite;
end;

procedure TdxSQLiteConnectionProvider.CreateTable(ATable: TdxDBTable);

  function GetForeignKeyClause: string;
  var
    AForeignKeys: TStringBuilder;
    AForeignKey: TdxDBForeignKey;
  begin
    if ATable.ForeignKeys.Count = 0 then
      Exit('');
    AForeignKeys := TStringBuilder.Create;
    try
      for AForeignKey in ATable.ForeignKeys do
      begin
        AForeignKeys.Append(',').
          Append(' FOREIGN KEY(').
          Append(TdxFormatterHelper.Concat(AForeignKey.Columns, FormatColumnSafe)).
          Append(') REFERENCES ').
          Append(FormatTable(ComposeSafeSchemaName(AForeignKey.PrimaryKeyTable), ComposeSafeTableName(AForeignKey.PrimaryKeyTable))).
          Append(' (').
          Append(TdxFormatterHelper.Concat(AForeignKey.PrimaryKeyTableKeyColumns, FormatColumnSafe)).
          Append(')'#10);
      end;
      Result := AForeignKeys.ToString;
    finally
      AForeignKeys.Free;
    end;
  end;

var
  AColumns, AFormattedColumns: TStringBuilder;
  AColumn: TdxDBColumn;
  I: Integer;
begin
  AColumns := TStringBuilder.Create;
  try
    for I := 0 to ATable.Columns.Count - 1 do
    begin
      AColumn := ATable.Columns[I];
      if I > 0 then
        AColumns.Append(', ');
      AColumns.Append(FormatColumnSafe(AColumn.Name)).Append(' ').Append(GetSQLCreateColumnFullAttributes(ATable, AColumn));
    end;
    if ATable.HasPrimaryKey and not ATable.GetColumn(ATable.PrimaryKey.Columns[0]).IsIdentity then
    begin
      AFormattedColumns := TStringBuilder.Create;
      try
        for I := 0 to ATable.PrimaryKey.Columns.Count - 1 do
        begin
          if I > 0 then
            AFormattedColumns.Append(', ');
          AFormattedColumns.Append(FormatColumnSafe(ATable.PrimaryKey.Columns[I]));
        end;
        ExecuteSQLSchemaUpdate('Table', ATable.Name, '',
          Format('create table %s (%s, primary key (%s)%s)',
            [FormatTableSafe(ATable), AColumns.ToString, AFormattedColumns.ToString, GetForeignKeyClause]));
      finally
        AFormattedColumns.Free;
      end;
    end
    else
      ExecuteSQLSchemaUpdate('Table', ATable.Name, '',
        Format('create table %s (%s%s)', [FormatTableSafe(ATable), AColumns.ToString, GetForeignKeyClause]));
  finally
    AColumns.Free;
  end;
end;

class function TdxSQLiteConnectionProvider.GetDBTypes: TDictionary<string, TdxDBColumnType>;
begin
  if FDBTypes = nil then
  begin
    FDBTypes := TDictionary<string, TdxDBColumnType>.Create;
    PopulateDBTypes;
  end;
  Result := FDBTypes;
end;

function TdxSQLiteConnectionProvider.GetSQLCreateColumnTypeForBoolean(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'bit';
end;

function TdxSQLiteConnectionProvider.GetSQLCreateColumnTypeForByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'tinyint';
end;

function TdxSQLiteConnectionProvider.GetSQLCreateColumnTypeForSByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'numeric(3,0)';
end;

function TdxSQLiteConnectionProvider.GetSQLCreateColumnTypeForChar(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'nchar(1)';
end;

function TdxSQLiteConnectionProvider.GetSQLCreateColumnTypeForDecimal(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'money';
end;

function TdxSQLiteConnectionProvider.GetSQLCreateColumnTypeForDouble(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'double';
end;

function TdxSQLiteConnectionProvider.GetSQLCreateColumnTypeForSingle(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'float';
end;

function TdxSQLiteConnectionProvider.GetSQLCreateColumnTypeForInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'int';
end;

function TdxSQLiteConnectionProvider.GetSQLCreateColumnTypeForUInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'numeric(10,0)';
end;

function TdxSQLiteConnectionProvider.GetSQLCreateColumnTypeForInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'smallint';
end;

function TdxSQLiteConnectionProvider.GetSQLCreateColumnTypeForUInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'numeric(5,0)';
end;

function TdxSQLiteConnectionProvider.GetSQLCreateColumnTypeForInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'numeric(20,0)';
end;

function TdxSQLiteConnectionProvider.GetSQLCreateColumnTypeForUInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'numeric(20,0)';
end;

function TdxSQLiteConnectionProvider.GetStorageTableNames(AIncludeViews: Boolean): TArray<string>;
var
  AQuery: TdxQuery;
  AResult: TList<string>;
  ATables: TDataSet;
begin
  AQuery := TdxQuery.Create(Format('SELECT [name] FROM [sqlite_master] WHERE [type] LIKE ''table''%s and [name] not like ''SQLITE_%%''',
    [IfThen(AIncludeViews, ' or [type] LIKE ''view''', '')]));
  try
    ATables := SelectData(AQuery);
    if ATables = nil then
      Exit;
    AResult := TList<string>.Create;
    try
      while not ATables.Eof do
      begin
        AResult.Add(ATables.Fields[0].AsString);
        ATables.Next;
      end;
      Result := AResult.ToArray;
    finally
      ATables.Free;
      AResult.Free;
    end;
  finally
    AQuery.Free;
  end;
end;

procedure TdxSQLiteConnectionProvider.GetTableSchema(ATable: TdxDBTable; ACheckIndexes, ACheckForeignKeys: Boolean);
begin
  GetColumns(ATable);
  GetPrimaryKey(ATable);
  if ACheckIndexes then
    GetIndexes(ATable);
  if ACheckForeignKeys then
    GetForeignKeys(ATable);
end;

function TdxSQLiteConnectionProvider.GetTypeFromString(ATypeName: string; out ASize: Integer): TdxDBColumnType;
var
  ASizeText: string;
begin
  ASize := 0;
  ATypeName := LowerCase(ATypeName);
  ASizeText := TdxFormatterHelper.ExtractFromBrackets(ATypeName);
  if ASizeText <> '' then
    ATypeName := {$IFDEF DELPHIXE3}ATypeName.Substring(0, ATypeName.IndexOf('(')){$ELSE}
      TdxStringHelper.Substring(ATypeName, TdxStringHelper.IndexOf(ATypeName, ('('))){$ENDIF};
  if not DBTypes.TryGetValue(ATypeName, Result) then
    Exit;
  if (Result in [TdxDBColumnType.String, TdxDBColumnType.ByteArray]) and (ASizeText <> '') then
    ASize := StrToInt(ASizeText);
  if (Result = TdxDBColumnType.String) then
    if ((ATypeName = 'char') or (ATypeName = 'nchar')) and (ASize <= 1) then
      Result := TdxDBColumnType.Char;
end;

function TdxSQLiteConnectionProvider.NeedsIndexForForeignKey: Boolean;
begin
  Result := False;
end;

function TdxSQLiteConnectionProvider.GetSQLCreateColumnTypeForString(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  if (AColumn.Size > 0) and (AColumn.Size <= MaximumStringSize) then
    Result := Format('nvarchar(%d)', [AColumn.Size])
  else
    if AColumn.Size = 0 then
      Result := Format('nvarchar(%d)', [DefaultStringSize])
    else
      Result := 'text';
end;

function TdxSQLiteConnectionProvider.GetSQLCreateColumnTypeForDateTime(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'datetime';
end;

function TdxSQLiteConnectionProvider.GetSQLCreateColumnTypeForGuid(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'char(36)';
end;

function TdxSQLiteConnectionProvider.GetSQLCreateColumnTypeForByteArray(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'blob';
end;

function TdxSQLiteConnectionProvider.GetBraceJoin: Boolean;
begin
  Result := False;
end;

function TdxSQLiteConnectionProvider.GetMaximumStringSize: Integer;
begin
  Result := 800;
end;

procedure TdxSQLiteConnectionProvider.GetColumns(ATable: TdxDBTable);
var
  AQuery: TdxQuery;
  ADataSet: TDataSet;
  ASize: Integer;
  AColumn: TdxDBColumn;
  AType: TdxDBColumnType;
  AFieldName, AFieldType, AFieldNotNull, AFieldDefaultValue, APrimaryKey: TField;
begin
  AQuery := TdxQuery.Create('PRAGMA table_info('#$27 + ComposeSafeTableName(ATable.Name) + #$27')');
  try
    ADataSet := SelectData(AQuery);
    try
      AFieldName := ADataSet.Fields[1];
      AFieldType := ADataSet.Fields[2];
      AFieldNotNull := ADataSet.Fields[3];
      AFieldDefaultValue := ADataSet.Fields[4];
      APrimaryKey := ADataSet.Fields[5];
      while not ADataSet.Eof do
      begin
        AType := GetTypeFromString(AFieldType.AsString, ASize);
        AColumn := TdxDBColumn.Create(AFieldName.AsString, APrimaryKey.AsInteger <> 0, AFieldType.AsString, ASize, AType);
        if AFieldDefaultValue.AsString <> '' then
          AColumn.DefaultValue := AFieldDefaultValue.AsString;
        AColumn.IsNullable := AFieldNotNull.AsInteger = 0;
        ATable.AddColumn(AColumn);
        ADataSet.Next;
      end;
    finally
      ADataSet.Free;
    end;
  finally
    AQuery.Free;
  end;
end;

function TdxSQLiteConnectionProvider.GetConnectionParameter(const AConnectionType: TdxConnectionType;
  AParameter: TdxConnectionParameter): string;
begin
  Result := ConnectionParameters[AConnectionType][AParameter];
end;

class function TdxSQLiteConnectionProvider.GetConnectionParameters: TDictionary<string, TdxConnectionParameterValues>;
begin
  if FConnectionParameters = nil then
  begin
    FConnectionParameters := TDictionary<string, TdxConnectionParameterValues>.Create;
    PopulateConnectionParameters;
  end;
  Result := FConnectionParameters;
end;

function TdxSQLiteConnectionProvider.GetDataForTables(const ATables: TArray<TdxDBTable>;
  AFilter: TdxSQLiteConnectionProvider.TTableFilter; const AQueryText: string): TDataSet;
var
  AParameters: TdxQueryParameterCollection;
  AInList: TList<string>;
  ATable: TdxDBTable;
  AQuery: TdxQuery;
begin
  AParameters := TdxQueryParameterCollection.Create;
  AInList := TList<string>.Create;
  try
    for ATable in ATables do
    begin
      if Assigned(AFilter) and not AFilter(ATable) then
        Continue;
      AParameters.Add(TdxOperandValue.Create(ComposeSafeTableName(ATable.Name)));
      AInList.Add(GetParamAlias(AInList.Count));
    end;
    if AInList.Count = 0 then
      Exit(nil);
    AQuery := TdxQuery.Create(Format(AQueryText,
      [TdxFormatterHelper.Concat(AInList, function (const AValue: string): string
       begin
         Result := ':' + AValue;
       end,
       ',')]), AParameters, AInList.ToArray);
    try
      Result := SelectData(AQuery);
    finally
      AQuery.Free;
    end;
  finally
    AParameters.Free;
    AInList.Free;
  end;
end;

procedure TdxSQLiteConnectionProvider.GetForeignKeys(ATable: TdxDBTable);
var
  AQuery: TdxQuery;
  ADataSet: TDataSet;
  APrimaryKeyColumns, AForeignKeyColumns: TList<string>;
begin
  APrimaryKeyColumns := TList<string>.Create;
  AForeignKeyColumns := TList<string>.Create;
  AQuery := TdxQuery.Create('pragma foreign_key_list('#$27 + ComposeSafeTableName(ATable.Name) + #$27')');
  try
    try
      ADataSet := SelectData(AQuery);
    except
      ADataSet := nil;
    end;
    if ADataSet = nil then
      Exit;
    while not ADataSet.Eof do
    begin
      APrimaryKeyColumns.Clear;
      AForeignKeyColumns.Clear;
      APrimaryKeyColumns.Add(ADataSet.Fields[3].AsString);
      AForeignKeyColumns.Add(ADataSet.Fields[4].AsString);
      ATable.ForeignKeys.Add(TdxDBForeignKey.Create([APrimaryKeyColumns[0]],
        {$IFDEF DELPHIXE3}ADataSet.Fields[2].AsString.Trim(['"']){$ELSE}TdxStringHelper.Trim(ADataSet.Fields[2].AsString, ['"']){$ENDIF},
        [AForeignKeyColumns[0]]));
      ADataSet.Next;
    end;
  finally
    APrimaryKeyColumns.Free;
    AForeignKeyColumns.Free;
    AQuery.Free;
  end;
end;

function TdxSQLiteConnectionProvider.GetIdentity(ASQL: TdxQuery): Int64;
begin
  Execute(ASQL);
  Result := ConnectionVendor.ExecuteScalar('select last_insert_rowid()');
end;

procedure TdxSQLiteConnectionProvider.GetIndexes(ATable: TdxDBTable);
var
  AQuery, AQueryIndexData: TdxQuery;
  ADataSet, AIndexData: TDataSet;
  AColumns: TList<string>;
  AField1, AField2, AFieldIndexData2: TField;
begin
  AColumns := TList<string>.Create;
  AQuery := TdxQuery.Create('pragma index_list('#$27 + ComposeSafeTableName(ATable.Name) + #$27')');
  try
    try
      ADataSet := SelectData(AQuery);
    except
      ADataSet := nil;
    end;
    if ADataSet = nil then
      Exit;
    AField1 := ADataSet.Fields[1];
    AField2 := ADataSet.Fields[2];
    while not ADataSet.Eof do
    begin
      AColumns.Clear;
      AQueryIndexData := TdxQuery.Create('pragma index_info('#$27 + AField1.AsString + #$27')');
      try
        AIndexData := SelectData(AQueryIndexData);
        if AIndexData <> nil then
        try
          AFieldIndexData2 := AIndexData.Fields[2];
          while not AIndexData.Eof do
          begin
            AColumns.Add(AFieldIndexData2.AsString);
            AIndexData.Next;
          end;
        finally
          AIndexData.Free;
        end;
        ATable.Indexes.Add(TdxDBIndex.Create(AField1.AsString, AColumns.ToArray, AField2.AsInteger = 1));
      finally
        AQueryIndexData.Free;
      end;
      ADataSet.Next;
    end;
  finally
    AColumns.Free;
    AQuery.Free;
  end;
end;

function TdxSQLiteConnectionProvider.SupportsNativeSkipTake: Boolean;
begin
  Result := True;
end;

function TdxSQLiteConnectionProvider.GetParameterName(const AParameter: IdxOperandValue; AIndex: Integer;
  var ACreateParameter: Boolean): string;
begin
  ACreateParameter := False;
  if (AParameter is TdxConstantValue) and (not AParameter.Value.IsEmpty) then
  begin
    if AParameter.Value.IsType<Boolean> then
    begin
      if AParameter.Value.AsBoolean then
        Exit('1')
      else
        Exit('0');
    end
    else
    case AParameter.Value.Kind of
      tkInteger:
        Exit(IntToStr(AParameter.Value.AsInteger));
      tkEnumeration:
        Exit(IntToStr(AParameter.Value.AsOrdinal));
      tkString, tkLString, tkWString, tkUString:
        Exit(Concat(#$27, ReplaceText(AParameter.Value.AsString, #$27, #$27#$27), #$27));
    end;
  end;
  ACreateParameter := True;
  Result := GetParamAlias(AIndex);
end;

procedure TdxSQLiteConnectionProvider.GetPrimaryKey(ATable: TdxDBTable);
var
  AQuery: TdxQuery;
  ADataSet: TDataSet;
  AField1, AField5: TField;
  AColumns: TList<string>;
  AColumnName: string;
  AColumn: TdxDBColumn;
begin
  AColumns := TList<string>.Create;
  AQuery := TdxQuery.Create('PRAGMA table_info('#$27 + ComposeSafeTableName(ATable.Name) + #$27')');
  try
    ADataSet := SelectData(AQuery);
    if ADataSet = nil then
      Exit;
    AField1 := ADataSet.Fields[1];
    AField5 := ADataSet.Fields[5];
    while not ADataSet.Eof do
    begin
      if AField5.AsInteger >= 1 then
      begin
        AColumnName := AField1.AsString;
        AColumn := ATable.GetColumn(AColumnName);
        if AColumn <> nil then
          AColumn.IsKey := True;
        AColumns.Add(AColumnName);
      end;
      ADataSet.Next;
    end;
    if AColumns.Count > 0 then
      ATable.PrimaryKey := TdxDBPrimaryKey.Create(AColumns)
    else
      ATable.PrimaryKey := nil;
  finally
    AColumns.Free;
    AQuery.Free;
  end;
end;

function TdxSQLiteConnectionProvider.GetSafeNameTableMaxLength: Integer;
begin
  Result := 1024;
end;

function TdxSQLiteConnectionProvider.GetSQLCreateColumnFullAttributes(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := GetSQLCreateColumnType(ATable, AColumn);
  if AColumn.ColumnType = TdxDBColumnType.Boolean then
    Exit;
  if AColumn.IsKey then
  begin
    if (AColumn.IsIdentity and (AColumn.ColumnType in [TdxDBColumnType.Int32, TdxDBColumnType.Int64])) and
      IsSingleColumnPKColumn(ATable, AColumn) then
      Result := 'INTEGER PRIMARY KEY AUTOINCREMENT'
    else
      Result := Result + ' NOT NULL';
  end
  else
    if AColumn.IsNullable then
      Result := Result + ' NULL'
    else
      Result := Result + ' NOT NULL';
end;

function TdxSQLiteConnectionProvider.FormatTable(const ASchemaName: string; const ATableName: string): string;
begin
  Result := '[' + ATableName + ']';
end;

function TdxSQLiteConnectionProvider.FormatTable(const ASchemaName: string; const ATableName: string; const ATableAlias: string): string;
begin
  Result := '[' + ATableName + '] ' + ATableAlias;
end;

function TdxSQLiteConnectionProvider.FormatUpdate(const ATableName, ASetClause, AWhereClause: string): string;
begin
  Result := Format('update %s set %s where %s', [ATableName, ASetClause, AWhereClause]);
end;

function TdxSQLiteConnectionProvider.FormatColumn(const AColumnName: string): string;
begin
  Result := '[' + AColumnName + ']';
end;

function TdxSQLiteConnectionProvider.CollectTablesToCreate(const ATables: TArray<TdxDBTable>): TArray<TdxDBTable>;

 procedure ProcessTables(ADataSet: TDataSet; ADBTables: TDictionary<string, Boolean>; AIsView: Boolean);
 var
   AField: TField;
 begin
   if ADataSet = nil then
     Exit;
   AField := ADataSet.Fields[0];
   while not ADataSet.Eof do
   begin
     ADBTables.Add(AField.AsString, AIsView);
     ADataSet.Next;
   end;
 end;

var
  ADBTables: TDictionary<string, Boolean>;
  ADataSet: TDataSet;
  AList: TList<TdxDBTable>;
  ATable: TdxDBTable;
  AIsView: Boolean;
begin
  ADBTables := TDictionary<string, Boolean>.Create;
  try
    ADataSet := GetDataForTables(ATables, nil,
      'SELECT [name] FROM [sqlite_master] WHERE [type] LIKE ''table'' and [name] in (%s)');
    try
      ProcessTables(ADataSet, ADBTables, False);
    finally
      ADataSet.Free;
    end;
    ADataSet := GetDataForTables(ATables, nil,
      'SELECT [name] FROM [sqlite_master] WHERE [type] LIKE ''view'' and [name] in (%s)');
    try
      ProcessTables(ADataSet, ADBTables, True);
    finally
      ADataSet.Free;
    end;
    AList := TList<TdxDBTable>.Create;
    try
      for ATable in ATables do
      begin
        if ADBTables.TryGetValue(ComposeSafeTableName(ATable.Name), AIsView) then
          ATable.IsView := AIsView
        else
          AList.Add(ATable);
      end;
      Result := AList.ToArray;
    finally
      AList.Free;
    end;
  finally
    ADBTables.Free;
  end;
end;

class destructor TdxSQLiteConnectionProvider.Destroy;
begin
  FreeAndNil(FDBTypes);
end;

function TdxSQLiteConnectionProvider.FormatBinary(AOperatorType: TdxBinaryOperatorType; const ALeftOperand,
  ARightOperand: string): string;
begin
  case AOperatorType of
    TdxBinaryOperatorType.BitwiseAnd:
      Result := Format('%s & %s', [ALeftOperand, ARightOperand]);
    TdxBinaryOperatorType.BitwiseOr:
      Result := Format('%s | %s', [ALeftOperand, ARightOperand]);
    TdxBinaryOperatorType.BitwiseXor:
      Result := Format('(~(%0:s&%1:s) & (%0:s|%1:s))', [ALeftOperand, ARightOperand]);
    TdxBinaryOperatorType.Modulo:
      Result := Format('%s % %s', [ALeftOperand, ARightOperand]);
    else
      Result := inherited FormatBinary(AOperatorType, ALeftOperand, ARightOperand);
  end;
end;

function TdxSQLiteConnectionProvider.FormatColumn(const AColumnName: string; const ATableAlias: string): string;
begin
  Result := ATableAlias + '.[' + AColumnName + ']';
end;

function TdxSQLiteConnectionProvider.FormatConstraint(const AConstraintName: string): string;
begin
  Result := '[' + AConstraintName + ']';
end;

function TdxSQLiteConnectionProvider.FormatDelete(const ATableName, AWhereClause: string): string;
begin
  Result := Format('delete from %s where %s', [ATableName, AWhereClause]);
end;

function TdxSQLiteConnectionProvider.FormatFunction(AOperatorType: TdxFunctionOperatorType;
  const AOperands: TArray<string>): string;
begin
  case AOperatorType of
    TdxFunctionOperatorType.Abs:
      Result := Format('Abs(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Sign:
      Result := Format('Sign(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.BigMul:
      Result := Format('CAST((%s) * (%s) as BIGINT)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Round:
      case Length(AOperands) of
        1:
          Result := Format('Round(%s)', [AOperands[0]]);
        2:
          Result := Format('Round(%s,%s)', [AOperands[0], AOperands[1]]);
        else
          Result := inherited FormatFunction(AOperatorType, AOperands);
      end;
    TdxFunctionOperatorType.Floor:
      Result := Format('Floor(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Ceil:
      Result := Format('Ceiling(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Cos:
      Result := Format('Cos(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Sin:
      Result := Format('Sin(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcCos:
      Result := Format('ACos(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcSin:
      Result := Format('ASin(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Cosh:
      Result := Format('Cosh(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Sinh:
      Result := Format('Sinh(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Exp:
      Result := Format('Exp(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Log:
      case Length(AOperands) of
        1:
          Result := Format('Log(%s)', [AOperands[0]]);
        2:
          Result := Format('(Log(%s) / Log(%s))', [AOperands[0], AOperands[1]]);
        else
          Result := inherited FormatFunction(AOperatorType, AOperands);
      end;
    TdxFunctionOperatorType.Log10:
      Result := Format('Log10(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcTan:
      Result := Format('Atan(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcTan2:
      Result := Format('Atan2(%s, %s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Tan:
      Result := Format('Tan(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Tanh:
      Result := Format('Tanh(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Power:
      Result := Format('Power(%s, %s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Sqr:
      Result := Format('Sqrt(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Random:
      Result := '((random() / 18446744073709551614) + 0.5)';
    TdxFunctionOperatorType.Now:
      Result := 'datetime('#$27'now'#$27','#$27'localtime'#$27')';
    TdxFunctionOperatorType.UtcNow:
      Result := 'datetime('#$27'now'#$27')';
    TdxFunctionOperatorType.Today:
      Result := 'datetime(date('#$27'now'#$27','#$27'localtime'#$27'))';
    TdxFunctionOperatorType.Replace:
      Result := Format('Replace(%s, %s, %s)', [AOperands[0], AOperands[1], AOperands[2]]);
    TdxFunctionOperatorType.Reverse:
      Result := Format('Reverse(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Insert:
      Result := Format('(SUBSTR(%0:s, 1, %1:s) || (%2:s) || SUBSTR(%0:s, (%1:s) + 1))',
        [AOperands[0], AOperands[1], AOperands[2]]);
    TdxFunctionOperatorType.Remove:
      case Length(AOperands) of
        2:
          Result := Format('SUBSTR(%0:s, 1, %1:s)', [AOperands[0], AOperands[1]]);
        3:
          Result := Format('(SUBSTR(%0:s, 1, %1:s) || SUBSTR(%0:s, (%1:s) + (%2:s) + 1))',
            [AOperands[0], AOperands[1], AOperands[2]]);
        else
          Result := inherited FormatFunction(AOperatorType, AOperands);
      end;
    TdxFunctionOperatorType.IncTick:
      Result := Format('strftime('#$27'%%Y-%%m-%%d %%H:%%M:%%f'#$27', %0:s , CAST(((%1:s) / 10000000) as text) || '#$27' second'#$27')',
        [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncMillisecond:
      Result := Format('strftime('#$27'%%Y-%%m-%%d %%H:%%M:%%f'#$27', %0:s , CAST(((%1:s) / 1000) as text) || '#$27' second'#$27')',
        [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.AddTimeSpan,
    TdxFunctionOperatorType.IncSecond:
      Result := Format('strftime('#$27'%%Y-%%m-%%d %%H:%%M:%%f'#$27', %0:s , CAST(%1:s as text) || '#$27' second'#$27')',
        [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncMinute:
      Result := Format('strftime('#$27'%%Y-%%m-%%d %%H:%%M:%%f'#$27', %0:s , CAST(%1:s as text) || '#$27' minute'#$27')',
        [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncHour:
      Result := Format('strftime('#$27'%%Y-%%m-%%d %%H:%%M:%%f'#$27', %0:s , CAST(%1:s as text) || '#$27' hour'#$27')',
        [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncDay:
      Result := Format('strftime('#$27'%%Y-%%m-%%d %%H:%%M:%%f'#$27', %0:s , CAST(%1:s as text) || '#$27' day'#$27')',
        [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncMonth:
      Result := Format('strftime('#$27'%%Y-%%m-%%d %%H:%%M:%%f'#$27', %0:s , CAST(%1:s as text) || '#$27' month'#$27')',
        [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncYear:
      Result := Format('strftime('#$27'%%Y-%%m-%%d %%H:%%M:%%f'#$27', %0:s , CAST(%1:s as text) || '#$27' year'#$27')',
        [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MillisecondOf:
      Result := Format('(CAST((strftime('#$27'%%f'#$27', %0:s) * 1000) as integer) % 1000)', [AOperands[0]]);
    TdxFunctionOperatorType.SecondOf:
      Result := Format('CAST(strftime('#$27'%%S'#$27', %s) as integer)', [AOperands[0]]);
    TdxFunctionOperatorType.MinuteOf:
      Result := Format('CAST(strftime('#$27'%%M'#$27', %s) as integer)', [AOperands[0]]);
    TdxFunctionOperatorType.HourOf:
      Result := Format('CAST(strftime('#$27'%%H'#$27', %s) as integer)', [AOperands[0]]);
    TdxFunctionOperatorType.DayOf:
      Result := Format('CAST(strftime('#$27'%%d'#$27', %s) as integer)', [AOperands[0]]);
    TdxFunctionOperatorType.MonthOf:
      Result := Format('CAST(strftime('#$27'%%m'#$27', %s) as integer)', [AOperands[0]]);
    TdxFunctionOperatorType.YearOf:
      Result := Format('CAST(strftime('#$27'%%Y'#$27', %s) as integer)', [AOperands[0]]);
    TdxFunctionOperatorType.DayOfTheWeek:
      Result := Format('CAST(strftime('#$27'%%w'#$27', %s) as integer)', [AOperands[0]]);
    TdxFunctionOperatorType.DayOfTheYear:
      Result := Format('CAST(strftime('#$27'%%j'#$27', %s) as integer)', [AOperands[0]]);
    TdxFunctionOperatorType.GetTimeOfDay:
      Result := Format('(CAST((strftime('#$27'%%H'#$27', %0:s) * 36000000000) as integer) + ' +
        'CAST((strftime('#$27'%%M'#$27', %0:s) * 600000000) as integer) + ' +
        'CAST((strftime('#$27'%%f'#$27', %0:s) * 10000000) as integer))', [AOperands[0]]);
    TdxFunctionOperatorType.DateOf:
      Result := Format('date(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.YearsBetween:
      Result := Format('(strftime('#$27'%%Y'#$27', %1:s) - strftime('#$27'%%Y'#$27', %0:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MonthsBetween:
      Result := Format('(((strftime('#$27'%%Y'#$27', %1:s) - strftime('#$27'%%Y'#$27', %0:s)) * 12) + ' +
        'strftime('#$27'%%m'#$27', %1:s) - strftime('#$27'%%m'#$27', %0:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.DaysBetween:
      Result := Format('(julianday(date(%1:s)) - julianday(date(%0:s)))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.HoursBetween:
      Result := Format('(((julianday(date(%1:s)) - julianday(date(%0:s))) * 24) + (strftime('#$27'%%H'#$27', %1:s) - ' +
        'strftime('#$27'%%H'#$27', %0:s)))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MinutesBetween:
      Result := Format('((((((julianday(date(%1:s)) - julianday(date(%0:s))) * 24) + (strftime('#$27'%%H'#$27', %1:s) - ' +
        'strftime('#$27'%%H'#$27', %0:s)))) * 60)  + (strftime('#$27'%%M'#$27', %1:s) - strftime('#$27'%%M'#$27', %0:s)))',
        [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.SecondsBetween:
      Result := Format('(strftime('#$27'%%s'#$27', %1:s) - strftime('#$27'%%s'#$27', %0:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MillisecondsBetween:
      Result := Format('((strftime('#$27'%%s'#$27', %1:s) - strftime('#$27'%%s'#$27', %0:s)) * 1000)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.DateDiffTicks:
      Result := Format('((strftime('#$27'%%s'#$27', %1:s) - strftime('#$27'%%s'#$27', %0:s))) * 10000000', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Len:
      Result := 'Length(' + TdxFormatterHelper.Concat(AOperands) + ')';
    TdxFunctionOperatorType.PadLeft:
      case Length(AOperands) of
        2:
          Result := Format('PadL(%0:s, %1:s)', [AOperands[0], AOperands[1]]);
        else
          raise ENotSupportedException.Create('');
      end;
    TdxFunctionOperatorType.PadRight:
      case Length(AOperands) of
        2:
          Result := Format('PadR(%0:s, %1:s)', [AOperands[0], AOperands[1]]);
        else
          raise ENotSupportedException.Create('');
      end;
    TdxFunctionOperatorType.CharIndex:
      case Length(AOperands) of
        2:
          Result := Format('(CharIndex(%0:s, %1:s) - 1)', [AOperands[0], AOperands[1]]);
        3:
          Result := Format('(CharIndex(%0:s, %1:s, (%2:s) + 1) - 1)', [AOperands[0], AOperands[1], AOperands[2]]);
        4:
          Result := Format('(CharIndex(%0:s, Substr(%1:s, 1, (%2:s) + (%3:s)), (%2:s) + 1) - 1)',
            [AOperands[0], AOperands[1], AOperands[2], AOperands[3]]);
        else
          Result := inherited FormatFunction(AOperatorType, AOperands);
      end;
    TdxFunctionOperatorType.Substring:
      case Length(AOperands) of
        2:
          Result := Format('substr(%0:s, (%1:s) + 1)', [AOperands[0], AOperands[1]]);
        3:
          Result := Format('substr(%0:s, (%1:s) + 1, %2:s)', [AOperands[0], AOperands[1], AOperands[2]]);
        else
          Result := inherited FormatFunction(AOperatorType, AOperands);
      end;
    TdxFunctionOperatorType.ToInteger:
      Result := Format('CAST(%s as int)', [AOperands[0]]);
    TdxFunctionOperatorType.ToInt64:
      Result := Format('CAST(%s as bigint)', [AOperands[0]]);
    TdxFunctionOperatorType.ToSingle:
      Result := Format('CAST(%s as real)', [AOperands[0]]);
    TdxFunctionOperatorType.ToDouble:
      Result := Format('CAST(%s as double precision)', [AOperands[0]]);
    TdxFunctionOperatorType.ToDecimal:
      Result := Format('CAST(%s as money)', [AOperands[0]]);
    TdxFunctionOperatorType.ToString:
      Result := Format('CAST(%s as text)', [AOperands[0]]);
    TdxFunctionOperatorType.Ascii,
    TdxFunctionOperatorType.Char,
    TdxFunctionOperatorType.Max,
    TdxFunctionOperatorType.Min:
      raise ENotSupportedException.Create('');
    TdxFunctionOperatorType.IsNull:
      case Length(AOperands) of
        1:
          Result := Format('((%s) is null)', [AOperands[0]]);
        2:
          Result := Format('COALESCE(%0:s, %1:s)', [AOperands[0], AOperands[1]]);
        else
          Result := inherited FormatFunction(AOperatorType, AOperands);
      end;
    TdxFunctionOperatorType.IsNullOrEmpty:
      Result := Format('((%0:s) is null or length(%0:s) = 0)', [AOperands[0]]);
    TdxFunctionOperatorType.EndsWith:
      Result := Format('(Substr(%0:s, Length(%0:s) - Length(%1:s) + 1) = (%1:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Contains:
      Result := Format('(CharIndex(%1:s, %0:s) > 0)', [AOperands[0], AOperands[1]]);
    else
      Result := inherited FormatFunction(AOperatorType, AOperands);
  end;
end;

function TdxSQLiteConnectionProvider.FormatInsert(const ATableName, AFieldNames, AValuesClause: string): string;
begin
  Result := Format('insert into %s (%s) values(%s)', [ATableName, AFieldNames, AValuesClause]);
end;

function TdxSQLiteConnectionProvider.FormatInsertDefaultValues(ATable: TdxDBTable): string;
var
  AValues: string;
begin
  AValues := DupeString('null,', ATable.Columns.Count);
  Delete(AValues, Length(AValues), 1);
  Result := Format('insert into %s values(%s)', [FormatTableSafe(ATable), AValues]);
end;

function TdxSQLiteConnectionProvider.FormatSelect(const ASelectClause, AFromClause, AWhereClause, AOrderByClause,
  AGroupByClause, AHavingClause: string; ASkipSelectedRecords, ATopSelectedRecords: Integer): string;
var
  AExpandedWhereClause, AExpandedOrderByClause, AExpandedHavingClause, AExpandedGroupByClause, AModificatorsSQL: string;
begin
  inherited FormatSelect(ASelectClause, AFromClause, AWhereClause, AOrderByClause, AGroupByClause, AHavingClause,
    ASkipSelectedRecords, ATopSelectedRecords);
  if AWhereClause <> '' then
    AExpandedWhereClause := #10'where ' + AWhereClause
  else
    AExpandedWhereClause := '';
  if AOrderByClause <> '' then
    AExpandedOrderByClause := #10'order by ' + AOrderByClause
  else
    AExpandedOrderByClause := '';
  if AHavingClause <> '' then
    AExpandedHavingClause := #10'having ' + AHavingClause
  else
    AExpandedHavingClause := '';
  if AGroupByClause <> '' then
    AExpandedGroupByClause := #10'group by ' + AGroupByClause
  else
    AExpandedGroupByClause := '';
  if (ASkipSelectedRecords <> 0) or (ATopSelectedRecords <> 0) then
    AModificatorsSQL := Format(' LIMIT %d OFFSET %d ',
      [IfThen(ATopSelectedRecords = 0, MaxInt, ATopSelectedRecords), ASkipSelectedRecords])
  else
    AModificatorsSQL := '';
  Result := Format('select %s from %s%s%s%s%s%s',
    [ASelectClause, AFromClause, AExpandedWhereClause, AExpandedGroupByClause, AExpandedHavingClause, AExpandedOrderByClause, AModificatorsSQL]);
end;

function TdxSQLiteConnectionProvider.FormatFunction(AProcessParameter: TdxProcessParameter;
  AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<TValue>): string;

  function ProcessParameter(const AValue: string): string;
  var
    AParameter: IdxCriteriaOperator;
  begin
    AParameter := TdxConstantValue.Create(AValue);
    Result := AProcessParameter(AParameter);
  end;

var
  ASecondOperator, AValueOperator: TdxCriteriaOperator;
  ASecondOperand: TdxOperandValue;
  AOperandString: string;
  ALikeIndex: Integer;
begin
  case AOperatorType of
    TdxFunctionOperatorType.StartsWith:
      begin
        ASecondOperator := Safe<TdxCriteriaOperator>.Cast(AOperands[0].AsObject);
        if AOperands[1].IsObject then
          ASecondOperand := Safe<TdxOperandValue>.Cast(AOperands[1].AsObject)
        else
          ASecondOperand := nil;

        if (ASecondOperand <> nil) and ASecondOperand.Value.IsString then
        begin
          AOperandString := ASecondOperand.Value.AsString;
        {$IFDEF DELPHIXE3}
          ALikeIndex := AOperandString.IndexOfAny(StopChars);
        {$ELSE}
          ALikeIndex := TdxStringHelper.IndexOfAny(AOperandString, StopChars);
        {$ENDIF}
          if ALikeIndex < 0 then
            Exit(Format('(%s like %s)', [AProcessParameter(ASecondOperator), ProcessParameter(AOperandString + '%')]))
          else
            if ALikeIndex > 0 then
              Exit(Format('((%0:s like %2:s) And (Substr(%0:s, 1, Length(%1:s)) = (%1:s)))',
                [AProcessParameter(ASecondOperator), AProcessParameter(ASecondOperand),
                 ProcessParameter({$IFDEF DELPHIXE3}AOperandString.Substring(0, ALikeIndex){$ELSE}TdxStringHelper.Substring(AOperandString, 0, ALikeIndex){$ENDIF} + '%')]));
        end;
        AValueOperator := Safe<TdxCriteriaOperator>.Cast(AOperands[1].AsObject);
        Result := Format('(Substr(%0:s, 1, Length(%1:s)) = (%1:s))', [AProcessParameter(ASecondOperator), AProcessParameter(AValueOperator)]);
      end;
    else
      Result := inherited FormatFunction(AProcessParameter, AOperatorType, AOperands);
  end;
end;

initialization
  TdxSQLConnectionProviderFactory.Register(TdxSQLiteConnectionProvider);

finalization
  TdxSQLConnectionProviderFactory.Unregister(TdxSQLiteConnectionProvider);

end.
