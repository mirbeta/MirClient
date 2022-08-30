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

unit dxEMF.DataProvider.FireDAC;

{$IF (CompilerVersion >= 30) AND (DEFINED(IOS) OR DEFINED(ANDROID))}
  {$HPPEMIT LINKUNIT}
{$ELSE}
  {$HPPEMIT '#pragma link "dxEMF.DataProvider.FireDAC"'}
{$IFEND}

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  Classes, SysUtils, Generics.Defaults, Generics.Collections, Rtti, DB,
{$IFDEF DELPHIXE5}
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Stan.Def, FireDAC.Stan.Intf, FireDAC.Phys.Intf, FireDAC.Comp.Script,
{$ELSE}
  uADCompClient, uADStanParam, uADStanIntf, uADPhysIntf,
{$ENDIF}
  dxEMF.Utils.FireDACAliases,
  dxEMF.Metadata,
  dxEMF.Core.Loader,
  dxEMF.Core,
  dxEMF.Types,
  dxEMF.DB.Model,
  dxEMF.DB.Query,
  dxEMF.DB.Generator,
  dxEMF.DB.SQLConnectionProvider;

type

  {$WARN UNSUPPORTED_CONSTRUCT OFF}

  { TdxFireDACCommand }

  TdxFireDACCommand = class(TdxCustomDBCommand)
  strict private
    [HPPGEN('TFDQuery* FCommandExecutor;')]
    FCommandExecutor: TFDQuery;
  protected
    procedure SetCommandText(const Value: string); override;
    procedure PrepareParameters(AConvertParameter: TdxConvertParameter); override;
  public
  	[HPPGEN('__fastcall TdxFireDACCommand(TFDConnection* AConnection, Dxemf::Db::Query::TdxQuery* AQuery);')]
    constructor Create(AConnection: TFDConnection; AQuery: TdxQuery);
    destructor Destroy; override;
    function ExecuteNonQuery: Integer; override;
    function GetScalar: Variant; override;
  end;

  { TdxFireDACConnectionVendor }

  TdxFireDACConnectionVendor = class(TdxCustomConnectionVendor, IdxDBTransaction)
  strict private
    const
      SchemaInfo: array[TdxSchemaInfo] of TFDPhysMetaInfoKind = (
        mkTables,
        mkTables,
        mkProcs,
        mkTableFields,
        mkIndexes,
        mkIndexFields,
        mkPrimaryKeyFields,
        mkForeignKeys,
        mkForeignKeyFields
      );
  strict private class var
    FDriverNameAliases: TDictionary<string, TdxDBEngine>;
    class destructor Destroy;
    class procedure PopulateDriverNameAliases;
    class function GetDriverNameAliases: TDictionary<string, TdxDBEngine>; static;
    class function GetDriverNameAlias(const ADriverID: string): TdxDBEngine; static;
    class function GetType(AType: TFDDataType): TdxDBColumnType; static;
  strict private
    [HPPGEN('TFDConnection* __fastcall GetConnection(void);')]
    function GetConnection: TFDConnection; inline;
    procedure GetColumns(ATable: TdxDBTable; const ASchemaName, ATableName: string);
    procedure GetPrimaryKey(ATable: TdxDBTable; const ASchemaName, ATableName: string);
    procedure GetIndexes(ATable: TdxDBTable; const ASchemaName, ATableName: string);
    procedure GetForeignKeys(ATable: TdxDBTable; const ASchemaName, ATableName: string);
  protected
  	[HPPGEN('static void __fastcall AssignParameterValue(TFDParam* AParameter, const System::Rtti::TValue &AValue);')]
    class procedure AssignParameterValue(AParameter: TFDParam; const AValue: TValue); static;
    class function GetCaseSensitiveFieldNames: Boolean; override;
    { IdxDBTransaction }
    procedure Commit;
    procedure Rollback;

    function GetConnectionString: string; override;
    function CreateCommand(AQuery: TdxQuery): TdxCustomDBCommand; override;
    function GetExpectedDBEngine: TdxDBEngine; override;
    function SelectData(AOwner: TComponent; AQuery: TdxQuery): TDataSet; override;
  public
    class function GetConnectionType: TdxConnectionType; override;
    function BeginTransaction: IdxDBTransaction; override;
    function CreateConnection(ASQLConnectionProvider: TdxSQLConnectionProvider;
      const ADatabaseName: string = ''): TCustomConnection; override;
    function CreateDataSet(AQuery: TdxQuery; AOwner: TComponent = nil): TDataSet; override;
    function Execute(const ACommandText: string): Integer; override;
    function ExecuteScalar(AConnection: TCustomConnection; const ACommandText: string): Variant; override;
    procedure GetTableSchema(ATable: TdxDBTable; ACheckIndexes, ACheckForeignKeys: Boolean); override;
    class property DriverNameAliases: TDictionary<string, TdxDBEngine> read GetDriverNameAliases;
    function GetSchema(ASchemaInfo: TdxSchemaInfo; const ACatalogName, ASchemaName, ATableName: string; const AObjectName: string = ''): TDataSet; override;
    [HPPGEN('__property TFDConnection* Connection = {read=GetConnection};')]
    property Connection: TFDConnection read GetConnection;
  end;

  { TdxEMFFireDACDataProvider }

  TdxEMFFireDACDataProvider = class(TdxEMFCustomDataProvider)
  strict private
    [HPPGEN('TFDConnection* __fastcall GetConnection(void);')]
    function GetConnection: TFDConnection; inline;
    [HPPGEN('HIDESBASE void __fastcall SetConnection(TFDConnection* const Value);')]
    procedure SetConnection(const Value: TFDConnection);
    function GetConnectionVendor: TdxFireDACConnectionVendor; inline;
  protected
    function CreateDatabase: Boolean;
    procedure CheckConnectAndCreate; override;
    function CreateConnectionVendor: TdxCustomConnectionVendor; override;
    function CreateLoader(ASession: TdxEMFCustomSession; AConnectionVendor: TdxCustomConnectionVendor; AQuery: TdxQuery;
      AEntityInfo: TdxEntityInfo; APredicate: TdxPredicate = nil): TdxDataLoader; override;
    property ConnectionVendor: TdxFireDACConnectionVendor read GetConnectionVendor;
  published
    [HPPGEN('__property TFDConnection* Connection = {read=GetConnection, write=SetConnection};')]
    property Connection: TFDConnection read GetConnection write SetConnection;
  end;

implementation

uses
  StrUtils, Variants, TypInfo,
  dxCore,
  dxEMF.Utils.Exceptions,
  dxEMF.Utils,
  dxEMF.DB.Utils,
  dxEMF.DB.SQLGenerator,
  dxEMF.DB.Criteria,
  dxEMF.Core.Pool,
  dxEMF.Strs;


type
  TdxFireDACLoader = class(TdxDataSetLoader<TFDCustomQuery>);

{ TdxFireDACCommand }

constructor TdxFireDACCommand.Create(AConnection: TFDConnection; AQuery: TdxQuery);
begin
  inherited Create(AQuery);
  FCommandExecutor := TFDQuery.Create(nil);
  FCommandExecutor.Connection := AConnection;
  CommandText := AQuery.SQLText;
end;

destructor TdxFireDACCommand.Destroy;
begin
  FreeAndNil(FCommandExecutor);
  inherited Destroy;
end;

function TdxFireDACCommand.ExecuteNonQuery: Integer;
begin
  FCommandExecutor.Execute();
  Result := 0;
end;

function TdxFireDACCommand.GetScalar: Variant;
begin
  try
    FCommandExecutor.Open;
    try
      if FCommandExecutor.FieldCount > 0 then
        Result := FCommandExecutor.Fields[0].Value
      else
        Result := -1;
    finally
      FCommandExecutor.Close;
    end;
  except
    Result := Null;
  end;
end;

procedure TdxFireDACCommand.PrepareParameters(AConvertParameter: TdxConvertParameter);
var
  I: Integer;
  AParameter: TFDParam;
begin
  for I := 0 to Query.Parameters.Count - 1 do
  begin
    AParameter := FCommandExecutor.Params[I];
    Assert(SameText(AParameter.Name, Query.ParameterNames[I]));
    TdxFireDACConnectionVendor.AssignParameterValue(AParameter, AConvertParameter(Query.Parameters[I].Value));
  end;
end;

procedure TdxFireDACCommand.SetCommandText(const Value: string);
begin
  inherited SetCommandText(Value);
  if FCommandExecutor <> nil then
    FCommandExecutor.SQL.Text := Value;
end;

{ TdxFireDACConnectionVendor }

class destructor TdxFireDACConnectionVendor.Destroy;
begin
  FreeAndNil(FDriverNameAliases);
end;

class function TdxFireDACConnectionVendor.GetCaseSensitiveFieldNames: Boolean;
begin
  Result := True;
end;

class procedure TdxFireDACConnectionVendor.AssignParameterValue(AParameter: TFDParam;
  const AValue: TValue);
begin
  case AValue.Kind of
    tkInteger:
      if AValue.TypeInfo = TypeInfo(Cardinal) then
      {$IFDEF DELPHIXE2}
        AParameter.AsLongword := AValue.AsCardinal
      {$ELSE}
        AParameter.AsLargeInt := AValue.AsCardinal
      {$ENDIF}
      else
        AParameter.AsInteger := AValue.AsInteger;
    tkFloat:
      if (AValue.TypeInfo = TypeInfo(TDateTime)) or (AValue.TypeInfo = TypeInfo(TDate)) or
        (AValue.TypeInfo = TypeInfo(TTime)) then
        AParameter.AsDateTime := AValue.AsDateTime
      else
        AParameter.AsFloat := AValue.AsExtended;
    tkRecord:
      if AValue.TypeInfo = TypeInfo(TGUID) then
        AParameter.AsGUID := AValue.AsType<TGUID>
      else
        AParameter.Value := AValue.ToVariant;
    tkInt64:
      AParameter.AsLargeInt := AValue.AsInt64;
    tkUString:
      AParameter.AsString := AValue.AsString;
    tkArray, tkDynArray:
      begin
        AParameter.DataType := ftBlob;
        AParameter.SetBlobRawData(AValue.GetArrayLength, PPointer(AValue.GetReferenceToRawData)^);
      end;
  else
    AParameter.Value := AValue.ToVariant;
  end;
end;

class procedure TdxFireDACConnectionVendor.PopulateDriverNameAliases;
begin
  FDriverNameAliases.Add('MSSQL', TdxDBEngines.MSSQL);
  FDriverNameAliases.Add('MySQL', TdxDBEngines.MySQL);
  FDriverNameAliases.Add('SQLite', TdxDBEngines.SQLite);
{$IFDEF DELPHIXE5}
  FDriverNameAliases.Add('FB', TdxDBEngines.Firebird);
{$ELSE}
  FDriverNameAliases.Add('IB', TdxDBEngines.Firebird);
{$ENDIF}
  FDriverNameAliases.Add('Ora', TdxDBEngines.Oracle);
  FDriverNameAliases.Add('MSAcc', TdxDBEngines.MSAccess);
end;

function TdxFireDACConnectionVendor.GetConnection: TFDConnection;
begin
  Result := TFDConnection(inherited Connection);
end;

procedure TdxFireDACConnectionVendor.Rollback;
begin
  Connection.Rollback;
end;

class function TdxFireDACConnectionVendor.GetDriverNameAliases: TDictionary<string, TdxDBEngine>;
begin
  if FDriverNameAliases = nil then
  begin
    FDriverNameAliases := TDictionary<string, TdxDBEngine>.Create;
    PopulateDriverNameAliases;
  end;
  Result := FDriverNameAliases;
end;

class function TdxFireDACConnectionVendor.GetDriverNameAlias(const ADriverID: string): TdxDBEngine;
begin
  if not DriverNameAliases.TryGetValue(ADriverID, Result) then
    Result := TdxDBEngines.Unknown;
end;

procedure TdxFireDACConnectionVendor.GetColumns(ATable: TdxDBTable; const ASchemaName, ATableName: string);
var
  ADataSet: TDataSet;
  AFieldName: TField;
  AFieldSize: TField;
  AFieldType: TField;
  AFieldTypeName: TField;
  AFieldAttribute: TField;
  AColumn: TdxDBColumn;
  ASize: Integer;
  AFDDataType: TFDDataType;
  AFDDataAttributes: TFDDataAttributes;
  AFDDataAttr: Word absolute AFDDataAttributes;
  AType: TdxDBColumnType;
begin
  Assert(SizeOf(AFDDataAttributes) = SizeOf(AFDDataAttr));
  ADataSet := GetSchema(TdxSchemaInfo.Columns, '', ASchemaName, ATableName);
  try
    AFieldName := ADataSet.FieldByName('COLUMN_NAME');
    AFieldSize := ADataSet.FieldByName('COLUMN_LENGTH');
    AFieldType := ADataSet.FieldByName('COLUMN_DATATYPE');
    AFieldTypeName := ADataSet.FieldByName('COLUMN_TYPENAME');
    AFieldAttribute := ADataSet.FieldByName('COLUMN_ATTRIBUTES');
    while not ADataSet.Eof do
    begin
      AFDDataType := TFDDataType(AFieldType.AsInteger);
      AFDDataAttr := AFieldAttribute.AsInteger;
      ASize := AFieldSize.AsInteger;
      AType := GetType(AFDDataType);
      AColumn := TdxDBColumn.Create(AFieldName.AsString, False, AFieldTypeName.AsString, ASize, AType);
      AColumn.IsNullable := TFDDataAttribute.caAllowNull in AFDDataAttributes;
      AColumn.IsIdentity := TFDDataAttribute.caAutoInc in AFDDataAttributes;
      ATable.Columns.Add(AColumn);
      ADataSet.Next;
    end;
  finally
    ADataSet.Free;
  end;
end;

function TdxFireDACConnectionVendor.GetConnectionString: string;
begin
  Result := Connection.ConnectionString;
end;

function TdxFireDACConnectionVendor.SelectData(AOwner: TComponent; AQuery: TdxQuery): TDataSet;
var
  AResult: TFDQuery;
begin
  AResult := CreateDataSet(AQuery, AOwner) as TFDQuery;
  if not AResult.OpenOrExecute then
    FreeAndNil(AResult);
  Result := AResult;
end;

function TdxFireDACConnectionVendor.BeginTransaction: IdxDBTransaction;
begin
  Connection.StartTransaction;
  Result := Self;
end;

procedure TdxFireDACConnectionVendor.Commit;
begin
  Connection.Commit;
end;

class function TdxFireDACConnectionVendor.GetConnectionType: TdxConnectionType;
begin
  Result := TdxConnectionTypes.FireDAC;
end;

function TdxFireDACConnectionVendor.CreateCommand(AQuery: TdxQuery): TdxCustomDBCommand;
begin
  Result := TdxFireDACCommand.Create(Connection, AQuery);
end;

function TdxFireDACConnectionVendor.CreateConnection(ASQLConnectionProvider: TdxSQLConnectionProvider;
  const ADatabaseName: string): TCustomConnection;
var
  AConnectionStrings: TStrings;
  AConnection: TFDConnection;
  ASB: TStringBuilder;
  S: string;
begin
  ProviderSQL := ASQLConnectionProvider;
  AConnection := TFDConnection.Create(nil);
  if ADatabaseName <> '' then
  begin
    AConnectionStrings := GetParsedConnectionString;
    try
      ASB := TStringBuilder.Create;
      try
        AConnectionStrings.Values[ASQLConnectionProvider.GetConnectionParameter(GetConnectionType, TdxConnectionParameter.Database)] := ADatabaseName;
        for S in AConnectionStrings do
          ASB.Append(S).Append(';');
        AConnection.LoginPrompt := Connection.LoginPrompt;
        AConnection.ConnectionString := ASB.ToString;
      finally
        ASB.Free;
      end
    finally
      AConnectionStrings.Free;
    end
  end
  else
    AConnection.ConnectionString := Connection.ConnectionString;
  Result := AConnection;
end;

function TdxFireDACConnectionVendor.CreateDataSet(AQuery: TdxQuery; AOwner: TComponent): TDataSet;
var
  I: Integer;
  AParameter: TFDParam;
  AResult: TFDQuery;
begin
  AResult := TFDQuery.Create(AOwner);
  AResult.Connection := Connection;
  AResult.SQL.Text := AQuery.SQLText;
  AResult.FetchOptions.Unidirectional := True;
  for I := 0 to AQuery.Parameters.Count - 1 do
  begin
    AParameter := AResult.ParamByName(AQuery.ParameterNames[I]);
    AssignParameterValue(AParameter, AQuery.Parameters[I].Value);
  end;
  Result := AResult;
end;


function TdxFireDACConnectionVendor.GetExpectedDBEngine: TdxDBEngine;
begin
  Result := GetDriverNameAlias(Connection.DriverName);
  if not TdxSQLConnectionProviderFactory.HasProvider(Result) then
    Result := TdxDBEngines.Unknown
end;

procedure TdxFireDACConnectionVendor.GetForeignKeys(ATable: TdxDBTable; const ASchemaName, ATableName: string);
var
  ADataSet, ADataSetFields: TDataSet;
  AForeignKey: TdxDBForeignKey;
  AForeignKeyName: string;
begin
  ADataSet := GetSchema(TdxSchemaInfo.ForeignKeys, '', ASchemaName, ATableName);
  try
    while not ADataSet.Eof do
    begin
      AForeignKeyName := ADataSet.FieldByName('FKEY_NAME').AsString;
      ADataSetFields := GetSchema(TdxSchemaInfo.ForeignKeyFields, '', ASchemaName, ATableName, AForeignKeyName);
      try
        AForeignKey := TdxDBForeignKey.Create(ADataSet.FieldByName('PKEY_TABLE_NAME').AsString);
        while not ADataSetFields.Eof do
        begin
          AForeignKey.Columns.Add(ADataSet.FieldByName('COLUMN_NAME').AsString);
          AForeignKey.PrimaryKeyTableKeyColumns.Add(ADataSet.FieldByName('PKEY_COLUMN_NAME').AsString);
          ADataSetFields.Next;
        end;
        ATable.ForeignKeys.Add(AForeignKey);
      finally
        ADataSetFields.Free;
      end;
      ADataSet.Next;
    end;
  finally
    ADataSet.Free
  end;
end;

procedure TdxFireDACConnectionVendor.GetIndexes(ATable: TdxDBTable; const ASchemaName, ATableName: string);
var
  ADataSet: TDataSet;
  AIndexes: TStringList;
  AField: TField;
  AIndex: TdxDBIndex;
  AIndexKind: TFDPhysIndexKind;
  I: Integer;
begin
  AIndexes := TStringList.Create;
  try
    ADataSet := GetSchema(TdxSchemaInfo.Indexes, '', ASchemaName, ATableName);
    try
      AField := ADataSet.FieldByName('INDEX_NAME');
      while not ADataSet.Eof do
      begin
        AIndexKind := TFDPhysIndexKind(ADataSet.FieldByName('INDEX_TYPE').AsInteger);
        if AIndexKind = TFDPhysIndexKind.ikPrimaryKey then
          Continue;
        AIndex := TdxDBIndex.Create(AField.AsString, nil, AIndexKind = TFDPhysIndexKind.ikUnique);
        AIndexes.AddObject(AField.AsString, AIndex);
        ADataSet.Next;
      end;
    finally
      ADataSet.Free;
    end;
    for I := 0 to AIndexes.Count - 1 do
    begin
      ADataSet := GetSchema(TdxSchemaInfo.IndexFields, '', ASchemaName, ATableName, AIndexes[I]);
      try
        AIndex := TdxDBIndex(AIndexes.Objects[I]);
        AField := ADataSet.FieldByName('COLUMN_NAME');
        while not ADataSet.Eof do
        begin
          AIndex.Columns.Add(AField.AsString);
          ADataSet.Next;
        end;
        ATable.Indexes.Add(AIndex);
      finally
        ADataSet.Free;
      end;
    end;
  finally
    AIndexes.Free;
  end;
end;

procedure TdxFireDACConnectionVendor.GetPrimaryKey(ATable: TdxDBTable; const ASchemaName, ATableName: string);
var
  ADataSet: TDataSet;
  AColumns: TList<string>;
begin
  AColumns := TList<string>.Create;
  try
    ADataSet := GetSchema(TdxSchemaInfo.PrimaryKeyFields, '', ASchemaName, ATableName);
    try
      while not ADataSet.Eof do
      begin
        AColumns.Add(ADataSet.FieldByName('COLUMN_NAME').AsString);
        ADataSet.Next;
      end;
    finally
      ADataSet.Free;
    end;
    ATable.PrimaryKey := TdxDBPrimaryKey.Create(AColumns);
  finally
    AColumns.Free;
  end;
end;

function TdxFireDACConnectionVendor.GetSchema(ASchemaInfo: TdxSchemaInfo; const ACatalogName, ASchemaName, ATableName,
  AObjectName: string): TDataSet;
var
  AMetaInfo: TFDMetaInfoQuery;
begin
  AMetaInfo := TFDMetaInfoQuery.Create(nil);
  AMetaInfo.Connection := Connection;
  AMetaInfo.MetaInfoKind := SchemaInfo[ASchemaInfo];
  case ASchemaInfo of
    TdxSchemaInfo.Tables:
      AMetaInfo.TableKinds := [TFDPhysTableKind.tkTable];
    TdxSchemaInfo.Views:
      AMetaInfo.TableKinds := [TFDPhysTableKind.tkView];
    else
      AMetaInfo.TableKinds := [TFDPhysTableKind.tkTable, TFDPhysTableKind.tkView];
  end;
  case ASchemaInfo of
    TdxSchemaInfo.IndexFields, TdxSchemaInfo.PrimaryKeyFields, TdxSchemaInfo.ForeignKeyFields:
      begin
        AMetaInfo.ObjectName := AObjectName;
        AMetaInfo.BaseObjectName := ATableName;
      end;
    TdxSchemaInfo.Columns, TdxSchemaInfo.Indexes, TdxSchemaInfo.ForeignKeys:
      AMetaInfo.ObjectName := ATableName;
    else
      AMetaInfo.Wildcard := ATableName;
  end;

  AMetaInfo.ObjectScopes := [TFDPhysObjectScope.osMy];
  AMetaInfo.CatalogName := ACatalogName;
  AMetaInfo.SchemaName := ASchemaName;

  AMetaInfo.Open;
  Result := AMetaInfo;
end;

procedure TdxFireDACConnectionVendor.GetTableSchema(ATable: TdxDBTable; ACheckIndexes, ACheckForeignKeys: Boolean);
var
  ASchemaName, ATableName: string;
begin
  ASchemaName := ProviderSQL.ComposeSafeSchemaName(ATable.Name);
  ATableName := ProviderSQL.ComposeSafeTableName(ATable.Name);
  GetColumns(ATable, ASchemaName, ATableName);
  GetPrimaryKey(ATable, ASchemaName, ATableName);
  if ACheckIndexes then
    GetIndexes(ATable, ASchemaName, ATableName);
  if ACheckForeignKeys then
    GetForeignKeys(ATable, ASchemaName, ATableName);
end;

class function TdxFireDACConnectionVendor.GetType(AType: TFDDataType): TdxDBColumnType;
begin
  case AType of
    dtBoolean:
      Result := TdxDBColumnType.Boolean;
    dtSByte:
      Result := TdxDBColumnType.SByte;
    dtInt16:
      Result := TdxDBColumnType.Int16;
    dtInt32:
      Result := TdxDBColumnType.Int32;
    dtInt64:
      Result := TdxDBColumnType.Int64;
    dtByte:
      Result := TdxDBColumnType.Byte;
    dtUInt16:
      Result := TdxDBColumnType.UInt16;
    dtUInt32:
      Result := TdxDBColumnType.UInt32;
    dtUInt64:
      Result := TdxDBColumnType.UInt64;
    dtSingle:
      Result := TdxDBColumnType.Single;
    dtDouble, dtExtended:
      Result := TdxDBColumnType.Double;
    dtCurrency, dtBCD, dtFmtBCD:
      Result := TdxDBColumnType.Decimal;
    dtDateTime, dtTime, dtDate, dtDateTimeStamp:
      Result := TdxDBColumnType.DateTime;
    dtAnsiString, dtWideString, dtByteString,
    dtMemo, dtWideMemo, dtXML,
    dtHMemo, dtWideHMemo:
      Result := TdxDBColumnType.String;
    dtBlob, dtHBlob:
      Result := TdxDBColumnType.ByteArray;
    dtGUID:
      Result := TdxDBColumnType.Guid;
    else
      Result := TdxDBColumnType.Unknown;
  end;
end;

function TdxFireDACConnectionVendor.ExecuteScalar(AConnection: TCustomConnection; const ACommandText: string): Variant;
begin
  Result := (AConnection as TFDConnection).ExecSQLScalar(ACommandText);
end;

function TdxFireDACConnectionVendor.Execute(const ACommandText: string): Integer;
begin
  Result := Connection.ExecSQL(ACommandText);
end;

{ TdxEMFFireDACDataProvider }

function TdxEMFFireDACDataProvider.GetConnection: TFDConnection;
begin
  Result := TFDConnection(inherited Connection);
end;

function TdxEMFFireDACDataProvider.GetConnectionVendor: TdxFireDACConnectionVendor;
begin
  Result := TdxFireDACConnectionVendor(inherited ConnectionVendor);
end;


procedure TdxEMFFireDACDataProvider.SetConnection(const Value: TFDConnection);
begin
  inherited Connection := Value;
end;

procedure TdxEMFFireDACDataProvider.CheckConnectAndCreate;
begin
  if Connection = nil then
    raise EArgumentNilException.CreateFmt(sdxConnectionIsNull, [Name]);
  if Connection.Connected then
    Exit;
  try
    Connection.Open;
  except
    on E: Exception do
    begin
      if CanCreateDatabase and CreateDatabase then
      begin
        Connection.Open;
      end
      else
        raise EdxUnableToOpenDatabaseException.Create(E.Message);
    end;
  end;
end;

function TdxEMFFireDACDataProvider.CreateConnectionVendor: TdxCustomConnectionVendor;
begin
  Result := TdxFireDACConnectionVendor.Create(Connection);
end;

function TdxEMFFireDACDataProvider.CreateDatabase: Boolean;
var
  ADatabaseName: string;
begin
  CheckProvider;
  ADatabaseName := ConnectionVendor.GetDatabaseName(SQLConnectionProvider);
  try
    SQLConnectionProvider.CreateDatabase(ADatabaseName, []);
    Result := True;
  except
    Result := False;
  end;
end;

function TdxEMFFireDACDataProvider.CreateLoader(ASession: TdxEMFCustomSession;
  AConnectionVendor: TdxCustomConnectionVendor; AQuery: TdxQuery; AEntityInfo: TdxEntityInfo;
  APredicate: TdxPredicate = nil): TdxDataLoader;
var
  ALoader: TdxFireDACLoader;
begin
  CheckProvider;
  Assert(AEntityInfo <> nil);
  ALoader := TdxFireDACLoader.Create(ASession, AConnectionVendor.CreateDataSet(AQuery));
  ALoader.EntityInfo := AEntityInfo;
  ALoader.Predicate := APredicate;
  Result := ALoader;
end;

end.
