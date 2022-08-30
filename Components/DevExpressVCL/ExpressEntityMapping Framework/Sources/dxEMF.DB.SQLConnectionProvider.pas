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

unit dxEMF.DB.SQLConnectionProvider;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, DB, Rtti,
  dxCoreClasses,
  dxEMF.Utils,
  dxEMF.Types,
  dxEMF.DB.Model,
  dxEMF.DB.Query,
  dxEMF.DB.Criteria,
  dxEMF.Utils.Evaluator,
  dxEMF.DB.AliasExpander;

type

  TdxProcessParameter = reference to function (const ACriteriaOperator: IdxCriteriaOperator): string;
  TdxConnectionParameterValues = array[TdxConnectionParameter] of string;

  TdxConvertParameter = function(const AParameterValue: TValue): TValue of object;

  TdxSQLConnectionProvider = class;

  { TdxModificationResult }

  TdxModificationResult = class
  strict private
    FIdentities: TList<IdxParameterValue>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AIdentity: IdxParameterValue);
    property Identities: TList<IdxParameterValue> read FIdentities;
  end;

  { IdxDBTransaction }

  IdxDBTransaction = interface
  ['{3B9B9CC5-33BC-4643-94CD-24EFE3D9CE94}']
    procedure Commit;
    procedure Rollback;
  end;

  { TdxCustomDBCommand }

  TdxCustomDBCommand = class
  strict private
    FHash: Integer;
    FQuery: TdxQuery;
    FCommandText: string;
    procedure SetQuery(const Value: TdxQuery);
  protected
    procedure SetCommandText(const Value: string); virtual;
    procedure PrepareParameters(AConvertParameter: TdxConvertParameter); virtual; abstract;
    property Hash: Integer read FHash;
    property Query: TdxQuery read FQuery write SetQuery;
  public
    constructor Create(AQuery: TdxQuery);
    function ExecuteNonQuery: Integer; virtual; abstract;
    function GetScalar: Variant; virtual; abstract;
    property CommandText: string read FCommandText write SetCommandText;
  end;


  { TdxSchemaInfo }

  TdxSchemaInfo = (
    Tables,
    Views,
    Procedures,
    Columns,
    Indexes,
    IndexFields,
    PrimaryKeyFields,
    ForeignKeys,
    ForeignKeyFields
  );

  { TdxCustomConnectionVendor }

  TdxCustomConnectionVendorClass = class of TdxCustomConnectionVendor;

  TdxCustomConnectionVendor = class abstract(TcxIUnknownObject)
  strict private
    FConnection: TCustomConnection;
    FProviderSQL: TdxSQLConnectionProvider;
  protected
    class function GetCaseSensitiveFieldNames: Boolean; virtual;
    function CreateCommand(AQuery: TdxQuery): TdxCustomDBCommand; virtual; abstract;
    function GetConnectionString: string; virtual; abstract;
    function GetExpectedDBEngine: TdxDBEngine; virtual;
    function GetParsedConnectionString: TStrings; virtual;
    procedure SetConnection(const AValue: TCustomConnection); virtual;

    function SelectData(AOwner: TComponent; AQuery: TdxQuery): TDataSet; virtual; abstract;
    property ConnectionString: string read GetConnectionString;
    property ProviderSQL: TdxSQLConnectionProvider read FProviderSQL write FProviderSQL;
  public
    constructor Create(AConnection: TCustomConnection);
    function BeginTransaction: IdxDBTransaction; virtual; abstract;
    function CreateConnection(AProviderSQL: TdxSQLConnectionProvider;
      const ADatabaseName: string = ''): TCustomConnection; virtual; abstract;
    function CreateDataSet(AQuery: TdxQuery; AOwner: TComponent = nil): TDataSet; virtual; abstract;
    function Execute(const ACommandText: string): Integer; overload; virtual; abstract;
    function ExecuteScalar(const ACommandText: String): Variant; overload; virtual;
    function ExecuteScalar(AConnection: TCustomConnection; const ACommandText: String): Variant; overload; virtual; abstract;
    function GetDatabaseName(ASQLConnectionProvider: TdxSQLConnectionProvider): string; virtual;
    function GetSchema(ASchemaInfo: TdxSchemaInfo; const ACatalogName, ASchemaName, ATableName: string; const AObjectName: string = ''): TDataSet; virtual; abstract;
    procedure GetTableSchema(ATable: TdxDBTable; ACheckIndexes, ACheckForeignKeys: Boolean); virtual; abstract;
    procedure Open; virtual;
    class function GetConnectionType: TdxConnectionType; virtual;
    property Connection: TCustomConnection read FConnection;
  end;

  { TdxSQLConnectionProvider }

  TdxSQLConnectionProviderClass = class of TdxSQLConnectionProvider;

  TdxSQLConnectionProvider = class abstract
  public class var
    UseLegacyTimeSpanSupport: Boolean;
    DefaultStringSize: Integer;
  strict private type

    TDataSetHolder = TComponent;
    TCommandPool = class
    private
      const
        MaxPoolSize = 10;
    strict private
      FCommandPool: TObjectList<TdxCustomDBCommand>;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Add(ACommand: TdxCustomDBCommand);
      function Get(const ACommandText: string): TdxCustomDBCommand;
    end;

  protected type
    TTableFilter = reference to function(ATable: TdxDBTable): Boolean;

    TConnectionParameters = record
    public
      Values: TdxConnectionParameterValues;
      constructor Create(const AValues: array of string);
      class operator Implicit(const A: TConnectionParameters): TdxConnectionParameterValues;
    end;

  strict private class var
    FParametersAliasCache: TArray<string>;
  strict private
    FAutoCreate: TdxAutoCreateOption;
    FConnectionVendor: TdxCustomConnectionVendor;
    FCommandPool: TCommandPool;
    FDataSetHolder: TDataSetHolder;
    FExplicitTransaction: Boolean;
    FTransaction: IdxDBTransaction;
    procedure CreateCommandPool;
    procedure ReleaseCommandPool;
    function ConnectionBeginTransaction: IdxDBTransaction;
    function GetCanCreateDatabase: Boolean;
    function GetCanCreateSchema: Boolean;
    function FormatCustomFunction(const AOperands: TArray<string>): string;
    function DoInsertRecord(ARoot: TdxInsertStatement; AIdentities: TdxTaggedParameterHolder): IdxParameterValue;
    function DoUpdateRecord(ARoot: TdxUpdateStatement; AIdentities: TdxTaggedParameterHolder): IdxParameterValue;
    function DoDeleteRecord(ARoot: TdxDeleteStatement; AIdentities: TdxTaggedParameterHolder): IdxParameterValue;
    function InternalExecSQL(ACommand: TdxCustomDBCommand): Integer; inline;
    function ColumnExists(ATable: TdxDBTable; AColumn: TdxDBColumn): Boolean;
    function AreColumnsEqual(AFirst, ASecond: TList<string>): Boolean;
    function ForeignKeyExists(ATable: TdxDBTable; AForeignKey: TdxDBForeignKey): Boolean;
    function IndexExists(ATable: TdxDBTable; AIndex: TdxDBIndex): Boolean;
    function NeedCreateIndex(ATable: TdxDBTable; AIndex: TdxDBIndex): Boolean;
  protected
    class constructor Create;

    procedure BeginTransactionCore; virtual;
    procedure CommitTransactionCore; virtual;
    procedure RollbackTransactionCore; virtual;

    function ConvertToDBParameter(const AParameterValue: TValue): TValue; virtual;
    function GetCommandFromPool(AQuery: TdxQuery): TdxCustomDBCommand;
    procedure ReleasePooledCommand(ACommand: TdxCustomDBCommand);
    function GetScalar(AQuery: TdxQuery): Variant;
    function Execute(AQuery: TdxQuery): Integer; overload;
    procedure Execute(AQueries: TdxQueryCollection); overload; inline;
    procedure ExecuteSQLSchemaUpdate(const AObjectTypeName, AObjectName, AParentObjectName, ACommandText: string);
    function CreateIndexTemplate: string; virtual;
    function CreateForeignKeyTemplate: string; virtual;
    function CollectTablesToCreate(const ATables: TArray<TdxDBTable>): TArray<TdxDBTable>; virtual; abstract;
    function GetBraceJoin: Boolean; virtual;
    class function GetDBNameHashString(const ADBName: string): string; static;
    function GetForeignKeyName(AForeignKey: TdxDBForeignKey; ATable: TdxDBTable): string; virtual;
    function GetSafeNameConstraintMaxLength: Integer; virtual;
    function GetSafeNameColumnMaxLength: Integer; virtual;
    function GetSafeNameRoot(const AOriginalName: string): string; virtual;
    function GetSafeNameTableMaxLength: Integer; virtual; abstract;
    class function GetSafeObjectName(const AOriginalName, APatchedName: string; AMaxLength: Integer): string; static;
    function GetIndexName(AIndex: TdxDBIndex; ATable: TdxDBTable): string; virtual;
    function GetSQLCreateColumnType(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
    function GetSQLCreateColumnTypeForBoolean(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual; abstract;
    function GetSQLCreateColumnTypeForByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual; abstract;
    function GetSQLCreateColumnTypeForSByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual; abstract;
    function GetSQLCreateColumnTypeForChar(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual; abstract;
    function GetSQLCreateColumnTypeForDecimal(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual; abstract;
    function GetSQLCreateColumnTypeForDouble(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual; abstract;
    function GetSQLCreateColumnTypeForSingle(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual; abstract;
    function GetSQLCreateColumnTypeForInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual; abstract;
    function GetSQLCreateColumnTypeForUInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual; abstract;
    function GetSQLCreateColumnTypeForInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual; abstract;
    function GetSQLCreateColumnTypeForUInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual; abstract;
    function GetSQLCreateColumnTypeForInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual; abstract;
    function GetSQLCreateColumnTypeForUInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual; abstract;
    function GetSQLCreateColumnTypeForString(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual; abstract;
    function GetSQLCreateColumnTypeForDateTime(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual; abstract;
    function GetSQLCreateColumnTypeForGuid(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual; abstract;
    function GetSQLCreateColumnTypeForTimeSpan(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual;
    function GetSQLCreateColumnTypeForByteArray(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual; abstract;
    function GetPrimaryKeyName(APrimaryKey: TdxDBPrimaryKey; ATable: TdxDBTable): string; virtual;
    function GetIdentity(ASQL: TdxQuery): Int64; overload; virtual;
    function GetIdentity(ARoot: TdxInsertStatement; AIdentities: TdxTaggedParameterHolder): Int64; overload; virtual;
    class function GetParamAlias(AIndex: Integer): string; static;
    class function GetDBTypes: TDictionary<string, TdxDBColumnType>; virtual;
    class function GetSafeNameDefault(const AOriginalName: string): string; static;
    class function IsSingleColumnPKColumn(ATable: TdxDBTable; AColumn: TdxDBColumn): Boolean; static;
    function ProcessModifyData(const ADMLStatements: TArray<TdxModificationStatement>): TdxModificationResult; virtual;
    function UpdateRecord(ARoot: TdxModificationStatement; AIdentities: TdxTaggedParameterHolder): IdxParameterValue;
    function NeedsIndexForForeignKey: Boolean; virtual;

    function GetMaximumStringSize: Integer; virtual; abstract;

    property ExplicitTransaction: Boolean read FExplicitTransaction;
    property DBTypes: TDictionary<string, TdxDBColumnType> read GetDBTypes;
    property ConnectionVendor: TdxCustomConnectionVendor read FConnectionVendor;
    property MaximumStringSize: Integer read GetMaximumStringSize;
    property Transaction: IdxDBTransaction read FTransaction;
  public
    constructor Create(AConnectionVendor: TdxCustomConnectionVendor; AAutoCreateOption: TdxAutoCreateOption); virtual;
    destructor Destroy; override;

    class function GetDBEngine: TdxDBEngine; virtual;

    procedure CreateDatabase(const ADatabaseName: string; const AValues: array of string); virtual;
    function CreateCommand(AQuery: TdxQuery): TdxCustomDBCommand; virtual;
    procedure CreateTable(ATable: TdxDBTable); virtual;
    procedure CreatePrimaryKey(ATable: TdxDBTable); virtual;
    procedure CreateColumn(ATable: TdxDBTable; AColumn: TdxDBColumn); virtual;
    procedure CreateIndex(ATable: TdxDBTable; AIndex: TdxDBIndex); virtual;
    procedure CreateForeignKey(ATable: TdxDBTable; AForeignKey: TdxDBForeignKey); virtual;
    function ComposeSafeColumnName(const AColumnName: string): string; virtual;
    function ComposeSafeConstraintName(const AConstraintName: string): string; virtual;
    function ComposeSafeSchemaName(const ATableName: string): string; virtual;
    function ComposeSafeTableName(const ATableName: string): string; virtual;
    function GetSQLCreateColumnFullAttributes(ATable: TdxDBTable; AColumn: TdxDBColumn): string; virtual; abstract;
    function GetConnectionParameter(const AConnectionType: TdxConnectionType; AParameter: TdxConnectionParameter): string; virtual; abstract;
    function GetParameterName(const AParameter: IdxOperandValue; AIndex: Integer; var ACreateParameter: Boolean): string; virtual; abstract;
    procedure GetTableSchema(ATable: TdxDBTable; ACheckIndexes, ACheckForeignKeys: Boolean); virtual; abstract;
    function GetStorageTableNames(AIncludeViews: Boolean): TArray<string>; virtual; abstract;
    function ProcessUpdateSchema(ASkipIfFirstTableNotExists: Boolean; const ATables: TArray<TdxDBTable>): TdxUpdateSchemaResult; virtual;
    function SelectData(AQuery: TdxQuery): TDataSet; overload;
    function SelectData(ASelect: TdxSelectStatement): TDataSet; overload;
    function SupportsNamedParameters: Boolean; virtual;
    function SupportsNativeOuterApply: Boolean; virtual;
    function SupportsNativeSkipTake: Boolean; virtual;
    procedure UpdateSchema(const ATables: array of TdxDBTable);

    function FormatBinary(AOperatorType: TdxBinaryOperatorType; const ALeftOperand, ARightOperand: string): string; virtual;
    function FormatColumn(const AColumnName: string): string; overload; virtual; abstract;
    function FormatColumn(const AColumnName, ATableAlias: string): string; overload; virtual; abstract;
    function FormatColumnSafe(const AColumnName: string): string;
    function FormatConstraint(const AConstraintName: string): string; virtual; abstract;
    function FormatConstraintSafe(const AConstraintName: string): string;
    function FormatFunction(AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<string>): string; overload; virtual;
    function FormatFunction(AProcessParameter: TdxProcessParameter; AOperatorType: TdxFunctionOperatorType;
      const AOperands: TArray<TValue>): string; overload; virtual;
    function FormatTable(const ASchemaName, ATableName: string): string; overload; virtual; abstract;
    function FormatTable(const ASchemaName, ATableName, ATableAlias: string): string; overload; virtual; abstract;
    function FormatTableSafe(ATable: TdxDBTable): string;
    function FormatUnary(AOperatorType: TdxUnaryOperatorType; const AOperand: string): string; virtual;

    function FormatCreateTable(const ATableName, AColumns: string): string; virtual;
    function FormatSelect(const ASelectClause, AFromClause, AWhereClause, AOrderByClause, AGroupByClause, AHavingClause: string;
      ATopSelectedRecords: Integer): string; overload; virtual;
    function FormatSelect(const ASelectClause, AFromClause, AWhereClause, AOrderByClause, AGroupByClause, AHavingClause: string;
      ASkipSelectedRecords: Integer; ATopSelectedRecords: Integer): string; overload; virtual;
    function FormatOrder(const ASortProperty: string; ASortDirection: TdxSortDirection): string; virtual;
    function FormatInsertDefaultValues(ATable: TdxDBTable): string; virtual; abstract;
    function FormatInsert(const ATableName, AFieldNames, AValuesClause: string): string; virtual; abstract;
    function FormatUpdate(const ATableName, ASetClause, AWhereClause: string): string; virtual; abstract;
    function FormatDelete(const ATableName, AWhereClause: string): string; virtual; abstract;

    function FormatOuterApply(const ACommandText, AAlias: string): string; virtual;

    procedure BeginTransaction;
    procedure CommitTransaction;
    procedure RollbackTransaction;
    function ModifyData(const ADMLStatements: TArray<TdxModificationStatement>): TdxModificationResult; overload;
    function ModifyData(const ADMLStatements: array of TdxModificationStatement): TdxModificationResult; overload;

    property BraceJoin: Boolean read GetBraceJoin;

    property CanCreateDatabase: Boolean read GetCanCreateDatabase;
    property CanCreateSchema: Boolean read GetCanCreateSchema;
    property AutoCreate: TdxAutoCreateOption read FAutoCreate;
  end;

  { TdxSQLConnectionProviderFactory }

  TdxSQLConnectionProviderFactory = class sealed
  strict private class var
    FProviderSQLs: TList<TdxSQLConnectionProviderClass>;
    class destructor Destroy;
  public
    class procedure Register(AClass: TdxSQLConnectionProviderClass); static;
    class procedure Unregister(AClass: TdxSQLConnectionProviderClass); static;

    class function HasProvider(const ADBEngine: TdxDBEngine): Boolean; static;
    class function GetProviderSQL(const ADBEngine: TdxDBEngine): TdxSQLConnectionProviderClass; static;
    class procedure PopulateDBEngines(const AList: TList<TdxDBEngine>); static;
  end;

implementation

uses
  Character, StrUtils,
  dxCore, dxStringHelper,
  dxEMF.DB.Utils,
  dxEMF.Utils.Exceptions,
  dxEMF.DB.SQLGenerator,
  dxEMF.Strs;

{ TdxModificationResult }

constructor TdxModificationResult.Create;
begin
  inherited Create;
  FIdentities := TList<IdxParameterValue>.Create;
end;

destructor TdxModificationResult.Destroy;
begin
  FreeAndNil(FIdentities);
  inherited Destroy;
end;

procedure TdxModificationResult.Add(const AIdentity: IdxParameterValue);
begin
  FIdentities.Add(AIdentity);
end;

{ TdxCustomDBCommand }

constructor TdxCustomDBCommand.Create(AQuery: TdxQuery);
begin
  inherited Create;
  FQuery := AQuery;
end;

procedure TdxCustomDBCommand.SetCommandText(const Value: string);
begin
  FCommandText := Value;
end;

procedure TdxCustomDBCommand.SetQuery(const Value: TdxQuery);
begin
  FQuery := Value;
  if FQuery <> nil then
    CommandText := FQuery.SQLText;
end;

{ TdxCustomConnectionVendor }

constructor TdxCustomConnectionVendor.Create(AConnection: TCustomConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;

class function TdxCustomConnectionVendor.GetConnectionType: TdxConnectionType;
begin
  Result := TdxConnectionTypes.Unknown;
end;

class function TdxCustomConnectionVendor.GetCaseSensitiveFieldNames: Boolean;
begin
  Result := False;
end;

function TdxCustomConnectionVendor.GetExpectedDBEngine: TdxDBEngine;
begin
  Result := TdxDBEngines.Unknown;
end;

function TdxCustomConnectionVendor.GetParsedConnectionString: TStrings;
var
  AListParameters: TArray<string>;
  APair: TArray<string>;
  AParam: string;
begin
  Result := TStringList.Create;
  AListParameters := {$IFDEF DELPHIXE3}ConnectionString.Split([';']){$ELSE}TdxStringHelper.Split(ConnectionString, [';']){$ENDIF};
  for AParam in AListParameters do
  begin
    APair := {$IFDEF DELPHIXE3}AParam.Split(['=']){$ELSE}TdxStringHelper.Split(AParam, ['=']){$ENDIF};
    if Length(APair) = 2 then
      Result.Values[LowerCase(APair[0])] := APair[1];
  end;
end;

procedure TdxCustomConnectionVendor.Open;
begin
  Connection.Open;
end;

function TdxCustomConnectionVendor.ExecuteScalar(const ACommandText: String): Variant;
begin
  Result := ExecuteScalar(Self.Connection, ACommandText);
end;

procedure TdxCustomConnectionVendor.SetConnection(const AValue: TCustomConnection);
begin
  FConnection := AValue;
end;

function TdxCustomConnectionVendor.GetDatabaseName(ASQLConnectionProvider: TdxSQLConnectionProvider): string;
var
  ADatabaseParamName: string;
  AConnectionString: TStrings;
begin
  ADatabaseParamName := ASQLConnectionProvider.GetConnectionParameter(GetConnectionType, TdxConnectionParameter.Database);
  AConnectionString := GetParsedConnectionString;
  try
    Result := AConnectionString.Values[ADatabaseParamName];
  finally
    AConnectionString.Free;
  end;
end;

{ TdxSQLConnectionProvider.TCommandPool }

constructor TdxSQLConnectionProvider.TCommandPool.Create;
begin
  inherited Create;
  FCommandPool := TObjectList<TdxCustomDBCommand>.Create;
  FCommandPool.Capacity := MaxPoolSize + 1;
end;

destructor TdxSQLConnectionProvider.TCommandPool.Destroy;
begin
  FreeAndNil(FCommandPool);
  inherited Destroy;
end;

procedure TdxSQLConnectionProvider.TCommandPool.Add(ACommand: TdxCustomDBCommand);
begin
  FCommandPool.Add(ACommand);
  if FCommandPool.Count > MaxPoolSize then
    FCommandPool.Delete(0);
end;

function TdxSQLConnectionProvider.TCommandPool.Get(const ACommandText: string): TdxCustomDBCommand;
var
  I: Integer;
begin
  for I := 0 to FCommandPool.Count - 1 do
    if FCommandPool[I].CommandText = ACommandText then
    begin
      Result := FCommandPool[I];
      FCommandPool.Extract(Result);
      Exit;
    end;
  Result := nil;
end;

{ TdxSQLConnectionProvider.TConnectionParameters }

constructor TdxSQLConnectionProvider.TConnectionParameters.Create(const AValues: array of string);
var
  J: TdxConnectionParameter;
  I: Integer;
begin
  Assert(Length(Values) = Length(AValues));
  I := 0;
  for J := Low(TdxConnectionParameter) to High(TdxConnectionParameter) do
  begin
    Values[J] := AValues[I];
    Inc(I);
  end;
end;

class operator TdxSQLConnectionProvider.TConnectionParameters.Implicit(
  const A: TConnectionParameters): TdxConnectionParameterValues;
begin
  Result := A.Values;
end;

{ TdxSQLConnectionProvider }

constructor TdxSQLConnectionProvider.Create(AConnectionVendor: TdxCustomConnectionVendor;
  AAutoCreateOption: TdxAutoCreateOption);
begin
  inherited Create;
  Assert(AConnectionVendor <> nil);
  FAutoCreate := AAutoCreateOption;
  FConnectionVendor := AConnectionVendor;
  FConnectionVendor.ProviderSQL := Self;
end;

destructor TdxSQLConnectionProvider.Destroy;
begin
  ReleaseCommandPool;
  FreeAndNil(FDataSetHolder);
  inherited Destroy;
end;

class constructor TdxSQLConnectionProvider.Create;
begin
  DefaultStringSize := 100;
end;

function TdxSQLConnectionProvider.InternalExecSQL(ACommand: TdxCustomDBCommand): Integer;
begin
  Result := ACommand.ExecuteNonQuery;
end;

function TdxSQLConnectionProvider.Execute(AQuery: TdxQuery): Integer;
var
  ACommand: TdxCustomDBCommand;
begin
  ACommand := GetCommandFromPool(AQuery);
  try
        Result := InternalExecSQL(ACommand);
  finally
    ReleasePooledCommand(ACommand);
  end;
end;

procedure TdxSQLConnectionProvider.Execute(AQueries: TdxQueryCollection);
var
  AQuery: TdxQuery;
begin
  for AQuery in AQueries do
    Execute(AQuery);
end;

procedure TdxSQLConnectionProvider.BeginTransaction;
begin
  if not FExplicitTransaction then
    BeginTransactionCore;
end;

procedure TdxSQLConnectionProvider.BeginTransactionCore;
begin
    FTransaction := ConnectionBeginTransaction;
    CreateCommandPool;
end;

procedure TdxSQLConnectionProvider.CommitTransaction;
begin
  if not FExplicitTransaction then
    CommitTransactionCore;
end;

procedure TdxSQLConnectionProvider.CommitTransactionCore;
begin
  ReleaseCommandPool;
  if FTransaction <> nil then
    FTransaction.Commit;
  FTransaction := nil;
end;

function TdxSQLConnectionProvider.ComposeSafeColumnName(const AColumnName: string): string;
begin
  Result := GetSafeObjectName(AColumnName, GetSafeNameRoot(AColumnName), GetSafeNameColumnMaxLength);
end;

function TdxSQLConnectionProvider.ComposeSafeConstraintName(const AConstraintName: string): string;
begin
  Result := GetSafeObjectName(AConstraintName, GetSafeNameRoot(AConstraintName), GetSafeNameConstraintMaxLength);
end;

function TdxSQLConnectionProvider.ComposeSafeSchemaName(const ATableName: string): string;
var
  ADot, AStart: Integer;
  ASchemaName: string;
begin
  ADot := {$IFDEF DELPHIXE3}ATableName.LastIndexOf('.');{$ELSE}TdxStringHelper.LastIndexOf(ATableName, '.');{$ENDIF}
  if ADot > 0 then
  begin
    AStart := {$IFDEF DELPHIXE3}ATableName.IndexOf('.');{$ELSE}TdxStringHelper.IndexOf(ATableName, '.');{$ENDIF}
    if (AStart < 0) or (ADot = AStart) then
      AStart := 0
    else
      Inc(AStart);
    {$IFDEF DELPHIXE3}
    ASchemaName := ATableName.Substring(AStart, ADot - AStart);
    {$ELSE}
    ASchemaName := TdxStringHelper.Substring(ATableName, AStart, ADot - AStart);
    {$ENDIF}
    Result := GetSafeObjectName(ASchemaName, GetSafeNameRoot(ASchemaName), GetSafeNameTableMaxLength);
  end
  else
    Result := '';
end;

function TdxSQLConnectionProvider.ComposeSafeTableName(const ATableName: string): string;
var
  ADot: Integer;
begin
  ADot := {$IFDEF DELPHIXE3}ATableName.LastIndexOf('.');{$ELSE}TdxStringHelper.LastIndexOf(ATableName, '.');{$ENDIF}
  if ADot > 0 then
    {$IFDEF DELPHIXE3}
    Result := ATableName.Substring(ADot + 1, Length(ATableName))
    {$ELSE}
    Result := TdxStringHelper.Substring(ATableName, ADot + 1, Length(ATableName))
    {$ENDIF}
  else
    Result := ATableName;
  Result := GetSafeObjectName(Result, GetSafeNameRoot(Result), GetSafeNameTableMaxLength);
end;

function TdxSQLConnectionProvider.ConnectionBeginTransaction: IdxDBTransaction;
begin
  Result := ConnectionVendor.BeginTransaction;
end;

function TdxSQLConnectionProvider.ConvertToDBParameter(const AParameterValue: TValue): TValue;
begin
  Result := AParameterValue;
end;

procedure TdxSQLConnectionProvider.CreateTable(ATable: TdxDBTable);
var
  AColumns: TStringBuilder;
  AColumn: TdxDBColumn;
begin
  AColumns := TStringBuilder.Create;
  try
    for AColumn in ATable.Columns do
    begin
      if AColumns.Length > 0 then
        AColumns.Append(', ');
      AColumns.Append(FormatColumnSafe(AColumn.Name)).
        Append(' ').
        Append(GetSQLCreateColumnFullAttributes(ATable, AColumn));
    end;
    ExecuteSQLSchemaUpdate('Table', ATable.Name, '', FormatCreateTable(FormatTableSafe(ATable), AColumns.ToString));
  finally
    AColumns.Free;
  end;
end;

function TdxSQLConnectionProvider.DoDeleteRecord(ARoot: TdxDeleteStatement;
  AIdentities: TdxTaggedParameterHolder): IdxParameterValue;
var
  ACount: Integer;
  ASQLGenerator: TdxDeleteSQLGenerator;
  AQuery: TdxQuery;
begin
  ASQLGenerator := TdxDeleteSQLGenerator.Create(Self, AIdentities);
  try
    AQuery := ASQLGenerator.GenerateSQL(ARoot);
    try
      ACount := Execute(AQuery);
    finally
      AQuery.Free;
    end;
  finally
    ASQLGenerator.Free;
  end;
  if (ARoot.AffectedRecordCount <> 0) and (ARoot.AffectedRecordCount <> ACount) then
  begin
    RollbackTransaction;
    raise EdxLockingException.Create('');
  end;
  Result := nil;
end;

function TdxSQLConnectionProvider.DoInsertRecord(ARoot: TdxInsertStatement;
  AIdentities: TdxTaggedParameterHolder): IdxParameterValue;
var
  ASQLGenerator: TdxInsertSQLGenerator;
  AQuery: TdxQuery;
begin
  if ARoot.IdentityParameter = nil then
  begin
    ASQLGenerator := TdxInsertSQLGenerator.Create(Self, AIdentities);
    try
      AQuery := ASQLGenerator.GenerateSQL(ARoot);
      try
        Execute(AQuery);
      finally
        AQuery.Free;
      end;
    finally
      ASQLGenerator.Free;
    end;
    Result := nil;
  end
  else
  begin
    AIdentities.ConsolidateIdentity(ARoot.IdentityParameter);
    case ARoot.IdentityColumnType of
      TdxDBColumnType.Int32:
        ARoot.IdentityParameter.Value := Integer(GetIdentity(ARoot, AIdentities));
      TdxDBColumnType.Int64:
        ARoot.IdentityParameter.Value := GetIdentity(ARoot, AIdentities);
      else
        raise ENotSupportedException.CreateFmt(sdxAutoIncrementedKeyNotSupported,
          [TdxFormatterHelper.GetName(ARoot.IdentityColumnType), ClassName]);
    end;
    Result := ARoot.IdentityParameter;
  end;
end;

function TdxSQLConnectionProvider.DoUpdateRecord(ARoot: TdxUpdateStatement;
  AIdentities: TdxTaggedParameterHolder): IdxParameterValue;
var
  ASQLGenerator: TdxUpdateSQLGenerator;
  AQuery: TdxQuery;
  ACount: Integer;
begin
  ASQLGenerator := TdxUpdateSQLGenerator.Create(Self, AIdentities);
  try
    AQuery := ASQLGenerator.GenerateSQL(ARoot);
    try
      Result := nil;
      if AQuery.SQLText <> '' then
      begin
        ACount := Execute(AQuery);
        if (ARoot.AffectedRecordCount <> 0) and (ARoot.AffectedRecordCount <> ACount) then
        begin
          RollbackTransaction;
          raise EdxLockingException.Create('');
        end;
      end;
    finally
      AQuery.Free;
    end;
  finally
    ASQLGenerator.Free;
  end;
end;

procedure TdxSQLConnectionProvider.ExecuteSQLSchemaUpdate(const AObjectTypeName, AObjectName, AParentObjectName,
  ACommandText: string);
begin
  if not CanCreateSchema then
    raise EdxSchemaCorrectionNeededException.Create(ACommandText);
  try
    ConnectionVendor.Execute(ACommandText);
  except
    on E: Exception do
    begin
      raise EdxUnableToCreateDBObjectException.Create(AObjectTypeName, AObjectName, AParentObjectName, E.Message);
    end
  end;
end;


procedure TdxSQLConnectionProvider.CreatePrimaryKey(ATable: TdxDBTable);
var
  AFormattedColumns: string;
begin
  Assert(ATable.PrimaryKey <> nil);
  AFormattedColumns := TdxFormatterHelper.Concat(ATable.PrimaryKey.Columns, FormatColumnSafe);
  ExecuteSQLSchemaUpdate('PrimaryKey', GetPrimaryKeyName(ATable.PrimaryKey, ATable), ATable.Name,
    Format('alter table %s add constraint %s primary key (%s)',
      [FormatTableSafe(ATable), FormatConstraintSafe(GetPrimaryKeyName(ATable.PrimaryKey, ATable)), AFormattedColumns]));
end;

procedure TdxSQLConnectionProvider.CreateColumn(ATable: TdxDBTable; AColumn: TdxDBColumn);
begin
  ExecuteSQLSchemaUpdate('Column', AColumn.Name, ATable.Name,
    Format('alter table %s add %s %s',
      [FormatTableSafe(ATable), FormatColumnSafe(AColumn.Name), GetSQLCreateColumnFullAttributes(ATable, AColumn)]));
end;

function TdxSQLConnectionProvider.CreateCommand(AQuery: TdxQuery): TdxCustomDBCommand;
begin
  Result := FConnectionVendor.CreateCommand(AQuery);
  Result.PrepareParameters(ConvertToDBParameter);
end;

procedure TdxSQLConnectionProvider.CreateCommandPool;
begin
  FCommandPool := TCommandPool.Create;
end;

class function TdxSQLConnectionProvider.GetDBEngine: TdxDBEngine;
begin
  Result := TdxDBEngines.Unknown;
end;

procedure TdxSQLConnectionProvider.CreateDatabase(const ADatabaseName: string; const AValues: array of string);
begin
  ExecuteSQLSchemaUpdate('Database', ADatabaseName, '', Format('CREATE Database [%s]', [ADatabaseName]));
end;

procedure TdxSQLConnectionProvider.CreateIndex(ATable: TdxDBTable; AIndex: TdxDBIndex);
var
  AFormattedColumns: string;
begin
  AFormattedColumns := TdxFormatterHelper.Concat(AIndex.Columns, FormatColumnSafe);
  ExecuteSQLSchemaUpdate('Index', GetIndexName(AIndex, ATable), ATable.Name,
    Format(CreateIndexTemplate, [IfThen(AIndex.IsUnique, 'unique'), FormatConstraintSafe(GetIndexName(AIndex, ATable)),
      FormatTableSafe(ATable), AFormattedColumns]));
end;

function TdxSQLConnectionProvider.CreateIndexTemplate: string;
begin
  Result := 'create %s index %s on %s(%s)';
end;

procedure TdxSQLConnectionProvider.CreateForeignKey(ATable: TdxDBTable; AForeignKey: TdxDBForeignKey);
var
  AFormattedColumns, AFormattedRefColumns: string;
begin
  AFormattedColumns := TdxFormatterHelper.Concat(AForeignKey.Columns, FormatColumnSafe);
  AFormattedRefColumns := TdxFormatterHelper.Concat(AForeignKey.PrimaryKeyTableKeyColumns, FormatColumnSafe);
  ExecuteSQLSchemaUpdate('ForeignKey', GetForeignKeyName(AForeignKey, ATable), ATable.Name,
    Format(CreateForeignKeyTemplate,
     [FormatTableSafe(ATable), FormatConstraintSafe(GetForeignKeyName(AForeignKey, ATable)), AFormattedColumns,
     FormatTable(ComposeSafeSchemaName(AForeignKey.PrimaryKeyTable), ComposeSafeTableName(AForeignKey.PrimaryKeyTable)),
     AFormattedRefColumns]));
end;

function TdxSQLConnectionProvider.CreateForeignKeyTemplate: string;
begin
  Result := 'alter table %s add constraint %s foreign key (%s) references %s(%s)';
end;

function TdxSQLConnectionProvider.FormatBinary(AOperatorType: TdxBinaryOperatorType; const ALeftOperand,
  ARightOperand: string): string;
begin
  Result := TdxBaseFormatterHelper.DefaultFormatBinary(AOperatorType, ALeftOperand, ARightOperand);
end;

function TdxSQLConnectionProvider.FormatColumnSafe(const AColumnName: string): string;
begin
  Result := FormatColumn(ComposeSafeColumnName(AColumnName));
end;

function TdxSQLConnectionProvider.FormatConstraintSafe(const AConstraintName: string): string;
begin
  Result := FormatConstraint(ComposeSafeConstraintName(AConstraintName));
end;

function TdxSQLConnectionProvider.FormatCustomFunction(const AOperands: TArray<string>): string;
begin
  Result := '';
  NotImplemented;
end;

function TdxSQLConnectionProvider.FormatFunction(AProcessParameter: TdxProcessParameter;
  AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<TValue>): string;
var
  AStrings: TArray<string>;
  I: Integer;
begin
  SetLength(AStrings, Length(AOperands));
  for I := 0 to Length(AOperands) - 1 do
  begin
    if (I = 0) and (AOperatorType in [TdxFunctionOperatorType.Custom, TdxFunctionOperatorType.CustomNonDeterministic]) then
      AStrings[I] := AOperands[I].AsString
    else
      AStrings[I] := AProcessParameter(Safe<TdxCriteriaOperator>.Cast(AOperands[I].AsObject));
  end;
  Result := FormatFunction(AOperatorType, AStrings);
end;

function TdxSQLConnectionProvider.FormatFunction(AOperatorType: TdxFunctionOperatorType;
  const AOperands: TArray<string>): string;
begin
  if AOperatorType in [TdxFunctionOperatorType.Custom, TdxFunctionOperatorType.CustomNonDeterministic] then
    Result := FormatCustomFunction(AOperands)
  else
    Result := TdxBaseFormatterHelper.DefaultFormatFunction(AOperatorType, AOperands);
end;

function TdxSQLConnectionProvider.FormatOrder(const ASortProperty: string; ASortDirection: TdxSortDirection): string;
const
  SortDirectionName: array[TdxSortDirection] of string = ('asc', 'desc');
begin
  Result := Format('%s %s', [ASortProperty, SortDirectionName[ASortDirection]]);
end;

function TdxSQLConnectionProvider.FormatOuterApply(const ACommandText, AAlias: string): string;
begin
  Result := Format('outer apply (%s) %s', [ACommandText, AAlias]);
end;

function TdxSQLConnectionProvider.FormatCreateTable(const ATableName, AColumns: string): string;
begin
  Result := Format('create table %s (%s)', [ATableName, AColumns]);
end;

function TdxSQLConnectionProvider.FormatSelect(const ASelectClause, AFromClause, AWhereClause, AOrderByClause,
  AGroupByClause, AHavingClause: string; ATopSelectedRecords: Integer): string;
begin
  Result := FormatSelect(ASelectClause, AFromClause, AWhereClause, AOrderByClause, AGroupByClause, AHavingClause, 0,
    ATopSelectedRecords);
end;

function TdxSQLConnectionProvider.FormatSelect(const ASelectClause, AFromClause, AWhereClause, AOrderByClause,
  AGroupByClause, AHavingClause: string; ASkipSelectedRecords: Integer; ATopSelectedRecords: Integer): string;
begin
  if not SupportsNativeSkipTake then
    raise ENotSupportedException.Create(sdxConnectionProviderUnableToCreateDBObject);
  if (ASkipSelectedRecords <> 0) and (AOrderByClause = '') then
    raise EInvalidOperation.Create(sdxCannotSkipRecords);
  Result := '';
end;

function TdxSQLConnectionProvider.FormatTableSafe(ATable: TdxDBTable): string;
begin
  Result := FormatTable(ComposeSafeSchemaName(ATable.Name), ComposeSafeTableName(ATable.Name));
end;

function TdxSQLConnectionProvider.FormatUnary(AOperatorType: TdxUnaryOperatorType; const AOperand: string): string;
begin
  Result := TdxBaseFormatterHelper.DefaultFormatUnary(AOperatorType, AOperand);
end;

function TdxSQLConnectionProvider.GetBraceJoin: Boolean;
begin
  Result := True;
end;

function TdxSQLConnectionProvider.GetCanCreateDatabase: Boolean;
begin
  Result := AutoCreate = TdxAutoCreateOption.DatabaseAndSchema;
end;

function TdxSQLConnectionProvider.GetCanCreateSchema: Boolean;
begin
  Result := (AutoCreate = TdxAutoCreateOption.SchemaOnly) or CanCreateDatabase;
end;

function TdxSQLConnectionProvider.GetCommandFromPool(AQuery: TdxQuery): TdxCustomDBCommand;
begin
  if FCommandPool = nil then
    Result := nil
  else
    Result := FCommandPool.Get(AQuery.SQLText);
  if Result = nil then
    Exit(CreateCommand(AQuery));
  Result.Query := AQuery;
  Result.PrepareParameters(ConvertToDBParameter);
end;

class function TdxSQLConnectionProvider.GetDBNameHashString(const ADBName: string): string;
const
  HashLength = 32;
  HashShift = 7;
var
  AHash, AHashOverflow: Cardinal;
  ACh: Char;
begin
  AHash := 0;
  for ACh in ADBName do
  begin
    AHashOverflow := AHash shr (HashLength - HashShift);
    AHash := HashShift;
    AHash := AHash xor AHashOverflow;
    AHash := AHash xor Cardinal(ACh);
  end;
  Result := IntToHex(AHash, 8);
end;

class function TdxSQLConnectionProvider.GetDBTypes: TDictionary<string, TdxDBColumnType>;
begin
  Result := nil;
end;

function TdxSQLConnectionProvider.GetForeignKeyName(AForeignKey: TdxDBForeignKey; ATable: TdxDBTable): string;
var
  AResult: TStringBuilder;
  AColumn: string;
begin
  if AForeignKey.Name <> '' then
    Exit(AForeignKey.Name);
  AResult := TStringBuilder.Create;
  try
    AResult.Append('FK_').Append(ATable.Name).Append('_');
    for AColumn in AForeignKey.Columns do
      AResult.Append(AColumn);
    Result := AResult.ToString;
  finally
    AResult.Free;
  end;
end;

function TdxSQLConnectionProvider.GetIdentity(ASQL: TdxQuery): Int64;
begin
  Result := -1;
end;

function TdxSQLConnectionProvider.GetIdentity(ARoot: TdxInsertStatement; AIdentities: TdxTaggedParameterHolder): Int64;
var
  ASQLGenerator: TdxInsertSQLGenerator;
  AQuery: TdxQuery;
begin
  ASQLGenerator := TdxInsertSQLGenerator.Create(Self, AIdentities);
  try
    AQuery := ASQLGenerator.GenerateSQL(ARoot);
    try
      Result := GetIdentity(AQuery);
    finally
      AQuery.Free;
    end;
  finally
    ASQLGenerator.Free;
  end;
end;

function TdxSQLConnectionProvider.GetIndexName(AIndex: TdxDBIndex; ATable: TdxDBTable): string;
var
  AResult: TStringBuilder;
  ACol: string;
begin
  if AIndex.Name <> '' then
    Exit(AIndex.Name);
  AResult := TStringBuilder.Create;
  try
    AResult.Append('i');
    for ACol in AIndex.Columns do
      AResult.Append(ACol);
    AResult.Append('_').Append(ATable.Name);
    Result := AResult.ToString;
  finally
    AResult.Free;
  end;
end;

function TdxSQLConnectionProvider.SupportsNativeOuterApply: Boolean;
begin
  Result := False;
end;

function TdxSQLConnectionProvider.SupportsNativeSkipTake: Boolean;
begin
  Result := False;
end;

class function TdxSQLConnectionProvider.GetParamAlias(AIndex: Integer): string;
var
  ALength, I: Integer;
begin
  ALength := Length(FParametersAliasCache);
  if ALength <= AIndex then
  begin
    SetLength(FParametersAliasCache, AIndex + 4);
    for I := ALength to Length(FParametersAliasCache) - 1 do
      FParametersAliasCache[I] := 'p' + IntToStr(I);
  end;
  Result := FParametersAliasCache[AIndex];
end;

function TdxSQLConnectionProvider.GetPrimaryKeyName(APrimaryKey: TdxDBPrimaryKey; ATable: TdxDBTable): string;
begin
  if APrimaryKey.Name <> '' then
    Result := APrimaryKey.Name
  else
    Result := 'PK_' + ATable.Name;
end;

function TdxSQLConnectionProvider.GetSafeNameColumnMaxLength: Integer;
begin
  Result := GetSafeNameTableMaxLength;
end;

function TdxSQLConnectionProvider.GetSafeNameConstraintMaxLength: Integer;
begin
  Result := GetSafeNameTableMaxLength;
end;

class function TdxSQLConnectionProvider.GetSafeNameDefault(const AOriginalName: string): string;
var
  ASrc, ADest: PChar;
  AChar: Char;
  ALen, I: Integer;
  ASpacesToUnderscore: Boolean;
begin
  ALen := Length(AOriginalName);
  if ALen = 0 then
    Exit(AOriginalName);

  SetLength(Result, ALen);
  ASrc := PChar(AOriginalName);
  ADest := PChar(Result);
  MoveChars(ASrc[0], ADest[0], ALen);
  ASpacesToUnderscore := (ADest[0] = ' ') or (ADest[ALen - 1] = ' ');
  for I := 0 to ALen - 1 do
  begin
    AChar := ADest[I];
  {$IFDEF DELPHIXE4}
    case AChar.GetUnicodeCategory of
  {$ELSE}
    case TCharacter.GetUnicodeCategory(AChar) of
  {$ENDIF}
      TUnicodeCategory.ucControl,
      TUnicodeCategory.ucLineSeparator,
      TUnicodeCategory.ucParagraphSeparator,
      TUnicodeCategory.ucDashPunctuation,
      TUnicodeCategory.ucOpenPunctuation,
      TUnicodeCategory.ucClosePunctuation,
      TUnicodeCategory.ucOtherPunctuation:
        ADest[I] := '_';
      TUnicodeCategory.ucSpaceSeparator:
        if ASpacesToUnderscore then
          ADest[I] := '_';
    end;
  end;
end;

function TdxSQLConnectionProvider.GetSafeNameRoot(const AOriginalName: string): string;
begin
  Result := GetSafeNameDefault(AOriginalName);
end;

class function TdxSQLConnectionProvider.GetSafeObjectName(const AOriginalName, APatchedName: string;
  AMaxLength: Integer): string;
var
  ABuilder: TStringBuilder;
  APrevChar, ACh: Char;
  APatched, ASuffix, APrefix: string;
begin
  if (Length(AOriginalName) <= AMaxLength) and (AOriginalName = APatchedName) then
    Exit(AOriginalName);
  {$IF Defined(DELPHIXE4)}
  APatched := APatchedName.TrimRight(['_']);
  {$ELSEIF Defined(DELPHIXE3)}
  APatched := APatchedName.TrimEnd(['_']);
  {$ELSE}
  APatched := TdxStringHelper.TrimEnd(APatchedName, ['_']);
  {$IFEND}
  {$IFDEF DELPHIXE3}if APatched.IndexOf('__') >= 0{$ELSE}if TdxStringHelper.IndexOf(APatched, '__') >= 0{$ENDIF} then
  begin
    ABuilder := TStringBuilder.Create(Length(APatched) - 1);
    try
      APrevChar := #0;
      for ACh in APatched do
      begin
        if (APrevChar = '_') and (ACh = '_') then
          Continue;
        APrevChar := ACh;
        ABuilder.Append(ACh);
      end;
      APatched := ABuilder.ToString;
    finally
      ABuilder.Free;
    end;
  end;
  ASuffix := '_' + GetDBNameHashString(AOriginalName);
  APrefix := APatched;
  if Length(APrefix)+ Length(ASuffix) > AMaxLength then
    {$IFDEF DELPHIXE3}
    APrefix := APrefix.Substring(0, AMaxLength - Length(ASuffix));
    {$ELSE}
    APrefix := TdxStringHelper.Substring(APrefix, 0, AMaxLength - Length(ASuffix));
    {$ENDIF}
  Result := APrefix + ASuffix;
end;

function TdxSQLConnectionProvider.GetScalar(AQuery: TdxQuery): Variant;
var
  ACommand: TdxCustomDBCommand;
begin
  ACommand := GetCommandFromPool(AQuery);
  try

    Result := ACommand.GetScalar;

  finally
    ReleasePooledCommand(ACommand);
  end;
end;

function TdxSQLConnectionProvider.GetSQLCreateColumnType(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := AColumn.DBTypeName;
  if Result <> '' then
    Exit;
  case AColumn.ColumnType of
    TdxDBColumnType.Boolean:
      Result := GetSQLCreateColumnTypeForBoolean(ATable, AColumn);
    TdxDBColumnType.Byte:
      Result := GetSQLCreateColumnTypeForByte(ATable, AColumn);
    TdxDBColumnType.SByte:
      Result := GetSQLCreateColumnTypeForSByte(ATable, AColumn);
    TdxDBColumnType.Char:
      Result := GetSQLCreateColumnTypeForChar(ATable, AColumn);
    TdxDBColumnType.Decimal:
      Result := GetSQLCreateColumnTypeForDecimal(ATable, AColumn);
    TdxDBColumnType.Double:
      Result := GetSQLCreateColumnTypeForDouble(ATable, AColumn);
    TdxDBColumnType.Single:
      Result := GetSQLCreateColumnTypeForSingle(ATable, AColumn);
    TdxDBColumnType.Int32:
      Result := GetSQLCreateColumnTypeForInt32(ATable, AColumn);
    TdxDBColumnType.UInt32:
      Result := GetSQLCreateColumnTypeForUInt32(ATable, AColumn);
    TdxDBColumnType.Int16:
      Result := GetSQLCreateColumnTypeForInt16(ATable, AColumn);
    TdxDBColumnType.UInt16:
      Result := GetSQLCreateColumnTypeForUInt16(ATable, AColumn);
    TdxDBColumnType.Int64:
      Result := GetSQLCreateColumnTypeForInt64(ATable, AColumn);
    TdxDBColumnType.UInt64:
      Result := GetSQLCreateColumnTypeForUInt64(ATable, AColumn);
    TdxDBColumnType.String:
      Result := GetSQLCreateColumnTypeForString(ATable, AColumn);
    TdxDBColumnType.DateTime:
      Result := GetSQLCreateColumnTypeForDateTime(ATable, AColumn);
    TdxDBColumnType.Guid:
      Result := GetSQLCreateColumnTypeForGuid(ATable, AColumn);
    TdxDBColumnType.ByteArray:
      Result := GetSQLCreateColumnTypeForByteArray(ATable, AColumn);
    TdxDBColumnType.TimeSpan:
      if UseLegacyTimeSpanSupport then
        Result := GetSQLCreateColumnTypeForTimeSpan(ATable, AColumn)
  end;
  if Result = '' then
    raise EArgumentException.Create('');
end;

function TdxSQLConnectionProvider.GetSQLCreateColumnTypeForTimeSpan(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := GetSQLCreateColumnTypeForDouble(ATable, AColumn);
end;


function TdxSQLConnectionProvider.SupportsNamedParameters: Boolean;
begin
  Result := True;
end;

function TdxSQLConnectionProvider.ColumnExists(ATable: TdxDBTable; AColumn: TdxDBColumn): Boolean;
var
  I: Integer;
begin
  for I := 0 to ATable.Columns.Count - 1 do
    if SameText(ATable.Columns[I].Name, ComposeSafeColumnName(AColumn.Name)) then
      Exit(True);
  Result := False;
end;

function TdxSQLConnectionProvider.AreColumnsEqual(AFirst, ASecond: TList<string>): Boolean;
var
  I: Integer;
begin
  if AFirst.Count <> ASecond.Count then
    Exit(False);
  for I := 0 to AFirst.Count - 1 do
    if not SameText(ComposeSafeColumnName(AFirst[I]), ComposeSafeColumnName(ASecond[I])) then
      Exit(False);
  Result := True;
end;

function TdxSQLConnectionProvider.ForeignKeyExists(ATable: TdxDBTable; AForeignKey: TdxDBForeignKey): Boolean;
var
  AKey: TdxDBForeignKey;
begin
  for AKey in ATable.ForeignKeys do
    if SameText(ComposeSafeTableName(AForeignKey.PrimaryKeyTable), ComposeSafeTableName(AKey.PrimaryKeyTable)) and
      AreColumnsEqual(AKey.Columns, AForeignKey.Columns) and
      AreColumnsEqual(AKey.PrimaryKeyTableKeyColumns, AForeignKey.PrimaryKeyTableKeyColumns) then
      Exit(True);
  Result := False;
end;

function TdxSQLConnectionProvider.IndexExists(ATable: TdxDBTable; AIndex: TdxDBIndex): Boolean;
var
  I: Integer;
begin
  for I := 0 to ATable.Indexes.Count - 1 do
    if AreColumnsEqual(ATable.Indexes[I].Columns, AIndex.Columns) then
      Exit(True);
  Result := False;
end;

class function TdxSQLConnectionProvider.IsSingleColumnPKColumn(ATable: TdxDBTable; AColumn: TdxDBColumn): Boolean;
var
  C: TdxDBColumn;
begin
  if not AColumn.IsKey then
    Exit(False);
  if ATable = nil then
    Exit(True);
  if ATable.PrimaryKey <> nil then
  begin
    Assert(ATable.PrimaryKey.Columns.Contains(AColumn.Name));
    Result := ATable.PrimaryKey.Columns.Count = 1;
  end
  else
  begin
    for C in ATable.Columns do
      if C.IsKey and (C.Name <> AColumn.Name) then
        Exit(False);
    Result := True;
  end;
end;

function TdxSQLConnectionProvider.ModifyData(
  const ADMLStatements: TArray<TdxModificationStatement>): TdxModificationResult;
begin
  Result := ProcessModifyData(ADMLStatements);
end;

function TdxSQLConnectionProvider.ModifyData(const ADMLStatements: array of TdxModificationStatement): TdxModificationResult;
var
  I: Integer;
  L: TArray<TdxModificationStatement>;
begin
  SetLength(L, Length(ADMLStatements));
  for I := 0 to High(ADMLStatements) do
    L[I] := ADMLStatements[I];
  Result := ModifyData(L);
end;

function TdxSQLConnectionProvider.NeedCreateIndex(ATable: TdxDBTable; AIndex: TdxDBIndex): Boolean;
begin
  if (ATable.PrimaryKey <> nil) and (AreColumnsEqual(ATable.PrimaryKey.Columns, AIndex.Columns)) then
    Result := False
  else
    Result := True;
end;

function TdxSQLConnectionProvider.NeedsIndexForForeignKey: Boolean;
begin
  Result := True;
end;

function TdxSQLConnectionProvider.ProcessModifyData(
  const ADMLStatements: TArray<TdxModificationStatement>): TdxModificationResult;
var
  ARoot: TdxModificationStatement;
  AResult: IdxParameterValue;
  AIdentities: TdxTaggedParameterHolder;
begin
  Result := nil;
  BeginTransaction;
  try
    Result := TdxModificationResult.Create;
    AIdentities := TdxTaggedParameterHolder.Create;
    try
      for ARoot in ADMLStatements do
      begin
        AResult := UpdateRecord(ARoot, AIdentities);
        if AResult <> nil then
          Result.Add(AResult);
      end;
    finally
      AIdentities.Free;
    end;
    CommitTransaction;
  except
    RollbackTransaction;
    FreeAndNil(Result);
    raise;
  end;
end;

function TdxSQLConnectionProvider.ProcessUpdateSchema(ASkipIfFirstTableNotExists: Boolean;
  const ATables: TArray<TdxDBTable>): TdxUpdateSchemaResult;
var
  ACollectedTables: TArray<TdxDBTable>;
  ATable, ANewTable: TdxDBTable;
  ANewTables: TDictionary<TdxDBTable, TdxDBTable>;
  ACollectIndexes, ACollectFKs: Boolean;
  AColumn: TdxDBColumn;
  AIndex: TdxDBIndex;
  AForeignKey: TdxDBForeignKey;
begin
  ACollectedTables := CollectTablesToCreate(ATables);
  if ASkipIfFirstTableNotExists and (Length(ACollectedTables) > 0) then
    if ATables[0] = ACollectedTables[0] then
      Exit(TdxUpdateSchemaResult.FirstTableNotExists);
  if CanCreateSchema then
    BeginTransaction;
  try
    if not CanCreateSchema and (Length(ACollectedTables) > 0) then
      raise EdxSchemaCorrectionNeededException.CreateFmt(sdxTableNotFound,
        [ComposeSafeTableName(ACollectedTables[0].Name)])
    else
      for ATable in ACollectedTables do
        CreateTable(ATable);
    ANewTables := TObjectDictionary<TdxDBTable, TdxDBTable>.Create([doOwnsValues]);
    try
      for ATable in ATables do
      begin
        if ATable.IsView then
          Continue;
        ANewTable := TdxDBTable.Create(ATable.Name);
        ACollectIndexes := False;
        ACollectFKs := False;
        if CanCreateSchema then
        begin
          ACollectIndexes := ATable.Indexes.Count > 0;
          ACollectFKs := ATable.ForeignKeys.Count > 0;
          if NeedsIndexForForeignKey then
            ACollectIndexes := ACollectIndexes or ACollectFKs;
        end;
        GetTableSchema(ANewTable, ACollectIndexes, ACollectFKs);
        ANewTables.AddOrSetValue(ATable, ANewTable);
        for AColumn in ATable.Columns do
        begin
          if not ColumnExists(ANewTable, AColumn) then
          begin
            if CanCreateSchema then
              CreateColumn(ATable, AColumn)
            else
              raise EdxSchemaCorrectionNeededException.CreateFmt(sdxColumnNotFound,
                [ComposeSafeColumnName(AColumn.Name), ComposeSafeTableName(ATable.Name)]);
          end;
        end;
        if (CanCreateSchema and (not ANewTable.HasPrimaryKey)) and ATable.HasPrimaryKey then
          CreatePrimaryKey(ATable);
      end;
      if CanCreateSchema then
      begin
        for ATable in ATables do
        begin
          ANewTables.TryGetValue(ATable, ANewTable);
          if ANewTable = nil then
            Continue;
          for AIndex in ATable.Indexes do
          begin
            if not IndexExists(ANewTable, AIndex) and NeedCreateIndex(ATable, AIndex) then
            begin
              CreateIndex(ATable, AIndex);
              ANewTable.AddIndex(AIndex.Clone);
            end;
          end;
          if NeedsIndexForForeignKey then
            for AForeignKey in ATable.ForeignKeys do
            begin
              AIndex := TdxDBIndex.Create(AForeignKey.Columns, False);
              try
                if not IndexExists(ANewTable, AIndex) and
                  ((ATable.PrimaryKey = nil) or (not AreColumnsEqual(ATable.PrimaryKey.Columns, AIndex.Columns))) then
                begin
                  CreateIndex(ATable, AIndex);
                  ANewTable.AddIndex(AIndex);
                  AIndex := nil;
                end;
              finally
                AIndex.Free;
              end;
            end;
          for AForeignKey in ATable.ForeignKeys do
          begin
            if not ForeignKeyExists(ANewTable, AForeignKey) then
              CreateForeignKey(ATable, AForeignKey);
          end;
        end;
        CommitTransaction;
      end;
    finally
      ANewTables.Free;
    end;
  except
    on E: Exception do
    begin
      if CanCreateSchema then
        RollbackTransaction;
      raise EdxUnableToCreateDBObjectException.Create(E.Message);
    end;
  end;
  Result := TdxUpdateSchemaResult.SchemaExists;
end;

procedure TdxSQLConnectionProvider.ReleaseCommandPool;
begin
  FreeAndNil(FCommandPool);
end;

procedure TdxSQLConnectionProvider.ReleasePooledCommand(ACommand: TdxCustomDBCommand);
begin
  if FCommandPool <> nil then
    FCommandPool.Add(ACommand)
  else
    ACommand.Free;
end;

procedure TdxSQLConnectionProvider.RollbackTransaction;
begin
  if not FExplicitTransaction then
    RollbackTransactionCore;
end;

procedure TdxSQLConnectionProvider.RollbackTransactionCore;
begin
  ReleaseCommandPool;
  if FTransaction <> nil then
  begin
    try
      FTransaction.Rollback;
    except
    end;
    FTransaction := nil;
  end;
end;

function TdxSQLConnectionProvider.SelectData(ASelect: TdxSelectStatement): TDataSet;
var
  ASQLGenerator: TdxSelectSQLGenerator;
  AQuery: TdxQuery;
begin
  ASQLGenerator := TdxSelectSQLGenerator.Create(Self);
  try
    AQuery := ASQLGenerator.GenerateSQL(ASelect);
    try
      Result := SelectData(AQuery);
    finally
      AQuery.Free;
    end;
  finally
    ASQLGenerator.Free;
  end;
end;

function TdxSQLConnectionProvider.SelectData(AQuery: TdxQuery): TDataSet;
begin
  if FDataSetHolder = nil then
    FDataSetHolder := TDataSetHolder.Create(nil);
  Result := FConnectionVendor.SelectData(FDataSetHolder, AQuery);
end;

function TdxSQLConnectionProvider.UpdateRecord(ARoot: TdxModificationStatement;
  AIdentities: TdxTaggedParameterHolder): IdxParameterValue;
begin
  if ARoot is TdxInsertStatement then
    Result := DoInsertRecord(TdxInsertStatement(ARoot), AIdentities)
  else
    if ARoot is TdxUpdateStatement then
      Result := DoUpdateRecord(TdxUpdateStatement(ARoot), AIdentities)
    else
      if ARoot is TdxDeleteStatement then
        Result := DoDeleteRecord(TdxDeleteStatement(ARoot), AIdentities)
      else
        raise EInvalidOperation.Create(sdxUnknownModificationStatement);
end;

procedure TdxSQLConnectionProvider.UpdateSchema(const ATables: array of TdxDBTable);
var
  I: Integer;
  L: TArray<TdxDBTable>;
begin
  SetLength(L, Length(ATables));
  for I := 0 to High(ATables) do
    L[I] := ATables[I];
  ProcessUpdateSchema(False, L);
end;

{ TdxSQLConnectionProviderFactory }

class destructor TdxSQLConnectionProviderFactory.Destroy;
begin
  FreeAndNil(FProviderSQLs);
end;

class function TdxSQLConnectionProviderFactory.GetProviderSQL(const ADBEngine: TdxDBEngine): TdxSQLConnectionProviderClass;
var
  I: Integer;
begin
  Result := nil;
  if FProviderSQLs = nil then
    Exit;
  for I := 0 to FProviderSQLs.Count - 1 do
    if UpperCase(FProviderSQLs[I].GetDBEngine) = UpperCase(ADBEngine) then
    begin
      Result := FProviderSQLs[I];
      Exit;
    end;
end;

class function TdxSQLConnectionProviderFactory.HasProvider(const ADBEngine: TdxDBEngine): Boolean;
var
  I: Integer;
begin
  Result := FProviderSQLs <> nil;
  if Result then
  begin
    for I := 0 to FProviderSQLs.Count - 1 do
      if UpperCase(FProviderSQLs[I].GetDBEngine) = UpperCase(ADBEngine) then
        Exit;
    Result := False;
  end;
end;

class procedure TdxSQLConnectionProviderFactory.Register(AClass: TdxSQLConnectionProviderClass);
begin
  if FProviderSQLs = nil then
    FProviderSQLs := TList<TdxSQLConnectionProviderClass>.Create;
  if FProviderSQLs.IndexOf(AClass) = -1 then
    FProviderSQLs.Add(AClass);
end;

class procedure TdxSQLConnectionProviderFactory.Unregister(AClass: TdxSQLConnectionProviderClass);
begin
  if FProviderSQLs = nil then
    Exit;
  FProviderSQLs.Remove(AClass);
end;

class procedure TdxSQLConnectionProviderFactory.PopulateDBEngines(const AList: TList<TdxDBEngine>);
var
  I: Integer;
begin
  if FProviderSQLs = nil then
    Exit;
  for I := 0 to FProviderSQLs.Count - 1 do
    AList.Add(FProviderSQLs[I].GetDBEngine);
end;

end.
