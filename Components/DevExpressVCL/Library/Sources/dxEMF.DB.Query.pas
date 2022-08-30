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

unit dxEMF.DB.Query;

{$I cxVer.inc}
{$I dxEMF.inc}

interface

uses
  Classes, SysUtils, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxGenerics,
  dxEMF.Types,
  dxEMF.Utils,
  dxEMF.DB.Model,
  dxEMF.DB.Criteria;

type
  TdxJoinNodeList = class;
  TdxJoinNodeCollection = class;
  TdxQuerySubQueryContainer = class;

  IdxQuerySubQueryContainer = interface;

  IdxQueryCriteriaVisitor = interface(IdxCriteriaVisitor)
    procedure Visit(const AOperand: IdxQuerySubQueryContainer); overload;
  end;

  IdxQueryCriteriaVisitor<T> = interface(IdxCriteriaVisitor<T>)
    function Visit(const AOperand: IdxQueryOperand): T; overload;
    function Visit(const AOperand: IdxQuerySubQueryContainer): T; overload;
  end;

  IdxQueryCriteriaOperatorVisitor = interface(IdxCriteriaOperatorVisitor)
  ['{863E9E7C-9D9A-451F-97A0-9DEE037ED818}']
    function Visit(const AOperand: IdxQueryOperand): IdxCriteriaOperator; overload;
    function Visit(const AOperand: IdxQuerySubQueryContainer): IdxCriteriaOperator; overload;
  end;

  IdxQuerySQLGeneratorVisitor = interface(IdxSQLGeneratorVisitor)
  ['{757997C3-960C-4400-9064-534D827669D9}']
    function Visit(const AOperand: IdxQuerySubQueryContainer): string; overload;
  end;

  IQueryCriteriaToStringCriteriaVisitor = interface(IdxClientCriteriaToStringVisitor)
  ['{BD60AC01-9F11-43C9-8718-D02F32662013}']
    function Visit(const AOperand: IdxQueryOperand): TdxCriteriaToStringVisitResult; overload;
    function Visit(const AOperand: IdxQuerySubQueryContainer): TdxCriteriaToStringVisitResult; overload;
  end;

  IQueryCriteriaToStringListVisitor = interface(IdxQueryCriteriaVisitor<TList<string>>)
  ['{3C62D670-A4A4-4A84-A4C4-D2D3BC0F6687}']
  end;

  { TdxQueryParameterCollection }

  TdxQueryParameterCollection = class(TList<IdxOperandValue>)
  strict private
    class var FEmpty: TdxQueryParameterCollection;
    class destructor Destroy;
    class function GetEmpty: TdxQueryParameterCollection; static;
  protected
    class property Empty: TdxQueryParameterCollection read GetEmpty;
  public
    constructor Create(const AValue: IdxOperandValue); overload;
    constructor Create(const AValues: array of IdxOperandValue); overload;
  end;

  { IdxParameterValue }

  IdxParameterValue = interface(IdxOperandValue)
  ['{7C3D45DF-AEFA-4CDE-A662-AEF680CA93DA}']
  end;

  { TdxParameterValue }

  TdxParameterValue = class(TdxOperandValue, IdxParameterValue)
  strict private
    FTag: Integer;
  public
    constructor Create(ATag: Integer);
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    property Tag: Integer read FTag;
  end;

  { TdxTaggedParameterHolder }

  TdxTaggedParameterHolder = class
  strict private
    FParametersByTag: TDictionary<Integer, IdxParameterValue>;
  public
    constructor Create;
    destructor Destroy; override;
    function ConsolidateParameter(const ADeserializedParameter: IdxOperandValue): IdxOperandValue;
    procedure ConsolidateIdentity(const AIdentityInsertParameter: IdxParameterValue);
  end;

  { TdxSortingColumn }

  TdxSortingColumn = class
  strict private
    FSortDirection: TdxSortDirection;
    FProperty: IdxCriteriaOperator;
  public
    constructor Create(const AColumnName: string; const ANodeAlias: string; ASortDirection: TdxSortDirection); overload;
    constructor Create(const AProperty: IdxCriteriaOperator; ASortDirection: TdxSortDirection); overload;
//    destructor Destroy; override;

    property SortDirection: TdxSortDirection read FSortDirection write FSortDirection;
    property &Property: IdxCriteriaOperator read FProperty;
  end;

  TdxQuerySortingCollection = TObjectList<TdxSortingColumn>;

  { TdxSubQueriesFinder }

  TdxSubQueriesFinder = class(TcxIUnknownObject, IdxCriteriaVisitor, IdxQueryCriteriaVisitor)
  strict private
    FResult: TdxJoinNodeCollection;
  public
    constructor Create;
    class function FindSubQueries(const ACriteria: IdxCriteriaOperator): TdxJoinNodeCollection; static;
    procedure Process(const ACriteria: IdxCriteriaOperator); overload;
    procedure Process(const ACriteria: IEnumerable<IdxCriteriaOperator>); overload;
    procedure Visit(const AOperator: IdxBetweenOperator); overload;
    procedure Visit(const AOperator: IdxBinaryOperator); overload;
    procedure Visit(const AOperator: IdxUnaryOperator); overload;
    procedure Visit(const AOperator: IdxInOperator); overload;
    procedure Visit(const AOperator: IdxGroupOperator); overload;
    procedure Visit(const AOperand: IdxOperandValue); overload;
    procedure Visit(const AOperator: IdxFunctionOperator); overload;
    procedure Visit(const AOperand: IdxQueryOperand); overload;
    procedure Visit(const AOperand: IdxQuerySubQueryContainer); overload;
  end;

  { TdxJoinNode }

  TdxJoinNode = class
  strict private
    FSubNodes: TdxJoinNodeCollection;
    FAlias: string;
    FType: TdxJoinType;
    FTable: TdxDBTable;
    FCondition: IdxCriteriaOperator;
  protected
    class function GetHashCode(AObject: TObject): Integer; reintroduce; overload; static;
    procedure CollectJoinNodesAndCriteriaInternal(ANodes: TdxJoinNodeList; ACriteria: TdxCriteriaOperatorList); virtual;
  public
    constructor Create; overload;
    constructor Create(ATable: TdxDBTable; const AAlias: string; AType: TdxJoinType); overload;
    destructor Destroy; override;

    function GetColumn(const AName: string): TdxDBColumn;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; overload; override;
    procedure CollectJoinNodesAndCriteria(out ANodes: TdxJoinNodeList; out ACriteria: TdxCriteriaOperatorList);
    function ToString: string; override;
    property Alias: string read FAlias write FAlias;
    property Table: TdxDBTable read FTable write FTable;
    property Condition: IdxCriteriaOperator read FCondition write FCondition;
    property SubNodes: TdxJoinNodeCollection read FSubNodes;
    property &Type: TdxJoinType read FType write FType;
  end;

  { TdxJoinNodeList }

  TdxJoinNodeList = class(TList<TdxJoinNode>)
  public
    function Any(AFunc: TFunc<TdxJoinNode, Boolean>): Boolean;
  end;

  { TdxJoinNodeCollection }

  TdxJoinNodeCollection = class(TdxJoinNodeList)
  protected
    procedure Notify(const Value: TdxJoinNode; Action: TCollectionNotification); override;
  public
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    function ToString: string; override;
  end;

  { TdxBaseStatement }

  TdxBaseStatement = class abstract(TdxJoinNode)
  strict private
    FOperands: TdxCriteriaOperatorList;
  protected
    procedure CollectJoinNodesAndCriteriaInternal(ANodes: TdxJoinNodeList; ACriteria: TdxCriteriaOperatorList); override;
  public
    constructor Create(ATable: TdxDBTable; const AAlias: string); overload;
    destructor Destroy; override;
    function Equals(AObject: TObject): Boolean; overload; override;
    class function Equals(AObjectA, AObjectB: TObject): Boolean; reintroduce; overload; static; inline;
    function GetTableNames: TArray<string>; overload;
    class function GetTableNames(const AStatements: TArray<TdxBaseStatement>): TArray<string>; overload; static;
    property Operands: TdxCriteriaOperatorList read FOperands;
  end;

  { TdxSelectStatement }

  TdxSelectStatement = class(TdxBaseStatement)
  strict private
    FSkipSelectedRecords: Integer;
    FTopSelectedRecords: Integer;
    FSortProperties: TdxQuerySortingCollection;
    FGroupProperties: TdxCriteriaOperatorList;
    FGroupCondition: IdxCriteriaOperator;
    function GetSortProperties: TdxQuerySortingCollection; {$IFDEF DELPHIXE3}inline;{$ENDIF}
    function GetGroupProperties: TdxCriteriaOperatorList; {$IFDEF DELPHIXE3}inline;{$ENDIF}
  protected
    procedure CollectJoinNodesAndCriteriaInternal(ANodes: TdxJoinNodeList; ACriteria: TdxCriteriaOperatorList); override;
  public
    destructor Destroy; override;
    function Equals(AObject: TObject): Boolean; override;
    function ToString: string; override;

    property SkipSelectedRecords: Integer read FSkipSelectedRecords write FSkipSelectedRecords;
    property TopSelectedRecords: Integer read FTopSelectedRecords write FTopSelectedRecords;
    property SortProperties: TdxQuerySortingCollection read GetSortProperties;
    property GroupProperties: TdxCriteriaOperatorList read GetGroupProperties;
    property GroupCondition: IdxCriteriaOperator read FGroupCondition write FGroupCondition;
  end;

  { TdxModificationStatement }

  TdxModificationStatement = class abstract(TdxBaseStatement)
  strict private
    FAffectedRecordCount: Integer;
    FParameters: TdxQueryParameterCollection;
  public
    constructor Create(ATable: TdxDBTable; const AAlias: string);
    destructor Destroy; override;
    property AffectedRecordCount: Integer read FAffectedRecordCount write FAffectedRecordCount;
    property Parameters: TdxQueryParameterCollection read FParameters;
  end;

  { TdxInsertStatement }

  TdxInsertStatement = class(TdxModificationStatement)
  strict private
    FIdentityParameter: IdxParameterValue;
    FIdentityColumn: string;
    FIdentityColumnType: TdxDBColumnType;
  public
    property IdentityParameter: IdxParameterValue read FIdentityParameter write FIdentityParameter;
    property IdentityColumn: string read FIdentityColumn write FIdentityColumn;
    property IdentityColumnType: TdxDBColumnType read FIdentityColumnType write FIdentityColumnType;
  end;

  { TdxUpdateStatement }

  TdxUpdateStatement = class(TdxModificationStatement)
  end;

  { TdxDeleteStatement }

  TdxDeleteStatement = class(TdxModificationStatement)
  end;

  { TdxDBProjection }

  TdxDBProjection = class(TdxDBTable)
  strict private
    FProjection: TdxSelectStatement;
  public
    constructor Create(AProjection: TdxSelectStatement);
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    function ToString: string; override;
    property Projection: TdxSelectStatement read FProjection;
  end;

  { TdxQuery }

  TdxQuery = class
  strict private
    FSQLText: string;
    FParameters: TdxQueryParameterCollection;
    FParameterNames: TArray<string>;
    FParametersBuf: TDictionary<string, IdxOperandValue>;
    FSkipSelectedRecords: Integer;
    FTopSelectedRecords: Integer;
    FConstantValues: TDictionary<Integer, IdxOperandValue>;
    FOperandIndexes: TDictionary<Integer, Integer>;
    procedure PopulateParametersBuffer;
  public
    constructor Create(const ASQLText: string); overload;
    constructor Create(const ASQLText: string; AParameters: TdxQueryParameterCollection; const AParameterNames: TArray<string>); overload;
    constructor Create(const ASQLText: string; AParameters: TdxQueryParameterCollection; const AParameterNames: TArray<string>;
      ATopSelectedRecords: Integer); overload;
    constructor Create(const ASQLText: string; AParameters: TdxQueryParameterCollection; const AParameterNames: TArray<string>;
      ASkipSelectedRecords: Integer; ATopSelectedRecords: Integer); overload;
    constructor Create(const ASQLText: string; AParameters: TdxQueryParameterCollection; const AParameterNames: TArray<string>;
      ATopSelectedRecords: Integer; AConstantValues: TDictionary<Integer, IdxOperandValue>; AOperandIndexes: TDictionary<Integer, Integer>); overload;
    constructor Create(const ASQLText: string; AParameters: TdxQueryParameterCollection; const AParameterNames: TArray<string>;
      ASkipSelectedRecords: Integer; ATopSelectedRecords: Integer; AConstantValues: TDictionary<Integer, IdxOperandValue>;
      AOperandIndexes: TDictionary<Integer, Integer>); overload;
    destructor Destroy; override;
    procedure AppendParameterName(const AName: string);
    function ParameterValueByName(const AName: string): IdxOperandValue;

    property ConstantValues: TDictionary<Integer, IdxOperandValue> read FConstantValues;
    property OperandIndexes: TDictionary<Integer, Integer> read FOperandIndexes;
    property SQLText: string read FSQLText;
    property Parameters: TdxQueryParameterCollection read FParameters;
    property ParameterNames: TArray<string> read FParameterNames;
    property SkipSelectedRecords: Integer read FSkipSelectedRecords;
    property TopSelectedRecords: Integer read FTopSelectedRecords;
  end;

  { TdxQueryCollection }

  TdxQueryCollection = class(TList<TdxQuery>);

  { IdxQuerySubQueryContainer }

  IdxQuerySubQueryContainer = interface(IdxCriteriaOperator)
  ['{B5FA8DE7-7F65-4408-9114-B0357B96C255}']
    function GetNode: TdxBaseStatement;
    function GetAggregateFunctionType: TdxAggregateFunctionType;
    function GetAggregateProperty: IdxCriteriaOperator;
    property Node: TdxBaseStatement read GetNode;
    property AggregateFunctionType: TdxAggregateFunctionType read GetAggregateFunctionType;
    property AggregateProperty: IdxCriteriaOperator read GetAggregateProperty;
  end;

  { TdxQuerySubQueryContainer }

  TdxQuerySubQueryContainer = class(TdxCriteriaOperator, IdxQuerySubQueryContainer)
  strict private
    FNode: TdxBaseStatement;
    FAggregateFunctionType: TdxAggregateFunctionType;
    FAggregateProperty: IdxCriteriaOperator;
    function GetNode: TdxBaseStatement;
    function GetAggregateFunctionType: TdxAggregateFunctionType;
    function GetAggregateProperty: IdxCriteriaOperator;
  public
    constructor Create; overload;
    constructor Create(ANode: TdxBaseStatement; const AAggregateProperty: IdxCriteriaOperator; AAggregateFunctionType: TdxAggregateFunctionType); overload;
    destructor Destroy; override;
    function Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator; override;
    function Accept(const AVisitor: IdxSQLGeneratorVisitor): string; override;
    function Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult; override;
    function Accept(const AVisitor: IdxCriteriaVisitorListString): TList<string>; override;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;

    property Node: TdxBaseStatement read FNode write FNode;
    property AggregateFunctionType: TdxAggregateFunctionType read FAggregateFunctionType write FAggregateFunctionType;
    property AggregateProperty: IdxCriteriaOperator read FAggregateProperty write FAggregateProperty;
  end;

implementation

uses
  dxEMF.DB.Utils, dxCore;

{ TdxQueryParameterCollection }

constructor TdxQueryParameterCollection.Create(const AValue: IdxOperandValue);
begin
  inherited Create;
  Add(AValue);
end;

constructor TdxQueryParameterCollection.Create(const AValues: array of IdxOperandValue);
begin
  inherited Create;
  AddRange(AValues);
end;

class destructor TdxQueryParameterCollection.Destroy;
begin
  FreeAndNil(FEmpty);
end;

class function TdxQueryParameterCollection.GetEmpty: TdxQueryParameterCollection;
begin
  if FEmpty = nil then
    FEmpty := TdxQueryParameterCollection.Create;
  Result := FEmpty;
end;

{ TdxParameterValue }

constructor TdxParameterValue.Create(ATag: Integer);
begin
  inherited Create;
  FTag := ATag;
end;

function TdxParameterValue.Equals(AObject: TObject): Boolean;
var
  AAnother: TdxParameterValue;
begin
  AAnother := Safe<TdxParameterValue>.Cast(AObject);
  if AAnother = nil then
    Exit(False);
  Result := (Tag = AAnother.Tag) and inherited Equals(AAnother);
end;

function TdxParameterValue.GetHashCode: Integer;
begin
  Result := inherited GetHashCode xor FTag;
end;

{ TdxTaggedParameterHolder }

constructor TdxTaggedParameterHolder.Create;
begin
  inherited Create;
  FParametersByTag := TDictionary<Integer, IdxParameterValue>.Create;
end;

destructor TdxTaggedParameterHolder.Destroy;
begin
  FreeAndNil(FParametersByTag);
  inherited Destroy;
end;

function TdxTaggedParameterHolder.ConsolidateParameter(const ADeserializedParameter: IdxOperandValue): IdxOperandValue;
var
  AValue: IdxParameterValue;
  AResult: IdxParameterValue;
  ATag: Integer;
begin
  if not Supports(ADeserializedParameter, IdxParameterValue, AValue) then
    Exit(ADeserializedParameter);
  ATag := (AValue as TdxParameterValue).Tag;
  if not FParametersByTag.TryGetValue(ATag, AResult) then
  begin
    FParametersByTag.Add(ATag, AValue);
    Result := AValue;
  end
  else
    Result := AResult;
end;

procedure TdxTaggedParameterHolder.ConsolidateIdentity(const AIdentityInsertParameter: IdxParameterValue);
{$IFOPT C+}
var
  AConsolidated: IdxOperandValue;
{$ENDIF}
begin
{$IFOPT C+}
  AConsolidated := ConsolidateParameter(AIdentityInsertParameter);
  Assert((AConsolidated as TdxCriteriaOperator) = (AIdentityInsertParameter as TdxCriteriaOperator));
{$ELSE}
  ConsolidateParameter(AIdentityInsertParameter);
{$ENDIF}
end;

{ TdxSortingColumn }

constructor TdxSortingColumn.Create(const AProperty: IdxCriteriaOperator; ASortDirection: TdxSortDirection);
begin
  inherited Create;
  FProperty := AProperty;
  FSortDirection := ASortDirection;
end;

//destructor TdxSortingColumn.Destroy;
//begin
//  FProperty.Release;
//  inherited Destroy;
//end;

constructor TdxSortingColumn.Create(const AColumnName: string; const ANodeAlias: string; ASortDirection: TdxSortDirection);
begin
  Create(TdxQueryOperand.Create(AColumnName, ANodeAlias), ASortDirection);
end;



{ TdxSubQueriesFinder }

constructor TdxSubQueriesFinder.Create;
begin
  inherited Create;
  FResult := TdxJoinNodeCollection.Create;
end;

class function TdxSubQueriesFinder.FindSubQueries(const ACriteria: IdxCriteriaOperator): TdxJoinNodeCollection;
var
  AFinder: TdxSubQueriesFinder;
begin
  AFinder := TdxSubQueriesFinder.Create;
  try
    AFinder.Process(ACriteria as TdxCriteriaOperator);
    Result := AFinder.FResult;
  finally
    AFinder.Free;
  end;
end;


procedure TdxSubQueriesFinder.Process(const ACriteria: IdxCriteriaOperator);
begin
  if ACriteria = nil then
    Exit;
  (ACriteria as TdxCriteriaOperator).Accept(Self);
end;

procedure TdxSubQueriesFinder.Process(const ACriteria: IEnumerable<IdxCriteriaOperator>);
var
  ACriterion: IdxCriteriaOperator;
begin
  if ACriteria = nil then
    Exit;
  for ACriterion in ACriteria do
    Process(ACriterion as TdxCriteriaOperator);
end;

procedure TdxSubQueriesFinder.Visit(const AOperator: IdxBetweenOperator);
begin
  Process(AOperator.TestExpression as TdxCriteriaOperator);
  Process(AOperator.BeginExpression as TdxCriteriaOperator);
  Process(AOperator.EndExpression as TdxCriteriaOperator);
end;

procedure TdxSubQueriesFinder.Visit(const AOperator: IdxBinaryOperator);
begin
  Process(AOperator.LeftOperand as TdxCriteriaOperator);
  Process(AOperator.RightOperand as TdxCriteriaOperator);
end;

procedure TdxSubQueriesFinder.Visit(const AOperator: IdxUnaryOperator);
begin
  Process(AOperator.Operand as TdxCriteriaOperator);
end;

procedure TdxSubQueriesFinder.Visit(const AOperator: IdxInOperator);
begin
  Process(AOperator.LeftOperand);
  Process(AOperator.Operands);
end;

procedure TdxSubQueriesFinder.Visit(const AOperator: IdxGroupOperator);
begin
  Process(AOperator.Operands);
end;

procedure TdxSubQueriesFinder.Visit(const AOperand: IdxOperandValue);
begin
end;

procedure TdxSubQueriesFinder.Visit(const AOperator: IdxFunctionOperator);
begin
  Process(AOperator.Operands);
end;

procedure TdxSubQueriesFinder.Visit(const AOperand: IdxQueryOperand);
begin
end;

procedure TdxSubQueriesFinder.Visit(const AOperand: IdxQuerySubQueryContainer);
begin
  if AOperand.Node <> nil then
    FResult.Add(AOperand.Node)
  else
    Process(AOperand.AggregateProperty);
end;

{ TdxJoinNode }

constructor TdxJoinNode.Create;
begin
  inherited Create;
  FSubNodes := TdxJoinNodeCollection.Create;
  FType := TdxJoinType.Inner;
end;

constructor TdxJoinNode.Create(ATable: TdxDBTable; const AAlias: string; AType: TdxJoinType);
begin
  Create;
  FType := AType;
  FAlias := AAlias;
  FTable := ATable;
end;

destructor TdxJoinNode.Destroy;
begin
  FreeAndNil(FSubNodes);
  inherited Destroy;
end;

function TdxJoinNode.GetColumn(const AName: string): TdxDBColumn;
begin
  if FTable = nil then
    raise EInvalidOperation.Create('');
  Result := FTable.GetColumn(AName);
end;

function TdxJoinNode.Equals(AObject: TObject): Boolean;
var
  AAnother: TdxJoinNode;
begin
  AAnother := AObject as TdxJoinNode;
  if AAnother = nil then
    Exit(False);
  Result := (FAlias = AAnother.FAlias) and (FType = AAnother.FType) and (FTable = AAnother.FTable) and
    (FCondition = AAnother.FCondition) and FSubNodes.Equals(AAnother.FSubNodes);
end;

class function TdxJoinNode.GetHashCode(AObject: TObject): Integer;
begin
  if AObject = nil then
    Result := 0
  else
    Result := AObject.GetHashCode;
end;

function TdxJoinNode.GetHashCode: Integer;
begin
  Result := 0;
end;

procedure TdxJoinNode.CollectJoinNodesAndCriteriaInternal(ANodes: TdxJoinNodeList; ACriteria: TdxCriteriaOperatorList);
var
  ANextNode: TdxJoinNode;
begin
  ANodes.Add(Self);
  ACriteria.Add(FCondition);
  for ANextNode in TdxSubQueriesFinder.FindSubQueries(Condition) do
    ANextNode.CollectJoinNodesAndCriteriaInternal(ANodes, ACriteria);
  for ANextNode in FSubNodes do
    ANextNode.CollectJoinNodesAndCriteriaInternal(ANodes, ACriteria);
end;

procedure TdxJoinNode.CollectJoinNodesAndCriteria(out ANodes: TdxJoinNodeList; out ACriteria: TdxCriteriaOperatorList);
begin
  ANodes := TdxJoinNodeList.Create;
  ACriteria := TdxCriteriaOperatorList.Create;
  CollectJoinNodesAndCriteriaInternal(ANodes, ACriteria);
end;

function TdxJoinNode.ToString: string;
begin
  Result := Format(#9'%s join "%s" %s on %s'#10'%s',
    [TdxFormatterHelper.GetName(FType), FTable.Name, FAlias, (Condition as TdxCriteriaOperator).ToString, FSubNodes.ToString]);
end;

{ TdxJoinNodeList }

function TdxJoinNodeList.Any(AFunc: TFunc<TdxJoinNode, Boolean>): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if AFunc(Items[I]) then
      Exit(True);
  Result := False;
end;

{ TdxJoinNodeCollection }

function TdxJoinNodeCollection.Equals(AObject: TObject): Boolean;
var
  AAnother: TdxJoinNodeCollection;
  I: Integer;
begin
  AAnother := AObject as TdxJoinNodeCollection;
  if AAnother = nil then
    Exit(False);
  if Count <> AAnother.Count then
    Exit(False);
  for I := 0 to Count - 1 do
    if not Self[I].Equals(AAnother[I]) then
      Exit(False);
  Result := True;
end;

function TdxJoinNodeCollection.GetHashCode: Integer;
var
  AResult: Integer;
  O: TObject;
begin
  AResult := 0;
  for O in Self do
    AResult := AResult xor O.GetHashCode;
  Result := AResult;
end;

procedure TdxJoinNodeCollection.Notify(const Value: TdxJoinNode; Action: TCollectionNotification);
begin
  inherited;
{$IFNDEF AUTOREFCOUNT}
  if (Action = cnRemoved) then
    Value.Free;
{$ENDIF !AUTOREFCOUNT}
end;

function TdxJoinNodeCollection.ToString: string;
var
  AResult: TStringBuilder;
  ANode: TdxJoinNode;
begin
  AResult := TStringBuilder.Create;
  try
    for ANode in Self do
      AResult.Append(ANode.ToString);
    Result := AResult.ToString;
  finally
    AResult.Free;
  end;
end;

{ TdxBaseStatement }

constructor TdxBaseStatement.Create(ATable: TdxDBTable; const AAlias: string);
begin
  inherited Create(ATable, AAlias, TdxJoinType.Inner);
  FOperands := TdxCriteriaOperatorList.Create;
end;

destructor TdxBaseStatement.Destroy;
begin
  FreeAndNil(FOperands);
  inherited Destroy;
end;

class function TdxBaseStatement.Equals(AObjectA, AObjectB: TObject): Boolean;
begin
  Result := dxEMF.Utils.Equals(AObjectA, AObjectB);
end;

function TdxBaseStatement.Equals(AObject: TObject): Boolean;
var
  AAnother: TdxBaseStatement;
begin
  AAnother := Safe<TdxBaseStatement>.Cast(AObject);
  if AAnother = nil then
    Exit(False);
  Result := inherited Equals(AAnother) and Equals(FOperands, AAnother.FOperands);
end;

function TdxBaseStatement.GetTableNames: TArray<string>;
begin
  Result := GetTableNames(TArray<TdxBaseStatement>.Create(Self));
end;

class function TdxBaseStatement.GetTableNames(const AStatements: TArray<TdxBaseStatement>): TArray<string>;
var
  ATablesNames: TList<string>;
  AStatement: TdxBaseStatement;
  ANodes: TdxJoinNodeList;
  ACriteria: TdxCriteriaOperatorList;
  ANode: TdxJoinNode;
  ATable: TdxDBTable;
begin
  ATablesNames := TList<string>.Create;
  try
    for AStatement in AStatements do
    begin
      ANodes := TdxJoinNodeList.Create;
      ACriteria := TdxCriteriaOperatorList.Create;
      try
        AStatement.CollectJoinNodesAndCriteriaInternal(ANodes, ACriteria);
        for ANode in ANodes do
        begin
          ATable := ANode.Table;
          if ATable = nil then
            Continue;
          ATablesNames.Add(ATable.Name);
        end;
      finally
        ANodes.Free;
        ACriteria.Free;
      end;
    end;
    Result := ATablesNames.ToArray;
  finally
    ATablesNames.Free;
  end;
end;

procedure TdxBaseStatement.CollectJoinNodesAndCriteriaInternal(ANodes: TdxJoinNodeList; ACriteria: TdxCriteriaOperatorList);
var
  C: IdxCriteriaOperator;
  ANextNode: TdxJoinNode;
begin
  inherited CollectJoinNodesAndCriteriaInternal(ANodes, ACriteria);
  for C in FOperands do
  begin
    ACriteria.Add(C);
    for ANextNode in TdxSubQueriesFinder.FindSubQueries(C as TdxCriteriaOperator) do
      ANextNode.CollectJoinNodesAndCriteriaInternal(ANodes, ACriteria);
  end;
end;


{ TdxSelectStatement }

function TdxSelectStatement.GetSortProperties: TdxQuerySortingCollection;
begin
  if FSortProperties = nil then
    FSortProperties := TdxQuerySortingCollection.Create;
  Result := FSortProperties;
end;

function TdxSelectStatement.GetGroupProperties: TdxCriteriaOperatorList;
begin
  if FGroupProperties = nil then
    FGroupProperties := TdxCriteriaOperatorList.Create;
  Result := FGroupProperties;
end;

destructor TdxSelectStatement.Destroy;
begin
  FreeAndNil(FSortProperties);
  FreeAndNil(FGroupProperties);
  inherited Destroy;
end;

function TdxSelectStatement.Equals(AObject: TObject): Boolean;
begin
  Result := False;
end;

function TdxSelectStatement.ToString: string;
var
  AResult: TStringBuilder;
begin
  AResult := TStringBuilder.Create('Select ');
  try
    if SkipSelectedRecords <> 0 then
      AResult.Append('skip(').Append(SkipSelectedRecords).Append(') ');
    if TopSelectedRecords <> 0 then
      AResult.Append('top(').Append(TopSelectedRecords).Append(') ');
    AResult.Append(Operands.ToString);
    AResult.Append(#10'  from "').Append(Table).Append('" ').Append(Alias).Append(#10).Append(SubNodes.ToString);
    if Condition <> nil then
      AResult.Append(' where ').Append(TdxCriteriaOperator.ToString(Condition as TdxCriteriaOperator));
    Result := AResult.ToString;
  finally
    AResult.Free;
  end;
end;

procedure TdxSelectStatement.CollectJoinNodesAndCriteriaInternal(ANodes: TdxJoinNodeList; ACriteria: TdxCriteriaOperatorList);
var
  ASort: TdxSortingColumn;
  ANextNode: TdxJoinNode;
  C: IdxCriteriaOperator;
begin
  inherited CollectJoinNodesAndCriteriaInternal(ANodes, ACriteria);
  for ASort in SortProperties do
  begin
    ACriteria.Add(ASort.&Property);
    for ANextNode in TdxSubQueriesFinder.FindSubQueries(ASort.&Property) do
      ANextNode.CollectJoinNodesAndCriteriaInternal(ANodes, ACriteria);
  end;
  for C in GroupProperties do
  begin
    ACriteria.Add(C);
    for ANextNode in TdxSubQueriesFinder.FindSubQueries(C as TdxCriteriaOperator) do
      ANextNode.CollectJoinNodesAndCriteriaInternal(ANodes, ACriteria);
  end;
  ACriteria.Add(GroupCondition);
  for ANextNode in TdxSubQueriesFinder.FindSubQueries(GroupCondition) do
    ANextNode.CollectJoinNodesAndCriteriaInternal(ANodes, ACriteria);
end;

{ TdxModificationStatement }

constructor TdxModificationStatement.Create(ATable: TdxDBTable; const AAlias: string);
begin
  inherited Create(ATable, AAlias);
  FParameters := TdxQueryParameterCollection.Create;
end;

destructor TdxModificationStatement.Destroy;
begin
  FreeAndNil(FParameters);
  inherited Destroy;
end;

{ TdxDBProjection }

constructor TdxDBProjection.Create(AProjection: TdxSelectStatement);
begin
  inherited Create;
  FProjection := AProjection;
end;

function TdxDBProjection.Equals(AObject: TObject): Boolean;
var
  AAnother: TdxDBProjection;
begin
  AAnother := Safe<TdxDBProjection>.Cast(AObject);
  if AAnother = nil then
    Result := False
  else
    Result := Projection.Equals(AAnother.Projection);
end;

function TdxDBProjection.GetHashCode: Integer;
begin
  if Projection = nil then
    Result := $6428231
  else
    Result := Projection.GetHashCode;
end;

function TdxDBProjection.ToString: string;
begin
  if Projection = nil then
    Result := 'nil'
  else
    Result := Projection.ToString;
end;

{ TdxQuery }

constructor TdxQuery.Create(const ASQLText: string; AParameters: TdxQueryParameterCollection; const AParameterNames: TArray<string>; ASkipSelectedRecords: Integer; ATopSelectedRecords: Integer);
begin
  inherited Create;
  FConstantValues := nil;
  FOperandIndexes := nil;
  FSQLText := ASQLText;
  FParameters := AParameters;
  FParameterNames := AParameterNames;
  FSkipSelectedRecords := ASkipSelectedRecords;
  FTopSelectedRecords := ATopSelectedRecords;
  FParametersBuf := TDictionary<string, IdxOperandValue>.Create;
  PopulateParametersBuffer;
end;

constructor TdxQuery.Create(const ASQLText: string; AParameters: TdxQueryParameterCollection; const AParameterNames: TArray<string>;
  ASkipSelectedRecords: Integer; ATopSelectedRecords: Integer; AConstantValues: TDictionary<Integer, IdxOperandValue>;
  AOperandIndexes: TDictionary<Integer, Integer>);
begin
  Create(ASQLText, AParameters, AParameterNames, ASkipSelectedRecords, ATopSelectedRecords);
  FConstantValues := TDictionary<Integer, IdxOperandValue>.Create(AConstantValues);
  FOperandIndexes := TDictionary<Integer, Integer>.Create(AOperandIndexes);
end;

constructor TdxQuery.Create(const ASQLText: string; AParameters: TdxQueryParameterCollection; const AParameterNames: TArray<string>;
  ATopSelectedRecords: Integer; AConstantValues: TDictionary<Integer, IdxOperandValue>; AOperandIndexes: TDictionary<Integer, Integer>);
begin
  Create(ASQLText, AParameters, AParameterNames, 0, ATopSelectedRecords, AConstantValues, AOperandIndexes);
end;

constructor TdxQuery.Create(const ASQLText: string; AParameters: TdxQueryParameterCollection; const AParameterNames: TArray<string>; ATopSelectedRecords: Integer);
begin
  Create(ASQLText, AParameters, AParameterNames, 0, ATopSelectedRecords);
end;

constructor TdxQuery.Create(const ASQLText: string; AParameters: TdxQueryParameterCollection; const AParameterNames: TArray<string>);
begin
  Create(ASQLText, AParameters, AParameterNames, 0, 0);
end;

constructor TdxQuery.Create(const ASQLText: string);
begin
  Create(ASQLText, TdxQueryParameterCollection.Empty, nil);
end;

destructor TdxQuery.Destroy;
begin
  FreeAndNil(FParametersBuf);
  FreeAndNil(FConstantValues);
  FreeAndNil(FOperandIndexes);
  inherited Destroy;
end;

procedure TdxQuery.AppendParameterName(const AName: string);
var
  I: Integer;
  ANames: TArray<string>;
begin
  I := Length(FParameterNames);
  SetLength(ANames, I + 1);
  ANames[I] := AName;
  FParametersBuf.Add(AName, FParameters[I]);
  for I := 0 to Length(FParameterNames) - 1 do
    ANames[I] := FParameterNames[I];
  FParameterNames := ANames;
end;

procedure TdxQuery.PopulateParametersBuffer;
var
  I: Integer;
begin
  for I := 0 to Length(FParameterNames) - 1 do
    FParametersBuf.Add(FParameterNames[I], FParameters[I]);
end;

function TdxQuery.ParameterValueByName(const AName: string): IdxOperandValue;
begin
  if not FParametersBuf.TryGetValue(AName, Result) then
    Result := nil;
end;

{ TdxQuerySubQueryContainer }

constructor TdxQuerySubQueryContainer.Create;
begin
  Create(nil, nil, TdxAggregateFunctionType.Exists);
end;

constructor TdxQuerySubQueryContainer.Create(ANode: TdxBaseStatement; const AAggregateProperty: IdxCriteriaOperator;
  AAggregateFunctionType: TdxAggregateFunctionType);
begin
  inherited Create;
  FNode := ANode;
  FAggregateProperty := AAggregateProperty;
  FAggregateFunctionType := AAggregateFunctionType;
end;

destructor TdxQuerySubQueryContainer.Destroy;
begin
  FreeAndNil(FNode);
  inherited Destroy;
end;

function TdxQuerySubQueryContainer.Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult;
var
  AQueryVisitor: IQueryCriteriaToStringCriteriaVisitor;
begin
  AQueryVisitor := AVisitor as IQueryCriteriaToStringCriteriaVisitor;
  Result := AQueryVisitor.Visit(Self);
end;

function TdxQuerySubQueryContainer.Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator;
begin
  Result := (AVisitor as IdxQueryCriteriaOperatorVisitor).Visit(Self);
end;

function TdxQuerySubQueryContainer.Accept(const AVisitor: IdxSQLGeneratorVisitor): string;
begin
  Result := IdxQuerySQLGeneratorVisitor(AVisitor).Visit(Self);
end;

function TdxQuerySubQueryContainer.Accept(const AVisitor: IdxCriteriaVisitorListString): TList<string>;
begin
  Result := (AVisitor as IQueryCriteriaToStringListVisitor).Visit(Self);
end;

function TdxQuerySubQueryContainer.GetNode: TdxBaseStatement;
begin
  Result := FNode;
end;

function TdxQuerySubQueryContainer.GetAggregateFunctionType: TdxAggregateFunctionType;
begin
  Result := FAggregateFunctionType;
end;

function TdxQuerySubQueryContainer.GetAggregateProperty: IdxCriteriaOperator;
begin
  Result := FAggregateProperty;
end;


function TdxQuerySubQueryContainer.Equals(AObject: TObject): Boolean;
var
  AAnother: TdxQuerySubQueryContainer;
begin
  AAnother := Safe<TdxQuerySubQueryContainer>.Cast(AObject);
  if AAnother = nil then
    Exit(False);
  Result := (TdxCriteriaOperator.Equals(Node, AAnother.Node) and TdxCriteriaOperator.Equals(AggregateProperty, AAnother.AggregateProperty)) and
    (AggregateFunctionType = AAnother.AggregateFunctionType);
end;

function TdxQuerySubQueryContainer.GetHashCode: Integer;
begin
  if Node <> nil then
    Result := Node.GetHashCode
  else
    if AggregateProperty <> nil then
      Result := (AggregateProperty as TdxCriteriaOperator).GetHashCode
    else
      Result := Integer(AggregateFunctionType);
end;


end.
