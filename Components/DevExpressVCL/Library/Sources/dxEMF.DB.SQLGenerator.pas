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

unit dxEMF.DB.SQLGenerator;

{$I cxVer.inc}
{$I dxEMF.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxEMF.Types,
  dxEMF.Utils,
  dxEMF.DB.Criteria,
  dxEMF.DB.Model,
  dxEMF.DB.Query,
  dxEMF.DB.SQLConnectionProvider;

type

  TdxBaseSQLGenerator = class;

  { TdxOuterApplyAggregateInfo }

  TdxOuterApplyAggregateInfo = record
  public
    Node: TdxBaseStatement;
    &Type: TdxAggregateFunctionType;
    constructor Create(ANode: TdxBaseStatement; AType: TdxAggregateFunctionType);
  end;

  { TdxOuterApplyInfo }

  TdxOuterApplyInfo = class
  strict private type

    TComparer = class(TcxIUnknownObject, IEqualityComparer<TdxOuterApplyAggregateInfo>)
    public
      function Equals(const ALeft, ARight: TdxOuterApplyAggregateInfo): Boolean; reintroduce;
      function GetHashCode(const AValue: TdxOuterApplyAggregateInfo): Integer; reintroduce;
    end;
    class var
      FComparer: TComparer;

  strict private
    FAggregates: TDictionary<TdxOuterApplyAggregateInfo, string>;
    FFirstSqlBody: string;
    FAlias: string;
    FTargetNode: TdxBaseStatement;
  protected
    class destructor Destroy;
    property Alias: string read FAlias;
  public
    constructor Create(const AAlias: string; ATargetNode: TdxBaseStatement; const AFirstSqlBody: string);
    destructor Destroy; override;

    function GetAggregateAlias(AAggregateNode: TdxBaseStatement; AAggregateType: TdxAggregateFunctionType): string;
    function GenerateOuterApplySqlBody(AParentGenerator: TdxBaseSqlGenerator; AFormatter: TdxSQLConnectionProvider): string;
  end;

  { TdxOuterApplyCacheItem }

  TdxOuterApplyCacheItem = record
  public type

    TComparer = class(TInterfacedObject, IEqualityComparer<TdxOuterApplyCacheItem>)
    public
      function Equals(const ALeftItem, ARightItem: TdxOuterApplyCacheItem): Boolean; reintroduce;
      function GetHashCode(const AValue: TdxOuterApplyCacheItem): Integer; reintroduce;
    end;

  strict private
    FOperateAsJoinNode: Boolean;
    FNode: TdxJoinNode;
    FHash: Integer;
  public
    constructor Create(ANode: TdxJoinNode; AOperateAsJoinNode: Boolean);
    function Equals(const AValue: TdxOuterApplyCacheItem): Boolean;
    property OperateAsJoinNode: Boolean read FOperateAsJoinNode;
    property Node: TdxJoinNode read FNode;
    property Hash: Integer read FHash;
  end;

  { TdxBaseSQLGenerator }

  TdxBaseSQLGenerator = class abstract(TcxIUnknownObject, IdxSQLGeneratorVisitor, IdxQuerySQLGeneratorVisitor)
  strict private const
    GroupOperatorName: array[TdxGroupOperatorType] of string = ('and', 'or');
  private
    FRoot: TdxBaseStatement;
    FHasSubQuery: Boolean;
    FInTopLevelAggregate: Boolean;
    FOuterApplyAliasCounter: Integer;
    FOuterApplyResultCounter: Integer;
    FOuterApplyCache: TDictionary<TdxOuterApplyCacheItem, TdxOuterApplyInfo>;
    FJoins: TStringBuilder;
    function GetOuterApplyCache: TDictionary<TdxOuterApplyCacheItem, TdxOuterApplyInfo>; inline;
  protected
    FFormatter: TdxSQLConnectionProvider;
    class function GetNodeSingleProperty(ANode: TdxBaseStatement): IdxCriteriaOperator; static;
    function GetForceOuterApply: Boolean; virtual;
    function GetIsSubQuery: Boolean; virtual;
    function GetNextOuterApplyAlias: string;
    function GetUseOuterApply: Boolean; virtual;
    procedure SetUpRootQueryStatement(ARoot: TdxBaseStatement); inline;
    function ProcessParameter(const AOperand: IdxCriteriaOperator; ANullOnNull: Boolean): string;
    function Process(const AOperand: IdxCriteriaOperator): string;
    procedure AppendJoinNode(ANode: TdxJoinNode; AJoins: TStringBuilder);
    function BuildJoins: TStringBuilder;
    function BuildOuterApply: string;
    function BuildCriteria: string;
    function Visit(const AOperand: IdxOperandValue): string; overload;
    function Visit(const AOperator: IdxBetweenOperator): string; overload;
    function Visit(const AOperator: IdxBinaryOperator): string; overload;
    function Visit(const AOperator: IdxInOperator): string; overload;
    function Visit(const AOperator: IdxGroupOperator): string; overload;
    function Visit(const AOperator: IdxUnaryOperator): string; overload;
    function Visit(const AOperator: IdxFunctionOperator): string; overload;
    function Visit(const AContainer: IdxQuerySubQueryContainer): string; overload;
    function Visit(const AOperand: IdxQueryOperand): string; overload;
    class function FormatSubQuery(AAggregateFunctionType: TdxAggregateFunctionType; const ASubQuery: string): string; overload; static;
    class function FormatSubQuery(const ASubQuery: string; const AAlias: string): string; overload; static;
    property OuterApplyResultCounter: Integer read FOuterApplyResultCounter;
    property OuterApplyCache: TDictionary<TdxOuterApplyCacheItem, TdxOuterApplyInfo> read GetOuterApplyCache;
    property UseOuterApply: Boolean read GetUseOuterApply;
    property IsSubQuery: Boolean read GetIsSubQuery;
    property ForceOuterApply: Boolean read GetForceOuterApply;
    property Root: TdxBaseStatement read FRoot;
  public
    constructor Create(const AFormatter: TdxSQLConnectionProvider);
    destructor Destroy; override;

    function GetNextParameterName(const AParameter: IdxOperandValue): string; virtual; abstract;
    property HasSubQuery: Boolean read FHasSubQuery;
  end;

  { TdxBaseSQLGeneratorWithParameters }

  TdxBaseSQLGeneratorWithParameters = class abstract(TdxBaseSQLGenerator)
  strict private
    FQueryParameters: TdxQueryParameterCollection;
    FQueryParameterNames: TList<string>;
    FIdentitiesByTag: TdxTaggedParameterHolder;
    FParameters: TDictionary<IdxOperandValue, string>;
  protected
    function CreateQuery(const ACommandText: string; AParameters: TdxQueryParameterCollection;
      const AParameterNames: TArray<string>): TdxQuery; virtual;
    function InternalGenerateSQL: string; virtual; abstract;
    procedure SetUpParameters;
    property Parameters: TDictionary<IdxOperandValue, string> read FParameters;
    property QueryParameterNames: TList<string> read FQueryParameterNames;
  public
    constructor Create(const AFormatter: TdxSQLConnectionProvider; AIdentitiesByTag: TdxTaggedParameterHolder);
    destructor Destroy; override;
    function GetNextParameterName(const AParameter: IdxOperandValue): string; override;
    function GenerateSQL(ANode: TdxBaseStatement): TdxQuery; overload;
  end;

  { TdxSelectSQLGenerator }

  TdxSelectSQLGenerator = class(TdxBaseSQLGeneratorWithParameters)
  strict private
    FParentGenerator: TdxBaseSQLGenerator;
    FPropertyAliases: TArray<string>;
    FConstantValues: TDictionary<Integer, IdxOperandValue>;
    FOperandIndexes: TDictionary<Integer, Integer>;
    FOAProperties: TdxHashSetString;
    FGroupProperties: TdxHashSetString;
    FIsBuildGrouping: Boolean;
    FIdentities: TdxTaggedParameterHolder;
    function CreateIdentities: TdxTaggedParameterHolder;
    function GetRoot: TdxSelectStatement; inline;
  protected
    SkipSelectedRecords: Integer;
    TopSelectedRecords: Integer;
    function GetUseOuterApply: Boolean; override;
    function BuildSorting: string;
    function GetForceOuterApply: Boolean; override;
    function BuildGrouping: string;
    function BuildAdditionalGroupingOuterApply: string;
    function PatchProperty(const APropertyOperator: IdxCriteriaOperator; const APropertyString: string): string; virtual;
    function BuildProperties: string;
    function BuildGroupCriteria: string;
    function InternalGenerateSQL: string; override;
    function CreateQuery(const ACommandText: string; AParameters: TdxQueryParameterCollection;
      const AParameterNames: TArray<string>): TdxQuery; override;

    property Root: TdxSelectStatement read GetRoot;
  public
    constructor Create(const AFormatter: TdxSQLConnectionProvider); overload;
    constructor Create(const AFormatter: TdxSQLConnectionProvider;
      AParentGenerator: TdxBaseSQLGenerator; const APropertyAliases: TArray<string>); overload;
    destructor Destroy; override;

    function GetNextParameterName(const AParameter: IdxOperandValue): string; override;
  end;

  { TdxBaseObjectSQLGenerator }

  TdxBaseObjectSQLGenerator = class abstract(TdxBaseSQLGeneratorWithParameters)
  public
    function GenerateSQL(const ADMLStatements: TArray<TdxModificationStatement>): TdxQueryCollection; overload;
  end;

 { TdxDeleteSQLGenerator }

  TdxDeleteSQLGenerator = class(TdxBaseObjectSQLGenerator)
  protected
    function InternalGenerateSQL: string; override;
  end;

  { TdxUpdateSQLGenerator }

  TdxUpdateSQLGenerator = class(TdxBaseObjectSQLGenerator)
  protected
    function InternalGenerateSQL: string; override;
  end;

  { TdxInsertSQLGenerator }

  TdxInsertSQLGenerator = class(TdxBaseObjectSQLGenerator)
  protected
    function InternalGenerateSQL: string; override;
  end;

implementation

uses
  StrUtils, Rtti,
  dxCore,
  dxEMF.Utils.Evaluator,
  dxEMF.Strs;

type

  { TdxSubSelectAggregateInfo }

  TdxSubSelectAggregateInfo = record
  public
    &Property: IdxCriteriaOperator;
    AggregateType: TdxAggregateFunctionType;
    Alias: string;
    constructor Create(const AProperty: IdxCriteriaOperator; AAggregateType: TdxAggregateFunctionType;
      const AAlias: string);
  end;

  { TdxSubSelectSQLGenerator }

  TdxSubSelectSQLGenerator = class(TdxBaseSQLGenerator)
  strict private const
    AggregateClause: array[TdxAggregateFunctionType] of string = (
      '%s',
      'count(%s)',
      'max(%s)',
      'min(%s)',
      'avg(%s)',
      'sum(%s)',
      '');
  strict private
    FParentGenerator: TdxBaseSQLGenerator;
    FAggregateProperties: TArray<TdxSubSelectAggregateInfo>;
    FForceOuterApply: Boolean;
  protected
    function GetUseOuterApply: Boolean; override;
    function GetIsSubQuery: Boolean; override;
    function GetForceOuterApply: Boolean; override;
  public
    constructor Create(AParentGenerator: TdxBaseSQLGenerator; AFormatter: TdxSQLConnectionProvider;
      const AAggregateProperty: IdxCriteriaOperator; AAggregate: TdxAggregateFunctionType; AForceOuterApply: Boolean = False); overload;
    constructor Create(AParentGenerator: TdxBaseSqlGenerator; AFormatter: TdxSQLConnectionProvider;
      const AAggregateProperties: TArray<TdxSubSelectAggregateInfo>; AForceOuterApply: Boolean); overload;
    function GenerateSelect(ANode: TdxBaseStatement; ASubSelectUseOnly: Boolean): string;
    class function GetSelectValue(const AAggregateProperty: IdxCriteriaOperator; AAggregate: TdxAggregateFunctionType;
      AGenerator: TdxBaseSQLGenerator; const AAlias: string): string; overload; static;
    function GetSelectValue(ASubSelectUseOnly: Boolean): string; overload;
    function GetNextParameterName(const AParameter: IdxOperandValue): string; override;
  end;

  { TdxOuterApplyCompareCache }

  TdxOuterApplyCompareCache = class
  strict private
    FNodeAliasCounter: Integer;
    FNodeAliasCache: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
    function GetCacheAliasName(const AAlias: string): string;
    procedure AddNode(const ANodeAlias: string; const ANodeCacheAlias: string);
    function GetNextCacheAlias: string;
  end;

  { TdxOuterApplyCacheCompareHelper }

  TdxOuterApplyCacheCompareHelper = class sealed
  protected
    class function GetHashCode(const AInterface: IInterface): Integer; reintroduce; static;
  public
    class function PrepareCacheForConversion(ACompareCache: TdxOuterApplyCompareCache;
      AFrom, ATo: TdxJoinNode): Boolean; overload; static;
    class function PrepareCacheForConversion(ACompareCache: TdxOuterApplyCompareCache; AFromSubNodes: TdxJoinNodeCollection;
      AToSubNodes: TdxJoinNodeCollection): Boolean; overload; static;
    class function AreEquals(ACompareCache: TdxOuterApplyCompareCache; X, Y: TdxJoinNode; AOperateAsJoinNode: Boolean = False): Boolean; overload; static;
    class function AreEquals(ACompareCache: TdxOuterApplyCompareCache; ATable1, ATable2: TdxDBTable): Boolean; overload; static;
    class function AreEquals(ACompareCache: TdxOuterApplyCompareCache; X, Y: TdxJoinNodeCollection): Boolean; overload; static;
    class function GetHash(AObject: TdxJoinNode; AOperateAsJoinNode: Boolean = False): Integer; overload; static;
    class function GetHash(ADbProjection: TdxDBProjection): Integer; overload; static;
    class function GetHash(AObject: TdxJoinNodeCollection): Integer; overload; static;
  end;

  { TdxOuterApplyCacheCriteriaPreprocessor }

  TdxOuterApplyCacheCriteriaPreprocessor = class(TdxClientCriteriaVisitorBase, IdxQueryCriteriaOperatorVisitor)
  strict private
    FCollectNodes: Boolean;
    FConvertNodes: Boolean;
    FCache: TdxOuterApplyCompareCache;
    FSubNodes: TdxJoinNodeCollection;
  public
    constructor Create(ACompareCache: TdxOuterApplyCompareCache; ACollectNodes: Boolean; AConvertNodes: Boolean);
    function Visit(const AOperand: IdxQueryOperand): IdxCriteriaOperator; overload;
    function Visit(const AOperand: IdxQuerySubQueryContainer): IdxCriteriaOperator; overload;
    function Process(ANode: TdxBaseStatement): TdxBaseStatement; overload;
    function Process(ASrcNode: TdxJoinNode): TdxJoinNode; overload;
    function Process(ASrcNode: TdxJoinNode; ADestNode: TdxJoinNode): Boolean; overload;
    class function Preprocess(ACompareCache: TdxOuterApplyCompareCache; const ACriteria: IdxCriteriaOperator): IdxCriteriaOperator; static;
    class function Convert(ACompareCache: TdxOuterApplyCompareCache; const ACriteria: IdxCriteriaOperator): IdxCriteriaOperator; static;
    class function CollectNodes(ACompareCache: TdxOuterApplyCompareCache; const ACriteria: IdxCriteriaOperator): TdxJoinNodeCollection; static;

    property SubNodes: TdxJoinNodeCollection read FSubNodes;
  end;

{ TdxSubSelectAggregateInfo }

constructor TdxSubSelectAggregateInfo.Create(const AProperty: IdxCriteriaOperator;
  AAggregateType: TdxAggregateFunctionType; const AAlias: string);
begin
  &Property := AProperty;
  AggregateType := AAggregateType;
  Alias := AAlias;
end;

{ TdxSubSelectSQLGenerator }

constructor TdxSubSelectSQLGenerator.Create(AParentGenerator: TdxBaseSQLGenerator; AFormatter: TdxSQLConnectionProvider;
  const AAggregateProperty: IdxCriteriaOperator; AAggregate: TdxAggregateFunctionType; AForceOuterApply: Boolean);
var
  AInfos: TArray<TdxSubSelectAggregateInfo>;
begin
  AInfos := TArray<TdxSubSelectAggregateInfo>.Create(
    TdxSubSelectAggregateInfo.Create(AAggregateProperty, AAggregate, 'Res0'));
  Create(AParentGenerator, AFormatter, AInfos, AForceOuterApply);
end;

constructor TdxSubSelectSQLGenerator.Create(AParentGenerator: TdxBaseSqlGenerator; AFormatter: TdxSQLConnectionProvider;
  const AAggregateProperties: TArray<TdxSubSelectAggregateInfo>; AForceOuterApply: Boolean);
begin
  inherited Create(AFormatter);
  FParentGenerator := AParentGenerator;
  FAggregateProperties := AAggregateProperties;
  FForceOuterApply := AForceOuterApply;
end;

function TdxSubSelectSQLGenerator.GetUseOuterApply: Boolean;
begin
  Result := True;
end;

function TdxSubSelectSQLGenerator.GetIsSubQuery: Boolean;
begin
  Result := True;
end;

function TdxSubSelectSQLGenerator.GetForceOuterApply: Boolean;
begin
  Result := FForceOuterApply or (inherited ForceOuterApply);
end;

function TdxSubSelectSQLGenerator.GenerateSelect(ANode: TdxBaseStatement; ASubSelectUseOnly: Boolean): string;
var
  AJoins: TStringBuilder;
  AWhereClause, ASelectValue, AOuterApply: string;
begin
  SetUpRootQueryStatement(ANode);
  AJoins := BuildJoins;
  AWhereClause := BuildCriteria;
  ASelectValue := GetSelectValue(ASubSelectUseOnly);
  AOuterApply := BuildOuterApply;
  if AOuterApply <> '' then
    AJoins.Append(AOuterApply);
  if AWhereClause <> '' then
    AJoins.Append(' where ').Append(AWhereClause);
  Result := Format('select %s from %s', [ASelectValue, AJoins.ToString]);
end;

class function TdxSubSelectSQLGenerator.GetSelectValue(const AAggregateProperty: IdxCriteriaOperator;
  AAggregate: TdxAggregateFunctionType; AGenerator: TdxBaseSQLGenerator; const AAlias: string): string;
var
  AProperty: string;
begin
  if AAggregateProperty = nil then
    AProperty := '*'
  else
    AProperty := (AAggregateProperty as TdxCriteriaOperator).Accept(AGenerator);
  Result := Format(AggregateClause[AAggregate], [AProperty]);
  if (AAggregate = TdxAggregateFunctionType.Exists) or (AAlias = '') then
    Exit;
  Result := Result + ' as ' + AAlias;
end;

function TdxSubSelectSQLGenerator.GetSelectValue(ASubSelectUseOnly: Boolean): string;
var
  ASelectString: TStringBuilder;
  I: Integer;
begin
  ASelectString := TStringBuilder.Create;
  try
    for I := 0 to Length(FAggregateProperties) - 1 do
    begin
      if ASelectString.Length > 0 then
        ASelectString.Append(', ');
      ASelectString.Append(GetSelectValue(FAggregateProperties[I].&Property, FAggregateProperties[I].AggregateType, Self,
        IfThen(ASubSelectUseOnly, '', FAggregateProperties[I].Alias)));
    end;
    Result := ASelectString.ToString;
  finally
    ASelectString.Free;
  end;
end;

function TdxSubSelectSQLGenerator.GetNextParameterName(const AParameter: IdxOperandValue): string;
begin
  Result := FParentGenerator.GetNextParameterName(AParameter);
end;

{ TdxOuterApplyCompareCache }

constructor TdxOuterApplyCompareCache.Create;
begin
  inherited Create;
  FNodeAliasCache := TDictionary<string, string>.Create;
end;

destructor TdxOuterApplyCompareCache.Destroy;
begin
  FreeAndNil(FNodeAliasCache);
  inherited Destroy;
end;

function TdxOuterApplyCompareCache.GetCacheAliasName(const AAlias: string): string;
var
  AResult: string;
begin
  if not FNodeAliasCache.TryGetValue(AAlias, AResult) then
    Result := AAlias
  else
    Result := AResult;
end;

procedure TdxOuterApplyCompareCache.AddNode(const ANodeAlias: string; const ANodeCacheAlias: string);
begin
  FNodeAliasCache.AddOrSetValue(ANodeAlias, ANodeCacheAlias);
end;

function TdxOuterApplyCompareCache.GetNextCacheAlias: string;
begin
  Result := Format('NCA%d', [FNodeAliasCounter]);
  Inc(FNodeAliasCounter)
end;

{ TdxOuterApplyCacheCompareHelper }

class function TdxOuterApplyCacheCompareHelper.PrepareCacheForConversion(ACompareCache: TdxOuterApplyCompareCache;
  AFrom, ATo: TdxJoinNode): Boolean;
var
  ABaseFrom, ABaseTo: TdxBaseStatement;
  ASelectX, ASelectY: TdxSelectStatement;
  AFromConditionSubNodes, AToConditionSubNodes, AXGroupConditionSubNodes, AYGroupConditionSubNodes: TdxJoinNodeCollection;
  AOperandsCount, I, APropertiesCount, ASortPropertiesCount: Integer;
  AXOperandsSubNodes, AYOperandsSubNodes: TdxJoinNodeCollection;
begin
  if AFrom = ATo then
    Exit(True);
  if (AFrom = nil) or (ATo = nil) then
    Exit(False);
  ABaseFrom := Safe<TdxBaseStatement>.Cast(AFrom);
  ABaseTo := Safe<TdxBaseStatement>.Cast(ATo);
  if ((ABaseFrom = nil) and (ABaseTo <> nil)) or ((ABaseFrom <> nil) and (ABaseTo = nil)) then
    Exit(False);
  ASelectX := Safe<TdxSelectStatement>.Cast(AFrom);
  ASelectY := Safe<TdxSelectStatement>.Cast(ATo);
  if ((ASelectX = nil) and (ASelectY <> nil)) or ((ASelectX <> nil) and (ASelectY = nil)) then
    Exit(False);
  if (AFrom.&Type = ATo.&Type) and AreEquals(ACompareCache, AFrom.Table, ATo.Table) then
  begin
    ACompareCache.AddNode(AFrom.Alias, ATo.Alias);
    if not PrepareCacheForConversion(ACompareCache, AFrom.SubNodes, ATo.SubNodes) then
      Exit(False);
    AFromConditionSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, AFrom.Condition);
    try
      AToConditionSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, ATo.Condition);
      try
        if not PrepareCacheForConversion(ACompareCache, AFromConditionSubNodes, AToConditionSubNodes) then
          Exit(False);
      finally
        AToConditionSubNodes.Free;
      end;
    finally
      AFromConditionSubNodes.Free;
    end;
    if (ABaseFrom <> nil) and (ABaseTo <> nil) then
    begin
      if ABaseFrom.Operands.Count <> ABaseTo.Operands.Count then
        Exit(False);
      AOperandsCount := ABaseFrom.Operands.Count;
      AYOperandsSubNodes := nil;
      for I := 0 to AOperandsCount - 1 do
      begin
        AXOperandsSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, ABaseFrom.Operands[I]);
        AYOperandsSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, ABaseTo.Operands[I]);
        try
          if not PrepareCacheForConversion(ACompareCache, AXOperandsSubNodes, AYOperandsSubNodes) then
            Exit(False);
        finally
          AXOperandsSubNodes.Free;
          FreeAndNil(AYOperandsSubNodes);
        end;
      end;
      if (ASelectX <> nil) and (ASelectY <> nil) then
      begin
        if (ASelectX.TopSelectedRecords <> ASelectY.TopSelectedRecords) or (ASelectX.SkipSelectedRecords <> ASelectY.SkipSelectedRecords) then
          Exit(False);
        AXGroupConditionSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, ASelectX.GroupCondition);
        try
          AYGroupConditionSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, ASelectY.GroupCondition);
          try
            if not PrepareCacheForConversion(ACompareCache, AXGroupConditionSubNodes, AYGroupConditionSubNodes) then
              Exit(False);
          finally
            AYGroupConditionSubNodes.Free;
          end;
        finally
          AXGroupConditionSubNodes.Free;
        end;
        if ASelectX.GroupProperties.Count <> ASelectY.GroupProperties.Count then
          Exit(False);
        APropertiesCount := ASelectX.GroupProperties.Count;
        for I := 0 to APropertiesCount - 1 do
        begin
          AYOperandsSubNodes := nil;
          AXOperandsSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, ASelectX.GroupProperties[I]);
          AYOperandsSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, ASelectY.GroupProperties[I]);
          try
            if not PrepareCacheForConversion(ACompareCache, AXOperandsSubNodes, AYOperandsSubNodes) then
              Exit(False);
          finally
            AXOperandsSubNodes.Free;
            AYOperandsSubNodes.Free;
          end;
        end;
        if ASelectX.SortProperties.Count <> ASelectY.SortProperties.Count then
          Exit(False);
        ASortPropertiesCount := ASelectX.SortProperties.Count;
        for I := 0 to ASortPropertiesCount - 1 do
        begin
          AYOperandsSubNodes := nil;
          AXOperandsSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, ASelectX.SortProperties[I].&Property);
          AYOperandsSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, ASelectY.SortProperties[I].&Property);
          try
            if not PrepareCacheForConversion(ACompareCache, AXOperandsSubNodes, AYOperandsSubNodes) then
              Exit(False);
          finally
            AXOperandsSubNodes.Free;
            AYOperandsSubNodes.Free;
          end;
        end;
      end;
    end;
    Result := True;
  end
  else
    Result := False;
end;

class function TdxOuterApplyCacheCompareHelper.PrepareCacheForConversion(ACompareCache: TdxOuterApplyCompareCache;
  AFromSubNodes: TdxJoinNodeCollection; AToSubNodes: TdxJoinNodeCollection): Boolean;
var
  I: Integer;
begin
  if AFromSubNodes <> AToSubNodes then
  begin
    if (AFromSubNodes = nil) or (AToSubNodes = nil) or (AFromSubNodes.Count <> AToSubNodes.Count) then
      Exit(False);
    for I := 0 to AFromSubNodes.Count - 1 do
      if not PrepareCacheForConversion(ACompareCache, AFromSubNodes[I], AToSubNodes[I]) then
        Exit(False);
  end;
  Result := True;
end;

class function TdxOuterApplyCacheCompareHelper.AreEquals(ACompareCache: TdxOuterApplyCompareCache;
  X, Y: TdxJoinNode; AOperateAsJoinNode: Boolean): Boolean;
var
  ABaseX, ABaseY: TdxBaseStatement;
  ASelectX, ASelectY: TdxSelectStatement;
  AXGroupConditionSubNodes, AYGroupConditionSubNodes: TdxJoinNodeCollection;
  ANextCacheNodeAlias: string;
  AXConditionSubNodes, AYConditionSubNodes, AXOperandsSubNodes, AYOperandsSubNodes: TdxJoinNodeCollection;
  AOperandsCount, I, APropertiesCount, ASortPropertiesCount: Integer;
begin
  if X = Y then
    Exit(True);
  if (X = nil) or (Y = nil) then
    Exit(False);
  ABaseX := Safe<TdxBaseStatement>.Cast(X);
  ABaseY := Safe<TdxBaseStatement>.Cast(Y);
  if (((ABaseX = nil) and (ABaseY <> nil))) or (((ABaseX <> nil) and (ABaseY = nil))) then
    Exit(False);
  ASelectX := Safe<TdxSelectStatement>.Cast(X);
  ASelectY := Safe<TdxSelectStatement>.Cast(Y);
  if ((ASelectX = nil) and (ASelectY <> nil)) or ((ASelectX <> nil) and (ASelectY = nil)) then
    Exit(False);
  if not ((X.&Type = Y.&Type) and AreEquals(ACompareCache, X.Table, Y.Table)) then
    Exit(False);
  ANextCacheNodeAlias := ACompareCache.GetNextCacheAlias;
  ACompareCache.AddNode(X.Alias, ANextCacheNodeAlias);
  ACompareCache.AddNode(Y.Alias, ANextCacheNodeAlias);
  if not AreEquals(ACompareCache, X.SubNodes, Y.SubNodes) then
    Exit(False);
  AXConditionSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, X.Condition);
  try
    AYConditionSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, Y.Condition);
    try
      if (not AreEquals(ACompareCache, AXConditionSubNodes, AYConditionSubNodes)) or
        (TdxOuterApplyCacheCriteriaPreprocessor.Preprocess(ACompareCache, X.Condition) <> TdxOuterApplyCacheCriteriaPreprocessor.Preprocess(ACompareCache, Y.Condition)) then
        Exit(False);
    finally
      AYConditionSubNodes.Free;
    end;
  finally
    AXConditionSubNodes.Free;
  end;
  if ((ABaseX <> nil) and (ABaseY <> nil)) and not AOperateAsJoinNode then
  begin
    if ABaseX.Operands.Count <> ABaseY.Operands.Count then
      Exit(False);
    AOperandsCount := ABaseX.Operands.Count;
    for I := 0 to AOperandsCount - 1 do
    begin
      AXOperandsSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, ABaseX.Operands[I]);
      try
        AYOperandsSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, ABaseY.Operands[I]);
        try
          if (not AreEquals(ACompareCache, AXOperandsSubNodes, AYOperandsSubNodes)) or
            (TdxOuterApplyCacheCriteriaPreprocessor.Preprocess(ACompareCache, ABaseX.Operands[I]) <> TdxOuterApplyCacheCriteriaPreprocessor.Preprocess(ACompareCache, ABaseY.Operands[I])) then
            Exit(False);
        finally
          AYOperandsSubNodes.Free;
        end;
      finally
        AXOperandsSubNodes.Free;
      end;
    end;
    if (ASelectX <> nil) and (ASelectY <> nil) then
    begin
      if (ASelectX.TopSelectedRecords <> ASelectY.TopSelectedRecords) or (ASelectX.SkipSelectedRecords <> ASelectY.SkipSelectedRecords) then
        Exit(False);
      AXGroupConditionSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, ASelectX.GroupCondition);
      try
        AYGroupConditionSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, ASelectY.GroupCondition);
        try
          if (not AreEquals(ACompareCache, AXGroupConditionSubNodes, AYGroupConditionSubNodes)) or
            (TdxOuterApplyCacheCriteriaPreprocessor.Preprocess(ACompareCache, ASelectX.GroupCondition) <> TdxOuterApplyCacheCriteriaPreprocessor.Preprocess(ACompareCache, ASelectY.GroupCondition)) then
            Exit(False);
        finally
          AYGroupConditionSubNodes.Free;
        end;
      finally
        AXGroupConditionSubNodes.Free;
      end;
      if ASelectX.GroupProperties.Count <> ASelectY.GroupProperties.Count then
        Exit(False);
      APropertiesCount := ASelectX.GroupProperties.Count;
      for I := 0 to APropertiesCount - 1 do
      begin
        AXOperandsSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, ASelectX.GroupProperties[I]);
        try
          AYOperandsSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, ASelectY.GroupProperties[I]);
          try
            if (not AreEquals(ACompareCache, AXOperandsSubNodes, AYOperandsSubNodes)) or
              (TdxOuterApplyCacheCriteriaPreprocessor.Preprocess(ACompareCache, ASelectX.GroupProperties[I]) <> TdxOuterApplyCacheCriteriaPreprocessor.Preprocess(ACompareCache, ASelectY.GroupProperties[I])) then
              Exit(False);
          finally
            AYOperandsSubNodes.Free;
          end;
        finally
          AXOperandsSubNodes.Free;
        end;
      end;
      if ASelectX.SortProperties.Count <> ASelectY.SortProperties.Count then
        Exit(False);
      ASortPropertiesCount := ASelectX.SortProperties.Count;
      for I := 0 to ASortPropertiesCount - 1 do
      begin
        AXOperandsSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, ASelectX.SortProperties[I].&Property);
        try
          AYOperandsSubNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache, ASelectY.SortProperties[I].&Property);
          try
            if (not AreEquals(ACompareCache, AXOperandsSubNodes, AYOperandsSubNodes)) or
              (ASelectX.SortProperties[I].SortDirection <> ASelectY.SortProperties[I].SortDirection) or
              (TdxOuterApplyCacheCriteriaPreprocessor.Preprocess(ACompareCache, ASelectX.SortProperties[I].&Property) <>
               TdxOuterApplyCacheCriteriaPreprocessor.Preprocess(ACompareCache, ASelectY.SortProperties[I].&Property)) then
              Exit(False);
          finally
            AYOperandsSubNodes.Free;
          end;
        finally
          AXOperandsSubNodes.Free;
        end;
      end;
    end;
  end;
  Result := True;
end;

class function TdxOuterApplyCacheCompareHelper.AreEquals(ACompareCache: TdxOuterApplyCompareCache; ATable1: TdxDBTable; ATable2: TdxDBTable): Boolean;
var
  AProjection1, AProjection2: TdxDBProjection;
  AColumn1, AColumn2: TdxDBColumn;
  I: Integer;
begin
  if ATable1 = ATable2 then
    Exit(True);
  if (ATable1 = nil) or (ATable2 = nil) then
    Exit(False);
  AProjection1 := Safe<TdxDBProjection>.Cast(ATable1);
  AProjection2 := Safe<TdxDBProjection>.Cast(ATable2);
  if (AProjection1 = nil) and (AProjection2 = nil) then
    Exit(ATable1.Equals(ATable2));
  if (AProjection1 = nil) or (AProjection2 = nil) or (AProjection1.Columns.Count <> AProjection2.Columns.Count) then
    Exit(False);
  for I := 0 to AProjection1.Columns.Count - 1 do
  begin
    AColumn1 := AProjection1.Columns[I];
    AColumn2 := AProjection2.Columns[I];
    if AColumn1 = AColumn2 then
      Continue;
    if (AColumn1 = nil) or (AColumn2 = nil) then
      Exit(False);
    if AColumn1.Name <> AColumn2.Name then
      Exit(False);
  end;
  Result := AreEquals(ACompareCache, AProjection1.Projection, AProjection2.Projection);
end;

class function TdxOuterApplyCacheCompareHelper.GetHashCode(const AInterface: IInterface): Integer;
begin
  if AInterface = nil then
    Result := 0
  else
    Result := (AInterface as TObject).GetHashCode;
end;

class function TdxOuterApplyCacheCompareHelper.GetHash(AObject: TdxJoinNode; AOperateAsJoinNode: Boolean): Integer;
var
  AHash, I: Integer;
  ABaseStatement: TdxBaseStatement;
  ASelectStatement: TdxSelectStatement;
  AJoinNodes: TdxJoinNodeCollection;
  AOperand: IdxCriteriaOperator;
  ASortingColumn: TdxSortingColumn;
begin
  if AObject = nil then
    Exit($64323421);
  AHash := Integer(AObject.&Type);
  if AObject.Table is TdxDBProjection then
    AHash := AHash xor GetHash(TdxDBProjection(AObject.Table))
  else
    if AObject.Table <> nil then
      AHash := AHash xor AObject.Table.GetHashCode;
  AHash := AHash xor GetHash(AObject.SubNodes);
  AJoinNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(nil, AObject.Condition);
  try
    AHash := AHash xor GetHash(AJoinNodes);
  finally
    AJoinNodes.Free;
  end;
  AHash := AHash xor (GetHashCode(TdxOuterApplyCacheCriteriaPreprocessor.Preprocess(nil, AObject.Condition))) xor
    Integer(AObject.&Type);

  if AOperateAsJoinNode then
    Exit(AHash);
  ABaseStatement := Safe<TdxBaseStatement>.Cast(AObject);
  if ABaseStatement = nil then
    Exit(AHash);
  for I := 0 to ABaseStatement.Operands.Count - 1 do
  begin
    AJoinNodes := TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(nil, ABaseStatement.Operands[I]);
    try
      AHash := AHash xor GetHash(AJoinNodes);
    finally
      AJoinNodes.Free;
    end;
    AHash := AHash xor GetHashCode(TdxOuterApplyCacheCriteriaPreprocessor.Preprocess(nil, ABaseStatement.Operands[I]));
  end;
  ASelectStatement := Safe<TdxSelectStatement>.Cast(ABaseStatement);
  if ASelectStatement = nil then
    Exit(AHash);
  if ASelectStatement.GroupProperties <> nil then
  begin
    AHash := AHash xor ASelectStatement.GroupProperties.Count;
    for AOperand in ASelectStatement.GroupProperties do
      AHash := AHash xor GetHashCode(TdxOuterApplyCacheCriteriaPreprocessor.Preprocess(nil, AOperand));
  end;
  if ASelectStatement.SortProperties <> nil then
  begin
    AHash := AHash xor ASelectStatement.SortProperties.Count;
    for ASortingColumn in ASelectStatement.SortProperties do
      AHash := AHash xor GetHashCode(TdxOuterApplyCacheCriteriaPreprocessor.Preprocess(nil, ASortingColumn.&Property)) xor
        Integer(ASortingColumn.SortDirection);
  end;
  AHash := AHash xor GetHashCode(TdxOuterApplyCacheCriteriaPreprocessor.Preprocess(nil, ASelectStatement.GroupCondition));
  Result := AHash xor ASelectStatement.SkipSelectedRecords xor ASelectStatement.TopSelectedRecords;
end;

class function TdxOuterApplyCacheCompareHelper.GetHash(ADbProjection: TdxDBProjection): Integer;
var
  AHash: Integer;
  AColumn: TdxDBColumn;
begin
  if ADbProjection = nil then
    Exit($78422058);
  AHash := (GetValueHash(ADbProjection.Name)) xor (GetHash(ADbProjection.Projection));
  AHash := AHash xor ADbProjection.Columns.Count;
  for AColumn in ADbProjection.Columns do
  begin
    if AColumn = nil then
      Continue;
    AHash := AHash xor GetValueHash(AColumn.Name);
  end;
  Result := AHash;
end;

class function TdxOuterApplyCacheCompareHelper.AreEquals(ACompareCache: TdxOuterApplyCompareCache; X, Y: TdxJoinNodeCollection): Boolean;
var
  I: Integer;
begin
  if X = Y then
    Exit(True);
  if (X = nil) or (Y = nil) then
    Exit(False);
  if X.Count <> Y.Count then
    Exit(False);
  for I := 0 to X.Count - 1 do
    if not AreEquals(ACompareCache, X[I], Y[I]) then
      Exit(False);
  Result := True;
end;

class function TdxOuterApplyCacheCompareHelper.GetHash(AObject: TdxJoinNodeCollection): Integer;
var
  O: TdxJoinNode;
begin
  Result := $1259321;
  if AObject <> nil then
    for O in AObject do
      Result := Result xor GetHash(O);
end;

{ TdxOuterApplyCacheCriteriaPreprocessor }

constructor TdxOuterApplyCacheCriteriaPreprocessor.Create(ACompareCache: TdxOuterApplyCompareCache; ACollectNodes: Boolean; AConvertNodes: Boolean);
begin
  inherited Create;
  FSubNodes := nil;
  FCache := ACompareCache;
  FCollectNodes := ACollectNodes;
  FConvertNodes := AConvertNodes;
end;

function TdxOuterApplyCacheCriteriaPreprocessor.Visit(const AOperand: IdxQueryOperand): IdxCriteriaOperator;
var
  ACacheAliasName: string;
  AParentCriteria: IdxParentCriteria;
  ACriteria: IdxCriteriaOperator;
begin
  if FCollectNodes then
    Exit(nil);
  Supports(AOperand, IdxParentCriteria, AParentCriteria);
  ACriteria := AParentCriteria.ParentCriteria;
  if FCache = nil then
    Exit(TdxQueryOperand.Create(AOperand.ColumnName, '', AOperand.ColumnType, ACriteria));
  ACacheAliasName := FCache.GetCacheAliasName(AOperand.NodeAlias);
  if ACacheAliasName = AOperand.NodeAlias then
    Result := AOperand
  else
    Result := TdxQueryOperand.Create(AOperand.ColumnName, ACacheAliasName, AOperand.ColumnType, ACriteria);
end;

function TdxOuterApplyCacheCriteriaPreprocessor.Visit(const AOperand: IdxQuerySubQueryContainer): IdxCriteriaOperator;
var
  AAggregateProperty: IdxCriteriaOperator;
  ANode: TdxBaseStatement;
begin
  if FCollectNodes then
  begin
    if FSubNodes = nil then
      FSubNodes := TdxJoinNodeCollection.Create;
    FSubNodes.Add(AOperand.Node);
  end;
  AAggregateProperty := Process(AOperand.AggregateProperty);
  if FCollectNodes then
    Exit(nil);
  if not FConvertNodes then
    Exit(TdxQuerySubQueryContainer.Create(nil, AAggregateProperty, AOperand.AggregateFunctionType));
  ANode := Process(AOperand.Node);
  if (ANode = AOperand.Node) and (AAggregateProperty = AOperand.AggregateProperty) then
    Result := AOperand
  else
    Result := TdxQuerySubQueryContainer.Create(ANode, AAggregateProperty, AOperand.AggregateFunctionType);
end;

function TdxOuterApplyCacheCriteriaPreprocessor.Process(ANode: TdxBaseStatement): TdxBaseStatement;
var
  ASrcStatement, ADestStatement: TdxSelectStatement;
  AHasModifications: Boolean;
  ASrcOperand, ADestOperand: IdxCriteriaOperator;
begin
  if ANode = nil then
    Exit(nil);
  ASrcStatement := Safe<TdxSelectStatement>.Cast(ANode);
  if ASrcStatement = nil then
    raise EInvalidOperation.Create('');
  ADestStatement := TdxSelectStatement.Create;
  AHasModifications := Process(ASrcStatement, ADestStatement);
  for ASrcOperand in ASrcStatement.Operands do
  begin
    ADestOperand := Process(ASrcOperand);
    if ADestOperand = ASrcOperand then
      ADestStatement.Operands.Add(ASrcOperand)
    else
    begin
      ADestStatement.Operands.Add(ADestOperand);
      AHasModifications := True;
    end;
  end;
  ADestStatement.GroupCondition := Process(ASrcStatement.GroupCondition);
  if ADestStatement.GroupCondition <> ASrcStatement.GroupCondition then
    AHasModifications := True;
  for ASrcOperand in ASrcStatement.GroupProperties do
  begin
    ADestOperand := Process(ASrcOperand);
    if ADestOperand = ASrcOperand then
      ADestStatement.GroupProperties.Add(ASrcOperand)
    else
    begin
      ADestStatement.GroupProperties.Add(ADestOperand);
      AHasModifications := True;
    end;
  end;
  ADestStatement.SkipSelectedRecords := ASrcStatement.SkipSelectedRecords;
  ADestStatement.TopSelectedRecords := ASrcStatement.TopSelectedRecords;
  if AHasModifications then
    Result := ADestStatement
  else
    Result := ASrcStatement;
end;

function TdxOuterApplyCacheCriteriaPreprocessor.Process(ASrcNode: TdxJoinNode): TdxJoinNode;
var
  ADestNode: TdxJoinNode;
begin
  if ASrcNode = nil then
    Exit(nil);
  ADestNode := TdxJoinNode.Create;
  try
    if Process(ASrcNode, ADestNode) then
    begin
      Result := ADestNode;
      ADestNode := nil;
    end
    else
      Result := ASrcNode;
  finally
    ADestNode.Free;
  end;
end;

function TdxOuterApplyCacheCriteriaPreprocessor.Process(ASrcNode, ADestNode: TdxJoinNode): Boolean;
var
  AHasModifications: Boolean;
  ASrcSubNode, ADestSubNode: TdxJoinNode;
begin
  ADestNode.Table := ASrcNode.Table;
  ADestNode.Alias := FCache.GetCacheAliasName(ASrcNode.Alias);
  ADestNode.Condition := Process(ASrcNode.Condition);
  ADestNode.&Type := ASrcNode.&Type;
  AHasModifications := False;
  for ASrcSubNode in ASrcNode.SubNodes do
  begin
    ADestSubNode := Process(ASrcSubNode);
    if ADestSubNode = ASrcSubNode then
      ADestNode.SubNodes.Add(ASrcSubNode)
    else
    begin
      ADestNode.SubNodes.Add(ADestSubNode);
      AHasModifications := True;
    end;
  end;
  Result := AHasModifications or (ADestNode.Alias <> ASrcNode.Alias) or
    (ADestNode.Condition <> ASrcNode.Condition) or (ADestNode.&Type <> ASrcNode.&Type);
end;

class function TdxOuterApplyCacheCriteriaPreprocessor.Preprocess(ACompareCache: TdxOuterApplyCompareCache;
  const ACriteria: IdxCriteriaOperator): IdxCriteriaOperator;
var
  APreprocessor: TdxOuterApplyCacheCriteriaPreprocessor;
begin
  APreprocessor := TdxOuterApplyCacheCriteriaPreprocessor.Create(ACompareCache, False, False);
  try
    Result := APreprocessor.Process(ACriteria);
  finally
    APreprocessor.Free;
  end;
end;

class function TdxOuterApplyCacheCriteriaPreprocessor.Convert(ACompareCache: TdxOuterApplyCompareCache;
  const ACriteria: IdxCriteriaOperator): IdxCriteriaOperator;
var
  APreprocessor: TdxOuterApplyCacheCriteriaPreprocessor;
begin
  APreprocessor := TdxOuterApplyCacheCriteriaPreprocessor.Create(ACompareCache, False, True);
  try
    Result := APreprocessor.Process(ACriteria);
  finally
    APreprocessor.Free;
  end;
end;

class function TdxOuterApplyCacheCriteriaPreprocessor.CollectNodes(ACompareCache: TdxOuterApplyCompareCache;
  const ACriteria: IdxCriteriaOperator): TdxJoinNodeCollection;
var
  APreprocessor: TdxOuterApplyCacheCriteriaPreprocessor;
begin
  APreprocessor := TdxOuterApplyCacheCriteriaPreprocessor.Create(ACompareCache, True, False);
  try
    APreprocessor.Process(ACriteria);
    Result := APreprocessor.SubNodes;
  finally
    APreprocessor.Free;
  end;
end;

{ TdxOuterApplyAggregateInfo }

constructor TdxOuterApplyAggregateInfo.Create(ANode: TdxBaseStatement; AType: TdxAggregateFunctionType);
begin
  Node := ANode;
  &Type := AType;
end;

{ TdxOuterApplyInfo.TComparer }

function TdxOuterApplyInfo.TComparer.Equals(const ALeft, ARight: TdxOuterApplyAggregateInfo): Boolean;
begin
  Result := (ALeft.&Type = ARight.&Type) and
    TdxOuterApplyCacheCompareHelper.AreEquals(TdxOuterApplyCompareCache.Create, ALeft.Node, ARight.Node);
end;

function TdxOuterApplyInfo.TComparer.GetHashCode(const AValue: TdxOuterApplyAggregateInfo): Integer;
begin
  Result := TdxOuterApplyCacheCompareHelper.GetHash(AValue.Node) xor Integer(AValue.&Type);
end;

{ TdxOuterApplyInfo }

constructor TdxOuterApplyInfo.Create(const AAlias: string; ATargetNode: TdxBaseStatement; const AFirstSqlBody: string);
begin
  inherited Create;
  if FComparer = nil then
    FComparer := TComparer.Create;
  FAggregates := TDictionary<TdxOuterApplyAggregateInfo, string>.Create(FComparer);
  FAlias := AAlias;
  FTargetNode := ATargetNode;
  FFirstSqlBody := AFirstSqlBody;
end;

destructor TdxOuterApplyInfo.Destroy;
begin
  FreeAndNil(FAggregates);
  inherited Destroy;
end;

class destructor TdxOuterApplyInfo.Destroy;
begin
  FreeAndNil(FComparer);
end;

function TdxOuterApplyInfo.GetAggregateAlias(AAggregateNode: TdxBaseStatement; AAggregateType: TdxAggregateFunctionType): string;
var
  APropertyInfo: TdxOuterApplyAggregateInfo;
  AAlias: string;
begin
  APropertyInfo := TdxOuterApplyAggregateInfo.Create(AAggregateNode, AAggregateType);
  if not FAggregates.TryGetValue(APropertyInfo, AAlias) then
  begin
    AAlias := 'Res' + IntToStr(FAggregates.Count);
    FAggregates.Add(APropertyInfo, AAlias);
  end;
  Result := AAlias;
end;

function TdxOuterApplyInfo.GenerateOuterApplySqlBody(AParentGenerator: TdxBaseSqlGenerator;
  AFormatter: TdxSQLConnectionProvider): string;
var
  ASubSelectSqlGenerator: TdxSubSelectSqlGenerator;
  ASubSelectAggregateInfos: TArray<TdxSubSelectAggregateInfo>;
  AItem: TPair<TdxOuterApplyAggregateInfo, string>;
  AAggregateProperty: IdxCriteriaOperator;
  ACache: TdxOuterApplyCompareCache;
  I: Integer;
begin
  if (FAggregates.Count < 2) and (FFirstSqlBody <> '') then
    Exit(FFirstSqlBody);
  SetLength(ASubSelectAggregateInfos, FAggregates.Count);
  I := 0;
  for AItem in FAggregates do
  begin
    AAggregateProperty := TdxBaseSQLGenerator.GetNodeSingleProperty(AItem.Key.Node);
    if AAggregateProperty <> nil then
    begin
      ACache := TdxOuterApplyCompareCache.Create;
      try
        TdxOuterApplyCacheCompareHelper.PrepareCacheForConversion(ACache, AItem.Key.Node, FTargetNode);
        AAggregateProperty := TdxOuterApplyCacheCriteriaPreprocessor.Convert(ACache, AAggregateProperty);
      finally
        ACache.Free;
      end;
    end;
    ASubSelectAggregateInfos[I] := TdxSubSelectAggregateInfo.Create(AAggregateProperty, AItem.Key.&Type, AItem.Value);
    Inc(I);
  end;
  ASubSelectSqlGenerator := TdxSubSelectSqlGenerator.Create(AParentGenerator, AFormatter, ASubSelectAggregateInfos, True);
  try
    Result := ASubSelectSqlGenerator.GenerateSelect(FTargetNode, False);
  finally
    ASubSelectSqlGenerator.Free;
  end;
end;

{ TdxOuterApplyCacheItem }

constructor TdxOuterApplyCacheItem.Create(ANode: TdxJoinNode; AOperateAsJoinNode: Boolean);
begin
  FNode := ANode;
  FOperateAsJoinNode := AOperateAsJoinNode;
  FHash := TdxOuterApplyCacheCompareHelper.GetHash(ANode, AOperateAsJoinNode);
end;

function TdxOuterApplyCacheItem.Equals(const AValue: TdxOuterApplyCacheItem): Boolean;
begin
  Result := TdxOuterApplyCacheCompareHelper.AreEquals(TdxOuterApplyCompareCache.Create, Node, AValue.Node, FOperateAsJoinNode);
end;

{ TdxOuterApplyCacheItem.TComparer }

function TdxOuterApplyCacheItem.TComparer.Equals(const ALeftItem, ARightItem: TdxOuterApplyCacheItem): Boolean;
begin
  Result := ALeftItem.Equals(ARightItem);
end;

function TdxOuterApplyCacheItem.TComparer.GetHashCode(const AValue: TdxOuterApplyCacheItem): Integer;
begin
  Result := AValue.Hash;
end;

{ TdxBaseSQLGenerator }

constructor TdxBaseSQLGenerator.Create(const AFormatter: TdxSQLConnectionProvider);
begin
  inherited Create;
  FFormatter := AFormatter;
end;

destructor TdxBaseSQLGenerator.Destroy;
begin
  FreeAndNil(FOuterApplyCache);
  FreeAndNil(FJoins);
  inherited Destroy;
end;

function TdxBaseSQLGenerator.GetOuterApplyCache: TDictionary<TdxOuterApplyCacheItem, TdxOuterApplyInfo>;
begin
  if FOuterApplyCache = nil then
    FOuterApplyCache := TDictionary<TdxOuterApplyCacheItem, TdxOuterApplyInfo>.Create(TdxOuterApplyCacheItem.TComparer.Create);
  Result := FOuterApplyCache;
end;

function TdxBaseSQLGenerator.GetUseOuterApply: Boolean;
begin
  Result := False;
end;

function TdxBaseSQLGenerator.GetIsSubQuery: Boolean;
begin
  Result := False;
end;

function TdxBaseSQLGenerator.GetForceOuterApply: Boolean;
begin
  Result := FInTopLevelAggregate;
end;

procedure TdxBaseSQLGenerator.SetUpRootQueryStatement(ARoot: TdxBaseStatement);
begin
  FRoot := ARoot;
end;

function TdxBaseSQLGenerator.ProcessParameter(const AOperand: IdxCriteriaOperator; ANullOnNull: Boolean): string;
begin
  if AOperand = nil then
  begin
    if ANullOnNull then
      Exit('')
    else
      raise EInvalidOperation.Create(sdxEmptySubCriteria);
  end;
  Result := (AOperand as TdxCriteriaOperator).Accept(Self);
end;

function TdxBaseSQLGenerator.Process(const AOperand: IdxCriteriaOperator): string;
begin
  Result := ProcessParameter(AOperand, False);
end;

function TdxBaseSQLGenerator.GetNextOuterApplyAlias: string;
begin
  Inc(FOuterApplyAliasCounter);
  Result := Format('OA%d', [FOuterApplyAliasCounter]);
end;

class function TdxBaseSQLGenerator.GetNodeSingleProperty(ANode: TdxBaseStatement): IdxCriteriaOperator;
begin
 if ANode.Operands.Count > 0 then
    Result := ANode.Operands[0]
  else
    Result := nil;
end;

procedure TdxBaseSQLGenerator.AppendJoinNode(ANode: TdxJoinNode; AJoins: TStringBuilder);
var
  AProjection: TdxDBProjection;
  AGenerator: TdxSelectSQLGenerator;
  AProjectionQuery: TdxQuery;
  ASubNode: TdxJoinNode;
  AOnStatement: string;
begin
  if FFormatter.BraceJoin then
    AJoins.Insert(0, '(');
  AJoins.Append(#10' ');
  AJoins.Append(IfThen(ANode.&Type = TdxJoinType.Inner, 'inner', 'left'));
  AJoins.Append(' join ');
  AProjection := Safe<TdxDBProjection>.Cast(ANode.Table);
  if AProjection <> nil then
  begin
    AGenerator := TdxSelectSQLGenerator.Create(FFormatter, Self, AProjection.Columns.GetNames);
    try
      AProjectionQuery := AGenerator.GenerateSQL(AProjection.Projection);
      try
        AJoins.Append(FormatSubQuery(AProjectionQuery.SQLText, ANode.Alias));
      finally
        AProjectionQuery.Free;
      end;
    finally
      AGenerator.Free;
    end;
  end
  else
    AJoins.Append(FFormatter.FormatTable(FFormatter.ComposeSafeSchemaName(ANode.Table.Name),
      FFormatter.ComposeSafeTableName(ANode.Table.Name), ANode.Alias));
  AJoins.Append(' on ');
  AOnStatement := Process(ANode.Condition);
  if AOnStatement = '' then
    AOnStatement := Process(TdxBinaryOperator.Create(TdxConstantValue.Create(1), TdxConstantValue.Create(1), TdxBinaryOperatorType.Equal));
  AJoins.Append(AOnStatement);
  if FFormatter.BraceJoin then
    AJoins.Append(')');
  for ASubNode in ANode.SubNodes do
    AppendJoinNode(ASubNode, AJoins);
end;

function TdxBaseSQLGenerator.BuildJoins: TStringBuilder;
var
  AProjection: TdxDBProjection;
  AGenerator: TdxSelectSQLGenerator;
  AProjectionQuery: TdxQuery;
  ASubNode: TdxJoinNode;
begin
  if FJoins = nil then
    FJoins := TStringBuilder.Create
  else
    FJoins.Clear;
  AProjection := Safe<TdxDBProjection>.Cast(Root.Table);
  if AProjection <> nil then
  begin
    AGenerator := TdxSelectSQLGenerator.Create(FFormatter, Self, AProjection.Columns.GetNames);
    try
      AProjectionQuery := AGenerator.GenerateSQL(AProjection.Projection);
      FJoins.Append(FormatSubQuery(AProjectionQuery.SQLText, Root.Alias));
    finally
      AGenerator.Free;
    end;
  end
  else
    FJoins.Append(FFormatter.FormatTable(FFormatter.ComposeSafeSchemaName(Root.Table.Name),
      FFormatter.ComposeSafeTableName(Root.Table.Name), Root.Alias));
  for ASubNode in Root.SubNodes do
    AppendJoinNode(ASubNode, FJoins);
  Result := FJoins;
end;

function TdxBaseSQLGenerator.BuildOuterApply: string;
var
  AResult: TStringBuilder;
  AOuterApply: TdxOuterApplyInfo;
begin
  if UseOuterApply then
  begin
    if FFormatter.SupportsNativeOuterApply and (OuterApplyCache.Count > 0) then
    begin
      AResult := TStringBuilder.Create;
      try
        for AOuterApply in OuterApplyCache.Values do
          AResult.Append(#10).
            Append(FFormatter.FormatOuterApply(AOuterApply.GenerateOuterApplySqlBody(Self, FFormatter), AOuterApply.Alias)).
            Append(' ');
        AResult.Append(#10);
        Result := AResult.ToString;
      finally
        AResult.Free;
      end;
    end;
  end
  else
    Result := '';
end;

function TdxBaseSQLGenerator.BuildCriteria: string;
begin
  Result := ProcessParameter(Root.Condition as TdxCriteriaOperator, True);
end;

function TdxBaseSQLGenerator.Visit(const AOperand: IdxOperandValue): string;
begin
  Result := GetNextParameterName(AOperand);
end;

function TdxBaseSQLGenerator.Visit(const AOperand: IdxQueryOperand): string;
begin
  if AOperand.NodeAlias = '' then
    Result := FFormatter.FormatColumn(FFormatter.ComposeSafeColumnName(AOperand.ColumnName))
  else
    Result := FFormatter.FormatColumn(FFormatter.ComposeSafeColumnName(AOperand.ColumnName), AOperand.NodeAlias);
end;

function TdxBaseSQLGenerator.Visit(const AOperator: IdxBetweenOperator): string;
begin
  Result := Process(TdxGroupOperator.And(
    TdxBinaryOperator.Create(AOperator.BeginExpression, AOperator.TestExpression, TdxBinaryOperatorType.LessOrEqual),
    TdxBinaryOperator.Create(AOperator.TestExpression, AOperator.EndExpression, TdxBinaryOperatorType.LessOrEqual)) as TdxCriteriaOperator);
end;

function TdxBaseSQLGenerator.Visit(const AOperator: IdxBinaryOperator): string;
begin
  Result := FFormatter.FormatBinary(AOperator.OperatorType, Process(AOperator.LeftOperand as TdxCriteriaOperator), Process(AOperator.RightOperand as TdxCriteriaOperator));
end;

function TdxBaseSQLGenerator.Visit(const AOperator: IdxInOperator): string;
var
  ALeft: string;
  AInString: TStringBuilder;
  AValue: IdxCriteriaOperator;
  I: Integer;
begin
  ALeft := Process(AOperator.LeftOperand);
  AInString := TStringBuilder.Create;
  try
    AInString.Append(ALeft).Append(' in (');
    for I := 0 to AOperator.Operands.Count - 1 do
    begin
      AValue := AOperator.Operands[I];
      if I > 0 then
        AInString.Append(',');
      AInString.Append(Process(AValue as TdxCriteriaOperator));
    end;
    AInString.Append(')');
    Result := AInString.ToString;
  finally
    AInString.Free;
  end;
end;

function TdxBaseSQLGenerator.Visit(const AOperator: IdxGroupOperator): string;
var
  AResult: TStringBuilder;
  ADelimiter: string;
  ACriteriaOperator: IdxCriteriaOperator;
  ACriteria: string;
  I, AHigh: Integer;
begin
  AResult := TStringBuilder.Create;
  try
    ADelimiter := ' ' + GroupOperatorName[AOperator.OperatorType] + ' ';
    AResult.Append('(');
    AHigh := AOperator.Operands.Count - 1;
    for I := 0 to AHigh do
    begin
      ACriteriaOperator := AOperator.Operands[I];
      ACriteria := Process(ACriteriaOperator as TdxCriteriaOperator);
      if ACriteria = '' then
        Continue;
      AResult.Append(ACriteria);
      if I <> AHigh then
        AResult.Append(ADelimiter);
    end;
    AResult.Append(')');
    Result := AResult.ToString;
  finally
    AResult.Free;
  end;
end;

function TdxBaseSQLGenerator.Visit(const AOperator: IdxUnaryOperator): string;
begin
  Result := FFormatter.FormatUnary(AOperator.OperatorType, Process(AOperator.Operand as TdxCriteriaOperator));
end;

function TdxBaseSQLGenerator.Visit(const AOperator: IdxFunctionOperator): string;
var
  AOperands: TArray<TValue>;
  AStringOperands: TArray<string>;
  I: Integer;
begin
  if TdxEvalHelpers.IsLocalDateTime(AOperator.OperatorType) then
  begin
    if AOperator.Operands.Count <> 0 then
      raise EArgumentException.Create(sdxTheOperatorOperandsCountNot0);
    Exit(Process(TdxConstantValue.Create(TdxEvalHelpers.EvaluateLocalDateTime(AOperator.OperatorType))));
  end;
  if TdxEvalHelpers.IsOutlookInterval(AOperator.OperatorType) then
    Exit(Process(TdxEvalHelpers.ExpandIsOutlookInterval(AOperator)));
  if FFormatter <> nil then
  begin
    SetLength(AOperands, AOperator.Operands.Count);
    for I := 0 to Length(AOperands) - 1 do
      AOperands[I] := AOperator.Operands[I] as TdxCriteriaOperator;
    if AOperator.OperatorType in [TdxFunctionOperatorType.Custom, TdxFunctionOperatorType.CustomNonDeterministic] then
    begin
      if (Length(AOperands) < 1) or (not (AOperator.Operands[0] is TdxOperandValue)) or
        (not ((TdxOperandValue(AOperator.Operands[0])).Value.IsString)) then
        raise Exception.Create('');
      AOperands[0] := (TdxOperandValue(AOperator.Operands[0])).Value;
    end;
    Result := FFormatter.FormatFunction(Process, AOperator.OperatorType, AOperands);
  end
  else
  begin
    SetLength(AStringOperands, AOperator.Operands.Count);
    if AOperator.OperatorType in [TdxFunctionOperatorType.Custom, TdxFunctionOperatorType.CustomNonDeterministic] then
    begin
      I := 1;
      if (Length(AStringOperands) < 1) or (not (AOperator.Operands[0] is TdxOperandValue)) or
        (not (TdxOperandValue(AOperator.Operands[0])).Value.IsString) then
        raise Exception.Create('');
      AStringOperands[0] := TdxOperandValue(AOperator.Operands[0]).Value.AsString;
    end
    else
      I := 0;
    while I < AOperator.Operands.Count do
    begin
      AStringOperands[I] := Process(TdxCriteriaOperator(AOperator.Operands[I]));
      Inc(I);
    end;
    Result := FFormatter.FormatFunction(AOperator.OperatorType, AStringOperands);
  end;
end;

function TdxBaseSQLGenerator.Visit(const AContainer: IdxQuerySubQueryContainer): string;
var
  AMemOuterApplyResultCounter: Integer;
  AIsExists: Boolean;
  AProp: IdxCriteriaOperator;
  ACacheItem: TdxOuterApplyCacheItem;
  AInfo: TdxOuterApplyInfo;
  AOuterGena: TdxSubSelectSQLGenerator;
  ASubQuery, APropertyAlias: string;
  ASubSelectSQLGenerator: TdxSubSelectSQLGenerator;
  AAggregateType: TdxAggregateFunctionType;
begin
  if AContainer.Node = nil then
  begin
    FInTopLevelAggregate := True;
    AMemOuterApplyResultCounter := FOuterApplyResultCounter;
    try
      Exit(TdxSubSelectSQLGenerator.GetSelectValue(AContainer.AggregateProperty, AContainer.AggregateFunctionType, Self, ''));
    finally
      FOuterApplyResultCounter := AMemOuterApplyResultCounter;
      FInTopLevelAggregate := False;
    end;
  end;
  AIsExists := AContainer.AggregateFunctionType = TdxAggregateFunctionType.Exists;
  FHasSubQuery := not AIsExists;
  if (AContainer.Node.Operands.Count > 0) then
    AProp := AContainer.Node.Operands[0]
  else
    AProp := nil;
  if UseOuterApply then
  begin
    if FFormatter.SupportsNativeOuterApply then
    begin
      ACacheItem := TdxOuterApplyCacheItem.Create(AContainer.Node, True);
      if not OuterApplyCache.TryGetValue(ACacheItem, AInfo) then
      begin
        AOuterGena := TdxSubSelectSQLGenerator.Create(Self, FFormatter, AProp, AContainer.AggregateFunctionType, not AIsExists);
        try
          ASubQuery := AOuterGena.GenerateSelect(AContainer.Node, False);
          if (not AOuterGena.HasSubQuery and not (IsSubQuery and not AIsExists)) and not ForceOuterApply then
            Exit(FormatSubQuery(AContainer.AggregateFunctionType, ASubQuery));
        finally
          AOuterGena.Free;
        end;
        FHasSubQuery := True;
        if AIsExists then
          ASubQuery := '';
        AInfo := TdxOuterApplyInfo.Create(GetNextOuterApplyAlias, AContainer.Node, ASubQuery);
        OuterApplyCache.Add(ACacheItem, AInfo);
      end;
      Inc(FOuterApplyResultCounter);
      if AIsExists then
        AAggregateType := TdxAggregateFunctionType.Count
      else
        AAggregateType := AContainer.AggregateFunctionType;
      APropertyAlias := AInfo.GetAggregateAlias(AContainer.Node, AAggregateType);
      if AIsExists then
        Exit(Concat('(', AInfo.Alias, '.', APropertyAlias, ' > 0)'))
      else
        Exit(Concat(AInfo.Alias, '.', APropertyAlias));
    end;
  end;
  ASubSelectSQLGenerator := TdxSubSelectSQLGenerator.Create(Self, FFormatter, AProp, AContainer.AggregateFunctionType, not AIsExists);
  try
    Result := FormatSubQuery(AContainer.AggregateFunctionType, ASubSelectSQLGenerator.GenerateSelect(AContainer.Node, True));
  finally
    ASubSelectSQLGenerator.Free;
  end;
end;

class function TdxBaseSQLGenerator.FormatSubQuery(AAggregateFunctionType: TdxAggregateFunctionType; const ASubQuery: string): string;
begin
  Result := Format('%s(%s)', [IfThen(AAggregateFunctionType = TdxAggregateFunctionType.Exists, 'exists'), ASubQuery]);
end;

class function TdxBaseSQLGenerator.FormatSubQuery(const ASubQuery: string; const AAlias: string): string;
begin
  Result := Format('(%s) %s', [ASubQuery, AAlias]);
end;

{ TdxBaseSQLGeneratorWithParameters }

constructor TdxBaseSQLGeneratorWithParameters.Create(const AFormatter: TdxSQLConnectionProvider;
  AIdentitiesByTag: TdxTaggedParameterHolder);
begin
  inherited Create(AFormatter);
  FIdentitiesByTag := AIdentitiesByTag;
  FParameters := TDictionary<IdxOperandValue, string>.Create;
end;

destructor TdxBaseSQLGeneratorWithParameters.Destroy;
begin
  FreeAndNil(FQueryParameters);
  FreeAndNil(FQueryParameterNames);
  FreeAndNil(FParameters);
  inherited Destroy;
end;

function TdxBaseSQLGeneratorWithParameters.GetNextParameterName(const AParameter: IdxOperandValue): string;
var
  AName: string;
  ACreateParameter: Boolean;
  AConsolidateParameter: IdxOperandValue;
begin
  AConsolidateParameter := FIdentitiesByTag.ConsolidateParameter(AParameter);
  if not AConsolidateParameter.Value.IsEmpty then
  begin
    if FFormatter.SupportsNamedParameters then
    begin
      if not FParameters.TryGetValue(AConsolidateParameter, AName) then
      begin
        ACreateParameter := True;
        AName := FFormatter.GetParameterName(AConsolidateParameter, FParameters.Count, ACreateParameter);
        if ACreateParameter then
        begin
          FQueryParameterNames.Add(AName);
          AName := ':' + AName;
          FParameters.Add(AConsolidateParameter, AName);
          FQueryParameters.Add(AConsolidateParameter);
        end;
      end;
      Result := AName;
    end
    else
    begin
      ACreateParameter := True;
      AName := FFormatter.GetParameterName(AConsolidateParameter, FQueryParameters.Count, ACreateParameter);
      if ACreateParameter then
      begin
        FQueryParameters.Add(AConsolidateParameter);
        FQueryParameterNames.Add(AName);
        AName := ':' + AName;
      end;
      Result := AName;
    end;
  end
  else
    Result := 'null';
end;

procedure TdxBaseSQLGeneratorWithParameters.SetUpParameters;
begin
  FQueryParameters := TdxQueryParameterCollection.Create;
  FQueryParameterNames := TList<string>.Create;
end;

function TdxBaseSQLGeneratorWithParameters.CreateQuery(const ACommandText: string; AParameters: TdxQueryParameterCollection;
  const AParameterNames: TArray<string>): TdxQuery;
begin
  Result := TdxQuery.Create(ACommandText, AParameters, AParameterNames);
end;

function TdxBaseSQLGeneratorWithParameters.GenerateSQL(ANode: TdxBaseStatement): TdxQuery;
var
  ACommandText: string;
begin
  SetUpRootQueryStatement(ANode);
  SetUpParameters;
  ACommandText := InternalGenerateSQL;
  Result := CreateQuery(ACommandText, FQueryParameters, FQueryParameterNames.ToArray);
end;

{ TdxSelectSQLGenerator }

constructor TdxSelectSQLGenerator.Create(const AFormatter: TdxSQLConnectionProvider);
begin
  inherited Create(AFormatter, CreateIdentities);
  FConstantValues := TDictionary<Integer, IdxOperandValue>.Create;
  FOperandIndexes := TDictionary<Integer, Integer>.Create;
end;

constructor TdxSelectSQLGenerator.Create(const AFormatter: TdxSQLConnectionProvider;
  AParentGenerator: TdxBaseSQLGenerator; const APropertyAliases: TArray<string>);
begin
  Create(AFormatter);
  FParentGenerator := AParentGenerator;
  FPropertyAliases := APropertyAliases;
end;

destructor TdxSelectSQLGenerator.Destroy;
begin
  FreeAndNil(FConstantValues);
  FreeAndNil(FOperandIndexes);
  FreeAndNil(FIdentities);
  FreeAndNil(FGroupProperties);
  inherited Destroy;
end;

function TdxSelectSQLGenerator.GetRoot: TdxSelectStatement;
begin
  Result := TdxSelectStatement(inherited Root);
end;

function TdxSelectSQLGenerator.CreateIdentities: TdxTaggedParameterHolder;
begin
  FIdentities := TdxTaggedParameterHolder.Create;
  Result := FIdentities;
end;

function TdxSelectSQLGenerator.GetUseOuterApply: Boolean;
begin
  Result := True;
end;

function TdxSelectSQLGenerator.BuildSorting: string;
var
  AList: TStringBuilder;
  I, J: Integer;
  ASp: TdxSortingColumn;
  ASortProperties: TdxQuerySortingCollection;
begin
  ASortProperties := Root.SortProperties;
  if ASortProperties.Count = 0 then
    Exit('');
  AList := TStringBuilder.Create;
  try
    for I := 0 to ASortProperties.Count - 1 do
    begin
      ASp := ASortProperties{$IFDEF DELPHIXE3}.List{$ENDIF}[I];
      J := 0;
      while J < I do
      begin
        if (ASortProperties{$IFDEF DELPHIXE3}.List{$ENDIF}[J].&Property as TdxCriteriaOperator).Equals(ASp.&Property as TdxCriteriaOperator) then
          Break;
        Inc(J);
      end;
      if J < I then
        Continue;
      AList.Append(FFormatter.FormatOrder(Process(ASp.&Property), ASp.SortDirection)).
        Append(',');
    end;
    Result := AList.ToString(0, AList.Length - 1);
  finally
    AList.Free;
  end;
end;

function TdxSelectSQLGenerator.GetForceOuterApply: Boolean;
begin
  Result := FIsBuildGrouping or (inherited GetForceOuterApply);
end;

function TdxSelectSQLGenerator.BuildGrouping: string;
var
  AList: TStringBuilder;
  ASp: IdxCriteriaOperator;
  AGroupProperty: string;
begin
  FIsBuildGrouping := True;
  try
    if Root.GroupProperties.Count = 0 then
      Exit('');
    FGroupProperties.Free;
    FGroupProperties := TdxHashSetString.Create;
    AList := TStringBuilder.Create;
    try
      for ASp in Root.GroupProperties do
      begin
        AGroupProperty := Process(ASp as TdxCriteriaOperator);
        FGroupProperties.Add(AGroupProperty);
        AList.Append(AGroupProperty).Append(',');
      end;
      Result := AList.ToString(0, AList.Length - 1);
    finally
      AList.Free;
    end;
  finally
    FIsBuildGrouping := False;
  end;
end;

function TdxSelectSQLGenerator.BuildAdditionalGroupingOuterApply: string;
var
  AResult: TStringBuilder;
  AOuterApplyProperty: string;
begin
  if ((FOAProperties <> nil) and (FGroupProperties <> nil)) and UseOuterApply then
  begin
    AResult := TStringBuilder.Create;
    try
      for AOuterApplyProperty in FOAProperties.Keys do
      begin
        if not FGroupProperties.Contains(AOuterApplyProperty) then
          AResult.Append(',').Append(AOuterApplyProperty);
      end;
      Result := AResult.ToString;
    finally
      AResult.Free;
    end;
  end
  else
    Result := '';
end;

function TdxSelectSQLGenerator.PatchProperty(const APropertyOperator: IdxCriteriaOperator; const APropertyString: string): string;
begin
  Result := APropertyString;
end;

function TdxSelectSQLGenerator.BuildProperties: string;
var
  AOperandIndex, I, AOAResultCount: Integer;
  AList: TStringBuilder;
  AMic: IdxCriteriaOperator;
  AProperty, APropertyAlias, AAlias: string;
  AQueryOperand: IdxQueryOperand;
begin
  AOperandIndex := 0;
  AList := TStringBuilder.Create;
  try
    for I := 0 to Root.Operands.Count - 1 do
    begin
      AMic := Root.Operands[I];
      if (AMic is TdxOperandValue) and (FParentGenerator = nil) then
        FConstantValues.Add(I, AMic as IdxOperandValue)
      else
      begin
        if AList.Length > 0 then
          AList.Append(',');
        AOAResultCount := OuterApplyResultCounter;
        AProperty := Process(AMic);
        if OuterApplyResultCounter > AOAResultCount then
        begin
          if FOAProperties = nil then
            FOAProperties := TdxHashSetString.Create;
          FOAProperties.Add(AProperty);
        end;
        AList.Append(PatchProperty(AMic, AProperty));
        if FPropertyAliases <> nil then
        begin
          if AOperandIndex < Length(FPropertyAliases) then
            APropertyAlias := FPropertyAliases[AOperandIndex]
          else
            APropertyAlias := '';
          AList.Append(' as ');
          AAlias := '';
          if not Supports(AMic, IdxQueryOperand) then
          begin
            if APropertyAlias <> '' then
              AAlias := APropertyAlias
            else
              AAlias := 'PrP' + IntToStr(AOperandIndex);
          end
          else
          begin
            AQueryOperand := AMic as IdxQueryOperand;
            if (APropertyAlias = '') and (APropertyAlias <> AQueryOperand.ColumnName) then
              AAlias := AQueryOperand.ColumnName
            else
              AAlias := APropertyAlias;
          end;
          AList.Append(FFormatter.FormatColumn(FFormatter.ComposeSafeColumnName(AAlias)));
        end;
        FOperandIndexes.Add(I, AOperandIndex);
        Inc(AOperandIndex);
      end;
    end;
    if AOperandIndex = 0 then
      Result := '1'
    else
      Result := AList.ToString;
  finally
    AList.Free;
  end;
end;

function TdxSelectSQLGenerator.BuildGroupCriteria: string;
begin
  Result := ProcessParameter(Root.GroupCondition, True);
end;

function TdxSelectSQLGenerator.InternalGenerateSQL: string;
var
  AFromClause: TStringBuilder;
  AGroupByClause, APropertiesSQL, AWhereClause, AHavingClause, AOrderByClause: string;
  ASkipSelectedRecords, ATopSelectedRecords: Integer;
  ASkipFormatter: Boolean;
begin
  AGroupByClause := BuildGrouping;
  APropertiesSQL := BuildProperties;
  AFromClause := BuildJoins;
  AWhereClause := BuildCriteria;
  AHavingClause := BuildGroupCriteria;
  AOrderByClause := BuildSorting;
  ASkipSelectedRecords := Root.SkipSelectedRecords;
  ATopSelectedRecords := Root.TopSelectedRecords;
  AFromClause.Append(BuildOuterApply);
  if AGroupByClause <> '' then
    AGroupByClause := AGroupByClause + BuildAdditionalGroupingOuterApply;
  if ASkipSelectedRecords <> 0 then
  begin
    ASkipFormatter := FFormatter.SupportsNativeSkipTake;
    if ASkipFormatter then
      Exit(FFormatter.FormatSelect(APropertiesSQL, AFromClause.ToString, AWhereClause, AOrderByClause, AGroupByClause, AHavingClause,
        ASkipSelectedRecords, ATopSelectedRecords))
    else
      if ATopSelectedRecords <> 0 then
      begin
        SkipSelectedRecords := ASkipSelectedRecords;
        TopSelectedRecords := ATopSelectedRecords;
        Exit(FFormatter.FormatSelect(APropertiesSQL, AFromClause.ToString, AWhereClause, AOrderByClause, AGroupByClause, AHavingClause,
          ASkipSelectedRecords + ATopSelectedRecords));
      end;
  end;
  Result := FFormatter.FormatSelect(APropertiesSQL, AFromClause.ToString, AWhereClause, AOrderByClause, AGroupByClause, AHavingClause,
    ATopSelectedRecords);
end;

function TdxSelectSQLGenerator.CreateQuery(const ACommandText: string; AParameters: TdxQueryParameterCollection;
  const AParameterNames: TArray<string>): TdxQuery;
begin
  Result := TdxQuery.Create(ACommandText, AParameters, AParameterNames, SkipSelectedRecords, TopSelectedRecords,
    FConstantValues, FOperandIndexes);
end;

function TdxSelectSQLGenerator.GetNextParameterName(const AParameter: IdxOperandValue): string;
begin
  if FParentGenerator = nil then
    Result := inherited GetNextParameterName(AParameter)
  else
    Result := FParentGenerator.GetNextParameterName(AParameter);
end;

{ TdxBaseObjectSQLGenerator }

function TdxBaseObjectSQLGenerator.GenerateSQL(const ADMLStatements: TArray<TdxModificationStatement>): TdxQueryCollection;
var
  AList: TdxQueryCollection;
  ANode: TdxModificationStatement;
  AQuery: TdxQuery;
begin
  AList := TdxQueryCollection.Create;
  for ANode in ADMLStatements do
  begin
    AQuery := inherited GenerateSQL(ANode);
    if AQuery.SQLText <> '' then
      AList.Add(AQuery)
    else
      AQuery.Free;
  end;
  Result := AList;
end;

{ TdxDeleteSQLGenerator }

function TdxDeleteSQLGenerator.InternalGenerateSQL: string;
begin
  if Root.Table is TdxDBProjection then
    raise EInvalidOperation.Create(sdxTableIsDBProjection);
  Result := FFormatter.FormatDelete(FFormatter.FormatTableSafe(Root.Table), BuildCriteria);
end;

{ TdxUpdateSQLGenerator }

function TdxUpdateSQLGenerator.InternalGenerateSQL: string;
var
  AValues: TStringBuilder;
  I: Integer;
begin
  if Root.Table is TdxDBProjection then
    raise EInvalidOperation.Create(sdxTableIsDBProjection);
  if Root.Operands.Count = 0 then
    Exit('');
  AValues := TStringBuilder.Create;
  try
    for I := 0 to Root.Operands.Count - 1 do
    begin
      if I > 0 then
        AValues.Append(',');
      AValues.
        Append(Process(Root.Operands[I])).
        Append('=').
        Append(GetNextParameterName((TdxUpdateStatement(Root)).Parameters[I]));
    end;
    Result := FFormatter.FormatUpdate(FFormatter.FormatTableSafe(Root.Table), AValues.ToString, BuildCriteria);
  finally
    AValues.Free;
  end;
end;

{ TdxInsertSQLGenerator }

function TdxInsertSQLGenerator.InternalGenerateSQL: string;
var
  ANames, AValues: TStringBuilder;
  I: Integer;
begin
  if Root.Table is TdxDBProjection then
    raise EInvalidOperation.Create(sdxTableIsDBProjection);
  if Root.Operands.Count = 0 then
    Exit(FFormatter.FormatInsertDefaultValues(Root.Table));
  ANames := TStringBuilder.Create;
  AValues := TStringBuilder.Create;
  try
    for I := 0 to Root.Operands.Count - 1 do
    begin
      if I > 0 then
      begin
        ANames.Append(',');
        AValues.Append(',');
      end;
      ANames.Append(Process(Root.Operands[I] as TdxCriteriaOperator));
      AValues.Append(GetNextParameterName((TdxInsertStatement(Root)).Parameters[I]));
    end;
    Result := FFormatter.FormatInsert(FFormatter.FormatTableSafe(Root.Table), ANames.ToString, AValues.ToString);
  finally
    ANames.Free;
    AValues.Free;
  end;
end;

end.
