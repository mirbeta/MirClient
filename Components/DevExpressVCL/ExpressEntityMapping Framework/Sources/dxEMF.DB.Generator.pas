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

unit dxEMF.DB.Generator;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, Rtti,
  dxCoreClasses, dxGenerics,
  dxEMF.Types,
  dxEMF.Metadata,
  dxEMF.Utils,
  dxEMF.DB.Model,
  dxEMF.DB.Criteria,
  dxEMF.DB.Query;

type

  { TdxCollectionCriteriaPatcher }

  TdxCollectionCriteriaPatcher = record
  strict private
    FHasValue: Boolean;
    FSelectDeleted: Boolean;
  public
    constructor Create(ASelectDeleted: Boolean);
    function PatchCriteria(AEntityInfo: TdxEntityInfo; const AOriginalCriteria: IdxCriteriaOperator): IdxCriteriaOperator;
    property HasValue: Boolean read FHasValue;
  end;

  { TdxSingleAggregateItem }

  TdxSingleAggregateItem = record
  public
    Condition: IdxCriteriaOperator;
    EntityInfo: TdxEntityInfo;
    constructor Create(const ACondition: IdxCriteriaOperator; AEntityInfo: TdxEntityInfo);
  end;

  { TdxObjectsQuery }

  TdxObjectsQuery = class
  strict private
    FEntityInfo: TdxEntityInfo;
    FCriteria: IdxCriteriaOperator;
    FSorting: IdxSortByExpressions;
    FTopSelectedRecords: Integer;
    FSkipSelectedRecords: Integer;
    FCollectionCriteriaPatcher: TdxCollectionCriteriaPatcher;
    FForce: Boolean;
  public
    constructor Create(AEntityInfo: TdxEntityInfo; const ACriteria: IdxCriteriaOperator; const ASortByExpressions: IdxSortByExpressions;
      ATopSelectedRecords: Integer; const ACollectionCriteriaPatcher: TdxCollectionCriteriaPatcher; AForce: Boolean); overload;
    constructor Create(AEntityInfo: TdxEntityInfo; const ACriteria: IdxCriteriaOperator; const ASortByExpressions: IdxSortByExpressions;
      ASkipSelectedRecords, ATopSelectedRecords: Integer; const ACollectionCriteriaPatcher: TdxCollectionCriteriaPatcher;
      AForce: Boolean); overload;

    property EntityInfo: TdxEntityInfo read FEntityInfo write FEntityInfo;
    property Criteria: IdxCriteriaOperator read FCriteria write FCriteria;
    property Sorting: IdxSortByExpressions read FSorting write FSorting;
    property TopSelectedRecords: Integer read FTopSelectedRecords write FTopSelectedRecords;
    property SkipSelectedRecords: Integer read FSkipSelectedRecords write FSkipSelectedRecords;
    property CollectionCriteriaPatcher: TdxCollectionCriteriaPatcher read FCollectionCriteriaPatcher
      write FCollectionCriteriaPatcher;
    property Force: Boolean read FForce write FForce;
  end;

  { TdxMemberPathOperand }

  TdxMemberPathOperand = class(TdxOperandProperty)
  strict private
    FMic: TdxMemberInfoCollection;
  public
    constructor Create(APath: TdxMemberInfoCollection);
    property Path: TdxMemberInfoCollection read FMic;
  end;

  { IdxMemberOperand }

  IdxMemberOperand = interface(IdxCriteriaOperator)
  ['{3D82515C-C4D8-42F3-A57E-F43A4D005C00}']
    function GetMember: TdxMappingMemberInfo;
    property Member: TdxMappingMemberInfo read GetMember;
  end;

  { TdxMemberOperand }

  TdxMemberOperand = class(TdxCriteriaOperator, IdxMemberOperand)
  strict private
    FMember: TdxMappingMemberInfo;
    function GetMember: TdxMappingMemberInfo;
  public
    constructor Create(AMember: TdxMappingMemberInfo); overload;
    function Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator; override;
    function Accept(const AVisitor: IdxSQLGeneratorVisitor): string; override;
    property Member: TdxMappingMemberInfo read GetMember;
  end;

  { IdxPropertyAlias }

  IdxPropertyAlias = interface(IdxMemberOperand)
  ['{E910C359-6427-4F89-9338-086F25A715E2}']
    function GetCount: Integer;
//    function GetMember: TdxMappingMemberInfo;
    function GetNode: TdxJoinNode;
    property Count: Integer read GetCount;
//    property Member: TdxMappingMemberInfo read GetMember;
    property Node: TdxJoinNode read GetNode;
  end;

  { TdxPropertyAlias }

  TdxPropertyAlias = class(TdxMemberOperand, IdxPropertyAlias)
  strict private
    class var FEmpty: IdxPropertyAlias;
  strict private
    FSubMembers: TObject;
    FPrefix: string;
    FNode: TdxJoinNode;
    function GetCount: Integer;
    function GetNode: TdxJoinNode;
    class function GetEmpty: IdxPropertyAlias; static;
  public
    constructor Create(ANode: TdxJoinNode; AMember: TdxMappingMemberInfo; ASub: Boolean); overload;
    constructor Create(ANode: TdxJoinNode; AMember: TdxMappingMemberInfo; ASubMembers: TdxMemberInfoList;
      const APrefix: string); overload;
    constructor Create(ANode: TdxJoinNode; AMember: TdxMappingMemberInfo); overload;

    function GetMember(I: Integer): TdxMappingMemberInfo; overload;
    function GetMappingField(I: Integer): string;
//    function Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator; override;
//    function Accept(const AVisitor: IdxSQLGeneratorVisitor): string; override;

    property Count: Integer read GetCount;
    property Node: TdxJoinNode read FNode;
    class property Empty: IdxPropertyAlias read GetEmpty;
  end;

  { TdxProjectionNodeItem }

  TdxProjectionNodeItem = record
  strict private
    FNode: TdxJoinNode;
    FProjectedNodes: TdxJoinNodeList;
  public
    constructor Create(ANode: TdxJoinNode; AProjectedNodes: TdxJoinNodeList);
    property Node: TdxJoinNode read FNode;
    property ProjectedNodes: TdxJoinNodeList read FProjectedNodes;
  end;

  { TdxBatchWideDataHolder }

  TdxBatchWideDataHolder = class abstract
  strict private
    FCurrentTag: Integer;
  public
    constructor Create; overload;
    function GetNextTag: Integer;
    function GetParameter(const AValue: TValue): IdxOperandValue; virtual; abstract;
  end;

  { TdxBatchWideDataHolderForSelect }

  TdxBatchWideDataHolderForSelect = class(TdxBatchWideDataHolder)
  strict private
    FParametersByValues: TDictionary<TValue, IdxParameterValue>;
    FParametersByObjects: TdxObjectDictionary<IdxParameterValue>;
  public
    destructor Destroy; override;
    function GetParameter(const AValue: TValue): IdxOperandValue; override;
  end;

  { TdxBatchWideDataHolderForModification }

  TdxBatchWideDataHolderForModification = class(TdxBatchWideDataHolder)
  strict private
    FDeletedObjects: TArray<TObject>;
    FInsertedObjects: TdxObjectSet;
    FIdentityParameters: TdxObjectDictionary<IdxParameterValue>;
    FUpdatedMembersBeforeDeleteDictionary: TdxObjectDictionary<TdxMemberInfoCollection>;
    FQueryOperandsCache: TDictionary<IdxQueryOperand, IdxQueryOperand>;
  protected
    function CacheQueryOperand(const AToCache: IdxQueryOperand): IdxQueryOperand;
  public
    destructor Destroy; override;
    procedure RegisterDeletedObjects(const AObjects4Delete: TArray<TObject>);
    function CreateIdentityParameter(AObject: TObject): IdxParameterValue;
    function IsObjectAlreadyInserted(AObject: TObject): Boolean;
    procedure RegisterInsertedObject(AObject: TObject);
    function GetParameter(const AValue: TValue): IdxOperandValue; override;
    procedure RegisterUpdatedMembersBeforeDelete(AObject: TObject; AMembers: TdxMemberInfoCollection);
    function TryGetUpdatedMembersBeforeDelete(AObject: TObject; out AMembers: TdxMemberInfoCollection): Boolean;

    property InsertedObjects: TdxObjectSet read FInsertedObjects;
    property DeletedObjects: TArray<TObject> read FDeletedObjects;
  end;

  { TdxNamedJoinNodeDictionary }

  TdxNamedJoinNodeDictionary = class(TdxNamedObjectDictionary<TdxJoinNode>);

  { TdxBaseQueryGenerator }

  TdxBaseQueryGenerator = class(TcxIUnknownObject, IdxCriteriaOperatorVisitor)
  strict private
    class var
      FNodeAliasCache: TArray<string>;
  strict private
    FBatchWideData: TdxBatchWideDataHolder;
    FCollectionCriteriaPatcher: TdxCollectionCriteriaPatcher;
    FNodeStackHashSet: TdxHashSetString;
    FProjectedSingleNodes: TdxNamedJoinNodeDictionary;
    FProjectionNodes: TdxNamedOrdinalDictionary<TdxProjectionNodeItem>;
    FMultiColumnAliases: TDictionary<IdxCriteriaOperator, IdxPropertyAlias>;
    FRoot: TdxBaseStatement;
    FQueryParameters: TdxQueryParameterCollection;
    FIndexCount: Integer;
    FInheritanceSubNodes: TObjectDictionary<string, TDictionary<TdxDBTable, TdxJoinNode>>;
    FReferenceSubNodes: TObjectDictionary<string, TdxNamedJoinNodeDictionary>;
    FSingleAggregateSubNodes: TObjectDictionary<string, TDictionary<TdxSingleAggregateItem, TdxJoinNode>>;
    FNakedSingleNodes: TdxNamedJoinNodeDictionary;
    FSingleNesting: Integer;
    FClassInfoStack: TStack<TdxEntityInfo>;
    FNodeStack: TStack<TdxJoinNode>;
    FEntityInfo: TdxEntityInfo;
    FRootNode: TdxJoinNode;
    FSuppressDefaultValueConverters: Boolean;
    FCurrentJoinType: TdxJoinType;
    FCurrentLeftJoinEnforcer: TdxGroupOperatorType;
    procedure AddMultiColumnAlias(const AOperator: IdxCriteriaOperator; const AAlias: IdxPropertyAlias);
    function AppendInheritanceJoinNode(ABranch: TdxEntityInfo; AProperty: TdxMappingMemberInfo; APrevNode: TdxJoinNode)
      : TdxJoinNode;
    function AppendJoinNode(AProperty: TdxMappingMemberInfo; APrevNode: TdxJoinNode; AType: TdxJoinType): TdxJoinNode;
    function BuildInGroup(const ADefaultMembers: IdxPropertyAlias; AValues: TdxCriteriaOperatorList;
      const ALeftOperator: IdxCriteriaOperator): IdxCriteriaOperator;
    function CreateBinary(const ALeftOperator: IdxCriteriaOperator; const ALeftMembers: IdxPropertyAlias;
      const ARightOperator: IdxCriteriaOperator; const ARightMembers: IdxPropertyAlias;
      AOperatorType: TdxBinaryOperatorType; AIndex: Integer): IdxBinaryOperator; overload;
    function CreateBinaryGroup(const ASubMembers: IdxPropertyAlias; const ALeftOperator: IdxCriteriaOperator; const ALeftMembers: IdxPropertyAlias;
      const ARightOperator: IdxCriteriaOperator; const ARightMembers: IdxPropertyAlias; AOperatorType: TdxBinaryOperatorType): IdxCriteriaOperator;
    function CollectInValues(const AOperands: IdxCriteriaOperatorCollection; AValues: TdxCriteriaOperatorList): IdxPropertyAlias;
    function GetClassInfoStack: TStack<TdxEntityInfo>;
    function GetConstParameter(AMember: TdxMappingMemberInfo; const AOperand: IdxOperandValue; ATargetMember: TdxMappingMemberInfo)
      : IdxOperandValue; overload;
    function GetJoinCondition(const ALeftAlias, ARightAlias: string; ALeftMember, ARightMember: TdxMappingMemberInfo)
      : IdxCriteriaOperator;
    function GetNodeStackHashSet: TdxHashSetString;
    function GetNodeStack: TStack<TdxJoinNode>;
    function ProcessTopSubSelect(const AAggregateProperty, AOperandClause: IdxCriteriaOperator;
      AAggregateFunctionType: TdxAggregateFunctionType): TdxQuerySubQueryContainer;
    class procedure SetJoinTypeWithSubNodes(ANode: TdxJoinNode; AType: TdxJoinType); static;
    function TryGetMultiColumnAlias(const AOperator: IdxCriteriaOperator; out AAlias: IdxPropertyAlias): Boolean;
  protected
    procedure BuildAssociationTree(const ACriteria: IdxCriteriaOperator);
    function CreateBinary(AType: TdxBinaryOperatorType; const ALeftOperator, ARightOperator: IdxCriteriaOperator): IdxCriteriaOperator; overload;
    function CreateRootStatement(ATable: TdxDBTable; const AAlias: string): TdxBaseStatement; virtual; abstract;
    function CreateOperand(AMember: TdxMappingMemberInfo; const ANodeAlias: string): IdxQueryOperand; overload;
    function CreateOperand(const AMappingField, ANodeAlias: string): IdxQueryOperand; overload;
    function ConvertViaDefaultValueConverter(var AOperandValue: TValue): Boolean;
    function ExecuteWithPropertyNameDiving(const APropertyName: string; const AWorker: TFunc<string, IdxCriteriaOperator>;
      AThrowOnEmptyStack: Boolean = True): IdxCriteriaOperator;
    function GenerateSQL(const ACriteria: IdxCriteriaOperator): TdxBaseStatement;
    function GetConstParameter(const AValue: TValue): IdxOperandValue; overload;
    function GetMembers(ANode: TdxJoinNode; AMember: TdxMappingMemberInfo): IdxPropertyAlias; overload;
    function GetMembers(AMember: TdxMappingMemberInfo; out APrefix: string): TdxMemberInfoList; overload;
    function GetNextNodeAlias: string; virtual;
    function GetParameter(const AParam: IdxCriteriaOperator; const AAlias: IdxPropertyAlias; AIndex: Integer): IdxCriteriaOperator;
    function GetPropertyNode(APropertyPath: TdxMemberInfoCollection; AType: TdxJoinType): IdxPropertyAlias; overload;
    function GetPropertyNode(const AProperty: IdxOperandProperty; AType: TdxJoinType): IdxPropertyAlias; overload; virtual;
    function GetSubJoinCriteria(AGena: TdxBaseQueryGenerator): IdxCriteriaOperator; virtual;
    function GetQueryOperandFromAlias(const AAliasFromParam, AAlias: IdxPropertyAlias; AIndex: Integer): IdxCriteriaOperator;
    class function GetProjectedMemberColumn(const AMappingField: string; ANode: TdxJoinNode;
      AProjectedNodeList: TdxJoinNodeList; const AParentCriteria: IdxCriteriaOperator = nil): TdxDBColumn; static;
  	function IsGrouped: Boolean; virtual;
    procedure InitData; virtual;
    procedure InternalGenerateSQL(const ACriteria: IdxCriteriaOperator); virtual; abstract;
    function AppendSingleAggregateJoinNode(AEntityInfo: TdxEntityInfo; ACollectionProperty: TdxMappingMemberInfo;
      const ACondition: IdxCriteriaOperator; APrevNode: TdxJoinNode; AType: TdxJoinType): TdxJoinNode;
    function PatchCriteria(const AOriginalCriteria: IdxCriteriaOperator): IdxCriteriaOperator; overload; inline;
    function PatchCriteria(const AOriginalCriteria: IdxCriteriaOperator; AEntityInfo: TdxEntityInfo): IdxCriteriaOperator; overload;
    function Process(const AOperand: IdxCriteriaOperator): IdxCriteriaOperator;
    function ProcessLogical(const AOperand: IdxCriteriaOperator): IdxCriteriaOperator;
    function ProcessLogicalInContext(AEntityInfo: TdxEntityInfo; ASingleNode: TdxJoinNode; const ACriteria: IdxCriteriaOperator): IdxCriteriaOperator;
    function ProcessSingleAggregateOperand(const ACollectionProperty: IdxOperandProperty;
      const ACondition, AAggregatedExpression: IdxCriteriaOperator): IdxCriteriaOperator;
    function ProcessSubSelectOperator(const ACollectionProperty: IdxOperandProperty; const AOperandClause: IdxCriteriaOperator;
      const AAggregateProperty: IdxCriteriaOperator; AAggregateFunctionType: TdxAggregateFunctionType): TdxQuerySubQueryContainer; overload;
    function ProcessSubSelectOperator(const AJoinTypeName: string; const AOperandClause, AAggregateProperty: IdxCriteriaOperator;
      AAggregateFunctionType: TdxAggregateFunctionType): TdxQuerySubQueryContainer; overload;
    function ProcessSingleJoinOperand(const AJoinTypeName: string;
      const ACondition, AAggregatedExpression: IdxCriteriaOperator): IdxCriteriaOperator;
    procedure TryAddNodeIntoProjection(APrevNode: TdxJoinNode; ANode: TdxJoinNode);
    { IdxCriteriaOperatorVisitor }
    function Visit(const AOperator: IdxBetweenOperator): IdxCriteriaOperator; overload;
    function Visit(const AOperator: IdxBinaryOperator): IdxCriteriaOperator; overload;
    function Visit(const AOperator: IdxUnaryOperator): IdxCriteriaOperator; overload;
    function Visit(const AOperator: IdxInOperator): IdxCriteriaOperator; overload;
    function Visit(const AOperator: IdxGroupOperator): IdxCriteriaOperator; overload;
    function Visit(const AOperand: IdxOperandValue): IdxCriteriaOperator; overload;
    function Visit(const AOperator: IdxFunctionOperator): IdxCriteriaOperator; overload;

    function Visit(const AOperand: IdxOperandProperty): IdxCriteriaOperator; overload;
    function Visit(const AOperator: IdxAggregateOperand): IdxCriteriaOperator; overload;
    function Visit(const AOperator: IdxJoinOperand): IdxCriteriaOperator; overload;

    property BatchWideData: TdxBatchWideDataHolder read FBatchWideData;
    property Root: TdxBaseStatement read FRoot;
    property ClassInfoStack: TStack<TdxEntityInfo> read GetClassInfoStack;
    property NodeStack: TStack<TdxJoinNode> read GetNodeStack;
  public
    constructor Create(AEntityInfo: TdxEntityInfo; ABatchWideData: TdxBatchWideDataHolder); overload;
    constructor Create(AEntityInfo: TdxEntityInfo; ABatchWideData: TdxBatchWideDataHolder;
      const ACollectionCriteriaPatcher: TdxCollectionCriteriaPatcher); overload;
    destructor Destroy; override;
    property NodeStackHashSet: TdxHashSetString read GetNodeStackHashSet;
    property EntityInfo: TdxEntityInfo read FEntityInfo write FEntityInfo;
  end;

  { TdxClientSelectSQLGenerator }

  TdxClientSelectSQLGenerator = class(TdxBaseQueryGenerator)
  strict private
    FGrouping: IdxCriteriaOperatorCollection;
    FGroupCriteria: IdxCriteriaOperator;
    FProperties: IdxCriteriaOperatorCollection;
    FSorting: IdxSortByExpressions;
    function BuildAssociation(const AProperties: IdxCriteriaOperatorCollection): TArray<IdxCriteriaOperator>;
    procedure BuildAssociationByProperties(const AProperties: IdxCriteriaOperatorCollection);
    procedure BuildAssociationByGrouping;
    procedure BuildAssociationByGroupCriteria;
    procedure BuildAssociationBySorting;
    function GetRoot: TdxSelectStatement; inline;
  protected
    FCurrentJoinType: TdxJoinType;
    function CreateRootStatement(ATable: TdxDBTable; const AAlias: string): TdxBaseStatement; override;
    function GetSubJoinCriteria(AGena: TdxBaseQueryGenerator): IdxCriteriaOperator; override;
    procedure InternalGenerateSQL(const ACriteria: IdxCriteriaOperator); override;
		function IsGrouped: Boolean; override;
    property Root: TdxSelectStatement read GetRoot;
  public
    constructor Create(AEntityInfo: TdxEntityInfo; ABatchWideData: TdxBatchWideDataHolderForSelect;
      const AProperties: IdxCriteriaOperatorCollection; const ASortByExpressions: IdxSortByExpressions;
      const AGrouping: IdxCriteriaOperatorCollection;
      const AGroupCriteria: IdxCriteriaOperator; const ACollectionCriteriaPatcher: TdxCollectionCriteriaPatcher);
    class function GenerateSelect(AQuery: TdxObjectsQuery; const AProperties, AGroupProperties: IdxCriteriaOperatorCollection;
      const AGroupCriteria: IdxCriteriaOperator): TdxSelectStatement;
      overload; static;
    class function GenerateSelect(AEntityInfo: TdxEntityInfo; const ACriteria: IdxCriteriaOperator;
      const AProperties: IdxCriteriaOperatorCollection; const ASortByExpressions: IdxSortByExpressions;
      const AGrouping: IdxCriteriaOperatorCollection;
      const AGroupCriteria: IdxCriteriaOperator; const ACollectionCriteriaPatcher: TdxCollectionCriteriaPatcher;
      ATopSelectedRecords: Integer): TdxSelectStatement; overload; static;
    class function GenerateSelect(AEntityInfo: TdxEntityInfo; const ACriteria: IdxCriteriaOperator;
      const AProperties: IdxCriteriaOperatorCollection;  const ASortByExpressions: IdxSortByExpressions;
      const AGrouping: IdxCriteriaOperatorCollection;
      const AGroupCriteria: IdxCriteriaOperator; const ACollectionCriteriaPatcher: TdxCollectionCriteriaPatcher;
      ASkipSelectedRecords, ATopSelectedRecords: Integer): TdxSelectStatement; overload; static;
    class function GenerateSelect(AEntityInfo: TdxEntityInfo; const ACriteria: IdxCriteriaOperator; AProperties: TdxMemberPathCollection;
      const ASortByExpressions: IdxSortByExpressions; const AGrouping: IdxCriteriaOperatorCollection; const AGroupCriteria: IdxCriteriaOperator;
      const ACollectionCriteriaPatcher: TdxCollectionCriteriaPatcher; ATopSelectedRecords: Integer): TdxSelectStatement; overload; static;
    class function GenerateSelect(AEntityInfo: TdxEntityInfo; const ACriteria: IdxCriteriaOperator; AProperties: TdxMemberPathCollection;
      const ASortByExpressions: IdxSortByExpressions; const AGrouping: IdxCriteriaOperatorCollection; const AGroupCriteria: IdxCriteriaOperator;
      const ACollectionCriteriaPatcher: TdxCollectionCriteriaPatcher;
      ASkipSelectedRecords, ATopSelectedRecords: Integer): TdxSelectStatement; overload; static;
  end;

  { TdxBaseObjectQueryGenerator }

  TdxBaseObjectQueryGenerator = class abstract(TdxBaseQueryGenerator)
  strict private
    FTheObject: TObject;
    FProperties: TdxMemberInfoCollection;
    function GetBatchWideData: TdxBatchWideDataHolderForModification;
  protected
    function GenerateSQL(ACriteriaSet: TdxObjectGeneratorCriteriaSet;
      const AProperties: TArray<TdxMappingMemberInfo>; AReverse: Boolean = True): TList<TdxModificationStatement>; overload;
    function GetClasses: TList<TdxEntityInfo>;
    function GetMemberParameter(AMember: TdxMappingMemberInfo; AObject: TObject): IdxOperandValue;
    function ShouldPersist(AMember: TdxMappingMemberInfo): Boolean; virtual;
    procedure AddParameter(const AParameter: IdxOperandValue); virtual;
    procedure BuildFieldList;
    procedure InitData; override;

    property BatchWideData: TdxBatchWideDataHolderForModification read GetBatchWideData;
    property Properties: TdxMemberInfoCollection read FProperties;
    property TheObject: TObject read FTheObject;
  public
    constructor Create(AEntityInfo: TdxEntityInfo; ABatchWideData: TdxBatchWideDataHolderForModification; AObject: TObject = nil); overload;
    constructor Create(ABatchWideData: TdxBatchWideDataHolderForModification; AObject: TObject); overload;
    class function BuildKeyCriteria(AObject: TObject): IdxCriteriaOperator; static;
    destructor Destroy; override;
  end;

  { TdxRefHolder }

  TdxRefHolder = class
  strict private
    FPool: TDictionary<TdxEntityInfo, TdxRefHolder>;
    FEntityInfo: TdxEntityInfo;
    FObjects: TList<TObject>;
    FCanLoop: Boolean;
    FDirectlyAssignableNotDeleted: TDictionary<TdxEntityInfo, TdxRefHolder>;
  public
    constructor Create(APool: TDictionary<TdxEntityInfo, TdxRefHolder>; AMe: TdxEntityInfo);
    destructor Destroy; override;
    class function GetNonReferenced(APool: TDictionary<TdxEntityInfo, TdxRefHolder>): TArray<TdxEntityInfo>; static;
    class function CreatePool(AObjects: TList<TObject>): TDictionary<TdxEntityInfo, TdxRefHolder>; static;
    class procedure ProcessDelete(APool: TDictionary<TdxEntityInfo, TdxRefHolder>; AEntityInfo: TdxEntityInfo); static;
    class function GenerateUpdatesBeforeDelete(AEntityInfo: TdxEntityInfo; AUpdateList: TdxMemberInfoCollection;
      const AKeys: TArray<TObject>; ABatchWideData: TdxBatchWideDataHolderForModification;
      ALocking: TdxLockingOption): TArray<TdxModificationStatement>; static;
    function CanReach(ATarget: TdxEntityInfo): Boolean; overload;
    function CanReach(AProcessed: TDictionary<TdxEntityInfo, TdxEntityInfo>; ATarget: TdxEntityInfo): Boolean; overload;
    function IsInLoop: Boolean;
    procedure FillDirectlyRefs;
    function DoUpdates(ALocking: TdxLockingOption; ABatchWideData: TdxBatchWideDataHolderForModification): TArray<TdxModificationStatement>;
    property Objects: TList<TObject> read FObjects;
    property DirectlyAssignableNotDeleted: TDictionary<TdxEntityInfo, TdxRefHolder> read FDirectlyAssignableNotDeleted;
  end;

  { TdxDeleteQueryGenerator }

  TdxDeleteQueryGenerator = class(TdxBaseObjectQueryGenerator)
  strict private
    function GetRoot: TdxDeleteStatement; inline;
  protected
    class function GenerateDeleteCore(AEntityInfo: TdxEntityInfo; const AKeys: TArray<TObject>;
      ALocking: TdxLockingOption; ABatchWideData: TdxBatchWideDataHolderForModification): TList<TdxModificationStatement>; static;
    procedure InitData; override;
    procedure InternalGenerateSQL(const ACriteria: IdxCriteriaOperator); override;
    function CreateRootStatement(ATable: TdxDBTable; const AAlias: string): TdxBaseStatement; override;

    property Root: TdxDeleteStatement read GetRoot;
  public
    class function GenerateDelete(AObject: TObject; ALocking: TdxLockingOption;
      ABatchWideData: TdxBatchWideDataHolderForModification): TList<TdxModificationStatement>; overload; static;
    class function GenerateDelete(AEntityInfo: TdxEntityInfo; const AKeys: TList<TObject>; ALocking: TdxLockingOption;
      ABatchWideData: TdxBatchWideDataHolderForModification): TList<TdxModificationStatement>; overload; static;
    class function GenerateDelete(const AObjects: TList<TObject>; ALocking: TdxLockingOption;
      ABatchWideData: TdxBatchWideDataHolderForModification): TList<TdxModificationStatement>; overload; static;
    class function GenerateDelete(AEntityInfo: TdxEntityInfo; ACriteriaSet: TdxObjectGeneratorCriteriaSet;
      ABatchWideData: TdxBatchWideDataHolderForModification): TList<TdxModificationStatement>; overload; static;
    class procedure GenerateDeletes(AEntityInfo: TdxEntityInfo; ACriteriaSet: TdxObjectGeneratorCriteriaSet;
      ARes: TList<TdxModificationStatement>; ACount: Integer; ABatchWideData: TdxBatchWideDataHolderForModification); static;
  end;

  { TdxUpdateQueryGenerator }

  TdxUpdateQueryGenerator = class(TdxBaseObjectQueryGenerator)
  strict private
    function GetRoot: TdxUpdateStatement;
  protected
    procedure InitData; override;
    procedure AddParameter(const AParameter: IdxOperandValue); override;
    procedure InternalGenerateSQL(const ACriteria: IdxCriteriaOperator); override;
    function CreateRootStatement(ATable: TdxDBTable; const AAlias: string): TdxBaseStatement; override;

    property Root: TdxUpdateStatement read GetRoot;
  public
    class function GenerateUpdate(ABatchWideData: TdxBatchWideDataHolderForModification; AObject: TObject;
      const AProperties: TArray<TdxMappingMemberInfo>; ACriteriaSet: TdxObjectGeneratorCriteriaSet): TList<TdxModificationStatement>; overload; static;
    class function GenerateUpdate(AObject: TObject; const AProperties: TArray<TdxMappingMemberInfo>;
      ACriteriaSet: TdxObjectGeneratorCriteriaSet): TList<TdxModificationStatement>; overload; static;
    class function GenerateUpdate(AEntityInfo: TdxEntityInfo; const AProperties: TArray<TdxMappingMemberInfo>;
      ACriteriaSet: TdxObjectGeneratorCriteriaSet; ABatchWideData: TdxBatchWideDataHolderForModification): TList<TdxModificationStatement>; overload; static;
    class function GenerateUpdate(ABatchWideData: TdxBatchWideDataHolderForModification; AObject: TObject;
      const AProperties: TArray<TdxMappingMemberInfo>): TList<TdxModificationStatement>; overload; static;
  end;

  { TdxInsertQueryGenerator }

  TdxInsertQueryGenerator = class(TdxBaseObjectQueryGenerator)
  strict private
    FAutoIncrement: Boolean;
    function GetRoot: TdxInsertStatement; inline;
  protected
    procedure InitData; override;
    procedure AddParameter(const AParameter: IdxOperandValue); override;
    function ShouldPersist(AMember: TdxMappingMemberInfo): Boolean; override;
    procedure InternalGenerateSQL(const ACriteria: IdxCriteriaOperator); override;
    function CreateRootStatement(ATable: TdxDBTable; const AAlias: string): TdxBaseStatement; override;

    property Root: TdxInsertStatement read GetRoot;
  public
    class function GenerateInsert(AObject: TObject; const AProperties: TArray<TdxMappingMemberInfo>): TList<TdxModificationStatement>; overload; static;
    class function GenerateInsert(ABatchWideData: TdxBatchWideDataHolderForModification; AObject: TObject;
      const AProperties: TArray<TdxMappingMemberInfo>): TList<TdxModificationStatement>; overload; static;
  end;

  { TdxPlanAliasCriteriaInfo }

  TdxPlanAliasCriteriaInfo = class
  strict private
    FAliases: TArray<string>;
    FCriteria: TdxCriteriaOperatorList;
  public
    constructor Create(const AAliases: TArray<string>; ACriteria: TdxCriteriaOperatorList);
    destructor Destroy; override;
    property Aliases: TArray<string> read FAliases;
    property Criteria: TdxCriteriaOperatorList read FCriteria;
  end;

implementation

uses
  TypInfo,
  dxCore,
  dxStringHelper,
  dxEMF.Strs,
  dxEMF.DB.Utils,
  dxEMF.Utils.Evaluator,
  dxEMF.DB.SQLConnectionProvider,
  dxEMF.Utils.Exceptions,
  dxEMF.Serializer,
  dxEMF.Linq.Expressions;

type

  TdxEntityManagerHelper = class helper for TdxEntityManager
  public
    function QueryEntityInfo(const AValue: TValue): TdxEntityInfo; overload;
  end;

  TdxQueryOperandHelper = class helper for TdxQueryOperand
  public
    class function Create(AColumn: TdxDBColumn; const ANodeAlias: string;
      const AParentCriteria: IdxCriteriaOperator = nil): IdxQueryOperand; overload; static;
  end;

  { TdxGetRangeHelper }

  TdxGetRangeHelper = class
  public
    class function GetRange(ASrc: TList<TObject>; AIndex, ACount: Integer): TArray<TObject>; overload; static;
    class function GetRange(const ASrc: array of TObject; AIndex, ACount: Integer): TArray<TObject>; overload; static;
  end;

  { TdxQueryOperandCollector }

  TdxQueryOperandCollector = class(TcxIUnknownObject, IdxQueryCriteriaVisitor)
  strict private
    FOperandList: TList<IdxQueryOperand>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Visit(const AOperand: IdxQuerySubQueryContainer); overload;
    procedure Visit(const AOperand: IdxQueryOperand); overload;
    procedure Visit(const AOperator: IdxFunctionOperator); overload;
    procedure Visit(const AOperand: IdxOperandValue); overload;
    procedure Visit(const AOperator: IdxGroupOperator); overload;
    procedure Visit(const AOperator: IdxInOperator); overload;
    procedure Visit(const AOperator: IdxUnaryOperator); overload;
    procedure Visit(const AOperator: IdxBinaryOperator); overload;
    procedure Visit(const AOperator: IdxBetweenOperator); overload;
    procedure ProcessNode(ANode: TdxJoinNode);
    procedure ProcessList(const AOperands: IdxCriteriaOperatorCollection);
    procedure Process(const AOperand: IdxCriteriaOperator);

    property OperandList: TList<IdxQueryOperand> read FOperandList;
  end;

  { TdxProjectionAliasPatcher }

  TdxProjectionAliasPatcher = class
  strict private
    FRoot: TdxBaseStatement;
    FProjectedNodes: TdxNamedJoinNodeDictionary;
    FProjectionNodes: TdxNamedOrdinalDictionary<TdxProjectionNodeItem>;
    FUsedProjectionNodes: TdxHashSetString;
    FQueryOperandCollector: TdxQueryOperandCollector;
  protected
    procedure PatchInternal(AStatement: TdxBaseStatement);
  public
    constructor Create(ARoot: TdxBaseStatement; AProjectedNodes: TdxNamedJoinNodeDictionary;
      AProjectionNodes: TdxNamedOrdinalDictionary<TdxProjectionNodeItem>);
    destructor Destroy; override;
    class procedure Patch(ARoot: TdxBaseStatement; AProjectedNodes: TdxNamedJoinNodeDictionary;
      AProjectionNodes: TdxNamedOrdinalDictionary<TdxProjectionNodeItem>); overload; static;
    procedure Patch; overload;
  end;

  { TdxPathInfo }

  TdxPathInfo = class
  public
    LeftNode: string;
    RightNode: string;
    CriteriaList: TList<TdxPlanAliasCriteriaInfo>;
    function GetCriteria: IdxCriteriaOperator;
  end;

  { TdxPathFinder }

  TdxPathFinder = class
  strict private
    FNodeCount: Integer;
    FRootNode: string;
    FResult: TArray<TdxPathInfo>;
    FPathStack: TStack<TdxPathInfo>;
    FNodeStack: TStack<string>;
    FLeftCriteriaList: TList<TdxPlanAliasCriteriaInfo>;
    FNodeRelations: TDictionary<string, TdxStringList>;
    FLeftNodes: TdxStringBooleanDictionary;
    FUsedNodes: TdxStringBooleanDictionary;
  public
    constructor Create(const ARootNode: string; const ANodes: TArray<string>;
      const ACriteria: TEnumerable<TdxPlanAliasCriteriaInfo>; ANodeRelations: TDictionary<string, TdxStringList>);
    destructor Destroy; override;
    class function Find(const ARootNode: string; const ANodes: TArray<string>;
      ACriteria: TEnumerable<TdxPlanAliasCriteriaInfo>; ANodeRelations: TDictionary<string, TdxStringList>): TArray<TdxPathInfo>; overload; static;
    function Find: TArray<TdxPathInfo>; overload;
    function ProcessNode(const ACurrentNode: string): Boolean;
    procedure ProcessCriteria(APathInfo: TdxPathInfo);
    function MoveUp(const ALeftNode: string; const ARightNode: string): Boolean;
    function MoveDown: Boolean;
  end;

  { TdxStatementNormalizer }

  TdxStatementNormalizer = class
  strict private
    FNodeCriteriaInfoDictionary: TDictionary<string, TList<TdxPlanAliasCriteriaInfo>>;
    FNodeDictionary: TdxNamedJoinNodeDictionary;
    FRoot: TdxBaseStatement;
  protected
    class procedure CollectNodes(ANode: TdxJoinNode; ACollectedNodes: TdxNamedJoinNodeDictionary); static;
    class procedure ClearNodes(ANode: TdxJoinNode); static;
    procedure FindNodes(ANode: TdxJoinNode);
    procedure FindNode(ASubNode: TdxJoinNode; AParentNode: TdxJoinNode);
    procedure NormalizeNodeTree(AParentNode: TdxJoinNode; ASubNode: TdxJoinNode; ACollectedNodes: TdxNamedJoinNodeDictionary);
  public
    constructor Create(ARoot: TdxBaseStatement);
    destructor Destroy; override;
    procedure ProcessStatement;
    class procedure Normalize(AStatement: TdxBaseStatement); static;
  end;

  { TdxSubSelectQueryGenerator }

  TdxSubSelectQueryGenerator = class(TdxBaseQueryGenerator)
  strict private
    FPropertyName: TArray<string>;
    FParent: TdxBaseQueryGenerator;
    FAggregateProperty: IdxCriteriaOperator;
  protected
    procedure InternalGenerateSQL(const ACriteria: IdxCriteriaOperator); override;
    procedure AddSelectValue;
    function GetNextNodeAlias: string; override;
    function GetPropertyNode(const AProperty: IdxOperandProperty; AType: TdxJoinType): IdxPropertyAlias; override;
    function CreateRootStatement(ATable: TdxDBTable; const AAlias: string): TdxBaseStatement; override;
  public
    constructor Create(AParent: TdxBaseQueryGenerator; ABatchWideData: TdxBatchWideDataHolder; const APropertyName: string;
      AObjectInfo: TdxEntityInfo; const AAggregateProperty: IdxCriteriaOperator; AAggregate: TdxAggregateFunctionType;
      ACollectionCriteriaPatcher: TdxCollectionCriteriaPatcher);
    function GenerateSelect(const ACriteria: IdxCriteriaOperator): TdxBaseStatement;
  end;

  { TdxSingleAggregateItemComparer }

  TdxSingleAggregateItemComparer = class(TEqualityComparer<TdxSingleAggregateItem>)
  public
    function Equals(const ALeftItem, ARightItem: TdxSingleAggregateItem): Boolean; override;
    function GetHashCode(const AValue: TdxSingleAggregateItem): Integer; override;
  end;

{ TdxEntityManagerHelper }

function TdxEntityManagerHelper.QueryEntityInfo(const AValue: TValue): TdxEntityInfo;
begin
  if AValue.IsEmpty or not AValue.IsObject then
    Exit(nil);
  Result := GetEntityInfo(AValue.AsObject.ClassType);
end;

{ TdxSubSelectQueryGenerator }

constructor TdxSubSelectQueryGenerator.Create(AParent: TdxBaseQueryGenerator; ABatchWideData: TdxBatchWideDataHolder;
  const APropertyName: string; AObjectInfo: TdxEntityInfo; const AAggregateProperty: IdxCriteriaOperator;
  AAggregate: TdxAggregateFunctionType; ACollectionCriteriaPatcher: TdxCollectionCriteriaPatcher);
begin
  inherited Create(AObjectInfo, ABatchWideData, ACollectionCriteriaPatcher);
  FAggregateProperty := AAggregateProperty;
  FPropertyName := TdxMemberInfoCollection.SplitPath(APropertyName);
  FParent := AParent;
end;

procedure TdxSubSelectQueryGenerator.InternalGenerateSQL(const ACriteria: IdxCriteriaOperator);
var
  APatchedCriteria: IdxCriteriaOperator;
begin
  APatchedCriteria := PatchCriteria(ACriteria);
  BuildAssociationTree(APatchedCriteria);
  AddSelectValue;
end;

procedure TdxSubSelectQueryGenerator.AddSelectValue;
var
  ARes: IdxCriteriaOperator;
  AParam: IdxPropertyAlias;
begin
  if FAggregateProperty <> nil then
  begin
    ARes := Process(FAggregateProperty);
    if Supports(ARes, IdxPropertyAlias, AParam) then
      Root.Operands.Add(GetQueryOperandFromAlias(AParam, AParam, 0))
    else
      Root.Operands.Add(ARes);
  end;
end;

function TdxSubSelectQueryGenerator.GetNextNodeAlias: string;
begin
  Result := FParent.GetNextNodeAlias;
end;

function TdxSubSelectQueryGenerator.GetPropertyNode(const AProperty: IdxOperandProperty; AType: TdxJoinType): IdxPropertyAlias;
begin
  Result := IdxPropertyAlias(ExecuteWithPropertyNameDiving(AProperty.PropertyName,
    function (APropertyPath: string): IdxCriteriaOperator
    var
      I, ALength: Integer;
      AOperandProperty: IdxOperandProperty;
    begin
      if {$IFDEF DELPHIXE3}APropertyPath.StartsWith('^.'){$ELSE}TdxStringHelper.StartsWith(APropertyPath, '^.'){$ENDIF} then
      begin
        ALength := Length(FPropertyName);
        I := {$IFDEF DELPHIXE3}APropertyPath.LastIndexOf('^.'){$ELSE}TdxStringHelper.LastIndexOf(APropertyPath, '^.'){$ENDIF} div 2 + 1;
        if I >= ALength then
          APropertyPath := {$IFDEF DELPHIXE3}APropertyPath.Remove(0, ALength * 2){$ELSE}TdxStringHelper.Remove(APropertyPath, 0, ALength * 2){$ENDIF}
        else
        begin
          {$IFDEF DELPHIXE3}
          APropertyPath := APropertyPath.Remove(0, I * 2 - 1);
          APropertyPath := string.Join('.', FPropertyName, 0, ALength - I) + APropertyPath;
          {$ELSE}
          APropertyPath := TdxStringHelper.Remove(APropertyPath, 0, I * 2 - 1);
          APropertyPath := TdxStringHelper.Join('.', FPropertyName, 0, ALength - I) + APropertyPath;
          {$ENDIF};
        end;
        AOperandProperty := TdxOperandProperty.Create(APropertyPath);
        Result := FParent.GetPropertyNode(AOperandProperty, AType);
      end
      else
      begin
        AOperandProperty := TdxOperandProperty.Create(APropertyPath);
        Result := inherited GetPropertyNode(AOperandProperty, AType);
      end;
    end,
    False));
end;

function TdxSubSelectQueryGenerator.GenerateSelect(const ACriteria: IdxCriteriaOperator): TdxBaseStatement;
begin
  Result := GenerateSQL(ACriteria);
end;

function TdxSubSelectQueryGenerator.CreateRootStatement(ATable: TdxDBTable; const AAlias: string): TdxBaseStatement;
begin
  Result := TdxSelectStatement.Create(ATable, AAlias);
end;

{ TdxSingleAggregateItem }

constructor TdxSingleAggregateItem.Create(const ACondition: IdxCriteriaOperator; AEntityInfo: TdxEntityInfo);
begin
  Condition := ACondition;
  EntityInfo := AEntityInfo;
end;

{ TdxPlanAliasCriteriaInfo }

constructor TdxPlanAliasCriteriaInfo.Create(const AAliases: TArray<string>; ACriteria: TdxCriteriaOperatorList);
begin
  inherited Create;
  FAliases := AAliases;
  FCriteria := ACriteria;
end;

destructor TdxPlanAliasCriteriaInfo.Destroy;
begin
  FreeAndNil(FCriteria);
  inherited Destroy;
end;

{ TdxMemberPathOperand }

constructor TdxMemberPathOperand.Create(APath: TdxMemberInfoCollection);
begin
  inherited Create;
  FMic := APath;
end;

{ TdxMemberOperand }

constructor TdxMemberOperand.Create(AMember: TdxMappingMemberInfo);
begin
  inherited Create;
  FMember := AMember;
end;

function TdxMemberOperand.Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator;
begin
  raise ENotSupportedException.Create('');
end;

function TdxMemberOperand.Accept(const AVisitor: IdxSQLGeneratorVisitor): string;
begin
  raise ENotSupportedException.Create('');
end;

function TdxMemberOperand.GetMember: TdxMappingMemberInfo;
begin
  Result := FMember;
end;

{ TdxPropertyAlias }

constructor TdxPropertyAlias.Create(ANode: TdxJoinNode; AMember: TdxMappingMemberInfo);
begin
  inherited Create(AMember);
  FNode := ANode;
end;

constructor TdxPropertyAlias.Create(ANode: TdxJoinNode; AMember: TdxMappingMemberInfo; ASubMembers: TdxMemberInfoList;
  const APrefix: string);
begin
  Create(ANode, AMember);
  FSubMembers := ASubMembers;
  FPrefix := APrefix;
end;

constructor TdxPropertyAlias.Create(ANode: TdxJoinNode; AMember: TdxMappingMemberInfo; ASub: Boolean);
begin
  Create(ANode, AMember);
  FSubMembers := AMember;
end;

class function TdxPropertyAlias.GetEmpty: IdxPropertyAlias;
begin
  if FEmpty = nil then
    FEmpty := TdxPropertyAlias.Create;
  Result := FEmpty;
end;

function TdxPropertyAlias.GetMember(I: Integer): TdxMappingMemberInfo;
begin
  if FSubMembers = nil then
    raise EArgumentException.CreateFmt(sdxIncorrectPathNonReferenceMember,
      [Member.Owner.FullName, Member.MemberName]);
  if FSubMembers is TdxMappingMemberInfo then
    Result := TdxMappingMemberInfo(FSubMembers)
  else
    Result := (TdxMemberInfoList(FSubMembers))[I];
end;

function TdxPropertyAlias.GetMappingField(I: Integer): string;
begin
  Result := FPrefix + GetMember(I).ColumnName;
end;

function TdxPropertyAlias.GetNode: TdxJoinNode;
begin
  Result := FNode;
end;

function TdxPropertyAlias.GetCount: Integer;
begin
  if FSubMembers = nil then
    Exit(0);
  if FSubMembers is TdxMemberInfoList then
    Result := (TdxMemberInfoList(FSubMembers)).Count
  else
    Result := 1;
end;

//function TdxPropertyAlias.Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator;
//begin
//  raise ENotSupportedException.Create('');
//end;
//
//function TdxPropertyAlias.Accept(const AVisitor: IdxSQLGeneratorVisitor): string;
//begin
//  raise ENotSupportedException.Create('');
//end;

{ TdxStatementNormalizer }

constructor TdxStatementNormalizer.Create(ARoot: TdxBaseStatement);
begin
  inherited Create;
  FNodeDictionary := TdxNamedJoinNodeDictionary.Create(False);
  FNodeCriteriaInfoDictionary := TObjectDictionary<string, TList<TdxPlanAliasCriteriaInfo>>.Create([doOwnsValues]);
  FRoot := ARoot;
  FNodeDictionary.Add(ARoot.Alias, ARoot);
end;

destructor TdxStatementNormalizer.Destroy;
begin
  FreeAndNil(FNodeDictionary);
  FreeAndNil(FNodeCriteriaInfoDictionary);
  inherited Destroy;
end;

procedure TdxStatementNormalizer.ProcessStatement;
begin
  FindNodes(FRoot);
end;

procedure TdxStatementNormalizer.FindNodes(ANode: TdxJoinNode);
var
  ASubNodes: TArray<TdxJoinNode>;
  ASubNode: TdxJoinNode;
begin
  ASubNodes := ANode.SubNodes.ToArray;
  for ASubNode in ASubNodes do
    FindNode(ASubNode, ANode);
end;

procedure TdxStatementNormalizer.FindNode(ASubNode: TdxJoinNode; AParentNode: TdxJoinNode);
var
  AMissedNode, AAlias: string;
  AResult: TList<TdxPlanAliasCriteriaInfo>;
  AItem: TdxPlanAliasCriteriaInfo;
  ACollectedNodes: TdxNamedJoinNodeDictionary;
  AValues: TDictionary<string, TdxPlanAliasCriteriaInfo>;
begin
  AMissedNode := '';
  FNodeDictionary.AddOrSetValue(ASubNode.Alias, ASubNode);
  if not FNodeCriteriaInfoDictionary.TryGetValue(ASubNode.Alias, AResult) then
  begin
    AValues := TdxNodeCriteriaFinder.FindCriteria('', ASubNode.Condition);
    try
      AResult := TObjectList<TdxPlanAliasCriteriaInfo>.Create;
      AResult.AddRange(AValues.Values);
      FNodeCriteriaInfoDictionary.Add(ASubNode.Alias, AResult);
    finally
      AValues.Free;
    end;
  end;
  for AItem in AResult do
  begin
    for AAlias in AItem.Aliases do
      if not FNodeDictionary.ContainsKey(AAlias) then
      begin
        AMissedNode := AAlias;
        Break;
      end;
    if AMissedNode <> '' then
      Break;
  end;
  if AMissedNode <> '' then
  begin
    ACollectedNodes := TdxNamedJoinNodeDictionary.Create(False);
    CollectNodes(ASubNode, ACollectedNodes);
    if ACollectedNodes.ContainsKey(AMissedNode) then
    begin
      ClearNodes(ASubNode);
      AParentNode.SubNodes.Remove(ASubNode);
      NormalizeNodeTree(AParentNode, ASubNode, ACollectedNodes);
      Exit;
    end;
  end;
  FindNodes(ASubNode);
end;

class procedure TdxStatementNormalizer.CollectNodes(ANode: TdxJoinNode; ACollectedNodes: TdxNamedJoinNodeDictionary);
var
  ASubNodes: TdxJoinNodeCollection;
  I: Integer;
  ASubNode: TdxJoinNode;
begin
  ACollectedNodes[ANode.Alias] := ANode;
  ASubNodes := ANode.SubNodes;
  for I := ASubNodes.Count - 1 downto 0 do
  begin
    ASubNode := ASubNodes[I];
    CollectNodes(ASubNode, ACollectedNodes);
  end;
end;

class procedure TdxStatementNormalizer.ClearNodes(ANode: TdxJoinNode);
var
  ASubNodes: TdxJoinNodeCollection;
  I: Integer;
  ASubNode: TdxJoinNode;
begin
  ASubNodes := ANode.SubNodes;
  for I := ASubNodes.Count - 1 downto 0 do
  begin
    ASubNode := ASubNodes[I];
    ASubNodes.Delete(I);
    ClearNodes(ASubNode);
  end;
end;

procedure TdxStatementNormalizer.NormalizeNodeTree(AParentNode: TdxJoinNode; ASubNode: TdxJoinNode; ACollectedNodes: TdxNamedJoinNodeDictionary);
var
  ANodeRelations: TDictionary<string, TdxStringList>;
  ACriteriaList: TList<TdxPlanAliasCriteriaInfo>;
  ANode: TdxJoinNode;
  AInfoList: TList<TdxPlanAliasCriteriaInfo>;
  AInfo: TdxPlanAliasCriteriaInfo;
  AAliases: TArray<string>;
  I, J: Integer;
  ACurrentAlias: string;
  ARelations, AParentNodeRelationList: TdxStringList;
  AResult: TArray<TdxPathInfo>;
  AValues: TDictionary<string, TdxPlanAliasCriteriaInfo>;
begin
  ANodeRelations := TDictionary<string, TdxStringList>.Create;
  ACriteriaList := TList<TdxPlanAliasCriteriaInfo>.Create;
  for ANode in ACollectedNodes.Values do
  begin
    if not FNodeCriteriaInfoDictionary.TryGetValue(ANode.Alias, AInfoList) then
    begin
      AValues := TdxNodeCriteriaFinder.FindCriteria('', ANode.Condition);
      try
        AInfoList := TObjectList<TdxPlanAliasCriteriaInfo>.Create;
        AInfoList.AddRange(AValues.Values);
        FNodeCriteriaInfoDictionary.Add(ANode.Alias, AInfoList);
      finally
        AValues.Free;
      end;
    end;
    for AInfo in AInfoList do
    begin
      AAliases := AInfo.Aliases;
      for I := 0 to Length(AAliases) - 1 do
      begin
        ACurrentAlias := AAliases[I];
        if not ANodeRelations.TryGetValue(ACurrentAlias, ARelations) then
        begin
          ARelations := TdxStringList.Create;
          ANodeRelations.Add(ACurrentAlias, ARelations);
        end;
        for J := 0 to Length(AAliases) - 1 do
        begin
          if I = J then
            Continue;
          ARelations.Add(AAliases[J]);
        end;
      end;
      ACriteriaList.Add(AInfo);
    end;
    ANode.Condition := nil;
  end;
  if not ANodeRelations.TryGetValue(AParentNode.Alias, AParentNodeRelationList) then
  begin
    AParentNodeRelationList := TdxStringList.Create;
    ANodeRelations.Add(AParentNode.Alias, AParentNodeRelationList);
  end;
  if not AParentNodeRelationList.Contains(ASubNode.Alias) then
    AParentNodeRelationList.Add(ASubNode.Alias);
  AResult := TdxPathFinder.Find(AParentNode.Alias, ACollectedNodes.Keys, ACriteriaList, ANodeRelations);
  if AResult = nil then
    raise EInvalidOperation.Create('');
  NotImplemented;
end;

class procedure TdxStatementNormalizer.Normalize(AStatement: TdxBaseStatement);
var
  ANormalizer: TdxStatementNormalizer;
begin
  ANormalizer := TdxStatementNormalizer.Create(AStatement);
  try
    ANormalizer.ProcessStatement;
  finally
    ANormalizer.Free;
  end;
end;

{ TdxBatchWideDataHolder }

constructor TdxBatchWideDataHolder.Create;
begin
  inherited Create;
end;

function TdxBatchWideDataHolder.GetNextTag: Integer;
begin
  Inc(FCurrentTag);
  Result := FCurrentTag;
end;

{ TdxProjectionNodeItem }

constructor TdxProjectionNodeItem.Create(ANode: TdxJoinNode; AProjectedNodes: TdxJoinNodeList);
begin
  FNode := ANode;
  FProjectedNodes := AProjectedNodes;
end;

{ TdxBatchWideDataHolderForSelect }

destructor TdxBatchWideDataHolderForSelect.Destroy;
begin
  FreeAndNil(FParametersByValues);
  FreeAndNil(FParametersByObjects);
  inherited Destroy;
end;

function TdxBatchWideDataHolderForSelect.GetParameter(const AValue: TValue): IdxOperandValue;
var
  AEntityInfo: TdxEntityInfo;
  AResult: IdxParameterValue;
  AObject: TObject;
begin
  if AValue.IsEmpty then
    raise EArgumentNilException.Create(sdxValue);
  AEntityInfo := EntityManager.QueryEntityInfo(AValue);
  if AEntityInfo = nil then
  begin
    if FParametersByValues = nil then
      FParametersByValues := TDictionary<TValue, IdxParameterValue>.Create;
    if not FParametersByValues.TryGetValue(AValue, AResult) then
    begin
      AResult := TdxParameterValue.Create(GetNextTag);
      AResult.Value := AValue;
      FParametersByValues.Add(AValue, AResult);
    end;
    Result := AResult;
  end
  else
  begin
    AObject := AValue.AsObject;
    if FParametersByObjects = nil then
      FParametersByObjects := TdxObjectDictionary<IdxParameterValue>.Create([]);
    if not FParametersByObjects.TryGetValue(AObject, AResult) then
    begin
      AResult := TdxParameterValue.Create(GetNextTag);
      AResult.Value := AEntityInfo.GetKeyValue(AObject);
      FParametersByObjects.Add(AObject, AResult);
    end;
    Result := AResult;
  end;
end;

{ TdxPathFinder }

constructor TdxPathFinder.Create(const ARootNode: string; const ANodes: TArray<string>;
  const ACriteria: TEnumerable<TdxPlanAliasCriteriaInfo>; ANodeRelations: TDictionary<string, TdxStringList>);
var
  ANode: string;
begin
  inherited Create;
  FPathStack := TStack<TdxPathInfo>.Create;
  FNodeStack := TStack<string>.Create;
  FNodeRelations := TDictionary<string, TdxStringList>.Create;
  FLeftNodes := TdxStringBooleanDictionary.Create;
  FUsedNodes := TdxStringBooleanDictionary.Create;
  FRootNode := ARootNode;
  FUsedNodes.Add(ARootNode, True);
  for ANode in ANodes do
  begin
    FLeftNodes.Add(ANode, True);
    Inc(FNodeCount);
  end;
  FLeftCriteriaList := TList<TdxPlanAliasCriteriaInfo>.Create(ACriteria);
  FNodeRelations := ANodeRelations;
end;

destructor TdxPathFinder.Destroy;
begin
  FreeAndNil(FPathStack);
  FreeAndNil(FNodeStack);
  FreeAndNil(FNodeRelations);
  FreeAndNil(FLeftNodes);
  FreeAndNil(FUsedNodes);
  FreeAndNil(FLeftCriteriaList);
  inherited Destroy;
end;

class function TdxPathFinder.Find(const ARootNode: string; const ANodes: TArray<string>;
  ACriteria: TEnumerable<TdxPlanAliasCriteriaInfo>;
  ANodeRelations: TDictionary<string, TdxStringList>): TArray<TdxPathInfo>;
begin
  Result := TdxPathFinder.Create(ARootNode, ANodes, ACriteria, ANodeRelations).Find;
end;

function TdxPathFinder.Find: TArray<TdxPathInfo>;
begin
  FResult := nil;
  ProcessNode(FRootNode);
  Result := FResult;
end;

function TdxPathFinder.ProcessNode(const ACurrentNode: string): Boolean;
var
  ARelations: TdxStringList;
  ARel: string;
begin
  if FPathStack.Count = FNodeCount then
  begin
    if FLeftCriteriaList.Count > 0 then
      Exit(False);
    FResult := TdxArrayHelper<TdxPathInfo>.Reverse(FPathStack.ToArray);
    Exit(True);
  end;
  if FNodeRelations.TryGetValue(ACurrentNode, ARelations) then
  begin
    for ARel in ARelations do
    begin
      if not FLeftNodes.ContainsKey(ARel) then
        Continue;
      if MoveUp(ACurrentNode, ARel) then
        Exit(True);
    end;
  end;
  Result := MoveDown;
end;

procedure TdxPathFinder.ProcessCriteria(APathInfo: TdxPathInfo);
var
  I, J: Integer;
  ACriteriaInfo: TdxPlanAliasCriteriaInfo;
  AAliases: TArray<string>;
  ACanUseCriteria: Boolean;
  AAlias: string;
begin
  for I := FLeftCriteriaList.Count - 1 downto 0 do
  begin
    ACriteriaInfo := FLeftCriteriaList[I];
    AAliases := ACriteriaInfo.Aliases;
    ACanUseCriteria := True;
    for J := 0 to Length(AAliases) - 1 do
    begin
      AAlias := AAliases[J];
      if not FUsedNodes.ContainsKey(AAlias) then
      begin
        ACanUseCriteria := False;
        Break;
      end;
    end;
    if ACanUseCriteria then
    begin
      NotImplemented;
    end;
  end;
end;

function TdxPathFinder.MoveUp(const ALeftNode: string; const ARightNode: string): Boolean;
var
  APathInfo: TdxPathInfo;
begin
  FNodeStack.Push(ALeftNode);
  FLeftNodes.Remove(ARightNode);
  FUsedNodes.Add(ARightNode, True);
  APathInfo := TdxPathInfo.Create;
  ProcessCriteria(APathInfo);
  FPathStack.Push(APathInfo);
  try
    Result := ProcessNode(ARightNode);
  finally
    FPathStack.Pop;
    NotImplemented;
    FUsedNodes.Remove(ARightNode);
    FLeftNodes.Add(ARightNode, True);
    FNodeStack.Pop;
  end;
end;

function TdxPathFinder.MoveDown: Boolean;
var
  ACurrentNode: string;
begin
  if FNodeStack.Count = 0 then
    Exit(False);
  ACurrentNode := FNodeStack.Pop;
  try
    Result := ProcessNode(ACurrentNode);
  finally
    FNodeStack.Push(ACurrentNode);
  end;
end;

{ TdxBatchWideDataHolderForModification }

destructor TdxBatchWideDataHolderForModification.Destroy;
begin
  FreeAndNil(FInsertedObjects);
  FreeAndNil(FIdentityParameters);
  FreeAndNil(FUpdatedMembersBeforeDeleteDictionary);
  FreeAndNil(FQueryOperandsCache);
  inherited Destroy;
end;

procedure TdxBatchWideDataHolderForModification.RegisterDeletedObjects(const AObjects4Delete: TArray<TObject>);
begin
  FDeletedObjects := AObjects4Delete;
end;

function TdxBatchWideDataHolderForModification.CreateIdentityParameter(AObject: TObject): IdxParameterValue;
begin
  if AObject = nil then
    raise EArgumentNilException.Create('');
  if FIdentityParameters = nil then
    FIdentityParameters := TdxObjectDictionary<IdxParameterValue>.Create;

  Result := TdxParameterValue.Create(GetNextTag);
  FIdentityParameters.Add(AObject, Result);
end;

function TdxBatchWideDataHolderForModification.IsObjectAlreadyInserted(AObject: TObject): Boolean;
begin
  if FInsertedObjects = nil then
    Result := False
  else
    Result := FInsertedObjects.Contains(AObject);
end;

procedure TdxBatchWideDataHolderForModification.RegisterInsertedObject(AObject: TObject);
begin
  if FInsertedObjects = nil then
    FInsertedObjects := TdxObjectSet.Create;
  FInsertedObjects.Add(AObject);
end;

function TdxBatchWideDataHolderForModification.GetParameter(const AValue: TValue): IdxOperandValue;
var
  AEntityInfo: TdxEntityInfo;
  AResult: IdxParameterValue;
  AObject: TObject;
begin
  if AValue.IsEmpty then
    raise EArgumentNilException.Create(sdxValue);
  AEntityInfo := EntityManager.QueryEntityInfo(AValue);
  if AEntityInfo = nil then
  begin
    Result := TdxParameterValue.Create(GetNextTag);
    Result.Value := AValue;
  end
  else
  begin
    AObject := AValue.AsObject;
    if (FIdentityParameters <> nil) and FIdentityParameters.TryGetValue(AObject, AResult) then
      Result := AResult
    else
    begin
      Result := TdxParameterValue.Create(GetNextTag);
      Result.Value := AEntityInfo.GetKeyValue(AObject);
    end;
  end;
end;

procedure TdxBatchWideDataHolderForModification.RegisterUpdatedMembersBeforeDelete(AObject: TObject; AMembers: TdxMemberInfoCollection);
begin
  if FUpdatedMembersBeforeDeleteDictionary = nil then
    FUpdatedMembersBeforeDeleteDictionary := TdxObjectDictionary<TdxMemberInfoCollection>.Create;
  FUpdatedMembersBeforeDeleteDictionary[AObject] := AMembers;
end;

function TdxBatchWideDataHolderForModification.TryGetUpdatedMembersBeforeDelete(AObject: TObject; out AMembers: TdxMemberInfoCollection): Boolean;
begin
  if FUpdatedMembersBeforeDeleteDictionary = nil then
  begin
    AMembers := nil;
    Result := False;
  end
  else
    Result := FUpdatedMembersBeforeDeleteDictionary.TryGetValue(AObject, AMembers);
end;

function TdxBatchWideDataHolderForModification.CacheQueryOperand(const AToCache: IdxQueryOperand): IdxQueryOperand;
begin
  if FQueryOperandsCache = nil then
    FQueryOperandsCache := TDictionary<IdxQueryOperand, IdxQueryOperand>.Create;
  if FQueryOperandsCache.TryGetValue(AToCache, Result) then
    Exit;
  FQueryOperandsCache.Add(AToCache, AToCache);
  Result := AToCache;
end;

{ TdxCollectionCriteriaPatcher }

constructor TdxCollectionCriteriaPatcher.Create(ASelectDeleted: Boolean);
begin
  FSelectDeleted := ASelectDeleted;
  FHasValue := True;
end;



function TdxCollectionCriteriaPatcher.PatchCriteria(AEntityInfo: TdxEntityInfo; const AOriginalCriteria: IdxCriteriaOperator)
  : IdxCriteriaOperator;
begin
  Result := AOriginalCriteria;
  if AEntityInfo.ClassAttributes.IsParentTable then
  begin
    Result := TdxGroupOperator.And(TdxBinaryOperator.Create(AEntityInfo.ServiceFields.DiscriminatorField.Discriminator.ColumnName,
      TValue.FromVariant(AEntityInfo.ClassAttributes.DiscriminatorValue)), Result);
  end;
end;

{ TdxPathInfo }

function TdxPathInfo.GetCriteria: IdxCriteriaOperator;
var
  ACriteriaList: TdxCriteriaOperatorList;
  AInfo: TdxPlanAliasCriteriaInfo;
begin
  if (CriteriaList = nil) or (CriteriaList.Count = 0) then
    Exit(nil);
  ACriteriaList := TdxCriteriaOperatorList.Create;
  try
    for AInfo in CriteriaList do
      ACriteriaList.AddRange(AInfo.Criteria);
    if ACriteriaList.Count = 0 then
      Result := ACriteriaList[0]
    else
      Result := TdxGroupOperator.And(ACriteriaList);
  finally
    ACriteriaList.Free;
  end;
end;

{ TdxObjectsQuery }

constructor TdxObjectsQuery.Create(AEntityInfo: TdxEntityInfo; const ACriteria: IdxCriteriaOperator; const ASortByExpressions: IdxSortByExpressions;
  ASkipSelectedRecords: Integer; ATopSelectedRecords: Integer; const ACollectionCriteriaPatcher: TdxCollectionCriteriaPatcher;
  AForce: Boolean);
begin
  inherited Create;
  if AEntityInfo = nil then
    raise EArgumentNilException.Create('');
  FEntityInfo := AEntityInfo;
  FCriteria := ACriteria;
  FSorting := ASortByExpressions;
  FTopSelectedRecords := ATopSelectedRecords;
  FSkipSelectedRecords := ASkipSelectedRecords;
  FCollectionCriteriaPatcher := ACollectionCriteriaPatcher;
  FForce := AForce;
end;

constructor TdxObjectsQuery.Create(AEntityInfo: TdxEntityInfo; const ACriteria: IdxCriteriaOperator; const ASortByExpressions: IdxSortByExpressions;
  ATopSelectedRecords: Integer; const ACollectionCriteriaPatcher: TdxCollectionCriteriaPatcher; AForce: Boolean);
begin
  Create(AEntityInfo, ACriteria, ASortByExpressions, 0, ATopSelectedRecords, ACollectionCriteriaPatcher, AForce);
end;

{ TdxBaseQueryGenerator }

constructor TdxBaseQueryGenerator.Create(AEntityInfo: TdxEntityInfo; ABatchWideData: TdxBatchWideDataHolder;
  const ACollectionCriteriaPatcher: TdxCollectionCriteriaPatcher);
begin
  inherited Create;
  FEntityInfo := AEntityInfo;
  FBatchWideData := ABatchWideData;
  FCollectionCriteriaPatcher := ACollectionCriteriaPatcher;
  FCurrentLeftJoinEnforcer := TdxGroupOperatorType.Or;
end;

constructor TdxBaseQueryGenerator.Create(AEntityInfo: TdxEntityInfo; ABatchWideData: TdxBatchWideDataHolder);
begin
  Create(AEntityInfo, ABatchWideData, Default (TdxCollectionCriteriaPatcher));
end;

destructor TdxBaseQueryGenerator.Destroy;
begin
  FreeAndNil(FRoot);
  FreeAndNil(FProjectedSingleNodes);
  FreeAndNil(FProjectionNodes);
  FreeAndNil(FNodeStackHashSet);
  FreeAndNil(FMultiColumnAliases);
  FreeAndNil(FQueryParameters);
  FreeAndNil(FClassInfoStack);
  FreeAndNil(FNodeStack);
  FreeAndNil(FReferenceSubNodes);
  FreeAndNil(FInheritanceSubNodes);
  FreeAndNil(FSingleAggregateSubNodes);
  FreeAndNil(FNakedSingleNodes);
  inherited Destroy;
end;

function TdxBaseQueryGenerator.CollectInValues(const AOperands: IdxCriteriaOperatorCollection;
  AValues: TdxCriteriaOperatorList): IdxPropertyAlias;
var
  ADefaultMembers: IdxPropertyAlias;
  ACriteria: IdxCriteriaOperator;
  R5AB0F59060745B8A39FE1D073C4ED43: IdxCriteriaOperator;
begin
  ADefaultMembers := TdxPropertyAlias.Empty;
  for ACriteria in AOperands do
  begin
    R5AB0F59060745B8A39FE1D073C4ED43 := Process(ACriteria);
    AValues.Add(R5AB0F59060745B8A39FE1D073C4ED43);
    Supports(R5AB0F59060745B8A39FE1D073C4ED43, IdxPropertyAlias, ADefaultMembers);
  end;
  Result := ADefaultMembers;
end;

function TdxBaseQueryGenerator.ConvertViaDefaultValueConverter(var AOperandValue: TValue): Boolean;
begin
  Result := False;
end;

function TdxBaseQueryGenerator.ExecuteWithPropertyNameDiving(const APropertyName: string;
  const AWorker: TFunc<string, IdxCriteriaOperator>; AThrowOnEmptyStack: Boolean = True): IdxCriteriaOperator;
var
  AInternalClassInfoStack: TStack<TdxEntityInfo>;
  AInternalNodeStack: TStack<TdxJoinNode>;
  ACurrentPropertyName: string;
begin
  Result := nil;
  if not Assigned(AWorker) then
    raise EArgumentNilException.Create('');
  AInternalClassInfoStack := nil;
  AInternalNodeStack := nil;
  try
    ACurrentPropertyName := APropertyName;
    {$IFDEF DELPHIXE3}
    while (ACurrentPropertyName <> '') and ACurrentPropertyName.StartsWith('^.') do
    {$ELSE}
    while (ACurrentPropertyName <> '') and TdxStringHelper.StartsWith(ACurrentPropertyName, '^.') do
    {$ENDIF}
    begin
      if AInternalClassInfoStack = nil then
      begin
        AInternalClassInfoStack := TStack<TdxEntityInfo>.Create;
        AInternalNodeStack := TStack<TdxJoinNode>.Create;
      end;
      if ClassInfoStack.Count = 0 then
      begin
        if AThrowOnEmptyStack then
          raise EInvalidOperation.Create(sdxEmptyStack);
        Break;
      end;
      AInternalClassInfoStack.Push(EntityInfo);
      AInternalNodeStack.Push(FRootNode);
      FEntityInfo := ClassInfoStack.Pop;
      FRootNode := NodeStack.Pop;
      NodeStackHashSet.Remove(FRootNode.Alias);
      {$IFDEF DELPHIXE3}
      ACurrentPropertyName := ACurrentPropertyName.Substring(2);
      {$ELSE}
      ACurrentPropertyName := TdxStringHelper.Substring(ACurrentPropertyName, 2);
      {$ENDIF}
    end;
    Result := AWorker(ACurrentPropertyName);
  finally
    try
      if AInternalClassInfoStack <> nil then
      begin
        while AInternalClassInfoStack.Count > 0 do
        begin
          ClassInfoStack.Push(FEntityInfo);
          NodeStack.Push(FRootNode);
          NodeStackHashSet.Add(FRootNode.Alias);
          FEntityInfo := AInternalClassInfoStack.Pop;
          FRootNode := AInternalNodeStack.Pop;
        end;
      end;
    finally
      AInternalClassInfoStack.Free;
      AInternalNodeStack.Free;
    end;
  end;
end;

procedure TdxBaseQueryGenerator.AddMultiColumnAlias(const AOperator: IdxCriteriaOperator; const AAlias: IdxPropertyAlias);
begin
  if FMultiColumnAliases = nil then
    FMultiColumnAliases := TDictionary<IdxCriteriaOperator, IdxPropertyAlias>.Create;
  FMultiColumnAliases.AddOrSetValue(AOperator, AAlias);
end;

function TdxBaseQueryGenerator.AppendInheritanceJoinNode(ABranch: TdxEntityInfo; AProperty: TdxMappingMemberInfo;
  APrevNode: TdxJoinNode): TdxJoinNode;
var
  AUpClasses,
  AClasses: TList<TdxEntityInfo>;
  AUpCast: Boolean;
  ATop: TdxEntityInfo;
  APos,
  I: Integer;
  AOnClauseAlias: string;
  AMappingTable: TdxDBTable;
  AProjectionNode: TdxJoinNode;
  AInnerNode: TdxSelectStatement;
  AProjection: TdxDBProjection;
  AProjectionNodeItem: TdxProjectionNodeItem;
  AInheritedCache: TDictionary<TdxDBTable, TdxJoinNode>;
  AJoinType: TdxJoinType;
  AJoinNodeList: TdxJoinNodeList;
begin
  AUpCast := not ABranch.IsAssignableTo(AProperty.Owner);
  AClasses := TList<TdxEntityInfo>.Create;
  try
    Result := nil;
    if AUpCast then
    begin
      AUpClasses := TList<TdxEntityInfo>.Create;
      try
        ATop := AProperty.Owner;
        repeat
          AUpClasses.Add(ATop);
          ATop := ATop.BaseEntityInfo;
        until ATop = nil;
        AUpClasses.Reverse;
        APos := AUpClasses.IndexOf(ABranch);
        while APos < 0 do
        begin
          AClasses.Add(ABranch);
          ABranch := ABranch.BaseEntityInfo;
          if ABranch = nil then
            raise Exception.Create('');
          APos := AUpClasses.IndexOf(ABranch);
        end;
        AUpClasses.DeleteRange(0, APos);
        AClasses.AddRange(AUpClasses);
      finally
        AUpClasses.Free;
      end;
    end
    else
      repeat
        ABranch := ABranch.ParentEntity;
        if ABranch = nil then
          Break;
        AClasses.Add(ABranch);
      until AProperty.IsMappingClass(ABranch);
    AOnClauseAlias := APrevNode.Alias;

    for I := 0 to AClasses.Count - 1 do
    begin
      ABranch := AClasses[I];

      AMappingTable := ABranch.DBTable;
      if (APrevNode.Table = AMappingTable) or
        (((FProjectionNodes <> nil) and FProjectionNodes.TryGetValue(APrevNode.Alias, AProjectionNodeItem)) and
           AProjectionNodeItem.ProjectedNodes.Any(function (AJoinNode: TdxJoinNode): Boolean
             begin
               Result := AJoinNode.Table.Equals(AMappingTable)
             end))
      then
        Result := APrevNode
      else
      begin
        if ((FNakedSingleNodes <> nil) and FNakedSingleNodes.ContainsKey(APrevNode.Alias)) and
          ((FProjectedSingleNodes = nil) or (not FProjectedSingleNodes.ContainsKey(APrevNode.Alias))) then
        begin
          if FProjectedSingleNodes = nil then
            FProjectedSingleNodes := TdxNamedJoinNodeDictionary.Create(False);
          if FProjectionNodes = nil then
            FProjectionNodes := TdxNamedOrdinalDictionary<TdxProjectionNodeItem>.Create;
          AProjectionNode := APrevNode;
          AInnerNode := TdxSelectStatement.Create(APrevNode.Table, APrevNode.Alias);
          AInnerNode.SubNodes.AddRange(APrevNode.SubNodes);
          AProjection := TdxDBProjection.Create(AInnerNode);
          AProjectionNode.Alias := GetNextNodeAlias;
          AProjectionNode.Table := AProjection;
          FProjectedSingleNodes.Add(AInnerNode.Alias, AProjectionNode);
          APrevNode := AInnerNode;

          AJoinNodeList := TdxJoinNodeList.Create;
          AJoinNodeList.Add(AInnerNode);
          FProjectionNodes.Add(AProjectionNode.Alias, TdxProjectionNodeItem.Create(AProjectionNode, AJoinNodeList));
        end;

        if FInheritanceSubNodes = nil then
          FInheritanceSubNodes := TObjectDictionary<string, TDictionary<TdxDBTable, TdxJoinNode>>.Create([doOwnsValues]);
        if not FInheritanceSubNodes.TryGetValue(APrevNode.Alias, AInheritedCache) then
        begin
          AInheritedCache := TDictionary<TdxDBTable, TdxJoinNode>.Create;
          FInheritanceSubNodes.Add(APrevNode.Alias, AInheritedCache);
        end;
        if not AInheritedCache.TryGetValue(AMappingTable, Result) then
        begin
          if AUpCast then
            AJoinType := TdxJoinType.LeftOuter
          else
            AJoinType := APrevNode.&Type;
          Result := TdxJoinNode.Create(AMappingTable, GetNextNodeAlias, AJoinType);
          Result.Condition := GetJoinCondition(AOnClauseAlias, Result.Alias, ABranch.KeyProperty.Member, ABranch.KeyProperty.Member);
          APrevNode.SubNodes.Add(Result);
          AInheritedCache.Add(AMappingTable, Result);
          TryAddNodeIntoProjection(APrevNode, Result);
        end;
        AOnClauseAlias := Result.Alias;
      end;
    end;
  finally
    AClasses.Free;
  end;
end;

function TdxBaseQueryGenerator.AppendJoinNode(AProperty: TdxMappingMemberInfo; APrevNode: TdxJoinNode; AType: TdxJoinType)
  : TdxJoinNode;
var
  AJoinCache: TdxNamedJoinNodeDictionary;
  ANode: TdxJoinNode;
  ANodeType: TdxJoinType;
begin
  if FReferenceSubNodes = nil then
    FReferenceSubNodes := TObjectDictionary<string, TdxNamedJoinNodeDictionary>.Create([doOwnsValues]);
  if not FReferenceSubNodes.TryGetValue(APrevNode.Alias, AJoinCache) then
  begin
    AJoinCache := TdxNamedJoinNodeDictionary.Create(False);
    FReferenceSubNodes.Add(APrevNode.Alias, AJoinCache);
  end;
  if AJoinCache.TryGetValue(AProperty.MappingField, ANode) then
  begin
    if (AType <> TdxJoinType.Inner) and (ANode.&Type <> AType) then
      SetJoinTypeWithSubNodes(ANode, AType);
  end
  else
  begin
    if AType = APrevNode.&Type then
      ANodeType := AType
    else
      ANodeType := TdxJoinType.LeftOuter;
    ANode := TdxJoinNode.Create(AProperty.ReferenceType.DBTable, GetNextNodeAlias, ANodeType);
    ANode.Condition := GetJoinCondition(APrevNode.Alias, ANode.Alias, AProperty, AProperty.ReferenceType.KeyAttributes[0]) as TdxCriteriaOperator;
    APrevNode.SubNodes.Add(ANode);
    AJoinCache.Add(AProperty.MappingField, ANode);
    TryAddNodeIntoProjection(APrevNode, ANode);
  end;
  Result := ANode;
end;

procedure TdxBaseQueryGenerator.BuildAssociationTree(const ACriteria: IdxCriteriaOperator);
begin
  Root.Condition := ProcessLogical(ACriteria as TdxCriteriaOperator);
end;

function TdxBaseQueryGenerator.BuildInGroup(const ADefaultMembers: IdxPropertyAlias; AValues: TdxCriteriaOperatorList;
  const ALeftOperator: IdxCriteriaOperator): IdxCriteriaOperator;
var
  ABinaryGroup: IdxCriteriaOperator;
  AValue: IdxCriteriaOperator;
  ARightMembers: IdxPropertyAlias;
begin
  Result := nil;
  for AValue in AValues do
  begin
    if not Supports(AValue, IdxPropertyAlias, ARightMembers) then
      ARightMembers := ADefaultMembers;
    ABinaryGroup := CreateBinaryGroup(ADefaultMembers, ALeftOperator, ADefaultMembers, AValue, ARightMembers,
      TdxBinaryOperatorType.Equal);
    Result := TdxGroupOperator.Or(Result, ABinaryGroup);
  end;
end;


function TdxBaseQueryGenerator.CreateBinary(const ALeftOperator: IdxCriteriaOperator; const ALeftMembers: IdxPropertyAlias;
  const ARightOperator: IdxCriteriaOperator; const ARightMembers: IdxPropertyAlias; AOperatorType: TdxBinaryOperatorType;
  AIndex: Integer): IdxBinaryOperator;
begin
  Result := TdxBinaryOperator.Create(GetParameter(ALeftOperator, ALeftMembers, AIndex),
    GetParameter(ARightOperator, ARightMembers, AIndex), AOperatorType);
end;

function TdxBaseQueryGenerator.CreateBinary(AType: TdxBinaryOperatorType;
  const ALeftOperator, ARightOperator: IdxCriteriaOperator): IdxCriteriaOperator;
var
  ALeft, ARight: IdxCriteriaOperator;
  AAlias: IdxPropertyAlias;
  ADefaultMembers, ALeftMembers: IdxPropertyAlias;
begin
  ALeft := ALeftOperator;
  ARight := ARightOperator;
  if not Supports(ALeft, IdxPropertyAlias) and TryGetMultiColumnAlias(ALeft, AAlias) then
    ALeft := TdxCriteriaOperator(AAlias);
  if not Supports(ARight, IdxPropertyAlias) and TryGetMultiColumnAlias(TdxCriteriaOperator(ARight), AAlias) then
    ARight := TdxCriteriaOperator(AAlias);
  ADefaultMembers := TdxPropertyAlias.Empty;
  if Supports(ALeft, IdxPropertyAlias) then
    ADefaultMembers := IdxPropertyAlias(ALeft);
  if Supports(ARight, IdxPropertyAlias) then
    ADefaultMembers := IdxPropertyAlias(ARight);
  if Supports(ALeft, IdxPropertyAlias) then
    ALeftMembers := IdxPropertyAlias(ALeft)
  else
    ALeftMembers := ADefaultMembers;
  if ADefaultMembers.Count > 1 then
    Result := CreateBinaryGroup(ADefaultMembers, ALeft, ALeftMembers, ARight, ADefaultMembers, AType)
  else
    Result := CreateBinary(ALeft, ALeftMembers, ARight, ADefaultMembers, AType, 0);
end;

function TdxBaseQueryGenerator.CreateBinaryGroup(const ASubMembers: IdxPropertyAlias; const ALeftOperator: IdxCriteriaOperator;
  const ALeftMembers: IdxPropertyAlias; const ARightOperator: IdxCriteriaOperator; const ARightMembers: IdxPropertyAlias; AOperatorType: TdxBinaryOperatorType)
  : IdxCriteriaOperator;
var
  ASubMemberResult: IdxCriteriaOperator;
  I: Integer;
begin
  if AOperatorType <> TdxBinaryOperatorType.Equal then
    raise ENotSupportedException.Create(TdxFormatterHelper.GetName(AOperatorType));
  Result := nil;
  for I := 0 to ASubMembers.Count - 1 do
  begin
    ASubMemberResult := CreateBinary(ALeftOperator, ALeftMembers, ARightOperator, ARightMembers, AOperatorType, I);
    Result := TdxGroupOperator.And(Result, ASubMemberResult);
  end;
end;

function TdxBaseQueryGenerator.CreateOperand(AMember: TdxMappingMemberInfo; const ANodeAlias: string): IdxQueryOperand;
begin
  Result := TdxQueryOperand.Create(AMember.MappingField, ANodeAlias);
end;

function TdxBaseQueryGenerator.CreateOperand(const AMappingField, ANodeAlias: string): IdxQueryOperand;
begin
  Result := TdxQueryOperand.Create(AMappingField, ANodeAlias);
end;

function TdxBaseQueryGenerator.GenerateSQL(const ACriteria: IdxCriteriaOperator): TdxBaseStatement;
begin
  FQueryParameters.Free;
  FQueryParameters := TdxQueryParameterCollection.Create;
  InitData;
  InternalGenerateSQL(ACriteria);
  if FRoot.Alias <> '' then
  begin
    TdxProjectionAliasPatcher.Patch(FRoot, FProjectedSingleNodes, FProjectionNodes);
    TdxStatementNormalizer.Normalize(FRoot);
  end;
  Result := FRoot;
  FRoot := nil;
end;

function TdxBaseQueryGenerator.GetClassInfoStack: TStack<TdxEntityInfo>;
begin
  if FClassInfoStack = nil then
    FClassInfoStack := TStack<TdxEntityInfo>.Create;
  Result := FClassInfoStack;
end;

function TdxBaseQueryGenerator.GetConstParameter(AMember: TdxMappingMemberInfo; const AOperand: IdxOperandValue;
  ATargetMember: TdxMappingMemberInfo): IdxOperandValue;
var
  AValue: TValue;
  AEntityInfo: TdxEntityInfo;
  AConverter: TdxValueConverter;
begin
  AValue := AOperand.Value;
  AEntityInfo := EntityManager.QueryEntityInfo(AValue);
  if ((AEntityInfo <> nil) and (AEntityInfo.KeyProperty <> nil)) and not AEntityInfo.KeyProperty.IsIdentity then
    AValue := AEntityInfo.GetKeyValue(AValue.AsObject);
  AConverter := AMember.Converter;
  if AConverter <> nil then
    AValue := AConverter.ConvertToStorageType(AValue)
  else
    ConvertViaDefaultValueConverter(AValue);
  if AOperand is TdxConstantValue then
    Result := TdxConstantValue.Create(AValue)
  else
    Result := GetConstParameter(AValue);
end;

function TdxBaseQueryGenerator.GetConstParameter(const AValue: TValue): IdxOperandValue;
begin
  if AValue.IsEmpty then
    Result := TdxOperandValue.Create
  else
      Result := BatchWideData.GetParameter(AValue)
end;

function TdxBaseQueryGenerator.GetJoinCondition(const ALeftAlias, ARightAlias: string;
  ALeftMember, ARightMember: TdxMappingMemberInfo): IdxCriteriaOperator;
begin
  Result := TdxBinaryOperator.Create(CreateOperand(ALeftMember, ALeftAlias), CreateOperand(ARightMember, ARightAlias),
    TdxBinaryOperatorType.Equal);
end;

function TdxBaseQueryGenerator.GetMembers(AMember: TdxMappingMemberInfo; out APrefix: string): TdxMemberInfoList;
begin
  APrefix := '';
    Result := nil;
end;

function TdxBaseQueryGenerator.GetMembers(ANode: TdxJoinNode; AMember: TdxMappingMemberInfo): IdxPropertyAlias;
var
  APrefix: string;
  AMembers: TdxMemberInfoList;
begin
  AMembers := GetMembers(AMember, APrefix);
  if AMembers <> nil then
    Result := TdxPropertyAlias.Create(ANode, AMember, AMembers, APrefix)
  else
    Result := TdxPropertyAlias.Create(ANode, AMember, True);
end;

function TdxBaseQueryGenerator.GetNextNodeAlias: string;
var
  ALength, I: Integer;
begin
  ALength := Length(FNodeAliasCache);
  if ALength <= FIndexCount then
  begin
    SetLength(FNodeAliasCache, FIndexCount + 4);
    for I := ALength to Length(FNodeAliasCache) - 1 do
      FNodeAliasCache[I] := 'N' + IntToStr(I);
  end;
  Result := FNodeAliasCache[FIndexCount];
  Inc(FIndexCount);
end;

function TdxBaseQueryGenerator.GetNodeStack: TStack<TdxJoinNode>;
begin
  if FNodeStack = nil then
    FNodeStack := TStack<TdxJoinNode>.Create;
  Result := FNodeStack;
end;

function TdxBaseQueryGenerator.GetNodeStackHashSet: TdxHashSetString;
begin
  if FNodeStackHashSet = nil then
    FNodeStackHashSet := TdxHashSetString.Create;
  Result := FNodeStackHashSet;
end;

function TdxBaseQueryGenerator.GetParameter(const AParam: IdxCriteriaOperator;
  const AAlias: IdxPropertyAlias; AIndex: Integer): IdxCriteriaOperator;
var
  APropertyAlias: TdxPropertyAlias;
  AParamPropertyAlias: IdxPropertyAlias;
  AOperandValue: IdxOperandValue;
begin
  APropertyAlias := AAlias as TdxPropertyAlias;
  if APropertyAlias.Member = nil then
    Result := AParam
  else
    if Supports(AParam, IdxOperandValue, AOperandValue) then
      Result := GetConstParameter(APropertyAlias.GetMember(AIndex), AOperandValue, APropertyAlias.Member)
    else
      if Supports(AParam, IdxPropertyAlias, AParamPropertyAlias) then
        Result := GetQueryOperandFromAlias(AParamPropertyAlias, APropertyAlias, AIndex)
      else
        Result := AParam;
end;

class function TdxBaseQueryGenerator.GetProjectedMemberColumn(const AMappingField: string; ANode: TdxJoinNode;
  AProjectedNodeList: TdxJoinNodeList; const AParentCriteria: IdxCriteriaOperator): TdxDBColumn;
var
  AFoundNode: TdxJoinNode;
  AProjectionNode: TdxJoinNode absolute ANode;
  AProjection: TdxDBProjection;
  AProjectedNode: TdxJoinNode;
  AColumn: TdxDBColumn;
  ASelect: TdxSelectStatement;
  AMemberColumn: TdxDBColumn;
begin
  AFoundNode := ANode;
  AProjection := AProjectionNode.Table as TdxDBProjection;
  AMemberColumn := AProjection.GetColumn(AMappingField);
  if AMemberColumn = nil then
  begin
    for AProjectedNode in AProjectedNodeList do
    begin
      AColumn := AProjectedNode.GetColumn(AMappingField);
      if AColumn <> nil then
      begin
        AMemberColumn := AColumn;
        AFoundNode := AProjectedNode;
        Break;
      end;
    end;
    if AMemberColumn = nil then
      raise EInvalidOperation.Create(sdxMemberColumn);
    AProjection.AddColumn(AMemberColumn);
    ASelect := AProjection.Projection;
    ASelect.Operands.Add(TdxQueryOperand.Create(AMemberColumn, AFoundNode.Alias, AParentCriteria));
  end;
  Result := AMemberColumn;
end;

function TdxBaseQueryGenerator.GetPropertyNode(APropertyPath: TdxMemberInfoCollection; AType: TdxJoinType): IdxPropertyAlias;
var
  ANode: TdxJoinNode;
  AMember: TdxMappingMemberInfo;
  ALastIndex, I: Integer;
  ACurrentClass: TdxEntityInfo;
begin
  ANode := FRootNode;
  ALastIndex := APropertyPath.Count - 1;
  if (ALastIndex > 0) and APropertyPath[ALastIndex].IsKey then
    Dec(ALastIndex);
  ACurrentClass := FEntityInfo;
  for I := 0 to ALastIndex do
  begin
    AMember := APropertyPath[I];
    if not AMember.IsMappingClass(ACurrentClass) then
      ANode := AppendInheritanceJoinNode(ACurrentClass, AMember, ANode);
    if AMember.IsAssociationList then
    begin
      if I = ALastIndex then
        Exit(TdxPropertyAlias.Create(ANode, AMember))
      else
        Break;
    end;
    if I <> ALastIndex then
    begin
      ANode := AppendJoinNode(AMember, ANode, AType);
      ACurrentClass := AMember.ReferenceType;
    end
    else
      Exit(GetMembers(ANode, AMember));
  end;
  raise Exception.Create('');
end;

function TdxBaseQueryGenerator.GetPropertyNode(const AProperty: IdxOperandProperty; AType: TdxJoinType): IdxPropertyAlias;
var
  AOperand: TdxMemberPathOperand;
begin
  AOperand := Safe<TdxMemberPathOperand>.Cast(AProperty as TdxOperandProperty);
  if AOperand <> nil then
    Result := GetPropertyNode(AOperand.Path, AType)
  else
    Result := ExecuteWithPropertyNameDiving(AProperty.PropertyName,
      function(APropertyName: string): IdxCriteriaOperator
      begin
        Result := GetPropertyNode(EntityInfo.ParsePersistentPath(APropertyName), AType);
      end) as IdxPropertyAlias;
end;

function TdxBaseQueryGenerator.GetSubJoinCriteria(AGena: TdxBaseQueryGenerator): IdxCriteriaOperator;
begin
  Result := CreateBinary(TdxBinaryOperatorType.Equal,
    TdxCriteriaOperator(GetPropertyNode(TdxOperandProperty.Create(EntityInfo.KeyProperty.Member.MemberName), FCurrentJoinType)),
    TdxCriteriaOperator(AGena.Process(TdxOperandProperty.Create(EntityInfo.KeyProperty.Member.MemberName))));
end;

function TdxBaseQueryGenerator.GetQueryOperandFromAlias(const AAliasFromParam, AAlias: IdxPropertyAlias; AIndex: Integer)
  : IdxCriteriaOperator;
var
  ANode: TdxJoinNode;
  AMappingField: string;
  ANodeAlias: string;
  AFoundProjectionNode: TdxJoinNode;
  AMemberColumn: TdxDBColumn;
  AProjectionNodeItem: TdxProjectionNodeItem;
  AMemberOperand: IdxMemberOperand;
begin
//  AMemberOperand := TdxMemberOperand.Create(AAlias.Member);
  AMemberOperand := AAlias;
  AMappingField := (AAlias as TdxPropertyAlias).GetMappingField(AIndex);
  ANode := AAliasFromParam.Node;
  ANodeAlias := ANode.Alias;
  if ((FProjectedSingleNodes <> nil) and FProjectedSingleNodes.TryGetValue(ANodeAlias, AFoundProjectionNode)) and
    not NodeStackHashSet.Contains(ANodeAlias) then
  begin
    ANode := AFoundProjectionNode;
    ANodeAlias := ANode.Alias;
  end;
  if (FProjectionNodes <> nil) and FProjectionNodes.TryGetValue(ANodeAlias, AProjectionNodeItem) then
    AMemberColumn := GetProjectedMemberColumn(AMappingField, ANode, AProjectionNodeItem.ProjectedNodes, AMemberOperand)
  else
    AMemberColumn := ANode.GetColumn(AMappingField);
  Result := TdxQueryOperand.Create(AMemberColumn, ANodeAlias, AMemberOperand);
end;

function TdxBaseQueryGenerator.IsGrouped: Boolean;
begin
  Result := False;
end;

procedure TdxBaseQueryGenerator.InitData;
var
  ABranch: TdxEntityInfo;
begin
  FIndexCount := 0;
  ABranch := FEntityInfo;
  if not ABranch.ClassAttributes.IsPersistent and (ABranch.ClassAttributes.PersistentBaseClass <> nil) then
    ABranch := EntityManager.GetEntityInfo(ABranch.ClassAttributes.PersistentBaseClass);
  FRoot.Free;
  FRoot := CreateRootStatement(ABranch.DBTable, GetNextNodeAlias);
  FRootNode := FRoot;
end;

function TdxBaseQueryGenerator.PatchCriteria(const AOriginalCriteria: IdxCriteriaOperator): IdxCriteriaOperator;
begin
  Result := PatchCriteria(AOriginalCriteria, FEntityInfo);
end;

function TdxBaseQueryGenerator.AppendSingleAggregateJoinNode(AEntityInfo: TdxEntityInfo; ACollectionProperty: TdxMappingMemberInfo;
  const ACondition: IdxCriteriaOperator; APrevNode: TdxJoinNode; AType: TdxJoinType): TdxJoinNode;
var
  ASingleNodeCache: TDictionary<TdxSingleAggregateItem, TdxJoinNode>;
  AItem: TdxSingleAggregateItem;
  ASingleNode: TdxJoinNode;
  AJoinCondition: IdxCriteriaOperator;
begin
  if FSingleNesting > 0 then
    raise EInvalidOperation.Create(sdxGeneratorTheUseOfNestedSingleAggregatesIsProhibited);
  Inc(FSingleNesting);
  try
    if FSingleAggregateSubNodes = nil then
      FSingleAggregateSubNodes := TObjectDictionary<string, TDictionary<TdxSingleAggregateItem, TdxJoinNode>>.Create([doOwnsValues]);
    if not FSingleAggregateSubNodes.TryGetValue(APrevNode.Alias, ASingleNodeCache) then
    begin
      ASingleNodeCache := TDictionary<TdxSingleAggregateItem, TdxJoinNode>.Create(TdxSingleAggregateItemComparer.Create);
      FSingleAggregateSubNodes.Add(APrevNode.Alias, ASingleNodeCache);
    end;
    AItem := TdxSingleAggregateItem.Create(ACondition, AEntityInfo);
    if ASingleNodeCache.TryGetValue(AItem, ASingleNode) then
    begin
      if (AType <> TdxJoinType.Inner) and (ASingleNode.&Type <> AType) then
        SetJoinTypeWithSubNodes(ASingleNode, AType);
    end
    else
    begin
      if AType <> APrevNode.&Type then
        AType := TdxJoinType.LeftOuter;
      ASingleNode := TdxJoinNode.Create(AEntityInfo.DBTable, GetNextNodeAlias, AType);
      ASingleNodeCache.Add(AItem, ASingleNode);
      if FNakedSingleNodes = nil then
        FNakedSingleNodes := TdxNamedJoinNodeDictionary.Create(False);
      FNakedSingleNodes.Add(ASingleNode.Alias, ASingleNode);
      AJoinCondition := nil;
      if ACollectionProperty <> nil then
        AJoinCondition := GetJoinCondition(APrevNode.Alias, ASingleNode.Alias, FEntityInfo.KeyProperty.Member,
          ACollectionProperty.AssociatedMember);
      ASingleNode.Condition := TdxGroupOperator.And(AJoinCondition,
        ProcessLogicalInContext(AEntityInfo, ASingleNode, PatchCriteria(ACondition, AEntityInfo)) as TdxCriteriaOperator);
      APrevNode.SubNodes.Add(ASingleNode);
    end;
    Result := ASingleNode;
  finally
    Dec(FSingleNesting);
  end;
end;

function TdxBaseQueryGenerator.PatchCriteria(const AOriginalCriteria: IdxCriteriaOperator; AEntityInfo: TdxEntityInfo)
  : IdxCriteriaOperator;
begin
  if not FCollectionCriteriaPatcher.HasValue then
    Result := AOriginalCriteria
  else
  begin
    Result := FCollectionCriteriaPatcher.PatchCriteria(AEntityInfo, AOriginalCriteria);
  end;
end;

function TdxBaseQueryGenerator.ProcessTopSubSelect(const AAggregateProperty, AOperandClause: IdxCriteriaOperator;
  AAggregateFunctionType: TdxAggregateFunctionType): TdxQuerySubQueryContainer;
var
  AGena: TdxSubSelectQueryGenerator;
  AQueryStatement: TdxBaseStatement;
  AAggregatedOperand: IdxCriteriaOperator;
begin
  AGena := TdxSubSelectQueryGenerator.Create(Self, BatchWideData, '', ClassInfo, AAggregateProperty,
    AAggregateFunctionType, FCollectionCriteriaPatcher);
  AQueryStatement := AGena.GenerateSelect(AOperandClause);
  AQueryStatement.Condition := TdxGroupOperator.And(GetSubJoinCriteria(AGena), AQueryStatement.Condition);
  if AQueryStatement.Operands.Count > 0 then
  begin
    Assert(AQueryStatement.Operands.Count = 1);
    AAggregatedOperand := AQueryStatement.Operands[0];
  end
  else
    AAggregatedOperand := nil;
  Result := TdxQuerySubQueryContainer.Create(AQueryStatement, AAggregatedOperand, AAggregateFunctionType);
end;

function TdxBaseQueryGenerator.Process(const AOperand: IdxCriteriaOperator): IdxCriteriaOperator;
begin
  if AOperand = nil then
    Result := nil
  else
    Result := (AOperand as TdxCriteriaOperator).Accept(Self);
end;

function TdxBaseQueryGenerator.ProcessLogical(const AOperand: IdxCriteriaOperator): IdxCriteriaOperator;
var
  AProcessedOperand: IdxCriteriaOperator;
  AOperator: IdxCriteriaOperator;
  AAlias: IdxPropertyAlias;
begin
  AProcessedOperand := Process(AOperand);
  if Supports(AProcessedOperand, IdxPropertyAlias, AAlias) then
  begin
    AOperator := GetParameter(AProcessedOperand, AAlias, 0);
    if AAlias.Count > 1 then
      AddMultiColumnAlias(AOperator, AAlias);
  end
  else
    AOperator := AProcessedOperand;
  if Supports(AOperator, IdxOperandProperty) or Supports(AOperator, IdxOperandValue) then
    Result := CreateBinary(TdxBinaryOperatorType.Equal, AProcessedOperand, TdxConstantValue.Create(True))
  else
    Result := AOperator;
end;

function TdxBaseQueryGenerator.ProcessLogicalInContext(AEntityInfo: TdxEntityInfo; ASingleNode: TdxJoinNode;
  const ACriteria: IdxCriteriaOperator): IdxCriteriaOperator;
begin
  ClassInfoStack.Push(FEntityInfo);
  NodeStack.Push(FRootNode);
  NodeStackHashSet.Add(FRootNode.Alias);
  FRootNode := ASingleNode;
  FEntityInfo := AEntityInfo;
  try
    Result := ProcessLogical(ACriteria as TdxCriteriaOperator);
  finally
    FEntityInfo := ClassInfoStack.Pop;
    FRootNode := NodeStack.Pop;
    NodeStackHashSet.Remove(FRootNode.Alias);
  end;
end;

function TdxBaseQueryGenerator.ProcessSingleAggregateOperand(const ACollectionProperty: IdxOperandProperty;
  const ACondition, AAggregatedExpression: IdxCriteriaOperator): IdxCriteriaOperator;
begin
  if ACollectionProperty = nil then
    raise EArgumentNilException.Create('');
  Result := ExecuteWithPropertyNameDiving(ACollectionProperty.PropertyName,
    function (APersistentPath: string): IdxCriteriaOperator
    var
      AMic: TdxMemberInfoCollection;
      AAlias, ACollectionAlias: IdxPropertyAlias;
      I: Integer;
      AMember: TdxMappingMemberInfo;
      ASingleNode: TdxJoinNode;
    begin
      AMic := EntityInfo.ParsePersistentPath(APersistentPath);
      if AMic.Count = 0 then
        raise EInvalidOperation.Create('');
      try
        for I := 0 to AMic.Count - 1 - 1 do
        begin
          AAlias := GetPropertyNode(TdxMemberInfoCollection.Create(FEntityInfo, [AMic[I], AMic[I + 1]]), FCurrentJoinType);
          ClassInfoStack.Push(FEntityInfo);
          NodeStack.Push(FRootNode);
          NodeStackHashSet.Add(FRootNode.Alias);
          FEntityInfo := AMic[I].ReferenceType;
          FRootNode := AAlias.Node;
        end;
        ACollectionAlias := GetPropertyNode(TdxMemberInfoCollection.Create(FEntityInfo, [AMic[AMic.Count - 1]]), FCurrentJoinType);
        AMember := ACollectionAlias.Member;
        if AMember.IsManyToMany then
        begin
          NotImplemented;
          Result := nil;
        end
        else
        begin
          ASingleNode := AppendSingleAggregateJoinNode(AMember.CollectionElementType, AMember, ACondition, FRootNode, FCurrentJoinType);
          Exit(ProcessLogicalInContext(AMember.CollectionElementType, ASingleNode, AAggregatedExpression) as TdxCriteriaOperator);
        end;
      finally
        for I := 0 to AMic.Count - 1 - 1 do
        begin
          FEntityInfo := ClassInfoStack.Pop;
          FRootNode := NodeStack.Pop;
          NodeStackHashSet.Remove(FRootNode.Alias);
        end;
      end;
    end);
end;

function TdxBaseQueryGenerator.ProcessSubSelectOperator(const ACollectionProperty: IdxOperandProperty;
  const AOperandClause, AAggregateProperty: IdxCriteriaOperator; AAggregateFunctionType: TdxAggregateFunctionType): TdxQuerySubQueryContainer;
var
  AAlias: IdxPropertyAlias;
  AMember: TdxMappingMemberInfo;
  AGena: TdxSubSelectQueryGenerator;
  AAssociatedPropertyName: string;
  AQueryStatement: TdxBaseStatement;
  AAggregatedOperand: IdxCriteriaOperator;
  AOperandProperty: TdxOperandProperty;
  ALeftOperator, ARightOperator: IdxCriteriaOperator;
begin
  AAlias := GetPropertyNode(ACollectionProperty, FCurrentJoinType);
  AMember := AAlias.Member;
  if AMember.IsManyToMany then
  begin
    AGena := NotImplemented;
  end
  else
  begin
    AAssociatedPropertyName := AMember.AssociatedMember.MemberName;
    AGena := TdxSubSelectQueryGenerator.Create(Self, BatchWideData, ACollectionProperty.PropertyName,
      AMember.CollectionElementType, AAggregateProperty, AAggregateFunctionType, FCollectionCriteriaPatcher);
  end;
  try
    AQueryStatement := AGena.GenerateSelect(AOperandClause);
    AOperandProperty := TdxOperandProperty.Create(AAssociatedPropertyName);
    try
      ALeftOperator := GetMembers(AAlias.Node, AMember.Owner.KeyProperty.Member);
      ARightOperator := AGena.Process(AOperandProperty);
      AQueryStatement.Condition := TdxGroupOperator.And(CreateBinary(TdxBinaryOperatorType.Equal,
        ALeftOperator, ARightOperator), AQueryStatement.Condition);
    finally
      AOperandProperty.Free;
    end;
  finally
    AGena.Free;
  end;
  AAggregatedOperand := nil;
  if AQueryStatement.Operands.Count > 0 then
  begin
    Assert(AQueryStatement.Operands.Count = 1);
    AAggregatedOperand := AQueryStatement.Operands[0];
  end;
  Result := TdxQuerySubQueryContainer.Create(AQueryStatement, AAggregatedOperand, AAggregateFunctionType);
end;

function TdxBaseQueryGenerator.ProcessSubSelectOperator(const AJoinTypeName: string;
  const AOperandClause, AAggregateProperty: IdxCriteriaOperator; AAggregateFunctionType: TdxAggregateFunctionType): TdxQuerySubQueryContainer;
var
  AJoinEntityInfo: TdxEntityInfo;
  AGena: TdxSubSelectQueryGenerator;
  AQueryStatement: TdxBaseStatement;
  AAggregatedOperand: IdxCriteriaOperator;
begin
  AJoinEntityInfo := nil;
  if not TdxMemberInfoCollection.TryResolveTypeAlsoByShortName(AJoinTypeName, FEntityInfo, AJoinEntityInfo) then
    raise EdxCannotResolveClassInfoException.CreateFmt(sdxMetadataCannotResolveEntityInfo, [AJoinTypeName]);
  AGena := TdxSubSelectQueryGenerator.Create(Self, BatchWideData, '', AJoinEntityInfo, AAggregateProperty,
    AAggregateFunctionType, FCollectionCriteriaPatcher);
  try
    AQueryStatement := AGena.GenerateSelect(AOperandClause);
  finally
    AGena.Free;
  end;
  if AQueryStatement.Operands.Count > 0 then
  begin
    Assert(AQueryStatement.Operands.Count = 1);
    AAggregatedOperand := AQueryStatement.Operands[0];
  end
  else
    AAggregatedOperand := nil;
  Result := TdxQuerySubQueryContainer.Create(AQueryStatement, AAggregatedOperand, AAggregateFunctionType);
end;

function TdxBaseQueryGenerator.ProcessSingleJoinOperand(const AJoinTypeName: string;
  const ACondition, AAggregatedExpression: IdxCriteriaOperator): IdxCriteriaOperator;
var
  AJoinEntityInfo: TdxEntityInfo;
  AJoinNode: TdxJoinNode;
begin
  AJoinEntityInfo := nil;
  if not TdxMemberInfoCollection.TryResolveTypeAlsoByShortName(AJoinTypeName, FEntityInfo, AJoinEntityInfo) then
    raise EdxCannotResolveClassInfoException.CreateFmt(sdxMetadataCannotResolveEntityInfo, [AJoinTypeName]);
  AJoinNode := AppendSingleAggregateJoinNode(AJoinEntityInfo, nil, ACondition, FRootNode, FCurrentJoinType);
  Result := ProcessLogicalInContext(AJoinEntityInfo, AJoinNode, AAggregatedExpression);
end;

class procedure TdxBaseQueryGenerator.SetJoinTypeWithSubNodes(ANode: TdxJoinNode; AType: TdxJoinType);
var
  ASubNode: TdxJoinNode;
begin
  ANode.&Type := AType;
  for ASubNode in ANode.SubNodes do
    SetJoinTypeWithSubNodes(ASubNode, AType);
end;

procedure TdxBaseQueryGenerator.TryAddNodeIntoProjection(APrevNode, ANode: TdxJoinNode);
var
  AProjectionNode: TdxJoinNode;
  AProjectedNodeItem: TdxProjectionNodeItem;
begin
  if (FProjectedSingleNodes <> nil) and FProjectedSingleNodes.TryGetValue(APrevNode.Alias, AProjectionNode) then
  begin
    FProjectedSingleNodes.Add(ANode.Alias, AProjectionNode);
    if (FProjectionNodes <> nil) and FProjectionNodes.TryGetValue(AProjectionNode.Alias, AProjectedNodeItem) then
      AProjectedNodeItem.ProjectedNodes.Add(ANode);
  end;
end;

function TdxBaseQueryGenerator.TryGetMultiColumnAlias(const AOperator: IdxCriteriaOperator; out AAlias: IdxPropertyAlias): Boolean;
begin
  if FMultiColumnAliases = nil then
  begin
    AAlias := nil;
    Result := False;
  end
  else
    Result := FMultiColumnAliases.TryGetValue(AOperator, AAlias);
end;

function TdxBaseQueryGenerator.Visit(const AOperand: IdxOperandValue): IdxCriteriaOperator;
begin
  if (AOperand is TdxOperandValue) or (AOperand is TdxConstantValue) then
    Result := AOperand
  else
    Result := TdxOperandValue.Create(AOperand.Value);
end;

function TdxBaseQueryGenerator.Visit(const AOperator: IdxBetweenOperator): IdxCriteriaOperator;
var
  AGroupOperator: IdxCriteriaOperator;
begin
  AGroupOperator := TdxGroupOperator.And(
    TdxBinaryOperator.Create(AOperator.TestExpression, AOperator.BeginExpression, TdxBinaryOperatorType.GreaterOrEqual),
    TdxBinaryOperator.Create(AOperator.TestExpression, AOperator.EndExpression, TdxBinaryOperatorType.LessOrEqual));
  Result := Process(AGroupOperator as TdxCriteriaOperator);
end;

function TdxBaseQueryGenerator.Visit(const AOperator: IdxBinaryOperator): IdxCriteriaOperator;
var
  APrevLeftJoinEnforcer: TdxGroupOperatorType;
  ALeftOperator, ARightOperator: IdxCriteriaOperator;
begin
  Result := nil;

  APrevLeftJoinEnforcer := FCurrentLeftJoinEnforcer;
  try
    if AOperator.OperatorType = TdxBinaryOperatorType.NotEqual then
      if FCurrentLeftJoinEnforcer = TdxGroupOperatorType.Or then
        FCurrentLeftJoinEnforcer := TdxGroupOperatorType.And
      else
        FCurrentLeftJoinEnforcer := TdxGroupOperatorType.Or;
    if (AOperator.LeftOperand = nil) or (AOperator.RightOperand = nil) then
      raise EArgumentNilException.Create(sdxGeneratorOneOfBinaryOperatorsOperandsIsNull);

    ALeftOperator := Process(AOperator.LeftOperand);
    ARightOperator := Process(AOperator.RightOperand);
    if AOperator.LeftOperand is TdxOperandValue then
    begin
      FSuppressDefaultValueConverters := True;
      try
        ALeftOperator := Process(AOperator.LeftOperand);
      finally
        FSuppressDefaultValueConverters := False;
      end;
    end;
    if AOperator.RightOperand is TdxOperandValue then
    begin
      FSuppressDefaultValueConverters := True;
      try
        ARightOperator := Process(AOperator.RightOperand);
      finally
        FSuppressDefaultValueConverters := False;
      end;
    end;
    Result := CreateBinary(AOperator.OperatorType, ALeftOperator, ARightOperator);
  finally
    FCurrentLeftJoinEnforcer := APrevLeftJoinEnforcer;
  end;
end;

function TdxBaseQueryGenerator.Visit(const AOperator: IdxInOperator): IdxCriteriaOperator;
var
  ALeftOperator: IdxCriteriaOperator;
  AValue: IdxCriteriaOperator;
  APrevJoinType: TdxJoinType;
  ADefaultMembers, ARightMembers: IdxPropertyAlias;
  AValues, AList: TdxCriteriaOperatorList;
begin
  ALeftOperator := Process(AOperator.LeftOperand);
  APrevJoinType := FCurrentJoinType;
  if AOperator.Operands.Count > 1 then
    FCurrentJoinType := TdxJoinType.LeftOuter;
  try
    AValues := TdxCriteriaOperatorList.Create;
    try
      AValues.Capacity := AOperator.Operands.Count;
      ADefaultMembers := CollectInValues(AOperator.Operands, AValues);
      if ALeftOperator is TdxPropertyAlias then
        ADefaultMembers := TdxPropertyAlias(ALeftOperator);
      if ADefaultMembers.Count > 1 then
        Result := BuildInGroup(ADefaultMembers, AValues, ALeftOperator) as TdxCriteriaOperator
      else
      begin
        AList := TdxCriteriaOperatorList.Create;
        try
          AList.Capacity := AValues.Count;
          for AValue in AValues do
          begin
            if not Supports(AValue, IdxPropertyAlias, ARightMembers) then
              ARightMembers := ADefaultMembers;
            AList.Add(GetParameter(AValue, ARightMembers, 0));
          end;
          Result := TdxInOperator.Create(GetParameter(ALeftOperator, ADefaultMembers, 0), AList);
        finally
          AList.Free;
        end;
      end;
    finally
      AValues.Free;
    end;
  finally
    FCurrentJoinType := APrevJoinType;
  end;
end;

function TdxBaseQueryGenerator.Visit(const AOperator: IdxGroupOperator): IdxCriteriaOperator;
var
  APrevJoinType: TdxJoinType;
  ACurrentOperator, AProcessed: IdxCriteriaOperator;
begin
  APrevJoinType := FCurrentJoinType;
  if (AOperator.OperatorType = FCurrentLeftJoinEnforcer) and (AOperator.Operands.Count > 1) then
    FCurrentJoinType := TdxJoinType.LeftOuter;
  try
    Result := nil;
    for ACurrentOperator in AOperator.Operands do
    begin
      AProcessed := ProcessLogical(ACurrentOperator as TdxCriteriaOperator);
      Result := TdxGroupOperator.Combine(AOperator.OperatorType, Result, AProcessed) as TdxCriteriaOperator;
    end;
  finally
    FCurrentJoinType := APrevJoinType;
  end;
end;

function TdxBaseQueryGenerator.Visit(const AOperator: IdxUnaryOperator): IdxCriteriaOperator;
var
  APrevLeftJoinEnforcer: TdxGroupOperatorType;
  APrevJoinType: TdxJoinType;
  AOperandProperty: TdxOperandProperty;
  AProcessedOperand: IdxCriteriaOperator;
  AAlias, ANode: IdxPropertyAlias;
  ACount, I: Integer;
begin
  APrevLeftJoinEnforcer := FCurrentLeftJoinEnforcer;
  APrevJoinType := FCurrentJoinType;
  try
    if AOperator.OperatorType = TdxUnaryOperatorType.Not then
      if (FCurrentLeftJoinEnforcer = TdxGroupOperatorType.Or) then
        FCurrentLeftJoinEnforcer := TdxGroupOperatorType.And
      else
        FCurrentLeftJoinEnforcer := TdxGroupOperatorType.Or;
    if (AOperator.OperatorType = TdxUnaryOperatorType.IsNull) and (FCurrentLeftJoinEnforcer = TdxGroupOperatorType.Or) then
      FCurrentJoinType := TdxJoinType.LeftOuter;
    AOperandProperty := Safe<TdxOperandProperty>.Cast(AOperator.Operand as TdxCriteriaOperator);
    if AOperandProperty = nil then
    begin
      if AOperator.OperatorType = TdxUnaryOperatorType.Not then
        Result := TdxUnaryOperator.Create(TdxUnaryOperatorType.Not, ProcessLogical(AOperator.Operand as TdxCriteriaOperator))
      else
      begin
        AProcessedOperand := Process(AOperator.Operand);
        if not Supports(AProcessedOperand, IdxPropertyAlias, AAlias) then
          AAlias := TdxPropertyAlias.Empty;
        Result := TdxUnaryOperator.Create(AOperator.OperatorType, GetParameter(AProcessedOperand, AAlias, 0));
      end;
    end
    else
    begin
      ANode := GetPropertyNode(AOperandProperty, FCurrentJoinType);
      Result := nil;
      ACount := ANode.Count;
      for I := 0 to ACount - 1 do
        Result := TdxGroupOperator.And(Result,
          TdxUnaryOperator.Create(AOperator.OperatorType, GetQueryOperandFromAlias(ANode, ANode, I))) as TdxCriteriaOperator;
    end;
  finally
    FCurrentLeftJoinEnforcer := APrevLeftJoinEnforcer;
    FCurrentJoinType := APrevJoinType;
  end;
end;

function TdxBaseQueryGenerator.Visit(const AOperator: IdxFunctionOperator): IdxCriteriaOperator;
var
  APrevJoinType: TdxJoinType;
  ANewOperatorType: TdxNullableValue<TdxFunctionOperatorType>;
  AProcessedOperands: TArray<IdxCriteriaOperator>;
  I: Integer;
  AProcessedOperand: IdxCriteriaOperator;
  AAlias: IdxPropertyAlias;
begin
  APrevJoinType := FCurrentJoinType;
  try
    ANewOperatorType.Reset;
    if (AOperator.OperatorType = TdxFunctionOperatorType.Custom) and (AOperator.Operands.Count = 3) then
      NotImplemented
    else
      if AOperator.OperatorType in [TdxFunctionOperatorType.IsNull, TdxFunctionOperatorType.IsNullOrEmpty, TdxFunctionOperatorType.Iif] then
        FCurrentJoinType := TdxJoinType.LeftOuter
      else
        if (AOperator.OperatorType = TdxFunctionOperatorType.AddTimeSpan) and not TdxSQLConnectionProvider.UseLegacyTimeSpanSupport then
          ANewOperatorType := TdxFunctionOperatorType.IncSecond;
    SetLength(AProcessedOperands, AOperator.Operands.Count);
    for I := 0 to AOperator.Operands.Count - 1 do
    begin
      AProcessedOperand := Process(AOperator.Operands[I]);
      if not Supports(AProcessedOperand, IdxPropertyAlias, AAlias) then
        AAlias := TdxPropertyAlias.Empty;
      AProcessedOperands[I] := GetParameter(AProcessedOperand, AAlias, 0);
    end;
    if ANewOperatorType.IsNull then
      ANewOperatorType := AOperator.OperatorType;
    Result := TdxFunctionOperator.Create(ANewOperatorType, AProcessedOperands);
  finally
    FCurrentJoinType := APrevJoinType;
  end;
end;

function TdxBaseQueryGenerator.Visit(const AOperand: IdxOperandProperty): IdxCriteriaOperator;
begin
  Result := GetPropertyNode(AOperand, FCurrentJoinType);
end;

function TdxBaseQueryGenerator.Visit(const AOperator: IdxAggregateOperand): IdxCriteriaOperator;
var
  ARes: IdxCriteriaOperator;
  AProp: IdxCriteriaOperator;
  ANode: IdxPropertyAlias;
begin
  if (AOperator as TdxAggregateOperand).IsTopLevel then
  begin
    if AOperator.AggregateFunctionType = TdxAggregateFunctionType.Exists then
    begin
      if IsGrouped then
        Exit(ProcessTopSubSelect(AOperator.AggregatedExpression, AOperator.Condition, AOperator.AggregateFunctionType));
      Exit(Process(TdxBinaryOperator.Create(
        TdxAggregateOperand.Create(nil, AOperator.AggregatedExpression, TdxAggregateFunctionType.Count, AOperator.Condition),
        TdxOperandValue.Create(0),
        TdxBinaryOperatorType.Greater)));
    end;
    if AOperator.AggregateFunctionType = TdxAggregateFunctionType.Single then
      raise EInvalidOperation.Create(sdxGeneratorTheUseOfATopLevelSingleAggregateIsProhibited);
    if AOperator.Condition <> nil then
      raise ENotSupportedException.Create('');
    ARes := Process(AOperator.AggregatedExpression);
    if Supports(ARes, IdxPropertyAlias, ANode) then
      AProp := GetQueryOperandFromAlias(ANode, ANode, 0)
    else
      AProp := ARes;
    if AOperator.AggregatedExpression = nil then
      AProp := nil;
    Exit(TdxQuerySubQueryContainer.Create(nil, AProp, AOperator.AggregateFunctionType));
  end;
  if AOperator.AggregateFunctionType = TdxAggregateFunctionType.Single then
    Result := ProcessSingleAggregateOperand(AOperator.CollectionProperty, AOperator.Condition, AOperator.AggregatedExpression) as TdxCriteriaOperator
  else
    Result := ProcessSubSelectOperator(AOperator.CollectionProperty, AOperator.Condition, AOperator.AggregatedExpression, AOperator.AggregateFunctionType) as TdxCriteriaOperator;
end;

function TdxBaseQueryGenerator.Visit(const AOperator: IdxJoinOperand): IdxCriteriaOperator;
begin
  if AOperator.AggregateFunctionType = TdxAggregateFunctionType.Single then
    Result := ProcessSingleJoinOperand(AOperator.JoinTypeName, AOperator.Condition, AOperator.AggregatedExpression) as TdxCriteriaOperator
  else
    Result := ProcessSubSelectOperator(AOperator.JoinTypeName, AOperator.Condition, AOperator.AggregatedExpression, AOperator.AggregateFunctionType) as TdxCriteriaOperator;
end;

{ TdxProjectionAliasPatcher }

constructor TdxProjectionAliasPatcher.Create(ARoot: TdxBaseStatement; AProjectedNodes: TdxNamedJoinNodeDictionary; AProjectionNodes: TdxNamedOrdinalDictionary<TdxProjectionNodeItem>);
begin
  inherited Create;
  FRoot := ARoot;
  FProjectedNodes := AProjectedNodes;
  FProjectionNodes := AProjectionNodes;
end;

destructor TdxProjectionAliasPatcher.Destroy;
begin
  FreeAndNil(FQueryOperandCollector);
  FreeAndNil(FUsedProjectionNodes);
  inherited Destroy;
end;

class procedure TdxProjectionAliasPatcher.Patch(ARoot: TdxBaseStatement;
  AProjectedNodes: TdxNamedJoinNodeDictionary; AProjectionNodes: TdxNamedOrdinalDictionary<TdxProjectionNodeItem>);
var
  APatcher: TdxProjectionAliasPatcher;
begin
  APatcher := TdxProjectionAliasPatcher.Create(ARoot, AProjectedNodes, AProjectionNodes);
  try
    APatcher.Patch;
  finally
    APatcher.Free;
  end;
end;

procedure TdxProjectionAliasPatcher.Patch;
begin
  if (FProjectedNodes = nil) or (FProjectedNodes.Count = 0) then
    Exit;
  FQueryOperandCollector := TdxQueryOperandCollector.Create;
  PatchInternal(FRoot);
end;

procedure TdxProjectionAliasPatcher.PatchInternal(AStatement: TdxBaseStatement);
var
  ANodes: TdxJoinNodeList;
  ACriteriaList: TdxCriteriaOperatorList;
  ACriteria, AParentCriteria: IdxCriteriaOperator;
  ANode: TdxJoinNode;
  AOperand: IdxQueryOperand;
  AProjectionNode: TdxJoinNode;
  AProjectionNodeAlias: string;
  AProjectionNodeItem: TdxProjectionNodeItem;
  AProjection: TdxDBProjection;
  I: Integer;
  AParentCriteriaHolder: IdxParentCriteria;
begin
  if AStatement = nil then
    Exit;
  AStatement.CollectJoinNodesAndCriteria(ANodes, ACriteriaList);
  try
    for ACriteria in ACriteriaList do
    begin
      FQueryOperandCollector.Clear;
      FQueryOperandCollector.Process(ACriteria);
      for I := 0 to FQueryOperandCollector.OperandList.Count - 1 do
      begin
        AOperand := FQueryOperandCollector.OperandList[I];
        if Supports(AOperand, IdxParentCriteria, AParentCriteriaHolder) then
          AParentCriteria := AParentCriteriaHolder.ParentCriteria
        else
          AParentCriteria := nil;
        if AOperand.NodeAlias = '' then
          Continue;
        if FProjectedNodes.TryGetValue(AOperand.NodeAlias, AProjectionNode) then
        begin
          AProjectionNodeAlias := AProjectionNode.Alias;
          if (FUsedProjectionNodes <> nil) and FUsedProjectionNodes.Contains(AProjectionNodeAlias) then
            Continue;
          (AOperand as TdxQueryOperand).NodeAlias := AProjectionNodeAlias;
          if (FProjectionNodes <> nil) and FProjectionNodes.TryGetValue(AProjectionNodeAlias, AProjectionNodeItem) then
            TdxBaseQueryGenerator.GetProjectedMemberColumn(AOperand.ColumnName, AProjectionNode,
              AProjectionNodeItem.ProjectedNodes, AParentCriteria);
        end
        else
          if FProjectionNodes.TryGetValue(AOperand.NodeAlias, AProjectionNodeItem) then
            TdxBaseQueryGenerator.GetProjectedMemberColumn(AOperand.ColumnName, AProjectionNodeItem.Node,
              AProjectionNodeItem.ProjectedNodes, AParentCriteria);
      end;
    end;
    for ANode in ANodes do
    begin
      AProjection := Safe<TdxDBProjection>.Cast(ANode.Table);
      if AProjection = nil then
        Continue;
      if FUsedProjectionNodes = nil then
        FUsedProjectionNodes := TdxHashSetString.Create;
      FUsedProjectionNodes.Add(ANode.Alias);
      try
        PatchInternal(AProjection.Projection);
      finally
        FUsedProjectionNodes.Remove(ANode.Alias);
      end;
    end;
  finally
    ANodes.Free;
    ACriteriaList.Free;
  end;
end;

{ TdxClientSelectSQLGenerator }

constructor TdxClientSelectSQLGenerator.Create(AEntityInfo: TdxEntityInfo; ABatchWideData: TdxBatchWideDataHolderForSelect;
  const AProperties: IdxCriteriaOperatorCollection; const ASortByExpressions: IdxSortByExpressions;
  const AGrouping: IdxCriteriaOperatorCollection;
  const AGroupCriteria: IdxCriteriaOperator; const ACollectionCriteriaPatcher: TdxCollectionCriteriaPatcher);
begin
  inherited Create(AEntityInfo, ABatchWideData, ACollectionCriteriaPatcher);
  FSorting := ASortByExpressions;
  FProperties := AProperties;
  FGrouping := AGrouping;
  FGroupCriteria := AGroupCriteria;
end;

function TdxClientSelectSQLGenerator.GetRoot: TdxSelectStatement;
begin
  Result := TdxSelectStatement(inherited Root);
end;

function TdxClientSelectSQLGenerator.CreateRootStatement(ATable: TdxDBTable; const AAlias: string): TdxBaseStatement;
begin
  Result := TdxSelectStatement.Create(ATable, AAlias);
end;

function TdxClientSelectSQLGenerator.GetSubJoinCriteria(AGena: TdxBaseQueryGenerator): IdxCriteriaOperator;
var
  ACriteria: TArray<IdxCriteriaOperator>;
  I: Integer;
begin
  if IsGrouped then
  begin
    SetLength(ACriteria, FGrouping.Count);
    for I := 0 to FGrouping.Count - 1 do
      ACriteria[I] := CreateBinary(TdxBinaryOperatorType.Equal, Root.GroupProperties[I], (FGrouping[I] as TdxCriteriaOperator).Accept(AGena));
    Result := TdxGroupOperator.And(ACriteria);
  end
  else
    Result := inherited GetSubJoinCriteria(AGena);
end;

class function TdxClientSelectSQLGenerator.GenerateSelect(AEntityInfo: TdxEntityInfo; const ACriteria: IdxCriteriaOperator;
  const AProperties: IdxCriteriaOperatorCollection; const ASortByExpressions: IdxSortByExpressions;
  const AGrouping: IdxCriteriaOperatorCollection;
  const AGroupCriteria: IdxCriteriaOperator; const ACollectionCriteriaPatcher: TdxCollectionCriteriaPatcher;
  ATopSelectedRecords: Integer): TdxSelectStatement;
begin
  Result := GenerateSelect(AEntityInfo, ACriteria, AProperties, ASortByExpressions, AGrouping, AGroupCriteria,
    ACollectionCriteriaPatcher, 0, ATopSelectedRecords);
end;

class function TdxClientSelectSQLGenerator.GenerateSelect(AEntityInfo: TdxEntityInfo; const ACriteria: IdxCriteriaOperator;
  const AProperties: IdxCriteriaOperatorCollection; const ASortByExpressions: IdxSortByExpressions;
  const AGrouping: IdxCriteriaOperatorCollection;
  const AGroupCriteria: IdxCriteriaOperator; const ACollectionCriteriaPatcher: TdxCollectionCriteriaPatcher;
  ASkipSelectedRecords, ATopSelectedRecords: Integer): TdxSelectStatement;
var
  ASQLGenerator: TdxClientSelectSQLGenerator;
  ABatchWideData: TdxBatchWideDataHolderForSelect;
begin
  ABatchWideData := TdxBatchWideDataHolderForSelect.Create;
  ASQLGenerator := TdxClientSelectSQLGenerator.Create(AEntityInfo,
    ABatchWideData, AProperties, ASortByExpressions, AGrouping, AGroupCriteria, ACollectionCriteriaPatcher);
  try
    Result := TdxSelectStatement(ASQLGenerator.GenerateSQL(ACriteria));
    Result.SkipSelectedRecords := ASkipSelectedRecords;
    Result.TopSelectedRecords := ATopSelectedRecords;
  finally
    ABatchWideData.Free;
    ASQLGenerator.Free;
  end;
end;

class function TdxClientSelectSQLGenerator.GenerateSelect(AQuery: TdxObjectsQuery;
  const AProperties, AGroupProperties: IdxCriteriaOperatorCollection;
  const AGroupCriteria: IdxCriteriaOperator): TdxSelectStatement;
begin
  Result := GenerateSelect(AQuery.EntityInfo, AQuery.Criteria, AProperties, AQuery.Sorting, AGroupProperties, AGroupCriteria,
    AQuery.CollectionCriteriaPatcher, AQuery.SkipSelectedRecords, AQuery.TopSelectedRecords);
end;

class function TdxClientSelectSQLGenerator.GenerateSelect(AEntityInfo: TdxEntityInfo; const ACriteria: IdxCriteriaOperator;
  AProperties: TdxMemberPathCollection; const ASortByExpressions: IdxSortByExpressions;
  const AGrouping: IdxCriteriaOperatorCollection;
  const AGroupCriteria: IdxCriteriaOperator; const ACollectionCriteriaPatcher: TdxCollectionCriteriaPatcher;
  ATopSelectedRecords: Integer): TdxSelectStatement;
begin
  Result := GenerateSelect(AEntityInfo, ACriteria, AProperties, ASortByExpressions, AGrouping, AGroupCriteria,
    ACollectionCriteriaPatcher, 0, ATopSelectedRecords);
end;

class function TdxClientSelectSQLGenerator.GenerateSelect(AEntityInfo: TdxEntityInfo; const ACriteria: IdxCriteriaOperator;
  AProperties: TdxMemberPathCollection; const ASortByExpressions: IdxSortByExpressions;
  const AGrouping: IdxCriteriaOperatorCollection;
  const AGroupCriteria: IdxCriteriaOperator; const ACollectionCriteriaPatcher: TdxCollectionCriteriaPatcher;
  ASkipSelectedRecords, ATopSelectedRecords: Integer): TdxSelectStatement;
var
  AProps: IdxCriteriaOperatorCollection;
  AMic: TdxMemberInfoCollection;
begin
  AProps := TdxCriteriaOperatorCollection.Create(AProperties.Count);
  for AMic in AProperties do
    AProps.Add(TdxMemberPathOperand.Create(AMic));
  Result := GenerateSelect(AEntityInfo, ACriteria, AProps, ASortByExpressions, AGrouping, AGroupCriteria,
    ACollectionCriteriaPatcher, ASkipSelectedRecords, ATopSelectedRecords);
end;

function TdxClientSelectSQLGenerator.BuildAssociation(const AProperties: IdxCriteriaOperatorCollection): TArray<IdxCriteriaOperator>;
var
  AProps: TdxCriteriaOperatorList;
  AMic: IdxCriteriaOperator;
  ARes: IdxCriteriaOperator;
  ANode: IdxPropertyAlias;
  ACount, I: Integer;
begin
  AProps := TdxCriteriaOperatorList.Create;
  try
    AProps.Capacity := AProperties.Count;
    Assert(FCurrentJoinType = TdxJoinType.Inner);
    FCurrentJoinType := TdxJoinType.LeftOuter;
    try
      for AMic in AProperties do
      begin
        ARes := Process(AMic);
        if Supports(ARes, IdxPropertyAlias, ANode) then
        begin
          ACount := ANode.Count;
          for I := 0 to ACount - 1 do
            AProps.Add(GetQueryOperandFromAlias(ANode, ANode, I));
        end
        else
          AProps.Add(ARes);
      end;
    finally
      FCurrentJoinType := TdxJoinType.Inner;
    end;
    Result := AProps.ToArray;
  finally
    AProps.Free;
  end;
end;

procedure TdxClientSelectSQLGenerator.BuildAssociationByGroupCriteria;
begin
  Root.GroupCondition := ProcessLogical(FGroupCriteria) as TdxCriteriaOperator;
end;

procedure TdxClientSelectSQLGenerator.BuildAssociationByGrouping;
begin
  if (FGrouping = nil) or (FGrouping.Count = 0) then
    Exit;
  Root.GroupProperties.AddRange(BuildAssociation(FGrouping));
end;

procedure TdxClientSelectSQLGenerator.BuildAssociationByProperties(const AProperties: IdxCriteriaOperatorCollection);
begin
  if (AProperties = nil) or (AProperties.Count = 0) then
    Exit;
  Root.Operands.AddRange(BuildAssociation(AProperties));
end;

procedure TdxClientSelectSQLGenerator.BuildAssociationBySorting;
var
  ASortProperty: IdxSortByExpression;
  ARes: IdxCriteriaOperator;
  ANode: IdxPropertyAlias;
  ACount, I: Integer;
  ASortProperties: TdxQuerySortingCollection;
begin
  if FSorting = nil then
    Exit;
  Assert(FCurrentJoinType = TdxJoinType.Inner);
  FCurrentJoinType := TdxJoinType.LeftOuter;
  try
    ASortProperties := Root.SortProperties;
    for ASortProperty in FSorting do
    begin
      ARes := Process(ASortProperty.Expression as IdxCriteriaOperator);
      if Supports(ARes, IdxPropertyAlias, ANode)  then
      begin
        ACount := ANode.Count;
        for I := 0 to ACount - 1 do
          ASortProperties.Add(TdxSortingColumn.Create(GetQueryOperandFromAlias(ANode, ANode, I), ASortProperty.SortDirection));
      end
      else
        ASortProperties.Add(TdxSortingColumn.Create(ARes, ASortProperty.SortDirection));
    end;
  finally
    FCurrentJoinType := TdxJoinType.Inner;
  end;
end;

procedure TdxClientSelectSQLGenerator.InternalGenerateSQL(const ACriteria: IdxCriteriaOperator);
var
  APatchedCriteria: IdxCriteriaOperator;
begin
  BuildAssociationByProperties(FProperties);
  APatchedCriteria := PatchCriteria(ACriteria);
  BuildAssociationTree(APatchedCriteria);
  BuildAssociationByGrouping;
  BuildAssociationByGroupCriteria;
  BuildAssociationBySorting;
end;

function TdxClientSelectSQLGenerator.IsGrouped: Boolean;
begin
  Result := (Root.GroupProperties <> nil) and (Root.GroupProperties.Count > 0);
end;

{ TdxQueryOperandCollector }

constructor TdxQueryOperandCollector.Create;
begin
  inherited Create;
  FOperandList := TList<IdxQueryOperand>.Create;
end;

destructor TdxQueryOperandCollector.Destroy;
begin
  FreeAndNil(FOperandList);
  inherited Destroy;
end;

procedure TdxQueryOperandCollector.Clear;
begin
  FOperandList.Clear;
end;

procedure TdxQueryOperandCollector.Visit(const AOperand: IdxQuerySubQueryContainer);
begin
  Process(AOperand.AggregateProperty);
  ProcessNode(AOperand.Node);
end;

procedure TdxQueryOperandCollector.Visit(const AOperand: IdxQueryOperand);
begin
  FOperandList.Add(AOperand);
end;

procedure TdxQueryOperandCollector.Visit(const AOperator: IdxFunctionOperator);
begin
  ProcessList(AOperator.Operands);
end;

procedure TdxQueryOperandCollector.Visit(const AOperand: IdxOperandValue);
begin
end;

procedure TdxQueryOperandCollector.Visit(const AOperator: IdxGroupOperator);
begin
  ProcessList(AOperator.Operands);
end;

procedure TdxQueryOperandCollector.Visit(const AOperator: IdxInOperator);
begin
  Process(AOperator.LeftOperand);
  ProcessList(AOperator.Operands);
end;

procedure TdxQueryOperandCollector.Visit(const AOperator: IdxUnaryOperator);
begin
  Process(AOperator.Operand as TdxCriteriaOperator);
end;

procedure TdxQueryOperandCollector.Visit(const AOperator: IdxBinaryOperator);
begin
  Process(AOperator.LeftOperand as TdxCriteriaOperator);
  Process(AOperator.RightOperand as TdxCriteriaOperator);
end;

procedure TdxQueryOperandCollector.Visit(const AOperator: IdxBetweenOperator);
begin
  Process(AOperator.TestExpression as TdxCriteriaOperator);
  Process(AOperator.BeginExpression as TdxCriteriaOperator);
  Process(AOperator.EndExpression as TdxCriteriaOperator);
end;

procedure TdxQueryOperandCollector.ProcessNode(ANode: TdxJoinNode);
var
  AInnerNode: TdxJoinNode;
begin
  if ANode = nil then
    Exit;
  Process(ANode.Condition as TdxCriteriaOperator);
  for AInnerNode in ANode.SubNodes do
    ProcessNode(AInnerNode);
end;

procedure TdxQueryOperandCollector.ProcessList(const AOperands: IdxCriteriaOperatorCollection);
var
  AOperand: IdxCriteriaOperator;
begin
  if AOperands = nil then
    Exit;
  for AOperand in AOperands do
    Process(AOperand as TdxCriteriaOperator);
end;

procedure TdxQueryOperandCollector.Process(const AOperand: IdxCriteriaOperator);
begin
  if AOperand = nil then
    Exit;
  if Supports(AOperand, IdxQuerySubQueryContainer) then
    Visit(AOperand as IdxQuerySubQueryContainer)
  else
  case TdxAcceptor.GetCriteriaType(AOperand) of
    TdxAcceptor.TCriteria.TBetweenOperator:
      Visit(AOperand as IdxBetweenOperator);
    TdxAcceptor.TCriteria.TBinaryOperator:
      Visit(AOperand as IdxBinaryOperator);
    TdxAcceptor.TCriteria.TUnaryOperator:
      Visit(AOperand as IdxUnaryOperator);
    TdxAcceptor.TCriteria.TInOperator:
      Visit(AOperand as IdxInOperator);
    TdxAcceptor.TCriteria.TGroupOperator:
      Visit(AOperand as IdxGroupOperator);
    TdxAcceptor.TCriteria.TOperandValue:
      Visit(AOperand as IdxOperandValue);
    TdxAcceptor.TCriteria.TFunctionOperator:
      Visit(AOperand as IdxFunctionOperator);
    else
      raise EArgumentException.Create('');
  end;
end;

{ TdxBaseObjectQueryGenerator }

constructor TdxBaseObjectQueryGenerator.Create(AEntityInfo: TdxEntityInfo;
  ABatchWideData: TdxBatchWideDataHolderForModification; AObject: TObject = nil);
begin
  inherited Create(AEntityInfo, ABatchWideData);
  FTheObject := AObject;
  FProperties := TdxMemberInfoCollection.Create(AEntityInfo);
end;

constructor TdxBaseObjectQueryGenerator.Create(ABatchWideData: TdxBatchWideDataHolderForModification; AObject: TObject);
begin
  Create(EntityManager.GetEntityInfo(AObject.ClassType), ABatchWideData, AObject);
end;

destructor TdxBaseObjectQueryGenerator.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

function TdxBaseObjectQueryGenerator.GetBatchWideData: TdxBatchWideDataHolderForModification;
begin
  Result := TdxBatchWideDataHolderForModification(inherited BatchWideData);
end;

function TdxBaseObjectQueryGenerator.GenerateSQL(ACriteriaSet: TdxObjectGeneratorCriteriaSet;
  const AProperties: TArray<TdxMappingMemberInfo>; AReverse: Boolean = True): TList<TdxModificationStatement>;
var
  I: Integer;
  M: TdxMappingMemberInfo;
  ACriteria: IdxCriteriaOperator;
  AList: TList<TdxEntityInfo>;
begin
  if AProperties = nil then
  begin
    for M in EntityInfo.MemberAttributes do
      FProperties.Add(M);
  end
  else
    FProperties.AddRange(AProperties);
  Result := TList<TdxModificationStatement>.Create;
  AList := GetClasses;
  try
    if AReverse then
      AList.Reverse;
    Result.Capacity := AList.Count;
    for I := 0 to AList.Count - 1 do
    begin
      EntityInfo := AList[I];
      if ACriteriaSet = nil then
        ACriteria := nil
      else
        ACriteria := ACriteriaSet.GetCompleteCriteria(EntityInfo.TableName);
      Result.Add(TdxModificationStatement(inherited GenerateSQL(ACriteria as TdxCriteriaOperator)));
    end;
  finally
    AList.Free;
  end;
end;

function TdxBaseObjectQueryGenerator.GetClasses: TList<TdxEntityInfo>;
var
  AEntityInfo: TdxEntityInfo;
begin
  Result := TList<TdxEntityInfo>.Create;
  AEntityInfo := EntityInfo;
  repeat
    Result.Add(AEntityInfo);
    AEntityInfo := AEntityInfo.ParentEntity;
  until AEntityInfo = nil;
end;

function TdxBaseObjectQueryGenerator.GetMemberParameter(AMember: TdxMappingMemberInfo; AObject: TObject): IdxOperandValue;
var
  AValue: TValue;
  AConverter: TdxValueConverter;
  ASerializedObject: TObject;
begin
  if AObject = nil then
    Exit(TdxOperandValue.Create(nil));
  if AMember.ReferenceType <> nil then
    Exit(GetMemberParameter(AMember.ReferenceType.KeyProperty.Member, AMember.GetValue(AObject).AsObject));
  if AMember.IsKey then
    AValue := AObject
  else
    AValue := AMember.GetValue(AObject);
  if AMember.IsSerialize then
  begin
    ASerializedObject := AValue.AsObject;
    if ASerializedObject <> nil then
      AValue := AMember.Serializer.GetAsValue(ASerializedObject)
    else
      AValue := TValue.Empty;
  end;
  AConverter := AMember.Converter;
  if AConverter <> nil then
    AValue := AConverter.ConvertToStorageType(AValue);
  Result := GetConstParameter(AValue);
end;

function TdxBaseObjectQueryGenerator.ShouldPersist(AMember: TdxMappingMemberInfo): Boolean;
begin
  Result := AMember.IsMappingClass(EntityInfo);
end;

procedure TdxBaseObjectQueryGenerator.AddParameter(const AParameter: IdxOperandValue);
begin
end;

procedure TdxBaseObjectQueryGenerator.BuildFieldList;
var
  I, J, AMemberCount: Integer;
  APropertyMember, AMember: TdxMappingMemberInfo;
  APrefix: string;
  AMembers: TdxMemberInfoList;
  AObject: TObject;
begin
  for I := 0 to FProperties.Count - 1 do
  begin
    APropertyMember := FProperties[I];
    if not ShouldPersist(APropertyMember) then
      Continue;
    AMembers := GetMembers(APropertyMember, APrefix);
    if AMembers = nil then
    begin
      Root.Operands.Add(BatchWideData.CacheQueryOperand(TdxQueryOperand.Create(Root.GetColumn(APropertyMember.MappingField), '',
        TdxMemberOperand.Create(APropertyMember))));
      AddParameter(GetMemberParameter(APropertyMember, FTheObject));
    end
    else
    begin
      if (APropertyMember.ReferenceType = nil) or (AMembers[0] = APropertyMember) then
        AObject := FTheObject
      else
        AObject := APropertyMember.GetValue(FTheObject).AsObject;
      AMemberCount := AMembers.Count;
      for J := 0 to AMemberCount - 1 do
      begin
        AMember := TdxMappingMemberInfo(AMembers[J]);
        Root.Operands.Add(BatchWideData.CacheQueryOperand(TdxQueryOperand.Create(Root.GetColumn(APrefix + AMember.MappingField), '',
          TdxMemberOperand.Create(AMember))));
        AddParameter(GetMemberParameter(AMember, AObject));
      end;
    end;
  end;
end;

class function TdxBaseObjectQueryGenerator.BuildKeyCriteria(AObject: TObject): IdxCriteriaOperator;
var
  AEntityInfo: TdxEntityInfo;
  AMappingMemberInfo: TdxMappingMemberInfo;
  ACondition: IdxCriteriaOperator;
  I: Integer;
begin
  AEntityInfo := EntityManager.GetEntityInfo(AObject.ClassType);
//  Assert(not AEntityInfo.KeyProperty.IsCompositeKey, 'Not implemented');
  if AEntityInfo.KeyProperty.IsCompositeKey then
  begin
    Result := nil;
    for I := 0 to AEntityInfo.KeyProperty.Members.Count - 1 do
    begin
      AMappingMemberInfo := AEntityInfo.KeyProperty.Members[I];
      ACondition := TdxBinaryOperator.Create(TdxOperandProperty.Create(AMappingMemberInfo.MemberName),
        TdxOperandValue.Create(AMappingMemberInfo.GetValue(AObject)), TdxBinaryOperatorType.Equal);
      Result := TdxGroupOperator.And(Result, ACondition);
    end;
  end
  else
    Result := TdxBinaryOperator.Create(TdxOperandProperty.Create(AEntityInfo.KeyProperty.Member.MemberName),
      TdxOperandValue.Create(AObject), TdxBinaryOperatorType.Equal);
end;

procedure TdxBaseObjectQueryGenerator.InitData;
begin
  inherited InitData;
  Root.Alias := '';
end;

{ TdxDeleteQueryGenerator }

function TdxDeleteQueryGenerator.GetRoot: TdxDeleteStatement;
begin
  Result := TdxDeleteStatement(inherited Root);
end;

procedure TdxDeleteQueryGenerator.InitData;
begin
  inherited InitData;
  Root.AffectedRecordCount := 1;
end;

procedure TdxDeleteQueryGenerator.InternalGenerateSQL(const ACriteria: IdxCriteriaOperator);
begin
  BuildAssociationTree(ACriteria);
end;

class function TdxDeleteQueryGenerator.GenerateDelete(AObject: TObject; ALocking: TdxLockingOption;
  ABatchWideData: TdxBatchWideDataHolderForModification): TList<TdxModificationStatement>;
var
  AEntityInfo: TdxEntityInfo;
  ACriteriaSet: TdxObjectGeneratorCriteriaSet;
  AQueryGenerator: TdxDeleteQueryGenerator;
begin
  AEntityInfo := EntityManager.GetEntityInfo(AObject);
  ACriteriaSet := nil;
  if ACriteriaSet = nil then
    ACriteriaSet := TdxObjectGeneratorCriteriaSet.GetCommonCriteriaSet(BuildKeyCriteria(AObject))
  else
    ACriteriaSet.UpdateCommonCriteria(BuildKeyCriteria(AObject));
  try
    AQueryGenerator := TdxDeleteQueryGenerator.Create(AEntityInfo, ABatchWideData, AObject);
    try
      Result := AQueryGenerator.GenerateSQL(ACriteriaSet, nil, False);
    finally
      AQueryGenerator.Free;
    end;
  finally
    ACriteriaSet.Free;
  end;
end;

class procedure TdxDeleteQueryGenerator.GenerateDeletes(AEntityInfo: TdxEntityInfo;
  ACriteriaSet: TdxObjectGeneratorCriteriaSet; ARes: TList<TdxModificationStatement>; ACount: Integer;
  ABatchWideData: TdxBatchWideDataHolderForModification);
var
  AQueryGenerator: TdxDeleteQueryGenerator;
  ADeletes: TList<TdxModificationStatement>;
  AStatement: TdxModificationStatement;
begin
  Assert(ARes <> nil);
  AQueryGenerator := TdxDeleteQueryGenerator.Create(AEntityInfo, ABatchWideData);
  try
    ADeletes := AQueryGenerator.GenerateSQL(ACriteriaSet, nil, False);
    try
      for AStatement in ADeletes do
        AStatement.AffectedRecordCount := ACount;
      ARes.AddRange(ADeletes);
    finally
      ADeletes.Free;
    end;
  finally
    AQueryGenerator.Free;
  end;
end;

class function TdxDeleteQueryGenerator.GenerateDelete(AEntityInfo: TdxEntityInfo; const AKeys: TList<TObject>;
  ALocking: TdxLockingOption; ABatchWideData: TdxBatchWideDataHolderForModification): TList<TdxModificationStatement>;
var
  AStartPos, ACountLeft, AInSize: Integer;
  ADeletes: TList<TdxModificationStatement>;
begin
  Result := TList<TdxModificationStatement>.Create;
  AStartPos := 0;
  while AStartPos < AKeys.Count do
  begin
    ACountLeft := AKeys.Count - AStartPos;
    AInSize := TdxEMFDefault.GetTerminalInSize(ACountLeft);
    ADeletes := GenerateDeleteCore(AEntityInfo, TdxGetRangeHelper.GetRange(AKeys, AStartPos, AInSize), ALocking, ABatchWideData);
    try
      Result.AddRange(ADeletes);
    finally
      ADeletes.Free;
    end;
    Inc(AStartPos, AInSize);
  end;
end;

class function TdxDeleteQueryGenerator.GenerateDeleteCore(AEntityInfo: TdxEntityInfo; const AKeys: TArray<TObject>;
  ALocking: TdxLockingOption; ABatchWideData: TdxBatchWideDataHolderForModification): TList<TdxModificationStatement>;
var
  ADeletes: TList<TdxModificationStatement>;
  ACriteria: IdxCriteriaOperator;
  AStatement: TdxModificationStatement;
  ADeleteQueryGenerator: TdxDeleteQueryGenerator;
  AObjectGeneratorCriteriaSet: TdxObjectGeneratorCriteriaSet;
  AKeyCount: Integer;
begin
  AKeyCount := Length(AKeys);
  if AKeyCount = 1 then
    Result := GenerateDelete(AKeys[0], ALocking, ABatchWideData)
  else
  begin
    Result := TList<TdxModificationStatement>.Create;
        if not AEntityInfo.KeyProperty.IsCompositeKey then
        begin
          ACriteria := TdxInOperator.Create(AEntityInfo.KeyProperty.Member.MemberName, AKeys);
          ADeleteQueryGenerator := TdxDeleteQueryGenerator.Create(AEntityInfo, ABatchWideData);
          try
            AObjectGeneratorCriteriaSet := TdxObjectGeneratorCriteriaSet.GetCommonCriteriaSet(ACriteria);
            try
              ADeletes := ADeleteQueryGenerator.GenerateSQL(AObjectGeneratorCriteriaSet, nil, False);
              try
                for AStatement in ADeletes do
                  TdxDeleteStatement(AStatement).AffectedRecordCount := AKeyCount;
                Result.AddRange(ADeletes);
              finally
                ADeletes.Free;
              end;
            finally
              AObjectGeneratorCriteriaSet.Free;
            end;
          finally
            ADeleteQueryGenerator.Free;
          end;
        end
        else
          NotImplemented;
  end;
end;



class function TdxDeleteQueryGenerator.GenerateDelete(const AObjects: TList<TObject>;
  ALocking: TdxLockingOption; ABatchWideData: TdxBatchWideDataHolderForModification): TList<TdxModificationStatement>;
var
  ASQL, ADeletes: TList<TdxModificationStatement>;
  APool: TDictionary<TdxEntityInfo, TdxRefHolder>;
  ANonReferenced: TArray<TdxEntityInfo>;
  AEntityInfo: TdxEntityInfo;
  AVictim, ARh: TdxRefHolder;
begin
  ASQL := TList<TdxModificationStatement>.Create;
  APool := TdxRefHolder.CreatePool(AObjects);
  try
    while APool.Count > 0 do
    begin
      ANonReferenced := TdxRefHolder.GetNonReferenced(APool);
      if Length(ANonReferenced) > 0 then
      begin
        for AEntityInfo in ANonReferenced do
        begin
          ADeletes := GenerateDelete(AEntityInfo, APool[AEntityInfo].Objects, ALocking, ABatchWideData);
          try
            ASQL.AddRange(ADeletes);
            TdxRefHolder.ProcessDelete(APool, AEntityInfo);
          finally
            ADeletes.Free;
          end;
        end;
      end
      else
      begin
        AVictim := nil;
        for ARh in APool.Values do
        begin
          if (AVictim <> nil) and (ARh.Objects.Count >= AVictim.Objects.Count) then
            Continue;
          if not ARh.IsInLoop then
            Continue;
          AVictim := ARh;
        end;
        Assert(AVictim <> nil);
        ASQL.AddRange(AVictim.DoUpdates(ALocking, ABatchWideData));
        Assert(not AVictim.IsInLoop);
      end;
    end;
    Result := ASQL;
  finally
    APool.Free;
  end;
end;

class function TdxDeleteQueryGenerator.GenerateDelete(AEntityInfo: TdxEntityInfo;
  ACriteriaSet: TdxObjectGeneratorCriteriaSet;
  ABatchWideData: TdxBatchWideDataHolderForModification): TList<TdxModificationStatement>;
var
  AQueryGenerator: TdxDeleteQueryGenerator;
begin
  AQueryGenerator := TdxDeleteQueryGenerator.Create(AEntityInfo, ABatchWideData);
  try
    Result := AQueryGenerator.GenerateSQL(ACriteriaSet, nil, False);
  finally
    AQueryGenerator.Free;
  end;
end;

function TdxDeleteQueryGenerator.CreateRootStatement(ATable: TdxDBTable; const AAlias: string): TdxBaseStatement;
begin
  Result := TdxDeleteStatement.Create(ATable, AAlias);
end;

{ TdxGetRangeHelper }

class function TdxGetRangeHelper.GetRange(ASrc: TList<TObject>; AIndex: Integer; ACount: Integer): TArray<TObject>;
var
  I: Integer;
begin
  SetLength(Result, ACount);
  for I := 0 to ACount - 1 do
    Result[I] := ASrc[I + AIndex];
end;

class function TdxGetRangeHelper.GetRange(const ASrc: array of TObject; AIndex: Integer; ACount: Integer): TArray<TObject>;
var
  I: Integer;
begin
  SetLength(Result, ACount);
  for I := 0 to ACount - 1 do
    Result[I] := ASrc[I + AIndex];
end;

{ TdxRefHolder }

constructor TdxRefHolder.Create(APool: TDictionary<TdxEntityInfo, TdxRefHolder>; AMe: TdxEntityInfo);
begin
  inherited Create;
  FObjects := TList<TObject>.Create;
  FCanLoop := True;
  FPool := APool;
  FEntityInfo := AMe;
end;

destructor TdxRefHolder.Destroy;
begin
  FreeAndNil(FObjects);
  FreeAndNil(FDirectlyAssignableNotDeleted);
  inherited Destroy;
end;

class function TdxRefHolder.GetNonReferenced(APool: TDictionary<TdxEntityInfo, TdxRefHolder>): TArray<TdxEntityInfo>;
var
  ABuff: TDictionary<TdxEntityInfo, TdxEntityInfo>;
  AEntityInfo: TdxEntityInfo;
  ARh: TdxRefHolder;
begin
  ABuff := TDictionary<TdxEntityInfo, TdxEntityInfo>.Create(APool.Count);
  try
    for AEntityInfo in APool.Keys do
      ABuff.Add(AEntityInfo, AEntityInfo);
    for ARh in APool.Values do
      for AEntityInfo in ARh.DirectlyAssignableNotDeleted.Keys do
        ABuff.Remove(AEntityInfo);
    Result := ABuff.Keys.ToArray;
  finally
    ABuff.Free;
  end;
end;

function TdxRefHolder.CanReach(ATarget: TdxEntityInfo): Boolean;
var
  AProcessed: TDictionary<TdxEntityInfo, TdxEntityInfo>;
begin
  AProcessed := TDictionary<TdxEntityInfo, TdxEntityInfo>.Create(FPool.Count);
  try
    Result := CanReach(AProcessed, ATarget);
  finally
    AProcessed.Free;
  end;
end;

function TdxRefHolder.CanReach(AProcessed: TDictionary<TdxEntityInfo, TdxEntityInfo>; ATarget: TdxEntityInfo): Boolean;
var
  ARh: TdxRefHolder;
begin
  if AProcessed.ContainsKey(FEntityInfo) then
    Exit(False);
  AProcessed.Add(FEntityInfo, FEntityInfo);
  for ARh in FDirectlyAssignableNotDeleted.Values do
  begin
    if ARh.FEntityInfo = ATarget then
      Exit(True);
    if ARh.CanReach(AProcessed, ATarget) then
      Exit(True);
  end;
  Result := False;
end;

function TdxRefHolder.IsInLoop: Boolean;
begin
  if not FCanLoop then
    Exit(False);
  FCanLoop := CanReach(FEntityInfo);
  Result := FCanLoop;
end;

procedure TdxRefHolder.FillDirectlyRefs;
var
  AMemberInfo: TdxMappingMemberInfo;
  ARefHolder: TdxRefHolder;
begin
  FDirectlyAssignableNotDeleted := TDictionary<TdxEntityInfo, TdxRefHolder>.Create;
  for AMemberInfo in FEntityInfo.ObjectProperties do
  begin
    for ARefHolder in FPool.Values do
      if ARefHolder.FEntityInfo.IsAssignableTo(AMemberInfo.ReferenceType) then
        DirectlyAssignableNotDeleted.AddOrSetValue(ARefHolder.FEntityInfo, ARefHolder);
  end;
end;

class function TdxRefHolder.CreatePool(AObjects: TList<TObject>): TDictionary<TdxEntityInfo, TdxRefHolder>;
var
  AObject: TObject;
  AEntityInfo: TdxEntityInfo;
  AHolder, ARh: TdxRefHolder;
begin
  Result := TObjectDictionary<TdxEntityInfo, TdxRefHolder>.Create([doOwnsValues]);
  for AObject in AObjects do
  begin
    AEntityInfo := EntityManager.GetEntityInfo(AObject);
    if not Result.TryGetValue(AEntityInfo, AHolder) then
    begin
      AHolder := TdxRefHolder.Create(Result, AEntityInfo);
      Result.Add(AEntityInfo, AHolder);
    end;
    AHolder.FObjects.Add(AObject);
  end;
  for ARh in Result.Values do
    ARh.FillDirectlyRefs;
end;

class procedure TdxRefHolder.ProcessDelete(APool: TDictionary<TdxEntityInfo, TdxRefHolder>; AEntityInfo: TdxEntityInfo);
var
  ARh: TdxRefHolder;
begin
  APool.Remove(AEntityInfo);
  for ARh in APool.Values do
    ARh.FDirectlyAssignableNotDeleted.Remove(AEntityInfo);
end;

class function TdxRefHolder.GenerateUpdatesBeforeDelete(AEntityInfo: TdxEntityInfo; AUpdateList: TdxMemberInfoCollection;
  const AKeys: TArray<TObject>; ABatchWideData: TdxBatchWideDataHolderForModification; ALocking: TdxLockingOption): TArray<TdxModificationStatement>;
var
  AResult: TList<TdxModificationStatement>;
  AUpdates: TList<TdxModificationStatement>;
  AInSize: Integer;
  AKind: TdxOptimisticLockingBehavior;
  ACriteria: IdxCriteriaOperator;
  AStatement: TdxModificationStatement;
  ACriteriaSet: TdxObjectGeneratorCriteriaSet;
begin
  if (Length(AKeys) = 0) or (AUpdateList.Count = 0) then
    Exit(nil);
  AResult := TList<TdxModificationStatement>.Create;
  try
    AInSize := TdxEMFDefault.GetTerminalInSize(Length(AKeys), AEntityInfo.KeyProperty.KeyFieldCount);
    if Length(AKeys) <> AInSize then
    begin
      AResult.AddRange(GenerateUpdatesBeforeDelete(AEntityInfo, AUpdateList,
        TdxGetRangeHelper.GetRange(AKeys, 0, AInSize), ABatchWideData, ALocking));
      AResult.AddRange(GenerateUpdatesBeforeDelete(AEntityInfo, AUpdateList,
        TdxGetRangeHelper.GetRange(AKeys, AInSize, Length(AKeys) - AInSize), ABatchWideData, ALocking));
    end
    else
    begin
      AKind := AEntityInfo.OptimisticLockingBehavior;
      if (ALocking = TdxLockingOption.None) or
        (AKind in [TdxOptimisticLockingBehavior.ConsiderOptimisticLockingField, TdxOptimisticLockingBehavior.NoLocking]) then
      begin
        ACriteria := TdxInOperator.Create(AEntityInfo.KeyProperty.Member.MemberName, AKeys);
        ACriteriaSet := TdxObjectGeneratorCriteriaSet.GetCommonCriteriaSet(ACriteria);
        try
          AUpdates := TdxUpdateQueryGenerator.GenerateUpdate(AEntityInfo, AUpdateList.ToArray,
            ACriteriaSet, ABatchWideData);
          try
            for AStatement in AUpdates do
              AStatement.AffectedRecordCount := Length(AKeys);
            AResult.AddRange(AUpdates);
          finally
            AUpdates.Free;
          end;
        finally
          ACriteriaSet.Free;
        end;
      end
    end;
    Result := AResult.ToArray;
  finally
    AResult.Free;
  end;
end;

function TdxRefHolder.DoUpdates(ALocking: TdxLockingOption;
  ABatchWideData: TdxBatchWideDataHolderForModification): TArray<TdxModificationStatement>;
var
  ARefLoopClasses: TList<TdxEntityInfo>;
  ARh: TdxRefHolder;
  AUpdateList: TdxMemberInfoCollection;
  AMemberInfo: TdxMappingMemberInfo;
  AEntityInfo: TdxEntityInfo;
begin
  ARefLoopClasses := TList<TdxEntityInfo>.Create;
  try
    ARefLoopClasses.Capacity := FDirectlyAssignableNotDeleted.Count;
    for ARh in FDirectlyAssignableNotDeleted.Values do
      if ARh.CanReach(FEntityInfo) then
        ARefLoopClasses.Add(ARh.FEntityInfo);
    AUpdateList := TdxMemberInfoCollection.Create(FEntityInfo);
    try
      for AMemberInfo in FEntityInfo.ObjectProperties do
      begin
        if TdxAttribute.NoForeignKey in AMemberInfo.Attributes then
          Continue;
        for AEntityInfo in ARefLoopClasses do
        begin
          if AEntityInfo.IsAssignableTo(AMemberInfo.ReferenceType) then
          begin
            AUpdateList.Add(AMemberInfo);
            Break;
          end;
        end;
      end;
      for AEntityInfo in ARefLoopClasses do
        FDirectlyAssignableNotDeleted.Remove(AEntityInfo);
      Assert(Objects.Count > 0);
      Assert(AUpdateList.Count > 0);
      Result := GenerateUpdatesBeforeDelete(FEntityInfo, AUpdateList, Objects.ToArray, ABatchWideData, ALocking);
    finally
      AUpdateList.Free;
    end;
  finally
    ARefLoopClasses.Free;
  end;
end;

{ TdxSingleAggregateItemComparer }

function TdxSingleAggregateItemComparer.Equals(const ALeftItem, ARightItem: TdxSingleAggregateItem): Boolean;
begin
  Result := (ALeftItem.EntityInfo = ARightItem.EntityInfo) and
    TdxCriteriaOperator.CriterionEquals(ALeftItem.Condition as TdxCriteriaOperator, ARightItem.Condition as TdxCriteriaOperator);
end;

function TdxSingleAggregateItemComparer.GetHashCode(const AValue: TdxSingleAggregateItem): Integer;
var
  AHash: Integer;
begin
  if AValue.EntityInfo = nil then
    Result := $1984652
  else
    Result := AValue.EntityInfo.GetHashCode;
  if AValue.Condition = nil then
    AHash := $54382732
  else
    AHash := (AValue.Condition as TdxCriteriaOperator).GetHashCode;
  Result := Result xor AHash;
end;

{ TdxUpdateQueryGenerator }

function TdxUpdateQueryGenerator.GetRoot: TdxUpdateStatement;
begin
  Result := TdxUpdateStatement(inherited Root);
end;

procedure TdxUpdateQueryGenerator.InitData;
begin
  inherited InitData;
  Root.AffectedRecordCount := 1;
end;

procedure TdxUpdateQueryGenerator.AddParameter(const AParameter: IdxOperandValue);
begin
  TdxUpdateStatement(Root).Parameters.Add(AParameter);
end;

procedure TdxUpdateQueryGenerator.InternalGenerateSQL(const ACriteria: IdxCriteriaOperator);
begin
  Properties.Remove(EntityInfo.KeyProperty.Members);
  BuildFieldList;
  BuildAssociationTree(ACriteria);
end;

class function TdxUpdateQueryGenerator.GenerateUpdate(ABatchWideData: TdxBatchWideDataHolderForModification;
  AObject: TObject; const AProperties: TArray<TdxMappingMemberInfo>;
  ACriteriaSet: TdxObjectGeneratorCriteriaSet): TList<TdxModificationStatement>;
var
  AQueryGenerator: TdxUpdateQueryGenerator;
  AKeyCriteria: IdxCriteriaOperator;
begin
  Assert(ACriteriaSet <> nil);
  AKeyCriteria := BuildKeyCriteria(AObject);
  ACriteriaSet.UpdateCommonCriteria(AKeyCriteria);
  AQueryGenerator := TdxUpdateQueryGenerator.Create(ABatchWideData, AObject);
  try
    Result := AQueryGenerator.GenerateSQL(ACriteriaSet, AProperties);
  finally
    AQueryGenerator.Free;
  end;
end;

class function TdxUpdateQueryGenerator.GenerateUpdate(AObject: TObject;
  const AProperties: TArray<TdxMappingMemberInfo>; ACriteriaSet: TdxObjectGeneratorCriteriaSet): TList<TdxModificationStatement>;
var
  ABatchWideDataHolder: TdxBatchWideDataHolderForModification;
begin
  ABatchWideDataHolder := TdxBatchWideDataHolderForModification.Create;
  try
    Result := GenerateUpdate(ABatchWideDataHolder, AObject, AProperties, ACriteriaSet);
  finally
    ABatchWideDataHolder.Free;
  end;
end;

class function TdxUpdateQueryGenerator.GenerateUpdate(AEntityInfo: TdxEntityInfo;
 const AProperties: TArray<TdxMappingMemberInfo>; ACriteriaSet: TdxObjectGeneratorCriteriaSet; ABatchWideData: TdxBatchWideDataHolderForModification): TList<TdxModificationStatement>;
var
  AQueryGenerator: TdxUpdateQueryGenerator;
begin
  AQueryGenerator := TdxUpdateQueryGenerator.Create(AEntityInfo, ABatchWideData);
  try
    Result := AQueryGenerator.GenerateSQL(ACriteriaSet, AProperties);
  finally
    AQueryGenerator.Free;
  end;
end;

class function TdxUpdateQueryGenerator.GenerateUpdate(ABatchWideData: TdxBatchWideDataHolderForModification; AObject: TObject;
  const AProperties: TArray<TdxMappingMemberInfo>): TList<TdxModificationStatement>;
var
  ACriteriaSet: TdxObjectGeneratorCriteriaSet;
begin
  ACriteriaSet := TdxObjectGeneratorCriteriaSet.Create;
  try
    Result := GenerateUpdate(ABatchWideData, AObject, AProperties, ACriteriaSet);
  finally
    ACriteriaSet.Free;
  end;
end;

function TdxUpdateQueryGenerator.CreateRootStatement(ATable: TdxDBTable; const AAlias: string): TdxBaseStatement;
begin
  Result := TdxUpdateStatement.Create(ATable, AAlias);
end;

{ TdxQueryOperandHelper }

class function TdxQueryOperandHelper.Create(AColumn: TdxDBColumn; const ANodeAlias: string;
  const AParentCriteria: IdxCriteriaOperator): IdxQueryOperand;
begin
  Result := TdxQueryOperand.Create(AColumn.Name, ANodeAlias, AColumn.ColumnType, AParentCriteria);
end;

{ TdxInsertQueryGenerator }

function TdxInsertQueryGenerator.GetRoot: TdxInsertStatement;
begin
  Result := TdxInsertStatement(inherited Root);
end;

procedure TdxInsertQueryGenerator.InitData;
var
  AKey: TdxKeyMemberInfo;
begin
  inherited InitData;
  AKey := EntityInfo.KeyProperty;
  if AKey.IsIdentity and not EntityInfo.ClassAttributes.IsOwnTable then
  begin
    Root.IdentityParameter := BatchWideData.CreateIdentityParameter(TheObject);
    Root.IdentityColumn := AKey.Member.MappingField;
    if AKey.Member.TypeKind = tkInt64 then
      Root.IdentityColumnType := TdxDBColumnType.Int64
    else
      Root.IdentityColumnType := TdxDBColumnType.Int32;
    FAutoIncrement := True;
  end
  else
    FAutoIncrement := False;
end;

procedure TdxInsertQueryGenerator.AddParameter(const AParameter: IdxOperandValue);
begin
  Root.Parameters.Add(AParameter);
end;

function TdxInsertQueryGenerator.ShouldPersist(AMember: TdxMappingMemberInfo): Boolean;
begin
  if FAutoIncrement and AMember.IsKey then
    Exit(False);
  Result := inherited;
end;

procedure TdxInsertQueryGenerator.InternalGenerateSQL(const ACriteria: IdxCriteriaOperator);
begin
  BuildFieldList;
end;

class function TdxInsertQueryGenerator.GenerateInsert(AObject: TObject;
  const AProperties: TArray<TdxMappingMemberInfo>): TList<TdxModificationStatement>;
var
  ABatchWideDataHolder: TdxBatchWideDataHolderForModification;
begin
  ABatchWideDataHolder := TdxBatchWideDataHolderForModification.Create;
  try
    Result := GenerateInsert(ABatchWideDataHolder, AObject, AProperties);
  finally
    ABatchWideDataHolder.Free;
  end;
end;

class function TdxInsertQueryGenerator.GenerateInsert(ABatchWideData: TdxBatchWideDataHolderForModification;
  AObject: TObject; const AProperties: TArray<TdxMappingMemberInfo>): TList<TdxModificationStatement>;
var
  AQueryGenerator: TdxInsertQueryGenerator;
begin
  AQueryGenerator := TdxInsertQueryGenerator.Create(ABatchWideData, AObject);
  try
    Result := AQueryGenerator.GenerateSQL(nil, AProperties);
  finally
    AQueryGenerator.Free;
  end;
end;

function TdxInsertQueryGenerator.CreateRootStatement(ATable: TdxDBTable; const AAlias: string): TdxBaseStatement;
begin
  Result := TdxInsertStatement.Create(ATable, AAlias);
end;

end.
