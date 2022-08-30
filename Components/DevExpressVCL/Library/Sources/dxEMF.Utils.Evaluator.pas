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

unit dxEMF.Utils.Evaluator;

{$I cxVer.inc}
{$I dxEMF.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, RTTI,
  dxCoreClasses,
  dxEMF.Utils,
  dxEMF.Metadata,
  dxEMF.Types,
  dxEMF.DB.Criteria,
  dxEMF.DB.Query,
  dxEMF.DB.Generator;

type
  TdxEvaluatorContextDescriptor = class;
  TdxEvaluateCustomFunctionHandler = reference to function(const AFunctionName: string; const AOperands: array of TValue): TValue;

  { TdxClientCriteriaVisitorBase }

  TdxClientCriteriaVisitorBase = class(TcxIUnknownObject, IdxCriteriaOperatorVisitor) //IdxClientCriteriaVisitor<TdxCriteriaOperator>)
  protected
    function DoVisit(const AOperand: IdxAggregateOperand; AProcessCollectionProperty: Boolean): IdxCriteriaOperator; overload; virtual;
    function DoVisit(const AOperand: IdxOperandProperty): IdxCriteriaOperator; overload; virtual;
    function DoVisit(const AOperand: IdxJoinOperand): IdxCriteriaOperator; overload; virtual;

    function Process(const AInput: IdxCriteriaOperator): IdxCriteriaOperator; overload;
    function ProcessCollection(const AOperands: IdxCriteriaOperatorCollection; out AModified: Boolean): IdxCriteriaOperatorCollection;
    function Visit(const AOperand: IdxJoinOperand): IdxCriteriaOperator; overload;
    function Visit(const AOperand: IdxOperandProperty): IdxCriteriaOperator; overload;
    function Visit(const AOperand: IdxAggregateOperand): IdxCriteriaOperator; overload;
    function Visit(const AOperator: IdxFunctionOperator): IdxCriteriaOperator; overload;
    function Visit(const AOperand: IdxOperandValue): IdxCriteriaOperator; overload;
    function Visit(const AOperator: IdxGroupOperator): IdxCriteriaOperator; overload;
    function Visit(const AOperator: IdxInOperator): IdxCriteriaOperator; overload;
    function Visit(const AOperator: IdxUnaryOperator): IdxCriteriaOperator; overload;
    function Visit(const AOperator: IdxBinaryOperator): IdxCriteriaOperator; overload;
    function Visit(const AOperator: IdxBetweenOperator): IdxCriteriaOperator; overload;
  end;

  { TdxNodeCriteriaFinder }

  TdxNodeCriteriaFinder = class(TcxIUnknownObject, IdxCriteriaVisitorListString, IQueryCriteriaToStringListVisitor)
  strict private
    FInAtomicGroupLevel: Integer;
    FCriteriaDictionary: TDictionary<string, TdxPlanAliasCriteriaInfo>;
    FCurrentNodeAlias: string;
  public
    class function FindCriteria(const ACurrentNodeAlias: string;
      const ACriteria: IdxCriteriaOperator): TDictionary<string, TdxPlanAliasCriteriaInfo>; overload; static;
    class procedure FindCriteria(const ACurrentNodeAlias: string;
      const ACriteria: IdxCriteriaOperator; APreviousDictionary: TDictionary<string, TdxPlanAliasCriteriaInfo>); overload; static;
    function Find(const ACurrentNodeAlias: string;
      const ACriteria: IdxCriteriaOperator): TDictionary<string, TdxPlanAliasCriteriaInfo>; overload;
    procedure Find(const ACurrentNodeAlias: string; const ACriteria: IdxCriteriaOperator;
      APreviousDictionary: TDictionary<string, TdxPlanAliasCriteriaInfo>); overload;
    class function GetNodesString(AList: TList<string>): string; static;
    function Process(const ACriteria: IdxCriteriaOperator): TList<string>;
    function ProcessAddOperands(const AOperands: TArray<IdxCriteriaOperator>): TList<string>;
    function ProcessOperands(const AOperands: IdxCriteriaOperatorCollection): TList<string>;
    function Visit(const AOperand: IdxQuerySubQueryContainer): TList<string>; overload;
    function Visit(const AOperand: IdxQueryOperand): TList<string>; overload;
    function Visit(const AOperator: IdxFunctionOperator): TList<string>; overload;
    function Visit(const AOperand: IdxOperandValue): TList<string>; overload;
    function Visit(const AOperator: IdxGroupOperator): TList<string>; overload;
    function Visit(const AOperator: IdxInOperator): TList<string>; overload;
    function Visit(const AOperator: IdxUnaryOperator): TList<string>; overload;
    function Visit(const AOperator: IdxBinaryOperator): TList<string>; overload;
    function Visit(const AOperator: IdxBetweenOperator): TList<string>; overload;
  end;

  { TdxIsLogicalCriteriaChecker }

  TdxIsLogicalCriteriaChecker = class(TcxIUnknownObject, IdxClientCriteriaVisitor<TdxBooleanCriteriaState>)
  strict private
    class var
      FInstance: TdxIsLogicalCriteriaChecker;
    class destructor Finalize;
    class function GetInstance: TdxIsLogicalCriteriaChecker; static;
  public
    class function GetBooleanState(const AOperand: IdxCriteriaOperator): TdxBooleanCriteriaState; static;
    function Visit(const AOperand: IdxAggregateOperand): TdxBooleanCriteriaState; overload;
    function Visit(const AOperand: IdxJoinOperand): TdxBooleanCriteriaState; overload;
    function Visit(const AOperand: IdxOperandProperty): TdxBooleanCriteriaState; overload;
    function Visit(const AOperator: IdxBetweenOperator): TdxBooleanCriteriaState; overload;
    function Visit(const AOperator: IdxBinaryOperator): TdxBooleanCriteriaState; overload;
    function Visit(const AOperator: IdxUnaryOperator): TdxBooleanCriteriaState; overload;
    function Visit(const AOperator: IdxInOperator): TdxBooleanCriteriaState; overload;
    function Visit(const AOperator: IdxGroupOperator): TdxBooleanCriteriaState; overload;
    function Visit(const AOperand: IdxOperandValue): TdxBooleanCriteriaState; overload;
    function Visit(const AOperator: IdxFunctionOperator): TdxBooleanCriteriaState; overload;
    function Process(AOperand: TdxCriteriaOperator): TdxBooleanCriteriaState; overload;

    class property Instance: TdxIsLogicalCriteriaChecker read GetInstance;
  end;

  { TdxEvaluatorProperty }

  TdxEvaluatorProperty = class
  strict private
    FSubProperty: TdxEvaluatorProperty;
    FUpDepth: Integer;
    FPropertyPath: string;
    FTokenized: TArray<string>;
    function GetSubProperty: TdxEvaluatorProperty;
    function GetPropertyPathTokenized: TArray<string>;
  public
    constructor Create(const ASourcePath: string); overload;
    class function Create(const AProperty: idxOperandProperty): TdxEvaluatorProperty; overload; static;
    class function CalcCollectionPropertyDepth(const AProp: string): Integer; static;
    class function GetPropertySeparatorDotPos(const AProperty: string): Integer; overload; static;
    class function GetPropertySeparatorDotPos(const AProperty: string; AStartPos: Integer): Integer; overload; static;
    class function GetIsThisProperty(const APropertyName: string): Boolean; static;
    class function Split(const AProp: string): TArray<string>; static;

    property UpDepth: Integer read FUpDepth;
    property PropertyPath: string read FPropertyPath;
    property Tokenized: TArray<string> read FTokenized;
    property SubProperty: TdxEvaluatorProperty read GetSubProperty;
    property PropertyPathTokenized: TArray<string> read GetPropertyPathTokenized;
  end;

  { IdxEvaluatorDataAccess }

  IdxEvaluatorDataAccess = interface
  end;

  IdxComparer = interface

  end;

  { TdxEvaluatorContext }

  TdxEvaluatorContext = class
  strict private
    FDescriptor: TdxEvaluatorContextDescriptor;
    FSource: TObject;
  public
    constructor Create(ADescriptor: TdxEvaluatorContextDescriptor; ASource: TObject);
    function GetPropertyValue(APropertyPath: TdxEvaluatorProperty): TObject;
    function GetNestedContext(const APropertyPath: string): TdxEvaluatorContext;
    function GetCollectionContexts(const ACollectionName: string): IEnumerable;
    function GetQueryContexts(const AQueryTypeName: string;
      const ACondition: IdxCriteriaOperator; ATop: Integer): TEnumerable<TdxEvaluatorContext>;
    property Descriptor: TdxEvaluatorContextDescriptor read FDescriptor;
    property Source: TObject read FSource;
  end;

  { TdxEvaluatorContextDescriptor }

  TdxEvaluatorContextDescriptor = class abstract
  protected
    function GetIsTopLevelCollectionSource: Boolean; virtual;
  public
    function GetPropertyValue(ASource: TObject; APropertyPath: TdxEvaluatorProperty): TObject; virtual; abstract;
    function GetNestedContext(ASource: TObject; const APropertyPath: string): TdxEvaluatorContext; virtual; abstract;
    function GetCollectionContexts(ASource: TObject; const ACollectionName: string): IEnumerable; virtual; abstract;
    function GetQueryContexts(ASource: TObject; const AQueryTypeName: string;
      const ACondition: IdxCriteriaOperator; ATop: Integer): TEnumerable<TdxEvaluatorContext>; virtual;

    property IsTopLevelCollectionSource: Boolean read GetIsTopLevelCollectionSource;
  end;

  { TdxFnFuncKey }

  TdxFnFuncKey = record
  strict private
    FCaseSensitive: Boolean;
    FFnType: TdxFunctionOperatorType;
    FArgumentCount: Integer;
  public
    constructor Create(AFnType: TdxFunctionOperatorType; ACaseSensitive: Boolean; AArgumentCount: Integer);
    function GetHashCode: Integer;
    function Equals(const AOther: TdxFnFuncKey): Boolean;
  end;

  { TdxJoinContextPropertyInfo }

  TdxJoinContextPropertyInfo = class
  strict private
    FProperty: TdxEvaluatorProperty;
    FPropertyNameInCriteria: string;
  public
    constructor Create(AProperty: TdxEvaluatorProperty; const APropertyNameInCriteria: string);
    function GetHashCode: Integer; override;
    function Equals(AObj: TObject): Boolean; override;
    function ToString: string; override;
    property &Property: TdxEvaluatorProperty read FProperty;
    property PropertyNameInCriteria: string read FPropertyNameInCriteria;
  end;

  { TdxJoinContextValueInfoSet }

  TdxJoinContextValueInfoSet = class
  strict private
    FProperties: TDictionary<string, TValue>;
  public
    constructor Create(AProperties: TDictionary<string, TValue>);

    property Properties: TDictionary<string, TValue> read FProperties;
  end;

  { TdxJoinContextPropertyInfoSet }

  TdxJoinContextPropertyInfoSet = class
  strict private
    FProperties: TDictionary<TdxJoinContextPropertyInfo, Boolean>;
    function GetCount: Integer;
  public
    constructor Create(AProperties: TDictionary<TdxJoinContextPropertyInfo, Boolean>);
    function GetJoinContextValueInfoSet(AContext: TdxEvaluatorContext): TdxJoinContextValueInfoSet;

    property Count: Integer read GetCount;
  end;

  { TdxJoinEvaluationContextCache }

  TdxJoinEvaluationContextCache = class
  strict private type
{$REGION 'Evaluation Cache types'}
      { TdxJoinEvaluationCacheChunk }

      TJoinEvaluationCacheChunk = class
      strict private
        FIsEmpty: Boolean;
        FObjects: TEnumerable<TdxEvaluatorContext>;
        FCriteria: IdxCriteriaOperator;
      public
        constructor Create(const ACriteria: IdxCriteriaOperator);
        procedure Fill(AContext: TdxEvaluatorContext; const AQueryTypeName: string);

        property IsEmpty: Boolean read FIsEmpty;
        property Objects: TEnumerable<TdxEvaluatorContext> read FObjects;
        property Criteria: IdxCriteriaOperator read FCriteria;
      end;

      { TJoinEvaluationCacheInfo }

      TJoinEvaluationCacheInfo = class
      strict private
        FChunks: TList<TJoinEvaluationCacheChunk>;
        FAllChunksObjects: TDictionary<TObject, Integer>;
      public
        constructor Create(AChunks: TList<TJoinEvaluationCacheChunk>; AAllChunksObjects: TDictionary<TObject, Integer>);

        property Chunks: TList<TJoinEvaluationCacheChunk> read FChunks;
        property AllChunksObjects: TDictionary<TObject, Integer> read FAllChunksObjects;
      end;

      { TdxJoinEvaluationContextCacheKey }

      TJoinEvaluationContextCacheKey = class
      strict private
        FQueryTypeName: string;
        FCondition: IdxCriteriaOperator;
      public
        constructor Create(const AQueryTypeName: string; const ACondition: IdxCriteriaOperator);
        function GetHashCode: Integer; override;
        function Equals(AObj: TObject): Boolean; override;
      end;

      { TdxCriteriaOperatorKey }

      TCriteriaOperatorKey = class
      strict private
        FCondition: IdxCriteriaOperator;
      public
        constructor Create(const ACondition: IdxCriteriaOperator);
        function GetHashCode: Integer; override;
        function Equals(AObj: TObject): Boolean; override;
        property Condition: IdxCriteriaOperator read FCondition;
      end;
{$ENDREGION}
  strict private
    FCollectionContextStack: TStack<TEnumerable<TdxEvaluatorContext>>;
    FCacheStack: TStack<TDictionary<TJoinEvaluationContextCacheKey, TJoinEvaluationCacheInfo>>;
    FCacheDictionary: TDictionary<TJoinEvaluationContextCacheKey, TJoinEvaluationCacheInfo>;
  public
    constructor Create;
    procedure PushCollectionContext(AContext: TEnumerable<TdxEvaluatorContext>);
    function PopCollectionContext: TEnumerable<TdxEvaluatorContext>;
    function GetQueryContexts(const AContexts: TArray<TdxEvaluatorContext>; const AQueryTypeName: string;
      const ACondition: IdxCriteriaOperator; ATop: Integer; out AFiltered: Boolean): TEnumerable<TdxEvaluatorContext>;
  end;

  { TdxExpressionEvaluatorCoreBase }

  TdxExpressionEvaluatorCoreBase = class abstract(TcxIUnknownObject, IdxCriteriaVisitor<TValue>)
  strict private type
{$REGION 'AggregateProcessingParam types'}

      { TdxAggregateProcessingParam }

      TAggregateProcessingParam = class abstract
      strict private
        FEvaluator: TdxExpressionEvaluatorCoreBase;
      public
        constructor Create(AEvaluator: TdxExpressionEvaluatorCoreBase);
        function GetResult: TValue; virtual; abstract;
        function Process(const AOperand: TValue): Boolean; virtual; abstract;
        property Evaluator: TdxExpressionEvaluatorCoreBase read FEvaluator;
      end;

      { TExistsProcessingParam }

      TExistsProcessingParam = class(TAggregateProcessingParam)
      strict private
        FResult: Boolean;
      public
        function GetResult: TValue; override;
        function Process(const AOperand: TValue): Boolean; override;
      end;

      { TSingleProcessingParam }

      TSingleProcessingParam = class(TAggregateProcessingParam)
      strict private
        FProcessed: Boolean;
        FResult: TValue;
      public
        function GetResult: TValue; override;
        function Process(const AOperand: TValue): Boolean; override;
      end;

      { TCountProcessingParam }

      TCountProcessingParam = class(TAggregateProcessingParam)
      strict private
        FResult: Integer;
      public
        function GetResult: TValue; override;
        function Process(const AOperand: TValue): Boolean; override;
      end;

      { TMinProcessingParam }

      TMinProcessingParam = class(TAggregateProcessingParam)
      strict private
        FResult: TValue;
      public
        function GetResult: TValue; override;
        function Process(const AOperand: TValue): Boolean; override;
      end;

      { TMaxProcessingParam }

      TMaxProcessingParam = class(TAggregateProcessingParam)
      strict private
        FResult: TValue;
      public
        function GetResult: TValue; override;
        function Process(const AOperand: TValue): Boolean; override;
      end;

      { TSumProcessingParam }

      TSumProcessingParam = class(TAggregateProcessingParam)
      strict private
        FResult: TValue;
      public
        function GetResult: TValue; override;
        function Process(const AOperand: TValue): Boolean; override;
      end;

      { TAvgProcessingParam }

      TAvgProcessingParam = class(TAggregateProcessingParam)
      strict private
      public
        function GetResult: TValue; override;
        function Process(const AOperand: TValue): Boolean; override;
      end;
{$ENDREGION}
  strict private
    class var
      FTrueValue: TValue;
      FFalseValue: TValue;
      FFnFunctions: TDictionary<TdxFnFuncKey, TFunc<TArray<TObject>, TObject>>;
    class constructor Initialize;
    class destructor Finalize;
  strict private
  protected
    FCaseSensitive: Boolean;
    function GetHasContext: Boolean; virtual; abstract;
    function FixValue(AValue: TObject): TObject;
    function Process(const AOperand: IdxCriteriaOperator): TValue;
    function Compare(const ALeftValue, ARightValue: TValue; AIsEqualityCompare: Boolean): Integer; overload;
    function Compare(const ALeftValue, ARightValue: TValue): Integer; overload;
    class function GetBool(const AValue: TdxNullableValue<Boolean>): TValue; overload; static;
    class function GetBool(const AValue: TValue): TdxNullableValue<Boolean>; overload; static;
    class function GetValueAsBool(const AValue: TValue): Boolean; overload; static;
    function FnIsSameDay(AOperator: TdxFunctionOperator): TValue;
    function FnIif(AOperator: TdxFunctionOperator): TValue;
    function FnIsNull(AOperator: TdxFunctionOperator): TValue;
    class function EvaluateLambdaFunction(AFnType: TdxFunctionOperatorType; ACaseSensitive: Boolean; AArgs: TArray<TObject>): TValue; static;
    function UnaryNumericPromotions(const AOperand: TValue): TValue;
    procedure DoAggregate(AParam: TAggregateProcessingParam; const AContextsCollection: TEnumerable<TdxEvaluatorContext>;
      const AFilterExpression: IdxCriteriaOperator; const AExpression: IdxCriteriaOperator); overload;
    function DoAggregate(AAggregateFunctionType: TdxAggregateFunctionType; const AContextsCollection: TEnumerable<TdxEvaluatorContext>;
      const AFilterExpression: IdxCriteriaOperator; const AExpression: IdxCriteriaOperator): TValue; overload;
    procedure SetContext(AContext: TdxEvaluatorContext); virtual; abstract;
    procedure ClearContext; virtual; abstract;
    function GetContext: TdxEvaluatorContext; overload; virtual; abstract;
    function GetContext(AUpDepth: Integer): TdxEvaluatorContext; overload;virtual; abstract;
    procedure PushCollectionContext(AContext: TEnumerable<TdxEvaluatorContext>); virtual;
    function PopCollectionContext: TEnumerable<TdxEvaluatorContext>; virtual;
    function Fit(const AFilterCriteria: IdxCriteriaOperator): Boolean; overload;
    // IdxCriteriaVisitor
    function Visit(const AOperator: IdxBetweenOperator): TValue; overload;
    function Visit(const AOperator: IdxBinaryOperator): TValue; overload;
    function Visit(const AOperator: IdxInOperator): TValue; overload;
    function Visit(const AOperator: IdxGroupOperator): TValue; overload;
    function Visit(const AOperator: IdxFunctionOperator): TValue; overload;
    function Visit(const AOperand: IdxOperandValue): TValue; overload;
    function Visit(const AOperator: IdxUnaryOperator): TValue; overload;

    property CaseSensitive: Boolean read FCaseSensitive;
    property HasContext: Boolean read GetHasContext;
  public
    constructor Create(ACaseSensitive: Boolean); overload;
    constructor Create(ACaseSensitive: Boolean; AEvaluateCustomFunction: TdxEvaluateCustomFunctionHandler); overload;
    function Evaluate(AEvaluationContext: TdxEvaluatorContext; const AEvaluatorCriteria: IdxCriteriaOperator): TValue; overload;
    function Evaluate(AEvaluationContext: TdxEvaluatorContext; const AEvaluatorCriteria: IdxCriteriaOperator; const ACustomComparer: IdxComparer): TValue; overload;
    function EvaluateOnObjects(AEvaluatorContextCollection: TEnumerable<TdxEvaluatorContext>; AFilterCriteria: TdxCriteriaOperator; const ACustomComparer: IdxComparer = nil): TArray<TValue>;
    function Filter(const AEvaluatorContextCollection: TEnumerable<TdxEvaluatorContext>; AFilterCriteria: TdxCriteriaOperator): TEnumerable<TdxEvaluatorContext>;
    function Fit(AEvaluationContext: TdxEvaluatorContext; const AFilterCriteria: IdxCriteriaOperator): Boolean; overload;
  end;

  { TdxEvaluatorPropertyCache }

  TdxEvaluatorPropertyCache = class
  strict private
    FStore: TDictionary<TdxOperandProperty, TdxEvaluatorProperty>;
  private
    function GetStore(AIndex: TdxOperandProperty): TdxEvaluatorProperty;
  public
    constructor Create;
    destructor Destroy; override;
    property Store[AIndex: TdxOperandProperty]: TdxEvaluatorProperty read GetStore; default;
  end;

  { TdxExpressionEvaluatorCore }

  TdxExpressionEvaluatorCore = class(TdxExpressionEvaluatorCoreBase, IdxClientCriteriaVisitor<TValue>)
  strict private
    FContexts: TArray<TdxEvaluatorContext>;
    FJoinCache: TdxJoinEvaluationContextCache;
    FPropertyCache: TdxEvaluatorPropertyCache;
  protected
    procedure SetContext(AContext: TdxEvaluatorContext); override;
    procedure ClearContext; override;
    function GetHasContext: Boolean; override;
    function GetContext: TdxEvaluatorContext; override;
    function GetContext(AUpDepth: Integer): TdxEvaluatorContext; override;
    procedure PushCollectionContext(AContext: TEnumerable<TdxEvaluatorContext>); override;
    function PopCollectionContext: TEnumerable<TdxEvaluatorContext>; override;
    function CreateNestedJoinContext(const AJoinTypeName: string; ACondition: TdxCriteriaOperator; ATop: Integer;
      out AFiltered: Boolean): TEnumerable<TdxEvaluatorContext>;
    function CreateNestedContext(ACollectionProperty: TdxEvaluatorProperty): TEnumerable<TdxEvaluatorContext>;
    function Visit(const AOperand: IdxAggregateOperand): TValue; overload;
    function Visit(const AOperand: IdxJoinOperand): TValue; overload;
    function Visit(const AOperand: IdxOperandProperty): TValue; overload;
  public
    constructor Create(ACaseSensitive: Boolean; AEvaluateCustomFunction: TdxEvaluateCustomFunctionHandler = nil); overload;
    destructor Destroy; override;
  end;

  { TdxExpressionEvaluator }

  TdxExpressionEvaluator = class
  strict private
    procedure SetDataAccess(const AValue: IdxEvaluatorDataAccess);
  protected
    FDefaultDescriptor: TdxEvaluatorContextDescriptor;
    FEvaluatorCriteria: IdxCriteriaOperator;
    FEvaluatorCore: TdxExpressionEvaluatorCoreBase;
    FThrowExceptionIfNotFoundCustomFunction: Boolean;
    function GetEvaluatorCore: TdxExpressionEvaluatorCoreBase; virtual;
    function PrepareContext(AValuesSource: TObject): TdxEvaluatorContext; virtual;
    function EvaluateCustomFunction(const AFunctionName: string; const AOperands: TArray<TValue>): TValue; virtual;

    property EvaluatorCore: TdxExpressionEvaluatorCoreBase read GetEvaluatorCore;
  public
    constructor Create(ADescriptor: TdxEvaluatorContextDescriptor; const ACriteria: IdxCriteriaOperator; ACaseSensitive: Boolean; ADoCreateEvaluatorCore: Boolean); overload;
    constructor Create(ADescriptor: TdxEvaluatorContextDescriptor; const ACriteria: IdxCriteriaOperator; ACaseSensitive: Boolean); overload;
    constructor Create(ADescriptor: TdxEvaluatorContextDescriptor; const ACriteria: IdxCriteriaOperator); overload;
    constructor Create(AProperties: TdxPropertyDescriptorCollection; const ACriteria: IdxCriteriaOperator; ACaseSensitive: Boolean; ADoCreateEvaluatorCore: Boolean); overload;
    constructor Create(AProperties: TdxPropertyDescriptorCollection; const ACriteria: IdxCriteriaOperator; ACaseSensitive: Boolean); overload;
    constructor Create(AProperties: TdxPropertyDescriptorCollection; const ACriteria: IdxCriteriaOperator); overload;
    constructor Create(AProperties: TdxPropertyDescriptorCollection; const ACriteria: string; ACaseSensitive: Boolean); overload;
    constructor Create(AProperties: TdxPropertyDescriptorCollection; const ACriteria: string); overload;
    function Evaluate(AObject: TObject): TObject; overload;
    function Evaluate(AObject: TObject; const ACustomComparer: IdxComparer): TObject; overload;
    function EvaluateOnObjects(const AObjects: IEnumerable): TArray<TObject>; overload;
    function EvaluateOnObjects(const AObjects: IEnumerable; const ACustomComparer: IdxComparer): TArray<TObject>; overload;
    function Fit(AObject: TObject): Boolean;
    function Filter(const AObjects: IEnumerable): IEnumerable;

    property DataAccess: IdxEvaluatorDataAccess write SetDataAccess;
    property ThrowExceptionIfNotFoundCustomFunction: Boolean read FThrowExceptionIfNotFoundCustomFunction write FThrowExceptionIfNotFoundCustomFunction;
  end;

  { TdxEvalHelpers }

  TdxEvalHelpers = class
  protected
    class function MakeTypicalOutlookInterval(const AOp: IdxCriteriaOperator; ALowerBound, AUpperBound: TdxFunctionOperatorType): IdxCriteriaOperator; static;
    class function MakeIsSameDayInterval(const AOp: IdxCriteriaOperator; ALowerBound, AUpperBound: TDateTime): IdxCriteriaOperator; static;
    class function MakeIsSameDayCriteria(const AOperator: IdxFunctionOperator): IdxCriteriaOperator; static;
    class function MakeMonthCriteria(const AOp: IdxCriteriaOperator; AMonth: Integer): TdxBinaryOperator; static;
    class function FnLocalDateTimeNextYear: TDateTime; static;
    class function FnLocalDateTimeTwoYearsAway: TDateTime; static;
    class function FnLocalDateTimeNextMonth: TDateTime; static;
    class function FnLocalDateTimeTwoMonthsAway: TDateTime; static;
    class function FnLocalDateTimeTwoWeeksAway: TDateTime; static;
    class function FnLocalDateTimeLastMonth: TDateTime; static;
    class function FnLocalDateTimeLastYear: TDateTime; static;
    class function FnLocalDateTimeNextWeek: TDateTime; static;
    class function FnLocalDateTimeDayAfterTomorrow: TDateTime; static;
    class function FnLocalDateTimeTomorrow: TDateTime; static;
    class function FnLocalDateTimeNow: TDateTime; static;
    class function FnLocalDateTimeToday: TDateTime; static;
    class function FnLocalDateTimeYesterday: TDateTime; static;
    class function FnLocalDateTimeThisWeek: TDateTime; static;
    class function FnLocalDateTimeLastWeek: TDateTime; static;
    class function FnLocalDateTimeThisMonth: TDateTime; static;
    class function FnLocalDateTimeThisYear: TDateTime; static;
    class function FnLocalDateTimeYearBeforeToday: TDateTime; static;
  public
    class function ExpandIsOutlookInterval(const AOperator: IdxFunctionOperator): IdxCriteriaOperator; static;
    class function EvaluateLocalDateTime(AType: TdxFunctionOperatorType): TDateTime; static;
    class function IsLocalDateTime(AFunctionOperatorType: TdxFunctionOperatorType): Boolean; static;
    class function IsOutlookInterval(AFunctionOperatorType: TdxFunctionOperatorType): Boolean; static;
  end;

implementation

uses
  DateUtils,
  dxCore, dxTypeHelpers, dxStringHelper,
  dxEMF.Strs;

type

  { TdxJoinContextCriteriaPatcher }

  TdxJoinContextCriteriaPatcher = class(TdxClientCriteriaVisitorBase)
  strict private
    FValueInfoSet: TdxJoinContextValueInfoSet;
  protected
    function DoVisit(const AOperand: IdxOperandProperty): IdxCriteriaOperator; override;
    function DoVisit(const AOperand: IdxAggregateOperand; AProcessCollectionProperty: Boolean): IdxCriteriaOperator; override;
  public
    constructor Create(AValueInfoSet: TdxJoinContextValueInfoSet);
    class function Process(AValueInfoSet: TdxJoinContextValueInfoSet; const ACriteria: IdxCriteriaOperator): IdxCriteriaOperator; overload; static;
  end;

  { TdxJoinContextCriteriaCreator }

  TdxJoinContextCriteriaCreator = class(TdxClientCriteriaVisitorBase)
  strict private
    FLevel: Integer;
    FZeroLevelLeave: Boolean;
    FContexts: TArray<TdxEvaluatorContext>;
    FZeroLevelProperties: TDictionary<TdxJoinContextPropertyInfo, Boolean>;
    FPropertyCache: TDictionary<string, TdxEvaluatorProperty>;
  protected
    function DoVisit(const AOperand: IdxAggregateOperand; AProcessCollectionProperty: Boolean): IdxCriteriaOperator; override;
    function DoVisit(const AOperand: IdxOperandProperty): IdxCriteriaOperator; override;
    function DoVisit(const AOperand: IdxJoinOperand): IdxCriteriaOperator; override;
  public
    constructor Create(const AContexts: TArray<TdxEvaluatorContext>); overload;
    constructor Create(const AContexts: TArray<TdxEvaluatorContext>; AZeroLevelLeave: Boolean); overload;
    destructor Destroy; override;
    class function ProcessZeroLevelLeave(const AContexts: TArray<TdxEvaluatorContext>; const ACriteria: IdxCriteriaOperator; out AZeroLevelProperties: TdxJoinContextPropertyInfoSet): IdxCriteriaOperator; static;
    class function Process(const AContexts: TArray<TdxEvaluatorContext>; const ACriteria: IdxCriteriaOperator): IdxCriteriaOperator; overload; static;
    function GetProperty(const APropertyPath: string): TdxEvaluatorProperty;
    property ZeroLevelLeave: Boolean read FZeroLevelLeave;
    property ZeroLevelProperties: TDictionary<TdxJoinContextPropertyInfo, Boolean> read FZeroLevelProperties;
  end;

{ TdxJoinContextCriteriaPatcher }

constructor TdxJoinContextCriteriaPatcher.Create(AValueInfoSet: TdxJoinContextValueInfoSet);
begin
  inherited Create;
  FValueInfoSet := AValueInfoSet;
end;

class function TdxJoinContextCriteriaPatcher.Process(AValueInfoSet: TdxJoinContextValueInfoSet;
  const ACriteria: IdxCriteriaOperator): IdxCriteriaOperator;
begin
  with TdxJoinContextCriteriaPatcher.Create(AValueInfoSet) do
  try
    Result := Process(ACriteria);
  finally
    Free;
  end;
end;

function TdxJoinContextCriteriaPatcher.DoVisit(const AOperand: IdxOperandProperty): IdxCriteriaOperator;
var
  AValue: TValue;
begin
  if FValueInfoSet.Properties.TryGetValue(AOperand.PropertyName, AValue) then
    Result := TdxOperandValue.Create(AValue)
  else
    Result := AOperand;
end;

function TdxJoinContextCriteriaPatcher.DoVisit(const AOperand: IdxAggregateOperand; AProcessCollectionProperty: Boolean): IdxCriteriaOperator;
begin
  Result := inherited DoVisit(AOperand, False);
end;

{ TdxJoinContextCriteriaCreator }

constructor TdxJoinContextCriteriaCreator.Create(const AContexts: TArray<TdxEvaluatorContext>);
begin
  inherited Create;
  FLevel := 1;
  FPropertyCache := TDictionary<string, TdxEvaluatorProperty>.Create;
  FContexts := AContexts;
end;

constructor TdxJoinContextCriteriaCreator.Create(const AContexts: TArray<TdxEvaluatorContext>; AZeroLevelLeave: Boolean);
begin
  Create(AContexts);
  FZeroLevelProperties := TDictionary<TdxJoinContextPropertyInfo, Boolean>.Create;
  FZeroLevelLeave := AZeroLevelLeave;
end;

destructor TdxJoinContextCriteriaCreator.Destroy;
begin
  FreeAndNil(FPropertyCache);
  inherited Destroy;
end;

class function TdxJoinContextCriteriaCreator.ProcessZeroLevelLeave(const AContexts: TArray<TdxEvaluatorContext>;
  const ACriteria: IdxCriteriaOperator; out AZeroLevelProperties: TdxJoinContextPropertyInfoSet): IdxCriteriaOperator;
var
  AJoinContextCriteriaCreator: TdxJoinContextCriteriaCreator;
begin
  AJoinContextCriteriaCreator := TdxJoinContextCriteriaCreator.Create(AContexts, True);
  try
    Result := AJoinContextCriteriaCreator.Process(ACriteria as TdxCriteriaOperator);
    AZeroLevelProperties := TdxJoinContextPropertyInfoSet.Create(AJoinContextCriteriaCreator.ZeroLevelProperties);
  finally
    AJoinContextCriteriaCreator.Free;
  end;
end;

class function TdxJoinContextCriteriaCreator.Process(const AContexts: TArray<TdxEvaluatorContext>;
  const ACriteria: IdxCriteriaOperator): IdxCriteriaOperator;
begin
  Result := TdxJoinContextCriteriaCreator.Create(AContexts).Process(ACriteria);
end;

function TdxJoinContextCriteriaCreator.GetProperty(const APropertyPath: string): TdxEvaluatorProperty;
var
  AResult: TdxEvaluatorProperty;
begin
  if not FPropertyCache.TryGetValue(APropertyPath, AResult) then
  begin
    AResult := TdxEvaluatorProperty.Create(TdxOperandProperty.Create(APropertyPath));
    FPropertyCache.Add(APropertyPath, AResult);
  end;
  Result := AResult;
end;

function TdxJoinContextCriteriaCreator.DoVisit(const AOperand: IdxAggregateOperand; AProcessCollectionProperty: Boolean): IdxCriteriaOperator;
begin
  Inc(FLevel);
  try
    Result := inherited DoVisit(AOperand, False);
  finally
    Dec(FLevel);
  end;
end;

function TdxJoinContextCriteriaCreator.DoVisit(const AOperand: IdxOperandProperty): IdxCriteriaOperator;
var
  APropertyPath, APropertyIdName: string;
  ACurrentLevel: Integer;
begin
  APropertyPath := AOperand.PropertyName;
  ACurrentLevel := FLevel;
  while {$IFDEF DELPHIXE3}APropertyPath.StartsWith('^.'){$ELSE}TdxStringHelper.StartsWith(APropertyPath, '^.'){$ENDIF} do
  begin
    APropertyPath := {$IFDEF DELPHIXE3}APropertyPath.Substring(2){$ELSE}TdxStringHelper.Substring(APropertyPath, 2){$ENDIF};
    Dec(ACurrentLevel);
  end;
  if ACurrentLevel <= 0 then
  begin
    if FZeroLevelLeave and (ACurrentLevel = 0) then
    begin
      APropertyIdName := Format('%d#|#%s', [FLevel, AOperand.PropertyName]);
      FZeroLevelProperties.AddOrSetValue(TdxJoinContextPropertyInfo.Create(GetProperty(APropertyPath), APropertyIdName), True);
      Exit(TdxOperandProperty.Create(APropertyIdName));
    end;
    Exit(TdxOperandValue.Create(FContexts[1 - ACurrentLevel].GetPropertyValue(GetProperty(APropertyPath))));
  end;
  Result := AOperand;
end;

function TdxJoinContextCriteriaCreator.DoVisit(const AOperand: IdxJoinOperand): IdxCriteriaOperator;
begin
  Inc(FLevel);
  try
    Result := inherited Visit(AOperand);
  finally
    Dec(FLevel);
  end;
end;

{ TdxClientCriteriaVisitorBase }

function TdxClientCriteriaVisitorBase.Visit(const AOperand: IdxOperandProperty): IdxCriteriaOperator;
begin
  Result := DoVisit(AOperand);
end;

function TdxClientCriteriaVisitorBase.DoVisit(const AOperand: IdxAggregateOperand; AProcessCollectionProperty: Boolean): IdxCriteriaOperator;
var
  ACollectionProperty: IdxOperandProperty;
  AAggregatedExpression, ACondition: IdxCriteriaOperator;
begin
  if AProcessCollectionProperty then
    ACollectionProperty := TdxOperandProperty(Process(AOperand.CollectionProperty))
  else
    ACollectionProperty := AOperand.CollectionProperty;
  AAggregatedExpression := Process(AOperand.AggregatedExpression);
  ACondition := Process(AOperand.Condition);
  if ((ACollectionProperty = AOperand.CollectionProperty) and (AAggregatedExpression = AOperand.AggregatedExpression)) and
    (ACondition = AOperand.Condition) then
    Result := AOperand
  else
    Result := TdxAggregateOperand.Create(ACollectionProperty, AAggregatedExpression, AOperand.AggregateFunctionType, ACondition);
end;

function TdxClientCriteriaVisitorBase.Visit(const AOperand: IdxJoinOperand): IdxCriteriaOperator;
begin
  Result := DoVisit(AOperand);
end;

function TdxClientCriteriaVisitorBase.Visit(const AOperator: IdxFunctionOperator): IdxCriteriaOperator;
var
  AModified: Boolean;
  AResultOperands: IdxCriteriaOperatorCollection;
begin
  AResultOperands := ProcessCollection(AOperator.Operands, AModified);
  if AModified then
    Result := TdxFunctionOperator.Create(AOperator.OperatorType, AResultOperands)
  else
    Result := AOperator;
end;

function TdxClientCriteriaVisitorBase.Visit(const AOperand: IdxOperandValue): IdxCriteriaOperator;
begin
  Result := AOperand;
end;

function TdxClientCriteriaVisitorBase.Visit(const AOperator: IdxGroupOperator): IdxCriteriaOperator;
var
  AModified: Boolean;
  AResultOperands: IdxCriteriaOperatorCollection;
begin
  AResultOperands := ProcessCollection(AOperator.Operands, AModified);
  if AModified then
    Result := TdxGroupOperator.Create(AOperator.OperatorType, AResultOperands)
  else
    Result := AOperator;
end;

function TdxClientCriteriaVisitorBase.Visit(const AOperator: IdxInOperator): IdxCriteriaOperator;
var
  AModified: Boolean;
  ALeftOperand: IdxCriteriaOperator;
  AResultOperands: IdxCriteriaOperatorCollection;
begin
  ALeftOperand := Process(AOperator.LeftOperand);
  AResultOperands := ProcessCollection(AOperator.Operands, AModified);
  if (AModified) or (ALeftOperand <> AOperator.LeftOperand) then
    Result := TdxInOperator.Create(ALeftOperand, AResultOperands)
  else
    Result := AOperator;
end;

function TdxClientCriteriaVisitorBase.Visit(const AOperator: IdxUnaryOperator): IdxCriteriaOperator;
var
  AOperand: IdxCriteriaOperator;
begin
  AOperand := Process(AOperator.Operand);
  if (AOperand = AOperator.Operand) then
    Result := AOperator
  else
    Result := TdxUnaryOperator.Create(AOperator.OperatorType, AOperand);
end;

function TdxClientCriteriaVisitorBase.Visit(const AOperator: IdxBinaryOperator): IdxCriteriaOperator;
var
  ALeftOperand, ARightOperand: IdxCriteriaOperator;
begin
  ALeftOperand := Process(AOperator.LeftOperand);
  ARightOperand := Process(AOperator.RightOperand);
  if (ALeftOperand = AOperator.LeftOperand) and (ARightOperand = AOperator.RightOperand) then
    Result := AOperator
  else
    Result := TdxBinaryOperator.Create(ALeftOperand, ARightOperand, AOperator.OperatorType);
end;

function TdxClientCriteriaVisitorBase.Visit(const AOperator: IdxBetweenOperator): IdxCriteriaOperator;
var
  ATest, ABegin, AEnd: IdxCriteriaOperator;
begin
  ATest := Process(AOperator.TestExpression);
  ABegin := Process(AOperator.BeginExpression);
  AEnd := Process(AOperator.EndExpression);
  if ((ATest = AOperator.TestExpression) and (ABegin = AOperator.BeginExpression)) and
    (AEnd = AOperator.EndExpression) then
    Result := AOperator
  else
    Result := TdxBetweenOperator.Create(ATest, ABegin, AEnd);
end;

function TdxClientCriteriaVisitorBase.DoVisit(const AOperand: IdxOperandProperty): IdxCriteriaOperator;
begin
  Result := AOperand;
end;

function TdxClientCriteriaVisitorBase.DoVisit(const AOperand: IdxJoinOperand): IdxCriteriaOperator;
var
  AAggregatedExpression, ACondition: IdxCriteriaOperator;
begin
  AAggregatedExpression := Process(AOperand.AggregatedExpression);
  ACondition := Process(AOperand.Condition);
  if (AAggregatedExpression = AOperand.AggregatedExpression) and (ACondition = AOperand.Condition) then
    Exit(AOperand);
  Result := TdxJoinOperand.Create(AOperand.JoinTypeName, Process(AOperand.Condition), AOperand.AggregateFunctionType,
    Process(AOperand.AggregatedExpression));
end;

function TdxClientCriteriaVisitorBase.Process(const AInput: IdxCriteriaOperator): IdxCriteriaOperator;
begin
  if AInput = nil then
    Result := nil
  else
    Result := (AInput as TdxCriteriaOperator).Accept(Self);
end;

function TdxClientCriteriaVisitorBase.ProcessCollection(const AOperands: IdxCriteriaOperatorCollection;
  out AModified: Boolean): IdxCriteriaOperatorCollection;
var
  AResultOperands: IdxCriteriaOperatorCollection;
  I: Integer;
  AResultOp: IdxCriteriaOperator;
begin
  AModified := False;
  AResultOperands := TdxCriteriaOperatorCollection.Create(AOperands.Count);
  for I := 0 to AOperands.Count - 1 do
  begin
    AResultOp := Process(AOperands[I]);
    if not AModified and (AResultOp <> AOperands[I]) then
      AModified := True;
    AResultOperands.Add(AResultOp);
  end;
  Result := AResultOperands;
end;

function TdxClientCriteriaVisitorBase.Visit(const AOperand: IdxAggregateOperand): IdxCriteriaOperator;
begin
  Result := DoVisit(AOperand, True);
end;

{ TdxNodeCriteriaFinder }

class function TdxNodeCriteriaFinder.FindCriteria(const ACurrentNodeAlias: string;
  const ACriteria: IdxCriteriaOperator): TDictionary<string, TdxPlanAliasCriteriaInfo>;
var
  AFinder: TdxNodeCriteriaFinder;
begin
  AFinder := TdxNodeCriteriaFinder.Create;
  try
    Result := AFinder.Find(ACurrentNodeAlias, ACriteria);
  finally
    AFinder.Free;
  end;
end;

class procedure TdxNodeCriteriaFinder.FindCriteria(const ACurrentNodeAlias: string; const ACriteria: IdxCriteriaOperator;
  APreviousDictionary: TDictionary<string, TdxPlanAliasCriteriaInfo>);
var
  AFinder: TdxNodeCriteriaFinder;
begin
  AFinder := TdxNodeCriteriaFinder.Create;
  try
    AFinder.Find(ACurrentNodeAlias, ACriteria, APreviousDictionary);
  finally
    AFinder.Free;
  end;
end;

function TdxNodeCriteriaFinder.Find(const ACurrentNodeAlias: string; const ACriteria: IdxCriteriaOperator): TDictionary<string, TdxPlanAliasCriteriaInfo>;
begin
  Find(ACurrentNodeAlias, ACriteria, TDictionary<string, TdxPlanAliasCriteriaInfo>.Create);
  Result := FCriteriaDictionary;
end;

procedure TdxNodeCriteriaFinder.Find(const ACurrentNodeAlias: string; const ACriteria: IdxCriteriaOperator;
  APreviousDictionary: TDictionary<string, TdxPlanAliasCriteriaInfo>);
begin
  if APreviousDictionary = nil then
    raise EArgumentNilException.Create('');
  FCurrentNodeAlias := ACurrentNodeAlias;
  FCriteriaDictionary := APreviousDictionary;
  ProcessAddOperands(TArray<IdxCriteriaOperator>.Create(ACriteria));
end;

function TdxNodeCriteriaFinder.Process(const ACriteria: IdxCriteriaOperator): TList<string>;
begin
  if ACriteria = nil then
    Result := nil
  else
    Result := (ACriteria as TdxCriteriaOperator).Accept(Self);
end;

function TdxNodeCriteriaFinder.Visit(const AOperand: IdxQuerySubQueryContainer): TList<string>;
begin
  Result := nil;
  NotImplemented;
end;

function TdxNodeCriteriaFinder.Visit(const AOperand: IdxQueryOperand): TList<string>;
var
  AList: TList<string>;
begin
  AList := TList<string>.Create;
  AList.Add(AOperand.NodeAlias);
  Result := AList;
end;

function TdxNodeCriteriaFinder.Visit(const AOperator: IdxFunctionOperator): TList<string>;
begin
  Inc(FInAtomicGroupLevel);
  try
    Result := ProcessOperands(AOperator.Operands);
  finally
    Dec(FInAtomicGroupLevel);
  end;
end;

function TdxNodeCriteriaFinder.Visit(const AOperand: IdxOperandValue): TList<string>;
begin
  Result := nil;
end;

function TdxNodeCriteriaFinder.Visit(const AOperator: IdxGroupOperator): TList<string>;
begin
  if AOperator.OperatorType = TdxGroupOperatorType.Or then
  begin
    Inc(FInAtomicGroupLevel);
    try
      Result := ProcessOperands(AOperator.Operands);
    finally
      Dec(FInAtomicGroupLevel);
    end;
  end
  else
  begin
    if FInAtomicGroupLevel > 0 then
      Result := ProcessOperands(AOperator.Operands)
    else
      Result := ProcessAddOperands((AOperator.Operands as TdxCriteriaOperatorCollection).ToArray);
  end;
end;

function TdxNodeCriteriaFinder.ProcessAddOperands(const AOperands: TArray<IdxCriteriaOperator>): TList<string>;
var
  ACount, I: Integer;
  ACurrentList: TList<string>;
  ANodesString: string;
  AList: TdxPlanAliasCriteriaInfo;
begin
  ACount := Length(AOperands);
  for I := 0 to ACount - 1 do
  begin
    ACurrentList := Process(AOperands[I] as TdxCriteriaOperator);
    if ACurrentList = nil then
      Continue;
    try
      if FCurrentNodeAlias <> '' then
        ACurrentList.Add('*' + FCurrentNodeAlias);
      ANodesString := GetNodesString(ACurrentList);
    finally
      ACurrentList.Free;
    end;
    if not FCriteriaDictionary.TryGetValue(ANodesString, AList) then
    begin
      AList := TdxPlanAliasCriteriaInfo.Create({$IFDEF DELPHIXE3}ANodesString.Split(['.']){$ELSE}TdxStringHelper.Split(ANodesString, ['.']){$ENDIF},
        TdxCriteriaOperatorList.Create);
      FCriteriaDictionary.Add(ANodesString, AList);
    end;
    AList.Criteria.Add(AOperands[I]);
  end;
  Result := nil;
end;

class function TdxNodeCriteriaFinder.GetNodesString(AList: TList<string>): string;
var
  ASb: TStringBuilder;
  ACount, I: Integer;
  APrevNode: string;
begin
  AList.Sort;
  ASb := TStringBuilder.Create;
  try
    ACount := AList.Count;
    APrevNode := '';
    for I := 0 to ACount - 1 do
    begin
      if AList[I] = APrevNode then
        Continue;
      if I > 0 then
        ASb.Append('.');
      ASb.Append(AList[I]);
      APrevNode := AList[I];
    end;
    Result := ASb.ToString;
  finally
    ASb.Free;
  end;
end;

function TdxNodeCriteriaFinder.ProcessOperands(const AOperands: IdxCriteriaOperatorCollection): TList<string>;
var
  AList, ACurrentList: TList<string>;
  ACount, I: Integer;
begin
  AList := nil;
  ACount := AOperands.Count;
  for I := 0 to ACount - 1 do
  begin
    ACurrentList := Process(AOperands[I]);
    if ACurrentList = nil then
      Continue;
    if AList = nil then
      AList := ACurrentList
    else
    begin
      AList.AddRange(ACurrentList);
      ACurrentList.Free;
    end;
  end;
  Result := AList;
end;

function TdxNodeCriteriaFinder.Visit(const AOperator: IdxInOperator): TList<string>;
var
  AOperands: IdxCriteriaOperatorCollection;
begin
  Inc(FInAtomicGroupLevel);
  try
    AOperands := TdxCriteriaOperatorCollection.Create;
    AOperands.Add(AOperator.LeftOperand);
    TdxCriteriaOperatorCollection(AOperands).AddRange(AOperator.Operands);
    Result := ProcessOperands(AOperands);
  finally
    Dec(FInAtomicGroupLevel);
  end;
end;

function TdxNodeCriteriaFinder.Visit(const AOperator: IdxUnaryOperator): TList<string>;
begin
  Inc(FInAtomicGroupLevel);
  try
    Result := Process(AOperator.Operand as TdxCriteriaOperator);
  finally
    Dec(FInAtomicGroupLevel);
  end;
end;

function TdxNodeCriteriaFinder.Visit(const AOperator: IdxBinaryOperator): TList<string>;
var
  ALeftResult, ARightResult: TList<string>;
begin
  Inc(FInAtomicGroupLevel);
  try
    ALeftResult := Process(AOperator.LeftOperand as TdxCriteriaOperator);
    ARightResult := Process(AOperator.RightOperand as TdxCriteriaOperator);
    if ALeftResult = nil then
      Exit(ARightResult);
    if ARightResult = nil then
      Exit(ALeftResult);
    ALeftResult.AddRange(ARightResult);
    ARightResult.Free;
    Result := ALeftResult;
  finally
    Dec(FInAtomicGroupLevel);
  end;
end;

function TdxNodeCriteriaFinder.Visit(const AOperator: IdxBetweenOperator): TList<string>;
var
  ABeginResult, AEndResult, ATestResult, ATempResult: TList<string>;
begin
  Inc(FInAtomicGroupLevel);
  try
    ABeginResult := Process(AOperator.BeginExpression as TdxCriteriaOperator);
    AEndResult := Process(AOperator.EndExpression as TdxCriteriaOperator);
    ATestResult := Process(AOperator.TestExpression as TdxCriteriaOperator);
    if AEndResult = nil then
      ATempResult := ATestResult
    else
    begin
      ATempResult := AEndResult;
      if ATestResult <> nil then
        ATempResult.AddRange(ATestResult);
    end;
    if ABeginResult = nil then
      Exit(ATempResult);
    if ATempResult = nil then
      Exit(ABeginResult);
    ABeginResult.AddRange(ATempResult);
    Result := ABeginResult;
  finally
    Dec(FInAtomicGroupLevel);
  end;
end;

{ TdxIsLogicalCriteriaChecker }

class function TdxIsLogicalCriteriaChecker.GetInstance: TdxIsLogicalCriteriaChecker;
begin
  if FInstance = nil then
    FInstance := TdxIsLogicalCriteriaChecker.Create;
  Result := FInstance;
end;

class destructor TdxIsLogicalCriteriaChecker.Finalize;
begin
  FreeAndNil(FInstance);
end;

class function TdxIsLogicalCriteriaChecker.GetBooleanState(const AOperand: IdxCriteriaOperator): TdxBooleanCriteriaState;
begin
  Result := Instance.Process(AOperand as TdxCriteriaOperator);
end;

function TdxIsLogicalCriteriaChecker.Visit(const AOperand: IdxAggregateOperand): TdxBooleanCriteriaState;
begin
  if AOperand.AggregateFunctionType = TdxAggregateFunctionType.Exists then
    Result := TdxBooleanCriteriaState.Logical
  else
    Result := TdxBooleanCriteriaState.Value;
end;

function TdxIsLogicalCriteriaChecker.Visit(const AOperand: IdxJoinOperand): TdxBooleanCriteriaState;
begin
  if AOperand.AggregateFunctionType = TdxAggregateFunctionType.Exists then
    Result := TdxBooleanCriteriaState.Logical
  else
    Result := TdxBooleanCriteriaState.Value;
end;

function TdxIsLogicalCriteriaChecker.Visit(const AOperand: IdxOperandProperty): TdxBooleanCriteriaState;
begin
  Result := TdxBooleanCriteriaState.Value;
end;

function TdxIsLogicalCriteriaChecker.Visit(const AOperator: IdxBetweenOperator): TdxBooleanCriteriaState;
begin
  Result := TdxBooleanCriteriaState.Logical;
end;

function TdxIsLogicalCriteriaChecker.Visit(const AOperator: IdxBinaryOperator): TdxBooleanCriteriaState;
begin
  case AOperator.OperatorType of
    TdxBinaryOperatorType.Equal,
    TdxBinaryOperatorType.Greater,
    TdxBinaryOperatorType.GreaterOrEqual,
    TdxBinaryOperatorType.Less,
    TdxBinaryOperatorType.LessOrEqual,
    TdxBinaryOperatorType.NotEqual:
      Result := TdxBooleanCriteriaState.Logical;
    else
      Result := TdxBooleanCriteriaState.Value;
  end;
end;

function TdxIsLogicalCriteriaChecker.Visit(const AOperator: IdxUnaryOperator): TdxBooleanCriteriaState;
begin
  if AOperator.OperatorType = TdxUnaryOperatorType.Not then
    Exit(TdxBooleanCriteriaState.Logical);
  if AOperator.OperatorType = TdxUnaryOperatorType.IsNull then
    Exit(TdxBooleanCriteriaState.Logical);
  Result := TdxBooleanCriteriaState.Value;
end;

function TdxIsLogicalCriteriaChecker.Visit(const AOperator: IdxInOperator): TdxBooleanCriteriaState;
begin
  Result := TdxBooleanCriteriaState.Logical;
end;

function TdxIsLogicalCriteriaChecker.Visit(const AOperator: IdxGroupOperator): TdxBooleanCriteriaState;
begin
  Result := TdxBooleanCriteriaState.Logical;
end;

function TdxIsLogicalCriteriaChecker.Visit(const AOperand: IdxOperandValue): TdxBooleanCriteriaState;
begin
  Result := TdxBooleanCriteriaState.Value;
end;

function TdxIsLogicalCriteriaChecker.Visit(const AOperator: IdxFunctionOperator): TdxBooleanCriteriaState;
begin
  if AOperator.OperatorType = TdxFunctionOperatorType.IsNull then
  begin
    if AOperator.Operands.Count = 2 then
      Exit(TdxBooleanCriteriaState.Value);
    Exit(TdxBooleanCriteriaState.Logical);
  end;
  case AOperator.OperatorType of
    TdxFunctionOperatorType.IsOutlookIntervalBeyondThisYear,
    TdxFunctionOperatorType.IsOutlookIntervalEarlierThisMonth,
    TdxFunctionOperatorType.IsOutlookIntervalEarlierThisWeek,
    TdxFunctionOperatorType.IsOutlookIntervalEarlierThisYear,
    TdxFunctionOperatorType.IsOutlookIntervalLastWeek,
    TdxFunctionOperatorType.IsOutlookIntervalLaterThisMonth,
    TdxFunctionOperatorType.IsOutlookIntervalLaterThisWeek,
    TdxFunctionOperatorType.IsOutlookIntervalLaterThisYear,
    TdxFunctionOperatorType.IsOutlookIntervalNextWeek,
    TdxFunctionOperatorType.IsOutlookIntervalPriorThisYear,
    TdxFunctionOperatorType.IsOutlookIntervalToday,
    TdxFunctionOperatorType.IsOutlookIntervalTomorrow,
    TdxFunctionOperatorType.IsOutlookIntervalYesterday,
    TdxFunctionOperatorType.IsNullOrEmpty,
    TdxFunctionOperatorType.StartsWith,
    TdxFunctionOperatorType.EndsWith,
    TdxFunctionOperatorType.Contains,
    TdxFunctionOperatorType.IsThisMonth,
    TdxFunctionOperatorType.IsThisWeek,
    TdxFunctionOperatorType.IsThisYear,
    TdxFunctionOperatorType.IsNextMonth,
    TdxFunctionOperatorType.IsNextYear,
    TdxFunctionOperatorType.IsLastMonth,
    TdxFunctionOperatorType.IsLastYear,
    TdxFunctionOperatorType.IsYearToDate,
    TdxFunctionOperatorType.IsJanuary,
    TdxFunctionOperatorType.IsFebruary,
    TdxFunctionOperatorType.IsMarch,
    TdxFunctionOperatorType.IsApril,
    TdxFunctionOperatorType.IsMay,
    TdxFunctionOperatorType.IsJune,
    TdxFunctionOperatorType.IsJuly,
    TdxFunctionOperatorType.IsAugust,
    TdxFunctionOperatorType.IsSeptember,
    TdxFunctionOperatorType.IsOctober,
    TdxFunctionOperatorType.IsNovember,
    TdxFunctionOperatorType.IsDecember,
    TdxFunctionOperatorType.IsSameDay:
      Exit(TdxBooleanCriteriaState.Logical);
    TdxFunctionOperatorType.Custom,
    TdxFunctionOperatorType.CustomNonDeterministic:
      if TdxFunctionOperator.GuessIsLogicalCustomFunction(AOperator) then
        Exit(TdxBooleanCriteriaState.Logical)
      else
        Exit(TdxBooleanCriteriaState.Undefined);
  end;
  Result := TdxBooleanCriteriaState.Value;
end;

function TdxIsLogicalCriteriaChecker.Process(AOperand: TdxCriteriaOperator): TdxBooleanCriteriaState;
begin
  if AOperand = nil then
    Result := TdxBooleanCriteriaState.Logical
  else
    Result := AOperand.Accept(Self);
end;

{ TdxEvaluatorProperty }

constructor TdxEvaluatorProperty.Create(const ASourcePath: string);
begin
  inherited Create;
  FPropertyPath := ASourcePath;
  while {$IFDEF DELPHIXE3}FPropertyPath.StartsWith('^.'){$ELSE}TdxStringHelper.StartsWith(FPropertyPath, '^.'){$ENDIF} do
  begin
    Inc(FUpDepth);
    FPropertyPath := {$IFDEF DELPHIXE3}FPropertyPath.Substring(2){$ELSE}TdxStringHelper.Substring(FPropertyPath, 2){$ENDIF};
  end;
  if FPropertyPath = '^' then
  begin
    Inc(FUpDepth);
    FPropertyPath := 'This';
  end;
end;

class function TdxEvaluatorProperty.GetPropertySeparatorDotPos(const AProperty: string): Integer;
begin
  Result := GetPropertySeparatorDotPos(AProperty, 0);
end;

class function TdxEvaluatorProperty.GetPropertySeparatorDotPos(const AProperty: string; AStartPos: Integer): Integer;
var
  APos: Integer;
begin
  if AProperty = '' then
    raise EArgumentNilException.Create('property');
  APos := AStartPos;
  while APos < Length(AProperty) do
  begin
    case AProperty[APos] of
      '.':
        Exit(APos);
      '<':
        while (APos < Length(AProperty)) and (PChar(AProperty)[APos] <> '>') do
          Inc(APos);
    end;
  end;
  Result := -1;
end;

class function TdxEvaluatorProperty.CalcCollectionPropertyDepth(const AProp: string): Integer;
var
  ARv, APos: Integer;
begin
  ARv := 1;
  APos := 0;
  while True do
  begin
    APos := GetPropertySeparatorDotPos(AProp, APos);
    if APos < 0 then
      Exit(ARv)
    else
    begin
      Inc(APos);
      Inc(ARv);
    end;
  end;
end;

class function TdxEvaluatorProperty.Split(const AProp: string): TArray<string>;
var
  ARv: TList<string>;
  ASubPropertyStart, ADotPos: Integer;
begin
  ARv := TList<string>.Create;
  ASubPropertyStart := 0;
  while True do
  begin
    ADotPos := GetPropertySeparatorDotPos(AProp, ASubPropertyStart);
    if ADotPos < 0 then
    begin
      ARv.Add({$IFDEF DELPHIXE3}AProp.Substring(ASubPropertyStart){$ELSE}TdxStringHelper.Substring(AProp, ASubPropertyStart){$ENDIF});
      Break;
    end
    else
    begin
      {$IFDEF DELPHIXE3}
      ARv.Add(AProp.Substring(ASubPropertyStart, ADotPos - ASubPropertyStart));
      {$ELSE}
      ARv.Add(TdxStringHelper.Substring(AProp, ASubPropertyStart, ADotPos - ASubPropertyStart));
      {$ENDIF}
      ASubPropertyStart := ADotPos + 1;
    end;
  end;
  Result := ARv.ToArray;
end;

function TdxEvaluatorProperty.GetSubProperty: TdxEvaluatorProperty;
begin
  if (FSubProperty = nil) and (Length(PropertyPathTokenized) > 1) then
  {$IFDEF DELPHIXE3}
    FSubProperty := TdxEvaluatorProperty.Create(string.Join('.', PropertyPathTokenized, 1, Length(PropertyPathTokenized) - 1));
  {$ELSE}
    FSubProperty := TdxEvaluatorProperty.Create(TdxStringHelper.Join('.', PropertyPathTokenized, 1, Length(PropertyPathTokenized) - 1));
  {$ENDIF}
  Result := FSubProperty;
end;

function TdxEvaluatorProperty.GetPropertyPathTokenized: TArray<string>;
begin
  if FTokenized = nil then
    FTokenized := Split(PropertyPath);
  Result := FTokenized;
end;

class function TdxEvaluatorProperty.Create(const AProperty: IdxOperandProperty): TdxEvaluatorProperty;
begin
  Result := TdxEvaluatorProperty.Create(AProperty.PropertyName);
end;

class function TdxEvaluatorProperty.GetIsThisProperty(const APropertyName: string): Boolean;
const
  ThisLowerString = 'this';
begin
  if APropertyName = '' then
    Exit(True);
  Result := (Length(APropertyName) = Length(ThisLowerString)) and (LowerCase(APropertyName) = ThisLowerString)
end;

{ TdxEvaluatorContext }

constructor TdxEvaluatorContext.Create(ADescriptor: TdxEvaluatorContextDescriptor; ASource: TObject);
begin
  inherited Create;
  FDescriptor := ADescriptor;
  FSource := ASource;
end;

function TdxEvaluatorContext.GetPropertyValue(APropertyPath: TdxEvaluatorProperty): TObject;
begin
  Result := Descriptor.GetPropertyValue(Source, APropertyPath);
end;

function TdxEvaluatorContext.GetNestedContext(const APropertyPath: string): TdxEvaluatorContext;
begin
  Result := Descriptor.GetNestedContext(Source, APropertyPath);
end;

function TdxEvaluatorContext.GetCollectionContexts(const ACollectionName: string): IEnumerable;
begin
  Result := Descriptor.GetCollectionContexts(Source, ACollectionName);
end;

function TdxEvaluatorContext.GetQueryContexts(const AQueryTypeName: string;
  const ACondition: IdxCriteriaOperator; ATop: Integer): TEnumerable<TdxEvaluatorContext>;
begin
  Result := Descriptor.GetQueryContexts(Source, AQueryTypeName, ACondition, ATop);
end;

{ TdxEvaluatorContextDescriptor }

function TdxEvaluatorContextDescriptor.GetQueryContexts(ASource: TObject; const AQueryTypeName: string;
  const ACondition: IdxCriteriaOperator; ATop: Integer): TEnumerable<TdxEvaluatorContext>;
begin
  raise ENotSupportedException.Create('');
end;

function TdxEvaluatorContextDescriptor.GetIsTopLevelCollectionSource: Boolean;
begin
  Result := False;
end;

{ TdxFnFuncKey }

constructor TdxFnFuncKey.Create(AFnType: TdxFunctionOperatorType; ACaseSensitive: Boolean; AArgumentCount: Integer);
begin
  FFnType := AFnType;
  FCaseSensitive := ACaseSensitive;
  FArgumentCount := AArgumentCount;
end;

function TdxFnFuncKey.GetHashCode: Integer;
begin
  Result := 0;
end;

function TdxFnFuncKey.Equals(const AOther: TdxFnFuncKey): Boolean;
begin
  Result := ((FFnType = AOther.FFnType) and (FArgumentCount = AOther.FArgumentCount)) and (FCaseSensitive = AOther.FCaseSensitive);
end;

{ TdxJoinContextPropertyInfo }

constructor TdxJoinContextPropertyInfo.Create(AProperty: TdxEvaluatorProperty; const APropertyNameInCriteria: string);
begin
  FProperty := AProperty;
  FPropertyNameInCriteria := APropertyNameInCriteria;
end;

function TdxJoinContextPropertyInfo.GetHashCode: Integer;
begin
  if FProperty = nil then
    Result := $1D45A594
  else
    Result := FProperty.GetHashCode;
  if FPropertyNameInCriteria = '' then
    Result := Result xor $3F436A32
  else
    Result := Result xor GetValueHash(FPropertyNameInCriteria);
end;

function TdxJoinContextPropertyInfo.Equals(AObj: TObject): Boolean;
var
  AOther: TdxJoinContextPropertyInfo;
begin
  AOther := Safe<TdxJoinContextPropertyInfo>.Cast(AObj);
  if AOther = nil then
    Exit(False);
  Result := (FProperty = AOther.FProperty) and (FPropertyNameInCriteria = AOther.FPropertyNameInCriteria);
end;

function TdxJoinContextPropertyInfo.ToString: string;
begin
  Result := Format('%s(%s)', [FProperty, FPropertyNameInCriteria]);
end;

{ TdxJoinContextValueInfoSet }

constructor TdxJoinContextValueInfoSet.Create(AProperties: TDictionary<string, TValue>);
begin
  FProperties := AProperties;
end;

{ TdxJoinContextPropertyInfoSet }

constructor TdxJoinContextPropertyInfoSet.Create(AProperties: TDictionary<TdxJoinContextPropertyInfo, Boolean>);
begin
  FProperties := AProperties;
end;

function TdxJoinContextPropertyInfoSet.GetCount: Integer;
begin
  if FProperties = nil then
    Result := 0
  else
    Result := FProperties.Count;
end;

function TdxJoinContextPropertyInfoSet.GetJoinContextValueInfoSet(AContext: TdxEvaluatorContext): TdxJoinContextValueInfoSet;
var
  AResult: TDictionary<string, TValue>;
  APropertyInfo: TdxJoinContextPropertyInfo;
begin
  AResult := TDictionary<string, TValue>.Create;
  for APropertyInfo in FProperties.Keys do
    AResult.AddOrSetValue(APropertyInfo.PropertyNameInCriteria, AContext.GetPropertyValue(APropertyInfo.&Property));
  Result := TdxJoinContextValueInfoSet.Create(AResult);
end;

{ TdxExpressionEvaluatorCoreBase.TAggregateProcessingParam }

constructor TdxExpressionEvaluatorCoreBase.TAggregateProcessingParam.Create(AEvaluator: TdxExpressionEvaluatorCoreBase);
begin
  FEvaluator := AEvaluator;
end;

{ TdxJoinEvaluationContextCache.TJoinEvaluationCacheChunk }

constructor TdxJoinEvaluationContextCache.TJoinEvaluationCacheChunk.Create(const ACriteria: IdxCriteriaOperator);
begin
  FCriteria := ACriteria;
  FIsEmpty := ACriteria <> nil;
end;

procedure TdxJoinEvaluationContextCache.TJoinEvaluationCacheChunk.Fill(AContext: TdxEvaluatorContext;
  const AQueryTypeName: string);
begin
  FObjects := AContext.GetQueryContexts(AQueryTypeName, FCriteria, 0);
  FIsEmpty := False;
end;

{ TdxJoinEvaluationContextCache.TJoinEvaluationCacheInfo }

constructor TdxJoinEvaluationContextCache.TJoinEvaluationCacheInfo.Create(AChunks: TList<TJoinEvaluationCacheChunk>;
  AAllChunksObjects: TDictionary<TObject, Integer>);
begin
  FChunks := AChunks;
  FAllChunksObjects := AAllChunksObjects;
end;

{ TdxJoinEvaluationContextCache.TJoinEvaluationContextCacheKey }

constructor TdxJoinEvaluationContextCache.TJoinEvaluationContextCacheKey.Create(const AQueryTypeName: string;
  const ACondition: IdxCriteriaOperator);
begin
  FQueryTypeName := AQueryTypeName;
  FCondition := ACondition;
end;

function TdxJoinEvaluationContextCache.TJoinEvaluationContextCacheKey.Equals(AObj: TObject): Boolean;
var
  AOther: TJoinEvaluationContextCacheKey;
begin
  AOther := Safe<TJoinEvaluationContextCacheKey>.Cast(AObj);
  if AOther = nil then
    Exit(False);
  Result := (FQueryTypeName = AOther.FQueryTypeName) and TdxCriteriaOperator.Equals(FCondition, AOther.FCondition);
end;

function TdxJoinEvaluationContextCache.TJoinEvaluationContextCacheKey.GetHashCode: Integer;
begin
  if FQueryTypeName = '' then
    Result := $23416643
  else
    Result := GetValueHash(FQueryTypeName);
  if FCondition = nil then
    Result := Result xor $73423562
  else
    Result := Result xor (FCondition as TdxCriteriaOperator).GetHashCode;
end;

{ TdxJoinEvaluationContextCache.TCriteriaOperatorKey }

constructor TdxJoinEvaluationContextCache.TCriteriaOperatorKey.Create(const ACondition: IdxCriteriaOperator);
begin
  FCondition := ACondition;
end;

function TdxJoinEvaluationContextCache.TCriteriaOperatorKey.Equals(AObj: TObject): Boolean;
var
  AOther: TCriteriaOperatorKey;
begin
  AOther := Safe<TCriteriaOperatorKey>.Cast(AObj);
  if AOther = nil then
    Exit(False);
  Result := TdxCriteriaOperator.Equals(FCondition, AOther.FCondition);
end;

function TdxJoinEvaluationContextCache.TCriteriaOperatorKey.GetHashCode: Integer;
begin
  if FCondition = nil then
    Result := $73423562
  else
    Result := (FCondition as TdxCriteriaOperator).GetHashCode;
end;

{ TdxJoinEvaluationContextCache }

constructor TdxJoinEvaluationContextCache.Create;
begin
  FCollectionContextStack := TStack<TEnumerable<TdxEvaluatorContext>>.Create;
  FCacheStack := TStack<TDictionary<TJoinEvaluationContextCacheKey, TJoinEvaluationCacheInfo>>.Create;
  FCacheDictionary := TDictionary<TJoinEvaluationContextCacheKey, TJoinEvaluationCacheInfo>.Create;
end;

procedure TdxJoinEvaluationContextCache.PushCollectionContext(AContext: TEnumerable<TdxEvaluatorContext>);
begin
  FCollectionContextStack.Push(AContext);
  FCacheStack.Push(FCacheDictionary);
  FCacheDictionary := TDictionary<TJoinEvaluationContextCacheKey, TJoinEvaluationCacheInfo>.Create;
end;

function TdxJoinEvaluationContextCache.PopCollectionContext: TEnumerable<TdxEvaluatorContext>;
begin
  FCacheDictionary.Clear;
  FCacheDictionary := FCacheStack.Pop;
  Result := FCollectionContextStack.Pop;
end;

function TdxJoinEvaluationContextCache.GetQueryContexts(const AContexts: TArray<TdxEvaluatorContext>;
  const AQueryTypeName: string; const ACondition: IdxCriteriaOperator; ATop: Integer; out AFiltered: Boolean): TEnumerable<TdxEvaluatorContext>;
var
  AZeroLevelProperties: TdxJoinContextPropertyInfoSet;
  ACacheCondition, ACurrentCondition: IdxCriteriaOperator;
  ACacheInfo: TJoinEvaluationCacheInfo;
  ACacheKey: TJoinEvaluationContextCacheKey;
  AFullCollectionContext: TEnumerable<TdxEvaluatorContext>;
  AChunks: TList<TJoinEvaluationCacheChunk>;
  AChunkObjects: TDictionary<TObject, Integer>;
  AChunkGroup: TDictionary<TCriteriaOperatorKey, Boolean>;
  AContext: TdxEvaluatorContext;
  ACriteriaKey: TCriteriaOperatorKey;
  AFoundChunkIndex: Integer;
  AFoundChunk: TJoinEvaluationCacheChunk;
begin
  if (FCollectionContextStack.Count = 0) or (ATop <> 0) then
  begin
    AFiltered := True;
    Exit(AContexts[1].GetQueryContexts(AQueryTypeName, TdxJoinContextCriteriaCreator.Process(AContexts, ACondition), ATop));
  end;
  ACacheCondition := TdxJoinContextCriteriaCreator.ProcessZeroLevelLeave(AContexts, ACondition, AZeroLevelProperties);
  ACacheKey := TJoinEvaluationContextCacheKey.Create(AQueryTypeName, ACacheCondition);
  if not FCacheDictionary.TryGetValue(ACacheKey, ACacheInfo) then
  begin
    AFullCollectionContext := FCollectionContextStack.Peek;
    AChunks := TList<TJoinEvaluationCacheChunk>.Create;
    AChunkObjects := TDictionary<TObject, Integer>.Create;
    AChunkGroup := TDictionary<TCriteriaOperatorKey, Boolean>.Create;
    for AContext in AFullCollectionContext do
    begin
      if AContext.Source = nil then
        Continue;
      ACurrentCondition := TdxJoinContextCriteriaPatcher.Process(AZeroLevelProperties.GetJoinContextValueInfoSet(AContext), ACacheCondition);
      AChunkObjects.Add(AContext.Source, AChunks.Count);
      AChunkGroup[TCriteriaOperatorKey.Create(ACurrentCondition)] := True;
      if AChunkGroup.Count > 25 then
      begin
        AChunks.Add(TJoinEvaluationCacheChunk.Create(
          TdxGroupOperator.Create(TdxGroupOperatorType.Or, TdxArray<TCriteriaOperatorKey>.Select<IdxCriteriaOperator>(AChunkGroup.Keys.ToArray,
            function (AValue: TCriteriaOperatorKey): IdxCriteriaOperator
            begin
              Result := AValue.Condition;
            end))));
        AChunkGroup.Clear;
      end;
    end;
    if (AChunkGroup <> nil) and (AChunkGroup.Count > 0) then
    begin
      case AChunkGroup.Count of
        0:
          AChunks.Add(TJoinEvaluationCacheChunk.Create(nil));
        1:
          for ACriteriaKey in AChunkGroup.Keys do
          begin
            AChunks.Add(TJoinEvaluationCacheChunk.Create(ACriteriaKey.Condition));
            Break;
          end;
        else
          AChunks.Add(TJoinEvaluationCacheChunk.Create(TdxGroupOperator.Create(TdxGroupOperatorType.Or,TdxArray<TCriteriaOperatorKey>.Select<IdxCriteriaOperator>(AChunkGroup.Keys.ToArray,
            function (AValue: TCriteriaOperatorKey): IdxCriteriaOperator
            begin
              Result := AValue.Condition;
            end))));
      end;
      AChunkGroup.Clear;
    end;
    ACacheInfo := TJoinEvaluationCacheInfo.Create(AChunks, AChunkObjects);
    FCacheDictionary.Add(ACacheKey, ACacheInfo);
  end;
  AFiltered := False;
  if not ACacheInfo.AllChunksObjects.TryGetValue(AContexts[1].Source, AFoundChunkIndex) then
    raise EInvalidOperation.Create('');
  AFoundChunk := ACacheInfo.Chunks[AFoundChunkIndex];
  if AFoundChunk.IsEmpty then
    AFoundChunk.Fill(AContexts[1], AQueryTypeName);
  if AFoundChunk.Criteria = nil then
  begin
    AFiltered := True;
    Exit(AContexts[1].GetQueryContexts(AQueryTypeName, TdxJoinContextCriteriaCreator.Process(AContexts, ACondition), ATop));
  end;
  Result := AFoundChunk.Objects;
end;


{ TdxExpressionEvaluatorCoreBase }

class constructor TdxExpressionEvaluatorCoreBase.Initialize;
begin
  FFnFunctions := TDictionary<TdxFnFuncKey, TFunc<TArray<TObject>, TObject>>.Create;
end;

class destructor TdxExpressionEvaluatorCoreBase.Finalize;
begin
  FreeAndNil(FFnFunctions);
end;

constructor TdxExpressionEvaluatorCoreBase.Create(ACaseSensitive: Boolean; AEvaluateCustomFunction: TdxEvaluateCustomFunctionHandler);
begin
  FCaseSensitive := ACaseSensitive;
end;

constructor TdxExpressionEvaluatorCoreBase.Create(ACaseSensitive: Boolean);
begin
  Create(ACaseSensitive, nil);
end;

function TdxExpressionEvaluatorCoreBase.FixValue(AValue: TObject): TObject;
begin
  NotImplemented;
  Result := AValue;
end;

function TdxExpressionEvaluatorCoreBase.Process(const AOperand: IdxCriteriaOperator): TValue;
begin
  if AOperand = nil then
    Exit(nil);
  NotImplemented;
end;

function TdxExpressionEvaluatorCoreBase.Visit(const AOperator: IdxBetweenOperator): TValue;
var
  AVal: TValue;
begin
  AVal := Process(AOperator.TestExpression as TdxCriteriaOperator);
  if Compare(AVal, Process(AOperator.BeginExpression as TdxCriteriaOperator)) < 0 then
    Exit(False);
  if Compare(AVal, Process(AOperator.EndExpression as TdxCriteriaOperator)) > 0 then
    Exit(False);
  Result := True;
end;

function TdxExpressionEvaluatorCoreBase.Compare(const ALeftValue, ARightValue: TValue; AIsEqualityCompare: Boolean): Integer;
begin
  NotImplemented;
  Result := 0;
end;

function TdxExpressionEvaluatorCoreBase.Compare(const ALeftValue, ARightValue: TValue): Integer;
begin
  Result := Compare(ALeftValue, ARightValue, False);
end;

class function TdxExpressionEvaluatorCoreBase.GetBool(const AValue: TdxNullableValue<Boolean>): TValue;
begin
  if AValue = True then
    Result := FTrueValue
  else
    Result := FFalseValue;
end;

function TdxExpressionEvaluatorCoreBase.Visit(const AOperator: IdxBinaryOperator): TValue;
var
  ALeftValue, ARightValue: TValue;
begin


  ALeftValue := Process(AOperator.LeftOperand as TdxCriteriaOperator);
  ARightValue := Process(AOperator.RightOperand as TdxCriteriaOperator);
  case AOperator.OperatorType of
    TdxBinaryOperatorType.Equal:
      Exit(Compare(ALeftValue, ARightValue, True) = 0);
    TdxBinaryOperatorType.NotEqual:
      Exit(Compare(ALeftValue, ARightValue, True) <> 0);
    TdxBinaryOperatorType.Less:
      Exit(Compare(ALeftValue, ARightValue) < 0);
    TdxBinaryOperatorType.LessOrEqual:
      Exit(Compare(ALeftValue, ARightValue) <= 0);
    TdxBinaryOperatorType.Greater:
      Exit(Compare(ALeftValue, ARightValue) > 0);
    TdxBinaryOperatorType.GreaterOrEqual:
      Exit(Compare(ALeftValue, ARightValue) >= 0);
    else
      NotImplemented;
  end;
end;

function TdxExpressionEvaluatorCoreBase.Visit(const AOperator: IdxInOperator): TValue;
var
  AVal: TValue;
  AOp: IdxCriteriaOperator;
begin
  AVal := Process(AOperator.LeftOperand);
  for AOp in AOperator.Operands do
    if Compare(AVal, Process(AOp as TdxCriteriaOperator), True) = 0 then
      Exit(True);
  Result := False;
end;

function TdxExpressionEvaluatorCoreBase.Visit(const AOperator: IdxGroupOperator): TValue;
var
  ACount, I: Integer;
  AShortCircuitValue, ANullsDetected: Boolean;
  AProcessed: TdxNullableValue<Boolean>;
begin
  ACount := AOperator.Operands.Count;
  if ACount = 0 then
    Exit(nil);
  AShortCircuitValue := AOperator.OperatorType = TdxGroupOperatorType.Or;
  ANullsDetected := False;
  for I := 0 to ACount - 1 do
  begin
    AProcessed := GetBool(Process(TdxCriteriaOperator(AOperator.Operands[I])));

    if not AProcessed.HasValue then
      ANullsDetected := True
    else
      if AProcessed.Value = AShortCircuitValue then
        Exit(AShortCircuitValue);
  end;
  if ANullsDetected then
    Exit(nil);
  Result := not AShortCircuitValue;
end;

function TdxExpressionEvaluatorCoreBase.Visit(const AOperator: IdxFunctionOperator): TValue;
begin
  NotImplemented;
end;

function TdxExpressionEvaluatorCoreBase.FnIsSameDay(AOperator: TdxFunctionOperator): TValue;
var
  AOperands: IdxCriteriaOperatorCollection;
  AOpValue, AValue: TValue;
  AOpDate: TDateTime;
  I: Integer;
begin
  if AOperator.Operands.Count < 2 then
    raise EArgumentException.Create('');
  AOperands := AOperator.Operands;
  AOpValue := Process(AOperands[0]);
  if AOpValue.IsEmpty then
    Exit(False);
  AOpDate := AOpValue.AsDateTime;
  for I := 1 to AOperands.Count - 1 do
  begin
    AValue := Process(AOperands[I]);
    if AValue.IsEmpty then
      Continue;
    if AOpDate = AValue.AsDateTime then
      Exit(True);
  end;
  Result := False;
end;

class function TdxExpressionEvaluatorCoreBase.GetBool(const AValue: TValue): TdxNullableValue<Boolean>;
begin
  if AValue.IsEmpty or not AValue.IsBoolean then
    Result.Reset
  else
    Result := AValue.AsBoolean;
end;

class function TdxExpressionEvaluatorCoreBase.GetValueAsBool(const AValue: TValue): Boolean;
begin
  if AValue.IsEmpty or not AValue.IsBoolean then
    Result := False
  else
    Result := AValue.AsBoolean;
end;

function TdxExpressionEvaluatorCoreBase.FnIif(AOperator: TdxFunctionOperator): TValue;
var
  AIndex: Integer;
  AIifDiscriminator: TdxNullableValue<Boolean>;
begin
  if (AOperator.Operands.Count < 3) or (((AOperator.Operands.Count mod 2) = 0)) then
    raise EArgumentException.Create('');
  AIndex := -2;
  repeat
    Inc(AIndex, 2);
    AIifDiscriminator := GetBool(Process(TdxCriteriaOperator(AOperator.Operands[AIndex])));
    if AIifDiscriminator = False then
      Exit(Process(TdxCriteriaOperator(AOperator.Operands[AIndex + 1])));
  until not (AOperator.Operands.Count > AIndex + 3);;
  Result := Process(TdxCriteriaOperator(AOperator.Operands[AIndex + 2]));
end;


function TdxExpressionEvaluatorCoreBase.FnIsNull(AOperator: TdxFunctionOperator): TValue;
var
  AOp: IdxCriteriaOperator;
  AObj: TValue;
begin
  if AOperator.Operands.Count = 1 then
    Exit(Process(TdxCriteriaOperator(AOperator.Operands[0])).IsEmpty)
  else
  begin
    for AOp in AOperator.Operands do
    begin
      AObj := Process(AOp as TdxCriteriaOperator);
      if not AObj.IsEmpty then
        Exit(AObj);
    end;
    Exit(nil);
  end;
end;

class function TdxExpressionEvaluatorCoreBase.EvaluateLambdaFunction(AFnType: TdxFunctionOperatorType; ACaseSensitive: Boolean; AArgs: TArray<TObject>): TValue;
begin
  NotImplemented;
end;

function TdxExpressionEvaluatorCoreBase.Visit(const AOperand: IdxOperandValue): TValue;
begin
  Result := AOperand.Value;
end;

function TdxExpressionEvaluatorCoreBase.UnaryNumericPromotions(const AOperand: TValue): TValue;
begin
  NotImplemented;
end;

function TdxExpressionEvaluatorCoreBase.Visit(const AOperator: IdxUnaryOperator): TValue;
var
  AOperand, AConverted: TValue;
  ABoolOperand: TdxNullableValue<Boolean>;
begin
  AOperand := Process(AOperator.Operand as TdxCriteriaOperator);
  case AOperator.OperatorType of
    TdxUnaryOperatorType.IsNull:
      Exit(AOperand.IsEmpty);
    TdxUnaryOperatorType.Not:
      begin
        ABoolOperand := GetBool(AOperand);
        if not ABoolOperand.HasValue then
          Exit(nil);
        Exit(not ABoolOperand.Value);
      end;
    TdxUnaryOperatorType.Plus:
      begin
        if AOperand.IsEmpty then
          Exit(nil);
        AConverted := UnaryNumericPromotions(AOperand);
        NotImplemented;
      end;
    TdxUnaryOperatorType.Minus:
      begin
        if AOperand.IsEmpty then
          Exit(nil);
        NotImplemented;
      end;
    TdxUnaryOperatorType.BitwiseNot:
      begin
        if AOperand.IsEmpty then
          Exit(nil);
        AConverted := UnaryNumericPromotions(AOperand);
        NotImplemented;
      end;
    else
      raise ENotImplemented.Create('ExpressionEvaluatorOperatorSubtypeNotImplemented');
  end;
end;

procedure TdxExpressionEvaluatorCoreBase.DoAggregate(AParam: TAggregateProcessingParam; const AContextsCollection: TEnumerable<TdxEvaluatorContext>;
  const AFilterExpression: IdxCriteriaOperator; const AExpression: IdxCriteriaOperator);
var
  ASubContext: TdxEvaluatorContext;
  ACandidate: TValue;
begin
  if AContextsCollection <> nil then
  begin
    PushCollectionContext(AContextsCollection);
    try
      for ASubContext in AContextsCollection do
      begin
        SetContext(ASubContext);
        if Fit(AFilterExpression) then
        begin
          ACandidate := nil;
          if AExpression <> nil then
          begin
            ACandidate := Process(AExpression);
            if ACandidate.IsEmpty then
              Continue;
          end;
          if AParam.Process(ACandidate) then
            Exit;
        end;
      end;
    finally
      PopCollectionContext;
    end;
  end;
end;

function TdxExpressionEvaluatorCoreBase.DoAggregate(AAggregateFunctionType: TdxAggregateFunctionType;
  const AContextsCollection: TEnumerable<TdxEvaluatorContext>; const AFilterExpression: IdxCriteriaOperator;
  const AExpression: IdxCriteriaOperator): TValue;
var
  AParam: TAggregateProcessingParam;
begin
  case AAggregateFunctionType of
    TdxAggregateFunctionType.Exists:
      AParam := TExistsProcessingParam.Create(Self);
    TdxAggregateFunctionType.Count:
      AParam := TCountProcessingParam.Create(Self);
    TdxAggregateFunctionType.Avg:
      AParam := TAvgProcessingParam.Create(Self);
    TdxAggregateFunctionType.Max:
      AParam := TMaxProcessingParam.Create(Self);
    TdxAggregateFunctionType.Min:
      AParam := TMinProcessingParam.Create(Self);
    TdxAggregateFunctionType.Sum:
      AParam := TSumProcessingParam.Create(Self);
    TdxAggregateFunctionType.Single:
      AParam := TSingleProcessingParam.Create(Self);
    else
      raise ENotImplemented.Create(Format('ExpressionEvaluatorOperatorSubtypeNotImplemented', []{, TypeOf(AggregateOperand).Name, AAggregateFunctionType.ToString)}));
  end;
  try
    DoAggregate(AParam, AContextsCollection, AFilterExpression, AExpression);
    Result := AParam.GetResult;
  finally
    AParam.Free;
  end;
end;

function TdxExpressionEvaluatorCoreBase.Evaluate(AEvaluationContext: TdxEvaluatorContext;
  const AEvaluatorCriteria: IdxCriteriaOperator): TValue;
begin
  Result := Evaluate(AEvaluationContext, AEvaluatorCriteria, nil);
end;

function TdxExpressionEvaluatorCoreBase.Evaluate(AEvaluationContext: TdxEvaluatorContext;
  const AEvaluatorCriteria: IdxCriteriaOperator; const ACustomComparer: IdxComparer): TValue;
begin
  Assert(not HasContext);
  try
    SetContext(AEvaluationContext);
    Result := Process(AEvaluatorCriteria as TdxCriteriaOperator);
  finally
    ClearContext;
  end;
end;

function TdxExpressionEvaluatorCoreBase.EvaluateOnObjects(AEvaluatorContextCollection: TEnumerable<TdxEvaluatorContext>; AFilterCriteria: TdxCriteriaOperator; const ACustomComparer: IdxComparer = nil): TArray<TValue>;
var
  AResult: TList<TValue>;
  AEvaluatorContext: TdxEvaluatorContext;
begin
  PushCollectionContext(AEvaluatorContextCollection);
  try
    AResult := TList<TValue>.Create;
    try
      for AEvaluatorContext in AEvaluatorContextCollection do
        AResult.Add(Evaluate(AEvaluatorContext, AFilterCriteria, ACustomComparer));
      Result := AResult.ToArray;
    finally
      AResult.Free;
    end;
  finally
    PopCollectionContext;
  end;
end;

procedure TdxExpressionEvaluatorCoreBase.PushCollectionContext(AContext: TEnumerable<TdxEvaluatorContext>);
begin
end;

function TdxExpressionEvaluatorCoreBase.PopCollectionContext: TEnumerable<TdxEvaluatorContext>;
begin
  Result := nil;
end;

function TdxExpressionEvaluatorCoreBase.Filter(const AEvaluatorContextCollection: TEnumerable<TdxEvaluatorContext>; AFilterCriteria: TdxCriteriaOperator): TEnumerable<TdxEvaluatorContext>;
var
  AResult: TList<TdxEvaluatorContext>;
  AEvaluatorContext: TdxEvaluatorContext;
begin
  PushCollectionContext(AEvaluatorContextCollection);
  try
    AResult := TList<TdxEvaluatorContext>.Create;
    for AEvaluatorContext in AEvaluatorContextCollection do
    begin
      if Fit(AEvaluatorContext, AFilterCriteria) then
      begin
        AResult.Add(AEvaluatorContext);
      end;
    end;
    Exit(AResult);
  finally
    PopCollectionContext;
  end;
end;

function TdxExpressionEvaluatorCoreBase.Fit(AEvaluationContext: TdxEvaluatorContext; const AFilterCriteria: IdxCriteriaOperator): Boolean;
begin
  if AFilterCriteria = nil then
    Exit(True);
  Result := GetValueAsBool(Evaluate(AEvaluationContext, AFilterCriteria));
end;

function TdxExpressionEvaluatorCoreBase.Fit(const AFilterCriteria: IdxCriteriaOperator): Boolean;
begin
  if AFilterCriteria = nil then
    Result := True
  else
    Result := GetValueAsBool(Process(AFilterCriteria as TdxCriteriaOperator));
end;

{ TdxEvaluatorPropertyCache }

constructor TdxEvaluatorPropertyCache.Create;
begin
  FStore := TDictionary<TdxOperandProperty, TdxEvaluatorProperty>.Create;
end;

destructor TdxEvaluatorPropertyCache.Destroy;
begin
  FreeAndNil(FStore);
  inherited Destroy;
end;

function TdxEvaluatorPropertyCache.GetStore(AIndex: TdxOperandProperty): TdxEvaluatorProperty;
begin
  if not FStore.TryGetValue(AIndex, Result) then
  begin
    Result := TdxEvaluatorProperty.Create(AIndex);
    FStore.Add(AIndex, Result);
  end;
end;

{ TdxExpressionEvaluatorCore }

constructor TdxExpressionEvaluatorCore.Create(ACaseSensitive: Boolean;
  AEvaluateCustomFunction: TdxEvaluateCustomFunctionHandler = nil);
begin
  inherited Create(ACaseSensitive, AEvaluateCustomFunction);
  FJoinCache := TdxJoinEvaluationContextCache.Create;
  FPropertyCache := TdxEvaluatorPropertyCache.Create;
  FContexts := nil;
end;

destructor TdxExpressionEvaluatorCore.Destroy;
begin
  FreeAndNil(FJoinCache);
  FreeAndNil(FPropertyCache);
  inherited Destroy;
end;

procedure TdxExpressionEvaluatorCore.SetContext(AContext: TdxEvaluatorContext);
begin
  if FContexts = nil then
    FContexts := TArray<TdxEvaluatorContext>.Create(AContext)
  else
    FContexts[0] := AContext;
end;

procedure TdxExpressionEvaluatorCore.ClearContext;
begin
  FContexts := nil;
end;

function TdxExpressionEvaluatorCore.GetHasContext: Boolean;
begin
  Result := FContexts <> nil;
end;

function TdxExpressionEvaluatorCore.GetContext: TdxEvaluatorContext;
begin
  Result := FContexts[0];
end;

function TdxExpressionEvaluatorCore.GetContext(AUpDepth: Integer): TdxEvaluatorContext;
begin
  Result := FContexts[AUpDepth];
end;

procedure TdxExpressionEvaluatorCore.PushCollectionContext(AContext: TEnumerable<TdxEvaluatorContext>);
begin
  FJoinCache.PushCollectionContext(AContext);
end;

function TdxExpressionEvaluatorCore.PopCollectionContext: TEnumerable<TdxEvaluatorContext>;
begin
  Result := FJoinCache.PopCollectionContext;
end;

function TdxExpressionEvaluatorCore.CreateNestedJoinContext(const AJoinTypeName: string; ACondition: TdxCriteriaOperator;
  ATop: Integer; out AFiltered: Boolean): TEnumerable<TdxEvaluatorContext>;
var
  AParentContext: TArray<TdxEvaluatorContext>;
begin
  AParentContext := FContexts;
  SetLength(FContexts, Length(AParentContext) + 1);
  TArray.Copy<TdxEvaluatorContext>(AParentContext, FContexts, 0, 1, Length(AParentContext));
  Result := FJoinCache.GetQueryContexts(FContexts, AJoinTypeName, ACondition, ATop, AFiltered);
end;

function TdxExpressionEvaluatorCore.CreateNestedContext(ACollectionProperty: TdxEvaluatorProperty): TEnumerable<TdxEvaluatorContext>;
begin
  NotImplemented;
  Result := nil;
end;

function TdxExpressionEvaluatorCore.Visit(const AOperand: IdxAggregateOperand): TValue;
begin
  NotImplemented;
  Result := TValue.Empty;
end;

function TdxExpressionEvaluatorCore.Visit(const AOperand: IdxJoinOperand): TValue;
begin
  NotImplemented;
  Result := TValue.Empty;
end;

function TdxExpressionEvaluatorCore.Visit(const AOperand: IdxOperandProperty): TValue;
begin
  NotImplemented
end;

{ TdxExpressionEvaluator }

constructor TdxExpressionEvaluator.Create(ADescriptor: TdxEvaluatorContextDescriptor; const ACriteria: IdxCriteriaOperator; ACaseSensitive: Boolean; ADoCreateEvaluatorCore: Boolean);
begin
  inherited Create;
  FDefaultDescriptor := ADescriptor;
  FEvaluatorCriteria := ACriteria;
  FThrowExceptionIfNotFoundCustomFunction := True;
end;


constructor TdxExpressionEvaluator.Create(AProperties: TdxPropertyDescriptorCollection; const ACriteria: string);
begin
  Create(AProperties, ACriteria, True);
end;

constructor TdxExpressionEvaluator.Create(AProperties: TdxPropertyDescriptorCollection; const ACriteria: string; ACaseSensitive: Boolean);
begin
  Create(AProperties, TdxCriteriaOperator.Parse(ACriteria), ACaseSensitive);
end;

constructor TdxExpressionEvaluator.Create(AProperties: TdxPropertyDescriptorCollection; const ACriteria: IdxCriteriaOperator);
begin
  Create(AProperties, ACriteria, True);
end;

constructor TdxExpressionEvaluator.Create(AProperties: TdxPropertyDescriptorCollection; const ACriteria: IdxCriteriaOperator; ACaseSensitive: Boolean);
begin
  Create(AProperties, ACriteria, ACaseSensitive, True);
end;

constructor TdxExpressionEvaluator.Create(AProperties: TdxPropertyDescriptorCollection; const ACriteria: IdxCriteriaOperator; ACaseSensitive: Boolean; ADoCreateEvaluatorCore: Boolean);
begin
  NotImplemented;
end;


constructor TdxExpressionEvaluator.Create(ADescriptor: TdxEvaluatorContextDescriptor; const ACriteria: IdxCriteriaOperator);
begin
  Create(ADescriptor, ACriteria, True);
end;

constructor TdxExpressionEvaluator.Create(ADescriptor: TdxEvaluatorContextDescriptor; const ACriteria: IdxCriteriaOperator; ACaseSensitive: Boolean);
begin
  Create(ADescriptor, ACriteria, ACaseSensitive, True);
end;

function TdxExpressionEvaluator.GetEvaluatorCore: TdxExpressionEvaluatorCoreBase;
begin
  Result := FEvaluatorCore;
end;

procedure TdxExpressionEvaluator.SetDataAccess(const AValue: IdxEvaluatorDataAccess);
begin
  NotImplemented;
end;

function TdxExpressionEvaluator.PrepareContext(AValuesSource: TObject): TdxEvaluatorContext;
begin
  Result := NotImplemented
end;

function TdxExpressionEvaluator.Evaluate(AObject: TObject): TObject;
begin
  Result := Evaluate(AObject, nil);
end;

function TdxExpressionEvaluator.Evaluate(AObject: TObject; const ACustomComparer: IdxComparer): TObject;
begin
  Result := NotImplemented;
end;

function TdxExpressionEvaluator.EvaluateOnObjects(const AObjects: IEnumerable): TArray<TObject>;
begin
  Result := EvaluateOnObjects(AObjects, nil);
end;

function TdxExpressionEvaluator.EvaluateOnObjects(const AObjects: IEnumerable; const ACustomComparer: IdxComparer): TArray<TObject>;
var
  AContextList: TList<TdxEvaluatorContext>;
  AObject: TObject;
begin
  AContextList := TList<TdxEvaluatorContext>.Create;
  for AObject in AObjects do
    AContextList.Add(PrepareContext(AObject));
  NotImplemented;
end;

function TdxExpressionEvaluator.Fit(AObject: TObject): Boolean;
begin
  Result := EvaluatorCore.Fit(PrepareContext(AObject), FEvaluatorCriteria);
end;

function TdxExpressionEvaluator.Filter(const AObjects: IEnumerable): IEnumerable;
begin
  NotImplemented
end;


function TdxExpressionEvaluator.EvaluateCustomFunction(const AFunctionName: string; const AOperands: TArray<TValue>): TValue;
begin
end;

{ TdxExpressionEvaluatorCoreBase.TExistsProcessingParam }

function TdxExpressionEvaluatorCoreBase.TExistsProcessingParam.GetResult: TValue;
begin
  Result := FResult;
end;

function TdxExpressionEvaluatorCoreBase.TExistsProcessingParam.Process(const AOperand: TValue): Boolean;
begin
  FResult := True;
  Result := True;
end;

{ TdxExpressionEvaluatorCoreBase.TSingleProcessingParam }

function TdxExpressionEvaluatorCoreBase.TSingleProcessingParam.GetResult: TValue;
begin
  Result := FResult;
end;

function TdxExpressionEvaluatorCoreBase.TSingleProcessingParam.Process(const AOperand: TValue): Boolean;
begin
  if FProcessed then
    raise EInvalidOperation.Create('The collection to which the Single aggregate is applied must be empty or contain exactly one item');
  FResult := AOperand;
  FProcessed := True;
  Result := False;
end;

{ TdxExpressionEvaluatorCoreBase.TCountProcessingParam }

function TdxExpressionEvaluatorCoreBase.TCountProcessingParam.GetResult: TValue;
begin
  Result := FResult;
end;

function TdxExpressionEvaluatorCoreBase.TCountProcessingParam.Process(const AOperand: TValue): Boolean;
begin
  Inc(FResult);
  Result := False;
end;

{ TdxExpressionEvaluatorCoreBase.TMinProcessingParam }

function TdxExpressionEvaluatorCoreBase.TMinProcessingParam.GetResult: TValue;
begin
  Result := FResult;
end;

function TdxExpressionEvaluatorCoreBase.TMinProcessingParam.Process(const AOperand: TValue): Boolean;
begin
  if AOperand.IsEmpty then
    if FResult.IsEmpty or (Evaluator.Compare(AOperand, FResult) < 0) then
      FResult := AOperand;
  Result := False;
end;

{ TdxExpressionEvaluatorCoreBase.TMaxProcessingParam }

function TdxExpressionEvaluatorCoreBase.TMaxProcessingParam.GetResult: TValue;
begin
  Result := FResult;
end;

function TdxExpressionEvaluatorCoreBase.TMaxProcessingParam.Process(const AOperand: TValue): Boolean;
begin
  if AOperand.IsEmpty then
  begin
    if FResult.IsEmpty or (Evaluator.Compare(AOperand, FResult) > 0) then
    begin
      FResult := AOperand;
    end;
  end;
  Result := False;
end;

{ TdxExpressionEvaluatorCoreBase.TSumProcessingParam }

function TdxExpressionEvaluatorCoreBase.TSumProcessingParam.GetResult: TValue;
begin
  Result := FResult;
end;

function TdxExpressionEvaluatorCoreBase.TSumProcessingParam.Process(const AOperand: TValue): Boolean;
begin
  if AOperand.IsEmpty then
  begin
    if FResult.IsEmpty then
      FResult := AOperand
    else
      FResult := Evaluator.Process(
        TdxBinaryOperator.Create(TdxOperandValue.Create(FResult), TdxOperandValue.Create(AOperand), TdxBinaryOperatorType.Plus));
  end;
  Result := False;
end;

{ TdxExpressionEvaluatorCoreBase.TAvgProcessingParam }

function TdxExpressionEvaluatorCoreBase.TAvgProcessingParam.GetResult: TValue;
begin
  NotImplemented
end;

function TdxExpressionEvaluatorCoreBase.TAvgProcessingParam.Process(const AOperand: TValue): Boolean;
begin
  NotImplemented;
  Result := False;
end;

{ TdxEvalHelpers }

class function TdxEvalHelpers.MakeTypicalOutlookInterval(const AOp: IdxCriteriaOperator; ALowerBound, AUpperBound: TdxFunctionOperatorType): IdxCriteriaOperator;
begin
  Result := TdxGroupOperator.Create(TdxGroupOperatorType.&And, [
      TdxBinaryOperator.Create(AOp, TdxFunctionOperator.Create(ALowerBound, []), TdxBinaryOperatorType.GreaterOrEqual),
      TdxBinaryOperator.Create(AOp, TdxFunctionOperator.Create(AUpperBound, []), TdxBinaryOperatorType.Less)
    ]);
end;

class function TdxEvalHelpers.ExpandIsOutlookInterval(const AOperator: IdxFunctionOperator): IdxCriteriaOperator;
var
  AOp: IdxCriteriaOperator;
begin
  NotImplemented;
  if AOperator.OperatorType = TdxFunctionOperatorType.IsSameDay then
    Exit(MakeIsSameDayCriteria(AOperator));
  if AOperator.Operands.Count <> 1 then
    raise EArgumentException.Create(sdxEvaluatorOperandsCountNotEqualOne);
  AOp := AOperator.Operands[0];
  case AOperator.OperatorType of
    TdxFunctionOperatorType.IsThisMonth:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeThisMonth, TdxFunctionOperatorType.LocalDateTimeNextMonth);
    TdxFunctionOperatorType.IsThisWeek:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeThisWeek, TdxFunctionOperatorType.LocalDateTimeNextWeek);
    TdxFunctionOperatorType.IsThisYear:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeThisYear, TdxFunctionOperatorType.LocalDateTimeNextYear);
    TdxFunctionOperatorType.IsNextMonth:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeNextMonth, TdxFunctionOperatorType.LocalDateTimeTwoMonthsAway);
    TdxFunctionOperatorType.IsNextYear:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeNextYear, TdxFunctionOperatorType.LocalDateTimeTwoYearsAway);
    TdxFunctionOperatorType.IsLastMonth:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeLastMonth, TdxFunctionOperatorType.LocalDateTimeThisMonth);
    TdxFunctionOperatorType.IsLastYear:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeLastYear, TdxFunctionOperatorType.LocalDateTimeThisYear);
    TdxFunctionOperatorType.IsYearToDate:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeThisYear, TdxFunctionOperatorType.LocalDateTimeToday);
    TdxFunctionOperatorType.IsOutlookIntervalBeyondThisYear:
      Result := TdxBinaryOperator.Create(AOp, TdxFunctionOperator.Create(TdxFunctionOperatorType.LocalDateTimeNextYear, []), TdxBinaryOperatorType.GreaterOrEqual);
    TdxFunctionOperatorType.IsOutlookIntervalLaterThisYear:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeNextMonth, TdxFunctionOperatorType.LocalDateTimeNextYear);
    TdxFunctionOperatorType.IsOutlookIntervalLaterThisMonth:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeTwoWeeksAway, TdxFunctionOperatorType.LocalDateTimeNextMonth);
    TdxFunctionOperatorType.IsOutlookIntervalNextWeek:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeNextWeek, TdxFunctionOperatorType.LocalDateTimeTwoWeeksAway);
    TdxFunctionOperatorType.IsOutlookIntervalLaterThisWeek:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeDayAfterTomorrow, TdxFunctionOperatorType.LocalDateTimeNextWeek);
    TdxFunctionOperatorType.IsOutlookIntervalTomorrow:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeTomorrow, TdxFunctionOperatorType.LocalDateTimeDayAfterTomorrow);
    TdxFunctionOperatorType.IsOutlookIntervalToday:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeToday, TdxFunctionOperatorType.LocalDateTimeTomorrow);
    TdxFunctionOperatorType.IsOutlookIntervalYesterday:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeYesterday, TdxFunctionOperatorType.LocalDateTimeToday);
    TdxFunctionOperatorType.IsOutlookIntervalEarlierThisWeek:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeThisWeek, TdxFunctionOperatorType.LocalDateTimeYesterday);
    TdxFunctionOperatorType.IsOutlookIntervalLastWeek:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeLastWeek, TdxFunctionOperatorType.LocalDateTimeThisWeek);
    TdxFunctionOperatorType.IsOutlookIntervalEarlierThisMonth:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeThisMonth, TdxFunctionOperatorType.LocalDateTimeLastWeek);
    TdxFunctionOperatorType.IsOutlookIntervalEarlierThisYear:
      Result := MakeTypicalOutlookInterval(AOp, TdxFunctionOperatorType.LocalDateTimeThisYear, TdxFunctionOperatorType.LocalDateTimeThisMonth);
    TdxFunctionOperatorType.IsOutlookIntervalPriorThisYear:
      Result := TdxBinaryOperator.Create(AOp, TdxFunctionOperator.Create(TdxFunctionOperatorType.LocalDateTimeThisYear, []), TdxBinaryOperatorType.Less);
    TdxFunctionOperatorType.IsJanuary:
      Result := MakeMonthCriteria(AOp, 1);
    TdxFunctionOperatorType.IsFebruary:
      Result := MakeMonthCriteria(AOp, 2);
    TdxFunctionOperatorType.IsMarch:
      Result := MakeMonthCriteria(AOp, 3);
    TdxFunctionOperatorType.IsApril:
      Result := MakeMonthCriteria(AOp, 4);
    TdxFunctionOperatorType.IsMay:
      Result := MakeMonthCriteria(AOp, 5);
    TdxFunctionOperatorType.IsJune:
      Result := MakeMonthCriteria(AOp, 6);
    TdxFunctionOperatorType.IsJuly:
      Result := MakeMonthCriteria(AOp, 7);
    TdxFunctionOperatorType.IsAugust:
      Result := MakeMonthCriteria(AOp, 8);
    TdxFunctionOperatorType.IsSeptember:
      Result := MakeMonthCriteria(AOp, 9);
    TdxFunctionOperatorType.IsOctober:
      Result := MakeMonthCriteria(AOp, 10);
    TdxFunctionOperatorType.IsNovember:
      Result := MakeMonthCriteria(AOp, 11);
    TdxFunctionOperatorType.IsDecember:
      Result := MakeMonthCriteria(AOp, 12);
    else
      raise EInvalidOperation.Create('theOperator.OperatorType is not IsOutlookInterval* or internal error');
  end;
end;

class function TdxEvalHelpers.MakeIsSameDayInterval(const AOp: IdxCriteriaOperator;
  ALowerBound, AUpperBound: TDateTime): IdxCriteriaOperator;
begin
  Result := TdxGroupOperator.Create(TdxGroupOperatorType.&And, [
    TdxBinaryOperator.Create(AOp, TdxOperandValue.Create(ALowerBound), TdxBinaryOperatorType.GreaterOrEqual),
    TdxBinaryOperator.Create(AOp, TdxOperandValue.Create(AUpperBound), TdxBinaryOperatorType.Less)
    ]);
end;

class function TdxEvalHelpers.MakeIsSameDayCriteria(const AOperator: IdxFunctionOperator): IdxCriteriaOperator;
var
  AOperands: IdxCriteriaOperatorCollection;
  AConstants: TList<TDateTime>;
  AResultOperands, ANonConstants: TdxCriteriaOperatorList;
  AOp, ARightOperator, AOperand: IdxCriteriaOperator;
  ARightConstant: TdxOperandValue;
  AOperandCount, I: Integer;
  AConstant, ARightDateTime: TDateTime;
  AIntervalStart, AIntervalEnd: TdxNullableValue<TDateTime>;
begin
  if AOperator.Operands.Count < 2 then
    raise EArgumentException.Create(sdxEvaluatorOperandsCountLessTwo);

  AOp := AOperator.Operands[0];
  AOperands := AOperator.Operands;
  AOperandCount := AOperands.Count;
  AConstants := TList<TDateTime>.Create;
  AConstants.Capacity := AOperandCount;
  ANonConstants := TdxCriteriaOperatorList.Create;
  try
    for I := 1 to AOperandCount - 1 do
    begin
      ARightOperator := AOperands[I];
      if ARightOperator = nil then
        Continue;
      ARightConstant := Safe<TdxOperandValue>.Cast(ARightOperator as TdxCriteriaOperator);
      if ARightConstant = nil then
      begin
        ANonConstants.Add(ARightOperator);
        Continue;
      end;
      ARightDateTime := ARightConstant.Value.AsDateTime;
      AConstants.Add(DateOf(ARightDateTime));
    end;
    AConstants.Sort;

    AResultOperands := TdxCriteriaOperatorList.Create;
    try
      AResultOperands.Capacity := AOperandCount;
      AIntervalStart.Reset;
      AIntervalEnd.Reset;
      for AConstant in AConstants do
      begin
        if AIntervalStart.IsNull then
        begin
          AIntervalStart := AConstant;
          AIntervalEnd := IncDay(AConstant);
          Continue;
        end;
        if AIntervalStart = AConstant then
          Continue;
        if AIntervalEnd = AConstant then
        begin
          AIntervalEnd := IncDay(AConstant);
          Continue;
        end;
        AResultOperands.Add(MakeIsSameDayInterval(AOp, AIntervalStart.Value, AIntervalEnd.Value));
        AIntervalStart := AConstant;
        AIntervalEnd := IncDay(AConstant);
      end;
      if AIntervalStart.HasValue then
        AResultOperands.Add(MakeIsSameDayInterval(AOp, AIntervalStart.Value, AIntervalEnd.Value));
      for AOperand in ANonConstants do
        AResultOperands.Add(
          TdxGroupOperator.Create(TdxGroupOperatorType.&And, [
            TdxBinaryOperator.Create(AOp, TdxFunctionOperator.Create(TdxFunctionOperatorType.DateOf, AOperand), TdxBinaryOperatorType.GreaterOrEqual),
            TdxBinaryOperator.Create(AOp, TdxFunctionOperator.Create(TdxFunctionOperatorType.IncDay,
              [TdxFunctionOperator.Create(TdxFunctionOperatorType.DateOf, AOperand), TdxConstantValue.Create(1)]),
              TdxBinaryOperatorType.Less)
          ]));
      Result := TdxGroupOperator.Or(AResultOperands);
    finally
      AResultOperands.Free;
    end;
  finally
    AConstants.Free;
    ANonConstants.Free;
  end;
end;

class function TdxEvalHelpers.MakeMonthCriteria(const AOp: IdxCriteriaOperator; AMonth: Integer): TdxBinaryOperator;
begin
  Result := TdxBinaryOperator.Create(
    TdxFunctionOperator.Create(TdxFunctionOperatorType.MonthOf, AOp),
    TdxOperandValue.Create(AMonth), TdxBinaryOperatorType.Equal);
end;

class function TdxEvalHelpers.EvaluateLocalDateTime(AType: TdxFunctionOperatorType): TDateTime;
begin
  case AType of
    TdxFunctionOperatorType.LocalDateTimeThisYear:
      Result := FnLocalDateTimeThisYear;
    TdxFunctionOperatorType.LocalDateTimeThisMonth:
      Result := FnLocalDateTimeThisMonth;
    TdxFunctionOperatorType.LocalDateTimeLastWeek:
      Result := FnLocalDateTimeLastWeek;
    TdxFunctionOperatorType.LocalDateTimeThisWeek:
      Result := FnLocalDateTimeThisWeek;
    TdxFunctionOperatorType.LocalDateTimeYesterday:
      Result := FnLocalDateTimeYesterday;
    TdxFunctionOperatorType.LocalDateTimeToday:
      Result := FnLocalDateTimeToday;
    TdxFunctionOperatorType.LocalDateTimeNow:
      Result := FnLocalDateTimeNow;
    TdxFunctionOperatorType.LocalDateTimeTomorrow:
      Result := FnLocalDateTimeTomorrow;
    TdxFunctionOperatorType.LocalDateTimeDayAfterTomorrow:
      Result := FnLocalDateTimeDayAfterTomorrow;
    TdxFunctionOperatorType.LocalDateTimeNextWeek:
      Result := FnLocalDateTimeNextWeek;
    TdxFunctionOperatorType.LocalDateTimeTwoWeeksAway:
      Result := FnLocalDateTimeTwoWeeksAway;
    TdxFunctionOperatorType.LocalDateTimeNextMonth:
      Result := FnLocalDateTimeNextMonth;
    TdxFunctionOperatorType.LocalDateTimeNextYear:
      Result := FnLocalDateTimeNextYear;
    TdxFunctionOperatorType.LocalDateTimeTwoMonthsAway:
      Result := FnLocalDateTimeTwoMonthsAway;
    TdxFunctionOperatorType.LocalDateTimeTwoYearsAway:
      Result := FnLocalDateTimeTwoYearsAway;
    TdxFunctionOperatorType.LocalDateTimeLastMonth:
      Result := FnLocalDateTimeLastMonth;
    TdxFunctionOperatorType.LocalDateTimeLastYear:
      Result := FnLocalDateTimeLastYear;
    TdxFunctionOperatorType.LocalDateTimeYearBeforeToday:
      Result := FnLocalDateTimeYearBeforeToday;
    else
      raise EInvalidOperation.Create('theOperator.OperatorType is not LocalDateTime* or internal error');
  end;
end;

class function TdxEvalHelpers.FnLocalDateTimeNextYear: TDateTime;
begin
  Result := EncodeDate(YearOf(Now) + 1, 1, 1);
end;

class function TdxEvalHelpers.FnLocalDateTimeTwoYearsAway: TDateTime;
begin
  Result := EncodeDate(YearOf(Now) + 2, 1, 1);
end;

class function TdxEvalHelpers.FnLocalDateTimeNextMonth: TDateTime;
var
  ANow: TDateTime;
begin
  ANow := Now;
  Result := IncMonth(EncodeDate(YearOf(ANow), MonthOf(ANow), 1));
end;

class function TdxEvalHelpers.FnLocalDateTimeTwoMonthsAway: TDateTime;
var
  ANow: TDateTime;
begin
  ANow := Now;
  Result := IncMonth(EncodeDate(YearOf(ANow), MonthOf(ANow), 1), 2);
end;

class function TdxEvalHelpers.FnLocalDateTimeTwoWeeksAway: TDateTime;
begin
  Result := StartOfTheWeek(Now) + 14;
end;

class function TdxEvalHelpers.FnLocalDateTimeLastMonth: TDateTime;
begin
  Result := IncMonth(StartOfTheMonth(Now), -1);
end;

class function TdxEvalHelpers.FnLocalDateTimeLastYear: TDateTime;
begin
  Result := EncodeDate(YearOf(Now) - 1, 1, 1);
end;

class function TdxEvalHelpers.FnLocalDateTimeNextWeek: TDateTime;
begin
  Result := StartOfTheWeek(Now) + 7;
end;

class function TdxEvalHelpers.FnLocalDateTimeDayAfterTomorrow: TDateTime;
begin
  Result := IncDay(Date, 2);
end;

class function TdxEvalHelpers.FnLocalDateTimeTomorrow: TDateTime;
begin
  Result := IncDay(Date);
end;

class function TdxEvalHelpers.FnLocalDateTimeNow: TDateTime;
begin
  Result := Now;
end;

class function TdxEvalHelpers.FnLocalDateTimeToday: TDateTime;
begin
  Result := Date;
end;

class function TdxEvalHelpers.FnLocalDateTimeYesterday: TDateTime;
begin
  Result := IncDay(Date, -1);
end;

class function TdxEvalHelpers.IsLocalDateTime(AFunctionOperatorType: TdxFunctionOperatorType): Boolean;
begin
  Result := AFunctionOperatorType in [
    TdxFunctionOperatorType.LocalDateTimeThisYear,
    TdxFunctionOperatorType.LocalDateTimeThisMonth,
    TdxFunctionOperatorType.LocalDateTimeLastWeek,
    TdxFunctionOperatorType.LocalDateTimeThisWeek,
    TdxFunctionOperatorType.LocalDateTimeYesterday,
    TdxFunctionOperatorType.LocalDateTimeToday,
    TdxFunctionOperatorType.LocalDateTimeNow,
    TdxFunctionOperatorType.LocalDateTimeTomorrow,
    TdxFunctionOperatorType.LocalDateTimeDayAfterTomorrow,
    TdxFunctionOperatorType.LocalDateTimeNextWeek,
    TdxFunctionOperatorType.LocalDateTimeTwoWeeksAway,
    TdxFunctionOperatorType.LocalDateTimeTwoMonthsAway,
    TdxFunctionOperatorType.LocalDateTimeNextMonth,
    TdxFunctionOperatorType.LocalDateTimeNextYear,
    TdxFunctionOperatorType.LocalDateTimeTwoYearsAway,
    TdxFunctionOperatorType.LocalDateTimeLastMonth,
    TdxFunctionOperatorType.LocalDateTimeLastYear,
    TdxFunctionOperatorType.LocalDateTimeYearBeforeToday
  ];
end;

class function TdxEvalHelpers.IsOutlookInterval(AFunctionOperatorType: TdxFunctionOperatorType): Boolean;
begin
  Result := AFunctionOperatorType in [
    TdxFunctionOperatorType.IsSameDay,
    TdxFunctionOperatorType.IsThisMonth,
    TdxFunctionOperatorType.IsThisWeek,
    TdxFunctionOperatorType.IsThisYear,
    TdxFunctionOperatorType.IsNextMonth,
    TdxFunctionOperatorType.IsNextYear,
    TdxFunctionOperatorType.IsLastMonth,
    TdxFunctionOperatorType.IsLastYear,
    TdxFunctionOperatorType.IsYearToDate,
    TdxFunctionOperatorType.IsOutlookIntervalBeyondThisYear,
    TdxFunctionOperatorType.IsOutlookIntervalLaterThisYear,
    TdxFunctionOperatorType.IsOutlookIntervalLaterThisMonth,
    TdxFunctionOperatorType.IsOutlookIntervalNextWeek,
    TdxFunctionOperatorType.IsOutlookIntervalLaterThisWeek,
    TdxFunctionOperatorType.IsOutlookIntervalTomorrow,
    TdxFunctionOperatorType.IsOutlookIntervalToday,
    TdxFunctionOperatorType.IsOutlookIntervalYesterday,
    TdxFunctionOperatorType.IsOutlookIntervalEarlierThisWeek,
    TdxFunctionOperatorType.IsOutlookIntervalLastWeek,
    TdxFunctionOperatorType.IsOutlookIntervalEarlierThisMonth,
    TdxFunctionOperatorType.IsOutlookIntervalEarlierThisYear,
    TdxFunctionOperatorType.IsOutlookIntervalPriorThisYear,
    TdxFunctionOperatorType.IsJanuary,
    TdxFunctionOperatorType.IsFebruary,
    TdxFunctionOperatorType.IsMarch,
    TdxFunctionOperatorType.IsApril,
    TdxFunctionOperatorType.IsMay,
    TdxFunctionOperatorType.IsJune,
    TdxFunctionOperatorType.IsJuly,
    TdxFunctionOperatorType.IsAugust,
    TdxFunctionOperatorType.IsSeptember,
    TdxFunctionOperatorType.IsOctober,
    TdxFunctionOperatorType.IsNovember,
    TdxFunctionOperatorType.IsDecember
  ];
end;

class function TdxEvalHelpers.FnLocalDateTimeThisWeek: TDateTime;
begin
  Result := StartOfTheWeek(Now);
end;

class function TdxEvalHelpers.FnLocalDateTimeLastWeek: TDateTime;
begin
  Result := StartOfTheWeek(Now) - 7;
end;

class function TdxEvalHelpers.FnLocalDateTimeThisMonth: TDateTime;
var
  ANow: TDateTime;
begin
  ANow := Now;
  Result := EncodeDate(YearOf(ANow), MonthOf(ANow), 1);
end;

class function TdxEvalHelpers.FnLocalDateTimeThisYear: TDateTime;
begin
  Result := EncodeDate(YearOf(Now), 1, 1);
end;

class function TdxEvalHelpers.FnLocalDateTimeYearBeforeToday: TDateTime;
begin
  Result := IncYear(Now, -1);
end;


end.
