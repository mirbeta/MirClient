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

unit dxEMF.DB.AliasExpander;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  Classes, SysUtils, Generics.Collections, RTTI,
  dxCoreClasses,
  dxEMF.Types,
  dxEMF.DB.Criteria,
  dxEMF.Metadata,
  dxEMF.Utils.Evaluator;

type

  TdxPersistentCriterionExpanderRequiresPostProcessingAction = (
		None,
    WriteToConsole,
		ThrowException,
    WriteToLog
	);

  { IdxPersistentValueExtractor }

  IdxPersistentValueExtractor = interface
  ['{FB143F7C-7AA4-4077-9AEA-FFD94DDCA976}']
    function GetCaseSensitive: Boolean;
    function ExtractPersistentValue(const ACriterionValue: TValue): TValue;
    property CaseSensitive: Boolean read GetCaseSensitive;
  end;

  { TdxExpandedCriteriaHolder }

  TdxExpandedCriteriaHolder = record
  strict private
    class var
      FAlwaysFalseCriteria: IdxCriteriaOperator;
      FNullOperandValue: IdxCriteriaOperator;
      FTrue: TdxExpandedCriteriaHolder;
      FFalse: TdxExpandedCriteriaHolder;
    class constructor Initialize;
  strict private
    FExpandedCriteria: IdxCriteriaOperator;
    FPostProcessingCause: string;
    FIsConstant: Boolean;
    function GetRequiresPostProcessing: Boolean;
    function GetIsTrue: Boolean;
    function GetIsFalse: Boolean;
    function GetIsNullValue: Boolean;
  public
    constructor Create(const AExpandedCriteria: IdxCriteriaOperator); overload;
    constructor Create(const AExpandedCriteria: IdxCriteriaOperator; const APostProcessingCause: string); overload;
    constructor Create(const AExpandedCriteria: IdxCriteriaOperator; AIsConstant: Boolean); overload;
    constructor Create(const AExpandedCriteria: IdxCriteriaOperator; const APostProcessingCause: string; AIsConstant: Boolean); overload;
    class function Indeterminate(const ACauseProperty: string): TdxExpandedCriteriaHolder; overload; static;
    class function Indeterminate(const AIndeterminateProperty: IdxOperandProperty): TdxExpandedCriteriaHolder; overload; static;
    class function TryConvertToLogicalConstant(const AOperand: IdxCriteriaOperator; AIsConstantIfFail: Boolean): TdxExpandedCriteriaHolder; static;
    class function IfNeededConvertToLogicalOperator(const AOperand: IdxCriteriaOperator): IdxCriteriaOperator; static;
    class function IfNeededConvertToBoolOperator(const AOperand: IdxCriteriaOperator): IdxCriteriaOperator; static;
    class function IfNeededConvertToLogicalHolder(AHolder: TdxExpandedCriteriaHolder): TdxExpandedCriteriaHolder; static;
    class function IfNeededConvertToBoolHolder(AHolder: TdxExpandedCriteriaHolder): TdxExpandedCriteriaHolder; static;
    class operator Implicit(AValue: pointer): TdxExpandedCriteriaHolder;
    function IsEmpty: Boolean;

    property ExpandedCriteria: IdxCriteriaOperator read FExpandedCriteria;
    property PostProcessingCause: string read FPostProcessingCause;
    property IsConstant: Boolean read FIsConstant;
    property RequiresPostProcessing: Boolean read GetRequiresPostProcessing;
    property IsTrue: Boolean read GetIsTrue;
    property IsFalse: Boolean read GetIsFalse;
    property IsNullValue: Boolean read GetIsNullValue;
    class property FalseCriteria: TdxExpandedCriteriaHolder read FFalse;
    class property TrueCriteria: TdxExpandedCriteriaHolder read FTrue;
  end;

  { TdxStronglyTypedCriteriaVisitorBase }

  TdxStronglyTypedCriteriaVisitorBase<T> = class abstract(TcxIUnknownObject, IdxClientCriteriaVisitor<T>)
  strict private
    FUpLevelsClassInfos: TArray<TdxEntityInfo>;
  protected
    class function GetUpLevels(const AUpLevels: TArray<TdxEntityInfo>; AProp: TdxEvaluatorProperty): TArray<TdxEntityInfo>; overload; static;
    function GetUpLevels(const ACollection: IdxOperandProperty): TArray<TdxEntityInfo>; overload;
    class function GetUpLevels(const AUpLevels: TArray<TdxEntityInfo>; const AClassName: string): TArray<TdxEntityInfo>; overload; static;
    function GetUpLevels(const AClassName: string): TArray<TdxEntityInfo>; overload;
    function DoVisit(const AOperand: IdxOperandProperty): T; overload; virtual; abstract;
    function DoVisit(const AOperand: IdxAggregateOperand): T; overload; virtual; abstract;
    function DoVisit(const AOperand: IdxJoinOperand): T; overload; virtual; abstract;
    function DoVisit(const AOperator: IdxFunctionOperator): T; overload; virtual; abstract;
    function DoVisit(const AOperand: IdxOperandValue): T; overload; virtual; abstract;
    function DoVisit(const AOperator: IdxGroupOperator): T; overload; virtual; abstract;
    function DoVisit(const AOperator: IdxInOperator): T; overload; virtual; abstract;
    function DoVisit(const AOperator: IdxUnaryOperator): T; overload; virtual; abstract;
    function DoVisit(const AOperator: IdxBinaryOperator): T; overload; virtual; abstract;
    function DoVisit(const AOperator: IdxBetweenOperator): T; overload; virtual; abstract;

    function Visit(const AOperand: IdxOperandProperty): T; overload;
    function Visit(const AOperand: IdxAggregateOperand): T; overload;
    function Visit(const AOperand: IdxJoinOperand): T; overload;
    function Visit(const AOperator: IdxFunctionOperator): T; overload;
    function Visit(const AOperand: IdxOperandValue): T; overload;
    function Visit(const AOperator: IdxGroupOperator): T; overload;
    function Visit(const AOperator: IdxInOperator): T; overload;
    function Visit(const AOperator: IdxUnaryOperator): T; overload;
    function Visit(const AOperator: IdxBinaryOperator): T; overload;
    function Visit(const AOperator: IdxBetweenOperator): T; overload;

    property UpLevelsClassInfos: TArray<TdxEntityInfo> read FUpLevelsClassInfos;
  public
    constructor Create(const AUpLevelsClassInfos: TArray<TdxEntityInfo>);
  end;

  { TdxUnknownCriteriaEliminatorBase }

  TdxUnknownCriteriaEliminatorBase = class abstract(TdxStronglyTypedCriteriaVisitorBase<TdxExpandedCriteriaHolder>)
  strict private
    FUnderNot: Boolean;
    FCaseSensitive: Boolean;
  protected
    function DoVisit(const AOperand: IdxAggregateOperand): TdxExpandedCriteriaHolder; override;
    function DoVisit(const AOperand: IdxJoinOperand): TdxExpandedCriteriaHolder; override;
    function DoVisit(const AOperator: IdxFunctionOperator): TdxExpandedCriteriaHolder; override;
    function DoVisit(const AOperator: IdxGroupOperator): TdxExpandedCriteriaHolder; override;
    function DoVisit(const AOperator: IdxInOperator): TdxExpandedCriteriaHolder; override;
    function DoVisit(const AOperator: IdxUnaryOperator): TdxExpandedCriteriaHolder; override;
    function DoVisit(const AOperator: IdxBinaryOperator): TdxExpandedCriteriaHolder; override;
    function DoVisit(const AOperator: IdxBetweenOperator): TdxExpandedCriteriaHolder; override;
    function Process(const AOperand: IdxCriteriaOperator): TdxExpandedCriteriaHolder;
    function ProcessInContext(const AUpLevels: TArray<TdxEntityInfo>;
      const AOperand: IdxCriteriaOperator): TdxExpandedCriteriaHolder; virtual; abstract;
    function Evaluate(const AOperator: IdxCriteriaOperator): IdxOperandValue;
  public
    constructor Create(const AUpLevelsClassInfo: TArray<TdxEntityInfo>; ACaseSensitive: Boolean);
    class function GetFalseConditionAggregate(AAggregate: TdxAggregateFunctionType): TdxExpandedCriteriaHolder; static;
  end;

  { TdxPersistentCriterionExpander }

  TdxPersistentCriterionExpander = class(TdxUnknownCriteriaEliminatorBase)
  strict private type
  strict private
    class var
      FPersistentCriterionExpanderRequiresPostProcessingAction: TdxPersistentCriterionExpanderRequiresPostProcessingAction;
  strict private
    FDoDetectPostProcessing: Boolean;
    FPersistentValuesSource: IdxPersistentValueExtractor;
    FAliasDepthWatchDog: Integer;
  protected
    class function IsSingle(const ACriteria: IdxCriteriaOperator): Boolean; static;
    class function IsIif(const ACriteria: IdxCriteriaOperator): Boolean; static;
    class function IsTwoArgumentIsNull(const ACriteria: IdxCriteriaOperator): Boolean; static;
    class function FixPropertyExclamation(const AOriginalOperand: IdxOperandProperty; out AAddKeyTail: Boolean): IdxOperandProperty; static;
    function IsValidForPersistentCriterion(AMi: TdxMappingMemberInfo): Boolean; virtual;
    function ProcessInContext(const AUpLevels: TArray<TdxEntityInfo>; const AOperand: IdxCriteriaOperator): TdxExpandedCriteriaHolder; override;
    class function Expand(const AUpLevels: TArray<TdxEntityInfo>; const APersistentValuesSource: IdxPersistentValueExtractor;
      const AOperator: IdxCriteriaOperator; AAliasDepthWatchDog: Integer; ADoDetectPostProcessing: Boolean): TdxExpandedCriteriaHolder; overload; static;
    class function Expand(const AUpLevels: TArray<TdxEntityInfo>; const APersistentValuesSource: IdxPersistentValueExtractor;
      const AOperator: IdxCriteriaOperator; ADoDetectPostProcessing: Boolean): TdxExpandedCriteriaHolder; overload; static;
    function DoVisit(const AOperator: IdxBinaryOperator): TdxExpandedCriteriaHolder; override;
    function DoVisit(const AOriginalOperand: IdxOperandProperty): TdxExpandedCriteriaHolder; override;
    function DoVisit(const AOperand: IdxAggregateOperand): TdxExpandedCriteriaHolder; override;
    function DoVisit(const AOperand: IdxOperandValue): TdxExpandedCriteriaHolder; override;
    function DoVisit(const AOperator: IdxFunctionOperator): TdxExpandedCriteriaHolder; override;
  public
    constructor Create(const AUpLevels: TArray<TdxEntityInfo>; const APersistentValuesSource: IdxPersistentValueExtractor; AAliasDepthWatchDog: Integer; ADoDetectPostProcessing: Boolean = True);
    function ProcessSingleAlias(const AUpLevelsWithReference: TArray<TdxEntityInfo>; const AAlias: IdxCriteriaOperator; APath: TdxMemberInfoCollection; AStart: Integer): TdxExpandedCriteriaHolder;
    function ProcessIifAlias(const AUpLevelsWithReference: TArray<TdxEntityInfo>; const AExpression: IdxCriteriaOperator; APath: TdxMemberInfoCollection; AStart: Integer): TdxExpandedCriteriaHolder;
    function ProcessTwoArgumentIsNullAlias(const AUpLevelsWithReference: TArray<TdxEntityInfo>;
      const AExpression: IdxCriteriaOperator; APath: TdxMemberInfoCollection; AStart: Integer): TdxExpandedCriteriaHolder;
    function MergeAlias(const AUpLevelsWithReference: TArray<TdxEntityInfo>; APath: TdxMemberInfoCollection; AStart: Integer;
      AMemberInfo: TdxMappingMemberInfo; const AExpression: IdxCriteriaOperator): TdxExpandedCriteriaHolder;
    class function ExpandToLogical(AEntityInfo: TdxEntityInfo; const APersistentValuesSource: IdxPersistentValueExtractor; const AOperator: IdxCriteriaOperator; ADoDetectPostProcessing: Boolean): TdxExpandedCriteriaHolder; overload; static;
    class function ExpandToLogical(const APersistentValuesSource: IdxPersistentValueExtractor; AEntityInfo: TdxEntityInfo; const AOperator: IdxCriteriaOperator; ADoDetectPostProcessing: Boolean): TdxExpandedCriteriaHolder; overload; static;
    class function ExpandToValue(AEntityInfo: TdxEntityInfo; const APersistentValuesSource: IdxPersistentValueExtractor; const AOperator: IdxCriteriaOperator; ADoDetectPostProcessing: Boolean): TdxExpandedCriteriaHolder; overload; static;
    class function ExpandToValue(const APersistentValuesSource: IdxPersistentValueExtractor; AEntityInfo: TdxEntityInfo; const AOperator: IdxCriteriaOperator; ADoDetectPostProcessing: Boolean): TdxExpandedCriteriaHolder; overload; static;
    class function Expand(AEntityInfo: TdxEntityInfo; const APersistentValuesSource: IdxPersistentValueExtractor; const AOperator: IdxCriteriaOperator): TdxExpandedCriteriaHolder; overload; static;
    class function Expand(AEntityInfo: TdxEntityInfo; const APersistentValuesSource: IdxPersistentValueExtractor; const AOperator: IdxCriteriaOperator; ADoDetectPostProcessing: Boolean): TdxExpandedCriteriaHolder; overload; static;
    class function Expand(const APersistentValuesSource: IdxPersistentValueExtractor; AEntityInfo: TdxEntityInfo; const AOperator: IdxCriteriaOperator): TdxExpandedCriteriaHolder; overload; static;
    class function Expand(const APersistentValuesSource: IdxPersistentValueExtractor; AEntityInfo: TdxEntityInfo; const AOperator: IdxCriteriaOperator; ADoDetectPostProcessing: Boolean): TdxExpandedCriteriaHolder; overload; static;
  end;

implementation

uses
  dxCore, dxTypeHelpers, dxStringHelper,
  dxEMF.Utils,
  dxEMF.Utils.Exceptions,
  dxEMF.Strs;

type

  { TdxEMFPropertyDescriptor }

  TdxEMFPropertyDescriptor = class sealed
  public const
    ReferenceAsObjectTail = '!';
    ReferenceAsKeyTail = '!Key';
  end;

  { TdxJoinOperandExpander }

  TdxJoinOperandExpander = class(TdxClientCriteriaVisitorBase, IdxCriteriaOperatorVisitor)
  strict private type
{$REGION 'TJoinOperandExpanderState'}
      TJoinOperandExpanderState = record
      public
        IsInJoinOperand: Boolean;
        InAtomOperator: Integer;
        EntityInfo: TdxEntityInfo;
        JoinReferenceMemberInfo: TdxMappingMemberInfo;
        ParentKeyMemberInfo: TdxMappingMemberInfo;
        EqualsBinaryOperator: IdxBinaryOperator;
        FoundAssociationProperty: IdxOperandProperty;
        constructor Create(AEntityInfo: TdxEntityInfo; AIsInJoinOperand: Boolean = false);
      end;
{$ENDREGION}
  strict private
    FCurrentState: TJoinOperandExpanderState;
    FStateStack: TStack<TJoinOperandExpanderState>;
  protected
    function DoVisit(const AOperand: IdxJoinOperand): IdxCriteriaOperator; override;
    function DoVisit(const AOperand: IdxOperandProperty): IdxCriteriaOperator; override;
    function DoVisit(const AOperand: IdxAggregateOperand; AProcessCollectionProperty: Boolean): IdxCriteriaOperator; override;

    function Visit(const AOperator: IdxFunctionOperator): IdxCriteriaOperator; overload;
    function Visit(const AOperator: IdxGroupOperator): IdxCriteriaOperator; overload;
    function Visit(const AOperator: IdxInOperator): IdxCriteriaOperator; overload;
    function Visit(const AOperator: IdxUnaryOperator): IdxCriteriaOperator; overload;
    function Visit(const AOperatorArg: IdxBinaryOperator): IdxCriteriaOperator; overload;
    function Visit(const AOperator: IdxBetweenOperator): IdxCriteriaOperator; overload;
  public
    constructor Create(ACurrentEntityInfo: TdxEntityInfo); overload;
    constructor Create(const AUpLevelsClassInfo: TArray<TdxEntityInfo>); overload;
    destructor Destroy; override;
    class function Expand(ACurrentEntityInfo: TdxEntityInfo; const ACriteria: IdxCriteriaOperator): IdxCriteriaOperator; overload; static;
    class function Expand(const AUpLevelsClassInfo: TArray<TdxEntityInfo>; const ACriteria: IdxCriteriaOperator): IdxCriteriaOperator; overload; static;
  end;

  { TdxConstantDescriptor }

  TdxConstantDescriptor = class(TdxEvaluatorContextDescriptor)
  strict private
    class var
      FInstance: TdxEvaluatorContextDescriptor;
    class destructor Finalize;
    class function GetInstance: TdxEvaluatorContextDescriptor; static; inline;
  public
    function GetPropertyValue(ASource: TObject; APropertyPath: TdxEvaluatorProperty): TObject; override;
    function GetNestedContext(ASource: TObject; const APropertyPath: string): TdxEvaluatorContext; override;
    function GetCollectionContexts(ASource: TObject; const ACollectionName: string): IEnumerable; override;
    function GetQueryContexts(ASource: TObject; const AQueryTypeName: string;
      const ACondition: IdxCriteriaOperator; ATop: Integer): TEnumerable<TdxEvaluatorContext>; override;
    class property Instance: TdxEvaluatorContextDescriptor read GetInstance;
  end;

{ TJoinOperandExpanderState }

constructor TdxJoinOperandExpander.TJoinOperandExpanderState.Create(AEntityInfo: TdxEntityInfo; AIsInJoinOperand: Boolean);
begin
  Self := Default(TJoinOperandExpanderState);
  EntityInfo := AEntityInfo;
  IsInJoinOperand := AIsInJoinOperand;
end;

{ TdxJoinOperandExpander }

constructor TdxJoinOperandExpander.Create(const AUpLevelsClassInfo: TArray<TdxEntityInfo>);
var
  I: Integer;
begin
  FStateStack := TStack<TJoinOperandExpanderState>.Create;
  FCurrentState := TJoinOperandExpanderState.Create(AUpLevelsClassInfo[0]);
  for I := Length(AUpLevelsClassInfo) - 1 downto 1 do
    FStateStack.Push(TJoinOperandExpanderState.Create(AUpLevelsClassInfo[I]));
end;

constructor TdxJoinOperandExpander.Create(ACurrentEntityInfo: TdxEntityInfo);
begin
  FStateStack := TStack<TJoinOperandExpanderState>.Create;
  FCurrentState := TJoinOperandExpanderState.Create(ACurrentEntityInfo);
end;

destructor TdxJoinOperandExpander.Destroy;
begin
  FStateStack.Free;
  //FreeAndNil(FStateStack);
  //FreeAndNil(FCurrentState);
  inherited Destroy;
end;

class function TdxJoinOperandExpander.Expand(ACurrentEntityInfo: TdxEntityInfo; const ACriteria: IdxCriteriaOperator): IdxCriteriaOperator;
begin
  with TdxJoinOperandExpander.Create(ACurrentEntityInfo) do
  try
    Result := Process(ACriteria as TdxCriteriaOperator);
  finally
    Free;
  end;
end;

class function TdxJoinOperandExpander.Expand(const AUpLevelsClassInfo: TArray<TdxEntityInfo>; const ACriteria: IdxCriteriaOperator): IdxCriteriaOperator;
begin
  with TdxJoinOperandExpander.Create(AUpLevelsClassInfo) do
  try
    Result := Process(ACriteria  as TdxCriteriaOperator);
  finally
    Free;
  end;
end;

function TdxJoinOperandExpander.DoVisit(const AOperand: IdxJoinOperand): IdxCriteriaOperator;
var
  AJoinedCi: TdxEntityInfo;
  ANewState: TJoinOperandExpanderState;
  AResultCondition, AAggregatedExpression: IdxCriteriaOperator;
begin
  AJoinedCi := nil;
  if not TdxMemberInfoCollection.TryResolveTypeAlsoByShortName(AOperand.JoinTypeName, FCurrentState.EntityInfo, AJoinedCi) then
    raise EdxCannotResolveClassInfoException.CreateFmt(sdxMetadataCannotResolveEntityInfo, [AOperand.JoinTypeName]);
  ANewState := TJoinOperandExpanderState.Create(AJoinedCi, True);
  FStateStack.Push(FCurrentState);
  FCurrentState := ANewState;
  try
    AResultCondition := Process(AOperand.Condition);
    if FCurrentState.FoundAssociationProperty <> nil then
    begin
      Inc(FCurrentState.InAtomOperator);
      try
        Exit(TdxAggregateOperand.Create(FCurrentState.FoundAssociationProperty,
          Process(AOperand.AggregatedExpression), AOperand.AggregateFunctionType, AResultCondition));
      finally
        Dec(FCurrentState.InAtomOperator);
      end;
    end;
    Inc(FCurrentState.InAtomOperator);
    try
      AAggregatedExpression := Process(AOperand.AggregatedExpression);
      if (AResultCondition = AOperand.Condition) and (AAggregatedExpression = AOperand.AggregatedExpression) then
        Result := AOperand
      else
        Result := TdxJoinOperand.Create(AOperand.JoinTypeName, AResultCondition, AOperand.AggregateFunctionType, AAggregatedExpression);
    finally
      Dec(FCurrentState.InAtomOperator);
    end;
  finally
    FCurrentState := FStateStack.Pop;
  end;
end;

function TdxJoinOperandExpander.DoVisit(const AOperand: IdxOperandProperty): IdxCriteriaOperator;
var
  AParentEntityInfo: TdxEntityInfo;
  APropertyName, AMemberName: string;
  APointLastIndex: Integer;
  AReferenceMember: TdxMappingMemberInfo;
begin
  if ((FCurrentState.IsInJoinOperand and (FCurrentState.InAtomOperator = 0)) and
    (FCurrentState.EqualsBinaryOperator <> nil)) and (FStateStack.Count > 0) then
  begin
    AParentEntityInfo := FStateStack.Peek.EntityInfo;
    APropertyName := {$IFDEF DELPHIXE4}AOperand.PropertyName.TrimRight(['!']){$ELSE}TdxStringHelper.TrimEnd(AOperand.PropertyName, ['!']){$ENDIF};
    if {$IFDEF DELPHIXE3}APropertyName.StartsWith('^.'){$ELSE}TdxStringHelper.StartsWith(APropertyName, '^.'){$ENDIF} then
    begin
      APropertyName := {$IFDEF DELPHIXE3}APropertyName.Substring(2){$ELSE}TdxStringHelper.Substring(APropertyName, 2){$ENDIF};
      if AParentEntityInfo.KeyProperty.Member.MemberName = APropertyName then
        FCurrentState.ParentKeyMemberInfo := AParentEntityInfo.KeyProperty.Member;
    end
    else
    begin
      APointLastIndex := TdxMemberInfoCollection.LastIndexOfSplittingDotInPath(APropertyName);
      if APointLastIndex >= 0 then
        AMemberName := {$IFDEF DELPHIXE3}APropertyName.Substring(0, APointLastIndex){$ELSE}TdxStringHelper.Substring(APropertyName, 0, APointLastIndex){$ENDIF}
      else
        AMemberName := APropertyName;
      AReferenceMember := FCurrentState.EntityInfo.FindMember(AMemberName);
      if (((AReferenceMember <> nil) and (AReferenceMember.ReferenceType = AParentEntityInfo)) and
        AReferenceMember.IsAssociation) and ((APointLastIndex < 0) or ((APointLastIndex >= 0) and
        (AParentEntityInfo.KeyProperty.Member.MemberName =
        {$IFDEF DELPHIXE3}APropertyName.Substring(APointLastIndex + 1){$ELSE}TdxStringHelper.Substring(APropertyName, APointLastIndex + 1){$ENDIF}))) then
        FCurrentState.JoinReferenceMemberInfo := AReferenceMember;
    end;
  end;
  Result := AOperand;
end;

function TdxJoinOperandExpander.DoVisit(const AOperand: IdxAggregateOperand; AProcessCollectionProperty: Boolean): IdxCriteriaOperator;
var
  ALocalStateStack: TStack<TJoinOperandExpanderState>;
  APropertyName: string;
  AEntityInfo: TdxEntityInfo;
  AMemberInfos: TdxMemberInfoCollection;
  I: Integer;
  AMemberInfo, ACollectionMemberInfo: TdxMappingMemberInfo;
begin
  if AOperand.IsTopLevel then
    Exit(TdxAggregateOperand.Create(nil, Process(AOperand.AggregatedExpression), AOperand.AggregateFunctionType,
      Process(AOperand.Condition)));
  ALocalStateStack := nil;
  APropertyName := AOperand.CollectionProperty.PropertyName;
  while {$IFDEF DELPHIXE3}APropertyName.StartsWith('^.'){$ELSE}TdxStringHelper.StartsWith(APropertyName, '^.'){$ENDIF} do
  begin
    if ALocalStateStack = nil then
      ALocalStateStack := TStack<TJoinOperandExpanderState>.Create;
    if FStateStack.Count = 0 then
      raise EInvalidOperation.Create('^.');
    APropertyName := {$IFDEF DELPHIXE3}APropertyName.Substring(2){$ELSE}TdxStringHelper.Substring(APropertyName, 2){$ENDIF};
    ALocalStateStack.Push(FCurrentState);
    FCurrentState := FStateStack.Pop;
  end;
  Result := nil;
  try
    AMemberInfos := FCurrentState.EntityInfo.ParsePersistentPath(APropertyName);
    if AMemberInfos.Count = 0 then
      raise EdxInvalidPropertyPathException.CreateFmt(sdxInvalidPropertyPathException,
        [APropertyName, FCurrentState.EntityInfo.FullName]);
    for I := 0 to (AMemberInfos.Count - 1) - 1 do
    begin
      AMemberInfo := AMemberInfos[I];
      FStateStack.Push(FCurrentState);
      if AMemberInfo.ReferenceType = nil then
        AEntityInfo := AMemberInfo.CollectionElementType
      else
        AEntityInfo := AMemberInfo.ReferenceType;
      FCurrentState := TJoinOperandExpanderState.Create(AEntityInfo);
    end;
    ACollectionMemberInfo := AMemberInfos[AMemberInfos.Count - 1];
    if not ACollectionMemberInfo.IsAssociationList then
      raise EdxInvalidPropertyPathException.CreateFmt(sdxInvalidPropertyPathException,
        [APropertyName, FCurrentState.EntityInfo.FullName]);
    FStateStack.Push(FCurrentState);
    FCurrentState := TJoinOperandExpanderState.Create(ACollectionMemberInfo.CollectionElementType);
    try
      Result := inherited DoVisit(AOperand, False);
    finally
      I := 0;
      while (I < AMemberInfos.Count) and (FStateStack.Count > 0) do
      begin
        FCurrentState := FStateStack.Pop;
        Inc(I);
      end;
    end;
  finally
    if ALocalStateStack <> nil then
    begin
      while ALocalStateStack.Count > 0 do
      begin
        FStateStack.Push(FCurrentState);
        FCurrentState := ALocalStateStack.Pop;
      end;
    end;
  end;
end;

function TdxJoinOperandExpander.Visit(const AOperator: IdxFunctionOperator): IdxCriteriaOperator;
begin
  Inc(FCurrentState.InAtomOperator);
  try
    Exit(inherited Visit(AOperator));
  finally
    Dec(FCurrentState.InAtomOperator);
  end;
end;

function TdxJoinOperandExpander.Visit(const AOperator: IdxGroupOperator): IdxCriteriaOperator;
var
  AModified: Boolean;
  AResultOperands: IdxCriteriaOperatorCollection;
  I: Integer;
begin
  if AOperator.OperatorType = TdxGroupOperatorType.Or then
  begin
    Inc(FCurrentState.InAtomOperator);
    try
      Exit(inherited Visit(AOperator));
    finally
      Dec(FCurrentState.InAtomOperator);
    end;
  end;
  AResultOperands := ProcessCollection(AOperator.Operands, AModified);
  if FCurrentState.FoundAssociationProperty <> nil then
  begin
    for I := AResultOperands.Count - 1 downto 0 do
    begin
      if AResultOperands[I] = nil then
      begin
        (AResultOperands as TdxCriteriaOperatorCollection).Delete(I);
        AModified := True;
      end;
    end;
  end;
  if AModified then
    Result := TdxGroupOperator.Create(AOperator.OperatorType, AResultOperands)
  else
    Result := AOperator;
end;

function TdxJoinOperandExpander.Visit(const AOperator: IdxInOperator): IdxCriteriaOperator;
begin
  Inc(FCurrentState.InAtomOperator);
  try
    Exit(inherited Visit(AOperator));
  finally
    Dec(FCurrentState.InAtomOperator);
  end;
end;

function TdxJoinOperandExpander.Visit(const AOperator: IdxUnaryOperator): IdxCriteriaOperator;
begin
  Inc(FCurrentState.InAtomOperator);
  try
    Exit(inherited Visit(AOperator));
  finally
    Dec(FCurrentState.InAtomOperator);
  end;
end;

function TdxJoinOperandExpander.Visit(const AOperatorArg: IdxBinaryOperator): IdxCriteriaOperator;
var
  AOperator: IdxBinaryOperator;
  AAggregate: IdxAggregateOperand;
  AInAtomOperatorSet: Boolean;
  ARightOperator, ALeftOperator: IdxCriteriaOperator;
  ACollectionMember: TdxMappingMemberInfo;
begin
  AOperator := AOperatorArg;
  if (AOperator.RightOperand is TdxOperandValue) and TdxOperandValue(AOperator.RightOperand).Value.Equals(0)
    and (AOperator.LeftOperand is TdxAggregateOperand) and
    (TdxAggregateOperand(AOperator.LeftOperand).AggregateFunctionType = TdxAggregateFunctionType.Count) then
  begin
    AAggregate := TdxAggregateOperand(AOperator.LeftOperand);
    if not (AAggregate as TdxAggregateOperand).IsTopLevel then
    begin
      case AOperator.OperatorType of
        TdxBinaryOperatorType.Equal:
          Exit(Process(TdxAggregateOperand.Create((AAggregate as TdxAggregateOperand).CollectionProperty,
            (AAggregate as TdxAggregateOperand).AggregatedExpression,
            TdxAggregateFunctionType.Exists, (AAggregate as TdxAggregateOperand).Condition).&Not));
        TdxBinaryOperatorType.Greater:
          Exit(Process(TdxAggregateOperand.Create((AAggregate as TdxAggregateOperand).CollectionProperty,
            (AAggregate as TdxAggregateOperand).AggregatedExpression,
            TdxAggregateFunctionType.Exists, (AAggregate as TdxAggregateOperand).Condition)));
      end;
    end;
  end;
  AInAtomOperatorSet := False;
  if (AOperator.OperatorType = TdxBinaryOperatorType.Equal) and (FCurrentState.FoundAssociationProperty = nil) then
  begin
    if FCurrentState.EqualsBinaryOperator = nil then
      FCurrentState.EqualsBinaryOperator := AOperator
    else
    begin
      AInAtomOperatorSet := True;
      Inc(FCurrentState.InAtomOperator);
    end;
  end
  else
  begin
    FCurrentState.EqualsBinaryOperator := nil;
    ARightOperator := Process(AOperator.RightOperand);
    ALeftOperator := Process(AOperator.LeftOperand);
    Exit(TdxBinaryOperator.Create(ALeftOperator, ARightOperator, AOperator.OperatorType));
  end;
  try
    ALeftOperator := Process(AOperator.LeftOperand);
    ARightOperator := Process(AOperator.RightOperand);
    if (FCurrentState.JoinReferenceMemberInfo <> nil) and (FCurrentState.ParentKeyMemberInfo <> nil) then
    begin
      if FCurrentState.JoinReferenceMemberInfo.IsAssociation then
      begin
        ACollectionMember := FCurrentState.JoinReferenceMemberInfo.AssociatedMember;
        if ACollectionMember.IsAssociationList and (ACollectionMember.CollectionElementType = FCurrentState.EntityInfo) then
        begin
          FCurrentState.FoundAssociationProperty := TdxOperandProperty.Create(ACollectionMember.MemberName);
          Exit(nil);
        end;
      end;
    end;
    FCurrentState.JoinReferenceMemberInfo := nil;
    FCurrentState.ParentKeyMemberInfo := nil;
    Result := TdxBinaryOperator.Create(ALeftOperator, ARightOperator, AOperator.OperatorType);
  finally
    FCurrentState.EqualsBinaryOperator := nil;
    if AInAtomOperatorSet then
      Dec(FCurrentState.InAtomOperator);
  end;
end;

function TdxJoinOperandExpander.Visit(const AOperator: IdxBetweenOperator): IdxCriteriaOperator;
begin
  Inc(FCurrentState.InAtomOperator);
  try
    Exit(inherited Visit(AOperator));
  finally
    Dec(FCurrentState.InAtomOperator);
  end;
end;

{ TdxConstantDescriptor }

class destructor TdxConstantDescriptor.Finalize;
begin
  FreeAndNil(FInstance);
end;

function TdxConstantDescriptor.GetPropertyValue(ASource: TObject; APropertyPath: TdxEvaluatorProperty): TObject;
begin
  raise EdxEMFException.Create(sdxCommonMethodOrOperationNotImplemented);
end;

function TdxConstantDescriptor.GetNestedContext(ASource: TObject; const APropertyPath: string): TdxEvaluatorContext;
begin
  raise EdxEMFException.Create(sdxCommonMethodOrOperationNotImplemented);
end;

function TdxConstantDescriptor.GetCollectionContexts(ASource: TObject; const ACollectionName: string): IEnumerable;
begin
  raise EdxEMFException.Create(sdxCommonMethodOrOperationNotImplemented);
end;

class function TdxConstantDescriptor.GetInstance: TdxEvaluatorContextDescriptor;
begin
  if FInstance = nil then
    FInstance := TdxConstantDescriptor.Create;
  Result := FInstance;
end;

function TdxConstantDescriptor.GetQueryContexts(ASource: TObject; const AQueryTypeName: string;
  const ACondition: IdxCriteriaOperator; ATop: Integer): TEnumerable<TdxEvaluatorContext>;
begin
  raise EdxEMFException.Create(sdxCommonMethodOrOperationNotImplemented);
end;

{ TdxExpandedCriteriaHolder }

class constructor TdxExpandedCriteriaHolder.Initialize;
begin
  FAlwaysFalseCriteria := TdxBinaryOperator.Create(TdxConstantValue.Create(1), TdxConstantValue.Create(2), TdxBinaryOperatorType.Equal);
  FNullOperandValue := TdxOperandValue.Create(nil);
  FTrue := TdxExpandedCriteriaHolder.Create(nil, True);
  FFalse := TdxExpandedCriteriaHolder.Create(FAlwaysFalseCriteria, True);
end;

function TdxExpandedCriteriaHolder.IsEmpty: Boolean;
begin
  Result := FExpandedCriteria = nil;
end;

constructor TdxExpandedCriteriaHolder.Create(const AExpandedCriteria: IdxCriteriaOperator; const APostProcessingCause: string; AIsConstant: Boolean);
begin
  FExpandedCriteria := AExpandedCriteria;
  FPostProcessingCause := APostProcessingCause;
  FIsConstant := AIsConstant;
end;

constructor TdxExpandedCriteriaHolder.Create(const AExpandedCriteria: IdxCriteriaOperator; AIsConstant: Boolean);
begin
  Create(AExpandedCriteria, '', AIsConstant);
end;

constructor TdxExpandedCriteriaHolder.Create(const AExpandedCriteria: IdxCriteriaOperator; const APostProcessingCause: string);
begin
  Create(AExpandedCriteria, APostProcessingCause, False);
end;

constructor TdxExpandedCriteriaHolder.Create(const AExpandedCriteria: IdxCriteriaOperator);
begin
  Create(AExpandedCriteria, '');
end;

function TdxExpandedCriteriaHolder.GetRequiresPostProcessing: Boolean;
begin
  Result := FPostProcessingCause <> '';
end;

function TdxExpandedCriteriaHolder.GetIsTrue: Boolean;
begin
  Result := FExpandedCriteria = nil;
end;

function TdxExpandedCriteriaHolder.GetIsFalse: Boolean;
begin
  Result := TdxCriteriaOperator.Equals(FExpandedCriteria as TdxCriteriaOperator, FAlwaysFalseCriteria);
end;

function TdxExpandedCriteriaHolder.GetIsNullValue: Boolean;
begin
  Result := TdxCriteriaOperator.Equals(FExpandedCriteria as TdxCriteriaOperator, FNullOperandValue);
end;

class function TdxExpandedCriteriaHolder.Indeterminate(const ACauseProperty: string): TdxExpandedCriteriaHolder;
begin
  Assert(ACauseProperty <> '');
  Result := TdxExpandedCriteriaHolder.Create(nil, ACauseProperty);
end;

class function TdxExpandedCriteriaHolder.Indeterminate(const AIndeterminateProperty: IdxOperandProperty): TdxExpandedCriteriaHolder;
var
  APropName: string;
begin
  APropName := AIndeterminateProperty.PropertyName;
  Result := Indeterminate(APropName);
end;

class function TdxExpandedCriteriaHolder.TryConvertToLogicalConstant(const AOperand: IdxCriteriaOperator; AIsConstantIfFail: Boolean): TdxExpandedCriteriaHolder;
begin
  if Supports(AOperand, IdxOperandValue) and TdxOperandValue(AOperand).Value.IsBoolean then
  begin
    if TdxOperandValue(AOperand).Value.AsBoolean then
      Result := FTrue
    else
      Result := FFalse;
  end
  else
    Result := TdxExpandedCriteriaHolder.Create(AOperand, AIsConstantIfFail);
end;

class function TdxExpandedCriteriaHolder.IfNeededConvertToLogicalOperator(const AOperand: IdxCriteriaOperator): IdxCriteriaOperator;
begin
  if TdxIsLogicalCriteriaChecker.GetBooleanState(AOperand as TdxCriteriaOperator) = TdxBooleanCriteriaState.Value then
    Result := TdxBinaryOperator.Create(AOperand, TdxConstantValue.Create(True), TdxBinaryOperatorType.Equal)
  else
    Result := AOperand;
end;

class operator TdxExpandedCriteriaHolder.Implicit(AValue: pointer): TdxExpandedCriteriaHolder;
begin
  Assert(AValue = nil);
  Result := TdxExpandedCriteriaHolder.Create(nil);
end;

class function TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(const AOperand: IdxCriteriaOperator): IdxCriteriaOperator;
begin
  if TdxIsLogicalCriteriaChecker.GetBooleanState(AOperand as TdxCriteriaOperator) = TdxBooleanCriteriaState.Logical then
  begin
    if AOperand = nil then
      Result := TdxConstantValue.Create(True)
    else
      Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Iif,
        [AOperand, TdxConstantValue.Create(True), TdxConstantValue.Create(False)]);
  end
  else
    Result := AOperand;
end;

class function TdxExpandedCriteriaHolder.IfNeededConvertToLogicalHolder(AHolder: TdxExpandedCriteriaHolder): TdxExpandedCriteriaHolder;
begin
  if TdxIsLogicalCriteriaChecker.GetBooleanState(AHolder.ExpandedCriteria) = TdxBooleanCriteriaState.Value then
    Result := TdxExpandedCriteriaHolder.Create(TdxBinaryOperator.Create(AHolder.ExpandedCriteria, TdxConstantValue.Create(True), TdxBinaryOperatorType.Equal), AHolder.PostProcessingCause, AHolder.IsConstant)
  else
    Result := AHolder;
end;

class function TdxExpandedCriteriaHolder.IfNeededConvertToBoolHolder(AHolder: TdxExpandedCriteriaHolder): TdxExpandedCriteriaHolder;
begin
  if TdxIsLogicalCriteriaChecker.GetBooleanState(AHolder.ExpandedCriteria) = TdxBooleanCriteriaState.Logical then
  begin
    if AHolder.IsTrue then
      Exit(TdxExpandedCriteriaHolder.Create(TdxConstantValue.Create(True), AHolder.PostProcessingCause, AHolder.IsConstant));
    Result := TdxExpandedCriteriaHolder.Create(TdxFunctionOperator.Create(TdxFunctionOperatorType.Iif,
      [AHolder.ExpandedCriteria, TdxConstantValue.Create(True), TdxConstantValue.Create(False)]),
      AHolder.PostProcessingCause, AHolder.IsConstant);
  end
  else
    Result := AHolder;
end;

{ TdxStronglyTypedCriteriaVisitorBase }

constructor TdxStronglyTypedCriteriaVisitorBase<T>.Create(const AUpLevelsClassInfos: TArray<TdxEntityInfo>);
begin
  if AUpLevelsClassInfos = nil then
    raise EArgumentNilException.Create(sdxUpLevelsClassInfos);
  FUpLevelsClassInfos := AUpLevelsClassInfos;
end;

class function TdxStronglyTypedCriteriaVisitorBase<T>.GetUpLevels(const AUpLevels: TArray<TdxEntityInfo>; AProp: TdxEvaluatorProperty): TArray<TdxEntityInfo>;
var
  AResult: TList<TdxEntityInfo>;
  I: Integer;
  ACurrentBase: TdxEntityInfo;
  ACollection: TdxMemberInfoCollection;
  AMemberInfo: TdxMappingMemberInfo;
begin
  AResult := TList<TdxEntityInfo>.Create;
  try
    for I := Length(AUpLevels) - 1 downto AProp.UpDepth do
      AResult.Insert(0, AUpLevels[I]);
    if TdxEvaluatorProperty.GetIsThisProperty(AProp.PropertyPath) and (AResult.Count > 0) then
      Exit(AResult.ToArray);
    ACurrentBase := AUpLevels[AProp.UpDepth];
    ACollection := TdxMemberInfoCollection.ParsePath(ACurrentBase, AProp.PropertyPath);
    try
      for AMemberInfo in ACollection do
      begin
        if (AMemberInfo.ReferenceType <> nil) and AMemberInfo.IsPersistent then
          ACurrentBase := AMemberInfo.ReferenceType
        else
          if AMemberInfo.IsAssociationList then
            ACurrentBase := AMemberInfo.CollectionElementType
          else
            raise EArgumentException.CreateFmt(sdxPersistentAliasExpanderReferenceOrCollectionExpectedInTheMiddleOfThePath,
               [AMemberInfo.MemberName]);
        AResult.Insert(0, ACurrentBase);
      end;
      Result := AResult.ToArray;
    finally
      ACollection.Free;
    end;
  finally
    AResult.Free;
  end;
end;

function TdxStronglyTypedCriteriaVisitorBase<T>.GetUpLevels(const ACollection: IdxOperandProperty): TArray<TdxEntityInfo>;
begin
  if (ACollection = nil) or (ACollection.PropertyName = '') then
    Result := FUpLevelsClassInfos
  else
    Result := GetUpLevels(FUpLevelsClassInfos, TdxEvaluatorProperty.Create(ACollection));
end;

class function TdxStronglyTypedCriteriaVisitorBase<T>.GetUpLevels(const AUpLevels: TArray<TdxEntityInfo>; const AClassName: string): TArray<TdxEntityInfo>;
var
  AAddEntityInfo: TdxEntityInfo;
  AResult: TArray<TdxEntityInfo>;
begin
  if not TdxMemberInfoCollection.TryResolveTypeAlsoByShortName(AClassName, AUpLevels[0], AAddEntityInfo) then
    raise EdxCannotResolveClassInfoException.CreateFmt(sdxMetadataCannotResolveEntityInfo, [AClassName]);
  SetLength(AResult, Length(AUpLevels) + 1);
  TArray.Copy<TdxEntityInfo>(AUpLevels, AResult, 0, 1, Length(AUpLevels));
  AResult[0] := AAddEntityInfo;
  Result := AResult;
end;

function TdxStronglyTypedCriteriaVisitorBase<T>.GetUpLevels(const AClassName: string): TArray<TdxEntityInfo>;
begin
  Result := GetUpLevels(FUpLevelsClassInfos, AClassName);
end;

function TdxStronglyTypedCriteriaVisitorBase<T>.Visit(const AOperand: IdxOperandProperty): T;
begin
  Result := DoVisit(AOperand);
end;

function TdxStronglyTypedCriteriaVisitorBase<T>.Visit(const AOperand: IdxAggregateOperand): T;
begin
  Result := DoVisit(AOperand);
end;

function TdxStronglyTypedCriteriaVisitorBase<T>.Visit(const AOperand: IdxJoinOperand): T;
begin
  Result := DoVisit(AOperand);
end;

function TdxStronglyTypedCriteriaVisitorBase<T>.Visit(const AOperator: IdxFunctionOperator): T;
begin
  Result := DoVisit(AOperator);
end;

function TdxStronglyTypedCriteriaVisitorBase<T>.Visit(const AOperand: IdxOperandValue): T;
begin
  Result := DoVisit(AOperand);
end;

function TdxStronglyTypedCriteriaVisitorBase<T>.Visit(const AOperator: IdxGroupOperator): T;
begin
  Result := DoVisit(AOperator);
end;

function TdxStronglyTypedCriteriaVisitorBase<T>.Visit(const AOperator: IdxInOperator): T;
begin
  Result := DoVisit(AOperator);
end;

function TdxStronglyTypedCriteriaVisitorBase<T>.Visit(const AOperator: IdxUnaryOperator): T;
begin
  Result := DoVisit(AOperator);
end;

function TdxStronglyTypedCriteriaVisitorBase<T>.Visit(const AOperator: IdxBinaryOperator): T;
begin
  Result := DoVisit(AOperator);
end;

function TdxStronglyTypedCriteriaVisitorBase<T>.Visit(const AOperator: IdxBetweenOperator): T;
begin
  Result := DoVisit(AOperator);
end;

{ TdxUnknownCriteriaEliminatorBase }

constructor TdxUnknownCriteriaEliminatorBase.Create(const AUpLevelsClassInfo: TArray<TdxEntityInfo>; ACaseSensitive: Boolean);
begin
  inherited Create(AUpLevelsClassInfo);
  FUnderNot := False;
  FCaseSensitive := ACaseSensitive;
end;

function TdxUnknownCriteriaEliminatorBase.DoVisit(const AOperand: IdxAggregateOperand): TdxExpandedCriteriaHolder;
var
  AProcessedCollectionPropertyHolder, AProcessedAggregatedExpressionHolder, AProcessedConditionHolder: TdxExpandedCriteriaHolder;
  AProcessedCollectionProperty: TdxOperandProperty;
  ANestedUpLevels: TArray<TdxEntityInfo>;
  APersistentOperator, AOperator: IdxCriteriaOperator;
begin
  AProcessedCollectionPropertyHolder := Process(AOperand.CollectionProperty);
  if AProcessedCollectionPropertyHolder.RequiresPostProcessing then
    Exit(TdxExpandedCriteriaHolder.Indeterminate(AProcessedCollectionPropertyHolder.PostProcessingCause));
  AProcessedCollectionProperty := TdxOperandProperty(AProcessedCollectionPropertyHolder.ExpandedCriteria);
  ANestedUpLevels := GetUpLevels(AProcessedCollectionProperty);
  AProcessedAggregatedExpressionHolder := ProcessInContext(ANestedUpLevels, AOperand.AggregatedExpression);
  if AProcessedAggregatedExpressionHolder.RequiresPostProcessing then
    Exit(TdxExpandedCriteriaHolder.Indeterminate(AProcessedAggregatedExpressionHolder.PostProcessingCause));
  AProcessedConditionHolder := ProcessInContext(ANestedUpLevels, AOperand.Condition);
  if AProcessedConditionHolder.RequiresPostProcessing then
    Exit(TdxExpandedCriteriaHolder.Indeterminate(AProcessedConditionHolder.PostProcessingCause));
  if AProcessedConditionHolder.IsFalse then
    Exit(GetFalseConditionAggregate(AOperand.AggregateFunctionType));
  if AProcessedAggregatedExpressionHolder.ExpandedCriteria = nil then
    AOperator := nil
  else
    AOperator := TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(AProcessedAggregatedExpressionHolder.ExpandedCriteria);
  APersistentOperator := TdxAggregateOperand.Create(AProcessedCollectionProperty,
    AOperator, AOperand.AggregateFunctionType, TdxExpandedCriteriaHolder.IfNeededConvertToLogicalOperator(AProcessedConditionHolder.ExpandedCriteria));
  Result := TdxExpandedCriteriaHolder.Create(APersistentOperator);
end;

class function TdxUnknownCriteriaEliminatorBase.GetFalseConditionAggregate(AAggregate: TdxAggregateFunctionType): TdxExpandedCriteriaHolder;
begin
  case AAggregate of
    TdxAggregateFunctionType.Exists:
      Result := TdxExpandedCriteriaHolder.FalseCriteria;
    TdxAggregateFunctionType.Count:
      Result := TdxExpandedCriteriaHolder.Create(TdxOperandValue.Create(0), True);
    else
      Result := TdxExpandedCriteriaHolder.Create(TdxOperandValue.Create(nil), True);
  end;
end;

function TdxUnknownCriteriaEliminatorBase.DoVisit(const AOperand: IdxJoinOperand): TdxExpandedCriteriaHolder;
var
  ANestedUpLevels: TArray<TdxEntityInfo>;
  AProcessedAggregatedExpressionHolder, AProcessedConditionHolder: TdxExpandedCriteriaHolder;
  APersistentOperator, AAggregatedExpression: IdxCriteriaOperator;
begin
  ANestedUpLevels := GetUpLevels(AOperand.JoinTypeName);
  AProcessedAggregatedExpressionHolder := ProcessInContext(ANestedUpLevels, AOperand.AggregatedExpression);
  if AProcessedAggregatedExpressionHolder.RequiresPostProcessing then
    Exit(TdxExpandedCriteriaHolder.Indeterminate(AProcessedAggregatedExpressionHolder.PostProcessingCause));
  AProcessedConditionHolder := ProcessInContext(ANestedUpLevels, AOperand.Condition);
  if AProcessedConditionHolder.RequiresPostProcessing then
    Exit(TdxExpandedCriteriaHolder.Indeterminate(AProcessedConditionHolder.PostProcessingCause));
  if AProcessedConditionHolder.IsFalse then
    Exit(GetFalseConditionAggregate(AOperand.AggregateFunctionType));
  if AProcessedAggregatedExpressionHolder.ExpandedCriteria = nil then
    AAggregatedExpression := nil
  else
    AAggregatedExpression := TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(AProcessedAggregatedExpressionHolder.ExpandedCriteria);
  APersistentOperator := TdxJoinOperand.Create(ANestedUpLevels[0].FullName,
    TdxExpandedCriteriaHolder.IfNeededConvertToLogicalOperator(AProcessedConditionHolder.ExpandedCriteria),
    AOperand.AggregateFunctionType, AAggregatedExpression);
  Result := TdxExpandedCriteriaHolder.Create(APersistentOperator);
end;

function TdxUnknownCriteriaEliminatorBase.DoVisit(const AOperator: IdxFunctionOperator): TdxExpandedCriteriaHolder;
var
  AResult: TdxFunctionOperator;
  AAllConstants, AAllAreConstants, ACutTail: Boolean;
  AHolderList: TList<TdxExpandedCriteriaHolder>;
  I, AIndex, AResultShift: Integer;
  AOperand: IdxCriteriaOperator;
  AProcessedOp, AFirstHolder: TdxExpandedCriteriaHolder;
begin
  AResult := TdxFunctionOperator.Create(AOperator.OperatorType, []);
  AAllConstants := True;
  AHolderList := TList<TdxExpandedCriteriaHolder>.Create;
  try
    for I := 0 to AOperator.Operands.Count - 1 do
    begin
      AOperand := AOperator.Operands[I];
      AProcessedOp := Process(AOperand as TdxCriteriaOperator);
      if not AProcessedOp.IsConstant then
        AAllConstants := False;
      if AProcessedOp.RequiresPostProcessing then
        Exit(TdxExpandedCriteriaHolder.Indeterminate(AProcessedOp.PostProcessingCause));
      AHolderList.Add(AProcessedOp);
      AResult.Operands.Add(AProcessedOp.ExpandedCriteria);
    end;
    if AResult.OperatorType = TdxFunctionOperatorType.Iif then
    begin
      if (AHolderList.Count < 3) or ((AHolderList.Count mod 2) = 0) then
        raise EArgumentException.Create(sdxFilteringTheIifFunctionOperatorRequiresThree);
      if AHolderList.Count = 3 then
      begin
        if AHolderList[0].IsConstant then
        begin
          AFirstHolder := Process(TdxExpandedCriteriaHolder.IfNeededConvertToLogicalOperator(AResult.Operands[0]) as TdxCriteriaOperator);
          if AFirstHolder.IsTrue then
            Exit(AHolderList[1]);
          if AFirstHolder.IsFalse then
            Exit(AHolderList[2]);
        end;
        TdxCriteriaOperatorCollection(AResult.Operands)[0] := TdxExpandedCriteriaHolder.IfNeededConvertToLogicalOperator(AResult.Operands[0]);
        TdxCriteriaOperatorCollection(AResult.Operands)[1] := TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(AResult.Operands[1]);
        TdxCriteriaOperatorCollection(AResult.Operands)[2] := TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(AResult.Operands[2]);
      end
      else
      begin
        AIndex := -2;
        AResultShift := 0;
        AAllAreConstants := True;
        ACutTail := False;
        AFirstHolder := nil;
        repeat
          if not AFirstHolder.IsEmpty then
          begin
            if AFirstHolder.IsFalse then
            begin
              TdxCriteriaOperatorCollection(AResult.Operands).DeleteRange(AIndex + AResultShift, 2);
              Dec(AResultShift, 2);
            end;
            AFirstHolder := nil;
          end;
          Inc(AIndex, 2);
          if AHolderList[AIndex].IsConstant then
          begin
            AFirstHolder := Process(TdxExpandedCriteriaHolder.IfNeededConvertToLogicalOperator(AResult.Operands[AIndex + AResultShift]) as TdxCriteriaOperator);
            if AFirstHolder.IsTrue then
            begin
              if AAllAreConstants then
                Exit(AHolderList[AIndex + 1]);
              TdxCriteriaOperatorCollection(AResult.Operands)[AIndex + AResultShift] := TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(AResult.Operands[AIndex + AResultShift + 1]);
              TdxCriteriaOperatorCollection(AResult.Operands).DeleteRange(AIndex + AResultShift + 1, AResult.Operands.Count - (AIndex + AResultShift + 1));
              ACutTail := True;
              Break;
            end;
          end
          else
          begin
            AAllAreConstants := False;
            TdxCriteriaOperatorCollection(AResult.Operands)[AIndex + AResultShift] := TdxExpandedCriteriaHolder.IfNeededConvertToLogicalOperator(AResult.Operands[AIndex + AResultShift]);
            TdxCriteriaOperatorCollection(AResult.Operands)[AIndex + AResultShift + 1] := TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(AResult.Operands[AIndex + AResultShift + 1]);
          end;
        until not ((AIndex + 3) < AHolderList.Count);;
        if not ACutTail then
        begin
          if (not AFirstHolder.IsEmpty) and AFirstHolder.IsFalse then
          begin
            if AAllAreConstants then
              Exit(AHolderList[AIndex + 2]);
            TdxCriteriaOperatorCollection(AResult.Operands).DeleteRange(AIndex + AResultShift, 2);
            Dec(AResultShift, 2);
          end;
          TdxCriteriaOperatorCollection(AResult.Operands)[AIndex + AResultShift + 2] := TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(AResult.Operands[AIndex + AResultShift + 2]);
        end;
      end;
    end
    else
    begin
      case AResult.OperatorType of
        TdxFunctionOperatorType.UtcNow,
        TdxFunctionOperatorType.Now,
        TdxFunctionOperatorType.Today,
        TdxFunctionOperatorType.Random,
        TdxFunctionOperatorType.CustomNonDeterministic:
          ;
        TdxFunctionOperatorType.IsNull:
          if AAllConstants then
          begin
            AOperand := Evaluate(AResult);
            Exit(TdxExpandedCriteriaHolder.TryConvertToLogicalConstant(AOperand, True));
          end
          else
            for I := 0 to AResult.Operands.Count - 1 do
              TdxCriteriaOperatorCollection(AResult.Operands)[I] := TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(AResult.Operands[I]);
        TdxFunctionOperatorType.Custom:
          NotImplemented;
        else
          if AAllConstants then
          begin
            AOperand := Evaluate(AResult);
            Exit(TdxExpandedCriteriaHolder.Create(AOperand, True));
          end
          else
            for I := 0 to AResult.Operands.Count - 1 do
              TdxCriteriaOperatorCollection(AResult.Operands)[I] := TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(AResult.Operands[I]);
      end;
    end;
    Result := TdxExpandedCriteriaHolder.Create(AResult);
  finally
    AHolderList.Free;
  end;
end;

function TdxUnknownCriteriaEliminatorBase.DoVisit(const AOperator: IdxGroupOperator): TdxExpandedCriteriaHolder;
const
  UnderNotType: array[Boolean] of TdxGroupOperatorType = (
    TdxGroupOperatorType.Or,
    TdxGroupOperatorType.And
  );
var
  AFalseFound: Boolean;
  ANonFalseCount: Integer;
  AResult, ACriteriaOperator: IdxCriteriaOperator;
  AReqPostProc: string;
  AProcessed: TdxExpandedCriteriaHolder;
begin
  AFalseFound := False;
  ANonFalseCount := 0;
  AResult := nil;
  AReqPostProc := '';
  for ACriteriaOperator in AOperator.Operands do
  begin
    if ACriteriaOperator = nil then
      Continue;
    AProcessed := Process(ACriteriaOperator as TdxCriteriaOperator);
    if AProcessed.RequiresPostProcessing then
      AReqPostProc := AProcessed.PostProcessingCause;
    if AProcessed.IsFalse then
    begin
      AFalseFound := True;
      if AOperator.OperatorType = TdxGroupOperatorType.And then
        Exit(TdxExpandedCriteriaHolder.FalseCriteria);
    end
    else
    begin
      Inc(ANonFalseCount);
      if AProcessed.IsTrue then
      begin
        if not AProcessed.RequiresPostProcessing and (AOperator.OperatorType = TdxGroupOperatorType.Or) then
          Exit(TdxExpandedCriteriaHolder.TrueCriteria);
      end
      else
        AResult := TdxGroupOperator.Combine(AOperator.OperatorType, AResult,
          TdxExpandedCriteriaHolder.IfNeededConvertToLogicalOperator(AProcessed.ExpandedCriteria));
    end;
  end;
  if AFalseFound and (ANonFalseCount = 0) then
    Exit(TdxExpandedCriteriaHolder.FalseCriteria);
  if ((ANonFalseCount > 1) and (AReqPostProc <> '')) and (AOperator.OperatorType = UnderNotType[FUnderNot]) then
    Exit(TdxExpandedCriteriaHolder.Indeterminate(AReqPostProc));
  Result := TdxExpandedCriteriaHolder.Create(AResult, AReqPostProc);
end;

function TdxUnknownCriteriaEliminatorBase.DoVisit(const AOperator: IdxInOperator): TdxExpandedCriteriaHolder;
var
  ALeftProcessed, AProcessedOp: TdxExpandedCriteriaHolder;
  AResult: TdxInOperator;
  AOperands: IdxCriteriaOperatorCollection;
  AReq: string;
  AAllConstants: Boolean;
  ACriteriaOperator: IdxCriteriaOperator;
  I: Integer;
begin
  ALeftProcessed := Process(AOperator.LeftOperand);
  if ALeftProcessed.RequiresPostProcessing then
    Exit(TdxExpandedCriteriaHolder.Indeterminate(ALeftProcessed.PostProcessingCause));
  AResult := TdxInOperator.Create(TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(ALeftProcessed.ExpandedCriteria));
  AOperands := AOperator.Operands;
  AReq := '';
  AAllConstants := True;
  for ACriteriaOperator in AOperands do
  begin
    AProcessedOp := Process(ACriteriaOperator as TdxCriteriaOperator);
    if AProcessedOp.RequiresPostProcessing then
    begin
      AReq := AProcessedOp.PostProcessingCause;
      if not FUnderNot then
        Exit(TdxExpandedCriteriaHolder.Indeterminate(AReq));
    end;
    if not AProcessedOp.IsConstant then
      AAllConstants := False;
    AResult.Operands.Add(AProcessedOp.ExpandedCriteria);
  end;
  if AResult.Operands.Count = 0 then
    if AReq <> '' then
      Exit(TdxExpandedCriteriaHolder.Indeterminate(AReq))
    else
      Exit(TdxExpandedCriteriaHolder.FalseCriteria);

  if ALeftProcessed.IsConstant and AAllConstants then
    Exit(TdxExpandedCriteriaHolder.TryConvertToLogicalConstant(Evaluate(AResult), True))
  else
    for I := 0 to AResult.Operands.Count - 1 do
      TdxCriteriaOperatorCollection(AResult.Operands)[I] := TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(AResult.Operands[I]);
  Result := TdxExpandedCriteriaHolder.Create(AResult, AReq);
end;

function TdxUnknownCriteriaEliminatorBase.DoVisit(const AOperator: IdxUnaryOperator): TdxExpandedCriteriaHolder;
var
  AProcessedOp: TdxExpandedCriteriaHolder;
  AResult: TdxUnaryOperator;
  ACriteriaOperator: IdxCriteriaOperator;
begin
  if AOperator.OperatorType = TdxUnaryOperatorType.Not then
  begin
    FUnderNot := not FUnderNot;
    try
      AProcessedOp := Process(AOperator.Operand as TdxCriteriaOperator);
      if AProcessedOp.IsTrue then
      begin
        if AProcessedOp.RequiresPostProcessing then
          Result := TdxExpandedCriteriaHolder.Indeterminate(AProcessedOp.PostProcessingCause)
        else
          Result := TdxExpandedCriteriaHolder.FalseCriteria;
      end
      else
        if AProcessedOp.IsFalse then
          Result := TdxExpandedCriteriaHolder.TrueCriteria
        else
          Result := TdxExpandedCriteriaHolder.Create(TdxUnaryOperator.Create(TdxUnaryOperatorType.Not,
            TdxExpandedCriteriaHolder.IfNeededConvertToLogicalOperator(AProcessedOp.ExpandedCriteria)),
            AProcessedOp.PostProcessingCause);
    finally
      FUnderNot := not FUnderNot;
    end;
  end
  else
  begin
    AProcessedOp := Process(AOperator.Operand as TdxCriteriaOperator);
    if AProcessedOp.RequiresPostProcessing then
      Exit(TdxExpandedCriteriaHolder.Indeterminate(AProcessedOp.PostProcessingCause));
    AResult := TdxUnaryOperator.Create(AOperator.OperatorType, AProcessedOp.ExpandedCriteria);
    if AProcessedOp.IsConstant then
    begin
      ACriteriaOperator := Evaluate(AResult);
      if AOperator.OperatorType = TdxUnaryOperatorType.IsNull then
        Exit(TdxExpandedCriteriaHolder.TryConvertToLogicalConstant(ACriteriaOperator, True));
      Exit(TdxExpandedCriteriaHolder.Create(ACriteriaOperator, True));
    end;
    AResult.Operand := TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(AResult.Operand);
    Result := TdxExpandedCriteriaHolder.Create(AResult);
  end;
end;

function TdxUnknownCriteriaEliminatorBase.DoVisit(const AOperator: IdxBinaryOperator): TdxExpandedCriteriaHolder;
var
  ALeftProcessed, ARightProcessed: TdxExpandedCriteriaHolder;
  AResult: TdxBinaryOperator;
  ACriteriaOperator: IdxCriteriaOperator;
begin
  ALeftProcessed := Process(AOperator.LeftOperand as TdxCriteriaOperator);
  if ALeftProcessed.RequiresPostProcessing then
    Exit(TdxExpandedCriteriaHolder.Indeterminate(ALeftProcessed.PostProcessingCause));
  ARightProcessed := Process(AOperator.RightOperand as TdxCriteriaOperator);
  if ARightProcessed.RequiresPostProcessing then
    Exit(TdxExpandedCriteriaHolder.Indeterminate(ARightProcessed.PostProcessingCause));
  if (ARightProcessed.IsNullValue) or (ALeftProcessed.IsNullValue) then
  begin
    if AOperator.OperatorType = TdxBinaryOperatorType.NotEqual then
      Exit(TdxExpandedCriteriaHolder.TrueCriteria)
    else
      Exit(TdxExpandedCriteriaHolder.FalseCriteria);
  end;
  AResult := TdxBinaryOperator.Create(ALeftProcessed.ExpandedCriteria, ARightProcessed.ExpandedCriteria, AOperator.OperatorType);
  if AResult.LeftOperand = nil then
    raise EArgumentNilException.Create('LeftOperand: ' + sdxGeneratorOneOfBinaryOperatorsOperandsIsNull);
  if AResult.RightOperand = nil then
    raise EArgumentNilException.Create('RightOperand: ' + sdxGeneratorOneOfBinaryOperatorsOperandsIsNull);
  if ALeftProcessed.IsConstant and ARightProcessed.IsConstant then
  begin
    ACriteriaOperator := Evaluate(AResult);
    case AOperator.OperatorType of
      TdxBinaryOperatorType.Equal,
      TdxBinaryOperatorType.Greater,
      TdxBinaryOperatorType.GreaterOrEqual,
      TdxBinaryOperatorType.Less,
      TdxBinaryOperatorType.LessOrEqual,
      TdxBinaryOperatorType.NotEqual:
        Exit(TdxExpandedCriteriaHolder.TryConvertToLogicalConstant(ACriteriaOperator, True));
    end;
    Exit(TdxExpandedCriteriaHolder.Create(ACriteriaOperator));
  end;
  AResult.LeftOperand := TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(AResult.LeftOperand);
  AResult.RightOperand := TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(AResult.RightOperand);
  Result := TdxExpandedCriteriaHolder.Create(AResult);
end;

function TdxUnknownCriteriaEliminatorBase.DoVisit(const AOperator: IdxBetweenOperator): TdxExpandedCriteriaHolder;
var
  ATestProcessed, ABeginProcessed, AEndProcessed: TdxExpandedCriteriaHolder;
  AResult: TdxBetweenOperator;
begin
  ATestProcessed := Process(AOperator.TestExpression as TdxCriteriaOperator);
  if ATestProcessed.RequiresPostProcessing then
    Exit(TdxExpandedCriteriaHolder.Indeterminate(ATestProcessed.PostProcessingCause));
  ABeginProcessed := Process(AOperator.BeginExpression as TdxCriteriaOperator);
  if ABeginProcessed.RequiresPostProcessing then
    Exit(TdxExpandedCriteriaHolder.Indeterminate(ABeginProcessed.PostProcessingCause));
  AEndProcessed := Process(AOperator.EndExpression as TdxCriteriaOperator);
  if AEndProcessed.RequiresPostProcessing then
    Exit(TdxExpandedCriteriaHolder.Indeterminate(AEndProcessed.PostProcessingCause));
  AResult := TdxBetweenOperator.Create(ATestProcessed.ExpandedCriteria, ABeginProcessed.ExpandedCriteria, AEndProcessed.ExpandedCriteria);
  if (ATestProcessed.IsConstant and ABeginProcessed.IsConstant) and AEndProcessed.IsConstant then
    Exit(TdxExpandedCriteriaHolder.TryConvertToLogicalConstant(Evaluate(AResult), True));
  AResult.BeginExpression := TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(AResult.BeginExpression);
  AResult.EndExpression := TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(AResult.EndExpression);
  AResult.TestExpression := TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(AResult.TestExpression);
  Result := TdxExpandedCriteriaHolder.Create(AResult);
end;

function TdxUnknownCriteriaEliminatorBase.Evaluate(const AOperator: IdxCriteriaOperator): IdxOperandValue;
var
  AEval: TdxExpressionEvaluator;
begin
  AEval := TdxExpressionEvaluator.Create(TdxConstantDescriptor.Instance, AOperator, FCaseSensitive);
  try
    Result := TdxOperandValue.Create(AEval.Evaluate(AOperator as TdxCriteriaOperator));
  finally
    AEval.Free;
  end;
end;

function TdxUnknownCriteriaEliminatorBase.Process(const AOperand: IdxCriteriaOperator): TdxExpandedCriteriaHolder;
begin
  if AOperand = nil then
    Result := TdxExpandedCriteriaHolder.TrueCriteria
  else
    Result := TdxAcceptor.Accept<TdxExpandedCriteriaHolder>(AOperand, Self);
end;

{ TdxPersistentCriterionExpander }

constructor TdxPersistentCriterionExpander.Create(const AUpLevels: TArray<TdxEntityInfo>;
  const APersistentValuesSource: IdxPersistentValueExtractor; AAliasDepthWatchDog: Integer;
  ADoDetectPostProcessing: Boolean = True);
var
  ACaseSensitive: Boolean;
begin
  if APersistentValuesSource = nil then
    ACaseSensitive := TdxEMFDefault.DefaultCaseSensitive
  else
    ACaseSensitive := APersistentValuesSource.CaseSensitive;
  inherited Create(AUpLevels, ACaseSensitive);
  FDoDetectPostProcessing := True;
  FPersistentValuesSource := APersistentValuesSource;
  FAliasDepthWatchDog := AAliasDepthWatchDog;
  FDoDetectPostProcessing := ADoDetectPostProcessing;
end;

function TdxPersistentCriterionExpander.DoVisit(const AOperator: IdxBinaryOperator): TdxExpandedCriteriaHolder;
begin
  Result := inherited DoVisit(AOperator);
end;


class function TdxPersistentCriterionExpander.IsSingle(const ACriteria: IdxCriteriaOperator): Boolean;
begin
  Result := ((ACriteria is TdxJoinOperand) and ((TdxJoinOperand(ACriteria)).AggregateFunctionType = TdxAggregateFunctionType.Single)) or
    ((ACriteria is TdxAggregateOperand) and ((TdxAggregateOperand(ACriteria)).AggregateFunctionType = TdxAggregateFunctionType.Single));
end;

class function TdxPersistentCriterionExpander.IsIif(const ACriteria: IdxCriteriaOperator): Boolean;
begin
  Result := (ACriteria is TdxFunctionOperator) and ((TdxFunctionOperator(ACriteria)).OperatorType = TdxFunctionOperatorType.Iif);
end;

class function TdxPersistentCriterionExpander.IsTwoArgumentIsNull(const ACriteria: IdxCriteriaOperator): Boolean;
var
  AFunc: IdxFunctionOperator;
begin
  if ACriteria is TdxFunctionOperator then
  begin
    AFunc := ACriteria as IdxFunctionOperator;
    Result := ((AFunc <> nil) and (AFunc.OperatorType = TdxFunctionOperatorType.IsNull)) and (AFunc.Operands.Count = 2);
  end
  else
    Result := False;
end;

function TdxPersistentCriterionExpander.ProcessSingleAlias(const AUpLevelsWithReference: TArray<TdxEntityInfo>;
  const AAlias: IdxCriteriaOperator; APath: TdxMemberInfoCollection; AStart: Integer): TdxExpandedCriteriaHolder;
var
  AJoinOperand: TdxJoinOperand;
  AAggregateOperand: TdxAggregateOperand;
  AIsAggregateOperand, AIsJoinOperand: Boolean;
  AExpression: idxCriteriaOperator;
  AUpLevels: TArray<TdxEntityInfo>;
  AExpanded: TdxExpandedCriteriaHolder;
begin
  AJoinOperand := Safe<TdxJoinOperand>.Cast(AAlias as TdxCriteriaOperator);
  AAggregateOperand := Safe<TdxAggregateOperand>.Cast(AAlias as TdxCriteriaOperator);
  AIsAggregateOperand := AAggregateOperand <> nil;
  AIsJoinOperand := AJoinOperand <> nil;
  if ((not AIsAggregateOperand and not AIsJoinOperand)) or
    ((AIsAggregateOperand and (AAggregateOperand.AggregateFunctionType <> TdxAggregateFunctionType.Single))) or
    ((AIsJoinOperand and (AJoinOperand.AggregateFunctionType <> TdxAggregateFunctionType.Single))) then
    raise EArgumentException.Create(sdxAlias);
  AUpLevels := nil;
  if AIsJoinOperand then
  begin
    AUpLevels := GetUpLevels(AUpLevelsWithReference, AJoinOperand.JoinTypeName);
    AExpression := AJoinOperand.AggregatedExpression;
  end
  else
  begin
    AUpLevels := GetUpLevels(AUpLevelsWithReference, TdxEvaluatorProperty.Create(AAggregateOperand.CollectionProperty));
    AExpression := AAggregateOperand.AggregatedExpression;
  end;
  AExpanded := MergeAlias(AUpLevels, APath, AStart, nil, AExpression);
  if AExpanded.RequiresPostProcessing then
    Exit(TdxExpandedCriteriaHolder.Indeterminate(AExpanded.PostProcessingCause));
  if AIsJoinOperand then
    Result := TdxExpandedCriteriaHolder.Create(TdxJoinOperand.Create(AJoinOperand.JoinTypeName, AJoinOperand.Condition,
      TdxAggregateFunctionType.Single, AExpanded.ExpandedCriteria))
  else
    Result := TdxExpandedCriteriaHolder.Create(TdxAggregateOperand.Create(AAggregateOperand.CollectionProperty,
      AExpanded.ExpandedCriteria, TdxAggregateFunctionType.Single, AAggregateOperand.Condition));
end;

function TdxPersistentCriterionExpander.ProcessIifAlias(const AUpLevelsWithReference: TArray<TdxEntityInfo>;
  const AExpression: IdxCriteriaOperator; APath: TdxMemberInfoCollection; AStart: Integer): TdxExpandedCriteriaHolder;
var
  AFunc: TdxFunctionOperator;
  AMi: TdxMappingMemberInfo;
  ALeftOperand, ARightOperand, AOperand: IdxCriteriaOperator;
  ALeftExpanded, ARightExpanded, AExpanded: TdxExpandedCriteriaHolder;
  AOperands: IdxCriteriaOperatorCollection;
  AOperandCount, I: Integer;
begin
  AFunc := Safe<TdxFunctionOperator>.Cast(AExpression  as TdxCriteriaOperator);
  if ((AFunc = nil)) or (AFunc.OperatorType <> TdxFunctionOperatorType.Iif) then
    raise EArgumentException.Create(sdxAlias);
  AMi := APath[AStart];
  if AFunc.Operands.Count = 3 then
  begin
    ALeftOperand := AFunc.Operands[1];
    ARightOperand := AFunc.Operands[2];
    ALeftExpanded := MergeAlias(AUpLevelsWithReference, APath, AStart, AMi, ALeftOperand);
    if ALeftExpanded.RequiresPostProcessing then
      Exit(TdxExpandedCriteriaHolder.Indeterminate(ALeftExpanded.PostProcessingCause));
    ARightExpanded := MergeAlias(AUpLevelsWithReference, APath, AStart, AMi, ARightOperand);
    if ALeftExpanded.RequiresPostProcessing then
      Exit(TdxExpandedCriteriaHolder.Indeterminate(ALeftExpanded.PostProcessingCause));
    Exit(TdxExpandedCriteriaHolder.Create(TdxFunctionOperator.Create(TdxFunctionOperatorType.Iif,
      [AFunc.Operands[0], ALeftExpanded.ExpandedCriteria, ARightExpanded.ExpandedCriteria])));
  end;
  AOperands := TdxCriteriaOperatorCollection.Create;
  AOperandCount := AFunc.Operands.Count;
  for I := 0 to AOperandCount - 1 do
  begin
    AOperand := AFunc.Operands[I];
    if ((I mod 2) = 1) or (I = (AOperandCount - 1)) then
    begin
      AExpanded := MergeAlias(AUpLevelsWithReference, APath, AStart, AMi, AOperand);
      if AExpanded.RequiresPostProcessing then
        Exit(TdxExpandedCriteriaHolder.Indeterminate(AExpanded.PostProcessingCause));
      AOperands.Add(AExpanded.ExpandedCriteria);
      Continue;
    end;
    AOperands.Add(AOperand);
  end;
  Result := TdxExpandedCriteriaHolder.Create(TdxFunctionOperator.Create(TdxFunctionOperatorType.Iif, AOperands));
end;

function TdxPersistentCriterionExpander.ProcessTwoArgumentIsNullAlias(const AUpLevelsWithReference: TArray<TdxEntityInfo>;
  const AExpression: IdxCriteriaOperator; APath: TdxMemberInfoCollection; AStart: Integer): TdxExpandedCriteriaHolder;
var
  AFunc: TdxFunctionOperator;
  AMi: TdxMappingMemberInfo;
  ALeftOperand, ARightOperand: IdxCriteriaOperator;
  ALeftExpanded, ARightExpanded: TdxExpandedCriteriaHolder;
begin
  AFunc := Safe<TdxFunctionOperator>.Cast(AExpression as TdxCriteriaOperator);
  if ((AFunc = nil)) or (AFunc.OperatorType <> TdxFunctionOperatorType.IsNull) or (AFunc.Operands.Count <> 2) then
    raise EArgumentException.Create(sdxAlias);
  AMi := APath[AStart];
  ALeftOperand := AFunc.Operands[0];
  ARightOperand := AFunc.Operands[1];
  ALeftExpanded := MergeAlias(AUpLevelsWithReference, APath, AStart, AMi, ALeftOperand);
  if ALeftExpanded.RequiresPostProcessing then
    Exit(TdxExpandedCriteriaHolder.Indeterminate(ALeftExpanded.PostProcessingCause));
  ARightExpanded := MergeAlias(AUpLevelsWithReference, APath, AStart, AMi, ARightOperand);
  if ALeftExpanded.RequiresPostProcessing then
    Exit(TdxExpandedCriteriaHolder.Indeterminate(ALeftExpanded.PostProcessingCause));
  Result := TdxExpandedCriteriaHolder.Create(TdxFunctionOperator.Create(TdxFunctionOperatorType.IsNull,
    [ALeftExpanded.ExpandedCriteria, ARightExpanded.ExpandedCriteria]));
end;

function TdxPersistentCriterionExpander.MergeAlias(const AUpLevelsWithReference: TArray<TdxEntityInfo>; APath: TdxMemberInfoCollection;
  AStart: Integer; AMemberInfo: TdxMappingMemberInfo; const AExpression: IdxCriteriaOperator): TdxExpandedCriteriaHolder;
var
  AAliasAsProperty: TdxOperandProperty;
  AExpanded: TdxExpandedCriteriaHolder;
  AAliasedProperty: TdxMemberInfoCollection;
  J, ALastPropIndex: Integer;
  AProp, APathJ: TdxMappingMemberInfo;
  AEntityInfo: TdxEntityInfo;
  ANewExpression: IdxCriteriaOperator;
begin
  AAliasAsProperty := Safe<TdxOperandProperty>.Cast(AExpression as TdxCriteriaOperator);
  if (AStart < APath.Count - 1) or (AAliasAsProperty <> nil) then
  begin
    if IsSingle(AExpression as TdxCriteriaOperator) then
    begin
      AExpanded := ProcessSingleAlias(AUpLevelsWithReference, AExpression, APath, AStart);
      if AExpanded.RequiresPostProcessing then
        Exit(TdxExpandedCriteriaHolder.Indeterminate(AExpanded.PostProcessingCause));
      ANewExpression := AExpanded.ExpandedCriteria;
    end
    else
      if IsIif(AExpression as TdxCriteriaOperator) then
      begin
        AExpanded := ProcessIifAlias(AUpLevelsWithReference, AExpression, APath, AStart);
        if AExpanded.RequiresPostProcessing then
          Exit(TdxExpandedCriteriaHolder.Indeterminate(AExpanded.PostProcessingCause));
        ANewExpression := AExpanded.ExpandedCriteria;
      end
      else
        if IsTwoArgumentIsNull(AExpression as TdxCriteriaOperator) then
        begin
          AExpanded := ProcessTwoArgumentIsNullAlias(AUpLevelsWithReference, AExpression, APath, AStart);
          if AExpanded.RequiresPostProcessing then
            Exit(TdxExpandedCriteriaHolder.Indeterminate(AExpanded.PostProcessingCause));
          ANewExpression := AExpanded.ExpandedCriteria;
        end
        else
        begin
          if AAliasAsProperty = nil then
            raise EArgumentException.CreateFmt(sdxPersistentAliasExpanderReferenceOrCollectionExpectedInTheMiddleOfThePath,
              [AMemberInfo.MemberName]);
          AAliasedProperty := TdxMemberInfoCollection.Create(AUpLevelsWithReference[0]);
          if AMemberInfo = nil then
            AEntityInfo := AUpLevelsWithReference[0]
          else
            AEntityInfo := AMemberInfo.Owner;
          AAliasedProperty.AddRange(TdxMemberInfoCollection.ParsePath(AEntityInfo, AAliasAsProperty.PropertyName));
          for J := AStart + 1 to APath.Count - 1 do
          begin
            AEntityInfo := nil;
            ALastPropIndex := AAliasedProperty.Count - 1;
            AProp := AAliasedProperty[ALastPropIndex];
            while TdxEvaluatorProperty.GetIsThisProperty(AProp.MemberName) do
            begin
              AAliasedProperty.Delete(ALastPropIndex);
              if ALastPropIndex = 0 then
              begin
                AEntityInfo := AUpLevelsWithReference[0];
                Break;
              end;
              if ALastPropIndex >= AAliasedProperty.Count then
                ALastPropIndex := AAliasedProperty.Count - 1;
              AProp := AAliasedProperty[ALastPropIndex];
            end;
            if AEntityInfo = nil then
              AEntityInfo := AProp.ReferenceType;
            APathJ := APath[J];
            if APathJ.Owner.IsAssignableTo(AEntityInfo) or AEntityInfo.IsAssignableTo(APathJ.Owner) then
              AAliasedProperty.Add(APath[J])
            else
              AAliasedProperty.Add(AEntityInfo.FindMember(APathJ.MemberName));
          end;
          ANewExpression := TdxOperandProperty.Create(AAliasedProperty.ToString);
        end;
  end;
  Result := TdxExpandedCriteriaHolder.Create(ANewExpression);
end;

function TdxPersistentCriterionExpander.DoVisit(const AOriginalOperand: IdxOperandProperty): TdxExpandedCriteriaHolder;
var
  AAddKeyTail: Boolean;
  AOperand: IdxOperandProperty;
  AProp: TdxEvaluatorProperty;
  APatchedPropertyName: TStringBuilder;
  I: Integer;
  ARes, APath: TdxMemberInfoCollection;
  ACurrentEntityInfo: TdxEntityInfo;
  AUpLevelList: TList<TdxEntityInfo>;
  AMemberInfo: TdxMappingMemberInfo;
begin
  AOperand := FixPropertyExclamation(AOriginalOperand, AAddKeyTail);
  AProp := TdxEvaluatorProperty.Create(AOperand);
  if FDoDetectPostProcessing and TdxEvaluatorProperty.GetIsThisProperty(AProp.PropertyPath) then
  begin
    APatchedPropertyName := TStringBuilder.Create;
    try
      for I := 0 to AProp.UpDepth - 1 do
        APatchedPropertyName.Append('^.');
      UpLevelsClassInfos[AProp.UpDepth].CheckAbstractReference;
      APatchedPropertyName.Append(UpLevelsClassInfos[AProp.UpDepth].KeyProperty.Member.MemberName);
      AProp := TdxEvaluatorProperty.Create(TdxOperandProperty.Create(APatchedPropertyName.ToString));
    finally
      APatchedPropertyName.Free;
    end;
  end;
  ARes := TdxMemberInfoCollection.Create(UpLevelsClassInfos[AProp.UpDepth]);
  for I := 0 to AProp.UpDepth - 1 do
  begin
    ARes.Add(nil);
  end;
  ACurrentEntityInfo := UpLevelsClassInfos[AProp.UpDepth];
  APath := TdxMemberInfoCollection.ParsePath(ACurrentEntityInfo, AProp.PropertyPath);
  if (AAddKeyTail and (APath.Count > 0)) and (APath[0].ReferenceType <> nil) then
    APath := TdxMemberInfoCollection.ParsePath(ACurrentEntityInfo, Concat(AProp.PropertyPath, '.', APath[0].ReferenceType.KeyProperty.Member.MemberName));
  AUpLevelList := TList<TdxEntityInfo>.Create;
  AUpLevelList.Capacity := Length(UpLevelsClassInfos) - AProp.UpDepth;
  for I := AProp.UpDepth to Length(UpLevelsClassInfos) - 1 do
    AUpLevelList.Add(UpLevelsClassInfos[I]);
  for I := 0 to APath.Count - 1 do
  begin
    AMemberInfo := APath[I];
    if AMemberInfo.IsAliased then
    begin
      NotImplemented;
    end
    else
      if IsValidForPersistentCriterion(AMemberInfo) then
      begin
        ARes.Add(AMemberInfo);
        ACurrentEntityInfo := AMemberInfo.ReferenceType;
        AUpLevelList.Insert(0, ACurrentEntityInfo);
      end
      else
        if AMemberInfo.IsAssociationList and (I = APath.Count - 1) then
        begin
          ARes.Add(AMemberInfo);
        end
        else
          begin
            if FDoDetectPostProcessing then
              Exit(TdxExpandedCriteriaHolder.Indeterminate(AOriginalOperand))
            else
            begin
              ARes.Add(AMemberInfo);
            end;
          end;
  end;
  Result := TdxExpandedCriteriaHolder.Create(TdxOperandProperty.Create(ARes.ToString));
end;

class function TdxPersistentCriterionExpander.FixPropertyExclamation(const AOriginalOperand: IdxOperandProperty; out AAddKeyTail: Boolean): IdxOperandProperty;
var
  AOperand: IdxOperandProperty;
begin
  AOperand := AOriginalOperand;
  AAddKeyTail := False;
  {$IFDEF DELPHIXE3}
  if AOperand.PropertyName.EndsWith(TdxEMFPropertyDescriptor.ReferenceAsKeyTail) then
  {$ELSE}
  if TdxStringHelper.EndsWith(AOperand.PropertyName, TdxEMFPropertyDescriptor.ReferenceAsKeyTail) then
  {$ENDIF}
  begin
    {$IFDEF DELPHIXE3}
    AOperand := TdxOperandProperty.Create(AOperand.PropertyName.Substring(0, Length(AOperand.PropertyName) - Length(TdxEMFPropertyDescriptor.ReferenceAsKeyTail)));
    {$ELSE}
    AOperand := TdxOperandProperty.Create(TdxStringHelper.Substring(AOperand.PropertyName, 0, Length(AOperand.PropertyName) - Length(TdxEMFPropertyDescriptor.ReferenceAsKeyTail)));
    {$ENDIF}
    AAddKeyTail := True;
  end
  else
    {$IFDEF DELPHIXE3}
    if AOperand.PropertyName.EndsWith(TdxEMFPropertyDescriptor.ReferenceAsObjectTail) then
    {$ELSE}
    if TdxStringHelper.EndsWith(AOperand.PropertyName, TdxEMFPropertyDescriptor.ReferenceAsObjectTail) then
    {$ENDIF}
      AOperand := TdxOperandProperty.Create({$IFDEF DELPHIXE3}AOperand.PropertyName.Substring(0,{$ELSE}TdxStringHelper.Substring(AOperand.PropertyName, 0,{$ENDIF}
        Length(AOperand.PropertyName) - Length(TdxEMFPropertyDescriptor.ReferenceAsObjectTail)));
  Result := AOperand;
end;

function TdxPersistentCriterionExpander.DoVisit(const AOperand: IdxAggregateOperand): TdxExpandedCriteriaHolder;
begin
  Result := inherited DoVisit(AOperand);
end;


function TdxPersistentCriterionExpander.IsValidForPersistentCriterion(AMi: TdxMappingMemberInfo): Boolean;
begin
  Result := (AMi.IsPersistent) or (AMi.IsAssociationList);
end;

function TdxPersistentCriterionExpander.DoVisit(const AOperand: IdxOperandValue): TdxExpandedCriteriaHolder;
var
  AExtracted: TValue;
  AValue: IdxOperandValue;
begin
  if FDoDetectPostProcessing then
    AExtracted := FPersistentValuesSource.ExtractPersistentValue(AOperand.Value)
  else
    AExtracted := AOperand.Value;
  if AExtracted.Equals(AOperand.Value) then
    AValue := AOperand
  else
    if AOperand is TdxConstantValue then
      AValue := TdxConstantValue.Create(AExtracted)
    else
      AValue := TdxOperandValue.Create(AExtracted);
  Result := TdxExpandedCriteriaHolder.Create(AValue, True);
end;

function TdxPersistentCriterionExpander.ProcessInContext(const AUpLevels: TArray<TdxEntityInfo>;
  const AOperand: IdxCriteriaOperator): TdxExpandedCriteriaHolder;
begin
  Result := Expand(AUpLevels, FPersistentValuesSource, AOperand, FAliasDepthWatchDog + 1, FDoDetectPostProcessing);
end;

class function TdxPersistentCriterionExpander.Expand(const AUpLevels: TArray<TdxEntityInfo>; const APersistentValuesSource: IdxPersistentValueExtractor;
  const AOperator: IdxCriteriaOperator; AAliasDepthWatchDog: Integer; ADoDetectPostProcessing: Boolean): TdxExpandedCriteriaHolder;
begin
  Result := TdxPersistentCriterionExpander.Create(AUpLevels, APersistentValuesSource, AAliasDepthWatchDog, ADoDetectPostProcessing).Process(AOperator);
end;

class function TdxPersistentCriterionExpander.Expand(const AUpLevels: TArray<TdxEntityInfo>;
  const APersistentValuesSource: IdxPersistentValueExtractor; const AOperator: IdxCriteriaOperator;
  ADoDetectPostProcessing: Boolean): TdxExpandedCriteriaHolder;
var
  AResult: TdxExpandedCriteriaHolder;
  AClassInfos, AMessage: string;
  AEntityInfo: TdxEntityInfo;
begin
  AResult := Expand(AUpLevels, APersistentValuesSource, AOperator, 0, ADoDetectPostProcessing);
  if AResult.RequiresPostProcessing and (FPersistentCriterionExpanderRequiresPostProcessingAction <>
    TdxPersistentCriterionExpanderRequiresPostProcessingAction.None) then
  begin
    AClassInfos := '';
    for AEntityInfo in AUpLevels do
    begin
      if Length(AClassInfos) > 0 then
        AClassInfos := AClassInfos + ',';
      AClassInfos := AClassInfos + AEntityInfo.FullName;
    end;
    AMessage := Format('Expanding '#$27'{0}'#$27' criterion for '#$27'{1}'#$27' classInfo(s) PostProcessing was ' +
      'requested because it uses a NonPersistent (and/or modified for NestedUnitOfWork) property '#$27'{2}'#$27,
      [(AOperator as TObject).ClassName, AClassInfos, AResult.PostProcessingCause]);
    case FPersistentCriterionExpanderRequiresPostProcessingAction of
      TdxPersistentCriterionExpanderRequiresPostProcessingAction.ThrowException:
        raise EArgumentException.Create(AMessage);
    end;
    Result := AResult;
  end
  else
    Result := TdxExpandedCriteriaHolder.Create(TdxJoinOperandExpander.Expand(AUpLevels, AResult.ExpandedCriteria),
      AResult.PostProcessingCause, AResult.IsConstant);
end;

class function TdxPersistentCriterionExpander.ExpandToLogical(AEntityInfo: TdxEntityInfo;
  const APersistentValuesSource: IdxPersistentValueExtractor; const AOperator: IdxCriteriaOperator;
  ADoDetectPostProcessing: Boolean): TdxExpandedCriteriaHolder;
begin
  Result := TdxExpandedCriteriaHolder.IfNeededConvertToLogicalHolder(Expand(AEntityInfo, APersistentValuesSource, AOperator, ADoDetectPostProcessing));
end;

class function TdxPersistentCriterionExpander.ExpandToLogical(const APersistentValuesSource: IdxPersistentValueExtractor; AEntityInfo: TdxEntityInfo; const AOperator: IdxCriteriaOperator; ADoDetectPostProcessing: Boolean): TdxExpandedCriteriaHolder;
begin
  Result := TdxExpandedCriteriaHolder.IfNeededConvertToLogicalHolder(Expand(APersistentValuesSource, AEntityInfo, AOperator, ADoDetectPostProcessing));
end;

class function TdxPersistentCriterionExpander.ExpandToValue(AEntityInfo: TdxEntityInfo; const APersistentValuesSource: IdxPersistentValueExtractor; const AOperator: IdxCriteriaOperator; ADoDetectPostProcessing: Boolean): TdxExpandedCriteriaHolder;
begin
  Result := TdxExpandedCriteriaHolder.IfNeededConvertToBoolHolder(Expand(AEntityInfo, APersistentValuesSource, AOperator, ADoDetectPostProcessing));
end;

class function TdxPersistentCriterionExpander.ExpandToValue(const APersistentValuesSource: IdxPersistentValueExtractor; AEntityInfo: TdxEntityInfo; const AOperator: IdxCriteriaOperator; ADoDetectPostProcessing: Boolean): TdxExpandedCriteriaHolder;
begin
  Result := TdxExpandedCriteriaHolder.IfNeededConvertToBoolHolder(Expand(APersistentValuesSource, AEntityInfo, AOperator, ADoDetectPostProcessing));
end;

class function TdxPersistentCriterionExpander.Expand(AEntityInfo: TdxEntityInfo; const APersistentValuesSource: IdxPersistentValueExtractor; const AOperator: IdxCriteriaOperator): TdxExpandedCriteriaHolder;
begin
  Result := Expand(AEntityInfo, APersistentValuesSource, AOperator, True);
end;

class function TdxPersistentCriterionExpander.Expand(AEntityInfo: TdxEntityInfo; const APersistentValuesSource: IdxPersistentValueExtractor; const AOperator: IdxCriteriaOperator; ADoDetectPostProcessing: Boolean): TdxExpandedCriteriaHolder;
begin
  if AEntityInfo = nil then
    raise EArgumentNilException.Create('AEntityInfo');
  Result := Expand(TArray<TdxEntityInfo>.Create(AEntityInfo), APersistentValuesSource, AOperator, ADoDetectPostProcessing);
end;

class function TdxPersistentCriterionExpander.Expand(const APersistentValuesSource: IdxPersistentValueExtractor; AEntityInfo: TdxEntityInfo; const AOperator: IdxCriteriaOperator): TdxExpandedCriteriaHolder;
begin
  Result := Expand(APersistentValuesSource, AEntityInfo, AOperator, True);
end;

class function TdxPersistentCriterionExpander.Expand(const APersistentValuesSource: IdxPersistentValueExtractor; AEntityInfo: TdxEntityInfo; const AOperator: IdxCriteriaOperator; ADoDetectPostProcessing: Boolean): TdxExpandedCriteriaHolder;
begin
  Result := Expand(AEntityInfo, APersistentValuesSource, AOperator, ADoDetectPostProcessing);
end;

function TdxPersistentCriterionExpander.DoVisit(const AOperator: IdxFunctionOperator): TdxExpandedCriteriaHolder;
begin
  case AOperator.OperatorType of
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
    TdxFunctionOperatorType.LocalDateTimeNextMonth,
    TdxFunctionOperatorType.LocalDateTimeNextYear:
      Result := Process(TdxOperandValue.Create(TdxEvalHelpers.EvaluateLocalDateTime(AOperator.OperatorType)));
    TdxFunctionOperatorType.IsOutlookIntervalBeyondThisYear,
    TdxFunctionOperatorType.IsOutlookIntervalLaterThisYear,
    TdxFunctionOperatorType.IsOutlookIntervalLaterThisMonth,
    TdxFunctionOperatorType.IsOutlookIntervalLaterThisWeek,
    TdxFunctionOperatorType.IsOutlookIntervalNextWeek,
    TdxFunctionOperatorType.IsOutlookIntervalTomorrow,
    TdxFunctionOperatorType.IsOutlookIntervalToday,
    TdxFunctionOperatorType.IsOutlookIntervalYesterday,
    TdxFunctionOperatorType.IsOutlookIntervalEarlierThisWeek,
    TdxFunctionOperatorType.IsOutlookIntervalLastWeek,
    TdxFunctionOperatorType.IsOutlookIntervalEarlierThisMonth,
    TdxFunctionOperatorType.IsOutlookIntervalEarlierThisYear,
    TdxFunctionOperatorType.IsOutlookIntervalPriorThisYear:
      Result := Process(TdxEvalHelpers.ExpandIsOutlookInterval(AOperator)  as TdxCriteriaOperator);
    else
      Result := inherited DoVisit(AOperator);
  end;
end;


end.
