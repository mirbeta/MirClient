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

unit dxEMF.Core.Helpers;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  Classes, SysUtils,
  RTTI, Generics.Defaults, Generics.Collections,
  dxEMF.Metadata,
  dxEMF.DB.Generator,
  dxEMF.DB.Query,
  dxEMF.DB.SQLConnectionProvider;

type

  TdxSessionState = (
    GetObjectsNonReentrant,
    BeginTransaction,
    CommitTransactionNonReentrant,
    CommitChangesToDataLayer,
    RollbackTransaction,
    LoadingObjectsIntoNestedUow,
    ReceivingObjectsFromNestedUow,
    CancelEdit,
    CreateObjectLoadingEnforcer,
    OptimisticLockFieldsProcessing,
    ApplyIdentities,
    SessionObjectsToSaveOrDeleteChanging,
    CommitChangesToDataLayerInner,
    CrossThreadFailure
  );

  TdxSessionStates = set of TdxSessionState;

  { TdxSessionStateStack }

  TdxSessionStateStack = class
  strict private
    class var
      FSuppressCrossThreadFailureDetection: Boolean;
    const
      LoadingStates =
       [TdxSessionState.GetObjectsNonReentrant,
        TdxSessionState.ReceivingObjectsFromNestedUow,
        TdxSessionState.CancelEdit,
        TdxSessionState.CreateObjectLoadingEnforcer,
        TdxSessionState.OptimisticLockFieldsProcessing,
        TdxSessionState.ApplyIdentities];
      SavingStates =
       [TdxSessionState.CommitTransactionNonReentrant,
        TdxSessionState.LoadingObjectsIntoNestedUow,
        TdxSessionState.OptimisticLockFieldsProcessing];
      ProhibitedForGetObjects =
       [TdxSessionState.BeginTransaction,
        TdxSessionState.RollbackTransaction,
        TdxSessionState.CommitChangesToDataLayer,
        TdxSessionState.GetObjectsNonReentrant,
        TdxSessionState.CancelEdit,
        TdxSessionState.CreateObjectLoadingEnforcer,
        TdxSessionState.OptimisticLockFieldsProcessing];
      ProhibitedForCancelEdit = ProhibitedForGetObjects + [TdxSessionState.LoadingObjectsIntoNestedUow];
      ProhibitedGeneric = [TdxSessionState.BeginTransaction, TdxSessionState.RollbackTransaction] + SavingStates + LoadingStates;
      ProhibitedSessionObjectsToSaveOrDeleteChanging =
       [TdxSessionState.CommitChangesToDataLayer,
        TdxSessionState.GetObjectsNonReentrant,
        TdxSessionState.LoadingObjectsIntoNestedUow];
  private
    class function AsString(AStates: TdxSessionStates): string; overload; static;
    class function AsString(AState: TdxSessionState): string; overload; static;
  public
    class procedure ThrowIfCannotEnter(ASession: TComponent; ANewState: TdxSessionState); static;
    class procedure Enter(ASession: TComponent; ANewState: TdxSessionState); static;
    class procedure Leave(ASession: TComponent; AState: TdxSessionState); static;
    class function IsInAnyOf(ASession: TComponent; AStates: TdxSessionStates): Boolean; static;

    class property SuppressCrossThreadFailureDetection: Boolean read FSuppressCrossThreadFailureDetection write FSuppressCrossThreadFailureDetection;
  end;

  { TdxProcessingSave }

  TdxProcessingSave = class
  strict private type
    TInsertUpdate = record
    strict private
      FTheObject: TObject;
      FUpdateMembers: TArray<TdxMappingMemberInfo>;
    public
      constructor Create(AObject: TObject; const AUpdateMembers: TArray<TdxMappingMemberInfo>);
      property TheObject: TObject read FTheObject;
      property UpdateMembers: TArray<TdxMappingMemberInfo> read FUpdateMembers;
    end;
  strict private
    FProcessedObjectList: TArray<TObject>;
    FSession: TComponent;
    FInsertList: TList<TObject>;
    FBatchWideData: TdxBatchWideDataHolderForModification;
    procedure InternalInsertObject(AInsertList: TList<TdxModificationStatement>; AUpdateList: TList<TInsertUpdate>; AObject: TObject);
    procedure InternalUpdateObject(AUpdateList: TList<TdxModificationStatement>; AObject: TObject);
  protected
    property Session: TComponent read FSession;
    property BatchWideData: TdxBatchWideDataHolderForModification read FBatchWideData;
  public
    constructor Create(ASession: TComponent; ABatchWideData: TdxBatchWideDataHolderForModification);
    destructor Destroy; override;

    function Process: TList<TdxModificationStatement>;
    procedure ProcessResults(AResult: TdxModificationResult);
  end;

  { TdxObjectRecord }

  TdxObjectRecord = class






  end;

  IdxObjectChange = interface
  end;

  TdxObjectChangeEventHandler = TNotifyEvent;

  { TdxSessionIdentityMap }


implementation

uses
  TypInfo,
  dxCore,
  dxEMF.Utils,
  dxEMF.Types,
  dxEMF.Core,
  dxEMF.DB.Criteria,
  dxEMF.Strs,
  dxEMF.Utils.Exceptions;

type
  TdxEMFSessionOptionsAccess = class(TdxEMFSessionOptions);
  TdxEMFCustomSessionAccess = class(TdxEMFCustomSession);

  { TdxSaveOrderer }

  TdxSaveOrderer = class
  strict private
    FSession: TdxEMFCustomSession;
    FReferredBy: TObjectDictionary<TObject, TdxObjectSet>;
    FReferences: TObjectDictionary<TObject, TdxObjectSet>;
    FResult: TList<TObject>;
    FNonReferencedObjects: TList<TObject>;
  protected
    procedure &Do;
    property Result: TList<TObject> read FResult;
    property ReferredBy: TObjectDictionary<TObject, TdxObjectSet> read FReferredBy;
    property References: TObjectDictionary<TObject, TdxObjectSet> read FReferences;
    property NonReferencedObjects: TList<TObject> read FNonReferencedObjects;
  public
    constructor Create(ASession: TdxEMFCustomSession; ANewObjectsToSave: TList<TObject>);
    destructor Destroy; override;

    procedure MarkObjectAsSaved(AObject: TObject);
    procedure ProcessGoodObjects;
    procedure EliminateLoop(ANextObject: TObject);
    class function DoOrder(ASession: TdxEMFCustomSession; ANewObjectsToSave: TList<TObject>): TArray<TObject>; static;
    class function PrepareSaveOrder(ASession: TdxEMFCustomSession): TArray<TObject>; static;
  end;






{ TdxSessionStateStack }

class procedure TdxSessionStateStack.ThrowIfCannotEnter(ASession: TComponent; ANewState: TdxSessionState);
var
  AProhibited: TdxSessionStates;
  AMessage: string;
begin
  Assert(ASession is TdxEMFCustomSession);
  case ANewState of
    TdxSessionState.BeginTransaction,
    TdxSessionState.CommitTransactionNonReentrant,
    TdxSessionState.RollbackTransaction,
    TdxSessionState.LoadingObjectsIntoNestedUow,
    TdxSessionState.ReceivingObjectsFromNestedUow:
      AProhibited := ProhibitedGeneric;
    TdxSessionState.GetObjectsNonReentrant:
      AProhibited := ProhibitedForGetObjects;
    TdxSessionState.CancelEdit:
      AProhibited := ProhibitedForCancelEdit;
    TdxSessionState.CommitChangesToDataLayer,
    TdxSessionState.CommitChangesToDataLayerInner,
    TdxSessionState.CreateObjectLoadingEnforcer,
    TdxSessionState.OptimisticLockFieldsProcessing,
    TdxSessionState.ApplyIdentities:
      AProhibited := LoadingStates;
    TdxSessionState.SessionObjectsToSaveOrDeleteChanging:
      AProhibited := ProhibitedSessionObjectsToSaveOrDeleteChanging;
    else
      raise EArgumentException.Create('');
  end;
  AProhibited := AProhibited + [ANewState, TdxSessionState.CrossThreadFailure];
  if (TdxEMFCustomSessionAccess(ASession).FStateStack * AProhibited) <> [] then
  begin
    AMessage := Format(sdxSessionEnteringTheX0StateFromTheX1StateIsProhibit,
      [AsString(ANewState),
      AsString(TdxEMFCustomSessionAccess(ASession).FStateStack),
      AsString(TdxEMFCustomSessionAccess(ASession).FStateStack * AProhibited),
      ASession.Name]);
    if ANewState = TdxSessionState.GetObjectsNonReentrant then
    begin
      if IsInAnyOf(ASession, LoadingStates) then
        AMessage := AMessage + sdxSessionMostProbablyYouAreTryingToInitiateAnObjectEx
      else
        if IsInAnyOf(ASession, SavingStates) then
          AMessage := AMessage + sdxSessionMostProbablyYouAreTryingToInitiateAnObject;
    end;
    raise EInvalidOperation.Create(AMessage);
  end;
end;

class function TdxSessionStateStack.AsString(AStates: TdxSessionStates): string;
var
  AStateInt: Integer absolute AStates;
begin
  Result := SetToString(PTypeInfo(TypeInfo(TdxSessionStates)), AStateInt);
end;

class function TdxSessionStateStack.AsString(AState: TdxSessionState): string;
begin
  Result := GetEnumName(TypeInfo(TdxSessionState), Ord(AState));
end;

class procedure TdxSessionStateStack.Enter(ASession: TComponent; ANewState: TdxSessionState);
begin
  ThrowIfCannotEnter(ASession, ANewState);
  Include(TdxEMFCustomSessionAccess(ASession).FStateStack, ANewState);
end;

class procedure TdxSessionStateStack.Leave(ASession: TComponent; AState: TdxSessionState);
begin
  Assert(AState in TdxEMFCustomSessionAccess(ASession).FStateStack);
  Exclude(TdxEMFCustomSessionAccess(ASession).FStateStack, AState);
end;

class function TdxSessionStateStack.IsInAnyOf(ASession: TComponent; AStates: TdxSessionStates): Boolean;
begin
  Result := (TdxEMFCustomSessionAccess(ASession).FStateStack * AStates) <> [];
end;

{ TdxProcessingSave }

constructor TdxProcessingSave.Create(ASession: TComponent; ABatchWideData: TdxBatchWideDataHolderForModification);
begin
  inherited Create;
  FInsertList := TList<TObject>.Create;
  FSession := ASession;
  FBatchWideData := ABatchWideData;
  FProcessedObjectList := TdxSaveOrderer.PrepareSaveOrder(TdxEMFCustomSession(ASession));
end;

destructor TdxProcessingSave.Destroy;
begin
  FreeAndNil(FInsertList);
  inherited Destroy;
end;

procedure TdxProcessingSave.InternalInsertObject(AInsertList: TList<TdxModificationStatement>;
  AUpdateList: TList<TInsertUpdate>; AObject: TObject);
var
  AEntityInfo: TdxEntityInfo;
  AOptimisticLock, AKey, AUpdateMember: TdxMappingMemberInfo;
  AInsertMembers, AUpdateMembers: TdxMemberInfoCollection;
  AReferredObject: TObject;
  AKeyValue: TValue;
  AQueries: TList<TdxModificationStatement>;
  ARootQuery: TdxInsertStatement;
  ASession: TdxEMFCustomSession;
begin
  ASession := TdxEMFCustomSession(Session);
  AEntityInfo := EntityManager.GetEntityInfo(AObject);
  AOptimisticLock := AEntityInfo.OptimisticLockField;
  if (AOptimisticLock <> nil) and AOptimisticLock.GetValue(AObject).IsEmpty then
    AOptimisticLock.SetValue(AObject, 0);
  AKey := AEntityInfo.KeyProperty.Member;
  AInsertMembers := TdxMemberInfoCollection.Create(AEntityInfo);
  try
    AUpdateMembers := TdxMemberInfoCollection.Create(AEntityInfo);
    try
      for AUpdateMember in AEntityInfo.GetPropertiesListForUpdateInsert(AObject, False, False) do
      begin
        if (AKey.MappingField = AUpdateMember.MappingField) and (AKey <> AUpdateMember) then
          Continue;
        if AUpdateMember.ReferenceType <> nil then
        begin
          AReferredObject := AUpdateMember.GetValue(AObject).AsObject;
          if (AReferredObject <> nil) and ASession.IsNewObject(AReferredObject) and
            not FBatchWideData.IsObjectAlreadyInserted(AReferredObject) then
            AUpdateMembers.Add(AUpdateMember)
          else
            AInsertMembers.Add(AUpdateMember);
        end
        else
          AInsertMembers.Add(AUpdateMember);
      end;
      if AKey.IsAutoGenerate then
      begin
        if AKey.IsType(TypeInfo(TGuid)) then
        begin
          AKeyValue := AKey.GetValue(AObject);
          if AKeyValue.IsEmpty or (AKeyValue.AsGUID = TGUID.Empty) then
            AKey.SetValue(AObject, TValue.From<TGUID>(AKey.DBGenerator.NewGuid));
        end
        else
          if not AKey.IsIdentity then
            raise EdxKeysAutogenerationNonSupportedTypeException.Create(AEntityInfo.FullName);
      end
      else
        TdxEMFCustomSessionAccess(ASession).CheckDuplicateObjectInIdentityMap(AObject);
      FBatchWideData.RegisterInsertedObject(AObject);
      AQueries := TdxInsertQueryGenerator.GenerateInsert(FBatchWideData, AObject, AInsertMembers.ToArray);
      try
        ARootQuery := TdxInsertStatement(AQueries[0]);
        if ARootQuery.IdentityParameter <> nil then
          FInsertList.Add(AObject);
        AInsertList.AddRange(AQueries);
        if AUpdateMembers.Count > 0 then
          AUpdateList.Add(TInsertUpdate.Create(AObject, AUpdateMembers.ToArray));
      finally
        AQueries.Free;
      end;
    finally
      AUpdateMembers.Free;
    end;
  finally
    AInsertMembers.Free;
  end;
end;

procedure TdxProcessingSave.InternalUpdateObject(AUpdateList: TList<TdxModificationStatement>; AObject: TObject);
var
  AEntityInfo: TdxEntityInfo;
  AOptimisticLock: TdxMappingMemberInfo;
  APropsList: TdxMemberInfoCollection;
  ASession: TdxEMFCustomSession;
  AStatement: TList<TdxModificationStatement>;
begin
  ASession := TdxEMFCustomSession(Session);
  TdxEMFCustomSessionAccess(ASession).CheckDuplicateObjectInIdentityMap(AObject);
  AEntityInfo := EntityManager.GetEntityInfo(AObject);
  AOptimisticLock := AEntityInfo.OptimisticLockField;
  APropsList := AEntityInfo.GetPropertiesListForUpdateInsert(AObject, True, False);
  if TdxEMFSessionOptionsAccess(ASession.Options).LockingOption = TdxLockingOption.Optimistic then
  begin
    case AEntityInfo.OptimisticLockingBehavior of
      TdxOptimisticLockingBehavior.ConsiderOptimisticLockingField:
        if AOptimisticLock <> nil then
        begin
          NotImplemented;
          Exit;
        end;
      TdxOptimisticLockingBehavior.LockAll,
      TdxOptimisticLockingBehavior.LockModified:
        begin
          NotImplemented;
          Exit;
        end;
    end;
  end;
  AStatement := TdxUpdateQueryGenerator.GenerateUpdate(FBatchWideData, AObject, APropsList.ToArray);
  try
    AUpdateList.AddRange(AStatement);
  finally
    AStatement.Free;
  end;
end;

function TdxProcessingSave.Process: TList<TdxModificationStatement>;
var
  AObject: TObject;
  AInsertList, AStatements: TList<TdxModificationStatement>;
  AUpdateList: TList<TInsertUpdate>;
  AUpdateObjectList: TList<TObject>;
  AUpdate: TInsertUpdate;
  ASession: TdxEMFCustomSession;
begin
  ASession := TdxEMFCustomSession(Session);

  AInsertList := TList<TdxModificationStatement>.Create;
  AUpdateList := TList<TInsertUpdate>.Create;
  AUpdateObjectList := TList<TObject>.Create;
  try
    for AObject in FProcessedObjectList do
    begin
      if ASession.IsNewObject(AObject) then
        InternalInsertObject(AInsertList, AUpdateList, AObject)
      else
        AUpdateObjectList.Add(AObject);
    end;
    for AUpdate in AUpdateList do
    begin
      AStatements := TdxUpdateQueryGenerator.GenerateUpdate(FBatchWideData, AUpdate.TheObject, AUpdate.UpdateMembers);
      try
        AInsertList.AddRange(AStatements);
      finally
        AStatements.Free;
      end;
    end;
    for AObject in AUpdateObjectList do
      InternalUpdateObject(AInsertList, AObject);
    Result := AInsertList;
  finally
    AUpdateList.Free;
    AUpdateObjectList.Free;
  end;
end;

procedure TdxProcessingSave.ProcessResults(AResult: TdxModificationResult);
var
  ASession: TdxEMFCustomSession;
  I: Integer;
  AObject: TObject;
begin
  ASession := TdxEMFCustomSession(Session);
  if FInsertList.Count > 0 then
  begin
    TdxSessionStateStack.Enter(Session, TdxSessionState.ApplyIdentities);
    try
      for I := 0 to FInsertList.Count - 1 do
        TdxEMFCustomSessionAccess(ASession).SetKeyValue(FInsertList[I], AResult.Identities[I].Value);
    finally
      TdxSessionStateStack.Leave(Session, TdxSessionState.ApplyIdentities);
    end;
  end;
  if FBatchWideData.InsertedObjects <> nil then
    for AObject in FBatchWideData.InsertedObjects do
      TdxEMFCustomSessionAccess(Session).RegisterInsertedObject(AObject);
  if FBatchWideData.DeletedObjects <> nil then
  begin
    for AObject in BatchWideData.DeletedObjects do
    begin
      if not ASession.IsNewObject(AObject) then
        TdxEMFCustomSessionAccess(Session).UnregisterObject(AObject);
    end;
  end;
end;

{ TdxProcessingSave.TInsertUpdate }

constructor TdxProcessingSave.TInsertUpdate.Create(AObject: TObject;
  const AUpdateMembers: TArray<TdxMappingMemberInfo>);
begin
  FTheObject := AObject;
  FUpdateMembers := AUpdateMembers;
end;

{ TdxSaveOrderer }

constructor TdxSaveOrderer.Create(ASession: TdxEMFCustomSession; ANewObjectsToSave: TList<TObject>);
var
  AObject, AReferredObject: TObject;
  AReferred: TValue;
  ARefers: TdxObjectSet;
  AEntityInfo: TdxEntityInfo;
  R: TdxMappingMemberInfo;
  AReference: TPair<TObject, TdxObjectSet>;
begin
  inherited Create;
  FSession := ASession;
  FResult := TList<TObject>.Create;
  FResult.Capacity := ANewObjectsToSave.Count;
  FReferredBy := TObjectDictionary<TObject, TdxObjectSet>.Create([doOwnsValues], ANewObjectsToSave.Count);
  FReferences := TObjectDictionary<TObject, TdxObjectSet>.Create([doOwnsValues], ANewObjectsToSave.Count);
  FNonReferencedObjects := TList<TObject>.Create;

  for AObject in ANewObjectsToSave do
    ReferredBy.Add(AObject, TdxObjectSet.Create);
  for AObject in ANewObjectsToSave do
  begin
    ARefers := TdxObjectSet.Create;
    References.Add(AObject, ARefers);
    AEntityInfo := EntityManager.GetEntityInfo(AObject);
    for R in AEntityInfo.ObjectProperties do
    begin
      AReferred := R.GetValue(AObject);
      if AReferred.IsEmpty then
        Continue;
      AReferredObject := AReferred.AsObject;
      if not ReferredBy.ContainsKey(AReferredObject) then
        Continue;
      ARefers.Add(AReferredObject);
      ReferredBy[AReferredObject].Add(AObject);
    end;
  end;
  for AReference in References do
    if AReference.Value.Count = 0 then
      NonReferencedObjects.Add(AReference.Key);
end;

destructor TdxSaveOrderer.Destroy;
begin
  FreeAndNil(FResult);
  FreeAndNil(FReferredBy);
  FreeAndNil(FReferences);
  FreeAndNil(FNonReferencedObjects);
  inherited Destroy;
end;

procedure TdxSaveOrderer.MarkObjectAsSaved(AObject: TObject);
var
  AReferredBy, AList: TdxObjectSet;
  R: TObject;
begin
  if not ReferredBy.TryGetValue(AObject, AReferredBy) then
    Exit;
  for R in AReferredBy do
  begin
    AList := References[R];
    AList.Remove(AObject);
    if AList.Count = 0 then
      NonReferencedObjects.Add(R);
  end;
  for R in References[AObject] do
    ReferredBy[R].Remove(AObject);
  ReferredBy.Remove(AObject);
  References.Remove(AObject);
  Result.Add(AObject);
end;

procedure TdxSaveOrderer.&Do;
var
  AEnFirst: TObjectDictionary<TObject, TdxObjectSet>.TPairEnumerator;
begin
  ProcessGoodObjects;
  AEnFirst := References.GetEnumerator;
  try
    while AEnFirst.MoveNext do
    begin
      EliminateLoop(AEnFirst.Current.Key);
      AEnFirst.Free;
      AEnFirst := References.GetEnumerator;
    end;
  finally
    AEnFirst.Free;
  end;
end;

procedure TdxSaveOrderer.ProcessGoodObjects;
var
  AObject: TObject;
begin
  for AObject in NonReferencedObjects do
    MarkObjectAsSaved(AObject);
  NonReferencedObjects.Clear;
end;

procedure TdxSaveOrderer.EliminateLoop(ANextObject: TObject);
var
  AList: TList<TObject>;
  APassedObjects: TdxObjectDictionary<Integer>;
  AIndex, I, J: Integer;
  AEnNext: TdxObjectSet.TEnumerator;
  AObject: TObject;
begin
  AList := TList<TObject>.Create;
  APassedObjects := TdxObjectDictionary<Integer>.Create;
  try
    AIndex := 0;
    while True do
    begin
      APassedObjects.AddOrSetValue(ANextObject, AIndex);
      AList.Add(ANextObject);
      AEnNext := References[ANextObject].GetEnumerator;
      try
        if not AEnNext.MoveNext then
          raise EInvalidOperation.Create(sdxSessionInternalEMFError);
        ANextObject := AEnNext.Current;
      finally
        AEnNext.Free;
      end;
      if APassedObjects.ContainsKey(ANextObject) then
      begin
        NonReferencedObjects.Add(ANextObject);
        I := 0;
        while I < NonReferencedObjects.Count do
        begin
          AObject := NonReferencedObjects[I];
          MarkObjectAsSaved(AObject);
          if APassedObjects.TryGetValue(AObject, J) then
          begin
            if J < AIndex then
              AIndex := J;
          end;
          Inc(I);
        end;
        NonReferencedObjects.Clear;
        Dec(AIndex);
        if AIndex < 0 then
          Exit;
        ANextObject := AList[AIndex];
        AList.DeleteRange(AIndex, AList.Count - AIndex);
        Dec(AIndex);
      end;
      Inc(AIndex);
    end;
  finally
    AList.Free;
    APassedObjects.Free;
  end;
end;

class function TdxSaveOrderer.DoOrder(ASession: TdxEMFCustomSession; ANewObjectsToSave: TList<TObject>): TArray<TObject>;
var
  AInstance: TdxSaveOrderer;
begin
  AInstance := TdxSaveOrderer.Create(ASession, ANewObjectsToSave);
  try
    AInstance.&Do;
    Result := AInstance.Result.ToArray;
  finally
    AInstance.Free;
  end;
end;

class function TdxSaveOrderer.PrepareSaveOrder(ASession: TdxEMFCustomSession): TArray<TObject>;
var
  ANewObjects, AOldObjects, AResult: TList<TObject>;
  AObject: TObject;
begin
  ANewObjects := TList<TObject>.Create;
  AOldObjects := TList<TObject>.Create;
  AResult := TList<TObject>.Create;
  try
    for AObject in TdxEMFCustomSessionAccess(ASession).GetObjectsToSave do
    begin
      if not ASession.IsObjectToDelete(AObject) then
      begin
        if ASession.IsNewObject(AObject) then
          ANewObjects.Add(AObject)
        else
          AOldObjects.Add(AObject);
      end
    end;
    AResult.AddRange(DoOrder(ASession, ANewObjects));
    AResult.AddRange(AOldObjects);
    Result := AResult.ToArray;
  finally
    ANewObjects.Free;
    AOldObjects.Free;
    AResult.Free;
  end;
end;

{ TdxObjectRecord }








end.
