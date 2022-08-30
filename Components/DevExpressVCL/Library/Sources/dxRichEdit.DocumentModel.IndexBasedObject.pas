{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.DocumentModel.IndexBasedObject;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

{.$DEFINE USESINGLETONFACTORY}

interface

uses
  SysUtils, Classes, Types, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem;

type
  TdxObtainAffectedRangeEventArgs = class;

  TdxObtainAffectedRangeEvent = procedure(Sender: TObject; E: TdxObtainAffectedRangeEventArgs) of object;
  TdxObtainAffectedRangeEventHandler = TdxMulticastMethod<TdxObtainAffectedRangeEvent>;

  { IdxObtainAffectedRangeListener }

  IdxObtainAffectedRangeListener = interface
  ['{25DEF6AE-E37F-482D-86F8-FCEDD1768410}']
    procedure NotifyObtainAffectedRange(AArgs: TdxObtainAffectedRangeEventArgs);
  end;

  { TdxObtainAffectedRangeEventArgs }

  TdxObtainAffectedRangeEventArgs = class
  private
    FEnd: TdxRunIndex;
    FStart: TdxRunIndex;
  public
    constructor Create;

    property Start: TdxRunIndex read FStart write FStart;
    property &End: TdxRunIndex read FEnd write FEnd;
  end;

  { TdxItemsListComparer }

  TdxItemsListComparer<T: class> = class(TComparer<T>)
  public
    function Compare(const Left, Right: T): Integer; override;
  end;

  { TdxItemsDictionaryComparer }

  TdxItemsDictionaryComparer<T: class> = class(TEqualityComparer<T>)
  public
    function Equals(const Left, Right: T): Boolean; override;
    function GetHashCode(const Value: T): Integer; override;
  end;

  { TdxCloneable }

  TdxCloneableClass = class of TdxCloneable;
  TdxCloneable = class
  public
    constructor Create; virtual;
    procedure CopyFrom(Source: TdxCloneable); virtual;
    function Clone: TdxCloneable; virtual;
  end;

  { TcxCloneableIUnknownObject }

  TdxCloneableIUnknownObject = class(TdxCloneable, IUnknown)
  protected
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  { TdxUniqueItemsCache }

  TdxUniqueItemsCache<T: TdxCloneable> = class(TcxIUnknownObject)
  private
    FItems: TdxFastObjectList;
    FDocumentModel: TdxCustomDocumentModel;
    function GetCount: Integer;
    function GetItem(Index: Integer): T;
    function GetDefaultItem: T;
  protected
    procedure InitItems(const AUnitConverter: IdxDocumentModelUnitConverter); virtual;
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): T; virtual; abstract;
    function AddItemCore(const AItem: T): Integer; virtual;
    function LookupItem(const AItem: T): Integer;
    function AppendItem(const AItem: T): Integer;
    property DocumentModel: TdxCustomDocumentModel read FDocumentModel;
  public
    constructor Create(const AUnitConverter: IdxDocumentModelUnitConverter; const ADocumentModel: TdxCustomDocumentModel = nil);
    destructor Destroy; override;
    function IsIndexValid(AIndex: Integer): Boolean;
    function GetItemIndex(const AItem: T): Integer;
    function AddItem(const AItem: T): Integer;

    property Count: Integer read GetCount;
    property DefaultItem: T read GetDefaultItem;
    property Items[Index: Integer]: T read GetItem; default;
  end;

  { TdxIndexBasedObject }

  TdxIndexBasedObjectValueSetter<T, U> = reference to procedure (const AInfo: T; const ANewValue: U);

  TdxIndexBasedObject<
    TInfo: TdxCloneable;
    TOptions: record> =
    class(TdxCloneableIUnknownObject)
  private
    FInfo: TInfo;
    FOptions: TOptions;
    FDocumentModel: TdxCustomDocumentModel;
  protected
    function CanSetPropertyValue: Boolean; virtual;
    procedure CopyFromCore(const ANewInfo: TInfo; const ANewOptions: TOptions);

    procedure SetPropertyValue<U>(ASetter: TdxIndexBasedObjectValueSetter<TInfo, U>;
     const ANewValue: U);
    procedure SetInfo(const ANewInfo: TInfo);

    function PropertyEquals(const AOther: TdxIndexBasedObject<TInfo, TOptions>): Boolean; virtual; abstract;
  public
    constructor Create(const ADocumentModel: TdxCustomDocumentModel; const AFormattingInfo: TInfo;
      const AFormattingOptions: TOptions); reintroduce;
    destructor Destroy; override;

    procedure ReplaceInfo(const ANewInfo: TInfo; const ANewOptions: TOptions); overload; {$IFDEF DELPHIXE3}inline;{$ENDIF}
    procedure ReplaceInfo(const ANewInfo: TInfo); overload;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CancelUpdate;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Equals(Obj: TObject): Boolean; override;
    function GetHashCode: Integer; override;

    property Info: TInfo read FInfo;
    property Options: TOptions read FOptions write FOptions;
    property DocumentModel: TdxCustomDocumentModel read FDocumentModel;
  end;

  { TdxIndexBasedObjectB }

  TdxIndexBasedObjectB<
      TInfo: TdxCloneable;
      TOptions: record> =
    class(TdxIndexBasedObject<TInfo, TOptions>)
  private
    FPieceTable: TdxCustomPieceTable;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const ADocumentModel: TdxCustomDocumentModel;
      const AFormattingInfo: TInfo; const AFormattingOptions: TOptions); reintroduce;

    property PieceTable: TdxCustomPieceTable read FPieceTable;
  end;

  { TdxUndoableIndexBasedObject }

  TdxUndoableIndexBasedObject<T: TdxCloneable> =
    class(TdxIndexBasedObject,
      IdxBatchUpdateable,
      IdxBatchUpdateHandler,
      IdxBatchInit,
      IdxBatchInitHandler)
  private
    FDocumentModelPart: TdxCustomPieceTable;
		FIndex: Integer;
    FBatchUpdateHelper: TdxBatchUpdateHelper<T>;
    function ChangeIndex(AIndex: Integer; const AChangeActions: TdxDocumentModelChangeActions): Boolean;
    function GetDeferredInfo: T;
    function GetDocumentModel: TdxCustomDocumentModel; {$IFDEF DELPHIXE3}inline;{$ENDIF}
    function GetIsUpdateLocked: Boolean; {$IFDEF DELPHIXE3}inline;{$ENDIF}
    function GetInfo: T;
    function GetInfoCore: T;
    function GetIsDirectNotificationsEnabled: Boolean;
  protected
    procedure ApplyChanges(const AChangeActions: TdxDocumentModelChangeActions); virtual; abstract;
    function CreateIndexChangedHistoryItem: TdxIndexChangedHistoryItemCore; virtual;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<T>; virtual; abstract;
    function GetInfoIndex(const AValue: T): Integer;
    function GetInfoForModification(out AIsDeferred: Boolean): T;
    procedure NotifyFakeAssign; virtual;

    procedure OnFirstBeginUpdateCore; virtual;
    procedure OnIndexChanging; virtual;
    procedure OnIndexChanged; virtual;
    procedure OnLastEndUpdateCore; virtual;
    procedure OnLastCancelUpdateCore; virtual;

    procedure OnBeginAssign; virtual;
    procedure OnEndAssign; virtual;

    //IdxIndexBasedObject
    function GetDocumentModelPart: TdxCustomPieceTable; override;
    function GetIndex: Integer; override;

    //IdxBatchUpdateable
    function GetBatchUpdateHelper: TdxBatchUpdateHelper;

    //IdxBatchInitHandler
    procedure OnBeginInit;
    procedure OnEndInit;
    procedure OnFirstBeginInit;
    procedure OnLastEndInit;
    procedure OnCancelInit;
    procedure OnLastCancelInit;

    // IdxBatchUpdateHandler
    procedure OnFirstBeginUpdate;
    procedure OnBeginUpdate;
    procedure OnEndUpdate;
    procedure OnLastEndUpdate;
    procedure OnCancelUpdate;
    procedure OnLastCancelUpdate;

    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; virtual; abstract;
    function ReplaceInfoCore(const ANewValue: T; const AChangeActions: TdxDocumentModelChangeActions): Boolean;

    property DeferredInfo: T read GetDeferredInfo;

    property IsDirectNotificationsEnabled: Boolean read GetIsDirectNotificationsEnabled;
  public
    constructor Create(const ADocumentModelPart: TdxCustomPieceTable);
    destructor Destroy; override;

    procedure CopyFromCore(const Source: TdxUndoableIndexBasedObject<T>);
    procedure CopyFrom(const Source: TdxUndoableIndexBasedObject<T>); overload; {$IFNDEF DELPHI17} virtual;{$ENDIF}
    procedure CopyFrom(const Source: T); overload; {$IFNDEF DELPHI17} virtual;{$ENDIF}
    function ReplaceInfo(const ANewValue: T; const AChangeActions: TdxDocumentModelChangeActions): Boolean;

    procedure SetIndex(AIndex: Integer; const AChangeActions: TdxDocumentModelChangeActions); override;

    //IdxBatchUpdateable
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CancelUpdate;

    //IdxBatchInit
    procedure BeginInit;
    procedure EndInit;
    procedure CancelInit;

    procedure SuppressDirectNotifications;
    procedure ResumeDirectNotifications;
    procedure SuppressIndexRecalculationOnEndInit;
    procedure ResumeIndexRecalculationOnEndInit;
    procedure SetIndexInitial(AValue: Integer);

    procedure ChangeIndexCore(ANewIndex: Integer; const AChangeActions: TdxDocumentModelChangeActions); virtual;

    property DocumentModelPart: TdxCustomPieceTable read FDocumentModelPart;
    property DocumentModel: TdxCustomDocumentModel read GetDocumentModel;

    property BatchUpdateChangeActions: TdxDocumentModelChangeActions read GetBatchUpdateChangeActions;
    property BatchUpdateHelper: TdxBatchUpdateHelper read GetBatchUpdateHelper;
    property Index: Integer read FIndex;
    property Info: T read GetInfo;
    property InfoCore: T read GetInfoCore;
    property IsUpdateLocked: Boolean read GetIsUpdateLocked;
  end;

  { TdxRichEditIndexBasedObject }

  TdxRichEditIndexBasedObjectValueSetter<T, U> = reference to function (const AInfo: T; const AValue: U): TdxDocumentModelChangeActions;

  TdxRichEditIndexBasedObject<T: TdxCloneable> =
    class(TdxUndoableIndexBasedObject<T>)
  private
    FOnObtainAffectedRange: TdxObtainAffectedRangeEventHandler;
  protected
    function CanSetPropertyValue: Boolean; virtual;
    procedure SetPropertyValue<U>(ASetter: TdxRichEditIndexBasedObjectValueSetter<T, U>; const ANewValue: U);

    procedure ApplyChanges(const AChangeActions: TdxDocumentModelChangeActions); override;
    procedure RaiseObtainAffectedRange(const AArgs: TdxObtainAffectedRangeEventArgs); virtual;
    function GetObtainAffectedRangeListener: IdxObtainAffectedRangeListener; virtual;
  public
    property OnObtainAffectedRange: TdxObtainAffectedRangeEventHandler read FOnObtainAffectedRange;
  end;

implementation

uses
  RTLConsts, dxThreading, Windows,
  dxRichEdit.Utils.Exceptions;

{ TdxObtainAffectedRangeEventArgs }

constructor TdxObtainAffectedRangeEventArgs.Create;
begin
  inherited Create;
  FEnd := -1;
  FStart := -1;
end;

{ TdxEqualsComparer }

function TdxItemsListComparer<T>.Compare(const Left, Right: T): Integer;
begin
  if Left.Equals(Right) then
    Result := 0
  else
    Result := -1;
end;

{ TdxItemsDictionaryComparer }

function TdxItemsDictionaryComparer<T>.Equals(const Left, Right: T): Boolean;
begin
  Result := Left.Equals(Right);
end;

function TdxItemsDictionaryComparer<T>.GetHashCode(const Value: T): Integer;
begin
  Result := Value.GetHashCode;
end;

{ TdxUniqueItemsCache }

constructor TdxUniqueItemsCache<T>.Create(const AUnitConverter: IdxDocumentModelUnitConverter;
  const ADocumentModel: TdxCustomDocumentModel = nil);
begin
  Assert(AUnitConverter <> nil, 'AUnitConverter');
  inherited Create;
  FItems := TdxFastObjectList.Create(True, 256);
  FDocumentModel := ADocumentModel;
  InitItems(AUnitConverter);
end;

destructor TdxUniqueItemsCache<T>.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxUniqueItemsCache<T>.GetItem(Index: Integer): T;
begin
  Result := T(FItems[Index]);
end;


function TdxUniqueItemsCache<T>.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxUniqueItemsCache<T>.AddItem(const AItem: T): Integer;
begin
  Result := LookupItem(AItem);
  if Result < 0 then
    Result := AddItemCore(AItem);
end;

function TdxUniqueItemsCache<T>.AddItemCore(const AItem: T): Integer;
begin
  Result := -1;
  if AItem <> nil then
    Result := AppendItem(AItem.Clone);
end;

function TdxUniqueItemsCache<T>.AppendItem(const AItem: T): Integer;
begin
  Result := Count;
  FItems.Add(AItem);
end;

function TdxUniqueItemsCache<T>.GetDefaultItem: T;
begin
  Result := T(FItems.First);
end;

function TdxUniqueItemsCache<T>.GetItemIndex(const AItem: T): Integer;
begin
  Result := LookupItem(AItem);
  if Result < 0 then
    Result := AddItemCore(AItem);
end;

procedure TdxUniqueItemsCache<T>.InitItems(const AUnitConverter: IdxDocumentModelUnitConverter);
var
  ADefaultItem: T;
begin
  ADefaultItem := CreateDefaultItem(AUnitConverter);
  if ADefaultItem <> nil then
    AppendItem(ADefaultItem);
end;

function TdxUniqueItemsCache<T>.IsIndexValid(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FItems.Count);
end;

function TdxUniqueItemsCache<T>.LookupItem(const AItem: T): Integer;
var
  I: Integer;
begin
  for I := FItems.Count - 1 downto 0 do
    if AItem.Equals(FItems.List[I]) then
      Exit(I);
  Result := -1;
end;

{ TdxIndexBasedObject }

constructor TdxIndexBasedObject<TInfo, TOptions>.Create(const ADocumentModel: TdxCustomDocumentModel;
  const AFormattingInfo: TInfo; const AFormattingOptions: TOptions);
begin
  inherited Create;
  Assert(ADocumentModel <> nil, 'ADocumentModel');
  FDocumentModel := ADocumentModel;

  if AFormattingInfo <> nil then
    FInfo := TInfo(AFormattingInfo.Clone);
  FOptions := AFormattingOptions;
end;

destructor TdxIndexBasedObject<TInfo, TOptions>.Destroy;
begin
  FreeAndNil(FInfo);
  inherited Destroy;
end;

procedure TdxIndexBasedObject<TInfo, TOptions>.ReplaceInfo(const ANewInfo: TInfo; const ANewOptions: TOptions);
begin
  ReplaceInfo(ANewInfo);
  Options := ANewOptions;
end;

procedure TdxIndexBasedObject<TInfo, TOptions>.ReplaceInfo(const ANewInfo: TInfo);
begin
  if ANewInfo <> FInfo then
    FInfo.CopyFrom(ANewInfo);
end;

procedure TdxIndexBasedObject<TInfo, TOptions>.BeginUpdate;
begin
end;

procedure TdxIndexBasedObject<TInfo, TOptions>.EndUpdate;
begin
end;

procedure TdxIndexBasedObject<TInfo, TOptions>.CancelUpdate;
begin
end;

//procedure TdxIndexBasedObject<TInfo, TOptions>.CopyFrom(const Source: TdxIndexBasedObject<TInfo, TOptions>);
procedure TdxIndexBasedObject<TInfo, TOptions>.CopyFrom(Source: TdxCloneable);
var
  ASource: TdxIndexBasedObject<TInfo, TOptions>;
begin
  ASource := TdxIndexBasedObject<TInfo, TOptions>(Source);
  CopyFromCore(ASource.Info, ASource.Options);
end;

function TdxIndexBasedObject<TInfo, TOptions>.Equals(Obj: TObject): Boolean;
var
  AOther: TdxIndexBasedObject<TInfo, TOptions>;
begin
  if Obj is TdxIndexBasedObject<TInfo, TOptions> then
  begin
    AOther := TdxIndexBasedObject<TInfo, TOptions>(Obj);
    if AOther.DocumentModel = DocumentModel then
      Result := FInfo.Equals(AOther.Info) and (TComparer<TOptions>.Default.Compare(Options, AOther.Options) = 0)
    else
      Result := PropertyEquals(AOther);
  end
  else
    Result := False;
end;

function TdxIndexBasedObject<TInfo, TOptions>.GetHashCode: Integer;
begin
  dxAbstractError;
end;

procedure TdxIndexBasedObject<TInfo, TOptions>.SetInfo(const ANewInfo: TInfo);
begin
  FInfo := ANewInfo;
end;

procedure TdxIndexBasedObject<TInfo, TOptions>.SetPropertyValue<U>(
  ASetter: TdxIndexBasedObjectValueSetter<TInfo, U>;
  const ANewValue: U);
var
  AInfo: TInfo;
  AIsDeferredInfo, AIsDeferredOptions: Boolean;
begin
  if not CanSetPropertyValue then
    Exit;
  AInfo := FInfo;
  ASetter(AInfo, ANewValue);
  ReplaceInfo(AInfo);
end;

function TdxIndexBasedObject<TInfo, TOptions>.CanSetPropertyValue: Boolean;
begin
  Result := True;
end;

procedure TdxIndexBasedObject<TInfo, TOptions>.CopyFromCore(const ANewInfo: TInfo; const ANewOptions: TOptions);
begin
  FOptions := ANewOptions;
  ReplaceInfo(ANewInfo);
end;

{ TdxIndexBasedObjectB }

constructor TdxIndexBasedObjectB<TInfo, TOptions>.Create(APieceTable: TdxCustomPieceTable;
  const ADocumentModel: TdxCustomDocumentModel; const AFormattingInfo: TInfo; const AFormattingOptions: TOptions);
begin
  inherited Create(ADocumentModel, AFormattingInfo, AFormattingOptions);
  FPieceTable := APieceTable;
end;

{ TdxUndoableIndexBasedObject }

constructor TdxUndoableIndexBasedObject<T>.Create(const ADocumentModelPart: TdxCustomPieceTable);
begin
  inherited Create;
  if ADocumentModelPart = nil then
    raise EArgumentNilException.Create('DocumentModelPart');
  FDocumentModelPart := ADocumentModelPart;
end;

destructor TdxUndoableIndexBasedObject<T>.Destroy;
begin
  FreeAndNil(FBatchUpdateHelper);
  inherited Destroy;
end;

function TdxUndoableIndexBasedObject<T>.GetIndex: Integer;
begin
  Result := FIndex;
end;

function TdxUndoableIndexBasedObject<T>.GetBatchUpdateHelper: TdxBatchUpdateHelper;
begin
  Result := FBatchUpdateHelper;
end;

function TdxUndoableIndexBasedObject<T>.GetDeferredInfo: T;
begin
  Result := FBatchUpdateHelper.DeferredNotifications;
end;

function TdxUndoableIndexBasedObject<T>.GetDocumentModel: TdxCustomDocumentModel;
begin
  Result := FDocumentModelPart.DocumentModel;
end;

function TdxUndoableIndexBasedObject<T>.GetIsUpdateLocked: Boolean;
begin
  Result := (FBatchUpdateHelper <> nil) and FBatchUpdateHelper.IsUpdateLocked;
end;

function TdxUndoableIndexBasedObject<T>.GetInfoCore: T;
begin
  Result := GetCache(DocumentModel)[FIndex];
end;

function TdxUndoableIndexBasedObject<T>.GetInfo: T;
begin
  if IsUpdateLocked then
    Result := FBatchUpdateHelper.DeferredNotifications
  else
    Result := InfoCore;
end;

function TdxUndoableIndexBasedObject<T>.GetIsDirectNotificationsEnabled: Boolean;
begin
  Result := not IsUpdateLocked or FBatchUpdateHelper.IsDirectNotificationsEnabled;
end;

function TdxUndoableIndexBasedObject<T>.ChangeIndex(AIndex: Integer;
  const AChangeActions: TdxDocumentModelChangeActions): Boolean;
begin
  Assert(AIndex < GetCache(DocumentModel).Count);
  Result := FIndex <> AIndex;
  if Result then
    ChangeIndexCore(AIndex, AChangeActions);
end;

procedure TdxUndoableIndexBasedObject<T>.CopyFromCore(const Source: TdxUndoableIndexBasedObject<T>);
begin
  if Index <> Source.Index then
    SetIndex(Source.Index, BatchUpdateChangeActions);
end;

procedure TdxUndoableIndexBasedObject<T>.CopyFrom(const Source: TdxUndoableIndexBasedObject<T>);
begin
  Assert(DocumentModel = Source.DocumentModel);
  if Index <> Source.Index then
    ChangeIndex(Source.Index, BatchUpdateChangeActions)
  else
    NotifyFakeAssign;
end;

procedure TdxUndoableIndexBasedObject<T>.CopyFrom(const Source: T);
var
  AInfo: T;
  AIsDeferredInfo: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferredInfo);
  AInfo.CopyFrom(Source);
  if not ReplaceInfo(AInfo, BatchUpdateChangeActions) then
    NotifyFakeAssign;
  if not AIsDeferredInfo then AInfo.Free;
end;

procedure TdxUndoableIndexBasedObject<T>.BeginUpdate;
begin
  if FBatchUpdateHelper = nil then
    FBatchUpdateHelper := TdxBatchUpdateHelper<T>.Create(Self, TdxBatchUpdateHelper.TLifetimeStrategy.FreeAfterTransaction);
  FBatchUpdateHelper.BeginUpdate;
end;

procedure TdxUndoableIndexBasedObject<T>.EndUpdate;
begin
  FBatchUpdateHelper.EndUpdate;
end;

procedure TdxUndoableIndexBasedObject<T>.CancelUpdate;
begin
  Assert(FBatchUpdateHelper is TdxBatchUpdateHelper<T>);
  FBatchUpdateHelper.CancelUpdate;
end;

procedure TdxUndoableIndexBasedObject<T>.SuppressDirectNotifications;
begin
  Assert(IsUpdateLocked);
  FBatchUpdateHelper.SuppressDirectNotifications;
end;

procedure TdxUndoableIndexBasedObject<T>.ResumeDirectNotifications;
begin
  Assert(IsUpdateLocked);
  FBatchUpdateHelper.ResumeDirectNotifications;
end;

procedure TdxUndoableIndexBasedObject<T>.SuppressIndexRecalculationOnEndInit;
begin
  Assert(IsUpdateLocked);
  FBatchUpdateHelper.SuppressIndexRecalculationOnEndInit;
end;

procedure TdxUndoableIndexBasedObject<T>.ResumeIndexRecalculationOnEndInit;
begin
  Assert(IsUpdateLocked);
  FBatchUpdateHelper.ResumeIndexRecalculationOnEndInit;
end;

procedure TdxUndoableIndexBasedObject<T>.SetIndexInitial(AValue: Integer);
begin
  FIndex := AValue;
  Assert(FIndex < GetCache(DocumentModel).Count);
end;

function TdxUndoableIndexBasedObject<T>.GetInfoIndex(const AValue: T): Integer;
begin
  Result := GetCache(DocumentModel).GetItemIndex(AValue);
end;

function TdxUndoableIndexBasedObject<T>.GetInfoForModification(out AIsDeferred: Boolean): T;
begin
  AIsDeferred := IsUpdateLocked;
  if AIsDeferred then
    Result := FBatchUpdateHelper.DeferredNotifications
  else
    Result := T(InfoCore.Clone);
end;

procedure TdxUndoableIndexBasedObject<T>.ChangeIndexCore(ANewIndex: Integer;
  const AChangeActions: TdxDocumentModelChangeActions);
var
  ADocumentModel: TdxCustomDocumentModel;
  AItem: TdxIndexChangedHistoryItemCore;
begin
  ADocumentModel := DocumentModel;
  ADocumentModel.BeginUpdate;
  try
    OnBeginAssign;
    try
      AItem := CreateIndexChangedHistoryItem;
      AItem.OldIndex := Index;
      AItem.NewIndex := ANewIndex;
      AItem.ChangeActions := AChangeActions;
      DocumentModel.AddHistoryItem(AItem);
      AItem.Execute;
    finally
      OnEndAssign;
    end;
  finally
    ADocumentModel.EndUpdate;
  end;
end;

function TdxUndoableIndexBasedObject<T>.CreateIndexChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(DocumentModelPart, Self);
end;

procedure TdxUndoableIndexBasedObject<T>.NotifyFakeAssign;
begin
  if IsUpdateLocked then
    FBatchUpdateHelper.FakeAssignDetected := True
  else
  begin
    DocumentModel.BeginUpdate;
    try
      OnBeginAssign;
      try
      finally
        OnEndAssign;
      end;
    finally
      DocumentModel.EndUpdate;
    end;
  end;
end;

procedure TdxUndoableIndexBasedObject<T>.OnFirstBeginUpdateCore;
begin
  FBatchUpdateHelper.DeferredNotifications := T(InfoCore.Clone)
end;

procedure TdxUndoableIndexBasedObject<T>.OnIndexChanging;
begin
end;

procedure TdxUndoableIndexBasedObject<T>.OnIndexChanged;
begin
end;

procedure TdxUndoableIndexBasedObject<T>.OnLastEndUpdateCore;
begin
  Assert(FBatchUpdateHelper is TdxBatchUpdateHelper<T>);
  if not ReplaceInfo(FBatchUpdateHelper.DeferredNotifications, BatchUpdateChangeActions) then
    if FBatchUpdateHelper.FakeAssignDetected then
      NotifyFakeAssign;
  FreeAndNil(FBatchUpdateHelper);
end;

procedure TdxUndoableIndexBasedObject<T>.OnLastCancelUpdateCore;
begin
  Assert(FBatchUpdateHelper is TdxBatchUpdateHelper<T>);
  FreeAndNil(FBatchUpdateHelper);
end;

function TdxUndoableIndexBasedObject<T>.ReplaceInfoCore(const ANewValue: T; const AChangeActions: TdxDocumentModelChangeActions): Boolean;
var
  AIndex: Integer;
begin
  AIndex := GetInfoIndex(ANewValue);
  Result := ChangeIndex(AIndex, AChangeActions);
end;

function TdxUndoableIndexBasedObject<T>.ReplaceInfo(const ANewValue: T; const AChangeActions: TdxDocumentModelChangeActions): Boolean;
begin
  if IsUpdateLocked then
    Result := False
  else
    Result := ReplaceInfoCore(ANewValue, AChangeActions);
end;

procedure TdxUndoableIndexBasedObject<T>.OnBeginAssign;
begin
end;

procedure TdxUndoableIndexBasedObject<T>.OnEndAssign;
begin
end;

function TdxUndoableIndexBasedObject<T>.GetDocumentModelPart: TdxCustomPieceTable;
begin
  Result := FDocumentModelPart;
end;

procedure TdxUndoableIndexBasedObject<T>.SetIndex(AIndex: Integer;
  const AChangeActions: TdxDocumentModelChangeActions);
begin
  if FIndex <> AIndex then
  begin
    OnIndexChanging;
    FIndex := AIndex;
    Assert(FIndex < GetCache(DocumentModel).Count);
    ApplyChanges(AChangeActions);
    OnIndexChanged;
  end;
end;

procedure TdxUndoableIndexBasedObject<T>.BeginInit;
begin
  if FBatchUpdateHelper = nil then
    FBatchUpdateHelper := TdxBatchInitHelper<T>.Create(Self);
  Assert(FBatchUpdateHelper is TdxBatchInitHelper<T>);
  FBatchUpdateHelper.BeginUpdate;
end;

procedure TdxUndoableIndexBasedObject<T>.EndInit;
begin
  Assert(FBatchUpdateHelper is TdxBatchInitHelper<T>);
  FBatchUpdateHelper.EndUpdate;
end;

procedure TdxUndoableIndexBasedObject<T>.CancelInit;
begin
  Assert(FBatchUpdateHelper is TdxBatchInitHelper<T>);
  FBatchUpdateHelper.CancelUpdate;
end;

procedure TdxUndoableIndexBasedObject<T>.OnBeginInit;
begin
end;

procedure TdxUndoableIndexBasedObject<T>.OnEndInit;
begin
end;

procedure TdxUndoableIndexBasedObject<T>.OnFirstBeginInit;
begin
  FBatchUpdateHelper.DeferredNotifications := T(InfoCore.Clone)
end;

procedure TdxUndoableIndexBasedObject<T>.OnLastEndInit;
begin
  if FBatchUpdateHelper.IsIndexRecalculationOnEndInitEnabled then
    FIndex := GetInfoIndex(FBatchUpdateHelper.DeferredNotifications);
  Assert(FIndex < GetCache(DocumentModel).Count);
  FreeAndNil(FBatchUpdateHelper);
end;

procedure TdxUndoableIndexBasedObject<T>.OnCancelInit;
begin
end;

procedure TdxUndoableIndexBasedObject<T>.OnLastCancelInit;
begin
  if FBatchUpdateHelper.IsIndexRecalculationOnEndInitEnabled then
    FIndex := GetInfoIndex(FBatchUpdateHelper.DeferredNotifications);
  Assert(FIndex < GetCache(DocumentModel).Count);
  FreeAndNil(FBatchUpdateHelper);
end;

procedure TdxUndoableIndexBasedObject<T>.OnFirstBeginUpdate;
begin
  OnFirstBeginUpdateCore;
end;

procedure TdxUndoableIndexBasedObject<T>.OnBeginUpdate;
begin
end;

procedure TdxUndoableIndexBasedObject<T>.OnEndUpdate;
begin
end;

procedure TdxUndoableIndexBasedObject<T>.OnLastEndUpdate;
begin
  OnLastEndUpdateCore;
end;

procedure TdxUndoableIndexBasedObject<T>.OnCancelUpdate;
begin
end;

procedure TdxUndoableIndexBasedObject<T>.OnLastCancelUpdate;
begin
  OnLastCancelUpdateCore;
end;

{ TdxRichEditIndexBasedObject }

function TdxRichEditIndexBasedObject<T>.CanSetPropertyValue: Boolean;
begin
  Result := True;
end;

procedure TdxRichEditIndexBasedObject<T>.SetPropertyValue<U>(
  ASetter: TdxRichEditIndexBasedObjectValueSetter<T, U>;
  const ANewValue: U);
var
  AInfo: T;
  AChangeActions: TdxDocumentModelChangeActions;
begin
  if not CanSetPropertyValue then
    Exit;
  if IsUpdateLocked then
    ASetter(DeferredInfo, ANewValue)
  else
  begin
    begin
      AInfo := T(InfoCore.Clone);
      try
        AChangeActions := ASetter(AInfo, ANewValue);
        ReplaceInfoCore(AInfo, AChangeActions);
      finally
        AInfo.Free;
      end;
    end;
  end;
end;

procedure TdxRichEditIndexBasedObject<T>.ApplyChanges(const AChangeActions: TdxDocumentModelChangeActions);
var
  AArgs: TdxObtainAffectedRangeEventArgs;
  AListener: IdxObtainAffectedRangeListener;
begin
  AArgs := TdxObtainAffectedRangeEventArgs.Create;
  try
    AListener := GetObtainAffectedRangeListener;
    if (AListener <> nil) and IsDirectNotificationsEnabled then
      AListener.NotifyObtainAffectedRange(AArgs)
    else
      RaiseObtainAffectedRange(AArgs);
    if AArgs.Start >= 0 then
      DocumentModelPart.ApplyChangesCore(AChangeActions, AArgs.Start, AArgs.&End);
  finally
    FreeAndNil(AArgs);
  end;
end;

procedure TdxRichEditIndexBasedObject<T>.RaiseObtainAffectedRange(const AArgs: TdxObtainAffectedRangeEventArgs);
begin
  if not FOnObtainAffectedRange.Empty then
    FOnObtainAffectedRange.Invoke(Self, AArgs);
end;

function TdxRichEditIndexBasedObject<T>.GetObtainAffectedRangeListener: IdxObtainAffectedRangeListener;
begin
  Result := nil;
end;

{ TdxCloneable }

constructor TdxCloneable.Create;
begin
  inherited Create;
end;

function TdxCloneable.Clone: TdxCloneable;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxCloneableClass(ClassType).Create;
  Result.CopyFrom(Self);
end;

procedure TdxCloneable.CopyFrom(Source: TdxCloneable);
begin

end;

{ TdxCloneableIUnknownObject }

function TdxCloneableIUnknownObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TdxCloneableIUnknownObject._AddRef: Integer;
begin
  Result := -1;
end;

function TdxCloneableIUnknownObject._Release: Integer;
begin
  Result := -1;
end;

end.
