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

unit dxRichEdit.DocumentModel.Styles.Core;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Contnrs,
  dxCoreClasses,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.Utils.BatchUpdateHelper;

type
  { TdxStyleBase }

  TdxStyleType = (ParagraphStyle, CharacterStyle, TableStyle, NumberingListStyle, TableCellStyle);

  IdxStyle = interface
  ['{DFA740C2-A0BA-442A-8330-8F345400F073}']
    function GetStyleName: string;
    function GetType: TdxStyleType;
    function GetDeleted: Boolean;
    function GetHidden: Boolean;
    function GetSemihidden: Boolean;

    procedure ResetCachedIndices(AResetFormattingCacheType: TdxResetFormattingCacheType);

    property Deleted: Boolean read GetDeleted;
    property Hidden: Boolean read GetHidden;
    property Semihidden: Boolean read GetSemihidden;
    property StyleName: string read GetStyleName;
    property StyleType: TdxStyleType read GetType;
  end;

  { TdxCustomStyle }

  TdxCustomStyleObject = class(TcxIUnknownObject)
  private
    FDocumentModel: TdxCustomDocumentModel;
    function GetPieceTable: TdxCustomPieceTable;
  protected
    property PieceTable: TdxCustomPieceTable read GetPieceTable;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); virtual;

    property DocumentModel: TdxCustomDocumentModel read FDocumentModel;
  end;

  { TdxStyleBase }

  TdxStyleBase = class(TdxCustomStyleObject, IdxStyle, IdxBatchUpdateable, IdxBatchUpdateHandler)
  private
    FDeleted: Boolean;
    FDocumentModel: TObject;
    FHidden: Boolean;
    FId: TGUID;
    FLocalizedStyleName: string;
    FBatchUpdateHelper: TdxBatchUpdateHelper;
    FParentStyle: TdxStyleBase;
    FPrimary: Boolean;
    FSemihidden: Boolean;
    FStyleName: string;
    procedure SetParentStyle(const Value: TdxStyleBase);
    procedure SetStyleName(const Value: string);
  protected
    function CalculateChangeActions: TdxDocumentModelChangeActions; virtual; abstract;
    function CalculateLocalizedName(const AStyleName: string): string;
    procedure MergePropertiesWithParent; virtual; abstract;
    procedure NotifyStyleChanged;
    procedure NotifyStyleChangedCore;
    procedure OnLastEndUpdateCore;
    procedure OnParentDeleting; virtual;
    procedure SetStyleNameCore(const ANewStyleName: string);
    //IdxStyle
    function GetStyleName: string;
    function GetType: TdxStyleType; virtual; abstract;
    function GetDeleted: Boolean;
    function GetHidden: Boolean;
    function GetSemihidden: Boolean;
    procedure ResetCachedIndices(AResetFormattingCacheType: TdxResetFormattingCacheType); virtual;

    property DeletedCore: Boolean read FDeleted write FDeleted;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel;
      AParentStyle: TdxStyleBase = nil; const AStyleName: string = ''); reintroduce; virtual;
    destructor Destroy; override;
    procedure SetParentStyleCore(const ANewStyle: TdxStyleBase);

    function Copy(ATargetModel: TdxCustomDocumentModel): Integer; virtual; abstract;
    procedure CopyProperties(ASource: TdxStyleBase); virtual; abstract;

    function IsParentValid(AParent: TdxStyleBase): Boolean;
    procedure SetId(const Value: TGUID);

    //IdxBatchUpdateable
    function GetIsUpdateLocked: Boolean;
    function GetBatchUpdateHelper: TdxBatchUpdateHelper;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CancelUpdate;

    //IdxBatchUpdateHandler
    procedure OnFirstBeginUpdate;
    procedure OnBeginUpdate;
    procedure OnEndUpdate;
    procedure OnLastEndUpdate;
    procedure OnCancelUpdate;
    procedure OnLastCancelUpdate;

    property SemihiddenCore: Boolean read FSemihidden write FSemihidden;

    property Id: TGUID read FId;
    property IsUpdateLocked: Boolean read GetIsUpdateLocked;
    property BatchUpdateHelper: TdxBatchUpdateHelper read GetBatchUpdateHelper;
    property Parent: TdxStyleBase read FParentStyle write SetParentStyle;
    property Primary: Boolean read FPrimary write FPrimary;
    property StyleName: string read FStyleName write SetStyleName;
    property Deleted: Boolean read FDeleted;
    property Hidden: Boolean read FHidden write FHidden;
    property Semihidden: Boolean read FSemihidden write FSemihidden;
    property StyleType: TdxStyleType read GetType;
    property LocalizedStyleName: string read FLocalizedStyleName;
  end;

  { TdxStyleCollectionBase }

  TdxStyleCollectionBase = class(TdxCustomStyleObject)
  private
    FItems: TdxObjectList<TdxStyleBase>;
    FOnCollectionChanged: TNotifyEvent;
    procedure AddDeletedStyle(AItem: TdxStyleBase);
    function GetCount: Integer;
    function GetDefaultItem: TdxStyleBase;
    function GetDefaultItemIndex: Integer;
    function GetItem(Index: Integer): TdxStyleBase;
    function MustAddParent(AItem: TdxStyleBase): Boolean;
  protected
    procedure AddCore(AItem: TdxStyleBase);
    procedure AddDeletedStyleCore(AItem: TdxStyleBase);
    function CanDeleteStyle(AItem: TdxStyleBase): Boolean; virtual;
    function CreateDefaultItem: TdxStyleBase; virtual; abstract;
    procedure DeleteCore(AItem: TdxStyleBase);
    procedure NotifyChildrenParentDeleting(AItem: TdxStyleBase); virtual;
    procedure NotifyDocumentStyleDeleting(AItem: TdxStyleBase); virtual;
    procedure NotifyPieceTableStyleDeleting(APieceTable: TdxCustomPieceTable; AStyle: TdxStyleBase); virtual; abstract;
    procedure RaiseCollectionChanged;
    procedure ResetItemCachedIndices(AStyle: TdxStyleBase; AResetFormattingCacheType: TdxResetFormattingCacheType);
    property InnerList: TdxObjectList<TdxStyleBase> read FItems;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); override;
    destructor Destroy; override;

    function Add(AItem: TdxStyleBase): Integer;
    function AddNewStyle(AItem: TdxStyleBase): Integer;
    procedure Clear;
    procedure Delete(AStyle: TdxStyleBase);
    function IndexOf(AItem: TdxStyleBase): Integer;
    procedure ResetCachedIndices(AResetFormattingCacheType: TdxResetFormattingCacheType);

    function GetStyleIndexById(const AId: TGUID): Integer;
    function GetStyleIndexByName(const AStyleName: string; AIgnoreCase: Boolean = False): Integer;
    function GetStyleById(const AId: TGUID): TdxStyleBase;
    function GetStyleByName(const AStyleName: string): TdxStyleBase;
    procedure RemoveLastStyle;

    property Count: Integer read GetCount;
    property DefaultItemIndex: Integer read GetDefaultItemIndex;
    property DefaultItem: TdxStyleBase read GetDefaultItem;
    property Self[Index: Integer]: TdxStyleBase read GetItem; default;
    property Items: TdxObjectList<TdxStyleBase> read FItems;
    property OnCollectionChanged: TNotifyEvent read FOnCollectionChanged write FOnCollectionChanged;
  end;

implementation

uses
  RTLConsts,
  dxCore,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs;

type
  { TdxChangeParentStyleHistoryItem }

  TdxChangeParentStyleHistoryItem<T: TdxStyleBase> = class(TdxHistoryItem)
  private
    FStyle: TdxStyleBase;
    FOldParentStyle: T;
    FNewParentStyle: T;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    constructor Create(AStyle: TdxStyleBase; AOldParentStyle, ANewParentStyle: T); reintroduce;
    property OldParentStyle: T read FOldParentStyle;
    property NewParentStyle: T read FNewParentStyle;
    property Style: TdxStyleBase read FStyle;
  end;

  { TdxChangeStyleNameHistoryItem }

  TdxChangeStyleNameHistoryItem = class(TdxHistoryItem)
  private
    FStyle: TdxStyleBase;
    FOldStyleName: string;
    FNewStyleName: string;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    constructor Create(AStyle: TdxStyleBase; const AOldStyleName, ANewStyleName: string); reintroduce;
    property Style: TdxStyleBase read FStyle;
    property OldStyleName: string read FOldStyleName;
    property NewStyleName: string read FNewStyleName;
  end;

  { TdxStyleHistoryItem }

  TdxStyleHistoryItem = class(TdxHistoryItem)
  private
    FStyle: TdxStyleBase;
  public
    constructor Create(AStyle: TdxStyleBase); reintroduce; virtual;
    property Style: TdxStyleBase read FStyle;
  end;

  { TdxDeleteStyleHistoryItem }

  TdxDeleteStyleHistoryItem = class(TdxStyleHistoryItem)
  strict private
    FOwner: TdxStyleCollectionBase;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    constructor Create(AOwner: TdxStyleCollectionBase; AStyle: TdxStyleBase); reintroduce;

    property Owner: TdxStyleCollectionBase read FOwner;
  end;

  { TdxAddStyleHistoryItem }

  TdxAddStyleHistoryItem = class(TdxStyleHistoryItem)
  private
    FCanRedo: Boolean;
    FOwner: TdxStyleCollectionBase;
    FOldDeleted: Boolean;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    constructor Create(AOwner: TdxStyleCollectionBase; AStyle: TdxStyleBase); reintroduce;
    destructor Destroy; override;

    property Owner: TdxStyleCollectionBase read FOwner;
    property OldDeleted: Boolean read FOldDeleted write FOldDeleted;
  end;

  { TdxAddDeletedStyleHistoryItem }

  TdxAddDeletedStyleHistoryItem = class(TdxStyleHistoryItem)
  private
    FOwner: TdxStyleCollectionBase;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    constructor Create(AOwner: TdxStyleCollectionBase; AStyle: TdxStyleBase); reintroduce;
  end;

{ TdxChangeParentStyleHistoryItem<T> }

constructor TdxChangeParentStyleHistoryItem<T>.Create(AStyle: TdxStyleBase; AOldParentStyle, ANewParentStyle: T);
begin
  inherited Create(AStyle.DocumentModel.MainPart);
  FStyle := AStyle;
  FOldParentStyle := AOldParentStyle;
  FNewParentStyle := ANewParentStyle;
end;

procedure TdxChangeParentStyleHistoryItem<T>.RedoCore;
begin
  Style.SetParentStyleCore(NewParentStyle);
end;

procedure TdxChangeParentStyleHistoryItem<T>.UndoCore;
begin
  Style.SetParentStyleCore(OldParentStyle);
end;

{ TdxChangeStyleNameHistoryItem }

constructor TdxChangeStyleNameHistoryItem.Create(AStyle: TdxStyleBase; const AOldStyleName, ANewStyleName: string);
begin
  inherited Create(AStyle.DocumentModel.MainPart);
  FStyle := AStyle;
  FOldStyleName := AOldStyleName;
  FNewStyleName := ANewStyleName;
end;

procedure TdxChangeStyleNameHistoryItem.UndoCore;
begin
  Style.SetStyleNameCore(OldStyleName);
end;

procedure TdxChangeStyleNameHistoryItem.RedoCore;
begin
  Style.SetStyleNameCore(NewStyleName);
end;

{ TdxStyleHistoryItem }

constructor TdxStyleHistoryItem.Create(AStyle: TdxStyleBase);
begin
  inherited Create(AStyle.DocumentModel.MainPart);
  FStyle := AStyle;
end;

{ TdxDeleteStyleHistoryItem }

constructor TdxDeleteStyleHistoryItem.Create(AOwner: TdxStyleCollectionBase; AStyle: TdxStyleBase);
begin
  inherited Create(AStyle);
  Assert(AOwner <> nil);
  FOwner := AOwner;
end;

procedure TdxDeleteStyleHistoryItem.UndoCore;
begin
  Style.DeletedCore := False;
end;

procedure TdxDeleteStyleHistoryItem.RedoCore;
begin
  Owner.DeleteCore(Style);
end;

{ TdxAddStyleHistoryItem }

constructor TdxAddStyleHistoryItem.Create(AOwner: TdxStyleCollectionBase; AStyle: TdxStyleBase);
begin
  inherited Create(AStyle);
  FOwner := AOwner;
end;

destructor TdxAddStyleHistoryItem.Destroy;
begin
  if FCanRedo then
    FreeAndNil(FStyle);
  inherited Destroy;
end;

procedure TdxAddStyleHistoryItem.UndoCore;
begin
  Assert(Owner.Count > 0);
  Assert(Owner.Items[Owner.Count - 1] = Style);
  FOwner.Items.OwnsObjects := False;
  try
    FOwner.RemoveLastStyle;
  finally
    FOwner.Items.OwnsObjects := True;
  end;
  Style.DeletedCore := OldDeleted;
  FCanRedo := True;
end;

procedure TdxAddStyleHistoryItem.RedoCore;
begin
  OldDeleted := Style.Deleted;
  Owner.AddCore(Style);
  Style.DeletedCore := False;
  FCanRedo := False;
end;

{ TdxAddDeletedStyleHistoryItem }

constructor TdxAddDeletedStyleHistoryItem.Create(AOwner: TdxStyleCollectionBase; AStyle: TdxStyleBase);
begin
  inherited Create(AStyle);
  FOwner := AOwner;
end;

procedure TdxAddDeletedStyleHistoryItem.UndoCore;
begin
  FOwner.DeleteCore(Style);
end;

procedure TdxAddDeletedStyleHistoryItem.RedoCore;
begin
  FOwner.AddDeletedStyleCore(Style);
end;

{ TdxCustomStyle }

constructor TdxCustomStyleObject.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  Assert(ADocumentModel <> nil);
  inherited Create;
  FDocumentModel := ADocumentModel;
end;

function TdxCustomStyleObject.GetPieceTable: TdxCustomPieceTable;
begin
  Result := FDocumentModel.MainPart;
end;

{ TdxStyleBase }

constructor TdxStyleBase.Create(ADocumentModel: TdxCustomDocumentModel;
  AParentStyle: TdxStyleBase = nil;
  const AStyleName: string = '');
begin
  inherited Create(ADocumentModel);
  FBatchUpdateHelper := TdxBatchUpdateHelper.Create(Self);
  FDocumentModel := ADocumentModel;
  FParentStyle := AParentStyle;
  SetStyleNameCore(AStyleName);
  CreateGUID(FId);
end;

destructor TdxStyleBase.Destroy;
begin
  FreeAndNil(FBatchUpdateHelper);
  inherited Destroy;
end;

function TdxStyleBase.GetIsUpdateLocked: Boolean;
begin
  Result := FBatchUpdateHelper.IsUpdateLocked;
end;

function TdxStyleBase.GetBatchUpdateHelper: TdxBatchUpdateHelper;
begin
  Result := FBatchUpdateHelper;
end;

procedure TdxStyleBase.BeginUpdate;
begin
  FBatchUpdateHelper.BeginUpdate;
end;

procedure TdxStyleBase.EndUpdate;
begin
  FBatchUpdateHelper.EndUpdate;
end;

procedure TdxStyleBase.CancelUpdate;
begin
  FBatchUpdateHelper.CancelUpdate;
end;

procedure TdxStyleBase.OnFirstBeginUpdate;
begin
  DocumentModel.BeginUpdate;
end;

procedure TdxStyleBase.OnBeginUpdate;
begin
end;

procedure TdxStyleBase.OnEndUpdate;
begin
end;

procedure TdxStyleBase.OnLastEndUpdate;
begin
  OnLastEndUpdateCore;
  DocumentModel.EndUpdate;
end;

procedure TdxStyleBase.OnCancelUpdate;
begin
end;

procedure TdxStyleBase.OnLastCancelUpdate;
begin
  OnLastEndUpdateCore;
  DocumentModel.EndUpdate;
end;

function TdxStyleBase.CalculateLocalizedName(const AStyleName: string): string;
begin
  Result := AStyleName;
end;

procedure TdxStyleBase.NotifyStyleChanged;
begin
  if not IsUpdateLocked then
    NotifyStyleChangedCore;
end;

procedure TdxStyleBase.NotifyStyleChangedCore;
var
  Actions: TdxDocumentModelChangeActions;
begin
  DocumentModel.ResetDocumentFormattingCaches(TdxResetFormattingCacheType.All);
  Actions := CalculateChangeActions;
  DocumentModel.ApplyChangesCore(PieceTable, Actions, 0, PieceTable.Runs.Count - 1);
end;

procedure TdxStyleBase.OnLastEndUpdateCore;
begin
  NotifyStyleChangedCore;
end;

procedure TdxStyleBase.OnParentDeleting;
begin
  Assert(Parent <> nil);
  BeginUpdate;
  try
    MergePropertiesWithParent;
    Parent := Parent.Parent;
  finally
    EndUpdate;
  end;
end;

procedure TdxStyleBase.SetStyleNameCore(const ANewStyleName: string);
begin
  FStyleName := ANewStyleName;
  FLocalizedStyleName := CalculateLocalizedName(ANewStyleName);
end;

function TdxStyleBase.GetStyleName: string;
begin
  Result := FStyleName;
end;

function TdxStyleBase.GetDeleted: Boolean;
begin
  Result := FDeleted;
end;

function TdxStyleBase.GetHidden: Boolean;
begin
  Result := FHidden;
end;

function TdxStyleBase.GetSemihidden: Boolean;
begin
  Result := FSemihidden;
end;

procedure TdxStyleBase.ResetCachedIndices(AResetFormattingCacheType: TdxResetFormattingCacheType);
begin
end;

function TdxStyleBase.IsParentValid(AParent: TdxStyleBase): Boolean;
var
  ACurrentStyle: TdxStyleBase;
begin
  ACurrentStyle := AParent;
  while ACurrentStyle <> nil do
  begin
    if ACurrentStyle = Self then
      Exit(False);
    ACurrentStyle := ACurrentStyle.Parent;
  end;
  Result := True;
end;

procedure TdxStyleBase.SetId(const Value: TGUID);
begin
  FId := Value;
end;

procedure TdxStyleBase.SetParentStyle(const Value: TdxStyleBase);
var
  AItem: TdxChangeParentStyleHistoryItem<TdxStyleBase>;
begin
  if Parent = Value then
    Exit;
  if not IsParentValid(Value) then
  begin
    if (DocumentModel.DeferredChanges = nil) or not DocumentModel.DeferredChanges.IsSetContentMode then
      TdxRichEditExceptions.ThrowInvalidOperationException(sdxRichEditExceptionInvalidParentStyle)
    else
      Exit;
  end;

  AItem := TdxChangeParentStyleHistoryItem<TdxStyleBase>.Create(Self, Parent, Value);
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

procedure TdxStyleBase.SetParentStyleCore(const ANewStyle: TdxStyleBase);
begin
  if IsUpdateLocked then
  begin
    FParentStyle := ANewStyle;
    Exit;
  end;

  DocumentModel.BeginUpdate;
  try
    FParentStyle := ANewStyle;
    NotifyStyleChangedCore;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxStyleBase.SetStyleName(const Value: string);
var
  AHistoryItem: TdxChangeStyleNameHistoryItem;
begin
  if FStyleName <> Value then
  begin
    AHistoryItem := TdxChangeStyleNameHistoryItem.Create(Self, FStyleName, Value);
    DocumentModel.History.Add(AHistoryItem);
    AHistoryItem.Execute;
  end;
end;

{ TdxStyleCollectionBase }

constructor TdxStyleCollectionBase.Create(ADocumentModel: TdxCustomDocumentModel);
var
  ADefaultItem: TdxStyleBase;
begin
  inherited Create(ADocumentModel);
  FItems := TdxObjectList<TdxStyleBase>.Create;
  ADefaultItem := CreateDefaultItem;
  if ADefaultItem <> nil then
    FItems.Add(ADefaultItem);
end;

destructor TdxStyleCollectionBase.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxStyleCollectionBase.Add(AItem: TdxStyleBase): Integer;
begin
  Assert(AItem <> nil);
  Assert(AItem.DocumentModel = DocumentModel);
  DocumentModel.History.BeginTransaction;
  try
    if MustAddParent(AItem) then
      Add(AItem.Parent);
    Result := FItems.IndexOf(AItem);
    if Result >= 0 then
    begin
      if AItem.Deleted then
        AddDeletedStyle(AItem);
    end
    else
      Result := AddNewStyle(AItem);
  finally
    DocumentModel.History.EndTransaction;
  end;
end;

procedure TdxStyleCollectionBase.Clear;
begin
  FItems.Clear;
end;

function TdxStyleCollectionBase.GetStyleIndexById(const AId: TGUID): Integer;
var
  I, ACount: Integer;
  AStyle: TdxStyleBase;
begin
  ACount := Count;
  for I := 0 to ACount - 1 do
  begin
    AStyle := Self[I];
    if IsEqualGUID(AStyle.Id, AId) then
      Exit(I);
  end;
  Result := -1;
end;

function TdxStyleCollectionBase.GetStyleIndexByName(const AStyleName: string; AIgnoreCase: Boolean = False): Integer;
var
  I: Integer;
begin
  if AIgnoreCase then
  begin
    for I := 0 to Count - 1 do
      if SameText(Items[I].StyleName, AStyleName) then
        Exit(I);
  end
  else
  begin
    for I := 0 to Count - 1 do
      if Items[I].StyleName = AStyleName then
        Exit(I);
  end;
  Result := -1;
end;

function TdxStyleCollectionBase.GetStyleById(const AId: TGUID): TdxStyleBase;
var
  AIndex: Integer;
begin
  AIndex := GetStyleIndexById(AId);
  if AIndex >= 0 then
    Result := Self[AIndex]
  else
    Result := nil;
end;

function TdxStyleCollectionBase.GetStyleByName(const AStyleName: string): TdxStyleBase;
var
  AIndex: Integer;
begin
  AIndex := GetStyleIndexByName(AStyleName);
  if AIndex >= 0 then
    Result := Items[AIndex]
  else
    Result := nil;
end;

function TdxStyleCollectionBase.IndexOf(AItem: TdxStyleBase): Integer;
begin
  Result := FItems.IndexOf(AItem);
end;

procedure TdxStyleCollectionBase.ResetCachedIndices(AResetFormattingCacheType: TdxResetFormattingCacheType);
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    ResetItemCachedIndices(Items[I], AResetFormattingCacheType);
end;

procedure TdxStyleCollectionBase.AddCore(AItem: TdxStyleBase);
begin
  Assert(not FItems.Contains(AItem));
  FItems.Add(AItem);
  RaiseCollectionChanged;
end;

procedure TdxStyleCollectionBase.AddDeletedStyleCore(AItem: TdxStyleBase);
begin
  AItem.DeletedCore := False;
  RaiseCollectionChanged;
end;

function TdxStyleCollectionBase.AddNewStyle(AItem: TdxStyleBase): Integer;
var
  AHistoryItem: TdxAddStyleHistoryItem;
begin
  AHistoryItem := TdxAddStyleHistoryItem.Create(Self, AItem);
  DocumentModel.History.Add(AHistoryItem);
  AHistoryItem.Execute;
  Result := FItems.Count - 1;
end;

function TdxStyleCollectionBase.CanDeleteStyle(AItem: TdxStyleBase): Boolean;
begin
  Result := AItem <> DefaultItem;
end;

procedure TdxStyleCollectionBase.Delete(AStyle: TdxStyleBase);
var
  AStyleIndex: Integer;
  AItem: TdxDeleteStyleHistoryItem;
begin
  Assert(AStyle <> nil);
  AStyleIndex := Items.IndexOf(AStyle);
  if AStyleIndex < 0 then
    Exit;
  DocumentModel.History.BeginTransaction;
  try
    AItem := TdxDeleteStyleHistoryItem.Create(Self, AStyle);
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    DocumentModel.History.EndTransaction;
  end;
end;

procedure TdxStyleCollectionBase.DeleteCore(AItem: TdxStyleBase);
begin
  Assert(CanDeleteStyle(AItem));
  NotifyChildrenParentDeleting(AItem);
  NotifyDocumentStyleDeleting(AItem);
  AItem.DeletedCore := True;
  RaiseCollectionChanged;
end;

procedure TdxStyleCollectionBase.NotifyChildrenParentDeleting(AItem: TdxStyleBase);
var
  I: Integer;
  AStyle: TdxStyleBase;
begin
  for I := 0 to Count - 1 do
  begin
    AStyle := Items[I];
    if AStyle.Parent = TObject(AItem) then
      AStyle.OnParentDeleting;
  end;
end;

procedure TdxStyleCollectionBase.NotifyDocumentStyleDeleting(AItem: TdxStyleBase);
var
  APieceTable: TdxCustomPieceTable;
  AList: TdxFastList;
  I: Integer;
begin
  AList := DocumentModel.GetPieceTables(True);
  try
    for I := 0 to AList.Count - 1 do
    begin
      APieceTable := TdxCustomPieceTable(AList[I]);
      NotifyPieceTableStyleDeleting(APieceTable, AItem);
    end;
  finally
    AList.Free;
  end;
end;

procedure TdxStyleCollectionBase.RaiseCollectionChanged;
begin
  dxCallNotify(OnCollectionChanged, Self);
end;

procedure TdxStyleCollectionBase.RemoveLastStyle;
begin
  Assert(FItems.Count > 0);
  FItems.Delete(FItems.Count - 1);
  RaiseCollectionChanged;
end;

procedure TdxStyleCollectionBase.ResetItemCachedIndices(AStyle: TdxStyleBase;
  AResetFormattingCacheType: TdxResetFormattingCacheType);
begin
  AStyle.ResetCachedIndices(AResetFormattingCacheType);
end;

procedure TdxStyleCollectionBase.AddDeletedStyle(AItem: TdxStyleBase);
var
  AHistoryItem: TdxAddDeletedStyleHistoryItem;
begin
  AHistoryItem := TdxAddDeletedStyleHistoryItem.Create(Self, AItem);
  DocumentModel.History.Add(AHistoryItem);
  AHistoryItem.Execute;
end;

function TdxStyleCollectionBase.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxStyleCollectionBase.GetDefaultItem: TdxStyleBase;
begin
  Result := FItems[DefaultItemIndex];
end;

function TdxStyleCollectionBase.GetDefaultItemIndex: Integer;
begin
  Result := 0;
end;

function TdxStyleCollectionBase.GetItem(Index: Integer): TdxStyleBase;
begin
  Result := FItems[Index];
end;

function TdxStyleCollectionBase.MustAddParent(AItem: TdxStyleBase): Boolean;
var
  AParent: TdxStyleBase;
begin
  AParent := AItem.Parent;
  Result := (AParent <> nil) and
    (not FItems.Contains(AParent) or AParent.Deleted);
end;

end.
