{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit cxCheckListBox;

interface

{$I cxVer.inc}

uses
  Windows, Messages, Types, Variants, SysUtils, Classes, Controls, Graphics, StdCtrls,
  Forms, Math, ImgList,
  dxCore, dxMessages, cxClasses, cxGeometry, cxControls, cxContainer, cxGraphics, cxVariants, dxGDIPlusClasses,
  cxDataUtils, cxEdit, cxListBox, cxCheckBox, cxExtEditConsts, cxExtEditUtils, cxScrollBar,
  cxLookAndFeels, cxLookAndFeelPainters, cxCustomListBox;

type
  TcxCustomCheckListBox = class;
  TcxCustomInnerCheckListBox = class;
  TcxCheckListBoxItems = class;

  TcxCheckListBoxImageLayout = (ilBeforeChecks, ilAfterChecks);

  TcxClickCheckEvent = procedure(Sender: TObject; AIndex: Integer;
    APrevState, ANewState: TcxCheckBoxState) of object;

  TcxCheckStatesToEditValueEvent = procedure(Sender: TObject;
    const ACheckStates: TcxCheckStates; out AEditValue: TcxEditValue) of object;
  TcxEditValueToCheckStatesEvent = procedure(Sender: TObject;
    const AEditValue: TcxEditValue; var ACheckStates: TcxCheckStates) of object;

  { TcxCheckListBoxItem }

  TcxCheckListBoxItem = class(TcxCaptionItem)
  private
    FEnabled: Boolean;
    FImageIndex: TcxImageIndex;
    FItemObject: TObject;
    FState: TcxCheckBoxState;
    function GetChecked: Boolean;
    function GetCheckListBox: TcxCustomInnerCheckListBox;
    function GetCollection: TcxCheckListBoxItems;
    function GetText: TCaption;
    procedure SetText(const Value: TCaption);
    procedure SetEnabled(Value: Boolean);
    procedure SetImageIndex(Value: TcxImageIndex);
    procedure SetState(Value: TcxCheckBoxState);
    procedure SetChecked(Value: Boolean);
  protected
    function GetDisplayName: string; override;

    property CheckListBox: TcxCustomInnerCheckListBox read GetCheckListBox;
    property Collection: TcxCheckListBoxItems read GetCollection;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property Checked: Boolean read GetChecked write SetChecked;
    property ItemObject: TObject read FItemObject write FItemObject;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex default -1;
    property State: TcxCheckBoxState read FState write SetState default cbsUnchecked;
    property Tag;
    property Text: TCaption read GetText write SetText;
  end;

  { TcxCheckListBoxItems }

  TcxCheckListBoxItems = class(TcxCaptionItems)
  private
    FChangedLockCount: Integer;
    FCheckListBox: TcxCustomInnerCheckListBox;
    function GetItems(Index: Integer): TcxCheckListBoxItem;
    function GetObjects(Index: Integer): TObject;
    procedure SetItems(Index: Integer; const Value: TcxCheckListBoxItem);
    procedure SetObjects(Index: Integer; Value: TObject);
  protected
    procedure SynchronizeInnerListBoxItems;
    procedure Update(Item: TCollectionItem); override;
    function IsChangedLocked: Boolean;
    procedure LockChanged(ALock: Boolean; AInvokeChangedOnUnlock: Boolean = True);
  public
    constructor Create(AOwner: TcxCustomInnerCheckListBox; AItemClass: TCollectionItemClass);
    destructor Destroy; override;
    property CheckListBox: TcxCustomInnerCheckListBox read FCheckListBox;
    property Items[Index: Integer]: TcxCheckListBoxItem read GetItems write SetItems; default;
    function Add: TcxCheckListBoxItem;
    procedure Delete(Index: Integer);
    function IndexOf(const S: TCaption): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    procedure LoadStrings(AStrings: TStrings);
    property Objects[Index: Integer]: TObject read GetObjects write SetObjects;
  end;

  { TcxCustomInnerCheckListBox }

  TcxCheckListBoxMetrics = record
    CheckFrameWidth: Integer;
    ContentOffset: Integer;
    ImageFrameWidth: Integer;
    TextAreaOffset: Integer;
    TextOffset: Integer;
    TextWidthCorrection: Integer;
  end;

  { TcxCustomInnerCheckListBox }

  TcxCustomInnerCheckListBox = class(TcxCustomInnerListBox, IcxMouseTrackingCaller)
  private
    FAllowDblClickToggle: Boolean;
    FAllowGrayed: Boolean;
    FCapturedCheckIndex: Integer;
    FCheckItems: TcxCheckListBoxItems;
    FGlyph: TdxSmartGlyph;
    FGlyphCount: Integer;
    FHotCheckIndex: Integer;
    FMetrics: TcxCheckListBoxMetrics;
    FNewPressedCheckIndex: Integer;
    FNewPressedCheckItemFullyVisible: Boolean;

    FPressedCheckIndex: Integer;
    FOnClickCheck: TcxClickCheckEvent;

    function GetContainer: TcxCustomCheckListBox;
    procedure DrawCheck(R: TRect; AState: TcxCheckBoxState; ACheckState: TcxEditCheckState);
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TdxSmartGlyph);
    procedure SetGlyphCount(Value: Integer);
    procedure ToggleClickCheck(Index: Integer);
    procedure InvalidateCheck(Index: Integer);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    // IcxMouseTrackingCaller
    procedure IcxMouseTrackingCaller.MouseLeave = MouseTrackingMouseLeave;
    procedure MouseTrackingMouseLeave;

    procedure AdjustItemHeight;
    procedure CheckHotTrack;
    procedure Click; override;
    function GetCheckAt(X, Y: Integer): Integer;
    function GetCheckAreaSize: TSize;
    function GetCheckAreaRect(const R: TRect): TRect;
    procedure GetCheckMetrics(out ACheckSize: TSize; out ACheckBorderOffset: Integer);
    function GetCheckRect(const R: TRect; AReturnFullRect: Boolean): TRect;
    function GetCheckRegionWidth: Integer; virtual;
    function GetMetrics: TcxCheckListBoxMetrics; virtual;
    function GetStandardItemHeight: Integer; virtual;
    function GetVisibleItemCount: Integer;
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Integer);
    procedure FullRepaint; virtual;
    procedure InvalidateItem(Index: Integer); virtual;
    procedure SynchronizeCheckStates(ANewHotCheckIndex, ANewPressedCheckIndex: Integer);
    procedure UpdateCheckStates;
    procedure UpdateEditValue;
    procedure UpdateMetrics;
    procedure WndProc(var Message: TMessage); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DoClickCheck(const AIndex: Integer; const OldState, NewState: TcxCheckBoxState); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DblClick; override;

    property AllowDblClickToggle: Boolean read FAllowDblClickToggle write FAllowDblClickToggle default True;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property CheckItems: TcxCheckListBoxItems read FCheckItems write FCheckItems;
    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph;
    property GlyphCount: Integer read FGlyphCount write SetGlyphCount default 6;
    property Metrics: TcxCheckListBoxMetrics read FMetrics;

    property OnClickCheck: TcxClickCheckEvent read FOnClickCheck write FOnClickCheck;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Container: TcxCustomCheckListBox read GetContainer;
  end;

  TcxCustomInnerCheckListBoxClass = class of TcxCustomInnerCheckListBox;

  { TcxCustomCheckListBox }

  TcxCustomCheckListBox = class(TcxCustomListBox, IdxMultiPartGlyphSupport)
  private
    FCheckBorderStyle: TcxEditCheckBoxBorderStyle;
    FEditValue: TcxEditValue;
    FEditValueFormat: TcxCheckStatesValueFormat;
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;
    FImageLayout: TcxCheckListBoxImageLayout;
    FIsModified: Boolean;
    FListStyle: TListBoxStyle;
    FLoadedSortedValue: Boolean;
    FNativeStyle: Boolean;
    FShowChecks: Boolean;

    FOnCheckStatesToEditValue: TcxCheckStatesToEditValueEvent;
    FOnDrawItem: TDrawItemEvent;
    FOnEditValueChanged: TNotifyEvent;
    FOnEditValueToCheckStates: TcxEditValueToCheckStatesEvent;
    FOnMeasureItem: TMeasureItemEvent;

    function GetOnClickCheck: TcxClickCheckEvent;
    function GetOnCompare: TcxCollectionCompareEvent;
    function GetAllowGrayed: Boolean;
    function GetAllowDblClickToggle: Boolean;
    function GetGlyph: TdxSmartGlyph;
    function GetItems: TcxCheckListBoxItems;
    function GetColumns: Integer;
    function GetSorted: Boolean;
    procedure ImagesChanged(Sender: TObject);
    function IsItemHeightStored: Boolean;
    procedure SetOnClickCheck(Value: TcxClickCheckEvent);
    procedure SetOnCompare(Value: TcxCollectionCompareEvent);
    procedure SetAllowGrayed(Value: Boolean);
    procedure SetAllowDblClickToggle(Value: Boolean);
    procedure SetEditValueFormat(Value: TcxCheckStatesValueFormat);
    procedure SetGlyph(Value: TdxSmartGlyph);
    procedure SetGlyphCount(Value: Integer);
    procedure SetItems(Value: TcxCheckListBoxItems);
    procedure SetColumns(Value: Integer);
    procedure SetImageLayout(Value: TcxCheckListBoxImageLayout);
    procedure SetImages(Value: TCustomImageList);
    procedure SetShowChecks(Value: Boolean);
    procedure SetSorted(Value: Boolean);
    procedure CheckEditValueFormat;
    function GetRealEditValueFormat: TcxCheckStatesValueFormatEx;
    procedure ItemsChanged(Sender: TObject; AItem: TCollectionItem);
    function GetInnerCheckListBox: TcxCustomInnerCheckListBox;
  protected
    function AllowTouchScrollUIMode: Boolean; override;
    procedure CalculateDrawCheckParams;
    procedure CreateHandle; override;
    procedure Loaded; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CorrectAlignControlRect(var R: TRect); override;
    procedure FontChanged; override;
    function IsInternalControl(AControl: TControl): Boolean; override;
    procedure DoSetSize; override;

    procedure DataChange; override;
    function DoCreateInnerListBox: TcxCustomInnerListBox; override;
    procedure UpdateData; override;
    procedure WndProc(var Message: TMessage); override;
    procedure DrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure DrawItemText(const AText: string; const ARect: TRect); virtual;
    procedure DoEditValueChanged; virtual;
    procedure DoScrollUIModeChanged; override;
    function GetInnerCheckListBoxClass: TcxCustomInnerCheckListBoxClass; virtual;
    function GetItemText(AItemIndex: Integer): string; override;
    function GetListItemHeight(AIndex: Integer): Integer; override;
    function GetListStyle: TListBoxStyle; override;
    function IndexOf(const S: string): Integer; override;
    procedure InitializeInnerListBox; override;
    procedure InternalKeyDown(var Key: Word; Shift: TShiftState); override;
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure SetEditValue(const Value: TcxEditValue); virtual;
    function IsValueValid(const AValue: TcxEditValue; AAllowEmpty: Boolean): Boolean; virtual;
    function NeedIgnorePressedKey(var Key: Char): Boolean; override;
    function NeedsScrollBars: Boolean; override;
    procedure SetItemHeight(Value: Integer); override;
    procedure SetListStyle(Value: TListBoxStyle); override;

    // IdxMultiPartGlyphSupport
    function GetGlyphCount: Integer;
    function GetStateCaption(AIndex: Integer): string;

    property AllowDblClickToggle: Boolean read GetAllowDblClickToggle write SetAllowDblClickToggle default True;
    property AllowGrayed: Boolean read GetAllowGrayed write SetAllowGrayed default False;
    property AutoComplete;
    property AutoCompleteDelay;
    property Columns: Integer read GetColumns write SetColumns default 0;
    property EditValue: TcxEditValue read FEditValue write SetEditValue stored False;
    property EditValueFormat: TcxCheckStatesValueFormat read FEditValueFormat write SetEditValueFormat default cvfInteger;
    property Glyph: TdxSmartGlyph read GetGlyph write SetGlyph;
    property GlyphCount: Integer read GetGlyphCount write SetGlyphCount default 6;
    property ImageLayout: TcxCheckListBoxImageLayout read FImageLayout write SetImageLayout default ilBeforeChecks;
    property ItemHeight stored IsItemHeightStored;
    property ShowChecks: Boolean read FShowChecks write SetShowChecks default True;
    property Sorted: Boolean read GetSorted write SetSorted default False;
    property TabWidth;
    property OnCheckStatesToEditValue: TcxCheckStatesToEditValueEvent read FOnCheckStatesToEditValue write FOnCheckStatesToEditValue;
    property OnClickCheck: TcxClickCheckEvent read GetOnClickCheck write SetOnClickCheck;
    property OnCompare: TcxCollectionCompareEvent read GetOnCompare write SetOnCompare;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnEditValueChanged: TNotifyEvent read FOnEditValueChanged write FOnEditValueChanged;
    property OnEditValueToCheckStates: TcxEditValueToCheckStatesEvent read FOnEditValueToCheckStates write FOnEditValueToCheckStates;
    property OnMeasureItem: TMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CheckAtPos(const APos: TPoint): Integer;
    procedure Clear;
    function GetBestFitWidth: Integer;
    function GetHeight(ARowCount: Integer): Integer; virtual;
    function GetItemWidth(AIndex: Integer): Integer; virtual;
    function GetVisibleItemCount: Integer;
    function ItemAtPos(const APos: TPoint; AExisting: Boolean): Integer;
    function ItemRect(Index: Integer): TRect;
    procedure Sort;
    procedure AddItem(const AItem: string);
    procedure CopySelection(ADestination: TcxCustomCheckListBox);
    procedure DeleteSelected;
    procedure MoveSelection(ADestination: TcxCustomCheckListBox);

    property Count;
    property InnerCheckListBox: TcxCustomInnerCheckListBox read GetInnerCheckListBox;
    property IsModified: Boolean read FIsModified write FIsModified;
    property ItemIndex;
    property Selected;
    property TopIndex;
// !!!
    property Images: TCustomImageList read FImages write SetImages;
    property Items: TcxCheckListBoxItems read GetItems write SetItems;
    property LookAndFeel;
  end;

  { TcxCheckListBox }

  TcxCheckListBox = class(TcxCustomCheckListBox)
  published
    property AllowDblClickToggle;
    property AllowGrayed;
    property Anchors;
    property AutoComplete;
    property AutoCompleteDelay;
    property BiDiMode;
    property Columns;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditValue;
    property EditValueFormat;
    property Enabled;
    property Glyph;
    property GlyphCount;
    property Images;
    property ImageLayout;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property Items;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ScrollWidth;
    property ShowChecks;
    property ShowHint;
    property Sorted;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnCheckStatesToEditValue;
    property OnClick;
    property OnClickCheck;
    property OnCompare;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEditValueChanged;
    property OnEditValueToCheckStates;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem; // deprecated
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  RTLConsts, Consts, cxEditPaintUtils, cxEditUtils, dxThemeManager, dxForms;

const
  cxCheckListBoxCheckFrameWidth = 1;
  cxCheckListBoxContentOffset = 0;
  cxCheckListBoxImageFrameWidth = 1;
  cxCheckListBoxTextAreaOffset = 1;
  cxCheckListBoxTextOffset = 2;
  cxCheckListBoxTextWidthCorrection = 3;

{ TcxCheckListBoxItem }

constructor TcxCheckListBoxItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEnabled := True;
  FImageIndex := -1;
  FState := cbsUnchecked;
end;

procedure TcxCheckListBoxItem.Assign(Source: TPersistent);
begin
  if Source is TcxCheckListBoxItem then
  begin
    Enabled := TcxCheckListBoxItem(Source).Enabled;
    ImageIndex := TcxCheckListBoxItem(Source).ImageIndex;
    ItemObject := TcxCheckListBoxItem(Source).ItemObject;
    State := TcxCheckListBoxItem(Source).State;
  end;
  inherited;
end;

function TcxCheckListBoxItem.GetDisplayName: string;
begin
  Result := Text;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TcxCheckListBoxItem.GetChecked: Boolean;
begin
  Result := (State = cbsChecked);
end;

function TcxCheckListBoxItem.GetCheckListBox: TcxCustomInnerCheckListBox;
begin
  if Collection <> nil then
    Result := Collection.CheckListBox
  else
    Result := nil;
end;

function TcxCheckListBoxItem.GetCollection: TcxCheckListBoxItems;
begin
  Result := TcxCheckListBoxItems(inherited Collection);
end;

function TcxCheckListBoxItem.GetText: TCaption;
begin
  Result := Caption;
end;

procedure TcxCheckListBoxItem.SetText(const Value: TCaption);
begin
  Caption := Value;
end;

procedure TcxCheckListBoxItem.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

procedure TcxCheckListBoxItem.SetImageIndex(Value: TcxImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

procedure TcxCheckListBoxItem.SetState(Value: TcxCheckBoxState);
begin
  if CheckListBox <> nil then
  begin
//TODO:    if (Value = cbsGrayed) and (CheckListBox.Container.GetRealEditValueFormat = cvfInteger) then
    if (Value = cbsGrayed) and (CheckListBox.Container.EditValueFormat = cvfInteger) then
      Value := cbsUnchecked;
    if FState = Value then
      Exit;
    FState := Value;
    CheckListBox.InvalidateCheck(Index);
    if not Collection.IsChangedLocked then
      CheckListBox.UpdateEditValue;
  end
  else
    FState := Value;
end;

procedure TcxCheckListBoxItem.SetChecked(Value: Boolean);
begin
  if Value then
    State := cbsChecked
  else
    State := cbsUnchecked;
end;

{ TcxCheckListBoxItems }

constructor TcxCheckListBoxItems.Create(AOwner: TcxCustomInnerCheckListBox; AItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, AItemClass);
  FCheckListBox := AOwner;
end;

destructor TcxCheckListBoxItems.Destroy;
begin
  FCheckListBox := nil;
  inherited;
end;

function TcxCheckListBoxItems.GetItems(Index: Integer): TcxCheckListBoxItem;
begin
  Result := TcxCheckListBoxItem(inherited Items[Index]);
end;

function TcxCheckListBoxItems.GetObjects(Index: Integer): TObject;
begin
  Result := Items[Index].ItemObject;
end;

procedure TcxCheckListBoxItems.SetItems(Index: Integer; const Value: TcxCheckListBoxItem);
begin
  inherited Items[Index] := Value;
end;

procedure TcxCheckListBoxItems.SetObjects(Index: Integer; Value: TObject);
begin
  Items[Index].ItemObject := Value;
end;

procedure TcxCheckListBoxItems.SynchronizeInnerListBoxItems;
var
  I, ATopIndex, AItemIndex: Integer;
begin
  CheckListBox.Items.BeginUpdate;
  try
    AItemIndex := CheckListBox.ItemIndex;
    ATopIndex := CheckListBox.TopIndex;

    CheckListBox.Items.Clear;
    for I := 0 to Count - 1 do
      CheckListBox.Items.Add(Items[I].Text);

    CheckListBox.TopIndex := ATopIndex;
    CheckListBox.ItemIndex := AItemIndex;
  finally
    CheckListBox.Items.EndUpdate;
  end;
end;

procedure TcxCheckListBoxItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if not Assigned(CheckListBox) then
    Exit;

  if (CheckListBox.Items.Count <> Count) or (Item = nil) then
    SynchronizeInnerListBoxItems
  else
  begin
    if not CheckListBox.Container.IsDesigning then
      CheckListBox.Items[Item.Index] := TcxCheckListBoxItem(Item).Text;
    CheckListBox.InvalidateItem(Item.Index);
  end;

  if CheckListBox.Container.IsModified then
    CheckListBox.UpdateEditValue
  else
    CheckListBox.UpdateCheckStates;
end;

function TcxCheckListBoxItems.IsChangedLocked: Boolean;
begin
  Result := FChangedLockCount > 0;
end;

procedure TcxCheckListBoxItems.LockChanged(ALock: Boolean;
  AInvokeChangedOnUnlock: Boolean = True);
begin
  if ALock then
    Inc(FChangedLockCount)
  else
    if FChangedLockCount > 0 then
    begin
      Dec(FChangedLockCount);
      if AInvokeChangedOnUnlock and (FChangedLockCount = 0) then
        Changed;
    end;
end;

function TcxCheckListBoxItems.Add: TcxCheckListBoxItem;
begin
  Result := TcxCheckListBoxItem(inherited Add);
end;

procedure TcxCheckListBoxItems.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

function TcxCheckListBoxItems.IndexOf(const S: TCaption): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if InternalCompareString(Items[I].Text, S, False) then
    begin
      Result := I;
      Break;
    end;
end;

function TcxCheckListBoxItems.IndexOfObject(AObject: TObject): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Objects[I] = AObject then
    begin
      Result := I;
      Break;
    end;
end;

procedure TcxCheckListBoxItems.LoadStrings(AStrings: TStrings);
var
  I: Integer;
begin
  AStrings.Clear;
  for I := 0 to Count - 1 do
    AStrings.Add(Items[I].Text);
end;

{ TcxCustomInnerCheckListBox }

constructor TcxCustomInnerCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FContainer := TcxCustomCheckListBox(AOwner);
  FAllowGrayed := False;
  FAllowDblClickToggle := True;
  FCapturedCheckIndex := -1;
  FCheckItems := TcxCheckListBoxItems.Create(Self, TcxCheckListBoxItem);
  FGlyph := TdxSmartGlyph.Create;
  FGlyph.OnChange := GlyphChanged;
  FGlyphCount := 6;
  FHotCheckIndex := -1;
  FPressedCheckIndex := -1;
  FNewPressedCheckIndex := -1;
  UpdateMetrics;
end;

destructor TcxCustomInnerCheckListBox.Destroy;
begin
  EndMouseTracking(Self);
  FreeAndNil(FGlyph);
  FreeAndNil(FCheckItems);
  inherited Destroy;
end;

procedure TcxCustomInnerCheckListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
    if Style and (LBS_OWNERDRAWFIXED or LBS_OWNERDRAWVARIABLE) = 0 then
      Style := Style or LBS_OWNERDRAWFIXED;
end;

procedure TcxCustomInnerCheckListBox.MouseTrackingMouseLeave;
begin
  InternalMouseMove([], -1, -1);
  EndMouseTracking(Self);
end;

procedure TcxCustomInnerCheckListBox.AdjustItemHeight;
begin
  if HandleAllocated then
  begin
    if Container.FListStyle = lbStandard then
      Perform(LB_SETITEMHEIGHT, 0, GetStandardItemHeight);
    SetExternalScrollBarsParameters;
    FullRepaint;
  end;
end;

procedure TcxCustomInnerCheckListBox.CheckHotTrack;
var
  P: TPoint;
begin
  P := ScreenToClient(GetMouseCursorPos);
  InternalMouseMove(KeyboardStateToShiftState, P.X, P.Y);
end;

procedure TcxCustomInnerCheckListBox.Click;
begin
  if Container.ShowChecks or Container.DataBinding.SetEditMode then
    inherited Click;
end;

function TcxCustomInnerCheckListBox.GetCheckAt(X, Y: Integer): Integer;
var
  P: TPoint;
begin
  P := Point(X, Y);
  Result := ItemAtPos(P, True);
  if Result <> -1 then
    if not PtInRect(GetCheckAreaRect(ItemRect(Result)), P) then
      Result := -1;
end;

function TcxCustomInnerCheckListBox.GetCheckAreaSize: TSize;
var
  ACheckBorderOffset: Integer;
begin
  GetCheckMetrics(Result, ACheckBorderOffset);
  Inc(Result.cx, Metrics.CheckFrameWidth * 2 - ACheckBorderOffset * 2);
  Inc(Result.cy, Metrics.CheckFrameWidth * 2 - ACheckBorderOffset * 2);
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TcxCustomInnerCheckListBox.GetCheckAreaRect(const R: TRect): TRect;
var
  ACheckOffset: Integer;
  ACheckAreaSize: TSize;
begin
  if not Container.ShowChecks then
  begin
    Result := cxEmptyRect;
    Exit;
  end;

  ACheckAreaSize := GetCheckAreaSize;

  ACheckOffset := Metrics.ContentOffset;
  if (Container.ImageLayout = ilBeforeChecks) and IsImageAssigned(Container.Images, 0) then
    Inc(ACheckOffset, dxGetImageSize(Container.Images, ScaleFactor).cx + Metrics.ImageFrameWidth * 2);

  Result := cxRectCenterVertically(R, ACheckAreaSize.cy);
  Result.Bottom := Result.Top + ACheckAreaSize.cy;
  if UseRightToLeftAlignment then
  begin
    Result.Right := R.Right - ACheckOffset;
    Result.Left := Result.Right - ACheckAreaSize.cx;
  end
  else
  begin
    Result.Left := R.Left + ACheckOffset;
    Result.Right := Result.Left + ACheckAreaSize.cx;
  end;
end;

procedure TcxCustomInnerCheckListBox.GetCheckMetrics(out ACheckSize: TSize;
  out ACheckBorderOffset: Integer);
begin
  ACheckSize := GetScaledEditCheckSize(Self.Canvas, Container.Style.LookAndFeel.NativeStyle,
    Glyph, GlyphCount, Container.Style.LookAndFeel.SkinPainter, ScaleFactor);
  ACheckBorderOffset := GetEditCheckBorderOffset(Container.Style.LookAndFeel.Kind,
    Container.FNativeStyle, IsGlyphAssigned(Glyph) and (GlyphCount > 0),
    LookAndFeel.SkinPainter);
end;

function TcxCustomInnerCheckListBox.GetCheckRect(const R: TRect;
  AReturnFullRect: Boolean): TRect;
var
  ACheckBorderOffset: Integer;
  ACheckSize: TSize;
begin
  Result := GetCheckAreaRect(R);
  if cxRectIsEmpty(Result) then
    Exit;

  GetCheckMetrics(ACheckSize, ACheckBorderOffset);
  ACheckSize.cx := ACheckSize.cx - ACheckBorderOffset * 2;
  ACheckSize.cy := ACheckSize.cy - ACheckBorderOffset * 2;

  Result := cxRectCenter(Result, ACheckSize);

  if AReturnFullRect then
    InflateRect(Result, ACheckBorderOffset, ACheckBorderOffset);
end;

function TcxCustomInnerCheckListBox.GetCheckRegionWidth: Integer;
begin
  Result := Metrics.ContentOffset;
  if Container.ShowChecks or IsImageAssigned(Container.Images, 0) then
    Inc(Result, Metrics.TextAreaOffset);
  if Container.ShowChecks then
    Inc(Result, GetCheckAreaSize.cx);
  if IsImageAssigned(Container.Images, 0) then
    Inc(Result, dxGetImageSize(Container.Images, ScaleFactor).cx + Metrics.ImageFrameWidth * 2);
end;

function TcxCustomInnerCheckListBox.GetMetrics: TcxCheckListBoxMetrics;
begin
  Result.CheckFrameWidth := ScaleFactor.Apply(cxCheckListBoxCheckFrameWidth);
  Result.ContentOffset := ScaleFactor.Apply(cxCheckListBoxContentOffset);
  Result.ImageFrameWidth := ScaleFactor.Apply(cxCheckListBoxImageFrameWidth);
  Result.TextAreaOffset := ScaleFactor.Apply(cxCheckListBoxTextAreaOffset);
  Result.TextOffset := ScaleFactor.Apply(cxCheckListBoxTextOffset);
  Result.TextWidthCorrection := ScaleFactor.Apply(cxCheckListBoxTextWidthCorrection);
end;

function TcxCustomInnerCheckListBox.GetStandardItemHeight: Integer;
var
  ACheckBorderOffset: Integer;
  ACheckSize: TSize;
begin
  Canvas.Font := Font;
  Result := cxTextHeight(Canvas.Handle);
  if Container.ShowChecks then
  begin
    GetCheckMetrics(ACheckSize, ACheckBorderOffset);
    Result := Max(Result, ACheckSize.cy - ACheckBorderOffset * 2 + Metrics.CheckFrameWidth * 2);
  end;
  if IsImageAssigned(Container.Images, 0) then
    Result := Max(Result, dxGetImageSize(Container.Images, ScaleFactor).cy + Metrics.ImageFrameWidth * 2);
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TcxCustomInnerCheckListBox.GetVisibleItemCount: Integer;
begin
  Result := Min(Height div GetStandardItemHeight, Count);
end;

procedure TcxCustomInnerCheckListBox.InternalMouseMove(Shift: TShiftState; X, Y: Integer);
var
  ANewHotCheckIndex, ANewPressedCheckIndex: Integer;
begin
  if FCapturedCheckIndex = -1 then
  begin
    if cxShiftStateMoveOnly(Shift) then
      ANewHotCheckIndex := GetCheckAt(X, Y)
    else
      ANewHotCheckIndex := FHotCheckIndex;
    ANewPressedCheckIndex := FPressedCheckIndex;
  end
  else
  begin
    ANewHotCheckIndex := -1;
    if GetCheckAt(X, Y) = FCapturedCheckIndex then
      ANewPressedCheckIndex := FCapturedCheckIndex
    else
      ANewPressedCheckIndex := -1;
  end;
  SynchronizeCheckStates(ANewHotCheckIndex, ANewPressedCheckIndex);
end;

procedure TcxCustomInnerCheckListBox.DblClick;
var
  P: TPoint;
begin
  inherited DblClick;
  if (ItemIndex <> -1) and CheckItems[ItemIndex].Enabled then
  begin
    P := ScreenToClient(GetMouseCursorPos);
    if (GetCheckAt(P.X, P.Y) <> -1) or AllowDblClickToggle then
      ToggleClickCheck(ItemIndex);
  end;
end;

function TcxCustomInnerCheckListBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or Container.FDataBinding.ExecuteAction(Action);
end;

function TcxCustomInnerCheckListBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or Container.FDataBinding.UpdateAction(Action);
end;

procedure TcxCustomInnerCheckListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustItemHeight;
end;

procedure TcxCustomInnerCheckListBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  BeginMouseTracking(Self, GetControlRect(Self), Self);
end;

procedure TcxCustomInnerCheckListBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  InternalMouseMove([], -1, -1);
  EndMouseTracking(Self);
end;

function TcxCustomInnerCheckListBox.GetContainer: TcxCustomCheckListBox;
begin
  Result := TcxCustomCheckListBox(Owner);
end;

procedure TcxCustomInnerCheckListBox.FullRepaint;
var
  R: TRect;
begin
  if HandleAllocated then
  begin
    R := GetControlRect(Self);
    InvalidateRect(Handle, @R, True);
  end;
end;

procedure TcxCustomInnerCheckListBox.WMLButtonDown(var Message: TWMLButtonDown);
var
  R: TRect;
begin
  if Container.ShowChecks or Container.DataBinding.SetEditMode then
  begin
    FNewPressedCheckIndex := GetCheckAt(Message.XPos, Message.YPos);
    try
      if FNewPressedCheckIndex <> -1 then
      begin
        R := ItemRect(FNewPressedCheckIndex);
        FNewPressedCheckItemFullyVisible := R.Bottom <= ClientHeight;
        DragMode := dmManual;
      end;
      inherited;
    finally
      FNewPressedCheckIndex := -1;
      DragMode := Container.DragMode;
    end;
  end
  else
  begin
    SetFocus;
    with Message do
      MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
  end;
end;

procedure TcxCustomInnerCheckListBox.CNDrawItem(var Message: TWMDrawItem);
var
  ADrawItemStruct: PDrawItemStruct;
begin
  if Items.Count = 0 then
    Exit;

  ADrawItemStruct := Message.DrawItemStruct;
  if UseRightToLeftAlignment then
    Dec(ADrawItemStruct^.rcItem.Right, GetCheckRegionWidth)
  else
    Inc(ADrawItemStruct^.rcItem.Left, GetCheckRegionWidth);

  inherited;
end;

procedure TcxCustomInnerCheckListBox.CNMeasureItem(var Message: TWMMeasureItem);
begin
  if Container.FListStyle = lbStandard then
    Message.MeasureItemStruct.itemHeight := GetStandardItemHeight
  else
    inherited;
end;

procedure TcxCustomInnerCheckListBox.CMColorChanged(var Message: TMessage);
begin
  inherited;
  FullRepaint;
end;

procedure TcxCustomInnerCheckListBox.DrawCheck(R: TRect;
  AState: TcxCheckBoxState; ACheckState: TcxEditCheckState);

  function GetCheckBorderStyle: TcxEditCheckBoxBorderStyle;
  begin
    Result := Container.FCheckBorderStyle;
    if (Result = ebsFlat) and not Container.FNativeStyle and (ACheckState in [ecsHot, ecsPressed]) then
      Result := ebs3D;
  end;

begin
  if R.Top < 0 then
    Exit;
  R := GetCheckRect(R, True);
  DrawScaledEditCheck(Canvas, R, AState, ACheckState, Glyph,
    GlyphCount, GetCheckBorderStyle, Container.FNativeStyle, clBtnText, Color,
    True, Container.IsDesigning, False, True, LookAndFeel.SkinPainter, ScaleFactor);
end;

procedure TcxCustomInnerCheckListBox.InvalidateItem(Index: Integer);
begin
  cxInvalidateRect(Handle, ItemRect(Index), True);
end;

procedure TcxCustomInnerCheckListBox.SynchronizeCheckStates(ANewHotCheckIndex,
  ANewPressedCheckIndex: Integer);
begin
  if ANewHotCheckIndex <> FHotCheckIndex then
  begin
    InvalidateCheck(FHotCheckIndex);
    FHotCheckIndex := ANewHotCheckIndex;
    InvalidateCheck(FHotCheckIndex);
  end;
  if ANewPressedCheckIndex <> FPressedCheckIndex then
  begin
    InvalidateCheck(FPressedCheckIndex);
    FPressedCheckIndex := ANewPressedCheckIndex;
    InvalidateCheck(FPressedCheckIndex);
  end;
end;

procedure TcxCustomInnerCheckListBox.UpdateCheckStates;
var
  I: Integer;
  ACheckStates: TcxCheckStates;
begin
  if Assigned(Container.FOnEditValueToCheckStates) then
  begin
    SetLength(ACheckStates, Container.Items.Count);
    Container.FOnEditValueToCheckStates(Container, Container.EditValue,
      ACheckStates);
  end
  else
    CalculateCheckStates(Container.EditValue, CheckItems,
      Container.EditValueFormat, ACheckStates);
  CheckItems.LockChanged(True);
  try
    for I := 0 to CheckItems.Count - 1 do
      CheckItems[I].State := ACheckStates[I];
  finally
    CheckItems.LockChanged(False, False);
  end;
end;

procedure TcxCustomInnerCheckListBox.UpdateEditValue;
var
  ANewEditValue: TcxEditValue;
  AEditValueChanged: Boolean;
  I: Integer;
  ACheckStates: TcxCheckStates;
begin
  SetLength(ACheckStates, CheckItems.Count);
  for I := 0 to CheckItems.Count - 1 do
    ACheckStates[I] := CheckItems[I].State;
  if Assigned(Container.FOnCheckStatesToEditValue) then
    Container.FOnCheckStatesToEditValue(Container, ACheckStates, ANewEditValue)
  else
    ANewEditValue := CalculateCheckStatesValue(ACheckStates, CheckItems,
      Container.EditValueFormat);

  if Assigned(Container.OnEditValueChanged) then
    AEditValueChanged := not InternalVarEqualsExact(Container.FEditValue, ANewEditValue)
  else
    AEditValueChanged := False;

  Container.FEditValue := ANewEditValue;
  Container.IsModified := True;

  if AEditValueChanged then
    Container.DoEditValueChanged;
end;

procedure TcxCustomInnerCheckListBox.UpdateMetrics;
begin
  FMetrics := GetMetrics;
end;

procedure TcxCustomInnerCheckListBox.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  case Message.Msg of
    WM_HSCROLL,
    WM_MOUSEWHEEL,
    WM_VSCROLL:
      CheckHotTrack;
  end;
end;

procedure TcxCustomInnerCheckListBox.InvalidateCheck(Index: Integer);
var
  R: TRect;
begin
  if not HandleAllocated then
    Exit;
  R := ItemRect(Index);
  R := GetCheckRect(R, False);
  InvalidateRect(Handle, @R, False);
end;

procedure TcxCustomInnerCheckListBox.GlyphChanged(Sender: TObject);
begin
  AdjustItemHeight;
end;

procedure TcxCustomInnerCheckListBox.SetGlyph(Value: TdxSmartGlyph);
begin
  Glyph.Assign(Value);
end;

procedure TcxCustomInnerCheckListBox.SetGlyphCount(Value: Integer);
begin
  if FGlyphCount <> Value then
  begin
    FGlyphCount := Value;
    if FGlyph <> nil then
      AdjustItemHeight;
  end;
end;

procedure TcxCustomInnerCheckListBox.KeyPress(var Key: Char);
begin
  if (Key = ' ') then
    ToggleClickCheck(ItemIndex);
  inherited KeyPress(Key);
end;

procedure TcxCustomInnerCheckListBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ANewHotCheckIndex, ANewPressedCheckIndex: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    ANewPressedCheckIndex := FNewPressedCheckIndex;
    if (ANewPressedCheckIndex <> -1) and
      ((Container.DragMode = dmAutomatic) or not FNewPressedCheckItemFullyVisible) and
      CheckItems[ANewPressedCheckIndex].Enabled then
    begin
      ToggleClickCheck(ANewPressedCheckIndex);
      ANewPressedCheckIndex := -1;
    end;
  end
  else
    ANewPressedCheckIndex := -1;
  FCapturedCheckIndex := ANewPressedCheckIndex;
  ANewHotCheckIndex := -1;
  SynchronizeCheckStates(ANewHotCheckIndex, ANewPressedCheckIndex);
end;

procedure TcxCustomInnerCheckListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if (Container.DragMode = dmAutomatic) and (GetCaptureControl <> Self) then
  begin
    FCapturedCheckIndex := -1;
    SynchronizeCheckStates(FHotCheckIndex, -1);
  end;
  InternalMouseMove(Shift, X, Y);
  BeginMouseTracking(Self, GetControlRect(Self), Self);
end;

procedure TcxCustomInnerCheckListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  ACheckIndex: Integer;
  ANewHotCheckIndex, ANewPressedCheckIndex: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbLeft) and (Container.DragMode <> dmAutomatic) then
  begin
    ACheckIndex := GetCheckAt(X, Y);
    if (ACheckIndex = FPressedCheckIndex) and (ACheckIndex <> -1) and
      CheckItems[ACheckIndex].Enabled then
        ToggleClickCheck(ACheckIndex);
  end;
  FCapturedCheckIndex := -1;
  ANewPressedCheckIndex := -1;
  if cxShiftStateMoveOnly(Shift) then
    ANewHotCheckIndex := GetCheckAt(X, Y)
  else
    ANewHotCheckIndex := -1;
  SynchronizeCheckStates(ANewHotCheckIndex, ANewPressedCheckIndex);
end;

function TcxCustomInnerCheckListBox.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  CheckHotTrack;
end;

procedure TcxCustomInnerCheckListBox.ToggleClickCheck(Index: Integer);
var
  ANewState, APrevState: TcxCheckBoxState;
begin
  if (Index < 0) or (Index >= CheckItems.Count) or
      not CheckItems[Index].Enabled then
    Exit;
  if not Container.DataBinding.SetEditMode then
    Exit;

  APrevState := CheckItems[Index].State;
  case APrevState of
    cbsUnchecked:
//TODO:      if AllowGrayed and (Container.GetRealEditValueFormat <> cvfInteger) then
      if AllowGrayed and (Container.EditValueFormat <> cvfInteger) then
        ANewState := cbsGrayed
      else
        ANewState := cbsChecked;
    cbsGrayed: ANewState := cbsChecked;
    else
      ANewState := cbsUnchecked;
  end;
  CheckItems[Index].State := ANewState;
  DoClickCheck(Index, APrevState, ANewState);
end;

procedure TcxCustomInnerCheckListBox.DoClickCheck(const AIndex: Integer;
  const OldState, NewState: TcxCheckBoxState);
begin
  if Assigned(FOnClickCheck) then
    FOnClickCheck(Container, AIndex, OldState, NewState);
end;

{ TcxCustomCheckListBox }

constructor TcxCustomCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditValue := VarAsType(0, varInt64);
  FEditValueFormat := cvfInteger;

  CreateInnerListBox;

  InnerControl := InnerCheckListBox;
  DataBinding.VisualControl := InnerCheckListBox;
  Width := 121;
  Height := 97;

  FImageLayout := ilBeforeChecks;
  FListStyle := lbStandard;
  InnerCheckListBox.Style := lbOwnerDrawFixed;
  FShowChecks := True;
  CalculateDrawCheckParams;

  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImagesChanged;
end;

destructor TcxCustomCheckListBox.Destroy;
begin
  FreeAndNil(FImagesChangeLink);
  inherited Destroy;
end;

function TcxCustomCheckListBox.CheckAtPos(const APos: TPoint): Integer;
begin
  Result := InnerCheckListBox.GetCheckAt(APos.X - InnerCheckListBox.Left,
    APos.Y - InnerCheckListBox.Top);
end;

procedure TcxCustomCheckListBox.Clear;
begin
  Items.Clear;
end;

function TcxCustomCheckListBox.GetBestFitWidth: Integer;
var
  AItemTextWidth, AMaxItemTextWidth: Integer;
  I: Integer;
begin
  with GetBorderExtent do
    Result := Left + Right;
  Inc(Result, InnerCheckListBox.GetCheckRegionWidth +
    InnerCheckListBox.Metrics.TextWidthCorrection);
  AMaxItemTextWidth := 0;
  for I := 0 to Items.Count - 1 do
  begin
    AItemTextWidth := InnerCheckListBox.Canvas.TextWidth(Items[I].Text);
    if AItemTextWidth > AMaxItemTextWidth then
      AMaxItemTextWidth := AItemTextWidth;
  end;
  Inc(Result, AMaxItemTextWidth);
end;

function TcxCustomCheckListBox.GetHeight(ARowCount: Integer): Integer;
begin
  with GetBorderExtent do
    Result := InnerCheckListBox.GetStandardItemHeight * ARowCount + Top + Bottom;
end;

function TcxCustomCheckListBox.GetItemWidth(AIndex: Integer): Integer;
begin
  with GetBorderExtent do
    Result := Left + Right;
  Inc(Result, InnerCheckListBox.GetCheckRegionWidth +
    Canvas.TextWidth(Items[AIndex].Text) + InnerCheckListBox.Metrics.TextWidthCorrection);
end;

function TcxCustomCheckListBox.GetVisibleItemCount: Integer;
begin
  Result := InnerCheckListBox.GetVisibleItemCount;
end;

function TcxCustomCheckListBox.ItemAtPos(const APos: TPoint; AExisting: Boolean): Integer;
begin
  Result := InnerCheckListBox.ItemAtPos(
    Point(APos.X - InnerCheckListBox.Left, APos.Y - InnerCheckListBox.Top),
    AExisting);
end;

function TcxCustomCheckListBox.ItemRect(Index: Integer): TRect;
begin
  Result := InnerCheckListBox.ItemRect(Index);
  OffsetRect(Result, InnerCheckListBox.Left, InnerCheckListBox.Top);
end;

procedure TcxCustomCheckListBox.Sort;
begin
  Items.Sort;
end;

procedure TcxCustomCheckListBox.AddItem(const AItem: string);
var
  ACheckItem: TcxCheckListBoxItem;
begin
  ACheckItem := Items.Add;
  ACheckItem.Text := AItem;
end;

function TcxCustomCheckListBox.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

procedure TcxCustomCheckListBox.CopySelection(ADestination: TcxCustomCheckListBox);
begin
  if ItemIndex <> -1 then
    ADestination.AddItem(Items[ItemIndex].Text);
end;

procedure TcxCustomCheckListBox.DeleteSelected;
begin
  if ItemIndex <> -1 then
    Items.Delete(ItemIndex);
end;

procedure TcxCustomCheckListBox.MoveSelection(ADestination: TcxCustomCheckListBox);
begin
  CopySelection(ADestination);
  DeleteSelected;
end;

procedure TcxCustomCheckListBox.CalculateDrawCheckParams;
const
  BorderStyleMap: array[TcxLookAndFeelKind] of TcxEditCheckBoxBorderStyle = (ebsFlat, ebs3D, ebsUltraFlat, ebsOffice11);
begin
  with Style.LookAndFeel do
  begin
    FNativeStyle := NativeStyle and AreVisualStylesAvailable([totButton, totComboBox]);
    if not FNativeStyle then
      FCheckBorderStyle := BorderStyleMap[Kind];
  end;
end;

procedure TcxCustomCheckListBox.CreateHandle;
begin
  inherited CreateHandle;
  if InnerCheckListBox <> nil then
  begin
    if InnerCheckListBox.Count <> Items.Count then
      Items.SynchronizeInnerListBoxItems;
  end;
end;

procedure TcxCustomCheckListBox.Loaded;
begin
  inherited Loaded;
  DataBinding.Reset;
  Sorted := FLoadedSortedValue;
  cxRecreateControlWnd(InnerCheckListBox);
end;

procedure TcxCustomCheckListBox.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  CalculateDrawCheckParams;
  inherited LookAndFeelChanged(Sender, AChangedValues);
  if InnerCheckListBox <> nil then
  begin
    InnerCheckListBox.UpdateMetrics;
    InnerCheckListBox.AdjustItemHeight;
    cxRecreateControlWnd(InnerCheckListBox);
  end;
end;

procedure TcxCustomCheckListBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil;
end;

procedure TcxCustomCheckListBox.CheckEditValueFormat;
begin
  if (GetRealEditValueFormat = cvfInteger) and (Items.Count > 64) then
    raise EdxException.Create(cxGetResourceString(@cxSCheckControlIncorrectItemCount));
end;

function TcxCustomCheckListBox.GetRealEditValueFormat: TcxCheckStatesValueFormatEx;
begin
  if Assigned(OnEditValueToCheckStates) and Assigned(OnCheckStatesToEditValue) then
    Result := cvfCustom
  else
    Result := EditValueFormat;
end;

procedure TcxCustomCheckListBox.ItemsChanged(Sender: TObject; AItem: TCollectionItem);
begin
  CheckEditValueFormat;
end;

procedure TcxCustomCheckListBox.CorrectAlignControlRect(var R: TRect);
begin
  R := cxRectContent(R, GetBorderExtent);
end;

procedure TcxCustomCheckListBox.FontChanged;
begin
  inherited FontChanged;
  SetSize;
end;

procedure TcxCustomCheckListBox.DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);

  function GetDisabledTextColor: TColor;
  var
    AColor: TColor;
  begin
    AColor := LookAndFeelPainter.DefaultEditorTextColor(True);
    if (csvTextColor in StyleDisabled.AssignedValues) or (AColor = clDefault) then
      Result := StyleDisabled.TextColor
    else
      Result := AColor;
  end;

  procedure PrepareColors(AIsItemEnabled: Boolean);
  begin
    if (odSelected in State) and not IsDesigning then
    begin
      InnerCheckListBox.Canvas.Font.Color := LookAndFeelPainter.DefaultSelectionTextColor;
      InnerCheckListBox.Canvas.Brush.Color := LookAndFeelPainter.DefaultSelectionColor;
    end
    else
    begin
      InnerCheckListBox.Canvas.Brush.Color := ViewInfo.BackgroundColor;
      InnerCheckListBox.Canvas.Font.Color := VisibleFont.Color;
    end;
    if not (AIsItemEnabled or (odSelected in State)) then
      InnerCheckListBox.Canvas.Font.Color := GetDisabledTextColor;
  end;

  function GetCheckState(AIsItemEnabled: Boolean): TcxEditCheckState;
  begin
    if not AIsItemEnabled then
      Result := ecsDisabled
    else
      if InnerCheckListBox.FHotCheckIndex = Index then
        Result := ecsHot
      else
        if InnerCheckListBox.FPressedCheckIndex = Index then
          Result := ecsPressed
        else
          Result := ecsNormal;
  end;

  procedure DrawCheckRegion(const ACheckRegion: TRect; AIsItemEnabled: Boolean);

    function GetGlyphRect: TRect;
    var
      ASize: TSize;
      AGlyphOffset: Integer;
    begin
      AGlyphOffset := InnerCheckListBox.Metrics.ContentOffset +
        InnerCheckListBox.Metrics.ImageFrameWidth;
      if ShowChecks and (ImageLayout = ilAfterChecks) then
        Inc(AGlyphOffset, InnerCheckListBox.GetCheckAreaSize.cx);

      ASize := dxGetImageSize(Images, ScaleFactor);
      Result.Top := ACheckRegion.Top + (ACheckRegion.Bottom - ACheckRegion.Top - ASize.cy) div 2;
      Result.Bottom := Result.Top + ASize.cy;
      if UseRightToLeftAlignment then
      begin
        Result.Right := ACheckRegion.Right - AGlyphOffset;
        Result.Left := Result.Right - ASize.cx;
      end
      else
      begin
        Result.Left := ACheckRegion.Left + AGlyphOffset;
        Result.Right := Result.Left + ASize.cx;
      end;
    end;

  var
    AImageIndex: Integer;
    R: TRect;
    ABitmap: TcxBitmap;
  begin
    if ShowChecks then
      InnerCheckListBox.DrawCheck(ACheckRegion, Items[Index].State,
        GetCheckState(AIsItemEnabled));
    AImageIndex := Items[Index].ImageIndex;
    if IsImageAssigned(Images, AImageIndex) then
    begin
      R := GetGlyphRect;
      ABitmap := TcxBitmap.CreateSize(R);
      try
        ABitmap.cxCanvas.FillRect(ABitmap.ClientRect, ViewInfo.BackgroundColor);
        cxDrawImage(ABitmap.cxCanvas.Handle, ABitmap.ClientRect, ABitmap.ClientRect,
          nil, Images, AImageIndex, EnabledImageDrawModeMap[AIsItemEnabled]);
        cxBitBlt(InnerCheckListBox.Canvas.Handle, ABitmap.cxCanvas.Handle, R, cxNullPoint, SRCCOPY);
      finally
        ABitmap.Free;
      end;
    end;
  end;

var
  ACheckRegion, ATextRect, ADrawEventRect: TRect;
  AEnabled: Boolean;
  AText: string;
  APrevMode: Integer;
begin
  if Index < Items.Count then
  begin
    InnerCheckListBox.Canvas.Font.Assign(VisibleFont);
    ACheckRegion := Rect;
    ATextRect := Rect;
    AEnabled := Enabled and Items[Index].Enabled;
    if not UseRightToLeftAlignment then
    begin
      Dec(ACheckRegion.Left, InnerCheckListBox.GetCheckRegionWidth);
      Inc(ATextRect.Left, InnerCheckListBox.Metrics.TextOffset);
    end
    else
    begin
      Inc(ACheckRegion.Right, InnerCheckListBox.GetCheckRegionWidth);
      Dec(ATextRect.Right, InnerCheckListBox.Metrics.TextOffset);
    end;
    DrawCheckRegion(ACheckRegion, AEnabled);
    PrepareColors(AEnabled);
    InnerCheckListBox.Canvas.FillRect(Rect);
    AText := Items[Index].Text;
    PrepareColors(AEnabled);
    APrevMode := SetBkMode(InnerCheckListBox.Canvas.Handle, Windows.TRANSPARENT);
    DrawItemText(AText, ATextRect);
    if odFocused in State then
      InnerCheckListBox.Canvas.DrawFocusRect(Rect);
    SetBkMode(InnerCheckListBox.Canvas.Handle, APrevMode);
  end;
  ADrawEventRect := Rect;
  if Assigned(FOnDrawItem) then
    FOnDrawItem(Self, Index, ADrawEventRect, State);
end;

procedure TcxCustomCheckListBox.DrawItemText(const AText: string; const ARect: TRect);
const
  ADrawTextAlignmentFlags: array[Boolean] of LongWord = (DT_LEFT, DT_RIGHT);
var
  ABaseTestFlag: Cardinal;
  ADrawTextParams: tagDRAWTEXTPARAMS;
  R: TRect;
begin
  ABaseTestFlag := DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE or ADrawTextAlignmentFlags[UseRightToLeftAlignment];
  if InnerCheckListBox.TabWidth > 0 then
    ABaseTestFlag := ABaseTestFlag or DT_EXPANDTABS or DT_TABSTOP;
  if UseRightToLeftReading then
    ABaseTestFlag := ABaseTestFlag or DT_RTLREADING;
  with ADrawTextParams do
  begin
    cbSize := SizeOf(ADrawTextParams);
    iTabLength := InnerCheckListBox.TabWidth;
    iLeftMargin := 0;
    iRightMargin := 0;
  end;
  R := ARect;
  Windows.DrawTextEx(InnerCheckListBox.Canvas.Handle, PChar(AText), Length(AText),
    R, ABaseTestFlag, @ADrawTextParams);
end;

function TcxCustomCheckListBox.IsInternalControl(AControl: TControl): Boolean;
begin
  Result := (AControl = InnerCheckListBox.HScrollBar) or (AControl = InnerCheckListBox.VScrollBar) or
    inherited IsInternalControl(AControl);
end;

procedure TcxCustomCheckListBox.DataChange;
begin
  if ShowChecks then
    EditValue := DataBinding.GetStoredValue(evsValue, Focused)
  else
    inherited DataChange;
end;

function TcxCustomCheckListBox.DoCreateInnerListBox: TcxCustomInnerListBox;
begin
  Result := GetInnerCheckListBoxClass.Create(Self);
end;

procedure TcxCustomCheckListBox.UpdateData;
begin
  if ShowChecks then
    DataBinding.SetStoredValue(evsValue, EditValue)
  else
    inherited UpdateData;
end;

procedure TcxCustomCheckListBox.DoSetSize;
var
  ANewHeight: Integer;
begin
  if IsLoading then
    Exit;
// TODO
//  try
    if not IntegralHeight or (Align in [alLeft, alRight, alClient]) then
    begin
      inherited;
      Exit;
    end;
    ANewHeight := Height;
    GetOptimalHeight(ANewHeight);
    Height := ANewHeight;
    inherited;
//  finally
//    if InnerCheckListBox.HandleAllocated then
//      KillMouseMoveMessages;
//  end;
end;

procedure TcxCustomCheckListBox.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  if (InnerCheckListBox <> nil) and (Message.Msg = WM_COMMAND) and (Message.WParamHi = LBN_SELCHANGE) then
    InnerCheckListBox.SetExternalScrollBarsParameters;
end;

function TcxCustomCheckListBox.GetOnClickCheck : TcxClickCheckEvent;
begin
  Result := InnerCheckListBox.FOnClickCheck;
end;

function TcxCustomCheckListBox.GetOnCompare: TcxCollectionCompareEvent;
begin
  Result := Items.OnCompare;
end;

function TcxCustomCheckListBox.GetAllowGrayed : Boolean;
begin
  Result := InnerCheckListBox.FAllowGrayed;
end;

function TcxCustomCheckListBox.GetAllowDblClickToggle: Boolean;
begin
  Result := InnerCheckListBox.AllowDblClickToggle;
end;

function TcxCustomCheckListBox.GetGlyph: TdxSmartGlyph;
begin
  Result := InnerCheckListBox.Glyph;
end;

function TcxCustomCheckListBox.GetItems: TcxCheckListBoxItems;
begin
  Result := InnerCheckListBox.CheckItems;
end;

function TcxCustomCheckListBox.GetColumns: Integer;
begin
  Result := InnerCheckListBox.Columns;
end;

function TcxCustomCheckListBox.GetSorted: Boolean;
begin
  Result := Items.Sorted;
end;

procedure TcxCustomCheckListBox.ImagesChanged(Sender: TObject);
begin
  if InnerCheckListBox <> nil then
    InnerCheckListBox.AdjustItemHeight;
end;

function TcxCustomCheckListBox.IsItemHeightStored: Boolean;
begin
  Result := FListStyle <> lbStandard;
end;

procedure TcxCustomCheckListBox.SetOnClickCheck(Value: TcxClickCheckEvent);
begin
  InnerCheckListBox.FOnClickCheck := Value;
end;

procedure TcxCustomCheckListBox.SetOnCompare(Value: TcxCollectionCompareEvent);
begin
  Items.OnCompare := Value;
end;

procedure TcxCustomCheckListBox.SetAllowGrayed(Value: Boolean);
begin
  InnerCheckListBox.FAllowGrayed := Value;
end;

procedure TcxCustomCheckListBox.SetAllowDblClickToggle(Value: Boolean);
begin
  InnerCheckListBox.AllowDblClickToggle := Value;
end;

procedure TcxCustomCheckListBox.SetEditValueFormat(Value: TcxCheckStatesValueFormat);

  procedure ResetGrayedStates;
  var
    I: Integer;
  begin
    Items.LockChanged(True);
    try
      for I := 0 to Items.Count - 1 do
        if Items[I].State = cbsGrayed then
          Items[I].State := cbsUnchecked;
    finally
      Items.LockChanged(False, False);
    end;
  end;

begin
  if Value <> FEditValueFormat then
  begin
    FEditValueFormat := Value;
    if IsModified then
    begin
      if Value = cvfInteger then
        ResetGrayedStates;
      InnerCheckListBox.UpdateEditValue;
    end
    else
      InnerCheckListBox.UpdateCheckStates;
    CheckEditValueFormat;
  end;
end;

procedure TcxCustomCheckListBox.SetGlyph(Value: TdxSmartGlyph);
begin
  InnerCheckListBox.SetGlyph(Value);
end;

procedure TcxCustomCheckListBox.SetGlyphCount(Value: Integer);
begin
  InnerCheckListBox.SetGlyphCount(Value);
end;

procedure TcxCustomCheckListBox.SetItemHeight(Value: Integer);
begin
  if FListStyle <> lbStandard then
    inherited;
end;

procedure TcxCustomCheckListBox.SetItems(Value: TcxCheckListBoxItems);
begin
  InnerCheckListBox.CheckItems.Assign(Value);
  DataChange;
end;

procedure TcxCustomCheckListBox.SetColumns(Value: Integer);
begin
  InnerCheckListBox.Columns := Value;
end;

procedure TcxCustomCheckListBox.SetImageLayout(Value: TcxCheckListBoxImageLayout);
begin
  if Value <> FImageLayout then
  begin
    FImageLayout := Value;
    InnerCheckListBox.FullRepaint;
  end;
end;

procedure TcxCustomCheckListBox.SetListStyle(Value: TListBoxStyle);
begin
  if Value <> FListStyle then
  begin
    FListStyle := Value;
    if Value = lbStandard then
      Value := lbOwnerDrawFixed;
    if InnerCheckListBox.Style = Value then
      InnerCheckListBox.RecreateWnd
    else
      inherited SetListStyle(Value);
  end;
end;

function TcxCustomCheckListBox.GetGlyphCount: Integer;
begin
  Result := InnerCheckListBox.GlyphCount;
end;

function TcxCustomCheckListBox.GetStateCaption(AIndex: Integer): string;
begin
  Result := GetCheckBoxStateCaptionByGlyphIndex(AIndex);
end;

procedure TcxCustomCheckListBox.SetImages(Value: TCustomImageList);
begin
  cxSetImageList(Value, FImages, FImagesChangeLink, Self);
end;

procedure TcxCustomCheckListBox.SetShowChecks(Value: Boolean);
begin
  if Value <> FShowChecks then
  begin
    FShowChecks := Value;
    InnerCheckListBox.AdjustItemHeight;
    DataBinding.Reset;
  end;
end;

procedure TcxCustomCheckListBox.SetSorted(Value: Boolean);
begin
  if IsLoading then
    FLoadedSortedValue := Value
  else
    Items.Sorted := Value;
end;

procedure TcxCustomCheckListBox.DoEditValueChanged;
begin
  if Assigned(FOnEditValueChanged) then
    FOnEditValueChanged(Self);
end;

procedure TcxCustomCheckListBox.DoScrollUIModeChanged;
begin
  if InnerCheckListBox <> nil then
    InnerCheckListBox.RecreateWnd;
end;

function TcxCustomCheckListBox.GetInnerCheckListBox: TcxCustomInnerCheckListBox;
begin
  Result := InnerListBox as TcxCustomInnerCheckListBox;
end;

function TcxCustomCheckListBox.GetInnerCheckListBoxClass: TcxCustomInnerCheckListBoxClass;
begin
  Result := TcxCustomInnerCheckListBox;
end;

function TcxCustomCheckListBox.GetItemText(AItemIndex: Integer): string;
begin
  Result := Items[AItemIndex].Text;
end;

function TcxCustomCheckListBox.GetListItemHeight(AIndex: Integer): Integer;
begin
  Result := InnerCheckListBox.GetStandardItemHeight;
end;

function TcxCustomCheckListBox.GetListStyle: TListBoxStyle;
begin
  Result := FListStyle;
end;

function TcxCustomCheckListBox.IndexOf(const S: string): Integer;
begin
  Result := Items.IndexOf(S);
end;

procedure TcxCustomCheckListBox.InitializeInnerListBox;
begin
  InnerCheckListBox.AutoSize := False;
  InnerCheckListBox.BorderStyle := bsNone;
  InnerCheckListBox.OnDrawItem := DrawItem;
  InnerCheckListBox.Parent := Self;
  InnerCheckListBox.LookAndFeel.MasterLookAndFeel := Style.LookAndFeel;
  InnerCheckListBox.CheckItems.OnChange := ItemsChanged;
end;

procedure TcxCustomCheckListBox.InternalKeyDown(var Key: Word; Shift: TShiftState);
begin
  if not ShowChecks then
    inherited InternalKeyDown(Key, Shift);
end;

function TcxCustomCheckListBox.IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := False;
end;

function TcxCustomCheckListBox.IsValueValid(const AValue: Variant;
  AAllowEmpty: Boolean): Boolean;
begin
  Result := False;
  if (IsVarEmpty(AValue) and AAllowEmpty) or VarIsNumericEx(AValue) then
    Result := True
  else
    if VarIsStr(AValue) then
      Result := IsValidStringForInt(VarToStr(AValue)) or
        IsValidStringForDouble(VarToStr(AValue));
end;

function TcxCustomCheckListBox.NeedIgnorePressedKey(var Key: Char): Boolean;
begin
  Result := not ShowChecks and inherited NeedIgnorePressedKey(Key);
end;

function TcxCustomCheckListBox.NeedsScrollBars: Boolean;
begin
  Result := True;
end;

procedure TcxCustomCheckListBox.SetEditValue(const Value: TcxEditValue);
var
  AEditValueChanged: Boolean;
begin
  IsModified := False;

  if Assigned(FOnEditValueChanged) then
    AEditValueChanged := not InternalVarEqualsExact(Value, FEditValue)
  else
    AEditValueChanged := False;

  FEditValue := Value;
  InnerCheckListBox.UpdateCheckStates;

  if AEditValueChanged then
    FOnEditValueChanged(Self);
end;

end.
