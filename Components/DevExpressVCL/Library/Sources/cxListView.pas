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

unit cxListView;

{$I cxVer.inc}
{$DEFINE USETCXSCROLLBAR}

interface

uses
  Windows, Forms, Classes, ComCtrls, CommCtrl, Controls, ImgList, Menus,
  Messages, StdCtrls, SysUtils,
  dxCore, dxMessages, cxClasses, cxContainer, cxControls, cxGraphics,
  cxGeometry, cxLookAndFeels, cxLookAndFeelPainters, cxScrollBar, cxExtEditConsts, cxHeader, cxEdit, dxTypeHelpers;

const
  cxiaTop = TIconArrangement(0);

type
  TcxCustomListView = class;
  TcxListViewContainer = class;

  { TcxIconOptions }

  TcxIconOptions = class(TPersistent)
  private
    FInnerIconOptions: TIconOptions;
    FListViewContainer: TcxListViewContainer;
    function GetArrangement: TIconArrangement;
    function GetAutoArrange: Boolean;
    function GetWrapText: Boolean;
    procedure SetArrangement(Value: TIconArrangement);
    procedure SetAutoArrange(Value: Boolean);
    procedure SetWrapText(Value: Boolean);
  protected
    property ListViewContainer: TcxListViewContainer read FListViewContainer;
  public
    constructor Create(AOwner: TcxListViewContainer);
    procedure Assign(Source: TPersistent); override;
  published
    property Arrangement: TIconArrangement read GetArrangement write SetArrangement default cxiaTop;
    property AutoArrange: Boolean read GetAutoArrange write SetAutoArrange default False;
    property WrapText: Boolean read GetWrapText write SetWrapText default True;
  end;

  TcxIconOptionsClass = class of TcxIconOptions;

  TcxInnerListViewClass = class of TcxBaseInnerListView;
  TcxBaseInnerListView = class(TListView, IUnknown, IcxContainerInnerControl)
  private
    FCanvas: TcxCanvas;
    FContainerMouseUpNeeded: Boolean;
    FHeaderHandle: HWND;
    FPressedHeaderItemIndex: Integer;
    FWindowProcObject: TcxWindowProcLinkedObject;
    FIsStored: Boolean;
    FScrollUIActivityHelper: TdxTouchScrollUIActivityHelper;

    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    // header
    function GetHeaderHotItemIndex: Integer;
    function GetHeaderItemRect(AItemIndex: Integer): TRect;
    function GetHeaderPressedItemIndex: Integer;
    function HeaderItemIndex(AHeaderItem: Integer): Integer;
    procedure HeaderWndProc(var Message: TMessage);
    // IcxContainerInnerControl
    function GetControl: TWinControl;
    function GetControlContainer: TcxContainer;

    function GetLookAndFeel: TcxLookAndFeel;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    procedure HScrollHandler(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure VScrollHandler(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);

    procedure WMGestureNotify(var Message: TWMGestureNotify); message WM_GESTURENOTIFY;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure LVMGetHeaderItemInfo(var Message: TDXMHeaderItemInfo); message DXM_GETHEADERITEMINFO;
  protected
    FContainer: TcxListViewContainer;

    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  {$IFNDEF DELPHI10SEATTLE}
    procedure ChangeScale(M, D: Integer); override;
  {$ENDIF}

    procedure Click; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WndProc(var Message: TMessage); override;

    procedure DrawHeader; virtual;
    function GetClientOrigin: TPoint; override;
    function GetSortOrder(AColumnIndex: Integer): TdxSortOrder; virtual;
    procedure HeaderInvalidate;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); virtual;
    procedure MouseEnter(AControl: TControl); dynamic;
    procedure MouseLeave(AControl: TControl); dynamic;
    function NeedCheckScrollBars(var Message: TMessage): Boolean; virtual;

    property Canvas: TcxCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanFocus: Boolean; override;
    procedure DefaultHandler(var Message); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  { TcxCustomInnerListView }

  TcxCustomInnerListViewClass = class of TcxCustomInnerListView;
  TcxCustomInnerListView = class(TcxBaseInnerListView)
  private
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    function GetContainer: TcxCustomListView;
  protected
    function CanEdit(Item: TListItem): Boolean; override;
    procedure DoStartDock(var DragObject: TDragObject); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoCancelEdit; virtual;

    property Container: TcxCustomListView read GetContainer;
  public
    procedure DeleteSelected; override;
  end;

  { TcxListViewContainer }

  TcxListViewContainer = class(TcxCustomEditContainer)
  private
    FIconOptions: TcxIconOptions;
    FInnerListView: TcxBaseInnerListView;

    function GetMultiSelect: Boolean;
    function GetReadOnly: Boolean;
    function GetShowColumnHeaders: Boolean;
    function GetViewStyle: TViewStyle;
    procedure SetIconOptions(Value: TcxIconOptions);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetShowColumnHeaders(Value: Boolean);
  protected
    function AllowTouchScrollUIMode: Boolean; override;
    procedure CorrectAlignControlRect(var R: TRect); override;
    function GetIconOptionsClass: TcxIconOptionsClass; virtual;
    function GetInnerListViewClass: TcxInnerListViewClass; virtual;
    function GetScrollBarBounds(const AScrollBarRect: TRect): TRect; override;
    function HandleInnerContolGestures: Boolean; override;
    procedure InitializeInnerListView; virtual;
    function IsPanArea(const APoint: TPoint): Boolean; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    function NeedsScrollBars: Boolean; override;
    procedure SetReadOnly(Value: Boolean); virtual;
    procedure SetViewStyle(Value: TViewStyle); virtual;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    function UseSystemRightToLeftLayoutForInnerControl: Boolean; override;

    property IconOptions: TcxIconOptions read FIconOptions write SetIconOptions;
    property InnerListView: TcxBaseInnerListView read FInnerListView;
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowColumnHeaders: Boolean read GetShowColumnHeaders write SetShowColumnHeaders default True;
    property ViewStyle: TViewStyle read GetViewStyle write SetViewStyle default vsIcon;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TcxCustomListView }

  TcxCustomListView = class(TcxListViewContainer)
  private
    FOnCancelEdit: TNotifyEvent;
    FOwnerDraw: Boolean;
    procedure CheckInnerListViewAlign;
    function GetListItems: TListItems;
    function GetListColumns: TListColumns;
    function GetListViewCanvas: TcxCanvas;
    function GetColumnClick: Boolean;
    function GetHideSelection: Boolean;
    function GetAllocBy: Integer;
    function GetHoverTime: Integer;
    function GetLargeImages: TCustomImageList;
    function GetOwnerData: Boolean;
    function GetOwnerDraw: Boolean;
    function GetOnAdvancedCustomDraw: TLVAdvancedCustomDrawEvent;
    function GetOnAdvancedCustomDrawItem: TLVAdvancedCustomDrawItemEvent;
    function GetOnAdvancedCustomDrawSubItem: TLVAdvancedCustomDrawSubItemEvent;
    function GetOnChange: TLVChangeEvent;
    function GetOnChanging: TLVChangingEvent;
    function GetOnColumnClick: TLVColumnClickEvent;
    function GetOnColumnDragged: TNotifyEvent;
    function GetOnColumnRightClick: TLVColumnRClickEvent;
    function GetOnCompare: TLVCompareEvent;
    function GetOnCustomDraw: TLVCustomDrawEvent;
    function GetOnCustomDrawItem: TLVCustomDrawItemEvent;
    function GetOnCustomDrawSubItem: TLVCustomDrawSubItemEvent;
    function GetOnData: TLVOwnerDataEvent;
    function GetOnDataFind: TLVOwnerDataFindEvent;
    function GetOnDataHint: TLVOwnerDataHintEvent;
    function GetOnDataStateChange: TLVOwnerDataStateChangeEvent;
    function GetOnDeletion: TLVDeletedEvent;
    function GetOnDrawItem: TLVDrawItemEvent;
    function GetOnEdited: TLVEditedEvent;
    function GetOnEditing: TLVEditingEvent;
    function GetOnInfoTip: TLVInfoTipEvent;
    function GetOnInsert: TLVDeletedEvent;
    function GetOnGetImageIndex: TLVNotifyEvent;
    function GetOnGetSubItemImage: TLVSubItemImageEvent;
    function GetShowWorkAreas: Boolean;
    function GetOnSelectItem: TLVSelectItemEvent;
    function GetSmallImages: TCustomImageList;
    function GetSortType: TSortType;
    function GetStateImages: TCustomImageList;
    function GetOnCreateItemClass: TLVCreateItemClassEvent;
    procedure SetListItems(Value: TListItems);
    procedure SetListColumns(Value: TListColumns);
    procedure SetColumnClick(Value: Boolean);
    procedure SetHideSelection(Value: Boolean);
    procedure SetAllocBy(Value: Integer);
    procedure SetHoverTime(Value: Integer);
    procedure SetLargeImages(Value: TCustomImageList);
    procedure SetOwnerData(Value: Boolean);
    procedure SetOwnerDraw(Value: Boolean);
    procedure SetOnAdvancedCustomDraw(Value: TLVAdvancedCustomDrawEvent);
    procedure SetOnAdvancedCustomDrawItem(Value: TLVAdvancedCustomDrawItemEvent);
    procedure SetOnAdvancedCustomDrawSubItem(Value: TLVAdvancedCustomDrawSubItemEvent);
    procedure SetOnChange(Value: TLVChangeEvent);
    procedure SetOnChanging(Value: TLVChangingEvent);
    procedure SetOnColumnClick(Value: TLVColumnClickEvent);
    procedure SetOnColumnDragged(Value: TNotifyEvent);
    procedure SetOnColumnRightClick(Value: TLVColumnRClickEvent);
    procedure SetOnCompare(Value: TLVCompareEvent);
    procedure SetOnCustomDraw(Value: TLVCustomDrawEvent);
    procedure SetOnCustomDrawItem(Value: TLVCustomDrawItemEvent);
    procedure SetOnCustomDrawSubItem(Value: TLVCustomDrawSubItemEvent);
    procedure SetOnData(Value: TLVOwnerDataEvent);
    procedure SetOnDataFind(Value: TLVOwnerDataFindEvent);
    procedure SetOnDataHint(Value: TLVOwnerDataHintEvent);
    procedure SetOnDataStateChange(Value: TLVOwnerDataStateChangeEvent);
    procedure SetOnDeletion(Value: TLVDeletedEvent);
    procedure SetOnDrawItem(Value: TLVDrawItemEvent);
    procedure SetOnEdited(Value: TLVEditedEvent);
    procedure SetOnEditing(Value: TLVEditingEvent);
    procedure SetOnInfoTip(Value: TLVInfoTipEvent);
    procedure SetOnInsert(Value: TLVDeletedEvent);
    procedure SetOnGetImageIndex(Value: TLVNotifyEvent);
    procedure SetOnGetSubItemImage(Value: TLVSubItemImageEvent);
    procedure SetShowWorkAreas(Value: Boolean);
    procedure SetOnSelectItem(Value: TLVSelectItemEvent);
    procedure SetSmallImages(Value: TCustomImageList);
    procedure SetSortType(Value: TSortType);
    procedure SetStateImages(Value: TCustomImageList);
    procedure SetOnCreateItemClass(Value: TLVCreateItemClassEvent);
    function GetCheckBoxes: Boolean;
    function GetColumnFromIndex(Index: Integer): TListColumn;
    function GetDropTarget: TListItem;
    function GetFullDrag: Boolean;
    function GetGridLines: Boolean;
    function GetHotTrack: Boolean;
    function GetHotTrackStyles: TListHotTrackStyles;
    function GetInnerListView: TcxCustomInnerListView;
    function GetItemFocused: TListItem;
    function GetRowSelect: Boolean;
    function GetSelCount: Integer;
    function GetSelected: TListItem;
    function GetTopItem: TListItem;
    function GetViewOrigin: TPoint;
    function GetVisibleRowCount: Integer;
    function GetBoundingRect: TRect;
    function GetWorkAreas: TWorkAreas;
    procedure SetCheckboxes(Value: Boolean);
    procedure SetDropTarget(Value: TListItem);
    procedure SetFullDrag(Value: Boolean);
    procedure SetGridLines(Value: Boolean);
    procedure SetHotTrack(Value: Boolean);
    procedure SetHotTrackStyles(Value: TListHotTrackStyles);
    procedure SetItemFocused(Value: TListItem);
    procedure SetRowSelect(Value: Boolean);
    procedure SetSelected(Value: TListItem);
  protected
    procedure DoExit; override;
    procedure FontChanged; override;
    function IsReadOnly: Boolean; override;
    procedure Loaded; override;
    procedure WriteState(Writer: TWriter); override;
    function CanChange(Item: TListItem; Change: Integer): Boolean;
    function CanEdit(Item: TListItem): Boolean;
    function ColumnsShowing: Boolean;
    procedure DoScrollUIModeChanged; override;
    function GetCount: Integer;
    function GetInnerListViewClass: TcxInnerListViewClass; override;
    function GetItemIndex: Integer; overload;
    class function GetListViewClass: TcxCustomInnerListViewClass; virtual;
    function GetListViewItemIndex: Integer;
    procedure InitializeInnerListView; override;
    procedure SetItemIndex(Value: Integer);
    procedure SetReadOnly(Value: Boolean); override;
    function GetItemIndex(Value: TListItem): Integer; overload;
    procedure UpdateColumn(AnIndex: Integer);
    procedure UpdateColumns;

    property Columns: TListColumns read GetListColumns write SetListColumns;
    property ColumnClick: Boolean read GetColumnClick write SetColumnClick default True;
    property HideSelection: Boolean read GetHideSelection write SetHideSelection default True;
    property Items: TListItems read GetListItems write SetListItems;
    property AllocBy: Integer read GetAllocBy write SetAllocBy default 0;
    property HoverTime: Integer read GetHoverTime write SetHoverTime default -1;
    property ListViewCanvas: TcxCanvas read GetListViewCanvas;
    property LargeImages: TCustomImageList read GetLargeImages write SetLargeImages;
    property OwnerData: Boolean read GetOwnerData write SetOwnerData default False;
    property OwnerDraw: Boolean read GetOwnerDraw write SetOwnerDraw default False;
    property RowSelect: Boolean read GetRowSelect write SetRowSelect default False;
    property ShowWorkAreas: Boolean read GetShowWorkAreas write SetShowWorkAreas default False;
    property SmallImages: TCustomImageList read GetSmallImages write SetSmallImages;
    property SortType: TSortType read GetSortType write SetSortType default stNone;
    property StateImages: TCustomImageList read GetStateImages write SetStateImages;
    property OnAdvancedCustomDraw: TLVAdvancedCustomDrawEvent read GetOnAdvancedCustomDraw
      write SetOnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem: TLVAdvancedCustomDrawItemEvent
      read GetOnAdvancedCustomDrawItem write SetOnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem: TLVAdvancedCustomDrawSubItemEvent
      read GetOnAdvancedCustomDrawSubItem write SetOnAdvancedCustomDrawSubItem;
    property OnCancelEdit: TNotifyEvent read FOnCancelEdit write FOnCancelEdit;
    property OnChange: TLVChangeEvent read GetOnChange write SetOnChange;
    property OnChanging: TLVChangingEvent read GetOnChanging write SetOnChanging;
    property OnColumnClick: TLVColumnClickEvent read GetOnColumnClick write SetOnColumnClick;
    property OnColumnDragged: TNotifyEvent read GetOnColumnDragged write SetOnColumnDragged;
    property OnColumnRightClick: TLVColumnRClickEvent read GetOnColumnRightClick
      write SetOnColumnRightClick;
    property OnCompare: TLVCompareEvent read GetOnCompare write SetOnCompare;
    property OnCustomDraw: TLVCustomDrawEvent read GetOnCustomDraw write SetOnCustomDraw;
    property OnCustomDrawItem: TLVCustomDrawItemEvent read GetOnCustomDrawItem
      write SetOnCustomDrawItem;
    property OnCustomDrawSubItem: TLVCustomDrawSubItemEvent read GetOnCustomDrawSubItem
      write SetOnCustomDrawSubItem;
    property OnData: TLVOwnerDataEvent read GetOnData write SetOnData;
    property OnDataFind: TLVOwnerDataFindEvent read GetOnDataFind write SetOnDataFind;
    property OnDataHint: TLVOwnerDataHintEvent read GetOnDataHint write SetOnDataHint;
    property OnDataStateChange: TLVOwnerDataStateChangeEvent read GetOnDataStateChange
      write SetOnDataStateChange;
    property OnDeletion: TLVDeletedEvent read GetOnDeletion write SetOnDeletion;
    property OnDrawItem: TLVDrawItemEvent read GetOnDrawItem write SetOnDrawItem;
    property OnEdited: TLVEditedEvent read GetOnEdited write SetOnEdited;
    property OnEditing: TLVEditingEvent read GetOnEditing write SetOnEditing;
    property OnInfoTip: TLVInfoTipEvent read GetOnInfoTip write SetOnInfoTip;
    property OnInsert: TLVDeletedEvent read GetOnInsert write SetOnInsert;
    property OnGetImageIndex: TLVNotifyEvent read GetOnGetImageIndex write SetOnGetImageIndex;
    property OnGetSubItemImage: TLVSubItemImageEvent read GetOnGetSubItemImage
      write SetOnGetSubItemImage;
    property OnSelectItem: TLVSelectItemEvent read GetOnSelectItem write SetOnSelectItem;
    property OnCreateItemClass: TLVCreateItemClassEvent read GetOnCreateItemClass
      write SetOnCreateItemClass;
  public
    constructor Create(AOwner: TComponent); override;
    function AlphaSort: Boolean;
    procedure Arrange(Code: TListArrangement);
    procedure Clear;
    procedure ClearSelection;
    procedure AddItem(Item: string; AObject: TObject);
    procedure CopySelection(Destination: TCustomListControl);
    procedure DeleteSelected;
    procedure SelectAll;
    function FindCaption(StartIndex: Integer; Value: string;
      Partial, Inclusive, Wrap: Boolean): TListItem;
    function FindData(StartIndex: Integer; Value: Pointer;
      Inclusive, Wrap: Boolean): TListItem;
    function GetHitTestInfoAt(X, Y: Integer): THitTests;
    function GetItemAt(X, Y: Integer): TListItem;
    function GetNearestItem(Point: TPoint;
      Direction: TSearchDirection): TListItem;
    function GetNextItem(StartItem: TListItem;
      Direction: TSearchDirection; States: TItemStates): TListItem;
    function GetSearchString: string;
    function IsEditing: Boolean;
    function CustomSort(SortProc: TLVCompare; lParam: Longint): Boolean;
    function StringWidth(S: string): Integer;
    procedure UpdateItems(FirstIndex, LastIndex: Integer);

    property Checkboxes: Boolean read GetCheckBoxes write SetCheckboxes default False;
    property Column[Index: Integer]: TListColumn read GetColumnFromIndex;
    property DropTarget: TListItem read GetDropTarget write SetDropTarget;
    property FullDrag: Boolean read GetFullDrag write SetFullDrag default False;
    property GridLines: Boolean read GetGridLines write SetGridLines default False;
    property HotTrack: Boolean read GetHotTrack write SetHotTrack default False;
    property HotTrackStyles: TListHotTrackStyles read GetHotTrackStyles write SetHotTrackStyles default [];
    property InnerListView: TcxCustomInnerListView read GetInnerListView;
    property ItemFocused: TListItem read GetItemFocused write SetItemFocused;
    property ItemIndex: Integer read GetListViewItemIndex write SetItemIndex
      default -1;
    property SelCount: Integer read GetSelCount;
    property Selected: TListItem read GetSelected write SetSelected;
    property TopItem: TListItem read GetTopItem;
    property ViewOrigin: TPoint read GetViewOrigin;
    property VisibleRowCount: Integer read GetVisibleRowCount;
    property BoundingRect: TRect read GetBoundingRect;
    property WorkAreas: TWorkAreas read GetWorkAreas;
  end;

  { TcxListView }

  TcxListView = class(TcxCustomListView)
  public
    property ListViewCanvas;
  published
    property AllocBy default 0;
    property Anchors;
    property BiDiMode;
    property Checkboxes;
    property ColumnClick default True;
    property Columns;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property HideSelection default True;
    property HotTrack default False;
    property HoverTime default -1;
    property IconOptions;
    property ItemIndex;
    property Items;
    property LargeImages;
    property MultiSelect;
    property OwnerData default False;
    property OwnerDraw default False;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RowSelect default False;
    property ShowColumnHeaders;
    property ShowHint;
    property ShowWorkAreas default False;
    property SmallImages;
    property SortType default stNone;
    property StateImages;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property ViewStyle;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnCancelEdit;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnCompare;
    property OnContextPopup;
    property OnCreateItemClass;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSubItemImage;
    property OnInfoTip;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Variants, Graphics, Types, Math, dxDPIAwareUtils;

//type
//  TcxContainerAccess = class(TcxContainer);

{ TcxIconOptions }

constructor TcxIconOptions.Create(AOwner: TcxListViewContainer);
begin
  inherited Create;
  FListViewContainer := AOwner;
  FInnerIconOptions := FListViewContainer.InnerListView.IconOptions;
end;

procedure TcxIconOptions.Assign(Source: TPersistent);
var
  ASourceOptions: TcxIconOptions;
begin
  if Source is TcxIconOptions then
  begin
    ASourceOptions := TcxIconOptions(Source);
    Arrangement := ASourceOptions.Arrangement;
    AutoArrange := ASourceOptions.AutoArrange;
    WrapText := ASourceOptions.WrapText;
  end
  else
    inherited Assign(Source);
end;

function TcxIconOptions.GetArrangement: TIconArrangement;
begin
  Result := FInnerIconOptions.Arrangement;
end;

function TcxIconOptions.GetAutoArrange: Boolean;
begin
  Result := FInnerIconOptions.AutoArrange;
end;

function TcxIconOptions.GetWrapText: Boolean;
begin
  Result := FInnerIconOptions.WrapText;
end;

procedure TcxIconOptions.SetArrangement(Value: TIconArrangement);
begin
  FInnerIconOptions.Arrangement := Value;
end;

procedure TcxIconOptions.SetAutoArrange(Value: Boolean);
begin
  FInnerIconOptions.AutoArrange := Value;
end;

procedure TcxIconOptions.SetWrapText(Value: Boolean);
begin
  FInnerIconOptions.WrapText := Value;
end;

{ TcxCustomInnerListView }

function TcxCustomInnerListView.CanEdit(Item: TListItem): Boolean;
begin
  if Container <> nil then
  begin
    Result := (not Container.ReadOnly) {and (not OwnerData)}; {<- Prevent bug, when Caption not saved after CreateWnd in "OwnerData" mode}
    if Result then
      Result := inherited CanEdit(Item);
  end
  else
    Result := inherited CanEdit(Item);
end;

procedure TcxCustomInnerListView.DoStartDock(var DragObject: TDragObject);
begin
  _TcxContainerAccess.BeginAutoDrag(Container);
end;

procedure TcxCustomInnerListView.DoCancelEdit;
begin
  if IsEditing and Assigned(Container) and not Container.IsDestroying and
    Assigned(Container.OnCancelEdit) then
    Container.OnCancelEdit(Container);
end;

procedure TcxCustomInnerListView.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if Container.IconOptions.AutoArrange then
    Params.Style := Params.Style or LVS_AUTOARRANGE
  else
    Params.Style := Params.Style and not LVS_AUTOARRANGE;
  if not Container.ShowColumnHeaders then
    Params.Style := Params.Style or LVS_NOCOLUMNHEADER;
end;

procedure TcxCustomInnerListView.CreateWnd;
begin
  inherited CreateWnd;
  Container.SetScrollBarsParameters;
  Container.AdjustInnerControl;
end;

procedure TcxCustomInnerListView.DeleteSelected;
begin
  if Assigned(Container) then
    inherited DeleteSelected;
end;

procedure TcxCustomInnerListView.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  if Dragging then
  begin
    CancelDrag;
    Container.BeginDrag(False);
  end;
end;

procedure TcxCustomInnerListView.WMNotify(var Message: TWMNotify);
begin
  inherited;
  if Message.NMHdr.code = HDN_ITEMCHANGED then
    Container.SetScrollBarsParameters(True);
end;

procedure TcxCustomInnerListView.CMHintShow(var Message: TCMHintShow);
var
  AInfoTip: string;
  AItem: TListItem;
  AItemRect, AItemDisplayRect: TRect;
  AHintInfo: PHintInfo;
begin
  Message.HintInfo.HintControl := Container;
  AItem := GetItemAt(Message.HintInfo.CursorPos.X,
    Message.HintInfo.CursorPos.Y);
  if not Assigned(OnInfoTip) then
    inherited
  else
    if AItem <> nil then
    begin
      AInfoTip := AItem.Caption;
      DoInfoTip(AItem, AInfoTip);

      AItemDisplayRect := AItem.DisplayRect(drBounds);
      AItemRect.TopLeft := ClientToScreen(AItemDisplayRect.TopLeft);
      AItemRect.BottomRight := ClientToScreen(AItemDisplayRect.BottomRight);

      AHintInfo := Message.HintInfo;
      AHintInfo.HintStr := AInfoTip;
      AHintInfo.CursorRect := AItemDisplayRect;
      AHintInfo.HintPos := Point(
        AItemRect.Left + GetSystemMetrics(SM_CXCURSOR),
        AItemRect.Top + GetSystemMetrics(SM_CYCURSOR));
      AHintInfo.HintMaxWidth := ClientWidth;
      Message.Result := 0;
    end;
end;

procedure TcxCustomInnerListView.CNNotify(
  var Message: TWMNotify);
var
  AItem: PLVItem;
  APrevBrushChangeHandler, APrevFontChangeHandler: TNotifyEvent;
begin
  if Message.NMHdr.code = LVN_ENDLABELEDIT then
  begin
    AItem := @PLVDispInfo(Message.NMHdr)^.item;
    if (AItem.iItem <> -1) then
      if (AItem.pszText <> nil) then
      begin
        if CanChange(Items[AItem.iItem], LVIF_TEXT) then
          Edit(AItem^);
      end
      else
        DoCancelEdit;
  end
  else
    if Message.NMHdr.code = NM_CUSTOMDRAW then
    begin
      APrevBrushChangeHandler := Canvas.Brush.OnChange;
      APrevFontChangeHandler := Canvas.Font.OnChange;
      inherited;
      Canvas.Brush.OnChange := APrevBrushChangeHandler;
      Canvas.Font.OnChange := APrevFontChangeHandler;
    end
    else
      inherited;
end;

function TcxCustomInnerListView.GetContainer: TcxCustomListView;
begin
  Result := TcxCustomListView(FContainer);
end;

{ TcxListViewContainer }

constructor TcxListViewContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInnerListView := GetInnerListViewClass.Create(Self);
  FIconOptions := GetIconOptionsClass.Create(Self);
  InitializeInnerListView;
  InnerControl := FInnerListView;
end;

destructor TcxListViewContainer.Destroy;
begin
  FreeAndNil(FIconOptions);
  FreeAndNil(FInnerListView);
  inherited Destroy;
end;

function TcxListViewContainer.AllowTouchScrollUIMode: Boolean;
begin
  Result := False;
end;

procedure TcxListViewContainer.CorrectAlignControlRect(var R: TRect);
begin
  R := cxRectContent(R, GetBorderExtent);
end;

function TcxListViewContainer.GetIconOptionsClass: TcxIconOptionsClass;
begin
  Result := TcxIconOptions;
end;

function TcxListViewContainer.GetInnerListViewClass: TcxInnerListViewClass;
begin
  Result := TcxBaseInnerListView;
end;

function TcxListViewContainer.HandleInnerContolGestures: Boolean;
begin
  Result := True;
end;

procedure TcxListViewContainer.InitializeInnerListView;
begin
  FInnerListView.Parent := Self;
  FInnerListView.FContainer := Self;
end;

function TcxListViewContainer.IsPanArea(const APoint: TPoint): Boolean;

  function PtInHeader: Boolean;
  begin
    Result := WindowFromPoint(ClientToScreen(APoint)) = InnerListView.FHeaderHandle;
  end;

begin
  Result := inherited IsPanArea(APoint) and ((ViewStyle <> vsReport) or not PtInHeader);
end;

procedure TcxListViewContainer.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  if not (FIsCreating or IsLoading) then
  begin
    FInnerListView.LookAndFeelChanged(Sender, AChangedValues);
    FInnerListView.RecreateWnd;
  end;
end;

function TcxListViewContainer.NeedsScrollBars: Boolean;
begin
  Result := True;
end;

procedure TcxListViewContainer.SetReadOnly(Value: Boolean);
begin
  FInnerListView.ReadOnly := Value;
end;

procedure TcxListViewContainer.SetViewStyle(Value: TViewStyle);
begin
  FInnerListView.ViewStyle := Value;
  SetScrollBarsParameters;
end;

procedure TcxListViewContainer.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  inherited Scroll(AScrollBarKind, AScrollCode, AScrollPos);
  if not Enabled then
    Exit;
{$IFDEF USETCXSCROLLBAR}
  if AScrollBarKind = sbHorizontal then
    FInnerListView.HScrollHandler(Self, AScrollCode, AScrollPos)
  else
    FInnerListView.VScrollHandler(Self, AScrollCode, AScrollPos);
  SetScrollBarsParameters;
{$ENDIF}
end;

function TcxListViewContainer.UseSystemRightToLeftLayoutForInnerControl: Boolean;
begin
  Result := True;
end;

function TcxListViewContainer.GetMultiSelect: Boolean;
begin
  Result := InnerListView.MultiSelect;
end;

function TcxListViewContainer.GetReadOnly: Boolean;
begin
  Result := FInnerListView.ReadOnly;
end;

function TcxListViewContainer.GetScrollBarBounds(const AScrollBarRect: TRect): TRect;
begin
  if UseRightToLeftScrollBar and not IsWinVistaOrLater then // #Ch WinXp ListView RTL bug
  begin
    Result := dxMapWindowRect(0, Handle, AScrollBarRect);
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, Bounds);
    Result.Right := AScrollBarRect.Width;
    Result.Bottom := AScrollBarRect.Height;
    if (Result.Left < 0) or (Result.Right > Width) or
      (Result.Top < 0) or (Result.Bottom > Height) then
        Result := cxEmptyRect;
  end
  else
    Result := inherited GetScrollBarBounds(AScrollBarRect);
end;

function TcxListViewContainer.GetShowColumnHeaders: Boolean;
begin
  Result := InnerListView.ShowColumnHeaders;
end;

function TcxListViewContainer.GetViewStyle: TViewStyle;
begin
  Result := FInnerListView.ViewStyle;
end;

procedure TcxListViewContainer.SetIconOptions(Value: TcxIconOptions);
begin
  FIconOptions.Assign(Value);
end;

procedure TcxListViewContainer.SetMultiSelect(Value: Boolean);
begin
  InnerListView.MultiSelect := Value;
end;

procedure TcxListViewContainer.SetShowColumnHeaders(Value: Boolean);
begin
  InnerListView.ShowColumnHeaders := Value;
end;

{ TcxCustomListView }

constructor TcxCustomListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 121;
  Height := 97;
end;

class function TcxCustomListView.GetListViewClass: TcxCustomInnerListViewClass;
begin
  Result := TcxCustomInnerListView;
end;

procedure TcxCustomListView.AddItem(Item: string; AObject: TObject);
begin
  InnerListView.AddItem(Item, AObject);
end;

procedure TcxCustomListView.ClearSelection;
begin
  InnerListView.ClearSelection;
end;

procedure TcxCustomListView.DeleteSelected;
begin
  InnerListView.DeleteSelected;
end;

procedure TcxCustomListView.Clear;
begin
  InnerListView.Clear;
end;

procedure TcxCustomListView.DoExit;
begin
  if IsDestroying then
    Exit;
  try
  except
    SetFocus;
    raise;
  end;
  inherited DoExit;
end;

procedure TcxCustomListView.FontChanged;
begin
  inherited FontChanged;
  SetSize;
end;

function TcxCustomListView.IsReadOnly: Boolean;
begin
  Result := ReadOnly;
end;

procedure TcxCustomListView.Loaded;
begin
  inherited;
  InnerListView.OwnerDraw := FOwnerDraw;
end;

procedure TcxCustomListView.WriteState(Writer: TWriter);
begin
  FInnerListView.HandleNeeded;
  inherited;
end;

procedure TcxCustomListView.SetHideSelection(Value: Boolean);
begin
  InnerListView.HideSelection := Value;
end;

procedure TcxCustomListView.SetOwnerData(Value: Boolean);
begin
  InnerListView.OwnerData := Value;
end;

procedure TcxCustomListView.SetOwnerDraw(Value: Boolean);
begin
  FOwnerDraw := Value;
  InnerListView.OwnerDraw := Value;
end;

procedure TcxCustomListView.SetRowSelect(Value: Boolean);
begin
  InnerListView.RowSelect := Value;
end;

procedure TcxCustomListView.SetColumnClick(Value: Boolean);
begin
  InnerListView.ColumnClick := Value;
end;

function TcxCustomListView.AlphaSort: Boolean;
begin
  Result := InnerListView.AlphaSort;
end;

procedure TcxCustomListView.Arrange(Code: TListArrangement);
begin
  InnerListView.Arrange(Code);
end;

procedure TcxCustomListView.CopySelection(Destination: TCustomListControl);
begin
  InnerListView.CopySelection(Destination);
end;

function TcxCustomListView.FindCaption(StartIndex: Integer; Value: string;
  Partial, Inclusive, Wrap: Boolean): TListItem;
begin
  Result := InnerListView.FindCaption(StartIndex, Value, Partial, Inclusive, Wrap);
end;

function TcxCustomListView.FindData(StartIndex: Integer; Value: Pointer;
  Inclusive, Wrap: Boolean): TListItem;
begin
  Result := InnerListView.FindData(StartIndex, Value, Inclusive, Wrap);
end;

function TcxCustomListView.GetHitTestInfoAt(X, Y: Integer): THitTests;
begin
  Result := InnerListView.GetHitTestInfoAt(X, Y);
end;

function TcxCustomListView.GetItemAt(X, Y: Integer): TListItem;
begin
  Result := InnerListView.GetItemAt(X, Y);
end;

function TcxCustomListView.GetNearestItem(Point: TPoint; Direction: TSearchDirection): TListItem;
begin
  Result := InnerListView.GetNearestItem(Point, Direction);
end;

function TcxCustomListView.GetNextItem(StartItem: TListItem;
  Direction: TSearchDirection; States: TItemStates): TListItem;
begin
  Result := InnerListView.GetNextItem(StartItem, Direction, States);
end;

function TcxCustomListView.GetSearchString: string;
begin
  Result := InnerListView.GetSearchString;
end;

function TcxCustomListView.IsEditing: Boolean;
begin
  Result := InnerListView.IsEditing;
end;

procedure TcxCustomListView.SelectAll;
begin
  InnerListView.SelectAll;
end;

function TcxCustomListView.CustomSort(SortProc: TLVCompare;
  lParam: Longint): Boolean;
begin
  Result := InnerListView.CustomSort(SortProc, lParam);
end;

function TcxCustomListView.StringWidth(S: string): Integer;
begin
  Result := InnerListView.StringWidth(S);
end;

procedure TcxCustomListView.UpdateItems(FirstIndex, LastIndex: Integer);
begin
  InnerListView.UpdateItems(FirstIndex, LastIndex);
end;

procedure TcxCustomListView.CheckInnerListViewAlign;
begin
  if IsTouchScrollUIMode then
    InnerListView.Align := alNone
  else
    InnerListView.Align := alClient;
end;

function TcxCustomListView.GetListItems: TListItems;
begin
  Result := InnerListView.Items;
end;

procedure TcxCustomListView.SetListItems(Value: TListItems);
begin
  InnerListView.Items := Value;
end;

function TcxCustomListView.CanChange(Item: TListItem; Change: Integer): Boolean;
begin
  Result := InnerListView.CanChange(Item, Change);
end;

function TcxCustomListView.CanEdit(Item: TListItem): Boolean;
begin
  Result := InnerListView.CanEdit(Item);
end;

function TcxCustomListView.ColumnsShowing: Boolean;
begin
  Result := InnerListView.ColumnsShowing;
end;

procedure TcxCustomListView.DoScrollUIModeChanged;
begin
  CheckInnerListViewAlign;
end;

function TcxCustomListView.GetCount: Integer;
begin
  Result := InnerListView.GetCount;
end;

function TcxCustomListView.GetInnerListViewClass: TcxInnerListViewClass;
begin
  Result := GetListViewClass;
end;

function TcxCustomListView.GetItemIndex(Value: TListItem): Integer;
begin
  Result := InnerListView.GetItemIndex(Value);
end;

function TcxCustomListView.GetItemIndex: Integer;
begin
  Result := InnerListView.GetItemIndex;
end;

function TcxCustomListView.GetListViewItemIndex: Integer;
begin
  Result := GetItemIndex;
end;

procedure TcxCustomListView.InitializeInnerListView;
begin
  inherited InitializeInnerListView;
  CheckInnerListViewAlign;
  InnerListView.BorderStyle := bsNone;
end;

procedure TcxCustomListView.SetItemIndex(Value: Integer);
begin
  InnerListView.SetItemIndex(Value);
end;

procedure TcxCustomListView.SetReadOnly(Value: Boolean);
begin
  if Value <> ReadOnly then
  begin
    inherited SetReadOnly(Value);
    DataSetChange;
  end;
end;

procedure TcxCustomListView.UpdateColumn(AnIndex: Integer);
begin
  InnerListView.UpdateColumn(AnIndex);
end;

procedure TcxCustomListView.UpdateColumns;
begin
  InnerListView.UpdateColumns;
end;

function TcxCustomListView.GetListColumns: TListColumns;
begin
  Result := InnerListView.Columns;
end;

function TcxCustomListView.GetListViewCanvas: TcxCanvas;
begin
  Result := InnerListView.Canvas;
end;

procedure TcxCustomListView.SetListColumns(Value: TListColumns);
begin
  InnerListView.Columns := Value;
end;

function TcxCustomListView.GetColumnClick: Boolean;
begin
  Result := InnerListView.ColumnClick;
end;

function TcxCustomListView.GetHideSelection: Boolean;
begin
  Result := InnerListView.HideSelection;
end;

function TcxCustomListView.GetAllocBy: Integer;
begin
  Result := InnerListView.AllocBy;
end;

procedure TcxCustomListView.SetAllocBy(Value: Integer);
begin
  InnerListView.AllocBy := Value;
end;

function TcxCustomListView.GetHoverTime: Integer;
begin
  Result := InnerListView.HoverTime;
end;

procedure TcxCustomListView.SetHoverTime(Value: Integer);
begin
  InnerListView.HoverTime := Value;
end;

function TcxCustomListView.GetLargeImages: TCustomImageList;
begin
  Result := InnerListView.LargeImages;
end;

procedure TcxCustomListView.SetLargeImages(Value: TCustomImageList);
begin
  InnerListView.LargeImages := Value;
end;

function TcxCustomListView.GetOwnerData: Boolean;
begin
  Result := InnerListView.OwnerData;
end;

function TcxCustomListView.GetOwnerDraw: Boolean;
begin
  Result := InnerListView.OwnerDraw;
end;

function TcxCustomListView.GetOnAdvancedCustomDraw: TLVAdvancedCustomDrawEvent;
begin
  Result := InnerListView.OnAdvancedCustomDraw;
end;

procedure TcxCustomListView.SetOnAdvancedCustomDraw(Value: TLVAdvancedCustomDrawEvent);
begin
  InnerListView.OnAdvancedCustomDraw := Value;
end;

function TcxCustomListView.GetOnAdvancedCustomDrawItem: TLVAdvancedCustomDrawItemEvent;
begin
  Result := InnerListView.OnAdvancedCustomDrawItem;
end;

procedure TcxCustomListView.SetOnAdvancedCustomDrawItem(Value: TLVAdvancedCustomDrawItemEvent);
begin
  InnerListView.OnAdvancedCustomDrawItem := Value;
end;

function TcxCustomListView.GetOnAdvancedCustomDrawSubItem: TLVAdvancedCustomDrawSubItemEvent;
begin
  Result := InnerListView.OnAdvancedCustomDrawSubItem;
end;

procedure TcxCustomListView.SetOnAdvancedCustomDrawSubItem(Value: TLVAdvancedCustomDrawSubItemEvent);
begin
  InnerListView.OnAdvancedCustomDrawSubItem := Value;
end;

function TcxCustomListView.GetOnChange: TLVChangeEvent;
begin
  Result := InnerListView.OnChange;
end;

procedure TcxCustomListView.SetOnChange(Value: TLVChangeEvent);
begin
  InnerListView.OnChange := Value;
end;

function TcxCustomListView.GetOnChanging: TLVChangingEvent;
begin
  Result := InnerListView.OnChanging;
end;

procedure TcxCustomListView.SetOnChanging(Value: TLVChangingEvent);
begin
  InnerListView.OnChanging := Value;
end;

function TcxCustomListView.GetOnColumnClick: TLVColumnClickEvent;
begin
  Result := InnerListView.OnColumnClick;
end;

procedure TcxCustomListView.SetOnColumnClick(Value: TLVColumnClickEvent);
begin
  InnerListView.OnColumnClick := Value;
end;

function TcxCustomListView.GetOnColumnDragged: TNotifyEvent;
begin
  Result := InnerListView.OnColumnDragged;
end;

procedure TcxCustomListView.SetOnColumnDragged(Value: TNotifyEvent);
begin
  InnerListView.OnColumnDragged := Value;
end;

function TcxCustomListView.GetOnColumnRightClick: TLVColumnRClickEvent;
begin
  Result := InnerListView.OnColumnRightClick;
end;

procedure TcxCustomListView.SetOnColumnRightClick(Value: TLVColumnRClickEvent);
begin
  InnerListView.OnColumnRightClick := Value;
end;

function TcxCustomListView.GetOnCompare: TLVCompareEvent;
begin
  Result := InnerListView.OnCompare;
end;

procedure TcxCustomListView.SetOnCompare(Value: TLVCompareEvent);
begin
  InnerListView.OnCompare := Value;
end;

function TcxCustomListView.GetOnCustomDraw: TLVCustomDrawEvent;
begin
  Result := InnerListView.OnCustomDraw;
end;

procedure TcxCustomListView.SetOnCustomDraw(Value: TLVCustomDrawEvent);
begin
  InnerListView.OnCustomDraw := Value;
end;

function TcxCustomListView.GetOnCustomDrawItem: TLVCustomDrawItemEvent;
begin
  Result := InnerListView.OnCustomDrawItem;
end;

procedure TcxCustomListView.SetOnCustomDrawItem(Value: TLVCustomDrawItemEvent);
begin
  InnerListView.OnCustomDrawItem := Value;
end;

function TcxCustomListView.GetOnCustomDrawSubItem: TLVCustomDrawSubItemEvent;
begin
  Result := InnerListView.OnCustomDrawSubItem;
end;

procedure TcxCustomListView.SetOnCustomDrawSubItem(Value: TLVCustomDrawSubItemEvent);
begin
  InnerListView.OnCustomDrawSubItem := Value;
end;

function TcxCustomListView.GetOnData: TLVOwnerDataEvent;
begin
  Result := InnerListView.OnData;
end;

procedure TcxCustomListView.SetOnData(Value: TLVOwnerDataEvent);
begin
  InnerListView.OnData := Value;
end;

function TcxCustomListView.GetOnDataFind: TLVOwnerDataFindEvent;
begin
  Result := InnerListView.OnDataFind;
end;

procedure TcxCustomListView.SetOnDataFind(Value: TLVOwnerDataFindEvent);
begin
  InnerListView.OnDataFind := Value;
end;

function TcxCustomListView.GetOnDataHint: TLVOwnerDataHintEvent;
begin
  Result := InnerListView.OnDataHint;
end;

procedure TcxCustomListView.SetOnDataHint(Value: TLVOwnerDataHintEvent);
begin
  InnerListView.OnDataHint := Value;
end;

function TcxCustomListView.GetOnDataStateChange: TLVOwnerDataStateChangeEvent;
begin
  Result := InnerListView.OnDataStateChange;
end;

procedure TcxCustomListView.SetOnDataStateChange(Value: TLVOwnerDataStateChangeEvent);
begin
  InnerListView.OnDataStateChange := Value;
end;

function TcxCustomListView.GetOnDeletion: TLVDeletedEvent;
begin
  Result := InnerListView.OnDeletion;
end;

procedure TcxCustomListView.SetOnDeletion(Value: TLVDeletedEvent);
begin
  InnerListView.OnDeletion := Value;
end;

function TcxCustomListView.GetOnDrawItem: TLVDrawItemEvent;
begin
  Result := InnerListView.OnDrawItem;
end;

procedure TcxCustomListView.SetOnDrawItem(Value: TLVDrawItemEvent);
begin
  InnerListView.OnDrawItem := Value;
end;

function TcxCustomListView.GetOnEdited: TLVEditedEvent;
begin
  Result := InnerListView.OnEdited;
end;

procedure TcxCustomListView.SetOnEdited(Value: TLVEditedEvent);
begin
  InnerListView.OnEdited := Value;
end;

function TcxCustomListView.GetOnEditing: TLVEditingEvent;
begin
  Result := InnerListView.OnEditing;
end;

procedure TcxCustomListView.SetOnEditing(Value: TLVEditingEvent);
begin
  InnerListView.OnEditing := Value;
end;

function TcxCustomListView.GetOnInfoTip: TLVInfoTipEvent;
begin
  Result := InnerListView.OnInfoTip;
end;

procedure TcxCustomListView.SetOnInfoTip(Value: TLVInfoTipEvent);
begin
  InnerListView.OnInfoTip := Value;
end;

function TcxCustomListView.GetOnInsert: TLVDeletedEvent;
begin
  Result := InnerListView.OnInsert;
end;

procedure TcxCustomListView.SetOnInsert(Value: TLVDeletedEvent);
begin
  InnerListView.OnInsert := Value;
end;

function TcxCustomListView.GetOnGetImageIndex: TLVNotifyEvent;
begin
  Result := InnerListView.OnGetImageIndex;
end;

procedure TcxCustomListView.SetOnGetImageIndex(Value: TLVNotifyEvent);
begin
  InnerListView.OnGetImageIndex := Value;
end;

function TcxCustomListView.GetOnGetSubItemImage: TLVSubItemImageEvent;
begin
  Result := InnerListView.OnGetSubItemImage;
end;

procedure TcxCustomListView.SetOnGetSubItemImage(Value: TLVSubItemImageEvent);
begin
  InnerListView.OnGetSubItemImage := Value;
end;

function TcxCustomListView.GetOnSelectItem: TLVSelectItemEvent;
begin
  Result := InnerListView.OnSelectItem;
end;

procedure TcxCustomListView.SetOnSelectItem(Value: TLVSelectItemEvent);
begin
  InnerListView.OnSelectItem := Value;
end;

function TcxCustomListView.GetShowWorkAreas: Boolean;
begin
  Result := InnerListView.ShowWorkAreas;
end;

procedure TcxCustomListView.SetShowWorkAreas(Value: Boolean);
begin
  InnerListView.ShowWorkAreas := Value;
end;

function TcxCustomListView.GetSmallImages: TCustomImageList;
begin
  Result := InnerListView.SmallImages;
end;

procedure TcxCustomListView.SetSmallImages(Value: TCustomImageList);
begin
  InnerListView.SmallImages := Value;
end;

function TcxCustomListView.GetSortType: TSortType;
begin
  Result := InnerListView.SortType;
end;

procedure TcxCustomListView.SetSortType(Value: TSortType);
begin
  InnerListView.SortType := Value;
end;

function TcxCustomListView.GetStateImages: TCustomImageList;
begin
  Result := InnerListView.StateImages;
end;

procedure TcxCustomListView.SetStateImages(Value: TCustomImageList);
begin
  InnerListView.StateImages := Value;
end;

function TcxCustomListView.GetOnCreateItemClass: TLVCreateItemClassEvent;
begin
  Result := InnerListView.OnCreateItemClass;
end;

procedure TcxCustomListView.SetOnCreateItemClass(Value: TLVCreateItemClassEvent);
begin
  InnerListView.OnCreateItemClass := Value;
end;

function TcxCustomListView.GetCheckBoxes: Boolean;
begin
  Result := InnerListView.Checkboxes;
end;

function TcxCustomListView.GetColumnFromIndex(Index: Integer): TListColumn;
begin
  Result := InnerListView.Column[Index];
end;

function TcxCustomListView.GetDropTarget: TListItem;
begin
  Result := InnerListView.DropTarget;
end;

function TcxCustomListView.GetFullDrag: Boolean;
begin
  Result := InnerListView.FullDrag;
end;

function TcxCustomListView.GetGridLines: Boolean;
begin
  Result := InnerListView.GridLines;
end;

function TcxCustomListView.GetHotTrack: Boolean;
begin
  Result := InnerListView.HotTrack;
end;

function TcxCustomListView.GetHotTrackStyles: TListHotTrackStyles;
begin
  Result := InnerListView.HotTrackStyles;
end;

function TcxCustomListView.GetInnerListView: TcxCustomInnerListView;
begin
  Result := TcxCustomInnerListView(FInnerListView);
end;

function TcxCustomListView.GetItemFocused: TListItem;
begin
  Result := InnerListView.ItemFocused;
end;

function TcxCustomListView.GetRowSelect: Boolean;
begin
  Result := InnerListView.RowSelect;
end;

function TcxCustomListView.GetSelCount: Integer;
begin
  Result := InnerListView.SelCount;
end;

function TcxCustomListView.GetSelected: TListItem;
begin
  Result := InnerListView.Selected;
end;

function TcxCustomListView.GetTopItem: TListItem;
begin
  Result := InnerListView.TopItem;
end;

function TcxCustomListView.GetViewOrigin: TPoint;
begin
  Result := InnerListView.ViewOrigin;
end;

function TcxCustomListView.GetVisibleRowCount: Integer;
begin
  Result := InnerListView.VisibleRowCount;
end;

function TcxCustomListView.GetBoundingRect: TRect;
begin
  Result := InnerListView.BoundingRect;
end;

function TcxCustomListView.GetWorkAreas: TWorkAreas;
begin
  Result := InnerListView.WorkAreas;
end;

procedure TcxCustomListView.SetCheckboxes(Value: Boolean);
begin
  InnerListView.Checkboxes := value;
end;

procedure TcxCustomListView.SetDropTarget(Value: TListItem);
begin
  InnerListView.DropTarget := Value;
end;

procedure TcxCustomListView.SetFullDrag(Value: Boolean);
begin
  InnerListView.FullDrag := Value;
end;

procedure TcxCustomListView.SetGridLines(Value: Boolean);
begin
  InnerListView.GridLines := Value;
end;

procedure TcxCustomListView.SetHotTrack(Value: Boolean);
begin
  InnerListView.HotTrack := Value;
end;

procedure TcxCustomListView.SetHotTrackStyles(Value: TListHotTrackStyles);
begin
  InnerListView.HotTrackStyles := Value;
end;

procedure TcxCustomListView.SetItemFocused(Value: TListItem);
begin
  InnerListView.ItemFocused := Value;
end;

procedure TcxCustomListView.SetSelected(Value: TListItem);
begin
  InnerListView.Selected := Value;
end;

{ TcxBaseInnerListView }

constructor TcxBaseInnerListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TcxCanvas.Create(inherited Canvas);
  BorderStyle := bsNone;
  ControlStyle := ControlStyle + [csDoubleClicks{$IFDEF DELPHI16}, csOverrideStylePaint{$ENDIF}];
  ParentFont := True;
  FPressedHeaderItemIndex := -1;
  FScrollUIActivityHelper := TdxTouchScrollUIActivityHelper.Create;
end;

destructor TcxBaseInnerListView.Destroy;
begin
  FreeAndNil(FScrollUIActivityHelper);
  cxWindowProcController.Remove(FWindowProcObject);
  FHeaderHandle := 0;
  FreeAndNil(FCanvas);
  cxClearObjectLinks(Self);
  inherited Destroy;
end;

function TcxBaseInnerListView.CanFocus: Boolean;
begin
  Result := FContainer.CanFocus;
end;

procedure TcxBaseInnerListView.DefaultHandler(var Message);
begin
  if (FContainer = nil) or
    not FContainer.InnerControlDefaultHandler(TMessage(Message)) then
    inherited DefaultHandler(Message);
end;

procedure TcxBaseInnerListView.DragDrop(Source: TObject; X, Y: Integer);
begin
  if FContainer <> nil then
    FContainer.DragDrop(Source, Left + X, Top + Y);
end;

procedure TcxBaseInnerListView.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if FIsStored and not HandleAllocated then
    HandleNeeded;
  inherited;
end;

procedure TcxBaseInnerListView.CreateWnd;
begin
  FIsStored := False;
  inherited;
end;

procedure TcxBaseInnerListView.DestroyWnd;
begin
  inherited;
  FIsStored := csRecreating in ControlState;
end;

{$IFNDEF DELPHI10SEATTLE}
procedure TcxBaseInnerListView.ChangeScale(M, D: Integer);
var
  AColumn: TListColumn;
  I: Integer;
begin
  if not (sfWidth in ScalingFlags) then
    for I := 0 to Columns.Count - 1 do
    begin
      AColumn := Columns[I];
      if (AColumn.Width <> LVSCW_AUTOSIZE) and (AColumn.Width <> LVSCW_AUTOSIZE_USEHEADER) then
        AColumn.Width := MulDiv(AColumn.Width, M, D);
    end;

  inherited;
end;
{$ENDIF}

procedure TcxBaseInnerListView.Click;
begin
  inherited Click;
  if FContainer <> nil then
    FContainer.Click;
end;

procedure TcxBaseInnerListView.DblClick;
begin
  inherited DblClick;
  if FContainer <> nil then
    FContainer.DblClick;
end;

procedure TcxBaseInnerListView.DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FContainerMouseUpNeeded := True;
end;

function TcxBaseInnerListView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := (FContainer <> nil) and FContainer.DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
    inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TcxBaseInnerListView.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if FContainer <> nil then
    FContainer.DragOver(Source, Left + X, Top + Y, State, Accept);
end;

procedure TcxBaseInnerListView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FContainer <> nil then
    FContainer.KeyDown(Key, Shift);
  if Key <> 0 then
    inherited KeyDown(Key, Shift);
end;

procedure TcxBaseInnerListView.KeyPress(var Key: Char);
begin
  if Key = Char(VK_TAB) then
    Key := #0;
  if FContainer <> nil then
    FContainer.KeyPress(Key);
  if Word(Key) = VK_RETURN then
    Key := #0;
  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TcxBaseInnerListView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_TAB then
    Key := 0;
  if FContainer <> nil then
    FContainer.KeyUp(Key, Shift);
  if Key <> 0 then
    inherited KeyUp(Key, Shift);
end;

procedure TcxBaseInnerListView.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if FContainer <> nil then
  begin
    FContainer.InnerControlMouseDown := True;
    try
      FContainer.MouseDown(Button, Shift, X + Self.Left, Y + Self.Top);
    finally
      FContainer.InnerControlMouseDown := False;
    end;
  end;
end;

procedure TcxBaseInnerListView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FContainer <> nil then
    FContainer.MouseMove(Shift, X + Left, Y + Top);
end;

procedure TcxBaseInnerListView.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FContainerMouseUpNeeded := False;
  OnMouseUp := DoMouseUp;
  inherited MouseUp(Button, Shift, X, Y);
  if (FContainer <> nil) and FContainerMouseUpNeeded then
    FContainer.MouseUp(Button, Shift, X + Left, Y + Top);
end;

procedure TcxBaseInnerListView.WndProc(var Message: TMessage);
var
  ALink: TcxObjectLink;
begin
  if (FContainer <> nil) and FContainer.IsTouchScrollUIMode and
    FScrollUIActivityHelper.CheckScrollActivity(Self, Message) then
    FContainer.ShowTouchScrollUI(FContainer, True);
  if (FContainer <> nil) and FContainer.InnerControlMenuHandler(Message) then
    Exit;
  ALink := cxAddObjectLink(Self);
  try
    inherited WndProc(Message);
    if ALink.Ref <> nil then
      case Message.Msg of
        WM_PAINT:
          FContainer.UpdateScrollBarsParameters;
        WM_PARENTNOTIFY:
          if (Message.WParamLo = WM_CREATE) and (cxGetClassName(Message.LParam) = 'SysHeader32') then
          begin
            FHeaderHandle := Message.LParam;
            FWindowProcObject := cxWindowProcController.Add(FHeaderHandle, HeaderWndProc);
            dxSetWindowStyle(FHeaderHandle, HDS_HOTTRACK, soAdd);
          end;
      else
        if NeedCheckScrollBars(Message) then
          FContainer.SetScrollBarsParameters;
      end;
  finally
    cxRemoveObjectLink(ALink);
  end;
end;

procedure TcxBaseInnerListView.DrawHeader;
var
  AScaleFactor: TdxScaleFactor;
  I: Integer;
begin
  AScaleFactor := dxGetScaleFactor(Parent);
  Canvas.Brush.Color := GetLookAndFeelPainter.DefaultHeaderColor;
  Canvas.Font := Font;
  Canvas.Font.Color := GetLookAndFeelPainter.DefaultHeaderTextColor;
  for I := 0 to Columns.Count do
    DrawScaledHeaderSection(FHeaderHandle, I, Canvas, GetLookAndFeel, AScaleFactor, SmallImages);
end;

function TcxBaseInnerListView.GetSortOrder(AColumnIndex: Integer): TdxSortOrder;
begin
  Result := soNone;
end;

procedure TcxBaseInnerListView.HeaderInvalidate;
begin
  if FHeaderHandle <> 0 then
    InvalidateRect(FHeaderHandle, nil, False);
end;

procedure TcxBaseInnerListView.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  HeaderInvalidate;
end;

procedure TcxBaseInnerListView.MouseEnter(AControl: TControl);
begin
end;

procedure TcxBaseInnerListView.MouseLeave(AControl: TControl);
begin
  if FContainer <> nil then
    FContainer.ShortRefreshContainer(True);
end;

function TcxBaseInnerListView.NeedCheckScrollBars(var Message: TMessage): Boolean;
begin
  case Message.Msg of
    WM_HSCROLL,
    WM_MOUSEWHEEL,
    WM_VSCROLL,
    WM_WINDOWPOSCHANGED,
    CM_WININICHANGE,
    LVM_SETITEMCOUNT:
      Result := True
  else
    Result := False;
  end;
end;

function TcxBaseInnerListView.GetHeaderHotItemIndex: Integer;
var
  AHitTestInfo: THDHitTestInfo;
begin
  if WindowFromPoint(GetMouseCursorPos) <> FHeaderHandle then
  begin
    Result := -1;
    Exit;
  end;

  AHitTestInfo.Point := GetMouseCursorPos;
  Windows.ScreenToClient(FHeaderHandle, AHitTestInfo.Point);
  cxSendStructMessage(FHeaderHandle, HDM_HITTEST, 0, AHitTestInfo);
  Result := AHitTestInfo.Item;
end;

function TcxBaseInnerListView.GetHeaderItemRect(AItemIndex: Integer): TRect;
var
  AHeaderItem: THDItem;
  I: Integer;
  R: TRect;
begin
  if GetComCtlVersion >= ComCtlVersionIE3 then
    cxSendStructMessage(FHeaderHandle, HDM_GETITEMRECT, AItemIndex, Result)
  else
  begin
    Result.Top := 0;
    Result.Left := 0;
    AHeaderItem.Mask := HDI_WIDTH;
    for I := 0 to AItemIndex - 1 do
    begin
      cxSendStructMessage(FHeaderHandle, HDM_GETITEM, I, AHeaderItem);
      Inc(Result.Left, AHeaderItem.cxy);
    end;
    R := cxGetWindowRect(FHeaderHandle);
    Result.Bottom := cxRectHeight(R);
    cxSendStructMessage(FHeaderHandle, HDM_GETITEM, AItemIndex, AHeaderItem);
    Result.Right := Result.Left + AHeaderItem.cxy;
  end;
end;

function TcxBaseInnerListView.GetHeaderPressedItemIndex: Integer;
var
  AHitTestInfo: THDHitTestInfo;
begin
  AHitTestInfo.Point := GetMouseCursorPos;
  Windows.ScreenToClient(FHeaderHandle, AHitTestInfo.Point);
  cxSendStructMessage(FHeaderHandle, HDM_HITTEST, 0, AHitTestInfo);
  if AHitTestInfo.Flags and (HHT_ONDIVIDER or HHT_ONDIVOPEN) <> 0 then
    Result := -1
  else
    Result := AHitTestInfo.Item;
end;

function TcxBaseInnerListView.HeaderItemIndex(AHeaderItem: Integer): Integer;
begin
  Result := AHeaderItem;
  if GetComCtlVersion >= ComCtlVersionIE3 then
    Result := SendMessage(FHeaderHandle, HDM_ORDERTOINDEX, AHeaderItem, 0);
end;

procedure TcxBaseInnerListView.HeaderWndProc(var Message: TMessage);

  procedure CallDefHeaderProc;
  begin
    FWindowProcObject.DefaultProc(Message);
  end;

var
  ADC: HDC;
  APaintStruct: TPaintStruct;
  R: TRect;
begin
  case Message.Msg of
    WM_ERASEBKGND:
      Message.Result := 1;
    WM_PAINT, WM_PRINTCLIENT:
      begin
        ADC := Message.WParam;
        if ADC = 0 then
          ADC := BeginPaint(FHeaderHandle, APaintStruct);
        try
          Canvas.Canvas.Handle := ADC;
          Canvas.Canvas.Refresh;
          DrawHeader;
        finally
          if Message.WParam = 0 then
            EndPaint(FHeaderHandle, APaintStruct);
        end;
      end;
    WM_LBUTTONDOWN:
    begin
      CallDefHeaderProc;
      if ColumnClick and (GetCapture = FHeaderHandle) then
        FPressedHeaderItemIndex := GetHeaderPressedItemIndex;
    end;
    WM_CAPTURECHANGED:
    begin
      if FPressedHeaderItemIndex <> -1 then
      begin
        R := GetHeaderItemRect(FPressedHeaderItemIndex);
        InvalidateRect(FHeaderHandle, @R, False);
      end;
      FPressedHeaderItemIndex := -1;
      CallDefHeaderProc;
    end;
    DXM_GETHEADERITEMINFO:
      Perform(DXM_GETHEADERITEMINFO, Message.WParam, Message.LParam);
  else
    CallDefHeaderProc;
  end;
end;

function TcxBaseInnerListView.GetControl: TWinControl;
begin
  Result := Self;
end;

function TcxBaseInnerListView.GetControlContainer: TcxContainer;
begin
  Result := FContainer;
end;

function TcxBaseInnerListView.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := FContainer.LookAndFeel;
end;

function TcxBaseInnerListView.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := FContainer.LookAndFeelPainter;
end;

procedure TcxBaseInnerListView.HScrollHandler(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
var
  AColumnWidth, ANewPosition: Integer;
begin
  if ScrollCode = scTrack then
  begin
    ANewPosition := ScrollPos - GetScrollPos(Handle, SB_HORZ);
    if ViewStyle = vsList then
    begin
      AColumnWidth := SendMessage(Handle, LVM_GETCOLUMNWIDTH, 0, 0);
      if AColumnWidth <> 0 then
        ANewPosition := ANewPosition * AColumnWidth;
    end;
    SendMessage(Handle, LVM_SCROLL, ANewPosition, 0)
  end
  else
  begin
    CallWindowProc(DefWndProc, Handle, WM_HSCROLL,
      MakeWParam(Word(ScrollCode), Word(ScrollPos)), FContainer.GetScrollBarHandle(sbHorizontal));
    ScrollPos := GetScrollPos(Handle, SB_HORZ);
  end;
end;

procedure TcxBaseInnerListView.VScrollHandler(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);

  function GetLineHeight: Integer;
  begin
    Result := cxRectHeight(TopItem.DisplayRect(drBounds));
  end;

var
  P: TPoint;
  AScrollPerPixel: Boolean;
  AScrollParam: LPARAM;
begin
  if ScrollCode = scTrack then
  begin
    AScrollPerPixel := GroupView or (ViewStyle <> vsReport);
    if AScrollPerPixel then
    begin
      cxSendStructMessage(Handle, LVM_GETORIGIN, 0, P);
      AScrollParam := ScrollPos - P.Y;
    end
    else
      AScrollParam := (ScrollPos - ListView_GetTopIndex(Handle)) * GetLineHeight;
    SendMessage(Handle, LVM_SCROLL, 0, AScrollParam);
  end
  else
  begin
    CallWindowProc(DefWndProc, Handle, WM_VSCROLL, Word(ScrollCode) +
      Word(ScrollPos) shl 16, FContainer.GetScrollBarHandle(sbVertical));
    ScrollPos := GetScrollPos(Handle, SB_VERT);
  end;
end;

procedure TcxBaseInnerListView.WMGestureNotify(var Message: TWMGestureNotify);
begin
  Message.Result := DefWindowProc(Handle, Message.Msg, Message.Unused, LPARAM(Message.NotifyStruct));
end;

procedure TcxBaseInnerListView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if FContainer <> nil then
    with Message do
    begin
      Result := Result or DLGC_WANTCHARS;
      if GetKeyState(VK_CONTROL) >= 0 then
        Result := Result or DLGC_WANTTAB;
    end;
end;

procedure TcxBaseInnerListView.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if (FContainer <> nil) and not FContainer.IsDestroying then
    FContainer.FocusChanged;
end;

procedure TcxBaseInnerListView.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  if not FContainer.IsTouchScrollUIMode and UsecxScrollBars and not FContainer.ScrollBarsCalculating then
    FContainer.SetScrollBarsParameters;
end;

procedure TcxBaseInnerListView.WMNCPaint(var Message: TWMNCPaint);
begin
  if not UsecxScrollBars or FContainer.IsTouchScrollUIMode then
    inherited
  else
  begin
    Message.Result := 1;
    if FContainer.HScrollBarVisible and
      FContainer.VScrollBarVisible then
      cxFillSizeGrip(FContainer);
  end;
end;

procedure TcxBaseInnerListView.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if (FContainer <> nil) and not FContainer.IsDestroying and
    not(csDestroying in ComponentState) and
    (Message.FocusedWnd <> FContainer.Handle) then
    FContainer.FocusChanged;
end;

procedure TcxBaseInnerListView.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  if not FContainer.IsTouchScrollUIMode and not (csDestroying in ComponentState) and
    FContainer.HScrollBarVisible and
    FContainer.VScrollBarVisible then
      cxRedrawNCRect(Handle, GetSizeGripRect(Self));
end;

procedure TcxBaseInnerListView.CMBiDiModeChanged(var Message: TMessage);
var
  APrevWParam: Integer;
begin
  APrevWParam := Message.WParam;
  try
    Message.wParam := 1;
    inherited;
  finally
    Message.wParam := APrevWParam;
  end;
end;

procedure TcxBaseInnerListView.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseEnter(Self)
  else
    MouseEnter(TControl(Message.lParam));
end;

procedure TcxBaseInnerListView.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseLeave(Self)
  else
    MouseLeave(TControl(Message.lParam));
end;

procedure TcxBaseInnerListView.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if UseRightToLeftAlignment and FContainer.UseSystemRightToLeftLayoutForInnerControl then
    Params.ExStyle := (Params.ExStyle or WS_EX_LAYOUTRTL {or WS_EX_NOINHERITLAYOUT}) and
      not (WS_EX_LEFTSCROLLBAR or WS_EX_RIGHT or WS_EX_RTLREADING);
end;

function TcxBaseInnerListView.GetClientOrigin: TPoint;
begin
  Result := inherited;
  if UseRightToLeftAlignment then
    Dec(Result.X, ClientWidth);
end;

procedure TcxBaseInnerListView.LVMGetHeaderItemInfo(var Message: TDXMHeaderItemInfo);

  function GetItemState: TcxButtonState;

    function CanHotTrack: Boolean;
    var
      I: Integer;
    begin
      Result := ColumnClick;
      if Result then
        for I := 0 to Columns.Count - 1 do
          if Columns[I].ImageIndex <> -1 then
          begin
            Result := False;
            Break;
          end;
    end;

  var
    AHeaderItemIndex: Integer;
  begin
    if not Parent.Enabled then
      Result := cxbsDisabled
    else
    begin
      AHeaderItemIndex := HeaderItemIndex(Message.Index);
      if AHeaderItemIndex = FPressedHeaderItemIndex then
        Result := cxbsPressed
      else
        if CanHotTrack and (AHeaderItemIndex = GetHeaderHotItemIndex) then
          Result := cxbsHot
        else
          Result := cxbsNormal;
    end;
  end;

  function GetItemRect: TRect;
  var
    R: TRect;
  begin
    if Message.Index = Columns.Count then
    begin
      Windows.GetClientRect(FHeaderHandle, Result);
      if Columns.Count > 0 then
      begin
        R := GetHeaderItemRect(HeaderItemIndex(Columns.Count - 1));
        Result.Left := R.Right;
      end;
    end
    else
      Result := GetHeaderItemRect(HeaderItemIndex(Message.Index));
  end;

var
  AIndex: Integer;
  AHeaderItemInfo: PHeaderItemInfo;
begin
  AIndex := Message.Index;
  AHeaderItemInfo := Message.HeaderItemInfo;
  ZeroMemory(AHeaderItemInfo, SizeOf(THeaderItemInfo));
  if AIndex < Columns.Count then
  begin
    AHeaderItemInfo.ImageIndex := Columns[AIndex].ImageIndex;
    AHeaderItemInfo.SectionAlignment := Columns[AIndex].Alignment;
    AHeaderItemInfo.SortOrder := GetSortOrder(AIndex);
    AHeaderItemInfo.Text := Columns[AIndex].Caption;
  end
  else
    AHeaderItemInfo.ImageIndex := -1;
  AHeaderItemInfo.Rect := GetItemRect;
  AHeaderItemInfo.State := GetItemState;
  Message.HeaderItemInfo := AHeaderItemInfo;
end;

end.
