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

unit cxListBox;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, dxCore, Math, Types,
  Classes, Controls, Forms, Menus, StdCtrls, SysUtils, Graphics, ImgList, RTLConsts,
  cxClasses, cxControls, cxContainer, cxDataUtils, cxGraphics, cxLookAndFeels, dxTypeHelpers,
  cxScrollBar, cxLookAndFeelPainters, dxCustomHint, cxEdit, cxCustomListBox, dxCoreClasses;

type
  TdxCustomListBox = class;
  TdxCustomListBoxItems = class;
  TcxListBox = class;

  TdxCustomListBoxActionEvent = procedure(Sender: TdxCustomListBox; AItemIndex: Integer) of object;

  { TcxInnerListBox }

  TcxInnerListBox = class(TcxCustomInnerListBox)
  private
    function GetContainer: TcxListBox;
    procedure SetContainer(Value: TcxListBox);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
  protected
    procedure Click; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    property Container: TcxListBox read GetContainer write SetContainer;
  public
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function CanFocus: Boolean; override;
  end;

  TcxInnerListBoxClass = class of TcxInnerListBox;

  { TcxListBox }

  TcxListBoxDrawItemEvent = procedure(AControl: TcxListBox; ACanvas: TcxCanvas;
    AIndex: Integer; ARect: TRect; AState: TOwnerDrawState) of object;
  TcxListBoxMeasureItemEvent = procedure(AControl: TcxListBox; AIndex: Integer;
    var Height: Integer) of object;

  TcxListBox = class(TcxCustomListBox)
  private
    FOnDrawItem: TcxListBoxDrawItemEvent;
    FOnMeasureItem: TcxListBoxMeasureItemEvent;

    procedure DoMeasureItem(Control: TWinControl; Index: Integer;  var Height: Integer);
    function GetColumns: Integer;
    function GetExtendedSelect: Boolean;
    function GetInnerListBox: TcxInnerListBox;
    function GetItemObject: TObject;
    function GetItems: TStrings;
    function GetMultiSelect: Boolean;
    function GetSelCount: Integer;
    function GetSorted: Boolean;
    procedure SetColumns(Value: Integer);
    procedure SetExtendedSelect(Value: Boolean);
    procedure SetItemObject(Value: TObject);
    procedure SetItems(Value: TStrings);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetOnMeasureItem(Value: TcxListBoxMeasureItemEvent);
    procedure SetSorted(Value: Boolean);
    function GetOnData: TLBGetDataEvent;
    function GetOnDataFind: TLBFindDataEvent;
    function GetOnDataObject: TLBGetDataObjectEvent;
    procedure SetOnData(Value: TLBGetDataEvent);
    procedure SetOnDataFind(Value: TLBFindDataEvent);
    procedure SetOnDataObject(Value: TLBGetDataObjectEvent);
  protected
    function AllowTouchScrollUIMode: Boolean; override;
    procedure FontChanged; override;
    function GetItemText(AItemIndex: Integer): string; override;
    procedure InitializeInnerListBox; override;
    function IsInternalControl(AControl: TControl): Boolean; override;
    procedure Loaded; override;
    procedure CorrectAlignControlRect(var R: TRect); override;
    function DoCreateInnerListBox: TcxCustomInnerListBox; override;
    procedure DoScrollUIModeChanged; override;
    procedure DoSetSize; override;
    procedure ScaleFactorChanged; override;
    procedure WndProc(var Message: TMessage); override;
    function DrawItem(ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState): Boolean; virtual;
    function GetInnerListBoxClass: TcxInnerListBoxClass; virtual;
    function IndexOf(const S: string): Integer; override;
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function NeedsScrollBars: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddItem(AItem: string; AObject: TObject);
    procedure Clear;
    procedure ClearSelection;
    procedure DeleteSelected;
    function ItemAtPos(const APos: TPoint; AExisting: Boolean): Integer;
    function ItemRect(Index: Integer): TRect;
    function ItemVisible(Index: Integer): Boolean;
    procedure SelectAll;
    procedure CopySelection(ADestination: TCustomListControl);
    procedure MoveSelection(ADestination: TCustomListControl);

    property Count;
    property InnerListBox: TcxInnerListBox read GetInnerListBox;
    property ItemIndex;
    property ItemObject: TObject read GetItemObject write SetItemObject;
    property SelCount: Integer read GetSelCount;
    property Selected;
    property TopIndex;
  published
    property Anchors;
    property AutoComplete;
    property AutoCompleteDelay;
    property BiDiMode;
    property Columns: Integer read GetColumns write SetColumns default 0;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect: Boolean read GetExtendedSelect
      write SetExtendedSelect default True;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property Items: TStrings read GetItems write SetItems;
    property ListStyle;
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect
      default False;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ScrollWidth;
    property ShowHint;
    property Sorted: Boolean read GetSorted write SetSorted default False;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnData: TLBGetDataEvent read GetOnData write SetOnData;
    property OnDataFind: TLBFindDataEvent read GetOnDataFind write SetOnDataFind;
    property OnDataObject: TLBGetDataObjectEvent read GetOnDataObject
      write SetOnDataObject;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem: TcxListBoxDrawItemEvent read FOnDrawItem
      write FOnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem: TcxListBoxMeasureItemEvent read FOnMeasureItem
      write SetOnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TdxCustomListBoxItem }

  TdxCustomListBoxItem = class(TObject)
  private
    FCaption: string;
    FData: TObject;
    FImageIndex: Integer;
    FHasSeparator: Boolean;
    FOwner: TdxCustomListBoxItems;
    function GetIndex: Integer;
    procedure SetCaption(const ACaption: string);
    procedure SetImageIndex(AValue: Integer);
    procedure SetHasSeparator(const Value: Boolean);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TdxCustomListBoxItems); virtual;
    procedure Assign(Source: TObject); virtual;
    //
    property Caption: string read FCaption write SetCaption;
    property Data: TObject read FData write FData;
    property HasSeparator: Boolean read FHasSeparator write SetHasSeparator;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Index: Integer read GetIndex;
  end;
  TdxCustomListBoxItemClass = class of TdxCustomListBoxItem;

  { TdxCustomCheckListBoxItem }

  TdxCustomCheckListBoxItem = class(TdxCustomListBoxItem)
  private
    FState: TcxCheckBoxState;
    function GetChecked: Boolean;
    procedure SetChecked(AValue: Boolean);
    procedure SetState(AValue: TcxCheckBoxState);
  public
    property Checked: Boolean read GetChecked write SetChecked;
    property State: TcxCheckBoxState read FState write SetState;
  end;

  { TdxCustomListBoxSortOptions }

  TdxCustomListBoxSortOption = (lbsAnsiSort, lbsCaseInsensitive);
  TdxCustomListBoxSortOptions = set of TdxCustomListBoxSortOption;

  { TdxCustomListBoxItems }

  TdxCustomListBoxItems = class(TcxObjectList)
  private
    FLockCount: Integer;
    FOwnerControl: TdxCustomListBox;
    FSorted: Boolean;
    FSortOptions: TdxCustomListBoxSortOptions;
    function FindInSortedList(const ACaption: string): Integer;
    function GetItem(Index: Integer): TdxCustomListBoxItem;
    procedure SetSorted(AValue: Boolean);
    procedure SetSortOptions(AValue: TdxCustomListBoxSortOptions);
  protected
    procedure Changed; virtual;
    procedure DoSort; virtual;
    function GetItemClass: TdxCustomListBoxItemClass; virtual;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;

    property LockCount: Integer read FLockCount;
    property SortOptions: TdxCustomListBoxSortOptions read FSortOptions write SetSortOptions;
  public
    constructor Create(AOwnerControl: TdxCustomListBox); virtual;
    function Add(const ACaption: string; AImageIndex: Integer = -1;
      AData: TObject = nil; AHasSeparator: Boolean = False): TdxCustomListBoxItem;
    function AddObject(const ACaption: string; AData: TObject): TdxCustomListBoxItem;
    function AddSeparator: TdxCustomListBoxItem;
    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;
    function IndexOfCaption(const ACaption: string; ACaseSensitive: Boolean = True): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    function Insert(AIndex: Integer; const ACaption: string;
      AImageIndex: Integer = -1; AData: TObject = nil;
      AHasSeparator: Boolean = False): TdxCustomListBoxItem;
    function IsValidIndex(AIndex: Integer): Boolean;
    procedure LoadFromStrings(AStrings: TStrings);
    //
    property Items[Index: Integer]: TdxCustomListBoxItem read GetItem; default;
    property Sorted: Boolean read FSorted write SetSorted;
  end;
  TdxCustomListBoxItemsClass = class of TdxCustomListBoxItems;

  { TdxCustomCheckListBoxItems }

  TdxCustomCheckListBoxItems = class(TdxCustomListBoxItems)
  private
    function GetItem(Index: Integer): TdxCustomCheckListBoxItem;
  protected
    function GetItemClass: TdxCustomListBoxItemClass; override;
  public
    function AddObject(const ACaption: string; AData: TObject): TdxCustomCheckListBoxItem;

    property Items[Index: Integer]: TdxCustomCheckListBoxItem read GetItem; default;
  end;

  { TdxCustomListBoxHintHelper }

  TdxCustomListBoxHintHelper = class(TcxControlHintHelper)
  private
    FOwnerControl: TdxCustomListBox;
  protected
    function GetOwnerControl: TcxControl; override;
    function PtInCaller(const P: TPoint): Boolean; override;
  public
    constructor Create(AOwnerControl: TdxCustomListBox); virtual;
  end;

  { TdxCustomListBox }

  TdxCustomListBox = class(TcxControl,
    IcxMouseTrackingCaller,
    IcxMouseTrackingCaller2)
  strict private
    FAutoCompleteDelay: Cardinal;
    FChangeLink: TChangeLink;
    FColumns: Integer;
    FColumnWidth: Integer;
    FHintHelper: TdxCustomListBoxHintHelper;
    FHotIndex: Integer;
    FImages: TCustomImageList;
    FIncrementalSearch: Boolean;
    FItemAutoHeight: Boolean;
    FItemHeight: Integer;
    FItemIndex: Integer;
    FItems: TdxCustomListBoxItems;
    FLoopedNavigation: Boolean;
    FSearchText: string;
    FTopIndex: Integer;

    function GetCount: Integer;
    function GetImagesAreaSize: Integer;
    function GetIsLocked: Boolean; inline;
    function GetItemObject: TObject;
    function GetItemsHeight(Index: Integer): Integer;
    function GetMaxTopIndex: Integer;
    function GetPageSize: Integer;
    function GetSorted: Boolean;
    function GetSortOptions: TdxCustomListBoxSortOptions;
    function GetTopIndex: Integer;
    procedure DoImageListChanged(Sender: TObject);
    procedure SetColumns(AValue: Integer);
    procedure SetHotIndex(AValue: Integer);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetItemObject(Value: TObject);
    procedure SetItemHeight(AValue: Integer);
    procedure SetSorted(AValue: Boolean);
    procedure SetSortOptions(AValue: TdxCustomListBoxSortOptions);
    procedure SetTopIndex(AValue: Integer);
    // IcxMouseTrackingCaller
    function PtInCaller(const P: TPoint): Boolean;
    procedure IcxMouseTrackingCaller.MouseLeave = TrackingCallerMouseLeave;
    procedure IcxMouseTrackingCaller2.MouseLeave = TrackingCallerMouseLeave;
  protected
    FStartIncrementalSearch: Cardinal;

    procedure AdjustItemFont(AFont: TFont; AItem: TdxCustomListBoxItem; AState: TcxButtonState); virtual;
    function AllowTouchScrollUIMode: Boolean; override;
    procedure BoundsChanged; override;
    function CanFocusIndex(AItemIndex: Integer): Boolean; virtual;
    function CanProcessIncSearch(Key: Char): Boolean; virtual;
    function CanShowHint(AItemIndex: Integer; out AItemRect, AItemTextRect: TRect): Boolean; virtual;
    function CanStartIncSearch(Key: Char): Boolean;
    function CanUpdateHotState: Boolean; virtual;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure CheckFocusedItemIndex; virtual;
    procedure CheckTopItemIndex; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoLayoutChanged; virtual;
    function FindNextItemIndex(AStartItemIndex: Integer; AGoForward: Boolean = True; AGoOnCycle: Boolean = False): Integer; virtual;
    procedure FocusNextItemIndex(AStartItemIndex: Integer; AGoForward: Boolean = True; AGoOnCycle: Boolean = False); virtual;
    procedure FontChanged; override;
    procedure EraseBackground(DC: HDC); override;
    function GetHasSeparator(AItem: TdxCustomListBoxItem): Boolean; virtual;
    function GetImagesAreaRect: TRect; virtual;
    function GetItemsClass: TdxCustomListBoxItemsClass; virtual;
    function GetItemTextBestWidth: Integer;
    function GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind; override;
    function GetScrollStep: Integer;
    function HasBackground: Boolean; override;
    procedure InitScrollBarsParameters; override;
    procedure InvalidateItem(AIndex: Integer);
    procedure InvalidateItems(const AIndex1, AIndex2: Integer);
    function IsIncSearchChar(AChar: Char): Boolean; virtual;
    function IsStartIncSearchChar(AChar: Char): Boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MeasureItemHeight(AIndex: Integer; var AHeight: Integer); virtual;
    procedure ScaleFactorChanged; override;

    procedure InternalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    function NeedDrawPartVisibleItem: Boolean; virtual;
    function NeedHotTrack: Boolean; virtual;
    function NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean; override;
    function NeedUseHotColor(AItem: TdxCustomListBoxItem; AState: TcxButtonState): Boolean; virtual;
    function NeedUseSelectionColor(AItem: TdxCustomListBoxItem; AState: TcxButtonState): Boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function ProcessKeyPress(var Key: Char): Boolean; virtual;
    function ProcessNavigationKey(var Key: Word; Shift: TShiftState): Boolean; virtual;
    procedure SetItemIndex(AIndex: Integer); virtual;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure TrackingCallerMouseLeave;
    procedure UpdateHintState;
    procedure UpdateHotState;
    procedure UpdateColumnWidth;
    procedure UpdateItemAutoHeight;

    function GetBackgroundColor: TColor;
    function GetImageOffsets: TRect; virtual;
    function GetImageSize: TSize; virtual;
    function GetItemBackgroundColor(AItem: TdxCustomListBoxItem; AState: TcxButtonState): TColor; virtual;
    function GetItemBackgroundRect(const AItemRect: TRect): TRect;
    function GetItemImageRect(const AItemRect: TRect): TRect; virtual;
    procedure GetItemPartsRects(AItem: TdxCustomListBoxItem; const R: TRect; out AItemRect, ASeparatorRect: TRect); virtual;
    function GetItemRect(AItemIndex: Integer; const APrevItemRect: TRect): TRect;
    function GetItemState(AIndex: Integer): TcxButtonState; virtual;
    function GetItemTextRect(const AItemRect: TRect): TRect;
    function GetItemsAreaRect: TRect; virtual;
    function GetTextColor(AItem: TdxCustomListBoxItem; AState: TcxButtonState): TColor; virtual;
    function GetTextFlags: Integer; virtual;
    function GetTextOffsets: TRect; virtual;
    function GetVScrollBarBounds: TRect; override;
    function InternalGetItemHeight: Integer; virtual;

    procedure DrawBackground; virtual;
    procedure DrawItem(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState); virtual;
    procedure DrawItemBackground(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState); virtual;
    procedure DrawItemImage(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState); virtual;
    procedure DrawItemText(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState); virtual;
    procedure DrawItems; virtual;
    procedure DrawItemSeparator(const R: TRect); virtual;
    procedure Paint; override;

    //incremental search
    procedure ClearIncrementalSearch; virtual;
    function DoIncrementalSearch(var Key: Char): Boolean; virtual;
    function FocusItemWithText(const AText: string; AStartIndex, AFinishIndex: Integer): Boolean;
    function FocusNextItemWithText(const AText: string): Boolean;
    function IsItemWithText(AItem: TdxCustomListBoxItem; const AText: string): Boolean; virtual;

    property ColumnWidth: Integer read FColumnWidth;
    property HintHelper: TdxCustomListBoxHintHelper read FHintHelper;
    property HotIndex: Integer read FHotIndex write SetHotIndex;
    property ImageOffsets: TRect read GetImageOffsets;
    property ImagesAreaRect: TRect read GetImagesAreaRect;
    property ImagesAreaSize: Integer read GetImagesAreaSize;
    property ImageSize: TSize read GetImageSize;
    property IsLocked: Boolean read GetIsLocked;
    property ItemAutoHeight: Boolean read FItemAutoHeight write FItemAutoHeight;
    property ItemsAreaRect: TRect read GetItemsAreaRect;
    property ItemsHeight[Index: Integer]: Integer read GetItemsHeight;
    property MaxTopIndex: Integer read GetMaxTopIndex;
    property PageSize: Integer read GetPageSize;
    property SearchText: string read FSearchText write FSearchText;
    property SortOptions: TdxCustomListBoxSortOptions read GetSortOptions write SetSortOptions;
    property TextOffsets: TRect read GetTextOffsets;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(const ACaption: string; AImageIndex: Integer = -1): Integer;
    function AddItem(const ACaption: string; AObject: TObject; AImageIndex: Integer = -1): Integer;
    function CalculateContentSize(AMaxVisibleItemsCount: Integer): TSize; virtual;
    function CalculateItemHeight: Integer; virtual;

    procedure BeginUpdate;
    procedure Clear;
    procedure EndUpdate;
    function ItemAtPos(const APoint: TPoint; AExistOnly: Boolean = False): Integer; virtual;
    function ItemRect(AIndex: Integer): TRect;
    procedure LayoutChanged;
    procedure MakeVisible(AIndex: Integer);
    function Remove(AObject: TObject): Integer;

    property AutoCompleteDelay: Cardinal read FAutoCompleteDelay write FAutoCompleteDelay;
    property Columns: Integer read FColumns write SetColumns;
    property Count: Integer read GetCount;
    property Images: TCustomImageList read FImages write SetImages;
    property IncrementalSearch: Boolean read FIncrementalSearch write FIncrementalSearch;
    property ItemHeight: Integer read FItemHeight write SetItemHeight;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property ItemObject: TObject read GetItemObject write SetItemObject;
    property Items: TdxCustomListBoxItems read FItems write FItems;
    property LoopedNavigation: Boolean read FLoopedNavigation write FLoopedNavigation;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    property Sorted: Boolean read GetSorted write SetSorted;
    property Color;
    property Font;
    property LookAndFeel;
    property OnClick;
    property OnDblClick;
  end;

  { TdxCustomCheckListBox }

  TdxCustomCheckListBoxItemDragOverEvent = procedure(AItem: Pointer; var AAccept: Boolean) of object;

  TdxCustomCheckListBox = class(TdxCustomListBox)
  strict private
    FCachedTextMaxWidth: Integer;
    FDragItemIndex: Integer;
    FIsDragCanceled: Boolean;
    FDragAndDropLeftArrowRect: TRect;
    FDragAndDropRightArrowRect: TRect;
    FItemMoving: Boolean;
    FMultiSelect: Boolean;
    FOnAction: TdxCustomListBoxActionEvent;
    FOnItemDragOver: TdxCustomCheckListBoxItemDragOverEvent;
    FOnSelectionChanged: TNotifyEvent;
    FScrollDirection: TcxDirection;
    FScrollTimer: TcxTimer;
    FSelection: TdxFastList;
    FShowCheckBoxes: Boolean;
    FVisibleItemCount: Integer;
    FVisibleWidth: Integer;

    procedure ClearClickedState;
    function CreateSelectionBlocks: TcxObjectList;
    procedure DoSelect(AIndex: Integer; AValue: Boolean; var AChanged: Boolean);
    function GetAutoSizeItemWidth(const AUseCachedTextMaxWidth: Boolean = False): Integer;
    function GetCheckBoxAreaSize: TSize;
    function GetCheckBoxMargins: TRect;
    function GetChecked(AIndex: Integer): Boolean;
    function GetCheckedIndexes: TdxIntegerIndexes;
    function GetState(AIndex: Integer): TcxCheckBoxState;
    function GetItems: TdxCustomCheckListBoxItems;
    function GetSelected(AIndex: Integer): Boolean;
    procedure ScrollTimerHandler(Sender: TObject);
    procedure SetChecked(AIndex: Integer; AValue: Boolean);
    procedure SetCheckedIndexes(const Value: TdxIntegerIndexes);
    procedure SetState(AIndex: Integer; AValue: TcxCheckBoxState);
    procedure SetMultiSelect(AValue: Boolean);
    procedure SetScrollDirection(AValue: TcxDirection);
    procedure SetSelected(AIndex: Integer; AValue: Boolean);
    procedure SetShowCheckBoxes(AValue: Boolean);
    procedure SetVisibleItemCount(AValue: Integer);
    procedure SetVisibleWidth(AValue: Integer);
  protected
    FAnchorIndex: Integer;
    FInMouseSelectionProcess: Boolean;
    FSelectedViaMouseMovePriorIndex: Integer;

    procedure AdjustSize; override;
    function CalculateAutoHeight: Integer; virtual;
    function CalculateAutoWidth(AUseCachedTextMaxWidth: Boolean = False): Integer; virtual;
    function CanAutoSize(var NewWidth: Integer; var NewHeight: Integer): Boolean; override;
    function CanDrag(X, Y: Integer): Boolean; override;
    function CanProcessIncSearch(Key: Char): Boolean; override;
    function CanUpdateHotState: Boolean; override;
    procedure CheckedChanged(AItemIndex: Integer); virtual;
    procedure ClearSelection; virtual;
    procedure Click; override;
    procedure DblClick; override;
    procedure DoAction;
    procedure DoClick; virtual;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    function DoIncrementalSearch(var Key: Char): Boolean; override;
    procedure DoItemAction(AItemIndex: Integer); virtual;
    procedure DoLayoutChanged; override;
    procedure DoMultiSelectionAction; virtual;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragCanceled; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure DrawDragAndDropArrows;
    procedure DrawItem(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState); override;
    procedure DrawItemCheckBox(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState); virtual;
    procedure DrawItemFocus(const R: TRect); virtual;
    function GetCheckBoxState(AItemIndex: Integer): TcxButtonState; virtual;
    function GetCheckedCount: Integer;
    function GetCheckedItems: TList;
    function GetDragItemInsertionIndex(X, Y: Integer): Integer;
    function GetImagesAreaRect: TRect; override;
    function GetIsCopyDragDrop: Boolean; override;
    function GetItemCheckBoxRect(const AItemRect: TRect): TRect;
    function GetItemFocusRect(const AItemRect: TRect; AItem: TdxCustomListBoxItem): TRect; virtual;
    function GetItemsClass: TdxCustomListBoxItemsClass; override;
    function GetItemMaxWidth(const AUseCachedTextMaxWidth: Boolean = False): Integer;
    function GetSelectedItems(ANeedData: Boolean): TList;
    function GetSelectionCount: Integer;
    function GetVisibleItemCount: Integer; virtual;
    function HasCheckBox(AItemIndex: Integer): Boolean; virtual;
    function HitAtItemCheckBox(AItemIndex: Integer): Boolean;
    function InternalGetItemHeight: Integer; override;
    procedure InternalDragDrop(Source: TObject; X, Y: Integer); virtual;
    procedure ItemAction(AItemIndex: Integer; AValue: TdxDefaultBoolean = bDefault); virtual;
    function NeedBeginDrag(AButton: TMouseButton; AShiftState: TShiftState): Boolean;
    function NeedDrawPartVisibleItem: Boolean; override;
    function NeedHandleClick: Boolean; virtual;
    function NeedUseSelectionColor(AItem: TdxCustomListBoxItem; AState: TcxButtonState): Boolean; override;
    procedure Paint; override;
    procedure ResetCheckBoxStates; virtual;

    procedure InternalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function ProcessNavigationKey(var Key: Word; Shift: TShiftState): Boolean; override;

    procedure SelectAll;
    procedure SelectFocusedItem;
    procedure SelectItems(AStartIndex, AFinishIndex: Integer; AValue: Boolean);
    procedure SelectItemsViaMouseDown(APrevIndex: Integer; AShift: TShiftState);

    property CheckBoxAreaSize: TSize read GetCheckBoxAreaSize;
    property CheckBoxState[AItemIndex: Integer]: TcxButtonState read GetCheckBoxState;
    property CheckBoxMargins: TRect read GetCheckBoxMargins;
    property ScrollDirection: TcxDirection read FScrollDirection write SetScrollDirection;
    property Selection: TdxFastList read FSelection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function StartDrag(DragObject: TDragObject): Boolean; override;

    property AutoSize;
    property BorderStyle;
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property CheckedIndexes: TdxIntegerIndexes read GetCheckedIndexes write SetCheckedIndexes;
    property ItemMoving: Boolean read FItemMoving write FItemMoving default True;
    property Items: TdxCustomCheckListBoxItems read GetItems;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
    property ShowCheckBoxes: Boolean read FShowCheckBoxes write SetShowCheckBoxes;
    property States[Index: Integer]: TcxCheckBoxState read GetState write SetState;
    property VisibleItemCount: Integer read FVisibleItemCount write SetVisibleItemCount;
    property VisibleWidth: Integer read FVisibleWidth write SetVisibleWidth;

    property OnAction: TdxCustomListBoxActionEvent read FOnAction write FOnAction;
    property OnDragDrop;
    property OnItemDragOver: TdxCustomCheckListBoxItemDragOverEvent read FOnItemDragOver write FOnItemDragOver;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  end;

  { TdxCustomDropDownInnerListBox }

  TdxCustomDropDownListBoxCloseUpEvent = procedure (Sender: TObject; AClosedViaKeyboard: Boolean) of object;
  TdxCustomDropDownListBoxSelectedEvent = procedure (Sender: TObject;
    AItem: TdxCustomListBoxItem; ASelectedViaKeyboard: Boolean) of object;

  TdxCustomDropDownInnerListBox = class(TdxCustomListBox)
  private
    FDefaultItemIndex: Integer;
    FPrevMousePosition: TPoint;
    FOnSelectItem: TdxCustomDropDownListBoxSelectedEvent;
    procedure SetDefaultItemIndex(AValue: Integer);
  protected
    procedure AdjustItemFont(AFont: TFont; AItem: TdxCustomListBoxItem; AState: TcxButtonState); override;
    procedure DoSelectItem(ASelectedViaKeyboard: Boolean); virtual;
    procedure DrawBackground; override;
    procedure DrawItemBackground(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState); override;
    function GetBorderSize: Integer; override;
    function GetImageOffsets: TRect; override;
    function GetImageSize: TSize; override;
    function GetTextColor(AItem: TdxCustomListBoxItem; AState: TcxButtonState): TColor; override;
    function GetTextOffsets: TRect; override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function ProcessNavigationKey(var Key: Word; Shift: TShiftState): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    //
    property DefaultItemIndex: Integer read FDefaultItemIndex write SetDefaultItemIndex default -1;
    property OnSelectItem: TdxCustomDropDownListBoxSelectedEvent read FOnSelectItem write FOnSelectItem;
  end;

  { TdxCustomDropDownListBox }

  TdxCustomDropDownListBox = class(TcxCustomPopupWindow)
  private
    FDisplayRowsCount: Cardinal;
    FInnerListBox: TdxCustomDropDownInnerListBox;
    FMaxWidth: Cardinal;
    FMinWidth: Cardinal;
    FOnCloseUp: TdxCustomDropDownListBoxCloseUpEvent;
    FOnSelect: TdxCustomDropDownListBoxSelectedEvent;
    function GetCount: Integer;
    function GetImages: TCustomImageList;
    function GetItemHeight: Integer;
    function GetItemIndex: Integer;
    function GetItems: TdxCustomListBoxItems;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetItemHeight(const Value: Integer);
    procedure SetItemIndex(const Value: Integer);
  protected
    FClosedViaKeyboard: Boolean;
    procedure AdjustClientRect(var Rect: TRect); override;
    function CalculatePosition(const ASize: TSize): TPoint; override;
    function CalculateSize: TSize; override;
    function CreateInnerListBox: TdxCustomDropDownInnerListBox; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoCloseUp(AClosedViaKeyboard: Boolean); virtual;
    procedure DoHide; override;
    procedure DoSelectItem(AItem: TdxCustomListBoxItem; ASelectedViaKeyboard: Boolean); virtual;
    procedure InitInnerListBox; virtual;
    procedure InitPopup; override;
    procedure ItemSelected(Sender: TObject; AItem: TdxCustomListBoxItem; ASelectedViaKeyboard: Boolean);
  public
    constructor Create(AOwnerControl: TWinControl); override;
    constructor CreateEx(AOwnerControl: TWinControl; AOwnerParent: TcxControl);
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsShortCut(var Message: TWMKey): Boolean; override;
    procedure CloseUp(AClosedViaKeyboard: Boolean); reintroduce; virtual;
    procedure Popup; reintroduce; virtual;
    procedure PopupForBounds(const ABounds: TRect; AActivateKey: Char = #0);
    //
    property Count: Integer read GetCount;
    property DisplayRowsCount: Cardinal read FDisplayRowsCount write FDisplayRowsCount;
    property Images: TCustomImageList read GetImages write SetImages;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Items: TdxCustomListBoxItems read GetItems;
    property InnerListBox: TdxCustomDropDownInnerListBox read FInnerListBox;
    property MaxWidth: Cardinal read FMaxWidth write FMaxWidth;
    property MinWidth: Cardinal read FMinWidth write FMinWidth;
    property OnCloseUp: TdxCustomDropDownListBoxCloseUpEvent read FOnCloseUp write FOnCloseUp;
    property OnSelect: TdxCustomDropDownListBoxSelectedEvent read FOnSelect write FOnSelect;
  end;

  { TdxQuickCustomizationCustomCommand }

  TdxQuickCustomizationCustomCommand = class
  private
    FCaption: string;
    FHasCheckBox: Boolean;
    FChecked: Boolean;
    FOnAction: TNotifyEvent;
    procedure SetChecked(AValue: Boolean);
  public
    constructor Create(const ACaption: string; AHasCheckBox: Boolean);
    procedure Execute;

    property Caption: string read FCaption;
    property Checked: Boolean read FChecked write SetChecked;
    property HasCheckBox: Boolean read FHasCheckBox;

    property OnAction: TNotifyEvent read FOnAction write FOnAction;
  end;

  { TdxQuickCustomizationControlCustomCheckListBox }

  TdxQuickCustomizationCustomControl = class;

  TdxQuickCustomizationCustomCheckListBox = class(TdxCustomCheckListBox)
  protected
    function DoIncrementalSearch(var Key: Char): Boolean; override;
    function GetQuickCustomizationControl: TdxQuickCustomizationCustomControl;
    function GetTextOffsets: TRect; override;
    function NeedHandleClick: Boolean; override;
    function ProcessNavigationKey(var Key: Word; Shift: TShiftState): Boolean; override;
    procedure SetItemIndex(AIndex: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;

    property QuickCustomizationControl: TdxQuickCustomizationCustomControl read GetQuickCustomizationControl;
    property OnSelectionChanged;
  end;

  TdxQuickCustomizationCustomCheckListBoxClass = class of TdxQuickCustomizationCustomCheckListBox;

  { TdxComboCommandBoxCommandListBox }

  TdxQuickCustomizationCommandListBox = class(TdxQuickCustomizationCustomCheckListBox)
  private
    function GetCommand(AItemIndex: Integer): TdxQuickCustomizationCustomCommand;
  protected
    procedure CheckedChanged(AItemIndex: Integer); override;
    procedure DoItemAction(AItemIndex: Integer); override;
    function HasCheckBox(AItemIndex: Integer): Boolean; override;

    property Command[AItemIndex: Integer]: TdxQuickCustomizationCustomCommand read GetCommand;
  end;

  { TdxQuickCustomizationCheckListBox }

  TdxQuickCustomizationCheckListBox = class(TdxQuickCustomizationCustomCheckListBox)
  private
    FDblClickHandling: Boolean;

    FOnSelectedItemCheckedStateChanged: TNotifyEvent;
    FOnSelectedItemCheckedStateChanging: TNotifyEvent;

    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
  protected
    procedure InternalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoMultiSelectionAction; override;
  public
    constructor Create(AOwner: TComponent); override;

    property OnSelectedItemCheckedStateChanged: TNotifyEvent read FOnSelectedItemCheckedStateChanged write FOnSelectedItemCheckedStateChanged;
    property OnSelectedItemCheckedStateChanging: TNotifyEvent read FOnSelectedItemCheckedStateChanging write FOnSelectedItemCheckedStateChanging;
  end;

  { TdxQuickCustomizationController }

  TdxQuickCustomizationController = class
  private
    FOwner: TdxQuickCustomizationCustomControl;
    function GetCheckListBox: TdxCustomCheckListBox;
    function GetCommandListBox: TdxCustomCheckListBox;
  protected
    function DoIncrementalSearchOnOtherListBox(Sender: TdxCustomCheckListBox; const ASearchText: string): Boolean;
    procedure DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandleCommandListBoxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure HandleItemIndexChanging(Sender: TdxCustomListBox; AItemIndex: Integer);
    function ProcessCheckListBoxNavigationKey(var Key: Word; Shift: TShiftState): Boolean;
    function ProcessCommandListBoxNavigationKey(var Key: Word; Shift: TShiftState): Boolean;
    function ProcessNavigationKey(Sender: TdxCustomListBox; var Key: Word; Shift: TShiftState): Boolean;
  public
    constructor Create(AOwner: TdxQuickCustomizationCustomControl); virtual;

    property CheckListBox: TdxCustomCheckListBox read GetCheckListBox;
    property CommandListBox: TdxCustomCheckListBox read GetCommandListBox;
    property Owner: TdxQuickCustomizationCustomControl read FOwner;
  end;

  { TdxQuickCustomizationCustomControl }

  TdxQuickCustomizationCustomControl = class(TcxContainer)
  strict private
    FCheckListBox: TdxQuickCustomizationCheckListBox;
    FCheckListBoxVisibleRowCount: Integer;
    FCommandListBox: TdxQuickCustomizationCustomCheckListBox;
    FCommandList: TcxObjectList;
    FController: TdxQuickCustomizationController;

    FOnInitialize: TNotifyEvent;
  protected
    function AddCommand(const ACaption: string; AHasCheckBox: Boolean; AOnAction: TNotifyEvent): TdxQuickCustomizationCustomCommand;
    procedure AdjustBounds(const AUseCachedTextMaxWidth: Boolean = False); virtual;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure DrawSeparator; virtual;
    function GetCommandListBoxClass: TdxQuickCustomizationCustomCheckListBoxClass; virtual;
    function GetSeparatorHeight: Integer; virtual;
    procedure Paint; override;
    procedure PopulateCheckListBox; virtual; abstract;
    procedure PopulateCommandListBox; virtual; abstract;

    property Controller: TdxQuickCustomizationController read FController;
    property CheckListBoxVisibleRowCount: Integer read FCheckListBoxVisibleRowCount write FCheckListBoxVisibleRowCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; virtual;
    function HasCommands: Boolean;
    procedure Initialize(AParent: TWinControl); virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    property CheckListBox: TdxQuickCustomizationCheckListBox read FCheckListBox;
    property CommandListBox: TdxQuickCustomizationCustomCheckListBox read FCommandListBox;
    property OnInitialize: TNotifyEvent read FOnInitialize write FOnInitialize;
  end;

implementation

uses
  Variants, cxGeometry, StrUtils, dxDPIAwareUtils;

const
  dxCheckBoxOffset = 2;
  dxListBoxScrollZoneWidth = 15;
  dxListBoxScrollTimeInterval = 200;

type
  TWinControlAccess = class(TWinControl);

  TdxCustomListBoxSelectionBlock = class
  public
    Start: Integer;
    Finish: Integer;

    constructor Create(AStart, AFinish: Integer);
  end;

function dxCustomListBoxItemsAnsiCompare(Item1, Item2: TdxCustomListBoxItem): Integer;
begin
  Result := AnsiCompareStr(Item1.Caption, Item2.Caption);
end;

function dxCustomListBoxItemsAnsiInsensitiveCompare(Item1, Item2: TdxCustomListBoxItem): Integer;
begin
  Result := AnsiCompareText(Item1.Caption, Item2.Caption);
end;

function dxCustomListBoxItemsCompare(Item1, Item2: TdxCustomListBoxItem): Integer;
begin
  Result := CompareStr(Item1.Caption, Item2.Caption);
end;

function dxCustomListBoxItemsInsensitiveCompare(Item1, Item2: TdxCustomListBoxItem): Integer;
begin
  Result := CompareStr(UpperCase(Item1.Caption), UpperCase(Item2.Caption));
end;

{ TdxCustomListBoxSelectionBlock }

constructor TdxCustomListBoxSelectionBlock.Create(AStart, AFinish: Integer);
begin
  Start := AStart;
  Finish := AFinish;
end;

{ TcxInnerListBox }

function TcxInnerListBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or
    Container.FDataBinding.ExecuteAction(Action);
end;

function TcxInnerListBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or
    Container.FDataBinding.UpdateAction(Action);
end;

function TcxInnerListBox.CanFocus: Boolean;
begin
  Result := Container.CanFocus;
end;

procedure TcxInnerListBox.Click;
begin
  if Container.DataBinding.SetEditMode then
    inherited Click;
end;

procedure TcxInnerListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  if not Container.DrawItem(Canvas, Index, Rect, State) then
    inherited DrawItem(Index, Rect, State);
end;

function TcxInnerListBox.GetContainer: TcxListBox;
begin
  Result := TcxListBox(Owner);
end;

procedure TcxInnerListBox.SetContainer(Value: TcxListBox);
begin
  FContainer := Value;
end;

procedure TcxInnerListBox.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if Container.DataBinding.SetEditMode then
    inherited
  else
  begin
    SetFocus;
    MouseDown(mbLeft, KeysToShiftState(Message.Keys), Message.XPos, Message.YPos);
  end;
end;

{ TcxListBox }

constructor TcxListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateInnerListBox;
  Width := 121;
  Height := 97;
end;

procedure TcxListBox.AddItem(AItem: string; AObject: TObject);
begin
  InnerListBox.AddItem(AItem, AObject);
end;

function TcxListBox.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

procedure TcxListBox.Clear;
begin
  InnerListBox.Clear;
end;

procedure TcxListBox.ClearSelection;
begin
  InnerListBox.Items.BeginUpdate;
  try
    InnerListBox.ClearSelection;
  finally
    InnerListBox.Items.EndUpdate;
  end;
end;

procedure TcxListBox.DeleteSelected;
begin
  InnerListBox.Items.BeginUpdate;
  try
    InnerListBox.DeleteSelected;
  finally
    InnerListBox.Items.EndUpdate;
  end;
end;

function TcxListBox.ItemAtPos(const APos: TPoint; AExisting: Boolean): Integer;
begin
  Result := InnerListBox.ItemAtPos(Point(APos.X - InnerListBox.Left, APos.Y - InnerListBox.Top), AExisting);
end;

function TcxListBox.ItemRect(Index: Integer): TRect;
begin
  Result := InnerListBox.ItemRect(Index);
  OffsetRect(Result, InnerListBox.Left, InnerListBox.Top);
end;

function TcxListBox.ItemVisible(Index: Integer): Boolean;
begin
  Result := InnerListBox.ItemVisible(Index);
end;

procedure TcxListBox.SelectAll;
begin
  InnerListBox.Items.BeginUpdate;
  try
    InnerListBox.SelectAll;
  finally
    InnerListBox.Items.EndUpdate;
  end;
end;

procedure TcxListBox.CopySelection(ADestination: TCustomListControl);
begin
  InnerListBox.Items.BeginUpdate;
  try
    InnerListBox.CopySelection(ADestination);
  finally
    InnerListBox.Items.EndUpdate;
  end;
end;

procedure TcxListBox.MoveSelection(ADestination: TCustomListControl);
begin
  InnerListBox.Items.BeginUpdate;
  try
    InnerListBox.MoveSelection(ADestination);
  finally
    InnerListBox.Items.EndUpdate;
  end;
end;

procedure TcxListBox.FontChanged;
begin
  inherited FontChanged;
  SetSize;
end;

function TcxListBox.GetItemText(AItemIndex: Integer): string;
begin
  Result := Items[AItemIndex];
end;

procedure TcxListBox.InitializeInnerListBox;
begin
  InnerListBox.BorderStyle := bsNone;
  InnerListBox.Parent := Self;
  InnerListBox.Container := Self;
  InnerControl := InnerListBox;
  InnerListBox.LookAndFeel.MasterLookAndFeel := Style.LookAndFeel;
end;

function TcxListBox.IsInternalControl(AControl: TControl): Boolean;
begin
  if InnerListBox = nil then
    Result := True
  else
    Result := (AControl = InnerListBox.HScrollBar) or (AControl = InnerListBox.VScrollBar);
  Result := Result or inherited IsInternalControl(AControl);
end;

procedure TcxListBox.Loaded;
begin
  inherited Loaded;
  cxRecreateControlWnd(InnerListBox);
end;

procedure TcxListBox.CorrectAlignControlRect(var R: TRect);
begin
  R := cxRectContent(R, GetBorderExtent);
end;

function TcxListBox.DoCreateInnerListBox: TcxCustomInnerListBox;
begin
  Result := GetInnerListBoxClass.Create(Self);
end;

procedure TcxListBox.DoSetSize;
var
  ANewHeight: Integer;
  APrevBoundsRect: TRect;
begin
  if IsLoading then
    Exit;
  APrevBoundsRect := InnerListBox.BoundsRect;
  try
    if not IntegralHeight or (Align in [alLeft, alRight, alClient]) then
    begin
      inherited;
      Exit;
    end;
    ANewHeight := Height;
    GetOptimalHeight(ANewHeight);
    Height := ANewHeight;
    inherited;
  finally
    if not EqualRect(APrevBoundsRect, InnerListBox.BoundsRect) and InnerListBox.HandleAllocated then
      dxMessagesController.KillMessages(InnerListBox.Handle, WM_MOUSEMOVE);
  end;
end;

procedure TcxListBox.ScaleFactorChanged;
begin
  inherited ScaleFactorChanged;
  cxRecreateControlWnd(InnerListBox);
end;

procedure TcxListBox.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  if (InnerListBox <> nil) and (Message.Msg = WM_COMMAND) and (Message.WParamHi = LBN_SELCHANGE) then
    InnerListBox.SetExternalScrollBarsParameters;
end;

function TcxListBox.DrawItem(ACanvas: TcxCanvas; AIndex: Integer;
  const ARect: TRect; AState: TOwnerDrawState): Boolean;
begin
  SynchronizeTextFlags;
  Result := Assigned(FOnDrawItem);
  if Result then
    FOnDrawItem(Self, ACanvas, AIndex, ARect, AState);
end;

function TcxListBox.GetInnerListBoxClass: TcxInnerListBoxClass;
begin
  Result := TcxInnerListBox;
end;

procedure TcxListBox.DoMeasureItem(Control: TWinControl; Index: Integer;
  var Height: Integer);
begin
  FOnMeasureItem(Self, Index, Height);
end;

procedure TcxListBox.DoScrollUIModeChanged;
begin
  if InnerListBox <> nil then
    InnerListBox.RecreateWnd;
end;

function TcxListBox.GetColumns: Integer;
begin
  Result := InnerListBox.Columns;
end;

function TcxListBox.GetExtendedSelect: Boolean;
begin
  Result := InnerListBox.ExtendedSelect;
end;

function TcxListBox.GetInnerListBox: TcxInnerListBox;
begin
  Result := inherited InnerListBox as TcxInnerListBox;
end;

function TcxListBox.GetItemObject: TObject;
begin
  if ItemIndex <> -1 then
    Result := Items.Objects[ItemIndex]
  else
    Result := nil;
end;

function TcxListBox.GetItems: TStrings;
begin
  Result := InnerListBox.Items;
end;

function TcxListBox.GetMultiSelect: Boolean;
begin
  Result := InnerListBox.MultiSelect;
end;

function TcxListBox.GetSelCount: Integer;
begin
  Result := InnerListBox.SelCount;
end;

function TcxListBox.GetSorted: Boolean;
begin
  Result := InnerListBox.Sorted;
end;

procedure TcxListBox.SetColumns(Value: Integer);
begin
  InnerListBox.Columns := Value;
  InnerListBox.SetExternalScrollBarsParameters;
end;

procedure TcxListBox.SetExtendedSelect(Value: Boolean);
begin
  InnerListBox.ExtendedSelect := Value;
end;

procedure TcxListBox.SetItemObject(Value: TObject);
begin
  ItemIndex := Items.IndexOfObject(Value);
end;

procedure TcxListBox.SetItems(Value: TStrings);
begin
  InnerListBox.Items := Value;
  DataChange;
end;

procedure TcxListBox.SetMultiSelect(Value: Boolean);
begin
  InnerListBox.MultiSelect := Value;
end;

procedure TcxListBox.SetOnMeasureItem(Value: TcxListBoxMeasureItemEvent);
begin
  FOnMeasureItem := Value;
  if Assigned(FOnMeasureItem) then
    InnerListBox.OnMeasureItem := DoMeasureItem
  else
    InnerListBox.OnMeasureItem := nil;
end;

procedure TcxListBox.SetSorted(Value: Boolean);
begin
  InnerListBox.Sorted := Value;
end;

function TcxListBox.GetOnData: TLBGetDataEvent;
begin
  Result := InnerListBox.OnData;
end;

function TcxListBox.GetOnDataFind: TLBFindDataEvent;
begin
  Result := InnerListBox.OnDataFind;
end;

function TcxListBox.GetOnDataObject: TLBGetDataObjectEvent;
begin
  Result := InnerListBox.OnDataObject;
end;

procedure TcxListBox.SetOnData(Value: TLBGetDataEvent);
begin
  InnerListBox.OnData := Value;
end;

procedure TcxListBox.SetOnDataFind(Value: TLBFindDataEvent);
begin
  InnerListBox.OnDataFind := Value;
end;

procedure TcxListBox.SetOnDataObject(Value: TLBGetDataObjectEvent);
begin
  InnerListBox.OnDataObject := Value;
end;

function TcxListBox.IndexOf(const S: string): Integer;
begin
  Result := Items.IndexOf(S);
end;

function TcxListBox.IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := False;
end;

function TcxListBox.NeedsScrollBars: Boolean;
begin
  Result := True;
end;

{ TdxCustomListBoxItem }

constructor TdxCustomListBoxItem.Create(AOwner: TdxCustomListBoxItems);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxCustomListBoxItem.Assign(Source: TObject);
begin
  if Source is TdxCustomListBoxItem then
  begin
    Caption := TdxCustomListBoxItem(Source).Caption;
    Data := TdxCustomListBoxItem(Source).Data;
    ImageIndex := TdxCustomListBoxItem(Source).ImageIndex;
    HasSeparator := TdxCustomListBoxItem(Source).HasSeparator;
  end
end;

procedure TdxCustomListBoxItem.Changed;
begin
  if FOwner <> nil then
    FOwner.Changed;
end;

function TdxCustomListBoxItem.GetIndex: Integer;
begin
  if FOwner <> nil then
    Result := FOwner.IndexOf(Self)
  else
    Result := -1;
end;

procedure TdxCustomListBoxItem.SetCaption(const ACaption: string);
begin
  if ACaption <> FCaption then
  begin
    FCaption := ACaption;
    Changed;
  end;
end;

procedure TdxCustomListBoxItem.SetHasSeparator(const Value: Boolean);
begin
  if FHasSeparator <> Value then
  begin
    FHasSeparator := Value;
    Changed;
  end;
end;

procedure TdxCustomListBoxItem.SetImageIndex(AValue: Integer);
begin
  if AValue <> FImageIndex then
  begin
    FImageIndex := AValue;
    Changed;
  end;
end;

{ TdxCustomCheckListBoxItem }

function TdxCustomCheckListBoxItem.GetChecked: Boolean;
begin
  Result := State = cbsChecked;
end;

procedure TdxCustomCheckListBoxItem.SetChecked(AValue: Boolean);
const
  AState: array[Boolean] of TcxCheckBoxState = (cbsUnchecked, cbsChecked);
begin
  if AValue <> Checked then
    State := AState[AValue];
end;

procedure TdxCustomCheckListBoxItem.SetState(AValue: TcxCheckBoxState);
begin
  if FState <> AValue then
  begin
    FState := AValue;
    Changed;
  end;
end;

{ TdxCustomListBoxItems }

constructor TdxCustomListBoxItems.Create(AOwnerControl: TdxCustomListBox);
begin
  inherited Create;
  FOwnerControl := AOwnerControl;
end;

function TdxCustomListBoxItems.Add(const ACaption: string;
  AImageIndex: Integer = -1; AData: TObject = nil; AHasSeparator: Boolean = False): TdxCustomListBoxItem;
var
  AIndex: Integer;
begin
  if Sorted then
    AIndex := FindInSortedList(ACaption)
  else
    AIndex := Count;
  Result := Insert(AIndex, ACaption, AImageIndex, AData, AHasSeparator);
end;

function TdxCustomListBoxItems.AddObject(const ACaption: string; AData: TObject): TdxCustomListBoxItem;
begin
  Result := Add(ACaption, -1, AData);
end;

function TdxCustomListBoxItems.AddSeparator: TdxCustomListBoxItem;
begin
  if Count > 0 then
  begin
    Result := Items[Count - 1];
    Result.HasSeparator := True;
  end
  else
    Result := nil;
end;

procedure TdxCustomListBoxItems.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxCustomListBoxItems.CancelUpdate;
begin
  Dec(FLockCount);
end;

procedure TdxCustomListBoxItems.EndUpdate;
begin
  Dec(FLockCount);
  Changed;
end;

procedure TdxCustomListBoxItems.Changed;
begin
  FOwnerControl.LayoutChanged;
end;

procedure TdxCustomListBoxItems.DoSort;
begin
  if lbsAnsiSort in SortOptions then
  begin
    if lbsCaseInsensitive in SortOptions then
      Sort(@dxCustomListBoxItemsAnsiInsensitiveCompare)
    else
      Sort(@dxCustomListBoxItemsAnsiCompare)
  end
  else
  begin
    if lbsCaseInsensitive in SortOptions then
      Sort(@dxCustomListBoxItemsInsensitiveCompare)
    else
      Sort(@dxCustomListBoxItemsCompare)
  end;
end;

function TdxCustomListBoxItems.FindInSortedList(const ACaption: string): Integer;
var
  ACompareResult: Integer;
  ALeft, ARight, I: Integer;
begin
  ALeft := 0;
  ARight := Count - 1;
  while ALeft <= ARight do
  begin
    I := (ALeft + ARight) div 2;
    ACompareResult := CompareStr(Items[I].Caption, ACaption);
    if ACompareResult < 0 then
      ALeft := I + 1
    else
      ARight := I - 1;
  end;
  Result := ALeft;
end;

function TdxCustomListBoxItems.IndexOfCaption(
  const ACaption: string; ACaseSensitive: Boolean = True): Integer;

  function Compare(const S1, S2: string): Boolean;
  begin
    if ACaseSensitive then
      Result := S1 = S2
    else
      Result := SameText(S1, S2)
  end;

var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Compare(Items[I].Caption, ACaption) then
    begin
      Result := I;
      Break;
    end;
end;

function TdxCustomListBoxItems.IndexOfObject(AObject: TObject): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I].Data = AObject then
    begin
      Result := I;
      Break;
    end;
end;

function TdxCustomListBoxItems.Insert(AIndex: Integer; const ACaption: string;
  AImageIndex: Integer = -1; AData: TObject = nil; AHasSeparator: Boolean = False): TdxCustomListBoxItem;
begin
  Result := GetItemClass.Create(Self);
  Result.FCaption := ACaption;
  Result.FImageIndex := AImageIndex;
  Result.FData := AData;
  Result.FHasSeparator := AHasSeparator;
  inherited Insert(AIndex, Result);
end;

function TdxCustomListBoxItems.IsValidIndex(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < Count);
end;

procedure TdxCustomListBoxItems.LoadFromStrings(AStrings: TStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    Clear;
    for I := 0 to AStrings.Count - 1 do
      AddObject(AStrings[I], AStrings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomListBoxItems.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  Changed;
end;

function TdxCustomListBoxItems.GetItem(Index: Integer): TdxCustomListBoxItem;
begin
  Result := TdxCustomListBoxItem(inherited Items[Index]);
end;

function TdxCustomListBoxItems.GetItemClass: TdxCustomListBoxItemClass;
begin
  Result := TdxCustomListBoxItem;
end;

procedure TdxCustomListBoxItems.SetSorted(AValue: Boolean);
begin
  if AValue <> FSorted then
  begin
    FSorted := AValue;
    if Sorted then
      DoSort;
  end;
end;

procedure TdxCustomListBoxItems.SetSortOptions(AValue: TdxCustomListBoxSortOptions);
begin
  if AValue <> FSortOptions then
  begin
    FSortOptions := AValue;
    if Sorted then
    begin
      DoSort;
      Changed;
    end;
  end;
end;

{ TdxCustomCheckListBoxItems }

function TdxCustomCheckListBoxItems.AddObject(const ACaption: string; AData: TObject): TdxCustomCheckListBoxItem;
begin
  Result := inherited AddObject(ACaption, AData) as TdxCustomCheckListBoxItem;
end;

function TdxCustomCheckListBoxItems.GetItem(Index: Integer): TdxCustomCheckListBoxItem;
begin
  Result := inherited GetItem(Index) as TdxCustomCheckListBoxItem;
end;

function TdxCustomCheckListBoxItems.GetItemClass: TdxCustomListBoxItemClass;
begin
  Result := TdxCustomCheckListBoxItem;
end;

{ TdxCustomListBox }

constructor TdxCustomListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemHeight := 16;
  DoubleBuffered := not IsWinSeven;
  FItems := GetItemsClass.Create(Self);
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoImageListChanged;
  FColumns := 0;
  FHintHelper := TdxCustomListBoxHintHelper.Create(Self);
  BorderStyle := cxcbsDefault;
  ParentColor := False;
  Color := clWindow;
  Keys := [kArrows, kChars];
  IncrementalSearch := True;
  FAutoCompleteDelay := 1000;
end;

destructor TdxCustomListBox.Destroy;
begin
  BeginUpdate;
  Images := nil;
  EndMouseTracking(Self);
  FreeAndNil(FChangeLink);
  FreeAndNil(FHintHelper);
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxCustomListBox.Add(const ACaption: string; AImageIndex: Integer = -1): Integer;
begin
  Result := AddItem(ACaption, nil, AImageIndex);
end;

function TdxCustomListBox.AddItem(const ACaption: string;
  AObject: TObject; AImageIndex: Integer = -1): Integer;
begin
  Result := FItems.Add(ACaption, AImageIndex, AObject).Index;
end;

procedure TdxCustomListBox.AdjustItemFont(
  AFont: TFont; AItem: TdxCustomListBoxItem; AState: TcxButtonState);
begin
  //nothing
end;

function TdxCustomListBox.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

procedure TdxCustomListBox.BeginUpdate;
begin
  FItems.BeginUpdate;
end;

procedure TdxCustomListBox.Clear;
begin
  FItems.BeginUpdate;
  try
    FItems.Clear;
    FItemIndex := -1;
  finally
    FItems.EndUpdate;
  end;
end;

procedure TdxCustomListBox.EndUpdate;
begin
  FItems.EndUpdate;
end;

procedure TdxCustomListBox.BoundsChanged;
begin
  inherited BoundsChanged;
  LayoutChanged;
end;

function TdxCustomListBox.CanFocusIndex(AItemIndex: Integer): Boolean;
begin
  Result := True;
end;

function TdxCustomListBox.CanProcessIncSearch(Key: Char): Boolean;
begin
  Result := IncrementalSearch and (Count > 0) and IsIncSearchChar(Key);
end;

function TdxCustomListBox.CanShowHint(
  AItemIndex: Integer; out AItemRect, AItemTextRect: TRect): Boolean;
begin
  Result := False;
  if Items.IsValidIndex(AItemIndex) then
  begin
    cxScreenCanvas.Font := Font;
    AItemRect := ItemRect(AItemIndex);
    AItemTextRect := GetItemTextRect(AItemRect);
    Result := cxScreenCanvas.TextWidth(Items[AItemIndex].Caption) > cxRectWidth(AItemTextRect);
    cxScreenCanvas.Dormant;
  end;
end;

function TdxCustomListBox.CanStartIncSearch(Key: Char): Boolean;
begin
  Result := IncrementalSearch and (Count > 0) and IsStartIncSearchChar(Key);
end;

function TdxCustomListBox.CanUpdateHotState: Boolean;
begin
  Result := HandleAllocated and Focused and (DragAndDropState = ddsNone);
end;

function TdxCustomListBox.CalculateContentSize(AMaxVisibleItemsCount: Integer): TSize;
var
  I: Integer;
begin
  Result := cxNullSize;
  cxScreenCanvas.Font := Font;
  cxScreenCanvas.Font.Style := cxScreenCanvas.Font.Style + [fsBold];
  for I := 0 to Items.Count - 1 do
    Result.cx := Max(Result.cx, cxScreenCanvas.TextWidth(Items[I].Caption));
  for I := 0 to Min(Items.Count, AMaxVisibleItemsCount) - 1 do
    Inc(Result.cy, ItemsHeight[I]);
  Inc(Result.cx, dxGetSystemMetrics(SM_CXVSCROLL, ScaleFactor) + cxMarginsWidth(TextOffsets) + ImagesAreaSize);
  cxScreenCanvas.Dormant;
  Inc(Result.cx, 2 * BorderSize);
  Inc(Result.cy, 2 * BorderSize);
end;

function TdxCustomListBox.CalculateItemHeight: Integer;
begin
  cxScreenCanvas.Font := Font;
  cxScreenCanvas.Font.Style := cxScreenCanvas.Font.Style + [fsBold];
  Result := InternalGetItemHeight;
  cxScreenCanvas.Dormant;
end;

function TdxCustomListBox.InternalGetItemHeight: Integer;
begin
  Result := Max(ImageSize.cy + cxMarginsHeight(ImageOffsets),
    cxMarginsHeight(TextOffsets) + cxTextHeight(cxScreenCanvas.Handle));
end;

function TdxCustomListBox.GetHasSeparator(AItem: TdxCustomListBoxItem): Boolean;
begin
  Result := (AItem.Index + 1 < Items.Count) and AItem.HasSeparator;
end;

function TdxCustomListBox.GetItemsClass: TdxCustomListBoxItemsClass;
begin
  Result := TdxCustomListBoxItems;
end;

function TdxCustomListBox.GetItemTextBestWidth: Integer;
var
  I: Integer;
  ATextOffsets: TRect;
begin
  Result := 0;
  cxScreenCanvas.Font := Font;
  for I := 0 to Count - 1 do
    Result := Max(Result, cxTextWidth(cxScreenCanvas.Font, Items[I].Caption));
  cxScreenCanvas.Dormant;
  ATextOffsets := TextOffsets;
  Inc(Result, ATextOffsets.Left + ATextOffsets.Right);
end;

function TdxCustomListBox.GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind;
begin
  if IsScrollBarActive(sbVertical) then
    Result := mwskVertical
  else
    if IsScrollBarActive(sbHorizontal) then
      Result := mwskHorizontal
    else
      Result := mwskNone;
end;

procedure TdxCustomListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WindowClass.style := Params.WindowClass.style and not (CS_VREDRAW or CS_HREDRAW);
end;

procedure TdxCustomListBox.DoLayoutChanged;
begin
  UpdateItemAutoHeight;
  UpdateColumnWidth;
  CheckTopItemIndex;
  CheckFocusedItemIndex;
  UpdateScrollBars;
  UpdateHotState;
end;

function TdxCustomListBox.FindNextItemIndex(AStartItemIndex: Integer; AGoForward: Boolean = True; AGoOnCycle: Boolean = False): Integer;
begin
  if AStartItemIndex < 0 then
    if AGoOnCycle and not AGoForward then
      AStartItemIndex := Items.Count - 1
    else
    begin
      AStartItemIndex := 0;
      AGoForward := True;
    end
  else
    if AStartItemIndex >= Items.Count then
      if AGoOnCycle and AGoForward then
        AStartItemIndex := 0
      else
      begin
        AStartItemIndex := Items.Count - 1;
        AGoForward := False;
      end;
  if CanFocusIndex(AStartItemIndex) then
    Result := AStartItemIndex
  else
  begin
    if AGoForward then
      Inc(AStartItemIndex)
    else
      Dec(AStartItemIndex);
    Result := FindNextItemIndex(AStartItemIndex, AGoForward, AGoOnCycle);
  end;
end;

procedure TdxCustomListBox.FocusNextItemIndex(AStartItemIndex: Integer; AGoForward: Boolean = True; AGoOnCycle: Boolean = False);
begin
  ItemIndex := FindNextItemIndex(AStartItemIndex, AGoForward, AGoOnCycle);
end;

procedure TdxCustomListBox.FontChanged;
begin
  inherited;
  UpdateItemAutoHeight;
  UpdateColumnWidth;
end;

procedure TdxCustomListBox.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  if not ItemAutoHeight then
    ItemHeight := MulDiv(ItemHeight, M, D);
end;

procedure TdxCustomListBox.CheckFocusedItemIndex;
begin
  if FItemIndex >= Items.Count then
    ItemIndex := Items.Count - 1;
end;

procedure TdxCustomListBox.CheckTopItemIndex;
begin
  FTopIndex := Max(Min(TopIndex, MaxTopIndex), 0);
  UpdateScrollBars;
end;

procedure TdxCustomListBox.DrawBackground;
begin
  LookAndFeelPainter.DrawBorder(Canvas, Bounds);
  Canvas.FillRect(ClientBounds, GetBackgroundColor);
end;

procedure TdxCustomListBox.DrawItem(
  const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState);
var
  AItemRect, ASeparatorRect: TRect;
  ABackgroundRect, AImageRect, ATextRect: TRect;
begin
  GetItemPartsRects(AItem, R, AItemRect, ASeparatorRect);
  ABackgroundRect := GetItemBackgroundRect(AItemRect);
  AImageRect := GetItemImageRect(AItemRect);
  ATextRect := GetItemTextRect(AItemRect);
  if UseRightToLeftAlignment then
  begin
    ABackgroundRect := TdxRightToLeftLayoutConverter.ConvertRect(ABackgroundRect, R);
    AImageRect := TdxRightToLeftLayoutConverter.ConvertRect(AImageRect, R);
    ATextRect := TdxRightToLeftLayoutConverter.ConvertRect(ATextRect, R);
    ASeparatorRect := TdxRightToLeftLayoutConverter.ConvertRect(ASeparatorRect, R);
  end;
  DrawItemBackground(ABackgroundRect, AItem, AState);
  DrawItemImage(AImageRect, AItem, AState);
  DrawItemText(ATextRect, AItem, AState);
  DrawItemSeparator(ASeparatorRect);
end;

procedure TdxCustomListBox.DrawItemBackground(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState);
var
  AColor: TColor;
begin
  AColor := GetItemBackgroundColor(AItem, AState);
  if AColor <> clDefault then
    Canvas.FillRect(R, AColor);
end;

procedure TdxCustomListBox.DrawItemImage(
  const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState);
var
  AImageRect: TRect;
begin
  if IsImageAssigned(Images, AItem.ImageIndex) then
  begin
    AImageRect := cxRectCenter(R, ImageSize);
    cxDrawImage(Canvas.Handle, AImageRect, AImageRect, nil, Images, AItem.ImageIndex, idmNormal);
  end;
end;

procedure TdxCustomListBox.DrawItemText(
  const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState);
begin
  Canvas.Font := Font;
  Canvas.Font.Color := GetTextColor(AItem, AState);
  AdjustItemFont(Canvas.Font, AItem, AState);
  Canvas.DrawTexT(AItem.Caption, R, GetTextFlags, True, ra0);
end;

procedure TdxCustomListBox.DrawItems;
var
  I: Integer;
  R: TRect;
begin
  Canvas.SaveClipRegion;
  try
    Canvas.IntersectClipRect(ClientBounds);
    Canvas.Font := Font;
    Canvas.Brush.Style := bsClear;
    R := cxRectSetHeight(ItemsAreaRect, 0);
    for I := TopIndex to Items.Count - 1 do
    begin
      R := GetItemRect(I, R);
      if (R.Bottom > ItemsAreaRect.Bottom) and not NeedDrawPartVisibleItem then
        Break;
      if Canvas.RectVisible(R) then
        DrawItem(R, Items[I], GetItemState(I));
      if  R.Top > ItemsAreaRect.Bottom then
        Break;
    end;
  finally
    Canvas.RestoreClipRegion;
  end;
end;

procedure TdxCustomListBox.DrawItemSeparator(const R: TRect);
begin
  if not cxRectIsEmpty(R) then
    LookAndFeelPainter.DrawDropDownListBoxScaledSeparator(Canvas, R, ImagesAreaRect, ScaleFactor);
end;

procedure TdxCustomListBox.EraseBackground(DC: HDC);
begin
end;

function TdxCustomListBox.ItemAtPos(const APoint: TPoint; AExistOnly: Boolean = False): Integer;
var
  AItemRect, ASeparatorRect: TRect;
  I: Integer;
  R: TRect;
begin
  Result := -1;
  if PtInRect(ItemsAreaRect, APoint) then
  begin
    R := cxRectSetHeight(ItemsAreaRect, 0);
    for I := TopIndex to Items.Count - 1 do
    begin
      R := GetItemRect(I, R);
      GetItemPartsRects(Items[I], R, AItemRect, ASeparatorRect);
      if UseRightToLeftAlignment then
        AItemRect := TdxRightToLeftLayoutConverter.ConvertRect(AItemRect, R);
      if PtInRect(AItemRect, APoint) then
      begin
        Result := I;
        Exit;
      end;
    end;
    if not AExistOnly then
      Result := Items.Count;
    if Result = -1 then
      Result := ItemIndex;
  end;
end;

function TdxCustomListBox.ItemRect(AIndex: Integer): TRect;
var
  I: Integer;
begin
  Result := cxRectSetHeight(ItemsAreaRect, 0);
  if AIndex >= TopIndex then
  begin
    for I := TopIndex to AIndex do
      Result := GetItemRect(I, Result);
  end;
end;

procedure TdxCustomListBox.LayoutChanged;
begin
  if not IsLocked then
  begin
    DoLayoutChanged;
    Invalidate;
  end;
end;

procedure TdxCustomListBox.MakeVisible(AIndex: Integer);
var
  APageSize, AScrollStep, ATopIndex: Integer;
begin
  APageSize := PageSize;
  if Columns > 0 then
  begin
    if InRange(AIndex, TopIndex, TopIndex + APageSize) then
      Exit;
    AScrollStep := GetScrollStep;
    ATopIndex := 0;
    while ATopIndex < MaxTopIndex do
    begin
      if ATopIndex + APageSize >= AIndex then
        Break
      else
        Inc(ATopIndex, AScrollStep);
    end;
    TopIndex := ATopIndex;
  end
  else
    if AIndex < TopIndex then
      TopIndex := AIndex
    else
      if AIndex >= TopIndex + APageSize then
        TopIndex := AIndex - APageSize + 1;
end;

function TdxCustomListBox.HasBackground: Boolean;
begin
  Result := False;
end;

procedure TdxCustomListBox.InitScrollBarsParameters;
begin
  if Columns = 0 then
    SetScrollBarInfo(sbVertical, 0, Items.Count - 1, GetScrollStep, PageSize, TopIndex, True, True)
  else
    SetScrollBarInfo(sbHorizontal, 0,  Items.Count - 1, GetScrollStep, PageSize, TopIndex, True, True);
end;

procedure TdxCustomListBox.InvalidateItem(AIndex: Integer);
begin
  if (AIndex >= TopIndex) and (AIndex <= TopIndex + PageSize) then
    InvalidateRect(ItemRect(AIndex), False);
end;

procedure TdxCustomListBox.InvalidateItems(const AIndex1, AIndex2: Integer);
begin
  if AIndex1 <> AIndex2 then
  begin
    InvalidateItem(AIndex1);
    InvalidateItem(AIndex2);
  end;
end;

function TdxCustomListBox.IsIncSearchChar(AChar: Char): Boolean;
begin
  Result := AChar >= ' ';
end;

function TdxCustomListBox.IsStartIncSearchChar(AChar: Char): Boolean;
begin
  Result := AChar > ' ';
end;

procedure TdxCustomListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if ProcessNavigationKey(Key, Shift) then
  begin
    ClearIncrementalSearch;
    Key := 0;
  end;
end;

procedure TdxCustomListBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if ProcessKeyPress(Key) then
    Key := #0;
end;

procedure TdxCustomListBox.MeasureItemHeight(AIndex: Integer; var AHeight: Integer);
begin
  if GetHasSeparator(Items[AIndex]) then
    Inc(AHeight, LookAndFeelPainter.DropDownListBoxScaledSeparatorSize(ScaleFactor));
  dxAdjustToTouchableSize(AHeight, ScaleFactor);
end;

procedure TdxCustomListBox.ScaleFactorChanged;
begin
  inherited;
  UpdateItemAutoHeight;
  UpdateColumnWidth;
end;

procedure TdxCustomListBox.InternalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ItemIndex := ItemAtPos(Point(X, Y), True);
end;

procedure TdxCustomListBox.InternalMouseMove(Shift: TShiftState; X, Y: Integer);
begin
//
end;

procedure TdxCustomListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  HintHelper.MouseDown;
  if (Button = mbLeft) and PtInRect(ItemsAreaRect, Point(X, Y)) then
    InternalMouseDown(Button, Shift, X, Y);
  Click;
end;

procedure TdxCustomListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  InternalMouseMove(Shift, X, Y);
  UpdateHotState;
end;

function TdxCustomListBox.NeedDrawPartVisibleItem: Boolean;
begin
  Result := False;
end;

function TdxCustomListBox.NeedHotTrack: Boolean;
begin
  Result := False;
end;

function TdxCustomListBox.NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := False;
end;

function TdxCustomListBox.NeedUseHotColor(AItem: TdxCustomListBoxItem; AState: TcxButtonState): Boolean;
begin
  Result := NeedHotTrack and (AState = cxbsHot);
end;

function TdxCustomListBox.NeedUseSelectionColor(AItem: TdxCustomListBoxItem; AState: TcxButtonState): Boolean;
begin
  Result := AState = cxbsPressed;
end;

procedure TdxCustomListBox.MouseEnter(AControl: TControl);
begin
  inherited MouseEnter(AControl);
  UpdateHotState;
  BeginMouseTracking(Self, Bounds, Self);
end;

procedure TdxCustomListBox.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  UpdateHotState;
end;

procedure TdxCustomListBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

function TdxCustomListBox.ProcessKeyPress(var Key: Char): Boolean;
begin
  Result := CanProcessIncSearch(Key) and DoIncrementalSearch(Key);
end;

procedure TdxCustomListBox.DoImageListChanged(Sender: TObject);
begin
  if not IsLocked then
    DoLayoutChanged;
end;

procedure TdxCustomListBox.Paint;
begin
  DrawBackground;
  DrawItems;
end;

procedure TdxCustomListBox.ClearIncrementalSearch;
begin
  FSearchText := '';
end;

function TdxCustomListBox.DoIncrementalSearch(var Key: Char): Boolean;
begin
  if GetTickCount - FStartIncrementalSearch > FAutoCompleteDelay then
    ClearIncrementalSearch;
  FStartIncrementalSearch := GetTickCount;
  SearchText := SearchText + Key;
  Result := FocusNextItemWithText(SearchText);
end;

function TdxCustomListBox.FocusNextItemWithText(const AText: string): Boolean;
var
  AStartIndex: Integer;
begin
  if Count = 0 then
    Result := False
  else
  begin
    AStartIndex := Max(ItemIndex, 0);
    Result := ((AStartIndex < Count) and FocusItemWithText(AText, AStartIndex, Count - 1)) or
      FocusItemWithText(AText, 0, AStartIndex - 1);
  end;
end;

function TdxCustomListBox.FocusItemWithText(const AText: string; AStartIndex, AFinishIndex: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AStartIndex <= AFinishIndex then
  begin
    for I := AStartIndex to AFinishIndex do
      if Items.IsValidIndex(I) and IsItemWithText(Items[I], AText) then
      begin
        ItemIndex := I;
        Result := True;
        Break;
      end;
  end
  else
  begin
    for I := AStartIndex downto AFinishIndex do
      if Items.IsValidIndex(I) and IsItemWithText(Items[I], AText) then
      begin
        ItemIndex := I;
        Result := True;
        Break;
      end;
  end;
end;

function TdxCustomListBox.IsItemWithText(AItem: TdxCustomListBoxItem; const AText: string): Boolean;
begin
  Result := AnsiStartsText(AText, AItem.Caption);
end;

function TdxCustomListBox.ProcessNavigationKey(var Key: Word; Shift: TShiftState): Boolean;

    function FocusNextColumnItem(AStep: Integer): Boolean;
    const
      AScrollCode: array[Boolean] of TScrollCode = (scLineUp, scLineDown);
    var
      AItemIndex, AIndex, AScrollPos: Integer;
    begin
      Result := False;
      AItemIndex := ItemIndex;
      AIndex := ItemIndex + AStep;
      if (AStep < 0) and (AIndex < 0) then
        Exit;
      if ((AStep < 0) and (AIndex < FTopIndex)) or
         ((AStep > 0) and (AIndex >= FTopIndex + PageSize) and (AIndex <= Count - 1)) then
        Scroll(sbHorizontal, AScrollCode[AStep > 0], AScrollPos);
      ItemIndex := AIndex;
      Result := ItemIndex <> AItemIndex;
    end;

begin
  Result := True;
  case Key of
    VK_PRIOR:
      if ssCtrl in Shift then
        FocusNextItemIndex(0)
      else
        if Columns > 0 then
        begin
          if not FocusNextColumnItem(-PageSize) then
            ItemIndex := 0;
        end
        else
          FocusNextItemIndex(ItemIndex - PageSize, False);
    VK_NEXT:
      if ssCtrl in Shift then
        FocusNextItemIndex(Items.Count - 1, False)
      else
        if Columns > 0 then
          FocusNextColumnItem(PageSize - Integer((FTopIndex = 0) and (ItemIndex = 0)))
        else
          FocusNextItemIndex(ItemIndex + PageSize);
    VK_END:
        FocusNextItemIndex(Items.Count - 1, False);
    VK_HOME:
        FocusNextItemIndex(0);
    VK_LEFT:
      if Columns > 0 then
        FocusNextColumnItem(-GetScrollStep * IfThen(UseRightToLeftAlignment, -1, 1));
    VK_UP:
      if Columns > 0 then
        FocusNextColumnItem(-1)
      else
        FocusNextItemIndex(ItemIndex - 1, False, LoopedNavigation);
    VK_RIGHT:
      if Columns > 0 then
        FocusNextColumnItem(GetScrollStep * IfThen(UseRightToLeftAlignment, -1, 1));
    VK_DOWN:
      if Columns > 0 then
        FocusNextColumnItem(1)
      else
        FocusNextItemIndex(ItemIndex + 1, True, LoopedNavigation);
    else
      Result := False;
  end;
end;

function TdxCustomListBox.Remove(AObject: TObject): Integer;
begin
  Result := FItems.FreeAndRemove(AObject);
  LayoutChanged;
end;

procedure TdxCustomListBox.SetItemIndex(AIndex: Integer);
begin
  if (AIndex >= Items.Count) and (Items.Count > 0) then
    AIndex := Items.Count - 1;
  if FItemIndex <> AIndex then
  begin
    InvalidateItems(AIndex, FItemIndex);
    FItemIndex := AIndex;
    if ItemIndex > -1 then
      MakeVisible(ItemIndex);
    Click;
  end;
end;

procedure TdxCustomListBox.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
var
  AStepCount, AScrollStep: Integer;
begin
  if IsLocked or (AScrollCode = scEndScroll) then
    Exit;
  case AScrollCode of
    scLineUp:
      Dec(FTopIndex, GetScrollStep);
    scLineDown:
      Inc(FTopIndex, GetScrollStep);
    scPageUp:
      Dec(FTopIndex, PageSize);
    scPageDown:
      Inc(FTopIndex, PageSize);
    scTop:
      FTopIndex := 0;
    scBottom:
      FTopIndex := Items.Count - PageSize;
    scTrack:
      if Columns = 0 then
        FTopIndex := AScrollPos
      else
      begin
        AScrollStep := GetScrollStep;
        AStepCount := (AScrollPos - FTopIndex) div AScrollStep;
        if AStepCount <> 0 then
          Inc(FTopIndex, AStepCount * AScrollStep)
        else
          if AScrollPos > MaxTopIndex - AScrollStep then
            Inc(FTopIndex, AScrollStep);
      end;
  end;
  LayoutChanged;
  if (AScrollCode <> scTrack) or (Columns = 0) then
    AScrollPos := TopIndex;
  Update;
end;

function TdxCustomListBox.PtInCaller(const P: TPoint): Boolean;
begin
  Result := PtInRect(ItemsAreaRect, P);
end;

procedure TdxCustomListBox.TrackingCallerMouseLeave;
begin
  MouseLeave(nil)
end;

procedure TdxCustomListBox.UpdateHintState;
var
  AItemRect, AItemTextRect: TRect;
begin
  if ShowHint and CanShowHint(HotIndex, AItemRect, AItemTextRect) then
    HintHelper.ShowHint(AItemRect, AItemTextRect, Items[HotIndex].Caption, False, Items[HotIndex])
  else
    HintHelper.CancelHint;
end;

procedure TdxCustomListBox.UpdateHotState;
begin
  if CanUpdateHotState then
    HotIndex := ItemAtPos(ScreenToClient(GetMouseCursorPos), True)
  else
    HotIndex := -1;
end;

procedure TdxCustomListBox.UpdateColumnWidth;
begin
  if Columns > 0 then
    FColumnWidth := (ClientBounds.Right - ClientBounds.Left) div Columns
  else
    if Count * ItemHeight > ClientBounds.Bottom - ClientBounds.Top then
      FColumnWidth := ClientBounds.Right - ClientBounds.Left - VScrollBar.Width
    else
      FColumnWidth := ClientBounds.Right - ClientBounds.Left;
end;

procedure TdxCustomListBox.UpdateItemAutoHeight;
begin
  if ItemAutoHeight then
    ItemHeight := CalculateItemHeight;
end;

function TdxCustomListBox.GetBackgroundColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultEditorBackgroundColor(not Enabled);
  if Result = clDefault then
    Result := clWindow;
end;

function TdxCustomListBox.GetCount: Integer;
begin
  Result := Items.Count;
end;

function TdxCustomListBox.GetImageOffsets: TRect;
begin
  Result := ScaleFactor.Apply(cxRect(2, 2, 2, 2));
end;

function TdxCustomListBox.GetImagesAreaRect: TRect;
begin
  Result := cxRectSetWidth(ClientBounds, ImagesAreaSize);
end;

function TdxCustomListBox.GetImagesAreaSize: Integer;
begin
  if Images <> nil then
    Result := Max(ImageSize.cx + cxMarginsWidth(ImageOffsets), ItemHeight)
  else
    Result := 0;
end;

function TdxCustomListBox.GetImageSize: TSize;
begin
  Result := dxGetImageSize(nil, Images, 0, ScaleFactor);
end;

function TdxCustomListBox.GetItemBackgroundColor(AItem: TdxCustomListBoxItem; AState: TcxButtonState): TColor;
begin
  if NeedUseSelectionColor(AItem, AState) then
    Result := LookAndFeelPainter.DefaultSelectionColor
  else
    if NeedUseHotColor(AItem, AState) then
      Result := clHighlight
    else
      Result := clDefault;
end;

function TdxCustomListBox.GetItemBackgroundRect(const AItemRect: TRect): TRect;
begin
  Result := AItemRect;
  if Columns > 1 then
    Result.Right := AItemRect.Left + ColumnWidth;
  Inc(Result.Left, ImagesAreaRect.Right - ClientBounds.Left);
end;

function TdxCustomListBox.GetItemImageRect(const AItemRect: TRect): TRect;
begin
  Result := Rect(AItemRect.Left + ImagesAreaRect.Left - ClientBounds.Left, AItemRect.Top,
    AItemRect.Left + ImagesAreaRect.Right - ClientBounds.Left, AItemRect.Bottom);
  Result := cxRectContent(Result, ImageOffsets);
end;

procedure TdxCustomListBox.GetItemPartsRects(
  AItem: TdxCustomListBoxItem; const R: TRect; out AItemRect, ASeparatorRect: TRect);
var
  ASeparatorSize: Integer;
begin
  if GetHasSeparator(AItem) then
  begin
    ASeparatorSize := LookAndFeelPainter.DropDownListBoxScaledSeparatorSize(ScaleFactor);
    AItemRect := cxRectSetHeight(R, cxRectHeight(R) - ASeparatorSize);
    ASeparatorRect := cxRectSetBottom(R, R.Bottom, ASeparatorSize);
  end
  else
  begin
    ASeparatorRect := cxNullRect;
    AItemRect := R;
    if Columns > 1 then
      AItemRect.Right := AItemRect.Left + ColumnWidth;
  end;
end;

function TdxCustomListBox.GetItemRect(AItemIndex: Integer; const APrevItemRect: TRect): TRect;
begin
  Result := cxRectSetTop(APrevItemRect, APrevItemRect.Bottom, ItemsHeight[AItemIndex]);
  if Columns > 0 then
  begin
    if UseRightToLeftAlignment then
      Result.Left := Result.Right - ColumnWidth
    else
      Result.Right := Result.Left + ColumnWidth;
    if Result.Bottom > ClientBounds.Height then
      if UseRightToLeftAlignment then
        Result := cxRectSetOrigin(Result, cxPoint(Result.Left - ColumnWidth, ItemsAreaRect.Top))
      else
        Result := cxRectSetOrigin(Result, cxPoint(Result.Left + ColumnWidth, ItemsAreaRect.Top));
  end;
end;

function TdxCustomListBox.GetItemTextRect(const AItemRect: TRect): TRect;
begin
  Result := GetItemBackgroundRect(AItemRect);
  Result := cxRectContent(Result, TextOffsets);
end;

function TdxCustomListBox.GetItemsAreaRect: TRect;
begin
  Result := ClientBounds;
end;

function TdxCustomListBox.GetItemState(AIndex: Integer): TcxButtonState;
begin
  if AIndex = ItemIndex then
    Result := cxbsPressed
  else
    if AIndex = HotIndex then
      Result := cxbsHot
    else
      Result := cxbsNormal;
end;

function TdxCustomListBox.GetItemsHeight(Index: Integer): Integer;
begin
  Result := ItemHeight;
  if Index < Items.Count then
    MeasureItemHeight(Index, Result);
end;

function TdxCustomListBox.GetIsLocked: Boolean;
begin
  Result := (FItems = nil) or (FItems.LockCount > 0);
end;

function TdxCustomListBox.GetItemObject: TObject;
begin
  if ItemIndex <> -1 then
    Result := Items[ItemIndex].Data
  else
    Result := nil;
end;

function TdxCustomListBox.GetMaxTopIndex: Integer;
var
  APageSize, AScrollStep: Integer;
  R: TRect;
begin
  if Columns > 0 then
  begin
    APageSize := PageSize;
    AScrollStep := GetScrollStep;
    Result := 0;
    while Result + APageSize < Count - 1 do
      Inc(Result, AScrollStep);
  end
  else
  begin
    Result := Items.Count - 1;
    R := ItemsAreaRect;
    while Result >= 0 do
    begin
      Dec(R.Bottom, ItemsHeight[Result]);
      if R.Bottom >= R.Top then
        Dec(Result)
      else
        Break;
    end;
    Result := Min(Result + 1, Items.Count - 1);
  end;
end;

function TdxCustomListBox.GetTextColor(
  AItem: TdxCustomListBoxItem; AState: TcxButtonState): TColor;
begin
  if Font.Color <> clWindowText then
    Result := Font.Color
  else
    if NeedUseSelectionColor(AItem, AState) then
      Result := LookAndFeelPainter.DefaultSelectionTextColor
    else
      if NeedUseHotColor(AItem, AState) then
        Result := clHighlightText
      else
        Result := LookAndFeelPainter.DefaultEditorTextColor(AState = cxbsDisabled);
end;

function TdxCustomListBox.GetTextFlags: Integer;
var
  AAlignmentHorz: Integer;
begin
  Result := cxAlignVCenter or cxSingleLine or cxShowEndEllipsis;
  if UseRightToLeftAlignment then
    AAlignmentHorz := cxAlignRight
  else
    AAlignmentHorz := cxAlignLeft;
  Result := Result or AAlignmentHorz;
  if UseRightToLeftReading then
    Result := Result or cxRtlReading;
end;

function TdxCustomListBox.GetTextOffsets: TRect;
begin
  Result := cxRect(cxTextOffset, cxTextOffset, cxTextOffset, cxTextOffset);
end;

function TdxCustomListBox.GetVScrollBarBounds: TRect;
begin
  if IsPopupScrollBars then
    Result := inherited GetVScrollBarBounds
  else
  begin
    Result := ItemsAreaRect;
    Result.Left := Result.Right;
    Result.Right := Result.Left + VScrollBar.Width;
    if UseRightToLeftScrollBar then
      Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, ClientBounds);
  end;
end;

function TdxCustomListBox.GetPageSize: Integer;

  function GetBottomIndex: Integer;
  var
    R: TRect;
  begin
    Result := TopIndex;
    R := ItemsAreaRect;
    while Result < Items.Count do
    begin
      Inc(R.Top, ItemsHeight[Result]);
      if R.Top > R.Bottom then Break;
      Inc(Result);
    end;
  end;

begin
  if Columns > 0 then
    Result := GetScrollStep * Columns
  else
    Result := Max(GetBottomIndex - TopIndex, 1);
end;

function TdxCustomListBox.GetScrollStep: Integer;
begin
  if Columns = 0 then
    Result := 1
  else
    Result := (ClientBounds.Bottom - ClientBounds.Top) div ItemHeight;
end;

function TdxCustomListBox.GetSorted: Boolean;
begin
  Result := Items.Sorted;
end;

function TdxCustomListBox.GetSortOptions: TdxCustomListBoxSortOptions;
begin
  Result := Items.SortOptions;
end;

function TdxCustomListBox.GetTopIndex: Integer;
begin
  Result := FTopIndex;
  if Result >= Items.Count then
    Result := Items.Count - 1;
  if Result < 0 then
    Result := 0;
end;

procedure TdxCustomListBox.SetColumns(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FColumns <> AValue then
  begin
    FColumns := AValue;
    LayoutChanged;
  end;
end;

procedure TdxCustomListBox.SetHotIndex(AValue: Integer);
begin
  if FHotIndex <> AValue then
  begin
    InvalidateItems(FHotIndex, AValue);
    FHotIndex := AValue;
    UpdateHintState;
  end;
end;

procedure TdxCustomListBox.SetImages(AValue: TCustomImageList);
begin
  cxSetImageList(AValue, FImages, FChangeLink, Self);
end;

procedure TdxCustomListBox.SetItemObject(Value: TObject);
begin
  ItemIndex := Items.IndexOfObject(Value);
end;

procedure TdxCustomListBox.SetItemHeight(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FItemHeight <> AValue then
  begin
    FItemHeight := AValue;
    LayoutChanged;
  end;
end;

procedure TdxCustomListBox.SetSorted(AValue: Boolean);
begin
  Items.Sorted := AValue;
end;

procedure TdxCustomListBox.SetSortOptions(AValue: TdxCustomListBoxSortOptions);
begin
  Items.SortOptions := AValue;
end;

procedure TdxCustomListBox.SetTopIndex(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if AValue <> FTopIndex then
  begin
    FTopIndex := AValue;
    LayoutChanged;
  end;
end;

{ TdxCustomListBoxHintHelper }

constructor TdxCustomListBoxHintHelper.Create(AOwnerControl: TdxCustomListBox);
begin
  inherited Create;
  FOwnerControl := AOwnerControl;
end;

function TdxCustomListBoxHintHelper.GetOwnerControl: TcxControl;
begin
  Result := FOwnerControl;
end;

function TdxCustomListBoxHintHelper.PtInCaller(const P: TPoint): Boolean;
begin
  Result := PtInRect(GetHintControl.Bounds, P);
end;

{ TdxCustomCheckListBox }

constructor TdxCustomCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelection := TdxFastList.Create;
  FDragAndDropLeftArrowRect := cxInvalidRect;
  FDragAndDropRightArrowRect := cxInvalidRect;
  ItemAutoHeight := True;
  FItemMoving := True;
  FShowCheckBoxes := True;
end;

destructor TdxCustomCheckListBox.Destroy;
begin
  FreeAndNil(FSelection);
  inherited Destroy;
end;

procedure TdxCustomCheckListBox.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
    OnDragDrop(Self, Source, X, Y)
  else
    InternalDragDrop(Source, X, Y);
end;

function TdxCustomCheckListBox.StartDrag(DragObject: TDragObject): Boolean;
begin
  Result := ItemIndex > -1;
end;

procedure TdxCustomCheckListBox.AdjustSize;
begin
  if not IsLocked then
  begin
    HandleNeeded;
    inherited AdjustSize;
  end;
end;

function TdxCustomCheckListBox.CalculateAutoHeight: Integer;
begin
  Result := GetVisibleItemCount * ItemHeight + 2 * BorderWidth;
end;

function TdxCustomCheckListBox.CalculateAutoWidth(AUseCachedTextMaxWidth: Boolean = False): Integer;
begin
  Result := GetAutoSizeItemWidth(AUseCachedTextMaxWidth);
  if Columns > 0 then
    Result := Result * Columns;
  if VisibleWidth <> 0 then
    Result := Min(Result, VisibleWidth);
  if GetVisibleItemCount < Items.Count then
    Inc(Result,  VScrollBar.Width);
end;

function TdxCustomCheckListBox.CanAutoSize(var NewWidth: Integer; var NewHeight: Integer): Boolean;
begin
  Result := True;
  NewHeight := CalculateAutoHeight;
  NewWidth := CalculateAutoWidth;
end;

function TdxCustomCheckListBox.CanDrag(X, Y: Integer): Boolean;
begin
  Result := not Sorted and ItemMoving and (DragAndDropState = ddsStarting);
end;

function TdxCustomCheckListBox.CanProcessIncSearch(Key: Char): Boolean;
begin
  if (Key = #32) and (SearchText = '') then
    Result := False
  else
    Result := inherited CanProcessIncSearch(Key);
end;

function TdxCustomCheckListBox.CanUpdateHotState: Boolean;
begin
  Result := HandleAllocated and (DragAndDropState = ddsNone);
end;

procedure TdxCustomCheckListBox.CheckedChanged(AItemIndex: Integer);
begin
  InvalidateItem(AItemIndex);
end;

procedure TdxCustomCheckListBox.ClearSelection;
begin
  if GetSelectionCount = 0 then
    Exit;
  Selection.Clear;
  if Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self);
  LayoutChanged;
end;

procedure TdxCustomCheckListBox.Click;
begin
  if NeedHandleClick then
  begin
    DoClick;
    ClearClickedState;
  end;
  inherited Click;
end;

procedure TdxCustomCheckListBox.DblClick;
begin
  if Items.IsValidIndex(ItemIndex) and HasCheckBox(ItemIndex) then
    DoClick;
end;

procedure TdxCustomCheckListBox.DoAction;
begin
  ItemAction(ItemIndex);
end;

procedure TdxCustomCheckListBox.DoClick;
begin
  DoAction;
end;

procedure TdxCustomCheckListBox.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  inherited DoEndDrag(Target, X, Y);
  DragAndDropState := ddsNone;
  ScrollDirection := dirNone;
end;

function TdxCustomCheckListBox.DoIncrementalSearch(var Key: Char): Boolean;
begin
  Result := inherited DoIncrementalSearch(Key);
  if Result and MultiSelect then
    SelectFocusedItem;
end;

procedure TdxCustomCheckListBox.DoItemAction(AItemIndex: Integer);
begin
  if Assigned(FOnAction) then
    FOnAction(Self, AItemIndex);
end;

procedure TdxCustomCheckListBox.DoLayoutChanged;
begin
  inherited DoLayoutChanged;
  if AutoSize then
    AdjustSize;
end;

procedure TdxCustomCheckListBox.DoMultiSelectionAction;
var
  I: Integer;
  AValue: TdxDefaultBoolean;
begin
  if not Items.IsValidIndex(ItemIndex) then
    Exit;
  ShowHourglassCursor;
  try
    BeginUpdate;
    try
      AValue := dxBooleanToDefaultBoolean(not Checked[ItemIndex]);
      for I := 0 to Selection.Count - 1 do
        ItemAction(TdxNativeUInt(Selection[I]), AValue);
    finally
      EndUpdate;
    end;
  finally
    HideHourglassCursor;
  end;
end;

procedure TdxCustomCheckListBox.DoStartDrag(var DragObject: TDragObject);
begin
  inherited DoStartDrag(DragObject);
  if DragObject <> nil then
    FDragItemIndex := -1;
end;

procedure TdxCustomCheckListBox.DragCanceled;
var
  AKeyState: TKeyboardState;
  AShiftState: TShiftState;
  P: TPoint;
begin
  FIsDragCanceled := True;
  inherited DragCanceled;
  AShiftState := [];
  if GetKeyboardState(AKeyState) then
    AShiftState := KeyboardStateToShiftState(AKeyState);
  P := ScreenToClient(GetMouseCursorPos);
  MouseUp(mbLeft, AShiftState, P.X, P.Y);
  FIsDragCanceled := False;
end;

procedure TdxCustomCheckListBox.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);

  function GetArrowBounds(APlace: TcxArrowPlace): TRect;
  const
    AArrowOffset = 1;
  begin
    Result := ItemRect(FDragItemIndex);
    if Columns > 1 then
      Result.Right := Result.Left + ColumnWidth;
    if Result.Top = Height then
      Dec(Result.Top, cxTextOffset);
    Result.Bottom := Result.Top;
    Result := TcxDragAndDropArrow.CalculateBounds(Result, cxEmptyRect, APlace, ScaleFactor);
    if APlace = apLeft then
      OffsetRect(Result, Result.Right - Result.Left + AArrowOffset, 0)
    else
      OffsetRect(Result, -(Result.Right - Result.Left + AArrowOffset), 0);
  end;

  function CanDragOver(AInsertionIndex: Integer): Boolean;
  var
    AItems: TList;
    I: Integer;
  begin
    Result := AInsertionIndex <> -1;
    if Result and Assigned(OnItemDragOver) then
    begin
      AItems := GetSelectedItems(True);
      try
        for I := 0 to AItems.Count - 1 do
        begin
          OnItemDragOver(AItems[I], Result);
          if not Result then
            Break;
        end;
      finally
        AItems.Free;
      end;
    end;
  end;

  procedure HideInsertionMark;
  var
    ALeft, ARight: TRect;
  begin
    if FDragItemIndex = -1 then Exit;
    ALeft := FDragAndDropLeftArrowRect;
    ARight := FDragAndDropRightArrowRect;
    FDragAndDropLeftArrowRect := cxInvalidRect;
    FDragAndDropRightArrowRect := cxInvalidRect;
    cxInvalidateRect(Self, ALeft);
    cxInvalidateRect(Self, ARight);
  end;

  procedure CalculateInsertionMark;
  begin
    if FDragItemIndex = -1 then Exit;
    FDragAndDropLeftArrowRect := GetArrowBounds(apLeft);
    FDragAndDropRightArrowRect := GetArrowBounds(apRight);
    cxInvalidateRect(Self, FDragAndDropLeftArrowRect);
    cxInvalidateRect(Self, FDragAndDropRightArrowRect);
  end;

  procedure CheckScrolling;
  begin
    if FDragItemIndex = -1 then
      ScrollDirection := dirNone
    else
      if VScrollBarVisible then
        if Y <= dxListBoxScrollZoneWidth then
          ScrollDirection := dirUp
        else
          if Y >= ClientHeight - dxListBoxScrollZoneWidth then
            ScrollDirection := dirDown
          else
            ScrollDirection := dirNone
      else
        if HScrollBarVisible then
          if X <= dxListBoxScrollZoneWidth then
            ScrollDirection := dirLeft
          else
            if X >= ClientWidth - dxListBoxScrollZoneWidth then
              ScrollDirection := dirRight
            else
              ScrollDirection := dirNone
  end;

var
  AItemIndex: Integer;
begin
  Accept := (DragAndDropState <> ddsNone) and ItemMoving;
  if not Accept then
    Exit;
  inherited;
  if Assigned(OnDragOver) and not Accept then
    Exit;
  AItemIndex := GetDragItemInsertionIndex(X, Y);
  Accept := CanDragOver(AItemIndex);
  if Accept then
  begin
    DragAndDropState := ddsInProcess;
    if State = dsDragLeave then
      AItemIndex := -1;
    if FDragItemIndex <> AItemIndex then
    begin
      HideInsertionMark;
      FDragItemIndex := AItemIndex;
      CalculateInsertionMark;
    end;
    CheckScrolling;
  end;
end;

procedure TdxCustomCheckListBox.DrawDragAndDropArrows;
begin
  if not cxRectIsInvalid(FDragAndDropLeftArrowRect) then
    TcxDragAndDropArrow.Draw(Canvas, FDragAndDropLeftArrowRect, apLeft, ScaleFactor);
  if not cxRectIsInvalid(FDragAndDropRightArrowRect) then
    TcxDragAndDropArrow.Draw(Canvas, FDragAndDropRightArrowRect, apRight, ScaleFactor);
end;

procedure TdxCustomCheckListBox.DrawItem(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState);
var
  AItemRect, ASeparatorRect: TRect;
  ABackgroundRect, AImageRect, ATextRect: TRect;
  AFocusRect, ACheckBoxRect: TRect;
begin
  GetItemPartsRects(AItem, R, AItemRect, ASeparatorRect);
  ABackgroundRect := GetItemBackgroundRect(AItemRect);
  AImageRect := GetItemImageRect(AItemRect);
  ATextRect := GetItemTextRect(AItemRect);
  AFocusRect := GetItemFocusRect(AItemRect, AItem);
  ACheckBoxRect := GetItemCheckBoxRect(AItemRect);
  if UseRightToLeftAlignment then
  begin
    ABackgroundRect := TdxRightToLeftLayoutConverter.ConvertRect(ABackgroundRect, R);
    AImageRect := TdxRightToLeftLayoutConverter.ConvertRect(AImageRect, R);
    ATextRect := TdxRightToLeftLayoutConverter.ConvertRect(ATextRect, R);
    ASeparatorRect := TdxRightToLeftLayoutConverter.ConvertRect(ASeparatorRect, R);
    AFocusRect := TdxRightToLeftLayoutConverter.ConvertRect(AFocusRect, R);
    ACheckBoxRect := TdxRightToLeftLayoutConverter.ConvertRect(ACheckBoxRect, R);
  end;
  DrawItemBackground(ABackgroundRect, AItem, AState);
  if MultiSelect and (AItem.Index = ItemIndex) and not Selected[ItemIndex] then
    DrawItemFocus(AFocusRect);
  if HasCheckBox(AItem.Index) then
    DrawItemCheckBox(ACheckBoxRect, AItem, AState);
  if IsImageAssigned(Images, AItem.ImageIndex) then
    DrawItemImage(AImageRect, AItem, AState);
  DrawItemText(ATextRect, AItem, AState);
  DrawItemSeparator(ASeparatorRect);
end;

procedure TdxCustomCheckListBox.DrawItemCheckBox(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState);
var
  ACheckItem: TdxCustomCheckListBoxItem absolute AItem;
begin
  LookAndFeelPainter.DrawScaledCheckButton(Canvas, R, GetCheckBoxState(AItem.Index), ACheckItem.State, ScaleFactor);
end;

procedure TdxCustomCheckListBox.DrawItemFocus(const R: TRect);
begin
  Canvas.DrawFocusRect(R);
end;

function TdxCustomCheckListBox.GetCheckBoxState(AItemIndex: Integer): TcxButtonState;
begin
  if HasCheckBox(AItemIndex) then
    if MouseCapture and HitAtItemCheckBox(AItemIndex) then
      Result := cxbsPressed
    else
      if (HotIndex = AItemIndex) and (NeedHotTrack or HitAtItemCheckBox(AItemIndex)) then
        Result := cxbsHot
      else
        Result := cxbsNormal
  else
    Result := cxbsDisabled;
end;

function TdxCustomCheckListBox.GetCheckedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Checked[I] then
      Inc(Result);
end;

function TdxCustomCheckListBox.GetCheckedItems: TList;
var
  I: Integer;
begin
  Result := TList.Create;
  for I := 0 to Count - 1 do
    if Checked[I] then
      Result.Add(Items[I]);
end;

function TdxCustomCheckListBox.GetDragItemInsertionIndex(X, Y: Integer): Integer;
var
  R: TRect;
begin
  Result := ItemAtPos(Point(X, Y), True);
  if Result = -1 then
  begin
    R := ItemRect(Count - 1);
    R.Bottom := ClientBounds.Bottom;
    if PtInRect(R, Point(X, Y)) then
      Result := Count;
  end
  else
  begin
    R := ItemRect(Result);
    if Y > GetRangeCenter(R.Top, R.Bottom) then
      Inc(Result);
  end;
end;

function TdxCustomCheckListBox.GetImagesAreaRect: TRect;
begin
  Result := inherited GetImagesAreaRect;
  if ShowCheckBoxes then
    Result := cxRectOffsetHorz(Result, CheckBoxAreaSize.cx);
end;

function TdxCustomCheckListBox.GetIsCopyDragDrop: Boolean;
begin
  Result := False;
end;

function TdxCustomCheckListBox.GetItemCheckBoxRect(const AItemRect: TRect): TRect;
begin
  Result := AItemRect;
  Result.Right := Result.Left + CheckBoxAreaSize.cx;
  Result := cxRectCenterVertically(Result, CheckBoxAreaSize.cy);
  Result := cxRectContent(Result, CheckBoxMargins);
end;

function TdxCustomCheckListBox.GetItemFocusRect(const AItemRect: TRect; AItem: TdxCustomListBoxItem): TRect;
begin
  Result := AItemRect;
  if HasCheckBox(AItem.Index) then
    Result.Left := Result.Left + CheckBoxAreaSize.cx;
  Result := cxRectInflate(Result, -1, -1);
end;

function TdxCustomCheckListBox.GetItemsClass: TdxCustomListBoxItemsClass;
begin
  Result := TdxCustomCheckListBoxItems;
end;

function TdxCustomCheckListBox.GetItemMaxWidth(const AUseCachedTextMaxWidth: Boolean = False): Integer;
begin
  if AUseCachedTextMaxWidth then
  begin
    Result := FCachedTextMaxWidth;
    Exit;
  end;
  Result := GetItemTextBestWidth;
  FCachedTextMaxWidth := Result;
end;

function TdxCustomCheckListBox.GetSelectedItems(ANeedData: Boolean): TList;
var
  I: Integer;
begin
  Result := TList.Create;
  for I := 0 to Count - 1 do
    if Selected[I] then
      if ANeedData then
        Result.Add(Items[I].Data)
      else
        Result.Add(Items[I]);
end;

function TdxCustomCheckListBox.GetSelectionCount: Integer;
begin
  Result := Selection.Count;
end;

function TdxCustomCheckListBox.GetVisibleItemCount: Integer;
begin
  Result := VisibleItemCount;
  if (Result = 0) or (Result > Items.Count) then
    Result := Items.Count;
end;

function TdxCustomCheckListBox.HasCheckBox(AItemIndex: Integer): Boolean;
begin
  Result := ShowCheckBoxes;
end;

function TdxCustomCheckListBox.HitAtItemCheckBox(AItemIndex: Integer): Boolean;
var
  R: TRect;
  AItemRect: TRect;
begin
  Result := Items.IsValidIndex(AItemIndex) and HasCheckBox(AItemIndex);
  if Result then
  begin
    AItemRect := ItemRect(AItemIndex);
    R := GetItemCheckBoxRect(AItemRect);
    if UseRightToLeftAlignment then
      R := TdxRightToLeftLayoutConverter.ConvertRect(R, AItemRect);
    Result := PtInRect(R, ScreenToClient(GetMouseCursorPos));
  end;
end;

function TdxCustomCheckListBox.InternalGetItemHeight: Integer;
begin
  Result := inherited InternalGetItemHeight;
  if ShowCheckBoxes then
    Result := Max(Result, CheckBoxAreaSize.cy);
end;

procedure TdxCustomCheckListBox.InternalDragDrop(Source: TObject; X, Y: Integer);
var
  AIndex, ANewIndex, I: Integer;
  ACheckedItems, ASelectedItems : TList;
  ANeedInsert, AChanged: Boolean;
begin
  AIndex := GetDragItemInsertionIndex(X, Y);
  if AIndex = -1 then
    Exit;
  BeginUpdate;
  try
    ACheckedItems := GetCheckedItems;
    try
      ASelectedItems := GetSelectedItems(False);
      try
        I := 0;
        while I < Count do
          if ASelectedItems.IndexOf(Items[I]) >= 0 then
          begin
            Items.Delete(I);
            if I < AIndex then
              Dec(AIndex);
          end
          else
            Inc(I);

        Selection.Clear;
        ANeedInsert := AIndex < Count;
        for I := 0 to ASelectedItems.Count - 1 do
        begin
          if ANeedInsert then
          begin
            ANewIndex := AIndex + I;
            TList(Items).Insert(ANewIndex, ASelectedItems[I]);
          end
          else
            ANewIndex := TList(Items).Add(ASelectedItems[I]);
          DoSelect(ANewIndex, True, AChanged);
          if I = 0 then
            ItemIndex := ANewIndex;
        end;
      finally
        ASelectedItems.Free;
      end;
    finally
      ACheckedItems.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomCheckListBox.ItemAction(AItemIndex: Integer; AValue: TdxDefaultBoolean = bDefault);
begin
  if not Items.IsValidIndex(AItemIndex) then
    Exit;
  if HasCheckBox(AItemIndex) then
    Checked[AItemIndex] := dxDefaultBooleanToBoolean(AValue, not Checked[AItemIndex]);
  DoItemAction(AItemIndex);
end;

function TdxCustomCheckListBox.NeedBeginDrag(AButton: TMouseButton; AShiftState: TShiftState): Boolean;
begin
  Result := (AButton = mbLeft) and (DragMode = dmAutomatic) and ItemMoving and not Sorted;
  if Result then
    if ssCtrl in AShiftState then
      Result := Selected[ItemIndex]
    else
      Result := not(MultiSelect and (ssShift in AShiftState));
end;

function TdxCustomCheckListBox.NeedDrawPartVisibleItem: Boolean;
begin
  Result := True;
end;

function TdxCustomCheckListBox.NeedHandleClick: Boolean;
begin
  Result := (csClicked in ControlState);
end;

function TdxCustomCheckListBox.NeedUseSelectionColor(AItem: TdxCustomListBoxItem; AState: TcxButtonState): Boolean;
begin
  Result := Selected[AItem.Index] or not MultiSelect and inherited NeedUseSelectionColor(AItem, AState);
end;

procedure TdxCustomCheckListBox.Paint;
begin
  inherited Paint;
  DrawDragAndDropArrows;
end;

procedure TdxCustomCheckListBox.ResetCheckBoxStates;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Checked[I] := False;
end;

procedure TdxCustomCheckListBox.InternalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  APrevIndex: Integer;
begin
  APrevIndex := ItemIndex;
  ItemIndex := ItemAtPos(Point(X, Y), True);
  FAnchorIndex := ItemIndex;
  if HitAtItemCheckBox(ItemIndex) then
    SelectFocusedItem
  else
    if NeedBeginDrag(Button, Shift) then
    begin
      if not Selected[ItemIndex] then
        SelectFocusedItem;
      DragAndDropState := ddsStarting;
      BeginDrag(False);
    end
    else
    if MultiSelect then
    begin
      SelectItemsViaMouseDown(APrevIndex, Shift);
      FSelectedViaMouseMovePriorIndex := FAnchorIndex;
      FInMouseSelectionProcess := True;
    end;
end;

procedure TdxCustomCheckListBox.InternalMouseMove(Shift: TShiftState; X, Y: Integer);
var
  X1, Y1, AItemIndex: Integer;
begin
  if not((DragAndDropState = ddsNone) and FInMouseSelectionProcess) then
    Exit;

  X1 := Min(Max(1, X), cxRectWidth(ItemsAreaRect) - 1);
  Y1 := Min(Max(1, Y), cxRectHeight(ItemsAreaRect) - 1);
  AItemIndex := ItemAtPos(Point(X1, Y1), True);
  if Items.IsValidIndex(AItemIndex) and (AItemIndex <> FSelectedViaMouseMovePriorIndex) then
    if ssCtrl in Shift then
    begin
      if ((FAnchorIndex <= AItemIndex) and (AItemIndex < FSelectedViaMouseMovePriorIndex)) or
        ((FSelectedViaMouseMovePriorIndex < AItemIndex) and (AItemIndex <= FAnchorIndex)) then
        Selected[FSelectedViaMouseMovePriorIndex] := not Selected[FSelectedViaMouseMovePriorIndex];
      if ((AItemIndex > FAnchorIndex) and (AItemIndex > FSelectedViaMouseMovePriorIndex)) or
        ((AItemIndex < FAnchorIndex) and (AItemIndex < FSelectedViaMouseMovePriorIndex)) then
        Selected[AItemIndex] := not Selected[AItemIndex];
    end
    else
    begin
      Selection.Clear;
      SelectItems(FAnchorIndex, AItemIndex, True);
    end;
  FSelectedViaMouseMovePriorIndex := AItemIndex;
end;

procedure TdxCustomCheckListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FIsDragCanceled then
  begin
    if not(ssCtrl in Shift) then
      SelectFocusedItem
    else
      Selected[ItemIndex] := not Selected[ItemIndex];
  end
  else
  if DragAndDropState <> ddsStarting then
    if not FInMouseSelectionProcess and (FAnchorIndex = ItemIndex) and not(ssCtrl in Shift) then
      SelectFocusedItem;
  FInMouseSelectionProcess := False;
end;

function TdxCustomCheckListBox.ProcessNavigationKey(var Key: Word; Shift: TShiftState): Boolean;
var
  APrevItem: Integer;
begin
  Result := True;
  case Key of
    VK_RETURN, VK_SPACE:
      if (GetSelectionCount >= 1) and Selected[ItemIndex] then
        DoMultiSelectionAction
      else
        DoAction;
    else
    begin
      APrevItem := ItemIndex;
      Result := inherited ProcessNavigationKey(Key, Shift);
      if Result and MultiSelect and not(ssCtrl in Shift) and (APrevItem <> ItemIndex) then
        SelectItemsViaMouseDown(APrevItem, Shift);
    end;
  end;
end;

procedure TdxCustomCheckListBox.SelectAll;
begin
  SelectItems(0, Count - 1, True);
end;

procedure TdxCustomCheckListBox.SelectFocusedItem;
begin
  Selection.Clear;
  Selected[ItemIndex] := True;
  Invalidate;
end;

procedure TdxCustomCheckListBox.SelectItems(AStartIndex, AFinishIndex: Integer; AValue: Boolean);
var
  AChanged: Boolean;
  I: Integer;
begin
  AChanged := False;
  for I := Min(AStartIndex, AFinishIndex) to Max(AStartIndex, AFinishIndex) do
    DoSelect(I, AValue, AChanged);
  if AChanged then
  begin
    Invalidate;
    if Assigned(FOnSelectionChanged) then
      FOnSelectionChanged(Self);
  end;
end;

procedure TdxCustomCheckListBox.SelectItemsViaMouseDown(APrevIndex: Integer; AShift: TShiftState);

  function GetNewSelectionStart: Integer;
  const
    ASign: array[Boolean] of Integer = (-1, 1);
  var
    ASelectionBlocks: TcxObjectList;
    ABlock: TdxCustomListBoxSelectionBlock;
    I: Integer;
  begin
    Result := APrevIndex;
    if not Selected[APrevIndex] then
      Inc(Result, ASign[APrevIndex < ItemIndex]);
    if (GetSelectionCount = 0) or not Selected[APrevIndex] then
      Exit;
    ASelectionBlocks := CreateSelectionBlocks;
    try
      for I := 0 to ASelectionBlocks.Count - 1 do
      begin
        ABlock := TdxCustomListBoxSelectionBlock(ASelectionBlocks[I]);
        if InRange(APrevIndex, ABlock.Start, ABlock.Finish) then
        begin
          Result := ABlock.Start;
          if APrevIndex < ABlock.Finish then
            Result := ABlock.Finish;
          Break;
        end;
      end;
    finally
      ASelectionBlocks.Free;
    end;
  end;

var
  AStart: Integer;
begin
  APrevIndex := Max(0, APrevIndex);
  if not MultiSelect or ([ssShift, ssCtrl] * AShift = []) then
    SelectFocusedItem
  else
  if ssShift in AShift then
  begin
    if ssCtrl in AShift then
      SelectItems(APrevIndex, ItemIndex, Selected[APrevIndex])
    else
    if APrevIndex = ItemIndex then
      SelectFocusedItem
    else
    begin
      AStart := GetNewSelectionStart;
      Selection.Clear;
      SelectItems(AStart, ItemIndex, True);
      Invalidate;
    end;
  end
  else
  if ssCtrl in AShift then
    Selected[ItemIndex] := not Selected[ItemIndex];
end;

procedure TdxCustomCheckListBox.ClearClickedState;
var
  AControlState: TControlState;
begin
  AControlState := ControlState;
  Exclude(AControlState, csClicked);
  ControlState := AControlState;
end;

function TdxCustomCheckListBox.CreateSelectionBlocks: TcxObjectList;
var
  I, AIndex: Integer;
  AStart, AFinish: Integer;
begin
  Result := TcxObjectList.Create;
  if GetSelectionCount = 0 then
    Exit;
  Selection.Sort(dxCompareValues);
  AStart := TdxNativeUInt(Selection[0]);
  AFinish := AStart;
  I := 1;
  while I <= GetSelectionCount - 1 do
  begin
    AIndex := TdxNativeUInt(Selection[I]);
    if AIndex = AFinish + 1 then
      AFinish := AIndex
    else
    begin
      Result.Add(TdxCustomListBoxSelectionBlock.Create(AStart, AFinish));
      AStart := AIndex;
      AFinish := AIndex;
    end;
    Inc(I);
  end;
  Result.Add(TdxCustomListBoxSelectionBlock.Create(AStart, AFinish));
end;

procedure TdxCustomCheckListBox.DoSelect(AIndex: Integer; AValue: Boolean; var AChanged: Boolean);
var
  I: Integer;
begin
  I := Selection.IndexOf(TObject(AIndex));
  if AValue then
    if I = -1 then
    begin
      Selection.Add(TObject(AIndex));
      AChanged := True;
    end
    else
  else
    if I <> -1 then
    begin
      Selection.Delete(I);
      AChanged := True;
    end;
end;

function TdxCustomCheckListBox.GetAutoSizeItemWidth(const AUseCachedTextMaxWidth: Boolean = False): Integer;
begin
  Result := 0;
  if ShowCheckBoxes then
    Result := CheckBoxAreaSize.cx;
  if Images <> nil then
    Result := ImagesAreaRect.Right;
  Inc(Result, GetItemMaxWidth(AUseCachedTextMaxWidth));
end;

function TdxCustomCheckListBox.GetCheckBoxAreaSize: TSize;
begin
  Result.cx := CheckBoxMargins.Left + LookAndFeelPainter.ScaledCheckButtonSize(ScaleFactor).cx + CheckBoxMargins.Right;
  Result.cy := CheckBoxMargins.Top + LookAndFeelPainter.ScaledCheckButtonSize(ScaleFactor).cy + CheckBoxMargins.Bottom;
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TdxCustomCheckListBox.GetCheckBoxMargins: TRect;
begin
  Result := cxRect(dxCheckBoxOffset, dxCheckBoxOffset, dxCheckBoxOffset, dxCheckBoxOffset);
  Result := ScaleFactor.Apply(Result);
end;

function TdxCustomCheckListBox.GetChecked(AIndex: Integer): Boolean;
begin
  Result := Items[AIndex].Checked;
end;

function TdxCustomCheckListBox.GetCheckedIndexes: TdxIntegerIndexes;
var
  I, AIndex: Integer;
begin
  Result := nil;
  for I := 0 to Items.Count - 1 do
    if Checked[I] then
    begin
      AIndex := Length(Result);
      SetLength(Result, AIndex + 1);
      Result[AIndex] := Items[I].Index;
    end;
end;

function TdxCustomCheckListBox.GetState(AIndex: Integer): TcxCheckBoxState;
begin
  Result := Items[AIndex].State;
end;

function TdxCustomCheckListBox.GetItems: TdxCustomCheckListBoxItems;
begin
  Result := inherited Items as TdxCustomCheckListBoxItems;
end;

function TdxCustomCheckListBox.GetSelected(AIndex: Integer): Boolean;
begin
  Result := Selection.IndexOf(TObject(AIndex)) <> -1;
end;

procedure TdxCustomCheckListBox.ScrollTimerHandler(Sender: TObject);
var
  ATopIndex, AScrollStep: Integer;
  AAccept: Boolean;
  APoint: TPoint;
begin
  if VScrollBarVisible then
    if ScrollDirection = dirUp then
      TopIndex := TopIndex - 1
    else
    begin
      ATopIndex := TopIndex;
      repeat
        Inc(ATopIndex);
        TopIndex := ATopIndex;
      until (TopIndex = ATopIndex) or (ATopIndex >= Count);
    end
  else
  begin
    AScrollStep := GetScrollStep;
    if ScrollDirection = dirLeft then
      TopIndex := TopIndex - AScrollStep
    else
    begin
      ATopIndex := TopIndex;
      repeat
        Inc(ATopIndex, AScrollStep);
        TopIndex := ATopIndex;
      until (TopIndex = ATopIndex) or (ATopIndex >= Count);
    end
  end;

  AAccept := True;
  APoint := ScreenToClient(GetMouseCursorPos);
  if Assigned(OnDragOver) then
    OnDragOver(nil, nil, APoint.X, APoint.Y, dsDragMove, AAccept);
end;

procedure TdxCustomCheckListBox.SetChecked(AIndex: Integer; AValue: Boolean);
begin
  if Checked[AIndex] <> AValue then
  begin
    Items[AIndex].Checked := AValue;
    CheckedChanged(AIndex);
  end;
end;

procedure TdxCustomCheckListBox.SetCheckedIndexes(const Value: TdxIntegerIndexes);
var
  I: Integer;
begin
  BeginUpdate;
  try
    ResetCheckBoxStates;
    for I := 0 to Length(Value) - 1 do
      Checked[Value[I]] := True;
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomCheckListBox.SetState(AIndex: Integer; AValue: TcxCheckBoxState);
begin
  if Items[AIndex].State <> AValue then
  begin
    Items[AIndex].State := AValue;
    InvalidateItem(AIndex);
  end;
end;

procedure TdxCustomCheckListBox.SetMultiSelect(AValue: Boolean);
begin
  if FMultiSelect <> AValue then
  begin
    FMultiSelect := AValue;
    if not FMultiSelect then
    begin
      ClearSelection;
      LayoutChanged;
    end;
  end;
end;

procedure TdxCustomCheckListBox.SetScrollDirection(AValue: TcxDirection);
begin
  if FScrollDirection <> AValue then
  begin
    FreeAndNil(FScrollTimer);
    FScrollDirection := AValue;
    if FScrollDirection <> dirNone then
    begin
      FScrollTimer := TcxTimer.Create(nil);
      FScrollTimer.Interval := dxListBoxScrollTimeInterval;
      FScrollTimer.OnTimer := ScrollTimerHandler;
    end;
  end;
end;

procedure TdxCustomCheckListBox.SetSelected(AIndex: Integer; AValue: Boolean);
var
  AChanged: Boolean;
begin
  AChanged := False;
  if not AValue or MultiSelect then
    DoSelect(AIndex, AValue, AChanged);
  if AChanged then
  begin
    InvalidateItem(AIndex);
    if Assigned(FOnSelectionChanged) then
      FOnSelectionChanged(Self);
  end;
end;

procedure TdxCustomCheckListBox.SetShowCheckBoxes(AValue: Boolean);
begin
  if ShowCheckBoxes <> AValue then
  begin
    FShowCheckBoxes := AValue;
    LayoutChanged;
  end;
end;

procedure TdxCustomCheckListBox.SetVisibleItemCount(AValue: Integer);
begin
  if VisibleItemCount <> AValue then
  begin
    FVisibleItemCount := AValue;
    LayoutChanged;
  end;
end;

procedure TdxCustomCheckListBox.SetVisibleWidth(AValue: Integer);
begin
  if VisibleWidth <> AValue then
  begin
    FVisibleWidth := AValue;
    LayoutChanged;
  end;
end;

{ TdxCustomDropDownInnerListBox }

constructor TdxCustomDropDownInnerListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ItemAutoHeight := True;
  FDefaultItemIndex := -1;
end;

procedure TdxCustomDropDownInnerListBox.AdjustItemFont(
  AFont: TFont; AItem: TdxCustomListBoxItem; AState: TcxButtonState);
begin
  if (DefaultItemIndex > -1) and (DefaultItemIndex = AItem.Index) then
    AFont.Style := AFont.Style + [fsBold];
end;

procedure TdxCustomDropDownInnerListBox.DoSelectItem(ASelectedViaKeyboard: Boolean);
begin
  if Items.IsValidIndex(ItemIndex) then
  begin
    if Assigned(OnSelectItem) then
      OnSelectItem(Self, Items[ItemIndex], ASelectedViaKeyboard);
  end;
end;

procedure TdxCustomDropDownInnerListBox.DrawBackground;
var
  AImagesAreaRect: TRect;
begin
  LookAndFeelPainter.DrawDropDownListBoxBackground(Canvas, ClientRect, BorderSize > 0);
  AImagesAreaRect := ImagesAreaRect;
  if UseRightToLeftAlignment then
    AImagesAreaRect := TdxRightToLeftLayoutConverter.ConvertRect(AImagesAreaRect, ClientBounds);
  LookAndFeelPainter.DrawDropDownListBoxScaledGutterBackground(Canvas, AImagesAreaRect, ScaleFactor);
end;

procedure TdxCustomDropDownInnerListBox.DrawItemBackground(
  const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState);
var
  AImagesAreaRect: TRect;
begin
  if AState = cxbsPressed then
  begin
    AImagesAreaRect := ImagesAreaRect;
    if UseRightToLeftAlignment then
      AImagesAreaRect := TdxRightToLeftLayoutConverter.ConvertRect(AImagesAreaRect,  Bounds);
    LookAndFeelPainter.DrawDropDownListBoxScaledSelection(Canvas, R, AImagesAreaRect, ScaleFactor);
  end;
end;

function TdxCustomDropDownInnerListBox.ProcessNavigationKey(var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := True;
  case Key of
    VK_RETURN:
      DoSelectItem(True);
    else
      Result := inherited ProcessNavigationKey(Key, Shift);
  end;
end;

procedure TdxCustomDropDownInnerListBox.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  ItemIndex := -1;
end;

procedure TdxCustomDropDownInnerListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if not cxPointIsEqual(FPrevMousePosition, Point(X, Y)) then
  begin
    ItemIndex := HotIndex;
    FPrevMousePosition := Point(X, Y);
  end;
end;

procedure TdxCustomDropDownInnerListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  DoSelectItem(False);
end;

function TdxCustomDropDownInnerListBox.GetBorderSize: Integer;
begin
  if BorderStyle = cxcbsNone then
    Result := 0
  else
    Result := LookAndFeelPainter.DropDownListBoxBordersSize;
end;

function TdxCustomDropDownInnerListBox.GetImageOffsets: TRect;
begin
  Result := LookAndFeelPainter.DropDownListBoxScaledItemImageOffsets(ScaleFactor);
end;

function TdxCustomDropDownInnerListBox.GetImageSize: TSize;
begin
  if Images <> nil then
    Result := dxGetImageSize(nil, Images, 0, ScaleFactor)
  else
    Result := cxSize(ScaleFactor.Apply(16));
end;

function TdxCustomDropDownInnerListBox.GetTextColor(
  AItem: TdxCustomListBoxItem; AState: TcxButtonState): TColor;
begin
  Result := Font.Color;
  if Result = clWindowText then
    Result := LookAndFeelPainter.DropDownListBoxItemTextColor(AState = cxbsPressed);
  if Result = clDefault then
    Result := clWindowText;
end;

function TdxCustomDropDownInnerListBox.GetTextOffsets: TRect;
begin
  Result := LookAndFeelPainter.DropDownListBoxScaledItemTextOffsets(ScaleFactor);
end;

procedure TdxCustomDropDownInnerListBox.SetDefaultItemIndex(AValue: Integer);
begin
  if AValue <> FDefaultItemIndex then
  begin
    FDefaultItemIndex := AValue;
    LayoutChanged;
  end;
end;

{ TdxCustomDropDownListBox }

constructor TdxCustomDropDownListBox.Create(AOwnerControl: TWinControl);
begin
  inherited Create(AOwnerControl);
  Adjustable := True;
  Position := poDesigned;
  FDisplayRowsCount := 8;
  FInnerListBox := CreateInnerListBox;
  FInnerListBox.Parent := Self;
  FInnerListBox.Align := alClient;
  FInnerListBox.OnSelectItem := ItemSelected;
end;

constructor TdxCustomDropDownListBox.CreateEx(AOwnerControl: TWinControl;
  AOwnerParent: TcxControl);
var
  AIntf: IcxLookAndFeelContainer;
begin
  Create(AOwnerControl);
  OwnerParent := AOwnerParent;
  if Supports(AOwnerParent, IcxLookAndFeelContainer, AIntf) then
    InnerListBox.LookAndFeel.MasterLookAndFeel := AIntf.GetLookAndFeel;
end;

destructor TdxCustomDropDownListBox.Destroy;
begin
  FreeAndNil(FInnerListBox);
  inherited Destroy;
end;

procedure TdxCustomDropDownListBox.BeginUpdate;
begin
  InnerListBox.BeginUpdate;
end;

procedure TdxCustomDropDownListBox.EndUpdate;
begin
  InnerListBox.EndUpdate;
end;

function TdxCustomDropDownListBox.CreateInnerListBox: TdxCustomDropDownInnerListBox;
begin
  Result := TdxCustomDropDownInnerListBox.Create(nil);
end;

procedure TdxCustomDropDownListBox.AdjustClientRect(var Rect: TRect);
begin
  // nothing to do
end;

function TdxCustomDropDownListBox.CalculatePosition(const ASize: TSize): TPoint;
begin
  if Count > 0 then
    Result := inherited CalculatePosition(ASize)
  else
    Result := Point(cxInvisibleCoordinate, cxInvisibleCoordinate);
end;

function TdxCustomDropDownListBox.CalculateSize: TSize;
begin
  InnerListBox.LayoutChanged;
  Result := InnerListBox.CalculateContentSize(DisplayRowsCount);
  if MaxWidth > 0 then
    Result.cx := Min(Result.cx, ScaleFactor.Apply(MaxWidth));
  Result.cx := Max(Result.cx, ScaleFactor.Apply(MinWidth));
end;

procedure TdxCustomDropDownListBox.CloseUp(AClosedViaKeyboard: Boolean);
begin
  FClosedViaKeyboard := AClosedViaKeyboard;
  inherited CloseUp;
end;

procedure TdxCustomDropDownListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if IsWinXPOrLater then
  begin
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
    Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST;
  end;
end;

procedure TdxCustomDropDownListBox.DoCloseUp(AClosedViaKeyboard: Boolean);
begin
  if Assigned(OnCloseUp) then
    OnCloseUp(Self, AClosedViaKeyboard);
end;

procedure TdxCustomDropDownListBox.DoHide;
begin
  inherited DoHide;
  DoCloseUp(FClosedViaKeyboard);
end;

procedure TdxCustomDropDownListBox.DoSelectItem(
  AItem: TdxCustomListBoxItem; ASelectedViaKeyboard: Boolean);
begin
  if Assigned(OnSelect) then OnSelect(Self, AItem, ASelectedViaKeyboard);
end;

function TdxCustomDropDownListBox.GetCount: Integer;
begin
  Result := InnerListBox.Count;
end;

function TdxCustomDropDownListBox.GetImages: TCustomImageList;
begin
  Result := InnerListBox.Images;
end;

function TdxCustomDropDownListBox.GetItemHeight: Integer;
begin
  Result := InnerListBox.ItemHeight;
end;

function TdxCustomDropDownListBox.GetItemIndex: Integer;
begin
  Result := InnerListBox.ItemIndex;
end;

function TdxCustomDropDownListBox.GetItems: TdxCustomListBoxItems;
begin
  Result := InnerListBox.Items;
end;

procedure TdxCustomDropDownListBox.InitInnerListBox;
begin
  InnerListBox.ShowHint := ShowHint;
  if OwnerControl <> nil then
    InnerListBox.Font := TWinControlAccess(OwnerControl).Font;
end;

procedure TdxCustomDropDownListBox.InitPopup;
begin
  inherited InitPopup;
  InitInnerListBox;
end;

procedure TdxCustomDropDownListBox.ItemSelected(Sender: TObject;
  AItem: TdxCustomListBoxItem; ASelectedViaKeyboard: Boolean);
var
  ALink: TcxObjectLink;
begin
  ALink := cxAddObjectLink(Self);
  try
    try
      DoSelectItem(AItem, ASelectedViaKeyboard);
    finally
      if ALink.Ref <> nil then
        CloseUp(ASelectedViaKeyboard);
    end;
  finally
    cxRemoveObjectLink(ALink);
  end;
end;

function TdxCustomDropDownListBox.IsShortCut(var Message: TWMKey): Boolean;
begin
  Result := inherited IsShortCut(Message);
  if Message.CharCode = VK_ESCAPE then
  begin
    Result := True;
    CloseUp(True);
  end;
end;

procedure TdxCustomDropDownListBox.Popup;
begin
  SetBounds(Left, Top, 0, 0);
  FClosedViaKeyboard := False;
  inherited Popup(FInnerListBox);
end;

procedure TdxCustomDropDownListBox.PopupForBounds(const ABounds: TRect; AActivateKey: Char = #0);
begin
  if InnerListBox.CanStartIncSearch(AActivateKey) then
  begin
    ItemIndex := -1;
    InnerListBox.DoIncrementalSearch(AActivateKey);
  end;
  OwnerBounds := ABounds;
  Popup;
end;

procedure TdxCustomDropDownListBox.SetImages(const Value: TCustomImageList);
begin
  InnerListBox.Images := Value;
end;

procedure TdxCustomDropDownListBox.SetItemHeight(const Value: Integer);
begin
  InnerListBox.ItemHeight := Value;
end;

procedure TdxCustomDropDownListBox.SetItemIndex(const Value: Integer);
begin
  InnerListBox.ItemIndex := Value;
end;

{ TdxQuickCustomizationCustomCommand }

constructor TdxQuickCustomizationCustomCommand.Create(const ACaption: string; AHasCheckBox: Boolean);
begin
  FCaption := ACaption;
  FHasCheckBox := AHasCheckBox;
end;

procedure TdxQuickCustomizationCustomCommand.Execute;
begin
  if Assigned(FOnAction) then
    FOnAction(Self);
end;

procedure TdxQuickCustomizationCustomCommand.SetChecked(AValue: Boolean);
begin
  if HasCheckBox then
    FChecked := AValue
  else
    FChecked := False;
end;

{ TdxQuickCustomizationCustomCheckListBox }

constructor TdxQuickCustomizationCustomCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := cxcbsNone;
  LookAndFeel.MasterLookAndFeel := QuickCustomizationControl.LookAndFeel;
  Parent := QuickCustomizationControl;
end;

function TdxQuickCustomizationCustomCheckListBox.DoIncrementalSearch(var Key: Char): Boolean;
begin
  Result := inherited DoIncrementalSearch(Key);
  if not Result and (SearchText <> '') then
    QuickCustomizationControl.Controller.DoIncrementalSearchOnOtherListBox(Self, SearchText);
end;

function TdxQuickCustomizationCustomCheckListBox.GetQuickCustomizationControl: TdxQuickCustomizationCustomControl;
begin
  Result := Owner as TdxQuickCustomizationCustomControl;
end;

function TdxQuickCustomizationCustomCheckListBox.GetTextOffsets: TRect;
begin
  Result := LookAndFeelPainter.DropDownListBoxScaledItemTextOffsets(ScaleFactor);
end;

function TdxQuickCustomizationCustomCheckListBox.NeedHandleClick: Boolean;
begin
  Result := inherited NeedHandleClick and (not HasCheckBox(ItemIndex) or HitAtItemCheckBox(ItemIndex));
end;

function TdxQuickCustomizationCustomCheckListBox.ProcessNavigationKey(var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := GetQuickCustomizationControl.Controller.ProcessNavigationKey(Self, Key, Shift) or
    inherited ProcessNavigationKey(Key, Shift);
end;

procedure TdxQuickCustomizationCustomCheckListBox.SetItemIndex(AIndex: Integer);
var
  APrevIndex: Integer;
begin
  APrevIndex := ItemIndex;
  inherited SetItemIndex(AIndex);
  if APrevIndex <> AIndex then
    QuickCustomizationControl.Controller.HandleItemIndexChanging(Self, AIndex);
end;

{ TdxQuickCustomizationCommandListBox }

procedure TdxQuickCustomizationCommandListBox.CheckedChanged(AItemIndex: Integer);
begin
  inherited CheckedChanged(AItemIndex);
  Command[AItemIndex].Checked := Checked[AItemIndex];
end;

procedure TdxQuickCustomizationCommandListBox.DoItemAction(AItemIndex: Integer);
begin
  inherited DoItemAction(AItemIndex);
  Command[AItemIndex].Execute;
end;

function TdxQuickCustomizationCommandListBox.HasCheckBox(AItemIndex: Integer): Boolean;
begin
  Result := Command[AItemIndex].HasCheckBox;
end;

function TdxQuickCustomizationCommandListBox.GetCommand(AItemIndex: Integer): TdxQuickCustomizationCustomCommand;
begin
  Result := TdxQuickCustomizationCustomCommand(Items[AItemIndex].Data);
end;

{ TdxQuickCustomizationCheckListBox }

constructor TdxQuickCustomizationCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MultiSelect := True;
  DragMode := dmAutomatic;
end;

procedure TdxQuickCustomizationCheckListBox.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  FDblClickHandling := True;
  inherited;
  FDblClickHandling := False;
end;

procedure TdxQuickCustomizationCheckListBox.InternalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  APrevIndex, APrevCount: Integer;
begin
  if FDblClickHandling then
    Exit;
  APrevIndex := ItemIndex;
  APrevCount := Count;
  ItemIndex := ItemAtPos(Point(X, Y), True);
  FAnchorIndex := ItemIndex;
  if HitAtItemCheckBox(ItemIndex) then
    SelectFocusedItem
  else
    if NeedBeginDrag(Button, Shift) then
    begin
      if not Selected[ItemIndex] then
        SelectFocusedItem;
      DragAndDropState := ddsStarting;
      BeginDrag(False);
    end
    else
    if MultiSelect and (APrevCount = Count) then
    begin
      SelectItemsViaMouseDown(APrevIndex, Shift);
      FSelectedViaMouseMovePriorIndex := FAnchorIndex;
      FInMouseSelectionProcess := True;
    end;
end;

procedure TdxQuickCustomizationCheckListBox.DoMultiSelectionAction;
var
  I: Integer;
  AValue: Boolean;
begin
  if not Items.IsValidIndex(ItemIndex) then
    Exit;
  ShowHourglassCursor;
  try
    CallNotify(OnSelectedItemCheckedStateChanging, Self);
    BeginUpdate;
    try
      AValue := not Checked[ItemIndex];
      for I := 0 to Selection.Count - 1 do
        Checked[TdxNativeUInt(Selection[I])] := AValue;
    finally
      CallNotify(OnSelectedItemCheckedStateChanged, Self);
      EndUpdate;
    end;
  finally
    HideHourglassCursor;
  end;
end;

{ TdxQuickCustomizationController }

constructor TdxQuickCustomizationController.Create(AOwner: TdxQuickCustomizationCustomControl);
begin
  FOwner := AOwner;
end;

function TdxQuickCustomizationController.DoIncrementalSearchOnOtherListBox(Sender: TdxCustomCheckListBox;
  const ASearchText: string): Boolean;
var
  AListBox: TdxCustomCheckListBox;
begin
  if Sender = CommandListBox then
    AListBox := CheckListBox
  else
    AListBox := CommandListBox;
  AListBox.SearchText := ASearchText;
  AListBox.FStartIncrementalSearch := Sender.FStartIncrementalSearch;
  Result := AListBox.FocusNextItemWithText(ASearchText);
  if Result then
  begin
    AListBox.SetFocus;
    if AListBox.MultiSelect  then
      AListBox.SelectFocusedItem;
  end;
end;

procedure TdxQuickCustomizationController.DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

  procedure HandleKey(AListBox: TdxCustomListBox);
  var
    ACharKey: Char;
  begin
    AListBox.SetFocus;
    AListBox.KeyDown(Key, Shift);
    if Key <> 0 then
    begin
      ACharKey := Chr(Key);
      AListBox.KeyPress(ACharKey);
    end;
  end;

begin
  if CommandListBox.ItemIndex >= 0 then
    HandleKey(CommandListBox)
  else
    if CheckListBox.ItemIndex >= 0 then
      HandleKey(CheckListBox);
end;

function TdxQuickCustomizationController.GetCheckListBox: TdxCustomCheckListBox;
begin
  Result := Owner.CheckListBox;
end;

function TdxQuickCustomizationController.GetCommandListBox: TdxCustomCheckListBox;
begin
  Result := Owner.CommandListBox;
end;

procedure TdxQuickCustomizationController.HandleCommandListBoxMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  Message: TWMMouseWheel;
begin
  Handled := True;
  Message.Msg := WM_MOUSEWHEEL;
  Message.Keys := ShiftStateToKeys(Shift);
  Message.WheelDelta := WheelDelta;
  Message.Pos := PointToSmallPoint(MousePos);
  Inc(Message.YPos, CheckListBox.Top);
  CheckListBox.SetFocus;
  CheckListBox.DefaultHandler(Message);
end;

procedure TdxQuickCustomizationController.HandleItemIndexChanging(Sender: TdxCustomListBox; AItemIndex: Integer);
begin
  if AItemIndex <> -1 then
  begin
    if Sender = CommandListBox then
      CheckListBox.ItemIndex := -1;
    if Sender = CheckListBox then
      CommandListBox.ItemIndex := -1;
  end
  else
    if Sender = CheckListBox then
      CheckListBox.ClearSelection;
end;

function TdxQuickCustomizationController.ProcessCheckListBoxNavigationKey(var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := Key in [VK_PRIOR, VK_HOME, VK_UP];
  if not(Result and Owner.HasCommands) then
  begin
    Result := False;
    Exit;
  end;
  if Key = VK_HOME then
  begin
    CommandListBox.SetFocus;
    CommandListBox.ItemIndex := 0;
    CheckListBox.TopIndex := 0;
  end
  else
    if (CheckListBox.ItemIndex = 0) and (Key in [VK_PRIOR, VK_UP]) then
    begin
      CommandListBox.SetFocus;
      if Key = VK_UP then
        CommandListBox.ItemIndex := CommandListBox.Count - 1
      else
      begin
        CommandListBox.ItemIndex := 0;
        CheckListBox.TopIndex := 0;
      end;
    end
    else
      Result := False;
  if Result then
  begin
    CheckListBox.ItemIndex := -1;
    CheckListBox.ClearSelection;
  end;
end;

function TdxQuickCustomizationController.ProcessCommandListBoxNavigationKey(var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := True;
  case Key of
    VK_NEXT:
      if CommandListBox.ItemIndex = CommandListBox.Count - 1 then
      begin
        CheckListBox.SetFocus;
        SendMessage(CheckListBox.Handle, WM_KEYDOWN, VK_NEXT, 0);
        if ssShift in Shift then
          CheckListBox.SelectItems(0, CheckListBox.ItemIndex, True);
      end
      else
        Result := False;
    VK_END:
      begin
        CheckListBox.SetFocus;
        CheckListBox.ItemIndex := CheckListBox.Count - 1;
        if ssShift in Shift then
          CheckListBox.SelectAll
        else
          CheckListBox.SelectFocusedItem;
      end;
    VK_DOWN:
      if CommandListBox.ItemIndex = CommandListBox.Count - 1 then
      begin
        CheckListBox.SetFocus;
        CheckListBox.ItemIndex := 0;
        CheckListBox.SelectFocusedItem;
      end
      else
        Result := False;
  else
    Result := False;
  end;
  if Result then
    CommandListBox.ItemIndex := -1;
end;

function TdxQuickCustomizationController.ProcessNavigationKey(Sender: TdxCustomListBox;
  var Key: Word; Shift: TShiftState): Boolean;
begin
  if Key = VK_ESCAPE then
  begin
    Owner.KeyDown(Key, Shift);
    Result := Key = 0;
    if Result then
      Exit;
  end;
  Result := ((Sender = CheckListBox) and ProcessCheckListBoxNavigationKey(Key, Shift)) or
    ((Sender = CommandListBox) and ProcessCommandListBoxNavigationKey(Key, Shift));
  if Result then
  begin
    Sender.ClearIncrementalSearch;
    Key := 0;
  end;
end;

{ TdxQuickCustomizationCustomControl }

constructor TdxQuickCustomizationCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FController := TdxQuickCustomizationController.Create(Self);
  FCheckListBox := TdxQuickCustomizationCheckListBox.Create(Self);
  FCommandListBox := GetCommandListBoxClass.Create(Self);
  FCommandList := TcxObjectList.Create;

  FCommandListBox.OnMouseWheel := Controller.HandleCommandListBoxMouseWheel;
  OnMouseWheel := Controller.HandleCommandListBoxMouseWheel;
  OnKeyUp := Controller.DoKeyUp;
end;

destructor TdxQuickCustomizationCustomControl.Destroy;
begin
  FController.Free;
  FCommandList.Free;
  FCommandListBox.Free;
  FCheckListBox.Free;
  inherited Destroy;
end;

function TdxQuickCustomizationCustomControl.AddCommand(const ACaption: string;
  AHasCheckBox: Boolean; AOnAction: TNotifyEvent): TdxQuickCustomizationCustomCommand;
begin
  Result := TdxQuickCustomizationCustomCommand.Create(ACaption, AHasCheckBox);
  Result.OnAction := AOnAction;
  CommandListBox.AddItem(ACaption, Result);
  FCommandList.Add(Result);
end;

procedure TdxQuickCustomizationCustomControl.AdjustBounds(const AUseCachedTextMaxWidth: Boolean = False);
var
  AWidth1, AWidth2, AHeight1, AHeight2: Integer;
  ATop2: Integer;
begin
  AHeight1 := CommandListBox.CalculateAutoHeight;
  AWidth1 := CommandListBox.CalculateAutoWidth(AUseCachedTextMaxWidth);
  ATop2 := AHeight1;
  if CommandListBox.Items.Count > 0 then
    Inc(ATop2, GetSeparatorHeight);

  CheckListBox.VisibleItemCount := CheckListBoxVisibleRowCount;

  AHeight2 := CheckListBox.CalculateAutoHeight;
  CheckListBox.Height := AHeight2;
  AWidth2 := CheckListBox.CalculateAutoWidth(AUseCachedTextMaxWidth);

  SetBounds(Left, Top, Max(AWidth1, AWidth2), ATop2 + AHeight2);

  CommandListBox.SetBounds(0, 0, Width, AHeight1);
  CheckListBox.SetBounds(0, ATop2, Width, AHeight2);
end;

procedure TdxQuickCustomizationCustomControl.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  AdjustBounds;
end;

procedure TdxQuickCustomizationCustomControl.Clear;
begin
  FCommandList.Clear;
  CommandListBox.Clear;
  CheckListBox.Selection.Clear;
  CheckListBox.Clear;
end;

procedure TdxQuickCustomizationCustomControl.DrawSeparator;
var
  R: TRect;
begin
  if HasCommands then
  begin
    R := ClientRect;
    R.Top := CommandListBox.Height;
    R.Bottom := CheckListBox.Top;
    R := cxRectInflate(R, -ScaleFactor.Apply(2 * cxTextOffset), 0);
    LookAndFeelPainter.DrawSeparator(Canvas, R, False);
  end;
end;

function TdxQuickCustomizationCustomControl.GetCommandListBoxClass: TdxQuickCustomizationCustomCheckListBoxClass;
begin
  Result := TdxQuickCustomizationCommandListBox;
end;

function TdxQuickCustomizationCustomControl.GetSeparatorHeight: Integer;
begin
  Result := ScaleFactor.Apply(5);
end;

function TdxQuickCustomizationCustomControl.HasCommands: Boolean;
begin
  Result := CommandListBox.Count > 0;
end;

procedure TdxQuickCustomizationCustomControl.Initialize(AParent: TWinControl);
begin
  PopulateCheckListBox;
  PopulateCommandListBox;
  Parent := AParent;
  RecreateWnd;
  HandleNeeded;
  AdjustBounds;

  if HasCommands and (CheckListBox.GetSelectionCount = 0) then
    CommandListBox.ItemIndex := 0
  else
    if CheckListBox.GetSelectionCount > 0 then
      CheckListBox.ItemIndex := TdxNativeUInt(CheckListBox.Selection[0])
    else
      if CheckListBox.Count > 0 then
      begin
        CheckListBox.ItemIndex := 0;
        CheckListBox.SelectFocusedItem;
      end;

  CallNotify(OnInitialize, Self);
end;

procedure TdxQuickCustomizationCustomControl.Paint;
begin
  inherited Paint;
  DrawSeparator;
end;

procedure TdxQuickCustomizationCustomControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if not IsScaleChanging then
    inherited;
end;

initialization
  RegisterClasses([TdxCustomListBox]);

finalization
  UnregisterClasses([TdxCustomListBox]);

end.

