{*************************************************************************}
{ TCustomItemsContainer                                                   }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2010 - 2015                                      }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit CustomItemsContainer;

interface

{$I TMSDEFS.INC}

uses
{$IFDEF DELPHI2006_LVL}
  uxTheme,
{$ENDIF}
  Windows, Forms, Graphics, Classes, Controls, Messages,
  GDIPFill, GDIPBase, AdvGDIP, AdvStyleIF, SysUtils,
  Dialogs, GDIPCustomItem, StdCtrls, Math, Themes,
  GDIPPictureContainer, ImgList, ExtCtrls, Types, Contnrs;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 6; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 3; // Build nr.

  s_Edit = 'Edit';

  // v0.9.0.0 : First Beta Release
  // v0.9.0.1 : Fixed : Issue with Stay on top items editor in older Delphi versions
  // v0.9.5.0 : Second Beta Release with new AdvPolyPager control
  // v0.9.5.5 : Significant fixes and improvements in TAdvPolyPager / CustomEditor / Section items
  // v0.9.6.0 : New : Transparency in different poly lists
  //          : New : Introducing Header Item, Splitter Item, Full DropDown Item, Large Buttoned HTML Item, Buttonbar Item with elements.
  //          : New : Integrated object inspector imagename property editor connected with GDIPPictureContainer.
  //          : New : lists with custom list bullets in the GDI+ HTML Engine
  //          : Improved : keyboard support in dropdown items
  //          : Improved : Custom drawing for Check Item, Radio Item and Button Item
  // v0.9.7.0 : New : PreviewItem for display shell preview thumbnails similar to Windows Vista and Windows 7
  //            for Delphi 2010 only
  //          : Smaller improvements in editor, lists and items
  // v0.9.7.1 : Fixed : Issue with dropdown indexes
  // v1.0.0.0 : First Release
  // v1.0.1.0 : New : ItemCount property on container level
  //          : Fixed : Issue with Default values
  // v1.1.0.0 : New : Shortcut support
  //          : Improved : Keyboard handling
  //          : Fixed : Issue with item interaction when deleting items during interaction
  //          : Fixed : Issue with imagelist and picturecontainer when destroying component
  //          : Improved : Transparent bitmap drawing
  // v1.1.0.1 : Fixed : Issue with selecting items/pages at runtime in TAdvPolyPager
  //          : Improved : Added UnSelectItem procedure
  // v1.1.0.2 : New : Expander left and top properties in GDIPExpandableImageSectionItem
  //          : Fixed : Issue with selecting items/pages at designtime in TAdvPolyPager
  //          : Improved: DropDownHeight and DropDownWidth in GDIPDropDownItem and GDIPFullDropDownItem
  //          : Improved : Drag & Drop in the Custom items editor
  // v1.1.0.3 : Fixed : Issue with initializing default values
  //          : Fixed : Repaint issue in AdvPolyPager
  //          : Fixed : Issue in Editor with default values
  //          : Improved : Drag & Drop at designtime
  // v1.1.0.4 : Repaint issue in Editor
  // v1.1.0.5 : Issue with status indicator position in wedge item
  // v1.1.0.6 : Fixed : Issue with parent in fulldropdownitem
  // v1.1.1.0 : New : Delphi and C++Builder XE Support
  //          : Fixed : Issue with parent in fulldropdownitem
  // v1.1.2.0 : Fixed : Issue with selecting page at designtime
  //          : Fixed : Issue with collapse expand group item
  //          : Fixed : Issue with setting item with control visible false
  //          : Fixed : Issue with scrollbar when setting items visible false
  //          : Improved : Small improvements in default values and designtime values of items
  //          : Improved : Selecting page at structure window
  // v1.2.0.0 : New : ActionList support, Hotkey support
  //          : New : TextRendering property
  //          : Fixed : Issue number of items and dropdown in code in dropdown items
  //          : Fixed : Issue with poly pager OnChanging event AllowChange := False;
  // v1.2.0.1 : Fixed : Issue with status position
  // v1.2.0.2 : Fixed : Issue with Vertical and horizontal Scroll position when resizing
  // v1.2.0.3 : Fixed : Issue with drawing vertical and horizontal drag line
  // v1.2.0.4 : Improved : Behavior with selection / deselection of items within pager with keyboard and mouse
  // v1.2.0.5 : Fixed : Issue with Click and button click event in ImageTextButtonItem
  // v1.2.0.6 : Fixed : Issue with OnItemDeselect in AdvPolyPager
  //          : Fixed : Issue with mousedown / mouseup
  // v1.2.1.0 : New : btPushButton type in Button type items
  //          : New : CheckFullArea for check / radio type items to allow full area checking
  //          : Fixed : Issue with deselecting items outside visible area
  //          : Fixed : Issue with default values fill
  //          : Fixed : Default caption values
  // v1.2.2.0 : New : ListWidth property in advpolypager
  //          : Fixed : Issue with image button up and down in pushbutton type
  //          : Fixed : Issue with checked onbuttonclick called in checked setter
  // v1.2.2.1 : Fixed : Issue in HTML rendering
  // v1.2.2.2 : Improved : Width and Height of Container changeable.
  //          : Fixed : Issue with exposing OnVerticalScroll & OnHorizontalScroll events
  //          : Fixed : Issue with image button item button position
  //          : Fixed : Issue with status position in full dropdown item
  //          : Fixed : Issue with status default offset left and top position
  // v1.2.2.3 : Fixed : Item deselect issue in TAdvPolyPager
  //          : Fixed : Issue with FullDropDownitem itemindex at designtime
  // v1.2.3.0 : New : OnItemAppearance event which enables you to customize the appearance per item
  // v1.2.3.1 : Fixed : Issue with hint on selected item
  //          : Fixed : Issue with fulldropdownitem visibility
  // v1.2.3.2 : Fixed : Issue with fulldropdownitem width and height after selection of list item
  // v1.2.3.3 : Fixed : Issue with selecting invisible item at designtime
  // v1.2.3.4 : Fixed : Issue with scrollbars in fulldropdownitem
  //          : Fixed : Issue with drawing images in GDIPFill
  // v1.2.3.5 : Fixed : Issue with dragdrop cursor in double-click state.
  // v1.3.0.0 : New : Metro Style support
  //          : Fixed : Small Issue with ImageTextButtonItem
  // v1.3.0.1 : Fixed : Issue with tabbing in controls and scrolling
  //          : Improved : Metro style issues
  // v1.3.1.0 : New : Event OnItemReorder
  // v1.3.1.1 : Fixed : Issue with mouse wheel scrolling
  // v1.3.1.1 : Fixed : Issue with OnItemReorder event not published
  // v1.3.1.3 : Fixed : Issue with scrolling and control repositioning
  // v1.3.1.4 : Fixed : Issue with selection / keyboard selection of items
  // v1.4.0.0 : New : XE3 support
  //          : New : CaptionFontStyle and DescriptionFontStyle properties added
  //          : Fixed : Issue with splitteritem not saving values due to default property values           
  // v1.4.0.1 : Fixed : Issue with checked state for radio button
  // v1.4.1.0 : New : Copy button in Custom Editor
  // v1.4.1.1 : Fixed : Issue with Caption / Description floating point error
  // v1.4.2.0 : New : ShowDesignTimeMessage property
  // v1.4.2.1 : Improved : Right-click on list does not trigger OnItemClick
  //          : Improved : Added OnListItemReorder in AdvPolyPager
  // v1.4.3.0 : New : Event OnItemRightClick added to the polylist item
  //          : New : Enter key used to trigger click events
  // v1.4.3.1 : Fixed : Issue with drag-drop insertion point
  // v1.5.0.0 : New : Windows 8, Office 2013 styles added
  // v1.6.0.0 : New : Hint and Visible property for each bar button element
  //          : New : ColumnSpan property for each item
  //          : Improved : border drawing
  //          : Fixed : Issue with ImageTextButtonItem text spacing
  //          : Fixed : Issue with DblClick
  // v1.6.0.1 : Fixed : Issue with text position in base item
  //          : Fixed : Issue with Strikeout font style not applied
  // v1.6.0.2 : Improved : Office Styles in AdvPolyPager
  // v1.6.0.3 : Fixed : Issue with caption alignment in items
  // v1.6.0.4 : Fixed : Issue with setting focus on ItemsContainer that became invisible programmatically
  // v1.6.0.5 : Improved: Horizontal and Vertical scrollbar adopt VCL styles
  // v1.6.2.0 : Improved : Turn off HTML caching with the HTMLCache property to enable smoother text-rendering
  // v1.6.2.1 : Improved : Office 2013 styles
  //          : Fixed : flickering in combination with VCL styles
  // v1.6.2.2 : Improved : Button colors in Office 2013 styles
  //          : Improved : Styles in TProgressItem
  // v1.6.2.3 : Fixed : Issue with key loop after showing message and pressing the enter key to hide it.


type
  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TListBorderType = (btNormalLeft, btNormalTop, btNormalRight, btNormalBottom,
    btGradientLeft, btGradientTop, btGradientRight, btGradientBottom);

  TListBorderTypes = set of TListBorderType;

  TListBorderMode = (bmNormal, bmTransition);

  TAutoSizeMode = (asmControl, asmItems, asmNone);

  TAutoSizeType = (astWidth, astHeight, astBoth);

  TListBorderTransition = (btLeft, btTop, btRight, btBottom);

  TListBorderTransitions = set of TListBorderTransition;

  TContainerConnection = (ccLeft, ccTop, ccRight, ccBottom);

  TContainerMode = (cmNoBorder, cmNormalBorder, cmGradientBorder);

  TDrawEvent = procedure(Sender: TObject; AGraphics: TGPGraphics;
    ARect: TGPRectF) of object;

  TScrollEvent = procedure(Sender: TObject; ScrollPosition: integer) of object;

  TExpandEvent = procedure(Sender: TObject; Item: TCustomItem;
    Expand: Boolean) of object;

  TCustomItemsContainer = class;

  TActionCustomItem = class(TCustomItem);

  TScrollContainer = class(TScrollingWinControl)
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    procedure PaintWindow(DC: HDC); override;
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  THandleAppearance = class(TPersistent)
  private
    FOwner: TCustomItemsContainer;
    FOpacity: Byte;
    FBorderColor: TColor;
    FColor: TColor;
    FOnChange: TNotifyEvent;
    FSize: integer;
    FArrowColor: TColor;
    procedure SetBorderColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetOpacity(const Value: Byte);
    procedure SetSize(const Value: integer);
    procedure SetArrowColor(const Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TCustomItemsContainer);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clSilver;
    property BorderColor
      : TColor read FBorderColor write SetBorderColor default clSilver;
    property ArrowColor
      : TColor read FArrowColor write SetArrowColor default clBlack;
    property Opacity: Byte read FOpacity write SetOpacity default 200;
    property Size: integer read FSize write SetSize default 35;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TScrollType = (stScrollers, stHandles);

  TCustomItemsContainer = class(TScrollContainer, ITMSStyle, IGDIPBase,
    IPictureContainerSupport, IGDIPAnchor, IGDIPExpand, IWinStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FDblClick, FFocusKilled: Boolean;
    FMetroStyle: Boolean;
    FShortCutChars: String;
    FShortCutItemHintShowing: Boolean;
    FScrollTimer: TTimer;
    keepoldcursor: Boolean;
    FoldCursor: TCursor;
    FHandleTimer: TTimer;
    FTimeDownOnArrow: integer;
    HorzScrollVis, VertScrollVis: Boolean;
    FHandleHorzLeftDown, FHandleHorzLeftHover, FHandleHorzRightDown,
      FHandleHorzRightHover, FHandleVertTopDown, FHandleVertTopHover,
      FHandleVertBottomDown, FHandleVertBottomHover: Boolean;
    FDrawDragLine: Boolean;
    FDropItem: TCustomItem;
    FOldScrollHPos, FOldScrollVPos: integer;
    FMouseDown: Boolean;
    FDragging: Boolean;
    FMouseDownOnItemX: integer;
    FMouseDownOnItemY: integer;
    FMouseDownOnItem: Boolean;
    FDragDropItem: TCustomItem;
    FListUpdating, FPosListUpdating: Boolean;
    FUpdateCount: integer;
    FList: TCustomBaseList;
    FFill: TGDIPFill;
    FListWidth: integer;
    FReadOnly: Boolean;
    FListMargins: TMargins;
    FOnChange: TNotifyEvent;
    FMinimumSize: integer;
    FRows: integer;
    FColumns: integer;
    FVerticalSpacing: integer;
    FHorizontalSpacing: integer;
    FAutoSizeMode: TAutoSizeMode;
    FAutoSizeType: TAutoSizeType;
    FShowFocus: Boolean;
    FBorderMode: TListBorderMode;
    FBorderTypes: TListBorderTypes;
    FOnStartDraw: TDrawEvent;
    FOnEndDraw: TDrawEvent;
    FOnItemSelect: TItemSelectEvent;
{$IFNDEF DELPHI2006_LVL}
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
{$ENDIF}
    FOnItemCompare: TItemCompareEvent;
    FOnInternalChange: TNotifyEvent;
    FReorder: Boolean;
    FHandleAppearance: THandleAppearance;
    FScrollType: TScrollType;
    FHorzScrollPos: integer;
    FVertScrollPos: integer;
    FThumbTracking: Boolean;
    FBorderStyle: TBorderStyle;
    FOnVerticalScroll: TScrollEvent;
    FOnHorizontalScroll: TScrollEvent;
    FOnItemDeSelect: TItemSelectEvent;
    FOnInternalExpand: TExpandEvent;
    FDragLine: Boolean;
    FDragLineColor: TColor;
    FTransparent: Boolean;
    FIsAppMenu: Boolean;
    FIsMainMenu: Boolean;
    FTextRenderingHint: TTextRenderingHint;
    FOnItemAppearance: TItemAppearanceEvent;
    FOnItemReorder: TItemReorderEvent;
    FShowDesignTimeMessage: Boolean;
    FHTMLCache: Boolean;
    FDidScale: Boolean;
    procedure SetList(const Value: TCustomBaseList);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetReadOnly(const Value: Boolean);
    function GetItem(Index: integer): TCustomItem;
    procedure SetItem(Index: integer; const Value: TCustomItem);
    procedure SetListMargins(const Value: TMargins);
    procedure SetColumns(const Value: integer);
    procedure SetRows(const Value: integer);
    procedure SetHorizontalSpacing(const Value: integer);
    procedure SetVerticalSpacing(const Value: integer);
    procedure SetAutoSizeMode(const Value: TAutoSizeMode);
    procedure SetAutoSizeType(const Value: TAutoSizeType);
    procedure SetShowFocus(const Value: Boolean);
    procedure SetBorderMode(const Value: TListBorderMode);
    procedure SetBorderTypes(const Value: TListBorderTypes);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMDrag(var Message: TCMDrag); message CM_DRAG;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    function GetImageList: TCustomImageList;
    function GetPictureContainer: TGDIPPictureContainer;
    procedure SetImageList(const Value: TCustomImageList);
    procedure SetPictureContainer(const Value: TGDIPPictureContainer);
    procedure Expand(Item: TCustomItem; Expand: Boolean);
    function GetVersion: String;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure SetReadorder(const Value: Boolean);
    procedure SetHandleAppearance(const Value: THandleAppearance);
    procedure SetScrollType(const Value: TScrollType);
    procedure SetHorzScrollPos(const Value: integer);
    procedure SetVertScrollPos(const Value: integer);
    function GetCursorEx: TCursor;
    procedure SetCursorEx(const Value: TCursor);
    procedure SetThumbTracking(const Value: Boolean);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure Scrolling(Sender: TObject);
    procedure SetDragLine(const Value: Boolean);
    procedure SetDragLineColor(const Value: TColor);
    procedure SetTransparent(const Value: Boolean);
    procedure SetIsMainMenu(const Value: Boolean);
    procedure SetTextRenderingHint(const Value: TTextRenderingHint);
    procedure SetShowDesignTimeMessage(const Value: Boolean);
    procedure SetHTMLCache(const Value: Boolean);
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    function IsMetroStyle: Boolean;
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    property HorzScrollPos: integer read FHorzScrollPos write SetHorzScrollPos;
    property VertScrollPos: integer read FVertScrollPos write SetVertScrollPos;
    procedure DragOver(Source: TObject; X, Y: integer; State: TDragState;
      var Accept: Boolean); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation);
      override;
    procedure UpdateControl;
    procedure UpdateScrollBars;
    procedure UpdateList;
    procedure UpdateItemPositions;
    procedure UpdateItems;
    procedure Changed;
    procedure RefreshList;
    procedure ListRefresh(Sender: TObject);
    procedure ListChanged(Sender: TObject);
    procedure ListItemCompare(Sender: TObject; Item1, Item2: TCustomItem;
      var Result: integer);
    procedure Anchor(Anchor: String);
    function GetHandleHorzLeft: TGPRectF;
    function GetHandleHorzRight: TGPRectF;
    function GetHandleVertTop: TGPRectF;
    function GetHandleVertBottom: TGPRectF;
    function IsHandleHorzLeft: Boolean;
    function IsHandleHorzRight: Boolean;
    function IsHandleVertTop: Boolean;
    function IsHandleVertBottom: Boolean;
    procedure ListItemSelect(Sender: TObject; Item: TCustomItem;
      var Allow: Boolean);
    procedure ListItemAppearance(Sender: TObject; Item: TCustomItem; Appearance: TItemAppearance);
    procedure ListItemDeSelect(Sender: TObject; Item: TCustomItem;
      var Allow: Boolean);
    procedure FillChanged(Sender: TObject);
    procedure HandleChanged(Sender: TObject);
    procedure ListMarginsChanged(Sender: TObject);
    procedure HandleTimerChanged(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Resize; override;
    procedure ReadItemState(Reader: TReader; Item: TCustomItem);
    procedure SetItemParentComponent(AParent: TComponent; Item: TCustomItem);
    procedure FillPictureNames(Proc: TGetStrProc);
    procedure GetPictures(APictureList: TPictureContainerList);
    function GetTotalRowSize: integer;
    function GetTotalColumnSize: integer;
    function GetMaxColumnBoxItem: TCustomItem;
    function GetShadowOffset: integer;
    function GetVersionNr: integer;
    property AutoSizeMode: TAutoSizeMode read FAutoSizeMode write SetAutoSizeMode default asmItems;
    property AutoSizeType: TAutoSizeType read FAutoSizeType write SetAutoSizeType default astWidth;
    property List: TCustomBaseList read FList write SetList;
    property Fill: TGDIPFill read FFill write SetFill;
    property HorizontalSpacing: integer read FHorizontalSpacing write SetHorizontalSpacing default 5;
    property VerticalSpacing: integer read FVerticalSpacing write SetVerticalSpacing default 5;
    property ListMargins: TMargins read FListMargins write SetListMargins;
    property Reorder: Boolean read FReorder write SetReadorder default true;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default false;
    property Columns: integer read FColumns write SetColumns default 1;
    property Rows: integer read FRows write SetRows default 0;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default true;
    property BorderMode : TListBorderMode read FBorderMode write SetBorderMode default bmNormal;
    property BorderTypes: TListBorderTypes read FBorderTypes write SetBorderTypes
      default[btNormalLeft, btNormalTop, btNormalRight, btNormalBottom];
    property Version: String read GetVersion;
    property OnStartDraw: TDrawEvent read FOnStartDraw write FOnStartDraw;
    property OnEndDraw: TDrawEvent read FOnEndDraw write FOnEndDraw;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnItemCompare: TItemCompareEvent read FOnItemCompare write FOnItemCompare;
    property OnItemSelect: TItemSelectEvent read FOnItemSelect write FOnItemSelect;
    property OnItemDeSelect: TItemSelectEvent read FOnItemDeSelect write FOnItemDeSelect;
    property OnItemAppearance: TItemAppearanceEvent read FOnItemAppearance write FOnItemAppearance;
    property OnItemReorder: TItemReorderEvent read FOnItemReorder write FOnItemReorder;
{$IFNDEF DELPHI2006_LVL}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
{$ENDIF}
    property HandleAppearance: THandleAppearance read FHandleAppearance write SetHandleAppearance;
    property ScrollType: TScrollType read FScrollType write SetScrollType default stScrollers;
    property Cursor: TCursor read GetCursorEx write SetCursorEx;
    property ThumbTracking : Boolean read FThumbTracking write SetThumbTracking default true;
    property BorderStyle : TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure DragDrop(Source: TObject; X, Y: integer); override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Loaded; override;
    procedure DblClick; override;
    procedure ApplyDefaultItemStyle;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    property IsMainMenu: Boolean read FIsMainMenu write SetIsMainMenu default false;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property Items[Index: integer]: TCustomItem read GetItem write SetItem; default;
    function ItemCount: integer;
    property DragLineColor: TColor read FDragLineColor write SetDragLineColor default clRed;
    property DragLine: Boolean read FDragLine write SetDragLine default true;
    procedure DrawBackGround(g: TGPGraphics);
    procedure DrawItems(g: TGPGraphics);
    procedure DrawHandles(g: TGPGraphics);
    procedure DrawDragLine(g: TGPGraphics; ARect: TGPRectF);
    procedure Draw(g: TGPGraphics);
    procedure ConnectContainer(Container: TCustomItemsContainer; Connection: TContainerConnection; ContainerMode: TContainerMode);
    function SelectedDragDropItem: TCustomItem;
    function DropItem: TCustomItem;
    function AddDropItem(it: TCustomItem): TCustomItem;
    function AddItem(AClass: TCustomItemClass): TCustomItem;
    procedure RemoveItem(Index: integer);
    procedure ClearItems;
    procedure SelectItem(Index: integer);
    procedure UnSelectItem(Index: integer);
    procedure VisualizeItem(Index: integer; AllowScrollItem: Boolean = true; AllowSelectItem: Boolean = true);
    procedure ScrollToItem(Index: integer);
    function InsertItem(Index: integer; AClass: TCustomItemClass): TCustomItem;
    property OnInternalChange: TNotifyEvent read FOnInternalChange write FOnInternalChange;
    function GetCanvas: TCanvas;
    property OnVerticalScroll: TScrollEvent read FOnVerticalScroll write FOnVerticalScroll;
    property OnHorizontalScroll: TScrollEvent read FOnHorizontalScroll write FOnHorizontalScroll;
    property OnInternalExpand: TExpandEvent read FOnInternalExpand write FOnInternalExpand;
    property PictureContainer: TGDIPPictureContainer read GetPictureContainer write SetPictureContainer;
    property ImageList: TCustomImageList read GetImageList write SetImageList;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    procedure ChangeStyle(AWin7: Boolean; AStyle: Integer);
    procedure ChangeMenu(AColor: TColor);
    procedure UpdateMenu;
    procedure HideMenu;
    procedure ShowShortCutHints;
    procedure HideShortCutHints;
    procedure ShowMenuShortCuts;
    procedure HideMenuShortCuts;
    property TextRendering: TTextRenderingHint read FTextRenderingHint write SetTextRenderingHint default TextRenderingHintClearTypeGridFit;
    property HTMLCache: Boolean read FHTMLCache write SetHTMLCache default True;
    property ShowDesignTimeMessage: Boolean read FShowDesignTimeMessage write SetShowDesignTimeMessage default True;
  end;

implementation

function Lighter(Color: TColor; Percent: Byte): TColor;
var
  r, g, b: Byte;
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  r := r + muldiv(255 - r, Percent, 100); // Percent% closer to white
  g := g + muldiv(255 - g, Percent, 100);
  b := b + muldiv(255 - b, Percent, 100);
  Result := RGB(r, g, b);
end;

function Darker(Color: TColor; Percent: Byte): TColor;
var
  r, g, b: Byte;
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  r := r - muldiv(r, Percent, 100); // Percent% closer to black
  g := g - muldiv(g, Percent, 100);
  b := b - muldiv(b, Percent, 100);
  Result := RGB(r, g, b);
end;

{ TCustomItemsContainer }

function TCustomItemsContainer.AddDropItem(it: TCustomItem): TCustomItem;
var
  newit: TCustomItem;
begin
  Result := nil;
  if Assigned(it) then
  begin
    newit := it.CreateNewItem(Self);
    newit.Assign(it);
    Result := List.AddItem(newit);
  end;
end;

function TCustomItemsContainer.AddItem(AClass: TCustomItemClass): TCustomItem;
begin
  Result := AClass.Create(Self);
  List.AddItem(Result);
end;

procedure TCustomItemsContainer.AlignControls(AControl: TControl;
  var ARect: TRect);
begin
  inherited;
  if FPosListUpdating then
    Exit;

  VertScrollPos := VertScrollBar.Position;
  HorzScrollPos := HorzScrollBar.Position;
  Changed;
end;

procedure TCustomItemsContainer.Anchor(Anchor: String);
begin
  keepoldcursor := true;
  if Anchor <> '' then
    Cursor := crHandPoint
  else
    Cursor := FoldCursor;
  keepoldcursor := false;
end;

procedure TCustomItemsContainer.ApplyDefaultItemStyle;
var
  I: integer;
  tmsif: ITMSStyle;
begin
  // default
  with FList.Appearance do
  begin
    Fill.Color := clWhite;
    Fill.ColorTo := clNone;
    Fill.BorderColor := clSilver;
    Normal.Rounding := 2;
    Hovered.Rounding := 2;
    Hovered.Glow := gmGradient;
    Selected.Rounding := 2;
    Selected.Glow := gmGradient;
    Down.Rounding := 2;
    Down.Glow := gmGradient;
    Disabled.Rounding := 2;
    Disabled.Glow := gmGradient;

    ButtonNormal.Rounding := 2;
    ButtonHovered.Rounding := 2;
    ButtonSelected.Rounding := 2;
    ButtonDown.Rounding := 2;
    ButtonDisabled.Rounding := 2;

    Normal.BorderColor := clNone;
    Normal.Color := clNone;
    Normal.ColorTo := clNone;
    Normal.ColorMirror := clNone;
    Normal.ColorMirrorTo := clNone;
    Hovered.ColorTo := clNone;
    Hovered.ColorMirror := clNone;
    Hovered.ColorMirrorTo := clNone;
    Selected.ColorTo := clNone;
    Selected.ColorMirror := clNone;
    Selected.ColorMirrorTo := clNone;
    Down.ColorTo := clNone;
    Down.ColorMirror := clNone;
    Down.ColorMirrorTo := clNone;
    Hovered.BorderColor := RGB(242, 205, 96);
    Hovered.Color := RGB(253, 227, 138);
    Selected.BorderColor := RGB(206, 160, 79);
    Selected.Color := RGB(254, 225, 69);
    Down.BorderColor := RGB(194, 138, 48);
    Down.Color := RGB(254, 216, 107);

    HoveredFont.Color := clBlack;
    SelectedFont.Color := clBlack;
    DownFont.Color := clBlack;
    DisabledFont.Color := clBlack;
    NormalFont.Color := clBlack;

    ButtonNormal.GradientType := gtVertical;
    ButtonNormal.GradientMirrorType := gtNone;
    ButtonNormal.BorderColor := RGB(236, 237, 237);
    ButtonNormal.Color := clWhite;
    ButtonNormal.ColorTo := RGB(237, 239, 241);
    ButtonNormal.ColorMirror := clNone;
    ButtonNormal.ColorMirrorTo := clNone;
    ButtonHovered.ColorTo := clNone;
    ButtonHovered.ColorMirror := clNone;
    ButtonHovered.ColorMirrorTo := clNone;
    ButtonSelected.ColorTo := clNone;
    ButtonSelected.ColorMirror := clNone;
    ButtonSelected.ColorMirrorTo := clNone;
    ButtonDown.ColorTo := clNone;
    ButtonDown.ColorMirror := clNone;
    ButtonDown.ColorMirrorTo := clNone;
    ButtonHovered.BorderColor := RGB(242, 205, 96);
    ButtonHovered.Color := RGB(253, 227, 138);
    ButtonSelected.BorderColor := RGB(206, 160, 79);
    ButtonSelected.Color := RGB(254, 225, 69);
    ButtonDown.BorderColor := RGB(194, 138, 48);
    ButtonDown.Color := RGB(254, 216, 107);

    ButtonHovered.Glow := gmGradient;
    ButtonDown.Glow := gmGradient;
    ButtonSelected.Glow := gmGradient;
    ButtonDisabled.Glow := gmGradient;
  end;

  for I := 0 to List.Items.Count - 1 do
  begin
    if List.Items[I].GetInterface(ITMSStyle, tmsif) then
      tmsif.SetComponentStyle(tsCustom);
  end;

end;

procedure TCustomItemsContainer.Assign(Source: TPersistent);
begin
  if Source is TCustomItemsContainer then
  begin
    FHTMLCache := (Source as TCustomItemsContainer).HTMLCache;
    FAutoSizeMode := (Source as TCustomItemsContainer).AutoSizeMode;
    FAutoSizeType := (Source as TCustomItemsContainer).AutoSizeType;
    FList.Assign((Source as TCustomItemsContainer).List);
    FFill.Assign((Source as TCustomItemsContainer).Fill);
    FHorizontalSpacing := (Source as TCustomItemsContainer).HorizontalSpacing;
    FVerticalSpacing := (Source as TCustomItemsContainer).VerticalSpacing;
    FListMargins.Assign((Source as TCustomItemsContainer).ListMargins);
    FReadOnly := (Source as TCustomItemsContainer).ReadOnly;
    FColumns := (Source as TCustomItemsContainer).Columns;
    FRows := (Source as TCustomItemsContainer).Rows;
    FShowFocus := (Source as TCustomItemsContainer).ShowFocus;
    FBorderMode := (Source as TCustomItemsContainer).BorderMode;
    FBorderTypes := (Source as TCustomItemsContainer).BorderTypes;
    PictureContainer := (Source as TCustomItemsContainer).PictureContainer;
    ImageList := (Source as TCustomItemsContainer).ImageList;
    FShowDesignTimeMessage := (Source as TCustomItemsContainer).ShowDesignTimeMessage;
    FReorder := (Source as TCustomItemsContainer).Reorder;
    FThumbTracking := (Source as TCustomItemsContainer).ThumbTracking;
    FTransparent := (Source as TCustomItemsContainer).Transparent;
    Changed;
  end;
end;

procedure TCustomItemsContainer.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TCustomItemsContainer.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TCustomItemsContainer.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TCustomItemsContainer.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomItemsContainer.Changed;
begin
  if csDestroying in ComponentState then
    Exit;

  if FUpdateCount = 0 then
  begin
    FList.Width := Width;
    FList.Height := Height;
    UpdateList;
    if Assigned(OnInternalChange) then
      OnInternalChange(Self);
    if Assigned(OnChange) then
      OnChange(Self);
    Invalidate;
  end;
end;

procedure TCustomItemsContainer.ChangeMenu(AColor: TColor);
begin
  if IsMainMenu then
  begin
    with List.Appearance do
    begin
      Normal.Glow := gmNone;
      Hovered.Glow := gmNone;
      Down.Glow := gmNone;
      Disabled.Glow := gmNone;
      Selected.Glow := gmRadialGradient;
      Hovered.BorderColor := Acolor;
      Hovered.Color := Lighter(AColor, 40);
      Hovered.Opacity := 40;
      Down.Assign(Hovered);
      Selected.BorderColor := Acolor;
      Selected.Color := Lighter(AColor, 20);
      Selected.ColorTo := clNone;
      Selected.GlowGradientColor := Darker(AColor, 8);
      Selected.GlowRadialColor := clWhite;
    end;
  end;
end;

procedure TCustomItemsContainer.ChangeStyle(AWin7: Boolean; AStyle: Integer);
begin
  if IsMainMenu then
  begin
    case TTMSStyle(AStyle) of
      tsOffice2010Blue:
      begin
        List.Appearance.NormalFont.Color := clBlack;
        List.Appearance.DownFont.Color := clBlack;
        List.Appearance.HoveredFont.Color := clBlack;
        List.Appearance.DisabledFont.Color := clBlack;
        List.Appearance.SelectedFont.Color := clBlack;
        Fill.Color := $FBF4EB;
        Fill.ColorTo := $ECD5C5;
        Fill.BorderColor := $DCC9B6;
      end;
      tsOffice2010Silver:
      begin
        List.Appearance.NormalFont.Color := clBlack;
        List.Appearance.DownFont.Color := clBlack;
        List.Appearance.HoveredFont.Color := clBlack;
        List.Appearance.DisabledFont.Color := clBlack;
        List.Appearance.SelectedFont.Color := clBlack;
        Fill.Color := $FCFBFA;
        Fill.ColorTo := $E9E5E2;
        Fill.BorderColor := $CCCBCA;
      end;
      tsOffice2010Black:
      begin
        List.Appearance.NormalFont.Color := clWhite;
        List.Appearance.DownFont.Color := clWhite;
        List.Appearance.HoveredFont.Color := clWhite;
        List.Appearance.DisabledFont.Color := clWhite;
        List.Appearance.SelectedFont.Color := clBlack;
        Fill.Color := $707070;
        Fill.ColorTo := $3E3E3E;
        Fill.BorderColor := $424242;
      end;
    end;
  end;
end;

procedure TCustomItemsContainer.ClearItems;
begin
  BeginUpdate;
  List.Items.Clear;
  UpdateScrollBars;
  EndUpdate;
end;

procedure TCustomItemsContainer.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;
  inherited;
end;

procedure TCustomItemsContainer.CMDesignHitTest(var Msg: TCMDesignHitTest);
var
  p: TPoint;
  it: TCustomItem;
begin
  inherited;
  if (csDesigning in ComponentState) then
  begin
    GetCursorPos(p);
    p := ScreenToClient(p);
    it := List.ItemAtXY(p.X, p.Y);
    if Assigned(it) and Assigned(List.OnNotifyItemDesignTimeSelect) then
      Msg.Result := 1;
  end;
end;

procedure TCustomItemsContainer.CMDialogChar(var Message: TCMDialogChar);
begin
  List.DoCMDialogChar(Message);
  if not (Message.Result = 1) then
    inherited;
end;

procedure TCustomItemsContainer.CMDrag(var Message: TCMDrag);
begin
  inherited;
  if Reorder or (csDesigning in ComponentState) then
  begin
    case Message.DragMessage of
      dmDragEnter:
        begin
          FDrawDragLine := true;
          invalidate;
        end;
      dmDragLeave:
        begin
          FDrawDragLine := false;
          invalidate;
        end;
    end;
  end;
end;

procedure TCustomItemsContainer.CMHintShow(var Message: TMessage);
begin
  List.DoCMHintShow(Message);
  inherited;
end;

procedure TCustomItemsContainer.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
{$IFNDEF DELPHI2006_LVL}
  if Assigned(OnMouseEnter) then
    OnMouseEnter(Self);
{$ENDIF}
end;

procedure TCustomItemsContainer.CMMouseLeave(var Message: TMessage);
begin
  inherited;
{$IFNDEF DELPHI2006_LVL}
  if Assigned(OnMouseLeave) then
    OnMouseLeave(Self);
{$ENDIF}
  FTimeDownOnArrow := 0;
  FHandleHorzLeftHover := false;
  FHandleHorzRightHover := false;
  FHandleVertTopHover := false;
  FHandleVertBottomHover := false;
  List.DoCMMouseLeave(Message);
  RefreshList;
end;

procedure TCustomItemsContainer.ConnectContainer
  (Container: TCustomItemsContainer; Connection: TContainerConnection;
  ContainerMode: TContainerMode);
begin
  BorderMode := bmTransition;
  Container.BorderMode := bmTransition;
  case Connection of
    ccLeft:
      begin
        BorderTypes := BorderTypes - [btNormalLeft];
        Container.BorderTypes := Container.BorderTypes - [btNormalRight];
        Container.Left := Left - Container.Width;
        Container.Top := Top;

        case ContainerMode of
          cmNormalBorder:
            BorderTypes := BorderTypes + [btNormalLeft];
          cmGradientBorder:
            BorderTypes := BorderTypes + [btGradientLeft];
        end;
      end;
    ccTop:
      begin
        BorderTypes := BorderTypes - [btNormalTop];
        Container.BorderTypes := Container.BorderTypes - [btNormalBottom];
        Container.Top := Top - Container.Height;
        Container.Left := Left;

        case ContainerMode of
          cmNormalBorder:
            BorderTypes := BorderTypes + [btNormalTop];
          cmGradientBorder:
            BorderTypes := BorderTypes + [btGradientTop];
        end;
      end;
    ccRight:
      begin
        BorderTypes := BorderTypes - [btNormalRight];
        Container.BorderTypes := Container.BorderTypes - [btNormalLeft];
        Container.Left := Left + Width;
        Container.Top := Top;

        case ContainerMode of
          cmNormalBorder:
            BorderTypes := BorderTypes + [btNormalRight];
          cmGradientBorder:
            BorderTypes := BorderTypes + [btGradientRight];
        end;
      end;
    ccBottom:
      begin
        BorderTypes := BorderTypes - [btNormalBottom];
        Container.BorderTypes := Container.BorderTypes - [btNormalTop];
        Container.Top := Top + Height;
        Container.Left := Left;

        case ContainerMode of
          cmNormalBorder:
            BorderTypes := BorderTypes + [btNormalBottom];
          cmGradientBorder:
            BorderTypes := BorderTypes + [btGradientBottom];
        end;
      end;
  end;
end;

constructor TCustomItemsContainer.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls, csOpaque]; 
  FBorderStyle := bsNone;
  FHTMLCache := True;
  FShowDesignTimeMessage := True;
  FHandleAppearance := THandleAppearance.Create(Self);
  FHandleAppearance.OnChange := HandleChanged;
  FReorder := true;
  FOldScrollHPos := 0;
  FOldScrollVPos := 0;
  AutoScroll := false;
  DoubleBuffered := true;
  FAutoSizeMode := asmItems;
  FAutoSizeType := astWidth;
  FColumns := 1;
  FRows := 0;
  Width := 280;
  Height := 300;
  TabStop := true;
  FBorderTypes := [btNormalLeft, btNormalTop, btNormalRight, btNormalBottom];
  FBorderMode := bmNormal;
  FList := TCustomBaseList.Create(Self);
  FList.OnRefresh := ListRefresh;
  FList.OnChange := ListChanged;
  FList.OnItemCompare := ListItemCompare;
  FList.OnItemSelect := ListItemSelect;
  FList.OnItemDeSelect := ListItemDeSelect;
  FList.OnItemAppearance := ListItemAppearance;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FListWidth := 200;
  FListMargins := TMargins.Create(nil);
  FListMargins.OnChange := ListMarginsChanged;
  FReadOnly := false;
  FMinimumSize := 200;
  FVerticalSpacing := 5;
  FHorizontalSpacing := 5;
  FShowFocus := true;
  FThumbTracking := true;
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.OnTimer := Scrolling;
  FScrollTimer.Interval := 10;
  FScrollTimer.Enabled := false;

  FDragLine := true;
  FDragLineColor := clRed;

  FFill.Color := clWhite;
  FFill.ColorTo := clWhite;
  FFill.BorderColor := clSilver;

  ApplyDefaultItemStyle;
  FHandleTimer := TTimer.Create(Self);
  FHandleTimer.OnTimer := HandleTimerChanged;
  FHandleTimer.Enabled := true;
  FHandleTimer.Interval := 10;

  FTransparent := false;

  FIsAppMenu := false;
  FTextRenderingHint := TextRenderingHintClearTypeGridFit;
end;

procedure TCustomItemsContainer.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array [TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TCustomItemsContainer.CreateWnd;
var
  i: integer;
  it: TCustomItem;
begin
  inherited;

  if not FDidScale then
  begin

    CalculateDPIScale(GetParentForm(Self), Canvas.Handle);

    for I := 0 to List.Items.Count - 1 do
    begin
      if (List.Items[I] is TCustomItem) then
      begin
        it := TCustomItem(List.Items[I]);
        if it.Visible then
        begin
          it.Height := Round(it.Height * DPIScale);
          it.Width := Round(it.Width * DPIScale);
        end;
      end;
    end;

    FDidScale := true;
  end;
end;

procedure TCustomItemsContainer.DblClick;
begin
  FDblClick := True;
  inherited;
  FList.DoDblClick(Self, ScreenToClient(Mouse.CursorPos));
end;

destructor TCustomItemsContainer.Destroy;
begin
  FScrollTimer.Free;
  FHandleTimer.Free;
  FHandleAppearance.Free;
  FListMargins.Free;
  FFill.Free;
  if Assigned(List.OnNotifyListDestroy) then
    List.OnNotifyListDestroy(Self);
  FList.Free;
  inherited;
end;

procedure TCustomItemsContainer.DoEnter;
begin
  inherited;
  List.DoEnter;
  Changed;
end;

procedure TCustomItemsContainer.DoExit;
begin
  inherited;
  List.DoExit;
  List.HideShortCutHints;
  Changed;
end;

procedure TCustomItemsContainer.DragDrop(Source: TObject; X, Y: integer);
var
  it: TCustomItem;
  Allow: Boolean;
begin
  inherited;
  if Reorder or (csDesigning in ComponentState) then
  begin
    it := SelectedDragDropItem;
    if Assigned(it) then
    begin
      if Assigned(FDropItem) then
      begin
        Allow := True;
        if Assigned(OnItemReorder) then
          OnItemReorder(Self, it, FDropItem, Allow);

        if Allow then
        begin
          if FDropItem.Index > it.Index then
            it.Index := FDropItem.Index - 1
          else
            it.Index := FDropItem.Index;
        end;
        FDropItem := nil;
      end;
    end;
  end;
end;

procedure TCustomItemsContainer.DragOver(Source: TObject; X, Y: integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited;
  if Reorder or (csDesigning in ComponentState) then
  begin
    Accept := Accept or (Source = Self);
    FDropItem := List.ItemAtXY(X, Y + VerticalSpacing div 2);
    invalidate;
  end;

  if State = dsDragLeave then
    FScrollTimer.Enabled := false
  else
    FScrollTimer.Enabled := (Y < 25) or (Y > Height - 25);
end;

procedure TCustomItemsContainer.Draw(g: TGPGraphics);
var
  r: TGPRectF;
begin
  r := MakeRect(0, 0, Width - 1, Height - 1);
  if Assigned(OnStartDraw) then
    OnStartDraw(Self, g, r);
  DrawBackGround(g);
  DrawItems(g);
  DrawDragLine(g, r);
  DrawHandles(g);
  if Assigned(OnEndDraw) then
    OnEndDraw(Self, g, r);
end;

procedure TCustomItemsContainer.DrawBackGround(g: TGPGraphics);
var
  r, rb: TGPRectF;
  pth: TGPGraphicsPath;
  gb: TGPPathGradientBrush;
  p: TGPPen;
  bw: integer;
begin
  bw := FFill.BorderWidth;
  r := MakeRect(0, 0, Width - 1, Height - 1);
  if (BorderMode = bmTransition) then
  begin
    FFill.BeginUpdate;
    FFill.BorderWidth := 0;
    r := MakeRect(-1, -1, Width + 1, Height + 1);
    FFill.EndUpdate;
  end;

  if BorderStyle <> bsNone then
  begin
    FFill.BeginUpdate;
    FFill.BorderWidth := 0;
    FFill.EndUpdate;
  end;

  FFill.Fill(g, r);

  FFill.BeginUpdate;
  FFill.BorderWidth := bw;
  FFill.EndUpdate;

  r := MakeRect(0, 0, Width - 1, Height - 1);
  if BorderMode = bmTransition then
  begin
    p := TGPPen.Create(MakeColor(FFill.BorderOpacity, FFill.BorderColor), bw);
    if btGradientLeft in BorderTypes then
    begin
      rb := MakeRect(r.X - 1, r.Y + 1, 2, r.Height - 2);
      pth := TGPGraphicsPath.Create;
      pth.AddRectangle(rb);
      gb := TGPPathGradientBrush.Create(pth);
      gb.SetCenterColor(MakeColor(Fill.BorderOpacity, Fill.BorderColor));
      g.FillPath(gb, pth);
      pth.Free;
      gb.Free;
    end
    else if btNormalLeft in BorderTypes then
      g.DrawLine(p, r.X, r.Y, r.X, r.Height);

    if btGradientTop in BorderTypes then
    begin
      rb := MakeRect(r.X + 1, r.Y - 1, r.Width - 2, 2);
      pth := TGPGraphicsPath.Create;
      pth.AddRectangle(rb);
      gb := TGPPathGradientBrush.Create(pth);
      gb.SetCenterColor(MakeColor(Fill.BorderOpacity, Fill.BorderColor));
      g.FillPath(gb, pth);
      pth.Free;
      gb.Free;
    end
    else if btNormalTop in BorderTypes then
      g.DrawLine(p, r.X, r.Y, r.X + r.Width, r.Y);

    if btGradientRight in BorderTypes then
    begin
      rb := MakeRect(r.X + r.Width - 1, r.Y + 1, 2, r.Height - 2);
      pth := TGPGraphicsPath.Create;
      pth.AddRectangle(rb);
      gb := TGPPathGradientBrush.Create(pth);
      gb.SetCenterColor(MakeColor(Fill.BorderOpacity, Fill.BorderColor));
      g.FillPath(gb, pth);
      pth.Free;
      gb.Free;
    end
    else if btNormalRight in BorderTypes then
      g.DrawLine(p, r.X + r.Width, r.Y, r.X + r.Width, r.Height);

    if btGradientBottom in BorderTypes then
    begin
      rb := MakeRect(r.X + 1, r.Y + r.Height - 1, r.Width - 2, 2);
      pth := TGPGraphicsPath.Create;
      pth.AddRectangle(rb);
      gb := TGPPathGradientBrush.Create(pth);
      gb.SetCenterColor(MakeColor(Fill.BorderOpacity, Fill.BorderColor));
      g.FillPath(gb, pth);
      pth.Free;
      gb.Free;
    end
    else if btNormalBottom in BorderTypes then
      g.DrawLine(p, r.X, r.Y + r.Height, r.X + r.Width, r.Height);
    p.Free;
  end;
end;

procedure TCustomItemsContainer.DrawDragLine(g: TGPGraphics; ARect: TGPRectF);
var
  p: TGPPen;
  drop, drag: TCustomItem;
  old: SmoothingMode;
begin
  if DragLine then
  begin
    drop := FDropItem;
    drag := SelectedDragDropItem;
    if Assigned(drop) and Assigned(drag) and FDrawDragLine then
    begin
      if drop <> drag then
      begin
        old := g.GetSmoothingMode;
        g.SetSmoothingMode(SmoothingModeDefault);

        p := TGPPen.Create(MakeColor(200, DragLineColor));

        if (drag.Y >= drop.Y) and (drag.Y <= drop.Y + drop.Height) then
        begin
          g.DrawLine(p, drop.X - HorizontalSpacing / 2, drop.Y,
            drop.X - HorizontalSpacing / 2, drop.Y + drop.Height)
        end
        else
        begin
          g.DrawLine(p, drop.X, drop.Y - VerticalSpacing / 2, drop.X + drop.Width,
            drop.Y - VerticalSpacing / 2);
        end;

        g.SetSmoothingMode(old);

        p.Free;
      end;
    end;
  end;
end;

procedure TCustomItemsContainer.DrawHandles(g: TGPGraphics);
var
  chl, chr, cvt, cvb: TColor;
  p: TGPPen;
  b: TGPBrush;
  path: TGPGraphicsPath;
  rhl, rhr, rvt, rvb: TGPRectF;
  doHandleVertTop, doHandleVertBottom, doHandleHorzLeft,
    doHandleHorzRight: Boolean;
begin
  if ScrollType = stHandles then
  begin
    with HandleAppearance do
    begin
      doHandleVertTop := IsHandleVertTop;
      doHandleVertBottom := IsHandleVertBottom;
      doHandleHorzLeft := IsHandleHorzLeft;
      doHandleHorzRight := IsHandleHorzRight;
      rhl := GetHandleHorzLeft;
      rhr := GetHandleHorzRight;
      rvt := GetHandleVertTop;
      rvb := GetHandleVertBottom;

      chl := Color;
      if FHandleHorzLeftDown then
        chl := Darker(chl, 30)
      else if FHandleHorzLeftHover then
        chl := Lighter(chl, 30);

      chr := Color;
      if FHandleHorzRightDown then
        chr := Darker(chl, 30)
      else if FHandleHorzRightHover then
        chr := Lighter(chl, 30);

      cvt := Color;
      if FHandleVertTopDown then
        cvt := Darker(cvt, 30)
      else if FHandleVertTopHover then
        cvt := Lighter(cvt, 30);

      cvb := Color;
      if FHandleVertBottomDown then
        cvb := Darker(cvb, 30)
      else if FHandleVertBottomHover then
        cvb := Lighter(cvb, 30);

      if doHandleHorzLeft then
      begin
        // horizontal left
        path := TGPGraphicsPath.Create;
        path.AddArc(rhl, 270, 180);
        path.CloseFigure;

        b := TGPSolidBrush.Create(MakeColor(Opacity, chl));
        g.FillPath(b, path);
        b.Free;

        b := TGPLinearGradientBrush.Create(MakeRect(rhl.X, rhl.Y, rhl.Height,
            rhl.Width / 2), MakeColor(100, clWhite), MakeColor(0, clWhite),
          LinearGradientModeHorizontal);
        g.FillPath(b, path);
        b.Free;

        p := TGPPen.Create(MakeColor(Opacity, BorderColor), 1);
        g.DrawPath(p, path);
        p.Free;

        path.Free;

        path := TGPGraphicsPath.Create;
        path.AddLine(MakePoint(rhl.X + rhl.Width * 2 / 3,
            rhl.Y + rhl.Height / 2), MakePoint(rhl.X + rhl.Width * 4 / 5,
            rhl.Y + rhl.Height / 3));
        path.AddLine(MakePoint(rhl.X + rhl.Width * 2 / 3,
            rhl.Y + rhl.Height / 2), MakePoint(rhl.X + rhl.Width * 4 / 5,
            rhl.Y + rhl.Height * 2 / 3));

        p := TGPPen.Create(MakeColor(Opacity, ArrowColor), 2);
        g.DrawPath(p, path);
        p.Free;

        path.Free;

      end;

      if doHandleHorzRight then
      begin
        // horizontal Right
        path := TGPGraphicsPath.Create;
        path.AddArc(rhr, 90, 180);
        path.CloseFigure;

        b := TGPSolidBrush.Create(MakeColor(Opacity, chr));
        g.FillPath(b, path);
        b.Free;

        b := TGPLinearGradientBrush.Create(MakeRect(rhr.X, rhr.Y, rhr.Height,
            rhr.Width / 2), MakeColor(100, clWhite), MakeColor(0, clWhite),
          LinearGradientModeHorizontal);
        g.FillPath(b, path);
        b.Free;

        p := TGPPen.Create(MakeColor(Opacity, BorderColor), 1);
        g.DrawPath(p, path);
        p.Free;

        path.Free;

        path := TGPGraphicsPath.Create;
        path.AddLine(MakePoint(rhr.X + rhr.Width / 3, rhr.Y + rhr.Height / 2),
          MakePoint(rhr.X + rhr.Width / 5, rhr.Y + rhr.Height / 3));
        path.AddLine(MakePoint(rhr.X + rhr.Width / 3, rhr.Y + rhr.Height / 2),
          MakePoint(rhr.X + rhr.Width / 5, rhr.Y + rhr.Height * 2 / 3));

        p := TGPPen.Create(MakeColor(Opacity, ArrowColor), 2);
        g.DrawPath(p, path);
        p.Free;

        path.Free;
      end;

      if doHandleVertTop then
      begin
        // Vertical Top
        path := TGPGraphicsPath.Create;
        path.AddArc(rvt, 0, 180);
        path.CloseFigure;

        b := TGPSolidBrush.Create(MakeColor(Opacity, cvt));
        g.FillPath(b, path);
        b.Free;

        b := TGPLinearGradientBrush.Create(MakeRect(rvt.X, rvt.Y, rvt.Width,
            rvt.Height / 2), MakeColor(100, clWhite), MakeColor(0, clWhite),
          LinearGradientModeHorizontal);
        g.FillPath(b, path);
        b.Free;

        p := TGPPen.Create(MakeColor(Opacity, BorderColor), 1);
        g.DrawPath(p, path);
        p.Free;

        path.Free;

        path := TGPGraphicsPath.Create;
        path.AddLine(MakePoint(rvt.X + rvt.Width / 2,
            rvt.Y + rvt.Height * 2 / 3), MakePoint(rvt.X + rhl.Width / 3,
            rvt.Y + rvt.Height * 4 / 5));
        path.AddLine(MakePoint(rvt.X + rvt.Width / 2,
            rvt.Y + rvt.Height * 2 / 3), MakePoint(rvt.X + rhl.Width * 2 / 3,
            rvt.Y + rvt.Height * 4 / 5));

        p := TGPPen.Create(MakeColor(Opacity, ArrowColor), 2);
        g.DrawPath(p, path);
        p.Free;

        path.Free;
      end;

      if doHandleVertBottom then
      begin
        // Vertical Bottom
        path := TGPGraphicsPath.Create;
        path.AddArc(rvb, 180, 180);
        path.CloseFigure;

        b := TGPSolidBrush.Create(MakeColor(Opacity, cvb));
        g.FillPath(b, path);
        b.Free;

        b := TGPLinearGradientBrush.Create(MakeRect(rvb.X, rvb.Y, rvb.Height,
            rvb.Width / 2), MakeColor(100, clWhite), MakeColor(0, clWhite),
          LinearGradientModeHorizontal);
        g.FillPath(b, path);
        b.Free;

        p := TGPPen.Create(MakeColor(Opacity, BorderColor), 1);
        g.DrawPath(p, path);
        p.Free;

        path.Free;

        path := TGPGraphicsPath.Create;
        path.AddLine(MakePoint(rvb.X + rvb.Width / 2, rvb.Y + rvb.Height / 3),
          MakePoint(rvb.X + rvb.Width / 3, rvb.Y + rvb.Height / 5));
        path.AddLine(MakePoint(rvb.X + rvb.Width / 2, rvb.Y + rvb.Height / 3),
          MakePoint(rvb.X + rvb.Width * 2 / 3, rvb.Y + rvb.Height / 5));

        p := TGPPen.Create(MakeColor(Opacity, ArrowColor), 2);
        g.DrawPath(p, path);
        p.Free;

        path.Free;
      end;
    end;
  end;
end;

procedure TCustomItemsContainer.DrawItems(g: TGPGraphics);
var
  rgn: TGPRegion;
begin
  rgn := TGPRegion.Create(MakeRect(1, 1, Width - 2, Height - 2));
  g.SetClip(rgn);
  FList.DrawItems(g, TabStop and Focused and ShowFocus);
  g.ResetClip;
  rgn.Free;
end;

function TCustomItemsContainer.DropItem: TCustomItem;
begin
  Result := FDropItem;
end;

procedure TCustomItemsContainer.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed;
end;

procedure TCustomItemsContainer.Expand(Item: TCustomItem; Expand: Boolean);
var
  I: integer;
begin
  BeginUpdate;
  for I := Item.Index + 1 to List.Items.Count - 1 do
  begin
    if List.Items[I] is TCustomItem then
    begin
      if not(List.Items[I] as TCustomItem).isSection then
      begin (List.Items[I] as TCustomItem)
        .Visible := Expand;
        if Assigned(OnInternalExpand) then
          OnInternalExpand(Self, List.Items[I] as TCustomItem, Expand);
      end
      else
        Break;
    end;
  end;
  EndUpdate;
end;

procedure TCustomItemsContainer.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TCustomItemsContainer.FillPictureNames(Proc: TGetStrProc);
var
  I: integer;
begin
  if Assigned(PictureContainer) then
  begin
    for I := 0 to PictureContainer.Items.Count - 1 do
      Proc(PictureContainer.Items[I].Name);
  end;
end;

function TCustomItemsContainer.GetHandleHorzLeft: TGPRectF;
begin
  Result := MakeRect(-HandleAppearance.Size / 2,
    (Height - HandleAppearance.Size) / 2, HandleAppearance.Size,
    HandleAppearance.Size);
end;

function TCustomItemsContainer.GetHandleHorzRight: TGPRectF;
begin
  Result := MakeRect(Width - HandleAppearance.Size / 2 - 1,
    (Height - HandleAppearance.Size) / 2, HandleAppearance.Size,
    HandleAppearance.Size);
end;

function TCustomItemsContainer.GetHandleVertBottom: TGPRectF;
begin
  Result := MakeRect((Width - HandleAppearance.Size) / 2,
    Height - 1 - HandleAppearance.Size / 2, HandleAppearance.Size,
    HandleAppearance.Size);
end;

function TCustomItemsContainer.GetHandleVertTop: TGPRectF;
begin
  Result := MakeRect((Width - HandleAppearance.Size) / 2,
    -HandleAppearance.Size / 2, HandleAppearance.Size,
    HandleAppearance.Size);
end;

function TCustomItemsContainer.GetCanvas: TCanvas;
begin
  Result := Canvas;
end;

procedure TCustomItemsContainer.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  I: integer;
  it: TCustomItem;
begin
  inherited;
  for I := 0 to List.Items.Count - 1 do
  begin
    if List.Items[I] is TCustomItem then
    begin
      it := TCustomItem(List.Items[I]);
      Proc(it);
    end;
  end;
end;

function TCustomItemsContainer.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TCustomItemsContainer.GetCursorEx: TCursor;
begin
  Result := inherited Cursor;
end;

function TCustomItemsContainer.GetImageList: TCustomImageList;
begin
  Result := nil;
  if not (csDestroying in ComponentState) then
    if Assigned(List) then
      Result := List.Appearance.ImageList;
end;

function TCustomItemsContainer.GetItem(Index: integer): TCustomItem;
begin
  if (List.Items[Index] is TCustomItem) then
    Result := TCustomItem(List.Items[index])
  else
    Result := nil;
end;

function TCustomItemsContainer.GetMaxColumnBoxItem: TCustomItem;
var
  I: integer;
  it: TCustomItem;
  maxy: integer;
begin
  maxy := 0;
  Result := nil;
  for I := 0 to List.Items.Count - 1 do
  begin
    if List.Items[I] is TCustomItem then
    begin
      it := List.Items[I] as TCustomItem;
      if maxy > it.Y then
        Result := it;
    end;
  end;
end;

function TCustomItemsContainer.GetPictureContainer: TGDIPPictureContainer;
begin
  Result := nil;
  if not (csDestroying in ComponentState) then
    if Assigned(List) then
      Result := List.Appearance.PictureContainer;
end;

procedure TCustomItemsContainer.GetPictures(APictureList: TPictureContainerList);
var
  I: integer;
begin
  if Assigned(PictureContainer) then
  begin
    for I := 0 to PictureContainer.Items.Count - 1 do
    begin
      with APictureList.Add do
      begin
        Name := PictureContainer.Items[I].Name;
        Picture := PictureContainer.Items[I].Picture;
      end;
    end;
  end;
end;

function TCustomItemsContainer.GetShadowOffset: integer;
begin
  Result := 0;
  if Fill.ShadowColor <> clNone then
    Result := Fill.ShadowOffset;
end;

function TCustomItemsContainer.GetTotalColumnSize: integer;
var
  it: TCustomItem;
  I: integer;
  m, totalm: integer;
begin
  totalm := 0;
  for I := 0 to List.Items.Count - 1 do
  begin
    if List.Items[I] is TCustomItem then
    begin
      it := List.Items[I] as TCustomItem;
      if it.Visible then
      begin
        m := it.Y + it.Height + VertScrollPos;
        m := Max(0, m + GetShadowOffset + ListMargins.Bottom);
        if m > totalm then
          totalm := m;
      end;
    end;
  end;

  Result := totalm;
end;

function TCustomItemsContainer.GetTotalRowSize: integer;
var
  it: TCustomItem;
  I: integer;
  m, totalm: integer;
begin
  totalm := 0;
  for I := 0 to List.Items.Count - 1 do
  begin
    if List.Items[I] is TCustomItem then
    begin
      it := List.Items[I] as TCustomItem;
      if it.Visible then
      begin
        m := it.X + it.Width + HorzScrollPos;
        m := Max(0, m + GetShadowOffset + ListMargins.Right);
        if m > totalm then
          totalm := m;
      end;
    end;
  end;

  Result := totalm;
end;

function TCustomItemsContainer.GetVersion: String;
var
  vn: integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn)))
    + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TCustomItemsContainer.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TCustomItemsContainer.HandleChanged(Sender: TObject);
begin
  RefreshList;
end;

procedure TCustomItemsContainer.HandleTimerChanged(Sender: TObject);
begin
  Inc(FTimeDownOnArrow);
  if FTimeDownOnArrow >= 30 then
  begin
    if FHandleHorzLeftDown then
    begin
      HorzScrollPos := HorzScrollPos - HorzScrollBar.Increment;
      UpdateItemPositions;
    end;

    if FHandleHorzRightDown then
    begin
      HorzScrollPos := HorzScrollPos + HorzScrollBar.Increment;
      UpdateItemPositions;
    end;

    if FHandleVertTopDown then
    begin
      VertScrollPos := VertScrollPos - VertScrollBar.Increment;
      UpdateItemPositions;
    end;

    if FHandleVertBottomDown then
    begin
      VertScrollPos := VertScrollPos + VertScrollBar.Increment;
      UpdateItemPositions;
    end;
  end;
end;

procedure TCustomItemsContainer.HideMenu;
begin
//
end;

procedure TCustomItemsContainer.HideMenuShortCuts;
begin
  HideShortCutHints;
end;

procedure TCustomItemsContainer.HideShortCutHints;
begin
  List.HideShortCutHints;
  FShortCutItemHintShowing := False;
end;

function TCustomItemsContainer.InsertItem(Index: integer;
  AClass: TCustomItemClass): TCustomItem;
begin
  Result := AClass.Create(Self);
  List.InsertItem(Index, Result);
end;

function TCustomItemsContainer.IsHandleHorzLeft: Boolean;
begin
  Result := HorzScrollVis and (ScrollType = stHandles) and (HorzScrollPos > 0);
end;

function TCustomItemsContainer.IsHandleHorzRight: Boolean;
begin
  Result := HorzScrollVis and (ScrollType = stHandles) and
    (HorzScrollPos < HorzScrollBar.Range - HorzScrollBar.ThumbSize);
end;

function TCustomItemsContainer.IsHandleVertBottom: Boolean;
begin
  Result := VertScrollVis and (ScrollType = stHandles) and
    (VertScrollPos < VertScrollBar.Range - VertScrollBar.ThumbSize);
end;

function TCustomItemsContainer.IsHandleVertTop: Boolean;
begin
  Result := VertScrollVis and (ScrollType = stHandles) and (VertScrollPos > 0);
end;

function TCustomItemsContainer.IsMetroStyle: Boolean;
begin
  Result := FMetroStyle;
end;

function TCustomItemsContainer.ItemCount: integer;
begin
  Result := List.Items.Count;
end;

procedure TCustomItemsContainer.KeyDown(var Key: Word; Shift: TShiftState);
var
  it, itshortcut: TCustomItem;
  s, shCut: string;
  i: Integer;
  k: Word;
begin
  inherited;

  if ReadOnly or (List.Items.Count = 0) then
    Exit;

  if (Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_TAB]) then
  begin
    if FShortCutItemHintShowing then
      HideShortCutHints;
  end;

  if FShortCutItemHintShowing then
  begin
    k := Key;
    if k in [VK_NUMPAD0..VK_NUMPAD9] then
      k := k - 48;
    s := char(k);
    FShortCutChars := FShortCutChars + s;

    if (FShortCutItemHintShowing) then
    begin
      for I := List.FirstVisibleIdx to List.LastVisibleIdx do
      begin
        if (I >= 0) and (I <= List.Items.Count - 1) then
        begin
          if List.Items[i] is TCustomItem then
          begin
            itshortcut := TCustomItem(List.Items[i]);
            if (itshortcut.ShortCutHint <> '') then
              shCut := itshortcut.ShortCutHint;

            if (UpperCase(shCut) = UpperCase(FShortCutChars)) then
            begin
              HideShortCutHints;
              SelectItem(itshortcut.Index);
              if Assigned(itshortcut.OnItemClick) then
                itshortcut.OnItemClick(Self, itshortcut);
              Break;
            end
          end;
        end;
      end;
    end;
    Exit;
  end;

  if (Columns = 0) and (Rows = 0) then
    FList.DoKeyDownBox(Self, Key, Shift)
  else
    FList.DoKeyDownGrid(Self, Key, Shift);

  it := FList.SelectedItem;
  if Assigned(it) then
  begin
    ScrollToItem(it.Index);
  end;

  FList.DoKeyDown(Self, Key, Shift);
end;

procedure TCustomItemsContainer.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if FFocusKilled then
  begin
    FFocusKilled := False;
    inherited;
    Exit;
  end;

  if ReadOnly or (List.Items.Count = 0) then
    Exit;

  if Key = VK_MENU then
  begin
    Key := 0;
    if FShortCutItemHintShowing then
      HideShortCutHints
    else
      ShowShortCutHints;
  end;

  inherited;

  FList.DoKeyUp(Self, Key, Shift);
end;

procedure TCustomItemsContainer.ListChanged(Sender: TObject);
begin
  Changed;
end;

procedure TCustomItemsContainer.ListItemAppearance(Sender: TObject;
  Item: TCustomItem; Appearance: TItemAppearance);
begin
  if Assigned(OnItemAppearance) then
    OnItemAppearance(Sender, Item, Appearance);
end;

procedure TCustomItemsContainer.ListItemCompare(Sender: TObject;
  Item1, Item2: TCustomItem; var Result: integer);
begin
  if Assigned(OnItemCompare) then
    OnItemCompare(Sender, Item1, Item2, Result)
  else
    Result := AnsiCompareStr(TCustomItem(Item1).CustomClassName,
      TCustomItem(Item2).CustomClassName);
end;

procedure TCustomItemsContainer.ListItemDeSelect(Sender: TObject;
  Item: TCustomItem; var Allow: Boolean);
begin
  if Assigned(OnItemDeSelect) then
    OnItemDeSelect(Sender, Item, Allow);
end;

procedure TCustomItemsContainer.ListItemSelect(Sender: TObject;
  Item: TCustomItem; var Allow: Boolean);
begin
  if Assigned(OnItemSelect) then
    OnItemSelect(Sender, Item, Allow);
end;

procedure TCustomItemsContainer.ListMarginsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TCustomItemsContainer.ListRefresh(Sender: TObject);
begin
  RefreshList;
end;

procedure TCustomItemsContainer.Loaded;
var
  it: integer;
begin
  inherited;
  if Assigned(List) then
  begin
    if list.GetCountFocusableItems > 0 then
    begin
      it := List.FocusedItemIndex;
      if (it >= 0) and (it <= List.items.Count - 1) then
      begin
        if not Items[it].TabStop or not List.IsItemFocusable(Items[it]) then
          List.FocusNextItem(false);
      end;
    end
    else
      List.FocusedItemIndex := -1;
  end;
end;

procedure TCustomItemsContainer.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  it: TCustomItem;
begin
  inherited;

  if csDesigning in ComponentState then
  begin
    it := FList.ItemAtXY(X, Y);
    FMouseDownOnItemX := -1;
    FMouseDownOnItemY := -1;
    FMouseDownOnItem := false;
    FDragDropItem := nil;
    if Assigned(it) then
    begin
      FMouseDownOnItem := true;
      FDragDropItem := it;
      FMouseDownOnItemX := X;
      FMouseDownOnItemY := Y;
    end;
    Exit;
  end;

  if TabStop and IsWindowVisible(Handle) then
  begin
    SetFocus;
    List.DoEnter;
    Changed;
  end;

  FTimeDownOnArrow := 0;
  if IsHandleHorzLeft then
  begin
    FHandleHorzLeftDown := PtInGPRect(GetHandleHorzLeft, Point(X, Y));
    RefreshList;
  end;

  if IsHandleHorzRight then
  begin
    FHandleHorzRightDown := PtInGPRect(GetHandleHorzRight, Point(X, Y));
    RefreshList;
  end;

  if IsHandleVertTop then
  begin
    FHandleVertTopDown := PtInGPRect(GetHandleVertTop, Point(X, Y));
    RefreshList;
  end;

  if IsHandleVertBottom then
  begin
    FHandleVertBottomDown := PtInGPRect(GetHandleVertBottom, Point(X, Y));
    RefreshList;
  end;

  if FHandleHorzLeftDown or FHandleHorzRightDown or FHandleVertTopDown or
    FHandleVertBottomDown then
    Exit;

  it := FList.ItemAtXY(X, Y);
  FMouseDownOnItemX := -1;
  FMouseDownOnItemY := -1;
  FMouseDownOnItem := false;
  FDragDropItem := nil;
  if Assigned(it) and not FDblClick then
  begin
    FMouseDownOnItem := true;
    FDragDropItem := it;
    FMouseDownOnItemX := X;
    FMouseDownOnItemY := Y;
  end;

  FDblClick := False;

  if ReadOnly then
    Exit;

  FMouseDown := true;

  if not FHandleHorzLeftDown and not FHandleHorzRightDown and not
    FHandleVertTopDown and not FHandleVertBottomDown then
    FList.DoMouseDown(Self, Button, Shift, X, Y);
end;

procedure TCustomItemsContainer.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited;

  if csDesigning in ComponentState then
    Exit;

  if IsHandleHorzLeft then
  begin
    FHandleHorzLeftHover := PtInGPRect(GetHandleHorzLeft, Point(X, Y));
    RefreshList;
  end
  else
    FHandleHorzLeftHover := false;

  if IsHandleHorzRight then
  begin
    FHandleHorzRightHover := PtInGPRect(GetHandleHorzRight, Point(X, Y));
    RefreshList;
  end
  else
    FHandleHorzRightHover := false;

  if IsHandleVertTop then
  begin
    FHandleVertTopHover := PtInGPRect(GetHandleVertTop, Point(X, Y));
    RefreshList;
  end
  else
    FHandleVertTopHover := false;

  if IsHandleVertBottom then
  begin
    FHandleVertBottomHover := PtInGPRect(GetHandleVertBottom, Point(X, Y));
    RefreshList;
  end
  else
    FHandleVertBottomHover := false;

  if FHandleHorzLeftHover or FHandleHorzRightHover or FHandleVertTopHover or
    FHandleVertBottomHover then
    Exit;

  if FMouseDownOnItem then
  begin
    if ((Abs(FMouseDownOnItemX - X) > 5) or (Abs(FMouseDownOnItemY - Y) > 5)) and Reorder then
    begin
      FMouseDownOnItem := false;
      FDragging := true;
      FMouseDown := false;
      BeginDrag(true);
    end;
  end;

  if ReadOnly then
    Exit;

  if FMouseDown then
    Exit;

  if not FHandleHorzLeftHover and not FHandleHorzRightHover and not
    FHandleVertTopHover and not FHandleVertBottomHover then
    FList.DoMouseMove(Self, Shift, X, Y)
  else
    FList.ClearItemState;
end;

procedure TCustomItemsContainer.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  it: TCustomItem;
begin
  inherited;
  FTimeDownOnArrow := 0;
  if (csDesigning in ComponentState) then
  begin
    it := List.ItemAtXY(X, Y);
    if Assigned(it) then
    begin
      if Assigned(List.OnNotifyItemDesignTimeSelect) then
        List.OnNotifyItemDesignTimeSelect(Self, it.Index);
    end;

    FDragging := false;
    Exit;
  end;

  FMouseDownOnItem := false;

  if FHandleHorzLeftDown then
  begin
    HorzScrollPos := HorzScrollPos - HorzScrollBar.Increment;
    UpdateItemPositions;
  end;

  if FHandleHorzRightDown then
  begin
    HorzScrollPos := HorzScrollPos + HorzScrollBar.Increment;
    UpdateItemPositions;
  end;

  if FHandleVertTopDown then
  begin
    VertScrollPos := VertScrollPos - VertScrollBar.Increment;
    UpdateItemPositions;
  end;

  if FHandleVertBottomDown then
  begin
    VertScrollPos := VertScrollPos + VertScrollBar.Increment;
    UpdateItemPositions;
  end;

  if ReadOnly then
  begin
    if FHandleHorzLeftDown then
    begin
      FHandleHorzLeftDown := false;
      RefreshList;
    end;

    if FHandleHorzRightDown then
    begin
      FHandleHorzRightDown := false;
      RefreshList;
    end;

    if FHandleVertTopDown then
    begin
      FHandleVertTopDown := false;
      RefreshList;
    end;

    if FHandleVertBottomDown then
    begin
      FHandleVertBottomDown := false;
      RefreshList;
    end;

    Exit;
  end;

  if FDragging then
  begin
    FDragging := false;
    Exit;
  end;

  if not FHandleHorzLeftDown and not FHandleHorzRightDown and not
    FHandleVertTopDown and not FHandleVertBottomDown then
  begin
    if FMouseDown then
      FList.DoMouseUp(Self, Button, Shift, X, Y)
  end
  else
  begin
    FHandleHorzLeftDown := false;
    FHandleHorzRightDown := false;
    FHandleVertTopDown := false;
    FHandleVertBottomDown := false;
    RefreshList;
  end;

  FMouseDown := false;
end;

procedure TCustomItemsContainer.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  i: integer;
begin
  if not(csDestroying in ComponentState) then
  begin
    if Assigned(List) then
    begin
      if Assigned(List.OnNotifyItemChange) then
        List.OnNotifyItemChange(Self);

      if (AComponent is TCustomItem) then
      begin
        if (AOperation = opRemove) then
        begin
          if List.Items.IndexOf(AComponent) <> -1 then
          begin
            List.Items.Extract(AComponent);
            if Assigned(List.OnNotifyItemDestroy) then
              List.OnNotifyItemDestroy(Self);
            Changed;
          end;
        end;
      end;
    end;
  end;

  if (AComponent is TBasicAction) then
  begin
    for i:= 0 to ItemCount-1 do
      if (AComponent = TActionCustomItem(Items[i]).Action) then
        TActionCustomItem(Items[i]).Action := nil;
  end;

  if (AOperation = opRemove) and (AComponent = PictureContainer) then
    PictureContainer := nil;

  if (AOperation = opRemove) and (AComponent = ImageList) then
    ImageList := nil;

  inherited;
end;

procedure TCustomItemsContainer.Paint;
var
  g: TGPGraphics;
  th: integer;
begin
  CalculateDPIScale(GetParentForm(Self), Canvas.Handle);

  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRendering);
  Draw(g);
  g.Free;

  if (csdesigning in ComponentState) and (List.Items.Count = 0) and ShowDesignTimeMessage then
  begin
    Canvas.Font.Assign(Self.Font);
    Canvas.Brush.Style := bsClear;
    th := Canvas.TextHeight('gh');
    Canvas.TextOut(10, Height div 2,
      'Double-click on the list or Right-click and choose "Edit"');
    Canvas.TextOut(10, (Height div 2) + th,
      'to add new items');

    Canvas.Font.Style := [fsItalic];
    Canvas.TextOut(10, Height div 2 + 3 * th,
      'If no such right-click menu option appears');
    Canvas.TextOut(10, Height div 2 + 4 * th,
      'please install designtime package!');
  end;
end;

procedure TCustomItemsContainer.ReadItemState(Reader: TReader;
  Item: TCustomItem);
begin
  if Reader.Parent = Self then
  begin
    if List.Items.IndexOf(Item) = -1 then
    begin
      Item.ItemOwner := Self;
      List.AssignEvents(Item);
      List.Items.Add(Item);
    end;
  end;
end;

procedure TCustomItemsContainer.RefreshList;
begin
  if FUpdateCount = 0 then
    invalidate;
end;

procedure TCustomItemsContainer.RemoveItem(Index: integer);
begin
  List.RemoveItem(Index);
end;

procedure TCustomItemsContainer.Resize;
begin
  inherited;
  VertScrollPos := VertScrollBar.Position;
  HorzScrollPos := HorzScrollBar.Position;
  Changed;
end;

procedure TCustomItemsContainer.Scrolling(Sender: TObject);
var
  pt: TPoint;
begin
  GetCursorPos(pt);
  pt := ScreenToClient(pt);
  with pt do
  begin
    if Y < 50 then
      perform(WM_VSCROLL, SB_LINEUP, 0)
    else if Y > Height - 50 then
      perform(WM_VSCROLL, SB_LINEDOWN, 0)
    else
      FScrollTimer.Enabled := false;
  end;
end;

procedure TCustomItemsContainer.ScrollToItem(Index: integer);
var
  it: TCustomItem;
  scrl: integer;
  f: integer;
begin
  it := nil;
  if (Index >= 0) and (Index <= List.Items.Count - 1) then
  begin
    if List.Items[Index] is TCustomItem then
      it := List.Items[Index] as TCustomItem;
  end;

  if Assigned(it) then
  begin
    if VertScrollVis then
    begin
      f := 0;
      if HorzScrollBar.Visible then
        f := GetSystemMetrics(SM_CXHSCROLL);

      if it.Y + it.Height > Height - f then
      begin
        scrl := it.Y - VertScrollBar.ThumbSize + VertScrollPos + it.Height +
          VerticalSpacing - 1 + f;
        VertScrollBar.Position := scrl;
        VertScrollPos := scrl;
        UpdateItemPositions;
      end
      else if it.Y < 0 then
      begin
        scrl := it.Y + VertScrollPos - VerticalSpacing + 2 - f;
        VertScrollBar.Position := scrl;
        VertScrollPos := scrl;
        UpdateItemPositions;
      end;
    end;

    if HorzScrollVis then
    begin
      f := 0;
      if VertScrollBar.Visible then
        f := GetSystemMetrics(SM_CXVSCROLL);

      if it.X + it.Width > Width - f then
      begin
        scrl := it.X - HorzScrollBar.ThumbSize + HorzScrollPos + it.Width +
          HorizontalSpacing - 1 + f;
        HorzScrollBar.Position := scrl;
        HorzScrollPos := scrl;
        UpdateItemPositions;
      end
      else if it.X < 0 then
      begin
        scrl := it.X + HorzScrollPos - HorizontalSpacing + 2 - f;
        HorzScrollBar.Position := scrl;
        HorzScrollPos := scrl;
        UpdateItemPositions;
      end;
    end;
  end;
end;

function TCustomItemsContainer.SelectedDragDropItem: TCustomItem;
begin
  Result := FDragDropItem;
end;

procedure TCustomItemsContainer.SelectItem(Index: integer);
begin
  List.SelectItem(Index);
end;

procedure TCustomItemsContainer.VisualizeItem(Index: integer;
  AllowScrollItem: Boolean = true; AllowSelectItem: Boolean = true);
begin
  if AllowSelectItem then
    List.SelectItem(Index);
  if AllowScrollItem then
    ScrollToItem(Index);
end;

procedure TCustomItemsContainer.SetAutoSizeMode(const Value: TAutoSizeMode);
begin
  if FAutoSizeMode <> Value then
  begin
    FAutoSizeMode := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetAutoSizeType(const Value: TAutoSizeType);
begin
  if FAutoSizeType <> Value then
  begin
    FAutoSizeType := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetBorderMode(const Value: TListBorderMode);
begin
  if FBorderMode <> Value then
  begin
    FBorderMode := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetBorderStyle(const Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomItemsContainer.SetBorderTypes(const Value: TListBorderTypes);
begin
  if FBorderTypes <> Value then
  begin
    FBorderTypes := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetColorTones(ATones: TColorTones);
var
  tmsif: ITMSTones;
  I: integer;
begin
  FMetroStyle := True;
  List.MetroStyle := True;
  Fill.Color := ATones.Background.BrushColor;
  Fill.ColorTo := ATones.Background.BrushColor;
  Fill.BorderColor := ATones.Background.BorderColor;
  DragLineColor := ATones.Selected.BorderColor;

  List.Appearance.Normal.Opacity := 255;
  List.Appearance.Normal.OpacityTo := 255;
  List.Appearance.Normal.OpacityMirror := 255;
  List.Appearance.Normal.OpacityMirrorTo := 255;

  List.Appearance.Hovered.Opacity := 255;
  List.Appearance.Hovered.OpacityTo := 255;
  List.Appearance.Hovered.OpacityMirror := 255;
  List.Appearance.Hovered.OpacityMirrorTo := 255;

  List.Appearance.Down.Opacity := 255;
  List.Appearance.Down.OpacityTo := 255;
  List.Appearance.Down.OpacityMirror := 255;
  List.Appearance.Down.OpacityMirrorTo := 255;

  List.Appearance.Disabled.Opacity := 255;
  List.Appearance.Disabled.OpacityTo := 255;
  List.Appearance.Disabled.OpacityMirror := 255;
  List.Appearance.Disabled.OpacityMirrorTo := 255;

  List.Appearance.Selected.Opacity := 255;
  List.Appearance.Selected.OpacityTo := 255;
  List.Appearance.Selected.OpacityMirror := 255;
  List.Appearance.Selected.OpacityMirrorTo := 255;

  List.Appearance.Normal.Glow := gmnone;
  List.Appearance.Hovered.Glow := gmnone;
  List.Appearance.Disabled.Glow := gmnone;
  List.Appearance.Selected.Glow := gmnone;
  List.Appearance.Down.Glow := gmnone;

  List.Appearance.Normal.Rounding := 0;
  List.Appearance.Hovered.Rounding := 0;
  List.Appearance.Disabled.Rounding := 0;
  List.Appearance.Selected.Rounding := 0;
  List.Appearance.Down.Rounding := 0;

  List.Appearance.Normal.Color := clNone;
  List.Appearance.Normal.ColorTo := clNone;
  List.Appearance.Normal.ColorMirror := clNone;
  List.Appearance.Normal.ColorMirrorTo := clNone;
  List.Appearance.Normal.BorderColor := clNone;
  List.Appearance.NormalFont.Color := ATones.Background.TextColor;

  List.Appearance.Hovered.Color := ATones.Hover.BrushColor;
  List.Appearance.Hovered.ColorTo := ATones.Hover.BrushColor;
  List.Appearance.Hovered.ColorMirror := clNone;
  List.Appearance.Hovered.ColorMirrorTo := clNone;
  List.Appearance.Hovered.BorderColor := ATones.Hover.BorderColor;
  List.Appearance.Hovered.GradientType := gtSolid;
  List.Appearance.HoveredFont.Color := ATones.Hover.TextColor;

  List.Appearance.Disabled.Color := ATones.Disabled.BrushColor;
  List.Appearance.Disabled.ColorTo := ATones.Disabled.BrushColor;
  List.Appearance.Disabled.ColorMirror := clNone;
  List.Appearance.Disabled.ColorMirrorTo := clNone;
  List.Appearance.DisabledFont.Color := ATones.Disabled.TextColor;
  List.Appearance.Disabled.BorderColor := ATones.Disabled.BorderColor;

  List.Appearance.Selected.Color := ATones.Selected.BrushColor;
  List.Appearance.Selected.ColorTo := ATones.Selected.BrushColor;
  List.Appearance.Selected.ColorMirror := clNone;
  List.Appearance.Selected.ColorMirrorTo := clNone;
  List.Appearance.Selected.BorderColor := ATones.Selected.BorderColor;
  List.Appearance.Selected.GradientType := gtSolid;
  List.Appearance.SelectedFont.Color := ATones.Selected.TextColor;

  List.Appearance.Down.Color := ATones.Selected.BrushColor;
  List.Appearance.Down.ColorTo := ATones.Selected.BrushColor;
  List.Appearance.Down.ColorMirror := clNone;
  List.Appearance.Down.ColorMirrorTo := clNone;
  List.Appearance.Down.BorderColor := ATones.Selected.BorderColor;
  List.Appearance.DownFont.Color := ATones.Selected.TextColor;

  List.Appearance.ButtonNormal.Assign(List.Appearance.Normal);
  List.Appearance.ButtonSelected.Assign(List.Appearance.Selected);
  List.Appearance.ButtonDown.Assign(List.Appearance.Down);
  List.Appearance.ButtonHovered.Assign(List.Appearance.Hovered);
  List.Appearance.ButtonDisabled.Assign(List.Appearance.Disabled);

  List.Appearance.ButtonNormal.Glow := gmNone;
  List.Appearance.ButtonDown.Glow := gmNone;
  List.Appearance.ButtonSelected.Glow := gmNone;
  List.Appearance.ButtonDisabled.Glow := gmNone;
  List.Appearance.ButtonHovered.Glow := gmNone;

  List.Appearance.ButtonNormal.Rounding := 0;
  List.Appearance.ButtonHovered.Rounding := 0;
  List.Appearance.ButtonDisabled.Rounding := 0;
  List.Appearance.ButtonSelected.Rounding := 0;
  List.Appearance.ButtonDown.Rounding := 0;

  for I := 0 to List.Items.Count - 1 do
  begin
    if List.Items[I].GetInterface(ITMSTones, tmsif) then
      tmsif.SetColorTones(ATones);
  end;
end;

procedure TCustomItemsContainer.SetColumns(const Value: integer);
begin
  if (FColumns <> Value) and (Value >= 0) then
  begin
    if FRows <> 0 then
      FRows := 0;

    FColumns := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetComponentStyle(AStyle: TTMSStyle);
var
  I: integer;
  tmsif: ITMSStyle;
begin
  FTMSStyle := AStyle;
  FMetroStyle := False;
  List.MetroStyle := False;

  // TODO : do color settings here
  List.Appearance.Normal.GradientMirrorType := gtVertical;
  List.Appearance.Normal.GradientType := gtVertical;


  List.Appearance.DownFont.Color := clBlack;
  List.Appearance.NormalFont.Color := clBlack;
  List.Appearance.HoveredFont.Color := clBlack;


  with List.Appearance do
  begin
    Normal.RoundingType := rtBoth;
    Normal.Glow := gmGradient;
    Disabled.RoundingType := rtBoth;
    Disabled.Glow := gmGradient;
    Hovered.RoundingType := rtBoth;
    Hovered.Glow := gmGradient;
    Down.RoundingType := rtBoth;
    Down.Glow := gmGradient;
    Selected.RoundingType := rtBoth;
    Selected.Glow := gmGradient;
  end;

  List.Appearance.HoveredFont.Color := clBlack;
  List.Appearance.SelectedFont.Color := clBlack;

  case AStyle of
    tsOffice2003Blue:
      begin
        Fill.Color := $00FFD2AF;
        Fill.ColorTo := $00FFD2AF;
        Fill.BorderColor := clNone;

        with List.Appearance do
        begin
          Normal.Color := $EEDBC8;
          Normal.ColorTo := $F6DDC9;
          Normal.ColorMirror := $EDD4C0;
          Normal.ColorMirrorTo := $F7E1D0;
          Normal.BorderColor := $E0B99B;
          Normal.GradientMirrorType := gtVertical;

          Hovered.Color := $EBFDFF;
          Hovered.ColorTo := $ACECFF;
          Hovered.ColorMirror := $59DAFF;
          Hovered.ColorMirrorTo := $A4E9FF;
          Hovered.BorderColor := $99CEDB;
          Hovered.GradientMirrorType := gtVertical;

          Selected.Color := $AAD9FF;
          Selected.ColorTo := $6EBBFF;
          Selected.ColorMirror := $42AEFE;
          Selected.ColorMirrorTo := $7AE1FE;
          Selected.BorderColor := $42AEFE;
          Selected.GradientMirrorType := gtVertical;

          Disabled.Color := $00F2F2F2;
          Disabled.ColorTo := $00B6B6B6;
          Disabled.ColorMirror := clNone;
          Disabled.ColorMirrorTo := clNone;
          DisabledFont.Color := clGray;
          Disabled.GradientMirrorType := gtVertical;

          Down.Color := $76AFF1;
          Down.ColorTo := $4190F3;
          Down.ColorMirror := $0E72F1;
          Down.ColorMirrorTo := $4C9FFD;
          Down.BorderColor := $45667B;
          Down.GradientMirrorType := gtVertical;
        end;
      end;
    tsOffice2003Silver:
      begin
        Fill.Color := $00E6D8D8;
        Fill.ColorTo := $00E6D8D8;
        Fill.BorderColor := clNone;

        with List.Appearance do
        begin
          Normal.Color := $E6E9E2;
          Normal.ColorTo := $00E6D8D8;
          Normal.ColorMirror := $C8B2B3;
          Normal.ColorMirrorTo := $E6E9E2;
          Normal.BorderColor := $927476;
          Normal.GradientMirrorType := gtVertical;

          Hovered.Color := $EBFDFF;
          Hovered.ColorTo := $ACECFF;
          Hovered.ColorMirror := $59DAFF;
          Hovered.ColorMirrorTo := $A4E9FF;
          Hovered.BorderColor := $99CEDB;
          Hovered.GradientMirrorType := gtVertical;

          Disabled.Color := $00F2F2F2;
          Disabled.ColorTo := $00B6B6B6;
          Disabled.ColorMirror := clNone;
          Disabled.ColorMirrorTo := clNone;
          DisabledFont.Color := clGray;
          Disabled.GradientMirrorType := gtVertical;

          Selected.Color := $AAD9FF;
          Selected.ColorTo := $6EBBFF;
          Selected.ColorMirror := $42AEFE;
          Selected.ColorMirrorTo := $7AE1FE;
          Selected.BorderColor := $42AEFE;
          Selected.GradientMirrorType := gtVertical;

          Down.Color := $76AFF1;
          Down.ColorTo := $4190F3;
          Down.ColorMirror := $0E72F1;
          Down.ColorMirrorTo := $4C9FFD;
          Down.BorderColor := $45667B;
          Down.GradientMirrorType := gtVertical;
        end;
      end;
    tsOffice2003Olive:
      begin
        Fill.Color := RGB(225, 234, 185);
        Fill.ColorTo := RGB(225, 234, 185);
        Fill.BorderColor := clNone;

        with List.Appearance do
        begin
          Normal.Color := $CFF0EA;
          Normal.ColorTo := $CFF0EA;
          Normal.ColorMirror := $8CC0B1;
          Normal.ColorMirrorTo := $CFF0EA;
          Normal.BorderColor := $8CC0B1;
          Normal.GradientMirrorType := gtVertical;

          Disabled.Color := $00F2F2F2;
          Disabled.ColorTo := $00B6B6B6;
          Disabled.ColorMirror := clNone;
          Disabled.ColorMirrorTo := clNone;
          DisabledFont.Color := clGray;
          Disabled.GradientMirrorType := gtVertical;

          Hovered.Color := $EBFDFF;
          Hovered.ColorTo := $ACECFF;
          Hovered.ColorMirror := $59DAFF;
          Hovered.ColorMirrorTo := $A4E9FF;
          Hovered.BorderColor := $99CEDB;
          Hovered.GradientMirrorType := gtVertical;

          Selected.Color := $AAD9FF;
          Selected.ColorTo := $6EBBFF;
          Selected.ColorMirror := $42AEFE;
          Selected.ColorMirrorTo := $7AE1FE;
          Selected.BorderColor := $42AEFE;
          Selected.GradientMirrorType := gtVertical;

          Down.Color := $76AFF1;
          Down.ColorTo := $4190F3;
          Down.ColorMirror := $0E72F1;
          Down.ColorMirrorTo := $4C9FFD;
          Down.BorderColor := $45667B;
          Down.GradientMirrorType := gtVertical;
        end;
      end;
    tsOffice2003Classic:
      begin
        Fill.Color := $00F2F2F2;
        Fill.ColorTo := $00F2F2F2;
        Fill.BorderColor := clNone;

        with List.Appearance do
        begin
          Normal.Color := clWhite;
          Normal.ColorTo := $C9D1D5;
          Normal.ColorMirror := clNone;
          Normal.ColorMirrorTo := clNone;
          Normal.BorderColor := $8CC0B1;
          Normal.GradientMirrorType := gtVertical;

          Hovered.Color := $D2BDB6;
          Hovered.ColorTo := $D2BDB6;
          Hovered.ColorMirror := clNone;
          Hovered.ColorMirrorTo := clNone;
          Hovered.BorderColor := $808080;
          Hovered.GradientMirrorType := gtVertical;

          Disabled.Color := $D8D5D4;
          Disabled.ColorTo := $D8D5D4;
          Disabled.ColorMirror := clNone;
          Disabled.ColorMirrorTo := clNone;
          DisabledFont.Color := clGray;
          Disabled.GradientMirrorType := gtVertical;

          Selected.Color := $B59285;
          Selected.ColorTo := $B59285;
          Selected.ColorMirror := clNone;
          Selected.ColorMirrorTo := clNone;
          Selected.BorderColor := $808080;
          Selected.GradientMirrorType := gtVertical;

          Down.Color := $D8D5D4;
          Down.ColorTo := $D8D5D4;
          Down.ColorMirror := clNone;
          Down.ColorMirrorTo := clNone;
          Down.BorderColor := $808080;
          Down.GradientMirrorType := gtVertical;
        end;
      end;
    tsOffice2007Luna:
      begin
        Fill.Color := $00F3E5DA;
        Fill.ColorTo := $00F0DED0;
        Fill.BorderColor := clNone;

        with List.Appearance do
        begin
          Normal.GradientType := gtSolid;

          Normal.Color := $FFEFE3;
          Normal.ColorTo := $FFDDC4;
          Normal.ColorMirror := $FFD1AD;
          Normal.ColorMirrorTo := $FFDBC0;
          Normal.BorderColor := $FFD1AD;
          Normal.GradientMirrorType := gtVertical;

          Hovered.Color := $EBFDFF;
          Hovered.ColorTo := $ACECFF;
          Hovered.ColorMirror := $59DAFF;
          Hovered.ColorMirrorTo := $A4E9FF;
          Hovered.BorderColor := $99CEDB;
          Hovered.GradientMirrorType := gtVertical;

          Disabled.Color := $00F2F2F2;
          Disabled.ColorTo := $00B6B6B6;
          Disabled.ColorMirror := $00B6B6B6;
          Disabled.ColorMirrorTo := $00F2F2F2;
          DisabledFont.Color := clGray;
          Disabled.GradientMirrorType := gtVertical;

          Selected.Color := $AAD9FF;
          Selected.ColorTo := $6EBBFF;
          Selected.ColorMirror := $42AEFE;
          Selected.ColorMirrorTo := $7AE1FE;
          Selected.BorderColor := $42AEFE;
          Selected.GradientMirrorType := gtVertical;

          Down.Color := $76AFF1;
          Down.ColorTo := $4190F3;
          Down.ColorMirror := $0E72F1;
          Down.ColorMirrorTo := $4C9FFD;
          Down.BorderColor := $45667B;
          Down.GradientMirrorType := gtVertical;
        end;
      end;
    tsOffice2007Obsidian:
      begin
        Fill.Color := $5C534C;
        Fill.ColorTo := $5C534C;
        Fill.BorderColor := clNone;

        with List.Appearance do
        begin
          Normal.Color := $F9F8F8;
          Normal.ColorTo := $E4E2DF;
          Normal.ColorMirror := $D1CBC7;
          Normal.ColorMirrorTo := $E2DEDB;
          Normal.BorderColor := $D1CBC7;
          Normal.GradientMirrorType := gtVertical;
          Normal.GradientType := gtVertical;

          Hovered.Color := $EBFDFF;
          Hovered.ColorTo := $ACECFF;
          Hovered.ColorMirror := $59DAFF;
          Hovered.ColorMirrorTo := $A4E9FF;
          Hovered.BorderColor := $99CEDB;
          Hovered.GradientMirrorType := gtVertical;

          Disabled.Color := $00F2F2F2;
          Disabled.ColorTo := $00B6B6B6;
          Disabled.ColorMirror := $00B6B6B6;
          Disabled.ColorMirrorTo := $00F2F2F2;
          DisabledFont.Color := clGray;
          Disabled.GradientMirrorType := gtVertical;

          Selected.Color := $AAD9FF;
          Selected.ColorTo := $6EBBFF;
          Selected.ColorMirror := $42AEFE;
          Selected.ColorMirrorTo := $7AE1FE;
          Selected.BorderColor := $42AEFE;
          Selected.GradientMirrorType := gtVertical;

          Down.Color := $76AFF1;
          Down.ColorTo := $4190F3;
          Down.ColorMirror := $0E72F1;
          Down.ColorMirrorTo := $4C9FFD;
          Down.BorderColor := $45667B;
          Down.GradientMirrorType := gtVertical;
        end;
      end;
    tsWindowsXP:
      begin
        Fill.Color := $00B6B6B6;
        Fill.ColorTo := $00B6B6B6;

        with List.Appearance do
        begin
          Normal.Color := clBtnFace; // clWhite;
          Normal.ColorTo := clBtnFace; // $B9D8DC;
          Normal.ColorMirror := clNone;
          Normal.ColorMirrorTo := clNone;
          Normal.BorderColor := $B9D8DC;
          Normal.GradientMirrorType := gtVertical;

          Disabled.Color := $00B6B6B6;
          Disabled.ColorTo := $00B6B6B6;
          Disabled.ColorMirror := clNone;
          Disabled.ColorMirrorTo := clNone;
          DisabledFont.Color := clGray;
          Disabled.GradientMirrorType := gtVertical;

          Hovered.Color := $EFD3C6;
          Hovered.ColorTo := $EFD3C6;
          Hovered.ColorMirror := clNone;
          Hovered.ColorMirrorTo := clNone;
          Hovered.BorderColor := clHighlight;
          Hovered.GradientMirrorType := gtVertical;

          Selected.Color := clInactiveCaption;
          Selected.ColorTo := clInactiveCaption;
          Selected.ColorMirror := clNone;
          Selected.ColorMirrorTo := clNone;
          Selected.BorderColor := clHighlight;
          Selected.GradientMirrorType := gtVertical;

          Down.Color := $B9D8DC;
          Down.ColorTo := $B9D8DC;
          Down.ColorMirror := clNone;
          Down.ColorMirrorTo := clNone;
          Down.BorderColor := clBlack;
          Down.GradientMirrorType := gtVertical;
        end;
      end;
    tsWhidbey:
      begin
        Fill.Color := $F5F9FA;
        Fill.ColorTo := $F5F9FA;
        Fill.BorderColor := clNone;

        with List.Appearance do
        begin
          Normal.Color := clWhite;
          Normal.ColorTo := $DFEDF0;
          Normal.ColorMirror := $DFEDF0;
          Normal.ColorMirrorTo := $DFEDF0;
          Normal.BorderColor := $99A8AC;
          Normal.GradientMirrorType := gtVertical;
          Normal.GradientType := gtVertical;

          Disabled.Color := $00F2F2F2;
          Disabled.ColorTo := $00B6B6B6;
          Disabled.ColorMirror := clNone;
          Disabled.ColorMirrorTo := clNone;
          DisabledFont.Color := clGray;
          Disabled.GradientMirrorType := gtVertical;

          Hovered.Color := $EBFDFF;
          Hovered.ColorTo := $ACECFF;
          Hovered.ColorMirror := $59DAFF;
          Hovered.ColorMirrorTo := $A4E9FF;
          Hovered.BorderColor := $99CEDB;
          Hovered.GradientMirrorType := gtVertical;

          Selected.Color := $AAD9FF;
          Selected.ColorTo := $6EBBFF;
          Selected.ColorMirror := $42AEFE;
          Selected.ColorMirrorTo := $7AE1FE;
          Selected.BorderColor := $42AEFE;
          Selected.GradientMirrorType := gtVertical;

          Down.Color := $76AFF1;
          Down.ColorTo := $4190F3;
          Down.ColorMirror := $0E72F1;
          Down.ColorMirrorTo := $4C9FFD;
          Down.BorderColor := $45667B;
          Down.GradientMirrorType := gtVertical;
        end;
      end;
    tsCustom:
      ;
    tsOffice2007Silver:
      begin
        Fill.Color := RGB(241, 244, 248);
        Fill.ColorTo := RGB(227, 232, 240);
        Fill.BorderColor := clNone;

        with List.Appearance do
        begin
          Normal.Color := $F9F8F8;
          Normal.ColorTo := $E4E2DF;
          Normal.ColorMirror := $D1CBC7;
          Normal.ColorMirrorTo := $E2DEDB;
          Normal.BorderColor := $D1CBC7;
          Normal.GradientMirrorType := gtVertical;

          Hovered.Color := $EBFDFF;
          Hovered.ColorTo := $ACECFF;
          Hovered.ColorMirror := $59DAFF;
          Hovered.ColorMirrorTo := $A4E9FF;
          Hovered.BorderColor := $99CEDB;
          Hovered.GradientMirrorType := gtVertical;

          Selected.Color := $AAD9FF;
          Selected.ColorTo := $6EBBFF;
          Selected.ColorMirror := $42AEFE;
          Selected.ColorMirrorTo := $7AE1FE;
          Selected.BorderColor := $42AEFE;
          Selected.GradientMirrorType := gtVertical;

          Disabled.Color := $00F2F2F2;
          Disabled.ColorTo := $00B6B6B6;
          Disabled.ColorMirror := $00B6B6B6;
          Disabled.ColorMirrorTo := $00F2F2F2;
          DisabledFont.Color := clGray;
          Disabled.GradientMirrorType := gtVertical;

          Down.Color := $76AFF1;
          Down.ColorTo := $4190F3;
          Down.ColorMirror := $0E72F1;
          Down.ColorMirrorTo := $4C9FFD;
          Down.BorderColor := $45667B;
          Down.GradientMirrorType := gtVertical;
        end;
      end;
    tsWindowsVista:
      begin
        Fill.Color := RGB(255, 255, 255);
        Fill.ColorTo := RGB(255, 255, 255);
        Fill.BorderColor := RGB(151, 151, 151);

        with List.Appearance do
        begin
          Normal.Color := $FFFFFF;
          Normal.ColorTo := $FFFFFF;
          Normal.ColorMirror := $FFFFFF;
          Normal.ColorMirrorTo := $FFFFFF;
          Normal.BorderColor := clSilver;
          Normal.GradientMirrorType := gtVertical;

          Disabled.Color := $FFFFFF;
          Disabled.ColorTo := $FFFFFF;
          Disabled.ColorMirror := clNone;
          Disabled.ColorMirrorTo := clNone;
          DisabledFont.Color := $646464;
          Disabled.GradientMirrorType := gtVertical;

          Hovered.Color := $FFFDF9;
          Hovered.ColorTo := $FFFAF0;
          Hovered.ColorMirror := clNone;
          Hovered.ColorMirrorTo := clNone;
          Hovered.BorderColor := $FCF2DA;
          Hovered.GradientMirrorType := gtVertical;

          Selected.Color := $FEF9F0;
          Selected.ColorTo := $FDF0D7;
          Selected.ColorMirror := clNone;
          Selected.ColorMirrorTo := clNone;
          Selected.BorderColor := $FEDF9A;
          Selected.GradientMirrorType := gtVertical;

          Down.Color := $FEF9F0;
          Down.ColorTo := $FDF0D7;
          Down.ColorMirror := clNone;
          Down.ColorMirrorTo := clNone;
          Down.BorderColor := $FEDF9AB;
          Down.GradientMirrorType := gtVertical;
        end;
      end;
    tsWindows7:
      begin
        Fill.Color := RGB(255, 255, 255);
        Fill.ColorTo := RGB(255, 255, 255);
        Fill.BorderColor := RGB(151, 151, 151);

        with List.Appearance do
        begin
          Normal.Color := $FFFFFF;
          Normal.ColorTo := $FFFFFF;
          Normal.ColorMirror := $FFFFFF;
          Normal.ColorMirrorTo := $FFFFFF;
          Normal.BorderColor := clSilver;
          Normal.GradientMirrorType := gtVertical;

          Disabled.Color := $FFFFFF;
          Disabled.ColorTo := $FFFFFF;
          Disabled.ColorMirror := clNone;
          Disabled.ColorMirrorTo := clNone;
          DisabledFont.Color := $646464;
          Disabled.GradientMirrorType := gtVertical;

          Hovered.Color := $FDFBFA;
          Hovered.ColorTo := $FDF3EB;
          Hovered.ColorMirror := clNone;
          Hovered.ColorMirrorTo := clNone;
          Hovered.BorderColor := $FBD6B8;
          Hovered.GradientMirrorType := gtVertical;

          Selected.Color := $FCEBDC;
          Selected.ColorTo := $FCDBC1;
          Selected.ColorMirror := clNone;
          Selected.ColorMirrorTo := clNone;
          Selected.BorderColor := $CEA27D;
          Selected.GradientMirrorType := gtVertical;

          Down.Color := $FEF9F0;
          Down.ColorTo := $FCDBC1;
          Down.ColorMirror := clNone;
          Down.ColorMirrorTo := clNone;
          Down.BorderColor := $CEA27D;
          Down.GradientMirrorType := gtVertical;
        end;
      end;
    tsTerminal:
      begin
        Fill.Color := clBtnFace;
        Fill.ColorTo := clBtnFace;
        Fill.BorderColor := clGray;

        with List.Appearance do
        begin
          Normal.Glow := gmNone;
          Disabled.Glow := gmNone;
          Hovered.Glow := gmNone;
          Down.Glow := gmNone;
          Selected.Glow := gmNone;
        end;

        with List.Appearance do
        begin
          Normal.Color := clBtnFace;
          Normal.ColorTo := clBtnFace;
          Normal.ColorMirror := clNone;
          Normal.ColorMirrorTo := clNone;
          Normal.BorderColor := clNone;

          Disabled.Color := clBtnFace;
          Disabled.ColorTo := clBtnFace;
          Disabled.ColorMirror := clNone;
          Disabled.ColorMirrorTo := clNone;
          DisabledFont.Color := clGray;

          Hovered.Color := clSilver;
          Hovered.ColorTo := clSilver;
          Hovered.ColorMirror := clNone;
          Hovered.ColorMirrorTo := clNone;
          Hovered.BorderColor := clGray;

          Selected.Color := clHighlight;
          Selected.ColorTo := clHighlight;
          Selected.ColorMirror := clNone;
          Selected.ColorMirrorTo := clNone;
          Selected.BorderColor := clGray;

          Down.Color := clBtnFace;
          Down.ColorTo := clBtnFace;
          Down.ColorMirror := clNone;
          Down.ColorMirrorTo := clNone;
          Down.BorderColor := clHighlight;
        end;
      end;
          tsWindows8, tsWindows10:
            begin
              Fill.Color := clWhite;
              Fill.ColorTo := clNone;
              Fill.BorderColor := $E4E3E2;

            with List.Appearance do
            begin
              Normal.RoundingType := rtNone;
              Normal.Glow := gmNone;
              Disabled.RoundingType := rtNone;
              Disabled.Glow := gmNone;
              Hovered.RoundingType := rtNone;
              Hovered.Glow := gmNone;
              Down.RoundingType := rtNone;
              Down.Glow := gmNone;
              Selected.RoundingType := rtNone;
              Selected.Glow := gmNone;
              SelectedFont.Color := clwhite;

           end;

              with List.Appearance do
              begin
                Normal.Color := $F7F6F5;
                Normal.ColorTo := clNone;
                Normal.ColorMirror := clNone;
                Normal.ColorMirrorTo := clNone;
                Normal.BorderColor := $E4E3E2;
                Normal.BorderColor := clNone;
                Normal.GradientMirrorType := gtVertical;

                Disabled.Color := $F7F7F7;
                Disabled.ColorTo := clNone;
                Disabled.ColorMirror := clNone;
                Disabled.ColorMirrorTo := clNone;
                DisabledFont.Color := clWhite;
                Disabled.BorderColor := $DEDEDE;
                Disabled.GradientMirrorType := gtVertical;

                Hovered.Color := $F7EFE8;
                Hovered.ColorTo := clNone;
                Hovered.ColorMirror := clNone;
                Hovered.ColorMirrorTo := clNone;
                Hovered.BorderColor := $F9CEA4;
                Hovered.GradientMirrorType := gtVertical;

                Selected.Color := $DAA026;
                Selected.ColorTo := clNone;
                Selected.ColorMirror := clNone;
                Selected.ColorMirrorTo := clNone;
                Selected.BorderColor := $DAA026;
                Selected.GradientMirrorType := gtVertical;

                Down.Color := $F7E0C9;
                Down.ColorTo := clNone;
                Down.ColorMirror := clNone;
                Down.ColorMirrorTo := clNone;
                Down.BorderColor := $E4A262;
                Down.GradientMirrorType := gtVertical;
              end;
            end;
          tsOffice2013White:
            begin
              List.MetroStyle := True;
              Fill.Color := clWhite;
              Fill.ColorTo := clNone;
              Fill.BorderColor := $D4D4D4;

            with List.Appearance do
            begin
              Normal.RoundingType := rtNone;
              Normal.Glow := gmNone;
              Disabled.RoundingType := rtNone;
              Disabled.Glow := gmNone;
              Hovered.RoundingType := rtNone;
              Hovered.Glow := gmNone;
              Down.RoundingType := rtNone;
              Down.Glow := gmNone;
              Selected.RoundingType := rtNone;
              Selected.Glow := gmNone;
              SelectedFont.Color := clwhite;
           end;

              with List.Appearance do
              begin

                Normal.Color := clWhite;
                Normal.ColorTo := clNone;
                Normal.ColorMirror := clNone;
                Normal.ColorMirrorTo := clNone;
                Normal.BorderColor := $D4D4D4;
                Normal.BorderColor := clNone;
                Normal.GradientMirrorType := gtVertical;

                Disabled.Color := $EEEEEE;
                Disabled.ColorTo := clNone;
                Disabled.ColorMirror := clNone;
                Disabled.ColorMirrorTo := clNone;
                DisabledFont.Color := clWhite;
                Disabled.BorderColor := $ACACAC;
                Disabled.GradientMirrorType := gtVertical;

                Hovered.Color := $FCF0E4;
                Hovered.ColorTo := clNone;
                Hovered.ColorMirror := clNone;
                Hovered.ColorMirrorTo := clNone;
                Hovered.BorderColor := $EAB47E;
                Hovered.GradientMirrorType := gtVertical;

                Selected.Color := $FF9933;
                Selected.ColorTo := clNone;
                Selected.ColorMirror := clNone;
                Selected.ColorMirrorTo := clNone;
                Selected.BorderColor := $FF9933;
                Selected.GradientMirrorType := gtVertical;

                Down.Color := $FCE2C8;
                Down.ColorTo := clNone;
                Down.ColorMirror := clNone;
                Down.ColorMirrorTo := clNone;
                Down.BorderColor := $E59D56;
                Down.GradientMirrorType := gtVertical;
              end;
            end;
          tsOffice2013LightGray:
            begin
              List.MetroStyle := True;
              Fill.Color := clWhite;
              Fill.ColorTo := clNone;
              Fill.BorderColor := $C6C6C6;

            with List.Appearance do
            begin
              Normal.RoundingType := rtNone;
              Normal.Glow := gmNone;
              Disabled.RoundingType := rtNone;
              Disabled.Glow := gmNone;
              Hovered.RoundingType := rtNone;
              Hovered.Glow := gmNone;
              Down.RoundingType := rtNone;
              Down.Glow := gmNone;
              Selected.RoundingType := rtNone;
              Selected.Glow := gmNone;
              SelectedFont.Color := clwhite;
           end;

              with List.Appearance do
              begin
                Normal.Color := $F6F6F6;
                Normal.ColorTo := clNone;
                Normal.ColorMirror := clNone;
                Normal.ColorMirrorTo := clNone;
                Normal.BorderColor := $C6C6C6;
                Normal.BorderColor := clNone;
                Normal.GradientMirrorType := gtVertical;

                Disabled.Color := $EEEEEE;
                Disabled.ColorTo := clNone;
                Disabled.ColorMirror := clNone;
                Disabled.ColorMirrorTo := clNone;
                DisabledFont.Color := clWhite;
                Disabled.BorderColor := $ACACAC;
                Disabled.GradientMirrorType := gtVertical;

                Hovered.Color := $FCF0E4;
                Hovered.ColorTo := clNone;
                Hovered.ColorMirror := clNone;
                Hovered.ColorMirrorTo := clNone;
                Hovered.BorderColor := $EAB47E;
                Hovered.GradientMirrorType := gtVertical;

                Selected.Color := $FF9933;
                Selected.ColorTo := clNone;
                Selected.ColorMirror := clNone;
                Selected.ColorMirrorTo := clNone;
                Selected.BorderColor := $FF9933;
                Selected.GradientMirrorType := gtVertical;

                Down.Color := $FCE2C8;
                Down.ColorTo := clNone;
                Down.ColorMirror := clNone;
                Down.ColorMirrorTo := clNone;
                Down.BorderColor := $E59D56;
                Down.GradientMirrorType := gtVertical;
              end;
            end;
          tsOffice2013Gray:
            begin
              List.MetroStyle := True;
              Fill.Color := clWhite;
              Fill.ColorTo := clNone;
              Fill.BorderColor := $ABABAB;

            with List.Appearance do
            begin

              Normal.RoundingType := rtNone;
              Normal.Glow := gmNone;
              Disabled.RoundingType := rtNone;
              Disabled.Glow := gmNone;
              Hovered.RoundingType := rtNone;
              Hovered.Glow := gmNone;
              Down.RoundingType := rtNone;
              Down.Glow := gmNone;
              Selected.RoundingType := rtNone;
              Selected.Glow := gmNone;
              SelectedFont.Color := clwhite;
           end;

              with List.Appearance do
              begin
                Normal.Color := $E5E5E5;
                Normal.ColorTo := clNone;
                Normal.ColorMirror := clNone;
                Normal.ColorMirrorTo := clNone;
                Normal.BorderColor := $ABABAB;
                Normal.BorderColor := clNone;
                Normal.GradientMirrorType := gtVertical;

                Disabled.Color := $EEEEEE;
                Disabled.ColorTo := clNone;
                Disabled.ColorMirror := clNone;
                Disabled.ColorMirrorTo := clNone;
                DisabledFont.Color := clWhite;
                Disabled.BorderColor := $ACACAC;
                Disabled.GradientMirrorType := gtVertical;

                Hovered.Color := $FCF0E4;
                Hovered.ColorTo := clNone;
                Hovered.ColorMirror := clNone;
                Hovered.ColorMirrorTo := clNone;
                Hovered.BorderColor := $EAB47E;
                Hovered.GradientMirrorType := gtVertical;

                Selected.Color := $FF9933;
                Selected.ColorTo := clNone;
                Selected.ColorMirror := clNone;
                Selected.ColorMirrorTo := clNone;
                Selected.BorderColor := $FF9933;
                Selected.GradientMirrorType := gtVertical;

                Down.Color := $FCE2C8;
                Down.ColorTo := clNone;
                Down.ColorMirror := clNone;
                Down.ColorMirrorTo := clNone;
                Down.BorderColor := $E59D56;
                Down.GradientMirrorType := gtVertical;
              end;
            end;
          tsOffice2016White:
            begin
              List.MetroStyle := True;
              Fill.Color := clWhite;
              Fill.ColorTo := clNone;
              Fill.BorderColor := $D4D4D4;

            with List.Appearance do
            begin
              Normal.RoundingType := rtNone;
              Normal.Glow := gmNone;
              Disabled.RoundingType := rtNone;
              Disabled.Glow := gmNone;
              Hovered.RoundingType := rtNone;
              Hovered.Glow := gmNone;
              Down.RoundingType := rtNone;
              Down.Glow := gmNone;
              Selected.RoundingType := rtNone;
              Selected.Glow := gmNone;
              SelectedFont.Color := clBlack;
           end;

              with List.Appearance do
              begin

                Normal.Color := clWhite;
                Normal.ColorTo := clNone;
                Normal.ColorMirror := clNone;
                Normal.ColorMirrorTo := clNone;
                Normal.BorderColor := $D4D4D4;
                Normal.BorderColor := clNone;
                Normal.GradientMirrorType := gtVertical;

                Disabled.Color := clWhite;
                Disabled.ColorTo := clNone;
                Disabled.ColorMirror := clNone;
                Disabled.ColorMirrorTo := clNone;
                DisabledFont.Color := clWhite;
                Disabled.BorderColor := $D4D4D4;
                Disabled.GradientMirrorType := gtVertical;

                Hovered.Color := $F2E1D5;
                Hovered.ColorTo := clNone;
                Hovered.ColorMirror := clNone;
                Hovered.ColorMirrorTo := clNone;
                Hovered.BorderColor := $F2E1D5;
                Hovered.GradientMirrorType := gtVertical;

                Selected.Color := $F2D5C2;
                Selected.ColorTo := clNone;
                Selected.ColorMirror := clNone;
                Selected.ColorMirrorTo := clNone;
                Selected.BorderColor := $F2D5C2;
                Selected.GradientMirrorType := gtVertical;

                Down.Color := $E3BDA3;
                Down.ColorTo := clNone;
                Down.ColorMirror := clNone;
                Down.ColorMirrorTo := clNone;
                Down.BorderColor := $E3BDA3;
                Down.GradientMirrorType := gtVertical;
              end;
            end;
          tsOffice2016Gray:
            begin
              List.MetroStyle := True;
              Fill.Color := $B2B2B2;
              Fill.ColorTo := clNone;
              Fill.BorderColor := $444444;

            with List.Appearance do
            begin
              Normal.RoundingType := rtNone;
              Normal.Glow := gmNone;
              Disabled.RoundingType := rtNone;
              Disabled.Glow := gmNone;
              Hovered.RoundingType := rtNone;
              Hovered.Glow := gmNone;
              Down.RoundingType := rtNone;
              Down.Glow := gmNone;
              Selected.RoundingType := rtNone;
              Selected.Glow := gmNone;
              SelectedFont.Color := clBlack;
           end;

              with List.Appearance do
              begin
                Normal.Color := $B2B2B2;
                Normal.ColorTo := clNone;
                Normal.ColorMirror := clNone;
                Normal.ColorMirrorTo := clNone;
                Normal.BorderColor := $444444;
                Normal.BorderColor := clNone;
                Normal.GradientMirrorType := gtVertical;

                Disabled.Color := $B2B2B2;
                Disabled.ColorTo := clNone;
                Disabled.ColorMirror := clNone;
                Disabled.ColorMirrorTo := clNone;
                DisabledFont.Color := clWhite;
                Disabled.BorderColor := $444444;
                Disabled.GradientMirrorType := gtVertical;

                Hovered.Color := $F2E1D5;
                Hovered.ColorTo := clNone;
                Hovered.ColorMirror := clNone;
                Hovered.ColorMirrorTo := clNone;
                Hovered.BorderColor := $F2E1D5;
                Hovered.GradientMirrorType := gtVertical;

                Selected.Color := $F2D5C2;
                Selected.ColorTo := clNone;
                Selected.ColorMirror := clNone;
                Selected.ColorMirrorTo := clNone;
                Selected.BorderColor := $F2D5C2;
                Selected.GradientMirrorType := gtVertical;

                Down.Color := $E3BDA3;
                Down.ColorTo := clNone;
                Down.ColorMirror := clNone;
                Down.ColorMirrorTo := clNone;
                Down.BorderColor := $E3BDA3;
                Down.GradientMirrorType := gtVertical;
              end;
            end;
          tsOffice2016Black:
            begin
              List.MetroStyle := True;
              Fill.Color := $363636;
              Fill.ColorTo := clNone;
              Fill.BorderColor := $444444;

            with List.Appearance do
            begin

              Normal.RoundingType := rtNone;
              Normal.Glow := gmNone;
              Disabled.RoundingType := rtNone;
              Disabled.Glow := gmNone;
              Hovered.RoundingType := rtNone;
              Hovered.Glow := gmNone;
              Down.RoundingType := rtNone;
              Down.Glow := gmNone;
              Selected.RoundingType := rtNone;
              Selected.Glow := gmNone;
              SelectedFont.Color := $FFFFFF;
              DownFont.Color := $FFFFFF;
              NormalFont.Color := $FFFFFF;
              HoveredFont.Color := $FFFFFF;
           end;

              with List.Appearance do
              begin
                Normal.Color := $363636;
                Normal.ColorTo := clNone;
                Normal.ColorMirror := clNone;
                Normal.ColorMirrorTo := clNone;
                Normal.BorderColor := $444444;
                Normal.BorderColor := clNone;
                Normal.GradientMirrorType := gtVertical;

                Disabled.Color := $363636;
                Disabled.ColorTo := clNone;
                Disabled.ColorMirror := clNone;
                Disabled.ColorMirrorTo := clNone;
                DisabledFont.Color := clWhite;
                Disabled.BorderColor := $444444;
                Disabled.GradientMirrorType := gtVertical;

                Hovered.Color := $6A6A6A;
                Hovered.ColorTo := clNone;
                Hovered.ColorMirror := clNone;
                Hovered.ColorMirrorTo := clNone;
                Hovered.BorderColor := $6A6A6A;
                Hovered.GradientMirrorType := gtVertical;

                Selected.Color := $575757;
                Selected.ColorTo := clNone;
                Selected.ColorMirror := clNone;
                Selected.ColorMirrorTo := clNone;
                Selected.BorderColor := $575757;
                Selected.GradientMirrorType := gtVertical;

                Down.Color :=  $444444;
                Down.ColorTo := clNone;
                Down.ColorMirror := clNone;
                Down.ColorMirrorTo := clNone;
                Down.BorderColor := $444444;
                Down.GradientMirrorType := gtVertical;
              end;
            end;
    tsOffice2010Blue, tsOffice2010Silver, tsOffice2010Black:
      begin
        if FIsAppMenu then
        begin
          FFill.Color := clWhite;
          FFill.ColorTo := clWhite;
          FFill.BorderColor := clSilver;

          ApplyDefaultItemStyle;
        end
        else
        begin
          case AStyle of
          tsOffice2010Blue:
            begin
              Fill.Color := $EDDBCD;
              Fill.ColorTo := clNone;
              Fill.BorderColor := clNone;

              with List.Appearance do
              begin
                Normal.Color := $EDDBCD;
                Normal.ColorTo := clNone;
                Normal.ColorMirror := clNone;
                Normal.ColorMirrorTo := clNone;
                Normal.BorderColor := $8CC0B1;
                Normal.GradientMirrorType := gtVertical;

                Disabled.Color := $DEC1A9;
                Disabled.ColorTo := clNone;
                Disabled.ColorMirror := clNone;
                Disabled.ColorMirrorTo := clNone;
                DisabledFont.Color := clWhite; // $C0C0C0;
                Disabled.BorderColor := $588060;
                Disabled.GradientMirrorType := gtVertical;

                Hovered.Color := $FFEBDB;
                Hovered.ColorTo := clNone;
                Hovered.ColorMirror := clNone;
                Hovered.ColorMirrorTo := clNone;
                Hovered.BorderColor := clNone;
                Hovered.GradientMirrorType := gtVertical;

                Selected.Color := $DEC1A9;
                Selected.ColorTo := clNone;
                Selected.ColorMirror := clNone;
                Selected.ColorMirrorTo := clNone;
                Selected.BorderColor := clNone;
                Selected.GradientMirrorType := gtVertical;

                Down.Color := $EDDBCD;
                Down.ColorTo := clNone;
                Down.ColorMirror := clNone;
                Down.ColorMirrorTo := clNone;
                Down.BorderColor := $055CC9;
                Down.GradientMirrorType := gtVertical;
              end;
            end;
          tsOffice2010Silver:
            begin
              Fill.Color := $EDE9E5;
              Fill.ColorTo := clNone;
              Fill.BorderColor := clNone;

              with List.Appearance do
              begin
                Normal.Color := $EDE9E5;
                Normal.ColorTo := clNone;
                Normal.ColorMirror := clNone;
                Normal.ColorMirrorTo := clNone;
                Normal.BorderColor := $8CC0B1;
                Normal.GradientMirrorType := gtVertical;

                Disabled.Color := $DEC1A9;
                Disabled.ColorTo := clNone;
                Disabled.ColorMirror := clNone;
                Disabled.ColorMirrorTo := clNone;
                DisabledFont.Color := clWhite; // $C0C0C0;
                Disabled.BorderColor := $588060;
                Disabled.GradientMirrorType := gtVertical;

                Hovered.Color := $EEDDCF;
                Hovered.ColorTo := clNone;
                Hovered.ColorMirror := clNone;
                Hovered.ColorMirrorTo := clNone;
                Hovered.BorderColor := clNone;
                Hovered.GradientMirrorType := gtVertical;

                Selected.Color := $DEC1A9;
                Selected.ColorTo := clNone;
                Selected.ColorMirror := clNone;
                Selected.ColorMirrorTo := clNone;
                Selected.BorderColor := clNone;
                Selected.GradientMirrorType := gtVertical;

                Down.Color := $EDE9E5;
                Down.ColorTo := clNone;
                Down.ColorMirror := clNone;
                Down.ColorMirrorTo := clNone;
                Down.BorderColor := $055CC9;
                Down.GradientMirrorType := gtVertical;
              end;
            end;
          tsOffice2010Black:
            begin
              Fill.Color := $828282;
              Fill.ColorTo := clNone;
              Fill.BorderColor := clNone;

              with List.Appearance do
              begin
                Normal.Color := $828282;
                Normal.ColorTo := clNone;
                Normal.ColorMirror := clNone;
                Normal.ColorMirrorTo := clNone;
                Normal.BorderColor := $8CC0B1;
                Normal.BorderColor := clNone;
                Normal.GradientMirrorType := gtVertical;

                Disabled.Color := $A3A3A3;
                Disabled.ColorTo := clNone;
                Disabled.ColorMirror := clNone;
                Disabled.ColorMirrorTo := clNone;
                DisabledFont.Color := clWhite;
                Disabled.BorderColor := $588060;
                Disabled.GradientMirrorType := gtVertical;

                Hovered.Color := $EEDDCF;
                Hovered.ColorTo := clNone;
                Hovered.ColorMirror := clNone;
                Hovered.ColorMirrorTo := clNone;
                Hovered.BorderColor := clNone;
                Hovered.GradientMirrorType := gtVertical;

                Selected.Color := $DEC1A9;
                Selected.ColorTo := clNone;
                Selected.ColorMirror := clNone;
                Selected.ColorMirrorTo := clNone;
                Selected.BorderColor := clNone;
                Selected.GradientMirrorType := gtVertical;

                Down.Color := $828282;
                Down.ColorTo := clNone;
                Down.ColorMirror := clNone;
                Down.ColorMirrorTo := clNone;
                Down.BorderColor := $055CC9;
                Down.GradientMirrorType := gtVertical;
              end;
            end;
          end;
        end;
      end;
  end;

  if (AStyle = tsWindows8) or (AStyle = tsOffice2013White) or (AStyle = tsOffice2013LightGray) or (AStyle = tsOffice2013Gray)
  or (AStyle = tsWindows10) or (AStyle = tsOffice2016White) or (AStyle = tsOffice2016Gray) or (AStyle = tsOffice2016Black) then
    Self.List.Appearance.ButtonNormal.Assign(Self.List.Appearance.Normal);

  Self.List.Appearance.ButtonDown.Assign(Self.List.Appearance.Down);
  Self.List.Appearance.ButtonSelected.Assign(Self.List.Appearance.Selected);
  Self.List.Appearance.ButtonDisabled.Assign(Self.List.Appearance.Disabled);
  Self.List.Appearance.ButtonHovered.Assign(Self.List.Appearance.Hovered);

  case AStyle of
    tsOffice2013Gray, tsOffice2013LightGray, tsOffice2013White:
    begin
      Self.List.Appearance.ButtonNormal.Color := $FDFDFD;
      Self.List.Appearance.ButtonNormal.BorderColor := $ABABAB;

      Self.List.Appearance.Hovered.Color := $F2E1D5;
      Self.List.Appearance.Hovered.BorderColor := $F2E1D5;
      Self.List.Appearance.Selected.Color := $B56D3E;
      Self.List.Appearance.Selected.BorderColor := $B56D3E;
      Self.List.Appearance.Down.Color := $B56D3E;
      Self.List.Appearance.Down.BorderColor := $B56D3E;
    end;
  end;

  for I := 0 to List.Items.Count - 1 do
  begin
    if List.Items[I].GetInterface(ITMSStyle, tmsif) then
      tmsif.SetComponentStyle(AStyle);
  end;
end;

procedure TCustomItemsContainer.SetCursorEx(const Value: TCursor);
begin
  inherited Cursor := Value;
  if not keepoldcursor then
    FoldCursor := Value;
end;

procedure TCustomItemsContainer.SetDragLine(const Value: Boolean);
begin
  if FDragLine <> Value then
  begin
    FDragLine := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetDragLineColor(const Value: TColor);
begin
  if FDragLineColor <> Value then
  begin
    FDragLineColor := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetHandleAppearance
  (const Value: THandleAppearance);
begin
  if FHandleAppearance <> Value then
  begin
    FHandleAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetHorizontalSpacing(const Value: integer);
begin
  if FHorizontalSpacing <> Value then
  begin
    FHorizontalSpacing := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetHorzScrollPos(const Value: integer);
begin
  FHorzScrollPos := Max(0, Min(Value,
    HorzScrollBar.Range - HorzScrollBar.ThumbSize));
  if Assigned(OnHorizontalScroll) then
    OnHorizontalScroll(Self, HorzScrollPos);
end;

procedure TCustomItemsContainer.SetHTMLCache(const Value: Boolean);
begin
  if FHTMLCache <> Value then
  begin
    FHTMLCache := Value;
    if Assigned(List) then
      List.HTMLCache := FHTMLCache;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetImageList(const Value: TCustomImageList);
begin
  if Assigned(List) then
    List.Appearance.ImageList := Value;
  Changed;
end;

procedure TCustomItemsContainer.SetIsMainMenu(const Value: Boolean);
begin
  if FIsMainMenu <> Value then
  begin
    FIsMainMenu := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetItem(Index: integer;
  const Value: TCustomItem);
begin
  List.Items[index] := Value;
end;

procedure TCustomItemsContainer.SetList(const Value: TCustomBaseList);
begin
  if FList <> Value then
  begin
    FList.Assign(Value);
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetListMargins(const Value: TMargins);
begin
  if FListMargins <> Value then
  begin
    FListMargins.Assign(Value);
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetPictureContainer
  (const Value: TGDIPPictureContainer);
begin
  if Assigned(List) then
    List.Appearance.PictureContainer := Value;
  Changed;
end;

procedure TCustomItemsContainer.SetItemParentComponent(AParent: TComponent;
  Item: TCustomItem);
begin
  if AParent = Self then
  begin
    if List.Items.IndexOf(Item) = -1 then
    begin
      Item.ItemOwner := Self;
      List.AssignEvents(Item);
      List.Items.Add(Item);
    end;
  end;
end;

procedure TCustomItemsContainer.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetReadorder(const Value: Boolean);
begin
  if FReorder <> Value then
  begin
    FReorder := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetRows(const Value: integer);
begin
  if (FRows <> Value) and (Value >= 0) then
  begin
    if FColumns <> 0 then
      FColumns := 0;

    FRows := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetScrollType(const Value: TScrollType);
begin
  if FScrollType <> Value then
  begin
    FScrollType := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetShowDesignTimeMessage(const Value: Boolean);
begin
  if FShowDesignTimeMessage <> Value then
  begin
    FShowDesignTimeMessage := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetTextRenderingHint(
  const Value: TTextRenderingHint);
begin
  if FTextRenderingHint <> Value then
  begin
    FTextRenderingHint := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetThumbTracking(const Value: Boolean);
begin
  if FThumbTracking <> Value then
  begin
    FThumbTracking := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetVerticalSpacing(const Value: integer);
begin
  if FVerticalSpacing <> Value then
  begin
    FVerticalSpacing := Value;
    Changed;
  end;
end;

procedure TCustomItemsContainer.SetVertScrollPos(const Value: integer);
begin
  FVertScrollPos := Max(0, Min(Value,
      VertScrollBar.Range - VertScrollBar.ThumbSize));
  if Assigned(OnVerticalScroll) then
    OnVerticalScroll(Self, VertScrollPos);
end;

procedure TCustomItemsContainer.ShowMenuShortCuts;
begin
  ShowShortCutHints;
end;

procedure TCustomItemsContainer.ShowShortCutHints;
begin
  List.ShowShortCutHints;
  FShortCutItemHintShowing := List.Items.Count > 0;
  FShortCutChars := '';
end;

procedure TCustomItemsContainer.UnSelectItem(Index: integer);
begin
  List.UnSelectItem(Index);
end;

procedure TCustomItemsContainer.UpdateControl;
var
  totalr, totalc: integer;
begin
  if ((AutoSizeType = astWidth) or (AutoSizeType = astBoth)) then
  begin
    totalr := GetTotalRowSize;
    Width := totalr + 1;
  end;

  if ((AutoSizeType = astHeight) or (AutoSizeType = astBoth)) then
  begin
    totalc := GetTotalColumnSize;
    Height := totalc + 1;
  end;
end;

procedure TCustomItemsContainer.UpdateItemPositions;
var
  I: integer;
  itx, ity: integer;
  it: TCustomItem;
  pX, pY: Double;
  controlr: TGPRectF;
  chk: Boolean;
  row, col: integer;
  startx, starty: integer;
  vspc, hspc: integer;
  itNext: TCustomItem;
begin
  if FPosListUpdating then
    Exit;

  FPosListUpdating := true;

  BeginUpdate;
  vspc := VerticalSpacing;
  hspc := HorizontalSpacing;
  startx := ListMargins.Left - HorzScrollPos;
  starty := ListMargins.Top - VertScrollPos;
  itx := startx;
  ity := starty;

  row := 0;
  col := 0;

  for I := 0 to List.Items.Count - 1 do
  begin
    if (List.Items[I] is TCustomItem) then
    begin
      it := TCustomItem(List.Items[I]);
      if it.Visible then
      begin
        if (Columns = 0) and (Rows = 0) then
        begin
          it.X := it.X - (HorzScrollPos - FOldScrollHPos);
          it.Y := it.Y - (VertScrollPos - FOldScrollVPos);
        end
        else
        begin
          it.X := itx;
          it.Y := ity;

          if (Columns = 0) and not(Rows = 0) then
          begin
            ity := ity + it.Height + vspc;
            Inc(row);

            if row = Rows then
            begin
              row := 0;
              ity := starty;
              itx := itx + it.Width + hspc;
            end;
          end
          else if (Rows = 0) and not (Columns = 0) then
          begin
            itx := itx + it.Width + hspc;
            col := col + it.ColumnSpan;

            if (col >= Columns) then
            begin
              col := 0;
              itx := startx;
              ity := ity + it.Height + vspc;
            end;

            if it.Index < ItemCount - 1 then
            begin
              itNext := Items[it.Index + 1];
              col := col + (itNext.ColumnSpan-1);
              if col > Columns then
              begin
                col := 0;
                itx := startx;
                ity := ity + it.Height + vspc;
              end;
            end;
          end;
        end;

        if (it.ItemOwner is TCustomItem) then
          chk := not(csDesigning in ComponentState) and Assigned(it.Control)
        else
          chk := Assigned(it.Control);

        if chk then
        begin
          if it.ControlStretched then
          begin
            it.Control.Left := it.X + it.ControlMargin.Left;
            it.Control.Top := it.Y + it.ControlMargin.Top;
            it.Control.Width := it.Width - it.ControlMargin.Left -
              it.ControlMargin.Right;
            it.Control.Height := it.Height - it.ControlMargin.Top -
              it.ControlMargin.Bottom;
          end
          else if it.ControlLocation <> tlCustom then
          begin
            pX := 0;
            pY := 0;
            controlr := MakeRect(it.X + it.ControlMargin.Left,
              it.Y + it.ControlMargin.Top,
              it.Width - it.ControlMargin.Left - it.ControlMargin.Right,
              it.Height - it.ControlMargin.Top - it.ControlMargin.Bottom);
            GetObjectLocation(pX, pY, controlr, it.Control.Width,
              it.Control.Height, it.ControlLocation);
            it.Control.Left := Round(pX) + it.X + it.ControlMargin.Left;
            it.Control.Top := Round(pY) + it.Y + it.ControlMargin.Top;
          end;
        end;
      end;
    end;
  end;
  FOldScrollHPos := HorzScrollPos;
  FOldScrollVPos := VertScrollPos;
  List.UpdateVisibleIndexes;
  EndUpdate;
  FPosListUpdating := false;
end;

procedure TCustomItemsContainer.UpdateItems;
var
  I: integer;
  it: TCustomItem;
  row, col: integer;
  vspc, hspc: integer;
  rowCount, colCount, s: integer;
  cntc, cntr: integer;
  c: Integer;
  f: integer;
  itNext: TCustomItem;
begin
  vspc := VerticalSpacing;
  hspc := HorizontalSpacing;

  row := 0;
  col := 0;
  rowCount := 0;
  colCount := 0;

  if (Rows > 0) or (Columns > 0) then
  begin
    for I := 0 to List.Items.Count - 1 do
    begin
      if (List.Items[I] is TCustomItem) then
      begin
        it := TCustomItem(List.Items[I]);
        it.RowIndex := -1;
        it.ColumnIndex := -1;
//        if it.Visible then
        begin
          it.RowIndex := rowCount;
          it.ColumnIndex := colCount;

          if (Columns = 0) then
          begin
            Inc(row);
            Inc(rowCount);

            if row = Rows then
            begin
              row := 0;
              rowCount := 0;
              Inc(colCount);
            end;
          end
          else if (Rows = 0) then
          begin
            col := col + it.ColumnSpan;
            colCount := colCount + it.ColumnSpan;
            if col >= Columns then
            begin
              col := 0;
              colCount := 0;
              Inc(rowCount);
            end;

            if it.Index < ItemCount - 1 then
            begin
              itNext := Items[it.Index + 1];
              col := col + (itNext.ColumnSpan-1);
              if col >= Columns then
              begin
                col := 0;
                colCount := 0;
                Inc(rowCount);
              end;
            end;
          end;
        end;
      end;
    end;

    cntc := List.ColumnCount;
    cntr := List.rowCount;
    for I := 0 to List.Items.Count - 1 do
    begin
      if (List.Items[I] is TCustomItem) then
      begin
        it := TCustomItem(List.Items[I]);
        if it.Visible then
        begin
          case AutoSizeMode of
            asmItems:
              begin
                if (AutoSizeType = astWidth) or (AutoSizeType = astBoth) then
                begin
                  if cntc > 0 then
                  begin
                    f := 0;
                    if VertScrollBar.Visible then
                      f := GetSystemMetrics(SM_CXVSCROLL);

                    s := Width - GetShadowOffset - ListMargins.Right -
                      ListMargins.Left - f - 1;

                    c := Min(cntc, cntc-(it.ColumnSpan - 1));
                    if c > 0 then
                      it.Width := ( s - (hspc * (cntc - 1))) div c;
                  end;
                end;

                if (AutoSizeType = astHeight) or (AutoSizeType = astBoth) then
                begin
                  if cntr > 0 then
                  begin
                    f := 0;
                    if HorzScrollBar.Visible then
                      f := GetSystemMetrics(SM_CXHSCROLL);

                    s := Height - GetShadowOffset - ListMargins.Bottom -
                      ListMargins.Top - f - 1;
                    it.Height := (s - (vspc * (cntr - 1))) div cntr;
                  end;
                end;
              end;
          end;
        end;
      end;
    end;
  end
  else
  begin
    for I := 0 to List.Items.Count - 1 do
    begin
      if (List.Items[I] is TCustomItem) then
      begin
        it := TCustomItem(List.Items[I]);
        it.RowIndex := -1;
        it.ColumnIndex := -1;
        if it.Visible then
        begin
          it.RowIndex := I;
          it.ColumnIndex := I;
        end;
      end;
    end;
  end;

  UpdateItemPositions;
end;

procedure TCustomItemsContainer.UpdateList;
begin
  if (List.Items.Count = 0) then
    Exit;

  if FListUpdating then
    Exit;

  FListUpdating := true;
  FList.BeginUpdate;

  UpdateItems;

  case AutoSizeMode of
    asmControl:
      begin
        UpdateControl;
      end;
  end;

  UpdateScrollBars;
  UpdateItems;

  FList.Items.UpdateIndexes;

  FList.EndUpdate;

  FListUpdating := false;
end;

procedure TCustomItemsContainer.UpdateMenu;
begin
  //
end;

procedure TCustomItemsContainer.UpdateScrollBars;
var
  totalr, totalc: integer;
  w, h: integer;
begin
  totalr := GetTotalRowSize;
  totalc := GetTotalColumnSize;

  w := Width;
  if (totalr > w) and (w > 0) and (totalr > 0) and
    ((((AutoSizeType = astHeight) or (AutoSizeType = astBoth)) and
        (AutoSizeMode = asmItems)) or (AutoSizeMode = asmNone)) then
  begin
    HorzScrollBar.ThumbSize := w;
    HorzScrollBar.Range := totalr + 1;
    case ScrollType of
      stScrollers:
        HorzScrollBar.Visible := not Transparent;
      stHandles:
        HorzScrollBar.Visible := false;
    end;
    HorzScrollVis := true;
    FHorzScrollPos := Min(FHorzScrollPos, HorzScrollBar.Range);
    if HandleAllocated then
      HorzScrollBar.Position := FHorzScrollPos;
  end
  else
  begin
    HorzScrollBar.Visible := false;
    HorzScrollVis := false;
    HorzScrollPos := 0;
  end;

  h := Height;
  if (totalc > h) and (h > 0) and (totalc > 0) and
    ((((AutoSizeType = astWidth) or (AutoSizeType = astBoth)) and
        (AutoSizeMode = asmItems)) or (AutoSizeMode = asmNone)) then
  begin
    VertScrollBar.ThumbSize := h;
    VertScrollBar.Range := totalc + 1;
    case ScrollType of
      stScrollers:
        VertScrollBar.Visible := not Transparent;
      stHandles:
        VertScrollBar.Visible := false;
    end;
    VertScrollVis := true;
    FVertScrollPos := Min(FVertScrollPos, VertScrollBar.Range);
    if HandleAllocated then
      VertScrollBar.Position := FVertScrollPos;
  end
  else
  begin
    VertScrollBar.Visible := false;
    VertScrollVis := false;
    VertScrollPos := 0;
  end;
end;

procedure TCustomItemsContainer.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if not Transparent then
  begin
    inherited;
    Exit;
  end;

  {$IFDEF DELPHI2006_LVL}
  inherited;
  {$ENDIF}
  {$IFNDEF DELPHI2006_LVL}
  message.Result := 1;
  {$ENDIF}
end;

procedure TCustomItemsContainer.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  // DLGC_WANTALLKEYS makes receive the ESC/Return keys
  Message.Result :=  DLGC_WANTALLKEYS or DLGC_WANTARROWS;

  if TabStop then
  begin
    if Assigned(List) then
      if List.GetCountFocusableItems > 0 then
        Message.Result := Message.Result or DLGC_WANTTAB
  end;
end;

procedure TCustomItemsContainer.WMHScroll(var Message: TWMHScroll);
begin
  inherited;
  if (Message.ScrollCode = SB_THUMBTRACK) and (ThumbTracking) then
    HorzScrollBar.Position := Message.Pos;

  HorzScrollPos := HorzScrollBar.Position;

  Changed;
end;

procedure TCustomItemsContainer.WMKeyDown(var Message: TWMKeyDown);
var
  itfocus: integer;
begin
  inherited;
  if ReadOnly or (List.Items.Count = 0) then
    Exit;

  itfocus := List.FocusedItemIndex;
  List.DoWMKeyDown(Message);
  if itfocus <> List.FocusedItemIndex then
    ScrollToItem(List.FocusedItemIndex);
end;

procedure TCustomItemsContainer.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  FFocusKilled := True;
  Changed;
end;

procedure TCustomItemsContainer.WMMouseWheel(var Message: TWMMouseWheel);
begin
  inherited;
  if VertScrollVis then
  begin
    case Message.Keys of
      0:
      begin
        VertScrollPos := Min(VertScrollPos - Message.WheelDelta, VertScrollBar.Range);
        VertScrollBar.Position := VertScrollPos;
      end;
    end;
    UpdateItemPositions;
  end;
end;

procedure TCustomItemsContainer.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  {$IFNDEF DELPHI_UNICODE}
  dbl: boolean;
  {$ENDIF}
  p: TPoint;
  i: integer;
begin
  if not Transparent then
  begin
    ControlState := ControlState + [csCustomPaint];
    inherited;
    ControlState := ControlState - [csCustomPaint];
    Exit;
  end
  else
    ControlState := ControlState + [csCustomPaint];

  if Assigned(Parent) {and (Fill.ShadowOffset > 0) ?} then
  begin
    DC := Message.DC;
    if DC <> 0 then
    begin
      {$IFNDEF DELPHI_UNICODE}
      dbl := Parent.DoubleBuffered;
      Parent.DoubleBuffered := false;
      {$ENDIF}
      i := SaveDC(DC);
      p := ClientOrigin;
      Windows.ScreenToClient(Parent.Handle, p);
      p.x := -p.x;
      p.y := -p.y;
      MoveWindowOrg(DC, p.x, p.y);
      SendMessage(Parent.Handle, WM_ERASEBKGND, DC, 0);
      SendMessage(Parent.Handle, WM_PAINT, DC, 0);
      if (Parent is TWinCtrl) then
        (Parent as TWinCtrl).PaintCtrls(DC, nil);
      RestoreDC(DC, i);
      {$IFNDEF DELPHI_UNICODE}
      Parent.DoubleBuffered := dbl;
      {$ENDIF}
    end;
  end;

  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      Perform(WM_ERASEBKGND, MemDC, MemDC);
      Message.DC := MemDC;
      WMPaint(Message);
      Message.DC := 0;
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;

  ControlState := ControlState - [csCustomPaint];
end;

procedure TCustomItemsContainer.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Changed;
end;

procedure TCustomItemsContainer.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
  if (Message.ScrollCode = SB_THUMBTRACK) and (ThumbTracking) then
    VertScrollBar.Position := Message.Pos;

  VertScrollPos := VertScrollBar.Position;

  Changed;
end;

{ TScrollContainer }

constructor TScrollContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TScrollContainer.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TScrollContainer.Paint;
begin
  //
end;

procedure TScrollContainer.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      TControlCanvas(FCanvas).UpdateTextFlags;
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

{ THandleAppearance }

procedure THandleAppearance.Assign(Source: TPersistent);
begin
  if (Source is THandleAppearance) then
  begin
    FOpacity := (Source as THandleAppearance).Opacity;
    FBorderColor := (Source as THandleAppearance).BorderColor;
    FColor := (Source as THandleAppearance).Color;
    FSize := (Source as THandleAppearance).Size;
    FArrowColor := (Source as THandleAppearance).ArrowColor;
  end;
end;

procedure THandleAppearance.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor THandleAppearance.Create(AOwner: TCustomItemsContainer);
begin
  FOwner := AOwner;
  FColor := clSilver;
  FOpacity := 200;
  FBorderColor := clSilver;
  FSize := 35;
  FArrowColor := clBlack;
end;

destructor THandleAppearance.Destroy;
begin
  inherited;
end;

procedure THandleAppearance.SetArrowColor(const Value: TColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    Changed;
  end;
end;

procedure THandleAppearance.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure THandleAppearance.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure THandleAppearance.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

procedure THandleAppearance.SetSize(const Value: integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    Changed;
  end;
end;

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

{$IFDEF DELPHIXE2_LVL}
initialization
  TCustomStyleEngine.RegisterStyleHook(TScrollContainer, TScrollBoxStyleHook);

finalization
  TCustomStyleEngine.UnRegisterStyleHook(TScrollContainer, TScrollBoxStyleHook);
{$ENDIF}

end.
