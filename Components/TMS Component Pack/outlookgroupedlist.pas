{*************************************************************************}
{ TMS AdvOutlookList component                                            }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2005 - 2014                                       }
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

unit OutlookGroupedList;

interface

{$HPPEMIT ' '}
{$HPPEMIT '#pragma link "msimg32.lib"'}
{$HPPEMIT ' '}

{$I TMSDEFS.INC}

uses
  Windows, Messages, Classes, Forms, Controls,
  Graphics, ImgList, ActiveX, PictureContainer
  , Types
   {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}

  ;
 
type
  TOGLItemState = (
    nsGroup,       // it's a group item
    nsClearing,    // an items's children are being deleted. don't reindex & invalidate OGL
    nsExpanding,   // all Group items is expanding
    nsCollapsing,  // all Group items is collapsing
    nsExpanded,    // set if the item is expanded
    nsSelected     // set if the item is in the current selection
  );
  TOGLItemStates = set of TOGLItemState;

  TOGLAutoOption = (
    toAutoExpandOnCreate,   // Group items are expanded when creating
    toAutoExpandOnFocus,    // Group items are expanded when getting the focus
    toAutoExpandOnDrop,     // expand Group item if it is the drop target
    toAutoScrollOnExpand,   // scroll as many Child items in view as possible after expanding an item
    toAutoSort,             // sort OGL when SortColumn or SortDirection change
                            // or sort item if child items are added
    toAutoDeleteMovedItems  // delete items which where moved in a drag operation (if not directed otherwise)
  );
  TOGLAutoOptions = set of TOGLAutoOption;

  TOGLDragType = (
    dtOLE,
    dtVCL
  );

  TOGLDragOption = (
    doOLEAcceptDrop,    // register OGL as OLE accepting drop target
    doOLEAllowDrag,     // allow to drag OLE data from OGL
    doOLEShowDragImage  // show Drag image during OLE-drag operation
  );
  TOGLDragOptions = set of TOGLDragOption;

  TOGLSelectionOption = (
    soMultiSelect,        // allow more than one item to be selected.
    soRightClickSelect    // allow selection, etc. with the right mouse button.
  );
  TOGLSelectionOptions = set of TOGLSelectionOption;

  // operations basically allowed during drag'n drop
  TOGLDragOperation = (doCopy, doMove, doLink);
  TOGLDragOperations = set of TOGLDragOperation;

const
  DefaultAutoOptions = [toAutoExpandOnCreate, toAutoScrollOnExpand,
    toAutoSort, toAutoDeleteMovedItems];

  DefaultDragOptions = [doOLEAcceptDrop, doOLEAllowDrag, doOLEShowDragImage];

  DefaultSelectionOptions = [soMultiSelect, soRightClickSelect];

  DefaultDragOperations = [doCopy];

  // special identifiers for columns
  NoColumn = -1;
  InvalidColumn = -2;

  // drag image helpers for OS >= W2k
  IID_IDragSourceHelper: TGUID = (
    D1: $DE5BF786; D2: $477A; D3: $11D2;
    D4: ($83, $9D, $00, $C0, $4F, $D9, $18, $D0));
  IID_IDropTargetHelper: TGUID = (
    D1: $4657278B; D2: $411B; D3: $11D2;
    D4: ($83, $9A, $00, $C0, $4F, $D9, $18, $D0));
  IID_IDropTarget: TGUID = (
    D1: $00000122; D2: $0000; D3: $0000;
    D4: ($C0, $00, $00, $00, $00, $00, $00, $46));
  CLSID_DragDropHelper: TGUID = (
    D1: $4657278A; D2: $411B; D3: $11D2;
    D4: ($83, $9A, $00, $C0, $4F, $D9, $18, $D0));

  SID_IDragSourceHelper = '{DE5BF786-477A-11D2-839D-00C04FD918D0}';
  SID_IDropTargetHelper = '{4657278B-411B-11D2-839A-00C04FD918D0}';
  SID_IDropTarget = '{00000122-0000-0000-C000-000000000046}';

type
  TOutlookGroupedList = class;
  TOutlookGroupedListClass = class of TOutlookGroupedList;

  {$IFDEF DELPHIXE2_LVL}
  TTagInt = nativeint;
  {$ENDIF}
  {$IFNDEF DELPHIXE2_LVL}
  TTagInt = integer;
  {$ENDIF}


  POGLItem = ^TOGLItem;
  TOGLItem = packed record
    Index: Cardinal;              // index of item with regard to its parent
    ChildCount: Cardinal;         // number of child items
    ChildSelectedCount: Cardinal; // number of selected child items
    ChildHeight: Cardinal;        // height of child items in pixels
    TotalCount: Cardinal;         // sum of this item, all of its child items and their child items etc.
    Dummy: Byte;                  // dummy value
    States: TOGLItemStates;       // states describing various properties of the item (expanded, selected etc.)
    Parent: POGLItem;             // reference to the item's parent (for the root this contains the OGL)
    PrevSibling: POGLItem;        // link to the item's previous sibling or nil if it is the first item
    NextSibling: POGLItem;        // link to the item's next sibling or nil if it is the last item
    FirstChild: POGLItem;         // link to the item's first child
    LastChild: POGLItem;          // link to the item's last child
    ItemObject: TObject;          // reference to Object that can be stored with item
    Tag: TTagInt;                 // tag for item
    GroupObject: TObject;         // reference to Object for group item
    Data: record end;             // item's extra data determined by ItemDataSize
  end;

  TOGLItemArray = array of POGLItem;

  // item's hit test result
  TOGLItemHitTest = (
    htInClientArea,        // over client area
    htOnExpandButton,      // on the +/- button of the Group item
    htOnItem,              // on the client area of the Group or Child item
    htOnItemLabel          // on the "Caption" text area of the item
  );

  // used when need hit test info of any position in the OGL
  TOGLItemHitInfo = packed record
    GroupItem: POGLItem;
    HitItem: POGLItem;
    HitTest: TOGLItemHitTest;
  end;

  // TOGLThumbnailOptions ------------------------------------------------------
  // used when OGL.ViewStyle = vsThumbnails
  
  TOGLThumbnailOptions = class(TPersistent)
  private
    FOwner: TOutlookGroupedList;

    FHorizSpacing: Byte;
    FVertSpacing: Byte;
    FThumbnailWidth: Word;
    FThumbnailHeight: Word;

    procedure SetHorizSpacing(const Value: Byte);
    procedure SetVertSpacing(const Value: Byte);
    procedure SetThumbnailWidth(const Value: Word);
    procedure SetThumbnailHeight(const Value: Word);
  public
    constructor Create(AOwner: TOutlookGroupedList);
  published
    property HorizSpacing: Byte
      read FHorizSpacing write SetHorizSpacing default 12;
    property VertSpacing: Byte
      read FVertSpacing write SetVertSpacing default 12;
    property ThumbnailWidth: Word
      read FThumbnailWidth write SetThumbnailWidth default 96;
    property ThumbnailHeight: Word
      read FThumbnailHeight write SetThumbnailHeight default 96;
  end;

  // TOGLHintOptions -----------------------------------------------------------

  TOGLHintOptions = class(TPersistent)
  private
    FAlphaBlend: Boolean;
    FAlphaBlendValue: Byte;
  public
    constructor Create;
  published
    property AlphaBlend: Boolean
      read FAlphaBlend write FAlphaBlend default True;
    property AlphaBlendValue: Byte
      read FAlphaBlendValue write FAlphaBlendValue default 240;
  end;

  // TOGLHintWindow ------------------------------------------------------------
  
  POGLHintData = ^TOGLHintData;
  TOGLHintData = packed record
    OutlookGroupedList: TOutlookGroupedList;
    HitInfo: TOGLItemHitInfo;
  end;

  TOGLHintWindow = class(THintWindow)
  private
    FBmpBlend: TBitmap;
    FHintData: TOGLHintData;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMShowWindow(var Message: TWMShowWindow); message WM_SHOWWINDOW;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(Rect: TRect; const AHint: String); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: String;
      AData: Pointer): TRect; override;
    function IsHintMsg(var Msg: TMsg): Boolean; override;
  end;

  // TOGLEnumFormatEtc ---------------------------------------------------------
  // used to enumerate an array of FORMATETC structures. 

  {$IFNDEF DELPHIXE2_LVL}
  TFormatEtcArray = array of TFormatEtc;
  {$ENDIF}
  TClipFormatArray = array of TClipFormat;

  TOGLEnumFormatEtc = class(TInterfacedObject, IEnumFormatEtc)
  private
    FOutlookGroupedList: TOutlookGroupedList;
    FFormatEtcArray: TFormatEtcArray;
    FIndex: Integer;
    function GetFormatCount: Integer;
  protected
    property FormatCount: Integer read GetFormatCount;
    property Index: Integer read FIndex write FIndex;
  public
    constructor Create(const AOutlookGroupedList: TOutlookGroupedList;
      const AFormatEtcArray: TFormatEtcArray; const AIndex: Integer = 0);
    destructor Destroy; override;
    // IEnumFormatEtc
    function Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumFormatEtc): HResult; stdcall;
  end;

  // TOGLDataObject ------------------------------------------------------------
  // used for data transfer and notification of changes in data

  TOGLDataObject = class(TInterfacedObject, IDataObject)
  private
    FOwner: TOutlookGroupedList;
    FFormatEtcArray: TFormatEtcArray;
  protected
    function EqualFormatEtc(const FormatEtc1, FormatEtc2: TFormatEtc): Boolean;

    property Owner: TOutlookGroupedList read FOwner;
    property FormatEtcArray: TFormatEtcArray
      read FFormatEtcArray write FFormatEtcArray;
  public
    constructor Create(AOwner: TOutlookGroupedList); virtual;
    destructor Destroy; override;

    // IDataObject
    function GetData(const FormatEtcIn: TFormatEtc;
      out medium: TStgMedium): HResult; stdcall;
    function GetDataHere(const FormatEtcIn: TFormatEtc;
      out medium: TStgMedium): HResult; stdcall;
    function QueryGetData(const FormatEtcIn: TFormatEtc): HResult; stdcall;
    function GetCanonicalFormatEtc(const FormatEtcIn: TFormatEtc;
      out FormatEtcOut: TFormatEtc): HResult; stdcall;
    function SetData(const FormatEtcIn: TFormatEtc;
      var medium: TStgMedium; fRelease: BOOL): HResult; stdcall;
    function EnumFormatEtc(dwDirection: Longint;
      out EnumFormatEtc: IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const FormatEtcIn: TFormatEtc; advf: Longint;
      const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
    function DUnadvise(dwConnection: Longint): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
  end;
  
  // contains the information needed to create a drag image

  PSHDragImage = ^TSHDragImage;
  TSHDragImage = packed record
    sizeDragImage: TSize;
    ptOffset: TPoint;
    hbmpDragImage: HBITMAP;
    crColorKey: TColorRef;
  end;

  (* This interface allows drop targets to display a drag image while
     the image is over the target window. *)

  {$M-}   
  IDropTargetHelper = interface(IUnknown)
    [SID_IDropTargetHelper]
    function DragEnter(hwndTarget: HWND; pDataObject: IDataObject;
      var ppt: TPoint; dwEffect: Longint): HRESULT; stdcall;
    function DragLeave: HRESULT; stdcall;
    function DragOver(var ppt: TPoint; dwEffect: Longint): HRESULT; stdcall;
    function Drop(pDataObject: IDataObject; var ppt: TPoint;
      dwEffect: Longint): HRESULT; stdcall;
    function Show(fShow: BOOL): HRESULT; stdcall;
  end;

  (* This interface is exposed by the Shell to allow an application to specify
     the image that will be displayed during a Shell drag-and-drop operation. *)

  IDragSourceHelper = interface(IUnknown)
    [SID_IDragSourceHelper]
    function InitializeFromBitmap(var SHDragImage: TSHDragImage;
      pDataObject: IDataObject): HRESULT; stdcall;
    function InitializeFromWindow(Window: HWND; var ppt: TPoint;
      pDataObject: IDataObject): HRESULT; stdcall;
  end;

  // custom interface used in OGL as DragEngine object

  IOGLDragEngine = interface(IUnknown)
    ['{39DE72C5-56D8-4E40-BABC-CBBCFA7B0D0A}']
    procedure ForceDragLeave; stdcall;
    function GetDataObject: IDataObject; stdcall;
    function GetDropActive: Boolean; stdcall;

    property DataObject: IDataObject read GetDataObject;
    property DropActive: Boolean read GetDropActive;
  end;

  // TOGLDragEngine ------------------------------------------------------------
  // used to manage OLE DragNDrop operations

  TOGLDragEngine = class(TInterfacedObject, IOGLDragEngine,
    IDropSource, IDropTarget)
  private
    FOwner: TOutlookGroupedList;
    FDropActive: Boolean;                 // True if drop operation is active
    FDataObject: IDataObject;             // a reference to the data object passed in by DragEnter
    FDropTargetHelper: IDropTargetHelper; // Display a drag image when OS >= Win2k

    // IOGLDragManager
    function GetDataObject: IDataObject; stdcall;
    function GetDropActive: Boolean; stdcall;
  public
    constructor Create(AOwner: TOutlookGroupedList); virtual;
    destructor Destroy; override;

    // IDropSource
    function QueryContinueDrag(fEscapePressed: BOOL;
      KeyState: Longint): HResult; stdcall;
    function GiveFeedback(dwEffect: Longint): HResult; stdcall;
    
    // IDropTarget
    function DragEnter(const DataObject: IDataObject; KeyState: Longint;
      Pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragOver(KeyState: Longint; Pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const DataObject: IDataObject; KeyState: Longint; Pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;

    // IOGLDragManager
    procedure ForceDragLeave; stdcall;
  end;

  // TOutlookGroupedList -------------------------------------------------------

  TDragDropMode = (ddmNormal, ddmCopy);  
  TOGLViewStyle = (vsList, vsThumbnails);

  TOGLMoveFocusDirection = (
    mfdHome, mfdPageUp, mfdUp, mfdLeft,
    mfdRight, mfdDown, mfdPageDown, mfdEnd, mfdUnknown
  );

  TOGLDrawSelectionMode = (
    dsmDottedRectangle,       // same as DrawFocusRect
    dsmBlendedRectangle,      // alpha blending, uses special colors
    dsmThemeAware             // alpha blending only if Themes enabled
  );

  TOGLStates = set of (
    lsHintShowed,             // Set when our hint is visible.
    lsMouseSelecting,         // Multiselection only. Mouse selection has actually started.
    lsMouseSelected,          // Multiselection only. Mouse selection has been really started.
    lsToggleFocusedSelection, // Item selection was modifed using Ctrl-click. Change selection state on next mouse up.
    lsLeftButtonDown,         // Set when the left mouse button is down.
    lsRightButtonDown,        // Set when the right mouse button is down.
    lsSelectionClearing,      // Need to clear the current selection on next mouse move.
    lsSearchPending,          // set in WM_KEYDOWN to tell to use the char in WM_CHAR for hot char search
    lsOLEDragging,            // OLE dragging in progress.
    lsOLEDragPending,         // User has requested to start delayed dragging.
    lsVCLDragging,            // VCL drag'n drop in progress.
    lsVCLDragPending          // One-shot flag to avoid clearing the current selection on implicit mouse up for VCL drag.
  );

  TOGLSortColumn = -2..High(Word);

  TOGLSortDirection = (
    sdAscending,
    sdDescending,
    sdDefault
  );

  TOGLSearchType = (
    stNone,                   // disable searching
    stAll,                    // search every items in OGL
    stChildOnly               // search Child items in OGL
  );

  TOGLItemEvent = procedure(Sender: TOutlookGroupedList;
    Item: POGLItem) of object;
  TOGLItemClickEvent = procedure(Sender: TOutlookGroupedList; Item: POGLItem; X, Y: Integer) of object;
  TOGLDrawItemEvent = procedure(Sender: TOutlookGroupedList;
    ItemCanvas: TCanvas; ItemRect: TRect;
    Item: POGLItem) of object;
  TOGLDrawHintEvent = procedure(Sender: TOutlookGroupedList;
    HintCanvas: TCanvas; HintRect: TRect;
    Item: POGLItem) of object;
  TOGLGetCaptionEvent = procedure(Sender: TOutlookGroupedList;
    Item: POGLItem; var Caption: String) of object;
  TOGLGetHintEvent = procedure(Sender: TOutlookGroupedList;
    Item: POGLItem; var HintText: String; var HintPos: TPoint) of object;
  TOGLGetHintSizeEvent = procedure(Sender: TOutlookGroupedList;
    Item: POGLItem; var HintRect: TRect) of object;
  TOGLGetImageIndexEvent = procedure(Sender: TOutlookGroupedList;
    Item: POGLItem; var ImageIndex: Integer) of object;
  TOGLGetChildItemHeightEvent = procedure(Sender: TOutlookGroupedList;
    const OGLCanvas: TCanvas; var ItemHeight: Word) of object;
  TOGLCompareItemsEvent = procedure(Sender: TOutlookGroupedList;
    Item1, Item2: POGLItem; Column: TOGLSortColumn; var Result: Integer) of object;
  TOGLSearchItemEvent = procedure(Sender: TOutlookGroupedList;
    Item: POGLItem; const SearchText: WideString;
    var Result: Integer) of object;

  TOGLMouseButtonEvent = procedure(Sender: TOutlookGroupedList; Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
    HitInfo: TOGLItemHitInfo) of object;

  // DragNDrop OLE events
  TOGLGetDataObjectEvent = procedure(Sender: TOutlookGroupedList;
    out IDataObject: IDataObject) of object;
  TOGLDragOverEvent = procedure(Sender: TOutlookGroupedList;
    const DataObject: IDataObject; Shift: TShiftState;
    Pt: TPoint; State: TDragState; var Effect: Integer;
    var Accept: Boolean) of object;
  TOGLDropEvent = procedure(Sender: TOutlookGroupedList;
    const DataObject: IDataObject; Shift: TShiftState;
    Pt: TPoint; Formats: TClipFormatArray; var Effect: Integer) of object;
  TOGLGetDataEvent = procedure(Sender: TOutlookGroupedList;
    const FormatEtcIn: TFormatEtc; out Medium: TStgMedium;
    var Result: HRESULT) of object;
  TOGLGetClipboardFormatsEvent = procedure(Sender: TOutlookGroupedList;
    var Formats: TFormatEtcArray) of object;
  TOGLDragAllowedEvent = procedure(Sender: TOutlookGroupedList;
    Item: POGLItem; var Allowed: Boolean) of object;

  TOutlookGroupedList = class(TScrollingWinControl)
  private
    FBorderStyle: TBorderStyle;
    FAutoOptions: TOGLAutoOptions;
    FHintOptions: TOGLHintOptions;
    FThumbnailOptions: TOGLThumbnailOptions;
    FSelectionOptions: TOGLSelectionOptions;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FStates: TOGLStates;
    FSortColumn: TOGLSortColumn;
    FSortDirection: TOGLSortDirection;
    FSearchType: TOGLSearchType;
    FViewStyle: TOGLViewStyle;

    FUpdateCount: Integer;
    FUpdateSelCount: Integer;
    FOGLItemColumn: Integer;         // the focused OGL item's column
    FTotalItemsHeight: Cardinal;     // the height of all Group items + their OGL items (if group expanded)
    FItemDataSize: Integer;          // number of bytes to allocate with each item
    FRootItem: POGLItem;
    FFocusedItem: POGLItem;
    FSelectedAnchorItem: POGLItem;
    FHintData: TOGLHintData;         // used while preparing the hint window
    FLastHintRect: TRect;
    FLastHintHitInfo: TOGLItemHitInfo;
    FSelection: TList;               // the list of selected items
    FFromSelPoint: TPoint;           // used while drawing selection
    FToSelPoint: TPoint;
    FDrawSelectionMode: TOGLDrawSelectionMode;
    FThumbnailCaptionHeight: Byte;   // the height of each caption of Child item 
    FItemsPerRow: Word;              // the OGL items count within each row
    FDrawSelShiftState: TShiftState;
    FGroupItemExpandButtonVertOffset: Byte;
    FGroupItemHeight: Word;
    FChildItemHeight: Word;

    FBmpPlus: TBitmap;                   // small bitmaps used for "+" buttons
    FBmpMinus: TBitmap;                  // small bitmaps used for "-" buttons
    FBmpCanvas: TBitmap;                 // bitmap used as temporary canvas

    // DragNDrop and clipboard support
    FDragEngine: IOGLDragEngine;         // DragNDrop engine
    FOLEDragEnterAccept: Boolean;        // whether the OLE-Drop can be accepted
    FDropTargetGroup: POGLItem;          // item currently selected as drop target

    FDragType: TOGLDragType;             // specify the Drag type: VCL or OLE
    FDragOptions: TOGLDragOptions;
    FDragImageWidth: Word;               // width of dragging image, OS >= W2k
    FDragImageHeight: Word;              // height of dragging image, OS >= W2k
    FDragOperations: TOGLDragOperations; // supported operations on OLE-Drag
    FDragThreshold: Integer;

    // main events
    FOnInitItem: TOGLItemEvent;
    FOnFreeItem: TOGLItemEvent;
    FOnDrawItem: TOGLDrawItemEvent;
    FOnDrawHint: TOGLDrawHintEvent;
    FOnGetCaption: TOGLGetCaptionEvent;
    FOnGetHint: TOGLGetHintEvent;
    FOnGetHintSize: TOGLGetHintSizeEvent;
    FOnGetGroupImageIndex: TOGLGetImageIndexEvent;
    FOnItemDblClick: TOGLItemClickEvent;
    FOnGetChildItemHeight: TOGLGetChildItemHeightEvent;
    FOnCompareItems: TOGLCompareItemsEvent;
    FOnSearchItem: TOGLSearchItemEvent;
    FOnExpandItem: TOGLItemEvent;
    FOnCollapsItem: TOGLItemEvent;

    // DragNDrop events
    FOnOLEGetDataObject: TOGLGetDataObjectEvent;
    FOnOLEDragOver: TOGLDragOverEvent;            // used on each mouse move
    FOnOLEDrop: TOGLDropEvent;                    // used when mouse button released
    FOnOLEGetData: TOGLGetDataEvent;              // used to render data of IDataObject
    FOnOLEGetClipboardFormats: TOGLGetClipboardFormatsEvent;
    FOnOLEDragAllowed: TOGLDragAllowedEvent;      // is OLE-Drag allowed?

    FOnMouseDownOnItem: TOGLMouseButtonEvent;
    FOnMouseUpOnItem: TOGLMouseButtonEvent;
    FDoNotClearItems: Boolean;
    FConsiderColHint: Boolean;
    FDragDropMode: TDragDropMode;
    FShowNodes: Boolean;
    FGroupShowCount: Boolean;
    FOnSelectionChange: TNotifyEvent;
    FHideSelection: Boolean;
    FIntSelectionColor: TColor;
    FGroupColor: TColor;
    FGroupLineColor: TColor;
    FGroupSelectionColor: TColor;
    FGroupSelectionTextColor: TColor;
    FGroupFont: TFont;
    FGroupCountFont: TFont;
    FSupressOnSelect: Boolean;
    FAllowOneOnSelect: Boolean;
    FInternalBtnUp: Boolean;
    function GetFirstGroupItem: POGLItem;
    function GetFirstSelectedItem: POGLItem;
    function GetSelectedCount: Integer;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetUpdateState(const Updating: Boolean);
    procedure SetSortColumn(Value: TOGLSortColumn);
    procedure SetSortDirection(const Value: TOGLSortDirection);
    procedure SetViewStyle(const Value: TOGLViewStyle);
    function GetDragEngine: IOGLDragEngine;
    procedure GetOLEClipboardFormats(var Formats: TFormatEtcArray); virtual;
    function GetDropEffect(const Shift: TShiftState;
      const AllowedEffect: Integer): Integer; virtual;
    procedure SetDragOptions(const Value: TOGLDragOptions);
    procedure SetDropTargetGroup(const GroupItem: POGLItem);
    procedure SetFocusedItem(Item: POGLItem);

    procedure FontOnChange(Sender: TObject);
    procedure ImagesOnChange(Sender: TObject);

    procedure InitRootItem;

    procedure RenderDragImage(const HotSpot: TPoint;
      const DataObject: IDataObject);

    function DoCompare(Item1, Item2: POGLItem;
      Column: TOGLSortColumn): Integer;
    procedure DoHotCharSearch(CharCode: Word);

    procedure WMMouseHover(var Message: TMessage); message WM_MOUSEHOVER;
    procedure WMMouseWheel(var Message: TCMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;

    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;

    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure SetDragDropMode(const Value: TDragDropMode);
    procedure SetShowNodes(const Value: Boolean);
    procedure SetGroupShowCount(const Value: Boolean);
    procedure SetGroupColor(const Value: TColor);
    procedure SetGroupCountFont(const Value: TFont);
    procedure SetGroupFont(const Value: TFont);
    procedure SetGroupSelectionColor(const Value: TColor);
    procedure SetGroupSelectionTextColor(const Value: TColor);
    procedure SetGroupLineColor(const Value: TColor);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure AdjustItemsSize; virtual;
    procedure AdjustItemsFont; virtual;
    procedure PaintWindow(DC: HDC); override;
    procedure PaintSelectionRectangle(TargetCanvas: TCanvas);
    function HTMLPaint(Canvas: TCanvas; s: string; fr:TRect): Boolean; virtual;
    procedure DrawItem(TargetCanvas: TCanvas;
      TargetRect: TRect; Item: POGLItem);

    procedure AddSelectionFromAnchorItemTo(Item: POGLItem);
    function GetSelectionDisplayRect: TRect;
    procedure ClearSelectionRect;
    function MoveFocus(MoveFocusDirection: TOGLMoveFocusDirection;
      SelectItems, ClearSelectionBeforeMove,
      SelectFocusedItem, MoveWithinGroup: Boolean): Boolean; overload;
    function DoSetFocusedItem(Item: POGLItem;
      SetAnchorItem: Boolean): Boolean;
    procedure DoStateChange(Enter: TOGLStates;
      Leave: TOGLStates = []); virtual;
    procedure DoTrackMouseEvent(const Start: Boolean);

    // DragNDrop engine
    // OLE DragNDrop
    function DoOLECreateDataObject: IDataObject; virtual;
    function DoOLEDragOver(const DataObject: IDataObject;
      Shift: TShiftState; Pt: TPoint; State: TDragState;
      var Effect: Integer): Boolean; virtual;
    procedure DoOLEDrop(const DataObject: IDataObject;
      Formats: TClipFormatArray; Shift: TShiftState; Pt: TPoint;
      var Effect: Integer); virtual;
    function DoOLEGetData(const FormatEtcIn: TFormatEtc;
      out Medium: TStgMedium): HRESULT; virtual;
    procedure DoGetOLEClipboardFormats(var Formats: TFormatEtcArray); virtual;
    function DoOLEDragAllowed(Item: POGLItem): Boolean; virtual;
    procedure DoOLEDragging(P: TPoint);
    // VCL DragNDrop
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoExpand(Item: POGLItem); virtual;
    procedure DoCollaps(Item: POGLItem); virtual;
    procedure DragCanceled; override;
    procedure DragFinished;

    function OLEDragEnter(const DataObject: IDataObject;
      KeyState: Integer; Pt: TPoint; var Effect: Integer): HResult; virtual;
    function OLEDragOver(KeyState: Integer; Pt: TPoint;
      State: TDragState; var Effect: Integer): HResult; virtual;
    procedure OLEDragLeave; virtual;
    function OLEDrop(const DataObject: IDataObject; KeyState: Integer;
      Pt: TPoint; var Effect: Integer): HResult; virtual;
    function OLEGetData(const FormatEtcIn: TFormatEtc;
      out Medium: TStgMedium): HResult; virtual;

    procedure HandleMouseDown(var Message: TWMMouse;
      const HitInfo: TOGLItemHitInfo);
    procedure HandleMouseUp(var Message: TWMMouse;
      const HitInfo: TOGLItemHitInfo);
    procedure HandleMouseSelection(var UpdateRect: TRect;
      const OldSelRect, NewSelRect: TRect);
    procedure HandleMouseDownSelection(LastFocused, HitItem: POGLItem;
      Shift: TShiftState; DragPending: Boolean);

    procedure DblClick; override;
    procedure MouseMove(Shift: TShiftState;
      X, Y: Integer); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Resize; override;

    function GetChildItemWidth: Word;

    property GroupItemHeight: Word read FGroupItemHeight;
    property ChildItemHeight: Word read FChildItemHeight;

    procedure SetGroupItemHeight(Value: Integer);
    procedure SetInternalRootItem(RItem: POGLItem);
    function GetRealRect(Item: POGLItem; IncludeChildren: Boolean = False): TRect;
    property ShowNodes: Boolean read FShowNodes write SetShowNodes;
    property GroupShowCount: Boolean read FGroupShowCount write SetGroupShowCount;
    property ConsiderColHint: Boolean read FConsiderColHint write FConsiderColHint;
    property DoNotClearItems: Boolean read FDoNotClearItems write FDoNotClearItems default False;
    property DragDropMode: TDragDropMode read FDragDropMode write SetDragDropMode;
    property HideSelection: Boolean read FHideSelection write FHideSelection;
    property GroupFont: TFont read FGroupFont write SetGroupFont;
    property GroupCountFont: TFont read FGroupCountFont write SetGroupCountFont;
    property GroupColor: TColor read FGroupColor write SetGroupColor;
    property GroupLineColor: TColor read FGroupLineColor write SetGroupLineColor;
    property GroupSelectionColor: TColor read FGroupSelectionColor write SetGroupSelectionColor;
    property GroupSelectionTextColor: TColor read FGroupSelectionTextColor write SetGroupSelectionTextColor;

    property OnMouseDownOnItem: TOGLMouseButtonEvent read FOnMouseDownOnItem write FOnMouseDownOnItem;
    property OnMouseUpOnItem: TOGLMouseButtonEvent read FOnMouseUpOnItem write FOnMouseUpOnItem;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure InternalPaintWindow(Canvas: TCanvas;
      OnlySelectedItems: Boolean = False);
    function InvalidateItem(Item: POGLItem;
      const IncludeChildren: Boolean = False): TRect; virtual;

    procedure GetHitTestInfoAt(X, Y: Integer; const Relative: Boolean;
      var HitInfo: TOGLItemHitInfo);
    function GetItemAt(X, Y: Integer; const Relative: Boolean): POGLItem;
    function IsItem2AfterItem1(Item1,Item2: POGLItem): Boolean;
    function GetCaption(Item: POGLItem): String;
    function CaptionToItem(ACaption: String;
      const GroupItem: POGLItem = nil): POGLItem;

    function AddItem(ParentItem: POGLItem = nil): POGLItem;
    function InsertItem(ParentItem: POGLItem; Index: Integer): POGLItem;
    procedure DeleteItem(Item: POGLItem; Reindex: Boolean = True);
    procedure DeleteSelectedItems(ReIndex: Boolean = False);
    procedure Clear;

    function GetItemData(Item: POGLItem): Pointer;
    procedure SelectItem(Item: POGLItem);
    function IsItemSelected(Item: POGLItem): Boolean;
    function IsGroupItem(Item: POGLItem): Boolean;
    function IsGroupExpanded(Item: POGLItem): Boolean;
    function ExpandItem(Item: POGLItem): Boolean;
    function CollapseItem(Item: POGLItem): Boolean;
    procedure ToggleExpandedItem(Item: POGLItem);
    function ExpandAll: Boolean;
    function CollapseAll: Boolean;

    function GetDisplayRect(Item: POGLItem): TRect;
    function ScrollIntoView(Item: POGLItem;
      AutoScrollOnExpand: Boolean = False): Boolean;

    procedure AddToSelection(Item: POGLItem;
      IncludeChildItems: Boolean = False);
    procedure RemoveFromSelection(Item: POGLItem;
      IncludeChildItems: Boolean = False);
    procedure InvertSelection(Item: POGLItem;
      IncludeChildItems: Boolean = False);
    procedure AddAllToSelection;
    procedure RemoveSelectionBeforeItem(Item: POGLItem);
    procedure RemoveSelectionAfterItem(Item: POGLItem);
    function ClearSelection: Boolean;

    procedure Sort(ParentItem: POGLItem; Column: TOGLSortColumn;
      Direction: TOGLSortDirection); overload;
    procedure Sort(Column: TOGLSortColumn = NoColumn;
      Direction: TOGLSortDirection = sdDefault); overload;

    procedure BeginDrag(Immediate: Boolean;
      Threshold: Integer = -1); reintroduce;
    function Dragging: Boolean; reintroduce;

    property ItemDataSize: Integer read FItemDataSize write FItemDataSize;
    property RootItem: POGLItem read FRootItem;
    property FocusedItem: POGLItem read FFocusedItem write SetFocusedItem;
    property FirstGroupItem: POGLItem read GetFirstGroupItem;
    property FirstSelectedItem: POGLItem read GetFirstSelectedItem;
    property SelectedCount: Integer read GetSelectedCount;

    property DragEngine: IOGLDragEngine read GetDragEngine;
    property DropTargetGroup: POGLItem read FDropTargetGroup;
    property SelectionColor: TColor read FIntSelectionColor write FIntSelectionColor;
  published
    property AutoOptions: TOGLAutoOptions
      read FAutoOptions write FAutoOptions default DefaultAutoOptions;
    property BorderStyle: TBorderStyle
      read FBorderStyle write SetBorderStyle default bsSingle;
    property DragOperations: TOGLDragOperations
      read FDragOperations write FDragOperations default DefaultDragOperations;
    property DragType: TOGLDragType
      read FDragType write FDragType default dtOLE;
    property DragOptions: TOGLDragOptions
      read FDragOptions write SetDragOptions default DefaultDragOptions;
    property DragImageWidth: Word
      read FDragImageWidth write FDragImageWidth default 350;
    property DragImageHeight: Word
      read FDragImageHeight write FDragImageHeight default 200;
    property DrawSelectionMode: TOGLDrawSelectionMode
      read FDrawSelectionMode write FDrawSelectionMode default dsmThemeAware;
    property HintOptions: TOGLHintOptions
      read FHintOptions write FHintOptions;
    property Images: TCustomImageList read FImages write SetImages;
    property ThumbnailOptions: TOGLThumbnailOptions
      read FThumbnailOptions write FThumbnailOptions;
    property SelectionOptions: TOGLSelectionOptions
      read FSelectionOptions write FSelectionOptions default DefaultSelectionOptions;
    property SortColumn: TOGLSortColumn
      read FSortColumn write SetSortColumn default NoColumn;
    property SortDirection: TOGLSortDirection
      read FSortDirection write SetSortDirection default sdAscending;
    property SearchType: TOGLSearchType
      read FSearchType write FSearchType default stChildOnly;
    property ViewStyle: TOGLViewStyle
      read FViewStyle write SetViewStyle default vsThumbnails;

    // main events
    property OnInitItem: TOGLItemEvent
      read FOnInitItem write FOnInitItem;
    property OnFreeItem: TOGLItemEvent
      read FOnFreeItem write FOnFreeItem;
    property OnGetCaption: TOGLGetCaptionEvent
      read FOnGetCaption write FOnGetCaption;
    property OnGetHint: TOGLGetHintEvent
      read FOnGetHint write FOnGetHint;
    property OnGetHintSize: TOGLGetHintSizeEvent
      read FOnGetHintSize write FOnGetHintSize;
    property OnGetGroupImageIndex: TOGLGetImageIndexEvent
      read FOnGetGroupImageIndex write FOnGetGroupImageIndex;
    property OnDrawItem: TOGLDrawItemEvent
      read FOnDrawItem write FOnDrawItem;
    property OnDrawHint: TOGLDrawHintEvent
      read FOnDrawHint write FOnDrawHint;
    property OnItemDblClick: TOGLItemClickEvent
      read FOnItemDblClick write FOnItemDblClick;
    property OnGetChildItemHeight: TOGLGetChildItemHeightEvent
      read FOnGetChildItemHeight write FOnGetChildItemHeight;

    // sort & search events
    property OnCompareItems: TOGLCompareItemsEvent
      read FOnCompareItems write FOnCompareItems;
    property OnSearchItem: TOGLSearchItemEvent
      read FOnSearchItem write FOnSearchItem;

    property OnExpandItem: TOGLItemEvent
      read FOnExpandItem write FOnExpandItem;
    property OnCollapsItem: TOGLItemEvent
      read FOnCollapsItem write FOnCollapsItem;    

    // DragNDrop events
    property OnOLEGetDataObject: TOGLGetDataObjectEvent
      read FOnOLEGetDataObject write FOnOLEGetDataObject;
    property OnOLEDragOver: TOGLDragOverEvent
      read FOnOLEDragOver write FOnOLEDragOver;
    property OnOLEDrop: TOGLDropEvent
      read FOnOLEDrop write FOnOLEDrop;
    property OnOLEGetData: TOGLGetDataEvent
      read FOnOLEGetData write FOnOLEGetData;
    property OnOLEGetClipboardFormats: TOGLGetClipboardFormatsEvent
      read FOnOLEGetClipboardFormats write FOnOLEGetClipboardFormats;
    property OnOLEDragAllowed: TOGLDragAllowedEvent
      read FOnOLEDragAllowed write FOnOLEDragAllowed;

    // derived from TScrollBox
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode default dmAutomatic;
    property Enabled;
    property Ctl3D;
    property Font;
    property ParentBiDiMode;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;

    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

const
  DefaultGlobalClipboardFormat: TFormatEtc = (
    cfFormat: 0;
    ptd: nil;
    dwAspect: DVASPECT_CONTENT;
    lindex: -1;
    tymed: TYMED_HGLOBAL;
  );

  DefaultStreamClipboardFormat: TFormatEtc = (
    cfFormat: 0;
    ptd: nil;
    dwAspect: DVASPECT_CONTENT;
    lindex: -1;
    tymed: TYMED_ISTREAM;
  );

{$IFDEF BCB}
{$EXTERNALSYM AlphaBlend}
function AlphaBlend(DC: HDC; p2, p3, p4, p5: Integer; DC6: HDC; p7, p8, p9, p10: Integer; p11: TBlendFunction): BOOL; stdcall;
{$ENDIF}

implementation

uses
  SysUtils, Math{, Themes}, CommCtrl, ShellApi;

{$R *.RES}

{$I HTMLENGO.PAS}

const
  OGLChildItemVertTextSpacing = 4;
  OGLTrackMouseVertScrollOffset = 20;

  DrawOGLItemCaptionFlags = DT_CENTER or DT_TOP or DT_SINGLELINE or
    DT_END_ELLIPSIS or DT_NOCLIP or DT_NOPREFIX;

  DrawOGLHintWindowFlags = DT_EDITCONTROL or DT_LEFT or DT_TOP or
    DT_WORDBREAK or DT_NOPREFIX;

var
  GlobalInitialized: Boolean;
  OleInitialized: Boolean;




{$IFDEF BCB}
function AlphaBlend; external msimg32 name 'AlphaBlend';
{$ENDIF}

procedure GlobalInitializeOLE;
begin
  if GlobalInitialized then Exit;
  GlobalInitialized := True;
  OleInitialized := Succeeded(OleInitialize(nil));
end;

procedure GlobalFinalizeOLE;
begin
  if not GlobalInitialized then Exit;
  if OleInitialized then
    OleUninitialize;
end;

function PointsToRect(const Point1,Point2: TPoint): TRect;
begin
  Result := Rect(
    Min(Point1.X, Point2.X),
    Min(Point1.Y, Point2.Y),
    Max(Point1.X, Point2.X),
    Max(Point1.Y, Point2.Y));
end;

function NormalizeRect(const SrcRect: TRect): TRect;
begin
  Result := Rect(
    Min(SrcRect.Left, SrcRect.Right),
    Min(SrcRect.Top, SrcRect.Bottom),
    Max(SrcRect.Left, SrcRect.Right),
    Max(SrcRect.Top, SrcRect.Bottom));
end;

function IsRectEmptyEx(const Rect: TRect): Boolean;
begin
  Result := (Rect.Left = Rect.Right) or (Rect.Top = Rect.Bottom);
end;

function IsIntersectRect(Rect1,Rect2: TRect): Boolean;
begin
  Result := IntersectRect(Rect1, Rect1, Rect2);
end;

{$WARNINGS OFF}
function CharCodeToMoveFocusDirection(CharCode: Word): TOGLMoveFocusDirection;
const
  aCharCode: array[TOGLMoveFocusDirection] of Word =
    (VK_HOME, VK_PRIOR, VK_UP, VK_LEFT, VK_RIGHT, VK_DOWN, VK_NEXT, VK_END, 0);
begin
  for Result := Low(Result) to High(Result) do
    if aCharCode[Result] = CharCode then
      Exit;
end;
{$WARNINGS ON}

// TOGLThumbnailOptions --------------------------------------------------------

constructor TOGLThumbnailOptions.Create(AOwner: TOutlookGroupedList);
begin
  inherited Create;
  FOwner := AOwner;
  FHorizSpacing := 12;
  FVertSpacing := 12;
  FThumbnailWidth := 96;
  FThumbnailHeight := 96;
end;

procedure TOGLThumbnailOptions.SetHorizSpacing(const Value: Byte);
begin
  if FHorizSpacing = Value then Exit;
  FHorizSpacing := Value;
  FOwner.AdjustItemsSize;
end;

procedure TOGLThumbnailOptions.SetVertSpacing(const Value: Byte);
begin
  if FVertSpacing = Value then Exit;
  FVertSpacing := Value;
  FOwner.AdjustItemsSize;
end;

procedure TOGLThumbnailOptions.SetThumbnailWidth(const Value: Word);
begin
  if FThumbnailWidth = Value then exit;
  FThumbnailWidth := Value;
  FOwner.AdjustItemsSize;
end;

procedure TOGLThumbnailOptions.SetThumbnailHeight(const Value: Word);
begin
  if FThumbnailHeight = Value then exit;
  FThumbnailHeight := Value;
  FOwner.AdjustItemsSize;
end;

// TOGLHintOptions -------------------------------------------------------------

constructor TOGLHintOptions.Create;
begin
  inherited;
  FAlphaBlend := True;
  FAlphaBlendValue := 240;
end;

// TOGLHintWindow --------------------------------------------------------------

constructor TOGLHintWindow.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := True;
  FBmpBlend := TBitmap.Create;
  FBmpBlend.PixelFormat := pf24bit;
end;

destructor TOGLHintWindow.Destroy;
begin
  FreeAndNil(FBmpBlend);
  inherited;
end;

procedure TOGLHintWindow.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  // do not erase the background
  Message.Result := 1;
end;

procedure TOGLHintWindow.WMNCPaint(var Message: TMessage);
begin
  // do not paint the borders
  Message.Result := 0;
end;

procedure TOGLHintWindow.WMShowWindow(var Message: TWMShowWindow);
begin
  // clear hint data when the window becomes hidden
  if not Message.Show then
  begin
    if Assigned(FHintData.OutlookGroupedList) then
    begin
      Exclude(FHintData.OutlookGroupedList.FStates, lsHintShowed);
      FHintData.OutlookGroupedList.FLastHintRect := Rect(0, 0, 0, 0);
    end;
    FHintData.HitInfo.HitItem := nil;
    FHintData.HitInfo.HitTest := htInClientArea;
    FillChar(FHintData, SizeOf(FHintData), 0);
  end;
end;

procedure TOGLHintWindow.CMTextChanged(var Message: TMessage);
begin
  // do nothing - prevent OGL from resizing the Hint
end;

procedure TOGLHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := WS_POPUP;
  Params.ExStyle := Params.ExStyle and not WS_EX_CLIENTEDGE;
end;

procedure TOGLHintWindow.Paint;
var
  rc: TRect;
  bf: TBlendFunction;
begin
  with Canvas do
  begin
    // still force tooltip back and text color
    rc := ClientRect;
    Font.Color := clInfoText;
    Pen.Color := clInfoText;
    Brush.Color := clInfoBk;
    Rectangle(rc.Left, rc.Top, rc.Right, rc.Bottom);
    Brush.Style := bsClear;
    Font := Screen.HintFont;
    if (FHintData.HitInfo.HitItem <> nil) and
       (FHintData.HitInfo.HitTest <> htInClientArea) and 
       Assigned(FHintData.OutlookGroupedList.OnDrawHint) then
      FHintData.OutlookGroupedList.OnDrawHint(FHintData.OutlookGroupedList,
        Canvas, rc, FHintData.HitInfo.HitItem) else
    begin
      Inc(rc.Top, 4);
      Inc(rc.Left, 8);
      Dec(rc.Bottom, 4);
      Dec(rc.Right, 8);
      DrawText(Canvas.Handle, PChar(Caption), -1, rc, DrawOGLHintWindowFlags);
    end;
  end;
  // make AlphaBlend effect if need
  if FHintData.OutlookGroupedList.HintOptions.AlphaBlend then
  begin
    bf.BlendOp := AC_SRC_OVER;
    bf.BlendFlags := 0;
    bf.SourceConstantAlpha := High(FHintData.OutlookGroupedList.
      HintOptions.AlphaBlendValue) -
      FHintData.OutlookGroupedList.HintOptions.AlphaBlendValue;
    bf.AlphaFormat := 0;

    {$IFDEF BCB}
    AlphaBlend(Canvas.Handle, 0, 0, Width, Height,
      FBmpBlend.Canvas.Handle, 0, 0, Width, Height, bf);
    {$ELSE}
    Windows.AlphaBlend(Canvas.Handle, 0, 0, Width, Height,
      FBmpBlend.Canvas.Handle, 0, 0, Width, Height, bf);
    {$ENDIF}

  end;
  BitBlt(Canvas.Handle, 0, 0, Width, Height,
    Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TOGLHintWindow.ActivateHint(Rect: TRect; const AHint: String);
begin
  if IsRectEmpty(Rect) then
    Application.CancelHint else
  begin
    SetWindowPos(Handle, 0, Rect.Left, Rect.Top, Width, Height,
      SWP_HIDEWINDOW or SWP_NOACTIVATE or SWP_NOZORDER);
    UpdateBoundsRect(Rect);
    if Rect.Top + Height > Screen.DesktopHeight then
      Rect.Top := Screen.DesktopHeight - Height;
    if Rect.Top < Screen.DesktopTop then
      Rect.Top := Screen.DesktopTop;
    if Rect.Left > Screen.DesktopWidth - Width then
      Rect.Left := Screen.DesktopWidth - Width;
    if Rect.Left < Screen.DesktopLeft then
      Rect.Left := Screen.DesktopLeft;
    // copy the image below hint before hint showed for AlphaBlend effect
    if FHintData.OutlookGroupedList.HintOptions.AlphaBlend then
    begin
      FBmpBlend.Width := Width;
      FBmpBlend.Height := Height;
      BitBlt(FBmpBlend.Canvas.Handle, 0, 0, Width, Height,
        GetWindowDC(GetDesktopWindow), Rect.Left, Rect.Top, SRCCOPY);
    end;
    SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top,
      Width, Height, SWP_SHOWWINDOW or SWP_NOACTIVATE);
    InvalidateRect(Self.Handle, nil, False);
  end;
end;

function TOGLHintWindow.CalcHintRect(MaxWidth: Integer;
  const AHint: String; AData: Pointer): TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if AData = nil then Exit;
  FHintData := POGLHintData(AData)^;
  Caption := AHint;
  // internal calc the HintRect
  if Length(Caption) = 0 then Exit;
  Canvas.Font := Screen.HintFont;
  Result.Right := MaxWidth;
  Result.Bottom := Screen.Height;
  DrawText(Canvas.Handle, PChar(Caption), -1, Result,
    DrawOGLHintWindowFlags or DT_CALCRECT);
  Inc(Result.Right, 16);
  Inc(Result.Bottom, 8);
  // request user for new Hint size
  if (FHintData.HitInfo.HitItem <> nil) and
     (FHintData.HitInfo.HitTest <> htInClientArea) and
     Assigned(FHintData.OutlookGroupedList.OnGetHintSize) then
    FHintData.OutlookGroupedList.OnGetHintSize(FHintData.OutlookGroupedList,
      FHintData.HitInfo.HitItem, Result);
end;

function TOGLHintWindow.IsHintMsg(var Msg: TMsg): Boolean;
begin
  Result := inherited IsHintMsg(Msg) and HandleAllocated and
    IsWindowVisible(Handle);
  // ignore NC mouse moving or key presses
  if Result and ((Msg.Message = WM_NCMOUSEMOVE) or
    ((Msg.Message >= WM_KEYFIRST) and (Msg.Message <= WM_KEYLAST))) then
    Result := False else
  // work around problems with keypresses while showing hint
  if HandleAllocated and IsWindowVisible(Handle) and
     (Msg.Message >= WM_KEYFIRST) and (Msg.Message <= WM_KEYLAST) and
     TranslateMessage(Msg) then
    DispatchMessage(Msg);
end;

// TOGLEnumFormatEtc -----------------------------------------------------------

constructor TOGLEnumFormatEtc.Create(
  const AOutlookGroupedList: TOutlookGroupedList;
  const AFormatEtcArray: TFormatEtcArray; const AIndex: Integer = 0);
begin
  inherited Create;
  FOutlookGroupedList := AOutlookGroupedList;
  // copy AFormatEtcArray to FFormatEtcArray
  SetLength(FFormatEtcArray, Length(AFormatEtcArray));
  Move(AFormatEtcArray[0], FFormatEtcArray[0],
    FormatCount * SizeOf(TFormatEtc));
  FIndex := AIndex;
end;

destructor TOGLEnumFormatEtc.Destroy;
begin
  Finalize(FFormatEtcArray);
  inherited Destroy;
end;

function TOGLEnumFormatEtc.GetFormatCount: Integer;
begin
  Result := Length(FFormatEtcArray);
end;

// IEnumFormatEtc realization

function TOGLEnumFormatEtc.Next(celt: Integer; out elt;
  pceltFetched: PLongint): HResult;
begin
  Result := S_FALSE;
  if Index + celt >= FormatCount then
    celt := FormatCount - Index;
  if celt > 0 then
  begin
    Move(FFormatEtcArray[Index], elt, celt * SizeOf(TFormatEtc));
    Inc(FIndex, celt);
    Result := S_OK;
  end;
  if pceltFetched <> nil then
    pceltFetched^ := celt;
end;

function TOGLEnumFormatEtc.Skip(celt: Integer): HResult;
begin
  if Index + celt < FormatCount then
  begin
    Inc(FIndex, celt);
    Result := S_Ok;
  end else
  begin
    Index := FormatCount-1;
    Result := S_FALSE;
  end;
end;

function TOGLEnumFormatEtc.Reset: HResult;
begin
  Index := 0;
  Result := S_OK;
end;

function TOGLEnumFormatEtc.Clone(out Enum: IEnumFormatEtc): HResult;
begin
  Result := S_OK;
  try
    Enum := TOGLEnumFormatEtc.Create(FOutlookGroupedList,
      FFormatEtcArray, Index);
  except
    Result := E_FAIL;
  end;
end;

// TOGLDataObject --------------------------------------------------------------

constructor TOGLDataObject.Create(AOwner: TOutlookGroupedList);
begin
  inherited Create;
  FOwner := AOwner;
  FOwner.GetOLEClipboardFormats(FFormatEtcArray);
end;

destructor TOGLDataObject.Destroy;
begin
  FormatEtcArray := nil;
  inherited Destroy;
end;

function TOGLDataObject.EqualFormatEtc(
  const FormatEtc1, FormatEtc2: TFormatEtc): Boolean;
begin
  Result :=
    (FormatEtc1.cfFormat = FormatEtc2.cfFormat) and
    (FormatEtc1.ptd = FormatEtc2.ptd) and
    (FormatEtc1.dwAspect = FormatEtc2.dwAspect) and
    (* // ignore lindex beacuse registered Clipboard Format may have const value
       // in a difference of queried data
    (FormatEtc1.lindex = FormatEtc2.lindex) and *)
    (FormatEtc1.tymed and FormatEtc2.tymed <> 0);
end;

// IDataObject realization

function TOGLDataObject.GetData(const FormatEtcIn: TFormatEtc;
  out medium: TStgMedium): HResult;
(* Called by a data consumer to obtain data from a source data object.
   The GetData method renders the data described in the specified FORMATETC
   structure and transfers it through the specified STGMEDIUM structure.
   The caller then assumes responsibility for releasing the STGMEDIUM
   structure. *)
var
  i: Integer;
begin
  try
    Result := QueryGetData(FormatEtcIn);
    if Result = S_OK then
      for i := 0 to High(FormatEtcArray) do
        if EqualFormatEtc(FormatEtcIn, FormatEtcArray[i]) then
        begin
          Result := FOwner.OLEGetData(FormatEtcIn, Medium);
          Break;
        end;
  except
    FillChar(Medium, SizeOf(Medium), #0);
    Result := E_FAIL;
  end;
end;

function TOGLDataObject.GetDataHere(const FormatEtcIn: TFormatEtc;
  out Medium: TStgMedium): HResult;
(* Called by a data consumer to obtain data from a source data object.
   This method differs from the GetData method in that the caller must allocate
   and free the specified storage medium. *)
begin
  Result := E_NOTIMPL;
end;

function TOGLDataObject.QueryGetData(const FormatEtcIn: TFormatEtc): HResult;
(* Determines whether the data object is capable of rendering the data described
   in the FORMATETC structure. Objects attempting a paste or drop operation can
   call this method before calling IDataObject::GetData to get an indication of
   whether the operation may be successful.*)
var
  i: Integer;
begin
  Result := DV_E_CLIPFORMAT;
  for i := 0 to High(FFormatEtcArray) do
  begin
    if FormatEtcIn.cfFormat = FFormatEtcArray[i].cfFormat then
    begin
      if (FormatEtcIn.tymed and FFormatEtcArray[i].tymed) <> 0 then
      begin
        if FormatEtcIn.dwAspect = FFormatEtcArray[i].dwAspect then
        begin
          (* // ignore lindex beacuse registered Clipboard Format may have const value
             // in a difference of queried data
          if FormatEtcIn.lindex = FFormatEtcArray[i].lindex then
          begin *)
            Result := S_OK;
            Break;
          (*
          end else
            Result := DV_E_LINDEX;
          *)
        end else
          Result := DV_E_DVASPECT;
      end else
        Result := DV_E_TYMED;
    end;
  end
end;

function TOGLDataObject.GetCanonicalFormatEtc(
  const FormatEtcIn: TFormatEtc; out FormatEtcOut: TFormatEtc): HResult;
(* Provides a standard FORMATETC structure that is logically equivalent to one
   that is more complex. You use this method to determine whether two different
   FORMATETC structures would return the same data, removing the need for
   duplicate rendering. *)
begin
  Result := DATA_S_SAMEFORMATETC;
end;

function TOGLDataObject.SetData(const FormatEtcIn: TFormatEtc;
  var Medium: TStgMedium; fRelease: BOOL): HResult;
(* Called by an object containing a data source to transfer data to the object
   that implements this method. *)
begin
  Result := E_NOTIMPL;
end;

function TOGLDataObject.EnumFormatEtc(dwDirection: Longint;
  out EnumFormatEtc: IEnumFormatEtc): HResult;
(* Creates an object for enumerating the FORMATETC structures for a data object.
  These structures are used in calls to IDataObject::GetData or
  IDataObject::SetData. *)
begin
  Result := E_FAIL;
  if dwDirection = DATADIR_GET then
  begin
    EnumFormatEtc := TOGLEnumFormatEtc.Create(FOwner, FormatEtcArray);
    Result := S_OK;
  end else
    EnumFormatEtc := nil;
  if EnumFormatEtc = nil then
    Result := OLE_S_USEREG;
end;

function TOGLDataObject.DAdvise(const FormatEtcIn: TFormatEtc; advf: Longint;
  const AdvSink: IAdviseSink; out dwConnection: Longint): HResult;
(* Advise sink management is greatly simplified by the IDataAdviseHolder
   interface. We use this interface and forward all concerning calls to it. *)
begin
  Result := E_NOTIMPL;
end;

function TOGLDataObject.DUnadvise(dwConnection: Longint): HResult;
begin
  Result := E_NOTIMPL;
end;

function TOGLDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
begin
  Result := E_NOTIMPL;
end;

// TOGLDragEngine --------------------------------------------------------------

constructor TOGLDragEngine.Create(AOwner: TOutlookGroupedList);
begin
  inherited Create;
  FOwner := AOwner;
  CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER,
    IID_IDropTargetHelper, FDropTargetHelper);
end;

destructor TOGLDragEngine.Destroy;
begin
  Pointer(FOwner.FDragEngine) := nil;
  FDropTargetHelper := nil;
  inherited Destroy;
end;

// IOGLDragEngine realization

function TOGLDragEngine.GetDataObject: IDataObject;
begin
  if Assigned(FDataObject) then
    Result := FDataObject else
  begin
    Result := FOwner.DoOLECreateDataObject;
    if Result = nil then
      Result := TOGLDataObject.Create(FOwner) as IDataObject;
  end;
end;

function TOGLDragEngine.GetDropActive: Boolean;
begin
  Result := FDropActive;
end;

procedure TOGLDragEngine.ForceDragLeave;
begin
  if Assigned(FDropTargetHelper) then
    FDropTargetHelper.DragLeave;
end;

// IDropSource realization

function TOGLDragEngine.QueryContinueDrag(
  fEscapePressed: BOOL; KeyState: Longint): HResult;
var
  RButton: Boolean;
  LButton: Boolean;
begin
  LButton := (KeyState and MK_LBUTTON) <> 0;
  RButton := (KeyState and MK_RBUTTON) <> 0;
  // DragNDrop canceled by pressing both mouse buttons or Esc?
  if (LButton and RButton) or fEscapePressed then
    Result := DRAGDROP_S_CANCEL
  else
    // is drag'n drop finished?
    if not (LButton or RButton) then
      Result := DRAGDROP_S_DROP
    else
      Result := S_OK;
end;

function TOGLDragEngine.GiveFeedback(dwEffect: Longint): HResult;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

// IDropTarget realization

function TOGLDragEngine.DragEnter(const DataObject: IDataObject;
  KeyState: Longint; Pt: TPoint; var dwEffect: Longint): HResult;
begin
  FDataObject := DataObject;
  FDropActive := True;
  if Assigned(FDropTargetHelper) then
    FDropTargetHelper.DragEnter(FOwner.Handle, DataObject, Pt, dwEffect);
  Result := FOwner.OLEDragEnter(DataObject, KeyState, Pt, dwEffect);
end;

function TOGLDragEngine.DragOver(KeyState: Longint; Pt: TPoint;
  var dwEffect: Longint): HResult;
begin
  if Assigned(FDropTargetHelper) then
    FDropTargetHelper.DragOver(Pt, dwEffect);
  Result := FOwner.OLEDragOver(KeyState, Pt, dsDragMove, dwEffect);
end;

function TOGLDragEngine.DragLeave: HResult;
begin
  if Assigned(FDropTargetHelper) then
    FDropTargetHelper.DragLeave;
  FOwner.OLEDragLeave;
  FDropActive := False;
  FDataObject := nil;
  Result := S_OK;
end;

function TOGLDragEngine.Drop(const DataObject: IDataObject; KeyState: Longint;
  Pt: TPoint; var dwEffect: Longint): HResult;
begin
  if Assigned(FDropTargetHelper) then
    FDropTargetHelper.Drop(DataObject, Pt, dwEffect);
  Result := FOwner.OLEDrop(DataObject, KeyState, Pt, dwEffect);
  FDropActive := False;
  FDataObject := nil;
end;

// TOutlookGroupedList ---------------------------------------------------------

constructor TOutlookGroupedList.Create(AOwner: TComponent);
begin
  if not GlobalInitialized then
    GlobalInitializeOLE;
  inherited Create(AOwner);
  AutoSize := False;
  ControlStyle := ControlStyle - [csSetCaption] + [csOpaque, csCaptureMouse,
    csClickEvents, csDoubleClicks, csDisplayDragImage, csReflector];
  DoubleBuffered := True;
  Width := 200;
  Height := 200;
  TabStop := True;
  ShowHint := True;
  Font.OnChange := FontOnChange;
  Color := clWhite;

  FGroupShowCount := True;
  FUpdateSelCount := 0;

  FBorderStyle := bsSingle;
  FAutoOptions := DefaultAutoOptions;
  FHintOptions := TOGLHintOptions.Create;
  FSelectionOptions := DefaultSelectionOptions;
  FThumbnailOptions := TOGLThumbnailOptions.Create(Self);
  FImages := nil;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImagesOnChange;
  FStates := [];
  FSortColumn := NoColumn;
  FSortDirection := sdAscending;
  FSearchType := stChildOnly;
  FViewStyle := vsThumbnails;
  FIntSelectionColor := clHighLight;

  FGroupColor := clNone;
  FGroupSelectionColor := clHighLight;
  FGroupSelectionTextColor := clNone;
  FGroupFont := TFont.Create;
  FGroupFont.Color := $00B96837;
  FGroupFont.Style := [fsBold];
  FGroupCountFont := TFont.Create;
  FGroupCountFont.Color := clBlack;
  FGroupLineColor := $00E0A47B;

  FUpdateCount := 0;
  FOGLItemColumn := 0;
  FTotalItemsHeight := 0;
  FItemDataSize := 0;
  FRootItem := nil;
  InitRootItem;
  FFocusedItem := nil;
  FSelectedAnchorItem := nil;
  FHintData.OutlookGroupedList := Self;
  FHintData.HitInfo.HitItem := nil;
  FHintData.HitInfo.HitTest := htInClientArea;
  FLastHintRect := Rect(0, 0, 0, 0);
  FLastHintHitInfo.HitItem := Pointer($FFFFFF); // to allow default hint showed
  FLastHintHitInfo.HitTest := htInClientArea;
  FSelection := TList.Create;
  ClearSelectionRect;
  FFromSelPoint := Point(0, 0);
  FToSelPoint := Point(0, 0);
  FDrawSelectionMode := dsmThemeAware;
  FThumbnailCaptionHeight := 0;
  FItemsPerRow := 0;
  FDrawSelShiftState := [];

  FBmpPlus := TBitmap.Create;
  FBmpPlus.LoadFromResourceName(hInstance, 'IDB_BUTTON_PLUS');
  FBmpPlus.Transparent := True;
  FBmpMinus := TBitmap.Create;
  FBmpMinus.LoadFromResourceName(hInstance, 'IDB_BUTTON_MINUS');
  FBmpMinus.Transparent := True;
  FBmpCanvas := TBitmap.Create;
  FBmpCanvas.Width := 1;
  FBmpCanvas.Height := 1;

  // DragNDrop and clipboard support
  DragMode := dmAutomatic;
  FDragEngine := nil;
  FOLEDragEnterAccept := False;
  FDropTargetGroup := nil;

  FDragType := dtOLE;
  FDragOptions := DefaultDragOptions;
  FDragImageWidth := 350;
  FDragImageHeight := 200;
  FDragOperations := DefaultDragOperations;
  FDragThreshold := Mouse.DragThreshold;

  // main events
  FOnInitItem := nil;
  FOnFreeItem := nil;
  FOnDrawItem := nil;
  FOnDrawHint := nil;
  FOnGetCaption := nil;
  FOnGetHint := nil;
  FOnGetHintSize := nil;
  FOnGetGroupImageIndex := nil;
  FOnItemDblClick := nil;
  FOnGetChildItemHeight := nil;
  FOnCompareItems := nil;
  FOnSearchItem := nil;

  FDragDropMode := ddmNormal;
  
  // DragNDrop events
  FOnOLEGetDataObject := nil;
  FOnOLEDragOver := nil;
  FOnOLEDrop := nil;
  FOnOLEGetData := nil;
  FOnOLEGetClipboardFormats := nil;
  FOnOLEDragAllowed := nil;

  FGroupItemHeight := 34;  // Added for SetGroupHeight
  FShowNodes := True;

  FHideSelection := True;
end;

destructor TOutlookGroupedList.Destroy;
begin
  if not FDoNotClearItems then
    Clear;
  FreeAndNil(FSelection);
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FThumbnailOptions);
  FreeAndNil(FHintOptions);
  if not FDoNotClearItems then
    FreeMem(FRootItem);
  FreeAndNil(FBmpCanvas);
  FreeAndNil(FBmpMinus);
  FreeAndNil(FBmpPlus);
  FGroupFont.Free;
  FGroupCountFont.Free;
  inherited Destroy;
end;

procedure TOutlookGroupedList.Loaded;
begin
  inherited Loaded;
  AdjustItemsFont;
end;

procedure TOutlookGroupedList.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or BorderStyles[FBorderStyle];
  if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
  begin
    Params.Style := Params.Style and not WS_BORDER;
    Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
  end;
end;

procedure TOutlookGroupedList.CreateWnd;
begin
  inherited;
  // register OGL as OLE IDropTarget
  if (not (csDesigning in ComponentState)) and
      (doOLEAcceptDrop in FDragOptions) then
    RegisterDragDrop(Handle, DragEngine as IDropTarget);
end;

procedure TOutlookGroupedList.WMChar(var Message: TWMChar);
begin
  if lsSearchPending in FStates then
  begin
    DoHotCharSearch(Message.CharCode);
    DoStateChange([], [lsSearchPending]);
  end;
  inherited;
end;

procedure TOutlookGroupedList.WMNCDestroy(var Message: TWMNCDestroy);
begin
  // unregister OGL as OLE IDropTarget
  if (not (csDesigning in ComponentState)) and
     (doOLEAcceptDrop in FDragOptions) then
    RevokeDragDrop(Handle);
  inherited;
end;

procedure TOutlookGroupedList.DoEnter;
begin
  inherited;
  if HandleAllocated and (ComponentState * [csLoading,csDestroying] = []) then
    Invalidate;
end;

procedure TOutlookGroupedList.DoExit;
begin
  inherited;
  ClearSelectionRect;
  if HandleAllocated and (ComponentState * [csLoading,csDestroying] = []) then
    Invalidate;
end;

procedure TOutlookGroupedList.WMMouseHover(var Message: TMessage);
var
  Pt: TPoint;
begin
  // this message is use to autoscroll when mouse selecting is active 
  inherited;
  Message.Result := 0;
  GetCursorPos(Pt);
  Pt := Self.ScreenToClient(Pt);
  MouseMove(KeysToShiftState(Message.WParam), Pt.X, Pt.Y);
end;

procedure TOutlookGroupedList.WMMouseWheel(var Message: TCMMouseWheel);
begin
  // this message is using when OS < WinXP
  if VertScrollBar.Range < ClientHeight then
    inherited else
  begin
    Message.Result := 0;
    VertScrollBar.Position := VertScrollBar.Position - Message.WheelDelta;
  end;
end;

procedure TOutlookGroupedList.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if HandleAllocated and (ComponentState * [csLoading,csDestroying] = []) then
    Invalidate;
end;

procedure TOutlookGroupedList.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  ClearSelectionRect;
  if HandleAllocated and (ComponentState * [csLoading,csDestroying] = []) then
    Invalidate;
end;

procedure TOutlookGroupedList.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TOutlookGroupedList.WMPaint(var Message: TWMPaint);
var
  DC,MemDC: HDC;
  MemBitmap,OldBitmap: HBITMAP;
  PS: TPaintStruct;
begin

  DC := GetDC(0);
  MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
  ReleaseDC(0, DC);
  MemDC := CreateCompatibleDC(0);
  OldBitmap := SelectObject(MemDC, MemBitmap);
  try
    DC := BeginPaint(Handle, PS);
    try
      Message.DC := MemDC;
      PaintWindow(Message.DC);
      Message.DC := 0;
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
    finally
      EndPaint(Handle, PS);
    end;
  finally
    SelectObject(MemDC, OldBitmap);
    DeleteDC(MemDC);
    DeleteObject(MemBitmap);
  end;

end;

procedure TOutlookGroupedList.WMMouseMove(var Message: TWMMouseMove);
var
  HitInfo: TOGLItemHitInfo;
begin
  inherited;
  if (not (lsMouseSelecting in FStates)) and (not Dragging) and
    (lsHintShowed in FStates) then
  begin
    GetHitTestInfoAt(Message.Pos.X, Message.Pos.Y, True, HitInfo);
    if (FLastHintHitInfo.HitItem <> HitInfo.HitItem) or
       ((FLastHintHitInfo.HitTest <> HitInfo.HitTest) and
        (HitInfo.HitTest <> htOnExpandButton)) then
    begin
      Application.CancelHint;
      Application.HintMouseMessage(Self, TMessage(Message));
    end;
  end;
end;

procedure TOutlookGroupedList.CMHintShow(var Message: TCMHintShow);
var
  Bmp: TBitmap;
  HitInfo: TOGLItemHitInfo;
begin
  with Message,HintInfo^ do
  begin
    Result := 1;
    if (lsMouseSelecting in FStates) or Dragging then Exit;
    Inc(CursorPos.Y, VertScrollBar.Position);
    if PtInRect(FLastHintRect, CursorPos) and not FConsiderColHint then Exit;
    Dec(CursorPos.Y, VertScrollBar.Position);
    GetHitTestInfoAt(Message.HintInfo^.CursorPos.X,
      Message.HintInfo^.CursorPos.Y, True, HitInfo);
    if (FLastHintHitInfo.HitItem <> HitInfo.HitItem) or
       ((FLastHintHitInfo.HitTest <> HitInfo.HitTest) or FConsiderColHint and
        (HitInfo.HitTest <> htOnExpandButton))  then
    begin
      FLastHintHitInfo := HitInfo;
      HintMaxWidth := ClientWidth;
      if HitInfo.HitTest = htInClientArea then
      begin
        // default OGL's hint
        HintStr := GetShortHint(Hint);
        if HintStr = '' then Exit;
        FLastHintRect := Rect(0, 0, 0, 0);
      end else
      if HitInfo.HitTest = htOnItemLabel then
      begin
        // caption's hint
        HintStr := '';
        if Assigned(FOnGetCaption) then
          FOnGetCaption(Self, HitInfo.HitItem, HintStr);
        if HintStr <> '' then
        begin
          Bmp := TBitmap.Create;
          try
            Bmp.Width := 1;
            Bmp.Height := 1;
            Bmp.Canvas.Font.Assign(Self.Font);
            if Bmp.Canvas.TextWidth(HintStr) <= GetChildItemWidth - 8 then
              HintStr := '';
            FLastHintRect := GetDisplayRect(HitInfo.HitItem);
            InflateRect(FLastHintRect, -4, 0);
            FLastHintRect.Top := FLastHintRect.Bottom -
              FThumbnailCaptionHeight - FThumbnailOptions.VertSpacing;
            HintMaxWidth := GetChildItemWidth - 8;
          finally
            FreeAndNil(Bmp);
          end;
        end;
      end;
      if ((HitInfo.HitTest = htOnItemLabel) and (HintStr = '')) or
        not (HitInfo.HitTest in [htOnItemLabel, htInClientArea]) then
      begin
        // default item's hint
        HintStr := '';
        if Assigned(FOnGetHint) then
          FOnGetHint(Self, HitInfo.HitItem, HintStr, HintPos);
        HintMaxWidth := ClientWidth;
        FLastHintRect := GetDisplayRect(HitInfo.HitItem);
      end;

      FHintData.OutlookGroupedList := Self;
      FHintData.HitInfo := HitInfo;
      Include(FStates, lsHintShowed);
      HintWindowClass := TOGLHintWindow;
      HintData := @FHintData;
      ReshowTimeout := 4000;
      Result := 0;
      Exit;
    end;
  end;
end;

procedure TOutlookGroupedList.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FLastHintHitInfo.HitItem := Pointer($FFFFFF); // to allow default hint showed
  FLastHintHitInfo.HitTest := htInClientArea;
  Exclude(FStates, lsHintShowed);
end;

procedure TOutlookGroupedList.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;

procedure TOutlookGroupedList.CNKeyDown(var Message: TWMKeyDown);
var
  Shift: TShiftState;
  FocusMoved,ClearSelectionBeforeMove: Boolean;
  MoveFocusDirection: TOGLMoveFocusDirection;
begin
  if not Focused then
    SetFocus;

  FocusMoved := False;
  Shift := KeyDataToShiftState(Message.KeyData);
  ClearSelectionBeforeMove := (not (ssShift in Shift)) and (not (ssCtrl in Shift));
  case Message.CharCode of
    Ord('A'):
      if ssCtrl in Shift then
      begin
        // select all
        AddAllToSelection;
        Invalidate;
      end;
    VK_SPACE:
    begin
      Message.Result := 0;
      if FFocusedItem <> nil then
      begin
        FocusMoved := True;
        // invert selection of focused item
        if ssCtrl in Shift then
          InvertSelection(FFocusedItem, True) else
        if ClearSelectionBeforeMove then
        begin
          // if Child item - select it, else
          // if Group item - toggle expanded state
          if not IsGroupItem(FFocusedItem) then
            AddToSelection(FFocusedItem)
          else
            ToggleExpandedItem(FFocusedItem);
        end;
      end;
    end;
    VK_HOME, VK_PRIOR, VK_UP, VK_LEFT,
    VK_RIGHT, VK_DOWN, VK_NEXT, VK_END:
    begin
      // simple move focus & select items if need
      Message.Result := 0;
      MoveFocusDirection := CharCodeToMoveFocusDirection(Message.CharCode);
      FocusMoved := MoveFocus(MoveFocusDirection,
        (ssShift in Shift) and (soMultiSelect in FSelectionOptions),
        ClearSelectionBeforeMove or not (soMultiSelect in FSelectionOptions),
        not (ssCtrl in Shift) or not (soMultiSelect in FSelectionOptions),
        ssCtrl in Shift);
    end;
  else
    inherited;
  end;
  // start searching
  if (Shift * [ssCtrl, ssAlt] = []) and (Message.CharCode >= 32) then
    DoStateChange([lsSearchPending]);
  if FocusMoved then
  begin
    // expand Group item (if it) & scroll to it
    ScrollIntoView(FFocusedItem, (toAutoExpandOnFocus in FAutoOptions) and
      ExpandItem(FFocusedItem));
    Invalidate;
  end;
end;

procedure TOutlookGroupedList.DblClick;
var
  Pt: TPoint;
  HitInfo: TOGLItemHitInfo;
begin
  inherited;
  // try to locate Item over mouse cursor
  if Assigned(FOnItemDblClick) then
  begin
    GetCursorPos(Pt);
    Pt := ScreenToClient(Pt);
    GetHitTestInfoAt(Pt.X, Pt.Y, True, HitInfo);
    if HitInfo.HitTest in [htOnItem, htOnItemLabel] then
      FOnItemDblClick(Self, HitInfo.HitItem, Pt.X, Pt.Y);
  end;
end;

procedure TOutlookGroupedList.WMLButtonDown(var Message: TWMLButtonDown);
var
  HitInfo: TOGLItemHitInfo;
begin
  DoStateChange([lsLeftButtonDown]);
  inherited;
  GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo);
  HandleMouseDown(Message, HitInfo);
  if Assigned(FOnMouseDownOnItem) then
    FOnMouseDownOnItem(self, mbLeft, KeysToShiftState(Message.Keys) * [ssShift, ssCtrl, ssAlt], Message.XPos, Message.YPos, HitInfo);
end;

procedure TOutlookGroupedList.WMLButtonUp(var Message: TWMLButtonUp);
  function LeftButtonUp: Boolean;
  begin
    if GetSystemMetrics(SM_SWAPBUTTON) = 0 then
      Result:= GetAsyncKeyState(VK_LBUTTON) >= 0
    else 
      Result:= GetAsyncKeyState(VK_RBUTTON) >= 0;  
 end;

var
  HitInfo: TOGLItemHitInfo;
  LBtnUp: Boolean;
begin
  LBtnUp := LeftButtonUp;
  if LBtnUp then
  begin
    DoStateChange([], [lsLeftButtonDown]);
    GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo);
    HandleMouseUp(Message, HitInfo);
  end;

  inherited;

  if LBtnUp and not FInternalBtnUp and Assigned(FOnMouseUpOnItem) then
    FOnMouseUpOnItem(self, mbLeft, KeysToShiftState(Message.Keys) * [ssShift, ssCtrl, ssAlt], Message.XPos, Message.YPos, HitInfo);
    
  if FInternalBtnUp then
    FInternalBtnUp := False;
end;

procedure TOutlookGroupedList.WMRButtonDown(var Message: TWMRButtonDown);
var
  HitInfo: TOGLItemHitInfo;
begin
  DoStateChange([lsRightButtonDown]);
  inherited;
  if soRightClickSelect in FSelectionOptions then
  begin
    GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo);
    HandleMouseDown(Message, HitInfo);
  end;
end;

procedure TOutlookGroupedList.WMRButtonUp(var Message: TWMRButtonUp);
var
  HitInfo: TOGLItemHitInfo;
begin
  DoStateChange([], [lsRightButtonDown]);
  Application.CancelHint;
  (* reset selection state already here, before the inherited
     handler opens the default menu *)
  if (lsMouseSelecting in FStates) and Assigned(PopupMenu) then
    ClearSelectionRect;
  inherited;

  GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo);
  if soRightClickSelect in FSelectionOptions then
  begin
    //GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo);
    HandleMouseUp(Message, HitInfo);

    // CC: kh
    if not FInternalBtnUp and Assigned(FOnMouseUpOnItem) then
      FOnMouseUpOnItem(self, mbRight, KeysToShiftState(Message.Keys) * [ssShift, ssCtrl, ssAlt], Message.XPos, Message.YPos, HitInfo);

    if FInternalBtnUp then
      FInternalBtnUp := False;
  end
  else if Assigned(HitInfo.HitItem) then
  begin
    if not FInternalBtnUp and Assigned(FOnMouseUpOnItem) then
      FOnMouseUpOnItem(self, mbRight, KeysToShiftState(Message.Keys) * [ssShift, ssCtrl, ssAlt], Message.XPos, Message.YPos, HitInfo);
  end;
end;

procedure TOutlookGroupedList.HandleMouseDown(var Message: TWMMouse;
  const HitInfo: TOGLItemHitInfo);
var
  OldFocusedItem: POGLItem;
  ShiftState: TShiftState;
  AutoDrag,
  IsHitItem,
  IsHitItemSelected,
  MultiSelect,
  ShiftEmpty,
  //ShiftTotalEmpty,
  MustUpdateClientRect,
  AltPressed: Boolean;
begin
  // On MDI-child forms doesn't set focus to OGL after mouse down.
  if not Focused and CanFocus then
    Windows.SetFocus(Handle);

  // Expand/collapse item
  if HitInfo.HitTest = htOnExpandButton then
  begin
    ToggleExpandedItem(HitInfo.HitItem);
    ScrollIntoView(HitInfo.HitItem, True);
    Invalidate;
    Exit;
  end;

  // Translate keys and filter out shift, control and alt keys
  ShiftState := KeysToShiftState(Message.Keys) * [ssShift, ssCtrl, ssAlt];
  if ssAlt in ShiftState then
  begin
    AltPressed := True;
    Exclude(ShiftState, ssAlt);
  end else
    AltPressed := False;

  IsHitItem := (HitInfo.HitTest in [htOnItemLabel, htOnItem]);
  IsHitItemSelected := IsHitItem and IsItemSelected(HitInfo.HitItem);

  MultiSelect := soMultiSelect in FSelectionOptions;
  ShiftEmpty := ShiftState = [];

  //ShiftTotalEmpty := ShiftEmpty and not AltPressed;

  if IsGroupItem(HitInfo.HitItem) and not MultiSelect then
    IsHitItemSelected := false;

  // query the application to learn if dragging may start now (if set to dmManual)
  AutoDrag := (DragMode = dmAutomatic) or Dragging;

  if Assigned(HitInfo.HitItem) and (not AutoDrag) and (DragMode = dmManual) then
    AutoDrag := DoOLEDragAllowed(HitInfo.HitItem);

  // immediate or on MouseMove selection clearing
  MustUpdateClientRect := False;

  if (not MultiSelect or ShiftEmpty) and
      IsHitItem and not IsHitItemSelected then
    MustUpdateClientRect := ClearSelection
  else
    if (not MultiSelect {or ShiftTotalEmpty}) then
    begin
      DoStateChange([lsSelectionClearing]);
    end;

  // Keep start selection's mouse position
  FFromSelPoint := SmallPointToPoint(Message.Pos);
  Inc(FFromSelPoint.Y, VertScrollBar.Position);
  // User starts a mouse selecting with a selection rectangle
  if not IsHitItem and MultiSelect and (FRootItem.ChildCount > 0) then
  begin
    SetCapture(Handle);
    DoStateChange([lsMouseSelecting]);
    FDrawSelShiftState := ShiftState;
    if AltPressed then
      Include(FDrawSelShiftState, ssAlt);
    FToSelPoint := FFromSelPoint;
  end;

  // Handle selection and item focus change
  if IsHitItem then
  begin
    if MultiSelect and (not ShiftEmpty) then
      HandleMouseDownSelection(FFocusedItem, HitInfo.HitItem,
        ShiftState, AutoDrag) else
    begin
      if ShiftEmpty then
        FSelectedAnchorItem := HitInfo.HitItem;
      if not IsHitItemSelected then
        AddToSelection(HitInfo.HitItem, True)
      else
      begin
        if not (MultiSelect and IsHitItemSelected) then  // FF: Unselect selected item issue
        ClearSelection;
        AddToSelection(HitInfo.HitItem, false);
      end;

    end;
    OldFocusedItem := FFocusedItem;
    DoSetFocusedItem(HitInfo.HitItem, False);
    if ShiftEmpty and ((not IsHitItemSelected) or
      (IsHitItemSelected and (OldFocusedItem <> FFocusedItem))) then
    begin
      InvalidateItem(OldFocusedItem);
      InvalidateItem(FFocusedItem);
    end;
  end;

  if MustUpdateClientRect then
    InvalidateRect(Handle, nil, False);

  // initialize DragNDrop operation, but not immediately 
  if AutoDrag and (FStates * [lsLeftButtonDown {, lsRightButtonDown}] <> []) then
    BeginDrag(False);
end;

procedure TOutlookGroupedList.HandleMouseUp(var Message: TWMMouse;
  const HitInfo: TOGLItemHitInfo);
var
  IsHitItem,
  MultiSelect,
  ShiftPressed, CtrlPressed: Boolean;
  i, j: Integer;
  remsel: boolean;
begin
  if not (lsVCLDragPending in FStates) then
  begin
    MultiSelect := soMultiSelect in FSelectionOptions;
    IsHitItem := (HitInfo.HitTest in [htOnItemLabel, htOnItem]);
    ShiftPressed := (GetKeyState(VK_SHIFT) and $8000 = $8000);
    CtrlPressed := (GetKeyState(VK_CONTROL) and $8000 = $8000);
    // UnSelected the items on mouse click when Shift and Ctrl are not pressed
    remsel := false;
    if MultiSelect and IsHitItem and not IsGroupItem(HitInfo.HitItem) and not Dragging then
    begin
      if not ShiftPressed and not CtrlPressed then
      begin
        j := 1;
        i := FSelection.Count - j;
        while i > -1 do
        begin
          if (FSelection[i] <> HitInfo.HitItem) then
          begin
            RemoveFromSelection(FSelection[i], False);
            remsel := true;
          end
          else
          begin
            if (j >= 2) then
              break;
            j := 2;
          end;
          i := FSelection.Count - j;
        end;
      end;
    end;

    if lsSelectionClearing in FStates then
    begin
      if (HitInfo.HitItem = nil) and not (lsOLEDragging in FStates) then
        ClearSelection
      else
        begin
          ClearSelection;
          AddToSelection(FFocusedItem, True);
        end;
    end;

    if (lsToggleFocusedSelection in FStates) and
       (HitInfo.HitItem = FFocusedItem) then
    begin
      InvertSelection(HitInfo.HitItem, True);

      if not IsItemSelected(HitInfo.HitItem) then
      begin
        if Assigned(FOnSelectionChange) then
          FOnSelectionChange(Self);
      end;
    end;

    if not (soMultiSelect in FSelectionOptions) or
       ((soMultiSelect in FSelectionOptions) and
        not (lsMouseSelected in FStates)) then
      ScrollIntoView(HitInfo.HitItem);

    ClearSelectionRect;

    if (HitInfo.HitItem = nil) or remsel then
      if Assigned(FOnSelectionChange) then
      begin
        FOnSelectionChange(Self);
      end;

    DoStateChange([], [lsOLEDragPending, lsOLEDragging,
      lsSelectionClearing, lsToggleFocusedSelection]);

    DoTrackMouseEvent(False);
    Invalidate;
  end;
  ReleaseCapture;
end;

procedure TOutlookGroupedList.HandleMouseSelection(var UpdateRect: TRect;
  const OldSelRect, NewSelRect: TRect);

  function DisplayRectToChildRect(DisplayRect: TRect): TRect;
  begin
    Result := Rect(
      DisplayRect.Left + FThumbnailOptions.HorizSpacing - 2,
      DisplayRect.Top + FThumbnailOptions.VertSpacing - 2,
      DisplayRect.Right - FThumbnailOptions.HorizSpacing + 2,
      DisplayRect.Top + FThumbnailOptions.HorizSpacing +
        FThumbnailOptions.ThumbnailHeight + 2);
  end;

type
  TSelectionAction = (saAdd, saRemove, saInvert);
var
  SelectionAction: TSelectionAction;
  GroupItem,ChildItem: POGLItem;
  MinY,MaxY: Integer;
  X,Y: Integer;
  DisplayRect,ChildRect: TRect;
  HitInfo: TOGLItemHitInfo;
  MustInvalidateGroupItem: Boolean;
  IsInOldRect,IsInNewRect: Boolean;
begin
  // convert Shift to items select action
  if ssShift in FDrawSelShiftState then
    SelectionAction := saAdd else
  if ssCtrl in FDrawSelShiftState then
    SelectionAction := saInvert else
  if ssAlt in FDrawSelShiftState then
    SelectionAction := saRemove
  else
    SelectionAction := saAdd;
  // try to find the top-left corner's Child item
  GroupItem := nil;
  ChildItem := nil;
  MinY := UpdateRect.Top;
  MaxY := UpdateRect.Bottom;
  X := GetChildItemWidth div 2;
  Y := MinY;
  while Y < MaxY do
  begin
    GetHitTestInfoAt(X, Y, False, HitInfo);
    if (HitInfo.HitTest = htOnItem) and
      (not IsGroupItem(HitInfo.HitItem)) then
    begin
      // Child item
      ChildItem := HitInfo.HitItem;
      GroupItem := ChildItem.Parent;
      Break;
    end;
    Inc(Y);
  end;
  if ChildItem <> nil then
  begin
    // we found the top-left corner's Child item
    while GroupItem <> nil do
    begin
      MustInvalidateGroupItem :=
        GroupItem.ChildSelectedCount = GroupItem.ChildCount;
      if ChildItem = nil then
        ChildItem := GroupItem.FirstChild;
      while ChildItem <> nil do
      begin
        DisplayRect := GetDisplayRect(ChildItem);
        ChildRect := DisplayRectToChildRect(DisplayRect);
        // disallow process Items below UpdateRect.Bottom
        if ChildRect.Top > MaxY then Break;
        IsInOldRect := IsIntersectRect(ChildRect, OldSelRect);
        IsInNewRect := IsIntersectRect(ChildRect, NewSelRect);
        if IsInOldRect xor IsInNewRect then
        begin
          (* check the item only if it's location changed from OldSelRect
             to NewSelRect and vice versa. *)
          UnionRect(UpdateRect, UpdateRect, DisplayRect);
          case SelectionAction of
            saInvert:
              InvertSelection(ChildItem, False);
            saAdd:
              if IsInNewRect then
                AddToSelection(ChildItem)
              else
                RemoveFromSelection(ChildItem);
            saRemove:
              if IsInNewRect then
                RemoveFromSelection(ChildItem)
              else
                AddToSelection(ChildItem);
          end;
        end;
        ChildItem := ChildItem.NextSibling;
      end;
      MustInvalidateGroupItem := MustInvalidateGroupItem or
        (GroupItem.ChildSelectedCount = GroupItem.ChildCount);
      if MustInvalidateGroupItem then
      begin
        DisplayRect := GetDisplayRect(GroupItem);
        UnionRect(UpdateRect, UpdateRect, DisplayRect);
      end;
      GroupItem := GroupItem.NextSibling;
    end;
  end;
end;

procedure TOutlookGroupedList.HandleMouseDownSelection(
  LastFocused, HitItem: POGLItem;
  Shift: TShiftState; DragPending: Boolean);
begin
  // Ctrl key down
  if ssCtrl in Shift then
  begin
    if ssShift in Shift then
    begin
      FSupressOnSelect := true;
      AddSelectionFromAnchorItemTo(HitItem);
      Invalidate;
      if Assigned(FOnSelectionChange) then
        FOnSelectionChange(self);
      FSupressOnSelect := false;
    end else
    begin
      FSelectedAnchorItem := HitItem;
      (* Delay selection change if a drag operation is pending.
         Otherwise switch selection state here. *)
      if DragPending then
        DoStateChange([lsToggleFocusedSelection])
      else
        InvertSelection(HitItem, True);
    end;
  end else
  // Shift key down
  if ssShift in Shift then
  begin
    if FSelectedAnchorItem = nil then
      FSelectedAnchorItem := FirstGroupItem;
    // select item range
    if Assigned(FSelectedAnchorItem) then
    begin

      FSupressOnSelect := true;
      AddSelectionFromAnchorItemTo(HitItem);
      Invalidate;
      if Assigned(FOnSelectionChange) then
        FOnSelectionChange(self);
      FSupressOnSelect := false;
    end;
  end else
  // any other case
  begin
    if not IsItemSelected(HitItem) then
    begin
      AddToSelection(HitItem);
      InvalidateItem(HitItem);
    end;
    FSelectedAnchorItem := HitItem;
  end;
end;

procedure TOutlookGroupedList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  tmpVertScrollOffset: Integer;
  MustUpdateClientRect: Boolean;
  UpdateRect: TRect;
  OldSelRect: TRect;
  NewSelRect: TRect;
begin
  // really start dragging if the mouse has been moved more than the threshold
  if (lsOLEDragPending in FStates) and
     ((Abs(FFromSelPoint.X - X) >= FDragThreshold) or
      (Abs(FFromSelPoint.Y - Y - VertScrollBar.Position) >= FDragThreshold)) then
    DoOLEDragging(FFromSelPoint) else
  if lsMouseSelecting in FStates then
  begin
    // try to autoscroll when mouse selecting
    if VertScrollBar.Range > 0 then
    begin
      if Y > ClientHeight then
        tmpVertScrollOffset := (Y - ClientHeight) div 2 else
      if Y < 0 then
        tmpVertScrollOffset := Y
      else
        tmpVertScrollOffset := 0;
      if tmpVertScrollOffset = 0 then
        DoTrackMouseEvent(False) else
      begin
        VertScrollBar.Position := VertScrollBar.Position + tmpVertScrollOffset;
        DoTrackMouseEvent(True);
      end;
    end;
    // update Selected Rect
    MustUpdateClientRect := False;
    OldSelRect := PointsToRect(FFromSelPoint, FToSelPoint);
    Inc(Y, VertScrollBar.Position);
    FToSelPoint := Point(X, Y);
    NewSelRect := PointsToRect(FFromSelPoint, FToSelPoint);
    UnionRect(UpdateRect, OldSelRect, NewSelRect);
    if (Abs(NewSelRect.Right - NewSelRect.Left) > Mouse.DragThreshold) or
       (Abs(NewSelRect.Bottom - NewSelRect.Top) > Mouse.DragThreshold) then
    begin
      if lsSelectionClearing in FStates then
      begin
        DoStateChange([], [lsSelectionClearing]);
        MustUpdateClientRect := ClearSelection;
      end;
      DoStateChange([lsMouseSelected]);
      HandleMouseSelection(UpdateRect, OldSelRect, NewSelRect);
    end;
    if MustUpdateClientRect then
      InvalidateRect(Handle, nil, False) else
    begin
      OffsetRect(UpdateRect, 0, -VertScrollBar.Position);
      if UpdateRect.Top < 0 then
        UpdateRect.Top := 0;
      if UpdateRect.Bottom > ClientHeight then
        UpdateRect.Bottom := ClientHeight;
      InvalidateRect(Handle, @UpdateRect, False);
    end;
  end else
    inherited;
end;

procedure TOutlookGroupedList.FontOnChange;
begin
  AdjustItemsFont;
  if not (csDesigning in ComponentState) then
    Invalidate;
end;

procedure TOutlookGroupedList.ImagesOnChange(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
    Invalidate;
end;

procedure TOutlookGroupedList.RenderDragImage(
  const Hotspot: TPoint; const DataObject: IDataObject);
(* Hotspot is the position of the mouse in client coordinates *)
var
  AvailableClientWidth: Integer;
  DragSourceHelper: IDragSourceHelper;
  DragInfo: TSHDragImage;
  DragImageRect: TRect;
  DragImage: TBitmap;
begin
  if (FSelection.Count > 0) and (doOLEShowDragImage in FDragOptions) and
    Assigned(DataObject) and Succeeded(CoCreateInstance(CLSID_DragDropHelper,
      nil, CLSCTX_INPROC_SERVER, IID_IDragSourceHelper, DragSourceHelper)) then
  begin
    // convert HotSpot to absolute coordinates
    DragImageRect := Rect(
      HotSpot.X - FDragImageWidth div 2,
      HotSpot.Y - FDragImageHeight div 2,
      HotSpot.X + FDragImageWidth div 2,
      HotSpot.Y + FDragImageHeight div 2);
    AvailableClientWidth := FItemsPerRow * GetChildItemWidth;
    if DragImageRect.Right > AvailableClientWidth then
    begin
      DragImageRect.Right := AvailableClientWidth;
      DragImageRect.Left := DragImageRect.Right - FDragImageWidth;
    end;
    if DragImageRect.Left < 0 then
    begin
      DragImageRect.Left := 0;
      DragImageRect.Right := Min(FDragImageWidth, AvailableClientWidth);
    end;
    if DragImageRect.Bottom > VertScrollBar.Range then
    begin
      DragImageRect.Bottom := VertScrollBar.Range;
      DragImageRect.Top := DragImageRect.Bottom -
        Min(FDragImageHeight, ClientHeight);
    end;
    if DragImageRect.Top < 0 then
    begin
      DragImageRect.Top := 0;
      DragImageRect.Bottom := Min(FDragImageHeight, ClientHeight);
    end;
    DragImage := TBitmap.Create;
    try
      DragImage.PixelFormat := pf32Bit;
      DragImage.Width := DragImageRect.Right - DragImageRect.Left;
      DragImage.Height := DragImageRect.Bottom - DragImageRect.Top;
      // crear the Image's background
      DragImage.Canvas.Brush.Color := clFuchsia;
      DragImage.Canvas.FillRect(Rect(0, 0, DragImage.Width, DragImage.Height));
      // offset to DragImage TopLeft point
      SetViewportOrgEx(DragImage.Canvas.Handle,
        - DragImageRect.Left, - DragImageRect.Top +
        VertScrollBar.Position, nil);
      InternalPaintWindow(DragImage.Canvas, True);
      SetViewportOrgEx(DragImage.Canvas.Handle, 0, 0, nil);
      // supply the drag source helper with our drag image
      DragInfo.sizeDragImage.cx := DragImage.Width;
      DragInfo.sizeDragImage.cy := DragImage.Height;
      DragInfo.ptOffset.x := HotSpot.X - DragImageRect.Left;
      DragInfo.ptOffset.y := HotSpot.Y - DragImageRect.Top;
      DragInfo.hbmpDragImage := CopyImage(DragImage.Handle, IMAGE_BITMAP,
        DragImage.Width, DragImage.Height, LR_COPYRETURNORG);
      DragInfo.crColorKey := ColorToRGB(clFuchsia);
      if not Succeeded(DragSourceHelper.InitializeFromBitmap(
          DragInfo, DataObject)) then
        DeleteObject(DragInfo.hbmpDragImage);
    finally
      FreeAndNil(DragImage);
    end;
  end;
end;

function TOutlookGroupedList.GetFirstGroupItem: POGLItem;
begin
  Result := RootItem.FirstChild;
end;

function TOutlookGroupedList.GetFirstSelectedItem: POGLItem;
var
  GroupItem: POGLItem;
  ChildItem: POGLItem;
begin
  Result := nil;
  if FSelection.Count = 0 then Exit;
  GroupItem := GetFirstGroupItem;
  while GroupItem <> nil do
  begin
    if IsItemSelected(GroupItem) then
    begin
      Result := GroupItem;
      Exit;
    end;
    if GroupItem.ChildSelectedCount > 0 then
    begin
      ChildItem := GroupItem.FirstChild;
      while ChildItem <> nil do
      begin
        if IsItemSelected(ChildItem) then
        begin
          Result := ChildItem;
          Exit;
        end;
        ChildItem := ChildItem.NextSibling;
      end;
    end;
    GroupItem := GroupItem.NextSibling;
  end;
end;

function TOutlookGroupedList.GetSelectedCount: Integer;
begin
  Result := FSelection.Count;
end;

procedure TOutlookGroupedList.SetImages(const Value: TCustomImageList);
begin
  if FImages <> nil then
    FImages.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  if not (csDesigning in ComponentState) then
    Invalidate;
end;

procedure TOutlookGroupedList.SetBorderStyle(const Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TOutlookGroupedList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FImages then
      Images := nil;
end;

function TOutlookGroupedList.GetChildItemWidth: Word;
begin
  if FViewStyle = vsList then
    Result := ClientWidth
  else
    (*
      |<------------>|<-------------->|<------------>|
      | HorizSpacing | ThumbnailWidth | HorizSpacing |
    *)
    Result := 2 * FThumbnailOptions.HorizSpacing +
      FThumbnailOptions.ThumbnailWidth;
end;

procedure TOutlookGroupedList.AdjustItemsSize;

  procedure __SetChildItemHeight;
  begin
    if FViewStyle = vsList then
    begin
      if Assigned(FOnGetChildItemHeight) then
      begin
        FBmpCanvas.Canvas.Font.Assign(Self.Font);
        FOnGetChildItemHeight(Self, FBmpCanvas.Canvas, FChildItemHeight)
      end else
        FChildItemHeight := FThumbnailOptions.ThumbnailHeight;
    end else
    begin
      (*
      -------------------------^
      ThumbnailHeight          |
      -------------------------|
      ThumbnailVertTextSpacing |
      -------------------------|
      ThumbnailCaptionHeight   |
      -------------------------|
      VertSpacing              |
      -------------------------V
      *)
      FChildItemHeight :=
        FThumbnailOptions.ThumbnailHeight +
        OGLChildItemVertTextSpacing +
        FThumbnailCaptionHeight +
        FThumbnailOptions.VertSpacing;
    end;
  end;

  function __CalcTotalHeight(AvailClientWidth: Cardinal;
    AllowExit: Boolean): Boolean;
  var
    Item: POGLItem;
  begin
    Result := False;
    FTotalItemsHeight := 0;
    if FViewStyle = vsList then
      FItemsPerRow := 1 else
    begin
      FItemsPerRow := AvailClientWidth div GetChildItemWidth;
      if FItemsPerRow < 1 then
        FItemsPerRow := 1;
    end;
    Item := FirstGroupItem;
    while Item <> nil do
    begin
      Item.ChildHeight := ((Item.ChildCount div FItemsPerRow) +
        Byte(Item.ChildCount mod FItemsPerRow > 0)) * ChildItemHeight +
        Byte(Item.ChildCount > 0) * Cardinal(FThumbnailOptions.VertSpacing) *
        Byte(FViewStyle = vsThumbnails);
      Inc(FTotalItemsHeight, FGroupItemHeight);
      if nsExpanded in Item.States then
        Inc(FTotalItemsHeight, Item.ChildHeight);
      if AllowExit and (FTotalItemsHeight > Cardinal(ClientHeight)) then Exit;
      Item := Item.NextSibling;
    end;
    Result := True;
  end;

var
  tmpClientWidth: Word;
  tmpGroupFontHeight: Word;
begin
  if FUpdateCount > 0 then
    Exit;

  FBmpCanvas.Canvas.Font.Assign(Self.Font);
  FBmpCanvas.Canvas.Font.Style := [fsBold];
  tmpGroupFontHeight := FBmpCanvas.Canvas.TextHeight(',^');
  //FGroupItemHeight := 21 + tmpGroupFontHeight;    // change for setGroupItemHeight
  if ShowNodes then
  begin
    FGroupItemExpandButtonVertOffset := FGroupItemHeight - 7 - tmpGroupFontHeight +
      (tmpGroupFontHeight - FBmpMinus.Height) div 2;
  end
  else
  begin
    FGroupItemExpandButtonVertOffset := FGroupItemHeight - 7 - tmpGroupFontHeight +
      (tmpGroupFontHeight) div 2;
  end;
  __SetChildItemHeight;
  tmpClientWidth := ClientWidth;
  if VertScrollBar.IsScrollBarVisible then
  begin
    Inc(tmpClientWidth, GetSystemMetrics(SM_CXVSCROLL));
    VertScrollBar.Tracking := True;
  end;

  if not __CalcTotalHeight(tmpClientWidth, True) then
    __CalcTotalHeight(tmpClientWidth -
      Cardinal(GetSystemMetrics(SM_CXVSCROLL)), False);
  VertScrollBar.Range := FTotalItemsHeight;
  if (not (csDesigning in ComponentState)) and
     (not (csLoading in ComponentState)) then
    Invalidate;
end;

procedure TOutlookGroupedList.AdjustItemsFont;
begin
  FBmpCanvas.Canvas.Font.Assign(Self.Font);
  FThumbnailCaptionHeight := FBmpCanvas.Canvas.TextHeight(',^');
  AdjustItemsSize;
end;

procedure TOutlookGroupedList.Resize;
begin
  inherited;
  AdjustItemsSize;
end;

function TOutlookGroupedList.HTMLPaint(Canvas: TCanvas; s: string;
  fr: TRect): Boolean;
var
  anchorval, stripval, focusanchor: string;
  xsize, ysize, hyperlinks, mouselink: integer;
  hoverrect: TRect;
begin
  HTMLDrawEx(Canvas, s, fr, {FImages}nil, 0, 0, -1, 0, 0, True, False, False, False, False,
                       False, False, 1.0, clBlue, clNone,clNone, clNone, anchorval,stripval,focusanchor,
                       XSize,YSize,HyperLinks,MouseLink,HoverRect, {FImageCache} nil, {FContainer} nil,0);

  if (YSize + 4 < (fr.Bottom - fr.Top)) then
    fr.Top := fr.Top + ((fr.Bottom - fr.Top) - YSize);
  Result := HTMLDrawEx(Canvas, s, fr, {FImages}nil, 0, 0, -1, 0, 0, False, False, False, False, False,
                       False, False, 1.0, clBlue, clNone,clNone, clNone, anchorval,stripval,focusanchor,
                       XSize,YSize,HyperLinks,MouseLink,HoverRect, {FImageCache} nil, {FContainer} nil,0);
end;

procedure TOutlookGroupedList.DrawItem(TargetCanvas: TCanvas;
  TargetRect: TRect; Item: POGLItem);

  procedure __ClipRect(ClipRect: TRect);
  var
    hClipRegion: HRGN;
  begin
    LPtoDP(TargetCanvas.Handle, ClipRect, 2);
    hClipRegion := CreateRectRgnIndirect(ClipRect);
    SelectClipRgn(TargetCanvas.Handle, hClipRegion);
    DeleteObject(hClipRegion);
  end;

const
  LineBitsDotted: array [0..8] of Word =
    ($55, $AA, $55, $AA, $55, $AA, $55, $AA, $55);
var
  IsFocused: Boolean;
  PatternBitmap: HBITMAP;
  DottedBrush: HBRUSH;
  Caption: String;
  rc,rc1: TRect;
  tw: Integer;
  ChildCount: String;
  ImageIndex: Integer;
begin
  if (Item = nil) or (Item = FRootItem) then
    Exit;
  
  IsFocused := Focused;
  
  if IsGroupItem(Item) then
  begin
    // Group item -------------------------
    // draw horiz line
    TargetCanvas.Pen.Color := FGroupLineColor; // $00E0A47B;
    TargetCanvas.Rectangle(TargetRect.Left, TargetRect.Bottom - 2,
      TargetRect.Right, TargetRect.Bottom);
    // draw bg only for selected
    if Item = FDropTargetGroup then
    begin
      TargetCanvas.Brush.Color := $00C2EEFF; // light orange
      rc := Rect(TargetRect.Left, TargetRect.Top,
        TargetRect.Right, TargetRect.Bottom - 2);
      TargetCanvas.FillRect(rc);
    end else
    if IsItemSelected(Item) then
    begin
      if (IsFocused or not HideSelection) and (Item = FFocusedItem) then
        TargetCanvas.Brush.Color := GroupSelectionColor //FIntSelectionColor // $00C56A31  // dark blue
      else
      begin
        if (GroupColor <> clNone) then
          TargetCanvas.Brush.Color := GroupColor
        else if (IsFocused or not HideSelection) then
          TargetCanvas.Brush.Color := GroupSelectionColor // $00F6E9E0; // light blue
        else
          TargetCanvas.Brush.Color := Color;
      end;
      rc := Rect(TargetRect.Left, TargetRect.Top,
        TargetRect.Right, TargetRect.Bottom - 2);
      TargetCanvas.FillRect(rc);
    end
    else if (GroupColor <> clNone) then
    begin
      TargetCanvas.Brush.Color := GroupColor;
      
      rc := Rect(TargetRect.Left, TargetRect.Top,
        TargetRect.Right, TargetRect.Bottom - 2);
      TargetCanvas.FillRect(rc);
    end;
    // draw focus rect
    if IsFocused and (Item = FFocusedItem) then
    begin
      PatternBitmap := CreateBitmap(8, 8, 1, 1, @LineBitsDotted);
      try
        DottedBrush := CreatePatternBrush(PatternBitmap);
        try
          if IsItemSelected(Item) then
          begin
            if (IsFocused or not HideSelection) then
            begin
              TargetCanvas.Brush.Color := $003A95CE;  // dark orange
              TargetCanvas.Font.Color := $00C56A31;   // dark blue
            end else
            begin
              TargetCanvas.Brush.Color := $007AAECE; // light orange
              TargetCanvas.Font.Color := $00F6E9E0;  // light blue
            end;
          end else
          begin
            TargetCanvas.Brush.Color := clBlack;
            TargetCanvas.Font.Color := clWhite;
          end;
          rc := Rect(TargetRect.Left, TargetRect.Top,
            TargetRect.Right, TargetRect.Top + 1);
          Windows.FillRect(TargetCanvas.Handle, rc, DottedBrush);
          rc := Rect(TargetRect.Left, TargetRect.Top,
            TargetRect.Left + 1, TargetRect.Bottom - 1);
          Windows.FillRect(TargetCanvas.Handle, rc, DottedBrush);
          rc := Rect(TargetRect.Right - 1, TargetRect.Top,
            TargetRect.Right, TargetRect.Bottom - 1);
          Windows.FillRect(TargetCanvas.Handle, rc, DottedBrush);
          if IsItemSelected(Item) then
          begin
            if (IsFocused or not HideSelection) then
            begin
              TargetCanvas.Brush.Color := $001F5B84; // very dark orange
              TargetCanvas.Font.Color := $00E0A47B;  // dark blue
            end else
            begin
              TargetCanvas.Brush.Color := $007AAECE; // light orange
              TargetCanvas.Font.Color := $00F6E9E0;  // light blue
            end;
          end else
          begin
            TargetCanvas.Brush.Color := clBlack;
            TargetCanvas.Font.Color := clWhite;
          end;
          rc := Rect(TargetRect.Left, TargetRect.Bottom - 3,
            TargetRect.Right, TargetRect.Bottom - 2);
          Windows.FillRect(TargetCanvas.Handle, rc, DottedBrush);
        finally
          DeleteObject(DottedBrush);
        end;
      finally
        DeleteObject(PatternBitmap);
      end;
    end;
    // draw blue line above each group if previous group has child and expanded
    if IsGroupExpanded(Item.PrevSibling) and ((Item <> FFocusedItem) or
      ((Item = FFocusedItem) and (not IsFocused))) then
    begin
      if Item = FDropTargetGroup then
        TargetCanvas.Brush.Color := $00C2EEFF // light orange
      else
        TargetCanvas.Brush.Color := $00F9F0EA;
      rc := Rect(TargetRect.Left, TargetRect.Top,
        TargetRect.Right, TargetRect.Top + 1);
      TargetCanvas.FillRect(rc);
    end;
    TargetCanvas.Brush.Style := bsClear;
    // draw +/- button
    if Item.ChildCount > 0 then
    begin
      if ShowNodes then
      begin
        if nsExpanded in Item.States then
          TargetCanvas.Draw(TargetRect.Left + 3, TargetRect.Top +
            FGroupItemExpandButtonVertOffset, FBmpMinus)
        else
          TargetCanvas.Draw(TargetRect.Left + 3, TargetRect.Top +
            FGroupItemExpandButtonVertOffset, FBmpPlus);
      end;
    end;
    TargetCanvas.Font.Assign(GroupFont{Self.Font});
    ChildCount := IntToStr(Item.ChildCount);
    if (Item.ChildSelectedCount > 0) and (soMultiSelect in FSelectionOptions) then
      ChildCount := IntToStr(Item.ChildSelectedCount) + ' / ' + ChildCount;
    // draw group caption
    if Assigned(FOnGetCaption) then
    begin
      Caption := '';
      FOnGetCaption(Self, Item, Caption);
    end else
      Caption := '  ';
    //TargetCanvas.Font.Style := [fsBold];
    if IsItemSelected(Item) then
    begin
      if {(Item = FFocusedItem) and }(IsFocused or not HideSelection) then
    begin
      if (GroupSelectionTextColor <> clNone) then
        TargetCanvas.Font.Color := GroupSelectionTextColor //clWhite
        else
          TargetCanvas.Font.Color := GroupFont.Color;
      end
      else
        TargetCanvas.Font.Color := GroupFont.Color;
    end
    else
      TargetCanvas.Font.Color := GroupFont.Color; // $00B96837;
    rc := Rect(TargetRect.Left + 22, TargetRect.Top,
      TargetRect.Right - 12 - TargetCanvas.TextWidth(ChildCount),
      TargetRect.Bottom - 7);
    // try to draw Image
    if (FImages <> nil) and Assigned(FOnGetGroupImageIndex) then
    begin
      ImageIndex := -1;
      FOnGetGroupImageIndex(Self, Item, ImageIndex);
      if ImageIndex <> -1 then
      begin
        FImages.Draw(TargetCanvas, rc.Left, TargetRect.Bottom - 6 -
          FImages.Height, ImageIndex, True);
        Inc(rc.Left, FImages.Width + 4);
      end;
    end;

    if (Pos('<', Caption) > 0) then
    begin
      HTMLPaint(TargetCanvas, Caption, rc);
      TargetCanvas.Brush.Style := bsClear;
    end
    else
      DrawText(TargetCanvas.Handle, PChar(Caption), -1, rc,
      DT_LEFT or DT_BOTTOM or DT_SINGLELINE or DT_NOCLIP or DT_END_ELLIPSIS or DT_NOPREFIX);
    // draw group child items count
    //TargetCanvas.Font.Style := [];
    TargetCanvas.Font.Assign(GroupCountFont);
    if IsItemSelected(Item) {and (Item = FFocusedItem)} and (IsFocused or not HideSelection) then
      TargetCanvas.Font.Color := GroupSelectionTextColor// clWhite
    else
      TargetCanvas.Font.Color := GroupCountFont.Color; // clBlack;
    rc := Rect(TargetRect.Left + 22, TargetRect.Top,
      TargetRect.Right - 7, TargetRect.Bottom - 7);
    if GroupShowCount then
      DrawText(TargetCanvas.Handle, PChar(ChildCount), -1, rc,
        DT_RIGHT or DT_BOTTOM or DT_SINGLELINE or DT_NOCLIP or DT_NOPREFIX);
  end else
  begin
    // Child item -------------------------
    // draw thumbnail's rectangle
    TargetCanvas.Brush.Style := bsClear;
    rc := TargetRect;
    if FViewStyle = vsList then
    begin
      // List -----------------------------
      if IsItemSelected(Item) then
      begin
        TargetCanvas.Brush.Style := bsSolid;
        if not IsFocused and HideSelection then
          TargetCanvas.Brush.Color := $00F6E9E0 else
        (* if Item = FFocusedItem then *)
          TargetCanvas.Brush.Color := FIntSelectionColor; //$00B96837
        (* else
          TargetCanvas.Brush.Color := $00E0A47B *);
        TargetCanvas.FillRect(rc);
      end else
      if Item.Parent = FDropTargetGroup then
      begin
        // fill when Dropping over Item's Group
        TargetCanvas.Brush.Style := bsSolid;
        TargetCanvas.Brush.Color := $00EDFAFF; // very light orange
        TargetCanvas.FillRect(rc);
      end;
      // draw bottom horiz line
      TargetCanvas.Pen.Color := clBtnFace;
      TargetCanvas.MoveTo(rc.Left, rc.Bottom - 1);
      TargetCanvas.LineTo(rc.Right, rc.Bottom - 1);
      Dec(rc.Bottom);
    end else
    begin
      // Thumbnail ------------------------
      InflateRect(rc, -FThumbnailOptions.HorizSpacing,
        -FThumbnailOptions.VertSpacing);
      rc.Bottom := rc.Top + FThumbnailOptions.ThumbnailHeight;
      if IsItemSelected(Item) then
      begin
        if Item <> FFocusedItem then
        begin
          TargetCanvas.Pen.Width := 2;
          Inc(rc.Right);
          Inc(rc.Bottom);
        end else
        begin
          TargetCanvas.Pen.Width := 3;
          InflateRect(rc, 1, 1);
        end;
        if not IsFocused and HideSelection then
          TargetCanvas.Pen.Color := $00C8D0D4 else // $00D8E9EC else (* clBtnFace *)
        if Item = FFocusedItem then
          TargetCanvas.Pen.Color := $00B96837
        else
          TargetCanvas.Pen.Color := $00E0A47B;
        if IsFocused or not HideSelection then
        begin
          TargetCanvas.Brush.Style := bsSolid;
          TargetCanvas.Brush.Color := $00EFE7E4;
        end;
        TargetCanvas.Rectangle(rc.Left, rc.Top, rc.Right, rc.Bottom);
        TargetCanvas.Pen.Width := 1;
        if Item <> FFocusedItem then
        begin
          Dec(rc.Right);
          Dec(rc.Bottom);
        end else
          InflateRect(rc, -1, -1);
      end else
      begin
        TargetCanvas.Pen.Width := 1;
        TargetCanvas.Pen.Color := $00C8D0D4; // $00D8E9EC; (* clBtnFace *)
        TargetCanvas.Rectangle(rc.Left, rc.Top, rc.Right, rc.Bottom);
      end;
      InflateRect(rc, -1, -1);
    end;
    // draw the thumbnail's client area
    if Assigned(FOnDrawItem) then
    begin
      TargetCanvas.Brush.Style := bsSolid;
      // prepare to draw thumbnail's caption
      TargetCanvas.Font.Assign(Self.Font);
      __ClipRect(rc);
      FOnDrawItem(Self, TargetCanvas, rc, Item);
      SelectClipRgn(TargetCanvas.Handle, 0);
      TargetCanvas.Brush.Style := bsClear;
    end;
    if FViewStyle = vsList then
    begin
      // List -----------------------------
      // draw focus rect
      if IsFocused and (Item = FFocusedItem) then
      begin
        DrawFocusRect(TargetCanvas.Handle, rc);
        Inc(rc.Bottom);
      end;
    end else
    begin
      // Thumbnail ------------------------
      InflateRect(rc, 1, 1);
      // prepare to draw thumbnail's caption
      TargetCanvas.Font.Assign(Self.Font);
      rc := Rect(TargetRect.Left + 4, rc.Bottom + OGLChildItemVertTextSpacing,
        TargetRect.Right - 4, rc.Top + FThumbnailCaptionHeight);
      // draw thumbnail's caption
      if Assigned(FOnGetCaption) then
      begin
        Caption := '';
        FOnGetCaption(Self, Item, Caption);
      end else
        Caption := '      ';
      DrawText(TargetCanvas.Handle, PChar(Caption), -1,
        rc, DrawOGLItemCaptionFlags or DT_CALCRECT);
      tw := (GetChildItemWidth - (rc.Right - rc.Left)) div 2;
      rc.Left := TargetRect.Left + tw;
      rc.Right := TargetRect.Right - tw;
      // fill the background of selected thumbnail
      InflateRect(rc, 2, 1);
      if IsItemSelected(Item) then
      begin
        TargetCanvas.Brush.Style := bsSolid;
        TargetCanvas.Brush.Color := TargetCanvas.Pen.Color;
        TargetCanvas.FillRect(rc);
      end;
      // draw focus rect
      if IsFocused and (Item = FFocusedItem) then
      begin
        PatternBitmap := CreateBitmap(8, 8, 1, 1, @LineBitsDotted);
        try
          DottedBrush := CreatePatternBrush(PatternBitmap);
          try
            TargetCanvas.Font.Color := TargetCanvas.Brush.Color;
            if IsItemSelected(Item) then
              TargetCanvas.Brush.Color := $0095DBF5 // light orange
            else
              TargetCanvas.Brush.Color := clBlack;
            rc1 := Rect(rc.Left, rc.Top, rc.Right, rc.Top + 1);
            Windows.FillRect(TargetCanvas.Handle, rc1, DottedBrush);
            rc1 := Rect(rc.Left, rc.Top, rc.Left + 1, rc.Bottom - 1);
            Windows.FillRect(TargetCanvas.Handle, rc1, DottedBrush);
            rc1 := Rect(rc.Right - 1, rc.Top, rc.Right, rc.Bottom - 1);
            Windows.FillRect(TargetCanvas.Handle, rc1, DottedBrush);
            rc1 := Rect(rc.Left, rc.Bottom - 1, rc.Right, rc.Bottom);
            Windows.FillRect(TargetCanvas.Handle, rc1, DottedBrush);
          finally
            DeleteObject(DottedBrush);
          end;
        finally
          DeleteObject(PatternBitmap);
        end;
      end;
      TargetCanvas.Brush.Style := bsClear;
      if IsItemSelected(Item) and (IsFocused or not HideSelection) then
        TargetCanvas.Font.Color := clWhite
      else
        TargetCanvas.Font.Color := clBlack;
      InflateRect(rc, -2, 0);
      // draw thumbnail's caption
      DrawText(TargetCanvas.Handle, PChar(Caption), -1,
        rc, DrawOGLItemCaptionFlags);
    end;
  end;
end;

procedure TOutlookGroupedList.PaintWindow(DC: HDC);
var
  tmpCanvas: TCanvas;
begin
  tmpCanvas := TCanvas.Create;
  try
    tmpCanvas.Handle := DC;
    InternalPaintWindow(tmpCanvas);
  finally
    FreeAndNil(tmpCanvas);
  end;
end;

procedure TOutlookGroupedList.InternalPaintWindow(Canvas: TCanvas;
  OnlySelectedItems: Boolean = False);
var
  VertScrollBarPos: Integer;
  ItemDrawed: Boolean;
  rc,rcChild: TRect;
  GroupItem,ChildItem: POGLItem;

  function MustDrawItem(AItem: POGLItem; AItemRect: TRect): Boolean;
  begin
    OffsetRect(AItemRect, 0, VertScrollBarPos);
    Result :=
      ((AItemRect.Top >= VertScrollBarPos) and
       (AItemRect.Top < VertScrollBarPos + ClientHeight)) or
      ((AItemRect.Bottom >= VertScrollBarPos) and
       (AItemRect.Bottom < VertScrollBarPos + ClientHeight));
  end;

begin
  ItemDrawed := False;
  VertScrollBarPos := VertScrollBar.Position;

  // fill background
  if not OnlySelectedItems then
  begin
    Canvas.Brush.Color := Color;//clWhite;
    Canvas.FillRect(ClientRect);
  end;

  rc := Rect(0, - VertScrollBarPos, ClientWidth,
    -VertScrollBarPos + FGroupItemHeight);

  try
    // cycle through the Group items
    GroupItem := FirstGroupItem;
    while GroupItem <> nil do
    begin
      if MustDrawItem(GroupItem, rc) then
      begin
        if (not OnlySelectedItems) or
         (OnlySelectedItems and (IsItemSelected(GroupItem))) then
        begin
          ItemDrawed := True;
          DrawItem(Canvas, rc, GroupItem);
        end;
      end else
      if ItemDrawed then Exit;
      // check the Item.ChildCount - i.e. real Thumbnails
      if (nsExpanded in GroupItem.States) and (GroupItem.ChildCount > 0) then
      begin
        Inc(rc.Top, FGroupItemHeight);
        rc.Bottom := rc.Top + Longint(GroupItem.ChildHeight);
        ChildItem := GroupItem.FirstChild;
        rcChild := Rect(rc.Left, rc.Top,
          rc.Left + GetChildItemWidth,
          rc.Top + ChildItemHeight);
        Canvas.Pen.Color := clBlue;
        while ChildItem <> nil do
        begin
          // draw Thumbnail item, go to next one & recalc rcChild
          if MustDrawItem(ChildItem, rcChild) then
          begin
            if (not OnlySelectedItems) or
              (OnlySelectedItems and (IsItemSelected(ChildItem))) then
            begin
              ItemDrawed := True;
              DrawItem(Canvas, rcChild, ChildItem);
            end;
          end else
          if ItemDrawed then Exit;
          ChildItem := ChildItem.NextSibling;
          if rcChild.Right + GetChildItemWidth <= ClientWidth then
            OffsetRect(rcChild, GetChildItemWidth, 0) else
          begin
            rcChild.Left := 0;
            rcChild.Right := GetChildItemWidth;
            OffsetRect(rcChild, 0, ChildItemHeight);
          end;
        end;
        rc.Top := rc.Bottom;
        rc.Bottom := rc.Top + FGroupItemHeight;
      end else
        OffsetRect(rc, 0, FGroupItemHeight);
      GroupItem := GroupItem.NextSibling;
    end;
  finally
    // draw Selection Rect
    if not OnlySelectedItems then
      PaintSelectionRectangle(Canvas);
  end;
end;

procedure TOutlookGroupedList.PaintSelectionRectangle(TargetCanvas: TCanvas);
var
  SelectionRect: TRect;
  TextColorBackup: COLORREF; // used to restore forground and background colors
  BackColorBackup: COLORREF; // when drawing a selection rectangle
  bf: TBlendFunction;        // used to draw
  tmpBmp: TBitmap;           // blend selection rectangle
begin
  if (not (lsMouseSelecting in FStates)) then Exit;
  SelectionRect := GetSelectionDisplayRect;
  if IsRectEmptyEx(SelectionRect) then Exit;
  if (FDrawSelectionMode = dsmBlendedRectangle) or
    ((FDrawSelectionMode = dsmThemeAware) {and (ThemeServices.ThemesEnabled)}) then
  begin
    // modern alpha blended style
    tmpBmp := TBitmap.Create;
    try
      tmpBmp.Width := 48;
      tmpBmp.Height := 48;
      tmpBmp.Canvas.Brush.Color := $00C56A31;
      tmpBmp.Canvas.FillRect(Rect(0, 0, tmpBmp.Width, tmpBmp.Height));
      bf.BlendOp := AC_SRC_OVER;
      bf.BlendFlags := 0;
      bf.SourceConstantAlpha := 127;
      bf.AlphaFormat := 0;
      {$IFDEF BCB}
      AlphaBlend(TargetCanvas.Handle,
        SelectionRect.Left, SelectionRect.Top,
        SelectionRect.Right - SelectionRect.Left,
        SelectionRect.Bottom - SelectionRect.Top,
        tmpBmp.Canvas.Handle, 0, 0, tmpBmp.Width, tmpBmp.Height, bf);
      {$ELSE}
      Windows.AlphaBlend(TargetCanvas.Handle,
        SelectionRect.Left, SelectionRect.Top,
        SelectionRect.Right - SelectionRect.Left,
        SelectionRect.Bottom - SelectionRect.Top,
        tmpBmp.Canvas.Handle, 0, 0, tmpBmp.Width, tmpBmp.Height, bf);
      {$ENDIF}
      TargetCanvas.Brush.Color := $00C56A31;
      TargetCanvas.FrameRect(SelectionRect);
    finally
      FreeAndNil(tmpBmp);
    end;
  end else
  begin
    // classical selection rectangle using dotted borderlines
    TextColorBackup := GetTextColor(TargetCanvas.Handle);
    SetTextColor(TargetCanvas.Handle, $FFFFFF);
    BackColorBackup := GetBkColor(TargetCanvas.Handle);
    SetBkColor(TargetCanvas.Handle, 0);
    TargetCanvas.DrawFocusRect(SelectionRect);
    SetTextColor(TargetCanvas.Handle, TextColorBackup);
    SetBkColor(TargetCanvas.Handle, BackColorBackup);
  end;
end;

function TOutlookGroupedList.InvalidateItem(Item: POGLItem;
  const IncludeChildren: Boolean = False): TRect;
(* Initiates repaint of the given item and returns the just
   invalidated rectangle. *)
begin
  if (FUpdateCount = 0) and (Item <> nil) then
  begin
    Result := GetDisplayRect(Item);
    InflateRect(Result, 1, 1);
    OffsetRect(Result, 0, -VertScrollBar.Position);
    if IncludeChildren and IsGroupItem(Item) then
      Inc(Result.Bottom, Item.ChildHeight);
    InvalidateRect(Handle, @Result, False);
  end;
end;

procedure TOutlookGroupedList.GetHitTestInfoAt(X, Y: Integer;
  const Relative: Boolean; var HitInfo: TOGLItemHitInfo);
var
  Caption: String;
  Pt: TPoint;
  ChildRect: TRect;
  ChildItem: POGLItem;
  xIndex,IndexAt: Integer;
  DisplayRect: TRect;
begin
  HitInfo.GroupItem := FirstGroupItem;
  HitInfo.HitItem := nil;
  HitInfo.HitTest := htInClientArea;
  if Relative then
  begin
    // convert from relative to absolute coordinates
    Inc(X, HorzScrollBar.Position);
    Inc(Y, VertScrollBar.Position);
  end;
  Pt := Point(X, Y);
  if not PtInRect(Rect(0, 0, ClientWidth, VertScrollBar.Range), Pt) then
  begin
    // set the last Group item if mouse below last existence Group
    if Pt.Y >= VertScrollBar.Range then
      HitInfo.GroupItem := RootItem.LastChild;
    Exit;
  end;
  HitInfo.GroupItem := FirstGroupItem;
  while HitInfo.GroupItem <> nil do
  begin
    // check for Group item
    DisplayRect := GetDisplayRect(HitInfo.GroupItem);
    if PtInRect(DisplayRect, Pt) then
    begin
      HitInfo.HitItem := HitInfo.GroupItem;
      // check whether the point is within +/- button
      if PtInRect(Rect(DisplayRect.Left + 2,
          DisplayRect.Top + FGroupItemExpandButtonVertOffset - 2,
          DisplayRect.Left + 3 + FBmpPlus.Width + 2, DisplayRect.Top +
          FGroupItemExpandButtonVertOffset + FBmpPlus.Height + 2), Pt) and ShowNodes then
        HitInfo.HitTest := htOnExpandButton
      else
        HitInfo.HitTest := htOnItem;
      Exit;
    end;
    // check for Child item
    // + whether the Point is in client area of Group item
    if IsGroupExpanded(HitInfo.GroupItem) and PtInRect(Rect(DisplayRect.Left,
      DisplayRect.Bottom, DisplayRect.Right,
      DisplayRect.Bottom + Integer(HitInfo.GroupItem.ChildHeight)), Pt) then
    begin
      // + whether the Point is in Child items client area
      if PtInRect(Rect(DisplayRect.Left, DisplayRect.Bottom,
        DisplayRect.Left + FItemsPerRow * GetChildItemWidth,
        DisplayRect.Bottom + Integer(HitInfo.GroupItem.ChildHeight)), Pt) then
      begin
        xIndex := X div GetChildItemWidth;
        IndexAt := (Y - DisplayRect.Bottom) div
          ChildItemHeight * FItemsPerRow + xIndex;
        if (IndexAt >= 0) and (IndexAt < Integer(HitInfo.GroupItem.ChildCount)) then
        begin
          ChildItem := HitInfo.GroupItem.FirstChild;
          while (ChildItem <> nil) and (Integer(ChildItem.Index) < IndexAt) do
            ChildItem := ChildItem.NextSibling;
          if (ChildItem <> nil) and (Integer(ChildItem.Index) = IndexAt) then
          begin
            DisplayRect := GetDisplayRect(ChildItem);
            // check whether the Point is in Child rect, i.e. inside of border
            ChildRect := DisplayRect;
            if ViewStyle = vsThumbnails then
            begin
              InflateRect(ChildRect, -FThumbnailOptions.HorizSpacing + 4,
                -FThumbnailOptions.VertSpacing);
              ChildRect.Bottom := ChildRect.Top + FThumbnailOptions.ThumbnailHeight;
              Dec(ChildRect.Top, 3);
              Inc(ChildRect.Bottom, 3);
            end;
            if PtInRect(ChildRect, Pt) then
            begin
              HitInfo.HitItem := ChildItem;
              HitInfo.HitTest := htOnItem;
              Exit;
            end;
            // check whether the Point is in Thumb's Caption rect
            FBmpCanvas.Canvas.Font.Assign(Self.Font);
            Dec(ChildRect.Bottom, 3);
            ChildRect := Rect(DisplayRect.Left + 4,
              ChildRect.Bottom + OGLChildItemVertTextSpacing,
              DisplayRect.Right - 4,
              ChildRect.Top + FThumbnailCaptionHeight);
            // draw thumbnail's caption
            if Assigned(FOnGetCaption) then
            begin
              Caption := '';
              FOnGetCaption(Self, ChildItem, Caption);
            end else
              Caption := '      ';
            DrawText(FBmpCanvas.Canvas.Handle, PChar(Caption), -1,
              ChildRect, DrawOGLItemCaptionFlags or DT_CALCRECT);
            xIndex := (GetChildItemWidth -
              (ChildRect.Right - ChildRect.Left)) div 2;
            ChildRect.Left := DisplayRect.Left + xIndex - 1;
            ChildRect.Right := DisplayRect.Right - xIndex + 1;
            InflateRect(ChildRect, 2, 1);
            if PtInRect(ChildRect, Pt) then
            begin
              HitInfo.HitItem := ChildItem;
              HitInfo.HitTest := htOnItemLabel;
              Exit;
            end;
          end;
        end;
      end;
      // point is in client area of Group item, but not over it's Child items
      Exit;
    end;
    HitInfo.GroupItem := HitInfo.GroupItem.NextSibling;
  end;
  // set the last Group item if mouse below any existence groups
  HitInfo.GroupItem := RootItem.LastChild;
end;

function TOutlookGroupedList.GetItemAt(X, Y: Integer;
  const Relative: Boolean): POGLItem;
var
  HitInfo: TOGLItemHitInfo;
begin
  GetHitTestInfoAt(X, Y, Relative, HitInfo);
  Result := HitInfo.HitItem;
end;

function TOutlookGroupedList.IsItem2AfterItem1(Item1,Item2: POGLItem): Boolean;
begin
  // Item1 is Group
  if IsGroupItem(Item1) then
  begin
    if IsGroupItem(Item2) then
      // Item2 is Group - compare Item2 & Item1 indexes
      Result := Item2.Index > Item1.Index
    else
      // Item2 is Child - compare Item2.Parent & Item1 indexes
      Result := Item2.Parent.Index >= Item1.Index;
  end else
  // Item1 is Child
  if Item1 <> nil then
  begin
    if IsGroupItem(Item2) then
      // Item2 is Group - compare Item2 & Item1.Parent indexes
      Result := Item2.Index > Item1.Parent.Index else
    begin
      // Item2 is Child
      Result := (Item2.Parent.Index > Item1.Parent.Index) or
       ((Item2.Parent.Index = Item1.Parent.Index) and
        (Item2.Index > Item1.Index));
    end;
  end else
    Result := False;
end;

function TOutlookGroupedList.GetCaption(Item: POGLItem): String;
begin
  Result := '';
  if Assigned(FOnGetCaption) then
    FOnGetCaption(Self, Item, Result);
end;

function TOutlookGroupedList.CaptionToItem(ACaption: String;
  const GroupItem: POGLItem = nil): POGLItem;
(* If GroupItem = nil then we search the Group item for specified ACaption,
   else we search the Child item for specified ACaption and GroupItem. *)
var
  tmpCaption: String;
begin
  if not Assigned(FOnGetCaption) then
  begin
    Result := nil;
    Exit;
  end;
  ACaption := AnsiLowerCase(ACaption);
  if GroupItem = nil then
    Result := GetFirstGroupItem
  else
    Result := GroupItem.FirstChild;
  while Result <> nil do
  begin
    FOnGetCaption(Self, Result, tmpCaption);
    if AnsiLowerCase(tmpCaption) = ACaption then Exit;
    Result := Result.NextSibling;
  end;
  Result := nil;
end;

procedure TOutlookGroupedList.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TOutlookGroupedList.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    SetUpdateState(False);
    AdjustItemsSize;
  end;
end;

procedure TOutlookGroupedList.SetUpdateState(const Updating: Boolean);
begin
  SendMessage(Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if not Updating then
    Refresh;
end;

procedure TOutlookGroupedList.InitRootItem;
begin
  if FRootItem <> nil then Exit;
  FRootItem := AllocMem(SizeOf(TOGLItem));
  // Indication that this item is the root item.
  FRootItem.States := [nsExpanded];
  FRootItem.Parent := nil;
  FRootItem.PrevSibling := FRootItem;
  FRootItem.NextSibling := FRootItem;
end;

function TOutlookGroupedList.AddItem(ParentItem: POGLItem = nil): POGLItem;
begin
  Result := nil;
  if ParentItem = nil then
    ParentItem := FRootItem
  else
  // to avoid adding Child items to Child items
    if (ParentItem <> nil) and (ParentItem.Parent <> FRootItem) then
      Exit;

  Result := AllocMem(SizeOf(TOGLItem) + FItemDataSize);

  if ParentItem = FRootItem then
    Result.States := [nsGroup];

  if toAutoExpandOnCreate in FAutoOptions then
    Include(Result.States, nsExpanded);

  Result.Parent := ParentItem;

  Result.PrevSibling := ParentItem.LastChild;
  Result.NextSibling := nil;
  Result.FirstChild := nil;
  Result.LastChild := nil;

  if ParentItem.LastChild <> nil then
    ParentItem.LastChild.NextSibling := Result;

  if ParentItem.FirstChild = nil then
    ParentItem.FirstChild := Result;

  ParentItem.LastChild := Result;

  if Result.PrevSibling = nil then
    Result.Index := 0
  else
    Result.Index := Result.PrevSibling.Index + 1;

  Inc(ParentItem.ChildCount);
  Inc(FRootItem.TotalCount);

  if Assigned(FOnInitItem) then
    FOnInitItem(Self, Result);

  AdjustItemsSize;
  // set focus to the first group & autosort OGL
  if (FRootItem.ChildCount = 1) and (ParentItem = FRootItem) then
    DoSetFocusedItem(Result, True);
  if (toAutoSort in FAutoOptions) and (FSortColumn > InvalidColumn) and
    ((ParentItem = FRootItem) or IsGroupItem(Result)) then
    Sort(ParentItem, FSortColumn, FSortDirection);
end;

function TOutlookGroupedList.InsertItem(ParentItem: POGLItem; Index: Integer): POGLItem;
var
  ogl: POGLItem;
  i: integer;

begin
  Result := nil;
  if ParentItem = nil then
    ParentItem := FRootItem
  else
  // to avoid adding Child items to Child items
    if (ParentItem <> nil) and (ParentItem.Parent <> FRootItem) then
      Exit;

  Result := AllocMem(SizeOf(TOGLItem) + FItemDataSize);

  if ParentItem = FRootItem then
    Result.States := [nsGroup];

  if toAutoExpandOnCreate in FAutoOptions then
    Include(Result.States, nsExpanded);

  Result.Parent := ParentItem;

  if (Index < 0) then
    Index := 0;

  if (Index > Integer(ParentItem.ChildCount)) then
    Index := ParentItem.ChildCount;

  if (Index = 0) then
  begin
    if Assigned(ParentItem.FirstChild) then
    begin
      ogl := ParentItem.FirstChild;
      Result.NextSibling := ogl;
      Result.PrevSibling := ogl.PrevSibling;
      ogl.PrevSibling := Result;
    end
    else
    begin
      ParentItem.FirstChild := Result;
      Result.NextSibling := nil;
      Result.PrevSibling := nil;
    end;

    Result.FirstChild := nil;
    Result.LastChild := nil;
    Result.Index := 0;

    ParentItem.FirstChild := Result;
  end
  else
  if (Index > 0) and (Index < Integer(ParentItem.ChildCount)) then
  begin
    i := 0;

    ogl := ParentItem.FirstChild;

    while (i < Index - 1) do
    begin
      ogl := ogl^.NextSibling;
      inc(i);
    end;

    Result.NextSibling := ogl.NextSibling;

    if Assigned(ogl.NextSibling) then
      ogl.NextSibling.PrevSibling := Result;

    ogl.NextSibling := Result;

    Result.Index := ogl.Index + 1;
    Result.PrevSibling := ogl;
    Result.FirstChild := nil;
    Result.LastChild := nil;
  end
  else
  if (Index = Integer(ParentItem.ChildCount)) then
  begin
    Result.PrevSibling := ParentItem.LastChild;
    Result.NextSibling := nil;
    Result.FirstChild := nil;
    Result.LastChild := nil;

    if ParentItem.LastChild <> nil then
      ParentItem.LastChild.NextSibling := Result;

    if ParentItem.FirstChild = nil then
      ParentItem.FirstChild := Result;

    ParentItem.LastChild := Result;
  end;

  // correct indexes 
  ogl := Result.NextSibling;
  while assigned(ogl) do
  begin
    ogl.Index := ogl.Index + 1;
    ogl := ogl.NextSibling;
  end;


  Inc(ParentItem.ChildCount);
  Inc(FRootItem.TotalCount);

  if Assigned(FOnInitItem) then
    FOnInitItem(Self, Result);

  AdjustItemsSize;
  // set focus to the first group & autosort OGL
  if (FRootItem.ChildCount = 1) and (ParentItem = FRootItem) then
    DoSetFocusedItem(Result, True);
  if (toAutoSort in FAutoOptions) and (FSortColumn > InvalidColumn) and
    ((ParentItem = FRootItem) or IsGroupItem(Result)) then
    Sort(ParentItem, FSortColumn, FSortDirection);
end;


procedure TOutlookGroupedList.DeleteItem(Item: POGLItem; Reindex: Boolean = True);
var
  ParentClearing: Boolean;
  ChildItem: POGLItem;
begin
  if (Item <> nil) and (Item <> FRootItem) then
  begin
    ParentClearing := nsClearing in Item.Parent.States;
    RemoveFromSelection(Item);
    if FHintData.HitInfo.HitItem = Item then
      Application.CancelHint;
    if Item = FFocusedItem then
    begin
      // set new Focused item before delete specified Item
      if Item.NextSibling <> nil then
        DoSetFocusedItem(Item.NextSibling, True) else
      if IsGroupExpanded(Item.PrevSibling) then
        DoSetFocusedItem(Item.PrevSibling.LastChild, True) else
      if Item.PrevSibling <> nil then
        DoSetFocusedItem(Item.PrevSibling, True) else
      if not IsGroupItem(Item) then
        DoSetFocusedItem(Item.Parent, True)
      else
        DoSetFocusedItem(nil, True);
    end;
    if IsGroupItem(Item) then
    begin
      Include(Item.States, nsClearing);
      // auto-clear all Child items from Group item
      ChildItem := Item.FirstChild;
      while ChildItem <> nil do
      begin
        // INFO: we mustn't reindex Child items of deleting Group item
        DeleteItem(ChildItem, False);
        ChildItem := Item.FirstChild;
      end;
    //  ChildItem := nil;
    end;
    //else
    //begin
    ChildItem := Item.NextSibling;
    //end;
    if Item.Parent.FirstChild = Item then
      Item.Parent.FirstChild := Item.NextSibling;
    if Item.Parent.LastChild = Item then
      Item.Parent.LastChild := Item.PrevSibling;
    if Item.PrevSibling <> nil then
      Item.PrevSibling.NextSibling := Item.NextSibling;
    if Item.NextSibling <> nil then
      Item.NextSibling.PrevSibling := Item.PrevSibling;
    Dec(Item.Parent.ChildCount);
    Dec(FRootItem.TotalCount);
    if Assigned(FOnFreeItem) then
      FOnFreeItem(Self, Item);
    ReAllocMem(Item, 0);
    // reindex all next Child items
    if Reindex then
      while ChildItem <> nil do
      begin
        Dec(ChildItem.Index);
        ChildItem := ChildItem.NextSibling;
      end;
    // recalc Group item's ClientHeight
    if not ParentClearing then
    begin
      AdjustItemsSize;
      if not (csDestroying in ComponentState) then
        Invalidate;
    end;
  end;
end;

procedure TOutlookGroupedList.DeleteSelectedItems(ReIndex: Boolean = False);
(* Delete all currently selected items (including Child items of Group one). *)
begin
  BeginUpdate;
  try
    // if selected any Group items - they will be deleted with its Child items
    while FSelection.Count > 0 do
      DeleteItem(FSelection[0], ReIndex);
  finally
    EndUpdate;
  end;
end;

procedure TOutlookGroupedList.Clear;
(* Clear all exists items within OGL. *)
var
  Item: POGLItem;
begin
  if not (csDestroying in ComponentState) then
    BeginUpdate;
  try
    ClearSelectionRect;
    ClearSelection;
    FFocusedItem := nil;
    if lsHintShowed in FStates then
      Application.CancelHint;
    FLastHintHitInfo.HitItem := Pointer($FFFFFF); // to allow default hint showed
    FLastHintHitInfo.HitTest := htInClientArea;
    Exclude(FStates, lsHintShowed);
    Include(FRootItem.States, nsClearing);
    Item := FirstGroupItem;
    while Item <> nil do
    begin
      // WARNING: we mustn't reindex Child items when deleting each Group item
      DeleteItem(Item, False);
      Item := FirstGroupItem;
    end;
  finally
    Exclude(FRootItem.States, nsClearing);
    if not (csDestroying in ComponentState) then
    begin
      AdjustItemsSize;
      EndUpdate;
    end;
  end;
end;

function TOutlookGroupedList.GetItemData(Item: POGLItem): Pointer;
(* Returns the address of the user defined data area in the item. *)
begin
  Assert(FItemDataSize > 0, 'ItemDataSize not initialized.');
  if (FItemDataSize <= 0) or (Item = nil) or (Item = FRootItem) then
    Result := nil
  else
    Result := PChar(@Item.Data);
end;


procedure TOutlookGroupedList.SelectItem(Item: POGLItem);
begin
  if Item <> nil then
  begin
    Item.States  := Item.States + [nsSelected];
  end;
end;

function TOutlookGroupedList.IsItemSelected(Item: POGLItem): Boolean;
(* Check wether specified Item is selected. *)
begin
  Result := (Item <> nil) and (Item <> FRootItem) and (nsSelected in Item.States);
end;

function TOutlookGroupedList.IsGroupItem(Item: POGLItem): Boolean;
(* Check wether specified Item is Group item. *)
begin
  Result := (Item <> nil) and
    ((Item.Parent = FRootItem) or (nsGroup in Item.States));
end;

function TOutlookGroupedList.IsGroupExpanded(Item: POGLItem): Boolean;
(* Whether specified Item is Group? *)
begin
  Result := IsGroupItem(Item) and
    (nsExpanded in Item.States) and (Item.ChildCount > 0);
end;

function TOutlookGroupedList.ExpandItem(Item: POGLItem): Boolean;
(* Expand specified Item. *)
begin
  Result := IsGroupItem(Item) and (Item.ChildCount > 0) and
    (not (nsExpanded in Item.States));
  if Result then
  begin
    Include(Item.States, nsExpanded);
    Inc(FTotalItemsHeight, Item.ChildHeight);
    if not (nsExpanding in FRootItem.States) then
    begin
      VertScrollBar.Range := FTotalItemsHeight;
      Invalidate;
    end;

    DoExpand(Item);
  end;
end;

function TOutlookGroupedList.CollapseItem(Item: POGLItem): Boolean;
(* Collapse specified Item. *)
begin
  Result := IsGroupItem(Item) and (Item.ChildCount > 0) and
    (nsExpanded in Item.States);
  if Result then
  begin
    Exclude(Item.States, nsExpanded);
    Dec(FTotalItemsHeight, Item.ChildHeight);
    if not (nsCollapsing in FRootItem.States) then
    begin
      VertScrollBar.Range := FTotalItemsHeight;
      Invalidate;
    end;

    DoCollaps(Item);    
  end;
end;

procedure TOutlookGroupedList.ToggleExpandedItem(Item: POGLItem);
(* Expand or Collapse specified Item. *)
begin
  if IsGroupExpanded(Item) then
    CollapseItem(Item)
  else
    ExpandItem(Item);
end;

function TOutlookGroupedList.ExpandAll: Boolean;
(* Expand all Group items. *)
var
  GroupItem: POGLItem;
begin
  Result := False;
  BeginUpdate;
  try
    Include(FRootItem.States, nsExpanding);
    GroupItem := FirstGroupItem;
    while GroupItem <> nil do
    begin
      Result := ExpandItem(GroupItem) or Result;
      GroupItem := GroupItem.NextSibling;
    end;
  finally
    Exclude(FRootItem.States, nsExpanding);
    VertScrollBar.Range := FTotalItemsHeight;
    EndUpdate;
  end;
end;

function TOutlookGroupedList.CollapseAll: Boolean;
(* Collapse all Group items. *)
var
  GroupItem: POGLItem;
begin
  Result := False;
  BeginUpdate;
  try
    Include(FRootItem.States, nsCollapsing);
    GroupItem := FirstGroupItem;
    while GroupItem <> nil do
    begin
      Result := CollapseItem(GroupItem) or Result;
      GroupItem := GroupItem.NextSibling;
    end;
    // move focus to Group item if selected Child item
    if (FFocusedItem <> nil) and (not IsGroupItem(FFocusedItem)) then
      FFocusedItem := FFocusedItem.Parent;
  finally
    Exclude(FRootItem.States, nsCollapsing);
    VertScrollBar.Range := FTotalItemsHeight;
    EndUpdate;
  end;
end;

function TOutlookGroupedList.GetDisplayRect(Item: POGLItem): TRect;
(* Function's result is only display rectangle - i.e. full item's area.
   For Group item - it's equal to clien area of item, for Child item -
   it includes HorizSpacing & VertSpacing area. *)
var
  GroupItem: POGLItem;
begin
  Result := Rect(0, 0, 0, 0);
  if (Item = nil) or (Item = FRootItem) then Exit;
  // calc the Height of area with 
  if IsGroupItem(Item) then
    GroupItem := Item.PrevSibling
  else
  begin
    if Assigned(Item.Parent) then    
      GroupItem := Item.Parent.PrevSibling
    else
      GroupItem := nil;
  end;
  while GroupItem <> nil do
  begin
    Inc(Result.Top, FGroupItemHeight);
    if nsExpanded in GroupItem.States then
      Inc(Result.Top, GroupItem.ChildHeight);
    GroupItem := GroupItem.PrevSibling;
  end;
  Result.Bottom := Result.Top + FGroupItemHeight;
  if IsGroupItem(Item) then
    Result.Right := ClientWidth else
  begin
    // add the offset of Child item
    Inc(Result.Top, FGroupItemHeight +
      Item.Index div FItemsPerRow * ChildItemHeight);
    Result.Bottom := Result.Top + ChildItemHeight;
    Result.Left := Item.Index mod FItemsPerRow * GetChildItemWidth;
    Result.Right := Result.Left + GetChildItemWidth;
  end;
end;

function TOutlookGroupedList.ScrollIntoView(Item: POGLItem;
  AutoScrollOnExpand: Boolean = False): Boolean;
(* Scroll to specified Item, showing as many child items in view as possible
   accordingly to AutoScrollOnExpand param (it's usefully after
   expanding a Group item). *)
var
  OldVertScrollBarPos: Integer;
  DisplayRect: TRect;
begin
  // disallow to scroll to invisible Child item
  Result := False;
  if (Item = nil) or (Item = FRootItem) then Exit;
  DisplayRect := GetDisplayRect(Item);
  AutoScrollOnExpand := AutoScrollOnExpand and IsGroupExpanded(Item) and
    (toAutoScrollOnExpand in FAutoOptions);
  OldVertScrollBarPos := VertScrollBar.Position;
  try
    if AutoScrollOnExpand then
    begin
      (* if Group item with its Child items not placed within ClientHeight -
         simple scroll to the Group item. *)
      if DisplayRect.Top < VertScrollBar.Position then
        VertScrollBar.Position := DisplayRect.Top else
      if DisplayRect.Bottom + Integer(Item.ChildHeight) >
         (VertScrollBar.Position + ClientHeight) then
      begin
        if (DisplayRect.Bottom - DisplayRect.Top + Integer(Item.ChildHeight)) <
            ClientHeight then
          VertScrollBar.Position :=
            DisplayRect.Bottom + Integer(Item.ChildHeight) - ClientHeight
         else
           VertScrollBar.Position := DisplayRect.Top;
      end;
    end else
    begin
      // scroll to Item be within ClientHeight
      if (not IsGroupItem(Item)) and (Item.Index < FItemsPerRow) then
        Dec(DisplayRect.Top, FGroupItemHeight);
      if VertScrollBar.Position > DisplayRect.Top then
        VertScrollBar.Position := DisplayRect.Top else
      if VertScrollBar.Position < DisplayRect.Bottom - ClientHeight then
        VertScrollBar.Position := DisplayRect.Bottom - ClientHeight;
    end;
  finally
    Result := VertScrollBar.Position <> OldVertScrollBarPos;
  end;
end;

procedure TOutlookGroupedList.AddToSelection(Item: POGLItem;
  IncludeChildItems: Boolean = False);
(* Add specified Item to selection, including Child items when
   IncludeItems = True. *)
var
  ChildItem: POGLItem;
begin
  if (Item = nil) or (Item = FRootItem) or IsItemSelected(Item) then
    Exit;

  Include(Item.States, nsSelected);
  FSelection.Add(Item);
  // inc ChildSelectedCount of Item.Parent (i.e. Group item) &
  // select Item.Parent when all Child items selected
  if not IsGroupItem(Item) then
  begin
    if Item.Parent.ChildSelectedCount < Item.Parent.ChildCount then
      Inc(Item.Parent.ChildSelectedCount);
    if Item.Parent.ChildSelectedCount = Item.Parent.ChildCount then
      AddToSelection(Item.Parent, False);

    if (FUpdateSelCount = 0) and not FSupressOnSelect then
    begin
      if Assigned(FOnSelectionChange) then
        FOnSelectionChange(Self);
      if FAllowOneOnSelect then
        FSupressOnSelect := True;
    end;
  end
  else
  // select all Child items of Group Item
  if IncludeChildItems and (soMultiSelect in FSelectionOptions) then
  begin
    if Assigned(FOnSelectionChange) and not FSupressOnSelect then
    begin
      FOnSelectionChange(Self);
      if FAllowOneOnSelect then
        FSupressOnSelect := True;
    end;    

    inc(FUpdateSelCount);
    ChildItem := Item.FirstChild;
    while ChildItem <> nil do
    begin
      AddToSelection(ChildItem);
      ChildItem := ChildItem.NextSibling;
    end;
    dec(FUpdateSelCount);
  end;
end;

procedure TOutlookGroupedList.RemoveFromSelection(Item: POGLItem;
  IncludeChildItems: Boolean = False);
(* Remove specified Item from selection, including Child items when
   IncludeChildItems = True. *)
var
  ChildItem: POGLItem;
begin
  if (Item = nil) or (Item = FRootItem) or (not IsItemSelected(Item)) then
    Exit;
    
  if FSelection.Remove(Item) <> -1 then
  begin
    Exclude(Item.States, nsSelected);

     if Assigned(FSelectedAnchorItem) and (Item = FSelectedAnchorItem) then
      FSelectedAnchorItem := nil;

    // dec Group item ChildSelectedCount &
    // deselect it when not all Child items selected
    if not IsGroupItem(Item) then
    begin
      if Item.Parent.ChildSelectedCount > 0 then
        Dec(Item.Parent.ChildSelectedCount);
      if (Item.Parent.ChildSelectedCount <> Item.Parent.ChildCount) and
         IsItemSelected(Item.Parent) then
        RemoveFromSelection(Item.Parent, False);
    end
    else
    // deselect all Child items of Group Item
    if IncludeChildItems and (soMultiSelect in FSelectionOptions) then
    begin
      ChildItem := Item.FirstChild;
      while ChildItem <> nil do
      begin
        RemoveFromSelection(ChildItem, False);
        ChildItem := ChildItem.NextSibling;
      end;
    end;
  end;
end;

procedure TOutlookGroupedList.InvertSelection(Item: POGLItem;
  IncludeChildItems: Boolean = False);
(* Invert specified Item's selection state, including Child items when
   IncludeChildItems = True. *)
begin
  if IsItemSelected(Item) then
    RemoveFromSelection(Item, IncludeChildItems)
  else
    AddToSelection(Item, IncludeChildItems);
end;

procedure TOutlookGroupedList.AddAllToSelection;
(* Select all exists items. *)
var
  GroupItem: POGLItem;
begin
  BeginUpdate;
  try
    GroupItem := FirstGroupItem;
    while GroupItem <> nil do
    begin
      // select each Group item with it's Child items
      AddToSelection(GroupItem, True);
      GroupItem := GroupItem.NextSibling;
    end;
    ScrollIntoView(FFocusedItem);
  finally
    EndUpdate;
  end;
end;

procedure TOutlookGroupedList.RemoveSelectionBeforeItem(Item: POGLItem);
(* Removes any selected items, located before specified Item. *)
var
  i: Integer;
begin
  if (Item = nil) or (Item = FRootItem) then Exit;
  i := FSelection.Count-1;
  while i > -1 do
  begin
    if (FSelection[i] <> Item) and
       (not IsItem2AfterItem1(Item, FSelection[i])) then
    begin
      RemoveFromSelection(FSelection[i], True);
      i := FSelection.Count-1;
    end else
      Dec(i);
  end;
end;

procedure TOutlookGroupedList.RemoveSelectionAfterItem(Item: POGLItem);
(* Removes any selected items, located after specified Item. *)
var
  i: Integer;
begin
  if (Item = nil) or (Item = FRootItem) then Exit;
  i := FSelection.Count-1;
  while i > -1 do
  begin
    if (FSelection[i] <> Item) and
       IsItem2AfterItem1(Item, FSelection[i]) then
    begin
      RemoveFromSelection(FSelection[i], True);
      i := FSelection.Count-1;
    end else
      Dec(i);
  end;
end;

procedure TOutlookGroupedList.AddSelectionFromAnchorItemTo(Item: POGLItem);
(* Select's all items between AnchorItem & specified Item. *)
var
  FromChildItem,ToChildItem: POGLItem;
  FromGroupItem,ToGroupItem: POGLItem;
  GroupItem,ChildItem: POGLItem;
begin
  if Item = nil then Exit;
  if FSelectedAnchorItem = nil then
  begin
    AddToSelection(Item, True);
    Exit;
  end;

  try
    FAllowOneOnSelect := True;
//    FSupressOnSelect := False;
  if IsItem2AfterItem1(FSelectedAnchorItem, Item) then
  begin
    FromChildItem := FSelectedAnchorItem;
    ToChildItem := Item;
    // clear all items Before From-Item & After To-Item
    RemoveSelectionBeforeItem(FromChildItem);
    RemoveSelectionAfterItem(ToChildItem);
    // select all items from From-Item to To-Item, i.e. from Top to Bottom
    if IsGroupItem(FromChildItem) then
      FromGroupItem := FromChildItem
    else
      FromGroupItem := FromChildItem.Parent;
    if IsGroupItem(ToChildItem) then
      ToGroupItem := ToChildItem
    else
      ToGroupItem := ToChildItem.Parent;
    GroupItem := FromGroupItem;
    while GroupItem <> nil do
    begin
      if ((GroupItem = FromChildItem) and IsGroupItem(FromChildItem) and
          (ToChildItem.Parent <> FromChildItem)) or
         ((GroupItem = ToChildItem) and IsGroupItem(ToChildItem)) or
         ((GroupItem <> FromGroupItem) and (GroupItem <> ToGroupItem)) then
        AddToSelection(GroupItem, True);
      if ((GroupItem = FromGroupItem) and (IsGroupItem(FromChildItem))) or
         ((GroupItem = FromGroupItem) and (not IsGroupItem(FromChildItem))) or
         ((GroupItem = ToGroupItem) and (not IsGroupItem(ToChildItem))) then
      begin
        if (GroupItem = FromGroupItem) and (not IsGroupItem(FromChildItem)) then
          ChildItem := FromChildItem
        else
          ChildItem := GroupItem.FirstChild;
        while ChildItem <> nil do
        begin
          AddToSelection(ChildItem);
          if ChildItem = ToChildItem then Exit;
          ChildItem := ChildItem.NextSibling;
        end;
      end;
      if GroupItem = ToGroupItem then Exit;
      GroupItem := GroupItem.NextSibling;
    end;
  end else
  begin
    ToChildItem := Item;
    FromChildItem := FSelectedAnchorItem;
    // clear all items Before To-Item & After From-Item
    RemoveSelectionBeforeItem(ToChildItem);
    RemoveSelectionAfterItem(FromChildItem);
    // select all items from To-Item to From-Item, i.e. from Bottom to Top
    if IsGroupItem(FromChildItem) then
      FromGroupItem := FromChildItem
    else
      FromGroupItem := FromChildItem.Parent;
    if IsGroupItem(ToChildItem) then
      ToGroupItem := ToChildItem
    else
      ToGroupItem := ToChildItem.Parent;
    GroupItem := FromGroupItem;
    while GroupItem <> nil do
    begin
      if ((GroupItem = ToChildItem) and IsGroupItem(ToChildItem) and
          (FromChildItem.Parent <> ToChildItem)) or
         ((GroupItem = FromChildItem) and IsGroupItem(FromChildItem)) or
         ((GroupItem <> FromGroupItem) and (GroupItem <> ToGroupItem)) then
        AddToSelection(GroupItem, True);
      if ((GroupItem = ToGroupItem) and (IsGroupItem(ToChildItem))) or
         ((GroupItem = ToGroupItem) and (not IsGroupItem(ToChildItem))) or
         ((GroupItem = FromGroupItem) and (not IsGroupItem(FromChildItem))) then
      begin
        if (GroupItem = FromGroupItem) and (not IsGroupItem(FromChildItem)) then
          ChildItem := FromChildItem
        else
          ChildItem := GroupItem.LastChild;
        while ChildItem <> nil do
        begin
          AddToSelection(ChildItem);
          if ChildItem = ToChildItem then Exit;
          ChildItem := ChildItem.PrevSibling;
        end;
      end;
      if GroupItem = ToGroupItem then Exit;
      GroupItem := GroupItem.PrevSibling;
      end;
    end;
  finally;
    FAllowOneOnSelect := False;
    FSupressOnSelect := False;
  end;
end;

function TOutlookGroupedList.GetSelectionDisplayRect: TRect;
begin
  Result := PointsToRect(FFromSelPoint, FToSelPoint);
  OffsetRect(Result, 0, -VertScrollBar.Position);
  if Result.Left < 0 then
    Result.Left := 0;
  if Result.Right > ClientWidth then
    Result.Right := ClientWidth;
  if VertScrollBar.Position = 0 then
  begin
    // at the begin of VertScrollBar
    if Result.Top < 0 then
      Result.Top := 0;
    if Result.Bottom > ClientHeight then
      Result.Bottom := ClientHeight + 1;
  end else
  if VertScrollBar.Position = VertScrollBar.Range - ClientHeight then
  begin
    // at the end of VertScrollBar
    if Result.Top < 0 then
      Result.Top := -1;
    if Result.Bottom > ClientHeight then
      Result.Bottom := ClientHeight;
  end else
  begin
    // within of VertScrollBar
    if Result.Top < 0 then
      Result.Top := -1;
    if Result.Bottom > ClientHeight then
      Result.Bottom := ClientHeight + 1;
  end;
end;

function TOutlookGroupedList.ClearSelection: Boolean;
var
  i: Integer;
begin
  try
    i := FSelection.Count-1;
    Result := i > -1;
    while i > -1 do
    begin
      RemoveFromSelection(FSelection[i], True);
      i := FSelection.Count - 1;
    end;
  finally
    FSelection.Clear;
  end;
end;

procedure TOutlookGroupedList.ClearSelectionRect;
var
  rc: TRect;
begin
  // clear Selection points
  if lsMouseSelecting in FStates then
  begin
    rc := GetSelectionDisplayRect;
    FFromSelPoint := Point(0, 0);
    FToSelPoint := Point(0, 0);
    DoStateChange([], [lsMouseSelecting, lsMouseSelected,
      lsToggleFocusedSelection]);
    if (not IsRectEmptyEx(rc)) and (not (csLoading	in ComponentState)) then
      InvalidateRect(Handle, @rc, False);
  end;
end;

function TOutlookGroupedList.MoveFocus(MoveFocusDirection: TOGLMoveFocusDirection;
  SelectItems, ClearSelectionBeforeMove, SelectFocusedItem,
  MoveWithinGroup: Boolean): Boolean;
var
  X,Y: Integer;
  OldVertScrollBarPos: Integer;
  DestinationItem,
  OldFocusedItem: POGLItem;

  function GetPrevExpandedGroup(GroupItem: POGLItem): POGLItem;
  begin
    Result := nil;
    GroupItem := GroupItem.PrevSibling;
    if GroupItem = nil then Exit;
    while (GroupItem <> nil) and (not IsGroupExpanded(GroupItem)) do
      GroupItem := GroupItem.PrevSibling;
    Result := GroupItem;
  end;

  function GetNextExpandedGroup(GroupItem: POGLItem): POGLItem;
  begin
    Result := nil;
    GroupItem := GroupItem.NextSibling;
    if GroupItem = nil then Exit;
    while (GroupItem <> nil) and (not IsGroupExpanded(GroupItem)) do
      GroupItem := GroupItem.NextSibling;
    Result := GroupItem;
  end;

  function GetPrevChildItemOfIndex(ChildItem: POGLItem;
    Index: Integer): POGLItem;
  begin
    Result := nil;
    while (ChildItem <> nil) and (Integer(ChildItem.Index) > Index) do
      ChildItem := ChildItem.PrevSibling;
    if (ChildItem <> nil) and (Integer(ChildItem.Index) = Index) then
      Result := ChildItem;
  end;

  function GetNextChildItemOfIndex(ChildItem: POGLItem;
    Index: Integer): POGLItem;
  begin
    Result := nil;
    while (ChildItem <> nil) and (Integer(ChildItem.Index) < Index) do
      ChildItem := ChildItem.NextSibling;
    if (ChildItem <> nil) and (Integer(ChildItem.Index) = Index) then
      Result := ChildItem;
  end;

  function GetPrevRowItem: POGLItem;
  var
    Index: Integer;
    GroupItem: POGLItem;
  begin
    if IsGroupItem(FFocusedItem) then
    begin
      // Group item
      if (not SelectItems) and IsGroupExpanded(FFocusedItem.PrevSibling) then
      begin
        GroupItem := FFocusedItem.PrevSibling;
        Result := GroupItem.LastChild;
        if GroupItem <> nil then
        begin
          Index := GroupItem.ChildCount -
              (GroupItem.ChildCount mod FItemsPerRow) +
              Cardinal(FOGLItemColumn);
          if Index > Integer(GroupItem.ChildCount)-1 then
            Index := Index - FItemsPerRow;
          if (Index < 0) or (Index > Integer(GroupItem.ChildCount)-1) then
            Result := GroupItem.LastChild
          else
            Result := GetPrevChildItemOfIndex(GroupItem.LastChild, Index);
        end;
      end else
        Result := FFocusedItem.PrevSibling;
    end else
    if FFocusedItem <> nil then
    begin
      // Child item
      Index := Integer(FFocusedItem.Index) - FItemsPerRow;
      if Index >= 0 then
        Result := GetPrevChildItemOfIndex(FFocusedItem, Index) else
      begin
        if not SelectItems then
          Result := FFocusedItem.Parent else
        begin
          Result := FFocusedItem.Parent.FirstChild;
          GroupItem := GetPrevExpandedGroup(FFocusedItem.Parent);
          if GroupItem <> nil then
          begin
            Index := GroupItem.ChildCount -
                (GroupItem.ChildCount mod FItemsPerRow) +
                Cardinal(FOGLItemColumn);
            if Index > Integer(GroupItem.ChildCount)-1 then
              Index := Index - FItemsPerRow;
            if (Index < 0) or (Index > Integer(GroupItem.ChildCount)-1) then
              Result := GroupItem.LastChild
            else
              Result := GetPrevChildItemOfIndex(GroupItem.LastChild, Index);
          end;
        end;
      end;
    end else
      Result := FirstGroupItem;
  end;

  function GetNextRowItem: POGLItem;
  var
    Index: Integer;
    GroupItem: POGLItem;
  begin
    if IsGroupItem(FFocusedItem) then
    begin
      // Group item
      if (not SelectItems) and IsGroupExpanded(FFocusedItem) then
      begin
        Result := FFocusedItem.FirstChild;
        GroupItem := FFocusedItem;
        if Integer(GroupItem.ChildCount)-1 < FOGLItemColumn then
          Result := GroupItem.LastChild
        else
          Result := GetNextChildItemOfIndex(Result, FOGLItemColumn);
      end else
        Result := FFocusedItem.NextSibling;
    end else
    if FFocusedItem <> nil then
    begin
      // Child item
      Index := Integer(FFocusedItem.Index) + FItemsPerRow;
      if Index < Integer(FFocusedItem.Parent.ChildCount) then
        Result := GetNextChildItemOfIndex(FFocusedItem, Index) else
      begin
        if FFocusedItem.Parent.NextSibling <> nil then
          Result := FFocusedItem.Parent.NextSibling
        else
          Result :=  FFocusedItem.Parent.LastChild;
        if SelectItems then
        begin
          GroupItem := GetNextExpandedGroup(FFocusedItem.Parent);
          if GroupItem <> nil then
          begin
            if Integer(GroupItem.ChildCount)-1 < FOGLItemColumn then
              Result := GroupItem.LastChild
            else
              Result := GetNextChildItemOfIndex(GroupItem.FirstChild,
                FOGLItemColumn);
          end;
        end;
      end;
    end else
      Result := FirstGroupItem;
  end;

  function GetTopMostItem: POGLItem;
  begin
    Result := nil;
    X := FOGLItemColumn * GetChildItemWidth + GetChildItemWidth div 2;
    Y := 0;
    while Y < ClientHeight do
    begin
      Result := GetItemAt(X, Y, True);
      if Result <> nil then Exit;
      Inc(Y);
    end;
  end;

  function GetBottomMostItem: POGLItem;
  begin
    Result := nil;
    X := FOGLItemColumn * GetChildItemWidth + GetChildItemWidth div 2;
    Y := ClientHeight;
    while Y > 0 do
    begin
      Result := GetItemAt(X, Y, True);
      if Result <> nil then Exit;
      Dec(Y);
    end;
  end;

  function GetPrevVisibleItem(Item: POGLItem): POGLItem;
  var
    GroupItem: POGLItem;
  begin
    Result := nil;
    if IsGroupItem(Item) then
      // Group item
      Result := Item.PrevSibling else
    if Item <> nil then
    begin
      // Child item
      if Item.PrevSibling <> nil then
        Result := Item.PrevSibling else
      begin
        // search prev Child item within Expanded Group items
        GroupItem := Item.Parent.PrevSibling;
        while GroupItem <> nil do
        begin
          if IsGroupExpanded(GroupItem) then
          begin
            Result := GroupItem.LastChild;
            Exit;
          end;
          GroupItem := GroupItem.PrevSibling;
        end;
      end;
    end;
  end;

  function GetNextVisibleItem(Item: POGLItem): POGLItem;
  var
    GroupItem: POGLItem;
  begin
    Result := nil;
    if IsGroupItem(Item) then
      // Group item
      Result := Item.NextSibling else
    if Item <> nil then
    begin
      // Child item
      if Item.NextSibling <> nil then
        Result := Item.NextSibling else
      begin
        // search prev Child item within Expanded Group items
        GroupItem := Item.Parent.NextSibling;
        while GroupItem <> nil do
        begin
          if IsGroupExpanded(GroupItem) then
          begin
            Result := GroupItem.FirstChild;
            Exit;
          end;
          GroupItem := GroupItem.NextSibling;
        end;
      end;
    end;
  end;

begin
  Result := False;
  
  if FFocusedItem = nil then
    Exit;
  OldFocusedItem := FFocusedItem;
  DestinationItem := FFocusedItem;
  // check whether the Focus is stands at begin or end item
  case MoveFocusDirection of
    mfdLeft:
      if ClearSelectionBeforeMove and CollapseItem(DestinationItem) then Exit;
    mfdRight:
    begin
      if ClearSelectionBeforeMove and
        ExpandItem(DestinationItem) then
      begin
        ScrollIntoView(DestinationItem, True);
        Exit;
      end;
    end;
  end;
  case MoveFocusDirection of
    mfdHome, mfdPageUp, mfdUp, mfdLeft:
      if DestinationItem = FRootItem.FirstChild then Exit;
    mfdRight, mfdDown, mfdPageDown, mfdEnd:
      if (DestinationItem = FRootItem.LastChild.LastChild) or
        ((DestinationItem = FRootItem.LastChild) and
         (not IsGroupExpanded(DestinationItem))) then Exit;
  end;


  if ClearSelectionBeforeMove then
    ClearSelection;
  case MoveFocusDirection of
   mfdHome:
     begin
       // move to the home
       if MoveWithinGroup then
         DestinationItem := DestinationItem.Parent.FirstChild
       else
         DestinationItem := FirstGroupItem;
     end;
   mfdPageUp:
     begin
       DestinationItem := GetTopMostItem;
       if DestinationItem = FFocusedItem then
       begin
         OldVertScrollBarPos := VertScrollBar.Position;
         VertScrollBar.Position := OldVertScrollBarPos - ClientHeight;
         if VertScrollBar.Position <> OldVertScrollBarPos then
           DestinationItem := GetTopMostItem;
       end;
     end;
   mfdUp:
     DestinationItem := GetPrevRowItem;
   mfdLeft:
     begin
       if SelectItems then
         DestinationItem := GetPrevVisibleItem(DestinationItem) else
       if IsGroupItem(DestinationItem) then
       begin
         // Group item
         if DestinationItem.PrevSibling <> nil then
         begin
           DestinationItem := DestinationItem.PrevSibling;
           if IsGroupExpanded(DestinationItem) then
             DestinationItem := DestinationItem.LastChild;
         end;
       end else
       if DestinationItem <> nil then
       begin
         // Child item
         if DestinationItem.PrevSibling <> nil then
           DestinationItem := DestinationItem.PrevSibling
         else
           DestinationItem := DestinationItem.Parent
       end;
     end;
   mfdRight:
     begin
       if SelectItems then
         DestinationItem := GetNextVisibleItem(DestinationItem) else
       if IsGroupItem(DestinationItem) then
       begin
         // Group item
         if IsGroupExpanded(DestinationItem) then
           DestinationItem := DestinationItem.FirstChild else
         if DestinationItem.NextSibling <> nil then
           DestinationItem := DestinationItem.NextSibling;
       end else
       if DestinationItem <> nil then
       begin
         // Child item
         if DestinationItem.NextSibling <> nil then
           DestinationItem := DestinationItem.NextSibling else
         if DestinationItem.Parent.NextSibling <> nil then
           DestinationItem := DestinationItem.Parent.NextSibling;
       end else
         DestinationItem := FirstGroupItem;
     end;
   mfdDown:
     DestinationItem := GetNextRowItem;
   mfdPageDown:
     begin
       DestinationItem := GetBottomMostItem;
       if DestinationItem = FFocusedItem then
       begin
         OldVertScrollBarPos := VertScrollBar.Position;
         VertScrollBar.Position := OldVertScrollBarPos + ClientHeight;
         if VertScrollBar.Position <> OldVertScrollBarPos then
           DestinationItem := GetBottomMostItem else
         if (not IsGroupItem(DestinationItem)) and
            (DestinationItem <> DestinationItem.Parent.LastChild) then
           DestinationItem := DestinationItem.Parent.LastChild;
       end;
     end;
   mfdEnd:
     begin
       // move to the end
       if MoveWithinGroup then
         DestinationItem := DestinationItem.Parent.LastChild else
       begin
         DestinationItem := FRootItem.LastChild;
         if IsGroupExpanded(DestinationItem) then
           DestinationItem := DestinationItem.LastChild;
       end;
     end;
  end;

  // check wether we locate DestinationItem
  if (DestinationItem <> nil) and (DestinationItem <> OldFocusedItem) then
  begin
    FSupressOnSelect := true;
    if SelectItems then
      AddSelectionFromAnchorItemTo(DestinationItem);
    DoSetFocusedItem(DestinationItem, (ClearSelectionBeforeMove or
     (MoveWithinGroup and (not SelectItems))));

    if SelectFocusedItem and (not IsGroupItem(FFocusedItem)) then
    begin
     AddToSelection(FFocusedItem);
    end;
    FSupressOnSelect := false;

    if Assigned(FOnSelectionChange) then
      FOnSelectionChange(self);

    Result := True;
  end;
end;

procedure TOutlookGroupedList.SetFocusedItem(Item: POGLItem);
begin
  DoSetFocusedItem(Item, True);
end;

function TOutlookGroupedList.DoSetFocusedItem(Item: POGLItem;
  SetAnchorItem: Boolean): Boolean;
begin
  Result := (Item <> FRootItem) and (FFocusedItem <> Item);
  if not Result then Exit;
  // set new FocusedItem & SelectedAnchorItem (if need)
  FFocusedItem := Item;
  if SetAnchorItem then
    FSelectedAnchorItem := FFocusedItem;
  // update ChildItemColumn (only for Child item)
  if (FFocusedItem <> nil) and (not IsGroupItem(FFocusedItem)) then
    FOGLItemColumn := integer(FFocusedItem.Index) mod FItemsPerRow;
end;

function TOutlookGroupedList.GetDragEngine: IOGLDragEngine;
begin
  if FDragEngine = nil then
    FDragEngine := TOGLDragEngine.Create(Self);
  Result := FDragEngine;
end;

procedure TOutlookGroupedList.GetOLEClipboardFormats(
  var Formats: TFormatEtcArray);
begin
  DoGetOLEClipboardFormats(Formats);
end;

function TOutlookGroupedList.GetDropEffect(
  const Shift: TShiftState; const AllowedEffect: Integer): Integer;
begin
  Result := AllowedEffect;
  if ssCtrl in Shift then
  begin
    // copy or link
    if ssShift in Shift then
    begin
      // link
      if (AllowedEffect and DROPEFFECT_LINK) <> 0 then
        Result := DROPEFFECT_LINK;
    end else
    begin
      // copy
      if (AllowedEffect and DROPEFFECT_COPY) <> 0 then
        Result := DROPEFFECT_COPY;
    end;
  end else
  begin
    // move, link or default
    if ssShift in Shift then
    begin
      // move
      if (AllowedEffect and DROPEFFECT_MOVE) <> 0 then
      begin
        //if (FDragDropMode = ddmNormal) then
          Result := DROPEFFECT_MOVE
        //else
          //Result := DROPEFFECT_COPY;
      end;
    end else
    begin
      // link or default
      if ssAlt in Shift then
      begin
        // link
        if (AllowedEffect and DROPEFFECT_LINK) <> 0 then
          Result := DROPEFFECT_LINK;
      end
      else
      begin
        if (AllowedEffect and DROPEFFECT_MOVE) <> 0 then
          Result := DROPEFFECT_MOVE;
      end;      
    end;
  end;
end;

procedure TOutlookGroupedList.SetDragOptions(const Value: TOGLDragOptions);
begin
  if FDragOptions = Value then Exit;
  FDragOptions := Value;
  if doOLEAcceptDrop in FDragOptions then
    RegisterDragDrop(Handle, DragEngine as IDropTarget)
  else
    RevokeDragDrop(Handle);
end;

procedure TOutlookGroupedList.SetDropTargetGroup(const GroupItem: POGLItem);
var
  tmpOldGroupItem: POGLItem;
begin
  if (FDropTargetGroup = nil) or (FDropTargetGroup <> GroupItem) then
  begin
    // reset & invalidate old DropTargetGroup
    // set & invalidate new DropTargetGroup
    tmpOldGroupItem := FDropTargetGroup;
    FDropTargetGroup := GroupItem;
    InvalidateItem(tmpOldGroupItem, True);
    InvalidateItem(FDropTargetGroup, True);
    // auto-expand collapsed Group item
    if (toAutoExpandOnDrop in FAutoOptions) and
      IsGroupItem(FDropTargetGroup) and not
      IsGroupExpanded(FDropTargetGroup) then
    begin
      ExpandItem(FDropTargetGroup);
      UpdateWindow(Handle);
    end;
  end;
end;

procedure TOutlookGroupedList.SetSortColumn(Value: TOGLSortColumn);
begin
  if csLoading in ComponentState then
    FSortColumn := Value else
  begin
    if Value < NoColumn then
      Value := NoColumn;
    if FSortColumn <> Value then
    begin
      FSortColumn := Value;
      if (toAutoSort in FAutoOptions) and (FUpdateCount = 0) then
        Sort;
    end;
  end;
end;

procedure TOutlookGroupedList.SetSortDirection(const Value: TOGLSortDirection);
begin
  if Value <> FSortDirection then
  begin
    FSortDirection := Value;
    if (toAutoSort in FAutoOptions) and (FUpdateCount = 0) then
      Sort;
  end;
end;

procedure TOutlookGroupedList.SetViewStyle(const Value: TOGLViewStyle);
begin
  if FViewStyle = Value then Exit;
  FViewStyle := Value;
  AdjustItemsSize;
  Invalidate;
end;

procedure TOutlookGroupedList.DoHotCharSearch(CharCode: Word);

  function __ItemCharSearch(const Item: POGLItem): Boolean;
  var
    tmpCaption: String;
  begin
    FOnGetCaption(Self, Item, tmpCaption);
    Result := (Length(tmpCaption) > 0) and
      (LowerCase(tmpCaption[1]) = Chr(CharCode));
    if Result then
    begin
      ClearSelection;
      FFocusedItem := Item;
      AddToSelection(Item);
      ScrollIntoView(Item, False);
      Invalidate;
    end;
  end;

var
  GroupItem: POGLItem;
  ChildItem: POGLItem;
  CheckGroup: Boolean;
begin
  if (FSearchType = stNone) or (CharCode = 0) or
     (not Assigned(FOnGetCaption)) then Exit;
  // search from next item after focused one
  GroupItem := FFocusedItem;
  if GroupItem = nil then
    GroupItem := GetFirstGroupItem;
  if not IsGroupItem(GroupItem) then
  begin
    ChildItem := GroupItem;
    GroupItem := GroupItem.Parent;
    CheckGroup := False;
  end else
  begin
    ChildItem := nil;
    CheckGroup := True;
  end;
  while GroupItem <> nil do
  begin
    if (FSearchType = stAll) and (GroupItem <> FFocusedItem) and CheckGroup then
      if __ItemCharSearch(GroupItem) then Exit;
    if ChildItem = nil then
      ChildItem := GroupItem.FirstChild;
    while ChildItem <> nil do
    begin
      if ChildItem <> FFocusedItem then
        if __ItemCharSearch(ChildItem) then Exit;
      ChildItem := ChildItem.NextSibling;
    end;
    CheckGroup := True;
    GroupItem := GroupItem.NextSibling;
  end;
  // search from first Group if exists
  GroupItem := FirstGroupItem;
  while GroupItem <> nil do
  begin
    if GroupItem = FFocusedItem then Exit;
    if FSearchType = stAll then
      if __ItemCharSearch(GroupItem) then Exit;
    ChildItem := GroupItem.FirstChild;
    while ChildItem <> nil do
    begin
      if (GroupItem = FFocusedItem) or
         __ItemCharSearch(ChildItem) then Exit;
      ChildItem := ChildItem.NextSibling;
    end;
    GroupItem := GroupItem.NextSibling;
  end;
end;

function TOutlookGroupedList.DoCompare(Item1, Item2: POGLItem;
  Column: TOGLSortColumn): Integer;
begin
  Result := 0;
  if Assigned(FOnCompareItems) then
    FOnCompareItems(Self, Item1, Item2, Column, Result);
end;

procedure TOutlookGroupedList.Sort(ParentItem: POGLItem;
  Column: TOGLSortColumn; Direction: TOGLSortDirection);

(* Sorts the child items of specified ParentItem. *)

  function __MergeAscending(A, B: POGLItem): POGLItem;
  (* Merges A and B (which both must be sorted via Compare) into one list. *)
  var
    Dummy: TOGLItem;
  begin
    // This avoids checking for Result = nil in the loops.
    Result := @Dummy;
    while Assigned(A) and Assigned(B) do
    begin
      if DoCompare(A, B, Column) <= 0 then
      begin
        Result.NextSibling := A;
        Result := A;
        A := A.NextSibling;
      end else
      begin
        Result.NextSibling := B;
        Result := B;
        B := B.NextSibling;
      end;
    end;
    (* Just append the list which is not nil (or set end
       of result list to nil if both lists are nil). *)
    if Assigned(A) then
      Result.NextSibling := A
    else
      Result.NextSibling := B;
    // return start of the new merged list
    Result := Dummy.NextSibling;
  end;


  function __MergeDescending(A, B: POGLItem): POGLItem;
  (* Merges A and B (which both must be sorted via Compare) into one list. *)
  var
    Dummy: TOGLItem;
  begin
    // this avoids checking for Result = nil in the loops
    Result := @Dummy;
    while Assigned(A) and Assigned(B) do
    begin
      if DoCompare(A, B, Column) >= 0 then
      begin
        Result.NextSibling := A;
        Result := A;
        A := A.NextSibling;
      end else
      begin
        Result.NextSibling := B;
        Result := B;
        B := B.NextSibling;
      end;
    end;
    (* Just append the list which is not nil (or set
       end of result list to nil if both lists are nil). *)
    if Assigned(A) then
      Result.NextSibling := A
    else
      Result.NextSibling := B;
    // Return start of the newly merged list.
    Result := Dummy.NextSibling;
  end;

  function __MergeSortAscending(var Item: POGLItem; N: Cardinal): POGLItem;
  (* Sorts the list of items given by Item (which must not be nil). *)
  var
    A, B: POGLItem;
  begin
    if N > 1 then
    begin
      A := __MergeSortAscending(Item, N div 2);
      B := __MergeSortAscending(Item, (N + 1) div 2);
      Result := __MergeAscending(A, B);
    end else
    begin
      Result := Item;
      Item := Item.NextSibling;
      Result.NextSibling := nil;
    end;
  end;

  function __MergeSortDescending(var Item: POGLItem; N: Cardinal): POGLItem;
  (* Sorts the list of items given by Item (which must not be nil). *)
  var
    A, B: POGLItem;
  begin
    if N > 1 then
    begin
      A := __MergeSortDescending(Item, N div 2);
      B := __MergeSortDescending(Item, (N + 1) div 2);
      Result := __MergeDescending(A, B);
    end else
    begin
      Result := Item;
      Item := Item.NextSibling;
      Result.NextSibling := nil;
    end;
  end;

var
  SortItem: POGLItem;
  Index: Cardinal;  
begin
  if ParentItem = nil then
    ParentItem := FRootItem;
  if ParentItem.ChildCount > 1 then
  begin
    // sort the linked list, check direction flag only once
    if Direction = sdAscending then
      ParentItem.FirstChild :=
        __MergeSortAscending(ParentItem.FirstChild, ParentItem.ChildCount)
    else
      ParentItem.FirstChild :=
        __MergeSortDescending(ParentItem.FirstChild, ParentItem.ChildCount);
    // consolidate the child items of ParentItem finally
    SortItem := ParentItem.FirstChild;
    SortItem.PrevSibling := nil;
    Index := 0;
    repeat
      SortItem.Index := Index;
      Inc(Index);
      if SortItem.NextSibling = nil then
        Break;
      SortItem.NextSibling.PrevSibling := SortItem;
      SortItem := SortItem.NextSibling;
    until False;
    ParentItem.LastChild := SortItem;
    if FUpdateCount = 0 then
      Invalidate;
  end;
end;

procedure TOutlookGroupedList.Sort(Column: TOGLSortColumn = NoColumn;
  Direction: TOGLSortDirection = sdDefault);
var
  GroupItem: POGLItem;
begin
  (* Instead of wrapping the sort using BeginUpdate/EndUpdate simply the update
     counter is modified. Otherwise the EndUpdate call will recurse here. *)
  if Column = NoColumn then
    Column := FSortColumn;
  if Column > InvalidColumn then
  begin
    if Direction = sdDefault then
      Direction := FSortDirection;
    Inc(FUpdateCount);
    try
      // sort the Group items
      Sort(FRootItem, Column, Direction);
      // sort the Child items of each Group item
      GroupItem := FirstGroupItem;
      while GroupItem <> nil do
      begin
        Sort(GroupItem, Column, Direction);
        GroupItem := GroupItem.NextSibling;
      end;
    finally
      ScrollIntoView(FFocusedItem, True);
      if FUpdateCount > 0 then
        Dec(FUpdateCount);
      if FUpdateCount = 0 then
        Invalidate;
    end;
  end;
end;

procedure TOutlookGroupedList.DoStateChange(Enter: TOGLStates;
  Leave: TOGLStates = []);
begin
  FStates := FStates + Enter - Leave;
end;

procedure TOutlookGroupedList.DoTrackMouseEvent(const Start: Boolean);
var
  tmpTrackMouseEvent: Windows.TTrackMouseEvent;
begin
  tmpTrackMouseEvent.cbSize := SizeOf(tmpTrackMouseEvent);
  tmpTrackMouseEvent.hwndTrack := Self.Handle;
  if Start then
  begin
    tmpTrackMouseEvent.dwFlags := TME_HOVER;
    tmpTrackMouseEvent.dwHoverTime := 200;
  end else
  begin
    tmpTrackMouseEvent.dwFlags := TME_CANCEL;
    tmpTrackMouseEvent.dwHoverTime := 0;
  end;
  Windows.TrackMouseEvent(tmpTrackMouseEvent);
end;

function TOutlookGroupedList.DoOLECreateDataObject: IDataObject;
begin
  Result := nil;
  if Assigned(FOnOLEGetDataObject) then
    FOnOLEGetDataObject(Self, Result);
end;

function TOutlookGroupedList.DoOLEDragOver(const DataObject: IDataObject;
  Shift: TShiftState; Pt: TPoint; State: TDragState; var Effect: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnOLEDragOver) then
    FOnOLEDragOver(Self, DataObject, Shift, Pt, State, Effect, Result);
end;

procedure TOutlookGroupedList.DoOLEDrop(const DataObject: IDataObject;
  Formats: TClipFormatArray; Shift: TShiftState;
  Pt: TPoint; var Effect: Integer);
begin
  if Assigned(FOnOLEDrop) then
    FOnOLEDrop(Self, DataObject, Shift, Pt, Formats, Effect);
end;

function TOutlookGroupedList.DoOLEGetData(const FormatEtcIn: TFormatEtc;
  out Medium: TStgMedium): HRESULT;
begin
  Result := E_FAIL;
  if Assigned(FOnOLEGetData) then
    FOnOLEGetData(Self, FormatEtcIn, Medium, Result);
end;

procedure TOutlookGroupedList.DoGetOLEClipboardFormats(
  var Formats: TFormatEtcArray);
begin
  if Assigned(FOnOLEGetClipboardFormats) then
    FOnOLEGetClipboardFormats(Self, Formats);
end;

function TOutlookGroupedList.DoOLEDragAllowed(Item: POGLItem): Boolean;
begin
  Result := False;
  if Assigned(FOnOLEDragAllowed) then
    FOnOLEDragAllowed(Self, Item, Result);
end;

procedure TOutlookGroupedList.DoOLEDragging(P: TPoint);

  function __GetDragOperations: Integer;
  begin
    if FDragOperations = [] then
      Result := DROPEFFECT_NONE else
    begin
      Result := 0;
      if doCopy in FDragOperations then
        Result := Result or DROPEFFECT_COPY;
      if doLink in FDragOperations then
        Result := Result or DROPEFFECT_LINK;
      if doMove in FDragOperations then
      begin
        if FDragDropMode = ddmNormal then
          Result := Result or DROPEFFECT_MOVE
        else
          Result := Result or DROPEFFECT_COPY;
      end;
    end;
  end;

var
  DragEffect,
  AllowedEffect: Integer;
  DataObject: IDataObject;
begin
  DataObject := nil;
  // Select the focused item if not already done.
  if not IsItemSelected(FFocusedItem) and (FFocusedItem <> nil) then
  begin
    AddToSelection(FFocusedItem, True);
    InvalidateItem(FFocusedItem);
  end;
  if ScrollIntoView(FFocusedItem) then
    InvalidateRect(Handle, nil, False)
  else
    UpdateWindow(Handle);
  DoStateChange([lsOLEDragging], [lsOLEDragPending, lsSelectionClearing]);
  DataObject := DragEngine.DataObject;
  RenderDragImage(P, DataObject);
  DragEffect := DROPEFFECT_NONE;
  AllowedEffect := __GetDragOperations;
  try
    ActiveX.DoDragDrop(DataObject, DragEngine as IDropSource,
      AllowedEffect, DragEffect);
    DragEngine.ForceDragLeave;
  finally
    GetCursorPos(P);
    P := ScreenToClient(P);
    DoEndDrag(Self, P.X, P.Y);
    // if the operation was a move - delete the previously selected items
    if ((DragEffect and DROPEFFECT_MOVE) = DROPEFFECT_MOVE) and
       (toAutoDeleteMovedItems in AutoOptions) then
    begin
      DeleteSelectedItems(True);
    end;
    DoStateChange([], [lsOLEDragging]);
  end;
end;

function TOutlookGroupedList.OLEDragEnter(const DataObject: IDataObject;
  KeyState: Integer; Pt: TPoint; var Effect: Integer): HResult;
// callback routine for IDropTarget.DragEnter
var
  Shift: TShiftState;
  tmpHitInfo: TOGLItemHitInfo;
begin
  try
    // determine acceptance of drag operation
    Pt := ScreenToClient(Pt);
    Shift := KeysToShiftState(KeyState);
    Effect := GetDropEffect(Shift, Effect);
    FOLEDragEnterAccept := DoOLEDragOver(DataObject, Shift,
      Pt, dsDragEnter, Effect);
    if Effect = DROPEFFECT_NONE then
      FOLEDragEnterAccept := False else
    if not FOLEDragEnterAccept then
      Effect := DROPEFFECT_NONE;
    if FOLEDragEnterAccept then
    begin
      // get & invalidate new DropTargetGroup
      GetHitTestInfoAt(Pt.X, Pt.Y, True, tmpHitInfo);
      SetDropTargetGroup(tmpHitInfo.GroupItem);
      if IsItemSelected(tmpHitInfo.GroupItem) then
        Effect := DROPEFFECT_NONE;
    end;
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TOutlookGroupedList.OLEDragOver(KeyState: Integer; Pt: TPoint;
  State: TDragState; var Effect: Integer): HResult;
// callback routine for IDropTarget.DragOver
var
  Shift: TShiftState;
  tmpHitInfo: TOGLItemHitInfo;
  tmpVertScrollOffset: Integer;
begin
  try
    Pt := ScreenToClient(Pt);
    Shift := KeysToShiftState(KeyState);
    Effect := GetDropEffect(Shift, Effect);
    if FOLEDragEnterAccept then
      DoOLEDragOver(nil, Shift, Pt, State, Effect);
    if not FOLEDragEnterAccept then
      Effect := DROPEFFECT_NONE else
    begin
      // get new DropTargetGroup
      GetHitTestInfoAt(Pt.X, Pt.Y, True, tmpHitInfo);
      SetDropTargetGroup(tmpHitInfo.GroupItem);
      if IsItemSelected(tmpHitInfo.GroupItem) then
        Effect := DROPEFFECT_NONE;
      
      // try to autoscroll when mouse selecting
      if VertScrollBar.Range > 0 then
      begin
        if Pt.Y > ClientHeight - OGLTrackMouseVertScrollOffset then
          tmpVertScrollOffset := OGLTrackMouseVertScrollOffset else
        if (Pt.Y < OGLTrackMouseVertScrollOffset) and (VertScrollBar.Position > 0) then
          tmpVertScrollOffset := -OGLTrackMouseVertScrollOffset
        else
          tmpVertScrollOffset := 0;
        if tmpVertScrollOffset = 0 then
          DoTrackMouseEvent(False) else
        begin
          Effect := Effect or Integer(DROPEFFECT_SCROLL);
          VertScrollBar.Position := VertScrollBar.Position + tmpVertScrollOffset;
          Invalidate;
          DoTrackMouseEvent(True);
        end;
      end;
    end;
    Result :=  S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

procedure TOutlookGroupedList.OLEDragLeave;
// callback routine for IDropTarget.DragLeave
var
  Effect: Integer;
begin
  SetDropTargetGroup(nil);
  UpdateWindow(Handle);
  Effect := DROPEFFECT_NONE;
  DoOLEDragOver(nil, [], Point(0, 0), dsDragLeave, Effect);
  DoTrackMouseEvent(False);
end;

function TOutlookGroupedList.OLEDrop(const DataObject: IDataObject;
  KeyState: Integer; Pt: TPoint; var Effect: Integer): HResult;
// callback routine for IDropTarget.Drop
var
  Shift: TShiftState;
  EnumFormat: IEnumFormatEtc;
  Fetched: Integer;
  OLEFormat: TFormatEtc;
  Formats: TClipFormatArray;
begin
  Formats := nil;
  Result := OLEDragOver(KeyState, Pt, dsDragMove, Effect);
  if (Result <> S_OK) or ((Effect and not DROPEFFECT_SCROLL) = DROPEFFECT_NONE) then
    Result := E_FAIL else
  begin
    Pt := ScreenToClient(Pt);
    Shift := KeysToShiftState(KeyState);
    (* Determine which formats we can get and pass them along with
       the data object to the drop handler. *)
    Result := DataObject.EnumFormatEtc(DATADIR_GET, EnumFormat);
    if Failed(Result) then Abort;
    Result := EnumFormat.Reset;
    if Failed(Result) then Abort;
    // create a list of available formats
    while EnumFormat.Next(1, OLEFormat, @Fetched) = S_OK do
    begin
      SetLength(Formats, Length(Formats) + 1);
      Formats[High(Formats)] := OLEFormat.cfFormat;
    end;
    DoOLEDrop(DataObject, Formats, Shift, Pt, Effect);
    DoTrackMouseEvent(False);
    // reset & invalidate old DropTargetGroup
    SetDropTargetGroup(nil);
    UpdateWindow(Handle);
  end;
end;

function TOutlookGroupedList.OLEGetData(const FormatEtcIn: TFormatEtc;
  out Medium: TStgMedium): HResult;
// callback routine for IDataObject.GetData
begin
  FillChar(Medium, SizeOf(Medium), 0);
  Result := DoOLEGetData(FormatEtcIn, Medium);
end;

procedure TOutlookGroupedList.DoExpand(Item: POGLItem);
begin
  if Assigned(OnExpandItem) then
    OnExpandItem(Self, Item);
end;

procedure TOutlookGroupedList.DoCollaps(Item: POGLItem);
begin
  if Assigned(OnCollapsItem) then
    OnCollapsItem(Self, Item);
end;

procedure TOutlookGroupedList.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  inherited;
  DragFinished;
end;

procedure TOutlookGroupedList.DragCanceled;
begin
  inherited;
  if (DragType =  dtVCL) then
    FInternalBtnUp := True;
  DragFinished;
end;

procedure TOutlookGroupedList.DragFinished;
(* Called by DragCancelled or EndDrag to make up for the still missing mouse
   button up messages. These are important for such important things like
   popup menus. *)
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);

  if lsRightButtonDown in FStates then
    Perform(WM_RBUTTONUP, 0, MakeLParam(p.X, p.y))
  else
    Perform(WM_LBUTTONUP, 0, MakeLParam(p.X, p.y));

  DoStateChange([], [lsVCLDragPending, lsVCLDragging]);
end;

procedure TOutlookGroupedList.BeginDrag(Immediate: Boolean; Threshold: Integer);
// Reintroduced method to allow to start OLE drag'n drop as well as VCL drag'n drop
begin
  if FDragType = dtVCL then
  begin
    DoStateChange([lsVCLDragPending]);
    inherited;
  end else
  if (FStates * [lsOLEDragPending, lsOLEDragging]) = [] then
  begin
    // Drag start position has already been recorded in WMMouseDown.
    if Threshold < 0 then
      FDragThreshold := Mouse.DragThreshold
    else
      FDragThreshold := Threshold;
    if Immediate then
      DoOLEDragging(FFromSelPoint)
    else
      DoStateChange([lsOLEDragPending]);
  end;
end;

function TOutlookGroupedList.Dragging: Boolean;
begin
  // Check for both OLE drag'n drop as well as VCL drag'n drop
  Result := inherited Dragging or
    (FStates * [lsOLEDragPending, lsOLEDragging] <> []);
end;

// Register --------------------------------------------------------------------

//procedure Register;
//begin
//  RegisterComponents('TMS', [TOutlookGroupedList]);
//end;

procedure TOutlookGroupedList.SetInternalRootItem(RItem: POGLItem);
begin
  if (RItem <> nil) and (RItem <> FRootItem) then
  begin
    FRootItem := RItem;
  end;
end;

function TOutlookGroupedList.GetRealRect(Item: POGLItem; IncludeChildren: Boolean = False): TRect;
begin
  if (Item <> nil) then
  begin
    Result := GetDisplayRect(Item);
    InflateRect(Result, 1, 1);
    OffsetRect(Result, 0, -VertScrollBar.Position);
    if IncludeChildren and IsGroupItem(Item) then
      Inc(Result.Bottom, Item.ChildHeight);
  end;
end;

procedure TOutlookGroupedList.SetDragDropMode(const Value: TDragDropMode);
begin
  if FDragDropMode <> Value then
  begin
    FDragDropMode := Value;
  end;
end;

procedure TOutlookGroupedList.SetGroupItemHeight(Value: Integer);
begin
  FGroupItemHeight := Value;
  AdjustItemsSize;
end;

procedure TOutlookGroupedList.SetGroupLineColor(const Value: TColor);
begin
  if (FGroupLineColor <> Value) then
  begin
    FGroupLineColor := Value;
    Invalidate;
  end;
end;

procedure TOutlookGroupedList.SetShowNodes(const Value: Boolean);
begin
  FShowNodes := Value;
  Invalidate;
end;

procedure TOutlookGroupedList.SetGroupShowCount(const Value: Boolean);
begin
  FGroupShowCount := Value;
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    Invalidate;
end;

procedure TOutlookGroupedList.SetGroupColor(const Value: TColor);
begin
  if (FGroupColor <> Value) then
  begin
    FGroupColor := Value;
    Invalidate;
  end;
end;

procedure TOutlookGroupedList.SetGroupCountFont(const Value: TFont);
begin
  FGroupCountFont.Assign(Value);
end;

procedure TOutlookGroupedList.SetGroupFont(const Value: TFont);
begin
  FGroupFont.Assign(Value);
end;

procedure TOutlookGroupedList.SetGroupSelectionColor(const Value: TColor);
begin
  if (FGroupSelectionColor <> Value) then
  begin
    FGroupSelectionColor := Value;
    Invalidate;
  end;
end;

procedure TOutlookGroupedList.SetGroupSelectionTextColor(
  const Value: TColor);
begin
  if (FGroupSelectionTextColor <> Value) then
  begin
    FGroupSelectionTextColor := Value;
    Invalidate;
  end;
end;

initialization
  // nothing to do here

finalization
  GlobalFinalizeOLE;

end.
