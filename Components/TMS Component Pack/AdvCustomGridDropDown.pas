{ **************************************************************************}
{ TAdvGridDropDown components                                               }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2010 - 2014                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

{$I TMSDEFS.INC}

unit AdvCustomGridDropDown;

interface

uses
  Classes, Windows, Graphics, Controls, Messages, ImgList, Grids, Forms,
  SysUtils, AdvDropDown, AdvStyleIF, AdvObj
  {$IFDEF TMSGDIPLUS}
  , AdvHintInfo
  {$ENDIF}
  ;

const
  MAJ_VER = 6; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // 5.5.0.1 : Fixed : Issue with sequence of OnBeforeDropDown event
  // 6.0.0.0 : Compatible with TAdvStringGrid v6.0
  //         : Fixed issue with Columns[].Alignment when UseItems = false
  //         : Fixed issue with autosize columns when UseItems = false
  //         : Fixed issue with format when UseItems = false
  // 6.0.1.0 : New : AutoColumnSize property added
  //         : New : LookupMethod lmNone added
  // 6.0.1.1 : Fixed : Issue with font initialization when UseItems = true
  // 6.0.1.2 : Fixed : Issue with handling Columns[].Font
  //         : Fixed : Issue with lookup & selecting from dropdown
  // 6.0.1.3 : Fixed : Issue with programmatically using filtering
  // 6.0.2.0 : New : Exposed Alignment property
  // 6.0.2.1 : Fixed : Issue with showing lookup value when FixedRows <> 1


type
  TCustomAdvGridDropDown = class;

  TColumnType = (ctText, ctImage);

  TGetCellTextEvent = procedure(Sender: TObject; Column, Row: Integer;
    var Text: string) of object;

  TDrawCellEvent = procedure(Sender: TObject; Column, Row: Integer;
    ACanvas: TCanvas; ARect: TRect) of object;

  TGetCellPropEvent = procedure(Sender: TObject; Column, Row: Integer;
    Font: TFont; var AColor, AColorTo: TColor) of object;

  TDropDownColumn = class(TCollectionItem)
  private
    FWordwrap: Boolean;
    FWidth: Integer;
    FAutoSize: Boolean;
    FHeader: string;
    FAlignment: TAlignment;
    FColor: TColor;
    FColumnType: TColumnType;
    FFont: TFont;
    FEllipsis: Boolean;
    procedure SetFont(const Value: TFont);
  protected
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Width: Integer read FWidth write FWidth default 80;
    property Color: TColor read FColor write FColor default clWhite;
    property Alignment: TAlignment read FAlignment write FAlignment default taLeftJustify;
    property Ellipsis: Boolean read FEllipsis write FEllipsis default False;
    property Font: TFont read FFont write SetFont;
    property ColumnType: TColumnType read FColumnType write FColumnType default ctText;
    // for ctImage, ImageIndex is used
    property Header: string read FHeader write FHeader; // (column header text)
    property Wordwrap: Boolean read FWordwrap write FWordwrap default False;
    // (allow wordwrap)
    property AutoSize: Boolean read FAutoSize write FAutoSize default False;
    // column width adapts to size of text
  end;

  TDropDownColumns = class(TCollection)
  private
    FMyOwner: TPersistent;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TDropDownColumn;
    procedure SetItem(Index: Integer; const Value: TDropDownColumn);
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TDropDownColumn read GetItem write SetItem;
    default;
    function Add: TDropDownColumn;
    function Insert(Index: Integer): TDropDownColumn;
    function GetOwner: TPersistent; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TDropDownItem = class(TCollectionItem)
  private
    FImageIndex: Integer;
    FText: TStringList;
    procedure SetText(const Value: TStringList);
  protected
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ImageIndex: Integer read FImageIndex write FImageIndex default - 1;
    property Text: TStringList read FText write SetText;
  end;

  TDropDownItemClass = class of TDropDownItem;

  TDropDownItems = class(TCollection)
  private
    FMyOwner: TPersistent;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TDropDownItem;
    procedure SetItem(Index: Integer; const Value: TDropDownItem);
  protected
    function GetItemClass: TDropDownItemClass; virtual;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TDropDownItem read GetItem write SetItem; default;
    function Add: TDropDownItem;
    function Insert(Index: Integer): TDropDownItem;
    function GetOwner: TPersistent; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TDropDownStyle = (dsDropDown, dsDropDownList);

  TGridSorting = (gsNone, gsSingleColumn, gsMultiColumn);

  TLookupMethod = (lmLookup, lmNarrowDown, lmNone);

  TCustomAdvGridDropDown = class(TAdvCustomDropDown)
  private
    FDesignTime: Boolean;
    FBalloonInit: Boolean;
    FCaseSensitive: Boolean;
    FColumnSizing: Boolean;
    FColumnMoving: Boolean;
    FLookupColumn: Integer;
    FLookupMethod: TLookupMethod;
    FHeaderColorTo: TColor;
    FHeaderColor: TColor;
    FLineColor: TColor;
    FUseItems: Boolean;
    FItems: TDropDownItems;
    FColumns: TDropDownColumns;
    FItemIndex: Integer;
    FHeaderHeight: Integer;
    FHeaderFont: TFont;
    FAdvColGrid: TStringGrid;
    FFixedLineColor: TColor;
    FSelRow: Integer;
    workmode: Boolean;
    FLookupStr: string;
    FOldValue: string;
    FRowSelect: Boolean;
    FReturnIsTab: Boolean;
    FLookupItems: TStringList;
    FMatchCase: Boolean;
    FMatchStart: Boolean;
    FItemChange: Boolean;
    FItemIdx: Integer;
    FItemSel: Integer;
    FSearchFooter: TSearchFooter;
    FBands: TBands;
    FNavigation: TNavigation;
    FMouseActions: TMouseActions;
    FOnSelect: TNotifyEvent;

    FOnAnchorClick: TAnchorClickEvent;
    FOnAnchorEnter: TAnchorEvent;
    FOnAnchorExit: TAnchorEvent;
    FOnAnchorHint: TAnchorHintEvent;

    FOnCanSort: TCanSortEvent;
    FOnClickSort: TClickSortEvent;
    FOnCustomCompare: TCustomCompareEvent;
    FOnRawCompare: TRawCompareEvent;

    FOnCustomCellDraw: TCustomCellDrawEvent;
    FOnCustomCellBkgDraw: TCustomCellDrawEvent;
    FOnGetAlignment: TGridAlignEvent;
    FOnGetCellColor: TGridColorEvent;
    FOnGetCellGradient: TGridGradientEvent;
    FOnGetDisplText: TGetDisplTextEvent;
    FOnGetFloatFormat: TFloatFormatEvent;
    FOnGetFormat: TGridFormatEvent;
    FOnGridHint: TGridHintEvent;
    FOnRightClickCell: TClickCellEvent;
    FOnScrollHint: TScrollHintEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyUp: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnButtonClick: TButtonClickEvent;
    FOnCheckBoxClick: TCheckBoxClickEvent;

    FOnColumnSize: TColumnSizeEvent;
    FOnColumnSizing: TColumnSizingEvent;
    FOnEndColumnSize: TEndColumnSizeEvent;
    FOnRowSize: TRowSizeEvent;
    FOnRowSizing: TRowSizingEvent;
    FOnEndRowSize: TEndRowSizeEvent;

    FOnGetEditorType: TGetEditorTypeEvent;
    FOnGetEditorProp: TClickCellEvent;
    FOnCanEditCell: TCanEditCellEvent;
    FOnEllipsClick: TEllipsClickEvent;
    FOnCellBalloon: TGridBalloonEvent;

    FOnAutoInsertCol: TAutoInsertColEvent;
    FOnAutoInsertRow: TAutoInsertRowEvent;
    FOnAutoDeleteRow: TAutoDeleteRowEvent;
    FOnAutoAddRow: TAutoAddRowEvent;
    FOnCanAddRow: TCanAddRowEvent;
    FOnCanDeleteRow: TCanDeleteRowEvent;
    FOnCanInsertRow: TCanInsertRowEvent;

    FOnColumnMove: TColumnSizeEvent;
    FOnColumnMoved: TMovedEvent;
    FOnColumnMoving: TColumnSizeEvent;
    FOnRowMove: TRowSizeEvent;
    FOnRowMoving: TRowSizeEvent;
    FOnRowMoved: TMovedEvent;
    FOnTopLeftChanged: TNotifyEvent;
    FOnSearchEditChange: TSearchEditChangeEvent;
    FOnSpinClick: TSpinClickEvent;
    {$IFDEF TMSGDIPLUS}
    FOnOfficeHint: TOfficeHintEvent;
    {$ENDIF}
    FOldItemIndex: Integer;
    FDropDownAutoWidth: Boolean;
    FColumnSizeWithDropDown: Boolean;
    FSelectionTextColor: TColor;
    FDropDownRowHeight: Integer;
    FTempItemIndex: Integer;
    FStyle: TDropDownStyle;
    FLookupEntry: string;
    FHoverRow: Boolean;
    FHoverRowColor: TColor;
    FHoverRowColorTo: TColor;
    FSorting: TGridSorting;
    FGridEditable: Boolean;
    FGridImages: TCustomImageList;
    FRowMoving: Boolean;
    FRowSizing: Boolean;
    FAutoShowDropDown: Boolean;
    FAutoSizeColumns: Boolean;
    FNodeCell: Boolean;

    procedure WMKeyDown(var Msg: TWMKeydown); message WM_KEYDOWN;
    procedure WMChar(var Msg: TWMKey); message WM_CHAR;
    procedure OnColGridSelectCell(Sender: TObject; ACol, ARow: Longint;
      var CanSelect: Boolean);
    procedure OnColGridSelect(Sender: TObject);
    procedure OnColGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OnGridBeforeSort(Sender: TObject; ACol: Integer;
      var DoSort: Boolean);
    procedure OnGridAfterSort(Sender: TObject; ACol: Integer);
    function GetRowCount: Integer;
    procedure SetLookupColumn(const Value: Integer);
    procedure SetItems(const Value: TDropDownItems);
    procedure SetColumns(const Value: TDropDownColumns);
    function GetTextEx: string;
    procedure SetItemIndex(const Value: Integer);
    procedure SetTextEx(const Value: string);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetHeaderHeight(const Value: Integer);
    procedure SetLineColor(const Value: TColor);
    procedure UpdateLookupList;
    procedure SyncColWidths;
    function GetItemIndexFromRow(ARow: Integer): Integer;
    function GetRowFromItemIndex(AItemIndex: Integer): Integer;
    procedure SetStyle(const Value: TDropDownStyle);
    procedure SetSorting(const Value: TGridSorting);
    procedure DoGridCheckBoxClick(Sender: TObject; ACol, ARow: Integer;
      State: Boolean);
    procedure SetGridImages(const Value: TCustomImageList);
    procedure SetSearchFooter(const Value: TSearchFooter);
    procedure SetLookupMethod(const Value: TLookupMethod);
    procedure SetMouseActions(const Value: TMouseActions);
    procedure SetNavigation(const Value: TNavigation);
    procedure SetBands(const Value: TBands);
  protected
    function GetVersionNr: Integer; override;
    function GetGrid: TStringGrid;

    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure Loaded; override;
    procedure Change; override;
    function CreateItems(AOwner: TComponent): TDropDownItems; virtual;

    procedure CreateDropDownForm; override;
    procedure AdaptDropDownSize(var AHeight: integer); override;
    procedure BeforeDropDown; override;
    procedure AfterDropDown; override;
    procedure OnHideDropDown; override;
    procedure UpdateDropDownSize; override;
    procedure DoHideDropDown(Canceled: Boolean); override;
    procedure DoShowDropDown; override;
    procedure SetSelectionColorStyle(const Value: TSelectionColorStyle); override;
    procedure SetCenterControl; override;

    procedure OnDropDownControlKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure OnDropDownControlKeyUp(var Key: Word; Shift: TShiftState); override;
    procedure OnDropDownControlKeyPress(var Key: char); override;
    procedure OnDropDownSizing; override;

    procedure DoGridAnchorClick(Sender: TObject; ARow, ACol: Integer;
      Anchor: string; var AutoHandle: Boolean); virtual;
    procedure DoGridAnchorEnter(Sender: TObject; ARow, ACol: Integer;
      Anchor: string); virtual;
    procedure DoGridAnchorExit(Sender: TObject; ARow, ACol: Integer;
      Anchor: string); virtual;
    procedure DoGridAnchorHint(Sender: TObject; ARow, ACol: Integer;
      var Anchor: string); virtual;

    procedure DoGridButtonClick(Sender: TObject; ACol, ARow: Integer); virtual;

    procedure DoGridCanSort(Sender: TObject; ACol: Integer;
      var DoSort: Boolean); virtual;
    procedure DoGridClickSort(Sender: TObject; ACol: Integer); virtual;
    procedure DoGridCustomCompare(Sender: TObject; str1, str2: string;
      var Res: Integer); virtual;
    procedure DoGridRawCompare(Sender: TObject; ACol, Row1, Row2: Integer;
      var Res: Integer); virtual;

    procedure DoGridColumnSize(Sender: TObject; ACol: Integer;
      var Allow: Boolean); virtual;
    procedure DoGridColumnSizing(Sender: TObject; ACol, ColumnSize: Integer); virtual;
    procedure DoGridEndColumnSize(Sender: TObject; ACol: Integer); virtual;

    procedure DoGridRowSize(Sender: TObject; ARow: Integer; var Allow: Boolean); virtual;
    procedure DoGridRowSizing(Sender: TObject; ARow, RowHeight: Integer); virtual;
    procedure DoGridEndRowSize(Sender: TObject; ARow: Integer); virtual;

    procedure DoGridRowMove(Sender: TObject; ARow: Integer; var Allow: Boolean); virtual;
    procedure DoGridRowMoving(Sender: TObject; ARow: Integer;
      var Allow: Boolean); virtual;
    procedure DoGridRowMoved(Sender: TObject; FromIndex, ToIndex: Integer); virtual;

    procedure DoGridColumnMove(Sender: TObject; ACol: Integer;
      var Allow: Boolean); virtual;
    procedure DoGridColumnMoving(Sender: TObject; ACol: Integer;
      var Allow: Boolean); virtual;
    procedure DoGridColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer); virtual;

    procedure DoGridCustomCellDraw(Sender: TObject; Canvas: TCanvas;
      ACol, ARow: Integer; AState: TGridDrawState; ARect: TRect;
      Printing: Boolean); virtual;

    procedure DoGridCustomCellBkgDraw(Sender: TObject; Canvas: TCanvas;
      ACol, ARow: Integer; AState: TGridDrawState; ARect: TRect;
      Printing: Boolean); virtual;

    procedure DoGridCellBalloon(Sender: TObject; ACol, ARow: Integer;
      var ATitle: string; var AText: string; var AIcon: Integer); virtual;
    procedure DoGridGetAlignment(Sender: TObject; ARow, ACol: Integer;
      var HAlign: TAlignment; var VAlign: TVAlignment); virtual;
    procedure DoGridGetCellColor(Sender: TObject; ARow, ACol: Integer;
      AState: TGridDrawState; ABrush: TBrush; AFont: TFont); virtual;
    procedure DoGridGetCellGradient(Sender: TObject; ARow, ACol: Integer;
      var Color, ColorTo, ColorMirror, ColorMirrorTo: TColor;
      var GD: TCellGradientDirection); virtual;

    procedure DoGridGetFloatFormat(Sender: TObject; ACol, ARow: Integer;
      var IsFloat: Boolean; var FloatFormat: string); virtual;

    procedure DoGridGetEditorType(Sender: TObject; ACol, ARow: Integer;
      var AEditor: TEditorType); virtual;
    procedure DoGridGetEditorProp(Sender: TObject; ACol, ARow: Integer); virtual;
    procedure DoGridCanEditCell(Sender: TObject; ARow, ACol: Integer;
      var CanEdit: Boolean); virtual;
    procedure DoGridEllipsClick(Sender: TObject; ACol, ARow: Integer;
      var S: string); virtual;

    procedure DoGridGetDisplText(Sender: TObject; ACol, ARow: Integer;
      var Value: string); virtual;
    procedure DoGridGetFormat(Sender: TObject; ACol: Integer;
      var AStyle: TSortStyle; var aPrefix, aSuffix: string); virtual;
    procedure DoGridHint(Sender: TObject; ARow, ACol: Integer;
      var hintstr: string); virtual;

    {$IFDEF TMSGDIPLUS}
    procedure DoGridOfficeHint(Sender: TObject; ACol, ARow: Integer;
      OfficeHint: TAdvHintInfo); virtual;
    {$ENDIF}
    procedure DoGridMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); virtual;
    procedure DoGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;

    procedure DoGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoGridKeyPress(Sender: TObject; var Key: char); virtual;

    procedure DoGridRightClickCell(Sender: TObject; ARow, ACol: Integer); virtual;
    procedure DoGridScrollHint(Sender: TObject; ARow: Integer;
      var hintstr: string); virtual;
    procedure DoGridTopLeftChanged(Sender: TObject); virtual;
    procedure DoGridSearchEditChange(Sender: TObject; Value: string;
      var DefaultSearch: Boolean); virtual;
    procedure DoGridSpinClick(Sender: TObject; ACol, ARow, AValue: Integer;
      UpDown: Boolean); virtual;

    procedure DoGridAutoAddRow(Sender: TObject; ARow: Integer); virtual;
    procedure DoGridAutoInsertRow(Sender: TObject; ARow: Integer); virtual;
    procedure DoGridAutoInsertCol(Sender: TObject; ACol: Integer); virtual;
    procedure DoGridAutoDeleteRow(Sender: TObject; ARow: Integer); virtual;

    procedure DoGridCanAddRow(Sender: TObject; var CanAdd: Boolean); virtual;
    procedure DoGridCanInsertRow(Sender: TObject; ARow: Integer;
      var CanInsert: Boolean); virtual;
    procedure DoGridCanDeleteRow(Sender: TObject; ARow: Integer;
      var CanDelete: Boolean); virtual;

    procedure HandleMouseWheelDown; override;
    procedure HandleMouseWheelUp; override;

    function GetItemCount: Integer;
    procedure SelectFirst;
    procedure SelectLast;
    procedure SelectNext;
    procedure SelectPrevious;
    procedure SelectNextPage;
    procedure SelectPrevPage;

    function LookupInColumn(Value: string; var lookup: string; var Row: Integer): Boolean;
    function DoKeyLookup(ch: char): Integer;
    procedure DoEnter; override;

    procedure StartBlockFocus(Sender: TObject);
    procedure EndBlockFocus(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetComponentStyle(AStyle: TTMSStyle); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property RowCount: Integer read GetRowCount;
    // number of rows in the dropdown
    property Text: string read GetTextEx write SetTextEx;
    // holds control.Items[ItemIndex].Text[LookupColumn]
    property ReturnIsTab: Boolean read FReturnIsTab write FReturnIsTab;
    property MatchCase: Boolean read FMatchCase write FMatchCase default False;
    property MatchStart: Boolean read FMatchStart write FMatchStart default true;
  published
    property AutoShowDropDown: Boolean read FAutoShowDropDown write FAutoShowDropDown default False;
    property AutoSizeColumns: Boolean read FAutoSizeColumns write FAutoSizeColumns default True;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default False;
    property DropDownAutoWidth: Boolean read FDropDownAutoWidth write FDropDownAutoWidth default true;
    property DropDownRowHeight: Integer read FDropDownRowHeight write FDropDownRowHeight default 22;
    property LineColor: TColor read FLineColor write SetLineColor default clGray;
    // (color of lines between columns, clNone = no line)
    property FixedLineColor: TColor read FFixedLineColor write FFixedLineColor;

    property Bands: TBands read FBands write SetBands;
    property ColumnSizing: Boolean read FColumnSizing write FColumnSizing default False;
    property ColumnMoving: Boolean read FColumnMoving write FColumnMoving default False;
    // when true, columns are sizeable at runtime
    property ColumnSizeWithDropDown: Boolean read FColumnSizeWithDropDown write
      FColumnSizeWithDropDown default true;
    property Columns: TDropDownColumns read FColumns write SetColumns;
    property GridEditable: Boolean read FGridEditable write FGridEditable default False;
    property GridImages: TCustomImageList read FGridImages write SetGridImages;
    property HeaderColor: TColor read FHeaderColor write FHeaderColor;
    // gradient start color of header
    property HeaderColorTo: TColor read FHeaderColorTo write FHeaderColorTo;
    // gradient end color of header (when clNone, solid color is used)
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property HoverRow: Boolean read FHoverRow write FHoverRow default False;
    property HoverRowColor: TColor read FHoverRowColor write FHoverRowColor default clInfoBk;
    property HoverRowColorTo: TColor read FHoverRowColorTo write FHoverRowColorTo default clNone;
    property Items: TDropDownItems read FItems write SetItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default - 1;
    property LookupColumn: Integer read FLookupColumn write SetLookupColumn default 0; // column used to show text of selected item in edit control and column in which lookup while typing is performed, ie. type 'M' , 'e' , 'r' and the item in LookupColumn in the dropdown will become selected.
    property LookupMethod: TLookupMethod read FLookupMethod write SetLookupMethod default lmLookup;
    property MouseActions: TMouseActions read FMouseActions write SetMouseActions;
    property Navigation: TNavigation read FNavigation write SetNavigation;
    property RowSizing: Boolean read FRowSizing write FRowSizing default False;
    property RowMoving: Boolean read FRowMoving write FRowMoving default False;
    property SelectionTextColor: TColor read FSelectionTextColor write FSelectionTextColor default clBlack;
    property RowSelect: Boolean read FRowSelect write FRowSelect default true;
    property SearchFooter: TSearchFooter read FSearchFooter write  SetSearchFooter;
    property Sorting: TGridSorting read FSorting write SetSorting default gsNone;
    property Style: TDropDownStyle read FStyle write SetStyle;
    property UseItems: Boolean read FUseItems write FUseItems default true;

    // published base class properties
    property Align;
    {$IFDEF DELPHIXE_LVL}
    property Alignment;
    {$ENDIF}
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property BorderColor;
    property DisabledBorder;
    property FocusBorderColor;

    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property DropDownColor;
    property DropDownBorderColor;
    property DropDownBorderWidth;
    property DropDownShadow;
    property DropDownWidth;
    property DropDownHeight;
    property DropPosition;
    property DropDownButtonWidth;
    property DropDownButtonHint;
    property DropDownSizeable;
    property Enabled;
    property Font;
    property DropDownButtonGlyph;
    property Images;

    property LabelCaption;
    property LabelPosition;
    property LabelMargin;
    property LabelTransparent;
    property LabelAlwaysEnabled;
    property LabelFont;

    property Version;
    property SelectionColor;
    property SelectionColorTo;
    property ButtonAppearance;
    property DropDownHeader;
    property DropDownFooter;
    property DragCursor;
    property DragKind;
    property DragMode;
    property TabStop;
    property TabOrder;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnEnter;
    property OnExit;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
{$IFDEF DELPHI2006_LVL}
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnBeforeDropDown;
    property OnDropDown;
    property OnDropUp;
    property OnDropDownHeaderButtonClick;
    property OnDropDownFooterButtonClick;
    property OnDrawHeader;
    property OnDrawFooter;
    property OnGetHeaderText;
    property OnGetFooterText;
    property OnGetDropDownPos;

    property OnGridAnchorClick
      : TAnchorClickEvent read FOnAnchorClick write FOnAnchorClick;
    property OnGridAnchorEnter
      : TAnchorEvent read FOnAnchorEnter write FOnAnchorEnter;
    property OnGridAnchorExit
      : TAnchorEvent read FOnAnchorExit write FOnAnchorExit;
    property OnGridAnchorHint
      : TAnchorHintEvent read FOnAnchorHint write FOnAnchorHint;

    property OnGridAutoAddRow
      : TAutoAddRowEvent read FOnAutoAddRow write FOnAutoAddRow;
    property OnGridInsertRow: TAutoInsertRowEvent read FOnAutoInsertRow write
      FOnAutoInsertRow;
    property OnGridDeleteRow: TAutoDeleteRowEvent read FOnAutoDeleteRow write
      FOnAutoDeleteRow;
    property OnGridInsertCol: TAutoInsertColEvent read FOnAutoInsertCol write
      FOnAutoInsertCol;
    property OnGridCanAddRow
      : TCanAddRowEvent read FOnCanAddRow write FOnCanAddRow;
    property OnGridCanDeleteRow
      : TCanDeleteRowEvent read FOnCanDeleteRow write FOnCanDeleteRow;
    property OnGridCanInsertRow: TCanInsertRowEvent read FOnCanInsertRow write FOnCanInsertRow;
    property OnGridButtonClick: TButtonClickEvent read FOnButtonClick write FOnButtonClick;
    property OnGridCanEditCell: TCanEditCellEvent read FOnCanEditCell write FOnCanEditCell;
    property OnGridCheckBoxClick: TCheckBoxClickEvent read FOnCheckBoxClick write FOnCheckBoxClick;
    property OnGridCanSort: TCanSortEvent read FOnCanSort write FOnCanSort;
    property OnGridClickSort: TClickSortEvent read FOnClickSort write FOnClickSort;
    property OnGridCustomCompare: TCustomCompareEvent read FOnCustomCompare write FOnCustomCompare;
    property OnGridRawCompare: TRawCompareEvent read FOnRawCompare write FOnRawCompare;
    property OnGridColumnSize: TColumnSizeEvent read FOnColumnSize write FOnColumnSize;
    property OnGridEndColumnSize: TEndColumnSizeEvent read FOnEndColumnSize write FOnEndColumnSize;
    property OnGridColumnSizing: TColumnSizingEvent read FOnColumnSizing write FOnColumnSizing;
    property OnGridRowSize: TRowSizeEvent read FOnRowSize write FOnRowSize;
    property OnGridRowSizing: TRowSizingEvent read FOnRowSizing write FOnRowSizing;
    property OnGridEndRowSize: TEndRowSizeEvent read FOnEndRowSize write FOnEndRowSize;

    property OnGridRowMove: TRowSizeEvent read FOnRowMove write FOnRowMove;
    property OnGridRowMoving: TRowSizeEvent read FOnRowMoving write FOnRowMoving;
    property OnGridRowMoved: TMovedEvent read FOnRowMoved write FOnRowMoved;

    property OnGridColumnMove: TColumnSizeEvent read FOnColumnMove write FOnColumnMove;
    property OnGridColumnMoving: TColumnSizeEvent read FOnColumnMoving write FOnColumnMoving;
    property OnGridColumnMoved: TMovedEvent read FOnColumnMoved write FOnColumnMoved;

    property OnGridCustomCellDraw: TCustomCellDrawEvent read FOnCustomCellDraw write FOnCustomCellDraw;
    property OnGridCustomCellBkgDraw: TCustomCellDrawEvent read FOnCustomCellBkgDraw write
      FOnCustomCellBkgDraw;
    property OnGridGetAlignment: TGridAlignEvent read FOnGetAlignment write FOnGetAlignment;
    property OnGridGetCellColor: TGridColorEvent read FOnGetCellColor write FOnGetCellColor;
    property OnGridGetCellGradient: TGridGradientEvent read FOnGetCellGradient write FOnGetCellGradient;
    property OnGridGetFloatFormat: TFloatFormatEvent read FOnGetFloatFormat write FOnGetFloatFormat;

    property OnGridGetEditorType: TGetEditorTypeEvent read FOnGetEditorType write FOnGetEditorType;
    property OnGridGetEditorProp: TClickCellEvent read FOnGetEditorProp write FOnGetEditorProp;
    property OnGridEllipsClick: TEllipsClickEvent read FOnEllipsClick write FOnEllipsClick;

    property OnGridGetDiplText: TGetDisplTextEvent read FOnGetDisplText write FOnGetDisplText;
    property OnGridGetFormat: TGridFormatEvent read FOnGetFormat write FOnGetFormat;
    property OnGridHint: TGridHintEvent read FOnGridHint write FOnGridHint;

    {$IFDEF TMSGDIPLUS}
    property OnGridOfficeHint: TOfficeHintEvent read FOnOfficeHint write FOnOfficeHint;
    {$ENDIF}

    property OnGridCellBalloon: TGridBalloonEvent read FOnCellBalloon write FOnCellBalloon;

    property OnGridMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnGridMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnGridMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;

    property OnGridKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnGridKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnGridKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;

    property OnGridRightClickCell: TClickCellEvent read FOnRightClickCell write FOnRightClickCell;
    property OnGridScrollHint: TScrollHintEvent read FOnScrollHint write FOnScrollHint;
    property OnGridTopLeftChanged: TNotifyEvent read FOnTopLeftChanged write FOnTopLeftChanged;
    property OnGridSearchEditChange: TSearchEditChangeEvent read FOnSearchEditChange write
      FOnSearchEditChange;
    property OnGridSpinClick: TSpinClickEvent read FOnSpinClick write FOnSpinClick;
  end;

implementation

uses
  StdCtrls, BaseGrid, AdvGrid, AdvUtil;

type

  TAdvColGrid = class(TAdvStringGrid)
  private
    FAdvDropDown: TCustomAdvGridDropDown;
    FColumns: TDropDownColumns;
    FHeaderColor: TColor;
    FHeaderColorTo: TColor;
    FLineColor: TColor;
    FImages: TCustomImageList;
    FOnDrawCell: TDrawCellEvent;
    FOnGetCellProp: TGetCellPropEvent;
    FOnGetCellText: TGetCellTextEvent;
    FHeaderHeight: Integer;
    FHeaderFont: TFont;
    FFixedLineColor: TColor;
    FGridLineWidth: Integer;
    FOnSelect: TNotifyEvent;
    FOldRow: Integer;
    FMouseWheelSelection: Boolean;
    FConfirmSelection: Boolean;
    procedure WMChar(var Msg: TWMChar);
    message WM_CHAR;
    procedure WMKeyDown(var Msg: TWMKeydown);
    message WM_KEYDOWN;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure SetHeaderColor(const Value: TColor);
    procedure SetHeaderColorTo(const Value: TColor);
    procedure SetLineColor(const Value: TColor);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetColumns(const Value: TDropDownColumns);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetHeaderHeight(const Value: Integer);
    procedure SetAdvDropDown(const Value: TCustomAdvGridDropDown);
    procedure SetGridLineWidth(const Value: Integer);
  protected
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation);
      override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoSelectionChanged; Virtual;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
      override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
      override;
    procedure AutoSizeColumnInt(ACol: Integer);
    procedure AutoSizeColumnsInt;
    procedure DoWordWrap;
    procedure Populate;
    procedure Initialize(DropDown: TCustomAdvGridDropDown);
    function GetDesiredWidth: Integer;
    property AdvDropDown: TCustomAdvGridDropDown read FAdvDropDown write
      SetAdvDropDown;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
  published
    property Columns: TDropDownColumns read FColumns write SetColumns;
    property LineColor: TColor read FLineColor write SetLineColor default clGray;
    // (color of lines between columns, clNone = no line)
    property FixedLineColor: TColor read FFixedLineColor write FFixedLineColor;
    property HeaderColor: TColor read FHeaderColor write SetHeaderColor;
    // gradient start color of header
    property HeaderColorTo: TColor read FHeaderColorTo write SetHeaderColorTo;
    // gradient end color of header (when clNone, solid color is used)
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight default 25;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property GridLineWidth: Integer read FGridLineWidth write SetGridLineWidth default 1;
    property Images: TCustomImageList read FImages write SetImages;

    property OnGetCellText: TGetCellTextEvent read FOnGetCellText write FOnGetCellText;
    property OnDrawCell: TDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnGetCellProp: TGetCellPropEvent read FOnGetCellProp write FOnGetCellProp;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;


  //------------------------------------------------------------------------------

  { TDropDownColumn }

procedure TDropDownColumn.Assign(Source: TPersistent);
begin
  if (Source is TDropDownColumn) then
  begin
    FWordwrap := (Source as TDropDownColumn).Wordwrap;
    FWidth := (Source as TDropDownColumn).Width;
    FHeader := (Source as TDropDownColumn).Header;
    FAlignment := (Source as TDropDownColumn).Alignment;
    FColor := (Source as TDropDownColumn).Color;
    FColumnType := (Source as TDropDownColumn).ColumnType;
    FFont.Assign((Source as TDropDownColumn).Font);
    FEllipsis := (Source as TDropDownColumn).Ellipsis;
    AutoSize := (Source as TDropDownColumn).AutoSize;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

constructor TDropDownColumn.Create(Collection: TCollection);
begin
  inherited;
  FWordwrap := False;
  FWidth := 80;
  FAutoSize := False;
  FHeader := '';
  FAlignment := taLeftJustify;
  FColor := clWhite;
  FColumnType := ctText;
  FFont := TFont.Create;
end;

//------------------------------------------------------------------------------

destructor TDropDownColumn.Destroy;
begin
  FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDropDownColumn.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------

{ TDropDownColumns }

function TDropDownColumns.Add: TDropDownColumn;
begin
  Result := TDropDownColumn( inherited Add);
end;

//------------------------------------------------------------------------------

constructor TDropDownColumns.Create(AOwner: TPersistent);
begin
  inherited Create(TDropDownColumn);
  FMyOwner := AOwner;
end;

//------------------------------------------------------------------------------

function TDropDownColumns.GetItem(Index: Integer): TDropDownColumn;
begin
  Result := TDropDownColumn( inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TDropDownColumns.GetOwner: TPersistent;
begin
  Result := FMyOwner;
end;

//------------------------------------------------------------------------------

function TDropDownColumns.Insert(Index: Integer): TDropDownColumn;
begin
  Result := TDropDownColumn( inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TDropDownColumns.SetItem(Index: Integer;
  const Value: TDropDownColumn);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TCustomAdvGridDropDown }

constructor TCustomAdvGridDropDown.Create(AOwner: TComponent);
begin
  FHeaderFont := TFont.Create;

  inherited;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  FColumnSizing := False;
  FColumnMoving := False;
  FRowSizing := False;
  FRowMoving := False;
  FLookupColumn := 0;
  FHeaderColorTo := clGray;
  FHeaderColor := clWhite;
  FHeaderHeight := 25;
  FLineColor := clSilver;
  FRowSelect := true;

  FLookupMethod := lmLookup;

  FItems := CreateItems(Self);

  FColumns := TDropDownColumns.Create(Self);
  DropDownColor := clWhite;
  DropDownHeight := 200;
  FItemIndex := -1;
  FUseItems := True;
  FOldItemIndex := FItemIndex;
  FLookupEntry := '';

  FLookupItems := TStringList.Create;
  workmode := True;
  FLookupStr := '';
  FMatchCase := False;
  FMatchStart := True;
  AutoSelect := True;
  FItemChange := False;
  FDropDownAutoWidth := True;
  SelectionColorStyle := scOffice2007;
  FSelectionTextColor := clBlack;
  FColumnSizeWithDropDown := True;
  FAutoSizeColumns := True;
  FDropDownRowHeight := 22;
  FHoverRow := False;
  FHoverRowColor := clInfoBk;
  FHoverRowColorTo := clNone;

  FSearchFooter := TSearchFooter.Create(Self, FDesignTime);
  FBands := TBands.Create;
  FNavigation := TNavigation.Create;
  FMouseActions := TMouseActions.Create(Self);
  FNavigation.HomeEndKey := heFirstLastRow;

  if FDesignTime then
  begin
    FColumns.Clear;
    FColumns.Add;
    FColumns.Add;
    FColumns.Add;
  end;
end;

//------------------------------------------------------------------------------

destructor TCustomAdvGridDropDown.Destroy;
begin
  FItems.Free;
  FBands.Free;
  FColumns.Free;
  FHeaderFont.Free;
  FLookupItems.Free;
  FSearchFooter.Free;
  FNavigation.Free;
  FMouseActions.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TCustomAdvGridDropDown.GetGrid: TStringGrid;
begin
  Result := FAdvColGrid;
end;

//------------------------------------------------------------------------------

function TCustomAdvGridDropDown.GetRowCount: Integer;
begin
  Result := Items.Count;
end;

//------------------------------------------------------------------------------

function TCustomAdvGridDropDown.GetTextEx: string;
begin
  Result := inherited Text;
end;

//------------------------------------------------------------------------------

function TCustomAdvGridDropDown.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

//------------------------------------------------------------------------------

function TCustomAdvGridDropDown.GetItemCount: Integer;
begin
  Result := 0;

  if UseItems then
    Result := Items.Count
  else if Assigned(FAdvColGrid) then
    Result := FAdvColGrid.RowCount - FAdvColGrid.FixedRows;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.HandleMouseWheelDown;
begin
  inherited;
  SelectNext;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.HandleMouseWheelUp;
begin
  inherited;
  SelectPrevious;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) then
  begin
    if AComponent = FGridImages then
    begin
      FGridImages := nil;
      if Assigned(FAdvColGrid) then
        (FAdvColGrid as TAdvColGrid).GridImages := nil;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.CreateDropDownForm;
begin
  inherited;

  if not Assigned(FAdvColGrid) then
  begin
    FAdvColGrid := TAdvColGrid.Create(Self);
    FAdvColGrid.Parent := FDropDownForm;
  end;

  (FAdvColGrid as TAdvColGrid).AdvDropDown := Self;
  FAdvColGrid.Left := 0;
  FAdvColGrid.Top := 0;
  FAdvColGrid.Height := 180;
  FAdvColGrid.Color := DropDownColor;
  FAdvColGrid.DoubleBuffered := true;
  (FAdvColGrid as TAdvColGrid).Navigation.HomeEndKey := heFirstLastRow;
  (FAdvColGrid as TAdvColGrid).MouseActions.NoAutoRangeScroll := true;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.AdaptDropDownSize(var AHeight: integer);
var
  h,hdelta: integer;

begin
  h := AHeight;
  hdelta := 0;

  if DropDownHeader.Visible then
  begin
    h := h - DropDownHeader.Height;
    hdelta := DropDownHeader.Height;
  end;

  if DropDownFooter.Visible then
  begin
    h := h - DropDownFooter.Height;
    hdelta := hdelta + DropDownFooter.Height;
  end;

  h := h - HeaderHeight - 2;
  hdelta := hdelta + HeaderHeight + 2;

  h := h div (DropDownRowHeight);

  // h = nr. of items that can be displayed , round to integral number here
  AHeight := hdelta + h * (DropDownRowHeight) + 4;
end;

procedure TCustomAdvGridDropDown.AfterDropDown;
begin
  inherited;
end;

procedure TCustomAdvGridDropDown.BeforeDropDown;
var
  idx, itemidx: Integer;
  c: string;
  InternalGrid: TAdvColGrid;

begin
  if Assigned(FDropDownForm) then
    FDropDownForm.CancelOnDeActivate := False;

  if Assigned(FAdvColGrid) then
  begin
    InternalGrid := (FAdvColGrid as TAdvColGrid);

    InternalGrid.Initialize(Self);
    FAdvColGrid.Color := DropDownColor;

    InternalGrid.SortSettings.Show := False;
    InternalGrid.SortSettings.IndexShow := False;

    case FSorting of
      gsSingleColumn:
        InternalGrid.SortSettings.Show := true;
      gsMultiColumn:
        begin
          InternalGrid.SortSettings.Show := true;
          InternalGrid.SortSettings.IndexShow := true;
        end;
    end;

    FBalloonInit := False;

    InternalGrid.GridImages := Images;
    InternalGrid.OnStartBlockFocus := StartBlockFocus;
    InternalGrid.OnEndBlockFocus := EndBlockFocus;
    InternalGrid.OnSelectCell := nil;
    InternalGrid.OnSelect := OnColGridSelect;
    InternalGrid.OnKeyDown := OnColGridKeyDown;
    InternalGrid.OnAnchorClick := DoGridAnchorClick;
    InternalGrid.OnAnchorEnter := DoGridAnchorEnter;
    InternalGrid.OnAnchorExit := DoGridAnchorExit;
    InternalGrid.OnAnchorHint := DoGridAnchorHint;
    InternalGrid.OnCanSort := DoGridCanSort;
    InternalGrid.OnGetAlignment := DoGridGetAlignment;
    InternalGrid.OnGetCellColor := DoGridGetCellColor;
    InternalGrid.OnGetCellGradient := DoGridGetCellGradient;
    InternalGrid.OnGetDisplText := DoGridGetDisplText;
    InternalGrid.OnGetFormat := DoGridGetFormat;
    InternalGrid.OnGridHint := DoGridHint;
    InternalGrid.OnCanSort := DoGridCanSort;
    InternalGrid.OnClickSort := DoGridClickSort;
    InternalGrid.OnRightClickCell := DoGridRightClickCell;
    InternalGrid.OnScrollHint := DoGridScrollHint;
    InternalGrid.OnMouseDown := DoGridMouseDown;
    InternalGrid.OnMouseUp := DoGridMouseUp;
    InternalGrid.OnMouseMove := DoGridMouseMove;
    InternalGrid.OnButtonClick := DoGridButtonClick;
    InternalGrid.OnCheckBoxClick := DoGridCheckBoxClick;
    InternalGrid.OnColumnSize := DoGridColumnSize;
    InternalGrid.OnColumnSizing := DoGridColumnSizing;
    InternalGrid.OnEndColumnSize := DoGridEndColumnSize;
    InternalGrid.OnRowSize := DoGridRowSize;
    InternalGrid.OnRowSizing := DoGridRowSizing;
    InternalGrid.OnEndRowSize := DoGridEndRowSize;
    InternalGrid.OnGetEditorType := DoGridGetEditorType;
    InternalGrid.OnGetEditorPropInt := DoGridGetEditorProp;
    InternalGrid.OnCanEditCell := DoGridCanEditCell;
    InternalGrid.OnEllipsClick := DoGridEllipsClick;
    InternalGrid.OnAutoAddRow := DoGridAutoAddRow;
    InternalGrid.OnAutoDeleteRow := DoGridAutoDeleteRow;
    InternalGrid.OnAutoInsertRow := DoGridAutoInsertRow;
    InternalGrid.OnAutoInsertCol := DoGridAutoInsertCol;
    InternalGrid.OnCanAddRow := DoGridCanAddRow;
    InternalGrid.OnCanDeleteRow := DoGridCanDeleteRow;
    InternalGrid.OnCanInsertRow := DoGridCanInsertRow;
    InternalGrid.OnCellBalloon := DoGridCellBalloon;
    InternalGrid.OnRowMove := DoGridRowMove;
    InternalGrid.OnRowMoving := DoGridRowMoving;
    InternalGrid.OnRowMoved := DoGridRowMoved;
    InternalGrid.OnColumnMove := DoGridColumnMove;
    InternalGrid.OnColumnMoving := DoGridColumnMoving;
    InternalGrid.OnColumnMoved := DoGridColumnMoved;
    InternalGrid.OnTopLeftChanged := DoGridTopLeftChanged;
    InternalGrid.OnSearchEditChange := DoGridSearchEditChange;
    InternalGrid.OnGetFloatFormat := DoGridGetFloatFormat;
    {$IFDEF TMSGDIPLUS}
    InternalGrid.OnOfficeHint := DoGridOfficeHint;
    {$ENDIF}
    InternalGrid.OnKeyUp := DoGridKeyUp;

    InternalGrid.SearchFooter.Assign(FSearchFooter);
    InternalGrid.MouseActions.Assign(FMouseActions);
    InternalGrid.Navigation.Assign(FNavigation);
    InternalGrid.Bands.Assign(FBands);

    if Assigned(OnGridCustomCellDraw) then
      InternalGrid.OnCustomCellDraw := DoGridCustomCellDraw;

    if Assigned(OnGridCustomCellBkgDraw) then
      InternalGrid.OnCustomCellBkgDraw := DoGridCustomCellBkgDraw;


    FAdvColGrid.Options := FAdvColGrid.Options + [goThumbTracking];

    if FRowSelect then
      FAdvColGrid.Options := FAdvColGrid.Options + [goRowSelect]
    else
      FAdvColGrid.Options := FAdvColGrid.Options - [goRowSelect];

    if FGridEditable then
      FAdvColGrid.Options := FAdvColGrid.Options + [goEditing]
    else
      FAdvColGrid.Options := FAdvColGrid.Options - [goEditing];

    InternalGrid.GridImages := FGridImages;
    InternalGrid.OnClickSort := OnGridClickSort;
    InternalGrid.OnCanSort := OnGridBeforeSort;
    InternalGrid.OnClickSort := OnGridAfterSort;
    InternalGrid.OnSelectCell := OnColGridSelectCell;

    InternalGrid.SetComponentStyle(GetComponentStyle);
    {
     FAdvColGrid.FixedColor := HeaderColor;
     FAdvColGrid.HeaderColor := HeaderColor;
     FAdvColGrid.HeaderColorTo := HeaderColorTo;
     FAdvColGrid.HeaderHeight := HeaderHeight;
     FAdvColGrid.HeaderFont.Assign(HeaderFont);
     FAdvColGrid.SelectionColor := SelectionColor;
     FAdvColGrid.SelectionColorTo := SelectionColorTo;
     }

    if FUseItems then
    begin
      // Invokes OnBeforeDropDown event
      inherited;
      itemidx := ItemIndex;
      InternalGrid.RowCount := Items.Count + 1;
      InternalGrid.FixedRows := 1;

      InternalGrid.Populate;
      idx := GetRowFromItemIndex(ItemIdx);
      if (idx >= InternalGrid.FixedRows) and (idx < InternalGrid.RowCount) then
        InternalGrid.Row := idx;
    end
    else
    begin
      // find first closest matching item or use item 0
      if LookupInColumn(Text, c, idx) then
        FAdvColGrid.Row := idx
      else
        FAdvColGrid.Row := FAdvColGrid.FixedRows;
    end;


    if (InternalGrid.SortSettings.Column >= 0) and
      InternalGrid.SortSettings.Show then
    begin
      InternalGrid.QSort;
    end;
  end;

  FOldItemIndex := ItemIndex;

  if not FUseItems then
    inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.OnHideDropDown;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoEnter;
begin
  inherited;
  if not (FAdvColGrid as TAdvColGrid).FilterActive and (LookupMethod = lmNarrowDown) then
    (FAdvColGrid as TAdvColGrid).RemoveAllFilters;
  FLookupEntry := '';
end;

procedure TCustomAdvGridDropDown.DoHideDropDown(Canceled: Boolean);
begin
  if not FDropDownForm.Visible then
  begin
    inherited;
    Exit;
  end;

  if not Canceled then
  begin
    if Assigned(FAdvColGrid) and
      (FAdvColGrid as TAdvColGrid).FConfirmSelection then
    begin
      if UseItems then
        ItemIndex := GetItemIndexFromRow(FSelRow)
      else
        ItemIndex := FSelRow - FAdvColGrid.FixedRows;
    end;
    FOldItemIndex := ItemIndex;
    (FAdvColGrid as TAdvColGrid).FConfirmSelection := false;
  end
  else
  begin
    ItemIndex := FOldItemIndex;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoShowDropDown;
begin
  inherited;

  if not UseItems then
  begin
    while FAdvColGrid.ColCount > Columns.Count do
      Columns.Add;
  end;

  if Self.DroppedDown then
    SyncColWidths;

  if Assigned(FAdvColGrid) then
  begin
    if (FAdvColGrid.VisibleRowCount + FAdvColGrid.FixedRows <
        FAdvColGrid.RowCount) then
    begin
      if FAdvColGrid.Row > FAdvColGrid.TopRow + FAdvColGrid.VisibleRowCount then
        FAdvColGrid.TopRow := FAdvColGrid.Row;

      if FAdvColGrid.Row < FAdvColGrid.TopRow then
        FAdvColGrid.TopRow := FAdvColGrid.Row;

      if (FAdvColGrid as TAdvStringGrid).SearchFooter.Visible then
      begin
        if FAdvColGrid.Row > FAdvColGrid.TopRow + FAdvColGrid.VisibleRowCount - 2 then
          FAdvColGrid.TopRow := FAdvColGrid.TopRow +2;
      end;
    end;
  end;

  if FSelRow < FAdvColGrid.FixedRows then
    FSelRow := FAdvColGrid.FixedRows;

  FAdvColGrid.TabStop := true;
  FAdvColGrid.SetFocus;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.EndBlockFocus(Sender: TObject);
begin
  FDropDownForm.BlockActivate := False;
  //  SetWindowPos(FDropDOwnForm.Handle,HWND_TOPMOST,0,0,0,0,SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
end;

procedure TCustomAdvGridDropDown.EndUpdate;
begin
  UpdateLookupList;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SelectFirst;
begin
  if (GetItemCount <= 0) then
    Exit;

  ItemIndex := 0;
  FAdvColGrid.Row := FAdvColGrid.FixedRows;
  FSelRow := FAdvColGrid.Row;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SelectLast;
begin
  if (GetItemCount <= 0) then
    Exit;

  ItemIndex := GetItemCount - 1;
  FAdvColGrid.Row := FAdvColGrid.RowCount - 1;
  FSelRow := FAdvColGrid.Row;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SelectNext;
begin
  if (GetItemCount <= 0) then
    Exit;

  if (FItemIndex < 0) then
  begin
    ItemIndex := 0;
    FAdvColGrid.Row := FAdvColGrid.FixedRows;
    FSelRow := FAdvColGrid.Row;
  end
  else
  begin
    if ((ItemIndex + 1) < GetItemCount) then
    begin
      ItemIndex := ItemIndex + 1;
      FAdvColGrid.Row := FAdvColGrid.Row + 1;
      FSelRow := FAdvColGrid.Row;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SelectNextPage;
begin
  if (GetItemCount <= 0) then
    Exit;

  if (FItemIndex < 0) then
  begin
    ItemIndex := 0;
    FAdvColGrid.Row := FAdvColGrid.FixedRows;
    FSelRow := FAdvColGrid.Row;
  end
  else
  begin
    if ((ItemIndex + 10) < GetItemCount) then
    begin
      ItemIndex := ItemIndex + 10;
      FAdvColGrid.Row := FAdvColGrid.Row + 10;
      FSelRow := FAdvColGrid.Row;
    end
    else
    begin
      ItemIndex := GetItemCount - 1;
      FAdvColGrid.Row := FAdvColGrid.RowCount - 1;
      FSelRow := FAdvColGrid.Row;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SelectPrevious;
begin
  if (GetItemCount <= 0) then
    Exit;

  if (FItemIndex < 0) then
  begin
    ItemIndex := 0;
    FAdvColGrid.Row := FAdvColGrid.FixedRows;
    FSelRow := FAdvColGrid.Row;
  end
  else
  begin
    if ((ItemIndex - 1) >= 0) then
    begin
      ItemIndex := ItemIndex - 1;
      FAdvColGrid.Row := FAdvColGrid.Row - 1;
      FSelRow := FAdvColGrid.Row;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SelectPrevPage;
begin
  if (GetItemCount <= 0) then
    Exit;

  if (FItemIndex < 0) then
  begin
    ItemIndex := 0;
    FAdvColGrid.Row := FAdvColGrid.FixedRows;
    FSelRow := FAdvColGrid.Row;
  end
  else
  begin
    if ((ItemIndex - 10) >= 0) then
    begin
      ItemIndex := ItemIndex - 10;
      FAdvColGrid.Row := FAdvColGrid.Row - 10;
      FSelRow := FAdvColGrid.Row;
    end
    else
    begin
      ItemIndex := 0;
      FAdvColGrid.Row := FAdvColGrid.FixedRows;
      FSelRow := FAdvColGrid.Row;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SetBands(const Value: TBands);
begin
  FBands.Assign(Value);
end;

procedure TCustomAdvGridDropDown.SetCenterControl;
begin
  Control := FAdvColGrid;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SetColumns(const Value: TDropDownColumns);
begin
  FColumns.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SetComponentStyle(AStyle: TTMSStyle);
begin
  inherited;
  case AStyle of
    tsOffice2003Blue:
      begin
        HeaderColor := $D68759;
        HeaderColorTo := $933803;
        HeaderFont.Color := clWhite;
        SelectionColorStyle := scOffice2007;
        SelectionTextColor := clBlack;
        LineColor := $962D00;
        FixedLineColor := $962D00;
      end;
    tsOffice2003Silver:
      begin
        HeaderColor := $BDA4A5;
        HeaderColorTo := $957475;
        SelectionColorStyle := scOffice2007;
        SelectionTextColor := clBlack;
        LineColor := $947C7C;
        FixedLineColor := $947C7C;
      end;
    tsOffice2003Olive:
      begin
        HeaderColor := $82C0AF;
        HeaderColorTo := $447A63;
        SelectionColorStyle := scOffice2007;
        SelectionTextColor := clBlack;
        LineColor := $588060;
        FixedLineColor := $588060;
      end;
    tsOffice2003Classic:
      begin
        HeaderColor := $808080;
        HeaderColorTo := $808080;
        SelectionColorStyle := scOffice2007;
        SelectionTextColor := clBlack;
        LineColor := $808080;
        FixedLineColor := $808080;
      end;
    tsOffice2007Luna:
      begin
        HeaderColor := $FFEFE3;
        HeaderColorTo := $FFD2AF;
        SelectionColorStyle := scOffice2007;
        HeaderFont.Color := $723708;
        SelectionTextColor := clBlack;
        LineColor := $00FFD2AF;
        FixedLineColor := $00FFD2AF;
      end;
    tsOffice2007Obsidian:
      begin
        HeaderColor := $F2F1F0;
        HeaderColorTo := $C9C2BD;
        SelectionColorStyle := scOffice2007;
        HeaderFont.Color := $433C37;
        SelectionTextColor := clBlack;
        LineColor := $5C534C;
        FixedLineColor := $5C534C;
      end;
    tsWindowsXP:
      begin
        FixedLineColor := clBlack;
        LineColor := clSilver;
        HeaderColor := clBtnFace;
        HeaderColorTo := clBtnFace;
        SelectionColorStyle := scCustom;
        SelectionColor := clHighLight;
        SelectionColorTo := clHighLight;
        HeaderFont.Color := clBlack;
        SelectionTextColor := clHighlightText;
      end;
    tsWhidbey:
      begin
        HeaderColor := $EBEEEF;
        HeaderColorTo := $7E9898;
        SelectionColorStyle := scOffice2007;
        HeaderFont.Color := clWhite;
        SelectionTextColor := clBlack;
        FixedLineColor := $962D00;
        LineColor := $962D00;
      end;
    tsOffice2007Silver:
      begin
        HeaderColor := $F8F7F6;
        HeaderColorTo := $E8E0DB;
        SelectionColorStyle := scOffice2007;
        HeaderFont.Color := $8B4215;
        SelectionTextColor := clBlack;
        FixedLineColor := $74706F;
        LineColor := $74706F;
      end;
    tsWindowsVista:
      begin
        HeaderColor := $FFFDF9;
        HeaderColorTo := $FFFAF0;
        SelectionColorStyle := scWindowsVista;
        HeaderFont.Color := clBlack;
        SelectionTextColor := clBlack;
        FixedLineColor := $FCF2DA;
        LineColor := $FCF2DA;
      end;
    tsWindows7:
      begin
        HeaderColor := $FDFBFA;
        HeaderColorTo := $FDF3EB;
        SelectionColorStyle := scWindows7;
        HeaderFont.Color := clBlack;
        SelectionTextColor := clBlack;
        FixedLineColor := $FBD6B8;
        LineColor := $FBD6B8;
      end;
    tsTerminal:
      begin
        FixedLineColor := clGray;
        LineColor := clGray;
        HeaderColor := clBtnFace;
        HeaderColorTo := clBtnFace;
        SelectionColorStyle := scCustom;
        SelectionColor := clHighLight;
        SelectionColorTo := clHighLight;
        HeaderFont.Color := clBlack;
        SelectionTextColor := clHighlightText;
      end;
      tsOffice2010Blue:
      begin
        FixedLineColor := $EEDDCF;
        LineColor := $EEDDCF;
        HeaderColor := $EDDBCD;
        HeaderColorTo := clNone;
        SelectionColorStyle := scCustom;
        SelectionColor := $6CD0FF;
        SelectionColorTo := $6CD0FF;
        HeaderFont.Color := $5B391E;
        SelectionTextColor := clBlack;
      end;
      tsOffice2010Silver:
      begin
        FixedLineColor := $EEDDCF;
        LineColor := $EEDDCF;
        HeaderColor := $EDE9E5;
        HeaderColorTo := clNone;
        SelectionColorStyle :=  scCustom;
        SelectionColor := $6CD0FF;
        SelectionColorTo := $6CD0FF;
        HeaderFont.Color := $5B391E;
        SelectionTextColor := clBlack;
      end;
      tsOffice2010Black:
      begin
        FixedLineColor := $EEDDCF;
        LineColor := $EEDDCF;
        HeaderColor := $828282;
        HeaderColorTo := clNone;
        SelectionColorStyle := scCustom;
        SelectionColor := $6CD0FF;
        SelectionColorTo := $6CD0FF;
        HeaderFont.Color := $D7D7D6;
        SelectionTextColor := clBlack;
      end;
      tsWindows8, tsWindows10:
      begin
        FixedLineColor := $E4E3E2;
        LineColor := $E4E3E2;
        HeaderColor := $F7F6F5;
        HeaderColorTo := clNone;
        SelectionColorStyle := scCustom;
        SelectionColor := $F7E0C9;
        SelectionColorTo := $F7E0C9;
        HeaderFont.Color := clBlack;
        SelectionTextColor := clBlack;
      end;
      tsOffice2013White:
      begin
        FixedLineColor := $D4D4D4;
        LineColor := $D4D4D4;
        HeaderColor := clWhite;
        HeaderColorTo := clNone;
        SelectionColorStyle := scCustom;
        SelectionColor := $FCE2C8;
        SelectionColorTo := $FCE2C8;
        HeaderFont.Color := clBlack;
        SelectionTextColor := clBlack;
      end;
      tsOffice2013LightGray:
      begin
        FixedLineColor := $C6C6C6;
        LineColor := $C6C6C6;
        HeaderColor := $F6F6F6;
        HeaderColorTo := clNone;
        SelectionColorStyle := scCustom;
        SelectionColor := $FCE2C8;
        SelectionColorTo := $FCE2C8;
        HeaderFont.Color := clBlack;
        SelectionTextColor := clBlack;
      end;
      tsOffice2013Gray:
      begin
        FixedLineColor := $ABABAB;
        LineColor := $ABABAB;
        HeaderColor := $E5E5E5;
        HeaderColorTo := clNone;
        SelectionColorStyle := scCustom;
        SelectionColor := $FCE2C8;
        SelectionColorTo := $FCE2C8;
        HeaderFont.Color := clBlack;
        SelectionTextColor := clBlack;
      end;
      tsOffice2016White:
      begin
        FixedLineColor := $D4D4D4;
        LineColor := $D4D4D4;
        HeaderColor := clWhite;
        HeaderColorTo := clNone;
        SelectionColorStyle := scCustom;
        SelectionColor := $E3BDA3;
        SelectionColorTo := $E3BDA3;
        HeaderFont.Color := clBlack;
        SelectionTextColor := clBlack;
      end;
      tsOffice2016Gray:
      begin
        FixedLineColor := $444444;
        LineColor := $444444;
        HeaderColor := $444444;
        HeaderColorTo := clNone;
        SelectionColorStyle := scCustom;
        SelectionColor := $B2B2B2;
        SelectionColorTo := $B2B2B2;
        HeaderFont.Color := $F0F0F0;
        SelectionTextColor := clBlack;
      end;
      tsOffice2016Black:
      begin
        FixedLineColor := $6A6A6A;
        LineColor := $6A6A6A;
        HeaderColor := $444444;
        HeaderColorTo := clNone;
        SelectionColorStyle := scCustom;
        SelectionColor := $444444;
        SelectionColorTo := $444444;
        HeaderFont.Color := clWhite;
        SelectionTextColor := clWhite;
      end;
  end;
end;

procedure TCustomAdvGridDropDown.SetGridImages(const Value: TCustomImageList);
begin
  FGridImages := Value;
  if Assigned(FAdvColGrid) then
    (FAdvColGrid as TAdvColGrid).GridImages := Value;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SetHeaderFont(const Value: TFont);
begin
  FHeaderFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SetHeaderHeight(const Value: Integer);
begin
  FHeaderHeight := Value;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SetItemIndex(const Value: Integer);
var
  S: string;
begin
  if UseItems then
  begin
    if {(FItemIndex <> Value) and} (Value < FItems.Count) then
    begin
      FItemIndex := Value;

      if (FItemIndex >= 0) and (FItemIndex < FItems.Count) and
        (FLookupColumn >= 0) then
      begin
        if (FLookupColumn < Items[FItemIndex].Text.Count) then
        begin
          S := Items[FItemIndex].Text[FLookupColumn];
          Text := S;
          //FOldItemIndex := FItemIndex;
        end
        else
          Text := '';
      end
      else
        Text := '';
    end;
  end
  else
  begin
    if Assigned(FAdvColGrid) then
    begin
      if (FItemIndex <> Value) and (Value < FAdvColGrid.RowCount) then
      begin
        FItemIndex := Value;
        if FItemIndex >= 0 then
          Text := FAdvColGrid.Cells[FLookupColumn, FItemIndex + FAdvColGrid.FixedRows]
        else
          Text := '';
      end;
    end
    else
      FItemIndex := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SetItems(const Value: TDropDownItems);
begin
  FItems.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SetLookupColumn(const Value: Integer);
begin
  FLookupColumn := Value;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SetLookupMethod(const Value: TLookupMethod);
begin
  FLookupMethod := Value;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SetMouseActions(const Value: TMouseActions);
begin
  FMouseActions.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SetNavigation(const Value: TNavigation);
begin
  FNavigation.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SetSearchFooter(const Value: TSearchFooter);
begin
  FSearchFooter.Assign(Value);
end;

procedure TCustomAdvGridDropDown.SetSelectionColorStyle
  (const Value: TSelectionColorStyle);
begin
  case Value of
    scOffice2007:
      begin
        LineColor := $F1EDEB;
        FixedLineColor := $D1BBA4;
        SelectionColor := $5EC1F1;
      end;
    scWindowsVista:
      begin
        SelectionColor := $00EACAB6;
        // SelectionTextColor := clBlack;
        LineColor := $ECECF0;
        FixedLineColor := $D4D2D1;
      end;
    scWindows7:
      begin
        SelectionColor := $00EACAB6;
        // SelectionTextColor := clBlack;
        LineColor := $ECECF0;
        FixedLineColor := $D4D2D1;
      end;
  end;

  inherited;
end;

procedure TCustomAdvGridDropDown.SetSorting(const Value: TGridSorting);
begin
  FSorting := Value;
end;

procedure TCustomAdvGridDropDown.SetStyle(const Value: TDropDownStyle);
begin
  FStyle := Value;
  EditorEnabled := (Value = dsDropDown);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SetTextEx(const Value: string);
begin
  inherited Text := Value;
end;

procedure TCustomAdvGridDropDown.StartBlockFocus(Sender: TObject);
begin
  if not FDropDownForm.BlockActivate then
  begin
    //    FDropDownForm.BlockActivate := true;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.UpdateDropDownSize;
var
  w: Integer;
begin
  inherited;
  if DropDownAutoWidth and Assigned(FDropDownForm) then
  begin
    w := (FAdvColGrid as TAdvColGrid).GetDesiredWidth;
    FAdvColGrid.Align := alNone;
    FAdvColGrid.Width := w;
    FDropDownForm.Width := FAdvColGrid.Width + DropDownBorderWidth * 2;
    FAdvColGrid.Align := alClient;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SetLineColor(const Value: TColor);
begin
  FLineColor := Value;
end;

//------------------------------------------------------------------------------

function TCustomAdvGridDropDown.GetItemIndexFromRow(ARow: Integer): Integer;
begin
  Result := ARow;
  if Assigned(FAdvColGrid) and (ARow < FAdvColGrid.RowCount) then
  begin
    Result := Integer((FAdvColGrid as TAdvStringGrid).Objects[0, ARow]);
  end;
end;

//------------------------------------------------------------------------------

function TCustomAdvGridDropDown.GetRowFromItemIndex(AItemIndex: Integer)
  : Integer;
var
  r: Integer;
begin
  Result := ItemIndex;
  if Assigned(FAdvColGrid) and (AItemIndex >= 0) then
  begin
    for r := FAdvColGrid.FixedRows to FAdvColGrid.RowCount - 1 do
    begin
      if (AItemIndex = Integer((FAdvColGrid as TAdvStringGrid).Objects[0, r])) then
      begin
        Result := r;
        Break;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.OnColGridSelectCell
  (Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  if Assigned(FAdvColGrid) then
  begin
    FNodeCell := false;
    if (FAdvColGrid as TAdvColGrid).IsNode(ARow) and (ACol = 0) then
    begin
      FNodeCell := true;
      Exit;
    end;

    FSelRow := ARow;

    //if not(FAdvColGrid as TAdvColGrid).FMouseWheelSelection then
    begin
      if UseItems then
        ItemIndex := GetItemIndexFromRow(ARow)
      else
      begin
        Text := FAdvColGrid.Cells[LookupColumn, ARow];
        ItemIndex := ARow - FAdvColGrid.FixedRows;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.OnDropDownControlKeyDown
  (var Key: Word; Shift: TShiftState);
begin
  if not(Key in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT, VK_HOME, VK_END]) or
    (ssAlt in Shift) then
    inherited;

  if Assigned(OnGridKeyDown) then
    OnGridKeyDown(Self, Key, Shift);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.OnDropDownControlKeyPress(var Key: char);
var
  IsAlt: Boolean;
  r: Integer;
begin
  IsAlt := GetKeyState(VK_MENU) and $8000 = $8000;

  if (not(Integer(Key) in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT, VK_HOME, VK_END,
      VK_RETURN, VK_ESCAPE]) or IsAlt) and
    (not(goEditing in FAdvColGrid.Options)) then
  begin
    r := DoKeyLookup(Key);

    if UseItems then
      r := r + FAdvColGrid.FixedRows;

    if (r <> -1) and (r < FAdvColGrid.RowCount) and (r >= FAdvColGrid.FixedRows) then
    begin
      if (FAdvColGrid.RowCount > FAdvColGrid.VisibleRowCount + FAdvColGrid.FixedRows) then
        FAdvColGrid.TopRow := r;

      FAdvColGrid.Row := r;
      FAdvColGrid.Col := 0;
      (FAdvColGrid as TAdvColGrid).RepaintRow(r);
    end
    else
    begin
      //FAdvColGrid.TopRow := FAdvColGrid.FixedRows;
      //FAdvColGrid.Row := FAdvColGrid.FixedRows;
      FLookupEntry := '';
    end;

    //inherited;
  end
  else
  begin
    FLookupEntry := '';
  end;

  if Assigned(OnGridKeyPress) then
    OnGridKeyPress(Self, Key);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.OnDropDownControlKeyUp
  (var Key: Word; Shift: TShiftState);
begin
  if not(Key in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT, VK_HOME, VK_END]) or
    (ssAlt in Shift) then
    inherited
  else
  begin
    FLookupEntry := '';
  end;

  if Assigned(OnGridKeyUp) then
    OnGridKeyUp(Self, Key, Shift);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.SyncColWidths;
var
  r, d: double;
  tw, aw: Integer;
  i: Integer;
begin
  if not FColumnSizeWithDropDown then
    Exit;

  if not Assigned(FAdvColGrid) then
    Exit;

  FAdvColGrid.ScrollBars := ssVertical;

  if Columns.Count > 0 then
  begin
    tw := 1;
    for i := 0 to Columns.Count - 1 do
    begin
      tw := tw + Columns[i].Width;
    end;

    // calculate ratio to size
    if FAdvColGrid.VisibleRowCount < FAdvColGrid.RowCount -
      FAdvColGrid.FixedRows then
    begin
      aw := FDropDownForm.Width - GetSystemMetrics(SM_CXVSCROLL);
    end
    else
      aw := FDropDownForm.Width;

    r := aw / tw;

    d := 0;

    for i := 0 to FAdvColGrid.ColCount - 1 do
    begin
      d := d + (r * Columns[i].Width) - Round(r * Columns[i].Width);

      if (d > 1) then
      begin
        FAdvColGrid.ColWidths[i] := Round(r * Columns[i].Width) + 1;
        d := d - 1;
      end
      else
        FAdvColGrid.ColWidths[i] := Round(r * Columns[i].Width);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.OnDropDownSizing;
begin
  inherited;
  // size columns proportionally
  SyncColWidths;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.OnGridAfterSort
  (Sender: TObject; ACol: Integer);
var
  r: Integer;
begin
  if Assigned(FAdvColGrid) then
  begin
    r := GetRowFromItemIndex(FTempItemIndex);
    if (r >= 0) then
    begin
      FAdvColGrid.Row := r;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridAnchorClick
  (Sender: TObject; ARow, ACol: Integer; Anchor: string;
  var AutoHandle: Boolean);
begin
  FDropDownForm.BlockActivate := true;
  if Assigned(FOnAnchorClick) then
    FOnAnchorClick(Sender, ARow, ACol, Anchor, AutoHandle);
  FDropDownForm.BlockActivate := False;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridAnchorEnter
  (Sender: TObject; ARow, ACol: Integer; Anchor: string);
begin
  if Assigned(FOnAnchorEnter) then
    FOnAnchorEnter(Sender, ARow, ACol, Anchor);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridAnchorExit
  (Sender: TObject; ARow, ACol: Integer; Anchor: string);
begin
  if Assigned(FOnAnchorExit) then
    FOnAnchorExit(Sender, ARow, ACol, Anchor);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridAnchorHint
  (Sender: TObject; ARow, ACol: Integer; var Anchor: string);
begin
  if Assigned(FOnAnchorHint) then
    FOnAnchorHint(Sender, ARow, ACol, Anchor);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridAutoAddRow
  (Sender: TObject; ARow: Integer);
begin
  if Assigned(FOnAutoAddRow) then
    FOnAutoAddRow(Sender, ARow);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridAutoDeleteRow
  (Sender: TObject; ARow: Integer);
begin
  if Assigned(FOnAutoDeleteRow) then
    FOnAutoDeleteRow(Sender, ARow);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridAutoInsertCol
  (Sender: TObject; ACol: Integer);
begin
  if Assigned(FOnAutoInsertCol) then
    FOnAutoInsertCol(Sender, ACol);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridAutoInsertRow
  (Sender: TObject; ARow: Integer);
begin
  if Assigned(FOnAutoInsertRow) then
    FOnAutoInsertRow(Sender, ARow);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridButtonClick
  (Sender: TObject; ACol, ARow: Integer);
begin
  FDropDownForm.BlockActivate := true;
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Sender, ACol, ARow);
  FDropDownForm.BlockActivate := False;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridCanAddRow
  (Sender: TObject; var CanAdd: Boolean);
begin
  if Assigned(FOnCanAddRow) then
    FOnCanAddRow(Sender, CanAdd);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridCanDeleteRow
  (Sender: TObject; ARow: Integer; var CanDelete: Boolean);
begin
  if Assigned(FOnCanDeleteRow) then
    FOnCanDeleteRow(Sender, ARow, CanDelete);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridCanEditCell
  (Sender: TObject; ARow, ACol: Integer; var CanEdit: Boolean);
begin
  if Assigned(FOnCanEditCell) then
    FOnCanEditCell(Sender, ARow, ACol, CanEdit);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridCanInsertRow
  (Sender: TObject; ARow: Integer; var CanInsert: Boolean);
begin
  if Assigned(FOnCanInsertRow) then
    FOnCanInsertRow(Sender, ARow, CanInsert);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridCanSort(Sender: TObject; ACol: Integer;
  var DoSort: Boolean);
begin
  if Assigned(FOnCanSort) then
    FOnCanSort(Sender, ACol, DoSort);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridCellBalloon
  (Sender: TObject; ACol, ARow: Integer; var ATitle, AText: string;
  var AIcon: Integer);
begin
  if Assigned(FOnCellBalloon) then
    FOnCellBalloon(Sender, ACol, ARow, ATitle, AText, AIcon);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridCheckBoxClick
  (Sender: TObject; ACol, ARow: Integer; State: Boolean);
begin
  if Assigned(FOnCheckBoxClick) then
    FOnCheckBoxClick(Sender, ACol, ARow, State);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridClickSort
  (Sender: TObject; ACol: Integer);
begin
  if Assigned(FOnClickSort) then
    FOnClickSort(Sender, ACol);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridColumnMove
  (Sender: TObject; ACol: Integer; var Allow: Boolean);
begin
  if Assigned(FOnColumnMove) then
    FOnColumnMove(Sender, ACol, Allow);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridColumnMoved
  (Sender: TObject; FromIndex, ToIndex: Integer);
begin
  if Assigned(FOnColumnMoved) then
    FOnColumnMoved(Sender, FromIndex, ToIndex);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridColumnMoving
  (Sender: TObject; ACol: Integer; var Allow: Boolean);
begin
  if Assigned(FOnColumnMoving) then
    FOnColumnMoving(Sender, ACol, Allow);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridColumnSize
  (Sender: TObject; ACol: Integer; var Allow: Boolean);
begin
  if Assigned(FOnColumnSize) then
    FOnColumnSize(Sender, ACol, Allow);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridColumnSizing
  (Sender: TObject; ACol, ColumnSize: Integer);
begin
  if Assigned(FOnColumnSizing) then
    FOnColumnSizing(Sender, ACol, ColumnSize);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridCustomCellBkgDraw
  (Sender: TObject; Canvas: TCanvas; ACol, ARow: Integer;
  AState: TGridDrawState; ARect: TRect; Printing: Boolean);
begin
  if Assigned(FOnCustomCellBkgDraw) then
    FOnCustomCellBkgDraw(Sender, Canvas, ACol, ARow, AState, ARect, Printing);
end;

procedure TCustomAdvGridDropDown.DoGridCustomCellDraw
  (Sender: TObject; Canvas: TCanvas; ACol, ARow: Integer;
  AState: TGridDrawState; ARect: TRect; Printing: Boolean);
begin
  if Assigned(FOnCustomCellDraw) then
    FOnCustomCellDraw(Sender, Canvas, ACol, ARow, AState, ARect, Printing);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridCustomCompare
  (Sender: TObject; str1, str2: string; var Res: Integer);
begin
  if Assigned(FOnCustomCompare) then
    FOnCustomCompare(Sender, str1, str2, Res);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridEllipsClick
  (Sender: TObject; ACol, ARow: Integer; var S: string);
begin
  FDropDownForm.BlockActivate := true;
  if Assigned(FOnEllipsClick) then
    FOnEllipsClick(Sender, ACol, ARow, S);
  FDropDownForm.BlockActivate := False;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridEndColumnSize
  (Sender: TObject; ACol: Integer);
begin
  if Assigned(FOnEndColumnSize) then
    FOnEndColumnSize(Sender, ACol);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridEndRowSize
  (Sender: TObject; ARow: Integer);
begin
  if Assigned(FOnEndRowSize) then
    FOnEndRowSize(Sender, ARow);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridGetAlignment
  (Sender: TObject; ARow, ACol: Integer; var HAlign: TAlignment;
  var VAlign: TVAlignment);
begin
  if ACol < Columns.Count then
    HAlign := Columns[ACol].Alignment;
  if Assigned(FOnGetAlignment) then
    FOnGetAlignment(Sender, ARow, ACol, HAlign, VAlign);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridGetCellColor
  (Sender: TObject; ARow, ACol: Integer; AState: TGridDrawState;
  ABrush: TBrush; AFont: TFont);
begin
  if (ACol < Columns.Count) then
  begin
    ABrush.Color := Columns[ACol].Color;
    AFont.Assign(Columns[Acol].Font);
  end;
  if Assigned(FOnGetCellColor) then
    FOnGetCellColor(Sender, ARow, ACol, AState, ABrush, AFont);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridGetCellGradient
  (Sender: TObject; ARow, ACol: Integer; var Color, ColorTo, ColorMirror,
  ColorMirrorTo: TColor; var GD: TCellGradientDirection);
begin
  if Assigned(FOnGetCellGradient) then
    FOnGetCellGradient(Sender, ARow, ACol, Color, ColorTo, ColorMirror,
      ColorMirrorTo, GD);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridGetDisplText
  (Sender: TObject; ACol, ARow: Integer; var Value: string);
begin
  if Assigned(FOnGetDisplText) then
    FOnGetDisplText(Sender, ACol, ARow, Value);
end;

procedure TCustomAdvGridDropDown.DoGridGetEditorProp
  (Sender: TObject; ACol, ARow: Integer);
begin
  if Assigned(FOnGetEditorProp) then
    FOnGetEditorProp(Sender, ACol, ARow);
end;

procedure TCustomAdvGridDropDown.DoGridGetEditorType
  (Sender: TObject; ACol, ARow: Integer; var AEditor: TEditorType);
begin
  if Assigned(FOnGetEditorType) then
    FOnGetEditorType(Sender, ACol, ARow, AEditor);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridGetFloatFormat
  (Sender: TObject; ACol, ARow: Integer; var IsFloat: Boolean;
  var FloatFormat: string);
begin
  FloatFormat := '';
  if Assigned(FOnGetFloatFormat) then
    FOnGetFloatFormat(Sender, ACol, ARow, IsFloat, FloatFormat);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridGetFormat
  (Sender: TObject; ACol: Integer; var AStyle: TSortStyle; var aPrefix,
  aSuffix: string);
begin
  if Assigned(FOnGetFormat) then
    FOnGetFormat(Sender, ACol, AStyle, aPrefix, aSuffix);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridHint
  (Sender: TObject; ARow, ACol: Integer; var hintstr: string);
begin
  if Assigned(FOnGridHint) then
    FOnGridHint(Sender, ARow, ACol, hintstr);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Sender, Key, Shift);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridKeyPress(Sender: TObject; var Key: char);
begin
  if Assigned(FOnKeyPress) then
    FOnKeyPress(Sender, Key);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Sender, Key, Shift);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Sender, Button, Shift, X, Y);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridMouseMove
  (Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if not FBalloonInit then
  begin
    if (FAdvColGrid as TAdvColGrid).Balloon.Enable then
    begin
      (FAdvColGrid as TAdvColGrid).Balloon.Enable := False;
      (FAdvColGrid as TAdvColGrid).Balloon.Enable := true;
    end;
    FBalloonInit := true;
  end;

  if Assigned(FOnMouseMove) then
    FOnMouseMove(Sender, Shift, X, Y);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Sender, Button, Shift, X, Y);
end;

//------------------------------------------------------------------------------
{$IFDEF TMSGDIPLUS}
procedure TCustomAdvGridDropDown.DoGridOfficeHint
  (Sender: TObject; ACol, ARow: Integer; OfficeHint: TAdvHintInfo);
begin
  if Assigned(FOnOfficeHint) then
    FOnOfficeHint(Sender, ACol, ARow, OfficeHint);
end;
{$ENDIF}
//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridRawCompare
  (Sender: TObject; ACol, Row1, Row2: Integer; var Res: Integer);
begin
  if Assigned(FOnRawCompare) then
    FOnRawCompare(Sender, ACol, Row1, Row2, Res);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridRightClickCell
  (Sender: TObject; ARow, ACol: Integer);
begin
  if Assigned(FOnRightClickCell) then
    FOnRightClickCell(Sender, ARow, ACol);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridRowMove(Sender: TObject; ARow: Integer;
  var Allow: Boolean);
begin
  if Assigned(FOnRowMove) then
    FOnRowMove(Sender, ARow, Allow);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridRowMoved(Sender: TObject;
  FromIndex, ToIndex: Integer);
begin
  if Assigned(FOnRowMoved) then
    FOnRowMoved(Sender, FromIndex, ToIndex);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridRowMoving
  (Sender: TObject; ARow: Integer; var Allow: Boolean);
begin
  if Assigned(FOnRowMoving) then
    FOnRowMoving(Sender, ARow, Allow);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridRowSize(Sender: TObject; ARow: Integer;
  var Allow: Boolean);
begin
  if Assigned(FOnRowSize) then
    FOnRowSize(Sender, ARow, Allow);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridRowSizing
  (Sender: TObject; ARow, RowHeight: Integer);
begin
  if Assigned(FOnRowSizing) then
    FOnRowSizing(Sender, ARow, RowHeight);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridScrollHint
  (Sender: TObject; ARow: Integer; var hintstr: string);
begin
  if Assigned(FOnScrollHint) then
    FOnScrollHint(Sender, ARow, hintstr);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridSearchEditChange
  (Sender: TObject; Value: string; var DefaultSearch: Boolean);
begin
  if Assigned(FOnSearchEditChange) then
    FOnSearchEditChange(Sender, Value, DefaultSearch);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridSpinClick
  (Sender: TObject; ACol, ARow, AValue: Integer; UpDown: Boolean);
begin
  if Assigned(FOnSpinClick) then
    FOnSpinClick(Sender, ACol, ARow, AValue, UpDown);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.DoGridTopLeftChanged(Sender: TObject);
begin
  if Assigned(FOnTopLeftChanged) then
    FOnTopLeftChanged(Self);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.OnGridBeforeSort
  (Sender: TObject; ACol: Integer; var DoSort: Boolean);
begin
  FTempItemIndex := ItemIndex;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.OnColGridSelect(Sender: TObject);
begin
  if Assigned(FAdvColGrid) then
  begin
    if FNodeCell then
      Exit;

    if (FAdvColGrid as TAdvColGrid).FConfirmSelection then
    begin
      if UseItems then
        ItemIndex := GetItemIndexFromRow(FAdvColGrid.Row)
      else
        ItemIndex := FAdvColGrid.Row - 1;
    end;

    if not GridEditable then
      DoHideDropDown(False);

    if Assigned(OnSelect) then
      OnSelect(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.KeyDown(var Key: Word; Shift: TShiftState);
begin
  // return key during edit
  if (goEditing in FAdvColGrid.Options) and
    ((FAdvColGrid as TAdvColGrid).EditMode) and (Key = VK_RETURN) and not
    (ssShift in Shift) then
  begin
    (FAdvColGrid as TAdvColGrid).HideInplaceEdit;
    Text := FAdvColGrid.Cells[FLookupColumn, FAdvColGrid.Row];
    ItemIndex := FAdvColGrid.Row - FAdvColGrid.FixedRows;
    Key := 0;
    Exit;
  end;

  if (FLookupColumn < 0) or (Items.Count = 0) then
  begin
    inherited;
    Exit;
  end;

  case Key of
    VK_ESCAPE, VK_BACK, VK_DELETE:
      workmode := False;
    VK_RETURN:
      begin
        if (FLookupItems.IndexOf(Text) <> -1) then
        begin
          Text := FLookupItems.Strings[FLookupItems.IndexOf(Text)];
          ItemIndex := FLookupItems.IndexOf(Text);
          Change;
        end;

        if FReturnIsTab then
        begin
          PostMessage(Handle, WM_KEYDOWN, VK_TAB, 0);
          Key := 0;
        end;
      end;
  else
    workmode := true;
  end;

  // reset lookup
  if (Key in [VK_ESCAPE, VK_UP, VK_DOWN, VK_HOME, VK_END, VK_LEFT, VK_RIGHT,
    VK_PRIOR, VK_NEXT]) then
  begin
    FLookupEntry := '';
  end;

  inherited KeyDown(Key, Shift);

  if (Key = VK_ESCAPE) then
    ItemIndex := FLookupItems.IndexOf(Text);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.KeyPress(var Key: char);
begin
  if (Key = #13) and FReturnIsTab then
    Key := #0
  else
  begin
    inherited KeyPress(Key);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if not((Key = VK_RETURN) and (FReturnIsTab)) then
    inherited KeyUp(Key, Shift);

  if (goEditing in FAdvColGrid.Options) and
    ((FAdvColGrid as TAdvColGrid).EditMode) and (Key = VK_RETURN) and not
    (ssShift in Shift) then
  begin
    if (FAdvColGrid.Col = FLookupColumn) then
      HideDropDown(False)
    else
     (FAdvColGrid as TAdvColGrid).HideInplaceEdit;
  end;

  if FItemChange then
  begin
    ItemIndex := FItemIdx;
    SendMessage(Handle, CB_SETEDITSEL, 0, MakeLong(FItemSel, length(Text)));
    FItemChange := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.UpdateLookupList;
var
  i: Integer;
begin
  if (FLookupColumn < 0) or (Items.Count = 0) then
    Exit;

  FLookupItems.Clear;

  for i := 0 to Items.Count - 1 do
  begin
    if (FLookupColumn < Items[i].Text.Count) then
      FLookupItems.Add(Items[i].Text[FLookupColumn])
    else
      FLookupItems.Add('');
  end;
end;

//------------------------------------------------------------------------------

function TCustomAdvGridDropDown.DoKeyLookup(ch: char): Integer;
var
  S, Res: string;
  Row: Integer;
begin
  Result := -1;

  if FLookupMethod = lmNone then
    Exit;

  if (ch <> #8) and (ch <> #9) then
  begin
    FLookupEntry := FLookupEntry + ch;
  end
  else
  begin
    if (ch = #8) then
    begin
      Res := Text;
      if (SelLength > 0) then
      begin
        Delete(Res, SelStart, SelLength);
        Text := Res;
        SelStart := Length(Text);
      end
      else
      begin
        Delete(FLookupEntry, length(FLookupEntry), 1);
        Text := FLookupEntry;
        SelStart := Length(FLookupEntry);
      end;

      Exit;
    end;
  end;

  S := Trim(FLookupEntry);

  if FLookupMethod = lmNarrowDown then
  begin
    if (S = '') then
      (FAdvColGrid as TAdvColGrid).RemoveAllFilters
    else
    begin
      (FAdvColGrid as TAdvColGrid).FixedRowAlways := true;
      (FAdvColGrid as TAdvColGrid).NarrowDown(S, FLookupColumn);
      if (Style = dsDropDown) then
      begin
        if FAdvColGrid.RowCount > FAdvColGrid.FixedRows then
        begin
          Res := FAdvColGrid.Cells[FLookupColumn, FAdvColGrid.FixedRows];
          Text := Res;
          SelStart := length(FLookupEntry);
          SelLength := length(Res) - length(FLookupEntry);
        end;
      end;
    end;

    Result := FAdvColGrid.FixedRows;
  end
  else
  begin
    if LookupInColumn(S, Res, Row) then
    begin
      FItemIndex := Row;
      FSelRow := Row;
      Text := Res;
      Result := Row;
      if (Style = dsDropDown) then
      begin
        SelStart := length(FLookupEntry);
        SelLength := length(Res) - length(FLookupEntry);
      end;
    end
    else
    begin
      if Style = dsDropDownList then
      begin
        S := ch;
        if LookupInColumn(S, Res, Row) then
        begin
          FItemIndex := Row;
          Text := Res;
          Result := Row;
          FLookupEntry := S
        end
        else
          FLookupEntry := '';
      end;
    end;
  end;
end;

procedure TCustomAdvGridDropDown.WMChar(var Msg: TWMKey);
//var
//  fs: integer;
begin
  inherited;

  DoKeyLookup(chr(Msg.CharCode));

  //fs := FSelRow;

  if AutoShowDropDown then
  begin
    ShowDropDown;
    //FSelRow := fs;
  end;
end;

procedure TCustomAdvGridDropDown.WMKeyDown(var Msg: TWMKeydown);
var
  IsAlt, IsShift: Boolean;
begin
  if Msg.CharCode = VK_RETURN then
    (FAdvColGrid as TAdvColGrid).FConfirmSelection := true;

  inherited;

  IsAlt := (GetKeyState(VK_MENU) and $8000 = $8000);
  IsShift := (GetKeyState(VK_SHIFT) and $8000 = $8000);

  if Enabled and not IsAlt then
  begin
    if (Msg.CharCode in [VK_UP, VK_DOWN, VK_HOME, VK_NEXT, VK_PRIOR, VK_END])
      then
      FLookupEntry := '';

    case Msg.CharCode of
      VK_UP:
        begin
          SelectPrevious;
        end;
      VK_DOWN:
        begin
          SelectNext;
        end;
      VK_HOME:
        begin
          if not IsShift and (Style = dsDropDownList) then
            SelectFirst;
        end;
      VK_END:
        begin
          if not IsShift and (Style = dsDropDownList) then
            SelectLast;
        end;
      VK_PRIOR:
        begin
          SelectPrevPage;
        end;
      VK_NEXT:
        begin
          SelectNextPage;
        end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.Loaded;
begin
  inherited;
  UpdateLookupList;
  if Assigned(FAdvColGrid) then
    (FAdvColGrid as TAdvColGrid).Populate;
end;

function TCustomAdvGridDropDown.LookupInColumn
  (Value: string; var lookup: string; var Row: Integer): Boolean;
var
  i: Integer;
  S: string;
begin
  Result := False;

  Row := -1;
  lookup := '';

  if not FCaseSensitive then
    Value := AnsiUppercase(Value);

  if UseItems then
  begin
    for i := 0 to Items.Count - 1 do
    begin
      if FCaseSensitive then
        S := Items[i].Text.Strings[FLookupColumn]
      else
        S := AnsiUppercase(Items[i].Text.Strings[FLookupColumn]);

      if pos(Value, S) = 1 then
      begin
        lookup := Items[i].Text.Strings[FLookupColumn];
        Row := i;
        Result := true;
        Break;
      end;
    end;
  end
  else
  begin
    if Assigned(FAdvColGrid) then
    begin
      for i := FAdvColGrid.FixedRows to FAdvColGrid.RowCount - 1 do
      begin
        if FCaseSensitive then
          S := FAdvColGrid.Cells[FLookupColumn, i]
        else
          S := AnsiUppercase(FAdvColGrid.Cells[FLookupColumn, i]);

        if pos(Value, S) = 1 then
        begin
          lookup := FAdvColGrid.Cells[FLookupColumn, i];
          Row := i;
          Result := true;
          Break;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function upstr(S: string; docase: Boolean): string;
begin
  if docase then
    Result := S
  else
    Result := AnsiUppercase(S);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.BeginUpdate;
begin
  //
end;

procedure TCustomAdvGridDropDown.Change;
var
  c, c1, NewT: string;
  i: Integer;
  UsrStr, AutoAdd: string;
  ni: boolean;
begin
  FItemChange := False;

  inherited Change;

  if csDesigning in ComponentState then
    Exit;

  if not workmode then
    Exit;

  // WorkMode := False;

  if Text <> FOldValue then
    Modified := true
  else
    Modified := False;

  NewT := Copy(Text, 1, SelStart);
  c1 := upstr(Text, FMatchCase);
  c := Copy(c1, 1, SelStart);

  if (FLookupItems.Count > 0) then
  begin
    for i := 0 to FLookupItems.Count - 1 do
    begin
      if pos(c, upstr(FLookupItems.Strings[i], FMatchCase)) = 1 then
      begin
        UsrStr := Copy(Text, 1, length(c));
        AutoAdd := Copy(FLookupItems.Strings[i], length(c) + 1, 255);

        // if Assigned(FAutoComplete) then
        // FAutoComplete(self, UsrStr, AutoAdd, i);

        ni := FItemIndex <> i;

        FItemIndex := i;
        FItemIdx := i;
        FItemSel := length(c);
        FItemChange := true;
        Text := UsrStr + AutoAdd;

        // SendMessage(Handle, EM_SETSEL, length(c), length(text));

        // SendMessage(Handle, EM_SETSEL, length(c), length(text));
        SendMessage(Handle, EM_SETSEL, length(Text), length(c));
        // SendMessage(Handle, CB_SETEDITSEL, 0, makelong(FItemSel, length(text)));

        if Assigned(OnChange) and ni then
          OnChange(Self);

        Exit;
      end;
    end;

    if (NewT <> '') then
    begin
      Text := NewT;
      SendMessage(Handle, EM_SETSEL, length(Text), length(NewT));
    end;
  end;
end;

//------------------------------------------------------------------------------

function TCustomAdvGridDropDown.CreateItems(AOwner: TComponent): TDropDownItems;
begin
  Result := TDropDownItems.Create(AOwner);
end;


//------------------------------------------------------------------------------

procedure TCustomAdvGridDropDown.OnColGridKeyDown
  (Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      DoHideDropDown(true);
    VK_RETURN:
      DoHideDropDown(False);
  end;
end;

//------------------------------------------------------------------------------

{ TDropDownItem }

procedure TDropDownItem.Assign(Source: TPersistent);
begin
  if (Source is TDropDownItem) then
  begin
    ImageIndex := (Source as TDropDownItem).ImageIndex;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

constructor TDropDownItem.Create(Collection: TCollection);
begin
  inherited;
  FText := TStringList.Create;
  FImageIndex := -1;
end;

//------------------------------------------------------------------------------

destructor TDropDownItem.Destroy;
begin
  FText.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDropDownItem.SetText(const Value: TStringList);
begin
  FText.Assign(Value);
end;

//------------------------------------------------------------------------------

{ TDropDownItems }

function TDropDownItems.Add: TDropDownItem;
begin
  Result := TDropDownItem( inherited Add);
end;

//------------------------------------------------------------------------------

constructor TDropDownItems.Create(AOwner: TPersistent);
begin
  inherited Create(GetItemClass);
  FMyOwner := AOwner;
end;

//------------------------------------------------------------------------------

function TDropDownItems.GetItem(Index: Integer): TDropDownItem;
begin
  Result := TDropDownItem( inherited Items[Index]);
end;

function TDropDownItems.GetItemClass: TDropDownItemClass;
begin
  Result := TDropDownItem;
end;

//------------------------------------------------------------------------------

function TDropDownItems.GetOwner: TPersistent;
begin
  Result := FMyOwner;
end;

//------------------------------------------------------------------------------

function TDropDownItems.Insert(Index: Integer): TDropDownItem;
begin
  Result := TDropDownItem( inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TDropDownItems.SetItem(Index: Integer; const Value: TDropDownItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TAdvColGrid }

constructor TAdvColGrid.Create(AOwner: TComponent);
begin
  inherited;
  FColumns := TDropDownColumns.Create(Self);
  FHeaderColorTo := clGray;
  FHeaderColor := clWhite;
  FLineColor := clGray;
  FixedRowAlways := true;
  FixedRows := 1;
  FixedCols := 0;

  FHeaderFont := TFont.Create;
  FHeaderHeight := 25;
  Options := Options + [goRowSelect, goHorzLine] - [
  { goVertLine,  goDrawFocusSelected,} goRangeSelect];
  //DefaultDrawing := False;
  ParentCtl3D := False;
  Ctl3D := False;
  //GridLineWidth := 0;
  BorderStyle := bsNone;
  FGridLineWidth := 1;
  FOldRow := Row;
end;

//------------------------------------------------------------------------------

destructor TAdvColGrid.Destroy;
begin
  FColumns.Free;
  FHeaderFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.WMKeyDown(var Msg: TWMKeydown);
begin
  if (Msg.CharCode = VK_TAB) then
  begin
    // if Assigned(AdvDropDown) and IsWindowVisible(AdvDropDown.Handle) then
    // PostMessage(AdvDropDown.Handle, WM_KEYDOWN, VK_TAB, 0);
    Exit;
  end;

  if (Msg.CharCode = VK_ESCAPE) or (Msg.CharCode = VK_F4) or
    (((Msg.CharCode = VK_UP) or (Msg.CharCode = VK_DOWN)) and
      (GetKeyState(VK_MENU) and $8000 = $8000)) then
  begin
    // if (msg.Charcode = VK_ESCAPE) then
    // ParentEdit.CancelChanges;
    // PostMessage((Parent as TForm).Handle,WM_CLOSE,0,0);
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FConfirmSelection := true;

  if ((Key in [VK_UP, VK_DOWN]) and (GetKeyState(VK_MENU) and $8000 = $8000))
    then
  begin
    if Assigned(FAdvDropDown) then
      SendMessage(FAdvDropDown.Handle, WM_KEYDOWN, Key, 0);
    Exit;
  end;

  if (Key = VK_RETURN) then
  begin
    Key := 0;
    inherited;
    DoSelectionChanged;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

function TAdvColGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint)
  : Boolean;
begin
  FMouseWheelSelection := true;
  FConfirmSelection := False;
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  FMouseWheelSelection := False;
end;

//------------------------------------------------------------------------------

function TAdvColGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint)
  : Boolean;
begin
  FMouseWheelSelection := true;
  FConfirmSelection := False;
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  FMouseWheelSelection := False;
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.WMChar(var Msg: TWMChar);
begin
  if Msg.CharCode = Ord(#13) then
  begin
    Msg.Result := 1;
    Exit;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTTAB or DLGC_WANTARROWS;
end;

//------------------------------------------------------------------------------

function TAdvColGrid.GetDesiredWidth: Integer;
var
  i: Integer;
begin
  Result := BorderWidth * 2;
  for i := 0 to ColCount - 1 do
    Result := Result + ColWidths[i];

  if (VisibleRowCount + FixedRows <> RowCount) then
    Result := Result + GetSystemMetrics(SM_CXVSCROLL);
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.AutoSizeColumnInt(ACol: Integer);
var
  S: string;
  r, ImgIndx, cw, w: Integer;
  Rt: TRect;
begin
  if (ACol >= 0) and (ACol < Columns.Count) and (Columns[ACol].AutoSize)
    and Assigned(Parent) and (Parent is TDropDownForm) then
  begin
    TDropDownForm(Parent).Canvas.Font.Assign(Columns[ACol].Font);
    ImgIndx := -1;

    w := 0;

    for r := 1 to RowCount - 1 do
    begin
      cw := 0;
      if Assigned(AdvDropDown) then
      begin
        if (ACol < AdvDropDown.Items[r - 1].Text.Count) then
        begin
          S := AdvDropDown.Items[r - 1].Text[ACol];
          if (Columns[ACol].ColumnType = ctImage) then
            ImgIndx := AdvDropDown.Items[r - 1].ImageIndex;
        end
        else
          S := '';
      end
      else
      begin
        S := Cells[ACol, r];
        ImgIndx := -1;
      end;

      if (ImgIndx >= 0) and Assigned(Images) then
      begin
        cw := cw + Images.Width + 4;
      end;

      if (S <> '') then
      begin // using Parent's canvas to control the abnormal behavior of grid
        Rt := Rect(0, 0, 1000, 500);
        DrawText(TDropDownForm(Parent).Canvas.Handle, PChar(S), length(S), Rt,
          DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
        cw := cw + Rt.Right + 6;
      end;

      w := Max(w, cw);
    end;

    Columns[ACol].Width := w;
    ColWidths[ACol] := w;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.DoWordWrap;
var
  S: string;
  r, c, ImgIndx, ch, h: Integer;
  Rt: TRect;
  ww: Boolean;
begin
  ww := False;
  for c := 0 to Columns.Count - 1 do
  begin
    if Columns[c].Wordwrap then
    begin
      ww := true;
      Break;
    end;
  end;

  if not ww or not Assigned(Parent) or not(Parent is TDropDownForm) then
    // no word wrap required
    Exit;

  for r := FixedRows to RowCount - 1 do
  begin
    h := 24;
    for c := 0 to ColCount - 1 do
    begin
      ch := 0;
      if Columns[c].Wordwrap then
      begin
        ImgIndx := -1;
        if Assigned(AdvDropDown) then
        begin
          if (c < AdvDropDown.Items[r - 1].Text.Count) then
          begin
            S := AdvDropDown.Items[r - 1].Text[c];
            if (Columns[c].ColumnType = ctImage) then
              ImgIndx := AdvDropDown.Items[r - 1].ImageIndex;
          end
          else
            S := '';
        end
        else
        begin
          S := Cells[c, r - 1];
          ImgIndx := -1;
        end;

        if (ImgIndx >= 0) and Assigned(Images) then
        begin
          ch := ch + Images.Height + 6;
        end;

        if (S <> '') then
        begin // using Parent's canvas to control the abnormal behavior of grid
          TDropDownForm(Parent).Canvas.Font.Assign(Columns[c].Font);
          Rt := Rect(0, 0, ColWidths[c], 500);
          DrawText(TDropDownForm(Parent).Canvas.Handle, PChar(S), length(S),
            Rt, DT_CALCRECT or DT_LEFT or DT_Top or DT_WORDBREAK);
          ch := Max(ch, Rt.Bottom + 6);
        end;
        h := Max(h, ch);
      end;
    end;

    RowHeights[r] := h;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.AutoSizeColumnsInt;
var
  i: Integer;
begin
  for i := 0 to Columns.Count - 1 do
  begin
    if Columns[i].AutoSize then
    begin
      AutoSizeColumnInt(i);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.Populate;
var
  r, c: Integer;
  cs: string;
begin
  if Assigned(AdvDropDown) then
  begin
    if (ColCount <> Columns.Count) then
      ColCount := Columns.Count;

    for c := 0 to Columns.Count - 1 do
    begin
      cs := '';
      if (c < Columns.Count) then
        cs := Columns[c].Header;

      Cells[c, 0] := cs;
      Objects[c, 0] := Pointer(-1);
    end;

   if AdvDropDown.Items.Count > 0 then
     RowCount := AdvDropDown.Items.Count + FixedRows;

    for r := 0 to AdvDropDown.Items.Count - 1 do
    begin
      for c := 0 to Columns.Count - 1 do
      begin
        cs := '';
        if (c < AdvDropDown.Items[r].Text.Count) then
          cs := AdvDropDown.Items[r].Text[c];
        Cells[c, r + 1] := cs;
      end;
      Objects[0, r + 1] := TObject(r);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.Initialize(DropDown: TCustomAdvGridDropDown);
var
  i: Integer;

begin
  if Assigned(DropDown) then
  begin
    AdvDropDown := DropDown;

    Columns.Assign(AdvDropDown.Columns);

    if ColCount < Columns.Count then
      ColCount := Columns.Count;

    if DropDown.UseItems then
      RowCount := AdvDropDown.Items.Count + 1;

    for i := 0 to AdvDropDown.Columns.Count - 1 do
    begin
      ColWidths[i] := Columns[i].Width;
    end;

    LineColor := AdvDropDown.LineColor;
    FixedLineColor := AdvDropDown.FixedLineColor;
    HeaderColor := AdvDropDown.HeaderColor;
    HeaderColorTo := AdvDropDown.HeaderColorTo;
    HeaderHeight := AdvDropDown.HeaderHeight;
    HeaderFont.Assign(AdvDropDown.HeaderFont);
    HoverRow := AdvDropDown.HoverRow;
    HoverRowColor := AdvDropDown.HoverRowColor;
    HoverRowColorTo := AdvDropDown.HoverRowColorTo;

    Images := AdvDropDown.Images;
    DefaultRowHeight := AdvDropDown.DropDownRowHeight;
    RowHeights[0] := AdvDropDown.HeaderHeight;

    if AdvDropDown.ColumnSizing then
      Options := Options + [goColSizing]
    else
      Options := Options - [goColSizing];

    if AdvDropDown.ColumnMoving then
      Options := Options + [goColMoving]
    else
      Options := Options - [goColMoving];

    if AdvDropDown.RowSizing then
      Options := Options + [goRowSizing]
    else
      Options := Options - [goRowSizing];

    if AdvDropDown.RowMoving then
      Options := Options + [goRowMoving]
    else
      Options := Options - [goRowMoving];

    if AdvDropDown.AutoSizeColumns then
    begin
      if DropDown.UseItems then
        AutoSizeColumnsInt
      else
        AutoSizeColumns(true);
    end;

    DoWordWrap;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.SetHeaderColor(const Value: TColor);
begin
  FHeaderColor := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.SetHeaderColorTo(const Value: TColor);
begin
  FHeaderColorTo := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.SetLineColor(const Value: TColor);
begin
  FLineColor := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if not(csDestroying in ComponentState) and (AOperation = opRemove) then
  begin
    if (AComponent = Images) then
      Images := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.Resize;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.SetColumns(const Value: TDropDownColumns);
begin
  FColumns.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.SetHeaderFont(const Value: TFont);
begin
  FHeaderFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.SetHeaderHeight(const Value: Integer);
begin
  FHeaderHeight := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.SetAdvDropDown(const Value: TCustomAdvGridDropDown);
begin
  FAdvDropDown := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.SetGridLineWidth(const Value: Integer);
begin
  FGridLineWidth := Value;
  inherited GridLineWidth := 0;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvColGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  Result := inherited SelectCell(ACol, ARow);
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  c, r: Longint;
begin
  inherited;

  if (Button = mbLeft) then
  begin
    MouseToCell(X, Y, c, r);

    if HasCheckBox(c,r) then
      Exit;

    if (r >= FixedRows) and (r < RowCount) then
      DoSelectionChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.DoSelectionChanged;
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvColGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FConfirmSelection := true;
  FOldRow := Row;
  inherited;
end;

//------------------------------------------------------------------------------

end.
