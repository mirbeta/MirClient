{********************************************************************}
{ TAdvDBLookupComboBox component                                     }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2002 - 2015                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the author and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit AdvDBLookupComboBox;

{$I TMSDEFS.INC}

interface

uses Windows, Classes, StdCtrls, ExtCtrls, Controls, Messages, SysUtils,
  Forms, Graphics, Buttons, Grids, DB, Dialogs, Math, ImgList
  , Variants, ALXPVS, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 9; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.2.0.1 : fixed issue with dropdown display in Delphi 2005
  // 1.2.1.0 : Added XP style dropdownbutton
  // 1.3.0.0 : Added capability to work without datasource
  //         : AutoSize capability in Columns added
  //         : Sort works on text, numeric & date values
  //         : OnDropDown, OnCloseUp events added
  //         : ReturnIsTab added
  //         : FocusColor property added
  //         : Imagelist image display support in dropdown list
  // 1.3.0.1 : Fixed issue with keylookup
  // 1.4.0.0 : New property LookupColumn added
  //         : New FixedColor, FixedColorTo added in TDBColumnItem
  //         : New ShowGridTitleRow property added
  // 1.4.0.1 : Improvement in TAdvDBLookupComboBox.Destroy
  // 1.4.0.2 : Fixed issue with AV in specific connection to non active dataset
  // 1.4.0.3 : Fixed issue with mouse selection of default selected item
  // 1.4.0.4 : Fixed issue with FilterField and empty FilterValue
  // 1.4.1.0 : Various fixes & refactoring
  // 1.5.0.0 : New DisabledColor property added
  //         : New DropDownStretchColumn added
  //         : New OnDrawProp event added
  //         : New autosizing dropdown capability (when DropWidth = 0)
  //         : New Dropdown sizing persistence
  // 1.5.0.1 : Fixed issue with calculated lookup fields
  // 1.5.1.0 : Improved : Used AnsiUpperCase instead of UpperCase
  // 1.5.2.0 : Improved : handling of Tab, Return, Esc key
  // 1.5.3.0 : Exposed BevelInner, BevelOuter, BevelKind, BevelEdges properties
  // 1.5.3.1 : Fixed : issue with handling arrow keys in dropdown grid
  // 1.5.3.2 : Fixed : disabled painting with XP themes of dropdown button

  // 1.6.0.0 : New : GridHeaderAutoSize property added
  //         : New : TitleOrientation added
  //         : New : TitleAlignment, TitleVerticalAlignment added
  //         : New : OnGridSelectCell event added
  // 1.6.0.1 : Fixed : issue with dataset Insert mode
  //         : Improved : behaviour with master/detail datasets
  // 1.6.1.0 : New : property DropDownRowCount added 
  // 1.6.1.1 : Fixed : issue with handling selection of record that is being deleted
  // 1.6.1.2 : Fixed : issue with Abort from dataset update after changing value in control
  // 1.6.1.3 : Fixed : issue in component destroy
  // 1.7.0.0 : New : grid hover row color support added
  // 1.7.0.1 : Fixed : issue with DB field update upon editing
  // 1.7.0.2 : Fixed : issue with selection in lookupcombo
  // 1.7.0.3 : Fixed : issue with dropdown window & form activation
  // 1.7.0.4 : Fixed : label not hidden when control was hidden at runtime
  // 1.7.0.5 : Fixed : issue with text value on dataset insert
  // 1.7.0.6 : Fixed : issue with Visible property
  // 1.7.0.7 : Fixed : issue when ListSource dataset changes
  // 1.8.0.0 : New : property ShowMemoFields added to show memo/blob fields
  // 1.8.0.1 : Fixed : issue with Modified flag
  //         : Fixed : issue with ESC key handling
  // 1.8.0.2 : Fixed : issue with Modified flag when editing
  // 1.8.0.3 : Fixed : issue with Modified when no DataSource is used
  // 1.8.0.4 : Fixed : issue with use / change of DropFont
  // 1.8.1.0 : New : Event OnClosed added
  // 1.8.1.1 : Fixed : Selection issue when DataSource.AutoEdit = false
  // 1.8.1.2 : Fixed : Issue with ListSource lookup after selecting same item twice
  // 1.8.1.3 : Fixed : Issue with updating attached label under specific circumstances
  // 1.8.2.0 : Improved : Keep dropdown always visible on screen, also on right side of desktop
  // 1.8.2.1 : Improved : Checks when component is programmatically destroyed
  // 1.8.2.2 : Fixed : Issue with text updating when cell is dropdown is reselected
  // 1.8.3.0 : New : LookupSearch property added to configure to perform lookup from first char or any char in fields
  // 1.8.4.0 : New : OnGridSelectedCell event added
  // 1.8.4.1 : Improved : Persistence of column widths when column sizing is allowed
  // 1.8.4.2 : Fixed : Border paint issue when ctl3d = false
  // 1.8.4.3 : Fixed : Issue with MouseUp handling for controls positioned under the TAdvDBLookupComboBox
  // 1.8.4.4 : Improved : Behavior with LookupSearch = isAnyChar
  // 1.8.5.0 : Improved : In lookup handle full match with preference to first partial match
  // 1.9.0.0 : New : Property LookupInAllColumns added

type
  TAdvDBLookupComboBox = class;

  TFindList = class(TStringlist)
  private
    BaseIndex: Integer;
    KeyField :String;
    FGrid: TAdvDBLookupComboBox;
  public
    constructor Create(Agrid:TAdvDBLookupComboBox);
    destructor Destroy; override;
  end;

  TLookupErrorEvent = procedure(Sender: TObject; LookupValue: string) of object;

  TLookupSuccessEvent = procedure(Sender: TObject; LookupValue,LookupResult: string) of object;

  TLabelPosition = (lpLeftTop,lpLeftCenter,lpLeftBottom,lpTopLeft,lpBottomLeft,
                    lpLeftTopLeft,lpLeftCenterLeft,lpLeftBottomLeft,lpTopCenter,
                    lpBottomCenter, lpRightTop,lpRightCenter,lpRightBottom);
  TGradientDirection = (gdHorizontal, gdVertical);

  TDropDownType = (ddUser,ddAuto,ddOnError);

  TSortType = (stAscendent,stDescendent);

  TTitleOrientation = (toHorizontal, toVertical);
  TVertAlignment = (tvaCenter, tvaTop, tvaBottom);

  TDBGridLookupDataLink = class(TDataLink)
  private
    FGrid: TAdvDBLookupComboBox;
  protected
    procedure Modify;
    procedure DataSetScrolled(distance:integer); override;
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure DataSetChanged; override;
  public
    constructor Create(AGrid: TAdvDBLookupComboBox);
    destructor Destroy; override;
    property Grid:TAdvDBLookupComboBox read FGrid;
  end;

  TDBGridDataLink = class(TDataLink)
  private
    FGrid:TAdvDBLookupComboBox;
    FNumberRecords:integer;
    OldState:TDataSetState;
    FLoadingData: Boolean;
    FOldRecNo: Integer;
  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure DataSetChanged; override;
  public
    constructor Create(AGrid: TAdvDBLookupComboBox);
    destructor Destroy; override;
    property Grid:TAdvDBLookupComboBox read FGrid;
  end;

  TEllipsType = (etNone, etEndEllips, etPathEllips);

  TLabelEx = class(TLabel)
  private
    FEllipsType: TEllipsType;
    procedure SetEllipsType(const Value: TEllipsType);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
  published
    { Published declarations }
    property EllipsType: TEllipsType read FEllipsType write SetEllipsType;
  end;

  TDBColumnType = (ctText,ctImage);

  TDBColumnItem = class(TCollectionItem)
  private
    FWidth: Integer;
    FAlignment: TAlignment;
    FFont: TFont;
    FColor: TColor;
    FColumnType: TDBColumnType;
    FListField: string;
    FTitle: string;
    FName: string;
    FTitleFont: TFont;
    FAutoSize: boolean;
    FFixedColor: TColor;
    FFixedColorTo: TColor;
    FGradientDir: TGradientDirection;
    FTitleOrientation: TTitleOrientation;
    FTitleAlignment: TAlignment;
    FTitleVerticalAlignment: TVertAlignment;
    FFontChanged: Boolean;
    FTitleFontChanged: Boolean;
    procedure SetWidth(const value:integer);
    procedure SetAlignment(const value:tAlignment);
    procedure SetFont(const value:TFont);
    procedure SetColor(const value:TColor);
    function  GetListField: string;
    procedure SetListField(const Value: string);
    procedure SetColumnType(const Value: TDBColumnType);
    function GetCombo: TAdvDBLookupComboBox;
    function GetName: string;
    procedure SetName(const Value: string);
    procedure SetTitleFont(const Value: TFont);
    procedure OnFontChanged(Sender: TObject);
    procedure OnTitleFontChanged(Sender: TObject);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection:TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Combo: TAdvDBLookupComboBox read GetCombo;
  published
    property AutoSize: boolean read FAutoSize write FAutoSize default false;
    property Color:TColor read fColor write SetColor default clWindow;
    property ColumnType: TDBColumnType read FColumnType write SetColumnType default ctText;
    property FixedColor: TColor read FFixedColor write FFixedColor;
    property FixedColorTo: TColor read FFixedColorTo write FFixedColorTo;
    property GradientDir: TGradientDirection read FGradientDir write FGradientDir;
    property Width:integer read fWidth write SetWidth default 100;
    property Alignment:TAlignment read fAlignment write SetAlignment default taLeftJustify;
    property Font:TFont read FFont write SetFont;
    property ListField: string read GetListField write SetListField;
    property Name: string read GetName write SetName;
    property Title: string read FTitle write FTitle;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property TitleAlignment: TAlignment read FTitleAlignment write FTitleAlignment default taCenter;
    property TitleVerticalAlignment: TVertAlignment read FTitleVerticalAlignment write FTitleVerticalAlignment default tvaCenter;
    property TitleOrientation: TTitleOrientation read FTitleOrientation write FTitleOrientation default toHorizontal;
  end;

  TDBColumnCollection = class(TCollection)
  private
    FOwner:TAdvDBLookupComboBox;
    function GetItem(Index: Integer): TDBColumnItem;
    procedure SetItem(Index: Integer; const Value: TDBColumnItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add:TDBColumnItem;
    function Insert(index:integer): TDBColumnItem;
    property Items[Index: Integer]: TDBColumnItem read GetItem write SetItem; default;
    constructor Create(AOwner: TAdvDBLookupComboBox);
    function GetOwner: TPersistent; override;
  end;

  {TDropForm}

  TDropForm = class(TForm)
  private
    FSizeable: Boolean;
    FDroppedDown: Boolean;
    FHideTimer: TTimer;
    procedure WMClose(var Msg:TMessage); message WM_CLOSE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure OnHideTimer(Sender: TObject);
  protected
    { Protected declarations }
    procedure CreateParams(var Params: TCreateParams); override;
  public  
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
  published
    property Sizeable: Boolean read FSizeable write FSizeable;
  end;

  {TInplaceStringGrid}
  TInplaceStringGrid = class(TStringGrid)
  private
    FParentEdit: TAdvDBLookupComboBox;
    procedure WMKeyDown(var Msg: TWMKeydown); message WM_KEYDOWN;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
  protected
    procedure DoExit; override;
  property
    ParentEdit:TAdvDBLookupComboBox read FParentEdit write FParentEdit;
  end;

  { TDropGridListButton }
  TDropGridListButton = class(TSpeedButton)
  private
    FFocusControl: TWinControl;
    FMouseClick: TNotifyEvent;
    FArrEnabled: TBitmap;
    FIsWinXP: Boolean;
    FHover: Boolean;
    procedure WMLButtonDown(var Msg:TMessage); message WM_LBUTTONDOWN;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  protected
    procedure Paint; override;
    property Hover: Boolean read FHover write FHover;
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property IsWinXP: Boolean read FIsWinXP write FIsWinXP;
  published
    property FocusControl: TWinControl read FFocusControl write FFocusControl;
    property MouseClick:TNotifyEvent read FMouseClick write FMouseClick;
  end;

  TGridListItemToText = procedure(sender:TObject;var aText:string) of object;
  TTextToGridListItem = procedure(sender:TObject;var aItem:string) of object;

  TDrawGridCellProp = procedure (Sender: TObject; RowIndex: Integer; ColIndex: Integer; DBField: TField; Value: string; AFont: TFont; var AColor: TColor) of object;
  TGridSelectCellEvent = procedure (Sender: TObject; Col, Row: Integer; Avalue: string; var CanSelect: Boolean) of object;

  TDropDirection = (ddDown,ddUp);

  TSortMethod = (smText, smNumeric, smDate);

  TLookupMethod = (lmNormal,lmFast,lmRequired);

  TLookupLoad = (llAlways, llOnNeed);

  TLookupSearch = (isFirstChar, isAnyChar);

  { TAdvDBLookupComboBox }

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvDBLookupComboBox = class(TCustomEdit)
  private
    FButton: TDropGridListButton;
    FEditorEnabled: Boolean;
    FOnClickBtn: TNotifyEvent;
    FStringGrid: TInplaceStringGrid;
    FDropHeight: Integer;
    FDropWidth: Integer;
    FSortColumns: Integer;
    FSortMethod: TSortMethod;
    FDropColor: TColor;
    FDropFont: TFont;
    FDropSorted: Boolean;
    fDropDirection: TDropDirection;
    FChkForm: TDropForm;
    FChkClosed: Boolean;
    FCloseClick: Boolean;
    FOnGridListItemToText: TGridListItemToText;
    FOnTextToGridListItem: TTextToGridListItem;
    FColumns: TDBColumnCollection;
    FListDataLink: TDBGridDataLink;
    FDataSourceLink: TDBGridLookupDataLink;
    FAllfields: TList;
    FBitmapUp,FBitmapdown:TBitmap;
    FDataScroll: Boolean;
    FItemIndex, FOldItemIndex, FOldItemIndex2: Integer;
    FKeyField: string;
    FDataField: string;
    FHeaderColor: Tcolor;
    FSelectionColor: Tcolor;
    FCurrentSearch:string;
    FAccept: Boolean;
    FSensSorted: TSortType;
    FSelectionTextColor: TColor;
    FGridLines: Boolean;
    FGridColumnSize: Boolean;
    FFilterValue: string;
    FFilterField: string;
    FBookmark:TBookmark;
    FLookupMethod: TLookupMethod;
    FLabelAlwaysEnabled: Boolean;
    FLabelTransparent: Boolean;
    FLabelMargin: Integer;
    FLabelFont: TFont;
    FLabelPosition: TLabelPosition;
    FLabel: TLabelEx;
    FDropDownType: TDropDownType;
    FOnLookupError: TLookupErrorEvent;
    FOnLookupSuccess: TLookupSuccessEvent;
    FLabelField: string;
    FSortColumn: string;
    FLabelWidth: Integer;
    FGridRowHeight: Integer;
    FGridHeaderHeight: Integer;
    FLookupLoad: TLookupLoad;
    FDisableChange: Boolean;
    FInLookup: Boolean;
    FDropSizeable: Boolean;
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    FOnClosed: TNotifyEvent;
    FReturnIsTab: Boolean;
    FFocusColor: TColor;
    FNormalColor: TColor;
    FImages: TImageList;
    FShowGridTitleRow: Boolean;
    FLookupColumn: Integer;
    FInternalCall: Boolean;
    FOnDrawProp: TDrawGridCellProp;
    FDropStretchColumn: Integer;
    FDisabledColor: TColor;
    FGridHeaderAutoSize: Boolean;
    FOnGridSelectCell: TGridSelectCellEvent;
    FOnGridSelectedCell: TGridSelectCellEvent;
    FGridCellNotSelected: Boolean;
    FAlwaysRefreshDropDownList: Boolean;
    FHoveredRow: Integer;
    FHoverColor: TColor;
    FHoverTextColor: TColor;
    FLookupInAllColumns: Boolean;
    FSelectionChanged: Boolean;
    FShowMemoFields: Boolean;
    FModified: Boolean;
    FLookupSearch: TLookupSearch;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit);   message CM_EXIT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMPaste(var Message: TWMPaste);   message WM_PASTE;
    procedure WMCut(var Message: TWMCut);   message WM_CUT;
    procedure WMKeyDown(var Msg:TWMKeydown); message WM_KEYDOWN;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMSysKeyDown(var Msg:TWMKeydown); message WM_SYSKEYDOWN;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;

    function GetMinHeight: Integer;
    procedure SetEditRect;
    procedure OnGridResize(Sender: TObject);
    procedure OnDropDownResize(Sender: TObject);    
    function  GridToString:string;
    procedure ShowGridList(Focus:boolean);
    procedure HideGridList;
    procedure UpdateLookup;
    procedure FormDeactivate(Sender: TObject);
    procedure MouseClick(Sender: TObject);
    procedure DownClick(Sender: TObject);
    procedure SetDropFont(const Value: TFont);
    function GetText: string;
    procedure SetText(const Value: string);
    procedure SetInternalText(const Value: string);
    function CheckDataSet:boolean;
    function CheckEditDataSet:boolean;
    function GetListsource: TDatasource;
    procedure SetListsource(const Value: TDatasource);
    function GetItemIndex: integer;
    procedure SetItemIndex(Value: integer);
    function GetDatasource: TDatasource;
    procedure SetDatasource(const Value: TDatasource);
    procedure SetSortColumns(const Value: Integer);
    function GetRealItemIndex(Index: Integer): Integer;
    procedure SetFilterField(const Value: string);
    procedure SetFilterValue(const Value: string);
    function GetLabelCaption: string;
    procedure SetLabelAlwaysEnabled(const Value: Boolean);
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelFont(const Value: TFont);
    procedure SetLabelMargin(const Value: Integer);
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelTransparent(const Value: Boolean);
    procedure UpdateLabel;
    procedure LabelFontChange(Sender: TObject);
    procedure SetLabelField(const Value: string);
    procedure SetSortColumn(const Value: string);
    procedure SetLabelWidth(const Value: Integer);
    procedure SetSortDownGlyph(const Value: TBitmap);
    procedure SetSortUpGlyph(const Value: TBitmap);
    procedure SetLookupLoad(const Value: TLookupLoad);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetEnabledEx: boolean;
    procedure SetEnabledEx(const Value: boolean);
    procedure SetDisabledColor(const Value: TColor);
    procedure SetLookupColumn(const Value: Integer);
    function SecureLookup(const Data:TDataSet; const Field: TField; const KeyFields: String; const KeyValues, KeyValuesDefault: Variant; const ResultFields: String): Variant;
    function GetDropDownRowCount: integer;
    procedure DestroyBookMark;
    procedure SetShowMemoFields(const Value: Boolean);
    function GetModified: Boolean;
    procedure SetModified(const Value: Boolean);
  protected
    function GetVersionNr: Integer; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    function GetParentForm(Control: TControl): TCustomForm; virtual;
    Procedure LoadGridOptions;
    procedure StringGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure StringGridKeyPress(Sender: TObject; var Key: Char);
    procedure StringGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    function LoadFromListSource: Integer;
    //procedure UpdateFromListSource;
    procedure SetActive(Active: Boolean);
    procedure Change; override;
    procedure DoEnter; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function FindField(Value: string): Boolean;
    function CreateLabel: TLabelEx;
    procedure UpdateText(s:string);
    property SortColumns: Integer read FSortColumns write SetSortColumns default 0;
    procedure SetSortMethod;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function GetGridColumnsWidth: Integer;
    procedure UpdateDropStretchColumnWidth;
    function GetColumnField(ACol: Integer): TField;
    procedure CancelChanges;
    function CanModify: Boolean; virtual;
    function GetLookupDataField: TField;
    procedure UpdateDisplayText;
    procedure DoChange; virtual;
    procedure DoCloseUp; virtual;
    procedure DoClosed; virtual;
  public
   {$IFDEF TMSDEBUG}
    procedure DebugTest;
   {$ENDIF}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Init; virtual;
    property Button: TDropGridListButton read FButton;
    property Text: string read GetText write SetText;
    property ItemIndex: Integer Read GetItemIndex write SetItemIndex;
    //property LookupColumn: Integer read FLookupColumn write SetLookupColumn;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DropDown;
    procedure UpdateDisplText;
    property AlwaysRefreshDropDownList: Boolean read FAlwaysRefreshDropDownList write FAlwaysRefreshDropDownList default False;
    property DropDownRowCount: integer read GetDropDownRowCount;
    property Modified: Boolean read GetModified write SetModified;
  published
    property Align;
    property Anchors;
    property Constraints;
    property DragKind;
    property AutoSelect;
    property AutoSize;
    {$IFDEF DELPHI7_LVL}
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelEdges;
    {$ENDIF}
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clSilver;    
    property DragCursor;
    property DragMode;
    property DropDownType: TDropDownType read FDropDownType write FDropDownType default ddUser;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Enabled: Boolean read GetEnabledEx write SetEnabledEx;
    property FilterField: string read FFilterField write SetFilterField;
    property FilterValue: string read FFilterValue write SetFilterValue;
    property Font;
    property LabelCaption:string read GetLabelCaption write SetLabelCaption;
    property LabelPosition:TLabelPosition read FLabelPosition write SetLabelPosition;
    property LabelMargin: Integer read FLabelMargin write SetLabelMargin;
    property LabelTransparent: Boolean read FLabelTransparent write SetLabelTransparent;
    property LabelAlwaysEnabled: Boolean read FLabelAlwaysEnabled write SetLabelAlwaysEnabled;
    property LabelField: string read FLabelField write SetLabelField;
    property LabelFont:TFont read FLabelFont write SetLabelFont;
    property LabelWidth: Integer read FLabelWidth write SetLabelWidth;
    property LookupColumn: Integer read FLookupColumn write SetLookupColumn;
    property LookupMethod: TLookupMethod read FLookupMethod write FLookupMethod;
    property LookupLoad: TLookupLoad read FLookupLoad write SetLookupLoad;
    property LookupSearch: TLookupSearch read FLookupSearch write FLookupSearch default isFirstChar;
    property LookupInAllColumns: Boolean read FLookupInAllColumns write FLookupInAllColumns default false;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Height;
    property Width;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
    property Columns: TDBColumnCollection read FColumns write FColumns;
    property DataField: string read FDataField write FDataField;
    property DataSource: TDatasource read GetDatasource write SetDatasource;
    property DropWidth: Integer read FDropWidth write FDropWidth;
    property DropStretchColumn: Integer read FDropStretchColumn write FDropStretchColumn;
    property DropHeight: Integer read FDropHeight write FDropHeight default 100;
    property DropColor: TColor read FDropColor write fDropColor default clWindow;
    property DropFont: TFont read FDropFont write SetDropFont;
    property DropDirection: TDropDirection read FDropDirection write FDropDirection default ddDown;
    property DropSorted: Boolean read FDropSorted write FDropSorted default False;
    property DropSizeable: Boolean read FDropSizeable write FDropSizeable default False;
    property FocusColor: TColor read FFocusColor write FFocusColor default clNone;    
    property GridLines: Boolean read FGridLines write FGridLines default true;
    property GridColumnSize: Boolean read FGridColumnSize write FGridColumnSize default true;
    property GridRowHeight: Integer read FGridRowHeight write FGridRowheight default 21;
    property GridHeaderAutoSize: Boolean read FGridHeaderAutoSize write FGridHeaderAutoSize default false;
    property GridHeaderHeight: Integer read FGridHeaderHeight write FGridHeaderheight default 21;    
    property HeaderColor: TColor read FHeaderColor write FHeaderColor default clBtnFace;
    property HoverColor: TColor read FHoverColor write FHoverColor default clHighLight;
    property HoverTextColor: TColor read FHoverTextColor write FHoverTextColor default clHighLightText;
    property Images: TImageList read FImages write FImages;
    property KeyField: string read FKeyField write FKeyField;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property ReturnIsTab: Boolean read FReturnIsTab write FReturnIsTab default False;
    property SelectionColor: TColor read FSelectionColor write FSelectionColor default clHighLight;
    property SelectionTextColor: TColor read FSelectionTextColor write FSelectionTextColor default clHighLightText;
    property ShowMemoFields: Boolean read FShowMemoFields write SetShowMemoFields default False;
    property SortColumn: string read FSortColumn write SetSortColumn;
    property SortUpGlyph: TBitmap read FBitmapDown write SetSortUpGlyph;
    property SortDownGlyph: TBitmap read FBitmapUp write SetSortDownGlyph;
    property ShowGridTitleRow: Boolean read FShowGridTitleRow write FShowGridTitleRow;
    property OnClickBtn: TNotifyEvent read FOnClickBtn write FOnClickBtn;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnClosed: TNotifyEvent read FOnClosed write FOnClosed;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnGridSelectCell: TGridSelectCellEvent read FOnGridSelectCell write FOnGridSelectCell;
    property OnGridSelectedCell: TGridSelectCellEvent read FOnGridSelectedCell write FOnGridSelectedCell;
    property OnTextToGridListItem: TTextToGridListItem read FOnTextToGridListItem write FOnTextToGridListItem;
    property OnGridListItemToText: TGridListItemToText read FOnGridListItemToText write FOnGridListItemToText;
    property OnLookupError: TLookupErrorEvent read FOnLookupError write FOnLookupError;
    property OnLookupSuccess: TLookupSuccessEvent read FOnLookupSuccess write FOnLookupSuccess;
    property Version: string read GetVersion write SetVersion;
    property OnDrawProp: TDrawGridCellProp read FOnDrawProp write FOnDrawProp;
  end;

implementation

{$R ADVDBCOMBO.RES}

type

{$IFNDEF DELPHI_UNICODE}
  TCharSet = set of char;
{$ENDIF}

{$IFDEF DELPHI_UNICODE}
  TCharSet = Array of char;
{$ENDIF}

const
  ALIGNSTYLE : array[TAlignment] of DWORD = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WORDWRAPSTYLE : array[Boolean] of DWORD = (DT_SINGLELINE, DT_WORDBREAK);
  LAYOUTSTYLE : array[TTextLayout] of DWORD = (0,DT_VCENTER,DT_BOTTOM);
  ELLIPSSTYLE : array[TEllipsType] of DWORD = (0,DT_END_ELLIPSIS,DT_PATH_ELLIPSIS);
  ACCELSTYLE : array[Boolean] of DWORD = (DT_NOPREFIX,0);
  MAX_COLUMNS = 100;

{$I DELPHIXE.INC}

function GetFileVersion(const AFileName: string): Cardinal;
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  Result := Cardinal(-1);
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  FileName := AFileName;
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
          Result:= FI.dwFileVersionMS;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

//----------------------------------------------------------------- DrawGradient

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;

begin
  if Direction then
    R.Right := R.Right - 1
  else
    R.Bottom := R.Bottom - 1;

  if Steps = 0 then
    Steps := 1;

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with Canvas do
  begin
    for i := 0 to steps - 1 do
    begin
      endr := startr + Round(rstepr * i);
      endg := startg + Round(rstepg * i);
      endb := startb + Round(rstepb * i);
      stepw := Round(i * rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
        Rectangle(R.Left + stepw, R.Top, R.Left + stepw + Round(rstepw) + 1, R.Bottom)
      else
        Rectangle(R.Left, R.Top + stepw, R.Right, R.Top + stepw + Round(rstepw) + 1);
    end;
  end;
end;

//------------------------------------------------------------------------------

function VarPos(su,s:string;var Respos:Integer):Integer;
begin
  Respos := Pos(su,s);
  Result := Respos;
end;

{$IFNDEF DELPHI_UNICODE}
function FirstChar(Charset:TCharSet;s:string):char;
var
  i:Integer;
begin
  i := 1;
  Result := #0;
  while i <= Length(s) do
  begin
    if s[i] in Charset then
    begin
      Result := s[i];
      Break;
    end;
    Inc(i);
  end;
end;
{$ENDIF}

{$IFDEF DELPHI_UNICODE}
function FirstChar(Charset:TCharSet;s:string):char;
var
  i:Integer;

  function InArray(ch: char): boolean;
  var
    j: integer;
  begin
    result := false;
    for j := 0 to High(CharSet) - 1 do
    begin
      if ch = CharSet[j] then 
      begin
        result := true;
        break;
      end;
    end;
  end;


begin
  i := 1;
  Result := #0;
  while i <= Length(s) do
  begin
    if InArray(s[i]) then
    begin
      Result := s[i];
      Break;
    end;
    Inc(i);
  end;
end;
{$ENDIF}


function IsDate(s:string;var dt:TDateTime):boolean;
var
  su: string;
  da,mo,ye: word;
  err: Integer;
  dp,mp,yp,vp: Integer;
begin
  Result:=False;

  su := UpperCase(shortdateformat);
  dp := pos('D',su);
  mp := pos('M',su);
  yp := pos('Y',su);

  da := 0;
  mo := 0;
  ye := 0;

  if VarPos(Dateseparator,s,vp)>0 then
  begin
    su := Copy(s,1,vp - 1);

    if (dp<mp) and
       (dp<yp) then
       val(su,da,err)
    else
    if (mp<dp) and
       (mp<yp) then
       val(su,mo,err)
    else
    if (yp<mp) and
       (yp<dp) then
       val(su,ye,err);

    if err<>0 then Exit;
    Delete(s,1,vp);

    if VarPos(DateSeparator,s,vp)>0 then
    begin
      su := Copy(s,1,vp - 1);

      if ((dp>mp) and (dp<yp)) or
         ((dp>yp) and (dp<mp)) then
         val(su,da,err)
      else
      if ((mp>dp) and (mp<yp)) or
         ((mp>yp) and (mp<dp)) then
         val(su,mo,err)
      else
      if ((yp>mp) and (yp<dp)) or
         ((yp>dp) and (yp<mp)) then
         val(su,ye,err);

      if err<>0 then Exit;
      Delete(s,1,vp);

      if (dp>mp) and
         (dp>yp) then
         val(s,da,err)
      else
      if (mp>dp) and
         (mp>yp) then
         val(s,mo,err)
      else
      if (yp>mp) and
         (yp>dp) then
         val(s,ye,err);

      if err<>0 then Exit;
      if (da>31) then Exit;
      if (mo>12) then Exit;

      Result:=True;

      try
        dt := EncodeDate(ye,mo,da);
      except
        Result := False;
      end;

     end;

  end;
end;


function Matches(s0a,s1a:PChar):boolean;
const
  larger = '>';
  smaller = '<';
  logand  = '&';
  logor   = '^';
  asterix = '*';
  qmark = '?';
  negation = '!';
  null = #0;

var
  matching:boolean;
  done:boolean;
  len:longint;
  lastchar:char;
  s0,s1,s2,s3:pchar;
  oksmaller,oklarger,negflag:boolean;
  compstr:array[0..255] of char;
  flag1,flag2,flag3:boolean;
  equal:boolean;
  n1,n2:double;
  code1,code2:Integer;
  dt1,dt2:TDateTime;

begin
  oksmaller := True;
  oklarger := True;
  flag1 := False;
  flag2 := False;
  flag3 := False;
  negflag := False;
  equal := False;

  { [<>] string [&|] [<>] string }


  // do larger than or larger than or equal
  s2 := StrPos(s0a,larger);
  if s2 <> nil then
  begin
    inc(s2);
    if (s2^ = '=') then
    begin
      Equal := True;
      inc(s2);
    end;

    while (s2^ = ' ') do
      inc(s2);

    s3 := s2;
    len := 0;

    lastchar := #0;

    while (s2^ <> ' ') and (s2^ <> NULL) and (s2^ <> '&') and (s2^ <> '|')  do
    begin
      if (len = 0) and (s2^ = '"') then
        inc(s3)
      else
        inc(len);

      lastchar := s2^;
      inc(s2);
    end;

    if (len > 0) and (lastchar = '"') then
      dec(len);

    StrLCopy(compstr,s3,len);

    Val(s1a,n1,code1);
    Val(compstr,n2,code2);

    if (code1 = 0) and (code2 = 0) then {both are numeric types}
    begin
      if equal then
        oklarger := n1 >= n2
      else
        oklarger := n1 > n2;
    end
    else
    begin
      if IsDate(StrPas(compstr),dt2) and IsDate(StrPas(s1a),dt1) then
      begin
        if equal then
         oklarger := dt1 >= dt2
        else
         oklarger := dt1 > dt2;
      end
      else
      begin
        if equal then
         oklarger := (strlcomp(compstr,s1a,255)<=0)
        else
         oklarger := (strlcomp(compstr,s1a,255)<0);
      end;
    end;
    flag1 := True;
  end;

  equal := False;

  // do smaller than or smaller than or equal
  s2 := strpos(s0a,smaller);
  if (s2 <> nil) then
  begin
    inc(s2);
    if (s2^ = '=') then
      begin
       equal := True;
       inc(s2);
      end;
      
    lastchar := #0;

    while (s2^=' ') do inc(s2);
    s3 := s2;
    len := 0;
    while (s2^ <> ' ') and (s2^ <> NULL) and (s2^ <> '&') and (s2^ <> '|') do
    begin
      if (len = 0) and (s2^ = '"') then
        inc(s3)
      else
        inc(len);

      lastchar := s2^;
      inc(s2);
    end;

    if (len > 0) and (lastchar = '"') then
      dec(len);

    strlcopy(compstr,s3,len);

    val(s1a,n1,code1);
    val(compstr,n2,code2);

    if (code1 = 0) and (code2 = 0) then // both are numeric types
     begin
      if equal then
       oksmaller := n1 <= n2
      else
       oksmaller := n1 < n2;
     end
    else
     begin
      // check for dates here ?
      if IsDate(strpas(compstr),dt2) and IsDate(strpas(s1a),dt1) then
       begin
        if equal then
         oksmaller := dt1 <= dt2
        else
         oksmaller := dt1 < dt2;
       end
      else
       begin
        if equal then
          oksmaller := (strlcomp(compstr,s1a,255)>=0)
        else
          oksmaller := (strlcomp(compstr,s1a,255)>0);
       end;
     end;

    flag2 := True;
  end;

  s2 := strpos(s0a,negation);
  
  if (s2 <> nil) then
  begin
    inc(s2);
    while (s2^=' ') do
      inc(s2);
    s3 := s2;
    len := 0;

    lastchar := #0;

    while (s2^ <> ' ') and (s2^ <> NULL) and (s2^ <> '&') and (s2^ <> '|') do
    begin
      if (len = 0) and (s2^ = '"') then
        inc(s3)
      else
        inc(len);
        
      lastchar := s2^;
      inc(s2);
    end;

    if (len > 0) and (lastchar = '"') then
      dec(len);

    strlcopy(compstr,s3,len);
    flag3 := True;
  end;

  if (flag3) then
  begin
    if strpos(s0a,larger) = nil then
      flag1 := flag3;
    if strpos(s0a,smaller) = nil then
      flag2 := flag3;
  end;

  if (strpos(s0a,logor) <> nil) then
    if flag1 or flag2 then
    begin
      matches := oksmaller or oklarger;
      Exit;
    end;

  if (strpos(s0a,logand)<>nil) then
    if flag1 and flag2 then
    begin
      matches := oksmaller and oklarger;
      Exit;
    end;

  if ((strpos(s0a,larger) <> nil) and (oklarger)) or
     ((strpos(s0a,smaller) <> nil) and (oksmaller)) then
  begin
    matches := True;
    Exit;
  end;

  s0 := s0a;
  s1 := s1a;

  matching := True;

  done := (s0^ = NULL) and (s1^ = NULL);

  while not done and matching do
  begin
    case s0^ of
    qmark:
      begin
        matching := s1^ <> NULL;
        if matching then
        begin
          inc(s0);
          inc(s1);
        end;
      end;
    negation:
      begin
        negflag:=True;
        inc(s0);
      end;
    '"':
      begin
        inc(s0);
      end;
    asterix:
      begin
        repeat
          inc(s0)
        until (s0^ <> asterix);
        len := strlen(s1);
        inc(s1,len);
        matching := matches(s0,s1);
        while (len >= 0) and not matching do
        begin
         dec(s1);
         dec(len);
         matching := Matches(s0,s1);
       end;
       if matching then
       begin
         s0 := strend(s0);
         s1 := strend(s1);
       end;
     end;
   else
     begin
       matching := s0^ = s1^;

       if matching then
       begin
         inc(s0);
         inc(s1);
       end;
     end;
   end;

   Done := (s0^ = NULL) and (s1^ = NULL);
  end;

  if negflag then
    Matches := not matching
  else
    Matches := matching;
end;

function MatchStr(s1,s2:string;DoCase:Boolean):Boolean;
begin
  if DoCase then
    MatchStr := Matches(PChar(s1),PChar(s2))
  else
    MatchStr := Matches(PChar(AnsiUpperCase(s1)),PChar(AnsiUpperCase(s2)));
end;

function MatchStrEx(s1,s2:string;DoCase:Boolean): Boolean;
var
  ch,lastop: Char;
  sep: Integer;
  res,newres: Boolean;
  {$IFDEF DELPHI_UNICODE}
  CharArray: TCharSet;
  {$ENDIF}

begin
 {remove leading & trailing spaces}
  s1 := Trim(s1);
 {remove spaces between multiple filter conditions}
  while VarPos(' &',s1,sep) > 0 do Delete(s1,sep,1);
  while VarPos(' ;',s1,sep) > 0 do Delete(s1,sep,1);
  while VarPos(' ^',s1,sep) > 0 do Delete(s1,sep,1);
  while VarPos(' |',s1,sep) > 0 do Delete(s1,sep,1);
  while VarPos(' =',s1,sep) > 0 do Delete(s1,sep,1);
  while VarPos('& ',s1,sep) > 0 do Delete(s1,sep+1,1);
  while VarPos('; ',s1,sep) > 0 do Delete(s1,sep+1,1);
  while VarPos('^ ',s1,sep) > 0 do Delete(s1,sep+1,1);
  while VarPos('| ',s1,sep) > 0 do Delete(s1,sep+1,1);
  while VarPos('= ',s1,sep) > 0 do Delete(s1,sep+1,1);

  while VarPos('=',s1,sep) > 0 do Delete(s1,sep,1);

  LastOp := #0;
  Res := True;

  {$IFDEF DELPHI_UNICODE}
  SetLength(CharArray, 4);
  CharArray[0] := ';';
  CharArray[1] := '^';
  CharArray[2] := '&';
  CharArray[3] := '|';
  {$ENDIF}

  repeat
    {$IFDEF DELPHI_UNICODE}
    ch := FirstChar(CharArray,s1);
    {$ENDIF}

    {$IFNDEF DELPHI_UNICODE}
    ch := FirstChar([';','^','&','|'],s1);
    {$ENDIF}

    {extract first part of filter}
    if ch <> #0 then
    begin
      VarPos(ch,s1,sep);
      NewRes := MatchStr(Copy(s1,1,sep-1),s2,DoCase);
      Delete(s1,1,sep);

      if LastOp = #0 then
        Res := NewRes
      else
        case LastOp of
        ';','^','|':Res := Res or NewRes;
        '&':Res := Res and NewRes;
        end;

      LastOp := ch;
     end;
  until ch = #0;

  NewRes := MatchStr(s1,s2,DoCase);

  if LastOp = #0 then
    Res := NewRes
  else
    case LastOp of
    ';','^','|':Res := Res or NewRes;
    '&':Res := Res and NewRes;
    end;

  Result := Res;
end;

{ TDropGridListButton }
constructor TDropGridListButton.Create(AOwner: TComponent);
var i: Integer;
begin
  inherited Create(AOwner);
  Cursor := crArrow;
  FArrEnabled := TBitmap.Create;
  FArrEnabled.LoadFromResourceName(HInstance,'AC_ARROW_DOWN');
  Glyph.Assign(FArrEnabled);
  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  FIsWinXP := (i > 5);
end;

procedure TDropGridListButton.Paint;
var htheme: THandle;
    ARect: TRect;
begin
  if not (IsWinXP and IsThemeActive) then
    inherited Paint
  else
    begin
    htheme := OpenThemeData(Parent.Handle,'combobox');
    ARect := ClientRect;
    InflateRect(ARect,1,1);
    ARect.Left := ARect.Left + 2;

    if not Enabled then
    begin
      DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_DISABLED,@ARect,nil);
    end
    else
    begin
      if Down then
        DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_PRESSED,@ARect,nil)
      else
      begin
        if Hover then
          DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_HOT,@ARect,nil)
        else
          DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_NORMAL,@ARect,nil);
      end;
    end;
    CloseThemeData(htheme);
    end;
end;

procedure TDropGridListButton.Click;
begin
  if (FFocusControl <> nil) and FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
  FFocusControl.SetFocus;
  inherited Click;
end;

procedure TDropGridListButton.WMLButtonDown(var Msg: TMessage);
begin
  if Assigned(FMouseClick) then FMouseClick(self);
  inherited;
end;

procedure TDropGridListButton.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
end;

destructor TDropGridListButton.Destroy;
begin
  FArrEnabled.Free;
  inherited;
end;

procedure TDropGridListButton.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FHover := true;
  Invalidate;
end;

procedure TDropGridListButton.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FHover := false;
  Invalidate;
end;

{ TAdvDBLookupComboBox }
constructor TAdvDBLookupComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLookupColumn := 0;
  FBitmapUp := TBitmap.Create;
  FBitmapUp.Handle := LoadBitmap(HInstance, 'AC_MINIARROW_UP');
  FBitmapUp.Transparent := True;

  FBitmapDown := TBitmap.Create;
  FBitmapDown.Handle := LoadBitmap(HInstance, 'AC_MINIARROW_DOWN');
  FBitmapDown.Transparent := True;

  SetBounds(left,top,250,Height);
  FAllfields := Tlist.Create;
  FSensSorted := stAscendent;
  FHeaderColor := clBtnFace;
  FDisabledColor := clSilver;
  FSelectionColor := clHighLight;
  FSelectionTextColor := clHighLightText;
  FSortColumns := 0;
  FLookupInAllColumns := false;
  FColumns := TDBColumnCollection.Create(self);
  FListDataLink := TDBGridDataLink.Create(self);
  FDataSourceLink := TDBGridLookupDataLink.Create(Self);

  FButton := TDropGridListButton.Create (Self);
  FButton.Width := 15;
  FButton.Height := 17;
  FButton.Visible := True;
  FButton.Parent := Self;
  FButton.FocusControl := Self;
  FButton.MouseClick := MouseClick;
  FButton.OnClick := DownClick;
  FButton.Enabled := False;

  FLookupSearch := isFirstChar;

  SetInternalText('');

  ControlStyle := ControlStyle - [csSetCaption];

  FEditorEnabled := True;
  FDropHeight := 100;
  FDropWidth := self.Width;
  FDropSorted := False;
  FDropColor := clWindow;
  FDropFont := TFont.Create;
  FDropFont.Name := 'Tahoma';
  FChkClosed := True;

  FGridLines := True;
  FGridColumnSize := True;

  FGridRowHeight := 21;
  FGridHeaderHeight := 21;
  FGridHeaderAutoSize := False;

  FLabel := nil;
  FLabelFont := TFont.Create;
  FLabelFont.OnChange := LabelFontChange;
  FLabelMargin := 4;
  FLabelWidth := 0;
  FInLookup := False;
  FFocusColor := clNone;

  FShowGridTitleRow := True;
  FDisableChange := False;

  FDropStretchColumn := -1;
  FOldItemIndex2 := -1;

  FHoveredRow := -1;
  FHoverColor := clHighLight;
  FHoverTextColor := clHighLightText;
end;

destructor TAdvDBLookupComboBox.Destroy;
var
  i: Integer;
begin
  DestroyBookMark;

  for i := 0 to FAllfields.Count - 1 do
    TFindList(FAllfields.Items[i]).Free;

  FAllfields.Free;
  FListDataLink.Free;
  FDataSourceLink.Free;
  FButton.Free;
  FColumns.Free;
  FDropFont.Free;
  FBitmapUp.Free;
  FBitmapDown.Free;
  FLabelFont.Destroy;
  if FLabel <> nil then
    FLabel.Free;
  inherited Destroy;
end;

procedure TAdvDBLookupComboBox.DestroyBookMark;
begin
  if Assigned(FListDataLink.DataSource)then
  begin
    if Assigned(FBookmark) then
    begin
      if Assigned(FListDataLink.DataSource.DataSet) then
      begin
        if FListDataLink.DataSource.DataSet.BookmarkValid(FBookmark) then
        begin
          FListDataLink.DataSource.DataSet.FreeBookmark(FBookmark);
          FBookMark := nil;
        end;
      end
      else
      begin
        {$IFNDEF DELPHI_UNICODE}
        Freemem(FBookmark);
        {$ENDIF}
        FBookmark := nil;
      end;
    end
    else
    begin
      {$IFNDEF DELPHI_UNICODE}
      Freemem(FBookmark);
      {$ENDIF}
      FBookmark := nil;
    end;
  end
  else
    FBookmark := nil;
end;

function TAdvDBLookupComboBox.CreateLabel: TLabelEx;
begin
  Result := TLabelEx.Create(self);
  Result.Parent := Self.Parent;
  Result.FocusControl := Self;
  Result.Font.Assign(LabelFont);
end;

procedure TAdvDBLookupComboBox.UpdateLabel;
begin
  if FLabel = nil then
    Exit;
    
  FLabel.Transparent := FLabeltransparent;
  case FLabelPosition of
  lpLeftTop:
    begin
      FLabel.top := self.top;
      FLabel.left := self.left-FLabel.canvas.textwidth(FLabel.caption)-FLabelMargin;
    end;
  lpLeftCenter:
    begin
      FLabel.top := self.top+((self.height-FLabel.height) shr 1);
      FLabel.left := self.left-FLabel.canvas.textwidth(FLabel.caption)-FLabelMargin;
    end;
  lpLeftBottom:
    begin
      FLabel.top := self.top+self.height-FLabel.height;
      FLabel.left := self.left-FLabel.canvas.textwidth(FLabel.caption)-FLabelMargin;
    end;
  lpTopLeft:
    begin
      FLabel.top := self.top-FLabel.height-FLabelMargin;
      FLabel.left := self.left;
    end;
  lpTopCenter:
    begin
      FLabel.Top := self.top-FLabel.height-FLabelMargin;
      FLabeL.Left := self.Left + ((self.Width-FLabel.width) shr 1);
    end;
  lpBottomLeft:
    begin
      FLabel.top := self.top+self.height+FLabelMargin;
      FLabel.left := self.left;
    end;
  lpBottomCenter:
    begin
      FLabel.top := self.top+self.height+FLabelMargin;
      FLabeL.Left := self.Left + ((self.Width-FLabel.width) shr 1);
    end;
  lpLeftTopLeft:
    begin
      FLabel.top := self.top;
      FLabel.left := self.left-FLabelMargin;
    end;
  lpLeftCenterLeft:
    begin
      FLabel.top := self.top+((self.height-FLabel.height) shr 1);
      FLabel.left := self.left-FLabelMargin;
    end;
  lpLeftBottomLeft:
    begin
      FLabel.top:=self.top+self.height-FLabel.height;
      FLabel.left:=self.left-FLabelMargin;
    end;
  lpRightTop:
    begin
      FLabel.Top := self.Top;
      FLabel.Left := self.Left + self.Width + FLabelMargin;
    end;
  lpRightCenter:
    begin
      FLabel.top := self.top+((self.height-FLabel.height) shr 1);
      FLabel.Left := self.Left + self.Width + FLabelMargin;
    end;
  lpRightBottom:
    begin
      FLabel.Left := self.Left + self.Width + FLabelMargin;
      FLabel.Top := self.Top + self.Height - FLabel.Height;          
    end;
  end;
  FLabel.Font.Assign(FLabelFont);

  FLabel.AutoSize := FLabelWidth = 0;
  if FLabelWidth <> 0 then
  begin
    FLabel.Width := FLabelWidth;
    FLabel.EllipsType := etEndEllips;
  end
  else
    FLabel.EllipsType := etNone;

  if not CheckDataSet then
  begin
    if LabelField <> '' then
      FLabel.Caption := '';
  end;

  if CheckDataSet then
  begin
    if (LabelField <> '') then
    begin
      if (Text = '') then
        FLabel.Caption := '';
    end;
  end;

  FLabel.Visible := Visible;
end;

procedure TAdvDBLookupComboBox.SetDisabledColor(const Value: TColor);
begin
  FDisabledColor := Value;
  Invalidate;
end;


function TAdvDBLookupComboBox.GetEnabledEx: boolean;
begin
  Result := inherited Enabled;
end;

procedure TAdvDBLookupComboBox.SetEnabledEx(const Value: boolean);
var
  OldValue: Boolean;
  OldColor: TColor;
begin
  OldValue := inherited Enabled;

  inherited Enabled := Value;

  if (csLoading in ComponentState) or
    (csDesigning in ComponentState) then
    Exit;

  if OldValue <> Value then
  begin
    if Value then
    begin
      Color := FNormalColor;
    end
    else
    begin
      OldColor := Color;
      Color := FDisabledColor;
      FNormalColor := OldColor;
    end;

    if Assigned(FLabel) then
      if not FLabelAlwaysEnabled then
        FLabel.Enabled := Value;
  end;
end;


procedure TAdvDBLookupComboBox.LabelFontChange(Sender: TObject);
begin
  if Assigned(FLabel) then
    UpdateLabel;
end;


function TAdvDBLookupComboBox.GetParentForm(Control: TControl): TCustomForm;
begin
  Result := nil;
  if Assigned(Control) then
    if Control is TCustomForm then
    begin
      Result := Control as TCustomForm;
      Exit;
    end else
    begin
      if Assigned(Control.Parent) then
        Result := GetParentForm(Control.Parent);
    end;
end;

procedure TAdvDBLookupComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TAdvDBLookupComboBox.DestroyWnd;
begin
  inherited;
end;

procedure TAdvDBLookupComboBox.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
  self.ReadOnly := not EditorEnabled;
end;

function TAdvDBLookupComboBox.LoadFromListsource: Integer;
var
  d: TDataset;
  cb,ct: TBookMark;
  i,index: Integer;
  fldlist:TFindList;
  fltr, aField: TField;
  DoAdd: Boolean;

begin
  Result := -1;

  FButton.Enabled := False;

  if not CheckDataSet or (KeyField = '') then
    Exit;

  if FLookupLoad = llOnNeed then
    if GetFocus <> self.Handle then
      Exit;

  if FColumns.Count = 0 then
    Exit;

  for i := 0 to FAllfields.Count - 1 do
    TFindList(FAllfields.Items[i]).Free;

  FAllfields.Clear;

  d := FListDataLink.DataSource.DataSet;
  FDataScroll := True;
  Index := 0;

  with d do
  begin
    DisableControls;

    try
      cb := GetBookMark;
      First;
      while not Eof do
      begin
        // Iterate through the fields array and put data in cells

        DoAdd := True;

        if (FilterField <> '') and (FilterValue <> '') then
        begin
          fltr := FieldByName(FilterField);
          if Assigned(fltr) then
            DoAdd := MatchStrEx(FilterValue,fltr.DisplayText,False);
        end;

        if DoAdd then
        begin
          fldlist := TFindlist.Create(self);
          fldlist.BaseIndex := Index;
          fldlist.KeyField := FieldByName(KeyField).DisplayText;
          ct := GetBookmark;
          if CompareBookmarks(ct,cb) = 0 then
            Result := Index;

          FreeBookmark(ct);

          for i := 1 to Columns.Count do
          begin
            if (Columns.Items[i - 1].ListField <> '') then
            begin
              try
                //if (Columns.Items[i - 1].FColumnType <> ctImage) then
                aField := FieldByName(Columns.Items[i - 1].ListField);
                if Assigned(aField) then
                begin
                  if (aField.IsBlob) then
                  begin
                    if (aField.DataType <> ftGraphic) then
                    begin
                      if not ShowMemoFields then
                      begin
                        fldlist.Add('(MEMO)')
                      end
                      else
                        fldlist.Add(aField.AsString);
                    end;
                  end
                  else
                    fldlist.Add(aField.DisplayText);
                end
                else
                  fldlist.Add('');
                //else
                //  fldlist.Add(FieldByName(Columns.Items[i - 1].ListField.AsInteger
              except
                on Exception do fldlist.Add('');
              end;
            end
            else
              fldlist.Add('');
          end;

          FAllfields.Add(pointer(fldlist));
        end;

        Inc(Index);
        Next;
      end;
      GotoBookMark(cb);
      FreeBookMark(cb);
      FListDataLink.FNumberRecords := d.RecordCount;

    finally
      EnableControls;
    end;  
  end;

  FButton.Enabled := FAllFields.Count > 0;

  {$IFDEF TMSDEBUG}
  if FButton.Enabled then
    outputdebugstring('set btn= enabled');
  {$ENDIF}

  FDataScroll := False;
  SortColumns := FSortColumns;
end;

function SortField(Item1, Item2: Pointer): Integer;
var
  index:integer;
  e: integer;
  v1,v2: double;
  d1,d2: TDateTime;
begin
  index := TFindList(Item1).FGrid.FSortColumns;

  Result := 0;

  case TFindList(Item1).FGrid.FSortMethod of
  smText:
    begin
      if TFindList(Item1).FGrid.FSensSorted = stAscendent then
        Result := CompareText(TFindList(Item1).Strings[index],TFindList(Item2).Strings[index])
      else
        Result := CompareText(TFindList(Item2).Strings[index],TFindList(Item1).Strings[index]);
    end;
  smNumeric:
    begin
      val(TFindList(Item1).Strings[index], v1,e);
      val(TFindList(Item2).Strings[index], v2,e);
      if v1 > v2 then
        Result := 1
      else
        if (v1 = v2) then
          Result := 0
        else
          Result := -1;
    end;
  smDate:
    begin
      try
        d1 := StrToDate(TFindList(Item1).Strings[index]);
        d2 := StrToDate(TFindList(Item2).Strings[index]);
        if d1 > d2 then
          Result := 1
        else
          if (d1 = d2) then
            Result := 0
          else
            Result := -1;
      except
        Result := 0;
      end;

    end;
  end;
end;

procedure TAdvDBLookupComboBox.SetSortMethod;
var
  sField: TField;
begin
  FSortMethod := smText;

  if Assigned(ListSource) then
  begin
    sField := ListSource.DataSet.Fields.FieldByName(Columns[FSortColumns].ListField);
    if Assigned(sField) then
    begin
      if sField.DataType in [ftSmallInt, ftInteger, ftWord, ftFloat, ftCurrency] then
        FSortMethod := smNumeric;
      if sField.DataType in [ftDate] then
        FSortMethod := smDate;
    end;
  end;
end;

function TAdvDBLookupComboBox.GetGridColumnsWidth: Integer;
var
  i: Integer;
begin
  Result := 0;
  if not Assigned(FStringGrid) then
    Exit;

  for i := 0 to FStringGrid.ColCount - 1 do
  begin
    Result := Result + FStringGrid.ColWidths[i];
  end;
end;

procedure TAdvDBLookupComboBox.OnDropDownResize(Sender: TObject);
begin
  DropWidth := (sender as tcustomform).Width;
  DropHeight := (sender as tcustomform).Height;
end;

procedure TAdvDBLookupComboBox.OnGridResize(Sender: TObject);
begin
  UpdateDropStretchColumnWidth;
end;

procedure TAdvDBLookupComboBox.UpdateDisplayText;
var
  tmpix: Integer;
begin
  if (ItemIndex >= 0) and (FAllfields.Count > ItemIndex) then
  begin
    tmpix := TFindList(FAllfields.items[ItemIndex]).BaseIndex;
    ItemIndex := GetRealItemIndex(tmpix);
  end;
end;

procedure TAdvDBLookupComboBox.UpdateDisplText;
begin
  if Assigned(FDataSourceLink.Datasource) and Assigned(FDataSourceLink.Datasource.DataSet) and FDataSourceLink.Datasource.DataSet.Active then
    FDataSourceLink.Modify;
end;

procedure TAdvDBLookupComboBox.UpdateDropStretchColumnWidth;
var
  w: Integer;
begin
  if not Assigned(FStringGrid) then
    Exit;

  if (DropStretchColumn >= 0) and (DropStretchColumn < FStringGrid.ColCount) then
  begin
    w := FStringGrid.ClientWidth - GetGridColumnsWidth - FStringGrid.ColCount;
    if (w > 0) then
    begin
      FStringGrid.ColWidths[FDropStretchColumn] := FStringGrid.ColWidths[FDropStretchColumn] + w;
    end
    else if (w < 0) then
    begin
      if (FStringGrid.ColWidths[FDropStretchColumn] > FColumns.Items[FDropStretchColumn].Width) and (not FColumns.Items[FDropStretchColumn].AutoSize) then
      begin
        w := FStringGrid.ColWidths[FDropStretchColumn] + w;   // w is -ve
        FStringGrid.ColWidths[FDropStretchColumn] := Max(w, FColumns.Items[FDropStretchColumn].Width);
      end;
    end;
  end;
end;

procedure TAdvDBLookupComboBox.ShowGridList(Focus:Boolean);
var
  P, P2: TPoint;
  fOldDropDirection: TDropDirection;
  i,j,k: Integer;
  cw: array[0..MAX_COLUMNS] of Integer;
  dr: TRect;
  sbw: Integer;
  s,txt:string;
  HeaderHeight: Integer;
  R: TRect;
  {$IFDEF DELPHI6_LVL}
  mon: TMonitor;
  {$ENDIF}
begin
  FAccept := False;
  FOldItemIndex := ItemIndex;

  FGridCellNotSelected := False;
  
  FHoveredRow := -1;
  
  if FColumns.Count = 0 then
    Exit;

  if not CheckDataSet then
    Exit;

  if FAllFields.Count = 0 then
    Exit;

  if Assigned(FOnDropDown) then
    FOnDropDown(Self);  

  FOldDropDirection := FDropDirection;
  FDataScroll := True;

  if AlwaysRefreshDropDownList then
  begin
    LoadFromListSource;

    if FAllFields.Count = 0 then
    begin
      FDataScroll := False;
      Exit;
    end;
  end;  

  P := Point(0, 0);
  P := Self.ClientToScreen(P);

  if P.y + FDropHeight >= GetSystemMetrics(SM_CYSCREEN) then
    FDropDirection := ddUp;

  if P.y - FDropHeight <= 0 then
    FDropDirection := ddDown;

  FChkForm := TDropForm.CreateNew(self,0);
  FChkForm.Visible := False;
  FChkForm.Sizeable := FDropSizeable;
  FChkForm.BorderStyle := bsNone;
  FChkForm.FormStyle := fsStayOnTop;

  FChkForm.Font.Assign(Self.Font);

  if (FDropWidth = 0) then
  begin
    FChkForm.Width := Self.Width;
  end
  else
  begin
    FChkForm.Width := FDropWidth;
  end;
  
  FChkForm.Height := FDropHeight;
  FChkForm.OnDeactivate := FormDeactivate;

  FStringGrid := TInplaceStringGrid.Create(FChkForm);
  FStringGrid.Parent := FChkForm;
  FStringGrid.Align := alClient;
  LoadGridOptions;

  if FSortColumns >= FColumns.Count then
    FSortColumns := 0;

  SetSortMethod;  

  // FSensSorted := stAscendent;
  //tmpix := TFindList(FAllfields.Items[ItemIndex]).BaseIndex;

  if (FDropSorted) then
    FAllfields.Sort(SortField);

  if ShowGridTitleRow then
    FStringGrid.RowCount := FAllfields.Count + 1
  else
    FStringGrid.RowCount := FAllfields.Count;

  FStringGrid.ColCount := FColumns.Count;

  HeaderHeight := 0;
  
  for i := 0 to FColumns.Count - 1 do
    cw[i] := 0;

  k := 0;
  
  for i := 0 to FColumns.Count - 1 do
  begin
    if FShowGridTitleRow then
    begin
      FStringGrid.Cells[i,0] := FColumns.Items[i].FListField;
      FStringGrid.RowHeights[0] := Height;
      k := 1;
    end;
    
    FStringGrid.ColWidths[i] := FColumns.Items[i].FWidth;

    txt := AnsiUpperCase(self.Text);

    for j := 0 to FAllfields.Count - 1 do
    begin
       s := AnsiUpperCase(TFindlist(FAllfields.items[j]).Strings[FLookUpColumn]);   // FSortColumns rep by FLookUpColumn
       if txt = s then
       begin
         FItemIndex := j;
       end;

      FStringGrid.Cells[i,j + k] := TFindList(FAllfields.Items[j]).Strings[i];

      if FColumns[i].AutoSize then
      begin
        dr := Rect(0,0,100,100);
        FStringGrid.Canvas.Font.Assign(FColumns.Items[i].Font);
        DrawTextEx(FStringGrid.Canvas.Handle, PChar(FStringGrid.Cells[i,j + k]), Length(FStringGrid.Cells[i,j + k]),dr, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or DT_NOPREFIX, nil);
        if (dr.Right > cw[i]) then
          cw[i] := dr.Right;
      end;

      FStringGrid.RowHeights[j] := Height;

      //**************
      //if TFindList(FAllfields.Items[j]).BaseIndex = tmpix then
      //  FItemIndex := j;
      //**************
    end;

    if GridHeaderAutoSize then
    begin
      dr := Rect(0,0,100,100);
      FStringGrid.Canvas.Font.Assign(FColumns.Items[i].TitleFont);
      if (FColumns.Items[i].Title <> '') then
        s := FColumns.Items[i].Title
      else
        s := FColumns.Items[i].FListField;
      DrawTextEx(FStringGrid.Canvas.Handle, PChar(s), Length(s),dr, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or DT_NOPREFIX, nil);

      if (FColumns[i].TitleOrientation = toVertical) then
        dr.Bottom := dr.Right;

      dr.Bottom := dr.Bottom {+ 8};

      if (HeaderHeight < dr.Bottom) then
        HeaderHeight := dr.Bottom;
    end;
  end;

  for i := 0 to FColumns.Count - 1 do
  begin
    if FColumns[i].AutoSize then
      FStringGrid.ColWidths[i] := cw[i] + 4;
  end;

  if FShowGridTitleRow and (FStringGrid.RowCount > 1) then
    FStringGrid.FixedRows := 1
  else
    FStringGrid.FixedRows := 0;

  FStringGrid.Font.Assign(FDropFont);
  FStringGrid.ParentEdit := Self;
  FStringGrid.TabStop := True;

  if FGridLines then
    FStringGrid.GridLineWidth := 1
  else
    FStringGrid.GridLineWidth := 0;

  FStringGrid.DefaultRowHeight := FGridRowHeight;

  if FShowGridTitleRow then
  begin
    if GridHeaderAutoSize then
      FStringGrid.RowHeights[0] := HeaderHeight
    else
      FStringGrid.RowHeights[0] := FGridHeaderHeight;
  end;

  P := Point(0, 0);
  P := ClientToScreen(P);
  FChkForm.Left := P.x;

  //--- adjusting pos in visible region
  R := Rect(-1, -1, -1, -1);

  mon := Screen.MonitorFromPoint(p);
  if Assigned(mon) then
    R := mon.WorkAreaRect
  else
    SystemParametersInfo(SPI_GETWORKAREA, 0, @R, 0);

  if (R.Left >= 0) then
  begin
    if (R.Right < FChkForm.Left + FChkForm.Width) and (R.Right > FChkForm.Left) then
    begin
      P2 := Point(Width - 2, 0);
      P2 := ClientToScreen(P2);
      if (R.Right > P2.X) then
        FChkForm.Left := P2.X - FChkForm.Width
      else
        FChkForm.Left := FChkForm.Left - ((FChkForm.Left + FChkForm.Width) - R.Right);
    end;
    p.x := FChkForm.Left;
  end;
  //---

  if (FDropDirection = ddDown) then
    FChkForm.Top := P.y + self.Height
  else
    FChkForm.Top := P.y - FDropHeight;

  {$IFDEF DELPHI9_LVL}
  FChkForm.Width := 0;
  FChkForm.Height := 0;
  {$ENDIF}

  FStringGrid.Visible := True;

  FChkForm.Show;

  {$IFDEF DELPHI9_LVL}
  FChkForm.Left := p.x;
  if (FDropDirection = ddDown) then
    FChkForm.Top := P.y + self.Height
  else
    FChkForm.Top := P.y - FDropHeight;
  FChkForm.Width := FDropWidth;
  FChkForm.Height := FDropHeight;
  {$ENDIF}

  FChkForm.OnResize := OnDropDownResize;

  if (FDropWidth = 0) then
  begin
    FChkForm.Width := GetGridColumnsWidth + FStringGrid.ColCount + 4;
    if (FStringGrid.RowCount > FStringGrid.VisibleRowCount) then
    begin
      sbw := GetSystemMetrics(SM_CXVSCROLL);
      FChkForm.Width := FChkForm.Width + sbw;
    end;
    if DropSizeable then
      FChkForm.Width := FChkForm.Width + 8;
  end;

  if ((FDropWidth > 0) or DropSizeable) and (FDropStretchColumn >= 0) and (FDropStretchColumn < FStringGrid.ColCount) then
  begin
    FStringGrid.OnResize := OnGridResize;
    UpdateDropStretchColumnWidth;
  end;

  if Focus then
    FStringGrid.SetFocus;

  FStringGrid.Height := FStringGrid.Height + 1;
  FStringGrid.Height := FStringGrid.Height - 1;
  FDropDirection := FOldDropDirection;
  FChkClosed := False;
  FDataScroll := False;

  if FShowGridTitleRow then
    FStringGrid.Row := Min(FItemIndex + 1, FStringGrid.RowCount - 1) //row=0=> header=>+1
  else
    FStringGrid.Row := Min(FItemIndex, FStringGrid.RowCount - 1);

  if Assigned(FBookmark) then
    if FListDataLink.DataSource.DataSet.BookmarkValid(FBookmark) then
      FListDataLink.DataSource.DataSet.GotoBookmark(FBookmark);

  FChkForm.FDroppedDown := True;
end;

procedure TAdvDBLookupComboBox.UpdateLookup;
begin
  try
    if CheckEditDataSet and (not FDataScroll) and FAccept and Assigned(FDataSourceLink.Datasource.DataSet) and FDataSourceLink.Datasource.DataSet.Active then
    begin
      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar('MODIFY IN TO:'+FListDataLink.Datasource.DataSet.FieldByName(FKeyField).DisplayText));
      {$ENDIF}
      try
        if FListDataLink.Datasource.DataSet.FieldByName(FKeyField).IsNull then
          FDataSourceLink.Datasource.DataSet.FieldByName(FDataField).Clear
        else
          FDataSourceLink.Datasource.DataSet.FieldByName(FDataField).AsVariant :=
            FListDataLink.Datasource.DataSet.FieldByName(FKeyField).AsVariant;
    
        FOldItemIndex := Itemindex;
        FAccept := False;
        Modified := True;
      except
        on e:Exception do
          MessageDlg(e.Message, mtWarning, [mbYes], 0);
      end;
    end
    else if FAccept then
    begin
      FAccept := False;

      if not Modified and Assigned(FListDataLink.DataSource) and Assigned(FListDataLink.DataSource.DataSet) and FListDataLink.DataSource.DataSet.Active then
        Modified := True;
    end;

  finally
    if FAccept then
      FAccept := False;
  end;
end;

procedure TAdvDBLookupComboBox.HideGridList;
var
  i: integer;
begin
  if FChkClosed then
    Exit;

  if FGridColumnSize then
  begin
    for i := 0 to FStringGrid.ColCount -1 do
    begin
      Columns[i].Width := FStringGrid.ColWidths[i];
    end;
  end;

  DoCloseUp;

  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar(FListDataLink.Datasource.DataSet.FieldByName(FKeyField).DisplayText));
  {$ENDIF}

  FDisableChange := true; // suppress OnChange

  //FChkClosed := True;
  //Change;
  //FChkClosed := False;

  try
    if FSelectionChanged or (ItemIndex <> FOldItemIndex) then
    begin
      UpdateLookup;
      //if Assigned(DataSource) and Assigned(FDataSourceLink.DataSet) and FDataSourceLink.DataSet.Active and not FDataSourceLink.ReadOnly then
      if CanModify then
        FDataSourceLink.Modify;
    end
    else
    begin
      if FAccept then
        FAccept := False;
      FOldItemIndex := Itemindex;
    end;
  finally
    //Change;
    PostMessage(FChkForm.Handle,WM_CLOSE,0,0);

    DoClosed;

    FDisableChange := false; // suppress OnChange
    FChkClosed := True;

    if CheckDataSet and CanModify and not (FDataSourceLink.DataSet.State in [dsInsert, dsEdit]) and (FOldItemIndex2 >= 0) then // FF: Abort call in Dataset.beforeEdit
      ItemIndex := FOldItemIndex2;


    FSelectionChanged := False;
  end;

  DoChange;
end;

procedure TAdvDBLookupComboBox.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
  Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FButton.Width - 3;

  if self.BorderStyle = bsNone then
   begin
    Loc.Top := 2;
    Loc.Left := 2;
   end
  else
   begin
    Loc.Top := 1;
    Loc.Left := 1;

    if not ctl3d then
      loc.Left := loc.Left + 1;
   end;
  SendMessage(Handle, EM_SETRECTNP, 0, LParam(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
end;


procedure TAdvDBLookupComboBox.WMSize(var Message: TWMSize);
var
  MinHeight, Dist, d: Integer;

begin
  inherited;

  if BorderStyle = bsNone then
    Dist := 2
  else
    Dist := 4;

  MinHeight := GetMinHeight;
  { Windows text edit bug: if size to less than minheight, then edit ctrl does
    not display the text }

  if ctl3D then
    d := 0
  else
    d := 1;

  if Height < MinHeight then
    Height := MinHeight
  else
  if FButton <> nil then
  begin
    if NewStyleControls and Ctl3D then
      FButton.SetBounds(Width - 21 + 0, 0, 17, Height - Dist)
    else
      FButton.SetBounds (Width - FButton.Width - d, 1, FButton.Width, Height - 3);
    SetEditRect;
  end;
end;


function TAdvDBLookupComboBox.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I := SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I := Metrics.tmHeight;
  Result := Metrics.tmHeight + I div 4 {+ GetSystemMetrics(SM_CYBORDER) * 4};
end;

procedure TAdvDBLookupComboBox.WMPaste(var Message: TWMPaste);
var
  ch: Char;
begin
  inherited;

  FCurrentSearch := Text;
  ch := #0;
  Keypress(ch);
end;

procedure TAdvDBLookupComboBox.WMCut(var Message: TWMPaste);
var
  ch: Char;
begin
  inherited;

  FCurrentSearch := Text;
  ch := #0;
  Keypress(ch);
end;


procedure TAdvDBLookupComboBox.WMChar(var Msg: TWMChar);
begin
  if Msg.CharCode = Ord(#13) then
    Msg.Result :=0
  else
    inherited;
end;

procedure TAdvDBLookupComboBox.WMKeyDown(var Msg:TWMKeydown);
begin
  inherited;
  if (msg.CharCode in [VK_DOWN, VK_F4]) or ((msg.CharCode in [Ord('A')..Ord('Z'), Ord('1')..Ord('9')]) and (DropDownType = ddAuto)) then//(msg.CharCode = VK_DOWN) or (msg.CharCode = VK_F4) then
  begin
    if FChkClosed then
      ShowGridList(true);
  end;
  if (msg.CharCode = VK_ESCAPE) then
  begin
    if Assigned(Parent) then
    SendMessage(Parent.Handle, CM_DIALOGKEY , ord(VK_ESCAPE),0);

    if Assigned(DataSource) and Assigned(FDataSourceLink.DataSet) and FDataSourceLink.DataSet.Active and (FDataSourceLink.DataSet.State in [dsEdit, dsInsert]) then
    begin
      FDataSourceLink.DataSet.Cancel;
      Modified := False;
    end;  
  end;
  
  if (msg.CharCode = VK_RETURN) and Assigned(Parent) then
    SendMessage(Parent.Handle, CM_DIALOGKEY , ord(VK_RETURN),0);

  if (msg.charcode = VK_RETURN) and (FReturnIsTab) then
  begin
    msg.charcode := VK_TAB;
    if IsWindowVisible(self.Handle) then
      PostMessage(self.Handle, WM_KEYDOWN, VK_TAB, 0);
  end;
end;


procedure TAdvDBLookupComboBox.WMSysKeyDown(var Msg: TWMKeydown);
begin
  inherited;
  if (msg.CharCode = VK_DOWN) and (GetKeyState(VK_MENU) and $8000 = $8000)  then
    ShowGridList(true);
end;


procedure TAdvDBLookupComboBox.CMExit(var Message: TCMExit);
begin
  if (DropDownType = ddAuto) then
    HideGridList;
  inherited;
end;

procedure TAdvDBLookupComboBox.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if LabelCaption <> '' then
    FLabel.Visible := Self.Visible;
end;

procedure TAdvDBLookupComboBox.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;

  inherited;

  if CheckDataSet then
  begin
    if (LookupLoad = llOnNeed) and (FAllFields.Count = 0) then
    begin
      LoadFromListSource;
      if Assigned(DataSource) and Assigned(FDataSourceLink.DataSet) and FDataSourceLink.DataSet.Active then
        FDataSourceLink.Modify;
    end;
  end;

  if (DropDownType = ddAuto) then
    ShowGridList(True);
end;

function TAdvDBLookupComboBox.GetLookupDataField: TField;
begin
  Result := nil;
  if CheckDataSet and Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active and (DataField <> '') then
    Result := DataSource.DataSet.Fields.FieldByName(DataField);
end;

function TAdvDBLookupComboBox.GridToString: string;
var
  fld,lfld: TField;
  LookupFields: string;
  VariantResult, DefaultVal: Variant;
  s:string;
  found: Boolean;
begin
  if (csLoading in ComponentState) then
    Exit;

  Result := '';
  try
    if FColumns.Count > 0 then
    begin
      if CheckDataSet then
      begin
        if Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active {(1>0) {(FAllFields.Count = 0) and (LookupLoad = llOnNeed)} then
        begin
          fld := DataSource.DataSet.Fields.FieldByName(DataField);
          if Assigned(fld) then
          begin
            FInLookup := True;

            VariantResult := '';
            if (fld.DataType = ftString) then
              DefaultVal := ''
            else
              DefaultVal := null;

            if (FilterField <> '') and (FilterValue <> '') then
            begin
              LookupFields := KeyField + ';' + FilterField;

              VariantResult := SecureLookup(ListSource.DataSet, fld, LookupFields, VarArrayOf([fld.AsVariant,FilterValue]), VarArrayOf([DefaultVal,FilterValue]), FColumns.Items[FLookUpColumn].FListField);
            end
            else
            begin
              LookupFields := KeyField;

              lfld := ListSource.DataSet.FieldByName(FColumns.Items[FLookUpColumn].FListField);

              if Assigned(lfld) and (lfld.FieldKind = fkCalculated) then
              begin
                ListSource.DataSet.DisableControls;
                found := True;
                if (fld.AsString <> '') then                
                  found := ListSource.DataSet.Locate(LookupFields, fld.AsString, [loPartialKey]);
                s := '';
                if Assigned(lfld.OnGetText) then
                  lfld.OnGetText(lfld, s, true)
                else if (fld.AsString <> '') and found then                     
                  s := lfld.DisplayText
                else
                  s := '';
                variantresult := s;
                ListSource.DataSet.EnableControls;
              end
              else
                VariantResult := SecureLookup(ListSource.DataSet, fld, LookupFields, fld.AsVariant, DefaultVal, FColumns.Items[FLookUpColumn].FListField);
            end;

            Result := VariantResult;

            VariantResult := '';

            if LabelField <> '' then
            begin
              if (FilterField <> '') and (FilterValue <> '') then
              begin
                LookupFields := KeyField + ';' + FilterField;

                VariantResult := SecureLookup(ListSource.DataSet, fld, LookupFields, VarArrayOf([fld.AsVariant, FilterValue]), VarArrayOf([DefaultVal, FilterValue]), FLabelField);
              end
              else
              begin
                LookupFields := KeyField;

                VariantResult := SecureLookup(ListSource.DataSet, fld, LookupFields, fld.AsVariant, DefaultVal, FLabelField);
              end;

              {$IFDEF TMSDEBUG}
              s := VariantResult;
              outputdebugstring(pchar('gridtostring:'+s));
              {$ENDIF}

              LabelCaption := VariantResult;
            end;

            FInLookup := False;
          end;
        end
        else
        begin
          if Assigned(FAllfields) and (FItemIndex >= 0) and (FItemIndex < FAllFields.Count) and Assigned(FAllfields.Items[FItemIndex]) then
            Result := TFindList(FAllfields.Items[FItemIndex]).Strings[FLookUpColumn];  // 0 rep by FLookUpColumn

          //----
          //Result := TFindList(FAllfields.Items[FItemIndex]).Strings[0];
          //----

          //++++
          (* new code removed
          fld := DataSource.DataSet.Fields.FieldByName(DataField);
          if Assigned(fld) then
            Result := fld.DisplayText;
          *)  
          //++++
        end;
      end
      else
      begin
        Result := FColumns.Items[FLookUpColumn].FListField;
      end;
    end;
  except
    on Exception do
    FInLookup := False;
  end;

  if Assigned(OnGridListItemToText) then
    OnGridListItemToText(Self,Result);
end;


procedure TAdvDBLookupComboBox.FormDeactivate(Sender: TObject);
var
  pt: TPoint;
  r: TRect;
begin
  {Grid cursor here...}
  GetCursorPos(pt);
  pt := ScreenToClient(pt);
  r := ClientRect;
  r.left := r.right - 16;
  FCloseClick := PtInRect(r,pt);
  HideGridList;
end;

procedure TAdvDBLookupComboBox.Init;
var
  OldColor: TColor;
begin
  FNormalColor := Color;

  if not Enabled then
  begin
    OldColor := Color;
    Color := FDisabledColor;
    FNormalColor := OldColor;
  end;
end;


procedure TAdvDBLookupComboBox.Loaded;
begin
  inherited;

  if not (csDesigning in ComponentState) then
    Init;

  if FLabel <> nil then
    UpdateLabel;

  UpdateText(GridToString);

end;

procedure TAdvDBLookupComboBox.DownClick(Sender: TObject);
begin
  if FChkClosed then
  begin
    if not FCloseClick then
      ShowGridList(true);
  end;
  FCloseClick := False;
  if Assigned(FOnClickBtn) then
    FOnClickBtn(Self);
end;

procedure TAdvDBLookupComboBox.MouseClick(Sender: TObject);
begin
  if not FChkClosed then
  begin
    HideGridList;
  end;
end;

procedure TAdvDBLookupComboBox.SetDropFont(const Value: TFont);
begin
  FDropFont.Assign(Value);
end;

function TAdvDBLookupComboBox.GetText: string;
begin
  Result := inherited Text;
end;

procedure TAdvDBLookupComboBox.SetText(const Value: string);
var
  i,fndmatch: Integer;
  txt, s: string;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) or (csReading in ComponentState) then
    SetInternalText(Value)
  else
  begin
    if Value <> Text then
      inherited Text := Value;

    if (Value = '') then
    begin
      FCurrentSearch := '';
    end;

    txt := AnsiUpperCase(Text);
    fndmatch := -1;

    for i := 0 to FAllfields.Count - 1 do
    begin
      s := AnsiUpperCase(TFindlist(FAllfields.Items[i]).Strings[FLookUpColumn]);

      if (AnsiPos(txt,s) = 1) and (fndmatch = -1) then // keep reference to first partial match
        fndmatch := I;

      if (txt = s) then // override & stop when found a full match
      begin
        fndmatch := i;
        break;
      end;
    end;

    if (fndmatch <> -1) then
    begin
      try
        FDisableChange := True;
        ItemIndex := fndmatch;
        FAccept := True;
        UpdateLookup;
      finally
        FDisableChange := False;
      end;
      Change;
    end;
  end;
end;

procedure TAdvDBLookupComboBox.SetInternalText(const Value: string);
begin
  //if (Datasource<>nil) and (Datasource.DataSet <> nil) and (DataSource.DataSet.Active) then
  begin
    if Value <> Text then
      inherited Text := Value;

    if (Value = '') then
    begin
      FCurrentSearch := '';
    end;
  end;
end;

function TAdvDBLookupComboBox.CheckDataSet: boolean;
begin
  Result := False;
  if not Assigned(FListDataLink) then
    Exit;
  if not Assigned(FListDataLink.Datasource) then
    Exit;
  if not Assigned(FListDataLink.Datasource.DataSet) then
    Exit;
  if not Assigned(FDataSourceLink) then
    Exit;
  //if not Assigned(FDataSourceLink.Datasource) then
    //Exit;
  //if not Assigned(FDataSourceLink.Datasource.DataSet) then
    //Exit;
  if not FListDataLink.Datasource.DataSet.Active then
    Exit;
  //if not FDataSourceLink.Datasource.DataSet.Active then
    //Exit;
    Result := True;
end;

procedure TAdvDBLookupComboBox.SetActive(Active: boolean);
var
  i:integer;
  df:string;
begin
  DestroyBookMark;
  
  for i := 0 to FAllfields.count-1 do
    Tfindlist(FAllfields.Items[i]).Free;
  FAllfields.Clear;

  if  Active then
    if not (csLoading in ComponentState)
       then LoadFromListsource;

  if not CheckDataSet then
   begin
     SetInternalText('');
     Exit;
   end;

  if Assigned(DataSource) and Assigned(DataSource.dataSet) and DataSource.dataSet.Active and (DataField <> '') then
  begin
    df := DataSource.DataSet.FieldByName(DataField).DisplayText;
    for i := 0 to FAllfields.Count - 1 do
      if TFindList(FAllfields.Items[i]).KeyField = df then
      begin
        Itemindex := i;
        Exit;
      end;
  end;

  if LookupLoad <> llOnNeed then
    SetInternalText('');
end;

function TAdvDBLookupComboBox.GetListsource: TDatasource;
begin
  Result := FListDataLink.Datasource;
end;

procedure TAdvDBLookupComboBox.SetListsource(const Value: TDatasource);
begin
  if (Value = nil) then
    SetActive(false);

  if (FListDataLink.Datasource <> Value) then
  begin
    if (FDataSourceLink.Datasource = VALUE) AND (Value <> nil) then
      raise Exception.Create('Circular referance: ' + Value.Name);
    FListDataLink.Datasource := Value;
  end;

  if (Value <> nil) then
    Value.FreeNotification(Self)
  else
    UpdateLabel;  
end;

function TAdvDBLookupComboBox.GetColumnField(ACol: Integer): TField;
begin
  Result := nil;
  if (ACol >= 0) and (ACol < Columns.Count) and Assigned(FListDataLink.DataSource)
    and Assigned(FListDataLink.DataSource.DataSet) and (FListDataLink.DataSource.DataSet.Active) then
  begin
    Result := FListDataLink.DataSource.DataSet.FieldByName(Columns.Items[ACol].ListField);
  end;  
end;

procedure TAdvDBLookupComboBox.StringGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  stl,ofsy,i,e:integer;
  s:string;
  aField: TField;
  bg: TColor;
  tf: TFont;
  lf: TLogFont;
  TextP: TPoint;
  ts: TSize;
begin
  FStringGrid.Canvas.Pen.Width := 1;

  if (ARow = 0) and FShowGridTitleRow then
  begin
    if FColumns.Items[ACol].FTitleFontChanged or not Self.ParentFont then
    FStringGrid.Canvas.Font.Assign(FColumns.Items[ACol].TitleFont)
  else
      FStringGrid.Canvas.Font.Assign(Self.Font);
  end
  else
  begin
    if FColumns.Items[ACol].FFontChanged then
      FStringGrid.Canvas.Font.Assign(FColumns.Items[ACol].Font)
    else if Self.ParentFont then
      FStringGrid.Canvas.Font.Assign(Self.Font)
    else
      FStringGrid.Canvas.Font.Assign(FDropFont);
  end;

  if gdFixed in state then
  begin
    if (FColumns.Items[ACol].FixedColor <> clNone) and (FColumns.Items[ACol].FixedColorTo <> clNone) then
      DrawGradient(FStringGrid.Canvas, FColumns.Items[ACol].FixedColor, FColumns.Items[ACol].FixedColorTo, 40, Rect, FColumns.Items[ACol].GradientDir = gdHorizontal)
    else
    begin
      FStringGrid.Canvas.Brush.Color := FColumns.Items[ACol].FixedColor; //FHeaderColor;
      FStringGrid.Canvas.FillRect(Rect);
    end;
    rect.Left := rect.Left + 1;
    if (Acol = FSortColumns) and FDropSorted then
    begin
      ofsy := rect.top + ((rect.Bottom - rect.top)-FbitmapUP.Height) div 2;
      if FSensSorted = stAscendent then
        FStringGrid.Canvas.Draw(rect.Left + 1,ofsy,FbitmapDown)
      else
        FStringGrid.Canvas.Draw(rect.Left + 1,ofsy,FbitmapUp);

      rect.Left := rect.Left + FBitmapdown.Width + 3; //Draw text after bitmap
    end;
  end
  else
  begin
    FStringGrid.Canvas.Brush.Color := FColumns.Items[ACol].FColor;
    if FColumns.Items[ACol].FFontChanged then
      FStringGrid.Canvas.Font.Assign(FColumns.Items[ACol].Font)
    else if Self.ParentFont then
      FStringGrid.Canvas.Font.Assign(Self.Font)
    else
      FStringGrid.Canvas.Font.Assign(FDropFont);


    if Assigned(FOnDrawProp) then
    begin
      AField := GetColumnField(ACol);
      bg := FStringGrid.Canvas.Brush.color;
      FOnDrawProp(self, ARow, ACol, AField, FStringGrid.Cells[ACol,ARow], FStringGrid.Canvas.Font, bg);
      FStringGrid.Canvas.Brush.color := bg;
      FStringGrid.Canvas.Pen.Color := bg;
    end;

    if (ARow = FHoveredRow) then
    begin
      if (FHoverColor <> clNone) then
        FStringGrid.Canvas.Brush.Color := FHoverColor;
      if (FHoverTextColor <> clNone) then
        FStringGrid.Canvas.Font.Color := FHoverTextColor;
    end;

    if (gdSelected in state)  then
    begin
      FStringGrid.Canvas.Brush.Color := FSelectionColor;
      FStringGrid.Canvas.Font.Color := FSelectionTextColor;
    end;

    FStringGrid.Canvas.FillRect(Rect);
  end;

  if (gdFixed in State) then
  begin
    case FColumns.Items[ACol].TitleAlignment of
      taLeftJustify :  Stl := DT_LEFT;
      taRightJustify : Stl := DT_RIGHT;
      taCenter       : Stl := DT_CENTER;
      else
        Stl := DT_LEFT;
    end;

    case FColumns.Items[ACol].TitleVerticalAlignment of
      tvaTop    : Stl := stl or DT_TOP;
      tvaBottom : Stl := stl or DT_BOTTOM;
      tvaCenter : Stl := stl or DT_VCENTER;
      else
        Stl := DT_VCENTER;
    end;
  end
  else
  begin
    case FColumns.Items[ACol].FAlignment of
    taLeftJustify :  Stl := DT_LEFT;
    taRightJustify : Stl := DT_RIGHT;
    taCenter       : Stl := DT_CENTER;
    else
      Stl := DT_LEFT;
    end;
  end;

  rect.Right := rect.Right - 2;
  rect.Left := rect.Left + 2;

  if (gdFixed in State) then
    stl := stl or DT_SINGLELINE or DT_END_ELLIPSIS or DT_NOPREFIX
  else
    stl := stl or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS or DT_NOPREFIX;

  if (ARow = 0) and FShowGridTitleRow then
  begin
    if FColumns[ACol].Title <> '' then
      s := FColumns[ACol].Title
    else
      s := FColumns[ACol].FListField
  end
  else
     s := FStringGrid.Cells[acol,arow];

  FStringGrid.Canvas.Brush.Style := bsClear;

  if (FColumns[ACol].ColumnType = ctImage) and Assigned(FImages) and not (gdFixed in State) then
  begin
    val(s,i,e);
    FImages.BkColor := FStringGrid.Canvas.Brush.Color;
    FImages.DrawingStyle := dsTransparent;
    FImages.Draw(FStringGrid.Canvas, rect.Left, rect.Top, i);
  end
  else
  begin
    if (gdFixed in State) then
    begin
      if (FColumns[ACol].TitleOrientation = toVertical) then
      begin
        ts.cx := FStringGrid.Canvas.TextHeight(s);
        ts.cy := FStringGrid.Canvas.TextWidth(s);
        //DrawTextEx(FStringGrid.Canvas.Handle, PChar(s), Length(s),dr, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or DT_NOPREFIX, nil);

        tf := TFont.Create;
        try
          FillChar(lf, SizeOf(lf), 0);
          tf.Assign(FStringGrid.Canvas.Font);
          GetObject(tf.Handle, SizeOf(Lf), @Lf);

          lf.lfEscapement := 900;
          lf.lfOrientation := 900;

          tf.Handle := CreateFontIndirect(Lf);
          FStringGrid.Canvas.Font.Assign(tf);
        finally
          tf.Free;
        end;

        case FColumns.Items[ACol].TitleAlignment of
          taLeftJustify : TextP.X := rect.Left {+ Ts.cx};
          taRightJustify : TextP.X := rect.Right - ts.cx;
          taCenter       : TextP.X := rect.Left {+ ts.cx }+ (rect.Right - rect.Left - Ts.cx) div 2;
        end;

        case FColumns.Items[ACol].TitleVerticalAlignment of
          tvaTop    : TextP.Y := rect.Top + ts.cy;
          tvaBottom : TextP.Y := rect.Bottom;
          tvaCenter :
          begin
            TextP.Y := rect.Bottom - (rect.Bottom - rect.Top - Ts.cy) div 2;
            TextP.Y := Min(TextP.Y, rect.Bottom);
          end;
        end;

        FStringGrid.Canvas.TextOut(TextP.X, Textp.Y, s);
        //DrawText(FStringGrid.Canvas.Handle, pchar(s), length(s), rect, stl);
      end
      else
      begin
        DrawText(FStringGrid.Canvas.Handle, pchar(s), length(s), rect, stl);
      end;
    end
    else
      DrawText(FStringGrid.Canvas.Handle,pchar(s),length(s),rect,stl);
  end;
end;

procedure TAdvDBLookupComboBox.LoadGridOptions;
begin
  FCurrentSearch := '';
  FStringGrid.Options:= [goFixedVertLine,
                         goFixedHorzLine,
                         goVertLine,
                         goHorzLine,
                         goDrawFocusSelected,
                         goTabs,
                         goRowSelect];

  if FGridColumnSize then
    FStringGrid.Options := FStringGrid.Options + [goColSizing];                        

  FStringGrid.Left := 0;
  FStringGrid.Top := 0;
  FStringGrid.Width := FDropWidth;
  FStringGrid.Height := FDropHeight;
  FStringGrid.Color := FDropColor;
  FStringGrid.ColCount := FColumns.Count;
  FStringGrid.FixedCols := 0;

  if ShowGridTitleRow then
    FStringGrid.RowCount := 2
  else
    FStringGrid.RowCount := 1;

  FStringGrid.DefaultDrawing := False;
  FStringGrid.GridLineWidth := 1;

  FStringGrid.OnDrawCell := StringGridDrawCell;
  FStringGrid.OnMouseUp := Gridmousedown;
  FStringGrid.OnMouseMove := GridMouseMove;
  FStringGrid.OnKeyPress := StringGridKeyPress;
  FStringGrid.OnSelectCell := StringGridSelectCell;
end;

function TAdvDBLookupComboBox.GetItemIndex: integer;
begin
  Result := FItemIndex;
end;

procedure TAdvDBLookupComboBox.SetItemIndex(Value: integer);
begin
  if FColumns.Count > 0 then
  begin
    if Value >= FAllfields.Count then
      Value := FAllfields.Count
  end
  else
    Value := 0;

  if Value < 0 then Value := 0;

  FItemIndex := Value;
  if FAllFields.Count = 0 then
  begin
    Exit;
  end;

  try
    //Text := GridToString;
    if Assigned(FAllfields) and (FItemIndex >= 0) and (FItemIndex < FAllFields.Count) and Assigned(FAllfields.Items[FItemIndex]) then
      SetInternalText(TFindList(FAllfields.Items[FItemIndex]).Strings[FLookUpColumn]);   // 0 rep by FLookUpColumn
  except
    on Exception do;
  end;

  if FChkClosed or FInternalCall then
    Exit;
  try
    if FShowGridTitleRow then
      FStringGrid.Row := FItemIndex + 1
    else
      FStringGrid.Row := FItemIndex;
  except
    on Exception do;
  end;
end;

function TAdvDBLookupComboBox.GetLabelCaption: string;
begin
  if FLabel <> nil then
    Result := FLabel.Caption
  else
    Result := '';
end;

procedure TAdvDBLookupComboBox.SetLabelAlwaysEnabled(const Value: Boolean);
begin
  FLabelAlwaysEnabled := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvDBLookupComboBox.SetLabelCaption(const Value: string);
begin
  if FLabel = nil then
     FLabel := CreateLabel;
  FLabel.Caption := Value;
  UpdateLabel;
end;

procedure TAdvDBLookupComboBox.SetLabelFont(const Value: TFont);
begin
  FLabelFont.Assign(Value);
end;

procedure TAdvDBLookupComboBox.SetLabelMargin(const Value: Integer);
begin
  FLabelMargin := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvDBLookupComboBox.SetLabelPosition(const Value: TLabelPosition);
begin
  FLabelPosition := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvDBLookupComboBox.SetLabelTransparent(const Value: Boolean);
begin
  FLabelTransparent := Value;
  if FLabel <> nil then UpdateLabel;
end;


procedure TAdvDBLookupComboBox.GridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  r,c,i,j: Integer;
begin
  FStringGrid.MouseToCell(x,y,c,r);
  if (c < 0) or (r < 0) then // click outside grid cells
    Exit;

  if (r > 0) or ((r = 0) and not FShowGridTitleRow)then
  begin
    FAccept := True;
    // ItemIndex := r - 1;
    if not FGridCellNotSelected then
      HideGridList;
  end
  else
  begin
    r := TFindList(FAllfields.Items[ItemIndex]).BaseIndex;
    if FSortColumns = c then
    begin
      if FSensSorted = stAscendent then
        FSensSorted := stDescendent
      else
        FSensSorted := stAscendent;
    end
    else
    begin
      FSensSorted := stAscendent;
      FSortColumns := c;
      FSortColumn := Columns[c].Name;
    end;
    SetSortMethod;
    if (FDropSorted) then
      FAllfields.Sort(sortfield);

    for i:=0 to fColumns.Count-1 do
    begin
      for j:=0 to FAllfields.Count-1 do
      begin
        FStringGrid.Cells[i,j+1] := TFindList(FAllfields.Items[j]).Strings[i];
        if TFindList(FAllfields.Items[j]).BaseIndex = r then
        begin
          ItemIndex := j;
        end;
      end;
    end;
    FStringGrid.Invalidate;
  end;
end;

procedure TAdvDBLookupComboBox.StringGridKeyPress(Sender: TObject; var Key: Char);
var
  i,lx, p: Integer;
  s: string;
  OldSearch: string;
  ColumnArr: array of Integer;
  j: Integer;
  k: Integer;
begin
  if not CheckDataSet then
    Exit;

  if FChkClosed then
    Exit;

  if (Key = #13) then
  begin
    FAccept := True;
    HideGridList;
    Exit;
  end;

  if FColumns.Count = 0 then
    Exit;

  lx := -1;

  OldSearch := FCurrentSearch;

  if (Key = #8) then
  begin
    if (Length(FCurrentSearch) > 0) then
      Delete(FCurrentSearch,Length(FCurrentSearch),1)
  end
  else
    FCurrentSearch := AnsiUpperCase(FCurrentSearch + Key);

  if FLookupInAllColumns then
    setlength( ColumnArr, FColumns.Count )
  else
    setlength( ColumnArr, 1 );
  try
    ColumnArr[ 0 ] := FLookupColumn;
    if FLookupInAllColumns then
    begin
      // MK: collect all columns to array, FLookupColumn first, after that the rest
      j := 0;
      for i := 0 to high( ColumnArr ) do
      begin
        if i <> FLookupColumn then
          ColumnArr[ j ] := i;
        Inc(j);
      end;
    end;
    for i := 0 to FAllfields.Count - 1 do
    begin
      for j := 0 to high( ColumnArr ) do // MK: 
      begin
        k := columnarr[ j ];
        s := AnsiUpperCase(TFindlist(FAllfields.items[i]).Strings[ k ]);   // FSortColumns rep by FLookUpColumn

        OldSearch := FCurrentSearch;
        p := AnsiPos(FCurrentSearch,s);

        if ((FLookupSearch = isFirstChar) and (p = 1)) or ((LookupSearch = isAnyChar) and (p > 0)) then
        begin
          ItemIndex := i;

          FAccept := True;
          UpdateLookup;

          if Assigned(FOnLookupSuccess) then
            FOnLookupSuccess(Self,FCurrentSearch,s);

          FCurrentSearch := OldSearch;
          Exit;
        end;
      end;
      if AnsiPos(AnsiUpperCase(key),s)=1 then
        lx := i;
    end;

    if Assigned(FOnLookupError) then
      FOnLookupError(Self, FCurrentSearch);

    FCurrentSearch := OldSearch;

    if not (LookupMethod = lmFast) then
      Exit;

    if lx > -1 then
    begin
      FCurrentsearch := key;
      ItemIndex := lx;
    end
    else
    begin
      FCurrentSearch := '';
    end;
  finally
    ColumnArr := nil;
  end;
end;

function TAdvDBLookupComboBox.CanModify: Boolean;
var
  fld: TField;
begin
  fld := GetLookupDataField;
  Result := Assigned(DataSource) and Assigned(FDataSourceLink.DataSet) and FDataSourceLink.DataSet.Active and
            not FDataSourceLink.ReadOnly and FDataSourceLink.DataSet.CanModify and Assigned(fld) and fld.CanModify and
            (FDataSourceLink.DataSource.AutoEdit or (FDataSourceLink.DataSet.State in [dsInsert, dsEdit]));
end;

procedure TAdvDBLookupComboBox.StringGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  s: string;
  OldV: Boolean;
begin
  if FDataScroll or (not CanModify and Assigned(DataSource) and Assigned(FDataSourceLink.DataSet) and FDataSourceLink.DataSet.Active) then
    Exit;

  if Assigned(FOnGridSelectCell) then
  begin
    s := FStringGrid.Cells[aCol, aRow];
    FOnGridSelectCell(Self, ACol, ARow, s, CanSelect);
    FGridCellNotSelected := not CanSelect;
    if not CanSelect then
      Exit;
  end;

  if Assigned(FChkForm) and FChkForm.FDroppedDown then
    FSelectionChanged := True;

  FGridCellNotSelected := False;
  // compares (arow-1<>FitemIndex)

  OldV := FChkClosed;
  FChkClosed := True;
  if FShowGridTitleRow then
  begin
    if (ARow > 0) and (ARow - 1 <> FItemIndex) then
    begin
      FOldItemIndex2 := ItemIndex;
      ItemIndex := ARow - 1;
    end
    else if (ARow > 0) and (ItemIndex = 0) then
    begin
      FInternalCall := True;
      FOldItemIndex2 := ItemIndex;
      ItemIndex := ItemIndex;
      FInternalCall := False;
    end;
  end
  else
  begin
    if (ARow >= 0) and ((ARow <> FItemIndex) or (Text <> FStringGrid.Cells[ACol,ARow])) then
    begin
      FOldItemIndex2 := ItemIndex;
      ItemIndex := ARow;
    end
    else if (ARow >= 0) and (ItemIndex = 0) then
    begin
      FInternalCall := True;
      FOldItemIndex2 := ItemIndex;
      ItemIndex := ItemIndex;
      FInternalCall := False;
    end;
  end;
  FChkClosed := OldV;
  FCurrentSearch := '';

  if Assigned(FOnGridSelectedCell) then
  begin
    s := FStringGrid.Cells[aCol, aRow];
    FOnGridSelectedCell(Self, ACol, ARow, s, CanSelect);
  end;
end;

procedure TAdvDBLookupComboBox.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAdvDBLookupComboBox.DoClosed;
begin
  if Assigned(OnClosed) then
    OnClosed(Self);
end;

procedure TAdvDBLookupComboBox.DoCloseUp;
begin
  if Assigned(OnCloseUp) then
    OnCloseUp(Self);
end;

procedure TAdvDBLookupComboBox.DoEnter;
begin
  inherited;
end;

procedure TAdvDBLookupComboBox.Change;
var
  Fld: TField;
  s, LookupFields:string;
  VariantResult, DefaultVal: Variant;
begin
  if FListDataLink.FLoadingData then
    Exit;

  if not FDisableChange then
    inherited;

  if CheckDataSet and not FDataScroll then
  begin
    FDataScroll := True;

    if (LookupLoad = llOnNeed) and (FAllFields.Count = 0) then
      Exit;

    if Assigned(FBookmark) and (FChkClosed) then
    begin
      if FListDataLink.DataSource.DataSet.BookmarkValid(FBookMark) then
        FListDataLink.DataSource.DataSet.FreeBookmark(FBookMark);
      FBookMark := nil;
    end;

    FListDataLink.DataSource.DataSet.DisableControls;
    try
      with FListDataLink.DataSource.DataSet do
      begin
        First;
        while not Eof do
        begin
          if (FItemIndex >= FAllfields.Count) then
            Break;

          if (FieldByName(KeyField).DisplayText = TFindList(FAllfields.Items[FItemIndex]).KeyField) then
          begin
            if (FilterField <> '') and (FilterValue <> '') then
            begin
              fld := FieldByName(FilterField);
              if Assigned(fld) then
              begin
                if MatchStrEx(FilterValue,fld.DisplayText,False) then
                  Break;
              end
              else
                Break;
            end
            else
              Break;
          end;
          Next;

        end;

        if LabelField <> '' then
        begin
          if Text = '' then
            LabelCaption := ''
          else
          begin
            (* Fld := FieldByName(LabelField);

            if Assigned(Fld) then
            begin
              s := Fld.DisplayText;
              {$IFDEF TMSDEBUG}
              outputdebugstring(pchar('change:'+text+':'+s));
              {$ENDIF}
              LabelCaption := s;
            end;
            *)

            if Assigned(Self.DataSource) and Assigned(Self.DataSource.DataSet) and Self.DataSource.DataSet.Active and (Self.DataSource.DataSet.State in [dsInsert, dsEdit]) then
              fld := Self.DataSource.DataSet.Fields.FieldByName(DataField)
            else
              fld := nil;

            if Assigned(fld) then
            begin
              VariantResult := '';

              if (fld.DataType = ftString) then
                DefaultVal := ''
              else
                DefaultVal := null;

              if (FilterField <> '') and (FilterValue <> '') then
              begin
                LookupFields := KeyField + ';' + FilterField;

                VariantResult := SecureLookup(ListSource.DataSet, fld, LookupFields, VarArrayOf([fld.AsVariant,FilterValue]), VarArrayOf([DefaultVal,FilterValue]), FLabelField);
              end
              else
              begin
                LookupFields := KeyField;

                VariantResult := SecureLookup(ListSource.DataSet, fld, LookupFields, fld.AsVariant, DefaultVal, FLabelField);
              end;

              LabelCaption := VariantResult;
            end
            else
            begin
              Fld := FieldByName(LabelField);
              if Assigned(Fld) then
              begin
                s := Fld.DisplayText;
                LabelCaption := s;
              end;
            end;

          end;
        end;
      end;

      if FChkClosed then
        FBookmark := FListDataLink.DataSource.DataSet.GetBookmark;

    finally
      FListDataLink.DataSource.DataSet.EnableControls;
    end;

    FDataScroll := False;
  end;
end;


function TAdvDBLookupComboBox.GetDatasource: TDatasource;
begin
  Result := FDataSourceLink.Datasource;
end;

function TAdvDBLookupComboBox.GetDropDownRowCount: integer;
begin
  Result := FStringGrid.RowCount - FStringGrid.FixedRows;
end;

procedure TAdvDBLookupComboBox.SetDatasource(const Value: TDatasource);
begin
  if (FDataSourceLink.Datasource <> Value) then
  begin
    if (Value = FListDataLink.Datasource) AND (Value <> nil) then
      raise Exception.Create('Circular Referance: ' + Value.Name);
    FDataSourceLink.Datasource := Value;
  end;

  if (Value <> nil) then
    Value.FreeNotification(Self)
  else
    UpdateLabel;  
end;

function TAdvDBLookupComboBox.CheckEditDataSet: boolean;
begin
  Result := False;
  if FColumns.Count < 1 then
    Exit;

  if (FDataField = '') or (FKeyField = '') then
    Exit;
  if not CheckDataSet then  Exit;
  
  //if not Assigned(FDataSourceLink.Datasource) or not Assigned(FDataSourceLink.DataSource.DataSet) or not (FDataSourceLink.DataSource.DataSet.CanModify) then  Exit;
  if not CanModify then
    Exit;

  //FDataSourceLink.Datasource.Edit;
  Result := FDataSourceLink.Edit;
  //Result :=true;
end;

procedure TAdvDBLookupComboBox.SetSortColumns(const Value: Integer);
var
  tmpix:Integer;
begin
  if (Value >= FColumns.Count) or (Value < 0) then
    Exit;
  if (ItemIndex < 0) or (ItemIndex >= FAllfields.Count) then
    Exit;
  FSortColumns := Value;
  tmpix := TFindList(FAllfields.items[ItemIndex]).BaseIndex;
  SetSortMethod;
  if (FDropSorted) then
    FAllfields.Sort(SortField);
  ItemIndex := GetRealItemIndex(tmpix);
end;

function TAdvDBLookupComboBox.GetRealItemIndex(index: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FAllfields.Count - 1 do
  begin
    if TFindList(FAllfields.Items[i]).BaseIndex = Index then
    begin
      Result := i;
      Exit;
    end;
  end;
end;


procedure TAdvDBLookupComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_DELETE) then
  begin
    if SelLength > 0 then
    begin
      FCurrentSearch := AnsiUpperCase(Copy(Text,1,SelStart));
    end
    else
      FCurrentSearch := '';
  end;
end;

function TAdvDBLookupComboBox.FindField(Value:string): Boolean;
var
  i, p: Integer;
  s: string;
begin
  Result := False;
  for i := 0 to FAllfields.Count - 1 do
  begin
    s := AnsiUpperCase(TFindlist(FAllfields.Items[i]).Strings[FLookUpColumn]);   // 0 rep by FLookUpColumn
    p := AnsiPos(Value,s);
    if ((FLookupSearch = isFirstChar) and (p = 1)) or ((LookupSearch = isAnyChar) and (p > 0)) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TAdvDBLookupComboBox.KeyPress(var Key: Char);
var
  lx,i, OldSS, p: Integer;
  s: string;
  OldSearch: string;
begin

  if not EditorEnabled then
  begin
    Key := #0;
    Exit;
  end;

  if Key = #27 then
  begin
    inherited;
    Exit;
  end;

  if (Key = #8) then
  begin
    s := Text;
    OldSS := SelStart;
    system.Delete(s,SelStart,SelLength+1);
    FCurrentSearch := s;
    SetInternalText(s);
    Key := #0;
    SelStart := OldSS;
    Exit;
    {
    end
    else
    begin
      if (Length(FCurrentSearch) > 0) then
        Delete(FCurrentSearch,Length(FCurrentSearch),1);
    end;
    OldSearch := FCurrentSearch;
    }
  end
  else
  begin
    OldSearch := FCurrentSearch;
    FCurrentSearch := AnsiUpperCase(FCurrentSearch + Key);
  end;

  if (SelLength > 0) and (FLookupSearch = isFirstChar) then
  begin
    FCurrentSearch := AnsiUpperCase(Copy(Text,1,SelStart) + Key);
  end;

  if Assigned(OnTextToGridListItem) then
    OnTextToGridListItem(Self,FCurrentSearch);

  if LookupMethod = lmRequired then
  begin
    if not FindField(FCurrentSearch) then
    begin
      Key := #0;

      if Assigned(FOnLookupError) then
        FOnLookupError(Self,FCurrentSearch);

      FCurrentSearch := OldSearch;

      if DropDownType = ddOnError then
        ShowGridList(True);

      inherited;
      Exit;
    end;
  end;

  inherited;

  lx := -1;

  OldSearch := FCurrentSearch;

  for i := 0 to FAllfields.Count - 1 do
  begin
    s := AnsiUpperCase(TFindlist(FAllfields.Items[i]).Strings[FLookUpColumn]);   // 0 rep by FLookUpColumn
    p := AnsiPos(FCurrentSearch,s);

    if ((FLookupSearch = isFirstChar) and (p = 1)) or ((LookupSearch = isAnyChar) and (p > 0)) then //if AnsiPos(FCurrentSearch,s) = 1 then
    begin
      try
        FDisableChange := True;
        ItemIndex := i;
        Key := #0;

        FAccept := True;
        UpdateLookup;
      finally
        FDisableChange := False;
      end;
      Change;

      if (DropDownType = ddAuto) then
        FCurrentSearch := OldSearch;

      if Assigned(FOnLookupSuccess) then
        FOnLookupSuccess(Self,FCurrentSearch,s);

      if (LookupSearch = isAnyChar) then
        SelStart := p + Length(FCurrentSearch) - 1
      else
        SelStart := Length(FCurrentSearch);

      SelLength := Length(Text) - SelStart;

      Exit;
    end;
    if AnsiPos(AnsiUpperCase(key),s) = 1 then
      lx := i;
  end;


  if not (LookupMethod = lmFast) then
    Exit;

  if lx > -1 then
  begin
    FCurrentSearch := key;

    ItemIndex := lx;
    Key := #0;
    FAccept := True;
    UpdateLookup;
    SelStart := 1;
    SelLength := Length(Text);
  end
  else
  begin
    FCurrentSearch := '';
  end;
end;

procedure TAdvDBLookupComboBox.SetFilterField(const Value: string);
begin
  if (FFilterField <> Value) then
  begin
    FFilterField := Value;
    LoadFromListSource;
  end;
end;

procedure TAdvDBLookupComboBox.SetFilterValue(const Value: string);
begin
  if FFilterValue <> Value then
  begin
    FFilterValue := Value;
    LoadFromListSource;
  end;
end;

{$IFDEF TMSDEBUG}
procedure TAdvDBLookupComboBox.DebugTest;
begin
  Showmessage('Jump to Bookmark and Folditemindex = '+Inttostr(Folditemindex)+' Fitemindex='+Inttostr(Fitemindex));
  if Assigned(fbookmark) then
    if FListDataLink.DataSource.DataSet.BookmarkValid(FBookmark) then
       FListDataLink.DataSource.DataSet.GotoBookmark(FBookmark);
end;
{$ENDIF}


procedure TAdvDBLookupComboBox.SetLabelField(const Value: string);
begin
  FLabelField := Value;
  GridToString;
end;

procedure TAdvDBLookupComboBox.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  if FLabel <> nil then
    UpdateLabel;
end;

procedure TAdvDBLookupComboBox.SetShowMemoFields(const Value: Boolean);
begin
  if (FShowMemoFields <> Value) then
  begin
    FShowMemoFields := Value;
    if not (csDesigning in ComponentState) then
      LoadFromListSource;
  end;
end;

procedure TAdvDBLookupComboBox.SetSortColumn(const Value: string);
var
  i: Integer;
begin
  FSortColumn := Value;
  for i := 1 to Columns.Count do
  begin
    if Value = Columns[i - 1].Name then
      FSortColumns := i - 1;
  end;
end;

procedure TAdvDBLookupComboBox.SetLabelWidth(const Value: Integer);
begin
  FLabelWidth := Value;
  if Assigned(FLabel) then
    UpdateLabel;
end;

procedure TAdvDBLookupComboBox.SetSortDownGlyph(const Value: TBitmap);
begin
  FBitmapUp.Assign(Value);
end;

procedure TAdvDBLookupComboBox.SetSortUpGlyph(const Value: TBitmap);
begin
  FBitmapDown.Assign(Value);
end;

procedure TAdvDBLookupComboBox.UpdateText(s: string);
begin
  FDisableChange := true;
  SetInternalText(s);
  FDisableChange := false;
end;

procedure TAdvDBLookupComboBox.SetLookupLoad(const Value: TLookupLoad);
var
  s: string;
begin
  FLookupLoad := Value;
  if FLookupLoad = llAlways then
  begin
    s := Text;
    LoadFromListSource;
    SetInternalText(s);
  end;
end;

procedure TAdvDBLookupComboBox.SetModified(const Value: Boolean);
begin
  FModified := Value;
end;

function TAdvDBLookupComboBox.GetModified: Boolean;
begin
  Result := FModified;
end;

function TAdvDBLookupComboBox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvDBLookupComboBox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvDBLookupComboBox.SetVersion(const Value: string);
begin

end;


procedure TAdvDBLookupComboBox.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;

  if FFocusColor <> clNone then
  begin
    FNormalColor := Color;
    inherited Color := FFocusColor;
    if FButton.IsWinXP then
    begin
      Width := Width + 1;
      Width := Width - 1;
    end;
  end;
end;

procedure TAdvDBLookupComboBox.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;

  if FFocusColor <> clNone then
  begin
    inherited Color := FNormalColor;
    if FButton.IsWinXP then
    begin
      Width := Width + 1;
      Width := Width - 1;
    end;
  end;
end;


procedure TAdvDBLookupComboBox.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if (csDestroying in ComponentState) then
    Exit;

  if (AOperation = opRemove) then
    if AComponent = FImages then
      FImages := nil;

  if (AOperation = opRemove) and (FListDataLink <> nil) and
    (AComponent = FListDataLink.DataSource) then
  begin
    ListSource := nil;
  end;

  if (AOperation = opRemove) and (FDataSourceLink <> nil) and
    (AComponent = FDataSourceLink.DataSource) then
  begin
    DataSource := nil;
  end;
end;

procedure TAdvDBLookupComboBox.SetLookupColumn(const Value: Integer);
begin
  if (Value >= 0) and ((Value < Columns.Count) or (csLoading in ComponentState)) then
  begin
    FLookupColumn := Value;
    ItemIndex := ItemIndex;
  end;
end;

function TAdvDBLookupComboBox.SecureLookup(const Data:TDataSet; const Field: TField; const KeyFields: String; const KeyValues, KeyValuesDefault: Variant; const ResultFields: String): Variant;
begin
//  if Field.AsVariant = unAssigned then
  if  Field.IsNull then
  begin
    Result := Data.Lookup(KeyFields, KeyValuesDefault, ResultFields);
  end
  else
  begin
    if (Field.AsVariant = NULL) or VarIsNull(Field.AsVariant) then
    begin
      Result := '';
    end
    else
    begin
      Result := Data.Lookup(KeyFields, KeyValues, ResultFields);
    end;
  end;

  if VarIsNull(Result) then
  begin
    {$IFDEF TMSDEBUG}
    OutputDebugString('VarIsNull(VariantResult)');
    {$ENDIF}
    Result := '';
  end;
end;

procedure TAdvDBLookupComboBox.DropDown;
begin
  if FChkClosed then
  begin
    ShowGridList(True);
    //DoTextSelect;
  end;
end;

procedure TAdvDBLookupComboBox.CancelChanges;
begin
  if Assigned(DataSource) and Assigned(FDataSourceLink.DataSet) and FDataSourceLink.DataSet.Active and (FDataSourceLink.DataSet.State = dsEdit) then
    FDataSourceLink.DataSet.Cancel;
end;

procedure TAdvDBLookupComboBox.GridMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  r, c: Integer;
begin
  FStringGrid.MouseToCell(X, Y, c, r);
  if (c < 0) or (r < 0) then // Move outside grid cells
  begin
    if (FHoveredRow >= 0) then
    begin
      FStringGrid.InvalidateRow(FHoveredRow);
      FHoveredRow := -1;
    end;
    Exit;
  end;

  if (r > 0) or ((r = 0) and not FShowGridTitleRow)then
  begin
    if (FHoveredRow <> r) then
    begin
      if (FHoveredRow >= 0) then
        FStringGrid.InvalidateRow(FHoveredRow);

      FHoveredRow := r;
      FStringGrid.InvalidateRow(FHoveredRow);
    end;
  end
  else
  begin
    if (FHoveredRow >= 0) then
    begin
      FStringGrid.InvalidateRow(FHoveredRow);
      FHoveredRow := -1;
    end;
  end;
end;

{ TInplaceStringGrid }

procedure TInplaceStringGrid.WMKeyDown(var Msg: TWMKeydown);
begin
  if (msg.CharCode = VK_TAB) then
  begin
    if Assigned(ParentEdit) and IsWindowVisible(ParentEdit.Handle) then
      PostMessage(ParentEdit.Handle, WM_KEYDOWN, VK_TAB, 0);
    Exit;
  end;

  if (msg.Charcode = VK_ESCAPE) or (msg.CharCode = VK_F4) or
     ((msg.CharCode = VK_UP) and (GetKeyState(VK_MENU) and $8000 = $8000)) then
  begin
    if (msg.Charcode = VK_ESCAPE) then
      ParentEdit.CancelChanges;
    PostMessage((Parent as TForm).Handle,WM_CLOSE,0,0);
  end;

  inherited;
end;

procedure TInplaceStringGrid.DoExit;
begin
  inherited;
  if Visible then
    ParentEdit.HideGridList;
end;

procedure TInplaceStringGrid.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTTAB or DLGC_WANTARROWS;
end;


{ TDropForm }

constructor TDropForm.Create(AOwner: TComponent);
begin
  inherited;
  FHideTimer := TTimer.Create(self);
  FHideTimer.Interval := 10;
  FHideTimer.Enabled := false;
  FHideTimer.OnTimer := OnHideTimer;
end;

constructor TDropForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  FHideTimer := TTimer.Create(self);
  FHideTimer.Interval := 10;
  FHideTimer.Enabled := false;
  FHideTimer.OnTimer := OnHideTimer;
end;

procedure TDropForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  { Create a sizeable window with no caption }
  if FSizeable then
    Params.Style := WS_ThickFrame or WS_PopUp or WS_Border;
end;

destructor TDropForm.Destroy;
begin
  FHideTimer.Free;
  inherited;
end;

procedure TDropForm.OnHideTimer(Sender: TObject);
begin
  FHideTimer.Enabled := False;
  Hide;
end;

procedure TDropForm.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
end;

procedure TDropForm.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if Message.Active = integer(False) then
  begin
    if Assigned(Owner) and (Owner is TAdvDBLookupComboBox) and not TAdvDBLookupComboBox(Owner).Focused then
    begin
      if Visible then
        FHideTimer.Enabled := true;
    end;
  end;
end;

procedure TDropForm.WMClose(var Msg: TMessage);
begin
  inherited;
  self.Free;
end;

procedure TDropForm.WMNCHitTest(var Message: TWMNCHitTest);
begin
  if Sizeable and Visible and FDroppedDown and ((Message.XPos < Left + 5) or (Message.YPos < Top + 5)) then
  begin
    Message.Result := 0;
    Exit;
  end;

  inherited;
end;

procedure TDropForm.WMSize(var Message: TWMSize);
begin
  inherited;
end;

//----------------------------------------------------
{ TDBColumnItem }

procedure TDBColumnItem.Assign(Source: TPersistent);
begin
  if Source is TDBColumnItem then
  begin
    Color := TDBColumnItem(source).Color;
    ColumnType := TDBColumnItem(source).ColumnType;
    Width := TDBColumnItem(source).Width;
    Alignment := TDBColumnItem(source).Alignment;
    Font.Assign(TDBColumnItem(source).Font);
    Title := TDBColumnItem(Source).Title;
    TitleFont.Assign(TDBColumnItem(source).TitleFont);
    ListField := TDBColumnItem(Source).ListField;
    GradientDir := TDBColumnItem(Source).FGradientDir;
    AutoSize := TDBColumnItem(Source).AutoSize;
    FixedColor := TDBColumnItem(Source).FixedColor;
    FixedColorTo := TDBColumnItem(Source).FixedColorTo;
    TitleOrientation := TDBColumnItem(Source).TitleOrientation;
    TitleAlignment := TDBColumnItem(Source).TitleAlignment;
    TitleVerticalAlignment := TDBColumnItem(Source).TitleVerticalAlignment;
  end;
end;

constructor TDBColumnItem.Create(Collection: TCollection);
begin
  inherited;
  FFont := TFont.Create;
  FFont.Name := 'Tahoma';
  FFont.OnChange := OnFontChanged;
  FTitleFont := TFont.Create;
  FTitleFont.Name := 'Tahoma';
  FTitleFont.OnChange := OnTitleFontChanged;
  FWidth := 100;
  FColor := clWindow;
  FFixedColor := clBtnFace;
  FFixedColorTo := clNone;
  FGradientDir := gdVertical;
  FTitleOrientation := toHorizontal;
  FTitleAlignment := taCenter;
  FTitleVerticalAlignment := tvaCenter;
end;

destructor TDBColumnItem.Destroy;
begin
  FFont.Free;
  FTitleFont.Free;
  inherited;
end;

procedure TDBColumnItem.OnFontChanged(Sender: TObject);
begin
  FFontChanged := True;
end;

procedure TDBColumnItem.OnTitleFontChanged(Sender: TObject);
begin
  FTitleFontChanged := True;
end;

function TDBColumnItem.GetListField: string;
begin
  Result := FListField;
end;

function TDBColumnItem.GetDisplayName: string;
begin
  Result := Name;
end;

procedure TDBColumnItem.SetAlignment(const value: tAlignment);
begin
  FAlignment := Value;
  TDBColumnCollection(collection).FOwner.Invalidate;
end;

procedure TDBColumnItem.SetColor(const value: TColor);
begin
  FColor := Value;
  TDBColumnCollection(collection).FOwner.Invalidate;
end;

procedure TDBColumnItem.SetColumnType(const Value: TDBColumnType);
begin
  FColumnType := Value;
  TDBColumnCollection(collection).FOwner.Invalidate;

end;

procedure TDBColumnItem.SetListField(const Value: string);
begin
  FListField := Value;
  with (Collection as TDBColumnCollection) do
  begin
    with (GetOwner as TAdvDBLookupComboBox) do
    begin
      if (csDesigning in ComponentState) and
        not (csLoading in ComponentState) then
          LoadFromListsource;
    end;
  end;
end;

procedure TDBColumnItem.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  TDBColumnCollection(collection).FOwner.Invalidate;
end;

procedure TDBColumnItem.SetWidth(const Value: integer);
begin
  FWidth := Value;
  TDBColumnCollection(collection).FOwner.Invalidate;
end;

function TDBColumnItem.GetCombo: TAdvDBLookupComboBox;
begin
  Result := TDBColumnCollection(Collection).FOwner;//22 Octombrie
  // Old source not compiled under D5 
end;

function TDBColumnItem.GetName: string;
begin
  if FName <> '' then
    Result := FName
  else
    Result := 'Column' + IntToStr(Index);
end;

procedure TDBColumnItem.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TDBColumnItem.SetTitleFont(const Value: TFont);
begin
  FTitleFont.Assign(Value);
end;

{ TDBColumnCollection }

function TDBColumnCollection.Add: TDBColumnItem;
begin
  Result := TDBColumnItem(inherited Add);
end;

constructor TDBColumnCollection.Create(AOwner: TAdvDBLookupComboBox);
begin
  inherited Create(TDBColumnItem);
  FOwner := AOwner;
end;

function TDBColumnCollection.GetItem(Index: Integer): TDBColumnItem;
begin
  Result := TDBColumnItem(inherited Items[index]);
end;

function TDBColumnCollection.GetOwner: tPersistent;
begin
  Result := FOwner;
end;

function TDBColumnCollection.Insert(index: integer): TDBColumnItem;
begin
  Result := TDBColumnItem(inherited Insert(index));
end;

procedure TDBColumnCollection.SetItem(Index: Integer;
  const Value: TDBColumnItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TDBColumnCollection.Update(Item: TCollectionItem);
begin
  inherited;
end;


{ TDBGridDataLink }

procedure TDBGridDataLink.ActiveChanged;
begin
  inherited;

  if Assigned(FGrid) and Assigned(DataSet) then
  begin
    with FGrid do
    begin
      SetActive(Dataset.Active);
      FButton.Enabled := (DataSet.Active) and (FAllFields.Count > 0);

      if (LookupLoad = llOnNeed) and (FAllFields.Count = 0) then
      begin
        SetInternalText(GridToString);
      end;
      UpdateLabel;
    end;
  end;

end;

constructor TDBGridDataLink.Create(AGrid: TAdvDBLookupComboBox);
begin
  inherited Create;
  FGrid := AGrid;
  FNumberRecords := 0;
  FOldRecNo := -1;
end;


procedure TDBGridDataLink.RecordChanged(Field: TField);
var
  lfld: TField;
begin
  inherited;
  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('in recordchanged:'));
  {$ENDIF}

  if Assigned(FGrid) and Assigned(DataSet) then
  begin
    if not FGrid.FInLookup then
    begin
      case  DataSet.State of
      dsBrowse:
        begin
          if (OldState = dsEdit) or (FNumberRecords <> DataSet.RecordCount) then
          begin
            OldState := dsBrowse;
            FGrid.LoadFromListsource;
            if Assigned(FGrid.FDataSourceLink.Datasource) and Assigned(FGrid.FDataSourceLink.Datasource.DataSet) and FGrid.FDataSourceLink.Datasource.DataSet.Active then
              FGrid.FDataSourceLink.Modify;
          end
          else if (FGrid.AlwaysRefreshDropDownList and not FLoadingData) then
          begin
            FLoadingData := True;
            if Assigned(FGrid.FDataSourceLink.Datasource) and Assigned(FGrid.FDataSourceLink.Datasource.DataSet) and FGrid.FDataSourceLink.Datasource.DataSet.Active
               and (FGrid.FDataSourceLink.DataSet.State = dsBrowse) then
            begin
              //FGrid.FDataSourceLink.Modify;
              with FGrid do
              begin
                lfld := ListSource.DataSet.FieldByName(FColumns.Items[FLookUpColumn].FListField);
                if Assigned(lfld) then
                begin
                  SetInternalText(lfld.DisplayText);
                end;
              end;
            end;
            FLoadingData := False;
            FOldRecNo := DataSet.RecNo;
          end;
          OldState := dsBrowse;
          FGrid.UpdateLabel;
         end;
      dsEdit:
        begin
          OldState := dsEdit;
        end;
      end; //end csse
    end;
  end;
end;



destructor TDBGridDataLink.Destroy;
begin
  inherited;
end;



procedure TDBGridDataLink.DataSetChanged;
begin
  inherited;

  with FGrid do
  begin
    if CheckDataSet and not FGrid.FInLookup then
    begin
      if (LookupLoad = llOnNeed) and (FAllFields.Count = 0) then
      begin
        SetInternalText(GridToString);
      end;
      UpdateLabel;
    end;
  end;
end;

{ TFindList }

constructor TFindList.Create;
begin
  inherited Create;
  FGrid := Agrid;
end;

destructor TFindlist.Destroy;
begin
  inherited;
end;

{ TglobalList }

{ TDBGridLookupDataLink }

procedure TDBGridLookupDataLink.ActiveChanged;
begin
  inherited;

  if Assigned(FGrid) and Assigned(DataSet) then
  begin
    with FGrid do
    begin
      SetActive(Dataset.Active);
      FButton.Enabled := (DataSet.Active) and (FAllFields.Count > 0);

      if (LookupLoad = llOnNeed) and (FAllFields.Count = 0) then
      begin
        SetInternalText(GridToString);
      end;
      UpdateLabel;
    end;
  end;

end;

constructor TDBGridLookupDataLink.Create(AGrid: TAdvDBLookupComboBox);
begin
  inherited Create;
  FGrid := AGrid;
end;

procedure TDBGridLookupDataLink.DataSetChanged;
begin
  inherited;

  Modify;

  with FGrid do
  begin
    if CheckDataSet and not FGrid.FInLookup then
    begin
      if (LookupLoad = llOnNeed) and (FAllFields.Count = 0) then
      begin
        SetInternalText(GridToString);
      end;
      UpdateLabel;

      if not (DataSet.State in [dsEdit, dsInsert]) then
        FGrid.Modified := False;
    end;
  end;

end;

procedure TDBGridLookupDataLink.DataSetScrolled(distance: integer);
begin
  inherited;
  Modify;
end;

destructor TDBGridLookupDataLink.Destroy;
begin
  inherited;
end;

procedure TDBGridLookupDataLink.Modify;
Var
  df: String;
  i: Integer;
begin
  if Assigned(FGrid) and Assigned(FGrid.ListSource) and Assigned(DataSet) then
  begin
    if FGrid.DataField = '' then
      Exit;

    if not DataSource.DataSet.Active then
      Exit;
      
    df := DataSource.DataSet.FieldByName(FGrid.DataField).DisplayText;

    with FGrid do
    begin
      if not Assigned(FGrid.ListSource.DataSet) then
        Exit;
        
      if (FAccept) then
      begin
        Change;
        Exit;
      end;

      // ff: ignoring data change
      for i := 0 to FAllfields.Count - 1 do
        if TFindList(FAllfields.Items[i]).KeyField = df then
        begin
          FItemindex := i;
          SetInternalText(GridToString);
          Exit;
        end;

      //++++
      SetInternalText(GridToString);
      //PostMessage(handle, WM_SETTEXT, 0, integer(PChar(GridToString)));
      Exit;
      //++++

      //----
      if (FAllFields.Count = 0) and (LookupLoad = llOnNeed) then
      begin
        SetInternalText(GridToString);
        Exit;
      end;

      for i := 0 to FAllfields.Count - 1 do
        if TFindList(FAllfields.Items[i]).KeyField = df then
        begin
          FItemindex := i;
          SetInternalText(GridToString);
          Exit;
        end;
      //----
    end;
  end;
end;

procedure TDBGridLookupDataLink.RecordChanged(Field: TField);
begin
  inherited;
  if not FGrid.FInLookup then
    Modify;
  FGrid.UpdateLabel;
end;

{ TEllipsLabel }

procedure TLabelEx.Paint;
var
  R: TRect;
  DrawStyle: DWORD;

begin
  R := GetClientRect;

  if not Transparent then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Color;
    Canvas.Rectangle(R.Left,R.Top,R.Right,R.Bottom);
  end;

  Canvas.Brush.Style := bsClear;

  DrawStyle := ALIGNSTYLE[Alignment] or WORDWRAPSTYLE[WordWrap] or
    LAYOUTSTYLE[Layout] or ELLIPSSTYLE[FEllipsType] or ACCELSTYLE[ShowAccelChar];

  DrawStyle := DrawTextBiDiModeFlags(DrawStyle);

  Canvas.Font := Font;

  if not Enabled then
  begin
    OffsetRect(R, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawTextEx(Canvas.Handle,PChar(Caption),Length(Caption),R, DrawStyle, nil);
    OffsetRect(R, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawTextEx(Canvas.Handle,PChar(Caption),Length(Caption),R, DrawStyle, nil);
  end
  else
    DrawTextEx(Canvas.Handle,PChar(Caption),Length(Caption),R, DrawStyle, nil);
end;

procedure TLabelEx.SetEllipsType(const Value: TEllipsType);
begin
  if FEllipsType <> Value then
  begin
    FEllipsType := Value;
    Invalidate;
  end;
end;


{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}

end.


