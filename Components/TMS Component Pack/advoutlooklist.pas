{*************************************************************************}
{ TMS AdvOutlookList component                                            }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2005 - 2015                                       }
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

{$I TMSDEFS.INC}

unit AdvOutlookList;

interface

uses
  Classes, SysUtils, OutlookGroupedList, ExtCtrls, Windows, Controls, Messages,
  Forms, Graphics, ImgList, Dialogs, StdCtrls, ComObj, ActiveX, shlObj, AxCtrls, Math,
  PictureContainer, AdvStyleIF, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  CHECKBOX_SIZE = 15;

  //version info
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 7; // Minor version nr.
  REL_VER = 9; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.0.0.0 : first release
  // 1.0.0.1 : Fixed: design time columns delete & move
  // 1.0.0.2 : Fixed: design time column editing in Delphi 5, C++Builder 5
  // 1.1.0.0 : Fixed:  Resizing Column with mouse, click event is now not triggered
  //         : New property SortGroups added
  //         : New property GroupItemHeight added
  //         : New ShowNodes added
  //         : New SortType stCustom added with event OnCustomCompare
  //         : New functions SelectAll, CollapseAll and ExpandAll added
  //         : New event OnGroupDblClick added
  //         : Improved Text is drawn vertically centered in items.
  //         : Improved Column order is persisted when setting GroupColumn.
  // 1.1.0.1 : New function ColumnIndex to get the index of a column in the Columns collection from the visual column index
  // 1.1.5.0 : New function FirstSelectedItem: POGLItem
  //         : New function NextSelectedItem(Item: POGLItem): POGLItem
  //         : New property SelectedCount: integer;
  //         : Fix : issue with use on Frames
  //         : Delphi 2006 support
  //         : Added POGLItem.ItemObject property to store object with item
  // 1.1.6.1 : Added function GroupIndex(Group: TOutlookGroup): integer;
  // 1.2.0.0 : New property Visible in Columns
  //         : New stFloat sort style
  //         : New GroupShowCount property
  // 1.2.1.0 : Procedure DeleteGroup(Index) added
  //         : Fixed issue with GroupIndex function
  // 1.2.1.1 : Fixed issue with prefix & processing
  //         : Published ItemHeight property
  // 1.3.0.0 : New property HideSelection
  //         : New property GroupColumnDisplay
  //         : New functions ItemAtXY, GroupAtXY
  //         : New VCL drag & drop support
  //         : New GroupImageIndex to set image for grouped column
  //         : New Lookup for direct or incremental keyboard lookup in a selected column
  //         : New SelectionOptions property to control multiselect & right click select
  // 1.3.1.0 : New SelectItem procedure added
  //         : Fixed issue with SetFocus method
  //         : New Group.ChildOGLItem[] property added
  // 1.3.5.0 : New AdvFormStyler, AdvAppStyler interface added
  // 1.3.5.1 : Tag & Data properties added in TOutlookGroup
  // 1.3.6.0 : New method DeleteAllGroups added
  // 1.3.6.1 : Fixed issue with ItemObject & Tag when grouping items
  // 1.3.7.0 : New property HeaderResize & OnHeaderResize event added
  //         : New event OnSorted added
  // 1.3.7.1 : Fixed issues with drag & drop
  // 1.3.8.0 : Added OutlookGroup.ItemIndex(p: POGLItem); function
  // 1.3.9.0 : New : added support for Office 2007 silver style
  // 1.3.10.0: New : method UnSelectAll added
  //         : Fixed : issue with SelectItem & multiselect option
  // 1.3.10.1: Fixed : issue with ClearChilds on group without childs
  // 1.3.10.2: Fixed : memory leak issue with TOutlookGroup
  // 1.3.10.3: Fixed : issue with DeleteGroup, DeleteAllGroups method
  // 1.4.0.0 : New : SelectionColor, SelectionTextColor properties added
  // 1.4.1.0 : New : OnMouseMove, OnMouseDown, OnMouseUp events added
  //         : Fixed : issue with OnSelectionChange
  // 1.5.0.0 : New : GroupFont property added
  //         : New : GroupCountFont property added
  //         : New : GroupSelectionColor property added
  //         : New : GroupSelectionTextColor property added
  //         : New : GroupColor property added
  //         : New : C++Builder 2007 support
  // 1.5.0.1 : Fixed : Issue with NextSelectedItem call
  // 1.5.1.0 : New : keyboard lookup ignores HTML tags
  // 1.5.2.0 : New : exposed PreviewSettings.Height
  //         : Fixed : issue with hiding columns
  // 1.5.3.0 : New : event OnGroupExpand/OnGroupCollaps added
  //         : Fixed : issue with OnGroupClick event during expand/collaps
  //         : Improved : unselect behaviour in single select mode
  // 1.5.3.1 : Fixed : issue with function group.InsertChild()
  // 1.5.3.2 : Improved : hint positioning
  // 1.5.3.3 : Fixed : issue with column index in the header events
  // 1.5.3.4 : Fixed : issue with NextSelectedItem function
  // 1.5.3.5 : Fixed : issue with VCL drag & drop & right clicks
  // 1.5.3.6 : Fixed : issue with OnSelectionChange event
  // 1.5.4.0 : New : support for HTML formatted text in AdvOutlookList group header
  // 1.5.5.0 : New : InsertGroup method added
  //         : New : event OnGetItemHint added
  //         : New : event OnGetGroupHint added
  // 1.5.5.1 : Fixed : issue with GroupIndex function
  // 1.5.5.2 : Fixed : issue with URL click on item & drag & drop
  // 1.5.6.0 : New : stAnsiText, stAnsiTextNoCase sort types added
  // 1.5.6.1 : Fixed : issue with multiselect & drag & drop
  // 1.5.6.2 : Improved : OnItemRightClick also triggered when right-click selection disabled
  // 1.5.7.0 : New : added Tag property in TAdvOutlookColumn
  // 1.5.8.0 : New : exposed  function IsGroupExpanded(Item: POGLItem): Boolean;
  //                          function ExpandItem(Item: POGLItem): Boolean;
  //                          function CollapseItem(Item: POGLItem): Boolean;
  //                          procedure ToggleExpandedItem(Item: POGLItem);
  // 1.5.8.1 : Fixed : issue with drag & drop of groups
  //         : Fixed : issue with hidden columns & group display
  // 1.5.8.2 : Fixed : issue with column index passed to OnCompareItems
  // 1.5.8.3 : Improved : built-in exception handling for incorrect sort formats
  // 1.5.8.4 : Fixed : issue with shift selection and OnSelectionChange event
  // 1.5.8.5 : Fixed : issue with keyboard selection and OnSelectionChange event
  // 1.5.8.6 : Improved : programmatic group selection behaviour   
  // 1.5.9.0 : Improved : scroll tracking
  //         : Improved : group expand toggling on dbl click
  // 1.5.9.1 : Fixed : issue with automatic group selection on programmatic selection of items
  // 1.5.10.0: New : OnItemDblClick event extended with Column parameter
  // 1.6.0.0 : New : Terminal, Vista & Windows 7 styles
  //         : Fixed : issue with memory references in ClearChilds
  // 1.6.0.1 : Improved : painting of right aligned text
  // 1.6.0.2 : Fixed : issue with InsertChild position
  // 1.6.0.3 : Improved : performance of adding items to the list with BeginUpdate/EndUpdate
  // 1.6.0.4 : Fixed : small issue with HideSelection = true for selected groups
  //         : Fixed : issue with TabStop = false
  //         : Fixed : issue with right-aligned text and scrollbar
  // 1.6.0.5 : Fixed : issue with NextSelectedItem method
  // 1.6.0.6 : Fixed : selected group display issue for non focused list
  // 1.6.1.0 : New : support for customizing bullets in HTML UL lists
  // 1.7.0.0 : New : Built in support for Office 2010 colors
  // 1.7.1.0 : New : Function ScrollList added to programmatically scroll the child list
  // 1.7.2.0 : New : OnHeaderHint event added
  // 1.7.3.0 : New : Added default param to SelectItem method to control focus
  // 1.7.3.1 : Fixed : Issue with drawing progress bar outside column boundaries
  // 1.7.3.2 : Fixed : Issue with column drag & drop
  //         : Fixed : Issue with column moving
  // 1.7.4.0 : New : Property GroupLineColor added
  // 1.7.4.1 : Fixed : Rare issue with grouping when overflow checking is enabled
  // 1.7.5.0 : Improved : In Preview, normal text is now displayed as wordwrapped text
  // 1.7.5.1 : Fixed : Issue with ItemAtXY() function
  // 1.7.5.2 : Fixed : Issue with handling item objects with OLE drag & drop
  // 1.7.6.0 : New : Windows 8, Office 2013 styles added
  // 1.7.7.0 : New : Support for HTML formatted text in column headers
  // 1.7.7.1 : Fixed : Issue with reordering columns in TAdvOutlookList
  // 1.7.7.2 : Fixed : Issue with setting HeaderHeight
  // 1.7.8.0 : New : Delphi XE5 & C++Builder XE5 support
  // 1.7.8.1 : Fixed : Rare issue with selection & reloading the list
  // 1.7.8.2 : Improved : Memory use for group access
  // 1.7.8.3 : Fixed : Issue with selection & removing items
  // 1.7.9.0 : New : Windows 10, Office 2016 styles added

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TProOutlookGroupedList = class(TOutlookGroupedList);
  TAdvOutlookList = class;

  TColumnType = (ctText, ctImage, ctURL, ctCheckBox, ctProgress);
  TGradientDir = (gdVertical, gdHorizontal);
  TCheckBoxStyle = (cbsClassic, cbsFlat, cbsWinXP, cbsBorland);
  TURLType = (utHTTP, utHTTPS, utFTP, utMailTo, utNNTP);
  TSortType = (stNone, stTextCase, stTextNoCase, stNumeric, stBoolean, stDate, stTime, stDateTime, stCustom, stFloat,stAnsiText, stAnsiTextNoCase);
  TSortDirection = (sdAscending, sdDescending);
  TDragDropSetting = (ddEnabled, ddDisabled);
  TLookUpMethod = (lmDirect, lmIncremental);
  TGroupColumnDisplay = (gdHidden, gdVisible);

  TAdvOutlookListStyle = (olsOffice2003Blue, olsOffice2003Silver, olsOffice2003Olive, olsOffice2003Classic, olsOffice2007Luna, olsOffice2007Obsidian, olsWindowsXP, olsWhidbey, olsCustom, olsOffice2007Silver, olsWindowsVista, olsWindows7, olsTerminal, olsOffice2010Blue, olsOffice2010Silver, olsOffice2010Black,
  olsWindows8, olsOffice2013White, olsOffice2013LightGray, olsOffice2013Gray,
  olsWindows10, olsOffice2016White, olsOffice2016Gray, olsOffice2016Black);

  TAdvOutlookColumn = class(TCollectionItem)
  private
    FCaption: string;
    FWidth: Integer;
    FHeaderImageIndex: Integer;
    FAlignment: TAlignment;
    FHeaderAlignment: TAlignment;
    FHeaderColor: TColor;
    FHeaderColorTo: TColor;
    FColor: TColor;
    FColumnType: TColumnType;
    FHeaderFont: TFont;
    FFont: TFont;
    //FHeaderBorderColor: TColor;
    FHeaderGradientDir: TGradientDir;
    FURLType: TURLType;
    FSortType: TSortType;
    FHeaderHint: String;
    FHeaderShowHint: Boolean;
    FHint: String;
    FShowHint: Boolean;
    FHeaderSecIndex: Integer;
    FVisible: Boolean;
    FGroupImageIndex: Integer;
    FHeaderSecOrgIndex: Integer;
    FTag: integer;
    procedure OnFontChanged(Sender: TObject);
    procedure SetCaption(const Value: string);
    procedure SetWidth(const Value: Integer);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetColor(const Value: TColor);
    procedure SetColumnType(const Value: TColumnType);
    procedure SetFont(const Value: TFont);
    procedure SetHeaderAlignment(const Value: TAlignment);
    procedure SetHeaderColor(const Value: TColor);
    procedure SetHeaderColorTo(const Value: TColor);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetHeaderImageIndex(const Value: Integer);
    //procedure SetHeaderBorderColor(const Value: TColor);
    procedure SetHeaderGradientDir(const Value: TGradientDir);
    procedure SetURLType(const Value: TURLType);
    procedure SetSortType(const Value: TSortType);
    procedure SetHeaderSecIndex(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetGroupImageIndex(const Value: Integer);
    procedure SetHeaderSecOrgIndex(const Value: Integer);
  protected
    procedure Changed;
    procedure Refresh;
    function GetDisplayName: string; override;
    procedure SetIndex(Value: Integer); override;

    property HeaderSecIndex: Integer read FHeaderSecIndex write SetHeaderSecIndex;
    property HeaderSecOrgIndex: Integer read FHeaderSecOrgIndex write SetHeaderSecOrgIndex;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: string read FCaption write SetCaption;
    property Width: Integer read FWidth write SetWidth default 50;
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor default clNone;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property ColumnType: TColumnType read FColumnType write SetColumnType default ctText;
    property URLType: TURLType read FURLType write SetURLType default utHTTP;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property HeaderColor: TColor read FHeaderColor write SetHeaderColor default clWhite;
    property HeaderColorTo: TColor read FHeaderColorTo write SetHeaderColorTo default clBtnFace;
    //property HeaderBorderColor: TColor read FHeaderBorderColor write SetHeaderBorderColor;
    property HeaderGradientDir: TGradientDir read FHeaderGradientDir write SetHeaderGradientDir default gdVertical;
    property HeaderAlignment: TAlignment read FHeaderAlignment write SetHeaderAlignment default taCenter;
    property HeaderImageIndex: Integer read FHeaderImageIndex write SetHeaderImageIndex default -1;
    property HeaderHint: String read FHeaderHint write FHeaderHint;
    property HeaderShowHint: Boolean read FHeaderShowHint write FHeaderShowHint default False;
    property Hint: String read FHint write FHint;
    property ShowHint: Boolean read FShowHint write FShowHint default False;
    property SortType: TSortType read FSortType write SetSortType default stTextNoCase;
    property Visible: Boolean read FVisible write SetVisible default True;
    property GroupImageIndex: Integer read FGroupImageIndex write SetGroupImageIndex default -1;
    property Tag: integer read FTag write FTag default 0;
  end;

  TAdvOutlookColumns = class(TCollection)
  private
    FOwner: TComponent;
    FOnChange: TNotifyEvent;
    FOnRefresh: TNotifyEvent;
    FAdvOutLookList: TAdvOutLookList;
    function GetItem(Index: Integer): TAdvOutlookColumn;
    procedure SetItem(Index: Integer; const Value: TAdvOutlookColumn);
  protected
    procedure Changed;
    procedure Refresh;
    procedure UpdateOnIndexChanged;
    procedure UpdateOnDeleteItem(Index: Integer);
    procedure Update(Item: TCollectionItem); override;
    property AdvOutLookList: TAdvOutLookList read FAdvOutLookList write FAdvOutLookList;
  public
    constructor Create(AOwner: TComponent);
    function GetOwner: TPersistent; override;
    function Add: TAdvOutlookColumn;
    function Insert(Index: Integer): TAdvOutlookColumn;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TAdvOutlookColumn read GetItem write SetItem; default;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnRefresh: TNotifyEvent read FOnRefresh write FOnRefresh;
  end;

  POutlookInfo = ^TOutlookInfo;
  TOutlookInfo = packed record
    data: TStringList;
  end;

  TOutlookGroup = class(TObject)
  private
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function GetChildItem(Index: integer): TStrings;
    function GetChildOLGItem(Index: integer): POGLItem;
    function GetChildCount: integer;
    function GetExpanded: boolean;
    procedure SetExpanded(const Value: boolean);
    function GetTag: Integer;
    procedure SetObject(const Value: TObject);
    procedure SetTag(const Value: Integer);
    function GetObject: TObject;
  protected
    OGLItem: POGLItem;
    List: TAdvOutlookList;
  public
    function ItemIndex(p: POGLItem): integer;
    function AddChild: TStrings;
    function InsertChild(Index: Integer): TStrings;
    procedure RemoveChild(Index: Integer);
    procedure ClearChilds;
    property Caption: string read GetCaption write SetCaption;
    property ChildItem[Index: Integer]: TStrings read GetChildItem;
    property ChildOGLItem[Index: Integer]: POGLItem read GetChildOLGItem;
    property ChildCount: integer read GetChildCount;
    property ListItem: POGLItem read OGLItem;
    property Expanded: boolean read GetExpanded write SetExpanded;
    property Data: TObject read GetObject write SetObject;
    property Tag: Integer read GetTag write SetTag;
  end;

  TOutlookItem = class(TObject)
  protected
    OGLItem: POGLItem;
  end;

  THeaderClickEvent = procedure(Sender: TObject; SectionIndex: Integer) of object;
  THeaderDragDropEvent = procedure(Sender: TObject; FromSection, ToSection: Integer) of object;
  THeaderResizedEvent = procedure(Sender: TObject; SectionIndex, NewWidth: Integer) of object;
  THeaderHintEvent = procedure(Sender: TObject; ColumnIndex: integer; var AHint:string) of object;

  TAdvOutLookHeader = class(THeader)
  private
    FOutLookList: TAdvOutlookList;
    FOnClick: THeaderClickEvent;
    FOnRightClick: THeaderClickEvent;
    FOnDblClick: THeaderClickEvent;
    FSectionDragDrop: Boolean;
    FDragging: Boolean;
    FDragStart: Integer;
    FOffset: Integer;
    FOnDragDrop: THeaderDragDropEvent;
    FMouseOverSec: Integer;
    FMouseDragPos: TPoint;
    FReSizing: Boolean;
    FOnSized: TSectionEvent;
    procedure OwnOnDragDrop(Sender: TObject; FromSection, ToSection: Integer);
    procedure OwnSizing(Sender: TObject; ASection, AWidth: Integer);
    procedure OwnSized(Sender: TObject; ASection, AWidth: Integer);
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMLButtonDblClk(var Message: TWMLButtonDBLClk); message WM_LBUTTONDBLCLK;
    procedure WMRButtonDown(var Message: TWMLButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonUp(var Message: TWMLButtonUp); message WM_RBUTTONUP;
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function XYToSection(X, Y: Integer): Integer;
    function GetSectionRect(X: Integer): TRect;

    function GetColIndex(SecIndex: Integer): Integer;
    procedure DrawSortIndicator(Canvas: TCanvas; SectionRect: TRect; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property SectionDragDrop: Boolean read FSectionDragDrop write FSectionDragDrop;
    property OnClick: THeaderClickEvent read FOnClick write FOnClick;
    property OnDragDrop: THeaderDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnDblClick: THeaderClickEvent read FOnDblClick write FOnDblClick;
    property OnRightClick: THeaderClickEvent read FOnRightClick write FOnRightClick;
    property OnSized: TSectionEvent read FOnSized write FOnSized;
  end;

  TProgressStyle = (psXP, psClassic);

  TProgressAppearance = class(TPersistent)
  private
    FUnCompleteFontColor: TColor;
    FCompleteColor: TColor;
    FUnCompleteColor: TColor;
    FCompleteFontColor: TColor;
    FOnChange: TNotifyEvent;
    FStacked: Boolean;
    FShowPercentage: Boolean;
    FShowBorder: Boolean;
    FCompletionSmooth: Boolean;
    FShowGradient: Boolean;
    FLevel2Perc: Integer;
    FLevel1Perc: Integer;
    FSteps: Integer;
    FLevel3Color: TColor;
    FLevel1Color: TColor;
    FLevel0Color: TColor;
    FLevel3ColorTo: TColor;
    FLevel2ColorTo: TColor;
    FLevel0ColorTo: TColor;
    FLevel1ColorTo: TColor;
    FBorderColor: TColor;
    FLevel2Color: TColor;
    FStyle: TProgressStyle;
    procedure SetCompleteColor(const Value: TColor);
    procedure SetCompleteFontColor(const Value: TColor);
    procedure SetUnCompleteColor(const Value: TColor);
    procedure SetUnCompleteFontColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetCompletionSmooth(const Value: Boolean);
    procedure SetLevel0Color(const Value: TColor);
    procedure SetLevel0ColorTo(const Value: TColor);
    procedure SetLevel1Color(const Value: TColor);
    procedure SetLevel1ColorTo(const Value: TColor);
    procedure SetLevel1Perc(const Value: Integer);
    procedure SetLevel2Color(const Value: TColor);
    procedure SetLevel2ColorTo(const Value: TColor);
    procedure SetLevel2Perc(const Value: Integer);
    procedure SetLevel3Color(const Value: TColor);
    procedure SetLevel3ColorTo(const Value: TColor);
    procedure SetShowBorder(const Value: Boolean);
    procedure SetShowGradient(const Value: Boolean);
    procedure SetShowPercentage(const Value: Boolean);
    procedure SetStacked(const Value: Boolean);
    procedure SetSteps(const Value: Integer);
    procedure SetStyle(const Value: TProgressStyle);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property CompleteColor: TColor read FCompleteColor write SetCompleteColor default clRed;
    property CompleteFontColor: TColor read FCompleteFontColor write SetCompleteFontColor default clBlue;
    property UnCompleteColor: TColor read FUnCompleteColor write SetUnCompleteColor default clNone;
    property UnCompleteFontColor: TColor read FUnCompleteFontColor write SetUnCompleteFontColor default clWindowText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Level0Color: TColor read FLevel0Color write SetLevel0Color default clLime;
    property Level0ColorTo: TColor read FLevel0ColorTo write SetLevel0ColorTo default $00E1FFE1;
    property Level1Color: TColor read FLevel1Color write SetLevel1Color default clYellow;
    property Level1ColorTo: TColor read FLevel1ColorTo write SetLevel1ColorTo default $00CAFFFF;
    property Level2Color: TColor read FLevel2Color write SetLevel2Color default $0053A9FF;
    property Level2ColorTo: TColor read FLevel2ColorTo write SetLevel2ColorTo default $00A8D3FF;
    property Level3Color: TColor read FLevel3Color write SetLevel3Color default clRed;
    property Level3ColorTo: TColor read FLevel3ColorTo write SetLevel3ColorTo default $00CACAFF;
    property Level1Perc: Integer read FLevel1Perc write SetLevel1Perc default 70;
    property Level2Perc: Integer read FLevel2Perc write SetLevel2Perc default 90;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property ShowBorder: Boolean read FShowBorder write SetShowBorder default False;
    property Stacked: Boolean read FStacked write SetStacked default False;
    property Style: TProgressStyle read FStyle write SetStyle default psXP;
    property ShowPercentage: Boolean read FShowPercentage write SetShowPercentage default true;
    property CompletionSmooth: Boolean read FCompletionSmooth write SetCompletionSmooth default true;
    property ShowGradient: Boolean read FShowGradient write SetShowGradient default true;
    property Steps: Integer read FSteps write SetSteps default 11;
  end;

  TPreviewSetting = class(TPersistent)
  private
    FActive: Boolean;
    FColumn: Integer;
    FOnChange: TNotifyEvent;
    FHeight: Integer;
    FFont: TFont;
    procedure SetActive(const Value: Boolean);
    procedure SetColumn(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetFont(const Value: TFont);
    procedure OnFontChange(Sender: TObject);
  protected
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Column: Integer read FColumn write SetColumn default -1;
    property Height: Integer read FHeight write SetHeight default 40;
    //property TextColor: TColor read FTextColor write SetTextColor default clBlue;
    property Font: TFont read FFont write SetFont;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TURLSettings = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FColor: TColor;
    FFontStyle: TFontStyles;
    procedure SetColor(const Value: TColor);
    procedure SetFontStyle(const Value: TFontStyles);
  protected
    procedure Change;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBlue;
    property FontStyle : TFontStyles read FFontStyle write SetFontStyle default [];
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TSortSettings = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FColumn: Integer;
    FGlyphDown: TBitMap;
    FGlyphUp: TBitMap;
    FDirection: TSortDirection;
    FEnabled: Boolean;
    FSortGroups: Boolean;
    procedure OnGlyphChange(Sender: TObject);
    procedure SetColumn(const Value: Integer);
    procedure SetDirection(const Value: TSortDirection);
    procedure SetGlyphDown(const Value: TBitMap);
    procedure SetGlyphUp(const Value: TBitMap);
    procedure SetSortGroups(const Value: Boolean);
  protected
    procedure Change;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Column: Integer read FColumn write SetColumn default -1;
    property Direction: TSortDirection read FDirection write SetDirection default sdAscending;
    property Enabled: boolean read FEnabled write FEnabled default true;
    property GlyphUp: TBitMap read FGlyphUp write SetGlyphUp;
    property GlyphDown: TBitMap read FGlyphDown write SetGlyphDown;
    property SortGroups: Boolean read FSortGroups write SetSortGroups;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TLookUpSettings = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FColumn: Integer;
    FMethod: TLookUpMethod;
    procedure SetColumn(const Value: Integer);
    procedure SetMethod(const Value: TLookUpMethod);
  protected
    procedure Change;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Column: Integer read FColumn write SetColumn default -1;
    property Method: TLookUpMethod read FMethod write SetMethod;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TDrawItemPropEvent = procedure (Sender: TObject; Item: POGLItem; Column: integer; AValue: String; ABrush: TBrush; AFont: TFont) of object;
  TDrawItemValueEvent = procedure (Sender: TObject; Item: POGLItem; Column: integer; var Value: String) of object;
  TDrawItemEvent = procedure (Sender: TObject; Item: POGLItem; Column: Integer; Value: String; ItemCanvas: TCanvas; ItemRect: TRect) of object;
  TURLClickEvent = procedure (sender: TObject; item: POGLItem; columnIndex: Integer; URL: string; var Default: boolean) of object;
  TAOLItemClickEvent = procedure(Sender: TObject; Item: POGLItem; Column: Integer) of object;
  TAOLItemEvent = procedure(Sender: TObject; Item: POGLItem) of object;
  TAOLHeaderClickEvent = procedure(Sender: TObject; Column: Integer) of object;
  TAnchorClick = procedure (Sender:TObject; Item: POGLItem; Column: Integer; Anchor:string) of object;
  TCheckBoxClickEvent = procedure(Sender: TObject; Item: POGLItem; Column: Integer; NewValue: Boolean) of object;
  TOnHeaderDragDropEvent = procedure(Sender: TObject; FromCol, ToCol: Integer) of object;
  TOnCustomCompareEvent = procedure(Sender: TObject; Item1, Item2: POGLItem; Column: Integer; Value1, Value2: string; var SortResult: Integer) of object;
  TOnSortedEvent = procedure(Sender: TObject; ColumnIndex: integer) of object;
  // DragNDrop OLE events
  TAOLDragOverEvent = procedure(Sender: TAdvOutlookList; const DataObject: IDataObject; Shift: TShiftState;
    Pt: TPoint; State: TDragState; var Effect: Integer; var Accept: Boolean) of object;
  TAOLDropEvent = procedure(Sender: TAdvOutlookList; const DataObject: IDataObject; Shift: TShiftState;
    Pt: TPoint; Formats: TClipFormatArray; var Effect: Integer) of object;
  TAOLGetDataEvent = procedure(Sender: TAdvOutlookList; const FormatEtcIn: TFormatEtc; out Medium: TStgMedium;
    var Result: HRESULT) of object;
  TAOLGetClipboardFormatsEvent = procedure(Sender: TAdvOutlookList; var Formats: TFormatEtcArray) of object;
  TAOLDragAllowedEvent = procedure(Sender: TAdvOutlookList; Item: POGLItem; var Allowed: Boolean) of object;
  TAOLGetCaptionEvent = procedure(Sender: TAdvOutlookList; Item: POGLItem; var Caption: String) of object;
  TOnGetItemHintEvent = procedure(Sender: TAdvOutlookList; Item: POGLItem; ColumnIndex: Integer; var HintText: String) of object;
  TOnGetGroupHintEvent = procedure(Sender: TAdvOutlookList; Item: POGLItem; var HintText: String) of object;


  TOLEItemData = procedure(Sender: TAdvOutlookList; Data: TStrings) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOutlookList = class(TCustomControl, ITMSStyle) //TWinControl
  private
    FColumns: TAdvOutlookColumns;
    FBorderStyle: TBorderStyle;
    FList: TOutlookGroupedList;
    FHeader: TAdvOutLookHeader;//THeader;
    FColumnLineColor: TColor;
    FURLSettings: TURLSettings;
    FImages: TImageList;
    FOnDrawItem: TDrawItemEvent;
    FOnDrawItemProp: TDrawItemPropEvent;
    FOnDrawItemValue: TDrawItemValueEvent;
    FCheckBoxStyle: TCheckBoxStyle;
    FProgressAppearance: TProgressAppearance;
    FOnURLClick: TURLClickEvent;
    FPreviewSettings: TPreviewSetting;
    FHeaderBorderColor: TColor;
    FBorderColor: TColor;
    FOnItemRightClick: TAOLItemClickEvent;
    FOnItemClick: TAOLItemClickEvent;
    FOnItemDblClick: TAOLItemClickEvent;
    FOnGroupClick: TAOLItemEvent;
    FOnGroupRightClick: TAOLItemEvent;
    FOnHeaderRightClick: TAOLHeaderClickEvent;
    FOnHeaderClick: TAOLHeaderClickEvent;
    FOnHeaderDblClick: TAOLHeaderClickEvent;
    FSortSettings: TSortSettings;
    FSorting: Boolean;
    FEllipsis: Boolean;
    FHeaderHeight: integer;
    FImageCache: THTMLPictureCache;
    FContainer: TPictureContainer;
    FHoverHyperLink: Integer;
    FOldHoverHyperLink: Integer;
    FShadowOffset: integer;
    FHover: boolean;
    FHoverColor: TColor;
    FHoverFontColor: TColor;
    FShadowColor: TColor;
    FAnchor: string;
    FAnchorExit: TAnchorClick;
    FAnchorClick: TAnchorClick;
    FAnchorEnter: TAnchorClick;
    FCurrHoverRect: TRect;
    FGroupColumn: Integer;
    FCurrentGroupColumn: Integer;
    FColor: TColor;
    FOnCheckBoxClick: TCheckBoxClickEvent;
    FHeaderDragDrop: TDragDropSetting;
    FDragDropSetting: TDragDropSetting;
    FOnHeaderDragDrop: TOnHeaderDragDropEvent;
    FOnHeaderResized: THeaderResizedEvent;
    FOnHeaderHint: THeaderHintEvent;
    FMouseOverCol: Integer;
    FItemHeight: Integer;
    FPreviewedColumn: Integer;
    FOnOLEDragAllowed: TAOLDragAllowedEvent;
    FOnOLEDragOver: TAOLDragOverEvent;
    FOnOLEDrop: TAOLDropEvent;
    FOnOLEGetClipboardFormats: TAOLGetClipboardFormatsEvent;
    FOnOLEGetData: TAOLGetDataEvent;
    FOnGetCaption: TAOLGetCaptionEvent;
    FDragDropMode: TDragDropMode;
    FDragFileList: TStringList;
    FOnOLEGetItemData: TOLEItemData;
    FOnOLEItemDrop: TOLEItemData;
    FGroupItemHeight: Integer;
    FOnCustomCompare: TOnCustomCompareEvent;
    FOnGroupDblClick: TAOLItemEvent;
    FGroupShowCount: Boolean;
    FOnSelectionChange: TNotifyEvent;
    FOnSorted: TOnSortedEvent;
    FHideSelection: Boolean;
    FLookUp: TLookUpSettings;
    FLookUpText: string;
    FGroupColumnDisplay: TGroupColumnDisplay;
    FDragType: TOGLDragType;
    FSelectionOptions: TOGLSelectionOptions;
    FHeaderResize: boolean;
    FGroupList: TList;
    FSelectionColor: TColor;
    FSelectionTextColor: TColor;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FGroupColor: TColor;
    FGroupLineColor: TColor;
    FGroupSelectionColor: TColor;
    FGroupSelectionTextColor: TColor;
    FGroupFont: TFont;
    FGroupCountFont: TFont;
    FOnGroupExpand: TAOLItemEvent;
    FOnGroupCollaps: TAOLItemEvent;
    FOnGetGroupHint: TOnGetGroupHintEvent;
    FOnGetItemHint: TOnGetItemHintEvent;
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    //procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    //procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTabStopChanged(var Message: TMessage); message CM_TABSTOPCHANGED;
    procedure OnPreviewSettingChange(Sender: TObject);
    procedure OnProgressAppearanceChange(Sender: TObject);
    procedure OnURLSettingsChange(Sender: TObject);
    procedure OnHeaderSized(Sender: TObject; ColIdx, ColWidth: Integer);
    procedure OnListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OnListMouseDownOnItem(Sender: TOutlookGroupedList; Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      HitInfo: TOGLItemHitInfo);
    procedure OnListMouseUpOnItem(Sender: TOutlookGroupedList; Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      HitInfo: TOGLItemHitInfo);
    procedure OnListItemDblClick(Sender: TOutlookGroupedList; Item: POGLItem; X, Y: Integer);
    procedure OnListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnListKeyPress(Sender: TObject; var Key: Char);
    procedure OnListCompareItems(Sender: TOutlookGroupedList; Item1, Item2: POGLItem; Column: TOGLSortColumn; var Result: Integer);
    procedure OnListGetHint(Sender: TOutlookGroupedList; Item: POGLItem; var HintText: String; var HintPos: TPoint);
    procedure OnListGetGroupImageIndex(Sender: TOutlookGroupedList; Item: POGLItem; var ImageIndex: Integer);
    procedure OnListSelectionChange(Sender: TObject);
    //--- Drag Events
    procedure OnListOLEDragAllowed(Sender: TOutlookGroupedList;
      Item: POGLItem; var Allowed: Boolean);
    procedure OnListOLEDragOver(Sender: TOutlookGroupedList; const DataObject: IDataObject; Shift: TShiftState; Pt: TPoint;
      State: TDragState; var Effect: Integer; var Accept: Boolean);
    procedure OnListOLEDrop(Sender: TOutlookGroupedList; const DataObject: IDataObject; Shift: TShiftState; Pt: TPoint;
      Formats: TClipFormatArray; var Effect: Integer);
    procedure OnListOLEGetData(Sender: TOutlookGroupedList; const FormatEtcIn: tagFORMATETC; out Medium: tagSTGMEDIUM; var Result: HRESULT);
    procedure OnListOLEGetClipboardFormats(Sender: TOutlookGroupedList; var Formats: TFormatEtcArray);
    //--- Drag VCL
    procedure OnListDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure OnListDragDrop(Sender, Source: TObject; X, Y: Integer);
    //---
    procedure OnColumnRefresh(Sender: TObject);
    procedure OnHeaderSectClick(Sender: TObject; SectionIndex: Integer);
    procedure OnHeaderSectRightClick(Sender: TObject; SectionIndex: Integer);
    procedure OnHeaderSectDblClick(Sender: TObject; SectionIndex: Integer);
    procedure OnSortChange(Sender: TObject);
    procedure OnGroupCountFontChanged(Sender: TObject);
    procedure OnGroupFontChanged(Sender: TObject);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure FreeItem(Sender: TOutlookGroupedList; Item: POGLItem);
    procedure DrawItem(Sender: TOutlookGroupedList; ItemCanvas: TCanvas; ItemRect: TRect; Item: POGLItem);
    procedure GetCaption(Sender: TOutlookGroupedList; Item: POGLItem; var Caption: String);
    procedure GetItemHeight(Sender: TOutlookGroupedList; const OGLCanvas: TCanvas; var ItemHeight: Word);
    procedure SetColumnLineColor(const Value: TColor);
    procedure SetURLSettings(const Value: TURLSettings);
    procedure SetImages(const Value: TImageList);
    procedure SetCheckBoxStyle(const Value: TCheckBoxStyle);
    procedure SetProgressAppearance(const Value: TProgressAppearance);
    procedure SetPreviewSettings(const Value: TPreviewSetting);
    procedure SetHeaderBorderColor(const Value: TColor);
    procedure SetHeaderHeight(const Value: Integer);
    procedure SetItemHeight(const Value: Integer);
    function GetHeaderHeight: Integer;
    procedure SetBorderColor(const Value: TColor);
    procedure SetSortSettings(const Value: TSortSettings);
    procedure SetEllipsis(const Value: Boolean);
    procedure SetShadowOffset(const Value: Integer);
    procedure SetHover(const Value: boolean);
    procedure SetHoverColor(const Value: TColor);
    procedure SetHoverFontColor(const Value: TColor);
    procedure SetShadowColor(const Value: TColor);
    procedure SetGroupColumn(const Value: Integer);
    procedure SetSelectionColor(const Value: TColor);
    procedure SetSelectionTextColor(const Value: TColor);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetColor(const Value: TColor);
    procedure SetHeaderDragDrop(const Value: TDragDropSetting);
    procedure SetHeaderResize(const Value: boolean);
    procedure SetDragDropSetting(const Value: TDragDropSetting);
    function GetDropTargetGroup: POGLItem;
    function GetFocusedItem: POGLItem;
    procedure SetFocusedItem(const Value: POGLItem);
    function GetRootItem: POGLItem;
    procedure SetDragDropMode(const Value: TDragDropMode);
    function GetGroup(Index: Integer): TOutlookGroup;
    function GetGroupCount: Integer;
    procedure SetGroupItemHeight(const Value: Integer);
    function GetShowNodes: Boolean;
    procedure SetShowNodes(const Value: Boolean);
    function GetFirstGroupItem: POGLItem;
    function GetFirstSelectedItem: POGLItem;
    function GetSelectedCount: Integer;
    procedure SetGroupShowCount(const Value: Boolean);
    procedure SetHideSelection(const Value: Boolean);
    procedure SetLookUp(const Value: TLookUpSettings);
    procedure SetGroupColumnDisplay(const Value: TGroupColumnDisplay);
    procedure SetDragType(const Value: TOGLDragType);
    procedure SetSelectionOptions(const Value: TOGLSelectionOptions);
    procedure SetGroupColor(const Value: TColor);
    procedure SetGroupCountFont(const Value: TFont);
    procedure SetGroupFont(const Value: TFont);
    procedure SetGroupSelectionColor(const Value: TColor);
    procedure SetGroupSelectionTextColor(const Value: TColor);
    procedure SetGroupLineColor(const Value: TColor);
  protected
    procedure HeaderChanged(Sender: TObject);
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;

    procedure DoExpand(Sender: TOutlookGroupedList; Item: POGLItem);
    procedure DoCollaps(Sender: TOutlookGroupedList; Item: POGLItem);

    function HTMLPaint(Canvas:TCanvas; s:string; fr:TRect; FImages:TImageList;
                       xpos,ypos,focuslink,hoverlink,shadowoffset: Integer;
                       checkhotspot,checkheight,print,selected,blink,hoverstyle:boolean;
                       resfactor:double;
                       urlcolor,hovercolor,hoverfontColor,shadowcolor:TColor;
                       var anchorval,stripval,focusanchor:string;
                       var xsize,ysize,hyperlinks,mouselink: Integer;
                       var hoverrect:TRect):boolean; virtual;
    function IsAnchor(x,y: Integer; Col: Integer; CellRect: TRect; CellValue: String; var HoverRect: TRect):string;
    procedure SortItems;
    procedure ToggleCheck(Item: POGLItem; ColIndex: Integer);
    function ColAtPoint(ItemRect: TRect; X, Y: Integer; var ColRect: TRect): TAdvOutlookColumn;
    function GetCellRect(ItemRect: TRect; ColIndex: Integer): TRect;
    function PtOnItemText(Item: POGLItem; ColIndex: Integer; ColRect: TRect; P: TPoint): Boolean;
    function MapHeaderSecToCol(SectionIndex: Integer): Integer;
    function MapColToHeaderSec(ColIndex: Integer): Integer;
    procedure MoveHeaderSec(FromIndex, ToIndex: Integer);
    function IsGroupColumn(ColIndex: Integer): Boolean;
    function IsPreviewColumn(ColIndex: Integer): Boolean;

    function GetHeaderSecIndex(ColIndex: Integer): Integer;

    procedure UpdateHeaderSecIndexes;

    procedure SetGrouping;
    function MapColumn(Column: Integer): Integer;
    function GetLastCol: Integer;

    procedure Locate(s: String; Col: Integer);

    procedure ListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    property Ellipsis: Boolean read FEllipsis write SetEllipsis;
    property ShadowOffset: Integer read fShadowOffset write SetShadowOffset;
    property Hover:boolean read fHover write SetHover;
    property HoverColor:TColor read fHoverColor write SetHoverColor;
    property HoverFontColor:TColor read FHoverFontColor write SetHoverFontColor;
    property ShadowColor:TColor read fShadowColor write SetShadowColor;
    property ImageCache: THTMLPictureCache read FImageCache;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure TestFill;

    procedure SetFocus; override;

    function IsGroupExpanded(Item: POGLItem): Boolean;
    function ExpandItem(Item: POGLItem): Boolean;
    function CollapseItem(Item: POGLItem): Boolean;
    procedure ToggleExpandedItem(Item: POGLItem);

    function AddGroup(Caption: string): TOutlookGroup;
    function FindGroup(Item: POGLItem): TOutlookGroup;
    function InsertGroup(Index: Integer; Caption: string) : TOutlookGroup;
    function AddItem(Group: TOutlookGroup): TStrings; overload;
    function AddItem(ParentItem: POGLItem = nil): POGLItem; overload;
    procedure DeleteItem(Item: POGLItem; Reindex: Boolean = True);
    procedure DeleteSelectedItems(ReIndex: Boolean = True);

    procedure DeleteGroup(Index: Integer);
    procedure DeleteAllGroups;
    property Groups[Index: Integer]: TOutlookGroup read GetGroup;
    function GroupIndex(Group: TOutlookGroup): integer;
    procedure ClearGroups;

    function ColumnIndex(Column: Integer): Integer;
    procedure MoveColumn(FromColIndex, ToColIndex: Integer);

    property GroupColumn: Integer read FGroupColumn write SetGroupColumn default 0;
    property GroupCount: Integer read GetGroupCount;
    function ItemGroup(Item: POGLItem): TOutlookGroup;
    function GetVersionNr: integer;

    procedure Sort;
    procedure SelectAll;
    procedure SelectItem(Item: POGLItem; Focus: boolean = true);
    procedure UnSelectItem(Item: POGLItem);
    procedure UnselectAll;
    function ExpandAll: Boolean;
    function CollapseAll: Boolean;
    function IsItemSelected(Item: POGLItem): Boolean;
    function IsGroupItem(Item: POGLItem): Boolean;
    function ScrollIntoView(Item: POGLItem; AutoScrollOnExpand: Boolean = False): Boolean;
    procedure ScrollList(deltax,deltay: integer);
    function GetItemData(Item: POGLItem): TStrings; overload;
    function CaptionToItem(ACaption: String; const GroupItem: POGLItem = nil): POGLItem;
    property DropTargetGroup: POGLItem read GetDropTargetGroup;
    property FocusedItem: POGLItem read GetFocusedItem write SetFocusedItem;
    property RootItem: POGLItem read GetRootItem;
    property FirstGroupItem: POGLItem read GetFirstGroupItem;
    property FirstSelectedItem: POGLItem read GetFirstSelectedItem;
    property SelectedCount: Integer read GetSelectedCount;
    property List: TOutlookGroupedList read FList;
    function NextSelectedItem(Item: POGLItem): POGLItem;

    function ItemAtXY(X,Y: Integer): POGLItem;
    function GroupAtXY(X,Y: Integer): TOutlookGroup;

    procedure SetStyle(AStyle: TAdvOutlookListStyle);
    procedure SetComponentStyle(AStyle: TTMSStyle);
  published
    property Align;
    property Anchors;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;
    property CheckBoxStyle: TCheckBoxStyle read FCheckBoxStyle write SetCheckBoxStyle default cbsWinXP;
    property Columns: TAdvOutlookColumns read FColumns write FColumns;
    property ColumnLineColor: TColor read FColumnLineColor write SetColumnLineColor default clSilver;
    property Color: TColor read FColor write SetColor default clWindow;
    property Constraints;
    property DragDropMode: TDragDropMode read FDragDropMode write SetDragDropMode default ddmNormal;
    property DragDropSetting: TDragDropSetting read FDragDropSetting write SetDragDropSetting default ddEnabled;
    property GroupItemHeight: Integer read FGroupItemHeight write SetGroupItemHeight;
    property GroupShowCount: Boolean read FGroupShowCount write SetGroupShowCount;
    property HeaderBorderColor: TColor read FHeaderBorderColor write SetHeaderBorderColor default clSilver;
    property HeaderDragDrop: TDragDropSetting read FHeaderDragDrop write SetHeaderDragDrop default ddEnabled;
    property HeaderHeight: Integer read GetHeaderHeight write SetHeaderHeight default 20;
    property HeaderResize: boolean read FHeaderResize write SetHeaderResize default true;
    property HideSelection: Boolean read FHideSelection write SetHideSelection;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 20;
    property Images: TImageList read FImages write SetImages;
    property DragType: TOGLDragType read FDragType write SetDragType default dtOLE;
    property LookUp: TLookUpSettings read FLookUp write SetLookUp;
    property GroupColumnDisplay: TGroupColumnDisplay read FGroupColumnDisplay write SetGroupColumnDisplay;

    property PictureContainer: TPictureContainer read FContainer write FContainer;
    property PopupMenu;
    property ProgressAppearance: TProgressAppearance read FProgressAppearance write SetProgressAppearance;
    property PreviewSettings: TPreviewSetting read FPreviewSettings write SetPreviewSettings;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor default clHighlight;
    property SelectionTextColor: TColor read FSelectionTextColor write SetSelectionTextColor default clHighlightText;
    property SelectionOptions: TOGLSelectionOptions read FSelectionOptions write SetSelectionOptions default DefaultSelectionOptions;
    property GroupFont: TFont read FGroupFont write SetGroupFont;
    property GroupCountFont: TFont read FGroupCountFont write SetGroupCountFont;
    property GroupColor: TColor read FGroupColor write SetGroupColor default clNone;
    property GroupLineColor: TColor read FGroupLineColor write SetGroupLineColor default $00E0A47B;
    property GroupSelectionColor: TColor read FGroupSelectionColor write SetGroupSelectionColor default clHighLight;
    property GroupSelectionTextColor: TColor read FGroupSelectionTextColor write SetGroupSelectionTextColor default clWhite;

    property ShowHint;
    property ShowNodes: Boolean read GetShowNodes write SetShowNodes;
    property SortSettings: TSortSettings read FSortSettings write SetSortSettings;
    property TabOrder;
    property TabStop;
    property URLSettings: TURLSettings read FURLSettings write SetURLSettings;
    property Version: string read GetVersion write SetVersion;
    property Visible;

    property OnDrawItemProp: TDrawItemPropEvent read FOnDrawItemProp write FOnDrawItemProp;
    property OnDrawItemValue: TDrawItemValueEvent read FOnDrawItemValue write FOnDrawItemValue;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnURLClick: TURLClickEvent read FOnURLClick write FOnURLClick;

    property OnHeaderClick: TAOLHeaderClickEvent read FOnHeaderClick write FOnHeaderClick;
    property OnHeaderRightClick: TAOLHeaderClickEvent read FOnHeaderRightClick write FOnHeaderRightClick;
    property OnHeaderDblClick: TAOLHeaderClickEvent read FOnHeaderDblClick write FOnHeaderDblClick;
    property OnHeaderDragDrop: TOnHeaderDragDropEvent read FOnHeaderDragDrop write FOnHeaderDragDrop;
    property OnHeaderResized: THeaderResizedEvent read FOnHeaderResized write FOnHeaderResized;
    property OnHeaderHint: THeaderHintEvent read FOnHeaderHint write FOnHeaderHint;
    property OnItemClick: TAOLItemClickEvent read FOnItemClick write FOnItemClick;
    property OnItemRightClick: TAOLItemClickEvent read FOnItemRightClick write FOnItemRightClick;
    property OnItemDblClick: TAOLItemClickEvent read FOnItemDblClick write FOnItemDblClick;
    property OnGroupClick: TAOLItemEvent read FOnGroupClick write FOnGroupClick;
    property OnGroupDblClick: TAOLItemEvent read FOnGroupDblClick write FOnGroupDblClick;
    property OnGroupRightClick: TAOLItemEvent read FOnGroupRightClick write FOnGroupRightClick;
    property OnAnchorClick: TAnchorClick read FAnchorClick write FAnchorClick;
    property OnAnchorEnter: TAnchorClick read FAnchorEnter write FAnchorEnter;
    property OnAnchorExit: TAnchorClick read FAnchorExit write FAnchorExit;
    property OnGroupExpand: TAOLItemEvent read FOnGroupExpand write FOnGroupExpand;
    property OnGroupCollaps: TAOLItemEvent read FOnGroupCollaps write FOnGroupCollaps;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnCheckBoxClick: TCheckBoxClickEvent read FOnCheckBoxClick write FOnCheckBoxClick;
    property OnOLEDragOver: TAOLDragOverEvent read FOnOLEDragOver write FOnOLEDragOver;
    property OnOLEDrop: TAOLDropEvent read FOnOLEDrop write FOnOLEDrop;
    property OnOLEGetData: TAOLGetDataEvent read FOnOLEGetData write FOnOLEGetData;
    property OnOLEGetClipboardFormats: TAOLGetClipboardFormatsEvent read FOnOLEGetClipboardFormats write FOnOLEGetClipboardFormats;
    property OnOLEDragAllowed: TAOLDragAllowedEvent read FOnOLEDragAllowed write FOnOLEDragAllowed;
    property OnGetCaption: TAOLGetCaptionEvent read FOnGetCaption write FOnGetCaption;
    property OnOLEGetItemData: TOLEItemData read FOnOLEGetItemData write FOnOLEGetItemData;
    property OnOLEItemDrop: TOLEItemData read FOnOLEItemDrop write FOnOLEItemDrop;
    property OnCustomCompare: TOnCustomCompareEvent read FOnCustomCompare write FOnCustomCompare;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnSorted: TOnSortedEvent read FOnSorted write FOnSorted;
    property OnGetItemHint: TOnGetItemHintEvent read FOnGetItemHint write FOnGetItemHint;
    property OnGetGroupHint: TOnGetGroupHintEvent read FOnGetGroupHint write FOnGetGroupHint;

    property OnDragDrop;
    property OnDragOver;
    property OnKeyPress;//: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyDown;//: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyUp;//: TKeyEvent read FOnKeyUp write FOnKeyUp;
  end;


  TGaugeOrientation = (goHorizontal, goVertical);
  TGaugeSettings = record
    Level0Color: TColor;
    Level0ColorTo: TColor;
    Level1Color: TColor;
    Level1ColorTo: TColor;
    Level2Color: TColor;
    Level2ColorTo: TColor;
    Level3Color: TColor;
    Level3ColorTo: TColor;
    Level1Perc: Integer;
    Level2Perc: Integer;
    BorderColor: TColor;
    ShowBorder: Boolean;
    Stacked: Boolean;
    ShowPercentage: Boolean;
    Font: TFont;
    CompletionSmooth: Boolean;
    ShowGradient: Boolean;
    Steps: Integer;
    Position: Integer;
    BackgroundColor: TColor;
    Orientation: TGaugeOrientation;
  end;


implementation
{$R *.RES}

uses
  CommCtrl, ShellApi;

{$I HTMLENGO.PAS}

var
  CF_FILEGROUPDESCRIPTOR: TClipFormat;
  CF_FILECONTENTS: TClipFormat;

{$IFNDEF DELPHI6_LVL}

type
  PCardinal = ^Cardinal;




function StrToBool(s: string): boolean;
begin
  Result :=  Uppercase(s) = 'TRUE';
end;
{$ENDIF}

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

//---------------------------------------------------------------- DrawRectangle

procedure DrawRectangle(Canvas: TCanvas; R: TRect; aColor: TColor);
begin
  Canvas.Brush.Color := aColor;
  Canvas.FillRect(R);
end;

//-------------------------------------------------------------------- DrawGauge

procedure DrawGauge(Canvas: TCanvas; R: TRect; Position: Integer;
  Settings: TGaugeSettings);
var
  RectL: TRect;
  RectM: TRect;
  RectR: TRect;

  WidthBar: integer;
  WidthPart: Integer;
  Continue: Boolean;
  GradDir: Boolean;
  BrushColor: TColor;
  BrushColorTo: TColor;
  Percentage: Integer;
  BarFilled: Integer;
  NumberOfBlock: Integer;
  i: Integer;
  EmptyWidth: integer;

{$IFNDEF TMSCLX}
  lf: TLogFont;
{$ENDIF}
  tf: TFont;

  R1: TRect;
  R2: TRect;
begin
  if (Settings.Orientation = goHorizontal) then
    WidthBar := R.Right - R.Left
  else
    WidthBar := R.Bottom - R.Top;

  Continue := true;
  Percentage := -1;
  Canvas.Brush.Color := Settings.BackgroundColor;
  GradDir := not (Settings.Orientation = goHorizontal);

  if (Settings.ShowPercentage) then
    Percentage := Position;

  //Draw Border
  if (Settings.ShowBorder) then
    Canvas.Pen.Color := Settings.BorderColor
  else
    Canvas.Pen.Color := Settings.BackgroundColor;

  Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  
  WidthBar := WidthBar - 2;

  if (Position > 0) then
  begin
    if (Settings.Stacked) then
    begin
      if (Position >= Settings.Level1Perc) then
        WidthPart := Min(WidthBar, Round((Settings.Level1Perc / 100) * WidthBar))
      else
      begin
        WidthPart := Min(WidthBar, Round((Position / 100) * WidthBar));
        Continue := false;
      end;

      //Draw first part
      if (Settings.Orientation = goHorizontal) then
      begin
        RectL.Left := R.Left + 1;
        RectL.Top := R.Top + 1;
        RectL.Right := RectL.Left + WidthPart;
        RectL.Bottom := r.Bottom - 1;
      end
      else
      begin
        RectL.Left := r.Left + 1;
        RectL.Right := R.Right - 1;
        RectL.Top := R.Bottom - WidthPart;
        RectL.Bottom := R.Bottom - 1;
      end;

      if (Settings.ShowGradient) then
      begin
        if not (Settings.Orientation = goHorizontal) then
        begin
          R1.Left := RectL.Left;
          R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
          R1.Bottom := RectL.Bottom;
          R1.Top := RectL.Top;
          R2.Left := R1.Right;
          R2.Right := RectL.Right;
          R2.Bottom := RectL.Bottom;
          R2.Top := RectL.Top;
        end
        else
        begin
          R1.Left := RectL.Left;
          R1.Right := RectL.Right;
          R1.Top := RectL.Top;
          R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
          R2.Top := R1.Bottom;
          R2.Left := RectL.Left;
          R2.Right := RectL.Right;
          R2.Bottom := RectL.Bottom;
        end;
        DrawGradient(Canvas, Settings.Level0ColorTo,
          Settings.Level0Color, Settings.Steps, R1, GradDir);
        DrawGradient(Canvas, Settings.Level0Color,
          Settings.Level0ColorTo, Settings.Steps, R2, GradDir);
      end
      else
        DrawRectangle(Canvas, RectL, Settings.Level0Color);

      BarFilled := WidthPart;

      if (Continue) then
      begin
        //Draw second part
        if (Settings.Orientation = goHorizontal) then
        begin
          RectM.Left := RectL.Right;
          RectM.Top := r.Top + 1;
          RectM.Bottom := r.Bottom - 1;
        end
        else
        begin
          RectM.Left := R.Left + 1;
          RectM.Right := R.Right - 1;
          RectM.Bottom := RectL.Top;
        end;

        if (Position >= Settings.Level2Perc) then
          WidthPart := Round(WidthBar * ((Settings.Level2Perc -
            Settings.Level1Perc) / 100))
        else
        begin
          WidthPart := Round(WidthBar * ((Position -
            Settings.Level1Perc) / 100));
          Continue := false;
        end;

        if (Settings.Orientation = goHorizontal) then
        begin
          RectM.Right := WidthPart + RectM.Left;
        end
        else
        begin
          RectM.Top := RectM.Bottom - WidthPart;
        end;

        if (Settings.ShowGradient) then
        begin
          if not (Settings.Orientation = goHorizontal) then
          begin
            R1.Left := RectM.Left;
            R1.Right := RectM.Left + (RectM.Right - RectM.Left) div 2;
            R1.Bottom := RectM.Bottom;
            R1.Top := RectM.Top;
            R2.Left := R1.Right;
            R2.Right := RectM.Right;
            R2.Bottom := RectM.Bottom;
            R2.Top := RectM.Top;
          end
          else
          begin
            R1.Left := RectM.Left;
            R1.Right := RectM.Right;
            R1.Top := RectM.Top;
            R1.Bottom := RectM.Top + (RectM.Bottom - RectM.Top) div 2;
            R2.Top := R1.Bottom;
            R2.Left := RectM.Left;
            R2.Right := RectM.Right;
            R2.Bottom := RectM.Bottom;
          end;
          DrawGradient(Canvas, Settings.Level1ColorTo,
            Settings.Level1Color, Settings.Steps, R1, GradDir);
          DrawGradient(Canvas,
            Settings.Level1Color, Settings.Level1ColorTo,
            Settings.Steps, R2, GradDir);
        end
        else
          DrawRectangle(Canvas, RectM, Settings.Level1Color);

        BarFilled := BarFilled + WidthPart;
        if (Continue) then
        begin
          //Draw third part
          if (Position = 100) then
            WidthPart := Round(WidthBar - BarFilled)
          else
            WidthPart := Round(WidthBar * ((Position -
              Settings.Level2Perc) / 100));

          if (Settings.Orientation = goHorizontal) then
          begin
            RectR.Left := RectM.Right;
            RectR.Top := R.Top + 1;
            RectR.Bottom := r.Bottom - 1;
            RectR.Right := Min(R.Left + WidthBar, RectR.Left + WidthPart);
          end
          else
          begin
            RectR.Left := R.Left + 1;
            RectR.Right := R.Right - 1;
            RectR.Bottom := RectM.Top - 1;
            RectR.Top := Min(R.Bottom - WidthBar, RectR.Bottom - WidthPart);
          end;

          if (Settings.ShowGradient) then
          begin
            if not (Settings.Orientation = goHorizontal) then
            begin
              R1.Left := RectR.Left;
              R1.Right := RectR.Left + (RectR.Right - RectR.Left) div
                2;
              R1.Bottom := RectR.Bottom;
              R1.Top := RectR.Top;
              R2.Left := R1.Right;
              R2.Right := RectR.Right;
              R2.Bottom := RectR.Bottom;
              R2.Top := RectR.Top;
            end
            else
            begin
              R1.Left := RectR.Left;
              R1.Right := RectR.Right;
              R1.Top := RectR.Top;
              R1.Bottom := RectR.Top + (RectR.Bottom - RectR.Top) div
                2;
              R2.Top := R1.Bottom;
              R2.Left := RectR.Left;
              R2.Right := RectR.Right;
              R2.Bottom := RectR.Bottom;
            end;
            DrawGradient(Canvas, Settings.Level3ColorTo,
              Settings.Level3Color, Settings.Steps, R1, GradDir);
            DrawGradient(Canvas, Settings.Level3Color,
              Settings.Level3ColorTo, Settings.Steps, R2, GradDir);
          end
          else
            DrawRectangle(Canvas, RectR, Settings.Level3Color);
        end;
      end;
    end
    else
    begin
      if (Position < Settings.Level1Perc) then
      begin
        BrushColor := Settings.Level0Color;
        BrushColorTo := Settings.Level0ColorTo;
      end
      else
      begin
        if (Position < Settings.Level2Perc) then
        begin
          BrushColor := Settings.Level1Color;
          BrushColorTo := Settings.Level1ColorTo;
        end
        else
        begin
          if (Position < 100) then
          begin
            BrushColor := Settings.Level2Color;
            BrushColorTo := Settings.Level2ColorTo;
          end
          else
          begin
            BrushColor := Settings.Level3Color;
            BrushColorTo := Settings.Level3ColorTo;
          end;
        end;
      end;

      if not (Settings.CompletionSmooth) then
      begin
        Canvas.Brush.Color := Settings.BackgroundColor;

        if (Round((Position * WidthBar) / 100) > 9) then
        begin
          if (Settings.Orientation = goHorizontal) then
          begin
            RectL.Left := R.Left + 2;
            RectL.Right := RectL.Left + 7;
            RectL.Top := R.Top + 2;
            RectL.Bottom := R.Bottom - 2;
          end
          else
          begin
            RectL.Left := R.Left + 2;
            RectL.Right := R.Right - 2;
            RectL.Bottom := R.Bottom - 2;
            RectL.Top := RectL.Bottom - 7;
          end;

          if (Settings.ShowGradient) then
          begin
            if not (Settings.Orientation = goHorizontal) then
            begin
              R1.Left := RectL.Left;
              R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                2;
              R1.Bottom := RectL.Bottom;
              R1.Top := RectL.Top;
              R2.Left := R1.Right;
              R2.Right := RectL.Right;
              R2.Bottom := RectL.Bottom;
              R2.Top := RectL.Top;
            end
            else
            begin
              R1.Left := RectL.Left;
              R1.Right := RectL.Right;
              R1.Top := RectL.Top;
              R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                2;
              R2.Top := R1.Bottom;
              R2.Left := RectL.Left;
              R2.Right := RectL.Right;
              R2.Bottom := RectL.Bottom;
            end;
            DrawGradient(Canvas, BrushColorTo, BrushColor,
              Settings.Steps, R1, GradDir);
            DrawGradient(Canvas, BrushColor, BrushColorTo,
              Settings.Steps, R2, GradDir);
          end
          else
            DrawRectangle(Canvas, RectL, BrushColor);

          NumberOfBlock := (Round((Position * WidthBar) / 100) div 9) -
            1;
          EmptyWidth := Round((Position * WidthBar) / 100) mod 9;

          for i := 0 to NumberOfBlock - 1 do
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := RectL.Right + 2;
              RectL.Right := RectL.Left + 7;
            end
            else
            begin
              RectL.Bottom := RectL.Top - 2;
              RectL.Top := RectL.Bottom - 7;
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                  2;
                R1.Bottom := RectL.Bottom;
                R1.Top := RectL.Top;
                R2.Left := R1.Right;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
                R2.Top := RectL.Top;
              end
              else
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Right;
                R1.Top := RectL.Top;
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                  2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor,
                Settings.Steps, R1, GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo,
                Settings.Steps, R2, GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;

          if (EmptyWidth > 2) then
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := RectL.Right + 2;
              RectL.Right := RectL.Left + (EmptyWidth - 1);
            end
            else
            begin
              RectL.Bottom := RectL.Top - 2;
              RectL.Top := RectL.Bottom - (EmptyWidth - 1);
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                  2;
                R1.Bottom := RectL.Bottom;
                R1.Top := RectL.Top;
                R2.Left := R1.Right;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
                R2.Top := RectL.Top;
              end
              else
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Right;
                R1.Top := RectL.Top;
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                  2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor,
                Settings.Steps, R1, GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo,
                Settings.Steps, R2, GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;
          Canvas.Brush.style := bsClear;
        end
        else
        begin
          if (Round((Position * WidthBar) / 100) > 1) then
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := R.Left + 2;
              RectL.Right := RectL.Left + (Round((Position *
                WidthBar) / 100) - 1);
              RectL.Top := R.Top + 2;
              RectL.Bottom := R.Bottom - 2;
            end
            else
            begin
              RectL.Left := R.Left + 2;
              RectL.Right := R.Right - 2;
              RectL.Bottom := R.Bottom - 2;
              RectL.Top := RectL.Bottom - (Round((Position *
                WidthBar) / 100) - 1);
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                  2;
                R1.Bottom := RectL.Bottom;
                R1.Top := RectL.Top;
                R2.Left := R1.Right;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
                R2.Top := RectL.Top;
              end
              else
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Right;
                R1.Top := RectL.Top;
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                  2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor,
                Settings.Steps, R1, GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo,
                Settings.Steps, R2, GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;
        end;
      end
      else
      begin
        WidthPart := Min(WidthBar, Round((Position / 100) * WidthBar));

        if (Settings.Orientation = goHorizontal) then
        begin
          RectL.Left := R.Left + 1;
          RectL.Top := R.Top + 1;
          RectL.Right := RectL.Left + WidthPart;
          RectL.Bottom := R.Bottom - 1;
        end
        else
        begin
          RectL.Left := r.Left + 1;
          RectL.Bottom := R.Bottom - 1;
          RectL.Top := RectL.Bottom - WidthPart;
          RectL.Right := r.Right - 1;
        end;

        if (Settings.ShowGradient) then
        begin
          if not (Settings.Orientation = goHorizontal) then
          begin
            R1.Left := RectL.Left;
            R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
            R1.Bottom := RectL.Bottom;
            R1.Top := RectL.Top;
            R2.Left := R1.Right;
            R2.Right := RectL.Right;
            R2.Bottom := RectL.Bottom;
            R2.Top := RectL.Top;
          end
          else
          begin
            R1.Left := RectL.Left;
            R1.Right := RectL.Right;
            R1.Top := RectL.Top;
            R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
            R2.Top := R1.Bottom;
            R2.Left := RectL.Left;
            R2.Right := RectL.Right;
            R2.Bottom := RectL.Bottom;
          end;
          DrawGradient(Canvas, BrushColorTo, BrushColor,
            Settings.Steps, R1, GradDir);
          DrawGradient(Canvas, BrushColor, BrushColorTo,
            Settings.Steps, R2, GradDir);
        end
        else
          DrawRectangle(Canvas, RectL, BrushColor);
      end;
    end;
  end;

  //Draw text with PositionPercentage
  if (Percentage <> -1) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Name := Settings.Font.Name;
    Canvas.Font.Size := Settings.Font.Size;
    Canvas.Font.Color := Settings.Font.Color;
    Canvas.Font.Style := Settings.Font.Style;
    if not (Settings.Orientation = goHorizontal) then
    begin
      tf := TFont.Create;
      try
        tf.Assign(Settings.Font);

{$IFNDEF TMSCLX}

        GetObject(tf.Handle, sizeof(lf), @lf);

        lf.lfEscapement := 900;
        lf.lfOrientation := 900;
        tf.Handle := CreateFontIndirect(lf);
{$ENDIF}

        Canvas.Font.Assign(tf);
        Canvas.TextOut(R.Left + ((R.Right - R.Left) div 2 -
          (Canvas.TextHeight(IntToStr(Percentage) + '%') div 2)), R.Top +
          ((R.Bottom
          - R.Top) div 2) + Canvas.TextWidth(IntToStr(Percentage) + '%') div 2
          , IntToStr(Percentage) + '%');
      finally
        tf.Free;
      end;
    end
    else
    begin
      Canvas.TextOut(((R.Right - R.Left) div 2) -
        (Canvas.TextWidth(IntToStr(Percentage) + '%') div 2) + r.Left, r.Top +
        ((R.Bottom - R.Top) div 2) - Canvas.TextHeight(IntToStr(Percentage) +
        '%') div 2, IntToStr(Percentage) + '%');
    end;
  end;

  if (Settings.ShowBorder) then
    Canvas.Pen.Color := Settings.BorderColor
  else
    Canvas.Pen.Color := Settings.BackgroundColor;

  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
end;

//------------------------------------------------------------------------------

procedure DrawCheck(Canvas: TCanvas; R:TRect; State, Enabled: Boolean; Color: TColor; CheckBoxStyle: TCheckBoxStyle);
var
  DrawState: Integer;
  DrawRect: TRect;
  BMP: TBitmap;
  //HTheme: THandle;
begin
  //if ControlLook.NoDisabledCheckRadioLook then
    //Enabled := true;

  case CheckBoxStyle of
  cbsClassic,cbsFlat:
    begin
      if State then
        DrawState := DFCS_BUTTONCHECK or DFCS_CHECKED
      else
        DrawState := DFCS_BUTTONCHECK;

      if CheckBoxStyle = cbsFlat then
        DrawState := DrawState or DFCS_FLAT;

      if not Enabled then
        DrawState := DrawState or DFCS_INACTIVE;

      DrawRect.Left := R.Left + (R.Right - R.Left - CHECKBOX_SIZE) div 2;
      DrawRect.Top:= R.Top + (R.Bottom - R.Top - CHECKBOX_SIZE) div 2;
      DrawRect.Right := DrawRect.Left + CHECKBOX_SIZE;
      DrawRect.Bottom := DrawRect.Top + CHECKBOX_SIZE;

      DrawFrameControl(Canvas.Handle,DrawRect,DFC_BUTTON,DrawState);
    end;
 (* csTMS:
    begin
      Bmp := TBitmap.Create;
      if State then
      begin
        if Enabled then
          Bmp.LoadFromResourceName(hinstance,'ASGCHK01')
        else
          Bmp.LoadFromResourceName(hinstance,'ASGCHK03');
      end
      else
      begin
        if Enabled then
          Bmp.LoadFromResourceName(hinstance,'ASGCHK02')
        else
          Bmp.LoadFromResourceName(hinstance,'ASGCHK04');
      end;

      Bmp.Transparent := True;
      Bmp.TransparentMode := tmAuto;

      Canvas.Draw(R.Left,R.Top,bmp);
      Bmp.free;
    end;
  csGlyph:
    begin
      if State and not ControlLook.CheckedGlyph.Empty then
      begin
        ControlLook.CheckedGlyph.Transparent := True;
        ControlLook.CheckedGlyph.TransparentMode := tmAuto;
        Canvas.Draw(R.Left,R.Top,ControlLook.CheckedGlyph);
      end;

      if not State and not ControlLook.UnCheckedGlyph.Empty then
      begin
        ControlLook.UnCheckedGlyph.Transparent := True;
        ControlLook.UnCheckedGlyph.TransparentMode := tmAuto;
        Canvas.Draw(R.Left,R.Top,ControlLook.UnCheckedGlyph);
      end;
    end;
  csTheme:
    begin
      if FIsWinXP then
      begin
        HTheme := OpenThemeData(Self.Handle,'button');

        r := Rect(R.Left, R.Top, R.Left + FControlLook.CheckSize, R.Top + FControlLook.CheckSize);

        if State then
        begin
          if Enabled then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDNORMAL,@r,nil)
          else
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDDISABLED,@r,nil);
        end
        else
        begin
          if Enabled then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDNORMAL,@r,nil)
          else
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDDISABLED,@r,nil);
        end;

        CloseThemeData(HTheme);

      end;
    end; *)
  cbsWinXP:
    begin
      Bmp := TBitmap.Create;
      try
        if State then
        begin
          if Enabled then
            Bmp.LoadFromResourceName(hinstance,'AOLCHK01')
          else
            Bmp.LoadFromResourceName(hinstance,'AOLCHK03');
        end
        else
        begin
          if Enabled then
            Bmp.LoadFromResourceName(hinstance,'AOLCHK02')
          else
            Bmp.LoadFromResourceName(hinstance,'AOLCHK04');
        end;

        Bmp.Transparent := True;
        Bmp.TransparentMode := tmAuto;

        DrawRect.Left := R.Left + (R.Right - R.Left - CHECKBOX_SIZE) div 2;
        DrawRect.Top:= R.Top + (R.Bottom - R.Top - CHECKBOX_SIZE) div 2;

        Canvas.Draw(DrawRect.Left,DrawRect.Top,bmp);
      finally
        Bmp.free;
      end;
    end;
  cbsBorland:
    begin
      if Enabled then
        Canvas.Brush.Color := clBtnFace
      else
        Canvas.Brush.Color := clBtnShadow;

      DrawRect.Left := R.Left + (R.Right - R.Left - CHECKBOX_SIZE) div 2;
      DrawRect.Top:= R.Top + (R.Bottom - R.Top - CHECKBOX_SIZE) div 2;
      DrawRect.Right := DrawRect.Left + CHECKBOX_SIZE;
      DrawRect.Bottom := DrawRect.Top + CHECKBOX_SIZE;
      R := DrawRect;

      Canvas.Pen.Color := clBtnFace;
      Canvas.Rectangle(R.Left,R.Top,R.Right,R.Bottom);
      Canvas.Pen.Color := clBtnHighLight;
      Canvas.MoveTo(R.Left,R.Bottom);
      Canvas.LineTo(R.Left,R.Top);
      Canvas.LineTo(R.Right,R.Top);
      Canvas.Pen.Color := clBtnShadow;
      Canvas.LineTo(R.Right,R.Bottom);
      Canvas.LineTo(R.Left,R.Bottom);

      if State then
      begin
        if Enabled then
          Canvas.Pen.Color := {FControlLook.}Color
        else
          Canvas.Pen.Color := clGray;

        Canvas.Pen.Width := 1;
        Dec(R.Top);
        Dec(R.Bottom);
        Canvas.MoveTo(R.Left + 2,R.Top + CHECKBOX_SIZE div 2 + 1);
        Canvas.LineTo(R.Left + 2,R.Bottom - 1);
        Canvas.MoveTo(R.Left + 3,R.Top + CHECKBOX_SIZE div 2);
        Canvas.LineTo(R.Left + 3,R.Bottom - 2);
        Canvas.MoveTo(R.Left + 2,R.Bottom - 1);
        Canvas.LineTo(R.Right - 2,R.Top + 3);
        Canvas.MoveTo(R.Left + 3,R.Bottom - 1);
        Canvas.LineTo(R.Right - 1,R.Top + 3);
      end;
    end;
  end;
end;

procedure TAdvOutlookList.ClearGroups;
begin
  FList.Clear;
end;



//------------------------------------------------------------------------------

{ TAdvOutlookList }

function TAdvOutlookList.IsGroupExpanded(Item: POGLItem): Boolean;
begin
  Result := False;
  if Assigned(FList) then
    Result := FList.IsGroupExpanded(Item);
end;

function TAdvOutlookList.ExpandItem(Item: POGLItem): Boolean;
begin
  Result := False;
  if Assigned(FList) then
    Result := FList.ExpandItem(Item);
end;

function TAdvOutlookList.CollapseItem(Item: POGLItem): Boolean;
begin
  Result := False;
  if Assigned(FList) then
    Result := FList.CollapseItem(Item);
end;

procedure TAdvOutlookList.ToggleExpandedItem(Item: POGLItem);
begin
  if Assigned(FList) then
    FList.ToggleExpandedItem(Item);
end;

function TAdvOutlookList.FindGroup(Item: POGLItem): TOutlookGroup;
var
  i: integer;
  olg: TOutlookGroup;
begin
  Result := nil;

  for i := 0 to FGroupList.Count - 1 do
  begin
    olg := TOutlookGroup(FGroupList.Items[i]);
    if olg.OGLItem = Item then
    begin
      Result := olg;
      break;
    end;
  end;
end;

function TAdvOutlookList.AddGroup(Caption: string): TOutlookGroup;
var
  poi: POutlookInfo;
  olg: TOutlookGroup;
begin
  olg := TOutlookGroup.Create;
  FGroupList.Add(pointer(olg));
  olg.OGLItem := FList.AddItem(nil);
  olg.List := Self;
  poi := FList.GetItemData(olg.OGLItem);
  poi.data := TStringList.Create;
  poi.data.Add(Caption);
  Result := olg;
end;

//------------------------------------------------------------------------------
function TAdvOutlookList.InsertGroup(Index: Integer; Caption: String) : TOutlookGroup;
var
  poi: POutlookInfo;
  olg: TOutlookGroup;
begin
  olg := TOutlookGroup.Create;
  FGroupList.Add(pointer(olg));
  if Cardinal( Index) >= FList.RootItem.ChildCount then
    olg.OGLItem := FList.AddItem(nil)
  else
    olg.OGLItem := FList.InsertItem(nil, Index);
  olg.List := Self;
  poi := FList.GetItemData(olg.OGLItem);
  poi.data := TStringList.Create;
  poi.data.Add(Caption);
  Result := olg;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.AddItem(Group: TOutlookGroup): TStrings;
var
  poi: POutlookInfo;
  OGLItem: POGLItem;
begin
  OGLItem := FList.AddItem(Group.OGLItem);
  poi := FList.GetItemData(OGLItem);
  poi.data := TStringList.Create;
  Result := poi.data;
end;

//------------------------------------------------------------------------------

constructor TAdvOutlookList.Create(AOwner: TComponent);
begin
  FBorderStyle := bsNone;
  inherited;
  FList := TOutlookGroupedList.Create(Self);
  Flist.OnMouseMove := OnListMouseMove;
  TProOutlookGroupedList(FList).OnMouseDownOnItem := OnListMouseDownOnItem;
  TProOutlookGroupedList(FList).OnMouseUpOnItem := OnListMouseUpOnItem;
  FList.OnItemDblClick := OnListItemDblClick;
  FList.OnKeyDown := OnListKeyDown;
  FList.OnKeyUp := OnListKeyUp;
  FList.OnKeyPress := OnListKeyPress;
  FList.AutoOptions := FList.AutoOptions - [toAutoSort, toAutoScrollOnExpand];
  FList.OnCompareItems := OnListCompareItems;
  FList.OnGetHint := OnListGetHint;
  FList.OnGetGroupImageIndex := OnListGetGroupImageIndex;
  FList.OnExpandItem := DoExpand;
  FList.OnCollapsItem := DoCollaps;
  FList.OnMouseDown := ListMouseDown;
  FList.OnMouseUp := ListMouseUp;
  FList.TabStop := False;

  TProOutlookGroupedList(FList).OnSelectionChange := OnListSelectionChange;
  FHideSelection := True;
  TProOutlookGroupedList(FList).HideSelection := FHideSelection;
  FList.SearchType := OutlookGroupedList.stNone;  // for LookUp here
  TProOutlookGroupedList(FList).ConsiderColHint := True;
  FGroupItemHeight := TProOutlookGroupedList(FList).GroupItemHeight;
  FList.ShowHint := True;
  FGroupShowCount := True;
  TProOutlookGroupedList(FList).GroupShowCount := FGroupShowCount;
  FHeader := TAdvOutLookHeader.Create(Self);//THeader.Create(Self);
  FHeader.OnSized := OnHeaderSized;
  FHeader.OnClick := OnHeaderSectClick;
  FHeader.OnRightClick := OnHeaderSectRightClick;
  FHeader.OnDblClick := OnHeaderSectDblClick;
  FColumns := TAdvOutlookColumns.Create(self);
  FColumns.AdvOutLookList := Self;
  FColumns.OnChange := HeaderChanged;
  FColumns.OnRefresh := OnColumnRefresh;
  FItemHeight := 20;
  FHeaderHeight := 20;
  FSelectionOptions := DefaultSelectionOptions;
  FHeaderResize := true;

  GroupColor := clNone;
  GroupSelectionColor := clHighLight;
  GroupSelectionTextColor := clWhite;
  FGroupFont := TFont.Create;
  FGroupFont.OnChange := OnGroupFontChanged;
  FGroupFont.Color := $00B96837;
  FGroupFont.Style := [fsBold];
  FGroupLineColor := $00E0A47B;

  FGroupCountFont := TFont.Create;
  FGroupCountFont.OnChange := OnGroupCountFontChanged;
  GroupCountFont.Color := clBlack;
  {
  with FColumns.Add do
  begin
    Caption := 'Data column 1';
  end;
  with FColumns.Add do
  begin
    Caption := 'Data column 2';
  end;
  with FColumns.Add do
  begin
    Caption := 'Data column 3';
  end; }

  FList.ItemDataSize  := sizeof(TOutlookInfo);
  FList.OnFreeItem := FreeItem;
  FList.OnDrawItem := DrawItem;
  FList.OnGetCaption := GetCaption;
  FList.OnGetChildItemHeight := GetItemHeight;

  FDragType := dtOLE;
  //--- Drag Events
  FList.DragType := DragType;
  FList.DragMode := dmManual;
  FList.DragOperations := [doCopy,doMove];
  FList.OnOLEDragOver := OnListOLEDragOver;
  FList.OnOLEDragAllowed := OnListOLEDragAllowed;
  FList.OnOLEGetData := OnListOLEGetData;
  FList.OnOLEDrop := OnListOLEDrop;
  FList.OnOLEGetClipboardFormats := OnListOLEGetClipboardFormats;

  FList.OnDragOver := OnListDragOver;
  FList.OnDragDrop := OnListDragDrop;
  //---

  FColumnLineColor := clSilver;
  FHeaderBorderColor := clSilver;
  FURLSettings := TURLSettings.Create;
  FURLSettings.OnChange := OnURLSettingsChange;
  FCheckBoxStyle := cbsWinXP;

  FProgressAppearance := TProgressAppearance.Create;
  FProgressAppearance.OnChange := OnProgressAppearanceChange;

  FPreviewSettings := TPreviewSetting.Create;
  FPreviewSettings.OnChange := OnPreviewSettingChange;
  FPreviewedColumn := -1;

  BorderStyle := bsSingle;
  FBorderColor := clGray;
  FSortSettings := TSortSettings.Create;
  FSortSettings.OnChange := OnSortChange;
  FSelectionColor := clHighlight;
  FSelectionTextColor := clHighlightText;

  FImageCache := THTMLPictureCache.Create;
  FHoverHyperLink := -1;
  FShadowOffset := 2;
  FShadowColor := clGray;
  FHover := False;
  FHoverColor := clNone;
  FHoverFontColor := clNone;
  FColor := clWhite;

  FDragDropSetting := ddEnabled;
  FHeaderDragDrop := ddEnabled;

  FMouseOverCol := -1;
  //ParentShowHint := True;
  self.ShowHint := True;

  FDragDropMode := ddmNormal;
  FDragFileList := TStringList.Create;

  FGroupColumn := 0;
  FCurrentGroupColumn := FGroupColumn;
  Width := 400;
  Height := 300;

  FLookUp := TLookUpSettings.Create;
  FLookUpText := '';
  FGroupColumnDisplay := gdHidden;
  FGroupList := TList.Create;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or BorderStyles[FBorderStyle];
  if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) and False then
  begin
    Params.Style := Params.Style and not WS_BORDER;
    Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.CreateWnd;
begin
  inherited;
  FList.Parent := Self;
  FHeader.Parent := Self;

  FHeader.Align := alTop;
  FHeader.Height := FHeaderHeight;
  FHeader.BorderStyle := bsNone;
  FList.Align := alClient;
  FList.BorderStyle := bsNone;
  FList.ViewStyle := vsList;
  FList.DragMode := dmManual;
  FList.SelectionOptions := FSelectionOptions;

  if (FColumns.Count = 0) then
  begin
    with FColumns.Add do
    begin
      Caption := 'Group Column';
    end;
    with FColumns.Add do
    begin
      Caption := 'column '+IntToStr(Index);
    end;
    with FColumns.Add do
    begin
      Caption := 'column '+IntToStr(Index);
    end;
    with FColumns.Add do
    begin
      Caption := 'column '+IntToStr(Index);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.DeleteItem(Item: POGLItem; Reindex: Boolean);
begin
  FList.DeleteItem(Item, ReIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.DeleteSelectedItems(ReIndex: Boolean = true);
begin
  FList.DeleteSelectedItems(ReIndex);
end;

//------------------------------------------------------------------------------

destructor TAdvOutlookList.Destroy;
var
  olg: TOutlookGroup;
begin
  FHeader.Free;
  FList.Free;
  FColumns.Free;
  FProgressAppearance.Free;
  FURLSettings.Free;
  FPreviewSettings.Free;
  FSortSettings.Free;
  FImageCache.ClearPictures;
  FImageCache.Free;
  FDragFileList.Free;
  FLookUp.Free;

  while FGroupList.Count > 0 do
  begin
    olg := TOutlookGroup(FGroupList.Items[0]);
    olg.Free;
    FGroupList.Delete(0);
  end;

  FGroupList.Free;
  FGroupFont.Free;
  FGroupCountFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.DrawItem(Sender: TOutlookGroupedList;
  ItemCanvas: TCanvas; ItemRect: TRect; Item: POGLItem);
var
  poi, Poi1: POutlookInfo;
  i, {x, cw,} x2, y, hyperlinks, mouselink, lastcol: Integer;
  S: String;
  ColRect, DrawRect, TxtRect, mr: TRect;
  format: cardinal;
  Ip, pt: TPoint;
  Anchor, Stripped, FocusAnchor: string;
  HTMLTextColor: TColor;

  procedure DrawCompletion(ACanvas: TCanvas; R: TRect; Completion: Integer; OldStyle: Boolean);
  var
    SrcColor: TColor;
    SrcRect, TgtRect: TRect;
    W, H: Integer;
    Txt: string;
    FS: TFontStyles;
    Settings: TGaugeSettings;

  begin
    inflaterect(r,-1,-1);
    if OldStyle then
    begin
      SrcColor := ACanvas.Brush.Color;
      ACanvas.Brush.Color := Color;
      ACanvas.Brush.Color := FProgressAppearance.CompleteColor;
      ACanvas.Pen.Color := FProgressAppearance.CompleteColor;
      ACanvas.Font.Color := FProgressAppearance.CompleteFontColor;
      FS := ACanvas.Font.Style;
      ACanvas.Font.Style := [];
      InflateRect(R, -2, -2);
      SrcRect := R;
      W := R.Right - R.Left;
      H := R.Bottom - R.Top;
      Txt := IntToStr(Completion) + '%';
      if Completion > 100 then
        Completion := 100;
      SrcRect.Right := SrcRect.Left + Round(W * Completion / 100);
      TgtRect.Left := R.Left + ((W - ACanvas.Textwidth(Txt)) shr 1);
      TgtRect.Top := R.Top + ((H - ACanvas.Textheight(Txt)) shr 1);
      ACanvas.TextRect(SrcRect, TgtRect.Left, TgtRect.Top, Txt);

      ACanvas.Brush.Color := FProgressAppearance.UnCompleteColor;
      ACanvas.Pen.Color := FProgressAppearance.UnCompleteColor;
      ACanvas.Font.Color := FProgressAppearance.UnCompleteFontColor;

      SrcRect.Left := SrcRect.Right;
      SrcRect.Right := R.Right;
      ACanvas.TexTRect(SrcRect, TgtRect.Left, TgtRect.Top, Txt);

      ACanvas.Brush.Color := SrcColor;
      ACanvas.Pen.Color := SrcColor;
      Inflaterect(R, 1, 1);
      ACanvas.FrameRect(R);
      Inflaterect(R, 1, 1);
      ACanvas.FrameRect(R);
      ACanvas.Font.Style := FS;
    end
    else
    begin
      Settings.Level0Color := FProgressAppearance.Level0Color;
      Settings.Level0ColorTo := FProgressAppearance.Level0ColorTo;
      Settings.Level1Color := FProgressAppearance.Level1Color;
      Settings.Level1ColorTo := FProgressAppearance.Level1ColorTo;
      Settings.Level2Color := FProgressAppearance.Level2Color;
      Settings.Level2ColorTo := FProgressAppearance.Level2ColorTo;
      Settings.Level3Color := FProgressAppearance.Level3Color;
      Settings.Level3ColorTo := FProgressAppearance.Level3ColorTo;
      Settings.Level1Perc := FProgressAppearance.Level1Perc;
      Settings.Level2Perc := FProgressAppearance.Level2Perc;
      Settings.ShowBorder := FProgressAppearance.ShowBorder;
      Settings.Stacked := FProgressAppearance.Stacked;
      Settings.ShowPercentage := FProgressAppearance.ShowPercentage;
      Settings.CompletionSmooth := FProgressAppearance.CompletionSmooth;
      Settings.ShowGradient := FProgressAppearance.ShowGradient;
      Settings.Font  := ACanvas.Font;
      Settings.Font.Color := FProgressAppearance.FCompleteFontColor;
      Settings.Orientation := goHorizontal;
      Settings.Steps := FProgressAppearance.Steps;

      if FProgressAppearance.UnCompleteColor <> clNone then
        Settings.BackgroundColor := FProgressAppearance.UnCompleteColor
      else
        Settings.BackgroundColor := ACanvas.Brush.Color;

      DrawGauge(ACanvas, R, Completion, Settings);
    end;
  end;

begin
  poi := FList.GetItemData(Item);

  if poi = nil then
    Exit;
    
  if poi.data = nil then
    Exit;

 { if Sender.Focused and Sender.IsItemSelected(Item) then
    ItemCanvas.Font.Color := clHighlightText
  else
    ItemCanvas.Font.Color := clWindowText;

  if poi.data.Count > 0 then
    ItemCanvas.TextOut(ItemRect.Left, ItemRect.Top, poi.data[0]);  }


  lastcol := GetLastCol;

  // Draw Columns
  with ItemCanvas do
  begin
    //x := ItemRect.Left;
    for i := 0 to FColumns.Count - 1 do
    begin
      if ((I = FGroupColumn) and (FGroupColumnDisplay = gdHidden)) or not FColumns.Items[i].Visible then
        Continue;

      with FColumns.Items[i] do
      begin
       { if FHeader.Sections.Count > i then
          cw := FHeader.SectionWidth[MapColToHeaderSec(i)]
        else
          cw := FColumns.Items[i].Width;
        ColRect := Rect(x, ItemRect.Top, x + cw -1, ItemRect.Bottom);
        }
        ColRect := GetCellRect(ItemRect, i);

        //cw := ColRect.Right - ColRect.Left;

        S := '';
        if (poi.data.Count > MapColumn(i)) or (((I = FGroupColumn) and (FGroupColumnDisplay = gdVisible))) then
        begin
          if ((I = FGroupColumn) and (FGroupColumnDisplay = gdVisible)) then
          begin
            if (Item.Parent <> nil) then
            begin
              poi1 := FList.GetItemData(Item.Parent);
              if poi1.data.Count >= 1 then
                S := poi1.data[0];
            end;
          end
          else
            S := poi.data[MapColumn(i)];
        end;

       { if (FPreviewSettings.Active) then
        begin
          if (FPreviewSettings.Column = i) then
          begin
            ColRect.Top := ColRect.Top + (ColRect.Bottom - ColRect.Top) - FPreviewSettings.Height;
            ColRect.Left := ItemRect.Left;
            ColRect.Right := ItemRect.Right;
          end
          else
          begin
            ColRect.Bottom := ColRect.Top + (ColRect.Bottom - ColRect.Top) - FPreviewSettings.Height;
            x := x + cw;
          end;
        end
        else
        begin
          x := x + cw;
        end;
       }
        if Assigned(FOnDrawItem) then
          FOnDrawItem(self, Item, i, S, ItemCanvas, ColRect)
        else
        begin
          ItemCanvas.Font.Assign(FColumns.Items[i].Font);
          if FColumnType = ctURL then
          begin
            ItemCanvas.Font.Style := FURLSettings.FontStyle;
            ItemCanvas.Font.Color := FURLSettings.Color;
          end;

          if (FPreviewSettings.Active) and (FPreviewSettings.Column = i) then
            ItemCanvas.Font.Assign(FPreviewSettings.Font);

          if (Sender.Focused or not HideSelection) and Sender.IsItemSelected(Item) then
          begin
            ItemCanvas.Font.Color := FSelectionTextColor;
            ItemCanvas.Brush.Color := FSelectionColor;

            if Assigned(FOnDrawItemProp) then
              FOnDrawItemProp(self, Item, i, S, ItemCanvas.Brush, ItemCanvas.Font);
          end
          else
          begin
            if FColumns.Items[i].Color <> clNone then
            begin
              Pen.Color := FColumns.Items[i].Color;
              Brush.Color := FColumns.Items[i].Color;
            end
            else
            begin
              Pen.Color := Self.Color;
              Brush.Color := Self.Color;
            end;

            if Assigned(FOnDrawItemProp) then
              FOnDrawItemProp(self, Item, i, S, ItemCanvas.Brush, ItemCanvas.Font);

            if (self.ColumnLineColor = clNone) then      // FF: show Column line when selection with ColumnLineColor = clNone
              Rectangle(ColRect.Left-1, ColRect.Top, ColRect.Right, ColRect.Bottom)
            else
              Rectangle(ColRect.Left+1, ColRect.Top, ColRect.Right, ColRect.Bottom);
          end;

          if Assigned(FOnDrawItemValue) then
            FOnDrawItemValue(self, Item, i, S);

          if (MapColToHeaderSec(i) < FHeader.Sections.Count-1{FColumns.Count-1}) and (self.ColumnLineColor <> clNone) then
          begin
            Brush.Style := bsClear;
            Pen.Color := Self.ColumnLineColor;
            if ((FPreviewSettings.Active) and (FPreviewSettings.Column = i)) then
            begin
              MoveTo(ColRect.Left, ColRect.Top);
              LineTo(ColRect.Right, ColRect.Top);
            end
            else
            begin
              MoveTo(ColRect.Left + FColumns.Items[i].Width -1, ColRect.Top);
              LineTo(ColRect.Left + FColumns.Items[i].Width -1, ColRect.Bottom);
            end;
          end;

          if (poi.data.Count > MapColumn(i)) or (((I = FGroupColumn) and (FGroupColumnDisplay = gdVisible))) then
          begin
            //S := poi.data[i];

            case ColumnType of
              ctText:
              begin
                //ItemCanvas.Font.Assign(FColumns.Items[i].Font);
                //if Sender.Focused and Sender.IsItemSelected(Item) then
                  //ItemCanvas.Font.Color := clHighlightText;

                TxtRect := ColRect;
                TxtRect.Left := TxtRect.Left+2;

                case FColumns.Items[i].alignment of
                  taLeftJustify:  format := DT_LEFT;
                  taRightJustify: format := DT_RIGHT;
                  else            format := DT_CENTER;
                end;

                if (i = lastcol) then
                  if FList.VertScrollBar.Visible then
                    TxtRect.Right := TxtRect.Right - GetSystemMetrics(SM_CXVSCROLL);

                ItemCanvas.Brush.Style := bsClear;
                if pos('</', S) > 0 then
                begin
                  GetCursorPos(pt);
                  pt := self.ScreenToClient(pt);

                  if (FPreviewSettings.Active) and (FPreviewSettings.Column = i) then
                    HTMLTextColor := FPreviewSettings.Font.Color
                  else
                    HTMLTextColor := FURLSettings.Color;

                  HTMLPaint(ItemCanvas,S,TxtRect,FImages,pt.x,pt.y,-1,FHoverHyperLink,FShadowOffset,False,False,False,False,False,FHover,
                            1.0, HTMLTextColor,FHoverColor,FHoverFontColor,FShadowColor,Anchor,Stripped,FocusAnchor,x2,y,HyperLinks,mouselink,mr);
                end
                else
                begin
                  if ((FPreviewSettings.Active) and (FPreviewSettings.Column = i)) then
                    DrawText(ItemCanvas.Handle, PChar(S), Length(S), TxtRect, DT_WORDBREAK or DT_NOPREFIX or DT_VCENTER or format)
                  else
                    DrawText(ItemCanvas.Handle, PChar(S), Length(S), TxtRect, DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER or format);
                end;

              end;
              ctImage:
              begin
                if Assigned(FImages) and (StrToInt(S) >= 0) then
                begin
                  Ip.Y := ColRect.Top + ((ColRect.Bottom - ColRect.Top) - fImages.Height) div 2;
                  case FColumns.Items[i].alignment of
                    taLeftJustify:  Ip.X := ColRect.Left;
                    taRightJustify: Ip.X := ColRect.Right - FImages.Width;
                    else            Ip.X := ColRect.Left + ((ColRect.Right - ColRect.Left) - fImages.Width) div 2;
                  end;

                  FImages.Draw(ItemCanvas, Ip.X, Ip.Y, StrToInt(S));
                end;
              end;
              ctURL:
              begin
                {ItemCanvas.Font.Style := FURLSettings.FontStyle;
                ItemCanvas.Font.Color := FURLSettings.Color;
                if Sender.Focused and Sender.IsItemSelected(Item) then
                  ItemCanvas.Font.Color := clHighlightText; }

                TxtRect := ColRect;
                TxtRect.Left := TxtRect.Left+2;

                case FColumns.Items[i].alignment of
                  taLeftJustify:  format := DT_LEFT;
                  taRightJustify: format := DT_RIGHT;
                  else            format := DT_CENTER;
                end;

                ItemCanvas.Brush.Style := bsClear;
                DrawText(ItemCanvas.Handle, PChar(S), Length(S), TxtRect, DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER or format);
              end;
              ctProgress:
              begin
                DrawCompletion(ItemCanvas, ColRect, StrToInt(S), ProgressAppearance.Style = psClassic);
              end;
              ctCheckBox:
              begin

                case FColumns.Items[i].alignment of
                  taLeftJustify:
                  begin
                    DrawRect.Left := ColRect.Left;
                    DrawRect.Right := DrawRect.Left + CHECKBOX_SIZE;
                  end;
                  taRightJustify:
                  begin
                    DrawRect.Left := ColRect.Right - CHECKBOX_SIZE;
                    DrawRect.Right := DrawRect.Left + CHECKBOX_SIZE;
                  end;
                  else
                  begin
                    DrawRect.Left := ColRect.Left + (ColRect.Right - ColRect.Left - CHECKBOX_SIZE) div 2;
                    DrawRect.Right := DrawRect.Left + CHECKBOX_SIZE;
                  end;
                end;
                DrawRect.Top:= ColRect.Top + (ColRect.Bottom - ColRect.Top - CHECKBOX_SIZE) div 2;
                DrawRect.Bottom := DrawRect.Top + CHECKBOX_SIZE;

                DrawCheck(ItemCanvas, DrawRect, StrToInt(S) = 1, true, clBlack, FCheckBoxStyle);
              end;
            end;
          end;
        end;
      end;
    end;

  end;

end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.FreeItem(Sender: TOutlookGroupedList;
  Item: POGLItem);
var
  poi: POutlookInfo;
begin
  poi := FList.GetItemData(Item);
  poi.data.Free;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.GetCaption(Sender: TOutlookGroupedList;
  Item: POGLItem; var Caption: String);
var
  poi: POutlookInfo;
begin
  poi := FList.GetItemData(Item);

  if poi <> nil then
  begin
    if poi.data <> nil then
    begin
      if poi.data.Count > 0 then
        Caption := poi.data[0];

      if Assigned(FOnGetCaption) then
        FOnGetCaption(self, Item, Caption);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.GetItemHeight(Sender: TOutlookGroupedList;
  const OGLCanvas: TCanvas; var ItemHeight: Word);
begin
  ItemHeight := FItemHeight;

  if FPreviewSettings.Active then
  begin
    ItemHeight := ItemHeight + FPreviewSettings.Height;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.GetLastCol: Integer;
begin
  Result := MapHeaderSecToCol(FHeader.Sections.Count - 1);
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.HeaderChanged(Sender: TObject);
var
  i: Integer;
begin
  FHeader.Sections.Clear;

  while (Columns.Count > FHeader.Sections.Count) do
    FHeader.Sections.Add('');

  //while (Columns.Count > FHeader.Sections.Count) do
  //  FHeader.Sections.Delete(FHeader.Sections.Count - 1);

  if FPreviewSettings.Active then
  begin
    if (FPreviewSettings.Column >= 0) and (FPreviewSettings.Column < FColumns.Count) then
    begin
      FHeader.Sections.Delete(Columns[FPreviewSettings.Column].HeaderSecIndex);
    end;
  end;

  if (FGroupColumn >= 0) and (FGroupColumn < FColumns.Count) and (FGroupColumnDisplay = gdHidden) then
  begin
    FHeader.Sections.Delete(0{Columns[FGroupColumn].HeaderSecIndex});
  end;

  UpdateHeaderSecIndexes;

  for i := 1 to Columns.Count do
  begin
    if ((FGroupColumn >= 0) and (FGroupColumn < FColumns.Count) and (((i-1) = FGroupColumn)) and (FGroupColumnDisplay = gdHidden)) or
       (FPreviewSettings.Active and (i-1 = FPreviewSettings.Column) and (FPreviewSettings.Column >= 0) and (FPreviewSettings.Column < FColumns.Count))
       or not Columns[i - 1].Visible then
      Continue;

    FHeader.Sections[{i - 1}Columns[i - 1].HeaderSecIndex] := Columns[i - 1].Caption;
    FHeader.SectionWidth[{i - 1}Columns[i - 1].HeaderSecIndex] := Columns[i - 1].Width;
  end;

  if Assigned(FHeader) then
    FHeader.Invalidate;
  if Assigned(FList) then
    FList.Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.MapColToHeaderSec(ColIndex: Integer): Integer;
begin
  Result := Columns[ColIndex].HeaderSecIndex;

{  Result := ColIndex;
  if Result > FGroupColumn then
    Result := Result -1;

  if PreviewSettings.Active then
  begin
    if Result >= FPreviewSettings.Column then
      Result := Result - 1;
  end;
 }
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.MapHeaderSecToCol(SectionIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := SectionIndex;

  for i := 0 to FColumns.Count - 1 do
  begin
    if (not IsGroupColumn(i) or (FGroupColumnDisplay = gdVisible)) and not IsPreviewColumn(i) and (FColumns[i].HeaderSecIndex = SectionIndex) and (FColumns[i].Visible) then
    begin
      Result := i;
      Break;
    end;
  end;

{
  Result := SectionIndex;
  if Result >= FGroupColumn then
    Result := Result +1;

  if PreviewSettings.Active then
  begin
    if Result >= FPreviewSettings.Column then
      Result:= Result + 1;
  end;
}
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnHeaderSized(Sender: TObject; ColIdx,
  ColWidth: Integer);
begin
  if FColumns.Count > ColIdx then
  begin
    FColumns.Items[MapHeaderSecToCol(ColIdx)].FWidth := ColWidth;
    Invalidate;
    if Assigned(OnHeaderResized) then
      OnHeaderResized(self, MapHeaderSecToCol(ColIdx), ColWidth);
//      OnHeaderResized(self, ColIdx, ColWidth);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.ToggleCheck(Item: POGLItem; ColIndex: Integer);
var
  poi: POutlookInfo;
begin
  if (ColIndex < 0) or (ColIndex >= FColumns.Count) or ((ColIndex = FGroupColumn) and (GroupColumnDisplay = gdVisible)) then
    Exit;

  ColIndex := MapColumn(ColIndex);  
  poi := FList.GetItemData(Item);
  if poi.data.Count > ColIndex then
  begin
    if poi.data[ColIndex] = '1' then
      poi.data[ColIndex] := '0'
    else
      poi.data[ColIndex] := '1';
  end
  else // Count is less then ColIndex
  begin
    while Poi.data.Count <= ColIndex do
    begin
      Poi.data.Add('');
    end;
    poi.data[ColIndex] := '1';
  end;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.ColAtPoint(ItemRect: TRect; X, Y: Integer; var ColRect: TRect): TAdvOutlookColumn;
var
  i: Integer;
  R: TRect;
begin
  Result := nil;
  for i:= 0 to FColumns.Count-1 do
  begin
    R := GetCellRect(ItemRect, i);
    if PtInRect(R, Point(X, Y)) then
    begin
      ColRect := R;
      Result := FColumns.Items[i];
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.GetCellRect(ItemRect: TRect;
  ColIndex: Integer): TRect;
var
  i, x, w, HeaderCol: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (ColIndex < 0) or (ColIndex >= FColumns.Count) or ((ColIndex = FGroupColumn) and (FGroupColumnDisplay = gdHidden)) or (not Columns.Items[ColIndex].Visible) then
    Exit;

  if IsPreviewColumn(ColIndex) then
  begin
    Result := Rect(ItemRect.Left, ItemRect.Top, ItemRect.Right, ItemRect.Bottom);
    Result.Top := Result.Top + (Result.Bottom - Result.Top) - FPreviewSettings.Height;
    Exit;
  end;

  x := ItemRect.Left;

  for i := 0 to FHeader.Sections.Count - 1 do
  begin
    HeaderCol := MapHeaderSecToCol(i);

    w := FHeader.SectionWidth[i];

    Result := Rect(x, ItemRect.Top, x + w -1, ItemRect.Bottom);

    if (FPreviewSettings.Active) then
    begin
      if (FPreviewSettings.Column = HeaderCol) then
      begin
        Result.Top := Result.Top + (Result.Bottom - Result.Top) - FPreviewSettings.Height;
        Result.Left := ItemRect.Left;
        Result.Right := ItemRect.Right;
      end
      else
      begin
        Result.Bottom := Result.Top + (Result.Bottom - Result.Top) - FPreviewSettings.Height;
        x := x + w;
      end;
    end
    else
    begin
      x := x + w;
    end;

    if HeaderCol = ColIndex then
      break;
  end;

  {
  for i:= 0 to FColumns.Count-1 do
  begin
    if (I = FGroupColumn) then
      Continue;

    if FHeader.Sections.Count > MapColToHeaderSec(i) then
      w := FHeader.SectionWidth[MapColToHeaderSec(i)]
    else
      w := FColumns.Items[i].Width;

    Result := Rect(x, ItemRect.Top, x + w -1, ItemRect.Bottom);
    //x := x + w;

    if (FPreviewSettings.Active) then
    begin
      if (FPreviewSettings.Column = i) then
      begin
        Result.Top := Result.Top + (Result.Bottom - Result.Top) - FPreviewSettings.Height;
        Result.Left := ItemRect.Left;
        Result.Right := ItemRect.Right;
      end
      else
      begin
        Result.Bottom := Result.Top + (Result.Bottom - Result.Top) - FPreviewSettings.Height;
        x := x + w;
      end;
    end
    else
    begin
      x := x + w;
    end;

    if i = ColIndex then
      break;
  end;
  }
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.PtOnItemText(Item: POGLItem; ColIndex: Integer; ColRect: TRect;
  P: TPoint): Boolean;
var
  ACanvas: TCanvas;
  //format: Cardinal;
  Tw, Th, i, d: Integer;
  poi: POutlookInfo;
  TextRect: TRect;
  bmp: TBitMap;
begin
  Result := False;
  if (ColIndex < 0) or (ColIndex >= FColumns.Count) or not Assigned(Item) or not (FColumns.Items[ColIndex].ColumnType in [ctText, ctURL]) then
    Exit;

  poi := FList.GetItemData(Item);
  if poi.data.Count > MapColumn(ColIndex) then
  begin
    bmp := TBitMap.Create;
    try
      bmp.Width := ColRect.Right - ColRect.Left;
      bmp.Height := ColRect.Bottom - ColRect.Top;
      ACanvas := bmp.Canvas;
      ACanvas.Font.Assign(FColumns.Items[ColIndex].Font);
      ACanvas.Font.Style := FURLSettings.FontStyle;

      if (FPreviewSettings.Active) and (FPreviewSettings.Column = ColIndex) then
        ACanvas.Font.Assign(FPreviewSettings.Font);

     { case FColumns.Items[ColIndex].alignment of
        taLeftJustify:  format := DT_LEFT;
        taRightJustify: format := DT_RIGHT;
        else            format := DT_CENTER;
      end;
      }
      //Tw := DrawText(ACanvas.Handle, PChar(poi.data[ColIndex]), Length(poi.data[ColIndex]), ColRect, DT_VCENTER or format);
      Tw := ACanvas.TextWidth(poi.data[MapColumn(ColIndex)]);
      //Tw := 47;
      Th := ACanvas.TextHeight(poi.data[MapColumn(ColIndex)]);
      //Th := 13;
      i := ((ColRect.Bottom - ColRect.Top) - Th) div 2;
      case FColumns.Items[ColIndex].alignment of
        taLeftJustify:  TextRect := Rect(ColRect.Left, ColRect.Top + i, ColRect.Left + Tw, ColRect.Top + i + Th);
        taRightJustify: TextRect := Rect(ColRect.Right - Tw, ColRect.Top + i, ColRect.Right, ColRect.Top + i + Th);
        else
        begin
          d := ((ColRect.Right - ColRect.Left) - Tw) div 2;
          TextRect := Rect(ColRect.Left + d, ColRect.Top + i, ColRect.Left + d + Tw, ColRect.Top + i + Th);
        end;
      end;

      if PtInRect(TextRect, P) then
        Result := True;

    finally
      bmp.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnListMouseUpOnItem(Sender: TOutlookGroupedList;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  HitInfo: TOGLItemHitInfo);
var
  AColItem : TAdvOutlookColumn;
  CellRect, DrawRect: TRect;
  default: Boolean;
  poi: POutlookInfo;
  S, NValue: String;
  NewValue: Boolean;
  Anchor:string;
  hr: TRect;
begin
  if Button = mbRight then
  begin
    if (Assigned(FOnItemRightClick) or Assigned(FOnGroupRightClick)) and Assigned(HitInfo.HitItem) then
    begin
      AColItem := ColAtPoint(TProOutlookGroupedList(FList).GetRealRect(HitInfo.HitItem), X, Y, CellRect);

      if FList.IsGroupItem(HitInfo.HitItem) then
      begin
        if Assigned(FOnGroupRightClick) then
          FOnGroupRightClick(self, HitInfo.HitItem);
      end
      else
        if (AColItem <> nil) and (HitInfo.HitTest = htOnItem) then
          if Assigned(FOnItemRightClick) then
            FOnItemRightClick(self, HitInfo.HitItem, AColItem.Index);
    end;
  end
  else
  begin
    if (FList.ViewStyle = vsList) and Assigned(HitInfo.HitItem) {and (X > 3)} then
    begin
      AColItem := ColAtPoint(TProOutlookGroupedList(FList).GetRealRect(HitInfo.HitItem), X, Y, CellRect);

      if FList.IsGroupItem(HitInfo.HitItem) then
      begin
        if (X > 17) then
          if Assigned(FOnGroupClick) then
             FOnGroupClick(self, HitInfo.HitItem);
        Exit;
      end;

      if (AColItem <> nil) and (HitInfo.HitTest = htOnItem) then
      begin
        S := '';
        poi := FList.GetItemData(HitInfo.HitItem);
        if poi.data.Count > MapColumn(AColItem.Index) then
          S := poi.data[MapColumn(AColItem.Index)];

        case AColItem.ColumnType of
          ctCheckBox:
          begin

            case AColItem.alignment of
              taLeftJustify:
              begin
                DrawRect.Left := CellRect.Left;
                DrawRect.Right := DrawRect.Left + CHECKBOX_SIZE;
              end;
              taRightJustify:
              begin
                DrawRect.Left := CellRect.Right - CHECKBOX_SIZE;
                DrawRect.Right := DrawRect.Left + CHECKBOX_SIZE;
              end;
              else
              begin
                DrawRect.Left := CellRect.Left + (CellRect.Right - CellRect.Left - CHECKBOX_SIZE) div 2;
                DrawRect.Right := DrawRect.Left + CHECKBOX_SIZE;
              end;
            end;
            //DrawRect.Left := CellRect.Left + (CellRect.Right - CellRect.Left - CHECKBOX_SIZE) div 2;
            DrawRect.Top:= CellRect.Top + (CellRect.Bottom - CellRect.Top - CHECKBOX_SIZE) div 2;
            //DrawRect.Right := DrawRect.Left + CHECKBOX_SIZE;
            DrawRect.Bottom := DrawRect.Top + CHECKBOX_SIZE;
            if PtInRect(DrawRect, Point(X, Y)) then
            begin
              ToggleCheck(HitInfo.HitItem, AColItem.Index);

              NValue := '0';
              if poi.data.Count > MapColumn(AColItem.Index) then
                NValue := poi.data[MapColumn(AColItem.Index)];

              if NValue = '0' then
                NewValue := False
              else
                NewValue := True;
              if Assigned(FOnCheckBoxClick) then
                FOnCheckBoxClick(self, HitInfo.HitItem, AColItem.Index, NewValue);
            end;
          end;
          ctURL:
          begin
            if PtOnItemText(HitInfo.HitItem, AColItem.Index, CellRect, Point(X, Y)) then
            begin
              Default := True;
              if Assigned(FOnURLClick) then
                FOnURLClick(self, HitInfo.HitItem, AColItem.Index, S, Default);
              if Default then
              begin
                ShellExecute(0, 'open', PChar(S), nil, nil, SW_NORMAL);
              end;
            end;
          end;
          ctText:
          begin
            Anchor := IsAnchor(x, y, AColItem.Index, CellRect, S, hr);
            if (Anchor <> '') then
            begin
              if (Pos('://',Anchor) > 0) or (Pos('mailto:',Anchor) > 0) then
                shellexecute(0,'open',PChar(Anchor),nil,nil,SW_NORMAL)
              else
              begin
                if Assigned(FAnchorClick) then
                   FAnchorClick(self, HitInfo.HitItem, AColItem.Index, Anchor);
              end;
            end;
          end;
        end;

        if Assigned(FOnItemClick) then
          FOnItemClick(self, HitInfo.HitItem, AColItem.Index);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnListMouseDownOnItem(Sender: TOutlookGroupedList;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  HitInfo: TOGLItemHitInfo);
begin
  FLookUpText := '';
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnProgressAppearanceChange(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnURLSettingsChange(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetBorderStyle(const Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetCheckBoxStyle(const Value: TCheckBoxStyle);
begin
  if FCheckBoxStyle <> Value then
  begin
    FCheckBoxStyle := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetColumnLineColor(const Value: TColor);
begin
  if FColumnLineColor <> Value then
  begin
    FColumnLineColor := Value;
    if Assigned(FList) then
      FList.Invalidate;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetImages(const Value: TImageList);
begin
  if Value <> FImages then
  begin
    FImages := Value;
    if Assigned(FList) then
      FList.Images := FImages;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetProgressAppearance(const Value: TProgressAppearance);
begin
  FProgressAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetSelectionColor(const Value: TColor);
begin
  FSelectionColor := Value;
  FList.SelectionColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetSelectionTextColor(const Value: TColor);
begin
  FSelectionTextColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetURLSettings(const Value: TURLSettings);
begin
  FURLSettings.Assign(Value);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetPreviewSettings(const Value: TPreviewSetting);
begin
  FPreviewSettings.Assign(Value);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnPreviewSettingChange(Sender: TObject);
var
  i, Hs: Integer;
begin
  if FPreviewSettings.Active then
  begin
    if (FPreviewSettings.Column < 0) or (FPreviewSettings.Column >= FColumns.Count) then
    begin
      FPreviewSettings.Active := False;
      raise exception.Create('Invalid Preview Column');
    end;

    if (FPreviewedColumn > -1) then
    begin
      Hs := FColumns[FPreviewedColumn].HeaderSecIndex;
      for i:= 0 to FColumns.Count-1 do
      begin
        if (FColumns[i].HeaderSecIndex >= Hs) and (i <> FPreviewedColumn) and (not IsGroupColumn(i) or (FGroupColumnDisplay = gdVisible)) then
        begin
          FColumns[i].HeaderSecIndex := FColumns[i].HeaderSecIndex + 1;
          FColumns[i].HeaderSecOrgIndex := FColumns[i].HeaderSecOrgIndex + 1;
        end;
      end;
      FPreviewedColumn := -1;
    end;

    for i:= 0 to FColumns.Count-1 do
    begin
      if (FColumns[i].HeaderSecIndex > FColumns[FPreviewSettings.Column].HeaderSecIndex) and (not IsGroupColumn(i) or (FGroupColumnDisplay = gdVisible)) then
      begin
        FColumns[i].HeaderSecIndex := FColumns[i].HeaderSecIndex - 1;
        FColumns[i].HeaderSecOrgIndex := FColumns[i].HeaderSecOrgIndex - 1;
      end;
    end;
    FPreviewedColumn := FPreviewSettings.Column;

    TProOutlookGroupedList(FList).AdjustItemsSize;
    HeaderChanged(FHeader);
  end
  else
  begin
    if FPreviewedColumn > -1 then
    begin
      Hs := FColumns[FPreviewedColumn].HeaderSecIndex;
      for i:= 0 to FColumns.Count-1 do
      begin
        if (FColumns[i].HeaderSecIndex >= Hs) and (i <> FPreviewedColumn) and (not IsGroupColumn(i) or (FGroupColumnDisplay = gdVisible)) then
        begin
          FColumns[i].HeaderSecIndex := FColumns[i].HeaderSecIndex + 1;
          FColumns[i].HeaderSecOrgIndex := FColumns[i].HeaderSecOrgIndex + 1;
        end;
      end;
      FPreviewedColumn := -1;
    end;
    TProOutlookGroupedList(FList).AdjustItemsSize;
    HeaderChanged(FHeader);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetHeaderBorderColor(const Value: TColor);
begin
  if FHeaderBorderColor <> Value then
  begin
    FHeaderBorderColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnColumnRefresh(Sender: TObject);
begin
  if Assigned(FHeader) then
    FHeader.Invalidate;
  if Assigned(FList) then
    FList.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetHeaderHeight(const Value: Integer);
begin
  FHeaderHeight := Value;
  if Assigned(FHeader) then
  begin
    FHeader.Height := Value;
  end;
end;


//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetItemHeight(const Value: Integer);
begin
  FItemHeight := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.GetHeaderHeight: Integer;
begin
  if Assigned(FHeader) then
    Result := FHeader.Height
  else
    Result := FHeaderHeight;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnListMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  HitInfo: TOGLItemHitInfo;
  AColItem : TAdvOutlookColumn;
  CellRect: TRect;
  poi: POutlookInfo;
  //OnURL: Boolean;
  S: String;
  Anchor:string;
  hr: TRect;
  pt: TPoint;
  ChangedCursor: Boolean;
begin
  if (csDesigning in ComponentState) then
    Exit;

  if Assigned(FOnMouseMove) then
    FOnMouseMove(Sender, Shift, X, Y);

  AColItem := nil;

  FList.GetHitTestInfoAt(X, Y, True, HitInfo);
  //OnURL := False;
  ChangedCursor := False;

  if (FList.ViewStyle = vsList) and Assigned(HitInfo.HitItem) then
  begin
    AColItem := ColAtPoint(TProOutlookGroupedList(FList).GetRealRect(HitInfo.HitItem), X, Y, CellRect);
    if (AColItem <> nil) and (HitInfo.HitTest = htOnItem) then
    begin
      S := '';
      poi := FList.GetItemData(HitInfo.HitItem);
      if Assigned(poi) and (poi.data.Count > MapColumn(AColItem.Index)) then
        S := poi.data[MapColumn(AColItem.Index)];

      if (FMouseOverCol <> AColItem.Index) then
      begin
        FMouseOverCol := AColItem.Index;
        Application.CancelHint;
        GetCursorPos(Pt);

        if (AColItem.ShowHint) and (AColItem.Hint <> '') then
        begin
          //FList.Hint := AColItem.Hint;
          Application.ActivateHint(Pt);
        end;
      end;

      if S <> '' then
      begin
        case AColItem.ColumnType of
          ctURL:
          begin
            if PtOnItemText(HitInfo.HitItem, AColItem.Index, CellRect, Point(X, Y)) then
            begin
              //OnURL := True;
              ChangedCursor := True;
              if Screen.Cursor <> crHandPoint then
                Screen.Cursor := crHandPoint;
            end;
          end;
          ctText:
          begin

            Anchor := IsAnchor(x, y, AColItem.Index, CellRect, S, hr);

            if Anchor <> '' then
            begin
              if (FAnchor <> Anchor) or not Equalrect(FCurrHoverRect,hr) or (FHoverHyperlink = -1) then
              begin
                if FHover then
                begin
                  //if hr.Left <> -1 then
                    //HoverInvalidate(FCurrHoverRect)
                end;
              end;

              if (Screen.Cursor = crHandPoint) then
                ChangedCursor := True;
              if (Screen.Cursor = crDefault) or (FAnchor <> Anchor) or (FOldHoverHyperLink <> FHoverHyperLink) then
              begin
               { if FAnchorHint then
                  Application.CancelHint;}
                Screen.Cursor := crHandPoint;
                ChangedCursor := True;

                if Assigned(FAnchorEnter) then
                  FAnchorEnter(self, HitInfo.HitItem, AColItem.Index, anchor);

                if FHover then
                begin
                  {if hr.Left <> -1 then
                    HoverInvalidate(FCurrHoverRect)
                  else }
                    Invalidate;
                end;
              end;

               FAnchor := Anchor;
               FOldHoverHyperLink := FHoverHyperLink;
               FCurrHoverRect := hr;

               //if FHover then
                 //HoverInvalidate(FCurrHoverRect)
            end
            else
            begin
              if Screen.Cursor = crHandPoint then
              begin
                Screen.Cursor := crDefault;
                if Assigned(FAnchorExit) then
                  FAnchorExit(self, HitInfo.HitItem, AColItem.Index, anchor);

                if FHover then
                begin
                 { if FCurrHoverRect.Left <> -1 then
                    HoverInvalidate(FCurrHoverRect)
                  else }
                    Invalidate;
                end;

                FCurrHoverRect := hr;
                //if FHover then
                  //HoverInvalidate(FCurrHoverRect)

              end;
            end;


          end;
        end;
      end;
    end;
  end;

  if (FHoverHyperLink <> -1) and (not Assigned(HitInfo.HitItem) or (AColItem = nil) or (Assigned(HitInfo.HitItem) and not (Assigned(HitInfo.HitItem)))) then
  begin
    {if FHover and (FHoverHyperLink <> -1) then
      HoverInvalidate(FCurrHoverRect); }
    FHoverHyperLink := -1;
  end;

{  if not OnURL and (Screen.Cursor <> Cursor) then
    Screen.Cursor := crDefault;
}
  if not ChangedCursor then
  begin
    if Screen.Cursor = crHandPoint then
      Screen.Cursor := crDefault;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.Paint;
var
  //R: TRect;
  ARect: TRect;
  BtnFaceBrush: HBRUSH;
  DC: HDC;
begin
  if (BorderStyle = bsSingle) and (BorderColor <> clNone) then
  begin
    DC := GetWindowDC(Handle);
    BtnFaceBrush := CreateSolidBrush(ColorToRGB(BorderColor));
    try
      GetWindowRect(Handle, ARect);
      OffsetRect(ARect, -ARect.Left, -ARect.Top);
      FrameRect(DC, ARect, BtnFaceBrush);
      //Canvas.Pen.Color := clLime;
      //Canvas.Rectangle(ARect);
    finally
      DeleteObject(BtnFaceBrush);
      ReleaseDC(Handle,DC);
    end;
  end;
 { inherited;
  R := ClientRect;
  R := Rect(R.Left-2, R.Top-2, R.Right+2, R.Bottom+2);
  if (BorderStyle = bsSingle) and (BorderColor <> clNone) then
  begin
    Canvas.Pen.Color := BorderColor;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(R);
  end;  }
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnListItemDblClick(Sender: TOutlookGroupedList;
  Item: POGLItem; X, Y: Integer);
var
  AColItem : TAdvOutlookColumn;
  CellRect: TRect;
  i: Integer;
begin
  if (FList.ViewStyle = vsList) and Assigned(Item) then
  begin
    if FList.IsGroupItem(Item) then
    begin
      ToggleExpandedItem(Item);
      if Assigned(FOnGroupDblClick) then
        FOnGroupDblClick(self, Item);
    end
    else
    begin
      if Assigned(FOnItemDblClick) then
      begin
        AColItem := ColAtPoint(TProOutlookGroupedList(FList).GetRealRect(Item), X, Y, CellRect);
        i := -1;
        if Assigned(AColItem) then
          i := AColItem.Index;
        FOnItemDblClick(self, Item, i);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnHeaderSectClick(Sender: TObject;
  SectionIndex: Integer);
begin
  if Assigned(FOnHeaderClick) then
    FOnHeaderClick(self, MapHeaderSecToCol(SectionIndex));
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnHeaderSectDblClick(Sender: TObject;
  SectionIndex: Integer);
begin
  if Assigned(FOnHeaderDblClick) then
    FOnHeaderDblClick(self, MapHeaderSecToCol(SectionIndex));
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnHeaderSectRightClick(Sender: TObject;
  SectionIndex: Integer);
begin
  if Assigned(FOnHeaderRightClick) then
    FOnHeaderRightClick(self, MapHeaderSecToCol(SectionIndex));
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.Locate(s: String; Col: Integer);
var
  GroupItem, ChildItem: POGLItem;
  poi, Poi1: POutlookInfo;
  S2: String;
  Found: Boolean;
  MCol: Integer;
begin
  MCol := MapColumn(Col);
  if (MCol >= 0) and (MCol < FColumns.Count) then
  begin
    Found := False;
    GroupItem := FList.FirstGroupItem;
    while (GroupItem <> nil) and not Found do
    begin
      poi1 := FList.GetItemData(GroupItem);
      ChildItem := GroupItem.FirstChild;
      while (ChildItem <> nil) do
      begin
        poi := FList.GetItemData(ChildItem);

        S2 := '';
        if MCol < poi.data.Count then
        begin
          if ((Col = FGroupColumn) and (FGroupColumnDisplay = gdVisible)) then
          begin
            if poi1.data.Count >= 1 then
              S2 := poi1.data[0];
          end
          else
            S2 := poi.data[MCol];
        end;

        if pos('</',S2) > 0 then
          S2 := HTMLStrip(S2);

        // Compare
        S2 := Copy(S2, 1, min(Length(s2), Length(S)));

        if StrIComp(PChar(S), PChar(S2)) = 0 then
        begin
          FList.ClearSelection;
          FList.FocusedItem := ChildItem;
          FList.AddToSelection(ChildItem);
          FList.ScrollIntoView(ChildItem, False);
          FList.Invalidate;
          Found := True;
          Break;
        end;

        ChildItem := ChildItem.NextSibling;
      end;
      GroupItem := GroupItem.NextSibling;
    end;

  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  LuCol: Integer;
begin
  if Assigned(OnKeyDown) then
    OnKeyDown(self, Key, Shift);

  if Key in [VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN, VK_ESCAPE, VK_RETURN] then
  begin
    FLookUpText := '';
  end
  else
  begin
    LuCol := -1;
    if FSortSettings.Enabled then
      LuCol := FSortSettings.Column;
    if (FLookUp.Column >= 0) then
      LuCol := FLookUp.Column;
    if (LuCol >= 0) and (LuCol < FColumns.Count) then
    begin
      if (FLookUp.Method = lmDirect) then
      begin
        Locate(Char(Key), LuCol);
      end
      else // FLookUp.Method = lmIncremental
      begin
        FLookUpText := FLookUpText + Char(Key);
        Locate(FLookUpText, LuCol);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnListKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnKeyUp) then
    OnKeyUp(self, Key, Shift);
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnListKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(OnKeyPress) then
    OnKeyPress(self, Key);
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetSortSettings(const Value: TSortSettings);
begin
  FSortSettings.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnSortChange(Sender: TObject);
begin
  SortItems;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.DoExpand(Sender: TOutlookGroupedList; Item: POGLItem);
begin
  if Assigned(OnGroupExpand) then
    OnGroupExpand(Self, Item);
end;
//------------------------------------------------------------------------------

procedure TAdvOutlookList.DoCollaps(Sender: TOutlookGroupedList; Item: POGLItem);
begin
  if Assigned(OnGroupCollaps) then
    OnGroupCollaps(Self, Item);
end;


//------------------------------------------------------------------------------

procedure TAdvOutlookList.SortItems;
{var
  GroupItem,ChildItem: POGLItem; }
begin
  if SortSettings.Column >= Columns.Count then
    Exit;

  if (SortSettings.Column >= 0) and (Columns.Items[SortSettings.Column].SortType <> stNone) then
  begin
     FSorting := True;
     if SortSettings.Direction = sdAscending then
       FList.Sort(SortSettings.Column, OutlookGroupedList.sdAscending)
     else
       FList.Sort(SortSettings.Column, OutlookGroupedList.sdDescending);
     FSorting := False;
     FHeader.Invalidate;

     if Assigned(FOnSorted) then
       FOnSorted(Self, SortSettings.Column);

     {

    try
      // cycle through the Group items
      GroupItem := FList.FirstGroupItem;
      while GroupItem <> nil do
      begin
        // check the Item.ChildCount - i.e. real Thumbnails
        if (GroupItem.ChildCount > 0) then
        begin
          ChildItem := GroupItem.FirstChild;
          while ChildItem <> nil do
          begin
            ChildItem := ChildItem.NextSibling;
            case Columns.Items[SortSettings.Column].SortType of
              stTextNoCase:
              begin

              end;
              stTextCase:
              begin

              end;
              stNumeric:
              begin

              end;
              stBoolean:
              begin

              end;
              stDate:
              begin

              end;
              stTime:
              begin

              end;
              stDateTime:
              begin

              end;
            end;
          end;
        end else
        GroupItem := GroupItem.NextSibling;
      end;
    finally

    end;
    }
  end
  else
  begin

  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnListCompareItems(Sender: TOutlookGroupedList;
  Item1, Item2: POGLItem; Column: TOGLSortColumn; var Result: Integer);
var
  V1, V2: String;
  poi1, poi2: POutlookInfo;
  ASortType: TSortType;
  col: Integer;
begin
  if not FSorting then
    Exit;

  if FList.IsGroupItem(Item1) or FList.IsGroupItem(Item2) then
  begin
    //Exit;
    if not FSortSettings.SortGroups then
      Exit;
      
    poi1 := FList.GetItemData(Item1);
    V1 := '';
    if poi1.data.Count > 0 then
      V1 := poi1.data[0];

    poi2 := FList.GetItemData(Item2);
    V2 := '';
    if poi2.data.Count > 0 then
      V2 := poi2.data[0];

    ASortType := Columns.Items[FGroupColumn].SortType;
    col := FGroupColumn;
  end
  else
  begin
    poi1 := FList.GetItemData(Item1);
    V1 := '';
    if poi1.data.Count > MapColumn(Column) then
      V1 := poi1.data[MapColumn(Column)];

    poi2 := FList.GetItemData(Item2);
    V2 := '';
    if poi2.data.Count > MapColumn(Column) then
      V2 := poi2.data[MapColumn(Column)];

    ASortType := Columns.Items[SortSettings.Column].SortType;
    col := SortSettings.Column;
  end;

  case ASortType of
    stTextNoCase:
    begin
      Result := StrIComp(PChar(V1), PChar(V2));
    end;
    stTextCase:
    begin
      Result := StrComp(PChar(V1), PChar(V2));
    end;
    stAnsiText:
    begin
      Result := AnsiCompareStr(V1,V2);
      if Result > 0 then
        Result := 1
      else
        if Result < 0 then
          Result := -1;
    end;
    stAnsiTextNoCase:
    begin
      Result := AnsiCompareText(V1,V2);
      if Result > 0 then
        Result := 1
      else
        if Result < 0 then
          Result := -1;
    end;
    stNumeric:
    begin
      try
        if StrToInt(V1) < StrToInt(V2) then
          Result := -1
        else if StrToInt(V1) > StrToInt(V2) then
          Result := 1
        else
          Result := 0;
      except
        Result := 0;
      end;
    end;
    stBoolean:
    begin
      try
        if StrToBool(V1) < StrToBool(V2) then
          Result := -1
        else if StrToBool(V1) > StrToBool(V2) then
          Result := 1
        else
          Result := 0;
      except
        Result := 0;
      end;
    end;
    stDate:
    begin
      try
        if StrToDate(V1) < StrToDate(V2) then
          Result := -1
        else if StrToDate(V1) > StrToDate(V2) then
          Result := 1
        else
          Result := 0;
      except
        Result := 0;
      end;
    end;
    stTime:
    begin
      try
        if StrToTime(V1) < StrToTime(V2) then
          Result := -1
        else if StrToTime(V1) > StrToTime(V2) then
          Result := 1
        else
          Result := 0;
      except
        Result := 0;
      end;
    end;
    stDateTime:
    begin
      try
        if StrToDateTime(V1) < StrToDateTime(V2) then
          Result := -1
        else if StrToDateTime(V1) > StrToDateTime(V2) then
          Result := 1
        else
          Result := 0;
      except
        Result := 0; 
      end;
    end;
    stCustom:
    begin
      if Assigned(FOnCustomCompare) then
        FOnCustomCompare(self, Item1, Item2, col, V1, V2, Result);
    end;
    stFloat:
    begin
      try
        if StrToFloat(V1) < StrToFloat(V2) then
          Result := -1
        else if StrToFloat(V1) > StrToFloat(V2) then
          Result := 1
        else
          Result := 0;
      except
        Result := 0
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.HTMLPaint(Canvas: TCanvas; s: string; fr: TRect;
  FImages: TImageList; xpos, ypos, focuslink, hoverlink,
  shadowoffset: Integer; checkhotspot, checkheight, print, selected, blink,
  hoverstyle: boolean; resfactor: double; urlcolor, hovercolor,
  hoverfontColor, shadowcolor: TColor; var anchorval, stripval,
  focusanchor: string; var xsize, ysize, hyperlinks, mouselink: Integer;
  var hoverrect: TRect): boolean;
begin
  Result := HTMLDrawEx(Canvas,s,fr,FImages,xpos,ypos,-1,HoverLink,ShadowOffset,checkhotspot,checkheight,print,selected,Blink,
                       Hoverstyle,not FEllipsis,Resfactor,urlcolor,hovercolor,hoverfontColor,shadowcolor,anchorval,stripval,focusanchor,
                       XSize,YSize,HyperLinks,MouseLink,HoverRect,FImageCache,FContainer,0);

  //FXSize := XSize;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;

  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetEllipsis(const Value: Boolean);
begin
  if FEllipsis <> Value then
  begin
    FEllipsis := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetShadowOffset(const Value: Integer);
begin
  FShadowOffset := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetHover(const Value: boolean);
begin
  FHover := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetHoverColor(const Value: TColor);
begin
  FHoverColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetHoverFontColor(const Value: TColor);
begin
  FHoverFontColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetSelectionOptions(
  const Value: TOGLSelectionOptions);
begin
  FSelectionOptions := value;
  if Assigned(FList) then
    FList.SelectionOptions := value;
end;

procedure TAdvOutlookList.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.IsAnchor(x, y: Integer; Col: Integer; CellRect: TRect; CellValue: String; var HoverRect: TRect): string;
var
  xsize,ysize: Integer;
  Anchor,Stripped,Focusanchor: string;
  hl: Integer;

begin
  //r := ClientRect;
 { if FBorderStyle = bsSingle then
  begin
    InflateRect(r,-BorderWidth,-BorderWidth);
  end;
 }
  //s := GetDisplText;

  Anchor := '';
  HoverRect := Rect(-1,-1,-1,-1);

 { if FVAlignment in [tvaCenter,tvaBottom] then
  begin
    HTMLPaint(Canvas,s,r,FImages,x,y,-1,-1,FShadowOffset,True,False,False,False,False,FHover,1.0,
      fURLcolor,FHoverColor,FHoverFontColor,FShadowColor,anchor,stripped,focusanchor,XSize,YSize,hl,FHOverHyperLink,HoverRect);
    if y < Height then
    case FVAlignment of
    tvaCenter:r.Top := r.Top+((r.Bottom - r.Top - y) shr 1);
    tvaBottom:r.Top := r.Bottom - y;
    end;
  end;
 }
  if (FPreviewSettings.Active) and (FPreviewSettings.Column = Col) then
    Canvas.Font.Assign(FPreviewSettings.Font);

  if HTMLPaint(Canvas,CellValue,CellRect,FImages,x,y,-1,-1,FShadowOffset,True,False,False,False,False,FHover,1.0,
     clWhite,clNone,clNone,clNone,Anchor,Stripped,FocusAnchor,XSize,YSize,hl,FHoverHyperlink,HoverRect) then
    Result := Anchor
  else
    FHoverHyperLink := -1;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetGroupColumn(const Value: Integer);
var
  i, j: Integer;
begin
  if (FGroupColumn <> Value) and (Value >= 0) and (Value < FColumns.Count) then
  begin
    //--- Setting HeaderSecIndex
    //FColumns[FGroupColumn].HeaderSecIndex := FColumns[Value].HeaderSecIndex;
    //FColumns[Value].HeaderSecIndex := FColumns[FGroupColumn].HeaderSecIndex;

    if (FGroupColumnDisplay = gdHidden) then
    begin
      j := 0;
      for i := 0 to FColumns.Count-1 do
      begin
        if ((Value >= 0) and (Value < FColumns.Count) and (i = Value)) or
           (FPreviewSettings.Active and (i = FPreviewSettings.Column) and (FPreviewSettings.Column >= 0) and (FPreviewSettings.Column < FColumns.Count)) then
          Continue;
        FColumns.Items[i].HeaderSecIndex := j;
        FColumns.Items[i].HeaderSecOrgIndex := j;
        Inc(j);
      end;
    end;
    //---
    
    FGroupColumn := Value;
    if PreviewSettings.Active then
      PreviewSettings.Active := False;

    if SortSettings.Column >= 0 then
      SortSettings.Column := -1;
    SetGrouping;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetGrouping;
var
  DummyList: TOutlookGroupedList;
  GroupItem, ChildItem, DOGLChildItem: POGLItem;
  DOLGItem: TOutlookGroup;
  GroupSL: TStringList;
  i: Integer;
  poi, poi1, poi2, Poi3: POutlookInfo;
  ioo: TObject;
  iot: integer;
  S: string;

begin
  if (FGroupColumn = FCurrentGroupColumn) then
    Exit;

  DummyList := TOutlookGroupedList.Create(Self);
  DummyList.Parent := Self;
  DummyList.Visible := false;
  DummyList.ItemDataSize  := sizeof(TOutlookInfo);

  TProOutlookGroupedList(DummyList).DoNotClearItems := True;

  GroupSL := TStringList.Create;
  FList.BeginUpdate;
  GroupItem := FList.FirstGroupItem;
  while GroupItem <> nil do
  begin
    poi1 := FList.GetItemData(GroupItem);
    ChildItem := GroupItem.FirstChild;
    while (ChildItem <> nil) do
    begin
      poi := FList.GetItemData(ChildItem);
      ioo := ChildItem.ItemObject;
      iot := ChildItem.Tag;

      S := '';
      if (poi1.data.Count >= 1) then
        S := poi1.data[0];

      // Insert FCurrentGroupColumn   
      if FCurrentGroupColumn < poi.data.Count then
        poi.data.Insert(FCurrentGroupColumn, S)
      else
        poi.data.Add(S);

      S := '';
      if FGroupColumn < poi.data.Count then
        S := poi.data[FGroupColumn];

      I := GroupSL.IndexOf(S);
      if (I < 0) then
      begin
        DOLGItem := TOutlookGroup.Create;
        DOLGItem.OGLItem := DummyList.AddItem(nil);
        poi2 := DummyList.GetItemData(DOLGItem.OGLItem);
        poi2.data := TStringList.Create;
        poi2.data.Add(S);

        FGroupList.Add(DOLGItem);

        DOLGItem.OGLItem.ItemObject := ioo;
        DOLGItem.OGLItem.Tag := iot;

        GroupSL.AddObject(S, DOLGItem);
      end
      else
      begin
        DOLGItem := TOutlookGroup(GroupSL.Objects[I]);
        DOLGItem.OGLItem.ItemObject := ioo;
        DOLGItem.OGLItem.Tag := iot;
      end;

      DOGLChildItem := DummyList.AddItem(DOLGItem.OGLItem);
      poi3 := DummyList.GetItemData(DOGLChildItem);
      poi3.data := TStringList.Create;
      poi3.data.Assign(poi.data);

      DOGLChildItem.ItemObject := ioo;
      DOGLChildItem.Tag := iot;


      // delete Group Column
      if FGroupColumn < poi3.data.Count then
        poi3.data.Delete(FGroupColumn);

      ChildItem := ChildItem.NextSibling;
    end;
    GroupItem := GroupItem.NextSibling;
  end;

  // Clean and ReAdd FList Items
  FList.FocusedItem := nil;
  FList.ClearSelection;
  FList.Clear;
  FreeMem(FList.RootItem);
  TProOutlookGroupedList(FList).SetInternalRootItem(DummyList.RootItem);
  TProOutlookGroupedList(FList).AdjustItemsSize;

  FCurrentGroupColumn := FGroupColumn;
  FList.EndUpdate;
  HeaderChanged(nil);
  GroupSL.Free;
  DummyList.Free;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.MapColumn(Column: Integer): Integer;
begin
  Result := Column;
  if (Column > FGroupColumn){ and (FGroupColumnDisplay = gdHidden)} then
    Result := Column - 1;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.ListMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TAdvOutlookList.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TAdvOutlookList.Loaded;
begin
  inherited;
  {if FColumns.Count = 0 then
  begin
    FColumns.Add;
    FColumns.Add;
    FColumns.Add;
  end; }
  HeaderChanged(nil);
  
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

function TAdvOutlookList.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.BeginUpdate;
begin
  FList.BeginUpdate;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.EndUpdate;
begin
  FList.EndUpdate;
end;

procedure TAdvOutlookList.TestFill;
var
  i,j: integer;
begin
  Columns.Clear;
  Columns.Add.Caption := 'Group';
  Columns.Add.Caption := 'Col 1';
  Columns.Add.Caption := 'Col 2';
  Columns.Add.Caption := 'Col 3';

  for i := 1 to 5 do
    with AddGroup('Group '+inttostr(i)) do
    begin
      for j := 1 to 1 + random(3) do
        with AddChild do
        begin
          Add('cell [0,' +inttostr(j)+']');
          Add('cell [1,' +inttostr(j)+']');
          Add('cell [2,' +inttostr(j)+']');
        end;
    end;


end;

procedure TAdvOutlookList.SetFocus;
begin
  FList.SetFocus;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetColor(const Value: TColor);
begin
  FColor := Value;
  if Assigned(FList) then
    TProOutlookGroupedList(FList).Color := FColor;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetHeaderDragDrop(const Value: TDragDropSetting);
begin
  FHeaderDragDrop := Value;
  FHeader.SectionDragDrop := FHeaderDragDrop = ddEnabled;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetHeaderResize(const Value: boolean);
begin
  if (FHeaderResize <> Value) then
  begin
    FHeaderResize := Value;
    FHeader.AllowResize := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetDragDropSetting(
  const Value: TDragDropSetting);
begin
  if FDragDropSetting <> Value then
  begin
    FDragDropSetting := Value;
    if Assigned(FList) then
    begin
      if FDragDropSetting = ddDisabled then
        FList.DragOptions := []
      else
        FList.DragOptions := DefaultDragOptions;
    end;

   { if FDragDropSetting = ddDisabled then
      FList.DragMode := dmAutomatic
    else
      FList.DragMode := dmManual; }
  end;
end;

//------------------------------------------------------------------------------
{
procedure TAdvOutlookList.CMHintShow(var Message: TMessage);
var
  PHI: PHintInfo;
begin
  PHI := TCMHintShow(Message).HintInfo;
  if (FMouseOverCol >= 0) then
  begin
    if (Columns.Items[FMouseOverCol].ShowHint) and (Columns.Items[FMouseOverCol].Hint <> '') then
      PHI^.HintStr := Columns.Items[FMouseOverCol].Hint
    else
      PHI^.HintStr := ''
  end;
end;
}
//------------------------------------------------------------------------------

procedure TAdvOutlookList.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseOverCol := -1;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.CMTabStopChanged(var Message: TMessage);
begin
  inherited;
  FList.TabStop := TabStop;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnListOLEDragAllowed(
  Sender: TOutlookGroupedList; Item: POGLItem; var Allowed: Boolean);
begin
  Allowed := FDragDropSetting = ddEnabled;
  if Allowed then
  begin
    if Assigned(FOnOLEDragAllowed) then
      FOnOLEDragAllowed(self, Item, Allowed);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnListOLEDragOver(
  Sender: TOutlookGroupedList; const DataObject: IDataObject;
  Shift: TShiftState; Pt: TPoint; State: TDragState; var Effect: Integer;
  var Accept: Boolean);
var
  tmpFormatEtc: TFormatEtc;
begin
  if (FDragDropSetting = ddDisabled) then
  begin
    Accept := False;
    Exit;
  end;

  if Assigned(FOnOLEDragOver) then
    FOnOLEDragOver(self, DataObject, Shift, Pt, State, Effect, Accept)
  else
  begin
    if State = dsDragEnter then
    begin
      if Assigned(DataObject) then
      begin
        // check the supported clipboard formats
        // CF_HDROP
        tmpFormatEtc.cfFormat := CF_HDROP;
        tmpFormatEtc.ptd := nil;
        tmpFormatEtc.dwAspect := DVASPECT_CONTENT;
        tmpFormatEtc.lindex := -1;
        tmpFormatEtc.tymed := TYMED_HGLOBAL;
        Accept := Succeeded(DataObject.QueryGetData(tmpFormatEtc));
        if Accept then Exit;
        // CF_FILEGROUPDESCRIPTOR
        tmpFormatEtc.cfFormat := CF_FILEGROUPDESCRIPTOR;
        tmpFormatEtc.ptd := nil;
        tmpFormatEtc.dwAspect := DVASPECT_CONTENT;
        tmpFormatEtc.lindex := -1;
        tmpFormatEtc.tymed := TYMED_HGLOBAL;
        Accept := Succeeded(DataObject.QueryGetData(tmpFormatEtc));
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnListOLEDrop(
  Sender: TOutlookGroupedList; const DataObject: IDataObject;
  Shift: TShiftState; Pt: TPoint; Formats: TClipFormatArray;
  var Effect: Integer);

var
  i,FilesCount: Integer;
  tmpFormatEtc: TFormatEtc;
  tmpStgMedium: TStgMedium;
  tmpStgMedium1: TStgMedium;
  szFileName: array [0..MAX_PATH] of Char;
  ChildItem: POGLItem;
  Sd: TStrings;
  lpGroupDescriptor: PFileGroupDescriptor;
  OleStream: TOleStream;
  SL: TStringList;
  j,h: Integer;
  s, subs: String;
begin
  if Assigned(FOnOLEDrop) then
    FOnOLEDrop(self, DataObject, Shift, Pt, Formats, Effect)
  else
  begin
    // try to get CF_HDROP
    tmpFormatEtc.cfFormat := CF_HDROP;
    tmpFormatEtc.ptd := nil;
    tmpFormatEtc.dwAspect := DVASPECT_CONTENT;
    tmpFormatEtc.lindex := -1;
    tmpFormatEtc.tymed := TYMED_HGLOBAL;
    if Succeeded(DataObject.GetData(tmpFormatEtc, tmpStgMedium)) then
    begin
      if {Sender.}DropTargetGroup = nil then Exit;
      FilesCount := DragQueryFile(tmpStgMedium.hGlobal, $FFFFFFFF, nil, 0);
      {Sender.}BeginUpdate;
      try
        ChildItem := nil;
        for i := 0 to FilesCount-1 do
        begin
          DragQueryFile(tmpStgMedium.hGlobal, i, szFileName, SizeOf(szFileName));
          ChildItem := {Sender.}CaptionToItem(szFileName, {Sender.}DropTargetGroup);
          if ChildItem = nil then
          begin
            ChildItem := {Sender.}AddItem(Sender.DropTargetGroup);
            Sd := {Sender.}GetItemData(ChildItem);
           { sd.Add('0');
            Sd.Add(ExtractFileName(szFileName)); }
            Sd.CommaText := {ExtractFileName(}szFileName{)};
            CopyFile(szFileName, PChar(ExtractFilePath(Application.ExeName) +
              ExtractFileName(szFileName)), False);
          end;
        end;
        if ChildItem <> nil then
        begin
          {Sender.}FocusedItem := ChildItem;
          {Sender.}ScrollIntoView({Sender.}FocusedItem)
        end;
      finally
        {Sender.}EndUpdate;
      end;
      Exit;
    end;
    // try to get CF_FILEGROUPDESCRIPTOR
    tmpFormatEtc.cfFormat := CF_FILEGROUPDESCRIPTOR;
    tmpFormatEtc.ptd := nil;
    tmpFormatEtc.dwAspect := DVASPECT_CONTENT;
    tmpFormatEtc.lindex := -1;
    tmpFormatEtc.tymed := TYMED_HGLOBAL;
    if Succeeded(DataObject.GetData(tmpFormatEtc, tmpStgMedium)) then
    begin
      if DropTargetGroup = nil then
        Exit;
      lpGroupDescriptor := GlobalLock(tmpStgMedium.hGlobal);
      if lpGroupDescriptor <> nil then
      try
        BeginUpdate;
        try
          ChildItem := nil;
          for i := 0 to lpGroupDescriptor.cItems-1 do
          begin
            ChildItem := nil;
            // Avoid Duplication
            //ChildItem := {Sender.}CaptionToItem(lpGroupDescriptor.fgd[i].cFileName,
              //{Sender.}DropTargetGroup);
            if ChildItem = nil then
            begin
              ChildItem := {Sender.}AddItem({Sender.}DropTargetGroup);
              sd := {Sender.}GetItemData(ChildItem);
             { sd.Add('1');
              sd.Add(lpGroupDescriptor.fgd[i].cFileName); }
              //sd.CommaText := lpGroupDescriptor.fgd[i].cFileName;

              subs := '';
              s := lpGroupDescriptor.fgd[i].cFileName;

              while (Pos('|', S) > 0) do
              begin
                subs := Copy(s, 1, Pos('|', S)-1);
                // text part
                sd.Add(subs);

                j := Pos('|', S) + 1;
                s := Copy(s, j, Length(S));

                // objects part
                subs := Copy(s, 1, Pos('|', S)-1);
                h := StrToInt(subs);
                sd.Objects[sd.Count - 1] := TObject(h);
                j := Pos('|', S)+1;
                s := Copy(s, j, Length(S));
              end;

              if Assigned(FOnOLEItemDrop) then
                FOnOLEItemDrop(self, Sd);
            end;
            // try to get data from file and save it
            tmpFormatEtc.cfFormat := CF_FILECONTENTS;
            tmpFormatEtc.lindex := i;
            if Succeeded(DataObject.GetData(tmpFormatEtc, tmpStgMedium1)) then
            begin
              if (tmpStgMedium1.tymed and TYMED_ISTREAM <> 0) and
                 (tmpStgMedium1.stm <> nil) then
              begin
                OleStream := TOleStream.Create(IStream(tmpStgMedium1.stm));
                try
                  SL := TStringList.Create;
                  try
                    SL.LoadFromStream(OleStream);
                    SL.SaveToFile(ExtractFilePath(Application.ExeName) +
                      lpGroupDescriptor.fgd[i].cFileName);
                  finally
                    FreeAndNil(SL);
                  end;
                finally
                  FreeAndNil(OleStream);
                end;
              end;
            end;
          end;
          if ChildItem <> nil then
          begin
            FocusedItem := ChildItem;
            ScrollIntoView(FocusedItem)
          end;
        finally
          EndUpdate;
        end;
      finally
        GlobalUnlock(tmpStgMedium.hGlobal);
      end;
      Exit;
    end;

  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnListOLEGetClipboardFormats(
  Sender: TOutlookGroupedList; var Formats: TFormatEtcArray);
begin
  if Assigned(FOnOLEGetClipboardFormats) then
    FOnOLEGetClipboardFormats(self, Formats)
  else
  begin
    SetLength(Formats, 3);
    Formats[0] := DefaultGlobalClipboardFormat;
    Formats[0].cfFormat := CF_FILEGROUPDESCRIPTOR;
    Formats[1] := DefaultGlobalClipboardFormat;
    Formats[1].cfFormat := CF_FILECONTENTS;
    Formats[2] := DefaultGlobalClipboardFormat;
    Formats[2].cfFormat := CF_FILEGROUPDESCRIPTOR;
    Formats[2].tymed := TYMED_HGLOBAL or TYMED_ISTORAGE;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnListOLEGetData(
  Sender: TOutlookGroupedList; const FormatEtcIn: tagFORMATETC;
  out Medium: tagSTGMEDIUM; var Result: HRESULT);

var
  GroupItem: POGLItem;
  ChildItem: POGLItem;
  sd: TStrings;
  lpGroupDescriptor: PFileGroupDescriptor;
  lpData: PCardinal;
  FileName: String;
  FS: TFileStream;
  i: Integer;
  s: String;
begin
  if Assigned(FOnOLEGetData) then
    FOnOLEGetData(self, FormatEtcIn, Medium, Result)
  else
  begin
    Result := E_FAIL;
    if FormatEtcIn.cfFormat = CF_FILEGROUPDESCRIPTOR then
    begin
      FDragFileList.Clear;
      // fill the FileList from selected items
      GroupItem := {AdvOutlookList}{Sender.}RootItem.FirstChild;
      while GroupItem <> nil do
      begin
        if GroupItem.ChildSelectedCount > 0 then
        begin
          ChildItem := GroupItem.FirstChild;
          while ChildItem <> nil do
          begin
            if IsItemSelected(ChildItem) then
            begin
              sd := {AdvOutLookList}{Sender.}GetItemData(ChildItem);
              if Assigned(FOnOLEGetItemData) then
                FOnOLEGetItemData(self, Sd);
              //FDragFileList.Add(ExtractFilePath(Application.ExeName) + sd.commatext{sd[1]});
              s := '';
              for i:= 0 to Sd.Count-1 do
                s := s + sd[i] + '|' + IntToStr(integer(sd.Objects[i]))+'|';

              FDragFileList.Add(s);
            end;
            ChildItem := ChildItem.NextSibling;
          end;
        end;
        GroupItem := GroupItem.NextSibling;
      end;

      Medium.hGlobal := GlobalAlloc(GMEM_FIXED, SizeOf(FILEGROUPDESCRIPTOR) +
        SizeOf(FILEDESCRIPTOR) * FDragFileList.Count);
      if Medium.hGlobal = 0 then
        // failed to allocate the memory block, raise exception
        OutOfMemoryError else
      begin
        Medium.tymed := TYMED_HGLOBAL;
        lpGroupDescriptor := GlobalLock(Medium.hGlobal);
        if lpGroupDescriptor = nil then
        begin
          (* Could not lock the memory block, so free it again and
             raise an out of memory exception. *)
          GlobalFree(Medium.hGlobal);
          Medium.hGlobal := 0;
          OutOfMemoryError;
        end else
        try
          lpGroupDescriptor.cItems := FDragFileList.Count;
          for i := 0 to FDragFileList.Count-1 do
          begin
            FillChar(lpGroupDescriptor.fgd[i], SizeOf(lpGroupDescriptor.fgd[i]), 0);
            lpGroupDescriptor.fgd[i].dwFlags := FD_ATTRIBUTES or FD_FILESIZE;
            lpGroupDescriptor.fgd[i].dwFileAttributes := FILE_ATTRIBUTE_NORMAL;
            lpGroupDescriptor.fgd[i].nFileSizeHigh := 0;
            lpGroupDescriptor.fgd[i].nFileSizeLow := 128;
            StrPCopy(lpGroupDescriptor.fgd[i].cFileName,
              {ExtractFileName(}FDragFileList[i]{)});
          end;
        finally
          GlobalUnlock(Medium.hGlobal);
          Result := S_OK;
        end;
      end;
    end else
    if FormatEtcIn.cfFormat = CF_FILECONTENTS then
    begin
      if not (FormatEtcIn.lindex in [0..FDragFileList.Count-1]) then Exit;
      FileName := FDragFileList[FormatEtcIn.lindex];
      if not FileExists(FileName) then Exit;
      FS := TFileStream.Create(FileName, fmOpenRead	or fmShareDenyWrite);
      try
        // return the Random file contents
        Medium.hGlobal := GlobalAlloc(GHND or GMEM_SHARE, FS.Size);
        if Medium.hGlobal = 0 then
          // failed to allocate the memory block, raise exception
          OutOfMemoryError else
        begin
          Medium.tymed := TYMED_HGLOBAL;
          lpData := GlobalLock(Medium.hGlobal);
          if lpData = nil then
          begin
            (* Could not lock the memory block, so free it again and
               raise an out of memory exception. *)
            GlobalFree(Medium.hGlobal);
            Medium.hGlobal := 0;
            OutOfMemoryError;
          end else
          try
            FS.Position := 0;
            FS.Read(lpData^, FS.Size);
            Result := S_OK;
          finally
            GlobalUnlock(Medium.hGlobal);
          end;
        end;
      finally
        FreeAndNil(FS);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.GetDropTargetGroup: POGLItem;
begin
  Result := nil;
  if Assigned(FList) then
   Result := FList.DropTargetGroup;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnListDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (FDragDropSetting = ddDisabled) then
  begin
    Accept := False;
    Exit;
  end;

  if Assigned(OnDragOver) then
    OnDragOver(self, Source, X, Y, State, Accept)
  else
  begin
    Accept := (Source is TOutlookGroupedList) or (Source is TAdvOutlookList);
    if (Sender = Source) then
      Accept := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnListDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  srcGroupItem: POGLItem;
  srcChildItem: POGLItem;
  dstGroupItem: POGLItem;
  dstChildItem: POGLItem;
  //lpSrcActionInfo: PActionInfo;
  //lpDstActionInfo: PActionInfo;
  Srcpoi, Dstpoi: POutlookInfo;
  Sd, Sr: TStrings;
  tmpCaption: String;
begin
  if Assigned(OnDragDrop) then
    OnDragDrop(self, Source, X, Y)
  else
  begin
    if not ((Source is TOutlookGroupedList) or (Source is TAdvOutlookList)) then
      Exit;

    TOutlookGroupedList(Sender).BeginUpdate;
    try
      srcGroupItem := TOutlookGroupedList(Source).FirstGroupItem;
      while srcGroupItem <> nil do
      begin
        if TOutlookGroupedList(Source).IsItemSelected(srcGroupItem) or
          (srcGroupItem.ChildSelectedCount > 0) then
        begin
          // the source Group has any selected child items
          tmpCaption := TOutlookGroupedList(Source).GetCaption(srcGroupItem);
          dstGroupItem := TOutlookGroupedList(Sender).CaptionToItem(tmpCaption);
          // create group if not found
          if dstGroupItem = nil then
            dstGroupItem := TOutlookGroupedList(Sender).AddItem;
          Dstpoi := TOutlookGroupedList(Sender).GetItemData(dstGroupItem);
          Sd := DstPoi.data;
          Srcpoi := TOutlookGroupedList(Source).GetItemData(srcGroupItem);
          Sr := Srcpoi.data;
          Sd.CommaText := Sr.CommaText;
          //lpDstActionInfo^ := lpSrcActionInfo^;
          // search Child items
          srcChildItem := srcGroupItem.FirstChild;
          while srcChildItem <> nil do
          begin
            if TOutlookGroupedList(Source).IsItemSelected(srcChildItem) then
            begin
              tmpCaption := TOutlookGroupedList(Source).GetCaption(srcChildItem);
              dstChildItem := nil;
              {dstChildItem := TOutlookGroupedList(Sender).CaptionToItem(
                tmpCaption, dstGroupItem); }
              if dstChildItem = nil then
                //dstChildItem := TOutlookGroupedList(Sender).AddItem(dstGroupItem);
                dstChildItem := {Sender.}AddItem(dstGroupItem);
              //Dstpoi := TOutlookGroupedList(Sender).GetItemData(dstChildItem);
              //Sd := DstPoi.data;
              Sd := {Sender.}GetItemData(dstChildItem);
              Srcpoi := TOutlookGroupedList(Source).GetItemData(srcChildItem);
              Sr := Srcpoi.data;
              Sd.CommaText := Sr.CommaText;
              //lpDstActionInfo^ := lpSrcActionInfo^;
            end;
            srcChildItem := srcChildItem.NextSibling;
          end;
        end;
        srcGroupItem := srcGroupItem.NextSibling;
      end;
      TOutlookGroupedList(Sender).Sort;
    finally
      TOutlookGroupedList(Sender).EndUpdate;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.CaptionToItem(ACaption: String;
  const GroupItem: POGLItem): POGLItem;
begin
  Result := nil;
  if Assigned(FList) then
    Result := FList.CaptionToItem(ACaption, GroupItem);
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.AddItem(ParentItem: POGLItem): POGLItem;
var
  poi: POutlookInfo;
begin
  Result := nil;
  if Assigned(FList) then
  begin
    Result := FList.AddItem(ParentItem);
    poi := FList.GetItemData(Result);
    poi.data := TStringList.Create;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.GetFocusedItem: POGLItem;
begin
  Result := nil;
  if Assigned(FList) then
    Result := FList.FocusedItem;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetFocusedItem(const Value: POGLItem);
begin
  if Assigned(FList) then
    FList.FocusedItem := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.ScrollList(deltax,deltay: integer);
begin
  FList.VertScrollBar.Position :=  FList.VertScrollBar.Position + deltay;
  FList.HorzScrollBar.Position :=  FList.HorzScrollBar.Position + deltax;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.ScrollIntoView(Item: POGLItem;
  AutoScrollOnExpand: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FList) then
    Result := FList.ScrollIntoView(Item, AutoScrollOnExpand);
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.GetRootItem: POGLItem;
begin
  Result := nil;
  if Assigned(FList) then
    Result := FList.RootItem;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.IsItemSelected(Item: POGLItem): Boolean;
begin
  Result := False;
  if Assigned(FList) then
    Result := FList.IsItemSelected(Item);
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.GetItemData(Item: POGLItem): TStrings;
var
  poi: POutlookInfo;
begin
  Result := nil;
  if Assigned(FList) then
  begin
    poi := FList.GetItemData(Item);
    Result := poi.data;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnListGetHint(Sender: TOutlookGroupedList; Item: POGLItem; var HintText: String; var HintPos: TPoint);
var
  cellrect: TRect;
  pt: tpoint;
begin
  if Sender.IsGroupItem(Item) then
  begin
    if Assigned(FOnGetGroupHint) then
      FOnGetGroupHint(Self, Item, HintText);
  end
  else
  begin
    if (FMouseOverCol >= 0) and (FMouseOverCol < FColumns.Count) then
    begin
      if (Columns.Items[FMouseOverCol].ShowHint) then
      begin
        HintText := Columns.Items[FMouseOverCol].Hint;
        if Columns[FMouseOverCol].ColumnType = ctCheckBox then
        begin
          pt := ScreenToClient(HintPos);
          ColAtPoint(TProOutlookGroupedList(FList).GetRealRect(Item), pt.X, pt.Y, CellRect);
          if pt.X < CellRect.Right then
            HintPos.X := HintPos.X + CHECKBOX_SIZE;
        end;

        if Assigned(FOnGetItemHint) then
          FOnGetItemHint(Self, Item, FMouseOverCol, HintText);
      end
      else
        HintText := ''
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.MoveColumn(FromColIndex, ToColIndex: Integer);
begin
  MoveHeaderSec(MapColToHeaderSec(FromColIndex), MapColToHeaderSec(ToColIndex));
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.MoveHeaderSec(FromIndex, ToIndex: Integer);
var
  I, j: Integer;
  FromColIndex, ToColIndex: Integer;
begin
  if FromIndex = ToIndex then
    Exit;
  FromColIndex := MapHeaderSecToCol(FromIndex);
  ToColIndex := MapHeaderSecToCol(ToIndex);

  j := FColumns.Items[ToColIndex].HeaderSecOrgIndex;
  if (FromIndex < ToIndex) then
  begin
    for i := 0 to FColumns.Count-1 do
    begin
      if (FromIndex < FColumns.Items[i].HeaderSecIndex) and (FColumns.Items[i].HeaderSecIndex <= ToIndex) then
      begin
        FColumns.Items[i].HeaderSecIndex := FColumns.Items[i].HeaderSecIndex - 1;
        FColumns.Items[i].HeaderSecOrgIndex := FColumns.Items[i].HeaderSecOrgIndex - 1;
      end;
    end;
  end
  else
  begin
    for i := 0 to FColumns.Count-1 do
    begin
      if (FromIndex > FColumns.Items[i].HeaderSecIndex) and (FColumns.Items[i].HeaderSecIndex >= ToIndex) then
      begin
        FColumns.Items[i].HeaderSecIndex := FColumns.Items[i].HeaderSecIndex + 1;
        FColumns.Items[i].HeaderSecOrgIndex := FColumns.Items[i].HeaderSecOrgIndex + 1;
      end;
    end;
  end;
  FColumns.Items[FromColIndex].HeaderSecIndex := ToIndex;
  FColumns.Items[FromColIndex].HeaderSecOrgIndex := j; //ToIndex;

  //FOutLookList.Columns.Items[FromSection].Index := ToSection;
  HeaderChanged(Self);
  if Assigned(FOnHeaderDragDrop) then
    FOnHeaderDragDrop(Self, FromColIndex, ToColIndex);
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.GetHeaderSecIndex(ColIndex: Integer): Integer;
begin
  Result := ColIndex;
  if (Result > FGroupColumn) and (FGroupColumnDisplay = gdHidden) then
    Result := Result - 1;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.UpdateHeaderSecIndexes;
var
  i, j, k: Integer;
  Found: Boolean;
  CL: TList;
begin
  for i := 1 to Columns.Count do
  begin
    if ((FGroupColumn >= 0) and (FGroupColumn < FColumns.Count) and (((i-1) = FGroupColumn)) and (FGroupColumnDisplay = gdHidden)) or
       (FPreviewSettings.Active and (i-1 = FPreviewSettings.Column) and (FPreviewSettings.Column >= 0) and (FPreviewSettings.Column < FColumns.Count)) then
      Continue;

    if not Columns[i - 1].Visible and (FHeader.Sections.Count > 0) then
      FHeader.Sections.Delete(0{Columns[i - 1].HeaderSecIndex});
  end;

  CL := TList.Create;

  for j := 0 to FHeader.Sections.Count - 1 do
  begin
    Found := false;
    k := -1;
    for i := 1 to Columns.Count do
    begin
      if ((FGroupColumn >= 0) and (FGroupColumn < FColumns.Count) and (((i-1) = FGroupColumn)) and (FGroupColumnDisplay = gdHidden)) or
         (FPreviewSettings.Active and (i-1 = FPreviewSettings.Column) and (FPreviewSettings.Column >= 0) and (FPreviewSettings.Column < FColumns.Count))
          or not Columns[i - 1].Visible then
        Continue;

      if (CL.IndexOf(Columns[i - 1]) >= 0) then
        Continue;

     {
      if (j = Columns[i - 1].HeaderSecIndex) then
      begin
        Found := True;
        CL.Add(Columns[i - 1]);
        Break;
      end;
      }

      //if (j < Columns[i - 1].HeaderSecIndex) then
      begin
        if k = -1 then
          k := (i - 1)
        else
        begin
          if Columns[k].HeaderSecOrgIndex > Columns[i - 1].HeaderSecOrgIndex then
            k := (i - 1);
        end;
      end;
    end;

    if not Found and (k >= 0) then
    begin
      Columns[k].HeaderSecIndex := j;
      CL.Add(Columns[k]);
    end;
  end;

  // FF: Column Drag iss after setting visible = false to lower column, now addressed in MoveHeaderSec
  //for i := 0 to Columns.Count - 1 do
    //Columns[i].HeaderSecOrgIndex := Columns[i].HeaderSecIndex;

  CL.Free;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.IsGroupColumn(ColIndex: Integer): Boolean;
begin
  Result := ColIndex = FGroupColumn;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.IsPreviewColumn(ColIndex: Integer): Boolean;
begin
  Result := (FPreviewSettings.Active and (ColIndex = FPreviewSettings.Column) and (FPreviewSettings.Column >= 0) and (FPreviewSettings.Column < FColumns.Count));
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.IsGroupItem(Item: POGLItem): Boolean;
begin
  Result := FList.IsGroupItem(Item);
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.ItemAtXY(X,Y: Integer): POGLItem;
begin
  Y := Y - HeaderHeight;
  Result := FList.GetItemAt(X,Y,true);
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.GroupAtXY(X,Y: Integer): TOutlookGroup;
var
  pogl: POGLItem;
begin
  Result := nil;

  pogl := FList.GetItemAt(X,Y,true);

  if Assigned(pogl) then
  begin
    Result := ItemGroup(pogl);
  end;
end;

//------------------------------------------------------------------------------
{
procedure TAdvOutlookList.WMNCPaint(var Message: TMessage);
begin
  Message.Result := 0;
end;
}
//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetDragDropMode(const Value: TDragDropMode);
begin
  if FDragDropMode <> Value then
  begin
    FDragDropMode := Value;
    if Assigned(FList) then
      TProOutlookGroupedList(Flist).DragDropMode := FDragDropMode;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.GroupIndex(Group: TOutlookGroup): Integer;
var
  i: Integer;
  tmp: TOutlookGroup;

begin
  Result := -1;
  for i := 1 to GroupCount do
  begin
    tmp := Groups[i - 1];
    if tmp.OGLItem = Group.OGLItem  then
    begin
      Result := i - 1;
      Break;
    end;
  end;
end;


//------------------------------------------------------------------------------

procedure TAdvOutlookList.DeleteGroup(Index: Integer);
var
  olg: TOutlookGroup;
  i: integer;
begin
  olg := Groups[Index];

  for I := 0 to FGroupList.Count - 1 do
  begin
    if (FGroupList.Items[i] = pointer(olg)) then
    begin
      FGroupList.Delete(i);
      break;
    end;
  end;

  FList.DeleteItem(olg.OGLItem);
  FreeAndNil(olg);    
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.DeleteAllGroups;
var
  i: Integer;
begin
  GroupColumn := 0;

  for i := GroupCount - 1 downto 0 do
   DeleteGroup(i);

  FGroupList.Clear; 
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.GetGroup(Index: Integer): TOutlookGroup;
var
  i: integer;
  ogl: POGLItem;
begin
  ogl := RootItem.FirstChild;

  i := 0;

  if (Index < 0) then
    raise Exception.Create('Index out of bounds');

  if ogl <> nil then
  repeat
    if (i = Index) and IsGroupItem(ogl) then
    begin
      Result := FindGroup(ogl);

      if not Assigned(Result) then
      begin
        Result := TOutlookGroup.Create;
        Result.OGLItem := ogl;
        Result.List := Self;
        FGroupList.Add(Result);
      end;

      Exit;
    end;

    inc(i);
    ogl := ogl^.NextSibling;
  until (ogl = nil) or (i > Index);

  raise Exception.Create('Index out of bounds');
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.GetGroupCount: Integer;
var
  i: integer;
  ogl: POGLItem;
begin
  ogl := RootItem.FirstChild;
  i := 0;

  if ogl <> nil then
  repeat
    if IsGroupItem(ogl) then
      inc(i);
    ogl := ogl^.NextSibling;
  until (ogl = nil);

  Result := i;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.ItemGroup(Item: POGLItem): TOutlookGroup;
begin
  Result := TOutlookGroup.Create;
  FGroupList.Add(Result);

  if IsGroupItem(Item) then
    Result.OGLItem := Item
  else
    Result.OGLItem := Item.Parent;
    
  Result.List := Self;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetGroupItemHeight(const Value: Integer);
begin
  FGroupItemHeight := Value;
  TProOutlookGroupedList(FList).SetGroupItemHeight(FGroupItemHeight);
end;

procedure TAdvOutlookList.SetGroupLineColor(const Value: TColor);
begin
  FGroupLineColor := Value;
  if Assigned(FList) then
    TProOutlookGroupedList(FList).GroupLineColor := FGroupLineColor;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SelectAll;
begin
  FList.AddAllToSelection;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SelectItem(Item: POGLItem; Focus: boolean = true);
begin
  if not (soMultiSelect in SelectionOptions) then
    FList.ClearSelection;

  if not FList.Focused and FList.CanFocus and Focus then
    Windows.SetFocus(FList.Handle);
    
  FList.AddToSelection(Item, True);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.UnSelectItem(Item: POGLItem);
begin
  FList.RemoveFromSelection(item,true);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.UnSelectAll; // Walter
begin
  FList.ClearSelection;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.GetShowNodes: Boolean;
begin
  Result := TProOutlookGroupedList(FList).ShowNodes;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetShowNodes(const Value: Boolean);
begin
  TProOutlookGroupedList(FList).ShowNodes := Value;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.CollapseAll: Boolean;
begin
  Result := FList.CollapseAll;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.ExpandAll: Boolean;
begin
  Result := Flist.ExpandAll;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.Sort;
begin
  SortItems;
end;

//------------------------------------------------------------------------------

function TAdvOutlookList.ColumnIndex(Column: Integer): Integer;
begin
  if (FGroupColumnDisplay = gdHidden) then
  begin
    if Column = GroupColumn then
      Result := 0 // Group column has index 0
    else
      if Column < GroupColumn then
        Result := Column + 1
      else
        Result := Column;
  end
  else
     Result := Column;
end;

function TAdvOutlookList.GetFirstGroupItem: POGLItem;
begin
  Result := FList.FirstGroupItem;
end;

function TAdvOutlookList.GetFirstSelectedItem: POGLItem;
begin
  Result := FList.FirstSelectedItem;
end;

function TAdvOutlookList.GetSelectedCount: Integer;
begin
  Result := FList.SelectedCount;
end;

function TAdvOutlookList.NextSelectedItem(Item: POGLItem): POGLItem;
var
  GroupItem: POGLItem;
  ChildItem: POGLItem;
  OrgItem: POGLItem;
begin
  Result := nil;
  OrgItem := Item;

  if FList.SelectedCount = 0 then
    Exit;

  if IsGroupItem(Item) then
  begin
    Result := Item.FirstChild;
    Exit;
  end
  else
  begin
    GroupItem := Item.Parent;
  end;

  while (GroupItem <> nil) do
  begin
    if GroupItem.ChildSelectedCount > 0 then
    begin
      ChildItem := Item.NextSibling;

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
    if Assigned(GroupItem) then
      Item := GroupItem.FirstChild;

    if IsItemSelected(GroupItem) then
    begin
      Result := GroupItem;
      Exit;
    end;

    if IsItemSelected(Item) and (OrgItem <> Item) then
    begin
      Result := Item;
      Exit;
    end;

  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetGroupShowCount(const Value: Boolean);
begin
  FGroupShowCount := Value;
  if Assigned(FList) then
    TProOutlookGroupedList(FList).GroupShowCount := FGroupShowCount;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnListGetGroupImageIndex(Sender: TOutlookGroupedList;
  Item: POGLItem; var ImageIndex: Integer);
begin
  if (FGroupColumn >= 0) and (FGroupColumn < FColumns.Count) then
    ImageIndex := FColumns.Items[FGroupColumn].GroupImageIndex;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnListSelectionChange(Sender: TObject);
begin
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetHideSelection(const Value: Boolean);
begin
  FHideSelection := Value;
  if Assigned(FList) then
    TProOutlookGroupedList(FList).HideSelection := FHideSelection;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetLookUp(const Value: TLookUpSettings);
begin
  FLookUp.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetGroupColumnDisplay(
  const Value: TGroupColumnDisplay);
begin
  if (FGroupColumnDisplay <> Value) then
  begin
    FGroupColumnDisplay := Value;
    HeaderChanged(nil);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetDragType(const Value: TOGLDragType);
begin
  if (FDragType <> Value) then
  begin
    FDragType := Value;
    if Assigned(FList) then
      FList.DragType := DragType;
  end;    
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetComponentStyle(AStyle: TTMSStyle);
begin
  SetStyle(TAdvOutlookListStyle(AStyle));
end;

procedure TAdvOutlookList.SetStyle(AStyle: TAdvOutlookListStyle);
var
 i : integer;
 c : TAdvOutlookColumn;
begin
    for i := 0 to Columns.Count - 1 do
    begin
    c := columns[i];
    c.HeaderFont.Color:= clBlack;

       case AStyle of
          olsOffice2003Blue:
            begin
            c.HeaderColor := $FCE1CB;
            c.HeaderColorTo := $E0A57D;
            end;
          olsOffice2003Silver:
            begin
            c.HeaderColor := $ECE2E1;
            c.HeaderColorTo := $B39698;
            end;
          olsOffice2003Olive:
            begin
            c.HeaderColor := $CFF0EA;
            c.HeaderColorTo := $8CC0B1;
            end;
          olsOffice2003Classic:
            begin
            c.HeaderColorTo := clBtnFace;
            c.HeaderColor := clWhite;
            end;
          olsOffice2007Luna:
            begin
            c.HeaderColorTo := $FFD2AF;
            c.HeaderColor := clWhite;
            end;
          olsOffice2007Obsidian:
            begin
            c.HeaderColorTo := $C9C2BD;
            c.HeaderColor := $F2F1F0;
            end;
          olsOffice2007Silver:
            begin
            c.HeaderColorTo := $DCD7D4;
            c.HeaderColor := clWhite;
            end;            
          olsWhidbey:
            begin
            c.HeaderColorTo := clBtnFace;
            c.HeaderColor := clWhite;
            end;
          olsWindowsXP:
            begin
            c.HeaderColorTo := clBtnFace;
            c.HeaderColor := clBtnFace;
            end;
          olsWindowsVista:
            begin
            c.HeaderColorTo := $FDF8F1;
            c.HeaderColor := $FCEFD5;
            end;
          olsWindows7:
            begin
            c.HeaderColorTo := $FCEBDC;
            c.HeaderColor := $FCDBC1;
            end;
          olsTerminal:
            begin
            c.HeaderColorTo := clBtnFace;
            c.HeaderColor := clBtnFace;
            end;
          olsOffice2010Blue:
            begin
            c.HeaderColorTo := $FDF6EF;
            c.HeaderColor := $F0DAC7;
            end;
          olsOffice2010Silver:
            begin
            c.HeaderColorTo := $FFFFFF;
            c.HeaderColor := $EDE5E0;
            end;
          olsOffice2010Black:
            begin
            c.HeaderColorTo := $BFBFBF;
            c.HeaderColor := $919191;
            end;
          olsWindows8, olsWindows10:
            begin
            c.HeaderColorTo := $F7F6F5;
            c.HeaderColor := $F7F6F5;
            end;
          olsOffice2013White:
            begin
            c.HeaderColorTo := clWhite;
            c.HeaderColor := clWhite;
            end;
          olsOffice2013LightGray:
            begin
            c.HeaderColorTo := $FAFAFA;
            c.HeaderColor := $FAFAFA;
            end;
          olsOffice2013Gray:
            begin
            c.HeaderColorTo := $F3F3F3;
            c.HeaderColor := $F3F3F3;
            end;
          olsOffice2016White:
            begin
            c.HeaderColorTo := clWhite;
            c.HeaderColor := clWhite;
            end;
          olsOffice2016Gray:
            begin
            c.HeaderColorTo := $444444;
            c.HeaderColor := $444444;
            c.HeaderFont.Color:= $F0F0F0;
            end;
          olsOffice2016Black:
            begin
            c.HeaderColorTo := $444444;
            c.HeaderColor := $444444;
            c.HeaderFont.Color:= $F0F0F0;
            end;
        end;
      end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetGroupColor(const Value: TColor);
begin
  FGroupColor := Value;
  if Assigned(FList) then
    TProOutlookGroupedList(FList).GroupColor := FGroupColor;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetGroupCountFont(const Value: TFont);
begin
  FGroupCountFont.Assign(Value);
  if Assigned(FList) then
    TProOutlookGroupedList(FList).GroupCountFont.Assign(FGroupCountFont);
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetGroupFont(const Value: TFont);
begin
  FGroupFont.Assign(Value);
  if Assigned(FList) then
    TProOutlookGroupedList(FList).GroupFont.Assign(FGroupFont);
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetGroupSelectionColor(const Value: TColor);
begin
  FGroupSelectionColor := Value;
  if Assigned(FList) then
    TProOutlookGroupedList(FList).GroupSelectionColor := FGroupSelectionColor;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.SetGroupSelectionTextColor(const Value: TColor);
begin
  FGroupSelectionTextColor := Value;
  if Assigned(FList) then
    TProOutlookGroupedList(FList).GroupSelectionTextColor := FGroupSelectionTextColor;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnGroupCountFontChanged(Sender: TObject);
begin
  if Assigned(FList) then
    TProOutlookGroupedList(FList).GroupCountFont.Assign(FGroupCountFont);
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookList.OnGroupFontChanged(Sender: TObject);
begin
  if Assigned(FList) then
    TProOutlookGroupedList(FList).GroupFont.Assign(FGroupFont);
end;

//------------------------------------------------------------------------------

{ TAdvOutlookColumn }

procedure TAdvOutlookColumn.Changed;
begin
  TAdvOutlookColumns(Collection).Changed;
end;

//------------------------------------------------------------------------------

{ TAdvOutlookColumn }

constructor TAdvOutlookColumn.Create(Collection: TCollection);
begin
  inherited;
  FVisible := True;
  FWidth := 50;
  FFont := TFont.Create;
  FFont.OnChange := OnFontChanged;
  FColor := clNone;
  FAlignment := taLeftJustify;
  FColumnType := ctText;
  FHeaderFont := TFont.Create;
  FHeaderFont.OnChange := OnFontChanged;
  FHeaderColor := clWhite;
  FHeaderColorTo := clBtnFace;
  FHeaderAlignment := taCenter;
  FHeaderImageIndex := -1;
  FHeaderGradientDir := gdVertical;
  //FHeaderBorderColor := clBlack;
  FURLType := utHTTP;
  FSortType := stTextNoCase;
  FHeaderHint := '';
  FHeaderShowHint := False;
  FHint := '';
  FShowHint := False;
  FGroupImageIndex := -1;
  FHeaderSecIndex := Index;
  if Assigned(TAdvOutlookColumns(Collection).AdvOutLookList) then
    FHeaderSecIndex := TAdvOutlookColumns(Collection).AdvOutLookList.GetHeaderSecIndex(Index);
  FHeaderSecOrgIndex := FHeaderSecIndex;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.Assign(Source: TPersistent);
begin
  if (Source is TAdvOutlookColumn) then
  begin
    Caption := (Source as TAdvOutlookColumn).Caption;
    Width := (Source as TAdvOutlookColumn).Width;
    Font.Assign((Source as TAdvOutlookColumn).Font);
    Color := (Source as TAdvOutlookColumn).Color;
    Alignment := (Source as TAdvOutlookColumn).Alignment;
    ColumnType := (Source as TAdvOutlookColumn).ColumnType;
    URLType := (Source as TAdvOutlookColumn).URLType;
    HeaderFont.Assign((Source as TAdvOutlookColumn).HeaderFont);
    HeaderColor := (Source as TAdvOutlookColumn).HeaderColor;
    HeaderColorTo := (Source as TAdvOutlookColumn).HeaderColorTo;
    HeaderGradientDir := (Source as TAdvOutlookColumn).HeaderGradientDir;
    HeaderAlignment := (Source as TAdvOutlookColumn).HeaderAlignment;
    HeaderImageIndex := (Source as TAdvOutlookColumn).HeaderImageIndex;
    HeaderHint := (Source as TAdvOutlookColumn).HeaderHint;
    HeaderShowHint := (Source as TAdvOutlookColumn).HeaderShowHint;
    Hint := (Source as TAdvOutlookColumn).Hint;
    ShowHint := (Source as TAdvOutlookColumn).ShowHint;
    SortType := (Source as TAdvOutlookColumn).SortType;
    GroupImageIndex := (Source as TAdvOutlookColumn).GroupImageIndex;
  end
  else
    inherited;

end;

//------------------------------------------------------------------------------

destructor TAdvOutlookColumn.Destroy;
begin
  if Assigned(TAdvOutlookColumns(Collection).AdvOutLookList) and not (csDestroying in TAdvOutlookColumns(Collection).AdvOutLookList.ComponentState) then
  begin
    TAdvOutlookColumns(Collection).UpdateOnDeleteItem(Index);
  end;
  FFont.Free;
  FHeaderFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TAdvOutlookColumn.GetDisplayName: string;
begin
  if Caption <> '' then
    Result := Caption
  else
    Result := 'Data column '+IntToStr(Index);
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.OnFontChanged(Sender: TObject);
begin
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.Refresh;
begin
  TAdvOutlookColumns(Collection).Refresh;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetCaption(const Value: string);
begin
  FCaption := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetColumnType(const Value: TColumnType);
begin
  if FColumnType <> Value then
  begin
    FColumnType := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetHeaderAlignment(const Value: TAlignment);
begin
  if FHeaderAlignment <> Value then
  begin
    FHeaderAlignment := Value;
    Changed;
  end;
end;
{
procedure TAdvOutlookColumn.SetHeaderBorderColor(const Value: TColor);
begin
  if FHeaderBorderColor <> Value then
  begin
    FHeaderBorderColor := Value;
    Changed;
  end;
end;
}

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetHeaderColor(const Value: TColor);
begin
  if FHeaderColor <> Value then
  begin
    FHeaderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetHeaderColorTo(const Value: TColor);
begin
  if FHeaderColorTo <> Value then
  begin
    FHeaderColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetHeaderFont(const Value: TFont);
begin
  FHeaderFont.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetHeaderGradientDir(
  const Value: TGradientDir);
begin
  if FHeaderGradientDir <> Value then
  begin
    FHeaderGradientDir := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetHeaderImageIndex(const Value: Integer);
begin
  if FHeaderImageIndex <> Value then
  begin
    FHeaderImageIndex := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetHeaderSecIndex(const Value: Integer);
begin
  FHeaderSecIndex := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetIndex(Value: Integer);
begin
  inherited;
  if Assigned(TAdvOutlookColumns(Collection).AdvOutLookList) and (csDesigning in TAdvOutlookColumns(Collection).AdvOutLookList.ComponentState) then
    TAdvOutlookColumns(Collection).UpdateOnIndexChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetSortType(const Value: TSortType);
begin
  if FSortType <> Value then
  begin
    FSortType := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetURLType(const Value: TURLType);
begin
  if FURLType <> Value then
  begin
    FURLType := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetWidth(const Value: Integer);
begin
  FWidth := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetGroupImageIndex(const Value: Integer);
begin
  if (FGroupImageIndex <> Value) then
  begin
    FGroupImageIndex := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumn.SetHeaderSecOrgIndex(const Value: Integer);
begin
  FHeaderSecOrgIndex := Value;
end;

//------------------------------------------------------------------------------

{ TAdvOutlookColumns }

function TAdvOutlookColumns.Add: TAdvOutlookColumn;
begin
  Result := TAdvOutlookColumn(inherited Add);
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumns.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TAdvOutlookColumns.Create(AOwner: TComponent);
begin
  inherited Create(TAdvOutlookColumn);
  FOwner := AOwner;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumns.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

//------------------------------------------------------------------------------

function TAdvOutlookColumns.GetItem(Index: Integer): TAdvOutlookColumn;
begin
  Result := TAdvOutlookColumn(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TAdvOutlookColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvOutlookColumns.Insert(Index: Integer): TAdvOutlookColumn;
begin
  Result := TAdvOutlookColumn(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumns.Refresh;
begin
  if Assigned(FOnRefresh) then
    FOnRefresh(self);
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumns.SetItem(Index: Integer;
  const Value: TAdvOutlookColumn);
begin
  Items[Index] := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumns.Update(Item: TCollectionItem);
begin
  inherited;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumns.UpdateOnDeleteItem(Index: Integer);
var
  i: Integer;
begin
  if Assigned(AdvOutLookList) and (Index >= 0) and (Index < Count) then
  begin
    for i := 0 to Count-1 do
    begin
      if (Items[Index].FHeaderSecIndex < Items[i].FHeaderSecIndex) then
      begin
        Items[i].FHeaderSecIndex := Items[i].FHeaderSecIndex - 1;
        Items[i].FHeaderSecOrgIndex := Items[i].FHeaderSecOrgIndex - 1;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutlookColumns.UpdateOnIndexChanged;
var
  i: Integer;
begin
  if Assigned(AdvOutLookList) then
  begin
    for i := 0 to Count-1 do
    begin
      Items[i].FHeaderSecIndex := AdvOutLookList.GetHeaderSecIndex(Items[i].Index);
      Items[i].FHeaderSecOrgIndex := Items[i].FHeaderSecIndex;
    end;
  end;
end;

//------------------------------------------------------------------------------

{ TURLSettings }

procedure TURLSettings.Assign(Source: TPersistent);
begin
  if Source is TURLSettings then
  begin
    FColor := TURLSettings(Source).Color;
    FFontStyle := TURLSettings(Source).FontStyle;
    inherited Assign(Source);
  end;
end;

//------------------------------------------------------------------------------

procedure TURLSettings.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

//------------------------------------------------------------------------------

constructor TURLSettings.Create;
begin
  inherited;
  FColor := clBlue;
  FFontStyle := [fsUnderLine];
end;

//------------------------------------------------------------------------------

destructor TURLSettings.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

procedure TURLSettings.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TURLSettings.SetFontStyle(const Value: TFontStyles);
begin
  if FFontStyle <> Value then
  begin
    FFontStyle := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvOutLookHeader }

constructor TAdvOutLookHeader.Create(AOwner: TComponent);
begin
  inherited;
  if not (AOwner is TAdvOutLookList) then
    raise exception.Create('Invalid Owner');

  BorderWidth := 0;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  BorderStyle := bsNone;
  FOutLookList := TAdvOutlookList(AOwner);
  FOnDragDrop := OwnOnDragDrop;
  FMouseOverSec := -1;
  OnSizing := OwnSizing;
  FSectionDragDrop := True;
  inherited OnSized := OwnSized;
end;

//------------------------------------------------------------------------------

destructor TAdvOutLookHeader.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

function TAdvOutLookHeader.GetColIndex(SecIndex: Integer): Integer;
begin
  if FOutLookList.Columns.Count <= SecIndex then
    raise exception.Create('Section Index exceeding Column Index');
  Result := SecIndex;
end;

//------------------------------------------------------------------------------

procedure TAdvOutLookHeader.Paint;
var
  i, sp, tw: Integer;
  R, SecRect, TxtRect: TRect;
  format: Cardinal;
  Ip: TPoint;
  S, sa,sv,fa: string;
  ml,hl,xsize,ysize: integer;
  hr: TRect;
begin
  //inherited;

  with Canvas do
  begin
    Pen.Width := 1;

    R := ClientRect;
    if FOutLookList.HeaderBorderColor <> clNone then
    begin
      Brush.Style := bsClear;
      Pen.Color := FOutLookList.HeaderBorderColor;
      Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      R := Rect(R.Left+1, R.Top+1, R.Right-1, R.Bottom-1);
    end;

    SecRect := Rect(R.Left, R.Top, R.Left, R.Bottom);
    sp := 4;   // space between Image and Text

    for i := 0 to Sections.Count - 1 do
    begin
      with FOutLookList.Columns.Items[FOutLookList.MapHeaderSecToCol(i)] do  //GetColIndex(i)
      begin
        SecRect.Left := SecRect.Right;
        SecRect.Right := SecRect.Left + SectionWidth[i];
        if (HeaderColor <> clNone) and (HeaderColorTo <> clNone) then
          DrawGradient(Canvas, HeaderColor, HeaderColorTo, 40, SecRect, HeaderGradientDir = gdHorizontal)
        else if (HeaderColor <> clNone) then
        begin
          Brush.Color := HeaderColor;
          Pen.Color := HeaderColor;
          Rectangle(SecRect.Left, SecRect.Top, SecRect.Right, SecRect.Bottom);
        end;

        if FOutLookList.HeaderBorderColor <> clNone then
        begin
          Brush.Style := bsClear;
          Pen.Color := FOutLookList.HeaderBorderColor;
          //Rectangle(Rect(SecRect.Left, SecRect.Top, SecRect.Right, SecRect.Bottom));
          MoveTo(SecRect.Right - 2, SecRect.Top);
          LineTo(SecRect.Right - 2, SecRect.Bottom);
          if i = Sections.Count - 1 then
          begin
            MoveTo(SecRect.Right - 2, SecRect.Top);
            LineTo(SecRect.Right - 2, SecRect.Bottom);
            SecRect.Right := SecRect.Right - 2;
          end;
        end;

        Canvas.Font.Assign(HeaderFont);
        TxtRect := SecRect;
        tw := TextWidth(Sections[i]);
        case Headeralignment of
          taLeftJustify:  format := DT_LEFT;
          taRightJustify: format := DT_RIGHT;
          else            format := DT_CENTER;
        end;

        if Assigned(FOutLookList.Images) and (HeaderImageIndex >= 0) then
        begin
          Ip.Y := ((SecRect.Bottom - SecRect.Top) - FOutLookList.Images.Height) div 2;
          case HeaderAlignment of
            taLeftJustify:
            begin
              Ip.X := SecRect.Left;
              TxtRect.left := Ip.X + FOutLookList.Images.Width + sp;
            end;
            taRightJustify:
            begin
              Ip.X := SecRect.Left + ((SecRect.Right - SecRect.Left) -(FOutLookList.Images.Width + sp + tw));
              TxtRect.Left := Ip.X + FOutLookList.Images.Width + sp;
            end;
            else
            begin
              Ip.X := SecRect.Left + (((SecRect.Right - SecRect.Left) - (FOutLookList.Images.Width + sp + tw)) div 2);
              TxtRect.Left := Ip.X + FOutLookList.Images.Width + sp;
              format := DT_LEFT;
            end;
          end;

          if Ip.X >= SecRect.Left then
            FOutLookList.Images.Draw(Canvas, Ip.X, Ip.Y, HeaderImageIndex);
        end;

        if (FOutLookList.SortSettings.Column = FOutLookList.MapHeaderSecToCol(i)) and (FOutLookList.Columns.Items[FOutLookList.MapHeaderSecToCol(i)].SortType <> stNone) then
        begin
          if FOutLookList.SortSettings.Direction = sdDescending then
          begin
            if not FOutLookList.SortSettings.GlyphDown.Empty then
              TxtRect.Right := TxtRect.Right - FOutLookList.SortSettings.GlyphDown.Width
            else
              TxtRect.Right := TxtRect.Right - 16;
          end
          else
          begin
            if not FOutLookList.SortSettings.GlyphUp.Empty then
              TxtRect.Right := TxtRect.Right - FOutLookList.SortSettings.GlyphUp.Width
            else
              TxtRect.Right := TxtRect.Right - 16;
          end;
        end;

        Brush.Style := bsClear;

        case HeaderAlignment of
        taLeftJustify:  TxtRect.Left := TxtRect.Left + 2;
        taRightJustify: TxtRect.Right := TxtRect.Right - 4;
        end;

        //
        s := Sections[i];

        if pos('</',s) > 0 then
        begin
          OffsetRect(TxtRect,2,2);
          FOutlookList.HTMLPaint(Canvas,s,TxtRect,FOutlookList.Images,-1,-1,-1,-1,2,false,false,false,false,false,false,1.0,clBlue,clNone,clNone,clGray,sa,sv,fa,xsize,ysize,hl,ml,hr);
        end
        else
          DrawText(Canvas.Handle, PChar(s), Length(s), TxtRect, DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER or format);

        if (FOutLookList.SortSettings.Column = FOutLookList.MapHeaderSecToCol(i)) and (FOutLookList.Columns.Items[FOutLookList.MapHeaderSecToCol(i)].SortType <> stNone) then
        begin

          DrawText(Canvas.Handle, PChar(Sections[i]), Length(Sections[i]), TxtRect, DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER or format or DT_CALCRECT);

          if (TxtRect.Right > TxtRect.Left) then
          begin
            if (SecRect.Right > TxtRect.Right) then
              DrawSortIndicator(Canvas, SecRect, SecRect.Right - 10, R.Top + (R.Bottom - R.Top)div 2)
            else
              DrawSortIndicator(Canvas, SecRect, TxtRect.Right + 8, R.Top + (R.Bottom - R.Top)div 2)
          end;
        end;

      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutLookHeader.WMLButtonDown(var Message: TWMLButtonDown);
{var
  r: TRect;
  Column: Integer;}
begin
  if FSectionDragDrop and not FDragging then
  begin
    FDragStart := XYToSection(Message.XPos, Message.YPos);

    if (FDragStart >= FOffset) then
    begin
      FMouseDragPos := Point(Message.XPos, Message.YPos);

      //FDragging := True;
      //Self.Cursor := crDrag;
      //SetCapture(Self.Handle);
    end;
  end;

  inherited;
{
  Column := XYToSection(Message.XPos, Message.YPos);

  with FOutLookList do
  begin
    r := GetSectionRect(Column);

    if (Column >= 0) and (Column < FOutLookList.Columns.Count) and
      (Abs(Message.Xpos - r.Left) > 4) and (Abs(Message.XPos - r.Right) > 4) then
    begin
      if FOutLookList.SortSettings.Column = FOutLookList.MapHeaderSecToCol(Column) then
      begin
        if FOutLookList.SortSettings.Direction = sdAscending then
          FOutLookList.SortSettings.Direction := sdDescending
        else
          FOutLookList.SortSettings.Direction := sdAscending;
      end
      else
      begin
        FOutLookList.SortSettings.Column := FOutLookList.MapHeaderSecToCol(Column);//Column;
      end;
    end;
  end;
}
  //if Assigned(FOnClick) then
    //FOnClick(Self, Column);

end;

//------------------------------------------------------------------------------

procedure TAdvOutLookHeader.WMLButtonUp(var Message: TWMLButtonUp);
var
  DragStop, Column: Integer;
  r: TRect;
  DoClick: Boolean;
begin
  DoClick := not FReSizing;
  inherited;

  Column := XYToSection(Message.XPos, Message.YPos);

  if DoClick then
    DoClick := not FDragging;
  //if Assigned(FOnClick) and DoClick then
    //FOnClick(Self, Column);

  if not FDragging then
  begin
    with FOutLookList do
    begin
      r := GetSectionRect(Column);

      if (Column >= 0) and (Column < FOutLookList.Columns.Count) and
        (Abs(Message.Xpos - r.Left) > 4) and (Abs(Message.XPos - r.Right) > 4) then
      begin
        if FOutlookList.SortSettings.Enabled then
        begin
          if FOutLookList.SortSettings.Column = FOutLookList.MapHeaderSecToCol(Column) then
          begin
            if FOutLookList.SortSettings.Direction = sdAscending then
              FOutLookList.SortSettings.Direction := sdDescending
            else
              FOutLookList.SortSettings.Direction := sdAscending;
          end
          else
          begin
            FOutLookList.SortSettings.Column := FOutLookList.MapHeaderSecToCol(Column);//Column;
          end;
        end;
      end;
    end;
  end;

  if FSectionDragDrop and FDragging then
  begin
    FDragging := False;
    Screen.Cursor := crDefault;
    ReleaseCapture;
    if Assigned(FOnDragDrop) then
    begin
      DragStop := XYToSection(Message.xpos, Message.ypos);

      if (DragStop >= FOffset) and (DragStop <> FDragStart) then
        FOnDragDrop(Self, FDragStart, DragStop);
    end;
  end;

  if Assigned(FOnClick) and DoClick then
    FOnClick(Self, Column);
end;

//------------------------------------------------------------------------------

procedure TAdvOutLookHeader.WMRButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
{  if Assigned(FOnRightClick) then
    FOnRightClick(Self, XYToSection(Message.xpos, Message.ypos));
}
end;

//------------------------------------------------------------------------------

function TAdvOutLookHeader.XYToSection(X, Y: Integer): Integer;
var
  Ofs, SectionIndex: Integer;
begin
  Ofs := 0;
  SectionIndex := 0;
  while (Ofs < X) and (SectionIndex < Sections.Count) do
  begin
    Ofs := Ofs + SectionWidth[SectionIndex];
    Inc(SectionIndex);
  end;
  Dec(SectionIndex);

  Result := SectionIndex;
end;

//------------------------------------------------------------------------------

function TAdvOutLookHeader.GetSectionRect(X: Integer): TRect;
var
  Offset, SectionIndex: Integer;
begin
  Offset := 0;
  for SectionIndex := 0 to X - 1 do
    Offset := Offset + SectionWidth[SectionIndex];

  Result.Left := Offset;
  Result.Right := Offset + SectionWidth[X];
  Result.Top := 0;
  Result.Bottom := Self.Height;
end;

//------------------------------------------------------------------------------

procedure TAdvOutLookHeader.WMRButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  if Assigned(FOnRightClick) then
    FOnRightClick(Self, XYToSection(Message.xpos, Message.ypos));
end;

//------------------------------------------------------------------------------

procedure TAdvOutLookHeader.WMLButtonDblClk(var Message: TWMLButtonDBLClk);
begin
  inherited;
  if Assigned(FOnDblClick) then
    FOnDblClick(Self, XYToSection(Message.xpos, Message.ypos));
end;

//------------------------------------------------------------------------------

procedure TAdvOutLookHeader.Click;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvOutLookHeader.DrawSortIndicator(Canvas: TCanvas;  SectionRect: TRect;
  X, Y: Integer);
var
  lft, vpos: Integer;
begin
  lft := x;
  vpos := y;

  if FOutLookList.SortSettings.Direction = sdDescending then
  begin
  {draw a full Colored triangle}
    if not FOutLookList.SortSettings.GlyphDown.Empty then
    begin
      lft := SectionRect.Right - FOutLookList.SortSettings.GlyphDown.Width;
      vpos := SectionRect.Top + (SectionRect.Bottom - SectionRect.Top - FOutLookList.SortSettings.GlyphDown.Height) div 2;
      FOutLookList.SortSettings.GlyphDown.Transparent := True;
      Canvas.Draw(lft, vpos, FOutLookList.SortSettings.GlyphDown);
    end
    else
    begin
      Canvas.Pen.Color := clWhite;
      Canvas.Pen.Width := 1;
      Canvas.MoveTo(Lft + 4, vpos - 4);
      Canvas.LineTo(Lft, vpos + 4);
      Canvas.Pen.Color := clGray;
      Canvas.LineTo(Lft - 4, vpos - 4);
      Canvas.LineTo(Lft + 4, vpos - 4);
      Canvas.Pen.Color := clBlack;
    end;
  end
  else
  begin
    if not FOutLookList.SortSettings.GlyphUp.Empty then
    begin
      lft := SectionRect.Right - FOutLookList.SortSettings.GlyphUp.Width;
      vpos := SectionRect.Top + (SectionRect.Bottom - SectionRect.Top - FOutLookList.SortSettings.GlyphUp.Height) div 2;
      FOutLookList.SortSettings.GlyphUp.Transparent := True;
      Canvas.Draw(lft, vpos, FOutLookList.SortSettings.GlyphUp);
    end
    else
    begin
      Canvas.Pen.Color := clWhite;
      Canvas.Pen.Width := 1;
      Canvas.MoveTo(Lft - 4, vpos + 4);
      Canvas.LineTo(Lft + 4, vpos + 4);
      Canvas.LineTo(Lft, vpos - 4);
      Canvas.Pen.Color := clGray;
      Canvas.LineTo(Lft - 4, vpos + 4);
      Canvas.Pen.Color := clBlack;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutLookHeader.OwnOnDragDrop(Sender: TObject; FromSection,
  ToSection: Integer);
begin
  FOutLookList.MoveHeaderSec(FromSection, ToSection);
 { FromSection := FOutLookList.MapHeaderSecToCol(FromSection);
  ToSection := FOutLookList.MapHeaderSecToCol(ToSection);
  FOutLookList.Columns.Items[FromSection].Index := ToSection;
  FOutLookList.HeaderChanged(FOutLookList);
  if Assigned(FOutLookList.FOnHeaderDragDrop) then
    FOutLookList.FOnHeaderDragDrop(FOutLookList, FromSection, ToSection);
  }
end;

//------------------------------------------------------------------------------

procedure TAdvOutLookHeader.CMHintShow(var Message: TMessage);
var
  PHI: PHintInfo;
  Col : Integer;
  hint: string;
begin
  PHI := TCMHintShow(Message).HintInfo;
  if (FMouseOverSec >= 0) and Assigned(FOutLookList) then
  begin
    Col := FOutLookList.MapHeaderSecToCol(FMouseOverSec);

    if (FOutLookList.Columns.Items[Col].HeaderShowHint) and (FOutLookList.Columns.Items[Col].HeaderHint <> '') then
      hint := FOutLookList.Columns.Items[Col].HeaderHint
    else
      hint := '';

    if Assigned(FOutLookList.OnHeaderHint) then
      FOutlookList.OnHeaderHint(FOutlookList, Col, hint);

    PHI^.HintStr := hint;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutLookHeader.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  r: TRect;
  Sec, Col: Integer;
  pt: TPoint;
begin
  inherited;
  Sec := XYToSection(X, Y);
  r := GetSectionRect(Sec);

  if FSectionDragDrop and not FDragging and (FDragStart >= FOffset) and (ssLeft in Shift) and not FReSizing
     and ((abs(FMouseDragPos.X - X) > 2) or ((abs(FMouseDragPos.Y - Y) > 2))) then
  begin
    FDragging := True;
    Screen.Cursor := crDrag;
    SetCapture(Self.Handle);
    Exit;
  end;

  if (Sec >= 0) and (Sec < FOutLookList.Columns.Count) and
    (Abs(X - r.Left) > 4) and (Abs(X - r.Right) > 4) then
  begin
    if (FMouseOverSec <> Sec) and Assigned(FOutLookList) then
    begin
      FMouseOverSec := Sec;
      Application.CancelHint;
      Col := FOutLookList.MapHeaderSecToCol(Sec);
      GetCursorPos(Pt);

      if (FOutLookList.Columns.Items[Col].HeaderShowHint) and (FOutLookList.Columns.Items[Col].HeaderHint <> '') then
      begin
        //Hint := FOutLookList.Columns.Items[Col].HeaderHint;
        Application.ActivateHint(Pt);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOutLookHeader.OwnSizing(Sender: TObject; ASection,
  AWidth: Integer);
begin
  FReSizing := True;
end;

//------------------------------------------------------------------------------

procedure TAdvOutLookHeader.OwnSized(Sender: TObject; ASection,
  AWidth: Integer);
begin
  FReSizing := False;
  if Assigned(FOnSized) then
    FOnSized(self, ASection, AWidth);
end;

{ TProgressAppearance }

procedure TProgressAppearance.Assign(Source: TPersistent);
begin
  if Source is TProgressAppearance then
  begin
    FUnCompleteFontColor := TProgressAppearance(Source).UnCompleteFontColor;
    FCompleteColor := TProgressAppearance(Source).CompleteColor;
    FUnCompleteColor := TProgressAppearance(Source).UnCompleteColor;
    FCompleteFontColor := TProgressAppearance(Source).CompleteFontColor;
    FStacked := TProgressAppearance(Source).Stacked;
    FShowPercentage := TProgressAppearance(Source).ShowPercentage;
    FShowBorder := TProgressAppearance(Source).ShowBorder;
    FCompletionSmooth := TProgressAppearance(Source).CompletionSmooth;
    FShowGradient := TProgressAppearance(Source).ShowGradient;
    FLevel2Perc := TProgressAppearance(Source).Level2Perc;
    FLevel1Perc := TProgressAppearance(Source).Level1Perc;
    FSteps := TProgressAppearance(Source).Steps;
    FLevel3Color := TProgressAppearance(Source).Level3Color;
    FLevel1Color := TProgressAppearance(Source).Level1Color;
    FLevel0Color := TProgressAppearance(Source).Level0Color;
    FLevel3ColorTo := TProgressAppearance(Source).Level3ColorTo;
    FLevel2ColorTo := TProgressAppearance(Source).Level2ColorTo;
    FLevel0ColorTo := TProgressAppearance(Source).Level0ColorTo;
    FLevel1ColorTo := TProgressAppearance(Source).Level1ColorTo;
    FBorderColor := TProgressAppearance(Source).BorderColor;
    FLevel2Color := TProgressAppearance(Source).Level2Color;
    FStyle := TProgressAppearance(Source).Style;
    Changed;
    inherited Assign(Source);
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TProgressAppearance.Create;
begin
  inherited;

  FCompleteColor := clRed;
  FCompleteFontColor := clBlue;
  FUnCompleteColor := clNone;
  FUnCompleteFontColor := clWindowText;

  FLevel0Color := clLime;
  FLevel0ColorTo := $00E1FFE1;
  FLevel1Color := clYellow;
  FLevel1ColorTo := $00CAFFFF;
  FLevel2Color := $0053A9FF;
  FLevel2ColorTo := $00A8D3FF;
  FLevel3Color := clRed;
  FLevel3ColorTo := $00CACAFF;

  FLevel1Perc := 70;
  FLevel2Perc := 90;

  FBorderColor := clBlack;
  FShowBorder := false;
  FStacked := false;
  FShowPercentage := true;
  FCompletionSmooth := true;
  FShowGradient := true;
  FSteps := 11;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetCompleteColor(const Value: TColor);
begin
  if FCompleteColor <> Value then
  begin
    FCompleteColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetCompleteFontColor(const Value: TColor);
begin
  if FCompleteFontColor <> Value then
  begin
    FCompleteFontColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetCompletionSmooth(const Value: Boolean);
begin
  if FCompletionSmooth <> Value then
  begin
    FCompletionSmooth := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetLevel0Color(const Value: TColor);
begin
  if FLevel0Color <> Value then
  begin
    FLevel0Color := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetLevel0ColorTo(const Value: TColor);
begin
  if FLevel0ColorTo <> Value then
  begin
    FLevel0ColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetLevel1Color(const Value: TColor);
begin
  if FLevel1Color <> Value then
  begin
    FLevel1Color := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetLevel1ColorTo(const Value: TColor);
begin
  if FLevel1ColorTo <> Value then
  begin
    FLevel1ColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetLevel1Perc(const Value: Integer);
begin
  if FLevel1Perc <> Value then
  begin
    FLevel1Perc := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetLevel2Color(const Value: TColor);
begin
  if FLevel2Color <> Value then
  begin
    FLevel2Color := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetLevel2ColorTo(const Value: TColor);
begin
  if FLevel2ColorTo <> Value then
  begin
    FLevel2ColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetLevel2Perc(const Value: Integer);
begin
  if FLevel2Perc <> Value then
  begin
    FLevel2Perc := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetLevel3Color(const Value: TColor);
begin
  if FLevel3Color <> Value then
  begin
    FLevel3Color := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetLevel3ColorTo(const Value: TColor);
begin
  if FLevel3ColorTo <> Value then
  begin
    FLevel3ColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetShowBorder(const Value: Boolean);
begin
  if FShowBorder <> Value then
  begin
    FShowBorder := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetShowGradient(const Value: Boolean);
begin
  if FShowGradient <> Value then
  begin
    FShowGradient := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetShowPercentage(const Value: Boolean);
begin
  if FShowPercentage <> Value then
  begin
    FShowPercentage := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetStacked(const Value: Boolean);
begin
  if FStacked <> Value then
  begin
    FStacked := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetSteps(const Value: Integer);
begin
  if FSteps <> Value then
  begin
    FSteps := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetStyle(const Value: TProgressStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;  
end;

procedure TProgressAppearance.SetUnCompleteColor(const Value: TColor);
begin
  if FUnCompleteColor <> Value then
  begin
    FUnCompleteColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetUnCompleteFontColor(const Value: TColor);
begin
  if FUnCompleteFontColor <> Value then
  begin
    FUnCompleteFontColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TPreviewSetting }

procedure TPreviewSetting.Assign(Source: TPersistent);
begin
  if Source is TPreviewSetting then
  begin
    FColumn := TPreviewSetting(Source).Column;
    FFont.Assign(TPreviewSetting(Source).Font);
    Active := TPreviewSetting(Source).Active;
    inherited Assign(Source);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewSetting.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

//------------------------------------------------------------------------------

constructor TPreviewSetting.Create;
begin
  inherited;
  FFont := TFont.Create;
  FFont.OnChange := OnFontChange;
  FFont.Color := clBlue;
  FActive := False;
  FColumn := -1;
  //FTextColor := clBlue;
  FHeight := 40;
end;

//------------------------------------------------------------------------------

destructor TPreviewSetting.Destroy;
begin
  FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TPreviewSetting.OnFontChange(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

procedure TPreviewSetting.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewSetting.SetColumn(const Value: Integer);
begin
  if FColumn <> Value then
  begin
    FColumn := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewSetting.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TPreviewSetting.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TSortSettings }

constructor TSortSettings.Create;
begin
  inherited;
  FColumn := -1;
  FGlyphDown := TBitMap.Create;
  FGlyphDown.OnChange := OnGlyphChange;
  FGlyphUp := TBitMap.Create;
  FGlyphUp.OnChange := OnGlyphChange;
  FDirection := sdAscending;
  FEnabled := true;
end;

//------------------------------------------------------------------------------

destructor TSortSettings.Destroy;
begin
  FGlyphDown.Free;
  FGlyphUp.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TSortSettings.Assign(Source: TPersistent);
begin
  if Source is TSortSettings then
  begin
    Column := TSortSettings(Source).Column;
    Direction := TSortSettings(Source).Direction;
    GlyphDown.Assign(TSortSettings(Source).GlyphDown);
    GlyphUp.Assign(TSortSettings(Source).GlyphUp);
    inherited Assign(Source);
  end;
end;

//------------------------------------------------------------------------------

procedure TSortSettings.Change;
begin
  if Enabled then
    if Assigned(FOnChange) then
      FOnChange(self);
end;

//------------------------------------------------------------------------------

procedure TSortSettings.SetColumn(const Value: Integer);
begin
  if FColumn <> Value then
  begin
    FColumn := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TSortSettings.SetDirection(const Value: TSortDirection);
begin
  if FDirection <> Value then
  begin
    FDirection := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TSortSettings.SetGlyphDown(const Value: TBitMap);
begin
  FGlyphDown.Assign(Value);
  Change;
end;

//------------------------------------------------------------------------------

procedure TSortSettings.SetGlyphUp(const Value: TBitMap);
begin
  FGlyphUp.Assign(Value);
  Change;
end;

//------------------------------------------------------------------------------

procedure TSortSettings.OnGlyphChange(Sender: TObject);
begin
  Change;
end;

//------------------------------------------------------------------------------

procedure TSortSettings.SetSortGroups(const Value: Boolean);
begin
  FSortGroups := Value;
  Change;
end;

//------------------------------------------------------------------------------

{ TOutlookGroup }

function TOutlookGroup.AddChild: TStrings;
var
  poi: POutlookInfo;
  ogl: POGLItem;
begin
  ogl := List.AddItem(OGLItem);
  poi := POutlookInfo(@ogl.Data);
  if not Assigned(poi.data) then
    poi.data := TStringList.Create;

  Result := poi.data;
end;

//------------------------------------------------------------------------------

function TOutlookGroup.InsertChild(Index: Integer): TStrings;
var
  poi: POutlookInfo;
  ogl: POGLItem;
begin
  if ChildCount = 0 then
    Result := AddChild
  else
  begin
    ogl := List.FList.InsertItem(OGLItem,Index);
    poi := POutlookInfo(@ogl.Data);
    if not Assigned(poi.data) then
      poi.data := TStringList.Create;
    Result := poi.data;
  end;
end;

function TOutlookGroup.ItemIndex(p: POGLItem): integer;
var
  i: integer;
  ogl : POGLItem;
begin
  Result := -1;
  
  ogl := OGLItem.FirstChild;

  i := -1;

  repeat
    if (ogl = p) then
    begin
      Result := i + 1;
      Exit;
    end;

    ogl := ogl^.NextSibling;
    inc(i);
  until (ogl = nil);
end;

//------------------------------------------------------------------------------

function TOutlookGroup.GetCaption: string;
var
  poi: POutlookInfo;
begin
  poi := POutlookInfo(@OGLItem.Data);
  if TStringList(poi.data).Count  > 0 then
    Result := TStringList(poi.Data).Strings[0];
end;

//------------------------------------------------------------------------------

function TOutlookGroup.GetChildOLGItem(Index: Integer): POGLItem;
var
  i: integer;
  ogl : POGLItem;
begin
  if (Index < 0) or (Word(Index) >= OGLItem.ChildCount) then
    raise Exception.Create('Index out of bounds');

  ogl := OGLItem.FirstChild;

  i := 0;

  repeat
    if (i = Index) then
    begin
      Result := ogl;
      Exit;
    end;

    ogl := ogl^.NextSibling;
    inc(i);

  until (ogl = nil);

  raise Exception.Create('Index out of bounds');
end;

//------------------------------------------------------------------------------

function TOutlookGroup.GetChildItem(Index: Integer): TStrings;
var
  i: integer;
  ogl : POGLItem;
  poi: POutlookInfo;
begin
  if (Index < 0) or (Word(Index) >= OGLItem.ChildCount) then
    raise Exception.Create('Index out of bounds');

  ogl := OGLItem.FirstChild;

  i := 0;

  repeat
    if (i = Index) then
    begin
      poi := POutlookInfo(@ogl.Data);
      Result := TStringList(poi.data);
      Exit;
    end;

    ogl := ogl^.NextSibling;
    inc(i);

  until (ogl = nil);

  raise Exception.Create('Index out of bounds');
end;

//------------------------------------------------------------------------------

procedure TOutlookGroup.RemoveChild(Index: Integer);
var
  i: integer;
  ogl : POGLItem;
begin
  if (Index < 0) or (Word(Index) >= OGLItem.ChildCount) then
    raise Exception.Create('Index out of bounds');

  ogl := OGLItem.FirstChild;

  i := 0;

  repeat
    if (i = Index) then
    begin
      List.FList.DeleteItem(ogl);
      Exit;
    end;

    ogl := ogl^.NextSibling;
    inc(i);

  until (ogl = nil);

  raise Exception.Create('Index out of bounds');

end;

//------------------------------------------------------------------------------

procedure TOutlookGroup.ClearChilds;
var
  ogl, ogln: POGLItem;
begin
  ogl := OGLItem.FirstChild;
  ogln := nil;

  if not Assigned(ogl) then
    Exit;

  repeat
    if Assigned(ogl) then
    begin
      ogln := ogl^.NextSibling;
      List.FList.DeleteItem(ogl);
    end;

    ogl := ogln;
  until (ogl = nil);

end;

//------------------------------------------------------------------------------

procedure TOutlookGroup.SetCaption(const Value: string);
var
  poi: POutlookInfo;
begin
  poi := POutlookInfo(@OGLItem.Data);
  TStringList(poi.data).Clear;
  TStringList(poi.data).Add(Value);
end;

//------------------------------------------------------------------------------

function TOutlookGroup.GetChildCount: integer;
begin
  Result := OGLItem.ChildCount;
end;

//------------------------------------------------------------------------------

function TOutlookGroup.GetExpanded: boolean;
begin
  Result := nsExpanded in oglItem.States;
end;

//------------------------------------------------------------------------------

function TOutlookGroup.GetObject: TObject;
begin
  Result := oglItem.GroupObject;
end;

//------------------------------------------------------------------------------

function TOutlookGroup.GetTag: Integer;
begin
  Result := oglItem.Tag;
end;

//------------------------------------------------------------------------------

procedure TOutlookGroup.SetExpanded(const Value: boolean);
begin
  {if Value then
    oglItem.States := oglItem.States + [nsExpanded]
  else
    oglItem.States := oglItem.States - [nsExpanded];}
  if Assigned(List) and Assigned(List.FList) then
  begin
    if Value then
      List.FList.ExpandItem(OGLItem)
    else
      List.FList.CollapseItem(OGLItem);
    List.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TOutlookGroup.SetObject(const Value: TObject);
begin
  oglItem.GroupObject := Value;
end;

//------------------------------------------------------------------------------

procedure TOutlookGroup.SetTag(const Value: Integer);
begin
  oglItem.Tag := Value;
end;

//------------------------------------------------------------------------------

{ TLookUpSettings }

constructor TLookUpSettings.Create;
begin
  inherited;
  FColumn := -1;
  FMethod := lmDirect;
end;

//------------------------------------------------------------------------------

destructor TLookUpSettings.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

procedure TLookUpSettings.Assign(Source: TPersistent);
begin
  if (Source is TLookUpSettings) then
  begin
    Column := (Source as TLookUpSettings).Column;
    Method := (Source as TLookUpSettings).Method;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TLookUpSettings.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TLookUpSettings.SetColumn(const Value: Integer);
begin
  if (FColumn <> Value) then
  begin
    FColumn := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TLookUpSettings.SetMethod(const Value: TLookUpMethod);
begin
  if (FMethod <> Value) then
  begin
    FMethod := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------



initialization
  CF_FILEGROUPDESCRIPTOR :=
    RegisterClipboardFormat(CFSTR_FILEDESCRIPTOR);
  CF_FILECONTENTS :=
    RegisterClipboardFormat(CFSTR_FILECONTENTS);

end.
