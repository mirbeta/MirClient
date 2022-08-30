{*************************************************************************}
{ TSECTIONLISTBOX component                                               }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ Copyright © 1998-2014                                                   }
{   TMS Software                                                          }
{   Email : info@tmssoftware.com                                          }
{   Web : http://www.tmssoftware.com                                      }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}
unit slstbox;

{$I TMSDEFS.INC}
{$DEFINE REMOVEDRAW}
{$DEFINE REMOVESTRIP}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, ShellApi, RichEdit, Inifiles, PictureContainer, ImgList
  , Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 9; // Minor version nr.
  REL_VER = 3; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.9.0.1 : Fixed issue with SubItems.Move()
  // 1.9.0.2 : Fixed issue with OnExpandSection, OnContractSection
  // 1.9.0.3 : Improved : code improved for AV during clear of empty sectionlistbox
  // 1.9.1.0 : OnSubitemClick, OnSectionClick triggered from keyboard actions as well
  // 1.9.1.1 : Fixed issue with DeleteSelected
  // 1.9.1.2 : Fixed issue with use on frames
  // 1.9.1.3 : Fixed issue with down key on last section without subitems
  // 1.9.1.4 : Fixed checkbox area click detection
  // 1.9.2.0 : Improved : Speed to save & load large nr. of items
  // 1.9.2.1 : Fixed : Issue with section height when doing reparenting
  // 1.9.2.2 : Fixed : Issue with FullFocus repainting when changed at runtime
  // 1.9.2.3 : Fixed : Issue with SaveToFile function
  // 1.9.2.4 : Fixed : Issue with ContractDisable & arrow keys
  // 1.9.2.5 : Improved : Wordwrapped section drawing
  // 1.9.2.6 : Fixed : Index out of bounds condition when inserting subitems
  // 1.9.2.7 : Fixed : Issue with inserting items on expanded section
  // 1.9.2.8 : Fixed : Issue with spurious horz. scrollbar
  // 1.9.2.9 : Fixed : Issue with inserting items in expanded sections
  // 1.9.3.0 : New : OnSectionHeight event added to customize height of section captions per section

type
  TOwnerDrawState = Windows.TOwnerDrawState;
{$NODEFINE TOwnerDrawState}

  ESectionListBoxError = class(Exception);

  TSectionListBox = class;
  TListSection = class;

 {---------------------------------------------------}
 { type definitions for TSectionListbox              }
 {---------------------------------------------------}
  TListNodeType = (lnFlat, ln3D, lnGlyph);

  TSListScrollStyle = (slsNormal, slsFlat, slsEncarta);

  TListSectionState = (lssExpanded, lssContracted);

  TActiveSection = (asFull, asNodeOnly);

  TSectionFocus = (sfDash, sf3D, sfNone);

  TSectionLines = (slNone, slVertical, slHorizontal, slBoth);

  TSectionControlType = (scText, scCheckbox, scRadiobutton);

  TSortDirection = (sdNone, sdAscending, sdDescending);

  TFontUsage = (fuSubItems, fuSection, fuBoth);

  TVAlignment = (vtaCenter, vtaTop, vtaBottom);

  TExpandSectionEvent = procedure(Sender: TObject; SectionIdx: Integer) of object;
  TContractSectionEvent = procedure(Sender: TObject; SectionIdx: Integer) of object;

  TSubItemClickEvent = procedure(Sender: TObject; SectionIdx, subitemidx: Integer) of object;

  TInsertSubItemEvent = procedure(Sender: TObject; SectionIdx, subitemidx: Integer; var subitem: string) of object;
  TInsertSectionEvent = procedure(Sender: TObject; SectionIdx: Integer; section: tlistsection) of object;

  TDeleteSubItemEvent = procedure(Sender: TObject; SectionIdx, subitemidx: Integer; var allow: Boolean) of object;
  TDeleteSectionEvent = procedure(Sender: TObject; SectionIdx: Integer; var allow: Boolean) of object;

  TDrawItemEvent = procedure(Sender: TObject; section: TListSection; SectionIdx, subitemidx: Integer; Canvas: TCanvas; arect: trect; astate: TOwnerDrawState) of object;

  TDrawItemPropEvent = procedure(Sender: TObject; section: TListSection; SectionIdx, subitemidx: Integer; Canvas: TCanvas) of object;

  TEditEvent = procedure(Sender: TObject; SectionIdx, subitemidx: Integer; var s: string) of object;

  TAnchorClick = procedure(Sender: TObject; index: Integer; anchor: string) of object;

  TAnchorEvent = procedure(Sender: TObject; index: Integer; anchor: string) of object;

  TSectionHeightEvent = procedure(Sender: TObject; SectionIndex: Integer; var Height: integer) of object;

 {---------------------------------------------------}
 { Stringlist to hold items of a section             }
 {---------------------------------------------------}
  TSectionListStrings = class(TStringList)
  private
    FListSection: TListSection;
  public
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure InsertObject(Index: Integer; const S: string; AObject: TObject); override;
    property OnChange;
  end;

 {---------------------------------------------------------}
 { Listbox section class, item of a collection of Sections }
 {---------------------------------------------------------}
  TListSection = class(TCollectionItem)
  private
    FAlignment: TAlignment;
    FVAlignment: TVAlignment;
//   fHeaderAlignment:TAlignment;
    FAutoEdit: Boolean;
    FAutoSize: Boolean;
    FCaption: string;
    FFixed: Boolean;
    FSubItems: TStringList;
    FState: TListSectionState;
    FColor: TColor;
    FImageIndex: Integer;
    FLines: TSectionLines;
    FFont: TFont;
    FFontUsage: TFontUsage;
    FEndEllipsis: Boolean;
    FControlType: TSectionControltype;
    FRadioIndex: Integer;
    FSortDirection: TSortDirection;
    FSortShow: Boolean;
    FTag: Integer;
    FOwnerdraw: Boolean;
    FReadOnly: Boolean;
    FItemHeight: Integer;
    procedure SetAlignment(const value: TAlignment);
    procedure SetVAlignment(const value: TVAlignment);
    procedure SetAutoSize(const value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetColor(const Value: TColor);
    procedure SetFont(value: TFont);
    procedure SetImageIndex(const Value: Integer);
    procedure SetLines(const Value: TSectionLines);
    procedure SetFixed(const value: Boolean);
    procedure SetState(const value: TListSectionState);
    procedure SetSubItems(const Value: TStringList);
    procedure AddSubItems(const S: string);
    procedure ClearSubItems;
    procedure DeleteSubItems(Index: Integer);
    procedure InsertSubItems(Index: Integer; const S: string);
    function GetSubItemImageIdx(i: Integer): smallint;
    procedure SetSubItemImageIdx(i: Integer; const Value: smallint);
    function GetSubItemCheckState(i: Integer): boolean;
    procedure SetSubItemCheckState(i: Integer; const Value: boolean);
    {
    procedure SetSubItemCheckStateEx(i: integer; const Value: boolean);
    }
    procedure FontChanged(Sender: TObject);
    procedure SubItemsChanged(Sender: TObject);
    procedure SetFontUsage(value: TFontUsage);
    procedure SetEndEllipsis(value: Boolean);
    procedure SetRadioIndex(value: Integer);
    procedure SetControlType(value: TSectionControlType);
    procedure SetSortDirection(value: TSortDirection);
    procedure SetSortShow(value: Boolean);
    procedure SetOwnerDraw(value: Boolean);
    procedure SetReadOnly(value: Boolean);
    procedure SetItemHeight(value: Integer);
    procedure QuickSortItems(left, right: Integer);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(source: TPersistent); override;
    property SubItemImageIdx[i: Integer]: smallint read GetSubItemImageIdx
    write SetSubItemImageIdx;
    property SubItemCheckState[i: Integer]: Boolean read GetSubItemCheckState
    write SetSubItemCheckState;
    procedure SortSubItems;
    procedure Focus;
    function ListBox: TSectionListBox;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property VAlignment: TVAlignment read FVAlignment write SetVAlignment;
    property AutoEdit: Boolean read FAutoEdit write fAutoEdit;
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property Caption: string read FCaption write SetCaption;
    property Fixed: Boolean read FFixed write SetFixed;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property State: TListSectionState read FState write SetState;
    property SubItems: TStringList read FSubItems write SetSubItems;
    property Color: TColor read FColor write SetColor;
    property Lines: TSectionLines read FLines write SetLines;
    property Font: TFont read fFont write SetFont;
    property FontUsage: TFontUsage read FFontUsage write SetFontUsage;
    property EndEllipsis: Boolean read FEndEllipsis write SetEndEllipsis;
    property ControlType: TSectionControlType read fControlType write SetControlType;
    property RadioIndex: Integer read FRadioIndex write SetRadioIndex;
    property SortDirection: TSortDirection read fSortDirection write SetSortDirection;
    property SortShow: Boolean read FSortShow write SetSortShow;
    property Tag: Integer read FTag write FTag;
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property ItemHeight: Integer read FItemHeight write SetItemHeight;
  end;

  {--------------------------------}
  { Listbox section collection     }
  {--------------------------------}
  TListSectionCollection = class(TCollection)
  private
    FOwner: TSectionListBox;
    function GetItem(Index: Integer): TListSection;
    procedure SetItem(Index: Integer; Value: TListSection);
  public
    function CreateItemClass: TCollectionItemClass; virtual;
    constructor Create(AOwner: TSectionListBox);
    function Add: TListSection;
    function Insert(index: Integer): TListSection;
    procedure SwapSections(idx1, idx2: Integer);
    function IndexOf(SectionText: string): Integer;
    property Items[Index: Integer]: TListSection read GetItem write SetItem; default;
    property ListOwner: TSectionListBox read FOwner;
    function InsertSection(ASection: TListSection): Integer;
  protected
    procedure Update(Item: TCollectionItem); override;
    function GetOwner: TPersistent; override;
    function GetListIndex(ASection: TListSection): Integer;
    function ExpandSection(ASection: TListSection): Integer;
    function ContractSection(ASection: TListSection): Integer;
    function RemoveSection(ASection: TListSection): Integer;
    function UpdateSection(ASection: TListSection): Integer;
    procedure InsertInSection(ASection: TListSection; index: Integer; const S: string);
    procedure AddToSection(ASection: TListSection; const S: string);
    procedure DeleteFromSection(ASection: TListSection; index: Integer);
    procedure ClearSection(ASection: TListSection);
  end;

  {--------------------------------}
  { Collection of tab positions    }
  {--------------------------------}
  TTabType = (tableft, tabright);

  TTabPositionItem = class(TCollectionItem)
  private
    FTabPosition: Integer;
    FTabType: TTabType;
    procedure SetTabPosition(value: Integer);
    procedure SetTabType(value: tTabType);
  published
    property TabPosition: Integer read fTabPosition write SetTabPosition;
    property TabType: TTabType read fTabType write SetTabType;
  end;

  TTabPositionCollection = class(TCollection)
  private
    FOwner: TSectionListBox;
    function GetTabPos(i: Integer): TTabPositionItem;
    procedure SetTabPos(i: Integer; tabpos: TTabPositionItem);
  public
    constructor Create(aOwner: TSectionListBox);
    function Add: TTabPositionItem;
    property Items[Index: Integer]: TTabPositionItem read GetTabPos write SetTabPos;
  protected
    function GetOwner: TPersistent; override;
  end;

  {------------------------------------------------}
  { Persistent class to hold URL aware settings    }
  {------------------------------------------------}
  TURLSettings = class(TPersistent)
  private
    FURLAware: Boolean;
    FURLColor: TColor;
    FURLFull: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetURLAware(value: Boolean);
    procedure SetURLColor(value: tcolor);
    procedure SetURLFull(value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property URLAware: Boolean read fURLAware write SetURLAware;
    property URLColor: TColor read fURLColor write SetURLColor;
    property URLFull: Boolean read fURLFull write SetURLFull;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  {--------------------------------}
  {  TSectionMemo helper class     }
  {--------------------------------}
  TSectionMemo = class(TMemo)
  private
    procedure WMKeyDown(var Msg: TWMKeydown); message wm_keydown;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    SectionIdx: Integer;
    SubItemIdx: Integer;
    ItemIdx: Integer;
    FListSection: TListSection;
  published
  end;

  {--------------------------------}
  { Main TSectionListbox class     }
  {--------------------------------}
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TSectionListBox = class(TCustomListbox)
  private
    { Private declarations }
    FHasScrollBar: Boolean;
    FFlat: Boolean;
    FWordWrap: Boolean;
    FWallpaper: TBitmap;
    FImageSpacing: smallint;
    FDtStyle: Integer;
    FFullFocus: Boolean;
    FScrollTrack: Boolean;
    FSectionFont: TFont;
    FSectionIndent: Integer;
    FSections: TListSectionCollection;
    FImageCache: THTMLPictureCache;
    FContainer: TPictureContainer;
    FSectionHeight: Integer;
    FSubItemIndent: Integer;
    FSubItemHeight: Integer;
    FTabPositions: TTabPositionCollection;
    FOnExpandSection: TExpandSectionEvent;
    FOnContractSection: TContractSectionEvent;
    FOnSectionRClick: TContractSectionEvent;
    FOnSectionClick: TContractSectionEvent;
    FOnSectionDblClk: TContractSectionEvent;
    FOnChange: TSubItemClickEvent;
    FOnSubItemClick: TSubItemClickEvent;
    FOnCheckBoxClick: TSubItemClickEvent;
    FOnRadioClick: TSubItemClickEvent;
    FOnSubItemRClick: TSubItemClickEvent;
    FOnSubItemDblClk: TSubItemClickEvent;
    FOnSubItemRDblClk: TSubItemClickEvent;
    FOnInsertSubItem: TInsertSubItemEvent;
    FOnDeleteSubItem: TDeleteSubItemEvent;
    FOnInsertSection: TInsertSectionEvent;
    FOnDeleteSection: TDeleteSectionEvent;
    FOnDrawItem: TDrawItemEvent;
    FOnDrawItemProp: TDrawItemPropEvent;
    FOnStartEdit: TEditEvent;
    FOnEndEdit: TEditEvent;
    FAnchorClick: TAnchorClick;
    FAnchorEnter: TAnchorEvent;
    FAnchorExit: TAnchorEvent;
    FOldCursor: TCursor;
    FRichEdit: TRichEdit;
    FMemo: TSectionMemo;
    FSectionImages: TImageList;
    FSubItemImages: TImageList;
    FURLSettings: TURLSettings;
    FUpdateCount: Integer;
    FOneExpanded: Boolean;
    FExpandDisable: Boolean;
    FExpandGlyph: TBitmap;
    FContractDisable: Boolean;
    FContractGlyph: TBitmap;
    FSectionColor: TColor;
    FActiveSection: TActiveSection;
    FNodeType: TListNodeType;
    FSectionFocus: TSectionFocus;
    FScrollStyle: TSListScrollStyle;
    FScrollColor: TColor;
    FScrollWidth: Integer;
    FSelectionColor: TColor;
    FSelectionTextColor: TColor;
    FTabSelect: Boolean;
    FTabMove: Boolean;
    FTabPosMove: Boolean;
    FTabPos: Integer;
    FOldTabPos: Integer;
    FOldAnchor: string;
    FOldScrollPos: integer;
    FOnSectionHeight: TSectionHeightEvent;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMVScroll(var WMScroll: TWMScroll); message WM_VSCROLL;
    procedure WMHScroll(var WMScroll: TWMScroll); message WM_HSCROLL;
    procedure WMEraseBkGnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMLButtonDblClk(var Message: TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure WMRButtonDBlClk(var Message: TWMRButtonDown); message WM_RBUTTONDBLCLK;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonDown); message WM_LBUTTONUP;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMDestroy(var Message: TMessage); message WM_DESTROY;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure SetSections(value: TListSectionCollection);
    procedure SetTabPositions(value: TTabPositionCollection);
    procedure SetTabPosMove(value: Boolean);
    procedure SetSectionHeight(value: Integer);
    function GetSectionHeight: integer;
    procedure SetSectionFont(value: TFont);
    procedure SetWordWrap(const value: Boolean);
    procedure SetOneExpanded(const value: Boolean);
    procedure SetURLSettings(value: TURLSettings);
    procedure ToggleSectionState(idx: Integer);
    procedure FontChanged(Sender: TObject);
    procedure SetFlat(const value: Boolean);
    procedure SetWallpaper(Value: TBitmap);
    procedure WallPaperChanged;
    procedure SetImageSpacing(const value: smallint);
    procedure SetSectionIndent(const Value: integer);
    procedure SetSubItemIndent(const Value: integer);
    procedure SetSubItemHeight(const Value: integer);
    procedure SetSectionImages(const Value: TImageList);
    procedure SetSubItemImages(const Value: TImageList);
    procedure SetSectionColor(const Value: TColor);
    function LinesInText(s: string; multiline: Boolean; height: Integer): Integer;
    procedure SetContractGlyph(const Value: TBitmap);
    procedure SetExpandGlyph(const Value: TBitmap);
    procedure SetNodeType(const Value: TListNodeType);

    procedure UpdateStyle;
    procedure UpdateVScrollBar;
    procedure UpdateHScrollBar;
    procedure UpdateColor;
    procedure UpdateWidth;

    procedure SetScrollStyle(const Value: TSListScrollStyle);
    procedure SetScrollColor(const Value: TColor);
    procedure SetScrollWidth(const Value: integer);

    procedure FlatSetScrollPos(code, pos: Integer; fRedraw: bool);
    procedure FlatSetScrollProp(index, newValue: Integer; fRedraw: bool);
    procedure FlatSetScrollInfo(code: Integer; var scrollinfo: tscrollinfo; fRedraw: bool);
    procedure FlatShowScrollBar(code: Integer; show: bool);

    procedure QuickSortSections(left, right: Integer);
    procedure SetContainer(const Value: TPictureContainer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetFullFocus(const Value: Boolean);
  protected
    { Protected declarations }
    FDestroyed: Boolean;
    FExpandContractBusy: Boolean;
    property Items;
    function GetVersionNr: Integer; virtual;
    procedure UpdateHeight(Index: Integer);
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyPress(var ch: char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
    procedure UpdateHorzScrollbar;
    procedure DoPaint;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function CreateSections: TListSectionCollection; virtual;
    function GetDisplText(ListSection: TListSection; Index: Integer): string; virtual;
    function GetImageCache: THTMLPictureCache;
    function GetCustomSectionHeight(SectionIndex: integer): integer; virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetActiveSection: TListSection;
    function GetItemSection(idx: Integer): TListSection;
    function GetItemSectionIndex(idx: Integer): Integer;
    function GetItemAtXY(x, y: Integer; var SectionIdx, subitemidx: Integer): Boolean;
    function GetSectionListIndex(idx: Integer): Integer;
    function GetSectionSubItem(SectionIdx, subitemidx: Integer): string;
    function GetRichSectionSubItem(SectionIdx, subitemidx: Integer): string;
    function GetListItemIndex(listindex: Integer; var SectionIdx, subitemidx: Integer): Boolean;
    function GetSelection(var SectionIdx, subitemidx: Integer; var selstring: string): Boolean;
    function GetSelectedItem(var SectionIdx, subitemidx: Integer): Boolean;
    function SetSelection(SectionIdx, subitemidx: Integer): Boolean;
    function IsSection(idx: Integer): Boolean;
    procedure UpdateItemHeight;
    procedure FocusSection(section: TListSection);
    procedure ExpandAll;
    procedure ContractAll;
    procedure SortAll;
    procedure SortAllSubItems;
    procedure SortAllSections;
    {$IFDEF DELPHI6_LVL}
    procedure DeleteSelected; override;
    {$ENDIF}
    {$IFNDEF DELPHI6_LVL}
    procedure DeleteSelected;
    {$ENDIF}
    procedure SaveToFile(filename: string);
    procedure LoadFromFile(filename: string);
    procedure SaveToInifile(filename: string);
    procedure LoadFromInifile(filename: string);
    procedure OptimizeTabs(padding: Integer);
    procedure Clear; {$IFDEF DELPHI6_LVL} override; {$ENDIF}
    procedure BeginUpdate;
    procedure EndUpdate;
    function StartEdit(idx: Integer): Boolean;
    procedure StringToRich(s: string; richeditor: TRichEdit);
    function RichToString(richeditor: TRichEdit): string;
    procedure RichPaint(Canvas: TCanvas; ARect: TRect; s: string; sel: Boolean);
    property RichEdit: TRichEdit read FRichEdit;
    function HasScrollBar: Boolean;
    procedure HilightInList(HiText: string; DoCase: Boolean);
    procedure UnHilightInList;
    procedure HiLightInSection(SectionIdx: Integer; HiText: string; DoCase: Boolean);
    procedure UnHiLightInSection(SectionIdx: Integer);
    procedure HilightInSubItem(SectionIdx, SubItemIdx: Integer; HiText: string; DoCase: Boolean);
    procedure UnHilightInSubItem(SectionIdx, SubItemIdx: Integer);
    procedure MarkInList(HiText: string; DoCase: Boolean);
    procedure UnMarkInList;
    procedure MarkInSection(SectionIdx: Integer; HiText: string; DoCase: Boolean);
    procedure UnMarkInSection(SectionIdx: Integer);
    procedure MarkInSubItem(SectionIdx, SubItemIdx: Integer; HiText: string; DoCase: Boolean);
    procedure UnMarkInSubItem(SectionIdx, SubItemIdx: Integer);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
   { Published declarations }
    property Sections: tListSectionCollection read fSections write SetSections;
    property TabPositions: tTabPositionCollection read fTabPositions write SetTabPositions;
    property TabPosMove: Boolean read fTabPosMove write SetTabPosMove;
    property Align;
    property WordWrap: Boolean read fWordWrap write SetWordWrap;
    property Wallpaper: TBitmap read fWallPaper write SetWallpaper;
    property OneExpanded: Boolean read fOneExpanded write SetOneExpanded;
    property FullFocus: Boolean read FFullFocus write SetFullFocus;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ImageSpacing: smallint read FImageSpacing write SetImageSpacing;
    property MultiSelect;
    property SubItemHeight: Integer read FSubItemHeight write SetSubItemHeight;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PictureContainer: TPictureContainer read FContainer write SetContainer;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
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
    property OnChange: TSubItemClickEvent read FOnChange write FOnChange;
    property OnExpandSection: TExpandSectionEvent read FOnExpandSection write FOnExpandSection;
    property OnContractSection: TContractSectionEvent read FOnContractSection write FOnContractSection;
    property OnSectionClick: TContractSectionEvent read FOnSectionClick write FOnSectionClick;
    property OnSectionRClick: TContractSectionEvent read FOnSectionRClick write FOnSectionRClick;
    property OnSectionDblClk: TContractSectionEvent read FOnSectionDblClk write FOnSectionDblClk;
    property OnSectionHeight: TSectionHeightEvent read FOnSectionHeight write FOnSectionHeight;
    property OnSubItemClick: TSubItemClickEvent read FOnSubItemClick write FOnSubItemClick;
    property OnCheckBoxClick: TSubItemClickEvent read FOnCheckBoxClick write FOnCheckBoxClick;
    property OnRadioClick: TSubItemClickEvent read FOnRadioClick write FOnRadioClick;
    property OnSubItemRClick: TSubItemClickEvent read FOnSubItemRClick write FOnSubItemRClick;
    property OnSubItemDblClk: TSubItemClickEvent read FOnSubItemDblClk write FOnSubItemDblClk;
    property OnSubItemRDblClk: TSubItemClickEvent read FOnSubItemRDblClk write FOnSubItemRDblClk;
    property OnInsertSubItem: TInsertSubItemEvent read FOnInsertSubItem write FOnInsertSubItem;
    property OnDeleteSubItem: TDeleteSubItemEvent read FOnDeleteSubItem write FOnDeleteSubItem;
    property OnInsertSection: TInsertSectionEvent read FOnInsertSection write FOnInsertSection;
    property OnDeleteSection: TDeleteSectionEvent read FOnDeleteSection write FOnDeleteSection;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnDrawItemProp: TDrawItemPropEvent read FOnDrawItemProp write FOnDrawItemProp;
    property OnStartEdit: TEditEvent read FOnStartEdit write FOnStartEdit;
    property OnEndEdit: TEditEvent read FOnEndEdit write FOnEndEdit;
    property OnAnchorClick: TAnchorClick read FAnchorClick write FAnchorClick;
    property OnAnchorEnter: TAnchorEvent read FAnchorEnter write FAnchorEnter;
    property OnAnchorExit: TAnchorEvent read FAnchorExit write FAnchorExit;
    property Flat: Boolean read FFlat write SetFlat;
    property SectionHeight: Integer read GetSectionHeight write SetSectionHeight default 14;
    property SectionFont: TFont read FSectionFont write SetSectionFont;
    property SectionIndent: Integer read FSectionIndent write SetSectionIndent;
    property SectionImages: TImageList read FSectionImages write SetSectionImages;
    property SubItemImages: TImageList read FSubItemImages write SetSubItemImages;
    property SubItemIndent: Integer read FSubItemIndent write SetSubItemIndent;
    property URLSettings: TURLSettings read FURLSettings write SetURLSettings;
    property OnStartDrag;
    property ExpandDisable: Boolean read FExpandDisable write fExpandDisable;
    property ExpandGlyph: TBitmap read FExpandGlyph write SetExpandGlyph;
    property ContractDisable: Boolean read FContractDisable write fContractDisable;
    property ContractGlyph: TBitmap read FContractGlyph write SetContractGlyph;
    property SectionColor: TColor read FSectionColor write SetSectionColor;
    property ActiveSection: TActiveSection read FActiveSection write FActiveSection;
    property NodeType: TListNodeType read FNodeType write SetNodeType;
    property SectionFocus: TSectionFocus read FSectionFocus write FSectionFocus;
    property ScrollTrack: Boolean read FScrollTrack write FScrollTrack default True;
    property ScrollStyle: TSListScrollStyle read FScrollStyle write SetScrollStyle;
    property ScrollColor: TColor read FScrollColor write SetScrollColor;
    property ScrollWidth: Integer read FScrollWidth write SetScrollWidth;
    property SelectionColor: TColor read FSelectionColor write FSelectionColor;
    property SelectionTextColor: TColor read FSelectionTextColor write FSelectionTextColor;
    property Version: string read GetVersion write SetVersion;
  end;


implementation

uses
  CommCtrl;

{$DEFINE HILIGHT}

{$I HTMLENGO.PAS}

{--------------------------}
{ Constant definitions     }
{--------------------------}
const
  MAX_TABS = 20;
  LINEFEED = #13;
  Effect3DSize = 3;
  commctllib = 'comctl32.dll';

const
  WSB_PROP_CYVSCROLL = $0000001;
  WSB_PROP_CXHSCROLL = $0000002;
  WSB_PROP_CYHSCROLL = $0000004;
  WSB_PROP_CXVSCROLL = $0000008;
  WSB_PROP_CXHTHUMB = $0000010;
  WSB_PROP_CYVTHUMB = $0000020;
  WSB_PROP_VBKGCOLOR = $0000040;
  WSB_PROP_HBKGCOLOR = $0000080;
  WSB_PROP_VSTYLE = $0000100;
  WSB_PROP_HSTYLE = $0000200;
  WSB_PROP_WINSTYLE = $0000400;
  WSB_PROP_PALETTE = $0000800;
  WSB_PROP_MASK = $0000FFF;

  FSB_FLAT_MODE = 2;
  FSB_ENCARTA_MODE = 1;
  FSB_REGULAR_MODE = 0;


function Max(a, b: Integer): Integer;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

{--------------------------------------------}
{ Transparent bitmap draw helper function    }
{--------------------------------------------}

procedure DrawBitmapTransp(Canvas: tCanvas; bmp: tbitmap; bkcolor: tcolor; r: trect);
var
  tmpbmp: TBitmap;
  srcColor: TColor;
  tgtrect: TRect;
begin
  TmpBmp := TBitmap.Create;
  try
    TmpBmp.Height := bmp.Height;
    TmpBmp.Width := bmp.Width;
    tgtrect.Left := 0;
    tgtrect.Top := 0;
    tgtrect.Right := bmp.Width;
    tgtrect.Bottom := bmp.Height;
    r.Bottom := r.Top + bmp.Height;
    r.Right := r.Left + bmp.Width;
    TmpBmp.Canvas.Brush.Color := bkcolor;
    srcColor := bmp.Canvas.pixels[0, 0];
    TmpBmp.Canvas.BrushCopy(tgtrect, bmp, tgtrect, srcColor);
    Canvas.CopyRect(r, TmpBmp.Canvas, tgtrect);
  finally
    TmpBmp.Free;
  end;
end;

constructor TSectionListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := lbOwnerDrawVariable;

  FSections := CreateSections;

 // FSections := TListSectionCollection.Create(Self);
  FTabPositions := TTabPositionCollection.Create(Self);
  FRichEdit := TRichEdit.Create(Self);
  FMemo := TSectionMemo.Create(Self);
  FWallpaper := TBitmap.Create;
  FHasScrollBar := False;
  FDestroyed := True;
  FSectionHeight := 14;
  FSectionIndent := 14;
  FSubItemIndent := 14;
  FSubItemHeight := 13;
  FScrollTrack := True;
  FFullFocus := True;
  FSectionColor := clBtnFace;
  FDtStyle := 0;
  FExpandContractBusy := False;
  FSelectionColor := clHighlight;
  FSelectionTextColor := clHighLightText;
  FImageCache := THTMLPictureCache.Create;
  FContainer := TPictureContainer.Create(Self);
  FSectionFont := TFont.Create;
  FSectionFont.OnChange := FontChanged;
  FURLSettings := TURLSettings.Create;
  FURLSettings.OnChange := FontChanged;

  FContractGlyph := TBitmap.Create;
  FExpandGlyph := TBitmap.Create;

  FScrollColor := clNone;
  FScrollWidth := GetSystemMetrics(SM_CXVSCROLL);
  FScrollStyle := slsNormal;

  FOldAnchor := '';
  FContainer := nil;
end;

function TSectionListBox.CreateSections: TListSectionCollection;
begin
  Result := TListSectionCollection.Create(Self);

end;

function TSectionListBox.GetDisplText(ListSection: TListSection;
  Index: Integer): string;
begin
  Result := Items[Index];
end;


function TSectionListBox.GetImageCache: THTMLPictureCache;
begin
  Result := FImageCache;
end;

function TSectionListBox.HasScrollBar: Boolean;
var
  lpmin, lpmax: Integer;
begin
  GetScrollRange(self.Handle, SB_VERT, lpmin, lpmax);
  Result := (lpmin <> 0) or (lpmax <> 0);
end;

function TSectionListBox.LinesInText(s: string; multiline: Boolean; height: Integer): Integer;
var
  r, hr: TRect;
  anchor, stripped, fa: string;
  xsize, ysize, ml, hl: Integer;
begin
  if not multiline then
  begin
    Result := 1;
    Exit;
  end;

  if pos('{\', s) = 1 then
  begin
    stringtorich(s, FRichedit);
    s := FRichedit.Text;
  end;

  r.Left := FSubItemIndent;
  r.Right := Width - GetSystemMetrics(SM_CXVSCROLL);
  r.Top := 0;
  r.Bottom := Height;

  if Pos('</', s) > 0 then
  begin
    SendMessage(Handle, LB_GETITEMRECT, 0, LParam(@r));

    r.Left := r.Left + SubItemIndent;
    HTMLDrawEx(Canvas, s, r, SubItemImages, 0, 0, -1, -1, 1, False, True, False, True, True, True, FWordWrap,
      1.0, clBlue, clNone, clNone, clGray, Anchor, stripped, fa, xsize, ysize, ml, hl, hr, FImageCache, FContainer,0);
    Result := YSize + 2;
    Exit;
  end;

  Result := Height * (DrawText(self.Canvas.Handle, pchar(s), length(s), r, DT_CALCRECT or DT_NOPREFIX or FDtStyle) div Canvas.textheight('gh'));
end;

destructor TSectionListBox.Destroy;
begin
  FSections.Free;
  FTabPositions.Free;
  FSectionFont.Free;
  FURLSettings.Free;
  FExpandGlyph.Free;
  FContractGlyph.Free;
  FRichEdit.Free;
  FMemo.Free;
  FWallpaper.Free;
  FImageCache.Free;
  FContainer.Free;
  inherited Destroy;
end;

procedure TSectionListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_HSCROLL;
end;

procedure TSectionListBox.CreateWnd;
begin
  inherited CreateWnd;
  FDestroyed := False;
  FRichEdit.Parent := Self;
  FRichEdit.Left := 0;
  FRichEdit.Top := 0;
  FRichEdit.Width := 0;
  FRichEdit.Height := 0;
  FRichEdit.Visible := False;
  FRichEdit.BorderStyle := bsNone;

  FMemo.Parent := Self;
  FMemo.Left := 0;
  FMemo.Top := 0;
  FMemo.Width := 0;
  FMemo.Height := 0;
  FMemo.Visible := False;
  FMemo.BorderStyle := bsNone;

  if not (csLoading in ComponentState) then
  begin
    if FSections.Count > 0 then
      FSections.Update(FSections.Items[0]);
  end;
  SectionHeight := FSectionHeight;
end;

procedure TSectionListBox.FontChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TSectionListBox.SetSections(Value: TListSectioncollection);
begin
  FSections.Assign(Value);
end;

procedure TSectionListBox.SetTabPositions(Value: TTabPositioncollection);
begin
  FTabPositions.Assign(Value);
end;

procedure TSectionListBox.SetTabPosMove(value: Boolean);
begin
  if Value <> FTabPosMove then
  begin
    FTabPosMove := Value;
    Self.Repaint;
  end
  else
    FTabPosMove := Value;
end;

function TSectionListBox.IsSection(idx: Integer): Boolean;
var
  i, j: Integer;

begin
  Result := False;
  j := 0;

  for i := 1 to FSections.Count do
  begin
    Result := idx = j;
    if idx = j then
      Break;
    Inc(j);
    if FSections.Items[i - 1].State = lssExpanded then
      j := j + FSections.Items[i - 1].SubItems.Count;
    if j > idx then
      Break;
  end;
end;

procedure TSectionListBox.DoPaint;
begin
  if (FUpdateCount = 0) then
    Repaint;
end;

procedure TSectionListBox.UpdateHorzScrollBar;
var
  dwExtent: Integer;
  i, max: Integer;
  d: array[0..MAX_TABS] of integer;
  s: string;

begin
  max := 0;

  if FUpdateCount > 0 then
    Exit;
    
  if fWordWrap then
  begin
    SendMessage(self.handle, LB_SETHORIZONTALEXTENT, max, 0);
    Exit;
  end;

  if (fTabPositions.Count > 0) and (fTabPositions.Count < MAX_TABS) then
    for i := 0 to fTabPositions.Count - 1 do
    begin
      d[i] := FTabPositions.Items[i].TabPosition;
    end;

  for i := 0 to Items.Count - 1 do
  begin
    s := items[i];
    if (pos('{\', s) > 0) then
    begin
      stringtorich(s, frichedit);
      s := frichedit.Text;
    end;
    if (pos('</', s) > 0) then
    begin
      s := '';
    end;
    if (pos(#13, s) > 0) then
    begin
      s := copy(s, 1, pos(#13, s));
    end;
    if (fTabPositions.Count > 0) then
      dwExtent := fsubitemindent + loword(GetTabbedTextExtent(Canvas.handle, PChar(s), Length(s), fTabPositions.Count, d))
    else
      dwExtent := fsubitemindent + loword(GetTabbedTextExtent(Canvas.handle, PChar(s), Length(s), 0, d));

    if (loword(dwExtent) > max) and (not GetItemSection(i).EndEllipsis) then max := loword(dwExtent);

    //if Assigned(SectionImages) then dwExtent:=dwExtent+sectionimages.width;
  end;

  if (max < self.width) then max := 0;
  SendMessage(self.handle, lb_SETHORIZONTALEXTENT, max, 0)
end;


function TSectionListBox.GetCustomSectionHeight(SectionIndex: integer): integer;
begin
  if FSectionheight = 0 then
    Result := 14
  else
    Result := FSectionHeight;

  if Assigned(OnSectionHeight) then
    OnSectionHeight(Self, SectionIndex, Result);
end;

function TSectionListBox.GetSectionListIndex(idx: Integer): Integer;
var
  i, j: Integer;
begin
  j := 0;

  for i := 1 to idx do
  begin
    if FSections.Items[i - 1].State = lssExpanded then
      j := j + FSections.Items[i - 1].SubItems.Count;
    inc(j);
  end;
  Result := j;
end;

function TSectionListBox.GetSectionSubItem(SectionIdx, subitemidx: Integer): string;
var
  s: string;
begin
  Result := '';
  if Subitemidx < FSections.Items[SectionIdx].SubItems.Count then
  begin
    s := FSections.Items[SectionIdx].SubItems[subitemidx];
    if pos('{\', s) > 0 then
    begin
      stringtorich(s, FRichEdit);
      s := FRichEdit.Text;
    end;
    Result := s;
  end;
end;

function TSectionListBox.GetRichSectionSubItem(SectionIdx, subitemidx: Integer): string;
begin
  Result := '';
  if SubItemIdx < FSections.Items[SectionIdx].SubItems.Count then
  begin
    Result := FSections.Items[SectionIdx].SubItems[SubItemidx];
  end;
end;

function TSectionListBox.GetItemAtXY(x, y: Integer; var SectionIdx, subitemidx: Integer): Boolean;
var
  idx: Integer;
begin
  idx := SendMessage(handle, lb_itemfrompoint, 0, MAKELPARAM(X, Y));
  Result := GetListItemIndex(Idx, SectionIdx, subitemidx);
end;

function TSectionListBox.GetSelectedItem(var SectionIdx, subitemidx: Integer): Boolean;
begin
  Result := GetListItemIndex(ItemIndex, SectionIdx, subitemidx);
end;

function TSectionListBox.GetItemSectionIndex(idx: Integer): Integer;
var
  i, j: Integer;
begin
  Result := -1;
  j := 0;

  for i := 1 to FSections.Count do
  begin
    Result := i - 1;
    Inc(j);
    if FSections.Items[i - 1].State = lssExpanded then
      j := j + FSections.Items[i - 1].SubItems.Count;
    if j > idx then
      Break;
  end;
end;

procedure TSectionListBox.FocusSection(section: TListSection);
begin
  Section.Focus;
end;

function TSectionListBox.GetActiveSection: TListSection;
var
  i: Integer;
begin
  Result := nil;
  if Itemindex >= 0 then
  begin
    i := GetItemSectionIndex(self.itemindex);
    if (i >= 0) and (i < FSections.Count) then
      Result := FSections.Items[i]
    else
      Result := nil;
  end;
end;



function TSectionListBox.GetItemSection(idx: Integer): TListSection;
var
  i: Integer;
begin
  i := GetItemSectionIndex(idx);
  if (i >= 0) and (i < FSections.Count) then
    Result := FSections.Items[i]
  else
    Result := nil;
end;

procedure TSectionListBox.WMVScroll(var WMScroll: TWMScroll);
begin
  if (WMScroll.ScrollCode = SB_THUMBTRACK) and
    (FScrollTrack = False) then
  begin
    WMScroll.Result := 0;
    Exit;
  end
  else
  begin
    inherited;
    if (not FWallPaper.Empty)
      and (FWallPaper.Height <> SubItemHeight) and (fUpdateCount = 0) then
      Invalidate;
  end;

  WMScroll.Pos := GetScrollPos(Handle, SB_VERT);
  FlatSetScrollPos(SB_VERT, WMScroll.Pos, True);
  UpdateVScrollbar;
end;

procedure TSectionListBox.WMHScroll(var WMScroll: TWMScroll);
begin
  inherited;
  WMScroll.Pos := GetScrollPos(Handle, SB_HORZ);
  FlatSetScrollPos(SB_HORZ, WMScroll.Pos, True);

end;

procedure TSectionListBox.WMEraseBkGnd(var Message: TMessage);
begin
  if (FWallpaper.Empty) and (FUpdateCount > 0) then
    Message.Result := 0 {avoid erase bk gnd to reduce flicker !}
  else
    inherited;
end;

procedure TSectionListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do

  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;


    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State);

    Canvas.Handle := 0;
  end;
end;

function TSectionListBox.GetListItemIndex(listindex: Integer; var SectionIdx, subitemidx: Integer): Boolean;
var
  idx: Integer;
begin
  idx := listindex;
  result := idx >= 0;
  if idx >= 0 then
  begin
    SectionIdx := GetItemSectionIndex(idx);
    Subitemidx := idx - GetSectionListIndex(SectionIdx) - 1;
  end;
end;

function TSectionListBox.GetSelection(var SectionIdx, subitemidx: integer;
  var selstring: string): boolean;
var
  idx: Integer;
begin
  idx := Itemindex;
  result := idx >= 0;
  if idx >= 0 then
  begin
    Selstring := Items[idx];
    SectionIdx := GetItemSectionIndex(idx);
    Subitemidx := Idx - GetSectionListindex(SectionIdx) - 1;
  end;
end;

function TSectionListBox.SetSelection(SectionIdx, subitemidx: integer): boolean;
var
  idx: Integer;
  section: TListSection;
begin
  Result := False;
  if SectionIdx >= FSections.Count then
    Result := False
  else
  begin
    Section := FSections.Items[SectionIdx];

    if SubItemIdx >= Section.SubItems.Count then
      Result := False
    else
    begin
      if (Section.State = lssContracted) and (SubItemidx >= 0) then
        Section.State := lssExpanded;

      idx := GetSectionListIndex(SectionIdx);
      if SubItemIdx >= 0 then
        idx := idx + SubItemIdx + 1;
      ItemIndex := idx;
    end;
  end;
end;


procedure TSectionListBox.MeasureItem(Index: Integer; var Height: Integer);
var
  res: Integer;
begin
  res := SendMessage(Handle, lb_getitemheight, index, 0);
  if res = LB_ERR then
  begin
    if SectionHeight > 0 then
      res := SectionHeight
    else
      res := 14;  
  end;
  if res = 0 then
    res := 14;
  height := res;
end;

procedure TSectionListBox.Clear;
begin
  BeginUpdate;
  self.Sections.Clear;
  EndUpdate;
end;

procedure TSectionListBox.OptimizeTabs(padding: Integer);
var
  max: array[0..MAX_TABS] of integer;
  i, j, k, l, t, o: Integer;
  s, su: string;

begin
  for i := 0 to MAX_TABS do max[i] := 0;

  l := 0;
  Canvas.Font.Assign(Font);

  if Assigned(subitemimages) then
    o := SubItemImages.Width
  else
    o := 0;

  for i := 1 to Items.Count do
  begin
    s := Items[i - 1];

    j := 0;
    while (pos(#9, s) > 0) and (j < MAX_TABS) do
    begin
      su := Copy(s, 1, pos(#9, s) - 1);
      k := Canvas.textwidth(su);
      if (k > max[j]) then max[j] := k;
      Delete(s, 1, pos(#9, s));
      Inc(j);
      if j > l then
        l := j;
    end;
    k := Canvas.TextWidth(s);
    if k > max[j] then
      max[j] := k;
  end;

  if (Items.count > 0) then
  begin
    t := padding;
    for i := 1 to l do
    begin
      if TabPositions.Count >= i then
      begin
        if (TabPositions.Items[i - 1].TabType = tabright) then
          TabPositions.Items[i - 1].TabPosition := t + max[i - 1] + max[i] + subitemindent + o
        else
          TabPositions.Items[i - 1].TabPosition := t + max[i - 1] + subitemindent + o;
      end
      else
        TabPositions.Add.TabPosition := t + max[i - 1] + subitemindent + o;

      t := t + max[i - 1] + padding;
    end;
  end;
  Repaint;
end;

procedure TSectionListBox.StringToRich(s: string; richeditor: trichedit);
var
  ms: TMemoryStream;
  i: Integer;
begin
  if s <> '' then
  begin
    ms := TMemoryStream.Create;
    for i := 1 to length(s) do ms.write(s[i], 1);
    ms.position := 0;
    richeditor.lines.loadfromstream(ms);
    ms.free;
  end
  else
  begin
    richeditor.clear;
  end;
end;

function TSectionListBox.RichToString(richeditor: trichedit): string;
var
  ms: TMemoryStream;
  s: string;
  i: Integer;
  ch: Char;
begin
  s := '';
  ms := TMemoryStream.Create;
  RichEditor.Lines.SaveToStream(ms);
  ms.Position := 0;
  if ms.Size > 0 then
    for i := 0 to ms.Size - 1 do
    begin
      ms.Read(ch, 1);
      s := s + ch;
    end;
  ms.Free;
  Result := s;
end;

procedure TSectionListBox.RichPaint(Canvas: tCanvas; arect: trect; s: string; sel: Boolean);
const
  RTF_OFFSET = 0;
type
  rFormatRange = record
    hdcSrc: HDC;
    hdcTarget: HDC;
    rc: TRect;
    rcPage: TRect;
    chrg: TCharRange;
  end;

var
  fr: rFORMATRANGE;
  nLogPixelsX, nLogPixelsY: Integer;

begin
  stringtorich(s, frichedit);

  if sel then
  begin
    FRichEdit.SelStart := 0;
    FRichEdit.SelLength := 255;
    FRichEdit.SelAttributes.Color := clWhite;
    SendMessage(FRichEdit.handle, EM_SETBKGNDCOLOR, 0, colortorgb(fSelectionColor));
  end
  else
    SendMessage(frichedit.handle, EM_SETBKGNDCOLOR, 0, colortorgb(Color));

  FillChar(fr, SizeOf(TFormatRange), 0);

  nLogPixelsX := GetDeviceCaps(Canvas.handle, LOGPIXELSX);
  nLogPixelsY := GetDeviceCaps(Canvas.handle, LOGPIXELSY);

  with fr do
  begin
    fr.hdcSrc := Canvas.Handle;
    fr.hdcTarget := Canvas.Handle;

    fr.rcPage.Left := Round(((arect.Left + RTF_OFFSET) / nLogPixelsX) * 1440);
    fr.rcPage.Top := Round(((arect.Top + RTF_OFFSET) / nLogPixelsY) * 1440);
    fr.rcPage.Right := fr.rcPage.Left + round(((arect.Right - arect.Left - RTF_OFFSET) / nLogPixelsX) * 1440);
    fr.rcPage.Bottom := fr.rcPage.Top + round(((arect.Bottom - arect.Top - RTF_OFFSET) / nLogPixelsY) * 1440);
    fr.rc.Left := fr.rcPage.Left; { 1440 TWIPS = 1 inch. }
    fr.rc.Top := fr.rcPage.Top;
    fr.rc.Right := fr.rcPage.Right;
    fr.rc.Bottom := fr.rcPage.Bottom;

    fr.chrg.cpMin := 0;
    fr.chrg.cpMax := -1;
  end;
  SendMessage(frichedit.handle, EM_FORMATRANGE, 1, LParam(@fr));
end;


procedure TSectionListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  FlatStyle: array[Boolean] of Word = (0, DFCS_FLAT);
  InactiveStyle: array[Boolean] of Word = (0, DFCS_INACTIVE);
var
  sct, st, dis, fix, vertlines, horzlines, checked: Boolean;
  ListSection: TListSection;
  brcol, txtcol: TColor;
  imidx, imofs: Integer;
  fDtAlign, fDtValign: Integer;
  DText, Anchor, Stripped, fa: string;
  xsize, ysize: Integer;
  val: TValignment;

  procedure DrawListText(Canvas: TCanvas; x, y: Integer; s: string;
    Section, st, dis: Boolean; bcol, tcol: TColor);
  var
    r, hr: trect;
    er: trect;
    i: Integer;
    h: Integer;
    t, tl, hl, ml: Integer;
    d: array[0..MAX_TABS] of integer;
    DrawState: Integer;
    DrawRect: trect;

  begin
    tl := 0;
    if sct then
    begin
      Canvas.Brush.Color := FSectionColor;
      Canvas.Font.Color := clBtnText;
      if listsection.FFontUsage in [fuBoth, fuSection] then
        Canvas.Font.Assign(ListSection.Font)
      else
        Canvas.Font.Assign(FSectionFont);
    end
    else
    begin
      if ListSection.FFontUsage in [fuBoth, fuSubItems] then
        Canvas.Font.Assign(ListSection.Font)
      else
        Canvas.Font.Assign(Font);

      if (odSelected in State) then
      begin
        if FFullFocus then
          Canvas.Brush.Color := FSelectionColor;
        Canvas.Font.Color := FSelectiontextColor;
      end
      else
        Canvas.Brush.Color := bcol;
    end;

    if FURLSettings.URLAware then
      if (pos('://', s) > 0) or (pos('mailto:', s) > 0) then
      begin
        Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
        Canvas.Font.Color := FURLSettings.URLColor;
        if not FURLSettings.FURLFull then
        begin
          Delete(s, 1, pos(':', s));
          if pos('/', s) = 1 then delete(s, 1, 2);
        end;
      end;

    r := rect;

    if FWallpaper.Empty or sct then Canvas.FillRect(r);

    if sct and not fix then
      r.Left := r.Left + FSectionIndent
    else
      r.Left := r.Left + FSubItemIndent;
                                     {change from subitemheight}
    if sct then
    begin
      if SectionHeight > 0 then
        h := SectionHeight
      else
        h := 14;  
    end
    else
      h := ListSection.ItemHeight;

    if (ListSection.ControlType = scCheckbox) and not sct then
    begin
      DrawRect := r;
      DrawRect.Right := DrawRect.Left + 14;

      case val of
        vtaTop: DrawRect.Top := DrawRect.Top + 1;
        vtaCenter: DrawRect.Top := DrawRect.Top + ((DrawRect.Bottom - DrawRect.Top - 12) shr 1);
        vtaBottom: DrawRect.Top := DrawRect.Bottom - 14;
      end;

      DrawRect.Bottom := DrawRect.Top + 14;
      DrawState := DFCS_BUTTONCHECK or FlatStyle[flat] or InactiveStyle[ListSection.readonly];
      if checked then DrawState := DrawState or DFCS_CHECKED;
      DrawFrameControl(Canvas.Handle, DrawRect, DFC_BUTTON, DrawState);
      r.Left := r.Left + 16;
    end;

    if (ListSection.ControlType = scRadioButton) and not sct then
    begin
      DrawRect := r;
      DrawRect.Right := DrawRect.Left + 10;
      case val of
        vtaTop: DrawRect.Top := DrawRect.Top + 2;
        vtaCenter: DrawRect.Top := DrawRect.Top + ((DrawRect.Bottom - DrawRect.Top - 10) shr 1);
        vtaBottom: DrawRect.Top := DrawRect.Bottom - 10;
      end;
      DrawRect.Bottom := DrawRect.Top + 10;
      DrawState := DFCS_BUTTONRADIO or FlatStyle[flat] or InactiveStyle[ListSection.readonly];
      if checked then DrawState := DrawState or DFCS_CHECKED;
      DrawFrameControl(Canvas.Handle, DrawRect, DFC_BUTTON, DrawState);
      r.Left := r.Left + 16;
    end;

    if imidx >= 0 then
    begin
      if sct then
      begin
        case val of
          vtaTop: imofs := 0;
          vtaCenter: imofs := (r.Bottom - r.Top - SectionImages.Height) shr 1;
          vtaBottom: imofs := r.Bottom - r.Top - SectionImages.Height;
        end;

        SectionImages.Draw(self.Canvas, r.Left, r.Top + imofs, imidx);
        r.Left := r.Left + SectionImages.Width + fImageSpacing;
      end
      else
      begin
        case val of
          vtaTop: imofs := 0;
          vtaCenter: imofs := (r.Bottom - r.Top - SubItemImages.Height) shr 1;
          vtaBottom: imofs := r.Bottom - r.Top - SubItemImages.Height;
        end;

        SubItemImages.Draw(self.Canvas, r.Left, r.Top + imofs, imidx);
        r.Left := r.Left + subitemimages.Width + fImageSpacing;
      end;
    end;

    t := r.Left;

    if pos('{\', s) = 1 then
    begin
      RichPaint(Canvas, r, s, odSelected in State);
    end
    else
      if pos('</', s) > 0 then
        HTMLDrawEx(Canvas, s, r, subitemimages, 0, 0, -1, -1, 1, False, False, False, (odselected in state), True, True, FWordWrap,
          1.0, clBlue, clNone, clNone, clGray, anchor, stripped, fa, xsize, ysize, ml, hl, hr, FImageCache, FContainer,0)
      else

      begin
        if not FWallpaper.Empty then
          SetBKMode(Canvas.handle, transparent);

        if not FFullFocus and not sct then
        begin
          if odSelected in State then
            Canvas.Brush.Color := FSelectionColor;
        end;

        if Assigned(FOnDrawItemProp) then
        begin
          imidx := GetItemSectionIndex(index);
          imofs := GetSectionListIndex(imidx);
          FOnDrawItemProp(self, listsection, imidx, index - imofs - 1, Canvas);
        end;

        if (pos(#9, s) = 0) and not sct then
        begin
          if (r.Right > self.Width - GetSystemMetrics(SM_CXVSCROLL)) and FWordWrap then
            r.Right := self.Width - GetSystemMetrics(SM_CXVSCROLL);

          if FWordWrap then
            DrawText(Canvas.Handle, pchar(s), length(s), r,
              fDtAlign or DT_NOPREFIX or FDtStyle or DT_WORDBREAK)
          else
            DrawText(Canvas.Handle, pchar(s), length(s), r,
              fDtAlign or DT_NOPREFIX or FDtStyle or fdtVAlign);
          tl := makelong(Canvas.TextWidth(s), Canvas.TextHeight(s));

        end
        else
        begin
          if (FTabPositions.Count > 0) and (FTabPositions.Count < MAX_TABS) then
          begin
            for i := 0 to FTabPositions.Count - 1 do
            begin
              if FTabPositions.Items[i].TabType = tabLeft then
                d[i] := FTabPositions.Items[i].TabPosition
              else
                d[i] := -FTabPositions.Items[i].TabPosition
            end;

            case val of
              vtaCenter:
                tl := TabbedTextOut(Canvas.handle, r.Left, r.Top + ((Max(0, h - Canvas.textheight('gh'))) shr 1), pchar(s), length(s),
                  FTabPositions.Count, d, 0);
              vtaBottom:
                tl := TabbedTextOut(Canvas.handle, r.Left, r.Bottom - Canvas.textheight('gh'), pchar(s), length(s),
                  FTabPositions.Count, d, 0);
              vtaTop:
                tl := TabbedTextOut(Canvas.handle, r.Left, r.Top, pchar(s), length(s),
                  FTabPositions.Count, d, 0);
            else
              tl := 0;
            end;


            if (not sct and vertlines) or (fTabPosMove and (ListSection.State = lssExpanded)) then
              for i := 0 to fTabPositions.Count - 1 do
              begin
                if d[i] < 0 then d[i] := -d[i] + 2 else d[i] := d[i] - 2;
                Canvas.Pen.Color := clBlack;
                Canvas.Moveto(d[i], r.Top);
                Canvas.Lineto(d[i], r.Bottom);
                Canvas.Pen.Color := clWhite;
                Canvas.Moveto(d[i] + 1, r.Top);
                Canvas.Lineto(d[i] + 1, r.Bottom);
                Canvas.Pen.Color := clBlack;
              end;
          end
          else
          begin
            if FWordWrap then
            begin
              DrawText(Canvas.Handle, pchar(s), length(s), r, FDTAlign or DT_WORDBREAK {or DT_NOPREFIX});
            end
            else
            case val of
              vtaCenter:
                tl := TabbedTextOut(Canvas.handle, r.Left, r.Top + (Max(0, (h - Canvas.textheight('gh'))) shr 1), pchar(s), length(s), 0, d, 0);
              vtaBottom:
                tl := TabbedTextOut(Canvas.handle, r.Left, r.Bottom - Canvas.textheight('gh'), pchar(s), length(s), 0, d, 0);
              vtaTop:
                tl := TabbedTextOut(Canvas.handle, r.Left, r.Top, pchar(s), length(s), 0, d, 0);
            else
              tl := 0;
            end;
          end;

        end;
      end;

    r := rect;
    inc(r.Right, 1);

    if sct then
    begin
      if (odFocused in State) and (fSectionFocus = sf3D) then
        Frame3D(Canvas, r, clWhite, clGray, Effect3DSize)
      else
        Frame3D(Canvas, r, clWhite, clGray, 1)
    end
    else
      if horzlines then
      begin
        Canvas.moveto(r.Left, r.Bottom - 1);
        Canvas.lineto(r.Right, r.Bottom - 1);
      end;

    r := rect;

    if sct and ListSection.fSortShow and (ListSection.State = lssExpanded) then
    begin
      r.Right := self.Width - GetSystemMetrics(SM_CXVSCROLL) - 10;
      r.Top := r.Top + ((r.Bottom - r.Top - 8) shr 1);
      case ListSection.SortDirection of
        sdNone: begin
            Canvas.pen.Color := clGray;
            Canvas.moveto(r.Right - 10, r.Top);
            Canvas.lineto(r.Right - 10, r.Top + 8);
            Canvas.lineto(r.Right - 2, r.Top + 8);
            Canvas.lineto(r.Right - 2, r.Top);
            Canvas.lineto(r.Right - 10, r.Top);
          end;
        sdDescending: begin
            Canvas.pen.Color := clWhite;
            Canvas.moveto(r.Right - 10, r.Top);
            Canvas.lineto(r.Right - 6, r.Top + 8);
            Canvas.pen.Color := clGray;
            Canvas.lineto(r.Right - 2, r.Top);
            Canvas.lineto(r.Right - 10, r.Top);
          end;
        sdAscending: begin
            Canvas.pen.Color := clWhite;
            Canvas.moveto(r.Right - 6, r.Top);
            Canvas.lineto(r.Right - 10, r.Top + 8);
            Canvas.pen.Color := clGray;
            Canvas.lineto(r.Right - 2, r.Top + 8);
            Canvas.lineto(r.Right - 6, r.Top);
          end;
      end;
      r := rect;
    end;

    if sct and not fix then
    begin
      er.Left := r.Left + 4;
      er.Right := er.Left + 8;
      er.Top := r.Top + ((h - 8) shr 1);
      er.Bottom := er.Top + 8;

      case fNodeType of
        lnFlat:
          begin
            if dis then Canvas.pen.Color := clGray else Canvas.pen.Color := clBlack;
            Canvas.rectangle(er.Left - 1, er.Top - 1, er.Right, er.Bottom);
            if not dis then
              if st then
              begin
                Canvas.moveto(er.Left + 1, er.Top + 3);
                Canvas.lineto(er.Left + 6, er.Top + 3);
                Canvas.moveto(er.Left + 3, er.Top + 1);
                Canvas.lineto(er.Left + 3, er.Top + 6);
              end
              else
              begin
                Canvas.moveto(er.Left + 1, er.Top + 3);
                Canvas.lineto(er.Left + 6, er.Top + 3);
              end;
          end;
        ln3D:
          begin
            if dis then
              DrawEdge(Canvas.handle, er, EDGE_ETCHED, BF_RECT or BF_SOFT)
            else
              if st then
                DrawEdge(Canvas.handle, er, EDGE_RAISED, BF_RECT or BF_SOFT)
              else
                DrawEdge(Canvas.handle, er, EDGE_SUNKEN, BF_RECT or BF_SOFT);
          end;
        lnGlyph:
          begin
            r.Left := r.Left + Effect3DSize;
            r.Top := r.Top {+Effect3DSize} + (r.Bottom - r.Top - fContractGlyph.Height) shr 1;
            if st and not (fContractGlyph.Empty) then
              DrawBitmapTransp(Canvas, fContractGlyph, sectioncolor, r);
            if not st and not (fExpandGlyph.Empty) then
              DrawBitmapTransp(Canvas, fExpandGlyph, sectioncolor, r);
          end;
      end;
    end;

    if (odFocused in State) and not (fix and sct) and (fWallpaper.Empty) and
      not (sct and (fSectionFocus = sf3D)) then
    begin
      er := rect;

      dec(er.Right); inc(er.Left); inc(er.Top); dec(er.Bottom);

      if not fFullfocus then
      begin
        case val of
          vtaTop: er.Top := rect.Top;
          vtaCenter: er.Top := rect.Top + ((rect.Bottom - rect.Top - hiword(tl)) shr 1);
          vtaBottom: er.Top := rect.Bottom - hiword(tl);
        end;
        case val of
          vtaTop: er.Bottom := rect.Top + hiword(tl);
          vtaCenter: er.Bottom := er.Top + hiword(tl);
          vtaBottom: er.Bottom := rect.Bottom;
        end;
        er.Left := t - 1;
        er.Right := t + loword(tl) + 1;
      end;

      if not sct or (sct and (SectionFocus <> sfNone)) then
        Canvas.DrawFocusRect(er);
    end;
  end;

begin
  if FUpdateCount > 0 then Exit;
  if FExpandContractBusy then Exit;

  txtcol := clWindowText;
  st := False;
  dis := False;
  sct := SendMessage(Handle, LB_GETITEMDATA, index, 0) = 1;

  ListSection := GetItemSection(Index);

  if not Assigned(ListSection) then
    Exit;

  brcol := ListSection.Color;
  imidx := ListSection.ImageIndex;
  fix := ListSection.Fixed;
  vertlines := ListSection.Lines in [slVertical, slBoth];
  horzlines := ListSection.Lines in [slHorizontal, slBoth];

  if ListSection.OwnerDraw and Assigned(OnDrawItem) and not sct then
  begin
    imidx := GetItemSectionIndex(index);
    imofs := GetSectionListIndex(imidx);

    if odSelected in State then
    begin
      Canvas.Brush.Color := FSelectionColor;
      Canvas.Font.Color := FSelectiontextColor;
    end
    else
    begin
      Canvas.Brush.Color := ListSection.Color;
      Canvas.Font.Color := ListSection.Font.Color;
    end;

    Canvas.Fillrect(Rect);
    OnDrawItem(Self, ListSection, imidx, index - imofs - 1, Canvas, rect, state);
    Exit;
  end;

  case ListSection.Alignment of
    taLeftJustify: FDtAlign := DT_LEFT;
    taRightJustify: FDtAlign := DT_RIGHT;
    taCenter: FDtAlign := DT_CENTER;
  end;

  case ListSection.VAlignment of
    vtaBottom: FDtVAlign := DT_BOTTOM;
    vtaTop: FDtVAlign := DT_TOP;
    vtaCenter: FDtVAlign := DT_VCENTER;
  end;

  if Pos(#13, Items[index]) = 0 then
    FDtValign := FDtValign or DT_SINGLELINE
  else
    FDtValign := DT_TOP;

  val := ListSection.VAlignment;

  if ListSection.EndEllipsis then
    FDtAlign := FDtAlign or DT_END_ELLIPSIS;

  if sct then
  begin
    st := ListSection.State = lssContracted;
    dis := ListSection.SubItems.Count = 0;
    if not Assigned(SectionImages) then
      imidx := -1;
  end
  else
  begin
    imidx := index - GetSectionListIndex(getItemSectionIndex(index)) - 1;
    imidx := ListSection.SubItemImageIdx[imidx];

    if not Assigned(SubItemImages) then
      imidx := -1;

    Checked := ListSection.RadioIndex = Index - GetSectionListIndex(getItemSectionIndex(index)) - 1;

    if ListSection.ControlType = scCheckBox then
      Checked := ListSection.SubItemCheckState[index - GetSectionListIndex(getItemSectionIndex(index)) - 1];
  end;

  DText := GetDisplText(ListSection, Index);

  DrawListText(Canvas, Rect.Left, Rect.Top, DText, sct, st, dis, brcol, txtcol);
end;
{
procedure TSectionListBox.SetItemEx(Index: Integer; const value: string);
var
  sectionidx, subitemidx: integer;
begin
  GetListItemIndex(Index, sectionidx, subitemidx);
  Sections[sectionidx].SubItems[Subitemidx];
end;


function TSectionListBox.GetItemEx(Index: Integer): string;
var
  sectionidx, subitemidx: integer;
begin
  GetListItemIndex(Index, sectionidx, subitemidx);
  Result := Sections[sectionidx].SubItems[Subitemidx];
end;
}

procedure TSectionListBox.DeleteSelected;
var
  i: integer;
  sectionidx, subitemidx: integer;
begin
  for i := Items.Count - 1 downto 0 do
  begin
    if Selected[i] then
    begin
      GetListItemIndex(i, sectionidx, subitemidx);
      Sections[sectionidx].SubItems.Delete(Subitemidx);
    end;
  end;
end;


procedure TSectionListBox.ToggleSectionState(idx: Integer);
var
  i, j: Integer;
  ListSection: TListSection;
begin
  i := GetItemSectionIndex(idx);
  ListSection := GetItemSection(idx);

  if (ListSection.State = lssExpanded) and (not fOneExpanded) then
  begin
    ListSection.State := lssContracted;

    if Assigned(FOnContractSection) then
      FOnContractSection(self, i);
  end
  else
  begin
    if fOneExpanded then
    begin
      for j := 1 to Sections.Count do
      begin
        if Sections.items[j - 1] <> ListSection then
          Sections.items[j - 1].State := lssContracted
        else
          ListSection.State := lssExpanded;
      end;
    end
    else
    begin
      ListSection.State := lssExpanded;
    end;

    if Assigned(fOnExpandSection) then
      FOnExpandSection(self, i);
  end;
end;

procedure TSectionListBox.WMDestroy(var Message: TMessage);
begin
  FDestroyed := True;
  inherited;
end;

procedure TSectionListBox.WMSize(var Msg: TWMSize);
begin
  inherited;
  if FWordWrap then
  begin
    UpdateItemHeight;
    Repaint;
  end;
end;

procedure TSectionListBox.WMMouseMove(var Message: TWMMouseMove);
var
  res, i, xsize, ysize, ml, hl: Integer;
  buf: array[0..4095] of char;
  s, anchor, stripped, fa: string;
  r, hr: trect;
  lns, lne: Integer;
begin
  if (not fTabMove) then inherited;

  res := SendMessage(Handle, lb_itemfrompoint, 0, MAKELPARAM(Message.xPos, Message.yPos));

  if (res >= 0) and (res < Items.count) then
  begin
    if not fTabMove then
    begin
      fTabSelect := False;
      if (SendMessage(Handle, lb_getitemdata, res, 0) = 1) then
      begin
        if (fTabPosMove) then
          for i := 1 to tabpositions.Count do
          begin
            if (abs(message.xpos - ftabpositions.Items[i - 1].Tabposition) < 4) then
            begin
              fTabSelect := True;
              fTabPos := i - 1;
              Cursor := crHSplit;
              fOldTabPos := ftabpositions.Items[i - 1].Tabposition - 2;
              Exit;
            end;
          end;

        SendMessage(Handle, lb_gettext, res, LParam(@buf));

        s := strpas(buf);

        if pos('</', s) > 0 then
        begin
          SendMessage(Handle, lb_getitemrect, res, LParam(@r));
          r.Left := r.Left + self.SectionIndent;
          if HTMLDrawEx(Canvas, s, r, subitemimages, message.xpos, message.ypos, -1, -1, 1, True, False, False, True, True, True, FWordWrap,
            1.0, clBlue, clNone, clNone, clGray, anchor, stripped, fa, xsize, ysize, ml, hl, hr, FImageCache, FContainer,0) then
          begin
            Cursor := crHandPoint;
            if (anchor <> fOldAnchor) then
            begin
              if Assigned(fAnchorEnter) then fAnchorEnter(self, 1, anchor);
            end;
            fOldAnchor := anchor;
          end
          else
          begin
            if (fOldAnchor <> '') then
            begin
              if Assigned(fAnchorExit) then fAnchorExit(self, 1, anchor);
              fOldAnchor := '';
            end;
          end;
        end
        else
        begin
          Cursor := fOldCursor;
          if (FOldAnchor <> '') then
          begin
            if Assigned(fAnchorExit) then fAnchorExit(self, 1, anchor);
            FOldAnchor := '';
          end;
         end;
        Exit;

      end
      else
      begin
        Cursor := fOldCursor;
      end;
    end
    else
    begin
      while SendMessage(Handle, lb_getitemdata, res, 0) <> 1 do dec(res);

      SendMessage(Handle, lb_getitemrect, res, LParam(@r));
      if (message.ypos > r.Bottom) then Exit;

      Canvas.pen.mode := pmNotXor;
      Canvas.pen.Color := clBlack;

      lns := r.Top;
      SendMessage(Handle, lb_getitemrect, res + GetItemSection(res).Subitems.Count, LParam(@r));
      lne := r.Bottom;

      Canvas.moveto(fOldTabPos, lns);
      Canvas.lineto(foldTabPos, lne);

      Canvas.moveto(message.xpos, lns);
      Canvas.lineto(message.xpos, lne);

      fOldTabPos := message.xpos;

      Canvas.pen.mode := pmCopy;
    end;

    SendMessage(Handle, lb_gettext, res, LParam(@buf));
    s := strpas(buf);

    if ((pos('://', s) > 0) or (pos('mailto:', s) > 0)) and (pos('</', s) = 0) then
      Cursor := crHandPoint
    else
      if pos('</', s) > 0 then
      begin
        SendMessage(Handle, lb_getitemrect, res, LParam(@r));
        r.Left := r.Left + self.SubItemIndent;
        if HTMLDrawEx(Canvas, s, r, subitemimages, message.xpos, message.ypos, -1, -1, 1, True, False, False, True, True, True, FWordWrap,
          1.0, clBlue, clNone, clNone, clGray, anchor, stripped, fa, xsize, ysize, ml, hl, hr, FImageCache, FContainer,0) then
        begin
          Cursor := crHandPoint;
          if (anchor <> fOldAnchor) then
          begin
            if Assigned(fAnchorEnter) then fAnchorEnter(self, 1, anchor);
          end;
          fOldAnchor := anchor;
        end
        else
        begin
          if (fOldAnchor <> '') then
          begin
            if Assigned(fAnchorExit) then fAnchorExit(self, 1, anchor);
            fOldAnchor := '';
          end;
        end;
      end
      else
      begin
        Cursor := fOldCursor;
        if (fOldAnchor <> '') then
        begin
          if Assigned(fAnchorExit) then fAnchorExit(self, 1, anchor);
          fOldAnchor := '';
        end;
      end;
  end
  else
    Cursor := fOldCursor;
end;

procedure TSectionListBox.WMRButtonDown(var Message: TWMRButtonDown);
var
  idx, i, j: Integer;
  r: trect;
begin
  inherited;

  idx := SendMessage(self.handle, LB_ITEMFROMPOINT, 0, makelong(message.xpos, message.ypos));

  SendMessage(Handle, LB_GETITEMRECT, idx, LParam(@r));

  if not PtInRect(r, point(message.xpos, message.ypos)) then Exit;

  if (SendMessage(Handle, lb_getitemdata, idx, 0) <> 1) then
  begin
    i := self.GetItemSectionIndex(idx);
    if Assigned(fOnSubItemRClick) then
    begin
      j := self.GetSectionListIndex(i);
      fOnSubItemRClick(self, i, idx - j - 1);
    end;
  end
  else
  begin
    i := self.GetItemSectionIndex(idx);
    if Assigned(fOnSectionRClick) then
    begin
      fOnSectionRClick(self, i);
    end;
  end;
end;

function TSectionListBox.StartEdit(idx: Integer): Boolean;
var
  r: trect;
  ListSection: TListSection;
  s: string;
begin
  result := False;
  ListSection := self.GetItemSection(idx);
  if ListSection.ReadOnly or (ListSection.ControlType <> scText) then
    Exit;

  Result := True;

  SendMessage(Handle, lb_getitemrect, idx, LParam(@r));
  FMemo.ItemIdx := idx;
  FMemo.Color := listsection.Color;
  FMemo.Top := r.Top;
  FMemo.Left := r.Left;
  FMemo.Width := (r.Right - r.Left);
  FMemo.Height := (r.Bottom - r.Top);
  FMemo.ReadOnly := False;
  FMemo.Enabled := True;
  FMemo.SectionIdx := GetItemSectionIndex(idx);
  FMemo.SubItemIdx := idx - GetSectionListIndex(fMemo.SectionIdx);
  FMemo.Lines.Clear;
  ListSection := self.GetItemSection(idx);
  s := ListSection.SubItems.Strings[FMemo.SubItemIdx - 1];

  if Assigned(FOnStartEdit) then
    FOnStartEdit(self, fMemo.SectionIdx, fMemo.SubItemIdx, s);

  FMemo.Lines.Add(s);
  FMemo.SelStart := 0;
  FMemo.SelLength := length(fMemo.Text) - 2;
  FMemo.fListSection := ListSection;
  FMemo.WantReturns := False;
  FMemo.visible := True;
  FMemo.SetFocus;
  SendMessage(FMemo.handle, EM_LINESCROLL, 0, -255);
end;

procedure TSectionListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  Invalidate;
end;


procedure TSectionListBox.WMLButtonDblClk(var Message: TWMLButtonDown);
var
  idx, i, j: Integer;
begin
  inherited;

  idx := self.itemindex;
  i := self.GetItemSectionIndex(idx);

  if (SendMessage(handle, lb_getitemdata, idx, 0) = 1) then
  begin
    if Assigned(fOnSectionDblClk) then
    begin
      fOnSectionDblClk(self, i);
    end;
  end
  else
  begin
    if Assigned(fOnSubItemDblClk) then
    begin
      j := self.GetSectionListIndex(i);
      fOnSubItemDblClk(self, i, idx - j - 1);
    end;
  end;
end;

procedure TSectionListBox.WMRButtonDBlClk(var Message: TWMRButtonDown);
var
  idx, i, j: Integer;
begin
  inherited;
  idx := self.itemindex;
  i := self.GetItemSectionIndex(idx);
  if Assigned(fOnSubItemRDblClk) then
  begin
    j := self.GetSectionListIndex(i);
    fOnSubItemRDblClk(self, i, idx - j - 1);
  end;
end;

procedure TSectionListBox.WMLButtonUp(var Message: TWMLButtonDown);
var
  idx: integer;
  ListSection: TListSection;
begin
  if fTabMove then
  begin
    fTabMove := False;
    ftabpositions.Items[fTabPos].tabposition := message.xpos;
    Repaint;
  end;

  inherited;

  idx := self.itemindex;

  //i := self.GetItemSectionIndex(idx);
  ListSection := self.GetItemSection(idx);

   if (SendMessage(handle, lb_getitemdata, idx, 0) = 1) and (fTabSelect = False) then
  begin
    if (ListSection.State = lssExpanded) and FContractDisable then Exit;
    if (ListSection.State = lssContracted) and FExpandDisable then Exit;
    if not ListSection.Fixed then ToggleSectionState(idx);
    if (ListSection.State = lssExpanded) and ListSection.AutoEdit then StartEdit(idx + 1);
  end

end;

procedure TSectionListBox.WMLButtonDown(var Message: TWMLButtonDown);
var
  idx, i, j, data, xsize, ysize, ml, hl: Integer;
  sr, r, hr: trect;
  buf: array[0..4095] of char;
  s, anchor, stripped, fa: string;
  res: Integer;
  ListSection: TListSection;
  wascontrolclick: Boolean;

begin
  wascontrolclick := False;
  res := SendMessage(self.handle, lb_itemfrompoint, 0, MAKELPARAM(Message.xPos, Message.yPos));

  SendMessage(Handle, lb_getitemrect, res, LParam(@r));

  if not ptinrect(r, point(Message.xPos, Message.yPos)) then
  begin
    if MultiSelect then for i := 1 to Items.Count do Selected[i - 1] := False else
      ItemIndex := -1;

    if Assigned(OnClick) then
      OnClick(Self);
    Exit;
  end;

  if fTabSelect then fTabMove := True;

  if (res >= 0) and (res < Items.count) then
  begin
     SendMessage(Handle, lb_gettext, res, LParam(@buf));
     s := strpas(buf);

    if (fURLSettings.URLAware) then
    begin
      if (pos('://', s) > 0) or (pos('mailto:', s) > 0) and (pos('</', s) = 0) then
      begin
        ShellExecute(Application.Handle, 'open', buf, nil, nil, SW_NORMAL);
        Exit;
      end;
    end;
    if (pos('</', s) > 0) then
    begin
      SendMessage(Handle, lb_getitemrect, res, LParam(@r));
      r.Left := r.Left + SubItemIndent;
      if HTMLDrawEx(Canvas, s, r, subitemimages, message.xpos, message.ypos, -1, -1, 1, True, False, False, True, True, True, FWordWrap,
        1.0, clBlue, clNone, clNone, clGray, anchor, stripped, fa, xsize, ysize, ml, hl, hr, FImageCache, FContainer,0) then
      begin
        if (pos('://', anchor) > 0) or (pos('mailto:', anchor) > 0) then
          shellexecute(0, 'open', pchar(anchor), nil, nil, SW_NORMAL)
        else
        begin
          if Assigned(fAnchorClick) then
            fAnchorClick(self, res, anchor);
        end;
        Exit;
      end;
    end;

    if (message.xpos > subitemindent) and (message.xpos < subitemindent + 15) then
//    if (message.xpos > sectionindent) and (message.xpos < sectionindent + 15) then
    begin
      wascontrolclick := True;
      data := SendMessage(Handle, lb_getitemdata, res, 0);

      if (data = 0) then
      begin
        i := self.GetItemSectionIndex(res);
        ListSection := self.GetItemSection(res);
        j := self.GetSectionListIndex(i);

        if not ListSection.ReadOnly then
        begin
          ListSection.SetSubItemCheckState(res - j - 1, not ListSection.SubItemCheckState[res - j - 1]);

          if (ListSection.ControlType = scCheckBox) then
            if Assigned(fOnCheckBoxClick) then
              fOnCheckBoxClick(self, i, res - j - 1);

          if ListSection.ControlType = scRadioButton then
          begin
            ListSection.RadioIndex := res - j - 1;
            if Assigned(fOnRadioClick) then
              fOnRadioClick(self, i, res - j - 1);
          end;
        end;
      end;
    end;

  end;

  if (res = self.ItemIndex) and
    (SendMessage(handle, lb_getitemdata, res, 0) = 0) then
  begin
    if StartEdit(res) then Exit;
  end;
  inherited;
  {do expand/collapse here}

  idx := self.itemindex;

  SendMessage(Handle, lb_getitemrect, idx, LParam(@r));

  if (SendMessage(Handle, lb_getitemdata, idx, 0) = 1) then
  begin
    if Assigned(fOnSectionClick) then
    begin
      fOnSectionClick(self, self.GetItemSectionIndex(idx));
    end;

    sr := r;
    sr.Right := self.width - GetSystemMetrics(SM_CXVSCROLL) - 10;
    sr.Left := sr.Right - 10;
    if ptInRect(sr, point(message.xpos, message.ypos)) then
    begin
      ListSection := self.GetItemSection(idx);
      if ListSection.SortShow then
      begin
        BeginUpdate;
        if ListSection.SortDirection in [sdNone, sdDescending] then
          ListSection.SortDirection := sdAscending
        else
          ListSection.SortDirection := sdDescending;
        EndUpdate;
      end;
      Exit;
    end;

    if (fActiveSection = asNodeOnly) then
    begin
      if NodeType = lnGlyph then r.Right := 4 + fContractGlyph.width else
        r.Right := 12;
    end;
  end;

  if not ptinrect(r, point(message.xpos, message.ypos)) then Exit;

  {clicked on empty item !!}

  i := self.GetItemSectionIndex(idx);
  ListSection := self.GetItemSection(idx);

  if (SendMessage(handle, lb_getitemdata, idx, 0) = 1) and (fTabSelect = False) then
  begin
    {
    if (ListSection.State = lssExpanded) and FContractDisable then Exit;
    if (ListSection.State = lssContracted) and FExpandDisable then Exit;
    if not ListSection.Fixed then ToggleSectionState(idx);
    if (ListSection.State = lssExpanded) and ListSection.AutoEdit then StartEdit(idx + 1);
    }
  end
  else
  begin
    if (ListSection.ControlType = scCheckBox) and wascontrolclick then
    begin
      {force a repaint of the focused item}
      SendMessage(self.handle, lb_setcursel, res, 0);
    end;

    if Assigned(fOnSubItemClick) then
    begin
      j := self.GetSectionListIndex(i);
      fOnSubItemClick(self, i, idx - j - 1);
    end;
  end;
end;

procedure TSectionListBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  idx, SectionIdx, subitemidx, i: Integer;
  itemstr: string;
  ListSection: tListSection;
  allow: Boolean;
  j, k, l: Integer;
begin
  inherited;

  idx := itemindex;
  itemstr := '';
  SectionIdx := self.GetItemSectionIndex(itemindex);
  subitemidx := itemindex - self.GetSectionListIndex(SectionIdx) - 1;
  
  // Subitems list, must be done before sections

  j := SubItemIdx;
  if (SubItemIdx > -1) then                                       
  begin
    case Key of
      VK_DOWN, VK_RIGHT: j := SubItemIdx + 1;  // Correct 'last position' index
      VK_UP, VK_LEFT:    j := SubItemIdx - 1;  // Correct 'last position' index
    end;
    if Assigned(FOnSubItemClick) and (j > -1) and
      (j < Sections[SectionIdx].SubItems.Count) then
      FOnSubItemClick(Self, SectionIdx, j);    // Selection, must be implemented!
  end;

  // Sections list, must follow after subitems

  l := SectionIdx;


  if (Key in [VK_DOWN, VK_UP, VK_RIGHT, VK_LEFT]) then
  begin
    if (SubItemIdx = Sections[SectionIdx].SubItems.Count - 1) or
      ((SubItemIdx = -1) and (Key in [VK_UP, VK_LEFT])) then
    begin
      case Key of
        VK_DOWN, VK_RIGHT: if (SectionIdx < Sections.Count - 1) then l := SectionIdx + 1;  // Correct 'last position' index
        VK_UP, VK_LEFT:    if (SectionIdx > 0) then l := SectionIdx - 1;  // Correct 'last position' index
      end;

      if not ContractDisable then
      begin
        for k := 0 to Sections.Count - 1 do        // Collaps last section and expand new selection
          if k <> l then Sections[k].State := lssContracted;
      end;

      Sections[l].State := lssExpanded;

      {$IFDEF DELPHI6_LVL}
      if not ContractDisable then
        SetItemIndex(SectionIdx);                   // Reset last index to current
      {$ENDIF}

      if Assigned(FOnSectionClick) then
        FOnSectionClick(Self, l);                // Selection, must be implemented!
    end;
  end;

  if key = vk_space then
  begin
    if (SendMessage(handle, lb_getitemdata, idx, 0) = 0) then
    begin
      StartEdit(idx);
    end;
  end;

  if key = vk_escape then
  begin
    if MultiSelect then for i := 1 to Items.Count do Selected[i - 1] := False else ItemIndex := -1;
  end;

  if key = vk_insert then
  begin
    if (SendMessage(handle, lb_getitemdata, idx, 0) = 0) or

      ((sections.items[SectionIdx] as TListSection).SubItems.Count = 0) then
    begin
      if Assigned(FOnInsertSubItem) then
      begin
        FOnInsertSubItem(self, SectionIdx, subitemidx, itemstr);
        if (itemstr <> '') then
        begin
          if subitemidx < 0 then
          begin
            (fSections.Items[SectionIdx] as TListSection).SubItems.Add(ItemStr);
            self.items[ItemIndex] := self.items[ItemIndex]; {force repaint}
          end
          else
            (fSections.Items[SectionIdx] as TListSection).SubItems.Insert(subitemidx, ItemStr);
          itemindex := idx;
        end;
      end;
    end
    else
    begin
      if Assigned(FOnInsertSection) then
      begin
        ListSection := (fSections.Add as TListSection);
        FOnInsertSection(self, SectionIdx, ListSection);
      end;
    end;
  end;

  if key = vk_delete then
  begin
    allow := False;
    if (SendMessage(handle, lb_getitemdata, idx, 0) = 1) then
    begin
      if Assigned(FOnDeleteSection) then
      begin
        FOnDeleteSection(self, SectionIdx, allow);
        if allow then
        begin
          (fSections.Items[SectionIdx] as TListSection).Free;
          itemindex := idx;
        end;
      end;
    end
    else
    begin
      if Assigned(FOnDeleteSubItem) then
      begin
        FOnDeleteSubItem(self, SectionIdx, subitemidx, allow);
        if allow then
        begin
          (fsections.Items[SectionIdx] as TListSection).SubItems.Delete(subitemidx);
          itemindex := idx;
        end;
      end;
    end;
  end;

end;

procedure TSectionListBox.Keypress(var ch: char);
var
  idx, i, j: Integer;
  ListSection: TListSection;
begin
  inherited;

  idx := Itemindex;
  if ch = #32 then
  begin
    i := GetItemSectionIndex(idx);
    ListSection := GetItemSection(idx);
    if (SendMessage(Handle, LB_GETITEMDATA, idx, 0) = 1) then
    begin
      if (ListSection.State = lssExpanded) and FContractDisable then Exit;
      if (ListSection.State = lssContracted) and FExpandDisable then Exit;
      if not ListSection.Fixed then
        ToggleSectionState(idx)
    end
    else
    begin
      j := GetSectionListIndex(i);
      if (ListSection.ControlType = scCheckbox) and not (ListSection.ReadOnly) then
      begin
        ListSection.SubItemCheckState[idx - j - 1] := not ListSection.SubItemCheckState[idx - j - 1];
        SendMessage(Handle, lb_setcursel, idx, 0);
        if Assigned(FOnCheckBoxClick) then
          FOnCheckBoxClick(Self, i, idx - j - 1);
      end;

      if (ListSection.ControlType = scRadiobutton) and not (ListSection.ReadOnly) then
      begin
        ListSection.RadioIndex := idx - j - 1;
        if Assigned(FOnRadioClick) then
          FOnRadioClick(Self, i, idx - j - 1);
      end;

      if Assigned(fOnSubItemClick) then
      begin
        FOnSubItemClick(Self, i, idx - j - 1);
      end;
    end;
  end;
end;


procedure TSectionListBox.UpdateHeight(Index: Integer);
var
  ln: Integer;
  ls: TListSection;
  s: string;
  idx,h: integer;
begin
  ls := GetItemSection(Index - 1);
  if not Assigned(ls) then
    Exit;

  if SendMessage(Handle, LB_GETITEMDATA, Index - 1, 0) = 0 then
  begin
    if ls.FontUsage in [fuSubItems, fuBoth] then
      Canvas.Font.Assign(ls.Font);

    s := GetDisplText(ls, Index - 1);

    ln := LinesInText(s, ls.AutoSize, ls.Itemheight);

    if SendMessage(Handle, LB_GETITEMHEIGHT, Index - 1, 0) <> ln then
      SendMessage(Handle, LB_SETITEMHEIGHT, Index - 1, ln);
  end
  else
  begin
    idx := ls.Index;
    h := GetCustomSectionHeight(idx);

    if SendMessage(Handle, LB_GETITEMHEIGHT, Index - 1, 0) <> h then
    begin
      SendMessage(Handle, LB_SETITEMHEIGHT, Index - 1, h)
    end;
  end;
end;

procedure TSectionListBox.UpdateItemHeight;
var
  i, ln: Integer;
  ls: TListSection;
  s: string;
  idx,h: integer;
begin
  for i := 1 to Items.Count do
  begin
    ls := GetItemSection(i - 1);

    if SendMessage(Handle, LB_GETITEMDATA, i - 1, 0) = 0 then
    begin
      if ls.FontUsage in [fuSubItems, fuBoth] then
        Canvas.Font.Assign(ls.Font);

      s := GetDisplText(ls, i - 1);

      ln := LinesInText(s, ls.AutoSize, ls.Itemheight);
      SendMessage(Handle, LB_SETITEMHEIGHT, i - 1, ln);
    end
    else
    begin
      idx := ls.Index;
      h := GetCustomSectionHeight(idx);
      SendMessage(Handle, LB_SETITEMHEIGHT, i - 1, h);
    end;
  end;
  Invalidate;
end;

procedure TSectionListBox.Loaded;
begin
  inherited Loaded;
  FOldCursor := self.Cursor;

  UpdateHorzScrollBar;
  UpdateItemHeight;
  Wallpaperchanged;
  UpdateVScrollBar;
  UpdateHScrollBar;
  UpdateStyle;
  UpdateColor;
  UpdateWidth;
  Height := Height + 1;
  Height := Height - 1;
end;

procedure TSectionListBox.ContractAll;
var
  i: Integer;
begin
  BeginUpdate;
  ItemIndex := 0;
  for i := 1 to FSections.Count do
    FSections.Items[i - 1].State := lssContracted;
  EndUpdate;
end;

procedure TSectionListBox.SortAll;
var
  i: Integer;
begin
  if Sections.Count = 0 then
    Exit;

  BeginUpdate;
  QuickSortSections(0, Sections.Count - 1);
  for i := 1 to FSections.Count do
    FSections.Items[i - 1].SortSubItems;
  EndUpdate;
end;

procedure TSectionListBox.SortAllSubItems;
var
  i: Integer;
begin
  BeginUpdate;
  for i := 1 to FSections.Count do
    FSections.Items[i - 1].SortSubItems;
  EndUpdate;
end;

procedure TSectionListBox.QuickSortSections(left, right: Integer);
var
  i, j: Integer;
  s: string;
begin
  i := left;
  j := right;

  s := Sections.Items[(left + right) shr 1].Caption;

  repeat
    while (s > Sections.Items[i].Caption) do inc(i);
    while (s < Sections.Items[j].Caption) do dec(j);
    if (i <= j) then
    begin
      if (i <> j) then self.Sections.SwapSections(i, j);
      inc(i);
      dec(j);
    end;
  until (i > j);

  if (left < j) then QuicksortSections(left, j);
  if (i < right) then QuickSortSections(i, right);
end;


procedure TSectionListBox.SortAllSections;
begin
  if Sections.Count = 0 then
    Exit;

  BeginUpdate;
  QuickSortSections(0, self.Sections.Count - 1);
  EndUpdate;
end;

procedure TSectionListBox.ExpandAll;
var
  i: Integer;
  SectionIdx, subitemidx: Integer;
begin
  BeginUpdate;
  if (itemindex <> -1) then
    GetListItemIndex(itemindex, SectionIdx, subitemidx);

  for i := 1 to FSections.Count do
    FSections.Items[i - 1].State := lssExpanded;

  EndUpdate;

  if ItemIndex <> -1 then
  begin
    i := GetSectionListIndex(SectionIdx);
    if SubItemIdx >= 0 then
      i := i + SubItemIdx;
    ItemIndex := i;
  end;
end;

procedure TSectionListBox.SetWallpaper(Value: TBitmap);
begin
  FWallpaper.Assign(Value);
  WallPaperChanged;
end;


procedure TSectionListBox.SetFlat(const value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TSectionListBox.SetFullFocus(const Value: Boolean);
begin
  if (FFullFocus <> Value) then
  begin
    FFullFocus := Value;
    Invalidate;
  end;
end;

procedure TSectionListBox.SetSectionColor(const Value: tColor);
begin
  FSectionColor := Value;
  Dopaint;
end;

procedure TSectionListBox.SetOneExpanded(const value: Boolean);
var
  i: Integer;
begin
  FOneExpanded := value;
  if Value then
  begin
    for i := 1 to Sections.Count do
    begin
      if i = 1 then
        Sections.Items[i - 1].State := lssExpanded
      else
        Sections.Items[i - 1].State := lssContracted;
    end;
  end;
end;


procedure TSectionListBox.SetWordWrap(const value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    if Value then
      FDtStyle := DT_WORDBREAK
    else
      FDtStyle := 0;

    UpdateItemHeight;
    Repaint;
  end;
end;

procedure TSectionListBox.SetSectionFont(value: tFont);
begin
  FSectionFont.Assign(value);
end;

function TSectionListBox.GetSectionHeight: Integer;
begin
  if FSectionHeight > 0 then
    result := FSectionHeight
  else
    Result := 14;
end;

procedure TSectionListBox.SetSectionHeight(value: Integer);
begin
  FSectionHeight := value;
  UpdateItemHeight;
end;

procedure TSectionListBox.SetSectionIndent(const Value: integer);
begin
  FSectionIndent := Value;
  Repaint;
end;

procedure TSectionListBox.SetSubItemIndent(const Value: integer);
begin
  FSubItemIndent := Value;
  Repaint;
end;

procedure TSectionListBox.SetImageSpacing(const Value: smallint);
begin
  if (value <> FImageSpacing) then
  begin
    FImageSpacing := value;
    Repaint;
  end;
end;

procedure TSectionListBox.SetContainer(const Value: TPictureContainer);
begin
  FContainer := Value;
  UpdateItemHeight;
end;


procedure TSectionListBox.SetSubItemHeight(const Value: integer);
var
  i: Integer;
begin

  if (value <> fSubItemHeight) then
  begin
    if (csDesigning in ComponentState) then
    begin
      for i := 1 to sections.Count do sections.Items[i - 1].itemheight := value;
    end;

    FSubItemHeight := value;
    UpdateItemHeight;
    Repaint;
  end;
end;

procedure TSectionListBox.SetSectionImages(const Value: TImageList);
begin
  FSectionImages := Value;
end;

procedure TSectionListBox.SetSubItemImages(const Value: TImageList);
begin
  FSubItemImages := Value;
end;

procedure TSectionListBox.SetContractGlyph(const Value: tbitmap);
begin
  fContractGlyph.Assign(Value);
  self.Repaint;
end;

procedure TSectionListBox.SetExpandGlyph(const Value: tbitmap);
begin
  fExpandGlyph.Assign(Value);
  self.Repaint;
end;



procedure TSectionListBox.SetURLSettings(value: tURLSettings);
begin
  fURLSettings.Assign(value);
end;

procedure TSectionListBox.SaveToInifile(filename: string);
var
  IniFile: TMemIniFile;
  i, j: Integer;
  key, value: string;
begin
  IniFile := TMemIniFile.Create(filename);
  for i := 1 to sections.Count do
  begin
    for j := 1 to sections.items[i - 1].Subitems.Count do
    begin
      value := sections.items[i - 1].Subitems[j - 1];
      if (pos('=', value) > 0) then
        key := copy(value, 1, pos('=', value) - 1);
      system.delete(value, 1, pos('=', value));
      inifile.writestring(sections.items[i - 1].Caption, key, value);
    end;
  end;
  IniFile.UpdateFile;
  IniFile.Free;
end;

procedure TSectionListBox.LoadFromInifile(filename: string);
var
  Inifile: TMemInifile;
  sec, values: TStringList;
  i: Integer;
begin
  inifile := TMemIniFile.Create(filename);
  sec := TStringList.Create;
  values := TStringList.Create;

  Inifile.ReadSections(sec);
  BeginUpdate;
  for i := 1 to sec.Count do
  begin
    Values.Clear;
    IniFile.ReadSectionValues(sec.Strings[i - 1], values);
    with Sections.Add do
    begin
      Caption := sec.Strings[i - 1];
      Subitems.Assign(values);
    end;
  end;
  Sec.Free;
  Values.Free;
  Inifile.Free;
  EndUpdate;
end;

procedure TSectionListBox.SaveToFile(filename: string);
const
  ctrltype: array[TSectionControlType] of integer = (0, 1, 2);
  ctrlval: array[boolean] of integer = (0, 1);
  secstate: array[TListSectionState] of integer = (0, 1);

var
  Inifile: TMemIniFile;
  i, j: Integer;
  section: TListSection;
begin
  if (pos('\', Filename) = 0) then
    filename := '.\' + Filename;

  inifile := TMemInifile.Create(filename);

  inifile.WriteInteger('SECTIONS', 'COUNT', fSections.Count);

  for i := 1 to fSections.Count do
  begin
    section := (fSections.Items[i - 1] as TListSection);
    inifile.WriteString('SECTION' + inttostr(i), 'CAPTION', section.caption);
    inifile.WriteInteger('SECTION' + inttostr(i), 'SUBITEMCOUNT', section.subitems.Count);
    inifile.WriteInteger('SECTION' + inttostr(i), 'CONTROL', ctrltype[section.controltype]);
    inifile.WriteInteger('SECTION' + inttostr(i), 'RADIO', section.radioindex);
    inifile.WriteInteger('SECTION' + inttostr(i), 'STATE', secstate[section.state]);
    inifile.WriteInteger('SECTION' + inttostr(i), 'READONLY', ctrlval[section.readonly]);
    inifile.WriteInteger('SECTION' + inttostr(i), 'SORTSHOW', ctrlval[section.sortshow]);
    case section.SortDirection of
      sdNone: inifile.WriteInteger('SECTION' + inttostr(i), 'SORTDIR', 0);
      sdAscending: inifile.WriteInteger('SECTION' + inttostr(i), 'SORTDIR', 1);
      sdDescending: inifile.WriteInteger('SECTION' + inttostr(i), 'SORTDIR', 2);
    end;
    inifile.WriteInteger('SECTION' + inttostr(i), 'COLOR', integer(section.Color));
    for j := 1 to section.Subitems.Count do
    begin
      inifile.WriteString('SECTION' + inttostr(i), 'SUBITEM' + inttostr(j), section.subitems[j - 1]);
      inifile.WriteInteger('SECTION' + inttostr(i), 'SUBVAL' + inttostr(j), ctrlval[section.SubItemCheckState[j - 1]]);
    end;
  end;

  inifile.UpdateFile;

  inifile.Free;
end;

procedure TSectionListBox.LoadFromFile(filename: string);
var
  inifile: TMemIniFile;
  i, j, nse, nsu: Integer;
  section: tlistsection;

begin
  FSections.Clear;

  if (pos('\', filename) = 0) then
    filename := '.\' + filename;

  inifile := TMemIniFile.Create(filename);

  nse := inifile.ReadInteger('SECTIONS', 'COUNT', 0);

  for i := 1 to nse do
  begin
    section := (fSections.Add as TListSection);
    section.Caption := inifile.ReadString('SECTION' + inttostr(i), 'CAPTION', '');
    case inifile.ReadInteger('SECTION' + inttostr(i), 'CONTROL', 0) of
      0: section.ControlType := scText;
      1: section.ControlType := scCheckbox;
      2: section.ControlType := scRadioButton;
    end;

    nsu := Inifile.ReadInteger('SECTION' + inttostr(i), 'SUBITEMCOUNT', 0);
    for j := 1 to nsu do
    begin
      section.Subitems.Add(inifile.ReadString('SECTION' + inttostr(i), 'SUBITEM' + inttostr(j), ''));
      section.SubItemCheckState[j - 1] := inifile.ReadInteger('SECTION' + inttostr(i), 'SUBVAL' + inttostr(j), 0) = 1;
    end;

    section.RadioIndex := inifile.ReadInteger('SECTION' + inttostr(i), 'RADIO', 0);
    section.ReadOnly := inifile.ReadInteger('SECTION' + inttostr(i), 'READONLY', 0) = 1;
    section.SortShow := inifile.ReadInteger('SECTION' + inttostr(i), 'SORTSHOW', 0) = 1;
    case inifile.ReadInteger('SECTION' + inttostr(i), 'SORTDIR', 0) of
      0: section.SortDirection := sdNone;
      1: section.SortDirection := sdAscending;
      2: section.SortDirection := sdDescending;
    end;
    section.Color := TColor(inifile.ReadInteger('SECTION' + inttostr(i), 'COLOR', integer(clWindow)));
    case inifile.ReadInteger('SECTION' + inttostr(i), 'STATE', 0) of
      0: section.State := lssExpanded;
      1: section.State := lssContracted;
    end;
  end;
  inifile.Free;
end;

procedure TSectionListBox.Assign(Source: TPersistent);
begin
  inherited;
end;

procedure TSectionListBox.BeginUpdate;
begin
  Inc(FUpdateCount);
  if FUpdateCount = 1 then
    SendMessage(Handle, WM_SETREDRAW, 0, 0);
end;

procedure TSectionListBox.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      SendMessage(Handle, WM_SETREDRAW, 1, 0);

      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
      UpdateHorzScrollbar;
    end;
  end;
end;

procedure TSectionListBox.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FSectionImages) then
    FSectionImages := nil;
  if (AOperation = opRemove) and (AComponent = FSubItemImages) then
    FSubItemImages := nil;
  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;

  inherited;
end;

procedure TSectionListBox.HilightInList(HiText: string; DoCase: Boolean);
var
  i, j: Integer;
begin
  BeginUpdate;
  for i := 1 to Sections.Count do
    with Sections.Items[i - 1] do
    begin
      Caption := Hilight(Caption, HiText, 'hi', DoCase);
      for j := 1 to SubItems.Count do
        SubItems[j - 1] := Hilight(SubItems[j - 1], HiText, 'hi', DoCase);
    end;
  EndUpdate;
end;

procedure TSectionListBox.HiLightInSection(SectionIdx: Integer;
  HiText: string; DoCase: Boolean);
var
  j: Integer;
begin
  if (SectionIdx >= Sections.Count) or (SectionIdx < 0) then
    Exit;

  BeginUpdate;
  with Sections.Items[SectionIdx] do
  begin
    Caption := Hilight(Caption, HiText, 'hi', DoCase);
    for j := 1 to SubItems.Count do
      SubItems[j - 1] := Hilight(SubItems[j - 1], HiText, 'hi', DoCase);
  end;
  EndUpdate;
end;

procedure TSectionListBox.HilightInSubItem(SectionIdx, SubItemIdx: Integer;
  HiText: string; DoCase: Boolean);
begin
  if (SectionIdx >= Sections.Count) or (SectionIdx < 0) then
    Exit;

  with Sections.Items[SectionIdx] do
  begin
    if (SubItemIdx < 0) or (SubItemIdx >= SubItems.Count) then
      Exit;
    SubItems[SubItemIdx] := Hilight(SubItems[SubItemIdx], HiText, 'hi', DoCase);
  end;
end;

procedure TSectionListBox.MarkInList(HiText: string; DoCase: Boolean);
var
  i, j: Integer;
begin
  BeginUpdate;
  for i := 1 to Sections.Count do
    with Sections.Items[i - 1] do
    begin
      Caption := Hilight(Caption, HiText, 'e', DoCase);
      for j := 1 to SubItems.Count do
        SubItems[j - 1] := Hilight(SubItems[j - 1], HiText, 'e', DoCase);
    end;
  EndUpdate;
end;

procedure TSectionListBox.MarkInSection(SectionIdx: Integer;
  HiText: string; DoCase: Boolean);
var
  j: Integer;
begin
  if (SectionIdx >= Sections.Count) or (SectionIdx < 0) then
    Exit;

  BeginUpdate;
  with Sections.Items[SectionIdx] do
  begin
    Caption := Hilight(Caption, HiText, 'e', DoCase);
    for j := 1 to SubItems.Count do
      SubItems[j - 1] := Hilight(SubItems[j - 1], HiText, 'e', DoCase);
  end;
  EndUpdate;
end;

procedure TSectionListBox.MarkInSubItem(SectionIdx, SubItemIdx: Integer;
  HiText: string; DoCase: Boolean);
begin
  if (SectionIdx >= Sections.Count) or (SectionIdx < 0) then
    Exit;

  with Sections.Items[SectionIdx] do
  begin
    if (SubItemIdx < 0) or (SubItemIdx >= SubItems.Count) then
      Exit;
    SubItems[SubItemIdx] := Hilight(SubItems[SubItemIdx], HiText, 'e', DoCase);
  end;
end;

procedure TSectionListBox.UnHilightInList;
var
  i, j: Integer;
begin
  BeginUpdate;
  for i := 1 to Sections.Count do
    with Sections.Items[i - 1] do
    begin
      Caption := UnHilight(Caption, 'hi');
      for j := 1 to SubItems.Count do
        SubItems[j - 1] := UnHilight(SubItems[j - 1], 'hi');
    end;
  EndUpdate;
end;

procedure TSectionListBox.UnHiLightInSection(SectionIdx: Integer);

var
  j: Integer;
begin
  if (SectionIdx >= Sections.Count) or (SectionIdx < 0) then
    Exit;

  BeginUpdate;
  with Sections.Items[SectionIdx] do
  begin
    Caption := UnHilight(Caption, 'hi');
    for j := 1 to SubItems.Count do
      SubItems[j - 1] := UnHilight(SubItems[j - 1], 'hi');
  end;
  EndUpdate;

end;

procedure TSectionListBox.UnHilightInSubItem(SectionIdx, SubItemIdx: Integer);
begin
  if (SectionIdx >= Sections.Count) or (SectionIdx < 0) then
    Exit;
  with Sections.Items[SectionIdx] do
  begin
    if (SubItemIdx < 0) or (SubItemIdx >= SubItems.Count) then
      Exit;
    SubItems[SubItemIdx] := UnHilight(SubItems[SubItemIdx], 'hi');
  end;
end;

procedure TSectionListBox.UnMarkInList;
var
  i, j: Integer;
begin
  BeginUpdate;
  for i := 1 to Sections.Count do
    with Sections.Items[i - 1] do
    begin
      Caption := UnHilight(Caption, 'e');
      for j := 1 to SubItems.Count do
        SubItems[j - 1] := UnHilight(SubItems[j - 1], 'e');
    end;
  EndUpdate;
end;

procedure TSectionListBox.UnMarkInSection(SectionIdx: Integer);
var
  j: Integer;
begin
  if (SectionIdx >= Sections.Count) or (SectionIdx < 0) then
    Exit;

  BeginUpdate;
  with Sections.Items[SectionIdx] do
  begin
    Caption := UnHilight(Caption, 'e');
    for j := 1 to SubItems.Count do
      SubItems[j - 1] := UnHilight(SubItems[j - 1], 'e');
  end;
  EndUpdate;

end;

procedure TSectionListBox.UnMarkInSubItem(SectionIdx, SubItemIdx: Integer);
begin
  if (SectionIdx >= Sections.Count) or (SectionIdx < 0) then
    Exit;
  with Sections.Items[SectionIdx] do
  begin
    if (SubItemIdx < 0) or (SubItemIdx >= SubItems.Count) then
      Exit;
    SubItems[SubItemIdx] := UnHilight(SubItems[SubItemIdx], 'e');
  end;
end;

procedure TSectionListBox.WMPaint(var Msg: TWMPaint);
var
  j, h: Integer;
  r: TRect;
begin
  h := 0;

  if (FUpdateCount = 0) and FWallpaper.Empty then
  begin
    j := SendMessage(Handle, LB_GETTOPINDEX, 0,0);
    while (j < Items.Count) do
    begin
      h := h + SendMessage(Handle, lb_getitemheight, j, 0);
      inc(j);
    end;
  end;

  inherited;

  if (FUpdateCount = 0) and FWallpaper.Empty then
  begin
    if (h < Height) then
    begin
      r := GetClientRect;
      Canvas.Brush.Color := self.Color;
      Canvas.Pen.Color := self.Color;
      Canvas.Rectangle(0, h, r.Right, r.Bottom - 1);
    end;
  end;

end;

function TSectionListBox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TSectionListBox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TSectionListBox.SetVersion(const Value: string);
begin

end;

{ TListSection }

procedure TListSection.AddSubItems(const S: string);
begin
  {only add to the list if section in expanded state}
  if FState = lssContracted then
    Exit;
  (Collection as TListSectionCollection).AddToSection(self, S);
end;

procedure TListSection.ClearSubItems;
begin
  if FState = lssContracted then
    Exit;
  (Collection as TListSectionCollection).ClearSection(self);
end;

procedure TListSection.QuickSortItems(left, right: Integer);
var
  i, j: Integer;
  s, sw: string;
  o: TObject;
begin
  i := left;
  j := right;

  s := SubItems.Strings[(left + right) shr 1];

  repeat
    if (fSortDirection = sdDescending) then
    begin
      while (s < self.SubItems.Strings[i]) and (i < right) do inc(i);
      while (s > self.SubItems.Strings[j]) and (j > left) do dec(j);
    end
    else
    begin
      while (s > self.SubItems.Strings[i]) and (i < right) do inc(i);
      while (s < self.SubItems.Strings[j]) and (j > left) do dec(j);
    end;

    if (i <= j) then
    begin
      if (i <> j) then
      begin
        sw := SubItems.Strings[i];
        o := SubItems.Objects[i];
        SubItems.Strings[i] := SubItems.Strings[j];
        SubItems.Objects[i] := SubItems.Objects[j];
        SubItems.Strings[j] := sw;
        SubItems.Objects[j] := o;
      end;
      inc(i);
      dec(j);
    end;
  until (i > j);

  if left < j then
    QuicksortItems(left, j);
  if i < right then
    QuickSortItems(i, right);
end;

procedure TListSection.Focus;
var
  i: Integer;
begin
  i := (Collection as TListSectionCollection).GetListIndex(self);
  with ((Collection as TListSectionCollection).ListOwner) do
    SendMessage(Handle, LB_SETTOPINDEX, i, 0);
end;

procedure TListSection.SortSubItems;
begin
  if FState = lssExpanded then
  begin
    State := lssContracted;
    if FSubItems.Count > 0 then
      QuickSortItems(0, FSubItems.Count - 1);
    State := lssExpanded;
  end
  else
  begin
    if FSubItems.Count > 0 then
      QuickSortItems(0, fSubItems.Count - 1);
  end;
end;

constructor TListSection.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSubItems := TSectionListStrings.Create;
  (FSubItems as TSectionListStrings).fListSection := Self;
  (FSubItems as TSectionListStrings).OnChange := SubItemsChanged;
  FAutoSize := True;
  FAutoEdit := False;
  FState := lssContracted;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FReadOnly := True;
  FImageIndex := -1;

  (Collection as TListSectionCollection).InsertSection(self);
  with ((Collection as TListSectionCollection).GetOwner as TSectionListBox) do
  begin
    FColor := Color;
    FItemheight := FSubItemheight;
  end;
end;

procedure TListSection.Assign(Source: TPersistent);
begin
  Caption := (Source as TListSection).Caption;
  SubItems.Assign((Source as TListSection).SubItems);
  Alignment := (Source as TListSection).Alignment;
  Color := (Source as TListSection).Color;
  ControlType := (Source as TListSection).ControlType;
  EndEllipsis := (Source as TListSection).EndEllipsis;
  Fixed := (Source as TListSection).Fixed;
  RadioIndex := (Source as TListSection).RadioIndex;
  FontUsage := (Source as TListSection).FontUsage;
  Font.Assign((Source as TListSection).Font);
  ImageIndex := (Source as TListSection).ImageIndex;
  Lines := (Source as TListSection).Lines;
  State := (Source as TListSection).State;
end;

procedure TListSection.FontChanged(Sender: TObject);
begin
  ((Collection as TListSectionCollection).GetOwner as TSectionListBox).Dopaint;
end;

procedure TListSection.SubItemsChanged(Sender: TObject);
var
  i, j: Integer;
begin
  with ListBox do
  begin
    if FUpdateCount > 0 then
      Exit;

    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      if State = lssExpanded then
      begin
        i := (Collection as TListSectionCollection).GetListIndex(self);

        for j := i + 1 to i + SubItems.Count do
        begin
          if Items[j] <> SubItems[j - (i + 1)] then
          begin
            Items[j] := SubItems[j - (i + 1)]
          end;
        end;
      end;
    end;
  end;
end;

procedure TListSection.DeleteSubItems(index: integer);
begin
  if FState = lssContracted then
    Exit;
  (Collection as TListSectionCollection).DeleteFromSection(self, index);
end;

destructor TListSection.Destroy;
begin
  (Collection as TListSectionCollection).RemoveSection(self);
  FSubItems.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TListSection.InsertSubItems(index: integer; const S: string);
begin
  if (FState = lssContracted) then
    Exit;
  (Collection as TListSectionCollection).InsertInSection(self, index, S);
end;


function TListSection.ListBox: TSectionListBox;
begin
  Result := ((Collection as TListSectionCollection).GetOwner as TSectionListBox)
end;

function TListSection.GetDisplayName: string;
begin
  Result := FCaption;
end;

procedure TListSection.SetAlignment(const value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    ((Collection as TListSectionCollection).GetOwner as TSectionListBox).Dopaint;
  end;
end;

procedure TListSection.SetVAlignment(const value: TVAlignment);
begin
  if Value <> FVAlignment then
  begin
    FVAlignment := Value;
    ((Collection as TListSectionCollection).GetOwner as TSectionListBox).Dopaint;
  end;
end;


procedure TListSection.SetAutoSize(const Value: Boolean);
begin
  if value <> FAutoSize then
  begin
    FAutoSize := Value;
  end;
end;


procedure TListSection.SetCaption(const Value: string);
begin
  FCaption := Value;
  (Collection as TListSectionCollection).UpdateSection(self);
end;

procedure TListSection.SetFixed(const value: Boolean);
begin
  FFixed := value;
  ((Collection as TListSectionCollection).GetOwner as TSectionListBox).Dopaint;
end;

procedure TListSection.SetFont(Value: tFont);
begin
  fFont.Assign(value);
  ((Collection as TListSectionCollection).GetOwner as TSectionListBox).Dopaint;
end;

procedure TListSection.SetFontUsage(Value: tFontUsage);
begin
  fFontUsage := value;
  ((Collection as TListSectionCollection).GetOwner as TSectionListBox).Dopaint;
end;

procedure TListSection.SetEndEllipsis(Value: Boolean);
begin
  fEndEllipsis := value;
  ((Collection as TListSectionCollection).GetOwner as TSectionListBox).Dopaint;
end;

procedure TListSection.SetRadioIndex(Value: Integer);
begin
  fRadioindex := value;
  ((Collection as TListSectionCollection).GetOwner as TSectionListBox).Dopaint;
end;

procedure TListSection.SetSortDirection(value: TSortDirection);
begin
  FSortDirection := Value;
  if (FSubItems.Count > 0) and (Value <> sdNone) then
  begin
    if State = lssExpanded then
    begin
      ((Collection as TListSectionCollection).GetOwner as TSectionListBox).BeginUpdate;
      State := lssContracted;
      if FSubItems.Count > 0 then
        QuickSortItems(0, fSubItems.Count - 1);
      State := lssExpanded;
      ((Collection as TListSectionCollection).GetOwner as TSectionListBox).EndUpdate;
    end
    else
    begin
      if FSubItems.Count > 0 then
        QuickSortItems(0, fSubItems.Count - 1);
    end;
  end;
end;

procedure TListSection.SetSortShow(value: boolean);
begin
  FSortShow := value;
  ((Collection as TListSectionCollection).GetOwner as TSectionListBox).Dopaint;
end;

procedure TListSection.SetReadOnly(value: boolean);
begin
  FReadOnly := value;
  ((Collection as TListSectionCollection).GetOwner as TSectionListBox).Dopaint;
end;

procedure TListSection.SetOwnerDraw(value: boolean);
begin
  FOwnerDraw := value;
  ((Collection as TListSectionCollection).GetOwner as TSectionListBox).Dopaint;
end;

procedure TListSection.SetItemHeight(value: integer);
begin
  FItemheight := Value;
  if state = lssExpanded then
  begin
    state := lssContracted;
    state := lssExpanded;
  end;
end;

procedure TListSection.SetControlType(Value: TSectionControlType);
begin
  FControlType := value;
  ((Collection as TListSectionCollection).GetOwner as TSectionListBox).Dopaint;
end;


procedure TListSection.SetColor(const Value: tColor);
begin
  fColor := value;
  ((Collection as TListSectionCollection).GetOwner as TSectionListBox).Dopaint;
end;

procedure TListSection.SetImageIndex(const Value: integer);
begin
  FImageIndex := value;
  ((Collection as TListSectionCollection).GetOwner as TSectionListBox).Dopaint;
end;

procedure TListSection.SetState(const Value: TListSectionState);
begin
  if (FState <> Value) then
  begin
    ((Collection as TListSectionCollection).GetOwner as TSectionListBox).BeginUpdate;
    if value = lssExpanded then
      (Collection as TListSectionCollection).ExpandSection(self)
    else
      (Collection as TListSectionCollection).ContractSection(self);
    FState := Value;
    ((Collection as TListSectionCollection).GetOwner as TSectionListBox).EndUpdate;
  end;
end;

procedure TListSection.SetSubItems(const Value: tstringlist);
var
  oldstate: TListSectionState;
begin
  oldstate := fState;
  State := lssContracted;
  FSubItems.Assign(Value);
  State := oldState;

end;

procedure TListSection.SetLines(const Value: TSectionLines);
begin
  fLines := value;
  ((Collection as TListSectionCollection).GetOwner as TSectionListBox).Dopaint;
end;

function TListSection.GetSubItemCheckState(i: integer): boolean;
begin
  if subitems.count = 0 then result := False
  else
  begin
    if (subitems.Count < i) or (i < 0) then
      result := False
    else
      result := ((integer(subitems.Objects[i])) and $F0000) > 0;
  end;
end;

{
procedure TListSection.SetSubItemCheckStateEx(i: integer; const Value: boolean);
var
 l: Integer;
begin
 if (subitems.Count<i) or (i<0) then
  raise ESectionListBoxError.Create('Access of imageindex of non-existing subitem')
 else
  begin
   l:=integer(subitems.Objects[i]);
   if value then
    subitems.Objects[i]:= tobject($10000 or (l and $FFFF))
   else
    subitems.Objects[i]:= tobject((l and $FFFF));
  end;
end;
}

procedure TListSection.SetSubItemCheckState(i: integer; const Value: boolean);
var
  l: Integer;
  r: trect;
  listhandle: thandle;
begin
  if (subitems.Count < i) or (i < 0) then
    raise ESectionListBoxError.Create('Access of imageindex of non-existing subitem')
  else
  begin
    (fSubItems as tSectionListStrings).OnChange := nil;

    l := integer(subitems.Objects[i]);

    if value then
      subitems.Objects[i] := tobject($10000 or (l and $FFFF))
    else
      subitems.Objects[i] := tobject((l and $FFFF));

    (fSubItems as tSectionListStrings).OnChange := SubItemsChanged;

    if state = lssExpanded then
    begin
      l := (Collection as TListSectionCollection).GetListIndex(self);

     {l:=((Collection as TListSectionCollection).fOwner as TSectionListBox).GetItemSectionIndex(self.Index);}
      listhandle := (Collection as TListSectionCollection).FOwner.Handle;
      SendMessage(listhandle, lb_getitemrect, l + i + 1, LParam(@r));
      invalidaterect(listhandle, @r, False);
    end;
  end;
end;

function TListSection.GetSubItemImageIdx(i: integer): smallint;
begin
  if subitems.count = 0 then result := -1
  else
  begin
    if (subitems.Count <= i) or (i < 0) then result := -1
    else
    begin
      result := (integer(subitems.Objects[i]) and $FFFF) - 1;
    end;
  end;
end;

procedure TListSection.SetSubItemImageIdx(i: integer; const Value: smallint);
var
  l: Integer;
  r: trect;
  listhandle: thandle;
begin
  if (subitems.Count < i) or (i < 0) then
    raise ESectionListBoxError.Create('Access of imageindex of non-existing subitem')
  else
  begin
    l := integer(subitems.Objects[i]) and $FFFF0000;


    (FSubItems as tSectionListStrings).OnChange := nil;

    subitems.Objects[i] := tobject(l or ($FFFF and (value + 1)));

    (FSubItems as tSectionListStrings).OnChange := SubItemsChanged;

    if state = lssExpanded then
    begin
      l := (Collection as TListSectionCollection).GetListIndex(self);

      {l:=((Collection as TListSectionCollection).fOwner as TSectionListBox).GetItemSectionIndex(self.Index);}
      listhandle := (Collection as TListSectionCollection).fOwner.handle;
      SendMessage(listhandle, lb_getitemrect, l + i + 1, LParam(@r));
      InvalidateRect(listhandle, @r, False);
    end;
  end;
end;

{ TListSectionCollection }

constructor TListSectionCollection.Create(aOwner: TSectionListBox);
begin
  inherited Create(CreateItemClass);
  FOwner := AOwner;
end;

function TListSectionCollection.GetListIndex(ASection: TListSection): integer;
var
  i, j, k: Integer;
begin
  Result := 0;

  if Count = 0 then Exit;

  i := 0;
  j := 0;

  while (j < Count) do
  begin
    if (items[j] = ASection) then break;
    inc(i);
    if ((items[j] as TListSection).State = lssExpanded) then
    begin
      k := (items[j] as TListSection).SubItems.Count;
      i := i + k;
    end;
    inc(j);
  end;

  Result := i;
end;

function TListSectionCollection.ExpandSection(ASection: TListSection): integer;
var
  i, j, ln: Integer;
  s: string;
begin
  Result := 0;
  if (ASection.State = lssExpanded) then Exit;

  i := GetListIndex(ASection);

  with (GetOwner as TSectionListBox) do
  begin
    BeginUpdate;
    FExpandContractBusy := True;
    if ASection.Subitems.Count > 0 then
    begin
      for j := 0 to ASection.Subitems.Count - 1 do
      begin
        Items.Insert(i + j + 1, ASection.SubItems[j]);
        if ASection.FontUsage in [fuSubitems, fuBoth] then
          Canvas.Font.Assign(ASection.Font);
        s := GetDisplText(ASection, i + j + 1);

        ln := LinesInText(s, ASection.AutoSize, ASection.fitemheight);

        SendMessage(Handle, LB_SETITEMHEIGHT, i + j + 1, ln);
      end;
    end;
    FExpandContractBusy := False;
    EndUpdate;
  end;
end;

function TListSectionCollection.ContractSection(ASection: TListSection): integer;
var
  i, j: Integer;
begin
  result := 0;
  if (ASection.State = lssContracted) then Exit;
  i := GetListIndex(ASection);

  with (GetOwner as TSectionListBox) do
  begin
    BeginUpdate;
    fExpandContractBusy := True;
    if ASection.Subitems.Count > 0 then
      for j := 0 to ASection.Subitems.Count - 1 do
        Items.Delete(i + 1);
    fExpandContractBusy := False;
    EndUpdate;
  end;

end;

function TListSectionCollection.InsertSection(ASection: TListSection): integer;
var
  i: Integer;
begin
  with ListOwner.Items do
  begin
    ListOwner.FExpandContractBusy := True;
    i := Add('');
    Result := i;
    SendMessage(ListOwner.Handle, LB_SETITEMDATA, Result, 1);
    SendMessage(ListOwner.Handle, LB_SETITEMHEIGHT, Result, ListOwner.SectionHeight);
    ListOwner.FExpandContractBusy := False;
  end;
end;

procedure TListSectionCollection.SwapSections(idx1, idx2: Integer);
var
  ls: TListSection;
begin
  ls := TListSection.Create(self);
  ls.Assign(Items[idx1]);
  Items[idx1].Assign(Items[idx2]);
  Items[idx2].Assign(ls);
  ls.Free;
end;

function TListSectionCollection.RemoveSection(
  ASection: TListSection): integer;
var
  i, j: Integer;
begin
  i := GetListIndex(ASection);
  with (GetOwner as TSectionListBox) do
  begin
    if not fDestroyed then
    begin
      Items.Delete(i);
      if ASection.State = lssExpanded then
        for j := 1 to ASection.SubItems.Count do
          Items.Delete(i);
    end;
  end;
  Result := i;
end;

function TListSectionCollection.UpdateSection(ASection: TListSection): integer;
var
  i, ln: Integer;
begin
  i := GetListIndex(ASection);
  ListOwner.Items[i] := ASection.Caption;

  if SendMessage(ListOwner.Handle, lb_getitemdata, i, 0) = 1 then
    SendMessage(ListOwner.Handle, lb_setitemheight, i, ListOwner.SectionHeight)
  else
  begin
    ln := ListOwner.LinesInText(ListOwner.Items[i], ASection.AutoSize, ASection.itemheight);
    SendMessage(ListOwner.Handle, lb_setitemheight, i, ln);
  end;

  Result := i;
end;

function TListSectionCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TListSectionCollection.AddToSection(ASection: TListSection;
  const S: string);
var
  i, ln: Integer;
begin
  i := GetListIndex(ASection);
  ListOwner.Items.Insert(i + ASection.SubItems.Count + 1, S);
  ln := ListOwner.LinesInText(s, ASection.AutoSize, ASection.itemheight);
  SendMessage(ListOwner.Handle, lb_Setitemheight, i + ASection.SubItems.Count + 1, ln);

  ListOwner.UpdateHorzScrollBar;
end;

procedure TListSectionCollection.ClearSection(ASection: TListSection);
var
  i, j: Integer;
begin
  i := GetListIndex(ASection);
  if (ASection.SubItems.Count > 0) and (ASection.state = lssExpanded) then
  begin
    ListOwner.FExpandContractBusy := True;
    for j := 0 to ASection.SubItems.Count - 1 do
      ListOwner.Items.Delete(i + 1);
    ListOwner.FExpandContractBusy := False;
  end;
  ListOwner.UpdateHorzScrollBar;
end;

procedure TListSectionCollection.DeleteFromSection(ASection: TListSection;
  index: integer);
begin
  Index := Index + GetListIndex(ASection);
  ListOwner.Items.Delete(index + 1);
  ListOwner.UpdateHorzScrollBar;
end;

procedure TListSectionCollection.InsertInSection(ASection: TListSection;
  index: integer; const S: string);
var
  ln: Integer;
begin
  Index := Index + GetListIndex(ASection);
  ListOwner.Items.Insert(index + 1, S);
  ln := ListOwner.LinesInText(s, ASection.AutoSize, ASection.itemheight);
  SendMessage(ListOwner.Handle, LB_SETITEMHEIGHT, index + 1, ln);
  ListOwner.UpdateHorzScrollBar;
end;

procedure TListSectionCollection.Update(Item: TCollectionItem);
var
  i, j, k, ln: Integer;
  ls: tlistsection;
begin
  //before update close all

  if (csDesigning in (GetOwner as TSectionListBox).ComponentState) then
  begin
    ListOwner.Items.Clear;
  end;

  inherited Update(Item);

  //rebuild sections to listbox
  if (csDesigning in (GetOwner as TSectionListBox).ComponentState) then
  begin
    for i := 1 to Count do
    begin
      j := ListOwner.Items.Add((self.Items[i - 1] as TListSection).Caption);
      SendMessage(ListOwner.Handle, lb_setitemdata, j, 1);

      SendMessage(ListOwner.Handle, lb_setitemheight, j,
        ListOwner.SectionHeight);

      if ((self.Items[i - 1] as TListSection).State = lssExpanded) then
      begin
        for j := 1 to (self.Items[i - 1] as TListSection).SubItems.Count do
        begin
          ls := Self.Items[i - 1];
          k := ListOwner.Items.Add((self.Items[i - 1] as TListSection).SubItems[j - 1]);
          ln := ListOwner.LinesInText((Items[i - 1] as TListSection).SubItems[j - 1], ls.AutoSize, ls.itemHeight);
          SendMessage(ListOwner.Handle, lb_setitemheight, k, ln);
        end;
      end;
    end;
  end;
  ListOwner.UpdateHorzScrollBar;
end;

function TListSectionCollection.GetItem(Index: Integer): TListSection;
begin
  Result := TListSection(inherited GetItem(Index));
end;

procedure TListSectionCollection.SetItem(Index: Integer; Value: TListSection);
begin
  inherited SetItem(Index, Value);
end;

function TListSectionCollection.Add: TListSection;
begin
  Result := TListSection(inherited Add);
end;

function TListSectionCollection.Insert(index: Integer): TListSection;
var
  i: Integer;
begin
  {we know an add operation happened}
  Result := TListSection(inherited Insert(index));
  i := GetListIndex(result);
  with (GetOwner as TSectionListBox) do
  begin
    Items.Insert(i, '');
    SendMessage(Handle, LB_SETITEMDATA, i, 1);

    SendMessage(Handle, LB_SETITEMHEIGHT, i, SectionHeight);
    Items.Delete(items.Count - 1);
  end;
end;


function TListSectionCollection.CreateItemClass: TCollectionItemClass;
begin
  Result := TListSection;
end;

function TListSectionCollection.IndexOf(SectionText: string): Integer;
var
  i: Integer;
begin
  i := 0;
  Result := -1;

  while i < Count do
  begin
    if Items[i].Caption = SectionText then
    begin
      Result := i;
      Break;
    end;
    inc(i);
  end;
end;

{ TSectionListStrings }

function TSectionListStrings.Add(const S: string): Integer;
begin
  FListSection.AddSubItems(s);
  Result := Count - 1;

  if (FListSection.State <> lssContracted) and (FListSection.ListBox.FUpdateCount = 0) then
    Exit;

  Result := inherited Add(S);
end;

procedure TSectionListStrings.Clear;
begin
  FListSection.ClearSubItems;
  inherited Clear;
end;

procedure TSectionListStrings.Delete(Index: Integer);
begin
  FListSection.DeleteSubItems(Index);
  inherited Delete(Index);
end;

procedure TSectionListStrings.Insert(Index: Integer; const S: string);
begin
  FListSection.InsertSubItems(Index, S);

  if (FListSection.State <> lssContracted) and (FListSection.ListBox.FUpdateCount = 0) then
    Exit;

  inherited Insert(Index, S);
end;

procedure TSectionListStrings.InsertObject(Index: Integer; const S: string; AObject: TObject);
begin
  FListSection.InsertSubItems(Index, S);

  if (FListSection.State <> lssContracted) and (FListSection.ListBox.FUpdateCount = 0) then
    Exit;

  inherited InsertObject(Index, S, AObject);
end;

{ TTabPositionCollection }

function TTabPositionCollection.Add: TTabPositionItem;
begin
  Result := TTabPositionItem(inherited Add);
end;

constructor TTabPositionCollection.Create(aOwner: TSectionListBox);
begin
  inherited Create(TTabPositionItem);
  fOwner := aOwner;
end;

function TTabPositionCollection.GetOwner: TPersistent;
begin
  result := fOwner;
end;

function TTabPositionCollection.GetTabPos(i: Integer): TTabPositionItem;
begin
  Result := TTabPositionItem(inherited GetItem(I));
end;

procedure TTabPositionCollection.SetTabPos(i: Integer; tabpos: TTabPositionItem);
begin
  inherited SetItem(I, tabpos);
end;


{ TTabPositionItem }

procedure TTabPositionItem.SetTabPosition(value: integer);
begin
  FTabPosition := Value;
  (collection as TTabPositionCollection).fOwner.Repaint;
  ((collection as TTabPositionCollection).fOwner as TSectionListBox).UpdateHorzScrollBar;
end;

procedure TTabPositionItem.SetTabType(value: tTabType);
begin
  fTabType := value;
  (collection as TTabPositionCollection).fOwner.Repaint;
  ((collection as TTabPositionCollection).fOwner as TSectionListBox).UpdateHorzScrollBar;
end;



{ TURLSettings }

constructor TURLSettings.Create;
begin
  inherited Create;
  fURLColor := clBlue;
end;

destructor TURLSettings.Destroy;
begin
  inherited Destroy;
end;

procedure TURLSettings.SetURLAware(value: boolean);
begin
  fURLAware := value;
  if Assigned(OnChange) then OnChange(self);
end;

procedure TURLSettings.SetURLColor(value: tcolor);
begin
  fURLColor := value;
  if Assigned(OnChange) then OnChange(self);
end;

procedure TURLSettings.SetURLFull(value: boolean);
begin
  fURLFull := value;
  if Assigned(OnChange) then OnChange(self);
end;


procedure TSectionListBox.SetNodeType(const Value: TListNodeType);
begin
  fNodeType := Value;
  self.Repaint;
end;

procedure TSectionListbox.FlatSetScrollPos(code, pos: integer; fRedraw: bool);
var
  ComCtl32DLL: THandle;
  _FlatSB_SetScrollPos: function(wnd: hwnd; code, pos: Integer; fRedraw: bool): Integer; stdcall;

begin
  ComCtl32DLL := GetModuleHandle(commctllib);
  if (ComCtl32DLL > 0) then
  begin
    @_FlatSB_SetScrollPos := GetProcAddress(ComCtl32DLL, 'FlatSB_SetScrollPos');
    if Assigned(_FlatSB_SetScrollPos) then
      _FlatSB_SetScrollPos(self.handle, code, pos, fRedraw);
  end;
end;

procedure TSectionListbox.FlatSetScrollInfo(code: integer; var scrollinfo: tscrollinfo; fRedraw: bool);
var
  ComCtl32DLL: THandle;
  _FlatSB_SetScrollInfo: function(wnd: hwnd; code: Integer; var scrollinfo: tscrollinfo; fRedraw: bool): Integer; stdcall;

begin
  ComCtl32DLL := GetModuleHandle(commctllib);
  if (ComCtl32DLL > 0) then
  begin
    @_FlatSB_SetScrollInfo := GetProcAddress(ComCtl32DLL, 'FlatSB_SetScrollInfo');
    if Assigned(_FlatSB_SetScrollInfo) then
    begin
      _FlatSB_SetScrollInfo(self.handle, code, scrollinfo, fRedraw);
    end;
  end;
end;

procedure TSectionListbox.FlatSetScrollProp(index, newValue: integer;
  fRedraw: bool);
var
  ComCtl32DLL: THandle;
  _FlatSB_SetScrollProp: function(wnd: hwnd; Index, newValue: Integer; fredraw: bool): bool stdcall;

begin
  ComCtl32DLL := GetModuleHandle(commctllib);
  if (ComCtl32DLL > 0) then
  begin
    @_FlatSB_SetScrollProp := GetProcAddress(ComCtl32DLL, 'FlatSB_SetScrollProp');
    if Assigned(_FlatSB_SetScrollProp) then
      _FlatSB_SetScrollProp(self.handle, index, newValue, fRedraw);
  end;
end;


procedure TSectionListbox.FlatShowScrollBar(code: Integer; show: bool);
var
  ComCtl32DLL: THandle;
  _FlatSB_ShowScrollBar: function(wnd: hwnd; code: Integer; show: bool): Integer; stdcall;

begin
  ComCtl32DLL := GetModuleHandle(commctllib);
  if (ComCtl32DLL > 0) then
  begin
    @_FlatSB_ShowScrollBar := GetProcAddress(ComCtl32DLL, 'FlatSB_ShowScrollBar');
    if Assigned(_FlatSB_ShowScrollBar) then
      _FlatSB_ShowScrollBar(self.handle, code, show);
  end;
end;

procedure TSectionListbox.SetScrollStyle(const Value: TSListScrollStyle);
var
  ComCtl32DLL: THandle;
  _InitializeFlatSB: function(wnd: hwnd): Bool stdcall;
  _UnInitializeFlatSB: function(wnd: hwnd): Bool stdcall;
begin
  if (value in [slsEncarta, slsFlat]) and
    (fScrollStyle = slsNormal) then
  begin
    ComCtl32DLL := GetModuleHandle(commctllib);
    if (ComCtl32DLL > 0) then
    begin
      @_InitializeFlatSB := GetProcAddress(ComCtl32DLL, 'InitializeFlatSB');
      if Assigned(_InitializeFlatSB) then
      begin
        _InitializeFlatSB(self.Handle);
      end;
    end;
  end;

  if (value = slsNormal) and
    (fScrollStyle in [slsEncarta, slsFlat]) then
  begin
    ComCtl32DLL := GetModuleHandle(commctllib);
    if (ComCtl32DLL > 0) then
    begin
      @_UnInitializeFlatSB := GetProcAddress(ComCtl32DLL, 'UnInitializeFlatSB');
      if Assigned(_UnInitializeFlatSB) then
      begin
        _UnInitializeFlatSB(self.Handle);
      end;
    end;
  end;

  FScrollStyle := Value;
  UpdateStyle;
end;


procedure TSectionListbox.SetScrollColor(const Value: TColor);
begin
  FScrollColor := Value;
  UpdateColor;
end;

procedure TSectionListbox.SetScrollWidth(const Value: integer);
begin
  FScrollWidth := Value;
  UpdateWidth;
end;

procedure TSectionListBox.UpdateStyle;
begin
  case FScrollStyle of
    slsNormal: FlatSetScrollProp(WSB_PROP_VSTYLE, FSB_REGULAR_MODE, True);
    slsFlat: FlatSetScrollProp(WSB_PROP_VSTYLE, FSB_FLAT_MODE, True);
    slsEncarta: FlatSetScrollProp(WSB_PROP_VSTYLE, FSB_ENCARTA_MODE, True);
  end;
  case FScrollStyle of
    slsNormal: FlatSetScrollProp(WSB_PROP_HSTYLE, FSB_REGULAR_MODE, True);
    slsFlat: FlatSetScrollProp(WSB_PROP_HSTYLE, FSB_FLAT_MODE, True);
    slsEncarta: FlatSetScrollProp(WSB_PROP_HSTYLE, FSB_ENCARTA_MODE, True);
  end;

end;

procedure TSectionListbox.UpdateColor;
begin
  FlatSetScrollPROP(WSB_PROP_VBKGCOLOR, longint(fScrollColor), True);
  FlatSetScrollPROP(WSB_PROP_HBKGCOLOR, longint(fScrollColor), True);
end;

procedure TSectionListbox.UpdateWidth;
begin
  FlatSetScrollPROP(WSB_PROP_CXVSCROLL, FScrollWidth, True);
  FlatSetScrollPROP(WSB_PROP_CYHSCROLL, FScrollWidth, True);
end;

procedure TSectionListbox.UpdateVScrollBar;
var
  Scrollinfo: TScrollInfo;
begin
  Scrollinfo.FMask := SIF_ALL;
  Scrollinfo.cbSize := SizeOf(ScrollInfo);
  if GetScrollInfo(Handle, SB_VERT, scrollinfo) then
  begin
    Scrollinfo.FMask := SIF_ALL;
    Scrollinfo.cbSize := SizeOf(ScrollInfo);
    FlatSetScrollInfo(SB_VERT, ScrollInfo, True);
  end;
end;

procedure TSectionListbox.UpdateHScrollBar;
var
  ScrollInfo: TScrollInfo;
  mx: integer;
begin
  UpdateHorzScrollbar;

  mx := SendMessage(Handle,LB_GETHORIZONTALEXTENT,0,0);
  if mx > 0 then
  begin
    ScrollInfo.FMask := SIF_ALL;
    ScrollInfo.cbSize := SizeOf(scrollinfo);
    if GetScrollInfo(Handle, SB_HORZ, scrollinfo) then
    begin
      if (FOldScrollPos <> scrollinfo.nMax) then
      begin
        ScrollInfo.FMask := SIF_ALL;
        ScrollInfo.cbSize := SizeOf(scrollinfo);
        FlatSetScrollInfo(SB_HORZ, scrollinfo, True);
      end;
      FOldScrollPos := scrollinfo.nMax;
    end;
  end;
end;

procedure TSectionListBox.WndProc(var Message: TMessage);
begin
  inherited;

  if (message.msg = LB_ADDSTRING) or
    (message.msg = LB_INSERTSTRING) or
    (message.msg = LB_DELETESTRING) or
    (message.msg = LB_GETTOPINDEX) or
    (message.msg = LB_RESETCONTENT) then
  begin
    if FUpdateCount = 0 then
    begin
      UpdateHScrollBar;
      UpdateVScrollBar;
    end;
      {
      UpdateStyle;
      UpdateColor;
      UpdateWidth;
      }
    if message.msg = LB_RESETCONTENT then
      FlatShowScrollbar(SB_VERT, False);
  end;
end;

procedure TSectionListBox.CNCommand(var Message: TWMCommand);
var
  idx, i, j: Integer;
begin
  inherited;
  if FUpdateCount > 0 then
    Exit;

  case Message.NotifyCode of
    LBN_SELCHANGE:
      begin
        if Assigned(FOnChange) then
        begin
          idx := ItemIndex;
          i := GetItemSectionIndex(idx);
          j := GetSectionListIndex(i);
          FOnChange(self, i, idx - j - 1);
        end;

        UpdateHScrollBar;
        UpdateVScrollBar;
        UpdateStyle;
        UpdateColor;
        UpdateWidth;
      end;
  end;
end;

procedure TSectionListBox.WallpaperChanged;
begin
  if not self.HandleAllocated then Exit;
  Brush.Bitmap := nil;
  if (FWallpaper <> nil) and (not FWallpaper.Empty) then
  begin
    Brush.Color := clNone;
    Brush.Bitmap := FWallpaper;
  end
  else
  begin
    Brush.Color := Color;
  end;
  Invalidate;
end;


procedure TSectionMemo.DoEnter;
begin
  inherited;
end;

procedure TSectionMemo.DoExit;
var
  s: string;
  slb: TSectionListBox;

begin
  inherited;
  slb := (self.Parent as TSectionListBox);

  if Assigned(self.Lines) and Assigned(fListSection) then
  begin
    s := self.Text;

    if length(s) > 0 then
      while ( (s[length(s)] = #13) or (s[length(s)] = #10) or (s[length(s)] = #9)) do delete(s, length(s), 1);

    if Assigned(slb.fOnEndEdit) then
      slb.fOnEndEdit(slb, SectionIdx, SubItemIdx, s);
    fListSection.Subitems.Strings[subitemIdx - 1] := s;
    slb.UpdateItemHeight;
  end;
  Visible := False;
  Parent.SetFocus;
  slb.ItemIndex := ItemIdx;
end;

procedure TSectionMemo.WMKeyDown(var Msg: TWMKeydown);
begin
  inherited;
  if msg.CharCode = VK_ESCAPE then
  begin
  end;
  if ((msg.CharCode in [VK_UP, VK_DOWN]) and (Lines.Count <= 1)) then
  begin
    Visible := False;
    Parent.SetFocus;
  end;
  if (msg.CharCode = VK_RETURN) and
    (GetKeyState(VK_CONTROL) and $8000 = $0000) then
  begin
    Visible := False;
    Parent.SetFocus;
  end;
end;
end.

