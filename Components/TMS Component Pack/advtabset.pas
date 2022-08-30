{***************************************************************************}
{ TAdvTabSet component                                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2006 - 2013                                        }
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

unit AdvTabSet;

{$T-,H+,X+}

{$I TMSDEFS.INC}
interface

uses Windows, Classes, Graphics, Forms, Controls, Messages, ImgList, Math
  , Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  CloseButtonWidth = 14;
  CloseButtonHeight = 13;

  ScrollLength = 24;
  ScrollWidth = 13;

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 7; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 6; // Build nr.

  // version history
  // 1.4.0.0 : Custom close glyph added
  // 1.4.1.0 : FreeOnClose capability added
  // 1.4.1.1 : Fixed issue with ActiveFont use
  // 1.4.5.0 : Release update
  // 1.5.0.0 : Fixed ActiveFont issue
  // 1.5.0.1 : Fixed issue with setting caption & tab sizing
  // 1.5.0.2 : Fixed issue with TabRearrange and OnDragOver
  // 1.5.1.0 : New : event OnChanged added
  // 1.5.2.0 : New : event OnDrawTabSetBackground
  // 1.5.5.0 : New : RealToDisplTabIndex , DisplToRealTabIndex functions added
  // 1.6.0.0 : New : TAdvMDITabSet component added
  // 1.6.1.0 : Fixed : issue with toggling tab visible property
  // 1.7.0.0 : New : ClosePosition property added
  // 1.7.1.0 : New : function ItemIndex added that returns index from tab starting from displayed position
  // 1.7.1.1 : Fixed : issue with tab indexes when tabs are hidden
  // 1.7.1.2 : Fixed : issue with slow MDI form closing
  // 1.7.1.3 : Improved : behavior with tabs with empty caption text
  // 1.7.1.4 : Fixed : Issue with hover tab
  // 1.7.1.5 : Fixed : Issue with Win64
  // 1.7.1.6 : Fixed : Issue with index returned by OnChanged when invisible tabs are used

type
  TScrollBtn = (sbLeft, sbRight);
  TScrollPosition = (spHorizontal, spVertical);

  TScroller = class(TCustomControl)
  private
    { property usage }
    FMin: Longint;
    FMax: Longint;
    FPosition: Longint;
    FOnClick: TNotifyEvent;
    FChange: Integer;
    FArrowColor: TColor;
    FScrollPosition: TScrollPosition;

    { private usage }
    Bitmap: TBitmap;
    Pressed: Boolean;
    Down: Boolean;
    Current: TScrollBtn;
    pWidth: Integer;
    pHeight: Integer;

    { property access methods }
    procedure SetMin(Value: Longint);
    procedure SetMax(Value: Longint);
    procedure SetPosition(Value: Longint);
    procedure SetArrowColor(Value: TColor);
    procedure SetScrollPosition(Value: TScrollPosition);

    { private methods }
    function CanScrollLeft: Boolean;
    function CanScrollRight: Boolean;
    procedure DoMouseDown(X: Integer);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

    procedure DrawSBLeftDIS(aCanvas: TCanvas);
    procedure DrawSBLeftDN(aCanvas: TCanvas);
    procedure DrawSBLeft(aCanvas: TCanvas);
    procedure DrawSBRightDIS(aCanvas: TCanvas);
    procedure DrawSBRight(aCanvas: TCanvas);
    procedure DrawSBRightDN(aCanvas: TCanvas);

    procedure DrawSBTopDIS(aCanvas: TCanvas);
    procedure DrawSBTopDN(aCanvas: TCanvas);
    procedure DrawSBTop(aCanvas: TCanvas);
    procedure DrawSBBottomDIS(aCanvas: TCanvas);
    procedure DrawSBBottom(aCanvas: TCanvas);
    procedure DrawSBBottomDN(aCanvas: TCanvas);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Min: Longint read FMin write SetMin default 0;
    property Max: Longint read FMax write SetMax default 0;
    property Position: Longint read FPosition write SetPosition default 0;
    property Change: Integer read FChange write FChange default 1;
    property ArrowColor: TColor read FArrowColor write SetArrowColor default cl3DDkShadow;
    property ScrollPosition: TScrollPosition read FScrollPosition write SetScrollPosition default spHorizontal;
  end;

  TAdvCustomTabSet = class;

  TTabList = class(TStringList)
  private
    Tabs: TAdvCustomTabSet;
  public
    procedure Insert(Index: Integer; const S: string); override;
    procedure Delete(Index: Integer); override;
    function Add(const S: string): Integer; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure Clear; override;
    procedure AddStrings(Strings: TStrings); override;
  end;

  { each TEdgeType is made up of one or two of these parts }
  TEdgePart = (epSelectedLeft, epUnselectedLeft, epSelectedRight,
    epUnselectedRight);

  { represents the intersection between two tabs, or the edge of a tab }
  TEdgeType = (etNone, etFirstIsSel, etFirstNotSel, etLastIsSel, etLastNotSel,
    etNotSelToSel, etSelToNotSel, etNotSelToNotSel);

  TTabStyle = (tsStandard, tsOwnerDraw);
  TAdvTabStyle = (tsClassic, tsDotNet);
  TGradientDirection = (gdVertical, gdHorizontal);
  TAdvTabPosition = (pLeft, pRight, pTop, pBottom);
  TCloseButtonPos = (cbTabs, cbTabSet);
  TShowScroller  = (ssAuto, ssAlways);
  TClosePosition = (cpLeft, cpRight);

  TMeasureTabEvent = procedure(Sender: TObject; Index: Integer;
    var TabWidth: Integer) of object;
  TDrawTabEvent = procedure(Sender: TObject; TabCanvas: TCanvas; R: TRect;
    Index: Integer; Selected: Boolean) of object;

  TTabChangeEvent = procedure(Sender: TObject; NewTab: Integer;
    var AllowChange: Boolean) of object;

  TTabChangedEvent = procedure(Sender: TObject; NewTab: Integer) of object;

  TTabOverlapSize = 0..15;
  TMarginSize = -MaxInt..MaxInt;
  TMarginChange = procedure(NewValue: TMarginSize; OldValue: TMarginSize; Index: integer) of object;
  TTabCloseEvent = procedure(Sender: TObject; TabIndex: integer) of object;
  TTabMovedEvent = procedure(Sender: TObject; FromPos: integer; ToPos: Integer)of object;
  TCanCloseEvent = procedure(Sender: TObject; TabIndex: integer; var CanClose: Boolean) of object;
  TDrawTabSetBackgroundEvent = procedure(Sender: TObject; ACanvas: TCanvas; Rect: TRect) of object;

  TTabCollectionItem = class(TCollectionItem)
  private
    FCaption: TCaption;
    FVisible: Boolean;
    FShowClose: Boolean;

    FTag: integer;
    FObject: TObject;

    FTextColor: TColor;
    FTabColor: TColor;
    FTabColorTo: TColor;
    FTabGradientDirection: TGradientDirection;
    FHoverGradientDirection: TGradientDirection;

    FImageIndex: Integer;
    FIndexInAdvTabSet: integer;
    FDispOrder: Integer;
    FEnable: boolean;
    FVisIndex: Integer;
    procedure SetCaption(const Value: TCaption);
    procedure SetVisible(const Value: Boolean);
    procedure SetShowClose(const Value: Boolean);

    procedure SetTextColor(const Value: TColor);
    procedure SetTabColor(const Value: TColor);
    procedure SetTabColorTo(const Value: TColor);
    procedure SetTabGradientDirection(value: TGradientDirection);
    procedure SetHoverGradientDirection(value: TGradientDirection);

    procedure SetImageIndex(const Value: Integer);
    procedure SetEnable(const Value: boolean);
  protected
    FChildForm: TForm;
    FOnActivateForm: TNotifyEvent;
    FOnDestroyForm: TNotifyEvent;
    function GetDisplayName: string; override;
    procedure SetIndex(Value: Integer); override;
    property VisIndex: Integer read FVisIndex;
  public
    function MoveItemInTabSetTo(ItemIndexOfTabSet: integer): Boolean;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property AObject: TObject read FObject write FObject;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property Enable: boolean read FEnable write SetEnable;
    property Visible: Boolean read FVisible write SetVisible;
    property ShowClose: Boolean read FShowClose write SetShowClose;
    property TextColor: TColor read FTextColor write SetTextColor default clBlack;
    property TabColor: TColor read FTabColor write SetTabColor;
    property TabColorTo: TColor read FTabColorTo write SetTabColorTo;
    property HoverGradientDirection: TGradientDirection read FHoverGradientDirection write SetHoverGradientDirection default gdVertical;
    property TabGradientDirection: TGradientDirection read FTabGradientDirection write SetTabGradientDirection default gdVertical;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Tag: integer read FTag write FTag;
  end;

  TTabCollection = class(TCollection)
  private
    FOwner: TAdvCustomTabSet;
    function GetItem(Index: Integer): TTabCollectionItem;
    procedure SetItem(Index: Integer; const Value: TTabCollectionItem);
  protected
    procedure UpdateItemsIndexesInTabSet;
    procedure ValidateVisTabIndexes;
  public
    constructor Create(AOwner: TAdvCustomTabSet);
    property Items[Index: Integer]: TTabCollectionItem read GetItem write SetItem; default;
    property AdvTabSet: TAdvCustomTabSet read FOwner;
    function Add: TTabCollectionItem;
    function Insert(Index: Integer): TTabCollectionItem;
    function GetOwner: TPersistent; override;
  end;

  TTabMargin = class(TPersistent)
  private
    FLeftMargin: TMarginSize;
    FTopMargin: TMarginSize;
    FRightMargin: TMarginSize;
    FOnMarginChange: TMarginChange;
    procedure SetMargin(Index: integer; Value: TMarginSize);
  protected
    property OnMarginChange: TMarginChange read FOnMarginChange write FOnMarginChange;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property LeftMargin: TMarginSize index 0 read FLeftMargin write SetMargin default 0;
    property TopMargin: TMarginSize index 1 read FTopMargin write SetMargin default 0;
    property RightMargin: TMarginSize index 2 read FRightMargin write SetMargin;
  end;

  TAdvCustomTabSet = class(TCustomControl)
  private
    { property instance variables }
    FStartMargin: Integer;
    FEndMargin: Integer;
    FTabs: TStrings;
    FTabIndex: Integer;
    FFirstIndex: Integer;
    FVisibleTabs: Integer;
    FSelectedColor: TColor;
    FUnselectedColor: TColor;
    FBackgroundColor: TColor;
    FDitherBackground: Boolean;
    FAutoScroll: Boolean;
    FStyle: TTabStyle;
    FOwnerDrawHeight: Integer;
    FOnMeasureTab: TMeasureTabEvent;
    FOnDrawTab: TDrawTabEvent;
    FOnChange: TTabChangeEvent;
    FOnChanged: TTabChangedEvent;

    FAdvTabs: TTabCollection;
    FSelectedColorTo: TColor;
    FUnSelectedColorTo: TColor;
    FTextColor: TColor;
    FTabBorderColor: TColor;

    FTabBackGround: TBitmap;
    FTabBackGroundSelected: TBitmap;

    FGradientDirection: TGradientDirection;
    FHoverGradientDirection: TGradientDirection;
    FTabHoverColor: TColor;
    FTabHoverColorTo: TColor;
    FTabHoverBorder: TColor;

    FImages: TCustomImageList;

    FTabMargin: TTabMargin;
    FTabOverlap: TTabOverlapSize;
    FShowFocus: Boolean;
    FOnTabClose: TTabCloseEvent;

    FOnCanClose: TCanCloseEvent;

    FOnDrawTabSetBackground: TDrawTabSetBackgroundEvent;
    
    FAdvTabStyle: TAdvTabStyle;

    FOnTabMoved: TTabMovedEvent;

    FTabPosition: TAdvTabPosition;
    FCloseButtonAt: TCloseButtonPos;
    FTabRearrange: Boolean;

    FShowScroller: TShowScroller;
    FLowerSelected: integer;
    { private instance variables }

    FHoverClosedButton: Boolean;
    FDuplicateTabs: TStringList;
    FHoverTab: Integer;
    FCloseButtonDown: boolean;

    ImageList: TImageList;
    MemBitmap: TBitmap;   { used for off-screen drawing }
    BrushBitmap: TBitmap; { used for background pattern }

    TabPositions: TList;
    FTabHeight: Integer;
    FScroller: TScroller;
    FDoFix: Boolean;
    FSoftTop: Boolean;
    FActiveFont: TFont;
    FCloseGlyph: TBitmap;
    FDisableCloseGlyph: TBitmap;
    FFreeOnClose: Boolean;
    FClosePosition: TClosePosition;

    { property access methods }
    procedure SetSelectedColor(Value: TColor);
    procedure SetUnselectedColor(Value: TColor);
    procedure SetBackgroundColor(Value: TColor);
    procedure SetDitherBackground(Value: Boolean);
    procedure SetAutoScroll(Value: Boolean);
    procedure SetStartMargin(Value: Integer);
    procedure SetEndMargin(Value: Integer);
    procedure SetTabIndex(Value: Integer);
    procedure SetFirstIndex(Value: Integer);
    procedure SetTabList(Value: TStrings);
    procedure SetTabStyle(Value: TTabStyle);
    procedure SetTabHeight(Value: Integer);

    procedure SetSelectedColorTo(Value: TColor);
    procedure SetUnSelectedColorTo(Value: TColor);
    procedure SetTextColor(Value: TColor);
    procedure SetTabBorderColor(Value: TColor);
    procedure SetTabBackGround(Const Value: TBitmap);
    procedure SetTabBackGroundSelected(Const Value: TBitmap);
    procedure SetGradientDirection(Value: TGradientDirection);
    procedure SetHoverGradientDirection(value: TGradientDirection);
    procedure SetTabMargin(Value: TTabMargin);
    procedure SetTabOverlap(Value: TTabOverlapSize);
    procedure SetImages(value: TCustomImageList);
    procedure SetAdvTabStyle(Value: TAdvTabStyle);

    procedure SetTabPosition(Value: TAdvTabPosition);
    procedure SetCloseButtonAt(Value: TCloseButtonPos);

    procedure SetLowerSelected(Value: integer);
    procedure SetShowScroller(Value: TShowScroller);

    { private methods }
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CreateBrushPattern(Bitmap: TBitmap);
    function CalcTabPositions(Start, Stop: Integer; Canvas: TCanvas;
      First: Integer): Integer;
    procedure CreateScroller;
    procedure InitBitmaps;
    procedure DoneBitmaps;
    procedure CreateEdgeParts;
    procedure FixTabPos;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure ScrollClick(Sender: TObject);
    procedure ReadIntData(Reader: TReader);
    procedure ReadBoolData(Reader: TReader);
    procedure SetSoftTop(const Value: Boolean);

    procedure DrawCloseGlyph(Canvas: TCanvas; P: TPoint; IsEnable: Boolean);
    procedure DrawCloseButton(Canvas: TCanvas; Rect: TRect; Active: Boolean);
    procedure DrawHoverCloseButton(Rect: TRect);
    procedure DrawDownCloseButton(Rect: TRect);
    function IsOnButton(TabIndex, X, Y: integer): Boolean; overload;
    function IsOnButton(TabIndex, X, Y: integer; var aRect: TRect): Boolean; overload;
    procedure TabMarginChange(NewValue, OldValue: TMarginSize; Index: integer);
    procedure SetOriginalTabWidth;
    procedure IncTabWidth(w: integer);

    procedure DrawCrossAtTabSet(IsEnable: Boolean);
    procedure DrawCloseButtonAtTabSet(IsEnable: Boolean);
    procedure DrawDownCloseButtonAtTabSet();
    function IsOnCloseButtonAtTabSet(X, Y: integer): Boolean;

    procedure CloseButtonClick;
    procedure SetActiveFont(const Value: TFont);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure CloseGlyphOnChange(Sender: TObject);
    procedure SetCloseGlyph(const Value: TBitmap);
    procedure SetClosePosition(const Value: TClosePosition);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;

    procedure Paint; override;
    procedure DrawTab(TabCanvas: TCanvas; R: TRect; Index: Integer;
      Selected: Boolean); virtual;
    function CanChange(NewIndex: Integer): Boolean;
    procedure MeasureTab(Index: Integer; var TabWidth: Integer); virtual;
    procedure DefineProperties(Filer: TFiler); override;

    procedure AdjustTabWidth;
    property DuplicateTabs: TStringList read FDuplicateTabs;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Loaded; override;

    property Scroller: TScroller read FScroller;
    property Tabs: TStrings read FTabs write SetTabList;
    property Style: TTabStyle read FStyle write SetTabStyle default tsOwnerDraw;
    property DitherBackground: Boolean read FDitherBackground write SetDitherBackground default false;

    procedure UpdateScroller;
    procedure ChangeActiveTab(Value: Integer); virtual;
    procedure BeforeCloseTab(Tab: TTabCollectionItem; var CloseAction: TCloseAction); virtual;
    function CanCloseTab(TabIdx: Integer; var CloseAction: TCloseAction): Boolean; virtual;

    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll default True;
    property ActiveFont: TFont read FActiveFont write SetActiveFont;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clBtnFace;
    property CloseGlyph: TBitmap read FCloseGlyph write SetCloseGlyph;
    property EndMargin: Integer read FEndMargin write SetEndMargin default 5;
    property StartMargin: Integer read FStartMargin write SetStartMargin default 5;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clBtnFace;
    property SoftTop: Boolean read FSoftTop write SetSoftTop default False;

    property AdvTabs: TTabCollection read FAdvTabs write FAdvTabs;
    property ClosePosition: TClosePosition read FClosePosition write SetClosePosition default cpLeft;
    property FreeOnClose: Boolean read FFreeOnClose write FFreeOnClose;
    property SelectedColorTo: TColor read FSelectedColorTo write SetSelectedColorTo default clNone;
    property UnSelectedColorTo: TColor read FUnSelectedColorTo write SetUnSelectedColorTo default clNone;
    property TextColor: TColor read FTextColor write SetTextColor default clBlack;
    property TabBorderColor: TColor read FTabBorderColor write SetTabBorderColor default clGray;
    property TabBackGround: TBitmap read FTabBackGround write SetTabBackGround;
    property TabBackGroundSelected: TBitmap read FTabBackGroundSelected write SetTabBackGroundSelected;
    property GradientDirection : TGradientDirection read FGradientDirection write SetGradientDirection;
    property HoverGradientDirection: TGradientDirection read FHoverGradientDirection write SetHoverGradientDirection default gdVertical;
    property TabHoverColor: TColor read FTabHoverColor write FTabHoverColor default clNone;
    property TabHoverColorTo: TColor read FTabHoverColorTo write FTabHoverColorTo default clNone;
    property TabHoverBorder: TColor read FTabHoverBorder write FTabHoverBorder default clNone;
    property TabMargin: TTabMargin read FTabMargin write SetTabMargin;
    property TabOverlap: TTabOverlapSize read FTabOverlap write SetTabOverlap;
    property ShowFocus: Boolean read FShowFocus write FShowFocus default false;
    property Images: TCustomImageList read FImages write SetImages;
    property TabStyle: TAdvTabStyle read FAdvTabStyle write SetAdvTabStyle default tsClassic;
    property TabPosition: TAdvTabPosition read FTabPosition write SetTabPosition default pTop;
    property CloseButtonAt: TCloseButtonPos read FCloseButtonAt write SetCloseButtonAt default cbTabs;
    property TabRearrange: Boolean read FTabRearrange write FTabRearrange default false;
    property ShowScroller: TShowScroller read FShowScroller write SetShowScroller default ssAlways;
    property LowerSelected: integer read FLowerSelected write SetLowerSelected default 2;
    property TabHeight: Integer read FOwnerDrawHeight write SetTabHeight default 20;
    property TabIndex: Integer read FTabIndex write SetTabIndex default -1;
    property UnselectedColor: TColor read FUnselectedColor write SetUnselectedColor default clWindow;
    property VisibleTabs: Integer read FVisibleTabs;
    property OnTabMoved: TTabMovedEvent read FOnTabMoved write FOnTabMoved;
    property OnTabClose: TTabCloseEvent read FOnTabClose write FOnTabClose;
    property OnCanClose: TCanCloseEvent read FOnCanClose write FOnCanClose;
    property OnChange: TTabChangeEvent read FOnChange write FOnChange;
    property OnChanged: TTabChangedEvent read FOnChanged write FOnChanged;
    property OnMeasureTab: TMeasureTabEvent read FOnMeasureTab write FOnMeasureTab;
    property OnDrawTabSetBackground: TDrawTabSetBackgroundEvent read FOnDrawTabSetBackground write FOnDrawTabSetBackground;
  public
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemAtPos(Pos: TPoint): Integer;
    function ItemRect(Item: Integer): TRect;
    function ItemIndex(Pos: integer): integer;
    function ItemWidth(Index: Integer): Integer;
    function MinClientRect: TRect; overload;
    function MinClientRect(IncludeScroller: Boolean): TRect; overload;
    function MinClientRect(TabCount: Integer; IncludeScroller: Boolean = False): TRect; overload;
    procedure SelectNext(Direction: Boolean);
    function DisplToRealTabIndex(tab: integer): integer;
    function RealToDisplTabIndex(tab: integer): integer;

    procedure DragDrop(Source: TObject; X, Y: Integer); override;

    property Canvas;
    property FirstIndex: Integer read FFirstIndex write SetFirstIndex default 0;
    property VersionNr: Integer read GetVersionNr;
  published
    property Version: string read GetVersion write SetVersion;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvTabSet = class(TAdvCustomTabSet)
  published
    property Align;
    property Anchors;
    property AutoScroll;
    property ActiveFont;
    property BackgroundColor;
    property CloseGlyph;
    property ClosePosition;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EndMargin;
    property Font;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property StartMargin;
    property SelectedColor;
    property SoftTop;

    property AdvTabs;
    property FreeOnClose;
    property SelectedColorTo;
    property UnSelectedColorTo;
    property TextColor;
    property TabBorderColor;
    property TabBackGround;
    property TabBackGroundSelected;
    property GradientDirection;
    property HoverGradientDirection;
    property TabHoverColor;
    property TabHoverColorTo;
    property TabHoverBorder;
    property TabMargin;
    property TabOverlap;
    property ShowFocus;
    property Images;
    property TabStyle;
    property TabPosition;
    property CloseButtonAt;
    property TabRearrange;
    property ShowScroller;
    property LowerSelected;
    property TabHeight;
    property TabIndex;
    property UnselectedColor;
    property Visible;
    property VisibleTabs;

    property OnTabMoved;
    property OnTabClose;
    property OnCanClose;
    property OnClick;
    property OnChange;
    property OnChanged;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMeasureTab;
    property OnStartDock;
    property OnStartDrag;
    property OnDrawTabSetBackground;
  end;

   {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvMDITabSet = class(TAdvCustomTabSet)
  private
    FInternalDelete: Boolean;
    function GetAdvTabCount: integer;
    procedure OnChildFormActivate(Sender: TObject);
    procedure OnChildFormDestroy(Sender: TObject);
  protected
    function GetAdvTabs(index: integer): TTabCollectionItem;
    procedure Change;
    procedure ChangeActiveTab(Value: Integer); override;
    procedure BeforeCloseTab(Tab: TTabCollectionItem; var CloseAction: TCloseAction); override;
    function CanCloseTab(TabIdx: Integer; var CloseAction: TCloseAction): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddTab(ChildForm: TForm): TTabCollectionItem;
    function GetChildForm(Tab: TTabCollectionItem): TForm;

    property AdvTabCount: integer read GetAdvTabCount;
    property AdvTab[index: integer]: TTabCollectionItem read GetAdvTabs;
    function GetTab(AChild: TForm): TTabCollectionItem;
    property TabIndex;
  published
    property Align;
    property Anchors;
    property AutoScroll;
    property ActiveFont;
    property BackgroundColor;
    property CloseGlyph;
    property ClosePosition;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EndMargin;
    property Font;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property StartMargin;
    property SelectedColor;
    property SoftTop;

    //property AdvTabs;
    //property FreeOnClose;
    property SelectedColorTo;
    property UnSelectedColorTo;
    property TextColor;
    property TabBorderColor;
    property TabBackGround;
    property TabBackGroundSelected;
    property GradientDirection;
    property HoverGradientDirection;
    property TabHoverColor;
    property TabHoverColorTo;
    property TabHoverBorder;
    property TabMargin;
    property TabOverlap;
    property ShowFocus;
    property Images;
    property TabStyle;
    property TabPosition;
    property CloseButtonAt;
    property TabRearrange;
    property ShowScroller;
    property LowerSelected;
    property TabHeight;
    property UnselectedColor;
    property Visible;
    property VisibleTabs;

    property OnTabMoved;
    property OnTabClose;
    property OnCanClose;
    property OnClick;
    property OnChange;
    property OnChanged;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMeasureTab;
    property OnStartDock;
    property OnStartDrag;
    property OnDrawTabSetBackground;
  end;

implementation

uses
  Consts, SysUtils;


const
  EdgeWidth = 9;  { This controls the angle of the tab edges }

type
  {$IFDEF WIN64}
  TTabPos = packed record
    Size, StartPos: Integer;
  end;
  {$ENDIF}
  {$IFDEF WIN32}
  TTabPos = packed record
    Size, StartPos: Word;
  end;
  {$ENDIF}

  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}
  IntPtr = Pointer;


//----------------------------------------------------------------- DrawGradient
procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;

begin
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

//-------------------------------------------------------- DrawTransparentBitmap
procedure DrawTransparentBitmap(hdc: THandle; hBitmap: THandle; xStart, yStart: Integer;
  width, height, offsx, offsy: Integer; cTransparentColor: TColor);
// The function draws a bitmap with a transparent background.
var
  cColor: TColor;
  bmAndBack, bmAndObject, bmAndMem, bmSave: THandle;
  bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: THandle;
  hdcMem, hdcBack, hdcObject, hdcTemp, hdcSave: THandle;
  ptSize: TPoint;
begin
  hdcTemp := CreateCompatibleDC(hdc);
  SelectObject(hdcTemp, hBitmap);

  ptSize.x := width;
  ptSize.y := height;

  DPtoLP(hdcTemp, ptSize, 1);

  hdcBack := CreateCompatibleDC(hdc);
  hdcObject := CreateCompatibleDC(hdc);
  hdcMem := CreateCompatibleDC(hdc);
  hdcSave := CreateCompatibleDC(hdc);

  bmAndBack := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);
  bmAndObject := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);

  bmAndMem := CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);
  bmSave := CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);

  bmBackOld := SelectObject(hdcBack, bmAndBack);
  bmObjectOld := SelectObject(hdcObject, bmAndObject);
  bmMemOld := SelectObject(hdcMem, bmAndMem);
  bmSaveOld := SelectObject(hdcSave, bmSave);

  SetMapMode(hdcTemp, GetMapMode(hdc));

  BitBlt(hdcSave, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, SRCCOPY);

  cColor := SetBkColor(hdcTemp, cTransparentColor);

  BitBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, SRCCOPY);

  SetBkColor(hdcTemp, cColor);

  BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, NOTSRCCOPY);

  // take copy of existing canvas
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdc, xStart, yStart, SRCCOPY);
  // and existing canvas with copy
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SRCAND);

  BitBlt(hdcTemp, offsx, offsy, ptSize.x, ptSize.y, hdcBack, 0, 0, SRCAND);
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, SRCPAINT);
  BitBlt(hdc, xStart, yStart, ptSize.x, ptSize.y, hdcMem, 0, 0, SRCCOPY);
  BitBlt(hdcTemp, 0, 0, ptSize.x, ptSize.y, hdcSave, 0, 0, SRCCOPY);

  DeleteObject(SelectObject(hdcBack, bmBackOld));
  DeleteObject(SelectObject(hdcObject, bmObjectOld));
  DeleteObject(SelectObject(hdcMem, bmMemOld));
  DeleteObject(SelectObject(hdcSave, bmSaveOld));

  DeleteDC(hdcMem);
  DeleteDC(hdcBack);

  DeleteDC(hdcObject);
  DeleteDC(hdcSave);
  DeleteDC(hdcTemp);
end;

//----------------------------------------------------- StretchTransparentBitmap
procedure StretchTransparentBitmap(hdc: THandle; hBitmap: THandle; xStart, yStart: Integer;
  width, height, offsx, offsy, bmpw, bmph: Integer; cTransparentColor: TColor);
// The function draws a bitmap with a transparent background.
var
  cColor: TColor;
  bmAndBack, bmAndObject, bmAndMem, bmSave: THandle;
  bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: THandle;
  hdcMem, hdcBack, hdcObject, hdcTemp, hdcSave: THandle;
  ptSize: TPoint;
begin
  hdcTemp := CreateCompatibleDC(hdc);
  SelectObject(hdcTemp, hBitmap);

  ptSize.x := width;
  ptSize.y := height;

  DPtoLP(hdcTemp, ptSize, 1);

  hdcBack := CreateCompatibleDC(hdc);
  hdcObject := CreateCompatibleDC(hdc);
  hdcMem := CreateCompatibleDC(hdc);
  hdcSave := CreateCompatibleDC(hdc);

  bmAndBack := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);
  bmAndObject := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);

  bmAndMem := CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);
  bmSave := CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);

  bmBackOld := SelectObject(hdcBack, bmAndBack);
  bmObjectOld := SelectObject(hdcObject, bmAndObject);
  bmMemOld := SelectObject(hdcMem, bmAndMem);
  bmSaveOld := SelectObject(hdcSave, bmSave);

  SetMapMode(hdcTemp, GetMapMode(hdc));

  StretchBlt(hdcSave, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, bmpw, bmph, SRCCOPY);

  cColor := SetBkColor(hdcTemp, cTransparentColor);

  StretchBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, bmpw, bmph, SRCCOPY);

  SetBkColor(hdcTemp, cColor);

  BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, NOTSRCCOPY);

  // take copy of existing canvas
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdc, xStart, yStart, SRCCOPY);
  // and existing canvas with copy
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SRCAND);
  StretchBlt(hdcTemp, offsx, offsy, bmpw, bmph, hdcback, 0, 0, ptsize.x, ptsize.y, SRCAND);
  StretchBlt(hdcMem, 0, 0, ptSize.X, ptSize.Y, hdctemp, offsx, offsy, bmpw, bmph, SRCPAINT);
  BitBlt(hdc, xStart, yStart, ptSize.x, ptSize.y, hdcMem, 0, 0, SRCCOPY);
  StretchBlt(hdcTemp, offsx, offsy, bmpw, bmph, hdcSave, 0, 0, ptsize.x, ptsize.y, SRCCOPY);

  DeleteObject(SelectObject(hdcBack, bmBackOld));
  DeleteObject(SelectObject(hdcObject, bmObjectOld));
  DeleteObject(SelectObject(hdcMem, bmMemOld));
  DeleteObject(SelectObject(hdcSave, bmSaveOld));

  DeleteDC(hdcMem);
  DeleteDC(hdcBack);

  DeleteDC(hdcObject);
  DeleteDC(hdcSave);
  DeleteDC(hdcTemp);
end;

//---------------------------------------------------------------- BitmapStretch
procedure BitmapStretch(bmp: tbitmap; canvas: tcanvas; x, y, height: integer);
var
  mid: integer;
  fillh: integer;
  c: TColor;
begin
  mid := bmp.height div 2;
  fillh := height - bmp.height;
  c := bmp.Canvas.Pixels[0, bmp.Height - 1];
  
  DrawTransparentBitmap(canvas.handle, bmp.handle, x, y, bmp.Width, bmp.Height div 2, 0, 0, c);
  StretchTransparentBitmap(canvas.handle, bmp.Handle, x, y + mid, bmp.width, fillh, 0, mid - 1, bmp.Width, 2, c);
  DrawTransparentBitmap(canvas.handle, bmp.handle, x, y + mid + fillh, bmp.width, bmp.Height div 2, 0, mid, c);
end;

//--------------------------------------------------------- BitmapStretchInWidth
procedure BitmapStretchInWidth(bmp: TBitmap; canvas: tcanvas; x, y, width: integer);
var
  mid, ofs: integer;
  fillw: integer;
  c: TColor;
begin
  mid := bmp.Width div 2;
  fillw := width - bmp.Width;
  
//  if P = 'T' then
//    c := bmp.Canvas.Pixels[bmp.Width - 1, 0]
//  else
  c := bmp.Canvas.Pixels[0, bmp.Height - 1];

  ofs := 0;
  if odd(bmp.Width) then inc(ofs);

  DrawTransparentBitmap(canvas.handle, bmp.handle, x, y, bmp.Width div 2, bmp.Height, 0, 0, c);
  StretchTransparentBitmap(canvas.handle, bmp.Handle, x + mid, y, fillw, bmp.height, mid - 1, 0, 2, bmp.height, c);
  DrawTransparentBitmap(canvas.handle, bmp.handle, x + mid + fillw, y, bmp.width div 2, bmp.Height, mid + ofs, 0, c);
end;

//------------------------------------------------------------------------------
//------------------------------{ TScroller }-----------------------------------
//------------------------------------------------------------------------------
constructor TScroller.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Bitmap := TBitmap.Create;
  pWidth := 24;
  pHeight := 13;
  FMin := 0;
  FMax := 0;
  FPosition := 0;
  FChange := 1;

  FArrowColor:= cl3DDkShadow;
  FScrollPosition:= spHorizontal;
end;

//---------------------------------------------------------------------- Destroy
destructor TScroller.Destroy;
begin
  Bitmap.Free;
  inherited Destroy;
end;

//---------------------------------------------------------------- DrawSBLeftDIS
procedure TScroller.DrawSBLeftDIS(aCanvas: TCanvas);
begin
  // Top Position
  with Canvas do
  begin
    Brush.color:= Color;
    FillRect(Rect(0,0,12,13));

    Pen.Color:= FArrowColor;
      // |
    MoveTo(8,2);
    LineTo(8,10);
      // /
    MoveTo(8,2);
    LineTo(4,6);
      // \
    MoveTo(8,10);
    LineTo(3,5);
  end;
end;

//------------------------------------------------------------------- DrawSBLeft
procedure TScroller.DrawSBLeft(aCanvas: TCanvas);
begin
  // Top Position
  with Canvas do
  begin
    Brush.color:= Color;
    FillRect(Rect(0,0,12,13));

    Pen.Color:= FArrowColor;
      // |
    MoveTo(8,2);
    LineTo(8,10);
      // /
    MoveTo(8,2);
    LineTo(4,6);
      // \
    MoveTo(8,10);
    LineTo(3,5);
      // Fill arrow |
    MoveTo(7,4);
    LineTo(7,9);
    MoveTo(6,5);
    LineTo(6,8);
    Pixels[5,6]:= FArrowColor;
  end;
end;

//----------------------------------------------------------------- DrawSBLeftDN
procedure TScroller.DrawSBLeftDN(aCanvas: TCanvas);
begin
  // Top Position
  with Canvas do
  begin
    Brush.color:= Color;
    FillRect(Rect(0,0,12,13));

    Pen.Color:= clwhite;
      //  |
    MoveTo(11,0);
    LineTo(11,12);
      //  _
    LineTo(0,12);

    Pen.Color:= clBlack;
      // |
    MoveTo(1,11);
    LineTo(1,0);
      // -
    LineTo(11,0);

    Pen.Color:= FArrowColor;
      // |
    MoveTo(8,2);
    LineTo(8,10);
      // /
    MoveTo(8,2);
    LineTo(4,6);
      // \
    MoveTo(8,10);
    LineTo(3,5);

      // Fill arrow |
    MoveTo(7,4);
    LineTo(7,9);
    MoveTo(6,5);
    LineTo(6,8);
    Pixels[5,6]:= FArrowColor;
  end;
end;

//--------------------------------------------------------------- DrawSBRightDIS
procedure TScroller.DrawSBRightDIS(aCanvas: TCanvas);
begin
  // Top Position
  with Canvas do
  begin
    Brush.color:= Color;
    FillRect(Rect(12,0,24,13));

    Pen.Color:= FArrowColor;
      // |
    MoveTo(16,2);
    LineTo(16,10);
      // \
    MoveTo(16,2);
    LineTo(20,6);
      // /
    MoveTo(16,10);
    LineTo(21,5);
  end;
end;

//------------------------------------------------------------------ DrawSBRight
procedure TScroller.DrawSBRight(aCanvas: TCanvas);
begin
  // Top Position
  with Canvas do
  begin
    Brush.color:= Color;
    FillRect(Rect(12,0,24,13));

    Pen.Color:= FArrowColor;
      // |
    MoveTo(16,2);
    LineTo(16,10);
      // \
    MoveTo(16,2);
    LineTo(20,6);
      // /
    MoveTo(16,10);
    LineTo(21,5);
      // Fill Arrow |
    MoveTo(17,4);
    LineTo(17,9);
    MoveTo(18,5);
    LineTo(18,8);
    Pixels[19,6]:= FArrowColor;
  end;
end;

//---------------------------------------------------------------- DrawSBRightDN
procedure TScroller.DrawSBRightDN(aCanvas: TCanvas);
begin
  // Top Position
  with Canvas do
  begin
    Brush.color:= Color;
    FillRect(Rect(12,0,24,13));

    Pen.Color:= clwhite;
      //  |
    MoveTo(22,0);
    LineTo(22,12);
      //  _
    LineTo(11,12);

    Pen.Color:= clBlack;
      // |
    MoveTo(12,11);
    LineTo(12,0);
      // -
    LineTo(22,0);

    Pen.Color:= FArrowColor;
      // |
    MoveTo(16,2);
    LineTo(16,10);
      // \
    MoveTo(16,2);
    LineTo(20,6);
      // /
    MoveTo(16,10);
    LineTo(21,5);
      // Fill arrow |
    MoveTo(17,4);
    LineTo(17,9);
    MoveTo(18,5);
    LineTo(18,8);
    Pixels[19,6]:= FArrowColor;
  end;
end;



//----------------------------------------------------------------- DrawSBTopDIS
procedure TScroller.DrawSBTopDIS(aCanvas: TCanvas);
begin
  // Top Position
  with Canvas do
  begin
    Brush.color:= Color;
    FillRect(Rect(0,0,13,12));

    Pen.Color:= FArrowColor;
      // |
    MoveTo(2,8);
    LineTo(10,8);
      // /
    MoveTo(2,8);
    LineTo(6,4);
      // \
    MoveTo(10,8);
    LineTo(5,3);
  end;
end;

//-------------------------------------------------------------------- DrawSBTop
procedure TScroller.DrawSBTop(aCanvas: TCanvas);
begin
  // Top Position
  with Canvas do
  begin
    Brush.color:= Color;
    FillRect(Rect(0,0,13,12));

    Pen.Color:= FArrowColor;
      // |
    MoveTo(2,8);
    LineTo(10,8);
      // /
    MoveTo(2,8);
    LineTo(6,4);
      // \
    MoveTo(10,8);
    LineTo(5,3);
      // Fill arrow |
    MoveTo(4,7);
    LineTo(9,7);
    MoveTo(5,6);
    LineTo(8,6);
    Pixels[6,5]:= FArrowColor;
  end;
end;

//----------------------------------------------------------------- DrawSBTopDN
procedure TScroller.DrawSBTopDN(aCanvas: TCanvas);
begin
  // Top Position
  with Canvas do
  begin
    Brush.color:= Color;
    FillRect(Rect(0,0,13,12));

    Pen.Color:= clwhite;
      //  |
    MoveTo(0,11);
    LineTo(12,11);
      //  _
    LineTo(12,0);

    Pen.Color:= clBlack;
      // |
    MoveTo(11,1);
    LineTo(0,1);
      // -
    LineTo(0,11);

    Pen.Color:= FArrowColor;
      // |
    MoveTo(2,8);
    LineTo(10,8);
      // /
    MoveTo(2,8);
    LineTo(6,4);
      // \
    MoveTo(10,8);
    LineTo(5,3);

      // Fill arrow |
    MoveTo(4,7);
    LineTo(9,7);
    MoveTo(5,6);
    LineTo(8,6);
    Pixels[6,5]:= FArrowColor;
  end;
end;

//-------------------------------------------------------------- DrawSBBottomDIS
procedure TScroller.DrawSBBottomDIS(aCanvas: TCanvas);
begin
  // Top Position
  with Canvas do
  begin
    Brush.color:= Color;
    FillRect(Rect(0,12,13,24));

    Pen.Color:= FArrowColor;
      // |
    MoveTo(2,16);
    LineTo(10,16);
      // \
    MoveTo(2,16);
    LineTo(6,20);
      // /
    MoveTo(10,16);
    LineTo(5,21);
  end;
end;

//----------------------------------------------------------------- DrawSBBottom
procedure TScroller.DrawSBBottom(aCanvas: TCanvas);
begin
  // Top Position
  with Canvas do
  begin
    Brush.color:= Color;
    FillRect(Rect(0,12,13,24));

    Pen.Color:= FArrowColor;
      // |
    MoveTo(2,16);
    LineTo(10,16);
      // \
    MoveTo(2,16);
    LineTo(6,20);
      // /
    MoveTo(10,16);
    LineTo(5,21);
      // Fill Arrow |
    MoveTo(4,17);
    LineTo(9,17);
    MoveTo(5,18);
    LineTo(8,18);
    Pixels[6,19]:= FArrowColor;
  end;
end;

//--------------------------------------------------------------- DrawSBBottomDN
procedure TScroller.DrawSBBottomDN(aCanvas: TCanvas);
begin
  // Top Position
  with Canvas do
  begin
    Brush.color:= Color;
    FillRect(Rect(0,12,13,24));

    Pen.Color:= clwhite;
      //  |
    MoveTo(0,22);
    LineTo(12,22);
      //  _
    LineTo(12,11);

    Pen.Color:= clBlack;
      // |
    MoveTo(11,12);
    LineTo(0,12);
      // -
    LineTo(0,22);

    Pen.Color:= FArrowColor;
      // |
    MoveTo(2,16);
    LineTo(10,16);
      // \
    MoveTo(2,16);
    LineTo(6,20);
      // /
    MoveTo(10,16);
    LineTo(5,21);
      // Fill arrow |
    MoveTo(4,17);
    LineTo(9,17);
    MoveTo(5,18);
    LineTo(8,18);
    Pixels[6,19]:= FArrowColor;
  end;
end;

//------------------------------------------------------------------------ Paint
procedure TScroller.Paint;
begin
  with Canvas do
  begin
    if ScrollPosition = spHorizontal then
    begin
      { paint left button }
      if CanScrollLeft then
      begin
        if Down and (Current = sbLeft) then
          DrawSBLeftDN(Canvas)
        else
          DrawSBLeft(Canvas);
      end
      else
        DrawSBLeftDIS(Canvas);

      { paint right button }

      if CanScrollRight then
      begin
        if Down and (Current = sbRight) then
          DrawSBRightDN(Canvas)
        else
          DrawSBRight(Canvas);
      end
      else
        DrawSBRightDIS(Canvas);
    end
    else
    begin
      { paint Top button }
      if CanScrollLeft then
      begin
        if Down and (Current = sbLeft) then
          DrawSBTopDN(Canvas)
        else
          DrawSBTop(Canvas);
      end
      else
        DrawSBTopDIS(Canvas);

      { paint Bottom button }

      if CanScrollRight then
      begin
        if Down and (Current = sbRight) then
          DrawSBBottomDN(Canvas)
        else
          DrawSBBottom(Canvas);
      end
      else
        DrawSBBottomDIS(Canvas);
    end;
  end;
end;

//----------------------------------------------------------------------- WMSize
procedure TScroller.WMSize(var Message: TWMSize);
begin
  inherited;
  if ScrollPosition = spHorizontal then
  begin
    Width := pWidth - 1;
    Height := pHeight;
  end
  else
  begin
    Width := pWidth;
    Height := pHeight-1;
  end;
end;

//----------------------------------------------------------------------- SetMin
procedure TScroller.SetMin(Value: Longint);
begin
  if Value < FMax then FMin := Value;
end;

//----------------------------------------------------------------------- SetMax
procedure TScroller.SetMax(Value: Longint);
begin
  if Value >= FMin then FMax := Value;
end;

//------------------------------------------------------------------ SetPosition
procedure TScroller.SetPosition(Value: Longint);
begin
  if Value <> FPosition then
  begin
    if Value < Min then Value := Min;
    if Value > Max then Value := Max;
    FPosition := Value;
    Invalidate;
    if Assigned(FOnClick) then
      FOnClick(Self);
  end;
end;

//---------------------------------------------------------------- SetArrowColor
procedure TScroller.SetArrowColor(Value: TColor);
begin
  if Value <> FArrowColor then
  begin
    FArrowColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------ SetScrollPosition
procedure TScroller.SetScrollPosition(Value: TScrollPosition);
begin
  if Value <> FScrollPosition then
  begin
    FScrollPosition:= Value;
    if FScrollPosition = spHorizontal then
    begin
      pWidth:= 24;
      pHeight:= 13;
      Width:= pWidth-1;
      Height:= pHeight;
    end
    else
    begin
      pWidth:= 13;
      pHeight:= 24;
      Width:= pWidth;
      Height:= pHeight-1;
    end;
    Invalidate;
  end;
end;
//---------------------------------------------------------------- CanScrollLeft
function TScroller.CanScrollLeft: Boolean;
begin
  Result := Position > Min;
end;

//--------------------------------------------------------------- CanScrollRight
function TScroller.CanScrollRight: Boolean;
begin
  Result := Position < Max;
end;

//------------------------------------------------------------------ DoMouseDown
procedure TScroller.DoMouseDown(X: Integer);
begin
  if ScrollPosition = spHorizontal then
  begin
    if X < pWidth div 2 then Current := sbLeft
    else Current := sbRight;
  end
  else
  begin
    if X < pHeight div 2 then Current := sbLeft
    else Current := sbRight;
  end;

  case Current of
    sbLeft: if not CanScrollLeft then Exit;
    sbRight: if not CanScrollRight then Exit;
  end;
  Pressed := True;
  Down := True;
  Invalidate;
  SetCapture(Handle);
end;

//---------------------------------------------------------------- WMLButtonDown
procedure TScroller.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if ScrollPosition = spHorizontal then
    DoMouseDown(Message.XPos)
  else
    DoMouseDown(Message.YPos);
end;

//-------------------------------------------------------------- WMLButtonDblClk
procedure TScroller.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if ScrollPosition = spHorizontal then
    DoMouseDown(Message.XPos)
  else
    DoMouseDown(Message.YPos);
end;

//------------------------------------------------------------------ WMMouseMove
procedure TScroller.WMMouseMove(var Message: TWMMouseMove);
var
  P: TPoint;
  R: TRect;
begin
  if Pressed then
  begin
    P := Point(Message.XPos, Message.YPos);
    if ScrollPosition = spHorizontal then
      R := Rect(0, 0, pWidth div 2, pHeight)
    else
      R := Rect(0, 0, pWidth, pHeight div 2);

    if Current = sbRight then
    begin
      if ScrollPosition = spHorizontal then
        OffsetRect(R, pWidth div 2, 0)
      else
        OffsetRect(R, 0, pHeight div 2);
    end;
    if PtInRect(R, P) <> Down then
    begin
      Down := not Down;
      Invalidate;
    end;
  end;
end;

//------------------------------------------------------------------ WMLButtonUp
procedure TScroller.WMLButtonUp(var Message: TWMLButtonUp);
var
  NewPos: Longint;
begin
  ReleaseCapture;
  Pressed := False;

  if Down then
  begin
    Down := False;
    NewPos := Position;
    case Current of
      sbLeft: Dec(NewPos, Change);
      sbRight: Inc(NewPos, Change);
    end;
    Position := NewPos;
  end;
end;

//------------------------------------------------------------------------------
//-------------------------------{ TTabList }-----------------------------------
//------------------------------------------------------------------------------

function TTabList.Add(const S: string): Integer;
begin
  Result := inherited Add(S);
  if Tabs <> nil then
    Tabs.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TTabList.Insert(Index: Integer; const S: string);
begin
  inherited Insert(Index, S);
  if Tabs <> nil then
  begin
    if Index <= Tabs.FTabIndex then Inc(Tabs.FTabIndex);
    Tabs.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TTabList.Delete(Index: Integer);
var
  OldIndex: Integer;
begin
  OldIndex := Tabs.Tabindex;
  inherited Delete(Index);

  if OldIndex < Count then
    Tabs.FTabIndex := OldIndex
  else
    Tabs.FTabIndex := Count - 1;

  Tabs.Invalidate;
  Tabs.Invalidate;
  if OldIndex = Index then Tabs.Click;  { deleted selected tab }
end;

//------------------------------------------------------------------------------
procedure TTabList.Put(Index: Integer; const S: string);
begin
  inherited Put(Index, S);
  if Tabs <> nil then
    Tabs.Invalidate;
end;
//------------------------------------------------------------------------------
procedure TTabList.Clear;
begin
  inherited Clear;
  Tabs.FTabIndex := -1;
  Tabs.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TTabList.AddStrings(Strings: TStrings);
begin
  SendMessage(Tabs.Handle, WM_SETREDRAW, 0, 0);
  inherited AddStrings(Strings);
  SendMessage(Tabs.Handle, WM_SETREDRAW, 1, 0);
  Tabs.Invalidate;
end;

//------------------------------------------------------------------------------
//-------------------------{ TTabCollectionItem }-------------------------------
//------------------------------------------------------------------------------
constructor TTabCollectionItem.Create(Collection: TCollection);
begin
  inherited;
  if Assigned(TTabCollection(Collection).AdvTabSet) and (csDesigning in TTabCollection(Collection).AdvTabSet.ComponentState) and not (csLoading in TTabCollection(Collection).AdvTabSet.ComponentState) then
  FCaption:= 'TabSheet';
  FVisible:= true;
  FEnable:= true;

  FChildForm := nil;
  FOnActivateForm := nil;
  FOnDestroyForm := nil;

  FIndexInAdvTabSet:= TTabCollection(Collection).AdvTabSet.Tabs.Add(FCaption);
  FDispOrder := FIndexInAdvTabSet;
  if (csDesigning in TTabCollection(Collection).AdvTabSet.ComponentState) and not (csLoading in TTabCollection(Collection).AdvTabSet.ComponentState) then
  FCaption := FCaption + inttostr(FIndexInAdvTabSet + 1);
  DisplayName:= FCaption;

  FTextColor := TTabCollection(Collection).AdvTabSet.TextColor;
  FTabColor := TTabCollection(Collection).AdvTabSet.UnselectedColor;
  FTabColorTo := TTabCollection(Collection).AdvTabSet.UnSelectedColorTo;
  FTabGradientDirection := TTabCollection(Collection).AdvTabSet.GradientDirection;
  FHoverGradientDirection := TTabCollection(Collection).AdvTabSet.HoverGradientDirection;

  TTabCollection(Collection).AdvTabSet.Tabs[FIndexInAdvTabSet] := FCaption;
  //TTabCollection(Collection).AdvTabSet.Tabs.Objects[FIndexInAdvTabSet]:= pointer(ID); // this is romoved by others
  TTabCollection(Collection).AdvTabSet.DuplicateTabs.InsertObject(FIndexInAdvTabSet,FCaption,pointer(Index{ID}));

  TTabCollection(Collection).AdvTabSet.TabIndex := 0;
  TTabCollection(Collection).AdvTabSet.AdjustTabWidth;
  FVisIndex := Index;
  //TTabCollection(Collection).AdvTabSet.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TTabCollectionItem.Assign(Source: TPersistent);
begin
  if (Source is TTabCollectionItem) then
  begin
    FCaption := (Source as TTabCollectionItem).Caption;
    FVisible := (Source as TTabCollectionItem).Visible;
    FShowClose := (Source as TTabCollectionItem).ShowClose;
    FTextColor := (Source as TTabCollectionItem).TextColor;
    FTabColor := (Source as TTabCollectionItem).TabColor;
    FTabColorTo := (Source as TTabCollectionItem).TabColorTo;
    FHoverGradientDirection := (Source as TTabCollectionItem).HoverGradientDirection;
    FTabGradientDirection := (Source as TTabCollectionItem).TabGradientDirection;
    FImageIndex := (Source as TTabCollectionItem).ImageIndex;
    FTag := (Source as TTabCollectionItem).Tag;
  end;
end;


//------------------------------------------------------------------------------
destructor TTabCollectionItem.Destroy;
var
  i: integer;
begin
  if (csDestroying in TTabCollection(Collection).AdvTabSet.ComponentState) then
  begin
    inherited;
    Exit;
  end;

  if self.Visible then
  begin
    TTabCollection(Collection).AdvTabSet.Tabs.Delete(FIndexInAdvTabSet);
    TTabCollection(Collection).AdvTabSet.DuplicateTabs.Delete(FIndexInAdvTabSet);
    for i := 0 to TTabCollection(Collection).Count - 1 do
    begin
      if TTabCollection(Collection).Items[i].Visible and (TTabCollection(Collection).Items[i].FIndexInAdvTabSet > FIndexInAdvTabSet) then
      begin
        Dec(TTabCollection(Collection).Items[i].FIndexInAdvTabSet);
      end;

      if (TTabCollection(Collection).Items[i].FDispOrder > FDispOrder) then
        Dec(TTabCollection(Collection).Items[i].FDispOrder);
    end;
  end;

  for i := 0 to TTabCollection(Collection).AdvTabSet.DuplicateTabs.Count - 1 do
  begin
    if Integer(TTabCollection(Collection).AdvTabSet.DuplicateTabs.Objects[i]) > Index then
    begin
      TTabCollection(Collection).AdvTabSet.DuplicateTabs.Objects[i] := pointer(Integer(TTabCollection(Collection).AdvTabSet.DuplicateTabs.Objects[i])-1);

    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TTabCollectionItem.SetCaption(const Value: TCaption);
begin
  {if (Value = '') then
    FCaption := ' '
  else}
  FCaption := Value;
  TTabCollection(Collection).AdvTabSet.Tabs[FIndexInAdvTabSet{Index}]:= Value;
  TTabCollection(Collection).AdvTabSet.AdjustTabWidth;
  TTabCollection(Collection).AdvTabSet.UpdateScroller;
  //TTabCollection(Collection).AdvTabSet.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TTabCollectionItem.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  TTabCollection(Collection).AdvTabSet.Invalidate;
end;

//------------------------------------------------------------------------------
function TTabCollectionItem.GetDisplayName: string;
begin
  Result := 'Tab '+IntToStr(Index)+' : '+ Caption;
end;

//------------------------------------------------------------------------------

procedure TTabCollectionItem.SetEnable(const Value: boolean);
begin
  if FEnable <> Value then
  begin
    if Value then
    begin
      FEnable := Value;
    end
    else // false
    begin
      if FVisible then
      begin
        if TTabCollection(Collection).AdvTabSet.TabIndex = FIndexInAdvTabSet then
          raise Exception.Create('Can not disable active tab');
        FEnable := Value;
      end;
    end;
    TTabCollection(Collection).AdvTabSet.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TTabCollectionItem.SetVisible(const Value: Boolean);
var
  i: integer;
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    if Value then
    begin
      //i := FIndexInAdvTabSet; //TTabCollection(Collection).AdvTabSet.RealToDisplTabIndex(Index);
      i := Min(FDispOrder, TTabCollection(Collection).AdvTabSet.Tabs.Count);
      TTabCollection(Collection).AdvTabSet.Tabs.Insert(i,FCaption);
      FIndexInAdvTabSet := i;

      //FIndexInAdvTabSet:= TTabCollection(Collection).AdvTabSet.Tabs.Add(FCaption);
      TTabCollection(Collection).AdvTabSet.DuplicateTabs.InsertObject(FIndexInAdvTabSet,FCaption,pointer(Index{ID}));

      for i:=0 to TTabCollection(Collection).Count-1 do
      begin
        if (i <> Index) and TTabCollection(Collection).Items[i].Visible and (TTabCollection(Collection).Items[i].FIndexInAdvTabSet >= FIndexInAdvTabSet) then
          Inc(TTabCollection(Collection).Items[i].FIndexInAdvTabSet);
      end;

      TTabCollection(Collection).ValidateVisTabIndexes;
      TTabCollection(Collection).AdvTabSet.AdjustTabWidth;
    end
    else
    begin
      TTabCollection(Collection).AdvTabSet.Tabs.Delete(FIndexInAdvTabSet);
      TTabCollection(Collection).AdvTabSet.DuplicateTabs.Delete(FIndexInAdvTabSet);
      for i := 0 to TTabCollection(Collection).Count - 1 do
      begin
        if (i <> Index) and TTabCollection(Collection).Items[i].Visible and (TTabCollection(Collection).Items[i].FIndexInAdvTabSet > FIndexInAdvTabSet) then
          Dec(TTabCollection(Collection).Items[i].FIndexInAdvTabSet);
      end;
    end;
    TTabCollection(Collection).AdvTabSet.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabCollectionItem.SetIndex(Value: Integer);
var
  OldIndex: Integer;
begin
  OldIndex := Index;
  Inherited SetIndex(Value);
  if (OldIndex <> Index) then
  begin
    TTabCollection(Collection).UpdateItemsIndexesInTabSet;
    FVisIndex := Index;
  end;
end;

//------------------------------------------------------------------------------
function TTabCollectionItem.MoveItemInTabSetTo(ItemIndexOfTabSet: integer): Boolean;
var
  i: integer;
begin
  Result:= False;
  if ItemIndexOfTabSet >= TTabCollection(Collection).AdvTabSet.Tabs.Count then
    raise exception.Create('Invalid Index');

  if ItemIndexOfTabSet <> FIndexInAdvTabSet then
  begin
    //--------- Delete from TabSet
    TTabCollection(Collection).AdvTabSet.Tabs.Delete(FIndexInAdvTabSet);
    TTabCollection(Collection).AdvTabSet.DuplicateTabs.Delete(FIndexInAdvTabSet);
    for i:=0 to TTabCollection(Collection).Count-1 do
    begin
      if TTabCollection(Collection).Items[i].Visible and (TTabCollection(Collection).Items[i].FIndexInAdvTabSet > FIndexInAdvTabSet) then
        Dec(TTabCollection(Collection).Items[i].FIndexInAdvTabSet);

      if (TTabCollection(Collection).Items[i].FDispOrder > FDispOrder) then
        Dec(TTabCollection(Collection).Items[i].FDispOrder);
    end;
    //--------- Add in TabSet
    FIndexInAdvTabSet:= ItemIndexOfTabSet;
    FDispOrder := FIndexInAdvTabSet;
    TTabCollection(Collection).AdvTabSet.Tabs.Insert(FIndexInAdvTabSet,FCaption);
    //FIndexInAdvTabSet:= TTabCollection(Collection).AdvTabSet.Tabs.Add(FCaption);
    TTabCollection(Collection).AdvTabSet.DuplicateTabs.InsertObject(FIndexInAdvTabSet,FCaption,pointer(Index{ID}));
    for i:=0 to TTabCollection(Collection).Count-1 do
    begin
      if TTabCollection(Collection).Items[i].Visible and (TTabCollection(Collection).Items[i].FIndexInAdvTabSet >= ItemIndexOfTabSet) then
        Inc(TTabCollection(Collection).Items[i].FIndexInAdvTabSet);

      if (TTabCollection(Collection).Items[i].FDispOrder >= FDispOrder) then
        Inc(TTabCollection(Collection).Items[i].FDispOrder);
    end;
    FIndexInAdvTabSet:= ItemIndexOfTabSet;
    FDispOrder := FIndexInAdvTabSet;
    TTabCollection(Collection).AdvTabSet.AdjustTabWidth;
    FVisIndex := ItemIndexOfTabSet;
    Result:= true;
  end;
end;
//------------------------------------------------------------------------------
procedure TTabCollectionItem.SetShowClose(const Value: Boolean);
begin
  FShowClose := Value;
  TTabCollection(Collection).AdvTabSet.AdjustTabWidth;
  TTabCollection(Collection).AdvTabSet.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TTabCollectionItem.SetTextColor(const Value: TColor);
begin
  if Value <> FTextColor then
  begin
    FTextColor := Value;
    TTabCollection(Collection).AdvTabSet.Invalidate;
  end;
end;
//------------------------------------------------------------------------------
procedure TTabCollectionItem.SetTabColor(const Value: TColor);
begin
  if Value <> FTabColor then
  begin
    FTabColor := Value;
    TTabCollection(Collection).AdvTabSet.Invalidate;
  end;
end;
//------------------------------------------------------------------------------
procedure TTabCollectionItem.SetTabColorTo(const Value: TColor);
begin
  if Value <> FTabColorTo then
  begin
    FTabColorTo := Value;
    TTabCollection(Collection).AdvTabSet.Invalidate;
  end;
end;
//------------------------------------------------------------------------------
procedure TTabCollectionItem.SetTabGradientDirection(value: TGradientDirection);
begin
  if Value <> FTabGradientDirection then
  begin
    FTabGradientDirection := Value;
    TTabCollection(Collection).AdvTabSet.Invalidate;
  end;
end;
//------------------------------------------------------------------------------
procedure TTabCollectionItem.SetHoverGradientDirection(value: TGradientDirection);
begin
  if Value <> FHoverGradientDirection then
  begin
    FHoverGradientDirection := Value;
    TTabCollection(Collection).AdvTabSet.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
//----------------------------{ TTabCollection}---------------------------------
//------------------------------------------------------------------------------

function TTabCollection.Add: TTabCollectionItem;
begin
  Result := TTabCollectionItem(inherited Add);
end;

//------------------------------------------------------------------------------

constructor TTabCollection.Create(AOwner: TAdvCustomTabSet);
begin
  inherited Create(TTabCollectionItem);
  FOwner := AOwner;
end;

//------------------------------------------------------------------------------
function TTabCollection.GetItem(Index: Integer): TTabCollectionItem;
begin
  Result := TTabCollectionItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------
function TTabCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

function TTabCollection.Insert(Index: Integer): TTabCollectionItem;
begin
  Result := TTabCollectionItem(inherited Insert(Index));
  Result.MoveItemInTabSetTo(Index);
end;

//------------------------------------------------------------------------------

procedure TTabCollection.SetItem(Index: Integer; const Value: TTabCollectionItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

procedure TTabCollection.UpdateItemsIndexesInTabSet;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
  begin
    AdvTabSet.DuplicateTabs.Objects[Items[i].FIndexInAdvTabSet] := pointer(Items[i].Index);
  end;
end;

//------------------------------------------------------------------------------

procedure TTabCollection.ValidateVisTabIndexes;
var
  i, j, ixT: Integer;
begin
  if (csLoading in FOwner.ComponentState) or (csDesigning in FOwner.ComponentState) then
    Exit;

  for i := 0 to Count- 1 do
  begin
    if (not Items[i].Visible) then
      continue;
    ixt := i;
    for j := i + 1 to Count - 1 do
    begin
      if (not Items[j].Visible) then
        continue;
      if (Items[ixt].VisIndex < Items[j].VisIndex) and (Items[ixt].FIndexInAdvTabSet >= Items[j].FIndexInAdvTabSet) then
      begin
        ixt := j;
      end;
    end;

    if (i <> ixt) then
    begin
      Items[ixt].MoveItemInTabSetTo(Items[i].FIndexInAdvTabSet);
    end;
  end;
end;

//------------------------------------------------------------------------------
//-------------------------------{ TTabMargin}----------------------------------
//------------------------------------------------------------------------------
procedure TTabMargin.Assign(Source: TPersistent);
begin
  if Source is TTabMargin then
  begin
    FLeftMargin := TTabMargin(Source).LeftMargin;
    FTopMargin := TTabMargin(Source).TopMargin;
    FRightMargin := TTabMargin(Source).RightMargin;
    inherited Assign(Source);
  end;
end;

//------------------------------------------------------------------------------
procedure TTabMargin.SetMargin(Index: integer; Value: TMarginSize);
begin
  case Index of
    0:
      if Value <> FLeftMargin then
      begin
        if Assigned(FOnMarginChange) then FOnMarginChange(Value, FLeftMargin, 0);
        FLeftMargin := Value;
      end;
    1:
      if Value <> FTopMargin then
      begin
        if Assigned(FOnMarginChange) then FOnMarginChange(Value, FTopMargin, 1);
        FTopMargin := Value;
      end;
    2:
      if Value <> FRightMargin then
      begin
        FRightMargin := Value;
        if Assigned(FOnMarginChange) then FOnMarginChange(Value, FRightMargin, 2);
        //FRightMargin := Value;
      end;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------{ TAdvCustomTabSet }----------------------------------
//------------------------------------------------------------------------------
constructor TAdvCustomTabSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csDoubleClicks, csOpaque];
  Width := 185;
  Height := 21;

  TabPositions := TList.Create;
  FTabHeight := 20;

  FTabs := TTabList.Create;
  TTabList(FTabs).Tabs := Self;
  InitBitmaps;

  { initialize default values }
  FSelectedColor := clBtnFace;
  FUnselectedColor := clWindow;
  FBackgroundColor := clBtnFace;
  FDitherBackground := false; //True;
  CreateBrushPattern(BrushBitmap);
  FAutoScroll := True;


  CreateScroller;

  FTabIndex := -1;
  FFirstIndex := 0;
  FVisibleTabs := 0;  { set by draw routine }
  FStartMargin := 5;
  FEndMargin := 5;


  //FStyle := tsStandard;
  FStyle := tsOwnerDraw;
  FOwnerDrawHeight := 20;

  ParentFont := False;
  Font.Name := string(DefFontData.Name);
  Font.Height := DefFontData.Height;
  Font.Style := [];

  AdvTabs := TTabCollection.Create(Self);
  FDuplicateTabs := TStringList.Create;
  FHoverClosedButton := false;

  FSelectedColorTo := clNone;
  FUnSelectedColorTo := clNone;
  FTextColor := clBlack;
  FTabBorderColor := clGray;

  FTabBackGround := TBitmap.Create;
  FTabBackGroundSelected := TBitmap.Create;

  FGradientDirection := gdVertical;
  FHoverGradientDirection := gdVertical;
  FTabHoverColor := clnone;
  FTabHoverColorTo := clnone;
  FTabHoverBorder := clNone;
  FHoverTab := -1;

  FTabMargin := TTabMargin.Create;
  FTabMargin.LeftMargin := 2;
  FTabMargin.TopMargin := 2;
  FTabMargin.RightMargin := 0;
  FTabMargin.OnMarginChange := TabMarginChange;

  FLowerSelected := 2;

  FTabOverlap := 0;
  FShowFocus := false;
  FAdvTabStyle := tsClassic;

  FTabPosition := pTop;
  FCloseButtonAt := cbTabs;
  FCloseButtonDown := false;
  FTabRearrange := false;

  FShowScroller := ssAlways;

  FCloseGlyph := TBitmap.Create;
  FCloseGlyph.Transparent := True;
  FCloseGlyph.TransparentMode := tmAuto;
  FCloseGlyph.OnChange := CloseGlyphOnChange;
  FDisableCloseGlyph := TBitmap.Create;
  FDisableCloseGlyph.PixelFormat := pf32bit;
  FClosePosition := cpLeft;

  // make sure to use a Truetype font
  Font.Name := 'Tahoma';
  FActiveFont := TFont.Create;
  FActiveFont.Name := 'Tahoma';

  { create the edge bitmaps }
  CreateEdgeParts;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_VREDRAW or CS_HREDRAW);
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.CreateScroller;
begin
  FScroller := TScroller.Create(Self);
  with Scroller do
  begin
    Parent := Self;
    Top := 3;
    Min := 0;
    Max := 0;
    Position := 0;
    Visible := False;
    OnClick := ScrollClick;
  end;
  FScroller.Color:= ColorToRGB(BackgroundColor);
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.InitBitmaps;
begin
  MemBitmap := TBitmap.Create;
  BrushBitmap := TBitmap.Create;
end;

//------------------------------------------------------------------------------
destructor TAdvCustomTabSet.Destroy;
begin
  FDisableCloseGlyph.Free;
  FCloseGlyph.Free;
  FTabMargin.Free;
  FTabBackGround.Free;
  FTabBackGroundSelected.Free;
  FDuplicateTabs.Free;
  FScroller.Free;
  FTabs.Free;
  TabPositions.Free;
  DoneBitmaps;
  FActiveFont.Free;
  FAdvTabs.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.DoneBitmaps;
begin
  MemBitmap.Free;
  BrushBitmap.Free;
  ImageList.Free;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.ScrollClick(Sender: TObject);
begin
  FirstIndex := TScroller(Sender).Position;
end;

//------------------------------------------------------------------------------
{ cache the tab position data, and return number of visible tabs }
function TAdvCustomTabSet.CalcTabPositions(Start, Stop: Integer; Canvas: TCanvas;
  First: Integer): Integer;
var
  Index: Integer;
  TabPos: TTabPos;
begin
  TabPositions.Count := 0;  { erase all previously cached data }
  Index := First;
  while (Start < Stop) and (Index < Tabs.Count) do
    with Canvas do
    begin
      TabPos.StartPos := Start;
      TabPos.Size := ItemWidth(Index);
      Inc(Start, TabPos.Size + EdgeWidth);    { next usable position }

      if Start <= Stop then
      begin
        TabPositions.Add(Pointer(TabPos));    { add to list }
        Inc(Index);
      end;
    end;
  Result := Index - First;
end;


function TAdvCustomTabSet.ItemIndex(Pos: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to AdvTabs.Count - 1 do
  begin
    if AdvTabs[i].FIndexInAdvTabSet = Pos then
    begin
      Result := i;
      break;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TAdvCustomTabSet.ItemAtPos(Pos: TPoint): Integer;
var
  TabPos: TTabPos;
  I: Integer;
begin
  Result := -1;
  if (TabPosition in [pTop, pBottom]) and ((Pos.Y < 0) or (Pos.Y > ClientHeight)) then Exit;
  if (TabPosition in [pLeft, pRight]) and ((Pos.X < 0) or (Pos.X > ClientWidth)) then Exit;
  for I := 0 to TabPositions.Count - 1 do
  begin
    Pointer(TabPos) := TabPositions[I];

    if TabPosition in [pTop, pBottom] then
    begin
      if (Pos.X >= TabPos.StartPos) and (Pos.X <= TabPos.StartPos + TabPos.Size + EdgeWidth) then
      begin
        Result := I + FirstIndex;
        Exit;
      end;
    end
    else // pLeft, pRight
    begin
      if (Pos.Y >= TabPos.StartPos) and (Pos.Y <= TabPos.StartPos + TabPos.Size+ EdgeWidth) then
      begin
        Result := I + FirstIndex;
        Exit;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TAdvCustomTabSet.ItemRect(Item: Integer): TRect;
var
  TabPos: TTabPos;
begin
  if (TabPositions.Count > 0) and (Item >= 0) and (Item < TabPositions.Count) then
  begin
    Pointer(TabPos) := TabPositions[Item];

    Result := Rect(TabPos.StartPos, 0, TabPos.StartPos + TabPos.Size, FTabHeight);
    InflateRect(Result, 1, -2);
  end
  else
    Result := Rect(0, 0, 0, 0);
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  P: TPoint;
  i: integer;
begin
  inherited;

  if TabRearrange then
  begin
    Accept := false;

    if Source = self then
    begin
      if TabPosition in[pTop, pBottom] then
        if (Y > FTabHeight) then
          Exit;

      if TabPosition in[pLeft, pRight] then
        if (X > FTabHeight) then
          Exit;

      P.X:= X;
      P.Y:= Y;
      i := ItemAtPos(P);
      if i >= 0 then
        Accept := true;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.DragDrop(Source: TObject; X, Y: Integer);
var
  i, ItemIndexToBeDroped, DesIndex, FromPos: integer;
  TabPos: TTabPos;
  P: TPoint;
begin
  inherited;
  P.X := X;
  P.Y := Y;
  i := ItemAtPos(P);

  if (i>=0) and (i <> FTabIndex) then
  begin
    FromPos:= FTabIndex;
    ItemIndexToBeDroped := integer(FDuplicateTabs.Objects[FTabIndex]);
    Pointer(TabPos) := TabPositions[i - FirstIndex];

    if TabPosition in [pTop, pBottom] then
    begin
      //r := Rect(TabPos.StartPos, 0, TabPos.StartPos + TabPos.Size + EdgeWidth, FTabHeight);
      if X >= (TabPos.StartPos + (TabPos.Size + EdgeWidth) div 2) then
      begin
        if FromPos{ItemIndexToBeDroped }< i then
          DesIndex:= i
        else
          DesIndex:= i+1;
      end
      else
      begin
        if FromPos{ItemIndexToBeDroped} < i then
          DesIndex:= i-1
        else
          DesIndex:= i;
      end;
    end
    else  // pLeft, PRight
    begin
      if Y >= (TabPos.StartPos + (TabPos.Size + EdgeWidth) div 2) then
      begin
        if FromPos{ItemIndexToBeDroped} < i then
          DesIndex:= i
        else
          DesIndex:= i+1;
      end
      else
      begin
        if FromPos{ItemIndexToBeDroped} < i then
          DesIndex:= i-1
        else
          DesIndex:= i;
      end;
    end;

    if FAdvTabs.Items[ItemIndexToBeDroped].MoveItemInTabSetTo(DesIndex) then
    begin
      SetTabIndex(DesIndex);
      if Assigned(FOnTabMoved) then
        FOnTabMoved(self,FromPos,DesIndex);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.Paint;
var
  TabStart, LastTabPos: Integer;
  TabPos: TTabPos;
  Tab: Integer;
  isSelected: Boolean;
  R, TabR, TextR: TRect;
  clr, clrto: TColor;
  HorizontalGradient: Boolean;
  SelectedTabIndex:integer;
  OverLapSelectedRect, HoverTabRect: TRect;
  tbmp: TBitmap;
  RealItemIndex: integer;
  ShowHoverTab, isLast, isNextSelected: Boolean;
  MinLeft, MaxRight: integer;
  MinTop, MaxBottom: integer;
  tf: TFont;
  lf: TLogFont;
  EnableCloseButton: Boolean;
  DownSelected: integer;
  cbr: TRect;

  procedure DrawFocusRectangle(aCanvas: TCanvas; aRect: TRect; Active: Boolean; OverLapDiff: integer);
  begin
    //------- Draw Focus
    if not (csDesigning in ComponentState) then
    begin
      with aCanvas do
      begin
        if FShowFocus and Active {and Focused} then
        begin
          Brush.Style := bsClear;
          Pen.Color := clBlack;
          Pen.Style := psDot;
          Rectangle(aRect.Left + 5, aRect.Top + 3, aRect.Right - 5 - OverLapDiff, aRect.Bottom - 3);
          Pen.Style := pssolid;
        end;
      end;
    end;
  end;

begin
  if not HandleAllocated then Exit;
  { Set the size of the off-screen bitmap.  Make sure that it is tall enough to
    display the entire tab, even if the screen won't display it all.  This is
    required to avoid problems with using FloodFill. }

  if TabPosition in [pTop, pBottom] then
  begin
    MemBitmap.Width := ClientWidth;

    if ClientHeight < FTabHeight + 5 then
      MemBitmap.Height := FTabHeight + 5
    else MemBitmap.Height := ClientHeight;

    MemBitmap.Canvas.Font := Self.Canvas.Font;

    TabStart := StartMargin + EdgeWidth;        { where does first text appear? }
    LastTabPos := Width - EndMargin;
    if FShowScroller = ssAlways then
      LastTabPos:= LastTabPos - ScrollLength{Scroller.Width} - FTabOverlap {+ 2};
    //Scroller.Left := Width - Scroller.Width - 2;
    if FCloseButtonAt = cbTabSet then
    begin
      LastTabPos := LastTabPos - CloseButtonWidth;     { tabs draw until this position }
      //Scroller.Left := Scroller.Left - CloseButtonWidth;
      Scroller.Left := Width - Scroller.Width - 2- CloseButtonWidth;
    end
    else
      Scroller.Left := Width - Scroller.Width - 2;
    Scroller.Top:= 3;
  end
  else // if TabPosition in [pLeft, pRight] then
  begin
    MemBitmap.Height := ClientHeight;// ClientWidth;

    if ClientWidth  < FTabHeight + 5 then
      MemBitmap.Width  := FTabHeight + 5
    else MemBitmap.Width  := ClientWidth; //ClientHeight;

    MemBitmap.Canvas.Font := Self.Canvas.Font;

    TabStart := StartMargin + EdgeWidth;        { where does first text appear? }
    LastTabPos := Height - EndMargin;            { tabs draw until this position }
    if FShowScroller = ssAlways then
      LastTabPos:= LastTabPos - {Scroller.Height}ScrollLength - FTabOverlap{ + 2};
    Scroller.Left:= 3;

    if FCloseButtonAt = cbTabSet then
    begin
      LastTabPos := LastTabPos - CloseButtonHeight - 2;
      Scroller.Top := Height - Scroller.Height - 2 - CloseButtonHeight;
    end
    else
      Scroller.Top := Height - Scroller.Height - 2;
  end;

  { do initial calculations for how many tabs are visible }
  FVisibleTabs := CalcTabPositions(TabStart, LastTabPos, MemBitmap.Canvas,
    FirstIndex);

  { enable the scroller if FAutoScroll = True and not all tabs are visible }
  if AutoScroll and ((ShowScroller = ssAlways) or ((ShowScroller = ssAuto) and (FVisibleTabs < Tabs.Count))){and (FVisibleTabs < Tabs.Count) }then
  begin
    if ShowScroller <> ssAlways then
    begin
      if TabPosition in [pTop, pBottom] then
        Dec(LastTabPos, Scroller.Width {- 2} + FTabOverlap)
      else
        Dec(LastTabPos, Scroller.Height {- 2} + FTabOverlap);
    end;

    { recalc the tab positions }
    FVisibleTabs := CalcTabPositions(TabStart, LastTabPos, MemBitmap.Canvas,
      FirstIndex);

    { set the scroller's range }
    Scroller.Visible := True;
    ShowWindow(Scroller.Handle, SW_SHOW);
    Scroller.Min := 0;
    Scroller.Max := Tabs.Count - VisibleTabs;
    Scroller.Position := FirstIndex;
  end
  else
    if (VisibleTabs >= Tabs.Count) and (ShowScroller = ssAuto) then
    begin
      Scroller.Visible := False;
      ShowWindow(Scroller.Handle, SW_HIDE);
    end;

  if FDoFix then
  begin
    FixTabPos;
    FVisibleTabs := CalcTabPositions(TabStart, LastTabPos, MemBitmap.Canvas,
      FirstIndex);
  end;
  FDoFix := False;

  { draw background of tab area }
  with MemBitmap.Canvas do
  begin
    if Assigned(FOnDrawTabSetBackground) then
    begin
      FOnDrawTabSetBackground(Self, MemBitmap.Canvas, Rect(0, 0, MemBitmap.Width, MemBitmap.Height));
    end
    else
    begin
      Brush.Bitmap := BrushBitmap;
      FillRect(Rect(0, 0, MemBitmap.Width, MemBitmap.Height));
      Brush.Bitmap := nil;
    end;
  end;

  SelectedTabIndex := -1;
  tbmp := TBitmap.Create;
  if not FTabBackGround.Empty then
  begin
    tbmp.Width := FTabBackGround.Width;
    tbmp.Height := FTabBackGround.Height;
  end;

  ShowHoverTab:= false;
  MinLeft:= Width;
  MaxRight:= 0;

  MinTop:= Height;
  MaxBottom:= 0;

  EnableCloseButton:= false;

  for Tab := 0 to TabPositions.Count - 1 do
  begin
    Pointer(TabPos) := TabPositions[Tab];
    isSelected := Tab + FirstIndex = TabIndex;

    isLast := Tab = VisibleTabs - 1;
    isNextSelected := (Tab + FirstIndex) + 1 = TabIndex;

    RealItemIndex := integer(FDuplicateTabs.Objects[Tab + FirstIndex]);

    { set up the canvas }
    if TabPosition in [pTop, pBottom] then
      R := Rect(TabPos.StartPos, 0, TabPos.StartPos + TabPos.Size, FTabHeight)
    else // if TabPosition in [pLeft, pRight] then
      R := Rect(0,TabPos.StartPos, FTabHeight, TabPos.StartPos + TabPos.Size);

    { restore font for drawing the text }
    MemBitmap.Canvas.Font := Self.Canvas.Font;

    //Selecting paiting color and gradient direction to start paiting
    //HorizontalGradient:= FGradientDirection = gdHorizontal;
    if FAdvTabs.Items[RealItemIndex].TabGradientDirection = gdVertical then
      HorizontalGradient := false
    else
      HorizontalGradient := true;

    if isSelected then
    begin
      clr := FSelectedColor;
      clrto := FSelectedColorTo;
    end
    else
    begin
      if (FHoverTab = (Tab+FirstIndex)) and ((TabHoverColorTo <> clNone) or (TabHoverColor <> clNone)) then
      begin
        clr := TabHoverColor;
        clrto := TabHoverColorTo;
        //HorizontalGradient := FHoverGradientDirection = gdHorizontal;
        if FAdvTabs.Items[RealItemIndex].HoverGradientDirection = gdVertical then
          HorizontalGradient := false
        else
          HorizontalGradient := true;
        ShowHoverTab:= true;
        HoverTabRect := R;
      end
      else
      begin
        clr := FAdvTabs.Items[RealItemIndex].FTabColor; //FUnSelectedColor;
        clrto := FAdvTabs.Items[RealItemIndex].TabColorTo; //FUnSelectedColorTo;
      end;
    end;

    TabR:= R;

    MemBitmap.Canvas.Brush.Color := clr;

    // Top Position
    if TabPosition in [pTop,pBottom] then
    begin
      TabR.Right:= TabR.Right + EdgeWidth + 1;
      TextR:= TabR;

      if not FTabBackGround.Empty then
      begin
        if isSelected and not FTabBackGroundSelected.Empty then
        begin
          SelectedTabIndex := Tab + FirstIndex;
          OverLapSelectedRect := TabR;
        end
        else
        begin
          tbmp.Canvas.Draw(0, 0, FTabBackGround);
          BitmapStretchInWidth(tbmp,MemBitmap.Canvas, TabR.Left, TabR.Top, TabR.Right - TabR.Left + FTabOverlap);
        end
      end
      else
      begin
        if isSelected and (FAdvTabStyle = tsDotNet) then
          TabR.Bottom := TabR.Bottom + 1;

        if ClrTo = clNone then
        begin
          MemBitmap.Canvas.FillRect(TabR);
        end
        else
        begin
          if HorizontalGradient then
            TabR.Right := TabR.Right - 1
          else
            TabR.Bottom := TabR.Bottom - 1;

          DrawGradient(MemBitmap.Canvas, clr, clrto, 16, TabR, HorizontalGradient);

          if HorizontalGradient then
            TabR.Right := TabR.Right + 1
          else
            TabR.Bottom := TabR.Bottom + 1;
        end;

        MemBitmap.Canvas.Brush.Style := bsClear;
        MemBitmap.Canvas.Pen.Color := FTabBorderColor;

        if FAdvTabStyle = tsDotNet then
        begin
          if MinLeft > TabR.Left then
            MinLeft:= TabR.Left;
          if MaxRight < TabR.Right then
            MaxRight:= TabR.Right;

          if TabPosition = pTop then
          begin
            if isSelected then
            begin
              MemBitmap.Canvas.Pen.Color := FTabBorderColor; //clBlack;
              MemBitmap.Canvas.MoveTo(TabR.Left, TabR.Bottom);
              MemBitmap.Canvas.LineTo(TabR.Left, TabR.Top);

              if isLast then
              begin
                MemBitmap.Canvas.LineTo(TabR.Right-1, TabR.Top);
                MemBitmap.Canvas.LineTo(TabR.Right-1, TabR.Bottom+1);
              end
              else
              begin
                MemBitmap.Canvas.LineTo(TabR.Right-2, TabR.Top);
                MemBitmap.Canvas.LineTo(TabR.Right-2, TabR.Bottom+1);
              end;
            end
            else
            begin
              MemBitmap.Canvas.Pen.Color := FTabBorderColor;//clBlack;
              MemBitmap.Canvas.MoveTo(TabR.Left, TabR.Bottom);
              MemBitmap.Canvas.LineTo(TabR.Right, TabR.Bottom);

              if not isNextSelected {and not isLast} then
              begin
                MemBitmap.Canvas.Pen.Color := SelectedColor;
                MemBitmap.Canvas.MoveTo(TabR.Right-2, TabR.Bottom-3);
                MemBitmap.Canvas.LineTo(TabR.Right-2, TabR.Top+2);
                MemBitmap.Canvas.MoveTo(TabR.Right-3, TabR.Bottom-3);
                MemBitmap.Canvas.LineTo(TabR.Right-3, TabR.Top+2);
              end;
            end;
          end
          else   // TabPosition = Bottom
          begin
            if isSelected then
            begin
              MemBitmap.Canvas.Pen.Color := FTabBorderColor; //clBlack;
              MemBitmap.Canvas.MoveTo(TabR.Left, TabR.Top);
              MemBitmap.Canvas.LineTo(TabR.Left, TabR.Bottom);

              if isLast then
              begin
                MemBitmap.Canvas.LineTo(TabR.Right-1, TabR.Bottom);
                MemBitmap.Canvas.LineTo(TabR.Right-1, TabR.Top-1);
              end
              else
              begin
                MemBitmap.Canvas.LineTo(TabR.Right-2, TabR.Bottom);
                MemBitmap.Canvas.LineTo(TabR.Right-2, TabR.Top-1);
              end;
            end
            else
            begin
              MemBitmap.Canvas.Pen.Color := FTabBorderColor;//clBlack;
              MemBitmap.Canvas.MoveTo(TabR.Left, TabR.Top);
              MemBitmap.Canvas.LineTo(TabR.Right, TabR.Top);

              if not isNextSelected {and not isLast} then
              begin
                MemBitmap.Canvas.Pen.Color := SelectedColor;
                MemBitmap.Canvas.MoveTo(TabR.Right-2, TabR.Bottom-3);
                MemBitmap.Canvas.LineTo(TabR.Right-2, TabR.Top+2);
                MemBitmap.Canvas.MoveTo(TabR.Right-3, TabR.Bottom-3);
                MemBitmap.Canvas.LineTo(TabR.Right-3, TabR.Top+2);
              end;
            end;
          end;
        end
        else  // FAdvTabStyle = tsClassic
          begin
            MemBitmap.Canvas.Rectangle(TabR.Left, TabR.Top, TabR.Right, TabR.Bottom);
          end;
      end;
    end
    else if TabPosition in [pLeft, pRight] then
    begin
      TabR.Bottom := TabR.Bottom + EdgeWidth + 1;
      TextR := TabR;

      if not FTabBackGround.Empty then
      begin
        if isSelected and not FTabBackGroundSelected.Empty then
        begin
          SelectedTabIndex := Tab + FirstIndex;
          OverLapSelectedRect := TabR;
        end
        else
        begin
          tbmp.Canvas.Draw(0, 0, FTabBackGround);
          BitmapStretch(tbmp,MemBitmap.Canvas, TabR.Left, TabR.Top, TabR.Bottom - TabR.Top + FTabOverlap);
        end
      end
      else
      begin
       // if isSelected and (FAdvTabStyle = tsDotNet) then
       //   TabR.Bottom := TabR.Bottom + 1;

        if ClrTo = clNone then
          MemBitmap.Canvas.FillRect(TabR)
        else
        begin

          if HorizontalGradient then
            TabR.Right := TabR.Right - 1
          else
            TabR.Bottom := TabR.Bottom - 1;

          DrawGradient(MemBitmap.Canvas, clr, clrto, 16, TabR, HorizontalGradient);

          if HorizontalGradient then
            TabR.Right := TabR.Right + 1
          else
            TabR.Bottom := TabR.Bottom + 1;
        end;

        MemBitmap.Canvas.Brush.Style := bsClear;
        MemBitmap.Canvas.Pen.Color := FTabBorderColor;

        if FAdvTabStyle = tsDotNet then
        begin
          if MinTop > TabR.Top then
            MinTop:= TabR.Top;
          if MaxBottom < TabR.Bottom then
            MaxBottom:= TabR.Bottom;

          if TabPosition = pLeft then
          begin
            if isSelected then
            begin
              MemBitmap.Canvas.Pen.Color := FTabBorderColor; //clBlack;
              MemBitmap.Canvas.MoveTo(TabR.Right, TabR.Top);
              MemBitmap.Canvas.LineTo(TabR.Left, TabR.Top);

              if isLast then
              begin
                MemBitmap.Canvas.LineTo(TabR.Left, TabR.Bottom);
                MemBitmap.Canvas.LineTo(TabR.Right, TabR.Bottom);
              end
              else
              begin
                MemBitmap.Canvas.LineTo(TabR.Left, TabR.Bottom-2);
                MemBitmap.Canvas.LineTo(TabR.Right, TabR.Bottom-2);
              end;
            end
            else
            begin
              MemBitmap.Canvas.Pen.Color := FTabBorderColor;//clBlack;
              MemBitmap.Canvas.MoveTo(TabR.Right, TabR.Top);
              MemBitmap.Canvas.LineTo(TabR.Right, TabR.Bottom);

              if not isNextSelected {and not isLast} then
              begin
                MemBitmap.Canvas.Pen.Color := SelectedColor;
                MemBitmap.Canvas.MoveTo(TabR.Right-3, TabR.Bottom-2);
                MemBitmap.Canvas.LineTo(TabR.Left+2, TabR.Bottom-2);
                MemBitmap.Canvas.MoveTo(TabR.Right-3, TabR.Bottom-3);
                MemBitmap.Canvas.LineTo(TabR.Left+2, TabR.Bottom-3);
              end;
            end;
          end
          else   // TabPosition = pRight
          begin
            if isSelected then
            begin
              MemBitmap.Canvas.Pen.Color := FTabBorderColor; //clBlack;
              MemBitmap.Canvas.MoveTo(TabR.Left, TabR.Top);
              MemBitmap.Canvas.LineTo(TabR.Right, TabR.Top);

              if isLast then
              begin
                MemBitmap.Canvas.LineTo(TabR.Right, TabR.Bottom);
                MemBitmap.Canvas.LineTo(TabR.Left, TabR.Bottom);
              end
              else
              begin
                MemBitmap.Canvas.LineTo(TabR.Right, TabR.Bottom-2);
                MemBitmap.Canvas.LineTo(TabR.Left, TabR.Bottom-2);
              end;
            end
            else
            begin
              MemBitmap.Canvas.Pen.Color := FTabBorderColor;//clBlack;
              MemBitmap.Canvas.MoveTo(TabR.Left, TabR.Top);
              MemBitmap.Canvas.LineTo(TabR.Left, TabR.Bottom);

              if not isNextSelected {and not isLast} then
              begin
                MemBitmap.Canvas.Pen.Color := SelectedColor;
                MemBitmap.Canvas.MoveTo(TabR.Right-3, TabR.Bottom-2);
                MemBitmap.Canvas.LineTo(TabR.Left+2, TabR.Bottom-2);
                MemBitmap.Canvas.MoveTo(TabR.Right-3, TabR.Bottom-3);
                MemBitmap.Canvas.LineTo(TabR.Left+2, TabR.Bottom-3);
              end;
            end;
          end;
        end
        else  // FAdvTabStyle = tsClassic
          begin
            MemBitmap.Canvas.Rectangle(TabR.Left, TabR.Top, TabR.Right, TabR.Bottom);
          end;
      end;

    end;

   // Tab position End

//    RealItemIndex := integer(FDuplicateTabs.Objects[Tab + FirstIndex]);
    if FCloseButtonAt = cbTabs then
    begin
      if FAdvTabs.Items[RealItemIndex].ShowClose then
      begin
        cbr := TextR;
        if (ClosePosition = cpRight) then
        begin
          case TabPosition of
            pTop, pBottom:  cbr.Right := cbr.Right - EdgeWidth - 1 + (EdgeWidth div 2);
            //pLeft:  cbr.Top := cbr.Top + (EdgeWidth div 2);
            //pRight: cbr.Bottom := cbr.Bottom - (EdgeWidth div 2);
          end;
        end;

        DrawCloseButton(MemBitmap.Canvas, cbr, isSelected);
        if TabPosition in [pTop, pBottom] then
        begin
          if (ClosePosition = cpLeft) then
            TextR.Left:= TextR.Left + CloseButtonWidth + {2}3
          else
            TextR.Right := TextR.Right - CloseButtonWidth - 3;
        end
        else
        begin
          if TabPosition = pLeft then
          begin
            if (ClosePosition = cpLeft) then
              TextR.Bottom := TextR.Bottom - CloseButtonHeight -3{+ 3}
            else
              TextR.Top := TextR.Top + CloseButtonHeight + 3
          end
          else
          begin
            if (ClosePosition = cpLeft) then
              TextR.Top := TextR.Top + CloseButtonHeight + 3
            else
              TextR.Bottom := TextR.Bottom - CloseButtonHeight - 3;
          end;
        end;
      end;
    end;

    if isSelected and (FCloseButtonAt = cbTabSet) then
      EnableCloseButton:= FAdvTabs.Items[RealItemIndex].ShowClose = true;

    // Margin setting
    if TabPosition in [pTop, pBottom] then
    begin
      TextR.Left := TextR.Left + FTabMargin.LeftMargin;
      TextR.Top := TextR.Top + FTabMargin.TopMargin;

      if TabPosition = pTop then
      begin
        if isSelected then
          TextR.Top:= TextR.Top + {2}FLowerSelected;
      end
      else  // pBottom
      begin
        if isSelected then
          TextR.Top:= TextR.Top + FLowerSelected;
      end;
    end
    else
    begin
      if TabPosition = pLeft then
      begin
        TextR.Bottom := TextR.Bottom - FTabMargin.TopMargin;
        TextR.Left := TextR.Left + FTabMargin.LeftMargin;
        if isSelected then
          TextR.Left:= TextR.Left + {2}FLowerSelected;
      end
      else // pRight
      begin
        TextR.Left := TextR.Left + FTabMargin.LeftMargin + 2;
        TextR.Top := TextR.Top + FTabMargin.TopMargin;
        if isSelected then
          TextR.Left:= TextR.Left - {2}FLowerSelected;
      end;
    end;

    if (Images <> nil) and (FAdvTabs.Items[RealItemIndex].ImageIndex >= 0) then
    begin
      case TabPosition of
        pTop:
        begin
          FImages.Draw(MemBitmap.Canvas, TextR.Left, TextR.Top, FAdvTabs.Items[RealItemIndex].ImageIndex);
          TextR.Left := TextR.Left + FImages.Width + 2;
        end;
        pBottom:
        begin
          FImages.Draw(MemBitmap.Canvas, TextR.Left, TextR.Top, FAdvTabs.Items[RealItemIndex].ImageIndex);
          TextR.Left := TextR.Left + FImages.Width + 2;
        end;
        pLeft:
        begin
          FImages.Draw(MemBitmap.Canvas, TextR.Left, TextR.Bottom - Images.Height, FAdvTabs.Items[RealItemIndex].ImageIndex);
          TextR.Bottom := TextR.Bottom - FImages.Height - 2;
        end;
        pRight:
        begin
          FImages.Draw(MemBitmap.Canvas, TextR.Left, TextR.Top, FAdvTabs.Items[RealItemIndex].ImageIndex);
          TextR.Top := TextR.Top + FImages.Height + 2;
        end;
      end;
    end;

    // Text Orientation
    if Tabposition in [pLeft, pRight] then
    begin
      with MemBitmap.Canvas do
      begin
        tf := TFont.Create;
        try
          FillChar(lf, SizeOf(lf), 0);
          if IsSelected then
            tf.Assign(ActiveFont)
          else
            tf.Assign(Font);
          GetObject(tf.Handle, SizeOf(Lf), @Lf);

          if TabPosition = pLeft then
            lf.lfEscapement := -2700
          else
            lf.lfEscapement := -900;
          lf.lfOrientation := 30;

          tf.Handle := CreateFontIndirect(Lf);
          Font.Assign(tf);
        finally
          tf.Free;
        end;
      end;
    end;


    if TabPosition in [pTop, pBottom] then
    begin
      if isSelected then
        MemBitmap.Canvas.Font.Assign(ActiveFont)
      else
        MemBitmap.Canvas.Font.Assign(Font);
    end;

    MemBitmap.Canvas.Brush.Style := bsClear;
    if not IsSelected then
      MemBitmap.Canvas.Font.Color := FAdvTabs.Items[RealItemIndex].TextColor; //TextColor;

    if not FAdvTabs.Items[RealItemIndex].Enable then
      MemBitmap.Canvas.Font.Color:= clGray;

    if TabPosition in [pTop, pBottom] then
      DrawText(MemBitmap.Canvas.Handle, PChar(FAdvTabs.Items[RealItemIndex].Caption), Length(FAdvTabs.Items[RealItemIndex].Caption), TextR, DT_LEFT)
     else if TabPosition = pLeft then
      MemBitmap.Canvas.TextOut(TextR.Left, TextR.Bottom,PChar(FAdvTabs.Items[RealItemIndex].Caption))

    else // pRight
      MemBitmap.Canvas.TextOut(TextR.Left+ MemBitmap.Canvas.TextHeight('T'), TextR.Top,PChar(FAdvTabs.Items[RealItemIndex].Caption));

    if isSelected then
      MemBitmap.Canvas.Font.Assign(Self.Canvas.Font);

    DrawFocusRectangle(MemBitmap.Canvas, TabR, isSelected, 0);
  end;

  DownSelected := 0;

  if SelectedTabIndex >= 0 then
  begin
    if FLowerSelected > 0 then
      DownSelected := 1;

    tbmp.Canvas.Draw(0, 0, FTabBackGroundSelected);

    TabR := OverLapSelectedRect;

    if TabPosition in [pTop, pBottom] then
    begin
      if TabPosition = pTop then
        BitmapStretchInWidth(tbmp,MemBitmap.Canvas, TabR.Left, TabR.Top + DownSelected, TabR.Right - TabR.Left + FTabOverlap)
      else
        BitmapStretchInWidth(tbmp,MemBitmap.Canvas, TabR.Left, TabR.Top + DownSelected, TabR.Right - TabR.Left + FTabOverlap);
    end
    else
    begin
      if TabPosition = pLeft then
        BitmapStretch(tbmp,MemBitmap.Canvas, TabR.Left + DownSelected, TabR.Top, TabR.Bottom - TabR.Top + FTabOverlap)
      else
        BitmapStretch(tbmp,MemBitmap.Canvas, TabR.Left - DownSelected, TabR.Top, TabR.Bottom - TabR.Top + FTabOverlap);
    end;

    TextR := OverLapSelectedRect;

    RealItemIndex := Integer(FDuplicateTabs.Objects[SelectedTabIndex]);

    if FCloseButtonAt = cbTabs then
    begin
      if FAdvTabs.Items[RealItemIndex].ShowClose then
      begin
        cbr := TextR;
        if (ClosePosition = cpRight) then
        begin
          case TabPosition of
            pTop, pBottom:  cbr.Right := cbr.Right - EdgeWidth - 1 + (EdgeWidth div 2);
            //pLeft:  cbr.Top := cbr.Top + (EdgeWidth div 2);
            //pRight: cbr.Bottom := cbr.Bottom - (EdgeWidth div 2);
          end;
        end;

        DrawCloseButton(MemBitmap.Canvas, cbr, true);
        if TabPosition in [pTop, pBottom] then
        begin
          if (ClosePosition = cpLeft) then
            TextR.Left:= TextR.Left + CloseButtonWidth + {2}3
          else
            TextR.Right := TextR.Right - CloseButtonWidth - 3;
        end
        else
        begin
          if TabPosition = pLeft then
          begin
            if (ClosePosition = cpLeft) then
              TextR.Bottom:= TextR.Bottom - CloseButtonHeight - 3
            else
              TextR.Top := TextR.Top + CloseButtonHeight + 3
          end
          else
          begin
            if (ClosePosition = cpLeft) then
              TextR.Top:= TextR.Top + CloseButtonHeight + 3
            else
              TextR.Bottom := TextR.Bottom - CloseButtonHeight - 3;
          end;
        end;

      end;
    end;

    if TabPosition in [pTop, pBottom] then
    begin
      if TabPosition = pTop then
        TextR.Top:= TextR.Top + {2}FLowerSelected
      else
        TextR.Top:= TextR.Top + {2}FLowerSelected;

      TextR.Left := TextR.Left + FTabMargin.LeftMargin;
      TextR.Top := TextR.Top + FTabMargin.TopMargin;
    end
    else
    begin
      if TabPosition = pLeft then
      begin
        TextR.Left:= TextR.Left + {2}FLowerSelected;
        TextR.Bottom := TextR.Bottom - FTabMargin.TopMargin;
        TextR.Left := TextR.Left + FTabMargin.LeftMargin;
      end
      else
      begin
        //TextR.Left:= TextR.Left + 2;
        TextR.Top := TextR.Top + FTabMargin.TopMargin;
        TextR.Left := TextR.Left + FTabMargin.LeftMargin +2;
        TextR.Left:= TextR.Left - {2}FLowerSelected;
      end;
    end;

    if Images <> nil then
    begin
      if TabPosition in [pTop, pBottom] then
      begin
        FImages.Draw(MemBitmap.Canvas, TextR.Left, TextR.Top, self.FAdvTabs.Items[RealItemIndex].ImageIndex);
        TextR.Left := TextR.Left + FImages.Width + 2;
      end
      else
      begin
        if TabPosition = pLeft then
        begin
          FImages.Draw(MemBitmap.Canvas, TextR.Left, TextR.Bottom - Images.Height, FAdvTabs.Items[RealItemIndex].ImageIndex);
          TextR.Bottom := TextR.Bottom - FImages.Height - 2;
        end
        else //pRight:
        begin
          FImages.Draw(MemBitmap.Canvas, TextR.Left, TextR.Top, FAdvTabs.Items[RealItemIndex].ImageIndex);
          TextR.Top := TextR.Top + FImages.Height + 2;
        end;
      end;
    end;

    // Text Orientation
    if Tabposition in [pLeft, pRight] then
    begin
      with MemBitmap.Canvas do
      begin
        tf := TFont.Create;
        try
          FillChar(lf, SizeOf(lf), 0);
          tf.Assign(ActiveFont);
          GetObject(tf.Handle, SizeOf(Lf), @Lf);

          if TabPosition = pLeft then
            lf.lfEscapement := -2700
          else
            lf.lfEscapement := -900;
          lf.lfOrientation := 30;

          tf.Handle := CreateFontIndirect(Lf);
          Font.Assign(tf);
        finally
          tf.Free;
        end;
      end;
    end
    else
      MemBitmap.Canvas.Font.Assign(ActiveFont);

    //MemBitmap.Canvas.Font := FActiveFont;
    MemBitmap.Canvas.Brush.Style:= bsClear;
    //MemBitmap.Canvas.Font.Color:= FAdvTabs.Items[RealItemIndex].TextColor; //TextColor;

    if TabPosition in [pTop, pBottom] then
      DrawText(MemBitmap.Canvas.Handle, PChar(FAdvTabs.Items[RealItemIndex].Caption), Length(FAdvTabs.Items[RealItemIndex].Caption), TextR, DT_LEFT)
    else if TabPosition = pLeft then
      MemBitmap.Canvas.TextOut(TextR.Left, TextR.Bottom,PChar(FAdvTabs.Items[RealItemIndex].Caption))
    else // pRight
      MemBitmap.Canvas.TextOut(TextR.Left+ MemBitmap.Canvas.TextHeight('T'), TextR.Top,PChar(FAdvTabs.Items[RealItemIndex].Caption));

    MemBitmap.Canvas.Font := Self.Canvas.Font;
    DrawFocusRectangle(MemBitmap.Canvas, OverLapSelectedRect, true, 2);
  end
  else
    if FTabBackGround.Empty and FTabBackGroundSelected.Empty then
    begin
      MemBitmap.Canvas.Pen.Color := FTabBorderColor;
      if TabPosition = pTop then
      begin
        if FAdvTabStyle = tsDotNet then
        begin
          MemBitmap.Canvas.MoveTo(0,TabR.Bottom);
          MemBitmap.Canvas.LineTo(MinLeft,TabR.Bottom);
          MemBitmap.Canvas.MoveTo(MaxRight,TabR.Bottom);
          MemBitmap.Canvas.LineTo(Width,TabR.Bottom);
        end
        else
        begin // AdvTabStyle = tsClassic
          MemBitmap.Canvas.MoveTo(0,TabR.Bottom-1);
          MemBitmap.Canvas.LineTo(Width,TabR.Bottom-1);
        end;
      end
      else if TabPosition = pBottom then
      begin
        if FAdvTabStyle = tsDotNet then
        begin
          MemBitmap.Canvas.MoveTo(0,TabR.Top);
          MemBitmap.Canvas.LineTo(MinLeft,TabR.Top);
          MemBitmap.Canvas.MoveTo(MaxRight,TabR.Top);
          MemBitmap.Canvas.LineTo(Width,TabR.Top);
        end
        else
        begin // AdvTabStyle = tsClassic
          MemBitmap.Canvas.MoveTo(0,TabR.Top);
          MemBitmap.Canvas.LineTo(Width,TabR.Top);
        end;
      end
      else if TabPosition = pLeft then
      begin
        if FAdvTabStyle = tsDotNet then
        begin
          MemBitmap.Canvas.MoveTo(TabR.Right,0);
          MemBitmap.Canvas.LineTo(TabR.Right,MinTop);
          MemBitmap.Canvas.MoveTo(TabR.Right, MaxBottom);
          MemBitmap.Canvas.LineTo(TabR.Right,Height);
        end
        else
        begin // AdvTabStyle = tsClassic
          MemBitmap.Canvas.MoveTo(TabR.Right-1,0);
          MemBitmap.Canvas.LineTo(TabR.Right-1,Height);
        end;
      end
      else if TabPosition = pRight then
      begin
        if FAdvTabStyle = tsDotNet then
        begin
          MemBitmap.Canvas.MoveTo(TabR.Left,0);
          MemBitmap.Canvas.LineTo(TabR.Left,MinTop);
          MemBitmap.Canvas.MoveTo(TabR.Left, MaxBottom);
          MemBitmap.Canvas.LineTo(TabR.Left,Height);
        end
        else
        begin // AdvTabStyle = tsClassic
          MemBitmap.Canvas.MoveTo(TabR.Left,0);
          MemBitmap.Canvas.LineTo(TabR.Left,Height);
        end;
      end;
    end;

  tbmp.Free;

  if ShowHoverTab then
  begin
    if TabPosition in [pTop, pBottom] then
      HoverTabRect.Right:= HoverTabRect.Right + EdgeWidth + 1
    else
      HoverTabRect.Bottom:= HoverTabRect.Bottom + EdgeWidth + 1;

    MemBitmap.Canvas.Pen.Color := FTabHoverBorder;
    MemBitmap.Canvas.Rectangle(HoverTabRect.Left, HoverTabRect.Top, HoverTabRect.Right, HoverTabRect.Bottom)
  end;

  { draw onto the screen }
  Canvas.Draw(0, 0, MemBitmap);

  if FCloseButtonAt = cbTabSet then
  begin
    DrawCloseButtonAtTabSet(EnableCloseButton);
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.CreateEdgeParts;
var
  H: Integer;
  Working: TBitmap;
  EdgePart: TEdgePart;
  MaskColor: TColor;

  procedure DrawUL(Canvas: TCanvas);
  begin
    with Canvas do
    begin
      Pen.Color := clBtnShadow;
      PolyLine([Point(0, 0), Point(EdgeWidth + 1, 0)]);

      Pen.Color := UnselectedColor;
      Brush.Color := UnselectedColor;
      Polygon([Point(3, 1), Point(EdgeWidth - 1, H), Point(EdgeWidth, H),
        Point(EdgeWidth, 1), Point(3, 1)]);

      if SoftTop then
      begin
        Pen.Color := ColorToRGB(BackgroundColor);
        PolyLine([Point(4, 1), Point(EdgeWidth + 1, 1)]);
        Pen.Color := clWindowFrame;
        PolyLine([Point(3, 1), Point(EdgeWidth - 1, H), Point(EdgeWidth, H)]);
      end
      else
      begin
        Pen.Color := clWindowFrame;
        PolyLine([Point(0, 1), Point(EdgeWidth + 1, 1), Point(3, 1),
          Point(EdgeWidth - 1, H), Point(EdgeWidth, H)]);
      end;
      (*Pen.Color := clWindowFrame;
      if SoftTop then
        PolyLine([{Point(0, 1),} Point(EdgeWidth + 1, 1), Point(3, 1),
          Point(EdgeWidth - 1, H), Point(EdgeWidth, H)])
      else
        PolyLine([Point(0, 1), Point(EdgeWidth + 1, 1), Point(3, 1),
          Point(EdgeWidth - 1, H), Point(EdgeWidth, H)]);*)
    end;
  end;

  procedure DrawSL(Canvas: TCanvas);
  begin
    with Canvas do
    begin
      Pen.Color := SelectedColor;
      Brush.Color := SelectedColor;
      Polygon([Point(3, 0), Point(EdgeWidth - 1, H), Point(EdgeWidth, H),
        Point(EdgeWidth, 0), Point(3, 0)]);

      Pen.Color := clBtnShadow;
      PolyLine([Point(0, 0), Point(4, 0)]);

      Pen.Color := clBtnHighlight;
      PolyLine([Point(4, 1), Point(EdgeWidth, H + 1)]);

      Pen.Color := clWindowFrame;
      if SoftTop then
        PolyLine([{Point(0, 1),} Point(3, 1), Point(EdgeWidth - 1, H),
          Point(EdgeWidth, H)])
      else
        PolyLine([Point(0, 1), Point(3, 1), Point(EdgeWidth - 1, H),
          Point(EdgeWidth, H)]);
    end;
  end;

  procedure DrawUR(Canvas: TCanvas);
  begin
    with Canvas do
    begin
      Pen.Color := clBtnShadow;
      PolyLine([Point(-1, 0), Point(EdgeWidth + 1, 0)]);

      Pen.Color := UnselectedColor;
      Brush.Color := UnselectedColor;
      Polygon([Point(EdgeWidth - 3, 1), Point(1, H), Point(0, H),
        Point(0, 1), Point(EdgeWidth - 3, 1)]);

      { workaround for bug in S3 driver }
      Pen.Color := clBtnShadow;
      PolyLine([Point(-1, 0), Point(EdgeWidth + 1, 0)]);

      if SoftTop then
      begin
        Pen.Color := ColorToRGB(BackgroundColor);
        PolyLine([Point(0, 1), Point(EdgeWidth - 1, 1)]);
        Pen.Color := clWindowFrame;
        PolyLine([Point(EdgeWidth - 2, 1), Point(2, H), Point(-1, H)]);
      end
      else
      begin
        Pen.Color := clWindowFrame;
        PolyLine([Point(0, 1), Point(EdgeWidth + 1, 1), Point(EdgeWidth - 2, 1),
          Point(2, H), Point(-1, H)]);
      end;
      (*Pen.Color := clWindowFrame;
      if SoftTop then
        PolyLine([Point(0, 1), {Point(EdgeWidth + 1, 1),} Point(EdgeWidth - 2, 1),
          Point(2, H), Point(-1, H)])
      else
        PolyLine([Point(0, 1), Point(EdgeWidth + 1, 1), Point(EdgeWidth - 2, 1),
          Point(2, H), Point(-1, H)])*)
    end;
  end;

  procedure DrawSR(Canvas: TCanvas);
  begin
    with Canvas do
    begin
      Pen.Color := SelectedColor;
      Brush.Color := SelectedColor;
      Polygon([Point(EdgeWidth - 3, 1), Point(2, H), Point(0, H),
        Point(0, 0), Point(EdgeWidth + 1, 0)]);

      Pen.Color := clBtnShadow;
      PolyLine([Point(EdgeWidth + 1, 0), Point(EdgeWidth - 3, 0),
        Point(EdgeWidth - 3, 1), Point(1, H), Point(-1, H)]);
      //PolyLine([Point(EdgeWidth - 3, 0), Point(EdgeWidth + 1, 0),   { wrong }
      //  Point(EdgeWidth - 3, 1), Point(1, H), Point(0, H - 2)]);

      Pen.Color := clWindowFrame;
      if SoftTop then
        PolyLine([{Point(EdgeWidth, 1), }Point(EdgeWidth - 2, 1), Point(2, H),
          Point(-1, H)])
      else
        PolyLine([Point(EdgeWidth, 1), Point(EdgeWidth - 2, 1), Point(2, H),
          Point(-1, H)]);
    end;
  end;

var
  TempList: TImageList;
  SaveHeight: Integer;
begin
  MemBitmap.Canvas.Font := Font;

  { Owner }
  SaveHeight := FTabHeight;
  try
    if FStyle = tsOwnerDraw then FTabHeight := FOwnerDrawHeight
    else FTabHeight := MemBitmap.Canvas.TextHeight('T') + 4;

    H := FTabHeight - 1;

    TempList := TImageList.CreateSize(EdgeWidth, FTabHeight); {exceptions}
  except
    FTabHeight := SaveHeight;
    raise;
  end;
  ImageList.Free;
  ImageList := TempList;

  Working := TBitmap.Create;
  try
    Working.Width := EdgeWidth;
    Working.Height := FTabHeight;
    MaskColor := clOlive;

    for EdgePart := Low(TEdgePart) to High(TEdgePart) do
    begin
      with Working.Canvas do
      begin
        Brush.Color := MaskColor;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, EdgeWidth, FTabHeight));
      end;
      case EdgePart of
        epSelectedLeft: DrawSL(Working.Canvas);
        epUnselectedLeft: DrawUL(Working.Canvas);
        epSelectedRight: DrawSR(Working.Canvas);
        epUnselectedRight: DrawUR(Working.Canvas);
      end;
      ImageList.AddMasked(Working, MaskColor);
    end;
  finally
    Working.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.CreateBrushPattern(Bitmap: TBitmap);
var
  X, Y: Integer;
begin
  Bitmap.Width := 8;
  Bitmap.Height := 8;
  with Bitmap.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := ColorToRGB(FBackgroundColor);
    FillRect(Rect(0, 0, Width, Height));
    if FDitherBackground then
      for Y := 0 to 7 do
        for X := 0 to 7 do
          if (Y mod 2) = (X mod 2) then  { toggles between even/odd pixles }
            Pixels[X, Y] := clWhite;     { on even/odd rows }
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.FixTabPos;
var
  FLastVisibleTab: Integer;

  function GetRightSide: Integer;
  begin
    if TabPosition in [pTop, pBottom] then
      Result := Width - EndMargin
    else
      Result := Height - EndMargin;

    if AutoScroll and (FVisibleTabs < Tabs.Count - 1) then
    begin
      if TabPosition in [pTop, pBottom] then
      begin
        if FCloseButtonAt = cbTabs then
          Dec(Result, Scroller.Width + 4 + FTabOverlap)
        else
          Dec(Result, Scroller.Width + 4 + FTabOverlap + CloseButtonWidth);
      end
      else
      begin
        if FCloseButtonAt = cbTabs then
          Dec(Result, Scroller.Height + 4 + FTabOverlap)
        else
          Dec(Result, Scroller.Height + 4 + FTabOverlap + CloseButtonHeight);
      end;
    end;
  end;

  function ReverseCalcNumTabs(Start, Stop: Integer; Canvas: TCanvas;
    Last: Integer): Integer;
  var
    W, W1, W2, RealItemIndex, ImgW: Integer;
  begin
    if HandleAllocated then
    begin
      Result := Last;
      while (Start >= Stop) and (Result >= 0) do
        with Canvas do
        begin

          ImgW:= 0;
          RealItemIndex := integer(FDuplicateTabs.Objects[Result]);
          if (FImages<> nil) and (FAdvTabs.Items[RealItemIndex].ImageIndex>=0) then
            begin
              if TabPosition in [pTop, pBottom] then
                ImgW:= FImages.Width
              else
                ImgW:= FImages.Height;
            end;

          Font.Assign(Self.Font);
          if TabPosition in [pTop, pBottom] then
            W1 := TextWidth(Tabs[Result]) + TabMargin.RightMargin + TabMargin.LeftMargin - 4 + ImgW
          else
            W1 := TextWidth(Tabs[Result]) + TabMargin.RightMargin + TabMargin.TopMargin - 4 + ImgW;

          Font.Assign(self.ActiveFont);
          if TabPosition in [pTop, pBottom] then
            W2 := TextWidth(Tabs[Result]) + TabMargin.RightMargin + TabMargin.LeftMargin - 4 + ImgW
          else
            W2 := TextWidth(Tabs[Result]) + TabMargin.RightMargin + TabMargin.TopMargin - 4 + ImgW;

          W := Max(W1, W2);

          if (FStyle = tsOwnerDraw) then
            MeasureTab(Result, W);
            
          Dec(Start, W + EdgeWidth);    { next usable position }
          if Start >= Stop then Dec(Result);
        end;
     if (Start < Stop) or (Result < 0) then Inc(Result);
    end else Result := FFirstIndex;
  end;

begin
  if Tabs.Count > 0 then
  begin
    FLastVisibleTab := FFirstIndex + FVisibleTabs - 1;
    if FTabIndex > FLastVisibleTab then
      FFirstIndex := ReverseCalcNumTabs(GetRightSide, StartMargin + EdgeWidth,
        Canvas, FTabIndex)
    else if (FTabIndex >= 0) and (FTabIndex < FFirstIndex) then
      FFirstIndex := FTabIndex;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetSelectedColor(Value: TColor);
begin
  if Value <> FSelectedColor then
  begin
    FSelectedColor := Value;
    //CreateEdgeParts;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetUnselectedColor(Value: TColor);
begin
  if Value <> FUnselectedColor then
  begin
    FUnselectedColor := Value;
    //CreateEdgeParts;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetBackgroundColor(Value: TColor);
begin
  if Value <> FBackgroundColor then
  begin
    FBackgroundColor := Value;
    CreateBrushPattern(BrushBitmap);
    MemBitmap.Canvas.Brush.Style := bsSolid;
    FScroller.Color:= ColorToRGB(FBackgroundColor);
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetDitherBackground(Value: Boolean);
begin
  if Value <> FDitherBackground then
  begin
    FDitherBackground := Value;
    CreateBrushPattern(BrushBitmap);
    MemBitmap.Canvas.Brush.Style := bsSolid;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetAutoScroll(Value: Boolean);
begin
  if Value <> FAutoScroll then
  begin
    FAutoScroll := Value;
    Scroller.Visible := False;
    ShowWindow(Scroller.Handle, SW_HIDE);
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetStartMargin(Value: Integer);
begin
  if Value <> FStartMargin then
  begin
    FStartMargin := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetEndMargin(Value: Integer);
begin
  if Value <> FEndMargin then
  begin
    FEndMargin := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
function TAdvCustomTabSet.CanChange(NewIndex: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnChange) then
  begin
    //FOnChange(Self, NewIndex, Result);
    NewIndex := integer(FDuplicateTabs.Objects[NewIndex]);
    FOnChange(Self, NewIndex, Result);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomTabSet.BeforeCloseTab(Tab: TTabCollectionItem; var CloseAction: TCloseAction);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvCustomTabSet.ChangeActiveTab(Value: Integer);
var
  NewIndex: integer;
begin
  if Value <> FTabIndex then
  begin
    if (Value < 0) or (Value >= Tabs.Count) then
      Exit;

    if not FAdvTabs.Items[integer(FDuplicateTabs.Objects[Value])].Enable then
      Exit;

    if CanChange(Value) then
    begin
      FTabIndex := Value;
      FixTabPos;
      Click;
      Invalidate;
      NewIndex := integer(FDuplicateTabs.Objects[FTabIndex]);
      if Assigned(FOnChanged) then
        FOnChanged(Self, NewIndex);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomTabSet.SetTabIndex(Value: Integer);
begin
  ChangeActiveTab(Value);
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SelectNext(Direction: Boolean);
var
  NewIndex: Integer;
begin
  if Tabs.Count > 1 then
  begin
    NewIndex := TabIndex;
    if Direction then
      Inc(NewIndex)
    else Dec(NewIndex);
    if NewIndex = Tabs.Count then
      NewIndex := 0
    else if NewIndex < 0 then
      NewIndex := Tabs.Count - 1;
    SetTabIndex(NewIndex);
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetFirstIndex(Value: Integer);
begin
  if (Value >= 0) and (Value < Tabs.Count) then
  begin
    FFirstIndex := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetTabList(Value: TStrings);
begin
  FTabs.Assign(Value);
  FTabIndex := -1;
  if FTabs.Count > 0 then TabIndex := 0
  else Invalidate;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetTabStyle(Value: TTabStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    CreateEdgeParts;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetTabHeight(Value: Integer);
var
  SaveHeight: Integer;
begin
  if Value <> FOwnerDrawHeight then
  begin
    SaveHeight := FOwnerDrawHeight;
    try
      FOwnerDrawHeight := Value;
      CreateEdgeParts;
      Invalidate;
    except
      FOwnerDrawHeight := SaveHeight;
      raise;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.DrawTab(TabCanvas: TCanvas; R: TRect; Index: Integer;
  Selected: Boolean);
begin
  if Assigned(FOnDrawTab) then
    FOnDrawTab(Self, TabCanvas, R, Index, Selected);
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.MeasureTab(Index: Integer; var TabWidth: Integer);
begin
  if Assigned(FOnMeasureTab) then
    FOnMeasureTab(Self, Index, TabWidth);
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
  TabPos: TTabPos;
  RealItemIndex, i: integer;
  p: TPoint;
  PosFound: boolean;
  cbr: TRect;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  if TabPosition in [pTop, pBottom] then
    if Y > FTabHeight then
      Exit;

  if TabPosition in [pLeft, pRight] then
    if X > FTabHeight then
      Exit;

  if AdvTabs.Count = 0 then
    Exit;

  if FTabBackGround.Empty then
  begin
    P.X := X;
    P.Y := Y;
    i := ItemAtPos(P);

    if (FHoverTab <> i) and ((FTabHoverColor <> clNone) or (FTabHoverColorTo <> clNone)) then
    begin
      FHoverTab := i;
      Invalidate;
    end;
  end;

  if (FCloseButtonAt = cbTabs) and (FTabIndex >= 0){ and (i>=0)} then
  begin
    RealItemIndex := integer(FDuplicateTabs.objects[FTabIndex]);
    if FAdvTabs.Items[RealItemIndex].ShowClose then
    begin
      PosFound := false;
      for i :=0 to TabPositions.Count-1 do
      begin
        if (i + FirstIndex) = FTabIndex then
        begin
          Pointer(TabPos) := TabPositions[i];

          if TabPosition in [pTop, pBottom] then
            r := Rect(TabPos.StartPos, 0, TabPos.StartPos + TabPos.Size, FTabHeight)
          else
            r := Rect(0,TabPos.StartPos, FTabHeight, TabPos.StartPos + TabPos.Size + EdgeWidth + 1);
          PosFound:= true;
          break;
        end;
      end;
      if not PosFound then
        Exit;

     // Pointer(TabPos) := TabPositions[FTabIndex - FirstIndex];
     // r := Rect(TabPos.StartPos, 0, TabPos.StartPos + TabPos.Size, FTabHeight);

      if not FHoverClosedButton then
      begin
        if IsOnButton(FTabIndex, X, Y) then
        begin
          cbr := r;
          if (ClosePosition = cpRight) then
          begin
            case TabPosition of
              pTop, pBottom:  cbr.Right := cbr.Right + (EdgeWidth div 2);
              //pLeft:  cbr.Top := cbr.Top + (EdgeWidth div 2);
              //pRight: cbr.Bottom := cbr.Bottom - (EdgeWidth div 2);
            end;
          end;
          DrawHoverCloseButton(cbr);
          FHoverClosedButton := True;
        end;
      end
      else
      begin
        if not IsOnButton(FTabIndex, X, Y) then
        begin
          cbr := r;
          if (ClosePosition = cpRight) then
          begin
            case TabPosition of
              pTop, pBottom:  cbr.Right := cbr.Right + (EdgeWidth div 2);
              //pLeft:  cbr.Top := cbr.Top + (EdgeWidth div 2);
              //pRight: cbr.Bottom := cbr.Bottom - (EdgeWidth div 2);
            end;
          end;
          
          DrawCloseButton(Canvas, cbr, True);
          FHoverClosedButton := false;
        end;
      end;
    end;
  end
  else if FCloseButtonAt = cbTabSet then
  begin
    if not IsOnCloseButtonAtTabSet(X,Y) and FCloseButtonDown then
      begin
        DrawCloseButtonAtTabSet(true);
        FCloseButtonDown:= false;
      end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  TabPos: TTabPos;
  I: Integer;
  Extra: Integer;
  MinLeft: Integer;
  MaxRight: Integer;
  MinTop: integer;
  MaxBottom: integer;
  CanBeginDrag: Boolean;
  r, cbr: TRect;
  RealItemIndex: integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  CanBeginDrag := false;

  if AdvTabs.Count = 0 then
    Exit;

  if TabPositions.Count = 0 then
    Exit;

  if (Button = mbLeft) then
  begin
    if (TabPosition in [pTop, pBottom]) and (Y <= FTabHeight) then
    begin
      if Y < FTabHeight div 2 then Extra := EdgeWidth div 3
      else Extra := EdgeWidth div 2;

      for I := 0 to TabPositions.Count - 1 do
      begin
        Pointer(TabPos) := TabPositions[I];

        MinLeft := TabPos.StartPos - Extra;
        MaxRight := TabPos.StartPos + TabPos.Size + Extra;
        if (X >= MinLeft) and (X <= MaxRight) then
        begin
          SetTabIndex(FirstIndex + I);
          CanBeginDrag:= True;
          Break;
        end;
      end;
    end
    else if (TabPosition in [pLeft, pRight]) and (X <= FTabHeight) then
    begin
      if X < FTabHeight div 2 then Extra := EdgeWidth div 3
      else Extra := EdgeWidth div 2;

      for I := 0 to TabPositions.Count - 1 do
      begin
        Pointer(TabPos) := TabPositions[I];

        MinTop := TabPos.StartPos - Extra;
        MaxBottom := TabPos.StartPos + TabPos.Size + Extra;
        if (Y >= MinTop) and (Y <= MaxBottom) then
        begin
          SetTabIndex(FirstIndex + I);
          CanBeginDrag:= True;
          Break;
        end;
      end;
    end;

    if (csDesigning in ComponentState) then
      Exit;

    RealItemIndex := integer(FDuplicateTabs.objects[FTabIndex]);
    if FAdvTabs.Items[RealItemIndex].ShowClose then
    begin
      if FCloseButtonAt = cbTabs then
      begin
        if IsOnButton(FTabIndex, X, Y, r) then
        begin
          //Pointer(TabPos) := TabPositions[FTabIndex - FirstIndex];
          //r := Rect(TabPos.StartPos, 0, TabPos.StartPos + TabPos.Size, FTabHeight);
          cbr := r;
          if (ClosePosition = cpRight) then
          begin
            case TabPosition of
              pTop, pBottom:  cbr.Right := cbr.Right + (EdgeWidth div 2);
              //pLeft:  cbr.Top := cbr.Top + (EdgeWidth div 2);
              //pRight: cbr.Bottom := cbr.Bottom - (EdgeWidth div 2);
            end;
          end;
          DrawDownCloseButton(cbr);
          CanBeginDrag := false;
        end;
      end
      else  // cbTabSet
      begin
        if IsOnCloseButtonAtTabSet(X,Y) then
        begin
          DrawDownCloseButtonAtTabSet;
          FCloseButtonDown := true;
        end;
      end;
    end;
    if FTabRearrange and CanBeginDrag then
      BeginDrag(false,6);
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  {OldTabIndex,} RealItemIndex: integer;
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;

  if TabPositions.Count = 0 then
    Exit;

  if AdvTabs.Count = 0 then
    Exit;

  if (Button = mbLeft) then
  begin
    if (TabPosition in [pTop, pBottom]) and (Y > FTabHeight) then
      Exit;
    if (TabPosition in [pLeft, pRight]) and (X > FTabHeight) then
      Exit;

    RealItemIndex := integer(FDuplicateTabs.objects[FTabIndex]);
    if (FAdvTabs.Items[RealItemIndex].ShowClose) then
    begin
      if (FCloseButtonAt= cbTabs) then
      begin
        if IsOnButton(FTabIndex, X, Y) then
        begin
        {  OldTabIndex:= integer(FDuplicateTabs.Objects[FTabIndex]);
          SelectNext(false);
          if Assigned(FOnTabClose) then FOnTabClose(Self, OldTabIndex);
          FAdvTabs.Items[OldTabIndex].Visible:= false;
          invalidate;}
          CloseButtonClick;
        end;
      end
      else if IsOnCloseButtonAtTabSet(X,Y) {and cbTabSet}then
      begin
        CloseButtonClick;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.CMMouseLeave(var Message: TMessage);
begin
  if FTabBackGround.Empty then
  begin
    if FHoverTab <> -1 then
    begin
      FHoverTab := -1;
      Invalidate;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomTabSet.UpdateScroller;
var
  NumVisTabs, LastTabPos: Integer;

  function CalcNumTabs(Start, Stop: Integer; Canvas: TCanvas;
    First: Integer): Integer;
  begin
    Result := First;
    while (Start < Stop) and (Result < Tabs.Count) do
      with Canvas do
      begin
        Inc(Start, ItemWidth(Result) + EdgeWidth);    { next usable position }
        if Start <= Stop then Inc(Result);
      end;
  end;

begin
  if Tabs.Count > 1 then
  begin
    LastTabPos := Width - EndMargin;
    NumVisTabs := CalcNumTabs(StartMargin + EdgeWidth, LastTabPos, Canvas, 0);
    if (FTabIndex = Tabs.Count) or (NumVisTabs > FVisibleTabs) or
      (NumVisTabs = Tabs.Count) then
      FirstIndex := Tabs.Count - NumVisTabs;
    FDoFix := True;
  end;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomTabSet.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateScroller;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  CreateEdgeParts;
  CreateBrushPattern(BrushBitmap);
  MemBitmap.Canvas.Brush.Style := bsSolid;
  { Windows follows this message with a WM_PAINT }
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font := Font;
  CreateEdgeParts;
  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTALLKEYS;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  for I := 0 to FTabs.Count - 1 do
  begin
    if IsAccel(Message.CharCode, FTabs[I]) then
    begin
      Message.Result := 1;
      if FTabIndex <> I then
        SetTabIndex(I);
      Exit;
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.DefineProperties(Filer: TFiler);
begin
  { Can be removed after version 1.0 }
  if Filer is TReader then inherited DefineProperties(Filer);
  Filer.DefineProperty('TabOrder', ReadIntData, nil, False);
  Filer.DefineProperty('TabStop', ReadBoolData, nil, False);
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.ReadIntData(Reader: TReader);
begin
  Reader.ReadInteger;
end;

//------------------------------------------------------------------------------

function TAdvCustomTabSet.RealToDisplTabIndex(tab: integer): integer;
begin
  Result := -1;
  if (Tab < 0) or (Tab >= AdvTabs.Count) or (not FAdvTabs.Items[Tab].Visible) or
     (FAdvTabs.Items[Tab].FIndexInAdvTabSet < 0) or (FAdvTabs.Items[Tab].FIndexInAdvTabSet >= Tabs.Count) then
    Exit;

  Result := FAdvTabs.Items[Tab].FIndexInAdvTabSet;
end;

//------------------------------------------------------------------------------

function TAdvCustomTabSet.DisplToRealTabIndex(tab: integer): integer;
begin
  Result := -1;
  if (Tab < 0) or (tab >= FDuplicateTabs.Count) then
    Exit;

  Result := Integer(FDuplicateTabs.Objects[tab]);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomTabSet.ReadBoolData(Reader: TReader);
begin
  Reader.ReadBoolean;
end;

//------------------------------------------------------------------------------
function TAdvCustomTabSet.MinClientRect: TRect;
begin
  Result := MinClientRect(Tabs.Count, False);
end;

//------------------------------------------------------------------------------
function TAdvCustomTabSet.MinClientRect(IncludeScroller: Boolean): TRect;
begin
  Result := MinClientRect(Tabs.Count, IncludeScroller);
end;

//------------------------------------------------------------------------------
function TAdvCustomTabSet.MinClientRect(TabCount: Integer; IncludeScroller: Boolean): TRect;
var
  I: Integer;
begin
  Result := Rect(0, 0, StartMargin, FTabHeight + 5);
  for I := 0 to TabCount - 1 do
    Inc(Result.Right, ItemWidth(I) + EdgeWidth);
  Inc(Result.Right, EndMargin);
  if IncludeScroller then
    Inc(Result.Right, Scroller.Width + 4);
end;

//------------------------------------------------------------------------------
function TAdvCustomTabSet.ItemWidth(Index: Integer): Integer;
var
  W1, W2, RealItemIndex, ImgW: integer;
  s: string;
begin
  ImgW:= 0;
  RealItemIndex := integer(FDuplicateTabs.Objects[Index]);
  if (FImages<> nil) and (FAdvTabs.Items[RealItemIndex].ImageIndex>=0) then
    begin
      if TabPosition in [pTop, pBottom] then
        ImgW:= FImages.Width
      else
        ImgW:= FImages.Height;
    end;

  with Canvas do
  begin
    Font.Assign(self.Font);
    s := Tabs[Index];
    if (s = '') then
      s := ' ';
      
    if TabPosition in [pTop, pBottom] then
      W1 := TextWidth(s) + TabMargin.RightMargin + TabMargin.LeftMargin - 4 + ImgW
    else
      W1 := TextWidth(s) + TabMargin.RightMargin + TabMargin.TopMargin - 4 + ImgW;

    Font.Assign(self.ActiveFont);
    if TabPosition in [pTop, pBottom] then
      W2 := TextWidth(s) + TabMargin.RightMargin + TabMargin.LeftMargin - 4 + ImgW
    else
      W2 := TextWidth(s) + TabMargin.RightMargin + TabMargin.TopMargin - 4 + ImgW;

    Result := Max(W1, W2);
  end;
  
  if (FStyle = tsOwnerDraw) then
    MeasureTab(Index, Result);
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetSoftTop(const Value: Boolean);
begin
  if Value <> SoftTop then
  begin
    FSoftTop := Value;
    CreateEdgeParts;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetOriginalTabWidth;
var
  i: integer;
begin
  for i:=0 to FTabs.Count-1 do
  begin
    FTabs[i] := Trim(FTabs[i]);
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.IncTabWidth(w: integer);
var
  i, SpCount, RealItemIndex: integer;
  s: String;
begin
  s:='';
  SpCount:= w div 3;
  for i:= 1 to SpCount {+ 3}  do
    s:= s + ' ';
  for i:=0 to FTabs.Count-1 do
  begin
    RealItemIndex := integer(FDuplicateTabs.objects[i]);

    if FAdvTabs.Items[RealItemIndex].ShowClose and (CloseButtonAt = cbTabs) then
      FTabs[i] := FTabs[i] + s + '      '
    else
      FTabs[i] := FTabs[i] + s;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.AdjustTabWidth;
var
  TotalIncWidth: integer;
begin
  SetOriginalTabWidth;
  TotalIncWidth:= 0;

  //TotalIncWidth:= FTabMargin.RightMargin;
  {
  if TabPosition in [pTop, pBottom] then
  begin
    if FTabMargin.LeftMargin > 0 then
      TotalIncWidth:= FTabMargin.LeftMargin;
  end
  else // pLeft, pRight
  begin
    if FTabMargin.TopMargin > 0 then
      TotalIncWidth:= FTabMargin.TopMargin;
  end;
  }

//  if FImages <> nil then
//    TotalIncWidth:= TotalIncWidth + FImages.Width;
  IncTabWidth(TotalIncWidth);
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FImages) then
  begin
    FImages := nil;
    AdjustTabWidth;
    Invalidate;
  end;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.Loaded;
begin
  inherited Loaded;
  AdjustTabWidth
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetSelectedColorTo(Value: TColor);
begin
  if Value <> FSelectedColorTo then
  begin
    FSelectedColorTo:= Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetUnSelectedColorTo(Value: TColor);
begin
  if Value <> FUnSelectedColorTo then
  begin
    FUnSelectedColorTo:= Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetTextColor(Value: TColor);
begin
  if Value <> FTextColor then
  begin
    FTextColor:= Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetTabBorderColor(Value: TColor);
begin
  if Value <> FTabBorderColor then
  begin
    FTabBorderColor:= Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetTabBackGround(Const Value: TBitmap);
begin
  FTabBackGround.Assign(Value);

  if (csDesigning in ComponentState) and Assigned(Value) then
  begin
    if not Value.Empty then
    if TabPosition in [pTop,pBottom] then
      TabHeight := Value.Height - 3
    else
      TabHeight := Value.Width - 3
  end;

  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetTabBackGroundSelected(Const Value: TBitmap);
begin
  TabBackGroundSelected.Assign(Value);
  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetGradientDirection(Value: TGradientDirection);
begin
  if Value <> FGradientDirection then
  begin
    FGradientDirection:= Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.DrawCloseGlyph(Canvas: TCanvas; P: TPoint; IsEnable: Boolean);
var
  GlRgn: HRGN;
begin
  if not CloseGlyph.Empty then
  begin
    GlRgn := CreateRectRgn(P.X, P.Y, P.X + CloseButtonWidth-2, P.Y + CloseButtonHeight - 2);
    SelectClipRgn(Canvas.Handle,GlRgn);
    if IsEnable then
    begin
      FCloseGlyph.Transparent:= true;
      FCloseGlyph.TransparentMode:= tmAuto;
      Canvas.Draw(P.X, P.Y, FCloseGlyph);
    end
    else
    begin
      //FDisableCloseGlyph.Transparent:= true;
      //FDisableCloseGlyph.TransparentMode:= tmAuto;
      Canvas.Draw(P.X, P.Y, FDisableCloseGlyph);
    end;
    SelectClipRgn(Canvas.Handle,0);
    DeleteObject(GlRgn);
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.DrawCloseButton(Canvas: TCanvas; Rect: TRect; Active: Boolean);
var
  a: integer;
  P: TPoint;
begin
  if Active then a := {2}FLowerSelected
  else a := 0;

  //Rect.Left := Rect.Left + FTabMargin.LeftMargin;
  //Rect.Top := Rect.Top + FTabMargin.TopMargin + a;

  if not FCloseGlyph.Empty then
  begin
    case TabPosition of
      pLeft:
      begin
        if (ClosePosition = cpLeft) then
        begin
          Rect.Left := Rect.Left + FTabMargin.LeftMargin + a;
          Rect.Bottom := Rect.Bottom - FTabMargin.TopMargin;
        end
        else
        begin
          Rect.Left := Rect.Left + FTabMargin.LeftMargin + a;
          Rect.Bottom := Rect.Top + FTabMargin.RightMargin + CloseButtonHeight;
        end;
        if not FTabBackGroundSelected.Empty then Rect.Bottom := Rect.Bottom + TabOverlap;
        P.X:= Rect.Left + 2;
        P.Y:= Rect.Bottom - CloseButtonHeight;
      end;
      pRight:
      begin
        if (ClosePosition = cpLeft) then
        begin
          Rect.Left := Rect.Left + FTabMargin.LeftMargin + 2 - a;
          Rect.Top := Rect.Top + FTabMargin.TopMargin;
        end
        else
        begin
          Rect.Left := Rect.Left + FTabMargin.LeftMargin + 2 - a;
          Rect.Top := Rect.Bottom - FTabMargin.RightMargin - CloseButtonHeight;
        end;
        P.X:= Rect.Left + 2;
        P.Y:= Rect.Top + 2;
      end;
      pTop, pBottom:
      begin
        if (ClosePosition = cpLeft) then
        begin
          Rect.Left := Rect.Left + FTabMargin.LeftMargin;
          Rect.Top := Rect.Top + FTabMargin.TopMargin + a;
        end
        else
        begin
          Rect.Left := Rect.Right - FTabMargin.RightMargin - CloseButtonWidth;
          Rect.Top := Rect.Top + FTabMargin.TopMargin + a;
        end;
        P.X:= Rect.Left + 2;
        P.Y:= Rect.Top + 2;
      end;
      {pBottom:
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin + a;
        P.X:= Rect.Left + 2;
        P.Y:= Rect.Top + 2;
      end;}
    end;
    DrawCloseGlyph(Canvas, P, true);
  end
  else
  begin
    if TabPosition = pLeft then
    begin
      if (ClosePosition = cpLeft) then
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin + a;
        Rect.Bottom := Rect.Bottom - FTabMargin.TopMargin;
        if not FTabBackGroundSelected.Empty then
          Rect.Bottom := Rect.Bottom + TabOverlap;
      end
      else
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin + a;
        Rect.Bottom := Rect.Top + FTabMargin.RightMargin + CloseButtonHeight;
        if not FTabBackGroundSelected.Empty then
          Rect.Bottom := Rect.Bottom + TabOverlap;
      end;

      with canvas do
      begin
        Brush.Color := clWhite;
        Pen.Color := clGray;
        Rectangle(Rect.Left + 2, Rect.Bottom - 2, Rect.Left + CloseButtonWidth, Rect.Bottom - CloseButtonHeight);
        Pen.Color := clBlack;
                      {/}
        MoveTo(Rect.Left + 2 + 3, Rect.Bottom - 2 - 3 - 4 - 1);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Bottom - 2 - 2 - 1); //-1
        MoveTo(Rect.Left + 2 + 4, Rect.Bottom - 2 - 3 - 4 - 1);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Bottom - 2 - 2 - 1);
                      {\}
        MoveTo(Rect.Left + 2 + 3, Rect.Bottom - 2 - 3 - 1);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Bottom - 2 - 3 - 5 - 1);
        MoveTo(Rect.Left + 2 + 4, Rect.Bottom - 2 - 3 - 1);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Bottom - 2 - 3 - 5 - 1);
      end;
    end
    else
    begin
      if TabPosition = pRight then
      begin
        if (ClosePosition = cpLeft) then
        begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin + 2 - a;
        Rect.Top := Rect.Top + FTabMargin.TopMargin;
        end
        else
        begin
          Rect.Left := Rect.Left + FTabMargin.LeftMargin + 2 - a;
          Rect.Top := Rect.Bottom - FTabMargin.RightMargin - CloseButtonHeight;
        end;
      end
      else if TabPosition in [pTop, pBottom] then
      begin
        if (ClosePosition = cpLeft) then
        begin
          Rect.Left := Rect.Left + FTabMargin.LeftMargin;
          Rect.Top := Rect.Top + FTabMargin.TopMargin + a;
        end
        else
        begin
          Rect.Left := Rect.Right - FTabMargin.RightMargin - CloseButtonWidth;
          Rect.Top := Rect.Top + FTabMargin.TopMargin + a;
        end;
      {end
      else // Bottom
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin + a;}
      end;

      with canvas do
      begin
        Brush.Color := clWhite;
        Pen.Color := clGray;
        Rectangle(Rect.Left + 2, Rect.Top + 2, Rect.Left + CloseButtonWidth, Rect.Top + CloseButtonHeight);
        Pen.Color := clBlack;
                      {/}
        MoveTo(Rect.Left + 2 + 3, Rect.Top + 2 + 3 + 4);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Top + 2 + 2);
        MoveTo(Rect.Left + 2 + 4, Rect.Top + 2 + 3 + 4);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Top + 2 + 2);
                      {\}
        MoveTo(Rect.Left + 2 + 3, Rect.Top + 2 + 3);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Top + 2 + 3 + 5);
        MoveTo(Rect.Left + 2 + 4, Rect.Top + 2 + 3);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Top + 2 + 3 + 5);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.DrawHoverCloseButton(Rect: TRect);
var
  p: TPoint;
begin
  //Rect.Left := Rect.Left + FTabMargin.LeftMargin;
  //Rect.Top := Rect.Top + FTabMargin.TopMargin + 2;

  if not FCloseGlyph.Empty then
  begin
    case TabPosition of
      pLeft:
      begin
        if (ClosePosition = cpLeft) then
        begin
          Rect.Left := Rect.Left + FTabMargin.LeftMargin + FLowerSelected{2} {Active};
          Rect.Bottom := Rect.Bottom - FTabMargin.TopMargin;
          if not FTabBackGroundSelected.Empty then Rect.Bottom := Rect.Bottom + TabOverlap;
        end
        else
        begin
          Rect.Left := Rect.Left + FTabMargin.LeftMargin + FLowerSelected {2} {Active};
          Rect.Bottom := Rect.Top + FTabMargin.RightMargin + CloseButtonHeight;
          if not FTabBackGroundSelected.Empty then Rect.Bottom := Rect.Bottom + TabOverlap;
        end;
        P.X:= Rect.Left + 2;
        P.Y:= Rect.Bottom - CloseButtonHeight;
      end;
      pRight:
      begin
        if (ClosePosition = cpLeft) then
        begin
          Rect.Left := Rect.Left + FTabMargin.LeftMargin -FLowerSelected+2;
          Rect.Top := Rect.Top + FTabMargin.TopMargin;
        end
        else
        begin
          Rect.Left := Rect.Left + FTabMargin.LeftMargin;
          Rect.Top := Rect.Bottom - FTabMargin.RightMargin - CloseButtonHeight;
        end;
        P.X:= Rect.Left + 2;
        P.Y:= Rect.Top + 2;
      end;
      pTop, pBottom:
      begin
        if (ClosePosition = cpLeft) then
        begin
          Rect.Left := Rect.Left + FTabMargin.LeftMargin;
          Rect.Top := Rect.Top + FTabMargin.TopMargin + {2}FLowerSelected;
        end
        else
        begin
          Rect.Left := Rect.Right - FTabMargin.RightMargin - CloseButtonWidth;
          Rect.Top := Rect.Top + FTabMargin.TopMargin + {2} FLowerSelected;
        end;
        P.X:= Rect.Left + 2;
        P.Y:= Rect.Top + 2;
      end;
      {pBottom:
      begin
        if (ClosePosition = cpLeft) then
        begin
          Rect.Left := Rect.Left + FTabMargin.LeftMargin;
          Rect.Top := Rect.Top + FTabMargin.TopMargin + FLowerSelected;
        end
        else
        begin
          Rect.Left := Rect.Right - FTabMargin.RightMargin - CloseButtonWidth;
          Rect.Top := Rect.Top + FTabMargin.TopMargin + FLowerActive;
        end;
        P.X:= Rect.Left + 2;
        P.Y:= Rect.Top + 2;
      end;}
    end;
    DrawCloseGlyph(Canvas, P, true);
  end
  else
  begin
    if TabPosition = pLeft then
    begin
      if (ClosePosition = cpLeft) then
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin + FLowerSelected{2} {Active};
        Rect.Bottom := Rect.Bottom - FTabMargin.TopMargin;
      end
      else
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin + FLowerSelected;
        Rect.Bottom := Rect.Top + FTabMargin.RightMargin + CloseButtonHeight;
      end;
      if not FTabBackGroundSelected.Empty then Rect.Bottom := Rect.Bottom + TabOverlap;

      with canvas do
      begin
        Brush.Color := clWhite;
        Pen.Color := clBlack; //clGray;
        Rectangle(Rect.Left + 2, Rect.Bottom - 2, Rect.Left + CloseButtonWidth, Rect.Bottom - CloseButtonHeight);
        Pen.Color := clGray; //clBlack;
                      {/}
        MoveTo(Rect.Left + 2 + 3, Rect.Bottom - 2 - 3 - 4 - 1);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Bottom - 2 - 2 - 1); //-1
        MoveTo(Rect.Left + 2 + 4, Rect.Bottom - 2 - 3 - 4 - 1);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Bottom - 2 - 2 - 1);
                      {\}
        MoveTo(Rect.Left + 2 + 3, Rect.Bottom - 2 - 3 - 1);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Bottom - 2 - 3 - 5 - 1);
        MoveTo(Rect.Left + 2 + 4, Rect.Bottom - 2 - 3 - 1);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Bottom - 2 - 3 - 5 - 1);
      end;
    end
    else
    begin
      if TabPosition = pRight then
      begin
        if (ClosePosition = cpLeft) then
        begin
          Rect.Left := Rect.Left + FTabMargin.LeftMargin -FLowerSelected+2;
          Rect.Top := Rect.Top + FTabMargin.TopMargin;
        end
        else
        begin
          Rect.Left := Rect.Left + FTabMargin.LeftMargin - FLowerSelected + 2;
          Rect.Top := Rect.Bottom - FTabMargin.RightMargin - CloseButtonHeight;
        end;
      end
      else if TabPosition in [pTop, pBottom] then
      begin
        if (ClosePosition = cpLeft) then
        begin
          Rect.Left := Rect.Left + FTabMargin.LeftMargin;
          Rect.Top := Rect.Top + FTabMargin.TopMargin + {2}FLowerSelected;
        end
        else
        begin
          Rect.Left := Rect.Right - FTabMargin.RightMargin - CloseButtonWidth;
          Rect.Top := Rect.Top + FTabMargin.TopMargin + FLowerSelected;
        end;
      {end
      else // Bottom
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin + FLowerSelected;}
      end;

      with canvas do
      begin
        Brush.Color := clWhite;
        Pen.Color := clBlack;
        Rectangle(Rect.Left + 2, Rect.Top + 2, Rect.Left + CloseButtonWidth, Rect.Top + CloseButtonHeight);
        Pen.Color := clGray;
                      {/}
        MoveTo(Rect.Left + 2 + 3, Rect.Top + 2 + 3 + 4);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Top + 2 + 2);
        MoveTo(Rect.Left + 2 + 4, Rect.Top + 2 + 3 + 4);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Top + 2 + 2);
                      {\}
        MoveTo(Rect.Left + 2 + 3, Rect.Top + 2 + 3);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Top + 2 + 3 + 5);
        MoveTo(Rect.Left + 2 + 4, Rect.Top + 2 + 3);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Top + 2 + 3 + 5);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.DrawDownCloseButton(Rect: TRect);
var
  p: TPoint;
begin
 // Rect.Left := Rect.Left + FTabMargin.LeftMargin;
 // Rect.Top := Rect.Top + FTabMargin.TopMargin + 2;

  if not FCloseGlyph.Empty then
  begin
    case TabPosition of
      pLeft:
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin + FLowerSelected{2} {active};
        if (ClosePosition = cpLeft) then
          Rect.Bottom := Rect.Bottom - FTabMargin.TopMargin
        else
          Rect.Bottom := Rect.Top + FTabMargin.RightMargin + CloseButtonHeight;
        if not FTabBackGroundSelected.Empty then Rect.Bottom := Rect.Bottom + TabOverlap;
        P.X:= Rect.Left + 2;
        P.Y:= Rect.Bottom - CloseButtonHeight;
      end;
      pRight:
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin -FLowerSelected+2;
        if (ClosePosition = cpLeft) then
          Rect.Top := Rect.Top + FTabMargin.TopMargin
        else
          Rect.Top := Rect.Bottom - FTabMargin.RightMargin - CloseButtonHeight;
        P.X:= Rect.Left + 2;
        P.Y:= Rect.Top + 2;
      end;
      pTop, pBottom:
      begin
        if (ClosePosition = cpLeft) then
          Rect.Left := Rect.Left + FTabMargin.LeftMargin
        else
          Rect.Left := Rect.Right - FTabMargin.RightMargin - CloseButtonWidth;
        Rect.Top := Rect.Top + FTabMargin.TopMargin + {2}FLowerSelected;
        P.X:= Rect.Left + 2;
        P.Y:= Rect.Top + 2;
      end;
      {pBottom:
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin + FLowerSelected;
        P.X:= Rect.Left + 2;
        P.Y:= Rect.Top + 2;
      end;}
    end;
    DrawCloseGlyph(Canvas, P, true);
  end
  else
  begin
    if TabPosition = pLeft then
    begin
      Rect.Left := Rect.Left + FTabMargin.LeftMargin + FLowerSelected{2} {active};
      if (ClosePosition = cpLeft) then
        Rect.Bottom := Rect.Bottom - FTabMargin.TopMargin
      else
        Rect.Bottom := Rect.Top + FTabMargin.RightMargin + CloseButtonHeight;

      if not FTabBackGroundSelected.Empty then Rect.Bottom := Rect.Bottom + TabOverlap;

      with canvas do
      begin
        Brush.Color := clSilver; //clWhite;
        Pen.Color := clBlack; //clGray;
        Rectangle(Rect.Left + 2, Rect.Bottom - 2, Rect.Left + CloseButtonWidth, Rect.Bottom - CloseButtonHeight);
        Pen.Color := clBlack;
                      {/}
        MoveTo(Rect.Left + 2 + 3, Rect.Bottom - 2 - 3 - 4 - 1);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Bottom - 2 - 2 - 1); //-1
        MoveTo(Rect.Left + 2 + 4, Rect.Bottom - 2 - 3 - 4 - 1);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Bottom - 2 - 2 - 1);
                      {\}
        MoveTo(Rect.Left + 2 + 3, Rect.Bottom - 2 - 3 - 1);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Bottom - 2 - 3 - 5 - 1);
        MoveTo(Rect.Left + 2 + 4, Rect.Bottom - 2 - 3 - 1);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Bottom - 2 - 3 - 5 - 1);
      end;
    end
    else
    begin
      if TabPosition = pRight then
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin -FLowerSelected+2;
        if (ClosePosition = cpLeft) then
          Rect.Top := Rect.Top + FTabMargin.TopMargin
        else
          Rect.Top := Rect.Bottom - FTabMargin.RightMargin - CloseButtonHeight;
      end
      else if TabPosition in [pTop, pBottom] then
      begin
        if (ClosePosition = cpLeft) then
          Rect.Left := Rect.Left + FTabMargin.LeftMargin
        else
          Rect.Left := Rect.Right - FTabMargin.RightMargin - CloseButtonWidth;
        Rect.Top := Rect.Top + FTabMargin.TopMargin + {2}FLowerSelected;
      {end
      else // bottom
      begin
        Rect.Left := Rect.Left + FTabMargin.LeftMargin;
        Rect.Top := Rect.Top + FTabMargin.TopMargin + FLowerSelected;}
      end;

      with canvas do
      begin
        Brush.Color := clSilver;
        Pen.Color := clBlack;
        Rectangle(Rect.Left + 2, Rect.Top + 2, Rect.Left + CloseButtonWidth, Rect.Top + CloseButtonHeight);
        Pen.Color := clBlack;
                      {/}
        MoveTo(Rect.Left + 2 + 3, Rect.Top + 2 + 3 + 4);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Top + 2 + 2);
        MoveTo(Rect.Left + 2 + 4, Rect.Top + 2 + 3 + 4);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Top + 2 + 2);
                      {\}
        MoveTo(Rect.Left + 2 + 3, Rect.Top + 2 + 3);
        LineTo(Rect.Left + 2 + 3 + 6, Rect.Top + 2 + 3 + 5);
        MoveTo(Rect.Left + 2 + 4, Rect.Top + 2 + 3);
        LineTo(Rect.Left + 2 + 3 + 5, Rect.Top + 2 + 3 + 5);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TAdvCustomTabSet.IsOnButton(TabIndex, X, Y: integer): Boolean;
var
  aRect: TRect;
begin
  result:= IsOnButton(TabIndex, X, Y, aRect);
end;

//------------------------------------------------------------------------------
function TAdvCustomTabSet.IsOnButton(TabIndex, X, Y: integer; var aRect:TRect): Boolean;
var
  r: TRect;
  TabPos: TTabPos;
  i: integer;
  PosFound: boolean;
begin
  Result := false;

  PosFound:= false;
  for i:=0 to TabPositions.Count-1 do
  begin
    if (i + FirstIndex) = TabIndex then
    begin
      Pointer(TabPos) := TabPositions[i];

      if TabPosition in [pTop, pBottom] then
        r := Rect(TabPos.StartPos, 0, TabPos.StartPos + TabPos.Size, FTabHeight)
      else
        r := Rect(0, TabPos.StartPos, FTabHeight, TabPos.StartPos + TabPos.Size + EdgeWidth + 1);
      aRect:= r;
      PosFound:= true;
      break;
    end;
  end;
  if not PosFound then
    Exit;

  if TabPosition in [pTop, pBottom] then
  begin
    if (ClosePosition = cpLeft) then
      r.Left := r.Left + FTabMargin.LeftMargin
    else
    begin
      r.Right := r.Right + (EdgeWidth div 2);
      r.Left := r.Right - FTabMargin.RightMargin - CloseButtonWidth;
    end;
    r.Top := r.Top + FTabMargin.TopMargin;
    r := Rect(r.Left + 2, r.Top + 2, r.Left + CloseButtonWidth, r.Top + CloseButtonHeight);
    if PtInRect(r, Point(X, Y)) then
    begin
      Result := true;
    end;
  end
  else // pLeft, pRight
  begin
    if TabPosition = pLeft then
    begin
      if (ClosePosition = cpLeft) then
        r.Bottom := r.Bottom - FTabMargin.TopMargin
      else
      begin
        //r.Top := r.Top + (EdgeWidth div 2);
        r.Bottom := r.Top + FTabMargin.RightMargin + CloseButtonHeight;
      end;
      r.Left := r.Left + FTabMargin.LeftMargin;
      r := Rect(r.Left + 2, r.Bottom - CloseButtonHeight, r.Left + CloseButtonWidth, r.Bottom - 2);
    end
    else // pRight
    begin
      if (ClosePosition = cpLeft) then
        r.Top := r.Top + FTabMargin.TopMargin
      else
        r.Top := r.Bottom - FTabMargin.RightMargin - CloseButtonHeight;
      r.Left := r.Left + FTabMargin.LeftMargin;
      r := Rect(r.Left + 2, r.Top, r.Left + CloseButtonWidth, r.Top + CloseButtonHeight);
    end;
    if PtInRect(r, Point(X, Y)) then
    begin
      Result := true;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.TabMarginChange(NewValue, OldValue: TMarginSize; Index: integer);
begin
  AdjustTabWidth;
  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetTabMargin(Value: TTabMargin);
begin
  if Assigned(Value) then
    FTabMargin.Assign(Value);
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetTabOverlap(Value: TTabOverlapSize);
begin
  if Value <> FTabOverlap then
  begin
    FTabOverlap := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetImages(value: TCustomImageList);
begin
  FImages := Value;
  AdjustTabWidth;
  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetHoverGradientDirection(value: TGradientDirection);
begin
  FHoverGradientDirection := value;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetAdvTabStyle(Value: TAdvTabStyle);
begin
  if Value <> FAdvTabStyle then
  begin
    FAdvTabStyle := Value;
    if (FAdvTabStyle = tsDotNet) and (TabPosition in [pLeft, pRight]) then
      if TabMargin.TopMargin = 0 then
        TabMargin.TopMargin := 2;

    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetTabPosition(Value: TAdvTabPosition);
begin
  if Value <> FTabPosition then
  begin
    FTabPosition := Value;
    if TabPosition in [pTop, pBottom] then
      Scroller.ScrollPosition := spHorizontal
    else
    begin // pLeft, pRight
      if FAdvTabStyle = tsDotNet then
        if TabMargin.TopMargin = 0 then
          TabMargin.TopMargin := 2;
      Scroller.ScrollPosition := spVertical;
    end;
    FixTabPos;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetCloseButtonAt(Value: TCloseButtonPos);
begin
  if Value<> FCloseButtonAt then
  begin
    FCloseButtonAt:= Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetLowerSelected(Value: integer);
begin
  if Value <> FLowerSelected then
  begin
    FLowerSelected:= Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.SetShowScroller(Value: TShowScroller);
begin
  if FShowScroller <> Value then
  begin
    FShowScroller:= Value;
    Invalidate;
  end;
end;
//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.DrawCloseButtonAtTabSet(IsEnable: Boolean);
var
  aRect: TRect;
  P: TPoint;
begin
  if TabPosition in [pTop, pBottom] then
    aRect:= Rect(Width-CloseButtonWidth-1,3,Width-2, CloseButtonHeight+3 )
  else
    aRect:= Rect(3, Height-CloseButtonHeight, CloseButtonWidth+2, Height);

  if not FCloseGlyph.Empty then
  begin
    P:= Point(aRect.Left, aRect.Top);
    DrawCloseGlyph(Canvas, P, IsEnable);
  end
  else
  begin
    Canvas.Pen.Color:= self.BackgroundColor;
    Canvas.Brush.Style:= bsClear;
    Canvas.Rectangle(aRect.Left, aRect.Top, aRect.Right, aRect.Bottom);
    DrawCrossAtTabSet(IsEnable);
  end;
  FCloseButtonDown:= false;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.DrawCrossAtTabSet(IsEnable: Boolean);
var
  aRect: TRect;
begin
  if TabPosition in [pTop, pBottom] then
    aRect:= Rect(Width-CloseButtonWidth,1,Width, CloseButtonHeight + 2)
  else
    aRect:= Rect(4, Height-CloseButtonHeight+2, CloseButtonWidth+1, Height-1);

  with canvas do
  begin
    Pen.Color := cl3DDkShadow;
                  {/}
    MoveTo(aRect.Left+2 , aRect.Bottom - 4);
    LineTo(aRect.Left + 2 +6, aRect.Bottom - 2 - 8);
    if IsEnable then
    begin
      MoveTo(aRect.Left+2 , aRect.Bottom - 3);
      LineTo(aRect.Left + 2 +7, aRect.Bottom - 2 - 8);
    end;
    MoveTo(aRect.Left + 3, aRect.Bottom - 3);
    LineTo(aRect.Left + 2 + 7, aRect.Bottom - 2 - 7);
                  {\}
    MoveTo(aRect.Left + 2, aRect.Bottom - 2 - 6);
    LineTo(aRect.Left + 2 + 6, aRect.Bottom - 2);
    if IsEnable then
    begin
      MoveTo(aRect.Left + 2, aRect.Bottom - 2 - 7);
      LineTo(aRect.Left + 2 + 7, aRect.Bottom - 2);
    end;
    MoveTo(aRect.Left + 3, aRect.Bottom - 2 - 7);
    LineTo(aRect.Left + 2 + 7, aRect.Bottom - 3);
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomTabSet.DrawDownCloseButtonAtTabSet();
var
  aRect: TRect;
  P: TPoint;
begin
  if TabPosition in [pTop, pBottom] then
    aRect:= Rect(Width-CloseButtonWidth-1,3,Width-3, CloseButtonHeight+2 )
  else
    aRect:= Rect(3, Height-CloseButtonHeight, CloseButtonWidth+1, Height-1);

  if not FCloseGlyph.Empty then
  begin
    P:= Point(aRect.Left, aRect.Top);
    DrawCloseGlyph(Canvas, P, true);
  end
  else
  begin
    with canvas do
    begin
      Pen.Color := clBlack;
      MoveTo(aRect.Left, aRect.Bottom-1);
      LineTo(aRect.Left, aRect.Top);
      LineTo(aRect.Right, aRect.Top);

      Pen.Color := clWhite;
      MoveTo(aRect.Left, aRect.Bottom);
      LineTo(aRect.Right, aRect.Bottom);
      LineTo(aRect.Right, aRect.Top);
    end;
    DrawCrossAtTabSet(true);
  end;
  FCloseButtonDown:= true;
end;
//------------------------------------------------------------------------------
function TAdvCustomTabSet.IsOnCloseButtonAtTabSet(X, Y: integer): Boolean;
var
  aRect: TRect;
begin
  if TabPosition in [pTop, pBottom] then
    aRect:= Rect(Width-CloseButtonWidth-1,3,Width-3, CloseButtonHeight+2 )
  else
    aRect:= Rect(3, Height-CloseButtonHeight, CloseButtonWidth+1, Height-1);

  if PtInRect(aRect, Point(X, Y)) then
    Result:= true
  else
    Result:= false;
end;

//------------------------------------------------------------------------------

function TAdvCustomTabSet.CanCloseTab(TabIdx: Integer;
  var CloseAction: TCloseAction): Boolean;
begin
  Result := False;
  if (TabIdx >= 0) then
  begin
    Result := True;
    if Assigned(FOnCanClose) then
      FOnCanClose(self, TabIdx, Result);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomTabSet.CloseButtonClick;
var
  OldTabIndex: integer;
  CanClose: Boolean;
  ca: TCloseAction;
  i: Integer;
begin
  //CanClose:= true;
  //RealItemIndex := integer(FDuplicateTabs.objects[FTabIndex]);
  OldTabIndex:= integer(FDuplicateTabs.Objects[FTabIndex]);

  i := FAdvTabs.Count;
  ca := caFree;
  CanClose := CanCloseTab(OldTabIndex, ca);

  if CanClose and (ca <> caNone) then
  begin
    SelectNext(false);
    if Assigned(FOnTabClose) then
      FOnTabClose(Self, OldTabIndex);

    if (FAdvTabs.Count < i) then // Item delete externally ie: form deleted
      Exit;

    BeforeCloseTab(FAdvTabs.Items[OldTabIndex], ca);

    if FFreeOnClose and (ca = caFree) then
      FAdvTabs.Items[OldTabIndex].Free
    else if not (ca = caMinimize) then
      FAdvTabs.Items[OldTabIndex].Visible:= false;
  end;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomTabSet.SetActiveFont(const Value: TFont);
begin
  FActiveFont.Assign(Value);
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvCustomTabSet.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAdvCustomTabSet.SetVersion(const Value: string);
begin
end;

//------------------------------------------------------------------------------

function TAdvCustomTabSet.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvCustomTabSet.SetCloseGlyph(const Value: TBitmap);
begin
  FCloseGlyph.Assign(Value);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomTabSet.CloseGlyphOnChange(Sender: TObject);
var
  x, y: Integer;
  PxlColor: TColor;
  c: byte;
begin
  FDisableCloseGlyph.Assign(FCloseGlyph);
  if not FDisableCloseGlyph.Empty then
  begin
   FDisableCloseGlyph.Width := FCloseGlyph.Width;
   FDisableCloseGlyph.Height := FCloseGlyph.Height;
   FDisableCloseGlyph.PixelFormat := pf32bit;

    for x := 0 to FCloseGlyph.Width - 1 do
      for y := 0 to FCloseGlyph.Height - 1 do
      begin
        PxlColor := ColorToRGB(FCloseGlyph.Canvas.Pixels[x, y]);

        c := Round((((PxlColor shr 16) + ((PxlColor shr 8) and $00FF) +
          (PxlColor and $0000FF)) div 3)) div 2 + 128;

        FDisableCloseGlyph.Canvas.Pixels[x, y] := RGB(c, c, c);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomTabSet.SetClosePosition(const Value: TClosePosition);
begin
  if (FClosePosition <> Value) then
  begin
    FClosePosition := Value;
    Invalidate;
  end;
end;


//------------------------------------------------------------------------------

{ TAdvMDITabSet }

constructor TAdvMDITabSet.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;

  if not(AOwner is TForm) then
  begin
    raise Exception.create('AdvMDITabSet can only be placed on a Form.');
  end
  else if TForm(AOwner).FormStyle <> fsMDIForm then
  begin
    raise Exception.create('AdvMDITabSet can only be placed on a MDIForm.');
  end;

  for i := 0 to AOwner.ComponentCount - 1 do
  begin
    if (AOwner.Components[i] is TAdvMDITabSet) and (AOwner.Components[i] <> Self) then
    begin
      raise Exception.create('Only one instance of AdvMDITabSet can be placed on a MDIForm.');
    end;
  end;

  FreeOnClose := True;
end;

//------------------------------------------------------------------------------

destructor TAdvMDITabSet.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

function TAdvMDITabSet.AddTab(ChildForm: TForm): TTabCollectionItem;
var
  i: Integer;
begin
  Result := nil;

  if not Assigned(ChildForm) then
    Exit;

  for i:= 0 to FAdvTabs.Count-1 do
  begin
    if (FAdvTabs.Items[i].FChildForm = ChildForm) then
    begin
      Exit;
    end;
  end;

  Result := FAdvTabs.Add;
  Result.Caption := ChildForm.Caption;
  Result.FChildForm := ChildForm;
  Result.FOnActivateForm := ChildForm.OnActivate;
  Result.FOnDestroyForm := ChildForm.OnDestroy;

  ChildForm.OnActivate := OnChildFormActivate;
  ChildForm.OnDestroy := OnChildFormDestroy;


  TabIndex := RealToDisplTabIndex(Result.Index);
  //if not (csLoading in ComponentState) then
    //InitializeAndUpdateButtons;
end;

//------------------------------------------------------------------------------

function TAdvMDITabSet.GetAdvTabCount: integer;
begin
  Result := FAdvTabs.Count;
end;

//------------------------------------------------------------------------------

function TAdvMDITabSet.GetAdvTabs(index: integer): TTabCollectionItem;
begin
  Result := nil;
  if (Index >= 0) and (Index < FAdvTabs.Count) then
  begin
    Result := FAdvTabs[Index];
  end;
end;

//------------------------------------------------------------------------------

function TAdvMDITabSet.GetChildForm(Tab: TTabCollectionItem): TForm;
begin
  Result := nil;
  if Assigned(Tab) then
  begin
    Result := Tab.FChildForm;
  end;
end;

//------------------------------------------------------------------------------

function TAdvMDITabSet.GetTab(AChild: TForm): TTabCollectionItem;
var
  i: integer;
begin
  Result := nil;
  for I := 0 to FAdvTabs.Count - 1 do
  begin
    if FAdvTabs.Items[I].FChildForm = AChild then
    begin
      Result := FAdvTabs.Items[I];
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMDITabSet.OnChildFormActivate(Sender: TObject);
var
  i, j:integer;
begin
  if (Sender is TForm) and (TForm(Sender).FormStyle = fsMDIChild) then
  begin
    j := -1;
    for i:= 0 to FAdvTabs.count-1 do
    begin
      if (FAdvTabs[i].FChildForm = Sender) then
      begin
        if not FAdvTabs[i].Visible then
        begin
          FAdvTabs[i].Visible := True;
        end;
        j := i;
        TabIndex := RealToDisplTabIndex(i);
        break;
      end;
    end;

    if (j >= 0) and (j < FAdvTabs.count) and Assigned(FAdvTabs[j]) and Assigned(FAdvTabs[j].FOnActivateForm)
       and Assigned(FAdvTabs[j].FChildForm) then
    begin
      FAdvTabs[j].FOnActivateForm(FAdvTabs[j].FChildForm);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMDITabSet.OnChildFormDestroy(Sender: TObject);
var
  i, j :integer;
begin
  if not (csDestroying in ComponentState) and (Sender is TForm) and (TForm(Sender).FormStyle = fsMDIChild) then
  begin
    j := -1;
    for i:= 0 to FAdvTabs.count-1 do
    begin
      if (FAdvTabs[i].FChildForm = Sender) then
      begin
        j := i;
        break;
      end;
    end;

    if (j >= 0) and Assigned(FAdvTabs[j]) and Assigned(FAdvTabs[j].FOnDestroyForm)
       and Assigned(FAdvTabs[j].FChildForm) then
    begin
      FAdvTabs[j].FOnDestroyForm(FAdvTabs[j].FChildForm);
    end;

    if (j >= 0) and not FInternalDelete then
    begin
      if (FAdvTabs[j].FIndexInAdvTabSet = TabIndex) then
        SelectNext(False);
      FAdvTabs.Delete(j);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMDITabSet.Change;
var
  RealTabIndex: Integer;
begin
  RealTabIndex := DisplToRealTabIndex(TabIndex);
  if (RealTabIndex < 0) or (RealTabIndex >= FAdvTabs.Count) then
    Exit;

  if Assigned(FAdvTabs[RealTabIndex].FChildForm) then
  begin
    SendMessage(FAdvTabs[RealTabIndex].FChildForm.Handle, WM_NCActivate, WA_ACTIVE, 0);

    if FAdvTabs[RealTabIndex].FChildForm.Windowstate = WSMinimized then
      FAdvTabs[RealTabIndex].FChildForm.WindowState := WSNormal;

    FAdvTabs[RealTabIndex].FChildForm.BringToFront;
    FAdvTabs[RealTabIndex].FChildForm.SetFocus;
    FAdvTabs[RealTabIndex].FChildForm.Visible := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMDITabSet.BeforeCloseTab(Tab: TTabCollectionItem; var CloseAction: TCloseAction);
begin
  if not Assigned(Tab) or not Assigned(Tab.FChildForm) then
  begin
    inherited;
    Exit
  end;

  if FreeOnClose then
  begin
    case CloseAction of
      caFree:
      begin
        //FInternalDelete := True;
        Tab.FChildForm.Release;
        CloseAction := caMinimize; // just to avoid deletion of tab that should be deleted on form's destoy
        //FInternalDelete := False;
      end;
      caMinimize: Tab.FChildForm.Windowstate := WSMinimized;
      caHide: Tab.FChildForm.visible := False;
    end;
  end
  else
  begin
    Tab.FChildForm.visible := False;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

function TAdvMDITabSet.CanCloseTab(TabIdx: Integer;
  var CloseAction: TCloseAction): Boolean;
var
  Tab: TTabCollectionItem;
begin
  Result := inherited CanCloseTab(TabIdx, CloseAction);
  if not Result or (TabIdx < 0) or (TabIdx >= FAdvTabs.Count) then
    Exit;

  Tab := FAdvTabs.Items[TabIdx];
  if not Assigned(Tab.FChildForm) then
    Exit;

  if Assigned(Tab.FChildForm.OnCloseQuery) then
  begin
    Tab.FChildForm.OnCloseQuery(Tab.FChildForm, Result);
  end;

  if not Result then
    Abort;

  if FreeOnClose then
  begin
    if Assigned(Tab.FChildForm.OnClose) then
    begin
      Tab.FChildForm.OnClose(Tab.FChildForm, CloseAction);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMDITabSet.ChangeActiveTab(Value: Integer);
var
  OldActiveTabIndex: Integer;
begin
  OldActiveTabIndex := TabIndex;

  inherited;

  if (Value >= 0) and (Value < FAdvTabs.Count) and (Value <> OldActiveTabIndex) then
  begin
    Change;
  end;
end;

//------------------------------------------------------------------------------


end.

