{***********************************************************************}
{ TAdvNavBar component                                                  }
{ for Delphi & C++ Builder                                              }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 1999-2015                                      }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the writer and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}

unit AdvNavBar;

{$I TMSDEFS.INC}

interface

uses
  Classes, Controls, ExtCtrls, Dialogs, Messages, Windows, Forms, AdvStyleIF,
  AdvGDIP, ImgList, SysUtils, Graphics, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  CaptionHeight = 27;
  DefSplitterHeight = 7;
  ScrollerWidth = 20;
  FixedBtnWidth = 24;
  SectionHeight = 16;
  IndicatorWidth = 20;
  IndicatorWidthOffice2010 = 14;
  DefCollapsedWidth = 40;

  MAJ_VER = 2; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.1.0.2 : SplitterPosition control fix for runtime created AdvNavBar
  // 1.2.0.0 : New method MovePanel(FromIndex, ToIndex) added
  // 1.2.0.1 : Added UpdateScroller in AdvNavBar.WMSize
  // 1.3.0.0 : Added SmallImages property
  //         : Added DownTextColor, HoverTextColor properties
  // 1.3.0.1 : splitter position control when resizing height added
  // 1.3.0.2 : fix for enforcing MouseLeave message when mouse is over splitter
  // 1.3.0.3 : added esCustom style
  // 1.3.5.0 : Fixed: cursor handling over splitter
  //         : New: esWhidbey style
  // 1.3.6.0 : Whidbey style via design
  // 1.3.7.0 : Virtual DoChange method added
  // 1.3.8.0 : Added Tag property in TAdvNavBarPanelSection
  // 1.3.8.1 : Fixed issue with remove panel in Delphi 2006
  // 1.4.0.0 : New Style interface added
  //         : New Office 2007 Luna & Obsidian styles added
  // 1.5.0.0 : New capability to collaps TAdvNavBar
  //         : New Runtime section sizing
  //         : New Handpoint cursor for mouse over panel tab
  //         : New CaptionHint added in TAdvNavBarPanel
  //         : New CaptionAlignment added in TAdvNavBarPanel
  //         : New automatic tab panel caption display when no panel image is specified
  // 1.5.0.1 : New public property DoEraseBkg added
  // 1.5.1.0 : New Office2007Silver style added
  //         : Fixed issue with hidden panels & collaps
  // 1.5.1.1 : Fixed issue with OnCollapsChange event
  // 1.5.2.0 : New : SectionFont property added
  // 1.5.2.1 : Fixed issue with mouse up on panel icon during splitter move
  // 1.5.2.2 : Fixed issue with caption font redraw
  // 1.5.2.3 : Fixed memory leak with SectionFont
  // 1.5.2.4 : Fixed issue with RemovePanel()
  // 1.5.2.5 : Fixed issue with down state painting
  // 1.5.2.6 : Fixed issue with splitterposition for client aligned navbar on maximized form
  // 1.5.2.7 : Fixed issue with removing all panels and handling events
  // 1.5.2.8 : Fixed issue with assigning sections
  // 1.5.3.0 : New : property CaptionAlignment added
  // 1.5.4.0 : New : AdvNavBarPanel.Assign() added
  // 1.5.4.1 : Fixed : issue with drawing tab caption for non visible panels
  // 1.5.4.2 : Fixed : issue with adapting splitter position on maximize forms
  // 1.5.5.0 : New : property PanelOrder added
  // 1.5.5.1 : Fixed : issue with hover & hidden panels 
  // 1.5.5.2 : Fixed : painting issue with Delphi 2009
  // 1.5.5.3 : Fixed : splitter behaviour during minimize/maximize of form
  // 1.5.5.4 : Fixed : TAdvNavBar.OnClick event issue
  // 1.6.0.0 : New : Terminal, Vista & Windows 7 styles
  // 1.6.0.1 : Fixed : issue with fixed caption painting
  // 1.6.0.2 : Improved : painting
  // 1.7.0.0 : New : Built in support for Office 2010 colors
  // 1.7.1.0 : New : TAdvNavBarPanel.CaptionStyle added
  // 1.7.1.1 : Fixed : Issue with setting TabVisible = false & toggling Collapsed
  // 2.0.0.0 : New : Office 2010 style added
  //         : New : Possibility to set border visibility
  //         : New : Possibility to hide caption
  //         : New : Possibility to hide shortcutbar
  //         : Fixed : Issue with copy & paste in IDE
  // 2.0.0.1 : Fixed : Painting issue with CaptionVisible = false and BorderStyle = bsSingle
  //         : Fixed : Painting issue in older Delphi versions
  // 2.0.0.2 : Fixed : Issue with painting collapsed caption bar & collapsed images
  // 2.0.0.3 : Fixed : Issue with ActiveTabIndex persistence
  // 2.0.1.0 : New : Property ActiveTextColor added
  // 2.0.1.1 : Fixed : Issue with incorrect triggering of OnCollapsedClick
  // 2.0.1.2 : Fixed : Issue with RemovePanel call & PanelIndex
  // 2.0.1.3 : Improved : Painting performance in older Delphi versions
  // 2.0.1.4 : Fixed : Issue with OnClick event
  // 2.0.1.5 : Fixed : Paint issue with programmatically fixing tab index
  // 2.0.1.6 : Fixed : Issue with tab caption image drawing
  // 2.0.1.7 : Fixed : Issue with fixed caption drawing when collapsed captions overflow
  // 2.0.2.0 : New : Windows 8, Office 2013 styles added
  // 2.0.2.1 : Fixed : Issue with collaps & disabled controls
  // 2.0.2.2 : Fixed : Issue with handling hidden panels
  // 2.0.2.3 : Fixed : Painting issue when CaptionVisible = false
  // 2.0.2.4 : New: Delphi XE5 & C++Builder XE5 support
  // 2.0.2.5 : Fixed : Issue with caption painting under specific circumstances
  // 2.0.2.6 : Fixed : Issue with removing hidden panels
  // 2.0.2.7 : Fixed : Issue with handling hints
  // 2.1.0.0 : New : Windows 10, Office 2016 styles added

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TGradientDirection = (gdVertical, gdHorizontal);
  TBottomIconAlign = (biLeft, biRight);
  TDefaultTabPosition = (tpTop, tpBottom);

  TAdvNavBar = class;
  TAdvNavBarPanel = class;

  TAdvNavBarPanelSection = class(TCollectionItem)
  private
    FHeight: Integer;
    FCaption: string;
    FTag: Integer;
    FControl: TWinControl;
    procedure SetCaption(const Value: string);
    procedure SetHeight(const Value: Integer);
    procedure SetControl(const Value: TWinControl);
    procedure UpdateControlBounds;
    function GetOwnerPanel: TAdvNavBarPanel;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: string read FCaption write SetCaption;
    property Control:TWinControl read FControl write SetControl;
    property Height: Integer read FHeight write SetHeight default 48;
    property Tag: Integer read FTag write FTag default 0;
  end;

  TAdvNavBarPanelSections = class(TCollection)
  private
    FOwner: TAdvNavBarPanel;
    function GetItem(Index: Integer): TAdvNavBarPanelSection;
    procedure SetItem(Index: Integer; const Value: TAdvNavBarPanelSection);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvNavBarPanel);
    function Add: TAdvNavBarPanelSection;
    function Insert(Index: Integer): TAdvNavBarPanelSection;
    property Items[Index: Integer]: TAdvNavBarPanelSection read GetItem write SetItem; default;
  end;

  TAdvNavBarPanel = class(TCustomPanel)
  private
    FFontStyles: TFontStyles;
    FAdvNavBar: TAdvNavBar;
    FTextColor: TColor;
    FImageIndex: TImageIndex;
    FPanelIndex: integer;
    FCaption: string;
    FCollapsedCaption:String;
    FColorTo: TColor;
    FGradientDirection: TGradientDirection;
    FSections: TAdvNavBarPanelSections;
    FSizing: Boolean;
    FSizeSection: Integer;
    FSizeHeight: Integer;
    FSizeY: Integer;
    FTabVisible: Boolean;
    FSectionSizing: Boolean;
    FCaptionHint: string;
    FCaptionAlignment: TAlignment;
    procedure SetAdvNavBar(Value: TAdvNavBar);
    procedure SetTextColor(const Value: TColor);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    procedure SetCollapsedCaption(const Value:string);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetFPanelIndex(const Value: integer);
    procedure SetColorTo(const Value: TColor);
    procedure SetGradientDirection(const Value: TGradientDirection);
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure WMEraseBkGnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    function GetEnable: Boolean;
    procedure SetEnable(const Value: Boolean);
    function GetTabVisible: Boolean;
    procedure SetTabVisible(const Value: Boolean);
    function GetAdjustedCaption:String;
    procedure UpdateControlBounds;
    procedure SetCaptionAlignment(const Value: TAlignment);
    procedure SetFontStyles(const Value: TFontStyles);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ReadState(Reader: TReader); override;
    procedure SetPosInAdvNavBar;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property AdvNavBar: TAdvNavBar read FAdvNavBar write SetAdvNavBar;
    property TabVisible: Boolean read GetTabVisible write SetTabVisible;
    property AdjustedCaption:String read GetAdjustedCaption;
  published
    property Caption: string read Getcaption write SetCaption;
    property CaptionAlignment: TAlignment read FCaptionAlignment write SetCaptionAlignment default taLeftJustify;
    property CaptionHint: string read FCaptionHint write FCaptionHint;
    property CaptionStyle: TFontStyles read FFontStyles write SetFontStyles;

    property CollapsedCaption: string read FCollapsedCaption write SetCollapsedCaption;
    property Color;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property GradientDirection: TGradientDirection read FGradientDirection write SetGradientDirection default gdVertical;
    property Font;
    property Hint;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property PanelIndex: Integer read FPanelIndex write SetFPanelIndex;
    property PopupMenu;
    property Sections: TAdvNavBarPanelSections read FSections write FSections;
    property SectionSizing: boolean read FSectionSizing write FSectionSizing default false;
    property ShowHint;
    property TextColor: TColor read FTextColor write SetTextColor default clBlack;
    property Enabled: Boolean read GetEnable write SetEnable default True;
    { inherited events }
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnExit;
    property OnEnter;
    property OnResize;
    property OnStartDrag;
    property OnContextPopup;
  end;

  TExchangeScroller = class(TObject)
  private
    FMin: integer;
    FMax: integer;
    FPosition: integer;
    FVisible: Boolean;
    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    procedure SetPosition(const Value: integer);
    procedure SetVisible(const Value: Boolean);
  protected
  public
    constructor Create;
    function CanGoForward: Boolean;
    function CanGoBack: Boolean;
    property Min: integer read FMin write SetMin;
    property Max: integer read FMax write SetMax;
    property Position: integer read FPosition write SetPosition;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  TAdvNavBarStyle = (esOffice2003Blue, esOffice2003Silver, esOffice2003Olive, esOffice2003Classic, esOffice2007Luna, esOffice2007Obsidian, esWindowsXP, esWhidbey, esCustom, esOffice2007Silver,
    esWindowsVista, esWindows7, esTerminal, esOffice2010Blue, esOffice2010Silver, esOffice2010Black,
    esWindows8, esOffice2013White, esOffice2013LightGray, esOffice2013Gray,
    esWindows10, esOffice2016White, esOffice2016Gray, esOffice2016Black);

  TOnPanelActivate = procedure(Sender: TObject; OldActivePanel: Integer; NewActivePanel: Integer; var Allow: Boolean) of object;
  TSplitterMove = procedure(Sender: TObject; OldSplitterPosition: integer; NewSplitterPosition: Integer) of object;

  TPanelOrder = (poBottomToTop, poTopToBottom);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvNavBar = class(TCustomControl, ITMSStyle)
  private
    FAdvNavBarPanel: TList;
    FDupAdvNavBarPanel: TList;
    FTempAdvNavBarPanel: TList;
    FSeparatorLine: integer;
    FCaptionTabHeight: integer;
    FActiveColor: TColor;
    FActiveColorTo: TColor;
    FActiveTextColor: TColor;
    FDefaultTabColor: TColor;
    FDefaultTabColorTo: TColor;
    FDefaultTextColor: TColor;
    FActiveTabIndex: integer;
    FHoverTabIndex: integer;
    FDownTabIndex: integer;
    FSplitterColor: TColor;
    FSplitterColorTo: TColor;
    FOldCursor: TCursor;
    FMouseCaptured: Boolean;
    FCheckForSplitterMove: Boolean;
    FDisplayCaptionTabsCount: integer;
    FHoverTabColor: TColor;
    FHoverTabColorTo: TColor;
    FDownTabColorTo: TColor;
    FDownTabColor: TColor;
    FImages: TCustomImageList;
    FMinClientHeight: integer;
    FDefaultGradientDirection: TGradientDirection;
    FFixedBtnMargin: integer;
    FBottomIconAlign: TBottomIconAlign;
    FSplitterPosition: Integer;
    FTempSplitterPos: Integer;
    FPropertiesLoaded: Boolean;
    FScroller: TExchangeScroller;
    FScrollerHoverLeftBtn: Boolean;
    FScrollerDownLeftBtn: Boolean;
    FScrollerHoverRightBtn: Boolean;
    FScrollerDownRightBtn: Boolean;
    FPopupIndicatorHover: Boolean;
    FPopupIndicatorDown: Boolean;
    FActivePanel: TAdvNavBarPanel;
    FPropertiesCreated: Boolean;
    FOnChange: TNotifyEvent;
    FCaptionColor: TColor;
    FCaptionColorTo: TColor;
    FCaptionFont: TFont;
    FBorderColor: TColor;
    FStyle: TAdvNavBarStyle;
    FSectionColorTo: TColor;
    FSectionColor: TColor;
    FSectionFont: TFont;
    FOnTPanelActivate: TOnPanelActivate;
    FOnSplitterMove: TSplitterMove;
    FPopupIndicator: Boolean;
    FOnPopupMenuClick: TNotifyEvent;
    FOriginalHint: string;
    FSplitterInternalCall: Boolean;
    FAutoThemeAdapt: Boolean;
    FDefaultTabPosition: TDefaultTabPosition;
    FInternalCall: Boolean;
    FDownTextColor: TColor;
    FHoverTextColor: TColor;
    FSmallImages: TCustomImageList;
    FOldSplitterPosForSizeChange: integer;
    FInternalSplitterMove: Boolean;
    FMouseDown: Boolean;
    FUpdateCount: Integer;
    FCollapsed: Boolean;
    FPreCollapsedWidth: Integer;
    FIsCollapsing: Boolean;
    FCollapsedCaption: string;
    FAllowCollaps: boolean;
    FHoverCollapsedCaption: Boolean;
    FDownCollapsedCaption: Boolean;
    FCollapsDown: Boolean;
    FCollapsedWidth: Integer;
    FHoverTabMirrorColor: TColor;
    FHoverTabMirrorColorTo: TColor;
    FDownTabMirrorColor: TColor;
    FDownTabMirrorColorTo: TColor;
    FActiveMirrorColor: TColor;
    FActiveMirrorColorTo: TColor;
    FDefaultTabMirrorColor: TColor;
    FDefaultTabMirrorColorTo: TColor;
    FCollapsedDownColor: TColor;
    FCollapsedDownColorTo: TColor;
    FCollapsedHoverColor: TColor;
    FCollapsedHoverColorTo: TColor;
    FOnCollapsedClick: TNotifyEvent;
    FOnCollapsChange: TNotifyEvent;
    FCaptionHintIndex: Integer;
    FShowSplitter: boolean;
    FHoverCollapsBtn: boolean;
    FDoEraseBkg: boolean;
    FCaptionAlignment: TAlignment;
    FReverseOrder: boolean;
    FSplitterBorderColor: TColor;
    FCaptionVisible: Boolean;
    FShowShortCutBar: Boolean;
    FBorderStyle: TBorderStyle;
    procedure SetCaptionTabHeight(Value: integer);
    procedure ShowAdvNavBarPanel(PanelIndex: integer);
    procedure SetAllAdvNavBarPanelPosition;
    procedure DrawAllCaptionTabs;
    procedure DrawCaptionTab(Index: integer);
    procedure DrawFixedTab;
    procedure DrawCaption;
    procedure DrawAllFixedTabButtons;
    procedure DrawFixedTabButton(Index: integer);
    procedure DrawScrollButtons;
    procedure DrawScrollBtnLeft(R:TRect);
    procedure DrawScrollBtnRight(R:TRect);
    procedure DrawCollapsButton(Collaps: Boolean);
    function PtInCollapsButton(X,Y: Integer): Boolean;
    procedure DrawPopupIndicator;
    procedure RefreshCaptionTabOrButton(index: integer);
    procedure DrawSplitter;
    procedure SetActiveColor(const Value: TColor);
    procedure SetActiveColorTo(const Value: TColor);
    procedure SetDefaultTabColor(const Value: TColor);
    procedure SetDefaultTabColorTo(const Value: TColor);
    procedure SetDefaultTextColor(const Value: TColor);
    procedure SetActiveTabIndex(const Value: integer);
    function GetActiveTabIndex: integer;
    function GetSplitterRect: TRect;
    function GetCaptionRect: TRect;
    function PtOnSplitter(P: TPoint): Boolean;
    procedure UpdateScroller;
    function MoveSplitterTo(Y: integer): Boolean; // Return true if moved, false on unMoved
    function MoveSplitterInTabs(TabCount: integer): integer; // -ve to move up and +ve to move down, and returns actuall moved in tabs.
    function GetFixedTabRect: TRect;
    function FixedTabButtonRect(index: integer): TRect;
    function FixedBtnMinIndex: integer;
    function FixedBtnMaxIndex: integer;
    function GetFixedTabButtonCount: integer;
    function GetFixedTabButtonMaxCount: integer;
    function PtOnScrollLeftBtn(X, Y: integer): Boolean;
    function PtOnScrollRightBtn(X, Y: integer): Boolean;
    function PtOnPopupIndicator(X, Y: integer): Boolean;
    procedure ShowScrollButtons;
    procedure HideScrollButtons;
    procedure ScrollLeftBtnClick;
    procedure ScrollRightBtnClick;
    procedure SetHoverTabColor(const Value: TColor);
    procedure SetHoverTabColorTo(const Value: TColor);
    procedure SetDownTabColor(const Value: TColor);
    procedure SetDownTabColorTo(const Value: TColor);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetMinClientHeight(const Value: integer);
    procedure SetDefaultGradientDirection(const Value: TGradientDirection);
    procedure SetBottomIconAlign(const Value: TBottomIconAlign);
    procedure SetSplitterPosition(const Value: integer);
    procedure SetActivePanel(const Value: TAdvNavBarPanel);
    procedure SetCaptionColor(const Value: TColor);
    procedure SetCaptionColorTo(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetCaptionFont(const Value: TFont);
    procedure SetSectionFont(const Value: TFont);
    procedure SetSplitterColor(const Value: TColor);
    procedure SetSplitterColorTo(const Value: TColor);
    procedure SetStyle(const Value: TAdvNavBarStyle);
    procedure SetSectionColor(const Value: TColor);
    procedure SetSectionColorTo(const Value: TColor);
    procedure SetPopupIndicator(const Value: Boolean);
    procedure ThemeAdapt;
    procedure SetDefaultTabPosition(const Value: TDefaultTabPosition);
    function GetDupIndex(Index: integer): integer;
    function GetDisplayIndex(Index: integer): integer;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetDownTextColor(const Value: TColor);
    procedure SetHoverTextColor(const Value: TColor);
    procedure SetSmallImages(const Value: TCustomImageList);
    procedure SetCollapsed(const Value:Boolean);
    procedure SetCollapsedCaption(const Value:String);
    procedure DrawCollapsedCaption;
    procedure SetAllowCollaps(const Value: Boolean);
    procedure SetDownTabMirrorColor(const Value: TColor);
    procedure SetDownTabMirrorColorTo(const Value: TColor);
    procedure SetHoverTabMirrorColor(const Value: TColor);
    procedure SetHoverTabMirrorColorTo(const Value: TColor);
    procedure SetActiveMirrorColor(const Value: TColor);
    procedure SetActiveMirrorColorTo(const Value: TColor);
    procedure SetDefaultTabMirrorColor(const Value: TColor);
    procedure SetDefaultTabMirrorColorTo(const Value: TColor);
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure CMHintShow(var Msg: TCMHintShow); message CM_HINTSHOW;
    procedure SetShowSplitter(const Value: boolean);
    procedure FontChanged(sender: TObject);
    procedure SetCaptionAlignment(const Value: TAlignment);
    function CaptionTabMaxIndex: integer;
    procedure SetReverseOrder(const Value: boolean);
    function GetPanelOrder: TPanelOrder;
    procedure SetPanelOrder(const Value: TPanelOrder);
    procedure SetSplitterBorderColor(const Value: TColor);
    procedure SetCaptionVisible(const Value: Boolean);
    procedure SetShowShortCutBar(const Value: Boolean);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetActiveTextColor(const Value: TColor);
  protected
    function GetVersionNr: Integer; virtual;
    function CaptionTabRect(Index: Integer): TRect;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkGnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure Paint; override;
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure SetAdvNavBarPanelPosition(AAdvNavBarPanel: TAdvNavBarPanel);
    property CaptionTabHeight: integer read FCaptionTabHeight write SetCaptionTabHeight;
    function GetPanelRect: TRect;
    procedure InsertAdvNavBarPanel(Value: TAdvNavBarPanel);
    procedure RemoveAdvNavBarPanel(Value: TAdvNavBarPanel);
    function GetPanel(Index: Integer): TAdvNavBarPanel;
    function GetPanelCount: Integer;
    procedure WndProc(var Message: TMessage); override;
    procedure UpdateList;
    procedure Resize; override;
    procedure DoChange; virtual;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    function SplitterHeight: integer;
    procedure Click; override;
    function IsOffice2010: Boolean;
    function GetIndicatorWidth: Integer;
    function GetCaptionHeight: Integer;
    function GetCaptionTabHeight: Integer;
    function GetFixedCaptionTabHeight: Integer;
  public
    constructor Create(Aowner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function CreatePanel: TAdvNavBarPanel; virtual;
    function AddPanel: TAdvNavBarPanel;
    function InsertPanel(Index: Integer): TAdvNavBarPanel;
    procedure RemovePanel(Index: Integer);
    procedure MovePanel(FromIndex, ToIndex: Integer);
    property Panels[Index: Integer]: TAdvNavBarPanel read GetPanel;
    property PanelCount: Integer read GetPanelCount;
    property DoEraseBkg: boolean read FDoEraseBkg write FDoEraseBkg;

    procedure SelectNextPanel;
    procedure SelectPreviousPanel;

    function IndexOfTabAt(X, Y: integer): Integer;
    function IndexOfCaptionTabAt(X, Y: integer): Integer;
    function IndexOfBtnAt(X, Y: integer): Integer;

    function FindNextPanelIndex(CurIndex: Integer; GoForward: Boolean): integer;
    function FindNextPanel(CurPanel: TAdvNavBarPanel; GoForward: Boolean): TAdvNavBarPanel;
    property ActivePanel: TAdvNavBarPanel read FActivePanel write SetActivePanel;
    procedure SetComponentStyle(AStyle: TTMSStyle);
  published
    property Align;
    property Anchors;
    property Color;

    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;

    property ActiveColor: TColor read FActiveColor write SetActiveColor default clBtnFace;
    property ActiveColorTo: TColor read FActiveColorTo write SetActiveColorTo default clNone;
    property ActiveMirrorColor: TColor read FActiveMirrorColor write SetActiveMirrorColor default clNone;
    property ActiveMirrorColorTo: TColor read FActiveMirrorColorTo write SetActiveMirrorColorTo default clNone;
    property ActiveTextColor: TColor read FActiveTextColor write SetActiveTextColor default clWindowText;

    property ActiveTabIndex: integer read GetActiveTabIndex write SetActiveTabIndex;
    property AllowCollaps: Boolean read FAllowCollaps write SetAllowCollaps default false;
    property AutoThemeAdapt: Boolean read FAutoThemeAdapt write FAutoThemeAdapt default False;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;
    property BottomIconAlign: TBottomIconAlign read FBottomIconAlign write SetBottomIconAlign default biRight;
    property CaptionVisible: Boolean read FCaptionVisible write SetCaptionVisible default True;
    property CaptionAlignment: TAlignment read FCaptionAlignment write SetCaptionAlignment default taLeftJustify;
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor default clGray;
    property CaptionColorTo: TColor read FCaptionColorTo write SetCaptionColorTo default clNone;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property Collapsed: Boolean read FCollapsed write SetCollapsed default false;
    property CollapsedCaption: string read FCollapsedCaption write SetCollapsedCaption;
    property CollapsedWidth: integer read FCollapsedWidth write FCollapsedWidth default DefCollapsedWidth;
    property CollapsedHoverColor: TColor read FCollapsedHoverColor write FCollapsedHoverColor default clInfoBk;
    property CollapsedHoverColorTo: TColor read FCollapsedHoverColorTo write FCollapsedHoverColorTo default clNone;
    property CollapsedDownColor: TColor read FCollapsedDownColor write FCollapsedDownColor default clSilver;
    property CollapsedDownColorTo: TColor read FCollapsedDownColorTo write FCollapsedDownColorTo default clNone;

    property Constraints;
    property DefaultGradientDirection: TGradientDirection read FDefaultGradientDirection write SetDefaultGradientDirection;
    property DefaultTextColor: TColor read FDefaultTextColor write SetDefaultTextColor default clBlack;
    property DefaultTabColor: TColor read FDefaultTabColor write SetDefaultTabColor default clWhite;
    property DefaultTabColorTo: TColor read FDefaultTabColorTo write SetDefaultTabColorTo default clBtnFace;
    property DefaultTabMirrorColor: TColor read FDefaultTabMirrorColor write SetDefaultTabMirrorColor default clNone;
    property DefaultTabMirrorColorTo: TColor read FDefaultTabMirrorColorTo write SetDefaultTabMirrorColorTo default clNone;
    property DefaultTabPosition: TDefaultTabPosition read FDefaultTabPosition write SetDefaultTabPosition default tpBottom;
    property DownTabColor: TColor read FDownTabColor write SetDownTabColor;
    property DownTabColorTo: TColor read FDownTabColorTo write SetDownTabColorTo;
    property DownTabMirrorColor: TColor read FDownTabMirrorColor write SetDownTabMirrorColor;
    property DownTabMirrorColorTo: TColor read FDownTabMirrorColorTo write SetDownTabMirrorColorTo;
    property DownTextColor: TColor read FDownTextColor write SetDownTextColor default clBlack;
    property Font;
    property HoverTabColor: TColor read FHoverTabColor write SetHoverTabColor;
    property HoverTabColorTo: TColor read FHoverTabColorTo write SetHoverTabColorTo;
    property HoverTabMirrorColor: TColor read FHoverTabMirrorColor write SetHoverTabMirrorColor;
    property HoverTabMirrorColorTo: TColor read FHoverTabMirrorColorTo write SetHoverTabMirrorColorTo;

    property HoverTextColor: TColor read FHoverTextColor write SetHoverTextColor default clBlack;
    property MinClientHeight: integer read FMinClientHeight write SetMinClientHeight default 60;
    property Images: TCustomImageList read FImages write SetImages;
    property PanelOrder: TPanelOrder read GetPanelOrder write SetPanelOrder default poBottomToTop;
    property PopupIndicator: Boolean read FPopupIndicator write SetPopupIndicator default True;
    property PopupMenu;
    property SectionColor: TColor read FSectionColor write SetSectionColor;
    property SectionColorTo: TColor read FSectionColorTo write SetSectionColorTo;
    property SectionFont: TFont read FSectionFont write SetSectionFont;
    property ShowHint;
    property ShowSplitter: boolean read FShowSplitter write SetShowSplitter default true;
    property ShowShortCutBar: Boolean read FShowShortCutBar write SetShowShortCutBar default True;
    property SmallImages: TCustomImageList read FSmallImages write SetSmallImages;
    property SplitterPosition: integer read FSplitterPosition write SetSplitterPosition;
    property SplitterColor: TColor read FSplitterColor write SetSplitterColor;
    property SplitterColorTo: TColor read FSplitterColorTo write SetSplitterColorTo;
    property SplitterBorderColor: TColor read FSplitterBorderColor write SetSplitterBorderColor default clNone;
    property Style: TAdvNavBarStyle read FStyle write SetStyle default esOffice2003Blue;
    property Visible;
    property Version: string read GetVersion write SetVersion;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnCollapsChange: TNotifyEvent read FOnCollapsChange write FOnCollapsChange;
    property OnCollapsedClick: TNotifyEvent read FOnCollapsedClick write FOnCollapsedClick;
    property OnPanelActivate: TOnPanelActivate read FOnTPanelActivate write FOnTPanelActivate;
    property OnSplitterMove: TSplitterMove read FOnSplitterMove write FOnSplitterMove;
    property OnPopupMenuClick: TNotifyEvent read FOnPopupMenuClick write FOnPopupMenuClick;

    //MAYBE an overide of dblclick that allows for autoexpanding....

    { inherited events }
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnExit;
    property OnEnter;
    property OnStartDrag;
    property OnContextPopup;
    property OnResize;
    property OnCanResize;

  end;

implementation

uses
  ComObj, Math;

const
  // theme changed notifier
  WM_THEMECHANGED = $031A;

type
  XPColorScheme = (xpNone, xpBlue, xpGreen, xpGray);

var
  GetCurrentThemeName: function(pszThemeFileName: PWideChar;
    cchMaxNameChars: Integer;
    pszColorBuff: PWideChar;
    cchMaxColorChars: Integer;
    pszSizeBuff: PWideChar;
    cchMaxSizeChars: Integer): THandle cdecl stdcall;

  IsThemeActive: function: BOOL cdecl stdcall;


function IsWinXP: Boolean;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);
  Result := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));
end;

function CurrentXPTheme: XPColorScheme;
var
  FileName, ColorScheme, SizeName: WideString;
  hThemeLib: THandle;
begin
  hThemeLib := 0;
  Result := xpNone;

  if not IsWinXP then
    Exit;

  try
    hThemeLib := LoadLibrary('uxtheme.dll');

    if hThemeLib > 0 then
    begin
      IsThemeActive := GetProcAddress(hThemeLib,'IsThemeActive');

      if Assigned(IsThemeActive) then
        if IsThemeActive then
        begin
          GetCurrentThemeName := GetProcAddress(hThemeLib,'GetCurrentThemeName');
          if Assigned(GetCurrentThemeName) then
          begin
            SetLength(FileName, 255);
            SetLength(ColorScheme, 255);
            SetLength(SizeName, 255);
            OleCheck(GetCurrentThemeName(PWideChar(FileName), 255,
              PWideChar(ColorScheme), 255, PWideChar(SizeName), 255));
            if (PWideChar(ColorScheme) = 'NormalColor') then
              Result := xpBlue
            else if (PWideChar(ColorScheme) = 'HomeStead') then
              Result := xpGreen
            else if (PWideChar(ColorScheme) = 'Metallic') then
              Result := xpGray
            else
              Result := xpNone;
          end;
        end;
    end;
  finally
    if hThemeLib <> 0 then
      FreeLibrary(hThemeLib);
  end;
end;

function Darker(Color: TColor; Percent: Byte): TColor;
var
  R, g, b: Byte;
begin
  Color := ColorToRGB(Color);
  R := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  R := R - muldiv(R, Percent, 100); // Percent% closer to black
  g := g - muldiv(g, Percent, 100);
  b := b - muldiv(b, Percent, 100);
  result := RGB(R, g, b);
end;


//----------------------------------------------------------------- DrawGradient

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;
  DoPaint : Boolean;
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
      begin
        DoPaint := (( R.Top + stepw + Round(rstepw) + 1) <= R.Bottom);
        if DoPaint then
          Rectangle(R.Left, R.Top + stepw, R.Right, R.Top + stepw + Round(rstepw) + 1);
      end;
    end;
  end;
end;

// Draw gradient in the specified rectangle (if Fill = True and ColorFrom <> clNone),
// frame it with BorderColor color.
procedure DrawVistaGradient(ACanvas: TCanvas; ARect: TRect; ColorFrom, ColorTo, ColorMirrorFrom, ColorMirrorTo: TColor;
  Direction: TGradientDirection; BorderColor: TColor; Fill: Boolean = True);
const
	GradientSteps: Integer = 32;	//128
var
  r: Trect;

begin
  if Fill and (ColorFrom <> clNone) then
  begin
    if ColorMirrorFrom <> clNone then
    begin
      r := ARect;

      if Direction = gdVertical then
      begin
        r.Right := r.Left + ((r.Right - r.Left) div 2);
		DrawGradient(ACanvas,  ColorFrom, ColorTo, GradientSteps, r, Direction = gdVertical);
        r := ARect;
        r.Left := r.Left + ((r.Right - r.Left) div 2);
		DrawGradient(ACanvas,  ColorMirrorFrom, ColorMirrorTo, GradientSteps, r, Direction = gdVertical);
      end
      else
      begin
        r.Bottom := r.Top + ((r.Bottom - r.Top) div 2);
		DrawGradient(ACanvas,  ColorFrom, ColorTo, GradientSteps, r, Direction = gdVertical);
        r := ARect;
        r.Top := r.Top + ((r.Bottom - r.Top) div 2);
		DrawGradient(ACanvas,  ColorMirrorFrom, ColorMirrorTo, GradientSteps, r, Direction = gdVertical);
      end;
    end
    else
      DrawGradient(ACanvas, ColorFrom, ColorTo, GradientSteps, ARect, Direction = gdVertical);
  end;

  if BorderColor <> clNone then
  begin
    ACanvas.Brush.Color := BorderColor;
    ACanvas.FrameRect(ARect);
  end;
end;



//------------------------------------------------------------------------------
//---------------------------{ TAdvNavBarPanel }---------------------------------
//------------------------------------------------------------------------------

procedure TAdvNavBarPanel.Assign(Source: TPersistent);
begin
  if (Source is TAdvNavBarPanel) then
  begin
    Caption := (Source as TAdvNavBarPanel).Caption;
    CaptionAlignment := (Source as TAdvNavBarPanel).CaptionAlignment;
    CaptionHint := (Source as TAdvNavBarPanel).CaptionHint;
    CollapsedCaption := (Source as TAdvNavBarPanel).CollapsedCaption;
    Color := (Source as TAdvNavBarPanel).Color;
    ColorTo := (Source as TAdvNavBarPanel).ColorTo;
    GradientDirection := (Source as TAdvNavBarPanel).GradientDirection;
    Font.Assign((Source as TAdvNavBarPanel).Font);
    Hint := (Source as TAdvNavBarPanel).Hint;
    ImageIndex := (Source as TAdvNavBarPanel).ImageIndex;
    PanelIndex := (Source as TAdvNavBarPanel).PanelIndex;
    PopupMenu := (Source as TAdvNavBarPanel).PopupMenu;
    Sections.Assign((Source as TAdvNavBarPanel).Sections);
    SectionSizing := (Source as TAdvNavBarPanel).SectionSizing;
    ShowHint := (Source as TAdvNavBarPanel).ShowHint;
    TextColor := (Source as TAdvNavBarPanel).TextColor;
    Enabled := (Source as TAdvNavBarPanel).Enabled;
  end;
end;

procedure TAdvNavBarPanel.CMDesignHitTest(var Msg: TCMDesignHitTest);
var
  P: TPoint;
  i, h: Integer;
begin
  inherited;

  if (csDesigning in ComponentState) and FSizing then
  begin
    Msg.Result := 1;
    Exit;
  end;

  if (csDesigning in ComponentState) then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);

    h := 0;

    for i := 1 to Sections.Count do
    begin
      if (P.Y > h) and (P.Y < h + SectionHeight) then
      begin
        if (i > 1) then
        begin
          Msg.Result := 1;
          Exit;
        end;
      end;
      h := h + TAdvNavBarPanelSection(Sections.Items[i - 1]).Height;
    end;
  end;

  //inherited;
end;

constructor TAdvNavBarPanel.Create(AOwner: TComponent);
begin
  inherited;
  Align := alNone;
  BevelOuter := bvNone;
  FTextColor := clBlack;
  FImageIndex := -1;
  DoubleBuffered := true;
  Color := clWhite;
  ColorTo := clNone;
  FSections := TAdvNavBarPanelSections.Create(Self);
  FSizing := False;
  FTabVisible := True;
  Visible:=False;
end;


//------------------------------------------------------------------------------

destructor TAdvNavBarPanel.Destroy;
begin
  FSections.Free;
  inherited;
end;

function TAdvNavBarPanel.GetCaption: string;
begin
  Result := FCaption;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBarPanel.MouseMove(Shift: TShiftState; X, Y: integer);
var
  d: Integer;
begin
  inherited;

  if (csDesigning in ComponentState) or SectionSizing then
  begin
    if FSizing then
    begin
      d := Y - FSizeY;
      if FSizeHeight + d > SectionHeight then
        TAdvNavBarPanelSection(Sections.Items[FSizeSection - 1]).Height := FSizeHeight + d
      else if TAdvNavBarPanelSection(Sections.Items[FSizeSection - 1]).Height <> SectionHeight + 1 then
        TAdvNavBarPanelSection(Sections.Items[FSizeSection - 1]).Height := SectionHeight + 1;
      UpdateControlBounds;
    end;
  end;
end;

procedure TAdvNavBarPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  h, i: Integer;
begin
  inherited;

  h := 0;
  if ((csDesigning in ComponentState) or SectionSizing) and not FSizing then
  begin
    for i := 1 to Sections.Count do
    begin
      if (Y > h) and (Y < h + SectionHeight) then
      begin
        if (i > 1) then
        begin
          FSizeSection := i - 1;
          FSizeHeight := TAdvNavBarPanelSection(Sections.Items[FSizeSection - 1]).Height;
          FSizing := true;
          FSizeY := Y;
        end;
        Exit;
      end;
      h := h + TAdvNavBarPanelSection(Sections.Items[i - 1]).Height;
    end;
  end;

end;

procedure TAdvNavBarPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FSizing := false;
end;

procedure TAdvNavBarPanel.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  i: Integer;
begin
  inherited;

  if (AOperation = opRemove) and not (csDestroying in ComponentState) then
  begin
    for i := 0 to Sections.Count - 1 do
    begin
      if AComponent = Sections[i].FControl then
      begin
        Sections[i].FControl := nil;
        Invalidate;
        Break;
      end;
    end;
  end;

end;

procedure TAdvNavBarPanel.Paint;
var
  i, h: Integer;
  r: TRect;
  DTStyle: DWORD;
begin
  if Assigned(FAdvNavBar) and FAdvNavBar.FCollapsed then
    Exit;


  if ColorTo = clNone then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Color;
    Canvas.Rectangle(ClientRect.Left, ClientRect.Top, ClientRect.Right, ClientRect.Bottom);
  end
  else
  begin
    DrawGradient(Canvas, Color, ColorTo, 128, ClientRect, GradientDirection = gdVertical);
  end;

  h := 0;
  for i := 1 to Sections.Count do
  begin
    DrawGradient(Canvas, AdvNavBar.SectionColor, AdvNavBar.SectionColorTo, 16, Rect(0, h, width, h + SectionHeight), False);

    Canvas.Pen.Color := AdvNavBar.BorderColor;
    Canvas.MoveTo(0, h + SectionHeight);
    Canvas.LineTo(Width, h + SectionHeight);

    Canvas.Pen.Color := AdvNavBar.SectionColorTo;
    Canvas.MoveTo(0, h);
    Canvas.LineTo(Width, h);

    Canvas.Brush.Style := bsClear;

    Canvas.Font.Assign(AdvNavBar.SectionFont);

    r := Rect(4, h + 2, Width, Height);

    case CaptionAlignment of
      taRightJustify: DTStyle := DT_SINGLELINE or DT_RIGHT;
      taCenter: DTStyle := DT_SINGLELINE or DT_CENTER;
    else
       DTStyle := DT_SINGLELINE or DT_LEFT;
    end;

    DrawText(Canvas.Handle, pchar(TAdvNavBarPanelSection(Sections.Items[i - 1]).Caption),
      Length(TAdvNavBarPanelSection(Sections.Items[i - 1]).Caption), r, DTStyle);

    //Canvas.TextOut(4, h + 2, TAdvNavBarPanelSection(Sections.Items[i - 1]).Caption);

    h := h + TAdvNavBarPanelSection(Sections.Items[i - 1]).Height;


  end;

  if Assigned(nil) then begin //Panel after draw.
     //FOnPanelAfterDraw(Self,Canvas);
  end;
end;

procedure TAdvNavBarPanel.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TAdvNavBar then
    AdvNavBar := TAdvNavBar(Reader.Parent);
end;

//------------------------------------------------------------------------------

procedure TAdvNavBarPanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if (ALeft <> Left) or (ATop <> Top) or
    (AWidth <> Width) or (AHeight <> Height) then
  begin
    if FAdvNavBar <> nil then
    begin
      SetPosInAdvNavBar;
    end
    else
      inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBarPanel.SetCaption(const Value: string);
var
  i: Integer;
begin
  FCaption := Value;

  if FAdvNavBar <> nil then
    FAdvNavBar.Invalidate;

  // do not show the default panel caption
  inherited Caption := '';

  // workaround for panel design time painting bug
  if (csDesigning in ComponentState) then
  begin
    for i := 1 to ControlCount do
    begin
      Controls[i - 1].Width := Controls[i - 1].Width - 1;
      Controls[i - 1].Width := Controls[i - 1].Width + 1;
      Controls[i - 1].Invalidate;
    end;
  end;
end;

procedure TAdvNavBarPanel.SetCaptionAlignment(const Value: TAlignment);
begin
  FCaptionAlignment := Value;
  Invalidate;
end;

procedure TAdvNavBarPanel.SetCollapsedCaption(const Value:string);
begin
  FCollapsedCaption := Value;
  if Assigned(FAdvNavBar) and FAdvNavBar.FCollapsed then
    SetCaption(Caption); //Trigger above redraw items, if applicable
end;

function TAdvNavBarPanel.GetAdjustedCaption:String;
begin
  Result := FCaption;
  if Assigned(FAdvNavBar) and FAdvNavBar.Collapsed then
  begin
    Result := FCollapsedCaption;
  end;
end;


//------------------------------------------------------------------------------

procedure TAdvNavBarPanel.SetColorTo(const Value: TColor);
begin
  if (FColorTo <> Value) then
  begin
    FColorTo := Value;
    Invalidate;
  end;
end;

procedure TAdvNavBarPanel.SetAdvNavBar(Value: TAdvNavBar);
begin
  if Value <> FAdvNavBar then
  begin
    if FAdvNavBar <> nil then
      FAdvNavBar.RemoveAdvNavBarPanel(Self);
    FAdvNavBar := Value;
    Parent := FAdvNavBar;
    if FAdvNavBar <> nil then
    begin
      Value.InsertAdvNavBarPanel(Self);
      if FTextColor = clBlack then
        FTextColor := FAdvNavBar.DefaultTextColor;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBarPanel.SetFontStyles(const Value: TFontStyles);
begin
  FFontStyles := Value;
  if Assigned(FAdvNavBar) then
    FAdvNavBar.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBarPanel.SetFPanelIndex(const Value: integer);
var
  i: Integer;
begin
  if (csLoading in ComponentState) then
    FPanelIndex := Value
  else
    if (csDesigning in ComponentState) then
    begin
      FAdvNavBar.MovePanel(FPanelIndex, Value);
      FPanelIndex := Value;

      for i := 1 to FAdvNavBar.FAdvNavBarPanel.Count do
      begin
        TAdvNavBarPanel(FAdvNavBar.FAdvNavBarPanel[i - 1]).FPanelIndex := i - 1;
      end;
    end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBarPanel.SetGradientDirection(
  const Value: TGradientDirection);
begin
  if (FGradientDirection <> Value) then
  begin
    FGradientDirection := Value;
    Invalidate;
  end;
end;

procedure TAdvNavBarPanel.SetImageIndex(const Value: TImageIndex);
begin
  FImageIndex := Value;
  if FAdvNavBar <> nil then
    FAdvNavBar.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBarPanel.SetPosInAdvNavBar;
var
  R: TRect;
begin
  if FAdvNavBar = nil then
    raise Exception.Create('No AdvNavBar is assigned.');

  R := FAdvNavBar.GetPanelRect;

  inherited SetBounds(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
end;

//------------------------------------------------------------------------------

procedure TAdvNavBarPanel.SetTextColor(const Value: TColor);
begin
  FTextColor := Value;
  if FAdvNavBar <> nil then
    FAdvNavBar.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBarPanel.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBarPanel.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
  i, h: Integer;
begin
  if ((csDesigning in ComponentState) or SectionSizing) and FSizing then
    Exit;

  inherited;

  if ((csDesigning in ComponentState) or SectionSizing) and (Sections.Count > 0) then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);

    h := 0;
    Windows.SetCursor(Screen.Cursors[Cursor]);

    for i := 1 to Sections.Count do
    begin
      if (P.Y > h) and (P.Y < h + SectionHeight) then
      begin
        if (i > 1) then
          Windows.SetCursor(Screen.Cursors[crVSplit]);
        Break;
      end;
      h := h + TAdvNavBarPanelSection(Sections.Items[i - 1]).Height;
    end;
  end;

end;

procedure TAdvNavBarPanel.UpdateControlBounds;
var
  i:Integer;
begin
  for i:=0 to Sections.Count-1 do
  begin
     Sections[i].UpdateControlBounds;
  end;
end;

procedure TAdvNavBarPanel.WMSize(var Message: TWMSize);
begin
  if FAdvNavBar <> nil then
  begin
    //FAdvNavBar.SetAdvNavBarPanelPosition(Self);
    SetPosInAdvNavBar;
    inherited;
    UpdateControlBounds;
  end;
end;

//------------------------------------------------------------------------------

function TAdvNavBarPanel.GetEnable: Boolean;
begin
  Result := inherited Enabled;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBarPanel.SetEnable(const Value: Boolean);
var
  i: integer;
begin
  inherited Enabled := Value;
  for i := 0 to ControlCount - 1 do
    Controls[i].Enabled := Value;
end;

//------------------------------------------------------------------------------

function TAdvNavBarPanel.GetTabVisible: Boolean;
begin
  Result := FTabVisible;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBarPanel.SetTabVisible(const Value: Boolean);
begin
  if Assigned(FAdvNavBar) then Visible:=(Value and not FAdvNavbar.FCollapsed); //PHYSICAL visibility

  FTabVisible := Value;
  if FAdvNavBar <> nil then
    FAdvNavBar.UpdateList;
end;

//------------------------------------------------------------------------------
//---------------------------{ TExchangeScroller }------------------------------
//------------------------------------------------------------------------------

function TExchangeScroller.CanGoBack: Boolean;
begin
  Result := Position > Min;
end;

//------------------------------------------------------------------------------

function TExchangeScroller.CanGoForward: Boolean;
begin
  Result := Position < Max;
end;

//------------------------------------------------------------------------------

constructor TExchangeScroller.Create;
begin
  inherited;
  FMin := 0;
  FMax := 0;
  FPosition := 0;
  FVisible := False;
end;

//------------------------------------------------------------------------------

procedure TExchangeScroller.SetMax(const Value: integer);
begin
  if Value >= FMin then FMax := Value;
end;

//------------------------------------------------------------------------------

procedure TExchangeScroller.SetMin(const Value: integer);
begin
  if Value <= FMax then FMin := Value;
end;

//------------------------------------------------------------------------------

procedure TExchangeScroller.SetPosition(const Value: integer);
begin
  FPosition := Value;
end;

//------------------------------------------------------------------------------

procedure TExchangeScroller.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

//------------------------------------------------------------------------------
//----------------------------{ TAdvNavBar }----------------------------------
//------------------------------------------------------------------------------

constructor TAdvNavBar.Create(Aowner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csAcceptsControls];

  DoubleBuffered := True;
  FBorderStyle := bsSingle;

  Color := clWhite;
  FReverseOrder := true;
  FAdvNavBarPanel := TList.Create; //Specifically moved up above WIDTH for new collapsing
  FDupAdvNavBarPanel := TList.Create;
  FTempAdvNavBarPanel := TList.Create;

  FUpdateCount := 0;
  Height := 300;
  Width := 250;
  FBorderColor := clGray;
  FSeparatorLine := 0; //Height - (SplitterHeight div 2) - 1;
  FSplitterPosition := 0;
  FTempSplitterPos := 0;
  FCaptionTabHeight := 36;
  FCaptionColor := clGray;
  FCaptionColorTo := clNone;
  FDefaultTextColor := clBlack;
  FDefaultTabColor := clWhite;
  FDefaultTabColorTo := clBtnFace;
  FCaptionVisible := True;

  FActiveColor := clSilver;
  FActiveColorTo := clSilver;
  FActiveMirrorColor := clNone;
  FActiveMirrorColorTo := clNone;
  FActiveTextColor := clWindowText;

  FHoverTabColor := $00BBA9A2; //$00D1B1A5;
  FHoverTabColorTo := $00BBA9A2; //$00D1B1A5;
  FHoverTabMirrorColor := clNone;
  FHoverTabMirrorColorTo := clNone;

  FDownTabColor := $00A78F87;
  FDownTabColorTo := $00A78F87;
  FDownTabMirrorColor := clNone;
  FDownTabMirrorColorTo := clNone;

  FDefaultTabMirrorColor := clNone;
  FDefaultTabMirrorColorTo := clNone;

  FCollapsedHoverColor := clInfoBk;
  FCollapsedHoverColorTo := clNone;
  FCollapsedDownColor := clSilver;
  FCollapsedDownColorTo := clNone;

  FDownTextColor := clBlack;
  FHoverTextColor := clBlack;
  FShowShortCutBar := True;

  FInternalCall := false;
  FDefaultTabPosition := TpBottom;

  FActivePanel := nil;
  FActiveTabIndex := -1;
  FHoverTabIndex := -1;
  FDownTabIndex := -1;

  FPopupIndicator := True;
  FShowSplitter := True;
  FDoEraseBkg := False;

  FSplitterColor := clSilver;
  FSplitterColorTo := clGray;
  FSplitterBorderColor := clNone;
  FMouseCaptured := false;
  FCheckForSplitterMove := false;
  FSplitterInternalCall := false;

  FDisplayCaptionTabsCount := 0;
  FCaptionAlignment := taLeftJustify;

  FFixedBtnMargin := 1;
  FBottomIconAlign := biRight;
  FPropertiesLoaded := false;

  FScroller := TExchangeScroller.Create;
  FScrollerHoverLeftBtn := false;
  FScrollerDownLeftBtn := false;
  FScrollerHoverRightBtn := false;
  FScrollerDownRightBtn := false;

  FOriginalHint := '';
  ShowHint := true;

  FMinClientHeight := 60;
  FPropertiesCreated := true;
  FCaptionFont := TFont.Create;
  FCaptionFont.Size := 11;
  FCaptionFont.Color := clWhite;
  FCaptionFont.Name := 'Tahoma';
  FCaptionFont.Style := [fsBold];
  FCaptionFont.OnChange := FontChanged;

  FSectionFont := TFont.Create;
  FSectionFont.Assign(Font);
  FSectionFont.OnChange := FontChanged;

  FOldSplitterPosForSizeChange := -1;

  Style := esCustom;
  Style := esOffice2003Blue;

  FCollapsedCaption := 'Collapsed';
  FCollapsed := False;
  FCollapsedWidth := DefCollapsedWidth;
  FPreCollapsedWidth := Width;
  FIsCollapsing := True; //to inhibit collapsing until after loadup.
  FHoverCollapsedCaption := False;
  FDownCollapsedCaption := False;
end;

//------------------------------------------------------------------------------

destructor TAdvNavBar.Destroy;
var
  i: integer;
begin
  {for i := 0 to FAdvNavBarPanel.Count - 1 do
    TAdvNavBarPanel(FAdvNavBarPanel[i]).Free;
  }
  for i := 0 to FDupAdvNavBarPanel.Count - 1 do
    TAdvNavBarPanel(FDupAdvNavBarPanel[i]).Free;
  FAdvNavBarPanel.Free;
  FDupAdvNavBarPanel.Free;
  FTempAdvNavBarPanel.Free;
  FCaptionFont.Free;
  FSectionFont.Free;
  FScroller.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.InsertAdvNavBarPanel(Value: TAdvNavBarPanel);
var
  i: integer;
begin
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    i := FAdvNavBarPanel.Add(Value);
    Value.FAdvNavBar := Self;
    MoveSplitterInTabs(-1);
    //FDisplayCaptionTabsCount:= FDisplayCaptionTabsCount + 1;
    SetAdvNavBarPanelPosition(Value);
    FInternalCall := true;
    ActiveTabIndex := i;
    FInternalCall := false;

    i := FDupAdvNavBarPanel.Add(Value);
    Value.FPanelIndex := i;
  end
  else if not FPropertiesLoaded then
  begin
    FTempAdvNavBarPanel.Add(Value);
  end;
end;

//------------------------------------------------------------------------------
function TAdvNavBar.CreatePanel: TAdvNavBarPanel;
begin
  Result := TAdvNavBarPanel.Create(Self);
end;


//------------------------------------------------------------------------------

function TAdvNavBar.AddPanel: TAdvNavBarPanel;
begin
  Result := CreatePanel;
  Result.AdvNavBar := Self;

  FAdvNavBarPanel.Add(Result);
  FDupAdvNavBarPanel.Add(Result);
  SetAdvNavBarPanelPosition(Result);
  FInternalCall := true;
  ActiveTabIndex := FAdvNavBarPanel.Count - 1;
  FInternalCall := false;
  Result.FPanelIndex := FAdvNavBarPanel.Count - 1;
end;

//------------------------------------------------------------------------------

function TAdvNavBar.InsertPanel(Index: Integer): TAdvNavBarPanel;
begin
  Result := CreatePanel;
  Result.AdvNavBar := Self;
  FAdvNavBarPanel.Insert(Index, Result);
  FDupAdvNavBarPanel.Insert(Index, Result);
  SetAdvNavBarPanelPosition(Result);
  FInternalCall := true;
  ActiveTabIndex := FAdvNavBarPanel.Count - 1;
  FInternalCall := false;
end;

function TAdvNavBar.IsOffice2010: Boolean;
begin
  Result := (Style = esOffice2010Blue) or (Style = esOffice2010Silver) or (Style = esOffice2010Black);
end;

//------------------------------------------------------------------------------

function TAdvNavBar.GetPanel(Index: Integer): TAdvNavBarPanel;
begin
  Result := nil;
  {if (FAdvNavBarPanel.Count > Index) and (Index >= 0) then
    Result := TAdvNavBarPanel(FAdvNavBarPanel.Items[Index]);}
  if (FDupAdvNavBarPanel.Count > Index) and (Index >= 0) then
    Result := TAdvNavBarPanel(FDupAdvNavBarPanel.Items[Index]);
end;

//------------------------------------------------------------------------------

function TAdvNavBar.GetPanelCount: Integer;
begin
  Result := FDupAdvNavBarPanel.Count; //FAdvNavBarPanel.Count;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.MovePanel(FromIndex, ToIndex: Integer);
begin
  FDupAdvNavBarPanel.Move(FromIndex, ToIndex);
  FAdvNavBarPanel.Move(FromIndex, ToIndex);
  Invalidate;
  ActiveTabIndex := ActiveTabIndex;

  if ActiveTabIndex < FAdvNavBarPanel.Count then
    ActivePanel := TAdvNavBarPanel(FAdvNavBarPanel[ActiveTabIndex]);
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.RemovePanel(Index: Integer);
var
  atp: TAdvNavBarPanel;
begin
  atp := GetPanel(Index);
  RemoveAdvNavBarPanel(atp);
  if Assigned(atp) then
    atp.Free;
end;

procedure TAdvNavBar.Resize;
begin
  inherited;
  SetAllAdvNavBarPanelPosition;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.Paint;
var
  th: Integer;
begin
  if FUpdateCount > 0 then
    Exit;

  //---- Rectangle around the component
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Bounds(0, 0, Width, Height));
  Canvas.Brush.Style := bsClear;

  if (FBorderColor <> clNone) and (BorderStyle <> bsNone) then
  begin
    Canvas.Pen.Color := FBorderColor;
    Canvas.Pen.Width := 1;
    Canvas.Rectangle(Bounds(0, 0, Width, Height));
  end;

  {$IFNDEF NODESIGNHINT}
  if FAdvNavBarPanel.Count = 0 then
    if (csDesigning in ComponentState) then
    begin
      th := Canvas.TextHeight('gh');
      Canvas.Font.Assign(self.Font);
      Canvas.TextOut(10, Height div 2, 'Right-click and choose "Add Panel"');
      Canvas.TextOut(10, (Height div 2) + th,'to insert a new panel');
      Canvas.Font.Style := [fsItalic];
      Canvas.TextOut(10, Height div 2 + 3*th, 'If no such right-click menu option appears');
      Canvas.TextOut(10, Height div 2 + 4*th, 'please install the designtime package!');
    end;
  {$ENDIF}
  //---- Caption Rectangle
  if CaptionVisible then
    DrawCaption;

  if AllowCollaps then
    DrawCollapsedCaption;

  //---- Draw Splitter
  if ShowSplitter then
    DrawSplitter;

  //---- Draw Caption Tabs
  DrawAllCaptionTabs; //

  //---- Draw Fixed Tab
  if ShowShortCutBar then
    DrawFixedTab;

  //if Assigned(nil) then
  //begin
  //FOnAfterDraw(Self,Canvas);
  //end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetAdvNavBarPanelPosition(
  aAdvNavBarPanel: TAdvNavBarPanel);
begin
  aAdvNavBarPanel.SetPosInAdvNavBar;
{
  aAdvNavBarPanel.Align:= alNone;
  aAdvNavBarPanel.Top:= CaptionHeight+1;
  aAdvNavBarPanel.Left:= 1;
  aAdvNavBarPanel.Width:= Width-2;
  aAdvNavBarPanel.Height:= FSeparatorLine-(SpliterHeight div 2)-CaptionHeight-1;
}
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.ShowAdvNavBarPanel(PanelIndex: integer);
begin
  if (PanelIndex < 0) or (PanelIndex >= FAdvnavBarPanel.Count) then
    Exit;

  TAdvNavBarPanel(FAdvNavBarPanel[PanelIndex]).TabVisible := True;
  TAdvNavBarPanel(FAdvNavBarPanel[PanelIndex]).BringToFront;
end;

procedure TAdvNavBar.WMEraseBkGnd(var Message: TMessage);
begin
  if FDOEraseBkg then
    inherited
  else
    message.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.WMSetCursor(var Msg: TWMSetCursor);
var
  i: Integer;
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);

  i := IndexOfTabAt(P.X,P.Y);
  if i < 0 then
    Windows.SetCursor(Screen.Cursors[Cursor])
  else
    Windows.SetCursor(Screen.Cursors[crHandPoint]);
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetAllAdvNavBarPanelPosition;
var
  i: integer;

begin
  if FDefaultTabPosition = tpBottom then
  begin
    if FDisplayCaptionTabsCount <= 0 then
      FSeparatorLine := Height - (SplitterHeight div 2) - 1 - GetFixedCaptionTabHeight {FixedTabHeight}
    else
      FSeparatorLine := CaptionTabRect(CaptionTabMaxIndex).Top - (SplitterHeight div 2); //Height - (FDisplayCaptionTabsCount*(FCaptionTabHeight))- (SplitterHeight div 2);
  end
  else
  begin
    if FDisplayCaptionTabsCount <= 0 then
      FSeparatorLine := (SplitterHeight div 2) + 1 + GetFixedCaptionTabHeight {FixedTabHeight}
    else
      FSeparatorLine := CaptionTabRect(CaptionTabMaxIndex).Bottom + (SplitterHeight div 2); //Height - (FDisplayCaptionTabsCount*(FCaptionTabHeight))- (SplitterHeight div 2);
  end;

  for i := 0 to FAdvNavBarPanel.Count - 1 do
    SetAdvNavBarPanelPosition(TAdvNavBarPanel(FAdvNavBarPanel[i]));
end;

procedure TAdvNavBar.SetAllowCollaps(const Value: Boolean);
begin
  if Value <> FAllowCollaps then
  begin
    FAllowCollaps := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetCaptionTabHeight(Value: integer);
begin
  if Value <> FCaptionTabHeight then
  begin
    FCaptionTabHeight := Value;
    Invalidate;
  end;
end;

procedure TAdvNavBar.SetCaptionVisible(const Value: Boolean);
begin
  if FCaptionVisible <> Value then
  begin
    FCaptionVisible := Value;
    Width := Width + 1;
    Width := Width - 1;
  end;
end;

//------------------------------------------------------------------------------

function TAdvNavBar.CaptionTabMaxIndex: integer;
begin
  if FReverseOrder then
    result := FAdvNavBarPanel.Count - 1
  else
    result := 0;
end;

function TAdvNavBar.CaptionTabRect(index: integer): TRect;
var
  HideTabs: integer;
begin
  Result := Rect(0, 0, 0, 0);

  if FReverseOrder then
  begin
  HideTabs := Abs(FAdvNavBarPanel.Count - FDisplayCaptionTabsCount);
  if Index >= HideTabs then
  begin
    Index := abs((FAdvNavBarPanel.Count - FDisplayCaptionTabsCount) - index);
    if FDefaultTabPosition = tpBottom then
      Result := Rect(0, Height - ((index + 1  {FixedTab}) * GetCaptionTabHeight + GetFixedCaptionTabHeight) - 1, Width, (Height - ((index + 1  {FixedTab}) * GetCaptionTabHeight + GetFixedCaptionTabHeight)) + GetCaptionTabHeight)
    else
      Result := Rect(0, ((index  {FixedTab}) * GetCaptionTabHeight + GetFixedCaptionTabHeight), Width, (((index  {FixedTab}) * GetCaptionTabHeight +  + GetFixedCaptionTabHeight)) + GetCaptionTabHeight + 1);
  end;
  end else
  begin
    HideTabs := FDisplayCaptionTabsCount;
    if Index < HideTabs then
    begin
      Index := FDisplayCaptionTabsCount - index - 1;
      if FDefaultTabPosition = tpBottom then
        Result := Rect(0, Height - ((index + 1  {FixedTab}) * GetCaptionTabHeight + GetFixedCaptionTabHeight) - 1, Width, (Height - ((index + 1 {FixedTab}) * GetCaptionTabHeight + GetFixedCaptionTabHeight)) + GetCaptionTabHeight)
      else
        Result := Rect(0, ((index {FixedTab}) * GetCaptionTabHeight + GetFixedCaptionTabHeight), Width, (((index {FixedTab}) * GetCaptionTabHeight + GetFixedCaptionTabHeight)) + GetCaptionTabHeight + 1);
    end;
  end;
end;

procedure TAdvNavBar.Click;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.DrawCaptionTab(Index: integer);
var
  TabR, R2: TRect;
  HorizontalGradient: Boolean;
  clr, clrTo, ClrText: TColor;
  clrMirror, clrMirrorTo: TColor;
  tp,tl,d: integer;
  //tbmp: TBitMap;
  R, DR: TRect;
  DRWSTYLE: DWORD;
  g: TGPGraphics;
  lb: TGPLinearGradientBrush;
  sb: TGPSolidBrush;
  gb: TGPPathGradientBrush;
  p: TGPPen;
  pth: TGPGraphicsPath;
  colors : array[0..0] of TGPColor;
  rgn: TGPRegion;
  rt, rtinner, rtshadow, er,
  rtinnertop, rtinnerbottom: TGPRectF;
  cCount: integer;
  isVis: boolean;

begin
  TabR := CaptionTabRect(Index);

  isVis := (TabR.Top <> 0) and (TabR.Bottom <> 0);

  if not IsOffice2010 then
  begin
    Canvas.Pen.Color := FBorderColor;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(TabR.Left, TabR.Top, TabR.Right, TabR.Bottom);
  end
  else
  begin
    if IsOffice2010 then
      InflateRect(TabR, -8, -3);
  end;

  R := GetCaptionRect;

  ClrText := TAdvNavBarPanel(FAdvNavBarPanel[Index]).TextColor;

  DRWSTYLE := DT_LEFT or DT_SINGLELINE or DT_END_ELLIPSIS;

  case CaptionAlignment of
  taRightJustify: DRWSTYLE := DT_RIGHT or DT_SINGLELINE or DT_END_ELLIPSIS;
  taCenter: DRWSTYLE := DT_CENTER or DT_SINGLELINE or DT_END_ELLIPSIS;
  end;

  if (Index = FActiveTabIndex) and CaptionVisible then
  begin
    Canvas.Font.Assign(CaptionFont);

    DR := Rect(8, r.Top + (GetCaptionHeight - Canvas.TextHeight('gh')) div 2, Width, Height);

    if AllowCollaps then
      DR.Right := DR.Right - 24;

    DR.Right := DR.Right - 4;

    if not FCollapsed then
    begin
      DrawText(Canvas.Handle, PChar(TAdvNavBarPanel(FAdvNavBarPanel[Index]).AdjustedCaption),
               Length(TAdvNavBarPanel(FAdvNavBarPanel[Index]).AdjustedCaption), DR, DRWSTYLE);
    end;

    //Canvas.TextOut(8, R.Top + (CaptionHeight - Canvas.TextHeight('gh')) div 2, TAdvNavBarPanel(FAdvNavBarPanel[Index]).Caption);

    if (Index = FHoverTabIndex) and (Index = FDownTabIndex) then
    begin
      clr := FDownTabColor;
      clrTo := FDownTabColorTo;
      clrText := FDownTextColor;
      clrMirror := FDownTabMirrorColor;
      clrMirrorTo := FDownTabMirrorColorTo;
    end
    else
    begin
      clr := ActiveColor;
      clrTo := ActiveColorTo;
      clrMirror := ActiveMirrorColor;
      clrText := ActiveTextColor;
      clrMirrorTo := ActiveMirrorColorTo;
    end;

    Canvas.Pen.Color := BorderColor;
    Canvas.Pen.Width := 1;
    Canvas.MoveTo(0, {CaptionHeight} R.Bottom - 1);
    Canvas.LineTo(Width, {CaptionHeight} R.Bottom - 1);

  end
  else
  if Index = FHoverTabIndex then
  begin
    if Index = FDownTabIndex then
    begin
      clr := FDownTabColor;
      clrTo := FDownTabColorTo;
      ClrText := FDownTextColor;
      clrMirror := FDownTabMirrorColor;
      clrMirrorTo := FDownTabMirrorColorTo;
    end
    else
    begin
      clr := FHoverTabColor;
      clrTo := FHoverTabColorTo;
      ClrText:= FHoverTextColor;
      clrMirror := FHoverTabMirrorColor;
      clrMirrorTo := FHoverTabMirrorColorTo;
    end;
  end
  else
  begin
    clr := DefaultTabColor;
    clrto := DefaultTabColorTo;
    clrMirror := DefaultTabMirrorColor;
    clrMirrorTo := DefaultTabMirrorColorTo;
  end;

  if (TabR.Left = 0) and (TabR.Right = 0) then
    Exit;

  HorizontalGradient := FDefaultGradientDirection = gdHorizontal;
  R2 := Rect(TabR.Left + 1, TabR.Top + 1, TabR.Right - 1, TabR.Bottom );

  if not HorizontalGradient then
    R2.Bottom := R2.Bottom - 1;

  //DrawGradient(Canvas, clr, clrTo, 16, R2, HorizontalGradient);
  if (IsOffice2010 and ((Index = FDownTabIndex) or (Index = FHoverTabIndex) or (Index = FActiveTabIndex))) or (not IsOffice2010) then
  begin
    if IsOffice2010 then
    begin
      if Collapsed then
        rt := MakeRect(R2.Left - 4, R2.Top, CollapsedWidth - 10, r2.Bottom - r2.Top)
      else
        rt := MakeRect(R2.Left, R2.Top, R2.Right - R2.Left, r2.Bottom - r2.Top);

      g := TGPGraphics.Create(Canvas.Handle);
      g.SetSmoothingMode(SmoothingModeAntiAlias);

      if (index <> FDownTabIndex) then
      begin
        rtshadow := MakeRect(rt.X - 2, rt.Y, rt.Width + 4, rt.Height + 2);
        sb := TGPSolidBrush.Create(MakeColor(10, clBlack));
        g.FillRectangle(sb, rtshadow);
        sb.Free;
      end;

      lb := TGPLinearGradientBrush.Create(MakeRect(rt.X - 1, rt.Y - 1, rt.Width + 2, rt.Height + 2), MakeColor(255, clr), MakeColor(255, clrTo), LinearGradientModeVertical);
      g.FillRectangle(lb, rt);
      lb.Free;

      p := TGPPen.Create(MakeColor(255, BorderColor));
      g.DrawRectangle(p, rt);
      p.Free;

      if index <> FDownTabIndex then
        p := TGPPen.Create(MakeColor(100, clWhite))
      else
        p := TGPPen.Create(MakeColor(20, clBlack));
      rtinner := MakeRect(rt.X + 1, rt.Y + 1, rt.Width - 2, rt.Height - 2);
      g.DrawRectangle(p, rtinner);
      p.Free;

      rtinnertop := MakeRect(rtinner.X, rtinner.Y, rtinner.Width, rtinner.Height / 4);
      if index <> FDownTabIndex then
      begin
        lb := TGPLinearGradientBrush.Create(MakeRect(rtinnertop.X - 1, rtinnertop.Y - 1, rtinnertop.Width + 2, rtinnertop.Height + 2),
        MakeColor(50, clWhite), MakeColor(0, clWhite), LinearGradientModeVertical);
      end
      else
      begin
        lb := TGPLinearGradientBrush.Create(MakeRect(rtinnertop.X - 1, rtinnertop.Y - 1, rtinnertop.Width + 2, rtinnertop.Height + 2),
        MakeColor(20, clBlack), MakeColor(0, clBlack), LinearGradientModeVertical);
      end;

      g.FillRectangle(lb, rtinnertop);
      lb.Free;

      rtinnerbottom := MakeRect(rtinner.X, rtinner.Y + rtinner.Height - rtinner.Height / 4, rtinner.Width, rtinner.Height / 4);

      if index <> FDownTabIndex then
      begin
        lb := TGPLinearGradientBrush.Create(MakeRect(rtinnerbottom.X - 1, rtinnerbottom.Y - 1, rtinnerbottom.Width + 2, rtinnerbottom.Height + 2),
        MakeColor(0, clWhite), MakeColor(50, clWhite), LinearGradientModeVertical);
      end
      else
      begin
        lb := TGPLinearGradientBrush.Create(MakeRect(rtinnerbottom.X - 1, rtinnerbottom.Y - 1, rtinnerbottom.Width + 2, rtinnerbottom.Height + 2),
        MakeColor(0, clBlack), MakeColor(20, clBlack), LinearGradientModeVertical);
      end;

      g.FillRectangle(lb, rtinnerbottom);
      lb.Free;

      if index <> FDownTabIndex then
      begin
        rgn := TGPRegion.Create(rtinner);
        g.SetClip(rgn);
        pth := TGPGraphicsPath.Create;
        er := MakeRect(rtinner.X + rtinner.Width / 8, rtinner.Y + rtinner.Height / 4, rtinner.Width - rtinner.width / 4, rtinner.Height * 2);
        pth.AddEllipse(er);
        gb := TGPPathGradientBrush.Create(pth);
        gb.SetCenterPoint(MakePoint(er.X + (er.Width / 2), er.Y + er.Height / 2));
        gb.SetCenterColor(MakeColor(100, clWhite));
        colors[0] := MakeColor(0, clWhite);
        cCount := 1;
        gb.SetSurroundColors(@colors, cCount);
        g.FillPath(gb, pth);
        pth.Free;
        gb.Free;

        g.ResetClip;
        rgn.Free;
      end;

      g.Free;
    end
    else
    begin
      DrawVistaGradient(Canvas, R2, clr, clrTo, clrMirror, clrMirrorTo, gdHorizontal, clNone);
    end;
  end;

  Canvas.Font.Assign(Font);
  Canvas.Font.Style := TAdvNavBarPanel(FAdvNavBarPanel[Index]).CaptionStyle;
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Color := clrText; //TAdvNavBarPanel(FAdvNavBarPanel[Index]).TextColor;

  if Assigned(FImages) and isVis then
  begin
    tp := (TabR.Bottom - TabR.Top - FImages.Height) div 2;

    if Collapsed then
    begin
      d := TabR.Right - TabR.Left - FImages.Width;
      if d > 0 then
        d := d div 2
      else
        d := 0;
      FImages.Draw(Canvas, TabR.left + d, TabR.Top + tp, TAdvNavBarPanel(FAdvNavBarPanel[Index]).ImageIndex);
    end
    else
    case CaptionAlignment of
    taLeftJustify:
      begin
        FImages.Draw(Canvas, TabR.left + 5, TabR.Top + tp, TAdvNavBarPanel(FAdvNavBarPanel[Index]).ImageIndex);
        R2 := Rect(TabR.Left + 5 + Fimages.Width, TabR.Top, TabR.Right, TabR.Bottom);
      end;
    taRightJustify:
      begin
        FImages.Draw(Canvas, TabR.Right - FImages.Width - 4, TabR.Top + tp, TAdvNavBarPanel(FAdvNavBarPanel[Index]).ImageIndex);
        R2 := Rect(TabR.Left, TabR.Top, TabR.Right - Fimages.Width - 8, TabR.Bottom);
      end;
    taCenter:
      begin
        tl := Canvas.TextWidth(TAdvNavBarPanel(FAdvNavBarPanel[Index]).AdjustedCaption);
        tl := (TabR.Right - TabR.Left - tl) div 2;
        tl := tl - FImages.Width - 4;

        FImages.Draw(Canvas, tl, TabR.Top + tp, TAdvNavBarPanel(FAdvNavBarPanel[Index]).ImageIndex);
        R2 := Rect(TabR.Left, TabR.Top, TabR.Right, TabR.Bottom);
      end;
    end;
  end;

  tp := (TabR.Bottom - TabR.Top - Canvas.TextHeight('gh')) div 2;

  DR := Rect(R2.Left + 5, R2.Top + tp, R2.Right, Height);

  DrawText(Canvas.Handle,PChar(TAdvNavBarPanel(FAdvNavBarPanel[Index]).AdjustedCaption),
           Length(TAdvNavBarPanel(FAdvNavBarPanel[Index]).AdjustedCaption), DR, DRWSTYLE);

  //Canvas.TextOut(R2.Left + 5, R2.Top + tp, TAdvNavBarPanel(FAdvNavBarPanel[Index]).Caption);
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.DrawAllCaptionTabs;
var
  i: integer;
begin
  if FReverseOrder then
  for i := (FAdvNavBarPanel.Count - FDisplayCaptionTabsCount) to FAdvNavBarPanel.Count - 1 do
  begin
    if i <> FActiveTabIndex then
      DrawCaptionTab(i)
  end
  else
    for i := 0 to Max(FAdvNavBarPanel.Count, FDisplayCaptionTabsCount) - 1 do
    begin
      if i <> FActiveTabIndex then
        DrawCaptionTab(i);
    end;

  if (FActiveTabIndex >= 0) {and (FDisplayCaptionTabsCount > 0)} then
    DrawCaptionTab(FActiveTabIndex);
end;

//------------------------------------------------------------------------------
procedure TAdvNavBar.DrawCollapsedCaption;
var
  tf: TFont;
  lf: TLogFont;
  oldfont: TFont;
  VWidth,VHeight:Integer;
  R: TRect;
  HRGN: THandle;
  g: TGPGraphics;
  lb: TGPLinearGradientBrush;
  sb: TGPSolidBrush;
  gb: TGPPathGradientBrush;
  p: TGPPen;
  rt, rtinner, rtshadow, er,
  rtinnertop, rtinnerbottom: TGPRectF;
  pth: TGPGraphicsPath;
  colors : array[0..0] of TGPColor;
  cCount: integer;
  rgn: TGPRegion;
  clr, clrto: TColor;
begin
  if not FCollapsed then
    Exit;
  //---- Rectangle around the component

  if not IsOffice2010 then
  begin
    Canvas.Pen.Color := BorderColor;
    Canvas.Pen.Width := 1;
  end;

  R := ClientRect;
  R.Top  := GetCaptionRect.Bottom;

  if IsOffice2010 then
    InflateRect(R, -4, -4);

  if FHoverCollapsedCaption then
  begin
    if FDownCollapsedCaption then
    begin
      clr := CollapsedDownColor;
      clrto := CollapsedDownColorTo;
    end
    else
    begin
      clr := CollapsedHoverColor;
      clrto := CollapsedHoverColorTo;
    end;

    if not IsOffice2010 then
    begin
      if clrto <> clNone then
      begin
        R.Right := R.Right - 1;
        DrawVistaGradient(Canvas, R, clr, clrto, clNone, clNone, gdVertical, clNone);
      end
      else
      begin
        InflateRect(R, +1, +1);
        Canvas.Brush.Color := clr;
        Canvas.Rectangle(R);
        InflateRect(R, -1, -1);
      end;
    end
    else
    begin
      rt := MakeRect(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
      g := TGPGraphics.Create(Canvas.Handle);
      g.SetSmoothingMode(SmoothingModeAntiAlias);

      if not FDownCollapsedCaption then
      begin
        rtshadow := MakeRect(rt.X - 2, rt.Y - 2, rt.Width + 4, rt.Height + 4);
        sb := TGPSolidBrush.Create(MakeColor(10, clBlack));
        g.FillRectangle(sb, rtshadow);
        sb.Free;
      end;

      lb := TGPLinearGradientBrush.Create(MakeRect(rt.X - 1, rt.Y - 1, rt.Width + 2, rt.Height + 2), MakeColor(255, clr), MakeColor(255, clrTo), LinearGradientModeHorizontal);
      g.FillRectangle(lb, rt);
      lb.Free;

      p := TGPPen.Create(MakeColor(255, BorderColor));
      g.DrawRectangle(p, rt);
      p.Free;

      if not FDownCollapsedCaption then
        p := TGPPen.Create(MakeColor(100, clWhite))
      else
        p := TGPPen.Create(MakeColor(20, clBlack));
      rtinner := MakeRect(rt.X + 1, rt.Y + 1, rt.Width - 2, rt.Height - 2);
      g.DrawRectangle(p, rtinner);
      p.Free;

      rtinnertop := MakeRect(rtinner.X, rtinner.Y, rtinner.Width / 4, rtinner.Height);
      if not FDownCollapsedCaption then
      begin
        lb := TGPLinearGradientBrush.Create(MakeRect(rtinnertop.X - 1, rtinnertop.Y - 1, rtinnertop.Width + 2, rtinnertop.Height + 2),
        MakeColor(50, clWhite), MakeColor(0, clWhite), LinearGradientModeHorizontal);
      end
      else
      begin
        lb := TGPLinearGradientBrush.Create(MakeRect(rtinnertop.X - 1, rtinnertop.Y - 1, rtinnertop.Width + 2, rtinnertop.Height + 2),
        MakeColor(20, clBlack), MakeColor(0, clBlack), LinearGradientModeHorizontal);
      end;
      g.FillRectangle(lb, rtinnertop);
      lb.Free;

      rtinnerbottom := MakeRect(rtinner.X + rtinner.Width - rtinner.Width / 4, rtinner.Y, rtinner.Width / 4, rtinner.Height);
      if not FDownCollapsedCaption then
      begin
        lb := TGPLinearGradientBrush.Create(MakeRect(rtinnerbottom.X - 1, rtinnerbottom.Y - 1, rtinnerbottom.Width + 2, rtinnerbottom.Height + 2),
        MakeColor(0, clWhite), MakeColor(50, clWhite), LinearGradientModeHorizontal);
      end
      else
      begin
        lb := TGPLinearGradientBrush.Create(MakeRect(rtinnerbottom.X - 1, rtinnerbottom.Y - 1, rtinnerbottom.Width + 2, rtinnerbottom.Height + 2),
        MakeColor(0, clBlack), MakeColor(20, clBlack), LinearGradientModeHorizontal);
      end;
      g.FillRectangle(lb, rtinnerbottom);
      lb.Free;

      if not FDownCollapsedCaption then
      begin
        rgn := TGPRegion.Create(rtinner);
        g.SetClip(rgn);
        pth := TGPGraphicsPath.Create;
        er := MakeRect(rtinner.X + rtinner.Width / 4, rtinner.Y + rtinner.Height / 8, rtinner.Width * 2, rtinner.Height - rtinner.Height / 4);
        pth.AddEllipse(er);
        gb := TGPPathGradientBrush.Create(pth);
        gb.SetCenterPoint(MakePoint(er.X + er.Width / 2, er.Y + er.Height / 2));
        gb.SetCenterColor(MakeColor(100, clWhite));
        colors[0] := MakeColor(0, clWhite);
        cCount := 1;
        gb.SetSurroundColors(@colors, cCount);
        g.FillPath(gb, pth);
        pth.Free;
        gb.Free;

        g.ResetClip;
        rgn.Free;
      end;

      g.Free;
    end;
  end
  else
  begin
    if not IsOffice2010 then
    begin
      if Assigned(ActivePanel) then
      begin
        if ActivePanel.ColorTo <> clNone then
        begin
          R.Right := R.Right - 1;
          DrawGradient(Canvas, ActivePanel.Color, ActivePanel.ColorTo, 32, R, false);
        end
        else
        begin
          InflateRect(R, +1, +1);
          Canvas.Brush.Color := ActivePanel.Color;
          Canvas.Rectangle(R);
          InflateRect(R, -1, -1);
        end;
      end
      else
      begin
        if DefaultTabColorTo <> clNone then
        begin
          R.Right := R.Right - 1;
          DrawGradient(Canvas, DefaultTabColor, DefaultTabColorTo, 32, R, false);
        end
        else
        begin
          InflateRect(R, +1, +1);
          Canvas.Brush.Color := DefaultTabColor;
          Canvas.Rectangle(R);
          InflateRect(R, -1, -1);
        end;
      end;
    end;
  end;


  Canvas.Brush.Style := bsClear;

  oldfont := TFont.Create;
  oldfont.Assign(Canvas.Font);

  tf := TFont.Create;
  try
    if True then
    begin
      FillChar(lf, SizeOf(lf), 0);
      tf.Assign(CaptionFont);
      GetObject(tf.Handle, SizeOf(Lf), @Lf);

      if Align = alRight then
      begin
        lf.lfEscapement := -900;
        lf.lfOrientation := 30;
      end
      else
      begin
        lf.lfEscapement := -2700;
        lf.lfOrientation := 30;
      end;

      tf.Handle := CreateFontIndirect(Lf);
      Canvas.Font.Assign(tf);
    end;
  finally
    tf.Free;
  end;

  VHeight := Canvas.TextWidth(FCollapsedCaption);
  VWidth := Canvas.TextHeight(FCollapsedCaption);

  Canvas.Font.Color := HoverTextColor;

  // apply clipping for text

  HRGN := CreateRectRgn(R.Left, R.Top + 2, R.Right, R.Bottom - 2);
  SelectClipRgn(Canvas.Handle,HRGN);

  if Align = alRight then
    Canvas.TextOut(R.Right - (R.Right - R.Left - VWidth) div 2,
      R.Top + (R.Bottom - R.Top - VHeight) div 2,FCollapsedCaption)
  else
    Canvas.TextOut(R.Left + (R.Right - R.Left - VWidth) div 2,
      R.Bottom - (R.Bottom - R.Top - VHeight) div 2, FCollapsedCaption);

  SelectClipRgn(Canvas.Handle,0);
  DeleteObject(HRGN);
  
  Canvas.Font.Assign(oldfont);
  oldfont.Free;
end;

function TAdvNavBar.PtInCollapsButton(X,Y: Integer): boolean;
var
  R: TRect;
begin
  R := GetCaptionRect;
  R.Left := Width - 20;
  R.Right := Width - 4;
  Result := PtInRect(R, Point(X,Y));
end;

procedure TAdvNavBar.DrawCaption;
var
  r: TRect;
//  DR: TRect;
//  dx: Integer;
begin
  R := GetCaptionRect;

  if FCaptionColorTo <> clNone then
     DrawGradient(Canvas, FCaptionColor, FCaptionColorTo, 64, R, DefaultGradientDirection = gdHorizontal)
  else
  begin
    Canvas.Brush.Color := FCaptionColor;
    Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  end;

  if FCollapsed and FAllowCollaps then
  begin
    DrawCollapsButton(True);
  end
  else
    if Assigned(ActivePanel) then
    begin
      //dx := 0;
      if AllowCollaps then
      begin
        DrawCollapsButton(False);
        //dx := 20;
      end;
      {
      Canvas.Font.Assign(CaptionFont);
      Canvas.Brush.Style := bsClear;

      DR := Rect(8, GetCaptionRect.Top + (CaptionHeight - Canvas.TextHeight('gh')) div 2, Width - 4 - dx, Height);

      if AllowCollaps then
        DR.Right := DR.Right - 24;

      DrawText(Canvas.Handle, PChar(ActivePanel.AdjustedCaption), Length(ActivePanel.AdjustedCaption), DR, DT_SINGLELINE or DT_LEFT or DT_END_ELLIPSIS);
      }
    end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.DrawFixedTab;
var
  TabR, R2: TRect;
  HorizontalGradient: Boolean;
  clr, clrTo: TColor;
  clrMirror, clrMirrorTo: TColor;

begin
  TabR := GetFixedTabRect;

  if not IsOffice2010 then
  begin
    Canvas.Pen.Color := FBorderColor;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(TabR.Left, TabR.Top, TabR.Right, TabR.Bottom);

    clr := DefaultTabColor;
    clrTo := DefaultTabColorTo;
    clrMirror := DefaultTabMirrorColor;
    clrMirrorTo := DefaultTabMirrorColorTo;

    HorizontalGradient := DefaultGradientDirection = gdHorizontal;
    R2 := Rect(TabR.Left + 1, TabR.Top + 1, TabR.Right - 1, TabR.Bottom);

    if not HorizontalGradient then
      R2.Bottom := R2.Bottom - 1;

    DrawVistaGradient(Canvas, R2, clr, clrTo, clrMirror, clrMirrorTo, gdHorizontal, clNone);
  end;

  //---- Draw FixedTabButtons
  if not (AllowCollaps and FCollapsed) then
    DrawAllFixedTabButtons;
  //---- Draw Popup Indicator
  DrawPopupIndicator;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.DrawFixedTabButton(Index: integer);
var
  TabR, R2, DR: TRect;
  HorizontalGradient: Boolean;
  clr, clrTo: TColor;
  tbmp: TBitmap;
  clrMirror, clrMirrorTo: TColor;
  bcaption: string;
  //DTSTYLE: dword;
  g: TGPGraphics;
  lb: TGPLinearGradientBrush;
  sb: TGPSolidBrush;
  gb: TGPPathGradientBrush;
  p: TGPPen;
  rt, rtinner, rtshadow, er,
  rtinnertop, rtinnerbottom: TGPRectF;
  pth: TGPGraphicsPath;
  colors : array[0..0] of TGPColor;
  cCount: integer;
  rgn: TGPRegion;
  istop: boolean;

begin
  if AllowCollaps and FCollapsed then
    Exit;

  TabR := FixedTabButtonRect(Index);

  if (TabR.Top = TabR.Bottom) then
    Exit;

  istop := tabr.Top = 0;

  if IsOffice2010 then
  begin
    InflateRect(TabR, 0, -2);
  end;

  if (Index = FActiveTabIndex) and CaptionVisible then
  begin
    (*
    Canvas.Font.Assign(CaptionFont);
    Canvas.Brush.Style := bsClear;

    DR := Rect(8, GetCaptionRect.Top + (GetCaptionHeight - Canvas.TextHeight('gh')) div 2, Width - 4, Height);

    if FCollapsed then
    begin
       //anything?
    end
    else
    begin
      if AllowCollaps then
        DR.Right := DR.Right - 24;

      case CaptionAlignment of
      taRightJustify: DTSTYLE := DT_RIGHT;
      taCenter: DTSTYLE := DT_CENTER;
      else
        DTSTYLE := DT_LEFT;
      end;

      DrawText(Canvas.Handle, PChar(TAdvNavBarPanel(FAdvNavBarPanel[Index]).AdjustedCaption),
        Length(TAdvNavBarPanel(FAdvNavBarPanel[Index]).AdjustedCaption), DR, DT_SINGLELINE or DTSTYLE or DT_END_ELLIPSIS);
    end;
    *)
    //Canvas.TextOut(8, GetCaptionRect.Top + (CaptionHeight - Canvas.TextHeight('gh')) div 2, TAdvNavBarPanel(FAdvNavBarPanel[Index]).Caption);

    if Index = FHoverTabIndex then
    begin
      clr := DownTabColor;
      clrto := DownTabColorTo;
      clrMirror := DownTabMirrorColor;
      clrMirrorTo := DownTabMirrorColorTo;
    end
    else
    begin
      clr := ActiveColor;
      clrTo := ActiveColorTo;
      clrMirror := ActiveMirrorColor;
      clrMirrorTo := ActiveMirrorColorTo;
    end;

  end
  else
    if Index = FHoverTabIndex then
    begin
      if Index = FDownTabIndex then
      begin
        clr := FDownTabColor;
        clrTo := FDownTabColorTo;
        clrMirror := DownTabMirrorColor;
        clrMirrorTo := DownTabMirrorColorTo;
      end
      else
      begin
        clr := FHoverTabColor;
        clrTo := FHoverTabColorTo;
        clrMirror := FHoverTabMirrorColor;
        clrMirrorTo := FHoverTabMirrorColorTo;
      end;
    end
    else
    begin
      clr := DefaultTabColor;
      clrto := DefaultTabColorTo;
      clrMirror := DefaultTabMirrorColor;
      clrMirrorTo := DefaultTabMirrorColorTo;
    end;

  if istop then
    Exit;


  HorizontalGradient := FDefaultGradientDirection = gdHorizontal;
  R2 := Rect(TabR.Left, TabR.Top + 1, TabR.Right, TabR.Bottom - 1);

  if not HorizontalGradient then
    R2.Bottom := R2.Bottom - 1;

  if (IsOffice2010 and ((Index = FDownTabIndex) or (Index = FHoverTabIndex) or (Index = FActiveTabIndex))) or (not IsOffice2010) then
  begin
    if IsOffice2010 then
    begin
      rt := MakeRect(R2.Left, R2.Top, R2.Right - R2.Left, r2.Bottom - r2.Top);
      g := TGPGraphics.Create(Canvas.Handle);
      g.SetSmoothingMode(SmoothingModeAntiAlias);

      if index <> FDownTabIndex then
      begin
        rtshadow := MakeRect(rt.X , rt.Y, rt.Width, rt.Height + 2);
        sb := TGPSolidBrush.Create(MakeColor(10, clBlack));
        g.FillRectangle(sb, rtshadow);
        sb.Free;
      end;

      lb := TGPLinearGradientBrush.Create(MakeRect(rt.X - 1, rt.Y - 1, rt.Width + 2, rt.Height + 2), MakeColor(255, clr), MakeColor(255, clrTo), LinearGradientModeVertical);
      g.FillRectangle(lb, rt);
      lb.Free;

      p := TGPPen.Create(MakeColor(255, BorderColor));
      g.DrawRectangle(p, rt);
      p.Free;

      if index <> FDownTabIndex then
        p := TGPPen.Create(MakeColor(100, clWhite))
      else
        p := TGPPen.Create(MakeColor(20, clBlack));
      rtinner := MakeRect(rt.X + 1, rt.Y + 1, rt.Width - 2, rt.Height - 2);
      g.DrawRectangle(p, rtinner);
      p.Free;

      rtinnertop := MakeRect(rtinner.X, rtinner.Y, rtinner.Width, rtinner.Height / 4);
      if index <> FDownTabIndex then
      begin
        lb := TGPLinearGradientBrush.Create(MakeRect(rtinnertop.X - 1, rtinnertop.Y - 1, rtinnertop.Width + 2, rtinnertop.Height + 2),
        MakeColor(50, clWhite), MakeColor(0, clWhite), LinearGradientModeVertical);
      end
      else
      begin
        lb := TGPLinearGradientBrush.Create(MakeRect(rtinnertop.X - 1, rtinnertop.Y - 1, rtinnertop.Width + 2, rtinnertop.Height + 2),
        MakeColor(20, clBlack), MakeColor(0, clBlack), LinearGradientModeVertical);
      end;
      g.FillRectangle(lb, rtinnertop);
      lb.Free;

      rtinnerbottom := MakeRect(rtinner.X, rtinner.Y + rtinner.Height - rtinner.Height / 4, rtinner.Width, rtinner.Height / 4);
      if index <> FDownTabIndex then
      begin
        lb := TGPLinearGradientBrush.Create(MakeRect(rtinnerbottom.X - 1, rtinnerbottom.Y - 1, rtinnerbottom.Width + 2, rtinnerbottom.Height + 2),
        MakeColor(0, clWhite), MakeColor(120, clWhite), LinearGradientModeVertical);
      end
      else
      begin
        lb := TGPLinearGradientBrush.Create(MakeRect(rtinnerbottom.X - 1, rtinnerbottom.Y - 1, rtinnerbottom.Width + 2, rtinnerbottom.Height + 2),
        MakeColor(0, clBlack), MakeColor(20, clBlack), LinearGradientModeVertical);
      end;
      g.FillRectangle(lb, rtinnerbottom);
      lb.Free;

      if index <> FDownTabIndex then
      begin
        rgn := TGPRegion.Create(rtinner);
        g.SetClip(rgn);
        pth := TGPGraphicsPath.Create;
        er := MakeRect(rtinner.X + rtinner.Width / 8, rtinner.Y + rtinner.Height / 4, rtinner.Width - rtinner.width / 4, rtinner.Height * 2);
        pth.AddEllipse(er);
        gb := TGPPathGradientBrush.Create(pth);
        gb.SetCenterPoint(MakePoint(er.X + (er.Width / 2), er.Y + er.Height / 2));
        gb.SetCenterColor(MakeColor(175, clWhite));
        colors[0] := MakeColor(0, clWhite);
        cCount := 1;
        gb.SetSurroundColors(@colors, cCount);
        g.FillPath(gb, pth);
        pth.Free;
        gb.Free;

        g.ResetClip;
        rgn.Free;
      end;

      g.Free;
    end
    else
      DrawVistaGradient(Canvas, R2, clr, clrTo, clrMirror, clrMirrorTo, gdHorizontal, clNone);
  end;

  if Assigned(FSmallImages) then
  begin
    FSmallImages.Draw(Canvas, TabR.Left + 4, TabR.Top + 8,TAdvNavBarPanel(FAdvNavBarPanel[Index]).ImageIndex);
  end
  else
  if Assigned(FImages) then
  begin
    if (FImages.Width = 16) and (FImages.Height = 16) then
    begin
      if (TAdvNavBarPanel(FAdvNavBarPanel[Index]).ImageIndex >= 0) then
        FImages.Draw(Canvas, TabR.Left + 4, TabR.Top + 8,TAdvNavBarPanel(FAdvNavBarPanel[Index]).ImageIndex)
      else
      begin
        Canvas.Brush.Style := bsClear;
        Canvas.Font.Assign(Self.Font);
        Canvas.Font.Size := 7;
        DR := Rect(TabR.Left + 2, TabR.Top, TabR.Left + 24, TabR.Bottom);
        DrawText(Canvas.Handle, PChar(TAdvNavBarPanel(FAdvNavBarPanel[Index]).AdjustedCaption),
          Length(TAdvNavBarPanel(FAdvNavBarPanel[Index]).AdjustedCaption), DR, DT_SINGLELINE or DT_LEFT or DT_END_ELLIPSIS or DT_VCENTER);
      end;
    end
    else
    begin
      tbmp := TBitmap.Create;
      tbmp.Width := FImages.Width;
      tbmp.Height := FImages.Height;
      tbmp.TransparentMode := tmAuto;
      tbmp.Transparent := true;
      tbmp.Canvas.Brush.Color := clFuchsia;
      tbmp.Canvas.FillRect(Rect(0,0,tbmp.Width, tbmp.Height));
      FImages.DrawingStyle := dsTransparent;
      FImages.Draw(tbmp.Canvas, 0, 0, TAdvNavBarPanel(FAdvNavBarPanel[Index]).ImageIndex);
      Canvas.StretchDraw(Rect(TabR.left + 4, TabR.Top + 8, TabR.left + 20, TabR.Top + 24), tbmp);
      tbmp.Free;
    end;
  end
  else
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Assign(Self.Font);
    Canvas.Font.Size := 7;
    DR := Rect(TabR.Left + 2, TabR.Top, TabR.Left + 24, TabR.Bottom);

    bcaption := TAdvNavBarPanel(FAdvNavBarPanel[Index]).AdjustedCaption;
    if Collapsed and (TAdvNavBarPanel(FAdvNavBarPanel[Index]).CollapsedCaption <> '') then
      bcaption := TAdvNavBarPanel(FAdvNavBarPanel[Index]).CollapsedCaption;

    DrawText(Canvas.Handle, PChar(bcaption),
      Length(bcaption), DR, DT_SINGLELINE or DT_LEFT or DT_END_ELLIPSIS or DT_VCENTER);

  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.DrawScrollBtnLeft(R:TRect);
var
  Enble: boolean;
begin
  if (AllowCollaps and FCollapsed) then
    Exit;

  if FScroller.Visible then
  begin
    if BottomIconAlign = biLeft then
      Enble := FScroller.CanGoBack
    else // BottomIconAlign = biRight
      Enble := FScroller.CanGoForward;

    //R := GetFixedTabRect;
    R.Left := R.Left + 1;
    R.Top := R.Top + 1;
    R.Bottom := R.Bottom - 2;
    R.Right := R.Left + ScrollerWidth;
    if Enble then
    begin
      if FScrollerHoverLeftBtn then
      begin
        if FScrollerDownLeftBtn then
          DrawGradient(Canvas, DownTabColor, DownTabColorTo, 16, R, false)
        else
          DrawGradient(Canvas, HoverTabColor, HoverTabColorTo, 16, R, false)
      end
      else
        DrawGradient(Canvas, DefaultTabColor, DefaultTabColorTo, 16, R, false);

      canvas.Brush.Style := bsClear;
      Canvas.Font.Name := 'MS Sans Serif';
      canvas.Font.Style := [fsBold];
      canvas.Font.Color := clBlack;
      Canvas.Font.Size := 8;
      Canvas.TextOut(R.Left + 2, R.Top + 8, '<');
      Canvas.TextOut(R.Left + 6, R.Top + 8, '<');
    end
    else
    begin
      DrawGradient(Canvas, DefaultTabColor, DefaultTabColorTo, 16, R, false);
      canvas.Brush.Style := bsClear;
      Canvas.Font.Name := 'MS Sans Serif';
      canvas.Font.Style := [fsBold];
      canvas.Font.Color := clGray;
      Canvas.Font.Size := 8;
      Canvas.TextOut(R.Left + 2, R.Top + 8, '<');
      Canvas.TextOut(R.Left + 6, R.Top + 8, '<');
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.DrawScrollBtnRight(R:TRect);
var
  Enble: boolean;
begin
  if (AllowCollaps and FCollapsed) then
    Exit;

  if FScroller.Visible then
  begin
    if BottomIconAlign = biLeft then
      Enble := FScroller.CanGoForward
    else // BottomIconAlign = biRight
      Enble := FScroller.CanGoBack;

    //R := GetFixedTabRect;
    R.Top := R.Top + 1;
    R.Bottom := R.Bottom - 2;

    if FPopupIndicator then
      R.Right := R.Right - GetIndicatorWidth;
    R.Left := R.Right - ScrollerWidth;
    R.Right := R.Right - 1;

    if Enble then
    begin

      if FScrollerHoverRightBtn then
      begin
        if FScrollerDownRightBtn then
          DrawGradient(Canvas, DownTabColor, DownTabColorTo, 16, R, false)
        else
          DrawGradient(Canvas, HoverTabColor, HoverTabColorTo, 16, R, false)
      end
      else
        DrawGradient(Canvas, DefaultTabColor, DefaultTabColorTo, 16, R, false);

      Canvas.Brush.Style := bsClear;
      Canvas.Font.Name := 'MS Sans Serif';
      Canvas.Font.Style := [fsBold];
      Canvas.Font.Color := clBlack;
      Canvas.Font.Size := 8;

      Canvas.TextOut(R.Right - ScrollerWidth + 6, R.Top + 8, '>');
      Canvas.TextOut(R.Right - ScrollerWidth + 10, R.Top + 8, '>');
    end
    else
    begin
      DrawGradient(Canvas, DefaultTabColor, DefaultTabColorTo, 16, R, false);
      Canvas.Brush.Style := bsClear;

      Canvas.Font.Name := 'MS Sans Serif';
      Canvas.Font.Style := [fsBold];
      Canvas.Font.Color := clGray;
      Canvas.Font.Size := 8;
      Canvas.TextOut(R.Right - ScrollerWidth + 6, R.Top + 8, '>');
      Canvas.TextOut(R.Right - ScrollerWidth + 10, R.Top + 8, '>');
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.DrawCollapsButton;
var
  s: string;
  R: TRect;
begin
  R := GetCaptionRect;
  
  R.Left := Width - 20;
  R.Right := Width - 4;
  R.Top := R.Top + 4;
  R.Bottom := R.Bottom - 4;

  if FHoverCollapsBtn then
  begin
    if FCollapsDown then
      DrawGradient(Canvas, DownTabColor, DownTabColorTo, 16, R, false)
    else
      DrawGradient(Canvas, HoverTabColor, HoverTabColorTo, 16, R, false)
  end;
//    else
//      DrawGradient(Canvas, DefaultTabColor, DefaultTabColorTo, 16, R, false);

  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := 'MS Sans Serif';
  Canvas.Font.Style := [fsBold];
  Canvas.Font.Color := CaptionFont.Color;
  Canvas.Font.Size := 8;

  if Align = alRight then
  begin
    if Collaps then
      s := '<'
    else
      s := '>';
  end
  else
  begin
    if Collaps then
      s := '>'
    else
      s := '<';
  end;

  if not IsOffice2010 then
  begin
    Canvas.TextOut(R.Right - 10, R.Top + 2, s);
    Canvas.TextOut(R.Right - 14, R.Top + 2, s);
  end
  else
  begin
    Canvas.TextOut(R.Right - 12, R.Top + 2, s);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.DrawPopupIndicator;
var
  R: TRect;
  g: TGPGraphics;
  lb: TGPLinearGradientBrush;
  gb: TGPPathGradientBrush;
  p: TGPPen;
  rt, rtinner, er,
  rtinnertop, rtinnerbottom: TGPRectF;
  pth: TGPGraphicsPath;
  colors : array[0..0] of TGPColor;
  cCount: integer;
  rgn: TGPRegion;
  clr, clrto: TColor;

begin
  if FPopupIndicator then
  begin
    R := GetFixedTabRect;
    R.Top := R.Top + 1;
    R.Bottom := R.Bottom - 2;
    R.Right := R.Right - 1;
    R.Left := R.Right - GetIndicatorWidth;

    if IsOffice2010 then
    begin
      InflateRect(R, 0, -2);
    end;

    if (IsOffice2010 and (FPopupIndicatorHover or FPopupIndicatorDown)) or (not IsOffice2010) then
    begin
      if IsOffice2010 then
      begin
        rt := MakeRect(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
        g := TGPGraphics.Create(Canvas.Handle);
        g.SetSmoothingMode(SmoothingModeAntiAlias);

        clr := DefaultTabColor;
        clrTo := DefaultTabColorTo;
        if FPopupIndicatorHover then
        begin
          if FPopupIndicatorDown then
          begin
            clr := DownTabColor;
            clrTo := DownTabColorTo;
          end
          else
          begin
            clr := HoverTabColor;
            clrTo := HoverTabColorTo;
          end;
        end;

        lb := TGPLinearGradientBrush.Create(MakeRect(rt.X - 1, rt.Y - 1, rt.Width + 2, rt.Height + 2), MakeColor(255, clr), MakeColor(255, clrTo), LinearGradientModeVertical);
        g.FillRectangle(lb, rt);
        lb.Free;

        p := TGPPen.Create(MakeColor(255, BorderColor));
        g.DrawRectangle(p, rt);
        p.Free;

        p := TGPPen.Create(MakeColor(100, clWhite));
        rtinner := MakeRect(rt.X + 1, rt.Y + 1, rt.Width - 2, rt.Height - 2);
        g.DrawRectangle(p, rtinner);
        p.Free;

        rtinnertop := MakeRect(rtinner.X, rtinner.Y, rtinner.Width, rtinner.Height / 4);
        lb := TGPLinearGradientBrush.Create(MakeRect(rtinnertop.X - 1, rtinnertop.Y - 1, rtinnertop.Width + 2, rtinnertop.Height + 2),
        MakeColor(50, clWhite), MakeColor(0, clWhite), LinearGradientModeVertical);
        g.FillRectangle(lb, rtinnertop);
        lb.Free;

        rtinnerbottom := MakeRect(rtinner.X, rtinner.Y + rtinner.Height - rtinner.Height / 4, rtinner.Width, rtinner.Height / 4);
        lb := TGPLinearGradientBrush.Create(MakeRect(rtinnerbottom.X - 1, rtinnerbottom.Y - 1, rtinnerbottom.Width + 2, rtinnerbottom.Height + 2),
        MakeColor(0, clWhite), MakeColor(120, clWhite), LinearGradientModeVertical);
        g.FillRectangle(lb, rtinnerbottom);
        lb.Free;

        rgn := TGPRegion.Create(rtinner);
        g.SetClip(rgn);
        pth := TGPGraphicsPath.Create;
        er := MakeRect(rtinner.X + rtinner.Width / 8, rtinner.Y + rtinner.Height / 4, rtinner.Width - rtinner.width / 4, rtinner.Height * 2);
        pth.AddEllipse(er);
        gb := TGPPathGradientBrush.Create(pth);
        gb.SetCenterPoint(MakePoint(er.X + (er.Width / 2), er.Y + er.Height / 2));
        gb.SetCenterColor(MakeColor(175, clWhite));
        colors[0] := MakeColor(0, clWhite);
        cCount := 1;
        gb.SetSurroundColors(@colors, cCount);
        g.FillPath(gb, pth);
        pth.Free;
        gb.Free;

        g.ResetClip;
        rgn.Free;

        g.Free;
      end
      else
      begin
        if FPopupIndicatorHover then
        begin
          if FPopupIndicatorDown then
            DrawVistaGradient(Canvas, R, DownTabColor, DownTabColorTo, DownTabMirrorColor, DownTabMirrorColorTo, gdHorizontal, clNone)
          else
            DrawVistaGradient(Canvas, R, HoverTabColor, HoverTabColorTo, HoverTabMirrorColor, HoverTabMirrorColorTo, gdHorizontal, clNone);
        end
        else
          DrawVistaGradient(Canvas, R, DefaultTabColor, DefaultTabColorTo, DefaultTabMirrorColor, DefaultTabMirrorColorTo, gdHorizontal, clNone);
      end;
    end;

    if AllowCollaps then
      R.Bottom := R.Bottom - 4;

    if IsOffice2010 then
    begin
      Canvas.Pen.Color := Darker(Color, 50);
             {-------}
      Canvas.MoveTo(R.Left + 4, R.Bottom - 13);
      Canvas.LineTo(R.Left + 11, R.Bottom - 13);
              {-----}
      Canvas.MoveTo(R.Left + 5, R.Bottom - 12);
      Canvas.LineTo(R.Left + 10, R.Bottom - 12);
               {---}
      Canvas.MoveTo(R.Left + 6, R.Bottom - 11);
      Canvas.LineTo(R.Left + 9, R.Bottom - 11);
                {-}
      Canvas.MoveTo(R.Left + 7, R.Bottom - 10);
      Canvas.LineTo(R.Left + 8, R.Bottom - 10);

      Canvas.Pen.Color := clWhite;
               {---}
      Canvas.MoveTo(R.Left + 4, R.Bottom - 12);
      Canvas.LineTo(R.Left + 8, R.Bottom - 8);

      Canvas.MoveTo(R.Left + 10, R.Bottom - 12);
      Canvas.LineTo(R.Left + 6, R.Bottom - 8);
    end
    else
    begin
      Canvas.Pen.Color := clBlack;
             {-------}
      Canvas.MoveTo(R.Left + 7, R.Bottom - 10);
      Canvas.LineTo(R.Left + 14, R.Bottom - 10);
              {-----}
      Canvas.MoveTo(R.Left + 8, R.Bottom - 9);
      Canvas.LineTo(R.Left + 13, R.Bottom - 9);
               {---}
      Canvas.MoveTo(R.Left + 9, R.Bottom - 8);
      Canvas.LineTo(R.Left + 12, R.Bottom - 8);
                {-}
      Canvas.MoveTo(R.Left + 10, R.Bottom - 7);
      Canvas.LineTo(R.Left + 11, R.Bottom - 7);
    end;

    if not IsOffice2010 then
    begin
      if not AllowCollaps then
      begin
        Canvas.Font.Name := 'MS Sans Serif';
        Canvas.Brush.Style := bsClear;
        Canvas.Font.Style := [fsBold];
        Canvas.Font.Color := clBlack;
        Canvas.Font.Size := 8;
        Canvas.TextOut(R.Right - GetIndicatorWidth + 6, R.Top + 6, '>');
        Canvas.TextOut(R.Right - GetIndicatorWidth + 9, R.Top + 6, '>');
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.DrawScrollButtons;
begin
  DrawScrollBtnLeft(GetFixedTabRect);
  DrawScrollBtnRight(GetFixedTabRect);
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.DrawAllFixedTabButtons;
var
  i, j, k: integer;
begin
  //j:= max((FAdvNavBarPanel.Count - GetFixedTabButtonCount), 0);
  k := FixedBtnMaxIndex; //(FAdvNavBarPanel.Count -1- FDisplayCaptionTabsCount);
  j := FixedBtnMinIndex; //max(k+1-GetFixedTabButtonCount,0);
  
  for i := k downto j do
    DrawFixedTabButton(i);

  if ((FActiveTabIndex < j) or (FActiveTabIndex > j))  and (FActiveTabIndex >= 0) then
    DrawFixedTabButton(FActiveTabIndex);

  if FScroller.Visible then
    DrawScrollButtons;
end;

//------------------------------------------------------------------------------

function TAdvNavBar.FixedBtnMaxIndex: integer;
begin
  if FReverseOrder then
    Result := (FAdvNavBarPanel.Count - 1 - FDisplayCaptionTabsCount)
  else
    Result := (FAdvNavBarPanel.Count - 1);

  if FScroller.Visible then
    Result := Result - FScroller.Position;
end;

//------------------------------------------------------------------------------

function TAdvNavBar.FixedBtnMinIndex: integer;
begin
  if FReverseOrder then
    Result := max(FixedBtnMaxIndex + 1 - GetFixedTabButtonCount, 0)
  else
    Result := max(FixedBtnMaxIndex + 1 - GetFixedTabButtonCount, FDisplayCaptionTabsCount);
end;

//------------------------------------------------------------------------------

function TAdvNavBar.PtOnScrollLeftBtn(X, Y: integer): Boolean;
var
  R: TRect;
  P: TPoint;
begin
  R := GetFixedTabRect;
  P := Point(X, Y);
  R.Right := R.Left + ScrollerWidth;
  Result := PtInRect(R, P);
end;

//------------------------------------------------------------------------------

function TAdvNavBar.PtOnScrollRightBtn(X, Y: integer): Boolean;
var
  R: TRect;
  P: TPoint;
begin
  R := GetFixedTabRect;
  P := Point(X, Y);
  if FPopupIndicator then
    R.Right := R.Right - GetIndicatorWidth - 1;
  R.Left := R.Right - ScrollerWidth;
  Result := PtInRect(R, P);
end;

//------------------------------------------------------------------------------

function TAdvNavBar.PtOnPopupIndicator(X, Y: integer): Boolean;
var
  R: TRect;
  P: TPoint;
begin
  Result := false;
  if FPopupIndicator then
  begin
    R := GetFixedTabRect;
    P := Point(X, Y);
    R.Left := R.Right - GetIndicatorWidth - 1;
    Result := PtInRect(R, P);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetCaptionAlignment(const Value: TAlignment);
begin
  if (FCaptionAlignment <> Value) then
  begin
    FCaptionAlignment := Value;
    Invalidate;
  end;
end;

procedure TAdvNavBar.SetCaptionColor(const Value: TColor);
begin
  if Value <> FCaptionColor then
  begin
    FCaptionColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetCaptionColorTo(const Value: TColor);
begin
  if Value <> FCaptionColorTo then
  begin
    FCaptionColorTo := Value;
    Invalidate;
  end;
end;


//------------------------------------------------------------------------------

procedure TAdvNavBar.SetActiveColor(const Value: TColor);
begin
  if Value <> FActiveColor then
  begin
    FActiveColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetActiveColorTo(const Value: TColor);
begin
  if Value <> FActiveColorTo then
  begin
    FActiveColorTo := Value;
    Invalidate;
  end;
end;

procedure TAdvNavBar.SetActiveMirrorColor(const Value: TColor);
begin
  FActiveMirrorColor := Value;
end;

procedure TAdvNavBar.SetActiveMirrorColorTo(const Value: TColor);
begin
  FActiveMirrorColorTo := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetDefaultTabColor(const Value: TColor);
begin
  if Value <> FDefaultTabColor then
  begin
    FDefaultTabColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetDefaultTabColorTo(const Value: TColor);
begin
  if Value <> FDefaultTabColorTo then
  begin
    FDefaultTabColorTo := Value;
    Invalidate;
  end;
end;

procedure TAdvNavBar.SetDefaultTabMirrorColor(const Value: TColor);
begin
  if Value <> FDefaultTabMirrorColor then
  begin
    FDefaultTabMirrorColor := Value;
    Invalidate;
  end;
end;

procedure TAdvNavBar.SetDefaultTabMirrorColorTo(const Value: TColor);
begin
  if Value <> FDefaultTabMirrorColorTo then
  begin
    FDefaultTabMirrorColorTo := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetDefaultTextColor(const Value: TColor);
begin
  if Value <> FDefaultTextColor then
  begin
    FDefaultTextColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetActiveTabIndex(const Value: integer);
var
  AllowActivate: Boolean;
  NewIndex: integer;
begin
  if (csLoading in ComponentState) then
    FActiveTabIndex := Value;

  NewIndex := GetDisplayIndex(Value); //GetDupIndex(Value);

  if Value < 0 then
  begin
    FActiveTabIndex := -1;
    FActivePanel := nil;
  end
  else
    if (NewIndex <> FActiveTabIndex) and (NewIndex >= 0) and (NewIndex < FAdvNavBarPanel.Count) then
    begin
      AllowActivate := True;

      if AllowActivate then
      begin
        if Assigned(FOnTPanelActivate) and (FActiveTabIndex >= 0) then
          FOnTPanelActivate(self, FDupAdvNavBarPanel.IndexOf(FAdvNavBarPanel[FActiveTabIndex]), FDupAdvNavBarPanel.IndexOf(FAdvNavBarPanel[NewIndex]), AllowActivate);

        if AllowActivate then
        begin
          if (FActiveTabIndex >= 0) and (FActiveTabIndex < FAdvNavBarPanel.Count) then
             TAdvNavBarPanel(FAdvNavBarPanel[FActiveTabIndex]).Visible := False;

          //if not FInternalCall then
           // FActiveTabIndex:= GetDisplayIndex(NewIndex)
          //else
          FActiveTabIndex := NewIndex; //Value;

          FActivePanel := TAdvNavBarPanel(FAdvNavBarPanel[FActiveTabIndex]);
          TAdvNavBarPanel(FAdvNavBarPanel[FActiveTabIndex]).Visible := not FCollapsed;
          ShowAdvNavBarPanel(FActiveTabIndex);

          Invalidate;
          DoChange;
        end;
     end;
   end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetActiveTextColor(const Value: TColor);
begin
  if (FActiveTextColor <> Value) then
  begin
    FActiveTextColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAdvNavBar.GetActiveTabIndex: integer;
begin
  Result := GetDupIndex(FActiveTabIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetActivePanel(const Value: TAdvNavBarPanel);
var
  AllowActivate: Boolean;
  i: integer;
begin
  if Value = nil then
  begin
    FActiveTabIndex := -1;
    FActivePanel := nil;
  end
  else
    if Value <> FActivePanel then
    begin
      AllowActivate := true;
      i := FAdvNavBarPanel.IndexOf(Value);
      if i < 0 then
        raise Exception.Create('Invalid AdvNavBarPanel');

      if Assigned(FOnTPanelActivate) then
        FOnTPanelActivate(self, FActiveTabIndex, {i}FDupAdvNavBarPanel.IndexOf(FAdvNavBarPanel[i]), AllowActivate);

      if AllowActivate then
      begin
        FActiveTabIndex := i;
        FActivePanel := Value;
        ShowAdvNavBarPanel(FActiveTabIndex);
        Invalidate;
        DoChange;
      end;
    end;
end;

procedure TAdvNavBar.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.RemoveAdvNavBarPanel(Value: TAdvNavBarPanel);
var
  aAdvNavBarPanel: TAdvNavBarPanel;
  i: integer;
  vis: boolean;
begin
  inc(FUpdateCount);

  vis := Value.TabVisible;

  aAdvNavBarPanel := ActivePanel;
  if Value = ActivePanel then
    aAdvNavBarPanel := FindNextPanel(Value, true);

  if aAdvNavBarPanel = Value then
    aAdvNavBarPanel := nil;

  FAdvNavBarPanel.Remove(Value);

  FDupAdvNavBarPanel.Remove(Value);

  Value.FAdvNavBar := nil;

  ActivePanel := aAdvNavBarPanel;

  if vis then
    MoveSplitterInTabs(1);

  dec(FUpdateCount);

  Invalidate;
  //FDisplayCaptionTabsCount:= FDisplayCaptionTabsCount - 1;
  //SetAdvNavBarPanelPosition(Value);

  for i := 1 to FAdvNavBarPanel.Count do
  begin
    TAdvNavBarPanel(FAdvNavBarPanel[i - 1]).FPanelIndex := i - 1;
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;

  if (Screen.Cursor = crSizeNS) then
    Screen.Cursor := FOldCursor;

  FHoverTabIndex := -1;
  FScrollerHoverLeftBtn := false;
  FScrollerHoverRightBtn := false;
  FPopupIndicatorHover := false;
  FPopupIndicatorDown := false;
  FHoverCollapsBtn := False;
  FCollapsDown := False;
  FHoverCollapsedCaption := False;

  if Hint <> FOriginalHint then
    Hint := FOriginalHint;

  invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  i: integer;
begin
  inherited;

  FMouseDown := true;

  if (csdesigning in ComponentState) then
  begin
    //FInternalCall:= true;
    i := IndexOfTabAt(X, Y);
    if (i >= 0) then
      ActiveTabIndex := i;
    //FInternalCall:= false;
    Exit;
  end;

  if Button = mbLeft then
  begin
    FInternalCall := true;
    i := IndexOfTabAt(X, Y);
    FInternalCall := false;
    if (i >= 0) then
    begin
      FDownTabIndex := i;
      RefreshCaptionTabOrButton(i);
      Invalidate;
    end;

    if AllowCollaps then
    begin
      if PtInRect(GetPanelRect,Point(X,Y)) then  // collapsed panel clicked
      begin
        FDownCollapsedCaption := true;
        Invalidate;

        if Assigned(OnCollapsedClick) then
          OnCollapsedClick(Self);
      end;

      if PtInCollapsButton(X,Y) then
      begin
        FCollapsDown := true;
        Invalidate;
      end;
    end;

    if FMouseCaptured then
      FCheckForSplitterMove := true;

    if (Y > GetFixedTabRect.Top - 10) then
    begin
      if PtOnScrollLeftBtn(X, Y) then
      begin
        FScrollerDownLeftBtn := true;
        invalidate;
      end;

      if PtOnScrollRightBtn(X, Y) then
      begin
        FScrollerDownRightBtn := true;
        invalidate;
      end;

      if PtOnPopupIndicator(X, Y) then
      begin
        FPopupIndicatorDown := true;
        invalidate;
      end;
    end;
  end;

  {$IFDEF DELPHI_UNICODE}
  if Assigned(FActivePanel) and (ActiveTabIndex <> - 1) then
    FActivePanel.Repaint;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  if AllowCollaps then
  begin
    if PtInRect(ClientRect,Point(X,Y)) then
    begin
      if not FHoverCollapsedCaption then
      begin
        FHoverCollapsedCaption := True;
        invalidate;
      end;
    end
    else
      if FHoverCollapsedCaption then
      begin
        FHoverCollapsedCaption := False;
        invalidate;
      end;

    if PtInCollapsButton(X,Y) then
    begin
      FHoverCollapsBtn := true;
      invalidate;
    end
    else
    begin
      if FHoverCollapsBtn then
      begin
        FHoverCollapsBtn := false;
        invalidate;
      end;
    end;
  end;

  if PtOnSplitter(Point(X, Y)) then
  begin
    if not FMouseCaptured then
    begin
      FOldCursor := Screen.Cursor;
      Screen.Cursor := crSizeNS;
      SetCapture(Handle);
      FMouseCaptured := true;
      Application.HandleMessage;
    end;
  end
  else
  begin
    if FMouseCaptured and not FCheckForSplitterMove then
    begin
      Screen.Cursor := FOldCursor;
      ReleaseCapture;
      FMouseCaptured := false;
    end;
  end;

  if FMouseCaptured and FCheckForSplitterMove then
  begin
    MoveSplitterTo(Y);
  end;

  if not FMouseCaptured and not FCheckForSplitterMove then
  begin
    i := IndexOfTabAt(X, Y);

    if (i <> FCaptionHintIndex) then
    begin
      Application.CancelHint;
      FCaptionHintIndex := i;
    end;

    FInternalCall := true;
    i := IndexOfTabAt(X, Y);
    FInternalCall := false;

    if (i >= 0) and (i <> FHoverTabIndex) then
    begin
      if (FHoverTabIndex >= 0) then
      begin
        FHoverTabIndex := -1;
        invalidate;
        if Hint <> FOriginalHint then
        begin
          Application.CancelHint;
          Hint := FOriginalHint;
        end;
      end;

      FHoverTabIndex := i;
      Invalidate;

      if (Y > GetFixedTabRect.Top) then
      begin
        Hint := TAdvNavBarPanel(FAdvNavBarPanel[i]).Hint;
        Application.CancelHint;
       // if Hint <> '' then
        //  Application.ActivateHint(Point(X, Y));
      end;
    end
    else
    if (i < 0) and (FHoverTabIndex >= 0) then
    begin
      FHoverTabIndex := -1;
      invalidate;

      if Hint <> FOriginalHint then
      begin
        Application.CancelHint;
        Hint := FOriginalHint;
      end;
    end;

    if (Y > GetFixedTabRect.Top - 10) then
    begin
      if PtOnScrollLeftBtn(X, Y) then
      begin
        if not FScrollerHoverLeftBtn then
        begin
          FScrollerHoverLeftBtn := true;
          Invalidate;
        end;
      end
      else if FScrollerHoverLeftBtn then
      begin
        FScrollerHoverLeftBtn := false;
        Invalidate;
      end;

      if PtOnScrollRightBtn(X, Y) then
      begin
        if not FScrollerHoverRightBtn then
        begin
          FScrollerHoverRightBtn := true;
          Invalidate;
        end;
      end
      else if FScrollerHoverRightBtn then
      begin
        FScrollerHoverRightBtn := false;
        Invalidate;
      end;

      if PtOnPopupIndicator(X, Y) then
      begin
        if not FPopupIndicatorHover then
        begin
          FPopupIndicatorHover := true;
          Invalidate;
        end;
      end
      else if FPopupIndicatorHover then
      begin
        FPopupIndicatorHover := false;
        Invalidate;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  i: integer;
begin
  inherited;

  {$IFNDEF DELPHI_UNICODE}
  if FMouseDown then
    if Assigned(OnClick) then
      OnClick(Self);
  {$ENDIF}

  FMouseDown := false;

  if AllowCollaps then
  begin
    if FDownCollapsedCaption then
    begin
      FDownCollapsedCaption := false;
      invalidate;
    end;

    if PtInCollapsButton(X,Y) then
    begin
      Collapsed := not Collapsed;
      if Assigned(OnCollapsChange) then
        OnCollapsChange(Self);
    end;
    FCollapsDown := false;
  end;

  FDownTabIndex := -1;
  Invalidate;

  if FMouseCaptured and FCheckForSplitterMove then
  begin
    Screen.Cursor := FOldCursor;
    FCheckForSplitterMove := false;
    ReleaseCapture;
    FMouseCaptured := false;

    {$IFDEF DELPHI_UNICODE}
    if Assigned(FActivePanel) and (ActiveTabIndex <> - 1) then
      FActivePanel.Repaint;
    {$ENDIF}
    Exit;
  end;

  if not FMouseCaptured and not FCheckForSplitterMove then
  begin
    FInternalCall := true;
    i := IndexOfTabAt(X, Y);
    if (i >= 0) and (i <> ActiveTabIndex) then
      ActiveTabIndex := i;
    FInternalCall := false;

    if (Y > GetFixedTabRect.Top) and (i < 0) then
    begin
      if PtOnScrollLeftBtn(X, Y) then
      begin
        FScrollerDownLeftBtn := false;
        ScrollLeftBtnClick;
      end
      else if PtOnScrollRightBtn(X, Y) then
      begin
        FScrollerDownRightBtn := false;
        ScrollRightBtnClick;
      end;

      if PtOnPopupIndicator(X, Y) then
      begin
        FPopupIndicatorDown := false;
        invalidate;
        if Assigned(FOnPopupMenuClick) then
          FOnPopupMenuClick(Self);
      end;
    end;
  end;
  {$IFDEF DELPHI_UNICODE}
  if Assigned(FActivePanel) and (ActiveTabIndex <> - 1) then
    FActivePanel.Repaint;
  {$ENDIF}

end;

//------------------------------------------------------------------------------

function TAdvNavBar.MoveSplitterTo(Y: integer): Boolean;
var
  SplitterTop, d: integer;
begin
  Result := false;
  SplitterTop := GetSplitterRect.Top;
  d := Y - SplitterTop;
  if abs(d) >= GetCaptionTabHeight then
  begin
    if (FDefaultTabPosition = tpBottom) then
      Result := MoveSplitterInTabs(d div GetCaptionTabHeight) > 0
    else
      Result := MoveSplitterInTabs(-(d div GetCaptionTabHeight)) > 0;
  end
end;

procedure TAdvNavBar.UpdateScroller;
begin
  if not Assigned(FScroller) then
    Exit;

  if FScroller.Visible then
  begin
    if GetFixedTabButtonMaxCount >= FAdvNavBarPanel.Count - FDisplayCaptionTabsCount then
      HideScrollButtons;
    FScroller.Position := FScroller.Min;
    FScroller.Max := (FAdvNavBarPanel.Count - FDisplayCaptionTabsCount) - GetFixedTabButtonCount;
  end
  else
  begin
    if GetFixedTabButtonMaxCount < FAdvNavBarPanel.Count - FDisplayCaptionTabsCount then
      ShowScrollButtons;
  end;
end;

//------------------------------------------------------------------------------

function TAdvNavBar.MoveSplitterInTabs(TabCount: Integer): Integer;
var
  CaptionTabsToBeDisplayed, OldSplitterPos: Integer;

begin
  Result := 0;

  if TabCount = 0 then
    Exit;

  //if (FDefaultTabPosition = tpTop) then
   // TabCount := -TabCount;

  if not FInternalSplitterMove and (FOldSplitterPosForSizeChange > 0) then
    FOldSplitterPosForSizeChange := -1;
  
  OldSplitterPos := FSplitterPosition;

  if TabCount < 0 then // Up
  begin
    (*
    if not ((csDesigning in ComponentState) and (csLoading in ComponentState)) then
    begin
      if ((FSeparatorLine < GetCaptionHeight + GetCaptionTabHeight + (SplitterHeight div 2) + 2 + MinClientHeight) and (FDefaultTabPosition = tpBottom) and not FInternalSplitterMove)
        or ((FSeparatorLine > Height - GetCaptionHeight - GetCaptionTabHeight - (SplitterHeight div 2) - 2 - MinClientHeight) and (FDefaultTabPosition = tpTop) and not FInternalSplitterMove) then
        Exit;
    end;
    *)

    CaptionTabsToBeDisplayed := FAdvNavBarPanel.Count - FDisplayCaptionTabsCount;

    if CaptionTabsToBeDisplayed <= 0 then
      Exit;

    CaptionTabsToBeDisplayed := min(CaptionTabsToBeDisplayed, abs(TabCount));
    //showmessage(inttostr(FDisplayCaptionTabsCount)+' '+inttostr(CaptionTabsToBeDisplayed));
    FDisplayCaptionTabsCount := FDisplayCaptionTabsCount + CaptionTabsToBeDisplayed;

    if (csDesigning in ComponentState) and (csLoading in ComponentState) then
      FTempSplitterPos := FDisplayCaptionTabsCount
    else
      FSplitterPosition := FDisplayCaptionTabsCount;

    //FSeparatorLine:= FSeparatorLine - (CaptionTabsToBeDisplayed*FCaptionTabHeight);
    SetAllAdvNavBarPanelPosition;
    if (FActiveTabIndex >= 0) and (FActiveTabIndex <= FAdvNavBarPanel.Count - 1) then
    begin
      TAdvNavBarPanel(FAdvNavBarPanel[FActiveTabIndex]).SendToBack;
      TAdvNavBarPanel(FAdvNavBarPanel[FActiveTabIndex]).BringToFront;
    end;
    Result := CaptionTabsToBeDisplayed;
    //Invalidate;
  end
  else // TabCount > 0   Down
  begin
//    if ((FSeparatorLine >= Height - 10 - CaptionTabHeight {FixedTab}) and (FDefaultTabPosition = tpBottom))
//      or ((FSeparatorLine <= 10 + CaptionTabHeight {FixedTab}) and (FDefaultTabPosition = tpTop)) then
//      begin
//        Exit;
//      end;
    CaptionTabsToBeDisplayed := min(FDisplayCaptionTabsCount, TabCount);
    FDisplayCaptionTabsCount := FDisplayCaptionTabsCount - CaptionTabsToBeDisplayed;
    //FSeparatorLine:= FSeparatorLine + (CaptionTabsToBeDisplayed*FCaptionTabHeight);
    if (csDesigning in ComponentState) and (csLoading in ComponentState) then
      FTempSplitterPos := FDisplayCaptionTabsCount
    else
      FSplitterPosition := FDisplayCaptionTabsCount;
    SetAllAdvNavBarPanelPosition;
    if (FActiveTabIndex >= 0) and (FActiveTabIndex <= FAdvNavBarPanel.Count - 1) then
    begin
      TAdvNavBarPanel(FAdvNavBarPanel[FActiveTabIndex]).SendToBack;
      TAdvNavBarPanel(FAdvNavBarPanel[FActiveTabIndex]).BringToFront;
    end;
    Result := CaptionTabsToBeDisplayed;
    //Invalidate;
  end;

  UpdateScroller;

  Invalidate;
  if Assigned(FOnSplitterMove) and (OldSplitterPos <> FSplitterPosition) and not FSplitterInternalCall then
    FOnSplitterMove(self, OldSplitterPos, FSplitterPosition);
end;

//------------------------------------------------------------------------------

function TAdvNavBar.IndexOfTabAt(X, Y: integer): integer;
begin
  if (y >= GetFixedTabRect.Top) and (y <= GetFixedTabRect.Bottom) then
  begin
    Result := IndexOfBtnAt(X, Y);
  end
  else
  begin
    Result := IndexOfCaptionTabAt(X, Y);
  end;
end;

//------------------------------------------------------------------------------

function TAdvNavBar.IndexOfCaptionTabAt(X, Y: integer): integer;
var
  P: TPoint;
  i: integer;
begin
  Result := -1;
  P := Point(X, Y);
  for i := 0 to FAdvNavBarPanel.Count - 1 do
  begin
    if PtInRect(CaptionTabRect(i), P) then
    begin
      Result := i;
      Break;
    end;
  end;
  Result := GetDupIndex(Result);
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  if (((Msg.YPos >= FSeparatorLine) and (FDefaultTabPosition = tpBottom)) or ((Msg.YPos <= FSeparatorLine) and (FDefaultTabPosition = tpTop))) and (GetAsyncKeyState(VK_LBUTTON) <> 0) then
    Msg.Result := 1
  else
    inherited;
end;

procedure TAdvNavBar.CMHintShow(var Msg: TCMHintShow);
var
  hi: PHintInfo;
  i: integer;

begin
  hi := Msg.HintInfo;

  i := IndexOfTabAt(hi.CursorPos.X, hi.CursorPos.Y);

  if (i >= 0) then
  begin
    hi.HintStr := Panels[i].CaptionHint;

    if (Panels[i].CaptionHint = '') and (Panels[i].Hint <> '') then
      hi.HintStr := Panels[i].Hint;

    FCaptionHintIndex := i;
  end
  else
  begin
    hi.HintStr := Hint;
    FCaptionHintIndex := -1;
  end;
end;

//------------------------------------------------------------------------------

function TAdvNavBar.GetSplitterRect: TRect;
var
  h: integer;
begin
  h := SplitterHeight div 2;
  if IsOffice2010 then
    Result := Rect(6, FSeparatorLine - h, width - 6, FSeparatorLine + h)
  else
    Result := Rect(1, FSeparatorLine - h, width - 1, FSeparatorLine + h)
end;

//------------------------------------------------------------------------------

function TAdvNavBar.GetCaptionHeight: Integer;
begin
  if CaptionVisible then
    Result := CaptionHeight
  else
    Result := 0;
end;

function TAdvNavBar.GetCaptionRect: TRect;
begin
  if FDefaultTabPosition = tpBottom then
    Result := Rect(1, 1, Width - 1, GetCaptionHeight)
  else
    Result := Rect(1, FSeparatorLine + (SplitterHeight div 2) + 1, Width - 1, FSeparatorLine + (SplitterHeight div 2) + 1 + GetCaptionHeight);
end;

function TAdvNavBar.GetCaptionTabHeight: Integer;
begin
  Result := CaptionTabHeight
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.DrawSplitter;
var
  PS: integer;
  i: integer;
  R: TRect;
  rt: TGPRectF;
  g: TGPGraphics;
  lb: TGPLinearGradientBrush;
  p: TGPPen;
begin
  R := GetSplitterRect;

  if IsOffice2010 then
  begin
    rt := MakeRect(R.Left, r.Top + 2, r.Right - r.Left, r.Bottom - r.Top - 2);
    g := TGPGraphics.Create(Canvas.Handle);
    g.SetSmoothingMode(SmoothingModeAntiAlias);
    lb := TGPLinearGradientBrush.Create(MakeRect(rt.X - 1, rt.Y - 1, rt.Width + 2, rt.Height + 2), MakeColor(255, SplitterColor),
      MakeColor(255, SplitterColorTo), LinearGradientModeVertical);

    g.FillRectangle(lb, rt);

    lb.Free;

    p := TGPPen.Create(MakeColor(255, SplitterBorderColor));
    g.DrawRectangle(p, rt);
    p.Free;
    g.Free;
  end
  else
  begin
    DrawGradient(Canvas, FSplitterColor, FSplitterColorTo, 16, R, false);

    Canvas.Pen.Color := clWhite;
    Canvas.Brush.Color := clWhite;
    R.Top := R.Top + 2;
    PS := (Width - 34 {Dots Length}) div 2;
    R.Left := PS + 1;
    for i := 1 to 9 do
    begin
      Canvas.Rectangle(R.Left, R.Top, R.Left + 2, R.Top + 2);
      R.Left := R.Left + 4;
    end;

    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := clBlack;
    R.Top := R.Top - 1;
    R.Left := PS;
    for i := 1 to 9 do
    begin
      Canvas.Rectangle(R.Left, R.Top, R.Left + 2, R.Top + 2);
      R.Left := R.Left + 4;
    end;

    R.Top := R.Top + 1;
    R.Left := PS + 1;
    for i := 1 to 9 do
    begin
      Canvas.Pixels[R.Left, R.Top] := FSplitterColorTo;
      R.Left := R.Left + 4;
    end;

    if SplitterBorderColor <> clNone then
    begin
      Canvas.Pen.Color := SplitterBorderColor;
      Canvas.Rectangle(R);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvNavBar.PtOnSplitter(P: TPoint): Boolean;
begin
  Result := PtInRect(GetSplitterRect, P) and ShowSplitter;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetHoverTabColor(const Value: TColor);
begin
  FHoverTabColor := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetHoverTabColorTo(const Value: TColor);
begin
  FHoverTabColorTo := Value;
end;

procedure TAdvNavBar.SetHoverTabMirrorColor(const Value: TColor);
begin
  FHoverTabMirrorColor := Value;
end;

procedure TAdvNavBar.SetHoverTabMirrorColorTo(const Value: TColor);
begin
  FHoverTabMirrorColorTo := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetDownTabColor(const Value: TColor);
begin
  FDownTabColor := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetDownTabColorTo(const Value: TColor);
begin
  FDownTabColorTo := Value;
end;

procedure TAdvNavBar.SetDownTabMirrorColor(const Value: TColor);
begin
  FDownTabMirrorColor := Value;
end;

procedure TAdvNavBar.SetDownTabMirrorColorTo(const Value: TColor);
begin
  FDownTabMirrorColorTo := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetImages(const Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    FImages := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SelectNextPanel;
begin
  FInternalCall := true;
  ActiveTabIndex := FindNextPanelIndex(FActiveTabIndex, true);
  FInternalCall := false;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SelectPreviousPanel;
begin
  FInternalCall := true;
  ActiveTabIndex := FindNextPanelIndex(FActiveTabIndex, false);
  FInternalCall := false;
end;

//------------------------------------------------------------------------------

function TAdvNavBar.FindNextPanelIndex(CurIndex: integer;
  GoForward: Boolean): integer;
begin
  Result := CurIndex;
  CurIndex := GetDisplayIndex(CurIndex);

  if (FAdvNavBarPanel.Count <= 1) or (CurIndex < 0) then
    exit;
  if GoForward then
  begin
    CurIndex := CurIndex + 1;
    if CurIndex >= FAdvNavBarPanel.Count then
      CurIndex := 0;
  end
  else
  begin
    CurIndex := CurIndex - 1;
    if CurIndex < 0 then
      CurIndex := FAdvNavBarPanel.Count - 1;
  end;
  Result := GetDupIndex(CurIndex);
end;

//------------------------------------------------------------------------------

function TAdvNavBar.FindNextPanel(CurPanel: TAdvNavBarPanel;
  GoForward: Boolean): TAdvNavBarPanel;
var
  i: integer;
begin
  Result := CurPanel;
  if CurPanel = nil then
    Exit;
  i := FAdvNavBarPanel.IndexOf(CurPanel);
  FInternalCall := true;
  i := FindNextPanelIndex(i, GoForward);
  FInternalCall := false;
  if i >= 0 then
    Result := TAdvNavBarPanel(FAdvNavBarPanel[i])
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  i: Integer;
begin
  inherited;
  
  if (AOperation = opRemove) and (AComponent = FImages) then
  begin
    FImages := nil;
    Invalidate;
  end;

  if (AOperation = opRemove) and not (csDestroying in ComponentState) and (csDesigning in ComponentState) then
  begin
    i := 0;
    while (i < FAdvNavBarPanel.Count) do
    begin
      if FAdvNavBarPanel[i] = AComponent then
        RemoveAdvNavBarPanel(TAdvNavBarPanel(AComponent))
      else
        inc(i);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvNavBar.GetPanelRect: TRect;
begin
  if FDefaultTabPosition = tpBottom then
  begin
    Result.Left := 1;
    if (BorderStyle = bsSingle) and not CaptionVisible then
      Result.Top := 1
    else
      Result.Top := GetCaptionHeight;
    Result.Right := Result.Left + Width - 2;
    Result.Bottom := Result.Top + FSeparatorLine - (SplitterHeight div 2) - GetCaptionHeight;
  end
  else
  begin
    Result.Left := 1;
    Result.Top := FSeparatorLine + (SplitterHeight div 2) + GetCaptionHeight;
    Result.Right := Result.Left + Width - 2;
    Result.Bottom := Height - 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetMinClientHeight(const Value: integer);
var
  ClRect: TRect;
  ClientHeight, df: integer;
begin
  if Value > 20 then
  begin
    FMinClientHeight := Value;
    ClRect := GetClientRect;
    ClientHeight := ClRect.Bottom - ClRect.Top;
    if ClientHeight < FMinClientHeight then
    begin
      df := FMinClientHeight - ClientHeight;
      inherited SetBounds(Left, Top, Width, Height + df);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvNavBar.GetFixedTabRect: TRect;
begin
  if not IsOffice2010 then
  begin
    if FDefaultTabPosition = tpBottom then
      Result := Rect(0, Height - GetFixedCaptionTabHeight - 1, Width, Height)
    else
      Result := Rect(0, 1, Width, GetFixedCaptionTabHeight + 1)
  end
  else
  begin
    if FDefaultTabPosition = tpBottom then
      Result := Rect(0, Height - GetFixedCaptionTabHeight - 1, Width - 7, Height)
    else
      Result := Rect(0, 1, Width - 7, GetFixedCaptionTabHeight + 1)
  end;
end;

function TAdvNavBar.GetIndicatorWidth: Integer;
begin
  if IsOffice2010 then
    Result := IndicatorWidthOffice2010
  else
    Result := IndicatorWidth;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetDefaultGradientDirection(
  const Value: TGradientDirection);
begin
  FDefaultGradientDirection := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvNavBar.FixedTabButtonRect(index: integer): TRect;
var
  R: TRect;
  lf, j, k: integer;
begin
  //R:= GetFixedTabRect;
  //lf:= FixedBtnStart+ abs(FAdvNavBarPanel.Count-1-index)* FixedBtnWidth;
  //lf:= FFixedBtnLeftMargin+ abs((FAdvNavBarPanel.Count -1- FDisplayCaptionTabsCount)-index)* FixedBtnWidth;

  Result := Rect(0, 0, 0, 0);

  //j:= max((FAdvNavBarPanel.Count - GetFixedTabButtonCount), 0);

  k := FixedBtnMaxIndex; //(FAdvNavBarPanel.Count -1- FDisplayCaptionTabsCount);
  j := FixedBtnMinIndex; //max(k+1-GetFixedTabButtonCount,0);

  if (index <= k) and (index >= j) then
  begin
    R := GetFixedTabRect;
    if FPopupIndicator then
      R.Right := R.Right - GetIndicatorWidth;

    if BottomIconAlign = biLeft then
    begin
      lf := FFixedBtnMargin + abs({(FAdvNavBarPanel.Count -1- FDisplayCaptionTabsCount)} FixedBtnMaxIndex - index) * FixedBtnWidth;
    end
    else // BottomIconAlign = biRight
    begin
      lf := R.Right - FFixedBtnMargin - FixedBtnWidth;
      lf := lf - abs({(FAdvNavBarPanel.Count -1- FDisplayCaptionTabsCount)} FixedBtnMaxIndex - index) * FixedBtnWidth;
    end;
    Result := Rect(lf, R.Top, lf + FixedBtnWidth, R.Bottom);
  end;

end;

procedure TAdvNavBar.FontChanged(sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvNavBar.GetFixedCaptionTabHeight: Integer;
begin
  if ShowShortCutBar then
    Result := CaptionTabHeight
  else
    Result := 0;
end;

function TAdvNavBar.GetFixedTabButtonCount: integer;
var
  R: TRect;
  wd: integer;
begin
  R := GetFixedTabRect;
  if FPopupIndicator then
    R.Right := R.Right - GetIndicatorWidth - 1;
  wd := R.Right - R.Left;
  Result := (wd - FFixedBtnMargin * 2) div FixedBtnWidth;
end;

//------------------------------------------------------------------------------

function TAdvNavBar.GetFixedTabButtonMaxCount: integer;
begin
  if FScroller.Visible then
    FFixedBtnMargin := FFixedBtnMargin - ScrollerWidth;
  Result := GetFixedTabButtonCount;
  if FScroller.Visible then
    FFixedBtnMargin := FFixedBtnMargin + ScrollerWidth;

end;

//------------------------------------------------------------------------------

function TAdvNavBar.IndexOfBtnAt(X, Y: integer): integer;
var
  i, j, k: integer;
  P: TPoint;
begin
  Result := -1;
  P := Point(X, Y);
  if (y >= GetFixedTabRect.top) and (y <= GetFixedTabRect.Bottom) then
  begin
    //j:= max((FAdvNavBarPanel.Count - GetFixedTabButtonCount), 0);
    k := FixedBtnMaxIndex; //(FAdvNavBarPanel.Count -1- FDisplayCaptionTabsCount);
    j := FixedBtnMinIndex; //max(k+1-GetFixedTabButtonCount,0);

    for i := k downto j do
    begin
      if PtInRect(FixedTabButtonRect(i), P) then
      begin
        Result := i;
        break;
      end;
    end;
  end;

  Result := GetDupIndex(Result);
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.RefreshCaptionTabOrButton(index: integer);
var
  j, k: integer;
begin
  //j:= max((FAdvNavBarPanel.Count - GetFixedTabButtonCount), 0);

  k := FixedBtnMaxIndex; //(FAdvNavBarPanel.Count -1- FDisplayCaptionTabsCount);
  j := FixedBtnMinIndex; //max(k+1-GetFixedTabButtonCount,0);

  if (index <= k) and (index >= j) then
  begin
    DrawFixedTabButton(Index);
  end
  else
//  if (index >= (FAdvNavBarPanel.Count - FDisplayCaptionTabsCount)) and (index <= FAdvNavBarPanel.Count - 1) then
  begin
    if index <> ActiveTabIndex then
//      DrawFixedTab;
      DrawCaptionTab(index);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetBottomIconAlign(const Value: TBottomIconAlign);
begin
  if Value <> FBottomIconAlign then
  begin
    FBottomIconAlign := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetSplitterPosition(const Value: integer);
var
  i: integer;
begin
  for I := 0 to FAdvNavBarPanel.Count - 1 do
  begin
    TAdvNavBarPanel(FAdvNavBarPanel[i]).AdvNavBar := Self;
  end;

  if (Value >= 0) then
  begin
    if ((csDesigning in ComponentState) and not (csLoading in ComponentState)) or FPropertiesLoaded then
    begin
      i := FSplitterPosition - Value;
      MoveSplitterInTabs(i);
    end
    else
    if (csLoading in ComponentState) and not FPropertiesLoaded then
    begin
      FTempSplitterPos := Value;
    end;
  end;

  if not (csDesigning in ComponentState) then
    UpdateScroller;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.Loaded;
var
  {i,} j, MinV, MinIdex: integer;
begin
  inherited;

  FOriginalHint := Hint;
  FPropertiesLoaded := true;
  FSplitterInternalCall := true;

  while FTempAdvNavBarPanel.Count > 0 do
  begin
    MinIdex := 0;
    MinV := TAdvNavBarPanel(FTempAdvNavBarPanel[0]).PanelIndex;
    for j := 0 to FTempAdvNavBarPanel.Count - 1 do
    begin
      if TAdvNavBarPanel(FTempAdvNavBarPanel[j]).PanelIndex < MinV then
      begin
        Minv := TAdvNavBarPanel(FTempAdvNavBarPanel[j]).PanelIndex;
        MinIdex := j;
      end;
    end;
    {i:= } FAdvNavBarPanel.Add(TAdvNavBarPanel(FTempAdvNavBarPanel[MinIdex]));
    TAdvNavBarPanel(FTempAdvNavBarPanel[MinIdex]).FAdvNavBar := Self;

//    MoveSplitterInTabs(-1);
    SetAdvNavBarPanelPosition(TAdvNavBarPanel(FTempAdvNavBarPanel[MinIdex]));
    FTempAdvNavBarPanel.Delete(MinIdex);
    //ActiveTabIndex:= i;
  end;

  FDupAdvNavBarPanel.Clear;
  for j := 0 to FAdvNavBarPanel.Count - 1 do
  begin
    FDupAdvNavBarPanel.Add(FAdvNavBarPanel[j]);
  end;

  //ActiveTabIndex := FAdvNavBarPanel.Count - 1;
  FInternalCall := true;
  ActiveTabIndex := FActiveTabIndex;
  if (FActiveTabIndex >= 0) and (FActiveTabIndex <= FAdvNavBarPanel.Count - 1) then
    FActivePanel := TAdvNavBarPanel(FAdvNavBarPanel[FActiveTabIndex]);

  FInternalCall := false;

  if (FSplitterPosition <> FTempSplitterPos) {and not(csDesigning in ComponentState) } then
  begin
    SplitterPosition := FTempSplitterPos;
  end;

  FSplitterPosition := FTempSplitterPos;

  FSplitterInternalCall := false;

  if AutoThemeAdapt and not (csDesigning in ComponentState) then
    ThemeAdapt;

  UpdateScroller;

  FOldSplitterPosForSizeChange := -1;




  FIsCollapsing := False; //Should be OK to enable now.

  if (Width <= FCollapsedWidth) and AllowCollaps then
    SetCollapsed(True);

  if (Width <= CollapsedWidth) then
    FPreCollapsedWidth := 200;



  if ActiveTabIndex >= 0 then
    ShowAdvNavBarPanel(ActiveTabIndex); //Needs to be updated to overcome collapse init
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.HideScrollButtons;
begin
  FScroller.Visible := false;
  FFixedBtnMargin := FFixedBtnMargin - ScrollerWidth;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.ShowScrollButtons;
begin
  FScroller.Visible := not FCollapsed;
  if FScroller.Visible then begin
     FFixedBtnMargin := FFixedBtnMargin + ScrollerWidth;
     FScroller.Min := 0;
     FScroller.Position := 0;
     FScroller.Max := (FAdvNavBarPanel.Count - FDisplayCaptionTabsCount) - GetFixedTabButtonCount;
  end;
end;

function TAdvNavBar.SplitterHeight: integer;
begin
  if ShowSplitter then
    Result := DefSplitterHeight
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.ScrollLeftBtnClick;
begin
  if BottomIconAlign = biLeft then
  begin
    if FScroller.CanGoBack then
    begin
      FScroller.Position := FScroller.Position - 1;
      DrawAllFixedTabButtons;
    end;
  end
  else //BottomIconAlign = biRight then
  begin
    if FScroller.CanGoForward then
    begin
      FScroller.Position := FScroller.Position + 1;
      DrawAllFixedTabButtons;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.ScrollRightBtnClick;
begin
  if BottomIconAlign = biLeft then
  begin
    if FScroller.CanGoForward then
    begin
      FScroller.Position := FScroller.Position + 1;
      invalidate;
    end;
  end
  else //BottomIconAlign = biRight then
  begin
    if FScroller.CanGoBack then
    begin
      FScroller.Position := FScroller.Position - 1;
      invalidate;
    end;
  end;
end;

procedure TAdvNavBar.WndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_THEMECHANGED) and AutoThemeAdapt then
  begin
    ThemeAdapt;
  end;

  if (Message.Msg = CM_SYSFONTCHANGED) and AutoThemeAdapt then
  begin
    ThemeAdapt;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.UpdateList;
var
  i, k, OldCount: integer;
  APanel: TAdvNavBarPanel;
  index: Integer;
begin
  index := 0;
  OldCount := FAdvNavBarPanel.Count;
  for i := 0 to FDupAdvNavBarPanel.Count - 1 do
  begin
    if not TAdvNavBarPanel(FDupAdvNavBarPanel[i]).TabVisible then
    begin
      k := FAdvNavBarPanel.IndexOf(FDupAdvNavBarPanel[i]);
      APanel := nil;
      if k >= 0 then
      begin // TabVisible = false
        if (FActiveTabIndex = k) then
        begin
          APanel := FindNextPanel(FActivePanel, true);
        end
        else if (FActiveTabIndex > k) then
        begin
          FActiveTabIndex := FActiveTabIndex - 1;
        end
        else // FActiveTabIndex < k
        begin

        end;

        TAdvNavBarPanel(FDupAdvNavBarPanel[i]).Visible := False;
        FAdvNavBarPanel.Remove(FDupAdvNavBarPanel[i]);
        if (FAdvNavBarPanel.Count >= 1) and (APanel <> nil) then
          ActivePanel := APanel;
      end;
    end
    else
    begin // TabVisible = true
      if FAdvNavBarPanel.IndexOf(FDupAdvNavBarPanel[i]) < 0 then
      begin
        //FAdvNavBarPanel.Add(FDupAdvNavBarPanel[i]);
        FAdvNavBarPanel.Insert(index, FDupAdvNavBarPanel[i]);
        ActivePanel := TAdvNavBarPanel(FDupAdvNavBarPanel[i]);
      end;
      inc(Index);
    end;
  end;

  if FAdvNavBarPanel.Count = 0 then
  begin
    ActiveTabIndex := -1;
  end;

  OldCount := OldCount - FAdvNavBarPanel.Count;
  //if FDefaultTabPosition = tpTop then
    //OldCount:= -OldCount;
  if OldCount <> 0 then
    MoveSplitterInTabs(OldCount);

  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  cw: integer;
  nsh,ns: integer;

begin
  inherited;

  if not (HandleAllocated) then
    Exit;

  if (FAdvNavBarPanel.Count > 0) then
  begin
    cw := MinClientHeight + GetCaptionHeight + (SplitterPosition + 1) * FixedBtnWidth + SplitterHeight;

    if (AHeight < cw) then
    begin
      if AHeight - MinClientHeight - GetCaptionHeight - FixedBtnWidth - SplitterHeight > 0 then
      begin
        nsh := AHeight - MinClientHeight - GetCaptionHeight - FixedBtnWidth - SplitterHeight;
        ns := trunc(nsh / FixedBtnWidth);
        FOldSplitterPosForSizeChange := Max(FOldSplitterPosForSizeChange, splitterposition);
        FInternalSplitterMove := True;
        splitterposition := ns;
        FInternalSplitterMove := False;
      end;
    end
    else
    begin
      if (FOldSplitterPosForSizeChange > 0) and not (csDesigning in ComponentState) and not (csLoading in ComponentState) and not FCheckForSplitterMove and not FMouseCaptured then
      begin
        if AHeight - MinClientHeight - GetCaptionHeight - FixedBtnWidth - SplitterHeight > 0 then
        begin
          nsh := AHeight - MinClientHeight - GetCaptionHeight - FixedBtnWidth - SplitterHeight;
          ns := trunc(nsh / FixedBtnWidth);
          if (ns <> splitterposition) then
          begin
            if (ns >= FOldSplitterPosForSizeChange) then
              ns := FOldSplitterPosForSizeChange;
            FInternalSplitterMove := True;
            SetAllAdvNavBarPanelPosition;
            splitterposition := ns;
            FInternalSplitterMove := False;
            if (splitterposition >= FOldSplitterPosForSizeChange) then
              FOldSplitterPosForSizeChange := -1;
          end;
        end;
      end;
    end;
  end;

  SetAllAdvNavBarPanelPosition;
  UpdateScroller;


  (*
  Exit;

  if (AHeight < MinClientHeight) then
  begin
    if (SplitterPosition > 0) then
    begin
      if (FOldSplitterPosForSizeChange < 0) and not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
        FOldSplitterPosForSizeChange := SplitterPosition;

      frm := GetParentForm(self);
      {
      if Assigned(frm) and (frm.WindowState = wsMaximized) then
      else
      begin
        SplitterPosition := SplitterPosition - 1;
      end;
      }
      SplitterPosition := MinClientHeight div (CaptionTabHeight + 10);

    end;
  end
  else
    if (AHeight > (MinClientHeight + CaptionTabHeight)) and (SplitterPosition < FAdvNavBarPanel.Count)
      and (FOldSplitterPosForSizeChange > 0) and not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    SplitterPosition := SplitterPosition + 1;
    if SplitterPosition >= FOldSplitterPosForSizeChange then
      FOldSplitterPosForSizeChange := -1;
  end;

  SetAllAdvNavBarPanelPosition;
  UpdateScroller;

  Exit;

  if (FPropertiesLoaded) then
  begin
    if FSeparatorLine < CaptionHeight + FMinClientHeight then
    begin
      if MoveSplitterInTabs(1) <= 0 then
        inherited SetBounds(aLeft, ATop, AWidth, CaptionHeight + FMinClientHeight + SplitterHeight + CaptionTabHeight);
    end;
  end
  else
  begin if ((csDesigning in ComponentState) and (FPropertiesCreated)) then
      if (FAdvNavBarPanel.Count > 0) then
        if FSeparatorLine < CaptionHeight + FMinClientHeight then
        begin
          if MoveSplitterInTabs(1) <= 0 then
            inherited SetBounds(aLeft, ATop, AWidth, CaptionHeight + FMinClientHeight + SplitterHeight + CaptionTabHeight);
        end;
  end;
  *)
end;


//------------------------------------------------------------------------------

procedure TAdvNavBar.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TAdvNavBar.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetCaptionFont(const Value: TFont);
begin
  FCaptionFont.Assign(Value);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetSectionFont(const Value: TFont);
begin
  FSectionFont.Assign(Value);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetSplitterBorderColor(const Value: TColor);
begin
  if FSplitterBorderColor <> Value then
  begin
    FSplitterBorderColor := Value;
    Invalidate;
  end;
end;

procedure TAdvNavBar.SetSplitterColor(const Value: TColor);
begin
  if (FSplitterColor <> Value) then
  begin
    FSplitterColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetSplitterColorTo(const Value: TColor);
begin
  if (FSplitterColorTo <> Value) then
  begin
    FSplitterColorTo := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetComponentStyle(AStyle: TTMSStyle);
begin
  Style := TAdvNavBarStyle(AStyle);
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetStyle(const Value: TAdvNavBarStyle);
var
  I: Integer;
begin
  if (FStyle <> Value) then
  begin
    FStyle := Value;
    SplitterBorderColor := clNone;
    Color := clWhite;
    case FStyle of
      esOffice2003Blue:
        begin
          CaptionColor := $D68759;
          CaptionColorTo := $933803;

          CaptionFont.Color := clWhite;
          Font.Color := clBlack;

          ActiveColor := $94E6FB;
          ActiveColorTo := $1595EE;

          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := clWindowText;

          DefaultTabColor := $FCE1CB;
          DefaultTabColorTo := $E0A57D;

          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := $087FE8;
          DownTabColorTo := $7CDAF7;

          DownTextColor := clBlack;
          HoverTextColor := clBlack;

          HoverTabColor := $DCFFFF;
          HoverTabColorTo := $5BC0F7;

          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          SplitterColor := $D68759;
          SplitterColorTo := $962D00;

          BorderColor := $962D00;

          SectionColor := $FADAC4;
          SectionColorTo := $F5BFA0;

          CollapsedHoverColor := clInfoBk;
          CollapsedHoverColorTo := clNone;
          CollapsedDownColor := clSilver;
          CollapsedDownColorTo := clNone;

          DefaultGradientDirection := gdVertical;
        end;
      esOffice2003Olive:
        begin
          CaptionColor := $82C0AF;
          CaptionColorTo := $447A63;

          CaptionFont.Color := clWhite;
          Font.Color := clBlack;

          ActiveColor := $94E6FB;
          ActiveColorTo := $1595EE;

          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := clWindowText;

          DefaultTabColor := $CFF0EA;
          DefaultTabColorTo := $8CC0B1;

          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := $087FE8;
          DownTabColorTo := $7CDAF7;

          DownTextColor := clBlack;
          HoverTextColor := clBlack;

          HoverTabColor := $DCFFFF;
          HoverTabColorTo := $5BC0F7;

          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          CollapsedHoverColor := clInfoBk;
          CollapsedHoverColorTo := clNone;
          CollapsedDownColor := clSilver;
          CollapsedDownColorTo := clNone;

          SplitterColor := $6F8E78;
          SplitterColorTo := $588060;

          BorderColor := $588060;

          SectionColor := $E4F1F2;
          SectionColorTo := $AADADA;

          DefaultGradientDirection := gdVertical;
        end;
      esOffice2003Silver:
        begin
          CaptionColor := $BDA4A5;
          CaptionColorTo := $957475;

          CaptionFont.Color := clWhite;
          Font.Color := clBlack;

          ActiveColor := $94E6FB;
          ActiveColorTo := $1595EE;

          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := clWindowText;

          DefaultTabColor := $ECE2E1;
          DefaultTabColorTo := $B39698;

          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := $087FE8;
          DownTabColorTo := $7CDAF7;

          DownTextColor := clBlack;
          HoverTextColor := clBlack;

          HoverTabColor := $DCFFFF;
          HoverTabColorTo := $5BC0F7;

          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          CollapsedHoverColor := clInfoBk;
          CollapsedHoverColorTo := clNone;
          CollapsedDownColor := clSilver;
          CollapsedDownColorTo := clNone;

          SplitterColor := $BFA7A8;
          SplitterColorTo := $947C7C;

          BorderColor := $947C7C;

          SectionColor := $F7F3F3;
          SectionColorTo := $E6D8D8;

          DefaultGradientDirection := gdVertical;
        end;
      esOffice2003Classic:
        begin
          CaptionColor := $808080;
          CaptionColorTo := $808080;

          CaptionFont.Color := clWhite;
          Font.Color := clBlack;

          ActiveColor := $D8D5D4;
          ActiveColorTo := $D8D5D4;

          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := clWindowText;

          DefaultTabColor := clWhite;
          DefaultTabColorTo := $C9D1D5;

          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := $B59285;
          DownTabColorTo := $B59285;

          DownTextColor := clWhite;
          HoverTextColor := clBlack;

          HoverTabColor := $D2BDB6;
          HoverTabColorTo := $D2BDB6;

          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          CollapsedHoverColor := clInfoBk;
          CollapsedHoverColorTo := clNone;
          CollapsedDownColor := clSilver;
          CollapsedDownColorTo := clNone;

          SplitterColor := $C8D0D4;
          SplitterColorTo := $8C8D8E;

          BorderColor := $808080;

          SectionColor := $F4F5F6;
          SectionColorTo := $CAD2D6;

          DefaultGradientDirection := gdVertical;
        end;
     esOffice2007Luna:
        begin
          CaptionColor := $FFEFE3;
          CaptionColorTo := $FFD2AF;

          CaptionFont.Color := $723708;
          Font.Color := $723708;

          ActiveColor := $AAD9FF;
          ActiveColorTo := $6EBBFF;

          ActiveMirrorColor := $42AEFE;
          ActiveMirrorColorTo := $7AE1FE;
          ActiveTextColor := clWindowText;

          DefaultTabColor := $FFEFE3;
          DefaultTabColorTo := $FFDDC4;

          DefaultTabMirrorColor := $FFD1AD;
          DefaultTabMirrorColorTo := $FFDBC0;

          DownTabColor := $76AFF1;
          DownTabColorTo := $4190F3;

          DownTextColor := clBlack;
          HoverTextColor := $723708;

          HoverTabColor := $EBFDFF;
          HoverTabColorTo := $ACECFF;

          HoverTabMirrorColor := $59DAFF;
          HoverTabMirrorColorTo := $A4E9FF;

          DownTabMirrorColor := $0E72F1;
          DownTabMirrorColorTo := $4C9FFD;

          CollapsedHoverColor := $7AE1FE;
          CollapsedHoverColorTo := clNone;
          CollapsedDownColor := $4C9FFD;
          CollapsedDownColorTo := clNone;

          SplitterColor := $FFEFE3;
          SplitterColorTo := $FFD2AF;

          BorderColor := $FFD2AF;

          SectionColor := $FFECDD;
          SectionColorTo := $FFECDD;

          DefaultGradientDirection := gdVertical;
        end;
     esOffice2007Obsidian:
        begin
          CaptionColor := $F2F1F0;
          CaptionColorTo := $C9C2BD;

          CaptionFont.Color := $433C37;
          Font.Color := clBlack;

          ActiveColor := $AAD9FF;
          ActiveColorTo := $6EBBFF;

          ActiveMirrorColor := $42AEFE;
          ActiveMirrorColorTo := $7AE1FE;
          ActiveTextColor := clWindowText;

          DefaultTabColor := $F9F8F8;
          DefaultTabColorTo := $E4E2DF;

          DefaultTabMirrorColor := $D1CBC7;
          DefaultTabMirrorColorTo := $E2DEDB;

          DownTabColor := $76AFF1;
          DownTabColorTo := $4190F3;

          DownTextColor := clBlack;
          HoverTextColor := $433C37;

          HoverTabColor := $EBFDFF;
          HoverTabColorTo := $ACECFF;

          HoverTabMirrorColor := $59DAFF;
          HoverTabMirrorColorTo := $A4E9FF;

          DownTabMirrorColor := $0E72F1;
          DownTabMirrorColorTo := $4C9FFD;

          CollapsedHoverColor := $7AE1FE;
          CollapsedHoverColorTo := clNone;
          CollapsedDownColor := $4C9FFD;
          CollapsedDownColorTo := clNone;
          

          SplitterColor := $F2F1F0;
          SplitterColorTo := $C9C2BD;

          BorderColor := $5C534C;

          SectionColor := $E4E1DE;
          SectionColorTo := $F0EFEE;

          DefaultGradientDirection := gdVertical;
        end;
    esOffice2007Silver:
        begin
          CaptionColor := $F8F7F6;
          CaptionColorTo := $E8E0DB;

          CaptionFont.Color := $8B4215;
          Font.Color := clBlack;

          ActiveColor := $AAD9FF;
          ActiveColorTo := $6EBBFF;

          ActiveMirrorColor := $42AEFE;
          ActiveMirrorColorTo := $7AE1FE;
          ActiveTextColor := clWindowText;

          DefaultTabColor := $FAEEEB;
          DefaultTabColorTo := $E5DBD7;

          DefaultTabMirrorColor := $E2D8D4;
          DefaultTabMirrorColorTo := $D1C7C5;

          DownTabColor := $76AFF1;
          DownTabColorTo := $4190F3;

          DownTextColor := clBlack;
          HoverTextColor := $5C5371;

          HoverTabColor := $EBFDFF;
          HoverTabColorTo := $ACECFF;

          HoverTabMirrorColor := $59DAFF;
          HoverTabMirrorColorTo := $A4E9FF;

          DownTabMirrorColor := $0E72F1;
          DownTabMirrorColorTo := $4C9FFD;

          CollapsedHoverColor := $7AE1FE;
          CollapsedHoverColorTo := clNone;
          CollapsedDownColor := $4C9FFD;
          CollapsedDownColorTo := clNone;

          SplitterColor := $BFA7A8;
          SplitterColorTo := $977778;

          BorderColor := $74706F;

          SectionColor := $E7DBD5;
          SectionColorTo := $F5F5F3;

          DefaultGradientDirection := gdVertical;
        end;    
    esWindowsXP:
        begin
          CaptionColor := clBtnFace;
          CaptionColorTo := clBtnFace;

          CaptionFont.Color := clBlack;
          Font.Color := clBlack;

          ActiveColor := clInactiveCaption;
          ActiveColorTo := clInactiveCaption;

          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := clWindowText;

          DefaultTabColor := clWhite;
          DefaultTabColorTo := clBtnFace;

          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := clHighlight;
          DownTabColorTo := clHighlight;

          DownTextColor := clWhite;
          HoverTextColor := clBlack;

          HoverTabColor := clBtnFace;
          HoverTabColorTo := clBtnFace;

          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          CollapsedHoverColor := clInfoBk;
          CollapsedHoverColorTo := clNone;
          CollapsedDownColor := clSilver;
          CollapsedDownColorTo := clNone;

          SplitterColor := clBtnFace;
          SplitterColorTo := clBtnFace;

          BorderColor := clBlack;

          SectionColor := clBtnFace;
          SectionColorTo := clBtnFace;

          DefaultGradientDirection := gdVertical;
        end;
        esWindowsVista:
        begin
          CaptionColor := $FDF8F1;
          CaptionColorTo := $FCEFD5;

          CaptionFont.Color := clBlack;
          Font.Color := clBlack;

          ActiveColor := $FFFDF9;
          ActiveColorTo := $FFFAF0;

          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := clWindowText;

          DefaultTabColor := $FFFFFF;
          DefaultTabColorTo := $FFFFFF;

          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := $FEF9F0;
          DownTabColorTo := $FDF0D7;

          DownTextColor := clBlack;
          HoverTextColor := clBlack;

          HoverTabColor := $FFFDF9;
          HoverTabColorTo := $FFFAF0;

          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          CollapsedHoverColor := clInfoBk; //define
          CollapsedHoverColorTo := clNone; //define
          CollapsedDownColor := clSilver;  //define
          CollapsedDownColorTo := clNone;  //define

          SplitterColor := $FDF8F1;
          SplitterColorTo := $FCEFD5;

          BorderColor := $FDDE99;

          SectionColor := $FFFDF9;
          SectionColorTo := $FFFAF0; 

          DefaultGradientDirection := gdVertical;
        end;
        esWindows7:
        begin
          CaptionColor := $FCEBDC;
          CaptionColorTo := $FCDBC1;

          CaptionFont.Color := clBlack;
          Font.Color := clBlack;

          ActiveColor := $FDFBFA;
          ActiveColorTo := $FDF3EB;

          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := clWindowText;

          DefaultTabColor := $FFFFFF;
          DefaultTabColorTo := $FFFFFF;

          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := $FCEBDC;
          DownTabColorTo := $FCDBC1;

          DownTextColor := clBlack;
          HoverTextColor := clBlack;

          HoverTabColor := $FDFBFA;
          HoverTabColorTo := $FDF3EB;

          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          CollapsedHoverColor := clInfoBk;  //define
          CollapsedHoverColorTo := clNone;//define
          CollapsedDownColor := clSilver;  //define
          CollapsedDownColorTo := clNone; //define

          SplitterColor := $FCEBDC;
          SplitterColorTo := $FCDBC1;

          BorderColor := $CEA27D;

          SectionColor := $FDFBFA;
          SectionColorTo := $FDF3EB;

          DefaultGradientDirection := gdVertical;
        end;
        esTerminal:
        begin
          CaptionColor := clBtnFace;
          CaptionColorTo := clBtnFace;

          CaptionFont.Color := clBlack;
          Font.Color := clBlack;

          ActiveColor := clSilver;
          ActiveColorTo := clSilver;

          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := clWindowText;

          DefaultTabColor := clWhite;
          DefaultTabColorTo := clWhite;

          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := clHighlight;
          DownTabColorTo := clHighlight;

          DownTextColor := clWhite;
          HoverTextColor := clBlack;

          HoverTabColor := clBtnFace;
          HoverTabColorTo := clBtnFace;

          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          CollapsedHoverColor := clGray;    //define
          CollapsedHoverColorTo := clNone;  //define
          CollapsedDownColor := clSilver;   //define
          CollapsedDownColorTo := clNone;   //define

          SplitterColor := clBtnFace;
          SplitterColorTo := clBtnFace;

          BorderColor := clGray;

          SectionColor := clBtnFace;
          SectionColorTo := clBtnFace;

          DefaultGradientDirection := gdVertical;
        end;
        esOffice2010Blue:
        begin
          Color := $E9D6C5;
          CaptionColor := $E9D6C5;
          CaptionColorTo := $E9D6C5;

          CaptionFont.Color := clBlack;
          Font.Color := clBlack;

          ActiveColor := $EFDBC8;
          ActiveColorTo := $EFDBC8;
          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := clWindowText;

          DefaultTabColor := $EFDBC8;
          DefaultTabColorTo := $EFDBC8;
          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := $D6B79F;
          DownTabColorTo := $D6B79F;
          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          DownTextColor := clBlack;
          HoverTextColor := clBlack;

          HoverTabColor := $F7E6D5;
          HoverTabColorTo := $F7E6D5;
          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          CollapsedHoverColor := $F8E9DB;
          CollapsedHoverColorTo := $F2DCC4;
          CollapsedDownColor := $D6B79F;
          CollapsedDownColorTo := $D6B79F;

          SplitterColor := $FFEBDB;
          SplitterColorTo := $EFDAC8;
          SplitterBorderColor := $BF9E85;

          BorderColor := SplitterBorderColor;

          SectionColor := $FFEEE2;
          SectionColorTo := $FFEEE2;

          DefaultGradientDirection := gdVertical;
        end;
        esOffice2010Silver:
        begin
          Color := $E7E2DE;
          CaptionColor := $E7E2DE;
          CaptionColorTo := $E7E2DE;

          CaptionFont.Color := clBlack;
          Font.Color := clBlack;

          ActiveColor := $EAE3DE;
          ActiveColorTo := $EAE3DE;

          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := clWindowText;

          DefaultTabColor := $EAE3DE;
          DefaultTabColorTo := $EAE3DE;

          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := $CFC7C0;
          DownTabColorTo := $CFC7C0;
          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          DownTextColor := clBlack;
          HoverTextColor := clBlack;

          HoverTabColor := $F1ECE7;
          HoverTabColorTo := $F1ECE7;
          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          CollapsedHoverColor := $F1ECE7;
          CollapsedHoverColorTo := $F1ECE7;
          CollapsedDownColor := $CFC7C0;
          CollapsedDownColorTo := $CFC7C0;

          SplitterColor := $F7F3F0;
          SplitterColorTo := $E3DED9;
          SplitterBorderColor := $B2ABA4;

          BorderColor := SplitterBorderColor;

          SectionColor := $F0EBE8;
          SectionColorTo := $F0EBE8;

          DefaultGradientDirection := gdVertical;
        end;
        esOffice2010Black:
        begin
          Color := $676767;
          CaptionColor := $676767;
          CaptionColorTo := $676767;

          CaptionFont.Color := clWhite;
          Font.Color := clWhite;

          ActiveColor := $6C6C6C;
          ActiveColorTo := $6C6C6C;

          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := clWindowText;

          DefaultTabColor := $6C6C6C;
          DefaultTabColorTo := $6C6C6C;

          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := $484848;
          DownTabColorTo := $484848;
          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          DownTextColor := clWhite;
          HoverTextColor := clWhite;
          DefaultTextColor := clWhite;


          HoverTabColor := $777777;
          HoverTabColorTo := $777777;
          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          CollapsedHoverColor := $707070;
          CollapsedHoverColorTo := $595959;
          CollapsedDownColor := $484848;
          CollapsedDownColorTo := $484848;

          SplitterColor := $666666;
          SplitterColorTo := $515151;
          SplitterBorderColor := $303030;

          BorderColor := SplitterBorderColor; //$D1CBC7;

          SectionColor := $ECEAE9;
          SectionColorTo := $ECEAE9;

          DefaultGradientDirection := gdVertical;
        end;
      esWhidbey:
        begin
          CaptionColor := $EBEEEF;
          CaptionColorTo := $7E9898;

          CaptionFont.Color := clWhite;
          Font.Color := clBlack;

          ActiveColor := $94E6FB;
          ActiveColorTo := $1595EE;

          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := clWindowText;

          DefaultTabColor := $F5F9FA;
          DefaultTabColorTo := $A8C0C0;

          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := $087FE8;
          DownTabColorTo := $7CDAF7;

          DownTextColor := clBlack;
          HoverTextColor := clBlack;

          HoverTabColor := $DCFFFF;
          HoverTabColorTo := $5BC0F7;

          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          CollapsedHoverColor := clInfoBk;
          CollapsedHoverColorTo := clNone;
          CollapsedDownColor := clSilver;
          CollapsedDownColorTo := clNone;

          SplitterColor := $EBEEEF;
          SplitterColorTo := $7E9898;

          BorderColor := $962D00;

          SectionColor := $EBEEEF;
          SectionColorTo := $A8C0C0;

          DefaultGradientDirection := gdVertical;

          AutoThemeAdapt := false;
        end;
      esWindows8, esWindows10:
        begin
          Color := clWhite;
          CaptionColor := $F7F6F5;
          CaptionColorTo := $F7F6F5;

          CaptionFont.Color := clBlack;
          Font.Color := clBlack;

          ActiveColor := $F7E0C9;
          ActiveColorTo := $F7E0C9;

          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := clBlack;

          DefaultTabColor := $F7F6F5;
          DefaultTabColorTo := $F7F6F5;

          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := $DAA026;
          DownTabColorTo := $DAA026;
          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          DownTextColor := clWhite;
          HoverTextColor := clBlack;
          DefaultTextColor := clBlack;


          HoverTabColor := $F7EFE8;
          HoverTabColorTo := $F7EFE8;
          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          CollapsedHoverColor := $F7EFE8;
          CollapsedHoverColorTo := $F7EFE8;
          CollapsedDownColor := clNone;
          CollapsedDownColorTo := clNone;

          SplitterColor := $E4E3E2;
          SplitterColorTo := $E4E3E2;
          SplitterBorderColor := clNone;


          BorderColor := $E4E3E2;

          SectionColor := $EEEEEE;
          SectionColorTo := $EEEEEE;

          DefaultGradientDirection := gdVertical;
        end;
      esOffice2013White:
        begin
             Color := clWhite;
          CaptionColor := clWhite;
          CaptionColorTo := clWhite;

          CaptionFont.Color := clBlack;
          Font.Color := clBlack;

          ActiveColor := $FCE2C8;
          ActiveColorTo := $FCE2C8;

          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := clBlack;

          DefaultTabColor := clWhite;
          DefaultTabColorTo := clWhite;

          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := $FF9933;
          DownTabColorTo := $FF9933;
          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          DownTextColor := clWhite;
          HoverTextColor := clBlack;
          DefaultTextColor := clBlack;


          HoverTabColor := $FCF0E4;
          HoverTabColorTo := $FCF0E4;
          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          CollapsedHoverColor := $FCF0E4;
          CollapsedHoverColorTo := $FCF0E4;
          CollapsedDownColor := clNone;
          CollapsedDownColorTo := clNone;

          SplitterColor := $D4D4D4;
          SplitterColorTo := $D4D4D4;
          SplitterBorderColor := clNone;


          BorderColor := $D4D4D4;

          SectionColor := $EEEEEE;
          SectionColorTo := $EEEEEE;

          DefaultGradientDirection := gdVertical;
        end;

      esOffice2013LightGray:
        begin
          Color := clWhite;
          CaptionColor := $F6F6F6;
          CaptionColorTo := $F6F6F6;

          CaptionFont.Color := clBlack;
          Font.Color := clBlack;

          ActiveColor := $FCE2C8;
          ActiveColorTo := $FCE2C8;

          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := clBlack;

          DefaultTabColor := $F6F6F6;
          DefaultTabColorTo := $F6F6F6;

          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := $FF9933;
          DownTabColorTo := $FF9933;
          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          DownTextColor := clWhite;
          HoverTextColor := clBlack;
          DefaultTextColor := clBlack;


          HoverTabColor := $FCF0E4;
          HoverTabColorTo := $FCF0E4;
          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          CollapsedHoverColor := $FCF0E4;
          CollapsedHoverColorTo := $FCF0E4;
          CollapsedDownColor := clNone;
          CollapsedDownColorTo := clNone;

          SplitterColor := $C6C6C6;
          SplitterColorTo := $C6C6C6;
          SplitterBorderColor := clNone;


          BorderColor := $C6C6C6;

          SectionColor := $F6F6F6;
          SectionColorTo := $F6F6F6;

          DefaultGradientDirection := gdVertical;
       end;
      esOffice2013Gray:
        begin
          Color := clWhite;
          CaptionColor := $E5E5E5;
          CaptionColorTo := $E5E5E5;

          CaptionFont.Color := clBlack;
          Font.Color := clBlack;

          ActiveColor := $FCE2C8;
          ActiveColorTo := $FCE2C8;

          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := clBlack;

          DefaultTabColor := $E5E5E5;
          DefaultTabColorTo := $E5E5E5;

          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := $FF9933;
          DownTabColorTo := $FF9933;
          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          DownTextColor := clWhite;
          HoverTextColor := clBlack;
          DefaultTextColor := clBlack;


          HoverTabColor := $FCF0E4;
          HoverTabColorTo := $FCF0E4;
          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          CollapsedHoverColor := $FCF0E4;
          CollapsedHoverColorTo := $FCF0E4;
          CollapsedDownColor := clNone;
          CollapsedDownColorTo := clNone;

          SplitterColor := $ABABAB;
          SplitterColorTo := $ABABAB;
          SplitterBorderColor := clNone;


          BorderColor := $ABABAB;

          SectionColor := $E5E5E5;
          SectionColorTo := $E5E5E5;

          DefaultGradientDirection := gdVertical;
         end;

      esOffice2016White:
        begin
          Color := clWhite;
          CaptionColor := clWhite;
          CaptionColorTo := clWhite;

          CaptionFont.Color := $444444;
          Font.Color := $444444;

          ActiveColor := $F2E1D5;
          ActiveColorTo := $F2E1D5;

          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := $505050;

          DefaultTabColor := clWhite;
          DefaultTabColorTo := clWhite;

          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := $E3BDA3;
          DownTabColorTo := $E3BDA3;
          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          DownTextColor := $505050;
          HoverTextColor := $505050;
          DefaultTextColor := $505050;


          HoverTabColor := $F2E1D5;
          HoverTabColorTo := $F2E1D5;
          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          CollapsedHoverColor := $F2E1D5;
          CollapsedHoverColorTo := $F2E1D5;
          CollapsedDownColor := clNone;
          CollapsedDownColorTo := clNone;

          SplitterColor := $F0F0F0;
          SplitterColorTo := $F0F0F0;
          SplitterBorderColor := clNone;


          BorderColor := $D4D4D4;

          SectionColor := $D4D4D4;
          SectionColorTo := $D4D4D4;

          DefaultGradientDirection := gdVertical;
        end;

      esOffice2016Gray:
        begin
          Color := clWhite;
          CaptionColor := $444444;
          CaptionColorTo := $444444;

          CaptionFont.Color := $F0F0F0;
          Font.Color := $262626;

          ActiveColor := $F2D5C2;
          ActiveColorTo := $F2D5C2;

          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := $424242;

          DefaultTabColor := $F6F6F6;
          DefaultTabColorTo := $F6F6F6;

          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := $E3BDA3;
          DownTabColorTo := $E3BDA3;
          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          DownTextColor := $424242;
          HoverTextColor := $424242;
          DefaultTextColor := $424242;


          HoverTabColor := $F2E1D5;
          HoverTabColorTo := $F2E1D5;
          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          CollapsedHoverColor := $F2E1D5;
          CollapsedHoverColorTo := $F2E1D5;
          CollapsedDownColor := clNone;
          CollapsedDownColorTo := clNone;

          SplitterColor := $B2B2B2;
          SplitterColorTo := $B2B2B2;
          SplitterBorderColor := clNone;


          BorderColor := $444444;

          SectionColor := $B2B2B2;
          SectionColorTo := $B2B2B2;

          DefaultGradientDirection := gdVertical;
       end;
      esOffice2016Black:
        begin
          Color := clWhite;
          CaptionColor := $444444;
          CaptionColorTo := $444444;

          CaptionFont.Color := $F0F0F0;
          Font.Color := $A6A6A6;

          ActiveColor := $575757;
          ActiveColorTo := $575757;

          ActiveMirrorColor := clNone;
          ActiveMirrorColorTo := clNone;
          ActiveTextColor := $C8C8C8;

          DefaultTabColor := $363636;
          DefaultTabColorTo := $363636;

          DefaultTabMirrorColor := clNone;
          DefaultTabMirrorColorTo := clNone;

          DownTabColor := $444444;
          DownTabColorTo := $444444;
          DownTabMirrorColor := clNone;
          DownTabMirrorColorTo := clNone;

          DownTextColor := $A6A6A6;
          HoverTextColor := $A6A6A6;
          DefaultTextColor := $A6A6A6;


          HoverTabColor := $6A6A6A;
          HoverTabColorTo := $6A6A6A;
          HoverTabMirrorColor := clNone;
          HoverTabMirrorColorTo := clNone;

          CollapsedHoverColor := $6A6A6A;
          CollapsedHoverColorTo := $6A6A6A;
          CollapsedDownColor := clNone;
          CollapsedDownColorTo := clNone;

          SplitterColor := $363636;
          SplitterColorTo := $363636;
          SplitterBorderColor := clNone;


          BorderColor := $444444;

          SectionColor := $575757;
          SectionColorTo := $575757;

          DefaultGradientDirection := gdVertical;
         end;

      esCustom:
        begin
          AutoThemeAdapt := false;
        end;
    end;
  end;


  for I := 0 to PanelCount - 1 do
  begin
    Panels[i].Color := Color;
    Panels[i].TextColor := Font.Color;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetPopupIndicator(const Value: Boolean);
begin
  FPopupIndicator := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvNavBar.GetPanelOrder: TPanelOrder;
begin
  if FReverseOrder then
    Result := poBottomToTop
  else
    Result := poTopToBottom;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetPanelOrder(const Value: TPanelOrder);
begin
  SetReverseOrder(Value = poBottomToTop);
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetReverseOrder(const Value: boolean);
begin
  if FReverseOrder <> Value then
  begin
    FReverseOrder := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetSectionColor(const Value: TColor);
begin
  if (FSectionColor <> Value) then
    FSectionColor := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetSectionColorTo(const Value: TColor);
begin
  if (FSectionColorTo <> Value) then
    FSectionColorTo := Value;
end;

procedure TAdvNavBar.SetShowShortCutBar(const Value: Boolean);
begin
  if FShowShortCutBar <> Value then
  begin
    FShowShortCutBar := Value;
    Width := Width + 1;
    Width := Width - 1;
  end;
end;

procedure TAdvNavBar.SetShowSplitter(const Value: boolean);
begin
  FShowSplitter := Value;
  SetAllAdvNavBarPanelPosition;
  UpdateScroller;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.ThemeAdapt;
var
  eTheme: XPColorScheme;
begin
  eTheme := CurrentXPTheme();
  case eTheme of
    xpBlue: Style := esOffice2003Blue;
    xpGreen: Style := esOffice2003Olive;
    xpGray: Style := esOffice2003Silver;
  else
    Style := esOffice2003Classic;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetDefaultTabPosition(
  const Value: TDefaultTabPosition);
begin
  if Value <> FDefaultTabPosition then
  begin
    FDefaultTabPosition := Value;
    SetAllAdvNavBarPanelPosition;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAdvNavBar.GetDupIndex(Index: Integer): integer;
begin
  Result := Index;
  if (Index >= 0) and not FInternalCall and (Index < FAdvNavBarPanel.Count) then
    Result := FDupAdvNavBarPanel.IndexOf(FAdvNavBarPanel[Index]);
end;

//------------------------------------------------------------------------------

function TAdvNavBar.GetDisPlayIndex(Index: integer): integer;
begin
  Result := Index;
  if (Index >= 0) and not FInternalCall and (Index < FDupAdvNavBarPanel.Count) then
    Result := FAdvNavBarPanel.IndexOf(FDupAdvNavBarPanel[Index]);
end;

//------------------------------------------------------------------------------

function TAdvNavBar.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TAdvNavBar.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvNavBar.CreateWnd;
begin
  inherited;
  if not (csReading in ComponentState) then
  begin
    FPropertiesLoaded := true;
  end;
  SetBounds(Left,Top,Width,Height);
end;


{ TAdvNavBarPanelSection }

procedure TAdvNavBarPanelSection.Assign(Source: TPersistent);
begin
  if (Source is TAdvNavBarPanelSection) then
  begin
    FCaption := (Source as TAdvNavBarPanelSection).Caption;
    FControl := (Source as TAdvNavBarPanelSection).Control;
    FHeight := (Source as TAdvNavBarPanelSection).Height;
    FTag := (Source as TAdvNavBarPanelSection).Tag;
  end;

end;

constructor TAdvNavBarPanelSection.Create(Collection: TCollection);
begin
  inherited;
  FHeight := 48;
  GetOwnerPanel.Invalidate;
end;

destructor TAdvNavBarPanelSection.Destroy;
begin
  GetOwnerpanel.Invalidate;
  inherited;
end;

procedure TAdvNavBarPanelSection.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    GetOwnerPanel.Invalidate;
  end;
end;

procedure TAdvNavBarPanelSection.UpdateControlBounds;
var
  i:Integer;
  ARect:TRect;
  Panel:TAdvNavBarPanel;
begin
  if Assigned(FControl) then
  begin
    //FControl.Anchors:=[akLeft,akRight]; Is this better?

    Panel := GetOwnerPanel;
    ARect := Panel.ClientRect;

    for i := 0 to Index-1 do
    begin
      Inc(ARect.Top,Panel.Sections[i].FHeight);
    end;

    Inc(ARect.Top,SectionHeight + 1); //One more for this section....

    if Index <> Panel.Sections.Count - 1 then
    begin //if NOT the last panel, set bottom, otherwise use ClientRect bottom
      ARect.Bottom := ARect.Top + FHeight - SectionHeight - 1;
    end;

    if not EqualRect(FControl.BoundsRect,ARect) then
    begin
      FControl.BoundsRect := ARect;
      Panel.Invalidate;
    end;
  end;
end;

procedure TAdvNavBarPanelSection.SetControl(const Value:TWinControl);
var
  i:Integer;
  Panel:TAdvnavBarPanel;
begin
  if (FControl <> Value) then
  begin

    if Assigned(Value) then
    begin
      Panel := GetOwnerPanel;

      for i := 0 to Panel.Sections.Count - 1 do
      begin
        //Maybe they set the control to another section in this panel and want to move it...
        //Let's make sure that it's not duped.
        if Panel.Sections[i].Control = Value then
          Panel.Sections[i].Control := nil;
      end;

      //Now that dupe ownership is resolved, let's move the parent to here....
      if Value.Parent <> Panel then
        Value.Parent := Panel;
    end;

    FControl := Value;

    if Assigned(FControl) then
      UpdateControlBounds;
  end;
end;


function TAdvNavBarPanelSection.GetOwnerPanel:TAdvNavBarPanel;
begin
{$IFDEF DELPHI6_LVL}
    Result := TAdvNavBarPanel(Collection.Owner);
{$ELSE}
    Result := TAdvNavBarPanelSections(Collection).FOwner;
{$ENDIF}
end;


procedure TAdvNavBarPanelSection.SetHeight(const Value: Integer);
begin
  if (FHeight <> Value) then
  begin
    FHeight := Value;
    GetOwnerPanel.Invalidate;
  end;
end;

{ TAdvNavBarPanelSections }

function TAdvNavBarPanelSections.Add: TAdvNavBarPanelSection;
begin
  Result := TAdvNavBarPanelSection(inherited Add);
end;

constructor TAdvNavBarPanelSections.Create(AOwner: TAdvNavBarPanel);
begin
  inherited Create(TAdvNavBarPanelSection);
  FOwner := AOwner;
end;

function TAdvNavBarPanelSections.GetItem(
  Index: Integer): TAdvNavBarPanelSection;
begin
  Result := TAdvNavBarPanelSection(inherited Items[Index]);
end;

function TAdvNavBarPanelSections.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvNavBarPanelSections.Insert(
  Index: Integer): TAdvNavBarPanelSection;
begin
  Result := TAdvNavBarPanelSection(inherited Insert(Index));
end;

procedure TAdvNavBarPanelSections.SetItem(Index: Integer;
  const Value: TAdvNavBarPanelSection);
begin
  inherited Items[Index] := Value;
end;


procedure TAdvNavBar.SetDownTextColor(const Value: TColor);
begin
  FDownTextColor := Value;
end;

procedure TAdvNavBar.SetHoverTextColor(const Value: TColor);
begin
  FHoverTextColor := Value;
end;

procedure TAdvNavBar.SetSmallImages(const Value: TCustomImageList);
begin
  if FSmallImages <> Value then
  begin
    FSmallImages := Value;
    Invalidate;
  end;
end;

procedure TAdvNavBar.SetCollapsed(const Value: Boolean);
var
  i: Integer;
begin
  if FIsCollapsing or (Value = FCollapsed) then
    Exit;

  FIsCollapsing := True;
  try
    if Value then
    begin
      if FPreCollapsedWidth > FCollapsedWidth then
      begin
        FPreCollapsedWidth := Width; //SAVE CURRENT WIDTH!!!
      end;

      Width := FCollapsedWidth;

      //Hide all the panels
      for i := 0 to PanelCount - 1 do
      begin
        Panels[i].Visible := False;
        Panels[i].Invalidate;
      end;
    end
    else
    begin
      if FPreCollapsedWidth = 0 then
      begin
        Width := FCollapsedWidth;
      end
      else
      begin
        Width := FPreCollapsedWidth;
      end;

      for i := 0 to PanelCount - 1 do
      begin
        Panels[i].Visible := Panels[i].TabVisible and (ActiveTabIndex = i);
        Panels[i].Invalidate;
      end;
    end;

    FCollapsed := Value;

    if ActiveTabIndex >= 0 then
    begin
//      Panels[ActiveTabIndex].Visible := true;
      Panels[ActiveTabIndex].SetPosInAdvNavBar;
      ShowAdvNavBarPanel(ActiveTabIndex); //Needs to be updated, visible or non.
    end;

    Invalidate;
  finally
    FIsCollapsing := False;
  end;
end;

function TAdvNavBar.CanResize(var NewWidth, NewHeight: Integer): Boolean;
var
  FCollapsState: boolean;
begin
  Result := inherited CanResize(NewWidth,NewHeight);

  if not Result or FIsCollapsing then
    Exit; //Can't resize due to internal constraints...

  if not AllowCollaps then
    Exit;

  if NewWidth = Width then
    Exit;

  if FCollapsed then
  begin //Already collapsed
    if (NewWidth > 0) and (NewWidth <= FCollapsedWidth) then
    begin
       Result := Width = 0; //*WAS* Sqz'ed all the way down, ok to show as collapsed and visible
       if Result then
         NewWidth := FCollapsedWidth;
    end
    else
      if NewWidth > FCollapsedWidth then
      begin
        FCollapsState := FCollapsed;

        SetCollapsed(False); //Was collapsed and now stretched wider...

        if FCollapsState then
        begin
          if Assigned(OnCollapsChange) then
            OnCollapsChange(Self);
        end;
      end
      else
      begin
        //NewWidth=0 .. OK.
      end;
  end
  else
  begin //Not collapsed yet.
    if NewWidth <= FCollapsedWidth then
    begin //resized into collapsed area
      if NewWidth > 0 then
        NewWidth := FCollapsedWidth;

      FCollapsState := FCollapsed;
      
      SetCollapsed(True); //we're now collapsed

      if not FCollapsState then
      begin
        if Assigned(OnCollapsChange) then
          OnCollapsChange(Self);
      end;

    end;
  end;
end;

procedure TAdvNavBar.SetCollapsedCaption(const Value:String);
begin
  if Value <> FCollapsedCaption then
  begin
     FCollapsedCaption := Value;
     Invalidate;
  end;
end;

{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}

end.
