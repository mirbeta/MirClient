{*************************************************************************}
{ TOfficeStatusBar component                                              }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2006 - 2015                                       }
{           Email : info@tmssoftware.com                                  }
{           Website : http://www.tmssoftware.com/                         }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvOfficeStatusBar;

{$I TMSDEFS.INC}
{$DEFINE REMOVESTRIP}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Dialogs, ComObj,
  PictureContainer,  AdvGDIP, GDIPicture, AdvHintInfo, AdvStyleIF, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  {$IFDEF TMSTOOLBAR}
  , AdvToolBar
  {$ENDIF}
  ;
const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 6; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 2; // Build nr.

  GRIP_SIZE = 15;

  // version history
  // 1.0.0.0 : first release
  // 1.0.0.1 : Added support to automatically hide size grip for maximized window
  // 1.0.1.0 : Exposed Canvas as public property & fixed OnDrawPanel event handling
  // 1.1.0.0 : Added support for Office 2007 silver style
  // 1.1.0.1 : Fixed issue with percentage display & max value <> 100.
  // 1.1.1.0 : New : AntiAlias property added
  // 1.1.2.0 : Improved : file ellipsis antialiased text drawing
  // 1.1.3.0 : New : method StepIt added in TAdvOfficeStatusPanel
  // 1.1.3.1 : Fixed : issue with panel & hyperlink click
  // 1.1.3.2 : Improved : painting
  // 1.2.0.0 : New : Added AutoSize capability on statusbar panels for text, date,time, capslock, numlock, scrollock, html
  //         : New : Added StretchPanel
  //         : New : Added design time resizing of panels
  // 1.2.0.1 : Fixed : issue with hotkey prefix drawing
  // 1.2.0.2 : Fixed : issue with SimplePanel drawing
  // 1.2.0.3 : Fixed : issue with possible overflow in progress position calculation
  // 1.2.0.4 : Fixed : issue with center aligned HTML formatted statusbar panels
  // 1.2.0.5 : Fixed : memory leak with timer when destroying component
  // 1.2.0.6 : Fixed : issue with sizegrip on TAdvToolBarForm
  // 1.3.0.0 : New : Windows Vista, Windows 7 & Terminal styles
  // 1.3.0.1 : Improved : Auto sizing of statusbar
  // 1.3.0.2 : Fixed : center alignment for psImage panel type
  // 1.3.0.3 : Improved update speed of progressbar
  // 1.4.0.0 : New : Automatic Office 2007 / 2010 theme synchronisation
  // 1.4.0.1 : Fixed : Issue when setting Cursor = crHandPoint at design time
  // 1.4.0.2 : Fixed : Prefix/Suffix now also showing for percentage display progress bars
  // 1.4.1.0 : New : Property ShowSplitter added
  // 1.4.1.1 : Fixed : Issue with progressbar size = 0
  // 1.5.0.0 : New : Support for Metro style
  // 1.5.1.0 : New : URLStyle property added
  //         : Fixed : Issue with OnAnchorClick and alignment in HTML panels
  // 1.5.2.0 : New : StatusBar.Panels[Index].Rect: TRect property exposed
  // 1.5.2.1 : Improved : Performance with updating panel text
  // 1.5.2.2 : Fixed : Issue when using on a TAdvMetroForm
  // 1.5.3.0 : New : Title hints for hyperlinks used in HTML panels
  // 1.5.3.1 : Improved : Panel Text property DFM storage
  // 1.5.3.2 : Fixed : Issue with destroying statusbar with central used styler
  // 1.6.0.0 : New : Windows 10, Office 2016 styles added
  // 1.6.0.1 : Fixed : Issue with AutoThemeAdapt and Office 2013 / 2016 styles
  // 1.6.1.0 : Improved : HighDPI support for 4K screens
  // 1.6.1.1 : Fixed : Background color issue in Office 2016 color style when no AppColor is used
  // 1.6.1.2 : Improved Office 2016 color styles

type

{ TAdvOfficeStatusBar }

  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TAdvOfficeStatusBar = class;
  TCustomAdvOfficeStatusBarStyler = class;

  TAdvOfficeStatusPanelStyle = (psHTML, psText, psOwnerDraw, psTime, psDate, psNumLock, psCapsLock, psScrollLock, psProgress, psImage, psImageList, psAnimation, psEllipsText, psFileEllipsText);
  TAdvOfficeStatusPanelBevel = (pbNone, pbLowered, pbRaised);
  TGaugeOrientation = (goHorizontal, goVertical);
  TPanelAppearanceStyle = (psLight, psDark);

  TGDIPGradient = (ggRadial, ggVertical, ggDiagonalForward, ggDiagonalBackward);
  TButtonLayout = (blGlyphLeft, blGlyphTop, blGlyphRight, blGlyphBottom);

  TAnchorClick = procedure(Sender: TObject; Anchor: string) of object;

  TPanelClick = procedure(Sender: TObject; PanelIndex: Integer) of object;

  TProgressIndication = (piPercentage, piAbsolute, piNone);

  TAdvOfficeStatusPanel = class;

  TProgressStyle = class(TPersistent)
  private
    FMin: integer;
    FPosition: integer;
    FMax: integer;
    FOldPerc: integer;
    FIndication: TProgressIndication;
    FBackground: TColor;
    FOwner: TAdvOfficeStatusPanel;
    FLevel0Color: TColor;
    FLevel0ColorTo: TColor;
    FLevel1Color: TColor;
    FLevel1ColorTo: TColor;
    FLevel2Color: TColor;
    FLevel2ColorTo: TColor;
    FLevel3Color: TColor;
    FLevel3ColorTo: TColor;
    FLevel1Perc: Integer;
    FLevel2Perc: Integer;
    FBorderColor: TColor;
    FShowBorder: Boolean;
    FStacked: Boolean;
    FShowPercentage: Boolean;
    FCompletionSmooth: Boolean;
    FShowGradient: Boolean;
    FSuffix: string;
    FPrefix: string;
    procedure SetIndication(const Value: TProgressIndication);
    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    procedure SetPosition(const Value: integer);
    procedure SetBackGround(const Value: tColor);
    procedure SetLevel0Color(const Value: tColor);
    procedure SetLevel0ColorTo(const Value: tColor);
    procedure SetLevel1Color(const Value: tColor);
    procedure SetLevel1ColorTo(const Value: tColor);
    procedure SetLevel2Color(const Value: tColor);
    procedure SetLevel2ColorTo(const Value: tColor);
    procedure SetLevel3Color(const Value: tColor);
    procedure SetLevel3ColorTo(const Value: tColor);
    procedure SetLevel1Perc(Value: integer);
    procedure SetLevel2Perc(Value: integer);
    procedure SetBorderColor(const Value: tColor);
    procedure SetShowBorder(Value: boolean);
    procedure SetStacked(Value: boolean);
    procedure SetShowPercentage(Value: boolean);
    procedure SetCompletionSmooth(Value: boolean);
    procedure SetShowGradient(Value: boolean);
    procedure SetPrefix(const Value: string);
    procedure SetSuffix(const Value: string);
  protected
    procedure Changed;
    procedure ChangedPerc;
  public
    constructor Create(aOwner: TAdvOfficeStatusPanel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property BackGround: TColor read FBackground write SetBackGround;
    property Indication: TProgressIndication read FIndication write SetIndication;
    property Min: integer read FMin write SetMin;
    property Max: integer read FMax write SetMax;
    property Position: integer read FPosition write SetPosition;

    property Level0Color: TColor read FLevel0Color write SetLevel0Color;
    property Level0ColorTo: TColor read FLevel0ColorTo write SetLevel0ColorTo;
    property Level1Color: TColor read FLevel1Color write SetLevel1Color;
    property Level1ColorTo: TColor read FLevel1ColorTo write SetLevel1ColorTo;
    property Level2Color: TColor read FLevel2Color write SetLevel2Color;
    property Level2ColorTo: TColor read FLevel2ColorTo write SetLevel2ColorTo;
    property Level3Color: TColor read FLevel3Color write SetLevel3Color;
    property Level3ColorTo: TColor read FLevel3ColorTo write SetLevel3ColorTo;
    property Level1Perc: Integer read FLevel1Perc write SetLevel1Perc;
    property Level2Perc: Integer read FLevel2Perc write SetLevel2Perc;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property ShowBorder: Boolean read FShowBorder write SetShowBorder;
    property Stacked: Boolean read FStacked write SetStacked;
    property ShowPercentage: Boolean read FShowPercentage write SetShowPercentage default True;
    property CompletionSmooth: Boolean read FCompletionSmooth write SetCompletionSmooth default False;
    property Suffix: string read FSuffix write SetSuffix;
    property Prefix: string read FPrefix write SetPrefix;
    property ShowGradient: Boolean read FShowGradient write SetShowGradient default true;
  end;

  TDummyHintControl = class(TControl)
  private
    FOfficeHint: TAdvHintInfo;
    procedure SetOfficeHint(const Value: TAdvHintInfo);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OfficeHint: TAdvHintInfo read FOfficeHint write SetOfficeHint;
  end;

  TAdvOfficeStatusPanel = class(TCollectionItem)
  private
    FText: string;
    FWidth: Integer;
    FAlignment: TAlignment;
    FBevel: TAdvOfficeStatusPanelBevel;
    FBiDiMode: TBiDiMode;
    FParentBiDiMode: Boolean;
    FStyle: TAdvOfficeStatusPanelStyle;
    FUpdateNeeded: Boolean;
    FTimeFormat: string;
    FDateFormat: string;
    FAnimIndex: Integer;
    FProgressStyle: TProgressStyle;
    FHTMLOffsetY: Integer;
    FHTMLOffsetX: Integer;
    FImageIndex: Integer;
    FImageIndexes: TStringList;
    FAnimationImages: TImageList;
    FAnimationDelay: Integer;
    FAnimated: Boolean;
    FEnabled: Boolean;
    FAppearanceStyle: TPanelAppearanceStyle;
    FOfficeHint: TAdvHintInfo;
    FButton: Boolean;
    FAutoSize: Boolean;
    FMinWidth: Integer;
    procedure SetAlignment(Value: TAlignment);
    procedure SetBevel(Value: TAdvOfficeStatusPanelBevel);
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetParentBiDiMode(Value: Boolean);
    function IsBiDiModeStored: Boolean;
    procedure SetStyle(Value: TAdvOfficeStatusPanelStyle);
    procedure SetText(const Value: string);
    procedure SetWidth(Value: Integer);
    procedure SetDateFormat(const Value: string);
    procedure SetTimeFormat(const Value: string);
    procedure SetProgressStyle(const Value: TProgressStyle);
    procedure SetHTMLOffsetX(const Value: Integer);
    procedure SetHTMLOffseTY(const Value: Integer);
    procedure SetImageIndex(const Value: Integer);
    function GetImageIndexes(Index: Integer): Integer;
    procedure SetImageIndexes(Index: Integer; const Value: Integer);
    procedure SetAnimationImages(const Value: TImageList);
    procedure SetAnimated(const Value: Boolean);
    procedure SetAnimationDelay(const Value: Integer);
    procedure SetEnabled(const Value: Boolean);
    procedure SetAppearanceStyle(Value: TPanelAppearanceStyle);
    procedure SetOfficeHint(const Value: TAdvHintInfo);
    procedure SetButton(const Value: Boolean);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetMinWidth(const Value: Integer);
    function IsTextStored: Boolean;
  protected
    function GetDisplayName: string; override;
    procedure Change;
    property Bevel: TAdvOfficeStatusPanelBevel read FBevel write SetBevel default pbNone;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ParentBiDiModeChanged;
    function UseRightToLeftAlignment: Boolean;
    function UseRightToLeftReading: Boolean;
    procedure ClearImageIndexes;
    function ImageCount: Integer;
    property ImageIndexes[Index: Integer]: Integer read GetImageIndexes write SetImageIndexes;
    procedure StepIt;
    function Rect: TRect;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Animated: Boolean read FAnimated write SetAnimated default false;
    property AnimationImages: TImageList read FAnimationImages write SetAnimationImages;
    property AnimationDelay: Integer read FAnimationDelay write SetAnimationDelay default 0;
    property AppearanceStyle: TPanelAppearanceStyle read FAppearanceStyle write SetAppearanceStyle;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default false;

    property BiDiMode: TBiDiMode read FBiDiMode write SetBiDiMode stored IsBiDiModeStored;
    property ParentBiDiMode: Boolean read FParentBiDiMode write SetParentBiDiMode default True;
    property Button: Boolean read FButton write SetButton default false;
    property DateFormat: string read FDateFormat write SetDateFormat;
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property HTMLOffsetX: Integer read FHTMLOffsetX write SetHTMLOffsetX default 1;
    property HTMLOffsetY: Integer read FHTMLOffsetY write SetHTMLOffseTY default 1;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property MinWidth: integer read FMinWidth write SetMinWidth default 20;
    property Progress: TProgressStyle read fProgressStyle write SetProgressStyle;
    property Style: TAdvOfficeStatusPanelStyle read FStyle write SetStyle default psHTML;
    property Text: string read FText write SetText stored IsTextStored;
    property TimeFormat: string read FTimeFormat write SetTimeFormat;
    property Width: Integer read FWidth write SetWidth;
    property OfficeHint: TAdvHintInfo read FOfficeHint write SetOfficeHint;
  end;

  TAdvOfficeStatusPanels = class(TCollection)
  private
    FStatusBar: TAdvOfficeStatusBar;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvOfficeStatusPanel;
    procedure SetItem(Index: Integer; Value: TAdvOfficeStatusPanel);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
    procedure Change;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(StatusBar: TAdvOfficeStatusBar);
    function Add: TAdvOfficeStatusPanel;
    property Items[Index: Integer]: TAdvOfficeStatusPanel read GetItem write SetItem; default;
  end;

  TDrawPanelEvent = procedure(StatusBar: TAdvOfficeStatusBar; Panel: TAdvOfficeStatusPanel;
    const Rect: TRect) of object;

  TURLStyle = (usUnderline, usNone);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeStatusBar = class(TCustomControl, ITMSTones)
  private
    FInternalOfficeStatusBarStyler: TCustomAdvOfficeStatusBarStyler;
    FOfficeStatusBarStyler: TCustomAdvOfficeStatusBarStyler;
    FCurrentOfficeStatusBarStyler: TCustomAdvOfficeStatusBarStyler;
    FPanels: TAdvOfficeStatusPanels;
    FSimpleText: string;
    FOldCursor: TCursor;
    FSimplePanel: Boolean;
    FSizeGrip: Boolean;
    FUseSystemFont: Boolean;
    FAutoHint: Boolean;
    FOnDrawPanel: TDrawPanelEvent;
    FOnHint: TNotifyEvent;
    FURLColor: TColor;
    FURLStyle: TURLStyle;
    FTimerID: Integer;
    FTimerCount: Integer;
    FImages: TImageList;
    FMousePanel: integer;
    FAnchor: string;
    FAnchorHint: boolean;
    FAnchorClick: TAnchorClick;
    FAnchorEnter: TAnchorClick;
    FAnchorExit: TAnchorClick;
    FOnPanelClick: TPanelClick;
    FOnPanelDblClick: TPanelClick;
    FOnPanelRightClick: TPanelClick;
    FContainer: TPictureContainer;
    FHotPanelIndex: Integer;
    FDownPanelIndex: Integer;
    FDummyHintControl: TDummyHintControl;
    FDisabledImages: TImageList;
    FAntiAlias: TAntiAlias;
    FStretchPanel: integer;
    FHitTest: TPoint;
    FSizePanel: integer;
    FSizeDownX: integer;
    FSizeWidth: integer;
    FShowSplitter: boolean;
    FMetro: boolean;
    function IsAnchor(x, y: integer; var Title: string): string;
    function GetPanel(x: integer): integer;
    procedure DoRightToLeftAlignment(var Str: string; AAlignment: TAlignment;
      ARTLAlignment: Boolean);
    function IsFontStored: Boolean;
    procedure SetPanels(Value: TAdvOfficeStatusPanels);
    procedure SetSimplePanel(Value: Boolean);
    procedure UpdateSimpleText;
    procedure SetSimpleText(const Value: string);
    procedure SetSizeGrip(Value: Boolean);
    procedure SyncToSystemFont;
    procedure DrawPanelBackGround(Panel: TAdvOfficeStatusPanel; R: TRect);
    procedure DrawSimplePanel;
    procedure DrawAllPanels;
    function PanelFixedSize: integer;
    function OnPanelBorder(x: integer): integer;
    procedure DrawSizeGrip(R: TRect);
    procedure UpdatePanel(Index: Integer; Repaint: Boolean);
    procedure UpdatePanels(UpdateRects, UpdateText: Boolean);
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMWinIniChange(var Message: TMessage); message CM_WININICHANGE;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure WMGetTextLength(var Message: TWMGetTextLength); message WM_GETTEXTLENGTH;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;        
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure OnPanelsChanged(Sender: TObject);
    procedure SetUseSystemFont(const Value: Boolean);
    procedure SetImages(const Value: TImageList);
    procedure SetOfficeStatusBarStyler(const Value: TCustomAdvOfficeStatusBarStyler);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetDisabledImages(const Value: TImageList);
    procedure SetAntiAlias(const Value: TAntiAlias);
    procedure SetShowSplitter(const Value: Boolean);
  protected
    procedure Paint; override;
    procedure UpdateMe(PropID: Integer);
    procedure UpdateStatusBar; virtual;
    procedure ChangeScale(M, D: Integer); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure WndProc(var Message: TMessage); override;    
    procedure Loaded; override;
    function DoHint: Boolean; virtual;
    procedure DrawPanel(Panel: TAdvOfficeStatusPanel; const Rect: TRect); dynamic;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    function GetSimplePanelRect: TRect;
    function GetPanelRect(Index: Integer): TRect;
    function IndexOfPanel(Panel: TAdvOfficeStatusPanel): Integer;
    function IsFirstPanel(Panel: TAdvOfficeStatusPanel): Boolean;
    function IsLastPanel(Panel: TAdvOfficeStatusPanel): Boolean;
    function HasSizeGrip: Boolean;
    procedure InvalidatePanel(PanelIndex: Integer);
    procedure AutoSizePanels;
    procedure SetStretchPanel(const Value: Integer);
    function ParentCanSize: boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetColorTones(ATones: TColorTones);
    procedure RemoveStyler;

    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure FlipChildren(AllLevels: Boolean); override;
    property PanelIndex[x: integer]: Integer read GetPanel;
    property Canvas;
  published
    property Action;
    property Anchors;
    property BiDiMode;
    //property BorderWidth;
    property DragKind;
    property Constraints;
    property ParentBiDiMode;
    property AnchorHint: boolean read fAnchorHint write fAnchorHint;
    property AntiAlias: TAntiAlias read FAntiAlias write SetAntiAlias default aaClearType;
    property AutoHint: Boolean read FAutoHint write FAutoHint default False;
    property Align default alBottom;
    property Color default clBtnFace;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font stored IsFontStored;
    property Images: TImageList read FImages write SetImages;
    property DisabledImages: TImageList read FDisabledImages write SetDisabledImages;
    property Panels: TAdvOfficeStatusPanels read FPanels write SetPanels;
    property ParentColor default False;
    property ParentFont default False;
    property ParentShowHint;
    property PictureContainer: TPictureContainer read FContainer write FContainer;   
    property PopupMenu;
    property ShowHint;
    property ShowSplitter: Boolean read FShowSplitter write SetShowSplitter;
    property SimplePanel: Boolean read FSimplePanel write SetSimplePanel;
    property SimpleText: string read FSimpleText write SetSimpleText;
    property SizeGrip: Boolean read FSizeGrip write SetSizeGrip default True;
    property URLColor: TColor read FURLColor write FURLColor default clBlue;
    property URLStyle: TURLStyle read FURLStyle write FURLStyle default usUnderline;
    property StretchPanel: integer read FStretchPanel write SetStretchPanel default -1;
    property Styler: TCustomAdvOfficeStatusBarStyler read FOfficeStatusBarStyler write SetOfficeStatusBarStyler;
    property UseSystemFont: Boolean read FUseSystemFont write SetUseSystemFont default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnStartDock;
    property OnResize;
    property OnEndDrag;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDrawPanel: TDrawPanelEvent read FOnDrawPanel write FOnDrawPanel;
    property OnPanelClick: TPanelClick read FOnPanelClick write FOnPanelClick;
    property OnPanelRightClick: TPanelClick read FOnPanelRightClick write FOnPanelRightClick;
    property OnPanelDblClick: TPanelClick read FOnPanelDblClick write FOnPanelDblClick;
    property OnStartDrag;
    property OnAnchorClick: TAnchorClick read fAnchorClick write fAnchorClick;
    property OnAnchorEnter: TAnchorClick read fAnchorEnter write fAnchorEnter;
    property OnAnchorExit: TAnchorClick read fAnchorExit write fAnchorExit;
    property Version: string read GetVersion write SetVersion;
  end;

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
    IsPercent: Boolean;
    Suffix: string;
    Prefix: string;
    Text: String;
  end;

  TVistaBackground = class(TPersistent)
  private
    FSteps: Integer;
    FColor: TColor;
    FColorTo: TColor;
    FOnChange: TNotifyEvent;
    FColorMirror: TColor;
    FColorMirrorTo: TColor;
    FBorderColor: TColor;
    FTextColor: TColor;
    FTextStyle: TFontStyles;
    FColorMirrorDownTo: TColor;
    FColorHot: TColor;
    FColorMirrorDown: TColor;
    FColorDownTo: TColor;
    FTextColorDown: TColor;
    FColorDown: TColor;
    FColorHotTo: TColor;
    FBorderColorDown: TColor;
    FColorMirrorHot: TColor;
    FBorderColorHot: TColor;
    FColorMirrorHotTo: TColor;
    FTextColorHot: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetSteps(const Value: Integer);
    procedure Changed;
    procedure SetBorderColor(const Value: TColor);
    procedure SetColorMirror(const Value: TColor);
    procedure SetColorMirrorTo(const Value: TColor);
    procedure SetTextColor(const Value: TColor);
    procedure SetTextStyle(const Value: TFontStyles);
    procedure SetBorderColorDown(const Value: TColor);
    procedure SetBorderColorHot(const Value: TColor);
    procedure SetColorDown(const Value: TColor);
    procedure SetColorDownTo(const Value: TColor);
    procedure SetColorHot(const Value: TColor);
    procedure SetColorHotTo(const Value: TColor);
    procedure SetColorMirrorDown(const Value: TColor);
    procedure SetColorMirrorDownTo(const Value: TColor);
    procedure SetColorMirrorHot(const Value: TColor);
    procedure SetColorMirrorHotTo(const Value: TColor);
    procedure SetTextColorDown(const Value: TColor);
    procedure SetTextColorHot(const Value: TColor);
  protected
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property BorderColorHot: TColor read FBorderColorHot write SetBorderColorHot;
    property BorderColorDown: TColor read FBorderColorDown write SetBorderColorDown;
    property Color: TColor read FColor write SetColor;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property ColorHot: TColor read FColorHot write SetColorHot;
    property ColorHotTo: TColor read FColorHotTo write SetColorHotTo;
    property ColorDown: TColor read FColorDown write SetColorDown;
    property ColorDownTo: TColor read FColorDownTo write SetColorDownTo;
    property ColorMirror: TColor read FColorMirror write SetColorMirror;
    property ColorMirrorTo: TColor read FColorMirrorTo write SetColorMirrorTo;
    property ColorMirrorHot: TColor read FColorMirrorHot write SetColorMirrorHot;
    property ColorMirrorHotTo: TColor read FColorMirrorHotTo write SetColorMirrorHotTo;
    property ColorMirrorDown: TColor read FColorMirrorDown write SetColorMirrorDown;
    property ColorMirrorDownTo: TColor read FColorMirrorDownTo write SetColorMirrorDownTo;
    property Steps: Integer read FSteps write SetSteps default 64;
    property TextColor: TColor read FTextColor write SetTextColor;
    property TextColorHot: TColor read FTextColorHot write SetTextColorHot;
    property TextColorDown: TColor read FTextColorDown write SetTextColorDown;
    property TextStyle: TFontStyles read FTextStyle write SetTextStyle;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TDbgList = class(TList)
  private
    function GetItemsEx(Index: Integer): Pointer;
    procedure SetItemsEx(Index: Integer; const Value: Pointer);
  public
    property Items[Index: Integer]: Pointer read GetItemsEx write SetItemsEx; default;
  end;

  TCustomAdvOfficeStatusBarStyler = class(TComponent)
  private
    FControlList: TDbgList;
    FAutoThemeAdapt: boolean;
    FBlendFactor: Integer;
    FPanelAppearanceLight: TVistaBackground;
    FPanelAppearanceDark: TVistaBackground;
    FBorderColor: TColor;
    procedure OnPanelAppearanceChanged(Sender: TObject);
    procedure SetPanelAppearanceLight(const Value: TVistaBackground);
    procedure SetPanelAppearanceDark(const Value: TVistaBackground);
    procedure SetBorderColor(const Value: TColor);
  protected
    procedure AddControl(AControl: TWinControl);
    procedure RemoveControl(AControl: TWinControl);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change(PropID: integer);
    property BlendFactor: Integer read FBlendFactor write FBlendFactor;
    procedure ThemeChanged(Sender: TObject); virtual;
    procedure SetAutoThemeAdapt(const Value: boolean); virtual;
    property AutoThemeAdapt: boolean read FAutoThemeAdapt write SetAutoThemeAdapt default False;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property PanelAppearanceLight: TVistaBackground read FPanelAppearanceLight write SetPanelAppearanceLight; // 1
    property PanelAppearanceDark: TVistaBackground read FPanelAppearanceDark write SetPanelAppearanceDark; // 1
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Assign(Source: TPersistent); override;
  end;


procedure DrawRectangle(Canvas: TCanvas; R: TRect; aColor: TColor);
procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
procedure DrawGauge(Canvas: TCanvas; R: TRect; Position: Integer; Settings: TGaugeSettings);
procedure DivideInSegment(Canvas: TCanvas; R: TRect; Position: integer; BkBmp: TBitMap; OrgR: TRect; Settings: TGaugeSettings);

implementation

uses
  Math, CommCtrl, ImgList, Forms, ShellAPI, StdActns, ComCtrls;


const
  CAPSLOCK = 'CAP';
  NUMLOCK = 'NUM';
  SCROLLLOCK = 'SCRL';

type
  {$IFDEF TMSTOOLBAR}
  TInternalToolBarPager = class(TAdvToolBarPager);
  {$ENDIF}
  TGradientDirection = (gdVertical, gdHorizontal);

{$I DELPHIXE.INC}

{$I HTMLENGO.PAS}

// Ellipisfy a text string.

function EllipsifyText( AsPath: boolean; const Text: string;
  const Canvas: TCanvas; MaxWidth: integer ): string;

  procedure CutFirstDirectory( var S: string );
  var
    Root: Boolean;
    P: Integer;
  begin
    if S = '' then
      exit;
    if S = '\' then
      S := ''
    else
    begin
      if S[ 1 ] = '\' then
      begin
        Root := True;
        Delete( S, 1, 1 );
      end
      else
        Root := False;
      if S[ 1 ] = '.' then
        Delete( S, 1, 4 );
      P := Pos( '\', S );
      if P <> 0 then
      begin
        Delete( S, 1, P );
        S := '...\' + S;
      end
      else
        S := '';
      if Root then
        S := '\' + S;
    end;
  end;

  function MinimizeName( const Filename: string; const Canvas: TCanvas;
    MaxLen: Integer ): string;
  var
    Drive: string;
    Dir: string;
    Name: string;
  begin
    Result := FileName;
    Dir := ExtractFilePath( Result );
    Name := ExtractFileName( Result );

    if ( Length( Dir ) >= 2 ) and ( Dir[ 2 ] = ':' ) then
    begin
      Drive := Copy( Dir, 1, 2 );
      Delete( Dir, 1, 2 );
    end
    else
      Drive := '';
    while ( ( Dir <> '' ) or ( Drive <> '' ) ) and ( Canvas.TextWidth( Result ) > MaxLen ) do
    begin
      if Dir = '\...\' then
      begin
        Drive := '';
        Dir := '...\';
      end
      else if Dir = '' then
        Drive := ''
      else
        CutFirstDirectory( Dir );
      Result := Drive + Dir + Name;
    end;
  end;

var
  Temp: string;
  AvgChar: integer;
  TLen, Index: integer;
  Metrics: TTextMetric;
begin
  try
    if AsPath then
    begin
      Result := MinimizeName( Text, Canvas, MaxWidth );
    end
    else
    begin
      Temp := Text;
      if ( Temp <> '' ) and ( Canvas.TextWidth( Temp ) > MaxWidth ) then
      begin
        GetTextMetrics( Canvas.Handle, Metrics );
        AvgChar := Metrics.tmAveCharWidth;
        if ( AvgChar * 3 ) < MaxWidth then
        begin
          Index := ( MaxWidth div AvgChar ) - 1;
          Temp := Copy( Text, 1, Index );
          if Canvas.TextWidth( Temp + '...' ) > MaxWidth then
          begin
            repeat
              dec( Index );
              SetLength( Temp, Index );
            until ( Canvas.TextWidth( Temp + '...' ) < MaxWidth ) or ( Index < 1 );
            { delete chars }
          end
          else
          begin
            TLen := Length( Text );
            repeat
              inc( Index );
              Temp := Copy( Text, 1, Index );
            until ( Canvas.TextWidth( Temp + '...' ) > MaxWidth ) or ( Index >= TLen );
            SetLength( Temp, Index - 1 );
          end;
          Temp := Temp + '...';
        end
        else
          Temp := '.';
      end;
      Result := Temp;
    end;
  except
    Result := '';
  end;
end;


//------------------------------------------------------------------------------

procedure DrawRectangle(Canvas: TCanvas; R: TRect; aColor: TColor);
begin
  canvas.Brush.Color := aColor;
  Canvas.FillRect(R);
end;

//------------------------------------------------------------------------------

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
      begin
        Rectangle(R.Left + stepw, R.Top, R.Left + stepw + Round(rstepw) + 1, R.Bottom)
      end
      else
      begin
        if R.Right <= R.Left + 1 then
        begin
          MoveTo(R.Left, R.Top + stepw);
          LineTo(R.Right, R.Top + stepw + Round(rstepw) + 1);
        end
        else
          Rectangle(R.Left, R.Top + stepw, R.Right, R.Top + stepw + Round(rstepw) + 1);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure DivideInSegment(Canvas: TCanvas; R: TRect; Position: integer; BkBmp: TBitMap; OrgR: TRect; Settings: TGaugeSettings);
var
  i: integer;
  r1, r2: TRect;
begin
  r1.Top := r.Top;
  r1.Left := r.Left + 8;
  r1.Right := r.Left + 10;
  r1.Bottom := R.Bottom+1;

  for i := 0 to (R.Right div 9) do
  begin
    if (r1.Right < r.Right) then
    begin
      if (Settings.BackgroundColor = clNone) then
      begin
        r2.Left := r1.Left - OrgR.Left;
        r2.Right := r2.Left + (r1.Right - r1.Left);
        r2.Top := r1.top - OrgR.Top;
        r2.Bottom := r2.Top + (r1.Bottom - r1.Top);
        Canvas.CopyRect(r1, BkBmp.Canvas, r2);
      end
      else
      begin
        Canvas.FillRect(r1);
      end;
    end;

    r1.Left := r1.Left + 9;
    r1.Right := r1.Left + 2;
  end;

end;

//------------------------------------------------------------------------------

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
  s: string;

{$IFNDEF TMSCLX}
  lf: TLogFont;
{$ENDIF}
  tf: TFont;

  R1: TRect;
  R2: TRect;
  bmp: TBitMap;
begin
  if (R.Right <= R.Left) or (R.Bottom <= R.Top) then
    Exit;

  bmp := TBitMap.Create;
  bmp.Height := R.Bottom - R.Top;
  bmp.Width := R.Right - R.Left;
  bmp.Canvas.CopyRect(Rect(0, 0, bmp.Width, bmp.Height), Canvas, R);

  WidthBar := R.Right - R.Left;

  Continue := true;
  Percentage := -1;
  Canvas.Brush.Color := Settings.BackgroundColor;
  GradDir := false;

  if (Settings.ShowPercentage) then
    Percentage := Position;

  //Draw Border
  if (Settings.ShowBorder) then
  begin
    Canvas.Pen.Color := Settings.BorderColor;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;
    InflateRect(R, -1, -1);
  end
  else
    Canvas.Pen.Color := Settings.BackgroundColor;

  if (Settings.BackgroundColor = clNone) then
  begin
    if (Settings.ShowBorder) then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    end;
  end
  else
  begin
    Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  end;

  WidthBar := WidthBar - 2;

  if Settings.ShowBorder then
    WidthBar := WidthBar - 2;

  if (Position > 0) then
  begin
    // stacked display

    if Settings.Stacked then
    begin
      if (Position >= Settings.Level1Perc) then
        WidthPart := Round((Settings.Level1Perc / 100) * WidthBar)
      else
      begin
        WidthPart := Round((Position / 100) * WidthBar);
        Continue := false;
      end;

      //Draw first part
      RectL.Left := R.Left + 1;
      RectL.Top := R.Top + 1;
      RectL.Right := RectL.Left + WidthPart;
      RectL.Bottom := r.Bottom - 2;

      if Settings.ShowGradient then
      begin
        R1.Left := RectL.Left;
        R1.Right := RectL.Right;
        R1.Top := RectL.Top;
        R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
        R2.Top := R1.Bottom;
        R2.Left := RectL.Left;
        R2.Right := RectL.Right;
        R2.Bottom := RectL.Bottom;

        DrawGradient(Canvas, Settings.Level0ColorTo, Settings.Level0Color, Settings.Steps, R1, GradDir);
        DrawGradient(Canvas, Settings.Level0Color, Settings.Level0ColorTo, Settings.Steps, R2, GradDir);
      end
      else
        DrawRectangle(Canvas, RectL, Settings.Level0Color);

      BarFilled := WidthPart;

      if not Settings.CompletionSmooth then
      begin
        RectL.Top := RectL.Top + 1;
        RectL.Bottom := RectL.Bottom - 1;
        Canvas.Brush.Color := Settings.BackgroundColor;
        DivideInSegment(Canvas, RectL, Position, bmp, R, Settings);
      end;

      if (Continue) then
      begin
        //Draw second part
        RectM.Left := RectL.Right;
        RectM.Top := r.Top + 1;
        RectM.Bottom := r.Bottom - 2;

        if (Position >= Settings.Level2Perc) then
          WidthPart := Round(WidthBar * ((Settings.Level2Perc - Settings.Level1Perc) / 100))
        else
        begin
          WidthPart := Round(WidthBar * ((Position - Settings.Level1Perc) / 100));
          Continue := false;
        end;

        RectM.Right := WidthPart + RectM.Left;

        if (Settings.ShowGradient) then
        begin
          R1.Left := RectM.Left;
          R1.Right := RectM.Right;
          R1.Top := RectM.Top;
          R1.Bottom := RectM.Top + (RectM.Bottom - RectM.Top) div 2;
          R2.Top := R1.Bottom;
          R2.Left := RectM.Left;
          R2.Right := RectM.Right;
          R2.Bottom := RectM.Bottom;

          DrawGradient(Canvas, Settings.Level1ColorTo, Settings.Level1Color, Settings.Steps, R1, GradDir);
          DrawGradient(Canvas, Settings.Level1Color, Settings.Level1ColorTo, Settings.Steps, R2, GradDir);
        end
        else
          DrawRectangle(Canvas, RectM, Settings.Level1Color);

        if not Settings.CompletionSmooth then
        begin
          RectM.Top := RectM.Top + 1;
          RectM.Bottom := RectM.Bottom - 1;
          Canvas.Brush.Color := Settings.BackgroundColor;
          DivideInSegment(Canvas, RectM, Position, bmp, R, Settings);
        end;

        BarFilled := BarFilled + WidthPart;
        if (Continue) then
        begin
          //Draw third part
          if (Position = 100) then
            WidthPart := Round(WidthBar - BarFilled)
          else
            WidthPart := Round(WidthBar * ((Position - Settings.Level2Perc) / 100));

          RectR.Left := RectM.Right;
          RectR.Top := R.Top + 1;
          RectR.Bottom := r.Bottom - 2;
          RectR.Right := RectR.Left + WidthPart;

          if (Settings.ShowGradient) then
          begin
            R1.Left := RectR.Left;
            R1.Right := RectR.Right;
            R1.Top := RectR.Top;
            R1.Bottom := RectR.Top + (RectR.Bottom - RectR.Top) div 2;
            R2.Top := R1.Bottom;
            R2.Left := RectR.Left;
            R2.Right := RectR.Right;
            R2.Bottom := RectR.Bottom;
            DrawGradient(Canvas, Settings.Level2ColorTo, Settings.Level2Color, Settings.Steps, R1, GradDir);
            DrawGradient(Canvas, Settings.Level2Color, Settings.Level2ColorTo, Settings.Steps, R2, GradDir);
          end
          else
            DrawRectangle(Canvas, RectR, Settings.Level3Color);

          if not Settings.CompletionSmooth then
          begin
            RectR.Top := RectR.Top + 1;
            RectR.Bottom := RectR.Bottom - 1;
            Canvas.Brush.Color := Settings.BackgroundColor;
            DivideInSegment(Canvas, RectR, Position, bmp, R, Settings);
          end;

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

        RectL.Left := R.Left + 2;
        RectL.Right := R.Left + Round((Position * WidthBar) / 100);
        RectL.Top := R.Top + 2;
        RectL.Bottom := R.Bottom - 3;

        if (Settings.ShowGradient) then
        begin
          R1.Left := RectL.Left;
          R1.Right := RectL.Right;
          R1.Top := RectL.Top;
          R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
          R2.Top := R1.Bottom;
          R2.Left := RectL.Left;
          R2.Right := RectL.Right;
          R2.Bottom := RectL.Bottom;
          DrawGradient(Canvas, BrushColorTo, BrushColor, Settings.Steps, R1, GradDir);
          DrawGradient(Canvas, BrushColor, BrushColorTo, Settings.Steps, R2, GradDir);
        end
        else
          DrawRectangle(Canvas, RectL, BrushColor);

        Canvas.Brush.Color := Settings.BackgroundColor;
        DivideInSegment(Canvas, RectL, Position, bmp, R, Settings);
      end
      else
      begin
        WidthPart := Round((Position / 100) * WidthBar);

        RectL.Left := R.Left + 1;
        RectL.Top := R.Top + 1;
        RectL.Right := RectL.Left + WidthPart;
        RectL.Bottom := R.Bottom - 2;

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
          DrawGradient(Canvas, BrushColorTo, BrushColor, Settings.Steps, R1, GradDir);
          DrawGradient(Canvas, BrushColor, BrushColorTo, Settings.Steps, R2, GradDir);
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
          ((R.Bottom - R.Top) div 2) + Canvas.TextWidth(IntToStr(Percentage) + '%') div 2, IntToStr(Percentage) + '%');
      finally
        tf.Free;
      end;
    end
    else
    begin
      if Settings.IsPercent then
        s := IntToStr(Percentage) + '%'
      else
        s := IntToStr(Settings.Position);

      s := Settings.Prefix + s + Settings.Suffix;

      Canvas.TextOut(((R.Right - R.Left) div 2) -
          (Canvas.TextWidth(s) div 2) + r.Left, r.Top +
          ((R.Bottom - R.Top) div 2) - Canvas.TextHeight(s) div 2, s);
    end;
  end
  else if (Settings.Text <> '') then
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
          (Canvas.TextHeight(Settings.Text) div 2)), R.Top +
          ((R.Bottom - R.Top) div 2) + Canvas.TextWidth(Settings.Text) div 2, Settings.Text);
      finally
        tf.Free;
      end;
    end
    else
    begin
      s := Settings.Text;

      Canvas.TextOut(((R.Right - R.Left) div 2) -
          (Canvas.TextWidth(s) div 2) + r.Left, r.Top +
          ((R.Bottom - R.Top) div 2) - Canvas.TextHeight(s) div 2, s);
    end;
  end;
  
  bmp.Free;
end;


//------------------------------------------------------------------------------

function ColorToARGB(Color: TColor): ARGB;
var
  c: TColor;
begin
  c := ColorToRGB(Color);
  Result := ARGB( $FF000000 or ((DWORD(c) and $FF) shl 16) or ((DWORD(c) and $FF00) or ((DWORD(c) and $ff0000) shr 16)));
end;

//------------------------------------------------------------------------------

function BlendColor(Col1,Col2:TColor; BlendFactor:Integer): TColor;
var
  r1,g1,b1: Integer;
  r2,g2,b2: Integer;

begin
  if BlendFactor >= 100 then
  begin
    Result := Col1;
    Exit;
  end;
  if BlendFactor <= 0 then
  begin
    Result := Col2;
    Exit;
  end;

  Col1 := Longint(ColorToRGB(Col1));
  r1 := GetRValue(Col1);
  g1 := GetGValue(Col1);
  b1 := GetBValue(Col1);

  Col2 := Longint(ColorToRGB(Col2));
  r2 := GetRValue(Col2);
  g2 := GetGValue(Col2);
  b2 := GetBValue(Col2);

  r1 := Round( BlendFactor/100 * r1 + (1 - BlendFactor/100) * r2);
  g1 := Round( BlendFactor/100 * g1 + (1 - BlendFactor/100) * g2);
  b1 := Round( BlendFactor/100 * b1 + (1 - BlendFactor/100) * b2);

  Result := RGB(r1,g1,b1);
end;

//------------------------------------------------------------------------------

procedure DrawRoundRect(graphics: TGPGraphics; Pen: TGPPen; X,Y,Width,Height,Radius: integer);
var
  path:TGPGraphicsPath;
begin
  path := TGPGraphicsPath.Create;
  path.AddLine(X + radius, Y, X + width - (radius*2), Y);
  path.AddArc(X + width - (radius*2), Y, radius*2, radius*2, 270, 90);
  path.AddLine(X + width, Y + radius, X + width, Y + height - (radius*2));
  path.AddArc(X + width - (radius*2), Y + height - (radius*2), radius*2, radius*2,0,90);
  path.AddLine(X + width - (radius*2), Y + height, X + radius, Y + height);
  path.AddArc(X, Y + height - (radius*2), radius*2, radius*2, 90, 90);
  path.AddLine(X, Y + height - (radius*2), X, Y + radius);
  path.AddArc(X, Y, radius*2, radius*2, 180, 90);
  path.CloseFigure;
  graphics.DrawPath(pen, path);
  path.Free;
end;

//------------------------------------------------------------------------------

procedure DrawRect(graphics: TGPGraphics; Pen: TGPPen; X,Y,Width,Height: integer);
var
  path:TGPGraphicsPath;
begin
  path := TGPGraphicsPath.Create;
  path.AddLine(X, Y, X + width, Y);
  path.AddLine(X + width, Y, X + width, Y + height);
  path.AddLine(X + width, Y + height, X, Y + height);
  path.AddLine(X, Y + height, X, Y);
  path.CloseFigure;
  graphics.DrawPath(pen, path);
  path.Free;
end;

//------------------------------------------------------------------------------

// Draw gradient in the specified rectangle (if Fill = True and ColorFrom <> clNone),
// frame it with BorderColor color.
procedure DrawVistaGradient(ACanvas: TCanvas; ARect: TRect; ColorFrom, ColorTo, ColorMirrorFrom, ColorMirrorTo: TColor;
  Direction: TGradientDirection; BorderColor: TColor; Fill: Boolean = True);
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
        DrawGradient(ACanvas,  ColorFrom, ColorTo, 128, r, Direction = gdVertical);
        r := ARect;
        r.Left := r.Left + ((r.Right - r.Left) div 2);
        DrawGradient(ACanvas,  ColorMirrorFrom, ColorMirrorTo, 128, r, Direction = gdVertical);
      end
      else
      begin
        r.Bottom := r.Top + ((r.Bottom - r.Top) div 2);
        DrawGradient(ACanvas,  ColorFrom, ColorTo, 128, r, Direction = gdVertical);
        r := ARect;
        r.Top := r.Top + ((r.Bottom - r.Top) div 2);
        DrawGradient(ACanvas,  ColorMirrorFrom, ColorMirrorTo, 128, r, Direction = gdVertical);
      end;  
    end
    else
      DrawGradient(ACanvas, ColorFrom, ColorTo, 128, ARect, Direction = gdVertical);
  end;

  if BorderColor <> clNone then
  begin
    ACanvas.Brush.Color := BorderColor;
    ACanvas.FrameRect(ARect);
  end;
end;

//------------------------------------------------------------------------------

function IsTTF(Canvas: TCanvas): Boolean;
var
  tm: TTextMetric;
begin
  Result := false;
  if not Assigned(Canvas) then
    Exit;

  GetTextMetrics(Canvas.Handle, tm);

  if ((tm.tmPitchAndFamily AND TMPF_VECTOR) = TMPF_VECTOR) then
  begin
    if not ((tm.tmPitchAndFamily AND TMPF_DEVICE) = TMPF_DEVICE) then
    begin
      Result := true;
      if (Screen.Fonts.IndexOf(Canvas.Font.Name) = -1) then
        Result := false;
    end;
  end;
end;

//------------------------------------------------------------------------------


function DrawVistaText(Canvas: TCanvas; Alignment: TAlignment; DTSTYLE: DWORD; r: TRect; Text:string; AFont: TFont; Enabled: Boolean; RealDraw: Boolean; AntiAlias: TAntiAlias; Ellipsis: Boolean; MultiLine: Boolean): TRect;
var
  graphics : TGPGraphics;
  w,h: Integer;
  fontFamily: TGPFontFamily;
  font: TGPFont;
  rectf: TGPRectF;
  stringFormat: TGPStringFormat;
  solidBrush: TGPSolidBrush;
  x1,y1,x2,y2: single;
  fs: integer;
  sizerect: TGPRectF;
  s, s2: string;
  i, j: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (Text <> '') then
  begin
    Canvas.Font.Name := AFont.Name;
    if (AntiAlias = aaNone) or not IsTTF(Canvas) then
    begin
      Result := Rect(0, 0, 1000, 100);
      DrawText(Canvas.Handle,PChar(Text),Length(Text), Result, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or DT_NOPREFIX);

      if RealDraw then
      begin
        DrawText(Canvas.Handle, PChar(Text), Length(Text), r, DTSTYLE);
      end;
    end
    else
    begin
      if (DTSTYLE AND DT_PATH_ELLIPSIS = DT_PATH_ELLIPSIS) then
      begin
        Text := EllipsifyText(true,Text, Canvas, r.Right - r.Left);
      end;

      graphics := TGPGraphics.Create(Canvas.Handle);
      fontFamily:= TGPFontFamily.Create(AFont.Name);
      fs := 0;

      font := TGPFont.Create(fontFamily, AFont.Size , fs, UnitPoint);
      graphics.SetSmoothingMode(SmoothingModeAntiAlias);

      w := R.Right - R.Left;
      h := R.Bottom - R.Top;

      x1 := r.Left;
      y1 := r.Top;
      x2 := w;
      y2 := h;

      rectf := MakeRect(x1,y1,x2,y2);

      stringFormat := TGPStringFormat.Create;

      if Enabled then
        solidBrush := TGPSolidBrush.Create(ColorToARGB(AFont.Color))
      else
        solidBrush := TGPSolidBrush.Create(ColorToARGB(clGray));

      case Alignment of
        taLeftJustify: stringFormat.SetAlignment(StringAlignmentNear);
        taRightJustify: stringFormat.SetAlignment(StringAlignmentFar);
        else
        begin
          // Center-justify each line of text.
          stringFormat.SetAlignment(StringAlignmentCenter);
        end;
      end;


      // Center the block of text (top to bottom) in the rectangle.
      stringFormat.SetLineAlignment(StringAlignmentCenter);

      stringFormat.SetHotkeyPrefix(HotkeyPrefixNone);

      case AntiAlias of
      aaClearType:graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
      aaAntiAlias:graphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
      end;

      if RealDraw and (Ellipsis or not MultiLine) then
      begin
        rectf := MakeRect(x1,y1,1000,y2);
        graphics.MeasureString(Text, Length(Text), font, rectf, stringFormat, sizerect);
      end
      else
        graphics.MeasureString(Text, Length(Text), font, rectf, stringFormat, sizerect);

      Result := Rect(round(sizerect.X), Round(sizerect.Y), Round(sizerect.X + sizerect.Width), Round(sizerect.Y + sizerect.Height));
      rectf := MakeRect(x1,y1,x2,y2);

      if RealDraw then
      begin
        //-- Add ellipsis
        if (sizerect.Width > x2) and (Ellipsis or not MultiLine) then
        begin
          rectf := MakeRect(x1,y1,1000,y2);
          if Ellipsis then
          begin
            s := '...';
            //Text := Copy(Text, 1, Length(Text)- 3);
            //Delete(Text, Length(Text)-3, 3);
          end
          else
          begin
            s := '';
          end;
          j := Length(Text);
          for i := 0 to j do
          begin
            s2 := Text + s;
            graphics.MeasureString(s2, Length(s2), font, rectf, stringFormat, sizerect);
            if (sizerect.Width > x2) and (Text <> '') then
            begin
              Text := Copy(Text, 1, Length(Text)-1);
              //Delete(Text, Length(Text)-1, 1);
            end
            else
            begin
              Break;
            end;
          end;
          Text := Text + s;
          rectf := MakeRect(x1,y1,x2,y2);
        end;

        graphics.DrawString(Text, Length(Text), font, rectf, stringFormat, solidBrush);
      end;

      stringformat.Free;
      solidBrush.Free;
      font.Free;
      fontfamily.Free;
      graphics.Free;
    end;
  end;
end;



//------------------------------------------------------------------------------

{ TAdvOfficeStatusPanel }

constructor TAdvOfficeStatusPanel.Create(Collection: TCollection);
begin
  FWidth := 50;
  FBevel := pbNone;
  inherited Create(Collection);
  FParentBiDiMode := True;
  ParentBiDiModeChanged;
  FTimeFormat := LongTimeFormat;
  FDateFormat := ShortDateFormat;
  FProgressStyle := TProgressStyle.Create(self);
  FStyle := psHTML;
  FHTMLOffsetX := 1;
  FHTMLOffsetY := 1;
  FImageIndex := -1;
  FAnimIndex := 0;
  FAlignment := taLeftJustify;
  FEnabled := true;
  FImageIndexes := TStringList.Create;
  FAppearanceStyle := psLight;
  FOfficeHint := TAdvHintInfo.Create;
  FButton := False;
  FMinWidth := 20;
  FAutoSize := False;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.Assign(Source: TPersistent);
begin
  if Source is TAdvOfficeStatusPanel then
  begin
    Text := TAdvOfficeStatusPanel(Source).Text;
    Width := TAdvOfficeStatusPanel(Source).Width;
    Alignment := TAdvOfficeStatusPanel(Source).Alignment;
    Bevel := TAdvOfficeStatusPanel(Source).Bevel;
    Style := TAdvOfficeStatusPanel(Source).Style;
    DateFormat := TAdvOfficeStatusPanel(Source).DateFormat;
    TimeFormat := TAdvOfficeStatusPanel(Source).TimeFormat;
    Progress.Assign(TAdvOfficeStatusPanel(Source).Progress);
    FOfficeHint.Assign(TAdvOfficeStatusPanel(Source).OfficeHint);
    AppearanceStyle := TAdvOfficeStatusPanel(Source).AppearanceStyle;
    AutoSize := TAdvOfficeStatusPanel(Source).AutoSize;
    MinWidth := TAdvOfficeStatusPanel(Source).MinWidth;    
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------


procedure TAdvOfficeStatusPanel.SetBiDiMode(Value: TBiDiMode);
begin
  if Value <> FBiDiMode then
  begin
    FBiDiMode := Value;
    FParentBiDiMode := False;
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusPanel.IsBiDiModeStored: Boolean;
begin
  Result := not FParentBiDiMode;
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusPanel.IsTextStored: Boolean;
begin
  Result := not (FStyle in [psTime, psDate, psNumLock, psCapsLock, psScrollLock]);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetParentBiDiMode(Value: Boolean);
begin
  if FParentBiDiMode <> Value then
  begin
    FParentBiDiMode := Value;
    ParentBiDiModeChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.ParentBiDiModeChanged;
begin
  if FParentBiDiMode then
  begin
    if GetOwner <> nil then
    begin
      BiDiMode := TAdvOfficeStatusPanels(GetOwner).FStatusBar.BiDiMode;
      FParentBiDiMode := True;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusPanel.Rect: TRect;
begin
  Result := TAdvOfficeStatusPanels(GetOwner).FStatusBar.GetPanelRect(Index);
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusPanel.UseRightToLeftReading: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode <> bdLeftToRight);
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusPanel.UseRightToLeftAlignment: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode = bdRightToLeft);
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusPanel.GetDisplayName: string;
begin
  Result := Text;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetBevel(Value: TAdvOfficeStatusPanelBevel);
begin
  if FBevel <> Value then
  begin
    FBevel := Value;
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetStyle(Value: TAdvOfficeStatusPanelStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed(False);
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    //Changed(False);
    //
    (Collection.Owner as TAdvOfficeStatusBar).Invalidate;

    if AutoSize then
      if Assigned(Collection) then
        TAdvOfficeStatusPanels(Collection).FStatusBar.AutoSizePanels;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed(True);
    Change;
  end;
end;

procedure TAdvOfficeStatusPanel.StepIt;
begin
  if Progress.Position < Progress.Max then
    Progress.Position := Progress.Position + 1;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetDateFormat(const Value: string);
begin
  FDateFormat := Value;
  Changed(True);
  Change;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetTimeFormat(const Value: string);
begin
  FTimeFormat := Value;
  Changed(True);
  Change;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetProgressStyle(const Value: TProgressStyle);
begin
  FProgressStyle.Assign(Value);
  Changed(True);
end;

//------------------------------------------------------------------------------

destructor TAdvOfficeStatusPanel.Destroy;
begin
  Changed(true);
  FProgressStyle.Free;
  FImageIndexes.Free;
  FOfficeHint.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetHTMLOffsetX(const Value: Integer);
begin
  FHTMLOffsetX := Value;
  Changed(true);
  Change;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetImageIndex(const Value: Integer);
begin
  FImageIndex := value;
  Changed(true);
  Change;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetHTMLOffseTY(const Value: Integer);
begin
  FHTMLOffsetY := Value;
  Changed(true);
  Change;
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusPanel.GetImageIndexes(Index: Integer): Integer;
begin
  Result := -1;
  if FImageIndexes.Count > Index then
    Result := StrToInt(FImageIndexes.Strings[Index]);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetImageIndexes(Index: Integer;
  const Value: Integer);
begin
  while FImageIndexes.Count <= Index do
    FImageIndexes.Add('-1');

  FImageIndexes.Strings[Index] := IntToStr(Value);
end;

procedure TAdvOfficeStatusPanel.SetMinWidth(const Value: Integer);
begin
  FMinWidth := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.ClearImageIndexes;
begin
  FImageIndexes.Clear;
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusPanel.ImageCount: Integer;
begin
  Result := FImageIndexes.Count;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetAnimationImages(const Value: TImageList);
begin
  FAnimationImages := Value;
  Changed(true);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetAnimated(const Value: Boolean);
begin
  FAnimated := Value;
  Changed(true);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetAnimationDelay(const Value: Integer);
begin
  if Value >= 0 then
    FAnimationDelay := Value;

end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    Changed(False);
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetAppearanceStyle(Value: TPanelAppearanceStyle);
begin
  if (FAppearanceStyle <> Value) then
  begin
    FAppearanceStyle := Value;
    Changed(true);
    Change;
  end;
end;

procedure TAdvOfficeStatusPanel.SetAutoSize(const Value: Boolean);
begin
  FAutoSize := Value;

  if FAutoSize then
  begin
    if Assigned(Collection) then
    begin
      TAdvOfficeStatusPanels(Collection).FStatusBar.AutoSizePanels;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetOfficeHint(const Value: TAdvHintInfo);
begin
  FOfficeHint.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.SetButton(const Value: Boolean);
begin
  if (FButton <> Value) then
  begin
    FButton := Value;
    Changed(true);
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanel.Change;
begin
  TAdvOfficeStatusPanels(Collection).Change;
end;

//------------------------------------------------------------------------------

{ TAdvOfficeStatusPanels }

constructor TAdvOfficeStatusPanels.Create(StatusBar: TAdvOfficeStatusBar);
begin
  inherited Create(TAdvOfficeStatusPanel);
  FStatusBar := StatusBar;
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusPanels.Add: TAdvOfficeStatusPanel;
begin
  Result := TAdvOfficeStatusPanel(inherited Add);
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusPanels.GetItem(Index: Integer): TAdvOfficeStatusPanel;
begin
  Result := TAdvOfficeStatusPanel(inherited GetItem(Index));
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusPanels.GetOwner: TPersistent;
begin
  Result := FStatusBar;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanels.SetItem(Index: Integer; Value: TAdvOfficeStatusPanel);
begin
  inherited SetItem(Index, Value);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanels.Update(Item: TCollectionItem);
begin
  if (Item <> nil) then
    FStatusBar.UpdatePanel(Item.Index, False)
  else
    FStatusBar.UpdatePanels(True, False);

  {$IFDEF DELPHI6_LVL}
  (Owner as TAdvOfficeStatusBar).Update;
  {$ENDIF}

  Change;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusPanels.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

//------------------------------------------------------------------------------

{ TAdvOfficeStatusBar }

constructor TAdvOfficeStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks];

  FInternalOfficeStatusBarStyler := TCustomAdvOfficeStatusBarStyler.Create(self);
  FInternalOfficeStatusBarStyler.Name := 'InternalStyler';

  FOfficeStatusBarStyler := nil;
  FCurrentOfficeStatusBarStyler := FInternalOfficeStatusBarStyler;
  FCurrentOfficeStatusBarStyler.AddControl(self);
  {$IFDEF DELPHI6_LVL}
  FInternalOfficeStatusBarStyler.SetSubComponent(True);
  {$ENDIF}

  Color := clBtnFace;
  Height := 19;
  Align := alBottom;
  FPanels := TAdvOfficeStatusPanels.Create(Self);
  FPanels.OnChange := OnPanelsChanged;
  FSizeGrip := True;
  ParentFont := False;
  FURLColor := clBlue;
  FURLStyle := usUnderline;
  FUseSystemFont := True;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FMousepanel := -1;
  FTimerCount := 0;
  FHotPanelIndex := -1;
  FDownPanelIndex := -1;
  FDummyHintControl := TDummyHintControl.Create(Self);
  FDummyHintControl.Visible := False;
  FAntiAlias := aaClearType;
  DoubleBuffered := true;
  FStretchPanel := -1;
  FSizePanel := -1;
  FShowSplitter := true;
end;

//------------------------------------------------------------------------------

destructor TAdvOfficeStatusBar.Destroy;
begin
  FPanels.Free;
  FPanels := nil;
  if Assigned(Styler) then
  try
    Styler := nil; // force a Styler.RemoveControl
  except
  end;
  FInternalOfficeStatusBarStyler.Free;
  FDummyHintControl.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusBar.ParentCanSize: boolean;
var
  parentok: boolean;
  haspager: boolean;
  i: integer;
  tmsif: ITMSMetro;
begin
  parentok := (Parent is TCustomForm);
  haspager := false;

  if parentok then
  begin
    for i := 0 to (Parent as TCustomForm).ControlCount - 1 do
    begin
      if (Parent as TCustomForm).Controls[i].ClassName = 'TAdvToolBarPager' then
        haspager := true;
    end;

    if not parent.GetInterface(ITMSMetro, tmsif) then // is Metro
    begin
      if not haspager then
        parentok := (TCustomForm(Parent).BorderStyle in [bsSizeable, bsSizeToolWin]);
    end;
  end;

  Result := parentok;
end;

procedure TAdvOfficeStatusBar.RemoveStyler;
begin
  FCurrentOfficeStatusBarStyler := FInternalOfficeStatusBarStyler;
  FCurrentOfficeStatusBarStyler.AddControl(self);
  UpdateMe(0);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.CreateParams(var Params: TCreateParams);
const
  GripStyles: array[Boolean] of DWORD = (CCS_TOP, SBARS_SIZEGRIP);
begin
  InitCommonControl(ICC_BAR_CLASSES);

  inherited CreateParams(Params);

  with Params do
  begin
    //Style := Style or GripStyles[FSizeGrip and (Parent is TCustomForm)
    //  and (TCustomForm(Parent).BorderStyle in [bsSizeable, bsSizeToolWin])];
    Style := Style or GripStyles[FSizeGrip and ParentCanSize];
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.CreateWnd;
var
  frm: TCustomForm;
begin
  inherited CreateWnd;

  if (Panels.Count = 0) and (csDesigning in ComponentState) then
  begin
    with Panels.Add do
    begin
      Width := 80;
    end;
    with Panels.Add do
    begin
      Width := 100;
    end;
    with Panels.Add do
    begin
      Width := 130;
    end;
    with Panels.Add do
    begin
    end;
  end;

  frm := GetParentForm(Self);

  if Assigned(frm) and (frm is TForm) and (frm as TForm).Scaled then
  begin
    Height := Round(19 * CalculateDPIScale(true, Canvas.Handle));
  end;

  if FSimplePanel then
    SendMessage(Handle, SB_SIMPLE, 1, 0);

  FTimerID := SetTimer(handle, 111, 100, nil);
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusBar.DoHint: Boolean;
begin
  if Assigned(FOnHint) then
  begin
    FOnHint(Self);
    Result := True;
  end
  else Result := False;
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusBar.IndexOfPanel(Panel: TAdvOfficeStatusPanel): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (Panel = nil) then
    Exit;
    
  for I := 0 to Panels.Count - 1 do
  begin
    if (Panels[I] = Panel) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusBar.IsFirstPanel(Panel: TAdvOfficeStatusPanel): Boolean;
begin
  Result := IndexOfPanel(Panel) = 0;
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusBar.IsLastPanel(Panel: TAdvOfficeStatusPanel): Boolean;
begin
  Result := IndexOfPanel(Panel) = (Panels.Count-1);
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusBar.HasSizeGrip: Boolean;
var
  Placement: TWindowPlacement;

begin
  Result := FSizeGrip and ParentCanSize;

  if Result then
  begin
    if (Parent is TCustomForm) then
    begin
       Placement.length := SizeOf(TWindowPlacement);
       GetWindowPlacement((Parent as TCustomForm).Handle, @Placement);
       if Placement.showCmd = SW_SHOWMAXIMIZED then
         Result := false;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.InvalidatePanel(PanelIndex: Integer);
var
  R: TRect;
begin
  if (PanelIndex >= 0) and (PanelIndex < Panels.Count) then
  begin
    R := GetPanelRect(PanelIndex);
    InvalidateRect(Handle, @R, True);
  end
  else
    Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.DrawPanelBackGround(Panel: TAdvOfficeStatusPanel; R: TRect);
var
  PanelAppearance: TVistaBackGround;
  r2: TRect;
  LnClrTo, ShdClr: TColor;
  y, PanelIndex: Integer;
  Clr, ClrTo, ClrMirror, ClrMirrorTo: TColor;

begin
  if Panel.AppearanceStyle = psLight then
    PanelAppearance := FCurrentOfficeStatusBarStyler.PanelAppearanceLight
  else
    PanelAppearance := FCurrentOfficeStatusBarStyler.PanelAppearanceDark;

  with PanelAppearance do
  begin
    PanelIndex := IndexOfPanel(Panel);
    if Panel.Button and (PanelIndex >= 0) and (PanelIndex = FDownPanelIndex) then
    begin
      LnClrTo := PanelAppearance.BorderColorDown;
      Clr := ColorDown;
      ClrTo := ColorDownTo;
      ClrMirror := ColorMirrorDown;
      ClrMirrorTo := ColorMirrorDownTo;
    end
    else if Panel.Button and (PanelIndex >= 0) and (PanelIndex = FHotPanelIndex) then
    begin
      LnClrTo := PanelAppearance.BorderColorHot;
      Clr := ColorHot;
      ClrTo := ColorHotTo;
      ClrMirror := ColorMirrorHot;
      ClrMirrorTo := ColorMirrorHotTo;
    end
    else
    begin
      LnClrTo := PanelAppearance.BorderColor;
      Clr := Color;
      ClrTo := ColorTo;
      ClrMirror := ColorMirror;
      ClrMirrorTo := ColorMirrorTo;
    end;

    R2 := R;

    if not IsFirstPanel(Panel) and FShowSplitter  then
      R2.Left := R2.Left + 2;

    DrawVistaGradient(Canvas, R2, Clr, ClrTo, ClrMirror, ClrMirrorTo, gdHorizontal, LnClrTo, true);

    // Draw 3D effect
    if not IsFirstPanel(Panel) and FShowSplitter and not FMetro then
    begin
      ShdClr := BlendColor(ColorTo, clWhite, 70);

      y := (R.Bottom - R.Top) div 2;

      R2 := Rect(R.Left + 0, R.Top, R.Left + 1, R.Top + y + 1);
      DrawGradient(Canvas, ShdClr, ColorTo, 40, R2, False);

      R2 := Rect(R.Left + 0, R.Top + y, R.Left + 1, R.Bottom );
      DrawGradient(Canvas, ColorTo, ShdClr, 40, R2, False);

      ShdClr := BlendColor(ColorTo, clWhite, 10);
      LnClrTo := BlendColor(ColorTo, clWhite, 50);

      y := (R.Bottom - R.Top) div 2;

      R2 := Rect(R.Left + 1, R.Top, R.Left + 2, R.Top + y + 1);
      DrawGradient(Canvas, LnClrTo, ShdClr, 40, R2, False);

      R2 := Rect(R.Left + 1, R.Top + y, R.Left + 2, R.Bottom );
      DrawGradient(Canvas, ShdClr, LnClrTo, 40, R2, False);
    end;

    if FMetro and (Panel.Index > 0) then
    begin
      Canvas.Pen.Color := FCurrentOfficeStatusBarStyler.BorderColor;
      Canvas.Pen.Width := 1;
      Canvas.MoveTo(R.Left, R.top);
      Canvas.LineTo(R.Left, R.Bottom + 1);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.DrawSimplePanel;
var
  R, r2, dr, re: TRect;
  anchor, stripped: string;
  xsize, ysize: Integer;
  HyperLinks,MouseLink: Integer;
  Focusanchor: string;

  procedure ChangeOrientation(RightToLeftOrientation: Boolean);
  var
    Org: TPoint;
    Ext: TPoint;
  begin
  if RightToLeftOrientation then
  begin
    Org := Point(ClientWidth,0);
    Ext := Point(-1,1);
    SetMapMode(Canvas.Handle, mm_Anisotropic);
    SetWindowOrgEx(Canvas.Handle, Org.X, Org.Y, nil);
    SetViewportExtEx(Canvas.Handle, ClientWidth, ClientHeight, nil);
    SetWindowExtEx(Canvas.Handle, Ext.X*ClientWidth, Ext.Y*ClientHeight, nil);
  end
  else
  begin
    Org := Point(0,0);
    Ext := Point(1,1);
    SetMapMode(Canvas.Handle, mm_Anisotropic);
    SetWindowOrgEx(Canvas.Handle, Org.X, Org.Y, nil);
    SetViewportExtEx(Canvas.Handle, ClientWidth, ClientHeight, nil);
    SetWindowExtEx(Canvas.Handle, Ext.X*ClientWidth, Ext.Y*ClientHeight, nil);
  end;
end;

begin
  R := GetSimplePanelRect;

  with FCurrentOfficeStatusBarStyler.PanelAppearanceLight do
  begin
    R2.Left := 0;
    R2.Top := 0;
    R2.Right := Width;
    R2.Bottom := Height;
    DrawVistaGradient(Canvas, R2, Color, ColorTo, ColorMirror, ColorMirrorTo, gdHorizontal, clNone);
    Canvas.Font.Color := TextColor;
  end;

  R2 := R;
  R2.Left := R2.Left + 2;
  R2.Right := R2.Right - 2;

  if HasSizeGrip then
  begin
    R2.Right := R2.Right - GRIP_SIZE;
  end;

  Canvas.Brush.Style := bsClear;

  dr := R;
  dr.Left := dr.Left + 1;
  dr.Top := dr.Top + 1;

  if BidiMode = bdRightToLeft then
  begin
    // calculate size & right-align
    HTMLDrawEx(Canvas, SimpleText, dr, fImages, 0, 0, -1, -1, 1, true, false, false, false, (FTimerCount > 5), URLStyle = usNone,
      false, 1.0, URLColor, clNone, clNone, clGray, anchor, stripped, focusanchor, xsize, ysize,
      hyperlinks, mouselink, re, nil , FContainer, 0, BidiMode);

    dr.Left := R2.Right - xsize;
  end;

  HTMLDrawEx(Canvas, SimpleText, dr, fImages, 0, 0, -1, -1, 1, false, false, false, false, (FTimerCount > 5), URLStyle = usNone,
    false, 1.0, URLColor, clNone, clNone, clGray, anchor, stripped, focusanchor, xsize, ysize,
    hyperlinks, mouselink, re, nil , FContainer, 0, BidiMode);
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusBar.GetSimplePanelRect: TRect;
begin
  Result := Rect(1, 1, Width-1, Height-1);
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusBar.GetPanelRect(Index: Integer): TRect;
var
  I, PanelPos, NR: Integer;
  R: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  R.Top := 1;
  R.Bottom := Height-1;
  PanelPos := 0;
  for I := 0 to Panels.Count - 1 do
  begin
    R.Left := PanelPos;

    if (StretchPanel = I) then
    begin
      NR := R.Left + (Width - PanelFixedSize);
      if NR > R.Left then
        R.Right := NR
      else
        R.Right := R.Left;

      PanelPos := PanelPos + (R.Right - R.Left);
    end
    else
    begin
      if (I = Panels.Count-1) then
      begin
        R.Right := max(Width-1, R.Left);
        PanelPos := PanelPos + R.Right;
      end
      else
      begin
        R.Right := R.Left + Panels[I].Width;
        PanelPos := PanelPos + Panels[I].Width;
      end;
    end;

    if (Index = I) then
    begin
      Result := R;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusBar.PanelFixedSize: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Panels.Count - 1 do
  begin
    if i <> FStretchPanel then
      Result := Result + Panels[i].Width;
  end;
end;

function TAdvOfficeStatusBar.OnPanelBorder(x: Integer): integer;
var
  i: integer;
  R: TRect;

begin
  Result := -1;
  for i := 0 to Panels.Count - 1 do
  begin
    R := GetPanelRect(i);

    if (x > r.Right - 2) and (x < r.Right +2) then
    begin
      Result := i;
      Break;
    end;

  end;
    

end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.DrawAllPanels;
const
  MaxPanelCount = 128;
var
  PanelPos, I, NR: Integer;
  R: TRect;
begin
  if SimplePanel then
  begin
    DrawSimplePanel;
  end
  else
  begin
    R.Top := 0;
    R.Bottom := Height - 1;

    PanelPos := 0;

    for I := 0 to Panels.Count - 1 do
    begin
      R.Left := PanelPos;

      if (StretchPanel = I) then
      begin
        NR := R.Left + (Width - PanelFixedSize);
        if NR > R.Left then
          R.Right := NR
        else
          R.Right := R.Left;

        PanelPos := PanelPos + (R.Right - R.Left);
      end
      else
      begin
        if ((I = Panels.Count - 1) and (StretchPanel = -1)) then
        begin
          R.Right := max(Width, R.Left);
          PanelPos := PanelPos + R.Right;
        end
        else
        begin
          R.Right := R.Left + Panels[I].Width;
          PanelPos := PanelPos + Panels[I].Width;
        end;
      end;

      if R.Left = 0 then
        R.Left := 1;

      if R.Right = Width -1 then
        R.Right := Width - 2;

      DrawPanel(Panels[i], R);
    end;
  end;

  if HasSizeGrip then
  begin
    R := Rect(0, 0, Width-3, Height-1);
    R.Left := R.Right - GRIP_SIZE;
    DrawSizeGrip(R);
  end;

  // Draw Border
  if not FMetro then
  begin
    R := Rect(0, 0, Width - 1, Height - 1);
    Canvas.Pen.Color := FCurrentOfficeStatusBarStyler.BorderColor;
    Canvas.MoveTo(R.Left, R.Top);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.MoveTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.MoveTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Right, R.Top);

    Canvas.Pen.Color := BlendColor(FCurrentOfficeStatusBarStyler.BorderColor, clWhite, 80);
    Canvas.MoveTo(R.Left, R.Top);
    Canvas.LineTo(R.Right + 1, R.Top);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.DrawSizeGrip(R: TRect);
var
  sp, fx, fy: Integer;
  clr: TColor;
begin
  sp := 4;
  fx := 5;
  fy := 2;
  with Canvas do
  begin
    // Light Dots
    Clr := FCurrentOfficeStatusBarStyler.PanelAppearanceDark.Color;

    if not FMetro then
    begin
      Brush.Color := BlendColor(Clr, clWhite, 50);
      Pen.Color := Brush.Color;
      Rectangle(R.Left + fx, R.Bottom - fy, R.Left + fx + 2, R.Bottom - fy - 2);
      Rectangle(R.Left + fx + sp, R.Bottom - fy, R.Left + fx + 2 + sp, R.Bottom - fy - 2);
      Rectangle(R.Left + fx + sp*2, R.Bottom - fy, R.Left + fx + 2 + sp*2, R.Bottom - fy - 2);

      Rectangle(R.Left + fx + sp*2, R.Bottom - fy - sp, R.Left + fx + 2 + sp*2, R.Bottom - fy - 2 - sp);
      Rectangle(R.Left + fx + sp*2, R.Bottom - fy - sp*2, R.Left + fx + 2 + sp*2, R.Bottom - fy - 2 - sp*2);
      Rectangle(R.Left + fx + sp, R.Bottom - fy - sp, R.Left + fx + 2 + sp, R.Bottom - fy - 2 - sp);
    end;

    // Dark Dots
    fx := fx - 1;
    fy := fy + 1;

    clr := FCurrentOfficeStatusBarStyler.BorderColor;

    if not FMetro then
      Canvas.Brush.Color := BlendColor(Clr, clWhite, 100)
    else
      Canvas.Brush.Color := clr;

    Pen.Color := Brush.Color;
    Rectangle(R.Left + fx, R.Bottom - fy, R.Left + fx + 2, R.Bottom - fy - 2);
    Rectangle(R.Left + fx + sp, R.Bottom - fy, R.Left + fx + 2 + sp, R.Bottom - fy - 2);
    Rectangle(R.Left + fx + sp*2, R.Bottom - fy, R.Left + fx + 2 + sp*2, R.Bottom - fy - 2);

    Rectangle(R.Left + fx + sp*2, R.Bottom - fy - sp, R.Left + fx + 2 + sp*2, R.Bottom - fy - 2 - sp);
    Rectangle(R.Left + fx + sp*2, R.Bottom - fy - sp*2, R.Left + fx + 2 + sp*2, R.Bottom - fy - 2 - sp*2);
    Rectangle(R.Left + fx + sp, R.Bottom - fy - sp, R.Left + fx + 2 + sp, R.Bottom - fy - 2 - sp);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.DrawPanel(Panel: TAdvOfficeStatusPanel; const Rect: TRect);
var
  anchor, stripped: string;
  xsize, ysize, i: Integer;
  Settings: TGaugeSettings;
  r, dr: TRect;
  DTSTYLE: DWORD;

  HyperLinks,MouseLink: Integer;
  Focusanchor: string;
  re: TRect;
  PanelAppearance: TVistaBackGround;
  TxtClr: TColor;
  PanelIndex: Integer;
  al: TAlignment;
  fpos: double;

begin
  if (Rect.Left >= Rect.Right) or (Rect.Top >= Rect.Bottom) or not Assigned(FCurrentOfficeStatusBarStyler) then
    Exit;

  if (csDestroying in ComponentState) then
    Exit;

  r := Rect;

  if Assigned(Styler) then
  begin
    if (Styler.BorderColor <> clNone) and not FMetro then
      InflateRect(r, 0, -1);
  end;

  if FMetro then
   InflateRect(r, 1, 0);

  al := Panel.Alignment;

  if UseRightToLeftAlignment then
    al := taRightJustify;

  DrawPanelBackGround(Panel, R);

  R.Left := R.Left + 2;
  R.Right := R.Right - 2;

  if HasSizeGrip and IsLastPanel(Panel) then
  begin
    R.Right := R.Right - GRIP_SIZE;
    if (R.Left >= R.Right) or (R.Top >= R.Bottom) then
      Exit;
  end;

  Canvas.Font.Assign(Self.Font);
  if Panel.AppearanceStyle = psLight then
  begin
    PanelAppearance := FCurrentOfficeStatusBarStyler.PanelAppearanceLight;
  end
  else
  begin
    PanelAppearance := FCurrentOfficeStatusBarStyler.PanelAppearanceDark;
  end;
  Canvas.Font.Style := PanelAppearance.TextStyle;

  PanelIndex := IndexOfPanel(Panel);
  if Panel.Button and (PanelIndex >= 0) and (PanelIndex = FDownPanelIndex) then
  begin
    TxtClr := PanelAppearance.TextColorDown;
  end
  else if Panel.Button and (PanelIndex >= 0) and (PanelIndex = FHotPanelIndex) then
  begin
    TxtClr := PanelAppearance.TextColorHot;
  end
  else
  begin
    TxtClr := PanelAppearance.TextColor;
  end;

  Canvas.Font.Color := TxtClr;
  Canvas.Brush.Style := bsClear;

  if not Panel.Enabled then
    Canvas.Font.Color := clGray;


  case Panel.Style of
    psHTML:
      begin
        dr := R;

        dr.Left := dr.Left + Panel.HTMLOffsetX;
        dr.Top := dr.Top + Panel.HTMLOffsetY;

        if al in [taRightJustify, taCenter] then
        begin
          HTMLDrawEx(Canvas, panel.Text, dr, fImages, 0, 0, -1, -1, 1, true, false, false, false, (fTimerCount > 5), false,
            true, 1.0, URLColor, clNone, clNone, clGray, anchor, stripped, focusanchor, xsize, ysize,
            hyperlinks, mouselink, re, nil , FContainer, 0);

          if (al = taRightJustify) then
          begin
            if xsize < (dr.Right - dr.Left) then
              dr.Left := dr.Right - xsize - Panel.HTMLOffsetX
          end
          else
          begin
            if Xsize < (dr.Right - dr.Left) then
              dr.Left := dr.Left + (dr.Right - dr.Left - Xsize) div 2;
          end;
        end;

        HTMLDrawEx(Canvas, panel.Text, dr, fImages, 0, 0, -1, -1, 1, false, false, false, false, (fTimerCount > 5), URLStyle = usNone,
          true, 1.0, URLColor, clNone, clNone, clGray, anchor, stripped, focusanchor, xsize, ysize,
          hyperlinks, mouselink, re, nil , FContainer, 0);
      end;
    psEllipsText:
      begin
        DTSTYLE := DT_SINGLELINE or DT_END_ELLIPSIS or DT_VCENTER or DT_NOPREFIX;
        case al of
          taLeftJustify: r.Left := r.Left + 2;
          taCenter: DTSTYLE := DTSTYLE or DT_CENTER;
          taRightJustify: DTSTYLE := DTSTYLE or DT_RIGHT;
        end;

        DrawVistaText(Canvas, Panel.Alignment, DTSTYLE, r, Panel.Text, Canvas.Font, Enabled, True, AntiAlias, True, False);
      end;
    psFileEllipsText:
      begin
        DTSTYLE := DT_SINGLELINE or DT_PATH_ELLIPSIS or DT_VCENTER or DT_NOPREFIX;

        case al of
          taLeftJustify: r.Left := r.Left + 2;
          taCenter: DTSTYLE := DTSTYLE or DT_CENTER;
          taRightJustify: DTSTYLE := DTSTYLE or DT_RIGHT;
        end;

        DrawVistaText(Canvas, Panel.Alignment, DTSTYLE, r, Panel.Text, Canvas.Font, Enabled, True, AntiAlias, False, False);
      end;
    psImage:
      begin
        if Assigned(Images) then
        begin
          case al of
          taLeftJustify: r.Left := r.Left + 2;
          taRightJustify: r.Left := r.Right - Images.Width - 2;
          taCenter: r.Left := r.Left + (r.Right - r.Left - Images.Width) div 2;
          end;

          dr := r;
          dr.Top := dr.Top + (r.Bottom - r.Top - Images.Height) div 2;

          if Panel.Enabled then
            Images.Draw(Canvas, dr.Left, dr.Top, Panel.ImageIndex)
          else
          begin
            if Assigned(DisabledImages) then
              DisabledImages.Draw(Canvas, dr.Left, dr.Top, Panel.ImageIndex)
            else
              Images.Draw(Canvas, dr.Left, dr.Top, Panel.ImageIndex, False);

          end;

          if al = taRightJustify then
            r.Right := r.Right - Images.Width
          else
            r.Left := r.Left + Images.Width;
        end;

        r.Left := r.Left + 2;

        DTSTYLE := DT_SINGLELINE or DT_END_ELLIPSIS or DT_VCENTER or DT_NOPREFIX;

        case Panel.Alignment of
          taCenter: DTSTYLE := DTSTYLE or DT_CENTER;
          taRightJustify: DTSTYLE := DTSTYLE or DT_RIGHT;
        end;

        Canvas.Brush.Style := bsClear;

        if not Panel.Enabled then
        begin
          Canvas.Font.Color := clRed;
          Canvas.Font.Color := clGray;  // requires forced color change ?
        end;

        DrawVistaText(Canvas, al, DTSTYLE, r, Panel.Text, Canvas.Font, Enabled, True, AntiAlias, True, False);
      end;
    psImageList:
      begin
        if Assigned(Images) then
        begin
          for i := 1 to Panel.ImageCount do
          begin
            r.Left := r.Left + 2;
            dr := r;
            dr.Top := dr.Top + (r.Bottom - r.Top - Images.Height) div 2;

            if Panel.Enabled then
              Images.Draw(Canvas, dr.Left, dr.Top, Panel.ImageIndexes[i - 1])
            else
            begin
              if Assigned(DisabledImages) then
                DisabledImages.Draw(Canvas, dr.Left, dr.Top, Panel.ImageIndexes[i - 1])
              else
                Images.Draw(Canvas, dr.Left, dr.Top, Panel.ImageIndexes[i - 1],False);
            end;
            r.Left := r.Left + Images.Width;
          end;
        end;
      end;
    psAnimation:
      begin
        if Assigned(Panel.AnimationImages) then
        begin
          r.Left := r.Left + 2;
          dr := r;
          dr.Top := dr.Top + (r.Bottom - r.Top - Panel.AnimationImages.Height) div 2;

          if Panel.Animated then
            Panel.AnimationImages.Draw(Canvas, dr.Left, dr.Top, Panel.FAnimIndex)
          else
            Panel.AnimationImages.Draw(Canvas, dr.Left, dr.Top, 0);
          r.Left := r.Left + Panel.AnimationImages.Width;
        end;
        r.Left := r.Left + 2;
        DTSTYLE := DT_SINGLELINE or DT_END_ELLIPSIS or DT_VCENTER or DT_NOPREFIX;
        case Panel.Alignment of
          taCenter: DTSTYLE := DTSTYLE or DT_CENTER;
          taRightJustify: DTSTYLE := DTSTYLE or DT_RIGHT;
        end;

        DrawVistaText(Canvas, Panel.Alignment, DTSTYLE, r, Panel.Text, Canvas.Font, Enabled, True, AntiAlias, True, False);
      end;
    psProgress:
      begin
        Settings.Level0Color := Panel.Progress.Level0Color;
        Settings.Level0ColorTo := Panel.Progress.Level0ColorTo;
        Settings.Level1Color := Panel.Progress.Level1Color;
        Settings.Level1ColorTo := Panel.Progress.Level1ColorTo;
        Settings.Level2Color := Panel.Progress.Level2Color;
        Settings.Level2ColorTo := Panel.Progress.Level2ColorTo;
        Settings.Level3Color := Panel.Progress.Level3Color;
        Settings.Level3ColorTo := Panel.Progress.Level3ColorTo;
        Settings.Level1Perc := Panel.Progress.Level1Perc;
        Settings.Level2Perc := Panel.Progress.Level2Perc;
        Settings.ShowBorder := Panel.Progress.ShowBorder;
        Settings.Stacked := Panel.Progress.Stacked;
        Settings.ShowPercentage := Panel.Progress.ShowPercentage;
        Settings.IsPercent := Panel.Progress.Indication = piPercentage;
        Settings.CompletionSmooth := Panel.Progress.CompletionSmooth;
        Settings.ShowGradient := Panel.Progress.ShowGradient;
        Settings.Font := Canvas.Font;
        Settings.Font.Color := TxtClr; //PanelAppearance.TextColor; // Panel.Progress.TextColor;
        Settings.Orientation := goHorizontal;
        Settings.Steps := 11;
        Settings.BackgroundColor := Panel.Progress.BackGround;
        Settings.BorderColor := Panel.Progress.BorderColor;
        Settings.Position := Panel.Progress.Position;
        Settings.Suffix := Panel.Progress.Suffix;
        Settings.Prefix := Panel.Progress.Prefix;
        Settings.Text := Panel.Text;

        fpos := Panel.Progress.Position - Panel.Progress.Min;
        if (Panel.Progress.Max - Panel.Progress.Min) > 0 then
          fpos := fpos / (Panel.Progress.Max - Panel.Progress.Min);
        fpos := fpos * 100;

        if Panel.Progress.Indication = piPercentage then
        begin
          if Panel.Progress.Max <> Panel.Progress.Min then
            DrawGauge(Canvas, r, Round(fpos), Settings)
          else
            DrawGauge(Canvas, r, 0, Settings)
        end
        else
        begin
          if Panel.Progress.Max <> Panel.Progress.Min then // avoid division by zero
            DrawGauge(Canvas, r, Round(fpos), Settings)
          else
            DrawGauge(Canvas, r, 0, Settings)
        end;
      end
    else
    begin
      if Assigned(FOnDrawPanel) and (Panel.Style = psOwnerDraw) then
        FOnDrawPanel(Self, Panel, Rect)
      else
      begin
        DTSTYLE := DT_SINGLELINE or DT_END_ELLIPSIS or DT_VCENTER or DT_NOPREFIX;
        case al of
          taCenter: DTSTYLE := DTSTYLE or DT_CENTER;
          taRightJustify: DTSTYLE := DTSTYLE or DT_RIGHT;
        end;

        InflateRect(r,-2,0);
        DrawVistaText(Canvas, Panel.Alignment, DTSTYLE, r, Panel.Text, Canvas.Font, Enabled, True, AntiAlias, True, False);
      end;
    end;
  end; {of case}
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.SetPanels(Value: TAdvOfficeStatusPanels);
begin
  FPanels.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.SetShowSplitter(const Value: Boolean);
begin
  if (FShowSplitter <> Value) then
  begin
    FShowSplitter := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.SetSimplePanel(Value: Boolean);
begin
  if FSimplePanel <> Value then
  begin
    FSimplePanel := Value;
    if HandleAllocated then
      SendMessage(Handle, SB_SIMPLE, Ord(FSimplePanel), 0);
    Invalidate;  
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.DoRightToLeftAlignment(var Str: string;
  AAlignment: TAlignment; ARTLAlignment: Boolean);
begin
  if ARTLAlignment then ChangeBiDiModeAlignment(AAlignment);

  case AAlignment of
    taCenter: Insert(#9, Str, 1);
    taRightJustify: Insert(#9#9, Str, 1);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.UpdateSimpleText;
const
  RTLReading: array[Boolean] of Longint = (0, SBT_RTLREADING);
begin
  DoRightToLeftAlignment(FSimpleText, taLeftJustify, UseRightToLeftAlignment);
  if HandleAllocated then
    SendMessage(Handle, SB_SETTEXT, 255 or RTLREADING[UseRightToLeftReading],
      LParam(PChar(FSimpleText)));
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.SetSimpleText(const Value: string);
begin
  if FSimpleText <> Value then
  begin
    FSimpleText := Value;
    UpdateSimpleText;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------


procedure TAdvOfficeStatusBar.CMBiDiModeChanged(var Message: TMessage);
var
  Loop: Integer;
begin
  inherited;
  if HandleAllocated then
    if not SimplePanel then
    begin
      for Loop := 0 to Panels.Count - 1 do
        if Panels[Loop].ParentBiDiMode then
          Panels[Loop].ParentBiDiModeChanged;
      UpdatePanels(True, True);
    end
    else
      UpdateSimpleText;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.FlipChildren(AllLevels: Boolean);
var
  Loop, FirstWidth, LastWidth: Integer;
  APanels: TAdvOfficeStatusPanels;
begin
  if HandleAllocated and
    (not SimplePanel) and (Panels.Count > 0) then
  begin
    { Get the true width of the last panel }
    LastWidth := ClientWidth;
    FirstWidth := Panels[0].Width;
    for Loop := 0 to Panels.Count - 2 do Dec(LastWidth, Panels[Loop].Width);
    { Flip 'em }
    APanels := TAdvOfficeStatusPanels.Create(Self);
    try
      for Loop := 0 to Panels.Count - 1 do with APanels.Add do
          Assign(Self.Panels[Loop]);
      for Loop := 0 to Panels.Count - 1 do
        Panels[Loop].Assign(APanels[Panels.Count - Loop - 1]);
    finally
      APanels.Free;
    end;
    { Set the width of the last panel }
    if Panels.Count > 1 then
    begin
      Panels[Panels.Count - 1].Width := FirstWidth;
      Panels[0].Width := LastWidth;
    end;
    UpdatePanels(True, True);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.SetSizeGrip(Value: Boolean);
begin
  if FSizeGrip <> Value then
  begin
    FSizeGrip := Value;
    RecreateWnd;
  end;
end;

procedure TAdvOfficeStatusBar.SetStretchPanel(const Value: Integer);
begin
  if (Value >= -1) and (Value < Panels.Count) then
  begin
    FStretchPanel := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.SyncToSystemFont;
begin
  if (csLoading in ComponentState) then
    Exit;

  if FUseSystemFont then
    Font := Screen.HintFont;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.UpdatePanel(Index: Integer; Repaint: Boolean);
var
  Flags: Integer;
  S: string;
  PanelRect: TRect;
begin
  if HandleAllocated then
    with Panels[Index] do
    begin
      if not Repaint then
      begin
        FUpdateNeeded := True;
        if SimplePanel then
        begin
          Invalidate;
        end
        else
        begin
          //SendMessage(Handle, SB_GETRECT, Index, Integer(@PanelRect));
          PanelRect := GetPanelRect(Index);
          InvalidateRect(Handle, @PanelRect, True);
        end;
        Exit;
      end
      else
        if not FUpdateNeeded then Exit;
      FUpdateNeeded := False;
      Flags := 0;
      case Bevel of
        pbNone: Flags := SBT_NOBORDERS;
        pbRaised: Flags := SBT_POPOUT;
      end;
      if UseRightToLeftReading then Flags := Flags or SBT_RTLREADING;
      
      if (Style in [psHTML, psOwnerDraw, psProgress, psImage, psImageList, psAnimation, psEllipsText, psFileEllipsText]) or not Enabled then
        Flags := Flags or SBT_OWNERDRAW;
      S := Text;
      if UseRightToLeftAlignment then
        DoRightToLeftAlignment(S, Alignment, UseRightToLeftAlignment)
      else
        case Alignment of
          taCenter: Insert(#9, S, 1);
          taRightJustify: Insert(#9#9, S, 1);
        end;
      SendMessage(Handle, SB_SETTEXT, Index or Flags, LParam(PChar(S)));
    end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.UpdatePanels(UpdateRects, UpdateText: Boolean);
const
  MaxPanelCount = 128;
var
  I, Count, PanelPos: Integer;
  PanelEdges: array[0..MaxPanelCount - 1] of Integer;
begin
  if HandleAllocated then
  begin
    Count := Panels.Count;
    if UpdateRects then
    begin
      if Count > MaxPanelCount then Count := MaxPanelCount;
      if Count = 0 then
      begin
        PanelEdges[0] := -1;
        SendMessage(Handle, SB_SETPARTS, 1, LParam(@PanelEdges));
        SendMessage(Handle, SB_SETTEXT, 0, LParam(PChar('')));
      end else
      begin
        PanelPos := 0;
        for I := 0 to Count - 2 do
        begin
          Inc(PanelPos, Panels[I].Width);
          PanelEdges[I] := PanelPos;
        end;
        PanelEdges[Count - 1] := -1;
        SendMessage(Handle, SB_SETPARTS, Count, LParam(@PanelEdges));
      end;
    end;
    //for I := 0 to Count - 1 do
      //UpdatePanel(I, UpdateText);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.CMWinIniChange(var Message: TMessage);
begin
  inherited;
  if (Message.WParam = 0) or (Message.WParam = SPI_SETNONCLIENTMETRICS) then
    SyncToSystemFont;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.WMGetTextLength(var Message: TWMGetTextLength);
begin
  Message.Result := Length(FSimpleText);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.UpdateMe(PropID: Integer);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.UpdateStatusBar;
var
  i: Integer;
  s: string;
begin
  if (csDestroying in ComponentState) then
    Exit;
    
  for i := 1 to Panels.Count do
  begin
    case Panels[i - 1].Style of
      psHTML: if (pos('<BLINK', uppercase(Panels[i - 1].Text)) <> 0) and (FTimerCount in [5, 10]) then
        begin
          s := Panels[i - 1].Text;
          Panels[i - 1].Text := '';
          Panels[i - 1].Text := s;
        end;
      psAnimation:
        begin
          if Assigned(Panels[i - 1].AnimationImages) then
          begin
            if FTimerCount mod (Panels[i - 1].AnimationDelay + 1) = 0 then
            begin
              if Panels[i - 1].FAnimIndex < Panels[i - 1].AnimationImages.Count - 1 then
                Panels[i - 1].FAnimIndex := Panels[i - 1].FAnimIndex + 1
              else
                Panels[i - 1].FAnimIndex := 0;

              s := Panels[i - 1].Text;
              Panels[i - 1].Text := '*';
              Panels[i - 1].Text := s;
            end;  
          end;

        end;
      psTime: Panels[i - 1].Text := FormatDateTime(Panels[i - 1].TimeFormat, Now);
      psDate: Panels[i - 1].Text := FormatDateTime(Panels[i - 1].DateFormat, Now);
      psNumLock:
        begin
          if getkeystate(vk_numlock) and $1 = $1 then
            Panels[i - 1].Text := NUMLOCK
          else
            Panels[i - 1].Text := ''
        end;
      psCapsLock:
        begin
          if getkeystate(vk_capital) and $1 = $1 then
            Panels[i - 1].Text := CAPSLOCK
          else
            Panels[i - 1].Text := ''
        end;
      psScrollLock:
        begin
          if getkeystate(vk_scroll) and $1 = $1 then
            Panels[i - 1].Text := SCROLLLOCK
          else
            Panels[i - 1].Text := ''
        end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.WMTimer(var Msg: TWMTimer);
begin
  UpdateStatusBar;
  inc(FTimerCount);
  if (FTimerCount > 10) then
    FTimerCount := 0;
end;

procedure TAdvOfficeStatusBar.WndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_DESTROY) then
  begin
    KillTimer(Handle, FTimerID);
  end;
  inherited;
end;

procedure TAdvOfficeStatusBar.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  FHitTest := Point(Msg.XPos, Msg.YPos);

  if (OnPanelBorder(Msg.XPos) <> -1) or (FSizePanel <> -1) then
    Msg.Result := 1
  else
    Msg.Result := 0;  
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.WMSetCursor(var Msg: TWMSetCursor);
var
  Cur: HCURSOR;
begin
  if (csDesigning in ComponentState) then
  begin
    Cur := 0;
    if OnPanelBorder(FHitTest.X) <> -1 then
    begin
      Cur := Screen.Cursors[crHSplit];
    end;

    if Cur <> 0 then
      SetCursor(Cur)
    else
      inherited;
  end
  else
    inherited;
end;

procedure TAdvOfficeStatusBar.WMSize(var Message: TWMSize);
begin
  // Eat WM_SIZE message to prevent control from doing alignment
  if not (csLoading in ComponentState) then
  begin
    inherited;
    //Resize;
  end;
  Repaint;
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusBar.IsFontStored: Boolean;
begin
  Result := not FUseSystemFont and not ParentFont and not DesktopFont;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.SetUseSystemFont(const Value: Boolean);
begin
  if FUseSystemFont <> Value then
  begin
    FUseSystemFont := Value;
    if Value then
    begin
      if ParentFont then ParentFont := False;
      SyncToSystemFont;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.CMColorChanged(var Message: TMessage);
begin
  inherited;
  RecreateWnd;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  if FUseSystemFont and ParentFont then FUseSystemFont := False;
end;

//------------------------------------------------------------------------------


function TAdvOfficeStatusBar.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if AutoHint and (Action is THintAction) and not DoHint then
  begin
    if SimplePanel or (Panels.Count = 0) then
      SimpleText := THintAction(Action).Hint else
      Panels[0].Text := THintAction(Action).Hint;
    Result := True;
  end
  else Result := inherited ExecuteAction(Action);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  RecreateWnd;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.CMSysFontChanged(var Message: TMessage);
begin
  inherited;
  SyncToSystemFont;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.AutoSizePanels;
var
  i: integer;
  w,imgw: integer;
  s: string;
  DTSTYLE: DWORD;
  r, re: TRect;
  anchor, stripped: string;
  xsize, ysize: Integer;
  HyperLinks,MouseLink: Integer;
  Focusanchor: string;

begin
  Canvas.Font.Assign(Font);
  for i := 0 to Panels.Count - 1 do
  begin
    if Panels[i].AutoSize then
    begin
      s := '';
      imgw := 0;

      case Panels[i].Style of
      psText,psDate,psTime:
        begin
          s := Panels[i].Text;
        end;
      psScrollLock:
        begin
          s := SCROLLLOCK;
        end;
      psNumLock:
        begin
          s := NUMLOCK;
        end;
      psCapsLock:
        begin
          s := CAPSLOCK;
        end;
      psImage:
        begin
          s := Panels[i].Text;
          if Assigned(Images) and (Panels[i].ImageIndex >= 0) then
           imgw := Images.Width;
        end;
      psHTML:
        begin
          r := Rect(0,0,1000,100);
          HTMLDrawEx(Canvas, Panels[i].Text, r, FImages, 0, 0, -1, -1, 1, true, false, false, false, (fTimerCount > 5), false,
            true, 1.0, URLColor, clNone, clNone, clGray, anchor, stripped, focusanchor, xsize, ysize,
            hyperlinks, mouselink, re, nil , FContainer, 0);
          Panels[i].Width := Max(xsize + 10, Panels[i].MinWidth);
        end;
      end;

      if (s <> '') then
      begin
        DTSTYLE := DT_SINGLELINE or DT_END_ELLIPSIS or DT_VCENTER;
        r := Rect(0,0,1000,100);
        r := DrawVistaText(Canvas, Panels[i].Alignment, DTSTYLE, r, s, Canvas.Font, Enabled, False, AntiAlias, True, False);
        w := imgw + (r.Right - r.Left) + 12;

        if SizeGrip and (i = Panels.Count - 1) then
          Panels[i].Width := Max(w, Panels[i].MinWidth) + 16
        else
          Panels[i].Width := Max(w, Panels[i].MinWidth);
      end;
    end;
  end;
end;

procedure TAdvOfficeStatusBar.ChangeScale(M, D: Integer);
begin
  if UseSystemFont then // status bar size based on system font size
    ScalingFlags := [sfTop];
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.SetDisabledImages(const Value: TImageList);
begin
  FDisabledImages := Value;
  Invalidate;
end;

procedure TAdvOfficeStatusBar.SetImages(const Value: TImageList);
begin
  FImages := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.SetOfficeStatusBarStyler(const Value: TCustomAdvOfficeStatusBarStyler);
begin
  if (FOfficeStatusBarStyler <> Value) or (Value = nil) then
  begin
    if Assigned(FOfficeStatusBarStyler) and (FOfficeStatusBarStyler <> FInternalOfficeStatusBarStyler) then
      FOfficeStatusBarStyler.RemoveControl(self);

    FOfficeStatusBarStyler := Value;

    if FOfficeStatusBarStyler = nil then
    begin
      FCurrentOfficeStatusBarStyler := FInternalOfficeStatusBarStyler;
    end
    else
    begin
      FCurrentOfficeStatusBarStyler := FOfficeStatusBarStyler;
      FOfficeStatusBarStyler.AddControl(self);
    end;

    UpdateMe(0);
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  i: Integer;
begin

  if (AOperation = opRemove) and not (csDestroying in ComponentState) then
  begin
    if (AComponent = FImages) then
      FImages := nil;

    if (AComponent = FDisabledImages) then
      FDisabledImages := nil;

    if AComponent = Styler then
    begin
      FOfficeStatusBarStyler := nil;
      FCurrentOfficeStatusBarStyler := FInternalOfficeStatusBarStyler;
      Invalidate;
      //Styler := nil;
    end;

    if Assigned(Panels) then
    begin
      for i := 1 to Panels.Count do
      begin
        if AComponent = Panels[i - 1].AnimationImages then
          Panels[i - 1].AnimationImages := nil;
      end;
    end;
  end;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;
    
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  anchor,hint: string;
  idx, i: Integer;
begin
  if (csDesigning in ComponentState) then
  begin
    if FSizePanel <> -1 then
    begin
      Panels[FSizePanel].Width := FSizeWidth + X - FSizeDownX;
    end;
  end;

  anchor := IsAnchor(x, y,hint);

  idx := GetPanel(x);

  if fMousePanel <> idx then
  begin
    Application.CancelHint;
    FMousePanel := idx;
  end;

  if (idx >= 0) then
  begin
    if (FHotPanelIndex <> Idx) then
    begin
      if (FHotPanelIndex >= 0) then
      begin
        i := FHotPanelIndex;
        FHotPanelIndex := -1;
        InvalidatePanel(i);
      end;

      if (Panels.Items[idx].Button) and (Y < Height) and (y > 0) then
      begin
        FHotPanelIndex := idx;
        InvalidatePanel(FHotPanelIndex);
      end;
    end;
  end
  else
  begin
    if (FHotPanelIndex >= 0) then
    begin
      i := FHotPanelIndex;
      FHotPanelIndex := -1;
      InvalidatePanel(i);
    end;
  end;

  if (Anchor <> '') then
  begin
    if (self.Cursor = crDefault) or (fAnchor <> Anchor) then
    begin
      FAnchor := Anchor;
      Cursor := crHandPoint;
      if FAnchorHint then
        Application.CancelHint;
      if Assigned(FAnchorEnter) then
        FAnchorEnter(Self, Anchor);
    end;
  end
  else
  begin
    if (self.Cursor = crHandPoint) and (FOldCursor <> crHandpoint) then
    begin
      self.Cursor := crDefault;
      if assigned(fAnchorExit) then fAnchorExit(self, anchor);
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusBar.IsAnchor(x, y: integer; var title: string): string;
var
  r: trect;
  xsize, ysize: integer;
  anchor, stripped: string;
  idx: integer;

  HyperLinks,MouseLink: Integer;
  Focusanchor: string;
  re: TRect;
  AText: String;
  sp: TAdvOfficeStatusPanel;
begin
  if SimplePanel then
  begin
    r := GetSimplePanelRect;
    AText := SimpleText;
  end
  else
  begin
    idx := GetPanel(x);
    if (idx < 0) then
      Exit;

    r := GetPanelRect(Idx);
    sp := Panels.Items[idx];
    r.Left := r.Left + sp.HTMLOffsetX + 2;
    r.Top := r.Top + sp.HTMLOffsetY + 2;
    r.Right := r.Right - 2;

    if (idx = Panels.Count - 1) and HasSizeGrip then
      r.Right := r.Right - GRIP_SIZE;

    AText := sp.Text;

    if sp.Alignment in [taRightJustify, taCenter] then
    begin
      HTMLDrawEx(Canvas, AText, r, fImages, 0, 0, -1, -1, 1, true, false, false, false, false, false,
        true, 1.0, URLColor, clNone, clNone, clGray, anchor, stripped, focusanchor, xsize, ysize,
        hyperlinks, mouselink, re, nil , FContainer, 0);


      if (sp.Alignment = taRightJustify) then
      begin
        if xsize < (r.Right - r.Left) then
          r.Left := r.Right - xsize - sp.HTMLOffsetX
      end
      else
      begin
        if Xsize < (r.Right - r.Left) then
          r.Left := r.Left + (r.Right - r.Left - Xsize) div 2;
      end;
    end;
  end;

  Anchor := '';

  if HTMLDrawEx(Canvas, AText, r, FImages, x, y, -1, -1, 1, true, false, false, true, true, false, true,
     1.0, FURLColor, clNone, clNone, clGray, anchor, stripped, focusanchor, xsize, ysize, hyperlinks,
     mouselink, re, nil, FContainer, 0) then
  begin
    Title := FocusAnchor;
    Result := anchor;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusBar.GetPanel(x: integer): integer;
var
  r: TRect;
  i: Integer;
begin
  Result := -1;
  for i := 1 to panels.Count do
  begin
    r := GetPanelRect(i - 1);
    if (x >= r.left) and (x <= r.right) then
    begin
      Result := i - 1;
      break;
    end;
  end;
end;

//------------------------------------------------------------------------------


procedure TAdvOfficeStatusBar.CMHintShow(var Msg: TMessage);
var
  CanShow: Boolean;
  hi: PHintInfo;
  anchor,hint: string;
  idx: integer;
begin
  CanShow := True;
  hi := PHintInfo(Msg.LParam);

  idx := GetPanel(hi^.cursorPos.x);

  if (idx >= 0) and (idx < Panels.Count) then
  begin
    if Panels[idx].Style = psHTML then
    begin
      if FAnchorHint then
      begin
        anchor := IsAnchor(hi^.cursorPos.x, hi^.cursorpos.y, hint);

        if (anchor <> '') or (hint <> '') then
        begin
          hi^.HintPos := clienttoscreen(hi^.CursorPos);
          hi^.hintpos.y := hi^.hintpos.y - 10;
          hi^.hintpos.x := hi^.hintpos.x + 10;

          FDummyHintControl.OfficeHint.Assign(Panels[idx].OfficeHint);
          FDummyHintControl.OfficeHint.Title := '';

          if hint <> '' then
          begin
            hi^.HintStr := hint;
            FDummyHintControl.OfficeHint.Notes.Text := hint
          end
          else
          begin
            hi^.HintStr := anchor;
            FDummyHintControl.OfficeHint.Notes.Text := anchor;
          end;

          hi^.HintControl := FDummyHintControl;
        end
        else
        begin
          FDummyHintControl.OfficeHint.Assign(Panels[idx].OfficeHint);
          hi^.HintControl := FDummyHintControl;
        end;
      end
      else
      begin
        FDummyHintControl.OfficeHint.Assign(Panels[idx].OfficeHint);
        hi^.HintControl := FDummyHintControl;
      end;
    end
    else
    begin
      FDummyHintControl.OfficeHint.Assign(Panels[idx].OfficeHint);
      hi^.HintControl := FDummyHintControl;
    end;
  end;
  Msg.Result := Ord(not CanShow);
end;

//------------------------------------------------------------------------------


//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  inherited;

  FSizePanel := -1;

  if (FDownPanelIndex >= 0) then
  begin
    i := FDownPanelIndex;
    FDownPanelIndex := -1;
    InvalidatePanel(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Anchor,Hint: string;
  idx, i: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  idx := GetPanel(x);

  if (csDesigning in ComponentState) then
  begin
    FSizePanel := OnPanelBorder(X);
    FSizeDownX := X;
    FSizeWidth := Panels[FSizePanel].Width;
  end;


  if (idx >= 0) then
  begin
    if (FDownPanelIndex <> Idx) then
    begin
      if (FDownPanelIndex >= 0) then
      begin
        i := FDownPanelIndex;
        FDownPanelIndex := -1;
        InvalidatePanel(i);
      end;

      if (Panels.Items[idx].Button) and (Y < Height) and (y > 0) then
      begin
        FDownPanelIndex := idx;
        InvalidatePanel(FDownPanelIndex);
      end;
    end;
  end
  else
  begin
    if (FDownPanelIndex >= 0) then
    begin
      i := FDownPanelIndex;
      FDownPanelIndex := -1;
      InvalidatePanel(i);
    end;
  end;

  if Assigned(OnPanelCLick) and (Button = mbLeft) then
    OnPanelClick(Self, Idx);

  if Assigned(OnPanelRightCLick) and (Button = mbRight) then
    OnPanelRightClick(Self, Idx);

  Anchor := IsAnchor(X, Y, Hint);
  if Anchor <> '' then
  begin
    if (Pos('://', anchor) > 0) or (pos('mailto:', anchor) > 0) then
      ShellExecute(0, 'open', pchar(anchor), nil, nil, SW_NORMAL)
    else
    begin
      if Assigned(fAnchorClick) then
        FAnchorClick(self, anchor);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.Loaded;
begin
  inherited;
  FOldCursor := Cursor;
  UpdateStatusBar;
  SyncToSystemFont;
  AutoSizePanels;
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusBar.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TAdvOfficeStatusBar.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  idx: Integer;
begin
  inherited;
  if Assigned(OnPanelDblClick) then
  begin
    idx := GetPanel(Message.XPos);
    OnPanelDblClick(Self, idx);
  end;
end;



//------------------------------------------------------------------------------

function IsVista: boolean;
var
  hKernel32: HMODULE;
begin
  hKernel32 := GetModuleHandle('kernel32');
  if (hKernel32 > 0) then
  begin
    Result := GetProcAddress(hKernel32, 'GetLocaleInfoEx') <> nil;
  end
  else
    Result := false;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.Paint;
{$IFDEF TMSTOOLBAR}
var
  clr: TColor;
  i: integer;
{$ENDIF}
begin
//  inherited;

  DrawAllPanels;

  {$IFDEF TMSTOOLBAR}
  clr := clGray;

  if Parent is TAdvToolBarForm then
  begin
    with Parent as TAdvToolBarForm do
    begin
      for i := 0 to ControlCount - 1 do
      begin
        if Controls[i] is TAdvToolBarPager then
        begin
          with (TInternalToolBarPager(Controls[i])) do
          begin
            clr := CurrentToolBarStyler.CaptionAppearance.CaptionTextColor;
          end;
        end;
      end;
    end;

    if Assigned(Styler) then
    begin
      if Styler.BorderColor <> clNone then
        clr := Styler.BorderColor;
    end;

    if not IsVista then
    begin
      if clr = clWhite then
        clr := clBlack;

      Canvas.Pen.Color := clr;
      Canvas.Pen.Width := 1;

      Canvas.MoveTo(0,Height - 7);
      Canvas.LineTo(0,Height);
      Canvas.MoveTo(1,Height - 4);
      Canvas.LineTo(1,Height);

      Canvas.MoveTo(0,Height - 1);
      Canvas.LineTo(7,Height - 1);
      Canvas.MoveTo(1,Height - 2);
      Canvas.LineTo(4,Height - 2);

      Canvas.MoveTo(Width - 1,Height - 7);
      Canvas.LineTo(Width - 1,Height);
      Canvas.MoveTo(Width - 2,Height - 4);
      Canvas.LineTo(Width - 2,Height);

      Canvas.MoveTo(Width - 1,Height - 1);
      Canvas.LineTo(Width - 8,Height - 1);
      Canvas.MoveTo(Width - 1,Height - 2);
      Canvas.LineTo(Width - 5,Height - 2);
    end;
  end;
  {$ENDIF}

  if Assigned(Styler) then
  begin
    if Styler.BorderColor <> clNone then
    begin
      Canvas.Pen.Color := Styler.BorderColor;
      Canvas.Pen.Width := 1;
      Canvas.MoveTo(0,0);
      Canvas.LineTo(Width,0);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.OnPanelsChanged(Sender: TObject);
begin
  if (Panels.UpdateCount = 0) then
    Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.WMNCHitTest(var Msg: TWMNCHitTest);
var
  pt: TPoint;
  frm: TCustomForm;
  tmsif: ITMSMetro;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  frm := GetParentForm(Self);

  pt := ScreenToClient(point(msg.xpos,msg.ypos));

  if Assigned(frm)  then
  begin
    if not frm.GetInterface(ITMSMetro, tmsif) then
    begin
      if not (frm.BorderStyle in [bsSizeable, bsSizeToolWin]) or FMetro then
        Exit;
    end
    else
    begin
        Msg.Result := HTCLIENT;
      if (pt.Y > Height - 4) or (pt.X > Width - 4) then
      begin
        Msg.Result := HTTRANSPARENT;
        Exit;
      end;
    end;
  end;

  if (pt.y > height - GetSystemMetrics(SM_CYHSCROLL)) and
     (pt.x > width - GetSystemMetrics(SM_CXHSCROLL)) and
     (Msg.Result = htClient) and HasSizeGrip then
  begin
    SetWindowPos(Handle, HWND_TOP,0,0,0,0,  SWP_NOMOVE or SWP_NOSIZE);
    Msg.Result := HTBOTTOMRIGHT;
  end
  else
  begin
    if (pt.Y > Height - 4) or (pt.X > Width - 4) then
      Msg.Result := HTTRANSPARENT;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.CMMouseLeave(var Message: TMessage);
var
  i: Integer;
begin
  inherited;
  if (FDownPanelIndex >= 0) then
  begin
    i := FDownPanelIndex;
    FDownPanelIndex := -1;
    InvalidatePanel(i);
  end;

  if (FHotPanelIndex >= 0) then
  begin
    i := FHotPanelIndex;
    FHotPanelIndex := -1;
    InvalidatePanel(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeStatusBar.SetAntiAlias(const Value: TAntiAlias);
begin
  FAntiAlias := Value;
  Invalidate;
end;

procedure TAdvOfficeStatusBar.SetColorTones(ATones: TColorTones);
begin
  FMetro := not IsClearTones(ATones);
  FCurrentOfficeStatusBarStyler.BorderColor := ATones.Background.BorderColor;

  { PanelAppearanceLight }
  FCurrentOfficeStatusBarStyler.PanelAppearanceLight.BorderColor := clNone; //ATones.Background.BorderColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceLight.Color := ATones.Background.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceLight.ColorTo := ATones.Background.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceLight.ColorMirror := ATones.Background.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceLight.ColorMirrorTo := ATones.Background.BrushColor;

  FCurrentOfficeStatusBarStyler.PanelAppearanceLight.TextColor := ATones.Background.TextColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceLight.TextColorHot := ATones.Hover.TextColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceLight.TextColorDown := ATones.Selected.TextColor;

  FCurrentOfficeStatusBarStyler.PanelAppearanceLight.ColorHot := ATones.Hover.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceLight.ColorHotTo := ATones.Hover.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceLight.ColorMirrorHot := ATones.Hover.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceLight.ColorMirrorHotTo := ATones.Hover.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceLight.BorderColorHot := ATones.Hover.BorderColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceLight.ColorDown := ATones.Selected.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceLight.ColorDownTo := ATones.Selected.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceLight.ColorMirrorDown := ATones.Selected.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceLight.ColorMirrorDownTo := ATones.Selected.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceLight.BorderColorDown := ATones.Selected.BorderColor;

  { PanelAppearanceDark }
  FCurrentOfficeStatusBarStyler.PanelAppearanceDark.BorderColor := clNone; //ATones.Foreground.BorderColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceDark.Color := ATones.Foreground.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceDark.ColorTo := ATones.Foreground.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceDark.ColorMirror := ATones.Foreground.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceDark.ColorMirrorTo := ATones.Foreground.BrushColor;

  FCurrentOfficeStatusBarStyler.PanelAppearanceDark.TextColor := ATones.Foreground.TextColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceDark.TextColorHot := ATones.Hover.TextColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceDark.TextColorDown := ATones.Selected.TextColor;

  FCurrentOfficeStatusBarStyler.PanelAppearanceDark.ColorHot := ATones.Hover.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceDark.ColorHotTo := ATones.Hover.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceDark.ColorMirrorHot := ATones.Hover.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceDark.ColorMirrorHotTo := ATones.Hover.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceDark.BorderColorHot := ATones.Hover.BorderColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceDark.ColorDown := ATones.Selected.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceDark.ColorDownTo := ATones.Selected.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceDark.ColorMirrorDown := ATones.Selected.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceDark.ColorMirrorDownTo := ATones.Selected.BrushColor;
  FCurrentOfficeStatusBarStyler.PanelAppearanceDark.BorderColorDown := ATones.Selected.BorderColor;
end;

//------------------------------------------------------------------------------

{ TProgressStyle }

procedure TProgressStyle.Assign(Source: TPersistent);
begin
  FBackGround := (Source as TProgressStyle).Background;
  FIndication := (Source as TProgressStyle).Indication;
  FMin := (Source as TProgressStyle).Min;
  FMax := (Source as TProgressStyle).Max;
  FPosition := (Source as TProgressStyle).Position;
  FShowPercentage := (Source as TProgressStyle).ShowPercentage;
  FShowGradient := (Source as TProgressStyle).ShowGradient;
  FStacked := (Source as TProgressStyle).Stacked;
  FShowBorder := (Source as TProgressStyle).ShowBorder;
  FBorderColor := (Source as TProgressStyle).BorderColor;
  FCompletionSmooth := (Source as TProgressStyle).CompletionSmooth;
  FLevel0Color := (Source as TProgressStyle).Level0Color;
  FLevel0ColorTo := (Source as TProgressStyle).Level0ColorTo;
  FLevel1Color := (Source as TProgressStyle).Level1Color;
  FLevel1ColorTo := (Source as TProgressStyle).Level1ColorTo;
  FLevel2Color := (Source as TProgressStyle).Level2Color;
  FLevel2ColorTo := (Source as TProgressStyle).Level2ColorTo;
  FLevel3Color := (Source as TProgressStyle).Level3Color;
  FLevel3ColorTo := (Source as TProgressStyle).Level3ColorTo;
  FLevel1Perc := (Source as TProgressStyle).Level1Perc;
  FLevel2Perc := (Source as TProgressStyle).Level2Perc;
  FPrefix := (Source as TProgressStyle).Prefix;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.ChangedPerc;
var
  FNewPerc: integer;
begin
  if (Max - Min > 0) then
  begin
    FNewPerc := Round(100 * (FPosition / (Max - Min)));
    if FNewPerc <> FOldPerc then
      Changed;
    FOldPerc := FNewPerc;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.Changed;
var
  r: trect;
begin
  r := TAdvOfficeStatusPanels(FOwner.Collection).fStatusbar.GetPanelRect(fOwner.index);
  InvalidateRect(TAdvOfficeStatusPanels(fOwner.Collection).fStatusbar.handle, @r, false);
end;

//------------------------------------------------------------------------------

constructor TProgressStyle.Create(aOwner: TAdvOfficeStatusPanel);
begin
  inherited Create;
  FBackground := clNone;
  FMin := 0;
  FMax := 100;
  FOwner := aOwner;

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
  FShowGradient := True;
  FCompletionSmooth := False;
  FShowPercentage := True;
end;

//------------------------------------------------------------------------------

destructor TProgressStyle.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetBackGround(const Value: tColor);
begin
  FBackground := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetIndication(const Value: TProgressIndication);
begin
  FIndication := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetMax(const Value: integer);
begin
  if (FMax <> Value) then
  begin
    FMax := Value;
    ChangedPerc;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetMin(const Value: integer);
begin
  if (FMin <> Value) then
  begin
    FMin := Value;
    ChangedPerc;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetPosition(const Value: integer);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    ChangedPerc;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetLevel0Color(const Value: tColor);
begin
  FLevel0Color := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetLevel0ColorTo(const Value: tColor);
begin
  FLevel0ColorTo := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetLevel1Color(const Value: tColor);
begin
  FLevel1Color := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetLevel1ColorTo(const Value: tColor);
begin
  FLevel1ColorTo := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetLevel2Color(const Value: tColor);
begin
  FLevel2Color := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetLevel2ColorTo(const Value: tColor);
begin
  FLevel2ColorTo := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetLevel3Color(const Value: tColor);
begin
  FLevel3Color := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetLevel3ColorTo(const Value: tColor);
begin
  FLevel3ColorTo := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetLevel1Perc(Value: integer);
begin
  FLevel1Perc := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetLevel2Perc(Value: integer);
begin
  FLevel2Perc := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetBorderColor(const Value: tColor);
begin
  FBorderColor := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetShowBorder(Value: boolean);
begin
  FShowBorder := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetStacked(Value: boolean);
begin
  FStacked := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetShowPercentage(Value: boolean);
begin
  FShowPercentage := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetCompletionSmooth(Value: boolean);
begin
  FCompletionSmooth := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetShowGradient(Value: boolean);
begin
  FShowGradient := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetPrefix(const Value: string);
begin
  FPrefix := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TProgressStyle.SetSuffix(const Value: string);
begin
  FSuffix := Value;
  Changed;
end;

//------------------------------------------------------------------------------

{ TVistaBackground }

constructor TVistaBackground.Create;
begin
  inherited;
  FSteps := 64;
  FColor := clWhite;
  FColorHot := clWhite;
  FColorDown := clWhite;
  FColorTo := clWhite;
  FColorHotTo := clWhite;
  FColorDownTo := clWhite;
  FColorMirror := clSilver;
  FColorMirrorHot := clSilver;
  FColorMirrorDown := clSilver;
  FColorMirrorTo := clWhite;
  FColorMirrorHotTo := clWhite;
  FColorMirrorDownTo := clWhite;
  FBorderColor := clNone;
  FBorderColorHot := clGray;
  FBorderColorDown := clGray;
  FTextColor := clBlack;
  FTextColorHot := clBlack;
  FTextColorDown := clBlack;
  FTextStyle := [];
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.Assign(Source: TPersistent);
begin
  if (Source is TVistaBackground) then
  begin
    FSteps := (Source as TVistaBackground).Steps;
    FColor := (Source as TVistaBackground).Color;
    FColorHot := (Source as TVistaBackground).ColorHot;
    FColorDown := (Source as TVistaBackground).ColorDown;
    FColorTo := (Source as TVistaBackground).ColorTo;
    FColorHotTo := (Source as TVistaBackground).ColorHotTo;
    FColorDownTo := (Source as TVistaBackground).ColorDownTo;
    FColorMirror := (Source as TVistaBackground).ColorMirror;
    FColorMirrorHot := (Source as TVistaBackground).ColorMirrorHot;
    FColorMirrorDown := (Source as TVistaBackground).ColorMirrorDown;
    FColorMirrorTo := (Source as TVistaBackground).ColorMirrorTo;
    FColorMirrorHotTo := (Source as TVistaBackground).ColorMirrorHotTo;
    FColorMirrorDownTo := (Source as TVistaBackground).ColorMirrorDownTo;
    FBorderColor := (Source as TVistaBackground).BorderColor;
    FBorderColorHot := (Source as TVistaBackground).BorderColorHot;
    FBorderColorDown := (Source as TVistaBackground).BorderColorDown;
    FTextColor := (Source as TVistaBackground).FTextColor;
    FTextColorHot := (Source as TVistaBackground).FTextColorHot;
    FTextColorDown := (Source as TVistaBackground).FTextColorDown;
    TextStyle := (Source as TVistaBackground).FTextStyle;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColorTo(const Value: TColor);
begin
  if (FColorTo  <> Value) then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColorMirror(const Value: TColor);
begin
  if (FColorMirror <> Value) then
  begin
    FColorMirror := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColorMirrorTo(const Value: TColor);
begin
  if (FColorMirrorTo <> Value) then
  begin
    FColorMirrorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------


procedure TVistaBackground.SetSteps(const Value: Integer);
begin
  if (FSteps <> Value) then
  begin
    FSteps := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetTextColor(const Value: TColor);
begin
  if (FTextColor <> Value) then
  begin
    FTextColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetTextStyle(const Value: TFontStyles);
begin
  FTextStyle := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetBorderColorDown(const Value: TColor);
begin
  if (FBorderColorDown <> Value) then
  begin
    FBorderColorDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetBorderColorHot(const Value: TColor);
begin
  if (FBorderColorHot <> Value) then
  begin
    FBorderColorHot := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColorDown(const Value: TColor);
begin
  if (FColorDown <> Value) then
  begin
    FColorDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColorDownTo(const Value: TColor);
begin
  if (FColorDownTo <> Value) then
  begin
    FColorDownTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColorHot(const Value: TColor);
begin
  FColorHot := Value;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColorHotTo(const Value: TColor);
begin
  FColorHotTo := Value;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColorMirrorDown(const Value: TColor);
begin
  if (FColorMirrorDown <> Value) then
  begin
    FColorMirrorDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColorMirrorDownTo(const Value: TColor);
begin
  if (FColorMirrorDownTo <> Value) then
  begin
    FColorMirrorDownTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColorMirrorHot(const Value: TColor);
begin
  FColorMirrorHot := Value;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColorMirrorHotTo(const Value: TColor);
begin
  FColorMirrorHotTo := Value;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetTextColorDown(const Value: TColor);
begin
  if (FTextColorDown <> Value) then
  begin
    FTextColorDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetTextColorHot(const Value: TColor);
begin
  FTextColorHot := Value;
end;

//------------------------------------------------------------------------------

{ TDbgList }

function TDbgList.GetItemsEx(Index: Integer): Pointer;
begin
  if (Index >= Count) then
  begin
    raise Exception.Create('Index out of bounds in list read access');
  end;

  if Index < Count then
    Result := inherited Items[Index]
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

procedure TDbgList.SetItemsEx(Index: Integer; const Value: Pointer);
begin
  if (Index >= Count) then
  begin
    raise Exception.Create('Index out of bounds in list write access');
  end;
  if Index < Count then
    inherited Items[Index] := value;
end;

//------------------------------------------------------------------------------

{ TCustomAdvOfficeStatusBarStyler }

procedure TCustomAdvOfficeStatusBarStyler.AddControl(AControl: TWinControl);
begin
  FControlList.Add(AControl);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeStatusBarStyler.Assign(Source: TPersistent);
begin
  if Source is TCustomAdvOfficeStatusBarStyler then
  begin
    AutoThemeAdapt := (Source as TCustomAdvOfficeStatusBarStyler).AutoThemeAdapt;
    PanelAppearanceLight.Assign((Source as TCustomAdvOfficeStatusBarStyler).PanelAppearanceLight);
    PanelAppearanceDark.Assign((Source as TCustomAdvOfficeStatusBarStyler).PanelAppearanceDark);
    BorderColor := (Source as TCustomAdvOfficeStatusBarStyler).BorderColor;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

constructor TCustomAdvOfficeStatusBarStyler.Create(AOwner: TComponent);
begin
  inherited;
  FControlList := TDbgList.Create;
  FBlendFactor := 50;

  FBorderColor := RGB(59, 90, 130);
  FPanelAppearanceLight := TVistaBackground.Create;
  FPanelAppearanceLight.OnChange := OnPanelAppearanceChanged;
  FPanelAppearanceDark := TVistaBackground.Create;
  FPanelAppearanceDark.OnChange := OnPanelAppearanceChanged;
end;

//------------------------------------------------------------------------------

destructor TCustomAdvOfficeStatusBarStyler.Destroy;
begin
  FControlList.Free;
  FPanelAppearanceLight.Free;
  FPanelAppearanceDark.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeStatusBarStyler.Change(PropID: integer);
var
  i: integer;
begin
  for i := 0 to FControlList.Count - 1 do
  begin
    if (TControl(FControlList[i]) is TAdvOfficeStatusBar) then
      TAdvOfficeStatusBar(FControlList[i]).UpdateMe(PropID);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeStatusBarStyler.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: integer;
begin
  inherited;
  if not (csDestroying in ComponentState) and (Operation = opRemove) then
  begin
    i := FControlList.IndexOf(AComponent);
    if i >= 0 then
      FControlList.Remove(AComponent);
  end;

end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeStatusBarStyler.RemoveControl(AControl: TWinControl);
var
  i: integer;
begin
  i := FControlList.IndexOf(AControl);
  if (i >= 0) then
    FControlList.Delete(i);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeStatusBarStyler.OnPanelAppearanceChanged(Sender: TObject);
begin
  Change(1);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeStatusBarStyler.Loaded;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeStatusBarStyler.SetPanelAppearanceLight(
  const Value: TVistaBackground);
begin
  FPanelAppearanceLight.Assign(Value);
end;

procedure TCustomAdvOfficeStatusBarStyler.ThemeChanged(Sender: TObject);
begin
//
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeStatusBarStyler.SetPanelAppearanceDark(
  const Value: TVistaBackground);
begin
  FPanelAppearanceDark.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeStatusBarStyler.SetAutoThemeAdapt(
  const Value: boolean);
begin
  if Value <> FAutoThemeAdapt then
  begin
    FAutoThemeAdapt := Value;
    ThemeChanged(Self);
  end;
end;

procedure TCustomAdvOfficeStatusBarStyler.SetBorderColor(
  const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Change(1);
  end;
end;

//------------------------------------------------------------------------------

{ TDummyHintControl }

constructor TDummyHintControl.Create(AOwner: TComponent);
begin
  inherited;
  FOfficeHint := TAdvHintInfo.Create;
end;

//------------------------------------------------------------------------------

destructor TDummyHintControl.Destroy;
begin
  FOfficeHint.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDummyHintControl.SetOfficeHint(const Value: TAdvHintInfo);
begin
  FOfficeHint.Assign(Value);
end;

//------------------------------------------------------------------------------

{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}


end.
