{***************************************************************************}
{ TAdvAlertWindow component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2004 - 2015                                        }
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

unit AdvAlertWindow;

interface

{$I TMSDEFS.INC}
{$DEFINE REMOVESTRIP}
{$DEFINE REMOVEDRAW}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  PictureContainer, Math, Dialogs, Menus, Registry, AdvStyleIF, ImgList, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  CloseBtnSize = 20;
  ScrollBtnHeight = 25;
  ScrollBtnWidth = 20;

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 9; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 6; // Build nr.

  // version history
  // 1.3.3.0 : OnShowMessage event added
  // 1.3.3.1 : improvement for correct font rendering
  // 1.3.3.2 : improvement for use from multiform applications
  // 1.3.3.3 : changed PopupLeft, PopupTop properties to get last alert position
  // 1.4.0.0 : Background image support added
  //           Mini scrollers support added
  //           Position persistence added
  //           Custom paint event added
  // 1.4.0.1 : fixed issue with autosize
  // 1.4.5.0 : style property added
  //         : new asWhidbey style added
  //         : fixed painting issue
  //         : improved anchor click handling with scroll text
  // 1.4.5.1 : improvement for conflict with OnHide, OnAutoHide events
  // 1.4.5.2 : Fixed design time issue with DisplayTime = 0
  // 1.4.5.3 : Fixed issue with hiding alert window
  // 1.4.5.4 : Fixed issue with auto hiding
  // 1.4.6.0 : New: OnAlertClick event added
  // 1.4.6.1 : Improvement in fade thread handling
  // 1.4.7.0 : Added URLColor property
  // 1.4.8.0 : New center position added
  // 1.4.8.1 : Improved handling of OnAlertClick
  // 1.5.0.0 : Added Office 2007 Luna, Obsidian, XP styles
  // 1.5.1.0 : New : Office2007 Silver style added
  // 1.5.2.0 : Exposed Handle property of the alert window
  //         : Improved alert window position control with AutoSize = true
  // 1.5.3.0 : Apply style change immediate when alertwindow is visible
  // 1.5.3.1 : Fixed issue in CloseAlert method
  // 1.5.3.2 : Fixed issue with Hide method & Fade thread
  // 1.5.4.0 : Improved : form creation to allow use of TAdvAlertWindow on datamodule
  // 1.5.4.1 : Fixed : issue with OnCanDelete event
  // 1.5.4.2 : Fixed : issue with Show call during auto hide
  // 1.5.4.3 : Fixed : issue with handling fade thread
  // 1.5.4.4 : Fixed : issue with programmatically deleting a message
  // 1.5.5.0 : New : method TAdvAlertWindow.DeleteMessage(index) exposed
  // 1.5.6.0 : New : exposed AlertWindow as public property
  // 1.5.6.1 : Improved : behaviour of AlwaysOnTop alert window with minimized apps
  // 1.5.6.2 : Improved : exception handling in WindowBlend function
  // 1.5.6.3 : Fixed : memory leak when app is closed when alert is fading
  // 1.5.6.4 : Improved : extra check if Application.MainForm exists when creating new Alert window
  // 1.5.6.5 : Improved : auto size calculation of alert window 
  // 1.6.0.0 : New : Terminal, Vista and Windows 7 styles added
  // 1.6.0.1 : Improved : timing of DisplayTime
  // 1.6.0.2 : Fixed : Issue with AutoThemeAdapt = true and application setting to have themes disabled
  // 1.6.1.0 : New : support for customizing bullets in HTML UL lists
  // 1.7.0.0 : New : Built in support for Office 2010 colors
  // 1.7.0.1 : Fixed : Property persistence of empty hint properties
  // 1.7.0.2 : Fixed : Issue with auto sizing when large images are used
  // 1.7.0.3 : Fixed : Issue with using on a datamodule when Application.MainForm is not assigned
  // 1.7.0.4 : Fixed : Exception wrt window handle under rare conditions
  // 1.7.1.0 : New : Support for Bidi right-to-left mode
  // 1.7.1.1 : Fixed : Issue with AutoSize when images are used
  // 1.8.0.0 : New: Method to create descendent classes of TAdvAlertWindow with custom TMsgCollectionItem collection item classes
  // 1.8.1.0 : New: Event OnHideMessage added
 //  1.8.2.0 : New : Windows 8, Office 2013 styles added
 //  1.8.3.0 : Delphi XE5 & C++Builder XE5 support
 //  1.8.3.1 : Improved : Usage on multimonitor systems
 //  1.9.0.0 : New : Windows 10, Office 2016 styles added
 //  1.9.0.1 : Fixed : Issue with handling AlwaysOnTop
 //  1.9.0.2 : Fixed : Issue with closing when AlwaysOnTop = true
 //  1.9.0.3 : Fixed : Rare issue with OnAnchorClick for alert with AlwaysOnTop = true
 //  1.9.0.4 : Fixed : Anchor detection in combination with paragraph alignments
 //  1.9.0.5 : Improved : Optimized anchor testing code
 //  1.9.0.6 : Improved : Focus handling when alert window appears

type
  TGradientDirection = (gdHorizontal, gdVertical);
  TAnchorClickEvent = procedure(Sender: TObject; Anchor: string; index: integer) of object;
  TDeleteMessageEvent = procedure(Sender: TObject; index: integer) of object;
  TCanCloseEvent = procedure(Sender: TObject; var CanClose: Boolean) of object;
  TCanDeleteEvent = procedure(Sender: TObject; Index: Integer; var CanDelete: Boolean) of object;

  TCustomPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect; var DefaultPaint: boolean) of object;

  TAdvAlertWindowStyle = (asOffice2003Blue, asOffice2003Silver, asOffice2003Olive, asOffice2003Classic, asOffice2007Luna, asOffice2007Obsidian, asWindowsXP, asWhidbey, asCustom, asOffice2007Silver, asWindowsVista, asWindows7, asTerminal, asOffice2010Blue, asOffice2010Silver, asOffice2010Black,
  asWindows8, asOffice2013White, asOffice2013LightGray, asOffice2013Gray,
  asWindows10, asOffice2016White, asOffice2016Gray, asOffice2016Black);

  TMsgScroller = class(TObject)
  private
    FMin: integer;
    FMax: integer;
    FPosition: integer;
    FVisible: Boolean;
    FOnChange: TNotifyEvent;
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
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TMsgCollection = class;

  TAlertWindow = class(THintWindow)
  private
    FWindowColor: TColor;
    FWindowColorTo: TColor;
    FGradientDir: TGradientDirection;
    FImageCache: THTMLPictureCache;
    FContainer: TPictureContainer;
    FHoverLink: Integer;
    FHover: Boolean;
    FHoverRect: TRect;
    FOnAnchorClick: TAnchorClickEvent;
    FImages: TImageList;
    FAutoHide: Boolean;
    FAutoSize: Boolean;
    FBorderSize: Integer;
    FAlwaysOnTop: Boolean;
    FMarginY: Integer;
    FMarginX: Integer;
    FHoverClose: Boolean;
    FDownClose: Boolean;
    FScrollLeftHover: Boolean;
    FScrollLeftDown: Boolean;
    FScrollRightHover: Boolean;
    FScrollRightDown: Boolean;
    FDeleteHover: Boolean;
    FDeleteDown: Boolean;
    FSelectedColor: TColor;
    FDragingStart: Boolean;
    FOldMouseX: integer;
    FOldMouseY: integer;
    FMsgScroller: TMsgScroller;
    FOnMouseLeave: TNotifyEvent;
    FAlertMessages: TStringList;
    FOnWindowMoved: TNotifyEvent;
    FHintNextBtn: string;
    FHintPrevBtn: string;
    FHintCloseBtn: string;
    FCaptionColorTo: TColor;
    FCaptionColor: TColor;
    FOriginalHint: string;
    FBorderColor: TColor;
    FBtnHoverColor: TColor;
    FBtnDownColor: TColor;
    FBtnHoverColorTo: TColor;
    FBtnDownColorTo: TColor;
    FGlyphNext: TBitmap;
    FGlyphNextDisabled: TBitmap;
    FGlyphPrev: TBitmap;
    FGlyphPrevDisabled: TBitmap;
    FGlyphDelete: TBitmap;
    FAutoDelete: Boolean;
    FShowDelete: Boolean;
    FHintDeleteBtn: string;
    FOnDeleteMessage: TDeleteMessageEvent;
    FOnClose: TNotifyEvent;
    FOnCanClose: TCanCloseEvent;
    FOnNextMessage: TNotifyEvent;
    FOnPrevMessage: TNotifyEvent;
    FGlyphClose: TBitmap;
    FMaxWidth: Integer;
    FMaxHeight: Integer;
    FPositionFormat: string;
    FAlertMessagesInfo: TMsgCollection;
    FShowPopup: Boolean;
    FHintPopupBtn: string;
    FPopupBtnDown: Boolean;
    FPopupBtnHover: Boolean;
    FScrollUpBtnDown: Boolean;
    FScrollUpBtnHover: Boolean;
    FScrollDnBtnDown: Boolean;
    FScrollDnBtnHover: Boolean;
    FGlyphPopup: TBitmap;
    FBackground: TPicture;
    FOnPopupbtnClick: TNotifyevent;
    FScrollPos: Integer;
    FShowScrollUp: Boolean;
    FShowScrollDn: Boolean;
    FShowScrollers: Boolean;
    FTimer: TTimer;
    FOnDestroy: TNotifyEvent;
    FOnCustomPaint: TCustomPaintEvent;
    FAutoHiding: Boolean;
    FURLColor: TColor;
    FOnAlertClick: TNotifyEvent;
    FOldX,FOldY: integer;
    procedure WMEraseBkGnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetHover(const Value: Boolean);
    procedure SetAlwaysOnTop(const Value: Boolean);
    procedure SetAutoSizeEx(const Value: Boolean);
    procedure DrawTopBar;
    procedure DrawCloseButton;
    procedure DrawLeftScrollBtn;
    procedure DrawRightScrollBtn;
    procedure DrawScrollBtns;
    procedure DrawDeleteBtn;
    procedure DrawPopupBtn;
    procedure DrawScrollUpBtn;
    procedure DrawScrollDnBtn;
    procedure Hide;
    procedure SetAlertMessages(const Value: TStringList);
    procedure SetGlyphDelete(const Value: TBitmap);
    procedure SetGlyphNext(const Value: TBitmap);
    procedure GlyphNextOnChange(Sender: TObject);
    procedure SetGlyphPrev(const Value: TBitmap);
    procedure GlyphPrevOnChange(Sender: TObject);
    procedure SetGlyphClose(const Value: TBitmap);
    procedure SetAutoDelete(const Value: Boolean);
    procedure SetShowDelete(const Value: Boolean);
    function GetCloseRect: TRect;
    function GetLeftScrollBtnRect: TRect;
    function GetRightScrollBtnRect: TRect;
    function GetDeleteBtnRect: TRect;
    function ScrollTextWidth: Integer;
    function GetPopupBtnRect: TRect;
    function GetScrollUpBtnRect: TRect;
    function GetScrollDnBtnRect: TRect;
    function PtOnDeleteBtn(X, Y: integer): Boolean;
    function PtOnPopupBtn(X, Y: integer): Boolean;
    function PtOnLeftScrollBtn(X, Y: integer): Boolean;
    function PtOnRightScrollBtn(X, Y: integer): Boolean;
    function PtOnScrollUpBtn(X, Y: integer): Boolean;
    function PtOnScrollDnBtn(X, Y: integer): Boolean;
    function GetWidthFromTextWidth(w: integer): integer;
    function GetHeightFromTextHeight(H: integer): integer;
    function PtOnCaption(X, Y: integer): Boolean;
    function MinWindowWidth: integer;
    function MinWindowHeight: integer;
    function GetPopupBtnWidth: integer;
    property BorderSize: Integer read FBorderSize write FBorderSize;
    function GetActiveMessage: Integer;
    procedure SetActiveMessage(const Value: Integer);
    procedure SetPositionFormat(const Value: string);
    procedure SetShowPopup(const Value: Boolean);
    procedure SetGlyphPopup(const Value: TBitmap);
    procedure SetBackground(const Value: TPicture);
    procedure PositionChanged(Sender: TObject);
    procedure TimerMessage(Sender: TObject);
  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure AlertMessagesOnChange(Sender: TObject);
    function PtOnClose(X, Y: integer): Boolean;
    function GetMessageHeight(Index: Integer): Integer;
    function GetTextRect: TRect;
    procedure TestAnchor;
    property SelectedColor: TColor read FSelectedColor write FSelectedColor;
    property AlertMessagesInfo: TMsgCollection read FAlertMessagesInfo write FAlertMessagesInfo;
    property AutoHiding: Boolean read FAutoHiding write FAutoHiding;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Next;
    procedure Previous;
    procedure First;
    procedure Last;
    procedure Delete(Index: integer); overload;
    procedure Delete; overload;
    property ActiveMessage: Integer read GetActiveMessage write SetActiveMessage;
  published
    property AlertMessages: TStringList read FAlertMessages write SetAlertMessages;
    property AlwaysOnTop: Boolean read FAlwaysOnTop write SetAlwaysOnTop;
    property AutoHide: Boolean read FAutoHide write FAutoHide;
    property AutoSize: Boolean read FAutoSize write SetAutoSizeEx;
    property AutoDelete: Boolean read FAutoDelete write SetAutoDelete;
    property Background: TPicture read FBackground write SetBackground;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property BtnHoverColor: TColor read FBtnHoverColor write FBtnHoverColor;
    property BtnHoverColorTo: TColor read FBtnHoverColorTo write FBtnHoverColorTo;
    property BtnDownColor: TColor read FBtnDownColor write FBtnDownColor;
    property BtnDownColorTo: TColor read FBtnDownColorTo write FBtnDownColorTo;
    property CaptionColor: TColor read FCaptionColor write FCaptionColor;
    property CaptionColorTo: TColor read FCaptionColorTo write FCaptionColorTo;
    property GradientDirection: TGradientDirection read FGradientDir write FGradientDir;
    property GlyphPrev: TBitmap read FGlyphPrev write SetGlyphPrev;
    property GlyphNext: TBitmap read FGlyphNext write SetGlyphNext;
    property GlyphDelete: TBitmap read FGlyphDelete write SetGlyphDelete;
    property GlyphClose: TBitmap read FGlyphClose write SetGlyphClose;
    property GlyphPopup: TBitmap read FGlyphPopup write SetGlyphPopup;
    property HintNextBtn: string read FHintNextBtn write FHintNextBtn;
    property HintPrevBtn: string read FHintPrevBtn write FHintPrevBtn;
    property HintCloseBtn: string read FHintCloseBtn write FHintCloseBtn;
    property HintDeleteBtn: string read FHintDeleteBtn write FHintDeleteBtn;
    property HintPopupBtn: string read FHintPopupBtn write FHintPopupBtn;
    property Hover: Boolean read FHover write SetHover;
    property Images: TImageList read FImages write FImages;
    property MarginX: Integer read FMarginX write FMarginX;
    property MarginY: Integer read FMarginY write FMarginY;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth default 0;
    property MaxHeight: Integer read FMaxHeight write FMaxHeight default 0;
    property PictureContainer: TPictureContainer read FContainer write FContainer;
    property PositionFormat: string read FPositionFormat write SetPositionFormat;
    property ShowScrollers: Boolean read FShowScrollers write FShowScrollers;
    property ShowDelete: Boolean read FShowDelete write SetShowDelete;
    property ShowPopup: Boolean read FShowPopup write SetShowPopup;
    property URLColor: TColor read FURLColor write FURLColor;
    property WindowColor: TColor read FWindowColor write FWindowColor;
    property WindowColorTo: TColor read FWindowColorTo write FWindowColorTo;
    property OnAlertClick: TNotifyEvent read FOnAlertClick write FOnAlertClick;
    property OnAnchorClick: TAnchorClickEvent read FOnAnchorClick write FOnAnchorClick;
    property OnDeleteMessage: TDeleteMessageEvent read FOnDeleteMessage write FOnDeleteMessage;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnWindowMoved: TNotifyEvent read FOnWindowMoved write FOnWindowMoved;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnCanClose: TCanCloseEvent read FOnCanClose write FOnCanClose;
    property OnNextMessage: TNotifyEvent read FOnNextMessage write FOnNextMessage;
    property OnPrevMessage: TNotifyEvent read FOnPrevMessage write FOnPrevMessage;
    property OnPopupbtnClick: TNotifyevent read FOnPopupbtnClick write FOnPopupbtnClick;
    property OnCustomPaint: TCustomPaintEvent read FOnCustomPaint write FOnCustomPaint;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

  TFadeThreadDone = procedure(Sender: TObject) of object;
  TOnDeleteItemEvent = procedure(Sender: TObject; Index: integer) of object;

  TAdvAlertWindow = class;

  TFadeAction = (faNone, faShow, faHide);

  TFadeThread = class(TThread)
  private
    FLayeredWindow: TAdvAlertWindow;
  protected
    procedure Execute; override;
  public
    constructor Create(aLayeredWindow: TAdvAlertWindow);
  end;

  TMsgCollectionItem = class(TCollectionItem)
  private
    FText: TStringList;
    FImageIndex: TImageIndex;
    FTag: integer;
    FColor: TColor;
    FColorTo: TColor;
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetText(const Value: TStringList);
    procedure TextChanged(Sender: TObject);
    procedure SetTag(const Value: integer);
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Text: TStringList read FText write SetText;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property Tag: Integer read FTag write SetTag;
    property Color: TColor read FColor write SetColor default clNone;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
  end;

  TMsgCollection = class(TCollection)
  private
    FOwner: TComponent;
    FOnChange: TNotifyEvent;
    FOnDeleteItem: TOnDeleteItemEvent;
    function GetItem(Index: Integer): TMsgCollectionItem;
    procedure SetItem(Index: Integer; const Value: TMsgCollectionItem);
  protected
    procedure Update(Item: TCollectionItem); override;
    function GetItemClass: TCollectionItemClass; virtual;
  public
    constructor Create(AOwner: TComponent);
    property Items[Index: Integer]: TMsgCollectionItem read GetItem write SetItem; default;
    function Add: TMsgCollectionItem;
    function Insert(Index: Integer): TMsgCollectionItem;
    function GetOwner: TPersistent; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDeleteItem: TOnDeleteItemEvent read FOnDeleteItem write FOnDeleteItem;
  end;

  TWindowPosition = (wpLeftTop, wpRightTop, wpLeftBottom, wpRightBottom, wpPreset, wpCenter);

  TOnAnchorClickEvent = procedure(Sender: TObject; Anchor: string; Item: TMsgCollectionItem) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvAlertWindow = class(TComponent, ITMSStyle)
  private
    FOwner: TWinControl;
    FAlertWindow: TAlertWindow;
    FContainer: TPictureContainer;
    FHover: Boolean;
    FOnAnchorClick: TOnAnchorClickEvent;
    FAlphaActual: byte;
    FAlphaStart: byte;
    FAlphaEnd: byte;
    FFadeIn: Boolean;
    FFadeOut: Boolean;
    FFading: Boolean;
    FHeight: Integer;
    FWidth: Integer;
    FLeft: Integer;
    FTop: Integer;
    FImages: TImageList;
    FWindowColor: TColor;
    FWindowColorTo: TColor;
    FFont: TFont;
    FGradientDirection: TGradientDirection;
    FAutoHide: Boolean;
    FAutoSize: Boolean;
    FBorderSize: Integer;
    FAlwaysOnTop: Boolean;
    FMarginY: Integer;
    FMarginX: Integer;
    FFadeStep: byte;
    FFadeTime: Integer;
    FFadeThread: TFadeThread;
    FShowFullAlpha: Boolean;
    FTimer: TTimer;
    FAlertMessages: TMsgCollection;
    FDisplayTime: Integer;
    FDisplayCounter: Integer;
    FWindowPosition: TWindowPosition;
    FCaptionColorTo: TColor;
    FCaptionColor: TColor;
    FHintNextBtn: string;
    FHintPrevBtn: string;
    FHintCloseBtn: string;
    FStyle: TAdvAlertWindowStyle;
    FBtnHoverColor: TColor;
    FBtnDownColor: TColor;
    FBtnHoverColorTo: TColor;
    FBtnDownColorTo: TColor;
    FBorderColor: TColor;
    FGlyphNext: TBitmap;
    FGlyphPrev: TBitmap;
    FGlyphDelete: TBitmap;
    FAutoDelete: Boolean;
    FOnDeleteMessage: TDeleteMessageEvent;
    FOnCanDeleteMessage: TCanDeleteEvent;
    FOnShowMessage: TNotifyEvent;
    FOnAutoHide: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnCanClose: TCanCloseEvent;
    FOnNextMessage: TNotifyEvent;
    FOnPrevMessage: TNotifyEvent;
    FShowDelete: Boolean;
    FHintDeleteBtn: string;
    FGlyphClose: TBitmap;
    FPositionFormat: string;
    FMaxWidth: Integer;
    FMaxHeight: Integer;
    FAutoThemeAdapt: Boolean;
    FScreenMarginX: integer;
    FScreenMarginY: integer;
    FShowPopup: Boolean;
    FGlyphPopup: TBitmap;
    FHintPopupBtn: string;
    FPopupMenu: TPopupMenu;
    FOnPopupClick: TNotifyEvent;
    FBackground: TPicture;
    FShowScrollers: Boolean;
    FPersistPosition: Boolean;
    FRegistryKey: string;
    FOnCustomPaint: TCustomPaintEvent;
    FAutoHiding: Boolean;
    FURLColor: TColor;
    FBidiMode: TBidiMode;
    FOnAlertClick: TNotifyEvent;
    FOnHideMessage: TNotifyEvent;
    procedure SetFont(const Value: TFont);
    procedure SetAlphaEnd(const Value: byte);
    procedure SetAlphaStart(const Value: byte);
    procedure SetAlphaActual(const Value: byte);
    procedure SetImages(const Value: TImageList);
    function SetAlertPos: integer;
    procedure ThreadDone(Sender: TObject);
    procedure UpdateAlertMessages;
    procedure AlertMessagesOnChange(Sender: TObject);
    procedure AlertMessagesOnDeleteItem(Sender: TObject; index: integer);
    //procedure SetAlertMessages(const Value: TMsgCollection);
    procedure AlertWindowMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure AlertWindowOnMouseLeave(Sender: TObject);
    procedure AlertWindowOnClose(Sender: TObject);
    procedure AlertWindowOnCanClose(Sender: TObject; var CanClose: Boolean);
    procedure AlertWindowOnNextMessage(Sender: TObject);
    procedure AlertWindowOnPrevMessage(Sender: TObject);
    procedure AlertWindowOnDelete(Sender: TObject; index: integer);
    procedure AlertWindowOnPopupBtnClick(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure SetDisplayTime(const Value: integer);
    procedure SetWindowPosition(const Value: TWindowPosition);
    procedure AlertWindowMoved(Sender: TObject);
    procedure SetTimer(Active: Boolean);
    procedure SetStyle(const Value: TAdvAlertWindowStyle);
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetGlyphDelete(const Value: TBitmap);
    procedure SetGlyphNext(const Value: TBitmap);
    procedure SetGlyphPrev(const Value: TBitmap);
    procedure SetAutoHide(const Value: Boolean);
    procedure SetAutoDelete(const Value: Boolean);
    procedure SetShowDelete(const Value: Boolean);
    procedure SetShowPopup(const Value: Boolean);
    procedure SetGlyphClose(const Value: TBitmap);
    function GetActiveMessage: Integer;
    procedure SetActiveMessage(const Value: Integer);
    procedure SetPositionFormat(const Value: string);
    procedure SetBackground(const Value: TPicture);
    procedure ThemeAdapt;
    procedure SetGlyphPopup(const Value: TBitmap);
    procedure SetPopupMenu(const Value: TPopupMenu);
    function GetVisible: Boolean;
    function GetLeft: Integer;
    function GetTop: Integer;
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    function GetWindowHandle: THandle;
  protected
    procedure WindowAnchorClick(Sender: TObject; Anchor: string; index: integer);
    procedure AlertClick(Sender: TObject);
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure CreateAlertWindow;
    procedure ApplyStyle;
    procedure SetMessages;
    procedure DoHide;
    procedure TextChanged(Sender: TObject);
    procedure AlertDestroyed(Sender: TObject);
    procedure CustomPaint(Sender: TObject; ACanvas: TCanvas; ARect: TRect; var DefaultDraw: boolean);
    property BorderSize: Integer read FBorderSize write FBorderSize;
    property AlphaActual: byte read FAlphaActual write SetAlphaActual;
    property AutoHiding: Boolean read FAutoHiding write FAutoHiding;
    function CreateMsgCollection: TMsgCollection; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Update;
    procedure Show;
    procedure Hide;
    procedure CloseAlert;
    procedure Next;
    procedure Previous;
    procedure First;
    procedure Last;
    procedure SavePosition;
    procedure LoadPosition;
    procedure DeleteMessage(Index: integer);
    property ActiveMessage: Integer read GetActiveMessage write SetActiveMessage;
    property IsVisible: Boolean read GetVisible;
    property IsFading: Boolean read FFading;
    property Handle: THandle read GetWindowHandle;
    property AlertWindow: TAlertWindow read FAlertWindow;
  published
    property AlertMessages: TMsgCollection read FAlertMessages write FAlertMessages;
    property AlwaysOnTop: Boolean read FAlwaysOnTop write FAlwaysOnTop;
    property AutoHide: Boolean read FAutoHide write SetAutoHide;
    property AutoThemeAdapt: Boolean read FAutoThemeAdapt write FAutoThemeAdapt default False;
    property AutoSize: Boolean read FAutoSize write FAutoSize;
    property AutoDelete: Boolean read FAutoDelete write SetAutoDelete;
    property Background: TPicture read FBackground write SetBackground;
    property BidiMode: TBiDiMode read FBidiMode write FBidiMode default bdLeftToRight;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property BtnHoverColor: TColor read FBtnHoverColor write FBtnHoverColor;
    property BtnHoverColorTo: TColor read FBtnHoverColorTo write FBtnHoverColorTo;
    property BtnDownColor: TColor read FBtnDownColor write FBtnDownColor;
    property BtnDownColorTo: TColor read FBtnDownColorTo write FBtnDownColorTo;
    property CaptionColor: TColor read FCaptionColor write FCaptionColor;
    property CaptionColorTo: TColor read FCaptionColorTo write FCaptionColorTo;
    property Font: TFont read FFont write SetFont;
    property GradientDirection: TGradientDirection read FGradientDirection write FGradientDirection;
    property GlyphPrev: TBitmap read FGlyphPrev write SetGlyphPrev;
    property GlyphNext: TBitmap read FGlyphNext write SetGlyphNext;
    property GlyphDelete: TBitmap read FGlyphDelete write SetGlyphDelete;
    property GlyphClose: TBitmap read FGlyphClose write SetGlyphClose;
    property GlyphPopup: TBitmap read FGlyphPopup write SetGlyphPopup;
    property HintNextBtn: string read FHintNextBtn write FHintNextBtn;
    property HintPrevBtn: string read FHintPrevBtn write FHintPrevBtn;
    property HintCloseBtn: string read FHintCloseBtn write FHintCloseBtn;
    property HintDeleteBtn: string read FHintDeleteBtn write FHintDeleteBtn;
    property HintPopupBtn: string read FHintPopupBtn write FHintPopupBtn;
    property Hover: Boolean read FHover write FHover;
    property Images: TImageList read FImages write SetImages;
    property MarginX: Integer read FMarginX write FMarginX;
    property MarginY: Integer read FMarginY write FMarginY;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth default 0;
    property MaxHeight: Integer read FMaxHeight write FMaxHeight default 0;
    property PersistPosition: Boolean read FPersistPosition write FPersistPosition default False;    
    property PictureContainer: TPictureContainer read FContainer write FContainer;
    property PopupLeft: Integer read GetLeft write SetLeft;
    property PopupTop: Integer read GetTop write SetTop;
    property PopupWidth: Integer read FWidth write FWidth;
    property PopupHeight: Integer read FHeight write FHeight;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property PositionFormat: string read FPositionFormat write SetPositionFormat;
    property RegistryKey: string read FRegistryKey write FRegistryKey;    
    property WindowColor: TColor read FWindowColor write FWindowColor;
    property WindowColorTo: TColor read FWindowColorTo write FWindowColorTo;
    property ShowScrollers: Boolean read FShowScrollers write FShowScrollers;
    property ShowDelete: Boolean read FShowDelete write SetShowDelete;
    property ShowPopup: Boolean read FShowPopup write SetShowPopup;
    property AlphaEnd: byte read fAlphaEnd write SetAlphaEnd;
    property AlphaStart: byte read fAlphaStart write SetAlphaStart;
    property FadeTime: integer read FFadeTime write FFadeTime default 50;
    property DisplayTime: integer read FDisplayTime write SetDisplayTime;
    property FadeStep: byte read FFadeStep write FFadeStep;
    property WindowPosition: TWindowPosition read FWindowPosition write SetWindowPosition;
    property ScreenMarginX: integer read FScreenMarginX write FScreenMarginX default 0;
    property ScreenMarginY: integer read FScreenMarginY write FScreenMarginY default 0;
    property Style: TAdvAlertWindowStyle read FStyle write SetStyle default asOffice2003Blue;
    property URLColor: TColor read FURLColor write FURLColor default clBlue;
    property Version: string read GetVersion write SetVersion;

    property OnAnchorClick: TOnAnchorClickEvent read FOnAnchorClick write FOnAnchorClick;
    property OnAlertClick: TNotifyEvent read FOnAlertClick write FOnAlertClick;
    property OnAutoHide: TNotifyEvent read FOnAutoHide write FOnAutoHide;
    property OnCanDeleteMessage: TCanDeleteEvent read FOnCanDeleteMessage write FOnCanDeleteMessage;
    property OnDeleteMessage: TDeleteMessageEvent read FOnDeleteMessage write FOnDeleteMessage;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnCanClose: TCanCloseEvent read FOnCanClose write FOnCanClose;
    property OnCustomPaint: TCustomPaintEvent read FOnCustomPaint write FOnCustomPaint;
    property OnNextMessage: TNotifyEvent read FOnNextMessage write FOnNextMessage;
    property OnPrevMessage: TNotifyEvent read FOnPrevMessage write FOnPrevMessage;
    property OnPopupClick: TNotifyEvent read FOnPopupClick write FOnPopupClick;
    property OnShowMessage: TNotifyEvent read FOnShowMessage write FOnShowMessage;
    property OnHideMessage: TNotifyEvent read FOnHideMessage write FOnHideMessage;
  end;


implementation

uses
  ComObj, Commctrl, ShellApi
  {$IFDEF DELPHIXE_LVL}
  , MultiMon
  {$ENDIF}
  ;

{$I HTMLENGO.PAS}

var
  FadeAction: TFadeAction;

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


function CurrentXPTheme: XPColorScheme;
var
  FileName, ColorScheme, SizeName: WideString;
  hThemeLib: THandle;
begin
  hThemeLib := 0;
  Result := xpNone;

  try
    hThemeLib := LoadLibrary('uxtheme.dll');

    if hThemeLib > 0 then
    begin
      IsThemeActive := GetProcAddress(hThemeLib, 'IsThemeActive');

      if Assigned(IsThemeActive) then
        if IsThemeActive then
        begin
          GetCurrentThemeName := GetProcAddress(hThemeLib, 'GetCurrentThemeName');
          if Assigned(GetCurrentThemeName) then
          begin
            SetLength(FileName, 255);
            SetLength(ColorScheme, 255);
            SetLength(SizeName, 255);
            try
              OleCheck(GetCurrentThemeName(PWideChar(FileName), 255,
                PWideChar(ColorScheme), 255, PWideChar(SizeName), 255));
            except
            end;

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

//------------------------------------------------- DynaLink_UpdateLayeredWindow

function DynaLink_UpdateLayeredWindow(hwnd, hdcDst: thandle;
  pptDst, size: ppoint; hdcSrc: thandle;
  pptSrc: ppoint;
  crKey: dword;
  var pblend: _BLENDFUNCTION;
  dwFlags: DWORD): boolean;

var
  UserDLL: THandle;
  user_UpdateLayeredWindow: function(hwnd, hdcDst: thandle;
    pptDst, size: ppoint; hdcSrc: thandle;
    pptSrc: ppoint;
    crKey: dword;
    var pblend: _BLENDFUNCTION;
    dwFlags: DWORD): DWORD; stdcall;

begin
  Result := TRUE;
  UserDLL := GetModuleHandle('USER32.DLL');
  if (UserDLL > 0) then
  begin
    @user_UpdateLayeredWindow := GetProcAddress(UserDLL, 'UpdateLayeredWindow');
    if Assigned(user_UpdateLayeredWindow) then
    begin
      Result := user_UpdateLayeredWindow(hwnd, hdcDst, pptDst, size, hdcSrc, pptSrc, crKey, pblend, dwFlags) <> 0;
    end;
  end;
end;

//------------------------------------------ DynaLink_SetLayeredWindowAttributes

function DynaLink_SetLayeredWindowAttributes(HWND: thandle; crKey: DWORD; bAlpha: byte; dwFlags: DWORD): boolean;
var
  UserDLL: THandle;
  user_SetLayeredWindowAttributes: function(HWND: thandle; crKey: DWORD; bAlpha: byte; dwFlags: DWORD): DWORD; stdcall;

begin
  result := TRUE;
  UserDLL := GetModuleHandle('USER32.DLL');
  if (UserDLL > 0) then
  begin
    @user_SetLayeredWindowAttributes := GetProcAddress(UserDLL, 'SetLayeredWindowAttributes');
    if Assigned(user_SetLayeredWindowAttributes) then
    begin
      Result := user_SetLayeredWindowAttributes(hwnd, crKey, bAlpha, dwFlags) <> 0;
    end;
  end;
end;

//------------------------------------------------------------------ WindowBlend

procedure WindowBlend(hwnd, hdc: thandle; colorkey: tcolor; alpha: byte; r: trect);
var
  dw: dword;
  blnd: _BLENDFUNCTION;
  dskdc: thandle;
  size, src: TPoint;

begin
  try
    dw := GetWindowLong(hwnd, GWL_EXSTYLE);
    SetWindowLong(hwnd, GWL_EXSTYLE, dw or WS_EX_LAYERED);
    DynaLink_SetLayeredWindowAttributes(hwnd, DWORD(colorkey), alpha, 2);
    blnd.BlendOp := AC_SRC_OVER;
    blnd.BlendFlags := 0;
    blnd.SourceConstantAlpha := 0;
    blnd.AlphaFormat := 0;
    dskdc := GetDC(0);
    try
      size := point(r.right - r.left, r.bottom - r.top);
      src := point(r.left, r.top);
      DynaLink_UpdateLayeredWindow(hwnd, dskdc, nil, @size, hdc, @src, dword(colorkey), blnd, ULW_ALPHA);
    finally
      ReleaseDC(hwnd, dskdc);
    end;
  except
  end;
end;

//------------------------------------------------------------------------------
//---------------------------{ TExchangeScroller }------------------------------
//------------------------------------------------------------------------------

function TMsgScroller.CanGoBack: Boolean;
begin
  Result := Position > Min;
end;

//------------------------------------------------------------------------------

function TMsgScroller.CanGoForward: Boolean;
begin
  Result := Position < Max;
end;

//------------------------------------------------------------------------------

constructor TMsgScroller.Create;
begin
  inherited Create;
  FMin := 0;
  FMax := 0;
  FPosition := 0;
  FVisible := false;
end;

//------------------------------------------------------------------------------

procedure TMsgScroller.SetMax(const Value: integer);
begin
  if Value >= FMin then FMax := Value;
end;

//------------------------------------------------------------------------------

procedure TMsgScroller.SetMin(const Value: integer);
begin
  if Value <= FMax then FMin := Value;
end;

//------------------------------------------------------------------------------

procedure TMsgScroller.SetPosition(const Value: integer);
begin
  FPosition := Value;
  if Assigned(OnChange) then
    OnChange(self);
end;

//------------------------------------------------------------------------------

procedure TMsgScroller.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

//------------------------------------------------------------------------------
//--------------------------{ TAlertWindow }------------------------------------
//------------------------------------------------------------------------------

constructor TAlertWindow.Create(AOwner: TComponent);
var
  FDesignTime: boolean;
begin
  inherited;
  Height := 100;
  Width := 300;
  Left := -1;
  Top := -1;
  FWindowColor := clWhite;
  FWindowColorTo := clBtnFace;
  FCaptionColor := clWhite;
  FCaptionColorTo := clGray;
  FGradientDir := gdVertical;
  FBtnHoverColor := $00BBA9A2;
  FBtnHoverColorTo := $00BBA9A2;
  FBtnDownColor := $00A78F87;
  FBtnDownColorTo := $00A78F87;
  FURLColor := clBlue;

  FAutoHide := True;
  FAutoSize := False;
  FAlwaysOnTop := False;
  FMarginY := 1;
  FMarginX := 4;
  FSelectedColor := $00BBA9A2;
  FAlertMessages := TStringList.Create;
  FAlertMessages.OnChange := AlertMessagesOnChange;
  FImageCache := THTMLPictureCache.Create;
  FAlertMessagesInfo := TMsgCollection.Create(self);

  FBackground := TPicture.Create;

  FGlyphDelete := TBitmap.Create;
  FGlyphNext := TBitmap.Create;
  FGlyphNext.OnChange := GlyphNextOnChange;
  FGlyphNextDisabled := TBitmap.Create;
  FGlyphNextDisabled.PixelFormat := pf32bit;

  FGlyphPrev := TBitmap.Create;

  FGlyphPrev.OnChange := GlyphPrevOnChange;
  FGlyphPrevDisabled := TBitmap.Create;
  FGlyphPrevDisabled.PixelFormat := pf32bit;

  FGlyphClose := TBitmap.Create;
  FGlyphPopup := TBitmap.Create;

  FMsgScroller := TMsgScroller.Create;
  FMsgScroller.Min := 0;
  FMsgScroller.Max := 0;
  FMsgScroller.Position := 0;
  FMsgScroller.OnChange := PositionChanged;

  FHoverLink := -1;
  FHover := True;
  FImages := nil;
  FOriginalHint := '';

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
  begin
    FHintNextBtn := 'Next';
    FHintPrevBtn := 'Previous';
    FHintCloseBtn := 'Close';
    FHintDeleteBtn := 'Delete';
    FHintPopupBtn := 'Popup';
  end;

  Hint := '';
  ShowHint := true;
  FAutoDelete := false;
  FShowDelete := true;
  FShowPopup := false;
  FShowScrollUp := false;
  FShowScrollDn := false;

  FTimer := TTimer.Create(self);
  FTimer.Interval := 100;
  if not (csDesigning in ComponentState) then
  begin
    FTimer.Enabled := true;
  end;  
  FTimer.OnTimer := TimerMessage;
end;

//------------------------------------------------------------------------------

destructor TAlertWindow.Destroy;
begin

  FAlertMessages.Free;
  FAlertMessagesInfo.Free;
  FBackground.Free;
  FTimer.Free;
  FImageCache.Free;
  FMsgScroller.Free;
  FGlyphDelete.Free;
  FGlyphNext.Free;
  FGlyphNextDisabled.Free;
  FGlyphPrev.Free;
  FGlyphPrevDisabled.Free;
  FGlyphClose.Free;
  FGlyphPopup.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.WndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_DESTROY) then
  begin
    if Assigned(OnDestroy) then
      OnDestroy(Self);
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.WMNCHitTest(var Message: TWMNCHitTest);
var
  pt: TPoint;
  RD, hr, r: TRect;
  hl, ml: Integer;
  XSize, YSize: Integer;
  a, s, fa: string;
  Anchor: string;

begin
  pt := ScreenToClient(Point(message.Xpos, message.YPos));

  r := ClientRect;
  RD := GetTextRect;

  InflateRect(RD, -FBorderSize, -FBorderSize);

  Anchor := '';

  Canvas.Font.Assign(Font);

  if FScrollPos > 0 then
    RD.Bottom := RD.Bottom + 1000;

  if FMsgScroller.Position > 0 then
  begin
    if HTMLDrawEx(Canvas, AlertMessages.Strings[FMsgScroller.Position - 1], rd, FImages, pt.X, pt.Y + FScrollPos, -1, -1, 1, True, False, False, False, False, False, True,
      1.0, FURLColor, clNone, clNone, clGray, a, s, fa, XSize, YSize,
      hl, ml, hr, FImageCache, FContainer, 0, BidiMode) then
    begin
      Anchor := a;
    end;
  end;

  if (Anchor <> '') then
  begin
    Cursor := crHandPoint;
    if (FHoverLink <> hl) and FHover then
      InvalidateRect(Handle, @hr, True);
      FHoverLink := hl;
      FHoverRect := hr;
  end
  else
  begin
    Cursor := crDefault;
    if (FHoverLink <> -1) and (FHover) then
      InvalidateRect(Handle, @FHoverRect, True); ;
    FHoverLink := -1;
  end;

  Message.Result := HTCLIENT;
end;

procedure TAlertWindow.DrawCloseButton;
var
  R: TRect;
begin
  R := GetCloseRect;
  with Canvas do
  begin
    if FDownClose then
    begin
      //Brush.Color:= BtnDownColor; //$00A78F87;
      //FillRect(R);
      R.Bottom := R.Bottom - 1;
      DrawGradient(Canvas, FBtnDownColor, FBtnDownColorTo, 16, R, false);
      R.Bottom := R.Bottom + 1;
      Pen.Color := clBlack;
      Brush.Style := bsClear;
      Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    end
    else if FHoverClose then
    begin
      //Brush.Color:= BtnHoverColor;  //$00BBA9A2;
      //FillRect(R);
      R.Bottom := R.Bottom - 1;
      DrawGradient(Canvas, FBtnHoverColor, FBtnHoverColorTo, 16, R, false);
      R.Bottom := R.Bottom + 1;
      Pen.Color := clBlack;
      Brush.Style := bsClear;
      Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    end;
    if not FGlyphClose.Empty then
    begin
      FGlyphClose.Transparent := true;
      Canvas.StretchDraw(Rect(R.left + 2, R.Top + 2, R.left + 18, R.Top + 18), FGlyphClose)
    end
    else
    begin
      Pen.Color := clBlack;
                     {/}
      MoveTo(Width - 19 + 2 + 3 - 1, 12 + 3 + 4 + 1);
      LineTo(Width - 19 + 2 + 3 + 6 + 1, 12 + 2 - 1);
      MoveTo(Width - 19 + 2 + 4 - 1, 12 + 3 + 4 + 1);
      LineTo(Width - 19 + 2 + 3 + 5 + 1, 12 + 2 - 1);
                     {\}
      MoveTo(Width - 19 + 2 + 3 - 1, 12 + 3 - 1);
      LineTo(Width - 19 + 2 + 3 + 6 + 1, 12 + 3 + 5 + 1);
      MoveTo(Width - 19 + 2 + 4 - 1, 12 + 3 - 1);
      LineTo(Width - 19 + 2 + 3 + 5 + 1, 12 + 3 + 5 + 1);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.DrawTopBar;
var
  PS: integer;
  i: integer;
  R: TRect;
begin
  R := Rect(1, 1, width - 1, 7);
  DrawGradient(Canvas, FCaptionColor, FCaptionColorTo, 16, R, false);

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
    Canvas.Pixels[R.Left, R.Top] := FCaptionColorTo;
    R.Left := R.Left + 4;
  end;

end;

//------------------------------------------------------------------------------

procedure TAlertWindow.Paint;
var
  R, TextR, hr: TRect;
  Anchor, Stripped, FocusAnchor: string;
  XSize, YSize, HyperLinks, MouseLink: integer;
  clr, clrTo: TColor;
  HRGN: THandle;
  TH, MH: Integer;
  DefaultDraw: Boolean;
begin
  R := ClientRect;
  TextR := GetTextRect;

  clr := FWindowColor;
  clrTo := FWindowColorTo;

  DefaultDraw := true;

  if Assigned(OnCustomPaint) then
    OnCustomPaint(Self, Canvas, R, DefaultDraw);

  if not DefaultDraw then
    Exit;

  with Canvas do
  begin
    if (FAlertMessagesInfo.Count >= (FMsgScroller.Position)) and
       (FAlertMessagesInfo.Count > 0) and (FMsgScroller.Position > 0) then
      if FAlertMessagesInfo.Items[FMsgScroller.Position - 1].Color <> clNone then
      begin
        clr := FAlertMessagesInfo.Items[FMsgScroller.Position - 1].Color;
        clrTo := FAlertMessagesInfo.Items[FMsgScroller.Position - 1].ColorTo;
      end;

    DrawGradient(Canvas, clr, clrTo, 80, R, false);
    Pen.Color := BorderColor;
    Brush.Style := bsClear;
    Rectangle(R.Left, R.Top, R.Right, R.Bottom);

    if Assigned(Background.Graphic) then
      if not Background.Graphic.Empty then
        Draw(0,0,Background.Graphic);

    DrawTopBar;
    DrawCloseButton;
  end;

  if FMsgScroller.Position > 0 then
  begin
    //---- Draw Image
    if FImages <> nil then
      FImages.Draw(Canvas, 5, 16, Integer(AlertMessages.Objects[FMsgScroller.Position - 1]));

    Canvas.Font.Name := 'System';
    Canvas.Font.Assign(Self.Font);

    HRGN := CreateRectRgn(TextR.Left, TextR.Top, TextR.Right, TextR.Bottom);
    SelectClipRgn(Canvas.Handle,HRGN);

    FShowScrollDn := false;
    FShowScrollUp := false;

    TH := TextR.Bottom - TextR.Top;
    MH := GetMessageHeight(FMsgScroller.Position - 1);
    if (MH > TH) and ShowScrollers then
    begin
      TextR.Top := TextR.Top - FScrollPos;
      FShowScrollDn := (FScrollPos + TH < MH);
      FShowScrollUp := (FScrollPos > 0);
    end;

    HTMLDrawEx(Canvas, AlertMessages.Strings[FMsgScroller.Position - 1], TextR, FImages, 0, 0, -1, FHoverLink, 1, False, False, False, False, True, FHover, True,
      1.0, FURLColor, clNone, clNone, clGray, Anchor, Stripped, FocusAnchor, XSize, YSize,
      HyperLinks, MouseLink, hr, FImageCache, FContainer, 0, BidiMode);

    SelectClipRgn(Canvas.Handle,0);
    DeleteObject(HRGN);
  end;

  DrawScrollUpBtn;
  DrawScrollDnBtn;

  //---- Draw Scroll Buttons
  DrawScrollBtns;
  //---- Draw Delete Button
  DrawDeleteBtn;
  //---- Draw Popup Button
  DrawPopupBtn;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style AND NOT WS_BORDER;
  if FAlwaysOnTop then
  begin
    Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST;
  end;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  
  if FAlwaysOnTop then
  begin
    if (Owner is TCustomForm) then
    begin
      if not IsWindowVisible((Owner as TCustomForm).Handle) then
        Exit;
    end;

    SetWindowPos(Handle,HWND_TOPMOST, 0,0,0,0,SWP_NOSIZE or SWP_NOMOVE);
  end;
end;

//------------------------------------------------------------------------------

function TAlertWindow.PtOnClose(X, Y: integer): Boolean;
var
  P: TPoint;
begin
  P := Point(X, Y);
  Result := PtInRect(GetCloseRect, P);
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.CMMouseLeave(var Message: TMessage);
begin
  if FDragingStart then
  begin
    ReleaseCapture;
    FDragingStart := false;
  end;

  if FHoverClose then
  begin
    FHoverClose := false;
    Invalidate;
  end;

  if FAutoHide then
    Self.Visible := False;

  if FScrollLeftHover or FScrollLeftDown then
  begin
    FScrollLeftHover := false;
    FScrollLeftDown := false;
    Invalidate;
  end;

  if FScrollRightHover or FScrollRightDown then
  begin
    FScrollRightHover := false;
    FScrollRightDown := false;
    Invalidate;
  end;

  if FDeleteHover or FDeleteDown then
  begin
    FDeleteHover := false;
    FDeleteDown := false;
    Invalidate;
  end;

  if Hint <> FOriginalHint then
    Hint := FOriginalHint;

  if Assigned(OnMouseLeave) then
    FOnMouseLeave(self);
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  
  if PtOnCaption(X, Y) then
  begin
    FOldMouseX := X;
    FOldMouseY := Y;
    SetCapture(Handle);
    FDragingStart := true;
  end
  else if FDragingStart then
  begin
    ReleaseCapture;
    FDragingStart := false;
  end;

  if not FDragingStart then
  begin
    if PtOnClose(X, Y) then
    begin
      FDownClose := true;
      DrawCloseButton;
    end;

    if PtOnLeftScrollBtn(X, Y) then
    begin
      FScrollLeftDown := true;
      DrawLeftScrollBtn;
    end;

    if PtOnRightScrollBtn(X, Y) and (AlertMessages.Count > 1) then
    begin
      FScrollRightDown := true;
      DrawRightScrollBtn;
    end;

    if PtOnDeleteBtn(X, Y) and FShowDelete then
    begin
      FDeleteDown := true;
      DrawDeleteBtn;
    end;

    if PtOnPopupBtn(X, Y) and FShowPopup then
    begin
      FPopupBtnDown := true;
      DrawPopupBtn;
    end;

    if PtOnScrollUpBtn(X, Y) {and FShowPopup} then
    begin
      FScrollUpBtnDown := true;
      DrawScrollUpBtn;
    end;

    if PtOnScrollDnBtn(X, Y) {and FShowPopup} then
    begin
      FScrollDnBtnDown := true;
      DrawScrollDnBtn;
    end;

    TestAnchor;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
  oldhint: string;
begin
  oldhint := Hint;

  if (FOldx = x) and (FOldy = y) then
    Exit;


  inherited;

  FOldx := x;
  FOldy := y;

  if (csDesigning in ComponentState) then
    Exit;

  if FDragingStart and (ssLeft in Shift) then
  begin
    GetWindowRect(Handle, R);
    SetWindowPos(Handle, HWND_TOPMOST, R.left + (X - FOldMouseX), R.top + (Y - FOldMouseY), R.right - R.left, R.bottom - R.top, 0);
  end;

  if not FDragingStart then
  begin
    if PtOnClose(X, Y) then
    begin
      if not FHoverClose then
      begin
        FHoverClose := true;
        DrawCloseButton;
        Hint := HintCloseBtn;
        if Hint <> oldhint then
          Application.CancelHint;
      end;
    end
    else
    begin
      if Hint = HintCloseBtn then
      begin
        Hint := FOriginalHint;
        if Hint <> oldhint then
          Application.CancelHint;
      end;
      if FHoverClose or FDownClose then
      begin
        FHoverClose := false;
        FDownClose := false;
        Invalidate;
      end;
    end;

    if PtOnLeftScrollBtn(X, Y) then
    begin
      if not FScrollLeftHover and FMsgScroller.CanGoBack then
      begin
        FScrollLeftHover := true;
        DrawLeftScrollBtn;
        Hint := HintPrevBtn;
        if Hint <> oldhint then
          Application.CancelHint;
      end;
    end
    else
    begin
      if Hint = HintPrevBtn then
      begin
        Hint := FOriginalHint;
        if Hint <> oldhint then
          Application.CancelHint;
      end;
      if FScrollLeftHover or FScrollLeftDown then
      begin
        FScrollLeftHover := false;
        FScrollLeftDown := false;
        Invalidate;
      end;
    end;

    if PtOnRightScrollBtn(X, Y) then
    begin
      if not FScrollRightHover and FMsgScroller.CanGoForward then
      begin
        FScrollRightHover := true;
        DrawRightScrollBtn;
        Hint := HintNextBtn;
        if Hint <> oldhint then
          Application.CancelHint;
      end;
    end
    else
    begin
      if Hint = HintNextBtn then
      begin
        Hint := FOriginalHint;
        if Hint <> oldhint then
          Application.CancelHint;
      end;
      if FScrollRightHover or FScrollRightDown then
      begin
        FScrollRightHover := false;
        FScrollRightDown := false;
        Invalidate;
      end;
    end;

    if PtOnDeleteBtn(X, Y) then
    begin
      if not FDeleteHover and FShowDelete then
      begin
        FDeleteHover := true;
        DrawDeleteBtn;
        Hint := HintDeleteBtn;
        if Hint <> oldhint then
          Application.CancelHint;
      end;
    end
    else
    begin
      if Hint = HintDeleteBtn then
      begin
        Hint := FOriginalHint;
        if Hint <> oldhint then
          Application.CancelHint;
      end;
      if FDeleteHover or FDeleteDown then
      begin
        FDeleteHover := false;
        FDeleteDown := false;
        Invalidate;
      end;
    end;

    if PtOnPopupBtn(X, Y) then
    begin
      if not FPopupBtnHover and FShowPopup then
      begin
        FPopupBtnHover := true;
        DrawPopupBtn;
        Hint := HintPopupBtn;
        if Hint <> oldhint then
          Application.CancelHint;
      end;
    end
    else
    begin
      if Hint = HintPopupBtn then
      begin
        Hint := FOriginalHint;
        if Hint <> oldhint then
          Application.CancelHint;
      end;
      if FPopupBtnHover or FPopupBtnDown then
      begin
        FPopupBtnHover := false;
        FPopupBtnDown := false;
        Invalidate;
      end;
    end;

    if PtOnScrollUpBtn(X, Y) then
    begin
      if not FScrollUpBtnHover and FShowScrollUp then
      begin
        FScrollupBtnHover := true;
        DrawScrollUpBtn;
      end;
    end
    else
    begin
      if FScrollUpBtnHover or FScrollUpBtnDown then
      begin
        FScrollUpBtnHover := false;
        FScrollUpBtnDown := false;
        Invalidate;
      end;
    end;

    if PtOnScrollDnBtn(X, Y) then
    begin
      if not FScrollDnBtnHover and FShowScrollDn then
      begin
        FScrollDnBtnHover := true;
        DrawScrollDnBtn;
      end;
    end
    else
    begin
      if FScrollDnBtnHover or FScrollDnBtnDown then
      begin
        FScrollDnBtnHover := false;
        FScrollDnBtnDown := false;
        Invalidate;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.TimerMessage(Sender: TObject);
var
  pt: TPoint;
  TR: TRect;
begin
  if not FShowScrollUp and not FShowScrollDn then
    Exit;

  GetCursorPos(pt);
  pt := ScreenToClient(pt);

  if FScrollUpBtnDown and FShowScrollUp then
  begin
    if FScrollPos >= 4 then
      FScrollPos := FScrollPos - 4;
    Invalidate;
  end;

  if FScrollDnBtnDown and FShowScrollDn then
  begin
    TR := GetTextRect;
    if (FScrollPos + TR.Bottom - TR.Top < GetMessageHeight(FMsgScroller.Position - 1) + 4) then
      FScrollPos := FScrollPos + 4;
    Invalidate;
  end;

end;

//------------------------------------------------------------------------------

procedure TAlertWindow.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  TR: TRect;
begin
  inherited;

  if FDragingStart then
  begin
    ReleaseCapture;
    FDragingStart := false;
    if Assigned(FOnWindowMoved) then
      FOnWindowMoved(self);
  end
  else if FDownClose then
  begin
    FDownClose := false;
    if FAutoDelete then
      Delete;
    Hide;
  end
  else if FScrollLeftDown then
  begin
    FScrollLeftDown := false;
    FScrollPos := 0;    
    Previous;
  end else if FScrollRightDown then
  begin
    FScrollRightDown := false;
    FScrollPos := 0;
    Next;
  end else if FDeleteDown then
  begin
    FDeleteDown := false;
    FScrollPos := 0;
    Delete;
  end else if FPopupBtnDown then
  begin
    FPopupBtnDown := false;
    Invalidate;
    if Assigned(FOnPopupBtnClick) then
      FOnPopupBtnClick(Self);
  end else if FScrollUpBtnDown then
  begin
    FScrollUpBtnDown := false;
    if FShowScrollUp then
    begin
      if FScrollPos >= 4 then
        FScrollPos := FScrollPos - 4;
      Invalidate;
    end;        
  end else if FScrollDnBtnDown then
  begin
    FScrollDnBtnDown := false;

    if FShowScrollDn then
    begin
      TR := GetTextRect;
      if (FScrollPos + TR.Bottom - TR.Top < GetMessageHeight(FMsgScroller.Position - 1) + 4) then
        FScrollPos := FScrollPos + 4;
      Invalidate;
   end;   
  end
  else
  begin  
    if Assigned(FOnAlertClick) then
      FOnAlertClick(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.SetAlwaysOnTop(const Value: Boolean);
begin
  FAlwaysOnTop := Value;
  RecreateWnd;
end;

//------------------------------------------------------------------------------

function TAlertWindow.GetMessageHeight(Index: Integer): Integer;
var
  rd, hr: TRect;
  Anchor, Stripped, FocusAnchor: string;
  XSize, YSize, HyperLinks, MouseLink: integer;

begin
  RD := GetTextRect;
  RD.Bottom := RD.Top + 4096;

  Canvas.Font.Assign(Self.Font);

  HTMLDrawEx(Canvas, AlertMessages.Strings[Index], rd, FImages, 0, 0, -1, FHoverLink, 1, False, True, False, False, True, FHover, True,
    1.0, FURLColor, clNone, clNone, clGray, Anchor, Stripped, FocusAnchor, XSize, YSize,
    HyperLinks, MouseLink, hr, FImageCache, FContainer, 0, BidiMode);

  Result := YSize;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.SetAutoSizeEx(const Value: Boolean);
var
  R, rd, hr: TRect;
  Anchor, Stripped, FocusAnchor: string;
  XSize, YSize, HyperLinks, MouseLink: integer;
  i, MaxW, MaxH: integer;
  NW, NH: Integer;
begin
  FAutoSize := Value;

  if not FAutoSize then
    Exit;

  R := ClientRect;
  RD := GetTextRect;

  if MaxWidth > 0 then
    RD.Right := RD.Left + MaxWidth
  else
    RD.Right := RD.Left + 4096;

  RD.Bottom := RD.Top + 4096;

  MaxW := 0;
  MaxH := 0;
  Canvas.Font.Assign(Self.Font);

  for i := 0 to FAlertMessages.Count - 1 do
  begin
    if Assigned(FImages) and (integer(AlertMessages.Objects[i])>= 0) and (MaxWidth > 0) and (integer(AlertMessages.Objects[i])>= 0) then
      RD.Right := RD.Left + MaxWidth - FImages.Width - 10;

    HTMLDrawEx(Canvas, AlertMessages.Strings[i], rd, FImages, 0, 0, -1, FHoverLink, 1, False, False, False, False, True, FHover, True,
      1.0, FURLColor, clNone, clNone, clGray, Anchor, Stripped, FocusAnchor, XSize, YSize,
      HyperLinks, MouseLink, hr, FImageCache, FContainer, 0, BidiMode);

    if Assigned(FImages) and (integer(AlertMessages.Objects[i])>= 0) then
      XSize := XSize + FImages.Width + 10;

    MaxW := max(MaxW, XSize);
    MaxH := max(MaxH, YSize + 8);
  end;

  MaxW := GetWidthFromTextWidth(MaxW);
  MaxH := GetHeightFromTextHeight(MaxH);

  NW := MaxW + 16 + BorderSize * 2;
  NH := MaxH + ScrollBtnHeight + BorderSize * 2;

  if (MaxWidth = 0) or (NW < MaxWidth) then
    Width := NW
  else
    Width := MaxWidth;

  if (MaxHeight = 0) or (NH < MaxHeight) then
    Height := NH
  else
    Height := MaxHeight;
end;

//------------------------------------------------------------------------------



function TAlertWindow.GetCloseRect: TRect;
begin
  Result := Rect(Width - CloseBtnSize - 1, 8, Width - 1, 8 + CloseBtnSize);
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.SetAlertMessages(const Value: TStringList);
begin
  FAlertMessages.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.SetBackground(const Value: TPicture);
begin
  FBackground.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.SetGlyphDelete(const Value: TBitmap);
begin
  FGlyphDelete.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.SetGlyphNext(const Value: TBitmap);
begin
  FGlyphNext.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.GlyphNextOnChange(Sender: TObject);
var
  x, y: Integer;
  PxlColor: TColor;
  c: byte;
begin
  FGlyphNextDisabled.Assign(FGlyphNext);
  if not FGlyphNextDisabled.Empty then
  begin
    FGlyphNextDisabled.Width := FGlyphNext.Width;
    FGlyphNextDisabled.Height := FGlyphNext.Height;
    FGlyphNextDisabled.PixelFormat := pf32bit;

    for x := 0 to FGlyphNext.Width - 1 do
      for y := 0 to FGlyphNext.Height - 1 do
      begin
        PxlColor := ColorToRGB(FGlyphNext.Canvas.Pixels[x, y]);

        c := Round((((PxlColor shr 16) + ((PxlColor shr 8) and $00FF) +
          (PxlColor and $0000FF)) div 3)) div 2 + 128;

        FGlyphNextDisabled.Canvas.Pixels[x, y] := RGB(c, c, c);
      end;
  end;

end;

//------------------------------------------------------------------------------

procedure TAlertWindow.SetGlyphPrev(const Value: TBitmap);
begin
  FGlyphPrev.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.GlyphPrevOnChange(Sender: TObject);
var
  x, y: Integer;
  PxlColor: TColor;
  c: byte;
begin
  FGlyphPrevDisabled.Assign(FGlyphPrev);

  if not FGlyphPrevDisabled.Empty then
  begin
    FGlyphPrevDisabled.Width := FGlyphPrev.Width;
    FGlyphPrevDisabled.Height := FGlyphPrev.Height;
    FGlyphPrevDisabled.PixelFormat := pf32bit;

    for x := 0 to FGlyphPrevDisabled.Width - 1 do
      for y := 0 to FGlyphPrevDisabled.Height - 1 do
      begin
        PxlColor := ColorToRGB(FGlyphPrev.Canvas.Pixels[x, y]);
        c := Round((((PxlColor shr 16) + ((PxlColor shr 8) and $00FF) +
          (PxlColor and $0000FF)) div 3)) div 2 + 128;
        FGlyphPrevDisabled.Canvas.Pixels[x, y] := RGB(c, c, c);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.SetGlyphClose(const Value: TBitmap);
begin
  FGlyphClose.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.SetAutoDelete(const Value: Boolean);
begin
  FAutoDelete := Value;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.SetHover(const Value: Boolean);
begin
  FHover := Value;
  FHoverLink := -1;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.WMEraseBkGnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.TestAnchor;
var
  pt: TPoint;
  RD, hr, r: TRect;
  hl, ml: Integer;
  XSize, YSize: Integer;
  a, s, fa: string;
  Anchor: string;
begin
  GetCursorPos(pt);
  pt := ScreenToClient(pt);

  r := ClientRect;

  RD := GetTextRect;

{  RD.Left := R.Left + MarginX;
  RD.Top := R.Top + MarginY;
  RD.Bottom := R.bottom - MarginY;
  RD.Right := R.Right - MarginX;

  InflateRect(RD,-FBorderSize,-FBorderSize);
}
  Anchor := '';

  Canvas.Font.Assign(Self.Font);

  if FScrollPos > 0 then
    RD.Bottom := RD.Bottom + 1000;

  {
  if FAlertMessagesInfo.Count>= (FMsgScroller.Position) then
    if FAlertMessagesInfo.Items[FMsgScroller.Position - 1].Color <> clNone then
      Canvas.Font.Color:= FAlertMessagesInfo.Items[FMsgScroller.Position - 1].Color;
   }
  if FMsgScroller.Position > 0 then
  begin
    if HTMLDrawEx(Canvas, AlertMessages.Strings[FMsgScroller.Position - 1], rd, FImages, pt.X, pt.Y + FScrollPos, -1, -1, 1, True, False, False, False, False, False, True,
      1.0, FURLColor, clNone, clNone, clGray, a, s, fa, XSize, YSize,
      hl, ml, hr, FImageCache, FContainer, 0, BidiMode) then
    begin
      Anchor := a;
      if Assigned(FOnAnchorClick) then
        FOnAnchorClick(Self, a, FMsgScroller.Position - 1)
      else
       ShellExecute(0,'open',PChar(a),nil,nil,SW_NORMAL);
    end;
  end;

  //Windows.SetFocus(ParentWindow);

  if (FHoverLink <> -1) and (FHover) then
    InvalidateRect(Handle, @FHoverRect, True);

end;

procedure TAlertWindow.WMSetFocus(var Msg: TWMSetFocus);
begin
  msg.Result := 0;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.First;
begin
  FMsgScroller.Position := FMsgScroller.Min;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.Last;
begin
  FMsgScroller.Position := FMsgScroller.Max;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.Delete(Index: integer);
begin
  if (index >= FAlertMessages.Count) or (index < 0) then
    raise exception.Create('Invalid Index');

  if index < FMsgScroller.Position - 1 then
    FMsgScroller.Position := FMsgScroller.Position - 1
  else
  if index = FMsgScroller.Position - 1 then
  begin
    if not FMsgScroller.CanGoForward then
    begin
      if FMsgScroller.CanGoBack then
        FMsgScroller.Position := FMsgScroller.Position - 1;
    end;
  end;

  //FAlertMessages.Delete(index);

  if Assigned(FOnDeleteMessage) then
    FOnDeleteMessage(self, Index);

  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.Delete;
begin
  if FMsgScroller.Position > 0 then
    Delete(FMsgScroller.Position - 1);
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.Next;
begin
  if FMsgScroller.CanGoForward then
  begin
    FMsgScroller.Position := FMsgScroller.Position + 1;
    Invalidate;
    if assigned(FOnNextMessage) then
      FOnNextMessage(self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.Previous;
begin
  if FMsgScroller.CanGoBack then
  begin
    FMsgScroller.Position := FMsgScroller.Position - 1;
    Invalidate;
    if Assigned(FOnPrevMessage) then
      FOnPrevMessage(self);
  end;
end;

//------------------------------------------------------------------------------

function TAlertWindow.GetTextRect: TRect;
var
  R: TRect;
  ImgW: integer;
  SH: Integer;
begin
  R := ClientRect;
  ImgW := 0;
  if FImages <> nil then
    ImgW := FImages.Width + 5;

  SH := ScrollBtnHeight;
  if not ShowDelete and (AlertMessages.Count <= 1) then
    SH := 0;
  Result := Rect(R.Left + Imgw + MarginX, R.Top + CloseBtnSize + MarginY, R.Right - CloseBtnSize - MarginX, R.Bottom - MarginY - SH);
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.DrawLeftScrollBtn;
var
  R: TRect;
  //bmp: TBitMap;
begin
  if FMsgScroller.Visible then
  begin
    R := GetLeftScrollBtnRect;
    with Canvas do
    begin
      if FMsgScroller.CanGoBack then
      begin
        if FScrollLeftDown then
        begin
          //Brush.Color:= BtnDownColor; //$00A78F87;
          //FillRect(R);
          R.Bottom := R.Bottom - 1;
          DrawGradient(Canvas, FBtnDownColor, FBtnDownColorTo, 16, R, false);
          R.Bottom := R.Bottom + 1;
          Pen.Color := clBlack;
          Brush.Style := bsClear;
          Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        end
        else if FScrollLeftHover then
        begin
          //Brush.Color:= BtnHoverColor; //$00BBA9A2;
          //FillRect(R);
          R.Bottom := R.Bottom - 1;
          DrawGradient(Canvas, FBtnHoverColor, FBtnHoverColorTo, 16, R, false);
          R.Bottom := R.Bottom + 1;
          Pen.Color := clBlack;
          Brush.Style := bsClear;
          Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        end;
        Font.Color := clBlack;
      end
      else
        Font.Color := clGray;

      if not FGlyphPrev.Empty then
      begin
        FGlyphPrev.Transparent := true;
        if FMsgScroller.CanGoBack then
          Canvas.StretchDraw(Rect(R.left + 2, R.Top + 4, R.left + 18, R.Top + 20), FGlyphPrev)
        else
        begin
          Canvas.StretchDraw(Rect(R.left + 2, R.Top + 4, R.left + 18, R.Top + 20), FGlyphPrevDisabled);
        end;
      end
      else
      begin
        Brush.Style := bsClear;
        TextOut(R.Left + 5, R.Top + 6, '<');
        TextOut(R.Left + 8, R.Top + 6, '<');
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.DrawRightScrollBtn;
var
  R: TRect;
  //bmp: TBitMap;
begin
  if FMsgScroller.Visible then
  begin
    R := GetRightScrollBtnRect;
    with Canvas do
    begin
      if FMsgScroller.CanGoForward then
      begin
        if FScrollRightDown then
        begin
          //Brush.Color:= BtnDownColor; // $00A78F87;
          //FillRect(R);
          R.Bottom := R.Bottom - 1;
          DrawGradient(Canvas, FBtnDownColor, FBtnDownColorTo, 16, R, false);
          R.Bottom := R.Bottom + 1;
          Pen.Color := clBlack;
          Brush.Style := bsClear;
          Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        end
        else if FScrollRightHover then
        begin
          //Brush.Color:= BtnHoverColor; //$00BBA9A2;
          //FillRect(R);
          R.Bottom := R.Bottom - 1;
          DrawGradient(Canvas, FBtnHoverColor, FBtnHoverColorTo, 16, R, false);
          R.Bottom := R.Bottom + 1;
          Pen.Color := clBlack;
          Brush.Style := bsClear;
          Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        end;
        Font.Color := clBlack;
      end
      else
        Font.Color := clGray;

      if not FGlyphNext.Empty then
      begin
        FGlyphNext.Transparent := true;
        if FMsgScroller.CanGoForward then
          Canvas.StretchDraw(Rect(R.left + 2, R.Top + 4, R.left + 18, R.Top + 20), FGlyphNext)
        else
        begin
          Canvas.StretchDraw(Rect(R.left + 2, R.Top + 4, R.left + 18, R.Top + 20), FGlyphNextDisabled);
        end;
      end
      else
      begin
        Brush.Style := bsClear;
        TextOut(R.Left + 5, R.Top + 6, '>');
        TextOut(R.Left + 8, R.Top + 6, '>');
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.DrawScrollBtns;
var
  x: integer;
  S: string;
begin
  if FMsgScroller.Visible then
  begin
    DrawLeftScrollBtn;

    //---- Draw Text
    S := Format(FPositionFormat, [FMsgScroller.Position, FMsgScroller.Max]);
    with Canvas do
    begin
      Font.Assign(self.Font);
      x := TextWidth(S);
      x := (Width div 2) - (x div 2);
      Brush.Style := bsClear;
      TextOut(x, Height - TextHeight('gt') - 5, S);
    end;
    DrawRightScrollBtn;
  end;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.DrawDeleteBtn;
var
  R: TRect;
begin
  if FShowDelete and (FAlertMessages.Count > 0) then
  begin
    R := GetDeleteBtnRect;
    with Canvas do
    begin
      if FDeleteDown then
      begin
        R.Bottom := R.Bottom - 1;
        DrawGradient(Canvas, FBtnDownColor, FBtnDownColorTo, 16, R, false);
        R.Bottom := R.Bottom + 1;
        Pen.Color := clBlack;
        Brush.Style := bsClear;
        Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end
      else if FDeleteHover then
      begin
        R.Bottom := R.Bottom - 1;
        DrawGradient(Canvas, FBtnHoverColor, FBtnHoverColorTo, 16, R, false);
        R.Bottom := R.Bottom + 1;
        Pen.Color := clBlack;
        Brush.Style := bsClear;
        Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end;

      if not FGlyphDelete.Empty then
      begin
        FGlyphDelete.Transparent := true;
        Canvas.StretchDraw(Rect(R.left + 2, R.Top + 4, R.left + 18, R.Top + 20), FGlyphDelete);
      end
      else
      begin
        Pen.Color := clBlack;
                       {/}
        MoveTo(R.Right - 18 + 2 + 3 - 1, R.Top + 7 + 3 + 4 + 1);
        LineTo(R.Right - 18 + 2 + 3 + 6 + 1, R.Top + 7 + 2 - 1);
        MoveTo(R.Right - 18 + 2 + 4 - 1, R.Top + 7 + 3 + 4 + 1);
        LineTo(R.Right - 18 + 2 + 3 + 5 + 1, R.Top + 7 + 2 - 1);
                       {\}
        MoveTo(R.Right - 18 + 2 + 3 - 1, R.Top + 7 + 3 - 1);
        LineTo(R.Right - 18 + 2 + 3 + 6 + 1, R.Top + 7 + 3 + 5 + 1);
        MoveTo(R.Right - 18 + 2 + 4 - 1, R.Top + 7 + 3 - 1);
        LineTo(R.Right - 18 + 2 + 3 + 5 + 1, R.Top + 7 + 3 + 5 + 1);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.DrawPopupBtn;
var
  R: TRect;
begin
  if FShowPopup then
  begin
    R := GetPopupBtnRect;
    with Canvas do
    begin
      if FPopupBtnDown then
      begin
        R.Bottom := R.Bottom - 1;
        DrawGradient(Canvas, FBtnDownColor, FBtnDownColorTo, 16, R, false);
        R.Bottom := R.Bottom + 1;
        Pen.Color := clBlack;
        Brush.Style := bsClear;
        Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end
      else if FPopupBtnHover then
      begin
        R.Bottom := R.Bottom - 1;
        DrawGradient(Canvas, FBtnHoverColor, FBtnHoverColorTo, 16, R, false);
        R.Bottom := R.Bottom + 1;
        Pen.Color := clBlack;
        Brush.Style := bsClear;
        Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end;

      if not FGlyphPopup.Empty then
      begin
        FGlyphPopup.Transparent := true;
        Canvas.StretchDraw(Rect(R.left + 2, R.Top + 4, R.left + 18, R.Top + 20), FGlyphPopup);
      end
      else
      begin
        //Pen.Color := clBlack;
        //TextOut(R.Left + 2, R.Top + 5, 'Popup');
        Pen.Color := clBlack;
               {-------}
        MoveTo(R.Left + 7, R.Bottom - 10);
        LineTo(R.Left + 14, R.Bottom - 10);
                {-----}
        MoveTo(R.Left + 8, R.Bottom - 9);
        LineTo(R.Left + 13, R.Bottom - 9);
                 {---}
        MoveTo(R.Left + 9, R.Bottom - 8);
        LineTo(R.Left + 12, R.Bottom - 8);
                  {-}
        MoveTo(R.Left + 10, R.Bottom - 7);
        LineTo(R.Left + 11, R.Bottom - 7);

        Font.Name := 'MS Sans Serif';
        Brush.Style := bsClear;
        Font.Style := [fsBold];
        Font.Color := clBlack;
        Font.Size := 8;
        TextOut(R.Left + 5, R.Top + 3, '>');
        TextOut(R.Left + 8, R.Top + 3, '>');
      end;
    end;
  end;
end;             

//------------------------------------------------------------------------------

procedure TAlertWindow.DrawScrollDnBtn;
var
  R: TRect;
begin
  if FShowScrollDn then
  begin
    R := GetScrollDnBtnRect;
    with Canvas do
    begin
      if FScrollDnBtnDown then
      begin
        R.Bottom := R.Bottom - 1;
        DrawGradient(Canvas, FBtnDownColor, FBtnDownColorTo, 16, R, false);
        R.Bottom := R.Bottom + 1;
        Pen.Color := clBlack;
        Brush.Style := bsClear;
        Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end
      else if FScrollDnBtnHover then
      begin
        R.Bottom := R.Bottom - 1;
        DrawGradient(Canvas, FBtnHoverColor, FBtnHoverColorTo, 16, R, false);
        R.Bottom := R.Bottom + 1;
        Pen.Color := clBlack;
        Brush.Style := bsClear;
        Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end;

      Pen.Color := clBlack;
             {-------}
      MoveTo(R.Left + 2, R.Bottom - 7);
      LineTo(R.Left + 9, R.Bottom - 7);
              {-----}
      MoveTo(R.Left + 3, R.Bottom - 6);
      LineTo(R.Left + 8, R.Bottom - 6);
               {---}
      MoveTo(R.Left + 4, R.Bottom - 5);
      LineTo(R.Left + 7, R.Bottom - 5);
                {-}
      MoveTo(R.Left + 5, R.Bottom - 4);
      LineTo(R.Left + 6, R.Bottom - 4);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.DrawScrollUpBtn;
var
  R: TRect;
begin
  if FShowScrollUp then
  begin
    R := GetScrollUpBtnRect;
    with Canvas do
    begin
      if FScrollUpBtnDown then
      begin
        R.Bottom := R.Bottom - 1;
        DrawGradient(Canvas, FBtnDownColor, FBtnDownColorTo, 16, R, false);
        R.Bottom := R.Bottom + 1;
        Pen.Color := clBlack;
        Brush.Style := bsClear;
        Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end
      else if FScrollUpBtnHover then
      begin
        R.Bottom := R.Bottom - 1;
        DrawGradient(Canvas, FBtnHoverColor, FBtnHoverColorTo, 16, R, false);
        R.Bottom := R.Bottom + 1;
        Pen.Color := clBlack;
        Brush.Style := bsClear;
        Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end;

      Pen.Color := clBlack;
             {-------}
      MoveTo(R.Left + 2, R.Bottom - 4);
      LineTo(R.Left + 9, R.Bottom - 4);
              {-----}
      MoveTo(R.Left + 3, R.Bottom - 5);
      LineTo(R.Left + 8, R.Bottom - 5);
               {---}
      MoveTo(R.Left + 4, R.Bottom - 6);
      LineTo(R.Left + 7, R.Bottom - 6);
                {-}
      MoveTo(R.Left + 5, R.Bottom - 7);
      LineTo(R.Left + 6, R.Bottom - 7);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAlertWindow.GetLeftScrollBtnRect: TRect;
var
  lf: integer;
begin
  lf := Width div 2;
  lf := lf - ScrollBtnWidth - (ScrollTextWidth div 2);
  Result := Rect(lf, Height - ScrollBtnHeight - 1, lf + ScrollBtnWidth, Height - 1);
end;

//------------------------------------------------------------------------------

function TAlertWindow.GetRightScrollBtnRect: TRect;
var
  lf: integer;
begin
  lf := Width div 2;
  lf := lf + (ScrollTextWidth div 2);
  Result := Rect(lf, Height - ScrollBtnHeight - 1, lf + ScrollBtnWidth, Height - 1);
end;

//------------------------------------------------------------------------------

function TAlertWindow.PtOnLeftScrollBtn(X, Y: integer): Boolean;
begin
  Result := PtInRect(GetLeftScrollBtnRect, Point(X, Y));
end;

//------------------------------------------------------------------------------

function TAlertWindow.PtOnRightScrollBtn(X, Y: integer): Boolean;
begin
  Result := PtInRect(GetRightScrollBtnRect, Point(X, Y));
end;

function TAlertWindow.ScrollTextWidth: Integer;
var
  s: string;
begin
  s := Format(FPositionFormat, [FMsgScroller.Max - 1, FMsgScroller.Max]);
  with Canvas do
    Result := TextWidth(S) + 8;
end;

//------------------------------------------------------------------------------

function TAlertWindow.GetDeleteBtnRect: TRect;
var
  lf: integer;
begin
  lf := Width div 2;
  if FMsgScroller.Visible then
    lf := lf + (ScrollTextWidth div 2) + ScrollBtnWidth
  else
    lf := lf - (ScrollBtnWidth div 2);
  Result := Rect(lf, Height - ScrollBtnHeight - 1, lf + ScrollBtnWidth, Height - 1);
end;

//------------------------------------------------------------------------------

function TAlertWindow.GetScrollUpBtnRect: TRect;
begin
  Result.Left := Width - 16;
  Result.Right := Width - 4;
  Result.Top := 32;
  Result.Bottom := Result.Top + 12;
end;

//------------------------------------------------------------------------------

function TAlertWindow.GetScrollDnBtnRect: TRect;
begin
  Result.Left := Width - 16;
  Result.Right := Width - 4;
  Result.Top := Height - 40;
  Result.Bottom := Result.Top + 12;
end;
//------------------------------------------------------------------------------

function TAlertWindow.GetPopupBtnRect: TRect;
var
  lf: integer;
begin
  if FShowDelete then
  begin
    Result := GetDeleteBtnRect;
    Result.Left := Result.Right + 5;
    Result.Right := Result.Left + GetPopupBtnWidth;
  end
  else if FMsgScroller.Visible then
  begin
    Result := GetRightScrollBtnRect;
    Result.Left := Result.Right + 5;
    Result.Right := Result.Left + GetPopupBtnWidth;

  end
  else
  begin
    lf := Width div 2;
    lf := lf - (GetPopupBtnWidth div 2);
    Result := GetDeleteBtnRect;
    Result.Left := lf;
    Result.Right := Result.Left + GetPopupBtnWidth;
  end;
end;

//------------------------------------------------------------------------------

function TAlertWindow.PtOnDeleteBtn(X, Y: integer): Boolean;
begin
  Result := PtInRect(GetDeleteBtnRect, Point(X, Y));
end;

//------------------------------------------------------------------------------

function TAlertWindow.PtOnScrollUpBtn(X, Y: integer): Boolean;
begin
  Result := PtInRect(GetScrollUpBtnRect, Point(X,Y));
end;

//------------------------------------------------------------------------------

function TAlertWindow.PtOnScrollDnBtn(X, Y: integer): Boolean;
begin
  Result := PtInRect(GetScrollDnBtnRect, Point(X,Y));
end;

//------------------------------------------------------------------------------

function TAlertWindow.PtOnPopupBtn(X, Y: integer): Boolean;
begin
  Result := PtInRect(GetPopupBtnRect, Point(X, Y));
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.Hide;
var
  CanClose: Boolean;
begin
  if Visible or AlwaysOnTop then
  begin
    CanClose := True;

    if Assigned(FOnCanClose) then
      FOnCanClose(Self, CanClose);

    if CanClose then
    begin
      ShowWindow(Handle, SW_HIDE);
      Visible := False;
      if Assigned(FOnClose) then
        FOnClose(self);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.AlertMessagesOnChange(Sender: TObject);
begin
  if (csDestroying in ComponentState) then
    Exit;

    

  FMsgScroller.Max := FAlertMessages.Count;
  
  if FAlertMessages.Count <= 0 then
  begin
    FMsgScroller.Min := 0;
    FMsgScroller.Position := 0;
    FMsgScroller.Max := 0;
    if not FAutoHiding then
      Hide;
  end
  else
    FMsgScroller.Min := 1;
  if FMsgScroller.Position = 0 then
    FMsgScroller.Position := FMsgScroller.Min;

  FMsgScroller.Visible := FAlertMessages.Count > 1;
  
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAlertWindow.GetHeightFromTextHeight(H: integer): integer;
var
  ImgH, Cbtns: integer;

begin
  ImgH := 0;
  CBtns := CloseBtnSize + 5;

  if Assigned(FImages) then
    ImgH := FImages.Height + CBtns;

  if FMsgScroller.Visible then
    Result := Max(H + CBtns,ImgH) + MarginY * 2 + ScrollBtnHeight
  else
    Result := Max(H + CBtns,ImgH) + MarginY * 2;

  Result := Max(Result, MinWindowHeight);
end;

//------------------------------------------------------------------------------

function TAlertWindow.GetWidthFromTextWidth(w: integer): integer;
var
  ImgW: integer;
begin
  ImgW := 0;
  if FImages <> nil then
    ImgW := FImages.Width + 5;

  Result := W + Imgw + MarginX * 2 + CloseBtnSize;
  Result := Max(Result, MinWindowWidth);
end;

//------------------------------------------------------------------------------

function TAlertWindow.PtOnCaption(X, Y: integer): Boolean;
begin
  Result := PtInRect(Rect(1, 1, width - 1, 7), Point(X, Y));
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.SetShowDelete(const Value: Boolean);
begin
  FShowDelete := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAlertWindow.GetActiveMessage: Integer;
begin

  Result := FMsgScroller.Position;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.SetActiveMessage(const Value: Integer);
begin
  FMsgScroller.Position := Value;
  Invalidate;
end;

procedure TAlertWindow.PositionChanged(Sender: TObject);
begin
  FScrollPos := 0;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.SetPositionFormat(const Value: string);
begin
  FPositionFormat := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.SetShowPopup(const Value: Boolean);
begin
  FShowPopup := Value;
  //SetAutoSizeEx(FAutoSize);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.SetGlyphPopup(const Value: TBitmap);
begin
  FGlyphPopup.Assign(Value);
end;

//------------------------------------------------------------------------------

function TAlertWindow.MinWindowHeight: integer;
var
  i: integer;
begin
  i := 10;
  if FImages <> nil then
    i := i + FImages.Height;

  if FMsgScroller.Visible or FShowDelete or FShowPopup then
    Result := 75
  else
    Result := Max(i, CloseBtnSize + 10);
end;

//------------------------------------------------------------------------------

function TAlertWindow.MinWindowWidth: integer;
var
  i: integer;
begin
  i := 10;
  if FImages <> nil then
    i := i + FImages.Width;

  if FMsgScroller.Visible then
    Result := 145
  else
    Result := CloseBtnSize + i;

  if FShowPopup then
    if FShowDelete then
      Result := Result + GetPopupBtnWidth * 2 + 10
end;

//------------------------------------------------------------------------------

procedure TAlertWindow.Loaded;
begin
  inherited;
  FOriginalHint := Hint;
end;

//------------------------------------------------------------------------------

function TAlertWindow.GetPopupBtnWidth: integer;
begin
  Result := CloseBtnSize // 35;
end;

//------------------------------------------------------------------------------
//--------------------------{ TFadeThread }-------------------------------------
//------------------------------------------------------------------------------

constructor TFadeThread.Create(aLayeredWindow: TAdvAlertWindow);
begin
  FLayeredWindow := aLayeredWindow;
  FreeOnTerminate := True;
  inherited Create(false);
end;

//------------------------------------------------------------------------------

procedure TFadeThread.Execute;
var
  t: Integer;
  ti: DWORD;
begin
  if FLayeredWindow.FFadeIn then
  begin
    FLayeredWindow.FAlphaActual := FLayeredWindow.AlphaStart;

    Synchronize(FLayeredWindow.Update);

    t := FLayeredWindow.AlphaStart;

    while ((t < fLayeredWindow.fAlphaEnd) and not Terminated and not fLayeredWindow.FShowFullAlpha) do
    begin
      if FadeAction = faShow then
      begin
        FadeAction := faNone;
        FLayeredWindow.FAlphaActual := FLayeredWindow.fAlphaEnd;
        Synchronize(FLayeredWindow.Update);
        //fLayeredWindow.AlphaActual := fLayeredWindow.fAlphaEnd;
        Break;
      end;

      fLayeredWindow.FAlphaActual := t;
      Synchronize(FLayeredWindow.Update);

      t := t + FLayeredWindow.fFadeStep;

      ti := GetTickCount;
      while (GetTickCount - ti < DWORD(fLayeredWindow.FFadeTime)) do
      begin
        //Application.ProcessMessages;
        Sleep(1);
      end;
    end;
    if not fLayeredWindow.FShowFullAlpha and not Terminated then
    begin
      fLayeredWindow.fAlphaActual := fLayeredWindow.fAlphaEnd;
      Synchronize(FLayeredWindow.Update);
    end;
  end;

  if not FLayeredWindow.FFadeIn then
  begin
    t := FLayeredWindow.FAlphaEnd;
    while (t > 0) and not Terminated and not fLayeredWindow.FShowFullAlpha do
    begin
      FLayeredWindow.FAlphaActual := t;
      Synchronize(fLayeredWindow.Update);

      t := t - fLayeredWindow.FFadeStep;

      if FadeAction = faShow then
      begin
        FLayeredWindow.FAlphaActual := FLayeredWindow.FAlphaEnd;
        Synchronize(FLayeredWindow.Update);
        Break;
      end;

      ti := GetTickCount;

      while (GetTickCount - ti < DWORD(FLayeredWindow.FFadeTime)) do
      begin
        //Application.ProcessMessages;
        Sleep(1);
      end;
    end;

    if not fLayeredWindow.FShowFullAlpha and not Terminated and not (FadeAction = faShow) then
    begin
      FLayeredWindow.FAlphaActual := FLayeredWindow.AlphaStart;
      Synchronize(fLayeredWindow.Update);
    end;

    if (FadeAction <> faShow) then
    begin
      FLayeredWindow.DoHide;
    end;

    FadeAction := faNone;
  end;

end;

//------------------------------------------------------------------------------
//-------------------------{ TAdvAlertWindow }----------------------------------
//------------------------------------------------------------------------------

constructor TAdvAlertWindow.Create(AOwner: TComponent);
begin
  inherited;
  FOwner := TWinControl(AOwner);
  FAlertMessages := CreateMsgCollection;
  FAlertMessages.OnChange := AlertMessagesOnChange;
  FAlertMessages.OnDeleteItem := AlertMessagesOnDeleteItem;
  FAlertWindow := nil;
  FBackground := TPicture.Create;
  FWidth := 300;
  FHeight := 100;
  FBidiMode := bdLeftToRight;
  FWindowColor := clWhite;
  FWindowColorTo := clBtnFace;
  FBorderColor := clActiveCaption;
  FCaptionColor := clWhite;
  FCaptionColorTo := clGray;
  FBtnHoverColor := $00BBA9A2;
  FBtnHoverColorTo := $00BBA9A2;
  FBtnDownColor := $00A78F87;
  FBtnDownColorTo := $00A78F87;
  FURLColor := clBlue;
  FMarginX := 4;
  FMarginY := 1;
  FScreenMarginX := 0;
  FScreenMarginY := 0;
  FFont := TFont.Create;
  FAlphaEnd := 180;
  FFadeStep := 2;
  FFadeTime := 50;
  FFadeThread := nil;
  FWindowPosition := wpRightBottom;
  FAutoHide := true;
  FDisplayTime := 5000;
  FDisplayCounter := 0;
  FTimer := TTimer.Create(self);
  FTimer.Enabled := false;
  FTimer.Interval := 100;
  FTimer.OnTimer := OnTimer;

  FHintNextBtn := 'Next';
  FHintPrevBtn := 'Previous';
  FHintCloseBtn := 'Close';
  FHintDeleteBtn := 'Delete';
  FHintPopupBtn := 'Popup';
  FPositionFormat := '%d of %d';

  FShowDelete := true;
  FGlyphDelete := TBitmap.Create;
  FGlyphNext := TBitmap.Create;
  FGlyphPrev := TBitmap.Create;
  FGlyphClose := TBitmap.Create;
  FGlyphPopup := TBitmap.Create;
  Style := asOffice2003Blue;
  FAutoThemeAdapt := false;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.DeleteMessage(Index: integer);
begin
  FAlertWindow.Delete(Index);
end;

destructor TAdvAlertWindow.Destroy;
begin
  if (FFadeThread <> nil) then
  begin
    FFadeThread.FreeOnTerminate := false;
    try
      FFadeThread.Free;
    except
    end;
  end;

  if Assigned(FAlertWindow) then
    FAlertWindow.Free;


  FFont.Free;
  FTimer.Free;
  FAlertMessages.Free;
  FBackground.Free;
  FGlyphDelete.Free;
  FGlyphNext.Free;
  FGlyphPrev.Free;
  FGlyphClose.Free;
  FGlyphPopup.Free;
  inherited;
end;

procedure TAdvAlertWindow.Loaded;
begin
  if not (csDesigning in ComponentState) then
    if PersistPosition then
      LoadPosition;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.AlertMessagesOnChange(Sender: TObject);
begin
  if (csDestroying in ComponentState) then
    Exit;

  UpdateAlertMessages;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.AlertMessagesOnDeleteItem(Sender: TObject; Index: integer);
var
  p: TDeleteMessageEvent;
begin
  if (csDestroying in ComponentState) then
    Exit;

  if Assigned(FAlertWindow) then
    if FAlertWindow.Visible then
    begin
      P := FAlertWindow.OnDeleteMessage;
      FAlertWindow.OnDeleteMessage := nil;
      FAlertWindow.Delete(Index);
      if (FAlertWindow.AlertMessagesInfo.Count >= Index) then
        FAlertWindow.AlertMessagesInfo.Delete(Index);
      FAlertWindow.OnDeleteMessage := P;
      if Assigned(FOnDeleteMessage) then
        FOnDeleteMessage(self, index);

      if FAlertWindow.AlertMessagesInfo.Count = 0 then
      begin
        FAlertWindow.Hide;
        ActiveMessage := 0;
      end;
      
    end;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.UpdateAlertMessages;
var
  i, j: integer;
  s: string;
begin
  if AlertMessages.Count < ActiveMessage then
    ActiveMessage := AlertMessages.Count;

  if Assigned(FAlertWindow) then
    if FAlertWindow.Visible then
    begin
      for i := 0 to AlertMessages.Count - 1 do
      begin
        s := '';
        for j := 0 to AlertMessages.Items[i].Text.Count - 1 do
          s := s + AlertMessages.Items[i].Text.Strings[j];
        if FAlertWindow.AlertMessages.Count <= i then
          FAlertWindow.AlertMessages.AddObject(s, pointer(AlertMessages.Items[i].ImageIndex))
        else
        begin
          FAlertWindow.AlertMessages[i] := s;
          FAlertWindow.AlertMessages.Objects[i] := Pointer(AlertMessages.Items[i].ImageIndex);
        end;

        if FAlertWindow.AlertMessagesInfo.Count <= i then
        begin
          with FAlertWindow.AlertMessagesInfo.Add do
          begin
            ImageIndex := AlertMessages.Items[i].ImageIndex;
            Text.CommaText := AlertMessages.Items[i].Text.CommaText;
            Tag := AlertMessages.Items[i].Tag;
            Color := AlertMessages.Items[i].Color;
            ColorTo := AlertMessages.Items[i].ColorTo;
          end;
        end
        else
        begin
          FAlertWindow.AlertMessagesInfo.Items[i].ImageIndex := AlertMessages.Items[i].ImageIndex;
          FAlertWindow.AlertMessagesInfo.Items[i].Text.CommaText := AlertMessages.Items[i].Text.CommaText;
          FAlertWindow.AlertMessagesInfo.Items[i].Tag := AlertMessages.Items[i].Tag;
          FAlertWindow.AlertMessagesInfo.Items[i].Color := AlertMessages.Items[i].Color;
          FAlertWindow.AlertMessagesInfo.Items[i].ColorTo := AlertMessages.Items[i].ColorTo;
        end;
      end;
    end;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.AlertWindowMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  AlphaActual := 255;
  if not FShowFullAlpha then
  begin
    FShowFullAlpha := true;
    if (FFadeThread <> nil) then
    begin
      FFadeThread.Terminate;
      //FFadeThread.Free;
    end;

    FShowFullAlpha := false;
    AlphaActual := 255;
    FShowFullAlpha := true;
    SetTimer(false);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.DoHide;
begin
  if Assigned(FOnHideMessage) then
    FOnHideMessage(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetMessages;
var
  i, j: integer;
  s: string;
  vis: Boolean;
begin
  if not Assigned(FAlertWindow) then
    FAlertWindow := TAlertWindow.Create(nil);

  vis := FAlertWindow.Visible;

  if BidiMode = bdRightToLeft then
    SetWindowLong(FAlertWindow.Handle, GWL_EXSTYLE, GetWindowLong(FAlertWindow.Handle, GWL_EXSTYLE) OR WS_EX_LAYOUTRTL OR WS_EX_NOINHERITLAYOUT);

  FAlertWindow.AlertMessages.BeginUpdate;
  FAlertWindow.AlertMessagesInfo.BeginUpdate;
  FAlertWindow.AlertMessages.Clear;
  FAlertWindow.AlertMessagesInfo.Clear;

  for i := 0 to AlertMessages.Count - 1 do
  begin
    s := '';
    for j := 0 to AlertMessages.Items[i].Text.Count - 1 do
      s := s + AlertMessages.Items[i].Text.Strings[j];

    FAlertWindow.AlertMessages.AddObject(s, pointer(AlertMessages.Items[i].ImageIndex));

    with FAlertWindow.AlertMessagesInfo.Add do
    begin
      ImageIndex := AlertMessages.Items[i].ImageIndex;
      Text.CommaText := AlertMessages.Items[i].Text.CommaText;
      Tag := AlertMessages.Items[i].Tag;
      Color := AlertMessages.Items[i].Color;
      ColorTo := AlertMessages.Items[i].ColorTo;
    end;
  end;

  FAlertWindow.AlertMessages.EndUpdate;
  FAlertWindow.AlertMessagesInfo.EndUpdate;

  FAlertWindow.AutoSize :=  FAlertWindow.AutoSize;

  if Assigned(FOnShowMessage) then
    FOnShowmessage(Self);

  if AlertMessages.Count > 0 then
    FAlertWindow.Visible := vis;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.ApplyStyle;
begin
  FAlertWindow.WindowColor := WindowColor;
  FAlertWindow.WindowColorTo := WindowColorTo;
  FAlertWindow.CaptionColor := CaptionColor;
  FAlertWindow.CaptionColorTo := CaptionColorTo;
  FAlertWindow.BorderColor := BorderColor;
  FAlertWindow.BtnHoverColor := BtnHoverColor;
  FAlertWindow.BtnHoverColorTo := BtnHoverColorTo;
  FAlertWindow.BtnDownColor := BtnDownColor;
  FAlertWindow.BtnDownColorTo := BtnDownColorTo;
  FAlertWindow.HintNextBtn := HintNextBtn;
  FAlertWindow.HintPrevBtn := HintPrevBtn;
  FAlertWindow.HintCloseBtn := HintCloseBtn;
  FAlertWindow.HintDeleteBtn := HintDeleteBtn;
  FAlertWindow.HintPopupBtn := HintPopupBtn;
  FAlertWindow.GradientDirection := GradientDirection;
  FAlertWindow.Background := Background;
  FAlertWindow.MarginX := MarginX;
  FAlertWindow.MarginY := MarginY;
  FAlertWindow.Font.Assign(FFont);
  FAlertWindow.AutoHide := false; //AutoHide;
  FAlertWindow.BorderSize := BorderSize;
  FAlertWindow.MaxWidth := MaxWidth;
  FAlertWindow.MaxHeight := MaxHeight;
  FAlertWindow.ShowPopup := FShowPopup;
  FAlertWindow.ShowScrollers := FShowScrollers;
  FAlertWindow.AutoSize := AutoSize;
  FAlertWindow.URLColor := FURLColor;
  FAlertWindow.Hover := Hover;
  FAlertWindow.PositionFormat := PositionFormat;
  FAlertWindow.AutoDelete := FAutoDelete;
  FAlertWindow.ShowDelete := FShowDelete;
  FAlertWindow.GlyphPrev.Assign(GlyphPrev);
  FAlertWindow.GlyphNext.Assign(GlyphNext);
  FAlertWindow.GlyphDelete.Assign(GlyphDelete);
  FAlertWindow.GlyphClose.Assign(GlyphClose);
  FAlertWindow.GlyphPopup.Assign(FGlyphPopup);
end;

//------------------------------------------------------------------------------
procedure TAdvAlertWindow.CreateAlertWindow;
begin
  if not Assigned(FAlertWindow) then
  begin
    FAlertWindow := TAlertWindow.Create(nil);

    FAlertWindow.Visible := False;
    FAlertWindow.Parent := nil;


    if Assigned(FOwner) and not AlwaysOnTop then
    begin
      if (Owner is TDataModule) then
      begin
        if Assigned(Application.MainForm) then
          FAlertWindow.ParentWindow := Application.MainForm.Handle
        else
          FAlertWindow.ParentWindow := Application.Handle;
      end
      else
        FAlertWindow.ParentWindow := FOwner.Handle;
    end;

    FAlertWindow.OnMouseMove := AlertWindowMouseMove;
    FAlertWindow.OnMouseLeave := AlertWindowOnMouseLeave;
    FAlertWindow.OnWindowMoved := AlertWindowMoved;
    FAlertWindow.OnDeleteMessage := AlertWindowOnDelete;
    FAlertWindow.OnClose := AlertWindowOnClose;
    FAlertWindow.OnCanClose := AlertWindowOnCanClose;
    FAlertWindow.OnNextMessage := AlertWindowOnNextMessage;
    FAlertWindow.OnPrevMessage := AlertWindowOnPrevMessage;
    FAlertWindow.OnPopupbtnClick := AlertWindowOnPopupbtnClick;
    FAlertWindow.AlwaysOnTop := FAlwaysOnTop;
    FAlertWindow.OnAnchorClick := WindowAnchorClick;
    FAlertWindow.OnCustomPaint := CustomPaint;
    FAlertWindow.OnDestroy := AlertDestroyed;
    FAlertWindow.OnAlertClick := AlertClick;
  end;

  SetMessages;
  FAlertWindow.Width := FWidth;
  FAlertWindow.Height := FHeight;
  FAlertWindow.Left := FLeft;
  FAlertWindow.Top := FTop;
  FAlertWindow.PictureContainer := FContainer;
  FAlertWindow.Images := FImages;

  ApplyStyle;
end;

function TAdvAlertWindow.CreateMsgCollection: TMsgCollection;
begin
  Result := TMsgCollection.Create(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.Hide;
begin
  if Assigned(FAlertWindow) then
  begin
    FFadeIn := False;
    if not FFading then
    begin
      FFading := True;
      FFadeThread := TFadeThread.Create(self);
      FFadeThread.OnTerminate := ThreadDone;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.CloseAlert;
begin
  if (FFadeThread <> nil) then
  begin
    FFadeThread.Terminate;
    Sleep(250);
  end;

  if Assigned(FAlertWindow) then
  begin
    FAlertWindow.Hide;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;

  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;

  if (AOperation = opRemove) and (AComponent = FPopupMenu) then
    FPopupMenu := nil;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetAlphaStart(const Value: byte);
begin
  FAlphaStart := Value;
//  Update;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetAlphaActual(const Value: byte);
begin
  FAlphaActual := Value;
  Update;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetAlphaEnd(const Value: byte);
begin
  fAlphaEnd := Value;
end;

function TAdvAlertWindow.SetAlertPos: integer;
var
  tp,tl: integer;
  r: TRect;
  {$IFDEF DELPHIXE_LVL}
  MonInfo: TMonitorInfo;
  {$ENDIF}

begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @r, 0);
  tp := FTop;
  tl := 0;

 {$IFDEF DELPHIXE_LVL}
 try
   MonInfo.cbSize := SizeOf(MonInfo);
   GetMonitorInfo(MonitorFromWindow(FAlertWindow.ParentWindow, MONITOR_DEFAULTTONEAREST), @MonInfo);
   R.Left := MonInfo.rcWork.Left;
   R.Top := MonInfo.rcWork.Top;
   R.Bottom := MonInfo.rcWork.Bottom;
   R.Right := MonInfo.rcWork.Right;
   tl := MonInfo.rcWork.Left;
 except
 end;
 {$ENDIF}

  if FWindowPosition = wpPreset then
  begin
    tp := FTop;
    FAlertWindow.Left := FLeft;
  end
  else
  begin
    case FWindowPosition of
      wpLeftTop:
        begin
          tp := r.Top + FScreenMarginY;
          FAlertWindow.Left := R.Left + 2 + FScreenMarginX;
        end;
      wpRightTop:
        begin
          tp := r.Top + FScreenMarginY;
          FAlertWindow.Left := R.Right - FAlertWindow.Width - 2 - FScreenMarginX;
        end;
      wpLeftBottom:
        begin
          tp := r.Bottom - FAlertWindow.Height - FScreenMarginY;
          FAlertWindow.Left := R.Left + 2 + FScreenMarginX;
        end;
      wpRightBottom:
        begin
          tp := r.Bottom - FAlertWindow.Height - FScreenMarginY;
          FAlertWindow.Left := r.Right - FAlertWindow.Width - 2 - FScreenMarginX;
        end;
      wpCenter:
        begin
          tp := (r.Bottom - r.Top - FAlertWindow.Height - FScreenMarginY) div 2;
          FAlertWindow.Left := tl + (r.Right - r.Left - FAlertWindow.Width - FScreenMarginX) div 2;
        end;
    end;
  end;

  Result := tp;
  FAlertWindow.Top := tp;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.Show;
var
  tp: integer;
  wnd: THandle;
  blocked: boolean;
begin
  FDisplayCounter := 0;
  blocked := false;

  wnd := GetFocus;

  try
    if FFading and not FFadeIn then
    begin
      SetMessages;
      FFadein := true;
      FFading := false;
      FFadeThread.Terminate;
      blocked := true;
    end
    else
      if (FFading  or IsVisible) then
      begin
        SetMessages;
        if AutoSize then
        begin
          FAlertWindow.AutoSize := true;
          SetAlertPos;
        end;
        Exit;
      end;

    if FAlertMessages.Count = 0 then
      Exit;

    CreateAlertWindow;

    SetTimer(False);
    SetTimer(AutoHide);

    if FAutoThemeAdapt then
      ThemeAdapt;

    FAlertWindow.WindowColor := WindowColor;
    FAlertWindow.WindowColorTo := WindowColorTo;
    FAlertWindow.BtnDownColor := BtnDownColor;
    FAlertWindow.BtnDownColorTo := BtnDownColorTo;
    FAlertWindow.BtnHoverColor := BtnHoverColor;
    FAlertWindow.BtnHoverColorTo := BtnHoverColorTo;
    FAlertWindow.CaptionColor := CaptionColor;
    FAlertWindow.CaptionColorTo := CaptionColorTo;
    FAlertWindow.GradientDirection := GradientDirection;
    FAlertWindow.Font.Assign(self.Font);

    tp := SetAlertPos;

    FAlphaActual := FAlphaStart;
    Update;

    FFadeIn := true;

    if not IsVisible or blocked then
    begin
      FAlertWindow.Visible := True;
      FAlertWindow.Paint;  //force paint for the first time, otherwise we have a black/gray screen till we process the paint message (can take some time if app is still loading)

      SetWindowPos(FAlertWindow.Handle, HWND_TOPMOST, 0, 0, 0, 0,
        SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOMOVE or SWP_NOOWNERZORDER);

      FAlertWindow.Top := tp;

      if (AlphaStart < AlphaEnd) then
      begin
        FFading := true;
        FFadeThread := TFadeThread.Create(self);
        FFadeThread.OnTerminate := ThreadDone;
      end;
    end
    else
    begin
      FadeAction := faShow;
      FAlertWindow.Top := tp;
    end;

  finally
    if wnd <> 0 then
      Windows.SetFocus(wnd);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.ThreadDone(Sender: TObject);
begin
  if FFadeIn then
  begin
    if not FShowFullAlpha then
      SetTimer(true);
  end
  else
  begin
    if not FShowFullAlpha then
      FAlertWindow.Visible := false;
  end;
  FFadeThread := nil;
  FFading := false;
  FFadeOut := false;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.Update;
begin
  if csDesigning in ComponentState then
    Exit;

  if Assigned(FAlertWindow) and FAlertWindow.HandleAllocated then
  begin
    if not FShowFullAlpha or (FShowFullAlpha and (FAlphaStart = 255)) then
    begin
      with (FAlertWindow as THintWindow) do
        WindowBlend(Handle, Canvas.Handle, ColortoRGB(FWindowColor), FAlphaActual, ClientRect);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.AlertWindowOnMouseLeave(Sender: TObject);
var
  FAS: byte;
begin
  if FShowFullAlpha then
  begin
    FShowFullAlpha := False;
    if Assigned(FAlertWindow) and FAlertWindow.HandleAllocated  then
    begin
      FAS := FAlphaEnd;
      with (FAlertWindow as THintWindow) do
        WindowBlend(Handle, Canvas.Handle, ColortoRGB(FWindowColor), FAS, ClientRect);
    end;
    SetTimer(True);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.AlertWindowOnClose(Sender: TObject);
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.AlertWindowOnCanClose(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(FOnCanClose) then
    FOnCanClose(Self, CanClose);
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.AlertWindowOnNextMessage(Sender: TObject);
begin
  if Assigned(FOnNextMessage) then
    FOnNextMessage(self);
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.AlertWindowOnPrevMessage(Sender: TObject);
begin
  if Assigned(FOnPrevMessage) then
    FOnPrevMessage(self);
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.AlertWindowOnDelete(Sender: TObject; index: integer);
var
  p: TOnDeleteItemEvent;
  CanDelete: Boolean;
begin
  if (index >= FAlertMessages.Count) or (index < 0) then
    raise exception.Create('Invalid Index');

  CanDelete := true;

  if Assigned(OnCanDeleteMessage) then
    OnCanDeleteMessage(self, Index, CanDelete);

  if CanDelete then
  begin
    p := FAlertMessages.OnDeleteItem;
    FAlertMessages.OnDeleteItem := nil;

    FAlertWindow.AlertMessages.Delete(Index);

    if AlertMessages.Count <> 0 then
      AlertMessages.Delete(index);

    if (FAlertWindow.AlertMessagesInfo.Count >= Index) then
      FAlertWindow.AlertMessagesInfo.Delete(Index);
    FAlertMessages.OnDeleteItem := P;
    if Assigned(FOnDeleteMessage) then
      FOnDeleteMessage(self, index);
    FAlertWindow.Invalidate;
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.AlertWindowOnPopupBtnClick(Sender: TObject);
var
  r: TRect;
begin
  if Assigned(FPopupMenu) then
  begin
    r := FAlertWindow.GetPopupBtnRect;
    FPopupMenu.Popup(FAlertWindow.Left + r.Right, FAlertWindow.Top + r.Top)

    //if WindowPosition in [wpLeftTop, wpLeftBottom, wpPreset] then
    //  FPopupMenu.Popup(FAlertWindow.Left+FAlertWindow.Width, FAlertWindow.Top)
    //
    //else if WindowPosition in [wpRightTop, wpRightBottom] then
    //  FPopupMenu.Popup(FAlertWindow.Left, FAlertWindow.Top);
  end
  else
    if Assigned(FOnPopupClick) then
      FOnPopupClick(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetImages(const Value: TImageList);
begin
  FImages := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.TextChanged(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.WindowAnchorClick(Sender: TObject;
  Anchor: string; index: integer);
begin
  if Assigned(FOnAnchorClick) then
    FOnAnchorClick(Self, Anchor, FAlertMessages.Items[Index]);
end;

procedure TAdvAlertWindow.AlertClick(Sender: TObject);
begin
  if Assigned(OnAlertClick) then
    OnAlertClick(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.OnTimer(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
  begin
    inc(FDisplayCounter, 100);
    if (FDisplayCounter >= FDisplayTime) then
    begin
      try
        FDisplayCounter := 0;
        if Assigned(FAlertWindow) then
          FAlertWindow.AutoHiding := true;
        Hide;
      finally
        FTimer.Enabled := false;
        if Assigned(FAlertWindow) then
        begin
          if Assigned(OnAutoHide) then
            OnAutoHide(Self);
          FAlertWindow.AutoHiding := false;
        end;  
      end;
    end;
  end;  
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetDisplayTime(const Value: integer);
begin
  FDisplayTime := Value;
  FTimer.Interval := 100;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.First;
begin
  if Assigned(FAlertWindow) then
    if FAlertWindow.Visible then
      FAlertWindow.First;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.Last;
begin
  if Assigned(FAlertWindow) then
    if FAlertWindow.Visible then
      FAlertWindow.Last;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.Next;
begin
  if Assigned(FAlertWindow) then
    if FAlertWindow.Visible then
      FAlertWindow.Next;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.Previous;
begin
  if Assigned(FAlertWindow) then
    if FAlertWindow.Visible then
      FAlertWindow.Previous;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetBackground(const Value: TPicture);
begin
  FBackground.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetGlyphDelete(const Value: TBitmap);
begin
  FGlyphDelete.Assign(Value);
end;



//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetGlyphPopup(const Value: TBitmap);
begin
  FGlyphPopup.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetPopupMenu(const Value: TPopupMenu);
begin
  FPopupMenu := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetGlyphNext(const Value: TBitmap);
begin
  FGlyphNext.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetGlyphPrev(const Value: TBitmap);
begin
  FGlyphPrev.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetAutoHide(const Value: Boolean);
begin
  FAutoHide := Value;
  SetTimer(FAutoHide);
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetAutoDelete(const Value: Boolean);
begin
  FAutoDelete := Value;
  if Assigned(FAlertWindow) then
    FAlertWindow.AutoDelete := FAutoDelete;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetShowDelete(const Value: Boolean);
begin
  FShowDelete := Value;
  if Assigned(FAlertWindow) then
    FAlertWindow.ShowDelete := FShowDelete;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetShowPopup(const Value: Boolean);
begin
  FShowPopup := Value;
  if Assigned(FAlertWindow) then
    FAlertWindow.ShowPopup := FShowPopup;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetWindowPosition(const Value: TWindowPosition);
begin
  FWindowPosition := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.AlertWindowMoved(Sender: TObject);
begin
  FWindowPosition := wpPreset;
  FLeft := FAlertWindow.Left;
  FTop := FAlertWindow.Top;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetTimer(Active: Boolean);
begin
  if (FTimer = nil) or (csDestroying in FTimer.ComponentState) then
    Exit;

  if FAutoHide and Active then
  begin
    FTimer.Enabled := true;
    FDisplayCounter := 0;
  end
  else
    FTimer.Enabled := false;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetGlyphClose(const Value: TBitmap);
begin
  FGlyphClose.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetComponentStyle(AStyle: TTMSStyle);
begin
  Style := TAdvAlertWindowStyle(AStyle);
end;

procedure TAdvAlertWindow.SetStyle(const Value: TAdvAlertWindowStyle);
begin
  if (FStyle <> Value) or (1 > 0) then
  begin
    FStyle := Value;
    Font.Color := clBlack;
    case FStyle of
      asOffice2003Blue:
        begin
          WindowColor := $FADAC4;
          WindowColorTo := $F5BFA0;


          BtnDownColor := $087FE8;
          BtnDownColorTo := $7CDAF7;

          BtnHoverColor := $DCFFFF;
          BtnHoverColorTo := $5BC0F7;

          CaptionColor := $D68759;
          CaptionColorTo := $962D00;

          BorderColor := $962D00;

          GradientDirection := gdVertical;
        end;
      asOffice2003Olive:
        begin
          WindowColor := $E4F1F2;
          WindowColorTo := $AADADA;


          BtnDownColor := $087FE8;
          BtnDownColorTo := $7CDAF7;

          BtnHoverColor := $DCFFFF;
          BtnHoverColorTo := $5BC0F7;

          CaptionColor := $6F8E78;
          CaptionColorTo := $588060;

          BorderColor := $588060;

          GradientDirection := gdVertical;
        end;
      asOffice2003Silver:
        begin
          WindowColor := $F7F3F3;
          WindowColorTo := $E6D8D8;


          BtnDownColor := $087FE8;
          BtnDownColorTo := $7CDAF7;

          BtnHoverColor := $DCFFFF;
          BtnHoverColorTo := $5BC0F7;

          CaptionColor := $BFA7A8;
          CaptionColorTo := $947C7C;

          BorderColor := $947C7C;

          GradientDirection := gdVertical;
        end;
      asOffice2003Classic:
        begin
          WindowColor := clWhite;
          WindowColorTo := $C9D1D5;

          BtnDownColor := $B59285;
          BtnDownColorTo := $B59285;

          BtnHoverColor := $D2BDB6;
          BtnHoverColorTo := $D2BDB6;

          CaptionColor := $C8D0D4;
          CaptionColorTo := $8C8D8E;

          BorderColor := clActiveCaption;
          GradientDirection := gdVertical;
        end;
      asWhidbey:
        begin
          WindowColor := $F5F9FA;
          WindowColorTo := $A8C0C0;

          BtnDownColor := $087FE8;
          BtnDownColorTo := $7CDAF7;

          BtnHoverColor := $DCFFFF;
          BtnHoverColorTo := $5BC0F7;

          CaptionColor := $EBEEEF;
          CaptionColorTo := $7E9898;

          BorderColor := $7E9898;

          GradientDirection := gdVertical;

        end;

      asOffice2007Luna:
        begin
          WindowColor := $FFF4E3;
          WindowColorTo := $EDD9C8;

          BtnDownColor := $087FE8;
          BtnDownColorTo := $7CDAF7;

          BtnHoverColor := $DCFFFF;
          BtnHoverColorTo := $5BC0F7;

          CaptionColor := $FFEFE3;
          CaptionColorTo := $FFD2AF;

          BorderColor := $FFD2AF;

          GradientDirection := gdVertical;
        end;

      asOffice2007Obsidian:
        begin
          WindowColor := $F1F0E6;
          WindowColorTo := $C6BCB5;

          BtnDownColor := $087FE8;
          BtnDownColorTo := $7CDAF7;

          BtnHoverColor := $DCFFFF;
          BtnHoverColorTo := $5BC0F7;

          CaptionColor := $F2F1F0;
          CaptionColorTo := $C9C2BD;

          BorderColor := $5C534C;

          GradientDirection := gdVertical;
        end;
     asOffice2007Silver:
       begin
          WindowColor := $F8F7F6;
          WindowColorTo := $E8E0DB;

          BtnDownColor := $087FE8;
          BtnDownColorTo := $7CDAF7;

          BtnHoverColor := $DCFFFF;
          BtnHoverColorTo := $5BC0F7;

          CaptionColor := $FAEEEB;
          CaptionColorTo := $E2D8D4;

          BorderColor := $74706F;

          GradientDirection := gdVertical;
       end;

     asWindowsXP:
        begin
          WindowColor := clBtnFace;
          WindowColorTo := clBtnFace;

          BtnDownColor := clHighlight;
          BtnDownColorTo := clHighlight;

          BtnHoverColor := clInactiveCaption;
          BtnHoverColorTo := clInactiveCaption;

          CaptionColor := clBlue;
          CaptionColorTo := clHighlight;

          BorderColor := clBlack;
          GradientDirection := gdVertical;
        end;
      asWindowsVista:
        begin
          WindowColor := $FDF8F1;
          WindowColorTo := $FCEFD5;

          BtnDownColor := $FEF9F0;
          BtnDownColorTo := $FDF0D7;

          BtnHoverColor := $FFFDF9;
          BtnHoverColorTo := $FFFAF0;

          CaptionColor := $FBEDD3;
          CaptionColorTo := $FAE9C6;

          BorderColor := $FDDE99;
          GradientDirection := gdVertical;
        end;
      asWindows7:
        begin
          WindowColor := $FCEBDC;
          WindowColorTo := $FCDBC1;

          BtnDownColor := $FCEBDC;
          BtnDownColorTo := $FCDBC1;

          BtnHoverColor := $FDFBFA;
          BtnHoverColorTo := $FDF3EB;

          CaptionColor := $FBEDD3;
          CaptionColorTo := clHighLight;

          BorderColor := $CEA27D;
          GradientDirection := gdVertical;
        end;
      asTerminal:
        begin
          WindowColor := clBtnFace;
          WindowColorTo := clBtnFace;

          BtnDownColor := clHighlight;
          BtnDownColorTo := clHighlight;

          BtnHoverColor := clSilver;
          BtnHoverColorTo := clSilver;

          CaptionColor := clHighLight;
          CaptionColorTo := clHighlight;

          BorderColor := clGray;
          GradientDirection := gdVertical;
        end;
      asOffice2010Blue:
        begin
          WindowColor := $FDF6EF;
          WindowColorTo := $F0DAC7;

          BtnDownColor := $6CD0FF;
          BtnDownColorTo := $6CD0FF;

          BtnHoverColor := $8AE3FD;
          BtnHoverColorTo := $8AE3FD;

          CaptionColor := $EAD3BF;
          CaptionColorTo := $EAD3BF;

          BorderColor := $C7B29F;

          GradientDirection := gdVertical;
        end;
      asOffice2010Silver:
        begin
          WindowColor := $FFFFFF;
          WindowColorTo := $EDE5E0;

          BtnDownColor := $6CD0FF;
          BtnDownColorTo := $6CD0FF;

          BtnHoverColor := $8AE3FD;
          BtnHoverColorTo := $8AE3FD;

          CaptionColor := $D4CFCB;
          CaptionColorTo := $D4CFCB;

          BorderColor := $D2CDC8;

          GradientDirection := gdVertical;
        end;
      asOffice2010Black:
        begin
          WindowColor := $BFBFBF;
          WindowColorTo := $919191;

          BtnDownColor := $6CD0FF;
          BtnDownColorTo := $6CD0FF;

          BtnHoverColor := $8AE3FD;
          BtnHoverColorTo := $8AE3FD;

          CaptionColor := $656565;
          CaptionColorTo := $656565;

          BorderColor := $6D6D6D;

          GradientDirection := gdVertical;
        end;
      asWindows8, asWindows10:
        begin
          WindowColor := clWhite;
          WindowColorTo := clWhite;

          BtnDownColor := $F7E0C9;
          BtnDownColorTo := $F7E0C9;

          BtnHoverColor := $F7EFE8;
          BtnHoverColorTo := $F7EFE8;

          CaptionColor := $F7F6F5;
          CaptionColorTo := $F7F6F5;

          BorderColor := $E4E3E2;

          GradientDirection := gdVertical;
        end;
      asOffice2013White:
        begin
          WindowColor := clWhite;
          WindowColorTo := clWhite;

          BtnDownColor := $FCE2C8;
          BtnDownColorTo := $FCE2C8;

          BtnHoverColor := $FCF0E4;
          BtnHoverColorTo := $FCF0E4;

          CaptionColor := clWhite;
          CaptionColorTo := clWhite;

          BorderColor := $D4D4D4;

          GradientDirection := gdVertical;
        end;
     asOffice2013LightGray:
        begin
          WindowColor := clWhite;
          WindowColorTo := clWhite;

          BtnDownColor := $FCE2C8;
          BtnDownColorTo := $FCE2C8;

          BtnHoverColor := $FCF0E4;
          BtnHoverColorTo := $FCF0E4;

          CaptionColor := $F6F6F6;
          CaptionColorTo := $F6F6F6;

          BorderColor := $C6C6C6;

          GradientDirection := gdVertical;
        end;
     asOffice2013Gray:
        begin
          WindowColor := clWhite;
          WindowColorTo := clWhite;

          BtnDownColor := $FCE2C8;
          BtnDownColorTo := $FCE2C8;

          BtnHoverColor := $FCF0E4;
          BtnHoverColorTo := $FCF0E4;

          CaptionColor := $E5E5E5;
          CaptionColorTo := $E5E5E5;

          BorderColor := $ABABAB;

          GradientDirection := gdVertical;
        end;
     asOffice2016White:
        begin
          WindowColor := clWhite;
          WindowColorTo := clWhite;

          BtnDownColor := $E3BDA3;
          BtnDownColorTo := $E3BDA3;

          BtnHoverColor := $F2E1D5;
          BtnHoverColorTo := $F2E1D5;

          CaptionColor := clWhite;
          CaptionColorTo := clWhite;

          BorderColor := $D4D4D4;

          GradientDirection := gdVertical;
        end;
     asOffice2016Gray:
        begin
          WindowColor := $B2B2B2;
          WindowColorTo := $B2B2B2;

          BtnDownColor := $E3BDA3;
          BtnDownColorTo := $E3BDA3;

          BtnHoverColor := $F2E1D5;
          BtnHoverColorTo := $F2E1D5;

          CaptionColor := $B2B2B2;
          CaptionColorTo := $B2B2B2;

          BorderColor := $444444;

          GradientDirection := gdVertical;
        end;
     asOffice2016Black:
        begin
          WindowColor := $363636;
          WindowColorTo := $363636;
          Font.Color := $A6A6A6;

          BtnDownColor := $444444;
          BtnDownColorTo := $444444;

          BtnHoverColor := $6A6A6A;
          BtnHoverColorTo := $6A6A6A;

          CaptionColor := $363636;
          CaptionColorTo := $363636;

          BorderColor := $444444;

          GradientDirection := gdVertical;
        end;

    end;

    if Assigned(FAlertWindow) then
      if FAlertWindow.Visible then
        ApplyStyle;
  end;
end;

//------------------------------------------------------------------------------

function TAdvAlertWindow.GetActiveMessage: Integer;
begin
  if AlertMessages.Count = 0 then
    Result := -1
  else
    if Assigned(FAlertWindow) then
      Result := FAlertWindow.ActiveMessage
    else
      Result := -1;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetActiveMessage(const Value: Integer);
begin
  if Assigned(FAlertWindow) then
    FAlertWindow.ActiveMessage := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetPositionFormat(const Value: string);
begin
  FPositionFormat := Value;
  if Assigned(FAlertWindow) then
    FAlertWindow.PositionFormat := Value;
end;

//------------------------------------------------------------------------------

function TAdvAlertWindow.GetVisible: Boolean;
begin
  if Assigned(FAlertWindow) and FAlertWindow.Visible then
    Result := True
  else
    Result := False;
end;

//------------------------------------------------------------------------------

function TAdvAlertWindow.GetWindowHandle: THandle;
begin
  if Assigned(FAlertWindow) then
    Result := FAlertWindow.Handle
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

function TAdvAlertWindow.GetLeft: Integer;
begin
  if Assigned(FAlertWindow) then
  begin
    if FAlertWindow.Left <> -1 then
      Result := FAlertWindow.Left
    else
      Result := FLeft;
  end      
  else
    Result := FLeft;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetLeft(const Value: Integer);
begin
  FLeft := Value;
  if Assigned(FAlertWindow) then
    if FAlertWindow.Visible then
      FAlertWindow.Left := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetTop(const Value: Integer);
begin
  FTop := Value;
  if Assigned(FAlertWindow) then
    if FAlertWindow.Visible then
      FAlertWindow.Top := Value;
end;

//------------------------------------------------------------------------------

function TAdvAlertWindow.GetTop: Integer;
begin
  if Assigned(FAlertWindow) then
  begin
    if FAlertWindow.Top <> -1 then
      Result := FAlertWindow.Top
    else
      Result := FTop;
  end
  else
    Result := FTop;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.ThemeAdapt;
var
  eTheme: XPColorScheme;
begin
  eTheme := CurrentXPTheme();
  case eTheme of
    xpBlue: Style := asOffice2003Blue;
    xpGreen: Style := asOffice2003Olive;
    xpGray: Style := asOffice2003Silver;
  else
    Style := asOffice2003Classic;
  end;
end;

//------------------------------------------------------------------------------

function TAdvAlertWindow.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' +
    IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TAdvAlertWindow.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SetVersion(const Value: string);
begin

end;


//------------------------------------------------------------------------------

procedure TAdvAlertWindow.LoadPosition;
var
  myreg: TRegistry;
  rk: string;
begin
  myreg := TRegistry.Create;
  myreg.RootKey := HKEY_CURRENT_USER;

  rk := RegistryKey;
  if rk = '' then
    rk := 'TMS';

  if myreg.OpenKey('\SOFTWARE\' + rk + '\',true) then
    if myreg.OpenKey('ALERT',true) then
    begin
      if myreg.ValueExists('X') then
        PopupLeft := myreg.ReadInteger('X');
      if myreg.ValueExists('Y') then
        PopupTop := myreg.ReadInteger('Y');
    end;
  myreg.Free;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.SavePosition;
var
  myreg: TRegistry;
  rk: string;
begin
  myreg := TRegistry.Create;
  myreg.RootKey := HKEY_CURRENT_USER;

  rk := RegistryKey;
  if rk = '' then
    rk := 'TMS';

  if MyReg.OpenKey('\SOFTWARE\' + rk + '\',true) then
    if MyReg.openkey('ALERT',true) then
    begin
      myreg.WriteInteger('X',PopupLeft);
      myreg.WriteInteger('Y',PopupTop);
    end;
  myreg.Free;
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.CustomPaint(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; var DefaultDraw: boolean);
begin
  if Assigned(OnCustomPaint) then
    OnCustomPaint(Sender, ACanvas, ARect, DefaultDraw);
end;

//------------------------------------------------------------------------------

procedure TAdvAlertWindow.AlertDestroyed(Sender: TObject);
begin
  if PersistPosition then
    SavePosition;
end;



//------------------------------------------------------------------------------
//------------------------{ TMsgCollectionItem }--------------------------------
//------------------------------------------------------------------------------

procedure TMsgCollectionItem.Assign(Source: TPersistent);
begin
  if (Source is TMsgCollectionItem) then
  begin
    Text.Assign((Source as TMsgCollectionItem).Text);
    ImageIndex  := (Source as TMsgCollectionItem).ImageIndex;
    Tag  := (Source as TMsgCollectionItem).Tag;
    Color := (Source as TMsgCollectionItem).Color;
    ColorTo := (Source as TMsgCollectionItem).ColorTo;
  end;
end;

constructor TMsgCollectionItem.Create(Collection: TCollection);
begin
  inherited;
  FText := TStringList.Create;
  FText.OnChange := TextChanged;
  FText.CommaText := '';
  FImageIndex := 0;
  FColor := clNone;
  FColorTo := clNone;
end;

//------------------------------------------------------------------------------

destructor TMsgCollectionItem.Destroy;
begin
  if Assigned(TMsgCollection(Collection).OnDeleteItem) then
    TMsgCollection(Collection).OnDeleteItem(TMsgCollection(Collection), Index);

  FText.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TMsgCollectionItem.SetColor(const Value: TColor);
begin
  FColor := Value;
  if Assigned(TMsgCollection(Collection).OnChange) then
    TMsgCollection(Collection).OnChange(TMsgCollection(Collection));
end;

//------------------------------------------------------------------------------

procedure TMsgCollectionItem.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
  if Assigned(TMsgCollection(Collection).OnChange) then
    TMsgCollection(Collection).OnChange(TMsgCollection(Collection));
end;

//------------------------------------------------------------------------------

procedure TMsgCollectionItem.SetImageIndex(const Value: TImageIndex);
begin
  FImageIndex := Value;
end;

//------------------------------------------------------------------------------

procedure TMsgCollectionItem.SetTag(const Value: integer);
begin
  FTag := Value;
end;

//------------------------------------------------------------------------------

procedure TMsgCollectionItem.SetText(const Value: TStringList);
begin
  FText.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TMsgCollectionItem.TextChanged(Sender: TObject);
begin
  if Assigned(TMsgCollection(Collection).OnChange) then
    TMsgCollection(Collection).OnChange(TMsgCollection(Collection));
end;

//------------------------------------------------------------------------------
//-------------------------{ TMsgCollection }-----------------------------------
//------------------------------------------------------------------------------

function TMsgCollection.Add: TMsgCollectionItem;
begin
  Result := TMsgCollectionItem(inherited Add);
end;

//------------------------------------------------------------------------------


constructor TMsgCollection.Create(AOwner: TComponent);
begin
  inherited Create(GetItemClass);
  FOwner := AOwner;
end;

//------------------------------------------------------------------------------

function TMsgCollection.GetItem(Index: Integer): TMsgCollectionItem;
begin
  Result := TMsgCollectionItem(inherited Items[Index]);
end;

function TMsgCollection.GetItemClass: TCollectionItemClass;
begin
  Result := TMsgCollectionItem;
end;

//------------------------------------------------------------------------------

function TMsgCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

function TMsgCollection.Insert(Index: Integer): TMsgCollectionItem;
begin
  Result := TMsgCollectionItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TMsgCollection.SetItem(Index: Integer;
  const Value: TMsgCollectionItem);
begin
  inherited Items[Index] := Value;
end;

procedure TMsgCollection.Update(Item: TCollectionItem);
begin
  inherited;
  
  if (Item = nil) and (Count = 0) then
  begin
    if Assigned(OnChange) then
      OnChange(self);
  end;

end;

{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}


end.
