{ ************************************************************************* }
{ TAdvSmoothRotaryMenu component                                            }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{ copyright © 2013 - 2015                                                   }
{ Email : info@tmssoftware.com                                              }
{ Website : http://www.tmssoftware.com/                                     }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{ ************************************************************************* }

unit AdvSmoothRotaryMenu;
{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, Graphics, Menus, Messages, Forms, Controls,
  AdvGDIP, AdvStyleIF, Math, GDIPFill, stdctrls,
  GDIPPictureContainer, ImgList, Types, ExtCtrls;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed : Issue with hovered item when releasing mouse outside component.
  // v1.0.0.2 : Fixed : Issue with changing StartAngle at designtime
  // v1.1.0.0 : New : Windows 10, Office 2016 styles added

type
  TAdvSmoothRotaryMenuDialog = class;
  TAdvSmoothRotaryMenu = class;

  TPointArray = array of TGPPointF;

  TAdvSmoothRotaryMenuForm = class(TForm)
  private
    OldWndProc, NewWndProc: Pointer;
    FMouseEntered: Boolean;
    FMainBuffer: TGPBitmap;
    FRotaryMenuDialog: TAdvSmoothRotaryMenuDialog;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMActivate(var Message: TMessage); message WM_ACTIVATE;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DblClick; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CreateWnd; override;
    procedure DoCreate; override;
    procedure DoDestroy; override;
    procedure Paint; override;

    // ---- Paint proc
    procedure Draw(Graphics: TGPGraphics);

    // ---- Paint buffer
    procedure CreateMainBuffer;
    procedure DestroyMainBuffer;
    procedure ClearBuffer(Graphics: TGPGraphics);
    function CreateGraphics: TGPGraphics;

    // ---- Layered window
    procedure SetLayeredWindow;
    procedure UpdateLayered;
    procedure UpdateMainWindow;
    procedure UpdateWindow;
    procedure WndProc(var Message: TMessage); override;
    procedure HookWndProc(var Msg: TMessage);
  public
    procedure Init;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    property RotaryMenuDialog: TAdvSmoothRotaryMenuDialog read FRotaryMenuDialog write FRotaryMenuDialog;
    procedure FormHookInit;
    procedure FormHookDone;
  end;

  TRotaryMenuLocation = (clTopLeft, clTopCenter, clTopRight, clCenterLeft, clCenterCenter, clCenterRight, clBottomLeft, clBottomCenter, clBottomRight, clCustom);

  TRotaryMenuItem = class(TCollectionItem)
  private
    FOwner: TAdvSmoothRotaryMenu;
    FCaption: string;
    FEnabled: Boolean;
    FImage: TAdvGDIPPicture;
    FImageName: String;
    FImageIndex: Integer;
    FTag: Integer;
    FDownFill: TGDIPFill;
    FDisabledFill: TGDIPFill;
    FFill: TGDIPFill;
    FHoverFill: TGDIPFill;
    FImageHeight: Integer;
    FImageWidth: Integer;
    FObject: TObject;
    FDataString: String;
    FDataInteger: Integer;
    FHint: String;
    FToggle: Boolean;
    FDown: Boolean;
    FCaptionTop: Integer;
    FCaptionLeft: Integer;
    FCaptionLocation: TRotaryMenuLocation;
    FImageLocation: TRotaryMenuLocation;
    FImageTop: Integer;
    FImageLeft: Integer;
    FFont: TFont;
    FShortCut: TShortCut;
    FReadOnly: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetEnabled(const Value: Boolean);
    procedure SetImage(const Value: TAdvGDIPPicture);
    procedure SetImageIndex(const Value: Integer);
    procedure SetImageName(const Value: String);
    procedure SetDisabledFill(const Value: TGDIPFill);
    procedure SetDownFill(const Value: TGDIPFill);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetHoverFill(const Value: TGDIPFill);
    procedure SetImageHeight(const Value: Integer);
    procedure SetImageWidth(const Value: Integer);
    procedure SetHint(const Value: String);
    procedure SetToggle(const Value: Boolean);
    procedure SetDown(const Value: Boolean);
    procedure SetCaptionLeft(const Value: Integer);
    procedure SetCaptionLocation(const Value: TRotaryMenuLocation);
    procedure SetCaptionTop(const Value: Integer);
    procedure SetImageLocation(const Value: TRotaryMenuLocation);
    procedure SetImageLeft(const Value: Integer);
    procedure SetImageTop(const Value: Integer);
    procedure SetFont(const Value: TFont);
    procedure SetShortCut(const Value: TShortCut);
    function GetStartAngle: Double;
    function GetStopAngle: Double;
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure ImageChanged(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DataObject: TObject read FObject write FObject;
    property DataString: String read FDataString write FDataString;
    property DataInteger: Integer read FDataInteger write FDataInteger;
    property Down: Boolean read FDown write SetDown;
    function GetMenu: TAdvSmoothRotaryMenu;
    property StartAngle: Double read GetStartAngle;
    property StopAngle: Double read GetStopAngle;
  published
    property Toggle: Boolean read FToggle write SetToggle default False;
    property Fill: TGDIPFill read FFill write SetFill;
    property HoverFill: TGDIPFill read FHoverFill write SetHoverFill;
    property DownFill: TGDIPFill read FDownFill write SetDownFill;
    property DisabledFill: TGDIPFill read FDisabledFill write SetDisabledFill;
    property Caption: string read FCaption write SetCaption;
    property Font: TFont read FFont write SetFont;
    property CaptionLocation: TRotaryMenuLocation read FCaptionLocation write SetCaptionLocation default clCenterCenter;
    property ImageLocation: TRotaryMenuLocation read FImageLocation write SetImageLocation default clCenterCenter;
    property CaptionLeft: Integer read FCaptionLeft write SetCaptionLeft default 0;
    property CaptionTop: Integer read FCaptionTop write SetCaptionTop default 0;
    property ImageLeft: Integer read FImageLeft write SetImageLeft default 0;
    property ImageTop: Integer read FImageTop write SetImageTop default 0;
    property Image: TAdvGDIPPicture read FImage write SetImage;
    property ImageName: String read FImageName write SetImageName;
    property ImageWidth: Integer read FImageWidth write SetImageWidth default -1;
    property ImageHeight: Integer read FImageHeight write SetImageHeight default -1;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property Hint: String read FHint write SetHint;
    property Tag: Integer read FTag write FTag default 0;
    property ShortCut: TShortCut read FShortCut write SetShortCut default 0;
  end;

  TRotaryMenuItems = class(TCollection)
  private
    FOwner: TAdvSmoothRotaryMenu;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TRotaryMenuItem;
    procedure SetItem(Index: Integer; const Value: TRotaryMenuItem);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvSmoothRotaryMenu);
    property Items[Index: Integer]: TRotaryMenuItem read GetItem write SetItem; default;
    function Add: TRotaryMenuItem;
    function Insert(Index: Integer): TRotaryMenuItem;
    procedure Delete(Index: Integer);
    procedure Clear;
  end;


  TRotaryMenuLayout = (rmlCircularDonut, rmlCircularPie, rmlHorizontal, rmlVertical);

  TRotaryMenuPosition = (rmpTopLeft, rmpTopCenter, rmpTopRight,
    rmpCenterLeft, rmpCenterCenter, rmpCenterRight, rmpBottomLeft, rmpBottomCenter,
    rmpBottomRight);

  TRotaryMenuItemEvent = procedure(Sender: TObject; AItemIndex: Integer) of object;
  TRotaryMenuItemHintEvent = procedure(Sender: TObject; AItemIndex: Integer; var AHint: String) of object;
  TRotaryMenuItemMouseEvent = procedure(Sender: TObject; AItemIndex: Integer; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer) of object;
  TRotaryMenuItemMouseMoveEvent = procedure(Sender: TObject; AItemIndex: Integer; Shift: TShiftState;
    X, Y: Integer) of object;

  TRotaryMenuAnimationType = (atInOut, atIn, atOut);
  TRotaryMenuAnimationMode = (amAngle, amSize);
  TRotaryMenuAnimationModes = set of TRotaryMenuAnimationMode;


  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothRotaryMenu = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FUpdateCount: Integer;
    FBlockAnimate: Boolean;
    FSaveStopAngle: Double;
    FSaveW, FSaveH: Double;
    FAnimate, FDoAnimationStop, FDoAnimationStart, FDoAnimationX, FDoAnimationY,
    FDoAnimationW, FDoAnimationH: Boolean;
    FTimer: TTimer;
    FHoveredElement, FFocusedElement, FDownElement: TRotaryMenuItem;
    FShadowColor: TColor;
    FShadowOpacity: Byte;
    FItems: TRotaryMenuItems;
    FPictureContainer: TGDIPPictureContainer;
    FImageList: TCustomImageList;
    FShadowOffset: Integer;
    FOnMenuItemClick: TRotaryMenuItemEvent;
    FStopAngle, FStopAngleTo, FStopAngleTemp: Double;
    FStartAngle, FStartAngleTo, FStartAngleTemp: Double;
    FX, FXTo, FXTemp: Double;
    FY, FYTo, FYTemp: Double;
    FW, FWTo, FWTemp: Double;
    FH, FHTo, FHTemp: Double;
    FSize: Integer;
    FLayout: TRotaryMenuLayout;
    FOnChange: TNotifyEvent;
    FDefaultFill: TGDIPFill;
    FDefaultHoverFill: TGDIPFill;
    FDefaultDownFill: TGDIPFill;
    FDefaultDisabledFill: TGDIPFill;
    FOnMenuItemHint: TRotaryMenuItemHintEvent;
    FDefaultFont: TFont;
    FAnimation: Boolean;
    FAnimationType: TRotaryMenuAnimationType;
    FAnimationFactor: Integer;
    FOnMenuItemMouseLeave: TRotaryMenuItemEvent;
    FMenuItemMouseDown: TRotaryMenuItemMouseEvent;
    FOnMenuItemMouseEnter: TRotaryMenuItemEvent;
    FMenuItemMouseMove: TRotaryMenuItemMouseMoveEvent;
    FMenuItemMouseUp: TRotaryMenuItemMouseEvent;
    FOnMenuItemDblClick: TRotaryMenuItemEvent;
    FShowFocus: Boolean;
    FAnimationMode: TRotaryMenuAnimationModes;
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOpacity(const Value: Byte);
    procedure SetItems(const Value: TRotaryMenuItems);
    procedure SetShadowOffset(const Value: Integer);
    procedure SetStartAngle(const Value: Double);
    procedure SetStopAngle(const Value: Double);
    procedure SetSize(const Value: Integer);
    procedure SetLayout(const Value: TRotaryMenuLayout);
    procedure SetDefaultDisabledFill(const Value: TGDIPFill);
    procedure SetDefaultDownFill(const Value: TGDIPFill);
    procedure SetDefaultFill(const Value: TGDIPFill);
    procedure SetDefaultHoverFill(const Value: TGDIPFill);
    procedure SetDefaultFont(const Value: TFont);
    procedure SetAnimation(const Value: Boolean);
    procedure SetAnimationType(const Value: TRotaryMenuAnimationType);
    procedure SetAnimationFactor(const Value: Integer);
    procedure SetShowFocus(const Value: Boolean);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure SetAnimationMode(const Value: TRotaryMenuAnimationModes);
    procedure SetH(const Value: Double);
    procedure SetW(const Value: Double);
    procedure SetX(const Value: Double);
    procedure SetY(const Value: Double);
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Animate(Sender: TObject);
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function FindNextFocusedElement: TRotaryMenuItem;
    function FindPreviousFocusedElement: TRotaryMenuItem;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure ItemsChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure DoItemClick(Sender: TObject; AIndex: Integer);
    procedure Initialize;
    procedure DblClick; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Loaded; override;
    procedure Resize; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsFocused: Boolean;
    function GetMenuRect: TGPRectF;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    procedure DrawItems(g: TGPGraphics; sta, stp: Double; R, RI: TGPRectF);
    procedure GetMenuPath(gp: TGPGraphicsPath; sta, stp, b: Double; R, RI: TGPRectF; I: Integer);
    procedure DrawRotaryMenu(g: TGPGraphics; R: TGPRectF);
    procedure DrawCaption(g: TGPGraphics; AItem: TRotaryMenuItem; R: TGPRectF);
    procedure DrawImage(g: TGPGraphics; AItem: TRotaryMenuItem; R: TGPRectF);
    function ItemAtXY(pX, pY: Integer): TRotaryMenuItem;
    procedure SetDefaultStyle;
    procedure Paint; override;
    procedure Assign(Source: TPersistent); override;
    function IsAnimating: Boolean;
    function NextStopAngle: Double;
    function NextStartAngle: Double;
    function NextH: Double;
    function NextW: Double;
    function NextX: Double;
    function NextY: Double;
    property CurrentStopAngle: Double read FStopAngle write FStopAngle;
    property CurrentStartAngle: Double read FStartAngle write FStartAngle;
    procedure StartInAnimation;
    procedure StartOutAnimation;

    property X: Double read FX write SetX;
    property Y: Double read FY write SetY;
    property W: Double read FW write SetW;
    property H: Double read FH write SetH;

  published
    property Animation: Boolean read FAnimation write SetAnimation default True;
    property AnimationFactor: Integer read FAnimationFactor write SetAnimationFactor default 4;
    property AnimationType: TRotaryMenuAnimationType read FAnimationType write SetAnimationType default atInOut;
    property AnimationMode: TRotaryMenuAnimationModes read FAnimationMode write SetAnimationMode default [amAngle];
    property ShadowColor: TColor read FShadowColor write SetShadowColor default $2D1606;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset default 4;
    property ShadowOpacity: Byte read FShadowOpacity write SetShadowOpacity default 100;
    property StartAngle: Double read FStartAngle write SetStartAngle;
    property StopAngle: Double read FStopAngle write SetStopAngle;
    property Size: Integer read FSize write SetSize default 50;
    property Items: TRotaryMenuItems read FItems write SetItems;
    property PictureContainer: TGDIPPictureContainer read FPictureContainer write FPictureContainer;
    property ImageList: TCustomImageList read FImageList write FImageList;
    property Layout: TRotaryMenuLayout read FLayout write SetLayout default rmlCircularDonut;
    property DefaultFont: TFont read FDefaultFont write SetDefaultFont;
    property DefaultFill: TGDIPFill read FDefaultFill write SetDefaultFill;
    property DefaultHoverFill: TGDIPFill read FDefaultHoverFill write SetDefaultHoverFill;
    property DefaultDownFill: TGDIPFill read FDefaultDownFill write SetDefaultDownFill;
    property DefaultDisabledFill: TGDIPFill read FDefaultDisabledFill write SetDefaultDisabledFill;

    property OnMenuItemHint: TRotaryMenuItemHintEvent read FOnMenuItemHint write FOnMenuItemHint;
    property OnMenuItemClick: TRotaryMenuItemEvent read FOnMenuItemClick write FOnMenuItemClick;
    property OnMenuItemMouseDown: TRotaryMenuItemMouseEvent read FMenuItemMouseDown write FMenuItemMouseDown;
    property OnMenuItemMouseUp: TRotaryMenuItemMouseEvent read FMenuItemMouseUp write FMenuItemMouseUp;
    property OnMenuItemMouseMove: TRotaryMenuItemMouseMoveEvent read FMenuItemMouseMove write FMenuItemMouseMove;
    property OnMenuItemDblClick: TRotaryMenuItemEvent read FOnMenuItemDblClick write FOnMenuItemDblClick;
    property OnMenuItemMouseLeave: TRotaryMenuItemEvent read FOnMenuItemMouseLeave write FOnMenuItemMouseLeave;
    property OnMenuItemMouseEnter: TRotaryMenuItemEvent read FOnMenuItemMouseEnter write FOnMenuItemMouseEnter;

    property ShowFocus: Boolean read FShowFocus write SetShowFocus default True;

    property Align;
    property Anchors;
    property Constraints;
    property PopupMenu;
    property ParentShowHint;
    property ShowHint;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseDown;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnResize;
    property OnDblClick;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property Visible;
    property Hint;
    property TabStop default true;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothRotaryMenuDialog = class(TComponent, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FPosition: TRotaryMenuPosition;
    FWidth: Integer;
    FHeight: Integer;
    frm: TAdvSmoothRotaryMenuForm;
    FClosing: Boolean;
    FHideFromWindows: Boolean;
    FCloseOnDeactivate: Boolean;
    FOnShow: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnBeforeClose: TNotifyEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FFormStyle: TFormStyle;
    FVisible: Boolean;
    FMenu: TAdvSmoothRotaryMenu;
    FOnMenuItemClick: TRotaryMenuItemEvent;
    FOnMenuItemHint: TRotaryMenuItemHintEvent;
    FActiveFocus: Boolean;
    FCloseOnMouseLeave: Boolean;
    FMouseEntered: Boolean;
    FOnMenuItemMouseLeave: TRotaryMenuItemEvent;
    FMenuItemMouseDown: TRotaryMenuItemMouseEvent;
    FOnMenuItemMouseEnter: TRotaryMenuItemEvent;
    FMenuItemMouseMove: TRotaryMenuItemMouseMoveEvent;
    FMenuItemMouseUp: TRotaryMenuItemMouseEvent;
    FOnMenuItemDblClick: TRotaryMenuItemEvent;
    procedure SetCloseOnDeactivate(const Value: Boolean);
    procedure SetFormStyle(const Value: TFormStyle);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetPosition(const Value: TRotaryMenuPosition);
    procedure SetActiveFocus(const Value: Boolean);
    procedure SetCloseOnMouseLeave(const Value: Boolean);
    function GetAnimation: Boolean;
    function GetAnimationFactor: Integer;
    function GetAnimationType: TRotaryMenuAnimationType;
    function GetDefaultDisabledFill: TGDIPFill;
    function GetDefaultDownFill: TGDIPFill;
    function GetDefaultFill: TGDIPFill;
    function GetDefaultFont: TFont;
    function GetDefaultHoverFill: TGDIPFill;
    function GetImageList: TCustomImageList;
    function GetItems: TRotaryMenuItems;
    function GetLayout: TRotaryMenuLayout;
    function GetShadowColor: TColor;
    function GetShadowOffset: Integer;
    function GetShadowOpacity: Byte;
    function GetSize: Integer;
    function GetStartAngle: Double;
    function GetStopAngle: Double;
    procedure SetAnimation(const Value: Boolean);
    procedure SetAnimationFactor(const Value: Integer);
    procedure SetAnimationType(const Value: TRotaryMenuAnimationType);
    procedure SetDefaultDisabledFill(const Value: TGDIPFill);
    procedure SetDefaultDownFill(const Value: TGDIPFill);
    procedure SetDefaultFill(const Value: TGDIPFill);
    procedure SetDefaultFont(const Value: TFont);
    procedure SetDefaultHoverFill(const Value: TGDIPFill);
    procedure SetItems(const Value: TRotaryMenuItems);
    procedure SetLayout(const Value: TRotaryMenuLayout);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: Integer);
    procedure SetShadowOpacity(const Value: Byte);
    procedure SetSize(const Value: Integer);
    procedure SetStartAngle(const Value: Double);
    procedure SetStopAngle(const Value: Double);
    function GetShowFocus: Boolean;
    procedure SetShowFocus(const Value: Boolean);
    function GetPictureContainer: TGDIPPictureContainer;
    procedure SetImageList(const Value: TCustomImageList);
    procedure SetPictureContainer(const Value: TGDIPPictureContainer);
    function GetAnimationMode: TRotaryMenuAnimationModes;
    procedure SetAnimationMode(const Value: TRotaryMenuAnimationModes);
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure MenuChanged(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Changed;
    procedure DoMenuItemClick(Sender: TObject; AItemIndex: Integer);
    procedure DoMenuItemMouseLeave(Sender: TObject; AItemIndex: Integer);
    procedure DoMenuItemMouseEnter(Sender: TObject; AItemIndex: Integer);
    procedure DoMenuItemHint(Sender: TObject; AItemIndex: Integer; var AHint: String);
    procedure DoMenuItemDblClick(Sender: TObject; AItemIndex: Integer);
    procedure DoMenuItemMouseUp(Sender: TObject; AItemIndex: Integer; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoMenuItemMouseDown(Sender: TObject; AItemIndex: Integer; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoMenuItemMouseMove(Sender: TObject; AItemIndex: Integer; Shift: TShiftState;
      X, Y: Integer);
  public
    property MouseEntered: Boolean read FMouseEntered write FMouseEntered;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    property HideFromWindows: Boolean read FHideFromWindows write FHideFromWindows default False;
    procedure DoPopupMenu(X, Y: Integer; UsedControl: Boolean);
    procedure PopupMenuAt(X, Y: Integer);
    procedure PopupMenuAtControl(Control: TControl);
    procedure ClosePopupMenu;
    property Visible: Boolean read FVisible;
    property Menu: TAdvSmoothRotaryMenu read FMenu write FMenu;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Form: TAdvSmoothRotaryMenuForm read frm write frm;
  published
    property FormStyle: TFormStyle read FFormStyle write SetFormStyle default fsStayOnTop;
    property CloseOnDeactivate: Boolean read FCloseOnDeactivate write SetCloseOnDeactivate default True;
    property CloseOnMouseLeave: Boolean read FCloseOnMouseLeave write SetCloseOnMouseLeave default False;
    property ActiveFocus: Boolean read FActiveFocus write SetActiveFocus default True;
    property Height: Integer read FHeight write SetHeight default 300;
    property Width: Integer read FWidth write SetWidth default 300;
    property Position: TRotaryMenuPosition read FPosition write SetPosition default rmpCenterCenter;

    property Animation: Boolean read GetAnimation write SetAnimation default True;
    property AnimationFactor: Integer read GetAnimationFactor write SetAnimationFactor default 4;
    property AnimationType: TRotaryMenuAnimationType read GetAnimationType write SetAnimationType default atInOut;
    property AnimationMode: TRotaryMenuAnimationModes read GetAnimationMode write SetAnimationMode default [amAngle];
    property ShadowColor: TColor read GetShadowColor write SetShadowColor default $2D1606;
    property ShadowOffset: Integer read GetShadowOffset write SetShadowOffset default 4;
    property ShadowOpacity: Byte read GetShadowOpacity write SetShadowOpacity default 100;
    property StartAngle: Double read GetStartAngle write SetStartAngle;
    property StopAngle: Double read GetStopAngle write SetStopAngle;
    property Size: Integer read GetSize write SetSize default 50;
    property Items: TRotaryMenuItems read GetItems write SetItems;
    property Layout: TRotaryMenuLayout read GetLayout write SetLayout default rmlCircularDonut;
    property PictureContainer: TGDIPPictureContainer read GetPictureContainer write SetPictureContainer;
    property ImageList: TCustomImageList read GetImageList write SetImageList;
    property DefaultFont: TFont read GetDefaultFont write SetDefaultFont;
    property DefaultFill: TGDIPFill read GetDefaultFill write SetDefaultFill;
    property DefaultHoverFill: TGDIPFill read GetDefaultHoverFill write SetDefaultHoverFill;
    property DefaultDownFill: TGDIPFill read GetDefaultDownFill write SetDefaultDownFill;
    property DefaultDisabledFill: TGDIPFill read GetDefaultDisabledFill write SetDefaultDisabledFill;

    property OnMenuItemHint: TRotaryMenuItemHintEvent read FOnMenuItemHint write FOnMenuItemHint;
    property OnMenuItemClick: TRotaryMenuItemEvent read FOnMenuItemClick write FOnMenuItemClick;
    property OnMenuItemMouseDown: TRotaryMenuItemMouseEvent read FMenuItemMouseDown write FMenuItemMouseDown;
    property OnMenuItemMouseUp: TRotaryMenuItemMouseEvent read FMenuItemMouseUp write FMenuItemMouseUp;
    property OnMenuItemMouseMove: TRotaryMenuItemMouseMoveEvent read FMenuItemMouseMove write FMenuItemMouseMove;
    property OnMenuItemDblClick: TRotaryMenuItemEvent read FOnMenuItemDblClick write FOnMenuItemDblClick;
    property OnMenuItemMouseLeave: TRotaryMenuItemEvent read FOnMenuItemMouseLeave write FOnMenuItemMouseLeave;
    property OnMenuItemMouseEnter: TRotaryMenuItemEvent read FOnMenuItemMouseEnter write FOnMenuItemMouseEnter;

    property ShowFocus: Boolean read GetShowFocus write SetShowFocus default True;

    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnBeforeClose: TNotifyEvent read FOnBeforeClose write FOnBeforeClose;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
  end;

implementation

uses
  SysUtils;

type
  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}


procedure GetObjectLocation(var x, y: Single; rectangle: TGPRectF; objectwidth, objectheight: Single; location: TRotaryMenuLocation);
var
  w, h, tw, th: Single;
begin
  tw := objectwidth;
  th := objectheight;
  w := rectangle.Width;
  h := rectangle.Height;
  case location of
    clTopLeft:
    begin
      x := 0;
      y := 0;
    end;
    clTopRight:
    begin
      x := w - tw;
      y := 0;
    end;
    clBottomLeft:
    begin
      x := 0;
      y := h - th;
    end;
    clBottomRight:
    begin
      x := w - tw;
      y := h - th;
    end;
    clTopCenter:
    begin
      x := (w - tw) / 2;
      y := 0;
    end;
    clBottomCenter:
    begin
      x := (w - tw) / 2;
      y := h - th;
    end;
    clCenterCenter:
    begin
      x := (w - tw) / 2;
      y := (h - th) / 2;
    end;
    clCenterLeft:
    begin
      x := 0;
      y := (h - th) / 2;
    end;
    clCenterRight:
    begin
      x := w - tw;
      y := (h - th) / 2;
    end;
  end;

  x := x + rectangle.X;
  y := y + rectangle.Y;
end;

procedure GetAspectSize(var w, h: integer; ow, oh, nw, nh: double);
begin
  if (ow > 0) and (oh > 0) and (nw > 0) and (nh > 0) then
  begin
    if ow / oh < nw / nh then
    begin
      h := Round(nh);
      w := MulDiv(Round(nh), round(ow), round(oh));
    end
    else
    begin
      w := Round(nw);
      h := MulDiv(Round(nw), round(oh), round(ow));
    end;
  end;
end;

{ TAdvSmoothRotaryMenu }

procedure TAdvSmoothRotaryMenu.Initialize;
var
  I: Integer;
begin
  SetDefaultStyle;
  for I := 0 to 4 do
    Items.Add;
end;

function TAdvSmoothRotaryMenu.IsAnimating: Boolean;
begin
  Result := FAnimate;
end;

function TAdvSmoothRotaryMenu.IsFocused: Boolean;
begin
  if Owner is TAdvSmoothRotaryMenuDialog then
    Result := True
  else
    Result := Focused;
end;

function PtInPoly(const Points: TPointArray; X,
  Y: Integer): Boolean;
var
  Count, K, J : Integer;
begin
  Result := False;
  Count := Length(Points);
  J := Count-1;
  for K := 0 to Count-1 do begin
   if ((Points[K].Y <=Y) and (Y < Points[J].Y)) or
      ((Points[J].Y <=Y) and (Y < Points[K].Y)) then
   begin
    if (x < (Points[j].X - Points[K].X) *
       (y - Points[K].Y) /
       (Points[j].Y - Points[K].Y) + Points[K].X) then
        Result := not Result;
    end;
    J := K;
  end;
end;

function TAdvSmoothRotaryMenu.ItemAtXY(pX, pY: Integer): TRotaryMenuItem;
var
  I: Integer;
  b: Single;
  pth: TGPGraphicsPath;
  it: TRotaryMenuItem;
  stp: Double;
  r: TGPRectF;
  sta: Double;
  sz: Integer;
  ri: TGPRectF;
  pts: TPointArray;
  cnt: Integer;
begin
  Result := nil;
  if Items.Count = 0 then
    Exit;

  sta := StartAngle;
  stp := StopAngle - StartAngle;

  R := GetMenuRect;
  sz := Size;
  ri := MakeRect(r.X + sz, r.Y + sz, r.Width - sz * 2, r.Height - sz * 2);

  b := 0;
  case Layout of
    rmlCircularDonut, rmlCircularPie: b := stp / Items.Count;
    rmlHorizontal: b := R.Width / Items.Count;
    rmlVertical: b := R.Height / Items.Count;
  end;

  pth := TGPGraphicsPath.Create;
  for I := 0 to Items.Count - 1 do
  begin
    it := Items[I];
    if it.Enabled and not it.ReadOnly then
    begin
      pth.Reset;
      GetMenuPath(pth, sta, stp, b, R, RI, it.Index);
      pth.CloseFigure;

      cnt := pth.GetPointCount;
      SetLength(pts, cnt);
      pth.GetPathPoints(@pts[0], cnt);
      if PtInPoly(pts, pX, pY) then
      begin
        Result := it;
        Break;
      end;
    end;
  end;
  pth.free;
end;

procedure TAdvSmoothRotaryMenu.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothRotaryMenu.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothRotaryMenu.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TAdvSmoothRotaryMenu.FillChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothRotaryMenu.FindNextFocusedElement: TRotaryMenuItem;
var
  I: Integer;
begin
  Result := FFocusedElement;
  if Items.Count = 0 then
    Exit;

  if Assigned(FFocusedElement) then
  begin
    for I := FFocusedElement.Index + 1 to Items.Count - 1 do
    begin
      if Items[I].Enabled then
      begin
        Result := Items[I];
        Break;
      end;
    end;
  end
  else
    FFocusedElement := Items[0];
end;

function TAdvSmoothRotaryMenu.FindPreviousFocusedElement: TRotaryMenuItem;
var
  I: Integer;
begin
  Result := FFocusedElement;
  if Items.Count = 0 then
    Exit;

  if Assigned(FFocusedElement) then
  begin
    for I := FFocusedElement.Index - 1 downto 0 do
    begin
      if Items[I].Enabled then
      begin
        Result := Items[I];
        Break;
      end;
    end;
  end
  else
    Result := Items[Items.Count - 1];
end;

procedure TAdvSmoothRotaryMenu.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothRotaryMenuDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(frm) then
    frm.FormHookDone;
  Action := caFree;
end;

procedure TAdvSmoothRotaryMenuDialog.FormDeactivate(Sender: TObject);
begin
  if CloseOnDeactivate then
    ClosePopupMenu;
end;

function TAdvSmoothRotaryMenuDialog.GetAnimation: Boolean;
begin
  Result := FMenu.Animation;
end;

function TAdvSmoothRotaryMenuDialog.GetAnimationFactor: Integer;
begin
  Result := FMenu.AnimationFactor;
end;

function TAdvSmoothRotaryMenuDialog.GetAnimationMode: TRotaryMenuAnimationModes;
begin
  Result := FMenu.AnimationMode;
end;

function TAdvSmoothRotaryMenuDialog.GetAnimationType: TRotaryMenuAnimationType;
begin
  Result := FMenu.AnimationType;
end;

function TAdvSmoothRotaryMenuDialog.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothRotaryMenuDialog.GetDefaultDisabledFill: TGDIPFill;
begin
  Result := FMenu.DefaultDisabledFill;
end;

function TAdvSmoothRotaryMenuDialog.GetDefaultDownFill: TGDIPFill;
begin
  Result := FMenu.DefaultDownFill;
end;

function TAdvSmoothRotaryMenuDialog.GetDefaultFill: TGDIPFill;
begin
  Result := FMenu.DefaultFill;
end;

function TAdvSmoothRotaryMenuDialog.GetDefaultFont: TFont;
begin
  Result := FMenu.DefaultFont;
end;

function TAdvSmoothRotaryMenuDialog.GetDefaultHoverFill: TGDIPFill;
begin
  Result := FMenu.DefaultHoverFill;
end;

function TAdvSmoothRotaryMenuDialog.GetImageList: TCustomImageList;
begin
  Result := FMenu.ImageList;
end;

function TAdvSmoothRotaryMenuDialog.GetItems: TRotaryMenuItems;
begin
  Result := FMenu.Items
end;

function TAdvSmoothRotaryMenuDialog.GetLayout: TRotaryMenuLayout;
begin
  Result := FMenu.Layout;
end;

function TAdvSmoothRotaryMenuDialog.GetPictureContainer: TGDIPPictureContainer;
begin
  Result := FMenu.PictureContainer;
end;

function TAdvSmoothRotaryMenuDialog.GetShadowColor: TColor;
begin
  Result := FMenu.ShadowColor;
end;

function TAdvSmoothRotaryMenuDialog.GetShadowOffset: Integer;
begin
  Result := FMenu.ShadowOffset;
end;

function TAdvSmoothRotaryMenuDialog.GetShadowOpacity: Byte;
begin
  Result := FMenu.ShadowOpacity;
end;

function TAdvSmoothRotaryMenuDialog.GetShowFocus: Boolean;
begin
  Result := FMenu.ShowFocus;
end;

function TAdvSmoothRotaryMenuDialog.GetSize: Integer;
begin
  Result := FMenu.Size;
end;

function TAdvSmoothRotaryMenuDialog.GetStartAngle: Double;
begin
  Result := FMenu.StartAngle;
end;

function TAdvSmoothRotaryMenuDialog.GetStopAngle: Double;
begin
  Result := FMenu.StopAngle;
end;

procedure TAdvSmoothRotaryMenuDialog.MenuChanged(Sender: TObject);
begin
  if Assigned(frm) then
    frm.Invalidate;
end;

function TAdvSmoothRotaryMenu.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

procedure TAdvSmoothRotaryMenu.GetMenuPath(gp: TGPGraphicsPath; sta,
  stp, b: Double; R, RI: TGPRectF; I: Integer);
var
  stai, stpi: Double;
begin

  stai := sta + stp;
  stpi := -stp;

  if I = -1 then
  begin
    case Layout of
      rmlCircularPie: gp.AddPie(R, sta, stp);
      rmlCircularDonut:
      begin
        gp.AddArc(R, sta, stp);
        gp.AddArc(ri, stai, stpi);
      end;
      rmlVertical, rmlHorizontal: gp.AddRectangle(R);
    end;
  end
  else
  begin
    case Layout of
      rmlCircularPie: gp.AddPie(R, sta + (I * b), b);
      rmlCircularDonut:
      begin
        if (RI.Width < 0) or (RI.Height < 0) then
        begin
          gp.AddPie(R, sta + (I * b), b)
        end
        else
        begin
          gp.AddArc(R, sta + (I * b), b);
          gp.AddArc(Ri, sta + stp - (Items.Count - 1 - I) * b, -b)
        end;
      end;
      rmlHorizontal: gp.AddRectangle(MakeRect(R.X + (I * b), R.Y, b, r.Height));
      rmlVertical: gp.AddRectangle(MakeRect(R.X, R.Y + (I * b), r.Width, b));
    end;
  end;
end;

function TAdvSmoothRotaryMenu.GetMenuRect: TGPRectF;
begin
  Result := MakeRect((Width - FW) / 2 + ShadowOffset, (Height - FH) / 2 + ShadowOffset, FW - ShadowOffset * 2 - 1, FH - ShadowOffset * 2 - 1);
end;

procedure TAdvSmoothRotaryMenu.ItemsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothRotaryMenu.KeyDown(var Key: Word; Shift: TShiftState);
var
  I: Integer;
begin
  inherited;

  if Items.Count = 0 then
    Exit;

  case Key of
    VK_DOWN,VK_RIGHT: FFocusedElement := FindNextFocusedElement;
    VK_LEFT, VK_UP: FFocusedElement := FindPreviousFocusedElement;
    VK_HOME: FFocusedElement := Items[0];
    VK_END: FFocusedElement := Items[Items.Count - 1];
    VK_SPACE, VK_RETURN: FDownElement := FFocusedElement;
  end;

  for I := 0 to Items.Count - 1 do
  begin
    if ShortCut(Key, Shift) = Items[I].ShortCut then
    begin
      FDownElement := Items[I];
      Break;
    end;
  end;

  Changed;
end;

procedure TAdvSmoothRotaryMenu.KeyUp(var Key: Word; Shift: TShiftState);
var
  I: Integer;
begin
  inherited;
  if Assigned(FFocusedElement) and ((Key = VK_SPACE) or (Key = VK_RETURN)) then
  begin
    FDownElement := nil;
    if FFocusedElement.Toggle then
      FFocusedElement.Down := not FFocusedElement.Down;

    DoItemClick(Self, FFocusedElement.Index);
  end;

  for I := 0 to Items.Count - 1 do
  begin
    if ShortCut(Key, Shift) = Items[I].ShortCut then
    begin
      FDownElement := nil;
      if Items[I].Toggle then
        Items[I].Down := not Items[I].Down;

      DoItemClick(Self, I);
      Break;
    end;
  end;

  Changed;
end;

procedure TAdvSmoothRotaryMenu.Loaded;
begin
  inherited;
  if Items.Count > 0 then
    FFocusedElement := Items[0];
end;

function AnimateDouble(var Start: Double; Stop: Double; Delta: Double; Margin: Double): Boolean;
begin
  Result := true;
  if (Start > Stop - Margin) and (Start < Stop + Margin) then
  begin
    Start := Stop;
    Result := false;
  end
  else
  begin
    Delta := Max(1, Delta);
    if Start < Stop then
      Start := Round(Start + Delta)
    else
      Start := Round(Start - Delta);
  end;
end;

procedure TAdvSmoothRotaryMenu.Animate(Sender: TObject);
var
  d, ds, dx, dy, dw, dh: Double;
  pos, poss, posx, posy, posw, posh: Double;
const
  Margin: Double = 1;
begin
  if (Animation and FAnimate) or (Abs(NextStopAngle - FStopAngleTemp) > 0) or (Abs(NextStartAngle - FStartAngleTemp) > 0)
  or (Abs(NextH - FHTemp) > 0) or (Abs(NextW - FWTemp) > 0) or (Abs(NextX - FXTemp) > 0)
  or (Abs(NextY - FYTemp) > 0) then
  begin
    d := Abs(NextStopAngle - FStopAngleTemp) / Max(1, AnimationFactor);
    ds := Abs(NextStartAngle - FStartAngleTemp) / Max(1, AnimationFactor);
    dx := Abs(NextX - FXTemp)  / Max(1, AnimationFactor);
    dy := Abs(Nexty - FYTemp)  / Max(1, AnimationFactor);
    dw := Abs(NextW - FWTemp)  / Max(1, AnimationFactor);
    dh := Abs(NextH - FHTemp)  / Max(1, AnimationFactor);
    pos := FStopAngleTemp;
    poss := FStartAngleTemp;
    posx := FXTemp;
    posy := FYTemp;
    posw := FWTemp;
    posh := FHtemp;
    FDoAnimationStop := AnimateDouble(pos, FStopAngleTo, d, Margin);
    FDoAnimationStart := AnimateDouble(poss, FStartAngleTo, ds, Margin);
    FDoAnimationX := AnimateDouble(posx, FXTo, dx, Margin);
    FDoAnimationY := AnimateDouble(posY, FYTo, dy, Margin);
    FDoAnimationW := AnimateDouble(posW, FWTo, dw, Margin);
    FDoAnimationH := AnimateDouble(posH, FHTo, dh, Margin);
    if FDoAnimationStop or FDoAnimationStart or FDoAnimationX or FDoAnimationW or FDoAnimationH or FDoAnimationY then
    begin
      FStopAngle := pos;
      FStopAngleTemp := pos;
      FStartAngle := poss;
      FStartAngleTemp := poss;

      FX := posx;
      FXTemp := posx;
      FY := posy;
      FYTemp := posy;
      FW := posw;
      FWTemp := posw;
      FH := posh;
      FHTemp := posh;

      Changed;
    end
    else
    begin
      FAnimate := False;
      FStopAngle := NextStopAngle;
      FStopAngleTemp := NextStopAngle;
      FStartAngle := NextStartAngle;
      FStartAngleTemp := NextStartAngle;
      FX := NextX;
      FXTemp := NextX;
      FY := NextY;
      FYTemp := NextY;
      FW := NextW;
      FWTemp := NextW;
      FH := NextH;
      FHTemp := NextH;
      Changed;
    end;
  end;
end;

procedure TAdvSmoothRotaryMenu.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothRotaryMenu then
  begin
    FShowFocus := (Source as TAdvSmoothRotaryMenu).ShowFocus;
    FAnimationFactor := (Source as TAdvSmoothRotaryMenu).AnimationFactor;
    FAnimation := (Source as TAdvSmoothRotaryMenu).Animation;
    FAnimationType := (Source as TAdvSmoothRotaryMenu).AnimationType;
    FDefaultFont.Assign((Source as TAdvSmoothRotaryMenu).DefaultFont);
    FShadowColor := (Source as TAdvSmoothRotaryMenu).ShadowColor;
    FShadowOffset := (Source as TAdvSmoothRotaryMenu).ShadowOffset;
    FShadowOpacity := (Source as TAdvSmoothRotaryMenu).ShadowOpacity;
    FStartAngle := (Source as TAdvSmoothRotaryMenu).StartAngle;
    FStopAngle := (Source as TAdvSmoothRotaryMenu).StopAngle;
    FSize := (Source as TAdvSmoothRotaryMenu).Size;
    FItems.Assign((Source as TAdvSmoothRotaryMenu).Items);
    FLayout := (Source as TAdvSmoothRotaryMenu).Layout;
    FDefaultFill.Assign((Source as TAdvSmoothRotaryMenu).DefaultFill);
    FDefaultHoverFill.Assign((Source as TAdvSmoothRotaryMenu).DefaultHoverFill);
    FDefaultDownFill.Assign((Source as TAdvSmoothRotaryMenu).DefaultDownFill);
    FDefaultDisabledFill.Assign((Source as TAdvSmoothRotaryMenu).DefaultDisabledFill);
  end;
end;

procedure TAdvSmoothRotaryMenu.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TAdvSmoothRotaryMenu.Changed;
begin
  if csDestroying in ComponentState then
    Exit;

  if Assigned(OnChange) then
    OnChange(Self);

  Invalidate;
end;

procedure TAdvSmoothRotaryMenu.CMHintShow(var Message: TMessage);
var
  hint: String;
begin
  with TCMHintShow(Message).HintInfo^ do
  begin
    if Assigned(FHoveredElement) then
    begin
      hint := FHoveredElement.Hint;
      if Assigned(OnMenuItemHint) then
        OnMenuItemHint(Self, FHoveredElement.Index, hint);
      HintStr := hint;
      ReshowTimeout := 0;
    end;
  end;
end;

procedure TAdvSmoothRotaryMenu.CMMouseEnter(var Message: TMessage);
begin
end;

procedure TAdvSmoothRotaryMenu.CMMouseLeave(var Message: TMessage);
begin
  if not (csDestroying in ComponentState) then
  begin
    FHoveredElement := nil;
    FDownElement := nil;
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenuDialog.BeginUpdate;
begin
  Menu.BeginUpdate;
end;

procedure TAdvSmoothRotaryMenuDialog.Changed;
begin
  if csDestroying in ComponentState then
    Exit;

  if Assigned(frm) then
  begin
    frm.UpdateWindow;
  end;
end;

procedure TAdvSmoothRotaryMenuDialog.ClosePopupMenu;
var
  CanClose: Boolean;
begin
  if FClosing or not Visible then
    Exit;

  CanClose := True;
  if Assigned(OnCloseQuery) then
    OnCloseQuery(Self, CanClose);

  if CanClose then
  begin
    if Assigned(OnBeforeClose) then
      OnBeforeClose(Self);

    FClosing := True;

   Menu.StartOutAnimation;
   while Menu.IsAnimating do
   begin
     Sleep(1);
     Application.ProcessMessages;
   end;

    if Assigned(frm) then
    begin
      if frm.Visible then
        frm.SetFocus;
      frm.Close;
      frm := nil;
    end;

    if Assigned(OnClose) then
      OnClose(Self);

    FVisible := False;
  end;
end;

constructor TAdvSmoothRotaryMenuDialog.Create(AOwner: TComponent);
begin
  inherited;
  FMenu := TAdvSmoothRotaryMenu.Create(Self);
  FMenu.OnChange := MenuChanged;
  FMenu.OnMenuItemClick := DoMenuItemClick;
  FMenu.OnMenuItemHint := DoMenuItemHint;
  FMenu.OnMenuItemMouseDown := DoMenuItemMouseDown;
  FMenu.OnMenuItemMouseUp := DoMenuItemMouseUp;
  FMenu.OnMenuItemMouseMove := DoMenuItemMouseMove;
  FMenu.OnMenuItemMouseLeave := DoMenuItemMouseLeave;
  FMenu.OnMenuItemMouseEnter := DoMenuItemMouseEnter;
  FMenu.OnMenuItemDblClick := DoMenuItemDblClick;
  FHideFromWindows := False;
  FFormStyle := fsStayOnTop;
  FCloseOnDeactivate := true;
  FWidth := 250;
  FHeight := 250;
  FActiveFocus := True;
  FPosition := rmpCenterCenter;
end;

constructor TAdvSmoothRotaryMenu.Create(AOwner: TComponent);
begin
  inherited;
  FSaveStopAngle := -1;
  FSaveW := -1;
  FSaveH := -1;
  FAnimationMode := [amAngle];
  TabStop := True;
  FShowFocus := True;
  DoubleBuffered := True;
  Width := 250;
  Height := 250;
  FW := Width;
  FH := Height;
  FWTo := FW;
  FWTemp := FW;
  FHTo := FH;
  FHTemp := FH;
  FX := 0;
  FY := 0;
  FXTo := FX;
  FXTemp := FX;
  FYTo := FY;
  FYTemp := FY;
  FLayout := rmlCircularDonut;
  FAnimationFactor := 4;
  FStartAngle := 100;
  FStartAngleTo := FStartAngle;
  FStartAngleTemp := FStartAngle;
  FStopAngle := 360;
  FStopAngleTo := FStopAngle;
  FStopAngleTemp := FStopAngle;

  FSize := 50;
  FShadowColor := $2D1606;
  FShadowOpacity := 100;
  FShadowOffset := 4;

  FAnimation := True;
  FAnimationType := atInOut;

  FDefaultFont := TFont.Create;
  FDefaultFont.Name := 'Tahoma';
  FDefaultFont.OnChange := FontChanged;

  FItems := TRotaryMenuItems.Create(Self);
  FItems.OnChange := ItemsChanged;

  FDefaultFill := TGDIPFill.Create;
  FDefaultFill.OnChange := FillChanged;

  FDefaultDownFill := TGDIPFill.Create;
  FDefaultDownFill.OnChange := FillChanged;

  FDefaultHoverFill := TGDIPFill.Create;
  FDefaultHoverFill.OnChange := FillChanged;

  FDefaultDisabledFill := TGDIPFill.Create;
  FDefaultDisabledFill.OnChange := FillChanged;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 1;
  FTimer.OnTimer := Animate;
  FTimer.Enabled := true;

  if (csDesigning in Owner.ComponentState) and not ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState)) then
    Initialize;
end;

procedure TAdvSmoothRotaryMenu.DblClick;
var
  it: TRotaryMenuItem;
  pt: TPoint;
begin
  inherited;
  if Assigned(OnMenuItemDblClick) then
  begin
    pt := Mouse.CursorPos;
    if HandleAllocated then
      pt := ScreenToClient(pt)
    else if Owner is TAdvSmoothRotaryMenuDialog then
    begin
      if Assigned((Owner as TAdvSmoothRotaryMenuDialog).frm) then
        pt := (Owner as TAdvSmoothRotaryMenuDialog).frm.ScreenToClient(pt);
    end;

    it := ItemAtXY(pt.X, pt.Y);
    if Assigned(it) then
      OnMenuItemDblClick(Self, it.Index);
  end;
end;

destructor TAdvSmoothRotaryMenu.Destroy;
begin
  FTimer.Free;
  FItems.Free;
  FDefaultFont.Free;
  FDefaultFill.Free;
  FDefaultHoverFill.Free;
  FDefaultDownFill.Free;
  FDefaultDisabledFill.Free;
  inherited;
end;

procedure TAdvSmoothRotaryMenu.DoEnter;
begin
  inherited;
  Changed;
end;

procedure TAdvSmoothRotaryMenu.DoExit;
begin
  inherited;
  FHoveredElement := nil;
  FDownElement := nil;
  Changed;
end;

procedure TAdvSmoothRotaryMenu.DoItemClick(Sender: TObject; AIndex: Integer);
begin
  if Assigned(OnMenuItemClick) then
    OnMenuItemClick(Sender, AIndex);
end;

destructor TAdvSmoothRotaryMenuDialog.Destroy;
begin
  FMenu.Free;
  inherited;
end;

procedure TAdvSmoothRotaryMenuDialog.DoMenuItemClick(Sender: TObject;
  AItemIndex: Integer);
begin
  if Assigned(OnMenuItemClick) then
    OnMenuItemClick(Self, AItemIndex);
end;

procedure TAdvSmoothRotaryMenuDialog.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothRotaryMenuDialog.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothRotaryMenuDialog.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TAdvSmoothRotaryMenuDialog.DoMenuItemDblClick(Sender: TObject;
  AItemIndex: Integer);
begin
  if Assigned(OnMenuItemDblClick) then
    OnMenuItemDblClick(Self, AItemIndex);
end;

procedure TAdvSmoothRotaryMenuDialog.DoMenuItemHint(Sender: TObject;
  AItemIndex: Integer; var AHint: String);
begin
  if Assigned(OnMenuItemHint) then
    OnMenuItemHint(Self, AItemIndex, AHint);
end;

procedure TAdvSmoothRotaryMenuDialog.DoMenuItemMouseDown(Sender: TObject;
  AItemIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMenuItemMouseDown) then
    OnMenuItemMouseDown(Self, AItemIndex, Button, Shift, X, Y);
end;

procedure TAdvSmoothRotaryMenuDialog.DoMenuItemMouseEnter(Sender: TObject;
  AItemIndex: Integer);
begin
  if Assigned(OnMenuItemMouseEnter) then
    OnMenuItemMouseEnter(Self, AItemIndex);
end;

procedure TAdvSmoothRotaryMenuDialog.DoMenuItemMouseLeave(Sender: TObject;
  AItemIndex: Integer);
begin
  if Assigned(OnMenuItemMouseLeave) then
    OnMenuItemMouseLeave(Self, AItemIndex);
end;

procedure TAdvSmoothRotaryMenuDialog.DoMenuItemMouseMove(Sender: TObject;
  AItemIndex: Integer; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMenuItemMouseMove) then
    OnMenuItemMouseMove(Self, AItemIndex, Shift, X, Y);
end;

procedure TAdvSmoothRotaryMenuDialog.DoMenuItemMouseUp(Sender: TObject;
  AItemIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMenuItemMouseUp) then
    OnMenuItemMouseUp(Self, AItemIndex, Button, Shift, X, Y);
end;

procedure TAdvSmoothRotaryMenuDialog.DoPopupMenu(X, Y: Integer; UsedControl: Boolean);
var
  ExtendedStyle: Integer;
begin
  if Visible then
    Exit;

  if not Assigned(frm) then
  begin
    frm := TAdvSmoothRotaryMenuForm.CreateNew(Application);
    frm.RotaryMenuDialog := Self;
    frm.Width := Width;
    frm.Height := Height;
    Menu.Width := Width;
    Menu.Height := Height;
    Menu.Resize;
    frm.OnDeactivate := FormDeactivate;
    frm.OnClose := FormClose;
    frm.ShowHint := Menu.ShowHint;
    frm.TabStop := Menu.TabStop;
    frm.Init;
    frm.FormStyle := FormStyle;

    if HideFromWindows then
    begin
      ExtendedStyle := GetWindowLong(frm.Handle, GWL_EXSTYLE);
      SetWindowLong(frm.Handle, GWL_EXSTYLE, ExtendedStyle OR WS_EX_TOOLWINDOW
                                                   AND NOT WS_EX_APPWINDOW);
    end;

    SetWindowPos(frm.Handle, 0, X, Y, frm.Width, frm.Height, SWP_SHOWWINDOW or SWP_NOACTIVATE);

    if ActiveFocus then
    begin
      frm.Visible := True;
      frm.SetFocus;
    end;

    Menu.StartInAnimation;

    while Menu.IsAnimating do
    begin
      Sleep(1);
      Application.ProcessMessages;
    end;

    if ActiveFocus then
    begin
      frm.Visible := True;
      frm.SetFocus;
    end;

    FClosing := False;

    if Assigned(OnShow) then
      OnShow(Self);

    FVisible := True;

    frm.FormHookInit;
  end;
end;

procedure TAdvSmoothRotaryMenuDialog.EndUpdate;
begin
  Menu.EndUpdate;
end;

procedure TAdvSmoothRotaryMenu.DrawCaption(g: TGPGraphics;
  AItem: TRotaryMenuItem; R: TGPRectF);
var
  f: TGPFont;
  sf: TGPStringFormat;
  str: string;
  sizer: TGPRectF;
  x, y: Single;
  b: TGPSolidBrush;
begin
  if not Assigned(AItem) then
    Exit;

  if AItem.Caption = '' then
    Exit;

  str := AItem.Caption;

  f := g.MakeFont(AItem.Font);
  sf := TGPStringFormat.Create;
  sf.SetHotkeyPrefix(HotkeyPrefixShow);

  g.MeasureString(str, Length(str), f, MakeRect(0, 0, 10000, 10000), sf, sizer);

  if AItem.CaptionLocation <> clCustom then
  begin
    GetObjectLocation(x, y, r, sizer.Width, sizer.Height, AItem.CaptionLocation);
    x := x + AItem.CaptionLeft;
    y := y + AItem.CaptionTop;
  end
  else
  begin
    x := AItem.CaptionLeft;
    y := AItem.CaptionTop;
  end;

  b := TGPSolidBrush.Create(MakeColor(255, AItem.Font.Color));
  g.DrawString(str, Length(str), f, MakePoint(x, y), sf, b);
  b.free;

  sf.Free;
  f.Free;
end;

procedure TAdvSmoothRotaryMenu.DrawImage(g: TGPGraphics; AItem: TRotaryMenuItem;
  R: TGPRectF);
var
  it: TRotaryMenuItem;
  imgw, imgh: Integer;
  img: TAdvGDIPPicture;
  bmp: TBitmap;
  h: HDC;
  ca: TCanvas;
  x, y: Single;
begin
  if not Assigned(AItem) then
    Exit;

  it := AItem;
  if not it.Image.Empty then
  begin
    if (it.ImageWidth > -1) then
      imgw := it.ImageWidth
    else
      imgw := it.Image.Width;

    if (it.ImageHeight > -1) then
      imgh := it.ImageHeight
    else
      imgh := it.Image.Height;

    GetAspectSize(imgw, imgh, it.Image.Width, it.Image.Height, imgw, imgh);

    if AItem.ImageLocation <> clCustom then
    begin
      GetObjectLocation(x, y, r, imgw, imgh, AItem.ImageLocation);
      x := x + AItem.ImageLeft;
      y := y + AItem.ImageTop;
    end
    else
    begin
      x := AItem.ImageLeft;
      y := AItem.ImageTop;
    end;

    it.Image.GDIPDraw(g, MakeRect(x, y, imgw, imgh));
  end;

  if Assigned(PictureContainer) then
  begin
    if it.ImageName <> '' then
    begin
      img := PictureContainer.FindPicture(it.ImageName);
      if Assigned(img) then
      begin
        if not img.Empty then
        begin
          if (it.ImageWidth > -1) then
            imgw := it.ImageWidth
          else
            imgw := img.Width;

          if (it.ImageHeight > -1) then
            imgh := it.ImageHeight
          else
            imgh := img.Height;

          GetAspectSize(imgw, imgh, img.Width, img.Height, imgw, imgh);

          if AItem.ImageLocation <> clCustom then
          begin
            GetObjectLocation(x, y, r, imgw, imgh, AItem.ImageLocation);
            x := x + AItem.ImageLeft;
            x := x + AItem.ImageTop;
          end
          else
          begin
            x := AItem.ImageLeft;
            y := AItem.ImageTop;
          end;

          img.GDIPDraw
            (g, MakeRect(x, y, imgw, imgh));
        end;
      end;
    end;
  end;

  if Assigned(ImageList) then
  begin
    if (it.ImageIndex >= 0) and (it.ImageIndex <= ImageList.Count - 1) then
    begin
      bmp := TBitmap.Create;
      ImageList.GetBitmap(it.ImageIndex, bmp);
      if not bmp.Empty then
      begin
        if (it.ImageWidth > -1) then
          imgw := it.ImageWidth
        else
          imgw := bmp.Width;

        if (it.ImageHeight > -1) then
          imgh := it.ImageHeight
        else
          imgh := bmp.Height;

        GetAspectSize(imgw, imgh, bmp.Width, bmp.Height, imgw, imgh);

        if AItem.ImageLocation <> clCustom then
        begin
          GetObjectLocation(x, y, r, imgw, imgh, AItem.ImageLocation);
          x := x + AItem.ImageLeft;
          x := x + AItem.ImageTop;
        end
        else
        begin
          x := AItem.ImageLeft;
          y := AItem.ImageTop;
        end;

        h := g.GetHDC;
        ca := TCanvas.Create;
        ca.Handle := h;
        ca.StretchDraw(Bounds(Round(x), Round(y), imgw, imgh), bmp);
        ca.Free;
        g.ReleaseHDC(h);
      end;
      bmp.Free;
    end;
  end;
end;

procedure TAdvSmoothRotaryMenu.DrawItems(g: TGPGraphics; sta, stp: Double; R, RI: TGPRectF);
var
  I: Integer;
  b: Single;
  pth: TGPGraphicsPath;
  rgn: TGPRegion;
  p, pf: TGPPen;
  br: TGPRectF;
  it: TRotaryMenuItem;
  ac: Single;
  ex, ey: Single;
  c, cto: TGPPointF;
  fl: TGDIPFill;
  idx: Integer;
begin
  if Items.Count = 0 then
    Exit;

  b := 0;
  case Layout of
    rmlCircularDonut, rmlCircularPie: b := stp / Items.Count;
    rmlHorizontal: b := R.Width / Items.Count;
    rmlVertical: b := R.Height / Items.Count;
  end;

  pth := TGPGraphicsPath.Create;
  for I := 0 to Items.Count - 1 do
  begin
    it := Items[I];
    pth.Reset;

    GetMenuPath(pth, sta, stp, b, R, RI, I);

    pth.CloseFigure;
    rgn := TGPRegion.Create(pth);
    g.SetClip(rgn);
    pth.GetBounds(br);

    fl := TGDIPFill.Create;
    if it.Enabled then
    begin
      if (FDownElement = it) or (it.Down) then
        fl.Assign(it.DownFill)
      else if FHoveredElement = it then
        fl.Assign(it.HoverFill)
      else
        fl.Assign(it.Fill);
    end
    else
      fl.Assign(it.DisabledFill);

    fl.Fill(g, br);
    fl.Free;

    if (Layout = rmlCircularDonut) or (Layout = rmlCircularPie) then
    begin
      ac := DegToRad(sta + (I * B) + (b / 2));
      ex := r.Width / 2  * Cos(ac);
      ey := r.Height / 2 * Sin(ac);
      c := MakePoint(r.X + (r.Width / 2) + ex, r.Y + (r.Height / 2) + ey);

      if (Layout = rmlCircularDonut) then
      begin
        ex := ri.Width / 2  * Cos(ac);
        ey := ri.Height / 2 * Sin(ac);
        cto := MakePoint(ri.X + (ri.Width / 2) + ex, ri.Y + (ri.Height / 2) + ey);
      end
      else
        cto := MakePointF(ri.X + ri.Width / 2, ri.Y + ri.Height / 2);

      ex := c.X + (cto.X - c.X) / 2;
      ey := c.Y + (cto.Y - c.Y) / 2;
      br := MakeRect(ex - Size / 2, ey - Size / 2, Size, Size);
    end;

    DrawCaption(g, it, br);
    DrawImage(g, it, br);

    g.ResetClip;
    rgn.free;

    p := TGPPen.Create(MakeColor(fl.BorderOpacity, fl.BorderColor), fl.BorderWidth);
    g.DrawPath(p, pth);
    p.Free;
  end;


  if IsFocused and ShowFocus and Assigned(FFocusedElement) then
  begin
    pf := TGPPen.Create(MakeColor(255, FFocusedElement.Fill.FocusColor), 1);
    pf.SetDashStyle(DashStyleDash);
    idx := FFocusedElement.Index;
    pth.Reset;
    GetMenuPath(pth, sta, stp, b, R, RI, idx);
    pth.CloseFigure;
    g.SetSmoothingMode(SmoothingModeHighQuality);
    g.DrawPath(pf, pth);
    g.SetSmoothingMode(SmoothingModeAntiAlias);
    pf.Free;
  end;

  pth.free;
end;

procedure TAdvSmoothRotaryMenu.DrawRotaryMenu(g: TGPGraphics; R: TGPRectF);
var
  ri: TGPRectF;
  sz: Single;
  sta, stp: Single;
  pth: TGPGraphicsPath;
  pthb: TGPPathGradientBrush;
  cb: array[0..2] of TGPColor;
  pb: array[0..2] of Single;
  m: TGPMatrix;
begin
  sz := Size;
  ri := MakeRect(r.X + sz, r.Y + sz, r.Width - sz * 2, r.Height - sz * 2);

  sta := StartAngle;
  stp := StopAngle - StartAngle;

  if (Items.Count > 0) and (ShadowColor <> clNone) and (RI.Width > 0) and (RI.Height > 0) then
  begin
    pth := TGPGraphicsPath.Create;
    GetMenuPath(pth, sta, stp, 0, R, RI, -1);

    pth.CloseFigure;

    pthb := TGPPathGradientBrush.Create(pth);

    pthb.SetWrapMode(WrapModeClamp);

    cb[0] := MakeColor(0,0,0,0);
    cb[1] := MakeColor(ShadowOpacity, ShadowColor);
    cb[2] := MakeColor(ShadowOpacity, ShadowColor);

    pb[0] := 0;
    pb[1] := 0.1;
    pb[2] := 1;

    pthb.SetInterpolationColors(@cb, @pb, 3);

    m := TGPMatrix.Create;
    m.Translate(ShadowOffset, ShadowOffset);
    g.SetTransform(m);
    g.FillPath(pthb, pth);
    g.ResetTransform;
    m.Free;

    pthb.Free;
    pth.Free;
  end;

  DrawItems(g, sta, stp, r, ri);
end;

procedure TAdvSmoothRotaryMenu.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Changed;
end;

function TAdvSmoothRotaryMenu.NextH: Double;
begin
  Result := FHTo;
end;

function TAdvSmoothRotaryMenu.NextStartAngle: Double;
begin
  Result := FStartAngleTo;
end;

function TAdvSmoothRotaryMenu.NextStopAngle: Double;
begin
  Result := FStopAngleTo;
end;

function TAdvSmoothRotaryMenu.NextW: Double;
begin
  Result := FWTo;
end;

function TAdvSmoothRotaryMenu.NextX: Double;
begin
  Result := FXTo;
end;

function TAdvSmoothRotaryMenu.NextY: Double;
begin
  Result := FYTo;
end;

procedure TAdvSmoothRotaryMenu.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if (csDestroying in ComponentState) then
    Exit;

  if (AOperation = opRemove) and (AComponent = PictureContainer) then
    PictureContainer := nil;

  if (AOperation = opRemove) and (AComponent = ImageList) then
    ImageList := nil;
end;

procedure TAdvSmoothRotaryMenu.Paint;
var
  g: TGPGraphics;
  R: TGPRectF;
begin
  g := TGPGraphics.Create(Canvas.Handle);

  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);

  R := GetMenuRect;
  DrawRotaryMenu(g, R);
  g.Free;
end;

procedure TAdvSmoothRotaryMenu.Resize;
begin
  inherited;
  FW := Width;
  FH := Height;
  FWTo := FW;
  FWTemp := FW;
  FHTo := FH;
  FHTemp := FH;
  Changed;
end;

procedure TAdvSmoothRotaryMenuDialog.PopupMenuAt(X, Y: Integer);
begin
  DoPopupMenu(X, Y, False);
end;

procedure TAdvSmoothRotaryMenuDialog.PopupMenuAtControl(Control: TControl);
var
  X, Y: Integer;
  p: TPoint;
  r: TRect;
begin
  if Assigned(Control) then
  begin
    if Assigned(Control.Parent) then
    begin
      X := Control.Left;
      Y := Control.Top;

      p := Point(X, Y);
      p := Control.Parent.ClientToScreen(p);
      X := p.X;
      Y := p.Y;
      SystemParametersInfo(SPI_GETWORKAREA, 0,@r,0);

      case Position of
        rmpCenterLeft, rmpTopLeft, rmpBottomLeft: X := X - Width;
        rmpCenterCenter, rmpTopCenter, rmpBottomCenter: X := X - (Width - Control.Width) div 2;
        rmpCenterRight, rmpTopRight, rmpBottomright: X := X + Control.Width;
      end;

      case Position of
        rmpTopLeft, rmpTopCenter, rmpTopRight: Y := Y - Height;
        rmpCenterCenter, rmpCenterRight, rmpCenterLeft: Y := Y - (Height - Control.Height) div 2;
        rmpBottomLeft, rmpBottomCenter, rmpBottomRight: Y := Y + Control.Height;
      end;

      DoPopupMenu(X, Y, True);
    end;
  end;
end;

procedure TAdvSmoothRotaryMenuDialog.SetCloseOnDeactivate(const Value: Boolean);
begin
  if FCloseOnDeactivate <> Value then
  begin
    FCloseOnDeactivate := Value;
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenuDialog.SetCloseOnMouseLeave(const Value: Boolean);
begin
  if FCloseOnMouseLeave <> Value then
  begin
    FCloseOnMouseLeave := Value;
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenu.SetColorTones(ATones: TColorTones);
begin

end;

procedure TAdvSmoothRotaryMenuDialog.SetColorTones(ATones: TColorTones);
begin
  Menu.SetColorTones(ATones);
end;

procedure TAdvSmoothRotaryMenu.SetAnimation(const Value: Boolean);
begin
  if FAnimation <> Value then
  begin
    FAnimation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenu.SetAnimationFactor(const Value: Integer);
begin
  if FAnimationFactor <> Value then
  begin
    FAnimationFactor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenu.SetAnimationMode(
  const Value: TRotaryMenuAnimationModes);
begin
  if FAnimationMode <> Value then
  begin
    FAnimationMode := Value;
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenu.SetAnimationType(
  const Value: TRotaryMenuAnimationType);
begin
  if FAnimationType <> Value then
  begin
    FAnimationType := Value;
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenu.SetItems(const Value: TRotaryMenuItems);
begin
  if FItems <> Value then
  begin
    FItems.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenu.SetLayout(const Value: TRotaryMenuLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenu.SetComponentStyle(AStyle: TTMSStyle);
var
  I: Integer;
begin
  FTMSStyle := AStyle;
  case AStyle of
    tsOffice2003Blue:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := $D68759;
      DefaultFill.ColorTo := $933803;
      DefaultFill.BorderColor := $962D00;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $EBFDFF;
      DefaultHoverFill.ColorTo := $ACECFF;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $94E6FB;
      DefaultDownFill.ColorTo := $1595EE;
      DefaultDownFill.EndUpdate;
    end;

    tsOffice2003Silver:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := $BDA4A5;
      DefaultFill.ColorTo := $957475;
      DefaultFill.BorderColor := $947C7C;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $EBFDFF;
      DefaultHoverFill.ColorTo := $ACECFF;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $94E6FB;
      DefaultDownFill.ColorTo := $1595EE;
      DefaultDownFill.EndUpdate;
    end;

    tsOffice2003Olive:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := $82C0AF;
      DefaultFill.ColorTo := $447A63;
      DefaultFill.BorderColor := $588060;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $EBFDFF;
      DefaultHoverFill.ColorTo := $ACECFF;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $94E6FB;
      DefaultDownFill.ColorTo := $1595EE;
      DefaultDownFill.EndUpdate;
    end;

    tsOffice2003Classic:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := $808080;
      DefaultFill.ColorTo := $808080;
      DefaultFill.BorderColor := $D2BDB6;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $D2BDB6;
      DefaultHoverFill.ColorTo := $D2BDB6;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $B59285;
      DefaultDownFill.ColorTo := $B59285;
      DefaultDownFill.EndUpdate;
    end;

    tsOffice2007Luna:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := $FFEFE3;
      DefaultFill.ColorTo := $FFD2AF;
      DefaultFill.BorderColor := $00FFD2AF;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $EBFDFF;
      DefaultHoverFill.ColorTo := $59DAFF;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $AAD9FF;
      DefaultDownFill.ColorTo := $7AE1FE;
      DefaultDownFill.EndUpdate;
    end;

    tsOffice2007Obsidian:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := $F2F1F0;
      DefaultFill.ColorTo := $C9C2BD;
      DefaultFill.BorderColor := $5C534C;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $EBFDFF;
      DefaultHoverFill.ColorTo := $59DAFF;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $AAD9FF;
      DefaultDownFill.ColorTo := $7AE1FE;
      DefaultDownFill.EndUpdate;
    end;

    tsWindowsXP:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := clBtnFace;
      DefaultFill.ColorTo := clBtnFace;
      DefaultFill.BorderColor := clBlack;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := clInactiveCaptionText;
      DefaultHoverFill.ColorTo := clInactiveCaptionText;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := clInactiveCaption;
      DefaultDownFill.ColorTo := clInactiveCaption;
      DefaultDownFill.EndUpdate;
    end;

    tsWhidbey:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := $EBEEEF;
      DefaultFill.ColorTo := $7E9898;
      DefaultFill.BorderColor := $962D00;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $94E6FB;
      DefaultHoverFill.ColorTo := $1595EE;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $94E6FB;
      DefaultDownFill.ColorTo := $1595EE;
      DefaultDownFill.EndUpdate;
    end;

    tsCustom: ;
    tsOffice2007Silver:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := $F8F7F6;
      DefaultFill.ColorTo := $E8E0DB;
      DefaultFill.BorderColor := $74706F;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $EBFDFF;
      DefaultHoverFill.ColorTo := $59DAFF;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $AAD9FF;
      DefaultDownFill.ColorTo := $7AE1FE;
      DefaultDownFill.EndUpdate;
    end;

    tsWindowsVista:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := $FCF9F2;
      DefaultFill.ColorTo := $F7EED9;
      DefaultFill.BorderColor := $F9D996;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $FEF9F0;
      DefaultHoverFill.ColorTo := $FDF0D7;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $FEF9F0;
      DefaultDownFill.ColorTo := $FDF0D7;
      DefaultDownFill.EndUpdate;
    end;

    tsWindows7:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := $FCF9F2;
      DefaultFill.ColorTo := $F7EED9;
      DefaultFill.BorderColor := $F9D996;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $FDFBFA;
      DefaultHoverFill.ColorTo := $FDF3EB;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $FCEBDC;
      DefaultDownFill.ColorTo := $FCDBC1;
      DefaultDownFill.EndUpdate;
    end;

    tsTerminal:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := clBtnFace;
      DefaultFill.ColorTo := clBtnFace;
      DefaultFill.BorderColor := clGray;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := clSilver;
      DefaultHoverFill.ColorTo := clSilver;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := clHighLight;
      DefaultDownFill.ColorTo := clHighLight;
      DefaultDownFill.EndUpdate;
    end;

    tsOffice2010Blue:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := $FDF6EF;
      DefaultFill.ColorTo := $F0DAC7;
      DefaultFill.BorderColor := $C7B29F;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $8AE3FD;
      DefaultHoverFill.ColorTo := $CAECFD;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $6CD0FF;
      DefaultDownFill.ColorTo := $7BEEFF;
      DefaultDownFill.EndUpdate;
    end;

    tsOffice2010Silver:
     begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := $FFFFFF;
      DefaultFill.ColorTo := $EDE5E0;
      DefaultFill.BorderColor := $D2CDC8;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $8AE3FD;
      DefaultHoverFill.ColorTo := $CAECFD;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $6CD0FF;
      DefaultDownFill.ColorTo := $7BEEFF;
      DefaultDownFill.EndUpdate;
    end;


    tsOffice2010Black:
     begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := $BFBFBF;
      DefaultFill.ColorTo := $D7D7D6;
      DefaultFill.BorderColor := $6D6D6D;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $8AE3FD;
      DefaultHoverFill.ColorTo := $CAECFD;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $6CD0FF;
      DefaultDownFill.ColorTo := $7BEEFF;
      DefaultDownFill.EndUpdate;
    end;

    tsWindows8, tsWindows10:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := $F7F6F5;
      DefaultFill.ColorTo := $F7F6F5;
      DefaultFill.BorderColor := $E4E3E2;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $F7EFE8;
      DefaultHoverFill.ColorTo := $F7EFE8;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $F7E0C9;
      DefaultDownFill.ColorTo := $F7E0C9;
      DefaultDownFill.EndUpdate;
    end;

    tsOffice2013White:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := clWhite;
      DefaultFill.ColorTo := clWhite;
      DefaultFill.BorderColor := $D4D4D4;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $FCF0E4;
      DefaultHoverFill.ColorTo := $FCF0E4;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $FCE2C8;
      DefaultDownFill.ColorTo := $FCE2C8;
      DefaultDownFill.EndUpdate;
    end;


    tsOffice2013LightGray:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := $F6F6F6;
      DefaultFill.ColorTo := $F6F6F6;
      DefaultFill.BorderColor := $C6C6C6;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $FCF0E4;
      DefaultHoverFill.ColorTo := $FCF0E4;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $FCE2C8;
      DefaultDownFill.ColorTo := $FCE2C8;
      DefaultDownFill.EndUpdate;
    end;

    tsOffice2013Gray:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := $E5E5E5;
      DefaultFill.ColorTo := $E5E5E5;
      DefaultFill.BorderColor := $ABABAB;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $FCF0E4;
      DefaultHoverFill.ColorTo := $FCF0E4;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $FCE2C8;
      DefaultDownFill.ColorTo := $FCE2C8;
      DefaultDownFill.EndUpdate;
    end;

   tsOffice2016White:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := clWhite;
      DefaultFill.ColorTo := clWhite;
      DefaultFill.BorderColor := $D4D4D4;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $F2E1D5;
      DefaultHoverFill.ColorTo := $F2E1D5;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $E3BDA3;
      DefaultDownFill.ColorTo := $E3BDA3;
      DefaultDownFill.EndUpdate;
    end;


    tsOffice2016Gray:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := $B2B2B2;
      DefaultFill.ColorTo := $B2B2B2;
      DefaultFill.BorderColor := $444444;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $F2E1D5;
      DefaultHoverFill.ColorTo := $F2E1D5;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $E3BDA3;
      DefaultDownFill.ColorTo := $E3BDA3;
      DefaultDownFill.EndUpdate;
    end;

    tsOffice2016Black:
    begin
      DefaultFill.BeginUpdate;
      DefaultFill.Color := $363636;
      DefaultFill.ColorTo := $363636;
      DefaultFill.BorderColor := $444444;
      DefaultFill.EndUpdate;

      DefaultHoverFill.BeginUpdate;
      DefaultHoverFill.Assign(DefaultFill);
      DefaultHoverFill.Color := $6A6A6A;
      DefaultHoverFill.ColorTo := $6A6A6A;
      DefaultHoverFill.EndUpdate;

      DefaultDownFill.BeginUpdate;
      DefaultDownFill.Assign(DefaultFill);
      DefaultDownFill.Color := $444444;
      DefaultDownFill.ColorTo := $444444;
      DefaultDownFill.EndUpdate;
    end;


  end;

  for I := 0 to Items.Count - 1 do
  begin
    Items[I].Font.Assign(DefaultFont);
    Items[I].Fill.Assign(DefaultFill);
    Items[I].DownFill.Assign(DefaultDownFill);
    Items[I].HoverFill.Assign(DefaultHoverFill);
    Items[I].DisabledFill.Assign(DefaultDisabledFill);
  end;
end;

procedure TAdvSmoothRotaryMenuDialog.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  Menu.SetComponentStyle(AStyle);
end;

procedure TAdvSmoothRotaryMenuDialog.SetDefaultDisabledFill(
  const Value: TGDIPFill);
begin
  FMenu.DefaultDisabledFill.Assign(Value);
end;

procedure TAdvSmoothRotaryMenuDialog.SetDefaultDownFill(const Value: TGDIPFill);
begin
  FMenu.DefaultDownFill.Assign(Value);
end;

procedure TAdvSmoothRotaryMenuDialog.SetDefaultFill(const Value: TGDIPFill);
begin
  FMenu.DefaultFill.Assign(Value);
end;

procedure TAdvSmoothRotaryMenuDialog.SetDefaultFont(const Value: TFont);
begin
  FMenu.DefaultFont.Assign(Value);
end;

procedure TAdvSmoothRotaryMenuDialog.SetDefaultHoverFill(
  const Value: TGDIPFill);
begin
  FMenu.DefaultHoverFill.Assign(Value);
end;

procedure TAdvSmoothRotaryMenu.SetDefaultDisabledFill(const Value: TGDIPFill);
begin
  if FDefaultDisabledFill <> Value then
  begin
    FDefaultDisabledFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenu.SetDefaultDownFill(const Value: TGDIPFill);
begin
  if FDefaultDownFill <> Value then
  begin
    FDefaultDownFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenu.SetDefaultFill(const Value: TGDIPFill);
begin
  if FDefaultFill <> Value then
  begin
    FDefaultFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenu.SetDefaultFont(const Value: TFont);
begin
  if FDefaultFont <> Value then
  begin
    FDefaultFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenu.SetDefaultHoverFill(const Value: TGDIPFill);
begin
  if FDefaultHoverFill <> Value then
  begin
    FDefaultHoverFill := Value;
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenu.SetDefaultStyle;
var
  I: Integer;
begin
  DefaultFill.BeginUpdate;
  DefaultFill.Color := clWhite;
  DefaultFill.ColorTo := clSilver;
  DefaultFill.BorderColor := clGray;
  DefaultFill.EndUpdate;

  DefaultHoverFill.BeginUpdate;
  DefaultHoverFill.Assign(DefaultFill);
  DefaultHoverFill.Color := clWhite;
  DefaultHoverFill.ColorTo := RGB(191, 235, 255);
  DefaultHoverFill.EndUpdate;

  DefaultDownFill.BeginUpdate;
  DefaultDownFill.Assign(DefaultFill);
  DefaultDownFill.Color := RGB(89, 201, 255);
  DefaultDownFill.ColorTo := RGB(0, 131, 193);
  DefaultDownFill.EndUpdate;

  for I := 0 to Items.Count - 1 do
  begin
    Items[I].Font.Assign(DefaultFont);
    Items[I].Fill.Assign(DefaultFill);
    Items[I].DownFill.Assign(DefaultDownFill);
    Items[I].HoverFill.Assign(DefaultHoverFill);
    Items[I].DisabledFill.Assign(DefaultDisabledFill);
  end;

end;

procedure TAdvSmoothRotaryMenu.SetH(const Value: Double);
var
  anim: Boolean;
begin
  anim := (IsAnimating and Animation) or (FUpdateCount > 0);
  FHTo := Value;
  if anim then
  begin
    FH := NextH;
    FHTemp := FH;
    Changed;
  end;
  if not Animation then
  begin
    FH := NextH;
    FHTemp := FH;
    Changed;
  end
  else
    FAnimate := (not FBlockAnimate) and (FUpdateCount = 0);
end;

procedure TAdvSmoothRotaryMenuDialog.SetActiveFocus(const Value: Boolean);
begin
  FActiveFocus := Value;
end;

procedure TAdvSmoothRotaryMenuDialog.SetAnimation(const Value: Boolean);
begin
  FMenu.Animation := Value;
end;

procedure TAdvSmoothRotaryMenuDialog.SetAnimationFactor(const Value: Integer);
begin
  FMenu.AnimationFactor := Value;
end;

procedure TAdvSmoothRotaryMenuDialog.SetAnimationMode(
  const Value: TRotaryMenuAnimationModes);
begin
  FMenu.AnimationMode := Value;
end;

procedure TAdvSmoothRotaryMenuDialog.SetAnimationType(
  const Value: TRotaryMenuAnimationType);
begin
  FMenu.AnimationType := Value;
end;

procedure TAdvSmoothRotaryMenuDialog.SetFormStyle(const Value: TFormStyle);
begin
  FFormStyle := Value;
end;

procedure TAdvSmoothRotaryMenuDialog.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenuDialog.SetImageList(
  const Value: TCustomImageList);
begin
  FMenu.ImageList := Value;
end;

procedure TAdvSmoothRotaryMenuDialog.SetItems(const Value: TRotaryMenuItems);
begin
  FMenu.Items.Assign(Value);
end;

procedure TAdvSmoothRotaryMenuDialog.SetLayout(const Value: TRotaryMenuLayout);
begin
  FMenu.Layout := Value;
end;

procedure TAdvSmoothRotaryMenuDialog.SetPictureContainer(
  const Value: TGDIPPictureContainer);
begin
  FMenu.PictureContainer := Value;
end;

procedure TAdvSmoothRotaryMenuDialog.SetPosition(const Value: TRotaryMenuPosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenuDialog.SetShadowColor(const Value: TColor);
begin
  FMenu.ShadowColor := Value;
end;

procedure TAdvSmoothRotaryMenuDialog.SetShadowOffset(const Value: Integer);
begin
  FMenu.ShadowOffset := Value;
end;

procedure TAdvSmoothRotaryMenuDialog.SetShadowOpacity(const Value: Byte);
begin
  FMenu.ShadowOpacity := Value;
end;

procedure TAdvSmoothRotaryMenuDialog.SetShowFocus(const Value: Boolean);
begin
  FMenu.ShowFocus := Value;
end;

procedure TAdvSmoothRotaryMenuDialog.SetSize(const Value: Integer);
begin
  FMenu.Size := Value;
end;

procedure TAdvSmoothRotaryMenuDialog.SetStartAngle(const Value: Double);
begin
  FMenu.StartAngle := Value;
end;

procedure TAdvSmoothRotaryMenuDialog.SetStopAngle(const Value: Double);
begin
  FMenu.StopAngle := Value;
end;

procedure TAdvSmoothRotaryMenu.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenu.SetShadowOpacity(const Value: Byte);
begin
  if FShadowOpacity <> Value then
  begin
    FShadowOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenu.SetShowFocus(const Value: Boolean);
begin
  FShowFocus := Value;
end;

procedure TAdvSmoothRotaryMenu.SetSize(const Value: Integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenu.SetShadowOffset(const Value: Integer);
begin
  if FShadowOffset <> Value then
  begin
    FShadowOffset := Max(0, Value);
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenu.SetStartAngle(const Value: Double);
var
  anim: Boolean;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
  begin
    FStartAngle := Value;
    FStartAngleTo := Value;
    FStartAngleTemp := Value;
    Changed;
  end
  else
  begin
    anim := (IsAnimating and Animation) or (FUpdateCount > 0);
    FStartAngleTo := Value;
    if anim then
    begin
      FStartAngle := NextStartAngle;
      FStartAngleTemp := FStartAngle;
      Changed;
    end;
    if not Animation then
    begin
      FStartAngle := NextStartAngle;
      FStartAngleTemp := FStartAngle;
      Changed;
    end
    else
      FAnimate := (not FBlockAnimate) and (FUpdateCount = 0);
  end;
end;

procedure TAdvSmoothRotaryMenu.SetStopAngle(const Value: Double);
var
  anim: Boolean;
begin
  anim := (IsAnimating and Animation) or (FUpdateCount > 0);
  FStopAngleTo := Value;
  if anim then
  begin
    FStopAngle := NextStopAngle;
    FStopAngleTemp := FStopAngle;
    Changed;
  end;
  if not Animation then
  begin
    FStopAngle := NextStopAngle;
    FStopAngleTemp := FStopAngle;
    Changed;
  end
  else
    FAnimate := (not FBlockAnimate) and (FUpdateCount = 0);
end;

procedure TAdvSmoothRotaryMenu.SetW(const Value: Double);
var
  anim: Boolean;
begin
  anim := (IsAnimating and Animation) or (FUpdateCount > 0);
  FWTo := Value;
  if anim then
  begin
    FW := NextW;
    FWTemp := FW;
    Changed;
  end;
  if not Animation then
  begin
    FW := NextW;
    FWTemp := FW;
    Changed;
  end
  else
    FAnimate := (not FBlockAnimate) and (FUpdateCount = 0);
end;

procedure TAdvSmoothRotaryMenu.SetX(const Value: Double);
var
  anim: Boolean;
begin
  anim := (IsAnimating and Animation) or (FUpdateCount > 0);
  FXTo := Value;
  if anim then
  begin
    FX := NextX;
    FXTemp := FX;
    Changed;
  end;
  if not Animation then
  begin
    FX := NextX;
    FXTemp := FX;
    Changed;
  end
  else
    FAnimate := (not FBlockAnimate) and (FUpdateCount = 0);
end;

procedure TAdvSmoothRotaryMenu.SetY(const Value: Double);
var
  anim: Boolean;
begin
  anim := (IsAnimating and Animation) or (FUpdateCount > 0);
  FYTo := Value;
  if anim then
  begin
    FY := NextY;
    FYTemp := FY;
    Changed;
  end;
  if not Animation then
  begin
    FY := NextY;
    FYTemp := FY;
    Changed;
  end
  else
    FAnimate := (not FBlockAnimate) and (FUpdateCount = 0);
end;

procedure TAdvSmoothRotaryMenu.StartInAnimation;
var
  anim: Boolean;
begin
  if (amAngle in AnimationMode) and ((Layout = rmlCircularDonut) or (Layout = rmlCircularPie)) then
  begin
    if FSaveStopAngle = -1 then
      FSaveStopAngle := StopAngle;
  end;

  if amSize in AnimationMode then
  begin
    if FSaveW = -1 then
      FSaveW := FW;

    if FSaveH = -1 then
      FSaveH := FH;
  end;

  anim := Animation;
  Animation := False;

  if (amAngle in AnimationMode) and ((Layout = rmlCircularDonut) or (Layout = rmlCircularPie)) then
    StopAngle := StartAngle;

  if amSize in AnimationMode then
  begin
    X := Width div 2;
    Y := Height div 2;
    W := 0;
    H := 0;
  end;

  if (AnimationType = atInOut) or (AnimationType = atIn) then
    Animation := anim;

  FBlockAnimate := True;
  if (amAngle in AnimationMode) and ((Layout = rmlCircularDonut) or (Layout = rmlCircularPie)) then
    StopAngle := FSaveStopAngle;
  if amSize in AnimationMode then
  begin
    X := 0;
    Y := 0;
    W := FSaveW;
    H := FSaveH;
  end;
  FBlockAnimate := False;
  FAnimate := (FUpdateCount = 0);

  if not ((AnimationType = atInOut) or (AnimationType = atIn)) then
    animation := anim;
end;

procedure TAdvSmoothRotaryMenu.StartOutAnimation;
var
  anim: Boolean;
begin
  anim := Animation;
  Animation := False;

  FBlockAnimate := True;
  if (amAngle in AnimationMode) and ((Layout = rmlCircularDonut) or (Layout = rmlCircularPie)) then
    StopAngle := FSaveStopAngle;

  if amSize in AnimationMode then
  begin
    W := Width;
    H := Height;
    X := 0;
    Y := 0;
  end;

  if (AnimationType = atInOut) or (AnimationType = atOut) then
    Animation := anim;

  FBlockAnimate := True;
  if (amAngle in AnimationMode) and ((Layout = rmlCircularDonut) or (Layout = rmlCircularPie)) then
    StopAngle := StartAngle;
  if amSize in AnimationMode then
  begin
    X := Width div 2;
    Y := Height div 2;
    W := 0;
    H := 0;
  end;

  FBlockAnimate := False;
  FAnimate := (FUpdateCount = 0);

  if not ((AnimationType = atInOut) or (AnimationType = atOut)) then
    animation := anim;
end;

procedure TAdvSmoothRotaryMenu.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if TabStop then
    Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS
  else
    Message.Result := 0;
end;

procedure TAdvSmoothRotaryMenuDialog.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

{ TAdvSmoothRotaryMenuForm }

procedure TAdvSmoothRotaryMenuForm.ClearBuffer(Graphics: TGPGraphics);
var
  g: TGPGraphics;
begin
  g := Graphics;
  if not Assigned(g) then
    g := CreateGraphics;
  g.Clear($00000000);
  if not Assigned(Graphics) then
    g.Free;
end;

procedure TAdvSmoothRotaryMenuForm.CMHintShow(var Message: TMessage);
begin
  inherited;
  if Assigned(RotaryMenuDialog) then
    RotaryMenuDialog.Menu.CMHintShow(Message);
end;

procedure TAdvSmoothRotaryMenuForm.CMMouseEnter(var Message: TMessage);
begin
  FMouseEntered := True;
  if Assigned(RotaryMenuDialog) then
    RotaryMenuDialog.Menu.CMMouseEnter(Message);
end;

procedure TAdvSmoothRotaryMenuForm.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(RotaryMenuDialog) then
  begin
    RotaryMenuDialog.Menu.CMMouseLeave(Message);
    if RotaryMenuDialog.CloseOnMouseLeave then
      RotaryMenuDialog.ClosePopupMenu;
  end;
end;

function TAdvSmoothRotaryMenuForm.CreateGraphics: TGPGraphics;
begin
  result := nil;
  if Assigned(FMainBuffer) then
    Result := TGPGraphics.Create(FMainBuffer);
end;

procedure TAdvSmoothRotaryMenuForm.CreateMainBuffer;
begin
  if Assigned(FMainBuffer) then
    FMainBuffer.Free;

  FMainBuffer := TGPBitmap.Create(Width, Height, PixelFormat32bppARGB);
end;

constructor TAdvSmoothRotaryMenuForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  FMouseEntered := False;
end;

procedure TAdvSmoothRotaryMenuForm.CreateWnd;
begin
  inherited;
end;

procedure TAdvSmoothRotaryMenuForm.DblClick;
begin
  inherited;
  if Assigned(RotaryMenuDialog) then
    RotaryMenuDialog.Menu.DblClick;
end;

procedure TAdvSmoothRotaryMenuForm.DestroyMainBuffer;
begin
  if Assigned(FMainBuffer) then
    FMainBuffer.Free;
end;

procedure TAdvSmoothRotaryMenuForm.DoCreate;
begin
  inherited;
  FMainBuffer := nil;
end;

procedure TAdvSmoothRotaryMenuForm.DoDestroy;
begin
  inherited;
  DestroyMainBuffer;
end;

procedure TAdvSmoothRotaryMenuForm.DoEnter;
begin
  inherited;
  if Assigned(RotaryMenuDialog) then
    RotaryMenuDialog.Menu.DoEnter;
end;

procedure TAdvSmoothRotaryMenuForm.DoExit;
begin
  inherited;
  if Assigned(RotaryMenuDialog) then
    RotaryMenuDialog.Menu.DoExit;
end;

procedure TAdvSmoothRotaryMenuForm.Draw(Graphics: TGPGraphics);
var
  g: TGPGraphics;
  R: TGPRectF;
begin
  g := Graphics;
  if not Assigned(g) then
    g := CreateGraphics;

  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);

  if Assigned(RotaryMenuDialog) then
  begin
    R := RotaryMenuDialog.Menu.GetMenuRect;
    RotaryMenuDialog.Menu.DrawRotaryMenu(g, R);
  end;

  if not Assigned(Graphics) then
    g.Free;
end;

procedure TAdvSmoothRotaryMenuForm.FormHookDone;
var
  f: TCustomForm;
begin
  if Assigned(RotaryMenuDialog) and (RotaryMenuDialog.Owner is TCustomForm) then
  begin
    if RotaryMenuDialog.ActiveFocus then
      Exit;
    f := RotaryMenuDialog.Owner as TCustomForm;
    if Assigned(f) and f.HandleAllocated then
    {$IFDEF DELPHI_UNICODE}
      SetWindowLongPtr(f.Handle, GWL_WNDPROC, LInteger(OldWndProc));
    {$ENDIF}
    {$IFNDEF DELPHI_UNICODE}
      SetWindowLong(f.Handle, GWL_WNDPROC, LInteger(OldWndProc));
    {$ENDIF}
  end;
end;

procedure TAdvSmoothRotaryMenuForm.FormHookInit;
var
  f: TCustomForm;
begin
  if Assigned(RotaryMenuDialog) and (RotaryMenuDialog.Owner is TCustomForm) then
  begin
    if RotaryMenuDialog.ActiveFocus then
      Exit;

    f := RotaryMenuDialog.Owner as TCustomForm;
    if assigned(f) then
    begin
       { Hook parent }

      {$IFDEF DELPHI_UNICODE}
      OldWndProc := TFarProc(GetWindowLongPtr(f.Handle, GWL_WNDPROC));
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      OldWndProc := TFarProc(GetWindowLong(f.Handle, GWL_WNDPROC));
      {$ENDIF}

      {$IFDEF DELPHI9_LVL}
      NewWndProc := Classes.MakeObjectInstance(HookWndProc);
      {$ELSE}
      NewWndProc := MakeObjectInstance(HookWndProc);
      {$ENDIF}

      {$IFDEF DELPHI_UNICODE}
      SetWindowLongPtr(f.Handle, GWL_WNDPROC, LInteger(NewWndProc));
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      SetWindowLong(f.Handle, GWL_WNDPROC, LInteger(NewWndProc));
      {$ENDIF}
    end;
  end;
end;


procedure TAdvSmoothRotaryMenuForm.HookWndProc(var Msg: TMessage);
var
  f: TCustomForm;
begin
  if (csDestroying in ComponentState) then
    Exit;

  if Assigned(RotaryMenuDialog) and (RotaryMenuDialog.Owner is TCustomForm) then
  begin
    f := RotaryMenuDialog.Owner as TCustomForm;
    if Assigned(f) then
    begin
      Msg.Result := CallWindowProc(OldWndProc, f.Handle, Msg.Msg , Msg.wParam, Msg.lParam);
      case Msg.Msg of
       WM_WINDOWPOSCHANGED:
       begin
         if Assigned(RotaryMenuDialog.frm) then
         begin
           RotaryMenuDialog.ClosePopupMenu;
         end;
       end;
      end;
    end;
  end;
end;

procedure TAdvSmoothRotaryMenuForm.Init;
begin
  Visible := False;
  BorderIcons := [];
  BorderStyle := bsNone;
  Ctl3D := false;
  Color := clWhite;
  CreateMainBuffer;
  SetLayeredWindow;
  UpdateLayered;
end;

procedure TAdvSmoothRotaryMenuForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Assigned(RotaryMenuDialog) and TabStop then
  begin
    RotaryMenuDialog.Menu.KeyDown(Key, Shift);

    case Key of
      VK_ESCAPE: RotaryMenuDialog.ClosePopupMenu;
    end;
  end;
end;

procedure TAdvSmoothRotaryMenuForm.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Assigned(RotaryMenuDialog) and TabStop then
    RotaryMenuDialog.Menu.KeyUp(Key, Shift);
end;

procedure TAdvSmoothRotaryMenuForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(RotaryMenuDialog) then
  begin
    if RotaryMenuDialog.ActiveFocus then
      SetFocus;

    RotaryMenuDialog.Menu.MouseDown(Button, Shift, X, Y);
  end;
end;

procedure TAdvSmoothRotaryMenuForm.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(RotaryMenuDialog) then
    RotaryMenuDialog.Menu.MouseMove(Shift, X, Y);
end;

procedure TAdvSmoothRotaryMenuForm.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(RotaryMenuDialog) then
    RotaryMenuDialog.Menu.MouseUp(Button, Shift, X, Y);
end;

procedure TAdvSmoothRotaryMenu.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  prev: TRotaryMenuItem;
begin
  inherited;
  if (csDestroying in ComponentState) then
    Exit;

  if HandleAllocated then
    SetFocus;

  prev := FDownElement;
  FDownElement := ItemAtXY(X, Y);
  if Assigned(FDownElement) then
    FFocusedElement := FDownElement;

  if prev <> FDownElement then
  begin
    if Assigned(OnMenuItemMouseDown) and Assigned(FDownElement) then
      OnMenuItemMouseDown(Self, FDownElement.Index, Button, Shift, X, Y);
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenu.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  prev: TRotaryMenuItem;
begin
  inherited;
  if (csDestroying in ComponentState) or IsAnimating then
    Exit;

  prev := FHoveredElement;
  FHoveredElement := ItemAtXY(X, Y);
  if Assigned(OnMenuItemMouseMove) then
  begin
    if Assigned(FHoveredElement) then
      OnMenuItemMouseMove(Self, FHoveredElement.Index, Shift, X, Y)
    else
      OnMenuItemMouseMove(Self, -1, Shift, X, Y)
  end;

  if prev <> FHoveredElement then
  begin
    if Assigned(OnMenuItemMouseLeave) and Assigned(prev) then
      OnMenuItemMouseLeave(Self, prev.Index);

    if Assigned(OnMenuItemMouseEnter) and Assigned(FHoveredElement) then
      OnMenuItemMouseEnter(Self, FHoveredElement.Index);

    Application.CancelHint;
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenu.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if (csDestroying in ComponentState) then
    Exit;

  if Assigned(FDownElement) then
  begin
    if FDownElement.Toggle then
      FDownElement.Down := not FDownElement.Down;
    if Assigned(OnMenuItemMouseUp) and Assigned(FDownElement) then
      OnMenuItemMouseUp(Self, FDownElement.Index, Button, Shift, X, Y);
    DoItemClick(FDownElement, FDownElement.Index);
    FDownElement := nil;
    Changed;
  end;
end;

procedure TAdvSmoothRotaryMenuForm.Paint;
begin
  inherited;
  UpdateWindow;
end;

procedure TAdvSmoothRotaryMenuForm.SetLayeredWindow;
begin
  if GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_LAYERED = 0 then
    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE)
        or WS_EX_LAYERED);

  UpdateLayered;
end;

procedure TAdvSmoothRotaryMenuForm.UpdateLayered;
begin
  ClearBuffer(nil);

  SetWindowPos(Self.Handle, HWND_TOP, 0, 0, 0, 0,
    SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED or SWP_NOACTIVATE);

  Draw(nil);

  UpdateMainWindow;
end;

procedure TAdvSmoothRotaryMenuForm.UpdateMainWindow;
var
  ScrDC, MemDC: HDC;
  BitmapHandle, PrevBitmap: HBITMAP;
  BlendFunc: _BLENDFUNCTION;
  size: TSize;
  p, S: TPoint;
begin
  ScrDC := CreateCompatibleDC(0);
  MemDC := CreateCompatibleDC(ScrDC);

  FMainBuffer.GetHBITMAP(0, BitmapHandle);
  PrevBitmap := SelectObject(MemDC, BitmapHandle);
  size.cx := Width;
  size.cy := Height;
  p := Point(Left, Top);
  S := Point(0, 0);

  with BlendFunc do
  begin
    BlendOp := AC_SRC_OVER;
    BlendFlags := 0;
    SourceConstantAlpha := 255;
    AlphaFormat := AC_SRC_ALPHA;
  end;

  UpdateLayeredWindow(Handle, ScrDC, @p, @size, MemDC, @S, 0, @BlendFunc,
    ULW_ALPHA);

  SelectObject(MemDC, PrevBitmap);
  DeleteObject(BitmapHandle);

  DeleteDC(MemDC);
  DeleteDC(ScrDC);
end;

procedure TAdvSmoothRotaryMenuForm.UpdateWindow;
begin
  CreateMainBuffer;
  UpdateLayered;
end;

procedure TAdvSmoothRotaryMenuForm.WMActivate(var Message: TMessage);
begin
  inherited;
  Message.Result := 1;
end;

procedure TAdvSmoothRotaryMenuForm.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  inherited;
end;

procedure TAdvSmoothRotaryMenuForm.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if assigned(RotaryMenuDialog) then
    RotaryMenuDialog.Menu.WMGetDlgCode(Message);
end;

procedure TAdvSmoothRotaryMenuForm.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TAdvSmoothRotaryMenuForm.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

procedure TAdvSmoothRotaryMenuForm.WndProc(var Message: TMessage);
begin
  if Assigned(RotaryMenuDialog) and not (csDestroying in ComponentState) then
  begin
    if Message.Msg = WM_DESTROY then
    begin
      if Assigned(RotaryMenuDialog.frm) then
        RotaryMenuDialog.frm.FormHookDone;
    end;
  end;
  inherited;
end;

{ TRotaryMenuItem }

procedure TRotaryMenuItem.Assign(Source: TPersistent);
begin
  if (Source is TRotaryMenuItem) then
  begin
    FToggle := (Source as TRotaryMenuItem).Toggle;
    FHint := (Source as TRotaryMenuItem).Hint;
    FCaption := (Source as TRotaryMenuItem).Caption;
    FEnabled := (Source as TRotaryMenuItem).Enabled;
    FImage.Assign((Source as TRotaryMenuItem).Image);
    FImageIndex := (Source as TRotaryMenuItem).ImageIndex;
    FImageName := (Source as TRotaryMenuItem).ImageName;
    FImageWidth := (Source as TRotaryMenuItem).ImageWidth;
    FImageHeight := (Source as TRotaryMenuItem).ImageHeight;
    FFill.Assign((Source as TRotaryMenuItem).Fill);
    FFont.Assign((Source as TRotaryMenuItem).Font);
    FDownFill.Assign((Source as TRotaryMenuItem).DownFill);
    FDisabledFill.Assign((Source as TRotaryMenuItem).DisabledFill);
    FHoverFill.Assign((Source as TRotaryMenuItem).HoverFill);
    FImageLocation := (Source as TRotaryMenuItem).ImageLocation;
    FCaptionLocation := (Source as TRotaryMenuItem).CaptionLocation;
    FImageLeft := (Source as TRotaryMenuItem).ImageLeft;
    FImageTop := (Source as TRotaryMenuItem).ImageTop;
    FCaptionLeft := (Source as TRotaryMenuItem).CaptionLeft;
    FCaptionTop := (Source as TRotaryMenuItem).CaptionTop;
    FFont.Assign((Source as TRotaryMenuItem).Font);
    FShortCut := (Source as TRotaryMenuItem).ShortCut;
    FReadOnly := (Source as TRotaryMenuItem).ReadOnly;
  end;
end;

procedure TRotaryMenuItem.Changed;
begin
  FOwner.Changed;
end;

constructor TRotaryMenuItem.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TRotaryMenuItems).FOwner;

  FShortCut := 0;
  FReadOnly := False;

  FImageWidth := -1;
  FImageHeight := -1;

  FCaptionLocation := clCenterCenter;
  FImageLocation := clCenterCenter;
  FImageLeft := 0;
  FImageTop := 0;
  FCaptionLeft := 0;
  FCaptionTop := 0;

  FFont := TFont.Create;
  FFont.Name := 'Tahoma';
  FFont.OnChange := FontChanged;

  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FDownFill := TGDIPFill.Create;
  FDownFill.OnChange := FillChanged;
  FHoverFill := TGDIPFill.Create;
  FHoverFill.OnChange := FillChanged;
  FDisabledFill := TGDIPFill.Create;
  FDisabledFill.OnChange := FillChanged;

  FEnabled := true;
  FImage := TAdvGDIPPicture.Create;
  FImage.OnChange := ImageChanged;
  FImageIndex := -1;
  FTag := 0;

  FFont.Assign(FOwner.DefaultFont);
  FFill.Assign(FOwner.DefaultFill);
  FHoverFill.Assign(FOwner.DefaultHoverFill);
  FDownFill.Assign(FOwner.DefaultDownFill);
  FDisabledFill.Assign(FOwner.DefaultDisabledFill);

  FOwner.Changed;
end;

destructor TRotaryMenuItem.Destroy;
begin
  FFont.Free;
  FImage.Free;
  FFill.Free;
  FDownFill.Free;
  FHoverFill.Free;
  FDisabledFill.Free;
  inherited;
  FOwner.Changed;
end;

procedure TRotaryMenuItem.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TRotaryMenuItem.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TRotaryMenuItem.GetMenu: TAdvSmoothRotaryMenu;
begin
  Result := FOwner;
end;

function TRotaryMenuItem.GetStartAngle: Double;
var
  sta, stp: Double;
begin
  sta := FOwner.StartAngle;
  stp := FOwner.StopAngle;
  Result := sta + (stp - sta) / FOwner.Items.Count * Index;
end;

function TRotaryMenuItem.GetStopAngle: Double;
var
  sta, stp: Double;
begin
  sta := FOwner.StartAngle;
  stp := FOwner.StopAngle;
  Result := sta + (stp - sta) / FOwner.Items.Count * (Index + 1);
end;

procedure TRotaryMenuItem.ImageChanged(Sender: TObject);
begin
  Changed;
end;

procedure TRotaryMenuItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetCaptionLeft(const Value: Integer);
begin
  if FCaptionLeft <> Value then
  begin
    FCaptionLeft := Value;
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetCaptionLocation(const Value: TRotaryMenuLocation);
begin
  if FCaptionLocation <> Value then
  begin
    FCaptionLocation := Value;
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetCaptionTop(const Value: Integer);
begin
  if FCaptionTop <> Value then
  begin
    FCaptionTop := Value;
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetDisabledFill(const Value: TGDIPFill);
begin
  if FDisabledFill <> Value then
  begin
    FDisabledFill.Assign(Value);
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetDown(const Value: Boolean);
begin
  if FDown <> Value then
  begin
    FDown := Value;
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetDownFill(const Value: TGDIPFill);
begin
  if FDownFill <> Value then
  begin
    FDownFill.Assign(Value);
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetFont(const Value: TFont);
begin
  if FFont <> Value then
  begin
    FFont.Assign(Value);
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetHint(const Value: String);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetHoverFill(const Value: TGDIPFill);
begin
  if FHoverFill <> Value then
  begin
    FHoverFill.Assign(Value);
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetImage(const Value: TAdvGDIPPicture);
begin
  if FImage <> Value then
  begin
    FImage.Assign(Value);
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetImageHeight(const Value: Integer);
begin
  if FImageHeight <> Value then
  begin
    FImageHeight := Value;
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetImageLeft(const Value: Integer);
begin
  if FImageLeft <> Value then
  begin
    FImageLeft := Value;
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetImageLocation(const Value: TRotaryMenuLocation);
begin
  if FImageLocation <> Value then
  begin
    FImageLocation := Value;
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetImageName(const Value: String);
begin
  if FImageName <> Value then
  begin
    FImageName := Value;
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetImageTop(const Value: Integer);
begin
  if FImageTop <> Value then
  begin
    FImageTop := Value;
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetImageWidth(const Value: Integer);
begin
  if FImageWidth <> Value then
  begin
    FImageWidth := Value;
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetShortCut(const Value: TShortCut);
begin
  if FShortCut <> Value then
  begin
    FShortCut := Value;
    Changed;
  end;
end;

procedure TRotaryMenuItem.SetToggle(const Value: Boolean);
begin
  if FToggle <> Value then
  begin
    FToggle := Value;
    Changed;
  end;
end;

{ TRotaryMenuItems }

function TRotaryMenuItems.Add: TRotaryMenuItem;
begin
  Result := TRotaryMenuItem(inherited Add);
end;

procedure TRotaryMenuItems.Clear;
begin
  if Count > 0 then
  begin
    while Count > 0 do
      TCollectionItem(Items[Count - 1]).Free;
  end;
end;

constructor TRotaryMenuItems.Create(AOwner: TAdvSmoothRotaryMenu);
begin
  inherited Create(TRotaryMenuItem);
  FOwner := AOwner;
end;

procedure TRotaryMenuItems.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TRotaryMenuItems.GetItem(Index: Integer): TRotaryMenuItem;
begin
  result := TRotaryMenuItem( inherited Items[Index]);
end;

function TRotaryMenuItems.GetOwner: TPersistent;
begin
  result := FOwner;
end;

function TRotaryMenuItems.Insert(Index: Integer): TRotaryMenuItem;
begin
  result := TRotaryMenuItem( inherited Insert(Index));
end;

procedure TRotaryMenuItems.SetItem(Index: Integer; const Value: TRotaryMenuItem);
begin
  inherited Items[Index] := Value;
end;

end.
