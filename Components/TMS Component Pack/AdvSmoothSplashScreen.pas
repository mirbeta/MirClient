{***************************************************************************}
{ TAdvSmoothSplashScreen component                                          }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2010 - 2015                                        }
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

unit AdvSmoothSplashScreen;

interface

{$I TMSDEFS.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  GDIPFill, Math, GDIPPictureContainer, ImgList, AdvStyleIF,
  AdvGDIP, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  FADDING_DELAY = 5;
  
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 5; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0  : First release
  // v1.0.0.1  : Fixed : setting focus on parent form upon start
  // v1.0.1.0 : New : Support for Windows Vista and Windows Seven Style
  // v1.0.2.0 : New : Built-in support for reduced color set for use with terminal servers
  // v1.0.3.0 : New : OnClick event
  // v1.0.4.0 : New : TextRendering property to control text rendering
  // v1.0.5.0 : New : function SetFocusToForm
  // v1.0.6.0 : New : Built-in support for Office 2010 colors
  // v1.1.0.0 : New : Extra positions for basic program information
  //          : New : OnBeforeShow event added
  //          : New : Basicprogram information update improvements
  //          : Fixed : Focus Parent form setting properties at designtime
  // v1.1.0.1 : Improved : Property StayOnTop
  // v1.1.1.0 : New : OnClose event
  // v1.1.1.1 : Fixed : Issue with focusing main window after showing splash screen
  // v1.2.0.0 : New : Exposed MainForm property
  //          : New : Public FadeIn(), FadeOut() functions added
  // v1.2.0.1 : Fixed : Issue with Chinese text length calculation
  // v1.3.0.0 : New : Metro Style support
  // v1.3.1.0 : New : Method RefreshProgress added for faster progressbar update during startup
  // v1.3.1.1 : Fixed : Issue with design time ProgressFont config
  // v1.3.2.0 : New : Close method added
  // v1.3.2.1 : New : Shadow and Overlay properties added for progressbar
  // v1.4.0.0 : New : Windows 8, Office 2013 styles added
  // v1.4.0.1 : Fixed : Issue with StayOnTop
  // v1.5.0.0 : New : Windows 10, Office 2016 styles added
  
type
  TAdvSmoothSplashScreen = class;

  TDisplayStyle = (dsNormal, dsFadeInOut);
  TDisplayLocation = (dlFixedPos, dlCenter);
  TItemLocation = (ilTopLeft, ilTopCenter, ilTopRight, ilBottomLeft, ilBottomCenter, ilBottomRight, ilCustom);
  TThreadType = (ttMainThread, ttSeparateThread);
  TAdvSmoothSplashScreenLocation = (cpTopLeft, cpTopCenter, cpTopRight, cpCenterLeft, cpCenterCenter, cpCenterRight, cpBottomLeft, cpBottomCenter, cpBottomRight, cpCustom);
  TAdvSmoothSplashScreenTextRenderingHint = (tAntiAlias, tAntiAliasGridFit, tClearType);

  //--- ProgressBar
  TGDIPSplashProgress = class(TPersistent)
  private
    FMetroStyle: Boolean;
    FBackGroundFill: TGDIPFill;
    FValueFormat: String;
    FValueType: TGDIPProgressValueType;
    FProgressFill: TGDIPFill;
    FValuePositionTop: integer;
    FValuePositionLeft: integer;
    FFont: TFont;
    FValuePosition: TGDIPProgressValuePosition;
    FOnChange: TNotifyEvent;
    FValueVisible: Boolean;
    FProgressFont: TFont;
    FVisible: Boolean;
    FHeight: Integer;
    FLeft: Integer;
    FWidth: Integer;
    FTop: Integer;
    FProgressAnimation: Boolean;
    FGlowAnimation: Boolean;
    FPosition: Double;
    FStep: Double;
    FMaximum: Double;
    FMinimum: Double;
    FShadows: Boolean;
    FOverlays: Boolean;
    procedure SetBackGroundFill(const Value: TGDIPFill);
    procedure SetFont(const Value: TFont);
    procedure SetProgressFill(const Value: TGDIPFill);
    procedure SetValueFormat(const Value: String);
    procedure SetValuePosition(const Value: TGDIPProgressValuePosition);
    procedure SetValuePositionLeft(const Value: integer);
    procedure SetValuePositionTop(const Value: integer);
    procedure SetValueType(const Value: TGDIPProgressValueType);
    procedure SetValueVisible(const Value: Boolean);
    procedure SetProgressFont(const Value: TFont);
    procedure SetVisible(const Value: Boolean);
    procedure SetGlowAnimation(const Value: Boolean);
    procedure SetMaximum(const Value: Double);
    procedure SetMinimum(const Value: Double);
    procedure SetPosition(const Value: Double);
    procedure SetProgressAnimation(const Value: Boolean);
    procedure SetStep(const Value: Double);
    procedure SetHeight(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetOverlays(const Value: Boolean);
    procedure SetShadows(const Value: Boolean);
  protected
    procedure FontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure Changed;
    procedure DrawBackGround(g: TGPGraphics; r: TGPRectF);
    procedure DrawShadows(g: TGPGraphics; r: TGPRectF);
    procedure DrawValue(g: TGPGraphics; r: TRect; min, max, position: double);
    procedure DrawProgress(g: TGPGraphics; r: TRect; min, max, position: Double);
    procedure DrawOverLay(g: TGPGraphics; r: TRect);
    procedure DrawGlow(g: TGPGRaphics; r: TRect; glowposition, min, max, position: Double);
    function InsideRect(r: TRect): TRect;
    procedure GetTextSize(g: TGPGraphics; r: TRect; s: String; ft: TFont; var sizer: TGPRectF);

    property GlowAnimation: Boolean read FGlowAnimation write SetGlowAnimation default False;
    property ProgressAnimation: Boolean read FProgressAnimation write SetProgressAnimation default False;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(g: TGPGraphics; r: TRect; min, max, position: Double);
    function GetInsideRectF(r: TRect): TGPRectF;
    function CalculateProgressRectangle(r: TRect; min, max, pos: Double): TGPRectF;
  published
    property Shadows: Boolean read FShadows write SetShadows default True;
    property Overlays: Boolean read FOverlays write SetOverlays default True;
    property BackGroundFill: TGDIPFill read FBackGroundFill write SetBackGroundFill;
    property ProgressFill: TGDIPFill read FProgressFill write SetProgressFill;
    property Font: TFont read FFont write SetFont;
    property ProgressFont: TFont read FProgressFont write SetProgressFont;
    property ValueFormat: String read FValueFormat write SetValueFormat;
    property ValueType: TGDIPProgressValueType read FValueType write SetValueType default vtAbsolute;
    property ValuePosition: TGDIPProgressValuePosition read FValuePosition write SetValuePosition default vpCenter;
    property ValuePositionLeft: integer read FValuePositionLeft write SetValuePositionLeft default 0;
    property ValuePositionTop: integer read FValuePositionTop write SetValuePositionTop default 0;
    property ValueVisible: Boolean read FValueVisible write SetValueVisible default false;

    property Visible: Boolean read FVisible write SetVisible default False;
    property Left: Integer read FLeft write SetLeft default 50;
    property Top: Integer read FTop write SetTop default 100;
    property Width: Integer read FWidth write SetWidth default 300;
    property Height: Integer read FHeight write SetHeight default 17;
    property Step: Double read FStep write SetStep;
    property Minimum: Double read FMinimum write SetMinimum;
    property Maximum: Double read FMaximum write SetMaximum;
    property Position: Double read FPosition write SetPosition;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothProgressBarPositionChanged = procedure(Sender: TObject; Value: Double) of object;

  TAdvSplashProgressBar = class(TGraphicControl)
  private
    FPositionTo, FPositionTemp: double;
    FRect: TRect;
    FDesignTime: Boolean;
    FGlowCount: integer;
    FGlowPos: Double;
    FAnimationTimer: TTimer;
    FMaximum: Double;
    FAppearance: TGDIPSplashProgress;
    FGlowAnimation: Boolean;
    FOnChange: TNotifyEvent;
    FMinimum: Double;
    FProgressAnimation: Boolean;
    FStep: Double;
    FPosition: Double;
    FOnPositionChanged: TAdvSmoothProgressBarPositionChanged;
    procedure SetAppearance(const Value: TGDIPSplashProgress);
    procedure SetGlowAnimation(const Value: Boolean);
    procedure SetMaximum(const Value: Double);
    procedure SetMinimum(const Value: Double);
    procedure SetProgressAnimation(const Value: Boolean);
    procedure SetStep(const Value: Double);
    procedure SetPosition(const Value: Double);
    function GetVersion: String;
    procedure SetVersion(const Value: String);
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure AppearanceChanged(Sender: TObject);
    procedure AnimateProgress(Sender: TObject);
    procedure DrawGlow(g: TGPGRaphics; r: TRect);
    procedure SetStyle(AStyle: TTMSStyle; Selected: Boolean);
    function IsProgressAnimation: Boolean;
    function IsGlowAnimation: Boolean;
    function GetVersionNr: integer;    
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Next; overload;
    procedure Previous; overload;
    procedure Next(AStep: Double); overload;
    procedure Previous(AStep: Double); overload;
    procedure GoToValue(AValue: Double);
    procedure GoToEnd;
    procedure GoToStart;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetColorTones(ATones: TColorTones);
    function AnimationInProgress: Boolean;
  published
    property Step: Double read FStep write SetStep;
    property Minimum: Double read FMinimum write SetMinimum;
    property Maximum: Double read FMaximum write SetMaximum;
    property Position: Double read FPosition write SetPosition;
    property GlowAnimation: Boolean read FGlowAnimation write SetGlowAnimation default False;
    property ProgressAnimation: Boolean read FProgressAnimation write SetProgressAnimation default False;
    property Appearance: TGDIPSplashProgress read FAppearance write SetAppearance;
    property Version: String read GetVersion write SetVersion;
    
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnPositionChanged: TAdvSmoothProgressBarPositionChanged read FOnPositionChanged write FOnPositionChanged;

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
  end;
  //---

  TOnDeleteItemEvent = procedure(Sender: TObject; Index: integer) of object;
  
  TSplashItem = class(TCollectionItem)
  private
    FText: String;
    FImageIndex: Integer;
    FTag: integer;
    FIPicture: TAdvGDIPPicture;
    FPosY: Integer;
    FPosX: Integer;
    FFont: TFont;
    FVisible: Boolean;
    FUpdateCount: Integer;
    procedure SetImageIndex(const Value: Integer);
    procedure SetText(const Value: String);
    procedure SetTag(const Value: integer);
    procedure SetPicture(const Value: TAdvGDIPPicture);
    procedure OnPictureChanged(Sender: TObject);
    procedure SetPosX(const Value: Integer);
    procedure SetPosY(const Value: Integer);
    procedure SetFont(const Value: TFont);
    procedure OnFontChanged(Sender: TObject);
  protected
    procedure Changed;
    property Visible: Boolean read FVisible write FVisible default True;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Refresh(Fast: Boolean = False);
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property Text: String read FText write SetText;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Tag: integer read FTag write SetTag default 0;
    property Picture: TAdvGDIPPicture read FIPicture write SetPicture;
    property PosX: Integer read FPosX write SetPosX default 80;
    property PosY: Integer read FPosY write SetPosY default 130;
    property Font: TFont read FFont write SetFont;
  end;

  TSplashItems = class(TCollection)
  private
    FOwner: TComponent;
    FOnChange: TNotifyEvent;
    FOnDeleteItem: TOnDeleteItemEvent;
    function GetItem(Index: Integer): TSplashItem;
    procedure SetItem(Index: Integer; const Value: TSplashItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TComponent);
    property Items[Index: Integer]: TSplashItem read GetItem write SetItem; default;
    function Add: TSplashItem;
    function Insert(Index: Integer): TSplashItem;
    function GetOwner: TPersistent; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDeleteItem: TOnDeleteItemEvent read FOnDeleteItem write FOnDeleteItem;
  end;

  TListItemsRect = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FHeight: Integer;
    FLeft: Integer;
    FWidth: Integer;
    FTop: Integer;
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    function GetRect: TRect;
  published
    property Left: Integer read FLeft write FLeft default 30;
    property Top: Integer read FTop write FTop default 150;
    property Width: Integer read FWidth write FWidth default 300;
    property Height: Integer read FHeight write FHeight default 200;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TSplashListItem = class(TCollectionItem)
  private
    FHTMLText: String;
    FImageIndex: Integer;
    FTag: integer;
    FDisplayRect: TRect;
    FUpdateCount: Integer;
    procedure SetImageIndex(const Value: Integer);
    procedure SetHTMLText(const Value: String);
    procedure SetTag(const Value: integer);
  protected
    property DisplayRect: TRect read FDisplayRect write FDisplayRect;
    procedure Change;

    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;  // TODO: add it using GDIP
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Refresh;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property HTMLText: String read FHTMLText write SetHTMLText;
    property Tag: integer read FTag write SetTag default 0;
  end;

  TSplashListItems = class(TCollection)
  private
    FOwner: TComponent;
    FOnChange: TNotifyEvent;
    FOnDeleteItem: TOnDeleteItemEvent;
    function GetItem(Index: Integer): TSplashListItem;
    procedure SetItem(Index: Integer; const Value: TSplashListItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TComponent);
    property Items[Index: Integer]: TSplashListItem read GetItem write SetItem; default;
    function Add: TSplashListItem;
    function Insert(Index: Integer): TSplashListItem;
    function GetOwner: TPersistent; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDeleteItem: TOnDeleteItemEvent read FOnDeleteItem write FOnDeleteItem;
  end;

  TSplashScreenWindow = class(TForm)
  private
    FSplashScreen: TAdvSmoothSplashScreen;
    FSplashProgressBar: TAdvSplashProgressBar;
    FForcedTransparent: Boolean;
    FMainBuffer: TGPBitmap;
    FOpacity: byte;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    procedure SetForcedTransparent(const Value: Boolean);
  protected
    function GetParentWnd: HWnd;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Activate; override;
    procedure DoCreate; override;
    procedure DoDestroy; override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure DoHide; override;
    procedure DoShow; override;
    procedure Click; override;

    procedure CreateMainBuffer;
    procedure DestroyMainBuffer;
    function CreateGraphics: TGPGraphics;
    //---- Layered window
    procedure SetLayeredWindow;
    procedure UpdateLayered;
    procedure UpdateMainWindow;
    procedure ClearBuffer(graphics: TGPGraphics);
    //----

    function GetListItemSize(ListItem: TSplashListItem; graphic: TGPGraphics = nil): TRect;
    procedure DrawListItems(graphics: TGPGraphics);
    procedure DrawItems(graphics: TGPGraphics);
    procedure DrawItem(graphics: TGPGraphics; Item: TSplashItem);
    procedure DrawBackGround(graphics: TGPGraphics);
    procedure DrawBasicInfo(graphics: TGPGraphics);
    procedure DrawTopLayerItems;
    procedure DrawProgressBar(graphics: TGPGraphics);

    function GetMyClientRect: TRect;
    function InsideRect: TRect;
    function GetDisplayRectangle: TRect;
    function GetShadowOffset: integer;
    procedure InflatRounding(Rounding: Integer; var R: TRect);

    procedure SetRounding;
    procedure UpdateWindow;
    procedure HideEx;

    procedure FadeIn(const Step, Wait, Max: Integer);
    procedure FadeOut(const Step, Wait, Min: Integer);

    property SplashScreen: TAdvSmoothSplashScreen read FSplashScreen write FSplashScreen;
    property ForcedTransparent: Boolean read FForcedTransparent write SetForcedTransparent;
  public
    procedure Refresh;
    procedure RefreshProgress;
  end;

  TSplashScreenThread = class(TThread)
  private
    FAdvSplashScreen: TAdvSmoothSplashScreen;
  protected
    procedure Execute; override;
  public
    constructor Create(SplashScreen: TAdvSmoothSplashScreen);
  end;

  TSplashHeadingText = class(TPersistent)
  private
    FStartOpacity: Byte;
    FEndOpacity: Byte;
    FPosY: Integer;
    FPosX: Integer;
    FText: String;
    FPicture: TAdvGDIPPicture;
    FGradientType: TAdvGradientType;
    FEndColor: TColor;
    FStartColor: TColor;
    FFont: TFont;
    FHatchStyle: THatchStyle;
    FLocation: TItemLocation;
    FOnChange: TNotifyEvent;
    FAngle: Integer;
    procedure SetPicture(const Value: TAdvGDIPPicture);
    procedure SetFont(const Value: TFont);
    procedure SetText(const Value: String);
  protected
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Angle: Integer read FAngle write FAngle default 0;
    property Text: String read FText write SetText;
    property Font: TFont read FFont write SetFont;
    property Location: TItemLocation read FLocation write FLocation default ilTopLeft;
    property PosX: Integer read FPosX write FPosX default 10;
    property PosY: Integer read FPosY write FPosY default 100;
    property HatchStyle: THatchStyle read FHatchStyle write FHatchStyle default HatchStyleHorizontal;
    property Picture: TAdvGDIPPicture read FPicture write SetPicture;
    property ColorStart: TColor read FStartColor write FStartColor default clBlack;
    property ColorEnd: TColor read FEndColor write FEndColor default clNone;
    property OpacityStart: Byte read FStartOpacity write FStartOpacity default 255;
    property OpacityEnd: Byte read FEndOpacity write FEndOpacity default 255;
    property GradientType: TAdvGradientType read FGradientType write FGradientType default gtVertical;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TBasicProgramInfo = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FAutoLoad: Boolean;
    FCopyRightFont: TFont;
    FCopyRightPosX: Integer;
    FCopyRightPosY: Integer;
    FCopyRight: String;
    FProgramName: TSplashHeadingText;
    FProgramVersion: TSplashHeadingText;
    FCopyRightLocation: TItemLocation;
    procedure SetProgramName(const Value: TSplashHeadingText);
    procedure SetProgramVersion(const Value: TSplashHeadingText);
    procedure SetCopyRightFont(const Value: TFont);
    procedure ProgramChanged(Sender: TObject);
  protected
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AutoLoad: Boolean read FAutoLoad write FAutoLoad default True;
    property ProgramName: TSplashHeadingText read FProgramName write SetProgramName;
    property ProgramVersion: TSplashHeadingText read FProgramVersion write SetProgramVersion;
    property CopyRight: String read FCopyRight write FCopyRight;
    property CopyRightFont: TFont read FCopyRightFont write SetCopyRightFont;
    property CopyRightLocation: TItemLocation read FCopyRightLocation write FCopyRightLocation default ilTopLeft;
    property CopyRightPosX: Integer read FCopyRightPosX write FCopyRightPosX default 10;
    property CopyRightPosY: Integer read FCopyRightPosY write FCopyRightPosY default 130;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TSplashListItemsSettings = class(TPersistent)
  private
    FSpace: Integer;
    FHTMLShadowOffset: integer;
    FHTMLURLColor: TColor;
    FHTMLShadowColor: TColor;
    FHTMLFont: TFont;
    FRect: TListItemsRect;
    FOnChange: TNotifyEvent;
    FOwner: TAdvSmoothSplashScreen;
    procedure SetHTMLFont(const Value: TFont);
    procedure SetHTMLShadowColor(const Value: TColor);
    procedure SetHTMLShadowOffset(const Value: integer);
    procedure SetHTMLURLColor(const Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Rect: TListItemsRect read FRect write FRect;
    property Space: Integer read FSpace write FSpace default 8;
    property HTMLFont: TFont read FHTMLFont write SetHTMLFont;
    property HTMLURLColor: TColor read FHTMLURLColor write SetHTMLURLColor default clBlue;
    property HTMLShadowColor: TColor read FHTMLShadowColor write SetHTMLShadowColor default clGray;
    property HTMLShadowOffset: integer read FHTMLShadowOffset write SetHTMLShadowOffset default 5;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSplashHTMLText = class(TPersistent)
  private
    FDisableRepaint: Boolean;
    FOwner: TAdvSmoothSplashScreen;
    FURLColor: TColor;
    FShadowOffset: integer;
    FFont: TFont;
    FText: string;
    FShadowColor: TColor;
    FOnChange: TNotifyEvent;
    FLocation: TAdvSmoothSplashScreenLocation;
    FTop: integer;
    FLeft: integer;
    procedure SetFont(const Value: TFont);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: integer);
    procedure SetText(const Value: string);
    procedure SetURLColor(const Value: TColor);
    procedure SetLocation(const Value: TAdvSmoothSplashScreenLocation);
    procedure SetLeft(const Value: integer);
    procedure SetTop(const Value: integer);
  protected
    procedure Changed;
    procedure FontChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothSplashScreen);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Text: string read FText write SetText;
    property Location: TAdvSmoothSplashScreenLocation read FLocation write SetLocation default cpCenterLeft;
    property Top: integer read FTop write SetTop default 0;
    property Left: integer read FLeft write SetLeft default 0;
    property URLColor: TColor read FURLColor write SetURLColor default clBlue;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property ShadowOffset: integer read FShadowOffset write SetShadowOffset default 5;
    property Font: TFont read FFont write SetFont;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;    
  end;

  TAdvSplashTopLayerItem = class(TCollectionItem)
  private
    FOwner: TAdvSmoothSplashScreen;
    FAlign: TAlign;
    FHTMLText: TAdvSplashHTMLText;
    FWidth: integer;
    FVisible: Boolean;
    FTop: integer;
    FHeight: integer;
    FLeft: integer;
    FFill: TGDIPFill;
    procedure SetAlign(const Value: TAlign);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetHeight(const Value: integer);
    procedure SetHTMLText(const Value: TAdvSplashHTMLText);
    procedure SetLeft(const Value: integer);
    procedure SetTop(const Value: integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: integer);
  protected
    procedure Changed;
    procedure HTMLTextChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure DrawHTMLText(g: TGPGraphics; HTML: TAdvSplashHTMLText; R: TGPRectF);
    procedure Draw(g: TGPGraphics; r: TGPRectF); overload;
    procedure Draw(g: TGPGraphics; r: TRect); overload;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default true;
    property Top: integer read FTop write SetTop default 0;
    property Left: integer read FLeft write SetLeft default 0;
    property HTMLText: TAdvSplashHTMLText read FHTMLText write SetHTMLText;
    property Fill: TGDIPFill read FFill write SetFill;
    property Align: TAlign read FAlign write SetAlign default alCustom;
    property Width: integer read FWidth write SetWidth default 100;
    property Height: integer read FHeight write SetHeight default 100;
  end;

  TAdvSplashTopLayerItems = class(TCollection)
  private
    FOwner: TAdvSmoothSplashScreen;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvSplashTopLayerItem;
    procedure SetItem(Index: Integer; const Value: TAdvSplashTopLayerItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvSmoothSplashScreen);
    function Add: TAdvSplashTopLayerItem;
    function Insert(Index: Integer): TAdvSplashTopLayerItem;
    property Items[Index: Integer]: TAdvSplashTopLayerItem read GetItem write SetItem; default;
    procedure Delete(Index: Integer);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothSplashScreen = class(TComponent, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FAutoShow: Boolean;
    FCloseOnTimeout: Boolean;
    FCloseOnMainFormShow: Boolean;
    FTimeOut: Integer;
    FDisplayLocation: TDisplayLocation;
    FDisplayStyle: TDisplayStyle;
    FProgressBar: TGDIPSplashProgress;
    FImages: TCustomImageList;
    FItems: TSplashItems;
    FDisplayPosY: Integer;
    FDisplayPosX: Integer;
    FDisplayThread: TSplashScreenThread;
    FShowing: Boolean;
    FSplashWindow: TSplashScreenWindow;
    FOldMainFormShow: TNotifyEvent;
    FHeight: Integer;
    FWidth: Integer;
    FCloseTimer: TTimer;
    FThreadType: TThreadType;
    FMainForm: TForm;
    FPictureGap: Integer;
    FListItems: TSplashListItems;
    FBasicProgramInfo: TBasicProgramInfo;
    FOnProgressPositionChanged: TAdvSmoothProgressBarPositionChanged;
    FOnProgressChange: TNotifyEvent;
    FUpdateCount: Integer;
    FFill: TGDIPFill;
    FListItemsSettings: TSplashListItemsSettings;
    FContainer: TGDIPPictureContainer;
    FTopLayerItems: TAdvSplashTopLayerItems;
    FMaxOpacity: byte;
    FAutoAdaptPictureSize: Boolean;
    FFocusTimer: TTimer;
    FFocusParentForm: Boolean;    // Timer used to force focus to main form
    FFocusFormTimer: TTimer;
    FNewFocusForm: TCustomForm;
    FOnClick: TNotifyEvent;
    FTextRendering: TAdvSmoothSplashScreenTextRenderingHint;
    FOnBeforeShow: TNotifyEvent;
    FStayOnTop: Boolean;
    FOnClose: TNotifyEvent;    // Timer used to force focus to main form
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure OnFillChanged(Sender: TObject);
    procedure OnProgressBarChnaged(Sender: TObject);
    procedure TopLayerItemsChanged(Sender: TObject);
    procedure OnItemsChanged(Sender: TObject);
    procedure OnListItemsChanged(Sender: TObject);
    procedure OnBasicProgramInfoChange(Sender: TObject);
    procedure OnProgressBarPositionChange(Sender: TObject; Value: Double);
    procedure OnProgressBarChange(Sender: TObject);
    procedure OnMainFormShow(Sender: TObject);
    procedure OnFocusTimerTime(Sender: TObject);
    procedure OnCloseTimerTime(Sender: TObject);
    procedure OnDisplayThreadTerminate(Sender: TObject);
    procedure OnFocusFormTimerTime(Sender: TObject);
    procedure SetProgressBar(const Value: TGDIPSplashProgress);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetItems(const Value: TSplashItems);
    procedure SetListItems(const Value: TSplashListItems);
    procedure SetBasicProgramInfo(const Value: TBasicProgramInfo);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetListItemsSettings(const Value: TSplashListItemsSettings);
    procedure SetTopLayerItems(const Value: TAdvSplashTopLayerItems);
    procedure ForciblySetFocus(AForm: TCustomForm);
    procedure SetTextRendering(
      const Value: TAdvSmoothSplashScreenTextRenderingHint);
    procedure SetStayOnTop(const Value: Boolean);
    procedure SetMainForm(const Value: TForm);
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DestroySplashScreen;
    procedure LoadBasicInfo;
    procedure RefreshItem(Item: TSplashItem; Fast: Boolean = False);
    procedure RefreshRect(R: TRect; Fast: Boolean = False);
    procedure Changed;

    property ThreadType: TThreadType read FThreadType write FThreadType;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    
    procedure Show(X, Y: Integer); overload;
    procedure Show; overload;
    procedure Hide;
    procedure Refresh;
    procedure RefreshProgress;
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure SetFocusToForm(AForm: TCustomForm);

    property SplashWindow: TSplashScreenWindow read FSplashWindow;
    property Showing: Boolean read FShowing;
    property FocusParentForm: Boolean read FFocusParentForm write FFocusParentForm default False;
    property MainForm: TForm read FMainForm write SetMainForm;

    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeID: String;
    procedure FadeIn(const Step, Wait, Max: Integer);
    procedure FadeOut(const Step, Wait, Min: Integer);
    procedure Close;
  published
    property StayOnTop: Boolean read FStayOnTop write SetStayOnTop default true;
    property Version: string read GetVersion write SetVersion;
    property AutoShow: boolean read FAutoShow write FAutoShow default true;
    property AutoAdaptPictureSize: Boolean read FAutoAdaptPictureSize write FAutoAdaptPictureSize default True;
    property BasicProgramInfo: TBasicProgramInfo read FBasicProgramInfo write SetBasicProgramInfo;
    property CloseOnMainFormShow: Boolean read FCloseOnMainFormShow write FCloseOnMainFormShow default True;
    property CloseOnTimeout: Boolean read FCloseOnTimeout write FCloseOnTimeout default False; //(with a Timeout integer)
    property Fill: TGDIPFill read FFill write SetFill;
    property TimeOut: Integer read FTimeOut write FTimeOut default 1000;
    property ProgressBar: TGDIPSplashProgress read FProgressBar write SetProgressBar;
    property Items: TSplashItems read FItems write SetItems;
    property PictureGap: Integer read FPictureGap write FPictureGap default 5;
    property ListItems: TSplashListItems read FListItems write SetListItems;
    property ListItemsSettings: TSplashListItemsSettings read FListItemsSettings write SetListItemsSettings;
    property PictureContainer: TGDIPPictureContainer read FContainer write FContainer;
    property DisplayStyle: TDisplayStyle read FDisplayStyle write FDisplayStyle default dsFadeInOut;
    property DisplayLocation: TDisplayLocation read FDisplayLocation write FDisplayLocation default dlCenter;
    property DisplayPosX: Integer read FDisplayPosX write FDisplayPosX default 0;
    property DisplayPosY: Integer read FDisplayPosY write FDisplayPosY default 0;
    property Width: Integer read FWidth write FWidth default 600;
    property Height: Integer read FHeight write FHeight default 400;
    property Images: TCustomImageList read FImages write SetImages;
    property TopLayerItems: TAdvSplashTopLayerItems read FTopLayerItems write SetTopLayerItems;
    property MaxOpacity: byte read FMaxOpacity write FMaxOpacity default 255;
    property TextRendering: TAdvSmoothSplashScreenTextRenderingHint read FTextRendering write SetTextRendering default tClearType;

    //--- ProgressBar Events
    property OnProgressChange: TNotifyEvent read FOnProgressChange write FOnProgressChange;
    property OnProgressPositionChanged: TAdvSmoothProgressBarPositionChanged read FOnProgressPositionChanged write FOnProgressPositionChanged;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnBeforeShow: TNotifyEvent read FOnBeforeShow write FOnBeforeShow;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;


implementation
uses
  CommCtrl, ShellAPI, ActiveX;

{$i GDIPHTMLEngine.pas}

//------------------------------------------------------------- ShowSplashScreen

function ShowSplashScreen(SplashScreen: TAdvSmoothSplashScreen): TSplashScreenWindow;
var
  i, MaxOpacity, MinOpacity, bw: Integer;
  ForcedTransparent: Boolean;
begin
  Result := nil;

  if not Assigned(SplashScreen) or (SplashScreen.FShowing) then
    Exit;

  ForcedTransparent := (SplashScreen.Fill.Opacity < 255) or (SplashScreen.Fill.OpacityTo < 255);
  MaxOpacity := SplashScreen.MaxOpacity;
  MinOpacity := 5;

  SplashScreen.FSplashWindow := TSplashScreenWindow.CreateNew(Application);
  Result := SplashScreen.FSplashWindow;
  Result.Visible := False;
  Result.SplashScreen := SplashScreen;
  Result.BorderIcons := [];
  Result.BorderStyle := bsNone;
  Result.Ctl3D := false;
  if SplashScreen.StayOnTop then
    Result.FormStyle := fsStayOnTop;
  Result.Color := clWhite;

  if SplashScreen.Fill.BorderWidth = 1 then
    bw := 1
  else
    bw := (SplashScreen.Fill.BorderWidth + 1) div 2;

  if SplashScreen.AutoAdaptPictureSize and not SplashScreen.Fill.Picture.Empty then
  begin
    Result.Width := SplashScreen.Fill.Picture.Width + bw * 2;
    Result.Height := SplashScreen.Fill.Picture.Height + bw * 2;
  end
  else
  begin
    Result.Width := SplashScreen.Width;
    Result.Height := SplashScreen.Height;
  end;
  Result.DoubleBuffered := True;
  if (SplashScreen.DisplayLocation = dlFixedPos) then
  begin
    Result.Position := poDesigned;
    Result.Left := SplashScreen.DisplayPosX;
    Result.Top := SplashScreen.DisplayPosY;
  end
  else // (SplashScreen.DisplayLocation = dlCenter) then
  begin
    Result.Position := poScreenCenter;
  end;

  //if SplashScreen.ProgressBar.Visible then
  begin
    Result.FSplashProgressBar := TAdvSplashProgressBar.Create(Result);
    Result.FSplashProgressBar.Parent := Result;
    Result.FSplashProgressBar.Visible := False;
    Result.FSplashProgressBar.Appearance.Assign(SplashScreen.ProgressBar);
    Result.FSplashProgressBar.OnPositionChanged := SplashScreen.OnProgressBarPositionChange;
    Result.FSplashProgressBar.OnChange := SplashScreen.OnProgressBarChange;
  end;

  Result.UpdateWindow;

  if (SplashScreen.DisplayStyle = dsFadeInOut) then
  begin
    if ForcedTransparent then
      Result.FOpacity := MinOpacity
    else
    begin
      Result.AlphaBlend := True;
      Result.AlphaBlendValue := 5;
    end;
  end
  else
    Result.FOpacity := 255;

  Result.Visible := True;
  SplashScreen.FShowing := True;
  Result.SetRounding;

  Result.ForcedTransparent := ForcedTransparent;
  Result.UpdateWindow;
  if not Result.ForcedTransparent then
    Result.RePaint;

  if not ForcedTransparent then
  begin
    if (SplashScreen.DisplayStyle = dsFadeInOut) then
    begin
      i := 5;
      while (Result.AlphaBlendValue < MaxOpacity) do
      begin
        i := Min(MaxOpacity - Result.AlphaBlendValue, i);
        Result.AlphaBlendValue := Result.AlphaBlendValue + i;
        Sleep(FADDING_DELAY);
      end;
    end;
    Result.AlphaBlendValue := MaxOpacity;
  end
  else if (SplashScreen.DisplayStyle = dsFadeInOut) then
  begin
    Result.FadeIn(20, FADDING_DELAY, 255);
  end;

  SplashScreen.FCloseTimer.Interval := SplashScreen.TimeOut;
  SplashScreen.FCloseTimer.Enabled := SplashScreen.CloseOnTimeout;
end;

//------------------------------------------------------------------------------

procedure GetObjectLocation(var x, y: integer; rectangle: TGPRectF; objectwidth, objectheight: integer; location: TAdvSmoothSplashScreenLocation);
var
  w, h, tw, th: integer;
begin
  tw := objectwidth;
  th := objectheight;
  w := Round(rectangle.Width);
  h := Round(rectangle.Height);
  case location of
    cpTopLeft:
    begin
      x := 0;
      y := 0;
    end;
    cpTopRight:
    begin
      x := w - tw;
      y := 0;
    end;
    cpBottomLeft:
    begin
      x := 0;
      y := h - th;
    end;
    cpBottomRight:
    begin
      x := w - tw;
      y := h - th;
    end;
    cpTopCenter:
    begin
      x := (w - tw) div 2;
      y := 0;
    end;
    cpBottomCenter:
    begin
      x := (w - tw) div 2;
      y := h - th;
    end;
    cpCenterCenter:
    begin
      x := (w - tw) div 2;
      y := (h - th) div 2;
    end;
    cpCenterLeft:
    begin
      x := 0;
      y := (h - th) div 2;
    end;
    cpCenterRight:
    begin
      x := w - tw;
      y := (h - th) div 2;
    end;
  end;

  x := x + Round(rectangle.X);
  y := y + Round(rectangle.Y);
end;

//------------------------------------------------------------------------------

function DrawGDIPText(g: TGPGraphics; Text: string; P: TPoint; Font: TFont; GradType: TAdvGradientType; Pic: TAdvGDIPPicture; HatchStyle: THatchStyle; OpacityStart, OpacityEnd: Byte; ColorStart, ColorEnd: TColor; CalcSize: Boolean; Angle: integer; TextRendering: TAdvSmoothSplashScreenTextRenderingHint): TSize;
var
  ff: TGPFontFamily;
  fs, x, y: Integer;
  sf: TGPStringFormat;
  f: TGPFont;
  sizeRect, rectf: TGPRectF;
  xs, ys, textw, texth: Single;
  gppointf: TGPPointF;
  gpimg: TGPImage;
  textb: TGPBrush;
  st: TStream;
  sta: TStreamAdapter;
  start, stop: TGPColor;
begin
  Result.cx := 0;
  Result.cy := 0;
  if (Text <> '') and Assigned(g) then
  begin
    ff := TGPFontFamily.Create(Font.Name);
    if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      ff.Free;
      ff := TGPFontFamily.Create('Arial');
    end;

    case TextRendering of
      tAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
      tAntiAliasGridFit: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
      tClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    end;

    fs := 0;
    if (fsBold in Font.Style) then
      fs := fs + 1;
    if (fsItalic in Font.Style) then
      fs := fs + 2;
    if (fsUnderline in Font.Style) then
      fs := fs + 4;

    sf := TGPStringFormat.Create;
    sf.SetAlignment(StringAlignmentNear);
    f := TGPFont.Create(ff, Font.Size , fs, UnitPoint);
    sizerect.X := 1;
    sizerect.Y := 1;
    sizerect.Width := 0;
    sizerect.Height := 0;
    rectf := MakeRect(P.X, P.Y, 1500, 500);
    g.MeasureString(Text, Length(Text), f, rectf, sf, sizerect);

    Result.cx := Round(sizerect.Width);
    Result.cy := Round(sizerect.Height);

    if CalcSize then
    begin
      sf.Free;
      ff.Free;
      f.Free;
      Exit;
    end;
      
    x := P.X;
    y := P.Y;
    xs := x;
    ys := y;
    textw := sizeRect.Width;
    texth := sizeRect.Height;
    textb := nil;
    start := MakeColor(OpacityStart, ColorStart);
    stop := MakeColor(OpacityEnd, ColorEnd);

    if (ColorEnd = clNone) then
    begin
      if not (GradType in [gtTexture, gtNone]) then
        GradType := gtSolid;
    end;

    gppointf := MakePoint(xs, ys);
    sizeRect.X := gppointf.X;
    sizeRect.Y := gppointf.Y;

    case GradType of
      gtSolid: textb := TGPSolidBrush.Create(start);
      gtVertical: textb := TGPLinearGradientBrush.Create(MakePoint(xs, ys), MakePoint(xs, ys + texth), start, stop);
      gtHorizontal: textb := TGPLinearGradientBrush.Create(MakePoint(xs, ys), MakePoint(xs + textw, ys), start, stop);
      gtForwardDiagonal: textb := TGPLinearGradientBrush.Create(MakePoint(xs, ys), MakePoint(xs + textw, ys + texth), start, stop);
      gtBackwardDiagonal: textb := TGPLinearGradientBrush.Create(MakePoint(xs, ys + texth), MakePoint(xs + textw, ys), stop, start);
      gtHatch: textb := TGPHatchBrush.Create(HatchStyle, start, stop);
      gtTexture:
      begin
        if not Pic.Empty then
        begin
          st := TMemoryStream.Create;
          Pic.SaveToStream(st);
          sta := TStreamAdapter.Create(st);
          gpimg := TGPImage.Create(sta);
          textb := TGPTextureBrush.Create(gpimg, WrapModeTile);
          g.DrawString(Text, Length(Text), f, MakeRect(gppointf.X, gppointf.Y, Round(textw + 10), Texth + 10), sf, textb);
          st.free;
          gpimg.free;
          //sta.Free;
        end;
      end;
      gtAngle:
      begin
        textb := TGPLinearGradientBrush.Create(MakeRect(gppointf.X, gppointf.Y, Round(textw + 10), Texth + 10), MakeColor(OpacityStart, ColorStart),
                 MakeColor(OpacityEnd, ColorEnd), Angle, true);
        TGPLinearGradientBrush(textb).SetGammaCorrection(true);
      end;
      gtNone: ;
    end;

    if (GradType <> gtTexture) then
      g.DrawString(Text, Length(Text), f, MakeRect(gppointf.X, gppointf.Y, Round(Textw + 10), TextH + 10), sf, textb);

    if textb <> nil then
      textb.free;
      
    sf.Free;
    ff.Free;
    f.Free;
  end;    
end;

//------------------------------------------------------------------------------

procedure DrawGDIPImage(graphics: TGPGraphics; P: TPoint; Pic: TAdvGDIPPicture);
var
  Img: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  ImageAttributes: TGPImageAttributes;
  r, g, b: byte;
  GPBmp: TGPBitmap;
  Aclr: TGPColor;
  hr: HResult;
begin
  ms := TMemoryStream.Create;
  pic.SaveToStream(ms);
  hGlobal := GlobalAlloc(GMEM_MOVEABLE, ms.Size);
  if (hGlobal = 0) then
  begin
    ms.Free;
    raise Exception.Create('Could not allocate memory for image');
  end;

  pstm := nil;
  pcbWrite := 0;

  // Create IStream* from global memory
  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

  if hr = S_OK then
  begin
    pstm.Write(ms.Memory, ms.Size,@pcbWrite);

    if (ms.Size = pcbWrite) then
    begin
      Img := TGPImage.Create(pstm);

      if (Img.GetFormat <> ifPNG) then
      begin
        GPBmp := TGPBitmap.Create(pstm);
        GPBmp.GetPixel(0, Img.GetHeight - 1, AClr);
        GPBmp.Free;

        r := GetRed(AClr);
        g := GetGreen(AClr);
        b := GetBlue(AClr);

        ImageAttributes := TGPImageAttributes.Create;
        ImageAttributes.SetColorKey(MakeColor(r, g, b), MakeColor(r, g, b), ColorAdjustTypeDefault);
        graphics.DrawImage(Img, MakeRect(P.X, P.Y, Img.GetWidth, Img.Getheight),  // destination rectangle
          0, 0,        // upper-left corner of source rectangle
          Img.GetWidth,       // width of source rectangle
          Img.GetHeight,      // height of source rectangle
          UnitPixel,
          ImageAttributes);

        ImageAttributes.Free;
      end
      else
      begin
        //graphics.DrawImage(Img, p.X, p.Y);
        graphics.DrawImageRect(Img, p.X, p.Y, Img.GetWidth, Img.GetHeight);
      end;
      Img.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hGlobal);

  ms.Free;
end;

//------------------------------------------------------------------------------

function AnimateDouble(var Start, Stop: Double; Delta: Double; Margin: Double): Boolean;
begin
  Result := true;
  if (Start > Stop - Margin) and (Start < Stop + Margin) then
  begin
    Start := Stop;
    Result := false;
  end
  else
  begin
    Delta := Max(Margin, Delta);
    if Start < Stop then
      Start := Start + Delta
    else
      Start := Start - Delta;
  end;
end;

//------------------------------------------------------------------------------

{ TGDIPSplashProgress }

procedure TGDIPSplashProgress.Assign(Source: TPersistent);
begin
  if (Source is TGDIPSplashProgress) then
  begin
    FShadows := (Source as TGDIPSplashProgress).Shadows;
    FOverlays := (Source as TGDIPSplashProgress).Overlays;
    FBackGroundFill.Assign((Source as TGDIPSplashProgress).BackGroundFill);
    FProgressFill.Assign((Source as TGDIPSplashProgress).ProgressFill);
    FValueFormat := (Source as TGDIPSplashProgress).ValueFormat;
    FValueType := (Source as TGDIPSplashProgress).ValueType;
    FValuePositionTop := (Source as TGDIPSplashProgress).ValuePositionTop;
    FValuePositionLeft := (Source as TGDIPSplashProgress).ValuePositionLeft;
    FValuePosition := (Source as TGDIPSplashProgress).ValuePosition;
    FValueVisible := (Source as TGDIPSplashProgress).ValueVisible;
    FFont.Assign((Source as TGDIPSplashProgress).Font);
    FVisible := (Source as TGDIPSplashProgress).Visible;
    FHeight := (Source as TGDIPSplashProgress).Height;
    FWidth := (Source as TGDIPSplashProgress).Width;
    FLeft := (Source as TGDIPSplashProgress).Left;
    FTop := (Source as TGDIPSplashProgress).Top;
    FStep := (Source as TGDIPSplashProgress).Step;
    FMinimum := (Source as TGDIPSplashProgress).Minimum;
    FMaximum := (Source as TGDIPSplashProgress).Maximum;
    FPosition := (Source as TGDIPSplashProgress).Position;
    FGlowAnimation := (Source as TGDIPSplashProgress).GlowAnimation;
    FProgressAnimation := (Source as TGDIPSplashProgress).ProgressAnimation;
    FProgressFont.Assign((Source as TGDIPSplashProgress).ProgressFont);
    Changed;
  end;
end;

//------------------------------------------------------------------------------

function TGDIPSplashProgress.CalculateProgressRectangle(r: TRect; min, max, pos: Double): TGPRectF;
var
  totalw, xpos: Double;
begin
  if (ProgressFill.BorderColor <> clNone) and (ProgressFill.BorderWidth <> 0) then
    totalw := InsideRect(r).Right - ProgressFill.BorderWidth - InsideRect(r).Left
  else
    totalw := InsideRect(r).Right - InsideRect(r).Left;

  if (max - min) > 0 then
    xpos := Math.Min(((pos - min) / (max - min)) * totalw, totalw)
  else
    xpos := 0;

  if (ProgressFill.BorderColor <> clNone) and (ProgressFill.BorderWidth <> 0) then
    Result := MakeRect(InsideRect(r).Left + (ProgressFill.BorderWidth / 2), InsideRect(r).Top + (ProgressFill.BorderWidth / 2),
      xpos, InsideRect(r).Bottom - ProgressFill.BorderWidth  - InsideRect(r).Top)
  else
    Result := MakeRect(InsideRect(r).Left, InsideRect(r).Top, xpos, InsideRect(r).Bottom - InsideRect(r).Top);
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TGDIPSplashProgress.Create;
begin
  FBackGroundFill := TGDIPFill.Create;
  FBackGroundFill.OnChange := FillChanged;
  FProgressFill := TGDIPFill.Create;
  FProgressFill.OnChange := FillChanged;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FValueFormat := '%.0f%%';
  FValueType := vtAbsolute;
  FValuePositionTop := 0;
  FValuePositionLeft := 0;
  FShadows := True;
  FOverlays := True;
  FValueVisible := false;
  FValuePosition := vpCenter;
  FProgressFont := TFont.Create;
  FProgressFont.OnChange := FontChanged;
  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  FProgressFont.Name := 'Tahoma';  
  {$ENDIF}
  FVisible := False;
  FHeight := 17;
  FWidth := 300;
  FLeft := 50;
  FTop := 100;
  FStep := 10;
  FMinimum := 0;
  FMaximum := 100;
  FPosition := 0;
  FGlowAnimation := False;
  FProgressAnimation := False;
end;

//------------------------------------------------------------------------------

destructor TGDIPSplashProgress.Destroy;
begin
  FBackGroundFill.Free;
  FProgressFill.Free;
  FFont.Free;
  FProgressFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.Draw(g: TGPGraphics; r: TRect; min, max, position: Double);
var
  bg: TGPRectF;
begin
  bg := MakeRect(r.Left, r.Top, r.Right - r.Left - 1, r.Bottom - r.Top - 1);
  DrawBackGround(g, bg);

  DrawProgress(g, r, min, max, position);

  if not FMetroStyle then
    DrawShadows(g, GetInsideRectF(r));

  if ValueVisible then
    DrawValue(g, r, min, max, position);

  if not FMetroStyle and Overlays then
    DrawOverLay(g, r);
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.DrawBackGround(g: TGPGraphics; r: TGPRectF);
begin
  BackGroundFill.Fill(g, r);
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.DrawGlow(g: TGPGRaphics; r: TRect; glowposition, min, max, position: Double);
var
  b: TGPLinearGradientBrush;
  rr, lr, o: TGPRectF;
  rgn: TGPRegion;
begin
  rgn := TGPRegion.Create(CalculateProgressRectangle(r, min, max, position));
  g.SetClip(rgn);
  o := GetInsideRectF(r);
  lr := MakeRect(glowposition - 30, o.Y, 30, o.Height);
  rr := MakeRect(glowposition, o.Y, 30, o.Height);
  b := TGPLinearGradientBrush.Create(MakeRect(lr.X - 1, lr.Y - 1, lr.Width + 2, lr.Height + 2), MakeColor(0, clWhite), MakeColor(120, clWhite), LinearGradientModeHorizontal);
  g.FillRectangle(b, lr);
  b.free;
  b := TGPLinearGradientBrush.Create(MakeRect(rr.X - 1, rr.Y - 1, rr.Width + 2, rr.Height + 2), MakeColor(120, clWhite), MakeColor(0, clWhite), LinearGradientModeHorizontal);
  g.FillRectangle(b, rr);
  b.free;
  g.ResetClip;
  rgn.free;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.DrawOverLay(g: TGPGraphics; r: TRect);
var
  b: TGPLinearGradientBrush;
  ro: TGPRectF;
begin
  ro := MakeRect(InsideRect(r).Left, InsideRect(r).Top, InsideRect(r).Right - InsideRect(r).Left, (InsideRect(r).Bottom - InsideRect(r).Top) / 2.5);
  b := TGPLinearGradientBrush.Create(MakeRect(ro.X - 1, ro.Y - 1, ro.Width + 2, ro.Height + 2), MakeColor(180, clWhite), MakeColor(0, clWhite), LinearGradientModeVertical);
  g.FillRectangle(b, ro);
  b.free;
  //
  ro := MakeRect(InsideRect(r).Left, InsideRect(r).Bottom - (InsideRect(r).Bottom - InsideRect(r).Top) / 5, InsideRect(r).Right - InsideRect(r).Left, (InsideRect(r).Bottom - InsideRect(r).Top) / 5);
  b := TGPLinearGradientBrush.Create(MakeRect(ro.X - 1, ro.Y - 1, ro.Width + 2, ro.Height + 2), MakeColor(0, clWhite), MakeColor(255, clWhite), LinearGradientModeVertical);
  g.FillRectangle(b, ro);
  b.free;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.DrawProgress(g: TGPGraphics; r: TRect; min, max, position: Double);
var
  pr: TGPRectF;
begin
  pr := CalculateProgressRectangle(r, min, max, position);
  ProgressFill.Fill(g, pr);
  if pr.Width > 0 then
    DrawShadows(g, pr);
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.DrawShadows(g: TGPGraphics; r: TGPRectF);
var
  o, rRight, rLeft: TGPRectF;
  b: TGPLinearGradientBrush;
begin
  if not Shadows then
    Exit;

  o := r;
  rLeft := MakeRect(o.X, o.y, 10, o.Height);
  rRight := MakeRect(o.Width - 10 + o.X, o.y, 10, o.Height);

  b := TGPLinearGradientBrush.Create(MakeRect(rLeft.X - 1, rLeft.Y - 1, rLeft.Width + 2, rLeft.Height + 2), MakeColor(30, clBlack), MakeColor(0, clBlack), LinearGradientModeHorizontal);
  g.FillRectangle(b, rLeft);
  b.free;

  b := TGPLinearGradientBrush.Create(MakeRect(rRight.X - 1, rRight.Y - 1, rRight.Width + 2, rRight.Height + 2), MakeColor(0, clBlack), MakeColor(30, clBlack), LinearGradientModeHorizontal);
  g.FillRectangle(b, rRight);
  b.Free;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.DrawValue(g: TGPGraphics; r: TRect; min, max, position: double);
var
  f: TGPFont;
  ff: TGPFontFamily;
  fs: integer;
  sf: TGPStringFormat;
  b: TGPSolidBrush;
  bn, sizerect, pr: TGPRectF;
  s: String;
  tw, th: Double;
  p: TGPPointF;
  x, y, v: Double;
  ft: TFont;
begin
  if position < min then
    position := min
  else if position > max then
    position := max;

  v := 0;
  case ValueType of
    vtPercentage:
      begin  
        if (max > min) then
          v := (Position - min) / (max - min) * 100
        else
      	  v := max;
      end;
    vtAbsolute: v := Position;
  end;

  s := Format(ValueFormat, [v]);
  GetTextSize(g, r, s, Font, sizerect);
  tw := sizerect.Width;
  th := sizerect.Height;

  pr := CalculateProgressRectangle(r, min, max, position);
  bn := GetInsideRectF(r);
  y := (bn.Height - th) / 2;
  x := r.Left;

  case ValuePosition of
    vpProgressCenter:
    begin
      if pr.Width >= tw then
        x := bn.X + (pr.Width - tw) / 2
      else
        x := bn.X;
    end;
    vpProgressRight:
    begin
      if pr.Width >= tw then
        x := bn.X + (pr.Width - tw)
      else
        x := bn.X;
    end;
    vpCenter: x := x + (bn.Width - tw) / 2;
    vpLeft: x := bn.x;
    vpRight: x := x + bn.Width - tw;
    vpCustom:
    begin
      x := x + ValuePositionLeft;
      y := ValuePositionTop;
    end;
  end;

  if (x + (tw / 2) < pr.X + pr.Width) then
    ft := ProgressFont
  else
    ft := Font;

  ff := TGPFontFamily.Create(ft.Name);

  fs := 0;
  if (fsBold in ft.Style) then
    fs := fs + 1;
  if (fsItalic in ft.Style) then
    fs := fs + 2;
  if (fsUnderline in ft.Style) then
    fs := fs + 4;

  bn := GetInsideRectF(r);
  sf := TGPStringFormat.Create;
  f := TGPFont.Create(ff, ft.Size , fs, UnitPoint);

  p := MakePoint(x, y + r.Top);

  b := TGPSolidBrush.Create(MakeColor(255, ft.Color));
  g.DrawString(s, Length(s), f, p, sf, b);


  b.Free;
  sf.Free;
  ff.Free;
  f.Free;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.FillChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.FontChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

function TGDIPSplashProgress.GetInsideRectF(r: TRect): TGPRectF;
begin
  Result := MakeRect(InsideRect(r).Left, InsideRect(r).Top, InsideRect(r).Right - InsideRect(r).Left,
    InsideRect(r).Bottom - InsideRect(r).Top);
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.GetTextSize(g: TGPGraphics; r: TRect; s: String; ft: TFont; var sizer: TGPRectF);
var
  ff: TGPFontFamily;
  fs: integer;
  bn: TGPRectF;
  sf: TGPStringFormat;
  f: TGPFont;
begin
  ff := TGPFontFamily.Create(ft.Name);

  fs := 0;
  if (fsBold in ft.Style) then
    fs := fs + 1;
  if (fsItalic in ft.Style) then
    fs := fs + 2;
  if (fsUnderline in ft.Style) then
    fs := fs + 4;

  bn := GetInsideRectF(r);
  sf := TGPStringFormat.Create;
  f := TGPFont.Create(ff, ft.Size , fs, UnitPoint);
  g.MeasureString(s, Length(s), f, bn, sf, sizer);
  
  ff.Free;
  sf.free;
  f.free;
end;

//------------------------------------------------------------------------------

function TGDIPSplashProgress.InsideRect(r: TRect): TRect;
var
  bw: integer;
begin
  Result := Bounds(r.Left, r.Top, r.Right - r.Left, r.Bottom - r.Top);
  // adapt width & height for GDI+ drawing rect

  Result.Right := Result.Right - 1;
  Result.Bottom := Result.Bottom - 1;

  if (BackGroundFill.BorderColor <> clNone) then
  begin
    if BackGroundFill.BorderWidth = 1 then
      bw := 1
    else
      bw := (BackGroundFill.BorderWidth + 1) div 2;

    InflateRect(Result, -bw, -bw);
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetBackGroundFill(const Value: TGDIPFill);
begin
  if FBackGroundFill <> value then
  begin
    FBackGroundFill.Assign(Value);
    FillChanged(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetFont(const Value: TFont);
begin
  if FFont <> value then
  begin
    FFont.Assign(Value);
    FontChanged(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetGlowAnimation(const Value: Boolean);
begin
  if (FGlowAnimation <> Value) then
  begin
    FGlowAnimation := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetHeight(const Value: Integer);
begin
  if (FHeight <> Value) then
  begin
    FHeight := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetLeft(const Value: Integer);
begin
  if (FLeft <> Value) then
  begin
    FLeft := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetMaximum(const Value: Double);
begin
  if (FMaximum <> Value) then
  begin
    FMaximum := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetMinimum(const Value: Double);
begin
  if (FMinimum <> Value) then
  begin
    FMinimum := Value;
    Changed;
  end;
end;

procedure TGDIPSplashProgress.SetOverlays(const Value: Boolean);
begin
  if FOverlays <> Value then
  begin
    FOverlays := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetPosition(const Value: Double);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetProgressAnimation(const Value: Boolean);
begin
  if (FProgressAnimation <> Value) then
  begin
    FProgressAnimation := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetProgressFill(const Value: TGDIPFill);
begin
  if FProgressFill <> value then
  begin
    FProgressFill.Assign(Value);
    FillChanged(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetProgressFont(const Value: TFont);
begin
  if FProgressFont <> value then
  begin
    FProgressFont.Assign(Value);
    FontChanged(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetShadows(const Value: Boolean);
begin
  if FShadows <> Value then
  begin
    FShadows := Value;
    Changed;
  end;
end;

procedure TGDIPSplashProgress.SetStep(const Value: Double);
begin
  if FStep <> Value then
  begin
    FStep := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------


procedure TGDIPSplashProgress.SetTop(const Value: Integer);
begin
  if (FTop <> Value) then
  begin
    FTop := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetValueFormat(const Value: String);
begin
  if FValueFormat <> value then
  begin
    FValueFormat := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetValuePosition(
  const Value: TGDIPProgressValuePosition);
begin
  if FValuePosition <> Value then
  begin
    FValuePosition := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetValuePositionLeft(const Value: integer);
begin
  if FValuePositionLeft <> value then
  begin
    FValuePositionLeft := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetValuePositionTop(const Value: integer);
begin
  if FValuePositionTop <> value then
  begin
    FValuePositionTop := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetValueType(const Value: TGDIPProgressValueType);
begin
  if FValueType <> Value then
  begin
    FValueType := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetValueVisible(const Value: Boolean);
begin
  if FValueVisible <> value then
  begin
    FValueVisible := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  Changed;
end;

//------------------------------------------------------------------------------

{ TAdvSmoothProgressBar }

procedure TAdvSplashProgressBar.AnimateProgress(Sender: TObject);
var
  d: Double;
  pos: Double;
  res: Boolean;
begin
  if (IsProgressAnimation) then
  begin
    d := Abs(FPositionTo - FPositionTemp) / 15;
    pos := FPositionTemp;
    res := AnimateDouble(pos, FPositionTo, d, 0.000001);
    if res then
    begin
      FPositionTemp := pos;
      FPosition := Pos;
      if Assigned(FOnPositionChanged) then
        FOnPositionChanged(Self, FPosition);
      Changed;
    end
    else
    begin
      FPositionTo := pos;
      FPosition := pos;
      FPositionTemp := pos;
      if Assigned(FOnPositionChanged) then
        FOnPositionChanged(Self, FPosition);
    end;
  end;

  if (Position > Minimum) and IsGlowAnimation then
  begin
    if FGlowCount >= 1500 then
      FGlowPos := FGlowPos + 3;

    if FGlowPos > Appearance.CalculateProgressRectangle(FRect, Minimum, Maximum, Position).Width then
    begin
      FGlowCount := 0;
      FGlowPos := -60;
    end
    else
      FGlowCount := FGlowCount + 10;

    Changed;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSplashProgressBar.AnimationInProgress: Boolean;
begin
 Result := IsProgressAnimation and not (Abs(FPositionTemp - FPositionTo) < 1e-3);
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.AppearanceChanged(Sender: TObject);
begin
  Left := Appearance.Left;
  Top := Appearance.Top;
  Width := Appearance.Width;
  Height := Appearance.Height;
  Visible := Appearance.Visible;
  Step := Appearance.Step;
  Minimum := Appearance.Minimum;
  Maximum := Appearance.Maximum;
  Position := Appearance.Position;
  GlowAnimation := Appearance.GlowAnimation;
  ProgressAnimation := Appearance.ProgressAnimation;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.Assign(Source: TPersistent);
begin
  if (Source is TAdvSplashProgressBar) then
  begin
    FStep := (Source as TAdvSplashProgressBar).Step;
    FMinimum := (Source as TAdvSplashProgressBar).Minimum;
    FMaximum := (Source as TAdvSplashProgressBar).Maximum;
    FPosition := (Source as TAdvSplashProgressBar).Position;
    FGlowAnimation := (Source as TAdvSplashProgressBar).GlowAnimation;
    FProgressAnimation := (Source as TAdvSplashProgressBar).ProgressAnimation;
    FAppearance.Assign((Source as TAdvSplashProgressBar).Appearance);
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.Changed;
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

constructor TAdvSplashProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  FStep := 10;
  FMinimum := 0;
  FMaximum := 100;
  FPositionTo := 0;
  FPositionTemp := 0;
  FPosition := 0;
  FGlowAnimation := False;
  FProgressAnimation := False;
  FGlowCount := 3000; //3sec
  FGlowPos := -40;
  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Interval := 10;
  FAnimationTimer.Enabled := true;
  FAnimationTimer.OnTimer := AnimateProgress;
  FAppearance := TGDIPSplashProgress.Create;
  FAppearance.OnChange := AppearanceChanged;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    SetComponentStyle(tsOffice2007Luna);

  Height := 17;
  Width := 150;
end;

//------------------------------------------------------------------------------

destructor TAdvSplashProgressBar.Destroy;
begin
  FAppearance.Free;
  FAnimationTimer.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.DrawGlow(g: TGPGRaphics; r: TRect);
var
  b: TGPLinearGradientBrush;
  rr, lr, o: TGPRectF;
  rgn: TGPRegion;
begin
  rgn := TGPRegion.Create(Appearance.CalculateProgressRectangle(r, Minimum, Maximum, Position));
  g.SetClip(rgn);
  o := Appearance.GetInsideRectF(r);
  lr := MakeRect(FGlowPos - 30, o.Y, 30, o.Height);
  rr := MakeRect(FGlowPos, o.Y, 30, o.Height);
  b := TGPLinearGradientBrush.Create(MakeRect(lr.X - 1, lr.Y - 1, lr.Width + 2, lr.Height + 2), MakeColor(0, clWhite), MakeColor(120, clWhite), LinearGradientModeHorizontal);
  g.FillRectangle(b, lr);
  b.free;
  b := TGPLinearGradientBrush.Create(MakeRect(rr.X - 1, rr.Y - 1, rr.Width + 2, rr.Height + 2), MakeColor(120, clWhite), MakeColor(0, clWhite), LinearGradientModeHorizontal);
  g.FillRectangle(b, rr);
  b.free;
  g.ResetClip;
  rgn.free;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.FillChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

function TAdvSplashProgressBar.IsGlowAnimation: Boolean;
begin
  Result := false;
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    Result := GlowAnimation;
end;

//------------------------------------------------------------------------------

function TAdvSplashProgressBar.IsProgressAnimation: Boolean;
begin
  Result := false;
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    Result := ProgressAnimation;
end;

//------------------------------------------------------------------------------

function TAdvSplashProgressBar.GetVersion: String;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TAdvSplashProgressBar.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.GoToEnd;
begin
  Position := Maximum;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.GoToValue(AValue: Double);
begin
  Position := AValue;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.GoToStart;
begin
  Position := Minimum;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.Next(AStep: Double);
begin
  Position := Position + AStep;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.Next;
begin
  Position := Position + Step;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.Paint;
var
  g: TGPGraphics;
  r: TRect;
begin
  inherited;
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
  r := Rect(0, 0, Width, Height);
  Frect := r;
  Appearance.Draw(g, r, Minimum, Maximum, Position);
  if IsGlowAnimation and (Position > 0) then
    DrawGlow(g, r);
  g.Free;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.Previous(AStep: Double);
begin
  Position := Position - AStep;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.Previous;
begin
  Position := Position - Step;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.Resize;
begin
  inherited;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.SetAppearance(const Value: TGDIPSplashProgress);
begin
  if FAppearance <> value then
  begin
    FAppearance.Assign(Value);
    AppearanceChanged(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.SetColorTones(ATones: TColorTones);
begin
  Appearance.FMetroStyle := True;
  GlowAnimation := False;
  Appearance.BackGroundFill.Color := ATones.Foreground.BrushColor;
  Appearance.BackGroundFill.ColorTo := ATones.Foreground.BrushColor;
  Appearance.BackGroundFill.ColorMirror := ATones.Foreground.BrushColor;
  Appearance.BackGroundFill.ColorMirrorTo := ATones.Foreground.BrushColor;
  Appearance.BackGroundFill.BorderColor := ATones.Foreground.BorderColor;

  Appearance.ProgressFill.Color := ATones.Selected.BrushColor;
  Appearance.ProgressFill.ColorTo := ATones.Selected.BrushColor;
  Appearance.ProgressFill.ColorMirror := ATones.Selected.BrushColor;
  Appearance.ProgressFill.ColorMirrorTo := ATones.Selected.BrushColor;
  Appearance.ProgressFill.BorderColor := ATones.Selected.BorderColor;
  Appearance.Font.Color := ATones.Selected.TextColor;
end;

procedure TAdvSplashProgressBar.SetComponentStyle(AStyle: TTMSStyle);
begin
  Appearance.FMetroStyle := False;
  GlowAnimation := True;
  SetStyle(AStyle, false);
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.SetGlowAnimation(const Value: Boolean);
begin
  if FGlowAnimation <> value then
  begin
    FGlowAnimation := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.SetMaximum(const Value: Double);
begin
  if FMaximum <> value then
  begin
    FMaximum := Value;
    if FMaximum < FPosition then
    begin
      if IsProgressAnimation then
        FPositionTo := FMaximum
      else
      begin
        FPosition := FMaximum;
        FPositionTemp := FMaximum;
        FPositionTo := FMaximum;
      end;
    end;

    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.SetMinimum(const Value: Double);
begin
  if FMinimum <> value then
  begin
    FMinimum := Value;
    if FMinimum > FPosition then
    begin
      if IsProgressAnimation then
        FPositionTo := FMinimum
      else
      begin
        FPosition := FMinimum;
        FPositionTemp := FMinimum;
        FPositionTo := FMinimum;
      end;
    end;

    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.SetPosition(const Value: Double);
begin
  FPosition := Min(Max(Minimum, Value), Maximum);
  if IsProgressAnimation then
    FPositionTo := FPosition
  else
  begin
    FPositionTo := FPosition;
    FPositionTemp := FPositionTo;
    if Assigned(FOnPositionChanged) then
      FOnPositionChanged(Self, FPosition);

    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.SetProgressAnimation(const Value: Boolean);
begin
  if FProgressAnimation <> value then
  begin
    FProgressAnimation := Value;
    Changed;
  end;
  if FProgressAnimation = false then
    SetPosition(FPosition);
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.SetStep(const Value: Double);
begin
  if FStep <> value then
  begin
    FStep := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.SetStyle(AStyle: TTMSStyle; Selected: Boolean);
begin
  with Appearance do
  begin
    case AStyle of
      tsOffice2003Blue:
      begin
        BackGroundFill.Color := $00FFD2AF;
        BackGroundFill.ColorTo := $00FFD2AF;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $FCE1CB;
          ProgressFill.ColorTo := $E0A57D;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $94E6FB;
          ProgressFill.ColorTo := $1595EE;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2003Silver:
      begin
        BackGroundFill.Color := $00E6D8D8;
        BackGroundFill.ColorTo := $00E6D8D8;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $ECE2E1;
          ProgressFill.ColorTo := $B39698;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $947C7C;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $94E6FB;
          ProgressFill.ColorTo := $1595EE;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $947C7C;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2003Olive:
      begin
        BackGroundFill.Color := $CFF0EA;
        BackGroundFill.ColorTo := $CFF0EA;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $CFF0EA;
          ProgressFill.ColorTo := $8CC0B1;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $588060;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $94E6FB;
          ProgressFill.ColorTo := $1595EE;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $588060;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2003Classic:
      begin
        BackGroundFill.Color := $00F2F2F2;
        BackGroundFill.ColorTo := $00F2F2F2;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := clWhite;
          ProgressFill.ColorTo := $C9D1D5;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $808080;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $B59285;
          ProgressFill.ColorTo := $B59285;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2007Luna:
      begin
        BackGroundFill.Color := $00FFD2AF;
        BackGroundFill.ColorTo := $00FFD2AF;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $FFEFE3;
          ProgressFill.ColorTo := $FFDDC4;
          ProgressFill.ColorMirror := $FFD1AD;
          ProgressFill.ColorMirrorTo := $FFDBC0;
          ProgressFill.BorderColor := $FFD1AD;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $AAD9FF;
          ProgressFill.ColorTo := $6EBBFF;
          ProgressFill.ColorMirror := $42AEFE;
          ProgressFill.ColorMirrorTo := $7AE1FE;
          ProgressFill.BorderColor := $FFD1AD;//$42AEFE;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2007Obsidian:
      begin
        BackGroundFill.Color := $5C534C;
        BackGroundFill.ColorTo := $5C534C;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $F9F8F8;
          ProgressFill.ColorTo := $E4E2DF;
          ProgressFill.ColorMirror := $D1CBC7;
          ProgressFill.ColorMirrorTo := $E2DEDB;
          ProgressFill.BorderColor := clBlack;//$D1CBC7;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $AAD9FF;
          ProgressFill.ColorTo := $6EBBFF;
          ProgressFill.ColorMirror := $42AEFE;
          ProgressFill.ColorMirrorTo := $7AE1FE;
          ProgressFill.BorderColor := clBlack;//$42AEFE;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsWindowsXP:
      begin
        BackGroundFill.Color := $00B6B6B6;
        BackGroundFill.ColorTo := $00B6B6B6;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := clWhite;
          ProgressFill.ColorTo := clBtnFace;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := clBlack;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := clInActiveCaption;
          ProgressFill.ColorTo := clInActiveCaption;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := clBlack;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsWhidbey:
      begin
        BackGroundFill.Color := $F5F9FA;
        BackGroundFill.ColorTo := $F5F9FA;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $F5F9FA;
          ProgressFill.ColorTo := $A8C0C0;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $94E6FB;
          ProgressFill.ColorTo := $1595EE;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsCustom: ;
      tsOffice2007Silver:
      begin
        BackGroundFill.Color := $00CAC1BA;
        BackGroundFill.ColorTo := $00CAC1BA;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $FAEEEB;
          ProgressFill.ColorTo := $E5DBD7;
          ProgressFill.ColorMirror := $E2D8D4;
          ProgressFill.ColorMirrorTo := $D1C7C5;
          ProgressFill.BorderColor := clBlack;//$E2D8D4;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $AAD9FF;
          ProgressFill.ColorTo := $6EBBFF;
          ProgressFill.ColorMirror := $42AEFE;
          ProgressFill.ColorMirrorTo := $7AE1FE;
          ProgressFill.BorderColor := clBlack;//$42AEFE;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;

     tsWindowsVista:
      begin
        BackGroundFill.Color := $FDF8F1;
        BackGroundFill.ColorTo := $FDF8F1;
        BackGroundFill.BorderColor := $FDDE99;

        ProgressFill.Color := $FEF9F0;
        ProgressFill.ColorTo := $FDF0D7;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $FEDF9A;
        ProgressFill.GradientMirrorType := gtVertical;
      end;
      tsWindows7:
      begin
        BackGroundFill.Color := $FDF8F1;
        BackGroundFill.ColorTo := $FDF8F1;
        BackGroundFill.BorderColor := $FDDE99;

        ProgressFill.Color := $FCEBDC;
        ProgressFill.ColorTo := $FCDBC1;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $CEA27D;
        ProgressFill.GradientMirrorType := gtVertical;

      end;
      tsTerminal:
      begin
        BackGroundFill.Color := clBtnFace;
        BackGroundFill.ColorTo := clBtnFace;
        BackGroundFill.BorderColor := clGray;

        ProgressFill.Color := clWhite;
        ProgressFill.ColorTo := clWhite;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := clGray;

      end;
        tsOffice2010Blue:
      begin
        BackGroundFill.Color := $FDF6EF;
        BackGroundFill.ColorTo := $F0DAC7;
        BackGroundFill.BorderColor := $C7B29F;

        ProgressFill.Color := $EDDBCD;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $5B391E;
        ProgressFill.GradientMirrorType := gtVertical;

      end;
        tsOffice2010Silver:
      begin
        BackGroundFill.Color := $00FFD2AF;
        BackGroundFill.ColorTo := $00FFD2AF;
        BackGroundFill.BorderColor := $00C0C0C0;

        ProgressFill.Color := $EDE9E5;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $7C6D66;
        ProgressFill.GradientMirrorType := gtVertical;
      end;
        tsOffice2010Black:
      begin
        BackGroundFill.Color := $BFBFBF;
        BackGroundFill.ColorTo := $919191;
        BackGroundFill.BorderColor := $D7D7D6;

        ProgressFill.Color := $828282;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $6D6D6D;
        ProgressFill.GradientMirrorType := gtVertical;
      end;
    tsWindows8, tsWindows10:
      begin
        BackGroundFill.Color := $F7F6F5;
        BackGroundFill.ColorTo := $F7F6F5;
        BackGroundFill.BorderColor := $E4E3E2;

        ProgressFill.Color := $F7E0C9;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $E4A262;
        ProgressFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2013White:
      begin
        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clWhite;
        BackGroundFill.BorderColor := $D4D4D4;

        ProgressFill.Color := $FCE2C8;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $E59D56;
        ProgressFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2013LightGray:
      begin
        BackGroundFill.Color := $F6F6F6;
        BackGroundFill.ColorTo := $F6F6F6;
        BackGroundFill.BorderColor := $C6C6C6;

        ProgressFill.Color := $FCE2C8;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $E59D56;
        ProgressFill.GradientMirrorType := gtVertical;
      end;

    tsOffice2013Gray:
      begin
        BackGroundFill.Color := $E5E5E5;
        BackGroundFill.ColorTo := $E5E5E5;
        BackGroundFill.BorderColor := $ABABAB;

        ProgressFill.Color := $FCE2C8;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $E59D56;
        ProgressFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2016White:
      begin
        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clWhite;
        BackGroundFill.BorderColor := $D4D4D4;

        ProgressFill.Color := $E3BDA3;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $E3BDA3;
        ProgressFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2016Gray:
      begin
        Appearance.Overlays := False;
        Appearance.Shadows := False;

        BackGroundFill.Color := $B2B2B2;
        BackGroundFill.ColorTo := $B2B2B2;
        BackGroundFill.BorderColor := $444444;

        ProgressFill.Color := $E3BDA3;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $E3BDA3;
        ProgressFill.GradientMirrorType := gtVertical;
      end;

    tsOffice2016Black:
      begin
        Appearance.Overlays := False;
        Appearance.Shadows := False;

        BackGroundFill.Color := $363636;
        BackGroundFill.ColorTo := $363636;
        BackGroundFill.BorderColor := $444444;

        ProgressFill.Color := $6A6A6A;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $6A6A6A;
        ProgressFill.GradientMirrorType := gtVertical;
      end;

    end;
  end;
  end;



//------------------------------------------------------------------------------

procedure TAdvSplashProgressBar.SetVersion(const Value: String);
begin

end;

//------------------------------------------------------------------------------

function CompareRect(R1, R2: TRect): Boolean;
begin
  Result := (R1.Left = R2.Left) and
            (R1.Top = R2.Top) and
            (R1.Right = R2.Right) and
            (R1.Bottom = R2.Bottom);
end;

//------------------------------------------------------------------------------

procedure TGDIPSplashProgress.SetWidth(const Value: Integer);
begin
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------
{ TSplashItem }

procedure TSplashItem.Assign(Source: TPersistent);
begin
  if (Source is TSplashItem) then
  begin
    FText := (Source as TSplashItem).Text;
    FTag := (Source as TSplashItem).Tag;
    FImageIndex := (Source as TSplashItem).ImageIndex;
    Picture.Assign((Source as TSplashItem).Picture);
    PosX := (Source as TSplashItem).PosX;
    PosY := (Source as TSplashItem).PosY;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TSplashItem.Changed;
begin
  if Assigned(TSplashItems(Collection).FOnChange) then
    TSplashItems(Collection).FOnChange(TSplashItems(Collection));
end;

//------------------------------------------------------------------------------

constructor TSplashItem.Create(Collection: TCollection);
begin
  inherited;
  FIPicture := TAdvGDIPPicture.Create;
  FIPicture.OnChange := OnPictureChanged;
  FFont := TFont.Create;
  FFont.OnChange := OnFontChanged;
  FText := '';
  FImageIndex := -1;
  FPosX := 80;
  FPosY := 130;
  FVisible := True;
  FTag := 0;
  FUpdateCount := 0;
end;

//------------------------------------------------------------------------------

destructor TSplashItem.Destroy;
begin
  if Assigned(TSplashItems(Collection).OnDeleteItem) then
    TSplashItems(Collection).OnDeleteItem(TSplashItems(Collection), Index);

  FIPicture.Free;
  FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TSplashItem.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

//------------------------------------------------------------------------------

procedure TSplashItem.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Refresh;
end;

//------------------------------------------------------------------------------

procedure TSplashItem.OnFontChanged(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------

procedure TSplashItem.OnPictureChanged(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------

procedure TSplashItem.Refresh(Fast: Boolean = False);
begin
  if (TSplashItems(Collection).Owner is TAdvSmoothSplashScreen) then
  begin
    TAdvSmoothSplashScreen(TSplashItems(Collection).Owner).RefreshItem(Self, Fast);
  end;
end;

//------------------------------------------------------------------------------

procedure TSplashItem.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TSplashItem.SetImageIndex(const Value: Integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    //Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TSplashItem.SetPicture(const Value: TAdvGDIPPicture);
begin
  FIPicture.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TSplashItem.SetPosX(const Value: Integer);
begin
  if (FPosX <> Value) then
  begin
    FPosX := Value;
    //Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TSplashItem.SetPosY(const Value: Integer);
begin
  if (FPosY <> Value) then
  begin
    FPosY := Value;
    //Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TSplashItem.SetTag(const Value: integer);
begin
  FTag := Value;
end;

//------------------------------------------------------------------------------

procedure TSplashItem.SetText(const Value: String);
begin
  if (FText <> Value) then
  begin
    FText := Value;
    //Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TSplashItems }

function TSplashItems.Add: TSplashItem;
begin
  Result := TSplashItem(inherited Add);
end;

//------------------------------------------------------------------------------

constructor TSplashItems.Create(AOwner: TComponent);
begin
  inherited Create(TSplashItem);
  FOwner := AOwner;
end;

//------------------------------------------------------------------------------

function TSplashItems.GetItem(Index: Integer): TSplashItem;
begin
  Result := TSplashItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TSplashItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

function TSplashItems.Insert(Index: Integer): TSplashItem;
begin
  Result := TSplashItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TSplashItems.SetItem(Index: Integer;
  const Value: TSplashItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

procedure TSplashItems.Update(Item: TCollectionItem);
begin
  inherited;
  
  if (Item = nil) and (Count = 0) then
  begin
    if Assigned(OnChange) then
      OnChange(self);
  end;
end;

//------------------------------------------------------------------------------

{ TAdvSmoothSplashScreen }

constructor TAdvSmoothSplashScreen.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TSplashItems.Create(Self);
  FItems.OnChange := OnItemsChanged;

  FTopLayerItems := TAdvSplashTopLayerItems.Create(Self);
  FTopLayerItems.OnChange := TopLayerItemsChanged;

  FListItems := TSplashListItems.Create(Self);
  FListItems.OnChange := OnListItemsChanged;

  FFill := TGDIPFill.Create;
  FFill.OnChange := OnFillChanged;

  FProgressBar := TGDIPSplashProgress.Create;
  FProgressBar.OnChange := OnProgressBarChnaged;

  FBasicProgramInfo := TBasicProgramInfo.Create;
  FBasicProgramInfo.OnChange := OnBasicProgramInfoChange;

  FTimeOut := 1000;
  FAutoShow := true;

  FMaxOpacity := 255;

  FStayOnTop := True;

  FAutoAdaptPictureSize := True;

  FCloseTimer := TTimer.Create(Self);
  FCloseTimer.Enabled := False;
  FCloseTimer.Interval := FTimeOut;
  FCloseTimer.OnTimer := OnCloseTimerTime;
  FHeight := 400;
  FWidth := 600;
  FThreadType := ttMainThread;
  FPictureGap := 5;
  FListItemsSettings := TSplashListItemsSettings.Create(Self);

  FCloseOnMainFormShow := True;
  FCloseOnTimeout := False;
  FDisplayLocation := dlCenter;
  FDisplayStyle := dsFadeInOut;
  FDisplayPosX := 0;
  FDisplayPosY := 0;
  FUpdateCount := 0;
  FTextRendering := tClearType;

  FFocusParentForm := False;

  FFocusTimer := TTimer.Create(Self);
  FFocusTimer.Enabled := False;
  FFocusTimer.Interval := 20;
  FFocusTimer.OnTimer := OnFocusTimerTime;

  FFocusFormTimer := nil;
  FNewFocusForm := nil;
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothSplashScreen.Destroy;
begin
  FFocusTimer.Free;
  if (FFocusFormTimer <> nil) then
    FFocusFormTimer.Free;
  FFill.Free;
  FProgressBar.Free;
  FItems.Free;
  FListItems.Free;
  FListItemsSettings.Free;
  FBasicProgramInfo.Free;
  FTopLayerItems.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TAdvSmoothSplashScreen.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothSplashScreen.GetThemeID: String;
begin
  Result := ClassName;
end;

function TAdvSmoothSplashScreen.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' +
    IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

function TAdvSmoothSplashScreen.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.Loaded;
begin
  inherited;
  LoadBasicInfo;
  if AutoShow and not (csDesigning in ComponentState) then
  begin
    Show;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.LoadFromTheme(FileName: String);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothSplashScreen.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothSplashScreen.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TAdvSmoothSplashScreen.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if not (csDestroying in ComponentState) then
  begin
    if (AOperation = opRemove) and (AComponent = FContainer) then
      FContainer := nil;

    if (AOperation = opRemove) and (AComponent = FImages) then
      FImages := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.SetProgressBar(const Value: TGDIPSplashProgress);
begin
  FProgressBar.Assign(Value);
end;

procedure TAdvSmoothSplashScreen.SetStayOnTop(const Value: Boolean);
begin
  if FStayOnTop <> Value then
  begin
    FStayOnTop := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.OnProgressBarChnaged(Sender: TObject);
begin
  if not (csDesigning in ComponentState) and FShowing and Assigned(FSplashWindow) and Assigned(FSplashWindow.FSplashProgressBar) then
  begin
    FSplashWindow.FSplashProgressBar.Position := FProgressBar.Position;
    FSplashWindow.FSplashProgressBar.GlowAnimation := FProgressBar.GlowAnimation;
    FSplashWindow.FSplashProgressBar.Step := FProgressBar.Step;
    FSplashWindow.FSplashProgressBar.ProgressAnimation := FProgressBar.ProgressAnimation;
    FSplashWindow.FSplashProgressBar.Maximum := FProgressBar.Maximum;
    FSplashWindow.FSplashProgressBar.Minimum := FProgressBar.Minimum;
    FSplashWindow.FSplashProgressBar.Visible := FProgressBar.Visible;
    FSplashWindow.FSplashProgressBar.Left := FProgressBar.Left;
    FSplashWindow.FSplashProgressBar.Top := FProgressBar.Top;
    FSplashWindow.FSplashProgressBar.Width := FProgressBar.Width;
    FSplashWindow.FSplashProgressBar.Height := FProgressBar.Height;
    FSplashWindow.FSplashProgressBar.Appearance.Assign(FProgressBar);

    if FSplashWindow.ForcedTransparent then
    begin
      Refresh;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.SetItems(const Value: TSplashItems);
begin
  FItems.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.OnItemsChanged(Sender: TObject);
begin
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.Hide;
begin
  if not FShowing then
    Exit;

  FCloseTimer.Enabled := False;

  if (DisplayStyle = dsFadeInOut) and Assigned(FSplashWindow) and (FSplashWindow.Visible) then
    FSplashWindow.FadeOut(20, FADDING_DELAY, 10);

  if Assigned(FDisplayThread) then
  begin
    FDisplayThread.Terminate;
  end;

  FShowing := False;


  DestroySplashScreen;

  if Assigned(FOnClose) then
    FOnClose(Self);

  if Assigned(FMainForm) then
    FMainForm.OnShow := FOldMainFormShow;

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.FadeIn(const Step, Wait, Max: Integer);
begin
  if Assigned(FSplashWindow) and (FSplashWindow.Visible) then
  begin
    if not FSplashWindow.ForcedTransparent then
    begin
      while (FSplashWindow.AlphaBlendValue < Max) do
      begin
        if FSplashWindow.AlphaBlendValue + Step >= Max then
        begin
          FSplashWindow.AlphaBlendValue := Max;
          Break;
        end;
        FSplashWindow.AlphaBlendValue := FSplashWindow.AlphaBlendValue + Step;
        Sleep(Wait * 3);
      end;
    end
    else
    begin
      FSplashWindow.FadeIn(Step, Wait * 3, Max);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.FadeOut(const Step, Wait, Min: Integer);
begin
  if Assigned(FSplashWindow) and (FSplashWindow.Visible) then
    FSplashWindow.FadeOut(Step, Wait, Min);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.SetMainForm(const Value: TForm);
begin
  if Assigned(FMainForm) and FCloseOnMainFormShow then
    FMainForm.OnShow := FOldMainFormShow;

  FMainForm := Value;

  if FCloseOnMainFormShow and Assigned(FMainForm) then
  begin
    FOldMainFormShow := FMainForm.OnShow;
    FMainForm.OnShow := OnMainFormShow;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.Show(X, Y: Integer);
begin
  if FShowing then
    Exit;

  if Assigned(FOnBeforeShow) then
    FOnBeforeShow(Self);

  if (Owner is TForm) then
    FMainForm := TForm(Owner);
  if Assigned(Application.MainForm) then
    FMainForm := Application.MainForm;

  if FCloseOnMainFormShow and Assigned(FMainForm) then
  begin
    FOldMainFormShow := FMainForm.OnShow;
    FMainForm.OnShow := OnMainFormShow;
  end;

  if (ThreadType = ttSeparateThread) then
  begin
    if not Assigned(FDisplayThread) then
    begin
      FDisplayThread := TSplashScreenThread.Create(Self);
      FDisplayThread.OnTerminate := OnDisplayThreadTerminate;
    end;
  end
  else
  begin
    ShowSplashScreen(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.Show;
begin
  Show(DisplayPosX, DisplayPosY);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.OnDisplayThreadTerminate(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.OnFocusTimerTime(Sender: TObject);
var
  pt, pt2: TPoint;
begin
  FFocusTimer.Enabled := False;

  // Workaround/trick to forcibly set Keyboard focus to Parent form. 
  if Assigned(FMainForm) then
  begin
    Pt.x := 0;
    Pt.y := 0;
    Pt := FMainForm.ClientToScreen(Pt);
    Pt.x := Round(Pt.x * (65535 / Screen.Width));
    Pt.y := Round(Pt.y * (65535 / Screen.Height));

    // keeping old mouse pos
    GetCursorPos(pt2);

    // mouse move
    Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE, Pt.x, Pt.y, 0, 0) ;

    // left mouse button down
    Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN, Pt.x, Pt.y, 0, 0);
    // left mouse button Up
    Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, Pt.x, Pt.y, 0, 0) ;

    // moving mouse to old pos
    pt2.x := Round(Pt2.x * (65535 / Screen.Width)) ;
    Pt2.y := Round(Pt2.y * (65535 / Screen.Height)) ;
    Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE, Pt2.x, Pt2.y, 0, 0);
  end;  
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.OnMainFormShow(Sender: TObject);
begin
  Hide;
  if Assigned(FOldMainFormShow) then
    FOldMainFormShow(Sender);

  if FFocusParentForm and Assigned(FMainForm) then
    FFocusTimer.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.DestroySplashScreen;
begin
  if Assigned(FSplashWindow) then
  begin
    Hide;
    FreeAndNil(FSplashWindow);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.OnCloseTimerTime(Sender: TObject);
var
  WasShowing: Boolean;
begin
  WasShowing := Showing;
  Hide;

  if FFocusParentForm and Assigned(FMainForm) and WasShowing then
    FFocusTimer.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.SetListItems(const Value: TSplashListItems);
begin
  FListItems.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.OnListItemsChanged(Sender: TObject);
begin
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.Refresh;
begin
  if FShowing and Assigned(FSplashWindow) then
  begin
    FSplashWindow.Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.LoadBasicInfo;
type
  PTransBuffer = ^TTransBuffer;
  TTransBuffer = array[1..4] of smallint;
var
  iAppSize, iLenOfValue : DWord;
  pcBuf,pcValue         : PChar;
  VerSize               : DWord;
  pTrans                : PTransBuffer;
  TransStr              : string;
  sAppName              : String;
  fvip                  : pointer;
begin
  if not (csDesigning in ComponentState) and FBasicProgramInfo.AutoLoad then
  begin
    sAppName := Application.ExeName;
    // get version information values
    iAppSize:= GetFileVersionInfoSize(PChar(sAppName),// pointer to filename string
                                      iAppSize);      // pointer to variable to receive zero
    // if GetFileVersionInfoSize is successful
    if iAppSize > 0 then
    begin
      pcBuf := AllocMem(iAppSize);

      GetFileVersionInfo(PChar(sAppName),              // pointer to filename string
                         0,                            // ignored
                         iAppSize,                     // size of buffer
                         pcBuf);                       // pointer to buffer to receive file-version info.

      VerQueryValue(pcBuf, '\', fvip, iLenOfValue);

      VerQueryValue(pcBuf, PChar('\VarFileInfo\Translation'),
              pointer(ptrans), verSize);
      TransStr:= IntToHex(ptrans^[1], 4) + IntToHex(ptrans^[2], 4);

      if VerQueryValue(pcBuf,PChar('StringFileInfo\' + TransStr + '\' + 'LegalCopyright'), Pointer(pcValue),iLenOfValue) then
        FBasicProgramInfo.FCopyRight := pcValue
      else
        FBasicProgramInfo.FCopyRight := '';
      if VerQueryValue(pcBuf,PChar('StringFileInfo\' + TransStr + '\' + 'ProductName'), Pointer(pcValue),iLenOfValue) then
        FBasicProgramInfo.FProgramName.Text := pcValue
      else
        FBasicProgramInfo.FProgramName.Text := '';
      //if VerQueryValue(pcBuf,PChar('StringFileInfo\' + TransStr + '\' + 'ProductVersion'), Pointer(pcValue),iLenOfValue) then
        //FBasicProgramInfo.FProgramVersion.Text := pcValue
      if VerQueryValue(pcBuf,PChar('StringFileInfo\' + TransStr + '\' +  'FileVersion'), Pointer(pcValue),iLenOfValue) then
        FBasicProgramInfo.FProgramVersion.Text := pcValue
      else
        FBasicProgramInfo.FProgramVersion.Text := '';
      FreeMem(pcBuf,iAppSize);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.SaveToTheme(FileName: String);
begin

end;

procedure TAdvSmoothSplashScreen.SetBasicProgramInfo(
  const Value: TBasicProgramInfo);
begin
  FBasicProgramInfo.Assign(Value);
end;

procedure TAdvSmoothSplashScreen.SetColorTones(ATones: TColorTones);
begin
  Fill.BorderColor := ATones.Background.BorderColor;
  Fill.Color := ATones.Background.BrushColor;
  Fill.ColorTo := ATones.Background.BrushColor;
  Fill.ColorMirror := ATones.Background.BrushColor;
  Fill.ColorMirrorTo := ATones.Background.BrushColor;
  Fill.GradientMirrorType := gtNone;
end;

procedure TAdvSmoothSplashScreen.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  Fill.GradientType := gtVertical;


  case AStyle of
    tsOffice2003Blue:
      begin
//        Caption.ColorStart := clBlack;
//        Caption.ColorEnd := clBlack;
//        Caption.LineColor := clBlack;

        Fill.Color := $00FDEADA;
        Fill.ColorTo := $00E4AE88;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $DFD2C5;
      end;
    tsOffice2003Silver:
      begin
//        Caption.ColorStart := clBlack;
//        Caption.ColorEnd := clBlack;
//        Caption.LineColor := clBlack;

        Fill.Color := $00F7F3F3;
        Fill.ColorTo := $00E6D8D8;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $00927476;
      end;
    tsOffice2003Olive:
      begin
//        Caption.ColorStart := clBlack;
//        Caption.ColorEnd := clBlack;
//        Caption.LineColor := clBlack;

        Fill.Color := $00CFF0EA;
        Fill.ColorTo := $008CC0B1;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $006B7760;

      end;
    tsOffice2003Classic:
      begin
//        Caption.ColorStart := clBlack;
//        Caption.ColorEnd := clBlack;
//        Caption.LineColor := clBlack;

        Fill.Color := clWhite;
        Fill.ColorTo := $00C9D1D5;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := clGray;
      end;
    tsOffice2007Luna:
      begin
//        Caption.ColorStart := $00B0721C;
//        Caption.ColorEnd := $009F661A;
//        Caption.LineColor := $00B0721C;

        Fill.BorderColor := $E3B28D;
        Fill.Color := $FAF1E9;
        Fill.ColorTo := $EDD8C7;
        Fill.ColorMirror := $EDD8C7;
        Fill.ColorMirrorTo := $FFF2E7;
        Fill.GradientMirrorType := gtVertical;
        Fill.GradientType := gtVertical;
        //Fill.Color := $00FAF1E9;
        //Fill.ColorTo := $00EDD8C7;
        //Fill.BorderColor := $C2C2C2;

      end;
    tsOffice2007Obsidian:
      begin
//        Caption.ColorStart := clWhite;
//        Caption.ColorEnd := clWhite;
//        Caption.LineColor := clWhite;

        Fill.Color := $CFC6C1;
        Fill.ColorTo := $C5BBB4;
        Fill.ColorMirror := $C5BBB4;
        Fill.ColorMirrorTo := $ECECE5;
        Fill.BorderColor := clBlack;
        Fill.GradientMirrorType := gtVertical;


        //Fill.Color := $006E6E6D;
        //Fill.ColorTo := $00CFC6C1;
        //Fill.BorderColor := $00B4B0AE;
      end;
    tsWindowsXP:
      begin
//        Caption.ColorStart := clBlack;
//        Caption.ColorEnd := clBlack;
//        Caption.LineColor := clBlack;

        Fill.Color := clBtnFace;
        Fill.ColorTo := clBtnFace;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := clBlack;
      end;
    tsWhidbey:
      begin
//        Caption.ColorStart := clBlack;
//        Caption.ColorEnd := clBlack;
//        Caption.LineColor := clBlack;

        Fill.Color := clWhite;
        Fill.ColorTo := $00D9E9EC;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $00828F92;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
//        Caption.ColorStart := $00B0721C;
//        Caption.ColorEnd := $009F661A;
//        Caption.LineColor := $00B0721C;

        //Fill.Color := $00F6F1EE;
        //Fill.ColorTo := $00E7DCD5;
        //Fill.BorderColor := $00C1BFBD;

        Fill.BorderColor := $74706F;
        Fill.Color := $F6F1EE;
        Fill.ColorTo := $E7DCD5;
        Fill.ColorMirror := $E7DCD5;
        Fill.ColorMirrorTo := $F4F4EE;
        Fill.GradientMirrorType := gtVertical;

      end;
    tsWindowsVista:
      begin
        Fill.Color := $FDF8F1;
        Fill.ColorTo := $FCEFD5;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $FDDE99;
      end;
    tsWindows7:
      begin
        Fill.Color := $FCEBDC;
        Fill.ColorTo := $FCDBC1;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $CEA27D;
      end;
    tsTerminal:
      begin
        Fill.Color := clBtnFace;
        Fill.ColorTo := clBtnFace;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := clGray;
      end;
       tsOffice2010Blue:
      begin
        Fill.Color := $FDF6EF;
        Fill.ColorTo := $F0DAC7;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $C7B29F;
      end;
       tsOffice2010Silver:
      begin
        Fill.Color := $FFFFFF;
        Fill.ColorTo := $EDE5E0;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $D2CDC8;
      end;
       tsOffice2010Black:
      begin
        Fill.Color := $BFBFBF;
        Fill.ColorTo := $919191;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $6D6D6D;
      end;
    tsWindows8, tsWindows10:
      begin
       Fill.Color := $F7F6F5;
       Fill.ColorTo := clNone;
       Fill.ColorMirror := clNone;
       Fill.ColorMirrorTo := clNone;
       Fill.BorderColor := $E4E3E2;
      end;

    tsOffice2013White:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clNone;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $D4D4D4;
      end;

    tsOffice2013LightGray:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clNone;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $C6C6C6;
      end;

    tsOffice2013Gray:
      begin
        Fill.Color := $E5E5E5;
        Fill.ColorTo := clNone;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $ABABAB;
      end;
    tsOffice2016White:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clNone;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $D4D4D4;
      end;

    tsOffice2016Gray:
      begin
        Fill.Color := $B2B2B2;
        Fill.ColorTo := clNone;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $444444;
      end;

    tsOffice2016Black:
      begin
        Fill.Color := $363636;
        Fill.ColorTo := clNone;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $444444;
      end;

  end;

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.OnBasicProgramInfoChange(Sender: TObject);
begin
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    Refresh;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.OnProgressBarChange(Sender: TObject);
begin
  if Assigned(FOnProgressChange) then
    FOnProgressChange(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.OnProgressBarPositionChange(Sender: TObject;
  Value: Double);
begin
  if Assigned(OnProgressPositionChanged) then
    FOnProgressPositionChanged(Self, Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.RefreshRect(R: TRect; Fast: Boolean = False);
begin
  if not (csDesigning in ComponentState) and FShowing and Assigned(FSplashWindow) then
  begin
    if Fast then
      InvalidateRect(FSplashWindow.Handle, @R, True)
    else
      FSplashWindow.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.RefreshItem(Item: TSplashItem; Fast: Boolean = False);
var
  R: TRect;
begin
  if not Assigned(Item) then
    Exit;

  R := Rect(Item.PosX, Item.PosY, Width, Item.PosY + 30); // TODO: proper height
  RefreshRect(R, Fast);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.RefreshProgress;
begin
  if FShowing and Assigned(FSplashWindow) then
  begin
    FSplashWindow.RefreshProgress;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Refresh;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.SetFill(const Value: TGDIPFill);
begin
  FFill.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.OnFocusFormTimerTime(Sender: TObject);
begin
  FFocusFormTimer.Enabled := False;
  ForciblySetFocus(FNewFocusForm);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.ForciblySetFocus(AForm: TCustomForm);
var
  pt, pt2: TPoint;
begin
  if Assigned(AForm) then
  begin
    Pt.x := 0;
    Pt.y := 0;
    Pt := AForm.ClientToScreen(Pt);
    Pt.x := Round(Pt.x * (65535 / Screen.Width));
    Pt.y := Round(Pt.y * (65535 / Screen.Height));

    // keeping old mouse pos
    GetCursorPos(pt2);

    // mouse move
    Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE, Pt.x, Pt.y, 0, 0) ;

    // left mouse button down
    Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN, Pt.x, Pt.y, 0, 0);
    // left mouse button Up
    Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, Pt.x, Pt.y, 0, 0) ;

    // moving mouse to old pos
    pt2.x := Round(Pt2.x * (65535 / Screen.Width)) ;
    Pt2.y := Round(Pt2.y * (65535 / Screen.Height)) ;
    Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE, Pt2.x, Pt2.y, 0, 0);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.SetFocusToForm(AForm: TCustomForm);
begin
  if not Assigned(FFocusFormTimer) then
  begin
    FFocusFormTimer := TTimer.Create(Self);
    FFocusFormTimer.Enabled := False;
    FFocusFormTimer.Interval := 20;
    FFocusFormTimer.OnTimer := OnFocusFormTimerTime;
  end;
  FNewFocusForm := AForm;
  FFocusFormTimer.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.OnFillChanged(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.SetListItemsSettings(
  const Value: TSplashListItemsSettings);
begin
  FListItemsSettings.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.Changed;
begin
  if Assigned(FSplashWindow) and FShowing then
    FSplashWindow.Invalidate;
end;

procedure TAdvSmoothSplashScreen.Close;
begin
  DestroySplashScreen;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.SetTextRendering(
  const Value: TAdvSmoothSplashScreenTextRenderingHint);
begin
  if FTextRendering <> Value then
  begin
    FTextRendering := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSplashScreen.SetTopLayerItems(
  const Value: TAdvSplashTopLayerItems);
begin
  if FTopLayerItems <> value then
  begin
    FTopLayerItems.Assign(Value);
    TopLayerItemsChanged(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSplashScreen.TopLayerItemsChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

{ TSplashScreenWindow }

procedure TSplashScreenWindow.Activate;
{var
  frm: TForm;}
begin
  inherited;

  {frm := nil;
  if Assigned(Owner) and (Owner is TForm) then
    frm := TForm(Owner)
  else if Assigned(Application.MainForm) then
    frm := Application.MainForm;

  if Assigned(frm) then
    SendMessage(frm.Handle, WM_NCACTIVATE, integer(true), 0);
   }
  // Animate;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.Click;
begin
  inherited;
  if Assigned(FSplashScreen.OnClick) then
    FSplashScreen.OnClick(FSplashScreen);
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := WS_EX_NOACTIVATE;

  {if Assigned(FSmartMsg) and (FSmartMsg.Shadow) and (Win32Platform = VER_PLATFORM_WIN32_NT) and
     ((Win32MajorVersion > 5) or ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
  }
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.CreateWnd;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.DoCreate;
begin
  inherited;
  FForcedTransparent := False;
  FOpacity := 5;
  FMainBuffer := nil;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.DoDestroy;
begin
  inherited;
  DestroyMainBuffer;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.DoHide;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.DoShow;
begin
  inherited;

end;

//------------------------------------------------------------------------------

function TSplashScreenWindow.GetParentWnd: HWnd;
var
  Last, P: HWnd;
begin
  P := GetParent((Owner as TWinControl).Handle);
  Last := P;
  while P <> 0 do
  begin
    Last := P;
    P := GetParent(P);
  end;
  Result := Last;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.HideEx;
begin
  Self.Hide;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.Loaded;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.DrawItem(graphics: TGPGraphics; Item: TSplashItem);
  procedure DrawTextOnScreen(Text: string; Font: TFont; X, Y: Integer);
  begin
    if (Text <> '') then
    begin
      Canvas.Font.Assign(Font);
      Canvas.Brush.Style := bsClear;
      Canvas.TextOut(X, Y, Text);
    end;
  end;
var
  X, Y: Integer;
  g: TGPGraphics;
begin
  if not Assigned(SplashScreen) or not Assigned(Item) or (not Item.Visible) then
    Exit;

  g := graphics;
  if not Assigned(g) then
    g := CreateGraphics;

  case SplashScreen.TextRendering of
    tAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
    tAntiAliasGridFit: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
    tClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
  end;

  X := Item.PosX;
  Y := Item.PosY;
  
  // Draw Image
  if Assigned(Item.Picture) and (not Item.Picture.Empty) then
  begin
    //Canvas.Draw(Item.PosX, Item.PosY, Item.Picture);
    DrawGDIPImage(g, Point(Item.PosX, Item.PosY), Item.Picture);
    X := X + Item.Picture.Width + SplashScreen.PictureGap;
  end
  else if Assigned(SplashScreen.Images) and (Item.ImageIndex >= 0) then
  begin
    SplashScreen.Images.Draw(Canvas, Item.PosX, Item.PosY, Item.ImageIndex);
    X := X + SplashScreen.Images.Width + SplashScreen.PictureGap;
  end;

  //DrawTextOnScreen(Item.Text, Item.Font, X, Y);
  if (Item.Text <> '') then
    DrawGDIPText(g, Item.Text, Point(X, Y), Item.Font, gtSolid, nil, HatchStyleHorizontal, 255, 255, Item.Font.Color, Item.Font.Color, False, 0, SplashScreen.TextRendering);

  if not Assigned(graphics) then
    g.Free;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.DrawItems(graphics: TGPGraphics);
var
  i: Integer;
begin
  if not Assigned(SplashScreen) then
    Exit;

  for i := 0 to SplashScreen.Items.Count - 1 do
    DrawItem(graphics, SplashScreen.Items[i]);
end;

//------------------------------------------------------------------------------

function TSplashScreenWindow.GetListItemSize(ListItem: TSplashListItem; graphic: TGPGraphics = nil): TRect;
var
  R: TRect;
  g: TGPGraphics;
  a, s, k: String;
  XSize, YSize, l, m: integer;
  hr: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if not Assigned(SplashScreen) or not Assigned(ListItem) then
    Exit;

  {if Assigned(SplashScreen.Images) and (ListItem.ImageIndex >= 0) then
  begin
    Result.Right := SplashScreen.Images.Width;
    Result.Bottom := SplashScreen.Images.Height;
  end;

  if (ListItem.HTMLText <> '') then
  begin
    Canvas.Font.Assign(SplashScreen.ListItemsSettings.HTMLFont);
    R := Rect(0, 0, SplashScreen.ListItemsSettings.Rect.Width, SplashScreen.ListItemsSettings.Rect.Height);
    DrawText(Canvas.Handle, PChar(ListItem.HTMLText),Length(ListItem.HTMLText), R, DT_CALCRECT or DT_LEFT or DT_WORDBREAK);

    if (Result.Right > 0) then
      Result.Right := Result.Right + SplashScreen.PictureGap;

    Result.Right := Result.Right + R.Right;
    Result.Bottom := Max(Result.Bottom, R.Bottom);
  end;}

    if (ListItem.HTMLText <> '') then
    begin
      g := graphic;
      if not Assigned(g) then
        g := TGPGraphics.Create(Canvas.Handle);

      case SplashScreen.TextRendering of
        tAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
        tAntiAliasGridFit: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
        tClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
      end;

      R := Rect(0, 0, SplashScreen.ListItemsSettings.Rect.Width, SplashScreen.ListItemsSettings.Rect.Height);

      HTMLDrawGDIP(g, FSplashScreen.FListItemsSettings.HTMLFont, ListItem.HTMLText, R, FSplashScreen.Images, 0, 0, -1, -1, FSplashScreen.FListItemsSettings.HTMLShadowOffset, False, True, False, False,
        False, False, True, 1.0, FSplashScreen.FListItemsSettings.HTMLURLColor, clNone, clNone, FSplashScreen.FListItemsSettings.HTMLShadowColor, a, s, k, XSize, YSize, l, m, hr, nil, FSplashScreen.FContainer, 2);

      Result := Rect(0, 0, XSize, YSize);
      if not Assigned(graphic) then
        g.Free;
    end;

end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.DrawListItems(graphics: TGPGraphics);
var
  i, d, TopItem, X: Integer;
  R, R1, IR: TRect;
  Item: TSplashListItem;
  g: TGPGraphics;
  a, s, k: String;
  XSize, YSize, l, m: integer;
  hr: TRect;
begin
  if not Assigned(SplashScreen) then
    Exit;

  g := graphics;  
  if not Assigned(g) then
    g := CreateGraphics;

  case SplashScreen.TextRendering of
    tAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
    tAntiAliasGridFit: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
    tClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
  end;

  R := SplashScreen.ListItemsSettings.Rect.GetRect;

  R1 := Rect(R.Left, R.Top, R.Right, R.Top);
  for i := 0 to SplashScreen.ListItems.Count - 1 do
  begin
    //--- Initialize Display Rect
    IR := GetListItemSize(SplashScreen.ListItems[i], g);
    R1.Top := R1.Bottom;
    R1.Bottom := R1.Top + IR.Bottom;
    SplashScreen.ListItems[i].DisplayRect := R1;
    R1.Bottom := R1.Bottom + SplashScreen.ListItemsSettings.Space;
  end;

  TopItem := 0;
  if (SplashScreen.ListItems.Count > 0) and (SplashScreen.ListItems[SplashScreen.ListItems.Count-1].DisplayRect.Bottom > (SplashScreen.ListItemsSettings.Rect.GetRect.Bottom)) then
  begin
    d := SplashScreen.ListItems[SplashScreen.ListItems.Count-1].DisplayRect.Bottom - (SplashScreen.ListItemsSettings.Rect.GetRect.Bottom);
    for i := 0 to SplashScreen.ListItems.Count - 1 do
    begin
      IR := SplashScreen.ListItems[i].DisplayRect;
      SplashScreen.ListItems[i].FDisplayRect.Top := -1;
      SplashScreen.ListItems[i].FDisplayRect.Bottom := -1;
      d := d - (IR.Bottom - IR.Top) - SplashScreen.ListItemsSettings.Space;
      if (d <= 0) then
      begin
        TopItem := Min(SplashScreen.ListItems.Count - 1, i + 1);
        Break;
      end;
    end;

    if (TopItem < SplashScreen.ListItems.Count) then
    begin
      d := SplashScreen.ListItems[TopItem].DisplayRect.Top - SplashScreen.ListItemsSettings.Rect.Top;
      for i := TopItem to SplashScreen.ListItems.Count - 1 do
      begin
        SplashScreen.ListItems[i].FDisplayRect.Top := SplashScreen.ListItems[i].DisplayRect.Top - d;
        SplashScreen.ListItems[i].FDisplayRect.Bottom := SplashScreen.ListItems[i].DisplayRect.Bottom - d;
      end;
    end;
  end;
  //----

  //---- Draw Item
  for i := TopItem to SplashScreen.ListItems.Count - 1 do
  begin
    Item := SplashScreen.ListItems[i];
    X := Item.DisplayRect.Left;
    {if Assigned(SplashScreen.Images) and (Item.ImageIndex >= 0) then
    begin
      SplashScreen.Images.Draw(Canvas, X, Item.DisplayRect.Top + ((Item.DisplayRect.Bottom - Item.DisplayRect.Top - SplashScreen.Images.Height) div 2), Item.ImageIndex);
      X := X + SplashScreen.Images.Width + SplashScreen.PictureGap;
    end;
    }
    if (Item.HTMLText <> '') then
    begin
      {Canvas.Font.Assign(SplashScreen.ListItemsSettings.HTMLFont);
      Canvas.Brush.Style := bsClear;
      R1 := Rect(X, Item.DisplayRect.Top, X + SplashScreen.ListItemsSettings.Rect.Width, Item.DisplayRect.Top + SplashScreen.ListItemsSettings.Rect.Height);
      DrawText(Canvas.Handle, PChar(Item.HTMLText),Length(Item.HTMLText), R1, DT_VCENTER or DT_LEFT or DT_WORDBREAK);
      }

      R1 := Rect(X, Item.DisplayRect.Top, X + SplashScreen.ListItemsSettings.Rect.Width, Item.DisplayRect.Top + SplashScreen.ListItemsSettings.Rect.Height);
      HTMLDrawGDIP(g, FSplashScreen.FListItemsSettings.HTMLFont, Item.HTMLText, R1, FSplashScreen.Images, 0, 0, -1, -1, FSplashScreen.FListItemsSettings.HTMLShadowOffset, False, False, False, False,
        False, False, True, 1.0, FSplashScreen.FListItemsSettings.HTMLURLColor, clNone, clNone, FSplashScreen.FListItemsSettings.HTMLShadowColor, a, s, k, XSize, YSize, l, m, hr, nil, FSplashScreen.FContainer, 2);
    end;
  end;

  if not Assigned(graphics) then
    g.free;
  //----
end;

//------------------------------------------------------------------------------
                                                                                                                                                                                                                     // to calculate size only and not paint
procedure TSplashScreenWindow.DrawBasicInfo(graphics: TGPGraphics);
  function GetTextSize(Text: string; Font: TFont): TSize;
  begin
    Result.cx := 0;
    Result.cy := 0;
    if (Text <> '') then
    begin
      Canvas.Font.Assign(Font);
      Canvas.Brush.Style := bsClear;
      Result.cx := Canvas.TextWidth(Text);
      Result.cy := Canvas.TextHeight(Text);
    end;
  end;
  procedure DrawTextOnScreen(Text: string; Font: TFont; X, Y: Integer);
  begin
    if (Text <> '') then
    begin
      Canvas.Font.Assign(Font);
      Canvas.Brush.Style := bsClear;
      Canvas.TextOut(X, Y, Text);
    end;
  end;

var
  X, Y, sp, spy: Integer;
  PNS, PVS, CRS: TSize;
  R: TRect;
  g: TGPGraphics;
begin
  if not Assigned(SplashScreen) then
    Exit;

  R := GetDisplayRectangle; // GetMyClientRect;
  InflatRounding(FSplashScreen.Fill.Rounding, R);
  R.Right := R.Right - 5;
  sp := 1;
  spy := 3;

  g := graphics;
  if not Assigned(g) then
    g := CreateGraphics;

  case SplashScreen.TextRendering of
    tAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
    tAntiAliasGridFit: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
    tClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
  end;

  //g.SetSmoothingMode(SmoothingModeAntiAlias);

  //PNS := GetTextSize(SplashScreen.BasicProgramInfo.ProgramName.Text, SplashScreen.BasicProgramInfo.ProgramName.Font);
  //Inc(PNS.cx, 5);
  with SplashScreen.BasicProgramInfo.ProgramName do
    PNS := DrawGDIPText(g, Text, Point(0, 0), Font, GradientType, Picture, HatchStyle, OpacityStart, OpacityEnd, ColorStart, ColorEnd, True, Angle, SplashScreen.TextRendering);
   
  //PVS := GetTextSize(SplashScreen.BasicProgramInfo.ProgramVersion.Text, SplashScreen.BasicProgramInfo.ProgramVersion.Font);
  //Inc(PVS.cx, 5);
  with SplashScreen.BasicProgramInfo.ProgramVersion do
    PVS := DrawGDIPText(g, Text, Point(0, 0), Font, GradientType, Picture, HatchStyle, OpacityStart, OpacityEnd, ColorStart, ColorEnd, True, Angle, SplashScreen.TextRendering);

  CRS := GetTextSize(SplashScreen.BasicProgramInfo.CopyRight, SplashScreen.BasicProgramInfo.CopyRightFont);

  X := SplashScreen.BasicProgramInfo.ProgramName.PosX;
  Y := SplashScreen.BasicProgramInfo.ProgramName.PosY;
  case SplashScreen.BasicProgramInfo.ProgramName.Location of
    ilTopLeft:
    begin
      X := R.Left;
      Y := R.Top;
    end;
    ilTopRight:
    begin
      X := R.Right - PNS.cx;
      if (SplashScreen.BasicProgramInfo.ProgramVersion.Location = ilTopRight) and (PVS.cx > 0) then
        X := X - PVS.cx - sp;
      Y := R.Top;
    end;
    ilTopCenter:
    begin
      X := r.Left + (R.Right - r.Left - PNS.cx) div 2;
      Y := r.Top;
    end;
    ilBottomLeft:
    begin
      X := R.Left;
      Y := R.Bottom - PNS.cy;
      if (SplashScreen.BasicProgramInfo.CopyRightLocation = ilBottomLeft) and (CRS.cy > 0) then
        Y := Y - CRS.cy - spy;
    end;
    ilBottomCenter:
    begin
      X := r.Left + (R.Right - r.Left - PNS.cx) div 2;
      Y := R.Bottom - PNS.cy;
      if (SplashScreen.BasicProgramInfo.CopyRightLocation = ilBottomCenter) then
        Y := Y - CRS.cy - spy - PVS.cy;
    end;
    ilBottomRight:
    begin
      X := R.Right - PNS.cx;
      if (SplashScreen.BasicProgramInfo.ProgramVersion.Location = ilBottomRight) and (PVS.cx > 0) then
        X := X - PVS.cx - sp;
      Y := R.Bottom - PNS.cy;
      if (SplashScreen.BasicProgramInfo.CopyRightLocation = ilBottomRight) and (CRS.cy > 0) then
        Y := Y - CRS.cy - spy;
    end;
  end;
  //DrawTextOnScreen(SplashScreen.BasicProgramInfo.ProgramName.Text, SplashScreen.BasicProgramInfo.ProgramName.Font, X, Y);
  with SplashScreen.BasicProgramInfo.ProgramName do
    DrawGDIPText(g, Text, Point(X, Y), Font, GradientType, Picture, HatchStyle, OpacityStart, OpacityEnd, ColorStart, ColorEnd, False, Angle, SplashScreen.TextRendering);

  X := SplashScreen.BasicProgramInfo.ProgramVersion.PosX;
  Y := SplashScreen.BasicProgramInfo.ProgramVersion.PosY;
  case SplashScreen.BasicProgramInfo.ProgramVersion.Location of
    ilTopLeft:
    begin
      Y := R.Top;
      if (SplashScreen.BasicProgramInfo.ProgramName.Location = ilTopLeft) and (PNS.cx > 0) then
      begin
        X := R.Left + PNS.cx + sp;
        Y := R.Top + (PNS.cy - PVS.cy) div 2;
      end
      else
        X := R.Left;
    end;
    ilTopCenter:
    begin
      X := R.Left + (R.Right - R.Left - PVS.cx) div 2;
      Y := r.Top;
      if (SplashScreen.BasicProgramInfo.ProgramName.Location = ilTopCenter) then
        Y := R.Top + PNS.cy + PVS.cy;
    end;
    ilTopRight:
    begin
      X := R.Right - PVS.cx;
      Y := R.Top;
    end;
    ilBottomLeft:
    begin
      Y := R.Bottom - PVS.cy;
      if (SplashScreen.BasicProgramInfo.ProgramName.Location = ilBottomLeft) and (PNS.cx > 0) then
      begin
        X := R.Left + PNS.cx + sp;
        Y := R.Bottom - PNS.cy;
      end
      else
        X := R.Left;
      if (SplashScreen.BasicProgramInfo.CopyRightLocation = ilBottomLeft) and (CRS.cy > 0) then
        Y := Y - CRS.cy - spy;
    end;
    ilBottomCenter:
    begin
      X := R.Left + (R.Right - R.Left - PVS.cx) div 2;
      if (PNS.cy > 0) then
        Y := R.Bottom - PNS.cy
      else
        Y := R.Bottom - PVS.cy;
      if (SplashScreen.BasicProgramInfo.CopyRightLocation = ilBottomCenter) and (CRS.cy > 0) then
        Y := Y - CRS.cy - spy;
    end;
    ilBottomRight:
    begin
      X := R.Right - PVS.cx;
      if (PNS.cy > 0) then
        Y := R.Bottom - PNS.cy
      else
        Y := R.Bottom - PVS.cy;
      if (SplashScreen.BasicProgramInfo.CopyRightLocation = ilBottomRight) and (CRS.cy > 0) then
        Y := Y - CRS.cy - spy;
    end;
  end;
  //DrawTextOnScreen(SplashScreen.BasicProgramInfo.ProgramVersion.Text, SplashScreen.BasicProgramInfo.ProgramVersion.Font, X, Y);
  with SplashScreen.BasicProgramInfo.ProgramVersion do
    DrawGDIPText(g, Text, Point(X, Y), Font, GradientType, Picture, HatchStyle, OpacityStart, OpacityEnd, ColorStart, ColorEnd, False, Angle, SplashScreen.TextRendering);

    
  X := SplashScreen.BasicProgramInfo.CopyRightPosX;
  Y := SplashScreen.BasicProgramInfo.CopyRightPosY;
  case SplashScreen.BasicProgramInfo.CopyRightLocation of
    ilTopLeft:
    begin
      X := R.Left;
      Y := R.Top;
      if (SplashScreen.BasicProgramInfo.ProgramName.Location = ilTopLeft) and (PNS.cy > 0) then
        Y := Y + PNS.cy + spy
      else if (SplashScreen.BasicProgramInfo.ProgramVersion.Location = ilTopLeft) and (PVS.cy > 0) then
        Y := Y + PVS.cy + spy;
    end;
    ilTopCenter:
    begin
      X := r.Left + (R.Right - R.Left - CRS.cx) div 2;
      Y := R.Top;
      if (SplashScreen.BasicProgramInfo.ProgramName.Location = ilTopCenter) and (PNS.cy > 0) then
        Y := Y + PNS.cy + spy
      else if (SplashScreen.BasicProgramInfo.ProgramVersion.Location = ilTopCenter) and (PVS.cy > 0) then
        Y := Y + PVS.cy + spy;
    end;
    ilTopRight:
    begin
      X := R.Right - CRS.cx;
      Y := R.Top;
      if (SplashScreen.BasicProgramInfo.ProgramName.Location = ilTopRight) and (PNS.cy > 0) then
        Y := Y + PNS.cy + spy
      else if (SplashScreen.BasicProgramInfo.ProgramVersion.Location = ilTopRight) and (PVS.cy > 0) then
        Y := Y + PVS.cy + spy;
    end;
    ilBottomCenter:
    begin
      X := r.Left + (R.Right - R.Left - CRS.cx) div 2;
       Y := R.Bottom - CRS.cy;
    end;
    ilBottomLeft:
    begin
      X := R.Left;
      Y := R.Bottom - CRS.cy;
    end;
    ilBottomRight:
    begin
      X := R.Right - CRS.cx;
      Y := R.Bottom - CRS.cy;
    end;
  end;
  //DrawTextOnScreen(SplashScreen.BasicProgramInfo.CopyRight, SplashScreen.BasicProgramInfo.CopyRightFont, X, Y);
  DrawGDIPText(g, SplashScreen.BasicProgramInfo.CopyRight, Point(X, Y), SplashScreen.BasicProgramInfo.CopyRightFont,
               gtSolid, nil, HatchStyleHorizontal, 255, 255, SplashScreen.BasicProgramInfo.CopyRightFont.Color, SplashScreen.BasicProgramInfo.CopyRightFont.Color, False, 0, SplashScreen.TextRendering);

  if not Assigned(graphics) then
    g.free;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.DrawBackGround(graphics: TGPGraphics);
var
  rf: TGPRectF;
  R: TRect;
  g: TGPGraphics;
begin
  if Assigned(SplashScreen) then
  begin
    g := graphics;
    if not Assigned(g) then
      g := CreateGraphics;

    case SplashScreen.TextRendering of
      tAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
      tAntiAliasGridFit: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
      tClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    end;

    g.SetSmoothingMode(SmoothingModeAntiAlias);

    R := Rect(0, 0, Width-1, Height-1); // InsideRect;
    rf := MakeRect(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);

    SplashScreen.Fill.Fill(g, rf);

    if not Assigned(graphics) then
      g.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.Paint;
begin
  inherited;
  if not Assigned(SplashScreen) then
    Exit;

  if not FForcedTransparent then
  begin
    DrawBackground(nil);
    DrawBasicInfo(nil);
    DrawListItems(nil);
    DrawTopLayerItems;
    DrawItems(nil);
    DrawProgressBar(nil);
  end;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.UpdateWindow;
begin
  if FForcedTransparent then
  begin
    CreateMainBuffer;
    UpdateLayered;
  end;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.WMActivate(var Message: TWMActivate);
begin
  Message.Result := 0;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  //Message.Result := 1;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

//------------------------------------------------------------------------------

function TSplashScreenWindow.GetMyClientRect: TRect;
var
  i: Integer;
begin
  Result := ClientRect;
  i := 4;
  if Assigned(FSplashScreen) and (FSplashScreen.Fill.BorderWidth > 0) then
    i := FSplashScreen.Fill.BorderWidth;
  Result := Rect(Result.Left + i, Result.Top + i, Result.Right - i, Result.Bottom - i);
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.SetRounding;
var
  rgn, rgn1: THandle;
  R: TRect;
  rounding: Integer;
begin
  if not Assigned(FSplashScreen) or not HandleAllocated then
    Exit;

  if (FSplashScreen.Fill.Rounding > 0) and (FSplashScreen.Fill.RoundingType <> rtNone) then
  begin
    rgn := 0;
    R := ClientRect;
    rounding := FSplashScreen.Fill.Rounding shl 1; // because of smooth gradient implementation
    case FSplashScreen.Fill.RoundingType of
      rtBoth:
      begin
        rgn := CreateRoundRectRgn(0, 0, R.Right - R.Left, R.Bottom - R.Top, Rounding, Rounding);
      end;
      rtTop:
      begin
        rgn := CreateRoundRectRgn(R.Left, R.Top, R.Right + 1, R.Bottom, Rounding, Rounding);
        rgn1 := CreateRectRgn(R.Left, R.Top + Rounding, R.Right, R.Bottom);
        CombineRgn(rgn, rgn, rgn1, RGN_OR);
        DeleteObject(rgn1);
      end;
      rtBottom:
      begin
        rgn1 := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom - Rounding);
        rgn := CreateRoundRectRgn(R.Left, R.Top, R.Right + 1, R.Bottom, Rounding, Rounding);
        CombineRgn(rgn, rgn, rgn1, RGN_OR);
        DeleteObject(rgn1);
      end;
      rtLeft:
      begin
        rgn := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom + 1, Rounding, Rounding);
        rgn1 := CreateRectRgn(R.Left + Rounding, R.Top, R.Right, R.Bottom);
        CombineRgn(rgn, rgn, rgn1, RGN_OR);
        DeleteObject(rgn1);
      end;
      rtRight:
      begin
        rgn := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom + 1, Rounding, Rounding);
        rgn1 := CreateRectRgn(R.Left, R.Top, R.Right - Rounding, R.Bottom);
        CombineRgn(rgn, rgn, rgn1, RGN_OR);
        DeleteObject(rgn1);
      end;
    end;

    if rgn > 0 then
    begin
      try
        SetWindowRgn(Handle, rgn, true);
      finally
        DeleteObject(rgn);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.SetForcedTransparent(const Value: Boolean);
begin
  if (FForcedTransparent <> Value) then
  begin
    FForcedTransparent := Value;

    if FForcedTransparent then
    begin
      CreateMainBuffer;
      SetLayeredWindow;

      UpdateLayered;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TSplashScreenWindow.CreateGraphics: TGPGraphics;
begin
  if FForcedTransparent and Assigned(FMainBuffer) then
    Result := TGPGraphics.Create(FMainBuffer)
  else
  begin
    Result := TGPGraphics.Create(Canvas.Handle);
    //Result.SetSmoothingMode(SmoothingModeAntiAlias);
  end;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.CreateMainBuffer;
begin
  if not Assigned(FMainBuffer) and FForcedTransparent then
    FMainBuffer := TGPBitmap.Create(Width, Height, PixelFormat32bppARGB);
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.DestroyMainBuffer;
begin
  if Assigned(FMainBuffer) then
    FMainBuffer.Free;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.SetLayeredWindow;
begin
  if FForcedTransparent then
  begin
    if GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_LAYERED = 0 then
      SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);

    UpdateLayered;  
  end;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.UpdateLayered;
begin
  if FForcedTransparent then
  begin
    ClearBuffer(nil);

      if FormStyle = fsStayOnTop then
      begin
        SetWindowPos(Self.Handle, HWND_TOP or HWND_TOPMOST, 0, 0, 0, 0,
          SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED)
      end
      else
      begin
        SetWindowPos(Self.Handle, HWND_NOTOPMOST, 0, 0, 0, 0,
          SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED);
      end;

    DrawBackground(nil);
    DrawBasicInfo(nil);
    DrawListItems(nil);
    DrawTopLayerItems;
    DrawItems(nil);
    DrawProgressBar(nil);
    UpdateMainWindow;
  end;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.DrawProgressBar(graphics: TGPGraphics);
var
  g: TGPGraphics;
begin
  if FForcedTransparent and Assigned(FSplashScreen) and FSplashScreen.ProgressBar.Visible then
  begin
    g := graphics;
    if not Assigned(g) then
      g := CreateGraphics;

    FSplashProgressBar.Appearance.Draw(g, Rect(FSplashProgressBar.Left, FSplashProgressBar.Top, FSplashProgressBar.Left + FSplashProgressBar.Width, FSplashProgressBar.Top + FSplashProgressBar.Height), 0, 100, FSplashProgressBar.Position);
    if not Assigned(graphics) then
      g.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.ClearBuffer(graphics: TGPGraphics);
var
  g: TGPGraphics;
begin
  g := graphics;
  if not Assigned(g) then
    g := CreateGraphics;
  g.Clear($00000000);

  if not Assigned(graphics) then
    g.Free;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.UpdateMainWindow;
var
  ScrDC, MemDC: HDC;
  BitmapHandle, PrevBitmap: HBITMAP;
  BlendFunc: _BLENDFUNCTION;
  Size: TSize;
  P, S: TPoint;
  //Opacity: Integer;
begin
  if FForcedTransparent and Assigned(FSplashScreen) then
  begin
    //Opacity := FSplashScreen.Fill.OpacityTo;

    ScrDC := CreateCompatibleDC(0);
    MemDC := CreateCompatibleDC(ScrDC);

    FMainBuffer.GetHBITMAP(0, BitmapHandle);
    PrevBitmap := SelectObject(MemDC, BitmapHandle);
    Size.cx := Width;
    Size.cy := Height;
    P := Point(Left, Top);
    S := Point(0, 0);

    with BlendFunc do
    begin
      BlendOp := AC_SRC_OVER;
      BlendFlags := 0;
      SourceConstantAlpha := FOpacity;
      AlphaFormat := AC_SRC_ALPHA;
    end;

    UpdateLayeredWindow(Handle, ScrDC, @P, @Size, MemDC, @S, 0,
      @BlendFunc, ULW_ALPHA);

    SelectObject(MemDC, PrevBitmap);
    DeleteObject(BitmapHandle);

    DeleteDC(MemDC);
    DeleteDC(ScrDC);
  end;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.Refresh;
begin
  if ForcedTransparent then
    UpdateLayered
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.RefreshProgress;
begin
  if showing and  Assigned(FSplashProgressBar) and (FSplashScreen.Showing) then
  begin
    FSplashProgressBar.Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.DrawTopLayerItems;
var
  g: TGPGraphics;
  i: integer;
  r: TRect;
begin
  if not Assigned(FSplashScreen) then
    Exit;

  g := CreateGraphics;
  case SplashScreen.TextRendering of
    tAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
    tAntiAliasGridFit: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
    tClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
  end;
  r := InsideRect; // GetDisplayRectangle;
  for I := 0 to FSplashScreen.TopLayerItems.Count - 1 do
    FSplashScreen.TopLayerItems[I].Draw(g, r);
  g.Free;
end;

//------------------------------------------------------------------------------

function TSplashScreenWindow.GetDisplayRectangle: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if not Assigned(FSplashScreen) then
    Exit;

  Result := InsideRect;
  Result := Rect(Result.Left, Result.Top, Result.Right - Result.Left - GetShadowOffset, Result.Bottom - Result.Top - GetShadowOffset);
end;

//------------------------------------------------------------------------------

function TSplashScreenWindow.InsideRect: TRect;
var
  bw: integer;
begin
  Result := Rect(-1, -1, -1, -1);
  if not Assigned(FSplashScreen) then
    Exit;

  Result := Rect(0, 0, Width, Height);
  // adapt width & height for GDI+ drawing rect

  Result.Right := Result.Right - 1;
  Result.Bottom := Result.Bottom - 1;

  if (FSplashScreen.Fill.BorderColor <> clNone) then
  begin
    if FSplashScreen.Fill.BorderWidth = 1 then
      bw := 1
    else
      bw := (FSplashScreen.Fill.BorderWidth + 1) div 2;

    InflateRect(Result, -bw, -bw);
  end;
end;

//------------------------------------------------------------------------------

function TSplashScreenWindow.GetShadowOffset: integer;
begin
  Result := 0;
  if Assigned(FSplashScreen) and (FSplashScreen.Fill.ShadowColor <> clNone) then
    Result := FSplashScreen.Fill.ShadowOffset;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.FadeIn(const Step, Wait, Max: Integer);
begin
  while FOpacity < Max do
  begin
    if FOpacity + Step >= Max then
    begin
      //Application.ProcessMessages;
      FOpacity := Max;
      UpdateMainWindow;
      Break;
    end;
    FOpacity := FOpacity + Step;
    UpdateMainWindow;
    Sleep(Wait);
  end;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.FadeOut(const Step, Wait, Min: Integer);
begin
  if ForcedTransparent then
  begin
    while FOpacity > Min do
    begin
      if FOpacity - Step <= Min then
      begin
        //Application.ProcessMessages;
        FOpacity := Min;
        UpdateMainWindow;
        Break;
      end;
      FOpacity := FOpacity - Step;
      UpdateMainWindow;
      Sleep(Wait);
    end;
  end
  else
  begin
    while AlphaBlendValue > Min do
    begin
      if AlphaBlendValue - Step <= Min then
      begin
        AlphaBlendValue := Min;
        Break;
      end;
      AlphaBlendValue := AlphaBlendValue - Step;
      Sleep(Wait * 3);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenWindow.InflatRounding(Rounding: Integer;
  var R: TRect);
begin
  R.Left := R.Left + Rounding div 2;
  R.Top := R.Top + Rounding div 2;
  R.Right := R.Right - Rounding div 2;
  R.Bottom := R.Bottom - Rounding div 2;
end;

//------------------------------------------------------------------------------

{ TSplashScreenThread }

constructor TSplashScreenThread.Create(SplashScreen: TAdvSmoothSplashScreen);
begin
  inherited Create(False);
  FAdvSplashScreen := SplashScreen;
  FreeOnTerminate := True;
end;

//------------------------------------------------------------------------------

procedure TSplashScreenThread.Execute;
begin
  inherited;

  ShowSplashScreen(FAdvSplashScreen);

  while FAdvSplashScreen.Showing do
  begin
    Application.ProcessMessages;
    Application.HandleMessage;
  end;
end;

//------------------------------------------------------------------------------

{ TSplashListItem }

procedure TSplashListItem.Assign(Source: TPersistent);
begin
  if (Source is TSplashListItem) then
  begin
    FHTMLText := (Source as TSplashListItem).HTMLText;
    FTag := (Source as TSplashListItem).Tag;
    FImageIndex := (Source as TSplashListItem).ImageIndex;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TSplashListItem.Change;
begin
  if Assigned(TSplashListItems(Collection).FOnChange) then
    TSplashListItems(Collection).FOnChange(TSplashListItems(Collection));
end;

//------------------------------------------------------------------------------

constructor TSplashListItem.Create(Collection: TCollection);
begin
  inherited;
  FHTMLText := '';
  FImageIndex := -1;
  FTag := 0;
  FDisplayRect := Rect(-1, -1, -1, -1);
  FUpdateCount := 0;
end;

//------------------------------------------------------------------------------

destructor TSplashListItem.Destroy;
begin
  if Assigned(TSplashItems(Collection).OnDeleteItem) then
    TSplashItems(Collection).OnDeleteItem(TSplashItems(Collection), Index);

  inherited;
end;

//------------------------------------------------------------------------------

procedure TSplashListItem.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

//------------------------------------------------------------------------------

procedure TSplashListItem.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Refresh;
end;

//------------------------------------------------------------------------------

procedure TSplashListItem.Refresh;
begin
  Change;
end;

//------------------------------------------------------------------------------

procedure TSplashListItem.SetImageIndex(const Value: Integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TSplashListItem.SetTag(const Value: integer);
begin
  FTag := Value;
end;

//------------------------------------------------------------------------------

procedure TSplashListItem.SetHTMLText(const Value: String);
begin
  if (FHTMLText <> Value) then
  begin
    FHTMLText := Value;
  end;
end;

//------------------------------------------------------------------------------

{ TSplashListItems }

function TSplashListItems.Add: TSplashListItem;
begin
  Result := TSplashListItem(inherited Add);
end;

//------------------------------------------------------------------------------

constructor TSplashListItems.Create(AOwner: TComponent);
begin
  inherited Create(TSplashListItem);
  FOwner := AOwner;
end;

//------------------------------------------------------------------------------

function TSplashListItems.GetItem(Index: Integer): TSplashListItem;
begin
  Result := TSplashListItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TSplashListItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

function TSplashListItems.Insert(Index: Integer): TSplashListItem;
begin
  Result := TSplashListItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TSplashListItems.SetItem(Index: Integer;
  const Value: TSplashListItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

procedure TSplashListItems.Update(Item: TCollectionItem);
begin
  inherited;

  if (Item = nil) and (Count = 0) then
  begin
    if Assigned(OnChange) then
      OnChange(self);
  end;
end;

//------------------------------------------------------------------------------

{ TListItemsRect }

procedure TListItemsRect.Assign(Source: TPersistent);
begin
  if (Source is TListItemsRect) then
  begin
    Left := (Source as TListItemsRect).Left;
    Top := (Source as TListItemsRect).Top;
    Width := (Source as TListItemsRect).Width;
    Height := (Source as TListItemsRect).Height;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TListItemsRect.Changed;
begin
  if Assigned(OnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TListItemsRect.Create;
begin
  inherited;
  FLeft := 30;
  FTop := 150;
  FWidth := 300;
  FHeight := 200;
end;

//------------------------------------------------------------------------------

function TListItemsRect.GetRect: TRect;
begin
  Result := Rect(Left, Top, Left + Width, Top + Height);
end;

//------------------------------------------------------------------------------

{ TBasicProgramInfo }

procedure TBasicProgramInfo.Assign(Source: TPersistent);
begin
  if (Source is TBasicProgramInfo) then
  begin
    FAutoLoad := (Source as TBasicProgramInfo).AutoLoad;
    FCopyRightFont.Assign((Source as TBasicProgramInfo).CopyRightFont);
    FCopyRightLocation := (Source as TBasicProgramInfo).CopyRightLocation;
    FCopyRightPosX := (Source as TBasicProgramInfo).CopyRightPosX;
    FCopyRightPosY := (Source as TBasicProgramInfo).CopyRightPosY;
    FCopyRight := (Source as TBasicProgramInfo).CopyRight;
    FProgramName.Assign((Source as TBasicProgramInfo).ProgramName);
    FProgramVersion.Assign((Source as TBasicProgramInfo).ProgramVersion);
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TBasicProgramInfo.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TBasicProgramInfo.Create;
begin
  inherited;
  FAutoLoad := True;
  FCopyRightFont:= TFont.Create;
  {$IFNDEF DELPHI9_LVL}
  FCopyRightFont.Name := 'Tahoma';
  {$ENDIF}
  FCopyRightLocation := ilTopLeft;
  FCopyRightPosX := 10;
  FCopyRightPosY := 130;
  FCopyRight := '';
  FProgramName := TSplashHeadingText.Create;
  FProgramName.Font.Size := 14;
  FProgramName.Font.Style := [fsBold];
  FProgramName.OnChange := ProgramChanged;
  FProgramVersion := TSplashHeadingText.Create;
  FProgramVersion.Font.Size := 14;
  FProgramVersion.PosX := 80;
  FProgramVersion.PosY := 100;
  FProgramVersion.OnChange := ProgramChanged;
end;

//------------------------------------------------------------------------------

destructor TBasicProgramInfo.Destroy;
begin
  FProgramVersion.Free;
  FCopyRightFont.Free;
  FProgramName.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TBasicProgramInfo.ProgramChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

procedure TBasicProgramInfo.SetCopyRightFont(const Value: TFont);
begin
  FCopyRightFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TBasicProgramInfo.SetProgramName(
  const Value: TSplashHeadingText);
begin
  FProgramName.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TBasicProgramInfo.SetProgramVersion(
  const Value: TSplashHeadingText);
begin
  FProgramVersion.Assign(Value);
end;

//------------------------------------------------------------------------------

{ TSplashHeadingText }

procedure TSplashHeadingText.Assign(Source: TPersistent);
begin
  if (Source is TSplashHeadingText) then
  begin
    FAngle := TSplashHeadingText(Source).FAngle;
    FStartOpacity := TSplashHeadingText(Source).FStartOpacity;
    FEndOpacity := TSplashHeadingText(Source).FEndOpacity;
    FPosY := TSplashHeadingText(Source).FPosY;
    FPosX := TSplashHeadingText(Source).FPosX;
    FText := TSplashHeadingText(Source).FText;
    FPicture.Assign(TSplashHeadingText(Source).FPicture);
    FGradientType := TSplashHeadingText(Source).FGradientType;
    FEndColor := TSplashHeadingText(Source).FEndColor;
    FStartColor := TSplashHeadingText(Source).FStartColor;
    FFont.Assign(TSplashHeadingText(Source).Font);
    FHatchStyle := TSplashHeadingText(Source).FHatchStyle;
    FLocation := TSplashHeadingText(Source).FLocation;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

constructor TSplashHeadingText.Create;
begin
  inherited;
  FAngle := 0;
  FStartOpacity := 255;
  FEndOpacity := 255;
  FPosY := 100;
  FPosX := 10;
  FText := '';
  FPicture := TAdvGDIPPicture.Create;
  FGradientType := gtVertical;
  FEndColor := clNone;
  FStartColor := clBlack;
  FFont := TFont.Create;
  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  {$ENDIF}
  FFont.Name := 'Tahoma';
  FHatchStyle := HatchStyleHorizontal;
  FLocation := ilTopLeft;
end;

//------------------------------------------------------------------------------

destructor TSplashHeadingText.Destroy;
begin
  FFont.Free;
  FPicture.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TSplashHeadingText.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TSplashHeadingText.SetPicture(const Value: TAdvGDIPPicture);
begin
  FPicture.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TSplashHeadingText.SetText(const Value: String);
begin
  if (FText <> Value) then
  begin
    FText := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TSplashHeadingText.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------

{ TSplashListItemsSettings }

procedure TSplashListItemsSettings.Assign(Source: TPersistent);
begin
  if (Source is TSplashListItemsSettings) then
  begin
  FHTMLShadowColor := TSplashListItemsSettings(Source).HTMLShadowColor;
  FHTMLURLColor := TSplashListItemsSettings(Source).HTMLURLColor;
  FHTMLShadowOffset := TSplashListItemsSettings(Source).HTMLShadowOffset;
  FHTMLFont.Assign(TSplashListItemsSettings(Source).HTMLFont);
  FRect.Assign(TSplashListItemsSettings(Source).FRect);
  FSpace := TSplashListItemsSettings(Source).FSpace;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

constructor TSplashListItemsSettings.Create(AOwner: TComponent);
begin
  inherited Create;
  FHTMLShadowColor := clGray;
  FHTMLURLColor := clBlue;
  FHTMLShadowOffset := 5;
  FHTMLFont := TFont.Create;
  {$IFNDEF DEPLHI9_LVL}
  FHTMLFont.Name := 'Tahoma';
  {$ENDIF}
  FRect := TListItemsRect.Create;
  FSpace := 8;
  if (AOwner is TAdvSmoothSplashScreen) then
    FOwner := TAdvSmoothSplashScreen(AOwner);
end;

//------------------------------------------------------------------------------

destructor TSplashListItemsSettings.Destroy;
begin
  FHTMLFont.Free;
  FRect.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TSplashListItemsSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TSplashListItemsSettings.SetHTMLFont(const Value: TFont);
begin
  FHTMLFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TSplashListItemsSettings.SetHTMLShadowColor(const Value: TColor);
begin
  FHTMLShadowColor := Value;
end;

//------------------------------------------------------------------------------

procedure TSplashListItemsSettings.SetHTMLShadowOffset(const Value: integer);
begin
  FHTMLShadowOffset := Value;
end;

//------------------------------------------------------------------------------

procedure TSplashListItemsSettings.SetHTMLURLColor(const Value: TColor);
begin
  FHTMLURLColor := Value;
end;

//------------------------------------------------------------------------------

{ TAdvSplashHTMLText }

procedure TAdvSplashHTMLText.Assign(Source: TPersistent);
begin
  if (Source is TAdvSplashHTMLText) then
  begin
    FText := (Source as TAdvSplashHTMLText).Text;
    FURLColor := (Source as TAdvSplashHTMLText).URLColor;
    FShadowOffset := (Source as TAdvSplashHTMLText).ShadowOffset;
    FFont.Assign((Source as TAdvSplashHTMLText).Font);
    FShadowColor := (Source as TAdvSplashHTMLText).ShadowColor;
    FLocation := (Source as TAdvSplashHTMLText).Location;
    FLeft := (Source as TAdvSplashHTMLText).Left;
    FTop := (Source as TAdvSplashHTMLText).Top;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashHTMLText.Changed;
begin
  if not FDisableRepaint then
    FOwner.Changed;
end;

//------------------------------------------------------------------------------

constructor TAdvSplashHTMLText.Create(
  AOwner: TAdvSmoothSplashScreen);
begin
  FOwner := AOwner;
  FURLColor := clBlue;
  FShadowOffset := 5;
  FShadowColor := clGray;
  FLocation := cpTopLeft;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  {$IFNDEF DELPHI9_LVL}
  FDisableRepaint := true;
  FFont.Name := 'Tahoma';
  FDisableRepaint := false;
  {$ENDIF}  
  FTop := 0;
  FLeft := 0;
end;

//------------------------------------------------------------------------------

destructor TAdvSplashHTMLText.Destroy;
begin
  FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashHTMLText.FontChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashHTMLText.SetFont(const Value: TFont);
begin
  if FFont <> value then
  begin
    FFont.Assign(value);
    FontChanged(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashHTMLText.SetLeft(const Value: integer);
begin
  if FLeft <> value then
  begin
    FLeft := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashHTMLText.SetLocation(
  const Value: TAdvSmoothSplashScreenLocation);
begin
  if FLocation <> value then
  begin
    FLocation := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashHTMLText.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> value then
  begin
    FShadowColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashHTMLText.SetShadowOffset(const Value: integer);
begin
  if FShadowOffset <> value then
  begin
    FShadowOffset := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashHTMLText.SetText(const Value: string);
begin
  if FText <> value then
  begin
    FText := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashHTMLText.SetTop(const Value: integer);
begin
  if FTop <> value then
  begin
    FTop := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashHTMLText.SetURLColor(const Value: TColor);
begin
  if FURLColor <> value then
  begin
    FURLColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvSplashTopLayerItem }

procedure TAdvSplashTopLayerItem.Assign(Source: TPersistent);
begin
  if Source is TAdvSplashTopLayerItem then
  begin
    FHTMLText.Assign((Source as TAdvSplashTopLayerItem).HTMLText);
    FVisible := (Source as TAdvSplashTopLayerItem).Visible;
    FTop := (Source as TAdvSplashTopLayerItem).Top;
    FLeft := (Source as TAdvSplashTopLayerItem).Left;
    FHeight := (Source as TAdvSplashTopLayerItem).Height;
    FWidth := (Source as TAdvSplashTopLayerItem).Width;
    FFill.Assign((Source as TAdvSplashTopLayerItem).Fill);
    FAlign := (Source as TAdvSplashTopLayerItem).Align;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashTopLayerItem.HTMLTextChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashTopLayerItem.Changed;
begin
  FOwner.Changed;
end;

//------------------------------------------------------------------------------

constructor TAdvSplashTopLayerItem.Create(Collection: TCollection);
begin
  inherited;
  FAlign := alCustom;
  FOwner := (Collection as TAdvSplashTopLayerItems).FOwner;
  FHTMLText := TAdvSplashHTMLText.Create(FOwner);
  FHTMLText.OnChange := HTMLTextChanged;
  FWidth := 100;
  FHeight := 100;
  FVisible := true;
  FLeft := 0;
  FTop := 0;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FOwner.Changed;
end;

//------------------------------------------------------------------------------

destructor TAdvSplashTopLayerItem.Destroy;
begin
  FHTMLText.Free;
  FFill.Free;
  FOwner.Changed;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashTopLayerItem.Draw(g: TGPGraphics; r: TRect);
var
  rf: TGPRectF;
begin
  rf := MakeRect(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  Draw(g, rf);
end;

//------------------------------------------------------------------------------

procedure TAdvSplashTopLayerItem.Draw(g: TGPGraphics; r: TGPRectF);
var
  rd: TGPRectF;
begin
  if FVisible then
  begin
    if (FFill.Rounding > 0) and (FFill.RoundingType <> rtNone) then
      g.SetSmoothingMode(SmoothingModeAntiAlias);

    //FILL
    case Align of
      alNone:;
      alTop: rd := MakeRect(r.X, r.Y, r.Width - r.X, FHeight);
      alBottom: rd := MakeRect(r.X, R.Y + r.Height - FHeight, r.Width - r.X, FHeight);
      alLeft: rd := MakeRect(r.X, r.Y, FWidth, r.Height);
      alRight: rd := MakeRect(r.Width - FWidth, r.Y, FWidth, r.Height);
      alClient: rd := MakeRect(r.X, R.Y, R.Width - R.X, R.Height);
      alCustom: rd := MakeRect(r.X + FLeft, R.Y + FTop, FWidth, FHeight);
    end;

    if (Fill.BorderColor = clNone) or (Fill.BorderWidth = 0) then
      rd.Height := rd.Height + 1;

    Fill.Fill(g, rd);
    //HTML
    DrawHTMLText(g, HTMLText, rd);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashTopLayerItem.DrawHTMLText(g: TGPGraphics;
  HTML: TAdvSplashHTMLText; R: TGPRectF);
var
  htmlr: TRect;
  a, s, k: String;
  l, m, XSize, YSize: integer;
  hr: TRect;
  x, y: integer;
begin
  if (HTML.Text <> '') and Assigned(FOwner) then
  begin
    with HTML do
    begin
      htmlr := Rect(0, 0, Round(R.Width), Round(R.Height));
      //rf := MakeRect(R.Left, R.Top, R.Right- R.Left, R.Bottom - R.Top);

      HTMLDrawGDIP(g, FFont, HTML.Text, htmlr, FOwner.FImages, 0, 0, -1, -1, FShadowOffset,False,true,false,false,
        False,False,true,1.0,FURLColor,clNone,clNone,FShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,FOwner.FContainer,2);

      if FLocation <> cpCustom then
        GetObjectLocation(x, y, R, XSize, YSize, FLocation)
      else
      begin
        x := FLeft;
        y := FTop;
      end;

      htmlr := Bounds(Round(x), Round(y), xsize, ysize);

      HTMLDrawGDIP(g, FFont, HTML.Text,htmlr,FOwner.FImages, 0,0,-1,-1,FShadowOffset,False,false,false,false,
        False,False,true,1.0,FURLColor,clNone,clNone,FShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,FOwner.FContainer,2);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashTopLayerItem.FillChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashTopLayerItem.SetAlign(const Value: TAlign);
begin
  if FAlign <> value then
  begin
    FAlign := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashTopLayerItem.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill.Assign(Value);
    FillChanged(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashTopLayerItem.SetHeight(const Value: integer);
begin
  if FHeight <> value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashTopLayerItem.SetHTMLText(
  const Value: TAdvSplashHTMLText);
begin
  if FHTMLText <> value then
  begin
    FHTMLText.Assign(value);
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashTopLayerItem.SetLeft(const Value: integer);
begin
  if FLeft <> value then
  begin
    FLeft := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashTopLayerItem.SetTop(const Value: integer);
begin
  if FTop <> value then
  begin
    FTop := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashTopLayerItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashTopLayerItem.SetWidth(const Value: integer);
begin
  if FWidth <> value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvSplashTopLayerItems }

function TAdvSplashTopLayerItems.Add: TAdvSplashTopLayerItem;
begin
  Result := TAdvSplashTopLayerItem(inherited Add);
end;

//------------------------------------------------------------------------------

constructor TAdvSplashTopLayerItems.Create(
  AOwner: TAdvSmoothSplashScreen);
begin
  inherited Create(TAdvSplashTopLayerItem);
  FOwner := AOwner;
end;

//------------------------------------------------------------------------------

procedure TAdvSplashTopLayerItems.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

//------------------------------------------------------------------------------

function TAdvSplashTopLayerItems.GetItem(
  Index: Integer): TAdvSplashTopLayerItem;
begin
  Result := TAdvSplashTopLayerItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TAdvSplashTopLayerItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

function TAdvSplashTopLayerItems.Insert(
  Index: Integer): TAdvSplashTopLayerItem;
begin
  Result := TAdvSplashTopLayerItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TAdvSplashTopLayerItems.SetItem(Index: Integer;
  const Value: TAdvSplashTopLayerItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

end.
