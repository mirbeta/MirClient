{***************************************************************************}
{ GDI+ Fill                                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2015                                               }
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

{
WorkAround GDI+ Border issue with LinearGradientBrush

-> MakeRect(r.X - 1, r.Y - 1, r.Width + 2, r.Height + 2)
}

unit GDIPFill;

{$I TMSDEFS.INC}

interface

uses
  Windows, Graphics, Classes, Forms, SysUtils, Math, Clipbrd, IniFiles, AdvGDIP, ImgList, GDIPPictureContainer, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  {$IFDEF DIRECT2D}
  ,Direct2D, D2D1, Wincodec
  {$ENDIF}
  ;

type
  TGlowMode = (gmNone, gmGradient, gmRadial, gmRadialGradient);

  TFillPicturePosition = (ppTopLeft,ppTopCenter,ppTopRight,ppBottomLeft,ppBottomCenter,ppBottomRight,ppTiled,ppStretched,ppCenterLeft,ppCenterCenter,ppCenterRight,ppCustom);

  TFillRoundingType = (rtNone, rtTop, rtBottom, rtBoth, rtLeft, rtRight);

  TFillPictureSize = (psOriginal, psCustom);

  TFillPictureMode = (pmInsideFill, pmOutsideFill);

  TFillValueType = (vtNormal, vtDateTime);

  TPictureMode = (pmStretch, pmNormal);

  TShadowType = (stRightBottom, stBottom, stSurround);

  TGDIPFill = class(TPersistent)
  private
    FBorderColor: TColor;
    FOpacity: Byte;
    FPicturePosition: TFillPicturePosition;
    FGradientType: TAdvGradientType;
    FOpacityTo: Byte;
    FPicture: TAdvGDIPPicture;
    FHatchStyle: THatchStyle;
    FColor: TColor;
    FShadowOffset: integer;
    FShadowType: TShadowType;
    FOpacityMirror: Byte;
    FGradientMirrorType: TAdvGradientType;
    FOpacityMirrorTo: Byte;
    FRounding: integer;
    FShadowColor: TColor;
    FBorderWidth: integer;
    FColorMirror: TColor;
    FColorMirrorTo: TColor;
    FColorTo: TColor;
    FOnChange: TNotifyEvent;
    FHatchStyleMirror: THatchStyle;
    FPictureTop: integer;
    FPictureLeft: integer;
    FBackGroundPictureTop: integer;
    FBackGroundPictureLeft: integer;
    FBackGroundPicturePosition: TFillPicturePosition;
    FBackGroundPicture: TAdvGDIPPicture;
    FpictureWidth: integer;
    FpictureSize: TFillPictureSize;
    FpictureHeight: integer;
    FRoundingType: TFillRoundingType;
    FUpdateCount: integer;
    FFocus: Boolean;
    FFocusColor: TColor;
    FAngle: integer;
    FBorderOpacity: Byte;
    FFocusRect: TGPRectF;
    FBackGroundPictureMode: TFillPictureMode;
    FGlowGradientColor: TColor;
    FGlow: TGlowMode;
    FGlowRadialColor: TColor;
    FBackGroundPictureAspectRatio: Boolean;
    FPictureAspectRatio: Boolean;
    FBackGroundPictureAspectMode: TPictureMode;
    FPictureAspectMode: TPictureMode;
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderWidth(const Value: integer);
    procedure SetColor(const Value: TColor);
    procedure SetColorMirror(const Value: TColor);
    procedure SetColorMirrorTo(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetGradientMirrorType(const Value: TAdvGradientType);
    procedure SetGradientType(const Value: TAdvGradientType);
    procedure SetHatchStyle(const Value: THatchStyle);
    procedure SetOpacity(const Value: Byte);
    procedure SetOpacityMirror(const Value: Byte);
    procedure SetOpacityMirrorTo(const Value: Byte);
    procedure SetOpacityTo(const Value: Byte);
    procedure SetPicture(const Value: TAdvGDIPPicture);
    procedure SetPicturePosition(const Value: TFillPicturePosition);
    procedure SetRounding(const Value: integer);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: integer);
    procedure SetHatchStyleMirror(const Value: THatchStyle);
    procedure SetPictureLeft(const Value: integer);
    procedure SetPictureTop(const Value: integer);
    procedure SetBackGroundPicture(const Value: TAdvGDIPPicture);
    procedure SetBackGroundPictureLeft(const Value: integer);
    procedure SetBackGroundPicturePosition(const Value: TFillPicturePosition);
    procedure SetBackGroundPictureTop(const Value: integer);
    procedure SetPictureHeight(const Value: integer);
    procedure SetPictureSize(const Value: TFillPictureSize);
    procedure SetPictureWidth(const Value: integer);
    procedure SetRoundingType(const Value: TFillRoundingType);
    procedure SetFocus(const Value: Boolean);
    procedure SetFocusColor(const Value: TColor);
    procedure SetAngle(const Value: integer);
    procedure SetBorderOpacity(const Value: Byte);
    procedure SetBackGroundPictureMode(const Value: TFillPictureMode);
    procedure SetGlow(const Value: TGlowMode);
    procedure SetGlowGradientColor(const Value: TColor);
    procedure SetGlowRadialColor(const Value: TColor);
    procedure SetBackGroundPictureAspectRatio(const Value: Boolean);
    procedure SetPictureAspectRatio(const Value: Boolean);
    procedure SetBackGroundPictureAspectMode(const Value: TPictureMode);
    procedure SetPictureAspectMode(const Value: TPictureMode);
    procedure SetShadowType(const Value: TShadowType);
  protected
    procedure Changed;
    procedure PictureChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DrawGradientBackGround(Graphics: TGPGraphics; R: TGPRectF; Mirror: Boolean; oc, octo, ocmr, ocmrto: Byte);
    function Fill(Graphics: TGPGraphics; R: TGPRectF; ABorderOpacity: Byte = 255; AOpacity: Byte = 255; AOpacityTo: Byte = 255; AOpacityMirror: Byte = 255; AOpacityMirrorTo: Byte = 255): TGPRectF;
    procedure DrawGlow(Graphics: TGPGraphics; R: TGPRectF);
    {$IFDEF DIRECT2D}
    procedure DrawGradientBackGround2D(Graphics: TDirect2DCanvas; R: TD2D1RectF; Mirror: Boolean; oc, octo, ocmr, ocmrto: Byte);
    function Fill2D(Graphics: TDirect2DCanvas; R: TD2DRectF; ABorderOpacity: Byte = 255; AOpacity: Byte = 255; AOpacityTo: Byte = 255; AOpacityMirror: Byte = 255; AOpacityMirrorTo: Byte = 255): TD2DRectF;
    procedure DrawGlow2D(Graphics: TDirect2DCanvas; R: TD2DRectF);
    {$ENDIF}
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SaveToClipBoard;
    procedure LoadFromClipBoard;
    property Focus: Boolean read FFocus write SetFocus default false;
    property FocusColor: TColor read FFocusColor write SetFocusColor default clBlack;
    property FocusRect: TGPRectF read FFocusRect write FFocusRect;
    procedure SaveToFile(FileName: string; Section: String); overload;
    procedure SaveToFile(ini: TIniFile; Section: String); overload;
    procedure LoadFromFile(FileName: string; Section: String); overload;
    procedure LoadFromFile(ini: TIniFile; Section: String); overload;
  published
    property Color: TColor read FColor write SetColor default clSilver;
    property ColorTo: TColor read FColorTo write SetColorTo default clGray;
    property ColorMirror: TColor read FColorMirror write SetColorMirror{ default clNone};
    property ColorMirrorTo: TColor read FColorMirrorTo write SetColorMirrorTo{ default clNone};
    property GradientType: TAdvGradientType read FGradientType write SetGradientType{ default gtVertical};
    property GradientMirrorType: TAdvGradientType read FGradientMirrorType write SetGradientMirrorType{ default gtSolid};
    property HatchStyle: THatchStyle read FHatchStyle write SetHatchStyle default HatchStyleHorizontal;
    property HatchStyleMirror: THatchStyle read FHatchStyleMirror write SetHatchStyleMirror default HatchStyleHorizontal;
    property BackGroundPictureAspectRatio: Boolean read FBackGroundPictureAspectRatio write SetBackGroundPictureAspectRatio default False;
    property BackGroundPictureMode: TFillPictureMode read FBackGroundPictureMode write SetBackGroundPictureMode default pmOutsideFill;
    property BackGroundPicture: TAdvGDIPPicture read FBackGroundPicture write SetBackGroundPicture;
    property BackGroundPicturePosition: TFillPicturePosition read FBackGroundPicturePosition write SetBackGroundPicturePosition default ppTopLeft;
    property BackGroundPictureLeft: integer read FBackGroundPictureLeft write SetBackGroundPictureLeft default 0;
    property BackGroundPictureTop: integer read FBackGroundPictureTop write SetBackGroundPictureTop default 0;
    property BackGroundPictureAspectMode: TPictureMode read FBackGroundPictureAspectMode write SetBackGroundPictureAspectMode default pmStretch;
    property Picture: TAdvGDIPPicture read FPicture write SetPicture;
    property PicturePosition: TFillPicturePosition read FPicturePosition write SetPicturePosition default ppTopLeft;
    property PictureLeft: integer read FPictureLeft write SetPictureLeft default 0;
    property PictureTop: integer read FPictureTop write SetPictureTop default 0;
    property PictureSize: TFillPictureSize read FpictureSize write SetPictureSize default psOriginal;
    property PictureWidth: integer read FpictureWidth write SetPictureWidth default 50;
    property PictureHeight: integer read FpictureHeight write SetPictureHeight default 50;
    property PictureAspectRatio: Boolean read FPictureAspectRatio write SetPictureAspectRatio default False;
    property PictureAspectMode: TPictureMode read FPictureAspectMode write SetPictureAspectMode default pmStretch;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property OpacityTo: Byte read FOpacityTo write SetOpacityTo default 255;
    property OpacityMirror: Byte read FOpacityMirror write SetOpacityMirror default 255;
    property OpacityMirrorTo: Byte read FOpacityMirrorTo write SetOpacityMirrorTo default 255;
    property BorderColor: TColor read FBorderColor write SetBorderColor {default clNone};
    property BorderOpacity: Byte read FBorderOpacity write SetBorderOpacity default 255;
    property BorderWidth: integer read FBorderWidth write SetBorderWidth default 1;
    property Rounding: integer read FRounding write SetRounding {default 0};
    property RoundingType: TFillRoundingType read FRoundingType write SetRoundingType default rtBoth;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property ShadowOffset: integer read FShadowOffset write SetShadowOffset{ default 0};
    property ShadowType: TShadowType read FShadowType write SetShadowType default stRightBottom;
    property Angle: integer read FAngle write SetAngle default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Glow: TGlowMode read FGlow write SetGlow;
    property GlowGradientColor: TColor read FGlowGradientColor write SetGlowGradientColor default clWhite;
    property GlowRadialColor: TColor read FGlowRadialColor write SetGlowRadialColor default clWhite;
  end;

  TGDIPButtonLayout = (blPictureLeft, blPictureRight, blPictureBottom, blPictureTop, blNone);

  TFontStoredEvent = procedure(Sender: TObject; var IsStored: boolean) of object;

  TGDIPButton = class(TPersistent)
  private
    FUpdateCount: integer;
    FFont: TFont;
    FOnChange: TNotifyEvent;
    FOnFontChange: TNotifyEvent;
    FOnIsFontStored: TFontStoredEvent;
    FLayout: TGDIPButtonLayout;
    FSpacing: integer;
    FShiftDown: integer;
    FAlignment: TAlignment;
    FFocusColor: TColor;
    FPictureAlignment: TAlignment;
    FSimpleLayout: Boolean;
    FPictureContainer: TGDIPPictureContainer;
    FImageList: TCustomImageList;
    FPictureName: String;
    FImageIndex: Integer;
    FDisabledImageIndex: Integer;
    FDisabledPictureName: String;
    FGlowPercentage: Integer;
    FRounding: Integer;
    FPictureStretch: Boolean;
    FPictureStretchMode: TPictureMode;
    FWordWrapping: Boolean;
    FSimpleLayoutBorder: Boolean;
    procedure SetFont(const Value: TFont);
    procedure SetLayout(const Value: TGDIPButtonLayout);
    procedure SetSpacing(const Value: integer);
    procedure SetShiftDown(const Value: integer);
    function IsFontStored: boolean;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetFocusColor(const Value: TColor);
    procedure SetPictureAlignment(const Value: TAlignment);
    procedure SetSimpleLayout(const Value: Boolean);
    procedure SetImageIndex(const Value: Integer);
    procedure SetPictureName(const Value: String);
    procedure SetDisabledImageIndex(const Value: Integer);
    procedure SetDisabledPictureName(const Value: String);
    procedure SetGlowPercentage(const Value: Integer);
    procedure SetRounding(const Value: Integer);
    procedure SetPictureStretch(const Value: Boolean);
    procedure SetPictureStretchMode(const Value: TPictureMode);
    procedure SetWordWrapping(const Value: Boolean);
    procedure SetSimpleLayoutBorder(const Value: Boolean);
  protected
    procedure FontChanged(Sender: TObject);
    procedure Changed; virtual;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(g: TGPGraphics; Caption: String; x, y, Width, Height, VerticalSpacing,
      HorizontalSpacing: integer; Color, ColorDown, BevelColor, TextColor: TColor; Shadow, Down, Bevel, VerticalText, RightToLeft: Boolean;
      RoundingType: TFillRoundingType; APicture: TAdvGDIPPicture; TextW, TextH: integer; WW: boolean; AImageIndex: Integer; APictureName: String);
    procedure DoNotification(AOwner, AComponent: TComponent; AOperation: TOperation);
  published
    property GlowPercentage: Integer read FGlowPercentage write SetGlowPercentage default 100;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property PictureAlignment: TAlignment read FPictureAlignment write SetPictureAlignment default taRightJustify;
    property PictureStretch: Boolean read FPictureStretch write SetPictureStretch default False;
    property PictureStretchMode: TPictureMode read FPictureStretchMode write SetPictureStretchMode default pmNormal;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property Layout: TGDIPButtonLayout read FLayout write SetLayout default blPictureLeft;
    property Spacing: integer read FSpacing write SetSpacing default 3;
    property ShiftDown: integer read FShiftDown write SetShiftDown default 1;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnFontChange: TNotifyEvent read FOnFontChange write FOnFontChange;
    property OnIsFontStored: TFontStoredEvent read FOnIsFontStored write FOnIsFontStored;
    property FocusColor: TColor read FFocusColor write SetFocusColor default clBlack;
    property SimpleLayout: Boolean read FSimpleLayout write SetSimpleLayout default False;
    property SimpleLayoutBorder: Boolean read FSimpleLayoutBorder write SetSimpleLayoutBorder default False;
    property ImageList: TCustomImageList read FImageList write FImageList;
    property PictureContainer: TGDIPPictureContainer read FPictureContainer write FPictureContainer;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property PictureName: String read FPictureName write SetPictureName;
    property DisabledPictureName: String read FDisabledPictureName write SetDisabledPictureName;
    property DisabledImageIndex: Integer read FDisabledImageIndex write SetDisabledImageIndex default -1;
    property Rounding: Integer read FRounding write SetRounding default 8;
    property WordWrapping: Boolean read FWordWrapping write SetWordWrapping default True;
  end;

  TGDIPDialogButton = class(TPersistent)
  private
    FFont: TFont;
    FOnChange: TNotifyEvent;
    FLayout: TGDIPButtonLayout;
    FSpacing: integer;
    FCaptionRectF: TGPRectF;
    FDrawCaption: Boolean;
    FCaptionPos: TGPPointF;
    procedure SetFont(const Value: TFont);
    procedure SetLayout(const Value: TGDIPButtonLayout);
    procedure SetSpacing(const Value: integer);
  protected
    procedure FontChanged(Sender: TObject);
    procedure Changed;
  public
    property DrawCaption: Boolean read FDrawCaption write FDrawCaption default True;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(g: TGPGraphics; Caption: String; x, y, Width, Height: integer; Color, BorderColor: TColor;
      BorderWidth: integer; BorderOpacity, Opacity: Byte; Border, Down: Boolean; Picture: TAdvGDIPPicture; WW: boolean);
    property CaptionRect: TGPRectF read FCaptionRectF write FCaptionRectF;
    property CaptionPos: TGPPointF read FCaptionPos write FCaptionPos;
  published
    property Font: TFont read FFont write SetFont;
    property Layout: TGDIPButtonLayout read FLayout write SetLayout default blPictureLeft;
    property Spacing: integer read FSpacing write SetSpacing default 3;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGDIPStatus = class(TPersistent)
  private
    FWidth, FHeight: integer;
    FFill: TGDIPFill;
    FFont: TFont;
    FOnChange: TNotifyEvent;
    FAutoSize: Boolean;
    FSpacing: integer;
    FGlow: Boolean;
    procedure SetFill(const Value: TGDIPFill);
    procedure SetFont(const Value: TFont);
    procedure SetSpacing(const Value: integer);
    procedure SetGlow(const Value: Boolean);
  protected
    procedure FillChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(g: TGPGraphics; x, y, Width, Height: integer; AutoSize: boolean; Caption: String);
    function GetWidth: integer;
    function GetHeight: integer;
    procedure CalculateSize(g: TGPGraphics; Caption: String);    
  published
    property Fill: TGDIPFill read FFill write SetFill;
    property Font: TFont read FFont write SetFont;
    property OnChange: TNotifyEvent read  FOnChange write FOnChange;
    property Spacing: integer read FSpacing write SetSpacing default 0;
    property Glow: Boolean read FGlow write SetGlow default true;
  end;

  TGDIPProgressValueType = (vtPercentage, vtAbsolute);

  TGDIPProgressValuePosition = (vpProgressCenter, vpProgressRight, vpCenter, vpLeft, vpRight, vpCustom);

  TGDIPProgressDrawValueEvent = procedure(Sender: TObject; ValueFormat: String; var ValueText: String) of object;

  TGDIPProgressDirection = (pbdHorizontal, pbdVertical);

  TGDIPProgress = class(TPersistent)
  private
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
    FTransparent: Boolean;
    FShadows: Boolean;
    FOverlays: Boolean;
    FOnDrawValue: TGDIPProgressDrawValueEvent;
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
    procedure SetTransparent(const Value: Boolean);
    procedure SetShadows(const Value: Boolean);
    procedure SetOverlays(const Value: Boolean);
  protected
    procedure FontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure Changed;
    procedure DrawBackGround(g: TGPGraphics; r: TGPRectF);
    procedure DrawShadows(g: TGPGraphics; r: TGPRectF; Direction: TGDIPProgressDirection);
    procedure DrawValue(g: TGPGraphics; r: TRect; min, max, position: double; Direction: TGDIPProgressDirection);
    procedure DrawProgress(g: TGPGraphics; r: TRect; min, max, position: Double; Direction: TGDIPProgressDirection);
    procedure DrawOverLay(g: TGPGraphics; r: TRect; Direction: TGDIPProgressDirection);
    procedure DrawGlow(g: TGPGRaphics; r: TRect; glowposition, min, max, position: Double; Direction: TGDIPProgressDirection);
    {$IFDEF DIRECT2D}
    procedure DrawBackGround2D(g: TDirect2DCanvas; r: TD2DRectF);
    procedure DrawShadows2D(g: TDirect2DCanvas; r: TD2DRectF);
    procedure DrawValue2D(g: TDirect2DCanvas; r: TRect; min, max, position: double);
    procedure DrawProgress2D(g: TDirect2DCanvas; r: TRect; min, max, position: Double);
    procedure DrawOverLay2D(g: TDirect2DCanvas; r: TRect);
    procedure DrawGlow2D(g: TDirect2DCanvas; r: TRect; glowposition, min, max, position: Double);
    procedure GetTextSize2D(g: TDirect2DCanvas; r: TRect; s: String; ft: TFont; var sizer: TD2DRectF);
    {$ENDIF}
    function InsideRect(r: TRect): TRect;
    procedure GetTextSize(g: TGPGraphics; r: TRect; s: String; ft: TFont; var sizer: TGPRectF);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(g: TGPGraphics; r: TRect; min, max, position: Double; Direction: TGDIPProgressDirection);
    {$IFDEF DIRECT2D}
    procedure Draw2D(g: TDirect2DCanvas; r: TRect; min, max, position: Double);
    function GetInsideRectF2D(r: TRect): TD2DRectF;
    function CalculateProgressRectangle2D(r: TRect; min, max, pos: Double): TD2DRectF;
    {$ENDIF}
    function GetInsideRectF(r: TRect): TGPRectF;
    function CalculateProgressRectangle(r: TRect; min, max, pos: Double; Direction:TGDIPProgressDirection): TGPRectF;
  published
    property Transparent: Boolean read FTransparent write SetTransparent default false;
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
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDrawValue: TGDIPProgressDrawValueEvent read FOnDrawValue write FOnDrawValue;
  end;

  //--- CapacityBar
  TLegendPosition = (lpTop, lpBottom, lpNone);
  TOnItemEvent = procedure(Sender: TObject; Index: integer) of object;

  TCapacityItem = class(TCollectionItem)
  private
    FName: string;
    FHint: string;
    FColor: TColor;
    FTag: Integer;
    FDescription: string;
    FValue: Double;
    FColorTo: TColor;
    FLegendFormat: string;
    FDisplayValue: Double;
    FDesRect: TGPRectF;
    FFormatRect: TGPRectF;
    FMarkPT: TGPPointF;
    FWidth: Double;
    FLeft: Double;
    procedure SetColor(const Value: TColor);
    procedure SetDescription(const Value: string);
    procedure SetHint(const Value: string);
    procedure SetName(const Value: string);
    procedure SetTag(const Value: Integer);
    procedure SetValue(const Value: Double);
    procedure SetColorTo(const Value: TColor);
  protected
    procedure SetIndex(Value: Integer); override;
    procedure Change;

    property LegendFormat: string read FLegendFormat write FLegendFormat;  // current display Legend
    property DisplayValue: Double read FDisplayValue write FDisplayValue;
    property Left: Double read FLeft write FLeft;       // Auto Calc
    property Width: Double read FWidth write FWidth;    // Auto calc
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Value: Double read FValue write SetValue;
    property Color: TColor read FColor write SetColor;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property Name: string read FName write SetName;
    property Hint: string read FHint write SetHint;
    property Description: string read FDescription write SetDescription;
    property Tag: Integer read FTag write SetTag default 0;
  end;

  TCapacityItems = class(TCollection)
  private
    FOwner: TPersistent;
    FOnChange: TNotifyEvent;
    FOnDeleteItem: TOnItemEvent;
    FDownItem: TCapacityItem;
    FOnCreateItem: TOnItemEvent;
    function GetItem(Index: Integer): TCapacityItem;
    procedure SetItem(Index: Integer; const Value: TCapacityItem);
    procedure SetDownItem(const Value: TCapacityItem);
  protected
    procedure Change;
    property DownItem: TCapacityItem read FDownItem write SetDownItem; 
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TCapacityItem read GetItem write SetItem; default;
    function Add: TCapacityItem;
    function Insert(Index: Integer): TCapacityItem;
    function GetOwner: TPersistent; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDeleteItem: TOnItemEvent read FOnDeleteItem write FOnDeleteItem;
    property OnCreateItem: TOnItemEvent read FOnCreateItem write FOnCreateItem;
  end;

  TGetCapacityFormatEvent = procedure (Sender: TObject; var Format: string; var Capacity: Double) of object;
  TGetLegendFormatEvent = procedure (Sender: TObject; Item: TCapacityItem; var Format: string; var Value: Double) of object;

  TGDIPCapacityBar = class(TPersistent)
  private
    FBackGroundFill: TGDIPFill;
    FCapacityFont: TFont;
    FOnChange: TNotifyEvent;
    FLegendFont: TFont;
    FTransparent: Boolean;
    FCapacityFormat: string;
    FReflectionOpacityStart: Integer;
    FRounded: Boolean;
    FDivisions: Integer;
    FReflectionAxis: Integer;
    FColor: TColor;
    FColorTo: TColor;
    FReflectionOpacityEnd: Integer;
    FLegendFormat: string;
    FReflection: Boolean;
    FLegendPos: TLegendPosition;
    FOnGetLegendFormat: TGetLegendFormatEvent;
    FOnGetCapacityFormat: TGetCapacityFormatEvent;
    FCapDesRect: TGPRectF;
    FCapFormatRect: TGPRectF;
    FLegendRect: TGPRectF;
    FBarRect: TGPRectF;
    FFreeMarkPT: TGPPointF;
    FFreeDesRect: TGPRectF;
    FFreeFormatRect: TGPRectF;
    FOffsetX: Integer;
    FOffsetY: Integer;
    FTextGap: Integer;
    FMarkSize: Integer;
    FFreeSpace: Double;
    FFreeFormat: string;
    FFreeLeft: Double;
    FFreeWidth: Double;
    FReflectionPic: TGPBitmap;
    FUpdateReflection: Boolean;
    FCapacityTextShadowColor: TColor;
    FLegendTextShadowColor: TColor;
    FAutoFormatValues: Boolean;
    FShowLegend: Boolean;
    FShowTotal: Boolean;
    FShowFree: Boolean;
    procedure SetBackGroundFill(const Value: TGDIPFill);
    procedure SetCapacityFont(const Value: TFont);
    procedure SetLegendFont(const Value: TFont);
    procedure SetTransparent(const Value: Boolean);
    procedure SetCapacityFormat(const Value: string);
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetDivisions(const Value: Integer);
    procedure SetLegendFormat(const Value: string);
    procedure SetLegendPos(const Value: TLegendPosition);
    procedure SetReflectionAxis(const Value: Integer);
    procedure SetReflectionOpacityEnd(const Value: Integer);
    procedure SetReflectionOpacityStart(const Value: Integer);
    procedure SetRounded(const Value: Boolean);
    procedure SetReflection(const Value: Boolean);
    procedure SetOffsetX(const Value: Integer);
    procedure SetOffsetY(const Value: Integer);
    procedure SetTextGap(const Value: Integer);
    procedure SetFreeFormat(const Value: string);
    procedure SetCapacityTextShadowColor(const Value: TColor);
    procedure SetLegendTextShadowColor(const Value: TColor);
    procedure SetAutoFormatValues(const Value: Boolean);
    procedure SetShowLegend(const Value: Boolean);
    procedure SetShowTotal(const Value: Boolean);
    procedure SetShowFree(const Value: Boolean);
  protected
    function GetSizeValue(AValue: Double): String;
    procedure FontChanged(Sender: TObject);
    procedure BackGroundFillChanged(Sender: TObject);
    procedure Changed;
    procedure DrawBackGround(g: TGPGraphics; R: TGPRectF);
    procedure DrawCapacityDescription(g: TGPGraphics; R: TGPRectF; CapacityDes, DesFormat: string);
    procedure DrawLegend(g: TGPGraphics; R: TGPRectF; Items: TCapacityItems; FreeDes, FreeDesFormat: string);
    procedure DrawItem(g: TGPGraphics; Item: TCapacityItem); overload; // Item = nill : Free space
    procedure DrawItem(g: TGPGraphics; RF: TGPRectF; Clr, ClrTo: TColor; StartLeft: Integer); overload;
    procedure DrawBar(g: TGPGraphics; R: TGPRectF; Items: TCapacityItems);
    function InsideRect(r: TRect): TRect;
    procedure GetTextSize(g: TGPGraphics; r: TRect; s: String; ft: TFont; var sizer: TGPRectF);
    procedure CalculateCapacityBarRect(g: TGPGraphics; R: TRect; TotalCapacity: Double; Items: TCapacityItems;
      CapacityDes, DesFormat, FreeDes, FreeDesFormat: string);
    function PtOnItem(pt: TPoint; Items: TCapacityItems): TCapacityItem;
    procedure UpdateReflection;

    property OnGetCapacityFormat: TGetCapacityFormatEvent read FOnGetCapacityFormat write FOnGetCapacityFormat; // For component class
    property OnGetLegendFormat: TGetLegendFormatEvent read FOnGetLegendFormat write FOnGetLegendFormat;  // For component class
    property ReflectionAxis: Integer read FReflectionAxis write SetReflectionAxis default 1;
    property FreeSpace: Double read FFreeSpace;

    property TextGap: Integer read FTextGap write SetTextGap default 3;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(g: TGPGraphics; r: TRect; TotalCapacity: Double; Items: TCapacityItems; CapacityDes, DesFormat, FreeDes, FreeDesFormat: string; AntiAlias: TAntiAlias);
    function GetInsideRectF(r: TRect): TGPRectF;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property ShowLegend: Boolean read FShowLegend write SetShowLegend default True;
    property ShowFree: Boolean read FShowFree write SetShowFree default True;
    property ShowTotal: Boolean read FShowTotal write SetShowTotal default True;
    property AutoFormatValues: Boolean read FAutoFormatValues write SetAutoFormatValues;
    property BackGroundFill: TGDIPFill read FBackGroundFill write SetBackGroundFill;
    property Color: TColor read FColor write SetColor;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property CapacityFont: TFont read FCapacityFont write SetCapacityFont;
    property CapacityFormat: string read FCapacityFormat write SetCapacityFormat;
    property FreeFormat: string read FFreeFormat write SetFreeFormat;
    property Divisions: Integer read FDivisions write SetDivisions default 20;
    property LegendPos: TLegendPosition read FLegendPos write SetLegendPos default lpBottom;
    property LegendFormat: string read FLegendFormat write SetLegendFormat;
    property LegendFont: TFont read FLegendFont write SetLegendFont;
    property Rounded: Boolean read FRounded write SetRounded default True;
    property Reflection: Boolean read FReflection write SetReflection default True;
    property ReflectionOpacityStart: Integer read FReflectionOpacityStart write SetReflectionOpacityStart default 150;
    property ReflectionOpacityEnd: Integer read FReflectionOpacityEnd write SetReflectionOpacityEnd default 0;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property OffsetX: Integer read FOffsetX write SetOffsetX default 15;
    property OffsetY: Integer read FOffsetY write SetOffsetY default 8;
    property CapacityTextShadowColor: TColor read FCapacityTextShadowColor write SetCapacityTextShadowColor default clBlack;
    property LegendTextShadowColor: TColor read FLegendTextShadowColor write SetLegendTextShadowColor default clBlack;
  end;

var
  CF_GDIPFILL: Word;

  //Exported Functions
  function CreateRoundRectangle(R: TGPRectF; Radius: Integer; RoundingType: TFillRoundingType; Mirror: Boolean): TGPGraphicsPath; overload;
  procedure DrawLed(g: TGPGraphics; r: TGPRectF; ValueType: TFillValueType; Format: String; TimeFormat: String; Value: Double; TimeValue: TDateTime;
  ColorOff: TColor; ColorOffOpacity: Byte; ColorStart, ColorEnd: TColor; OpacityStart, OpacityEnd: Byte; GradientType: TAdvGradientType;
  Angle: Integer; HatchStyle:THatchStyle; Picture: TAdvGDIPPicture);
  procedure GetAspectSize(var w, h: Double; ow, oh, nw, nh: double; mode: TPictureMode);

  {$IFDEF DIRECT2D}
  function Create2DTextFormat(AFont: TFont; AHAlign: TAlignment; AVAlign: TVerticalAlignment;
  AWrap, ARightToLeft: boolean): IDWriteTextFormat;
  function DefaultBitmapBrushProperties: TD2D1BitmapBrushProperties;
  {$ENDIF}

implementation

uses
  ActiveX;

type
  TSaver = class(TComponent)
  private
    FSaveObject: TPersistent;
  published
    property SaveObject: TPersistent read FSaveObject write FSaveObject;
end;

{$I DELPHIXE.INC}

procedure GetAspectSize(var w, h: Double; ow, oh, nw, nh: double; mode: TPictureMode);
begin
  if (ow > 0) and (oh > 0) and (nw > 0) and (nh > 0) then
  begin
    if (ow < nw) and (oh < nh) and (mode = pmNormal) then
    begin
      w := ow;
      h := oh;
    end
    else
    begin
      if ow / oh < nw / nh then
      begin
        h := nh;
        w := nh * ow / oh;
      end
      else
      begin
        w := nw;
        h := nw * oh / ow;
      end;
    end;
  end
  else
  begin
    w := 0;
    h := 0;
  end;
end;

{$IFDEF DIRECT2D}
function Create2DTextFormat(AFont: TFont; AHAlign: TAlignment; AVAlign: TVerticalAlignment;
  AWrap, ARightToLeft: boolean): IDWriteTextFormat;
var
  fontWeight: DWRITE_FONT_WEIGHT;
  fontStyle: DWRITE_FONT_STYLE;
begin
  if fsBold in AFont.Style then
    fontWeight := DWRITE_FONT_WEIGHT_BOLD
  else
    fontWeight := DWRITE_FONT_WEIGHT_NORMAL;
  if fsItalic in AFont.Style then
    fontStyle := DWRITE_FONT_STYLE_ITALIC
  else
    fontStyle := DWRITE_FONT_STYLE_NORMAL;

  DWriteFactory.CreateTextFormat(PChar(AFont.Name), nil, fontWeight, fontStyle, DWRITE_FONT_STRETCH_NORMAL, Abs(AFont.height), '', result);

  case AHAlign of
    taLeftJustify:
      result.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_LEADING);
    taRightJustify:
      result.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_TRAILING);
    taCenter:
      result.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_CENTER);
  end;

  case AVAlign of
    taAlignTop:
      result.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_NEAR);
    taAlignBottom:
      result.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_FAR);
    taVerticalCenter:
      result.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_CENTER);
  end;

  if AWrap then
    result.SetWordWrapping(DWRITE_WORD_WRAPPING_WRAP)
  else
    result.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP);

  if ARightToLeft then
    result.SetReadingDirection(DWRITE_READING_DIRECTION_RIGHT_TO_LEFT)
  else
    result.SetReadingDirection(DWRITE_READING_DIRECTION_LEFT_TO_RIGHT);
end;

function DefaultBitmapBrushProperties: TD2D1BitmapBrushProperties;
begin
  result.extendModeX := D2D1_EXTEND_MODE_WRAP;
  result.extendModeY := D2D1_EXTEND_MODE_WRAP;
  result.interpolationMode := D2D1_BITMAP_INTERPOLATION_MODE_LINEAR;
end;

{$ENDIF}

procedure SaveObject(AStream: TStream; AObject: TPersistent);
var
  ASaver: TSaver;
begin
  Assert(Assigned(AObject));
  ASaver := TSaver.Create(nil);
  try
    ASaver.SaveObject := AObject;
    AStream.WriteComponent(ASaver);
  finally
    ASaver.Free;
  end;
end;

procedure LoadObject(AStream: TStream; AObject: TPersistent);
var
  ASaver: TSaver;
begin
  Assert(Assigned(AObject));
  ASaver := TSaver.Create(nil);
  try
    ASaver.SaveObject := AObject;
    AStream.ReadComponent(ASaver);
  finally
    ASaver.Free;
  end;
end;


function StreamToString(ms: TMemoryStream): string;
var
  i: integer;
  b: byte;
begin
  Result := '';
  ms.Position := 0;
  for i := 1 to ms.Size do
  begin
    ms.Read(b,1);
    Result := Result + inttohex(b,2);
  end;
end;

function HexToInt(s: string): byte;
var
  b: Byte;
  c: Char;
begin
  Result := 0;
  s := UpperCase(s);
  for b := 1 to 2 do
  begin
    Result := Result * 16;
    c := s[b];
    case c of
      '0'..'9': Inc(Result, Ord(c) - Ord('0'));
      'A'..'F': Inc(Result, Ord(c) - Ord('A') + 10);
      else
        raise EConvertError.Create('No Hex-Number');
    end;
  end;
end;

procedure StringToStream(value: string; ms: TMemoryStream);
var
  i: integer;
  s: string;
  l: integer;
  b: byte;
begin
  l := length(value) div 2;
  ms.Clear;

  for i := 1 to l do
  begin
    s := value[i * 2 - 1] + value[i * 2];
    b := HexToInt(s);
    ms.Write(b,1);
  end;
end;


procedure Load(var f: TGDIPFill);
var
  hbuf    : THandle;
  bufptr  : Pointer;
  mstream : TMemoryStream;
begin
  hbuf := ClipBoard.GetAsHandle(CF_GDIPFILL);
  if hbuf <> 0 then
  begin
    bufptr := GlobalLock(hbuf);
    if bufptr <> nil then begin
      try
        mstream := TMemoryStream.Create;
        try
          mstream.WriteBuffer(bufptr^, GlobalSize(hbuf));
          mstream.Position := 0;
          LoadObject(mstream, f);
        finally
          mstream.Free;
        end;
      finally
        GlobalUnlock(hbuf);
      end;
    end;
  end;
end; 

procedure Save(f: TGDIPFill);
var
  hbuf    : THandle;
  bufptr  : Pointer;
  mstream : TMemoryStream;
begin
  mstream := TMemoryStream.Create;
  try
    SaveObject(mstream, f);
    hbuf := GlobalAlloc(GMEM_MOVEABLE, mstream.size);
    try
      bufptr := GlobalLock(hbuf);
      try
        Move(mstream.Memory^, bufptr^, mstream.size);
        Clipboard.SetAsHandle(CF_GDIPFILL, hbuf);
      finally
        GlobalUnlock(hbuf);
      end;
    except
      GlobalFree(hbuf);
      raise;
    end;
  finally
    mstream.Free;
  end;
end;

function GetX(x: Double; width: Double): Double;
begin
  Result := x * width / 12;
end;

function GetY(y: Double; height: Double): Double;
begin
  Result := y * height / 15;
end;

function IsNumberAvailable(Number: integer; const listOfNumbers: array of integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if (Length(listOfNumbers) > 0) then
  begin
    for i := 0 to Length(ListOfNumbers) - 1 do
    begin
      if (ListOfNumbers[i] = number) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

procedure DrawDigit(g: TGPGraphics; number: Integer; position: TGPPointF; Width, Height: integer;  b: TGPBrush; l: integer; ColorOff: TColor; ColorOffOpacity: Byte);
type
  TArrayOfPointF = array of TGPPointF;
var
  segmentG: TArrayOfPointF;
  segmentF: TArrayOfPointF;
  segmentE: TArrayOfPointF;
  segmentD: TArrayOfPointF;
  segmentC: TArrayOfPointF;
  segmentB: TArrayOfPointF;
  segmentA: TArrayOfPointF;
  ot: TGPPen;
  w: Double;
  bo: TGPBrush;
begin
  ot := nil;
  try
    w := Width / l;

    ot := TGPPen.Create(MakeColor(ColorOffOpacity, ColorOff));

    SetLength(segmentA, 5);
    segmentA[0] := MakePoint((position.X + GetX(2.8, w)), (position.Y + GetY(1, Height)));
    segmentA[1] := MakePoint((position.X + GetX(10, w)), (position.Y + GetY(1, Height)));
    segmentA[2] := MakePoint((position.X + GetX(8.8, w)), (position.Y + GetY(2, Height)));
    segmentA[3] := MakePoint((position.X + GetX(3.8, w)), (position.Y + GetY(2, Height)));
    segmentA[4] := segmentA[0];

    SetLength(segmentB, 5);
    segmentB[0] := MakePoint((position.X + GetX(10, w)), (position.Y + GetY(1.4, Height)));
    segmentB[1] := MakePoint((position.X + GetX(9.3, w)), (position.Y + GetY(6.8, Height)));
    segmentB[2] := MakePoint((position.X + GetX(8.4, w)), (position.Y + GetY(6.4, Height)));
    segmentB[3] := MakePoint((position.X + GetX(9, w)), (position.Y + GetY(2.2, Height)));
    segmentB[4] := segmentB[0];

    SetLength(segmentC, 5);
    segmentC[0] := MakePoint((position.X + GetX(9.2, w)), (position.Y + GetY(7.2, Height)));
    segmentC[1] := MakePoint((position.X + GetX(8.7, w)), (position.Y + GetY(12.7, Height)));
    segmentC[2] := MakePoint((position.X + GetX(7.6, w)), (position.Y + GetY(11.9, Height)));
    segmentC[3] := MakePoint((position.X + GetX(8.2, w)), (position.Y + GetY(7.7, Height)));
    segmentC[4] := segmentC[0];

    SetLength(segmentD, 5);
    segmentD[0] := MakePoint((position.X + GetX(7.4, w)), (position.Y + GetY(12.1, Height)));
    segmentD[1] := MakePoint((position.X + GetX(8.4, w)), (position.Y + GetY(13, Height)));
    segmentD[2] := MakePoint((position.X + GetX(1.3, w)), (position.Y + GetY(13, Height)));
    segmentD[3] := MakePoint((position.X + GetX(2.2, w)), (position.Y + GetY(12.1, Height)));
    segmentD[4] := segmentD[0];

    SetLength(segmentE, 5);
    segmentE[0] := MakePoint((position.X + GetX(2.2, w)), (position.Y + GetY(11.8, Height)));
    segmentE[1] := MakePoint((position.X + GetX(1, w)), (position.Y + GetY(12.7, Height)));
    segmentE[2] := MakePoint((position.X + GetX(1.7, w)), (position.Y + GetY(7.2, Height)));
    segmentE[3] := MakePoint((position.X + GetX(2.8, w)), (position.Y + GetY(7.7, Height)));
    segmentE[4] := segmentE[0];

    SetLength(segmentF, 5);
    segmentF[0] := MakePoint((position.X + GetX(3, w)), (position.Y + GetY(6.4, Height)));
    segmentF[1] := MakePoint((position.X + GetX(1.8, w)), (position.Y + GetY(6.8, Height)));
    segmentF[2] := MakePoint((position.X + GetX(2.6, w)), (position.Y + GetY(1.3, Height)));
    segmentF[3] := MakePoint((position.X + GetX(3.6, w)), (position.Y + GetY(2.2, Height)));
    segmentF[4] := segmentF[0];

    SetLength(segmentG, 7);
    segmentG[0] := MakePoint((position.X + GetX(2, w)), (position.Y + GetY(7, Height)));
    segmentG[1] := MakePoint((position.X + GetX(3.1, w)), (position.Y + GetY(6.5, Height)));
    segmentG[2] := MakePoint((position.X + GetX(8.3, w)), (position.Y + GetY(6.5, Height)));
    segmentG[3] := MakePoint((position.X + GetX(9, w)), (position.Y + GetY(7, Height)));
    segmentG[4] := MakePoint((position.X + GetX(8.2, w)), (position.Y + GetY(7.5, Height)));
    segmentG[5] := MakePoint((position.X + GetX(2.9, w)), (position.Y + GetY(7.5, Height)));
    segmentG[6] := segmentG[0];

    if ColorOff <> clNone then
    begin
      bo := ot.GetBrush;
      g.FillPolygon(bo, PGPpointF(segmentA), Length(segmentA));
      g.FillPolygon(bo, PGPpointF(segmentB), Length(segmentB));
      g.FillPolygon(bo, PGPpointF(segmentC), Length(segmentC));
      g.FillPolygon(bo, PGPpointF(segmentD), Length(segmentD));
      g.FillPolygon(bo, PGPpointF(segmentE), Length(segmentE));
      g.FillPolygon(bo, PGPpointF(segmentF), Length(segmentF));
      g.FillPolygon(bo, PGPpointF(segmentG), Length(segmentG));
      bo.free;
    end;

    if IsNumberAvailable(number, [0, 2, 3, 5, 6, 7, 8, 9]) then
      g.FillPolygon(b, PGPpointF(segmentA), Length(SegmentA));
    if IsNumberAvailable(number, [0, 1, 2, 3, 4, 7, 8, 9]) then
      g.FillPolygon(b, PGPpointF(segmentB), Length(SegmentB));
    if IsNumberAvailable(number, [0, 1, 3, 4, 5, 6, 7, 8, 9]) then
      g.FillPolygon(b, PGPpointF(segmentC), Length(SegmentC));
    if IsNumberAvailable(number, [0, 2, 3, 5, 6, 8, 9]) then
      g.FillPolygon(b, PGPpointF(segmentD), Length(SegmentD));
    if IsNumberAvailable(number, [0, 2, 6, 8]) then
      g.FillPolygon(b, PGPpointF(segmentE), Length(SegmentE));
    if IsNumberAvailable(number, [0, 4, 5, 6, 8, 9]) then
      g.FillPolygon(b, PGPpointF(segmentF), Length(SegmentF));
    if IsNumberAvailable(number, [2, 3, 4, 5, 6, 8, 9, -1]) then
      g.FillPolygon(b, PGPpointF(segmentG), Length(SegmentG));
    if IsNumberAvailable(number, [-2]) then
    begin
      g.FillEllipse(b, MakeRect((position.X - (Width / l) + GetX(11.2, w)),(position.Y + GetY(3, Height)), (w / 7), (w / 7)));
      g.FillEllipse(b, MakeRect((position.X - (Width / l) + GetX(10, w)),(position.Y + GetY(9.5, Height)), (w / 7), (w / 7)));
    end;
    if IsNumberAvailable(number, [-3]) then
      g.FillEllipse(b, MakeRect((position.X - (Width / l) + GetX(10, w)),(position.Y + GetY(12, Height)), (w / 7), (w / 7)));
  finally
    ot.Free;
  end;

end;

procedure DrawLed(g: TGPGraphics; r: TGPRectF; ValueType: TFillValueType; Format: String; TimeFormat: String; Value: Double; TimeValue: TDateTime;
  ColorOff: TColor; ColorOffOpacity: Byte; ColorStart, ColorEnd: TColor; OpacityStart, OpacityEnd: Byte; GradientType: TAdvGradientType;
  Angle: Integer; HatchStyle:THatchStyle; Picture: TAdvGDIPPicture);
var
  num: String;
  i: Integer;
  shift: Double;
  numch: char;
  dRect: TGPRectF;
  textb: TGPBrush;
  start, stop: TGPColor;
  xs, ys, w, h: double;
  p: TGPPointF;
  gpimg: TGPImage;
  st: TStream;
  sta: TStreamAdapter;
  total: integer;
begin
  dRect := MakeRect(r.X, r.Y, r.Width, r.Height);
  case ValueType of
    vtNormal: num := FormatFloat(Format, Value);
    vtDateTime: num := FormatDateTime(TimeFormat, TimeValue)
  end;
  shift := 0;

  xs := drect.X;
  ys := drect.Y;
  w := drect.Width;
  h := drect.Height;
  textb := nil;

  p := MakePoint(xs, ys);

  start := MakeColor(OpacityStart, ColorStart);
  stop := MakeColor(OpacityEnd, ColorEnd);

  case GradientType of
    gtAngle: textb := TGPLinearGradientBrush.Create(Makerect(xs, ys, w, h), start, stop, Angle);
    gtRadial, gtPath, gtSolid: textb := TGPSolidBrush.Create(start);
    gtVertical: textb := TGPLinearGradientBrush.Create(MakePoint(xs, ys), MakePoint(xs, ys + h), start, stop);
    gtHorizontal: textb := TGPLinearGradientBrush.Create(MakePoint(xs, ys), MakePoint(xs + w, ys), start, stop);
    gtForwardDiagonal: textb := TGPLinearGradientBrush.Create(MakePoint(xs, ys), MakePoint(xs + w, ys + h), start, stop);
    gtBackwardDiagonal: textb := TGPLinearGradientBrush.Create(MakePoint(xs, ys + h), MakePoint(xs + w, ys), stop, start);
    gtHatch: textb := TGPHatchBrush.Create(HatchStyle, start, stop);
    gtTexture:
    begin
      if not Picture.Empty then
      begin
        st := TMemoryStream.Create;
        Picture.SaveToStream(st);
        sta := TStreamAdapter.Create(st);
        gpimg := TGPImage.Create(sta);
        textb := TGPTextureBrush.Create(gpimg, WrapModeTile);
        st.free;
        gpimg.free;
      end;
    end;
    gtNone: ;
  end;

  total := 0;
  for i := 1 to length(num) do
  begin
    {$IFDEF DELPHIXE3_LVL}
    if (num[I] = FormatSettings.DecimalSeparator) then
      num[I] := '.';
    {$ELSE}
    if (num[I] = DecimalSeparator) then
      num[I] := '.';
    {$ENDIF}

    if (num[I] <> '.') and (num[I] <> ':') then
    begin
      Inc(total);
    end;
  end;

  if textb <> nil then
  begin
    for i := 1 to length(num) do
    begin
      numch := num[i];
      if numch = '.' then
        DrawDigit(g, -3, MakePoint((drect.X + shift), drect.Y), Round(DrecT.Width), Round(drect.Height), textb, total, ColorOff, ColorOffOpacity)
      else
      begin
        case numch of
        ':':
          begin
            DrawDigit(g, -2, MakePoint((drect.X + shift), drect.Y), Round(DrecT.Width), Round(drect.Height), textb, total, ColorOff, ColorOffOpacity);
          end;
        '-':
          begin
            DrawDigit(g, -1, MakePoint((drect.X + shift), drect.Y), Round(DrecT.Width), Round(drect.Height), textb, total, ColorOff, ColorOffOpacity);
            shift := shift + (Drect.Width / total);
          end;
        '0'..'9':
          begin
            DrawDigit(g, ord(numch) - ord('0') , MakePoint((drect.X + shift), drect.Y), Round(DrecT.Width), Round(drect.Height), textb, total, ColorOff, ColorOffOpacity);
            shift := shift + (Drect.Width / total);
          end;
        end;
      end;
    end;
    textb.Free;
  end;
end;

procedure GetPath(l, t, w, h, d, radius: Double; RoundingType: TFillRoundingType; var path: TGPGraphicsPath);
begin
  case RoundingType of
    rtNone:
    begin
      path.AddLine(l, t, l + w, t); // top
      path.AddLine(l + w, t, l + w, t + h); // right
      path.AddLine(l + w, t + h, l, t + h); // bottom
      path.AddLine(l, t + h, l, t); // left
    end;
    rtRight:
    begin
      path.AddLine(l, t, l + w - radius, t); // top
      path.AddArc(l + w - d, t, d, d, 270, 90); // topright
      path.AddLine(l + w, t + radius, l + w, t + h - radius); // right
      path.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
      path.AddLine(l + w - radius, t + h, l, t + h); // bottom
      path.AddLine(l, t + h - radius, l, t + radius); // left
    end;
    rtLeft:
    begin
      path.AddArc(l, t, d, d, 180, 90); // topleft
      path.AddLine(l + radius, t, l + w - radius, t); // top
      path.AddLine(l + w, t, l + w, t + h); // right
      path.AddLine(l + w - radius, t + h, l + radius, t + h); // bottom
      path.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
      path.AddLine(l, t + h - radius, l, t + radius); // left
    end;
    rtTop:
    begin
      path.AddArc(l, t, d, d, 180, 90); // topleft
      path.AddLine(l + radius, t, l + w - radius, t); // top
      path.AddArc(l + w - d, t, d, d, 270, 90); // topright
      path.AddLine(l + w, t + radius, l + w, t + h); // right
      path.AddLine(l + w, t + h, l, t + h); // bottom
      path.AddLine(l, t + h, l, t + Radius); // left
    end;
    rtBottom:
    begin
      path.AddLine(l, t, l + w, t); // top
      path.AddLine(l + w, t, l + w, t + h - radius); // right
      path.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
      path.AddLine(l + w - radius, t + h, l + radius, t + h); // bottom
      path.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
      path.AddLine(l, t + h - Radius, l, t ); // left
    end;
    rtBoth:
    begin
      path.AddArc(l, t, d, d, 180, 90); // topleft
      path.AddLine(l + radius, t, l + w - radius, t); // top
      path.AddArc(l + w - d, t, d, d, 270, 90); // topright
      path.AddLine(l + w, t + radius, l + w, t + h - radius); // right
      path.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
      path.AddLine(l + w - radius, t + h, l + radius, t + h); // bottom
      path.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
      path.AddLine(l, t + h - radius, l, t + radius); // left
    end;
  end;
end;

{$IFDEF DIRECT2D}

procedure GetPath2D(l, t, w, h, d, radius: Double; RoundingType: TFillRoundingType; var path: ID2D1PathGeometry);
var
  sink2D: ID2D1GeometrySink;
begin
  path.Open(sink2D);
  case RoundingType of
    rtNone:
    begin
      sink2D.BeginFigure(D2D1PointF(l, t), D2D1_FIGURE_BEGIN_FILLED);
      sink2D.AddLine(D2D1PointF(l + w, t));
      sink2D.AddLine(D2D1PointF(l + w, t + h)); // right
      sink2D.AddLine(D2D1PointF(l, t + h));
    end;
    rtRight:
    begin
      sink2D.BeginFigure(D2D1PointF(l + w, t + d), D2D1_FIGURE_BEGIN_FILLED);
      sink2D.AddArc(D2D1ArcSegment(D2D1PointF(l + w - d, t), D2D1SizeF(d, d), 0, D2D1_SWEEP_DIRECTION_COUNTER_CLOCKWISE, D2D1_ARC_SIZE_SMALL));
      sink2D.AddLine(D2D1PointF(l - d, t));
      sink2D.AddLine(D2D1PointF(l , t));
      sink2D.AddLine(D2D1PointF(l , t + h));
      sink2D.AddLine(D2D1PointF(l + w - d, t + h));
      sink2D.AddArc(D2D1ArcSegment(D2D1PointF(l + w, t + h - d), D2D1SizeF(d, d), 0, D2D1_SWEEP_DIRECTION_COUNTER_CLOCKWISE, D2D1_ARC_SIZE_SMALL));
    end;
    rtLeft:
    begin
      sink2D.BeginFigure(D2D1PointF(l, t + d), D2D1_FIGURE_BEGIN_FILLED);
      sink2D.AddArc(D2D1ArcSegment(D2D1PointF(l + d, t), D2D1SizeF(d, d), 0, D2D1_SWEEP_DIRECTION_CLOCKWISE, D2D1_ARC_SIZE_SMALL));
      sink2D.AddLine(D2D1PointF(l + w - d, t));
      sink2D.AddLine(D2D1PointF(l + w, t));
      sink2D.AddLine(D2D1PointF(l + w, t + h));
      sink2D.AddLine(D2D1PointF(l + d, t + h));
      sink2D.AddArc(D2D1ArcSegment(D2D1PointF(l, t + h - d), D2D1SizeF(d, d), 0, D2D1_SWEEP_DIRECTION_CLOCKWISE, D2D1_ARC_SIZE_SMALL));
    end;
    rtTop:
    begin
      sink2D.BeginFigure(D2D1PointF(l, t + d), D2D1_FIGURE_BEGIN_FILLED);
      sink2D.AddArc(D2D1ArcSegment(D2D1PointF(l + d, t), D2D1SizeF(d, d), 0, D2D1_SWEEP_DIRECTION_CLOCKWISE, D2D1_ARC_SIZE_SMALL));
      sink2D.AddLine(D2D1PointF(l + w - d, t));
      sink2D.AddArc(D2D1ArcSegment(D2D1PointF(l + w, t + d), D2D1SizeF(d, d), 0, D2D1_SWEEP_DIRECTION_CLOCKWISE, D2D1_ARC_SIZE_SMALL));
      sink2D.AddLine(D2D1PointF(l + w, t + d));
      sink2D.AddLine(D2D1PointF(l + w, t + h));
      sink2D.AddLine(D2D1PointF(l, t + h));
    end;
    rtBottom:
    begin
      sink2D.BeginFigure(D2D1PointF(l, t), D2D1_FIGURE_BEGIN_FILLED);
      sink2D.AddLine(D2D1PointF(l + w, t));
      sink2D.AddLine(D2D1PointF(l + w, t + h - d));
      sink2D.AddArc(D2D1ArcSegment(D2D1PointF(l + w - d, t + h), D2D1SizeF(d, d),0, D2D1_SWEEP_DIRECTION_CLOCKWISE, D2D1_ARC_SIZE_SMALL));
      sink2D.AddLine(D2D1PointF(l + w - d, t + h));
      sink2D.AddLine(D2D1PointF(l + d, t + h));
      sink2D.AddArc(D2D1ArcSegment(D2D1PointF(l, t + h - d), D2D1SizeF(d, d), 0, D2D1_SWEEP_DIRECTION_CLOCKWISE, D2D1_ARC_SIZE_SMALL));
    end;
    rtBoth:
    begin
      sink2D.BeginFigure(D2D1PointF(l, t + d), D2D1_FIGURE_BEGIN_FILLED);
      sink2D.AddArc(D2D1ArcSegment(D2D1PointF(l + d, t), D2D1SizeF(d, d), 0, D2D1_SWEEP_DIRECTION_CLOCKWISE, D2D1_ARC_SIZE_SMALL));
      sink2D.AddLine(D2D1PointF(l + w - d, t));
      sink2D.AddArc(D2D1ArcSegment(D2D1PointF(l + w, t + d), D2D1SizeF(d, d), 0, D2D1_SWEEP_DIRECTION_CLOCKWISE, D2D1_ARC_SIZE_SMALL));
      sink2D.AddLine(D2D1PointF(l + w, t + d));
      sink2D.AddLine(D2D1PointF(l + w, t + h - d));
      sink2D.AddArc(D2D1ArcSegment(D2D1PointF(l + w - d, t + h), D2D1SizeF(d, d),0, D2D1_SWEEP_DIRECTION_CLOCKWISE, D2D1_ARC_SIZE_SMALL));
      sink2D.AddLine(D2D1PointF(l + w - d, t + h));
      sink2D.AddLine(D2D1PointF(l + d, t + h));
      sink2D.AddArc(D2D1ArcSegment(D2D1PointF(l, t + h - d), D2D1SizeF(d, d), 0, D2D1_SWEEP_DIRECTION_CLOCKWISE, D2D1_ARC_SIZE_SMALL));
    end;
  end;
  sink2D.EndFigure(D2D1_FIGURE_END_CLOSED);
  sink2D.Close;
end;
{$ENDIF}

{$IFDEF DIRECT2D}
function CreateRoundRectangle2D(R: TD2DRectF; Radius: Integer; RoundingType: TFillRoundingType; Mirror: Boolean): ID2D1PathGeometry;
var
  l, t, w, h, d: Double;
begin
  D2DFactory.CreatePathGeometry(Result);
  l := R.left;
  t := R.top;
  w := R.right - R.left;
  h := R.bottom - R.top;

  if radius > 0 then
    d := (Radius / 2) + 5
  else
    d := 0;
  GetPath2D(l, t, w, h, d, radius, RoundingType, Result);
end;
{$ENDIF}

function CreateRoundRectangle(R: TGPRectF; Radius: Integer; RoundingType: TFillRoundingType; Mirror: Boolean): TGPGraphicsPath; overload;
var
  l, t, w, h, d: Double;
begin
  Result := TGPGraphicsPath.Create;
  l := R.X;
  t := R.Y;
  w := R.Width;
  h := R.Height;
  d := Radius shl 1;
  GetPath(l, t, w, h, d, radius, RoundingType, Result);
  Result.CloseFigure();
end;

function DrawGDIPText(graphics: TGPGraphics; Alignment: TAlignment; RF: TGPRectF; Caption:string; WideCaption: widestring; AFont: TFont; Enabled: Boolean; RealDraw: Boolean; OutLineClr: TColor; Shadow: Boolean; ShadowClr: TColor): TGPRectF;
var
  w,h: Integer;
  fontFamily: TGPFontFamily;
  font: TGPFont;
  rectf: TGPRectF;
  stringFormat: TGPStringFormat;
  solidBrush, SB2: TGPSolidBrush;
  x1,y1,x2,y2: single;
  fs: integer;
  sizerect: TGPRectF;
  GPath: TGPGraphicsPath;
  R: TRect;
begin
  R := Rect(Round(RF.X), Round(RF.Y), Round(RF.X + RF.Width), Round(RF.Y + RF.Height));
  Result := MakeRect(0, 0, 0, 0);
  if (Caption <> '') or (WideCaption <> '') then
  begin
    fontFamily:= TGPFontFamily.Create(AFont.Name);

    if (fontFamily.Status in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      fontFamily.Free;
      fontFamily := TGPFontFamily.Create('Arial');
    end;


    fs := 0;

    if (fsBold in AFont.Style) then
      fs := fs + 1;

    if (fsItalic in AFont.Style) then
      fs := fs + 2;

    if (fsUnderline in AFont.Style) then
      fs := fs + 4;

    font := TGPFont.Create(fontFamily, AFont.Size , fs, UnitPoint);

    if not (Shadow and RealDraw) then
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
      taCenter:
      begin
        // Center-justify each line of text.
        stringFormat.SetAlignment(StringAlignmentCenter);
      end;
      taRightJustify: stringFormat.SetAlignment(StringAlignmentFar);
    end;

    // Center the block of text (top to bottom) in the rectangle.

    stringFormat.SetLineAlignment(StringAlignmentCenter);
    stringFormat.SetHotkeyPrefix(HotkeyPrefixShow);
    stringFormat.SetTrimming(StringTrimmingNone);

    FillChar(sizerect,SizeOf(sizerect),0);

    if (Caption <> '') then
      graphics.MeasureString(Caption, Length(Caption), font, rectf, stringFormat, Result)
    else
      graphics.MeasureString(WideCaption, Length(WideCaption), font, rectf, stringFormat, Result);

    rectf := MakeRect(x1,y1,x2,y2);

    if RealDraw then
    begin
      if Shadow then
      begin
        graphics.SetSmoothingMode(SmoothingModeHighQuality);

        if (ShadowClr <> clNone) then
        begin
          GPath := TGPGraphicsPath.Create();
          GPath.AddString(Caption, Length(Caption), fontFamily, FontStyleBold, AFont.Size + 3, MakeRect(x1, y1 + 1,x2,y2), stringFormat);
          SB2 := TGPSolidBrush.Create(ColorToARGB(ShadowClr));
          graphics.FillPath(SB2, GPath);
          SB2.Free;
          GPath.Free;
        end;

        GPath := TGPGraphicsPath.Create();
        GPath.AddString(Caption, Length(Caption), fontFamily, FontStyleBold{FontStyleRegular}, AFont.Size + 3, rectf, stringFormat);

        graphics.FillPath(solidbrush, GPath);

        GPath.Free;
      end
      else
      begin
        if (Caption <> '') then
          graphics.DrawString(Caption, Length(Caption), font, rectf, stringFormat, solidBrush)
        else
          graphics.DrawString(WideCaption, Length(WideCaption), font, rectf, stringFormat, solidBrush)
      end;
    end;
    stringformat.Free;
    solidBrush.Free;
    font.Free;
    fontfamily.Free;
  end;
end;

//------------------------------------------------------------------------------

function DarkenColor(Color: TColor; Perc: integer): TColor;
var
  r,g,b: longint;
  l: longint;
begin
  l := ColorToRGB(Color);
  r := ((l AND $FF0000) shr 16) and $FF;
  g := ((l AND $FF00) shr 8) and $FF;
  b := (l AND $FF);

  r := Round(r * (100 - Perc)/100);
  g := Round(g * (100 - Perc)/100);
  b := Round(b * (100 - Perc)/100);

  Result := (r shl 16) or (g shl 8) or b;
end;

//------------------------------------------------------------------------------

procedure DrawGDIPImage(gr: TGPGraphics; Canvas: TCanvas; P: TPoint; bmp: TGraphic; Transparent: Boolean = False);
var
  Img: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  graphics: TGPGraphics;
  ImageAttributes: TGPImageAttributes;
  r, g, b: byte;
  GPBmp: TGPBitmap;
  Aclr: TGPColor;
  hr: HResult;
begin
  if (not Assigned(gr) and not Assigned(Canvas)) then
    Exit;

  graphics := gr;
  if not Assigned(graphics) then
  begin
    graphics := TGPGraphics.Create(Canvas.Handle);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
  end;

  ms := TMemoryStream.Create;
  bmp.SaveToStream(ms);
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

  if (hr = S_OK) then
  begin
    pstm.Write(ms.Memory, ms.Size,@pcbWrite);

    if (ms.Size = pcbWrite) then
    begin
      Img := TGPImage.Create(pstm);

      if Transparent and (Img.GetFormat <> ifPNG) then
      begin
        GPBmp := TGPBitmap.Create(pstm);
        GPBmp.GetPixel(0, 0, AClr);
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
        graphics.DrawImage(Img, P.X, P.y);

      Img.Free;
    end;

    pstm := nil;
  end
  else
    GlobalFree(hGlobal);

  ms.Free;

  if not Assigned(gr) then
    graphics.Free;
end;

//------------------------------------------------------------------------------

function GetReflection(Picture: TBitmap; ReflectionOpacityStart, ReflectionOpacityEnd, ReflectionSize, ReflectionAxis: Integer): TGPBitmap;
var
  gpbmp: TGPBitmap;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  w, h, x, y, op, alph: integer;
  clr, clrTemp: TGPColor;
  a: byte;
  hr: HResult;
begin
  Result := nil;
  if Picture.Empty then
    Exit;

  w := Picture.Width;
  h := Picture.Height;

  ms := TMemoryStream.Create;
  Picture.SaveToStream(ms);
  hGlobal := GlobalAlloc(GMEM_MOVEABLE, ms.Size);
  if (hGlobal = 0) then
  begin
    ms.Free;
    raise Exception.Create('Could not allocate memory for reflection image');
  end;

  pstm := nil;
  pcbWrite := 0;

  // Create IStream* from global memory
  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

  if hr = S_OK then
  begin
    pstm.Write(ms.Memory, ms.Size,@pcbWrite);

    if ms.Size = pcbWrite then
    begin
      gpbmp := TGPBitmap.Create(pstm);
      gpbmp.RotateFlip(RotateNoneFlipY);

      Result := TGPBitmap.Create(w, h{, PixelFormat32bppARGB});

      for y := 0 to h do
      begin
        if (y < ReflectionSize) then
        begin
          op := Round( ((ReflectionSize - y)/ReflectionSize * ReflectionOpacityStart) +
                         y/ReflectionSize * ReflectionOpacityEnd);
        end
        else
          op := 0;

  //      op := Round((255.0 / h) * (h - y)) - FReflection;

        if (op < 0) then
          op := 0;
        if (op > 255) then
          op := 255;

        for x := 0 to w do
        begin
          gpbmp.GetPixel(x, y, clr);
          a := GetAlpha(clr);
          if (a = 0) then
            Continue;

          alph := Round((op / 255) * a);
          clrTemp := MakeColor(alph, GetRed(clr), GetGreen(clr), GetBlue(clr));
          Result.SetPixel(x, y, clrTemp);
        end;
      end;
      gpbmp.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hGlobal);

  ms.Free;
end;

//------------------------------------------------------------------------------

function CreateCapsolePath(R: TGPRectF; CurvedType: Integer): TGPGraphicsPath;   // CurvedType 0=Left; 1=Right; 2=Both
var
  l, t, w, h: Integer;
begin
  Result := TGPGraphicsPath.Create;
  l := Round(R.X);
  t := Round(R.Y);
  w := Round(R.Width);
  h := Round(R.Height);

  case CurvedType of
    0:
    begin
      Result.AddArc(l, t, h, h, 90, 180);
      Result.AddLine(l + h div 2, t, l + w, t);
      Result.AddLine(l + w, t, l + w, t + h);
      Result.AddLine(l + w, t + h, l + w, t + h);
    end;
    1:
    begin
      Result.AddArc(l + w - h, t, h, h, 90, -180);
      Result.AddLine(l + w - h div 2, t, l, t);
      Result.AddLine(l, t, l, t + h);
      Result.AddLine(l, t + h, l + w - h div 2, t + h);

    end;
    2:
    begin
      Result.AddArc(l, t, h, h, -90, -180);
      Result.AddArc(l + w - h, t, h, h, 90, -180);
    end;
  end;

  Result.CloseFigure();
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

  // in case someone screws up the rounding mode!
  if (r1 > 255) then r1 := 255;
  if (g1 > 255) then g1 := 255;
  if (b1 > 255) then b1 := 255;

  Result := RGB(r1,g1,b1);
end;

//------------------------------------------------------------------------------

procedure TGDIPFill.DrawGlow(Graphics: TGPGraphics; R: TGPRectF);
var
  gb: TGPPathGradientBrush;
  b: TGPLinearGradientBrush;
  p: TGPPen;
  ro, rf, er: TGPRectF;
  pth: TGPGraphicsPath;
  colors : array[0..0] of TGPColor;
  cCount: integer;
  rgn: TGPRegion;
begin
  if Glow = gmNone then
    Exit;

  rgn := TGPRegion.Create(R);
  Graphics.SetClip(rgn);

  rf := MakeRect(r.X + 1, r.Y + 1, r.Width - 2, r.Height - 2);

  if (Glow = gmGradient) or (Glow = gmRadialGradient) then
  begin
    pth := CreateRoundRectangle(rf, Rounding, fRoundingType, false);
    p := TGPPen.Create(MakeColor(200, GlowGradientColor));
    Graphics.DrawPath(p, pth);
    p.free;

    pth.Reset;
    ro := MakeRect(rf.x + 5, rf.y, rf.Width - 10, 4);
    b := TGPLinearGradientBrush.Create(MakeRect(ro.X - 1, ro.Y - 1, ro.Width + 2, ro.Height + 2), MakeColor(175, GlowGradientColor), MakeColor(0, GlowGradientColor), LinearGradientModeVertical);
    pth.AddLine(rf.X, rf.Y, rf.X + 5, rf.Y);
    pth.AddLine(rf.X + 5, rf.Y, rf.X + 5, rf.Y + 4);
    pth.CloseFigure;
    Graphics.FillPath(b, pth);
    pth.Reset;
    pth.AddLine(rf.X + rf.Width, rf.Y, rf.X + rf.Width - 5, rf.Y);
    pth.AddLine(rf.X + rf.Width - 5, rf.Y, rf.X + rf.Width - 5, rf.Y + 4);
    pth.CloseFigure;
    Graphics.FillPath(b, pth);
    Graphics.FillRectangle(b, ro);
    b.free;

    //
    ro := MakeRect(rf.X + 5, rf.Y + rf.Height - 7, rf.Width - 10, 7);
    b := TGPLinearGradientBrush.Create(MakeRect(ro.X - 1, ro.Y - 1, ro.Width + 2, ro.Height + 2), MakeColor(0, GlowGradientColor), MakeColor(220, GlowGradientColor), LinearGradientModeVertical);
    pth.Reset;
    pth.AddLine(rf.X, rf.Y + rf.Height, rf.X + 5, rf.Y + rf.Height);
    pth.AddLine(rf.X + 5, rf.Y  + rf.Height, rf.X + 5, rf.Y + rf.Height - 7);
    pth.CloseFigure;
    Graphics.FillPath(b, pth);
    pth.Reset;
    pth.AddLine(rf.X + rf.Width, rf.Y + rf.Height, rf.X + rf.Width - 5, rf.Y  + rf.Height);
    pth.AddLine(rf.X + rf.Width - 5, rf.Y  + rf.Height - 5, rf.X + rf.Width - 5, rf.Y + rf.Height - 7);
    pth.CloseFigure;
    Graphics.FillPath(b, pth);
    Graphics.FillRectangle(b, ro);
    b.free;

    pth.Free;
  end;

  if (Glow = gmRadial) or (Glow = gmRadialGradient) then
  begin
    pth := TGPGraphicsPath.Create;
    er := MakeRect(rf.X + 10, rf.Y - rf.Height / 2, rf.Width - 20, rf.Height * 3);
    pth.AddEllipse(er);
    gb := TGPPathGradientBrush.Create(pth);
    gb.SetCenterPoint(MakePoint(er.X + (er.Width / 2), er.Y + er.Height / 2));
    gb.SetCenterColor(MakeColor(150, GlowRadialColor));
    colors[0] := MakeColor(0, Self.Color);
    cCount := 1;
    gb.SetSurroundColors(@colors, cCount);
    Graphics.FillPath(gb, pth);
    pth.Free;
    gb.Free;
  end;

  Graphics.ResetClip;
  rgn.Free;
end;

{$IFDEF DIRECT2D}
procedure TGDIPFill.DrawGlow2D(Graphics: TDirect2DCanvas; R: TD2DRectF);
var
  ro, rf, er: TD2DRectF;
  pth, pth2D, path2D: ID2D1PathGeometry;
  geoSink: ID2D1GeometrySink;
  geoPath: ID2D1PathGeometry;
  geoR: ID2D1RectangleGeometry;
  geoLayerParam: TD2D1LayerParameters;
  geoLayer: ID2D1Layer;
  bounds: TD2DRectF;
  pth2DSink: ID2D1GeometrySink;
  ellipse: TD2D1Ellipse;
  solGrgBrush2D: ID2D1Brush;
  linGrgBrush2D: ID2D1LinearGradientBrush;
  linGrgBrush2DProp: TD2D1LinearGradientBrushProperties;
  linGrgBrush2DStops: array[0 .. 1] of TD2D1GradientStop;
  linGrgBrush2DStopCol: ID2D1GradientStopCollection;
  radGrgBrush2DProp: TD2D1RadialGradientBrushProperties;
  radGrgBrush2DStops: array[0 .. 1] of TD2D1GradientStop;
  radGrgBrush2DStopCol: ID2D1GradientStopCollection;
  radGrgBrush2D: ID2D1RadialGradientBrush;
begin
  if Glow = gmNone then
    Exit;

  rf := D2D1RectF(r.left + 1, r.top + 2, r.right - 1, r.bottom - 1);

  D2DFactory.CreateRectangleGeometry(Rf, geoR);
  path2D := CreateRoundRectangle2D(Rf, Rounding, RoundingType, False);
  D2DFactory.CreatePathGeometry(geoPath);
  geoPath.Open(geoSink);

  path2D.CombineWithGeometry(geoR, D2D1_COMBINE_MODE_INTERSECT, TD2D1Matrix3x2F.Identity, D2D1_DEFAULT_FLATTENING_TOLERANCE, geoSink);
  geoSink.Close;

  graphics.RenderTarget.CreateLayer(nil, geoLayer);
  geoLayerParam.geometricMask := geoPath;
  geoLayerParam.maskTransform := TD2DMatrix3x2F.Identity;
  geoLayerParam.geometricMask.GetBounds(geoLayerParam.maskTransform, bounds);
  geoLayerParam.contentBounds := bounds;
  geoLayerParam.maskAntialiasMode := D2D1_ANTIALIAS_MODE_PER_PRIMITIVE;
  geoLayerParam.opacityBrush := nil;
  geoLayerParam.layerOptions := D2D1_LAYER_OPTIONS_NONE;
  geoLayerParam.opacity := 1;
  Graphics.RenderTarget.PushLayer(geoLayerParam, geoLayer);

  if (Glow = gmGradient) or (Glow = gmRadialGradient) then
  begin

    pth := CreateRoundRectangle2D(rf, Rounding, fRoundingType, false);

    solGrgBrush2D := Graphics.CreateBrush(GlowGradientColor);
    solGrgBrush2D.SetOpacity(200 / 255);
    graphics.RenderTarget.DrawGeometry(pth, solGrgBrush2D);

    ro := D2D1RectF(rf.left + 5, rf.top, rf.right - 5, rf.top + 4);

    linGrgBrush2DProp.startPoint := D2D1PointF(ro.Left + (ro.right - ro.left) / 2, ro.Top);
    linGrgBrush2DProp.endPoint := D2D1PointF(ro.Left + (ro.right - ro.left) / 2, ro.Bottom);

    linGrgBrush2DStops[0].position := 0;
    linGrgBrush2DStops[1].position := 1;
    linGrgBrush2DStops[0].color := D2D1ColorF(GlowGradientColor, 175 / 255);
    linGrgBrush2DStops[1].color := D2D1ColorF(GlowGradientColor, 0);

    graphics.RenderTarget.CreateGradientStopCollection(@linGrgBrush2DStops, Length(linGrgBrush2DStops), D2D1_GAMMA_1_0, D2D1_EXTEND_MODE_CLAMP, linGrgBrush2DStopCol);
    graphics.RenderTarget.CreateLinearGradientBrush(linGrgBrush2DProp, nil, linGrgBrush2DStopCol, linGrgBrush2D);

    D2DFactory.CreatePathGeometry(pth2D);
    pth2D.Open(pth2DSink);
    pth2DSink.BeginFigure(D2D1PointF(ro.left, ro.top), D2D1_FIGURE_BEGIN_FILLED);
    pth2DSink.AddLine(D2D1PointF(ro.left + 5, ro.top));
    pth2DSink.AddLine(D2D1PointF(ro.left + 5, ro.top + 4));
    pth2DSink.EndFigure(D2D1_FIGURE_END_CLOSED);
    pth2DSink.Close;

    graphics.RenderTarget.FillGeometry(pth2D, linGrgBrush2D);

    D2DFactory.CreatePathGeometry(pth2D);
    pth2D.Open(pth2DSink);
    pth2DSink.BeginFigure(D2D1PointF(ro.right, ro.top), D2D1_FIGURE_BEGIN_FILLED);
    pth2DSink.AddLine(D2D1PointF(ro.right - 5, ro.top));
    pth2DSink.AddLine(D2D1PointF(ro.right - 5, ro.top + 4));
    pth2DSink.EndFigure(D2D1_FIGURE_END_CLOSED);
    pth2DSink.Close;

    graphics.RenderTarget.FillGeometry(pth2D, linGrgBrush2D);
    Graphics.RenderTarget.FillRectangle(ro, linGrgBrush2D);

    ro := D2D1RectF(rf.left + 5, rf.bottom - 7, rf.Right - 5, rf.bottom);

    linGrgBrush2DProp.startPoint := D2D1PointF(ro.Left + (ro.right - ro.left) / 2, ro.Top);
    linGrgBrush2DProp.endPoint := D2D1PointF(ro.Left + (ro.right - ro.left) / 2, ro.Bottom);


    graphics.RenderTarget.FillRectangle(ro, linGrgBrush2D);

    linGrgBrush2DStops[0].position := 0;
    linGrgBrush2DStops[1].position := 1;
    linGrgBrush2DStops[0].color := D2D1ColorF(GlowGradientColor, 0);
    linGrgBrush2DStops[1].color := D2D1ColorF(GlowGradientColor, 175 / 255);

    graphics.RenderTarget.CreateGradientStopCollection(@linGrgBrush2DStops, Length(linGrgBrush2DStops), D2D1_GAMMA_1_0, D2D1_EXTEND_MODE_CLAMP, linGrgBrush2DStopCol);
    graphics.RenderTarget.CreateLinearGradientBrush(linGrgBrush2DProp, nil, linGrgBrush2DStopCol, linGrgBrush2D);

    D2DFactory.CreatePathGeometry(pth2D);
    pth2D.Open(pth2DSink);
    pth2DSink.BeginFigure(D2D1PointF(ro.left, ro.Bottom), D2D1_FIGURE_BEGIN_FILLED);
    pth2DSink.AddLine(D2D1PointF(ro.left + 5, ro.Bottom));
    pth2DSink.AddLine(D2D1PointF(ro.left + 5, ro.Bottom - 7));
    pth2DSink.EndFigure(D2D1_FIGURE_END_CLOSED);
    pth2DSink.Close;

    graphics.RenderTarget.FillGeometry(pth2D, linGrgBrush2D);

    D2DFactory.CreatePathGeometry(pth2D);
    pth2D.Open(pth2DSink);

    pth2DSink.BeginFigure(D2D1PointF(ro.right, ro.Bottom), D2D1_FIGURE_BEGIN_FILLED);
    pth2DSink.AddLine(D2D1PointF(ro.right - 5, ro.Bottom));
    pth2DSink.AddLine(D2D1PointF(ro.right - 5, ro.Bottom - 7));
    pth2DSink.EndFigure(D2D1_FIGURE_END_CLOSED);
    pth2DSink.Close;

    graphics.RenderTarget.FillGeometry(pth2D, linGrgBrush2D);
    Graphics.RenderTarget.FillRectangle(ro, linGrgBrush2D);
  end;

  if (Glow = gmRadial) or (Glow = gmRadialGradient) then
  begin
    er := D2D1RectF(rf.left + 10, rf.top - (rf.Bottom - rf.top) / 2, rf.Right - 10, rf.top + (rf.bottom - rf.top) * 3);

    radGrgBrush2DProp.center := D2D1PointF(er.left + (er.right - er.left) / 2, er.Top + (er.bottom - er.top) / 2);
    radGrgBrush2DProp.gradientOriginOffset.X := 0;
    radGrgBrush2DProp.gradientOriginOffset.Y := 0;
    radGrgBrush2DProp.radiusX := er.right - er.left;
    radGrgBrush2DProp.radiusY := er.bottom - er.top;
    radGrgBrush2DStops[0].position := 0;
    radGrgBrush2DStops[0].color := D2D1ColorF(GlowRadialColor, 150 / 255);
    radGrgBrush2DStops[1].position := 0.55;
    radGrgBrush2DStops[1].color := D2D1ColorF(GlowRadialColor, 0);

    Graphics.RenderTarget.CreateGradientStopCollection(@radGrgBrush2DStops, Length(radGrgBrush2DStops), D2D1_GAMMA_1_0,
      D2D1_EXTEND_MODE_CLAMP,  radGrgBrush2DStopCol);

    Graphics.RenderTarget.CreateRadialGradientBrush(radGrgBrush2DProp, nil, radGrgBrush2DStopCol, radGrgBrush2D);

    ellipse.point := radGrgBrush2DProp.center;
    ellipse.radiusX := radGrgBrush2DProp.radiusX;
    ellipse.radiusY := radGrgBrush2DProp.radiusY;
    graphics.RenderTarget.FillEllipse(ellipse, radGrgBrush2D);
  end;


  Graphics.RenderTarget.PopLayer;
end;

procedure TGDIPFill.DrawGradientBackGround2D(Graphics: TDirect2DCanvas; R: TD2D1RectF; Mirror: Boolean; oc, octo, ocmr, ocmrto: Byte);
var
  Params: TGDIPFillParameters;
  gt: TAdvGradientType;
  c, ct: TColor;
  o, ot: Byte;
  ht: THatchStyle;
  pth2D: ID2D1PathGeometry;
  solGrBrush2D: TDirect2DBrush;
  solGrgBrush2D: ID2D1SolidColorBrush;
  linGrBrush2D: TDirect2DBrush;
  linGrgBrush2D: ID2D1LinearGradientBrush;
  linGrgBrush2DProp: TD2D1LinearGradientBrushProperties;
  linGrgBrush2DStops: array[0 .. 1] of TD2D1GradientStop;
  linGrgBrush2DStopCol: ID2D1GradientStopCollection;
  mode: TLinearGradientMode;
  bmp: ID2D1Bitmap;
  bmpBrushProps: D2D1_BITMAP_BRUSH_PROPERTIES;
  bmpGrBrush2D: TDirect2DBrush;
  bmpGrBrush2DHandle: ID2D1BitmapBrush;
begin
  if Mirror then
  begin
    gt := FGradientMirrorType;
    c := FColorMirror;
    ct := FColorMirrorTo;
    o := ocmr;
    ot := ocmrto;
    ht := HatchStyleMirror;
  end
  else
  begin
    gt := FGradientType;
    c := FColor;
    ct := FColorTo;
    o := oc;
    ot := octo;
    ht := HatchStyle;
  end;

  if c = clNone then
    gt := gtNone;

  if (ct = clNone) and (c <> clNone) then
    gt := gtSolid;

  if Rounding = 0 then
  begin
    Params.Fillpath := false;
  end
  else
  begin
    if (FColorMirror <> clNone) then
    begin
      if Mirror then
      begin
        case RoundingType of
          rtNone: pth2D := CreateRoundRectangle2D(r, Rounding, rtNone, Mirror);
          rtTop: pth2D := CreateRoundRectangle2D(r, Rounding, rtNone, Mirror);
          rtBoth, rtBottom: pth2D := CreateRoundRectangle2D(r, Rounding, rtBottom, Mirror);
          rtRight: pth2D := CreateRoundRectangle2D(r, Rounding, rtRight, Mirror);
          rtLeft: pth2D := CreateRoundRectangle2D(r, Rounding, rtLeft, Mirror);
        end;
      end
      else
      begin
        case RoundingType of
          rtNone: pth2D := CreateRoundRectangle2D(r, Rounding, rtNone, Mirror);
          rtBoth, rtTop: pth2D := CreateRoundRectangle2D(r, Rounding, rtTop, Mirror);
          rtBottom: pth2D := CreateRoundRectangle2D(r, Rounding, rtNone, Mirror);
          rtRight: pth2D := CreateRoundRectangle2D(r, Rounding, rtRight, Mirror);
          rtLeft: pth2D := CreateRoundRectangle2D(r, Rounding, rtLeft, Mirror);
        end;
      end;
    end
    else
      pth2D := CreateRoundRectangle2D(r, Rounding, RoundingType, Mirror);

    Params.Fillpath := true;
  end;

  Params.GT := gt;
  Params.Angle := FAngle;
  Params.ColorFrom := c;
  Params.ColorTo := ct;
  Params.OpacityFrom := o;
  Params.OpacityTo := ot;
  Params.HatchStyle := ht;
  Params.BorderColor := BorderColor;
  Params.BorderWidth := BorderWidth;
  Params.BorderStyle := DashStyleSolid;
  Params.Mirror := Mirror;

  if (FBackGroundPicturePosition = ppTiled) then
    Params.Image := FBackGroundPicture
  else
    Params.Image := nil;

   case Params.GT of
   gtSolid:
     begin
       Graphics.RenderTarget.CreateSolidColorBrush(D2D1ColorF(Params.ColorFrom, Params.OpacityFrom / 255), nil, solGrgBrush2D);

       solGrBrush2D := TDirect2DBrush.Create(Graphics);
       solGrBrush2D.Handle := solGrgBrush2D;
       Graphics.Brush := solGrBrush2D;

       if Params.FillPath then
       begin
         graphics.FillGeometry(pth2D);
       end
       else
         graphics.FillRectangle(R);

       solGrBrush2D.Free;
     end;
   gtVertical, gtHorizontal, gtForwardDiagonal, gtBackwardDiagonal:
     begin
       mode := LinearGradientModeVertical;
       if Params.GT = gtHorizontal then
          mode := LinearGradientModeHorizontal;
       if Params.GT = gtForwardDiagonal then
          mode := LinearGradientModeForwardDiagonal;
       if Params.GT = gtBackwardDiagonal then
          mode := LinearGradientModeBackwardDiagonal;


       case mode of
         LinearGradientModeHorizontal:
         begin
           linGrgBrush2DProp.startPoint := D2D1PointF(r.left, r.top + (r.bottom - r.top) / 2);
           linGrgBrush2DProp.endPoint := D2D1PointF(r.Right, r.top + (r.bottom - r.top) / 2);
         end;
         LinearGradientModeVertical:
         begin
           linGrgBrush2DProp.startPoint := D2D1PointF(r.Left + (r.Right - r.Left) / 2, r.Top);
           linGrgBrush2DProp.endPoint := D2D1PointF(r.Left + (r.Right - r.Left) / 2, r.Bottom);
         end;
         LinearGradientModeForwardDiagonal:
         begin
           linGrgBrush2DProp.startPoint := D2D1PointF(r.Left, r.Top);
           linGrgBrush2DProp.endPoint := D2D1PointF(r.right, r.Bottom);
         end;
         LinearGradientModeBackwardDiagonal:
         begin
           linGrgBrush2DProp.startPoint := D2D1PointF(r.Right, r.Top);
           linGrgBrush2DProp.endPoint := D2D1PointF(r.Left, r.Bottom);
         end;
       end;

       linGrgBrush2DStops[0].position := 0;
       linGrgBrush2DStops[1].position := 1;
       linGrgBrush2DStops[0].color := D2D1ColorF(params.ColorFrom, params.OpacityFrom / 255);
       linGrgBrush2DStops[1].color := D2D1ColorF(params.ColorTo, params.OpacityTo / 255);

       graphics.RenderTarget.CreateGradientStopCollection(@linGrgBrush2DStops, Length(linGrgBrush2DStops), D2D1_GAMMA_1_0, D2D1_EXTEND_MODE_CLAMP, linGrgBrush2DStopCol);
       graphics.RenderTarget.CreateLinearGradientBrush(linGrgBrush2DProp, nil, linGrgBrush2DStopCol, linGrgBrush2D);

       linGrBrush2D := TDirect2DBrush.Create(Graphics);
       linGrBrush2D.Handle := linGrgBrush2D;
       Graphics.Brush := linGrBrush2D;

       if Params.FillPath then
       begin
         graphics.FillGeometry(pth2D);
       end
       else
       begin
         graphics.FillRectangle(r);
       end;

       linGrBrush2D.Free;
     end;
   gtAngle:
     begin
     //todo
     end;
   gtHatch:
     begin
     //todo
     end;
   gtTexture:
     begin
       if Assigned(Params.Image) then
       begin
         if not Params.Image.Empty then
         begin
           bmp := Params.Image.Create2DBitmap(Graphics);
           bmpBrushProps := defaultBitmapbrushProperties;
           Graphics.RenderTarget.CreateBitmapBrush(bmp, @bmpBrushProps, nil, bmpGrBrush2DHandle);
           bmpGrBrush2D := TDirect2DBrush.Create(Graphics);
           bmpGrBrush2D.Handle := bmpGrBrush2DHandle;
           Graphics.Brush := bmpGrBrush2D;

           if Params.FillPath then
           begin
             graphics.FillGeometry(pth2D);
           end
           else
           begin
             graphics.FillRectangle(r);
           end;

           bmpGrBrush2D.Free;
         end;
       end;
     end;
   end;


  pth2D := nil;
end;
{$ENDIF}

procedure TGDIPFill.DrawGradientBackGround(Graphics: TGPGraphics; R: TGPRectF; Mirror: Boolean; oc, octo, ocmr, ocmrto: Byte);
var
  path: TGPGraphicsPath;
  par: TGDIPFillParameters;
  gt: TAdvGradientType;
  c, ct: TColor;
  o, ot: Byte;
  ht: THatchStyle;
  smt: TSmoothingMode;
begin
  path := nil;

  if Mirror then
  begin
    gt := FGradientMirrorType;
    c := FColorMirror;
    ct := FColorMirrorTo;
    o := ocmr;
    ot := ocmrto;
    ht := HatchStyleMirror;
  end
  else
  begin
    gt := FGradientType;
    c := FColor;
    ct := FColorTo;
    o := oc;
    ot := octo;
    ht := HatchStyle;
  end;

  if c = clNone then
    gt := gtNone;

  if (ct = clNone) and (c <> clNone) then
    gt := gtSolid;

  par.Graphics := Graphics;

  if Rounding = 0 then
  begin
    par.Path := nil;
    par.Fillpath := false;
  end
  else
  begin
    if (FColorMirror <> clNone) then
    begin
      if Mirror then
      begin
        case RoundingType of
          rtNone: path := CreateRoundRectangle(r, Rounding, rtNone, Mirror);
          rtTop: path := CreateRoundRectangle(r, Rounding, rtNone, Mirror);
          rtBoth, rtBottom: path := CreateRoundRectangle(r, Rounding, rtBottom, Mirror);
          rtRight: path := CreateRoundRectangle(r, Rounding, rtRight, Mirror);
          rtLeft: path := CreateRoundRectangle(r, Rounding, rtLeft, Mirror);
        end;
      end
      else
      begin
        case RoundingType of
          rtNone: path := CreateRoundRectangle(r, Rounding, rtNone, Mirror);
          rtBoth, rtTop: path := CreateRoundRectangle(r, Rounding, rtTop, Mirror);
          rtBottom: path := CreateRoundRectangle(r, Rounding, rtNone, Mirror);
          rtRight: path := CreateRoundRectangle(r, Rounding, rtRight, Mirror);
          rtLeft: path := CreateRoundRectangle(r, Rounding, rtLeft, Mirror);
        end;
      end;
    end
    else
      path := CreateRoundRectangle(r, Rounding, RoundingType, Mirror);

    par.Path := path;
    par.Fillpath := true;
  end;

  par.R := r;
  par.GT := gt;
  par.Angle := FAngle;
  par.ColorFrom := c;
  par.ColorTo := ct;
  par.OpacityFrom := o;
  par.OpacityTo := ot;
  par.HatchStyle := ht;
  par.BorderColor := clNone;
  par.BorderWidth := 0;
  par.BorderStyle := DashStyleSolid;
  par.Mirror := Mirror;

  if (FBackGroundPicturePosition = ppTiled) then
    par.Image := FBackGroundPicture
  else
    par.Image := nil;

  smt := Graphics.GetSmoothingMode;

  if par.Fillpath then
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

  FillGDIP(par);

  if par.Fillpath then
    Graphics.SetSmoothingMode(smt);

  if path <> nil then
    path.Free;
end;

procedure TGDIPFill.EndUpdate;
begin
  if (FUpdateCount > 0) then
  begin
    dec(FUpdateCount);
  end;
end;

{$IFDEF DIRECT2D}
procedure DrawRadialBackGround2D(Graphics: TDirect2DCanvas; R: TD2DRectF; CF,CT: TColor; o, ot: Byte; Upper: boolean;
  Radius: integer; RoundingType: TFillRoundingType);
var
  path2D: TD2D1RoundedRect;
  pthGrBrush2D: TDirect2DBrush;
  pthGrgBrush2D: ID2D1RadialGradientBrush;
  pthGrgBrush2DProp: TD2D1RadialGradientBrushProperties;
  pthGrgBrush2DStops: array[0 .. 1] of TD2D1GradientStop;
  pthGrgBrush2DStopCol: ID2D1GradientStopCollection;
  solGrBrush2D: TDirect2DBrush;
  solGrgBrush2D: ID2D1SolidColorBrush;
  h,h2: Double;
begin
  h := r.bottom - r.top;

  h2 := h / 2;

  if Upper then
  begin
    graphics.RenderTarget.CreateSolidColorBrush(D2D1ColorF(CF, o / 255), nil, solGrgBrush2D);
  end
  else
  begin
    graphics.RenderTarget.CreateSolidColorBrush(D2D1ColorF(CT, ot / 255), nil, solGrgBrush2D);
  end;

  path2D := D2D1RoundedRect(R, Radius, Radius);
  solGrBrush2D := TDirect2DBrush.Create(Graphics);
  solGrBrush2D.Handle := solGrgBrush2D;
  graphics.Brush := solGrBrush2D;
  graphics.FillRoundedRectangle(path2D);
  solGrBrush2D.Free;

  if Upper then        // take borders in account
  begin
    pthGrgBrush2DProp.center := D2D1PointF(R.left + (r.right - r.left) / 2, r.top + h2 * 2);
    pthGrgBrush2DProp.gradientOriginOffset.X := 0;
    pthGrgBrush2DProp.gradientOriginOffset.Y := 0;
    pthGrgBrush2DProp.radiusX := r.right - r.left;
    pthGrgBrush2DProp.radiusY := h2 * 3;
    pthGrgBrush2DStops[0].position := 0;
    pthGrgBrush2DStops[0].color := D2D1ColorF(CT, ot / 255);
    pthGrgBrush2DStops[1].position := 1;
    pthGrgBrush2DStops[1].color := D2D1ColorF(CF, o / 255);
  end
  else
  begin
    pthGrgBrush2DProp.center := D2D1PointF(R.left + (r.right - r.left) / 2, r.Top);
    pthGrgBrush2DProp.gradientOriginOffset.X := 0;
    pthGrgBrush2DProp.gradientOriginOffset.Y := 0;
    pthGrgBrush2DProp.radiusX := r.right - r.left;
    pthGrgBrush2DProp.radiusY := h2 * 3;
    pthGrgBrush2DStops[0].position := 0;
    pthGrgBrush2DStops[0].color := D2D1ColorF(CF, o / 255);
    pthGrgBrush2DStops[1].position := 1;
    pthGrgBrush2DStops[1].color := D2D1ColorF(CT, ot / 255);
  end;

  Graphics.RenderTarget.CreateGradientStopCollection(@pthGrgBrush2DStops, Length(pthGrgBrush2DStops), D2D1_GAMMA_1_0,
    D2D1_EXTEND_MODE_CLAMP,  pthGrgBrush2DStopCol);

  graphics.RenderTarget.CreateRadialGradientBrush(pthGrgBrush2DProp, nil, pthGrgBrush2DStopCol, pthGrgBrush2D);

  pthGrBrush2D := TDirect2DBrush.Create(Graphics);
  pthGrBrush2D.Handle := pthGrgBrush2D;
  Graphics.Brush := pthGrBrush2D;
  Graphics.FillRoundedRectangle(path2D);
  pthGrBrush2D.Free;
end;
{$ENDIF}

procedure DrawRadialBackGround(Graphics: TGPGraphics; R: TGPRectF; CF,CT: TColor; o, ot: Byte; Upper: boolean;
  Radius: integer; RoundingType: TFillRoundingType);
var
  path: TGPGraphicsPath;
  pthGrBrush: TGPPathGradientBrush;
  solGrBrush: TGPSolidBrush;

  w,h,w2,h2: Double;
  colors : array[0..0] of TGPColor;
  count: Integer;
  rgn: TGPRegion;
begin
  w := r.Width;
  h := r.Height;

  h2 := h / 2;
  w2 := w / 2;

  if Upper then
    solGrBrush := TGPSolidBrush.Create(MakeColor(o, CF))
  else
    solGrBrush := TGPSolidBrush.Create(MakeColor(ot, CT));

  path := CreateRoundRectangle(r, Radius, RoundingType, false);
  Graphics.FillPath(solgrBrush, path);

  rgn := TGPRegion.Create(path);
  path.free;
  graphics.SetClip(rgn);

//  Graphics.FillRectangle(solGrBrush, r);

  solGrBrush.Free;

  // Create a path that consists of a single ellipse.
  path := TGPGraphicsPath.Create;

  if Upper then        // take borders in account
    path.AddEllipse(r.X, r.Y - h2, r.Width , r.Height + h2)
  else
    path.AddEllipse(r.X, r.Y, r.Width , r.Height + h2);

  pthGrBrush := TGPPathGradientBrush.Create(path);


  if Upper then
    pthGrBrush.SetCenterPoint(MakePoint(r.X + w2, r.Y))
  else
    pthGrBrush.SetCenterPoint(MakePoint(r.X + w2, r.Y + R.Height));

  // Set the color at the center point to blue.
  if Upper then
  begin
    pthGrBrush.SetCenterColor(MakeColor(ot, CT));
    colors[0] := MakeColor(o, CF);
  end
  else
  begin
    pthGrBrush.SetCenterColor(MakeColor(o, CF));
    colors[0] := MakeColor(ot, CT);
  end;

  count := 1;
  pthGrBrush.SetSurroundColors(@colors, count);
  graphics.FillRectangle(pthGrBrush, r);
  pthGrBrush.Free;

  path.Free;
  rgn.free;
  Graphics.ResetClip;
end;

procedure DrawPicture(g: TGPGraphics; AspectRatio: Boolean; AspectMode: TPictureMode; picture: TAdvGDIPPicture; location: TFillPicturePosition; w, h: integer; r: TRect; x, y: integer);
var
  xw, xh: Double;
  par: TGDIPFillParameters;
begin
  if not picture.Empty then
  begin
    case location of
    ppTopLeft: picture.GDIPDraw(g, Rect(r.Left,r.Top,r.Left + w, r.Top + h));
    ppTopCenter:
    begin
      r.Left := r.Left + ((r.Right - r.Left) - w) div 2;
      picture.GDIPDraw(g, Rect(r.Left,r.Top,r.Left + w, r.Top + h));
    end;
    ppTopRight: picture.GDIPDraw(g, Rect(r.Right - w, r.Top, r.Right, r.Top + h));
    ppBottomLeft: picture.GDIPDraw(g, Rect(r.Left, r.Bottom - h, r.Left + w, r.Bottom));
    ppTiled:
    begin
      par.Graphics := g;
      par.R := MakeRect(r.Left, r.Top, r.Right - r.Left, r.Bottom - r.Top);
      par.GT := gtTexture;
      par.Image := picture;
      par.Image.Width := w;
      par.Image.Height := h;
      par.Fillpath := False;
      FillGDIP(par);
    end;
    ppBottomCenter:
    begin
      r.Left := r.Left + ((r.Right - r.Left) - w) div 2;
      picture.GDIPDraw(g, Rect(r.Left, r.Bottom - h, r.Left + w, r.Bottom));
    end;
    ppBottomRight: picture.GDIPDraw(g, Rect(r.Right - w, r.Bottom - h, r.Right, r.Bottom));
    ppStretched:
    begin
      if AspectRatio then
      begin
        GetAspectSize(xw, xh, w, h, r.Right - r.Left, r.Bottom - r.Top, AspectMode);
      end
      else
      begin
        xw := r.Right - r.Left;
        xh := r.Bottom - r.Top;
      end;

      picture.GDIPDraw(g, Bounds(Round(r.Left + (r.Right - r.Left - xw) / 2),
        Round(r.Top + (r.Bottom - r.Top - xh) / 2),Round(xw), Round(xh)));
    end;
    ppCustom: picture.GDIPDraw(g, Bounds(x, y, w, h));
    ppCenterLeft:
      begin
        r.Top := r.Top + ((r.Bottom - r.Top) - h) div 2;
        picture.GDIPDraw(g, Rect(r.Left,r.Top,r.Left + w, r.Top + h));
      end;
    ppCenterRight:
      begin
        r.Top := r.Top + ((r.Bottom - r.Top) - h) div 2;
        picture.GDIPDraw(g, Rect(r.Right - w,r.Top,r.Right, r.Top + h));
      end;
    ppCenterCenter:
      begin
        r.Left := r.Left + ((r.Right - r.Left) - w) div 2;
        r.Top := r.Top + ((r.Bottom - r.Top) - h) div 2;
        picture.GDIPDraw(g, Rect(r.Left,r.Top,r.Left + w, r.Top + h));
      end;
    end;
  end;
end;

{$IFDEF DIRECT2D}
function ConvertToBitmap(Source : TGraphic): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Width := Source.Width;
  Result.Height := Source.Height;
  Result.Canvas.Draw(0,0,Source);
end;

procedure DrawPicture2D(g: TDirect2DCanvas; picture: TAdvGDIPPicture; location: TFillPicturePosition; w, h: Double; r: TD2DRectF; x, y: Double);
var
  bmp2D: ID2D1Bitmap;
  rt: TD2DRectF;
begin
  if not picture.Empty then
  begin
    bmp2D := picture.Create2DBitmap(g);
    if not (bmp2D = nil) then
    begin
      case location of
      ppTopLeft: rt := D2D1RectF(r.left, r.top, r.left + w, r.top + h);
      ppTopCenter:  rt := D2D1RectF(r.Left + ((r.Right - r.Left) - w) / 2, r.top, r.Left + ((r.Right - r.Left) - w) / 2 + w, h);
      ppTopRight: rt := D2D1RectF(r.Right - w, r.Top, r.Right, r.Top + h);
      ppBottomLeft: rt := D2D1RectF(r.Left, r.Bottom - h, r.Left + w, r.Bottom);
      ppBottomCenter: rt := D2D1RectF(r.Left + ((r.Right - r.Left) - w) / 2, r.Bottom - h,r.left + ((r.Right - r.Left) - w) / 2 + w, r.Bottom);
      ppBottomRight: rt := D2D1RectF(r.Right - w, r.Bottom - h, r.Right, r.Bottom);
      ppStretched: rt := D2D1RectF(r.Left,r.Top,r.Right, r.Bottom);
      ppCustom:  rt := D2D1RectF(x, y, x + w, y + h);
      ppCenterLeft: rt := D2D1RectF(r.Left, r.Top + ((r.Bottom - r.Top) - h) / 2, r.Left + w, r.Top + ((r.Bottom - r.Top) - h) / 2 + h);
      ppCenterRight: rt :=D2D1RectF(r.Right - w,r.Top + ((r.Bottom - r.Top) - h) / 2,r.Right, r.Top + ((r.Bottom - r.Top) - h) / 2+ h);
      ppCenterCenter: rt := D2D1RectF(r.Left + ((r.Right - r.Left) - w) / 2,r.Top + ((r.Bottom - r.Top) - h) / 2,r.Left + ((r.Right - r.Left) - w) / 2 + w, r.Top + ((r.Bottom - r.Top) - h) / 2 + h);
      end;
      g.RenderTarget.DrawBitmap(bmp2D, @rt);
    end;
  end;
end;
{$ENDIF}

function TGDIPFill.Fill(Graphics: TGPGraphics; R: TGPRectF; ABorderOpacity: Byte = 255; AOpacity: Byte = 255; AOpacityTo: Byte = 255; AOpacityMirror: Byte = 255; AOpacityMirrorTo: Byte = 255): TGPRectF;
var
  path, pathshadow, pathfocus, pathborder: TGPGraphicsPath;
  rnonmirror, rmirror: TGPRectF;
  gBrush: TGPPathGradientBrush;
  cb: array[0..2] of TGPColor;
  pb: array[0..2] of single;
  h, h2: Double;
  c, ct: integer;
  pw, ph: integer;
  ocbdr, oc, octo, ocmr, ocmrto: Byte;
  pfocus, pBorder: TGPPen;
  rc: TRect;
  rgn: TGPRegion;
  imgr: TGPRectF;
  oldsmt: SmoothingMode;
begin
  if AOpacity < 255 then
    oc := AOpacity
  else
    oc := Opacity;

  if AOpacityTo < 255 then
    octo := AOpacityTo
  else
    octo := OpacityTo;

  if AOpacityMirror < 255 then
    ocmr := AOpacityMirror
  else
    ocmr := OpacityMirror;

  if AOpacityMirrorTo < 255 then
    ocmrto := AOpacityMirrorTo
  else
    ocmrto := OpacityMirrorTo;

  if ABorderOpacity < 255 then
    ocbdr := ABorderOpacity
  else
    ocbdr := BorderOpacity;

  if (R.Height <> 1) and (R.Width <> 1) then
  begin
    if Graphics = nil then
      Exit;

    path := nil;

    if (ShadowOffset > 0) and (ShadowColor <> clNone) then
    begin
      case ShadowType of


      stRightBottom, stSurround:
        begin
          if (ShadowType = stSurround) then
            pathshadow := CreateRoundRectangle(r, ShadowOffset, rtBoth, true)
          else
            pathshadow := CreateRoundRectangle(r, Rounding, RoundingType, true);
          gBrush := TGPPathGradientBrush.Create(pathshadow);
          gBrush.SetWrapMode(WrapModeClamp);

          cb[0] := MakeColor(0,0,0,0);
          cb[1] := MakeColor(180, ShadowColor);
          cb[2] := MakeColor(180, ShadowColor);

          pb[0] := 0;
          pb[1] := 0.1;
          pb[2] := 1;

          gBrush.SetInterpolationColors(@cb, @pb, 3);

          Graphics.FillPath(gbrush, pathshadow);
          pathshadow.Free;
          gBrush.Free;

          if ShadowType = stSurround then
          begin
            R.X := R.X + ShadowOffset;
            R.Y := R.Y + ShadowOffset;
            R.Width := R.Width - 2 * ShadowOffset;
            R.Height := R.Height - 2 * ShadowOffset;
          end
          else
          begin
            R.Width := R.Width - ShadowOffset;
            R.Height := R.Height - ShadowOffset;
          end;
        end;
      stBottom:
        begin
          rnonmirror := MakeRect(r.X, r.Y + r.Height - ShadowOffset *2, r.Width, 2 * ShadowOffset);

          DrawRadialBackGround(Graphics, rnonmirror, ShadowColor, ShadowColor, 0, 255, true, Rounding, rtBottom);

          r.Height := r.Height - ShadowOffset;
        end;
      end;
    end;

    if (FGradientType <> gtHatch) and (FGradientType <> gtTexture) and (FColorMirror <> clNone) then
    begin
      h := r.Height;

      h2 := h / 2;

      rnonmirror := MakeRect(r.X, R.Y, R.Width, R.Height - h2 + 1);

      //rnonmirror := MakeRect(r.X, R.Y, R.Width, h2);
      if FGradientType = gtRadial then
      begin
        if Color <> clNone then
        begin
          c := Color;
          ct := ColorTo;
          if ColorTo = clNone then
          begin
            ct := c;
          end;
          DrawRadialBackGround(Graphics, rnonmirror, c, ct, oc, octo, true, Rounding, rtTop)
        end
      end
      else
        DrawGradientBackGround(Graphics, rnonmirror, false, oc, octo, ocmr, ocmrto);


      rmirror := MakeRect(r.X, r.Y + h2 - 1, r.Width, r.Height - h2 + 1);

      //rmirror := MakeRect(r.X, r.Y + h2 , r.Width, h2);

      if GradientMirrorType = gtRadial then
      begin
        if ColorMirror <> clNone then
        begin
          c := ColorMirror;
          ct := ColorMirrorTo;
          if ColorMirrorTo = clNone then
          begin
            ct := c;
          end;
          DrawRadialBackGround(Graphics, rmirror, c, ct, ocmr, ocmrto, false, Rounding, rtBottom)
        end;
      end
      else
        DrawGradientBackGround(Graphics, rmirror, true, oc, octo, ocmr, ocmrto);
    end
    else
    begin
      if FGradientType = gtRadial then
      begin
        if Color <> clNone then
        begin
          c := Color;
          ct := ColorTo;
          if ColorTo = clNone then
          begin
            ct := c;
          end;
          DrawRadialBackGround(Graphics, r, c, ct, oc, octo, true, Rounding, RoundingType)
        end
      end
      else
        DrawGradientBackGround(Graphics, r, false, oc, octo, ocmr, ocmrto);
    end;

    if path <> nil then
      path.Free;

    if BackGroundPictureMode = pmInsideFill then
    begin
      if not BackGroundPicture.Empty then
      begin
        BackGroundPicture.GetImageSizes;
        pw := BackGroundPicture.Width;
        ph := BackGroundPicture.Height;

        rc := Bounds(Round(r.X), Round(r.Y), Round(r.Width), Round(r.Height));

        if (FGradientType = gtTexture) then
        begin
          path := nil;
          rgn := nil;
          if BackGroundPictureMode = pmInsideFill then
          begin
            imgr := r;
            path := CreateRoundRectangle(imgr, Rounding, RoundingType, false);
            rgn := TGPRegion.Create(path);
            graphics.SetClip(rgn);
          end;

          if BackGroundPicturePosition <> ppCustom then
          begin
            rc.Left := rc.Left + BackGroundPictureLeft;
            rc.Top := rc.Top + BackGroundPictureTop;
            rc.Right := rc.Right + BackGroundPictureLeft;
            rc.Bottom := rc.Bottom + BackGroundPictureTop;
            DrawPicture(Graphics, BackGroundPictureAspectRatio, BackGroundPictureAspectMode, BackGroundPicture, FBackGroundPicturePosition, pw, ph, rc,  rc.Left, rc.Top)
          end
          else
            DrawPicture(Graphics, BackGroundPictureAspectRatio, BackGroundPictureAspectMode, BackGroundPicture, FBackGroundPicturePosition, pw, ph, rc,  BackGroundPictureLeft, BackGroundPictureTop);

          if BackGroundPictureMode = pmInsideFill then
          begin
            graphics.ResetClip;
            if rgn <> nil then
              rgn.Free;

            if path <> nil then
              path.Free;
          end;
        end;
      end;
    end;

    if (BorderWidth > 0) and (BorderColor <> clNone) then
    begin
      pathborder := CreateRoundRectangle(r, Rounding, RoundingType, false);
      pBorder := TGPPen.Create(MakeColor(ocbdr, BorderColor), BorderWidth);
      pBorder.SetDashStyle(DashStyleSolid);
      Graphics.DrawPath(pBorder, pathborder);
      pBorder.Free;
      pathborder.Free;
    end;
  end
  else
  begin
    if (BorderWidth > 0) and (BorderColor <> clNone) then
    begin
      pBorder := TGPPen.Create(MakeColor(ocbdr, BorderColor), BorderWidth);
      Graphics.DrawLine(pBorder, r.X, r.Y, r.X + R.Width, r.Y + R.Height);
      pBorder.Free;
    end;
  end;

  DrawGlow(Graphics, R);

  FFocusRect := MakeRect(R.X + BorderWidth, R.Y + BorderWidth, R.Width - (BorderWidth * 2), R.Height - (BorderWidth * 2));
  if focus then
  begin
    pathfocus := CreateRoundRectangle(FFocusRect, Rounding - BorderWidth, RoundingType, false);
    oldsmt := graphics.GetSmoothingMode;
    graphics.SetSmoothingMode(SmoothingModeDefault);
    pfocus := TGPPen.Create(MakeColor(oc, focuscolor), 1);
    pfocus.SetDashStyle(DashStyleDot);
    Graphics.DrawPath(pfocus, pathfocus);
    pfocus.Free;
    pathfocus.Free;
    graphics.SetSmoothingMode(oldsmt);
  end;

  if BackGroundPictureMode = pmOutsideFill then
  begin
    if not BackGroundPicture.Empty then
    begin
      BackGroundPicture.GetImageSizes;
      pw := BackGroundPicture.Width;
      ph := BackGroundPicture.Height;

      rc := Bounds(Round(r.X), Round(r.Y), Round(r.Width), Round(r.Height));

      if (FGradientType = gtTexture) then
      begin
        if BackGroundPicturePosition <> ppCustom then
        begin
          rc.Left := rc.Left + BackGroundPictureLeft;
          rc.Top := rc.Top + BackGroundPictureTop;
          rc.Right := rc.Right + BackGroundPictureLeft;
          rc.Bottom := rc.Bottom + BackGroundPictureTop;
          DrawPicture(Graphics, PictureAspectRatio, PictureAspectMode, BackGroundPicture, FBackGroundPicturePosition, pw, ph, rc,  rc.Left, rc.Top)
        end
        else
          DrawPicture(Graphics, PictureAspectRatio, PictureAspectMode, BackGroundPicture, FBackGroundPicturePosition, pw, ph, rc,  BackGroundPictureLeft, BackGroundPictureTop);
      end;
    end;
  end;

  pw := 0;
  ph := 0;
  if not picture.Empty then
  begin
    picture.GetImageSizes;
    case FPictureSize of
      psOriginal:
      begin
        pw := Picture.Width;
        ph := Picture.Height;
      end;
      psCustom:
      begin
        pw := FPictureWidth;
        ph := FPictureHeight;
      end;
    end;

    rc := Bounds(Round(r.X), Round(r.Y), Round(r.Width), Round(r.Height));

    if FPicturePosition <> ppCustom then
    begin
      rc.Left := rc.Left + PictureLeft;
      rc.Right := rc.Right + PictureLeft;
      rc.Top := rc.Top + PictureTop;
      rc.Bottom := rc.Bottom + PictureTop;
      DrawPicture(Graphics, PictureAspectRatio,PictureAspectMode, Picture, FPicturePosition, pw, ph,  rc,  rc.Left, rc.Top)
    end
    else
      DrawPicture(Graphics, PictureAspectRatio,PictureAspectMode, Picture, FPicturePosition, pw, ph, rc,  PictureLeft, PictureTop);
  end;

  Result := R;
end;

{$IFDEF DIRECT2D}
function TGDIPFill.Fill2D(Graphics: TDirect2DCanvas; R: TD2DRectF;
  ABorderOpacity, AOpacity, AOpacityTo, AOpacityMirror,
  AOpacityMirrorTo: Byte): TD2DRectF;
var
  h, h2: Double;
  c, ct: integer;
  pw, ph: integer;
  ocbdr, oc, octo, ocmr, ocmrto: Byte;
  rnonmirror, rmirror, rc: TD2D1RectF;

  geoSink: ID2D1GeometrySink;
  geoPath: ID2D1PathGeometry;
  Path2D: ID2D1PathGeometry;
  Rect2D: ID2D1RectangleGeometry;
  ShadowPath2D: ID2D1PathGeometry;
  ShadowStops2D: array[0 .. 3] of TD2D1GradientStop;
  ShadowgBrushGradient2D: ID2D1GradientStopCollection;
  ShadowgBrush2D: ID2D1LinearGradientBrush;
  ShadowgBrushProp2D: TD2D1LinearGradientBrushProperties;
  ShadowBrush2D: TDirect2DBrush;

  bounds: TD2DRectF;
  geoLayer: ID2D1Layer;
  geoLayerParam: D2D1_LAYER_PARAMETERS;
  solGrgBrush2D: ID2D1Brush;
begin
  if AOpacity < 255 then
    oc := AOpacity
  else
    oc := Opacity;

  if AOpacityTo < 255 then
    octo := AOpacityTo
  else
    octo := OpacityTo;

  if AOpacityMirror < 255 then
    ocmr := AOpacityMirror
  else
    ocmr := OpacityMirror;

  if AOpacityMirrorTo < 255 then
    ocmrto := AOpacityMirrorTo
  else
    ocmrto := OpacityMirrorTo;

  if ABorderOpacity < 255 then
    ocbdr := ABorderOpacity
  else
    ocbdr := BorderOpacity;

  if (R.right - r.left <> 1) and (R.bottom - r.top <> 1) then
  begin
    if Graphics = nil then
      Exit;

    if (ShadowOffset > 0) and (ShadowColor <> clNone) then
    begin
      ShadowPath2D := CreateRoundRectangle2D(R, Rounding, RoundingType, False);

      ShadowStops2D[0].position := 0;
      ShadowStops2D[1].position := 0.1;
      ShadowStops2D[2].position := 0.9;
      ShadowStops2D[3].position := 1;

      ShadowStops2D[0].color := D2D1ColorF(ShadowColor, 0);
      ShadowStops2D[1].color := D2D1ColorF(ShadowColor, 0.3);
      ShadowStops2D[2].color := D2D1ColorF(ShadowColor, 0.3);
      ShadowStops2D[3].color := D2D1ColorF(ShadowColor, 0);

      ShadowgBrushProp2D.startPoint := D2D1PointF(r.left, r.top + (r.bottom - r.top) / 2);
      ShadowgBrushProp2D.endPoint := D2D1PointF(r.right, r.top + (r.bottom - r.top) / 2);

      graphics.RenderTarget.CreateGradientStopCollection(@ShadowStops2D[0], Length(ShadowStops2D), D2D1_GAMMA_1_0,
        D2D1_EXTEND_MODE_CLAMP, ShadowgBrushGradient2D);

      graphics.RenderTarget.CreateLinearGradientBrush(ShadowgBrushProp2D, nil, ShadowgBrushGradient2D, ShadowgBrush2D);
      ShadowBrush2D := TDirect2DBrush.Create(Graphics);
      ShadowBrush2D.Handle := ShadowgBrush2D;
      graphics.Brush := ShadowBrush2D;

      Graphics.FillGeometry(ShadowPath2D);

      ShadowBrush2D.Free;
      R.right := R.right - ShadowOffset;
      R.bottom := R.bottom - ShadowOffset;
    end;

    if (FGradientType <> gtHatch) and (FGradientType <> gtTexture) and (FColorMirror <> clNone) then
    begin
      h := r.bottom - R.top;

      h2 := h / 2;

      rnonmirror := D2D1RectF(r.left - 1, R.top - 1, R.right, R.bottom - h2 + 1);

      if FGradientType = gtRadial then
      begin
        if Color <> clNone then
        begin
          c := Color;
          ct := ColorTo;
          if ColorTo = clNone then
          begin
            ct := c;
          end;
          DrawRadialBackGround2D(Graphics, rnonmirror, c, ct, oc, octo, true, Rounding, rtTop)
        end
      end
      else
      DrawGradientBackGround2D(Graphics, rnonmirror, false, oc, octo, ocmr, ocmrto);

      rmirror := D2D1RectF(r.left - 1, r.top + h2 - 1, r.right, r.bottom - h2 - 1);

      if GradientMirrorType = gtRadial then
      begin
        if ColorMirror <> clNone then
        begin
          c := ColorMirror;
          ct := ColorMirrorTo;
          if ColorMirrorTo = clNone then
          begin
            ct := c;
          end;
          DrawRadialBackGround2D(Graphics, rmirror, c, ct, ocmr, ocmrto, false, Rounding, rtBottom)
        end;
      end
      else
      DrawGradientBackGround2D(Graphics, rmirror, true, oc, octo, ocmr, ocmrto);
    end
    else
    begin
      if FGradientType = gtRadial then
      begin
        if Color <> clNone then
        begin
          c := Color;
          ct := ColorTo;
          if ColorTo = clNone then
          begin
            ct := c;
          end;
          DrawRadialBackGround2D(Graphics, r, c, ct, oc, octo, true, Rounding, RoundingType)
        end
      end
      else
        DrawGradientBackGround2D(Graphics, r, false, oc, octo, ocmr, ocmrto);
    end;

    if BackGroundPictureMode = pmInsideFill then
    begin
      pw := 0;
      ph := 0;
      if not BackGroundPicture.Empty then
      begin
        BackGroundPicture.GetImageSizes;
        pw := BackGroundPicture.Width;
        ph := BackGroundPicture.Height;
      end;

      rc := r;

      if (FGradientType = gtTexture) then
      begin
        if BackGroundPictureMode = pmInsideFill then
        begin
          D2DFactory.CreateRectangleGeometry(r, Rect2D);
          Path2D := CreateRoundRectangle2D(r, Rounding, RoundingType, false);

          D2DFactory.CreatePathGeometry(geoPath);
          geoPath.Open(geoSink);

          path2D.CombineWithGeometry(Rect2D, D2D1_COMBINE_MODE_INTERSECT, TD2D1Matrix3x2F.Identity, D2D1_DEFAULT_FLATTENING_TOLERANCE, geoSink);
          geoSink.Close;

          graphics.Brush.Color := clRed;
          graphics.RenderTarget.CreateLayer(nil, geoLayer);
          geoLayerParam.geometricMask := geoPath;
          geoLayerParam.maskTransform := TD2DMatrix3x2F.Identity;
          geoLayerParam.geometricMask.GetBounds(geoLayerParam.maskTransform, bounds);
          geoLayerParam.contentBounds := bounds;
          geoLayerParam.maskAntialiasMode := D2D1_ANTIALIAS_MODE_PER_PRIMITIVE;
          geoLayerParam.opacityBrush := nil;
          geoLayerParam.layerOptions := D2D1_LAYER_OPTIONS_NONE;
          geoLayerParam.opacity := 1;
          Graphics.RenderTarget.PushLayer(geoLayerParam, geoLayer);
        end;

        if BackGroundPicturePosition <> ppCustom then
        begin
          rc.Left := rc.Left + BackGroundPictureLeft;
          rc.Top := rc.Top + BackGroundPictureTop;
          rc.Right := rc.Right + BackGroundPictureLeft;
          rc.Bottom := rc.Bottom + BackGroundPictureTop;
          DrawPicture2D(Graphics, BackGroundPicture, FBackGroundPicturePosition, pw, ph, rc,  rc.Left, rc.Top)
        end
        else
          DrawPicture2D(Graphics, BackGroundPicture, FBackGroundPicturePosition, pw, ph, rc,  BackGroundPictureLeft, BackGroundPictureTop);

        if BackGroundPictureMode = pmInsideFill then
        begin
          graphics.RenderTarget.PopLayer;
        end;
      end;
    end;

    if (BorderWidth > 0) and (BorderColor <> clNone) then
    begin
      Path2D := CreateRoundRectangle2D(R, Rounding, RoundingType, False);
      solGrgBrush2D := Graphics.CreateBrush(BorderColor);
      solGrgBrush2D.SetOpacity(ocbdr / 255);
      graphics.RenderTarget.DrawGeometry(Path2D, solGrgBrush2D, BorderWidth);
    end;
  end
  else
  begin
    if (BorderWidth > 0) and (BorderColor <> clNone) then
    begin
      solGrgBrush2D := Graphics.CreateBrush(BorderColor);
      solGrgBrush2D.SetOpacity(ocbdr / 255);
      graphics.RenderTarget.DrawLine(D2D1PointF(r.left, r.top), D2D1PointF(r.right, r.bottom), solGrgBrush2D, BorderWidth);
    end;
  end;

  DrawGlow2D(Graphics, R);

//  FFocusRect := MakeRect(R.X + BorderWidth, R.Y + BorderWidth, R.Width - (BorderWidth * 2), R.Height - (BorderWidth * 2));
//  if focus then
//  begin
//    pathfocus := CreateRoundRectangle(FFocusRect, Rounding - BorderWidth, RoundingType, false);
//    oldsmt := graphics.GetSmoothingMode;
//    graphics.SetSmoothingMode(SmoothingModeDefault);
//    pfocus := TGPPen.Create(MakeColor(oc, focuscolor), 1);
//    pfocus.SetDashStyle(DashStyleDot);
//    Graphics.DrawPath(pfocus, pathfocus);
//    pfocus.Free;
//    pathfocus.Free;
//    graphics.SetSmoothingMode(oldsmt);
//  end;

  if BackGroundPictureMode = pmOutsideFill then
  begin
    pw := 0;
    ph := 0;
    if not BackGroundPicture.Empty then
    begin
      BackGroundPicture.GetImageSizes;
      pw := BackGroundPicture.Width;
      ph := BackGroundPicture.Height;
    end;

    rc := r;

    if (FGradientType = gtTexture) then
    begin
      if BackGroundPicturePosition <> ppCustom then
      begin
        rc.Left := rc.Left + BackGroundPictureLeft;
        rc.Top := rc.Top + BackGroundPictureTop;
        rc.Right := rc.Right + BackGroundPictureLeft;
        rc.Bottom := rc.Bottom + BackGroundPictureTop;
        DrawPicture2D(Graphics, BackGroundPicture, FBackGroundPicturePosition, pw, ph, rc,  rc.Left, rc.Top)
      end
      else
        DrawPicture2D(Graphics, BackGroundPicture, FBackGroundPicturePosition, pw, ph, rc,  BackGroundPictureLeft, BackGroundPictureTop);
    end;
  end;

  pw := 0;
  ph := 0;
  if not picture.Empty then
  begin
    picture.GetImageSizes;
    case FPictureSize of
      psOriginal:
      begin
        pw := Picture.Width;
        ph := Picture.Height;
      end;
      psCustom:
      begin
        pw := FPictureWidth;
        ph := FPictureHeight;
      end;
    end;
  end;

  if FPicturePosition <> ppCustom then
  begin
    rc.Left := rc.Left + PictureLeft;
    rc.Top := rc.Top + PictureTop;
    rc.Right := rc.Right + PictureLeft;
    rc.Bottom := rc.Bottom + PictureTop;
    DrawPicture2D(Graphics, Picture, FPicturePosition, pw, ph,  rc,  rc.Left, rc.Top)
  end
  else
    DrawPicture2D(Graphics, Picture, FPicturePosition, pw, ph, rc,  PictureLeft, PictureTop);

  Result := R;
end;
{$ENDIF}

procedure TGDIPFill.LoadFromClipBoard;
begin
  Load(Self);
end;

procedure TGDIPFill.LoadFromFile(FileName:string; Section: String);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(FileName);
  LoadFromFile(ini, Section);
  ini.Free;
end;

procedure TGDIPFill.LoadFromFile(ini: TIniFile; Section: String);
var
  ms: TMemoryStream;
  p,bp: string;
  nsize: integer;
begin
  FColor := ini.ReadInteger(Section, 'Color', Color);
  FColorTo := ini.ReadInteger(Section, 'ColorTo', ColorTo);
  FColorMirror := ini.ReadInteger(Section, 'ColorMirror', ColorMirror);
  FColorMirrorTo := ini.ReadInteger(Section, 'ColorMirrorTo', ColorMirrorTo);
  FGradientType := TAdvGradientType(ini.ReadInteger(Section, 'GradientType', integer(GradientType)));
  FGradientMirrorType := TAdvGradientType(ini.ReadInteger(Section, 'GradientMirrorType', integer(GradientMirrorType)));
  FHatchStyle := THatchStyle(ini.ReadInteger(Section, 'HatchStyle', integer(HatchStyle)));
  FHatchStyleMirror := THatchStyle(ini.ReadInteger(Section, 'HatchStyleMirror', integer(HatchStyleMirror)));
  FBackGroundPictureMode := TFillPictureMode(ini.ReadInteger(Section, 'BackGroundPictureMode', integer(BackGroundPictureMode)));
//    property BackGroundPicture: TAdvGDIPPicture read FBackGroundPicture write SetBackGroundPicture;
  FBackGroundPicturePosition := TFillPicturePosition(ini.ReadInteger(Section, 'BackGroundPicturePosition', integer(BackGroundPicturePosition)));
  FBackGroundPictureLeft := ini.ReadInteger(Section, 'BackGroundPictureLeft', BackGroundPictureLeft);
  FBackGroundPictureTop := ini.ReadInteger(Section, 'BackGroundPictureTop', BackGroundPicturetop);
//    property Picture: TAdvGDIPPicture read FPicture write SetPicture;
  FPicturePosition := TFillPicturePosition(ini.ReadInteger(Section, 'PicturePosition', integer(PicturePosition)));
  FPictureLeft := ini.ReadInteger(Section, 'PictureLeft', PictureLeft);
  FPictureTop := ini.ReadInteger(Section, 'PictureTop', Picturetop);
  FpictureSize := TFillPictureSize(ini.ReadInteger(Section, 'PictureSize', integer(Picturesize)));
  FPictureWidth := ini.ReadInteger(Section, 'PictureWidth', PictureWidth);
  FPictureHeight := ini.ReadInteger(Section, 'PictureHeight', PictureHeight);
  FOpacity := ini.ReadInteger(Section, 'Opacity', Opacity);
  FOpacityTo := ini.ReadInteger(Section, 'OpacityTo', OpacityTo);
  FOpacityMirror := ini.ReadInteger(Section, 'OpacityMirror', OpacityMirror);
  FOpacityMirrorTo := ini.ReadInteger(Section, 'OpacityMirrorTo', OpacityMirrorTo);
  FBorderColor := ini.ReadInteger(Section, 'BorderColor', BorderColor);
  FBorderOpacity := ini.ReadInteger(Section, 'BorderOpacity', BorderOpacity);
  FBorderWidth := ini.ReadInteger(Section, 'BorderWidth', BorderWidth);
  FRounding := ini.ReadInteger(Section, 'Rounding', Rounding);
  FRoundingType := TFillRoundingType(ini.ReadInteger(Section, 'RoundingType', integer(RoundingType)));
  FShadowOffset := ini.ReadInteger(Section, 'ShadowOffset', ShadowOffset);
  FShadowColor := ini.ReadInteger(Section, 'ShadowColor', ShadowColor);
  FAngle := ini.ReadInteger(Section, 'Angle', Angle);
  FGlow := TGlowMode(ini.ReadInteger(Section, 'Glow', Integer(Glow)));
  FGlowGradientColor := ini.ReadInteger(Section, 'GlowGradientColor', GlowGradientColor);
  FGlowRadialColor := ini.ReadInteger(Section, 'GlowRadialColor', GlowRadialColor);

  nsize := ini.ReadInteger(Section,'PictureSize',0);

  if nsize > 0 then
  begin
    ms := TMemoryStream.Create;
    SetLength(p,nsize+1);
    GetPrivateProfileString(PChar(Section),'Picture','',pchar(p),nsize+1,PChar(ini.FileName));
    StringToStream(p,ms);
    ms.Position := 0;
    Picture.LoadFromStream(ms);
    ms.Free;
  end;

  nsize := ini.ReadInteger(Section,'BackgroundPictureSize',0);
  if nsize > 0 then
  begin
    ms := TMemoryStream.Create;
    SetLength(bp,nsize+1);
    GetPrivateProfileString(PChar(Section),'BackgroundPicture','',pchar(bp),nsize+1,PChar(ini.FileName));
    StringToStream(bp,ms);
    ms.Position := 0;
    BackgroundPicture.LoadFromStream(ms);
    ms.Free;
  end;
  Changed;

end;

procedure TGDIPFill.PictureChanged(Sender: TObject);
begin
  Changed;
end;

{ TGDIPFill }


procedure TGDIPFill.Assign(Source: TPersistent);
begin
  if Source is TGDIPFill then
  begin
    FPictureAspectRatio := (Source as TGDIPFill).PictureAspectRatio;
    FPictureAspectMode := (Source as TGDIPFill).PictureAspectMode;
    FBackGroundPictureAspectRatio := (Source as TGDIPFill).BackGroundPictureAspectRatio;
    FBackGroundPictureAspectMode := (Source as TGDIPFill).BackGroundPictureAspectMode;
    FColor := (Source as TGDIPFill).Color;
    FColorTo := (Source as TGDIPFill).ColorTo;
    FColorMirror := (Source as TGDIPFill).ColorMirror;
    FColorMirrorTo := (Source as TGDIPFill).ColorMirrorTo;
    FGradientType := (Source as TGDIPFill).GradientType;
    FGradientMirrorType := (Source as TGDIPFill).GradientMirrorType;
    FHatchStyle := (Source as TGDIPFill).HatchStyle;
    FHatchStyleMirror := (Source as TGDIPFill).HatchStyleMirror;
    FBackGroundPicture.Assign((Source as TGDIPFill).BackGroundPicture);
    FBackGroundPicturePosition := (Source as TGDIPFill).BackGroundPicturePosition;
    FBackGroundPictureLeft := (Source as TGDIPFill).BackGroundPictureLeft;
    FBackGroundPictureTop := (Source as TGDIPFill).BackGroundPictureTop;
    FPicture.Assign((Source as TGDIPFill).Picture);
    FPicturePosition := (Source as TGDIPFill).PicturePosition;
    FPictureLeft := (Source as TGDIPFill).PictureLeft;
    FPictureTop := (Source as TGDIPFill).PictureTop;
    FPictureSize := (Source as TGDIPFill).PictureSize;
    FPictureWidth := (Source as TGDIPFill).PictureWidth;
    FPictureHeight := (Source as TGDIPFill).PictureHeight;
    FOpacity := (Source as TGDIPFill).Opacity;
    FOpacityTo := (Source as TGDIPFill).OpacityTo;
    FOpacityMirror := (Source as TGDIPFill).OpacityMirror;
    FOpacityMirrorTo := (Source as TGDIPFill).OpacityMirrorTo;
    FBorderColor := (Source as TGDIPFill).BorderColor;
    FBorderWidth := (Source as TGDIPFill).BorderWidth;
    FRounding := (Source as TGDIPFill).Rounding;
    FRoundingType := (Source as TGDIPFill).RoundingType;
    FShadowColor := (Source as TGDIPFill).ShadowColor;
    FShadowOffset := (Source as TGDIPFill).ShadowOffset;
    FShadowType := (Source as TGDIPFill).ShadowType;
    FAngle := (Source as TGDIPFill).Angle;
    FBorderOpacity := (Source as TGDIPFill).BorderOpacity;
    FBackGroundPictureMode := (Source as TGDIPFill).BackGroundPictureMode;
    FGlowGradientColor := (Source as TGDIPFill).GlowGradientColor;
    FGlowRadialColor := (Source as TGDIPFill).GlowRadialColor;
    FGlow := (Source as TGDIPFill).Glow;
    Changed;
  end;
end;

procedure TGDIPFill.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TGDIPFill.Changed;
begin
  if Assigned(FOnChange) and (FUpdateCount = 0) then
    FOnChange(Self);
end;

constructor TGDIPFill.Create;
begin
  inherited;
  FUpdateCount := 0;
  FBackGroundPicture := TAdvGDIPPicture.Create;
  FBackGroundPicture.OnChange := PictureChanged;
  FPicture := TAdvGDIPPicture.Create;
  FPicture.OnChange := PictureChanged;
  FPictureAspectRatio := False;
  FBackGroundPictureAspectRatio := False;
  FBorderOpacity := 255;
  FAngle := 0;
  FFocus := false;
  FFocusColor := clBlack;
  FPictureSize := psOriginal;
  FPictureWidth := 50;
  FPictureHeight := 50;
  FBackGroundPictureTop := 0;
  FBackGroundPictureLeft := 0;
  FBackGroundPicturePosition := ppTopLeft;
  FPictureTop := 0;
  FPictureLeft := 0;
  FColor := clSilver;
  FColorTo := clGray;
  FColorMirror := clNone;
  FColorMirrorTo := clNone;
  FGradientType := gtVertical;
  FGradientMirrorType := gtSolid;
  FHatchStyle := HatchStyleHorizontal;
  FPicturePosition := ppTopLeft;
  FOpacity := 255;
  FOpacityTo := 255;
  FOpacityMirror := 255;
  FOpacityMirrorTo := 255;
  FBorderColor := clNone;
  FBorderWidth := 1;
  FRounding := 0;
  FRoundingType := rtBoth;
  FShadowColor := clGray;
  FShadowOffset := 0;
  FBackGroundPictureMode := pmOutsideFill;
  FGlow := gmNone;
  FGlowRadialColor := clWhite;
  FGlowGradientColor := clWhite;
end;

destructor TGDIPFill.Destroy;
begin
  Picture.Free;
  BackGroundPicture.Free;
  inherited;
end;

procedure TGDIPFill.SaveToClipBoard;
begin
  Save(Self);
end;


procedure TGDIPFill.SaveToFile(FileName: string; Section: String);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(FileName);
  SaveToFile(ini, Section);
  ini.Free;
end;


procedure TGDIPFill.SaveToFile(ini: TIniFile; Section: String);
var
  ms: TMemoryStream;
  s:string;
begin
  ini.WriteInteger(Section, 'Color', Color);
  ini.WriteInteger(Section, 'ColorTo', ColorTo);
  ini.WriteInteger(Section, 'ColorMirror', ColorMirror);
  ini.WriteInteger(Section, 'ColorMirrorTo', ColorMirrorTo);
  ini.WriteInteger(Section, 'GradientType', integer(GradientType));
  ini.WriteInteger(Section, 'GradientMirrorType', integer(GradientMirrorType));
  ini.WriteInteger(Section, 'HatchStyle', integer(HatchStyle));
  ini.WriteInteger(Section, 'HatchStyleMirror', integer(HatchStyleMirror));
  ini.WriteInteger(Section, 'BackGroundPictureMode', integer(BackGroundPictureMode));
//    property BackGroundPicture: TAdvGDIPPicture read FBackGroundPicture write SetBackGroundPicture;
  ini.WriteInteger(Section, 'BackGroundPicturePosition', integer(BackGroundPicturePosition));
  ini.WriteInteger(Section, 'BackGroundPictureLeft', BackGroundPictureLeft);
  ini.WriteInteger(Section, 'BackGroundPictureTop', BackGroundPicturetop);
//    property Picture: TAdvGDIPPicture read FPicture write SetPicture;
  ini.WriteInteger(Section, 'PicturePosition', integer(PicturePosition));
  ini.WriteInteger(Section, 'PictureLeft', PictureLeft);
  ini.WriteInteger(Section, 'PictureTop', Picturetop);
  ini.WriteInteger(Section, 'PictureSize', integer(Picturesize));
  ini.WriteInteger(Section, 'PictureWidth', PictureWidth);
  ini.WriteInteger(Section, 'PictureHeight', PictureHeight);
  ini.WriteInteger(Section, 'Opacity', Opacity);
  ini.WriteInteger(Section, 'OpacityTo', OpacityTo);
  ini.WriteInteger(Section, 'OpacityMirror', OpacityMirror);
  ini.WriteInteger(Section, 'OpacityMirrorTo', OpacityMirrorTo);
  ini.WriteInteger(Section, 'BorderColor', BorderColor);
  ini.WriteInteger(Section, 'BorderOpacity', BorderOpacity);
  ini.WriteInteger(Section, 'BorderWidth', BorderWidth);
  ini.WriteInteger(Section, 'Rounding', Rounding);
  ini.WriteInteger(Section, 'RoundingType', integer(RoundingType));
  ini.WriteInteger(Section, 'ShadowOffset', ShadowOffset);
  ini.WriteInteger(Section, 'ShadowColor', ShadowColor);
  ini.WriteInteger(Section, 'Angle', Angle);
  ini.WriteInteger(Section, 'Glow', Integer(Glow));
  ini.WriteInteger(Section, 'GlowGradientColor', GlowGradientColor);
  ini.WriteInteger(Section, 'GlowRadialColor', GlowRadialColor);

  ms := TMemoryStream.Create;
  Picture.SaveToStream(ms);
  s := StreamToString(ms);
  ini.WriteString(Section, 'Picture', s);
  ini.WriteInteger(Section, 'PictureSize', length(s));

  ms.Free;

  ms := TMemoryStream.Create;
  BackgroundPicture.SaveToStream(ms);
  s := StreamToString(ms);
  ini.WriteString(Section, 'BackgroundPicture', s);
  ini.WriteInteger(Section, 'BackgroundPictureSize', length(s));  
  ms.Free;
end;

procedure TGDIPFill.SetAngle(const Value: integer);
begin
  if FAngle <> value then
  begin
    FAngle := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetBackGroundPicture(const Value: TAdvGDIPPicture);
begin
  if FBackGroundPicture <> value then
  begin
    FBackGroundPicture.Assign(value);
    Changed;
  end;
end;

procedure TGDIPFill.SetBackGroundPictureAspectMode(const Value: TPictureMode);
begin
  if BackGroundPictureAspectMode <> Value then
  begin
    FBackGroundPictureAspectMode := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetBackGroundPictureAspectRatio(const Value: Boolean);
begin
  if FBackGroundPictureAspectRatio <> Value then
  begin
    FBackGroundPictureAspectRatio := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetBackGroundPictureLeft(const Value: integer);
begin
  if FBackGroundPictureLeft <> value then
  begin
    FBackGroundPictureLeft := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetBackGroundPictureMode(const Value: TFillPictureMode);
begin
  if FBackGroundPictureMode <> Value then
  begin
    FBackGroundPictureMode := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetBackGroundPicturePosition(const Value: TFillPicturePosition);
begin
  if FBackGroundPicturePosition <> value then
  begin
    FBackGroundPicturePosition := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetBackGroundPictureTop(const Value: integer);
begin
  if FBackGroundPictureTop <> value then
  begin
    FBackGroundPictureTop := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetBorderOpacity(const Value: Byte);
begin
  if FBorderOpacity <> value then
  begin
    FBorderOpacity := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetBorderWidth(const Value: integer);
begin
  if FBorderWidth <> value then
  begin
    FBorderWidth := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetColor(const Value: TColor);
begin
  if FColor <> value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetColorMirror(const Value: TColor);
begin
  if FColorMirror <> value then
  begin
    FColorMirror := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetColorMirrorTo(const Value: TColor);
begin
  if FColorMirrorTo <> value then
  begin
    FColorMirrorTo := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetColorTo(const Value: TColor);
begin
  if FColorTo <> value then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetFocus(const Value: Boolean);
begin
  if FFocus <> value then
  begin
    FFocus := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetFocusColor(const Value: TColor);
begin
  if FFocusColor <> value then
  begin
    FFocusColor := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetGlow(const Value: TGlowMode);
begin
  if FGlow <> Value then
  begin
    FGlow := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetGlowGradientColor(const Value: TColor);
begin
  if FGlowGradientColor <> Value then
  begin
    FGlowGradientColor := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetGlowRadialColor(const Value: TColor);
begin
  if FGlowRadialColor <> Value then
  begin
    FGlowRadialColor := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetGradientMirrorType(const Value: TAdvGradientType);
begin
  if FGradientMirrorType <> value then
  begin
    FGradientMirrorType := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetGradientType(const Value: TAdvGradientType);
begin
  if FGradientType <> value then
  begin
    FGradientType := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetHatchStyle(const Value: THatchStyle);
begin
  if FHatchStyle <> value then
  begin
    FHatchStyle := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetHatchStyleMirror(const Value: THatchStyle);
begin
  if FHatchStyleMirror <> value then
  begin
    FHatchStyleMirror := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetOpacity(const Value: Byte);
begin
  if FOpacity <> value then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetOpacityMirror(const Value: Byte);
begin
  if FOpacityMirror <> value then
  begin
    FOpacityMirror := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetOpacityMirrorTo(const Value: Byte);
begin
  if FOpacityMirrorTo <> value then
  begin
    FOpacityMirrorTo := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetOpacityTo(const Value: Byte);
begin
  if FOpacityTo <> value then
  begin
    FOpacityTo := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetPicture(const Value: TAdvGDIPPicture);
begin
  if FPicture <> value then
  begin
    FPicture.Assign(value);
    PictureChanged(Self);
  end;
end;

procedure TGDIPFill.SetPictureAspectMode(const Value: TPictureMode);
begin
  if FPictureAspectMode <> Value then
  begin
    FPictureAspectMode := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetPictureAspectRatio(const Value: Boolean);
begin
  if FPictureAspectRatio <> Value then
  begin
    FPictureAspectRatio := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetPictureHeight(const Value: integer);
begin
  if FpictureHeight <> value then
  begin
    FpictureHeight := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetPictureLeft(const Value: integer);
begin
  if FPictureLeft <> value then
  begin
    FPictureLeft := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetPicturePosition(const Value: TFillPicturePosition);
begin
  if FPicturePosition <> value then
  begin
    FPicturePosition := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetPictureSize(const Value: TFillPictureSize);
begin
  if FpictureSize <> value then
  begin
    FpictureSize := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetPictureTop(const Value: integer);
begin
  if FPictureTop <> value then
  begin
    FPictureTop := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetPictureWidth(const Value: integer);
begin
  if FpictureWidth <> value then
  begin
    FpictureWidth := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetRounding(const Value: integer);
begin
  if FRounding <> value then
  begin
    FRounding := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetRoundingType(const Value: TFillRoundingType);
begin
  if FRoundingType <> value then
  begin
    FRoundingType := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> value then
  begin
    FShadowColor := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetShadowOffset(const Value: integer);
begin
  if FShadowOffset <> value then
  begin
    FShadowOffset := Value;
    Changed;
  end;
end;

procedure TGDIPFill.SetShadowType(const Value: TShadowType);
begin
  if (FShadowType <> Value) then
  begin
    FShadowType := Value;
    Changed;
  end;
end;

{ TGDIPProgress }

procedure TGDIPProgress.Assign(Source: TPersistent);
begin
  if (Source is TGDIPProgress) then
  begin
    FBackGroundFill.Assign((Source as TGDIPProgress).BackGroundFill);
    FProgressFill.Assign((Source as TGDIPProgress).ProgressFill);
    FValueFormat := (Source as TGDIPProgress).ValueFormat;
    FValueType := (Source as TGDIPProgress).ValueType;
    FValuePositionTop := (Source as TGDIPProgress).ValuePositionTop;
    FValuePositionLeft := (Source as TGDIPProgress).ValuePositionLeft;
    FValuePosition := (Source as TGDIPProgress).ValuePosition;
    FValueVisible := (Source as TGDIPProgress).ValueVisible;
    FFont.Assign((Source as TGDIPProgress).Font);
    Ftransparent := (Source as TGDIPProgress).Transparent;
    FShadows := (Source as TGDIPProgress).Shadows;
    FOverlays := (Source as TGDIPProgress).Overlays;
  end;
end;

{$IFDEF DIRECT2D}
function TGDIPProgress.CalculateProgressRectangle2D(r: TRect; min, max, pos: Double): TD2DRectF;
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

  Result := D2D1RectF(InsideRect(r).Left, InsideRect(r).Top, InsideRect(r).Left + xpos, InsideRect(r).Bottom);
end;
{$ENDIF}

function TGDIPProgress.CalculateProgressRectangle(r: TRect; min, max, pos: Double; Direction: TGDIPProgressDirection): TGPRectF;
var
  totalw, xpos: Double;
begin
  case Direction of
    pbdHorizontal:
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
    pbdVertical:
    begin
      if (ProgressFill.BorderColor <> clNone) and (ProgressFill.BorderWidth <> 0) then
        totalw := InsideRect(r).Bottom - ProgressFill.BorderWidth - InsideRect(r).Top
      else
        totalw := InsideRect(r).Bottom - InsideRect(r).Top;

      if (max - min) > 0 then
        xpos := Math.Min(((pos - min) / (max - min)) * totalw, totalw)
      else
        xpos := 0;

      if (ProgressFill.BorderColor <> clNone) and (ProgressFill.BorderWidth <> 0) then
        Result := MakeRect(InsideRect(r).Left + (ProgressFill.BorderWidth / 2),InsideRect(r).Bottom - (ProgressFill.BorderWidth / 2) - xpos,
          InsideRect(r).Right - ProgressFill.BorderWidth  - InsideRect(r).Left, xpos)
      else
        Result := MakeRect(InsideRect(r).Left, InsideRect(r).Bottom - xpos, InsideRect(r).Right - InsideRect(r).Left, xpos);
    end;
  end;
end;

procedure TGDIPProgress.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TGDIPProgress.Create;
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
  FValueVisible := false;
  FValuePosition := vpCenter;
  FProgressFont := TFont.Create;
  FProgressFont.OnChange := FontChanged;
  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  FProgressFont.Name := 'Tahoma';  
  {$ENDIF}
  FTransparent := false;
  FShadows := True;
  FOverlays := True;
end;

destructor TGDIPProgress.Destroy;
begin
  FBackGroundFill.Free;
  FProgressFill.Free;
  FFont.Free;
  FProgressFont.Free;
  inherited;
end;

{$IFDEF DIRECT2D}
procedure TGDIPProgress.Draw2D(g: TDirect2DCanvas; r: TRect; min, max, position: Double);
var
  bg: TD2DRectF;
begin
  r.Left := r.Left + 1;
  r.Top := r.Top + 1;

  if (BackGroundFill.BorderWidth = 0) or (BackGroundFill.BorderColor = clNone) then
    bg := D2D1RectF(r.Left - 1, r.Top - 1, r.Right + 1, r.Bottom + 1)
  else
    bg := D2D1RectF(r.Left, r.Top, r.Right - 1, r.Bottom - 1);

  if not Transparent then
    DrawBackGround2D(g, bg);

  if position > min then
    DrawProgress2D(g, r, min, max, position);

  if not Transparent then
    DrawShadows2D(g, GetInsideRectF2D(r));

  if ValueVisible then
    DrawValue2D(g, r, min, max, position);

  if not Transparent and Overlays then
    DrawOverLay2D(g, r);
end;
{$ENDIF}

procedure TGDIPProgress.Draw(g: TGPGraphics; r: TRect; min, max, position: Double; Direction: TGDIPProgressDirection);
var
  bg: TGPRectF;
begin
  if (BackGroundFill.BorderWidth = 0) or (BackGroundFill.BorderColor = clNone) then
    bg := MakeRect(r.Left - 1, r.Top - 1, r.Right - r.Left + 1, r.Bottom - r.Top + 1)
  else
    bg := MakeRect(r.Left, r.Top, r.Right - r.Left - 1, r.Bottom - r.Top - 1);

  if not Transparent then
    DrawBackGround(g, bg);

  if position > min then
    DrawProgress(g, r, min, max, position, Direction);

  if not Transparent then
    DrawShadows(g, GetInsideRectF(r), Direction);

  if ValueVisible then
    DrawValue(g, r, min, max, position, Direction);

  if not Transparent and Overlays then
    DrawOverLay(g, r, Direction);
end;

procedure TGDIPProgress.DrawBackGround(g: TGPGraphics; r: TGPRectF);
begin
  BackGroundFill.Fill(g, r);
end;

{$IFDEF DIRECT2D}
procedure TGDIPProgress.DrawBackGround2D(g: TDirect2DCanvas; r: TD2DRectF);
begin
  BackGroundFill.Fill2D(g, r);
end;
{$ENDIF}

procedure TGDIPProgress.DrawGlow(g: TGPGRaphics; r: TRect; glowposition, min, max, position: Double; Direction: TGDIPProgressDirection);
var
  b: TGPLinearGradientBrush;
  rr, lr, o: TGPRectF;
  rgn: TGPRegion;
begin
  rgn := TGPRegion.Create(CalculateProgressRectangle(r, min, max, position, Direction));
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

{$IFDEF DIRECT2D}
procedure TGDIPProgress.DrawGlow2D(g: TDirect2DCanvas; r: TRect;
  glowposition, min, max, position: Double);
begin

end;
{$ENDIF}

procedure TGDIPProgress.DrawOverLay(g: TGPGraphics; r: TRect; Direction: TGDIPProgressDirection);
var
  b: TGPLinearGradientBrush;
  ro: TGPRectF;
begin
  case Direction of
    pbdHorizontal:
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
    pbdVertical:
    begin
      ro := MakeRect(InsideRect(r).Left, InsideRect(r).Top, (InsideRect(r).Right - InsideRect(r).Left) / 2.5, InsideRect(r).Bottom - InsideRect(r).Top);
      b := TGPLinearGradientBrush.Create(MakeRect(ro.X - 1, ro.Y - 1, ro.Width + 2, ro.Height + 2), MakeColor(180, clWhite), MakeColor(0, clWhite), LinearGradientModeHorizontal);
      g.FillRectangle(b, ro);
      b.free;
      //
      ro := MakeRect(InsideRect(r).Right - (InsideRect(r).Right - InsideRect(r).Left) / 5, InsideRect(r).Top, (InsideRect(r).Right - InsideRect(r).Left) / 5, InsideRect(r).Bottom - InsideRect(r).Top);
      b := TGPLinearGradientBrush.Create(MakeRect(ro.X - 1, ro.Y - 1, ro.Width + 2, ro.Height + 2), MakeColor(0, clWhite), MakeColor(255, clWhite), LinearGradientModeHorizontal);
      g.FillRectangle(b, ro);
      b.free;
    end;
  end;
end;

{$IFDEF DIRECT2D}
procedure TGDIPProgress.DrawOverLay2D(g: TDirect2DCanvas; r: TRect);
var
  ro: TD2DRectF;
  linGrgBrush2D: ID2D1LinearGradientBrush;
  linGrgBrush2DProp: TD2D1LinearGradientBrushProperties;
  linGrgBrush2DStops: array[0 .. 1] of TD2D1GradientStop;
  linGrgBrush2DStopCol: ID2D1GradientStopCollection;
begin

  ro := D2D1RectF(InsideRect(r).Left, InsideRect(r).Top, InsideRect(r).Right, InsideRect(r).Top + (InsideRect(r).Bottom - InsideRect(r).Top) / 2.5);

  linGrgBrush2DProp.startPoint := D2D1PointF(ro.Left + (ro.right - ro.left) / 2, ro.Top);
  linGrgBrush2DProp.endPoint := D2D1PointF(ro.Left + (ro.right - ro.left) / 2, ro.Bottom);

  linGrgBrush2DStops[0].position := 0;
  linGrgBrush2DStops[1].position := 1;
  linGrgBrush2DStops[0].color := D2D1ColorF(clWhite, 180 / 255);
  linGrgBrush2DStops[1].color := D2D1ColorF(clWhite, 0);

  g.RenderTarget.CreateGradientStopCollection(@linGrgBrush2DStops, Length(linGrgBrush2DStops), D2D1_GAMMA_1_0, D2D1_EXTEND_MODE_CLAMP, linGrgBrush2DStopCol);
  g.RenderTarget.CreateLinearGradientBrush(linGrgBrush2DProp, nil, linGrgBrush2DStopCol, linGrgBrush2D);

  g.RenderTarget.FillRectangle(ro, linGrgBrush2D);

  ro := D2D1RectF(InsideRect(r).Left, InsideRect(r).Bottom - (InsideRect(r).Bottom - InsideRect(r).Top) / 5, InsideRect(r).Right,InsideRect(r).Bottom);

  linGrgBrush2DProp.startPoint := D2D1PointF(ro.Left + (ro.right - ro.left) / 2, ro.Top);
  linGrgBrush2DProp.endPoint := D2D1PointF(ro.Left + (ro.right - ro.left) / 2, ro.Bottom);

  linGrgBrush2DStops[0].position := 0;
  linGrgBrush2DStops[1].position := 1;
  linGrgBrush2DStops[0].color := D2D1ColorF(clWhite, 0);
  linGrgBrush2DStops[1].color := D2D1ColorF(clWhite, 180 / 255);

  g.RenderTarget.CreateGradientStopCollection(@linGrgBrush2DStops, Length(linGrgBrush2DStops), D2D1_GAMMA_1_0, D2D1_EXTEND_MODE_CLAMP, linGrgBrush2DStopCol);
  g.RenderTarget.CreateLinearGradientBrush(linGrgBrush2DProp, nil, linGrgBrush2DStopCol, linGrgBrush2D);

  g.RenderTarget.FillRectangle(ro, linGrgBrush2D);

end;
{$ENDIF}

procedure TGDIPProgress.DrawProgress(g: TGPGraphics; r: TRect; min, max, position: Double; Direction: TGDIPProgressDirection);
var
  pr: TGPRectF;
begin
  pr := CalculateProgressRectangle(r, min, max, position, Direction);
  ProgressFill.Fill(g, pr);
  case Direction of
    pbdHorizontal:
    begin
      if pr.Width > 0 then
        DrawShadows(g, pr, Direction);
    end;
    pbdVertical:
    begin
    if pr.Height > 0 then
      DrawShadows(g, pr, Direction);
    end;
  end;
end;

{$IFDEF DIRECT2D}
procedure TGDIPProgress.DrawProgress2D(g: TDirect2DCanvas; r: TRect; min,
  max, position: Double);
var
  pr2D: TD2DRectF;
begin
  pr2D := CalculateProgressRectangle2D(r, min, max, position);
  ProgressFill.Fill2D(g, pr2D);
  if pr2D.right - pr2D.left > 0 then
    DrawShadows2D(g, pr2D);
end;
{$ENDIF}

procedure TGDIPProgress.DrawShadows(g: TGPGraphics; r: TGPRectF; Direction: TGDIPProgressDirection);
var
  o, rRight, rLeft: TGPRectF;
  b: TGPLinearGradientBrush;
begin
  if Shadows then
  begin
    o := r;

    case Direction of
      pbdHorizontal:
      begin
        rLeft := MakeRect(o.X, o.y, 10, o.Height);
        rRight := MakeRect(o.Width - 10 + o.X, o.y, 10, o.Height);
        b := TGPLinearGradientBrush.Create(MakeRect(rLeft.X - 1, rLeft.Y - 1, rLeft.Width + 2, rLeft.Height + 2), MakeColor(30, clBlack), MakeColor(0, clBlack), LinearGradientModeHorizontal);
        g.FillRectangle(b, rLeft);
        b.free;

        b := TGPLinearGradientBrush.Create(MakeRect(rRight.X - 1, rRight.Y - 1, rRight.Width + 2, rRight.Height + 2), MakeColor(0, clBlack), MakeColor(30, clBlack), LinearGradientModeHorizontal);
        g.FillRectangle(b, rRight);
        b.Free;
      end;
      pbdVertical:
      begin
        rLeft := MakeRect(o.X, o.y, o.Width, 10);
        rRight := MakeRect(o.x, o.Height - 10 + o.y, o.Width, 10);
        b := TGPLinearGradientBrush.Create(MakeRect(rLeft.X - 1, rLeft.Y - 1, rLeft.Width + 2, rLeft.Height + 2), MakeColor(30, clBlack), MakeColor(0, clBlack), LinearGradientModeVertical);
        g.FillRectangle(b, rLeft);
        b.free;

        b := TGPLinearGradientBrush.Create(MakeRect(rRight.X - 1, rRight.Y - 1, rRight.Width + 2, rRight.Height + 2), MakeColor(0, clBlack), MakeColor(30, clBlack), LinearGradientModeVertical);
        g.FillRectangle(b, rRight);
        b.Free;
      end;
    end;
  end;
end;

{$IFDEF DIRECT2D}
procedure TGDIPProgress.DrawShadows2D(g: TDirect2DCanvas; r: TD2DRectF);
var
  o, rRight, rLeft: TD2DRectF;
  linGrgBrush2D: ID2D1LinearGradientBrush;
  linGrgBrush2DProp: TD2D1LinearGradientBrushProperties;
  linGrgBrush2DStops: array[0 .. 1] of TD2D1GradientStop;
  linGrgBrush2DStopCol: ID2D1GradientStopCollection;
begin
  if Shadows then
  begin
    o := r;
    rLeft := D2D1RectF(o.left, o.top, o.left + 10, o.Bottom);
    rRight := D2D1RectF(o.Right - 10, o.top, o.right, o.Bottom);

    linGrgBrush2DProp.startPoint := D2D1PointF(rLeft.Left, rLeft.Top + (rLeft.Bottom - rLeft.Top) / 2);
    linGrgBrush2DProp.endPoint := D2D1PointF(rLeft.Right, rLeft.Top + (rLeft.Bottom - rLeft.Top) / 2);

    linGrgBrush2DStops[0].position := 0;
    linGrgBrush2DStops[1].position := 1;
    linGrgBrush2DStops[0].color := D2D1ColorF(clBlack, 30 / 255);
    linGrgBrush2DStops[1].color := D2D1ColorF(clBlack, 0);

    g.RenderTarget.CreateGradientStopCollection(@linGrgBrush2DStops, Length(linGrgBrush2DStops), D2D1_GAMMA_1_0, D2D1_EXTEND_MODE_CLAMP, linGrgBrush2DStopCol);

    g.RenderTarget.CreateLinearGradientBrush(linGrgBrush2DProp, nil, linGrgBrush2DStopCol, linGrgBrush2D);

    g.RenderTarget.FillRectangle(rLeft, linGrgBrush2D);

    linGrgBrush2DProp.startPoint := D2D1PointF(rRight.Left, rRight.Top + (rRight.Bottom - rRight.Top) / 2);
    linGrgBrush2DProp.endPoint := D2D1PointF(rRight.Right, rRight.Top + (rRight.Bottom - rRight.Top) / 2);

    linGrgBrush2DStops[0].position := 0;
    linGrgBrush2DStops[1].position := 1;
    linGrgBrush2DStops[0].color := D2D1ColorF(clBlack, 0);
    linGrgBrush2DStops[1].color := D2D1ColorF(clBlack, 30 / 255);

    g.RenderTarget.CreateGradientStopCollection(@linGrgBrush2DStops, Length(linGrgBrush2DStops), D2D1_GAMMA_1_0, D2D1_EXTEND_MODE_CLAMP, linGrgBrush2DStopCol);
    g.RenderTarget.CreateLinearGradientBrush(linGrgBrush2DProp, nil, linGrgBrush2DStopCol, linGrgBrush2D);

    g.RenderTarget.FillRectangle(rRight, linGrgBrush2D);
  end;
end;
{$ENDIF}

procedure TGDIPProgress.DrawValue(g: TGPGraphics; r: TRect; min, max, position: double; Direction: TGDIPProgressDirection);
var
  f: TGPFont;
  sf: TGPStringFormat;
  b: TGPSolidBrush;
  bn, sizerect, pr: TGPRectF;
  s: String;
  tw, th: Double;
  p: TGPPointF;
  x, y, v: Double;
  ft: TFont;
  vf: String;
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

  vf := ValueFormat;
  s := Format(vf, [v]);

  if Assigned(OnDrawValue) then
    OnDrawValue(Self, vf, s);

  GetTextSize(g, r, s, Font, sizerect);
  tw := sizerect.Width;
  th := sizerect.Height;
  bn := GetInsideRectF(r);

  pr := CalculateProgressRectangle(r, min, max, position, Direction);
  bn := GetInsideRectF(r);

  ft := nil;
  x := 0;
  y := 0;

  case Direction of
    pbdHorizontal:
    begin
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
    end;
    pbdVertical:
    begin
      x := (bn.Width - tw) / 2;
      y := r.Top;

      case ValuePosition of
        vpProgressCenter:
        begin
          if pr.Height >= th then
            y := bn.Y + (pr.Height - th) / 2
          else
            y := bn.Y;
        end;
        vpProgressRight:
        begin
          if pr.Height >= th then
            y := bn.Y + (pr.Height - th)
          else
            y := bn.Y;
        end;
        vpCenter: y := y + (bn.Height - th) / 2;
        vpLeft: y := bn.y;
        vpRight: y := y + bn.Height - th;
        vpCustom:
        begin
          y := y + ValuePositionTop;
          x := ValuePositionLeft;
        end;
      end;

      if (y + (th / 2) < pr.Y + pr.Height) then
        ft := ProgressFont
      else
        ft := Font;
    end;
  end;

  sf := TGPStringFormat.Create;
  f := g.MakeFont(ft);

  case Direction of
    pbdHorizontal:  p := MakePoint(x, y + r.Top);
    pbdVertical:  p := MakePoint(x + r.Left, y);
  end;

  b := TGPSolidBrush.Create(MakeColor(255, ft.Color));

  g.DrawString(s, Length(s), f, p, sf, b);

  b.Free;
  sf.Free;
  f.Free;
end;

{$IFDEF DIRECT2D}
procedure TGDIPProgress.DrawValue2D(g: TDirect2DCanvas; r: TRect; min, max,
  position: double);
var
  bn, sizerect, pr: TD2DRectF;
  s: String;
  tw, th: Double;
  x, y, v: Double;
  ft: TFont;
  vf: String;
  fmt: IDWriteTextFormat;
  solGrgBrush2D: ID2D1SolidColorBrush;
  p: TD2DPoint2f;
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

  vf := ValueFormat;
  s := Format(vf, [v]);

  if Assigned(OnDrawValue) then
    OnDrawValue(Self, vf, s);

  GetTextSize2D(g, r, s, Font, sizerect);
  tw := sizerect.right - sizerect.left;
  th := sizerect.bottom - sizerect.top;

  pr := CalculateProgressRectangle2D(r, min, max, position);
  bn := GetInsideRectF2D(r);
  y := (bn.Bottom - bn.Top - th) / 2;
  x := r.Left;

  case ValuePosition of
    vpProgressCenter:
    begin
      if pr.right - pr.left >= tw then
        x := bn.left + (pr.right - pr.left - tw) / 2
      else
        x := bn.left;
    end;
    vpProgressRight:
    begin
      if pr.right - pr.left >= tw then
        x := bn.left + (pr.right - pr.left - tw)
      else
        x := bn.left;
    end;
    vpCenter: x := x + (bn.right - bn.left - tw) / 2;
    vpLeft: x := bn.left;
    vpRight: x := x + bn.right - bn.left - tw;
    vpCustom:
    begin
      x := x + ValuePositionLeft;
      y := ValuePositionTop;
    end;
  end;

  if (x + (tw / 2) < pr.right) then
    ft := ProgressFont
  else
    ft := Font;

  fmt := Create2DTextFormat(ft, taLeftJustify, taAlignTop, False, False);
  g.RenderTarget.CreateSolidColorBrush(D2D1ColorF(ft.Color, 1), nil, solGrgBrush2D);
  p := D2D1PointF(x, y);
  sizeRect.left := p.X;
  sizeRect.right := sizerect.right + p.x;
  sizeRect.top := p.Y;
  sizeRect.bottom := sizerect.Bottom + p.y;
  g.RenderTarget.DrawText(PChar(s), Length(s), fmt, sizeRect, solGrgBrush2D);
end;
{$ENDIF}

procedure TGDIPProgress.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TGDIPProgress.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TGDIPProgress.GetInsideRectF(r: TRect): TGPRectF;
begin
  Result := MakeRect(InsideRect(r).Left, InsideRect(r).Top, InsideRect(r).Right - InsideRect(r).Left,
    InsideRect(r).Bottom - InsideRect(r).Top);
end;

{$IFDEF DIRECT2D}
function TGDIPProgress.GetInsideRectF2D(r: TRect): TD2D1RectF;
begin
  Result := D2D1RectF(InsideRect(r).Left, InsideRect(r).Top, InsideRect(r).Right,
    InsideRect(r).Bottom);
end;
{$ENDIF}

procedure TGDIPProgress.GetTextSize(g: TGPGraphics; r: TRect; s: String; ft: TFont; var sizer: TGPRectF);
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

{$IFDEF DIRECT2D}
procedure TGDIPProgress.GetTextSize2D(g: TDirect2DCanvas; r: TRect; s: String; ft: TFont; var sizer: TD2DRectF);
begin
  g.Font.Assign(ft);
  sizer := D2D1RectF(0, 0, g.TextWidth(s), g.TextHeight(s));
end;
{$ENDIF}

function TGDIPProgress.InsideRect(r: TRect): TRect;
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

procedure TGDIPProgress.SetBackGroundFill(const Value: TGDIPFill);
begin
  if FBackGroundFill <> value then
  begin
    FBackGroundFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TGDIPProgress.SetFont(const Value: TFont);
begin
  if FFont <> value then
  begin
    FFont.Assign(Value);
    FontChanged(Self);
  end;
end;

procedure TGDIPProgress.SetOverlays(const Value: Boolean);
begin
  if FOverlays <> Value then
  begin
    FOverlays := Value;
    Changed;
  end;
end;

procedure TGDIPProgress.SetProgressFill(const Value: TGDIPFill);
begin
  if FProgressFill <> value then
  begin
    FProgressFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TGDIPProgress.SetProgressFont(const Value: TFont);
begin
  if FProgressFont <> value then
  begin
    FProgressFont.Assign(Value);
    FontChanged(Self);
  end;
end;

procedure TGDIPProgress.SetShadows(const Value: Boolean);
begin
  if FShadows <> Value then
  begin
    FShadows := Value;
    Changed;
  end;
end;

procedure TGDIPProgress.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

procedure TGDIPProgress.SetValueFormat(const Value: String);
begin
  if FValueFormat <> value then
  begin
    FValueFormat := Value;
    Changed;
  end;
end;

procedure TGDIPProgress.SetValuePosition(
  const Value: TGDIPProgressValuePosition);
begin
  if FValuePosition <> Value then
  begin
    FValuePosition := Value;
    Changed;
  end;
end;

procedure TGDIPProgress.SetValuePositionLeft(const Value: integer);
begin
  if FValuePositionLeft <> value then
  begin
    FValuePositionLeft := Value;
    Changed;
  end;
end;

procedure TGDIPProgress.SetValuePositionTop(const Value: integer);
begin
  if FValuePositionTop <> value then
  begin
    FValuePositionTop := Value;
    Changed;
  end;
end;

procedure TGDIPProgress.SetValueType(const Value: TGDIPProgressValueType);
begin
  if FValueType <> Value then
  begin
    FValueType := Value;
    Changed;
  end;
end;

procedure TGDIPProgress.SetValueVisible(const Value: Boolean);
begin
  if FValueVisible <> value then
  begin
    FValueVisible := Value;
    Changed;
  end;
end;

{ TGDIPStatus }

procedure TGDIPStatus.Assign(Source: TPersistent);
begin
  if (Source is TGDIPStatus) then
  begin
    FFill.Assign((Source as TGDIPStatus).Fill);
    FFont.Assign((Source as TGDIPStatus).Font);
    FGlow := (Source as TGDIPStatus).Glow;
    FSpacing := (Source as TGDIPStatus).Spacing;
    Changed;
  end;
end;

procedure TGDIPStatus.CalculateSize(g: TGPGraphics; Caption: String);
var
  ff: TGPFontFamily;
  f: TGPFont;
  fs: integer;
  sf: TGPStringFormat;
  SizeRect: TGPRectF;
  pw, ph: integer;
begin
  if Caption <> '' then
  begin
    ff := TGPFontFamily.Create(Font.Name);

    fs := 0;
    if (fsBold in FFont.Style) then
      fs := fs + 1;
    if (fsItalic in FFont.Style) then
      fs := fs + 2;
    if (fsUnderline in FFont.Style) then
      fs := fs + 4;

    sf := TGPStringFormat.Create;
    f := TGPFont.Create(ff, FFont.Size, fs, UnitPoint);

    g.MeasureString(Caption, Length(Caption), f, MakeRect(0, 0, 10000, 10000), sf, sizeRect);

    FHeight := Round(SizeRect.Height + 4);
    FWidth := Max(Round(SizeRect.Width + 4), FHeight) + (Spacing * 2);

    if IsNaN(SizeRect.Height) then
      FHeight := 4
    else
      FHeight := Round(SizeRect.Height + 4);

    if IsNaN(SizeRect.Width) then
      FWidth := Max(4, FHeight) + (Spacing * 2)
    else
      FWidth := Max(Round(SizeRect.Width + 4), FHeight) + (Spacing * 2);

    ff.Free;
    f.free;
    sf.Free;
  end;

  if not Self.Fill.Picture.Empty then
  begin
    if Self.Fill.PictureSize = psOriginal then
    begin
      Self.Fill.Picture.GetImageSizes;
      pw := Fill.Picture.Width;
      ph := Fill.Picture.Height;
    end
    else
    begin
      pw := Fill.PictureWidth;
      ph := Fill.PictureHeight;
    end;

    if pw > FWidth then
      FWidth := pw;

    if ph > FHeight then
      FHeight := ph;
  end;
end;

procedure TGDIPStatus.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TGDIPStatus.Create;
begin
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  {$ENDIF}
  FAutoSize := true;
  FGlow := true;
  FSpacing := 0;
end;

destructor TGDIPStatus.Destroy;
begin
  FFill.Free;
  FFont.Free;
  inherited;
end;

procedure TGDIPStatus.Draw(g: TGPGraphics; x, y, Width, Height: integer; AutoSize: boolean; Caption: String);
var
  f: TGPFont;
  ff: TGPFontFamily;
  fs: integer;
  bw: Double;
  sf: TGPStringFormat;
  r, sizeRect: TGPRectF;
  p: TGPPointF;
  b: TGPSolidBrush;
  o: TGDIPFill;
  w, h: integer;
begin
  ff := TGPFontFamily.Create(FFont.Name);

  if AutoSize then
  begin
    w := FWidth;
    h := FHeight;
  end
  else
  begin
    w := Width;
    h := Height;
  end;

  w := w;
  h := h;

  fs := 0;
  if (fsBold in FFont.Style) then
    fs := fs + 1;
  if (fsItalic in FFont.Style) then
    fs := fs + 2;
  if (fsUnderline in FFont.Style) then
    fs := fs + 4;

  sf := TGPStringFormat.Create;
  f := TGPFont.Create(ff, FFont.Size, fs, UnitPoint);

  g.MeasureString(Caption, Length(Caption), f, MakeRect(0, 0, 10000, 10000), sf, sizeRect);

  Fill.BeginUpdate;
  bw := 0;
  if (Fill.BorderColor <> clNone) then
    bw := Fill.BorderWidth / 2;

  r := MakeRect(x, y, w - (bw * 2), h - (bw * 2));
  Fill.Rounding := Round(r.Height / 2);
  Fill.Fill(g, r);
  Fill.EndUpdate;

  p := MakePoint((w - (Spacing * 2) - sizeRect.Width) / 2, (h - sizeRect.Height) / 2);
  p.x := p.x + x + Spacing;
  p.y := p.y + y;
  b := TGPSolidBrush.Create(MakeColor(255, FFont.Color));
  g.DrawString(Caption, Length(Caption),f , p, sf, b);

  if Glow then
  begin
    o := TGDIPFill.Create;
    o.Color := clWhite;
    o.ColorTo := clWhite;
    o.GradientType := gtVertical;
    o.Opacity := 170;
    o.OpacityTo := 30;
    o.RoundingType := rtTop;
    o.Rounding := Fill.Rounding;
    r := MakeRect(r.X, r.y, r.Width, r.Height / 2);
    o.Fill(g, r);

    o.Free;
  end;

  b.Free;
  f.Free;
  ff.Free;
  sf.Free;
end;

procedure TGDIPStatus.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TGDIPStatus.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TGDIPStatus.GetHeight: integer;
begin
  Result := FHeight;
end;

function TGDIPStatus.GetWidth: integer;
begin
  Result := FWidth;
end;

procedure TGDIPStatus.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill.Assign(Value);
    FillChanged(self);
  end;
end;

procedure TGDIPStatus.SetFont(const Value: TFont);
begin
  if FFont <> value then
  begin
    FFont.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPStatus.SetGlow(const Value: Boolean);
begin
  if FGlow <> value then
  begin
    FGlow := Value;
    changed;
  end;
end;

procedure TGDIPStatus.SetSpacing(const Value: integer);
begin
  if FSpacing <> value then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

{ TGDIPButton }

{$IFNDEF DELPHI6_LVL}
const
  Nan = -361.0;

function IsNan(d: double): boolean;
begin
  Result := (d = Nan);
end;
{$ENDIF}

type
  EColorError = class(Exception);

  THSVTriplet = record
    H,S,V: double;
  end;

  TRGBTriplet = record
    R,G,B: double;
  end;

procedure RGBToHSV (const R,G,B: Double; var H,S,V: Double);
var
  Delta: double;
  Min : double;
begin
  Min := MinValue( [R, G, B] );    
  V := MaxValue( [R, G, B] );

  Delta := V - Min;

  // Calculate saturation: saturation is 0 if r, g and b are all 0
  if V = 0.0 then
    S := 0
  else
    S := Delta / V;

  if (S = 0.0) then
    H := NaN    // Achromatic: When s = 0, h is undefined
  else
  begin       // Chromatic
    if (R = V) then
    // between yellow and magenta [degrees]
      H := 60.0 * (G - B) / Delta
    else
      if (G = V) then
       // between cyan and yellow
        H := 120.0 + 60.0 * (B - R) / Delta
      else
        if (B = V) then
        // between magenta and cyan
          H := 240.0 + 60.0 * (R - G) / Delta;

    if (H < 0.0) then
      H := H + 360.0
  end;
end; {RGBtoHSV}


procedure HSVtoRGB (const H,S,V: double; var R,G,B: double);
var
  f : double;
  i : INTEGER;
  hTemp: double; // since H is CONST parameter
  p,q,t: double;
begin
  if (S = 0.0) then    // color is on black-and-white center line
  begin
    if IsNaN(H) then
    begin
      R := V;           // achromatic: shades of gray
      G := V;
      B := V
    end
    else
      raise EColorError.Create('HSVtoRGB: S = 0 and H has a value');
  end
  else
  begin // chromatic color
    if (H = 360.0) then         // 360 degrees same as 0 degrees
      hTemp := 0.0
    else
      hTemp := H;

    hTemp := hTemp / 60;     // h is now IN [0,6)
    i := TRUNC(hTemp);        // largest integer <= h
    f := hTemp - i;                  // fractional part of h

    p := V * (1.0 - S);
    q := V * (1.0 - (S * f));
    t := V * (1.0 - (S * (1.0 - f)));

    case i of
      0: begin R := V; G := t;  B := p  end;
      1: begin R := q; G := V; B := p  end;
      2: begin R := p; G := V; B := t   end;
      3: begin R := p; G := q; B := V  end;
      4: begin R := t;  G := p; B := V  end;
      5: begin R := V; G := p; B := q  end;
    end;
  end;
end; {HSVtoRGB}


function CreateBubbleRectangleConcave(R: TRect; Radius, Delta: Integer): TGPGraphicsPath;
var
  l, t, w, h, d: Integer;
  rs: single;
  alpha1, alpha2: single;
  mid: tpoint;
  ar: tgprectf;
  ri: integer;
begin
  Result := TGPGraphicsPath.Create;
  
  l := R.Left;
  t := R.Top;
  w := R.Right - R.Left;
  h := R.Bottom - R.Top;
  d := Radius shl 1;

  if delta = 0 then
    Exit;

  rs := (sqr(w/2) + sqr(Delta)) / (2 * Delta);

  ri := round(rs);

  if (rs - Delta = 0) then
  begin
    alpha1 := 0;
    alpha2 := 180;
  end
  else
  begin
    alpha1 := 90 + RadToDeg(ArcTan( 1/2 * w / (rs - Delta)));
    alpha2 := 180 - alpha1;
  end;

  mid.x := l + w div 2;
  mid.y := t + h + Delta - round(rs);

  ar := MakeRect(mid.x - ri, mid.Y - ri - delta, ri * 2, ri * 2);

  Result.AddArc(ar, alpha1, alpha2 - alpha1);
  Result.AddLine(l + w, t + h - delta, l + w, t + 2 * h - radius); // right
  Result.AddArc(l + w - d,  t + 2 * h - d, d, d, 0, 90); // topleft
  Result.AddLine(l + w - radius, t + 2 * h, l + radius, t + 2 * h ); // right
  Result.AddArc(l,  t + 2 * h - d, d, d, 90, 90); // topleft
  Result.AddLine(l , t + 2 * h - radius, l , t + h - delta ); // right
  Result.CloseFigure();
end;

function CreateBubbleRectangleConvex(R: TRect; Radius, Delta: Integer): TGPGraphicsPath;
var
  l, t, w, h, d: Integer;
  rs: single;
  alpha1, alpha2: single;
  mid: tpoint;
  ar: tgprectf;
  ri: integer;
begin
  Result := TGPGraphicsPath.Create;
  l := R.Left;
  t := R.Top;
  w := R.Right - R.Left;
  h := R.Bottom - R.Top;
  d := Radius shl 1;

  if delta = 0 then
    Exit;

  rs := (sqr(w/2) + sqr(Delta)) / (2 * Delta);

  ri := round(rs);

  if (rs - Delta = 0) then
  begin
    alpha1 := 0;
    alpha2 := 180;
  end
  else
  begin
    alpha1 := 90 + RadToDeg(ArcTan( 1/2 * w / (rs - Delta)));
    alpha2 := 180 - alpha1;
  end;

  mid.x := l + w div 2;
  mid.y := t + h + Delta - round(rs);

  ar := MakeRect(mid.x - ri, mid.Y - ri - delta, ri*2, ri*2);

  Result.AddArc(l, t, d, d, 180, 90); // topleft
  Result.AddLine(l + radius, t, l + w - radius, t); // top
  Result.AddArc(l + w - d, t, d, d, 270, 90); // topright
  Result.AddLine(l + w, t + radius, l + w, t + h - delta); // right


  Result.AddArc(ar, alpha1, alpha2 - alpha1);
  Result.AddLine(l, t + h - delta, l, t + radius); // right

  Result.CloseFigure();
end;

function CreateRoundRectangle(R: TRect; Radius: Integer; BottomRounding: Boolean): TGPGraphicsPath; overload;
var
  l, t, w, h, d: Integer;
begin
  Result := TGPGraphicsPath.Create;
  l := R.Left;
  t := R.Top;
  w := R.Right - R.Left;
  h := R.Bottom - R.Top;
  d := Radius shl 1;

  if not BottomRounding then
  begin
    Result.AddArc(l, t + h div 2, d, d, 180, 90); // topleft
    Result.AddLine(l + radius, t +  h div 2, l + w - radius, t + h div 2); // top
    Result.AddArc(l + w - d, t + h div 2, d, d, 270, 90); // topright
    Result.AddLine(l + w, t + radius + h div 2, l + w, t + h - radius); // right
    Result.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
    Result.AddLine(l + w - radius, t + h, l + radius, t + h); // bottom
    Result.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
    Result.AddLine(l, t + h - radius, l, t + radius +  h div 2); // left
  end
  else
  begin
    Result.AddArc(l, t + h div 2, d, d, 180, 90); // topleft
    Result.AddLine(l + radius, t +  h div 2, l + w - radius, t + h div 2); // top
    Result.AddArc(l + w - d, t + h div 2, d, d, 270, 90); // topright
    Result.AddLine(l + w, t + radius + h div 2, l + w, t + h); // right
//    Result.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
    Result.AddLine(l + w, t + h, l, t + h); // bottom
//    Result.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
    Result.AddLine(l, t + h, l, t + h div 2); // left
  end;

  Result.CloseFigure();
end;

function CreateRoundLine(R: TRect; Radius: Integer; RoundingType: TFillRoundingType; bw: integer): TGPGraphicsPath;
var
  l, t, w, h, d: Integer;
begin
  Result := TGPGraphicsPath.Create;
  l := R.Left;
  t := R.Top;
  w := R.Right - R.Left;
  h := R.Bottom - R.Top;
  d := Radius shl 1;

  GetPath(l, t, w, h, d, Radius, RoundingType, result);

  InflateRect(r, -bw, -bw);

  l := R.Left;
  t := R.Top;
  w := R.Right - R.Left;
  h := R.Bottom - R.Top;

  GetPath(l, t, w, h, d, Radius, RoundingType, result);
end;

function ChangeColor(Color: TColor; Delta: integer): TColor;
var
  r,g,b: longint;
  l: longint;
begin
  l := ColorToRGB(Color);
  b := ((l AND $FF0000) shr 16);
  g := ((l AND $FF00) shr 8);
  r := (l AND $FF);

  r := Max(0,Min(255,r + Delta));
  g := Max(0,Min(255,g + Delta));
  b := Max(0,Min(255,b + Delta));

//  r := round(Min(255,r * delta /100));
//  g := round(Min(255,g * delta /100));
//  b := round(Min(255,b * delta /100));

  Result := RGB(r,g,b);
end;

procedure TGDIPButton.Assign(Source: TPersistent);
begin
  if (Source is TGDIPButton) then
  begin
    FPictureStretch := (Source as TGDIPButton).PictureStretch;
    FFont.Assign((Source as TGDIPButton).Font);
    FLayout := (Source as TGDIPButton).Layout;
    FSpacing := (Source as TGDIPButton).Spacing;
    FShiftDown := (Source as TGDIPButton).ShiftDown;
    FAlignment := (Source as TGDIPButton).Alignment;
    FPictureAlignment := (Source as TGDIPButton).PictureAlignment;
    FFocusColor := (Source as TGDIPButton).FocusColor;
    FSimpleLayout := (Source as TGDIPButton).SimpleLayout;
    FSimpleLayoutBorder := (Source as TGDIPButton).SimpleLayoutBorder;
    FPictureStretchMode := (Source as TGDIPButton).PictureStretchMode;
    FGlowPercentage := (Source as TGDIPButton).GlowPercentage;
    FWordWrapping := (Source as TGDIPButton).WordWrapping;
    Changed;
  end;
end;

procedure TGDIPButton.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TGDIPButton.Changed;
begin
  if Assigned(FOnChange) and (FUpdateCount = 0) then
    FOnChange(Self);
end;

constructor TGDIPButton.Create;
begin
  FGlowPercentage := 100;
  FImageIndex := -1;
  FSimpleLayoutBorder := False;
  FWordWrapping := True;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  {$ENDIF}
  FLayout := blPictureLeft;
  FSpacing := 3;
  FShiftDown := 1;
  FAlignment := taCenter;
  FPictureStretch := False;
  FRounding := 8;
  FPictureStretchMode := pmNormal;
  FPictureAlignment := taRightJustify;
  FFocusColor := clBlack;
  FSimpleLayout := False;
  FDisabledImageIndex := -1;
end;

destructor TGDIPButton.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TGDIPButton.Draw(g: TGPGraphics; Caption: String; x, y, Width, Height, VerticalSpacing,
  HorizontalSpacing: integer; Color, ColorDown, BevelColor, TextColor: TColor; Shadow, Down, Bevel, VerticalText, RightToLeft: Boolean;
  RoundingType: TFillRoundingType; APicture: TAdvGDIPPicture; TextW, TextH: integer; WW: boolean; AImageIndex: Integer; APictureName: String);
var
  p: TGPGraphicsPath;
  b: TGPLinearGradientBrush;
  bs: TGPSolidBrush;
  delta: integer;
  ff: TGPFontFamily;
  fs: integer;
  f: TGPFont;
  stringformat: TGPStringFormat;
  l, t: integer;
  DrawColor: TColor;
  colors : array[0..3] of TGPColor;
  positions: array[0..3] of single;
  ph: integer;
  fHSV: THSVTriplet;
  fRGB: TRGBTriplet;
  vs, hs: integer;
  sizer, textr: TGPRectF;
  pt: TGPPointF;
  tw, th: Double;
  ly: TGDIPButtonLayout;
  pen: TGPPen;
  pathshadow, path: TGPGraphicsPath;
  gBrush: TGPPathGradientBrush;
  cb: array[0..2] of TGPColor;
  pb: array[0..2] of single;
  fl: StringAlignment;
  pic: TAdvGDIPPicture;
  bmp: TBitmap;
  picw, pich: Integer;
  wp, hp: Double;
  ca: TCanvas;
  sz: Single;
  h: HWND;
begin
  pic := TAdvGDIPPicture.Create;

  pic.Assign(APicture);

  if Assigned(PictureContainer) then
    if APictureName <> '' then
      pic.Assign(PictureContainer.FindPicture(APictureName));


  if Assigned(ImageList) then
  begin
    if (AImageIndex >= 0) and (AImageIndex <= ImageList.Count - 1) then
    begin
      bmp := TBitmap.Create;
      try
        ImageList.GetBitmap(AImageIndex, bmp);
        if not bmp.Empty then
          pic.Assign(bmp);
      finally
        bmp.Free;
      end;
    end;
  end;

  if (pic <> nil) and (pic.Empty = false) then
  begin
    Pic.GetImageSizes;
    picw := pic.Width;
    pich := pic.Height;

    if PictureStretch then
    begin
      if (Caption = '') and (Layout <> blPictureTop) and (Layout <> blPictureBottom) then
        GetAspectSize(wp, hp, picw, pich, Height - 7, Height - 7, PictureStretchMode)
      else
      begin
        ca := TCanvas.Create;
        h := g.GetHDC;
        ca.Handle := h;
        ca.Font.Assign(Font);
        sz := ca.TextHeight(Caption);
        ca.Free;
        g.ReleaseHDC(h);
        GetAspectSize(wp, hp, picw, pich, Height - sz, Height - sz, PictureStretchMode)
      end;
      picw := Round(wp);
      pich := Round(hp);
    end;
  end
  else
  begin
    picw := 0;
    pich := 0;
  end;

  ly := Layout;
  if VerticalText then
  begin
    if not RightToLeft then
    begin
      case Layout of
        blPictureLeft: ly := blPictureTop;
        blPictureRight: ly := blPictureBottom;
        blPictureBottom: ly := blPictureRight;
        blPictureTop: ly := blPictureLeft;
      end;
    end
    else
    begin
      case Layout of
        blPictureLeft: ly := blPictureBottom;
        blPictureRight: ly := blPictureTop;
        blPictureBottom: ly := blPictureRight;
        blPictureTop: ly := blPictureLeft;
      end;
    end;
  end;

  if Down and (ColorDown = clNone) then
  begin
    fRGB.R := GetRed(Color);
    fRGB.G := GetGreen(Color);
    fRGB.B := GetBlue(Color);
    RGBToHSV(fRGB.B, fRGB.G, fRGB.R, fHSV.H, fHSV.S, fHSV.V);
    fHSV.V := 0.7 * fHSV.V;
    HSVToRGB(fHSV.H, fHSV.S, fHSV.V, fRGB.R, fRGB.G, fRGB.B);
    DrawColor := RGB(Round(fRGB.R), Round(fRGB.G), Round(fRGB.B));
  end
  else if Down then
    DrawColor := ColorDown
  else
    DrawColor := Color;

  hs := HorizontalSpacing;
  vs := VerticalSpacing;

  if Shadow then
  begin
    {
    bs := TGPSolidBrush.Create(ColorToARGB(clBlack));
    p := CreateRoundRectangle(MakeRect(2 + hs + x,2 + vs + y, Width - 3 - hs, Height - 1 - vs), 4, RoundingType, false);
    ph := Height - 2;
    g.FillPath(bs, p);
    bs.Free;
    p.Free;
    }
    pathshadow := CreateRoundRectangle(MakeRect(2 + hs + x,2 + vs + y, Width - 4 - (hs * 2), Height - 2 - (vs * 2)), Rounding, RoundingType, false);
    gBrush := TGPPathGradientBrush.Create(pathshadow);
    gBrush.SetWrapMode(WrapModeClamp);

    cb[0] := MakeColor(0,0,0,0);
    cb[1] := MakeColor(180, clBlack);
    cb[2] := MakeColor(180, clBlack);

    pb[0] := 0;
    pb[1] := 0.1;
    pb[2] := 1;

    gBrush.SetInterpolationColors(@cb, @pb, 3);

    g.FillPath(gbrush, pathshadow);
    pathshadow.Free;
    gBrush.Free;

    Height := Height - 4;
    Width := Width - 6;
  end;

  ph := Height;

  if not SimpleLayout then
  begin
    delta := ph div 6;
    bs := TGPSolidBrush.Create(ColorToARGB(ChangeColor(DrawColor,-40)));
    p := TGPGraphicsPath.Create;
    p.AddRectangle(MakeRect(hs + x, ph div 2 - delta + y, Width - 1 - (hs * 2), delta * 2));
    g.FillPath(bs, p);
    bs.Free;
    p.Free;

    p := CreateBubbleRectangleConcave(Rect(hs + x,vs + y, x + Width - 1 - hs, y + (ph div 2)), Rounding, delta);

    b := TGPLinearGradientBrush.Create(MakeRect(hs - 1 + x,(ph div 2) - delta - 1 + y, Width - (hs * 2) + 1, (ph div 2) + delta + 2), ColorToARGB(ChangeColor(DrawColor,-40)),
      ColorToARGB(ChangeColor(DrawColor,180)),LinearGradientModeVertical);

    b.SetGammaCorrection(true);

    colors[0] := ColorToARGB(ChangeColor(DrawColor,-40 * GlowPercentage div 100));
    colors[1] := ColorToARGB(ChangeColor(DrawColor,100 * GlowPercentage div 100));
    colors[2] := ColorToARGB(ChangeColor(DrawColor,220 * GlowPercentage div 100));

    positions[0] := 0;
    positions[1] := 0.9;
    positions[2] := 1;

    b.SetInterpolationColors(@colors,@positions,3);

    g.FillPath(b,p);

    b.Free;
    p.Free;

    p := CreateBubbleRectangleConvex(Rect(hs + x,vs + y,x + Width - 1 - hs, y + (ph div 2)), Rounding, delta);

    b := TGPLinearGradientBrush.Create(MakeRect(hs - 1 + x,vs - 1 + y,Width - (hs * 2) + 1, (ph div 2) + 3) , ColorToARGB(ChangeColor(DrawColor,180)),
      ColorToARGB(DrawColor),LinearGradientModeVertical);

    b.SetGammaCorrection(true);

    colors[0] := ColorToARGB(ChangeColor(DrawColor,220 * GlowPercentage div 100));
    colors[1] := ColorToARGB(ChangeColor(DrawColor,100 * GlowPercentage div 100));
    colors[2] := ColorToARGB(DrawColor);

    positions[0] := 0;
    positions[1] := 0.2;
    positions[2] := 1;

    b.SetInterpolationColors(@colors,@positions,3);

    g.FillPath(b,p);

    b.Free;
    p.Free;

    if Bevel then
    begin
      p := CreateRoundLine(Rect(hs + x,1 + vs + y,x + Width - 1 - hs, y + ph -1 - vs), Rounding - 1, RoundingType, 2);
      bs := TGPSolidBrush.Create(MakeColor(160, BevelColor));
      g.FillPath(bs,p);
      bs.Free;
      p.Free;
    end;
  end
  else
  begin
    path := CreateRoundRectangle(MakeRect(hs + x,vs + y, Width - 1 - (hs * 2), Height - 1 - (vs * 2)), Rounding, RoundingType, false);
    bs := TGPSolidBrush.Create(MakeColor(255, DrawColor));
    if SimpleLayoutBorder then
      pen := Tgppen.Create(MakeColor(255, BevelColor))
    else
      pen := Tgppen.Create(MakeColor(255, DrawColor));

    g.FillPath(bs, path);
    g.DrawPath(pen, path);

    pen.Free;

    bs.Free;
    path.Free;
  end;

  tw := 0;
  th := 0;
  ////Button Caption///

  stringformat := nil;
  f := nil;
  ff := nil;
  if Caption <> '' then
  begin
    f := nil;
    if Down then
      textr := MakeRect(ShiftDown + hs + x,ShiftDown + vs + y,Width - (hs * 2),ph - (vs * 2))
    else
      textr := MakeRect(hs + x,vs + y,Width - (hs * 2),ph - (vs * 2));

    if (pic <> nil) and not pic.Empty then
      textr := MakeRect(textr.X + 2, textr.Y, textr.Width - 4, textr.Height)
    else
      textr := MakeRect(textr.X, textr.Y, textr.Width - 4, textr.Height);

    if (pic <> nil) and (pic.Empty = false)  then
    begin
      case ly of
        blPictureRight, blPictureLeft: textr.Width := textr.Width - picw - Spacing;
        blPictureTop, blPictureBottom: textr.Height := textr.Height - pich - Spacing;
      end;
      if Odd(Spacing) then
        textr.Width := textr.Width - 1;
    end;

    stringformat := nil;

    sizer := MakeRect(0, 0, 0, 0);
    if not VerticalText then
    begin
      ff := TGPFontFamily.Create(Font.Name);

      fs := 0;
      if (fsBold in Font.Style) then
        fs := fs + 1;
      if (fsItalic in Font.Style) then
        fs := fs + 2;
      if (fsUnderline in Font.Style) then
        fs := fs + 4;

      stringformat := TGPStringFormat.Create;
      if not WordWrapping then
        stringformat.SetFormatFlags(StringFormatFlagsNoWrap);

      fl := StringAlignmentNear;
      case Alignment of
        taRightJustify: fl := StringAlignmentFar;
        taCenter: fl := StringAlignmentCenter;
      end;

      stringformat.SetAlignment(fl);
      stringformat.SetLineAlignment(fl);

      g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);

      f := TGPFont.Create(ff, Font.Size, fs, UnitPoint);

      g.MeasureString(Caption, Length(Caption), f, textr, stringformat, sizer);

      bs := TGPSolidBrush.Create(ColorToARGB(TextColor));
    end;

    if not VerticalText then
    begin
      tw := sizer.Width;
      th := sizer.Height;
    end
    else
    begin
      tw := TextW;
      th := TextH;
    end;

    if Down then
      pt := MakePoint(X + ShiftDown, Y + ShiftDown + (Height - th) / 2)
    else
      pt := MakePoint(X + ShiftDown, Y + ShiftDown + (Height - th) / 2);

    if (pic <> nil) and not pic.Empty then
    begin
      case ly of
        blPictureLeft:
        begin
          pt := MakePoint(textr.X + (textr.Width - tw) / 2,
            textr.Y + (textr.Height - th) / 2);

          if pt.X < Spacing then
            pt.X := Spacing;

          pt.X := pt.X + picw + Spacing;
        end;
        blPictureRight:
        begin
          pt := MakePoint(textr.X + (textr.Width - tw) / 2,
            textr.Y + (textr.Height - th) / 2);

          if pt.X < Spacing then
          begin
            pt.X := Spacing;
            tw := Width - picw - Spacing - Spacing;
            textr.Width := tw;
          end;
          
        end;
        blPictureBottom:
        begin
          pt := MakePoint(textr.X + (textr.Width - tw) / 2,
            textr.Y + (textr.Height - th) / 2);
        end;
        blPictureTop:
        begin
          pt := MakePoint(textr.X + (textr.Width - tw) / 2,
            textr.Y + (textr.Height - th) / 2);
          pt.Y := pt.Y + pich + Spacing;
        end;
      end;
    end;

    if not VerticalText then
    begin
      textr := Makerect(X + 4 + hs, pt.Y, Width - 8 - (hs * 2), sizer.Height);
      if (pic <> nil) and not pic.Empty then
      begin
        case ly of
          blPictureLeft:textr := Makerect(pt.X, pt.Y, Width - 8 - hs - (pt.X - X), sizer.Height);
          blPictureRight:textr := Makerect(x + 4 + hs, pt.Y, Width - 8 - picw - Spacing - (pt.X - x) - hs, sizer.Height);
        end;
      end;
    end;
  end;

  if (pic <> nil) and not pic.Empty then
  begin
    l := 0;
    case PictureAlignment of
      taLeftJustify: l := hs + 4;
      taRightJustify: l := (Width - 4 - hs - picw);
      taCenter: l := (Width - picw) div 2;
    end;

    t := (ph - pich) div 2;
    if Down then
    begin
      l := l + ShiftDown;
      t := t + ShiftDown;
    end;

    if (Caption <> '') and (ly <> blNone) then
    begin
      case ly of
        blPictureLeft:
        begin
          case PictureAlignment of
            taLeftJustify: l := l + x;
            taRightJustify: l := Round(pt.X - picw - Spacing);
            taCenter: l := Round(pt.X - (pt.X + picw - x) / 2);
          end;
          pic.GDIPDraw(g, Bounds(l, t + y, picw, pich));
        end;
        blPictureRight:
        begin
          case PictureAlignment of
            taLeftJustify: l := Round(pt.X + tw + spacing);
            taRightJustify: l := l + x;
            taCenter: l := Round(pt.X + tw + (Width - (pt.X + tw) - picw) / 2);
          end;
          pic.GDIPDraw(g, Bounds(l, t + y, picw, pich));
        end;
        blPictureBottom:
        begin
          t := Round(pt.Y + th + spacing);
          pic.GDIPDraw(g, Bounds(l + x, t, picw, pich));
        end;
        blPictureTop:
        begin
          t := Round(pt.Y - pich - Spacing);
          pic.GDIPDraw(g, Bounds(l + x, t, picw, pich));
        end;
      end;
    end
    else
    begin
      pic.GDIPDraw(g, Bounds(l + x, t + y, picw, pich));
    end;

    case Layout of
      blPictureLeft: textr := Makerect(l + picw + Spacing, textr.Y, x + Width - 8 - (hs * 2) - picw - l - Spacing, textr.Height);
      blPictureRight: textr := Makerect(x + (hs * 2) + 8, textr.Y, l - Spacing - (hs * 2) - 8 - x, textr.Height);
    end;
  end;

  if (Caption <> '') and Assigned(stringformat) and Assigned(bs)
  and Assigned(f) and Assigned(ff) then
  begin
    if not VerticalText then
    begin
      stringFormat.SetHotkeyPrefix(HotkeyPrefixShow);
      if WW then
        g.DrawString(Caption, Length(Caption), f, MakeRect(textr.X - 2, textr.Y, textr.Width + 2, textr.Height), stringformat, bs)
      else
        g.DrawString(Caption, Length(Caption), f, MakeRect(textr.X, textr.Y, sizer.Width, sizer.Height), stringformat, bs);
    end;

    bs.Free;
    f.Free;
    ff.Free;
    stringformat.Free;
  end;


  pic.Free;
end;

procedure TGDIPButton.EndUpdate;
begin
  Dec(FUpdateCount);
end;

procedure TGDIPButton.FontChanged(Sender: TObject);
begin
  Changed;
  if Assigned(FOnFontChange) then
    FOnFontChange(Self);
end;

procedure TGDIPButton.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> value then
  begin
    FAlignment := Value;
    Changed;
  end;
end;

procedure TGDIPButton.SetDisabledImageIndex(const Value: Integer);
begin
  if FDisabledImageIndex <> Value then
  begin
    FDisabledImageIndex := Value;
    Changed;
  end;
end;

procedure TGDIPButton.SetDisabledPictureName(const Value: String);
begin
  if FDisabledPictureName <> Value then
  begin
    FDisabledPictureName := Value;
    Changed;
  end;
end;

function TGDIPButton.IsFontStored: boolean;
begin
  Result := true;
  if Assigned(OnIsFontStored) then
    OnIsFontStored(Self, Result);
end;

procedure TGDIPButton.DoNotification(AOwner, AComponent: TComponent;
  AOperation: TOperation);
begin
  if Assigned(AOwner) then
  begin
    if not (csDestroying in AOwner.ComponentState) then
    begin
      if (AOperation = opRemove) and (AComponent = FPictureContainer) then
        FPictureContainer := nil;

      if (AOperation = opRemove) and (AComponent = FImageList) then
        FImageList := nil;
    end;
  end;
  inherited;
end;

procedure TGDIPButton.SetFocusColor(const Value: TColor);
begin
  if FFocusColor <> Value then
  begin
    FFocusColor := Value;
    Changed;
  end;
end;

procedure TGDIPButton.SetFont(const Value: TFont);
begin
  if FFont <> value then
  begin
    FFont.Assign(Value);
    FontChanged(Self);
  end;
end;

procedure TGDIPButton.SetGlowPercentage(const Value: Integer);
begin
  if FGlowPercentage <> Value then
  begin
    FGlowPercentage := Value;
    Changed;
  end;
end;

procedure TGDIPButton.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TGDIPButton.SetLayout(const Value: TGDIPButtonLayout);
begin
  if FLayout <> value then
  begin
    FLayout := Value;
    Changed;
  end;
end;

procedure TGDIPButton.SetPictureAlignment(const Value: TAlignment);
begin
  if FPictureAlignment <> Value then
  begin
    FPictureAlignment := Value;
    Changed;
  end;
end;

procedure TGDIPButton.SetPictureName(const Value: String);
begin
  if FPictureName <> Value then
  begin
    FPictureName := Value;
    Changed;
  end;
end;

procedure TGDIPButton.SetPictureStretch(const Value: Boolean);
begin
  if FPictureStretch <> Value then
  begin
    FPictureStretch := Value;
    Changed;
  end;
end;

procedure TGDIPButton.SetPictureStretchMode(const Value: TPictureMode);
begin
  if FPictureStretchMode <> Value then
  begin
    FPictureStretchMode := Value;
    Changed;
  end;
end;

procedure TGDIPButton.SetRounding(const Value: Integer);
begin
  if FRounding <> Value then
  begin
    FRounding := Value;
    Changed;
  end;
end;

procedure TGDIPButton.SetShiftDown(const Value: integer);
begin
  if FShiftDown <> value then
  begin
    FShiftDown := Value;
    Changed;
  end;
end;

procedure TGDIPButton.SetSimpleLayout(const Value: Boolean);
begin
  if FSimpleLayout <> Value then
  begin
    FSimpleLayout := Value;
    Changed;
  end;
end;

procedure TGDIPButton.SetSimpleLayoutBorder(const Value: Boolean);
begin
  if FSimpleLayoutBorder <> Value then
  begin
    FSimpleLayoutBorder := Value;
    Changed;
  end;
end;

procedure TGDIPButton.SetSpacing(const Value: integer);
begin
  if FSpacing <> value then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

procedure TGDIPButton.SetWordWrapping(const Value: Boolean);
begin
  if FWordWrapping <> Value then
  begin
    FWordWrapping := Value;
    Changed;
  end;
end;

{ TGDIPDialogButton }

procedure TGDIPDialogButton.Assign(Source: TPersistent);
begin
  if (Source is TGDIPDialogButton) then
  begin
    FFont.Assign((Source as TGDIPDialogButton).Font);
    FLayout := (Source as TGDIPDialogButton).Layout;
    FSpacing := (Source as TGDIPDialogButton).Spacing;
    Changed;
  end;
end;

procedure TGDIPDialogButton.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TGDIPDialogButton.Create;
begin
  FDrawCaption := True;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  {$ENDIF}
  FLayout := blPictureLeft;
  FSpacing := 3;
end;

destructor TGDIPDialogButton.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TGDIPDialogButton.Draw(g: TGPGraphics; Caption: String; x, y, Width, Height: integer; Color, BorderColor: TColor;
  BorderWidth: integer; BorderOpacity, Opacity: Byte; Border, Down: Boolean; Picture: TAdvGDIPPicture; WW: boolean);
var
  p: TGPGraphicsPath;
  b: TGPLinearGradientBrush;
  bs: TGPSolidBrush;
  ff: TGPFontFamily;
  fs: integer;
  f: TGPFont;
  stringformat: TGPStringFormat;
  l, t: integer;
  DrawColor: TColor;
  ph: integer;
  sizer, textr: TGPRectF;
  pt: TGPPointF;
  tw, th: Double;
  ly: TGDIPButtonLayout;
  r: TGPRectF;
begin
  ly := Layout;

  DrawColor := Color;

  ph := Height;

  r := MakeRect(x, y, Width, Height);
  p := CreateRoundRectangle(r, 4, rtBoth, false);
  b := TGPLinearGradientBrush.Create(MakeRect(r.X - 1, r.Y - 1, r.Width + 2, r.Height + 2), MakeColor(Opacity, DrawColor), MakeColor(Opacity, Drawcolor), LinearGradientModeVertical);
  g.FillPath(b, p);
  p.Free;


  r := MakeRect(x, y, Width, Height / 2);
  p := CreateRoundRectangle(r, 4, rttop, false);
  b := TGPLinearGradientBrush.Create(MakeRect(r.X - 1, r.Y - 1, r.Width + 2, r.Height + 2), MakeColor(125, clWhite), MakeColor(30, clWhite), LinearGradientModeVertical);
  g.FillPath(b, p);
  p.Free;
  b.Free;

  tw := 0;
  th := 0;

  if Border and (BorderWidth > 0) then
  begin
    p := CreateRoundLine(Bounds(Round(X), Round(Y), Round(Width), Round(Height)), 4, rtBoth, BorderWidth);
    bs := TGPSolidBrush.Create(MakeColor(BorderOpacity, BorderColor));
    g.FillPath(bs,p);
    bs.Free;
    p.Free;
  end;

  ////Button Caption///
  bs := nil;
  f := nil;
  stringformat := nil;
  ff := nil;
  if Caption <> '' then
  begin
    if Down then
      textr := MakeRect(1 + x,1 + y,Width,ph)
    else
      textr := MakeRect(x,y,Width,ph);

    if (Picture <> nil) and (Picture.Empty = false)  then
    begin
      Picture.GetImageSizes;
      case ly of
        blPictureRight, blPictureLeft: textr.Width := textr.Width - picture.Width - Spacing;
        blPictureTop, blPictureBottom: textr.Height := textr.Height - picture.Height - Spacing;
      end;
    end;


    ff := TGPFontFamily.Create(Font.Name);

    fs := 0;
    if (fsBold in Font.Style) then
      fs := fs + 1;
    if (fsItalic in Font.Style) then
      fs := fs + 2;
    if (fsUnderline in Font.Style) then
      fs := fs + 4;

    stringformat := TGPStringFormat.Create(0);

    g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);

    f := TGPFont.Create(ff, Font.Size, fs, UnitPoint);

    g.MeasureString(Caption, Length(Caption), f, textr, stringformat, sizer);

    bs := TGPSolidBrush.Create(ColorToARGB(Font.Color));

    tw := sizer.Width;
    th := sizer.Height;

    pt := MakePoint(textr.X + (textr.Width - tw) / 2, textr.Y + (textr.Height - th) / 2);
    if (Picture <> nil) and not Picture.Empty then
    begin
      case ly of
        blPictureLeft:
        begin
          pt := MakePoint(textr.X + (textr.Width - tw) / 2,
            textr.Y + (textr.Height - th) / 2);

          if pt.X < Spacing then
            pt.X := Spacing;

          pt.X := pt.X + Picture.Width + Spacing;
        end;
        blPictureRight:
        begin
          pt := MakePoint(textr.X + (textr.Width - tw) / 2,
            textr.Y + (textr.Height - th) / 2);

          if pt.X < Spacing then
          begin
            pt.X := Spacing;
            tw := Width - Picture.Width - Spacing - Spacing;
            textr.Width := tw;
          end;

        end;
        blPictureBottom:
        begin
          pt := MakePoint(textr.X + (textr.Width - tw) / 2,
            textr.Y + (textr.Height - th) / 2);
        end;
        blPictureTop:
        begin
          pt := MakePoint(textr.X + (textr.Width - tw) / 2,
            textr.Y + (textr.Height - th) / 2);
          pt.Y := pt.Y + Picture.Height + Spacing;
        end;
      end;
    end;

    textr := MakeRect(pt.X, pt.Y, sizer.Width, sizer.Height);//textr.Width - (pt.X - X), textr.Height - (pt.Y - Y));
  end;

  if (Picture <> nil) and not Picture.Empty then
  begin
    Picture.GetImageSizes;
    l := (Width - Picture.Width) div 2;
    t := (ph - Picture.Height) div 2;
    if Down then
    begin
      l := l + 2;
      t := t + 2;
    end;

    if (Caption <> '') and (ly <> blNone) then
    begin
      case ly of
        blPictureLeft:
        begin
          l := Round(pt.X - Picture.Width - Spacing);
          Picture.GDIPDraw(g, Bounds(l, t + y, Picture.Width, Picture.Height));
        end;
        blPictureRight:
        begin
          l := Round(pt.X + tw + spacing);
          Picture.GDIPDraw(g, Bounds(l, t + y, Picture.Width, Picture.Height));
        end;
        blPictureBottom:
        begin
          t := Round(pt.Y + th + spacing);
          Picture.GDIPDraw(g, Bounds(l + x, t, Picture.Width, Picture.Height));
        end;
        blPictureTop:
        begin
          t := Round(pt.Y - Picture.Height - Spacing);
          Picture.GDIPDraw(g, Bounds(l + x, t, Picture.Width, Picture.Height));
        end;
      end;
    end
    else
    begin
      Picture.GDIPDraw(g, Bounds(l + x, t + y, Picture.Width, Picture.Height));
    end;
  end;

  if (Caption <> '') and Assigned(stringformat) and Assigned(bs) and Assigned(f) and Assigned(ff) then
  begin
    stringFormat.SetHotkeyPrefix(HotkeyPrefixShow);
    if WW then
    begin
      CaptionRect := textr;
      if DrawCaption then
        g.DrawString(Caption, Length(Caption), f, textr, stringformat, bs)
    end
    else
    begin
      CaptionPos := MakePointF(pt.X, pt.Y);
      if DrawCaption then
        g.DrawString(Caption, Length(Caption), f, pt, stringformat, bs);
    end;

    bs.Free;
    f.Free;
    ff.Free;
  end;
end;

procedure TGDIPDialogButton.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TGDIPDialogButton.SetFont(const Value: TFont);
begin
  if (FFont <> value) then
  begin
    FFont.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPDialogButton.SetLayout(const Value: TGDIPButtonLayout);
begin
  if FLayout <> value then
  begin
    FLayout := Value;
    Changed;
  end;
end;

procedure TGDIPDialogButton.SetSpacing(const Value: integer);
begin
  if FSpacing <> value then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TCapacityItem }

constructor TCapacityItem.Create(Collection: TCollection);
begin
  inherited;
  FValue := 10;
  FDescription := 'Item';
  FTag := 0;

  case Index of
    1:
    begin
      FColor := $0028FFFF;
      FColorTo := $0000F0F0;
    end;
    2:
    begin
      FColor := $0058D16D;
      FColorTo := $0035C44E;
    end;
    3:
    begin
      FColor := $0009B5FF;
      FColorTo := $001780FF;
    end;
    else
    begin
      FColor := $00FF35AE;
      FColorTo := $00FF17A3;
    end;
  end;

  if Assigned(TCapacityItems(Collection).OnCreateItem) then
    TCapacityItems(Collection).OnCreateItem(TCapacityItems(Collection), Index);
end;

//------------------------------------------------------------------------------

destructor TCapacityItem.Destroy;
begin
  if Assigned(TCapacityItems(Collection).OnDeleteItem) then
    TCapacityItems(Collection).OnDeleteItem(TCapacityItems(Collection), Index);

  inherited;
end;

//------------------------------------------------------------------------------

procedure TCapacityItem.Assign(Source: TPersistent);
begin
  if (Source is TCapacityItem) then
  begin
    Value := (Source as TCapacityItem).Value;
    Color := (Source as TCapacityItem).Color;
    ColorTo := (Source as TCapacityItem).ColorTo;
    Name := (Source as TCapacityItem).Name;
    Hint := (Source as TCapacityItem).Hint;
    Description := (Source as TCapacityItem).Description;
    Tag := (Source as TCapacityItem).Tag;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TCapacityItem.Change;
begin
  if Assigned(TCapacityItems(Collection)) then
    TCapacityItems(Collection).Change;
end;

//------------------------------------------------------------------------------

procedure TCapacityItem.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TCapacityItem.SetColorTo(const Value: TColor);
begin
  if (FColorTo <> Value) then
  begin
    FColorTo := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TCapacityItem.SetDescription(const Value: string);
begin
  if (FDescription <> Value) then
  begin
    FDescription := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TCapacityItem.SetHint(const Value: string);
begin
  if (FHint <> Value) then
  begin
    FHint := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TCapacityItem.SetIndex(Value: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCapacityItem.SetName(const Value: string);
begin
  if (FName <> Value) then
  begin
    FName := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TCapacityItem.SetTag(const Value: Integer);
begin
  if (FTag <> Value) then
  begin
    FTag := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TCapacityItem.SetValue(const Value: Double);
begin
  if (FValue <> Value) then
  begin
    FValue := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

{ TCapacityItems }

function TCapacityItems.Add: TCapacityItem;
begin
  Result := TCapacityItem(inherited Add);
end;

//------------------------------------------------------------------------------

procedure TCapacityItems.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

//------------------------------------------------------------------------------

constructor TCapacityItems.Create(AOwner: TPersistent);
begin
  inherited Create(TCapacityItem);
  FOwner := AOwner;
end;

//------------------------------------------------------------------------------

function TCapacityItems.GetItem(Index: Integer): TCapacityItem;
begin
  Result := TCapacityItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TCapacityItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

function TCapacityItems.Insert(Index: Integer): TCapacityItem;
begin
  Result := TCapacityItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TCapacityItems.SetDownItem(const Value: TCapacityItem);
begin
  FDownItem := Value;
end;

//------------------------------------------------------------------------------

procedure TCapacityItems.SetItem(Index: Integer;
  const Value: TCapacityItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TGDIPCapacityBar }

constructor TGDIPCapacityBar.Create;
begin
  inherited Create;
  FShowLegend := True;
  FShowTotal := True;
  FShowFree := True;
  FBackGroundFill := TGDIPFill.Create;
  FBackGroundFill.OnChange := BackGroundFillChanged;
  FBackGroundFill.Color := RGB(116, 126, 143);
  FBackGroundFill.ColorTo := RGB(141, 151, 167);
  FBackGroundFill.ColorMirror := clNone;
  FBackGroundFill.ColorMirrorTo := clNone;

  FCapacityFont := TFont.Create;
  FCapacityFont.OnChange := FontChanged;
  FCapacityFont.Color := clWhite;
  FCapacityFont.Size := 9;
  FLegendFont := TFont.Create;
  FLegendFont.OnChange := FontChanged;
  FLegendFont.Color := clWhite;
  FLegendFont.Size := 9;
  {$IFNDEF DELPHI9_LVL}
  FCapacityFont.Name := 'Tahoma';
  FLegendFont.Name := 'Tahoma';
  {$ENDIF}
  FTransparent := False;
  FColor := $00EDECEB;
  FColorTo := $00DEDDDC;
  FOffsetX := 15;
  FOffsetY := 8;
  FTextGap := 3;
  FMarkSize := 10;
  FCapacityFormat := '%.0f MB';
  FLegendFormat := '%.0f MB';
  FFreeFormat := '%.0f MB';
  FFreeSpace := 0;
  FLegendPos := lpBottom;
  FRounded := True;
  FReflection := True;
  FReflectionOpacityStart := 150;
  FReflectionOpacityEnd := 0;
  FReflectionAxis := 1;
  FDivisions := 20;
  FReflectionPic := nil;
  FCapacityTextShadowColor := clBlack;
  FLegendTextShadowColor := clBlack;
end;

//------------------------------------------------------------------------------

destructor TGDIPCapacityBar.Destroy;
begin
  FBackGroundFill.Free;
  FCapacityFont.Free;
  FLegendFont.Free;
  if Assigned(FReflectionPic) then
    FReflectionPic.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.Assign(Source: TPersistent);
begin
  if (Source is TGDIPCapacityBar) then
  begin
    FBackGroundFill.Assign((Source as TGDIPCapacityBar).BackGroundFill);
    FColor := (Source as TGDIPCapacityBar).Color;
    FColorTo := (Source as TGDIPCapacityBar).ColorTo;
    FCapacityFont.Assign((Source as TGDIPCapacityBar).CapacityFont);
    FCapacityFormat := (Source as TGDIPCapacityBar).CapacityFormat;
    FAutoFormatValues := (Source as TGDIPCapacityBar).AutoFormatValues;
    FFreeFormat := (Source as TGDIPCapacityBar).FreeFormat;
    FDivisions := (Source as TGDIPCapacityBar).Divisions;
    FLegendPos := (Source as TGDIPCapacityBar).LegendPos;
    FLegendFormat := (Source as TGDIPCapacityBar).LegendFormat;
    FLegendFont.Assign((Source as TGDIPCapacityBar).LegendFont);
    FRounded := (Source as TGDIPCapacityBar).Rounded;
    FReflection := (Source as TGDIPCapacityBar).Reflection;
    FReflectionOpacityStart := (Source as TGDIPCapacityBar).ReflectionOpacityStart;
    FReflectionOpacityEnd := (Source as TGDIPCapacityBar).ReflectionOpacityEnd;
    Transparent := (Source as TGDIPCapacityBar).Transparent;
    OffsetX := (Source as TGDIPCapacityBar).OffsetX;
    OffsetY := (Source as TGDIPCapacityBar).OffsetY;
    CapacityTextShadowColor := (Source as TGDIPCapacityBar).CapacityTextShadowColor;
    LegendTextShadowColor := (Source as TGDIPCapacityBar).LegendTextShadowColor;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.BackGroundFillChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.DrawBackGround(g: TGPGraphics; R: TGPRectF);
begin
  BackGroundFill.Fill(g, r);
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.DrawItem(g: TGPGraphics; Item: TCapacityItem);
var
  h: Integer;
begin
  if Reflection then
    h := Round(FBarRect.Height / 2)
  else
    h := Round(FBarRect.Height);
  if Assigned(Item) then
  begin
    DrawItem(g, MakeRect(Item.Left, FBarRect.Y, Item.Width, h), Item.Color, Item.ColorTo, Round(FBarRect.X));
  end
  else
  begin
    DrawItem(g, MakeRect(FFreeLeft, FBarRect.Y, FFreeWidth, h), Color, ColorTo, Round(FBarRect.X));
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.DrawItem(g: TGPGraphics; RF: TGPRectF; Clr, ClrTo: TColor; StartLeft: Integer);
var
  LGB: TGPLinearGradientBrush;
  SB: TGPSolidBrush;
  Pen, Pen2: TGPPen;
  i: Integer;
  h3: double;
  j, k: Double;
  LnClr: TColor;
begin

  g.SetSmoothingMode(SmoothingModeNone);

  h3 := (RF.Height / 3);
  SB := TGPSolidBrush.Create(ColorToARGB(Clr));
  g.FillRectangle(SB, MakeRect(RF.X, RF.Y, RF.Width, h3));
  SB.Free;

  LGB := TGPLinearGradientBrush.Create(MakeRect(RF.X, (RF.Y + h3 - 1), RF.Width, h3 + 1), ColorToARGB(Clr), ColorToARGB(ClrTo), LinearGradientModeVertical);
  g.FillRectangle(LGB, MakeRect(RF.X, (RF.Y + h3 ), RF.Width, h3 + 1));
  LGB.Free;

  SB := TGPSolidBrush.Create(ColorToARGB(ClrTo));
  g.FillRectangle(SB, MakeRect(RF.X, RF.Y + RF.Height - h3 - 1, RF.Width, h3 + 1));
  SB.Free;


  Pen := TGPPen.Create(MakeColor(30, clGray));
  g.DrawLine(Pen, RF.X, RF.Y + RF.Height - 2, RF.X + RF.width, RF.Y + RF.Height - 2);
  Pen.Free;

  Pen := TGPPen.Create(MakeColor(100, clGray));
  g.DrawLine(Pen, RF.X, RF.Y + RF.Height - 1, RF.X + RF.width, RF.Y + RF.Height - 1);
  Pen.Free;

  LnClr := DarkenColor(ClrTo, 25);
  Pen := TGPPen.Create(MakeColor(150, LnClr));
  Pen2 := TGPPen.Create(MakeColor(120, BlendColor(Clr, clWhite, 90)));
  k := StartLeft;
  j := (FBarRect.Width / Divisions);

  for I := 1 to Divisions do
  begin
    k := k + Ceil(j);
    if (k >= RF.X) and (K < RF.X + RF.Width) then
    begin
      g.DrawLine(Pen, k, RF.Y, k, RF.Y + RF.Height - 1);
      g.DrawLine(Pen2, k + 1, RF.Y, k + 1, RF.Y + RF.Height - 1);
    end;
  end;

  Pen.Free;
  Pen2.Free;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.DrawBar(g: TGPGraphics; R: TGPRectF;
  Items: TCapacityItems);
var
  I, h: Integer;
  Pen: TGPPen;
  bmp: TBitmap;
  bg: TGPGraphics;
  pt: TPoint;
  Path: TGPGraphicsPath;
  Rgn: TGPRegion;
begin
  if Reflection then
    h := Round(FBarRect.Height / 2)
  else
    h := Round(FBarRect.Height);

  if (h > 0) and (Round(FBarRect.Width) > 0) then
  begin
    bmp := TBitmap.Create;
    bmp.Width := Round(FBarRect.Width);
    bmp.Height := h;
    bg := TGPGraphics.Create(bmp.Canvas.Handle);

    Rgn := nil;
    if Rounded then
    begin
      path := CreateCapsolePath(MakeRect(FBarRect.X, FBarRect.Y, FBarRect.Width, h), 2);
      Rgn := TGPRegion.Create(path);
      g.SetClip(Rgn);
      path.Free;
    end;

    DrawItem(bg, MakeRect(FFreeLeft - FBarRect.X, 0, FFreeWidth, h), Color, ColorTo, 0);
    for I := 0 to Items.Count - 1 do
      DrawItem(bg, MakeRect(Items[I].Left - FBarRect.X, 0, Items[I].Width, h), Items[I].Color, Items[I].ColorTo, 0);

    if Rounded then
    begin
      path := CreateCapsolePath(MakeRect(0, 0, bmp.Width, bmp.Height), 2);
      Pen := TGPPen.Create(ColorToARGB(FBackGroundFill.Color));
      bg.DrawPath(Pen, Path);
      Pen.Free;
      path.free;
    end;

    pt := Point(Round(FBarRect.X), Round(FBarRect.Y));
    DrawGDIPImage(g, nil, Pt, bmp);

    if Assigned(Rgn) then
    begin
      g.ResetClip();
      Rgn.Free;
      Rgn := nil;
    end;

    if Reflection then
    begin
      if Rounded then
      begin
        path := CreateCapsolePath(MakeRect(FBarRect.X, FBarRect.Y + h + ReflectionAxis, FBarRect.Width, h), 2);
        Rgn := TGPRegion.Create(path);
        g.SetClip(Rgn);
        path.Free;
      end;

      if FUpdateReflection or not Assigned(FReflectionPic) then
      begin
        if Assigned(FReflectionPic) then
        begin
          FReflectionPic.Free;
          FReflectionPic := nil;
        end;
        FReflectionPic := GetReflection(bmp, ReflectionOpacityStart, ReflectionOpacityEnd, h, ReflectionAxis);
        FUpdateReflection := False;
      end;

      if Assigned(FReflectionPic) then
        g.DrawImageRect(FReflectionPic, pt.X, pt.Y + ReflectionAxis + bmp.Height, bmp.Width, bmp.Height);

      if Assigned(Rgn) then
      begin
        g.ResetClip();
        Rgn.Free;
      end;
    end;

    if Rounded then
    begin
      path := CreateCapsolePath(MakeRect(FBarRect.X, FBarRect.Y, FBarRect.Width, h), 2);
      Pen := TGPPen.Create(ColorToARGB(FBackGroundFill.Color));
      g.DrawPath(Pen, Path);
      Pen.Free;
      path.free;
    end;

    bg.Free;
    bmp.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.UpdateReflection;
begin
  FUpdateReflection := True;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.DrawCapacityDescription(g: TGPGraphics; R: TGPRectF;
  CapacityDes, DesFormat: string);
begin
  if not ShowTotal then
    Exit;
  DrawGDIPText(g, taLeftJustify, FCapDesRect, CapacityDes, '', CapacityFont, True, True, clWhite, CapacityTextShadowColor <> clNone, CapacityTextShadowColor);
  DrawGDIPText(g, taLeftJustify, FCapFormatRect, DesFormat, '', CapacityFont, True, True, clWhite, CapacityTextShadowColor <> clNone, CapacityTextShadowColor);
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.DrawLegend(g: TGPGraphics; R: TGPRectF;
  Items: TCapacityItems; FreeDes, FreeDesFormat: string);

  procedure DrawMark(pt: TGPPointF; Clr, ClrTo: TColor);
  var
    LGB: TGPLinearGradientBrush;
    Pen: TGPPen;
    bmp: TBitmap;
    gr: TGPGraphics;
    pti: TPoint;
  begin
    bmp := TBitmap.Create;

    try
      bmp.Width := FMarkSize;
      bmp.Height := FMarkSize;
      gr := TGPGraphics.Create(bmp.Canvas.Handle);
      try
        LGB := TGPLinearGradientBrush.Create(MakeRect(0, 0, FMarkSize, FMarkSize), ColorToARGB(Clr), ColorToARGB(ClrTo), 60);
        gr.FillRectangle(LGB, 0, 0, FMarkSize, FMarkSize);
        LGB.Free;

        Pen := TGPPen.Create(ColorToARGB(DarkenColor(ClrTo, 30)));
        gr.DrawRectangle(Pen, 0, 0, FMarkSize - 1, FMarkSize - 1);
        Pen.Free;

        pti := Point(Round(pt.X),Round(pt.Y));
        DrawGDIPImage(g, nil, pti, bmp);
        if Reflection then
        begin
          g.DrawImageRect(GetReflection(bmp, ReflectionOpacityStart, ReflectionOpacityEnd, FMarkSize - 1, ReflectionAxis), Round(pt.X), Round(pt.Y) + ReflectionAxis + bmp.Height, bmp.Width, bmp.Height);
        end;
      finally
        gr.Free;
      end;

    finally
      bmp.Free;
    end;
  end;
var
  I: Integer;
begin
  if not ShowLegend then
    Exit;

  for I := 0 to Items.Count - 1 do
  begin
    if FMarkSize > 0 then
      DrawMark(Items[I].FMarkPT, Items[I].Color, Items[I].ColorTo);

    DrawGDIPText(g, taLeftJustify, Items[I].FDesRect, Items[I].Description, '', LegendFont, True, True, clWhite, LegendTextShadowColor <> clNone, LegendTextShadowColor);
    DrawGDIPText(g, taLeftJustify, Items[I].FFormatRect, Items[I].LegendFormat, '', LegendFont, True, True, clWhite, LegendTextShadowColor <> clNone, LegendTextShadowColor);
  end;

  if ShowFree then
  begin
    if FMarkSize > 0 then
      DrawMark(FFreeMarkPT, Color, ColorTo{clWhite});

    DrawGDIPText(g, taLeftJustify, FFreeDesRect, FreeDes, '', LegendFont, True, True, clWhite, LegendTextShadowColor <> clNone, LegendTextShadowColor);
    DrawGDIPText(g, taLeftJustify, FFreeFormatRect, FreeDesFormat, '', LegendFont, True, True, clWhite, LegendTextShadowColor <> clNone, LegendTextShadowColor);
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.Draw(g: TGPGraphics; r: TRect; TotalCapacity: Double;
  Items: TCapacityItems; CapacityDes, DesFormat, FreeDes, FreeDesFormat: string; AntiAlias: TAntiAlias);
var
  bg: TGPRectF;
  i: Integer;
begin
  if AutoFormatValues then
  begin
    DesFormat := GetSizeValue(TotalCapacity);
    if Assigned(FOnGetCapacityFormat) then
      FOnGetCapacityFormat(Self, DesFormat, TotalCapacity);
  end
  else if (CapacityDes <> '') then
  begin
    if Assigned(FOnGetCapacityFormat) then
      FOnGetCapacityFormat(Self, DesFormat, TotalCapacity);
    DesFormat := Format(DesFormat, [TotalCapacity]);
  end;

  for I := 0 to Items.Count - 1 do
  begin

    Items[I].LegendFormat := LegendFormat;
    Items[I].DisplayValue := Items[I].Value;
    if Assigned(FOnGetLegendFormat) then
      FOnGetLegendFormat(Self, Items[I], Items[I].FLegendFormat, Items[I].FDisplayValue);

    if AutoFormatValues then
      Items[I].FLegendFormat := GetSizeValue(Items[I].FDisplayValue)
    else
      Items[I].FLegendFormat := Format(Items[I].FLegendFormat, [Items[I].FDisplayValue]);
  end;

  CalculateCapacityBarRect(g, InsideRect(r), TotalCapacity, Items, CapacityDes, DesFormat, FreeDes, FreeDesFormat);

  if AutoFormatValues then
    FreeDesFormat := GetSizeValue(FFreeSpace)
  else
    FreeDesFormat := Format(FFreeFormat, [FFreeSpace]);
  
  if (BackGroundFill.BorderWidth = 0) or (BackGroundFill.BorderColor = clNone) then
    bg := MakeRect(r.Left - 1, r.Top - 1, r.Right - r.Left + 1, r.Bottom - r.Top + 1)
  else
    bg := MakeRect(r.Left, r.Top, r.Right - r.Left - 1, r.Bottom - r.Top - 1);

  if not Transparent then
    DrawBackGround(g, bg);

  if CapacityDes <> '' then
    DrawCapacityDescription(g, FCapDesRect, CapacityDes, DesFormat);
  if LegendPos <> lpNone then
    DrawLegend(g, FLegendRect, Items, FreeDes, FreeDesFormat);

  DrawBar(g, FBarRect, Items);
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.CalculateCapacityBarRect(g: TGPGraphics; R: TRect;
  TotalCapacity: Double; Items: TCapacityItems; CapacityDes, DesFormat, FreeDes, FreeDesFormat: string);
var
  I, w, MGap, LGap, y, IC: Integer;
  x, l: Double;
  R2: TRect;
  RF: TGPRectF;
  d: Double;
  fds: string;
begin
  //--- Capacity Description
  GetTextSize(g, R, CapacityDes, CapacityFont, FCapDesRect);  // Description
  RF := MakeRect(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  //FCapDesRect := DrawGDIPText(g, taLeftJustify, RF, CapacityDes, '', CapacityFont, True, False, aaAntiAlias, clWhite, True);
  //--- Capacity format
  GetTextSize(g, R, DesFormat, CapacityFont, FCapFormatRect);  // Format
  FCapDesRect.Y := R.Top + ((R.Bottom - R.Top) - (FCapDesRect.Height + FCapFormatRect.Height + FTextGap)) / 2;
  FCapFormatRect.Y := FCapDesRect.Y + FCapDesRect.Height + FTextGap;

  if CapacityDes = '' then
  begin
    FCapDesRect.Width := 0;
    FCapFormatRect.Width := 0;
  end;

  R2 := R;
  MGap := 4;
  R2.Left := Round(Max(FCapDesRect.X + FCapDesRect.Width, FCapFormatRect.X + FCapFormatRect.Width)) + OffsetX;
  w := 0;
  y := 0;
  FFreeSpace := 0;

  for I := 0 to Items.Count - 1 do
  begin
    if LegendPos <> lpNone then
    begin
      GetTextSize(g, R2, Items[I].Description, LegendFont, Items[I].FDesRect);  // Description
      Items[i].FDesRect.Width := Items[i].FDesRect.Width * 1.2;
      GetTextSize(g, R2, Items[I].LegendFormat, LegendFont, Items[I].FFormatRect);  // Format
      w := w + Round(Max(Items[I].FDesRect.Width, Items[I].FFormatRect.Width));
      y := Round(Max(y, Items[I].FDesRect.Height));
    end;
    FFreeSpace := FFreeSpace + Items[I].Value;
  end;

  FFreeSpace := TotalCapacity - FFreeSpace;
  fds := Format(FreeFormat, [FFreeSpace]);

  GetTextSize(g, R2, FreeDes, LegendFont, FFreeDesRect);  // Description
  GetTextSize(g, R2, fds, LegendFont, FFreeFormatRect);  // Format
  w := w + Round(Max(FFreeDesRect.Width, FFreeFormatRect.Width));
  y := Round(Max(y, FFreeDesRect.Height));

  if LegendPos <> lpNone then
    FMarkSize := y - 3
  else
    FMarkSize := y;

  IC := Items.Count + 1;

  w := w + (FMarkSize + MGap) * IC;    // total legends width

  LGap := Max(0, ((R2.Right - R2.Left) - w) div IC);   // possible gap between Legends
  FLegendRect.X := R2.Left;
  if LGap > 20 then
  begin
    LGap := 20;
    FLegendRect.X := R2.Left + (((R2.Right - R2.Left) - (w + LGap * (IC - 1))) div 2);
  end;

  x := Round(FLegendRect.X);
  FLegendRect.Height := y * 2 + FTextGap;

  FBarRect.X := R2.Left;
  FBarRect.Width := R2.Right - R2.Left;

  case LegendPos of
  lpTop: y := R.Top;
  lpBottom: y := R.Bottom - Round(FLegendRect.Height);
  lpNone: y := R.Bottom;
  end;

  if LegendPos = lpNone then
    FLegendRect.Height := 0;

  FLegendRect.Y := y;

  l := Round(FBarRect.X);
  d := TotalCapacity / FBarRect.Width;
  for I := 0 to Items.Count - 1 do
  begin
    Items[I].FMarkPT.X := x;
    Items[I].FDesRect.X := Items[I].FMarkPT.X + FMarkSize + MGap;
    Items[I].FFormatRect.X := Items[I].FDesRect.X;
    //if (I < Items.Count - 1) then
      x := x + FMarkSize + Max(Items[I].FDesRect.Width, Items[I].FFormatRect.Width) + LGap;

    Items[I].FMarkPT.Y := y;
    Items[I].FDesRect.Y := y;
    Items[I].FFormatRect.Y := y + Items[I].FDesRect.Height + FTextGap;

    Items[I].Left := l;
    if Items[I].Value > 0 then
      Items[I].Width := Items[I].Value / d
    else
      Items[I].Width := 0;
    l := l + Items[I].Width;
  end;

  if (Items.Count > 0) then
    FFreeLeft := Items[Items.Count - 1].Left + Items[Items.Count - 1].Width
  else
    FFreeLeft := FBarRect.x;

  FFreeWidth := FBarRect.x + FBarRect.Width - FFreeLeft;

  FFreeMarkPT.X := x;
  FFreeMarkPT.Y := y;
  FFreeDesRect.X := FFreeMarkPT.X + FMarkSize + MGap;
  FFreeFormatRect.X := FFreeDesRect.X;
  FFreeDesRect.Y := y;
  FFreeFormatRect.Y := y + FFreeDesRect.Height + FTextGap;

  FLegendRect.Width := R2.Right - R2.Left; //x - FLegendRect.X;

  if LegendPos = lpTop then
  begin
    FBarRect.Y := FLegendRect.Y + FLegendRect.Height + 3;
    FBarRect.Height := ((R2.Bottom - R2.Top) - FLegendRect.Height);
  end
  else
  begin
    FBarRect.Y := R2.Top;
    if Reflection then
      FBarRect.Height := ((R2.Bottom - R2.Top) - FLegendRect.Height) + 4
    else
      FBarRect.Height := ((R2.Bottom - R2.Top) - FLegendRect.Height) - 8
  end;

end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.FontChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

function TGDIPCapacityBar.GetInsideRectF(r: TRect): TGPRectF;
begin
  Result := MakeRect(InsideRect(r).Left, InsideRect(r).Top, InsideRect(r).Right - InsideRect(r).Left,
    InsideRect(r).Bottom - InsideRect(r).Top);
end;

function TGDIPCapacityBar.GetSizeValue(AValue: Double): String;
begin
  if AValue < 1024 then
    Result := FloatToStr(AValue)+' bytes'
  else
  begin
    if AValue < (1024 * 1024) then
      Result := FormatFloat('####0.##',AValue / 1024)+' Kb'
    else
      if AValue < (1024 * 1024 * 1024) then
        Result := FormatFloat('####0.##',AValue / 1024 / 1024)+' Mb'
      else
        Result := FormatFloat('####0.##',AValue / 1024 / 1024 / 1024)+' Gb';
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.GetTextSize(g: TGPGraphics; r: TRect; s: String;
  ft: TFont; var sizer: TGPRectF);
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

  g.SetSmoothingMode(SmoothingModeAntiAlias);  
  bn := MakeRect(r.Left, r.Top, r.Right - r.Left, r.Bottom - r.Top);
  sf := TGPStringFormat.Create;
  f := TGPFont.Create(ff, ft.Size , fs, UnitPoint);
  g.MeasureString(s, Length(s), f, bn, sf, sizer);
  sizer.Width := sizer.Width + Max(9, Length(s));

  ff.Free;
  sf.free;
  f.free;
end;

//------------------------------------------------------------------------------

function TGDIPCapacityBar.PtOnItem(pt: TPoint; Items: TCapacityItems): TCapacityItem;
var
  I, h: Integer;
  R: TRect;
begin
  Result := nil;
  if Assigned(Items) then
  begin
    if Reflection then
      h := Round(FBarRect.Height / 2)
    else
      h := Round(FBarRect.Height);

    for I := 0 to Items.Count - 1 do
    begin
      R := Rect(Round(Items[I].Left), Round(FBarRect.Y), Round(Items[I].Left) + Round(Items[I].Width), Round(FBarRect.Y) + h);
      if PtInRect(R, pt) then
      begin
        Result := Items[I];
        Break;
      end;
    end;  
  end;
end;

//------------------------------------------------------------------------------

function TGDIPCapacityBar.InsideRect(r: TRect): TRect;
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
  InflateRect(Result, -FOffsetX, -FOffsetY);
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetAutoFormatValues(const Value: Boolean);
begin
  if FAutoFormatValues <> Value then
  begin
    FAutoFormatValues := Value;
    Changed;
  end;
end;

procedure TGDIPCapacityBar.SetBackGroundFill(const Value: TGDIPFill);
begin
  FBackGroundFill.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetCapacityFont(const Value: TFont);
begin
  FCapacityFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetCapacityFormat(const Value: string);
begin
  if (FCapacityFormat <> Value) then
  begin
    FCapacityFormat := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetCapacityTextShadowColor(const Value: TColor);
begin
  if (FCapacityTextShadowColor <> Value) then
  begin
    FCapacityTextShadowColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetColorTo(const Value: TColor);
begin
  if (FColorTo <> Value) then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetDivisions(const Value: Integer);
begin
  if (FDivisions <> Value) then
  begin
    FDivisions := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetFreeFormat(const Value: string);
begin
  if (FFreeFormat <> Value) then
  begin
    FFreeFormat := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetLegendFont(const Value: TFont);
begin
  LegendFont.Assign((Value));
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetLegendFormat(const Value: string);
begin
  if (FLegendFormat <> Value) then
  begin
    FLegendFormat := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetLegendPos(const Value: TLegendPosition);
begin
  if (FLegendPos <> Value) then
  begin
    FLegendPos := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetLegendTextShadowColor(const Value: TColor);
begin
  if (FLegendTextShadowColor <> Value) then
  begin
    FLegendTextShadowColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetOffsetX(const Value: Integer);
begin
  if (FOffsetX <> Value) then
  begin
    FOffsetX := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetOffsetY(const Value: Integer);
begin
  if (FOffsetY <> Value) then
  begin
    FOffsetY := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetReflection(const Value: Boolean);
begin
  if (FReflection <> Value) then
  begin
    FReflection := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetReflectionAxis(const Value: Integer);
begin
  if (FReflectionAxis <> Value) then
  begin
    FReflectionAxis := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetReflectionOpacityEnd(const Value: Integer);
begin
  if (FReflectionOpacityEnd <> Value) then
  begin
    FReflectionOpacityEnd := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetReflectionOpacityStart(const Value: Integer);
begin
  if (FReflectionOpacityStart <> Value) then
  begin
    FReflectionOpacityStart := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetRounded(const Value: Boolean);
begin
  if (FRounded <> Value) then
  begin
    FRounded := Value;
    Changed;
  end;
end;

procedure TGDIPCapacityBar.SetShowFree(const Value: Boolean);
begin
  if FShowFree <> Value then
  begin
    FShowFree := Value;
    Changed;
  end;
end;

procedure TGDIPCapacityBar.SetShowLegend(const Value: Boolean);
begin
  if FShowLegend <> Value then
  begin
    FShowLegend := Value;
    Changed;
  end;
end;

procedure TGDIPCapacityBar.SetShowTotal(const Value: Boolean);
begin
  if FShowTotal <> Value then
  begin
    FShowTotal := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetTextGap(const Value: Integer);
begin
  if FTextGap <> Value then
  begin
    FTextGap := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGDIPCapacityBar.SetTransparent(const Value: Boolean);
begin
  if (FTransparent <> Value) then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------


initialization
  CF_GDIPFILL := RegisterClipboardFormat('GDI+ Fill');

end.
