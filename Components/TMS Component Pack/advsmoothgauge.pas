{*************************************************************************}
{ TAdvSmoothGauge component                                               }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2010 - 2015                                      }
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
unit AdvSmoothGauge;

interface

{$I TMSDEFS.INC}

uses
  Forms, Math, Classes, Graphics, Controls, SysUtils, ExtCtrls,
  Windows, AdvStyleIF, Messages,
  AdvGDIP
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.1.0 : New : ValueFormat property added
  // v1.1.0.0 : New : Property Digit.Visible to show / hide digits
  //          : New : Property AnimationFactor
  //          : New : StartValue and EndValue of threshold
  //          : Fixed : C++ Builder for dynamic arrays
  // v1.2.0.0 : New : Focus indication
  //          : New : Change value by clicking on scale
  // v1.3.0.0 : New : Customizable threshold sections
  // v1.3.1.0 : New : Support for Windows Vista and Windows Seven Style
  // v1.3.2.0 : New : Built-in support for reduced color set for use with terminal servers
  //          : Improved: Led Display drawing and alignment
  // v1.3.3.0 : New : Delphi 2010 Touch Support
  //          : New : Event OnDisplayValue
  // v1.3.3.1 : Fixed : Small Memory Leak
  // v1.3.3.2 : Fixed : Small Issue with Font changed
  // v1.3.4.0 : New property ShowValues
  //          : New event OnGetValueText
  //          : New TextRendering property
  // v1.3.5.0 : New : Built-in support for Office 2010 colors
  // v1.3.5.1 : Fixed : Issue in C++builder with type declaration
  // v2.0.0.0 : New : Extra needles
  //          : New : CircleStartValue and CircleEndValue to set partial visibility of gauge
  //          : New : Logarithmic scale
  //          : Improved : Section type
  // v2.0.0.1 : Improved : Semi circle on full area
  // v2.0.0.2 : Fixed : Issue with animation margin causing continuous CPU usage
  // v2.0.1.0 : New : Added public method DrawGaugeGDIP();
  // v2.1.0.0 : New : Metro Style support
  //          : Fixed : Issue with smaller values and zero rounding
  // v2.1.1.0 : New : Boolean Property UnlimitedValue / UnlimitedDisplayValue to allow Values out of range
  // v2.2.0.0 : New : Windows 8, Office 2013 styles added
  // v2.3.0.0 : New : Windows 10, Office 2016 styles added

type
  TCalibrationValueType = (cvNone, cvSmall, cvLarge);

  TCalibrationValue = record
    x, y, x1, y1, tx, ty: Double;
    ValueType: TCalibrationValueType;
    rv: Double;
  end;

  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TArrayOfPointF = array of TGPPointF;

  TAdvSmoothGauge = class;

  TAdvSmoothGaugeThresholdKind = (tkAngle, tkValue);

  TAdvSmoothGaugeTextRenderingHint = (tAntiAlias, tAntiAliasGridFit, tClearType);

  TAdvSmoothGaugeThreshold = class(TPersistent)
  private
    FOwner: TAdvSmoothGauge;
    FColor: TColor;
    FOpacity: integer;
    FStartAngle: integer;
    FSweepAngle: integer;
    FCenter: Double;
    FSpan: integer;
    FOnChange: TNotifyEvent;
    FStartValue: Double;
    FEndValue: Double;
    FThresHoldKind: TAdvSmoothGaugeThresholdKind;
    procedure SetCenter(const Value: Double);
    procedure SetColor(const Value: TColor);
    procedure SetOpacity(const Value: Integer);
    procedure SetSpan(const Value: integer);
    procedure SetStartAngle(const Value: integer);
    procedure SetSweepAngle(const Value: integer);
    procedure SetEndValue(const Value: Double);
    procedure SetStartValue(const Value: Double);
    procedure SetThresHoldKind(const Value: TAdvSmoothGaugeThresholdKind);
    function GetStartAngle: integer;
    function GetSweepAngle: integer;
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TAdvSmoothGauge);
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default $00FC7C;
    property Opacity: Integer read FOpacity write SetOpacity default 200;
    property Center: Double read FCenter write SetCenter;
    property Span: integer read FSpan write SetSpan default 25;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property StartAngle: integer read GetStartAngle write SetStartAngle default 135;
    property SweepAngle: integer read GetSweepAngle write SetSweepAngle default 270;
    property StartValue: Double read FStartValue write SetStartValue;
    property EndValue: Double read FEndValue write SetEndValue;
    property ThresholdKind: TAdvSmoothGaugeThresholdKind read FThresHoldKind write SetThresHoldKind default tkAngle;
  end;

  TAdvSmoothGaugeArc = class(TPersistent)
  private
    FOwner: TAdvSmoothGauge;
    FColor: TColor;
    FWidth: Double;
    FOpacity: integer;
    FStartAngle: integer;
    FStopAngle: integer;
    FThreshold: TAdvSmoothGaugeThreshold;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetOpacity(const Value: integer);
    procedure SetStartAngle(const Value: integer);
    procedure SetStopAngle(const Value: integer);
    procedure SetThreshold(const Value: TAdvSmoothGaugeThreshold);
    procedure SetWidth(const Value: Double);
    function GetStartAngle: integer;
    function GetStopAngle: integer;
  protected
    procedure Changed;
    procedure ThresholdChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothGauge);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default $8B0000;
    property Width: Double read FWidth write SetWidth;
    property Opacity: integer read FOpacity write SetOpacity default 200;
    property StartAngle: integer read GetStartAngle write SetStartAngle default 135;
    property StopAngle: integer read GetStopAngle write SetStopAngle default 405;
    property Threshold: TAdvSmoothGaugeThreshold read FThreshold write SetThreshold;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothGaugeOuterCircle = class(TPersistent)
  private
    FOwner: TAdvSmoothGauge;
    FColor: TColor;
    FWidth: Double;
    fOpacity: integer;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetOpacity(const Value: integer);
    procedure SetWidth(const Value: Double);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TAdvSmoothGauge);
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default $908070;
    property Width: Double read FWidth write SetWidth;
    property Opacity: integer read fOpacity write SetOpacity default 100;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothGaugeGloss = class(TPersistent)
  private
    FOwner: TAdvSmoothGauge;
    FColor: TColor;
    FOpacity: integer;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetOpacity(const Value: integer);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TAdvSmoothGauge);
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clWhite;
    property Opacity: integer read FOpacity write SetOpacity default 72;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothGaugeOuterRim = class(TPersistent)
  private
    FOwner: TAdvSmoothGauge;
    FColor: TColor;
    FWidth: Integer;
    fOpacity: integer;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetOpacity(const Value: integer);
    procedure SetWidth(const Value: integer);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TAdvSmoothGauge);
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default $908070;
    property Opacity: integer read fOpacity write SetOpacity default 255;
    property Width: integer read FWidth write SetWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothGaugeInnerCircle = class(TPersistent)
  private
    FOwner: TAdvSmoothGauge;
    FColor: TColor;
    fOpacity: integer;
    FGloss: TAdvSmoothGaugeGloss;
    FOnChange: TNotifyEvent;
    procedure SetGloss(const Value: TAdvSmoothGaugeGloss);
    procedure SetColor(const Value: TColor);
    procedure SetOpacity(const Value: integer);
  protected
    procedure Changed;
    procedure GlossChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothGauge);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBlue;
    property Opacity: integer read fOpacity write SetOpacity default 150;
    property Gloss: TAdvSmoothGaugeGloss read FGloss write SetGloss;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothGaugeDigit = class(TPersistent)
  private
    FOwner: TAdvSmoothGauge;
    fColor: TColor;
    fBackgroundColor: TColor;
    fBackgroundOpacity: integer;
    fOnChange: TNotifyEvent;
    FVisible: Boolean;
    FAlignment: TAlignment;
    procedure SetBackGroundColor(const Value: TColor);
    procedure SetBackGroundOpacity(const Value: integer);
    procedure SetColor(const Value: TColor);
    procedure SetVisible(const Value: Boolean);
    procedure SetAlignment(const Value: TAlignment);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TAdvSmoothGauge);
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read fColor write SetColor default $00FC7C;
    property BackGroundColor: TColor read fBackgroundColor write SetBackGroundColor default $808080;
    property BackGroundOpacity: integer read fBackgroundOpacity write SetBackGroundOpacity default 30;
    property Visible: Boolean read FVisible write SetVisible default true;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothGaugeNeedle = class(TPersistent)
  private
    FOwner: TAdvSmoothGauge;
    FColor: TColor;
    FShineColor: TColor;
    FShineColorTo: TColor;
    FOuterCenterColor: TColor;
    FOuterCenterColorTo: TColor;
    FOuterCenterOpacity: integer;
    FInnerCenterColor: TColor;
    FInnerCenterColorTo: TColor;
    FInnerCenterOpacity: integer;
    FOnchange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetInnerCenterColor(const Value: TColor);
    procedure SetInnerCenterColorTo(const Value: TColor);
    procedure SetInnerCenterOpacity(const Value: integer);
    procedure SetOuterCenterColor(const Value: TColor);
    procedure SetOuterCenterColorTo(const Value: TColor);
    procedure SetOuterCenterOpacity(const Value: integer);
    procedure SetShineColor(const Value: TColor);
    procedure SetShineColorTo(const Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TAdvSmoothGauge);
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default $00FC7C;
    property ShineColor: TColor read FShineColor write SetShineColor default $00FC7C;
    property ShineColorTo: TColor read FShineColorTo write SetShineColorTo default $D3D3D3;
    property OuterCenterColor: TColor read FOuterCenterColor write SetOuterCenterColor default clBlue;
    property OuterCenterColorTo: TColor read FOuterCenterColorTo write SetOuterCenterColorTo default clBlue;
    property OuterCenterOpacity: integer read FOuterCenterOpacity write SetOuterCenterOpacity default 100;
    property InnerCenterColor: TColor read FInnerCenterColor write SetInnerCenterColor default $8B0000;
    property InnerCenterColorTo: TColor read FInnerCenterColorTo write SetInnerCenterColorTo default $8B0000;
    property InnerCenterOpacity: integer read FInnerCenterOpacity write SetInnerCenterOpacity default 255;
    property OnChange: TNotifyEvent read FOnchange write FOnchange;
  end;

  TAdvSmoothGaugeNeedleItem = class(TCollectionItem)
  private
    FOwner: TAdvSmoothGauge;
    FValue: Double;
    FColor: TColor;
    FShineColor: TColor;
    FShineColorTo: TColor;
    procedure SetValue(const Value: Double);
    procedure SetColor(const Value: TColor);
    procedure SetShineColor(const Value: TColor);
    procedure SetShineColorTo(const Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default $00FC7C;
    property ShineColor: TColor read FShineColor write SetShineColor default $00FC7C;
    property ShineColorTo: TColor read FShineColorTo write SetShineColorTo default $D3D3D3;
    property Value: Double read FValue write SetValue;
  end;

  TAdvSmoothGaugeNeedles = class(TCollection)
  private
    FOwner: TAdvSmoothGauge;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvSmoothGaugeNeedleItem;
    procedure SetItem(Index: Integer; const Value: TAdvSmoothGaugeNeedleItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvSmoothGauge);
    function Add: TAdvSmoothGaugeNeedleItem;
    function Insert(Index: Integer): TAdvSmoothGaugeNeedleItem;
    property Items[Index: Integer]: TAdvSmoothGaugeNeedleItem read GetItem write SetItem; default;
    procedure Delete(Index: Integer);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothGaugeSectionType = (stCustomMargin, stBorder);

  TAdvSmoothGaugeSection = class(TCollectionItem)
  private
    FOwner: TAdvSmoothGauge;
    FColor: TColor;
    fOpacity: integer;
    FGloss: TAdvSmoothGaugeGloss;
    FOnChange: TNotifyEvent;
    FStartValue: Double;
    FEndValue: Double;
    FInnerMargin: integer;
    FOuterMargin: integer;
    FSectionType: TAdvSmoothGaugeSectionType;
    procedure SetColor(const Value: TColor);
    procedure SetGloss(const Value: TAdvSmoothGaugeGloss);
    procedure SetOpacity(const Value: integer);
    procedure SetEndValue(const Value: Double);
    procedure SetStartValue(const Value: Double);
    procedure SetInnerMargin(const Value: integer);
    procedure SetOuterMargin(const Value: integer);
    procedure SetSectionType(const Value: TAdvSmoothGaugeSectionType);
  protected
    procedure Changed;
    procedure GlossChanged(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBlue;
    property Opacity: integer read fOpacity write SetOpacity default 150;
    property Gloss: TAdvSmoothGaugeGloss read FGloss write SetGloss;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property StartValue: Double read FStartValue write SetStartValue;
    property EndValue: Double read FEndValue write SetEndValue;
    property OuterMargin: integer read FOuterMargin write SetOuterMargin default 20;
    property InnerMargin: integer read FInnerMargin write SetInnerMargin default 0;
    property SectionType: TAdvSmoothGaugeSectionType read FSectionType write SetSectionType default stCustomMargin;
  end;

  TAdvSmoothGaugeSections = class(TCollection)
  private
    FOwner: TAdvSmoothGauge;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvSmoothGaugeSection;
    procedure SetItem(Index: Integer; const Value: TAdvSmoothGaugeSection);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvSmoothGauge);
    function Add: TAdvSmoothGaugeSection;
    function Insert(Index: Integer): TAdvSmoothGaugeSection;
    property Items[Index: Integer]: TAdvSmoothGaugeSection read GetItem write SetItem; default;
    procedure Delete(Index: Integer);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothGaugeValueChanged = procedure(Sender: TObject; value: Double) of object;

  TAdvSmoothGaugeDisplayValue = procedure(Sender: TObject; var value: Double) of object;

  TAdvSmoothGaugeGetValueText = procedure(Sender: TObject; value: Double; var str: String) of object;

  TAdvSmoothGaugeNeedleType = (ntPointer, ntSelector);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothGauge = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FMetroStyle: Boolean;
    doAnimation: Boolean;
    FFocused: Boolean;
    FSta, FSwpa: double;
    Frg: TGPRectF;
    FCalibrationValues: array[0..1000] of TCalibrationValue;
    FDesignTime: Boolean;
    FanimationTimer: TTimer;
    fminValue: Double;
    fmaxValue: Double;
    fcurrentValue: Double;
    FcurrentValueTo: Double;
    fcurrentDisplayValue: Double;
    FcurrentDisplayValueTo: Double;
    FValueFormat: string;
    fnoOfDivisions: Integer;
    fnoOfSubDivisions: Integer;
    fdialText: string;
    fxp: Integer;
    fyp: Integer;
    fwidth: Integer;
    fheight: Integer;
    fbackgroundImg: TGPBitmap;
    frectImg: TGPRectF;
    fFont: TFont;
    FValueFont: TFont;
    FDigit: TAdvSmoothGaugeDigit;
    FOuterCircle: TAdvSmoothGaugeOuterCircle;
    FInnerCircle: TAdvSmoothGaugeInnerCircle;
    FOuterRim: TAdvSmoothGaugeOuterRim;
    FNeedle: TAdvSmoothGaugeNeedle;
    FArc: TAdvSmoothGaugeArc;
    FSubDivWidth: integer;
    FDivWidth: integer;
    fSubDivColor: TColor;
    fDivColor: TColor;
    FAnimation: Boolean;
    FOnValueChanged: TAdvSmoothGaugeValueChanged;
    FAnimationFactor: integer;
    FNeedleType: TAdvSmoothGaugeNeedleType;
    FSections: TAdvSmoothGaugeSections;
    FOnDisplayValue: TAdvSmoothGaugeDisplayValue;
    FOnChange: TNotifyEvent;
    FShowValues: Boolean;
    FOnGetValueText: TAdvSmoothGaugeGetValueText;
    FTextRendering: TAdvSmoothGaugeTextRenderingHint;
    FCircleStartValue: Integer;
    FCircleEndValue: Integer;
    FExtraNeedles: TAdvSmoothGaugeNeedles;
    FEqualDimensions: Boolean;
    FLogaritchmic: Boolean;
    FLogaritchmicBase: Integer;
    FUnlimitedValue: Boolean;
    FUnlimitedDisplayValue: Boolean;
    FOnDisplayValueChanged: TAdvSmoothGaugeValueChanged;
    procedure SetDialText(const Value: string);
    procedure SetMaxValue(const Value: Double);
    procedure SetMinValue(const Value: double);
    procedure SetNoOfDivisions(const Value: Integer);
    procedure SetNoOfSubDivisions(const Value: Integer);
    procedure SetValue(const Value: Double);
    procedure SetFont(const Value: TFont);
    procedure SetValueFont(const Value: TFont);
    procedure SetValueFormat(const Value: string);
    procedure SetDigit(const Value: TAdvSmoothGaugeDigit);
    procedure SetInnerCircle(const Value: TAdvSmoothGaugeInnerCircle);
    procedure SetOuterCircle(const Value: TAdvSmoothGaugeOuterCircle);
    procedure SetOuterRim(const Value: TAdvSmoothGaugeOuterRim);
    procedure SetNeedle(const Value: TAdvSmoothGaugeNeedle);
    procedure SetArc(const Value: TAdvSmoothGaugeArc);
    procedure SetDivColor(const Value: TColor);
    procedure SetDivWidth(const Value: integer);
    procedure SetSubDivWidth(const Value: integer);
    procedure SetSubDivColor(const Value: TColor);
    procedure SetAnimation(const Value: Boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetAnimationFactor(const Value: integer);
    procedure SetNeedleType(const Value: TAdvSmoothGaugeNeedleType);
    procedure SetSections(const Value: TAdvSmoothGaugeSections);
    procedure SetShowValues(const Value: Boolean);
    procedure SetTextRendering(const Value: TAdvSmoothGaugeTextRenderingHint);
    procedure SetCircleEndValue(const Value: Integer);
    procedure SetCircleStartValue(const Value: Integer);
    procedure SetExtraNeedles(const Value: TAdvSmoothGaugeNeedles);
    procedure SetEqualDimensions(const Value: Boolean);
    procedure SetLogarithmic(const Value: Boolean);
    procedure SetLogarithmicBase(const Value: Integer);
    function GetValue: Double;
    procedure SetUnlimitedValue(const Value: Boolean);
    procedure SetUnlimitedDisplayValue(const Value: Boolean);
    function GetDisplayValue: Double;
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Changed;
    procedure FontChanged(Sender: TObject);
    procedure DigitChanged(Sender: TObject);
    procedure OuterCircleChanged(Sender: TObject);
    procedure InnerCircleChanged(Sender: TObject);
    procedure OuterRimChanged(Sender: TObject);
    procedure NeedleChanged(Sender: TObject);
    procedure ArcChanged(Sender: TObject);
    procedure SectionsChanged(Sender: TObject);
    procedure NeedlesChanged(Sender: TObject);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;    
    procedure AnimateGauge(Sender: TObject);
    procedure DrawPointer(gr: TGPGraphics; col, shncol, shncolto: TColor; cx: Double; cy: Double; value: Double);
    procedure Draw(gb: TGPGraphics);
    procedure DrawGloss(g: TGPGraphics);
    procedure DrawCenterPoint(g: TGPGraphics; rect: TGPRectF; cX: Integer; cY: Integer);
    procedure DrawCalibration(g: TGPGraphics);
    procedure DrawSections(g: TGPGraphics);
    procedure DrawString(g: TGPGraphics; value: WideString; ft: TFont; Brush: TGPBrush; pt: TGPPointF; stringformat: TGPStringFormat; DialDraw: Boolean);
    procedure DisplayNumber(g: TGPGraphics; number: Double; drect: TGPRectF);
    procedure DrawDigit(g: TGPGraphics; number: Integer; position: TGPPointF; dp: Boolean; h: integer);
    procedure CalculateImageRect;
    procedure CalculateCalibration;
    procedure CalculateArcThresHold;
    procedure DoExit; override;
    procedure DoEnter; override;
    function XYToScale(X, Y: integer): Double;
    function GetRadian(theta: Double): Double;
    function GetX(x: Double; width: Double): Double;
    function GetY(y: Double; height: Double): Double;
    function IsNumberAvailable(Number: integer; const listOfNumbers: array of integer): Boolean;
    function GetVersionNr: integer;
    function GetAngle(Value: Double): Double;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function CalcPosition(Value: Double): TArrayOfPointF;
    function PtInPoly(const Points: TArrayOfPointF; X,Y: Integer): Boolean;
    procedure DrawFocus(g: TGPGraphics; r: TGPRectF);
    function GetWidth: Integer;
    function GetHeight: Integer;
  public
    constructor Create(AOwner: TComponent); override;
//    procedure CreateParams(var Params: TCreateParams); override;
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeID: String;
    procedure DrawGauge(ACanvas: TCanvas; r: TRect);
    procedure DrawGaugeGDIP(Graphics: TGPGraphics; r: TRect);
    procedure SaveToImage(Filename: String; ImageWidth, ImageHeight: integer; ImageType: TImageType = itBMP; ImageQualityPercentage: integer = 100);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DoValueChanged(Sender: TObject; value: Double);
    procedure DoDisplayValueChanged(Sender: TObject; value: Double);
    property DisplayValue: Double read GetDisplayValue;
  published
    property UnlimitedValue: Boolean read FUnlimitedValue write SetUnlimitedValue default False;
    property UnlimitedDisplayValue: Boolean read FUnlimitedDisplayValue write SetUnlimitedDisplayValue default False;
    property EqualDimensions: Boolean read FEqualDimensions write SetEqualDimensions default True;
    property AnimationFactor: integer read FAnimationFactor write SetAnimationFactor default 5;
    property MinimumValue: Double read fminvalue write SetMinValue;
    property MaximumValue: Double read fmaxvalue write SetMaxValue;
    property Value: Double read GetValue write SetValue;
    property Digit: TAdvSmoothGaugeDigit read FDigit write SetDigit;
    property OuterCircle: TAdvSmoothGaugeOuterCircle read FOuterCircle write SetOuterCircle;
    property InnerCircle: TAdvSmoothGaugeInnerCircle read FInnerCircle write SetInnerCircle;
    property OuterRim: TAdvSmoothGaugeOuterRim read FOuterRim write SetOuterRim;
    property Needle: TAdvSmoothGaugeNeedle read FNeedle write SetNeedle;
    property Arc: TAdvSmoothGaugeArc read FArc write SetArc;
    property DivisionCount: Integer read fnoOfDivisions write SetNoOfDivisions default 10;
    property SubDivisionCount: Integer read fnoOfSubDivisions write SetNoOfSubDivisions default 3;
    property DivisionColor: TColor read fDivColor write SetDivColor default $00FC7C;
    property SubDivisionColor: TColor read fSubDivColor write SetSubDivColor default $00FC7C;
    property DivisionWidth: integer read FDivWidth write SetDivWidth default 5;
    property SubDivisionWidth: integer read FSubDivWidth write SetSubDivWidth default 2;
    property DialText: string read fdialText write SetDialText;
    property Font: TFont read fFont write SetFont;
    property ValueFont: TFont read FValueFont write SetValueFont;
    property ValueFormat: string read FValueFormat write SetValueFormat;
    property ShowValues: Boolean read FShowValues write SetShowValues default true;
    property Animation: Boolean read FAnimation write SetAnimation default true;
    property Version: string read GetVersion write SetVersion;
    property NeedleType: TAdvSmoothGaugeNeedleType read FNeedleType write SetNeedleType default ntPointer;
    property Sections: TAdvSmoothGaugeSections read FSections write SetSections;
    property TextRendering: TAdvSmoothGaugeTextRenderingHint read FTextRendering write SetTextRendering default tClearType;
    property CircleStartValue: Integer read FCircleStartValue write SetCircleStartValue default 0;
    property CircleEndValue: Integer read FCircleEndValue write SetCircleEndValue default 360;
    property Logarithmic: Boolean read FLogaritchmic write SetLogarithmic default False;
    property LogarithmicBase: Integer read FLogaritchmicBase write SetLogarithmicBase default 10;

    property OnValueChanged: TAdvSmoothGaugeValueChanged read FOnValueChanged write FOnValueChanged;
    property OnDisplayValueChanged: TAdvSmoothGaugeValueChanged read FOnDisplayValueChanged write FOnDisplayValueChanged;
    property OnDisplayValue: TAdvSmoothGaugeDisplayValue read FOnDisplayValue write FOnDisplayValue;
    property OnGetValueText: TAdvSmoothGaugeGetValueText read FOnGetValueText write FOnGetValueText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property ExtraNeedles: TAdvSmoothGaugeNeedles read FExtraNeedles write SetExtraNeedles;

    property Align;
    property Anchors;
    property Constraints;
    property PopupMenu;
    property TabOrder;
    property ParentShowHint;
    property ShowHint;
    property OnKeyUp;
    property OnKeyDown;
    property OnKeyPress;
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
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;   
    property Visible;
    property TabStop default true;
    property Enabled;
    {$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
    {$ENDIF}
  end;
  
implementation

{$I DELPHIXE.INC}

procedure TAdvSmoothGauge.DrawFocus(g: TGPGraphics; r: TGPRectF);
var
  pathfocus: TGPGraphicsPath;
  pfocus: TGPPen;
begin
  pathfocus := TGPGraphicsPath.Create;
  pathfocus.AddArc(r, CircleStartValue, CircleEndValue - CircleStartValue);
  pathfocus.AddLine(r.X + r.width / 2, r.y +r.height / 2, r.X +r.Width / 2, r.Y + r.HEight / 2);
  pathfocus.CloseFigure;
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  pfocus := TGPPen.Create(MakeColor(255, clBlack), 1);
  pfocus.SetDashStyle(DashStyleDot);
  g.DrawPath(pfocus, pathfocus);
  pfocus.Free;
  pathfocus.Free;
end;


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

procedure TAdvSmoothGauge.AnimateGauge(Sender: TObject);
var
  d, pos: Double;
  res: Boolean;
  doanim, doanimdis: Boolean;
begin
  if Animation and not (csLoading in ComponentState) and not (csDesigning in ComponentState) and doAnimation then
  begin
    d := Abs(FcurrentValueTo - fcurrentValue) / AnimationFactor;
    pos := fcurrentValue;
    res := AnimateDouble(pos, FcurrentValueTo, d, 0.001);
    if res then
    begin
      doanim := True;
      fcurrentValue := pos;
      DoValueChanged(Self, pos);

      Changed;
    end
    else
    begin
      doanim := false;
      FcurrentValue := FcurrentValueTo;
      Changed;
    end;

    d := Abs(FcurrentDisplayValueTo - fcurrentDisplayValue) / AnimationFactor;
    pos := fcurrentDisplayValue;
    res := AnimateDouble(pos, FcurrentDisplayValueTo, d, 0.001);
    if res then
    begin
      doanimdis := True;
      fcurrentDisplayValue := pos;
      DoDisplayValueChanged(Self, pos);

      Changed;
    end
    else
    begin
      doanimdis := FAlse;
      FcurrentDisplayValue := FcurrentDisplayValueTo;
      Changed;
    end;


    doanimation := doanimdis or doanim;
  end;
end;

procedure TAdvSmoothGauge.ArcChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothGauge.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothGauge) then
  begin
    FAnimationFactor := (Source as TAdvSmoothGauge).AnimationFactor;
    FMinValue := (Source as TAdvSmoothGauge).MinimumValue;
    FMaxValue := (Source as TAdvSmoothGauge).MaximumValue;
    fcurrentValue := (Source as TAdvSmoothGauge).Value;
    fcurrentDisplayValue := (Source as TAdvSmoothGauge).DisplayValue;
    FDigit.Assign((Source as TAdvSmoothGauge).Digit);
    FOuterCircle.Assign((Source as TAdvSmoothGauge).OuterCircle);
    FInnerCircle.Assign((Source as TAdvSmoothGauge).InnerCircle);
    FOuterRim.Assign((Source as TAdvSmoothGauge).OuterRim);
    FNeedle.Assign((Source as TAdvSmoothGauge).Needle);
    FArc.Assign((Source as TAdvSmoothGauge).Arc);
    fnoOfDivisions := (Source as TAdvSmoothGauge).DivisionCount;
    fnoOfSubDivisions := (Source as TAdvSmoothGauge).SubDivisionCount;
    fDivColor := (Source as TAdvSmoothGauge).DivisionColor;
    fSubDivColor := (Source as TAdvSmoothGauge).SubDivisionColor;
    FDivWidth := (Source as TAdvSmoothGauge).DivisionWidth;
    FSubDivWidth := (Source as TAdvSmoothGauge).SubDivisionWidth;
    FDialText := (Source as TAdvSmoothGauge).DialText;
    FFont.Assign((Source as TAdvSmoothGauge).Font);
    FValueFont.Assign((Source as TAdvSmoothGauge).ValueFont);
    FAnimation := (Source as TAdvSmoothGauge).Animation;
    FNeedleType := (Source as TAdvSmoothGauge).NeedleType;
    FSections.Assign((Source as TAdvSmoothGauge).Sections);
    FTextRendering := (Source as TAdvSmoothGauge).TextRendering;
    Changed;
  end;
end;

function TAdvSmoothGauge.CalcPosition(Value: Double): TArrayOfPointF;
var
  pts: TArrayOfPointF;
  val, rd, a, cx, cy: Double;
begin
  rd := GetWidth / 2 - (GetWidth * 0.12);
  val := (fMaxValue - fMinValue);
  val := ((100 * (Value - fMinValue)) / val);
  val := ((Arc.StopAngle - Arc.StartAngle) * val) / 100;
  val := val + Arc.StartAngle;

  a := GetRadian(val);

  SetLength(pts, 5);

  cx := fwidth / 2 + fxp;
  cy := fheight / 2 + fyp;

  pts[0] := MakePoint(cx + rd * Cos(a), cy + rd * Sin(a));
  pts[4] := MakePoint(cx + rd * Cos(a - 0.02), cy + rd * Sin(a - 0.02));
  a := GetRadian(val +  20);
  pts[1] := MakePoint(cx + (GetWidth * 0.09) * Cos(a), cy + (GetWidth * 0.09) * Sin(a));
  pts[2] := MakePoint(cx, cy);
  a := GetRadian((val - 20));
  pts[3] := MakePoint(cx + (GetWidth * 0.09) * Cos(a), cy + (GetWidth * 0.09) * Sin(a));

  result := pts;
end;

procedure TAdvSmoothGauge.CalculateArcThresHold;
var
  val: Double;
  gp: integer;
begin
  gp := Round(GetWidth * Arc.Width);
  Frg := Makerect((frectImg.X + gp), (frectImg.Y + gp), (frectImg.Width - (gp * 2)), (frectImg.Height - (gp * 2)));

  Frg := MakeRect((frectImg.X + gp), (frectImg.Y + gp),
      (frectImg.Width - (gp * 2)), (frectImg.Height - (gp * 2)));

  val := (fMaxValue - fMinValue);
  val := ((100 * (Arc.Threshold.Center - fMinValue)) / val);
  val := ((Arc.StopAngle - Arc.StartAngle) * val) / 100;
  val := val + Arc.StartAngle;

  Fsta := (val - ((Arc.Threshold.SweepAngle * Arc.Threshold.Span) / 200));

  if (Fsta <= Arc.StartAngle) then
    Fsta := Arc.StartAngle;

  Fswpa := ((Arc.Threshold.SweepAngle * Arc.Threshold.Span) / 100);

  if ((Fsta + Fswpa) > Arc.StopAngle) then
    Fswpa := (Arc.StopAngle - Fsta);
end;

procedure TAdvSmoothGauge.CalculateCalibration;
var
  i, j, p, k, gp: Integer;
  rv, ca, sht, incr, ta, rd, tx, ty, y1, x1, y, x: Double;
  r: TGPRectF;
  noint: Integer;
  nopart: Integer;
  rect: TGPRectF;
  cx, cy: Double;
begin
  for I := 0 to Length(FCalibrationValues) - 1 do
    FCalibrationValues[I].ValueType := cvNone;
    
  rect := frectImg;
  cx := Round(fwidth / 2) + fxp;
  cy := Round(fheight / 2) + fyp;
  nopart := (fnoOfDivisions + 1);
  noint := fnoOfSubDivisions;
  ca := GetRadian(Arc.StartAngle);
  gp := Round(getWidth * 0.01);
  sht := (GetWidth / 25);
  r := MakeRect((rect.x + gp), (rect.y + gp), (rect.Width - gp), (rect.Height - gp));
  rd := ((r.Width / 2) - (gp * 5));
  ta := (Arc.StopAngle - Arc.StartAngle);
  incr := GetRadian(((ta) / ((nopart - 1) * (noint + 1))));
  rv := MinimumValue;
  i := 0;
  p := 0;
  while (i <= nopart) do
  begin
    x := (cX + rd * Cos(ca));
    y := (cY + rd * Sin(ca));
    x1 := (cX + (rd - GetWidth / 20) * Cos(ca));
    y1 := (cY + (rd - GetWidth / 20) * Sin(ca));
    FCalibrationValues[p].x := x;
    FCalibrationValues[p].y := y;
    FCalibrationValues[p].x1 := x1;
    FCalibrationValues[p].y1 := y1;
    tx := cX + (rd - GetWidth / 10) * Cos(ca);
    ty := cY - sht + (rd - GetWidth / 10) * Sin(ca);
    FCalibrationValues[p].tx := tx;
    FCalibrationValues[p].ty := ty;
    FCalibrationValues[p].ValueType := cvLarge;
    FCalibrationValues[p].rv := rv;
    Inc(p);
    rv := rv * 10;
    rv := rv + ((MaximumValue - MinimumValue) / (nopart - 1)) * 10;
    rv := rv / 10;
    if (i = (nopart - 1)) then
      break;
    j := 0;
    while (j <= noint) do
    begin
      ca := ca + incr;
      x := cX + rd * Cos(ca);
      y := cY + rd * Sin(ca);
      x1 := cX + (rd - GetWidth/50) * Cos(ca);
      y1 := cY + (rd - GetWidth/50) * Sin(ca);
      k := p;
      FCalibrationValues[k].x := x;
      FCalibrationValues[k].y := y;
      FCalibrationValues[k].x1 := x1;
      FCalibrationValues[k].y1 := y1;
      FCalibrationValues[k].ValueType := cvSmall;
      Inc(p);      
      Inc(J);
    end;
    Inc(I);
  end;
end;

procedure TAdvSmoothGauge.CalculateImageRect;
begin
  fwidth := GetWidth - fxp * 2;
  fheight := GetHeight - fyp * 2;
  frectImg := Makerect(fxp, fyp, fwidth, fheight);
  /////////////////////////
end;

procedure TAdvSmoothGauge.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothGauge.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothGauge.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TAdvSmoothGauge.Changed;
begin
  if (csDestroying in ComponentState) then
    Exit;

  if Assigned(FOnChange) then
    FOnChange(Self);
  Invalidate;
end;

constructor TAdvSmoothGauge.Create(AOwner: TComponent);
begin
  inherited;
  FEqualDimensions := true;
  DoubleBuffered := True;
  FAnimation := True;
  FShowValues := true;
  FAnimationFactor := 5;
  Canvas.Brush.Style := bsClear;
  FFont := TFont.Create;
  FFont.Color := $00FC7C;
  FValueFont := TFont.Create;
  FValueFont.Color := $00FC7C;
  FValueFont.Size := 11;
  FFont.Color := $00FC7C;
  FFont.Size := 14;
  FUnlimitedValue := False;
  FUnlimitedDisplayValue := False;

  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  FValueFont.Name := 'Tahoma';
  {$ENDIF}
  FValuefont.OnChange := FontChanged;
  FFont.OnChange := FontChanged;

  Width := 250;
  Height := 250;
  FDivWidth := 5;
  FSubDivWidth := 2;
  FDivColor := $00FC7C;
  FSubDivColor := $00FC7C;

  FCircleStartValue := 0;
  FCircleEndValue := 360;

  FValueFormat := '000.00';

  fxp := 5;
  fyp := 5;
  fnoOfDivisions := 10;
  fnoOfSubDivisions := 3;
  fminValue := 0;
  fmaxValue := 100;
  fcurrentValue := 0;
  fcurrentDisplayValue := 0;
  FcurrentValueTo := 0;
  FcurrentDisplayValueTo := 0;

  FLogaritchmic := False;
  FLogaritchmicBase := 10;

  FDigit := TAdvSmoothGaugeDigit.Create(Self);
  FDigit.OnChange := DigitChanged;

  FOuterCircle := TAdvSmoothGaugeOuterCircle.Create(Self);
  FOuterCircle.OnChange := OuterCircleChanged;

  FInnerCircle := TAdvSmoothGaugeInnerCircle.Create(Self);
  FInnerCircle.OnChange := InnerCircleChanged;

  FOuterRim := TAdvSmoothGaugeOuterRim.Create(Self);
  FOuterRim.OnChange := OuterRimChanged;

  FNeedle := TAdvSmoothGaugeNeedle.Create(Self);
  FNeedle.OnChange := NeedleChanged;

  FArc := TAdvSmoothGaugeArc.Create(Self);
  FArc.OnChange := ArcChanged;

  FSections := TAdvSmoothGaugeSections.Create(Self);
  FSections.OnChange := SectionsChanged;

  FanimationTimer := TTimer.Create(Self);
  FAnimationTimer.Interval := 1;
  FAnimationTimer.Enabled := true;
  FanimationTimer.OnTimer := AnimateGauge;

  FNeedleType := ntPointer;

  FExtraNeedles := TAdvSmoothGaugeNeedles.Create(Self);
  FExtraNeedles.OnChange := NeedlesChanged;

  FTextRendering := tClearType;

  TabStop := true;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    SetComponentStyle(tsOffice2007Luna);

  CalculateImageRect;
  CalculateCalibration;
end;

//procedure TAdvSmoothGauge.CreateParams(var Params: TCreateParams);
//begin
  { call the create of the params }
//  inherited CreateParams(Params);
//  Params.ExStyle := Params.ExStyle + WS_EX_Transparent;
//  ControlStyle := ControlStyle - [csOpaque] + [csAcceptsControls];
//end;

destructor TAdvSmoothGauge.destroy;
begin
  FArc.Free;
  FDigit.Free;
  FOuterCircle.Free;
  FInnerCircle.Free;
  FFont.Free;
  FValueFont.Free;
  FExtraNeedles.Free;
  FNeedle.Free;
  FOuterRim.Free;
  FanimationTimer.Free;
  FSections.Free;
  inherited;
end;

procedure TAdvSmoothGauge.Draw(gb: TGPGraphics);
var
  drtf, drf, dr: TGPRectF;
  bp, cp, atc, drp, ot: TGPPen;
  bgb: TGPBrush;
  g: TGPGraphics;
  dsr, otb, bs: TGPSolidBrush;
  sa, swa: Double;
  pth: TGPGraphicsPath;
begin
  g := nil;
  cp := nil;
  drp := nil;
  ot := nil;
  bgb := nil;
  dsr := nil;
  bp := nil;
  otb := nil;
  atc := nil;
  try
    // Set Background image to Draw on Canvas
    fbackgroundImg := TGPBitmap.Create(GetWidth, GetHeight);
    /////////////////////////      

    // Create internal Graphics
    g := gb.FromImage(fbackgroundImg);
    g.SetSmoothingMode(SmoothingModeAntiAlias);
    case TextRendering of
      tAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
      tAntiAliasGridFit: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
      tClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    end;
    /////////////////////////

    // Draw Background circle //
    if InnerCircle.Color <> clNone then
    begin
      bgb := TGPSolidBrush.Create(MakeColor(InnerCircle.Opacity, InnerCircle.Color));
      pth := TGPGraphicsPath.Create;
      pth.AddArc(fxp, fyp, fwidth, fheight, CircleStartValue, CircleEndValue - CircleStartValue);
      pth.AddLine(fxp + fwidth / 2, fyp +fheight / 2, fxp +FWidth / 2, fyp + fHEight / 2);
      pth.CloseFigure;
      g.FillPath(bgb, pth);
      pth.Free;
    end;
    /////////////////////////

    // Draw Outline circle //
    if OuterCircle.Color <> clNone then
    begin
      otb := TGPSolidBrush.Create(MakeColor(OuterCircle.Opacity, OuterCircle.Color));
      ot := TGPPen.Create(otb, fWidth * OuterCircle.Width);

      pth := TGPGraphicsPath.Create;
      pth.AddArc(frectImg, CircleStartValue, CircleEndValue - CircleStartValue);
      pth.AddLine(frectImg.X + frectImg.Width / 2, frectImg.Y + frectImg.Height / 2, frectImg.X + frectImg.Width / 2, frectImg.Y + frectImg.Height / 2);
      pth.CloseFigure;
      g.DrawPath(ot, pth);
      pth.Free;
    end;
    /////////////////////////

    // Draw outer rim (Border) //
    if OuterRim.Color <> clNone then
    begin
      drp := TGPPen.Create(MakeColor(OuterRim.Opacity, OuterRim.Color), OuterRim.Width);
      pth := TGPGraphicsPath.Create;
      pth.AddArc(fxp, fyp, fwidth, fheight, CircleStartValue, CircleEndValue - CircleStartValue);
      pth.AddLine(fxp + fwidth / 2, fyp +fheight / 2, fxp +FWidth / 2, fyp + fHEight / 2);
      pth.CloseFigure;
      g.DrawPath(drp, pth);
      pth.Free;
    end;
    /////////////////////////


    /// Draw Sections //
    DrawSections(g);
    ///

    // Draw Calibration //
    DrawCalibration(g);
    /////////////////////////      

    // Draw Arc on circle //
    if Arc.Color <> clNone then
    begin
      cp := TGPPen.Create(MakeColor(Arc.Opacity, Arc.Color), (GetWidth / 40));
      bp := TGPPen.Create(MakeColor(250, clBlack), (GetWidth / 200));
      if Arc.Threshold.ThresholdKind = tkValue then
      begin
        sa := GetAngle(arc.Threshold.StartValue);
        swa := GetAngle(arc.Threshold.EndValue) - sa;
      end
      else
      begin
        sa := arc.Threshold.StartAngle;
        swa := arc.Threshold.SweepAngle;
      end;
      g.DrawArc(cp, Frg, sa, swa);

    end;
    /////////////////////////

    // Draw Arc Threshold //
    if arc.Threshold.Color <> clNone then
    begin
      atc := TGPPen.Create(MakeColor(Arc.Threshold.Opacity, Arc.Threshold.Color), (GetWidth / 50));
      g.DrawArc(atc, FRg, FSta, FSwpa);
    end;
    /////////////////////////


    if Digit.Visible then
    begin
      // Draw Rectangle to display number //
      if Digit.BackGroundColor <> clNone then
      begin
        dr := MakeRect((GetWidth / 2) - (Getwidth / 5), Getheight / 1.23, Getwidth / 2.5, GetHeight / 9);
        drf := MakeRect(dr.X , getheight / 1.21, dr.Width, (GetHeight / 12));
        dsr := TGPSolidBrush.Create(MakeColor(Digit.BackgroundOpacity, Digit.BackGroundColor));
        g.FillRectangle(dsr, dr);
        if UnlimitedValue then
          DisplayNumber(g, Value, drf)
        else if UnlimitedDisplayValue then
          DisplayNumber(g, DisplayValue, drf)
        else
          DisplayNumber(g, Value, drf);
      end;

      bs := TGPSolidBrush.Create(MakeColor(255, Font.Color));
      DrawString(g, fdialText, Font, bs, MakePoint(drtf.X - 100, drtf.Y + drtf.Height / 2), nil, true);
      bs.free;
      /////////////////////////
    end;

    if TabStop and FFocused then
      DrawFocus(g, MakeRect(frectImg.X - 3, frectImg.Y - 3, frectImg.Width + 6, frectImg.Height + 6));

  finally
    if dsr <> nil then
      dsr.Free;
    if bgb <> nil then
      bgb.Free;
    if otb <> nil then
      otb.Free;
    if ot <> nil then
      ot.Free;

    if bp <> nil then
      bp.Free;
    if cp <> nil then
      cp.Free;

    if atc <> nil then
      atc.Free;

    if drp <> nil then
      drp.Free;

    if g <> nil then
      g.Free;

  end;
  // Draw Image
  if fbackgroundImg <> nil then
  begin
    gb.DrawImage(fbackgroundImg, frectImg);
    fbackgroundImg.free;
  end;
end;

procedure TAdvSmoothGauge.Paint;
begin
  DrawGauge(Canvas, Bounds(0, 0, GetWidth, GetHeight));
end;

function TAdvSmoothGauge.PtInPoly(const Points: TArrayOfPointF; X,
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

procedure TAdvSmoothGauge.Resize;
begin
  inherited;
  fxp := GetWidth div 50;
  fyp := GetHeight div 50;
  CalculateImageRect;
  CalculateCalibration;
  CalculateArcThresHold;
  Changed;
end;

procedure TAdvSmoothGauge.SaveToImage(Filename: String; ImageWidth,
  ImageHeight: integer; ImageType: TImageType; ImageQualityPercentage: integer);
var
  img, finalimg: graphics.TBitmap;
  gpimg: TGPImage;
  g: TGPGraphics;
  enc: TEncoderParameters;
begin
  img := nil;
  gpimg := nil;
  g := nil;
  finalimg := nil;
  try
    img := graphics.TBitmap.Create;
    img.Width := GetWidth;
    img.Height := GetHeight;

    DrawGauge(img.Canvas, Bounds(0, 0, ImageWidth, ImageHeight));

    finalimg := graphics.TBitmap.Create;
    finalimg.Width := ImageWidth;
    finalimg.Height := ImageHeight;
    finalimg.Canvas.StretchDraw(Bounds(0, 0, ImageWidth, ImageHeight), img);

    gpimg := TGPImage.Create(CreateStream(finalimg));

    enc := GetEncoderQualityParameters(ImageQualityPercentage);

    gpimg.Save(filename, GetCLSID(ImageType), @enc);

  finally
    gpimg.Free;
    finalimg.Free;
    g.Free;
    img.Free;
  end;
end;

procedure TAdvSmoothGauge.SaveToTheme(FileName: String);
begin

end;

procedure TAdvSmoothGauge.SectionsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothGauge.SetAnimation(const Value: Boolean);
begin
  if FAnimation <> value then
  begin
    FAnimation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetAnimationFactor(const Value: integer);
begin
  if FAnimationFactor <> value then
  begin
    FAnimationFactor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetArc(const Value: TAdvSmoothGaugeArc);
begin
  if Assigned(FArc) then
  begin
    FArc.Assign(Value);
    ArcChanged(Self);
  end;
end;

procedure TAdvSmoothGauge.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if EqualDimensions then
  begin
    if CircleStartValue >= 180 then
      AWidth := AHeight * 2
    else
      AWidth := AHeight;
  end;

  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TAdvSmoothGauge.SetCircleEndValue(const Value: Integer);
begin
  if (FCircleEndValue <> value) and (Value <= 360) and (Value >= CircleStartValue) then
  begin
    FCircleEndValue := Value;
    Width := Width + 1;
    Width := Width - 1;
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetCircleStartValue(const Value: Integer);
begin
  if (FCircleStartValue <> value) and (Value >= 0) and (Value <= CircleEndValue) then
  begin
    FCircleStartValue := Value;
    Width := Width + 1;
    Width := Width - 1;
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetColorTones(ATones: TColorTones);
begin
  FMetroStyle := True;
  TextRendering := tAntiAlias;
  Arc.Color := ATones.Selected.BrushColor;
  InnerCircle.Color := ATones.Background.BrushColor;
  OuterCircle.Color := ATones.Selected.BrushColor;
  OuterRim.Color := ATones.Selected.BrushColor;
  OuterRim.Width := 1;
  Arc.Threshold.Color := ATones.Selected.BrushColor;
  Digit.BackGroundColor := ATones.Background.BrushColor;
  Digit.Color := ATones.Selected.BrushColor;
  DivisionColor := ATones.Selected.BrushColor;
  SubDivisionColor := ATones.Selected.BrushColor;
  Needle.Color := ATones.Selected.BrushColor;
  Needle.InnerCenterColor := ATones.Selected.BrushColor;
  Needle.InnerCenterColorTo := ATones.Selected.BrushColor;
  Needle.OuterCenterColor := ATones.Selected.BrushColor;
  Needle.OuterCenterColorTo := ATones.Selected.BrushColor;

  Needle.ShineColor := clNone;
  ValueFont.Color := ATones.Foreground.BrushColor;
  ValueFont.Name := GetMetroFont;
end;

procedure TAdvSmoothGauge.SetComponentStyle(AStyle: TTMSStyle);
begin
  FMetroStyle := False;
  FTMSStyle := AStyle;
  // TODO : do color settings here
  case AStyle of
    tsOffice2003Blue:
      begin
        Arc.Color := $D68759;
        InnerCircle.Color := $D68759;
        OuterCircle.Color := $00FFD2AF;
        Arc.Threshold.Color := $00FFD2AF;
        Digit.BackGroundColor := $D68759;
        Digit.Color := $962D00;
        DivisionColor := $962D00;
        SubDivisionColor := $962D00;
        Needle.Color := $962D00;
        Needle.InnerCenterColor := $00FFD2AF;
        Needle.OuterCenterColor := $933803;
        Needle.ShineColor := $D68759;
        ValueFont.Color := $962D00;
      end;
    tsOffice2003Silver:
    begin
        Arc.Color := $BDA4A5;
        InnerCircle.Color := $BDA4A5;
        OuterCircle.Color := $00E6D8D8;
        Arc.Threshold.Color := $00E6D8D8;
        Digit.BackGroundColor := $BDA4A5;
        Digit.Color := $947C7C;
        DivisionColor := $947C7C;
        SubDivisionColor := $947C7C;
        Needle.Color := $947C7C;
        Needle.InnerCenterColor := $00E6D8D8;
        Needle.InnerCenterColorTo := $00E6D8D8;
        Needle.OuterCenterColor := $957475;
        Needle.ShineColor := $BDA4A5;
        ValueFont.Color := $947C7C;
    end;
    tsOffice2003Olive:
    begin
        Arc.Color := $82C0AF;
        InnerCircle.Color := $82C0AF;
        OuterCircle.Color := $CFF0EA;
        Arc.Threshold.Color := $CFF0EA;
        Digit.BackGroundColor := $82C0AF;
        Digit.Color := $447A63;
        DivisionColor := $447A63;
        SubDivisionColor := $447A63;
        Needle.Color := $447A63;
        Needle.InnerCenterColor := $447A63;
        Needle.InnerCenterColorTo := $447A63;
        Needle.OuterCenterColor := $447A63;
        Needle.ShineColor := $447A63;
        ValueFont.Color := $447A63;
    end;
    tsOffice2003Classic:
    begin
        Arc.Color := $808080;
        InnerCircle.Color := $808080;
        OuterCircle.Color := $00F2F2F2;
        Arc.Threshold.Color := $00F2F2F2;
        Digit.BackGroundColor := $808080;
        Digit.Color := $808080;
        DivisionColor := $808080;
        SubDivisionColor := $808080;
        Needle.Color := $808080;
        Needle.InnerCenterColor := $00F2F2F2;
        Needle.InnerCenterColorTo := $00F2F2F2;
        Needle.OuterCenterColor := $808080;
        Needle.ShineColor := $808080;
        ValueFont.Color := $808080;
    end;
    tsOffice2007Luna:
    begin
        Arc.Color := $FFEFE3;
        InnerCircle.Color := $00FFD2AF;
        OuterCircle.Color := $00FFD2AF;
        Arc.Threshold.Color := $00FFD2AF;
        Digit.BackGroundColor := $FFEFE3;
        Digit.Color := $962D00;
        DivisionColor := $962D00;
        SubDivisionColor := $962D00;
        Needle.Color := $FFD2AF;
        Needle.InnerCenterColor := $00FFD2AF;
        Needle.InnerCenterColorTo := $00FFD2AF;
        Needle.OuterCenterColor := $FFD1AD;
        Needle.ShineColor := $FFEFE3;
        ValueFont.Color := $962D00;
    end;
    tsOffice2007Obsidian:
    begin
        Arc.Color := $F2F1F0;
        InnerCircle.Color := $5C534C;
        OuterCircle.Color := $5C534C;
        Arc.Threshold.Color := $5C534C;
        Digit.BackGroundColor := $F2F1F0;
        Digit.Color := $F2F1F0;
        DivisionColor := $F2F1F0;
        SubDivisionColor := $F2F1F0;
        Needle.Color := $F2F1F0;
        Needle.InnerCenterColor := $F2F1F0;
        Needle.InnerCenterColorTo := $F2F1F0;
        Needle.OuterCenterColor := $FFD1AD;
        Needle.ShineColor := $F2F1F0;
        ValueFont.Color := $F2F1F0;
    end;
    tsWindowsXP:
    begin
        Arc.Color := $5C534C;
        InnerCircle.Color := clBtnFace;
        OuterCircle.Color := clBtnFace;
        Arc.Threshold.Color := clBtnFace;
        Digit.BackGroundColor := $5C534C;
        Digit.Color := $5C534C;
        DivisionColor := $5C534C;
        SubDivisionColor := $5C534C;
        Needle.Color := $5C534C;
        Needle.InnerCenterColor := $5C534C;
        Needle.InnerCenterColorTo := $5C534C;
        Needle.OuterCenterColor := $FFD1AD;
        Needle.ShineColor := $5C534C;
        ValueFont.Color := $5C534C;
    end;
    tsWhidbey:
    begin
        Arc.Color := $F5F9FA;
        InnerCircle.Color := $EBEEEF;
        OuterCircle.Color := $EBEEEF;
        Arc.Threshold.Color := $5C534C;
        Digit.BackGroundColor := $F5F9FA;
        Digit.Color := $5C534C;
        DivisionColor := $5C534C;
        SubDivisionColor := $5C534C;
        Needle.Color := $5C534C;
        Needle.InnerCenterColor := $F5F9FA;
        Needle.InnerCenterColorTo := $F5F9FA;
        Needle.OuterCenterColor := $FFD1AD;
        Needle.ShineColor := $F5F9FA;
        ValueFont.Color := $5C534C;
    end;
    tsCustom: ;
    tsOffice2007Silver:
    begin
        Arc.Color := $00CAC1BA;
        InnerCircle.Color := $00CAC1BA;
        OuterCircle.Color := $00CAC1BA;
        Arc.Threshold.Color := $F8F7F6;
        Digit.BackGroundColor := $00CAC1BA;
        Digit.Color := $5C534C;
        DivisionColor := $5C534C;
        SubDivisionColor := $5C534C;
        Needle.Color := $5C534C;
        Needle.InnerCenterColor := $F5F9FA;
        Needle.InnerCenterColorTo := $F5F9FA;        
        Needle.OuterCenterColor := $FFD1AD;
        Needle.ShineColor := $F8F7F6;
        ValueFont.Color := $5C534C;
    end;
  tsWindowsvista:
    begin
        Arc.Color := $E0DAD1;
        InnerCircle.Color := $F3F1EA;
        OuterCircle.Color := $ADA9A5;
        Arc.Threshold.Color := $D8CEC2;
        Digit.BackGroundColor := $FFFFFF;
        Digit.Color := clBlack;
        DivisionColor := $B3ADA2;
        SubDivisionColor := $D2D9DE;
        Needle.Color := $989684;
        Needle.InnerCenterColor := $FDF8F1;
        Needle.InnerCenterColorTo := clBlack;
        Needle.OuterCenterColor := $FCEFD5;
        Needle.ShineColor := $D1d1d1;
        ValueFont.Color := clBlack;
    end;
  tsWindows7:
    begin
        Arc.Color := $E0DAD1;
        InnerCircle.Color := $F3F1EA;
        OuterCircle.Color := $ADA9A5;
        Arc.Threshold.Color := $D8CEC2;
        Digit.BackGroundColor := $FFFFFF;
        Digit.Color := clBlack;
        DivisionColor := $B3ADA2;
        SubDivisionColor := $D2D9DE;
        Needle.Color := $989684;
        Needle.InnerCenterColor := $FDF8F1;
        Needle.InnerCenterColorTo := clBlack;
        Needle.OuterCenterColor := $FCEFD5;
        Needle.ShineColor := $D1d1d1;
        ValueFont.Color := clBlack;
    end;
  tsTerminal:
    begin
        Arc.Color := clBtnFace;
        InnerCircle.Color := clBtnFace;
        OuterCircle.Color := clBtnFace;
        Arc.Threshold.Color := clGray;
        Digit.BackGroundColor := clBtnFace;
        Digit.Color := clBlack;
        DivisionColor := clGray;
        SubDivisionColor := clGray;
        Needle.Color := clGray;
        Needle.InnerCenterColor := clGray;
        Needle.InnerCenterColorTo := clGray;
        Needle.OuterCenterColor := clHighLight;
        Needle.ShineColor := clNone;
        ValueFont.Color := clBlack;
    end;
    tsOffice2010Blue:
      begin
        Arc.Color := $DEC1A9;
        InnerCircle.Color := $EAD3BF;
        OuterCircle.Color := $C7B29F;
        Arc.Threshold.Color := $FDF5ED;
        Digit.BackGroundColor := $795C44;
        Digit.Color := $795C44;
        DivisionColor := $5B391E;
        SubDivisionColor := $5B391E;
        Needle.Color := $962D00;
        Needle.InnerCenterColor := $FDF5ED;
        Needle.OuterCenterColor := $DEC1A9;
        Needle.ShineColor := $EAD3BF;
        ValueFont.Color := $962D00;
      end;
       tsOffice2010Silver:
      begin
        Arc.Color := $7C6D66;
        InnerCircle.Color := $D4CFCB;
        OuterCircle.Color := $D2CDC8;
        Arc.Threshold.Color := $EDDBCD;
        Digit.BackGroundColor := $FDFCFB;
        Digit.Color := $7C6D66;
        DivisionColor := $D3CECA;
        SubDivisionColor := $D3CECA;
        Needle.Color := $7C6D66;
        Needle.InnerCenterColor := $FDF5ED;
        Needle.OuterCenterColor := $DEC1A9;
        Needle.ShineColor := $FDFCFB;
        ValueFont.Color := $7C6D66;
      end;
       tsOffice2010Black:
      begin
        Arc.Color := $D7D7D6;
        InnerCircle.Color := $656565;
        OuterCircle.Color := $6F6F6F;
        Arc.Threshold.Color := $F2F2F1;
        Digit.BackGroundColor := $CFCFCF;
        Digit.Color := $2A2A2A;
        DivisionColor := $828282;
        SubDivisionColor := $828282;
        Needle.Color := $656565;
        Needle.InnerCenterColor := $F5F9FA;
        Needle.OuterCenterColor := $656565;;
        Needle.ShineColor := clWhite;
        ValueFont.Color := clWhite;
      end;
      tsWindows8, tsWindows10:
    begin
        Arc.Color := $E4A262;
        InnerCircle.Color := $F7F6F5;
        OuterCircle.Color := $F7EFE8;
        Arc.Threshold.Color := $F9CEA4;
        Digit.BackGroundColor := clWhite;
        Digit.Color := clBlack;
        DivisionColor := clGray;
        SubDivisionColor := clGray;
        Needle.Color := clGray;
        Needle.InnerCenterColor := clGray;
        Needle.InnerCenterColorTo := clGray;
        Needle.OuterCenterColor := $DAA026;
        Needle.ShineColor := clNone;
        ValueFont.Color := clBlack;
    end;
    tsOffice2013White:
    begin
        Arc.Color := $E59D56;
        InnerCircle.Color := clWhite;
        OuterCircle.Color := $FCF0E4;
        Arc.Threshold.Color := $EAB47E;
        Digit.BackGroundColor := clWhite;
        Digit.Color := clBlack;
        DivisionColor := clGray;
        SubDivisionColor := clGray;
        Needle.Color := clGray;
        Needle.InnerCenterColor := clGray;
        Needle.InnerCenterColorTo := clGray;
        Needle.OuterCenterColor := $FF9933;
        Needle.ShineColor := clNone;
        ValueFont.Color := clBlack;
    end;
    tsOffice2013LightGray:
    begin
        Arc.Color := $E59D56;
        InnerCircle.Color := $F6F6F6;
        OuterCircle.Color := $FCF0E4;
        Arc.Threshold.Color := $EAB47E;
        Digit.BackGroundColor := clWhite;
        Digit.Color := clBlack;
        DivisionColor := clGray;
        SubDivisionColor := clGray;
        Needle.Color := clGray;
        Needle.InnerCenterColor := clGray;
        Needle.InnerCenterColorTo := clGray;
        Needle.OuterCenterColor := $FF9933;
        Needle.ShineColor := clNone;
        ValueFont.Color := clBlack;
    end;
    tsOffice2013Gray:
    begin
        Arc.Color := $E59D56;
        InnerCircle.Color := $E5E5E5;
        OuterCircle.Color := $FCF0E4;
        Arc.Threshold.Color := $EAB47E;
        Digit.BackGroundColor := clWhite;
        Digit.Color := clBlack;
        DivisionColor := clGray;
        SubDivisionColor := clGray;
        Needle.Color := clGray;
        Needle.InnerCenterColor := clGray;
        Needle.InnerCenterColorTo := clGray;
        Needle.OuterCenterColor := $FF9933;
        Needle.ShineColor := clNone;
        ValueFont.Color := clBlack;
    end;
    tsOffice2016White:
    begin
        Arc.Color := $F2E1D5;
        InnerCircle.Color := clWhite;
        OuterCircle.Color := $D4D4D4;
        Arc.Threshold.Color := $E3BDA3;
        Digit.BackGroundColor := clWhite;
        Digit.Color := clBlack;
        DivisionColor := clGray;
        SubDivisionColor := clGray;
        Needle.Color := clGray;
        Needle.InnerCenterColor := clGray;
        Needle.InnerCenterColorTo := clGray;
        Needle.OuterCenterColor := $F2D5C2;
        Needle.ShineColor := clNone;
        ValueFont.Color := clBlack;
    end;
    tsOffice2016Gray:
    begin
        Arc.Color := $F2E1D5;
        InnerCircle.Color := $B2B2B2;
        OuterCircle.Color := $444444;
        Arc.Threshold.Color := $E3BDA3;
        Digit.BackGroundColor := clWhite;
        Digit.Color := clBlack;
        DivisionColor := clGray;
        SubDivisionColor := clGray;
        Needle.Color := clGray;
        Needle.InnerCenterColor := clGray;
        Needle.InnerCenterColorTo := clGray;
        Needle.OuterCenterColor := $F2D5C2;
        Needle.ShineColor := clNone;
        ValueFont.Color := clBlack;
    end;
    tsOffice2016Black:
    begin
        Arc.Color := $6A6A6A;
        InnerCircle.Color := $363636;
        OuterCircle.Color := $4E4E4E;
        Arc.Threshold.Color := $444444;
        Digit.BackGroundColor := clWhite;
        Digit.Color := clBlack;
        DivisionColor := clGray;
        SubDivisionColor := clGray;
        Needle.Color := clGray;
        Needle.InnerCenterColor := clGray;
        Needle.InnerCenterColorTo := clGray;
        Needle.OuterCenterColor := $575757;
        Needle.ShineColor := clNone;
        ValueFont.Color := clBlack;
    end;
  end;

  Font.Color := ValueFont.Color;
end;

procedure TAdvSmoothGauge.SetDialText(const Value: string);
begin
  if fdialText <> Value then
  begin
    fdialText := value;
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetDigit(const Value: TAdvSmoothGaugeDigit);
begin
  if FDigit <> value then
  begin
    FDigit.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetDivColor(const Value: TColor);
begin
  if fDivColor <> value then
  begin
    fDivColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetDivWidth(const Value: integer);
begin
  if FDivWidth <> Value then
  begin
    FDivWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetEqualDimensions(const Value: Boolean);
begin
  if FEqualDimensions <> Value then
  begin
    FEqualDimensions := Value;
    SetBounds(Left, Top, Width, Height);
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetExtraNeedles(const Value: TAdvSmoothGaugeNeedles);
begin
  if FExtraNeedles <> Value then
  begin
    FExtraNeedles.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetFont(const Value: TFont);
begin
  if FFont <> Value then
  begin
    fFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetInnerCircle(const Value: TAdvSmoothGaugeInnerCircle);
begin
  if FInnerCircle <> value then
  begin
    FInnerCircle.Assign(Value);
    InnerCircleChanged(Self);
  end;
end;

procedure TAdvSmoothGauge.SetLogarithmic(const Value: Boolean);
begin
  if FLogaritchmic <> Value then
  begin
    FLogaritchmic := Value;
    if Logarithmic then
      MaximumValue := Min(LogN(LogarithmicBase, MaxDouble), MaximumValue);
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetLogarithmicBase(const Value: Integer);
begin
  if FLogaritchmicBase <> Value then
  begin
    FLogaritchmicBase := Max(1, Value);
    if Logarithmic then
      MaximumValue := Min(LogN(LogarithmicBase, MaxDouble), MaximumValue);
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetMaxValue(const Value: Double);
var
  v: Double;
begin
  if (value > fminValue) then
  begin
    v := Value;
    if Logarithmic then
      v := Min(LogN(LogarithmicBase, MaxDouble), v);

    fmaxValue := v;
    if Animation then
    begin
      if (FcurrentValueTo > fmaxValue) and not UnlimitedValue then
        FcurrentValueTo := fmaxValue;

      if (FcurrentDisplayValueTo > fmaxValue) and not UnlimitedDisplayValue then
        FcurrentDisplayValueTo := fmaxValue;
    end
    else
    begin
      if (FcurrentValueTo > fmaxValue) and not UnlimitedValue then
        FcurrentValueTo := fmaxValue;

      if (FcurrentDisplayValueTo > fmaxValue) and not UnlimitedDisplayValue then
        FcurrentDisplayValueTo := fmaxValue;

      FcurrentValueTo := FcurrentValue;
      FcurrentDisplayValueTo := fcurrentDisplayValue;

      DoValueChanged(Self, fcurrentValue);
      DoDisplayValueChanged(Self, fcurrentDisplayValue);
    end;

    if (Arc.Threshold.FCenter > fmaxValue) then
      Arc.Threshold.FCenter := fmaxValue;

    CalculateCalibration;
    CalculateArcThresHold;
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetMinValue(const Value: Double);
begin
  if (value < fmaxValue) then
  begin
    fminValue := value;
    if Animation then
    begin
      if (FcurrentValueTo < fminvalue) and not UnlimitedValue then
        FcurrentValueTo := fminvalue;

      if (FcurrentDisplayValueTo < fminvalue) and not UnlimitedDisplayValue then
        FcurrentDisplayValueTo := fminValue;
    end
    else
    begin
      if (FcurrentValueTo < fminvalue) and not UnlimitedValue then
        FcurrentValueTo := fminvalue;

      if (FcurrentDisplayValueTo < fminvalue) and not UnlimitedDisplayValue then
        FcurrentDisplayValueTo := fminValue;

      FcurrentValueTo := FcurrentValue;
      fcurrentDisplayValueTo := FcurrentDisplayValue;
      DoValueChanged(Self, FcurrentValueTo);
      DoDisplayValueChanged(Self, FCurrentDisplayValueTo);
    end;

    if (Arc.Threshold.FCenter < fminValue) then
      Arc.Threshold.FCenter := fminValue;

    CalculateCalibration;
    CalculateArcThresHold;
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetNeedle(const Value: TAdvSmoothGaugeNeedle);
begin
  if FNeedle <> Value then
  begin
    FNeedle.Assign(Value);
    NeedleChanged(Self);
  end;
end;

procedure TAdvSmoothGauge.SetNeedleType(const Value: TAdvSmoothGaugeNeedleType);
begin
  if FNeedleType <> value then
  begin
    FNeedleType := Value;
    changed;
  end;
end;

procedure TAdvSmoothGauge.SetNoOfDivisions(const Value: Integer);
begin
  if ((value > 1) and (value < 25)) then
  begin
    fnoOfDivisions := value;
    CalculateCalibration;
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetNoOfSubDivisions(const Value: Integer);
begin
  if ((value > 0) and (value <= 10)) then
  begin
    fnoOfSubDivisions := value;
    CalculateCalibration;
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetOuterCircle(const Value: TAdvSmoothGaugeOuterCircle);
begin
  if FOuterCircle <> value then
  begin
    FOuterCircle := Value;
    OuterCircleChanged(Self);
  end;
end;

procedure TAdvSmoothGauge.SetOuterRim(const Value: TAdvSmoothGaugeOuterRim);
begin
  if FOuterRim <> value then
  begin
    FOuterRim.Assign(Value);
    OuterRimChanged(Self);
  end;
end;

procedure TAdvSmoothGauge.SetSections(const Value: TAdvSmoothGaugeSections);
begin
  if FSections <> value then
  begin
    FSections.Assign(Value);
    changed;
  end;
end;

procedure TAdvSmoothGauge.SetShowValues(const Value: Boolean);
begin
  if FShowValues <> Value then
  begin
    FShowValues := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetSubDivColor(const Value: TColor);
begin
  if fSubDivColor <> value then
  begin
    fSubDivColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetSubDivWidth(const Value: integer);
begin
  if FSubDivWidth <> value then
  begin
    FSubDivWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetTextRendering(
  const Value: TAdvSmoothGaugeTextRenderingHint);
begin
  if FTextRendering <> Value then
  begin
    FTextRendering := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetUnlimitedDisplayValue(const Value: Boolean);
begin
  if FUnlimitedDisplayValue <> Value then
  begin
    FUnlimitedDisplayValue := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetUnlimitedValue(const Value: Boolean);
begin
  if FUnlimitedValue <> Value then
  begin
    FUnlimitedValue := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetValue(const Value: Double);
var
  v, vl, vd: Double;
begin
  v := Value;
  vl := Value;
  vd := Value;
  if Logarithmic then
    v := LogN(LogarithmicBase, value);

  if not UnlimitedValue then
    vl := Max(Min(v, MaximumValue), MinimumValue);

  if not UnlimitedDisplayValue then
    vd := Max(Min(v, MaximumValue), MinimumValue);

  if Animation and not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    doAnimation := true;
    FcurrentValueTo := vl;
    FcurrentDisplayValueTo := vd;
  end
  else
  begin
    fcurrentValue := vl;
    FcurrentValueTo := vl;

    fcurrentDisplayValue := vd;
    FcurrentDisplayValueTo := vd;
    DoValueChanged(Self, fcurrentValue);
    DoDisplayValueChanged(Self, fcurrentDisplayValue);
  end;
  Changed;
end;

procedure TAdvSmoothGauge.SetValueFont(const Value: TFont);
begin
  if FValueFont <> Value then
  begin
    FValueFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothGauge.SetValueFormat(const Value: string);
begin
  if FValueFormat <> Value then
  begin
    FValueFormat := Value;
    Changed;
  end;
end;


procedure TAdvSmoothGauge.SetVersion(const Value: string);
begin

end;

procedure TAdvSmoothGauge.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TAdvSmoothGauge.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if TabStop then
    Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS
  else
    Message.Result := 0;
end;

procedure TAdvSmoothGauge.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  {$IFNDEF DELPHI_UNICODE}
  dbl: boolean;
  {$ENDIF}
  p: TPoint;
  i: integer;
begin
  if Assigned(Parent) {and (Fill.ShadowOffset > 0) ?} then
  begin
    DC := Message.DC;
    if DC <> 0 then
    begin
      {$IFNDEF DELPHI_UNICODE}
      dbl := Parent.DoubleBuffered;
      Parent.DoubleBuffered := false;
      {$ENDIF}
      i := SaveDC(DC);
      p := ClientOrigin;
      Windows.ScreenToClient(Parent.Handle, p);
      p.x := -p.x;
      p.y := -p.y;
      MoveWindowOrg(DC, p.x, p.y);
      SendMessage(Parent.Handle, WM_ERASEBKGND, DC, 0);
      SendMessage(Parent.Handle, WM_PAINT, DC, 0);
      if (Parent is TWinCtrl) then
        (Parent as TWinCtrl).PaintCtrls(DC, nil);
      RestoreDC(DC, i);
      {$IFNDEF DELPHI_UNICODE}
      Parent.DoubleBuffered := dbl;
      {$ENDIF}
    end;
  end;

  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      Perform(WM_ERASEBKGND, MemDC, MemDC);
      Message.DC := MemDC;
      WMPaint(Message);
      Message.DC := 0;
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;

function TAdvSmoothGauge.XYToScale(X, Y: integer): Double;
var
  K, S: Double;
  I: integer;
  pts: TArrayOfPointF;
  pt1, pt2, pt3: TGPPointF;
begin
  Result := NaN;

  K := MinimumValue;
  S := (MaximumValue - MinimumValue) / (DivisionCount * (SubDivisionCount + 1));
  
  for I := 0 to (DivisionCount * (SubDivisionCount + 1)) do
  begin
    pts := CalcPosition(K + (S / 2));

    pt1 := pts[0];
    pt2 := MakePoint(GetWidth / 2, GetHeight / 2);
    pts := CalcPosition(K - (S / 2));
    pt3 := pts[0];

    SetLength(pts, 3);
    pts[0] := pt1;
    pts[1] := pt2;
    pts[2] := pt3;
      
    if PtInPoly(pts, X, Y) then
    begin
      if Logarithmic then
        K := Power(LogarithmicBase, K);

      Value := K;
      break;
    end;
    K := K + s;
  end;
end;

procedure TAdvSmoothGauge.DrawPointer(gr: TGPGraphics; col, shncol, shncolto: TColor; cx: Double; cy: Double; value: Double);
type
  TArrayOfPointF = array of TGPPointF;
var
  gp: TGPLinearGradientBrush;
  shpts, pts: TArrayOfPointF;
  ptr: TGPBrush;
  g: TGPGraphics;
  img: TGPBitmap;
  val, rd, a: Double;
begin
  g := nil;
  img := nil;
  gp := nil;
  ptr := nil;
  try
    rd := GetWidth / 2 - (GetWidth * 0.12);
    val := (fMaxValue - fMinValue);

    img := TGPBitmap.Create(getWidth, getHeight);
    g := gr.FromImage(img);
    g.SetSmoothingMode(SmoothingModeAntiAlias);
    case TextRendering of
      tAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
      tAntiAliasGridFit: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
      tClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    end;

    val := ((100 * (value - fMinValue)) / val);
    val := ((Arc.StopAngle - Arc.StartAngle) * val) / 100;
    val := val + Arc.StartAngle;

    a := GetRadian(val);
  
    SetLength(pts, 5);

    pts[0] := MakePoint(cx + rd * Cos(a), cy + rd * Sin(a));
    pts[4] := MakePoint(cx + rd * Cos(a - 0.02), cy + rd * Sin(a - 0.02));
    a := GetRadian(val +  20);
    pts[1] := MakePoint(cx + (GetWidth * 0.09) * Cos(a), cy + (GetWidth * 0.09) * Sin(a));
    pts[2] := MakePoint(cx, cy);
    a := GetRadian((val - 20));
    pts[3] := MakePoint(cx + (GetWidth * 0.09) * Cos(a), cy + (GetWidth * 0.09) * Sin(a));

    if col <> clNone then
    begin
      ptr := TGPSolidBrush.Create(MakeColor(255, col));
      g.FillPolygon(ptr, PGPpointF(pts), Length(pts));
    end;

    SetLength(shpts, 3);
    a := GetRadian(val);
    shpts[0] := MakePoint(cx + rd * Cos(a), cy + rd * Sin(a));

    a := GetRadian((val + 20));
    shpts[1] := MakePoint(cx + (GetWidth * 0.09) * Cos(a), cy + (GetWidth * 0.09) * Sin(a));

    shpts[2] := MakePoint(cx, cy);

    if (shncol <> clNone) and (shncolto <> clNone) then
    begin
      gp := TGPLinearGradientBrush.Create(shpts[0], shpts[2], MakeColor(255, shncol), MakeColor(255, shncolto));
      g.FillPolygon(gp, PGPPointF(shpts), Length(shpts));
    end;

    gr.DrawImage(img, 0, 0);
  finally
    if gp <> nil then
      gp.Free;
    if ptr <> nil then
      ptr.Free;
    if g <> nil then
      g.free;
    if img <> nil then
      img.Free;
  end;
end;

procedure TAdvSmoothGauge.DrawGaugeGDIP(Graphics: TGPGraphics; r: TRect);
var
  g: TGPGraphics;
  I: integer;
begin
  g := Graphics;

  g.SetSmoothingMode(SmoothingModeAntiAlias);
  case TextRendering of
    tAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
    tAntiAliasGridFit: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
    tClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
  end;
  //Background draw//
  Draw(g);
  //Pointer draw//
  for I := 0 to ExtraNeedles.Count - 1 do
    DrawPointer(g, ExtraNeedles[I].Color, ExtraNeedles[I].ShineColor, ExtraNeedles[I].ShineColorTo, fwidth / 2 + fxp, fheight / 2 + fyp, ExtraNeedles[I].Value);

  DrawPointer(g, Needle.Color, Needle.ShineColor, Needle.ShineColorTo, fwidth / 2 + fxp, fheight / 2 + fyp, fcurrentValue);
  DrawCenterPoint(g, MakeRect(fxp, fyp, fwidth, fheight), Round(fwidth / 2) + fxp, Round(fheight / 2) + fyp);
  if not FMetroStyle then
    DrawGloss(g);

end;

procedure TAdvSmoothGauge.DrawGauge(ACanvas: TCanvas; r: TRect);
var
  g: TGPGraphics;
begin
  g := nil;
  try
    g := TGPGraphics.Create(ACanvas.Handle);
    DrawGaugeGDIP(g, r);
  finally
    g.free;
  end;
end;

procedure TAdvSmoothGauge.DrawGloss(g: TGPGraphics);
var
  gb1: TGPLinearGradientBrush;
  gb2: TGPLinearGradientBrush;
  gr: TGPRectF;
begin
  gb1 := nil;
  gb2 := nil;
  try
    if InnerCircle.Gloss.Color <> clNone then
    begin
      gr := MakeRect((fxp + fwidth * 0.1), fyp + (fheight * 0.07), fwidth * 0.8, fheight * 0.7);
      gb1 := TGPLinearGradientBrush.Create(MakeRect(gr.X - 1, gr.Y - 1, gr.Width + 2, gr.Height + 2), MakeColor(InnerCircle.Gloss.Opacity, InnerCircle.Gloss.Color) , MakeColor(0, clBlack), LinearGradientModeVertical);
      g.FillEllipse(gb1, gr);
    end;

    if InnerCircle.Gloss.Color <> clNone then
    begin
      gr := MakeRect(fxp + (fwidth * 0.25), fyp + (fheight * 0.77), fwidth * 0.5, fheight * 0.2);
      gb2 := TGPLinearGradientBrush.Create(MakeRect(gr.X - 1, gr.Y - 1, gr.Width + 2, gr.Height + 2), MakeColor(0, clBlack), MakeColor(InnerCircle.Gloss.Opacity, InnerCircle.Gloss.Color), LinearGradientModeVertical);
      g.FillEllipse(gb2, gr);
    end;
  finally
    gb1.Free;
    gb2.Free;
  end;
end;

procedure TAdvSmoothGauge.DrawCenterPoint(g: TGPGraphics; rect: TGPRectF; cX: Integer;
  cY: Integer);
var
  b1, b2: TGPLinearGradientBrush;
  r: TGPRectF;
  sht: Double;
begin
  b1 := nil;
  b2 := nil;
  try
    if (Needle.OuterCenterColor <> clNone) and (Needle.OuterCenterColorTo <> clNone) then
    begin
      sht := (fWidth / 5);
      r := MakeRect((cX - (sht / 2)), (cY - (sht / 2)), sht, sht);
      b1 := TGPLinearGradientBrush.Create(MakeRect(r.X - 1, r.Y - 2, r.Width + 2, r.Height + 2),MakeColor(Needle.OuterCenterOpacity, Needle.OuterCenterColor) , MakeColor(Needle.OuterCenterOpacity, Needle.OuterCenterColorTo), LinearGradientModeVertical);
      g.FillEllipse(b1, r);
    end;

    if (Needle.InnerCenterColor <> clNone) and (Needle.InnerCenterColorTo <> clNone) then
    begin
      sht := (fWidth / 7);
      r := MakeRect((cX - (sht / 2)), (cY - (sht / 2)), sht, sht);
      b2 := TGPLinearGradientBrush.Create(MakeRect(r.X - 1, r.Y - 2, r.Width + 2, r.Height + 2), MakeColor(Needle.InnerCenterOpacity, Needle.InnerCenterColor), MakeColor(Needle.InnerCenterOpacity, Needle.InnerCenterColorTo), LinearGradientModeForwardDiagonal);
      g.FillEllipse(b2, r);
    end;

  finally
    if b1 <> nil then
      b1.Free;
    if b2 <> nil then
      b2.Free;
  end;
end;

procedure TAdvSmoothGauge.DrawSections(g: TGPGraphics);
var
  i: integer;
  sa, swa: Double;
  path: TGPGraphicsPath;
  b: TGPBrush;
  r, rin: TGPRectF;
  bp: TGPPathGradientBrush;
  colors:array[0..2] of TGPColor;
  positions: array[0..2] of single;
  innermg, outermg: Double;
  gp: Integer;
  ca, y1, y, rd: Double;
  rpos: TGPRectF;
  cy: Double;
  rect: TGPRectF;
begin
  for I := 0 to Sections.Count - 1 do
  begin
    with Sections[I] do
    begin
      if Color <> clNone then
      begin
        innermg := InnerMargin;
        outermg := OuterMargin;

        rect := frectImg;
        cy := Round(fheight / 2) + fyp;
        ca := GetRadian(270);
        gp := Round(GetWidth * 0.01);
        rpos := MakeRect((rect.x + gp), (rect.y + gp), (rect.Width - gp), (rect.Height - gp));
        rd := ((rpos.Width / 2) - (gp * 5));
        y := (cY + rd * Sin(ca));
        y1 := (cY + (rd - GetWidth / 20) * Sin(ca));

        if SectionType = stBorder then
        begin
          innermg := (GetHeight / 2) - y1;
          outermg := y;
        end;

        sa := GetAngle(StartValue);
        swa := GetAngle(EndValue) - sa;
        path := TGPGraphicsPath.Create;
        r := MakeRect(outermg, outermg, GetWidth - (outermg * 2), GetHeight - (outermg * 2));
        rin := MakeRect((GetWidth / 2) - innermg, (GetHeight / 2) - innermg, innermg * 2, innermg * 2);
        if innermg > 0 then
        begin
          path.AddArc(r, sa + swa, -swa);
          path.AddArc(rin, sa, swa);
        end
        else
          path.AddPie(r, sa, swa);


        b := TGPSolidBrush.Create(MakeColor(Opacity, Color));
        g.FillPath(b, path);
        b.Free;

        if Gloss.Color <> clNone then
        begin
          bp := TGPPathGradientBrush.Create;

          colors[0] := MakeColor(0,0,0,0);
          colors[1] := MakeColor(180, clRed);
          colors[2] := MakeColor(180, clGray);

          positions[0] := 0;
          positions[1] := 0.5;
          positions[2] := 1;

          bp.SetInterpolationColors(@colors, @positions, 3);

          g.FillPath(bp, path);
          bp.Free;
        end;
        path.Free;
      end;
    end;
  end;
end;

procedure TAdvSmoothGauge.DrawString(g: TGPGraphics; value: WideString; ft: TFont; Brush: TGPBrush; pt: TGPPointF; stringformat: TGPStringFormat; DialDraw: Boolean);
var
  ftfam: TGPFontFamily;
  ftgp: TGPFont;
  fs: integer;
  sizeRect: TGPRectF;
  digiFRectText: TGPRectF;
begin
    fs := 0;
    if (fsBold in ft.Style) then
      fs := fs + 1;
    if (fsItalic in ft.Style) then
      fs := fs + 2;
    if (fsUnderline in ft.Style) then
      fs := fs + 4;

    ftfam := TGPFontFamily.Create(ft.Name);
    if (ftfam.Status in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      ftfam.Free;
      ftfam := TGPFontFamily.Create('Arial');
    end;
    ftgp := TGPFont.Create(ftfam, ft.Size , fs, UnitPoint);

    if DialDraw then
    begin
      g.MeasureString(value, Length(value), ftgp, pt, stringFormat, sizeRect);
      digiFRectText := MakeRect((GetWidth / 2) - (sizeRect.Width / 2), Getheight / 1.5, sizeRect.Width, sizeRect.Height);
      g.DrawString(value, Length(value), ftgp, digiFRectText, stringformat, Brush);
    end
    else
    begin
      g.DrawString(value, Length(value), ftgp, pt, stringformat, Brush);
    end;

    ftgp.Free;
    ftfam.Free;

end;

procedure TAdvSmoothGauge.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothGauge.DrawCalibration(g: TGPGraphics);
var
  i: Integer;
  strf: TGPStringFormat;
  strp: TGPBrush;
  tp, thp: TGPPen;
  str: String;
  val: Double;
begin
  strp := TGPSolidBrush.Create(MakeColor(255, ValueFont.Color));
  strf := TGPStringFormat.Create(StringFormatFlagsNoClip);
  strf.SetAlignment(StringAlignmentCenter);
  thp := TGPPen.Create(MakeColor(255, DivisionColor), DivisionWidth);
  tp := TGPPen.Create(MakeColor(255, SubDivisionColor), SubDivisionWidth);

  for I := 0 to Length(FCalibrationValues) - 1 do
  begin
    with FCalibrationValues[i] do
    begin
      case ValueType of
        cvSmall:
        begin
          if SubDivisionColor <> clNone then
            g.DrawLine(tp, x, y, x1, y1);
        end;
        cvLarge:
        begin
          if DivisionColor <> clNone then
            g.DrawLine(thp, x, y, x1, y1);

          if ShowValues then
          begin
            val := rv;
            if Logarithmic then
              val := Power(LogarithmicBase, val);

            str := floattostr(val);
            if Assigned(OnGetValueText) then
              OnGetValueText(Self, val, str);

            DrawString(g, str, ValueFont, strp, MakePoint(tx, ty), strf, false);
          end;
        end;
      end;
    end;
  end;

  tp.Free;
  thp.Free;
  strp.Free;
  strf.free;
end;

function TAdvSmoothGauge.GetAngle(Value: Double): Double;
begin
  result := ((Arc.StopAngle - Arc.StartAngle) / (MaximumValue - MinimumValue)) * (Value - MinimumValue);
  Result := Result + Arc.StartAngle;
end;

function TAdvSmoothGauge.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothGauge.GetDisplayValue: Double;
begin
  Result := fcurrentDisplayValue;
  if Logarithmic then
    Result := Power(LogarithmicBase, Result);
end;

function TAdvSmoothGauge.GetHeight: Integer;
begin
  if CircleStartValue >= 180 then
    Result := Height * 2
  else
    Result := Height;
end;

function TAdvSmoothGauge.GetRadian(theta: Double): Double;
begin
  Result := theta * (PI / 180);
end;

function TAdvSmoothGauge.GetThemeID: String;
begin
  Result := ClassName;
end;

function TAdvSmoothGauge.GetValue: Double;
begin
  Result := fcurrentValue;
  if Logarithmic then
    Result := Power(LogarithmicBase, Result);
end;

function TAdvSmoothGauge.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothGauge.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TAdvSmoothGauge.GetWidth: Integer;
begin
  Result := Width;
end;

procedure TAdvSmoothGauge.DigitChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothGauge.DisplayNumber(g: TGPGraphics; number: Double; drect: TGPRectF);
var
  num: String;
  i: Integer;
  drawDPS: Boolean;
  shift: Double;
  numch: char;
begin
  if Assigned(OnDisplayValue) then
    FOnDisplayValue(Self, number);

  num := FormatFloat(ValueFormat, number);
  shift := 0;
  for i := 1 to length(num) do
  begin
    numch := num[i];
    case numch of
    '.',',': shift := shift + 2 * (fWidth / 250);
    '-': shift := shift + 15 * (GetWidth / 250);
    '0'..'9': shift := shift + 15 * (GetWidth / 250);
    end;
  end;

  case Digit.Alignment of
    taLeftJustify: shift := 0;
    taRightJustify: shift := (drect.Width - shift);
    taCenter: shift := (drect.Width - shift) / 2;
  end;

  for i := 1 to length(num) do
  begin
    numch := num[i];
    drawDPS := (i < length(num) - 1) and (num[i+1] = DecimalSeparator);
    case numch of
    '.',',': shift := shift + 2 * (fWidth / 250);
    '-':
      begin
        DrawDigit(g, -1, MakePoint((drect.X + shift), drect.Y), drawDPS, Round(drect.Height));
        shift := shift + 15 * (GetWidth / 250);
      end;
    '0'..'9':
      begin
        DrawDigit(g, ord(numch) - ord('0') , MakePoint((drect.X + shift), drect.Y), drawDPS, Round(drect.Height));
        shift := shift + 15 * (GetWidth / 250);
      end;
    end;
  end;
end;

procedure TAdvSmoothGauge.DoDisplayValueChanged(Sender: TObject; value: Double);
begin
  if Logarithmic then
    value := Power(LogarithmicBase, value);

  if Assigned(OnDisplayValueChanged) then
    OnDisplayValueChanged(Sender, value);
end;

procedure TAdvSmoothGauge.DoEnter;
begin
  inherited;
  FFocused := true;
  Changed;
end;

procedure TAdvSmoothGauge.DoExit;
begin
  inherited;
  FFocused := false;
  Changed;
end;

procedure TAdvSmoothGauge.DoValueChanged(Sender: TObject; value: Double);
begin
  if Logarithmic then
    value := Power(LogarithmicBase, value);

  if Assigned(OnValueChanged) then
    OnValueChanged(Sender, value);
end;

procedure TAdvSmoothGauge.DrawDigit(g: TGPGraphics; number: Integer; position: TGPPointF; dp: Boolean; h: integer);
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
  fp: TGPPen;
  ot: TGPPen;
  w: Double;
  bo, bf: TGPBrush;
begin
  ot := nil;
  fp := nil;
  try
    w := 10 * (h / 13);
    ot := TGPPen.Create(MakeColor(40, InnerCircle.Color));
    fp := TGPPen.Create(MakeColor(255, Digit.Color));

    if Digit.Color <> clNone then
    begin
      SetLength(segmentA, 5);
      segmentA[0] := MakePoint((position.X + GetX(2.8, w)), (position.Y + GetY(1, h)));
      segmentA[1] := MakePoint((position.X + GetX(10, w)), (position.Y + GetY(1, h)));
      segmentA[2] := MakePoint((position.X + GetX(8.8, w)), (position.Y + GetY(2, h)));
      segmentA[3] := MakePoint((position.X + GetX(3.8, w)), (position.Y + GetY(2, h)));
      segmentA[4] := segmentA[0];

      SetLength(segmentB, 5);
      segmentB[0] := MakePoint((position.X + GetX(10, w)), (position.Y + GetY(1.4, h)));
      segmentB[1] := MakePoint((position.X + GetX(9.3, w)), (position.Y + GetY(6.8, h)));
      segmentB[2] := MakePoint((position.X + GetX(8.4, w)), (position.Y + GetY(6.4, h)));
      segmentB[3] := MakePoint((position.X + GetX(9, w)), (position.Y + GetY(2.2, h)));
      segmentB[4] := segmentB[0];

      SetLength(segmentC, 5);
      segmentC[0] := MakePoint((position.X + GetX(9.2, w)), (position.Y + GetY(7.2, h)));
      segmentC[1] := MakePoint((position.X + GetX(8.7, w)), (position.Y + GetY(12.7, h)));
      segmentC[2] := MakePoint((position.X + GetX(7.6, w)), (position.Y + GetY(11.9, h)));
      segmentC[3] := MakePoint((position.X + GetX(8.2, w)), (position.Y + GetY(7.7, h)));
      segmentC[4] := segmentC[0];

      SetLength(segmentD, 5);
      segmentD[0] := MakePoint((position.X + GetX(7.4, w)), (position.Y + GetY(12.1, h)));
      segmentD[1] := MakePoint((position.X + GetX(8.4, w)), (position.Y + GetY(13, h)));
      segmentD[2] := MakePoint((position.X + GetX(1.3, w)), (position.Y + GetY(13, h)));
      segmentD[3] := MakePoint((position.X + GetX(2.2, w)), (position.Y + GetY(12.1, h)));
      segmentD[4] := segmentD[0];

      SetLength(segmentE, 5);
      segmentE[0] := MakePoint((position.X + GetX(2.2, w)), (position.Y + GetY(11.8, h)));
      segmentE[1] := MakePoint((position.X + GetX(1, w)), (position.Y + GetY(12.7, h)));
      segmentE[2] := MakePoint((position.X + GetX(1.7, w)), (position.Y + GetY(7.2, h)));
      segmentE[3] := MakePoint((position.X + GetX(2.8, w)), (position.Y + GetY(7.7, h)));
      segmentE[4] := segmentE[0];

      SetLength(segmentF, 5);
      segmentF[0] := MakePoint((position.X + GetX(3, w)), (position.Y + GetY(6.4, h)));
      segmentF[1] := MakePoint((position.X + GetX(1.8, w)), (position.Y + GetY(6.8, h)));
      segmentF[2] := MakePoint((position.X + GetX(2.6, w)), (position.Y + GetY(1.3, h)));
      segmentF[3] := MakePoint((position.X + GetX(3.6, w)), (position.Y + GetY(2.2, h)));
      segmentF[4] := segmentF[0];

      SetLength(segmentG, 7);
      segmentG[0] := MakePoint((position.X + GetX(2, w)), (position.Y + GetY(7, h)));
      segmentG[1] := MakePoint((position.X + GetX(3.1, w)), (position.Y + GetY(6.5, h)));
      segmentG[2] := MakePoint((position.X + GetX(8.3, w)), (position.Y + GetY(6.5, h)));
      segmentG[3] := MakePoint((position.X + GetX(9, w)), (position.Y + GetY(7, h)));
      segmentG[4] := MakePoint((position.X + GetX(8.2, w)), (position.Y + GetY(7.5, h)));
      segmentG[5] := MakePoint((position.X + GetX(2.9, w)), (position.Y + GetY(7.5, h)));
      segmentG[6] := segmentG[0];

      bo := ot.GetBrush;
      g.FillPolygon(bo, PGPpointF(segmentA), Length(segmentA));
      g.FillPolygon(bo, PGPpointF(segmentB), Length(segmentB));
      g.FillPolygon(bo, PGPpointF(segmentC), Length(segmentC));
      g.FillPolygon(bo, PGPpointF(segmentD), Length(segmentD));
      g.FillPolygon(bo, PGPpointF(segmentE), Length(segmentE));
      g.FillPolygon(bo, PGPpointF(segmentF), Length(segmentF));
      g.FillPolygon(bo, PGPpointF(segmentG), Length(segmentG));
      bo.free;

      bf := fp.GetBrush;
      if IsNumberAvailable(number, [0, 2, 3, 5, 6, 7, 8, 9]) then
        g.FillPolygon(bf, PGPpointF(segmentA), Length(SegmentA));
      if IsNumberAvailable(number, [0, 1, 2, 3, 4, 7, 8, 9]) then
        g.FillPolygon(bf, PGPpointF(segmentB), Length(SegmentB));
      if IsNumberAvailable(number, [0, 1, 3, 4, 5, 6, 7, 8, 9]) then
        g.FillPolygon(bf, PGPpointF(segmentC), Length(SegmentC));
      if IsNumberAvailable(number, [0, 2, 3, 5, 6, 8, 9]) then
        g.FillPolygon(bf, PGPpointF(segmentD), Length(SegmentD));
      if IsNumberAvailable(number, [0, 2, 6, 8]) then
        g.FillPolygon(bf, PGPpointF(segmentE), Length(SegmentE));
      if IsNumberAvailable(number, [0, 4, 5, 6, 7, 8, 9]) then
        g.FillPolygon(bf, PGPpointF(segmentF), Length(SegmentF));
      if IsNumberAvailable(number, [2, 3, 4, 5, 6, 8, 9, -1]) then
        g.FillPolygon(bf, PGPpointF(segmentG), Length(SegmentG));
      if dp then
        g.FillEllipse(bf, MakeRect((position.X + GetX(10, w)),(position.Y + GetY(12, h)), (w / 7), (w / 7)));

      bf.free;
    end;
  finally
    fp.Free;
    ot.Free;
  end;
end;

function TAdvSmoothGauge.GetX(x: Double; width: Double): Double;
begin
  Result := x * width / 12;
end;

function TAdvSmoothGauge.GetY(y: Double; height: Double): Double;
begin
  Result := y * height / 15;
end;

procedure TAdvSmoothGauge.InnerCircleChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothGauge.IsNumberAvailable(Number: integer; const listOfNumbers: array of Integer): Boolean;
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

procedure TAdvSmoothGauge.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if not Enabled or not TabStop then
    Exit;
  case Key of
    VK_LEFT, VK_DOWN: Value := Value - 1;
    VK_RIGHT, VK_UP: Value := Value + 1;
    VK_NEXT: Value := Value - ((MaximumValue-MinimumValue)/DivisionCount);
    VK_PRIOR: Value := Value + ((MaximumValue-MinimumValue)/DivisionCount);
    VK_END: Value := MaximumValue;
    VK_HOME: Value := MinimumValue;
  end;
end;

procedure TAdvSmoothGauge.Loaded;
begin
  inherited;
  CalculateImageRect;
  CalculateCalibration;
  CalculateArcThresHold;
end;

procedure TAdvSmoothGauge.LoadFromTheme(FileName: String);
begin

end;

procedure TAdvSmoothGauge.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

end;

procedure TAdvSmoothGauge.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TAdvSmoothGauge.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if TabStop and Enabled then
    XYToScale(X, Y);
end;

procedure TAdvSmoothGauge.NeedleChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothGauge.NeedlesChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothGauge.OuterCircleChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothGauge.OuterRimChanged(Sender: TObject);
begin
  Changed;
end;

{ TAdvSmoothGaugeDigit }

procedure TAdvSmoothGaugeDigit.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothGaugeDigit) then
  begin
    FColor := (Source as TAdvSmoothGaugeDigit).Color;
    FBackGroundColor := (Source as TAdvSmoothGaugeDigit).BackGroundColor;
    FBackGroundOpacity := (Source as TAdvSmoothGaugeDigit).BackGroundOpacity;
    FVisible := (Source as TAdvSmoothGaugeDigit).Visible;
    FAlignment := (Source as TAdvSmoothGaugeDigit).Alignment;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeDigit.Changed;
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

constructor TAdvSmoothGaugeDigit.Create(AOwner: TAdvSmoothGauge);
begin
  FOwner := AOwner;
  fColor := $00FC7C;
  fBackgroundColor := $808080;
  fBackgroundOpacity := 30;
  FVisible := true;
  FAlignment := taCenter;
end;

procedure TAdvSmoothGaugeDigit.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> value then
  begin
    FAlignment := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeDigit.SetBackGroundColor(const Value: TColor);
begin
  if FBackGroundColor <> Value then
  begin
    fBackgroundColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeDigit.SetBackGroundOpacity(const Value: integer);
begin
  if fBackgroundOpacity <> Value then
  begin
    fBackgroundOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeDigit.SetColor(const Value: TColor);
begin
  if fColor <> value then
  begin
    fColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeDigit.SetVisible(const Value: Boolean);
begin
  if FVisible <> value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TAdvSmoothGaugeOuterCircle }

procedure TAdvSmoothGaugeOuterCircle.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothGaugeOuterCircle) then
  begin
    FColor := (Source as TAdvSmoothGaugeOuterCircle).Color;
    FWidth := (Source as TAdvSmoothGaugeOuterCircle).Width;
    FOpacity := (Source as TAdvSmoothGaugeOuterCircle).Opacity;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeOuterCircle.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TAdvSmoothGaugeOuterCircle.Create(AOwner: TAdvSmoothGauge);
begin
  FOwner := AOwner;
  FColor := $908070;
  fOpacity := 100;
  FWidth := 0.03;
end;

procedure TAdvSmoothGaugeOuterCircle.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeOuterCircle.SetOpacity(const Value: integer);
begin
  if FOpacity <> Value then
  begin
    fOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeOuterCircle.SetWidth(const Value: Double);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    changed;
  end;
end;

{ TAdvSmoothGaugeInnerCircle }

procedure TAdvSmoothGaugeInnerCircle.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothGaugeInnerCircle) then
  begin
    Color := (Source as TAdvSmoothGaugeInnerCircle).Color;
    Opacity := (Source as TAdvSmoothGaugeInnerCircle).Opacity;
    Gloss.Assign((Source as TAdvSmoothGaugeInnerCircle).Gloss);
  end;
end;

procedure TAdvSmoothGaugeInnerCircle.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TAdvSmoothGaugeInnerCircle.Create(AOwner: TAdvSmoothGauge);
begin
  FOwner := AOwner;
  FColor := clBlue;
  fOpacity := 150;
  FGloss := TAdvSmoothGaugeGloss.Create(AOwner);
  FGloss.OnChange := GlossChanged;
end;


destructor TAdvSmoothGaugeInnerCircle.Destroy;
begin
  FGloss.Free;
  inherited;
end;

procedure TAdvSmoothGaugeInnerCircle.GlossChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothGaugeInnerCircle.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeInnerCircle.SetGloss(const Value: TAdvSmoothGaugeGloss);
begin
  if FGloss <> value then
  begin
    FGloss.Assign(Value);
    Changed;
  end;
end;


procedure TAdvSmoothGaugeInnerCircle.SetOpacity(const Value: integer);
begin
  if FOpacity <> Value then
  begin
    fOpacity := Value;
    Changed;
  end;
end;

{ TAdvSmoothGaugeGloss }

procedure TAdvSmoothGaugeGloss.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothGaugeGloss) then
  begin
    FColor := (Source as TAdvSmoothGaugeGloss).Color;
    FOpacity := (Source as TAdvSmoothGaugeGloss).Opacity;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeGloss.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TAdvSmoothGaugeGloss.Create(AOwner: TAdvSmoothGauge);
begin
  FOwner := AOwner;
  FColor := clWhite;
  FOpacity := 72;
end;

procedure TAdvSmoothGaugeGloss.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeGloss.SetOpacity(const Value: integer);
begin
  if FOpacity <> value then
  begin
    FOpacity := value;
    Changed;
  end;
end;

{ TAdvSmoothGaugeOuterRim }

procedure TAdvSmoothGaugeOuterRim.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothGaugeOuterRim) then
  begin
    FColor := (Source as TAdvSmoothGaugeOuterRim).Color;
    FOpacity := (Source as TAdvSmoothGaugeOuterRim).Opacity;
    FWidth := (Source as TAdvSmoothGaugeOuterRim).Width;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeOuterRim.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TAdvSmoothGaugeOuterRim.Create(AOwner: TAdvSmoothGauge);
begin
  FOwner := AOwner;
  FColor := $908070;
  FWidth := 2;
  fOpacity := 255;
end;

procedure TAdvSmoothGaugeOuterRim.SetColor(const Value: TColor);
begin
  if FColor <> value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeOuterRim.SetOpacity(const Value: integer);
begin
  if Fopacity <> Value then
  begin
    fOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeOuterRim.SetWidth(const Value: integer);
begin
  if FWidth <> value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

{ TAdvSmoothGaugeNeedle }

procedure TAdvSmoothGaugeNeedle.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothGaugeNeedle) then
  begin
    FColor := (Source as TAdvSmoothGaugeNeedle).Color;
    FShineColor := (Source as TAdvSmoothGaugeNeedle).ShineColor;
    FShineColorTo := (Source as TAdvSmoothGaugeNeedle).ShineColorTo;
    FOuterCenterColor := (Source as TAdvSmoothGaugeNeedle).OuterCenterColor;
    FOuterCenterColorTo := (Source as TAdvSmoothGaugeNeedle).FOuterCenterColorTo;
    FOuterCenterOpacity := (Source as TAdvSmoothGaugeNeedle).OuterCenterOpacity;
    FInnerCenterColor := (Source as TAdvSmoothGaugeNeedle).InnerCenterColor;
    FInnerCenterColorTo := (Source as TAdvSmoothGaugeNeedle).InnerCenterColorTo;
    FInnerCenterOpacity := (Source as TAdvSmoothGaugeNeedle).InnerCenterOpacity;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeNeedle.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TAdvSmoothGaugeNeedle.Create(AOwner: TAdvSmoothGauge);
begin
  FOwner := AOwner;
  FColor := $00FC7C;
  OuterCenterColor := clBlue;
  OuterCenterColorTo := clBlue;
  OuterCenterOpacity := 100;
  InnerCenterColor := $8B0000;
  InnerCenterColorTo := $8B0000;
  InnerCenterOpacity := 255;
  ShineColor := $00FC7C;
  ShineColorTo := $D3D3D3;
end;

procedure TAdvSmoothGaugeNeedle.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeNeedle.SetInnerCenterColor(const Value: TColor);
begin
  if FInnerCenterColor <> value then
  begin
    FInnerCenterColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeNeedle.SetInnerCenterColorTo(const Value: TColor);
begin
  if FInnerCenterColorTo <> Value then
  begin
    FInnerCenterColorTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeNeedle.SetInnerCenterOpacity(const Value: integer);
begin
  if FInnerCenterOpacity <> value then
  begin
    FInnerCenterOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeNeedle.SetOuterCenterColor(const Value: TColor);
begin
  if FOuterCenterColor <> value then
  begin
    FOuterCenterColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeNeedle.SetOuterCenterColorTo(const Value: TColor);
begin
  if FouterCenterColorTo <> value then
  begin
    FOuterCenterColorTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeNeedle.SetOuterCenterOpacity(const Value: integer);
begin
  if FouterCenterOpacity <> value then
  begin
    FOuterCenterOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeNeedle.SetShineColor(const Value: TColor);
begin
  if FShineColor <> value then
  begin
    FShineColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeNeedle.SetShineColorTo(const Value: TColor);
begin
  if FShineColorTo <> value then
  begin
    FShineColorTo := Value;
    Changed;
  end;
end;

{ TAdvSmoothGaugeThreshold }

procedure TAdvSmoothGaugeThreshold.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothGaugeThreshold) then
  begin
    FColor := (Source as TAdvSmoothGaugeThreshold).Color;
    FOpacity := (Source as TAdvSmoothGaugeThreshold).Opacity;
    FStartAngle := (Source as TAdvSmoothGaugeThreshold).StartAngle;
    FSweepAngle := (Source as TAdvSmoothGaugeThreshold).SweepAngle;
    FCenter := (Source as TAdvSmoothGaugeThreshold).Center;
    FSpan := (Source as TAdvSmoothGaugeThreshold).Span;
    FStartValue := (Source as TAdvSmoothGaugeThreshold).StartValue;
    FEndValue := (Source as TAdvSmoothGaugeThreshold).EndValue;
    FThresHoldKind := (Source as TAdvSmoothGaugeThreshold).ThresholdKind;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeThreshold.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TAdvSmoothGaugeThreshold.Create(AOwner: TAdvSmoothGauge);
begin
  FOwner := AOwner;
  FOpacity := 200;
  FColor := $00FC7C;
  FStartAngle := 135;
  FSweepAngle := 270;
  FCenter := 25;
  FSpan := 25;
  if (csDesigning in fOwner.ComponentState) then
  begin
    FStartValue := 0;
    FEndValue := 100;
  end;
  FThresHoldKind := tkAngle;
end;

function TAdvSmoothGaugeThreshold.GetStartAngle: integer;
begin
  Result := Max(FStartAngle, FOwner.CircleStartValue);
end;

function TAdvSmoothGaugeThreshold.GetSweepAngle: integer;
begin
  Result := Min(FSweepAngle, FOwner.CircleEndValue - FOwner.CircleStartValue);
end;

procedure TAdvSmoothGaugeThreshold.SetCenter(const Value: Double);
begin
  if ((value >= FOwner.fminValue) and (value <= FOwner.fmaxValue)) then
  begin
    fCenter := value;
    FOwner.CalculateArcThresHold;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeThreshold.SetColor(const Value: TColor);
begin
  if FColor <> value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeThreshold.SetEndValue(const Value: Double);
begin
  if FEndValue <> Value then
  begin
    FEndValue := Value;
    FOwner.CalculateArcThresHold;    
    Changed;
  end;
end;

procedure TAdvSmoothGaugeThreshold.SetOpacity(const Value: Integer);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeThreshold.SetSpan(const Value: integer);
begin
  if ((value >= 0) and (value <= 100)) then
  begin
    fSpan := value;
    FOwner.CalculateArcThresHold;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeThreshold.SetStartAngle(const Value: integer);
begin
  if FStartAngle <> value then
  begin
    FStartAngle := Max(Value, FOwner.CircleStartValue);
    FOwner.CalculateArcThresHold;    
    Changed;
  end;
end;

procedure TAdvSmoothGaugeThreshold.SetStartValue(const Value: Double);
begin
  if FStartValue <> value then
  begin
    FStartValue := Value;
    FOwner.CalculateArcThresHold;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeThreshold.SetSweepAngle(const Value: integer);
begin
  if FSweepAngle <> value then
  begin
    FSweepAngle := Value;
    FOwner.CalculateArcThresHold;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeThreshold.SetThresHoldKind(
  const Value: TAdvSmoothGaugeThresholdKind);
begin
  if FThresHoldKind <> value then
  begin
    FThresHoldKind := Value;
    FOwner.CalculateArcThresHold;
    Changed;
  end;
end;

{ TAdvSmoothGaugeArc }

procedure TAdvSmoothGaugeArc.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothGaugeArc) then
  begin
    FColor := (Source as TAdvSmoothGaugeArc).Color;
    FWidth := (Source as TAdvSmoothGaugeArc).Width;
    FOpacity := (Source as TAdvSmoothGaugeArc).Opacity;
    FStartAngle := (Source as TAdvSmoothGaugeArc).StartAngle;
    FStopAngle := (Source as TAdvSmoothGaugeArc).StopAngle;
    FThreshold.Assign((Source as TAdvSmoothGaugeArc).Threshold);
    Changed;
  end;
end;

procedure TAdvSmoothGaugeArc.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TAdvSmoothGaugeArc.Create(AOwner: TAdvSmoothGauge);
begin
  FOwner := AOwner;
  FWidth := 0.03;
  FColor := $8B0000;
  FOpacity := 200;
  FStartAngle := 135;
  FStopAngle := 405;
  FThreshold := TAdvSmoothGaugeThreshold.Create(AOwner);
  FThreshold.OnChange := ThresholdChanged;
end;

destructor TAdvSmoothGaugeArc.Destroy;
begin
  FThreshold.Free;
  inherited;
end;

function TAdvSmoothGaugeArc.GetStartAngle: integer;
begin
  Result := Max(FStartAngle, FOwner.CircleStartValue);
end;

function TAdvSmoothGaugeArc.GetStopAngle: integer;
begin
  if FStopAngle >= FOwner.CircleEndValue - FOwner.CircleStartValue + StartAngle then
    Result := FOwner.CircleEndValue  - FOwner.CircleStartValue + StartAngle
  else
    Result := FStopAngle;
end;

procedure TAdvSmoothGaugeArc.SetColor(const Value: TColor);
begin
  if FColor <> value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeArc.SetOpacity(const Value: integer);
begin
  if FOpacity <> value then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeArc.SetStartAngle(const Value: integer);
begin
  if FStartAngle <> value then
  begin
    FStartAngle := Max(Value, FOwner.CircleStartValue);
    FOwner.CalculateArcThresHold;
    FOwner.CalculateCalibration;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeArc.SetStopAngle(const Value: integer);
begin
  if FStopAngle <> value then
  begin
    FStopAngle := Value;
    FOwner.CalculateArcThresHold;
    FOwner.CalculateCalibration;    
    Changed;
  end;
end;

procedure TAdvSmoothGaugeArc.SetThreshold(const Value: TAdvSmoothGaugeThreshold);
begin
  if FThreshold <> Value then
  begin
    FThreshold.Assign(Value);
    ThresholdChanged(Self);
  end;
end;

procedure TAdvSmoothGaugeArc.SetWidth(const Value: Double);
begin
  if FWidth <> value then
  begin
    FWidth := Value;
    FOwner.CalculateArcThresHold;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeArc.ThresholdChanged(Sender: TObject);
begin
  Changed;
end;

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

{ TAdvSmoothGaugeSection }

procedure TAdvSmoothGaugeSection.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothGaugeSection) then
  begin
    FColor := (Source as TAdvSmoothGaugeSection).Color;
    fOpacity := (Source as TAdvSmoothGaugeSection).Opacity;
    FGloss.Assign((Source as TAdvSmoothGaugeSection).Gloss);
    FStartValue := (Source as TAdvSmoothGaugeSection).StartValue;
    FEndValue := (Source as TAdvSmoothGaugeSection).EndValue;
    FOuterMargin := (Source as TAdvSmoothGaugeSection).OuterMargin;
    FInnerMargin := (Source as TAdvSmoothGaugeSection).InnerMargin;
    FSectionType := (Source as TAdvSmoothGaugeSection).SectionType;
  end;
end;

procedure TAdvSmoothGaugeSection.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothGaugeSection.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TAdvSmoothGaugeSections).FOwner;
  FColor := clBlue;
  fOpacity := 150;
  FGloss := TAdvSmoothGaugeGloss.Create(FOwner);
  FGloss.OnChange := GlossChanged;
  FStartValue := 0;
  FEndValue := 100;
  FOuterMargin := 20;
  FInnerMargin := 0;
  FSectionType := stCustomMargin;
  Changed;
end;

destructor TAdvSmoothGaugeSection.Destroy;
begin
  FGloss.Free;
  Changed;
  inherited;
end;

procedure TAdvSmoothGaugeSection.GlossChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothGaugeSection.SetColor(const Value: TColor);
begin
  if FColor <> value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeSection.SetEndValue(const Value: Double);
begin
  if FEndValue <> value then
  begin
    FEndValue := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeSection.SetGloss(const Value: TAdvSmoothGaugeGloss);
begin
  if FGloss <> value then
  begin
    FGloss.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothGaugeSection.SetInnerMargin(const Value: integer);
begin
  if FInnerMargin <> value then
  begin
    FInnerMargin := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeSection.SetOpacity(const Value: integer);
begin
  if fOpacity <> value then
  begin
    fOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeSection.SetOuterMargin(const Value: integer);
begin
  if FOuterMargin <> value then
  begin
    FOuterMargin := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeSection.SetSectionType(
  const Value: TAdvSmoothGaugeSectionType);
begin
  if FSectionType <> Value then
  begin
    FSectionType := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeSection.SetStartValue(const Value: Double);
begin
  if FStartValue <> Value then
  begin
    FStartValue := Value;
    Changed;
  end;
end;

{ TAdvSmoothGaugeSections }

function TAdvSmoothGaugeSections.Add: TAdvSmoothGaugeSection;
begin
  result := TAdvSmoothGaugeSection(inherited Add);
end;

constructor TAdvSmoothGaugeSections.Create(AOwner: TAdvSmoothGauge);
begin
  inherited Create(TAdvSmoothGaugeSection);
  FOwner := AOwner;
end;

procedure TAdvSmoothGaugeSections.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TAdvSmoothGaugeSections.GetItem(
  Index: Integer): TAdvSmoothGaugeSection;
begin
  Result := TAdvSmoothGaugeSection(inherited Items[Index]);
end;

function TAdvSmoothGaugeSections.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvSmoothGaugeSections.Insert(Index: Integer): TAdvSmoothGaugeSection;
begin
  Result := TAdvSmoothGaugeSection(inherited Insert(Index));
end;

procedure TAdvSmoothGaugeSections.SetItem(Index: Integer;
  const Value: TAdvSmoothGaugeSection);
begin
  inherited Items[Index] := Value;
end;

{ TAdvSmoothGaugeNeedles }

function TAdvSmoothGaugeNeedles.Add: TAdvSmoothGaugeNeedleItem;
begin
  result := TAdvSmoothGaugeNeedleItem(inherited Add);
end;

constructor TAdvSmoothGaugeNeedles.Create(AOwner: TAdvSmoothGauge);
begin
  inherited Create(TAdvSmoothGaugeNeedleItem);
  FOwner := AOwner;
end;

procedure TAdvSmoothGaugeNeedles.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TAdvSmoothGaugeNeedles.GetItem(
  Index: Integer): TAdvSmoothGaugeNeedleItem;
begin
  Result := TAdvSmoothGaugeNeedleItem(inherited Items[Index]);
end;

function TAdvSmoothGaugeNeedles.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvSmoothGaugeNeedles.Insert(Index: Integer): TAdvSmoothGaugeNeedleItem;
begin
  Result := TAdvSmoothGaugeNeedleItem(inherited Insert(Index));
end;

procedure TAdvSmoothGaugeNeedles.SetItem(Index: Integer;
  const Value: TAdvSmoothGaugeNeedleItem);
begin
  inherited Items[Index] := Value;
end;

{ TAdvSmoothGaugeNeedleItem }

procedure TAdvSmoothGaugeNeedleItem.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothGaugeNeedleItem then
  begin
    FColor := (Source as TAdvSmoothGaugeNeedleItem).Color;
    FShineColor := (Source as TAdvSmoothGaugeNeedleItem).ShineColor;
    FShineColorTo := (Source as TAdvSmoothGaugeNeedleItem).ShineColorTo;
  end;
end;

procedure TAdvSmoothGaugeNeedleItem.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothGaugeNeedleItem.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TAdvSmoothGaugeNeedles).FOwner;
  FColor := $00FC7C;
  FShineColor := $00FC7C;
  FShineColorTo := $D3D3D3;
  FOwner.Changed;
end;

destructor TAdvSmoothGaugeNeedleItem.Destroy;
begin
  inherited;
  FOwner.Changed;
end;

procedure TAdvSmoothGaugeNeedleItem.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeNeedleItem.SetShineColor(const Value: TColor);
begin
  if FShineColor <> Value then
  begin
    FShineColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeNeedleItem.SetShineColorTo(const Value: TColor);
begin
  if FShineColorTo <> Value then
  begin
    FShineColorTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothGaugeNeedleItem.SetValue(const Value: Double);
var
  v: Double;
begin
  v := Max(Min(value, FOwner.MaximumValue), FOwner.MinimumValue);
  FValue := v;
  Changed;
end;

end.
