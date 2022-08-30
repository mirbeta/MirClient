{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit cxProgressBar;

{$I cxVer.inc}

interface

uses
  Types, Variants, Windows, Classes, Controls, Forms, Graphics, Messages, SysUtils,
  dxCore, cxClasses, cxContainer, cxControls, cxEdit, cxExtEditConsts, dxGDIPlusClasses,
  cxFilterControlUtils, cxGraphics, cxTextEdit, cxVariants, cxLookAndFeelPainters;

const
  cxProgressBarDefaultAnimationSpeed = 10;
  cxProgressBarDefaultAnimationRestartDelay = 0;
type
  TcxBorderWidth = 0..MaxWord;
  TcxProgressBarAnimationSpeed = 0..20;
  TcxProgressBarBevelOuter = (cxbvNone, cxbvLowered, cxbvRaised);
  TcxProgressBarOrientation = (cxorHorizontal, cxorVertical);
  TcxProgressBarTextStyle = (cxtsPercent, cxtsPosition, cxtsText);
  TcxProgressBarBarStyle = (cxbsSolid, cxbsLEDs, cxbsGradient,
    cxbsGradientLEDs, cxbsBitmap, cxbsBitmapLEDs, cxbsAnimation,
    cxbsAnimationLEDs);
  TcxProgressBarAnimationPath = (cxapCycle, cxapPingPong);

const
  cxDefaultShowTextStyle = cxtsPercent;

type
  { TcxCustomProgressBarViewInfo }

  TcxCustomProgressBar = class;

  TcxCustomProgressBarViewInfo = class(TcxCustomTextEditViewInfo)
  private
    FAnimationDirection: Integer;
    FAnimationPath: TcxProgressBarAnimationPath;
    FAnimationPosition: Integer;
    FAnimationRestartDelay: Cardinal;
    FAnimationRestartDelayTimer: TcxTimer;
    FAnimationSpeed: Cardinal;
    FAnimationTimer: TcxTimer;
    FBarBevelOuter: TcxProgressBarBevelOuter;
    FBarStyle: TcxProgressBarBarStyle;
    FBeginColor: TColor;
    FEndColor: TColor;
    FForegroundImage: TBitmap;
    FMarquee: Boolean;
    FMax: Double;
    FMin: Double;
    FNativeBitmap: TcxAlphaBitmap;
    FOrientation: TcxProgressBarOrientation;
    FOverloadBeginColor: TColor;
    FOverloadEndColor: TColor;
    FOverloadValue: Double;
    FPainterBitmap: TcxBitmap;
    FPeakColor: TColor;
    FPeakSize: TcxNaturalNumber;
    FPeakValue: Double;
    FPosition: Double;
    FPropTransparent: Boolean;
    FRealShowOverload: Boolean;
    FRealShowPeak: Boolean;
    FShowOverload: Boolean;
    FShowPeak: Boolean;
    FShowText: Boolean;
    FShowTextStyle: TcxProgressBarTextStyle;
    FSolidTextColor: Boolean;
    FTextOrientation: TcxProgressBarOrientation;
    FUsualBitmap: TcxBitmap;
    FVistaStyleMarqueeBitmap: TcxAlphaBitmap;

    procedure CalcDrawingParams(out ADrawProgressBarRect, ADrawOverloadBarRect,
      ADrawPeakBarRect, ADrawAnimationBarRect, ASolidRect: TRect; out ALEDsWidth: Integer);
    function CanAnimationBarShow: Boolean;
    procedure CreateBarBmp;
    procedure CreateNativeBitmap(var ABitmap: TcxAlphaBitmap; const ASize: TSize);
    procedure CreatePainterBitmap;
    procedure ExcludeRects(ACanvas: TcxCanvas; const ABounds: TRect);
    procedure ExcludeLEDRects(ACanvas: TcxCanvas; const ABounds: TRect);
    function GetMaxMinDiff: Double;
    function GetRelativeOverloadValue: Double;
    function GetRelativePeakValue: Double;
    function GetRelativePosition: Double;
    function IsLEDStyle: Boolean;
    procedure DrawBackground(ACanvas: TcxCanvas; const ACanvasParent: TcxCanvas; const ABounds: TRect);
    function GetDrawDelta: Integer;
    function GetDrawText: string;
    function GetIsDoubleBuffered: Boolean;
    procedure DrawBarCaption(ACanvas: TcxCanvas);
    procedure PaintBarBevelOuter(ACanvas: TcxCanvas; ABBORect: TRect);
    procedure DrawBarBitmap(ACanvas: TcxCanvas; ARect: TRect);
    procedure DrawGradientBar(ACanvas: TcxCanvas; const ANormalRect, AOverloadRect, ABarRect: TRect);
    procedure DrawSolidBar(ACanvas: TcxCanvas; const ANormalRect, AOverloadRect: TRect);
    procedure DrawVistaStyleMarquee(ACanvas: TcxCanvas; const ARect: TRect);
    procedure DrawAnimationBar(ACanvas: TcxCanvas; const ABar, ASolidRect: TRect);
    procedure DrawAnimationBarBackground(ACanvas: TcxCanvas; const ASolidRect: TRect; ASolidColor: TColor; ADrawBar: Boolean);
    function CalcLEDsWidth: Integer;
    procedure AdjustForLEDsBarBounds(var ABarRect, AOverloadBarRect: TRect; const ALEDsWidth: Integer);
    procedure DrawPeak(ACanvas: TcxCanvas; const APeakRect: TRect);
    procedure DrawBorderLEDs(ACanvas: TcxCanvas; const ABarRect: TRect; ALEDsWidth: Integer);
    procedure SetBarStyle(AValue: TcxProgressBarBarStyle);
    procedure SetOrientation(AValue: TcxProgressBarOrientation);
    procedure SetMarquee(AValue: Boolean);
    procedure SetPeakSize(const Value: TcxNaturalNumber);

    // animation
    function GetAnimationLength: Integer;

    function GetAnimationTimerInterval: Cardinal;
    function GetAnimationOffset: Integer;
    function GetCorrectAnimationBarRect: TRect;
    procedure CalcAnimationCurrentPosition;
    procedure DoAnimationTimer(Sender: TObject);
    procedure DoAnimationRestartDelayTimer(Sender: TObject);
    procedure StartAnimationTimer;
    procedure StartAnimationRestartDelayTimer;
    procedure StopAnimationTimer;
    procedure StopAnimationRestartDelayTimer;
    procedure SetAnimationFirstPosition;
    procedure SetAnimationPath(AValue: TcxProgressBarAnimationPath);
    procedure SetAnimationSpeed(AValue: Cardinal);
  protected
    FIsBoundsChanged: Boolean;
    FIsBarRectChanged: Boolean;
    BarRect: TRect;
    ProgressBarRect: TRect;
    OverloadBarRect: TRect;
    PeakBarRect: TRect;

    procedure PaintProgressBarByPainter(ACanvas: TcxCanvas);
    function GetAnimationBarDimension: Integer; virtual;

    property AnimationPath: TcxProgressBarAnimationPath read FAnimationPath write SetAnimationPath;
    property AnimationRestartDelay: Cardinal read FAnimationRestartDelay write FAnimationRestartDelay;
    property AnimationRestartDelayTimer: TcxTimer read FAnimationRestartDelayTimer;
    property AnimationSpeed: Cardinal read FAnimationSpeed write SetAnimationSpeed;
    property AnimationTimer: TcxTimer read FAnimationTimer;
    property BeginColor: TColor read FBeginColor write FBeginColor;
    property BarBevelOuter: TcxProgressBarBevelOuter read FBarBevelOuter write FBarBevelOuter;
    property EndColor: TColor read FEndColor write FEndColor;
    property Marquee: Boolean read FMarquee write SetMarquee;
    property Min: Double read FMin write FMin;
    property Max: Double read FMax write FMax;
    property MaxMinDiff: Double read GetMaxMinDiff;
    property OverloadValue: Double read FOverloadValue write FOverloadValue;
    property PeakValue: Double read FPeakValue write FPeakValue;
    property Position: Double read FPosition write FPosition;
    property RelativePeakValue: Double read GetRelativePeakValue;
    property RelativeOverloadValue: Double read GetRelativeOverloadValue;
    property RelativePosition: Double read GetRelativePosition;

    property BarStyle: TcxProgressBarBarStyle read FBarStyle write SetBarStyle;
    property ForegroundImage: TBitmap read FForegroundImage write FForegroundImage;
    property IsDoubleBuffered: Boolean read GetIsDoubleBuffered;
    property Orientation: TcxProgressBarOrientation read FOrientation write SetOrientation;
    property OverloadBeginColor: TColor read FOverloadBeginColor write FOverloadBeginColor;
    property OverloadEndColor: TColor read FOverloadEndColor write FOverloadEndColor;
    property PeakColor: TColor read FPeakColor write FPeakColor;
    property PeakSize: TcxNaturalNumber read FPeakSize write SetPeakSize;
    property ShowOverload: Boolean read FShowOverload write FShowOverload;
    property ShowPeak: Boolean read FShowPeak write FShowPeak;
    property ShowText: Boolean read FShowText write FShowText;
    property ShowTextStyle: TcxProgressBarTextStyle read FShowTextStyle write FShowTextStyle;
    property SolidTextColor: Boolean read FSolidTextColor write FSolidTextColor;
    property TextOrientation: TcxProgressBarOrientation read FTextOrientation write FTextOrientation;
  public
    FocusRect: TRect;
    HasForegroundImage: Boolean;
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawText(ACanvas: TcxCanvas); override;
    function GetPercentDone: Integer;
    function GetUpdateRegion(AViewInfo: TcxContainerViewInfo): TcxRegion; override;
    function NeedShowHint(ACanvas: TcxCanvas;
      const P: TPoint; const AVisibleBounds: TRect; out AText: TCaption;
      out AIsMultiLine: Boolean; out ATextRect: TRect; AMaxLineCount: Integer = 0): Boolean; override;
    procedure Paint(ACanvas: TcxCanvas); override;
    procedure PaintProgressBar(ACanvas: TcxCanvas); virtual;
    procedure Offset(DX: Integer; DY: Integer); override;
  end;

  { TcxCustomProgressBarViewData }

  TcxCustomProgressBarProperties = class;

  TcxCustomProgressBarViewData = class(TcxCustomEditViewData)
  private
    function GetProperties: TcxCustomProgressBarProperties;
  protected
    procedure CalculateViewInfoProperties(AViewInfo: TcxCustomEditViewInfo); virtual;
    function InternalGetEditConstantPartSize(ACanvas: TcxCanvas; AIsInplace: Boolean;
      AEditSizeProperties: TcxEditSizeProperties;
      var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize; override;
    function GetDrawTextFlags: Integer; virtual;
    function GetIsEditClass: Boolean;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
      AIsMouseEvent: Boolean); override;
    procedure CalculateButtonsViewInfo(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
      AIsMouseEvent: Boolean); override;
    procedure EditValueToDrawValue(const AEditValue: TcxEditValue;
      AViewInfo: TcxCustomEditViewInfo); override;
    function GetBorderExtentByPainter: TRect; override;

    property Properties: TcxCustomProgressBarProperties read GetProperties;
  end;

  { TcxProgressBarPropertiesValues }

  TcxProgressBarPropertiesValues = class(TcxCustomEditPropertiesValues)
  private
    function GetMax: Boolean;
    function GetMin: Boolean;
    function IsMaxStored: Boolean;
    function IsMinStored: Boolean;
    procedure SetMax(Value: Boolean);
    procedure SetMin(Value: Boolean);
  published
    property Max: Boolean read GetMax write SetMax stored IsMaxStored;
    property Min: Boolean read GetMin write SetMin stored IsMinStored;
  end;

  TcxProgressBarListener = class
  private
    FOnChanged: TNotifyEvent;
    FProgressBar: TcxCustomProgressBar;
  protected
    procedure DoChanged;
  public
    constructor Create(AOwner: TcxCustomProgressBar);
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  { TcxCustomProgressBarProperties }

  TcxCustomProgressBarProperties = class(TcxCustomEditProperties)
  private
    FAnimationPath: TcxProgressBarAnimationPath;
    FAnimationRestartDelay: Cardinal;
    FAnimationSpeed: TcxProgressBarAnimationSpeed;
    FBeginColor: TColor;
    FBarBevelOuter: TcxProgressBarBevelOuter;
    FIsForegroundImageChanged: Boolean;
    FIsPositionChanged: Boolean;
    FEndColor: TColor;
    FForegroundImage: TBitmap;
    FMarquee: Boolean;
    FOrientation: TcxProgressBarOrientation;
    FShowText: Boolean;
    FShowTextStyle: TcxProgressBarTextStyle;
    FText: string;
    FTextOrientation: TcxProgressBarOrientation;
    FSolidTextColor: Boolean;
    FBarStyle: TcxProgressBarBarStyle;
    FTransparentImage: Boolean;
    FBorderWidth: TcxBorderWidth;
    FOverloadValue: Double;
    FShowOverload: Boolean;
    FOverloadBeginColor: TColor;
    FOverloadEndColor: TColor;
    FPeakValue: Double;
    FShowPeak: Boolean;
    FPeakColor: TColor;
    FPeakSize: TcxNaturalNumber;

    procedure ForegroundImageChanged(Sender: TObject);
    function GetAssignedValues: TcxProgressBarPropertiesValues;
    function GetMax: Double;
    function GetMin: Double;
    function GetOverloadValueStored: Boolean;
    function GetPeakValueStored: Boolean;
    function GetRealPeakValue(APosition: Double): Double;
    function IsMaxStored: Boolean;
    function IsMinStored: Boolean;
    function IsShowTextStyleStored: Boolean;
    procedure SetAnimationPath(AValue: TcxProgressBarAnimationPath);
    procedure SetAnimationRestartDelay(AValue: Cardinal);
    procedure SetAnimationSpeed(AValue: TcxProgressBarAnimationSpeed);
    procedure SetAssignedValues(Value: TcxProgressBarPropertiesValues);
    procedure SetBeginColor(Value: TColor);
    procedure SetBarBevelOuter(Value: TcxProgressBarBevelOuter);
    procedure SetColorVista;
    procedure SetEndColor(Value: TColor);
    procedure SetForegroundImage(Value: TBitmap);
    procedure SetMarquee(Value: Boolean);
    procedure SetMax(Value: Double);
    procedure SetMin(Value: Double);
    procedure SetOrientation(Value: TcxProgressBarOrientation);
    procedure SetShowText(Value: Boolean);
    procedure SetShowTextStyle(Value: TcxProgressBarTextStyle);
    procedure SetTextOrientation(Value: TcxProgressBarOrientation);
    procedure SetSolidTextColor(Value: Boolean);
    procedure SetBarStyle(Value: TcxProgressBarBarStyle);
    procedure SetText(const AValue: string);
    procedure SetTransparentImage(Value: Boolean);
    procedure SetBorderWidth(Value: TcxBorderWidth);
    procedure SetOverloadValue(Value: Double);
    procedure SetShowOverload(Value: Boolean);
    procedure SetOverloadBeginColor(Value: TColor);
    procedure SetOverloadEndColor(Value: TColor);
    procedure SetPeakValue(Value: Double);
    procedure SetShowPeak(Value: Boolean);
    procedure SetPeakColor(Value: TColor);
    procedure SetPeakSize(Value: TcxNaturalNumber);
    procedure PostMinValue;
    procedure PostMaxValue;
    procedure PostOverloadValue;
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    class function GetAssignedValuesClass: TcxCustomEditPropertiesValuesClass; override;
    function GetMaxValue: Double; override;
    function GetMinValue: Double; override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function HasDisplayValue: Boolean; override;

    procedure CorrectPositionWithMaxMin(AViewInfo: TcxCustomProgressBarViewInfo); virtual;
    function InternalGetDisplayText(const AEditValue: TcxEditValue; AMarquee: Boolean;
      AShowTextStyle: TcxProgressBarTextStyle; AFullText: Boolean = False; AIsInplace: Boolean = True): WideString; virtual;

    property AssignedValues: TcxProgressBarPropertiesValues read GetAssignedValues write SetAssignedValues;
    property IsForegroundImageChanged: Boolean read FIsForegroundImageChanged write FIsForegroundImageChanged;
    property IsPositionChanged: Boolean read FIsPositionChanged write FIsPositionChanged;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function CanCompareEditValue: Boolean; override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetDisplayText(const AEditValue: TcxEditValue; AFullText: Boolean = False; AIsInplace: Boolean = True): string; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function IsEditValueValid(var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean); override;
    // !!!
    property AnimationPath: TcxProgressBarAnimationPath read FAnimationPath write SetAnimationPath default cxapCycle;
    property AnimationRestartDelay: Cardinal read FAnimationRestartDelay write SetAnimationRestartDelay default cxProgressBarDefaultAnimationRestartDelay;
    property AnimationSpeed: TcxProgressBarAnimationSpeed read FAnimationSpeed write SetAnimationSpeed default cxProgressBarDefaultAnimationSpeed;
    property BarBevelOuter: TcxProgressBarBevelOuter read FBarBevelOuter write SetBarBevelOuter default cxbvNone;
    property BarStyle: TcxProgressBarBarStyle read FBarStyle write SetBarStyle default cxbsSolid;
    property BeginColor: TColor read FBeginColor write SetBeginColor default clNavy;
    property BorderWidth : TcxBorderWidth read FBorderWidth write SetBorderWidth default 0;
    property EndColor: TColor read FEndColor write SetEndColor default clWhite;
    property ForegroundImage: TBitmap read FForegroundImage write SetForegroundImage;
    property Marquee: Boolean read FMarquee write SetMarquee default False;
    property Max: Double read GetMax write SetMax stored IsMaxStored;
    property Min: Double read GetMin write SetMin stored IsMinStored;
    property Orientation: TcxProgressBarOrientation read FOrientation write SetOrientation default cxorHorizontal;
    property OverloadBeginColor: TColor read FOverloadBeginColor write SetOverloadBeginColor default $008080FF;
    property OverloadEndColor: TColor read FOverloadEndColor write SetOverloadEndColor default clFuchsia;
    property OverloadValue: Double read FOverloadValue write SetOverloadValue stored GetOverloadValueStored;
    property PeakColor: TColor read FPeakColor write SetPeakColor default clRed;
    property PeakSize: TcxNaturalNumber read FPeakSize write SetPeakSize default 2;
    property PeakValue: Double read FPeakValue write SetPeakValue stored GetPeakValueStored;
    property ShowOverload: Boolean read FShowOverload write SetShowOverload default False;
    property ShowPeak: Boolean read FShowPeak write SetShowPeak default False;
    property ShowText: Boolean read FShowText write SetShowText default True;
    property ShowTextStyle: TcxProgressBarTextStyle read FShowTextStyle write SetShowTextStyle stored IsShowTextStyleStored;
    property SolidTextColor: Boolean read FSolidTextColor write SetSolidTextColor default False;
    property Text: string read FText write SetText;
    property TextOrientation: TcxProgressBarOrientation read FTextOrientation write SetTextOrientation default cxorHorizontal;
    property Transparent; // deprecated
    property TransparentImage: Boolean read FTransparentImage write SetTransparentImage default True;
  end;

  { TcxProgressBarProperties }

  TcxProgressBarProperties = class(TcxCustomProgressBarProperties)
  published
    property AnimationPath;
    property AnimationRestartDelay;
    property AnimationSpeed;
    property AssignedValues;
    property BarBevelOuter;
    property BarStyle;
    property BeginColor;
    property BorderWidth;
    property EndColor;
    property ForegroundImage;
    property Marquee;
    property Max;
    property Min;
    property Orientation;
    property OverloadBeginColor;
    property OverloadEndColor;
    property OverloadValue;
    property PeakColor;
    property PeakSize;
    property PeakValue;
    property ShowOverload;
    property ShowPeak;
    property ShowText;
    property ShowTextStyle;
    property SolidTextColor;
    property Text;
    property TextOrientation;
    property Transparent; // deprecated
    property TransparentImage;
  end;

  { TcxCustomProgressBar }

  TcxCustomProgressBar = class(TcxCustomEdit)
  strict private
    FListeners: TList;

    function GetPercentDone: Integer;
    function GetPosition: Double;
    function GetPositionStored: Boolean;
    function GetProperties: TcxCustomProgressBarProperties;
    function GetActiveProperties: TcxCustomProgressBarProperties;
    function GetViewInfo: TcxCustomProgressBarViewInfo;
    procedure SetProperties(Value: TcxCustomProgressBarProperties);
    procedure SetPosition(Value: Double);
  protected
    function CanFocusOnClick: Boolean; override;
    procedure CheckEditorValueBounds; virtual;
    procedure CheckEditValue; virtual;
    function DefaultParentColor: Boolean; override;
    procedure DoNotifyListeners;
    function FadingCanFadeBackground: Boolean; override;
    procedure PopulateSizeProperties(var AEditSizeProperties: TcxEditSizeProperties); override;
    procedure Initialize; override;
    function InternalGetNotPublishedStyleValues: TcxEditStyleValues; override;
    function GetEditStateColorKind: TcxEditStateColorKind; override;
    procedure SynchronizeDisplayValue; override;
    procedure PropertiesChanged(Sender: TObject); override;
    //
    property ViewInfo: TcxCustomProgressBarViewInfo read GetViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function CanFocus: Boolean; override;
    procedure RegisterListener(AValue: TcxProgressBarListener);
    procedure UnRegisterListener(AValue: TcxProgressBarListener);

    property ActiveProperties: TcxCustomProgressBarProperties read GetActiveProperties;
    property PercentDone: Integer read GetPercentDone;
    property Position: Double read GetPosition write SetPosition stored GetPositionStored;
    property Properties: TcxCustomProgressBarProperties read GetProperties write SetProperties;
    property Transparent;
  end;

  { TcxCustomProgressBar }

  TcxProgressBar = class(TcxCustomProgressBar)
  private
    function GetActiveProperties: TcxProgressBarProperties;
    function GetProperties: TcxProgressBarProperties;
    procedure SetProperties(Value: TcxProgressBarProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxProgressBarProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property Properties: TcxProgressBarProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;

    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Math, cxEditConsts, cxDrawTextUtils, cxEditUtils, cxExtEditUtils, cxSpinEdit, dxThemeConsts, dxThemeManager,
  dxUxTheme, cxGeometry, dxOffice11, cxLookAndFeels, dxCoreClasses, dxCoreGraphics;

const
  cxAnimationBarColorLightPercentage = 60;
  cxAnimationBarMiddlePartWidth = 10;
  cxAnimationBarTopPartWidth = 6;
  cxAnimationBarTopBeginColorLightPercentage = 40;
  cxAnimationBarTopEndColorLightPercentage = 80;
  cxAnimationBarBorderExtPartColorLightPercentage = 90;
  cxAnimationBarBorderIntPartColorLightPercentage = 80;
  cxAnimationBarBorderExtPathWidth = 4;
  cxAnimationBarBorderIntPathWidth = 20;
  cxAnimationBarBackgroundBorderWidth = 8;

type
  { TcxFilterProgressBarHelper }

  TcxFilterProgressBarHelper = class(TcxFilterSpinEditHelper)
  public
    class procedure InitializeProperties(AProperties,
      AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean); override;
  end;

procedure CalculateCustomProgressBarViewInfo(AViewData: TcxCustomProgressBarViewData;
  AViewInfo: TcxCustomProgressBarViewInfo);

  procedure CheckFocusRectBounds;
  begin
    with AViewInfo do
    begin
      if FocusRect.Left < TextRect.Left - 1 then
        FocusRect.Left := TextRect.Left - 1;
      if FocusRect.Top < TextRect.Top - 1 then
        FocusRect.Top := TextRect.Top - 1;
      if FocusRect.Right > TextRect.Right + 1 then
        FocusRect.Right := TextRect.Right + 1;
      if FocusRect.Bottom > TextRect.Bottom + 1 then
        FocusRect.Bottom := TextRect.Bottom + 1;
    end;
  end;

begin
  with AViewInfo do
  begin
    if not IsInplace and Focused then
      if Length(Text) = 0 then
        FocusRect := cxEmptyRect
      else
      begin
        FocusRect := TextRect;
        InflateRect(FocusRect, 1, 1);
        CheckFocusRectBounds;
      end;
  end;
end;

function CalculateDelta(const APositionDelta, ARectWidth, AMaxMinDelta: Double): Integer;
var
  ACalc: Double;
begin
  ACalc := (APositionDelta * ARectWidth) / AMaxMinDelta;
  Result := Trunc(ACalc);
end;

{ TcxCustomProgressBarViewInfo }

constructor TcxCustomProgressBarViewInfo.Create;
begin
  inherited Create;
  FForegroundImage := TBitmap.Create;
  FAnimationRestartDelay := cxProgressBarDefaultAnimationRestartDelay;
  FUsualBitmap := TcxBitmap.CreateSize(0, 0, pf32bit);
end;

destructor TcxCustomProgressBarViewInfo.Destroy;
begin
  FreeAndNil(FForegroundImage);
  FreeAndNil(FAnimationTimer);
  FreeAndNil(FAnimationRestartDelayTimer);
  FreeAndNil(FUsualBitmap);
  FreeAndNil(FNativeBitmap);
  FreeAndNil(FVistaStyleMarqueeBitmap);
  FreeAndNil(FPainterBitmap);
  inherited Destroy;
end;

function TcxCustomProgressBarViewInfo.GetUpdateRegion(AViewInfo: TcxContainerViewInfo): TcxRegion;
begin
  Result := inherited GetUpdateRegion(AViewInfo);
  if not (AViewInfo is TcxCustomProgressBarViewInfo) then Exit;
end;

function TcxCustomProgressBarViewInfo.NeedShowHint(ACanvas: TcxCanvas;
  const P: TPoint; const AVisibleBounds: TRect; out AText: TCaption;
  out AIsMultiLine: Boolean; out ATextRect: TRect; AMaxLineCount: Integer = 0): Boolean;
begin
  Result := False;
end;

procedure TcxCustomProgressBarViewInfo.DrawText(ACanvas: TcxCanvas);
begin
  DrawBarCaption(ACanvas);
end;

procedure TcxCustomProgressBarViewInfo.DrawVistaStyleMarquee(ACanvas: TcxCanvas; const ARect: TRect);
var
  ASize: TSize;
  AGradientRect: TRect;
  AGradientBitmap: TcxAlphaBitmap;
  ARGBColors, ARGBGradientColors: TRGBColors;
  I: Integer;
begin
  ASize := cxRectSize(ARect);
  if not Assigned(FVistaStyleMarqueeBitmap) or not cxSizeIsEqual(ASize, cxRectSize(FVistaStyleMarqueeBitmap.ClientRect)) then
  begin
    CreateNativeBitmap(FNativeBitmap, cxSize(1, ASize.cy));
    if not Assigned(FNativeBitmap) then
      Exit;

    FVistaStyleMarqueeBitmap := TcxAlphaBitmap.CreateSize(ASize.cx, ASize.cy);
    cxStretchBlt(FVistaStyleMarqueeBitmap.Canvas.Handle, FNativeBitmap.Canvas.Handle, FVistaStyleMarqueeBitmap.ClientRect, FNativeBitmap.ClientRect, SRCCOPY);
    AGradientBitmap := TcxAlphaBitmap.CreateSize(ASize.cx, ASize.cy);
    try
      if FOrientation = cxorHorizontal then
      begin
        AGradientRect := cxRectSetWidth(AGradientBitmap.ClientRect, cxRectWidth(AGradientBitmap.ClientRect) div 2);
        FillGradientRect(AGradientBitmap.Canvas.Handle, AGradientRect, RGB(0, 0, 0), RGB(0, 255, 0), True);
        AGradientRect.Left := AGradientRect.Right;
        AGradientRect.Right := AGradientBitmap.ClientRect.Right;
        FillGradientRect(AGradientBitmap.Canvas.Handle, AGradientRect, RGB(0, 255, 0), RGB(0, 0, 0), True);
      end
      else
      begin
        AGradientRect := cxRectSetHeight(AGradientBitmap.ClientRect, cxRectHeight(AGradientBitmap.ClientRect) div 2);
        FillGradientRect(AGradientBitmap.Canvas.Handle, AGradientRect, RGB(0, 0, 0), RGB(0, 255, 0), False);
        AGradientRect.Top := AGradientRect.Bottom;
        AGradientRect.Bottom := AGradientBitmap.ClientRect.Bottom;
        FillGradientRect(AGradientBitmap.Canvas.Handle, AGradientRect, RGB(0, 255, 0), RGB(0, 0, 0), False);
      end;
      AGradientBitmap.GetBitmapColors(ARGBGradientColors);
      FVistaStyleMarqueeBitmap.GetBitmapColors(ARGBColors);
      for I := 0 to High(ARGBColors) do
      begin
        ARGBColors[I].rgbRed := ARGBGradientColors[I].rgbGreen * ARGBColors[I].rgbRed div 255;
        ARGBColors[I].rgbGreen := ARGBGradientColors[I].rgbGreen * ARGBColors[I].rgbGreen div 255;
        ARGBColors[I].rgbBlue := ARGBGradientColors[I].rgbGreen * ARGBColors[I].rgbBlue div 255;
        ARGBColors[I].rgbReserved := ARGBGradientColors[I].rgbGreen;
      end;
      FVistaStyleMarqueeBitmap.SetBitmapColors(ARGBColors);
    finally
      AGradientBitmap.Free;
    end;
  end;
  cxAlphaBlend(ACanvas.Handle, FVistaStyleMarqueeBitmap, ARect, FVistaStyleMarqueeBitmap.ClientRect);
end;

function TcxCustomProgressBarViewInfo.GetPercentDone: Integer;
begin
  Result := Math.Min(Round(RelativePosition * 100 / MaxMinDiff), 100);
end;

procedure TcxCustomProgressBarViewInfo.Offset(DX: Integer; DY: Integer);
begin
  inherited Offset(DX, DY);

  OffsetRect(BarRect, DX, DY);
  OffsetRect(ProgressBarRect, DX, DY);
  OffsetRect(OverloadBarRect, DX, DY);
  OffsetRect(PeakBarRect, DX, DY);
end;

procedure TcxCustomProgressBarViewInfo.Paint(ACanvas: TcxCanvas);

  function NeedInvalidate: Boolean;
  begin
    Result := FIsBoundsChanged or IsDoubleBuffered or (ShowText and (GetDrawText <> ''));
  end;

begin
  if FIsBarRectChanged then
    CreateBarBmp;

  if Assigned(FUsualBitmap) and NeedInvalidate then
  begin
    if UseSkins then
      PaintProgressBarByPainter(ACanvas)
    else
    begin
      if not (AreVisualStylesMustBeUsed(NativeStyle, totProgress) or IsInplace and Transparent) then
        DrawCustomEdit(ACanvas, False, False);
      PaintProgressBar(ACanvas);
    end;
  end;
end;

procedure TcxCustomProgressBarViewInfo.PaintProgressBar(ACanvas: TcxCanvas);
var
  ALEDsWidth: Integer;
  ABarRect: TRect;
  ADrawProgressBarRect: TRect;
  ADrawOverloadBarRect: TRect;
  ADrawPeakBarRect: TRect;
  ADrawAnimationBarRect: TRect;
  ASolidRect: TRect;
begin
  ACanvas.SaveState;
  try
    CalcDrawingParams(ADrawProgressBarRect, ADrawOverloadBarRect, ADrawPeakBarRect, ADrawAnimationBarRect, ASolidRect, ALEDsWidth);
    ABarRect := BarRect;
    OffsetRect(ABarRect, -BarRect.Left, -BarRect.Top);

    FUsualBitmap.cxCanvas.UseRightToLeftAlignment := UseRightToLeftAlignment;
    FUsualBitmap.cxCanvas.SaveClipRegion;
    try
      DrawBackground(FUsualBitmap.cxCanvas, ACanvas, ABarRect);
      if UseRightToLeftAlignment then
        FUsualBitmap.Flip(True, False);
      if FMarquee and not IsInplace then
      begin
        ExcludeRects(FUsualBitmap.cxCanvas, ASolidRect);
        ExcludeRects(FUsualBitmap.cxCanvas, ADrawAnimationBarRect);
        ExcludeLEDRects(FUsualBitmap.cxCanvas, ADrawAnimationBarRect);
        ASolidRect := ADrawAnimationBarRect;
      end
      else
      begin
        ExcludeRects(FUsualBitmap.cxCanvas, ASolidRect);
        ExcludeLEDRects(FUsualBitmap.cxCanvas, ASolidRect);
      end;
      case FBarStyle of
        cxbsSolid, cxbsLEDs, cxbsGradient, cxbsGradientLEDs:
          begin
            if (FBarStyle in [cxbsSolid, cxbsLEDs]) and not NativeStyle then
              DrawSolidBar(FUsualBitmap.cxCanvas, ADrawProgressBarRect, ADrawOverloadBarRect)
            else
              DrawGradientBar(FUsualBitmap.cxCanvas, ADrawProgressBarRect, ADrawOverloadBarRect, ABarRect);
            if not IsLEDStyle then
              PaintBarBevelOuter(FUsualBitmap.cxCanvas, ASolidRect);
          end;
        cxbsBitmap, cxbsBitmapLEDs:
          if IsGlyphAssigned(FForegroundImage) then
            DrawBarBitmap(FUsualBitmap.cxCanvas, ASolidRect);
        cxbsAnimation, cxbsAnimationLEDs:
          begin
            if not (FMarquee and IsDesigning) and not (FMarquee and (FBarStyle = cxbsAnimation) and NativeStyle) then
              DrawAnimationBarBackground(FUsualBitmap.cxCanvas, ASolidRect, FBeginColor, True);
            if not FMarquee then
              DrawAnimationBar(FUsualBitmap.cxCanvas, ADrawAnimationBarRect, ASolidRect)
            else
              DrawVistaStyleMarquee(FUsualBitmap.cxCanvas, ADrawAnimationBarRect);
          end;
      end;
      if IsLEDStyle then
        DrawBorderLEDs(FUsualBitmap.cxCanvas, ASolidRect, ALEDsWidth);
      if not (FBarStyle in [cxbsAnimation, cxbsAnimationLEDs]) then
        DrawPeak(FUsualBitmap.cxCanvas, ADrawPeakBarRect);
    finally
      FUsualBitmap.cxCanvas.RestoreClipRegion;
    end;
    if UseRightToLeftAlignment then
      FUsualBitmap.Flip(True, False);

    DrawText(FUsualBitmap.cxCanvas);
    cxBitBlt(ACanvas.Canvas.Handle, FUsualBitmap.cxCanvas.Handle, BarRect, cxNullPoint, SRCCOPY);
  finally
    ACanvas.RestoreState;
  end;
end;

procedure TcxCustomProgressBarViewInfo.PaintProgressBarByPainter(ACanvas: TcxCanvas);
var
  AChunkRect: TRect;
  AContentRect: TRect;
  ARect: TRect;
  AVertical: Boolean;

  function CalcRect(AVertical: Boolean; const AContentRect: TRect; AProgressKf: Double): TRect;
  begin
    Result := AContentRect;
    if AVertical then
      Inc(Result.Top, Trunc(cxRectHeight(Result) * (1 - AProgressKf)))
    else
      Result.Right := Result.Left + Trunc(cxRectWidth(Result) * AProgressKf);
  end;

  procedure DrawOverload(ACanvas: TcxCanvas);
  var
    AOverloadRect: TRect;
  begin
    AOverloadRect := CalcRect(AVertical, AContentRect, RelativeOverloadValue / MaxMinDiff);
    if AVertical then
    begin
      AOverloadRect.Bottom := AOverloadRect.Top;
      AOverloadRect.Top := AChunkRect.Top;
    end
    else
    begin
      AOverloadRect.Left := AOverloadRect.Right;
      AOverloadRect.Right := AChunkRect.Right;
    end;

    if not IsRectEmpty(AOverloadRect) then
      ACanvas.InvertRect(AOverloadRect);
  end;

begin
  ARect := Bounds;
  AVertical := Orientation = cxorVertical;
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  CreatePainterBitmap;
  if IsInplace then
    cxEditFillRect(FPainterBitmap.cxCanvas, ARect, BackgroundColor)
  else
    cxDrawTransparentControlBackground(Edit, FPainterBitmap.cxCanvas, Bounds);

  FPainterBitmap.cxCanvas.UseRightToLeftAlignment := UseRightToLeftAlignment;
  if UseRightToLeftAlignment then
    FPainterBitmap.Flip(True, False);

  ARect := Rect(BarRect.Left - Bounds.Left, BarRect.Top - Bounds.Top,
    FPainterBitmap.Width - (Bounds.Right - BarRect.Right),
    FPainterBitmap.Height - (Bounds.Bottom - BarRect.Bottom));

  AContentRect := cxRectContent(ARect, Painter.ProgressBarBorderSize(AVertical));
  if FMarquee and not IsInplace then
  begin
    AChunkRect := GetCorrectAnimationBarRect;
    AChunkRect.Left := Math.Max(AChunkRect.Left, AContentRect.Left);
    AChunkRect.Top := Math.Max(AChunkRect.Top, AContentRect.Top);
    AChunkRect.Right := Math.Min(AChunkRect.Right, AContentRect.Right);
    AChunkRect.Bottom := Math.Min(AChunkRect.Bottom, AContentRect.Bottom);
  end
  else
    AChunkRect := CalcRect(AVertical, AContentRect, RelativePosition / MaxMinDiff);

  Painter.DrawProgressBarBorder(FPainterBitmap.cxCanvas, ARect, AVertical);
  FPainterBitmap.cxCanvas.SetClipRegion(TcxRegion.Create(AContentRect), roSet);
  Painter.DrawProgressBarChunk(FPainterBitmap.cxCanvas, AChunkRect, AVertical);
  if FRealShowOverload then
    DrawOverload(FPainterBitmap.cxCanvas);
  if UseRightToLeftAlignment then
    FPainterBitmap.Flip(True, False);

  DrawText(FPainterBitmap.cxCanvas);
  cxBitBlt(ACanvas.Handle, FPainterBitmap.cxCanvas.Handle, Bounds, cxNullPoint, SRCCOPY);
end;

procedure TcxCustomProgressBarViewInfo.CalcDrawingParams(out ADrawProgressBarRect, ADrawOverloadBarRect,
  ADrawPeakBarRect, ADrawAnimationBarRect, ASolidRect: TRect; out ALEDsWidth: Integer);
begin
  ADrawProgressBarRect := ProgressBarRect;
  ADrawOverloadBarRect := OverloadBarRect;
  ADrawPeakBarRect := PeakBarRect;
  ADrawAnimationBarRect := GetCorrectAnimationBarRect;

  OffsetRect(ADrawProgressBarRect, -BarRect.Left, -BarRect.Top);
  OffsetRect(ADrawOverloadBarRect, -BarRect.Left, -BarRect.Top);
  OffsetRect(ADrawPeakBarRect, -BarRect.Left, -BarRect.Top);
  OffsetRect(ADrawAnimationBarRect, -BarRect.Left, -BarRect.Top);

  ALEDsWidth := CalcLEDsWidth;

  if IsLEDStyle then
    AdjustForLEDsBarBounds(ADrawProgressBarRect, ADrawOverloadBarRect, ALEDsWidth);

  if not FRealShowOverload then
    ASolidRect := ADrawProgressBarRect
  else
    if FOrientation = cxorHorizontal then
      ASolidRect := Rect(ADrawProgressBarRect.Left, ADrawProgressBarRect.Top,
        ADrawOverloadBarRect.Right, ADrawOverloadBarRect.Bottom)
    else
      ASolidRect := Rect(ADrawOverloadBarRect.Left, ADrawOverloadBarRect.Top,
        ADrawProgressBarRect.Right, ADrawProgressBarRect.Bottom);
end;

function TcxCustomProgressBarViewInfo.CanAnimationBarShow: Boolean;
begin
  Result := (((FBarStyle in [cxbsAnimation, cxbsAnimationLEDs]) and not FMarquee) or FMarquee) and
    (FAnimationSpeed > 0) and not IsDesigning and not IsInplace;
end;

procedure TcxCustomProgressBarViewInfo.CreateBarBmp;
begin
  FUsualBitmap.SetSize(cxRectWidth(BarRect), cxRectHeight(BarRect));
  FIsBarRectChanged := False;
end;

procedure TcxCustomProgressBarViewInfo.CreateNativeBitmap(var ABitmap: TcxAlphaBitmap; const ASize: TSize);
var
  ATheme: TdxTheme;
  ARect: TRect;
begin
  if not Assigned(ABitmap) then
    ABitmap := TcxAlphaBitmap.Create
  else
    if cxSizeIsEqual(cxRectSize(ABitmap.ClientRect), ASize) then
      Exit;

  ATheme := OpenTheme(totProgress);
  ABitmap.Width := ASize.cx;
  ABitmap.Height := ASize.cy;
  ARect := ABitmap.Canvas.ClipRect;
  if FOrientation = cxorHorizontal then
    DrawThemeBackground(ATheme, ABitmap.Canvas.Handle, PP_CHUNK, 1, ARect)
  else
    DrawThemeBackground(ATheme, ABitmap.Canvas.Handle, PP_CHUNKVERT, 1, ARect);
end;

procedure TcxCustomProgressBarViewInfo.CreatePainterBitmap;
begin
  if not Assigned(FPainterBitmap) then
    FPainterBitmap := TcxBitmap.CreateSize(Bounds, pf32bit)
  else
    FPainterBitmap.SetSize(Bounds);
end;

procedure TcxCustomProgressBarViewInfo.ExcludeRects(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  if (FBarStyle in [cxbsAnimation, cxbsAnimationLEDs]) and
    NativeStyle then
  begin
    if FOrientation = cxorHorizontal then
    begin
      ACanvas.ExcludeClipRect(Rect(ABounds.Right, Bounds.Top, Bounds.Right, Bounds.Bottom));
      ACanvas.ExcludeClipRect(Rect(Bounds.Left, Bounds.Top, ABounds.Left, Bounds.Bottom));
    end
    else
    begin
      ACanvas.ExcludeClipRect(Rect(Bounds.Left, Bounds.Top, Bounds.Right, ABounds.Top));
      ACanvas.ExcludeClipRect(Rect(Bounds.Left, ABounds.Bottom, Bounds.Right, Bounds.Bottom))
    end;
  end
  else
    ACanvas.SetClipRegion(TcxRegion.Create(ABounds), roIntersect);
end;

procedure TcxCustomProgressBarViewInfo.ExcludeLEDRects(ACanvas: TcxCanvas;
  const ABounds: TRect);
var
  I, ALEDsWidth, ALEDsMaxCount: Integer;
begin
  ALEDsWidth := CalcLEDsWidth;
  if IsLEDStyle then
  begin
    if FOrientation = cxorHorizontal then
    begin
      ALEDsMaxCount := cxRectWidth(ABounds) div ALEDsWidth;
      for I := 1 to ALEDsMaxCount do
        ACanvas.ExcludeClipRect(Rect(ABounds.Left + I * ALEDsWidth - 2, ABounds.Top,
          ABounds.Left + I * ALEDsWidth, ABounds.Bottom));
    end
    else
    begin
      ALEDsMaxCount := cxRectHeight(ABounds) div ALEDsWidth;
      for I := 1 to ALEDsMaxCount do
        ACanvas.ExcludeClipRect(Rect(ABounds.Left, ABounds.Bottom - I * ALEDsWidth,
          ABounds.Right, ABounds.Bottom - I * ALEDsWidth + 2));
    end;
  end;
end;

function TcxCustomProgressBarViewInfo.GetAnimationLength: Integer;
begin
  if FOrientation = cxorHorizontal then
    Result := cxRectWidth(ProgressBarRect)
  else
    Result := cxRectHeight(ProgressBarRect);

  case AnimationPath of
    cxapCycle: Inc(Result, GetAnimationBarDimension);
    cxapPingPong: Dec(Result, GetAnimationBarDimension);
  end;
end;

function TcxCustomProgressBarViewInfo.GetAnimationTimerInterval: Cardinal;
begin
  Result := 30;
end;

function TcxCustomProgressBarViewInfo.GetAnimationOffset: Integer;
begin
  Result := FAnimationSpeed * 2;
end;

function TcxCustomProgressBarViewInfo.GetMaxMinDiff: Double;
begin
  Result := Max - Min;
  if Result = 0 then
    Result := 1;
end;

function TcxCustomProgressBarViewInfo.GetRelativeOverloadValue: Double;
begin
  Result := OverloadValue - Min;
end;

function TcxCustomProgressBarViewInfo.GetRelativePeakValue: Double;
begin
  Result := PeakValue - Min;
end;

function TcxCustomProgressBarViewInfo.GetRelativePosition: Double;
begin
  if FMarquee then
    Result := Max
  else
    Result := Position - Min;
end;

function TcxCustomProgressBarViewInfo.IsLEDStyle: Boolean;
begin
  Result := FBarStyle in [cxbsLEDs, cxbsGradientLEDs, cxbsBitmapLEDs,
    cxbsAnimationLEDs];
end;

procedure TcxCustomProgressBarViewInfo.DrawBackground(ACanvas: TcxCanvas; const ACanvasParent: TcxCanvas; const ABounds: TRect);
const
  BarThemeTypeMap: array[TcxProgressBarOrientation] of Integer = (PP_BAR, PP_BARVERT);
var
  ACanvasWindowOrg: TPoint;
begin
  if FPropTransparent then
  begin
    if IsInplace or (csPaintCopy in Edit.ControlState) then
      cxBitBlt(ACanvas.Handle, ACanvasParent.Handle, ABounds, BarRect.TopLeft, SRCCOPY)
    else
    begin
      ACanvasWindowOrg := ACanvas.WindowOrg;
      ACanvas.WindowOrg := cxPointOffset(ACanvas.WindowOrg, BarRect.TopLeft);
      try
        cxDrawTransparentControlBackground(Edit, ACanvas, Bounds);
      finally
        ACanvas.WindowOrg := ACanvasWindowOrg;
      end;
    end;
  end
  else
  begin
    if not (FBarStyle in [cxbsAnimation, cxbsAnimationLEDs]) then
    begin
      ACanvas.Brush.Style := bsSolid;
      ACanvas.FillRect(ABounds, BackgroundColor);
    end;
  end;
  if (NativeStyle or (FBarStyle in [cxbsAnimation, cxbsAnimationLEDs])) and not IsInplace and not FPropTransparent then
  begin
    if NativeStyle then
      DrawThemeBackground(OpenTheme(totProgress), ACanvas.Handle, BarThemeTypeMap[FOrientation], 1, ABounds)
    else
      if (FBarStyle in [cxbsAnimation, cxbsAnimationLEDs]) and not FPropTransparent then
        DrawAnimationBarBackground(ACanvas, ABounds, BackgroundColor, False);
  end;
end;

function TcxCustomProgressBarViewInfo.GetDrawDelta: Integer;
begin
  if IsInplace then
    Result := 2 * Byte(NativeStyle)
  else
    Result := 2;
end;

function TcxCustomProgressBarViewInfo.GetDrawText: string;
begin
  Result := '';
  case FShowTextStyle of
    cxtsPercent:
      Result := IntToStr(GetPercentDone) + ' %';
    cxtsPosition:
      Result := FloatToStr(FPosition);
    cxtsText:
      Result := Text;
  end;
  if FMarquee then
    Result := Text;
end;

function TcxCustomProgressBarViewInfo.GetIsDoubleBuffered: Boolean;
begin
  Result := Assigned(Edit) and Edit.DoubleBuffered;
end;

procedure TcxCustomProgressBarViewInfo.DrawBarCaption(ACanvas: TcxCanvas);
var
  ABarRect: TRect;
  ABarText: string;
  AProgressBarRect: TRect;
  ATextRect: TRect;
  AStyleStorage: TBrushStyle;
begin
  if not FShowText then Exit;
  if FMarquee then
    AProgressBarRect := GetCorrectAnimationBarRect
  else
    AProgressBarRect := ProgressBarRect;
  ABarRect := BarRect;
  if IsInplace then
  begin
    OffsetRect(ABarRect, -BarRect.Left, -BarRect.Top);
    OffsetRect(AProgressBarRect, -AProgressBarRect.Left, -AProgressBarRect.Top);
  end;
  ABarText := GetDrawText;
  ACanvas.Font.Assign(Font);
  AStyleStorage := ACanvas.Brush.Style;
  try
    ACanvas.Brush.Style := bsClear;
    if FTextOrientation = cxorVertical then
      ACanvas.SetFontAngle(270);
    ATextRect := Rect(0, 0, ACanvas.TextWidth(ABarText), ACanvas.TextHeight(ABarText));
    if FTextOrientation = cxorVertical then
      ATextRect := Rect(ATextRect.Top, ATextRect.Left, ATextRect.Bottom, ATextRect.Right);
    OffsetRect(ATextRect,
      (cxRectWidth(ABarRect) - cxRectWidth(ATextRect)) div 2,
      (cxRectHeight(ABarRect) - cxRectHeight(ATextRect)) div 2);

    if UseRightToLeftAlignment then
    begin
      Painter.DrawProgressBarText(
        ACanvas, TextOrientation = cxorVertical, SolidTextColor, ABarText,
        TdxRightToLeftLayoutConverter.ConvertRect(ATextRect, ABarRect), ABarRect,
        TdxRightToLeftLayoutConverter.ConvertRect(AProgressBarRect, ABarRect), TextColor);
    end
    else
      Painter.DrawProgressBarText(ACanvas, TextOrientation = cxorVertical,
        SolidTextColor, ABarText, ATextRect, ABarRect, AProgressBarRect, TextColor);
  finally
    ACanvas.Brush.Style := AStyleStorage;
  end;
end;

procedure TcxCustomProgressBarViewInfo.PaintBarBevelOuter(ACanvas: TcxCanvas; ABBORect: TRect);
begin
  if FBarBevelOuter = cxbvLowered then
  begin
    DrawEdge(ACanvas.Handle, ABBORect, BDR_SUNKENOUTER, BF_TOPLEFT);
    DrawEdge(ACanvas.Handle, ABBORect, BDR_SUNKENOUTER, BF_BOTTOMRIGHT);
  end;
  if FBarBevelOuter = cxbvRaised then
  begin
    DrawEdge(ACanvas.Handle, ABBORect, BDR_RAISEDINNER, BF_TOPLEFT);
    DrawEdge(ACanvas.Handle, ABBORect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
  end;
end;

procedure TcxCustomProgressBarViewInfo.DrawBarBitmap(ACanvas: TcxCanvas; ARect: TRect);
begin
  ACanvas.SaveState;
  try
    ACanvas.Brush.Bitmap := FForegroundImage;
    ACanvas.FillRect(ARect);
  finally
    ACanvas.RestoreState;
  end;
end;

procedure TcxCustomProgressBarViewInfo.DrawGradientBar(ACanvas: TcxCanvas; const ANormalRect, AOverloadRect, ABarRect: TRect);
var
  ASize: TSize;
  R: TRect;
begin
  with ACanvas do
  begin
    if NativeStyle then
    begin
      if FOrientation = cxorHorizontal then
      with ANormalRect do
      begin
        ASize.cx := 1;
        if cxRectHeight(ANormalRect) < 0 then
          ASize.cy := 0
        else
          ASize.cy := cxRectHeight(ANormalRect);
        CreateNativeBitmap(FNativeBitmap, ASize);
        if not Assigned(FNativeBitmap) then
          Exit;
        StretchBlt(Handle, ANormalRect.Left, ANormalRect.Top, cxRectWidth(ANormalRect),
          cxRectHeight(ANormalRect), FNativeBitmap.Canvas.Handle, 0, 0,
          FNativeBitmap.Width, FNativeBitmap.Height, SRCCOPY);
      end
      else
        with ANormalRect do
        begin
          ASize.cy := 1;
          ASize.cx := cxRectWidth(ANormalRect);
          CreateNativeBitmap(FNativeBitmap, ASize);
          if not Assigned(FNativeBitmap) then
            Exit;
          StretchBlt(Handle, ANormalRect.Left, ANormalRect.Top, cxRectWidth(ANormalRect),
            cxRectHeight(ANormalRect), FNativeBitmap.Canvas.Handle, 0, 0,
            FNativeBitmap.Width, FNativeBitmap.Height, SRCCOPY);
        end;
    end
    else
      if FOrientation = cxorHorizontal then
        FillGradientRect(Handle, ABarRect, FBeginColor, FEndColor, True)
      else
        FillGradientRect(Handle, ABarRect, FEndColor, FBeginColor, False);

    if FRealShowOverload then
    begin
      R := AOverloadRect;
      R.Right := ABarRect.Right;
      R.Top := ABarRect.Top;
      if FOrientation = cxorHorizontal then
        FillGradientRect(Handle, R, FOverloadBeginColor, FOverloadEndColor, True)
      else
        FillGradientRect(Handle, R, FOverloadEndColor, FOverloadBeginColor, False);
    end;
  end;
end;

procedure TcxCustomProgressBarViewInfo.DrawSolidBar(ACanvas: TcxCanvas; const ANormalRect, AOverloadRect: TRect);
begin
  with ACanvas do
  begin
    cxEditFillRect(ACanvas, ANormalRect, FBeginColor);
    if FRealShowOverload then
      cxEditFillRect(ACanvas, AOverloadRect, FOverloadBeginColor);
  end;
end;

procedure TcxCustomProgressBarViewInfo.DrawAnimationBar(ACanvas: TcxCanvas; const ABar, ASolidRect: TRect);

  procedure LightCanvasByGradient(ACanvas: TcxCanvas; const ARect: TRect; AIsHorizontal: Boolean = True);
  var
    I: Integer;
    ABeginColor: TColor;
    AEndColor: TColor;
    ACurrentPercentage: Byte;
    ADeltaPercentage: Byte;
    AWidth: Integer;
  begin
    if AIsHorizontal then
    begin
      AWidth := cxRectWidth(ARect) div 2;
      for I := ARect.Top to (ARect.Top + ScaleFactor.Apply(cxAnimationBarTopPartWidth) - 1) do
      begin
        ABeginColor := ACanvas.Canvas.Pixels[ASolidRect.Left + 1, I];
        AEndColor := dxGetLighterColor(ABeginColor, cxAnimationBarColorLightPercentage);
        FillGradientRect(ACanvas.Handle, Rect(ARect.Left - AWidth, I,
          ARect.Left + AWidth - ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2, I + 1),
          ABeginColor, AEndColor, True);
        FillRectByColor(ACanvas.Handle, Rect(ARect.Left + AWidth - ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2,
          I, ARect.Left + AWidth + ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2, I +1), AEndColor);
        FillGradientRect(ACanvas.Handle, Rect(ARect.Left + AWidth + ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2,
          I, ARect.Right + AWidth, I + 1), AEndColor, ABeginColor, True);
      end;
      ACanvas.SaveClipRegion;
      try
        ACanvas.SetClipRegion(TcxRegion.Create(Rect(ARect.Left, ARect.Top,
          ARect.Right, ARect.Bottom)), roIntersect);
        for I := ARect.Left to ARect.Right do
        begin
          ACurrentPercentage := cxAnimationBarColorLightPercentage;
          ADeltaPercentage := 0;
          if I < (AWidth + ARect.Left - ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2) then
            ADeltaPercentage := (100 - cxAnimationBarColorLightPercentage) *
              (AWidth + ARect.Left - (ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2) - I) div
              (AWidth - ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2)
          else
            if I > (AWidth + ARect.Left + ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2) then
              ADeltaPercentage := (100 - ScaleFactor.Apply(cxAnimationBarColorLightPercentage)) *
                (I - AWidth - ARect.Left - ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2) div
                (AWidth - ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2);
          Inc(ACurrentPercentage, ADeltaPercentage);
          ABeginColor := ACanvas.Canvas.Pixels[I, ARect.Top + ScaleFactor.Apply(cxAnimationBarTopPartWidth)];
          AEndColor := dxGetLighterColor(ABeginColor, ACurrentPercentage);
          ACanvas.FillRect(Rect(I, ARect.Top + ScaleFactor.Apply(cxAnimationBarTopPartWidth), I + 1, ARect.Bottom), AEndColor);
        end;
      finally
        ACanvas.RestoreClipRegion;
      end;
    end
    else
    begin
      AWidth := cxRectHeight(ARect) div 2;
      for I := ARect.Left to (ARect.Left + ScaleFactor.Apply(cxAnimationBarTopPartWidth) - 1) do
      begin
        ABeginColor := GetPixel(ACanvas.Handle, I, ASolidRect.Bottom - 2);
        AEndColor := dxGetLighterColor(ABeginColor, cxAnimationBarColorLightPercentage);
        FillGradientRect(ACanvas.Handle, Rect(I, ARect.Top - AWidth, I + 1,
          ARect.Top + AWidth - ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2), ABeginColor, AEndColor, False);
        ACanvas.FillRect(Rect(I, ARect.Top + AWidth - ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2,
          I + 1, ARect.Top + AWidth + ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2), AEndColor);
        FillGradientRect(ACanvas.Handle, Rect(I, ARect.Top + AWidth + ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2,
          I + 1, ARect.Bottom + AWidth), AEndColor, ABeginColor, False);
      end;
      ACanvas.SaveClipRegion;
      try
        ACanvas.SetClipRegion(TcxRegion.Create(Rect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom)), roIntersect);
        for I := ARect.Top to ARect.Bottom do
        begin
          ACurrentPercentage := cxAnimationBarColorLightPercentage;
          ADeltaPercentage := 0;
          if I < (AWidth + ARect.Top - ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2) then
            ADeltaPercentage := (100 - cxAnimationBarColorLightPercentage) *
              (AWidth + ARect.Top - (ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2) - I) div
              (AWidth - ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2)
          else
            if I > (AWidth + ARect.Top + ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2) then
              ADeltaPercentage := (100 - cxAnimationBarColorLightPercentage) *
                (I - AWidth - ARect.Top - ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2) div
                (AWidth - ScaleFactor.Apply(cxAnimationBarMiddlePartWidth) div 2);
          Inc(ACurrentPercentage, ADeltaPercentage);
          ABeginColor := ACanvas.Canvas.Pixels[ARect.Left + ScaleFactor.Apply(cxAnimationBarTopPartWidth), I];
          if (ABeginColor < 0) or (ABeginColor >= (1 shl 24)) then
            Continue;
          AEndColor := dxGetLighterColor(ABeginColor, ACurrentPercentage);
          ACanvas.FillRect(Rect(ARect.Left + ScaleFactor.Apply(cxAnimationBarTopPartWidth), I, ARect.Right, I + 1), AEndColor);
        end;
      finally
        ACanvas.RestoreClipRegion;
      end;
    end;
  end;

begin
  if not CanAnimationBarShow then
  begin
    StopAnimationTimer;
    Exit;
  end;
  if not Assigned(FAnimationTimer) then
  begin
    if FAnimationSpeed > 0 then
      StartAnimationTimer;
    Exit;
  end;
  LightCanvasByGradient(ACanvas, ABar, (FOrientation = cxorHorizontal));
end;

procedure TcxCustomProgressBarViewInfo.DrawAnimationBarBackground(ACanvas: TcxCanvas; const ASolidRect: TRect; ASolidColor: TColor; ADrawBar: Boolean);
var
  ABorderBeginColor, ABorderEndColor: TColor;
  ATopBorderBeginColor, ATopBorderEndColor: TColor;
  ABorderExtPathWidth, ABorderIntPathWidth, ABorderWidth: Integer;
begin
  ATopBorderBeginColor := dxGetLighterColor(ASolidColor, cxAnimationBarTopBeginColorLightPercentage);
  ATopBorderEndColor := dxGetLighterColor(ASolidColor, cxAnimationBarTopEndColorLightPercentage);
  ABorderBeginColor := dxGetDarkerColor(ASolidColor, cxAnimationBarBorderExtPartColorLightPercentage);
  ABorderEndColor := dxGetDarkerColor(ASolidColor, cxAnimationBarBorderIntPartColorLightPercentage);

  if ADrawBar then
  begin
    ABorderExtPathWidth := cxAnimationBarBorderExtPathWidth;
    ABorderIntPathWidth := cxAnimationBarBorderIntPathWidth;
  end
  else
  begin
    ABorderExtPathWidth := 1;
    ABorderIntPathWidth := cxAnimationBarBackgroundBorderWidth;
  end;
  ABorderWidth := ABorderExtPathWidth + ABorderIntPathWidth;

  with ACanvas do
  begin
    cxEditFillRect(ACanvas, ASolidRect, ASolidColor);
    if FOrientation = cxorHorizontal then
    begin
      if ADrawBar and (cxRectWidth(ASolidRect) < 3 * ABorderWidth) then
      begin
        ABorderExtPathWidth := ABorderExtPathWidth * cxRectWidth(ASolidRect) div (3 * ABorderWidth);
        ABorderIntPathWidth := ABorderIntPathWidth * cxRectWidth(ASolidRect) div (3 * ABorderWidth);
      end;
      ABorderWidth := ABorderIntPathWidth + ABorderExtPathWidth;

      with ASolidRect do
      begin
        FillGradientRect(Handle, Rect(Left, Top, Left + ABorderExtPathWidth, Bottom),
          ABorderBeginColor, ABorderEndColor, True);
        FillGradientRect(Handle, Rect(Right - ABorderExtPathWidth, Top, Right, Bottom),
          ABorderEndColor, ABorderBeginColor, True);

        FillGradientRect(Handle, Rect(Left + ABorderExtPathWidth, Top, Left + ABorderWidth,
          Bottom), ABorderEndColor, ASolidColor, True);
        FillGradientRect(Handle, Rect(Right - ABorderWidth, Top, Right - ABorderExtPathWidth, Bottom),
          ASolidColor, ABorderEndColor, True);

        FillGradientRect(Handle, Rect(Left, Top, Right, Top + ScaleFactor.Apply(cxAnimationBarTopPartWidth)),
          ATopBorderBeginColor, ATopBorderEndColor, False);
      end;
    end
    else
    begin
      if cxRectHeight(ASolidRect) < 3 * ABorderWidth then
      begin
        ABorderExtPathWidth := ABorderExtPathWidth * cxRectHeight(ASolidRect) div (3 * ABorderWidth);
        ABorderIntPathWidth := ABorderIntPathWidth * cxRectHeight(ASolidRect) div (3 * ABorderWidth);
      end;
      ABorderWidth := ABorderIntPathWidth + ABorderExtPathWidth;

      with ASolidRect do
      begin
        FillGradientRect(Handle, Rect(Left, Top, Right, Top + ABorderExtPathWidth),
          ABorderBeginColor, ABorderEndColor, False);
        FillGradientRect(Handle, Rect(Left, Bottom - ABorderExtPathWidth, ASolidRect.Right, Bottom),
          ABorderEndColor, ABorderBeginColor, False);

        FillGradientRect(Handle, Rect(Left, Top + ABorderExtPathWidth, Right,
          Top + ABorderWidth), ABorderEndColor, ASolidColor, False);
        FillGradientRect(Handle, Rect(Left, Bottom - ABorderWidth, Right, Bottom - ABorderExtPathWidth),
          ASolidColor, ABorderEndColor, False);

        FillGradientRect(Handle, Rect(Left, Top, Left + ScaleFactor.Apply(cxAnimationBarTopPartWidth),
          Bottom), ATopBorderBeginColor, ATopBorderEndColor, True);
      end;
    end;
  end;
end;

function TcxCustomProgressBarViewInfo.CalcLEDsWidth: Integer;
begin
  if FOrientation = cxorHorizontal then
  begin
    Result := Trunc(cxRectHeight(ProgressBarRect) * 2 / 3) + 2;
    if (FBarStyle = cxbsBitmapLEDs) and IsGlyphAssigned(FForegroundImage) then
      Result := Math.Min(FForegroundImage.Width, Result);
  end
  else
  begin
    Result := Trunc(cxRectWidth(ProgressBarRect) * 2 / 3) + 2;
    if (FBarStyle = cxbsBitmapLEDs) and IsGlyphAssigned(FForegroundImage) then
      Result := Math.Min(FForegroundImage.Height, Result);
  end;
end;

procedure TcxCustomProgressBarViewInfo.AdjustForLEDsBarBounds(var ABarRect,
  AOverloadBarRect: TRect; const ALEDsWidth: Integer);
var
  ALEDsDelta: Integer;
begin
  if FOrientation = cxorHorizontal then
  begin
    if FRealShowOverload then
    begin
      ALEDsDelta := cxRectWidth(ABarRect) mod ALEDsWidth;
      Dec(ABarRect.Right, ALEDsDelta);
      Dec(AOverloadBarRect.Left, ALEDsDelta);
    end;
  end
  else
  begin
    if FRealShowOverload then
    begin
      ALEDsDelta := cxRectHeight(ABarRect) mod ALEDsWidth;
      Inc(ABarRect.Top, ALEDsDelta);
      Inc(AOverloadBarRect.Bottom, ALEDsDelta);
    end;
  end;
end;

procedure TcxCustomProgressBarViewInfo.DrawPeak(ACanvas: TcxCanvas; const APeakRect: TRect);
begin
  if FRealShowPeak then
  begin
    ACanvas.SetClipRegion(TcxRegion.Create(APeakRect), roAdd);
    cxEditFillRect(ACanvas, APeakRect, FPeakColor);
  end;
end;

procedure TcxCustomProgressBarViewInfo.DrawBorderLEDs(ACanvas: TcxCanvas;
  const ABarRect: TRect; ALEDsWidth: Integer);
var
  I, AMaxCount: Integer;
begin
  if FBarBevelOuter = cxbvNone then
    Exit;
  if FOrientation = cxorHorizontal then
  begin
    AMaxCount := cxRectWidth(ABarRect) div ALEDsWidth;
    for I := 1 to AMaxCount do
    begin
      PaintBarBevelOuter(ACanvas, Rect(ABarRect.Left + (I - 1) * ALEDsWidth, ABarRect.Top,
        ABarRect.Left + I * ALEDsWidth - 2, ABarRect.Bottom));
    end;
    if (ABarRect.Left + AMaxCount * ALEDsWidth) < ABarRect.Right then
      PaintBarBevelOuter(ACanvas, Rect(ABarRect.Left + AMaxCount * ALEDsWidth,
        ABarRect.Top, ABarRect.Right, ABarRect.Bottom));
  end
  else
  begin
    AMaxCount := cxRectHeight(ABarRect) div ALEDsWidth;
    for I := 1 to AMaxCount do
    begin
      PaintBarBevelOuter(ACanvas, Rect(ABarRect.Left, ABarRect.Bottom - (I - 1) * ALEDsWidth,
        ABarRect.Right, ABarRect.Bottom - I * ALEDsWidth + 2));
    end;
    if (ABarRect.Bottom - AMaxCount * ALEDsWidth) > ABarRect.Top then
      PaintBarBevelOuter(ACanvas, Rect(ABarRect.Left, ABarRect.Bottom - AMaxCount * ALEDsWidth,
        ABarRect.Right, ABarRect.Top));
  end;
end;

procedure TcxCustomProgressBarViewInfo.DoAnimationTimer(Sender: TObject);
begin
  if not CanAnimationBarShow then
    StopAnimationTimer;
  if FAnimationTimer = nil then
    Exit;
  CalcAnimationCurrentPosition;
  Edit.Repaint;
end;

procedure TcxCustomProgressBarViewInfo.DoAnimationRestartDelayTimer(Sender: TObject);
begin
  StopAnimationRestartDelayTimer;
end;

procedure TcxCustomProgressBarViewInfo.StartAnimationTimer;
begin
  if FAnimationTimer <> nil then
    StopAnimationTimer;
  if not CanAnimationBarShow then
    Exit;
  FAnimationTimer := cxCreateTimer(DoAnimationTimer, GetAnimationTimerInterval);
  SetAnimationFirstPosition;
end;

procedure TcxCustomProgressBarViewInfo.StartAnimationRestartDelayTimer;
begin
  if FAnimationRestartDelayTimer <> nil then
    StopAnimationRestartDelayTimer;
  if FAnimationRestartDelay = 0 then
    Exit;
  FAnimationRestartDelayTimer := cxCreateTimer(DoAnimationRestartDelayTimer, AnimationRestartDelay);
end;

procedure TcxCustomProgressBarViewInfo.StopAnimationTimer;
begin
  FreeAndNil(FAnimationTimer);
end;

procedure TcxCustomProgressBarViewInfo.StopAnimationRestartDelayTimer;
begin
  FreeAndNil(FAnimationRestartDelayTimer);
end;

procedure TcxCustomProgressBarViewInfo.SetAnimationPath(AValue: TcxProgressBarAnimationPath);
begin
  if AValue <> FAnimationPath then
  begin
    FAnimationPath := AValue;
    StartAnimationTimer;
  end;
end;

procedure TcxCustomProgressBarViewInfo.SetAnimationSpeed(AValue: Cardinal);
begin
  if AValue <> FAnimationSpeed then
  begin
    FAnimationSpeed := AValue;
    if FAnimationTimer <> nil then
    begin
      FAnimationTimer.Interval := GetAnimationTimerInterval;
      FAnimationTimer.Enabled := FAnimationSpeed > 0;
    end
    else
      if FAnimationSpeed > 0 then
        StartAnimationTimer;
  end;
end;

procedure TcxCustomProgressBarViewInfo.SetMarquee(AValue: Boolean);
begin
  if AValue <> FMarquee then
  begin
    FMarquee := AValue;
    StartAnimationTimer;
  end;
end;

procedure TcxCustomProgressBarViewInfo.SetBarStyle(
  AValue: TcxProgressBarBarStyle);
begin
  if AValue <> FBarStyle then
  begin
    FBarStyle := AValue;
    StartAnimationTimer;
  end;
end;

procedure TcxCustomProgressBarViewInfo.CalcAnimationCurrentPosition;
var
  AAnimationLength: Integer;
begin
  if FAnimationRestartDelayTimer <> nil then Exit;

  AAnimationLength := GetAnimationLength;

  Inc(FAnimationPosition, FAnimationDirection * GetAnimationOffset);
  case FAnimationPath of
    cxapCycle:
      if FAnimationPosition > AAnimationLength then
      begin
        SetAnimationFirstPosition;
        StartAnimationRestartDelayTimer;
      end;
    cxapPingPong:
      begin
        if FAnimationPosition > AAnimationLength then
        begin
          FAnimationDirection := -1;
          FAnimationPosition := AAnimationLength;
          StartAnimationRestartDelayTimer;
        end
        else
        if FAnimationPosition < 0 then
        begin
          SetAnimationFirstPosition;
          StartAnimationRestartDelayTimer;
        end;
      end;
  end;
end;

procedure TcxCustomProgressBarViewInfo.SetAnimationFirstPosition;
begin
  FAnimationPosition := 0;
  FAnimationDirection := 1;
end;

function TcxCustomProgressBarViewInfo.GetAnimationBarDimension: Integer;
begin
  if (BarStyle = cxbsAnimation) and NativeStyle then
    Result := ScaleFactor.Apply(127)
  else
    Result := ScaleFactor.Apply(50);
end;

function TcxCustomProgressBarViewInfo.GetCorrectAnimationBarRect: TRect;
var
  AAnimationBarRect: TRect;
  ARectOffset: Integer;
begin
  AAnimationBarRect := ProgressBarRect;
  if FOrientation = cxorHorizontal then
    AAnimationBarRect.Right := ProgressBarRect.Left + GetAnimationBarDimension
  else
    AAnimationBarRect.Bottom := AAnimationBarRect.Top + GetAnimationBarDimension;

  Result := AAnimationBarRect;

  if AnimationPath = cxapCycle then
    ARectOffset := FAnimationPosition - GetAnimationBarDimension
  else
    ARectOffset := FAnimationPosition;

  if FOrientation = cxorHorizontal then
    OffsetRect(Result, ARectOffset, 0)
  else
    OffsetRect(Result, 0, ARectOffset);
end;

procedure TcxCustomProgressBarViewInfo.SetOrientation(AValue: TcxProgressBarOrientation);
begin
  if AValue <> FOrientation then
  begin
    FOrientation := AValue;
    StartAnimationTimer;
  end;
end;

procedure TcxCustomProgressBarViewInfo.SetPeakSize(const Value: TcxNaturalNumber);
begin
  FPeakSize := Math.Max(Value, 1);
end;

{ TcxCustomProgressBarViewData }

procedure TcxCustomProgressBarViewData.CalculateViewInfoProperties(AViewInfo: TcxCustomEditViewInfo);
begin
  with TcxCustomProgressBarViewInfo(AViewInfo) do
  begin
    AnimationPath := Properties.AnimationPath;
    AnimationRestartDelay := Properties.AnimationRestartDelay;
    AnimationSpeed := Properties.AnimationSpeed;
    BeginColor := ColorToRGB(Properties.BeginColor);
    EndColor := ColorToRGB(Properties.EndColor);
    BarBevelOuter := Properties.BarBevelOuter;
    Marquee := Properties.Marquee;
    Min := Properties.Min;
    Max := Properties.Max;
    Orientation := Properties.Orientation;
    ShowText := Properties.ShowText;
    ShowTextStyle := Properties.ShowTextStyle;
    Text := Properties.Text;
    TextOrientation := Properties.TextOrientation;
    SolidTextColor := Properties.SolidTextColor;
    BarStyle := Properties.BarStyle;
    BorderWidth := Properties.BorderWidth;
    OverloadValue := Properties.OverloadValue;
    OverloadBeginColor := ColorToRGB(Properties.OverloadBeginColor);
    OverloadEndColor := ColorToRGB(Properties.OverloadEndColor);
    ShowOverload := Properties.ShowOverload;
    PeakValue := Properties.GetRealPeakValue(Position);
    PeakColor := ColorToRGB(Properties.PeakColor);
    PeakSize := ScaleFactor.Apply(Properties.PeakSize, Properties.ScaleFactor);
    ShowPeak := Properties.ShowPeak;
    if IsInplace then
      FPropTransparent := Transparent
    else
      FPropTransparent := TcxCustomProgressBar(Edit).Transparent;
  end;
end;

procedure TcxCustomProgressBarViewData.Calculate(ACanvas: TcxCanvas; const ABounds: TRect;
  const P: TPoint; Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
  AIsMouseEvent: Boolean);
var
  ADelta: Integer;
  ARect: TRect;
  AProgressBarViewInfo: TcxCustomProgressBarViewInfo;
  ABitmap: TBitmap;
  AProgressBarRect, ABarRect: TRect;
  AOverloadBarRect, APeakBarRect: TRect;
begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo, AIsMouseEvent);
  if (ABounds.Bottom = cxMaxRectSize) or (ABounds.Right = cxMaxRectSize) then Exit; // B94428
  AProgressBarViewInfo := TcxCustomProgressBarViewInfo(AViewInfo);
  CalculateViewInfo(AProgressBarViewInfo, AIsMouseEvent);
  CalculateViewInfoProperties(AViewInfo);
  AProgressBarViewInfo.NativeStyle := AreVisualStylesMustBeUsed(NativeStyle, totProgress);
  ADelta := AProgressBarViewInfo.GetDrawDelta;

  if AProgressBarViewInfo.NativeStyle then
  begin
    ARect := ABounds;
    ABarRect := ABounds;
    if IsInplace then
      ARect := cxRectTransform(ARect, (AViewInfo.BorderWidth + ADelta), (AViewInfo.BorderWidth + ADelta),
        -(AViewInfo.BorderWidth + ADelta), -(AViewInfo.BorderWidth + ADelta))
    else
      if not (Properties.BarStyle in [cxbsAnimation, cxbsAnimationLEDs]) then
        ARect := cxRectTransform(ARect, (AViewInfo.BorderWidth + ADelta + 1), (AViewInfo.BorderWidth + ADelta + 1),
          -(AViewInfo.BorderWidth + ADelta), -(AViewInfo.BorderWidth + ADelta))
      else
        ARect := cxRectTransform(ARect, (AViewInfo.BorderWidth + 1), (AViewInfo.BorderWidth + 1),
          -(AViewInfo.BorderWidth + 1), -(AViewInfo.BorderWidth + 1));
    AProgressBarRect := ARect;
  end
  else
  begin
    if IsInplace then
      ABarRect := cxRectInflate(ABounds, -ADelta, -ADelta)
    else
      ABarRect := AProgressBarViewInfo.BorderRect;
    ARect := AProgressBarViewInfo.BorderRect;
    AProgressBarRect := cxRectInflate(ARect, -AViewInfo.BorderWidth, -AViewInfo.BorderWidth);
  end;
  CalculateCustomProgressBarViewInfo(Self, AProgressBarViewInfo);

  if not Properties.Marquee then
  begin
    if AProgressBarViewInfo.FOrientation = cxorHorizontal then
      AProgressBarRect.Right := ARect.Left +
        CalculateDelta(AProgressBarViewInfo.Position - AProgressBarViewInfo.Min, cxRectWidth(ARect),
          AProgressBarViewInfo.MaxMinDiff)
    else
      AProgressBarRect.Top := ARect.Bottom -
        CalculateDelta((AProgressBarViewInfo.Position - AProgressBarViewInfo.Min), cxRectHeight(ARect),
          AProgressBarViewInfo.MaxMinDiff);
  end;

  AProgressBarViewInfo.FRealShowOverload := False;
  if not Properties.Marquee and Properties.ShowOverload and
    not AProgressBarViewInfo.NativeStyle and
    (AProgressBarViewInfo.Position >= AProgressBarViewInfo.OverloadValue) then
  begin
    AProgressBarViewInfo.FRealShowOverload := True;
    AOverloadBarRect := AProgressBarRect;
    if AProgressBarViewInfo.FOrientation = cxorHorizontal then
    begin
      AOverloadBarRect.Left := ARect.Left +
        CalculateDelta(AProgressBarViewInfo.RelativeOverloadValue, cxRectWidth(ARect),
          AProgressBarViewInfo.MaxMinDiff);
      AOverloadBarRect.Right := Math.Min(AOverloadBarRect.Right, ARect.Right);
      AProgressBarRect.Right := AOverloadBarRect.Left;
    end else
    begin
      AOverloadBarRect.Top := AOverloadBarRect.Bottom -
        CalculateDelta(AProgressBarViewInfo.RelativePosition, cxRectHeight(ARect),
          AProgressBarViewInfo.MaxMinDiff);
      AOverloadBarRect.Bottom := AOverloadBarRect.Bottom -
        CalculateDelta(AProgressBarViewInfo.RelativeOverloadValue, cxRectHeight(ARect),
          AProgressBarViewInfo.MaxMinDiff);
      AOverloadBarRect.Bottom := Math.Max(AOverloadBarRect.Bottom, ARect.Top);
      AProgressBarRect.Top := AOverloadBarRect.Bottom;
    end;
  end;

  AProgressBarViewInfo.FRealShowPeak := AProgressBarViewInfo.ShowPeak and not Properties.Marquee;
  if AProgressBarViewInfo.FRealShowPeak then
  begin
    APeakBarRect := AProgressBarRect;
    if AProgressBarViewInfo.FOrientation = cxorHorizontal then
    begin
      APeakBarRect.Left := ARect.Left +
        CalculateDelta(AProgressBarViewInfo.RelativePeakValue, cxRectWidth(ARect),
        AProgressBarViewInfo.MaxMinDiff);
      APeakBarRect.Left := Math.Min(APeakBarRect.Left, ARect.Right - AProgressBarViewInfo.PeakSize);
      APeakBarRect.Right := APeakBarRect.Left + AProgressBarViewInfo.PeakSize;
    end
    else
    begin
      APeakBarRect.Bottom := ARect.Bottom -
        CalculateDelta(AProgressBarViewInfo.RelativePeakValue, cxRectHeight(ARect),
          AProgressBarViewInfo.MaxMinDiff);
      APeakBarRect.Bottom := Math.Max(APeakBarRect.Bottom, ARect.Top + AProgressBarViewInfo.PeakSize);
      APeakBarRect.Top := APeakBarRect.Bottom - AProgressBarViewInfo.PeakSize;
    end;
  end;
  if Properties.IsForegroundImageChanged or not IsGlyphAssigned(AProgressBarViewInfo.ForegroundImage) then
  begin
    AProgressBarViewInfo.ForegroundImage.Assign(Properties.ForegroundImage);
    Properties.IsForegroundImageChanged := False;
    if Properties.TransparentImage then
    begin
      ABitmap := TBitmap.Create;
      try
        AProgressBarViewInfo.ForegroundImage.Transparent := True;
        ABitmap.Width := AProgressBarViewInfo.ForegroundImage.Width;
        ABitmap.Height := AProgressBarViewInfo.ForegroundImage.Height;
        ABitmap.Canvas.Brush.Color := AProgressBarViewInfo.BackgroundColor;
        ABitmap.Canvas.FillRect(ABitmap.Canvas.ClipRect);
        ABitmap.Canvas.Draw(0, 0, AProgressBarViewInfo.ForegroundImage);
        AProgressBarViewInfo.ForegroundImage.Assign(ABitmap);
      finally
        ABitmap.Free;
      end;
    end;
  end;

  if not IsInplace then AProgressBarViewInfo.DrawSelectionBar := False;

  if Properties.IsPositionChanged and not AIsMouseEvent and
    cxRectIsEqual(AProgressBarRect, AProgressBarViewInfo.ProgressBarRect) and
    cxRectIsEqual(ABarRect, AProgressBarViewInfo.BarRect) and
    (not AProgressBarViewInfo.FRealShowOverload or cxRectIsEqual(AOverloadBarRect, AProgressBarViewInfo.OverloadBarRect)) and
    (not AProgressBarViewInfo.FRealShowPeak or cxRectIsEqual(APeakBarRect, AProgressBarViewInfo.PeakBarRect)) then
  begin
    AProgressBarViewInfo.FIsBoundsChanged := False;
  end
  else
  begin
    AProgressBarViewInfo.ProgressBarRect := AProgressBarRect;
    if not cxRectIsEqual(AProgressBarViewInfo.BarRect, ABarRect) then
    begin
      AProgressBarViewInfo.BarRect := ABarRect;
      AProgressBarViewInfo.FIsBarRectChanged := True;
    end;
    AProgressBarViewInfo.OverloadBarRect := AOverloadBarRect;
    AProgressBarViewInfo.PeakBarRect := APeakBarRect;
    AProgressBarViewInfo.FIsBoundsChanged := True;
  end;
  Properties.IsPositionChanged := False;
end;

procedure TcxCustomProgressBarViewData.CalculateButtonsViewInfo(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
  Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
  AIsMouseEvent: Boolean);
begin
end;

procedure TcxCustomProgressBarViewData.EditValueToDrawValue(
  const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
var
  ADisplayValue: TcxEditValue;
begin
  CalculateCustomProgressBarViewInfo(Self, AViewInfo as TcxCustomProgressBarViewInfo);
  if PreviewMode then
    Properties.PrepareDisplayValue(30, ADisplayValue, InternalFocused)
  else
    Properties.PrepareDisplayValue(AEditValue, ADisplayValue, InternalFocused);
  TcxCustomProgressBarViewInfo(AViewInfo).Position := ADisplayValue;
  Properties.CorrectPositionWithMaxMin(TcxCustomProgressBarViewInfo(AViewInfo));
end;

function TcxCustomProgressBarViewData.InternalGetEditConstantPartSize(ACanvas: TcxCanvas;
  AIsInplace: Boolean; AEditSizeProperties: TcxEditSizeProperties;
  var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize;
var
  ASize1, ASize2: TSize;
  AText: string;
begin
  ACanvas.SaveState;
  try
    Result := inherited InternalGetEditConstantPartSize(ACanvas, AIsInplace,
      AEditSizeProperties, MinContentSize, AViewInfo);

    with TcxCustomProgressBarViewInfo(AViewInfo) do
    begin
      ASize1.cx := cxRectWidth(ProgressBarRect);

      if not(IsInplace or
        AreVisualStylesMustBeUsed(AViewInfo.NativeStyle, totButton)) then
          ASize1.cx := ASize1.cx + 4;

      AText := '';
      ASize2 := GetTextEditContentSize(ACanvas, Self, AText,
        DrawTextFlagsTocxTextOutFlags(cxTextOutFlagsToDrawTextFlags(GetDrawTextFlags) and
          not(CXTO_CENTER_VERTICALLY or CXTO_BOTTOM) or CXTO_TOP), AEditSizeProperties, 0, False);
      ASize2.cx := ASize2.cx + 3;
      ASize1.cx := ASize1.cx + ASize2.cx;
      ASize1.cy := ASize2.cy;
    end;
    Result.cx := Result.cx + ASize1.cx;
    Result.cy := Result.cy + ASize1.cy;
  finally
    ACanvas.RestoreState;
  end;
end;

function TcxCustomProgressBarViewData.GetBorderExtentByPainter: TRect;
begin
  Result := cxSimpleRect;
end;

function TcxCustomProgressBarViewData.GetDrawTextFlags: Integer;
begin
  Result := 0;
end;

function TcxCustomProgressBarViewData.GetIsEditClass: Boolean;
begin
  Result := False;
end;

function TcxCustomProgressBarViewData.GetProperties: TcxCustomProgressBarProperties;
begin
  Result := TcxCustomProgressBarProperties(FProperties);
end;

{ TProgressBarPropertiesValues }

function TcxProgressBarPropertiesValues.GetMax: Boolean;
begin
  Result := MaxValue;
end;

function TcxProgressBarPropertiesValues.GetMin: Boolean;
begin
  Result := MinValue;
end;

function TcxProgressBarPropertiesValues.IsMaxStored: Boolean;
begin
  Result := Max and (TcxCustomProgressBarProperties(Properties).Max = 0);
end;

function TcxProgressBarPropertiesValues.IsMinStored: Boolean;
begin
  Result := Min and (TcxCustomProgressBarProperties(Properties).Min = 0);
end;

procedure TcxProgressBarPropertiesValues.SetMax(Value: Boolean);
begin
  MaxValue := Value;
end;

procedure TcxProgressBarPropertiesValues.SetMin(Value: Boolean);
begin
  MinValue := Value;
end;

{ TcxProgressBarListener }

constructor TcxProgressBarListener.Create(AOwner: TcxCustomProgressBar);
begin
  inherited Create;
  FProgressBar := AOwner;
end;

procedure TcxProgressBarListener.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(FProgressBar);
end;

{ TcxCustomProgressBarProperties }

constructor TcxCustomProgressBarProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
//  FCurrentPosition := 0;
  FAnimationPath := cxapCycle;
  FAnimationRestartDelay := cxProgressBarDefaultAnimationRestartDelay;
  FBeginColor := clNavy;
  FEndColor := clWhite;
  FBarBevelOuter := cxbvNone;
  FPeakValue := 0;
  FOverloadValue := 80;
  FPeakSize := 2;
  FOrientation := cxorHorizontal;
  FShowText := True;
  FShowTextStyle := cxDefaultShowTextStyle;
  FTextOrientation := cxorHorizontal;
  FSolidTextColor := False;
  FBarStyle := cxbsSolid;
  FTransparentImage := True;
  FMarquee := False;
  FOverloadValue := 80;
  FBorderWidth := 0;
  FShowOverload := False;
  FOverloadBeginColor := $008080FF;
  FOverloadEndColor := clFuchsia;
  FShowPeak := False;
  FPeakColor := clRed;
  FAnimationSpeed := cxProgressBarDefaultAnimationSpeed;
  FText := '';
  FForegroundImage := TBitmap.Create;
  FForegroundImage.OnChange := ForegroundImageChanged;
end;

destructor TcxCustomProgressBarProperties.Destroy;
begin
  FreeAndNil(FForegroundImage);
  inherited Destroy;
end;

function TcxCustomProgressBarProperties.CanCompareEditValue: Boolean;
begin
  Result := True;
end;

class function TcxCustomProgressBarProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxProgressBar;
end;

function TcxCustomProgressBarProperties.GetDisplayText(const AEditValue: TcxEditValue;
  AFullText: Boolean = False; AIsInplace: Boolean = True): string;
begin
  Result := InternalGetDisplayText(AEditValue, Marquee, ShowTextStyle, AFullText, AIsInplace);
end;

function TcxCustomProgressBarProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := [esoAlwaysHotTrack, esoFiltering, esoSorting];
end;

class function TcxCustomProgressBarProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxCustomProgressBarViewInfo;
end;

function TcxCustomProgressBarProperties.IsEditValueValid(var EditValue: TcxEditValue;
  AEditFocused: Boolean): Boolean;
begin
  Result := inherited IsEditValueValid(EditValue, AEditFocused);
end;

procedure TcxCustomProgressBarProperties.PrepareDisplayValue(const AEditValue:
  TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean);
var
  AValue: Double;
  ACode: Integer;
begin
  DisplayValue := 0;
  if VarIsStr(AEditValue) then
  begin
    Val(VarToStr(AEditValue), AValue, ACode);
    if ACode = 0 then
      DisplayValue := AValue;
  end
  else
    if VarIsNumericEx(AEditValue) or VarIsDate(AEditValue) then
      DisplayValue := AEditValue;
end;

procedure TcxCustomProgressBarProperties.CorrectPositionWithMaxMin(AViewInfo: TcxCustomProgressBarViewInfo);
begin
  if Min < Max then
    AViewInfo.Position := Math.Min(Math.Max(AViewInfo.Position, Min), Max);
end;

function TcxCustomProgressBarProperties.InternalGetDisplayText(const AEditValue: TcxEditValue; AMarquee: Boolean;
  AShowTextStyle: TcxProgressBarTextStyle; AFullText: Boolean = False; AIsInplace: Boolean = True): WideString;
var
  ADisplayValue: TcxEditValue;
  AViewInfo: TcxCustomProgressBarViewInfo;
begin
  PrepareDisplayValue(AEditValue, ADisplayValue, False);
  AViewInfo := TcxCustomProgressBarViewInfo.Create;
  try
    AViewInfo.Position := ADisplayValue;
    CorrectPositionWithMaxMin(AViewInfo);
    AViewInfo.Min := Min;
    AViewInfo.Max := Max;
    AViewInfo.Marquee := AMarquee;
    AViewInfo.ShowTextStyle := AShowTextStyle;
    AViewInfo.Text := Text;
    Result := AViewInfo.GetDrawText;
  finally
    AViewInfo.Free;
  end;
end;

procedure TcxCustomProgressBarProperties.ChangeScale(M: Integer; D: Integer);
begin
  inherited ChangeScale(M, D);
  PeakSize := MulDiv(PeakSize, M, D);
end;

procedure TcxCustomProgressBarProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomProgressBarProperties then
    with TcxCustomProgressBarProperties(AProperties) do
    begin
      Self.AnimationPath := AnimationPath;
      Self.AnimationRestartDelay := AnimationRestartDelay;
      Self.BeginColor := BeginColor;
      Self.BarBevelOuter := BarBevelOuter;
      Self.EndColor := EndColor;
      Self.ForegroundImage := ForegroundImage;
      Self.Marquee := Marquee;
      Self.Min := Min;
      Self.Max := Max;
      Self.Orientation := Orientation;
      Self.ShowText := ShowText;
      Self.ShowTextStyle := ShowTextStyle;
      Self.TextOrientation := TextOrientation;
      Self.SolidTextColor := SolidTextColor;
      Self.Text := Text;
      Self.BarStyle := BarStyle;
      Self.TransparentImage := TransparentImage;
      Self.BorderWidth := BorderWidth;
      Self.OverloadValue := OverloadValue;
      Self.ShowOverload := ShowOverload;
      Self.OverloadBeginColor := OverloadBeginColor;
      Self.OverloadEndColor := OverloadEndColor;
      Self.PeakValue := PeakValue;
      Self.ShowPeak := ShowPeak;
      Self.PeakColor := PeakColor;
      Self.PeakSize := Self.ScaleFactor.Apply(PeakSize, ScaleFactor);
    end;
end;

class function TcxCustomProgressBarProperties.GetAssignedValuesClass: TcxCustomEditPropertiesValuesClass;
begin
  Result := TcxProgressBarPropertiesValues;
end;

function TcxCustomProgressBarProperties.GetMaxValue: Double;
begin
  if AssignedValues.Max then
    Result := inherited GetMaxValue
  else
    Result := 100;
end;

function TcxCustomProgressBarProperties.GetMinValue: Double;
begin
  if AssignedValues.Min then
    Result := inherited GetMinValue
  else
    Result := 0;
end;

class function TcxCustomProgressBarProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxCustomProgressBarViewData;
end;

function TcxCustomProgressBarProperties.HasDisplayValue: Boolean;
begin
  Result := True;
end;

procedure TcxCustomProgressBarProperties.ForegroundImageChanged(Sender: TObject);
begin
  IsForegroundImageChanged := True;
  Changed;
end;

function TcxCustomProgressBarProperties.GetAssignedValues: TcxProgressBarPropertiesValues;
begin
  Result := TcxProgressBarPropertiesValues(FAssignedValues);
end;

function TcxCustomProgressBarProperties.GetMax: Double;
begin
  Result := MaxValue;
end;

function TcxCustomProgressBarProperties.GetMin: Double;
begin
  Result := MinValue;
end;

function TcxCustomProgressBarProperties.GetOverloadValueStored: Boolean;
begin
  Result := FOverloadValue <> 80;
end;

function TcxCustomProgressBarProperties.GetPeakValueStored: Boolean;
begin
  Result := FPeakValue <> 0;
end;

procedure TcxCustomProgressBarProperties.SetAnimationPath(AValue: TcxProgressBarAnimationPath);
begin
  if AValue <> FAnimationPath then
  begin
    FAnimationPath := AValue;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetAnimationRestartDelay(AValue: Cardinal);
begin
  if AValue <> FAnimationRestartDelay then
  begin
    FAnimationRestartDelay := AValue;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetAnimationSpeed(AValue: TcxProgressBarAnimationSpeed);
begin
  if AValue < Low(AValue) then
    AValue := Low(AValue);
  if AValue > High(AValue) then
    AValue := High(AValue);
  if AValue <> FAnimationSpeed then
  begin
    FAnimationSpeed := AValue;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetAssignedValues(
  Value: TcxProgressBarPropertiesValues);
begin
  FAssignedValues.Assign(Value);
end;

procedure TcxCustomProgressBarProperties.SetBeginColor(Value: TColor);
begin
  if FBeginColor <> Value then
  begin
    FBeginColor := Value;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetBarBevelOuter(Value: TcxProgressBarBevelOuter);
begin
  if FBarBevelOuter <> Value then
  begin
    FBarBevelOuter := Value;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetColorVista;
begin
  FBeginColor := $D328;
end;

procedure TcxCustomProgressBarProperties.SetEndColor(Value: TColor);
begin
  if Value <> FEndColor then
  begin
    FEndColor := Value;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetForegroundImage(Value: TBitmap);
begin
  ForegroundImage.Assign(Value);
end;

procedure TcxCustomProgressBarProperties.SetMarquee(Value: Boolean);
begin
  if Value <> FMarquee then
  begin
    FMarquee := Value;
    if FMarquee then
      ShowTextStyle := cxtsText;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetMax(Value: Double);
begin
  MaxValue := Value;
  PostMaxValue;
end;

procedure TcxCustomProgressBarProperties.SetMin(Value: Double);
begin
  MinValue := Value;
  PostMinValue;
end;

procedure TcxCustomProgressBarProperties.SetOverloadValue(Value: Double);
begin
  if FOverloadValue <> Value then
  begin
    FOverloadValue := Value;
    PostOverloadValue;
    Changed;
  end;
end;

function TcxCustomProgressBarProperties.GetRealPeakValue(APosition: Double): Double;
begin
  Result := Math.Max(Math.Min(Math.Max(FPeakValue, Min), Max), APosition);
  FPeakValue := Math.Max(FPeakValue, Result);
end;

function TcxCustomProgressBarProperties.IsMaxStored: Boolean;
begin
  Result := IsMaxValueStored;
end;

function TcxCustomProgressBarProperties.IsMinStored: Boolean;
begin
  Result := IsMinValueStored;
end;

function TcxCustomProgressBarProperties.IsShowTextStyleStored: Boolean;
begin
  Result := not Marquee and (FShowTextStyle <> cxDefaultShowTextStyle);
end;

procedure TcxCustomProgressBarProperties.SetPeakValue(Value: Double);
begin
  if FPeakValue <> Value then
  begin
    FPeakValue := Value;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.PostMinValue;
begin
  if Min > Max then Max := Min;
//  if FCurrentPosition < FMin then FCurrentPosition := FMin;
  if FOverloadValue < Min then FOverloadValue := Min;
  if FPeakValue < Min then FPeakValue := Min;
end;

procedure TcxCustomProgressBarProperties.PostMaxValue;
begin
  if Min > Max then Min := Max;
//  if FCurrentPosition > FMax then FCurrentPosition := FMax;
  if FOverloadValue > Max then FOverloadValue := Max;
  if FPeakValue > Max then FPeakValue := Max;
end;

procedure TcxCustomProgressBarProperties.PostOverloadValue;
begin
  if FOverloadValue < Min then
    FOverloadValue := Min;
  if FOverloadValue > Max then
    FOverloadValue := Max;
end;

procedure TcxCustomProgressBarProperties.SetOrientation(Value: TcxProgressBarOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetShowText(Value: Boolean);
begin
  if FShowText <> Value then
  begin
    FShowText := Value;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetShowTextStyle(Value: TcxProgressBarTextStyle);
begin
  if (FShowTextStyle <> Value) and (not Marquee or (Marquee and (Value = cxtsText))) then
  begin
    FShowTextStyle := Value;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetTextOrientation(Value: TcxProgressBarOrientation);
begin
  if FTextOrientation <> Value then
  begin
    FTextOrientation := Value;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetSolidTextColor(Value: Boolean);
begin
  if FSolidTextColor <> Value then
  begin
    FSolidTextColor := Value;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetBarStyle(Value: TcxProgressBarBarStyle);
begin
  if FBarStyle <> Value then
  begin
    FBarStyle := Value;
    if FBarStyle in [cxbsAnimation, cxbsAnimationLEDs] then
      SetColorVista;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetText(const AValue: string);
begin
  if FText <> AValue then
  begin
    FText := AValue;
    if Length(FText) > 0 then
      ShowTextStyle := cxtsText;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetTransparentImage(Value: Boolean);
begin
  if FTransparentImage <> Value then
  begin
    FTransparentImage := Value;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetBorderWidth(Value: TcxBorderWidth);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetShowOverload(Value: Boolean);
begin
  if FShowOverload <> Value then
  begin
    FShowOverload := Value;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetOverloadBeginColor(Value: TColor);
begin
  if FOverloadBeginColor <> Value then
  begin
    FOverloadBeginColor := Value;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetOverloadEndColor(Value: TColor);
begin
  if FOverloadEndColor <> Value then
  begin
    FOverloadEndColor := Value;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetShowPeak(Value: Boolean);
begin
  if FShowPeak <> Value then
  begin
    FShowPeak := Value;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetPeakColor(Value: TColor);
begin
  if FPeakColor <> Value then
  begin
    FPeakColor := Value;
    Changed;
  end;
end;

procedure TcxCustomProgressBarProperties.SetPeakSize(Value: TcxNaturalNumber);
begin
  if FPeakSize <> Value then
  begin
    FPeakSize := Value;
    Changed;
  end;
end;

{ TcxCustomProgressBar }

constructor TcxCustomProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListeners := TList.Create;
end;

destructor TcxCustomProgressBar.Destroy;
begin
  FreeAndNil(FListeners);
  inherited Destroy;
end;

class function TcxCustomProgressBar.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomProgressBarProperties;
end;

procedure TcxCustomProgressBar.CheckEditorValueBounds;
var
  AValue: Variant;
begin
  BeginUserAction;
  try
    with ActiveProperties do
      if Min < Max then
      begin
        PrepareDisplayValue(FEditValue, AValue, Focused);
        if AValue < Min then
          InternalSetEditValue(Min, False)
        else
          if AValue > Max then
            InternalSetEditValue(Max, False);
      end;
  finally
    EndUserAction;
  end;
end;

procedure TcxCustomProgressBar.CheckEditValue;
begin
  if not(IsInplace or IsDBEdit or PropertiesChangeLocked) then
    CheckEditorValueBounds;
end;

function TcxCustomProgressBar.DefaultParentColor: Boolean;
begin
  Result := True;
end;

procedure TcxCustomProgressBar.DoNotifyListeners;
var
  I: Integer;
begin
  if FListeners <> nil then
    for I := 0 to FListeners.Count - 1 do
      TcxProgressBarListener(FListeners[I]).DoChanged;
end;

procedure TcxCustomProgressBar.PopulateSizeProperties(var AEditSizeProperties: TcxEditSizeProperties);
begin
  AEditSizeProperties := cxDefaultEditSizeProperties;
  AEditSizeProperties.MaxLineCount := 1;
  AEditSizeProperties.Width := ViewInfo.TextRect.Right - ViewInfo.TextRect.Left;
end;

procedure TcxCustomProgressBar.Initialize;
begin
  inherited Initialize;
  ControlStyle := ControlStyle - [csDoubleClicks, csCaptureMouse, csClickEvents];
  Width := 121;
  Height := 21;
end;

function TcxCustomProgressBar.InternalGetNotPublishedStyleValues: TcxEditStyleValues;
begin
  Result := inherited InternalGetNotPublishedStyleValues;
  Include(Result, svHotTrack);
end;

procedure TcxCustomProgressBar.SynchronizeDisplayValue;
var
  ADisplayValue: TcxEditValue;
begin
  ActiveProperties.PrepareDisplayValue(FEditValue, ADisplayValue, Focused);
  TcxCustomProgressBarViewInfo(ViewInfo).Position := ADisplayValue;
  if ActiveProperties.Transparent then
    Invalidate;
//  if not (IsInplace and (ActiveProperties.ShowTextStyle = cxtsPosition)) then
  if not IsLoading then
    ActiveProperties.CorrectPositionWithMaxMin(ViewInfo);
  ShortRefreshContainer(False);
  //Invalidate;
end;

procedure TcxCustomProgressBar.PropertiesChanged(Sender: TObject);
begin
  CheckEditValue;
//  if not (IsInplace and (ActiveProperties.ShowTextStyle = cxtsPosition)) then
  ActiveProperties.CorrectPositionWithMaxMin(ViewInfo);
  inherited PropertiesChanged(Sender);
  if ActiveProperties.Transparent then
    Invalidate;
  DoNotifyListeners;
end;

function TcxCustomProgressBar.CanFocus: Boolean;
begin
  Result := IsInplace;
end;

function TcxCustomProgressBar.FadingCanFadeBackground: Boolean;
begin
  Result := False;
end;

procedure TcxCustomProgressBar.RegisterListener(AValue: TcxProgressBarListener);
begin
  if FListeners <> nil then
    FListeners.Add(AValue);
end;

procedure TcxCustomProgressBar.UnRegisterListener(AValue: TcxProgressBarListener);
begin
  if FListeners <> nil then
    FListeners.Remove(AValue);
end;

function TcxCustomProgressBar.CanFocusOnClick: Boolean;
begin
  Result := inherited CanFocusOnClick and IsInplace;
end;

function TcxCustomProgressBar.GetEditStateColorKind: TcxEditStateColorKind;
begin
  Result := cxEditStateColorKindMap[Enabled];
end;

function TcxCustomProgressBar.GetPercentDone: Integer;
begin
  Result := ViewInfo.GetPercentDone;
end;

function TcxCustomProgressBar.GetPosition: Double;
begin
  Result := ViewInfo.Position;
end;

function TcxCustomProgressBar.GetPositionStored: Boolean;
begin
  Result := ViewInfo.Position <> 0;
end;

function TcxCustomProgressBar.GetProperties: TcxCustomProgressBarProperties;
begin
  Result := TcxCustomProgressBarProperties(inherited Properties);
end;

function TcxCustomProgressBar.GetActiveProperties: TcxCustomProgressBarProperties;
begin
  Result := TcxCustomProgressBarProperties(InternalGetActiveProperties);
end;

function TcxCustomProgressBar.GetViewInfo: TcxCustomProgressBarViewInfo;
begin
  Result := TcxCustomProgressBarViewInfo(FViewInfo);
end;

procedure TcxCustomProgressBar.SetProperties(
  Value: TcxCustomProgressBarProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxCustomProgressBar.SetPosition(Value: Double);
begin
  if Value = ViewInfo.Position then
    Exit;
  ActiveProperties.IsPositionChanged := True;
  with ActiveProperties do
    if (not IsLoading) and (Min < Max) then
      Value := Math.Min(Math.Max(Value, Min), Max);
  EditValue := Value;
  DoNotifyListeners;
end;

{ TcxProgressBar }

class function TcxProgressBar.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxProgressBarProperties;
end;

function TcxProgressBar.GetActiveProperties: TcxProgressBarProperties;
begin
  Result := TcxProgressBarProperties(InternalGetActiveProperties);
end;

function TcxProgressBar.GetProperties: TcxProgressBarProperties;
begin
  Result := TcxProgressBarProperties(inherited Properties);
end;

procedure TcxProgressBar.SetProperties(Value: TcxProgressBarProperties);
begin
  Properties.Assign(Value);
end;

{ TcxFilterProgressBarHelper }

class procedure TcxFilterProgressBarHelper.InitializeProperties(AProperties,
  AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean);
begin
  inherited InitializeProperties(AProperties, AEditProperties, AHasButtons);
  with TcxCustomSpinEditProperties(AProperties) do
  begin
    Buttons.Add;
    Buttons.Add;
    MinValue := TcxCustomProgressBarProperties(AEditProperties).Min;
    MaxValue := TcxCustomProgressBarProperties(AEditProperties).Max;
  end;
end;

initialization
  GetRegisteredEditProperties.Register(TcxProgressBarProperties, scxSEditRepositoryProgressBarItem);
  FilterEditsController.Register(TcxProgressBarProperties, TcxFilterProgressBarHelper);

finalization
  FilterEditsController.Unregister(TcxProgressBarProperties, TcxFilterProgressBarHelper);
  GetRegisteredEditProperties.Unregister(TcxProgressBarProperties);

end.
