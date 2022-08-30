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

unit dxActivityIndicator;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Types, Classes, Graphics, cxControls, dxCoreGraphics, cxGraphics, cxGeometry, cxClasses, dxGDIPlusClasses,
  cxLookAndFeelPainters, cxLookAndFeels;

type
  TdxActivityIndicatorProperties = class;
  TdxActivityIndicatorViewInfo = class;
  TdxCustomActivityIndicator = class;

  { TdxActivityIndicatorCustomRule }

  TdxActivityIndicatorCustomRuleClass = class of TdxActivityIndicatorCustomRule;
  TdxActivityIndicatorCustomRule = class abstract(TObject)
  public
    class function CalculateGapBetweenElements(const ABounds: TdxRectF; const AElementSize: Double): Double; virtual;
  end;

  { TdxActivityIndicatorArcBasedRule }

  TdxActivityIndicatorArcBasedRuleClass = class of TdxActivityIndicatorArcBasedRule;
  TdxActivityIndicatorArcBasedRule = class(TdxActivityIndicatorCustomRule)
  public
    class procedure CalculateAngles(const AProgress: Double; out AAngle1, AAngle2: Double); virtual;
  end;

  { TdxActivityIndicatorDotBasedRule }

  TdxActivityIndicatorDotBasedRuleClass = class of TdxActivityIndicatorDotBasedRule;
  TdxActivityIndicatorDotBasedRule = class abstract(TdxActivityIndicatorCustomRule)
  public
    class function AllowHideDots: Boolean; virtual;
    class function CalculateDotSize(const ADotSize, AProgress: Double): Double; virtual;
    class function CalculatePosition(const ABounds: TdxRectF; const ADotSize, AProgress: Double): TdxPointF; virtual;
  end;

  { TdxActivityIndicatorElasticCircleRule }

  TdxActivityIndicatorElasticCircleRule = class(TdxActivityIndicatorArcBasedRule)
  public
    class procedure CalculateAngles(const AProgress: Double; out AAngle1, AAngle2: Double); override;
  end;

  { TdxActivityIndicatorHorizontalDotsRule }

  TdxActivityIndicatorHorizontalDotsRule = class(TdxActivityIndicatorDotBasedRule)
  protected
    class function ProgressTransform(const AProgress: Double): Double; virtual;
  public
    class function CalculateGapBetweenElements(const ABounds: TdxRectF; const AElementSize: Double): Double; override;
    class function CalculatePosition(const ABounds: TdxRectF; const ADotSize, AProgress: Double): TdxPointF; override;
  end;

  { TdxActivityIndicatorCircularDotsRule }

  TdxActivityIndicatorCircularDotsRule = class(TdxActivityIndicatorDotBasedRule)
  protected
    class function GetAngleOffset: Double; virtual;
    class function GetRotationCount: Integer; virtual;
    class function ProgressTransform(const AProgress: Double): Double; virtual;
  public
    class function CalculateGapBetweenElements(const ABounds: TdxRectF; const AElementSize: Double): Double; override;
    class function CalculatePosition(const ABounds: TdxRectF; const ADotSize, AProgress: Double): TdxPointF; override;
  end;

  { TdxActivityIndicatorGravityDotsRule }

  TdxActivityIndicatorGravityDotsRule = class(TdxActivityIndicatorCircularDotsRule)
  protected
    class function GetAngleOffset: Double; override;
    class function GetRotationCount: Integer; override;
    class function ProgressTransform(const AProgress: Double): Double; override;
  public
    class function AllowHideDots: Boolean; override;
    class function CalculateDotSize(const ADotSize, AProgress: Double): Double; override;
    class function CalculatePosition(const ABounds: TdxRectF; const ADotSize, AProgress: Double): TdxPointF; override;
  end;

  { TdxActivityIndicatorPersistent }

  TdxActivityIndicatorPersistent = class(TPersistent)
  strict private
    FOwner: TdxCustomActivityIndicator;
  protected
    property Owner: TdxCustomActivityIndicator read FOwner;
  public
    constructor Create(AOwner: TdxCustomActivityIndicator); virtual;
  end;

  { TdxActivityIndicatorProperties }

  TdxActivityIndicatorPropertiesClass = class of TdxActivityIndicatorProperties;
  TdxActivityIndicatorProperties = class(TdxActivityIndicatorPersistent)
  strict private
    function IsOverlayColorStored: Boolean;
    procedure SetAnimationTime(AValue: Cardinal);
    procedure SetAnimationRestartDelay(AValue: Cardinal);
    procedure SetOverlayColor(AValue: TdxAlphaColor);
  protected
    FAnimationTime: Cardinal;
    FAnimationRestartDelay: Cardinal;
    FOverlayColor: TdxAlphaColor;
    FRule: TdxActivityIndicatorCustomRuleClass;

    procedure Changed(AChangeType: TdxChangeType = ctHard); virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    function CreateViewInfo: TdxActivityIndicatorViewInfo; virtual; abstract;
    //
    property AnimationRestartDelay: Cardinal read FAnimationRestartDelay write SetAnimationRestartDelay default 0;
    property Rule: TdxActivityIndicatorCustomRuleClass read FRule;
  public
    constructor Create(AOwner: TdxCustomActivityIndicator); override;
    procedure Assign(Source: TPersistent); override;
  published
    property AnimationTime: Cardinal read FAnimationTime write SetAnimationTime default 2000;
    property OverlayColor: TdxAlphaColor read FOverlayColor write SetOverlayColor stored IsOverlayColorStored;
  end;

  { TdxActivityIndicatorDotBasedProperties }

  TdxActivityIndicatorDotCount = 1..10;
  TdxActivityIndicatorDotSize  = 2..50;

  TdxActivityIndicatorDotBasedProperties = class(TdxActivityIndicatorProperties)
  strict private
    FDotColor: TdxAlphaColor;
    FDotCount: TdxActivityIndicatorDotCount;
    FDotSize: TdxActivityIndicatorDotSize;

    function IsDotColorStored: Boolean;
    procedure SetDotColor(AValue: TdxAlphaColor);
    procedure SetDotCount(AValue: TdxActivityIndicatorDotCount);
    procedure SetDotSize(AValue: TdxActivityIndicatorDotSize);
  protected
    procedure ChangeScale(M, D: Integer); override;
    function CreateViewInfo: TdxActivityIndicatorViewInfo; override;
  public
    constructor Create(AOwner: TdxCustomActivityIndicator); override;
    procedure Assign(Source: TPersistent); override;
  published
    property AnimationRestartDelay;
    property DotColor: TdxAlphaColor read FDotColor write SetDotColor stored IsDotColorStored;
    property DotCount: TdxActivityIndicatorDotCount read FDotCount write SetDotCount default 5;
    property DotSize: TdxActivityIndicatorDotSize read FDotSize write SetDotSize default 6;
  end;

  { TdxActivityIndicatorCircularDotsProperties }

  TdxActivityIndicatorCircularDotsProperties = class(TdxActivityIndicatorDotBasedProperties)
  public
    constructor Create(AOwner: TdxCustomActivityIndicator); override;
  published
    property AnimationTime default 4000;
  end;

  { TdxActivityIndicatorHorizontalDotsProperties }

  TdxActivityIndicatorHorizontalDotsProperties = class(TdxActivityIndicatorDotBasedProperties)
  public
    constructor Create(AOwner: TdxCustomActivityIndicator); override;
  end;

  { TdxActivityIndicatorGravityDotsProperties }

  TdxActivityIndicatorGravityDotsProperties = class(TdxActivityIndicatorDotBasedProperties)
  public
    constructor Create(AOwner: TdxCustomActivityIndicator); override;
  end;

  { TdxActivityIndicatorArcBasedProperties }

  TdxActivityIndicatorArcBasedProperties = class(TdxActivityIndicatorProperties)
  strict private
    FArcColor: TdxAlphaColor;
    FArcThickness: Integer;

    function IsArcColorStored: Boolean;
    procedure SetArcColor(AValue: TdxAlphaColor);
    procedure SetArcThickness(AValue: Integer);
  protected
    procedure ChangeScale(M, D: Integer); override;
    function CreateViewInfo: TdxActivityIndicatorViewInfo; override;
  public
    constructor Create(AOwner: TdxCustomActivityIndicator); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ArcColor: TdxAlphaColor read FArcColor write SetArcColor stored IsArcColorStored;
    property ArcThickness: Integer read FArcThickness write SetArcThickness default 3;
  end;

  { TdxActivityIndicatorElasticCircleProperties }

  TdxActivityIndicatorElasticCircleProperties = class(TdxActivityIndicatorArcBasedProperties)
  public
    constructor Create(AOwner: TdxCustomActivityIndicator); override;
  end;

  { TdxActivityIndicatorAnimationController }

  TdxActivityIndicatorAnimationController = class(TdxActivityIndicatorPersistent)
  strict private
    FGapBetweenElements: Double;
    FProgress: Double;
    FTimeFinish: Int64;
    FTimeFinishOfFirstElement: Int64;
    FTimer: TcxTimer;
    FTimeStart: Int64;

    function GetActive: Boolean;
    function GetProperties: TdxActivityIndicatorProperties; inline;
    function GetViewInfo: TdxActivityIndicatorViewInfo; inline;
    procedure SetActive(AValue: Boolean);
    procedure UpdateProgress;
    //
    procedure TimerHandler(Sender: TObject);
  public
    constructor Create(AOwner: TdxCustomActivityIndicator); override;
    destructor Destroy; override;
    function CalculateProgress(AElementIndex: Integer): Double;
    procedure ResetTimer;
    procedure UpdateParams;
    //
    property Active: Boolean read GetActive write SetActive;
    property Properties: TdxActivityIndicatorProperties read GetProperties;
    property ViewInfo: TdxActivityIndicatorViewInfo read GetViewInfo;
  end;

  { TdxActivityIndicatorViewInfo }

  TdxActivityIndicatorViewInfo = class(TdxActivityIndicatorPersistent)
  strict private
    FBounds: TdxRectF;
    FOverlayBrush: TdxGPBrush;

    function GetProperties: TdxActivityIndicatorProperties;
  protected
    FElementCount: Integer;
    FElementSize: Single;

    function GetDefaultElementColor: TdxAlphaColor;
    procedure DrawContent(ACanvas: TdxGPCanvas); virtual; abstract;
  public
    constructor Create(AOwner: TdxCustomActivityIndicator); override;
    destructor Destroy; override;
    procedure Calculate(const ABounds: TdxRectF); virtual;
    procedure Draw(ACanvas: TdxGPCanvas); virtual;
    //
    property Bounds: TdxRectF read FBounds;
    property ElementCount: Integer read FElementCount;
    property ElementSize: Single read FElementSize;
    property Properties: TdxActivityIndicatorProperties read GetProperties;
    property OverlayBrush: TdxGPBrush read FOverlayBrush;
  end;

  { TdxActivityIndicatorArcBasedViewInfo }

  TdxActivityIndicatorArcBasedViewInfo = class(TdxActivityIndicatorViewInfo)
  strict private
    function GetProperties: TdxActivityIndicatorArcBasedProperties;
    function GetRule: TdxActivityIndicatorArcBasedRuleClass;
  protected
    FArcArea: TRect;
    FArcColor: TdxAlphaColor;
    FArcThickness: Single;

    procedure DrawContent(ACanvas: TdxGPCanvas); override;
  public
    procedure Calculate(const ABounds: TdxRectF); override;
    //
    property ArcArea: TRect read FArcArea;
    property ArcColor: TdxAlphaColor read FArcColor;
    property ArcThickness: Single read FArcThickness;
    property Properties: TdxActivityIndicatorArcBasedProperties read GetProperties;
    property Rule: TdxActivityIndicatorArcBasedRuleClass read GetRule;
  end;

  { TdxActivityIndicatorDotBasedViewInfo }

  TdxActivityIndicatorDotBasedViewInfo = class(TdxActivityIndicatorViewInfo)
  strict private
    FDotBrush: TdxGPBrush;

    function GetProperties: TdxActivityIndicatorDotBasedProperties;
    function GetRule: TdxActivityIndicatorDotBasedRuleClass;
  protected
    procedure DrawContent(ACanvas: TdxGPCanvas); override;
  public
    constructor Create(AOwner: TdxCustomActivityIndicator); override;
    destructor Destroy; override;
    procedure Calculate(const ABounds: TdxRectF); override;
    //
    property DotBrush: TdxGPBrush read FDotBrush;
    property Properties: TdxActivityIndicatorDotBasedProperties read GetProperties;
    property Rule: TdxActivityIndicatorDotBasedRuleClass read GetRule;
  end;

  { TdxCustomActivityIndicator }

  TdxCustomActivityIndicator = class(TcxControl, IdxSkinSupport)
  strict private
    FAnimationController: TdxActivityIndicatorAnimationController;
    FAntialiasing: Boolean;
    FProperties: TdxActivityIndicatorProperties;
    FViewInfo: TdxActivityIndicatorViewInfo;

    function GetActive: Boolean;
    function GetPropertiesClass: TdxActivityIndicatorPropertiesClass;
    function GetPropertiesClassName: string;
    procedure SetActive(AValue: Boolean);
    procedure SetAntialiasing(AValue: Boolean);
    procedure SetProperties(AValue: TdxActivityIndicatorProperties);
    procedure SetPropertiesClass(AValue: TdxActivityIndicatorPropertiesClass);
    procedure SetPropertiesClassName(const AValue: string);
  protected
    procedure BoundsChanged; override;
    procedure Calculate; virtual;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    function CreateAnimationController: TdxActivityIndicatorAnimationController; virtual;
    procedure EraseBackground(ACanvas: TcxCanvas; const ARect: TRect); override;
    procedure DoPaint; override;
    function IsDoubleBufferedNeeded: Boolean; override;
    function IsTransparentBackground: Boolean; override;
    procedure Loaded; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    //
    property Active: Boolean read GetActive write SetActive default False;
    property AnimationController: TdxActivityIndicatorAnimationController read FAnimationController;
    property Antialiasing: Boolean read FAntialiasing write SetAntialiasing default True;
    property Properties: TdxActivityIndicatorProperties read FProperties write SetProperties;
    property PropertiesClass: TdxActivityIndicatorPropertiesClass read GetPropertiesClass write SetPropertiesClass;
    property PropertiesClassName: string read GetPropertiesClassName write SetPropertiesClassName;
    property ViewInfo: TdxActivityIndicatorViewInfo read FViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    //
    property TabStop default False;
  end;

  { TdxActivityIndicator }

  TdxActivityIndicator = class(TdxCustomActivityIndicator)
  public
    property PropertiesClass;
  published
    property Align;
    property Anchors;
    property Antialiasing;
    property BorderStyle default cxcbsNone;
    property LookAndFeel;
    property PropertiesClassName;
    property Properties;
    property Active;
    property Transparent;
    property Visible;
    //
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

function GetRegisteredActivityIndicatorProperties: TcxRegisteredClasses;
implementation

uses
  SysUtils, Math, dxAnimation;

var
  FRegisteredProperties: TcxRegisteredClasses;

function GetRegisteredActivityIndicatorProperties: TcxRegisteredClasses;
begin
  if FRegisteredProperties = nil then
  begin
    FRegisteredProperties := TcxRegisteredClasses.Create;
    FRegisteredProperties.Register(TdxActivityIndicatorHorizontalDotsProperties, 'Horizontal Dots');
    FRegisteredProperties.Register(TdxActivityIndicatorCircularDotsProperties, 'Circular Dots');
    FRegisteredProperties.Register(TdxActivityIndicatorGravityDotsProperties, 'Gravity Dots');
    FRegisteredProperties.Register(TdxActivityIndicatorElasticCircleProperties, 'Elastic Circle');
  end;
  Result := FRegisteredProperties;
end;

{ TdxActivityIndicatorCustomRule }

class function TdxActivityIndicatorCustomRule.CalculateGapBetweenElements(
  const ABounds: TdxRectF; const AElementSize: Double): Double;
begin
  Result := 0;
end;

{ TdxActivityIndicatorArcBasedRule }

class procedure TdxActivityIndicatorArcBasedRule.CalculateAngles(const AProgress: Double; out AAngle1, AAngle2: Double);
begin
  AAngle1 := 0;
  AAngle2 := 0;
end;

{ TdxActivityIndicatorDotBasedRule }

class function TdxActivityIndicatorDotBasedRule.AllowHideDots: Boolean;
begin
  Result := True;
end;

class function TdxActivityIndicatorDotBasedRule.CalculateDotSize(const ADotSize, AProgress: Double): Double;
begin
  Result := ADotSize;
end;

class function TdxActivityIndicatorDotBasedRule.CalculatePosition(
  const ABounds: TdxRectF; const ADotSize, AProgress: Double): TdxPointF;
begin
  Result := ABounds.TopLeft;
end;

{ TdxActivityIndicatorElasticCircleRule }

class procedure TdxActivityIndicatorElasticCircleRule.CalculateAngles(const AProgress: Double; out AAngle1, AAngle2: Double);

  function PointAtLine(const AProgress, X1, X2: Double): Double;
  begin
    Result := X1 * (1.0 - AProgress) + AProgress * X2;
  end;

  function CalcPos(const X, S1, S2: Double): Double;
  const
    NormalSpeedFactor = 0.55;
  begin
    if X < S1 then
      Result := X * NormalSpeedFactor
    else
      if X > S2 then
        Result := PointAtLine((X - S2) / (1.0 - S2), 1.0 - (1.0 - S2) * NormalSpeedFactor, 1.0)
      else
        Result := PointAtLine(Power((X - S1) / (S2 - S1), 1.5), S1 * NormalSpeedFactor, 1.0 - (1.0 - S2) * NormalSpeedFactor);
  end;

begin
  AAngle1 := 720 * CalcPos(1.0 - AProgress, 0.2, 0.5);
  AAngle2 := 720 * CalcPos(1.0 - AProgress, 0.7, 1.0) - 10;

end;

{ TdxActivityIndicatorHorizontalDotsRule }

class function TdxActivityIndicatorHorizontalDotsRule.CalculateGapBetweenElements(
  const ABounds: TdxRectF; const AElementSize: Double): Double;
begin
  Result := ABounds.Width;
  if Result > 0 then
    Result := 5 * AElementSize / Result;
end;

class function TdxActivityIndicatorHorizontalDotsRule.CalculatePosition(
  const ABounds: TdxRectF; const ADotSize, AProgress: Double): TdxPointF;
begin
  Result := cxPointF(ProgressTransform(AProgress), 0.5);
  Result.X := ABounds.Left + ABounds.Width * Result.X;
  Result.Y := ABounds.Top + ABounds.Height * Result.Y;
end;

class function TdxActivityIndicatorHorizontalDotsRule.ProgressTransform(const AProgress: Double): Double;
const
  Section1: Double = 0.2;
  Section2: Double = 0.8;
const
  Exactitude = 3;
begin
  if (AProgress > Section1) and (AProgress < Section2) then
  begin
    Result := (AProgress - Section1) / (Section2 - Section1);
    Result := ProgressTransform(Section1) * (1 - Result) + ProgressTransform(Section2) * Result;
  end
  else
    Result := 1 / (2 * SinH(Exactitude)) * (SinH(AProgress * (2 * Exactitude) - Exactitude) - SinH(-Exactitude));
end;

{ TdxActivityIndicatorCircularDotsRule }

class function TdxActivityIndicatorCircularDotsRule.CalculateGapBetweenElements(
  const ABounds: TdxRectF; const AElementSize: Double): Double;
begin
  Result := Min(ABounds.Width, ABounds.Height);
  if Result > 0 then
    Result := 0.6 * AElementSize / Result;
end;

class function TdxActivityIndicatorCircularDotsRule.CalculatePosition(
  const ABounds: TdxRectF; const ADotSize, AProgress: Double): TdxPointF;
const
  Indent = 3;
var
  ARadius: Double;
  AValue: Double;
begin
  AValue := AProgress * GetRotationCount;
  AValue := AValue - Floor(AValue);
  AValue := ProgressTransform(AValue);

  ARadius := (Min(ABounds.Width, ABounds.Height) - ADotSize - Indent) / 2;

  Result.X := ABounds.Left + ABounds.Width / 2;
  Result.Y := ABounds.Top + ABounds.Height / 2;

  Result.X := Result.X + ARadius * Cos(AValue * 2 * Pi + GetAngleOffset);
  Result.Y := Result.Y + ARadius * Sin(AValue * 2 * Pi + GetAngleOffset);
end;

class function TdxActivityIndicatorCircularDotsRule.GetAngleOffset: Double;
begin
  Result := Pi / 2 - Pi / 6;
end;

class function TdxActivityIndicatorCircularDotsRule.GetRotationCount: Integer;
begin
  Result := 2;
end;

class function TdxActivityIndicatorCircularDotsRule.ProgressTransform(const AProgress: Double): Double;
const
  Exactitude = 3;
begin
  Result := 1 / (2 * SinH(Exactitude)) * (SinH(AProgress * (2 * Exactitude) - Exactitude) - SinH(-Exactitude));
end;

{ TdxActivityIndicatorGravityDotsRule }

class function TdxActivityIndicatorGravityDotsRule.AllowHideDots: Boolean;
begin
  Result := False;
end;

class function TdxActivityIndicatorGravityDotsRule.CalculateDotSize(const ADotSize, AProgress: Double): Double;
begin
  if AProgress < 0.5 then
    Result := 1 - 2 * AProgress
  else
    Result := 2 * AProgress - 1;

  Result := ADotSize * (1 + 3 * TanH(Result));
end;

class function TdxActivityIndicatorGravityDotsRule.CalculatePosition(
  const ABounds: TdxRectF; const ADotSize, AProgress: Double): TdxPointF;
begin
  Result := inherited CalculatePosition(ABounds, 3 * ADotSize, AProgress);
  Result.Y := ABounds.Bottom - (Result.Y - ABounds.Top);
end;

class function TdxActivityIndicatorGravityDotsRule.GetAngleOffset: Double;
begin
  Result := Pi / 2;
end;

class function TdxActivityIndicatorGravityDotsRule.GetRotationCount: Integer;
begin
  Result := 1;
end;

class function TdxActivityIndicatorGravityDotsRule.ProgressTransform(const AProgress: Double): Double;
const
  Exponent = 3;
begin
  Result := 1.0 - AProgress;
  if Result >= 0.5 then
    Result := (1.0 - Power((1.0 - Result) * 2.0, Exponent)) * 0.5 + 0.5
  else
    Result := Power(Result * 2.0, Exponent) * 0.5;
end;

{ TdxActivityIndicatorPersistent }

constructor TdxActivityIndicatorPersistent.Create(AOwner: TdxCustomActivityIndicator);
begin
  inherited Create;
  FOwner := AOwner;
end;

{ TdxActivityIndicatorProperties }

constructor TdxActivityIndicatorProperties.Create(AOwner: TdxCustomActivityIndicator);
begin
  inherited Create(AOwner);
  FAnimationTime := 2000;
  FOverlayColor := TdxAlphaColors.Default;
end;

procedure TdxActivityIndicatorProperties.Assign(Source: TPersistent);
begin
  if Source is TdxActivityIndicatorProperties then
  begin
    FAnimationTime := TdxActivityIndicatorProperties(Source).AnimationTime;
    FOverlayColor := TdxActivityIndicatorProperties(Source).OverlayColor;
    FAnimationRestartDelay := TdxActivityIndicatorProperties(Source).AnimationRestartDelay;
    Changed;
  end;
end;

procedure TdxActivityIndicatorProperties.Changed(AChangeType: TdxChangeType = ctHard);
begin
  if AChangeType >= ctHard then
    Owner.AnimationController.ResetTimer;
  if AChangeType >= ctMedium then
    Owner.Calculate;
  Owner.Invalidate;
end;

procedure TdxActivityIndicatorProperties.ChangeScale(M, D: Integer);
begin
  // do nothing
end;

function TdxActivityIndicatorProperties.IsOverlayColorStored: Boolean;
begin
  Result := OverlayColor <> TdxAlphaColors.Default;
end;

procedure TdxActivityIndicatorProperties.SetAnimationTime(AValue: Cardinal);
begin
  AValue := Max(AValue, 1);
  if FAnimationTime <> AValue then
  begin
    FAnimationTime := AValue;
    Changed;
  end;
end;

procedure TdxActivityIndicatorProperties.SetAnimationRestartDelay(AValue: Cardinal);
begin
  if FAnimationRestartDelay <> AValue then
  begin
    FAnimationRestartDelay := AValue;
    Changed;
  end;
end;

procedure TdxActivityIndicatorProperties.SetOverlayColor(AValue: TdxAlphaColor);
begin
  if FOverlayColor <> AValue then
  begin
    FOverlayColor := AValue;
    Changed(ctMedium);
  end;
end;

{ TdxActivityIndicatorDotBasedProperties }

constructor TdxActivityIndicatorDotBasedProperties.Create(AOwner: TdxCustomActivityIndicator);
begin
  inherited Create(AOwner);
  FDotColor := TdxAlphaColors.Default;
  FDotCount := 5;
  FDotSize := 6;
end;

procedure TdxActivityIndicatorDotBasedProperties.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxActivityIndicatorDotBasedProperties then
  begin
    FDotColor := TdxActivityIndicatorDotBasedProperties(Source).DotColor;
    FDotCount := TdxActivityIndicatorDotBasedProperties(Source).DotCount;
    FDotSize := TdxActivityIndicatorDotBasedProperties(Source).DotSize;
    Changed;
  end;
end;

procedure TdxActivityIndicatorDotBasedProperties.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  DotSize := MulDiv(DotSize, M, D);
end;

function TdxActivityIndicatorDotBasedProperties.CreateViewInfo: TdxActivityIndicatorViewInfo;
begin
  Result := TdxActivityIndicatorDotBasedViewInfo.Create(Owner);
end;

function TdxActivityIndicatorDotBasedProperties.IsDotColorStored: Boolean;
begin
  Result := DotColor <> TdxAlphaColors.Default;
end;

procedure TdxActivityIndicatorDotBasedProperties.SetDotColor(AValue: TdxAlphaColor);
begin
  if FDotColor <> AValue then
  begin
    FDotColor := AValue;
    Changed(ctMedium);
  end;
end;

procedure TdxActivityIndicatorDotBasedProperties.SetDotCount(AValue: TdxActivityIndicatorDotCount);
begin
  if FDotCount <> AValue then
  begin
    FDotCount := AValue;
    Changed(ctMedium);
  end;
end;

procedure TdxActivityIndicatorDotBasedProperties.SetDotSize(AValue: TdxActivityIndicatorDotSize);
begin
  AValue := Max(Min(AValue, High(TdxActivityIndicatorDotSize)), Low(TdxActivityIndicatorDotSize));
  if FDotSize <> AValue then
  begin
    FDotSize := AValue;
    Changed(ctMedium);
  end;
end;

{ TdxActivityIndicatorCircularDotsProperties }

constructor TdxActivityIndicatorCircularDotsProperties.Create(AOwner: TdxCustomActivityIndicator);
begin
  inherited Create(AOwner);
  FAnimationTime := 4000;
  FRule := TdxActivityIndicatorCircularDotsRule;
end;

{ TdxActivityIndicatorHorizontalDotsProperties }

constructor TdxActivityIndicatorHorizontalDotsProperties.Create(AOwner: TdxCustomActivityIndicator);
begin
  inherited Create(AOwner);
  FRule := TdxActivityIndicatorHorizontalDotsRule;
end;

{ TdxActivityIndicatorGravityDotsProperties }

constructor TdxActivityIndicatorGravityDotsProperties.Create(AOwner: TdxCustomActivityIndicator);
begin
  inherited Create(AOwner);
  FRule := TdxActivityIndicatorGravityDotsRule;
end;

{ TdxActivityIndicatorArcBasedProperties }

constructor TdxActivityIndicatorArcBasedProperties.Create(AOwner: TdxCustomActivityIndicator);
begin
  inherited Create(AOwner);
  FArcColor := TdxAlphaColors.Default;
  FArcThickness := 3;
end;

procedure TdxActivityIndicatorArcBasedProperties.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxActivityIndicatorArcBasedProperties then
  begin
    ArcColor := TdxActivityIndicatorArcBasedProperties(Source).ArcColor;
    ArcThickness := TdxActivityIndicatorArcBasedProperties(Source).ArcThickness;
  end;
end;

procedure TdxActivityIndicatorArcBasedProperties.ChangeScale(M, D: Integer);
begin
  inherited;
  ArcThickness := MulDiv(ArcThickness, M, D);
end;

function TdxActivityIndicatorArcBasedProperties.CreateViewInfo: TdxActivityIndicatorViewInfo;
begin
  Result := TdxActivityIndicatorArcBasedViewInfo.Create(Owner);
end;

function TdxActivityIndicatorArcBasedProperties.IsArcColorStored: Boolean;
begin
  Result := FArcColor <> TdxAlphaColors.Default;
end;

procedure TdxActivityIndicatorArcBasedProperties.SetArcColor(AValue: TdxAlphaColor);
begin
  if ArcColor <> AValue then
  begin
    FArcColor := AValue;
    Changed(ctMedium);
  end;
end;

procedure TdxActivityIndicatorArcBasedProperties.SetArcThickness(AValue: Integer);
begin
  AValue := Max(AValue, 1);
  if ArcThickness <> AValue then
  begin
    FArcThickness := AValue;
    Changed(ctMedium);
  end;
end;

{ TdxActivityIndicatorElasticCircleProperties }

constructor TdxActivityIndicatorElasticCircleProperties.Create(AOwner: TdxCustomActivityIndicator);
begin
  inherited Create(AOwner);
  FRule := TdxActivityIndicatorElasticCircleRule;
end;

{ TdxActivityIndicatorAnimationController }

constructor TdxActivityIndicatorAnimationController.Create(AOwner: TdxCustomActivityIndicator);
begin
  inherited Create(AOwner);
  FTimer := cxCreateTimer(TimerHandler, 1, False);
end;

destructor TdxActivityIndicatorAnimationController.Destroy;
begin
  FreeAndNil(FTimer);
  inherited Destroy;
end;

function TdxActivityIndicatorAnimationController.CalculateProgress(AElementIndex: Integer): Double;
begin
  Result := FProgress - AElementIndex * FGapBetweenElements;
end;

procedure TdxActivityIndicatorAnimationController.ResetTimer;
begin
  if Active then
  begin
    FTimeStart := dxGetExactTickCount;
    FTimeFinish := FTimeStart + dxTimeToTickCount(Properties.AnimationTime + Properties.AnimationRestartDelay);
    UpdateParams;
    UpdateProgress;
  end;
end;

procedure TdxActivityIndicatorAnimationController.UpdateParams;
begin
  if Active then
  begin
    FGapBetweenElements := Properties.Rule.CalculateGapBetweenElements(ViewInfo.Bounds, ViewInfo.ElementSize);
    FTimeFinishOfFirstElement := FTimeStart +
      Trunc(dxTimeToTickCount(Properties.AnimationTime) / (1 + (ViewInfo.ElementCount - 1) * FGapBetweenElements));
  end;
end;

function TdxActivityIndicatorAnimationController.GetActive: Boolean;
begin
  Result := FTimer.Enabled;
end;

function TdxActivityIndicatorAnimationController.GetProperties: TdxActivityIndicatorProperties;
begin
  Result := ViewInfo.Properties;
end;

function TdxActivityIndicatorAnimationController.GetViewInfo: TdxActivityIndicatorViewInfo;
begin
  Result := Owner.ViewInfo;
end;

procedure TdxActivityIndicatorAnimationController.SetActive(AValue: Boolean);
begin
  AValue := AValue and (ViewInfo <> nil);
  if Active <> AValue then
  begin
    FTimer.Enabled := AValue;
    ResetTimer;
    Owner.Invalidate;
  end;
end;

procedure TdxActivityIndicatorAnimationController.UpdateProgress;
var
  APosition: Int64;
begin
  APosition := Min(dxGetExactTickCount, FTimeFinish);
  FProgress := (APosition - FTimeStart) / (FTimeFinishOfFirstElement - FTimeStart);

  Owner.Invalidate;
  Owner.Update;

  if APosition >= FTimeFinish then
    ResetTimer;
end;

procedure TdxActivityIndicatorAnimationController.TimerHandler(Sender: TObject);
begin
  UpdateProgress;
end;

{ TdxActivityIndicatorViewInfo }

constructor TdxActivityIndicatorViewInfo.Create(AOwner: TdxCustomActivityIndicator);
begin
  inherited Create(AOwner);
  FOverlayBrush := TdxGPBrush.Create;
  FElementCount := 1;
end;

destructor TdxActivityIndicatorViewInfo.Destroy;
begin
  FreeAndNil(FOverlayBrush);
  inherited Destroy;
end;

procedure TdxActivityIndicatorViewInfo.Calculate(const ABounds: TdxRectF);
begin
  FBounds := ABounds;

  OverlayBrush.Color := Properties.OverlayColor;
  if OverlayBrush.Color = TdxAlphaColors.Default then
    OverlayBrush.Color := TdxAlphaColors.Empty;
end;

procedure TdxActivityIndicatorViewInfo.Draw(ACanvas: TdxGPCanvas);
begin
  if not cxRectIsEmpty(Bounds) then
  begin
    ACanvas.FillRectangle(cxRectInflate(Bounds, 1.0), OverlayBrush);
    if Owner.Active then
      DrawContent(ACanvas);
  end;
end;

function TdxActivityIndicatorViewInfo.GetDefaultElementColor: TdxAlphaColor;
begin
  if cxColorIsValid(Owner.LookAndFeelPainter.PanelTextColor) then
    Result := dxColorToAlphaColor(Owner.LookAndFeelPainter.PanelTextColor)
  else
    Result := dxColorToAlphaColor(Owner.LookAndFeelPainter.DefaultContentTextColor);
end;

function TdxActivityIndicatorViewInfo.GetProperties: TdxActivityIndicatorProperties;
begin
  Result := Owner.Properties;
end;

{ TdxActivityIndicatorArcBasedViewInfo }

procedure TdxActivityIndicatorArcBasedViewInfo.Calculate(const ABounds: TdxRectF);
const
  Indent = 3;
var
  ARadius: Single;
begin
  inherited Calculate(ABounds);
  ARadius := (Min(Bounds.Width, Bounds.Height) - Properties.ArcThickness - Indent) / 2;
  FArcArea := cxRect(cxRectCenter(ABounds, ARadius, ARadius));
  FArcThickness := Properties.ArcThickness;

  FArcColor := Properties.ArcColor;
  if FArcColor = TdxAlphaColors.Default then
    FArcColor := GetDefaultElementColor;
end;

procedure TdxActivityIndicatorArcBasedViewInfo.DrawContent(ACanvas: TdxGPCanvas);
var
  AAngle1, AAngle2: Double;
begin
  if (cxRectWidth(ArcArea) > 2 * ArcThickness) and (cxRectHeight(ArcArea) > 2 * ArcThickness) then
  begin
    Rule.CalculateAngles(Owner.AnimationController.CalculateProgress(0), AAngle1, AAngle2);
    ACanvas.Arc(ArcArea, AAngle1, AAngle2 - AAngle1, ArcColor, ArcThickness);
  end;
end;

function TdxActivityIndicatorArcBasedViewInfo.GetProperties: TdxActivityIndicatorArcBasedProperties;
begin
  Result := TdxActivityIndicatorArcBasedProperties(inherited Properties);
end;

function TdxActivityIndicatorArcBasedViewInfo.GetRule: TdxActivityIndicatorArcBasedRuleClass;
begin
  Result := TdxActivityIndicatorArcBasedRuleClass(Properties.Rule);
end;

{ TdxActivityIndicatorDotBasedViewInfo }

constructor TdxActivityIndicatorDotBasedViewInfo.Create(AOwner: TdxCustomActivityIndicator);
begin
  inherited Create(AOwner);
  FDotBrush := TdxGPBrush.Create;
end;

destructor TdxActivityIndicatorDotBasedViewInfo.Destroy;
begin
  FreeAndNil(FDotBrush);
  inherited Destroy;
end;

procedure TdxActivityIndicatorDotBasedViewInfo.Calculate(const ABounds: TdxRectF);
begin
  inherited Calculate(ABounds);

  FElementSize := Properties.DotSize;
  FElementCount := Properties.DotCount;

  DotBrush.Color := Properties.DotColor;
  if DotBrush.Color = TdxAlphaColors.Default then
    DotBrush.Color := GetDefaultElementColor;
end;

procedure TdxActivityIndicatorDotBasedViewInfo.DrawContent(ACanvas: TdxGPCanvas);
var
  ADotSize: Double;
  APoint: TdxPointF;
  AProgress: Double;
  I: Integer;
begin
  for I := 0 to ElementCount - 1 do
  begin
    AProgress := Owner.AnimationController.CalculateProgress(I);
    if not Rule.AllowHideDots then
      AProgress := Min(AProgress, 1.0);
    if (AProgress >= 0.0) and (AProgress <= 1.0) then
    begin
      APoint := Rule.CalculatePosition(Bounds, ElementSize, AProgress);
      ADotSize := Rule.CalculateDotSize(ElementSize, AProgress);
      ACanvas.Ellipse(cxRectCenter(cxRectF(APoint, APoint), ADotSize, ADotSize), nil, DotBrush);
    end;
  end;
end;

function TdxActivityIndicatorDotBasedViewInfo.GetProperties: TdxActivityIndicatorDotBasedProperties;
begin
  Result := TdxActivityIndicatorDotBasedProperties(inherited Properties);
end;

function TdxActivityIndicatorDotBasedViewInfo.GetRule: TdxActivityIndicatorDotBasedRuleClass;
begin
  Result := TdxActivityIndicatorDotBasedRuleClass(Properties.Rule);
end;

{ TdxCustomActivityIndicator }

constructor TdxCustomActivityIndicator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAntialiasing := True;
  FAnimationController := CreateAnimationController;
  SetBounds(Left, Top, 300, 100);
  TabStop := False;
end;

destructor TdxCustomActivityIndicator.Destroy;
begin
  FreeAndNil(FAnimationController);
  FreeAndNil(FProperties);
  FreeAndNil(FViewInfo);
  inherited Destroy;
end;

procedure TdxCustomActivityIndicator.AfterConstruction;
begin
  inherited AfterConstruction;
  if GetRegisteredActivityIndicatorProperties.Count > 0 then
    PropertiesClassName := GetRegisteredActivityIndicatorProperties.Items[0].ClassName;
end;

procedure TdxCustomActivityIndicator.BoundsChanged;
begin
  inherited BoundsChanged;
  Calculate;
end;

procedure TdxCustomActivityIndicator.Calculate;
begin
  if ViewInfo <> nil then
    ViewInfo.Calculate(dxRectF(ClientBounds));
  AnimationController.UpdateParams;
end;

procedure TdxCustomActivityIndicator.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  if Properties <> nil then
    Properties.ChangeScale(M, D);
end;

function TdxCustomActivityIndicator.CreateAnimationController: TdxActivityIndicatorAnimationController;
begin
  Result := TdxActivityIndicatorAnimationController.Create(Self);
end;

procedure TdxCustomActivityIndicator.EraseBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  if IsTransparentBackground then
    cxDrawTransparentControlBackground(Self, ACanvas, ARect)
  else
    LookAndFeelPainter.DrawPanelContent(ACanvas, ARect, False);
end;

procedure TdxCustomActivityIndicator.DoPaint;
const
  SmoothingModeMap: array[Boolean] of TdxGPSmoothingMode = (smHighSpeed, smAntiAlias);
begin
  inherited DoPaint;

  if ViewInfo = nil then
    Exit;

  dxGPPaintCanvas.BeginPaint(Canvas.Handle, ClientBounds);
  try
    dxGPPaintCanvas.SmoothingMode := SmoothingModeMap[Antialiasing];
    ViewInfo.Draw(dxGPPaintCanvas);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TdxCustomActivityIndicator.Loaded;
begin
  inherited Loaded;
  Calculate;
end;

procedure TdxCustomActivityIndicator.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  Calculate;
end;

function TdxCustomActivityIndicator.IsDoubleBufferedNeeded: Boolean;
begin
  Result := True;
end;

function TdxCustomActivityIndicator.IsTransparentBackground: Boolean;
begin
  Result := Transparent;
end;

function TdxCustomActivityIndicator.GetActive: Boolean;
begin
  Result := AnimationController.Active;
end;

function TdxCustomActivityIndicator.GetPropertiesClass: TdxActivityIndicatorPropertiesClass;
begin
  if Properties <> nil then
    Result := TdxActivityIndicatorPropertiesClass(Properties.ClassType)
  else
    Result := nil;
end;

function TdxCustomActivityIndicator.GetPropertiesClassName: string;
begin
  if Properties <> nil then
    Result := Properties.ClassName
  else
    Result := '';
end;

procedure TdxCustomActivityIndicator.SetActive(AValue: Boolean);
begin
  AnimationController.Active := AValue;
end;

procedure TdxCustomActivityIndicator.SetAntialiasing(AValue: Boolean);
begin
  if FAntialiasing <> AValue then
  begin
    FAntialiasing := AValue;
    Invalidate;
  end;
end;

procedure TdxCustomActivityIndicator.SetProperties(AValue: TdxActivityIndicatorProperties);
begin
  if AValue <> nil then
  begin
    PropertiesClassName := AValue.ClassName;
    FProperties.Assign(AValue);
  end;
end;

procedure TdxCustomActivityIndicator.SetPropertiesClass(AValue: TdxActivityIndicatorPropertiesClass);
var
  APrevActive: Boolean;
begin
  if PropertiesClass <> AValue then
  begin
    APrevActive := Active;
    try
      Active := False;
      FreeAndNil(FProperties);
      FreeAndNil(FViewInfo);

      if AValue <> nil then
        FProperties := AValue.Create(Self);
      if Properties <> nil then
        FViewInfo := Properties.CreateViewInfo;

      Calculate;
    finally
      Active := APrevActive;
    end;
  end;
end;

procedure TdxCustomActivityIndicator.SetPropertiesClassName(const AValue: string);
begin
  if (AValue <> '') and (GetRegisteredActivityIndicatorProperties.FindByClassName(AValue) = nil) then
    Exit;
  PropertiesClass := TdxActivityIndicatorPropertiesClass(GetRegisteredActivityIndicatorProperties.FindByClassName(AValue));
end;

initialization

finalization
  FreeAndNil(FRegisteredProperties);

end.
