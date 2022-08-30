{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressGaugeControl                                      }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSGAUGECONTROL AND ALL           }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxGaugeCircularScale;

{$I cxVer.inc}

interface

uses
  Windows, Classes, Graphics, cxGeometry, dxCoreGraphics, cxGraphics, dxCore, dxXMLDoc, dxGDIPlusClasses,
  dxCompositeShape, dxGaugeCustomScale, dxGaugeQuantitativeScale;

type
  TdxGaugeCustomArcQuantitativeScale = class;
  TdxGaugeArcQuantitativeScale = class;
  TdxGaugeCustomCircularScaleRange = class;
  TdxGaugeCircularWideScaleRange = class;

  TdxGaugeArcQuantitativeScaleParameterValue = (spvAngleEnd, spvAngleStart);
  TdxGaugeArcQuantitativeScaleParameterValues = set of TdxGaugeArcQuantitativeScaleParameterValue;
  TdxGaugeCircularScaleLabelOrientation = (loLeftToRight, loCircular, loCircularInward, loCircularOutward, loRadial);

  { TdxGaugeArcQuantitativeScaleDefaultParameters }

  TdxGaugeArcQuantitativeScaleDefaultParameters = class(TdxGaugeQuantitativeScaleDefaultParameters)
  public
    AngleStart: Integer;
    AngleEnd: Integer;
    AxisLineColor: TdxAlphaColor;
    AxisLineWidth: Single;
    CenterPointPositionFactor: TdxPointF;
    ScaleArcRadiusFactor: Single;
    ValueIndicatorScaleFactor: TdxPointF;
    SpindleCapUnderNeedle: Boolean;

    constructor Create; override;
  end;

  { TdxGaugeCircularWideScaleDefaultParameters }

  TdxGaugeCircularWideScaleDefaultParameters = class(TdxGaugeArcQuantitativeScaleDefaultParameters)
  public
    ValueIndicatorClipAreaHeightFactor: Single;
  end;

  { TdxGaugeArcQuantitativeScaleParameters }

  TdxGaugeArcQuantitativeScaleParameters = class(TdxGaugeQuantitativeScaleParameters)
  public
    AngleStart: Integer;
    AngleEnd: Integer;
    LabelOrientation: TdxGaugeCircularScaleLabelOrientation;
    Radius: Single;
    ShowSpindleCap: Boolean;
  end;

  { TdxGaugeArcScaleInfo }

  TdxGaugeArcScaleInfo = class(TdxGaugeQuantitativeScaleInfo)
  public
    AngleStart: Single;
    AngleRange: Single;
    ClipArea: TdxRectF;
    MinValue: Single;
    NeedRotateBounds: Boolean;
    RotatePoint: TdxPointF;

    constructor Create; override;
  end;

  { TdxGaugeCircularRangeParameters }

  TdxGaugeCircularRangeParameters = class(TdxGaugeCustomRangeParameters)
  public
    RadiusFactor: Single;
  end;

  { TdxGaugeScaleOptionsArcLayout }

  TdxGaugeScaleOptionsArcLayout = class(TdxGaugeScaleOptionsCustomLayout)
  private
    function GetRadius: Integer;
    function GetRadiusFactor: Single;
    procedure SetRadius(const AValue: Integer);
    procedure SetRadiusFactor(const AValue: Single);

    function GetScale: TdxGaugeCustomArcQuantitativeScale;
  published
    property Height;
    property HeightFactor;
    property Width;
    property WidthFactor;

    property Radius: Integer read GetRadius write SetRadius stored False;
    property RadiusFactor: Single read GetRadiusFactor write SetRadiusFactor stored False;
  end;

  { TdxGaugeArcQuantitativeScaleOptionsView }

  TdxGaugeArcQuantitativeScaleOptionsView = class(TdxGaugeQuantitativeScaleOptionsView)
  private
    function GetAngleEnd: Integer;
    function GetAngleStart: Integer;
    function GetLabelOrientation: TdxGaugeCircularScaleLabelOrientation;
    function GetShowSpindleCap: Boolean;
    procedure SetAngleEnd(const AValue: Integer);
    procedure SetAngleStart(const AValue: Integer);
    procedure SetLabelOrientation(const AValue: TdxGaugeCircularScaleLabelOrientation);
    procedure SetShowSpindleCap(const AValue: Boolean);

    function GetScale: TdxGaugeCustomArcQuantitativeScale;
    function IsAngleEndStored: Boolean;
    function IsAngleStartStored: Boolean;
  published
    property LogarithmicBase;

    property AngleEnd: Integer read GetAngleEnd write SetAngleEnd stored IsAngleEndStored;
    property AngleStart: Integer read GetAngleStart write SetAngleStart stored IsAngleStartStored;
    property LabelOrientation: TdxGaugeCircularScaleLabelOrientation read GetLabelOrientation write SetLabelOrientation
      default loLeftToRight;
    property ShowNeedle: Boolean read GetShowValueIndicator write SetShowValueIndicator default True;
    property ShowSpindleCap: Boolean read GetShowSpindleCap write SetShowSpindleCap default True;
  end;

  { TdxGaugeArcQuantitativeScaleViewInfo }

  TdxGaugeArcQuantitativeScaleViewInfo = class(TdxGaugeQuantitativeScaleViewInfo)
  private
    FAngleRange: Single;
    FNeedle: TdxCompositeShape;
    FRotatePoint: TdxPointF;
    FSpindleCap: TdxCompositeShape;
    FSpindleCapBounds: TdxRectF;

    function GetDefaultParameters: TdxGaugeArcQuantitativeScaleDefaultParameters;
    function GetLabelTextRectQuarter(AAngle: Single): Integer;
    function GetParameters: TdxGaugeArcQuantitativeScaleParameters;
    function GetScaleRadius: Single;
    procedure CalculateAngleRange;
    procedure CalculateScaleRadius;
    procedure CalculateSpindleCap;
    procedure DrawAxisLine(AGPGraphics: TdxGPGraphics);
    procedure DrawSpindleCap(AGPGraphics: TdxGPGraphics);
  protected
    function GetScaleInfoClass: TdxGaugeCustomScaleInfoClass; override;
    function GetParametersClass: TdxGaugeCustomScaleParametersClass; override;

    function GetScaleEndPoint: TdxPointF; override;
    function GetScaleStartPoint: TdxPointF; override;

    function CanDrawValueIndicator: Boolean; override;
    function GetLabelTextRect(AAngle: Single; const AText: string): TdxRectF; override;
    procedure CalculateContent; override;
    procedure CalculateReferenceParameters; override;
    procedure CalculateScaleInfo; override;
    procedure DoDrawBackground(AGPGraphics: TdxGPGraphics); override;
    procedure DoDrawLayer(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer); override;
    procedure DoDrawTickmarkLabel(AGPGraphics: TdxGPGraphics; AAngle: Single;
      const ADrawInfo: TdxGaugeQuantitativeScaleTickmarkLabelDrawInfo); override;
    procedure DoDrawTickmarkTick(AGPGraphics: TdxGPGraphics; AAngle: Single;
      const ADrawInfo: TdxGaugeQuantitativeScaleTickmarkTickDrawInfo); override;
    procedure DoDrawValueIndicator(AGPGraphics: TdxGPGraphics); override;
    procedure LoadScaleElements; override;
    procedure PopulateParameters(AParameters: TdxGaugeCustomScaleParameters); override;

    function GetClipArea: TdxRectF; virtual;
    function GetReferencePoint: TdxPointF; virtual;
    function GetValidAngle(AAngle: Single): Integer; virtual;
    procedure CalculateRotatePoint; virtual;
    procedure DoDrawSpindleCap(AGPGraphics: TdxGPGraphics); virtual;

    property AngleRange: Single read FAngleRange;
    property RotatePoint: TdxPointF read FRotatePoint;
  public
    constructor Create(AScaleType: TdxGaugeScaleType; const AStyleName: string); override;
  end;

  { TdxGaugeCustomArcQuantitativeScale }

  TdxGaugeCustomArcQuantitativeScale = class(TdxGaugeQuantitativeScale)
  private
    FAssignedValues: TdxGaugeArcQuantitativeScaleParameterValues;

    function GetAngleEnd: Integer;
    function GetAngleStart: Integer;
    function GetLabelOrientation: TdxGaugeCircularScaleLabelOrientation;
    function GetOptionsView: TdxGaugeArcQuantitativeScaleOptionsView;
    function GetRadius: Integer;
    function GetRadiusFactor: Single;
    function GetShowSpindleCap: Boolean;
    procedure SetAngleEnd(const AValue: Integer);
    procedure SetAngleStart(const AValue: Integer);
    procedure SetLabelOrientation(const AValue: TdxGaugeCircularScaleLabelOrientation);
    procedure SetOptionsView(const AValue: TdxGaugeArcQuantitativeScaleOptionsView);
    procedure SetRadius(const AValue: Integer);
    procedure SetRadiusFactor(const AValue: Single);
    procedure SetShowSpindleCap(const AValue: Boolean);

    function GetScaleParameters: TdxGaugeArcQuantitativeScaleParameters;
    function GetViewInfo: TdxGaugeArcQuantitativeScaleViewInfo;

    function IsAngleEndStored: Boolean;
    function IsAngleStartStored: Boolean;
  protected
    class function GetLayerCount: Integer; override;
    class function GetScaleName: string; override;
    class function GetStyleReaderClass: TdxGaugeCustomScaleStyleReaderClass; override;

    function GetOptionsViewClass: TdxGaugeCustomScaleOptionsViewClass; override;
    function GetParametersClass: TdxGaugeCustomScaleParametersClass; override;
    function GetRangeClass: TdxGaugeCustomRangeClass; override;
    function GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass; override;

    procedure ApplyStyleParameters; override;
    procedure DoAssign(AScale: TdxGaugeCustomScale); override;
    procedure InitParameters; override;
    procedure InternalRestoreStyleParameters; override;

    procedure SetAngle(AAngle: Integer; var AScaleAngle: Integer); virtual;

    property OptionsView: TdxGaugeArcQuantitativeScaleOptionsView read GetOptionsView write SetOptionsView;
  end;

  { TdxGaugeArcQuantitativeScale }

  TdxGaugeArcQuantitativeScale = class(TdxGaugeCustomArcQuantitativeScale)
  private
    function GetOptionsLayout: TdxGaugeScaleOptionsArcLayout;
    procedure SetOptionsLayout(const AValue: TdxGaugeScaleOptionsArcLayout);
  protected
    function GetOptionsLayoutClass: TdxGaugeScaleOptionsCustomLayoutClass; override;

    property OptionsLayout: TdxGaugeScaleOptionsArcLayout read GetOptionsLayout write SetOptionsLayout;
  end;

  { TdxGaugeCustomCircularScaleViewInfo }

  TdxGaugeCustomCircularScaleViewInfo = class(TdxGaugeArcQuantitativeScaleViewInfo)
  protected
    function GetValidAngle(AAngle: Single): Integer; override;
  end;

  { TdxGaugeCustomCircularScale }

  TdxGaugeCustomCircularScale = class(TdxGaugeArcQuantitativeScale)
  protected
    class function GetScaleType: TdxGaugeScaleType; override;
    function GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass; override;

    procedure SetAngle(AAngle: Integer; var AScaleAngle: Integer); override;
  end;

  { TdxGaugeCircularScale }

  TdxGaugeCircularScale = class(TdxGaugeCustomCircularScale)
  published
    property AnchorScaleIndex;
    property Captions;
    property OptionsAnimate;
    property OptionsLayout;
    property OptionsView;
    property Ranges;
    property StyleName;
    property Value;
    property Visible;

    property OnAnimate;
    property OnAnimationComplete;
  end;

  { TdxGaugeCustomCircularHalfScale }

  TdxGaugeCustomCircularHalfScale = class(TdxGaugeArcQuantitativeScale)
  protected
    class function GetScaleName: string; override;
    class function GetScaleType: TdxGaugeScaleType; override;
    class function GetStyleReaderClass: TdxGaugeCustomScaleStyleReaderClass; override;
  end;

  { TdxGaugeCircularHalfScale }

  TdxGaugeCircularHalfScale = class(TdxGaugeCustomCircularHalfScale)
  published
    property AnchorScaleIndex;
    property Captions;
    property OptionsAnimate;
    property OptionsLayout;
    property OptionsView;
    property Ranges;
    property StyleName;
    property Value;
    property Visible;

    property OnAnimate;
    property OnAnimationComplete;
  end;

  { TdxGaugeCustomCircularQuarterLeftScale }

  TdxGaugeCustomCircularQuarterLeftScale = class(TdxGaugeCustomCircularHalfScale)
  protected
    class function GetScaleName: string; override;
    class function GetScaleType: TdxGaugeScaleType; override;
  end;

  { TdxGaugeCircularQuarterLeftScale }

  TdxGaugeCircularQuarterLeftScale = class(TdxGaugeCustomCircularQuarterLeftScale)
  published
    property AnchorScaleIndex;
    property Captions;
    property OptionsAnimate;
    property OptionsLayout;
    property OptionsView;
    property Ranges;
    property StyleName;
    property Value;
    property Visible;

    property OnAnimate;
    property OnAnimationComplete;
  end;

  { TdxGaugeCustomCircularQuarterRightScaleViewInfo }

  TdxGaugeCustomCircularQuarterRightScaleViewInfo = class(TdxGaugeArcQuantitativeScaleViewInfo)
  protected
    function GetReferencePoint: TdxPointF; override;
  end;

  { TdxGaugeCustomCircularQuarterRightScale }

  TdxGaugeCustomCircularQuarterRightScale = class(TdxGaugeCustomCircularHalfScale)
  protected
    class function GetScaleName: string; override;
    class function GetScaleType: TdxGaugeScaleType; override;

    function GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass; override;
  end;

  { TdxGaugeCircularQuarterRightScale }

  TdxGaugeCircularQuarterRightScale = class(TdxGaugeCustomCircularQuarterRightScale)
  published
    property AnchorScaleIndex;
    property Captions;
    property OptionsAnimate;
    property OptionsLayout;
    property OptionsView;
    property Ranges;
    property StyleName;
    property Value;
    property Visible;

    property OnAnimate;
    property OnAnimationComplete;
  end;

  { TdxGaugeCustomCircularThreeFourthScaleViewInfo }

  TdxGaugeCustomCircularThreeFourthScaleViewInfo = class(TdxGaugeArcQuantitativeScaleViewInfo)
  protected
    function GetLayoutSize(const ABounds: TdxRectF): TdxSizeF; override;
    procedure CalculateScaleInfo; override;
  end;

  { TdxGaugeCustomCircularThreeFourthScale }

  TdxGaugeCustomCircularThreeFourthScale = class(TdxGaugeCustomArcQuantitativeScale)
  private
    function GetOptionsLayout: TdxGaugeScaleOptionsArcLayout;
    procedure SetOptionsLayout(const AValue: TdxGaugeScaleOptionsArcLayout);
  protected
    class function GetScaleName: string; override;
    class function GetScaleType: TdxGaugeScaleType; override;

    function GetOptionsLayoutClass: TdxGaugeScaleOptionsCustomLayoutClass; override;
    function GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass; override;

    property OptionsLayout: TdxGaugeScaleOptionsArcLayout read GetOptionsLayout write SetOptionsLayout;
  end;

  { TdxGaugeCircularThreeFourthScale }

  TdxGaugeCircularThreeFourthScale = class(TdxGaugeCustomCircularThreeFourthScale)
  published
    property AnchorScaleIndex;
    property Captions;
    property OptionsAnimate;
    property OptionsLayout;
    property OptionsView;
    property Ranges;
    property StyleName;
    property Value;
    property Visible;

    property OnAnimate;
    property OnAnimationComplete;
  end;

  { TdxGaugeCustomCircularWideScaleViewInfo }

  TdxGaugeCustomCircularWideScaleViewInfo = class(TdxGaugeArcQuantitativeScaleViewInfo)
  private
    function GetClipRect: TdxRectF;
    function GetDefaultParameters: TdxGaugeCircularWideScaleDefaultParameters;
  protected
    function GetClipArea: TdxRectF; override;
    function GetLayoutSize(const ABounds: TdxRectF): TdxSizeF; override;
    procedure CalculateScaleInfo; override;
    procedure DoDrawSpindleCap(AGPGraphics: TdxGPGraphics); override;
    procedure DoDrawValueIndicator(AGPGraphics: TdxGPGraphics); override;
  end;

  { TdxGaugeCustomCircularWideScale }

  TdxGaugeCustomCircularWideScale = class(TdxGaugeCustomArcQuantitativeScale)
  private
    function GetOptionsLayout: TdxGaugeScaleOptionsRectangularLayout;
    procedure SetOptionsLayout(const AValue: TdxGaugeScaleOptionsRectangularLayout);
  protected
    class function GetScaleName: string; override;
    class function GetScaleType: TdxGaugeScaleType; override;
    class function GetStyleReaderClass: TdxGaugeCustomScaleStyleReaderClass; override;

    function GetOptionsLayoutClass: TdxGaugeScaleOptionsCustomLayoutClass; override;
    function GetRangeClass: TdxGaugeCustomRangeClass; override;
    function GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass; override;

    property OptionsLayout: TdxGaugeScaleOptionsRectangularLayout read GetOptionsLayout write SetOptionsLayout;
  end;

  { TdxGaugeCircularWideScale }

  TdxGaugeCircularWideScale = class(TdxGaugeCustomCircularWideScale)
  published
    property AnchorScaleIndex;
    property Captions;
    property OptionsAnimate;
    property OptionsLayout;
    property OptionsView;
    property Ranges;
    property StyleName;
    property Value;
    property Visible;

    property OnAnimate;
    property OnAnimationComplete;
  end;

  { TdxGaugeArcQuantitativeScaleStyleReader }

  TdxGaugeArcQuantitativeScaleStyleReader = class(TdxGaugeQuantitativeScaleStyleReader)
  private
    procedure ReadSpindleCapParameters(ANode: TdxXMLNode; AParameters: TdxGaugeArcQuantitativeScaleDefaultParameters);
  protected
    class function GetResourceNamePrefix: string; override;
    function GetDefaultParametersClass: TdxGaugeCustomScaleDefaultParametersClass; override;
    procedure ReadCommonScaleParameters(ANode: TdxXMLNode;
      AParameters: TdxGaugeQuantitativeScaleDefaultParameters); override;
    procedure ReadNeedleParameters(ANode: TdxXMLNode;
      AParameters: TdxGaugeArcQuantitativeScaleDefaultParameters); virtual;
  end;

  { TdxGaugeCustomCircularHalfScaleStyleReader }

  TdxGaugeCustomCircularHalfScaleStyleReader = class(TdxGaugeArcQuantitativeScaleStyleReader)
  protected
    class function GetResourceNamePrefix: string; override;
  end;

  { TdxGaugeCircularWideScaleStyleReader }

  TdxGaugeCircularWideScaleStyleReader = class(TdxGaugeArcQuantitativeScaleStyleReader)
  protected
    class function GetResourceNamePrefix: string; override;

    function GetDefaultParametersClass: TdxGaugeCustomScaleDefaultParametersClass; override;
    procedure ReadNeedleParameters(ANode: TdxXMLNode;
      AParameters: TdxGaugeArcQuantitativeScaleDefaultParameters); override;
  end;

  { TdxGaugeCustomCircularScaleRangeViewInfo }

  TdxGaugeCustomCircularScaleRangeViewInfo = class(TdxGaugeCustomRangeViewInfo)
  private
    FWidth: Single;

    function GetBounds: TdxRectF;
    function GetParameters: TdxGaugeCircularRangeParameters;
    function GetScaleInfo: TdxGaugeArcScaleInfo;
  protected
    function GetParametersClass: TdxGaugeCustomRangeParametersClass; override;
    procedure CalculateBounds; override;
    procedure CalculateSelectionBounds; override;
    procedure DoDraw(AGPGraphics: TdxGPGraphics); override;

    function GetRadius: Single; virtual;
    procedure CalculateSectorWidth; virtual;
  end;

  { TdxGaugeCustomCircularScaleRange }

  TdxGaugeCustomCircularScaleRange = class(TdxGaugeCustomRange)
  private
    function GetRadiusFactor: Single;
    procedure SetRadiusFactor(const AValue: Single);

    function GetParameters: TdxGaugeCircularRangeParameters;

    procedure ReadRadiusFactor(AReader: TReader);
    procedure WriteRadiusFactor(AWriter: TWriter);

    function IsRadiusFactorStored: Boolean;
  protected
    function GetParametersClass: TdxGaugeCustomRangeParametersClass; override;
    function GetViewInfoClass: TdxGaugeCustomRangeViewInfoClass; override;
    procedure DefineProperties(AFiler: TFiler); override;

    function GetViewInfo: TdxGaugeCustomCircularScaleRangeViewInfo;

    property RadiusFactor: Single read GetRadiusFactor write SetRadiusFactor stored IsRadiusFactorStored;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxGaugeCircularWideScaleRangeViewInfo }

  TdxGaugeCircularWideScaleRangeViewInfo = class(TdxGaugeCustomCircularScaleRangeViewInfo)
  protected
    function GetRadius: Single; override;
    procedure CalculateSectorWidth; override;
    procedure DoDraw(AGPGraphics: TdxGPGraphics); override;
  end;

  { TdxGaugeCircularScaleRange }

  TdxGaugeCircularScaleRange = class(TdxGaugeCustomCircularScaleRange)
  published
    property Color;
    property RadiusFactor;
    property LinkedWithScaleValue;
    property ValueEnd;
    property ValueStart;
    property Visible;
    property WidthFactor;
  end;

  { TdxGaugeCircularWideScaleRange }

  TdxGaugeCircularWideScaleRange = class(TdxGaugeCustomCircularScaleRange)
  protected
    function GetViewInfoClass: TdxGaugeCustomRangeViewInfoClass; override;
  published
    property Color;
    property RadiusFactor;
    property LinkedWithScaleValue;
    property ValueEnd;
    property ValueStart;
    property Visible;
    property WidthFactor;
  end;

implementation

uses
  Types, SysUtils, Math, dxGDIPlusAPI, dxGaugeUtils;

const
  dxGaugeCircularScaleDefaultRadiusFactor = 0.5;

type
  TdxGaugeCustomCircularScaleRangeAccess = class(TdxGaugeCustomCircularScaleRange);
  TdxGaugeCustomCircularScaleRangeViewInfoAccess = class(TdxGaugeCustomCircularScaleRangeViewInfo);
  TdxGaugeScaleStyleAccess = class(TdxGaugeScaleStyle);

function dxGaugeArcScaleValueToAngle(AValue, AMinValue, AValueRange, AAngleStart, AAngleRange: Single): Single;
begin
  Result := AAngleStart - (AValue - AMinValue) * AAngleRange / AValueRange;
end;

function dxGaugeGetSectorRect(const ARect: TdxRectF; ASectorWidth: Single; AStartAngle, ASweepAngle: Single): TdxRectF;

  procedure InitPath(APath: TdxGPPath);
  var
    R: TdxRectF;
  begin
    APath.FigureStart;
    R := cxRectContent(ARect, dxRectF(ASectorWidth, ASectorWidth, ASectorWidth, ASectorWidth));
    if (ASectorWidth < ARect.Width / 2) and (R.Width > 0) and (R.Height > 0) then
    begin
      APath.AddArc(R.Left, R.Top, R.Width, R.Height, -AStartAngle, -ASweepAngle);
      APath.AddArc(ARect.Left, ARect.Top, ARect.Width, ARect.Height, -AStartAngle - ASweepAngle, ASweepAngle);
    end
    else
      APath.AddPie(ARect.Left, ARect.Top, ARect.Width, ARect.Height, -AStartAngle, -ASweepAngle);
    APath.FigureFinish;
  end;

var
  APath: TdxGPPath;
begin
  if ASectorWidth > 0 then
  begin
    APath := TdxGPPath.Create(gpfmWinding);
    try
      InitPath(APath);
      Result := APath.GetBoundsF;
    finally
      APath.Free;
    end;
  end;
end;

{ TdxGaugeArcQuantitativeScaleDefaultParameters }

constructor TdxGaugeArcQuantitativeScaleDefaultParameters.Create;
begin
  inherited Create;
  CenterPointPositionFactor := dxPointF(cxNullPoint);
end;

{ TdxGaugeArcScaleInfo }

constructor TdxGaugeArcScaleInfo.Create;
begin
  inherited Create;
  RotatePoint := dxPointF(cxNullPoint);
end;

{ TdxGaugeScaleOptionsArcLayout }

function TdxGaugeScaleOptionsArcLayout.GetRadius: Integer;
begin
  Result := GetScale.GetRadius;
end;

function TdxGaugeScaleOptionsArcLayout.GetRadiusFactor: Single;
begin
  Result := GetScale.GetRadiusFactor;
end;

procedure TdxGaugeScaleOptionsArcLayout.SetRadius(const AValue: Integer);
begin
  GetScale.SetRadius(AValue);
end;

procedure TdxGaugeScaleOptionsArcLayout.SetRadiusFactor(const AValue: Single);
begin
  GetScale.SetRadiusFactor(AValue);
end;

function TdxGaugeScaleOptionsArcLayout.GetScale: TdxGaugeCustomArcQuantitativeScale;
begin
  Result := inherited GetScale as TdxGaugeCustomArcQuantitativeScale;
end;

{ TdxGaugeArcQuantitativeScaleOptionsView }

function TdxGaugeArcQuantitativeScaleOptionsView.GetAngleEnd: Integer;
begin
  Result := GetScale.GetAngleEnd;
end;

function TdxGaugeArcQuantitativeScaleOptionsView.GetAngleStart: Integer;
begin
  Result := GetScale.GetAngleStart;
end;

function TdxGaugeArcQuantitativeScaleOptionsView.GetLabelOrientation: TdxGaugeCircularScaleLabelOrientation;
begin
  Result := GetScale.GetLabelOrientation;
end;

function TdxGaugeArcQuantitativeScaleOptionsView.GetShowSpindleCap: Boolean;
begin
  Result := GetScale.GetShowSpindleCap;
end;

procedure TdxGaugeArcQuantitativeScaleOptionsView.SetAngleEnd(const AValue: Integer);
begin
  GetScale.SetAngleEnd(AValue);
end;

procedure TdxGaugeArcQuantitativeScaleOptionsView.SetAngleStart(const AValue: Integer);
begin
  GetScale.SetAngleStart(AValue);
end;

procedure TdxGaugeArcQuantitativeScaleOptionsView.SetLabelOrientation(const AValue: TdxGaugeCircularScaleLabelOrientation);
begin
  GetScale.SetLabelOrientation(AValue);
end;

procedure TdxGaugeArcQuantitativeScaleOptionsView.SetShowSpindleCap(const AValue: Boolean);
begin
  GetScale.SetShowSpindleCap(AValue);
end;

function TdxGaugeArcQuantitativeScaleOptionsView.IsAngleEndStored: Boolean;
begin
  Result := GetScale.IsAngleEndStored;
end;

function TdxGaugeArcQuantitativeScaleOptionsView.IsAngleStartStored: Boolean;
begin
  Result := GetScale.IsAngleStartStored;
end;

function TdxGaugeArcQuantitativeScaleOptionsView.GetScale: TdxGaugeCustomArcQuantitativeScale;
begin
  Result := inherited GetScale as TdxGaugeCustomArcQuantitativeScale;
end;

{ TdxGaugeArcQuantitativeScaleViewInfo }

constructor TdxGaugeArcQuantitativeScaleViewInfo.Create(AScaleType: TdxGaugeScaleType; const AStyleName: string);
begin
  inherited Create(AScaleType, AStyleName);
  FRotatePoint := dxPointF(cxNullPoint);
end;

function TdxGaugeArcQuantitativeScaleViewInfo.GetScaleInfoClass: TdxGaugeCustomScaleInfoClass;
begin
  Result := TdxGaugeArcScaleInfo;
end;

function TdxGaugeArcQuantitativeScaleViewInfo.GetParametersClass: TdxGaugeCustomScaleParametersClass;
begin
  Result := TdxGaugeArcQuantitativeScaleParameters;
end;

function TdxGaugeArcQuantitativeScaleViewInfo.GetScaleEndPoint: TdxPointF;
begin
  Result := dxPointF(GetParameters.AngleEnd, GetParameters.AngleEnd);
end;

function TdxGaugeArcQuantitativeScaleViewInfo.GetScaleStartPoint: TdxPointF;
begin
  Result := dxPointF(GetParameters.AngleStart, GetParameters.AngleStart);
end;

function TdxGaugeArcQuantitativeScaleViewInfo.CanDrawValueIndicator: Boolean;
begin
  Result := CanDrawImage(FNeedle);
end;

function TdxGaugeArcQuantitativeScaleViewInfo.GetLabelTextRect(AAngle: Single; const AText: string): TdxRectF;

  function GetTextRectReferencePointOffset(AQuarter: Integer; const AQuarterSize: TdxSizeF): Single;
  begin
    case AQuarter of
      0, 2:
        begin
          Result := AQuarterSize.cx / Cos(DegToRad(AAngle));
          if Result * Sin(DegToRad(AAngle)) > AQuarterSize.cy then
            Result := AQuarterSize.cy / Cos(DegToRad(90 - AAngle));
        end;
      1, 3:
        begin
          Result := AQuarterSize.cy / Cos(DegToRad(AAngle));
          if Result * Sin(DegToRad(AAngle)) > AQuarterSize.cx then
            Result := AQuarterSize.cx / Cos(DegToRad(90 - AAngle));
        end
      else
        Result := 0;
    end
  end;

  function GetReferencePoint(ATextSize: TdxSizeF): TdxPointF;
  var
    ASign: Integer;
    AOffset: Single;
    ADefaultParameters: TdxGaugeArcQuantitativeScaleDefaultParameters;
    AParameters: TdxGaugeArcQuantitativeScaleParameters;
  begin
    AParameters := GetParameters;
    ADefaultParameters := GetDefaultParameters;
    case AParameters.LabelOrientation of
      loRadial:
        AOffset := ATextSize.cx / 2;
      loCircularInward, loCircularOutward, loCircular:
        AOffset := ATextSize.cy / 2;
      else
        AOffset := 0;
    end;
    ASign := Sign(ADefaultParameters.LabelOffset);
    if ASign = 0 then
      ASign := 1;
    Result := dxRingPoint(RotatePoint, AParameters.Radius + ADefaultParameters.LabelOffset + ASign * AOffset,
      DegToRad(AAngle));
  end;

var
  P: TdxPointF;
  ATextSize, AQuarterSize: TdxSizeF;
  ARadius: Single;
  ATextRectQuarter: Integer;
begin
  ATextSize := GetLabelTextRectSize(AText);
  P := GetReferencePoint(ATextSize);
  if GetParameters.LabelOrientation = loLeftToRight then
  begin
    AQuarterSize := dxSizeF(ATextSize.cx / 2, ATextSize.cy / 2);
    AAngle := AAngle - Floor(AAngle / 360) * 360;
    ATextRectQuarter := GetLabelTextRectQuarter(AAngle);
    AAngle := AAngle - 90 * ATextRectQuarter;
    ARadius := GetTextRectReferencePointOffset(ATextRectQuarter, AQuarterSize);
    P := dxRingPoint(P, ARadius, DegToRad(180 + 90 * ATextRectQuarter + AAngle));
    Result := cxRectInflate(cxRectF(P, P), AQuarterSize.cx, AQuarterSize.cy);
  end
  else
    Result := cxRectInflate(cxRectF(P, P), ATextSize.cx, ATextSize.cy);
end;

procedure TdxGaugeArcQuantitativeScaleViewInfo.CalculateContent;
begin
  inherited CalculateContent;
  CalculateAngleRange;
  CalculateSpindleCap;
end;

procedure TdxGaugeArcQuantitativeScaleViewInfo.CalculateReferenceParameters;
begin
  CalculateRotatePoint;
  CalculateScaleRadius;
end;

procedure TdxGaugeArcQuantitativeScaleViewInfo.CalculateScaleInfo;
var
  AScaleInfo: TdxGaugeArcScaleInfo;
begin
  inherited  CalculateScaleInfo;
  AScaleInfo := ScaleInfo as TdxGaugeArcScaleInfo;
  AScaleInfo.AngleStart := GetParameters.AngleStart;
  AScaleInfo.AngleRange := AngleRange;
  AScaleInfo.RotatePoint := RotatePoint;
  AScaleInfo.MinValue := GetParameters.MinValue;
  AScaleInfo.ClipArea := GetClipArea;
end;

procedure TdxGaugeArcQuantitativeScaleViewInfo.DoDrawBackground(AGPGraphics: TdxGPGraphics);
begin
  inherited DoDrawBackground(AGPGraphics);
  DrawAxisLine(AGPGraphics);
end;

procedure TdxGaugeArcQuantitativeScaleViewInfo.DoDrawLayer(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer);
begin
  if (ALayerIndex = 2) and GetDefaultParameters.SpindleCapUnderNeedle then
    DrawSpindleCap(AGPGraphics);
  inherited DoDrawLayer(AGPGraphics, ALayerIndex);
  if (ALayerIndex = 2) and not GetDefaultParameters.SpindleCapUnderNeedle then
    DrawSpindleCap(AGPGraphics);
end;

procedure TdxGaugeArcQuantitativeScaleViewInfo.DoDrawTickmarkLabel(AGPGraphics: TdxGPGraphics;
  AAngle: Single; const ADrawInfo: TdxGaugeQuantitativeScaleTickmarkLabelDrawInfo);
var
  ATempAngle, ARotationAngle: Single;
  AOrientation: TdxGaugeCircularScaleLabelOrientation;
begin
  AOrientation := GetParameters.LabelOrientation;
  if AOrientation = loLeftToRight then
    dxGaugeDrawText(AGPGraphics, ADrawInfo.Text, ADrawInfo.Rect, Font, ADrawInfo.Color)
  else
  begin
    ATempAngle := AAngle - Floor(AAngle / 360) * 360;
    ARotationAngle := 0;
    case AOrientation of
      loCircularInward:
         ARotationAngle := 270;
      loCircularOutward:
         ARotationAngle := 90;
      loCircular:
        if (ATempAngle >= 0) and (ATempAngle <= 180) then
          ARotationAngle := 90
        else
          ARotationAngle := 270;
      loRadial:
        if (ATempAngle >= 90) and (ATempAngle <= 270) then
          ARotationAngle := 180;
    end;
    dxGaugeDrawText(AGPGraphics, ADrawInfo.Text, ADrawInfo.Rect, Font, ADrawInfo.Color, ARotationAngle - AAngle);
  end;
end;

procedure TdxGaugeArcQuantitativeScaleViewInfo.DoDrawTickmarkTick(AGPGraphics: TdxGPGraphics;
  AAngle: Single; const ADrawInfo: TdxGaugeQuantitativeScaleTickmarkTickDrawInfo);
var
  AAngleOffset: Single;
begin
  if GetDefaultParameters.RotateTicks then
    AAngleOffset := 180
  else
    AAngleOffset := 0;
  dxGaugeRotateAndDrawImage(AGPGraphics, ADrawInfo.Image, ADrawInfo.Size, RotatePoint, GetParameters.Radius + ADrawInfo.Offset,
    AAngle, AAngleOffset);
end;

procedure TdxGaugeArcQuantitativeScaleViewInfo.DoDrawValueIndicator(AGPGraphics: TdxGPGraphics);

  function GetStartOffset: Single;
  begin
    Result := GetDefaultParameters.ValueIndicatorStartOffset;
  end;

  function GetImageSize(AImage: TGraphic): TdxSizeF;
  var
    ADefaultParameters: TdxGaugeArcQuantitativeScaleDefaultParameters;
  begin
    ADefaultParameters := GetDefaultParameters;
    Result.cx := (GetParameters.Radius - ADefaultParameters.ValueIndicatorEndOffset - GetStartOffset) *
      ADefaultParameters.ValueIndicatorScaleFactor.X;
    Result.cy := (AImage.Height * Result.cx / AImage.Width) * ADefaultParameters.ValueIndicatorScaleFactor.Y;
  end;

begin
  dxGaugeRotateAndDrawImage(AGPGraphics, FNeedle, GetImageSize(FNeedle), RotatePoint, GetStartOffset, ScaleValuePosition);
end;

procedure TdxGaugeArcQuantitativeScaleViewInfo.LoadScaleElements;
begin
  inherited LoadScaleElements;
  FNeedle := TdxGaugeScaleStyleAccess(Style).GetElement(etNeedle);
  FSpindleCap := TdxGaugeScaleStyleAccess(Style).GetElement(etSpindleCap);
end;

procedure TdxGaugeArcQuantitativeScaleViewInfo.PopulateParameters(AParameters: TdxGaugeCustomScaleParameters);
var
  AScaleParameters: TdxGaugeArcQuantitativeScaleParameters;
begin
  inherited PopulateParameters(AParameters);
  AScaleParameters := GetParameters;
  AScaleParameters.AngleStart := GetValidAngle(AScaleParameters.AngleStart);
  AScaleParameters.AngleEnd := GetValidAngle(AScaleParameters.AngleEnd);
end;

function TdxGaugeArcQuantitativeScaleViewInfo.GetClipArea: TdxRectF;
begin
  Result := GetContentBounds;
end;

function TdxGaugeArcQuantitativeScaleViewInfo.GetReferencePoint: TdxPointF;
begin
  Result := GetContentBounds.TopLeft;
end;

function TdxGaugeArcQuantitativeScaleViewInfo.GetValidAngle(AAngle: Single): Integer;
var
  AScaleDefaultParameters: TdxGaugeArcQuantitativeScaleDefaultParameters;
begin
  AScaleDefaultParameters := GetDefaultParameters;
  Result := Round(Max(Min(AScaleDefaultParameters.AngleStart, AAngle), AScaleDefaultParameters.AngleEnd));
end;

procedure TdxGaugeArcQuantitativeScaleViewInfo.CalculateRotatePoint;
var
  AContentBounds: TdxRectF;
  ADefaultParameters: TdxGaugeArcQuantitativeScaleDefaultParameters;
begin
  AContentBounds := GetContentBounds;
  ADefaultParameters := GetDefaultParameters;
  FRotatePoint.X := AContentBounds.Left + AContentBounds.Width * ADefaultParameters.CenterPointPositionFactor.X;
  FRotatePoint.Y := AContentBounds.Top + AContentBounds.Height * ADefaultParameters.CenterPointPositionFactor.Y;
end;

procedure TdxGaugeArcQuantitativeScaleViewInfo.DoDrawSpindleCap(AGPGraphics: TdxGPGraphics);
begin
  dxGaugeDrawImage(AGPGraphics, FSpindleCap, FSpindleCapBounds);
end;

function TdxGaugeArcQuantitativeScaleViewInfo.GetDefaultParameters: TdxGaugeArcQuantitativeScaleDefaultParameters;
begin
  Result := DefaultParameters as TdxGaugeArcQuantitativeScaleDefaultParameters;
end;

function TdxGaugeArcQuantitativeScaleViewInfo.GetLabelTextRectQuarter(AAngle: Single): Integer;
var
  AQuarterOffset: Integer;
begin
  AQuarterOffset := Integer(GetDefaultParameters.LabelOffset >= 0) * 2;
  Result := AQuarterOffset;
  if (180 > AAngle) and (AAngle >= 90) or (-180 >= AAngle) and (AAngle > -270) then
    Result := 1 + AQuarterOffset;
    if (270 > AAngle) and (AAngle >= 180) or (-90 > AAngle) and (AAngle >= -180) then
      Result := 2 - AQuarterOffset;
    if (360 > AAngle) and (AAngle >= 270) or (0 > AAngle) and (AAngle >= -90) then
      Result := 3 - AQuarterOffset;
end;

function TdxGaugeArcQuantitativeScaleViewInfo.GetParameters: TdxGaugeArcQuantitativeScaleParameters;
begin
  Result := FParameters as TdxGaugeArcQuantitativeScaleParameters;
end;

function TdxGaugeArcQuantitativeScaleViewInfo.GetScaleRadius: Single;
var
  ADefaultParameters: TdxGaugeArcQuantitativeScaleDefaultParameters;
begin
  ADefaultParameters := GetDefaultParameters;
  Result := Min(125 * ADefaultParameters.ScaleArcRadiusFactor, 125 * ADefaultParameters.ScaleArcRadiusFactor);
end;

procedure TdxGaugeArcQuantitativeScaleViewInfo.CalculateAngleRange;
var
  AScaleParameters: TdxGaugeArcQuantitativeScaleParameters;
begin
  AScaleParameters := GetParameters;
  FAngleRange := AScaleParameters.AngleStart - AScaleParameters.AngleEnd;
end;

procedure TdxGaugeArcQuantitativeScaleViewInfo.CalculateScaleRadius;
begin
  GetParameters.Radius := GetScaleRadius;
end;

procedure TdxGaugeArcQuantitativeScaleViewInfo.CalculateSpindleCap;
var
  ASize: Single;
begin
  ASize := GetDefaultParameters.SpindleCapSize / 2;
  FSpindleCapBounds := cxRectInflate(cxRectF(RotatePoint, RotatePoint), ASize, ASize);
end;

procedure TdxGaugeArcQuantitativeScaleViewInfo.DrawAxisLine(AGPGraphics: TdxGPGraphics);
var
  AAngleStart: Single;
  ABounds: TdxRectF;
  ADefaultParameters: TdxGaugeArcQuantitativeScaleDefaultParameters;
begin
  if GetDefaultParameters.AxisLineWidth > 0 then
  begin
    AAngleStart := GetParameters.AngleEnd - MajorTicksDrawInfo.Size.cx / 20;
    ABounds := cxRectInflate(cxRectF(RotatePoint, RotatePoint), GetParameters.Radius);
    ADefaultParameters := GetDefaultParameters;
    dxGaugeDrawSector(AGPGraphics, ABounds, ADefaultParameters.AxisLineWidth, AAngleStart,
      AngleRange + MajorTicksDrawInfo.Size.cx / 10, ADefaultParameters.AxisLineColor, ADefaultParameters.AxisLineColor);
  end;
end;

procedure TdxGaugeArcQuantitativeScaleViewInfo.DrawSpindleCap(AGPGraphics: TdxGPGraphics);
begin
  if GetParameters.ShowSpindleCap and CanDrawImage(FSpindleCap) then
    DoDrawSpindleCap(AGPGraphics);
end;

{ TdxGaugeCustomArcQuantitativeScale }

class function TdxGaugeCustomArcQuantitativeScale.GetLayerCount: Integer;
begin
  Result := 4;
end;

class function TdxGaugeCustomArcQuantitativeScale.GetScaleName: string;
begin
  Result := 'Circular';
end;

class function TdxGaugeCustomArcQuantitativeScale.GetStyleReaderClass: TdxGaugeCustomScaleStyleReaderClass;
begin
  Result := TdxGaugeArcQuantitativeScaleStyleReader;
end;

function TdxGaugeCustomArcQuantitativeScale.GetOptionsViewClass: TdxGaugeCustomScaleOptionsViewClass;
begin
  Result := TdxGaugeArcQuantitativeScaleOptionsView;
end;

function TdxGaugeCustomArcQuantitativeScale.GetParametersClass: TdxGaugeCustomScaleParametersClass;
begin
  Result := TdxGaugeArcQuantitativeScaleParameters;
end;

function TdxGaugeCustomArcQuantitativeScale.GetRangeClass: TdxGaugeCustomRangeClass;
begin
  Result := TdxGaugeCircularScaleRange;
end;

function TdxGaugeCustomArcQuantitativeScale.GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass;
begin
  Result := TdxGaugeArcQuantitativeScaleViewInfo;
end;

procedure TdxGaugeCustomArcQuantitativeScale.ApplyStyleParameters;
var
  AParameters: TdxGaugeArcQuantitativeScaleParameters;
  ADefaultParameters: TdxGaugeArcQuantitativeScaleDefaultParameters;
begin
  inherited ApplyStyleParameters;
  AParameters := GetScaleParameters;
  ADefaultParameters := GetViewInfo.GetDefaultParameters;
  if not(spvAngleEnd in FAssignedValues) then
    AParameters.AngleEnd := ADefaultParameters.AngleEnd;
  if not(spvAngleStart in FAssignedValues) then
    AParameters.AngleStart := ADefaultParameters.AngleStart;
end;

procedure TdxGaugeCustomArcQuantitativeScale.DoAssign(AScale: TdxGaugeCustomScale);
begin
  inherited DoAssign(AScale);
  FAssignedValues := (AScale as TdxGaugeCustomArcQuantitativeScale).FAssignedValues;
end;

procedure TdxGaugeCustomArcQuantitativeScale.InitParameters;
begin
  inherited InitParameters;
  GetScaleParameters.ShowSpindleCap := True;
end;

procedure TdxGaugeCustomArcQuantitativeScale.InternalRestoreStyleParameters;
begin
  FAssignedValues := [];
  GetScaleParameters.ShowSpindleCap := True;
  inherited InternalRestoreStyleParameters;
end;

procedure TdxGaugeCustomArcQuantitativeScale.SetAngle(AAngle: Integer; var AScaleAngle: Integer);
begin
  AScaleAngle := AAngle;
end;

function TdxGaugeCustomArcQuantitativeScale.GetAngleEnd: Integer;
begin
  Result := GetScaleParameters.AngleEnd;
end;

function TdxGaugeCustomArcQuantitativeScale.GetAngleStart: Integer;
begin
  Result := GetScaleParameters.AngleStart;
end;

function TdxGaugeCustomArcQuantitativeScale.GetLabelOrientation: TdxGaugeCircularScaleLabelOrientation;
begin
  Result := GetScaleParameters.LabelOrientation;
end;

function TdxGaugeCustomArcQuantitativeScale.GetOptionsView: TdxGaugeArcQuantitativeScaleOptionsView;
begin
  Result := inherited OptionsView as TdxGaugeArcQuantitativeScaleOptionsView;
end;

function TdxGaugeCustomArcQuantitativeScale.GetRadius: Integer;
begin
  if GetScaleParameters.Width = 0 then
    Result := GetScaleParameters.Height div 2
  else
    if GetScaleParameters.Height = 0 then
      Result := GetScaleParameters.Width div 2
    else
      Result := Min(GetScaleParameters.Width div 2, GetScaleParameters.Height div 2);
end;

function TdxGaugeCustomArcQuantitativeScale.GetRadiusFactor: Single;
begin
  Result := Min(GetScaleParameters.WidthFactor / 2, GetScaleParameters.HeightFactor / 2);
end;

function TdxGaugeCustomArcQuantitativeScale.GetShowSpindleCap: Boolean;
begin
  Result := GetScaleParameters.ShowSpindleCap;
end;

procedure TdxGaugeCustomArcQuantitativeScale.SetAngleEnd(const AValue: Integer);
begin
  if GetScaleParameters.AngleEnd <> AValue then
  begin
    SetAngle(AValue, GetScaleParameters.AngleEnd);
    if GetScaleParameters.AngleEnd = GetViewInfo.GetDefaultParameters.AngleEnd then
      Exclude(FAssignedValues, spvAngleEnd)
    else
      Include(FAssignedValues, spvAngleEnd);
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomArcQuantitativeScale.SetAngleStart(const AValue: Integer);
begin
  if GetScaleParameters.AngleStart <> AValue then
  begin
    SetAngle(AValue, GetScaleParameters.AngleStart);
    if GetScaleParameters.AngleStart = GetViewInfo.GetDefaultParameters.AngleStart then
      Exclude(FAssignedValues, spvAngleStart)
    else
      Include(FAssignedValues, spvAngleStart);
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomArcQuantitativeScale.SetLabelOrientation(const AValue: TdxGaugeCircularScaleLabelOrientation);
begin
  if GetScaleParameters.LabelOrientation <> AValue then
  begin
    GetScaleParameters.LabelOrientation := AValue;
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomArcQuantitativeScale.SetOptionsView(const AValue: TdxGaugeArcQuantitativeScaleOptionsView);
begin
  inherited OptionsView := AValue;
end;

procedure TdxGaugeCustomArcQuantitativeScale.SetRadius(const AValue: Integer);
begin
  if (GetScaleParameters.Radius <> AValue) and (AValue >= 0) then
  begin
    GetScaleParameters.Width := AValue * 2;
    GetScaleParameters.Height := AValue * 2;
    GetScaleParameters.Radius := AValue;
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomArcQuantitativeScale.SetRadiusFactor(const AValue: Single);
begin
  if not SameValue(GetScaleParameters.WidthFactor, AValue) and (AValue > 0) then
  begin
    GetScaleParameters.WidthFactor := AValue * 2;
    GetScaleParameters.HeightFactor := AValue * 2;
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomArcQuantitativeScale.SetShowSpindleCap(const AValue: Boolean);
begin
  if GetScaleParameters.ShowSpindleCap <> AValue then
  begin
    GetScaleParameters.ShowSpindleCap := AValue;
    ScaleChanged([sclDynamicLayer]);
  end;
end;

function TdxGaugeCustomArcQuantitativeScale.GetScaleParameters: TdxGaugeArcQuantitativeScaleParameters;
begin
  Result := Parameters as TdxGaugeArcQuantitativeScaleParameters;
end;

function TdxGaugeCustomArcQuantitativeScale.GetViewInfo: TdxGaugeArcQuantitativeScaleViewInfo;
begin
  Result := inherited ViewInfo as TdxGaugeArcQuantitativeScaleViewInfo;
end;

function TdxGaugeCustomArcQuantitativeScale.IsAngleEndStored: Boolean;
begin
  Result := spvAngleEnd in FAssignedValues;
end;

function TdxGaugeCustomArcQuantitativeScale.IsAngleStartStored: Boolean;
begin
  Result := spvAngleStart in FAssignedValues;
end;

{ TdxGaugeArcQuantitativeScale }

function TdxGaugeArcQuantitativeScale.GetOptionsLayoutClass: TdxGaugeScaleOptionsCustomLayoutClass;
begin
  Result := TdxGaugeScaleOptionsArcLayout;
end;

function TdxGaugeArcQuantitativeScale.GetOptionsLayout: TdxGaugeScaleOptionsArcLayout;
begin
  Result := inherited OptionsLayout as TdxGaugeScaleOptionsArcLayout;
end;

procedure TdxGaugeArcQuantitativeScale.SetOptionsLayout(const AValue: TdxGaugeScaleOptionsArcLayout);
begin
  inherited OptionsLayout := AValue;
end;

{ TdxGaugeCustomCircularScaleViewInfo }

function TdxGaugeCustomCircularScaleViewInfo.GetValidAngle(AAngle: Single): Integer;
begin
  Result := Round(AAngle);
end;

{ TdxGaugeCustomCircularScale }

class function TdxGaugeCustomCircularScale.GetScaleType: TdxGaugeScaleType;
begin
  Result := stCircularScale;
end;

function TdxGaugeCustomCircularScale.GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass;
begin
  Result := TdxGaugeCustomCircularScaleViewInfo;
end;

procedure TdxGaugeCustomCircularScale.SetAngle(AAngle: Integer; var AScaleAngle: Integer);
begin
  inherited SetAngle(AAngle, AScaleAngle);
  if not IsReading and (GetScaleParameters.AngleStart = GetScaleParameters.AngleEnd) then
    Dec(AScaleAngle, 360);
end;

{ TdxGaugeCustomCircularHalfScale }

class function TdxGaugeCustomCircularHalfScale.GetScaleName: string;
begin
  Result := inherited GetScaleName + ' Half';
end;

class function TdxGaugeCustomCircularHalfScale.GetScaleType: TdxGaugeScaleType;
begin
  Result := stCircularHalfScale;
end;

class function TdxGaugeCustomCircularHalfScale.GetStyleReaderClass: TdxGaugeCustomScaleStyleReaderClass;
begin
  Result := TdxGaugeCustomCircularHalfScaleStyleReader;
end;

{ TdxGaugeCustomCircularQuarterLeftScale }

class function TdxGaugeCustomCircularQuarterLeftScale.GetScaleName: string;
begin
  Result := 'Circular Quarter Left';
end;

class function TdxGaugeCustomCircularQuarterLeftScale.GetScaleType: TdxGaugeScaleType;
begin
  Result := stCircularQuarterLeftScale;
end;

{ TdxGaugeCustomCircularQuarterRightScaleViewInfo }

function TdxGaugeCustomCircularQuarterRightScaleViewInfo.GetReferencePoint: TdxPointF;
begin
  Result := dxPointF(GetContentBounds.Right, GetContentBounds.Top);
end;

{ TdxGaugeCustomCircularQuarterRightScale }

class function TdxGaugeCustomCircularQuarterRightScale.GetScaleName: string;
begin
  Result := 'Circular Quarter Right';
end;

class function TdxGaugeCustomCircularQuarterRightScale.GetScaleType: TdxGaugeScaleType;
begin
  Result := stCircularQuarterRightScale;
end;

function TdxGaugeCustomCircularQuarterRightScale.GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass;
begin
  Result := TdxGaugeCustomCircularQuarterRightScaleViewInfo;
end;

{ TdxGaugeCustomCircularThreeFourthScaleViewInfo }

procedure TdxGaugeCustomCircularThreeFourthScaleViewInfo.CalculateScaleInfo;
var
  ASize: TdxSizeF;
begin
  inherited CalculateScaleInfo;
  ASize := GetContentOriginalSize;
  ScaleInfo.Selection := cxRect(GetScaledRect(ASize,
    dxPointF(ScaleInfo.Bounds.Width / ASize.cx, ScaleInfo.Bounds.Height / ASize.cy)), False);
end;

function TdxGaugeCustomCircularThreeFourthScaleViewInfo.GetLayoutSize(const ABounds: TdxRectF): TdxSizeF;
begin
  Result := dxGaugeGetRectangularScaleLayoutSize(ABounds, FParameters.Width, FParameters.Height, FParameters.WidthFactor,
    FParameters.HeightFactor);
end;

{ TdxGaugeCustomCircularThreeFourthScale }

class function TdxGaugeCustomCircularThreeFourthScale.GetScaleName: string;
begin
  Result := inherited GetScaleName + ' Three-Fourth';
end;

class function TdxGaugeCustomCircularThreeFourthScale.GetScaleType: TdxGaugeScaleType;
begin
  Result := stCircularThreeFourthScale;
end;

function TdxGaugeCustomCircularThreeFourthScale.GetOptionsLayoutClass: TdxGaugeScaleOptionsCustomLayoutClass;
begin
  Result := TdxGaugeScaleOptionsArcLayout;
end;

function TdxGaugeCustomCircularThreeFourthScale.GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass;
begin
  Result := TdxGaugeCustomCircularThreeFourthScaleViewInfo;
end;

function TdxGaugeCustomCircularThreeFourthScale.GetOptionsLayout: TdxGaugeScaleOptionsArcLayout;
begin
  Result := inherited OptionsLayout as TdxGaugeScaleOptionsArcLayout;
end;

procedure TdxGaugeCustomCircularThreeFourthScale.SetOptionsLayout(const AValue: TdxGaugeScaleOptionsArcLayout);
begin
  inherited OptionsLayout := AValue;
end;

{ TdxGaugeCustomCircularWideScaleViewInfo }

function TdxGaugeCustomCircularWideScaleViewInfo.GetClipArea: TdxRectF;
begin
  Result := GetClipRect;
  Result.Top := inherited GetClipArea.Top;
end;

function TdxGaugeCustomCircularWideScaleViewInfo.GetLayoutSize(const ABounds: TdxRectF): TdxSizeF;
begin
  Result := dxGaugeGetRectangularScaleLayoutSize(ABounds, FParameters.Width, FParameters.Height, FParameters.WidthFactor,
    FParameters.HeightFactor);
end;

procedure TdxGaugeCustomCircularWideScaleViewInfo.CalculateScaleInfo;
var
  ASize: TdxSizeF;
begin
  inherited CalculateScaleInfo;
  ASize := GetContentOriginalSize;
  ScaleInfo.Selection := cxRect(GetScaledRect(ASize,
    dxPointF(ScaleInfo.Bounds.Width / ASize.cx, ScaleInfo.Bounds.Height / ASize.cy)), False);
end;

procedure TdxGaugeCustomCircularWideScaleViewInfo.DoDrawSpindleCap(AGPGraphics: TdxGPGraphics);
begin
  AGPGraphics.SaveClipRegion;
  try
    AGPGraphics.SetClipRect(GetContentBounds, gmIntersect);
    inherited DoDrawSpindleCap(AGPGraphics);
  finally
    AGPGraphics.RestoreClipRegion;
  end;
end;

procedure TdxGaugeCustomCircularWideScaleViewInfo.DoDrawValueIndicator(AGPGraphics: TdxGPGraphics);
begin
  AGPGraphics.SaveClipRegion;
  try
    AGPGraphics.SetClipRect(GetClipRect, gmIntersect);
    inherited DoDrawValueIndicator(AGPGraphics);
  finally
    AGPGraphics.RestoreClipRegion;
  end;
end;

function TdxGaugeCustomCircularWideScaleViewInfo.GetClipRect: TdxRectF;
begin
  Result := GetContentBounds;
  Result := cxRectInflate(cxRectF(cxRectCenter(Result), cxRectCenter(Result)), Result.Width / 2,
    GetDefaultParameters.ValueIndicatorClipAreaHeightFactor * Result.Height / 2);
end;

function TdxGaugeCustomCircularWideScaleViewInfo.GetDefaultParameters: TdxGaugeCircularWideScaleDefaultParameters;
begin
  Result := DefaultParameters as TdxGaugeCircularWideScaleDefaultParameters;
end;

{ TdxGaugeCustomCircularWideScale }

class function TdxGaugeCustomCircularWideScale.GetScaleName: string;
begin
  Result := inherited GetScaleName + ' Wide';
end;

class function TdxGaugeCustomCircularWideScale.GetScaleType: TdxGaugeScaleType;
begin
  Result := stCircularWideScale;
end;

class function TdxGaugeCustomCircularWideScale.GetStyleReaderClass: TdxGaugeCustomScaleStyleReaderClass;
begin
  Result := TdxGaugeCircularWideScaleStyleReader;
end;

function TdxGaugeCustomCircularWideScale.GetOptionsLayoutClass: TdxGaugeScaleOptionsCustomLayoutClass;
begin
  Result := TdxGaugeScaleOptionsRectangularLayout;
end;

function TdxGaugeCustomCircularWideScale.GetRangeClass: TdxGaugeCustomRangeClass;
begin
  Result := TdxGaugeCircularWideScaleRange;
end;

function TdxGaugeCustomCircularWideScale.GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass;
begin
  Result := TdxGaugeCustomCircularWideScaleViewInfo;
end;

function TdxGaugeCustomCircularWideScale.GetOptionsLayout: TdxGaugeScaleOptionsRectangularLayout;
begin
  Result := inherited OptionsLayout as TdxGaugeScaleOptionsRectangularLayout;
end;

procedure TdxGaugeCustomCircularWideScale.SetOptionsLayout(const AValue: TdxGaugeScaleOptionsRectangularLayout);
begin
  inherited OptionsLayout := AValue;
end;

{ TdxGaugeArcQuantitativeScaleStyleReader }

class function TdxGaugeArcQuantitativeScaleStyleReader.GetResourceNamePrefix: string;
begin
  Result := 'Circular';
end;

function TdxGaugeArcQuantitativeScaleStyleReader.GetDefaultParametersClass: TdxGaugeCustomScaleDefaultParametersClass;
begin
  Result := TdxGaugeArcQuantitativeScaleDefaultParameters;
end;

procedure TdxGaugeArcQuantitativeScaleStyleReader.ReadCommonScaleParameters(ANode: TdxXMLNode;
  AParameters: TdxGaugeQuantitativeScaleDefaultParameters);
var
  ATempNode: TdxXMLNode;
  ACustomCircularParameters: TdxGaugeArcQuantitativeScaleDefaultParameters;
begin
  inherited ReadCommonScaleParameters(ANode, AParameters);
  ACustomCircularParameters := AParameters as TdxGaugeArcQuantitativeScaleDefaultParameters;
  ACustomCircularParameters.AngleEnd := GetAttributeValueAsInteger(GetChildNode(ANode, 'AngleEnd'), 'Value');
  ACustomCircularParameters.AngleStart := GetAttributeValueAsInteger(GetChildNode(ANode, 'AngleStart'), 'Value');
  ACustomCircularParameters.ScaleArcRadiusFactor := GetAttributeValueAsDouble(GetChildNode(ANode, 'ArcRadiusFactor'),
    'Value');
  ReadNeedleParameters(ANode, ACustomCircularParameters);
  ReadSpindleCapParameters(ANode, ACustomCircularParameters);
  ATempNode := GetChildNode(ANode, 'CenterPointPositionFactor');
  if ATempNode <> nil then
    ACustomCircularParameters.CenterPointPositionFactor := GetAttributeValueAsPointF(GetChildNode(ANode,
      'CenterPointPositionFactor'), 'Value')
  else
    ACustomCircularParameters.CenterPointPositionFactor := cxPointF(0.5, 0.5);
  if GetChildNode(ANode, 'AxisLineColor', ATempNode) then
    ACustomCircularParameters.AxisLineColor := dxColorToAlphaColor(GetAttributeValueAsColor(ATempNode, 'Value'));
  if GetChildNode(ANode, 'AxisLineWidth', ATempNode) then
    ACustomCircularParameters.AxisLineWidth := GetAttributeValueAsDouble(ATempNode, 'Value');
end;

procedure TdxGaugeArcQuantitativeScaleStyleReader.ReadNeedleParameters(ANode: TdxXMLNode;
  AParameters: TdxGaugeArcQuantitativeScaleDefaultParameters);
var
  ANeedleNode: TdxXMLNode;
begin
  if GetChildNode(ANode, 'Needle', ANeedleNode) then
  begin
    AParameters.ValueIndicatorStartOffset := GetAttributeValueAsDouble(GetChildNode(ANeedleNode, 'StartOffset'), 'Value');
    AParameters.ValueIndicatorEndOffset := GetAttributeValueAsDouble(GetChildNode(ANeedleNode, 'EndOffset'), 'Value');
    if GetChildNode(ANeedleNode, 'ScaleFactor') <> nil then
      AParameters.ValueIndicatorScaleFactor := GetAttributeValueAsPointF(GetChildNode(ANeedleNode, 'ScaleFactor'), 'Value')
    else
      AParameters.ValueIndicatorScaleFactor := dxPointF(1, 1);
  end;
end;

procedure TdxGaugeArcQuantitativeScaleStyleReader.ReadSpindleCapParameters(ANode: TdxXMLNode;
  AParameters: TdxGaugeArcQuantitativeScaleDefaultParameters);
var
  ASpindleCapNode: TdxXMLNode;
begin
  if GetChildNode(ANode, 'SpindleCap', ASpindleCapNode) then
  begin
    AParameters.SpindleCapSize := GetAttributeValueAsDouble(GetChildNode(ASpindleCapNode, 'Size'), 'Value');
    if GetChildNode(ASpindleCapNode, 'UnderNeedle') <> nil then
      AParameters.SpindleCapUnderNeedle := GetAttributeValueAsBoolean(GetChildNode(ASpindleCapNode, 'UnderNeedle'), 'Value');
  end;
end;

{ TdxGaugeCustomCircularHalfScaleStyleReader }

class function TdxGaugeCustomCircularHalfScaleStyleReader.GetResourceNamePrefix: string;
begin
  Result := 'CircularHalf';
end;

{ TdxGaugeCircularWideScaleStyleReader }

class function TdxGaugeCircularWideScaleStyleReader.GetResourceNamePrefix: string;
begin
  Result := inherited GetResourceNamePrefix + 'Wide';
end;

function TdxGaugeCircularWideScaleStyleReader.GetDefaultParametersClass: TdxGaugeCustomScaleDefaultParametersClass;
begin
  Result := TdxGaugeCircularWideScaleDefaultParameters;
end;

procedure TdxGaugeCircularWideScaleStyleReader.ReadNeedleParameters(ANode: TdxXMLNode;
  AParameters: TdxGaugeArcQuantitativeScaleDefaultParameters);
var
  ATempNode: TdxXMLNode;
begin
  inherited ReadNeedleParameters(ANode, AParameters);
  if GetChildNode(ANode, 'Needle', ATempNode) and GetChildNode(ATempNode, 'ClipAreaHeightFactor', ATempNode) then
    TdxGaugeCircularWideScaleDefaultParameters(AParameters).ValueIndicatorClipAreaHeightFactor :=
      GetAttributeValueAsDouble(ATempNode, 'Value')
  else
    TdxGaugeCircularWideScaleDefaultParameters(AParameters).ValueIndicatorClipAreaHeightFactor := 1;
end;

{ TdxGaugeCustomCircularScaleRangeViewInfo }

function TdxGaugeCustomCircularScaleRangeViewInfo.GetParametersClass: TdxGaugeCustomRangeParametersClass;
begin
  Result := TdxGaugeCircularRangeParameters;
end;

procedure TdxGaugeCustomCircularScaleRangeViewInfo.CalculateBounds;
var
  ARadius: Single;
begin
  ARadius := GetRadius;
  FBounds := cxRectInflate(cxRectF(GetScaleInfo.RotatePoint, GetScaleInfo.RotatePoint), ARadius, ARadius);
  CalculateSectorWidth;
end;

procedure TdxGaugeCustomCircularScaleRangeViewInfo.CalculateSelectionBounds;

  procedure CheckBounds;
  begin
    FSelectionBounds.Top := Max(ScaleInfo.Selection.Top, FSelectionBounds.Top);
    FSelectionBounds.Bottom := Min(ScaleInfo.Selection.Bottom, FSelectionBounds.Bottom);
    FSelectionBounds.Left := Max(ScaleInfo.Selection.Left, FSelectionBounds.Left);
    FSelectionBounds.Right := Min(ScaleInfo.Selection.Right, FSelectionBounds.Right);
  end;

var
  AMatrix: TdxGPMatrix;
  AParameters: TdxGaugeCircularRangeParameters;
  R: TdxRectF;
begin
  AParameters := GetParameters;
  if (AParameters.ValueEnd <> AParameters.ValueStart) and (AParameters.ValueEndPosition <> AParameters.ValueStartPosition) then
    R := dxGaugeGetSectorRect(FBounds, FWidth, AParameters.ValueStartPosition, AParameters.ValueEndPosition -
      AParameters.ValueStartPosition)
  else
    R := dxGaugeGetSectorRect(FBounds, FWidth, ScaleInfo.GetValuePositionFunc(ScaleInfo.MaxValue), ScaleInfo.GetValuePositionFunc(ScaleInfo.MinValue) -
      ScaleInfo.GetValuePositionFunc(ScaleInfo.MaxValue));
  FSelectionBounds := R;
  FSelectionBounds.Left := Max(FBounds.Left, R.Left);
  FSelectionBounds.Right := Min(FBounds.Right, R.Right);
  FSelectionBounds.Top := Max(FBounds.Top, R.Top);
  FSelectionBounds.Bottom := Min(FBounds.Bottom, R.Bottom);
  AMatrix := TdxGPMatrix.Create;
  try
    AMatrix.Scale(ScaleInfo.ScaleFactor, cxRectCenter(ScaleInfo.Bounds));
    FSelectionBounds := AMatrix.TransformRect(FSelectionBounds);
  finally
    AMatrix.Free;
  end;
  CheckBounds;
end;

procedure TdxGaugeCustomCircularScaleRangeViewInfo.DoDraw(AGPGraphics: TdxGPGraphics);
var
  AParameters: TdxGaugeCircularRangeParameters;
begin
  AParameters := GetParameters;
  dxGaugeDrawSector(AGPGraphics, FBounds, FWidth, AParameters.ValueStartPosition, AParameters.ValueEndPosition -
    AParameters.ValueStartPosition, AParameters.Color, AParameters.Color);
end;

function TdxGaugeCustomCircularScaleRangeViewInfo.GetRadius: Single;
var
  ABounds: TdxRectF;
begin
  ABounds := GetBounds;
  Result := Min(ABounds.Width, ABounds.Height) * GetParameters.RadiusFactor;
end;

procedure TdxGaugeCustomCircularScaleRangeViewInfo.CalculateSectorWidth;
begin
  FWidth := Min(GetBounds.Width, GetBounds.Height) * GetParameters.WidthFactor;
end;

function TdxGaugeCustomCircularScaleRangeViewInfo.GetBounds: TdxRectF;
begin
  Result := ScaleInfo.ContentBounds;
  if GetScaleInfo.RotatePoint.Y <= cxRectCenter(GetScaleInfo.ContentBounds).Y then
    Result.Top := GetScaleInfo.RotatePoint.Y
  else
    Result.Bottom := GetScaleInfo.RotatePoint.Y;
  if GetScaleInfo.RotatePoint.X <= cxRectCenter(GetScaleInfo.ContentBounds).X then
    Result.Left := GetScaleInfo.RotatePoint.X
  else
    Result.Right := GetScaleInfo.RotatePoint.X;
end;

function TdxGaugeCustomCircularScaleRangeViewInfo.GetParameters: TdxGaugeCircularRangeParameters;
begin
  Result := Parameters as TdxGaugeCircularRangeParameters;
end;

function TdxGaugeCustomCircularScaleRangeViewInfo.GetScaleInfo: TdxGaugeArcScaleInfo;
begin
  Result := ScaleInfo as TdxGaugeArcScaleInfo;
end;

{ TdxGaugeCustomCircularScaleRange }

constructor TdxGaugeCustomCircularScaleRange.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GetParameters.RadiusFactor := dxGaugeCircularScaleDefaultRadiusFactor;
end;

function TdxGaugeCustomCircularScaleRange.GetParametersClass: TdxGaugeCustomRangeParametersClass;
begin
  Result := TdxGaugeCircularRangeParameters;
end;

function TdxGaugeCustomCircularScaleRange.GetViewInfoClass: TdxGaugeCustomRangeViewInfoClass;
begin
  Result := TdxGaugeCustomCircularScaleRangeViewInfo;
end;

procedure TdxGaugeCustomCircularScaleRange.DefineProperties(AFiler: TFiler);
begin
  inherited DefineProperties(AFiler);
  AFiler.DefineProperty('RadiusFactor', ReadRadiusFactor, WriteRadiusFactor, SameValue(RadiusFactor, 0));
end;

function TdxGaugeCustomCircularScaleRange.GetViewInfo: TdxGaugeCustomCircularScaleRangeViewInfo;
begin
  Result := ViewInfo as TdxGaugeCustomCircularScaleRangeViewInfo;
end;

function TdxGaugeCustomCircularScaleRange.GetRadiusFactor: Single;
begin
  Result := GetParameters.RadiusFactor;
end;

procedure TdxGaugeCustomCircularScaleRange.SetRadiusFactor(const AValue: Single);
begin
  if GetParameters.RadiusFactor <> AValue then
  begin
    GetParameters.RadiusFactor := Min(Max(AValue, 0.001), 1);
    Changed(False);
  end;
end;

function TdxGaugeCustomCircularScaleRange.GetParameters: TdxGaugeCircularRangeParameters;
begin
  Result := Parameters as TdxGaugeCircularRangeParameters;
end;

procedure TdxGaugeCustomCircularScaleRange.ReadRadiusFactor(AReader: TReader);
begin
  RadiusFactor := AReader.ReadDouble;
end;

procedure TdxGaugeCustomCircularScaleRange.WriteRadiusFactor(AWriter: TWriter);
begin
  AWriter.WriteDouble(RadiusFactor);
end;

function TdxGaugeCustomCircularScaleRange.IsRadiusFactorStored: Boolean;
begin
  Result := not SameValue(GetParameters.RadiusFactor, dxGaugeCircularScaleDefaultRadiusFactor);
end;

{ TdxGaugeCircularWideScaleRangeViewInfo }

function TdxGaugeCircularWideScaleRangeViewInfo.GetRadius: Single;
begin
  Result := Abs(GetScaleInfo.ContentBounds.Bottom - GetScaleInfo.RotatePoint.Y) + GetScaleInfo.ContentBounds.Height *
    GetParameters.RadiusFactor;
end;

procedure TdxGaugeCircularWideScaleRangeViewInfo.CalculateSectorWidth;
begin
  FWidth := ScaleInfo.ContentBounds.Height * GetParameters.WidthFactor;
end;

procedure TdxGaugeCircularWideScaleRangeViewInfo.DoDraw(AGPGraphics: TdxGPGraphics);
begin
  AGPGraphics.SaveClipRegion;
  try
    AGPGraphics.SetClipRect(GetScaleInfo.ClipArea, gmIntersect);
    inherited DoDraw(AGPGraphics);
  finally
    AGPGraphics.RestoreClipRegion;
  end;
end;

{ TdxGaugeCircularWideScaleRange }

function TdxGaugeCircularWideScaleRange.GetViewInfoClass: TdxGaugeCustomRangeViewInfoClass;
begin
  Result := TdxGaugeCircularWideScaleRangeViewInfo;
end;

initialization
  dxGaugeRegisterScale(TdxGaugeCircularScale);
  dxGaugeRegisterScale(TdxGaugeCircularHalfScale);
  dxGaugeRegisterScale(TdxGaugeCircularQuarterLeftScale);
  dxGaugeRegisterScale(TdxGaugeCircularQuarterRightScale);
  dxGaugeRegisterScale(TdxGaugeCircularThreeFourthScale);
  dxGaugeRegisterScale(TdxGaugeCircularWideScale);

  Classes.RegisterClasses([TdxGaugeCircularScaleRange, TdxGaugeCircularWideScaleRange]);

finalization
  Classes.UnRegisterClasses([TdxGaugeCircularScaleRange, TdxGaugeCircularWideScaleRange]);

  dxGaugeUnregisterScale(TdxGaugeCircularWideScale);
  dxGaugeUnregisterScale(TdxGaugeCircularThreeFourthScale);
  dxGaugeUnregisterScale(TdxGaugeCircularQuarterRightScale);
  dxGaugeUnregisterScale(TdxGaugeCircularQuarterLeftScale);
  dxGaugeUnregisterScale(TdxGaugeCircularHalfScale);
  dxGaugeUnregisterScale(TdxGaugeCircularScale);

end.
