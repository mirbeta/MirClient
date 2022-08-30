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

unit dxGaugeLinearScale;

{$I cxVer.inc}

interface

uses
  Classes, Graphics, Windows, dxCore, cxGeometry, cxGraphics, dxXMLDoc, dxGDIPlusClasses, dxCompositeShape,
  dxGaugeCustomScale, dxGaugeQuantitativeScale;

type
  TdxGaugeCustomLinearScale = class;
  TdxGaugeLinearScaleAlignElements = TLeftRight;
  TdxGaugeLinearScaleLabelOrientation = TdxTextOrientation;

  { TdxGaugeLinearScaleDefaultParameters }

  TdxGaugeLinearScaleDefaultParameters = class(TdxGaugeQuantitativeScaleDefaultParameters)
  public
    LevelBarStartPosition: TdxPointF;
    LevelBarEndPosition: TdxPointF;
    LevelBarsScaleFactor: TdxPointF;

    constructor Create; override;
  end;

  { TdxGaugeLinearScaleParameters }

  TdxGaugeLinearScaleParameters = class(TdxGaugeQuantitativeScaleParameters)
  public
    AlignElements: TdxGaugeLinearScaleAlignElements;
    LabelOrientation: TdxGaugeLinearScaleLabelOrientation;
    RotationAngle: TcxRotationAngle;
  end;

  { TdxGaugeLinearScaleInfo }

  TdxGaugeLinearScaleInfo = class(TdxGaugeQuantitativeScaleInfo)
  public
    LevelBarLine: TdxRectF;
    NeedRotateBounds: Boolean;
    RotationAngle: Single;
  end;

  { TdxGaugeLinearRangeParameters }

  TdxGaugeLinearRangeParameters = class(TdxGaugeCustomRangeParameters)
  public
    CenterPositionFactor: Single;
  end;

  { TdxGaugeLinearScaleOptionsView }

  TdxGaugeLinearScaleOptionsView = class(TdxGaugeQuantitativeScaleOptionsView)
  private
    function GetAlignElements: TdxGaugeLinearScaleAlignElements;
    function GetLabelOrientation: TdxGaugeLinearScaleLabelOrientation;
    function GetRotationAngle: TcxRotationAngle;
    procedure SetAlignElements(const AValue: TdxGaugeLinearScaleAlignElements);
    procedure SetLabelOrientation(const AValue: TdxGaugeLinearScaleLabelOrientation);
    procedure SetRotationAngle(const AValue: TcxRotationAngle);

    function GetScale: TdxGaugeCustomLinearScale;
  published
    property LogarithmicBase;

    property AlignElements: TdxGaugeLinearScaleAlignElements read GetAlignElements write SetAlignElements
      default taLeftJustify;
    property LabelOrientation: TdxGaugeLinearScaleLabelOrientation read GetLabelOrientation write SetLabelOrientation
      default toLeftToRight;
    property ShowLevelBar: Boolean read GetShowValueIndicator write SetShowValueIndicator default True;
    property RotationAngle: TcxRotationAngle read GetRotationAngle write SetRotationAngle default raMinus90;
  end;

  { TdxGaugeCustomLinearScaleViewInfo }

  TdxGaugeCustomLinearScaleViewInfo = class(TdxGaugeQuantitativeScaleViewInfo)
  private
    FEmptyBar: TdxCompositeShape;
    FEndBar: TdxCompositeShape;
    FPackedBar: TdxCompositeShape;
    FStartBar: TdxCompositeShape;

    FBackgroundBounds: TdxRectF;
    FEmptyBarBounds: TdxRectF;
    FEndBarBounds: TdxRectF;
    FPackedBarBounds: TdxRectF;
    FStartBarBounds: TdxRectF;

    FLevelBarLine: TdxRectF;

    function GetDefaultParameters: TdxGaugeLinearScaleDefaultParameters;
    function GetLabelTextAlignment: TAlignment;
    function GetLabelTextOffset(ASize: TdxSizeF): Single;
    function GetLabelTextRotationAngle: Single;
    function GetParameters: TdxGaugeLinearScaleParameters;
    function GetRotationAngle: Single;
    function GetScaledImageRect(AImage: TGraphic): TdxRectF;
    function NeedRotateBounds: Boolean;
    procedure CalculateLevelBarLine;
    procedure CalculateBarsBounds;
    procedure RotateContent(AGPGraphics: TdxGPGraphics; AAngle: Single; const APivotPoint: TdxPointF);

    procedure DrawLevelBarBackground(AGPGraphics: TdxGPGraphics);
    procedure DrawLevelBarValue(AGPGraphics: TdxGPGraphics);
  protected
    function GetScaleInfoClass: TdxGaugeCustomScaleInfoClass; override;
    function GetParametersClass: TdxGaugeCustomScaleParametersClass; override;

    function GetScaleEndPoint: TdxPointF; override;
    function GetScaleStartPoint: TdxPointF; override;

    function CanDrawValueIndicator: Boolean; override;
    function GetContentBounds: TdxRectF; override;
    function GetContentOriginalSize: TdxSizeF; override;
    function GetLabelTextRect(APosition: Single; const AText: string): TdxRectF; override;
    procedure CalculateContent; override;
    procedure CalculateReferenceParameters; override;
    procedure CalculateScaleInfo; override;
    procedure CalculateScaleFactor; override;
    procedure DoDrawBackground(AGPGraphics: TdxGPGraphics); override;
    procedure DoDrawTickmarkLabel(AGPGraphics: TdxGPGraphics; APosition: Single;
      const ADrawInfo: TdxGaugeQuantitativeScaleTickmarkLabelDrawInfo); override;
    procedure DoDrawTickmarkTick(AGPGraphics: TdxGPGraphics;  APosition: Single;
      const ADrawInfo: TdxGaugeQuantitativeScaleTickmarkTickDrawInfo); override;
    procedure DoDrawValueIndicator(AGPGraphics: TdxGPGraphics); override;
    procedure DrawElements(AGPGraphics: TdxGPGraphics); override;
    procedure LoadScaleElements; override;

    procedure BeginDraw(AGPGraphics: TdxGPGraphics);
    procedure EndDraw(AGPGraphics: TdxGPGraphics);

    property LevelBarLine: TdxRectF read FLevelBarLine;
  end;

  { TdxGaugeCustomLinearScale }

  TdxGaugeCustomLinearScale = class(TdxGaugeQuantitativeScale)
  private
    function GetAlignElements: TdxGaugeLinearScaleAlignElements;
    function GetLabelOrientation: TdxGaugeLinearScaleLabelOrientation;
    function GetOptionsLayout: TdxGaugeScaleOptionsRectangularLayout;
    function GetOptionsView: TdxGaugeLinearScaleOptionsView;
    function GetRotationAngle: TcxRotationAngle;
    procedure SetAlignElements(const AValue: TdxGaugeLinearScaleAlignElements);
    procedure SetLabelOrientation(const AValue: TdxGaugeLinearScaleLabelOrientation);
    procedure SetOptionsLayout(const AValue: TdxGaugeScaleOptionsRectangularLayout);
    procedure SetOptionsView(const AValue: TdxGaugeLinearScaleOptionsView);
    procedure SetRotationAngle(const AValue: TcxRotationAngle);

    function GetScaleParameters: TdxGaugeLinearScaleParameters;
    function GetViewInfo: TdxGaugeCustomLinearScaleViewInfo;
  protected
    class function GetLayerCount: Integer; override;
    class function GetScaleName: string; override;
    class function GetScaleType: TdxGaugeScaleType; override;
    class function GetStyleReaderClass: TdxGaugeCustomScaleStyleReaderClass; override;

    function GetOptionsLayoutClass: TdxGaugeScaleOptionsCustomLayoutClass; override;
    function GetOptionsViewClass: TdxGaugeCustomScaleOptionsViewClass; override;
    function GetParametersClass: TdxGaugeCustomScaleParametersClass; override;
    function GetRangeClass: TdxGaugeCustomRangeClass; override;
    function GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass; override;

    procedure DrawCaptions(AGPGraphics: TdxGPGraphics); override;
    procedure DrawLayer(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer); override;
    procedure InitParameters; override;

    property OptionsLayout: TdxGaugeScaleOptionsRectangularLayout read GetOptionsLayout write SetOptionsLayout;
    property OptionsView: TdxGaugeLinearScaleOptionsView read GetOptionsView write SetOptionsView;
  end;

  { TdxGaugeLinearScale }

  TdxGaugeLinearScale = class(TdxGaugeCustomLinearScale)
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

  { TdxGaugeCustomLinearScaleStyleReader }

  TdxGaugeCustomLinearScaleStyleReader = class(TdxGaugeQuantitativeScaleStyleReader)
  protected
    class function GetResourceNamePrefix: string; override;
    function GetDefaultParametersClass: TdxGaugeCustomScaleDefaultParametersClass; override;
    procedure ReadCommonScaleParameters(ANode: TdxXMLNode;
      AParameters: TdxGaugeQuantitativeScaleDefaultParameters); override;
  end;

  { TdxGaugeLinearScaleRangeViewInfo }

  TdxGaugeLinearScaleRangeViewInfo = class(TdxGaugeCustomRangeViewInfo)
  private
    function GetParameters: TdxGaugeLinearRangeParameters;
  protected
    function GetParametersClass: TdxGaugeCustomRangeParametersClass; override;
    procedure CalculateBounds; override;
    procedure CalculateSelectionBounds; override;
    procedure DoDraw(AGPGraphics: TdxGPGraphics); override;
  end;

  { TdxGaugeLinearScaleRange }

  TdxGaugeLinearScaleRange = class(TdxGaugeCustomRange)
  private
    function GetCenterPositionFactor: Single;
    procedure SetCenterPositionFactor(const AValue: Single);

    function GetParameters: TdxGaugeLinearRangeParameters;

    procedure ReadCenterPositionFactor(AReader: TReader);
    procedure WriteCenterPositionFactor(AWriter: TWriter);

    function IsCenterPositionFactorStored: Boolean;
  protected
    function GetParametersClass: TdxGaugeCustomRangeParametersClass; override;
    function GetViewInfoClass: TdxGaugeCustomRangeViewInfoClass; override;
    procedure DefineProperties(AFiler: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property CenterPositionFactor: Single read GetCenterPositionFactor write SetCenterPositionFactor
      stored IsCenterPositionFactorStored;
    property Color;
    property LinkedWithScaleValue;
    property ValueEnd;
    property ValueStart;
    property Visible;
    property WidthFactor;
  end;

implementation

uses
  Math, Types, dxCoreGraphics, dxGDIPlusAPI, dxGaugeUtils;

const
  dxGaugeLinearRangeCenterPositionFactor = 0.5;

type
  TdxGaugeScaleStyleAccess = class(TdxGaugeScaleStyle);

function dxGaugeLinearScaleValueToPoint(AValue, AMinValue: Single; const ALevelBarLine: TdxRectF;
  AValueRange: Single): TdxPointF;
begin
  Result.X := ALevelBarLine.Left;
  Result.Y := ALevelBarLine.Bottom - (AValue - AMinValue) * ALevelBarLine.Height / AValueRange;
end;

{ TdxGaugeLinearScaleDefaultParameters }

constructor TdxGaugeLinearScaleDefaultParameters.Create;
begin
  inherited Create;
  LevelBarStartPosition := dxPointF(cxNullPoint);
  LevelBarEndPosition := dxPointF(cxNullPoint);
  LevelBarsScaleFactor := dxPointF(cxNullPoint);
end;

{ TdxGaugeLinearScaleOptionsView }

function TdxGaugeLinearScaleOptionsView.GetAlignElements: TdxGaugeLinearScaleAlignElements;
begin
  Result := GetScale.GetAlignElements;
end;

function TdxGaugeLinearScaleOptionsView.GetLabelOrientation: TdxGaugeLinearScaleLabelOrientation;
begin
  Result := GetScale.GetLabelOrientation;
end;

function TdxGaugeLinearScaleOptionsView.GetRotationAngle: TcxRotationAngle;
begin
  Result := GetScale.GetRotationAngle;
end;

procedure TdxGaugeLinearScaleOptionsView.SetAlignElements(const AValue: TdxGaugeLinearScaleAlignElements);
begin
  GetScale.SetAlignElements(AValue);
end;

procedure TdxGaugeLinearScaleOptionsView.SetLabelOrientation(const AValue: TdxGaugeLinearScaleLabelOrientation);
begin
  GetScale.SetLabelOrientation(AValue);
end;

procedure TdxGaugeLinearScaleOptionsView.SetRotationAngle(const AValue: TcxRotationAngle);
begin
  GetScale.SetRotationAngle(AValue);
end;

function TdxGaugeLinearScaleOptionsView.GetScale: TdxGaugeCustomLinearScale;
begin
  Result := inherited GetScale as TdxGaugeCustomLinearScale;
end;

{ TdxGaugeCustomLinearScaleViewInfo }

function TdxGaugeCustomLinearScaleViewInfo.GetScaleInfoClass: TdxGaugeCustomScaleInfoClass;
begin
  Result := TdxGaugeLinearScaleInfo;
end;

function TdxGaugeCustomLinearScaleViewInfo.GetParametersClass: TdxGaugeCustomScaleParametersClass;
begin
  Result := TdxGaugeLinearScaleParameters;
end;

function TdxGaugeCustomLinearScaleViewInfo.GetScaleEndPoint: TdxPointF;
begin
  Result := FLevelBarLine.TopLeft;
end;

function TdxGaugeCustomLinearScaleViewInfo.GetScaleStartPoint: TdxPointF;
begin
  Result := FLevelBarLine.BottomRight;
end;

function TdxGaugeCustomLinearScaleViewInfo.CanDrawValueIndicator: Boolean;
begin
  Result := CanDrawImage(FEndBar) or CanDrawImage(FEmptyBar) or CanDrawImage(FStartBar) or CanDrawImage(FPackedBar);
end;

function TdxGaugeCustomLinearScaleViewInfo.GetContentBounds: TdxRectF;
begin
  Result := inherited GetContentBounds;
  if NeedRotateBounds then
    Result := GetScaledRect(dxSizeF(Result.Height, Result.Width));
end;

function TdxGaugeCustomLinearScaleViewInfo.GetContentOriginalSize: TdxSizeF;
begin
  Result := GetElementImageOriginalSize(FBackground);
  if NeedRotateBounds then
    Result := dxSizeF(Result.cy, Result.cx);
end;

function TdxGaugeCustomLinearScaleViewInfo.GetLabelTextRect(APosition: Single; const AText: string): TdxRectF;
const
  ElementOffsetSing: array[TdxGaugeLinearScaleAlignElements] of Integer = (-1, 1);
var
  P: TdxPointF;
  ASize: TdxSizeF;
  AOffset: Single;
  AScaleParameters: TdxGaugeLinearScaleParameters;
begin
  ASize := GetLabelTextRectSize(AText);
  AScaleParameters := GetParameters;
  AOffset := GetLabelTextOffset(ASize);
  AOffset := GetDefaultParameters.LabelOffset + AOffset;
  if AScaleParameters.AlignElements = taRightJustify then
    AOffset := AOffset + 1;
  P := dxPointF(FLevelBarLine.Left + ElementOffsetSing[AScaleParameters.AlignElements] * AOffset, APosition);
  Result := cxRectInflate(cxRectF(P, P), ASize.cx / 2, ASize.cy / 2);
end;

procedure TdxGaugeCustomLinearScaleViewInfo.CalculateContent;
begin
  inherited CalculateContent;
  FBackgroundBounds := GetScaledRect(GetElementImageOriginalSize(FBackground));
  CalculateBarsBounds;
end;

procedure TdxGaugeCustomLinearScaleViewInfo.CalculateReferenceParameters;
begin
  CalculateLevelBarLine;
end;

procedure TdxGaugeCustomLinearScaleViewInfo.CalculateScaleInfo;
var
  AScaleInfo: TdxGaugeLinearScaleInfo;
begin
  inherited CalculateScaleInfo;
  AScaleInfo := ScaleInfo as TdxGaugeLinearScaleInfo;
  AScaleInfo.LevelBarLine := LevelBarLine;
  AScaleInfo.NeedRotateBounds := NeedRotateBounds;
  AScaleInfo.RotationAngle := GetRotationAngle;
  if NeedRotateBounds then
    ScaleInfo.ContentBounds := GetScaledRect(dxSizeF(ScaleInfo.ContentBounds.Height, ScaleInfo.ContentBounds.Width));
end;

procedure TdxGaugeCustomLinearScaleViewInfo.CalculateScaleFactor;
begin
  inherited CalculateScaleFactor;
  if NeedRotateBounds then
    FScaleFactor := dxPointF(FScaleFactor.Y, FScaleFactor.X);
end;

procedure TdxGaugeCustomLinearScaleViewInfo.DoDrawBackground(AGPGraphics: TdxGPGraphics);
begin
  dxGaugeDrawImage(AGPGraphics, FBackground, FBackgroundBounds);
end;

procedure TdxGaugeCustomLinearScaleViewInfo.DoDrawTickmarkLabel(AGPGraphics: TdxGPGraphics; APosition: Single;
  const ADrawInfo: TdxGaugeQuantitativeScaleTickmarkLabelDrawInfo);
begin
  dxGaugeDrawText(AGPGraphics, ADrawInfo.Text, ADrawInfo.Rect, Font, ADrawInfo.Color, GetLabelTextRotationAngle,
    GetLabelTextAlignment);
end;

procedure TdxGaugeCustomLinearScaleViewInfo.DoDrawTickmarkTick(AGPGraphics: TdxGPGraphics; APosition: Single;
  const ADrawInfo: TdxGaugeQuantitativeScaleTickmarkTickDrawInfo);

  function GetTickRect(const ASize: TdxSizeF): TdxRectF;
  begin
    if GetParameters.AlignElements = taRightJustify then
      Result.Left := FLevelBarLine.Left + ADrawInfo.Offset
    else
      Result.Left := FLevelBarLine.Left - ADrawInfo.Offset - ASize.cx;
    Result.Right := Result.Left + ASize.cx;
    Result.Top := APosition - ASize.cy / 2;
    Result.Bottom := Result.Top + ASize.cy;
  end;

const
  TickRotationAngleMap: array[TdxGaugeLinearScaleAlignElements] of Single = (180, 0);
var
  R: TdxRectF;
begin
  AGPGraphics.SaveWorldTransform;
  try
    R := GetTickRect(ADrawInfo.Size);
    if GetDefaultParameters.RotateTicks then
      AGPGraphics.RotateWorldTransform(-TickRotationAngleMap[GetParameters.AlignElements], cxRectCenter(R));
    dxGaugeDrawImage(AGPGraphics, ADrawInfo.Image, R);
  finally
    AGPGraphics.RestoreWorldTransform;
  end;
end;

procedure TdxGaugeCustomLinearScaleViewInfo.DoDrawValueIndicator(AGPGraphics: TdxGPGraphics);
begin
  DrawLevelBarBackground(AGPGraphics);
  DrawLevelBarValue(AGPGraphics);
end;

procedure TdxGaugeCustomLinearScaleViewInfo.DrawElements(AGPGraphics: TdxGPGraphics);
begin
  if GetParameters.ShowValueIndicator then
    DrawLevelBarBackground(AGPGraphics);
  inherited DrawElements(AGPGraphics);
end;

procedure TdxGaugeCustomLinearScaleViewInfo.LoadScaleElements;
begin
  inherited LoadScaleElements;
  FStartBar := TdxGaugeScaleStyleAccess(Style).GetElement(etLinearBarStart);
  FEndBar := TdxGaugeScaleStyleAccess(Style).GetElement(etLinearBarEnd);
  FEmptyBar := TdxGaugeScaleStyleAccess(Style).GetElement(etLinearBarEmpty);
  FPackedBar := TdxGaugeScaleStyleAccess(Style).GetElement(etLinearBarPacked);
end;

procedure TdxGaugeCustomLinearScaleViewInfo.BeginDraw(AGPGraphics: TdxGPGraphics);
begin
  AGPGraphics.SaveWorldTransform;
  RotateContent(AGPGraphics, -GetRotationAngle, cxRectCenter(GetContentBounds));
end;

procedure TdxGaugeCustomLinearScaleViewInfo.EndDraw(AGPGraphics: TdxGPGraphics);
begin
  AGPGraphics.RestoreWorldTransform;
end;

function TdxGaugeCustomLinearScaleViewInfo.GetDefaultParameters: TdxGaugeLinearScaleDefaultParameters;
begin
  Result := DefaultParameters as TdxGaugeLinearScaleDefaultParameters;
end;

function TdxGaugeCustomLinearScaleViewInfo.GetLabelTextAlignment: TAlignment;
type
  TdxRotationAngleSet = set of TcxRotationAngle;
  function GetTextAlignment(AValidAngles: TdxRotationAngleSet; ARotationAngle: TcxRotationAngle):
    TAlignment;
  begin
    Result := taCenter;
    if GetParameters.RotationAngle in AValidAngles then
      if GetParameters.RotationAngle = ARotationAngle then
        Result := GetParameters.AlignElements
      else
        if GetParameters.AlignElements = taLeftJustify then
          Result := taRightJustify
        else
          Result := taLeftJustify;
  end;

begin
  Result := taCenter;
  case GetParameters.LabelOrientation of
    toLeftToRight:
      Result := GetTextAlignment([raPlus90, raMinus90], raPlus90);
    toTopToBottom:
      Result := GetTextAlignment([ra0, ra180], ra0);
    toBottomToTop:
      Result := GetTextAlignment([ra0, ra180], ra180);
  end;
end;

function TdxGaugeCustomLinearScaleViewInfo.GetLabelTextOffset(ASize: TdxSizeF): Single;
begin
  case GetParameters.RotationAngle of
    ra0, ra180:
        if GetParameters.LabelOrientation in [toTopToBottom, toBottomToTop] then
          Result := ASize.cx / 2
        else
          Result := ASize.cy / 2;
    raPlus90, raMinus90:
        if GetParameters.LabelOrientation in [toTopToBottom, toBottomToTop] then
          Result := ASize.cy / 2
        else
          Result := ASize.cx / 2;
    else
      Result := 0;
  end;
end;

function TdxGaugeCustomLinearScaleViewInfo.GetLabelTextRotationAngle: Single;
var
  ADelta: Single;
begin
  case GetParameters.LabelOrientation of
    toTopToBottom:
      ADelta := 90;
    toBottomToTop:
      ADelta := -90;
    else
      ADelta := 0;
  end;
  Result := GetRotationAngle + ADelta;
end;

function TdxGaugeCustomLinearScaleViewInfo.GetParameters: TdxGaugeLinearScaleParameters;
begin
  Result := FParameters as TdxGaugeLinearScaleParameters;
end;

function TdxGaugeCustomLinearScaleViewInfo.GetRotationAngle: Single;
const
  RotationAngleMap: array[TcxRotationAngle] of Single = (90, 180, 0, -90);
begin
  Result := RotationAngleMap[GetParameters.RotationAngle];
end;

function TdxGaugeCustomLinearScaleViewInfo.GetScaledImageRect(AImage: TGraphic): TdxRectF;
begin
  Result := cxRectInflate(GetScaledRect(GetElementImageOriginalSize(AImage)),
    GetDefaultParameters.LevelBarsScaleFactor.X)
end;

function TdxGaugeCustomLinearScaleViewInfo.NeedRotateBounds: Boolean;
begin
  Result := GetParameters.RotationAngle in [ra0, ra180];
end;

procedure TdxGaugeCustomLinearScaleViewInfo.CalculateLevelBarLine;
begin
  FLevelBarLine := GetScaledRect(dxSizeF(0, GetDefaultParameters.LevelBarStartPosition.Y -
    GetDefaultParameters.LevelBarEndPosition.Y));
end;

procedure TdxGaugeCustomLinearScaleViewInfo.CalculateBarsBounds;
begin
  FPackedBarBounds := GetScaledImageRect(FPackedBar);
  FPackedBarBounds.Bottom := FLevelBarLine.Bottom + 1;
  FPackedBarBounds.Top := ScaleValuePosition;

  FEndBarBounds := GetScaledImageRect(FEndBar);
  FEndBarBounds.Top := FLevelBarLine.Top - FEndBarBounds.Height;
  FEndBarBounds.Bottom := FLevelBarLine.Top + 1;

  FEmptyBarBounds := GetScaledImageRect(FEmptyBar);
  FEmptyBarBounds.Top := FLevelBarLine.Top;
  FEmptyBarBounds.Bottom := FLevelBarLine.Bottom + 1;

  FStartBarBounds := GetScaledImageRect(FStartBar);
  FStartBarBounds.Bottom := FLevelBarLine.Bottom + FStartBarBounds.Height;
  FStartBarBounds.Top := FLevelBarLine.Bottom;
end;

procedure TdxGaugeCustomLinearScaleViewInfo.RotateContent(AGPGraphics: TdxGPGraphics; AAngle: Single;
  const APivotPoint: TdxPointF);
begin
  AGPGraphics.RotateWorldTransform(AAngle, APivotPoint);
end;

procedure TdxGaugeCustomLinearScaleViewInfo.DrawLevelBarBackground(AGPGraphics: TdxGPGraphics);
begin
  dxGaugeDrawImage(AGPGraphics, FEndBar, FEndBarBounds);
  dxGaugeDrawImage(AGPGraphics, FEmptyBar, FEmptyBarBounds);
  dxGaugeDrawImage(AGPGraphics, FStartBar, FStartBarBounds);
end;

procedure TdxGaugeCustomLinearScaleViewInfo.DrawLevelBarValue(AGPGraphics: TdxGPGraphics);
begin
  dxGaugeDrawImage(AGPGraphics, FPackedBar, FPackedBarBounds);
end;

{ TdxGaugeCustomLinearScale }

class function TdxGaugeCustomLinearScale.GetLayerCount: Integer;
begin
  Result := 5;
end;

class function TdxGaugeCustomLinearScale.GetScaleName: string;
begin
  Result := 'Linear';
end;

class function TdxGaugeCustomLinearScale.GetScaleType: TdxGaugeScaleType;
begin
  Result := stLinearScale;
end;

class function TdxGaugeCustomLinearScale.GetStyleReaderClass: TdxGaugeCustomScaleStyleReaderClass;
begin
  Result := TdxGaugeCustomLinearScaleStyleReader;
end;

function TdxGaugeCustomLinearScale.GetOptionsLayoutClass: TdxGaugeScaleOptionsCustomLayoutClass;
begin
  Result := TdxGaugeScaleOptionsRectangularLayout;
end;

function TdxGaugeCustomLinearScale.GetOptionsViewClass: TdxGaugeCustomScaleOptionsViewClass;
begin
  Result := TdxGaugeLinearScaleOptionsView;
end;

function TdxGaugeCustomLinearScale.GetParametersClass: TdxGaugeCustomScaleParametersClass;
begin
  Result := TdxGaugeLinearScaleParameters;
end;

function TdxGaugeCustomLinearScale.GetRangeClass: TdxGaugeCustomRangeClass;
begin
  Result := TdxGaugeLinearScaleRange;
end;

function TdxGaugeCustomLinearScale.GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass;
begin
  Result := TdxGaugeCustomLinearScaleViewInfo;
end;

procedure TdxGaugeCustomLinearScale.DrawCaptions(AGPGraphics: TdxGPGraphics);
begin
  AGPGraphics.SaveWorldTransform;
  try
    AGPGraphics.RotateWorldTransform(GetViewInfo.GetRotationAngle, cxRectCenter(GetViewInfo.GetContentBounds));
    inherited DrawCaptions(AGPGraphics);
  finally
    AGPGraphics.RestoreWorldTransform;
  end;
end;

procedure TdxGaugeCustomLinearScale.DrawLayer(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer);
begin
  GetViewInfo.BeginDraw(AGPGraphics);
  inherited DrawLayer(AGPGraphics, ALayerIndex);
  GetViewInfo.EndDraw(AGPGraphics);
end;

procedure TdxGaugeCustomLinearScale.InitParameters;
begin
  inherited InitParameters;
  GetScaleParameters.RotationAngle := raMinus90;
end;

function TdxGaugeCustomLinearScale.GetAlignElements: TdxGaugeLinearScaleAlignElements;
begin
  Result := GetScaleParameters.AlignElements;
end;

function TdxGaugeCustomLinearScale.GetLabelOrientation: TdxGaugeLinearScaleLabelOrientation;
begin
  Result := GetScaleParameters.LabelOrientation;
end;

function TdxGaugeCustomLinearScale.GetOptionsLayout: TdxGaugeScaleOptionsRectangularLayout;
begin
  Result := inherited OptionsLayout as TdxGaugeScaleOptionsRectangularLayout;
end;

function TdxGaugeCustomLinearScale.GetOptionsView: TdxGaugeLinearScaleOptionsView;
begin
  Result := inherited OptionsView as TdxGaugeLinearScaleOptionsView;
end;

function TdxGaugeCustomLinearScale.GetRotationAngle: TcxRotationAngle;
begin
  Result := GetScaleParameters.RotationAngle;
end;

procedure TdxGaugeCustomLinearScale.SetAlignElements(const AValue: TdxGaugeLinearScaleAlignElements);
var
  AScaleParameters: TdxGaugeLinearScaleParameters;
begin
  AScaleParameters := GetScaleParameters;
  if AScaleParameters.AlignElements <> AValue then
  begin
    AScaleParameters.AlignElements := AValue;
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomLinearScale.SetLabelOrientation(const AValue: TdxGaugeLinearScaleLabelOrientation);
var
  AScaleParameters: TdxGaugeLinearScaleParameters;
begin
  AScaleParameters := GetScaleParameters;
  if AScaleParameters.LabelOrientation <> AValue then
  begin
    AScaleParameters.LabelOrientation := AValue;
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomLinearScale.SetOptionsLayout(const AValue: TdxGaugeScaleOptionsRectangularLayout);
begin
  inherited OptionsLayout := AValue;
end;

procedure TdxGaugeCustomLinearScale.SetOptionsView(const AValue: TdxGaugeLinearScaleOptionsView);
begin
  inherited OptionsView := AValue;
end;

procedure TdxGaugeCustomLinearScale.SetRotationAngle(const AValue: TcxRotationAngle);
var
  AScaleParameters: TdxGaugeLinearScaleParameters;
begin
  AScaleParameters := GetScaleParameters;
  if AScaleParameters.RotationAngle <> AValue then
  begin
    AScaleParameters.RotationAngle := AValue;
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

function TdxGaugeCustomLinearScale.GetScaleParameters: TdxGaugeLinearScaleParameters;
begin
  Result := Parameters as TdxGaugeLinearScaleParameters;
end;

function TdxGaugeCustomLinearScale.GetViewInfo: TdxGaugeCustomLinearScaleViewInfo;
begin
  Result := inherited ViewInfo as TdxGaugeCustomLinearScaleViewInfo;
end;

{ TdxGaugeCustomLinearScaleStyleReader }

class function TdxGaugeCustomLinearScaleStyleReader.GetResourceNamePrefix: string;
begin
  Result := 'Linear';
end;

function TdxGaugeCustomLinearScaleStyleReader.GetDefaultParametersClass: TdxGaugeCustomScaleDefaultParametersClass;
begin
  Result := TdxGaugeLinearScaleDefaultParameters;
end;

procedure TdxGaugeCustomLinearScaleStyleReader.ReadCommonScaleParameters(ANode: TdxXMLNode;
  AParameters: TdxGaugeQuantitativeScaleDefaultParameters);
var
  ALinearParameters: TdxGaugeLinearScaleDefaultParameters;
begin
  inherited ReadCommonScaleParameters(ANode, AParameters);
  ALinearParameters := AParameters as TdxGaugeLinearScaleDefaultParameters;
  ALinearParameters.LevelBarStartPosition := GetAttributeValueAsPointF(GetChildNode(ANode, 'LevelBarStartPosition'),
    'Value');
  ALinearParameters.LevelBarEndPosition := GetAttributeValueAsPointF(GetChildNode(ANode, 'LevelBarEndPosition'),
    'Value');
  ALinearParameters.LevelBarsScaleFactor := GetAttributeValueAsPointF(GetChildNode(ANode, 'LevelBarsScaleFactor'),
    'Value');
end;

{ TdxGaugeLinearScaleRangeViewInfo }

function TdxGaugeLinearScaleRangeViewInfo.GetParametersClass: TdxGaugeCustomRangeParametersClass;
begin
  Result := TdxGaugeLinearRangeParameters;
end;

procedure TdxGaugeLinearScaleRangeViewInfo.CalculateBounds;
var
  AParameters: TdxGaugeLinearRangeParameters;
  AWidth: Single;
begin
  AParameters := GetParameters;
  AWidth := ScaleInfo.ContentBounds.Width * AParameters.WidthFactor;
  FBounds.Top := AParameters.ValueEndPosition;
  FBounds.Bottom := AParameters.ValueStartPosition;
  FBounds.Left := ScaleInfo.ContentBounds.Left + ScaleInfo.ContentBounds.Width * AParameters.CenterPositionFactor - AWidth / 2;
  FBounds.Right := FBounds.Left + AWidth;
  FBounds := cxRectAdjustF(FBounds);
end;

procedure TdxGaugeLinearScaleRangeViewInfo.CalculateSelectionBounds;
var
  AMatrix: TdxGPMatrix;
  AAngle: Single;
begin
  AMatrix := TdxGPMatrix.Create;
  try
    AAngle := (ScaleInfo as TdxGaugeLinearScaleInfo).RotationAngle;
    AMatrix.Rotate(-AAngle, cxRectCenter(ScaleInfo.Bounds));
    FSelectionBounds := AMatrix.TransformRect(FBounds);
    AMatrix.Scale(ScaleInfo.ScaleFactor, cxRectCenter(ScaleInfo.Bounds));
    FSelectionBounds := AMatrix.TransformRect(FSelectionBounds);
  finally
    AMatrix.Free;
  end;
end;

procedure TdxGaugeLinearScaleRangeViewInfo.DoDraw(AGPGraphics: TdxGPGraphics);
begin
  AGPGraphics.Rectangle(FBounds, GetParameters.Color, GetParameters.Color);
end;

function TdxGaugeLinearScaleRangeViewInfo.GetParameters: TdxGaugeLinearRangeParameters;
begin
  Result := Parameters as TdxGaugeLinearRangeParameters;
end;

{ TdxGaugeLinearScaleRange }

constructor TdxGaugeLinearScaleRange.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GetParameters.CenterPositionFactor := dxGaugeLinearRangeCenterPositionFactor;
end;

function TdxGaugeLinearScaleRange.GetParametersClass: TdxGaugeCustomRangeParametersClass;
begin
  Result := TdxGaugeLinearRangeParameters;
end;

function TdxGaugeLinearScaleRange.GetViewInfoClass: TdxGaugeCustomRangeViewInfoClass;
begin
  Result := TdxGaugeLinearScaleRangeViewInfo;
end;

procedure TdxGaugeLinearScaleRange.DefineProperties(AFiler: TFiler);
begin
  inherited DefineProperties(AFiler);
  AFiler.DefineProperty('CenterPositionFactor', ReadCenterPositionFactor, WriteCenterPositionFactor,
    IsCenterPositionFactorStored);
end;

function TdxGaugeLinearScaleRange.GetCenterPositionFactor: Single;
begin
  Result := GetParameters.CenterPositionFactor;
end;

procedure TdxGaugeLinearScaleRange.SetCenterPositionFactor(const AValue: Single);
begin
  if not SameValue(GetParameters.CenterPositionFactor, AValue) then
  begin
    GetParameters.CenterPositionFactor := Max(Min(AValue, 1), 0);
    Changed(False);
  end;
end;

function TdxGaugeLinearScaleRange.GetParameters: TdxGaugeLinearRangeParameters;
begin
  Result := Parameters as TdxGaugeLinearRangeParameters;
end;

procedure TdxGaugeLinearScaleRange.ReadCenterPositionFactor(AReader: TReader);
begin
  CenterPositionFactor := AReader.ReadDouble;
end;

procedure TdxGaugeLinearScaleRange.WriteCenterPositionFactor(AWriter: TWriter);
begin
  AWriter.WriteDouble(CenterPositionFactor);
end;

function TdxGaugeLinearScaleRange.IsCenterPositionFactorStored: Boolean;
begin
  Result := not SameValue(GetParameters.CenterPositionFactor, dxGaugeLinearRangeCenterPositionFactor);
end;

initialization
  dxGaugeRegisterScale(TdxGaugeLinearScale);
  Classes.RegisterClasses([TdxGaugeLinearScaleRange]);

finalization
  Classes.UnRegisterClasses([TdxGaugeLinearScaleRange]);
  dxGaugeUnregisterScale(TdxGaugeLinearScale);

end.
