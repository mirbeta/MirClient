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

unit dxGaugeUtils;

{$I cxVer.inc}

interface

uses
  Windows, Graphics, Classes, Math, dxCore, dxCoreGraphics, cxGeometry, dxGDIPlusAPI, dxGDIPlusClasses;

const
  dxGaugeSelectionMarkerSize = 5;

type
  TdxGaugeEasingFunc = function(ANormalizedTime: Double): Double;
  TdxGaugeEasingMode = (emEaseIn, emEaseOut, emEaseInOut);
  TdxGaugeEasingFunction = (efBackEase, efElasticEase, efBounceEase, efQuadraticEase, efCubicEase, efQuinticEase,
    efSineEase, efExponentialEase, efCircleEase);

procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ARect: TdxRectF; AFont: TFont;
  AHorzAlignment: TAlignment = taCenter); overload; inline;
procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ARect: TdxRectF; AFont: TFont;
  ATextColor: TdxAlphaColor; AHorzAlignment: TAlignment = taCenter); overload; inline;
procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ARect: TdxRectF; AFont: TFont;
  ARotationAngle: Single; AHorzAlignment: TAlignment = taCenter); overload; inline;
procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ARect: TdxRectF; AFont: TFont;
  ATextColor: TdxAlphaColor; ARotationAngle: Single; AHorzAlignment: TAlignment = taCenter); overload; inline;
procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ARect: TdxRectF; ARadius, AAngle: Single;
  AFont: TFont); overload; inline;
procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ARotatePoint: TdxPointF; ARadius, AAngle: Single;
  AFont: TFont); overload; inline;
procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ARotatePoint: TdxPointF; ARadius, AAngle: Single;
  AFont: TFont; ATextColor: TdxAlphaColor); overload; inline;
procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ATextRectSize: TdxSizeF;
  const ARect: TdxRectF; ARadius, AAngle: Single; AFont: TFont); overload; inline;
procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ATextRectSize: TdxSizeF;
  const ARotatePoint: TdxPointF; ARadius, AAngle: Single; AFont: TFont); overload; inline;
procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ATextRectSize: TdxSizeF;
  const ARotatePoint: TdxPointF; ARadius, AAngle: Single; AFont: TFont; ATextColor: TdxAlphaColor); overload; inline;

procedure dxGaugeDrawBitmap(AGPGraphics: TdxGPGraphics; ABitmap: TBitmap; const ARect: TdxRectF); inline;
procedure dxGaugeDrawImage(AGPGraphics: TdxGPGraphics; AImage: TGraphic; const ARect: TdxRectF);
procedure dxGaugeDrawRectangle(AGPGraphics: TdxGPGraphics; const ARect: TRect; AHatchBrushStyle: TdxGpHatchStyle;
  ABackgroundColor, AForegroundColor: TColor; ABackgroundAlpha, AForegroundAlpha: Byte);
procedure dxGaugeDrawRectangles(AGPGraphics: TdxGPGraphics; const ARects: TRects);
procedure dxGaugeDrawSector(AGPGraphics: TdxGPGraphics; const ARect: TdxRectF; ASectorWidth: Single;
  AStartAngle, ASweepAngle: Single; ABackgroundColor, ABorderColor: TdxAlphaColor);
procedure dxGaugeDrawContentRects(AGPGraphics: TdxGPGraphics; const ARects: TRects);
procedure dxGaugeDrawSelections(AGPGraphics: TdxGPGraphics; const ARects: TRects);
procedure dxGaugeDrawMarkers(AGPGraphics: TdxGPGraphics; const ARects: TRects; AIsSizable: Boolean);

procedure dxGaugeRotateAndDrawImage(AGPGraphics: TdxGPGraphics; AImage: TGraphic; const ADestImageSize: TdxSizeF;
  const ABounds: TdxRectF; ARadius, ARotateAngle: Single); overload; inline;
procedure dxGaugeRotateAndDrawImage(AGPGraphics: TdxGPGraphics; AImage: TGraphic; const ADestImageSize: TdxSizeF;
  const ARotatePoint: TdxPointF; ARadius, ARotateAngle: Single); overload; inline;
procedure dxGaugeRotateAndDrawImage(AGPGraphics: TdxGPGraphics; AImage: TGraphic; const ADestImageSize: TdxSizeF;
  const ARotatePoint: TdxPointF; ARadius, ARotateAngle, ARotationAngleOffset: Single); overload; inline;

function dxGaugeGetArcScaleLayoutSize(const ABounds: TdxRectF; ARadius, ARadiusFactor: Single): TdxSizeF;
function dxGaugeGetRectangularScaleLayoutSize(const ABounds: TdxRectF; AWidth, AHeight: Integer;
  AWidthFactor, AHeightFactor: Single): TdxSizeF; inline;
function dxGaugeGetTextRenderingMode(AFont: TFont): TdxGpTextRenderingHint;
function dxGaugeValueRange(AMaxValue, AMinValue: Single): Single; inline;
procedure dxGaugeRaiseException(const AFormatString: string; const AArguments: array of const);
procedure dxGaugeReverseList(AList: TList); inline;

function dxGaugeGetFontHeight(AGPGraphics: TdxGPGraphics; AFont: TFont): Single;
function dxGaugeGetTextRect(const AText: string; AGpFont: GpFont): TdxRectF; overload;
function dxGaugeGetTextRect(const AText: string; AFont: TFont): TdxRectF; overload;
function dxGaugeGetSelectionMarkers(const ABorderBounds: TRect): TRects;
procedure dxGaugeCopyParameters(ASource, ADestination: TObject; AIsFirstVariantField: Boolean);

implementation

uses
  Types, SysUtils, cxGraphics, dxSmartImage, dxCompositeShape;

const
  dxSelectionAlphaChannel = 20;
  dxSelectionBackgroundColor: TColor = $582801;
  dxSelectionBorderAlphaChannel = 255;
  dxSelectionBorderColor: TColor = $BD8753;
  dxSelectionMarkerBackgroundColor: TColor = clWhite;

type
  TdxCompositeShapeAccess = class(TdxCompositeShape);

  { TdxSelectionPainter }

  TdxSelectionPainter = class
  private
    class procedure DrawSelection(AGPGraphics: TdxGPGraphics; const ARect: TRect);
    class procedure DrawSelectionBackground(AGPGraphics: TdxGPGraphics; const ARect: TRect);

    class procedure InternalDrawContentRect(AGPGraphics: TdxGPGraphics; const ARect: TRect; ABorderStyle: TPenStyle;
      AWidth: Integer; ABackgroundColor, ABorderColor: TColor; ABackgroundAlpha, ABorderAlpha: Byte);
    class procedure InternalDrawMarkers(AGPGraphics: TdxGPGraphics; const ARects: TRects; AIsSizable: Boolean);
    class procedure InternalDrawSelections(AGPGraphics: TdxGPGraphics; const ARects: TRects);
  public
    class procedure DrawContentRects(AGPGraphics: TdxGPGraphics; const ARects: TRects);
    class procedure DrawMarkers(AGPGraphics: TdxGPGraphics; const ARects: TRects; AIsSizable: Boolean);
    class procedure DrawRectangle(AGPGraphics: TdxGPGraphics; const ARect: TRect; ABorderStyle: TPenStyle;
      ABackgroundAlpha, ABorderAlpha: Byte); overload;
    class procedure DrawSelections(ACanvas: TcxCanvas; const ARect: TRect;
      const ASelectionRects: TRects; ANeedDrawMarkers: Boolean = True); overload;
    class procedure DrawSelections(AGPGraphics: TdxGPGraphics; const ARects: TRects); overload;
  end;

procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ARect: TdxRectF; AFont: TFont;
  AHorzAlignment: TAlignment = taCenter);
begin
  dxGaugeDrawText(AGPGraphics, AText, ARect, AFont, dxColorToAlphaColor(AFont.Color), AHorzAlignment);
end;

procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ARect: TdxRectF; AFont: TFont;
  ATextColor: TdxAlphaColor; AHorzAlignment: TAlignment = taCenter);
begin
  dxGPDrawText(AGPGraphics, AText, ARect, AFont, ATextColor, AHorzAlignment, taVerticalCenter, False,
    dxGaugeGetTextRenderingMode(AFont));
end;

procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ARect: TdxRectF; AFont: TFont;
  ARotationAngle: Single; AHorzAlignment: TAlignment = taCenter);
begin
  dxGaugeDrawText(AGPGraphics, AText, ARect, AFont, dxColorToAlphaColor(AFont.Color), ARotationAngle, AHorzAlignment);
end;

procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ARect: TdxRectF; AFont: TFont;
  ATextColor: TdxAlphaColor; ARotationAngle: Single; AHorzAlignment: TAlignment = taCenter);
begin
  AGPGraphics.SaveWorldTransform;
  try
    AGPGraphics.RotateWorldTransform(ARotationAngle, cxRectCenter(ARect));
    dxGaugeDrawText(AGPGraphics, AText, ARect, AFont, ATextColor, AHorzAlignment);
  finally
    AGPGraphics.RestoreWorldTransform;
  end;
end;

procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ARect: TdxRectF; ARadius, AAngle: Single;
  AFont: TFont);
begin
  dxGaugeDrawText(AGPGraphics, AText, cxRectCenter(ARect), ARadius, AAngle, AFont);
end;

procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ARotatePoint: TdxPointF; ARadius,
  AAngle: Single; AFont: TFont);
begin
  dxGaugeDrawText(AGPGraphics, AText, ARotatePoint, ARadius, AAngle, AFont, dxColorToAlphaColor(AFont.Color));
end;

procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ARotatePoint: TdxPointF; ARadius, AAngle: Single;
  AFont: TFont; ATextColor: TdxAlphaColor);
var
  ATextRect: TdxRectF;
begin
  dxGPGetTextRect(AGPGraphics, AText, AFont, False, cxRectF(cxNullRect), ATextRect);
  dxGaugeDrawText(AGPGraphics, AText, dxSizeF(ATextRect.Width, ATextRect.Height), ARotatePoint, ARadius,
    AAngle, AFont, ATextColor);
end;

procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ATextRectSize: TdxSizeF;
  const ARect: TdxRectF; ARadius, AAngle: Single; AFont: TFont);
begin
  dxGaugeDrawText(AGPGraphics, AText, ATextRectSize, cxRectCenter(ARect), ARadius, AAngle, AFont);
end;

procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ATextRectSize: TdxSizeF;
  const ARotatePoint: TdxPointF; ARadius, AAngle: Single; AFont: TFont);
begin
  dxGaugeDrawText(AGPGraphics, AText, ATextRectSize, ARotatePoint, ARadius, AAngle, AFont, dxColorToAlphaColor(AFont.Color));
end;

procedure dxGaugeDrawText(AGPGraphics: TdxGPGraphics; const AText: string; const ATextRectSize: TdxSizeF;
  const ARotatePoint: TdxPointF; ARadius, AAngle: Single; AFont: TFont; ATextColor: TdxAlphaColor);
var
  P: TdxPointF;
  ASize: TdxSizeF;
  ATextRect: TdxRectF;
begin
  ASize := dxSizeF(ATextRectSize.cx / 2, ATextRectSize.cy / 2);
  P := dxRingPoint(ARotatePoint, ARadius, DegToRad(AAngle));
  ATextRect := cxRectF(P.X - ASize.cx, P.Y - ASize.cy, P.X + ASize.cx, P.Y + ASize.cy);
  dxGaugeDrawText(AGPGraphics, AText, ATextRect, AFont, ATextColor);
end;

procedure dxGaugeDrawBitmap(AGPGraphics: TdxGPGraphics; ABitmap: TBitmap; const ARect: TdxRectF);
var
  AImage: TdxGPImage;
begin
  AImage := TdxGPImage.CreateFromBitmap(ABitmap);
  AGPGraphics.Draw(AImage, cxRectOffset(ARect, dxPointF(-1, -1)));
  AImage.Free;
end;

procedure dxGaugeDrawGraphic(AGPGraphics: TdxGPGraphics; AGraphic: TGraphic; const ARect: TdxRectF);
var
  ATempBitmap: TcxBitmap32;
begin
  ATempBitmap := TcxBitmap32.CreateSize(AGraphic.Width, AGraphic.Height, True);
  ATempBitmap.Canvas.Draw(0, 0, AGraphic);
  dxGaugeDrawBitmap(AGPGraphics, ATempBitmap, ARect);
  ATempBitmap.Free;
end;

procedure dxGaugeDrawImage(AGPGraphics: TdxGPGraphics; AImage: TGraphic; const ARect: TdxRectF);
begin
  if AImage <> nil then
    if AImage is TdxCompositeShape then
      TdxCompositeShapeAccess(AImage).DrawF(AGPGraphics, ARect)
    else
      dxGaugeDrawGraphic(AGPGraphics, AImage, ARect);
end;

procedure dxGaugeDrawRectangle(AGPGraphics: TdxGPGraphics; const ARect: TRect; AHatchBrushStyle: TdxGpHatchStyle;
  ABackgroundColor, AForegroundColor: TColor; ABackgroundAlpha, AForegroundAlpha: Byte);
begin
  TdxSelectionPainter.DrawRectangle(AGPGraphics, ARect, psDash, ABackgroundAlpha, AForegroundAlpha);
end;

procedure dxGaugeDrawRectangles(AGPGraphics: TdxGPGraphics; const ARects: TRects);
var
  I: Integer;
begin
  for I := Low(ARects) to High(ARects) do
    dxGaugeDrawRectangle(AGPGraphics, ARects[I], HatchStyleForwardDiagonal,
      dxSelectionBackgroundColor, dxSelectionBorderColor, dxSelectionAlphaChannel, dxSelectionBorderAlphaChannel);
end;

procedure dxGaugeDrawSector(AGPGraphics: TdxGPGraphics; const ARect: TdxRectF; ASectorWidth: Single;
  AStartAngle, ASweepAngle: Single; ABackgroundColor, ABorderColor: TdxAlphaColor);

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
  ABrush: TdxGPBrush;
  APath: TdxGPPath;
  APen: TdxGPPen;
begin
  if ASectorWidth > 0 then
  begin
    APen := TdxGPPen.Create;
    ABrush := TdxGPBrush.Create;
    APath := TdxGPPath.Create(gpfmWinding);
    try
      APen.Brush.Color := ABorderColor;
      ABrush.Color := ABackgroundColor;
      InitPath(APath);
      AGPGraphics.Path(APath, APen, ABrush);
    finally
      APath.Free;
      ABrush.Free;
      APen.Free;
    end;
  end;
end;

procedure dxGaugeDrawContentRects(AGPGraphics: TdxGPGraphics; const ARects: TRects);
begin
  TdxSelectionPainter.DrawContentRects(AGPGraphics, ARects);
end;

procedure dxGaugeDrawSelections(AGPGraphics: TdxGPGraphics; const ARects: TRects);
begin
  TdxSelectionPainter.DrawSelections(AGPGraphics, ARects);
end;

procedure dxGaugeDrawMarkers(AGPGraphics: TdxGPGraphics; const ARects: TRects; AIsSizable: Boolean);
begin
  TdxSelectionPainter.DrawMarkers(AGPGraphics, ARects, AIsSizable);
end;

procedure dxGaugeInternalRotateAndDrawImage(AGPGraphics: TdxGPGraphics; AImage: TGraphic; const ADestImageSize: TdxSizeF;
  const ARotatePoint: TdxPointF; ARadius, ARotateAngle, ARotateAngleOffset: Single); inline;
var
  P: TdxPointF;
begin
  ARotateAngle := -ARotateAngle;
  P := dxRingPoint(ARotatePoint, ARadius, DegToRad(-ARotateAngle));
  AGPGraphics.SaveWorldTransform;
  try
    AGPGraphics.RotateWorldTransform(ARotateAngle + ARotateAngleOffset, P);
    dxGaugeDrawImage(AGPGraphics, AImage,
      cxRectF(P.X, P.Y - ADestImageSize.cy / 2, P.X + ADestImageSize.cx, P.Y + ADestImageSize.cy / 2));
  finally
    AGPGraphics.RestoreWorldTransform;
  end;
end;

procedure dxGaugeRotateAndDrawImage(AGPGraphics: TdxGPGraphics; AImage: TGraphic; const ADestImageSize: TdxSizeF;
  const ABounds: TdxRectF; ARadius, ARotateAngle: Single);
begin
  dxGaugeInternalRotateAndDrawImage(AGPGraphics, AImage, ADestImageSize, cxRectCenter(ABounds), ARadius, ARotateAngle, 0.0);
end;

procedure dxGaugeRotateAndDrawImage(AGPGraphics: TdxGPGraphics; AImage: TGraphic; const ADestImageSize: TdxSizeF;
  const ARotatePoint: TdxPointF; ARadius, ARotateAngle: Single);
begin
  dxGaugeInternalRotateAndDrawImage(AGPGraphics, AImage, ADestImageSize, ARotatePoint, ARadius, ARotateAngle, 0.0);
end;

procedure dxGaugeRotateAndDrawImage(AGPGraphics: TdxGPGraphics; AImage: TGraphic; const ADestImageSize: TdxSizeF;
  const ARotatePoint: TdxPointF; ARadius, ARotateAngle, ARotationAngleOffset: Single);
begin
  dxGaugeInternalRotateAndDrawImage(AGPGraphics, AImage, ADestImageSize, ARotatePoint, ARadius, ARotateAngle,
    ARotationAngleOffset);
end;

function dxGaugeGetArcScaleLayoutSize(const ABounds: TdxRectF; ARadius, ARadiusFactor: Single): TdxSizeF;
begin
  if ARadius <> 0 then
  begin
    Result.cx := ARadius * 2;
    Result.cy := Result.cx;
  end
  else
  begin
    Result.cx := ARadiusFactor * ABounds.Width * 2;
    Result.cy := ARadiusFactor * ABounds.Height * 2;
  end;
end;

function dxGaugeGetTextRenderingMode(AFont: TFont): TdxGpTextRenderingHint;
{$IFDEF DELPHIXE}
const
  RenderingModeMap: array[TFontQuality] of TdxGpTextRenderingHint = (TextRenderingHintAntiAlias,
    TextRenderingHintAntiAlias, TextRenderingHintAntiAlias, TextRenderingHintSingleBitPerPixel,
    TextRenderingHintAntiAlias, TextRenderingHintClearTypeGridFit, TextRenderingHintClearTypeGridFit);
{$ENDIF}
begin
{$IFDEF DELPHIXE}
  Result := RenderingModeMap[AFont.Quality];
{$ELSE}
  Result := TextRenderingHintAntiAlias;
{$ENDIF}
end;

function dxGaugeGetRectangularScaleLayoutSize(const ABounds: TdxRectF; AWidth, AHeight: Integer;
  AWidthFactor, AHeightFactor: Single): TdxSizeF;
begin
  if AWidth <> 0 then
    Result.cx := AWidth
  else
    Result.cx := AWidthFactor * ABounds.Width;
  if AHeight <> 0 then
    Result.cy := AHeight
  else
    Result.cy := AHeightFactor * ABounds.Height;
end;

function dxGaugeValueRange(AMaxValue, AMinValue: Single): Single;
begin
  Result := Abs(AMaxValue - AMinValue);
end;

procedure dxGaugeRaiseException(const AFormatString: string; const AArguments: array of const);
begin
  raise EdxException.Create(Format(AFormatString, AArguments));
end;

procedure dxGaugeReverseList(AList: TList);
var
  ATemp: Pointer;
  AStartIndex, AEndIndex: Integer;
begin
  AStartIndex := 0;
  AEndIndex := AList.Count - 1;
  while AStartIndex < AEndIndex do
  begin
    ATemp := AList[AStartIndex];
    AList[AStartIndex] := AList[AEndIndex];
    AList[AEndIndex] := ATemp;
    Inc(AStartIndex);
    Dec(AEndIndex);
  end;
end;

function dxGaugeGetFontHeight(AGPGraphics: TdxGPGraphics; AFont: TFont): Single;
var
  AGpFont: GpFont;
begin
  AGpFont := dxGpCreateFont(AFont);
  try
    GdipCheck(GdipGetFontHeight(AGpFont, AGPGraphics.Handle, Result));
  finally
    GdipDeleteFont(AGpFont);
  end;
end;

function dxGaugeGetTextRect(const AText: string; AGpFont: GpFont): TdxRectF; overload;
var
  AGPGraphics: TdxGPGraphics;
begin
  AGPGraphics := TdxGPGraphics.Create(cxScreenCanvas.Handle);
  try
    dxGPGetTextRect(AGPGraphics, AText, AGpFont, False, cxRectF(cxNullRect), Result);
  finally
    cxScreenCanvas.Dormant;
    AGPGraphics.Free;
  end;
end;

function dxGaugeGetTextRect(const AText: string; AFont: TFont): TdxRectF; overload;
var
  AGpFont: GpFont;
begin
  AGpFont := dxGpCreateFont(AFont);
  try
    Result := dxGaugeGetTextRect(AText, AGpFont);
  finally
    GdipDeleteFont(AGpFont);
  end;
end;

function dxGaugeGetSelectionMarkers(const ABorderBounds: TRect): TRects;

  function dxGetSelectionMarker(const P: TPoint; AMarkerWidth: Integer): TRect;
  begin
    Result := cxRectInflate(cxRect(P, P), (AMarkerWidth - 1) div 2, (AMarkerWidth - 1) div 2);
    Inc(Result.Bottom);
    Inc(Result.Right);
  end;

  function dxGetSelectionMarkers(const ABorderBounds: TRect; AMarkerWidth: Integer): TRects;
  var
    AMiddleX, AMiddleY: Integer;
  begin
    SetLength(Result, 9);
    AMiddleX := (ABorderBounds.Left + ABorderBounds.Right - 1) div 2;
    AMiddleY := (ABorderBounds.Top + ABorderBounds.Bottom - 1) div 2;
    Result[0] := dxGetSelectionMarker(ABorderBounds.TopLeft, AMarkerWidth);
    Result[1] := dxGetSelectionMarker(Point(AMiddleX, ABorderBounds.Top), AMarkerWidth);
    Result[2] := dxGetSelectionMarker(Point(ABorderBounds.Right - 1, ABorderBounds.Top), AMarkerWidth);
    Result[3] := dxGetSelectionMarker(Point(ABorderBounds.Right - 1, AMiddleY), AMarkerWidth);
    Result[4] := dxGetSelectionMarker(Point(ABorderBounds.Right - 1, ABorderBounds.Bottom - 1), AMarkerWidth);
    Result[5] := dxGetSelectionMarker(Point(AMiddleX, ABorderBounds.Bottom - 1), AMarkerWidth);
    Result[6] := dxGetSelectionMarker(Point(ABorderBounds.Left, ABorderBounds.Bottom - 1), AMarkerWidth);
    Result[7] := dxGetSelectionMarker(Point(ABorderBounds.Left, AMiddleY), AMarkerWidth);

    Result[8] := dxGetSelectionMarker(Point(AMiddleX, AMiddleY), AMarkerWidth);
  end;

begin
  Result := dxGetSelectionMarkers(cxRectInflate(ABorderBounds, Rect(0, 0, 1, 1)), dxGaugeSelectionMarkerSize);
end;

procedure dxGaugeCopyParameters(ASource, ADestination: TObject; AIsFirstVariantField: Boolean);
begin
  if AIsFirstVariantField then
  begin
    cxCopyData(ASource, ADestination, SizeOf(ASource) + SizeOf(Variant), SizeOf(ADestination) + SizeOf(Variant),
      ASource.InstanceSize -  SizeOf(ASource) - SizeOf(Variant))
  end
  else
    cxCopyData(ASource, ADestination, SizeOf(ASource), SizeOf(ADestination), ASource.InstanceSize -  SizeOf(ASource));
end;

{ TdxSelectionPainter }

class procedure TdxSelectionPainter.DrawContentRects(AGPGraphics: TdxGPGraphics; const ARects: TRects);
var
  I: Integer;
begin
  for I := Low(ARects) to High(ARects) do
    InternalDrawContentRect(AGPGraphics, ARects[I], psSolid, 1, dxSelectionBackgroundColor, dxSelectionBorderColor, 10, 40);
end;

class procedure TdxSelectionPainter.DrawMarkers(AGPGraphics: TdxGPGraphics; const ARects: TRects; AIsSizable: Boolean);
begin
  InternalDrawMarkers(AGPGraphics, ARects, AIsSizable);
end;

class procedure TdxSelectionPainter.DrawRectangle(AGPGraphics: TdxGPGraphics; const ARect: TRect;
  ABorderStyle: TPenStyle; ABackgroundAlpha, ABorderAlpha: Byte);
var
  R: TRect;
begin
  R := ARect;
  Inc(R.Right);
  Inc(R.Bottom);
  AGPGraphics.Rectangle(R, dxSelectionBorderColor, dxSelectionBackgroundColor, 1, ABorderStyle, ABorderAlpha,
    ABackgroundAlpha);
end;

class procedure TdxSelectionPainter.DrawSelections(ACanvas: TcxCanvas; const ARect: TRect; const ASelectionRects: TRects;
  ANeedDrawMarkers: Boolean = True);
var
  AGPGraphics: TdxGPGraphics;
begin
  AGPGraphics := dxGpBeginPaint(ACanvas.Handle, ARect);
  try
    DrawSelections(AGPGraphics, ASelectionRects);
  finally
    dxGpEndPaint(AGPGraphics);
  end;
end;

class procedure TdxSelectionPainter.DrawSelections(AGPGraphics: TdxGPGraphics; const ARects: TRects);
begin
  InternalDrawSelections(AGPGraphics, ARects);
end;

class procedure TdxSelectionPainter.DrawSelection(AGPGraphics: TdxGPGraphics; const ARect: TRect);
begin
  DrawSelectionBackground(AGPGraphics, ARect);
end;

class procedure TdxSelectionPainter.DrawSelectionBackground(AGPGraphics: TdxGPGraphics; const ARect: TRect);
begin
  DrawRectangle(AGPGraphics, ARect, psSolid, dxSelectionAlphaChannel, dxSelectionBorderAlphaChannel);
end;

class procedure TdxSelectionPainter.InternalDrawContentRect(AGPGraphics: TdxGPGraphics; const ARect: TRect;
  ABorderStyle: TPenStyle; AWidth: Integer; ABackgroundColor, ABorderColor: TColor; ABackgroundAlpha, ABorderAlpha: Byte);
var
  R: TRect;
begin
  R := ARect;
  Inc(R.Right);
  Inc(R.Bottom);
  AGPGraphics.Rectangle(ARect, ABorderColor, ABackgroundColor, AWidth, psSolid, ABorderAlpha, ABackgroundAlpha);
end;

class procedure TdxSelectionPainter.InternalDrawMarkers(AGPGraphics: TdxGPGraphics; const ARects: TRects;
  AIsSizable: Boolean);
var
  I: Integer;
  ABackgroundColor: TColor;
begin
  if AIsSizable then
    ABackgroundColor := dxSelectionBorderColor
  else
    ABackgroundColor := clWhite;
  for I := Low(ARects) to High(ARects) do
    if I < High(ARects) then
      AGPGraphics.Rectangle(ARects[I], dxSelectionBorderColor, ABackgroundColor, 1, psSolid, 255, 255)
    else
      AGPGraphics.Ellipse(ARects[I], dxSelectionBorderColor, clWhite, 1, psSolid, 255, 255)
end;

class procedure TdxSelectionPainter.InternalDrawSelections(AGPGraphics: TdxGPGraphics; const ARects: TRects);
var
  I: Integer;
begin
  for I := Low(ARects) to High(ARects) do
    DrawSelection(AGPGraphics, ARects[I]);
end;

end.

