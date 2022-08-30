{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetGraphics;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Graphics, cxGraphics, cxGeometry, cxLookAndFeelPainters, dxCore, dxGDIPlusClasses, dxCoreGraphics;

type
  TdxSpreadSheetDrawingStage = (dsFirst, dsSecond);
  TdxSpreadSheetDrawingStages = set of TdxSpreadSheetDrawingStage;

  TdxSpreadSheetCellBorderStyle = (sscbsDefault, sscbsHair, sscbsDotted, sscbsDashDotDot, sscbsDashDot, sscbsDashed,
    sscbsThin, sscbsMediumDashDotDot, sscbsSlantedDashDot, sscbsMediumDashDot, sscbsMediumDashed, sscbsMedium,
    sscbsThick, sscbsDouble, sscbsNone);

  TdxSpreadSheetCellBordersStyles = array[TcxBorder] of TdxSpreadSheetCellBorderStyle;

  TdxSpreadSheetCellFillStyle = (sscfsSolid, sscfsGray75, sscfsGray50, sscfsGray25, sscfsGray12, sscfsGray6,
    sscfsHorzStrip, sscfsVertStrip, sscfsRevDiagonalStrip, sscfsDiagonalStrip, sscfsDiagCrossHatch,
    sscfsThickCrossHatch, sscfsThinHorzStrip, sscfsThinVertStrip, sscfsThinRevDiagonalStrip,
    sscfsThinDiagonalStrip, sscfsThinDiagCrossHatch, sscfsThinThickCrossHatch);

  TdxSpreadSheetDataAlignHorz = (ssahGeneral, ssahLeft, ssahCenter, ssahRight, ssahFill, ssahJustify, ssahDistributed);
  TdxSpreadSheetDataAlignVert = (ssavTop, ssavCenter, ssavBottom, ssavJustify, ssavDistributed);

  TdxSpreadSheetSelectionElement = (ssseFrame, ssseCorners, ssseBackground);
  TdxSpreadSheetSelectionElements = set of TdxSpreadSheetSelectionElement;

  { TdxSpreadSheetSelectionHelper }

  TdxSpreadSheetSelectionHelper = class
  public
    class procedure Draw(const ACanvas: TcxCanvas; ABounds: TRect; const AColor, ABackgroundColor: TColor;
      const AElements: TdxSpreadSheetSelectionElements; const ACorners: TdxCorners);
    class function GetCornerBounds(const R: TRect; ACorner: TdxCorner): TRect;
    class function IsInFrame(const R: TRect; const P: TPoint): Boolean;
  end;

const
  dxSpreadSheetMaxBorderSize = 3;
  dxSpreadSheetSelectionThickness = 3;
  dxSpreadSheetBorderStyleWeights: array[TdxSpreadSheetCellBorderStyle] of Byte = (
    0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1
  );
  dxSpreadSheetBorderStyleThickness: array[TdxSpreadSheetCellBorderStyle] of Byte = (
    1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 0
  );
  dxSpreadSheetSelectionColors: array [0..6] of TColor = (
    $ED8C5F, $605EEB, $C2618D, $39962D, $914CBF, $2282E3, $9E7F37
  );

var
  dxSpreadSheetBordersBrushes: array[TdxSpreadSheetCellBorderStyle, Boolean] of HBRUSH;
  dxSpreadSheetFillBrushes: array[TdxSpreadSheetCellFillStyle] of HBRUSH;

function dxSpreadSheetGetColorDefault(AColor, ADefaultColor: TColor): TColor; inline;
function dxSpreadSheetIsColorDefault(AColor: TColor): Boolean; inline;
function dxSpreadSheetPrepareCanvas(ACanvas: TcxCanvas; AFont: TFont; AFontDPI: Integer = 0): Integer;
procedure dxSpreadSheetUnprepareCanvas(ACanvas: TcxCanvas; APrevFontDPI: Integer);

procedure dxSpreadSheetDrawBackground(ACanvas: TcxCanvas; const R: TRect;
  ABackgroundColor: TColor; AForegroundColor: TColor; AFillStyle: TdxSpreadSheetCellFillStyle);
procedure dxSpreadSheetDrawBorder(ACanvas: TcxCanvas; R: TRect; AColor, ABackgroundColor: TColor;
  AStyle: TdxSpreadSheetCellBorderStyle; AIsHorizontalBorder: Boolean);
procedure dxSpreadSheetDrawBorders(ACanvas: TcxCanvas; R: TRect;
  AColor, ABackgroundColor: TColor; AStyle: TdxSpreadSheetCellBorderStyle);

function dxSpreadSheetGetBorderBounds(const ABounds: TRect; ASide: TcxBorder; ABordersStyles: TdxSpreadSheetCellBordersStyles): TRect;
procedure dxSpreadSheetSetupGpCanvas(ACanvas: TdxGpCanvas; AUseAntialiasing: Boolean);

procedure dxSpreadSheetResetZoomFactor(ACanvas: TcxCanvas; var ARect: TRect; var APrevTransform: TXForm);
procedure dxSpreadSheetRestoreZoomFactor(ACanvas: TcxCanvas; const APrevTransform: TXForm); inline;
implementation

uses
  Math, dxGDIPlusAPI, SysUtils;

{$R dxSpreadSheetCore.res}

function dxSpreadSheetIsColorDefault(AColor: TColor): Boolean;
begin
  Result := not cxColorIsValid(AColor);
end;

function dxSpreadSheetPrepareCanvas(ACanvas: TcxCanvas; AFont: TFont; AFontDPI: Integer = 0): Integer;
begin
  Result := ACanvas.Font.PixelsPerInch;
  ACanvas.Font.PixelsPerInch := IfThen(AFontDPI > 0, AFontDPI, AFont.PixelsPerInch);
  ACanvas.Font := AFont;
end;

procedure dxSpreadSheetUnprepareCanvas(ACanvas: TcxCanvas; APrevFontDPI: Integer);
begin
  ACanvas.Font.PixelsPerInch := APrevFontDPI;
  if ACanvas = cxScreenCanvas then
    cxScreenCanvas.Dormant;
end;

function dxSpreadSheetGetColorDefault(AColor, ADefaultColor: TColor): TColor;
begin
  if dxSpreadSheetIsColorDefault(AColor) then
    AColor := ADefaultColor;
  Result := ColorToRGB(AColor);
end;

procedure dxSpreadSheetResetZoomFactor(ACanvas: TcxCanvas; var ARect: TRect; var APrevTransform: TXForm);
var
  ATransform: TXForm;
begin
  GetWorldTransform(ACanvas.Handle, APrevTransform);
  if APrevTransform.eM11 <> 1 then
  begin
    ATransform := APrevTransform;
    ATransform.eM11 := 1;
    ATransform.eM22 := 1;
    SetWorldTransform(ACanvas.Handle, ATransform);

    ARect := cxRectScale(ARect, Round(APrevTransform.eM11 * 100), 100);
    if cxRectHeight(ARect) <= 0 then
      ARect := cxRectSetHeight(ARect, 1);
    if cxRectWidth(ARect) <= 0 then
      ARect := cxRectSetWidth(ARect, 1);
  end;
end;

procedure dxSpreadSheetRestoreZoomFactor(ACanvas: TcxCanvas; const APrevTransform: TXForm); inline;
begin
  if APrevTransform.eM11 <> 1 then
    SetWorldTransform(ACanvas.Handle, APrevTransform);
end;

procedure dxSpreadSheetDrawBackground(ACanvas: TcxCanvas; const R: TRect;
  ABackgroundColor: TColor; AForegroundColor: TColor; AFillStyle: TdxSpreadSheetCellFillStyle);
var
  ABkColor: Cardinal;
  AFgColor: Cardinal;
  AOrigin: TPoint;
  APrevOrigin: TPoint;
begin
  if AFillStyle = sscfsSolid then
  begin
    FillRect(ACanvas.Handle, R, TdxSolidBrushCache.Get(ABackgroundColor));
    Exit;
  end;

  ABkColor := SetBkColor(ACanvas.Handle, ColorToRGB(ABackgroundColor));
  AFgColor := SetTextColor(ACanvas.Handle, ColorToRGB(AForegroundColor));

  if ACanvas is TcxControlCanvas then
    AOrigin := TcxControlCanvas(ACanvas).Origin
  else
    AOrigin := cxNullPoint;

  SetBrushOrgEx(ACanvas.Handle, -AOrigin.X, -AOrigin.Y, @APrevOrigin);
  FillRect(ACanvas.Handle, R, dxSpreadSheetFillBrushes[AFillStyle]);
  SetBrushOrgEx(ACanvas.Handle, APrevOrigin.X, APrevOrigin.Y, nil);

  SetBkColor(ACanvas.Handle, ABkColor);
  SetTextColor(ACanvas.Handle, AFgColor);
end;

procedure dxSpreadSheetDrawBorder(ACanvas: TcxCanvas; R: TRect; AColor, ABackgroundColor: TColor;
  AStyle: TdxSpreadSheetCellBorderStyle; AIsHorizontalBorder: Boolean);
var
  ABkColor: Integer;
  AFgColor: Integer;
  AOrigin: TPoint;
  APrevOrigin: TPoint;
begin
  if AStyle = sscbsNone then
    Exit;

  if AStyle in [sscbsDefault, sscbsThin, sscbsMedium, sscbsThick] then
  begin
    FillRect(ACanvas.Handle, R, TdxSolidBrushCache.Get(AColor));
    Exit;
  end;

  if ACanvas is TcxControlCanvas then
    AOrigin := TcxControlCanvas(ACanvas).Origin
  else
    AOrigin := cxNullPoint;

  SetBrushOrgEx(ACanvas.Handle, -AOrigin.X, -AOrigin.Y, @APrevOrigin);
  try
    ABkColor := SetBkColor(ACanvas.Handle, ColorToRGB(AColor));
    AFgColor := SetTextColor(ACanvas.Handle, ColorToRGB(ABackgroundColor));
    if AStyle <> sscbsDouble then
      FillRect(ACanvas.Handle, R, dxSpreadSheetBordersBrushes[AStyle, AIsHorizontalBorder])
    else
    begin
      if AIsHorizontalBorder then
      begin
        FillRect(ACanvas.Handle, cxRectSetHeight(R, 1), dxSpreadSheetFillBrushes[sscfsSolid]);
        FillRect(ACanvas.Handle, cxRectSetBottom(R, R.Bottom, 1), dxSpreadSheetFillBrushes[sscfsSolid]);
      end
      else
      begin
        FillRect(ACanvas.Handle, cxRectSetWidth(R, 1), dxSpreadSheetFillBrushes[sscfsSolid]);
        FillRect(ACanvas.Handle, cxRectSetRight(R, R.Right, 1), dxSpreadSheetFillBrushes[sscfsSolid]);
      end;
      InflateRect(R, -1, -1);
      SetBkColor(ACanvas.Handle, ColorToRGB(ABackgroundColor));
      FillRect(ACanvas.Handle, R, dxSpreadSheetFillBrushes[sscfsSolid]);
      ExcludeClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    end;
    SetBkColor(ACanvas.Handle, ABkColor);
    SetTextColor(ACanvas.Handle, AFgColor);
  finally
    SetBrushOrgEx(ACanvas.Handle, APrevOrigin.X, APrevOrigin.Y, nil);
  end;
end;

procedure dxSpreadSheetDrawBorders(ACanvas: TcxCanvas; R: TRect;
  AColor, ABackgroundColor: TColor; AStyle: TdxSpreadSheetCellBorderStyle);
var
  ABordersStyles: TdxSpreadSheetCellBordersStyles;
  ASide: TcxBorder;
begin
  for ASide := Low(ASide) to High(ASide) do
    ABordersStyles[ASide] := AStyle;
  for ASide := Low(ASide) to High(ASide) do
    dxSpreadSheetDrawBorder(ACanvas,
      dxSpreadSheetGetBorderBounds(R, ASide, ABordersStyles),
      AColor, ABackgroundColor, AStyle, ASide in [bTop, bBottom]);
end;

function dxSpreadSheetGetBorderBounds(const ABounds: TRect;
  ASide: TcxBorder; ABordersStyles: TdxSpreadSheetCellBordersStyles): TRect;
var
  ASize: Integer;
begin
  ASize := dxSpreadSheetBorderStyleThickness[ABordersStyles[ASide]];
  case ASide of
    bLeft:
      Result := cxRectSetRight(ABounds, ABounds.Left + 1 + ASize div 3, ASize);
    bTop:
      Result := cxRectSetBottom(ABounds, ABounds.Top + 1 + ASize div 3, ASize);
    bRight:
      Result := cxRectSetLeft(ABounds, ABounds.Right + 1 - ASize + ASize div 3, ASize);
    bBottom:
      Result := cxRectSetTop(ABounds, ABounds.Bottom + 1 - ASize + ASize div 3, ASize);
  end;

  if ASide in [bLeft, bRight] then
  begin
    if ASize >= 2 then
    begin
      if ABordersStyles[bTop] <> sscbsNone then
        Dec(Result.Top, (dxSpreadSheetBorderStyleThickness[ABordersStyles[bTop]] - ASize + 2) div 2);
      if ABordersStyles[bBottom] <> sscbsNone then
        Inc(Result.Bottom, Max(1, dxSpreadSheetBorderStyleThickness[ABordersStyles[bBottom]] - 1));
    end
    else
      Inc(Result.Bottom);
  end
  else
  begin
    if ASize >= 2 then
    begin
      if ABordersStyles[bLeft] <> sscbsNone then
        Dec(Result.Left, (dxSpreadSheetBorderStyleThickness[ABordersStyles[bLeft]] - ASize + 2) div 2);
      if ABordersStyles[bRight] <> sscbsNone then
        Inc(Result.Right, Max(1, dxSpreadSheetBorderStyleThickness[ABordersStyles[bRight]] - 1));
    end
    else
      Inc(Result.Right);
  end;
end;

procedure dxSpreadSheetSetupGpCanvas(ACanvas: TdxGpCanvas; AUseAntialiasing: Boolean);
const
  InterpolationModeMap: array[Boolean] of TdxGPInterpolationMode = (imLowQuality, imHighQuality);
  SmoothingModeMap: array[Boolean] of TdxGPSmoothingMode = (smHighSpeed, smAntiAlias);
  TextRenderingHintMap: array[Boolean] of TdxGpTextRenderingHint = (
    TextRenderingHintSingleBitPerPixelGridFit, TextRenderingHintAntiAliasGridFit
  );
begin
  ACanvas.InterpolationMode := InterpolationModeMap[AUseAntialiasing];
  ACanvas.SmoothingMode := SmoothingModeMap[AUseAntialiasing];
  ACanvas.TextRenderingHint := TextRenderingHintMap[AUseAntialiasing];
end;

procedure InitializeAssistants;
const
  BorderStyleResourceNames: array[TdxSpreadSheetCellBorderStyle] of string =
   ('SOLID', 'HAIR', 'DOTTED', 'DASHDOTDOT', 'DASHDOT', 'DASHED', 'SOLID', 'DASHDOTDOT',
    'SLANTED', 'DASHDOT', 'MEDIUMDASHED', 'SOLID', 'SOLID', 'DOUBLE', 'SOLID');
  FillStyleResourceNames: array[TdxSpreadSheetCellFillStyle] of string =
   ('SOLID', 'GRAY75', 'GRAY50', 'GRAY25', 'GRAY12', 'GRAY6', 'HORZSTRIP', 'VERTSTRIP', 'REVDIAGONALSTRIP',
    'DIAGONALSTRIP', 'DIAGCROSSHATCH', 'THICKCROSSHATCH', 'THINHORZSTRIP', 'THINVERTSTRIP', 'THINREVDIAGONALSTRIP',
    'THINDIAGONALSTRIP', 'THINTHICKCROSSHATCH', 'THINDIAGCROSSHATCH');
var
  ABitmap: HBitmap;
  ABorderStyle: TdxSpreadSheetCellBorderStyle;
  AFillStyle: TdxSpreadSheetCellFillStyle;
  ARotateBitmap: TcxBitmap;
begin
  for AFillStyle := Low(TdxSpreadSheetCellFillStyle) to High(TdxSpreadSheetCellFillStyle) do
  begin
    ABitmap := LoadBitmap(HInstance, PChar(FillStyleResourceNames[AFillStyle]));
    dxSpreadSheetFillBrushes[AFillStyle] := CreatePatternBrush(ABitmap);
    DeleteObject(ABitmap);
  end;

  ARotateBitmap := TcxBitmap.Create;
  try
    for ABorderStyle := Low(TdxSpreadSheetCellBorderStyle) to High(TdxSpreadSheetCellBorderStyle) do
    begin
      ABitmap := LoadBitmap(HInstance, PChar(BorderStyleResourceNames[ABorderStyle]));
      dxSpreadSheetBordersBrushes[ABorderStyle, True] := CreatePatternBrush(ABitmap);
      ARotateBitmap.Handle := ABitmap;
      ARotateBitmap.Rotate(raPlus90);
      dxSpreadSheetBordersBrushes[ABorderStyle, False] := CreatePatternBrush(ARotateBitmap.Handle);
      DeleteObject(ABitmap);
    end;
  finally
    ARotateBitmap.Free;
  end;
end;

procedure FinalizeAssistants;
var
  ABorderStyle: TdxSpreadSheetCellBorderStyle;
  AFillStyle: TdxSpreadSheetCellFillStyle;
  I: Boolean;
begin
  for AFillStyle := Low(TdxSpreadSheetCellFillStyle) to High(TdxSpreadSheetCellFillStyle) do
    DeleteObject(dxSpreadSheetFillBrushes[AFillStyle]);
  for ABorderStyle := Low(TdxSpreadSheetCellBorderStyle) to High(TdxSpreadSheetCellBorderStyle) do
    for I := False to True do
      DeleteObject(dxSpreadSheetBordersBrushes[ABorderStyle, I]);
end;

{ TdxSpreadSheetSelectionHelper }

class procedure TdxSpreadSheetSelectionHelper.Draw(const ACanvas: TcxCanvas; ABounds: TRect;
  const AColor, ABackgroundColor: TColor; const AElements: TdxSpreadSheetSelectionElements; const ACorners: TdxCorners);
var
  ACorner: TdxCorner;
  APrevForm: TXForm;
  ARect: TRect;
  ARegion: TcxRegion;
begin
  dxSetZoomFactor(ACanvas, 100, APrevForm);
  try
    ABounds := cxRectInflate(ABounds, 1, 1, -1, -1);
    ABounds := cxRectScale(ABounds, Round(APrevForm.eM11 * 100), 100);
    ABounds := cxRectInflate(ABounds, 1, 1, 2, 2);

    if ssseCorners in AElements then
    begin
      for ACorner := Low(ACorner) to High(ACorner) do
        if ACorner in ACorners then
        begin
          ARect := GetCornerBounds(ABounds, ACorner);
          ACanvas.FrameRect(cxRectInflate(ARect, 1), ABackgroundColor, 1);
          ACanvas.FillRect(ARect, AColor);
          ACanvas.ExcludeClipRect(cxRectInflate(ARect, 1));
        end;
    end;

    if ssseBackground in AElements then
      dxGpFillRect(ACanvas.Handle, cxRectInflate(ABounds, -3), AColor, 10);

    if ssseFrame in AElements then
    begin
      ACanvas.FrameRect(cxRectInflate(ABounds, -2), ABackgroundColor, 1);
      ACanvas.FrameRect(ABounds, AColor, 2);
      ARegion := TcxRegion.Create(cxRectInflate(ABounds, 1));
      ARegion.Combine(cxRectInflate(ABounds, -3), roSubtract);
      ACanvas.SetClipRegion(ARegion, roSubtract);
    end;
  finally
    SetWorldTransform(ACanvas.Handle, APrevForm);
  end;
end;

class function TdxSpreadSheetSelectionHelper.GetCornerBounds(const R: TRect; ACorner: TdxCorner): TRect;
begin
  case ACorner of
    coTopLeft:
      Result := cxRect(R.Left - 1, R.Top - 1, R.Left + 4, R.Top + 4);
    coTopRight:
      Result := cxRect(R.Right - 4, R.Top - 1, R.Right + 1, R.Top + 4);
    coBottomLeft:
      Result := cxRect(R.Left - 1, R.Bottom - 4, R.Left + 4, R.Bottom + 1);
  else
    Result := cxRect(R.Right - 4, R.Bottom - 4, R.Right + 1, R.Bottom + 1);
  end;
end;

class function TdxSpreadSheetSelectionHelper.IsInFrame(const R: TRect; const P: TPoint): Boolean;
begin
  Result := not cxRectIsEmpty(R) and cxRectPtIn(R, P) and not cxRectPtIn(cxRectInflate(R, -3), P);
end;

initialization
  InitializeAssistants;

finalization
  FinalizeAssistants;
end.
