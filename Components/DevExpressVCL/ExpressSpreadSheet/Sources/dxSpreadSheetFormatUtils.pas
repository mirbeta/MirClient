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

unit dxSpreadSheetFormatUtils;

{$I cxVer.inc}

interface

uses
  Types, Windows, Graphics, dxGDIPlusClasses, dxCore, dxCoreGraphics, dxCoreClasses,
  dxSpreadSheetCore, dxSpreadSheetPrinting, dxSpreadSheetTypes;

type

  { TdxValueUnitsHelper }

  TdxValueUnitsHelper = class
  public
    class function InchesToPixels(const AValue: Integer): Integer;
    class function InchesToPixelsF(const AValue: Double): Integer;
    class function PixelsToInches(const AValue: Integer): Integer;
    class function PixelsToInchesF(const AValue: Double): Double;
    class function PixelsToMillimeters(const AValue: Integer): Integer;
    class function PixelsToMillimetersF(const AValue: Double): Double;
    class function PixelsToPoints(const AValue: Double): Double;
    class function PointsToPixels(const AValue: Double): Integer;
    class function ValueToInches(const S: string): Integer;
    class function ValueToInchesF(const S: string): Double;
    class function ValueToPixels(const S: string): Integer;
    class function ValueToPixelsF(const S: string): Double;
  end;

  { TdxSpreadSheetColorHelper }

  TdxSpreadSheetColorHelper = class
  public
    class function ApplyTint(AColor: TColor; const ATintValue: Double): TColor;
  end;

  { TdxSpreadSheetHeaderFooterHelper }

  TdxSpreadSheetHeaderFooterHelper = class
  public
    class function Build(AText: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText): string;
    class procedure Parse(ATarget: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText; const ASource: string);
  end;

  { TdxSpreadSheetOutlineHelper }

  TdxSpreadSheetOutlineHelper = class
  public
    class procedure IncreaseOutlineLevelTo(AItems: TdxSpreadSheetTableItems; AIndex, ALevel: Integer); overload;
    class procedure IncreaseOutlineLevelTo(AItems: TdxSpreadSheetTableItems; AStartIndex, AFinishIndex, ALevel: Integer); overload;
  end;

  { TdxSpreadSheetPrintAreasHelper }

  TdxSpreadSheetPrintAreasHelper = class
  public
    class function BuildPrintTitlesReference(AView: TdxSpreadSheetTableView): string;
    class procedure ImportPrintArea(ADefinedName: TdxSpreadSheetDefinedName);
    class procedure ImportPrintTitles(ADefinedName: TdxSpreadSheetDefinedName);
  end;

procedure dxSpreadSheetInitializeBrushPattern(const ABrush: TdxGPBrush; const APattern: TdxGPImage;
  const AForegroundColor, ABackgroundColor: TdxAlphaColor; AIsTexture: Boolean = True);
procedure dxSpreadSheetLoadBrushPattern(const ABrush: TdxGPBrush; const AInstance: THandle;
  const AResName: UnicodeString; const AForegroundColor, ABackgroundColor: TdxAlphaColor);
implementation

uses
  Math, SysUtils, dxSpreadSheetUtils, cxGraphics, cxGeometry, dxTypeHelpers, dxDPIAwareUtils, dxStringHelper,
  dxSpreadSheetCoreHelpers;

const
  sdxHeaderFooterCenterSectionMacro = '&C';
  sdxHeaderFooterLeftSectionMacro = '&L';
  sdxHeaderFooterRightSectionMacro = '&R';

procedure InitializeBrushPattern(const ABrush: TdxGPBrush; const AWidth, AHeight: Integer;
  const AColors: TRGBColors; const AForegroundColor, ABackgroundColor: TRGBQuad);
var
  I: Integer;
begin
  ABrush.Style := gpbsTexture;
  for I := 0 to Length(AColors) - 1 do
  begin
    if AColors[I].rgbBlue > 0 then
      AColors[I] := ABackgroundColor
    else
      AColors[I] := AForegroundColor;
  end;
  ABrush.Texture.LoadFromBits(AWidth, AHeight, AColors, True);
end;

procedure dxSpreadSheetLoadBrushPattern(const ABrush: TdxGPBrush; const AInstance: THandle;
  const AResName: UnicodeString; const AForegroundColor, ABackgroundColor: TdxAlphaColor);
var
  ABitmap: TBitmap;
  AColors: TRGBColors;
begin
  if FindResource(AInstance, PChar(AResName), RT_BITMAP) = 0 then
  begin
    ABrush.Style := gpbsClear;
    Exit;
  end;
  ABitmap := TBitmap.Create;
  try
    ABitmap.LoadFromResourceName(HInstance, AResName);
    GetBitmapBits(ABitmap, AColors, True);
    InitializeBrushPattern(ABrush, ABitmap.Width, ABitmap.Height, AColors,
      dxAlphaColorToRGBQuad(AForegroundColor), dxAlphaColorToRGBQuad(ABackgroundColor));
  finally
    ABitmap.Free;
  end;
end;

procedure dxSpreadSheetInitializeBrushPattern(const ABrush: TdxGPBrush; const APattern: TdxGPImage;
  const AForegroundColor, ABackgroundColor: TdxAlphaColor; AIsTexture: Boolean = True);
begin
  if (APattern = nil) or (APattern.Empty) then
    Exit;
  ABrush.Style := gpbsTexture;
  if not AIsTexture then
    ABrush.Texture := APattern
  else
    InitializeBrushPattern(ABrush, APattern.Width, APattern.Height, APattern.GetBitmapBits,
      dxAlphaColorToRGBQuad(AForegroundColor), dxAlphaColorToRGBQuad(ABackgroundColor));
end;

{ TdxValueUnitsHelper }

class function TdxValueUnitsHelper.InchesToPixels(const AValue: Integer): Integer;
begin
  Result := InchesToPixelsF(AValue);
end;

class function TdxValueUnitsHelper.InchesToPixelsF(const AValue: Double): Integer;
begin
  Result := Round(AValue * dxDefaultDPI);
end;

class function TdxValueUnitsHelper.PixelsToInches(const AValue: Integer): Integer;
begin
  Result := Round(PixelsToInchesF(AValue));
end;

class function TdxValueUnitsHelper.PixelsToInchesF(const AValue: Double): Double;
begin
  Result := AValue / dxDefaultDPI
end;

class function TdxValueUnitsHelper.PixelsToMillimeters(const AValue: Integer): Integer;
begin
  Result := Round(PixelsToMillimetersF(AValue));
end;

class function TdxValueUnitsHelper.PixelsToMillimetersF(const AValue: Double): Double;
begin
  Result := AValue * 25.4 / dxDefaultDPI;
end;

class function TdxValueUnitsHelper.PixelsToPoints(const AValue: Double): Double;
begin
  Result := AValue * 0.75 * dxDefaultDPI / dxDefaultDPI;
end;

class function TdxValueUnitsHelper.PointsToPixels(const AValue: Double): Integer;
begin
  Result := Round(AValue / 0.75 * dxDefaultDPI / dxDefaultDPI);
end;

class function TdxValueUnitsHelper.ValueToInches(const S: string): Integer;
begin
  Result := PixelsToInches(ValueToPixels(S));
end;

class function TdxValueUnitsHelper.ValueToInchesF(const S: string): Double;
begin
  Result := PixelsToInchesF(ValueToPixelsF(S));
end;

class function TdxValueUnitsHelper.ValueToPixels(const S: string): Integer;
begin
  Result := Round(ValueToPixelsF(S));
end;

class function TdxValueUnitsHelper.ValueToPixelsF(const S: string): Double;
var
  ADelimPos: Integer;
  AValue: Double;
  AValueType: string;
begin
  Result := 0;
  ADelimPos := LastDelimiter('0123456789.', S);
  if ADelimPos > 0 then
  begin
    AValue := dxStrToFloat(Copy(S, 1, ADelimPos));
    AValueType := Copy(S, ADelimPos + 1, MaxInt);
    if AValueType = 'mm' then
      Result := AValue / 25.4 * dxDefaultDPI
    else

    if AValueType = 'cm' then
      Result := AValue / 2.54 * dxDefaultDPI
    else

    if AValueType = 'pt' then
      Result := AValue / 72 * dxDefaultDPI
    else

    if AValueType = 'in' then
      Result := AValue * dxDefaultDPI
  end;
end;

{ TdxSpreadSheetColorHelper }

class function TdxSpreadSheetColorHelper.ApplyTint(AColor: TColor; const ATintValue: Double): TColor;
var
  AColorHSL: TdxHSL;
begin
  Result := AColor;
  if ATintValue <> 0 then
  begin
    AColorHSL := TdxColorSpaceConverter.ColorToHSL(Result);
    if ATintValue < 0 then
      AColorHSL.L := AColorHSL.L * (1 + ATintValue)
    else
      AColorHSL.L := AColorHSL.L * (1 - ATintValue) + ATintValue;
    Result := TdxColorSpaceConverter.HSLToColor(AColorHSL);
  end;
end;

{ TdxSpreadSheetHeaderFooterHelper }

class function TdxSpreadSheetHeaderFooterHelper.Build(
  AText: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText): string;
begin
  Result := '';
  if AText.LeftSection <> '' then
    Result := Result + sdxHeaderFooterLeftSectionMacro + AText.LeftSection;
  if AText.CenterSection <> '' then
    Result := Result + sdxHeaderFooterCenterSectionMacro + AText.CenterSection;
  if AText.RightSection <> '' then
    Result := Result + sdxHeaderFooterRightSectionMacro + AText.RightSection;
end;

class procedure TdxSpreadSheetHeaderFooterHelper.Parse(
  ATarget: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText; const ASource: string);
var
  APosL, APosR, APosC, APos: Integer;
begin
  APosL := Pos(sdxHeaderFooterLeftSectionMacro, ASource);
  APosC := Pos(sdxHeaderFooterCenterSectionMacro, ASource);
  APosR := Pos(sdxHeaderFooterRightSectionMacro, ASource);

  if APosL > 0 then
  begin
    if APosC > 0 then
      APos := APosC
    else
      if APosR > 0 then
        APos := APosR
      else
        APos := Length(ASource) + 1;

    ATarget.LeftSection := Copy(ASource,
      APosL + Length(sdxHeaderFooterLeftSectionMacro),
      APos - (APosL + Length(sdxHeaderFooterLeftSectionMacro)));
  end;

  if APosC > 0 then
  begin
    if APosR > 0 then
      APos := APosR
    else
      APos := Length(ASource) + 1;

    ATarget.CenterSection := Copy(ASource,
      APosC + Length(sdxHeaderFooterCenterSectionMacro),
      APos - (APosC + Length(sdxHeaderFooterCenterSectionMacro)));
  end;

  if APosR > 0 then
    ATarget.RightSection := Copy(ASource, APosR + Length(sdxHeaderFooterRightSectionMacro), MaxInt);
end;

{ TdxSpreadSheetOutlineHelper }

class procedure TdxSpreadSheetOutlineHelper.IncreaseOutlineLevelTo(
  AItems: TdxSpreadSheetTableItems; AIndex, ALevel: Integer);
var
  AGroup: TdxSpreadSheetTableItemGroup;
begin
  if ALevel >= 0 then
  begin
    AItems.Groups.BeginUpdate;
    try
      repeat
        AGroup := AItems.Groups.Find(AIndex);
        if (AGroup = nil) or (AGroup.Level < ALevel) then
          AItems.Groups.Add(AIndex)
        else
          Break;
      until False;
    finally
      AItems.Groups.EndUpdate;
    end;
  end;
end;

class procedure TdxSpreadSheetOutlineHelper.IncreaseOutlineLevelTo(
  AItems: TdxSpreadSheetTableItems; AStartIndex, AFinishIndex, ALevel: Integer);
var
  I: Integer;
begin
  AItems.Groups.BeginUpdate;
  try
    for I := AStartIndex to AFinishIndex do
      IncreaseOutlineLevelTo(AItems, I, ALevel);
  finally
    AItems.Groups.EndUpdate;
  end;
end;

{ TdxSpreadSheetPrintAreasHelper }

class function TdxSpreadSheetPrintAreasHelper.BuildPrintTitlesReference(AView: TdxSpreadSheetTableView): string;

  function EncodePrintableArea(const R: TRect): string;
  begin
    Result := dxReferenceToString(R, AView.SpreadSheet.OptionsView.R1C1Reference, [croSheetName], AView.Caption);
  end;

var
  ABuffer: TStringBuilder;
begin
  ABuffer := TdxStringBuilderManager.Get;
  try
    if AView.OptionsPrint.Source.ColumnsToRepeat.Assigned then
      ABuffer.Append(EncodePrintableArea(AView.OptionsPrint.Source.ColumnsToRepeat.Rect));
    if ABuffer.Length > 0 then
      ABuffer.Append(',');
    if AView.OptionsPrint.Source.RowsToRepeat.Assigned then
      ABuffer.Append(EncodePrintableArea(AView.OptionsPrint.Source.RowsToRepeat.Rect));
    Result := ABuffer.ToString;
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

class procedure TdxSpreadSheetPrintAreasHelper.ImportPrintArea(ADefinedName: TdxSpreadSheetDefinedName);
var
  ARanges: TdxRectList;
  AView: TdxSpreadSheetCustomView;
begin
  try
    ARanges := TdxSpreadSheetDefinedNameHelper.GetUsedAreas(ADefinedName, AView);
    try
      if ARanges.Count > 0 then
      begin
        if AView is TdxSpreadSheetTableView then
          TdxSpreadSheetTableView(AView).OptionsPrint.Source.Area.Rect := ARanges.Union;
      end;
    finally
      ARanges.Free;
    end;
  finally
    ADefinedName.Free;
  end;
end;

class procedure TdxSpreadSheetPrintAreasHelper.ImportPrintTitles(ADefinedName: TdxSpreadSheetDefinedName);
var
  ARange: TRect;
  ARanges: TdxRectList;
  AView: TdxSpreadSheetCustomView;
  I: Integer;
begin
  try
    ARanges := TdxSpreadSheetDefinedNameHelper.GetUsedAreas(ADefinedName, AView);
    try
      if AView is TdxSpreadSheetTableView then
      begin
        for I := 0 to ARanges.Count - 1 do
        begin
          ARange := ARanges[I];
          if dxSpreadSheetIsEntireColumn(ARange) then
            TdxSpreadSheetTableView(AView).OptionsPrint.Source.ColumnsToRepeat.Rect := ARange;
          if dxSpreadSheetIsEntireRow(ARange) then
            TdxSpreadSheetTableView(AView).OptionsPrint.Source.RowsToRepeat.Rect := ARange;
        end;
      end;
    finally
      ARanges.Free;
    end;
  finally
    ADefinedName.Free;
  end;
end;

end.
