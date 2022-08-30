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

unit dxSpreadSheetFormatODSHelpers;

{$I cxVer.Inc}

interface

uses
  Windows, Graphics, cxGraphics, dxXMLDoc,
  dxSpreadSheetTypes, dxSpreadSheetGraphics, dxSpreadSheetFormatODSTags, dxSpreadSheetCore, dxSpreadSheetContainers,
  Classes;

type
  TdxSpreadSheetODSBrushPattern = (sobpCell, sobpDiagonal, sobpSingle0,
    sobpSingle45, sobpSingle45Wide, sobpSingle90, sobpSingle135, sobpTriple);
  TdxSpreadSheetODSFillMode = (sofmSolid, sofmGradient, sofmTexture, sofmClear, sofmHatch);
  TdxSpreadSheetODSValueType = (sovtUnknown, sovtBoolean,
    sovtDate, sovtTime, sovtCurrency, sovtFloat, sovtString, sovtPercentage);

  { EdxSpreadSheetODSUnexpectedToken }

  EdxSpreadSheetODSUnexpectedToken = class(EdxXMLUnexpectedToken);

  { TdxSpreadSheetODSTimeDuration }

  TdxSpreadSheetODSTimeDuration = packed record
    Years: Integer;
    Months: Integer;
    Days: Integer;
    Hours: Integer;
    Minutes: Integer;
    Seconds: Integer;
    Milliseconds: Integer;

    procedure Clear;
    procedure Parse(const S: string);
  end;

  { TdxSpreadSheetODSHelper }

  TdxSpreadSheetODSHelper = class
  public
    class function DetectShapeType(S: AnsiString): TdxSpreadSheetShapeType;
    class function IsBoldFontStyle(const S: AnsiString): Boolean;
    class procedure ParseTransform(S: string; out AAngle: Double; out AOffset: TPoint);

    class function StringToAlignHorz(const S: AnsiString): TdxSpreadSheetDataAlignHorz;
    class function StringToAlignment(const S: AnsiString): TAlignment;
    class function StringToAlignVert(const S: AnsiString): TdxSpreadSheetDataAlignVert;
    class function StringToBorderStyle(const S1, S2: AnsiString): TdxSpreadSheetCellBorderStyle; overload;
    class function StringToBorderStyle(const S1, S2: string): TdxSpreadSheetCellBorderStyle; overload;
    class function StringToColor(S: string): TColor;
    class function StringToDate(const S: AnsiString): TDateTime;
    class function StringToFillStyle(const S: AnsiString): TdxSpreadSheetODSFillMode;
    class function StringToFontPitch(const S: AnsiString): TFontPitch;
    class function StringToFontSize(const S: string): Integer;
    class function StringToPercents(const S: string): Integer;
    class function StringToSize(const S: string): Integer;
    class function StringToTime(const S: AnsiString): TDateTime;
    class function StringToValueType(const S: AnsiString): TdxSpreadSheetODSValueType;
    class function StringToVerticalAlignment(const S: AnsiString): TVerticalAlignment;
  end;

var
  sdxODSAttrFOBorders: array [TcxBorder] of AnsiString = (
    sdxODSAttrFOBorderLeft, sdxODSAttrFOBorderTop, sdxODSAttrFOBorderRight, sdxODSAttrFOBorderBottom
  );

  sdxODSAlignHorzStyles: array[TdxSpreadSheetDataAlignHorz] of AnsiString = (
    '', sdxODSValueTextAlignStart, sdxODSValueTextAlignCenter, sdxODSValueTextAlignEnd, '', sdxODSValueTextAlignJustify, ''
  );

  sdxODSAlignVertStyles: array[TVerticalAlignment] of AnsiString = (
    sdxODSValueVerticalAlignTop, sdxODSValueVerticalAlignBottom, sdxODSValueVerticalAlignMiddle
  );

  sdxODSBrushPatterns: array[TdxSpreadSheetODSBrushPattern] of UnicodeString = (
    'ODS_BRUSHPATTERN_CELL', 'ODS_BRUSHPATTERN_DIAG', 'ODS_BRUSHPATTERN_SINGLE_0', 'ODS_BRUSHPATTERN_SINGLE_45',
      'ODS_BRUSHPATTERN_SINGLE_45_WIDE', 'ODS_BRUSHPATTERN_SINGLE_90', 'ODS_BRUSHPATTERN_SINGLE_135', 'ODS_BRUSHPATTERN_TRIPLE'
  );

  sdxODSBorderStyles: array[TdxSpreadSheetCellBorderStyle] of AnsiString = (
    '', '', sdxODSValueBorderStyleDotted, sdxODSValueBorderStyleDotted, sdxODSValueBorderStyleDotted,
    sdxODSValueBorderStyleDashed, sdxODSValueBorderStyleSolid, sdxODSValueBorderStyleDotted, '',
    sdxODSValueBorderStyleDotted, sdxODSValueBorderStyleDashed, sdxODSValueBorderStyleSolid,
    sdxODSValueBorderStyleSolid, sdxODSValueBorderStyleDouble, sdxODSValueBorderStyleNone
  );

  sdxODSFillModes: array [TdxSpreadSheetODSFillMode] of AnsiString = (
    sdxODSValueFillSolid, sdxODSValueFillGradient, sdxODSValueFillBitmap, sdxODSValueFillNone, sdxODSValueFillHatch
  );

  sdxODSFontPitch: array[TFontPitch] of AnsiString = (
    '', sdxODSValueFontPitchVariable, sdxODSValueFontPitchFixed
  );

  sdxODSValueType: array[TdxSpreadSheetODSValueType] of AnsiString = (
    '', sdxODSValueValueTypeBoolean, sdxODSValueValueTypeDate, sdxODSValueValueTypeTime,
    sdxODSValueValueTypeCurrency, sdxODSValueValueTypeFloat, sdxODSValueValueTypeString,
    sdxODSValueValueTypePercentage
  );

implementation

uses
  AnsiStrings, SysUtils, Math, dxColorPicker, dxCoreGraphics, dxCore, StrUtils, dxSpreadSheetFormatUtils;

{ TdxSpreadSheetODSTimeDuration }

procedure TdxSpreadSheetODSTimeDuration.Clear;
begin
  ZeroMemory(@Self, SizeOf(Self));
end;

procedure TdxSpreadSheetODSTimeDuration.Parse(const S: string);
var
  ACode: Char;
  AIndex: Integer;
  AIsTimePart: Boolean;
  ALength: Integer;
  AValue: string;
  AValueAsFloat: Double;
  AValueStartIndex: Integer;
begin
  Clear;
  AIndex := Pos(AnsiString('P'), S);
  if AIndex > 0 then
  begin
    ALength := Length(S);
    AIsTimePart := False;
    AValueStartIndex := 1;
    while AIndex <= ALength do
    begin
      ACode := S[AIndex];
      if not CharInSet(ACode, ['0'..'9', '.']) then
      begin
        AValue := Copy(S, AValueStartIndex, AIndex - AValueStartIndex);
        case ACode of
          'Y':
            Years := StrToIntDef(AValue, 0);
          'D':
            Days := StrToIntDef(AValue, 0);
          'T':
            AIsTimePart := True;
          'H':
            Hours := StrToIntDef(AValue, 0);
          'M':
            if AIsTimePart then
              Minutes := StrToIntDef(AValue, 0)
            else
              Months := StrToIntDef(AValue, 0);
          'S':
            begin
              AValueAsFloat := dxStrToFloatDef(AValue);
              Seconds := Trunc(AValueAsFloat);
              Milliseconds := Trunc((AValueAsFloat - Seconds) * 1000);
            end;
        end;
        AValueStartIndex := AIndex + 1;
      end;
      Inc(AIndex);
    end;
  end;
end;

{ TdxSpreadSheetODSHelper }

class function TdxSpreadSheetODSHelper.DetectShapeType(S: AnsiString): TdxSpreadSheetShapeType;
type
  TPair = record
    Name: AnsiString;
    ShapeType: TdxSpreadSheetShapeType;
  end;

const
  Map: array [0..3] of TPair = (
    (Name: 'rounded'; ShapeType: stRoundRect),
    (Name: 'ellipse'; ShapeType: stEllipse),
    (Name: 'oval'; ShapeType: stEllipse),
    (Name: 'rectangle'; ShapeType: stRect)
  );
var
  I: Integer;
begin
  S := LowerCase(S);
  for I := Low(Map) to High(Map) do
  begin
    if Pos(Map[I].Name, S) > 0 then
      Exit(Map[I].ShapeType);
  end;
  Result := stRect;
end;

class function TdxSpreadSheetODSHelper.IsBoldFontStyle(const S: AnsiString): Boolean;
begin
  Result := (S = sdxODSValueFontWeightBold) or (S = sdxODSValueFontWeightBolder) or (StrToIntDef(dxAnsiStringToString(S), 0) >= 700);
end;

class procedure TdxSpreadSheetODSHelper.ParseTransform(S: string; out AAngle: Double; out AOffset: TPoint);
const
  sRotate = 'rotate (';
  sTranslate = 'translate (';
var
  I, J: Integer;
begin
  I := Pos(sRotate, S) + Length(sRotate);
  J := PosEx(')', S, I);
  AAngle := -180 * dxStrToFloatDef(Copy(S, I, J - I)) / Pi;

  I := Pos(sTranslate, S) + Length(sTranslate);
  J := PosEx(' ', S, I);
  AOffset.X := StringToSize(Copy(S, I, J - I));
  I := J + 1;
  J := PosEx(')', S, I);
  AOffset.Y := StringToSize(Copy(S, I, J - I));
end;

class function TdxSpreadSheetODSHelper.StringToAlignHorz(const S: AnsiString): TdxSpreadSheetDataAlignHorz;
var
  H: TdxSpreadSheetDataAlignHorz;
begin
  Result := ssahGeneral;
  for H := Low(TdxSpreadSheetDataAlignHorz) to High(TdxSpreadSheetDataAlignHorz) do
  begin
    if sdxODSAlignHorzStyles[H] = S then
      Exit(H);
  end;
end;

class function TdxSpreadSheetODSHelper.StringToAlignment(const S: AnsiString): TAlignment;
begin
  if S = sdxODSValueTextAlignCenter then
    Result := taCenter
  else
    if S = sdxODSValueTextAlignRight then
      Result := taRightJustify
    else
      Result := taLeftJustify;
end;

class function TdxSpreadSheetODSHelper.StringToAlignVert(const S: AnsiString): TdxSpreadSheetDataAlignVert;
const
  Map: array[TVerticalAlignment] of TdxSpreadSheetDataAlignVert = (ssavTop, ssavBottom, ssavCenter);
var
  V: TVerticalAlignment;
begin
  Result := ssavBottom;
  for V := Low(TVerticalAlignment) to High(TVerticalAlignment) do
  begin
    if sdxODSAlignVertStyles[V] = S then
      Exit(Map[V]);
  end;
end;

class function TdxSpreadSheetODSHelper.StringToBorderStyle(const S1, S2: AnsiString): TdxSpreadSheetCellBorderStyle;

  function GetStyle(const S: AnsiString): TdxSpreadSheetCellBorderStyle;
  var
    I: TdxSpreadSheetCellBorderStyle;
  begin
    for I := Low(TdxSpreadSheetCellBorderStyle) to High(TdxSpreadSheetCellBorderStyle) do
    begin
      if sdxODSBorderStyles[I] = S then
        Exit(I);
    end;
    Result := sscbsDefault;
  end;

begin
  if S2 = '' then
    Exit(GetStyle(S1));

  if S1 = sdxODSValueBorderSizeThick then
    Result := sscbsThick
  else
    if S1 <> sdxODSValueBorderSizeMedium then
      Result := GetStyle(S2)
    else
      case GetStyle(S2) of
        sscbsDotted, sscbsDashDotDot:
          Result := sscbsMediumDashDotDot;
        sscbsDashDot:
          Result := sscbsMediumDashDot;
        sscbsDashed:
          Result := sscbsMediumDashed;
      else
        Result := sscbsMedium;
      end;
end;

class function TdxSpreadSheetODSHelper.StringToBorderStyle(const S1, S2: string): TdxSpreadSheetCellBorderStyle;
begin
  Result := StringToBorderStyle(AnsiString(S1), AnsiString(S2));
end;

class function TdxSpreadSheetODSHelper.StringToColor(S: string): TColor;
begin
  if S = '' then
    Result := clDefault
  else
    if S = sdxODSValueColorTransparent then
      Result := clNone
    else
    begin
      if S[1] = '#' then
        Delete(S, 1, 1);
      Result := dxAlphaColorToColor(TdxColorHelper.HexCodeToAlphaColor(S, False));
    end;
end;

class function TdxSpreadSheetODSHelper.StringToDate(const S: AnsiString): TDateTime;
var
  ADateTime: TdxXMLDateTime;
begin
  ADateTime.Parse(dxAnsiStringToString(S));
  Result := ADateTime.ToDateTime;
end;

class function TdxSpreadSheetODSHelper.StringToFillStyle(const S: AnsiString): TdxSpreadSheetODSFillMode;
var
  AIndex: TdxSpreadSheetODSFillMode;
begin
  for AIndex := Low(TdxSpreadSheetODSFillMode) to High(TdxSpreadSheetODSFillMode) do
  begin
    if sdxODSFillModes[AIndex] = S then
      Exit(AIndex);
  end;
  Result := sofmSolid;
end;

class function TdxSpreadSheetODSHelper.StringToFontPitch(const S: AnsiString): TFontPitch;
begin
  for Result := Low(TFontPitch) to High(TFontPitch) do
  begin
    if sdxODSFontPitch[Result] = S then
      Exit;
  end;
  Result := fpDefault;
end;

class function TdxSpreadSheetODSHelper.StringToFontSize(const S: string): Integer;
var
  E: Integer;
begin
  Val(S, Result, E);
  if Result = 0 then
    Result := 8;
end;

class function TdxSpreadSheetODSHelper.StringToPercents(const S: string): Integer;
var
  E: Integer;
begin
  Val(S, Result, E);
end;

class function TdxSpreadSheetODSHelper.StringToSize(const S: string): Integer;
begin
  Result := TdxValueUnitsHelper.ValueToPixels(S);
end;

class function TdxSpreadSheetODSHelper.StringToTime(const S: AnsiString): TDateTime;
const
  MillisecondsToDateTime = 1 / MSecsPerDay;
  SecondsToDateTime = MSecsPerSec * MillisecondsToDateTime;
  MinutesToDateTime = SecsPerMin * SecondsToDateTime;
  HoursToDateTime = MinsPerHour * MinutesToDateTime;
var
  ADuration: TdxSpreadSheetODSTimeDuration;
begin
  ADuration.Parse(dxAnsiStringToString(S));
  Result := ADuration.Hours * HoursToDateTime + ADuration.Minutes * MinutesToDateTime +
    ADuration.Seconds * SecondsToDateTime + ADuration.Milliseconds * MillisecondsToDateTime;
end;

class function TdxSpreadSheetODSHelper.StringToValueType(const S: AnsiString): TdxSpreadSheetODSValueType;
begin
  for Result := Low(TdxSpreadSheetODSValueType) to High(TdxSpreadSheetODSValueType) do
  begin
    if sdxODSValueType[Result] = S then
      Exit;
  end;
  Result := sovtUnknown;
end;

class function TdxSpreadSheetODSHelper.StringToVerticalAlignment(const S: AnsiString): TVerticalAlignment;
var
  V: TVerticalAlignment;
begin
  Result := taAlignTop;
  for V := Low(TVerticalAlignment) to High(TVerticalAlignment) do
  begin
    if sdxODSAlignVertStyles[V] = S then
      Exit(V);
  end;
end;

end.
