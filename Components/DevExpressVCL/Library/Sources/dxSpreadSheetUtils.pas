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

unit dxSpreadSheetUtils;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, SysUtils, Variants, Math, Graphics, Generics.Defaults, Generics.Collections, cxGraphics, dxCore, cxGeometry,
  cxDateUtils, cxClasses, cxVariants, dxSpreadSheetTypes, dxSpreadSheetCoreStrs, dxTypeHelpers, dxSpreadSheetClasses;

const
  ValueIncr: array[Boolean] of Integer = (-1, 1);

type
  TdxSpreadSheetCellReferenceOption = (croAbsoluteColumn, croAbsoluteRow, croSheetName);
  TdxSpreadSheetCellReferenceOptions = set of TdxSpreadSheetCellReferenceOption;

  { TdxSpreadSheetColumnHelper }

  TdxSpreadSheetColumnHelper = class
  public
    class function IndexByName(AName: string; AIsRCReferenceStyle: Boolean = False): Integer; inline;
    class function NameByIndex(AIndex: Integer; AIsRCReferenceStyle: Boolean = False): string; inline;
  end;

  { TdxSpreadSheetExcelColumnWidthHelper }

  TdxSpreadSheetExcelColumnWidthHelper = class
  strict private
    FFont: TFont;
    FMaxDigitWidth: Integer;
    FTwoSpaceWidth: Integer;

    procedure FontChangeHandler(Sender: TObject);
    function GetMaxDigitWidth: Integer; inline;
    function GetTwoSpaceWidth: Integer; inline;
    procedure SetFont(const Value: TFont);
  protected
    procedure CalculateMaxDigitWidth;
    procedure CalculateTwoSpaceWidth;
  public
    constructor Create;
    destructor Destroy; override;
    function CharsNumberToPixels(const ACharsNumber: Double): Integer;
    function CharsNumberToWidth(const ACharsNumber: Double): Double;
    function PixelsToCharsNumber(const APixels: Integer): Integer;
    function PixelsToSpacesNumber(const APixels: Integer): Integer;
    function PixelsToWidth(const APixels: Integer): Double;
    function SpacesNumberToPixels(const ASpaceNumber: Integer): Integer;
    function WidthToCharsNumber(const AWidth: Double): Double;
    function WidthToPixels(const AWidth: Double): Integer;
    //
    property Font: TFont read FFont write SetFont;
    property MaxDigitWidth: Integer read GetMaxDigitWidth;
    property TwoSpaceWidth: Integer read GetTwoSpaceWidth;
  end;


function dxIsCurrency(const AValue: Double): Boolean; inline;
function dxIsDateTime(const AValue: Variant): Boolean; inline;
function dxIsLogical(const AValue: Variant): Boolean; inline;
function dxIsNumberOrDateTime(const AValue: Variant): Boolean; inline;
function dxIsNumericOrDateTime(const AValue: Variant): Boolean; inline;
function dxIsText(const AValue: Variant): Boolean; inline;
function dxTryStrToBool(const AText: string; var AValue: Boolean): Boolean; inline;
function dxTryStrToDateTime(const AText: string; var AValue: TDateTime; const AFormatSettings: TFormatSettings): Boolean;
function dxTryStrToOrdinal(const AText: string; var AValue: Variant; const AFormatSettings: TdxSpreadSheetFormatSettings): Boolean;

function dxConvertToXLSDate(const AValue: Variant; out ADate: TDateTime): Boolean; inline;
function dxConvertXLSDateToNumeric(const AValue: Variant; out AResult: Double): Boolean; inline;

procedure dxStringToReference(const S: string; out AColumnIndex, ARowIndex: Integer); overload;
function dxStringToReferenceArea(const S: string): TRect; overload;

function dxReferenceToString(const AArea: TRect; R1C1Reference: Boolean = False;
  AOptions: TdxSpreadSheetCellReferenceOptions = []; const ASheetName: string = ''): string; overload;
function dxReferenceToString(const ARow, AColumn: Integer; R1C1Reference: Boolean = False;
  AOptions: TdxSpreadSheetCellReferenceOptions = []; const ASheetName: string = ''): string; overload; inline;
function dxReferenceToString(const AArea: TRect; R1C1Reference: Boolean;
  const ARowOrigin, AColumnOrigin: Integer): string; overload;
function dxReferenceToString(const ARowOrigin, AColumnOrigin: Integer;
  const ARow, AColumn: TdxSpreadSheetReference): string; overload;
procedure dxReferenceToString(ABuffer: TStringBuilder;
  const ARowOrigin, AColumnOrigin: Integer; const ARow, AColumn: TdxSpreadSheetReference); overload;
procedure dxReferenceToString(ABuffer: TStringBuilder;
  const AArea: TRect; R1C1Reference: Boolean; const ARowOrigin, AColumnOrigin: Integer); overload;

function dxR1C1ReferenceAreaToString(
  const ARowOrigin, AColumnOrigin: Integer;
  const ASheet: string; const ARow, AColumn: TdxSpreadSheetReference;
  const ASheet2: string; const ARow2, AColumn2: TdxSpreadSheetReference): string; overload; inline;
procedure dxR1C1ReferenceAreaToString(
  const ABuffer: TStringBuilder; const ARowOrigin, AColumnOrigin: Integer;
  const ASheet: string; const ARow, AColumn: TdxSpreadSheetReference;
  const ASheet2: string; const ARow2, AColumn2: TdxSpreadSheetReference); overload; inline;
function dxR1C1ReferenceToString(const ARowOrigin, AColumnOrigin: Integer;
  const ARow, AColumn: TdxSpreadSheetReference): string; overload; inline;
procedure dxR1C1ReferenceToString(ABuffer: TStringBuilder;
  const ARowOrigin, AColumnOrigin: Integer; const ARow, AColumn: TdxSpreadSheetReference); overload; inline;
function dxR1C1ReferencePartToStr(const AValue: TStringBuilder;
  const APrefix: string; AOrigin: Integer; const AReference: TdxSpreadSheetReference): Boolean; inline;
function dxR1C1ReferenceAreaPartToString(
  const AValue: TStringBuilder; const AOrigin: Integer;
  const ARef1, ARef2, ARef3: TdxSpreadSheetReference;
  const ASheet, ASheet2, APrefix: string): Boolean; inline;

function dxSpreadSheetCompareText(const S1, S2: Pointer; L1, L2: Integer): Integer; overload; inline;
function dxSpreadSheetCompareText(const S1, S2: string): Integer; overload; inline;
function dxSpreadSheetCompareText(const S1: string; const S2: Pointer; L2: Integer = -1): Integer; overload; inline;
function dxSpreadSheetIsFormula(const S: string): Boolean;
function dxSpreadSheetLowerCase(const S: string): string; inline;
function dxSpreadSheetUpperCase(const S: string): string; inline;
function dxSpreadSheetVarCompare(const V1, V2: Variant; ACaseSensitive: Boolean = True): Integer; inline;

function dxSpreadSheetCharIsEqual(const AChar1, AChar2: Char): Boolean; inline;
function dxSpreadSheetTextIsEqual(const AText1, AText2: string): Boolean; inline;

function dxIntPower(const Base, Exp: Integer): Integer;

function dxEMUToPixels(const Value: Int64): Integer; overload;
function dxEMUToPixels(const Value: TRect): TRect; overload;
function dxEMUToPixelsF(const Value: Int64): Single;
function dxPixelsToEMU(const Value: Integer): Int64;
function dxPixelsToEMUF(const Value: Single): Int64;

function dxDateTimeToRealDateTime(const AValue: TDateTime; ADataTimeSystem: TdxSpreadSheetDateTimeSystem): TDateTime;
function dxRealDateTimeToDateTime(const AValue: TDateTime; ADataTimeSystem: TdxSpreadSheetDateTimeSystem): TDateTime;

function dxSpreadSheetArea(const ATopLeft: TPoint; const ASize: TSize): TRect; inline;
function dxSpreadSheetAreaHeight(const AArea: TRect): Integer; inline;
function dxSpreadSheetAreaSize(const AArea: TRect): TSize; inline;
function dxSpreadSheetAreaWidth(const AArea: TRect): Integer; inline;
function dxSpreadSheetCellsUnion(const AArea1, AArea2: TRect): TRect; inline;
function dxSpreadSheetContains(const AArea, ATestArea: TRect): Boolean; overload; inline;
function dxSpreadSheetContains(const AArea: TRect; ARow, AColumn: Integer): Boolean; overload; inline;
function dxSpreadSheetContainsColumn(const AArea: TRect; AColumn: Integer): Boolean; inline;
function dxSpreadSheetContainsRow(const AArea: TRect; ARow: Integer): Boolean; inline;
function dxSpreadSheetEntireSheetArea: TRect; inline;
function dxSpreadSheetGetRealArea(const AArea: TRect): TRect;
function dxSpreadSheetIntersects(const AArea1, AArea2: TRect): Boolean; overload; inline;
function dxSpreadSheetIntersects(const AArea1, AArea2: TRect; var AArea3: TRect): Boolean; overload; inline;
function dxSpreadSheetIsEntireColumn(const AArea: TRect): Boolean; inline;
function dxSpreadSheetIsEntireRow(const AArea: TRect): Boolean; inline;
function dxSpreadSheetIsEntireRowOrColumn(const AArea: TRect): Boolean; inline;
function dxSpreadSheetIsErrorString(const S: string; var ACode: TdxSpreadSheetFormulaErrorCode): Boolean; inline;
function dxSpreadSheetIsNullValue(const AValue: Variant): Boolean; inline;
function dxSpreadSheetIsSingleCellArea(const R: TRect): Boolean; inline;
function dxSpreadSheetIsValidArea(const AArea: TRect): Boolean; inline;
function dxSpreadSheetIsValidCellReference(const ARowIndex, AColumnIndex: Integer): Boolean; inline;
function dxSpreadSheetOffsetArea(const AArea: TRect; X, Y: Integer): TRect; inline;

procedure dxSpreadSheetValidate(var AValue: Integer; const AMinValue, AMaxValue: Integer); inline;

function dxSpreadSheetErrorCodeToString(ACode: TdxSpreadSheetFormulaErrorCode): string;

procedure dxSpreadSheetExtractConditionParams(const S: string; const AFormatSettings: TdxSpreadSheetFormatSettings;
  var AConditionValue: Variant; var AOperation: TdxSpreadSheetFormulaOperation);

implementation

uses
  StrUtils, dxDPIAwareUtils, dxSpreadSheetGraphics, dxCoreClasses, dxStringHelper;

const
  dxMaxReferenceLength = 16;
  dxExcelColumnWidthPadding = 2 * 2 {Edge margins} + 1 {GridLine width};

function dxSpreadSheetTryStrToCurrency(const AText: string; out AFloatValue: Double;
  const AFormatSettings: TdxSpreadSheetFormatSettings): Boolean;

  procedure RemoveThousandSeparators(var st: string);
  var
    ch: Char;
  begin
    ch := AFormatSettings.Data.ThousandSeparator;
    if ch <> '' then
      while Pos(ch, st) > 0 do
        Delete(st, Pos(ch, st), 1);
  end;

var
  ACurrency, st: string;
  P: Integer;
  AIsNegative: Boolean;
begin
  Result := False;
  ACurrency := AFormatSettings.CurrencyFormat;
  if Pos(ACurrency, AText) > 0 then
  begin
    st := Trim(AText);
    AIsNegative := ((st[1] = '(') and (st[Length(st)] = ')')) or (Pos('-', st) > 0);
    if AIsNegative then
    begin
      P := Pos('-', st);
      if P > 0 then
        Delete(st, P, 1);
      if (st[1] = '(') and (st[Length(st)] = ')') then
        st := Copy(st, 2, Length(st) - 2)
    end;
    P := Pos(ACurrency, st);
    Delete(st, P, Length(ACurrency));
    st := Trim(st);
    RemoveThousandSeparators(st);
    Result := TryStrToFloat(st, AFloatValue, AFormatSettings.Data) or TryStrToFloat(st, AFloatValue);
    if Result and AIsNegative then
      AFloatValue := -AFloatValue;
  end;
end;

function dxIsCurrency(const AValue: Double): Boolean;
begin
  Result := Ceil(100000000 * Abs(AValue)) mod 10000 = 0;
end;

function dxIsDateTime(const AValue: Variant): Boolean;
begin
  Result := VarIsType(AValue, varDate);
end;

function dxIsLogical(const AValue: Variant): Boolean;
begin
  Result := VarIsType(AValue, varBoolean);
end;

function dxIsNumberOrDateTime(const AValue: Variant): Boolean;
begin
 Result := dxIsNumericOrDateTime(AValue) and not dxIsLogical(AValue);
end;

function dxIsNumericOrDateTime(const AValue: Variant): Boolean;
begin
  Result := VarIsNumeric(AValue) or dxIsDateTime(AValue);
end;

function dxIsText(const AValue: Variant): Boolean;
begin
  Result := VarIsStr(AValue);
end;

function dxTryStrToBool(const AText: string; var AValue: Boolean): Boolean;
begin
  Result := True;
  if dxSpreadSheetTextIsEqual(AText, cxGetResourceString(@sdxTrue)) then
    AValue := True
  else
    if dxSpreadSheetTextIsEqual(AText, cxGetResourceString(@sdxFalse)) then
      AValue := False
    else
      Result := False;
end;

function dxTryStrToDateTime(const AText: string; var AValue: TDateTime; const AFormatSettings: TFormatSettings): Boolean;

  function InternalTryStrToDateTime(const ASettings: TFormatSettings): Boolean;
  begin
    Result := TryStrToDateTime(AText, AValue, ASettings) and (AValue >= 0);
    if Result then
    try
      AValue := cxGetLocalCalendar.ToDateTime(cxStrToDate(AText, ASettings));
    except
      Result := TryStrToTime(AText, AValue, ASettings);
    end
  end;

var
  ASettings: TFormatSettings;
begin
  ASettings := AFormatSettings;
  Result := InternalTryStrToDateTime(ASettings);
  if not Result then
  case AFormatSettings.DateSeparator of
    '-': begin
           ASettings.DateSeparator := '/';
           Result := InternalTryStrToDateTime(ASettings);
         end;
    '/': begin
           ASettings.DateSeparator := '-';
           Result := InternalTryStrToDateTime(ASettings);
         end;
    '.': begin
           ASettings.DateSeparator := '/';
           Result := InternalTryStrToDateTime(ASettings);
           if not Result then
           begin
             ASettings.DateSeparator := '-';
             Result := InternalTryStrToDateTime(ASettings);
           end;
         end;
  end;
end;

function dxTryStrToOrdinal(const AText: string; var AValue: Variant; const AFormatSettings: TdxSpreadSheetFormatSettings): Boolean;
var
  ABoolValue: Boolean;
  ADateTimeValue: TDateTime;
  AFloatValue: Double;
begin
  Result := (Pos('=', AText) <= 0) and (Pos('"', AText) <= 0);
  if not Result then
    Exit;

 {$IFDEF CPUX64}
  if StartsText('e', AText) then // Bug in VCL - http://qc.embarcadero.com/wc/qcmain.aspx?d=110729
    Exit(False);
 {$ENDIF}

  if AText = '' then
    AValue := Null
  else

  if dxSpreadSheetTryStrToCurrency(AText, AFloatValue, AFormatSettings) then
    AValue := FloatToCurr(AFloatValue)
  else

  if TryStrToFloat(AText, AFloatValue, AFormatSettings.Data) or TryStrToFloat(AText, AFloatValue) then
  begin
    if IsZero(Frac(AFloatValue)) then
      AValue := Int(AFloatValue)
    else
      AValue := AFloatValue;
  end
  else

  if dxTryStrToBool(AText, ABoolValue) then
    AValue := ABoolValue
  else

  if dxTryStrToDateTime(AText, ADateTimeValue, AFormatSettings.Data) then
    AValue := dxRealDateTimeToDateTime(ADateTimeValue, AFormatSettings.DateTimeSystem)
  else
    Result := False;
end;


function dxConvertToXLSDate(const AValue: Variant; out ADate: TDateTime): Boolean; inline;
begin
  Result := False;
  if VarIsStr(AValue) then
  begin
    Result := TextToDateEx(AValue, ADate);
    Result := Result and (ADate > 0);
  end;
end;

function dxConvertXLSDateToNumeric(const AValue: Variant; out AResult: Double): Boolean; inline;
begin
  Result := dxIsDateTime(AValue);
  if Result then
    AResult := Double(AValue);
end;

function dxEMUToPixelsFactor: Double; inline;
begin
  Result := dxDefaultDPI / 914400;
end;

function dxEMUToPixels(const Value: Int64): Integer;
begin
  Result := Round(Value * dxEMUToPixelsFactor);
end;

function dxEMUToPixels(const Value: TRect): TRect;
begin
  Result.Bottom := dxEMUToPixels(Value.Bottom);
  Result.Left := dxEMUToPixels(Value.Left);
  Result.Right := dxEMUToPixels(Value.Right);
  Result.Top := dxEMUToPixels(Value.Top);
end;

function dxEMUToPixelsF(const Value: Int64): Single;
begin
  Result := Value * dxEMUToPixelsFactor;
end;

function dxPixelsToEMU(const Value: Integer): Int64;
begin
  Result := Round(Value / dxEMUToPixelsFactor);
end;

function dxPixelsToEMUF(const Value: Single): Int64;
begin
  Result := Round(Value / dxEMUToPixelsFactor);
end;

function dxDateTimeToRealDateTime(const AValue: TDateTime; ADataTimeSystem: TdxSpreadSheetDateTimeSystem): TDateTime;
begin
  if ADataTimeSystem = dts1904 then
    Result := AValue + EncodeDate(1904, 1, 1)
  else
    Result := AValue;
end;

function dxRealDateTimeToDateTime(const AValue: TDateTime; ADataTimeSystem: TdxSpreadSheetDateTimeSystem): TDateTime;
begin
  if ADataTimeSystem = dts1904 then
    Result := AValue - EncodeDate(1904, 1, 1)
  else
    Result := AValue;
end;

function dxIntPower(const Base, Exp: Integer): Integer;
var
  I: Integer;
begin
  Result := 1;
  for I := 1 to Exp do
    Result := Result * Base;
end;

procedure dxStringToReference(const S: string; out AColumnIndex, ARowIndex: Integer);

  function FindFirstNumber(const S: string): Integer;
  var
    I: Integer;
  begin
    for I := 1 to Length(S) do
    begin
      if dxCharInSet(Char(S[I]), ['0'..'9']) then
        Exit(I);
    end;
    Result := 0;
  end;

var
  APos: Integer;
begin
  APos := FindFirstNumber(S);
  if APos > 0 then
  begin
    if APos > 1 then
      AColumnIndex := TdxSpreadSheetColumnHelper.IndexByName(Copy(S, 1, APos - 1))
    else
      AColumnIndex := MaxInt;

    ARowIndex := StrToIntDef(Copy(S, APos, MaxInt), 0) - 1;
  end
  else
  begin
    AColumnIndex := TdxSpreadSheetColumnHelper.IndexByName(S);
    ARowIndex := MaxInt;
  end;
end;

function dxStringToReferenceArea(const S: string): TRect;
var
  APos: Integer;
begin
  APos := Pos(string(':'), S);
  if APos > 0 then
  begin
    dxStringToReference(Copy(S, 1, APos - 1), Result.Left, Result.Top);
    dxStringToReference(Copy(S, APos + 1, MaxInt), Result.Right, Result.Bottom);
    if Result.Left = MaxInt then
      Result.Left := 0;
    if Result.Top = MaxInt then
      Result.Top := 0;
    if Result.Right < 0 then
      Result.Right := MaxInt;
  end
  else
  begin
    dxStringToReference(S, Result.Left, Result.Top);
    Result.BottomRight := Result.TopLeft;
  end;
end;

function dxReferenceToString(const AArea: TRect; R1C1Reference: Boolean = False;
  AOptions: TdxSpreadSheetCellReferenceOptions = []; const ASheetName: string = ''): string;
var
  ABuffer: TStringBuilder;
begin
  ABuffer := TdxStringBuilderManager.Get(dxMaxReferenceLength);
  try
    if (croSheetName in AOptions) and (ASheetName <> '') then
    begin
      ABuffer.Append(dxStringMarkChar2);
      ABuffer.Append(ASheetName);
      ABuffer.Append(dxStringMarkChar2);
      ABuffer.Append(dxRefSeparator);
    end;
    dxReferenceToString(ABuffer, AArea, R1C1Reference,
      IfThen(croAbsoluteRow in AOptions, -1),
      IfThen(croAbsoluteColumn in AOptions, -1));

    Result := ABuffer.ToString;
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

function dxReferenceToString(const ARow, AColumn: Integer; R1C1Reference: Boolean = False;
  AOptions: TdxSpreadSheetCellReferenceOptions = []; const ASheetName: string = ''): string;
begin
  Result := dxReferenceToString(cxRect(AColumn, ARow, AColumn, ARow), R1C1Reference, AOptions, ASheetName);
end;

function dxReferenceToString(const AArea: TRect; R1C1Reference: Boolean; const ARowOrigin, AColumnOrigin: Integer): string;
var
  ABuffer: TStringBuilder;
begin
  ABuffer := TdxStringBuilderManager.Get(dxMaxReferenceLength);
  try
    dxReferenceToString(ABuffer, AArea, R1C1Reference, ARowOrigin, AColumnOrigin);
    Result := ABuffer.ToString;
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

procedure dxReferenceToString(ABuffer: TStringBuilder; const AArea: TRect;
  R1C1Reference: Boolean; const ARowOrigin, AColumnOrigin: Integer);
var
  AColumn1: TdxSpreadSheetReference;
  AColumn2: TdxSpreadSheetReference;
  ARow1: TdxSpreadSheetReference;
  ARow2: TdxSpreadSheetReference;
begin
  AColumn1 := TdxSpreadSheetReference.Create(AArea.Left, AColumnOrigin, dxSpreadSheetMaxColumnIndex);
  AColumn2 := TdxSpreadSheetReference.Create(AArea.Right, AColumnOrigin, dxSpreadSheetMaxColumnIndex);
  ARow1 := TdxSpreadSheetReference.Create(AArea.Top, ARowOrigin, dxSpreadSheetMaxRowIndex);
  ARow2 := TdxSpreadSheetReference.Create(AArea.Bottom, ARowOrigin, dxSpreadSheetMaxRowIndex);

  if R1C1Reference then
  begin
    if (AArea.Left <> AArea.Right) or (AArea.Top <> AArea.Bottom) then
      dxR1C1ReferenceAreaToString(ABuffer, ARowOrigin, AColumnOrigin, '', ARow1, AColumn1, '', ARow2, AColumn2)
    else
      dxR1C1ReferenceToString(ABuffer, ARowOrigin, AColumnOrigin, ARow1, AColumn1);
  end
  else
  begin
    if ARow2.IsAllItems and (ARow1.ActualValue(ARowOrigin) = 0) then
      ARow1.IsAllItems := True;
    if AColumn2.IsAllItems and (AColumn1.ActualValue(AColumnOrigin) = 0) then
      AColumn1.IsAllItems := True;

    if ARow1.IsAllItems and ARow2.IsAllItems and AColumn1.IsAllItems and AColumn2.IsAllItems then
    begin
      ARow1.IsAllItems := False;
      ARow2.IsAllItems := False;
      ARow2.SetActualValue(0, dxSpreadSheetMaxRowIndex);
      AColumn1.IsAllItems := False;
      AColumn2.IsAllItems := False;
      AColumn2.SetActualValue(0, dxSpreadSheetMaxColumnIndex);
    end;

    dxReferenceToString(ABuffer, ARowOrigin, AColumnOrigin, ARow1, AColumn1);
    if (AArea.Left <> AArea.Right) or (AArea.Top <> AArea.Bottom) then
      dxReferenceToString(ABuffer.Append(dxAreaSeparator), ARowOrigin, AColumnOrigin, ARow2, AColumn2);
  end;
end;

function dxReferenceToString(const ARowOrigin, AColumnOrigin: Integer; const ARow, AColumn: TdxSpreadSheetReference): string;
var
  ABuffer: TStringBuilder;
begin
  ABuffer := TdxStringBuilderManager.Get(dxMaxReferenceLength);
  try
    dxReferenceToString(ABuffer, ARowOrigin, AColumnOrigin, ARow, AColumn);
    Result := ABuffer.ToString;
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

procedure dxReferenceToString(ABuffer: TStringBuilder;
  const ARowOrigin, AColumnOrigin: Integer; const ARow, AColumn: TdxSpreadSheetReference);
begin
  if (ARow.ActualValue(ARowOrigin) < 0) or (AColumn.ActualValue(AColumnOrigin) < 0) then
  begin
    ABuffer.Append(serRefError);
    Exit;
  end;

  if not AColumn.IsAllItems then
  begin
    if AColumn.IsAbsolute then
      ABuffer.Append(dxAbsoluteReferenceChar);
    ABuffer.Append(TdxSpreadSheetColumnHelper.NameByIndex(AColumn.ActualValue(AColumnOrigin), False));
  end;

  if not ARow.IsAllItems then
  begin
    if ARow.IsAbsolute then
      ABuffer.Append(dxAbsoluteReferenceChar);
    ABuffer.Append(ARow.ActualValue(ARowOrigin) + 1);
  end;
end;

function dxR1C1ReferencePartToStr(const AValue: TStringBuilder;
  const APrefix: string; AOrigin: Integer; const AReference: TdxSpreadSheetReference): Boolean;
begin
  Result := AReference.ActualValue(AOrigin) >= 0;
  if Result then
  begin
    AValue.Append(APrefix);
    if AReference.IsAbsolute then
    begin
      if not AReference.IsAllItems then
        AValue.Append(AReference.Offset + 1);
    end
    else

    if AReference.Offset <> 0 then
    begin
      AValue.Append(dxReferenceLeftParenthesis);
      AValue.Append(AReference.Offset);
      AValue.Append(dxReferenceRightParenthesis);
    end;
  end
  else
    AValue.Append(serRefError);
end;

function dxR1C1ReferenceAreaPartToString(
  const AValue: TStringBuilder; const AOrigin: Integer;
  const ARef1, ARef2, ARef3: TdxSpreadSheetReference;
  const ASheet, ASheet2, APrefix: string): Boolean; inline;
begin
  Result := ARef3.IsAllItems;
  if Result then
  begin
    AValue.Append(ASheet);
    dxR1C1ReferencePartToStr(AValue, APrefix, AOrigin, ARef1);
    if (ARef2 <> ARef1) or (ASheet2 <> '') then
    begin
      AValue.Append(dxAreaSeparator);
      AValue.Append(ASheet2);
      dxR1C1ReferencePartToStr(AValue, APrefix, AOrigin, ARef2);
    end;
  end;
end;

function dxR1C1ReferenceAreaToString(const ARowOrigin, AColumnOrigin: Integer;
  const ASheet: string; const ARow, AColumn: TdxSpreadSheetReference;
  const ASheet2: string; const ARow2, AColumn2: TdxSpreadSheetReference): string;
var
  ABuffer: TStringBuilder;
begin
  ABuffer := TdxStringBuilderManager.Get(dxMaxReferenceLength);
  try
    dxR1C1ReferenceAreaToString(ABuffer, ARowOrigin, AColumnOrigin, ASheet, ARow, AColumn, ASheet2, ARow2, AColumn2);
    Result := ABuffer.ToString;
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

procedure dxR1C1ReferenceAreaToString(
  const ABuffer: TStringBuilder; const ARowOrigin, AColumnOrigin: Integer;
  const ASheet: string; const ARow, AColumn: TdxSpreadSheetReference;
  const ASheet2: string; const ARow2, AColumn2: TdxSpreadSheetReference);
var
  ALength: Integer;
begin
  ALength := ABuffer.Length;
  if not dxR1C1ReferenceAreaPartToString(ABuffer, ARowOrigin, ARow, ARow2, AColumn2, ASheet, ASheet2, dxRCRowReferenceChar) then
    if not dxR1C1ReferenceAreaPartToString(ABuffer, AColumnOrigin, AColumn, AColumn2, ARow2, ASheet, ASheet2, dxRCColumnReferenceChar) then
    begin
      ABuffer.Length := ALength;
      ABuffer.Append(ASheet);
      dxR1C1ReferenceToString(ABuffer, ARowOrigin, AColumnOrigin, ARow, AColumn);
      ABuffer.Append(dxAreaSeparator);
      ABuffer.Append(ASheet2);
      dxR1C1ReferenceToString(ABuffer, ARowOrigin, AColumnOrigin, ARow2, AColumn2);
    end;
end;

function dxR1C1ReferenceToString(const ARowOrigin, AColumnOrigin: Integer; const ARow, AColumn: TdxSpreadSheetReference): string;
var
  ABuffer: TStringBuilder;
begin
  ABuffer := TdxStringBuilderManager.Get(dxMaxReferenceLength);
  try
    dxR1C1ReferenceToString(ABuffer, ARowOrigin, AColumnOrigin, ARow, AColumn);
    Result := ABuffer.ToString;
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

procedure dxR1C1ReferenceToString(ABuffer: TStringBuilder;
  const ARowOrigin, AColumnOrigin: Integer;  const ARow, AColumn: TdxSpreadSheetReference);
var
  ALength: Integer;
begin
  ALength := ABuffer.Length;
  if not dxR1C1ReferencePartToStr(ABuffer, dxRCRowReferenceChar, ARowOrigin, ARow) or
    not dxR1C1ReferencePartToStr(ABuffer, dxRCColumnReferenceChar, AColumnOrigin, AColumn) then
  begin
    ABuffer.Length := ALength;
    ABuffer.Append(serRefError);
  end;
end;

function dxSpreadSheetCompareText(const S1, S2: Pointer; L1, L2: Integer): Integer;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, L1, S2, L2) - 2;
end;

function dxSpreadSheetCompareText(const S1: string; const S2: Pointer; L2: Integer): Integer;
begin
  if L2 = -1 then
    L2 := Length(S1);
  Result := dxSpreadSheetCompareText(PWideChar(S1), S2, Length(S1), L2);
end;

function dxSpreadSheetCompareText(const S1, S2: string): Integer;
begin
  Result := dxSpreadSheetCompareText(PWideChar(S1), PWideChar(S2), Length(S1), Length(S2));
end;

function dxSpreadSheetIsFormula(const S: string): Boolean;
begin
  Result := (S <> '') and (S[1] = dxDefaultOperations[opEQ]);
end;

function dxSpreadSheetLowerCase(const S: string): string;
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PWideChar(S), Len);
  if Len > 0 then
    CharLowerBuffW(Pointer(Result), Len);
end;

function dxSpreadSheetUpperCase(const S: string): string;
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PWideChar(S), Len);
  if Len > 0 then
    CharUpperBuffW(Pointer(Result), Len);
end;

function dxSpreadSheetVarCompare(const V1, V2: Variant; ACaseSensitive: Boolean = True): Integer; inline;
var
  AIsNullV1, AIsNullV2: Boolean;
begin
  AIsNullV1 := dxSpreadSheetIsNullValue(V1);
  AIsNullV2 := dxSpreadSheetIsNullValue(V2);

  if AIsNullV1 and AIsNullV2 then
    Result := 0
  else

  if VarIsStr(V1) then
  begin
    if (V1 = '') and AIsNullV2 then
      Result := 0
    else if AIsNullV2 or VarIsNumeric(V2) then
      Result := 1
    else if dxIsLogical(V2) then
      Result := -1
    else if ACaseSensitive then
      Result := VarCompare(V1, V2)
    else if VarIsStr(V2) then
      Result := AnsiCompareText(V1, V2)
    else
      Result := VarCompare(dxSpreadSheetUpperCase(V1), V2);
  end
  else

  if VarIsStr(V2) then
    Result := -dxSpreadSheetVarCompare(V2, V1, ACaseSensitive)
  else

  if AIsNullV1 or AIsNullV2 then
  begin
    if AIsNullV1 then
      Result := VarCompare(0, V2)
    else
      Result := VarCompare(V1, 0);
  end
  else
    Result := VarCompare(V1, V2);
end;

function dxSpreadSheetCharIsEqual(const AChar1, AChar2: Char): Boolean;
begin
  Result := Integer(AChar1) = Integer(AChar2);
end;

function dxSpreadSheetTextIsEqual(const AText1, AText2: string): Boolean;
begin
  Result := dxSpreadSheetCompareText(AText1, AText2) = 0;
end;

function dxSpreadSheetArea(const ATopLeft: TPoint; const ASize: TSize): TRect;
begin
  Result.TopLeft := ATopLeft;
  Result.BottomRight := cxPointOffset(Result.TopLeft, ASize.cx - 1, ASize.cy - 1);
end;

function dxSpreadSheetAreaHeight(const AArea: TRect): Integer;
begin
  Result := AArea.Bottom - AArea.Top + 1;
end;

function dxSpreadSheetAreaWidth(const AArea: TRect): Integer;
begin
  Result := AArea.Right - AArea.Left + 1;
end;

function dxSpreadSheetAreaSize(const AArea: TRect): TSize;
begin
  Result := cxSize(dxSpreadSheetAreaWidth(AArea), dxSpreadSheetAreaHeight(AArea));
end;

function dxSpreadSheetCellsUnion(const AArea1, AArea2: TRect): TRect;
begin
  Result.Left := Min(AArea1.Left, AArea2.Left);
  Result.Top := Min(AArea1.Top, AArea2.Top);
  Result.Right := Max(AArea1.Right, AArea2.Right);
  Result.Bottom := Max(AArea1.Bottom, AArea2.Bottom);
end;

function dxSpreadSheetContains(const AArea, ATestArea: TRect): Boolean;
begin
  Result :=
    dxSpreadSheetContains(AArea, ATestArea.Top, ATestArea.Left) and
    dxSpreadSheetContains(AArea, ATestArea.Bottom, ATestArea.Right);
end;

function dxSpreadSheetContains(const AArea: TRect; ARow, AColumn: Integer): Boolean;
begin
  Result := (AColumn >= AArea.Left) and (AColumn <= AArea.Right) and (ARow >= AArea.Top) and (ARow <= AArea.Bottom);
end;

function dxSpreadSheetContainsColumn(const AArea: TRect; AColumn: Integer): Boolean;
begin
  Result := (AColumn >= AArea.Left) and (AColumn <= AArea.Right);
end;

function dxSpreadSheetContainsRow(const AArea: TRect; ARow: Integer): Boolean;
begin
  Result := (ARow >= AArea.Top) and (ARow <= AArea.Bottom);
end;

function dxSpreadSheetEntireSheetArea: TRect;
begin
  Result := cxRect(0, 0, dxSpreadSheetMaxColumnIndex, dxSpreadSheetMaxRowIndex);
end;

function dxSpreadSheetGetRealArea(const AArea: TRect): TRect;
begin
  Result := cxRectAdjust(AArea);
  Result.Top := Max(Result.Top, 0);
  Result.Left := Max(Result.Left, 0);
  Result.Right := Min(Result.Right, dxSpreadSheetMaxColumnIndex);
  Result.Bottom := Min(Result.Bottom, dxSpreadSheetMaxRowIndex);
end;

function dxSpreadSheetIntersects(const AArea1, AArea2: TRect): Boolean;
var
  R: TRect;
begin
  Result := dxSpreadSheetIntersects(AArea1, AArea2, R);
end;

function dxSpreadSheetIntersects(const AArea1, AArea2: TRect; var AArea3: TRect): Boolean;
begin
  AArea3.Left := Max(AArea2.Left, AArea1.Left);
  AArea3.Top := Max(AArea2.Top, AArea1.Top);
  AArea3.Right := Min(AArea2.Right, AArea1.Right);
  AArea3.Bottom := Min(AArea2.Bottom, AArea1.Bottom);
  Result := (AArea3.Left <= AArea3.Right) and (AArea3.Top <= AArea3.Bottom);
  if not Result then
    AArea3 := cxNullRect;
end;

function dxSpreadSheetIsNullValue(const AValue: Variant): Boolean; inline;
begin
  Result := VarIsEmpty(AValue) or VarIsNull(AValue);
end;

function dxSpreadSheetIsSingleCellArea(const R: TRect): Boolean;
begin
  Result := (R.Width = 0) and (R.Height = 0);
end;

function dxSpreadSheetIsValidArea(const AArea: TRect): Boolean;
begin
  Result := (AArea.Left >= 0) and (AArea.Top >= 0) and (AArea.Right >= AArea.Left) and (AArea.Bottom >= AArea.Top);
end;

function dxSpreadSheetIsValidCellReference(const ARowIndex, AColumnIndex: Integer): Boolean;
begin
  Result := InRange(AColumnIndex, 0, dxSpreadSheetMaxColumnIndex) and InRange(ARowIndex, 0, dxSpreadSheetMaxRowIndex);
end;

function dxSpreadSheetIsEntireColumn(const AArea: TRect): Boolean;
begin
  Result := (AArea.Top = 0) and (AArea.Bottom >= dxSpreadSheetMaxRowIndex);
end;

function dxSpreadSheetIsEntireRow(const AArea: TRect): Boolean;
begin
  Result := (AArea.Left = 0) and (AArea.Right >= dxSpreadSheetMaxColumnIndex);
end;

function dxSpreadSheetIsEntireRowOrColumn(const AArea: TRect): Boolean;
begin
  Result := dxSpreadSheetIsEntireRow(AArea) or dxSpreadSheetIsEntireColumn(AArea);
end;

function dxSpreadSheetIsErrorString(const S: string; var ACode: TdxSpreadSheetFormulaErrorCode): Boolean; inline;
var
  I: TdxSpreadSheetFormulaErrorCode;
begin
  for I := Low(TdxSpreadSheetFormulaErrorCode) to High(TdxSpreadSheetFormulaErrorCode) do
  begin
    Result := dxSpreadSheetCompareText(S, dxSpreadSheetErrorCodeToString(I)) = 0;
    if not Result then
      Continue;
    ACode := I;
    Break;
  end;
end;

function dxSpreadSheetOffsetArea(const AArea: TRect; X, Y: Integer): TRect;
begin
  Result := AArea;
  Inc(Result.Left, X);
  Inc(Result.Top, Y);
  if Result.Right <> MaxInt then
    Inc(Result.Right, X);
  if Result.Bottom <> MaxInt then
    Inc(Result.Bottom, Y);
end;

procedure dxSpreadSheetValidate(var AValue: Integer; const AMinValue, AMaxValue: Integer);
begin
  if AValue < AMinValue then
    AValue := AMinValue
  else
    if AValue > AMaxValue then
      AValue := AMaxValue
end;

{ TdxSpreadSheetColumnHelper }

class function TdxSpreadSheetColumnHelper.IndexByName(AName: string; AIsRCReferenceStyle: Boolean = False): Integer;
var
  I: Integer;
begin
  if AIsRCReferenceStyle then
    Result := StrToInt(AName)
  else
  begin
    Result := 0;
    AName := UpperCase(AName);
    for I := Length(AName) downto 1 do
      Inc(Result, dxIntPower(26, Length(AName) - I) * (Byte(AName[I]) - Byte('A') + 1));
    Dec(Result);
  end;
end;

class function TdxSpreadSheetColumnHelper.NameByIndex(
  AIndex: Integer; AIsRCReferenceStyle: Boolean = False): string;
begin
  if AIsRCReferenceStyle then
    Result := IntToStr(AIndex + 1)
  else
  begin
    Result := '';
    if AIndex >= 26 then
      Result := Result + NameByIndex(AIndex div 26 - 1, AIsRCReferenceStyle);
    Result := Result + Char(Byte('A') + AIndex mod 26);
  end;
end;

{ TdxSpreadSheetExcelColumnWidthHelper }

constructor TdxSpreadSheetExcelColumnWidthHelper.Create;
begin
  inherited Create;
  FFont := TFont.Create;
  FFont.PixelsPerInch := dxDefaultDPI;
  FFont.OnChange := FontChangeHandler;
end;

destructor TdxSpreadSheetExcelColumnWidthHelper.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

function TdxSpreadSheetExcelColumnWidthHelper.CharsNumberToPixels(const ACharsNumber: Double): Integer;
begin
  Result := WidthToPixels(CharsNumberToWidth(ACharsNumber));
end;

function TdxSpreadSheetExcelColumnWidthHelper.CharsNumberToWidth(const ACharsNumber: Double): Double;
begin
  Result := Trunc((ACharsNumber * MaxDigitWidth + dxExcelColumnWidthPadding) / MaxDigitWidth * 256) / 256;
end;

function TdxSpreadSheetExcelColumnWidthHelper.PixelsToCharsNumber(const APixels: Integer): Integer;
begin
  Result := Trunc(WidthToCharsNumber(PixelsToWidth(APixels)));
end;

function TdxSpreadSheetExcelColumnWidthHelper.PixelsToSpacesNumber(const APixels: Integer): Integer;
begin
  Result := MulDiv(APixels, 2, TwoSpaceWidth);
end;

function TdxSpreadSheetExcelColumnWidthHelper.PixelsToWidth(const APixels: Integer): Double;
begin
  Result := CharsNumberToWidth((APixels - dxExcelColumnWidthPadding) / MaxDigitWidth);
end;

function TdxSpreadSheetExcelColumnWidthHelper.WidthToCharsNumber(const AWidth: Double): Double;
begin
  Result := Trunc((WidthToPixels(AWidth) - dxExcelColumnWidthPadding) / MaxDigitWidth * 100 + 0.5) / 100;
end;

function TdxSpreadSheetExcelColumnWidthHelper.WidthToPixels(const AWidth: Double): Integer;
begin
  Result := Trunc((256 * AWidth + Trunc(128 / MaxDigitWidth)) / 256 * MaxDigitWidth);
end;

procedure TdxSpreadSheetExcelColumnWidthHelper.CalculateMaxDigitWidth;
var
  APrevPixelsPerInch: Integer;
  C: Char;
begin
  APrevPixelsPerInch := dxSpreadSheetPrepareCanvas(cxScreenCanvas, Font);
  try
    FMaxDigitWidth := 0;
    for C := '0' to '9' do
      FMaxDigitWidth := Max(FMaxDigitWidth, cxScreenCanvas.TextWidth(C));
  finally
    dxSpreadSheetUnprepareCanvas(cxScreenCanvas, APrevPixelsPerInch);
  end;
end;

procedure TdxSpreadSheetExcelColumnWidthHelper.CalculateTwoSpaceWidth;
var
  AMetrics: TTextMetric;
  APrevPixelsPerInch: Integer;
begin
  APrevPixelsPerInch := dxSpreadSheetPrepareCanvas(cxScreenCanvas, Font);
  GetTextMetrics(cxScreenCanvas.Handle, AMetrics);
  dxSpreadSheetUnprepareCanvas(cxScreenCanvas, APrevPixelsPerInch);
  FTwoSpaceWidth := AMetrics.tmAscent + AMetrics.tmDescent;
end;

procedure TdxSpreadSheetExcelColumnWidthHelper.FontChangeHandler(Sender: TObject);
begin
  FTwoSpaceWidth := 0;
  FMaxDigitWidth := 0;
end;

function TdxSpreadSheetExcelColumnWidthHelper.GetMaxDigitWidth: Integer;
begin
  if FMaxDigitWidth = 0 then
    CalculateMaxDigitWidth;
  Result := FMaxDigitWidth;
end;

function TdxSpreadSheetExcelColumnWidthHelper.GetTwoSpaceWidth: Integer;
begin
  if FTwoSpaceWidth = 0 then
    CalculateTwoSpaceWidth;
  Result := FTwoSpaceWidth;
end;

procedure TdxSpreadSheetExcelColumnWidthHelper.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

function TdxSpreadSheetExcelColumnWidthHelper.SpacesNumberToPixels(const ASpaceNumber: Integer): Integer;
begin
  Result := MulDiv(ASpaceNumber, TwoSpaceWidth, 2);
end;

function dxSpreadSheetErrorCodeToString(ACode: TdxSpreadSheetFormulaErrorCode): string;
begin
  Result := '';
  case ACode of
    ecNull:
      Result := cxGetResourceString(@serNullError);
    ecDivByZero:
      Result := cxGetResourceString(@serDivZeroError);
    ecValue:
      Result := cxGetResourceString(@serValueError);
    ecRefErr:
      Result := cxGetResourceString(@serRefError);
    ecNUM:
      Result := cxGetResourceString(@serNumError);
    ecName:
      Result := cxGetResourceString(@serNameError);
    ecNA:
      Result := cxGetResourceString(@serNAError);
  end;
end;

procedure dxSpreadSheetExtractConditionParams(const S: string; const AFormatSettings: TdxSpreadSheetFormatSettings;
  var AConditionValue: Variant; var AOperation: TdxSpreadSheetFormulaOperation);
var
  I, L, L1: Integer;
  S1: string;
const
  ACandidate: array[0..5] of TdxSpreadSheetFormulaOperation = (opLE, opGE, opNE, opLT, opGT, opEQ);
begin
  AOperation := opEQ;
  S1 := S;
  for I := 0 to High(ACandidate) do
  begin
    L := Length(dxDefaultOperations[ACandidate[I]]);
    L1 := Min(L, Length(S1));
    if dxSpreadSheetCompareText(PWideChar(S1), PWideChar(dxDefaultOperations[ACandidate[I]]), L1, L) = 0 then
    begin
      AOperation := ACandidate[I];
      Delete(S1, 1, L);
      Break;
    end;
  end;
  if not dxTryStrToOrdinal(S1, AConditionValue, AFormatSettings) then
    AConditionValue := dxSpreadSheetUpperCase(S1);
end;

end.
