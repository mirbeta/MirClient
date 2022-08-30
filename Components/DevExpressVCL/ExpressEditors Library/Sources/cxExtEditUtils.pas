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

unit cxExtEditUtils;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Forms, Classes, Controls, Graphics, ImgList, Messages,
  StdCtrls, SysUtils,
  cxCheckBox, cxClasses, cxContainer, cxControls, cxEdit,
  cxEditPaintUtils, cxEditUtils, cxGraphics, cxLookAndFeelPainters,
  cxLookAndFeels, cxTextEdit, cxVariants, dxThemeManager;

const
  MRUDelimiterWidth = 3;

function DrawBounds(ACanvas: TcxCanvas; Bounds: TRect; const AUpperLeftColor,
  ALowerRightColor: TColor): TRect;
procedure DrawCanvasLine(ACanvas: TCanvas; const AColor: TColor;
  const AFromPoint, AToPoint: TPoint);
procedure DrawMRUDelimiter(ACanvas: TCanvas; const AItemRect: TRect;
  AIsItemSelected: Boolean);
function CalcMaxWidth(ACanvas: TCanvas; const AText: string): Integer;
function IncColor(const AColor: TColor; const AR, AG, AB: Integer): TColor; overload;
function IncColor(const AColor: TColor; const X: Integer): TColor; overload;
function CalcCenterPosHeight(const ARect: TRect; const ADrawHeight: Integer): Integer;
function CalcDrawWidth(const ARect: TRect; const ADrawHeight: Integer): Integer;
function IsVarEmpty(const AValue: Variant): Boolean;
function IsValidStringForInt(S: string): Boolean;
function IsValidStringForDouble(const AValue: string): Boolean;
function cxStrToInt(const AValue: string;
  AToFirstNonNum: Boolean = False): Integer;
function cxStrToFloat(const AValue: string;
  AToFirstNonNum: Boolean = False): Extended;
function cxStrToColor(S: string; out AColor: TColor): Boolean;
function cxRGBStringColorToColor(const AString: string): TColor;
function cxHexRGBStringColorToColor(const AString: string): TColor;
function CheckStateToString(const Value: TcxCheckBoxState): string;
function StringToCheckState(const Value: string; const AllowGrayed: Boolean): TcxCheckBoxState;
function GetWord(const APosition: Integer; const S: string; const Delimiter: Char): string;
function AdjustCanvasFont(ACanvas: TCanvas; AFont: TFont; AAngle: Integer): Boolean;

implementation

uses
  Types, Math, cxDrawTextUtils, dxThemeConsts, dxUxTheme, dxCore, cxFormats, StrUtils, cxGeometry;

type
  TWinControlAccess = class(TWinControl);

function DrawBounds(ACanvas: TcxCanvas; Bounds: TRect; const AUpperLeftColor, ALowerRightColor: TColor): TRect;
begin
  ACanvas.Pen.Color := AUpperLeftColor;
  ACanvas.Line(Bounds.Left, Bounds.Top, Bounds.Left, Bounds.Bottom + 1);
  ACanvas.Line(Bounds.Left, Bounds.Top, Bounds.Right + 1, Bounds.Top);
  ACanvas.Pen.Color := ALowerRightColor;
  ACanvas.Line(Bounds.Right, Bounds.Top + 1, Bounds.Right, Bounds.Bottom);
  ACanvas.Line(Bounds.Left + 1, Bounds.Bottom, Bounds.Right + 1, Bounds.Bottom);
  Result := cxRectContent(Bounds, Rect(1, 1, 1, 1));
end;

procedure DrawCanvasLine(ACanvas: TCanvas; const AColor: TColor; const AFromPoint, AToPoint: TPoint);
begin
  ACanvas.Pen.Color := AColor;
  ACanvas.MoveTo(AFromPoint.x, AFromPoint.y);
  ACanvas.LineTo(AToPoint.x, AToPoint.y);
end;

procedure DrawMRUDelimiter(ACanvas: TCanvas; const AItemRect: TRect; AIsItemSelected: Boolean);
begin
  if AIsItemSelected then
    ACanvas.Pen.Color := clWindow
  else
    ACanvas.Pen.Color := clWindowText;
  ACanvas.MoveTo(AItemRect.Left, AItemRect.Bottom - MRUDelimiterWidth);
  ACanvas.LineTo(AItemRect.Right, AItemRect.Bottom - MRUDelimiterWidth);
  ACanvas.MoveTo(AItemRect.Left, AItemRect.Bottom - 1);
  ACanvas.LineTo(AItemRect.Right, AItemRect.Bottom - 1);
end;

function CalcMaxWidth(ACanvas: TCanvas; const AText: string): Integer;
var
  FStringList: TStringList;
  I, FWidth: Integer;
begin
  Result := ACanvas.TextWidth(AText);
  FStringList := TStringList.Create;
  try
    FStringList.Text := AText;
    for I := 0 to FStringList.Count - 1 do
    begin
      FWidth := ACanvas.TextWidth(FStringList[I]);
      if FWidth > Result then Result := FWidth;
    end;
  finally
    FStringList.Free;
  end;
  Inc(Result, 1);
end;

function IncColor(const AColor: TColor; const X: Integer): TColor;
begin
  Result := IncColor(AColor, X, X, X);
end;

function IncColor(const AColor: TColor; const AR, AG, AB: Integer): TColor;
var
  FR, FG, FB: Integer;
begin
  FR := GetRValue(ColorToRGB(AColor));
  FG := GetGValue(ColorToRGB(AColor));
  FB := GetBValue(ColorToRGB(AColor));
  if (FR + AR) > High(Byte) then
    FR := High(Byte)
  else
    Inc(FR, AR);
  if (FG + AG) > High(Byte) then
    FG := High(Byte)
  else
    Inc(FG, AG);
  if (FB + AB) > High(Byte) then
    FB := High(Byte)
  else
    Inc(FB, AB);
  Result := RGB(FR, FG, FB);
end;

function CalcCenterPosHeight(const ARect: TRect; const ADrawHeight: Integer): Integer;
begin
  Result := (ARect.Bottom - ARect.Top - ADrawHeight) div 2;
end;

function CalcDrawWidth(const ARect: TRect; const ADrawHeight: Integer): Integer;
begin
  Result := (CalcCenterPosHeight(ARect, ADrawHeight) * 2 + 2) + ADrawHeight;
end;

function IsVarEmpty(const AValue : Variant): Boolean;
begin
  Result := VarIsNull(AValue) or VarIsEmpty(AValue);
end;

{$HINTS OFF}
function IsValidStringForInt(S: string): Boolean;
var
  ACode, AValue: Integer;
begin
  Result := False;
  S := Trim(S);
  if Length(S) > 0 then
  begin
    Val(S, AValue, ACode);
    Result := ACode = 0;
  end;
end;
{$HINTS ON}

function IsValidStringForDouble(const AValue: string): Boolean;
var
  I: Integer;
  AString: string;
  ADecimalSeparatorCounter: Integer;
begin
  AString := Trim(AValue);
  ADecimalSeparatorCounter := 0;
  Result := Length(AString) > 0;
  { Check for valid numeric symbols in string }
  if Result then
    for I := 1 to Length(AString) do
    begin
      if not dxCharInSet(AString[I], ['0'..'9', dxFormatSettings.DecimalSeparator]) and
        ((AString[I] <> '-') or ((AString[I] = '-') and (I > 1))) then
          Result := False
      else
        if AString[I] = dxFormatSettings.DecimalSeparator then
        begin
          if ADecimalSeparatorCounter = 0 then
            Inc(ADecimalSeparatorCounter)
          else
            Result := False
        end;

      if not Result then
        Break;
    end;
  { Check for valid Double range }
  if Result then
    Result := (Abs(StrToFloat(AString)) <= MaxDouble);
end;

function cxStrToInt(const AValue: string;
  AToFirstNonNum: Boolean = False): Integer;
var
  I: Integer;
  S: string;
begin
  S := '';
  for I := 1 to Length(AValue) do
    if dxCharInSet(AValue[I], ['0'..'9', '-']) then
      S := S + AValue[I]
    else
      if AToFirstNonNum then
        Break;
  if S = '' then
    S := '0';
  Result := StrToInt(S);
end;

function cxStrToFloat(const AValue: string;
  AToFirstNonNum: Boolean = False): Extended;
var
  I: Integer;
  S: string;
begin
  S := '';
  for I := 1 to Length(AValue) do
    if dxCharInSet(AValue[I], ['0'..'9', '-', dxFormatSettings.DecimalSeparator]) then
        S := S + AValue[I]
    else
      if AToFirstNonNum then
        Break;
  if S = '' then
    S := '0';
  Result := StrToFloat(S);
end;

function TryRGBStrToColor(const S: string; out AColor: TColor): Boolean;
const
  RGBSeparatorChar = '.';
var
  APos: Integer;
  R, G, B: Integer;

  function ScanPart(out AValue: Integer): Boolean;
  var
    APart: string;
  begin
    APart := '';
    Inc(APos);
    while (APos <= Length(S)) and (S[APos] <> RGBSeparatorChar) do
    begin
      APart := APart + S[APos];
      Inc(APos);
    end;
    Result := (Length(APart) in [1..3]) and IsValidStringForInt(APart);
    if Result then
    begin
      AValue := cxStrToInt(APart);
      Result := AValue in [0..255];
    end;
  end;

begin
  AColor := 0;
  APos := 0;
  Result := ScanPart(R) and ScanPart(G) and ScanPart(B) and (S[Length(S)] <> RGBSeparatorChar);
  if Result then
    AColor := RGB(R, G, B);
end;

function cxStrToColor(S: string; out AColor: TColor): Boolean;
var
  ATempColor: Longint;
begin
  S := Trim(S);
  Result := False;
  if Length(S) > 0 then
  begin
    Result := IdentToColor(S, ATempColor);
    if Result then
      AColor := ATempColor
    else
    begin
      Result := S[1] = HexDisplayPrefix;
      if Result then
        AColor := StrToIntDef(S, 0)
      else
      begin
        Result := IsValidStringForInt(S);
        if Result then
          AColor := TColor(cxStrToInt(S))
        else
          Result := TryRGBStrToColor(S, AColor);
      end;
    end;
  end;
end;

function cxRGBStringColorToColor(const AString: string): TColor;
var
  I, FPos: Integer;
  R, G, B: Integer;
  S, FSColor: string;
begin
  R := 0;
  G := 0;
  B := 0;
  FSColor := AString;
  for I := 1 to 3 do
  begin
    S := '';
    FPos := Pos('.', FSColor);
    if (FPos > 0) then
      S := Copy(FSColor, 1, FPos - 1)
    else
      S := FSColor;
    FSColor := Copy(FSColor, FPos + 1, Length(FSColor) - FPos);
    case I of
      1: R := cxStrToInt(S);
      2: G := cxStrToInt(S);
      3: B := cxStrToInt(S);
    end;
  end;
  Result := RGB(R, G, B);
end;

function cxHexRGBStringColorToColor(const AString: string): TColor;
var
  R, G, B: Integer;
  S: string;

  function IsHexDigit(C: Char): Boolean;
  begin
    Result := (C >= '0') and (C <= '9') or (C >= 'A') and (C <= 'F') or
      (C >= 'a') and (C <= 'f');
  end;

  function RemoveNonHexChars(const AString: string): string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 1 to Length(AString) do
      if IsHexDigit(AString[I]) then
        Result := Result + AString[I];
  end;

  function HexStrToInt(const S: string): Longint;
  var
    HexStr: string;
  begin
    if Pos('$', S) = 0 then
      HexStr := '$' + S
    else
      HexStr := S;
    Result := StrToIntDef(HexStr, 0);
  end;

  function IntToByte(const Value: Integer): Byte;
  begin
    if Value > MaxByte then
      Result := MaxByte
    else
      Result := Value;
  end;

begin
  S := RemoveNonHexChars(AString);
  R := IntToByte(HexStrToInt(Copy(S, 1, 2)));
  G := IntToByte(HexStrToInt(Copy(S, 3, 2)));
  B := IntToByte(HexStrToInt(Copy(S, 5, 2)));
  Result := RGB(R, G, B);
end;

function CheckStateToString(const Value: TcxCheckBoxState): string;
begin
  case Value of
    cbsChecked: Result := '1';
    cbsGrayed: Result := '2';
    else Result := '0';
  end;
end;

function StringToCheckState(const Value: string; const AllowGrayed: Boolean): TcxCheckBoxState;
begin
  if AllowGrayed then
  begin
    if Value = '1' then Result := cbsChecked
     else if Value = '0' then Result := cbsUnchecked
      else Result := cbsGrayed;
  end
  else
  begin
    if Value = '1' then Result := cbsChecked
      else Result := cbsUnchecked;
  end;
end;

function GetWord(const APosition: Integer; const S: string;
  const Delimiter: Char): string;
var
  I, FPos: Integer;
  FStr: string;
begin
  Result := '';
  if APosition <= 0 then Exit;
  FStr := S;
  I := 1;
  FPos := Pos(Delimiter, FStr);
  if FPos = 0 then
  begin
    if APosition = 1 then Result := S;
  end
  else
  begin
    while FPos > 0 do
    begin
      if I = APosition then
      begin
        Result := Copy(FStr, 1, FPos - 1);
        Break;
      end
      else
        FStr := Copy(FStr, FPos + 1, Length(FStr));
      Inc(I);
      if FStr = '' then Break;
      FPos := Pos(Delimiter, FStr);
      if (FPos = 0) and (I = APosition) then
        Result := FStr;
    end;
  end;
end;

function AdjustCanvasFont(ACanvas: TCanvas; AFont: TFont; AAngle: Integer): Boolean;
var
  ALogFont: TLogFont;
  ARealAngle: Integer;
  ATextMetric: TTextMetric;
begin
  ACanvas.Font.Assign(AFont);
  GetTextMetrics(ACanvas.Handle, ATextMetric);
  ARealAngle := (AAngle mod 360 + 360) mod 360;
  Result := ((ATextMetric.tmPitchAndFamily and TMPF_TRUETYPE) <> 0);
  if not Result then Exit;
  if ARealAngle <> 0 then
  begin
    cxGetFontData(ACanvas.Font.Handle, ALogFont);
    ALogFont.lfEscapement := ARealAngle * 10;
    ACanvas.Font.Handle := CreateFontIndirect(ALogFont);
  end;
end;

end.
