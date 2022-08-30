{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.Utils.NumberParser;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Generics.Defaults, Generics.Collections,
  dxCoreGraphics, dxCultureInfo;

type

  { TdxNumberStyles }

  TdxNumberStyles = class sealed
  public type
    TStyle = (
      AllowLeadingWhite,
      AllowTrailingWhite,
      AllowLeadingSign,
      AllowTrailingSign,
      AllowParentheses,
      AllowDecimalPoint,
      AllowThousands,
      AllowExponent,
      AllowCurrencySymbol,
      AllowHexSpecifier
    );
    TStyles = set of TStyle;
  public const
    AllowLeadingWhite   = [TStyle.AllowLeadingWhite];
    AllowTrailingWhite  = [TStyle.AllowTrailingWhite];
    AllowLeadingSign    = [TStyle.AllowLeadingSign];
    AllowTrailingSign   = [TStyle.AllowTrailingSign];
    AllowParentheses    = [TStyle.AllowParentheses];
    AllowDecimalPoint   = [TStyle.AllowDecimalPoint];
    AllowThousands      = [TStyle.AllowThousands];
    AllowExponent       = [TStyle.AllowExponent];
    AllowCurrencySymbol = [TStyle.AllowCurrencySymbol];
    AllowHexSpecifier   = [TStyle.AllowHexSpecifier];

    Integer   = [TStyle.AllowLeadingWhite, TStyle.AllowTrailingWhite, TStyle.AllowLeadingSign];
    Number    = [TStyle.AllowLeadingWhite, TStyle.AllowTrailingWhite, TStyle.AllowLeadingSign,
                TStyle.AllowTrailingSign, TStyle.AllowDecimalPoint,  TStyle.AllowThousands];
    Float     = [TStyle.AllowLeadingWhite, TStyle.AllowTrailingWhite, TStyle.AllowLeadingSign,
                TStyle.AllowDecimalPoint, TStyle.AllowExponent];
    Currency  = [TStyle.AllowLeadingWhite, TStyle.AllowTrailingWhite, TStyle.AllowLeadingSign,
                TStyle.AllowTrailingSign, TStyle.AllowParentheses,   TStyle.AllowDecimalPoint,
                TStyle.AllowThousands,    TStyle.AllowCurrencySymbol];
    Any       = [TStyle.AllowLeadingWhite, TStyle.AllowTrailingWhite, TStyle.AllowLeadingSign,
                TStyle.AllowTrailingSign, TStyle.AllowParentheses,   TStyle.AllowDecimalPoint,
                TStyle.AllowThousands,    TStyle.AllowExponent,      TStyle.AllowCurrencySymbol];
    HexNumber = [TStyle.AllowLeadingWhite, TStyle.AllowTrailingWhite, TStyle.AllowHexSpecifier];
  end;

  { TdxNumber }

  TdxNumber = class sealed
  private
    class function GetValueText(const AText: string; AStyle: TdxNumberStyles.TStyles): string; static; inline;
    class function IsWhite(C: Char): Boolean; static; inline;
  public
    class function TryParse(const AText: string; out AValue: Integer): Boolean; overload; static;
    class function TryParse(const AText: string; AStyle: TdxNumberStyles.TStyles;
      out AValue: Integer): Boolean; overload; static;
    class function TryParse(const AText: string; out AValue: Int64): Boolean; overload; static;
    class function TryParse(const AText: string; AStyle: TdxNumberStyles.TStyles;
      out AValue: Int64): Boolean; overload; static;
    class function TryParse(const AText: string; out AValue: Double): Boolean; overload; static;
    class function TryParse(const AText: string; AStyle: TdxNumberStyles.TStyles;
      out AValue: Double): Boolean; overload; static;
    class function TryParse(const AText: string; AStyle: TdxNumberStyles.TStyles;
      const ACultureInfo: TdxCultureInfo; out AValue: Double): Boolean; overload; static;
    class function TryParse(const AText: string; out AValue: Single): Boolean; overload; static;
    class function TryParse(const AText: string; AStyle: TdxNumberStyles.TStyles;
      out AValue: Single): Boolean; overload; static;
  end;

  { TdxMarkupLanguageColorParser }

  TdxMarkupLanguageColorParser = class sealed
  public
    class function ParseColor(const AValue: string): TdxAlphaColor; static;
    class function GetColorByName(const AValue: string): TdxAlphaColor; static;
    class function GetColor(const AColorName: string; AStartIndex: Integer): Integer; static;
    class function GetColorByRgb(const AColorName: string): TdxAlphaColor; static;
    class function GetColorByArgb(const AColorName: string): TdxAlphaColor; static;
    class function ParseRGB(const AValue: string): TdxAlphaColor; static;
  end;

implementation

uses
  SysUtils, cxGraphics, Graphics, Character,
  dxGenerics,
  dxStringHelper;

{ TdxNumber }

class function TdxNumber.TryParse(const AText: string; AStyle: TdxNumberStyles.TStyles; out AValue: Integer): Boolean;
begin
  if TdxNumberStyles.TStyle.AllowHexSpecifier in AStyle then
    Result := TryStrToInt('$' + GetValueText(AText, AStyle), AValue)
  else
    Result := TryStrToInt(GetValueText(AText, AStyle), AValue)
end;

class function TdxNumber.TryParse(const AText: string; out AValue: Integer): Boolean;
begin
  Result := TryStrToInt(AText, AValue)
end;

class function TdxNumber.TryParse(const AText: string; AStyle: TdxNumberStyles.TStyles; out AValue: Int64): Boolean;
begin
  if TdxNumberStyles.TStyle.AllowHexSpecifier in AStyle then
    Result := TryStrToInt64('$' + GetValueText(AText, AStyle), AValue)
  else
    Result := TryStrToInt64(GetValueText(AText, AStyle), AValue)
end;

class function TdxNumber.TryParse(const AText: string; out AValue: Int64): Boolean;
begin
  Result := TryStrToInt64(AText, AValue);
end;

class function TdxNumber.TryParse(const AText: string; AStyle: TdxNumberStyles.TStyles; out AValue: Double): Boolean;
var
  ACode: Integer;
begin
  Val(AText, AValue, ACode);
  Result := ACode = 0;
end;

class function TdxNumber.TryParse(const AText: string; out AValue: Double): Boolean;
begin
  Result := TryStrToFloat(AText, AValue);
end;

class function TdxNumber.TryParse(const AText: string; out AValue: Single): Boolean;
begin
  Result := TryStrToFloat(AText, AValue);
end;

class function TdxNumber.IsWhite(C: Char): Boolean;
begin
  Result := (C = #$0020) or ((C >= #$0009) and (C <= #$000D));
end;

class function TdxNumber.GetValueText(const AText: string; AStyle: TdxNumberStyles.TStyles): string;
var
  ALen: Integer;
  AStart, AEnd: PChar;
begin
  ALen := Length(AText);
  if ALen = 0 then
    Exit(AText);
  AStart := PChar(AText);
  AEnd := AStart + ALen - 1;
  if TdxNumberStyles.TStyle.AllowLeadingWhite in AStyle then
  begin
    while (AStart <= AEnd) and IsWhite(AStart^) do
      Inc(AStart);
    if AStart > AEnd then
       Exit('');
  end;
  if TdxNumberStyles.TStyle.AllowTrailingWhite in AStyle then
  begin
    while (AStart <= AEnd) and IsWhite(AEnd^) do
      Dec(AEnd);
    if AStart > AEnd then
       Exit('');
  end;
  SetString(Result, AStart, AEnd - AStart + 1);
end;

class function TdxNumber.TryParse(const AText: string; AStyle: TdxNumberStyles.TStyles; out AValue: Single): Boolean;
begin
  Result := TryStrToFloat(AText, AValue);
end;

class function TdxNumber.TryParse(const AText: string; AStyle: TdxNumberStyles.TStyles; const ACultureInfo: TdxCultureInfo;
  out AValue: Double): Boolean;
begin
  Result := TryStrToFloat(AText, AValue, ACultureInfo.FormatSettings);
end;

{ TdxMarkupLanguageColorParser }

class function TdxMarkupLanguageColorParser.ParseColor(const AValue: string): TdxAlphaColor;
var
  AColorName: TStringBuilder;
  AColor, AResult: TdxAlphaColor;
begin
  AColorName := TStringBuilder.Create;
  try
    if AValue[1] = '#' then
    begin
      AColorName.Append(Copy(AValue, 2, Length(AValue) - 1));
      case AColorName.Length of
      8:
        Result := GetColorByArgb(AColorName.ToString);
      4:
        begin
          AColorName.Insert(0, AColorName[0]);
          AColorName.Insert(2, AColorName[2]);
          AColorName.Insert(4, AColorName[4]);
          AColorName.Insert(6, AColorName[6]);
          Result := GetColorByArgb(AColorName.ToString);
        end;
      6:
        Result := GetColorByRgb(AColorName.ToString);
      3:
        begin
          AColorName.Insert(0, AColorName[0]);
          AColorName.Insert(2, AColorName[2]);
          AColorName.Insert(4, AColorName[4]);
          Result := GetColorByRgb(AColorName.ToString);
        end;
      else
        Result := GetColorByName(AValue);
      end;
    end
    else
    begin
      if Length(AValue) = 6 then
      begin
        AColor := GetColorByRgb(AValue);
        if AColor <> TdxAlphaColors.Empty then
          Exit(AColor);
      end;
      if TdxStringHelper.StartsWith(AValue, 'rgb(', True) then
      begin
        AResult := ParseRGB(TdxStringHelper.Substring(AValue, 4));
        if AResult <> TdxAlphaColors.Empty then
          Exit(AResult);
      end;
      Result := GetColorByName(AValue);
    end;
  finally
    AColorName.Free;
  end;
end;

class function TdxMarkupLanguageColorParser.GetColorByName(const AValue: string): TdxAlphaColor;
var
  AColor: TColor;
begin
  if not cxColorByName(AValue, AColor) then
    Result := TdxAlphaColors.Empty
  else
    Result := TdxAlphaColors.FromColor(AColor);

end;

class function TdxMarkupLanguageColorParser.GetColor(const AColorName: string; AStartIndex: Integer): Integer;
var
  AColor: Integer;
  ASr: string;
begin
  ASr := TdxStringHelper.Substring(AColorName, AStartIndex, 2);
  TdxNumber.TryParse(ASr, TdxNumberStyles.HexNumber, AColor);
  if (AColor = 0) and (ASr <> '00') then
    AColor := -1;
  Result := AColor;
end;

class function TdxMarkupLanguageColorParser.GetColorByRgb(const AColorName: string): TdxAlphaColor;
var
  R, G, B: Integer;
begin
  R := GetColor(AColorName, 0);
  G := GetColor(AColorName, 2);
  B := GetColor(AColorName, 4);
  if ((R <> -1) and (G <> -1)) and (B <> -1) then
    Result := TdxAlphaColors.FromArgb(255, Byte(R), Byte(G), Byte(B))
  else
    Result := TdxAlphaColors.Empty;
end;

class function TdxMarkupLanguageColorParser.GetColorByArgb(const AColorName: string): TdxAlphaColor;
var
  A, R, G, B: Integer;
begin
  A := GetColor(AColorName, 0);
  R := GetColor(AColorName, 2);
  G := GetColor(AColorName, 4);
  B := GetColor(AColorName, 6);
  if (((A <> -1) and (R <> -1)) and (G <> -1)) and (B <> -1) then
    Result := TdxAlphaColors.FromArgb(Byte(A), Byte(R), Byte(G), Byte(B))
  else
    Result := TdxAlphaColors.Empty;
end;

class function TdxMarkupLanguageColorParser.ParseRGB(const AValue: string): TdxAlphaColor;
var
  ARgb: string;
  ACh: Char;
  AColor: Integer;
  AColors: TdxIntegerList;
  AIsDigit: Boolean;
begin
  ARgb := '';
  AColors := TdxIntegerList.Create;
  try
    for ACh in AValue do
    begin
      if (ACh <> ',') and (ACh <> ')') then
      begin
        {$IFDEF DELPHIXE4}
        if not ACh.IsWhiteSpace then
        {$ELSE}
        if not TCharacter.IsWhiteSpace(ACh) then
        {$ENDIF}
          ARgb := ARgb + ACh;
      end
      else
      begin
        AIsDigit := TdxNumber.TryParse(ARgb, AColor);
        if AIsDigit then
          AColors.Add(AColor);
        ARgb := '';
      end;
    end;
    if AColors.Count = 3 then
      Result := TdxAlphaColors.FromArgb(255, Byte(AColors[0]), Byte(AColors[1]), Byte(AColors[2]))
    else
      Result := TdxAlphaColors.Empty;
  finally
    AColors.Free;
  end;
end;

end.
