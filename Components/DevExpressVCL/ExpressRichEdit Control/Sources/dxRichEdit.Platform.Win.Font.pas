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

unit dxRichEdit.Platform.Win.Font;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
   Types, SysUtils, Windows, Graphics, Classes, dxCoreGraphics, cxGraphics,

   dxRichEdit.DocumentLayout.UnitConverter,
   dxRichEdit.Platform.Font,
   dxRichEdit.Utils.UnicodeRangeInfo;

type
  { TdxGdiUnicodeSubrangeBits }

  TdxUnicodeSubrangeBits = array[0..3] of DWORD;

  TdxGdiUnicodeSubrangeBits = record
  private const
    BitPerDWORD = 8 * SizeOf(DWORD);
  public
    Data: TdxUnicodeSubrangeBits;
    function GetBit(AIndex: Integer): Boolean; inline;
    procedure SetBit(AIndex: Integer; const Value: Boolean); inline;
    procedure Clear;

    property Bits[Index: Integer]: Boolean read GetBit write SetBit;
  end;

  { TdxCharacterDrawingAbilityTable }

  TdxCharacterDrawingAbilityTable = class
  strict private
    FDrawingAbility: TBits;
    FCalculated: TBits;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetDrawingAbility(ACharacter: Char; AValue: Boolean);
    function GetDrawingAbility(ACharacter: Char; out AAbility: Boolean): Boolean;
  end;

  { GdiFontInfo }

  TdxGdiFontInfo = class(TdxFontInfo)
  strict private
    FApplyCjkUnderline: Boolean;
    FGdiFontHandle: THandle;
    FCharacterDrawingAbilityTable: TdxCharacterDrawingAbilityTable;
    FCjkUnderlinePosition: Integer;
    FCjkUnderlineSize: Integer;
    FPanose: TPanose;
    FTrueType: Boolean;
    FUnicodeSubrangeBits: TdxUnicodeSubrangeBits;
    FUnicodeSubrangeBitsCalculated: Boolean;
    FUseGetGlyphIndices: Boolean;
    function CalculateFontCharsetCore(AMeasurer: TdxFontInfoMeasurer): Integer;
    procedure CalculateUnderlineAndStrikeoutParameters(const AOutlineTextmetric: TOutlineTextmetric);
  protected
    function CalculateCanDrawCharacter(AUnicodeRangeInfo: TdxUnicodeRangeInfo; ACanvas: TCanvas; ACharacter: Char): Boolean; virtual;
    procedure CalculateCJKFontParameters(AOutlineTextMetric: POutlineTextmetric);
    procedure CalculateFontParameters(AMeasurer: TdxFontInfoMeasurer; AAllowCjkCorrection: Boolean); override;
    procedure CalculateFontVerticalParameters(AMeasurer: TdxFontInfoMeasurer; AAllowCjkCorrection: Boolean); override;
    function CalculateSupportedUnicodeSubrangeBits(AUnicodeRangeInfo: TdxUnicodeRangeInfo; ACanvas: TCanvas): TdxGdiUnicodeSubrangeBits; virtual;
    procedure CreateFont(AMeasurer: TdxFontInfoMeasurer; const AName: string; ASize: Integer; const AFontStyle: TFontStyles); override;
    procedure Initialize(AMeasurer: TdxFontInfoMeasurer); override;
  public
    destructor Destroy; override;

    procedure CalculateSuperscriptOffset(ABaseFontInfo: TdxFontInfo); override;
    procedure CalculateSubscriptOffset(ABaseFontInfo: TdxFontInfo); override;
    function CalculateFontSizeInLayoutUnits(AFontUnit: TdxGraphicUnit; AUnitConverter: TdxDocumentLayoutUnitConverter): Single;
    function CalculateFontCharset(AMeasurer: TdxFontInfoMeasurer): Integer; override;
    function CanDrawCharacter(AUnicodeRangeInfo: TdxUnicodeRangeInfo; ACanvas: TCanvas; ACharacter: Char): Boolean; virtual;
    class function GetFontUnicodeRanges(ADC: HDC; AFont: HFONT): TdxFontCharacterRangeArray; static;

    property GdiFontHandle: THandle read FGdiFontHandle;
    property Panose: TPanose read FPanose;
    property UseGetGlyphIndices: Boolean read FUseGetGlyphIndices;
  end;

  { TdxGdiFontInfoMeasurer }

  TdxGdiFontInfoMeasurer = class(TdxFontInfoMeasurer)
  strict private
    FDpi: Single;
    FMeasureGraphics: TCanvas;
    function CreateMeasureGraphics: TCanvas;
  strict protected
    procedure Initialize; override;
  public
    constructor Create(AUnitConverter: TdxDocumentLayoutUnitConverter); override;
    destructor Destroy; override;
    function MeasureCharacterWidthF(ACharacter: Char; AFontInfo: TdxFontInfo): Single; override;
    function MeasureString(const AText: string; AFontInfo: TdxFontInfo): Size; override;
    function MeasureMaxDigitWidthF(AFontInfo: TdxFontInfo): Single; override;

    property MeasureGraphics: TCanvas read FMeasureGraphics;
  end;

  { TdxGdiFontHelper }

  TdxGdiFontHelper = class
{$REGION 'public types'}
  public const
    CJK_CODEPAGE_BITS =
      (1 shl 17) or
      (1 shl 18) or
      (1 shl 19) or
      (1 shl 20) or
      (1 shl 21);
  public type
    TFontMetrics = record
      OutlineTextmetric: TOutlineTextmetric;
      Ascent: Word;
      Descent: Word;
      ExternalLeading: SmallInt;
      LineSpacing: Word;
      IsCJKFont: Boolean;
      NeedScale: Boolean;
      HasVDMXTable: Boolean;
      UseGetGlyphIndices: Boolean;
    end;
    PFontMetrics = ^TFontMetrics;

    TT_OS2_V2 = packed record
      version: Word;
      xAvgCharWidth: SmallInt;
      usWeightClass: Word;
      usWidthClass: Word;
      fsType: SmallInt;
      ySubscriptXSize: SmallInt;
      ySubscriptYSize: SmallInt;
      ySubscriptXOffset: SmallInt;
      ySubscriptYOffset: SmallInt;
      ySuperscriptXSize: SmallInt;
      ySuperscriptYSize: SmallInt;
      ySuperscriptXOffset: SmallInt;
      ySuperscriptYOffset: SmallInt;
      yStrikeoutSize: SmallInt;
      yStrikeoutPosition: SmallInt;
      sFamilyClass: SmallInt;
      panose: array[0..9] of Byte;
      ulUnicodeRange1: ULONG;
      ulUnicodeRange3: ULONG;
      ulUnicodeRange2: ULONG;
      ulUnicodeRange4: ULONG;
      achVendID: array[0..3] of AnsiChar;
      fsSelection: Word;
      usFirstCharIndex: Word;
      usLastCharIndex: Word;
      sTypoAscender: SmallInt;
      sTypoDescender: SmallInt;
      sTypoLineGap: SmallInt;
      usWinAscent: Word;
      usWinDescent: Word;
      ulCodePageRange1: ULONG;
      ulCodePageRange2: ULONG;
      sxHeight: SmallInt;
      sCapHeight: SmallInt;
      usDefaultChar: Word;
      usBreakChar: Word;
      usMaxContext: Word;
      usLowerOpticalPointSize: Word;
      usUpperOpticalPointSize: Word;
    end;

    TT_HHEA = packed record
      Version: Cardinal;
      Ascender: SmallInt;
      Descender: SmallInt;
      LineGap: SmallInt;
      advanceWidthMax: word;
      minLeftSideBearing: SmallInt;
      minRightSideBearing: SmallInt;
      xMaxExtent: SmallInt;
      caretSlopeRise: SmallInt;
      caretSlopeRun: SmallInt;
      caretOffset: SmallInt;
      reserved: array[0..3] of SmallInt;
      metricDataFormat: SmallInt;
      numberOfHMetrics: word;
    end;
{$ENDREGION}
  strict private class var
    FDefaultCharSet: Byte;
    FFontQuality: Byte;
  strict private
    class constructor Initialize;
    class function CalculateActualFontQuality: Byte; static;
    class procedure GetDefaultFontMetrics(ADC: HDC; AFontMetrics: PFontMetrics); static;
    class function GetOpenTypeFontMetrics(ADC: HDC; AFontMetrics: PFontMetrics): Boolean; static;
    class function IsFontInstalled(AEnumLogFont: PEnumLogFontEx; ANewTextMetrics: PNewTextMetricEx;
      AFontType: DWORD; AData: LPARAM): Integer; stdcall; static;
    class function CheckFontStyles(AEnumLogFont: PEnumLogFontEx; ANewTextMetrics: PNewTextMetricEx;
      AFontType: DWORD; AData: LPARAM): Integer; stdcall; static;
    class function SwapBytes(AValue: Word): Word; static;
  public
    class function CreateFont(ADC: HDC; const AFontName: string; ASize: Single; AFontStyle: TFontStyles;
      ASizeScale: Single; out ARealFontName: string): HFONT; static;
    class function GetFontMetrics(ADC: HDC; AFont: HFONT; AFontMetrics: PFontMetrics): Boolean; static;
    class function GetFontStyles(const AFontFamilyName: string): TdxSupportedFontStylesInfo; static;

    class property DefaultCharSet: Byte read FDefaultCharSet;
    class property FontQuality: Byte read FFontQuality;
  end;

implementation

uses
  Math, cxGeometry, dxCore, dxTypeHelpers,
  dxRichEdit.Utils.Graphics;

{ TGdiFontHelper }

class constructor TdxGdiFontHelper.Initialize;
begin
  FDefaultCharSet := GetDefFontCharset;
  FFontQuality := CalculateActualFontQuality;
end;

class function TdxGdiFontHelper.SwapBytes(AValue: Word): Word;
begin
  Result := (AValue shr 8) or Word(AValue shl 8);
end;

class function TdxGdiFontHelper.IsFontInstalled(
  AEnumLogFont: PEnumLogFontEx;
  ANewTextMetrics: PNewTextMetricEx;
  AFontType: DWORD;
  AData: LParam): Integer; stdcall  ;
var
  ALogFont: PLogFont absolute AData;
begin
  if AFontType and RASTER_FONTTYPE <> 0 then
    Result := 1
  else
  begin
    ALogFont^ := AEnumLogFont.elfLogFont;
    Move(AEnumLogFont.elfFullName, ALogFont.lfFaceName, LF_FACESIZE * SizeOf(Char));
    Result := 0;
  end;
end;


class function TdxGdiFontHelper.GetFontMetrics(ADC: HDC; AFont: HFONT; AFontMetrics: PFontMetrics): Boolean;
var
  AOldFont: HFONT;
begin
  AOldFont := SelectObject(ADC, AFont);
  try
    Result := GetOpenTypeFontMetrics(ADC, AFontMetrics);
    if not Result then
      GetDefaultFontMetrics(ADC, AFontMetrics);
  finally
    SelectObject(ADC, AOldFont);
  end;
end;

class function TdxGdiFontHelper.CheckFontStyles(AEnumLogFont: PEnumLogFontEx; ANewTextMetrics: PNewTextMetricEx;
  AFontType: DWORD; AData: LPARAM): Integer;
const
  AllStyles = [
    TdxSupportedFontStyle.Regular,
    TdxSupportedFontStyle.Bold,
    TdxSupportedFontStyle.Italic,
    TdxSupportedFontStyle.Underline,
    TdxSupportedFontStyle.Strikeout,
    TdxSupportedFontStyle.BoldItalic
  ];
var
  AFontStylesInfo: ^TdxSupportedFontStylesInfo absolute AData;
  ACurrentFontStyles: TdxSupportedFontStyles;
  I: TdxSupportedFontStyle;
begin
  Result := 1;
  if ANewTextMetrics = nil then
    Exit;

  ACurrentFontStyles := [];
  if (ANewTextMetrics.ntmTm.tmItalic <> 0) and (ANewTextMetrics.ntmTm.tmWeight >= FW_BOLD) then
    Include(ACurrentFontStyles, TdxSupportedFontStyle.BoldItalic)
  else
  begin
    if ANewTextMetrics.ntmTm.tmWeight >= FW_BOLD then
      Include(ACurrentFontStyles, TdxSupportedFontStyle.Bold);
    if ANewTextMetrics.ntmTm.tmItalic <> 0 then
      Include(ACurrentFontStyles, TdxSupportedFontStyle.Italic);
  end;

  if ANewTextMetrics.ntmTm.tmUnderlined <> 0 then
    Include(ACurrentFontStyles, TdxSupportedFontStyle.Underline);
  if ANewTextMetrics.ntmTm.tmStruckOut <> 0 then
    Include(ACurrentFontStyles, TdxSupportedFontStyle.Strikeout);

  if ACurrentFontStyles = [] then
    Include(ACurrentFontStyles, TdxSupportedFontStyle.Regular);

  for I := Low(TdxSupportedFontStyle) to High(TdxSupportedFontStyle) do
    if I in ACurrentFontStyles then
      AFontStylesInfo.NativeStyles[I] := True;

  AFontStylesInfo.SupportedStyles := AFontStylesInfo.SupportedStyles + ACurrentFontStyles;
end;

class function TdxGdiFontHelper.GetFontStyles(const AFontFamilyName: string): TdxSupportedFontStylesInfo;
var
  ALogFont: TLogFont;
  ADC: HDC;
begin
  ZeroMemory(@Result, SizeOf(Result));
  ADC := CreateCompatibleDC(0);
  try
    ZeroMemory(@ALogFont, SizeOf(ALogFont));
    ALogFont.lfCharSet := DEFAULT_CHARSET;
    Move(AFontFamilyName[1], ALogFont.lfFaceName, Min(LF_FACESIZE, Length(AFontFamilyName)) * SizeOf(Char));
    // if not EnumFontFamiliesEx(ADC, ALogFont, @CheckFontStyles, LPARAM(@Result), 0) then
    EnumFontFamiliesEx(ADC, ALogFont, @CheckFontStyles, LPARAM(@Result), 0);
      Result.SupportedStyles := [TdxSupportedFontStyle.Regular];
  finally
    DeleteDC(ADC);
  end;
end;

class procedure TdxGdiFontHelper.GetDefaultFontMetrics(ADC: HDC; AFontMetrics: PFontMetrics);
var
  ATextMetric: TTextMetric;
begin
  GetTextMetricsW(ADC, ATextMetric);
  AFontMetrics.Ascent := ATextMetric.tmAscent;
  AFontMetrics.Descent := ATextMetric.tmDescent;
  AFontMetrics.ExternalLeading := ATextMetric.tmExternalLeading;
end;

class function TdxGdiFontHelper.GetOpenTypeFontMetrics(ADC: HDC; AFontMetrics: PFontMetrics): Boolean;
const
  MS_OS2_TAG  = $322F534F;
  MS_HHEA_TAG = $61656868;
  MS_VDMX_TAG = $584D4456;
var
  AOutlineTextmetric: TOutlineTextmetric;
  AOs2Metrics: TT_OS2_V2;
  AHorizontalHeader: TT_HHEA;
  AFontSignature: TFontSignature;
  ASize: DWORD;
  ALineGap: Word;
begin
  AFontMetrics.HasVDMXTable := False;
  AFontMetrics.NeedScale := False;
  AFontMetrics.UseGetGlyphIndices := True;

  AOutlineTextmetric.otmSize := SizeOf(AOutlineTextmetric);
  if GetOutlineTextMetrics(ADC, AOutlineTextmetric.otmSize, @AOutlineTextmetric) = 0 then
    Exit(False);

  AFontMetrics.IsCJKFont :=
    (Integer(GetTextCharsetInfo(ADC, @AFontSignature, 0)) <> DEFAULT_CHARSET) and
    (AFontSignature.fsCsb[0] and CJK_CODEPAGE_BITS <> 0);

  AFontMetrics.OutlineTextmetric := AOutlineTextmetric;
  AFontMetrics.HasVDMXTable := GetFontData(ADC, MS_HHEA_TAG, 0, nil, 0) > 0;

  ZeroMemory(@AHorizontalHeader, SizeOf(AHorizontalHeader));
  if GetFontData(ADC, MS_HHEA_TAG, 0, @AHorizontalHeader, SizeOf(AHorizontalHeader)) = GDI_ERROR then
    Exit(False);

  AFontMetrics.Ascent  := SwapBytes(AHorizontalHeader.Ascender);
  AFontMetrics.Descent := -SwapBytes(AHorizontalHeader.Descender);
  ALineGap := SwapBytes(AHorizontalHeader.LineGap);
  AFontMetrics.ExternalLeading := ALineGap;
  AFontMetrics.LineSpacing := AFontMetrics.Ascent + AFontMetrics.Descent + ALineGap;

  ASize := GetFontData(ADC, MS_OS2_TAG, 0, nil, 0);
  if ASize = GDI_ERROR then
    Exit(False);

  AFontMetrics.UseGetGlyphIndices := False;
  AFontMetrics.NeedScale := True;
  if AFontMetrics.Ascent + AFontMetrics.Descent <> 0 then
    Exit(True);

  if ASize > SizeOf(AOs2Metrics) then
    ASize := SizeOf(AOs2Metrics);
  ZeroMemory(@AOs2Metrics, SizeOf(AOs2Metrics));
  if GetFontData(ADC, MS_OS2_TAG, 0, @AOs2Metrics, ASize) <> ASize then
    Exit(False);
  AFontMetrics.Ascent := SwapBytes(AOs2Metrics.usWinAscent);
  AFontMetrics.Descent := SwapBytes(AOs2Metrics.usWinDescent);
  if AFontMetrics.Ascent + AFontMetrics.Descent = 0 then
  begin
    AFontMetrics.Ascent := SwapBytes(AOs2Metrics.sTypoAscender);
    AFontMetrics.Descent := SwapBytes(AOs2Metrics.sTypoDescender);
  end;
  ALineGap := SwapBytes(AOs2Metrics.sTypoLineGap);
  AFontMetrics.ExternalLeading := Max(0, ALineGap - ((AFontMetrics.Ascent + AFontMetrics.Descent) - (AFontMetrics.Ascent - AFontMetrics.Descent)));
  Result := True;
end;

class function TdxGdiFontHelper.CalculateActualFontQuality: Byte;
const
  CLEARTYPE_QUALITY = 5;
  CLEARTYPE_NATURAL_QUALITY = 6;
begin
  if IsWinVistaOrLater then
    Result := CLEARTYPE_NATURAL_QUALITY
  else
    Result := CLEARTYPE_QUALITY
end;

class function TdxGdiFontHelper.CreateFont(ADC: HDC; const AFontName: string; ASize: Single; AFontStyle: TFontStyles;
  ASizeScale: Single; out ARealFontName: string): HFONT;
const
  DefaultSubstitutionFontName: string = 'Times New Roman';
  CLIP_DFA_DISABLE	= 64;
var
  ALogFont: TLogFont;
begin

  ZeroMemory(@ALogFont, SizeOf(ALogFont));
  ALogFont.lfCharSet := DEFAULT_CHARSET;
  Move(AFontName[1], ALogFont.lfFaceName, Min(LF_FACESIZE, Length(AFontName)) * SizeOf(Char));
 // if not EnumFontFamiliesEx(ADC, ALogFont, @IsFontInstalled, LPARAM(@ALogFont), 0) then
  if EnumFontFamiliesEx(ADC, ALogFont, @IsFontInstalled, LPARAM(@ALogFont), 0)=0 then
  begin
    ARealFontName := PChar(@ALogFont.lfFaceName);
    ALogFont.lfCharset := DEFAULT_CHARSET;
    ALogFont.lfHeight := -Round(ASize / ASizeScale);
    ALogFont.lfWidth := 0;
    ALogFont.lfEscapement := 0;
    ALogFont.lfOrientation := 0;
    ALogFont.lfWeight := IfThen(fsBold in AFontStyle, FW_BOLD, FW_NORMAL);
    ALogFont.lfItalic := Byte(fsItalic in AFontStyle);
    ALogFont.lfUnderline := Byte(fsUnderline in AFontStyle);
    ALogFont.lfStrikeOut := Byte(fsStrikeOut in AFontStyle);
    ALogFont.lfOutPrecision := OUT_SCREEN_OUTLINE_PRECIS;
    ALogFont.lfClipPrecision := CLIP_DFA_DISABLE;
    ALogFont.lfQuality := FontQuality;
    ALogFont.lfPitchAndFamily := DEFAULT_PITCH;

    Result := CreateFontIndirectW(ALogFont);
  end
  else
  begin
    ARealFontName := AFontName;
    ZeroMemory(@ALogFont, SizeOf(ALogFont));
    Move(DefaultSubstitutionFontName[1], ALogFont.lfFaceName, Min(LF_FACESIZE, Length(DefaultSubstitutionFontName)) * SizeOf(Char));
    ALogFont.lfCharset := DEFAULT_CHARSET;
    ALogFont.lfHeight := -Trunc(ASize / ASizeScale);
    ALogFont.lfWidth := 0;
    ALogFont.lfEscapement := 0;
    ALogFont.lfOrientation := 0;
    ALogFont.lfWeight := IfThen(fsBold in AFontStyle, FW_BOLD, FW_NORMAL);
    ALogFont.lfItalic := Byte(fsItalic in AFontStyle);
    ALogFont.lfUnderline := Byte(fsUnderline in AFontStyle);
    ALogFont.lfStrikeOut := Byte(fsStrikeOut in AFontStyle);
    ALogFont.lfOutPrecision := OUT_SCREEN_OUTLINE_PRECIS;
    ALogFont.lfClipPrecision := CLIP_DFA_DISABLE;
    ALogFont.lfQuality := FontQuality;
    ALogFont.lfPitchAndFamily := DEFAULT_PITCH;

    Result := CreateFontIndirectW(ALogFont);
  end;
end;

{ TdxGdiUnicodeSubrangeBits }

procedure TdxGdiUnicodeSubrangeBits.Clear;
begin
  Data[0] := 0;
  Data[1] := 0;
  Data[2] := 0;
  Data[3] := 0;
end;

function TdxGdiUnicodeSubrangeBits.GetBit(AIndex: Integer): Boolean;
begin
  Result := Data[AIndex div BitPerDWORD] and (1 shl (AIndex mod BitPerDWORD)) <> 0;
end;

procedure TdxGdiUnicodeSubrangeBits.SetBit(AIndex: Integer;
  const Value: Boolean);
begin
  if Value then
    Data[AIndex div BitPerDWORD] := Data[AIndex div BitPerDWORD] or (1 shl (AIndex mod BitPerDWORD))
  else
    Data[AIndex div BitPerDWORD] := Data[AIndex div BitPerDWORD] and not DWORD((1 shl (AIndex mod BitPerDWORD)));
end;

{ TdxCharacterDrawingAbilityTable }

constructor TdxCharacterDrawingAbilityTable.Create;
begin
  FDrawingAbility := TBits.Create;
  FDrawingAbility.Size := $0600;
  FCalculated := TBits.Create;
  FCalculated.Size := $0600;
end;

destructor TdxCharacterDrawingAbilityTable.Destroy;
begin
  FCalculated.Free;
  FDrawingAbility.Free;
  inherited Destroy;
end;

function TdxCharacterDrawingAbilityTable.GetDrawingAbility(ACharacter: Char;
  out AAbility: Boolean): Boolean;
begin
  if Ord(ACharacter) >= FCalculated.Size then
  begin
    FCalculated.Size := Ord(ACharacter) + 1;
    FDrawingAbility.Size := Ord(ACharacter) + 1;
  end;
  Result := FCalculated[Ord(ACharacter)];
  if Result then
    AAbility := FDrawingAbility[Ord(ACharacter)];
end;

procedure TdxCharacterDrawingAbilityTable.SetDrawingAbility(ACharacter: Char;
  AValue: Boolean);
begin
  FCalculated[Ord(ACharacter)] := True;
  FDrawingAbility[Ord(ACharacter)] := AValue;
end;

{ TdxGdiFontInfo }

destructor TdxGdiFontInfo.Destroy;
begin
  FCharacterDrawingAbilityTable.Free;
  if GdiFontHandle <> 0 then
    DeleteObject(GdiFontHandle);
  inherited Destroy;
end;

procedure TdxGdiFontInfo.CalculateSuperscriptOffset(ABaseFontInfo: TdxFontInfo);
var
  Y, AResult: Integer;
begin
  AResult := ABaseFontInfo.SuperscriptOffset.Y;
  Y := ABaseFontInfo.AscentAndFree - AscentAndFree + AResult;
  if Y < 0 then
    Dec(AResult, Y);
  FSuperscriptOffset := TPoint.Create(SuperscriptOffset.X, AResult);
end;

procedure TdxGdiFontInfo.CalculateSubscriptOffset(ABaseFontInfo: TdxFontInfo);
var
  Y, AMaxOffset: Integer;
begin
  Y := ABaseFontInfo.SubscriptOffset.Y;
  AMaxOffset := ABaseFontInfo.LineSpacing - LineSpacing + AscentAndFree - ABaseFontInfo.AscentAndFree;
  if Y > AMaxOffset then
    Y := AMaxOffset;
  FSubscriptOffset := TPoint.Create(SubscriptOffset.X, Y);
end;

procedure TdxGdiFontInfo.Initialize(AMeasurer: TdxFontInfoMeasurer);
begin
  FCharacterDrawingAbilityTable := TdxCharacterDrawingAbilityTable.Create;
end;

procedure TdxGdiFontInfo.CreateFont(AMeasurer: TdxFontInfoMeasurer;
  const AName: string; ASize: Integer; const AFontStyle: TFontStyles);
var
  AGdiMeasurer: TdxGdiFontInfoMeasurer absolute AMeasurer;
begin
  FGdiFontHandle := TdxGdiFontHelper.CreateFont(AGdiMeasurer.MeasureGraphics.Handle,
    AName, FSizeInPoints, AFontStyle, AMeasurer.UnitConverter.FontSizeScale, FFamilyName);
end;

procedure TdxGdiFontInfo.CalculateCJKFontParameters(AOutlineTextMetric: POutlineTextmetric);
var
  AHalfTmpExtLeading, AOtherHalfTmpExtLeading, ANewLineSpacing, ADelta, AAscentWithoutFree: Integer;
begin
  if (AOutlineTextMetric.otmfsSelection and 128) <> 0 then
  begin
    FAscent := AOutlineTextMetric.otmAscent;
    FDescent := -AOutlineTextMetric.otmDescent;
    FLineSpacing := Ascent + Descent + Integer(AOutlineTextMetric.otmLineGap);
    FDrawingOffset := AOutlineTextMetric.otmTextMetrics.tmAscent - Ascent;
  end
  else
  begin
    FCjkUnderlinePosition := Ceil(Ascent * 1.15 + Descent * 0.85);
    ANewLineSpacing := Ceil(1.3 * (Ascent + Descent));
    ADelta := ANewLineSpacing - (Ascent + Descent);
    AHalfTmpExtLeading := ADelta div 2;
    AOtherHalfTmpExtLeading := ADelta - AHalfTmpExtLeading;
    Inc(FAscent, AHalfTmpExtLeading);
    Inc(FDescent, AOtherHalfTmpExtLeading);
    FLineSpacing := ANewLineSpacing;
    FCjkUnderlinePosition := FCjkUnderlinePosition - Ascent;
    FApplyCjkUnderline := True;
    AAscentWithoutFree := LineSpacing - Descent;
    FDrawingOffset := AOutlineTextMetric.otmTextMetrics.tmAscent - AAscentWithoutFree;
  end;
end;

function MulDivRound(AValue, ANumerator, ADenominator: Integer): Integer;
begin
  Result := Trunc((Int64(AValue) * ANumerator + ADenominator / 2) / ADenominator);
end;

procedure TdxGdiFontInfo.CalculateFontVerticalParameters(AMeasurer: TdxFontInfoMeasurer; AAllowCjkCorrection: Boolean);
var
  AGdiMeasurer: TdxGdiFontInfoMeasurer absolute AMeasurer;
  ASizeInUnits, ARatio: Single;
  AFontMetrics: TdxGdiFontHelper.TFontMetrics;
  AOutlineTextMetric: POutlineTextmetric;
  AExternalLeading, AFixRatio, ARoundedAscent: Integer;
begin
  TdxGdiFontHelper.GetFontMetrics(AGdiMeasurer.MeasureGraphics.Handle, FGdiFontHandle, @AFontMetrics);
  AOutlineTextMetric := @AFontMetrics.OutlineTextmetric;
  FUseGetGlyphIndices := AFontMetrics.UseGetGlyphIndices;
  FIsCJKFont := AFontMetrics.IsCJKFont and AAllowCjkCorrection;
  if AFontMetrics.NeedScale then
  begin
    ASizeInUnits := FSizeInPoints / AMeasurer.UnitConverter.FontSizeScale;
    FPanose := AOutlineTextMetric.otmPanoseNumber;
    ARatio := ASizeInUnits / AOutlineTextMetric.otmEMSquare;
    FCjkUnderlineSize := Ceil(SizeInPoints / AMeasurer.UnitConverter.FontSizeScale * 51 / 1024);
    FAscent := Ceil(AFontMetrics.Ascent * ARatio);
    FDescent := Ceil(AFontMetrics.Descent * ARatio);
    FLineSpacing := Ceil(AFontMetrics.LineSpacing * ARatio);
    if not IsCJKFont then
    begin
      AFixRatio := Trunc(ARatio * 65536 + 0.5);
      ARoundedAscent := MulDivRound(AFontMetrics.Ascent, AFixRatio, 65536);
      GdiOffset := AOutlineTextMetric.otmTextMetrics.tmAscent - ARoundedAscent;
      if AFontMetrics.HasVDMXTable then
        GdiOffset := Min(GdiOffset, 0);
      FDrawingOffset := AOutlineTextMetric.otmTextMetrics.tmAscent - Ascent;
    end;
  end
  else
  begin
    FAscent := AFontMetrics.Ascent;
    FDescent := AFontMetrics.Descent;
    AExternalLeading := AFontMetrics.ExternalLeading;
    FLineSpacing := AExternalLeading + Ascent + Descent;
  end;

  if IsCJKFont then
    CalculateCJKFontParameters(AOutlineTextMetric);

  CalculateUnderlineAndStrikeoutParameters(AOutlineTextMetric^);
end;

function TdxGdiFontInfo.CalculateFontSizeInLayoutUnits(AFontUnit: TdxGraphicUnit; AUnitConverter: TdxDocumentLayoutUnitConverter): Single;
begin
  case AFontUnit of
    guDocument:
      Result := AUnitConverter.DocumentsToFontUnitsF(FSizeInPoints);
    guInch:
      Result := AUnitConverter.InchesToFontUnitsF(FSizeInPoints);
    guMillimeter:
      Result := AUnitConverter.MillimetersToFontUnitsF(FSizeInPoints);
    else
      Result := AUnitConverter.PointsToFontUnitsF(FSizeInPoints);
  end;
end;

function TdxGdiFontInfo.CalculateFontCharset(AMeasurer: TdxFontInfoMeasurer): Integer;
begin
  Result := CalculateFontCharsetCore(AMeasurer);
end;

function TdxGdiFontInfo.CalculateFontCharsetCore(AMeasurer: TdxFontInfoMeasurer): Integer;
var
  AGdiMeasurer: TdxGdiFontInfoMeasurer absolute AMeasurer;
  ADC: HDC;
  AOldFontHandle: HFONT;
begin
  ADC := AGdiMeasurer.MeasureGraphics.Handle;
  AOldFontHandle := SelectObject(ADC, GdiFontHandle);
  try
    Result := GetTextCharset(ADC);
  finally
    SelectObject(ADC, AOldFontHandle);
  end;
end;

procedure TdxGdiFontInfo.CalculateUnderlineAndStrikeoutParameters(const AOutlineTextmetric: TOutlineTextmetric);
var
  AOffset: TPoint;
begin
  if not FApplyCjkUnderline then
  begin
    FUnderlinePosition := -AOutlineTextmetric.otmsUnderscorePosition;
    FUnderlineThickness := AOutlineTextmetric.otmsUnderscoreSize;
  end
  else
  begin
    FUnderlinePosition := FCjkUnderlinePosition;
    FUnderlineThickness := FCjkUnderlineSize;
  end;
  FStrikeoutPosition := AOutlineTextmetric.otmsStrikeoutPosition;
  FStrikeoutThickness := Integer(AOutlineTextmetric.otmsStrikeoutSize);
  FSubscriptSize := TSize(AOutlineTextmetric.otmptSubscriptSize);
  FSubscriptOffset := AOutlineTextmetric.otmptSubscriptOffset;
  FSuperscriptOffset := AOutlineTextmetric.otmptSuperscriptOffset;
  AOffset := SuperscriptOffset;
  AOffset.Y := -AOffset.Y;
  FSuperscriptOffset := AOffset;
  FSuperscriptSize := TSize(AOutlineTextmetric.otmptSuperscriptSize);
end;

function TdxGdiFontInfo.CanDrawCharacter(AUnicodeRangeInfo: TdxUnicodeRangeInfo;
  ACanvas: TCanvas; ACharacter: Char): Boolean;
begin
  if not FCharacterDrawingAbilityTable.GetDrawingAbility(ACharacter, Result) then
  begin
    Result := CalculateCanDrawCharacter(AUnicodeRangeInfo, ACanvas, ACharacter);
    FCharacterDrawingAbilityTable.SetDrawingAbility(ACharacter, Result);
  end;
end;

function TdxGdiFontInfo.CalculateCanDrawCharacter(AUnicodeRangeInfo: TdxUnicodeRangeInfo;
  ACanvas: TCanvas; ACharacter: Char): Boolean;
var
  AUnicodeSubrangeBits: TdxGdiUnicodeSubrangeBits;
  AUnicodeSubRange: PdxUnicodeSubrange;
begin
  AUnicodeSubRange := AUnicodeRangeInfo.LookupSubrange(ACharacter);
  if AUnicodeSubRange <> nil then
  begin
    Assert(AUnicodeSubRange.Bit < 126, '');
    AUnicodeSubrangeBits := CalculateSupportedUnicodeSubrangeBits(AUnicodeRangeInfo, ACanvas);
    Result := AUnicodeSubrangeBits.Bits[AUnicodeSubRange.Bit];
  end
  else
    Result := False;
end;

procedure TdxGdiFontInfo.CalculateFontParameters(AMeasurer: TdxFontInfoMeasurer; AAllowCjkCorrection: Boolean);
begin
  inherited CalculateFontParameters(AMeasurer, AAllowCjkCorrection);
end;

class function TdxGdiFontInfo.GetFontUnicodeRanges(ADC: HDC; AFont: HFONT): TdxFontCharacterRangeArray;
var
  I, AGlyphCount, ASize: Integer;
  AGlyphSet: PGlyphSet;
  AOldFont: HFONT;
begin
  if AFont <> 0 then
    AOldFont := SelectObject(ADC, AFont)
  else
    AOldFont := 0;
  try
    ASize := Windows.GetFontUnicodeRanges(ADC, nil);
    if ASize = 0 then
      Exit(nil);
    GetMem(AGlyphSet, ASize);
    try
      Windows.GetFontUnicodeRanges(ADC, AGlyphSet);
      AGlyphCount := AGlyphSet.cRanges;
      SetLength(Result, AGlyphCount + 2);
      for I := 0 to AGlyphCount - 1 do
        Result[I].CopyFrom(AGlyphSet.ranges[I]);

      Result[AGlyphCount].CopyFrom(0, 255);
      Result[AGlyphCount + 1].CopyFrom(61440, 61695);
    finally
      FreeMem(AGlyphSet);
    end;
  finally
    if AFont <> 0 then
      SelectObject(ADC, AOldFont);
  end;
end;

function TdxGdiFontInfo.CalculateSupportedUnicodeSubrangeBits(
  AUnicodeRangeInfo: TdxUnicodeRangeInfo; ACanvas: TCanvas): TdxGdiUnicodeSubrangeBits;
var
  ADC, AOldFont: THandle;
  AFontSignature: TFontSignature;
  ALocaleSignature: TLocaleSignature;
begin
  if not FUnicodeSubrangeBitsCalculated then
  begin
    if not FTrueType then
    begin
      GetLocaleInfoW(LOCALE_INVARIANT, LOCALE_FONTSIGNATURE, PChar(@ALocaleSignature),
        SizeOf(ALocaleSignature) div SizeOf(Char));
      Move(ALocaleSignature.lsUsb, FUnicodeSubrangeBits, SizeOf(FUnicodeSubrangeBits));
    end;
    ADC := ACanvas.Handle;
    AOldFont := SelectObject(ADC, GdiFontHandle);
    try
      if Integer(GetTextCharsetInfo(ADC, @AFontSignature, 0)) = DEFAULT_CHARSET then
        Result.Clear
      else
        Move(AFontSignature.fsUsb, FUnicodeSubrangeBits, SizeOf(FUnicodeSubrangeBits));
    finally
      SelectObject(ADC, AOldFont);
    end;
    FUnicodeSubrangeBitsCalculated := True;
  end;
  Result.Data := FUnicodeSubrangeBits;
end;

{ GdiFontInfoMeasurer }

constructor TdxGdiFontInfoMeasurer.Create(
  AUnitConverter: TdxDocumentLayoutUnitConverter);
begin
  inherited Create(AUnitConverter);
  FDpi := AUnitConverter.Dpi;
end;

destructor TdxGdiFontInfoMeasurer.Destroy;
begin
  DeleteDC(FMeasureGraphics.Handle);
  FMeasureGraphics.Handle := 0;
  FMeasureGraphics.Free;
  inherited Destroy;
end;

procedure TdxGdiFontInfoMeasurer.Initialize;
begin
  FMeasureGraphics := CreateMeasureGraphics;
end;

function TdxGdiFontInfoMeasurer.CreateMeasureGraphics: TCanvas;
begin
  Result := TCanvas.Create;
  Result.Handle := CreateCompatibleDC(0);
end;

function TdxGdiFontInfoMeasurer.MeasureCharacterWidthF(ACharacter: Char; AFontInfo: TdxFontInfo): Single;
var
  ACharacterSize: TSize;
  AGdiFontInfo: TdxGdiFontInfo absolute AFontInfo;
begin
  SelectObject(MeasureGraphics.Handle, AGdiFontInfo.GdiFontHandle);
  ACharacterSize := MeasureGraphics.TextExtent(ACharacter);
  Result := ACharacterSize.cx;
end;

function TdxGdiFontInfoMeasurer.MeasureString(const AText: string; AFontInfo: TdxFontInfo): Size;
var
  AGdiFontInfo: TdxGdiFontInfo absolute AFontInfo;
begin
  SelectObject(MeasureGraphics.Handle, AGdiFontInfo.GdiFontHandle);
  Result := MeasureGraphics.TextExtent(AText);
end;

function TdxGdiFontInfoMeasurer.MeasureMaxDigitWidthF(AFontInfo: TdxFontInfo): Single;
type
  TDigitWidths = packed array[0..9] of TABCFloat;
var
  I: Integer;
  ADigitWidths: TDigitWidths;
  AFont, ADC: THandle;
  AGdiFontInfo: TdxGdiFontInfo absolute AFontInfo;
  AWidth: Single;
begin
  AWidth := 0;
  ADC := MeasureGraphics.Handle;
  AFont := AGdiFontInfo.GdiFontHandle;
  SelectObject(ADC, AFont);
  if GetCharABCWidthsFloat(ADC, Ord('0'), Ord('9'), ADigitWidths) then
  begin
    for I := Low(ADigitWidths) to High(ADigitWidths) do
      with ADigitWidths[I] do
        AWidth := Max(AWidth, abcfA + abcfB + abcfC);
  end;
  if AWidth > 0 then
    Result := UnitConverter.PixelsToLayoutUnitsF(AWidth, FDpi)
  else
    Result := 0;
end;

end.

