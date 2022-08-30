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

unit dxRichEdit.Platform.Font;

interface

{$I cxVer.inc}
{$I dxRichEditControl.inc}

uses
  Types, Windows, Graphics, dxCoreClasses, Classes,
  Generics.Defaults, Generics.Collections,
  dxRichEdit.DocumentLayout.UnitConverter;

type
  TdxFontInfo = class;
  TdxFontCache = class;
  TdxFontInfoMeasurer = class;

  TdxTextDirection = (
    LeftToRightTopToBottom        = 0,
    TopToBottomRightToLeft        = 1,
    TopToBottomLeftToRightRotated = 2,
    BottomToTopLeftToRight        = 3,
    LeftToRightTopToBottomRotated = 4,
    TopToBottomRightToLeftRotated = 5
  );

  TdxVerticalAlignmentValues = class sealed
  public const
    Top = 0;
    Both = 1;
    Center = 2;
    Bottom = 3;
  end;

  TdxVerticalAlignment = (
    Top = TdxVerticalAlignmentValues.Top,
    Both = TdxVerticalAlignmentValues.Both,
    Center = TdxVerticalAlignmentValues.Center,
    Bottom = TdxVerticalAlignmentValues.Bottom);

  { TdxFontCharacterSet }

  TdxFontCharacterRange = record
    Low: Integer;
    Hi: Integer;
    procedure CopyFrom(ALow, AHi: Integer); overload;
    procedure CopyFrom(const ARange: TWCRange); overload;
  end;

  TdxFontCharacterRangeArray = array of TdxFontCharacterRange;

  TdxFontCharacterSet = class
  private
    FBits: TBits;
    procedure AddRange(const AFontCharacterRange: TdxFontCharacterRange);
  protected
    FPanose: TPanose;
    property Bits: TBits read FBits;
  public
    constructor Create(const ARanges: TdxFontCharacterRangeArray; const APanose: TPanose);
    destructor Destroy; override;
    class function CalculatePanoseDistance(const AFontCharacterSet1, AFontCharacterSet2: TdxFontCharacterSet): Integer;
    function ContainsChar(C: Char): Boolean; inline;
  end;

  TdxSupportedFontStyle = (Regular, Bold, Italic, Underline, Strikeout, BoldItalic);
  TdxSupportedFontStyles = set of TdxSupportedFontStyle;
  TdxSupportedFontStylesInfo = record
    SupportedStyles: TdxSupportedFontStyles;
    NativeStyles: array[TdxSupportedFontStyle] of Boolean;
  end;

  { IdxFontsContainer }

  IdxFontsContainer = interface
    function GetFontCache: TdxFontCache;

    property FontCache: TdxFontCache read GetFontCache;
  end;

  { TdxFontInfo }

  TdxFontInfo = class
  private
    function GetAscentAndFree: Integer;
    function GetBold: Boolean; inline;
    function GetItalic: Boolean; inline;
    function GetUnderline: Boolean; inline;
  strict protected
    FAscent: Integer;
    FBaseFontIndex: Integer;
    FCharset: Integer;
    FDashWidth: Integer;
    FDescent: Integer;
    FDotWidth: Integer;
    FDrawingOffset: Integer;
    FEqualSignWidth: Integer;
    FFamilyName: string;
    FFree: Integer;
    FGdiOffset: Integer;
    FIsCJKFont: Boolean;
    FLineSpacing: Integer;
    FMaxDigitWidth: Single;
    FMiddleDotWidth: Integer;
    FName: string;
    FNonBreakingSpaceWidth: Integer;
    FPilcrowSignWidth: Integer;
    FSizeInPoints: Single;
    FSpaceWidth: Integer;
    FStrikeoutPosition: Integer;
    FStrikeoutThickness: Integer;
    FStyle: TFontStyles;
    FSubscriptOffset: TPoint;
    FSubscriptSize: TSize;
    FSuperscriptOffset: TPoint;
    FSuperscriptSize: TSize;
    FUnderlinePosition: Integer;
    FUnderlineThickness: Integer;
    FUnderscoreWidth: Integer;
    FSupportedStyles: TdxSupportedFontStyles;
    procedure AdjustFontParameters; virtual;
    procedure CalculateFontParameters(AMeasurer: TdxFontInfoMeasurer; AAllowCjkCorrection: Boolean); virtual;
    procedure CalculateFontVerticalParameters(AMeasurer: TdxFontInfoMeasurer; AAllowCjkCorrection: Boolean); virtual; abstract;
    function CalculateMaxDigitWidth(AMeasurer: TdxFontInfoMeasurer): Single;
    function CalculateFontCharset(AMeasurer: TdxFontInfoMeasurer): Integer; virtual; abstract;
    procedure CreateFont(AMeasurer: TdxFontInfoMeasurer; const AName: string; ASize: Integer; const AFontStyle: TFontStyles); virtual; abstract;
    function GetBaseFontInfo(const AContainer: IdxFontsContainer): TdxFontInfo; virtual;
    procedure Initialize(AMeasurer: TdxFontInfoMeasurer); virtual; abstract;
  public
    constructor Create(AMeasurer: TdxFontInfoMeasurer; const AName: string; ADoubleSize: Integer;
      const AFontStyle: TFontStyles; AAllowCjkCorrection: Boolean = True);
    procedure CalculateSuperscriptOffset(ABaseFontInfo: TdxFontInfo); virtual; abstract;
    procedure CalculateSubscriptOffset(ABaseFontInfo: TdxFontInfo); virtual; abstract;
    function GetBaseAscentAndFree(const AContainer: IdxFontsContainer): Integer;
    function GetBaseDescent(const AContainer: IdxFontsContainer): Integer;

    property Ascent: Integer read FAscent;
    property AscentAndFree: Integer read GetAscentAndFree;
    property BaseFontIndex: Integer read FBaseFontIndex write FBaseFontIndex;
    property Bold: Boolean read GetBold;
    property Charset: Integer read FCharset;
    property DashWidth: Integer read FDashWidth ;
    property Descent: Integer read FDescent;
    property DotWidth: Integer read FDotWidth ;
    property DrawingOffset: Integer read FDrawingOffset write FDrawingOffset;
    property EqualSignWidth: Integer read FEqualSignWidth;
    property FamilyName: string read FFamilyName;
    property _Free: Integer read FFree;
    property GdiOffset: Integer read FGdiOffset write FGdiOffset;
    property IsCJKFont: Boolean read FIsCJKFont;
    property Italic: Boolean read GetItalic;
    property LineSpacing: Integer read FLineSpacing;
    property MaxDigitWidth: Single read FMaxDigitWidth;
    property MiddleDotWidth: Integer read FMiddleDotWidth ;
    property Name: string read FName;
    property NonBreakingSpaceWidth: Integer read FNonBreakingSpaceWidth ;
    property PilcrowSignWidth: Integer read FPilcrowSignWidth;
    property SizeInPoints: Single read FSizeInPoints;
    property SpaceWidth: Integer read FSpaceWidth;
    property StrikeoutPosition: Integer read FStrikeoutPosition ;
    property StrikeoutThickness: Integer read FStrikeoutThickness ;
    property Style: TFontStyles read FStyle;
    property SubscriptOffset: TPoint read FSubscriptOffset ;
    property SubscriptSize: TSize read FSubscriptSize;
    property SuperscriptOffset: TPoint read FSuperscriptOffset ;
    property SuperscriptSize: TSize read FSuperscriptSize;
    property SupportedStyles: TdxSupportedFontStyles read FSupportedStyles;
    property Underline: Boolean read GetUnderline;
    property UnderlinePosition: Integer read FUnderlinePosition;
    property UnderlineThickness: Integer read FUnderlineThickness;
    property UnderscoreWidth: Integer read FUnderscoreWidth ;
  end;

  { TdxFontInfoMeasurer }

  TdxFontInfoMeasurer = class abstract
  private
    FUnitConverter: TdxDocumentLayoutUnitConverter;
  protected
    procedure Initialize; virtual; abstract;
  public
    constructor Create(AUnitConverter: TdxDocumentLayoutUnitConverter); virtual;
    function MeasureCharacterWidth(ACharacter: Char; AFontInfo: TdxFontInfo): Integer; virtual;
    function MeasureCharacterWidthF(ACharacter: Char; AFontInfo: TdxFontInfo): Single; virtual; abstract;
    function MeasureString(const AText: string; AFontInfo: TdxFontInfo): TSize; virtual; abstract;
    function MeasureMaxDigitWidthF(AFontInfo: TdxFontInfo): Single; virtual; abstract;

    property UnitConverter: TdxDocumentLayoutUnitConverter read FUnitConverter;
  end;

  { TdxFontCache }

  TdxCharacterFormattingScript = (Normal, Subscript, Superscript);

  TdxFontCache = class abstract
  strict protected class var
    FNameToCharacterSetMap: TObjectDictionary<string, TdxFontCharacterSet>;
  private
    FCharsets: TDictionary<string, Integer>;
    FIndexHash: TDictionary<Int64, Integer>;
    FItems: TdxFastObjectList;
    FMeasurer: TdxFontInfoMeasurer;
    FUnitConverter: TdxDocumentLayoutUnitConverter;
    function GetCount: Integer; inline;
    function GetItem(AIndex: Integer): TdxFontInfo; inline;
  protected
    function CreateFontInfoMeasurer(AUnitConverter: TdxDocumentLayoutUnitConverter): TdxFontInfoMeasurer; virtual; abstract;
    function CreateFontInfoCore(const AFontName: string; ADoubleFontSize: Integer; const AFontStyle: TFontStyles): TdxFontInfo; virtual; abstract;
    function CalcSubscriptFontSize(ABaseFontIndex: Integer): Integer;
    function CalcSuperscriptFontSize(ABaseFontIndex: Integer): Integer;
    function CalcSuperscriptFontIndex(const AFontName: string; ADoubleFontSize: Integer; const AFontStyle: TFontStyles): Integer;
    function CalcSubscriptFontIndex(const AFontName: string; ADoubleFontSize: Integer; const AFontStyle: TFontStyles): Integer;
    function CalcFontIndexCore(const AFontName: string; ADoubleFontSize: Integer; const AFontStyle: TFontStyles; AScript: TdxCharacterFormattingScript): Integer;
    function CalcHash(const AFontName: string; ADoubleFontSize: Integer; const AFontStyle: TFontStyles; AScript: TdxCharacterFormattingScript): Int64;
    function CreateFontInfo(const AHash: Int64; const AFontName: string; ADoubleFontSize: Integer; const AFontStyle: TFontStyles): Integer;
    function GetFontCharacterRanges(AFontInfo: TdxFontInfo): TdxFontCharacterRangeArray; virtual; abstract;

    property IndexHash: TDictionary<Int64, Integer> read FIndexHash;
    class property NameToCharacterSetMap: TObjectDictionary<string, TdxFontCharacterSet> read FNameToCharacterSetMap;
  public
    constructor Create(const AUnit: TdxDocumentLayoutUnit; const ADpi: Single); virtual;
    destructor Destroy; override;
    function CalcFontIndex(const AFontName: string; ADoubleFontSize: Integer; const AFontStyle: TFontStyles; AScript: TdxCharacterFormattingScript): Integer; virtual;
    function CalcNormalFontIndex(const AFontName: string; ADoubleFontSize: Integer; const AFontStyle: TFontStyles): Integer; virtual;
    function FindSubstituteFont(const ASourceFontName: string; ACharacter: Char; var AFontCharacterSet: TdxFontCharacterSet): string; virtual; abstract;
    function GetCharsetByFontName(const AFontName: string): Integer;
    function GetFontCharacterSet(const AFontName: string): TdxFontCharacterSet; virtual; abstract;
    function ShouldUseDefaultFontToDrawInvisibleCharacter(AFontInfo: TdxFontInfo; ACharacter: Char): Boolean; virtual;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxFontInfo read GetItem; default;
    property Measurer: TdxFontInfoMeasurer read FMeasurer;
    property UnitConverter: TdxDocumentLayoutUnitConverter read FUnitConverter;
  end;

  { TdxFontCacheManager }

  TdxFontCacheManager = class sealed
  strict private
    class var FItems: TObjectList<TdxFontCache>;
    class constructor Initialize;
    class destructor Finalize;
  public
    class function GetFontCache(const AUnit: TdxDocumentLayoutUnit; const ADpi: Single): TdxFontCache; static;
  end;

  { TdxRichEditControlCompatibility }

  TdxRichEditControlCompatibility = class
  strict private const
    FDefaultFontName: string = 'Calibri';
    FDefaultFontSize: Single = 11.0;
  strict private
    class procedure SetDefaultFontName(const Value: string); static;
    class procedure SetDefaultFontSize(const Value: Single); static;
  public
    class function DefaultDoubleFontSize: Integer;

    class property DefaultFontName: string read FDefaultFontName write SetDefaultFontName;
    class property DefaultFontSize: Single read FDefaultFontSize write SetDefaultFontSize;
  end;

function CodePageFromCharset(ACharSet: Cardinal): Cardinal;

implementation

uses
  RTLConsts, Math, SysUtils, dxCore, dxHash, dxHashUtils, Character,
  dxCharacters,
  dxRichEdit.Platform.Win.FontCache;

const
  dxUnicodeCharacterCount = 65536;

function CodePageFromCharset(ACharSet: Cardinal): Cardinal;
var
  lpCs: TCharsetInfo;
begin
  if (ACharSet <> 2) and TranslateCharsetInfo(ACharSet, lpCs, TCI_SRCCHARSET) then
    Result := lpCs.ciACP
  else
    Result := ANSI_CHARSET;
end;

{ TdxFontCharacterRange }

procedure TdxFontCharacterRange.CopyFrom(ALow, AHi: Integer);
begin
  Low := ALow;
  Hi := AHi;
end;

procedure TdxFontCharacterRange.CopyFrom(const ARange: TWCRange);
begin
  Low := Ord(ARange.wcLow);
  Hi := Low + ARange.cGlyphs - 1;
end;

{ TdxFontCharacterSet }

constructor TdxFontCharacterSet.Create(const ARanges: TdxFontCharacterRangeArray; const APanose: TPanose);
var
  I, AMaxIndex: Integer;
  ARange: TdxFontCharacterRange;
begin
  inherited Create;
  FBits := TBits.Create;
  FPanose := APanose;
  AMaxIndex := 0;
  for I := Low(ARanges) to High(ARanges) do
  begin
    ARange := ARanges[I];
    AddRange(ARange);
    if ARange.Hi > AMaxIndex then
      AMaxIndex := ARange.Hi;
  end;
  FBits.Size := AMaxIndex + 1;
end;

destructor TdxFontCharacterSet.Destroy;
begin
  FBits.Free;
  inherited Destroy;
end;

class function TdxFontCharacterSet.CalculatePanoseDistance(const AFontCharacterSet1, AFontCharacterSet2: TdxFontCharacterSet): Integer;
var
  I, ADist: Integer;
  P1, P2: PByte;
begin
  Result := 0;
  P1 := @AFontCharacterSet1.FPanose;
  P2 := @AFontCharacterSet2.FPanose;
  for I := 0 to 9 do
  begin
    ADist := P1^ - P2^;
    Result := Result + Sqr(ADist);
    Inc(P1);
    Inc(P2);
  end;
end;

function TdxFontCharacterSet.ContainsChar(C: Char): Boolean;
begin
  Result := (Ord(C) < FBits.Size) and FBits[Ord(C)];
end;

procedure TdxFontCharacterSet.AddRange(const AFontCharacterRange: TdxFontCharacterRange);
var
  I: Integer;
begin
  for I := AFontCharacterRange.Low to AFontCharacterRange.Hi do
    FBits[I] := True;
end;

{ TdxFontInfo }

constructor TdxFontInfo.Create(AMeasurer: TdxFontInfoMeasurer; const AName: string; ADoubleSize: Integer;
  const AFontStyle: TFontStyles; AAllowCjkCorrection: Boolean = True);
begin
  inherited Create;
  FName := AName;
  FStyle := AFontStyle;
  FBaseFontIndex := -1;
  FSizeInPoints := ADoubleSize / 2;
  CreateFont(AMeasurer, AName, Trunc(FSizeInPoints), AFontStyle);

  Initialize(AMeasurer);
  CalculateFontParameters(AMeasurer, AAllowCjkCorrection);
end;

function TdxFontInfo.GetAscentAndFree: Integer;
begin
  Result := FAscent + FFree;
end;

function TdxFontInfo.GetBold: Boolean;
begin
  Result := fsBold in FStyle;
end;

function TdxFontInfo.GetItalic: Boolean;
begin
  Result := fsItalic in FStyle;
end;

function TdxFontInfo.GetUnderline: Boolean;
begin
  Result := fsUnderline in FStyle;
end;

procedure TdxFontInfo.AdjustFontParameters;
begin
  FFree := FLineSpacing - FAscent - FDescent;
  if FFree < 0 then
  begin
    FDescent := FDescent + FFree;
    if FDescent < 0 then
    begin
      FAscent := FAscent + FDescent;
      FDescent := 0;
    end;
    FFree := 0;
  end;
end;

procedure TdxFontInfo.CalculateFontParameters(AMeasurer: TdxFontInfoMeasurer; AAllowCjkCorrection: Boolean);
begin
  CalculateFontVerticalParameters(AMeasurer, AAllowCjkCorrection);
  AdjustFontParameters;
  FSpaceWidth := Round(AMeasurer.MeasureCharacterWidthF(TdxCharacters.Space, Self));
  FPilcrowSignWidth := AMeasurer.MeasureCharacterWidth(TdxCharacters.PilcrowSign, Self);
  FNonBreakingSpaceWidth := AMeasurer.MeasureCharacterWidth(TdxCharacters.NonBreakingSpace, Self);
  FUnderscoreWidth := AMeasurer.MeasureCharacterWidth(TdxCharacters.Underscore, Self);
  FMiddleDotWidth := AMeasurer.MeasureCharacterWidth(TdxCharacters.MiddleDot, Self);
  FDotWidth := AMeasurer.MeasureCharacterWidth(TdxCharacters.Dot, Self);
  FDashWidth := AMeasurer.MeasureCharacterWidth(TdxCharacters.Dash, Self);
  FEqualSignWidth := AMeasurer.MeasureCharacterWidth(TdxCharacters.EqualSign, Self);
  FCharset := CalculateFontCharset(AMeasurer);
  FMaxDigitWidth := CalculateMaxDigitWidth(AMeasurer);
end;

function TdxFontInfo.GetBaseAscentAndFree(const AContainer: IdxFontsContainer): Integer;
begin
  Result := GetBaseFontInfo(AContainer).AscentAndFree;
end;

function TdxFontInfo.GetBaseDescent(const AContainer: IdxFontsContainer): Integer;
begin
  Result := GetBaseFontInfo(AContainer).Descent;
end;

function TdxFontInfo.GetBaseFontInfo(const AContainer: IdxFontsContainer): TdxFontInfo;
begin
  if FBaseFontIndex >= 0 then
    Result := AContainer.FontCache[FBaseFontIndex]
  else
    Result := Self;
end;

function TdxFontInfo.CalculateMaxDigitWidth(AMeasurer: TdxFontInfoMeasurer): Single;
begin
  Result := AMeasurer.MeasureMaxDigitWidthF(Self);
end;

{ TdxFontInfoMeasurer }

constructor TdxFontInfoMeasurer.Create(AUnitConverter: TdxDocumentLayoutUnitConverter);
begin
  inherited Create;
  FUnitConverter := AUnitConverter;
  Initialize;
end;

function TdxFontInfoMeasurer.MeasureCharacterWidth(ACharacter: Char; AFontInfo: TdxFontInfo): Integer;
begin
  Result := Ceil(MeasureCharacterWidthF(ACharacter, AFontInfo));
end;

{ TdxFontCache }

constructor TdxFontCache.Create(const AUnit: TdxDocumentLayoutUnit; const ADpi: Single);
begin
  inherited Create;
  FUnitConverter := TdxDocumentLayoutUnitConverter.CreateConverter(AUnit, ADpi);
  FIndexHash := TDictionary<Int64, Integer>.Create;
  FItems := TdxFastObjectList.Create;
  FMeasurer := CreateFontInfoMeasurer(FUnitConverter);
  FCharsets := TDictionary<string, Integer>.Create;
end;

destructor TdxFontCache.Destroy;
begin
  FIndexHash.Free;
  FCharsets.Free;
  FMeasurer.Free;
  FItems.Free;
  FUnitConverter.Free;
  inherited Destroy;
end;


function TdxFontCache.CalcFontIndex(const AFontName: string; ADoubleFontSize: Integer;
  const AFontStyle: TFontStyles; AScript: TdxCharacterFormattingScript): Integer;
var
  ADoubleFontSizeInLayoutFontUnits: Integer;
begin
  ADoubleFontSizeInLayoutFontUnits := UnitConverter.PointsToFontUnits(ADoubleFontSize);
  case AScript of
    TdxCharacterFormattingScript.Normal:
      Result := CalcNormalFontIndex(AFontName, ADoubleFontSizeInLayoutFontUnits, AFontStyle);
    TdxCharacterFormattingScript.Subscript:
      Result := CalcSubscriptFontIndex(AFontName, ADoubleFontSizeInLayoutFontUnits, AFontStyle);
  else
    Result := CalcSuperscriptFontIndex(AFontName, ADoubleFontSizeInLayoutFontUnits, AFontStyle);
  end;
end;

function TdxFontCache.ShouldUseDefaultFontToDrawInvisibleCharacter(AFontInfo: TdxFontInfo;
  ACharacter: Char): Boolean;
begin
  Result := True;
end;

function TdxFontCache.CalcSubscriptFontSize(ABaseFontIndex: Integer): Integer;
var
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := Items[ABaseFontIndex];
  Result := Round(AFontInfo.SubscriptSize.cy * UnitConverter.FontSizeScale);
end;

function TdxFontCache.CalcSuperscriptFontSize(ABaseFontIndex: Integer): Integer;
var
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := Items[ABaseFontIndex];
  Result := Round(AFontInfo.SuperscriptSize.cy * UnitConverter.FontSizeScale);
end;

function TdxFontCache.CalcNormalFontIndex(const AFontName: string; ADoubleFontSize: Integer; const AFontStyle: TFontStyles): Integer;
begin
  Result := CalcFontIndexCore(AFontName, ADoubleFontSize, AFontStyle, TdxCharacterFormattingScript.Normal);
end;

function TdxFontCache.CalcSuperscriptFontIndex(const AFontName: string; ADoubleFontSize: Integer; const AFontStyle: TFontStyles): Integer;
var
  AFontInfo: TdxFontInfo;
  ASuperscriptFontSize: Integer;
  ABaseFontIndex: Integer;
begin
  ABaseFontIndex := CalcFontIndexCore(AFontName, ADoubleFontSize, AFontStyle, TdxCharacterFormattingScript.Normal);
  ASuperscriptFontSize := CalcSuperscriptFontSize(ABaseFontIndex);
  Result := CalcFontIndexCore(AFontName, ASuperscriptFontSize * 2, AFontStyle, TdxCharacterFormattingScript.Superscript);
  AFontInfo := Items[Result];
  AFontInfo.CalculateSuperscriptOffset(Items[ABaseFontIndex]);
  AFontInfo.BaseFontIndex := ABaseFontIndex;
end;

function TdxFontCache.CalcSubscriptFontIndex(const AFontName: string; ADoubleFontSize: Integer; const AFontStyle: TFontStyles): Integer;
var
  AFontInfo: TdxFontInfo;
  ASubscriptFontSize: Integer;
  ABaseFontIndex: Integer;
begin
  ABaseFontIndex := CalcFontIndexCore(AFontName, ADoubleFontSize, AFontStyle, TdxCharacterFormattingScript.Normal);
  ASubscriptFontSize := CalcSubscriptFontSize(ABaseFontIndex);
  Result := CalcFontIndexCore(AFontName, ASubscriptFontSize * 2, AFontStyle, TdxCharacterFormattingScript.Subscript);
  AFontInfo := Items[Result];
  AFontInfo.CalculateSubscriptOffset(Items[ABaseFontIndex]);
  AFontInfo.BaseFontIndex := ABaseFontIndex;
end;

function TdxFontCache.CalcFontIndexCore(const AFontName: string; ADoubleFontSize: Integer;
  const AFontStyle: TFontStyles; AScript: TdxCharacterFormattingScript): Integer;
var
  AHash: Int64;
  AUpperFontName: string;
begin
{$IFDEF DELPHIXE4}
  AUpperFontName := AFontName.ToUpper;
{$ELSE}
  AUpperFontName := ToUpper(AFontName);
{$ENDIF}
  AHash := CalcHash(AUpperFontName, ADoubleFontSize, AFontStyle, AScript);
  if not IndexHash.TryGetValue(AHash, Result) then
  begin
    TMonitor.Enter(Self);
    try
      if not IndexHash.TryGetValue(AHash, Result) then
        Result := CreateFontInfo(AHash, AFontName, ADoubleFontSize, AFontStyle);
    finally
      TMonitor.Exit(Self);
    end;
  end;
end;

function TdxFontCache.CalcHash(const AFontName: string; ADoubleFontSize: Integer;
  const AFontStyle: TFontStyles; AScript: TdxCharacterFormattingScript): Int64;
var
  AStyleValue: Integer;
  AFontStyleByte: Byte absolute AFontStyle;
begin
  AStyleValue := Ord(AScript) or (AFontStyleByte shl 2);
  Int64Rec(Result).Lo := (ADoubleFontSize shl 6) or AStyleValue;
  Int64Rec(Result).Hi := dxElfHash(PChar(AFontName), Length(AFontName), nil, 0);
end;

function TdxFontCache.CreateFontInfo(const AHash: Int64; const AFontName: string; ADoubleFontSize: Integer;
  const AFontStyle: TFontStyles): Integer;
var
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := CreateFontInfoCore(AFontName, ADoubleFontSize, AFontStyle);
  Result := FItems.Add(AFontInfo);
  if not FCharsets.ContainsKey(AFontName) then
    FCharsets.Add(AFontName, AFontInfo.Charset);
  IndexHash.Add(AHash, Result);
end;

function TdxFontCache.GetCharsetByFontName(const AFontName: string): Integer;
begin
  if not FCharsets.TryGetValue(AFontName, Result) then
  begin
    CalcFontIndex(AFontName, 24, [], TdxCharacterFormattingScript.Normal);
    Result := FCharsets[AFontName];
  end;
end;

function TdxFontCache.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxFontCache.GetItem(AIndex: Integer): TdxFontInfo;
begin
  Result := TdxFontInfo(FItems[AIndex]);
end;

{ TdxFontCacheManager }

class constructor TdxFontCacheManager.Initialize;
begin
  FItems := TObjectList<TdxFontCache>.Create;
end;

class destructor TdxFontCacheManager.Finalize;
begin
  FItems.Free;
end;

class function TdxFontCacheManager.GetFontCache(
  const AUnit: TdxDocumentLayoutUnit; const ADpi: Single): TdxFontCache;
var
  I: Integer;
  AUnitConverter: TdxDocumentLayoutUnitConverter;
begin
  AUnitConverter := TdxDocumentLayoutUnitConverter.CreateConverter(AUnit, ADpi);
  try
    for I := 0 to FItems.Count - 1 do
      if FItems[I].UnitConverter.Equals(AUnitConverter) then
        Exit(FItems[I]);
  finally
    AUnitConverter.Free;
  end;

  Result := TdxGdiFontCache.Create(AUnit, ADpi);
  FItems.Add(Result);
end;

{ TdxRichEditControlCompatibility }

class function TdxRichEditControlCompatibility.DefaultDoubleFontSize: Integer;
begin
  Result := Trunc(DefaultFontSize * 2);
end;

class procedure TdxRichEditControlCompatibility.SetDefaultFontName(
  const Value: string);
begin
  if Value = '' then
    raise Exception.Create('Invalid argument: DefaultFontName');
  FDefaultFontName := Value;
end;

class procedure TdxRichEditControlCompatibility.SetDefaultFontSize(
  const Value: Single);
begin
  if Value < 0 then
    raise Exception.Create('Invalid argument: DefaultFontSize');
  FDefaultFontSize := Value;
end;

end.

