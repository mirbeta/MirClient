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

unit dxRichEdit.Export.Html.Utils;

interface

{$I cxVer.inc}
{$I dxRichEditControl.inc}

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, Rtti, RegularExpressions,
  dxCore, dxCoreClasses, dxCoreGraphics, dxCultureInfo, dxGDIPlusAPI, dxGDIPlusClasses,

  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Graphics;

type
  { TdxHtmlKeywords }

  TdxHtmlKeywords = class sealed
  public const
  {$REGION 'public const'}
    OpenTag = '<';
    CloseTag = '>';
    TagIsClose = '/';
    Equal = '=';
    Semicolumn = ';';
    Html = 'html';
    Head = 'head';
    Body = 'body';
    Underline = 'underline';
    Italic = 'italic';
    Bold = 'bold';
    Paragraph = 'para';
    Code = 'code';
    NonBreakingSpace = '&nbsp';
    ParagraphAlignment = 'align';
    LeftAlignment = 'left';
    RightAlignment = 'right';
    CenterAlignment = 'center';
    JustifyAlignment = 'justify';
    Superscript = 'sup';
    Subscript = 'sub';
    Image = 'img';
    CData = 'CData';
  {$ENDREGION}
  end;

  { TdxHtmlConvert }

  TdxHtmlConvert = class
  strict private
    class var
      FUnitHT: TdxEnumeratedDictionary<TdxGraphicsUnit, string>;
      FHtmlAlignHT: TdxEnumeratedDictionary<TdxGPStringAlignment, string>;
      FHtmlVAlignHT: TdxEnumeratedDictionary<TdxGPStringAlignment, string>;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    class function CreateUnitTable: TdxEnumeratedDictionary<TdxGraphicsUnit, string>; static;
    class function CreateHorizontalAlignmentTable: TdxEnumeratedDictionary<TdxGPStringAlignment, string>; static;
    class function CreateVerticalAlignmentTable: TdxEnumeratedDictionary<TdxGPStringAlignment, string>; static;
  public
    class function ToHtml(AColor: TdxAlphaColor): string; static;
    class function ToHtmlAlign(AVal: TdxGPStringAlignment): string; static;
    class function ToHtmlVAlign(AVal: TdxGPStringAlignment): string; static;
    class function DocumentsToPointsF(AVal: Single): Single; static;
    class function FontSizeToString(AFont: TdxGPFont): string; overload; static;
    class function FontSizeToString(ASize: Single; AUnit: TdxGraphicsUnit): string; overload; static;
    class function FontSizeToStringInPixels(ASize: Single; AUnit: TdxGraphicsUnit): string; static;
  end;

  { TdxHttpUtility }

  TdxHttpUtility = class
  strict private const
    EntityEndingChars: array[0..1] of Char = (';', '&');
  protected
    class function AspCompatUrlEncode(const AUrl: string): string; static;
    class function CollapsePercentUFromStringInternal(const AStr: string; AEncoding: TEncoding): string; static;
    class function IndexOfHtmlAttributeEncodingChars(const AHtml: string; AStartPosition: Integer): Integer; static;
    class function IndexOfHtmlEncodingChars(const AHtml: string; AStartPosition: Integer): Integer; static;
    class function IsDirectorySeparatorChar(AValue: Char): Boolean; static;
    class function UrlDecodeBytesFromBytesInternal(const ABuffer: TArray<Byte>; AOffset, ACount: Integer): TArray<Byte>; static;
    class function UrlDecodeStringFromBytesInternal(const ABuffer: TArray<Byte>; AOffset, ACount: Integer; AEncoding: TEncoding): string; static;
    class function UrlDecodeStringFromStringInternal(const AUrl: string; AEncoding: TEncoding): string; static;
    class function UrlEncodeBytesToBytesInternal(const ABytes: TArray<Byte>; AOffset, ACount: Integer; AAlwaysCreateReturnValue: Boolean): TArray<Byte>; static;
    class function UrlEncodeBytesToBytesInternalNonAscii(const ABytes: TArray<Byte>; AOffset, ACount: Integer; AAlwaysCreateReturnValue: Boolean): TArray<Byte>; static;
    class function UrlEncodeUnicodeStringToStringInternal(const AUrl: string; AIgnoreAscii: Boolean): string; static;
  public
    class function HexToInt(AValue: Char): Integer; static;
    class function HtmlAttributeEncode(const AValue: string): string; overload; static;
    class procedure HtmlAttributeEncode(const AHtml: string; AOutput: TTextWriter); overload; static;
    class procedure HtmlAttributeEncodeInternal(const AHtml: string; AWriter: TTextWriter); static;
    class function HtmlDecode(const AValue: string): string; overload; static;
    class procedure HtmlDecode(const AValue: string; AOutput: TTextWriter); overload; static;
    class function HtmlEncode(const AValue: string): string; overload; static;
    class procedure HtmlEncode(const AHtml: string; AOutput: TTextWriter); overload; static;
    class function IntToHex(AValue: Integer): Char; static;
    class function IsNonAsciiByte(AValue: Byte): Boolean; static;
    class function IsSafe(AValue: Char): Boolean; static;
    class function UrlDecode(const AValue: string): string; overload; static;
    class function UrlDecode(const ABytes: TArray<Byte>; AEncoding: TEncoding): string; overload; static;
    class function UrlDecode(const AUrl: string; AEncoding: TEncoding): string; overload; static;
    class function UrlDecode(const ABytes: TArray<Byte>; AOffset, ACount: Integer; AEncoding: TEncoding): string; overload; static;
    class function UrlDecodeToBytes(const AValue: TArray<Byte>): TArray<Byte>; overload; static;
    class function UrlDecodeToBytes(const AUrl: string): TArray<Byte>; overload; static;
    class function UrlDecodeToBytes(const AUrl: string; AEncoding: TEncoding): TArray<Byte>; overload; static;
    class function UrlDecodeToBytes(const ABytes: TArray<Byte>; AOffset, ACount: Integer): TArray<Byte>; overload; static;
    class function UrlEncode(const ABytes: TArray<Byte>): string; overload; static;
    class function UrlEncode(const AUrl: string): string; overload; static;
    class function UrlEncode(const AUrl: string; AEncoding: TEncoding): string; overload; static;
    class function UrlEncode(const ABytes: TArray<Byte>; AOffset, ACount: Integer): string; overload; static;
    class function UrlEncodeNonAscii(const AUrl: string; AEncoding: TEncoding): string; static;
    class function UrlEncodeSpaces(const AUrl: string): string; static;
    class function UrlEncodeToBytes(const AUrl: string): TArray<Byte>; overload; static;
    class function UrlEncodeToBytes(const ABytes: TArray<Byte>): TArray<Byte>; overload; static;
    class function UrlEncodeToBytes(const AUrl: string; AEncoding: TEncoding): TArray<Byte>; overload; static;
    class function UrlEncodeToBytes(const ABytes: TArray<Byte>; AOffset, ACount: Integer): TArray<Byte>; overload; static;
    class function UrlEncodeUnicode(const AUrl: string): string; static;
    class function UrlEncodeUnicodeToBytes(const AUrl: string): TArray<Byte>; static;
    class function UrlPathEncode(const AUrl: string): string; static;
    class function UrlEncodeToUnicodeCompatible(const AStringData: string): string; static;
    class function IsSpecialSymbol(C: Char): Boolean; static;
    class function IsUncSharePath(const APath: string): Boolean; static;
  end;

  { TdxStateBag }

  TdxStateBag = class(TDictionary<string, TValue>)
  strict private
    function GetItem(const AKey: string): TValue;
    procedure SetItem(const AKey: string; const Value: TValue);
  public
    constructor Create;
    property Items[const AKey: string]: TValue read GetItem write SetItem; default;
  end;

implementation

uses
  Math, Character, StrUtils,
  dxStringHelper,
  dxRichEdit.Utils.Exceptions,
  dxEncoding,
  dxRichEdit.Utils.NumberParser;

type

  { TdxUrlDecoder }

  TdxUrlDecoder = class
  strict private
    FBufferSize: Integer;
    FByteBuffer: TArray<Byte>;
    FCharBuffer: TArray<Char>;
    FEncoding: TEncoding;
    FNumBytes: Integer;
    FNumChars: Integer;
    procedure FlushBytes;
  protected
    procedure AddByte(AValue: Byte);
    procedure AddChar(AValue: Char);
    function GetString: string;
  public
    constructor Create(ABufferSize: Integer; AEncoding: TEncoding);
  end;

  { TdxHtmlEntities }

  TdxHtmlEntities = class
  strict private
    class var
      FEntitiesList: TArray<string>;
      FEntitiesLookup: TdxNamedOrdinalDictionary<Char>;
      FLock: TObject;
    class constructor Initialize;
    class destructor Finalize;
  public
    class function Lookup(const AEntity: string): Char; static;
  end;

{ TdxUrlDecoder }

constructor TdxUrlDecoder.Create(ABufferSize: Integer; AEncoding: TEncoding);
begin
  FBufferSize := ABufferSize;
  FEncoding := AEncoding;
  SetLength(FCharBuffer, ABufferSize);
end;

procedure TdxUrlDecoder.AddByte(AValue: Byte);
begin
  if FByteBuffer = nil then
    SetLength(FByteBuffer, FBufferSize);
  FByteBuffer[FNumBytes] := AValue;
  Inc(FNumBytes);
end;

procedure TdxUrlDecoder.AddChar(AValue: Char);
begin
  if FNumBytes > 0 then
    FlushBytes;
  FCharBuffer[FNumChars] := AValue;
  Inc(FNumChars);
end;

procedure TdxUrlDecoder.FlushBytes;
begin
  if FNumBytes <= 0 then
    Exit;
  FNumChars := FNumChars + FEncoding.GetChars(FByteBuffer, 0, FNumBytes, FCharBuffer, FNumChars);
  FNumBytes := 0;
end;

function TdxUrlDecoder.GetString: string;
begin
  if FNumBytes > 0 then
    FlushBytes;
  if FNumChars > 0 then
    SetString(Result, PChar(@FCharBuffer[0]), FNumChars)
  else
    Result := '';
end;

{ TdxHtmlEntities }

class constructor TdxHtmlEntities.Initialize;
begin
  FEntitiesList := TArray<string>.Create(
    '"-quot', '&-amp', '<-lt', '>-gt', #$00A0'-nbsp', #$00A1'-iexcl', #$00A2'-cent', #$00A3'-pound',
    #$00A4'-curren', #$00A5'-yen', #$00A6'-brvbar', #$00A7'-sect', #$00A8'-uml', #$00A9'-copy',
    #$00AA'-ordf', #$00AB'-laquo', #$00AC'-not', #$00AD'-shy', #$00AE'-reg', #$00AF'-macr', #$00B0'-deg',
    #$00B1'-plusmn', #$00B2'-sup2', #$00B3'-sup3', #$00B4'-acute', #$00B5'-micro', #$00B6'-para',
    #$00B7'-middot', #$00B8'-cedil', #$00B9'-sup1', #$00BA'-ordm', #$00BB'-raquo', #$00BC'-frac14',
    #$00BD'-frac12', #$00BE'-frac34', #$00BF'-iquest', #$00C0'-Agrave', #$00C1'-Aacute', #$00C2'-Acirc',
    #$00C3'-Atilde', #$00C4'-Auml', #$00C5'-Aring', #$00C6'-AElig', #$00C7'-Ccedil', #$00C8'-Egrave',
    #$00C9'-Eacute', #$00CA'-Ecirc', #$00CB'-Euml', #$00CC'-Igrave', #$00CD'-Iacute', #$00CE'-Icirc',
    #$00CF'-Iuml', #$00D0'-ETH', #$00D1'-Ntilde', #$00D2'-Ograve', #$00D3'-Oacute', #$00D4'-Ocirc',
    #$00D5'-Otilde', #$00D6'-Ouml', #$00D7'-times', #$00D8'-Oslash', #$00D9'-Ugrave', #$00DA'-Uacute',
    #$00DB'-Ucirc', #$00DC'-Uuml', #$00DD'-Yacute', #$00DE'-THORN', #$00DF'-szlig', #$00E0'-agrave',
    #$00E1'-aacute', #$00E2'-acirc', #$00E3'-atilde', #$00E4'-auml', #$00E5'-aring', #$00E6'-aelig',
    #$00E7'-ccedil', #$00E8'-egrave', #$00E9'-eacute', #$00EA'-ecirc', #$00EB'-euml', #$00EC'-igrave',
    #$00ED'-iacute', #$00EE'-icirc', #$00EF'-iuml', #$00F0'-eth', #$00F1'-ntilde', #$00F2'-ograve',
    #$00F3'-oacute', #$00F4'-ocirc', #$00F5'-otilde', #$00F6'-ouml', #$00F7'-divide', #$00F8'-oslash',
    #$00F9'-ugrave', #$00FA'-uacute', #$00FB'-ucirc', #$00FC'-uuml', #$00FD'-yacute', #$00FE'-thorn',
    #$00FF'-yuml', #$0152'-OElig', #$0153'-oelig', #$0160'-Scaron', #$0161'-scaron', #$0178'-Yuml',
    #$0192'-fnof', #$02C6'-circ', #$02DC'-tilde', #$0391'-Alpha', #$0392'-Beta', #$0393'-Gamma',
    #$0394'-Delta', #$0395'-Epsilon', #$0396'-Zeta', #$0397'-Eta', #$0398'-Theta', #$0399'-Iota',
    #$039A'-Kappa', #$039B'-Lambda', #$039C'-Mu', #$039D'-Nu', #$039E'-Xi', #$039F'-Omicron', #$03A0'-Pi',
    #$03A1'-Rho', #$03A3'-Sigma', #$03A4'-Tau', #$03A5'-Upsilon', #$03A6'-Phi', #$03A7'-Chi', #$03A8'-Psi',
    #$03A9'-Omega', #$03B1'-alpha', #$03B2'-beta', #$03B3'-gamma', #$03B4'-delta', #$03B5'-epsilon',
    #$03B6'-zeta', #$03B7'-eta', #$03B8'-theta', #$03B9'-iota', #$03BA'-kappa', #$03BB'-lambda',
    #$03BC'-mu', #$03BD'-nu', #$03BE'-xi', #$03BF'-omicron', #$03C0'-pi', #$03C1'-rho', #$03C2'-sigmaf',
    #$03C3'-sigma', #$03C4'-tau', #$03C5'-upsilon', #$03C6'-phi', #$03C7'-chi', #$03C8'-psi', #$03C9'-omega',
    #$03D1'-thetasym', #$03D2'-upsih', #$03D6'-piv', #$2002'-ensp', #$2003'-emsp', #$2009'-thinsp',
    #$200C'-zwnj', #$200D'-zwj', #$200E'-lrm', #$200F'-rlm', #$2013'-ndash', #$2014'-mdash', #$2018'-lsquo',
    #$2019'-rsquo', #$201A'-sbquo', #$201C'-ldquo', #$201D'-rdquo', #$201E'-bdquo', #$2020'-dagger',
    #$2021'-Dagger', #$2022'-bull', #$2026'-hellip', #$2030'-permil', #$2032'-prime', #$2033'-Prime',
    #$2039'-lsaquo', #$203A'-rsaquo', #$203E'-oline', #$2044'-frasl', #$20AC'-euro', #$2111'-image',
    #$2118'-weierp', #$211C'-real', #$2122'-trade', #$2135'-alefsym', #$2190'-larr', #$2191'-uarr',
    #$2192'-rarr', #$2193'-darr', #$2194'-harr', #$21B5'-crarr', #$21D0'-lArr', #$21D1'-uArr', #$21D2'-rArr',
    #$21D3'-dArr', #$21D4'-hArr', #$2200'-forall', #$2202'-part', #$2203'-exist', #$2205'-empty', #$2207'-nabla',
    #$2208'-isin', #$2209'-notin', #$220B'-ni', #$220F'-prod', #$2211'-sum', #$2212'-minus', #$2217'-lowast',
    #$221A'-radic', #$221D'-prop', #$221E'-infin', #$2220'-ang', #$2227'-and', #$2228'-or', #$2229'-cap',
    #$222A'-cup', #$222B'-int', #$2234'-there4', #$223C'-sim', #$2245'-cong', #$2248'-asymp', #$2260'-ne',
    #$2261'-equiv', #$2264'-le', #$2265'-ge', #$2282'-sub', #$2283'-sup', #$2284'-nsub', #$2286'-sube',
    #$2287'-supe', #$2295'-oplus', #$2297'-otimes', #$22A5'-perp');
  FLock := TObject.Create;
end;

class destructor TdxHtmlEntities.Finalize;
begin
  FLock.Free;
  FEntitiesLookup.Free;
end;

class function TdxHtmlEntities.Lookup(const AEntity: string): Char;
var
  I: Integer;
begin
  if FEntitiesLookup = nil then
  begin
    TMonitor.Enter(FLock);
    try
      if FEntitiesLookup = nil then
      begin
        FEntitiesLookup := TdxNamedOrdinalDictionary<Char>.Create;
        for I := Low(FEntitiesList) to High(FEntitiesList) do
          FEntitiesLookup.Add(TdxStringHelper.Substring(FEntitiesList[I], 2), FEntitiesList[I][1]);
      end;
    finally
      TMonitor.Exit(FLock);
    end;
  end;
  if not FEntitiesLookup.TryGetValue(AEntity, Result) then
    Result := #0;
end;

{ TdxHtmlConvert }

class constructor TdxHtmlConvert.Initialize;
begin
  FUnitHT := CreateUnitTable;
  FHtmlAlignHT := CreateHorizontalAlignmentTable;
  FHtmlVAlignHT := CreateVerticalAlignmentTable;
end;

class destructor TdxHtmlConvert.Finalize;
begin
  FUnitHT.Free;
  FHtmlAlignHT.Free;
  FHtmlVAlignHT.Free;
end;

class function TdxHtmlConvert.CreateUnitTable: TdxEnumeratedDictionary<TdxGraphicsUnit, string>;
begin
  Result := TdxEnumeratedDictionary<TdxGraphicsUnit, string>.Create;
  Result.Add(TdxGraphicsUnit.guInch, 'in');
  Result.Add(TdxGraphicsUnit.guMillimeter, 'mm');
  Result.Add(TdxGraphicsUnit.guPixel, 'px');
  Result.Add(TdxGraphicsUnit.guPoint, 'pt');
end;

class function TdxHtmlConvert.CreateHorizontalAlignmentTable: TdxEnumeratedDictionary<TdxGPStringAlignment, string>;
begin
  Result := TdxEnumeratedDictionary<TdxGPStringAlignment, string>.Create;
  Result.Add(StringAlignmentNear, 'left');
  Result.Add(StringAlignmentCenter, 'center');
  Result.Add(StringAlignmentFar, 'right');
end;

class function TdxHtmlConvert.CreateVerticalAlignmentTable: TdxEnumeratedDictionary<TdxGPStringAlignment, string>;
begin
  Result := TdxEnumeratedDictionary<TdxGPStringAlignment, string>.Create;
  Result.Add(StringAlignmentNear, 'top');
  Result.Add(StringAlignmentCenter, 'middle');
  Result.Add(StringAlignmentFar, 'bottom');
end;

//class function TdxHtmlConvert.ToHtml(AValue: Integer): string;
//begin
//  Result := AValue + 'px';
//end;

class function TdxHtmlConvert.ToHtml(AColor: TdxAlphaColor): string;
var
  C: TdxAlphaColor;
begin
  C := TdxAlphaColors.Blend(AColor, TdxAlphaColors.White);
  if AColor = TdxAlphaColors.Transparent then
    Exit('transparent');
  if TdxAlphaColors.IsTransparentOrEmpty(C) then
    C := TdxAlphaColors.White;
  Result := TdxAlphaColors.ToHtml(C, False);
end;

class function TdxHtmlConvert.ToHtmlAlign(AVal: TdxGPStringAlignment): string;
begin
  if not FHtmlAlignHT.TryGetValue(AVal, Result) then
    Result := '';
end;

class function TdxHtmlConvert.ToHtmlVAlign(AVal: TdxGPStringAlignment): string;
begin
  if not FHtmlVAlignHT.TryGetValue(AVal, Result) then
    Result := '';
end;

class function TdxHtmlConvert.DocumentsToPointsF(AVal: Single): Single;
begin
  Result := AVal * 6.0 / 25.0;
end;

class function TdxHtmlConvert.FontSizeToString(AFont: TdxGPFont): string;
begin
  Result := FontSizeToString(AFont.Size, AFont.&Unit);
end;

class function TdxHtmlConvert.FontSizeToString(ASize: Single; AUnit: TdxGraphicsUnit): string;
var
  AUnitType: string;
begin
  if AUnit = TdxGraphicsUnit.guDocument then
    Exit(FloatToStr(SimpleRoundTo(ASize, -1)) + 'pt');

  if not FUnitHT.TryGetValue(AUnit, AUnitType) then
    AUnitType := 'pt';

  if AUnitType = 'px' then
    Result := IntToStr(Round(ASize)) + AUnitType
  else
    Result := FloatToStr(SimpleRoundTo(ASize, -1)) + AUnitType;
end;

class function TdxHtmlConvert.FontSizeToStringInPixels(ASize: Single; AUnit: TdxGraphicsUnit): string;
//var
//  AFontSizeInPx: Integer;
begin
  NotImplemented;
  Result := '';
//  AFontSizeInPx := Integer(Round(GraphicsUnitConverter.Convert(ASize, GraphicsDpi.UnitToDpi(AUnit), GraphicsDpi.DeviceIndependentPixel)));
//  Result := AFontSizeInPx.ToString + 'px';
end;

{ TdxHttpUtility }

class function TdxHttpUtility.AspCompatUrlEncode(const AUrl: string): string;
begin
  Result := UrlEncode(AUrl);
  Result := TdxStringHelper.Replace(Result, '!', '%21');
  Result := TdxStringHelper.Replace(Result, '*', '%2A');
  Result := TdxStringHelper.Replace(Result, '(', '%28');
  Result := TdxStringHelper.Replace(Result, ')', '%29');
  Result := TdxStringHelper.Replace(Result, '-', '%2D');
  Result := TdxStringHelper.Replace(Result, '.', '%2E');
  Result := TdxStringHelper.Replace(Result, '_', '%5F');
  Result := TdxStringHelper.Replace(Result, '\', '%5C');
end;

class function TdxHttpUtility.CollapsePercentUFromStringInternal(const AStr: string; AEncoding: TEncoding): string;
var
  ADecoder: TdxUrlDecoder;
  I, ANum4, ANum5, ANum6, ANum7, ALen: Integer;
  ACh: Char;
begin
  ALen := Length(AStr);
  ADecoder := TdxUrlDecoder.Create(ALen, AEncoding);
  if Pos('%u', AStr) = 0 then
    Exit(AStr);

  I := 1;
  while I <= ALen do
  begin
    ACh := AStr[I];
    if (ACh = '%') and (I <= ALen - 5) and (AStr[I + 1] = 'u') then
    begin
      ANum4 := HexToInt(AStr[I + 2]);
      ANum5 := HexToInt(AStr[I + 3]);
      ANum6 := HexToInt(AStr[I + 4]);
      ANum7 := HexToInt(AStr[I + 5]);
      if (ANum4 >= 0) and (ANum5 >= 0) and (ANum6 >= 0) and (ANum7 >= 0) then
      begin
        ACh := Char((ANum4 shl 12) or (ANum5 shl 8) or (ANum6 shl 4) or ANum7);
        Inc(I, 6);
        ADecoder.AddChar(ACh);
        Continue;
      end;
    end;
    if (Ord(ACh) and $ff80) = 0 then
      ADecoder.AddByte(Byte(ACh))
    else
      ADecoder.AddChar(ACh);
    Inc(I);
  end;
  Result := ADecoder.GetString;
end;

class function TdxHttpUtility.HexToInt(AValue: Char): Integer;
begin
  if (AValue >= '0') and (AValue <= '9') then
    Exit(Ord(AValue) - Ord('0'));

  if (AValue >= 'a') and (AValue <= 'f') then
    Exit(Ord(AValue) - Ord('a') + 10);

  if (AValue >= 'A') and (AValue <= 'F') then
    Exit(Ord(AValue) - Ord('A') + 10);

  Result := -1;
end;

class function TdxHttpUtility.HtmlAttributeEncode(const AValue: string): string;
var
  AHtmlAttributeEncodingIndex, AHtmlAttributeEncodingStartIndex, AValueLen: Integer;
  ABuilder: TStringBuilder;
begin
  AValueLen := Length(AValue);
  if AValueLen = 0 then
    Exit('');

  AHtmlAttributeEncodingIndex := IndexOfHtmlAttributeEncodingChars(AValue, 0);
  if AHtmlAttributeEncodingIndex = -1 then
    Exit(AValue);

  ABuilder := TStringBuilder.Create(AValueLen + 5);
  try
    AHtmlAttributeEncodingStartIndex := 0;
    while True do
    begin
      if AHtmlAttributeEncodingIndex > AHtmlAttributeEncodingStartIndex then
        ABuilder.Append(AValue, AHtmlAttributeEncodingStartIndex, AHtmlAttributeEncodingIndex - AHtmlAttributeEncodingStartIndex);
      case AValue[AHtmlAttributeEncodingIndex + 1] of
        '"':
          ABuilder.Append('&quot;');
        '&':
          ABuilder.Append('&amp;');
        '<':
          ABuilder.Append('&lt;');
      end;
      AHtmlAttributeEncodingStartIndex := AHtmlAttributeEncodingIndex + 1;
      AHtmlAttributeEncodingIndex := IndexOfHtmlAttributeEncodingChars(AValue, AHtmlAttributeEncodingStartIndex);
      if AHtmlAttributeEncodingIndex <> -1 then
        Continue;
      ABuilder.Append(AValue, AHtmlAttributeEncodingStartIndex, AValueLen - AHtmlAttributeEncodingStartIndex);
      Break;
    end;
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

class procedure TdxHttpUtility.HtmlAttributeEncode(const AHtml: string; AOutput: TTextWriter);
var
  AHtmlAttributeEncodingIndex, ACount, AIndex: Integer;
  AChr: Char;
begin
  if AHtml = '' then
    Exit;

  AHtmlAttributeEncodingIndex := IndexOfHtmlAttributeEncodingChars(AHtml, 0);
  if AHtmlAttributeEncodingIndex = -1 then
  begin
    AOutput.Write(AHtml);
    Exit;
  end;

  ACount := Length(AHtml) - AHtmlAttributeEncodingIndex;
  AIndex := 1;
  while AHtmlAttributeEncodingIndex > 0 do
  begin
    Dec(AHtmlAttributeEncodingIndex);
    Inc(AIndex);
    AOutput.Write(AHtml[AIndex]);
  end;

  while ACount > 0 do
  begin
    Dec(ACount);
    Inc(AIndex);
    AChr := AHtml[AIndex];
    if AChr > '<' then
    begin
      AOutput.Write(AChr);
      Exit;
    end;
    if AChr <> '"' then
    begin
      if AChr = '&' then
        AOutput.Write('&amp;')
      else
        if AChr <> '<' then
          AOutput.Write(AChr)
        else
          AOutput.Write('&lt;');
    end
    else
      AOutput.Write('&quot;');
  end;
end;

class procedure TdxHttpUtility.HtmlAttributeEncodeInternal(const AHtml: string; AWriter: TTextWriter);
var
  ANum, AIndex: Integer;
begin
  ANum := IndexOfHtmlAttributeEncodingChars(AHtml, 0);
  if ANum = -1 then
  begin
    AWriter.Write(AHtml);
    Exit;
  end;
  AIndex := 0;
  while True do
  begin
    if ANum > AIndex then
    begin
      AWriter.Write(TdxStringHelper.Substring(AHtml, AIndex, ANum - AIndex));
    end;
    case AHtml[ANum] of
      '"':
        AWriter.Write('&quot;');
      '&':
        AWriter.Write('&amp;');
      '<':
        AWriter.Write('&lt;');
    end;
    AIndex := ANum + 1;
    if AIndex < Length(AHtml) then
    begin
      ANum := IndexOfHtmlAttributeEncodingChars(AHtml, AIndex);
      if ANum <> -1 then
        Continue;
      AWriter.Write(TdxStringHelper.Substring(AHtml, AIndex, Length(AHtml) - AIndex));
    end;
    Break;
  end;
end;

class function TdxHttpUtility.HtmlDecode(const AValue: string): string;
var
  ABuilder: TStringBuilder;
  AOutput: TStringWriter;
begin
  if AValue = '' then
    Exit('');

  if Pos('&', AValue) = 0 then
    Exit(AValue);

  ABuilder := TStringBuilder.Create;
  try
    AOutput := TStringWriter.Create(ABuilder);
    try
      HtmlDecode(AValue, AOutput);
    finally
      AOutput.Free;
    end;
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

class procedure TdxHttpUtility.HtmlDecode(const AValue: string; AOutput: TTextWriter);
var
  I, AEntityEndingIndex, AIntValue: Integer;
  C, AEntityChar: Char;
  AEntity: string;
  P: PChar;
begin
  if AValue = '' then
    Exit;

  if Pos('&', AValue) = 0 then
  begin
    AOutput.Write(AValue);
    Exit;
  end;

  P := PChar(AValue);
  I := 0;
  while I < Length(AValue) do
  begin
    C := P[I];
    if C = '&' then
    begin
      AEntityEndingIndex := TdxStringHelper.IndexOfAny(AValue, EntityEndingChars, I + 1);
      if (AEntityEndingIndex > 0) and (P[AEntityEndingIndex] = ';') then
      begin
        AEntity := TdxStringHelper.Substring(AValue, I + 1, (AEntityEndingIndex - I) - 1);
        if (Length(AEntity) > 1) and (AEntity[1] = '#') then
        begin
          try
            if (AEntity[2] = 'x') or (AEntity[2] = 'X') then
            begin
              if TdxNumber.TryParse(TdxStringHelper.Substring(AEntity, 2), TdxNumberStyles.AllowHexSpecifier, AIntValue) then
              begin
                C := Char(AIntValue);
                I := AEntityEndingIndex;
              end
              else
                Inc(I);
            end
            else
            begin
              if TdxNumber.TryParse(TdxStringHelper.Substring(AEntity, 1), AIntValue) then
              begin
                C := Char(AIntValue);
                I := AEntityEndingIndex;
              end
              else
                Inc(I);
            end;
          except
            Inc(I);
          end;
        end
        else
        begin
          I := AEntityEndingIndex;
          AEntityChar := TdxHtmlEntities.Lookup(AEntity);
          if AEntityChar <> #0 then
            C := AEntityChar
          else
          begin
            AOutput.Write('&');
            AOutput.Write(AEntity);
            AOutput.Write(';');
            Inc(I);
            Continue;
          end;
        end;
      end;
    end;
    AOutput.Write(C);
    Inc(I);
  end;
end;

class function TdxHttpUtility.HtmlEncode(const AValue: string): string;
var
  AIndex, AStartIndex, ALength: Integer;
  ABuilder: TStringBuilder;
  C: Char;
begin
  if AValue = '' then
    Exit('');

  AIndex := IndexOfHtmlEncodingChars(AValue, 0);
  if AIndex = -1 then
    Exit(AValue);

  ALength := Length(AValue);
  ABuilder := TStringBuilder.Create(ALength + 5);
  try
    AStartIndex := 0;
    while True do
      if AIndex > AStartIndex then
      begin
        ABuilder.Append(AValue, AStartIndex, AIndex - AStartIndex);
        C := AValue[AIndex + 1];
        if C > '>' then
        begin
          ABuilder.Append('&#');
          ABuilder.Append(IntToStr(Ord(C)));
          ABuilder.Append(';');
        end
        else
        begin
          if C <> '"' then
          begin
            case C of
              '<':
                ABuilder.Append('&lt;');
              '>':
                ABuilder.Append('&gt;');
              '&':
                ABuilder.Append('&amp;');
            end;
          end
          else
            ABuilder.Append('&quot;');
        end;
        AStartIndex := AIndex + 1;
        if AStartIndex < ALength then
        begin
          AIndex := IndexOfHtmlEncodingChars(AValue, AStartIndex);
          if AIndex <> -1 then
            Continue;
          ABuilder.Append(AValue, AStartIndex, ALength - AStartIndex);
        end;
        Break;
      end;
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

class procedure TdxHttpUtility.HtmlEncode(const AHtml: string; AOutput: TTextWriter);
var
  AHtmlEncodingCharsIndex, ACount, AIndex: Integer;
  C: Char;
begin
  if AHtml = '' then
    Exit;

  AHtmlEncodingCharsIndex := IndexOfHtmlEncodingChars(AHtml, 0);
  if AHtmlEncodingCharsIndex = -1 then
  begin
    AOutput.Write(AHtml);
    Exit;
  end;
  ACount := Length(AHtml) - AHtmlEncodingCharsIndex;
  AIndex := 1;
  while AHtmlEncodingCharsIndex > 0 do
  begin
    AOutput.Write(AHtml[AIndex]);
    Inc(AIndex);
    Dec(AHtmlEncodingCharsIndex);
  end;
  while ACount > 0 do
  begin
    Dec(ACount);
    C := AHtml[AIndex];
    Inc(AIndex);
    if C <= '>' then
    begin
      if C <> '"' then
      begin
        case C of
          '<':
            AOutput.Write('&lt;');
          '=':
            AOutput.Write(C);
          '>':
            AOutput.Write('&gt;');
          '&':
            AOutput.Write('&amp;');
          else
            AOutput.Write(C);
        end;
        Continue;
      end;
      AOutput.Write('&quot;');
    end
    else
    begin
      if (C >= #$00A0) and (C < #$0100) then
      begin
        AOutput.Write('&#');
        AOutput.Write(IntToStr(Ord(C)));
        AOutput.Write(';');
      end
      else
        AOutput.Write(C);
    end;
  end;
end;

class function TdxHttpUtility.IndexOfHtmlAttributeEncodingChars(const AHtml: string; AStartPosition: Integer): Integer;
begin
  Result := TdxStringHelper.IndexOfAny(AHtml, ['"', '&', '<'], AStartPosition);
end;

class function TdxHttpUtility.IndexOfHtmlEncodingChars(const AHtml: string; AStartPosition: Integer): Integer;
var
  ALen: Integer;
  P: PChar;
  C: Char;
begin
  ALen := Length(AHtml);
  P := PChar(AHtml);
  while AStartPosition < ALen do
  begin
    C := P[AStartPosition];
    case C of
      '<', '>', '"', '&':
        Exit(AStartPosition);
      else
        if (C >= #$00A0) and (C < #$0100) then
           Exit(AStartPosition);
    end;
    Inc(AStartPosition);
  end;
  Result := -1;
end;

class function TdxHttpUtility.IntToHex(AValue: Integer): Char;
begin
  if AValue <= 9 then
    Result := Char(AValue + $30)
  else
    Result := Char(AValue - 10 + $61);
end;

class function TdxHttpUtility.IsNonAsciiByte(AValue: Byte): Boolean;
begin
  if AValue < $007F then
    Exit(AValue < $0020);
  Result := True;
end;

class function TdxHttpUtility.IsDirectorySeparatorChar(AValue: Char): Boolean;
begin
  Result := (AValue <> '\') or (AValue <> '/');
end;

class function TdxHttpUtility.IsSafe(AValue: Char): Boolean;
begin
  if ((AValue >= 'a') and (AValue <= 'z')) or ((AValue >= 'A') and (AValue <= 'Z')) or ((AValue >= '0') and (AValue <= '9')) then
    Exit(True);

  case AValue of
    #$27, '(', ')', '*', '-', '.', '_', '!':
      Exit(True);
  end;
  Result := False;
end;

class function TdxHttpUtility.UrlDecode(const AValue: string): string;
begin
  if AValue = '' then
    Exit('');
  Result := UrlDecode(AValue, TdxEncoding.UTF8);
end;

class function TdxHttpUtility.UrlDecode(const ABytes: TArray<Byte>; AEncoding: TEncoding): string;
begin
  if ABytes = nil then
    Exit('');
  Result := UrlDecode(ABytes, 0, Length(ABytes), AEncoding);
end;

class function TdxHttpUtility.UrlDecode(const AUrl: string; AEncoding: TEncoding): string;
begin
  if AUrl = '' then
    Exit('');
  Result := UrlDecodeStringFromStringInternal(AUrl, AEncoding);
end;

class function TdxHttpUtility.UrlDecode(const ABytes: TArray<Byte>; AOffset, ACount: Integer; AEncoding: TEncoding): string;
begin
  if ACount = 0 then
    Exit('');

  Assert(ABytes <> nil, 'bytes');
  if (AOffset < 0) or (AOffset > Length(ABytes)) then
    raise EArgumentOutOfRangeException.Create('offset');
  if (ACount < 0) or ((AOffset + ACount) > Length(ABytes)) then
    raise EArgumentOutOfRangeException.Create('count');
  Result := UrlDecodeStringFromBytesInternal(ABytes, AOffset, ACount, AEncoding);
end;

class function TdxHttpUtility.UrlDecodeBytesFromBytesInternal(const ABuffer: TArray<Byte>; AOffset, ACount: Integer): TArray<Byte>;
var
  ALength, I, AIndex, AByte1, AByte2: Integer;
  ACurrentByte: Byte;
begin
  ALength := 0;
  SetLength(Result, ACount);
  I := 0;
  while I < ACount do
  begin
    AIndex := AOffset + I;
    ACurrentByte := ABuffer[AIndex];
    if ACurrentByte = $2b then
      ACurrentByte := $20
    else
      if (ACurrentByte = $25) and (I < ACount - 2) then
      begin
        AByte1 := HexToInt(Char(ABuffer[AIndex + 1]));
        AByte2 := HexToInt(Char(ABuffer[AIndex + 2]));
        if (AByte1 >= 0) and (AByte2 >= 0) then
        begin
          ACurrentByte := Byte((AByte1 shl 4) or AByte2);
          Inc(I, 2);
        end;
      end;
    Result[ALength] := ACurrentByte;
    Inc(ALength);
    Inc(I);
  end;
  if ALength < Length(Result) then
    SetLength(Result, ALength);
end;

class function TdxHttpUtility.UrlDecodeStringFromBytesInternal(const ABuffer: TArray<Byte>; AOffset, ACount: Integer;
  AEncoding: TEncoding): string;
var
  ADecoder: TdxUrlDecoder;
  I, AIndex, AByte1, AByte2, AByte3, AByte4: Integer;
  ACurrentByte: Byte;
  AChr: Char;
begin
  ADecoder := TdxUrlDecoder.Create(ACount, AEncoding);
  try
    I := 0;
    while I < ACount do
    begin
      AIndex := AOffset + I;
      ACurrentByte := ABuffer[AIndex];
      if ACurrentByte = $2b then
        ACurrentByte := $20
      else
        if (ACurrentByte = $25) and (I < ACount - 2) then
        begin
          if (ABuffer[AIndex + 1] = $75) and (I < ACount - 5) then
          begin
            AByte1 := HexToInt(Char(ABuffer[AIndex + 2]));
            AByte2 := HexToInt(Char(ABuffer[AIndex + 3]));
            AByte3 := HexToInt(Char(ABuffer[AIndex + 4]));
            AByte4 := HexToInt(Char(ABuffer[AIndex + 5]));
            if (AByte1 < 0) or (AByte2 < 0) or (AByte3 < 0) or (AByte4 < 0) then
            begin
              ADecoder.AddByte(ACurrentByte);
              Inc(I);
              Continue;
            end;
            AChr := Char((AByte1 shl 12) or (AByte2 shl 8) or (AByte3 shl 4) or AByte4);
            Inc(I, 5 + 1);
            ADecoder.AddChar(AChr);
            Continue;
          end;
          AByte1 := HexToInt(Char(ABuffer[AIndex + 1]));
          AByte2 := HexToInt(Char(ABuffer[AIndex + 2]));
          if (AByte1 >= 0) and (AByte2 >= 0) then
          begin
            ACurrentByte := Byte((AByte1 shl 4) or AByte2);
            Inc(I, 2);
          end;
        end;
      ADecoder.AddByte(ACurrentByte);
      Inc(I);
    end;
    Result := ADecoder.GetString;
  finally
    ADecoder.Free;
  end;
end;

class function TdxHttpUtility.UrlDecodeStringFromStringInternal(const AUrl: string; AEncoding: TEncoding): string;
var
  ADecoder: TdxUrlDecoder;
  I, ALen, ANum1, ANum2, ANum3, ANum4: Integer;
  P: PChar;
  ACh: Char;
  B: Byte;
begin
  ALen := Length(AUrl);
  ADecoder := TdxUrlDecoder.Create(ALen, AEncoding);
  try
    I := 0;
    P := PChar(AUrl);
    while I < ALen do
    begin
      ACh := P[I];
      if ACh = '+' then
        ACh := ' '
      else
        if (ACh = '%') and (I < ALen - 2) then
        begin
          if (P[I + 1] = 'u') and (I < ALen - 5) then
          begin
            ANum1 := HexToInt(P[I + 2]);
            ANum2 := HexToInt(P[I + 3]);
            ANum3 := HexToInt(P[I + 4]);
            ANum4 := HexToInt(P[I + 5]);
            if (ANum1 < 0) or (ANum2 < 0) or (ANum3 < 0) or (ANum4 < 0) then
            begin
              if (Ord(ACh) and $ff80) = 0 then
                ADecoder.AddByte(Byte(ACh))
              else
                ADecoder.AddChar(ACh);
              Inc(I);
              Continue;
            end;
            ACh := Char((ANum1 shl 12) or (ANum2 shl 8) or (ANum3 shl 4) or ANum4);
            Inc(I, 5 + 1);
            ADecoder.AddChar(ACh);
            Continue;
          end;
          ANum1 := HexToInt(P[I + 1]);
          ANum2 := HexToInt(P[I + 2]);
          if (ANum1 >= 0) and (ANum2 >= 0) then
          begin
            B := Byte((ANum1 shl 4) or ANum2);
            Inc(I, 2 + 1);
            ADecoder.AddByte(B);
            Continue;
          end;
        end;
      if (Ord(ACh) and $ff80) = 0 then
        ADecoder.AddByte(Byte(ACh))
      else
        ADecoder.AddChar(ACh);
      Inc(I);
    end;
    Result := ADecoder.GetString;
  finally
    ADecoder.Free;
  end;
end;

class function TdxHttpUtility.UrlDecodeToBytes(const AValue: TArray<Byte>): TArray<Byte>;
begin
  if AValue = nil then
    Exit(nil);
  Result := UrlDecodeToBytes(AValue, 0, Length(AValue));
end;

class function TdxHttpUtility.UrlDecodeToBytes(const AUrl: string): TArray<Byte>;
begin
  if AUrl = '' then
    Exit(nil);
  Result := UrlDecodeToBytes(AUrl, TdxEncoding.UTF8);
end;

class function TdxHttpUtility.UrlDecodeToBytes(const AUrl: string; AEncoding: TEncoding): TArray<Byte>;
begin
  if AUrl = '' then
    Exit(nil);
  Result := UrlDecodeToBytes(AEncoding.GetBytes(AUrl));
end;

class function TdxHttpUtility.UrlDecodeToBytes(const ABytes: TArray<Byte>; AOffset, ACount: Integer): TArray<Byte>;
begin
  if ACount = 0 then
    Exit(nil);

  Assert(ABytes <> nil, 'bytes');
  if (AOffset < 0) or (AOffset > Length(ABytes)) then
    raise EArgumentOutOfRangeException.Create('offset');
  if (ACount < 0) or (AOffset + ACount > Length(ABytes)) then
    raise EArgumentOutOfRangeException.Create('count');
  Result := UrlDecodeBytesFromBytesInternal(ABytes, AOffset, ACount);
end;

class function TdxHttpUtility.UrlEncode(const ABytes: TArray<Byte>): string;
var
  AUrlBytes: TArray<Byte>;
begin
  if ABytes = nil then
    Exit('');
  AUrlBytes := UrlEncodeToBytes(ABytes);
  Result := TdxEncoding.ASCII.GetString(AUrlBytes, 0, Length(AUrlBytes));
end;

class function TdxHttpUtility.UrlEncode(const AUrl: string): string;
begin
  if AUrl = '' then
    Exit('');
  Result := UrlEncode(AUrl, TdxEncoding.UTF8);
end;

class function TdxHttpUtility.UrlEncode(const AUrl: string; AEncoding: TEncoding): string;
var
  AUrlBytes: TArray<Byte>;
begin
  if AUrl = '' then
    Exit('');
  AUrlBytes := UrlEncodeToBytes(AUrl, AEncoding);
  Result := TdxEncoding.ASCII.GetString(AUrlBytes, 0, Length(AUrlBytes));
end;

class function TdxHttpUtility.UrlEncode(const ABytes: TArray<Byte>; AOffset, ACount: Integer): string;
var
  AUrlBytes: TArray<Byte>;
begin
  if ABytes = nil then
    Exit('');
  AUrlBytes := UrlEncodeToBytes(ABytes, AOffset, ACount);
  Result := TdxEncoding.ASCII.GetString(AUrlBytes, 0, Length(AUrlBytes));
end;

class function TdxHttpUtility.UrlEncodeBytesToBytesInternal(const ABytes: TArray<Byte>; AOffset, ACount: Integer;
  AAlwaysCreateReturnValue: Boolean): TArray<Byte>;
var
  I, ASpacesCount, AUnsafeCharsCount, ANum: Integer;
  ACh: Char;
  B: Byte;
begin
  ASpacesCount := 0;
  AUnsafeCharsCount := 0;
  for I := 0 to ACount - 1 do
  begin
    ACh := Char(ABytes[AOffset + I]);
    if ACh = ' ' then
      Inc(ASpacesCount)
    else
      if not IsSafe(ACh) then
        Inc(AUnsafeCharsCount);
  end;
  if not AAlwaysCreateReturnValue and (ASpacesCount = 0) and (AUnsafeCharsCount = 0) then
    Exit(ABytes);
  SetLength(Result, ACount + AUnsafeCharsCount * 2);
  ANum := 0;
  for I := 0 to ACount - 1 do
  begin
    B := ABytes[AOffset + I];
    ACh := Char(B);
    if IsSafe(ACh) then
    begin
      Result[ANum] := B;
      Inc(ANum);
    end
    else
      if ACh = ' ' then
      begin
        Result[ANum] := $2b;
        Inc(ANum);
      end
      else
      begin
        Result[ANum] := $25;
        Inc(ANum);
        Result[ANum] := Byte(IntToHex((B shr 4) and $F));
        Inc(ANum);
        Result[ANum] := Byte(IntToHex(B and $F));
        Inc(ANum);
      end;
  end;
end;

class function TdxHttpUtility.UrlEncodeBytesToBytesInternalNonAscii(const ABytes: TArray<Byte>; AOffset, ACount: Integer;
  AAlwaysCreateReturnValue: Boolean): TArray<Byte>;
var
  I, ANum: Integer;
  B: Byte;
begin
  ANum := 0;
  for I := 0 to ACount - 1 do
    if IsNonAsciiByte(ABytes[AOffset + I]) then
      Inc(ANum);

  if not AAlwaysCreateReturnValue and (ANum = 0) then
    Exit(ABytes);

  SetLength(Result, ACount + (ANum * 2));
  ANum := 0;
  for I := 0 to ACount - 1 do
  begin
    B := ABytes[AOffset + I];
    if IsNonAsciiByte(B) then
    begin
      Result[ANum] := $25;
      Inc(ANum);
      Result[ANum] := Byte(IntToHex((B shr 4) and $F));
      Inc(ANum);
      Result[ANum] := Byte(IntToHex(B and $F));
      Inc(ANum);
    end
    else
    begin
      Result[ANum] := B;
      Inc(ANum);
    end;
  end;
end;

class function TdxHttpUtility.UrlEncodeNonAscii(const AUrl: string; AEncoding: TEncoding): string;
var
  ABytes: TArray<Byte>;
begin
  if AUrl = '' then
    Exit('');
  if AEncoding = nil then
    AEncoding := TdxEncoding.UTF8;

  ABytes := AEncoding.GetBytes(AUrl);
  ABytes := UrlEncodeBytesToBytesInternalNonAscii(ABytes, 0, Length(ABytes), False);
  Result := TdxEncoding.ASCII.GetString(ABytes, 0, Length(ABytes));
end;

class function TdxHttpUtility.UrlEncodeSpaces(const AUrl: string): string;
begin
  if (AUrl <> '') and (Pos(' ', AUrl) > 0) then
    Result := TdxStringHelper.Replace(AUrl, ' ', '%20')
  else
    Result := AUrl;
end;

class function TdxHttpUtility.UrlEncodeToBytes(const AUrl: string): TArray<Byte>;
begin
  if AUrl = '' then
    Exit(nil);
  Result := UrlEncodeToBytes(AUrl, TdxEncoding.UTF8);
end;

class function TdxHttpUtility.UrlEncodeToBytes(const ABytes: TArray<Byte>): TArray<Byte>;
begin
  if ABytes = nil then
    Exit(nil);
  Result := UrlEncodeToBytes(ABytes, 0, Length(ABytes));
end;

class function TdxHttpUtility.UrlEncodeToBytes(const AUrl: string; AEncoding: TEncoding): TArray<Byte>;
var
  ABytes: TArray<Byte>;
begin
  if AUrl = '' then
    Exit(nil);
  ABytes := AEncoding.GetBytes(AUrl);
  Result := UrlEncodeBytesToBytesInternal(ABytes, 0, Length(ABytes), False);
end;

class function TdxHttpUtility.UrlEncodeToBytes(const ABytes: TArray<Byte>; AOffset, ACount: Integer): TArray<Byte>;
begin
  if (ABytes = nil) and (ACount = 0) then
    Exit(nil);
  Assert(ABytes <> nil, 'bytes');
  if (AOffset < 0) or (AOffset > Length(ABytes)) then
    raise EArgumentOutOfRangeException.Create('offset');
  if (ACount < 0) or ((AOffset + ACount) > Length(ABytes)) then
    raise EArgumentOutOfRangeException.Create('count');
  Result := UrlEncodeBytesToBytesInternal(ABytes, AOffset, ACount, True);
end;

class function TdxHttpUtility.UrlEncodeUnicode(const AUrl: string): string;
begin
  if AUrl = '' then
    Exit('');
  Result := UrlEncodeUnicodeStringToStringInternal(AUrl, False);
end;

class function TdxHttpUtility.UrlEncodeUnicodeStringToStringInternal(const AUrl: string; AIgnoreAscii: Boolean): string;
var
  ABuilder: TStringBuilder;
  C: Char;
begin
  ABuilder := TStringBuilder.Create(Length(AUrl));
  try
    for C in AUrl do
    begin
      if (Ord(C) and $ff80) = 0 then
      begin
        if AIgnoreAscii or IsSafe(C) then
          ABuilder.Append(C)
        else
          if C = ' ' then
            ABuilder.Append('+')
          else
          begin
            ABuilder.Append('%');
            ABuilder.Append(IntToHex((Ord(C) shr 4) and $F));
            ABuilder.Append(IntToHex(Ord(C) and $F));
          end;
      end
      else
      begin
        ABuilder.Append('%u');
        ABuilder.Append(IntToHex((Ord(C) shr 12) and $F));
        ABuilder.Append(IntToHex((Ord(C) shr 8) and $F));
        ABuilder.Append(IntToHex((Ord(C) shr 4) and $F));
        ABuilder.Append(IntToHex(Ord(C) and $F));
      end;
    end;
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

class function TdxHttpUtility.UrlEncodeUnicodeToBytes(const AUrl: string): TArray<Byte>;
begin
  if AUrl = '' then
    Exit(nil);
  Result := TdxEncoding.ASCII.GetBytes(UrlEncodeUnicode(AUrl));
end;

class function TdxHttpUtility.UrlPathEncode(const AUrl: string): string;
var
  AArgumentsIndex: Integer;
begin
  if AUrl = '' then
    Exit('');
  AArgumentsIndex := TdxStringHelper.IndexOf(AUrl, '?');
  if AArgumentsIndex >= 0 then
    Exit(UrlPathEncode(TdxStringHelper.Substring(AUrl, 0, AArgumentsIndex)) + TdxStringHelper.Substring(AUrl, AArgumentsIndex));
  Result := UrlEncodeSpaces(UrlEncodeNonAscii(AUrl, TdxEncoding.UTF8));
end;

class function TdxHttpUtility.UrlEncodeToUnicodeCompatible(const AStringData: string): string;
var
  ABuilder: TStringBuilder;
  C: Char;
begin
  Result := UrlPathEncode(AStringData);
  ABuilder := TStringBuilder.Create(Length(Result));
  try
    for C in Result do
    begin
      if IsSpecialSymbol(C) then
        ABuilder.Append('\');
      ABuilder.Append(C);
    end;
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

class function TdxHttpUtility.IsSpecialSymbol(C: Char): Boolean;
begin
  Result := (C = '{') or (C = '}') or (C = '\');
end;

class function TdxHttpUtility.IsUncSharePath(const APath: string): Boolean;
begin
  Result := (Length(APath) > 2) and IsDirectorySeparatorChar(APath[1]) and IsDirectorySeparatorChar(APath[2]);
end;

{ TdxStateBag }

constructor TdxStateBag.Create;
begin
  inherited Create(TdxIStringComparer.Ordinal);
end;

function TdxStateBag.GetItem(const AKey: string): TValue;
begin
  if not ContainsKey(AKey) then
    Result := TValue.Empty
  else
    Result := inherited Items[AKey];
end;

procedure TdxStateBag.SetItem(const AKey: string; const Value: TValue);
begin
  if ContainsKey(AKey) then
    Remove(AKey);
  if not Value.IsEmpty then
    Add(AKey, Value);
end;

end.
