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

unit dxXMLClasses;

{$I cxVer.inc}
{$SCOPEDENUMS ON}

interface

uses
  Types, Classes, SysUtils, Generics.Defaults, Generics.Collections, Rtti,
  dxCore,
  dxGenerics;

type
  { EdxXmlException }

  EdxXmlException = class(EdxException)
  strict private
    FRes: string;
    FLineNumber: Integer;
    FLinePosition: Integer;
    FSourceUri: string;
    class function CreateMessage(const ARes: string; const AArgs: array of const; ALineNumber, ALinePosition: Integer): string; static;
    class function FormatUserMessage(const AMessage: string; ALineNumber: Integer; ALinePosition: Integer): string; static;
  protected
  public
    constructor Create(const ARes, AArg: string); overload;
    constructor Create(const ARes, AArg, ASourceUri: string); overload;
    constructor Create(const AMessage: string; AInnerException: Exception; ALineNumber: Integer; ALinePosition: Integer;
      const ASourceUri: string); overload;
    constructor Create(const ARes: string; const AArg: string; ALineNumber: Integer; ALinePosition: Integer); overload;
    constructor Create(const ARes: string; const AArgs: array of const; ALineNumber: Integer; ALinePosition: Integer;
      const ASourceUri: string = ''); overload;
    constructor Create(const ARes: string; const AArgs: array of const; AInnerException: Exception; ALineNumber: Integer;
      ALinePosition: Integer; const ASourceUri: string); overload;
    constructor Create(const ARes: string; const AArg: string; ALineNumber: Integer; ALinePosition: Integer;
      const ASourceUri: string); overload;

    class function BuildCharExceptionArgs(AInvChar: Char; ANextChar: Char): TArray<string>; overload; static;
    class function BuildCharExceptionArgs(const AData: TCharArray; ALength: Integer;
      AInvCharIndex: Integer): TArray<string>; overload; static;

    property LineNumber: Integer read FLineNumber;
    property LinePosition: Integer read FLinePosition;
  end;

  EdxXmlArgumentOutOfRangeException = class(EdxXmlException);
  EdxXmlArgumentNullException = class(EdxXmlException);
  EdxXmlArgumentException = class(EdxXmlException);
  EdxXmlInvalidOperationException = class(EdxXmlException);

  { TdxXmlReservedNs }

  TdxXmlReservedNs = class
  public const
    NsXml = 'http://www.w3.org/XML/1998/namespace';
    NsXmlNs = 'http://www.w3.org/2000/xmlns/';
    NsDataType = 'urn:schemas-microsoft-com:datatypes';
    NsDataTypeAlias = 'uuid:C2F41010-65B3-11D1-A29F-00AA00C14882';
    NsDataTypeOld = 'urn:uuid:C2F41010-65B3-11D1-A29F-00AA00C14882/';
    NsMsxsl = 'urn:schemas-microsoft-com:xslt';
    NsXdr = 'urn:schemas-microsoft-com:xml-data';
    NsXslDebug = 'urn:schemas-microsoft-com:xslt-debug';
    NsXdrAlias = 'uuid:BDC6E3F0-6DA3-11D1-A2A3-00AA00C14882';
    NsWdXsl = 'http://www.w3.org/TR/WD-xsl';
    NsXs = 'http://www.w3.org/2001/XMLSchema';
    NsXsd = 'http://www.w3.org/2001/XMLSchema-datatypes';
    NsXsi = 'http://www.w3.org/2001/XMLSchema-instance';
    NsXslt = 'http://www.w3.org/1999/XSL/Transform';
    NsExsltCommon = 'http://exslt.org/common';
    NsExsltDates = 'http://exslt.org/dates-and-times';
    NsExsltMath = 'http://exslt.org/math';
    NsExsltRegExps = 'http://exslt.org/regular-expressions';
    NsExsltSets = 'http://exslt.org/sets';
    NsExsltStrings = 'http://exslt.org/strings';
    NsXQueryFunc = 'http://www.w3.org/2003/11/xpath-functions';
    NsXQueryDataType = 'http://www.w3.org/2003/11/xpath-datatypes';
    NsCollationBase = 'http://collations.microsoft.com';
    NsCollCodePoint = 'http://www.w3.org/2004/10/xpath-functions/collation/codepoint';
    NsXsltInternal = 'http://schemas.microsoft.com/framework/2003/xml/xslt/internal';
  end;

  { TdxReplacementItem }

  TdxReplacementItem = record
  strict private
    FCharIndex: Integer;
    FReplaceWith: string;
  public
    constructor Create(ACharIndex: Integer; const AReplaceWith: string);

    property CharIndex: Integer read FCharIndex;
    property ReplaceWith: string read FReplaceWith;
  end;

  { TdxReplacementInfo }

  TdxReplacementInfo = class
  strict private
    FItems: TList<TdxReplacementItem>;
    FDeltaLength: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(ACharIndex: Integer; const AReplaceWith: string);

    property DeltaLength: Integer read FDeltaLength;
    property Items: TList<TdxReplacementItem> read FItems;
  end;

  { TdxFastCharacterMultiReplacement }

  TdxFastCharacterMultiReplacement = class
  strict private
    FBuffer: TStringBuilder;
  public
    constructor Create(AStringBuilder: TStringBuilder);
    function CreateReplacementInfo(const AText: string; const AReplaceTable: TdxCharStringDictionary): TdxReplacementInfo;
    function PerformReplacements(const AText: string; const AReplacementInfo: TdxReplacementInfo): string; overload;
    function PerformReplacements(const AText: string; const AReplaceTable: TdxCharStringDictionary): string; overload;
  end;

  { TdxSecureStringHasher }

  TdxSecureStringHasher = record
  strict private
    FHashCodeRandomizer: Cardinal;
  public
    class function Create: TdxSecureStringHasher; static;
    function Equals(const X, Y: string): Boolean;
    function GetHashCode(const AKey: string): Cardinal;
  end;

  { TdxXmlCharType }

  TdxXmlCharType = class
  public
    type
      TCharProperties = array[Char] of Byte;
    const
      Whitespace    = 1;
      Letter        = 2;
      NCStartNameSC = 4;
      NCNameSC      = 8;
      CharData      = 16;
      NCNameXml4e   = 32;
      Text          = 64;
      AttrValue     = 128;
      SurHighStart  = #$d800;
      SurHighEnd    = #$dbff;
      SurLowStart   = #$dc00;
      SurLowEnd     = #$dfff;
      SurMask       = #$fc00;
  strict private
    const
      PublicIdBitmap: array[0..7] of Word = ($2400, $0000, $FFBB, $AFFF, $FFFF, $87FF, $FFFE, $07FF);
    class var
      FCharProperties: TCharProperties;
      class constructor Initialize;
      class procedure InitRanges(const ARanges: string; Attribute: Byte); static;
  public
    class function CombineSurrogateChar(ALowChar, AHighChar: Char): Integer; static; inline;
    class procedure SplitSurrogateChar(ACombinedChar: Integer; out ALowChar: Char; out AHighChar: Char); static; inline;
    class function IsAttributeValueChar(C: Char): Boolean; static; inline;
    class function IsCharData(C: Char): Boolean; static; inline;
    class function IsTextChar(C: Char): Boolean; static; inline;
    class function IsSurrogate(C: Char): Boolean; static; inline;
    class function IsHighSurrogate(C: Char): Boolean; static; inline;
    class function IsLowSurrogate(C: Char): Boolean; static; inline;
    class function IsWhiteSpace(C: Char): Boolean; static; inline;
    class function IsOnlyCharData(const S: string): Integer; static; inline;
    class function IsOnlyWhitespace(const S: string): Boolean; static; inline;
    class function IsPubidChar(C: Char): Boolean; static; inline;
    class function IsPublicId(const S: string): Integer; static; inline;
    class function IsNameSingleChar(ACh: Char): Boolean; static; inline;
    class function IsNCNameSingleChar(ACh: Char): Boolean; static; inline;

    class property CharProperties: TCharProperties read FCharProperties;
  end;

  TdxXmlConformanceLevel = (
    Auto,
    Fragment,
    Document
  );

  TdxXmlNamespaceScope = (
    All,
    ExcludeXml,
    Local
  );

  IdxXmlNamespaceResolver = interface
  ['{087553CA-4239-4A8D-88E8-7150B9DACDEB}']
    function GetNamespacesInScope(scope: TdxXmlNamespaceScope): TdxStringsDictionary;
    function LookupNamespace(const prefix: string): string;
    function LookupPrefix(const namespaceName: string): string;
  end;

  { TdxXmlBasedExporterUtils }

  TdxXmlBasedExporterUtils = class
  strict private
    class var
      FPreProcessVariableValueStringBuilder: TStringBuilder;
      FXmlCharsReplacement: TdxFastCharacterMultiReplacement;
      FXmlCharsReplacementTable: TdxCharStringDictionary;
      FXmlCharsNoCrLfReplacementTable: TdxCharStringDictionary;
      FXmlCharsNoCrLfTabReplacementTable: TdxCharStringDictionary;
    class function CreateVariableValueReplacementTable: TdxCharStringDictionary; static;
    class function CreateVariableValueNoCrLfReplacementTable: TdxCharStringDictionary; static;
    class function CreateVariableValueNoTabCrLfReplacementTable: TdxCharStringDictionary; static;
  protected
    class constructor Initialize;
  {$IFDEF DELPHIXE}
    class destructor Finalize;
  {$ELSE}
    class procedure Finalize;
  {$ENDIF}
  public
    class function EncodeXmlChars(const AValue: string): string;
    class function EncodeXmlCharsNoCrLf(const AValue: string): string;
    class function EncodeXmlCharsXML1_0(const AValue: string): string;
  end;

function CreateInvalidSurrogatePairException(ALowChar, AHighChar: Char): EdxXmlException;

implementation

uses
  Windows, Character, Math,
  dxCharacters,
  dxStringHelper;

resourcestring
  SXmlInvalidSurrogatePair = 'The surrogate pair (%s, %s) is invalid. A high surrogate character (0xD800 - 0xDBFF) must ' +
    'always be paired with a low surrogate character (0xDC00 - 0xDFFF).';
  SXmlMessageWithErrorPosition = '%s Line %d, position %d';
  SXmlDefaultException = 'An XML error has occurred.';
  SXmlUserException = '%s';

function CharToHex(C: Char): string;
begin
  Result := Format('0x%.4x', [Ord(C)]);
end;

function CreateInvalidSurrogatePairException(ALowChar, AHighChar: Char): EdxXmlException;
begin
  Result := EdxXmlArgumentException.CreateFmt(SXmlInvalidSurrogatePair, [CharToHex(ALowChar), CharToHex(AHighChar)]);
end;

function IfThen(AValue: Boolean; const ATrue: Char; const AFalse: Char): Char; overload; inline;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{ TdxSecureStringHasher }

class function TdxSecureStringHasher.Create: TdxSecureStringHasher;
begin
  Result.FHashCodeRandomizer := GetTickCount;
end;

function TdxSecureStringHasher.Equals(const X, Y: string): Boolean;
begin
  Result := X = Y;
end;

function TdxSecureStringHasher.GetHashCode(const AKey: string): Cardinal;
var
  P: PChar;
begin
  Result := FHashCodeRandomizer;
  P := PChar(AKey);
  while P^ <> #0 do
  begin
    Inc(Result, ( Result shl 7 ) xor Ord(P^));
    Inc(P);
  end;

  Dec(Result, Result shr 17);
  Dec(Result, Result shr 11);
  Dec(Result, Result shr 5);
end;

{ TdxReplacementItem }

constructor TdxReplacementItem.Create(ACharIndex: Integer; const AReplaceWith: string);
begin
  FCharIndex := ACharIndex;
  FReplaceWith := AReplaceWith;
end;

{ TdxReplacementInfo }

constructor TdxReplacementInfo.Create;
begin
  FItems := TList<TdxReplacementItem>.Create;
end;

destructor TdxReplacementInfo.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TdxReplacementInfo.Add(ACharIndex: Integer; const AReplaceWith: string);
begin
  FItems.Add(TdxReplacementItem.Create(ACharIndex, AReplaceWith));
  FDeltaLength := FDeltaLength + Length(AReplaceWith) - 1;
end;

{ TdxFastCharacterMultiReplacement }

constructor TdxFastCharacterMultiReplacement.Create(AStringBuilder: TStringBuilder);
begin
  Assert(AStringBuilder <> nil);
  FBuffer := AStringBuilder;
end;

function TdxFastCharacterMultiReplacement.CreateReplacementInfo(const AText: string;
  const AReplaceTable: TdxCharStringDictionary): TdxReplacementInfo;
var
  AReplaceWith: string;
  I: Integer;
begin
  Result := nil;
  for I := Length(AText) downto 1 do
  begin
    if AReplaceTable.TryGetValue(AText[I], AReplaceWith) then
    begin
      if Result = nil then
        Result := TdxReplacementInfo.Create;
      Result.Add(I - 1, AReplaceWith);
    end;
  end;
end;

function TdxFastCharacterMultiReplacement.PerformReplacements(const AText: string;
  const AReplacementInfo: TdxReplacementInfo): string;
var
  AReplacementItems: TList<TdxReplacementItem>;
  ACount, I: Integer;
  AItem: TdxReplacementItem;
  AReplaceString: string;
begin
  if AReplacementInfo = nil then
    Exit(AText);

  FBuffer.Capacity := Max(FBuffer.Capacity, Length(AText) + AReplacementInfo.DeltaLength);
  FBuffer.Append(AText);
  AReplacementItems := AReplacementInfo.Items;
  ACount := AReplacementItems.Count;
  for I := 0 to ACount - 1 do
  begin
    AItem := AReplacementItems[I];
    FBuffer.Remove(AItem.CharIndex, 1);
    AReplaceString := AItem.ReplaceWith;
    if AReplaceString <> '' then
      FBuffer.Insert(AItem.CharIndex, AReplaceString);
  end;
  Result := FBuffer.ToString;
  FBuffer.Length := 0;
end;

function TdxFastCharacterMultiReplacement.PerformReplacements(const AText: string;
  const AReplaceTable: TdxCharStringDictionary): string;
var
  AReplacementInfo: TdxReplacementInfo;
begin
  AReplacementInfo := CreateReplacementInfo(AText, AReplaceTable);
  try
    Result := PerformReplacements(AText, AReplacementInfo);
  finally
    AReplacementInfo.Free;
  end;
end;

{ TdxXmlCharType }

class constructor TdxXmlCharType.Initialize;
const
  WhitespaceRanges = #$0009#$000A#$000D#$000D#$0020#$0020;
  NCStartNameRanges =
    #$0041#$005A#$005F#$005F#$0061#$007A#$00C0#$00D6#$00D8#$00F6#$00F8#$0131#$0134#$013E +
    #$0141#$0148#$014A#$017E#$0180#$01C3#$01CD#$01F0#$01F4#$01F5#$01FA#$0217#$0250#$02A8#$02BB#$02C1 +
    #$0386#$0386#$0388#$038A#$038C#$038C#$038E#$03A1#$03A3#$03CE#$03D0#$03D6#$03DA#$03DA#$03DC#$03DC +
    #$03DE#$03DE#$03E0#$03E0#$03E2#$03F3#$0401#$040C#$040E#$044F#$0451#$045C#$045E#$0481#$0490#$04C4 +
    #$04C7#$04C8#$04CB#$04CC#$04D0#$04EB#$04EE#$04F5#$04F8#$04F9#$0531#$0556#$0559#$0559#$0561#$0586 +
    #$05D0#$05EA#$05F0#$05F2#$0621#$063A#$0641#$064A#$0671#$06B7#$06BA#$06BE#$06C0#$06CE#$06D0#$06D3 +
    #$06D5#$06D5#$06E5#$06E6#$0905#$0939#$093D#$093D#$0958#$0961#$0985#$098C#$098F#$0990#$0993#$09A8 +
    #$09AA#$09B0#$09B2#$09B2#$09B6#$09B9#$09DC#$09DD#$09DF#$09E1#$09F0#$09F1#$0A05#$0A0A#$0A0F#$0A10 +
    #$0A13#$0A28#$0A2A#$0A30#$0A32#$0A33#$0A35#$0A36#$0A38#$0A39#$0A59#$0A5C#$0A5E#$0A5E#$0A72#$0A74 +
    #$0A85#$0A8B#$0A8D#$0A8D#$0A8F#$0A91#$0A93#$0AA8#$0AAA#$0AB0#$0AB2#$0AB3#$0AB5#$0AB9#$0ABD#$0ABD +
    #$0AE0#$0AE0#$0B05#$0B0C#$0B0F#$0B10#$0B13#$0B28#$0B2A#$0B30#$0B32#$0B33#$0B36#$0B39#$0B3D#$0B3D +
    #$0B5C#$0B5D#$0B5F#$0B61#$0B85#$0B8A#$0B8E#$0B90#$0B92#$0B95#$0B99#$0B9A#$0B9C#$0B9C#$0B9E#$0B9F +
    #$0BA3#$0BA4#$0BA8#$0BAA#$0BAE#$0BB5#$0BB7#$0BB9#$0C05#$0C0C#$0C0E#$0C10#$0C12#$0C28#$0C2A#$0C33 +
    #$0C35#$0C39#$0C60#$0C61#$0C85#$0C8C#$0C8E#$0C90#$0C92#$0CA8#$0CAA#$0CB3#$0CB5#$0CB9#$0CDE#$0CDE +
    #$0CE0#$0CE1#$0D05#$0D0C#$0D0E#$0D10#$0D12#$0D28#$0D2A#$0D39#$0D60#$0D61#$0E01#$0E2E#$0E30#$0E30 +
    #$0E32#$0E33#$0E40#$0E45#$0E81#$0E82#$0E84#$0E84#$0E87#$0E88#$0E8A#$0E8A#$0E8D#$0E8D#$0E94#$0E97 +
    #$0E99#$0E9F#$0EA1#$0EA3#$0EA5#$0EA5#$0EA7#$0EA7#$0EAA#$0EAB#$0EAD#$0EAE#$0EB0#$0EB0#$0EB2#$0EB3 +
    #$0EBD#$0EBD#$0EC0#$0EC4#$0F40#$0F47#$0F49#$0F69#$10A0#$10C5#$10D0#$10F6#$1100#$1100#$1102#$1103 +
    #$1105#$1107#$1109#$1109#$110B#$110C#$110E#$1112#$113C#$113C#$113E#$113E#$1140#$1140#$114C#$114C +
    #$114E#$114E#$1150#$1150#$1154#$1155#$1159#$1159#$115F#$1161#$1163#$1163#$1165#$1165#$1167#$1167 +
    #$1169#$1169#$116D#$116E#$1172#$1173#$1175#$1175#$119E#$119E#$11A8#$11A8#$11AB#$11AB#$11AE#$11AF +
    #$11B7#$11B8#$11BA#$11BA#$11BC#$11C2#$11EB#$11EB#$11F0#$11F0#$11F9#$11F9#$1E00#$1E9B#$1EA0#$1EF9 +
    #$1F00#$1F15#$1F18#$1F1D#$1F20#$1F45#$1F48#$1F4D#$1F50#$1F57#$1F59#$1F59#$1F5B#$1F5B#$1F5D#$1F5D +
    #$1F5F#$1F7D#$1F80#$1FB4#$1FB6#$1FBC#$1FBE#$1FBE#$1FC2#$1FC4#$1FC6#$1FCC#$1FD0#$1FD3#$1FD6#$1FDB +
    #$1FE0#$1FEC#$1FF2#$1FF4#$1FF6#$1FFC#$2126#$2126#$212A#$212B#$212E#$212E#$2180#$2182#$3007#$3007 +
    #$3021#$3029#$3041#$3094#$30A1#$30FA#$3105#$312C#$4E00#$9FA5#$AC00#$D7A3;

  NCNameRanges =
    #$002D#$002E#$0030#$0039#$0041#$005A#$005F#$005F#$0061#$007A#$00B7#$00B7#$00C0#$00D6#$00D8#$00F6 +
    #$00F8#$0131#$0134#$013E#$0141#$0148#$014A#$017E#$0180#$01C3#$01CD#$01F0#$01F4#$01F5#$01FA#$0217 +
    #$0250#$02A8#$02BB#$02C1#$02D0#$02D1#$0300#$0345#$0360#$0361#$0386#$038A#$038C#$038C#$038E#$03A1 +
    #$03A3#$03CE#$03D0#$03D6#$03DA#$03DA#$03DC#$03DC#$03DE#$03DE#$03E0#$03E0#$03E2#$03F3#$0401#$040C +
    #$040E#$044F#$0451#$045C#$045E#$0481#$0483#$0486#$0490#$04C4#$04C7#$04C8#$04CB#$04CC#$04D0#$04EB +
    #$04EE#$04F5#$04F8#$04F9#$0531#$0556#$0559#$0559#$0561#$0586#$0591#$05A1#$05A3#$05B9#$05BB#$05BD +
    #$05BF#$05BF#$05C1#$05C2#$05C4#$05C4#$05D0#$05EA#$05F0#$05F2#$0621#$063A#$0640#$0652#$0660#$0669 +
    #$0670#$06B7#$06BA#$06BE#$06C0#$06CE#$06D0#$06D3#$06D5#$06E8#$06EA#$06ED#$06F0#$06F9#$0901#$0903 +
    #$0905#$0939#$093C#$094D#$0951#$0954#$0958#$0963#$0966#$096F#$0981#$0983#$0985#$098C#$098F#$0990 +
    #$0993#$09A8#$09AA#$09B0#$09B2#$09B2#$09B6#$09B9#$09BC#$09BC#$09BE#$09C4#$09C7#$09C8#$09CB#$09CD +
    #$09D7#$09D7#$09DC#$09DD#$09DF#$09E3#$09E6#$09F1#$0A02#$0A02#$0A05#$0A0A#$0A0F#$0A10#$0A13#$0A28 +
    #$0A2A#$0A30#$0A32#$0A33#$0A35#$0A36#$0A38#$0A39#$0A3C#$0A3C#$0A3E#$0A42#$0A47#$0A48#$0A4B#$0A4D +
    #$0A59#$0A5C#$0A5E#$0A5E#$0A66#$0A74#$0A81#$0A83#$0A85#$0A8B#$0A8D#$0A8D#$0A8F#$0A91#$0A93#$0AA8 +
    #$0AAA#$0AB0#$0AB2#$0AB3#$0AB5#$0AB9#$0ABC#$0AC5#$0AC7#$0AC9#$0ACB#$0ACD#$0AE0#$0AE0#$0AE6#$0AEF +
    #$0B01#$0B03#$0B05#$0B0C#$0B0F#$0B10#$0B13#$0B28#$0B2A#$0B30#$0B32#$0B33#$0B36#$0B39#$0B3C#$0B43 +
    #$0B47#$0B48#$0B4B#$0B4D#$0B56#$0B57#$0B5C#$0B5D#$0B5F#$0B61#$0B66#$0B6F#$0B82#$0B83#$0B85#$0B8A +
    #$0B8E#$0B90#$0B92#$0B95#$0B99#$0B9A#$0B9C#$0B9C#$0B9E#$0B9F#$0BA3#$0BA4#$0BA8#$0BAA#$0BAE#$0BB5 +
    #$0BB7#$0BB9#$0BBE#$0BC2#$0BC6#$0BC8#$0BCA#$0BCD#$0BD7#$0BD7#$0BE7#$0BEF#$0C01#$0C03#$0C05#$0C0C +
    #$0C0E#$0C10#$0C12#$0C28#$0C2A#$0C33#$0C35#$0C39#$0C3E#$0C44#$0C46#$0C48#$0C4A#$0C4D#$0C55#$0C56 +
    #$0C60#$0C61#$0C66#$0C6F#$0C82#$0C83#$0C85#$0C8C#$0C8E#$0C90#$0C92#$0CA8#$0CAA#$0CB3#$0CB5#$0CB9 +
    #$0CBE#$0CC4#$0CC6#$0CC8#$0CCA#$0CCD#$0CD5#$0CD6#$0CDE#$0CDE#$0CE0#$0CE1#$0CE6#$0CEF#$0D02#$0D03 +
    #$0D05#$0D0C#$0D0E#$0D10#$0D12#$0D28#$0D2A#$0D39#$0D3E#$0D43#$0D46#$0D48#$0D4A#$0D4D#$0D57#$0D57 +
    #$0D60#$0D61#$0D66#$0D6F#$0E01#$0E2E#$0E30#$0E3A#$0E40#$0E4E#$0E50#$0E59#$0E81#$0E82#$0E84#$0E84 +
    #$0E87#$0E88#$0E8A#$0E8A#$0E8D#$0E8D#$0E94#$0E97#$0E99#$0E9F#$0EA1#$0EA3#$0EA5#$0EA5#$0EA7#$0EA7 +
    #$0EAA#$0EAB#$0EAD#$0EAE#$0EB0#$0EB9#$0EBB#$0EBD#$0EC0#$0EC4#$0EC6#$0EC6#$0EC8#$0ECD#$0ED0#$0ED9 +
    #$0F18#$0F19#$0F20#$0F29#$0F35#$0F35#$0F37#$0F37#$0F39#$0F39#$0F3E#$0F47#$0F49#$0F69#$0F71#$0F84 +
    #$0F86#$0F8B#$0F90#$0F95#$0F97#$0F97#$0F99#$0FAD#$0FB1#$0FB7#$0FB9#$0FB9#$10A0#$10C5#$10D0#$10F6 +
    #$1100#$1100#$1102#$1103#$1105#$1107#$1109#$1109#$110B#$110C#$110E#$1112#$113C#$113C#$113E#$113E +
    #$1140#$1140#$114C#$114C#$114E#$114E#$1150#$1150#$1154#$1155#$1159#$1159#$115F#$1161#$1163#$1163 +
    #$1165#$1165#$1167#$1167#$1169#$1169#$116D#$116E#$1172#$1173#$1175#$1175#$119E#$119E#$11A8#$11A8 +
    #$11AB#$11AB#$11AE#$11AF#$11B7#$11B8#$11BA#$11BA#$11BC#$11C2#$11EB#$11EB#$11F0#$11F0#$11F9#$11F9 +
    #$1E00#$1E9B#$1EA0#$1EF9#$1F00#$1F15#$1F18#$1F1D#$1F20#$1F45#$1F48#$1F4D#$1F50#$1F57#$1F59#$1F59 +
    #$1F5B#$1F5B#$1F5D#$1F5D#$1F5F#$1F7D#$1F80#$1FB4#$1FB6#$1FBC#$1FBE#$1FBE#$1FC2#$1FC4#$1FC6#$1FCC +
    #$1FD0#$1FD3#$1FD6#$1FDB#$1FE0#$1FEC#$1FF2#$1FF4#$1FF6#$1FFC#$20D0#$20DC#$20E1#$20E1#$2126#$2126 +
    #$212A#$212B#$212E#$212E#$2180#$2182#$3005#$3005#$3007#$3007#$3021#$302F#$3031#$3035#$3041#$3094 +
    #$3099#$309A#$309D#$309E#$30A1#$30FA#$30FC#$30FE#$3105#$312C#$4E00#$9FA5#$AC00#$D7A3;
  CharDataRanges = #$0009#$000A#$000D#$000D#$0020#$D7FF#$E000#$FFFD;

  PublicIDRanges =
    #$000A#$000A#$000D#$000D#$0020#$0021#$0023#$0025#$0027#$003B#$003D#$003D#$003F#$005A#$005F#$005F +
    #$0061#$007A;

  TextRanges =
    #$0020#$0025#$0027#$003B#$003D#$005C#$005E#$D7FF#$E000#$FFFD;

  AttrValueRanges =
    #$0020#$0021#$0023#$0025#$0028#$003B#$003D#$003D#$003F#$D7FF#$E000#$FFFD;

  LetterXml4eRanges =
    #$0041#$005A#$0061#$007A#$00C0#$00D6#$00D8#$00F6#$00F8#$0131#$0134#$013E#$0141#$0148#$014A#$017E +
    #$0180#$01C3#$01CD#$01F0#$01F4#$01F5#$01FA#$0217#$0250#$02A8#$02BB#$02C1#$0386#$0386#$0388#$038A +
    #$038C#$038C#$038E#$03A1#$03A3#$03CE#$03D0#$03D6#$03DA#$03DA#$03DC#$03DC#$03DE#$03DE#$03E0#$03E0 +
    #$03E2#$03F3#$0401#$040C#$040E#$044F#$0451#$045C#$045E#$0481#$0490#$04C4#$04C7#$04C8#$04CB#$04CC +
    #$04D0#$04EB#$04EE#$04F5#$04F8#$04F9#$0531#$0556#$0559#$0559#$0561#$0586#$05D0#$05EA#$05F0#$05F2 +
    #$0621#$063A#$0641#$064A#$0671#$06B7#$06BA#$06BE#$06C0#$06CE#$06D0#$06D3#$06D5#$06D5#$06E5#$06E6 +
    #$0905#$0939#$093D#$093D#$0958#$0961#$0985#$098C#$098F#$0990#$0993#$09A8#$09AA#$09B0#$09B2#$09B2 +
    #$09B6#$09B9#$09DC#$09DD#$09DF#$09E1#$09F0#$09F1#$0A05#$0A0A#$0A0F#$0A10#$0A13#$0A28#$0A2A#$0A30 +
    #$0A32#$0A33#$0A35#$0A36#$0A38#$0A39#$0A59#$0A5C#$0A5E#$0A5E#$0A72#$0A74#$0A85#$0A8B#$0A8D#$0A8D +
    #$0A8F#$0A91#$0A93#$0AA8#$0AAA#$0AB0#$0AB2#$0AB3#$0AB5#$0AB9#$0ABD#$0ABD#$0AE0#$0AE0#$0B05#$0B0C +
    #$0B0F#$0B10#$0B13#$0B28#$0B2A#$0B30#$0B32#$0B33#$0B36#$0B39#$0B3D#$0B3D#$0B5C#$0B5D#$0B5F#$0B61 +
    #$0B85#$0B8A#$0B8E#$0B90#$0B92#$0B95#$0B99#$0B9A#$0B9C#$0B9C#$0B9E#$0B9F#$0BA3#$0BA4#$0BA8#$0BAA +
    #$0BAE#$0BB5#$0BB7#$0BB9#$0C05#$0C0C#$0C0E#$0C10#$0C12#$0C28#$0C2A#$0C33#$0C35#$0C39#$0C60#$0C61 +
    #$0C85#$0C8C#$0C8E#$0C90#$0C92#$0CA8#$0CAA#$0CB3#$0CB5#$0CB9#$0CDE#$0CDE#$0CE0#$0CE1#$0D05#$0D0C +
    #$0D0E#$0D10#$0D12#$0D28#$0D2A#$0D39#$0D60#$0D61#$0E01#$0E2E#$0E30#$0E30#$0E32#$0E33#$0E40#$0E45 +
    #$0E81#$0E82#$0E84#$0E84#$0E87#$0E88#$0E8A#$0E8A#$0E8D#$0E8D#$0E94#$0E97#$0E99#$0E9F#$0EA1#$0EA3 +
    #$0EA5#$0EA5#$0EA7#$0EA7#$0EAA#$0EAB#$0EAD#$0EAE#$0EB0#$0EB0#$0EB2#$0EB3#$0EBD#$0EBD#$0EC0#$0EC4 +
    #$0F40#$0F47#$0F49#$0F69#$10A0#$10C5#$10D0#$10F6#$1100#$1100#$1102#$1103#$1105#$1107#$1109#$1109 +
    #$110B#$110C#$110E#$1112#$113C#$113C#$113E#$113E#$1140#$1140#$114C#$114C#$114E#$114E#$1150#$1150 +
    #$1154#$1155#$1159#$1159#$115F#$1161#$1163#$1163#$1165#$1165#$1167#$1167#$1169#$1169#$116D#$116E +
    #$1172#$1173#$1175#$1175#$119E#$119E#$11A8#$11A8#$11AB#$11AB#$11AE#$11AF#$11B7#$11B8#$11BA#$11BA +
    #$11BC#$11C2#$11EB#$11EB#$11F0#$11F0#$11F9#$11F9#$1E00#$1E9B#$1EA0#$1EF9#$1F00#$1F15#$1F18#$1F1D +
    #$1F20#$1F45#$1F48#$1F4D#$1F50#$1F57#$1F59#$1F59#$1F5B#$1F5B#$1F5D#$1F5D#$1F5F#$1F7D#$1F80#$1FB4 +
    #$1FB6#$1FBC#$1FBE#$1FBE#$1FC2#$1FC4#$1FC6#$1FCC#$1FD0#$1FD3#$1FD6#$1FDB#$1FE0#$1FEC#$1FF2#$1FF4 +
    #$1FF6#$1FFC#$2126#$2126#$212A#$212B#$212E#$212E#$2180#$2182#$3007#$3007#$3021#$3029#$3041#$3094 +
    #$30A1#$30FA#$3105#$312C#$4E00#$9FA5#$AC00#$D7A3;

  NCNameXml4eRanges =
    #$002D#$002E#$0030#$0039#$0041#$005A#$005F#$005F#$0061#$007A#$00B7#$00B7#$00C0#$00D6#$00D8#$00F6 +
    #$00F8#$0131#$0134#$013E#$0141#$0148#$014A#$017E#$0180#$01C3#$01CD#$01F0#$01F4#$01F5#$01FA#$0217 +
    #$0250#$02A8#$02BB#$02C1#$02D0#$02D1#$0300#$0345#$0360#$0361#$0386#$038A#$038C#$038C#$038E#$03A1 +
    #$03A3#$03CE#$03D0#$03D6#$03DA#$03DA#$03DC#$03DC#$03DE#$03DE#$03E0#$03E0#$03E2#$03F3#$0401#$040C +
    #$040E#$044F#$0451#$045C#$045E#$0481#$0483#$0486#$0490#$04C4#$04C7#$04C8#$04CB#$04CC#$04D0#$04EB +
    #$04EE#$04F5#$04F8#$04F9#$0531#$0556#$0559#$0559#$0561#$0586#$0591#$05A1#$05A3#$05B9#$05BB#$05BD +
    #$05BF#$05BF#$05C1#$05C2#$05C4#$05C4#$05D0#$05EA#$05F0#$05F2#$0621#$063A#$0640#$0652#$0660#$0669 +
    #$0670#$06B7#$06BA#$06BE#$06C0#$06CE#$06D0#$06D3#$06D5#$06E8#$06EA#$06ED#$06F0#$06F9#$0901#$0903 +
    #$0905#$0939#$093C#$094D#$0951#$0954#$0958#$0963#$0966#$096F#$0981#$0983#$0985#$098C#$098F#$0990 +
    #$0993#$09A8#$09AA#$09B0#$09B2#$09B2#$09B6#$09B9#$09BC#$09BC#$09BE#$09C4#$09C7#$09C8#$09CB#$09CD +
    #$09D7#$09D7#$09DC#$09DD#$09DF#$09E3#$09E6#$09F1#$0A02#$0A02#$0A05#$0A0A#$0A0F#$0A10#$0A13#$0A28 +
    #$0A2A#$0A30#$0A32#$0A33#$0A35#$0A36#$0A38#$0A39#$0A3C#$0A3C#$0A3E#$0A42#$0A47#$0A48#$0A4B#$0A4D +
    #$0A59#$0A5C#$0A5E#$0A5E#$0A66#$0A74#$0A81#$0A83#$0A85#$0A8B#$0A8D#$0A8D#$0A8F#$0A91#$0A93#$0AA8 +
    #$0AAA#$0AB0#$0AB2#$0AB3#$0AB5#$0AB9#$0ABC#$0AC5#$0AC7#$0AC9#$0ACB#$0ACD#$0AE0#$0AE0#$0AE6#$0AEF +
    #$0B01#$0B03#$0B05#$0B0C#$0B0F#$0B10#$0B13#$0B28#$0B2A#$0B30#$0B32#$0B33#$0B36#$0B39#$0B3C#$0B43 +
    #$0B47#$0B48#$0B4B#$0B4D#$0B56#$0B57#$0B5C#$0B5D#$0B5F#$0B61#$0B66#$0B6F#$0B82#$0B83#$0B85#$0B8A +
    #$0B8E#$0B90#$0B92#$0B95#$0B99#$0B9A#$0B9C#$0B9C#$0B9E#$0B9F#$0BA3#$0BA4#$0BA8#$0BAA#$0BAE#$0BB5 +
    #$0BB7#$0BB9#$0BBE#$0BC2#$0BC6#$0BC8#$0BCA#$0BCD#$0BD7#$0BD7#$0BE7#$0BEF#$0C01#$0C03#$0C05#$0C0C +
    #$0C0E#$0C10#$0C12#$0C28#$0C2A#$0C33#$0C35#$0C39#$0C3E#$0C44#$0C46#$0C48#$0C4A#$0C4D#$0C55#$0C56 +
    #$0C60#$0C61#$0C66#$0C6F#$0C82#$0C83#$0C85#$0C8C#$0C8E#$0C90#$0C92#$0CA8#$0CAA#$0CB3#$0CB5#$0CB9 +
    #$0CBE#$0CC4#$0CC6#$0CC8#$0CCA#$0CCD#$0CD5#$0CD6#$0CDE#$0CDE#$0CE0#$0CE1#$0CE6#$0CEF#$0D02#$0D03 +
    #$0D05#$0D0C#$0D0E#$0D10#$0D12#$0D28#$0D2A#$0D39#$0D3E#$0D43#$0D46#$0D48#$0D4A#$0D4D#$0D57#$0D57 +
    #$0D60#$0D61#$0D66#$0D6F#$0E01#$0E2E#$0E30#$0E3A#$0E40#$0E4E#$0E50#$0E59#$0E81#$0E82#$0E84#$0E84 +
    #$0E87#$0E88#$0E8A#$0E8A#$0E8D#$0E8D#$0E94#$0E97#$0E99#$0E9F#$0EA1#$0EA3#$0EA5#$0EA5#$0EA7#$0EA7 +
    #$0EAA#$0EAB#$0EAD#$0EAE#$0EB0#$0EB9#$0EBB#$0EBD#$0EC0#$0EC4#$0EC6#$0EC6#$0EC8#$0ECD#$0ED0#$0ED9 +
    #$0F18#$0F19#$0F20#$0F29#$0F35#$0F35#$0F37#$0F37#$0F39#$0F39#$0F3E#$0F47#$0F49#$0F69#$0F71#$0F84 +
    #$0F86#$0F8B#$0F90#$0F95#$0F97#$0F97#$0F99#$0FAD#$0FB1#$0FB7#$0FB9#$0FB9#$10A0#$10C5#$10D0#$10F6 +
    #$1100#$1100#$1102#$1103#$1105#$1107#$1109#$1109#$110B#$110C#$110E#$1112#$113C#$113C#$113E#$113E +
    #$1140#$1140#$114C#$114C#$114E#$114E#$1150#$1150#$1154#$1155#$1159#$1159#$115F#$1161#$1163#$1163 +
    #$1165#$1165#$1167#$1167#$1169#$1169#$116D#$116E#$1172#$1173#$1175#$1175#$119E#$119E#$11A8#$11A8 +
    #$11AB#$11AB#$11AE#$11AF#$11B7#$11B8#$11BA#$11BA#$11BC#$11C2#$11EB#$11EB#$11F0#$11F0#$11F9#$11F9 +
    #$1E00#$1E9B#$1EA0#$1EF9#$1F00#$1F15#$1F18#$1F1D#$1F20#$1F45#$1F48#$1F4D#$1F50#$1F57#$1F59#$1F59 +
    #$1F5B#$1F5B#$1F5D#$1F5D#$1F5F#$1F7D#$1F80#$1FB4#$1FB6#$1FBC#$1FBE#$1FBE#$1FC2#$1FC4#$1FC6#$1FCC +
    #$1FD0#$1FD3#$1FD6#$1FDB#$1FE0#$1FEC#$1FF2#$1FF4#$1FF6#$1FFC#$20D0#$20DC#$20E1#$20E1#$2126#$2126 +
    #$212A#$212B#$212E#$212E#$2180#$2182#$3005#$3005#$3007#$3007#$3021#$302F#$3031#$3035#$3041#$3094 +
    #$3099#$309A#$309D#$309E#$30A1#$30FA#$30FC#$30FE#$3105#$312C#$4E00#$9FA5#$AC00#$D7A3;
begin
  InitRanges(WhitespaceRanges,  Whitespace);
  InitRanges(LetterXml4eRanges, Letter);
  InitRanges(NCStartNameRanges, NCStartNameSC);
  InitRanges(NCNameRanges,      NCNameSC);
  InitRanges(CharDataRanges,    CharData);
  InitRanges(NCNameXml4eRanges, NCNameXml4e);
  InitRanges(TextRanges,        Text);
  InitRanges(AttrValueRanges,   AttrValue);
end;

class procedure TdxXmlCharType.InitRanges(const ARanges: string; Attribute: Byte);
var
  P: PChar;
  AChar, AEndChar: Char;
  L: Integer;
begin
  L := Length(ARanges);
  P := PChar(ARanges);
  while L > 0 do
  begin
    AChar    := P^;
    Inc(P);
    AEndChar := P^;
    while AChar <= AEndChar do
    begin
      FCharProperties[AChar] := FCharProperties[AChar] or Attribute;
      Inc(AChar);
    end;
    Inc(P);
    Dec(L, 2);
  end;
end;

class function TdxXmlCharType.CombineSurrogateChar(ALowChar, AHighChar: Char): Integer;
begin
  Result := (Ord(ALowChar) - Ord(SurLowStart)) or ((Ord(AHighChar) - Ord(SurHighStart)) shl 10) + $10000;
end;

class procedure TdxXmlCharType.SplitSurrogateChar(ACombinedChar: Integer; out ALowChar: Char; out AHighChar: Char);
var
  V: Integer;
begin
  V := ACombinedChar - $10000;
  ALowChar := Char(Ord(SurLowStart) + V mod 1024);
  AHighChar := Char(Ord(SurHighStart) + V div 1024);
end;

class function TdxXmlCharType.IsAttributeValueChar(C: Char): Boolean;
begin
  Result := (FCharProperties[C] and AttrValue) <> 0;
end;

class function TdxXmlCharType.IsCharData(C: Char): Boolean;
begin
  Result := (FCharProperties[C] and CharData) <> 0;
end;

class function TdxXmlCharType.IsSurrogate(C: Char): Boolean;
begin
  Result := (C >= SurHighStart) and (C <= SurLowEnd);
end;

class function TdxXmlCharType.IsTextChar(C: Char): Boolean;
begin
  Result := (FCharProperties[C] and Text) <> 0;
end;

class function TdxXmlCharType.IsHighSurrogate(C: Char): Boolean;
begin
  Result := (C >= SurHighStart) and (C <= SurHighEnd);
end;

class function TdxXmlCharType.IsLowSurrogate(C: Char): Boolean;
begin
  Result := (C >= SurLowStart) and (C <= SurLowEnd);
end;

class function TdxXmlCharType.IsNCNameSingleChar(ACh: Char): Boolean;
begin
  Result := (FCharProperties[ACh] and NCNameSC) <> 0;
end;

class function TdxXmlCharType.IsNameSingleChar(ACh: Char): Boolean;
begin
  Result := IsNCNameSingleChar(ACh) or (ACh = ':');
end;

class function TdxXmlCharType.IsOnlyCharData(const S: string): Integer;
var
  I: Integer;
begin
  I := 1;
  while I <= Length(S) do
  begin
    if (FCharProperties[S[I]] and CharData) = 0 then
    begin
      if (I + 1 > Length(S)) or
        not (TdxXmlCharType.IsHighSurrogate(S[I]) and TdxXmlCharType.IsLowSurrogate(S[I + 1])) then
        Exit(I)
      else
        Inc(I);
    end;
    Inc(I);
  end;
  Result := 0;
end;

class function TdxXmlCharType.IsOnlyWhitespace(const S: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if (FCharProperties[S[I]] and Whitespace) = 0 then
      Exit(False);
  Result := True;
end;

class function TdxXmlCharType.IsWhiteSpace(C: Char): Boolean;
begin
  Result := FCharProperties[C] and Whitespace <> 0;
end;

class function TdxXmlCharType.IsPubidChar(C: Char): Boolean;
begin
  if C < #$0080 then
    Result := (PublicIdBitmap[Ord(C) shr 4] and (1 shl (Ord(C) and $0F))) <> 0
  else
    Result := False;
end;

class function TdxXmlCharType.IsPublicId(const S: string): Integer;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if not IsPubidChar(S[I]) then
      Exit(I);
  Result := 0;
end;

{ TdxXmlBasedExporterUtils }

class constructor TdxXmlBasedExporterUtils.Initialize;
begin
  FXmlCharsReplacementTable := CreateVariableValueReplacementTable;
  FXmlCharsNoCrLfReplacementTable := CreateVariableValueNoCrLfReplacementTable;
  FXmlCharsNoCrLfTabReplacementTable := CreateVariableValueNoTabCrLfReplacementTable;
  FPreProcessVariableValueStringBuilder := TStringBuilder.Create;
  FXmlCharsReplacement := TdxFastCharacterMultiReplacement.Create(FPreProcessVariableValueStringBuilder);
end;

{$IFDEF DELPHIXE}
class destructor TdxXmlBasedExporterUtils.Finalize;
{$ELSE}
class procedure TdxXmlBasedExporterUtils.Finalize;
{$ENDIF}
begin
  FreeAndNil(FXmlCharsReplacementTable);
  FreeAndNil(FXmlCharsNoCrLfReplacementTable);
  FreeAndNil(FXmlCharsNoCrLfTabReplacementTable);
  FreeAndNil(FPreProcessVariableValueStringBuilder);
  FreeAndNil(FXmlCharsReplacement);
end;

class function TdxXmlBasedExporterUtils.CreateVariableValueReplacementTable: TdxCharStringDictionary;
var
  I: Char;
begin
  Result := TdxCharStringDictionary.Create;
  for I := #0 to #31 do
    Result.Add(I, Format('_x%s_', [TdxStringHelper.ToHex(I)]));
  Result.Add(#$FFFF, '_xffff_');
end;

class function TdxXmlBasedExporterUtils.CreateVariableValueNoCrLfReplacementTable: TdxCharStringDictionary;
var
  I: Char;
begin
  Result := TdxCharStringDictionary.Create;
  for I := #0 to #31 do
  begin
    if (I <> #13) and (I <> #10) then
      Result.Add(I, Format('_x%s_', [TdxStringHelper.ToHex(I)]));
  end;
  Result.Add(#$FFFF, '_xffff_');
end;

class function TdxXmlBasedExporterUtils.CreateVariableValueNoTabCrLfReplacementTable: TdxCharStringDictionary;
var
  I: Char;
begin
  Result := TdxCharStringDictionary.Create;
  for I := #0 to #31 do
  begin
    if (I <> #10) and (I <> #13) and (I <> #9) then
      Result.Add(I, Format('_x%s_', [TdxStringHelper.ToHex(I)]));
  end;
  Result.Add(#$FFFF, '_xffff_');
end;

class function TdxXmlBasedExporterUtils.EncodeXmlChars(const AValue: string): string;
begin
  Result := FXmlCharsReplacement.PerformReplacements(AValue, FXmlCharsReplacementTable);
end;

class function TdxXmlBasedExporterUtils.EncodeXmlCharsNoCrLf(const AValue: string): string;
begin
  Result := FXmlCharsReplacement.PerformReplacements(AValue, FXmlCharsNoCrLfReplacementTable);
end;

class function TdxXmlBasedExporterUtils.EncodeXmlCharsXML1_0(const AValue: string): string;
begin
  Result := FXmlCharsReplacement.PerformReplacements(AValue, FXmlCharsNoCrLfTabReplacementTable);
end;

{ EdxXmlException }

constructor EdxXmlException.Create(const AMessage: string; AInnerException: Exception; ALineNumber,
  ALinePosition: Integer; const ASourceUri: string);
begin
  inherited Create(FormatUserMessage(AMessage, ALineNumber, ALinePosition));
  FSourceUri := ASourceUri;
  FLineNumber := ALineNumber;
  FLinePosition := ALinePosition;
end;

constructor EdxXmlException.Create(const ARes, AArg: string; ALineNumber, ALinePosition: Integer);
begin
  Create(ARes, [AArg], nil, ALineNumber, ALinePosition, '');
end;

constructor EdxXmlException.Create(const ARes: string; const AArgs: array of const; ALineNumber,
  ALinePosition: Integer; const ASourceUri: string);
begin
  Create(ARes, AArgs, nil, ALineNumber, ALinePosition, ASourceUri);
end;

class function EdxXmlException.CreateMessage(const ARes: string; const AArgs: array of const; ALineNumber,
  ALinePosition: Integer): string;
var
  AMessage: string;
begin
  try
    if ALineNumber = 0 then
      Result := Format(ARes, AArgs)
    else
    begin
      AMessage := Format(ARes, AArgs);
      Result := Format(SXmlMessageWithErrorPosition, [AMessage, ALineNumber, ALinePosition]);
    end;
  except
    Result := '';
  end;
end;

class function EdxXmlException.FormatUserMessage(const AMessage: string; ALineNumber, ALinePosition: Integer): string;
begin
  if AMessage = '' then
    Result := CreateMessage(SXmlDefaultException, [], ALineNumber, ALinePosition)
  else
  begin
    if (ALineNumber = 0) and (ALinePosition = 0) then
      Result := AMessage
    else
      Result := CreateMessage(SXmlUserException, [AMessage], ALineNumber, ALinePosition);
  end;
end;

constructor EdxXmlException.Create(const ARes: string; const AArgs: array of const; AInnerException: Exception;
  ALineNumber, ALinePosition: Integer; const ASourceUri: string);
begin
  inherited Create(CreateMessage(ARes, AArgs, ALineNumber, ALinePosition));
  FRes := ARes;
  FSourceUri := ASourceUri;
  FLineNumber := ALineNumber;
  FLinePosition := ALinePosition;
end;

class function EdxXmlException.BuildCharExceptionArgs(AInvChar, ANextChar: Char): TArray<string>;
var
  AAStringList: TArray<string>;
  ACombinedChar: Integer;
begin
  SetLength(AAStringList, 2);
  if TdxXmlCharType.IsHighSurrogate(AInvChar) and (ANextChar <> #0) then
  begin
    ACombinedChar := TdxXmlCharType.CombineSurrogateChar(ANextChar, AInvChar);
    AAStringList[0] := AInvChar + ANextChar;
    AAStringList[1] := Format('0x%2x', [Ord(ACombinedChar)]);
  end
  else
  begin
    if Integer(AInvChar) = 0 then
      AAStringList[0] := '.'
    else
      AAStringList[0] := AInvChar;
    AAStringList[1] := Format('0x%2x', [Ord(AInvChar)]);
  end;
  Result := AAStringList;
end;

class function EdxXmlException.BuildCharExceptionArgs(const AData: TCharArray; ALength,
  AInvCharIndex: Integer): TArray<string>;
begin
  Assert(AInvCharIndex < Length(AData));
  Assert(AInvCharIndex < ALength);
  Assert(ALength <= Length(AData));

  Result := BuildCharExceptionArgs(AData[AInvCharIndex], IfThen(AInvCharIndex + 1 < ALength,
    AData[AInvCharIndex + 1], #0));
end;

constructor EdxXmlException.Create(const ARes, AArg: string; ALineNumber, ALinePosition: Integer;
  const ASourceUri: string);
begin
  Create(ARes, [AArg], nil, ALineNumber, ALinePosition, ASourceUri);
end;

constructor EdxXmlException.Create(const ARes, AArg, ASourceUri: string);
begin
  Create(ARes, [AArg], nil, 0, 0, ASourceUri);
end;

constructor EdxXmlException.Create(const ARes, AArg: string);
begin
  Create(ARes, [AArg], nil, 0, 0, '');
end;

{$IFNDEF DELPHIXE}
initialization

finalization
  TdxXmlBasedExporterUtils.Finalize;
{$ENDIF}
end.
