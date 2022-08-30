{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSPDFFonts;

interface

{$I cxVer.inc}

uses
  Windows, SysUtils, Classes, Graphics, cxClasses, dxCore,
  dxPSPDFExportCore, dxPSPDFStrings, dxPSTrueTypeFont;

type
  TdxPSPDFCIDFont = class;
  TdxPSPDFCIDFontConversionTable = class;
  TdxPSPDFFontDescriptor = class;

  { TdxPSPDFFontInfo }

  TdxPSPDFFontInfo = class(TdxPSPDFObject)
  private
    FOwner: TdxPSPDFCustomFont;
  public
    constructor Create(AOwner: TdxPSPDFCustomFont); virtual;
    //
    property Owner: TdxPSPDFCustomFont read FOwner;
  end;

  { TdxPSPDFFontFile }

  TdxPSPDFFontFile = class(TdxPSPDFFontInfo)
  private
    FFontData: TMemoryStream;
    function GetFontDataValid: Boolean;
  protected
    procedure FontDataNeeded; virtual;
    function GetContentStreamType: TdxPSPDFStreamType; override;
    procedure WriteContentStream(AWriter: TdxPSPDFWriter); override;
    procedure WriteHeader(AWriter: TdxPSPDFWriter); override;
  public
    destructor Destroy; override;
    //
    property FontData: TMemoryStream read FFontData;
    property FontDataValid: Boolean read GetFontDataValid;
  end;

  { TdxPSPDFFontDescriptor }

  TdxPSPDFFontDescriptor = class(TdxPSPDFFontInfo)
  private
    FAscent: Integer;
    FAveCharWidth: Integer;
    FCapHeight: Integer;
    FCharWidths: array[Byte] of Word;
    FDescent: Integer;
    FFirstChar: Byte;
    FFontBox: TRect;
    FFontFile: TdxPSPDFFontFile;
    FFontFlags: Cardinal;
    FItalicAngle: Integer;
    FLastChar: Byte;
    FLeading: Integer;
    FMaxCharWidth: Integer;
    FMetricValid: Boolean;
    FStemVertical: Integer;
  protected
    class function GetType: string; override;
    function CalculateFontFlags(const APanose: TPanose): Integer; virtual;
    function CreateFontFile: TdxPSPDFFontFile; virtual;
    function GetCharWidth(AIndex: Byte): Word;
    procedure CalculateMetric(DC: HDC; const AMetric: TOutlineTextmetricA); virtual;
    procedure MetricNeeded; virtual;
    procedure WriteHeader(AWriter: TdxPSPDFWriter); override;
  public
    destructor Destroy; override;
    procedure PopulateExportList(AList: TdxPSPDFObjectList); override;
    //
    property Ascent: Integer read FAscent;
    property AveCharWidth: Integer read FAveCharWidth;
    property CapHeight: Integer read FCapHeight;
    property CharWidth[Index: Byte]: Word read GetCharWidth;
    property Descent: Integer read FDescent;
    property FirstChar: Byte read FFirstChar;
    property FontBox: TRect read FFontBox;
    property FontFile: TdxPSPDFFontFile read FFontFile;
    property FontFlags: Cardinal read FFontFlags;
    property ItalicAngle: Integer read FItalicAngle;
    property LastChar: Byte read FLastChar;
    property MaxCharWidth: Integer read FMaxCharWidth;
    property MetricValid: Boolean read FMetricValid;
    property StemVertical: Integer read FStemVertical;
  end;

  { TdxPSPDFTrueTypeFontEncoding }

  TdxPSPDFTrueTypeFontEncoding = class(TdxPSPDFFontInfo)
  private
    function GetCharset: Integer;
  protected
    class function GetType: string; override;
    procedure WriteEncodingTable(AWriter: TdxPSPDFWriter);
    procedure WriteHeader(AWriter: TdxPSPDFWriter); override;
  public
    property Charset: Integer read GetCharset;
  end;

  { TdxPSPDFTrueTypeFont }

  TdxPSPDFTrueTypeFont = class(TdxPSPDFCustomFont)
  private
    FDescriptor: TdxPSPDFFontDescriptor;
    FEncoding: TdxPSPDFTrueTypeFontEncoding;
  protected
    function ConvertToAnsiString(const AText: string): AnsiString;
    class function GetSubType: string; override;
    procedure WriteCharWidths(AWriter: TdxPSPDFWriter);
    procedure WriteHeader(AWriter: TdxPSPDFWriter); override;
  public
    constructor Create(AOwner: TdxPSPDFFontList; AEmbed: Boolean; AFont: TFont); override;
    destructor Destroy; override;
    function CanEncodeText(const S: Char): Boolean; override;
    function EncodeText(const S: string): AnsiString; override;
    function TextWidth(const S: string): Integer; override;
    procedure PopulateExportList(AList: TdxPSPDFObjectList); override;
    //
    property Descriptor: TdxPSPDFFontDescriptor read FDescriptor;
    property Encoding: TdxPSPDFTrueTypeFontEncoding read FEncoding;
  end;

  { TdxPSPDFCIDSystemInfo }

  TdxPSPDFCIDSystemInfo = class(TObject)
  private
    FOrdering: string;
    FRegistry: string;
    FSupplement: Integer;
  public
    constructor Create(const AOrdering: string); virtual;
    procedure Write(AWriter: TdxPSPDFWriter); virtual;
    //
    property Ordering: string read FOrdering write FOrdering;
    property Registry: string read FRegistry write FRegistry;
    property Supplement: Integer read FSupplement write FSupplement;
  end;

  { TdxPSPDFCIDFontInfo }

  TdxPSPDFCIDFontInfo = class(TdxPSPDFFontInfo)
  private
    FDescriptor: TdxPSPDFFontDescriptor;
    FSystemInfo: TdxPSPDFCIDSystemInfo;
    function GetCharCache: TdxPSTTFCharCacheList;
    function GetGlyphIndexWidth(Index: Word): Integer;
    function GetOwner: TdxPSPDFCIDFont;
  protected
    class function GetSubType: string; override;
    class function GetType: string; override;
    procedure WriteCharWidths(AWriter: TdxPSPDFWriter); virtual;
    procedure WriteHeader(AWriter: TdxPSPDFWriter); override;
  public
    constructor Create(AOwner: TdxPSPDFCustomFont); override;
    destructor Destroy; override;
    procedure PopulateExportList(AList: TdxPSPDFObjectList); override;
    //
    property CharCache: TdxPSTTFCharCacheList read GetCharCache;
    property Descriptor: TdxPSPDFFontDescriptor read FDescriptor;
    property GlyphIndexWidth[Index: Word]: Integer read GetGlyphIndexWidth;
    property Owner: TdxPSPDFCIDFont read GetOwner;
    property SystemInfo: TdxPSPDFCIDSystemInfo read FSystemInfo;
  end;

  { TdxPSPDFCIDFontFile }

  TdxPSPDFCIDFontFile = class(TdxPSPDFFontFile)
  private
    function CanRebuildFont: Boolean;
    function GetOwner: TdxPSPDFCIDFont;
    function GetTTFFile: TdxPSTTFFile;
  protected
    procedure FontDataNeeded; override;
  public
    property Owner: TdxPSPDFCIDFont read GetOwner;
    property TTFFile: TdxPSTTFFile read GetTTFFile;
  end;

  { TdxPSPDFCIDFontDescriptor }

  TdxPSPDFCIDFontDescriptor = class(TdxPSPDFFontDescriptor)
  protected
    function CreateFontFile: TdxPSPDFFontFile; override;
  end;

  { TdxPSPDFCIDFont }

  TdxPSPDFCIDFont = class(TdxPSPDFCustomFont)
  private
    FCharCache: TdxPSTTFCharCacheList;
    FConversionTable: TdxPSPDFCIDFontConversionTable;
    FFontInfo: TdxPSPDFCIDFontInfo;
    FTTFFile: TdxPSTTFFile;
  protected
    class function GetSubType: string; override;
    procedure WriteHeader(AWriter: TdxPSPDFWriter); override;
  public
    constructor Create(AOwner: TdxPSPDFFontList; AEmbed: Boolean; AFont: TFont); override;
    destructor Destroy; override;
    function CanEncodeText(const S: Char): Boolean; override;
    function EncodeFontName: string; override;
    function EncodeGlyph(AGlyphIndex: Word): AnsiString;
    function EncodeText(const S: string): AnsiString; override;
    function TextWidth(const S: string): Integer; override;
    procedure PopulateExportList(AList: TdxPSPDFObjectList); override;
    //
    property CharCache: TdxPSTTFCharCacheList read FCharCache;
    property ConversionTable: TdxPSPDFCIDFontConversionTable read FConversionTable;
    property FontInfo: TdxPSPDFCIDFontInfo read FFontInfo;
    property TTFFile: TdxPSTTFFile read FTTFFile;
  end;

  { TdxPSPDFCIDFontConversionTable }

  TdxPSPDFCIDFontConversionTable = class(TdxPSPDFFontInfo)
  private
    FSystemInfo: TdxPSPDFCIDSystemInfo;
    function GetCharCache: TdxPSTTFCharCacheList;
    function GetCMapName: string;
    function GetGlyphIndex(ACharCode: Word): Integer;
    function GetOwner: TdxPSPDFCIDFont;
  protected
    function GetContentStreamType: TdxPSPDFStreamType; override;
    procedure WriteContentStream(AWriter: TdxPSPDFWriter); override;
    procedure WriteConversionTable(AWriter: TdxPSPDFWriter);
  public
    constructor Create(AOwner: TdxPSPDFCustomFont); override;
    destructor Destroy; override;
    //
    property CharCache: TdxPSTTFCharCacheList read GetCharCache;
    property CMapName: string read GetCMapName;
    property GlyphIndex[CharCode: Word]: Integer read GetGlyphIndex;
    property Owner: TdxPSPDFCIDFont read GetOwner;
    property SystemInfo: TdxPSPDFCIDSystemInfo read FSystemInfo;
  end;

  { TdxPSPDFFontGlyphsInfo }

  TdxPSPDFFontGlyphsBand = packed record
    StartIndex, FinishIndex: Word;
  end;

  TdxPSPDFFontGlyphsInfo = class(TObject)
  private
    FFontName: string;
    FGlyphsBands: array of TdxPSPDFFontGlyphsBand;

    procedure Analyze(AFont: TFont);
  public
    constructor Create(AFont: TFont); virtual;
    destructor Destroy; override;
    function HasGlyph(const AChar: WideChar): Boolean;
    //
    property FontName: string read FFontName;
  end;

  { TdxPSPDFFontsManager }

  TdxPSPDFFontsManager = class(TObject)
  private
    FAllFontsProcessed: Boolean;
    FCachedFont: TFont;
    FCachedInfo: TcxObjectList;
    FSymbol: WideChar;
    function CheckFont(const AFontName: string): Boolean;
    function GetCachedInfo(Index: Integer): TdxPSPDFFontGlyphsInfo;
    function GetCachedInfoCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Exists(const AFontName: string): Boolean;
    function GetFontForSymbol(const ASymbol: WideChar; ACurrentFont: TdxPSPDFCustomFont): TdxPSPDFCustomFont;
    function GetFontInfoForSymbol(const ASymbol: WideChar; out AInfo: TdxPSPDFFontGlyphsInfo): Boolean;
    //
    property CachedInfo[Index: Integer]: TdxPSPDFFontGlyphsInfo read GetCachedInfo;
    property CachedInfoCount: Integer read GetCachedInfoCount;
  end;

function dxPDFCanCreateCIDFont(AFont: TFont): Boolean;
function dxPDFCanEmbedFont(AFont: TFont): Boolean;
function dxPDFFontsManager: TdxPSPDFFontsManager;
implementation

type
  TdxPSPDFABCArray = array[Byte] of TABC;

const
  dxPDF_FONT_FIXEDWIDTH = $2;
  dxPDF_FONT_SERIF = $4;
  dxPDF_FONT_SYMBOLIC = $8;
  dxPDF_FONT_SCRIPT = $10;
  dxPDF_FONT_USE_ADOBE_STD_CHARSET = $20;
  dxPDF_FONT_ITALIC = $40;
  dxPDF_FONT_ALLCAPFONT = $10000;
  dxPDF_FONT_SMALL_CAP  = $20000;
  dxPDF_FONT_FORCE_BOLD_AT_SMALL_TEXT_SIZE = $40000;

const
  sdxArabicEncoding = '128/Euro/afii57506/quotesinglbase/florin/quotedblbase' +
    '/ellipsis/dagger/daggerdbl/circumflex/perthousand/afii57511/guilsinglleft' +
    '/OE/afii57507/afii57508 144 /afii57509/quoteleft/quoteright/quotedblleft' +
    '/quotedblright/bullet/endash/emdash 153 /trademark/afii57513/guilsinglright' +
    '/oe/afii61664/afii301/afii57514 161 /afii57388 186 /afii57403 191 /afii57407' +
    ' 193 /afii57409/afii57410/afii57411/afii57412/afii57413/afii57414/afii57415' +
    '/afii57416/afii57417/afii57418/afii57419/afii57420/afii57421/afii57422' +
    '/afii57423/afii57424/afii57425/afii57426/afii57427/afii57428/afii57429' +
    '/afii57430 216 /afii57431/afii57432/afii57433/afii57434/afii57440/afii57441' +
    '/afii57442/afii57443/afii57444 227 /afii57445/afii57446/afii57470/afii57448' +
    '/afii57449/afii57450 240 /afii57451/afii57452/afii57453/afii57454/afii57455' +
    '/afii57456 248 /afii57457 250 /afii57458 253 /afii299/afii300/afii57519';

  sdxBalticEncoding = '128/Euro/space/quotesinglbase/space/quotedblbase' +
    '/ellipsis/dagger/daggerdbl/space/perthousand/space/guilsinglleft/space' +
    '/dieresis/caron/cedilla/space/quoteleft/quoteright/quotedblleft/quotedblright' +
    '/bullet/endash/emdash/space/trademark/space/guilsinglright/space/macron' +
    '/ogonek/space 170/Rcommaaccent 175 /AE 184/oslash 186/rcommaaccent 191 /ae' +
    '/Aogonek/Iogonek/Amacron/Cacute 198/Eogonek/Emacron/Ccaron 202/Zacute' +
    '/Edotaccent/Gcommaaccent/Kcommaaccent/Imacron/Lcommaaccent/Scaron/Nacute' +
    '/Ncommaaccent/trademark/Omacron 216/Uogonek/Lslash/Sacute/Umacron 221' +
    '/Zdotaccent/Zcaron 224/aogonek/iogonek/amacron/cacute 230/eogonek/emacron' +
    '/ccaron 234/zacute/edotaccent/gcommaaccent/kcommaaccent/imacron/lcommaaccent' +
    '/scaron/nacute/ncommaaccent 244/omacron 248/uogonek/lslash/OE/umacron 253' +
    '/zdotaccent/zcaron/dotaccent';

  sdxEastEuropeEncoding = '128 /Euro 140/Sacute/Tcaron/Zcaron/Zacute 156' +
    '/sacute/tcaron/zcaron/zacute 161/caron/breve/Lslash 165/Aogonek 170' +
    '/Scedilla 175/Zdotaccent 178/ogonek/lslash 185/aogonek/scedilla 188' +
    '/Lcaron/hungarumlaut/lcaron/zdotaccent/Racute 195/Abreve 197/Lacute' +
    '/Cacute 200/Ccaron 202/Eogonek 204/Ecaron 207/Dcaron/Dslash 209/Nacute' +
    '/Ncaron/Oacute 213/Ohungarumlaut 216/Rcaron/Uring 219/Uhungarumlaut 222' +
    '/Tcedilla 224/racute 227/abreve 229/lacute/cacute/ccedilla/ccaron 234' +
    '/eogonek 236/ecaron 239/dcaron/dmacron/nacute/ncaron 245/ohungarumlaut 248' +
    '/rcaron/uring 251/uhungarumlaut 254/tcedilla/dotaccent';

  sdxGreekEncoding = '128/Euro 160/quoteleft/quoteright 175/afii00208 180/tonos' +
    '/dieresistonos/Alphatonos 184/Epsilontonos/Etatonos/Iotatonos 188' +
    '/Omicrontonos 190/Upsilontonos/Omegatonos/iotadieresistonos/Alpha/Beta' +
    '/Gamma/Delta/Epsilon/Zeta/Eta/Theta/Iota/Kappa/Lambda/Mu/Nu/Xi/Omicron' +
    '/Pi/Rho 211/Sigma/Tau/Upsilon/Phi/Chi/Psi/Omega/Iotadieresis' +
    '/Upsilondieresis/alphatonos/epsilontonos/etatonos/iotatonos' +
    '/upsilondieresistonos/alpha/beta/gamma/delta/epsilon/zeta/eta/theta' +
    '/iota/kappa/lambda/mu/nu/xi/omicron/pi/rho/sigma1/sigma/tau/upsilon' +
    '/phi/chi/psi/omega/iotadieresis/upsilondieresis/omicrontonos/upsilontonos' +
    '/omegatonos';

  sdxHebrewEncoding = '128/Euro 130/quotesinglbase/florin/quotedblbase/ellipsis' +
    '/dagger/daggerdbl/circumflex/perthousand139/guilsinglleft 145/quoteleft' +
    '/quoteright/quotedblleft/quotedblright/bullet/endash/emdash/tilde' +
    '/trademark 155/perthousand 164/afii57636 170/multiply186/divide 192' +
    '/afii57799/afii57801/afii57800/afii57802/afii57793/afii57794/afii57795' +
    '/afii57798/afii57797/afii57806 203/afii57796/afii57807/afii57839' +
    '/afii57645/afii57841/afii57842/afii57804/afii57803/afii57658/afii57716' +
    '/afii57717/afii57718 224/afii57664/afii57665/afii57666/afii57667' +
    '/afii57668/afii57669/afii57670/afii57671/afii57672/afii57673/afii57674' +
    '/afii57675/afii57676/afii57677/afii57678/afii57679/afii57680/afii57681' +
    '/afii57682/afii57683/afii57684/afii57685/afii57686/afii57687/afii57688' +
    '/afii57689/afii57690253/afii299/afii300';

  sdxRussianEncoding = '129 /afii10052/quotesinglbase/afii10100/quotedblbase' +
    '/ellipsis/dagger/daggerdbl/Euro/perthousand/afii10058/guilsinglleft' +
    '/afii10059/afii10061/afii10060/afii10145/afii10099/quoteleft/quoteright' +
    '/quotedblleft/quotedblright/bullet/endash/emdash/space/trademark/afii10106' +
    '/guilsinglright/afii10107/afii10109/afii10108/afii10193/space/afii10062' +
    '/afii10110/afii10057/currency/afii10050/brokenbar/section/afii10023' +
    '/copyright/afii10053/guillemotleft/logicalnot/hyphen/registered/afii10056' +
    '/degree/plusminus/afii10055/afii10103/afii10098/mu/paragraph/periodcentered' +
    '/afii10071/afii61352/afii10101/guillemotright/afii10105/afii10054/afii10102' +
    '/afii10104/afii10017/afii10018/afii10019/afii10020/afii10021/afii10022' +
    '/afii10024/afii10025/afii10026/afii10027/afii10028/afii10029/afii10030' +
    '/afii10031/afii10032/afii10033/afii10034/afii10035/afii10036/afii10037' +
    '/afii10038/afii10039/afii10040/afii10041/afii10042/afii10043/afii10044' +
    '/afii10045/afii10046/afii10047/afii10048/afii10049/afii10065/afii10066' +
    '/afii10067/afii10068/afii10069/afii10070/afii10072/afii10073/afii10074' +
    '/afii10075/afii10076/afii10077/afii10078/afii10079/afii10080/afii10081' +
    '/afii10082/afii10083/afii10084/afii10085/afii10086/afii10087/afii10088' +
    '/afii10089/afii10090/afii10091/afii10092/afii10093/afii10094/afii10095' +
    '/afii10096/afii10097/space';

  sdxThaiEncoding = '128/Euro 133/ellipsis 145/quoteleft/quoteright/quotedblleft' +
    '/quotedblright/bullet/endash/emdash 160/space/kokaithai/khokhaithai/khokhuatthai' +
    '/khokhwaithai/khokhonthai/khorakhangthai/ngonguthai/chochanthai/chochingthai' +
    '/chochangthai/sosothai/chochoethai/yoyingthai/dochadathai/topatakthai' +
    '/thothanthai/thonangmonthothai/thophuthaothai/nonenthai/dodekthai/totaothai' +
    '/thothungthai/thothahanthai/thothongthai/nonuthai/bobaimaithai/poplathai' +
    '/phophungthai/fofathai/phophanthai/fofanthai/phosamphaothai/momathai' +
    '/yoyakthai/roruathai/ruthai/lolingthai/luthai/wowaenthai/sosalathai' +
    '/sorusithai/sosuathai/hohipthai/lochulathai/oangthai/honokhukthai' +
    '/paiyannoithai/saraathai/maihanakatthai/saraaathai/saraamthai/saraithai' +
    '/saraiithai/sarauethai/saraueethai/sarauthai/sarauuthai/phinthuthai 223' +
    '/bahtthai/saraethai/saraaethai/saraothai/saraaimaimuanthai/saraaimaimalaithai' +
    '/lakkhangyaothai/maiyamokthai/maitaikhuthai/maiekthai/maithothai/maitrithai' +
    '/maichattawathai/thanthakhatthai/nikhahitthai/yamakkanthai/fongmanthai' +
    '/zerothai/onethai/twothai/threethai/fourthai/fivethai/sixthai/seventhai' +
    '/eightthai/ninethai/angkhankhuthai/khomutthai';

  sdxTurkishEncoding = '128/Euro 130/quotesinglbase/florin/quotedblbase/ellipsis' +
    '/dagger/daggerdbl/circumflex/perthousand/Scaron/guilsinglleft/OE 145' +
    '/quoteleft/quoteright/quotedblleft/quotedblright/bullet/endash/emdash' +
    '/tilde/trademark/scaron/guilsinglright/oe 159/Ydieresis 208/Gbreve 221' +
    '/Idotaccent/Scedilla 240/gbreve 253/dotlessi/scedilla';

  sdxVietnameseEncoding = '128 /Euro 130/quotesinglbase/florin/quotedblbase/ellipsis' +
    '/dagger/daggerdbl/circumflex/perthousand 139/guilsinglleft/OE 145/quoteleft' +
    '/quoteright/quotedblleft/quotedblright/bullet/endash/emdash/tilde/trademark 155' +
    '/guilsinglright/oe 159/Ydieresis/space/exclamdown/cent/sterling/currency' +
    '/yen/brokenbar/section/dieresis/copyright/ordfeminine/guillemotleft/logicalnot' +
    '/hyphen/registered/macron/degree/plusminus/twosuperior/threesuperior/acute/mu' +
    '/paragraph/periodcentered/cedilla/onesuperior/ordmasculine/guillemotright' +
    '/onequarter/onehalf/threequarters/questiondown/Agrave/Aacute/Acircumflex' +
    '/Abreve/Adieresis/Aring/AE/Ccedilla/Egrave/Eacute/Ecircumflex/Edieresis' +
    '/gravetonecmb /Iacute /Icircumflex /Idieresis/Dcroat/Ntilde/hookabovecomb' +
    '/Oacute/Ocircumflex/Ohorn/Odieresis/multiply/Oslash/Ugrave/Uacute/Ucircumflex' +
    '/Udieresis/Uhorn/tildecomb/germandbls/agrave/aacute/acircumflex/abreve' +
    '/adieresis/aring/ae/ccedilla/egrave/eacute/ecircumflex/edieresis/acutetonecmb' +
    '/iacute/icircumflex/idieresis/dcroat/ntilde/dotbelowcomb/oacute/ocircumflex' +
    '/ohorn/odieresis/divide/oslash/ugrave/uacute/ucircumflex/udieresis/uhorn' +
    '/dong/ydieresis';

var
  FFontsManager: TdxPSPDFFontsManager;

function dxPDFCanCreateCIDFont(AFont: TFont): Boolean;
begin
  Result := dxPDFCanEmbedFont(AFont);
end;

function dxPDFFontsManager: TdxPSPDFFontsManager;
begin
  if FFontsManager = nil then
    FFontsManager := TdxPSPDFFontsManager.Create;
  Result := FFontsManager;
end;

function dxPDFCanEmbedFont(AFont: TFont): Boolean;
begin
  Result := (AFont.Charset <> SYMBOL_CHARSET) and dxPSGetFontData(nil, AFont);
end;

function GetABCWidth(const ABC: TABC): Integer;
begin
  Result := ABC.abcA + Integer(ABC.abcB) + ABC.abcC;
end;

{ TdxPSPDFTrueTypeFont }

constructor TdxPSPDFTrueTypeFont.Create(AOwner: TdxPSPDFFontList; AEmbed: Boolean; AFont: TFont);
begin
  inherited Create(AOwner, AEmbed, AFont);
  FEncoding := TdxPSPDFTrueTypeFontEncoding.Create(Self);
  FDescriptor := TdxPSPDFFontDescriptor.Create(Self);
end;

destructor TdxPSPDFTrueTypeFont.Destroy;
begin
  FreeAndNil(FDescriptor);
  FreeAndNil(FEncoding);
  inherited Destroy;
end;

function TdxPSPDFTrueTypeFont.CanEncodeText(const S: Char): Boolean;
begin
  Result := True;
end;

function TdxPSPDFTrueTypeFont.ConvertToAnsiString(const AText: string): AnsiString;
var
  I: Integer;
begin
  if Charset <> SYMBOL_CHARSET then
    Result := dxstringToAnsiString(AText, CodePage)
  else
  begin
    SetLength(Result, Length(AText));
    for I := 1 to Length(AText) do
      Result[I] := AnsiChar(Word(AText[I]) and not $F000);
  end;
end;

function TdxPSPDFTrueTypeFont.EncodeText(const S: string): AnsiString;
begin
  Result := '(' + ConvertToAnsiString(S) + ')';
end;

class function TdxPSPDFTrueTypeFont.GetSubType: string;
begin
  Result := sdxPDFSubTypeFontTrueType;
end;

function TdxPSPDFTrueTypeFont.TextWidth(const S: string): Integer;
var
  ATempStr: AnsiString;
  I: Integer;
begin
  Result := 0;
  ATempStr := ConvertToAnsiString(S);
  for I := 1 to Length(ATempStr) do
    Inc(Result, Descriptor.CharWidth[Byte(ATempStr[I])]);
end;

procedure TdxPSPDFTrueTypeFont.PopulateExportList(AList: TdxPSPDFObjectList);
begin
  inherited PopulateExportList(AList);
  Encoding.PopulateExportList(AList);
  Descriptor.PopulateExportList(AList);
end;

procedure TdxPSPDFTrueTypeFont.WriteCharWidths(AWriter: TdxPSPDFWriter);
var
  I: Integer;
begin
  Descriptor.MetricNeeded;
  AWriter.WriteString(sdxPDFFirstChar + sdxPDFSpace + IntToStr(Descriptor.FirstChar));
  AWriter.WriteString(sdxPDFLastChar + sdxPDFSpace + IntToStr(Descriptor.LastChar));
  AWriter.WriteString(sdxPDFWidths + '[', False);
  for I := Descriptor.FirstChar to Descriptor.LastChar do
    AWriter.WriteString(IntToStr(Descriptor.CharWidth[I]) + sdxPDFSpace, False);
  AWriter.WriteString(']');
end;

procedure TdxPSPDFTrueTypeFont.WriteHeader(AWriter: TdxPSPDFWriter);
begin
  inherited WriteHeader(AWriter);
  AWriter.WriteString(sdxPDFEncoding + sdxPDFSpace + AWriter.MakeLinkToObject(Encoding));
  AWriter.WriteString(sdxPDFFontDescriptor + sdxPDFSpace + AWriter.MakeLinkToObject(Descriptor));
  WriteCharWidths(AWriter);
end;

{ TdxPSPDFTrueTypeFontEncoding }

function TdxPSPDFTrueTypeFontEncoding.GetCharset: Integer;
begin
  Result := Owner.Charset;
end;

class function TdxPSPDFTrueTypeFontEncoding.GetType: string;
begin
  Result := sdxPDFEncoding;
end;

procedure TdxPSPDFTrueTypeFontEncoding.WriteEncodingTable(AWriter: TdxPSPDFWriter);
var
  AEncodingTable: string;
begin
  case Charset of
    ARABIC_CHARSET:
      AEncodingTable := sdxArabicEncoding;
    BALTIC_CHARSET:
      AEncodingTable := sdxBalticEncoding;
    EASTEUROPE_CHARSET:
      AEncodingTable := sdxEastEuropeEncoding;
    HEBREW_CHARSET:
      AEncodingTable := sdxHebrewEncoding;
    GREEK_CHARSET:
      AEncodingTable := sdxGreekEncoding;
    RUSSIAN_CHARSET:
      AEncodingTable := sdxRussianEncoding;
    THAI_CHARSET:
      AEncodingTable := sdxThaiEncoding;
    TURKISH_CHARSET:
      AEncodingTable := sdxTurkishEncoding;
    VIETNAMESE_CHARSET:
      AEncodingTable := sdxVietnameseEncoding;
    else
      Exit;
  end;
  AWriter.WriteString(sdxPDFDifferences + ' [' + AEncodingTable + ']', False);
end;

procedure TdxPSPDFTrueTypeFontEncoding.WriteHeader(AWriter: TdxPSPDFWriter);
const
  BaseEncodingMap: array[Boolean] of string = (
    sdxPDFEncodingWinAnsi, sdxPDFEncodingMacRoman
  );
begin
  inherited WriteHeader(AWriter);
  AWriter.WriteString(sdxPDFBaseEncoding + BaseEncodingMap[Charset = SYMBOL_CHARSET]);
  WriteEncodingTable(AWriter);
end;

{ TdxPSPDFFontDescriptor }

destructor TdxPSPDFFontDescriptor.Destroy;
begin
  FreeAndNil(FFontFile);
  inherited Destroy;
end;

procedure TdxPSPDFFontDescriptor.CalculateMetric(DC: HDC; const AMetric: TOutlineTextmetricA);
var
  AABC: TdxPSPDFABCArray;
  I: Integer;
begin
  ZeroMemory(@FCharWidths[0], SizeOf(FCharWidths));
  FAscent := AMetric.otmAscent;
  FAveCharWidth := AMetric.otmTextMetrics.tmAveCharWidth;
  FCapHeight := AMetric.otmTextMetrics.tmHeight;
  FDescent := AMetric.otmDescent;
  FFontBox := AMetric.otmrcFontBox;
  FFontFlags := CalculateFontFlags(AMetric.otmPanoseNumber);
  FItalicAngle := AMetric.otmItalicAngle;
  FMaxCharWidth := AMetric.otmTextMetrics.tmMaxCharWidth;
  FStemVertical := 50 + Round(Sqr(AMetric.otmTextMetrics.tmWeight / 65));
  FLastChar := Ord(AMetric.otmTextMetrics.tmLastChar);
  FFirstChar := Ord(AMetric.otmTextMetrics.tmFirstChar);
  FLeading := AMetric.otmTextMetrics.tmInternalLeading;
  GetCharABCWidths(DC, FFirstChar, FLastChar, AABC);
  for I := 0 to LastChar - FirstChar do
  begin
    if AMetric.otmTextMetrics.tmPitchAndFamily and TMPF_FIXED_PITCH = 0 then
      FCharWidths[FirstChar + I] := GetABCWidth(AABC[0])
    else
      FCharWidths[FirstChar + I] := GetABCWidth(AABC[I]);
  end;
end;

procedure TdxPSPDFFontDescriptor.MetricNeeded;
var
  AFont: TFont;
  AMetric: POutlineTextmetricA;
  AMetricLength: Integer;
  AOldFont: HFONT;
  DC: HDC;
begin
  if not FMetricValid then
  begin
    FMetricValid := True;
    AFont := Owner.CreateFont;
    try
      DC := GetDC(0);
      AFont.PixelsPerInch := 96;
      AFont.Size := 750;
      AOldFont := SelectObject(DC, AFont.Handle);
      AMetricLength := GetOutlineTextMetricsA(DC, 0, nil);
      if AMetricLength > 0 then
      begin
        AMetric := AllocMem(AMetricLength);
        try
          AMetric^.otmSize := SizeOf(AMetric^);
          GetOutlineTextMetricsA(DC, AMetricLength, AMetric);
          CalculateMetric(DC, AMetric^);
        finally
          FreeMem(AMetric);
        end;
      end;
      SelectObject(DC, AOldFont);
      ReleaseDC(0, DC);
    finally
      AFont.Free;
    end;
  end;
end;

procedure TdxPSPDFFontDescriptor.PopulateExportList(AList: TdxPSPDFObjectList);
begin
  inherited PopulateExportList(AList);
  if (FontFile = nil) and Owner.Embed then
    FFontFile := CreateFontFile;
  if Assigned(FontFile) then
    FontFile.PopulateExportList(AList);
end;

function TdxPSPDFFontDescriptor.CalculateFontFlags(const APanose: TPanose): Integer;
begin
  Result := dxPDF_FONT_USE_ADOBE_STD_CHARSET;
  if APanose.bProportion = 9 then
    Result := Result or dxPDF_FONT_FIXEDWIDTH;
  if (APanose.bSerifStyle < 11) or (APanose.bSerifStyle > 13) then
    Result := Result or dxPDF_FONT_SERIF;
  if APanose.bFamilyType = 3 then
    Result := Result or dxPDF_FONT_SCRIPT;
end;

function TdxPSPDFFontDescriptor.CreateFontFile: TdxPSPDFFontFile;
begin
  Result := TdxPSPDFFontFile.Create(Owner);
end;

function TdxPSPDFFontDescriptor.GetCharWidth(AIndex: Byte): Word;
begin
  MetricNeeded;
  Result := FCharWidths[AIndex];
end;

class function TdxPSPDFFontDescriptor.GetType: string;
begin
  Result := sdxPDFFontDescriptor;
end;

procedure TdxPSPDFFontDescriptor.WriteHeader(AWriter: TdxPSPDFWriter);
begin
  inherited WriteHeader(AWriter);
  MetricNeeded;
  AWriter.WriteString(sdxPDFFontName + '/' + Owner.EncodeFontName);
  AWriter.WriteString(sdxPDFFontFlags + sdxPDFSpace + IntToStr(FFontFlags));
  AWriter.WriteString(sdxPDFItalicAngle + sdxPDFSpace + IntToStr(FItalicAngle));
  AWriter.WriteString(sdxPDFAscent + sdxPDFSpace + IntToStr(FAscent));
  AWriter.WriteString(sdxPDFDescent + sdxPDFSpace + IntToStr(FDescent));
  AWriter.WriteString(sdxPDFCapHeight + sdxPDFSpace + IntToStr(FCapHeight));
  AWriter.WriteString(sdxPDFStemV + sdxPDFSpace + IntToStr(FStemVertical));
  AWriter.WriteString(sdxPDFAvgWidth + sdxPDFSpace + IntToStr(FAveCharWidth));
  AWriter.WriteString(sdxPDFMaxWidth+ sdxPDFSpace + IntToStr(FMaxCharWidth));
  AWriter.WriteString(sdxPDFMissingWidth + sdxPDFSpace + IntToStr(FAveCharWidth));
  AWriter.WriteString(sdxPDFLeading + sdxPDFSpace + IntToStr(FLeading));
  AWriter.WriteString(Format(sdxPDFFontBox + ' [%d %d %d %d]',
    [FontBox.Left, FontBox.Bottom, FontBox.Right, FontBox.Top]));
  if Assigned(FontFile) then
    AWriter.WriteString(sdxPDFFontFile + sdxPDFSpace + AWriter.MakeLinkToObject(FontFile));
end;

{ TdxPSPDFFontInfo }

constructor TdxPSPDFFontInfo.Create(AOwner: TdxPSPDFCustomFont);
begin
  inherited Create;
  FOwner := AOwner;
end;

{ TdxPSPDFCIDFont }

constructor TdxPSPDFCIDFont.Create(AOwner: TdxPSPDFFontList; AEmbed: Boolean; AFont: TFont);
begin
  inherited Create(AOwner, True, AFont);
  FTTFFile := TdxPSTTFFile.Create(AFont);
  FCharCache := TdxPSTTFCharCacheList.Create;
  FFontInfo := TdxPSPDFCIDFontInfo.Create(Self);
  FConversionTable := TdxPSPDFCIDFontConversionTable.Create(Self);
end;

destructor TdxPSPDFCIDFont.Destroy;
begin
  FreeAndNil(FConversionTable);
  FreeAndNil(FTTFFile);
  FreeAndNil(FFontInfo);
  FreeAndNil(FCharCache);
  inherited Destroy;
end;

function TdxPSPDFCIDFont.CanEncodeText(const S: Char): Boolean;
begin
  Result := TTFFile.GlyphIndex[Word(S)] <> 0;
end;

function TdxPSPDFCIDFont.EncodeFontName: string;
begin
  Result := 'DX+' + inherited EncodeFontName;
end;

function TdxPSPDFCIDFont.EncodeGlyph(AGlyphIndex: Word): AnsiString;
begin
  CharCache.Add(TTFFile.CharCodeByGlyphIndex[AGlyphIndex], AGlyphIndex);
  Result := '<' + dxStringToAnsiString(IntToHex(AGlyphIndex, 4)) + '>';
end;

function TdxPSPDFCIDFont.EncodeText(const S: string): AnsiString;

  function DoEncodeText(const S: string): AnsiString;
  var
    AChar: Word;
    AGlyphIndex: Word;
    I: Integer;
  begin
    Result := '';
    for I := 1 to Length(S) do
    begin
      AChar := Word(S[I]);
      AGlyphIndex := TTFFile.GlyphIndex[AChar];
      CharCache.Add(AChar, AGlyphIndex);
      Result := Result + dxStringToAnsiString(IntToHex(AGlyphIndex, 4));
    end;
  end;

begin
  Result := '<' + DoEncodeText(S) + '>';
end;

function TdxPSPDFCIDFont.TextWidth(const S: string): Integer;
var
  I: Integer;
begin
  Result:= 0;
  for I := 1 to Length(S) do
    Inc(Result, TTFFile.CharWidthByCode[Word(S[I])]);
end;

class function TdxPSPDFCIDFont.GetSubType: string;
begin
  Result := sdxPDFSubTypeFontType0;
end;

procedure TdxPSPDFCIDFont.PopulateExportList(AList: TdxPSPDFObjectList);
begin
  inherited PopulateExportList(AList);
  FontInfo.PopulateExportList(AList);
  ConversionTable.PopulateExportList(AList);
end;

procedure TdxPSPDFCIDFont.WriteHeader(AWriter: TdxPSPDFWriter);
begin
  inherited WriteHeader(AWriter);
  AWriter.WriteString(sdxPDFEncoding + sdxPDFSpace + sdxPDFEncodingIdentityH);
  AWriter.WriteString(sdxPDFDescendantFonts + ' [' + AWriter.MakeLinkToObject(FontInfo) + ']');
  AWriter.WriteString(sdxPDFToUnicode + sdxPDFSpace + AWriter.MakeLinkToObject(ConversionTable));
end;

{ TdxPSPDFCIDFontInfo }

constructor TdxPSPDFCIDFontInfo.Create(AOwner: TdxPSPDFCustomFont);
begin
  inherited Create(AOwner);
  FDescriptor := TdxPSPDFCIDFontDescriptor.Create(AOwner);
  FSystemInfo := TdxPSPDFCIDSystemInfo.Create('Identity');
end;

destructor TdxPSPDFCIDFontInfo.Destroy;
begin
  FreeAndNil(FSystemInfo);
  FreeAndNil(FDescriptor);
  inherited Destroy;
end;

class function TdxPSPDFCIDFontInfo.GetSubType: string;
begin
  Result := sdxPDFSubTypeFontTypeCID;
end;

class function TdxPSPDFCIDFontInfo.GetType: string;
begin
  Result := sdxPDFFont;
end;

function TdxPSPDFCIDFontInfo.GetCharCache: TdxPSTTFCharCacheList;
begin
  Result := Owner.CharCache;
end;

function TdxPSPDFCIDFontInfo.GetGlyphIndexWidth(Index: Word): Integer;
begin
  Result := Owner.TTFFile.CharWidthByGlyphIndex[Index];
end;

function TdxPSPDFCIDFontInfo.GetOwner: TdxPSPDFCIDFont;
begin
  Result := TdxPSPDFCIDFont(inherited Owner);
end;

procedure TdxPSPDFCIDFontInfo.PopulateExportList(AList: TdxPSPDFObjectList);
begin
  inherited PopulateExportList(AList);
  Descriptor.PopulateExportList(AList);
end;

procedure TdxPSPDFCIDFontInfo.WriteCharWidths(AWriter: TdxPSPDFWriter);

  procedure WriteConsecutiveWidths(AGlyphIndex, AConsecutiveLength: Integer);
  var
    I: Integer;
  begin
    if AConsecutiveLength > 0 then
    begin
      AWriter.WriteString(IntToStr(AGlyphIndex) + ' [', False);
      for I := AGlyphIndex to AGlyphIndex + AConsecutiveLength - 1 do
      begin
        AWriter.WriteString(IntToStr(GlyphIndexWidth[I]), False);
        if I + 1 - AGlyphIndex < AConsecutiveLength then
          AWriter.WriteString(sdxPDFSpace, False);
      end;
      AWriter.WriteString(']');
    end;
  end;

var
  AConsecutiveLength: Integer;
  AOldGlyphIndex: Integer;
  AStartsFromGlyphIndex: Integer;
  I: Integer;
begin
  AWriter.WriteString('/W [');
  try
    AOldGlyphIndex := 0;
    AConsecutiveLength := 1;
    AStartsFromGlyphIndex := 0;
    CharCache.SortByGlyphIndex;
    for I := 0 to CharCache.Count - 1 do
    begin
      if CharCache.CacheItem[I].GlyphIndex <> AOldGlyphIndex + 1 then
      begin
        WriteConsecutiveWidths(AStartsFromGlyphIndex, AConsecutiveLength);
        AStartsFromGlyphIndex := CharCache.CacheItem[I].GlyphIndex;
        AConsecutiveLength := 0;
      end;
      AOldGlyphIndex := CharCache.CacheItem[I].GlyphIndex;
      Inc(AConsecutiveLength);
    end;
    WriteConsecutiveWidths(AStartsFromGlyphIndex, AConsecutiveLength);
  finally
    AWriter.WriteString(']');
  end;
end;

procedure TdxPSPDFCIDFontInfo.WriteHeader(AWriter: TdxPSPDFWriter);
begin
  inherited WriteHeader(AWriter);
  AWriter.WriteString(sdxPDFCharSet + sdxPDFSpace + IntToStr(Owner.Charset));
  AWriter.WriteString(sdxPDFBaseFont + sdxPDFSpace + '/' + Owner.EncodeFontName);
  AWriter.WriteString(sdxPDFFontDescriptor + sdxPDFSpace + AWriter.MakeLinkToObject(Descriptor));
  SystemInfo.Write(AWriter);
  WriteCharWidths(AWriter);
end;

{ TdxPSPDFCIDSystemInfo }

constructor TdxPSPDFCIDSystemInfo.Create(const AOrdering: string);
begin
  inherited Create;
  FRegistry := 'Adobe';
  FOrdering := AOrdering;
  FSupplement := 0;
end;

procedure TdxPSPDFCIDSystemInfo.Write(AWriter: TdxPSPDFWriter);
begin
  AWriter.WriteString(sdxPDFCIDSystemInfo);
  AWriter.BeginParamsSet;
  try
    AWriter.WriteString(sdxPDFRegistry + ' (' + Registry + ')');
    AWriter.WriteString(sdxPDFOrdering + ' (' + Ordering + ')');
    AWriter.WriteString(sdxPDFSupplement + sdxPDFSpace + IntToStr(Supplement));
  finally
    AWriter.EndParamsSet;
  end;
end;

{ TdxPSPDFCIDFontConversionTable }

constructor TdxPSPDFCIDFontConversionTable.Create(AOwner: TdxPSPDFCustomFont);
begin
  inherited Create(AOwner);
  FSystemInfo := TdxPSPDFCIDSystemInfo.Create(CMapName);
end;

destructor TdxPSPDFCIDFontConversionTable.Destroy;
begin
  FreeAndNil(FSystemInfo);
  inherited Destroy;
end;

function TdxPSPDFCIDFontConversionTable.GetContentStreamType: TdxPSPDFStreamType;
begin
  Result := pstText;
end;

function TdxPSPDFCIDFontConversionTable.GetGlyphIndex(ACharCode: Word): Integer;
begin
  Result := Owner.TTFFile.GlyphIndex[ACharCode];
end;

function TdxPSPDFCIDFontConversionTable.GetOwner: TdxPSPDFCIDFont;
begin
  Result := TdxPSPDFCIDFont(inherited Owner);
end;

function TdxPSPDFCIDFontConversionTable.GetCharCache: TdxPSTTFCharCacheList;
begin
  Result := Owner.CharCache;
end;

function TdxPSPDFCIDFontConversionTable.GetCMapName: string;
begin
  Result := sdxPDFCIDFontPrefix + '+' + Owner.Name;
end;

procedure TdxPSPDFCIDFontConversionTable.WriteContentStream(AWriter: TdxPSPDFWriter);
begin
  SystemInfo.Ordering := CMapName;
  AWriter.WriteString('/CIDInit /ProcSet findresource begin');
  AWriter.WriteString('12 dict begin');
  AWriter.WriteString('begincmap');
  SystemInfo.Write(AWriter);
  AWriter.WriteString('def');
  AWriter.WriteString(sdxPDFCMapName + ' /' + CMapName + ' def');
  AWriter.WriteString(sdxPDFCMapType + ' 2 def');
  AWriter.WriteString('1 begincodespacerange');
  AWriter.WriteString('<0000> <FFFF>');
  AWriter.WriteString('endcodespacerange');
  WriteConversionTable(AWriter);
  AWriter.WriteString('endcmap');
  AWriter.WriteString('CMapName currentdict /CMap defineresource pop');
  AWriter.WriteString('end');
  AWriter.WriteString('end', False);
end;

procedure TdxPSPDFCIDFontConversionTable.WriteConversionTable(AWriter: TdxPSPDFWriter);
var
  ACacheItem: TdxPSTTFCharCacheListItem;
  I: Integer;
begin
  AWriter.WriteString(IntToStr(CharCache.Count) + ' beginbfchar');
  for I := 0 to CharCache.Count - 1 do
  begin
    ACacheItem := CharCache.CacheItem[I];
    AWriter.WriteString('<' + IntToHex(ACacheItem.GlyphIndex, 4) + '>' +
      sdxPDFSpace + '<' + IntToHex(ACacheItem.CharCode , 4) + '>');
  end;
  AWriter.WriteString('endbfchar');
end;

{ TdxPSPDFFontFile }

destructor TdxPSPDFFontFile.Destroy;
begin
  FreeAndNil(FFontData);
  inherited Destroy;
end;

function TdxPSPDFFontFile.GetContentStreamType: TdxPSPDFStreamType;
begin
  Result := pstText;
end;

function TdxPSPDFFontFile.GetFontDataValid: Boolean;
begin
  Result := Assigned(FontData);
end;

procedure TdxPSPDFFontFile.FontDataNeeded;
var
  AFont: TFont;
begin
  if not FontDataValid then
  begin
    FreeAndNil(FFontData);
    AFont := Owner.CreateFont;
    try
      FFontData := TMemoryStream.Create;
      dxPSGetFontData(FFontData, AFont);
    finally
      AFont.Free;
    end;
  end;
end;

procedure TdxPSPDFFontFile.WriteContentStream(AWriter: TdxPSPDFWriter);
begin
  FontDataNeeded;
  AWriter.WriteStream(FontData);
end;

procedure TdxPSPDFFontFile.WriteHeader(AWriter: TdxPSPDFWriter);
begin
  FontDataNeeded;
  inherited WriteHeader(AWriter);
  AWriter.WriteString(sdxPDFLength + '1' + sdxPDFSpace + IntToStr(FontData.Size));
end;

{ TdxPSPDFCIDFontFile }

function TdxPSPDFCIDFontFile.CanRebuildFont: Boolean;
begin
  Result := TTFFile.OS2Section.FamilyClass <> 5;
end;

procedure TdxPSPDFCIDFontFile.FontDataNeeded;
begin
  if not FontDataValid then
  begin
    if CanRebuildFont then
    begin
      FreeAndNil(FFontData);
      FFontData := TMemoryStream.Create;
      TTFFile.Rebuild(Owner.CharCache);
      TTFFile.SaveToStream(FontData);
      FFontData.Position := 0;
    end
    else
      inherited FontDataNeeded;
  end;
end;

function TdxPSPDFCIDFontFile.GetOwner: TdxPSPDFCIDFont;
begin
  Result := TdxPSPDFCIDFont(inherited Owner);
end;

function TdxPSPDFCIDFontFile.GetTTFFile: TdxPSTTFFile;
begin
  Result := Owner.TTFFile;
end;

{ TdxPSPDFCIDFontDescriptor }

function TdxPSPDFCIDFontDescriptor.CreateFontFile: TdxPSPDFFontFile;
begin
  Result := TdxPSPDFCIDFontFile.Create(Owner);
end;

{ TdxPSPDFFontGlyphsInfo }

constructor TdxPSPDFFontGlyphsInfo.Create(AFont: TFont);
begin
  inherited Create;
  FFontName := AFont.Name;
  Analyze(AFont);
end;

destructor TdxPSPDFFontGlyphsInfo.Destroy;
begin
  FGlyphsBands := nil;
  inherited Destroy;
end;

procedure TdxPSPDFFontGlyphsInfo.Analyze(AFont: TFont);

  procedure SetBand(var ABandIndex: Integer; AStartIndex, AFinishIndex: Integer);
  begin
    if AStartIndex >= 0 then
    begin
      if ABandIndex = Length(FGlyphsBands) then
        SetLength(FGlyphsBands, MulDiv(Length(FGlyphsBands) + 1, 5, 4));
      FGlyphsBands[ABandIndex].FinishIndex := AFinishIndex;
      FGlyphsBands[ABandIndex].StartIndex := AStartIndex;
      Inc(ABandIndex);
    end;
  end;

var
  ABandIndex: Integer;
  AMap: TdxPSTTFCMapSection;
  APrevCode: Integer;
  AWordCode: Word;
begin
  if dxPSGetFontCharsMap(AFont, AMap) then
  try
    APrevCode := -1;
    ABandIndex := 0;
    for AWordCode := Low(Word) to High(Word) do
    begin
      if AMap.Map[AWordCode] = 0 then
      begin
        SetBand(ABandIndex, APrevCode, AWordCode - 1);
        APrevCode := -1;
      end
      else
        if APrevCode = -1 then
          APrevCode := AWordCode;
    end;
    SetBand(ABandIndex, APrevCode, MAXWORD);
  finally
    AMap.Free;
  end;
end;

function TdxPSPDFFontGlyphsInfo.HasGlyph(const AChar: WideChar): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(FGlyphsBands) - 1 do
  begin
    with FGlyphsBands[I] do
      Result := (Word(AChar) >= StartIndex) and (Word(AChar) <= FinishIndex);
    if Result then
      Break;
  end;
end;

{ TdxPSPDFFontsManager }

function EnumFontsProc(var ALogFont: TLogFont; var ATextMetric: TTextMetric; AFontType: DWORD; AData: LPARAM): Integer; stdcall;
begin
  Result := Integer(not FFontsManager.CheckFont(ALogFont.lfFaceName));
end;

constructor TdxPSPDFFontsManager.Create;
begin
  inherited Create;
  FCachedFont := TFont.Create;
  FCachedInfo := TcxObjectList.Create;
end;

destructor TdxPSPDFFontsManager.Destroy;
begin
  FreeAndNil(FCachedInfo);
  FreeAndNil(FCachedFont);
  inherited Destroy;
end;

function TdxPSPDFFontsManager.CheckFont(const AFontName: string): Boolean;
var
  AInfo: TdxPSPDFFontGlyphsInfo;
begin
  Result := False;
  if not Exists(AFontName) then
  begin
    FCachedFont.Name := AFontName;
    AInfo := TdxPSPDFFontGlyphsInfo.Create(FCachedFont);
    Result := AInfo.HasGlyph(FSymbol);
    FCachedInfo.Add(AInfo);
  end;
end;

function TdxPSPDFFontsManager.Exists(const AFontName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to CachedInfoCount - 1 do
  begin
    Result := SameText(AFontName, CachedInfo[I].FontName);
    if Result then Break;
  end;
end;

function TdxPSPDFFontsManager.GetFontForSymbol(const ASymbol: WideChar; ACurrentFont: TdxPSPDFCustomFont): TdxPSPDFCustomFont;
var
  AFontInfo: TdxPSPDFFontGlyphsInfo;
begin
  if GetFontInfoForSymbol(ASymbol, AFontInfo) then
  begin
    FCachedFont.Name := AFontInfo.FontName;
    Result := ACurrentFont.Owner.Add(FCachedFont, True, True);
  end
  else
    Result := ACurrentFont;
end;

function TdxPSPDFFontsManager.GetFontInfoForSymbol(const ASymbol: WideChar; out AInfo: TdxPSPDFFontGlyphsInfo): Boolean;

  function FindFontInfo(const ASymbol: WideChar; out AInfo: TdxPSPDFFontGlyphsInfo): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to CachedInfoCount - 1 do
    begin
      Result := CachedInfo[I].HasGlyph(ASymbol);
      if Result then
      begin
        AInfo := CachedInfo[I];
        Break;
      end;
    end;
  end;

var
  ALogFont: TLogFont;
  DC: HDC;
begin
  Result := FindFontInfo(ASymbol, AInfo);
  if not (Result or FAllFontsProcessed) then
  begin
    DC := GetDC(0);
    try
      FSymbol := ASymbol;
      ZeroMemory(@ALogFont, SizeOf(ALogFont));
      ALogFont.lfCharset := DEFAULT_CHARSET;
      //FAllFontsProcessed := EnumFontFamiliesEx(DC, ALogFont, @EnumFontsProc, TRUETYPE_FONTTYPE, 0);
      EnumFontFamiliesEx(DC, ALogFont, @EnumFontsProc, TRUETYPE_FONTTYPE, 0);
      Result := FindFontInfo(ASymbol, AInfo);
    finally
      ReleaseDC(0, DC);
    end;
  end;
end;

function TdxPSPDFFontsManager.GetCachedInfo(Index: Integer): TdxPSPDFFontGlyphsInfo;
begin
  Result := TdxPSPDFFontGlyphsInfo(FCachedInfo[Index]);
end;

function TdxPSPDFFontsManager.GetCachedInfoCount: Integer;
begin
  Result := FCachedInfo.Count;
end;

initialization

finalization
  FreeAndNil(FFontsManager);
end.
