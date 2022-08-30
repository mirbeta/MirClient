{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPDFViewer                                         }
{                                                                    }
{           Copyright (c) 2015-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL              }
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

unit dxPDFTypes;

{$I cxVer.inc}

interface

uses
  Types, SysUtils, Classes, Windows, Generics.Defaults, Generics.Collections, Math, dxCore, dxCoreClasses, cxClasses,
  cxGraphics, dxCoreGraphics, cxGeometry, dxGDIPlusClasses, dxProtectionUtils, dxPDFBase;

type
  TdxPDFPoints = TdxPointsF;

  EdxPDFAbortException = class(EAbort);
  TdxPDFDictionary = class;
  TdxPDFNumericObject = class;
  TdxPDFStream = class;
  EdxPDFException = class(EdxException);
  EdxPDFExceptionClass = class of EdxPDFException;
  EdxPDFEncryptionException = class(EdxPDFException);
  TdxPDFCustomRepository = class;
  TdxPDFCustomRepositoryClass = class of TdxPDFCustomRepository;
  TdxPDFCustomStreamFilter = class;
  TdxPDFReference = class;

  TdxPDFBlendMode = (bmNormal, bmCompatible, bmMultiply, bmScreen, bmOverlay, bmDarken, bmLighten, bmColorDodge,
    bmColorBurn, bmHardLight, bmSoftLight, bmDifference, bmExclusion, bmHue, bmSaturation, bmColor, bmLuminosity);
  TdxPDFLineCapStyle = (lcsButt, lcsRound, lcsProjectingSquare);
  TdxPDFLineJoinStyle = (ljsMiter, ljsRound, ljsBevel);
  TdxPDFPixelFormat = (pfUnknown, pfGray1bit, pfGray8bit, pfArgb24bpp, pfArgb32bpp);
  TdxPDFRenderingIntent = (riAbsoluteColorimetric, riRelativeColorimetric, riSaturation, riPerceptual);
  TdxPDFTextRenderingMode = (trmFill, trdStroke, trmFillAndStroke, trmInvisible, trmFillAndClip, trmStrokeAndClip,
    trmFillStrokeAndClip, trmClip);
  TdxPDFVersion = (v1_0, v1_1, v1_2, v1_3, v1_4, v1_5, v1_6, v1_7);

  IdxPDFEncryptionInfo = interface
  ['{DDECCF86-0499-45F3-9ED8-ED81624CE7EA}']
    function Decrypt(const AData: TBytes; ANumber: Integer): TBytes;
  end;

  TdxPDFDocumentDecodedImageData = record
    Data: TBytes;
    PixelFormat: TdxPDFPixelFormat;
  end;

  TdxPDFStringData = record
    CharacterCodes: array of TBytes;
    Glyphs: TWordDynArray;
    Offsets: TDoubleDynArray;
  end;

  TdxPDFPosition = record
    PageIndex: Integer;
    Point: TdxPointF;
  end;

  TdxPDFDocumentArea = record
    Rect: TdxRectF;
    PageIndex: Integer;
  end;

  TdxPDFOrientedRect = record
  public
    Angle: Single;
    Top: Single;
    Left: Single;
    Height: Single;
    Width: Single;
  end;

  TdxPDFOrientedRectList = class(TList<TdxPDFOrientedRect>);

  TdxPDFDocumentID = array[0..1] of TBytes;

  { TdxPDFOrientedRectHelper }

  TdxPDFOrientedRectHelper = record helper for TdxPDFOrientedRect
  strict private
    function GetBottom: Single;
    function GetRect: TdxRectF;
    function GetRight: Single;
    function GetRotatedRect: TdxRectF;
    function GetTopLeft: TdxPointF; inline;
    function GetTopRight: TdxPointF;
  protected
    class function Invalid: TdxPDFOrientedRect; static;
  public
    class function Create: TdxPDFOrientedRect; overload; static;
    class function Create(const ATopLeft: TdxPointF; AWidth, AHeight, AAngle: Single): TdxPDFOrientedRect; overload; static;
    class function Create(const ARect: TdxRectF): TdxPDFOrientedRect; overload; static;
    class function Create(const ARect: TdxRectF; AAngle: Single): TdxPDFOrientedRect; overload; static;

    function IsValid: Boolean;
    function Overlap(const R: TdxPDFOrientedRect): Boolean;
    function PtInRect(const APoint: TdxPointF; AExpandX: Single = 0; AExpandY: Single = 0): Boolean;

    property Bottom: Single read GetBottom;
    property Rect: TdxRectF read GetRect;
    property Right: Single read GetRight;
    property RotatedRect: TdxRectF read GetRotatedRect;
    property TopLeft: TdxPointF read GetTopLeft;
    property TopRight: TdxPointF read GetTopRight;
  end;

  { TdxPDFDocumentAreaHelper }

  TdxPDFDocumentAreaHelper = record helper for TdxPDFDocumentArea
  public
    class function Create(APageIndex: Integer; const R: TdxRectF): TdxPDFDocumentArea; overload; static;
    class function Create(const P1, P2: TdxPDFPosition): TdxPDFDocumentArea; overload; static;
    class function Empty: TdxPDFDocumentArea; static;
  end;

  { TdxPDFPositionHelper }

  TdxPDFPositionHelper = record helper for TdxPDFPosition
  public
    class function Create: TdxPDFPosition; overload; static;
    class function Create(APageNumber: Integer; const P: TdxPointF): TdxPDFPosition; overload; static;
    function NearTo(const APosition: TdxPDFPosition): Boolean;
    function IsValid: Boolean;
    procedure Invalid;
  end;

  { TdxPDFDefinedSymbols }

  TdxPDFDefinedSymbols = class
  public const
  {$REGION 'public const'}
    Bell = 7;
    Backslash = 92;
    Backspace = 8;
    CarriageReturn = 13;
    Comment = Byte('%');
    DigitEnd = Byte('9');
    DigitStart = Byte('0');
    EndArray = Byte(']');
    EndObject = Byte('>');
    EndString = Byte(')');
    ExclamationMark = Byte('!');
    FormFeed = 12;
    HexDigitEnd = Byte('F');
    HexDigitStart = Byte('A');
    HorizontalTab = 9;
    Minus = Byte('-');
    LeftBracket = Byte('{');
    LineFeed = 10;
    LowercaseHexDigitEnd = Byte('f');
    LowercaseHexDigitStart = Byte('a');
    NameIdentifier = Byte('/');
    Null = 0;
    NumberSign = Byte('#');
    Period = Byte('.');
    Plus = Byte('+');
    RightBracket = Byte('}');
    Space = Byte(' ');
    StartArray = Byte('[');
    StartString = Byte('(');
    StartObject = Byte('<');
  {$ENDREGION}
  end;

  { TdxPDFNamedObjectDictionary }

  TdxPDFNamedObjectDictionary = class
  strict private
    FDictionary: TDictionary<string, Integer>;
    FResourceKey: string;
    FPrefix: string;
    FNextResourceNumber: Integer;
  public
    constructor Create(const AResourceKey, APrefix: string);
    destructor Destroy; override;

    function GetNewResourceName(ADictionary: TdxPDFReferencedObjectDictionary): string; overload;
    function GetNewResourceName(ADictionary: TdxPDFReferencedObjectDictionary; const AName: string): string; overload;
    function ContainsValue(AValue: Integer): Boolean;
    procedure ClearResourceNames;

    property ResourceKey: string read FResourceKey;
  end;

  { TdxPDFArray }

  TdxPDFArray = class(TdxPDFBase)
  strict private
    FElementList: TdxPDFBaseList;

    function GetCount: Integer;
    function GetElement(AIndex: Integer): TdxPDFBase;
    function GetElementList: TdxPDFBaseList;
  protected
    class function GetObjectType: TdxPDFBaseType; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Add(AValue: TdxPDFBase); overload;
    procedure Add(AValue: Integer); overload;
    procedure Add(AValue: Single); overload;
    procedure Add(const AValue: string); overload;
    procedure Add(ANumber, AGeneration: Integer); overload;

    property Count: Integer read GetCount;
    property Elements[Index: Integer]: TdxPDFBase read GetElement; default;
    property ElementList: TdxPDFBaseList read GetElementList;
  end;

  { TdxPDFCustomStreamFilter }

  TdxPDFCustomStreamFilter = class(TdxPDFReferencedObject)
  protected
    function DoDecode(const AData: TBytes): TBytes; virtual; abstract;
    procedure Initialize(ADecodeParameters: TObject); virtual;
  public
    constructor Create(ADecodeParameters: TObject);

    class function GetName: string; virtual;
    class function GetShortName: string; virtual;
    function DecodeImageData(const AData: TBytes;
      AWidth, AHeight, AComponentCount: Integer): TdxPDFDocumentDecodedImageData; virtual;
    function Decode(const AData: TBytes): TBytes;
  end;

  TdxPDFFilters = class(TdxPDFObjectList<TdxPDFCustomStreamFilter>);

  { TdxPDFKeywords }

  TdxPDFKeywords = class
  public const
  {$REGION 'public const'}
    AcroForm = 'AcroForm';
    Action = 'Action';
    ActionFirst = 'First';
    ActionName = 'N';
    ActionNamed = 'Named';
    ActionNext = 'Next';
    ActionType = 'S';
    AESCryptMethodName = 'AESV3';
    Alternate = 'Alternate';
    Annotations = 'Annots';
    AntiAlias = 'AntiAlias';
    ArialBoldFontName = 'Arial,Bold';
    ArialFontName = 'Arial';
    ArialItalicFontName = 'Arial,Italic';
    ArialUnicodeMS = 'Arial Unicode MS';
    ArtBox = 'ArtBox';
    Author = 'Author';
    Background = 'Background';
    BaseEncoding = 'BaseEncoding';
    BaseFont = 'BaseFont';
    BaseVersion = 'BaseVersion';
    BBox = 'BBox';
    BitsPerComponent = 'BitsPerComponent';
    BlackPoint = 'BlackPoint';
    BleedBox = 'BleedBox';
    Bold = 'Bold';
    CaretAnnotation = 'Caret';
    Catalog = 'Catalog';
    CharProcs = 'CharProcs';
    CIDFontType0 = 'CIDFontType0';
    CIDFontType0C = 'CIDFontType0C';
    CIDFontType2 = 'CIDFontType2';
    CIDToGIDMap = 'CIDToGIDMap';
    CircleAnnotation = 'Circle';
    ColorSpace = 'ColorSpace';
    Components = 'Components';
    Contents = 'Contents';
    Coords = 'Coords';
    Count = 'N';
    CountFull = 'Count';
    CourierBoldFontName = 'Courier-Bold';
    CourierBoldObliqueFontName = 'Courier-BoldOblique';
    CourierFontName = 'Courier';
    CourierNewBoldFontName = 'CourierNew,Bold';
    CourierNewFontName = 'CourierNew';
    CourierNewFontName2 = 'Courier New';
    CourierNewItalicFontName = 'CourierNew,Italic';
    CourierObliqueFontName = 'Courier-Oblique';
    CreationDate = 'CreationDate';
    Creator = 'Creator';
    CropBox = 'CropBox';
    CryptFilterMode = 'CFM';
    CryptFilters = 'CF';
    Decode = 'Decode';
    DecodeParameters = 'DecodeParms';
    DescendantFonts = 'DescendantFonts';
    Destinations = 'Dests';
    DictionaryAppearance = 'DA';
    Differences = 'Differences';
    DocumentInfo = 'Info';
    Domain = 'Domain';
    EmbeddedFiles = 'EmbeddedFiles';
    EmbeddedFilesCryptFilter = 'EFF';
    Encode = 'Encode';
    EncodedOwnerPassword = 'OE';
    EncodedUserPassword = 'UE';
    Encoding = 'Encoding';
    EncryptedPermissions = 'Perms';
    EncryptMetadata = 'EncryptMetadata';
    Extend = 'Extend';
    ExtensionLevel = 'ExtensionLevel';
    ExtGState = 'ExtGState';
    Filter = 'Filter';
    FirstChar = 'FirstChar';
    Font = 'Font';
    FontDescriptor = 'FontDescriptor';
    FontDescriptorAscent = 'Ascent';
    FontDescriptorAvgWidth = 'AvgWidth';
    FontDescriptorBBox = 'FontBBox';
    FontDescriptorCapHeight = 'CapHeight';
    FontDescriptorCharSet = 'CharSet';
    FontDescriptorCIDSet = 'CIDSet';
    FontDescriptorDescent = 'Descent';
    FontDescriptorFamily = 'FontFamily';
    FontDescriptorFlags = 'Flags';
    FontDescriptorItalicAngle = 'ItalicAngle';
    FontDescriptorLeading = 'Leading';
    FontDescriptorMaxWidth = 'MaxWidth';
    FontDescriptorMissingWidth = 'MissingWidth';
    FontDescriptorStemH = 'StemH';
    FontDescriptorStemV = 'StemV';
    FontDescriptorWeight = 'FontWeight';
    FontDescriptorWeightNormal = 400;
    FontDescriptorWeightRegular = 'Regular';
    FontDescriptorXHeight = 'XHeight';
    FontFile = 'FontFile';
    FontFile2 = 'FontFile2';
    FontFile3 = 'FontFile3';
    FontFileDictionaryKey = 'FontFile';
    FontFileSubtype = 'Type1C';
    FontMatrix = 'FontMatrix';
    FontMTSuffix = 'MT';
    FontName = 'FontName';
    FontStretch = 'FontStretch';
    FontType0 = 'Type0';
    FontType3 = 'Type3';
    FormType = 'FormType';
    FreeTextAnnotation = 'FreeText';
    Functions = 'Functions';
    FunctionType = 'Function';
    Gamma = 'Gamma';
    Group = 'Group';
    Height = 'Height';
    HelveticaBoldFontName = 'Helvetica-Bold';
    HelveticaBoldObliqueFontName = 'Helvetica-BoldOblique';
    HelveticaFontName = 'Helvetica';
    HelveticaObliqueFontName = 'Helvetica-Oblique';
    HighlightAnnotation = 'Highlight';
    ID = 'ID';
    Identity = 'Identity';
    IdentityH = 'Identity-H';
    IdentityV = 'Identity-V';
    ImageMask = 'ImageMask';
    InkAnnotation = 'Ink';
    Intent = 'Intent';
    Interpolate = 'Interpolate';
    Italic = 'Italic';
    Keywords = 'Keywords';
    Kids = 'Kids';
    LastChar = 'LastChar';
    Length = 'Length';
    Length1 = 'Length1';
    Length2 = 'Length2';
    Length3 = 'Length3';
    LineAnnotation = 'Line';
    Marked = 'Marked';
    Mask = 'Mask';
    Matrix = 'Matrix';
    Matte = 'Matte';
    MediaBox = 'MediaBox';
    ModDate = 'ModDate';
    Names = 'Names';
    NeedAppearances = 'NeedAppearances';
    ObjectStream = 'ObjStm';
    Oblique = 'Oblique';
    OpenAction = 'OpenAction';
    OpenTypeFont = 'OpenType';
    Outline = 'Outline';
    OutlineAction = 'A';
    OutlineColor = 'C';
    OutlineCount = 'Count';
    OutlineDestination = 'Dest';
    OutlineFirst = 'First';
    OutlineFlags = 'F';
    OutlineNext = 'Next';
    OutlinePrev = 'Prev';
    Outlines = 'Outlines';
    OutlineTitle = 'Title';
    OwnerPasswordHash = 'O';
    Page = 'Page';
    Pages = 'Pages';
    PaintType = 'PaintType';
    Parent = 'Parent';
    Pattern = 'Pattern';
    PatternType = 'PatternType';
    Permissions = 'P';
    PolygonAnnotation = 'Polygon';
    PolyLineAnnotation = 'PolyLine';
    Producer = 'Producer';
    Range = 'Range';
    RedactAnnotation = 'Redact';
    Resources = 'Resources';
    Revision = 'R';
    Rotate = 'Rotate';
    Shading = 'Shading';
    ShadingType = 'ShadingType';
    ShortWidths = 'W';
    SigFlags = 'SigFlags';
    Size = 'Size';
    SoftMask = 'SMask';
    SquareAnnotation = 'Square';
    SquigglyAnnotation = 'Squiggly';
    StampAnnotation = 'Stamp';
    Standard = 'Standard';
    Standart = 'Standart';
    StandartEncoding = 'StandartEncoding';
    StandartFilterName = 'StdCF';
    StdCF = 'StdCF';
    StreamCryptFilter = 'StmF';
    StrikeOutAnnotation = 'StrikeOut';
    StringCryptFilter = 'StrF';
    StructParent = 'StructParent';
    Subject = 'Subject';
    Subtype = 'Subtype';
    Suspects = 'Suspects';
    SymbolFontName = 'Symbol';
    TextAnnotation = 'Text';
    TilingType = 'TilingType';
    TimesBoldFontName = 'Times-Bold';
    TimesBoldItalicFontName = 'Times-BoldItalic';
    TimesFontFamilyName = 'Times';
    TimesItalicFontName = 'Times-Italic';
    TimesNewRomanBoldFontName = 'TimesNewRoman,Bold';
    TimesNewRomanFontName = 'TimesNewRoman';
    TimesNewRomanFontName2 = 'Times New Roman';
    TimesNewRomanPSMTPrefix = 'TimesNewRomanPS';
    TimesRomanFontName = 'Times-Roman';
    Title = 'Title';
    ToUnicode = 'ToUnicode';
    Trailer = 'trailer';
    Transparency = 'Transparency';
    TrimBox = 'TrimBox';
    TrueType = 'TrueType';
    TypeKey = 'Type';
    UnderlineAnnotation = 'Underline';
    UserPasswordHash = 'U';
    UserProperties = 'UserProperties';
    UserUnit = 'UserUnit';
    Version = 'V';
    VersionSignature = '%PDF-';
    WhitePoint = 'WhitePoint';
    Width = 'Width';
    Widths = 'Widths';
    WinAnsiEncoding = 'WinAnsiEncoding';
    XObject = 'XObject';
    XObject2 = 'Xobject';
    XStep = 'XStep';
    XYZDestination = 'XYZ';
    YStep = 'YStep';
    ZapfDingbatsFontName = 'ZapfDingbats';
  {$ENDREGION}
  end;

  { TdxPDFStream }

  TdxPDFStream = class(TdxPDFBase)
  strict private
    FIsDecrypted: Boolean;
    FEncryptionInfo: IdxPDFEncryptionInfo;
    FDictionary: TdxPDFDictionary;
    FFilters: TdxPDFFilters;
    FRawData: TBytes;

    function GetRawData: TBytes;
    function GetDecryptedData: TBytes;
    function GetUncompressedData: TBytes;
    function NeedDecrypting: Boolean;
    procedure SetDictionary(const AValue: TdxPDFDictionary);
    procedure SetRawData(const AValue: TBytes);
    procedure AddFilter(AName: TdxPDFBase; AParameters: TObject);
  protected
    class function GetObjectType: TdxPDFBaseType; override;
    procedure PopulateFilters;

    property EncryptionInfo: IdxPDFEncryptionInfo read FEncryptionInfo write FEncryptionInfo;
  public
    constructor Create; overload; override;
    constructor Create(ADictionary: TdxPDFDictionary); overload;
    destructor Destroy; override;

    property Dictionary: TdxPDFDictionary read FDictionary write SetDictionary;
    property Filters: TdxPDFFilters read FFilters;
    property RawData: TBytes read GetRawData write SetRawData;
    property UncompressedData: TBytes read GetUncompressedData;
  end;

  { TdxPDFDictionary }

  TdxPDFDictionary = class(TdxPDFBase)
  strict private
    FEncryptionInfo: IdxPDFEncryptionInfo;
    FObjects: TdxPDFReferencedObjectDictionary;
    FStreamRef: TdxPDFStream;

    function GetItems: TDictionary<string, TdxPDFReferencedObject>;
    function GetValue(const AKey: string): TdxPDFBase;
    procedure SetValue(const AKey: string; AValue: TdxPDFBase);
    function TryGetValue<T: TdxPDFBase>(const AKey: string; out AValue: T): Boolean;
    function ParseDateTime(ADateTime: string): TDateTime;
    procedure ParseDateTimeComponent(var ADateTime: string; AComponent: PInteger; ADefaultValue: Byte = 0);
  protected
    class function GetObjectType: TdxPDFBaseType; override;
    function GetCount: Integer;
    property Items: TDictionary<string, TdxPDFReferencedObject> read GetItems;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetObject(const AKey: string): TdxPDFBase; virtual;

    function Contains(const AKey: string): Boolean;
    function CreateNumericList(const AKey: string): TdxPDFDoubleList;
    function GetArray(const AKey: string): TdxPDFArray; overload;
    function GetArray(const AKey, AAlternativeKey: string): TdxPDFArray; overload;
    function GetBoolean(const AKey: string; ADefaultValue: Boolean = False): Boolean;
    function GetBytes(const AKey: string): TBytes;
    function GetDate(const AKey: string): TDateTime;
    function GetDictionary(const AKey: string): TdxPDFDictionary;
    function GetDouble(const AKey: string; ADefaultValue: Double = dxPDFInvalidValue): Double;
    function GetDoubleArray(const AKey: string): TDoubleDynArray;
    function GetInteger(const AKey: string; ADefaultValue: Integer = dxPDFInvalidValue): Integer; overload;
    function GetInteger(const AKey, AAlternativeKey: string): Integer; overload;
    function GetRectangle(const AKey: string): TdxRectF; overload;
    function GetRectangle(const AKey: string; const ADefaultValue: TdxRectF): TdxRectF; overload;
    function GetStream(const AKey: string): TdxPDFStream;
    function GetString(const AKey: string): string; overload;
    function GetString(const AKey, ADefaultValue: string): string; overload;
    function GetTextString(const AKey: string): string;
    function TryGetArray(const AKey: string; out AValue: TdxPDFArray): Boolean;
    function TryGetBoolean(const AKey: string; out AValue: Boolean): Boolean;
    function TryGetObject(const AKey: string; out AValue: TdxPDFBase): Boolean;
    function TryGetReference(const AKey: string; out AValue: Integer): Boolean;
    function TryGetStream(const AKey: string; out AValue: TdxPDFStream): Boolean;
    function TryGetString(const AKey: string; out AValue: string): Boolean;
    function TryGetTextString(const AKey: string; out AValue: string): Boolean;
    procedure Add(const AKey: string; const AValue: TBytes); overload;
    procedure Add(const AKey: string; AValue: TdxPDFBase); overload;
    procedure Add(const AKey: string; AValue: Integer); overload;
    procedure Add(const AKey: string; AValue: Double); overload;
    procedure Add(const AKey: string; AValue: Boolean); overload;
    procedure Add(const AKey: string; const AValue: TdxRectF); overload;
    procedure Add(const AKey, AValue: string); overload;
    procedure Clear;
    procedure Remove(const AKey: string);

    property Count: Integer read GetCount;
    property EncryptionInfo: IdxPDFEncryptionInfo read FEncryptionInfo;
    property StreamRef: TdxPDFStream read FStreamRef write FStreamRef;
    property Value[const AKey: string]: TdxPDFBase read GetValue write SetValue; default;
  end;

  { TdxPDFValue }

  TdxPDFValue = class(TdxPDFBase)
  strict private
    FValue: Variant;
  protected
    procedure InternalSetValue(const AValue: Variant);

    property InternalValue: Variant read FValue;
  public
    constructor Create(ANumber, AGeneration: Integer; const AValue: Variant); reintroduce; overload;
    constructor Create(const AValue: Variant); reintroduce; overload;
  end;

  { TdxPDFBoolean }

  TdxPDFBoolean = class(TdxPDFValue)
  strict private
    function GetValue: Boolean; inline;
    procedure SetValue(const AValue: Boolean);
  protected
    class function GetObjectType: TdxPDFBaseType; override;
  public
    property Value: Boolean read GetValue write SetValue;
  end;

  { TdxPDFNull }

  TdxPDFNull = class(TdxPDFValue)
  strict private
    function GetValue: Variant; inline;
  public
    property Value: Variant read GetValue;
  end;

  { TdxPDFString }

  TdxPDFString = class(TdxPDFValue)
  strict private
    function GetValue: string;
    procedure SetValue(const AValue: string);
  protected
    class function GetObjectType: TdxPDFBaseType; override;
  public
    property Value: string read GetValue write SetValue;
  end;

  { TdxPDFComment }

  TdxPDFComment = class(TdxPDFString)
  protected
    class function GetObjectType: TdxPDFBaseType; override;
  end;

  { TdxPDFName }

  TdxPDFName = class(TdxPDFString)
  protected
    class function GetObjectType: TdxPDFBaseType; override;
  end;

  { TdxPDFNumericObject }

  TdxPDFNumericObject = class(TdxPDFValue)
  protected
    class function GetObjectType: TdxPDFBaseType; override;
    function GetValue: Variant; inline;
  end;

  { TdxPDFInteger }

  TdxPDFInteger = class(TdxPDFNumericObject)
  strict private
    function GetValue: Integer;
    procedure SetValue(const AValue: Integer);
  protected
    class function GetObjectType: TdxPDFBaseType; override;
  public
    property Value: Integer read GetValue write SetValue;
  end;

  { TdxPDFDouble }

  TdxPDFDouble = class(TdxPDFNumericObject)
  strict private
    function GetValue: Double;
    procedure SetValue(AValue: Double);
  public
    property Value: Double read GetValue write SetValue;
  end;

  { TdxPDFReference }

  TdxPDFReference = class(TdxPDFBase)
  strict private
    FIsFree: Boolean;
    FIsSlot: Boolean;
    FOffset: Int64;
  protected
    class function GetObjectType: TdxPDFBaseType; override;
  public
    constructor Create(ANumber, AGeneration: Integer; AOffset: Int64 = 0; AIsFree: Boolean = False;
      AIsSlot: Boolean = False); reintroduce;

    property IsFree: Boolean read FIsFree;
    property IsSlot: Boolean read FIsSlot;
    property Offset: Int64 read FOffset write FOffset;
  end;

  { TdxPDFIndirectObject }

  TdxPDFIndirectObject = class(TdxPDFBase)
  strict private
    FData: TBytes;
  protected
    class function GetObjectType: TdxPDFBaseType; override;
  public
    constructor Create(ANumber, AGeneration: Integer; const AData: TBytes);
    destructor Destroy; override;

    property Data: TBytes read FData;
  end;

  { TdxPDFToken }

  TdxPDFToken = class
  strict private
    FCurrentComparingSymbol: Byte;
    FIndexToCompare: Integer;
    FSequence: TBytes;
    FSequenceLength: Integer;

    procedure BeginCompare; overload; inline;
  public
    constructor Create(const ADescription: string); overload;
    constructor Create(const ASequence: TBytes); overload;

    class function BeginCompare(AToken: TdxPDFToken): TdxPDFToken; overload; inline;
    function IsStartWithComment: Boolean; inline;
    function Compare(ASymbol: Byte): Boolean; inline;

    property Sequence: TBytes read FSequence;
  end;

  { TdxPDFTransformationMatrix }

  TdxPDFTransformationMatrix = class(TdxMatrix)
  private
    function GetA: Double;
    function GetB: Double;
    function GetC: Double;
    function GetD: Double;
    function GetE: Double;
    function GetF: Double;
    function GetDeterminant: Double;
    function GetIsInvertable: Boolean;
    function GetIsRotated: Boolean;
  public
    constructor CreateEx(AMatrix1: TdxPDFTransformationMatrix); overload;
    class function Invert(M: TdxPDFTransformationMatrix): TdxPDFTransformationMatrix;
    class function MultiplyMatrix(M1, M2: TdxPDFTransformationMatrix): TdxPDFTransformationMatrix;
    class function TranslateMatrix(M: TdxPDFTransformationMatrix; const AOffset: TdxPointF): TdxPDFTransformationMatrix;
    function TransformPoints(const APoints: TdxPDFPoints): TdxPDFPoints;
    procedure Write(AStream: TdxPDFBytesStream);

    property A: Double read GetA;
    property B: Double read GetB;
    property C: Double read GetC;
    property D: Double read GetD;
    property E: Double read GetE;
    property F: Double read GetF;

    property IsInvertable: Boolean read GetIsInvertable;
    property IsRotated: Boolean read GetIsRotated;
  end;

  { TdxPDFStreamElement }

  TdxPDFStreamElement = class(TdxPDFReference)
  strict private
    FIndex: Integer;
  protected
    class function GetObjectType: TdxPDFBaseType; override;
  public
    constructor Create(ANumber: Integer; AIndex: Integer);

    property Index: Integer read FIndex;
  end;

  { TdxPDFCustomRepository }

  TdxPDFCustomRepository = class(TdxPDFReferencedObject)
  strict private
    FReferences: TdxPDFBaseReferences;
  protected
    function ResolveObject(ANumber: Integer): TdxPDFReferencedObject; virtual;

    procedure Replace(ANumber: Integer; AObject: TdxPDFReferencedObject);
    procedure TryAdd(ANumber: Integer; AObject: TdxPDFReferencedObject; ACanReplace: Boolean);

    property References: TdxPDFBaseReferences read FReferences;
  public
    constructor Create;
    destructor Destroy; override;

    function DecryptString(const S: string): string; virtual;

    function GetArray(ANumber: Integer): TdxPDFArray;
    function GetDictionary(ANumber: Integer): TdxPDFDictionary;
    function GetInteger(ANumber: Integer): TdxPDFInteger;
    function GetObject(ANumber: Integer): TdxPDFReferencedObject;
    function GetStream(ANumber: Integer): TdxPDFStream;

    procedure Add(ANumber: Integer; AObject: TdxPDFReferencedObject; ACanReplace: Boolean = True);
    procedure Clear; virtual;
    procedure Remove(ANumber: Integer);
    procedure TrimExcess;
  end;

  { TdxPDFRange }

  TdxPDFRange = record
  public
    Min: Double;
    Max: Double;

    class function Create(AMin, AMax: Double): TdxPDFRange; static;
    class function Invalid: TdxPDFRange; static;
    function Contains(AValue: Integer): Boolean;
    function IsSame(ARange: TdxPDFRange): Boolean;
  end;

  TdxPDFRanges = array of TdxPDFRange;

  { TdxPDFImageInfo }

  TdxPDFImageInfo = record
    BitsPerComponent: Integer;
    ColorKeyMask: TdxPDFRanges;
    Data: TBytes;
    Width: Integer;
    Height: Integer;
  end;

  { TdxPDFColor }

  TdxPDFColor = class(TdxPDFReferencedObject)
  strict private
    FPattern: TdxPDFReferencedObject;
    FComponents: TDoubleDynArray;

    procedure AddComponent(AValue: Double);
    procedure SetPattern(const AValue: TdxPDFReferencedObject);
  public
    constructor Create; overload;
    constructor Create(const AComponents: array of Double); overload;
    constructor Create(const AComponents: TDoubleDynArray); overload;
    constructor Create(AColor: TdxPDFColor); overload;
    constructor Create(X, Y, Z, AWhitePointZ: Double); overload;
    destructor Destroy; override;

    class function GetComponents(X, Y, Z, AWhitePointZ: Double): TDoubleDynArray;
    procedure Assign(AColor: TdxPDFColor);

    class function ClipColorComponent(AComponent: Double): Double;
    class function ColorComponentTransferFunction(AComponent: Double): Double;

    procedure AssignAndTransferComponents(const AComponents: TDoubleDynArray);

    property Components: TDoubleDynArray read FComponents;
    property Pattern: TdxPDFReferencedObject read FPattern write SetPattern;
  end;

  { TdxPDFARGBColor }

  TdxPDFARGBColor = class(TdxPDFReferencedObject)
  private
    FRed: Double;
    FGreen: Double;
    FBlue: Double;
    FAlpha: Byte;
  public
    constructor Create(AColor: TdxPDFColor); overload;
    constructor CreateFromRGB(ARed, AGreen, ABlue: Double; AAlpha: Byte = 255); overload;
    constructor CreateFromCMYK(ACyan, AMagenta, AYellow, ABlack: Double); overload;

    class function ConvertToBytes(ACyan, AMagenta, AYellow, ABlack: Byte): TBytes; inline;
    class function ConvertToRGB(const AData: TBytes; APixelFormat: TdxPDFPixelFormat): TBytes;

    property Alpha: Byte read FAlpha;
    property Red: Double read FRed;
    property Green: Double read FGreen;
    property Blue: Double read FBlue;
  end;

  { TdxPDFCustomProperties }

  TdxPDFCustomProperties = class(TdxPDFBase)
  strict private
    FDictionary: TdxPDFDictionary;
    procedure SetDictionary(const AValue: TdxPDFDictionary);
  protected
    property Dictionary: TdxPDFDictionary read FDictionary write SetDictionary;
  public
    constructor Create(ADictionary: TdxPDFDictionary);
    destructor Destroy; override;
  end;

  { TdxPDFBlendModeDictionary }

  TdxPDFBlendModeDictionary = class
  strict private
    FDictionary: TDictionary<string, TdxPDFBlendMode>;
  public
    constructor Create;
    destructor Destroy; override;
    function TryGetValue(const AKey: string; out AValue: TdxPDFBlendMode): Boolean;
  end;

function dxPDFBlendModeDictionary: TdxPDFBlendModeDictionary;
function dxPDFChangeValue(AValue: TdxPDFReferencedObject; var ACurrentValue: TdxPDFReferencedObject): Boolean; inline;
procedure dxPDFFreeObject(AObject: TdxPDFReferencedObject);

function dxPDFImageInfo(AWidth, AHeight: Double; ABitsPerComponent: Integer;
  const AColorKeyMask: TdxPDFRanges): TdxPDFImageInfo; overload; inline;
function dxPDFImageInfo(const AData: TBytes; AWidth, AHeight: Double; ABitsPerComponent: Integer;
  const AColorKeyMask: TdxPDFRanges): TdxPDFImageInfo; overload; inline;

implementation

uses
  StrUtils, Variants, cxDateUtils, dxGDIPlusAPI, dxPDFCore, dxPDFStreamFilter, dxPDFUtils;

var
  dxgPDFBlendModeDictionary: TdxPDFBlendModeDictionary;

function dxPDFBlendModeDictionary: TdxPDFBlendModeDictionary;
begin
  if dxgPDFBlendModeDictionary = nil then
    dxgPDFBlendModeDictionary := TdxPDFBlendModeDictionary.Create;
  Result := dxgPDFBlendModeDictionary;
end;

function dxPDFChangeValue(AValue: TdxPDFReferencedObject; var ACurrentValue: TdxPDFReferencedObject): Boolean;
var
  APreviousValue: TdxPDFReferencedObject;
begin
  Result := ACurrentValue <> AValue;
  if Result then
  begin
    APreviousValue := ACurrentValue;
    ACurrentValue := AValue;

    if ACurrentValue <> nil then
      ACurrentValue.Reference;

    if APreviousValue <> nil then
      APreviousValue.Release;
  end;
end;

procedure dxPDFFreeObject(AObject: TdxPDFReferencedObject);
begin
  dxPDFChangeValue(nil, AObject);
end;

function dxPDFImageInfo(AWidth, AHeight: Double; ABitsPerComponent: Integer;
  const AColorKeyMask: TdxPDFRanges): TdxPDFImageInfo;
begin
  Result := dxPDFImageInfo(nil, AWidth, AHeight, ABitsPerComponent, AColorKeyMask);
end;

function dxPDFImageInfo(const AData: TBytes; AWidth, AHeight: Double; ABitsPerComponent: Integer;
  const AColorKeyMask: TdxPDFRanges): TdxPDFImageInfo;
begin
  Result.Data := AData;
  Result.Width := Trunc(AWidth);
  Result.Height := Trunc(AHeight);
  Result.BitsPerComponent := ABitsPerComponent;
  Result.ColorKeyMask := AColorKeyMask;
end;

{ TdxPDFOrientedRectHelper }

class function TdxPDFOrientedRectHelper.Create: TdxPDFOrientedRect;
begin
  Result := Invalid;
end;

class function TdxPDFOrientedRectHelper.Create(const ATopLeft: TdxPointF;
  AWidth, AHeight, AAngle: Single): TdxPDFOrientedRect;
begin
  Result.Angle := TdxPDFUtils.NormalizeAngle(AAngle);
  Result.Top := ATopLeft.Y;
  Result.Left := ATopLeft.X;
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

class function TdxPDFOrientedRectHelper.Create(const ARect: TdxRectF): TdxPDFOrientedRect;
begin
  Result := Create(ARect, 0);
end;

class function TdxPDFOrientedRectHelper.Create(const ARect: TdxRectF; AAngle: Single): TdxPDFOrientedRect;
begin
  Result.Angle := AAngle;
  Result.Top := ARect.Top;
  Result.Left := ARect.Left;
  Result.Width := Abs(ARect.Width);
  Result.Height := Abs(ARect.Height);
end;

class function TdxPDFOrientedRectHelper.Invalid: TdxPDFOrientedRect;
begin
  Result.Angle := -1;
  Result.Top := -1;
  Result.Left := -1;
  Result.Width := -1;
  Result.Height := -1;
end;

function TdxPDFOrientedRectHelper.GetTopLeft: TdxPointF;
begin
  Result := dxPointF(Left, Top);
end;

function TdxPDFOrientedRectHelper.GetBottom: Single;
begin
  Result := Top + Width * Sin(Angle) - Height * Cos(Angle);
end;

function TdxPDFOrientedRectHelper.GetRect: TdxRectF;
begin
  Result.Left := Left;
  Result.Top := Bottom;
  Result.Width := Width;
  Result.Height := Height;
end;

function TdxPDFOrientedRectHelper.GetRight: Single;
begin
  Result := Left + Width * Cos(Angle) + Height * Sin(Angle);
end;

function TdxPDFOrientedRectHelper.GetRotatedRect: TdxRectF;
var
  ARealTopLeft, ARotatedTopLeft, ARealTopRight, ARealBottomLeft, ARealBottomRight: TdxPointF;
begin
  ARealTopLeft := TopLeft;
  ARotatedTopLeft := TdxPDFTextUtils.RotatePoint(ARealTopLeft, -Angle);
  ARealTopRight := TdxPDFTextUtils.RotatePoint(dxPointF(ARotatedTopLeft.X + Width, ARotatedTopLeft.Y), Angle);
  ARealBottomLeft := TdxPDFTextUtils.RotatePoint(dxPointF(ARotatedTopLeft.X, ARotatedTopLeft.Y - Height), Angle);
  ARealBottomRight := TdxPDFTextUtils.RotatePoint(dxPointF(ARotatedTopLeft.X + Width, ARotatedTopLeft.Y - Height), Angle);

  Result.Left := Min(Min(ARealTopLeft.X, ARealTopRight.X), Min(ARealBottomLeft.X, ARealBottomRight.X));
  Result.Top := Min(Min(ARealTopLeft.Y, ARealTopRight.Y), Min(ARealBottomLeft.Y, ARealBottomRight.Y));
  Result.Right := Max(Max(ARealTopLeft.X, ARealTopRight.X), Max(ARealBottomLeft.X, ARealBottomRight.X));
  Result.Bottom := Max(Max(ARealTopLeft.Y, ARealTopRight.Y), Max(ARealBottomLeft.Y, ARealBottomRight.Y));
end;

function TdxPDFOrientedRectHelper.GetTopRight: TdxPointF;
var
  ARotatedTopLeft: TdxPointF;
begin
  ARotatedTopLeft := TdxPDFTextUtils.RotatePoint(TopLeft, -Angle);
  Result := TdxPDFTextUtils.RotatePoint(dxPointF(ARotatedTopLeft.X + Width, ARotatedTopLeft.Y), Angle);
end;

function TdxPDFOrientedRectHelper.IsValid: Boolean;
var
  AInvalidRect: TdxPDFOrientedRect;
begin
  AInvalidRect := Invalid;
  Result := (AInvalidRect.Angle <> Angle) and (AInvalidRect.Height <> Angle) and (AInvalidRect.Width <> Width);
end;

function TdxPDFOrientedRectHelper.Overlap(const R: TdxPDFOrientedRect): Boolean;
const
  Distance = 1;
begin
  Result := (Abs(R.Left - Left) < Distance) and (Abs(R.Top - Top) < Distance) and
    (Abs(R.Width - Width) < Distance) and (Abs(R.Height - Height) < Distance) and (Angle = R.Angle);
end;

function TdxPDFOrientedRectHelper.PtInRect(const APoint: TdxPointF; AExpandX: Single = 0; AExpandY: Single = 0): Boolean;
var
  ARotatedPoint, ARotatedTopLeft: TdxPointF;
begin
  ARotatedPoint := TdxPDFTextUtils.RotatePoint(APoint, -Angle);
  ARotatedTopLeft := TdxPDFTextUtils.RotatePoint(TopLeft, -Angle);
  Result := (((ARotatedPoint.X >= (ARotatedTopLeft.X - AExpandX)) and
    (ARotatedPoint.X <= (ARotatedTopLeft.X + Width + AExpandX))) and
    (ARotatedPoint.Y <= (ARotatedTopLeft.Y + AExpandY))) and
    (ARotatedPoint.Y >= (ARotatedTopLeft.Y - Height - AExpandY));
end;

{ TdxPDFDocumentAreaHelper }

class function TdxPDFDocumentAreaHelper.Create(APageIndex: Integer; const R: TdxRectF): TdxPDFDocumentArea;
begin
  Result.Rect := R;
  Result.PageIndex := APageIndex;
end;

class function TdxPDFDocumentAreaHelper.Create(const P1, P2: TdxPDFPosition): TdxPDFDocumentArea;
var
  APoint1, APoint2: TdxPointF;
begin
  try
    if P1.PageIndex <> P2.PageIndex then
      Exit(Empty);
    APoint1 := P1.Point;
    APoint2 := P2.Point;
    Exit(TdxPDFDocumentArea.Create(P1.PageIndex, dxRectF(
      TdxPDFUtils.Min(APoint1.X, APoint2.X), TdxPDFUtils.Min(APoint1.Y, APoint2.Y),
      TdxPDFUtils.Max(APoint1.X, APoint2.X), TdxPDFUtils.Max(APoint1.Y, APoint2.Y))));
  except
    Exit(Empty)
  end;
end;

class function TdxPDFDocumentAreaHelper.Empty: TdxPDFDocumentArea;
begin
  Result.Rect := dxNullRectF;
  Result.PageIndex := -1;
end;

{ TdxPDFPositionHelper }

class function TdxPDFPositionHelper.Create: TdxPDFPosition;
begin
  Result.Invalid;
end;

class function TdxPDFPositionHelper.Create(APageNumber: Integer; const P: TdxPointF): TdxPDFPosition;
begin
  Result.PageIndex := APageNumber;
  Result.Point := P;
end;

function TdxPDFPositionHelper.NearTo(const APosition: TdxPDFPosition): Boolean;
const
  NearDistance = 3;
var
  AOtherPoint: TdxPointF;
begin
  AOtherPoint := APosition.point;
  Result := (PageIndex = APosition.PageIndex) and (Abs(Point.X - AOtherPoint.X) <= NearDistance) and
    (Abs(Point.Y - AOtherPoint.Y) <= NearDistance);
end;

function TdxPDFPositionHelper.IsValid: Boolean;
begin
  Result := PageIndex >= 0;
end;

procedure TdxPDFPositionHelper.Invalid;
begin
  PageIndex := -1;
  Point := dxNullPointF;
end;

{ TdxPDFNamedObjectDictionary }

constructor TdxPDFNamedObjectDictionary.Create(const AResourceKey: string; const APrefix: string);
begin
  inherited Create;
  FDictionary := TDictionary<string, Integer>.Create;
  FNextResourceNumber := 0;
  FResourceKey := AResourceKey;
  FPrefix := APrefix;
end;

destructor TdxPDFNamedObjectDictionary.Destroy;
begin
  FreeAndNil(FDictionary);
  inherited Destroy;
end;

function TdxPDFNamedObjectDictionary.GetNewResourceName(ADictionary: TdxPDFReferencedObjectDictionary): string;
begin
  Result := GetNewResourceName(ADictionary, FPrefix + IntToStr(FNextResourceNumber));
  Inc(FNextResourceNumber);
end;

function TdxPDFNamedObjectDictionary.GetNewResourceName(ADictionary: TdxPDFReferencedObjectDictionary;
  const AName: string): string;
begin
  Result := AName;
  while ADictionary.ContainsKey(Result) do
  begin
    Result := FPrefix + IntToStr(FNextResourceNumber);
    Inc(FNextResourceNumber);
  end;
end;

function TdxPDFNamedObjectDictionary.ContainsValue(AValue: Integer): Boolean;
begin
  Result := FDictionary.ContainsValue(AValue)
end;

procedure TdxPDFNamedObjectDictionary.ClearResourceNames;
begin
  FDictionary.Clear;
  FNextResourceNumber := 0;
end;

{ TdxPDFArray }

constructor TdxPDFArray.Create;
begin
  inherited Create;
  FElementList := TdxPDFBaseList.Create;
end;

destructor TdxPDFArray.Destroy;
begin
  FreeAndNil(FElementList);
  inherited Destroy;
end;

procedure TdxPDFArray.Add(AValue: TdxPDFBase);
begin
  FElementList.Add(AValue);
end;

procedure TdxPDFArray.Add(AValue: Integer);
begin
  Add(TdxPDFInteger.Create(AValue));
end;

procedure TdxPDFArray.Add(AValue: Single);
begin
  Add(TdxPDFDouble.Create(AValue));
end;

procedure TdxPDFArray.Add(const AValue: string);
begin
  Add(TdxPDFString.Create(AValue));
end;

procedure TdxPDFArray.Add(ANumber, AGeneration: Integer);
begin
  FElementList.Add(TdxPDFReference.Create(ANumber, 0));
end;

class function TdxPDFArray.GetObjectType: TdxPDFBaseType;
begin
  Result := otArray;
end;

function TdxPDFArray.GetElement(AIndex: Integer): TdxPDFBase;
begin
  Result := FElementList[AIndex];
end;

function TdxPDFArray.GetCount: Integer;
begin
  Result := FElementList.Count;
end;

function TdxPDFArray.GetElementList: TdxPDFBaseList;
begin
  Result := FElementList;
end;

{ TdxPDFCustomStreamFilter }

constructor TdxPDFCustomStreamFilter.Create(ADecodeParameters: TObject);
begin
  inherited Create;
  Initialize(ADecodeParameters);
end;

class function TdxPDFCustomStreamFilter.GetName: string;
begin
  Result := '';
end;

class function TdxPDFCustomStreamFilter.GetShortName: string;
begin
  Result := '';
end;

function TdxPDFCustomStreamFilter.DecodeImageData(const AData: TBytes;
  AWidth, AHeight, AComponentCount: Integer): TdxPDFDocumentDecodedImageData;
begin
  Result.Data := Decode(AData);
  Result.PixelFormat := pfUnknown;
end;

function TdxPDFCustomStreamFilter.Decode(const AData: TBytes): TBytes;
begin
  try
    Result := DoDecode(AData);
  except
    on E: EdxPDFAbortException do
      SetLength(Result, 0)
    else
      raise;
  end;
end;

procedure TdxPDFCustomStreamFilter.Initialize(ADecodeParameters: TObject);
begin
// do nothing
end;

{ TdxPDFStream }

constructor TdxPDFStream.Create;
begin
  inherited Create;
  FFilters := TdxPDFFilters.Create;
end;

constructor TdxPDFStream.Create(ADictionary: TdxPDFDictionary);
begin
  Create;
  Dictionary := ADictionary;
end;

destructor TdxPDFStream.Destroy;
begin
  Dictionary := nil;
  FreeAndNil(FFilters);
  SetLength(FRawData, 0);
  inherited Destroy;
end;

class function TdxPDFStream.GetObjectType: TdxPDFBaseType;
begin
  Result := otStream;
end;

procedure TdxPDFStream.PopulateFilters;

  function GetDecodeParametersObject: TdxPDFBase;
  begin
    Result := Dictionary.GetObject(TdxPDFKeywords.DecodeParameters);
    if (Result = nil) or (Result.ObjectType = otDictionary) then
      Result := nil;
  end;

  function GetDecodeParameters(AObject: TdxPDFBase; AFilterIndex: Integer): TObject;
  begin
    if (AObject <> nil) and (AObject.ObjectType = otArray) and (TdxPDFArray(AObject).Count > 0) then
    begin
      Result := TdxPDFArray(AObject)[Min(AFilterIndex, TdxPDFArray(AObject).Count - 1)];
      if TdxPDFBase(Result).ObjectType = otIndirectReference then
        Result := (Dictionary as TdxPDFReaderDictionary).Repository.GetObject(TdxPDFReference(Result).Number);
    end
    else
      Result := nil;
  end;

var
  I: Integer;
  AFilter, AParameters: TdxPDFBase;
begin
  FFilters.Clear;
  AFilter := Dictionary.GetObject(TdxPDFKeywords.Filter);
  if AFilter <> nil then
    case AFilter.ObjectType of
      otName:
        AddFilter(AFilter, Dictionary.GetObject(TdxPDFKeywords.DecodeParameters));
      otArray:
        begin
          AParameters := GetDecodeParametersObject;
          for I := 0 to TdxPDFArray(AFilter).Count - 1 do
            AddFilter(TdxPDFArray(AFilter)[I] as TdxPDFBase, GetDecodeParameters(AParameters, I));
        end;
    end;
end;

function TdxPDFStream.GetRawData;
begin
  Result := GetDecryptedData;
end;

function TdxPDFStream.GetDecryptedData: TBytes;
begin
  if NeedDecrypting then
  begin
    FRawData := FEncryptionInfo.Decrypt(FRawData, Number);
    FIsDecrypted := True;
  end;
  Result := FRawData;
end;

function TdxPDFStream.GetUncompressedData: TBytes;
var
  I: Integer;
  AData: TBytes;
begin
  Result := GetRawData;
  if FFilters.Count > 0 then
  begin
    AData := Result;
    SetLength(Result, 0);
    for I := 0 to FFilters.Count - 1 do
    begin
      TdxPDFUtils.AddData((FFilters[I] as TdxPDFCustomStreamFilter).Decode(AData), Result);
      SetLength(AData, 0);
      TdxPDFUtils.AddData(Result, AData);
      SetLength(Result, 0);
    end;
    Result := AData;
  end;
end;

function TdxPDFStream.NeedDecrypting: Boolean;
begin
  Result := (Length(FRawData) > 0) and (FEncryptionInfo <> nil) and not FIsDecrypted;
end;

procedure TdxPDFStream.SetDictionary(const AValue: TdxPDFDictionary);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FDictionary))
end;

procedure TdxPDFStream.SetRawData(const AValue: TBytes);
begin
  SetLength(FRawData, 0);
  FIsDecrypted := False;
  FRawData := AValue;
end;

procedure TdxPDFStream.AddFilter(AName: TdxPDFBase; AParameters: TObject);
begin
  FFilters.Add(dxPDFCreateStreamFilter(TdxPDFName(AName).Value, AParameters));
end;

{ TdxPDFDictionary }

constructor TdxPDFDictionary.Create;
begin
  inherited Create;
  FObjects := TdxPDFReferencedObjectDictionary.Create;
  FStreamRef := nil;
end;

destructor TdxPDFDictionary.Destroy;
begin
  FStreamRef := nil;
  FreeAndNil(FObjects);
  inherited Destroy;
end;

function TdxPDFDictionary.GetObject(const AKey: string): TdxPDFBase;
var
  AResult: TdxPDFReferencedObject;
begin
  if FObjects.TryGetValue(AKey, AResult) then
    Result := AResult as TdxPDFBase
  else
    Result := nil;
end;

function TdxPDFDictionary.Contains(const AKey: string): Boolean;
begin
  Result := FObjects.ContainsKey(AKey);
end;

function TdxPDFDictionary.CreateNumericList(const AKey: string): TdxPDFDoubleList;
var
  AArray: TdxPDFArray;
  AValue: TdxPDFBase;
begin
  AArray := GetArray(AKey);
  if AArray <> nil then
  begin
    Result := TdxPDFDoubleList.Create;
    for AValue in AArray.ElementList do
      Result.Add(TdxPDFUtils.ConvertToDouble(AValue));
  end
  else
    Result := nil;
end;

function TdxPDFDictionary.GetArray(const AKey: string): TdxPDFArray;
var
  AObject: TdxPDFBase;
begin
  AObject := GetObject(AKey);
  if (AObject <> nil) and (AObject.ObjectType = otArray) then
    Result := AObject as TdxPDFArray
  else
    Result := nil;
end;

function TdxPDFDictionary.GetArray(const AKey, AAlternativeKey: string): TdxPDFArray;
begin
  Result := GetArray(AKey);
  if Result = nil then
    Result := GetArray(AAlternativeKey);
end;

function TdxPDFDictionary.GetBoolean(const AKey: string; ADefaultValue: Boolean = False): Boolean;
begin
  Result := ADefaultValue;
  if Contains(AKey) then
    Result := TdxPDFBoolean(GetObject(AKey)).Value;
end;

function TdxPDFDictionary.GetBytes(const AKey: string): TBytes;
var
  AString: TdxPDFString;
begin
  SetLength(Result, 0);
  if Contains(AKey) then
  begin
    AString := GetObject(AKey) as TdxPDFString;
    if AString <> nil then
      Result := TdxPDFUtils.StrToByteArray(AString.Value)
  end;
end;

function TdxPDFDictionary.GetDate(const AKey: string): TDateTime;
var
  APosition: Integer;
  S: string;
begin
  S := GetString(AKey);
  APosition := Pos('D', S);
  if APosition > 0 then
    S := Copy(S, APosition, MaxInt)
  else
    S := '';
  try
    Result := ParseDateTime(S);
  except
    on E: EConvertError do
      Result := NullDate
    else
      raise;
  end;
end;

function TdxPDFDictionary.GetDictionary(const AKey: string): TdxPDFDictionary;
var
  AObject: TdxPDFBase;
begin
  AObject := GetObject(AKey);
  if (AObject <> nil) and (AObject.ObjectType = otDictionary) then
    Result := AObject as TdxPDFDictionary
  else
    Result := nil;
end;

function TdxPDFDictionary.GetDouble(const AKey: string; ADefaultValue: Double = dxPDFInvalidValue): Double;
begin
  Result := ADefaultValue;
  if Contains(AKey) then
    Result := TdxPDFDouble(GetObject(AKey)).Value;
end;

function TdxPDFDictionary.GetDoubleArray(const AKey: string): TDoubleDynArray;
var
  I: Integer;
  AArray: TdxPDFArray;
begin
  SetLength(Result, 0);
  AArray := GetArray(AKey);
  if AArray <> nil then
  begin
    SetLength(Result, AArray.Count);
    for I := 0 to AArray.Count - 1 do
      Result[I] := TdxPDFUtils.ConvertToDouble(AArray.Elements[I]);
  end;
end;

function TdxPDFDictionary.GetInteger(const AKey: string; ADefaultValue: Integer = dxPDFInvalidValue): Integer;
begin
  Result := ADefaultValue;
  if Contains(AKey) then
    Result := TdxPDFInteger(GetObject(AKey)).Value;
end;

function TdxPDFDictionary.GetInteger(const AKey, AAlternativeKey: string): Integer;
begin
  Result := GetInteger(AKey);
  if not TdxPDFUtils.IsIntegerValid(Result) then
    Result := GetInteger(AAlternativeKey);
end;

function TdxPDFDictionary.GetRectangle(const AKey: string): TdxRectF;
var
  R: TdxRectF;
begin
  Result := TdxPDFUtils.ArrayToRectF(GetObject(AKey) as TdxPDFArray);
  if Result.Top < Result.Bottom then
  begin
    R := Result;
    Result.Top := Result.Bottom;
    Result.Bottom := R.Top;
  end;
end;

function TdxPDFDictionary.GetRectangle(const AKey: string; const ADefaultValue: TdxRectF): TdxRectF;
begin
  Result := ADefaultValue;
  if Contains(AKey) then
    Result := GetRectangle(AKey);
end;

function TdxPDFDictionary.GetStream(const AKey: string): TdxPDFStream;
begin
  Result := GetObject(AKey) as TdxPDFStream;
end;

function TdxPDFDictionary.GetString(const AKey: string): string;
var
  AData: TBytes;
begin
  AData := GetBytes(AKey);
  if Length(AData) <> 0 then
    Result := TdxPDFUtils.ConvertToStr(AData)
  else
    Result := '';
end;

function TdxPDFDictionary.GetString(const AKey, ADefaultValue: string): string;
begin
  Result := GetString(AKey);
  if Result = '' then
    Result := ADefaultValue;
end;

function TdxPDFDictionary.GetTextString(const AKey: string): string;
var
  AData: TBytes;
begin
  AData := GetBytes(AKey);
  if Length(AData) <> 0 then
    Result := TdxPDFUtils.ConvertToText(AData)
  else
    Result := '';
end;

function TdxPDFDictionary.TryGetArray(const AKey: string; out AValue: TdxPDFArray): Boolean;
begin
  Result := TryGetValue<TdxPDFArray>(AKey, AValue);
end;

function TdxPDFDictionary.TryGetBoolean(const AKey: string; out AValue: Boolean): Boolean;
var
  AObject: TdxPDFBoolean;
begin
  Result := TryGetValue<TdxPDFBoolean>(AKey, AObject);
  if Result then
    AValue := AObject.Value;
end;

function TdxPDFDictionary.TryGetObject(const AKey: string; out AValue: TdxPDFBase): Boolean;
begin
  Result := TryGetValue<TdxPDFBase>(AKey, AValue);
end;

function TdxPDFDictionary.TryGetReference(const AKey: string; out AValue: Integer): Boolean;
var
  AObject: TdxPDFBase;
begin
  Result := TryGetObject(AKey, AObject);
  if Result then
    AValue := AObject.Number;
end;

function TdxPDFDictionary.TryGetStream(const AKey: string; out AValue: TdxPDFStream): Boolean;
var
  AObject: TdxPDFBase;
begin
  AValue := nil;
  AObject := GetObject(AKey);
  if AObject <> nil then
    case AObject.ObjectType of
      otStream:
        AValue := AObject as TdxPDFStream;
      otDictionary:
        AValue := TdxPDFDictionary(AObject).StreamRef;
    else
      AValue := nil;
    end;
  Result := AValue <> nil;
end;

function TdxPDFDictionary.TryGetString(const AKey: string; out AValue: string): Boolean;
var
  AObject: TdxPDFString;
begin
  Result := TryGetValue<TdxPDFString>(AKey, AObject);
  if Result then
    AValue := AObject.Value;
end;

function TdxPDFDictionary.TryGetTextString(const AKey: string; out AValue: string): Boolean;
begin
  Result := Contains(AKey);
  if Result then
    AValue := GetTextString(AKey)
  else
    AValue := '';
end;

procedure TdxPDFDictionary.Add(const AKey: string; const AValue: TBytes);
begin
  Add(AKey, TdxPDFString.Create(TdxPDFUtils.BytesToStr(AValue)));
end;

procedure TdxPDFDictionary.Add(const AKey: string; AValue: TdxPDFBase);
var
  ATemp: TdxPDFReferencedObject;
begin
  ATemp := nil;
  if FObjects.TryGetValue(AKey, ATemp) and (AValue <> ATemp) or (ATemp = nil) then
    FObjects.AddOrSetValue(AKey, AValue);
end;

procedure TdxPDFDictionary.Add(const AKey: string; AValue: Integer);
begin
  Add(AKey, TdxPDFInteger.Create(AValue));
end;

procedure TdxPDFDictionary.Add(const AKey: string; AValue: Double);
begin
  Add(AKey, TdxPDFNumericObject.Create(AValue));
end;

procedure TdxPDFDictionary.Add(const AKey: string; AValue: Boolean);
begin
  Add(AKey, TdxPDFBoolean.Create(AValue));
end;

procedure TdxPDFDictionary.Add(const AKey: string; const AValue: TdxRectF);
var
  AArray: TdxPDFArray;
begin
  AArray := TdxPDFArray.Create;
  AArray.Add(TdxPDFDouble.Create(AValue.Left));
  AArray.Add(TdxPDFDouble.Create(AValue.Bottom));
  AArray.Add(TdxPDFDouble.Create(AValue.Right));
  AArray.Add(TdxPDFDouble.Create(AValue.Top));
  Add(AKey, AArray);
end;

procedure TdxPDFDictionary.Add(const AKey, AValue: string);
begin
  Add(AKey, TdxPDFString.Create(AValue));
end;

procedure TdxPDFDictionary.Clear;
begin
  FObjects.Clear;
end;

procedure TdxPDFDictionary.Remove(const AKey: string);
begin
  FObjects.Remove(AKey);
end;

class function TdxPDFDictionary.GetObjectType: TdxPDFBaseType;
begin
  Result := otDictionary;
end;

function TdxPDFDictionary.GetCount: Integer;
begin
  Result := FObjects.Count;
end;

function TdxPDFDictionary.GetItems: TDictionary<string, TdxPDFReferencedObject>;
begin
 Result := FObjects.Items;
end;

function TdxPDFDictionary.GetValue(const AKey: string): TdxPDFBase;
begin
  if not TryGetValue<TdxPDFBase>(AKey, Result) then
    Result := nil;
end;

procedure TdxPDFDictionary.SetValue(const AKey: string; AValue: TdxPDFBase);
begin
  Add(AKey, AValue);
end;

function TdxPDFDictionary.ParseDateTime(ADateTime: string): TDateTime;
var
  AUTC: TcxDateTime;
begin
  if (Length(ADateTime) > 2) and (ADateTime[1] = 'D') and (ADateTime[2] = ':') then
    Delete(ADateTime, 1, 2);
  ADateTime := StringReplace(ADateTime, '"', '', [rfReplaceAll]);
  ADateTime := StringReplace(ADateTime, '''', '', [rfReplaceAll]);
  if Length(ADateTime) >= 4 then
  begin
    AUTC.Year :=
      TdxPDFUtils.ConvertToDigit(Byte(ADateTime[1])) * 1000 + TdxPDFUtils.ConvertToDigit(Byte(ADateTime[2])) * 100 +
      TdxPDFUtils.ConvertToDigit(Byte(ADateTime[3])) * 10 + TdxPDFUtils.ConvertToDigit(Byte(ADateTime[4]));
    Delete(ADateTime, 1, 4);
    ParseDateTimeComponent(ADateTime, PInteger(@AUTC.Month), 1);
    ParseDateTimeComponent(ADateTime, PInteger(@AUTC.Day), 1);
    ParseDateTimeComponent(ADateTime, PInteger(@AUTC.Hours));
    ParseDateTimeComponent(ADateTime, PInteger(@AUTC.Minutes));
    ParseDateTimeComponent(ADateTime, PInteger(@AUTC.Seconds));
    Result := cxGetLocalCalendar.ToDateTime(AUTC);
  end
  else
    Result := NullDate;
end;

function TdxPDFDictionary.TryGetValue<T>(const AKey: string; out AValue: T): Boolean;
var
  ATemp: TdxPDFReferencedObject;
begin
  Result := FObjects.TryGetValue(AKey, ATemp);
  if Result then
    AValue := Safe<T>.Cast(ATemp);
end;

procedure TdxPDFDictionary.ParseDateTimeComponent(var ADateTime: string; AComponent: PInteger; ADefaultValue: Byte = 0);
begin
  if Length(ADateTime) >= 2 then
  begin
    AComponent^ := TdxPDFUtils.ConvertToDigit(Byte(ADateTime[1])) * 10 + TdxPDFUtils.ConvertToDigit(Byte(ADateTime[2]));
    Delete(ADateTime, 1, 2);
  end
  else
    AComponent^ := ADefaultValue;
end;

{ TdxPDFValue }

constructor TdxPDFValue.Create(ANumber, AGeneration: Integer; const AValue: Variant);
begin
  inherited Create(ANumber, AGeneration);
  InternalSetValue(AValue);
end;

constructor TdxPDFValue.Create(const AValue: Variant);
begin
  inherited Create;
  InternalSetValue(AValue);
end;

procedure TdxPDFValue.InternalSetValue(const AValue: Variant);
begin
  FValue := AValue;
end;

{ TdxPDFBoolean }

class function TdxPDFBoolean.GetObjectType: TdxPDFBaseType;
begin
  Result := otBoolean;
end;

function TdxPDFBoolean.GetValue: Boolean;
begin
  Result := InternalValue;
end;

procedure TdxPDFBoolean.SetValue(const AValue: Boolean);
begin
  SetValue(AValue);
end;

{ TdxPDFNull }

function TdxPDFNull.GetValue: Variant;
begin
  Result := varEmpty;
end;

{ TdxPDFString }

class function TdxPDFString.GetObjectType: TdxPDFBaseType;
begin
  Result := otString;
end;

function TdxPDFString.GetValue: string;
begin
  Result := VarToStr(InternalValue);
end;

procedure TdxPDFString.SetValue(const AValue: string);
begin
  InternalSetValue(AValue);
end;

{ TdxPDFComment }

class function TdxPDFComment.GetObjectType: TdxPDFBaseType;
begin
  Result := otComment;
end;

{ TdxPDFName }

class function TdxPDFName.GetObjectType: TdxPDFBaseType;
begin
  Result := otName;
end;

{ TdxPDFNumericObject }

class function TdxPDFNumericObject.GetObjectType: TdxPDFBaseType;
begin
  Result := otDouble;
end;

function TdxPDFNumericObject.GetValue: Variant;
begin
  Result := InternalValue;
end;

{ TdxPDFInteger }

class function TdxPDFInteger.GetObjectType: TdxPDFBaseType;
begin
  Result := otInteger;
end;

function TdxPDFInteger.GetValue: Integer;
begin
  Result := InternalValue;
end;

procedure TdxPDFInteger.SetValue(const AValue: Integer);
begin
  InternalSetValue(AValue);
end;

{ TdxPDFDouble }

function TdxPDFDouble.GetValue: Double;
begin
  Result := InternalValue;
end;

procedure TdxPDFDouble.SetValue(AValue: Double);
begin
  InternalSetValue(AValue);
end;

{ TdxPDFReference }

constructor TdxPDFReference.Create(ANumber, AGeneration: Integer; AOffset: Int64 = 0;
  AIsFree: Boolean = False; AIsSlot: Boolean = False);
begin
  inherited Create(ANumber, AGeneration);
  FIsFree := AIsFree;
  FIsSlot := AIsSlot;
  FOffset := AOffset;
end;

class function TdxPDFReference.GetObjectType: TdxPDFBaseType;
begin
  Result := otIndirectReference;
end;

{ TdxPDFIndirectObject }

constructor TdxPDFIndirectObject.Create(ANumber, AGeneration: Integer; const AData: TBytes);
begin
  inherited Create(ANumber, AGeneration);
  FData := AData;
end;

destructor TdxPDFIndirectObject.Destroy;
begin
  SetLength(FData, 0);
  inherited Destroy;
end;

class function TdxPDFIndirectObject.GetObjectType: TdxPDFBaseType;
begin
  Result := otIndirectObject;
end;

{ TdxPDFToken }

constructor TdxPDFToken.Create(const ADescription: string);
begin
  Create(TdxPDFUtils.StrToByteArray(ADescription));
end;

constructor TdxPDFToken.Create(const ASequence: TBytes);
begin
  FSequence := ASequence;
  FSequenceLength := Length(FSequence);
  BeginCompare;
end;

class function TdxPDFToken.BeginCompare(AToken: TdxPDFToken): TdxPDFToken;
begin
  Result := TdxPDFToken.Create(AToken.Sequence);
end;

function TdxPDFToken.Compare(ASymbol: Byte): Boolean;
begin
  if ASymbol = FCurrentComparingSymbol then
  begin
    if FIndexToCompare = FSequenceLength - 1 then
      Exit(True);
    Inc(FIndexToCompare);
    FCurrentComparingSymbol := FSequence[FIndexToCompare];
  end
  else
    if FIndexToCompare <> 0 then
      BeginCompare;
  Result := False;
end;

function TdxPDFToken.IsStartWithComment: Boolean;
begin
  Result := FSequence[0] = TdxPDFDefinedSymbols.Comment;
end;

procedure TdxPDFToken.BeginCompare;
begin
  FIndexToCompare := 0;
  FCurrentComparingSymbol := FSequence[0];
end;

{ TdxPDFTransformationMatrix }

constructor TdxPDFTransformationMatrix.CreateEx(AMatrix1: TdxPDFTransformationMatrix);
begin
  CreateEx(AMatrix1.A, AMatrix1.B, AMatrix1.C, AMatrix1.D, AMatrix1.E, AMatrix1.F);
end;

class function TdxPDFTransformationMatrix.Invert(M: TdxPDFTransformationMatrix): TdxPDFTransformationMatrix;
var
  ADeterminant: Double;
begin
  ADeterminant := M.GetDeterminant;
  Result := TdxPDFTransformationMatrix.CreateEx(
    M.D / ADeterminant,
    -M.B / ADeterminant,
    -M.C / ADeterminant,
    M.A / ADeterminant,
    (M.C * M.F - M.E * M.D) / ADeterminant,
    (M.B * M.E - M.F * M.A) / ADeterminant);
end;

class function TdxPDFTransformationMatrix.MultiplyMatrix(M1, M2: TdxPDFTransformationMatrix): TdxPDFTransformationMatrix;
var
  AMatrix1A, AMatrix1B, AMatrix1C, AMatrix1D, AMatrix1E, AMatrix1F, AMatrix2A, AMatrix2B, AMatrix2C, AMatrix2D: Double;
begin
  AMatrix1A := M1.A;
  AMatrix1B := M1.B;
  AMatrix1C := M1.C;
  AMatrix1D := M1.D;
  AMatrix1E := M1.E;
  AMatrix1F := M1.F;
  AMatrix2A := M2.A;
  AMatrix2B := M2.B;
  AMatrix2C := M2.C;
  AMatrix2D := M2.D;
  Result := TdxPDFTransformationMatrix.Create;
  Result.Assign(
    AMatrix1A * AMatrix2A + AMatrix1B * AMatrix2C,
    AMatrix1A * AMatrix2B + AMatrix1B * AMatrix2D,
    AMatrix1C * AMatrix2A + AMatrix1D * AMatrix2C,
    AMatrix1C * AMatrix2B + AMatrix1D * AMatrix2D,
    AMatrix1E * AMatrix2A + AMatrix1F * AMatrix2C + M2.E,
    AMatrix1E * AMatrix2B + AMatrix1F * AMatrix2D + M2.F);
end;

class function TdxPDFTransformationMatrix.TranslateMatrix(M: TdxPDFTransformationMatrix;
  const AOffset: TdxPointF): TdxPDFTransformationMatrix;
begin
  Result := TdxPDFTransformationMatrix.CreateEx(M.A, M.B, M.C, M.D, M.E + AOffset.X, M.F + AOffset.Y);
end;

function TdxPDFTransformationMatrix.TransformPoints(const APoints: TdxPDFPoints): TdxPDFPoints;
var
  ALength, I: Integer;
begin
  ALength := Length(APoints);
  SetLength(Result, ALength);
  for I := 0 to ALength - 1 do
    Result[I] := Transform(APoints[I]);
end;

procedure TdxPDFTransformationMatrix.Write(AStream: TdxPDFBytesStream);
begin
  AStream.WriteSpace;
  AStream.WriteDouble(A);
  AStream.WriteSpace;
  AStream.WriteDouble(B);
  AStream.WriteSpace;
  AStream.WriteDouble(C);
  AStream.WriteSpace;
  AStream.WriteDouble(D);
  AStream.WriteSpace;
  AStream.WriteDouble(E);
  AStream.WriteSpace;
  AStream.WriteDouble(F);
end;

function TdxPDFTransformationMatrix.GetA: Double;
begin
  Result := XForm.eM11;
end;

function TdxPDFTransformationMatrix.GetB: Double;
begin
  Result := XForm.eM12;
end;

function TdxPDFTransformationMatrix.GetC: Double;
begin
  Result := XForm.eM21;
end;

function TdxPDFTransformationMatrix.GetD: Double;
begin
  Result := XForm.eM22;
end;

function TdxPDFTransformationMatrix.GetE: Double;
begin
  Result := XForm.eDx;
end;

function TdxPDFTransformationMatrix.GetF: Double;
begin
  Result := XForm.eDy;
end;

function TdxPDFTransformationMatrix.GetDeterminant: Double;
begin
  Result := A * D - B * C;
end;

function TdxPDFTransformationMatrix.GetIsInvertable: Boolean;
var
  AMax: Double;
begin
  AMax := TdxPDFUtils.Max(Abs(C * F - E * D), Abs(B * E - F * A));
  AMax := TdxPDFUtils.Max(AMax, TdxPDFUtils.Max(Abs(A), Abs(B)));
  AMax := TdxPDFUtils.Max(AMax, TdxPDFUtils.Max(Abs(C), Abs(D)));
  Result := (AMax + GetDeterminant <> AMax);
end;

function TdxPDFTransformationMatrix.GetIsRotated: Boolean;

  function IsZeroComponent(AComponent: Double): Boolean;
  begin
    Result := Abs(AComponent) < 1e-6;
  end;

begin
  Result := not (IsZeroComponent(A) and IsZeroComponent(D) or IsZeroComponent(B) and IsZeroComponent(C));
end;

{ TdxPDFStreamElement }

constructor TdxPDFStreamElement.Create(ANumber: Integer; AIndex: Integer);
begin
  inherited Create(ANumber, 0, 0, False);
  FIndex := AIndex;
end;

class function TdxPDFStreamElement.GetObjectType: TdxPDFBaseType;
begin
  Result := otStreamElement;
end;

{ TdxPDFCustomRepository }

constructor TdxPDFCustomRepository.Create;
begin
  inherited Create;
  FReferences := TdxPDFBaseReferences.Create;
end;

destructor TdxPDFCustomRepository.Destroy;
begin
  FreeAndNil(FReferences);
  inherited Destroy;
end;

function TdxPDFCustomRepository.DecryptString(const S: string): string;
begin
  Result := S;
end;

function TdxPDFCustomRepository.GetArray(ANumber: Integer): TdxPDFArray;
var
  AResult: TdxPDFReferencedObject;
begin
  AResult := GetObject(ANumber);
  if TdxPDFBase(AResult).ObjectType = otArray then
    Result := TdxPDFArray(AResult)
  else
    Result := nil;
end;

function TdxPDFCustomRepository.GetDictionary(ANumber: Integer): TdxPDFDictionary;
var
  AResult: TdxPDFReferencedObject;
begin
  AResult := ResolveObject(ANumber);
  Result := nil;
  if (AResult <> nil) and (AResult is TdxPDFBase) then
    case TdxPDFBase(AResult).ObjectType of
      otStream:
        Result := TdxPDFStream(AResult).Dictionary;
      otDictionary:
        Result := AResult as TdxPDFDictionary;
    end;
end;

function TdxPDFCustomRepository.GetInteger(ANumber: Integer): TdxPDFInteger;
begin
  Result := GetObject(ANumber) as TDxPDFInteger;
end;

function TdxPDFCustomRepository.GetObject(ANumber: Integer): TdxPDFReferencedObject;
begin
  Result := ResolveObject(ANumber);
end;

function TdxPDFCustomRepository.GetStream(ANumber: Integer): TdxPDFStream;
var
  AObject: TdxPDFBase;
begin
  Result := nil;
  AObject := GetObject(ANumber) as TdxPDFBase;
  if AObject <> nil then
    if AObject.ObjectType = otDictionary then
      Result := TdxPDFDictionary(AObject).StreamRef
    else
      Result := AObject as TdxPDFStream;
end;

procedure TdxPDFCustomRepository.Add(ANumber: Integer; AObject: TdxPDFReferencedObject; ACanReplace: Boolean = True);
begin
  TryAdd(ANumber, AObject, ACanReplace);
end;

procedure TdxPDFCustomRepository.Clear;
begin
  References.Clear;
end;

procedure TdxPDFCustomRepository.Remove(ANumber: Integer);
begin
  References.Remove(ANumber);
end;

procedure TdxPDFCustomRepository.TrimExcess;
begin
  FReferences.TrimExcess;
end;

function TdxPDFCustomRepository.ResolveObject(ANumber: Integer): TdxPDFReferencedObject;
begin
  if not References.TryGetValue(ANumber, Result) then
    Result := nil;
end;

procedure TdxPDFCustomRepository.Replace(ANumber: Integer; AObject: TdxPDFReferencedObject);
var
  ATemp: TdxPDFReferencedObject;
begin
  if References.TryGetValue(ANumber, ATemp) and (ATemp <> AObject) or (ATemp = nil) then
  begin
    Remove(ANumber);
    Add(ANumber, AObject);
  end;
end;

procedure TdxPDFCustomRepository.TryAdd(ANumber: Integer; AObject: TdxPDFReferencedObject; ACanReplace: Boolean);
begin
  if not References.ContainsKey(ANumber) then
    References.Add(ANumber, AObject)
  else
    if ACanReplace then
      Replace(ANumber, AObject)
    else
      dxPDFFreeObject(AObject);
end;

{ TdxPDFRange }

class function TdxPDFRange.Create(AMin, AMax: Double): TdxPDFRange;
begin
  Result.Min := AMin;
  Result.Max := AMax;
end;

class function TdxPDFRange.Invalid: TdxPDFRange;
begin
  Result := Create(-MaxInt, -MaxInt);
end;

function TdxPDFRange.Contains(AValue: Integer): Boolean;
begin
  Result := InRange(AValue, Min, Max);
end;

function TdxPDFRange.IsSame(ARange: TdxPDFRange): Boolean;
begin
  Result := SameValue(Min, ARange.Min) and SameValue(Max, ARange.Max);
end;

{ TdxPDFColor }

constructor TdxPDFColor.Create;
begin
  inherited Create;
  Pattern := nil;
  SetLength(FComponents, 0);
end;

constructor TdxPDFColor.Create(const AComponents: array of Double);
begin
  Create(AComponents);
end;

constructor TdxPDFColor.Create(const AComponents: TDoubleDynArray);
begin
  Create;
  TdxPDFUtils.AddData(AComponents, FComponents);
end;

constructor TdxPDFColor.Create(AColor: TdxPDFColor);
begin
  Create;
  Assign(AColor);
end;

constructor TdxPDFColor.Create(X, Y, Z, AWhitePointZ: Double);
var
  AComponents: TDoubleDynArray;
begin
  Create;
  AComponents := GetComponents(X, Y, X, AWhitePointZ);
  AddComponent(AComponents[0]);
  AddComponent(AComponents[1]);
  AddComponent(AComponents[2]);
end;

destructor TdxPDFColor.Destroy;
begin
  Pattern := nil;
  SetLength(FComponents, 0);
  inherited Destroy;
end;

class function TdxPDFColor.GetComponents(X, Y, Z, AWhitePointZ: Double): TDoubleDynArray;
var
  ARed, AGreen, ABlue: Double;
begin
  if AWhitePointZ < 1 then
  begin
    ARed := X * 3.1339 + Y * -1.6170 + Z * -0.4906;
    AGreen := X * -0.9785 + Y * 1.9160 + Z * 0.0333;
    ABlue := X * 0.0720 + Y * -0.2290 + Z * 1.4057;
  end
  else
  begin
    ARed := X * 3.2406 + Y * -1.5372 + Z * -0.4986;
    AGreen := X * -0.9689 + Y * 1.8758 + Z * 0.0415;
    ABlue := X * 0.0557 + Y * -0.2040 + Z * 1.0570;
  end;
  SetLength(Result, 3);
  Result[0] := ColorComponentTransferFunction(ARed);
  Result[1] := ColorComponentTransferFunction(AGreen);
  Result[2] := ColorComponentTransferFunction(ABlue);
end;

procedure TdxPDFColor.Assign(AColor: TdxPDFColor);
begin
  Pattern := AColor.Pattern;
  SetLength(FComponents, 0);
  TdxPDFUtils.AddData(AColor.Components, FComponents);
end;

class function TdxPDFColor.ClipColorComponent(AComponent: Double): Double;
begin
  Result := TdxPDFUtils.Min(1, TdxPDFUtils.Max(0, AComponent));
end;

class function TdxPDFColor.ColorComponentTransferFunction(AComponent: Double): Double;
var
  ATemp: Double;
begin
  ATemp := ClipColorComponent(AComponent);
  if ATemp > 0.0031308 then
    Result := ClipColorComponent(Power(ATemp, 1 / 2.4) * 1.055 - 0.055)
  else
    Result := ClipColorComponent(ATemp * 12.92);
end;

procedure TdxPDFColor.AssignAndTransferComponents(const AComponents: TDoubleDynArray);
var
  AValue: Double;
begin
  SetLength(FComponents, 0);
  if AComponents <> nil then
    for AValue in AComponents do
      AddComponent(AValue);
end;

procedure TdxPDFColor.AddComponent(AValue: Double);
begin
  TdxPDFUtils.AddValue(ColorComponentTransferFunction(AValue), FComponents);
end;

procedure TdxPDFColor.SetPattern(const AValue: TdxPDFReferencedObject);
begin
  dxPDFChangeValue(AValue, FPattern);
end;

{ TdxPDFARGBColor }

constructor TdxPDFARGBColor.Create(AColor: TdxPDFColor);
begin
  inherited Create;
  if AColor <> nil then
    case Length(AColor.Components) of
      1:
        CreateFromRGB(AColor.Components[0], AColor.Components[0], AColor.Components[0]);
      3:
        CreateFromRGB(AColor.Components[0], AColor.Components[1], AColor.Components[2]);
      4:
        CreateFromCMYK(AColor.Components[0], AColor.Components[1], AColor.Components[2], AColor.Components[3]);
    else
      CreateFromRGB(0, 0, 0);
    end
  else
    TdxPDFUtils.RaiseTestException;
end;

constructor TdxPDFARGBColor.CreateFromRGB(ARed, AGreen, ABlue: Double; AAlpha: Byte = 255);
begin
  Create;
  FRed := ARed;
  FGreen := AGreen;
  FBlue := ABlue;
  FAlpha := AAlpha;
end;

constructor TdxPDFARGBColor.CreateFromCMYK(ACyan: Double; AMagenta: Double; AYellow: Double; ABlack: Double);
var
  ACyanComplement, AMagentaComplement, AYellowComplement, ABlackComplement, AAddition, ARed, AGreen, ABlue: Double;
begin
  Create;
  ACyanComplement := 1 - ACyan;
  AMagentaComplement := 1 - AMagenta;
  AYellowComplement := 1 - AYellow;
  ABlackComplement := 1 - ABlack;
  AAddition := ACyanComplement * AMagentaComplement * AYellowComplement * ABlackComplement;
  ARed := AAddition;
  AGreen := AAddition;
  ABlue := AAddition;
  AAddition := ACyanComplement * AMagentaComplement * AYellowComplement * ABlack;

  ARed := ARed + 0.1373 * AAddition;
  AGreen := AGreen + 0.1216 * AAddition;
  ABlue := ABlue + 0.1255 * AAddition;
  AAddition := ACyanComplement * AMagentaComplement * AYellow * ABlackComplement;

  ARed := ARed + AAddition;
  AGreen := AGreen + 0.9490 * AAddition;
  AAddition := ACyanComplement * AMagentaComplement * AYellow * ABlack;

  ARed := ARed + 0.1098 * AAddition;
  AGreen := AGreen + 0.1020 * AAddition;
  AAddition := ACyanComplement * AMagenta * AYellowComplement * ABlackComplement;

  ARed := ARed + 0.9255 * AAddition;
  ABlue := ABlue + 0.5490 * AAddition;
  ARed := ARed + 0.1412 * (ACyanComplement * AMagenta * AYellowComplement * ABlack);
  AAddition := ACyanComplement * AMagenta * AYellow * ABlackComplement;

  ARed := ARed + 0.9294 * AAddition;
  AGreen := AGreen + 0.1098 * AAddition;
  ABlue := ABlue + 0.1412 * AAddition;
  ARed := ARed + 0.1333 * (ACyanComplement * AMagenta * AYellow * ABlack);
  AAddition := ACyan * AMagentaComplement * AYellowComplement * ABlackComplement;

  AGreen := AGreen + 0.6784 * AAddition;
  ABlue := ABlue + 0.9373 * AAddition;
  AAddition := ACyan * AMagentaComplement * AYellowComplement * ABlack;

  AGreen := AGreen + 0.0588 * AAddition;
  ABlue := ABlue + 0.1412 * AAddition;
  AAddition := ACyan * AMagentaComplement * AYellow * ABlackComplement;

  AGreen := AGreen + 0.6510 * AAddition;
  ABlue := ABlue + 0.3137 * AAddition;
  AGreen := AGreen + 0.0745 * (ACyan * AMagentaComplement * AYellow * ABlack);
  AAddition := ACyan * AMagenta * AYellowComplement * ABlackComplement;

  ARed := ARed + 0.1804 * AAddition;
  AGreen := AGreen + 0.1922 * AAddition;
  ABlue := ABlue + 0.5725 * AAddition;
  ABlue := ABlue + 0.0078 * (ACyan * AMagenta * AYellowComplement * ABlack);
  AAddition := ACyan * AMagenta * AYellow * ABlackComplement;

  FRed := TdxPDFColor.ClipColorComponent(ARed + 0.2118 * AAddition);
  FGreen := TdxPDFColor.ClipColorComponent(AGreen + 0.2119 * AAddition);
  FBlue := TdxPDFColor.ClipColorComponent(ABlue + 0.2235 * AAddition);
end;

class function TdxPDFARGBColor.ConvertToBytes(ACyan, AMagenta, AYellow, ABlack: Byte): TBytes;
var
  ACyanComplement, AMagentaComplement, AYellowComplement, ABlackComplement, ABlackDiv, AAddition, ARed, AGreen, ABlue: Double;
  D: Integer;
begin
  SetLength(Result , 3);
  Result[0] := 0;
  Result[1] := 0;
  Result[2] := 0;
  if ABlack <> 255 then
  begin
    ACyanComplement := 255 - ACyan;
    AMagentaComplement := 255 - AMagenta;
    AYellowComplement := 255 - AYellow;
    ABlackComplement := 255 - ABlack;
    ABlackDiv := ABlack / ABlackComplement;

    AAddition := ACyanComplement * AMagentaComplement * AYellowComplement * ABlackComplement;
    ARed := AAddition;
    AGreen := AAddition;
    ABlue := AAddition;

    AAddition := AAddition * ABlackDiv;
    ARed := ARed + 0.1373 * AAddition;
    AGreen := AGreen  + 0.1216 * AAddition;
    ABlue := ABlue + 0.1255 * AAddition;
    AAddition := ACyanComplement * AMagentaComplement * AYellow * ABlackComplement;
    ARed := ARed + AAddition;
    AGreen := AGreen + 0.9490 * AAddition;

    AAddition := AAddition * ABlackDiv;
    ARed := ARed + 0.1098 * AAddition;
    AGreen := AGreen + 0.1020 * AAddition;
    AAddition := ACyanComplement * AMagenta * AYellowComplement * ABlackComplement;
    ARed := ARed + 0.9255 * AAddition;
    ABlue := ABlue + 0.5490 * AAddition;

    ARed := ARed + 0.1412 * (AAddition * ABlackDiv);
    AAddition := ACyanComplement * AMagenta * AYellow * ABlackComplement;
    ARed := ARed + 0.9294 * AAddition;
    AGreen := AGreen + 0.1098 * AAddition;
    ABlue := ABlue + 0.1412 * AAddition;
    ARed := ARed + 0.1333 * AAddition * ABlackDiv;
    AAddition := ACyan * AMagentaComplement * AYellowComplement * ABlackComplement;
    AGreen := AGreen + 0.6784 * AAddition;
    ABlue := ABlue + 0.9373 * AAddition;

    AAddition := AAddition * ABlackDiv;
    AGreen := AGreen + 0.0588 * AAddition;
    ABlue := ABlue + 0.1412 * AAddition;
    AAddition := ACyan * AMagentaComplement * AYellow * ABlackComplement;
    AGreen := AGreen + 0.6510 * AAddition;
    ABlue := ABlue + 0.3137 * AAddition;
    AGreen := AGreen + 0.0745 * (ACyan * AMagentaComplement * AYellow * ABlack);
    AAddition := ACyan * AMagenta * AYellowComplement * ABlackComplement;
    ARed := ARed + 0.1804 * AAddition;
    AGreen := AGreen + 0.1922 * AAddition;
    ABlue := ABlue + 0.5725 * AAddition;
    ABlue := ABlue + 0.0078 * (AAddition * ABlackDiv);
    AAddition := ACyan * AMagenta * AYellow * ABlackComplement;

    D := 16581375;
    SetLength(Result , 3);
    Result[0] := Trunc((ARed + 0.2118 * AAddition) / D);
    Result[1] := Trunc((AGreen + 0.2119 * AAddition) / D);
    Result[2] := Trunc((ABlue + 0.2235 * AAddition) / D);
  end;
end;

class function TdxPDFARGBColor.ConvertToRGB(const AData: TBytes; APixelFormat: TdxPDFPixelFormat): TBytes;
var
  I, AIndex: Integer;
begin
  case APixelFormat of
    pfGray1bit:
      SetLength(Result, 0);
    pfGray8bit:
      begin
        AIndex := 0;
        SetLength(Result, Length(AData) * 3);
        for I := 0 to Length(AData) - 1 do
        begin
          TdxPDFUtils.CopyData(AData, I, Result, AIndex, 3);
          Inc(AIndex, 3);
        end;
      end;
  else
    Result := AData;
  end;
end;

{ TdxPDFCustomProperties }

constructor TdxPDFCustomProperties.Create(ADictionary: TdxPDFDictionary);
var
  AKey: string;
begin
  inherited Create;
  FDictionary := TdxPDFDictionary.Create;
  for AKey in ADictionary.Items.Keys do
    Dictionary.Add(AKey, ADictionary.GetObject(AKey));
end;

destructor TdxPDFCustomProperties.Destroy;
begin
  Dictionary := nil;
  inherited Destroy;
end;

procedure TdxPDFCustomProperties.SetDictionary(const AValue: TdxPDFDictionary);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FDictionary));
end;

{ TdxPDFBlendModeDictionary }

constructor TdxPDFBlendModeDictionary.Create;
begin
  inherited Create;
  FDictionary := TDictionary<string, TdxPDFBlendMode>.Create;
  FDictionary.Add('Normal', bmNormal);
  FDictionary.Add('Compatible', bmCompatible);
  FDictionary.Add('Multiply', bmMultiply);
  FDictionary.Add('Screen', bmScreen);
  FDictionary.Add('Overlay', bmOverlay);
  FDictionary.Add('Darken', bmDarken);
  FDictionary.Add('Lighten', bmLighten);
  FDictionary.Add('ColorDodge', bmColorDodge);
  FDictionary.Add('ColorBurn', bmColorBurn);
  FDictionary.Add('HardLight', bmHardLight);
  FDictionary.Add('SoftLight', bmSoftLight);
  FDictionary.Add('Difference', bmDifference);
  FDictionary.Add('Exclusion', bmExclusion);
  FDictionary.Add('Hue', bmHue);
  FDictionary.Add('Saturation', bmSaturation);
  FDictionary.Add('Color', bmColor);
  FDictionary.Add('Luminosity', bmLuminosity);
end;

destructor TdxPDFBlendModeDictionary.Destroy;
begin
  FreeAndNil(FDictionary);
  inherited Destroy;
end;

function TdxPDFBlendModeDictionary.TryGetValue(const AKey: string; out AValue: TdxPDFBlendMode): Boolean;
begin
  Result := FDictionary.TryGetValue(AKey, AValue);
end;

initialization

finalization
  FreeAndNil(dxgPDFBlendModeDictionary);

end.

