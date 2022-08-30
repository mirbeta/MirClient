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

unit dxPDFFontUtils;

{$I cxVer.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections, Classes, Graphics, dxCoreClasses, cxVariants, cxGeometry,
  dxGDIPlusClasses, dxPDFBase, dxPDFTypes, dxPDFCore, dxPDFFont, dxFontFile, dxPDFText, dxPDFCharacterMapping;

const
  sdxPDFTempFolder: string = '';

type
  { IdxPDFFontObject }

  IdxPDFFontObject = interface
  ['{F037419E-C902-4EC9-B664-2B0F3EC52615}']
    function GetFont: TdxPDFCustomFont;
    function IsUnsupportedFont: Boolean;
    procedure AddSubset(AMapping: TDictionary<Integer, string>);
    procedure UpdateFont;
  end;

  TdxPDFFontRegistratorParameters = record
    Name: string;
    IsItalic: Boolean;
    Weight: Integer;
    WidthFactor: Single;
  end;

  { TdxPDFFontCustomRegistrator }

  TdxPDFFontCustomRegistrator = class
  strict private
    FFont: IdxPDFFont;
  protected
    function PerformRegister: string; virtual; abstract;
    procedure Unregister; virtual; abstract;

    property Font: IdxPDFFont read FFont;
  public
    constructor Create(const AFont: IdxPDFFont); overload;
    destructor Destroy; override;

    function CreateSubstituteFontData: TdxPDFFontRegistrationData; virtual;

    class function CreateRegistrator(const AFont: IdxPDFFont; const AFolderName: string): TdxPDFFontCustomRegistrator;
    function Register: TdxPDFFontRegistrationData;
  end;

  { TdxPDFFontInMemoryRegistrator }

  TdxPDFFontInMemoryRegistrator = class(TdxPDFFontCustomRegistrator)
  strict private
    FFontFileData: TBytes;
    FFontHandle: THandle;
    function CreateHandle: Boolean;
    function HandleAllocated: Boolean;
  protected
    function GetSubstituteFontParameters(AFont: TdxPDFCustomFont): TdxPDFFontRegistratorParameters;
    function PerformRegister: string; override;
    procedure Unregister; override;

    function ShouldSubstituteFont(const ANormalizedFamilyName: string): Boolean; virtual;
  public
    constructor Create(const AFont: IdxPDFFont; const AFontFileData: TBytes);
  end;

  { TdxPDFOpenTypeFontCreator }

  TdxPDFOpenTypeFontCreator = class
  public
    class function GetFontFileData(const AFont: IdxPDFFont): TBytes;
  end;

  { TdxPDFTrueTypeFontRegistrator }

  TdxPDFTrueTypeFontRegistrator = class(TdxPDFFontInMemoryRegistrator)
  public
    constructor Create(const AFont: IdxPDFFont);
    function CreateSubstituteFontData: TdxPDFFontRegistrationData; override;
    function ShouldSubstituteFont(const ANormalizedFamilyName: string): Boolean; override;
  end;

  { TdxPDFOpenTypeFontRegistrator }

  TdxPDFOpenTypeFontRegistrator = class(TdxPDFFontInMemoryRegistrator)
  public
    constructor Create(const AFont: IdxPDFFont);
    function CreateSubstituteFontData: TdxPDFFontRegistrationData; override;
  end;

  { TdxPDFType1FontRegistrator }

  TdxPDFType1FontRegistrator = class(TdxPDFFontCustomRegistrator)
  strict private
    FFontName: string;
    FFontResourceName: string;
    FPfbFileName: string;
    FPfmFileName: string;
    class procedure DeleteFontFile(const AFileName: string); static;
  protected
    function PerformRegister: string; override;
  public
    constructor Create(const AFont: IdxPDFFont; const AFontFolderName: string);
    destructor Destroy; override;

    procedure Unregister; override;
  end;

  TdxFontFileInfo = record
    Data: TBytes;
    Encoding: TSmallIntDynArray;
    GlyphMapping: TdxPDFWordDictionary;
    GlyphRanges: TList<TdxFontFileCMapGlyphRange>;
  end;

  { TdxFontFileHelper }

  TdxFontFileHelper = class
  strict private
    class function CalculateAverageGlyphWidth(AFont: IdxPDFFont): SmallInt;
    class function CreateHeadTable(AFont: IdxPDFFont): TdxFontFileHeadTable;
    class function CreateHheaTable(AFont: IdxPDFFont): TdxFontFileHheaTable;
    class function CreateOS2Table(AFont: IdxPDFFont): TdxFontFileOS2Table;
    class function CreatePostTable(AFont: IdxPDFFont): TdxFontFilePostTable;
    class function PatchCMapTable(AFile: TdxFontFile; const AFont: IdxPDFFont;
      var AEncoding: TSmallIntDynArray): TdxFontFileCMapSegmentMappingRecord;
    class procedure PatchNameTable(AFile: TdxFontFile; const ABaseFontName: string);
    class procedure PatchOS2Table(AFile: TdxFontFile; AFontDescriptor: TdxPDFFontDescriptor);
  public
    class function CreateOpenTypeFile(const AFont: IdxPDFFont): TdxFontFile;
    class function GetCFFData(const AData: TBytes): TBytes;
    class function GetFontFileInfo(const AFont: IdxPDFFont; const AFontData: TBytes): TdxFontFileInfo;
    class function GetValidCFFData(const AData: TBytes): TBytes;
  end;

  { TdxPDFFontDataFacade }

  TdxPDFFontDataFacade = class
  strict private
    FFontFamily: string;
    FFontMetrics: TdxFontFileFontMetrics;
    FFontName: string;
    FFontStyle: TdxGPFontStyle;

    function GetBold: Boolean;
    function GetItalic: Boolean;
    function GetUnderline: Boolean;
    function GetStrikeout: Boolean;
    function GetIsSymbolFont: Boolean;
  protected
    function GetEmulateBold: Boolean; virtual; abstract;
    function GetEmulateItalic: Boolean; virtual; abstract;
    function GetFontFileBasedName: string; virtual;
  public
    constructor Create(const AFontFamily: string; AFontStyle: TdxGPFontStyle; AFontMetrics: TdxFontFileFontMetrics);

    function CreateFontDescriptorData: TdxPDFFontDescriptorData; virtual; abstract;
    function CreateFontFileSubset(ASubset: TDictionary<Integer, string>): TBytes; virtual; abstract;
    function GetGlyphWidth(AGlyphIndex: Integer): Double; virtual; abstract;

    property Bold: Boolean read GetBold;
    property EmulateBold: Boolean read GetEmulateBold;
    property EmulateItalic: Boolean read GetEmulateItalic;
    property FontFamily: string read FFontFamily;
    property FontFileBasedName: string read GetFontFileBasedName;
    property FontName: string read FFontName;
    property IsSymbolFont: Boolean read GetIsSymbolFont;
    property Italic: Boolean read GetItalic;
    property Metrics: TdxFontFileFontMetrics read FFontMetrics;
    property Strikeout: Boolean read GetStrikeout;
    property Style: TdxGPFontStyle read FFontStyle;
    property Underline: Boolean read GetUnderline;
  end;

  { TdxPDFFontFileDataFacade }

  TdxPDFFontFileDataFacade = class(TdxPDFFontDataFacade)
  strict private
    FFontFile: TdxFontFile;
    FFontFileBasedName: string;
    FEmulateBold: Boolean;
    FEmulateItalic: Boolean;
  protected
    function GetFontFileBasedName: string; override;
    function GetEmulateBold: Boolean; override;
    function GetEmulateItalic: Boolean; override;
  public
    constructor Create(const AFontFamily: string; AFontStyle: TdxGPFontStyle; AFontFile: TdxFontFile; AEmulateBold: Boolean);
    destructor Destroy; override;
    function CreateFontDescriptorData: TdxPDFFontDescriptorData; override;
    function CreateFontFileSubset(ASubset: TDictionary<Integer, string>): TBytes; override;
    function GetGlyphWidth(AGlyphIndex: Integer): Double; override;
  end;

  { TdxPDFCharacterSet }

  TdxPDFCharacterSet = class
  strict private
    FCurrentSet: TDictionary<Integer, string>;
    FHasNewCharacters: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddSubset(AToUnicode: TDictionary<Integer, string>);

    property CurrentSet: TDictionary<Integer, string> read FCurrentSet;
    property HasNewCharacters: Boolean read FHasNewCharacters write FHasNewCharacters;
  end;

  { TdxPDFEditableFontObject }

  TdxPDFEditableFontObject = class(TInterfacedObject, IdxPDFFontObject)
  strict private
    FCharSet: TdxPDFCharacterSet;
    FShouldUseTwoByteGlyphIndex: Boolean;
  protected
    function GetFont: TdxPDFCustomFont; virtual; abstract;
    function IsUnsupportedFont: Boolean; virtual; abstract;
    procedure SetFont(const AValue: TdxPDFCustomFont); virtual; abstract;
    procedure UpdateFontFile(ASubset: TDictionary<Integer, string>); virtual; abstract;

    property Font: TdxPDFCustomFont read GetFont write SetFont;
  public
    constructor Create(AShouldUseTwoByteGlyphIndex: Boolean);
    destructor Destroy; override;

    procedure AddSubset(ASubset: TDictionary<Integer, string>);
    procedure UpdateFont;
  end;

  { TdxPDFEmbeddedEditableFontObject }

  TdxPDFEmbeddedEditableFontObject = class(TdxPDFEditableFontObject)
  strict private
    FFont: TdxPDFDeferredCIDType2Font;
    FFontDataFacade: TdxPDFFontDataFacade;
    FIsUnsupportedFont: Boolean;
  protected
    function GetFont: TdxPDFCustomFont; override;
    function IsUnsupportedFont: Boolean; override;
    procedure SetFont(const AValue: TdxPDFCustomFont); override;
    procedure UpdateFontFile(ASubset: TDictionary<Integer, string>); override;
  public
    constructor Create(AFontDataFacade: TdxPDFFontDataFacade; AIsUnsupportedFont: Boolean);
    destructor Destroy; override;
  end;

  { TdxPDFNotEmbeddedEditableFontObject }

  TdxPDFNotEmbeddedEditableFontObject = class(TdxPDFEditableFontObject)
  strict private
    FFont: TdxPDFTrueTypeFont;
    FPDFWinAnsiEncodingName: string;
  protected
    function GetFont: TdxPDFCustomFont; override;
    function IsUnsupportedFont: Boolean; override;
    procedure SetFont(const AValue: TdxPDFCustomFont); override;
    procedure UpdateFontFile(ASubset: TDictionary<Integer, string>); override;
  public
    constructor Create(AFontDataFacade: TdxPDFFontDataFacade);
    destructor Destroy; override;

    procedure AddGlyphToEncoding(const AGlyphName: string; AGlyphIndex: Byte; AWidth: Double);
  end;

  { TdxPDFEditableFontData }

  TdxPDFEditableFontData = class(TdxPDFObject)
  strict private
    FFontDataFacade: TdxPDFFontDataFacade;
    FFontObject: IdxPDFFontObject;
    FFontStyle: TdxGPFontStyle;
    FMapper: TdxPDFGlyphMapper;

    function GetBold: Boolean;
    function GetFont: TdxPDFCustomFont;
    function GetItalic: Boolean;
    function GetMetrics: TdxFontFileFontMetrics;
    function GetNeedEmulateBold: Boolean;
    function GetNeedEmulateItalic: Boolean;
    function GetStrikeout: Boolean;
    function GetUnderline: Boolean;

    function GetCharacterWidth(ACh: Char): Double;
    function HasFlag(AFontStyle: TdxGPFontStyle): Boolean;
  protected
    procedure DestroySubClasses; override;

    property Bold: Boolean read GetBold;
    property FontStyle: TdxGPFontStyle read FFontStyle;
    property Italic: Boolean read GetItalic;
  public
    constructor Create(AFontObject: IdxPDFFontObject; AFontDataFacade: TdxPDFFontDataFacade;
      AMapper: TdxPDFGlyphMapper); reintroduce;

    function CreateGlyphRun: TdxPDFGlyphRun;
    function GetTextWidth(const AText: string; AFontSize: Double; ATextState: TdxPdfInteractiveFormFieldTextState): Double;
    function ProcessString(const AStr: string; AFlags: TdxPDFGlyphMappingFlags): TdxPDFGlyphRun;

    procedure UpdateFont;
    procedure UpdateFontFile;

    property Font: TdxPDFCustomFont read GetFont;
    property Metrics: TdxFontFileFontMetrics read GetMetrics;
    property NeedEmulateBold: Boolean read GetNeedEmulateBold;
    property NeedEmulateItalic: Boolean read GetNeedEmulateItalic;
    property Strikeout: Boolean read GetStrikeout;
    property Underline: Boolean read GetUnderline;
  end;

  { TdxPDFEditableFontDataCache }

  TdxPDFEditableFontDataCache = class
  strict private
    FFontCache: TObjectDictionary<Integer, TdxPDFEditableFontData>;
    FCatalog: TdxPDFCatalog;
  strict protected type
    TCacheKey = record
      FontFamily: string;
      Style: TdxGPFontStyle;
      class function Create(const AFontFamily: string; AStyle: TdxGPFontStyle): TCacheKey; static;
      function GetHash: Integer;
    end;
  protected
    function EmbedFont(const AFontFamily: string): Boolean;
    function TryGetEditableFontData(const AKey: TCacheKey; out AFontData: TdxPDFEditableFontData): Boolean;
    procedure CacheEditableFontData(const AKey: TCacheKey; AFontData: TdxPDFEditableFontData);
  public
    constructor Create(ACatalog: TdxPDFCatalog);
    destructor Destroy; override;

    procedure Clear;
    procedure UpdateFonts;
  end;

  { TdxPDFGDIEditableFontDataCache }

  TdxPDFGDIEditableFontDataCache = class(TdxPDFEditableFontDataCache)
  public
    function GetFontData(const AFontFamily: string; AFontStyle: TdxGPFontStyle; ACreateFont: Boolean): TdxPDFEditableFontData;
    function SearchFontData(const AFontFamilyName: string; AFontStyle: TdxGPFontStyle): TdxPDFEditableFontData;
  end;

implementation

uses
{$IFDEF DELPHI22}
  System.Hash,
{$ENDIF}
  Windows, Math, dxCore, dxTypeHelpers, dxStringHelper, dxCoreGraphics, dxGDIPlusAPI, dxHash, dxPDFPostScript,
  dxPDFFontEncoding, dxPDFType1Font, dxPDFUtils;

type
  TdxFontFileAccess = class(TdxFontFile);
  TdxFontFileHeadTableAccess = class(TdxFontFileHeadTable);
  TdxFontFileHheaTableAccess = class(TdxFontFileHheaTable);
  TdxFontFileOS2TableAccess = class(TdxFontFileOS2Table);
  TdxFontFilePostTableAccess = class(TdxFontFilePostTable);
  TdxPDFCustomFontAccess = class(TdxPDFCustomFont);
  TdxPDFObjectAccess = class(TdxPDFObject);
  TdxPDFPostScriptDictionaryAccess = class(TdxPDFPostScriptDictionary);

  TdxPDFDefaultSystemFontNameMapping = class(TDictionary<string, string>);

  { TdxPDFType1FontFileCreator }

  TdxPDFType1FontFileCreator = class
  strict private const
  {$REGION 'private const'}
    DefaultNullSegment: array[0.. 525] of Byte = (
      $0a, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $0a, $63, $6c, $65, $61, $72, $74, $6f, $6d, $61, $72, $6b, $0d);
  {$ENDREGION}
  strict private
    FAscent: SmallInt;
    FAvgWidth: SmallInt;
    FCapHeight: SmallInt;
    FDescent: SmallInt;
    FFont: IdxPDFFont;
    FFontWeight: SmallInt;
    FHeight: SmallInt;
    FMaxWidth: SmallInt;
    FPitchAndFamily: Byte;
    FTop: SmallInt;
    FXHeight: SmallInt;

    FPfbStream: TFileStream;
    FPfbWriter: TcxWriter;
    FPfmStream: TFileStream;
    FPfmWriter: TcxWriter;

    function CreateStream(const AFileName: string): TFileStream;
    function ConvertToShort(AValue: Double): SmallInt;
    procedure WritePlainText(const AData: TBytes; APlainTextLength, ACipherTextLength: Integer);
    procedure WriteNullSegment(const AData: TBytes; ANullSegmentLength, APlainTextLength, ACipherTextLength: Integer);
  protected
    function CreateFiles: string;
    procedure WriteByte(AValue: Byte);
    procedure WriteInt(AWriter: TcxWriter; AValue: Integer);
    procedure WriteShort(AValue: SmallInt);
    procedure WriteString(const AValue: string);
  public
    constructor Create(const AFont: IdxPDFFont; const APfmFileName, APfbFileName: string);
    destructor Destroy; override;

    class function CreateType1FontFiles(const AFont: IdxPDFFont; const APfmFileName, APfbFileName: string): string; static;
  end;

  { TdxPDFEditableFontDataFactory }

  TdxPDFEditableFontDataFactory = class
  public
    class function Create(AFont: TdxGPFont): TdxPDFEditableFontData; static;
    class function CreateFontFile(var AFont: TdxGPFont): TdxFontFile; static;
    class function ShouldEmulateBold(AFont: TdxGPFont; ABoldFontFile: TdxFontFile): Boolean; static;
  end;

  { TdxFontFamilyInfo }

  TdxFontFamilyInfo = class
  strict private
    FAdditionalStyles: TDictionary<string, string>;
    FSystemFontName: string;
  public
    constructor Create; overload;
    constructor Create(const ASystemFontName: string); overload;
    destructor Destroy; override;

    property AdditionalStyles: TDictionary<string, string> read FAdditionalStyles;
    property SystemFontName: string read FSystemFontName;
  end;

  { TdxFontFamilyInfos }

  TdxFontFamilyInfos = class
  strict private
    FAdditionalStylePattern: TArray<string>;
    FInstalledFontCollection: TdxGPInstalledFontCollection;
    FInfos: TObjectDictionary<string, TdxFontFamilyInfo>;
    FFamilies: TStringList;
    FSegoeUIPresent: Boolean;
    FStylePattern: TArray<string>;
    function GetFontStyle(const AFontName: string): string;
    function Normalize(const AName: string): string;
    function MatchPattern(const S: string; const APatternArray: TArray<string>): string;
    function RemovePattern(const S: string; const APatternArray: TArray<string>): string;
    procedure AddFamilyIfNotExists(const AKey, AValue: string);
    procedure PopulateInfos;
  public
    constructor Create;
    destructor Destroy; override;

    function ContainsBoldStyle(const AFontStyle: string): Boolean;
    function ContainsItalicStyle(const AFontStyle: string): Boolean;
    function ExtractAdditionalStyles(const AActualStyle: string): TArray<string>;
    function GetFontFamily(const AFontName: string): string;
    function GetNormalizedFontFamily(const AFontName: string): string;
    function GetNormalizedFontStyle(const AFontName: string): string;
    function TryGetValue(const AFamilyName: string; out AInfo: TdxFontFamilyInfo): Boolean;

    function Contains(const AFamily: string): Boolean;
  end;

var
  dxgFontFamilyInfos: TdxFontFamilyInfos;

function dxFontFamilyInfos: TdxFontFamilyInfos;
begin
  if dxgFontFamilyInfos = nil then
    dxgFontFamilyInfos := TdxFontFamilyInfos.Create;
  Result := dxgFontFamilyInfos;
end;

{ TdxPDFType1FontFileCreator }

constructor TdxPDFType1FontFileCreator.Create(const AFont: IdxPDFFont; const APfmFileName, APfbFileName: string);
var
  AFontDescriptor: TdxPDFFontDescriptor;
  AFontBBox: TdxRectF;
  AFlags: TdxFontFileFlags;
begin
  inherited Create;
  FFont := AFont;
  AFontDescriptor := (AFont.Instance as TdxPDFCustomFont).FontDescriptor;
  if AFontDescriptor = nil then
  begin
    FTop := 0;
    FHeight := 0;
    FFontWeight := 400;
    FPitchAndFamily := 48;
    FAvgWidth := 0;
    FMaxWidth := 0;
    FCapHeight := 0;
    FXHeight := 0;
    FAscent := 0;
    FDescent := 0;
  end
  else
  begin
    AFontBBox := AFontDescriptor.FontBBox;
    FTop := ConvertToShort(AFontBBox.Top);
    FHeight := ConvertToShort(Max(0, AFontBBox.Height - 1000));
    FFontWeight := SmallInt(AFontDescriptor.FontWeight);
    AFlags := AFontDescriptor.Flags;
    if (Integer(AFlags) and Integer(ffSerif)) = Integer(ffSerif) then
      FPitchAndFamily := 16
    else
      FPitchAndFamily := IfThen((Integer(AFlags) and Integer(ffScript)) = Integer(ffScript), 64, 48);
    if (Integer(AFlags) and Integer(ffFixedPitch)) = Integer(ffFixedPitch) then
      FPitchAndFamily := FPitchAndFamily + 1;
    FAvgWidth := ConvertToShort(AFontDescriptor.AvgWidth);
    FMaxWidth := ConvertToShort(AFontDescriptor.MaxWidth);
    FCapHeight := ConvertToShort(AFontDescriptor.CapHeight);
    FXHeight := ConvertToShort(AFontDescriptor.XHeight);
    FAscent := ConvertToShort(AFontDescriptor.Ascent);
    FDescent := ConvertToShort(Abs(AFontDescriptor.Descent));
  end;
  FPfmStream := CreateStream(APfmFileName);
  FPfmWriter := TcxWriter.Create(FPfmStream);
  FPfbStream := CreateStream(APfbFileName);
  FPfbWriter := TcxWriter.Create(FPfbStream);
end;

destructor TdxPDFType1FontFileCreator.Destroy;
begin
  FreeAndNil(FPfbWriter);
  FreeAndNil(FPfbStream);
  FreeAndNil(FPfmWriter);
  FreeAndNil(FPfmStream);
  inherited Destroy;
end;

function TdxPDFType1FontFileCreator.CreateStream(const AFileName: string): TFileStream;
begin
  Result := TFileStream.Create(AFileName, fmCreate);
end;

function TdxPDFType1FontFileCreator.ConvertToShort(AValue: Double): SmallInt;
begin
  Result := SmallInt(Ceil(AValue));
end;

procedure TdxPDFType1FontFileCreator.WritePlainText(const AData: TBytes; APlainTextLength, ACipherTextLength: Integer);
var
  ABinaryCipher: TdxPDFType1FontEexecBinaryCipher;
  ACipher: TdxPDFType1FontEexecCipher;
  ACipherData: TBytes;
  ACipherDataLength: Integer;
begin
  if TdxPDFType1FontEexecCipher.IsASCIICipher(AData, APlainTextLength) then
  begin
    ACipher := TdxPDFType1FontEexecCipher.CreateCipher(AData, APlainTextLength, ACipherTextLength);
    try
      ACipherData := ACipher.Decrypt;
    finally
      ACipher.Free;
    end;
    ABinaryCipher := TdxPDFType1FontEexecBinaryCipher.Create(ACipherData);
    try
      ACipherData := ABinaryCipher.Encrypt;
    finally
      ABinaryCipher.Free;
    end;
    ACipherDataLength := Length(ACipherData);
    WriteInt(FPfbWriter, ACipherDataLength);
    FPfbWriter.Stream.WriteBuffer(ACipherData[0], ACipherDataLength);
  end
  else
  begin
    SetLength(ACipherData, ACipherTextLength);
    TdxPDFUtils.CopyData(AData, APlainTextLength, ACipherData, 0, ACipherTextLength);
    WriteInt(FPfbWriter, ACipherTextLength);
    FPfbWriter.Stream.WriteBuffer(ACipherData[0], ACipherTextLength);
  end;
end;

procedure TdxPDFType1FontFileCreator.WriteNullSegment(const AData: TBytes; ANullSegmentLength, APlainTextLength,
  ACipherTextLength: Integer);
var
  ACipherData: TBytes;
begin
  if ANullSegmentLength = 0 then
  begin
    WriteInt(FPfbWriter, Length(DefaultNullSegment));
    FPfbWriter.Stream.Write(DefaultNullSegment[0], Length(DefaultNullSegment));
  end
  else
  begin
    SetLength(ACipherData, ANullSegmentLength);
    TdxPDFUtils.CopyData(AData, APlainTextLength + ACipherTextLength, ACipherData, 0, ANullSegmentLength);
    WriteInt(FPfbWriter, ANullSegmentLength);
    FPfbWriter.Stream.WriteBuffer(ACipherData[0], ANullSegmentLength);
  end;
end;

class function TdxPDFType1FontFileCreator.CreateType1FontFiles(const AFont: IdxPDFFont;
  const APfmFileName, APfbFileName: string): string;
var
  ACreator: TdxPDFType1FontFileCreator;
begin
  ACreator := TdxPDFType1FontFileCreator.Create(AFont, APfmFileName, APfbFileName);
  try
    Result := ACreator.CreateFiles;
  finally
    ACreator.Free;
  end;
end;

procedure TdxPDFType1FontFileCreator.WriteByte(AValue: Byte);
begin
  FPfmWriter.WriteByte(AValue);
end;

procedure TdxPDFType1FontFileCreator.WriteShort(AValue: SmallInt);
begin
  FPfmWriter.WriteByte(AValue and $FF);
  FPfmWriter.WriteByte((AValue and $FF00) shr 8);
end;

procedure TdxPDFType1FontFileCreator.WriteInt(AWriter: TcxWriter; AValue: Integer);
begin
  AWriter.WriteByte(AValue and $FF);
  AWriter.WriteByte((AValue and $FF00) shr 8);
  AWriter.WriteByte((AValue and $FF0000) shr 16);
  AWriter.WriteByte((AValue and $FF000000) shr 24);
end;

procedure TdxPDFType1FontFileCreator.WriteString(const AValue: string);
var
  C: Char;
begin
  for C in AValue do
    FPfmWriter.WriteByte(Byte(C));
  FPfmWriter.WriteByte(Byte(0));
end;

function TdxPDFType1FontFileCreator.CreateFiles: string;
var
  AData: TBytes;
  AFontName: string;
  AFontFileData: TdxPDFType1FontFileData;
  I, APlainTextLength, ACipherTextLength: Integer;
  AOffsetFullyQualifiedName, AOffsetExtentTable: Int64;
begin
  AFontName := FFont.RegistrationName;
  AFontFileData := FFont.Type1FontFileData;
  AData := AFontFileData.ActualData;
  APlainTextLength := AFontFileData.ActualPlainTextLength;
  ACipherTextLength := AFontFileData.CipherTextLength;
  FPfbWriter.WriteByte(Byte($80));
  FPfbWriter.WriteByte(Byte($01));
  WriteInt(FPfbWriter, APlainTextLength);
  FPfbWriter.Stream.WriteBuffer(AData[0], APlainTextLength);
  FPfbWriter.WriteByte(Byte($80));
  FPfbWriter.WriteByte(Byte($02));
  WritePlainText(AData, APlainTextLength, ACipherTextLength);
  FPfbWriter.WriteByte(Byte($80));
  FPfbWriter.WriteByte(Byte($01));
  WriteNullSegment(AData, AFontFileData.NullSegmentLength, APlainTextLength, ACipherTextLength);
  FPfbWriter.WriteByte(Byte($80));
  FPfbWriter.WriteByte(Byte($03));
  WriteShort(256);
  WriteInt(FPfmWriter, 0);
  for I := 0 to 60 - 1 do
    WriteByte(0);
  WriteShort(129);
  WriteShort(10);
  WriteShort(300);
  WriteShort(300);
  WriteShort(FTop);
  WriteShort(FHeight);
  WriteShort(196);
  WriteByte(0);
  WriteByte(0);
  WriteByte(0);
  WriteShort(FFontWeight);
  WriteByte(0);
  WriteShort(0);
  WriteShort(1000);
  WriteByte(FPitchAndFamily);
  WriteShort(FAvgWidth);
  WriteShort(FMaxWidth);
  WriteByte(32);
  WriteByte(32);
  WriteByte(0);
  WriteByte(0);
  WriteShort(0);
  WriteInt(FPfmWriter, 199);
  WriteInt(FPfmWriter, 210);
  WriteInt(FPfmWriter, 0);
  WriteInt(FPfmWriter, 0);
  WriteShort(30);
  WriteInt(FPfmWriter, 147);
  WriteInt(FPfmWriter, 0);
  WriteInt(FPfmWriter, 0);
  WriteInt(FPfmWriter, 0);
  WriteInt(FPfmWriter, 0);
  WriteInt(FPfmWriter, 0);
  WriteInt(FPfmWriter, 0);
  WriteShort(52);
  WriteShort(240);
  WriteShort(0);
  WriteShort(1000);
  WriteShort(3);
  WriteShort(1000);
  WriteShort(1000);
  WriteShort(FCapHeight);
  WriteShort(FXHeight);
  WriteShort(FAscent);
  WriteShort(FDescent);
  WriteShort(0);
  WriteShort(-500);
  WriteShort(250);
  WriteShort(500);
  WriteShort(500);
  WriteShort(0);
  WriteShort(0);
  WriteShort(0);
  WriteShort(0);
  WriteShort(0);
  WriteShort(0);
  WriteShort(405);
  WriteShort(50);
  WriteShort(0);
  WriteShort(0);
  WriteString('PostScript');
  WriteString(AFontName);
  AOffsetFullyQualifiedName := FPfmWriter.Stream.Position;
  WriteString(AFontName);
  AOffsetExtentTable := FPfmStream.Position;
  WriteShort(0);
  FPfmWriter.Stream.Position := 2;
  WriteInt(FPfmWriter, FPfmStream.Size);
  FPfmWriter.Stream.Position := 123;
  WriteInt(FPfmWriter, Integer(AOffsetExtentTable));
  FPfmWriter.Stream.Position := 139;
  WriteInt(FPfmWriter, Integer(AOffsetFullyQualifiedName));
  Result := AFontName;
end;

{ TdxPDFEditableFontDataFactory }

class function TdxPDFEditableFontDataFactory.CreateFontFile(var AFont: TdxGPFont): TdxFontFile;

  function dxPDFGetFontData(AStream: TMemoryStream; AFont: TFont): Boolean;
  var
    AFontDataLength: Cardinal;
    AOldFont: HFONT;
    DC: HDC;
  begin
    DC := GetDC(0);
    AOldFont := SelectObject(DC, AFont.Handle);
    AFontDataLength := GetFontData(DC, 0, 0, nil, 0);
    Result := AFontDataLength <> GDI_ERROR;
    if Result and Assigned(AStream) then
    begin
      AStream.Size := AFontDataLength;
      GetFontData(DC, 0, 0, AStream.Memory, AStream.Size);
    end;
    SelectObject(DC, AOldFont);
    ReleaseDC(0, DC);
  end;

var
  ABytesStream: TBytesStream;
  AGDIFont: TFont;
begin
  if AFont.FontFamily.Name = 'Symbol' then
  begin
    if AFont <> nil then
      FreeAndNil(AFont);
    AFont := TdxGPFont.Create('Symbol', 9, TdxGPFontStyle(AFont.Style));
  end;
  ABytesStream := TBytesStream.Create;
  try
    AGDIFont := TFont.Create;
    try
      AGDIFont.Handle := AFont.ToHfont;
      dxPDFGetFontData(ABytesStream, AGDIFont);
      Result := TdxFontFile.Create(ABytesStream.Bytes, False);
    finally
      AGDIFont.Free;
    end;
  finally
    ABytesStream.Free;
  end;
end;

class function TdxPDFEditableFontDataFactory.Create(AFont: TdxGPFont): TdxPDFEditableFontData;
var
  AFontFile: TdxFontFile;
  AFontFacade: TdxPdfFontFileDataFacade;
begin
  AFontFile := CreateFontFile(AFont);

  AFontFacade := TdxPdfFontFileDataFacade.Create(AFont.FontFamily.Name, TdxGPFontStyle(AFont.Style),
    AFontFile, ShouldEmulateBold(AFont, AFontFile));

  Result := TdxPDFEditableFontData.Create(
    TdxPdfEmbeddedEditableFontObject.Create(AFontFacade, not AFontFile.IsTrueTypeFont),
    AFontFacade, TdxPDFEmbeddedGlyphMapper.Create(AFontFile));
end;

class function TdxPDFEditableFontDataFactory.ShouldEmulateBold(AFont: TdxGPFont; ABoldFontFile: TdxFontFile): Boolean;
var
  AActualFontFile: TdxFontFile;
  ARegularFont: TdxGPFont;
begin
  Result := AFont.Bold and AFont.FontFamily.IsStyleAvailable(Integer(TdxGPFontStyle.FontStyleRegular));
  if Result then
  begin
    ARegularFont := TdxGPFont.Create(AFont, TdxGPFontStyle.FontStyleRegular);
    try
      AActualFontFile := CreateFontFile(ARegularFont);
      try
        Result := TdxFontFile.IsEqual(AActualFontFile, ABoldFontFile);
      finally
        AActualFontFile.Free;
      end;
    finally
      ARegularFont.Free;
    end;
  end;
end;

{ TdxFontFamilyInfo }

constructor TdxFontFamilyInfo.Create;
begin
  inherited Create;
  FAdditionalStyles := TDictionary<string, string>.Create;
end;

constructor TdxFontFamilyInfo.Create(const ASystemFontName: string);
begin
  Create;
  FSystemFontName := ASystemFontName;
end;

destructor TdxFontFamilyInfo.Destroy;
begin
  FreeAndNil(FAdditionalStyles);
  inherited Destroy;
end;

{ TdxFontFamilyInfos }

constructor TdxFontFamilyInfos.Create;
begin
  inherited Create;
  FFamilies := nil;
  FInstalledFontCollection := TdxGPInstalledFontCollection.Create;
  FInfos := TObjectDictionary<string, TdxFontFamilyInfo>.Create([doOwnsValues]);
  FStylePattern := TdxStringHelper.Split('semibold|semilight|demi|light|black|bold|italic|oblique|md|sb|bd|it|scn|mt', ['|']);
  FAdditionalStylePattern := TdxStringHelper.Split('semibold|semilight|demibold|demi|light|black|md|bd|italic|sb|scn', ['|']);
  PopulateInfos;
end;

destructor TdxFontFamilyInfos.Destroy;
begin
  FreeAndNil(FInfos);
  FreeAndNil(FInstalledFontCollection);
  FreeAndNil(FFamilies);
  inherited Destroy;
end;

function TdxFontFamilyInfos.Contains(const AFamily: string): Boolean;
var
  AGPFamily: TdxGPFontFamily;
begin
  if FFamilies = nil then
  begin
    FFamilies := TStringList.Create;
    for AGPFamily in FInstalledFontCollection.Families do
      FFamilies.Add(AGPFamily.Name);
  end;
  Result := FFamilies.IndexOf(AFamily) > -1;
end;

function TdxFontFamilyInfos.Normalize(const AName: string): string;
begin
  Result := StringReplace(AName, ' ', '', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
  Result := LowerCase(StringReplace(Result, ',', '', [rfReplaceAll]));
end;

function TdxFontFamilyInfos.MatchPattern(const S: string; const APatternArray: TArray<string>): string;
var
  ATemp, APattern: string;
begin
  Result := '';
  ATemp := LowerCase(S);
  for APattern in APatternArray do
    if Pos(APattern, ATemp) > 0 then
    begin
      ATemp := StringReplace(ATemp, APattern, '', [rfReplaceAll]);
      Result := Result + APattern;
    end;
end;

function TdxFontFamilyInfos.RemovePattern(const S: string; const APatternArray: TArray<string>): string;
var
  APattern: string;
begin
  Result := S;
  for APattern in APatternArray do
    Result := StringReplace(Result, APattern, '', [rfReplaceAll, rfIgnoreCase]);
end;

function TdxFontFamilyInfos.GetFontFamily(const AFontName: string): string;
begin
  Result := RemovePattern(AFontName, FStylePattern);
end;

function TdxFontFamilyInfos.GetFontStyle(const AFontName: string): string;
begin
  Result := MatchPattern(AFontName, FStylePattern);
end;

function TdxFontFamilyInfos.ContainsBoldStyle(const AFontStyle: string): Boolean;
begin
  Result := (AFontStyle <> '') and (MatchPattern(AFontStyle, TdxStringHelper.Split('demibold|demi|black|bold|bd', ['|'])) <> '');
end;

function TdxFontFamilyInfos.ContainsItalicStyle(const AFontStyle: string): Boolean;
begin
  Result := (AFontStyle <> '') and (MatchPattern(AFontStyle, TdxStringHelper.Split('italic|oblique|it', ['|'])) <> '');
end;

function TdxFontFamilyInfos.ExtractAdditionalStyles(const AActualStyle: string): TArray<string>;
var
  ATemp, APattern, AResult: string;
begin
  ATemp := LowerCase(AActualStyle);
  for APattern in FAdditionalStylePattern do
    if Pos(APattern, ATemp) > 0 then
    begin
      ATemp := StringReplace(ATemp, APattern, '', [rfReplaceAll]);
      AResult := AResult + '|' + APattern;
    end;
  Result := TdxStringHelper.Split(AResult, ['|']);
end;

function TdxFontFamilyInfos.GetNormalizedFontFamily(const AFontName: string): string;
begin
  Result := Normalize(GetFontFamily(AFontName));
end;

function TdxFontFamilyInfos.GetNormalizedFontStyle(const AFontName: string): string;
begin
  Result := Normalize(GetFontStyle(AFontName));
end;

function TdxFontFamilyInfos.TryGetValue(const AFamilyName: string; out AInfo: TdxFontFamilyInfo): Boolean;
begin
  Result := FInfos.TryGetValue(AFamilyName, AInfo);
end;

procedure TdxFontFamilyInfos.AddFamilyIfNotExists(const AKey, AValue: string);
var
  ANormalized: string;
begin
  ANormalized := GetNormalizedFontFamily(AKey);
  if not FInfos.ContainsKey(ANormalized) then
    FInfos.Add(ANormalized, TdxFontFamilyInfo.Create(AValue));
end;

procedure TdxFontFamilyInfos.PopulateInfos;
var
  AGPFamily: TdxGPFontFamily;
  AFamilyName, AActualFamily, AActualStyle: string;
  AInfo: TdxFontFamilyInfo;
begin
  for AGPFamily in FInstalledFontCollection.Families do
  begin
    AFamilyName := AGPFamily.Name;
    AActualFamily := GetNormalizedFontFamily(AFamilyName);
    AActualStyle := GetNormalizedFontStyle(AFamilyName);
    if not FInfos.TryGetValue(AActualFamily, AInfo) then
    begin
      AInfo := TdxFontFamilyInfo.Create(AActualFamily);
      FInfos.Add(AActualFamily, AInfo);
    end;
    if AActualStyle <> '' then
      AInfo.AdditionalStyles.AddOrSetValue(AActualStyle, AFamilyName);
    if TdxStringHelper.Contains(AFamilyName, 'Segoe UI') then
      FSegoeUIPresent := True;
  end;
  AddFamilyIfNotExists(TdxPDFKeywords.CourierFontName, TdxPDFKeywords.CourierNewFontName2);
  AddFamilyIfNotExists(TdxPDFKeywords.CourierNewFontName, TdxPDFKeywords.CourierNewFontName2);
  AddFamilyIfNotExists('CourierNewPS', TdxPDFKeywords.CourierNewFontName2);
  AddFamilyIfNotExists('CourierNewPS' + TdxPDFKeywords.FontMTSuffix, TdxPDFKeywords.CourierNewFontName2);

  AddFamilyIfNotExists(TdxPDFKeywords.TimesRomanFontName, TdxPDFKeywords.TimesNewRomanFontName2);
  AddFamilyIfNotExists('Times', TdxPDFKeywords.TimesNewRomanFontName2);
  AddFamilyIfNotExists('TimesNewRomanPS', TdxPDFKeywords.TimesNewRomanFontName2);
  AddFamilyIfNotExists('TimesNewRomanPS' + TdxPDFKeywords.FontMTSuffix, TdxPDFKeywords.TimesNewRomanFontName2);

  AddFamilyIfNotExists(TdxPDFKeywords.ArialFontName + TdxPDFKeywords.FontMTSuffix, TdxPDFKeywords.ArialFontName);

  AddFamilyIfNotExists('TallPaul', 'Gabriola');
  AddFamilyIfNotExists('CenturyGothic', 'Century Gothic');
  AddFamilyIfNotExists('GothicText', 'MS Gothic');
  AddFamilyIfNotExists('Flama', 'Tahoma');
  AddFamilyIfNotExists('FlamaBook', 'Tahoma');

  AddFamilyIfNotExists(TdxPDFKeywords.HelveticaFontName, TdxPDFKeywords.ArialFontName);

  AFamilyName := Normalize(TdxPDFKeywords.SymbolFontName);
  if FSegoeUIPresent then
    FInfos.AddOrSetValue(AFamilyName, TdxFontFamilyInfo.Create('Segoe UI'))
  else
    FInfos.AddOrSetValue(AFamilyName, TdxFontFamilyInfo.Create(TdxPDFKeywords.ArialUnicodeMS));
  AddFamilyIfNotExists(TdxPDFKeywords.ZapfDingbatsFontName, TdxPDFKeywords.ArialUnicodeMS);
end;

{ TdxPDFFontCustomRegistrator }

constructor TdxPDFFontCustomRegistrator.Create(const AFont: IdxPDFFont);
begin
  inherited Create;
  FFont := AFont;
end;

destructor TdxPDFFontCustomRegistrator.Destroy;
begin
  Unregister;
  inherited Destroy;
end;

function TdxPDFFontCustomRegistrator.CreateSubstituteFontData: TdxPDFFontRegistrationData;
begin
  Result := TdxPDFFontRegistrationData.Create(FFont.FontName, 0, FFont.Weight, FFont.Italic, FFont.PitchAndFamily,
    False, nil, False);
end;

class function TdxPDFFontCustomRegistrator.CreateRegistrator(const AFont: IdxPDFFont;
  const AFolderName: string): TdxPDFFontCustomRegistrator;
begin
  if AFont.TrueTypeFontFileData <> nil then
    Result := TdxPDFTrueTypeFontRegistrator.Create(AFont)
  else
    if AFont.Type1FontFileData <> nil then
    begin
      if not DirectoryExists(AFolderName) then
        if not ForceDirectories(AFolderName) then
          TdxPDFUtils.RaiseException('Cannot create temp directory');
      Result := TdxPDFType1FontRegistrator.Create(AFont, AFolderName)
    end
    else
      if AFont.ActualCompactFontFileData <> nil then
        Result := TdxPDFOpenTypeFontRegistrator.Create(AFont)
      else
        if (AFont.Instance is TdxPDFTrueTypeFont) or (AFont.Instance is TdxPDFCIDType2Font) then
          Result := TdxPDFTrueTypeFontRegistrator.Create(AFont)
        else
          Result := TdxPDFOpenTypeFontRegistrator.Create(AFont)
end;

function TdxPDFFontCustomRegistrator.Register: TdxPDFFontRegistrationData;
var
  AFontName: string;
begin
  AFontName := PerformRegister;
  if AFontName = '' then
    Result := CreateSubstituteFontData
  else
    Result := TdxPDFFontRegistrationData.Create(AFontName, 0, FFont.Weight, False, DEFAULT_PITCH or FF_DONTCARE,
      True, Self, False);
end;

{ TdxPDFFontInMemoryRegistrator }

constructor TdxPDFFontInMemoryRegistrator.Create(const AFont: IdxPDFFont; const AFontFileData: TBytes);
begin
  inherited Create(AFont);
  FFontFileData := AFontFileData;
end;

function TdxPDFFontInMemoryRegistrator.GetSubstituteFontParameters(AFont: TdxPDFCustomFont): TdxPDFFontRegistratorParameters;
var
  AFontName, AActualFamily, AActualStyle, AActualName, AAdditionalStyle: string;
  AFontDescriptor: TdxPdfFontDescriptor;
  AFontWeight: Integer;
  AWidthFactor: Double;
  AAdditionalStyleFound: Boolean;
  AInfo: TdxFontFamilyInfo;
begin
  AFontName := AFont.Name;
  AActualFamily := dxFontFamilyInfos.GetNormalizedFontFamily(AFontName);
  AActualStyle := dxFontFamilyInfos.GetNormalizedFontStyle(AFontName);
  AFontDescriptor := AFont.FontDescriptor;
  if AFontDescriptor <> nil then
    AFontWeight := AFontDescriptor.FontWeight
  else
    AFontWeight := TdxPDFKeywords.FontDescriptorWeightNormal;
  AActualName := AFontName;
  AWidthFactor := 0;
  AAdditionalStyleFound := False;
  if dxFontFamilyInfos.TryGetValue(AActualFamily, AInfo) then
  begin
    if ShouldSubstituteFont(AActualFamily) then
    begin
      for AAdditionalStyle in dxFontFamilyInfos.ExtractAdditionalStyles(AActualStyle) do
        if AInfo.AdditionalStyles.TryGetValue(AAdditionalStyle, AActualName) then
        begin
          AAdditionalStyleFound := True;
          Break;
        end;
      if not AAdditionalStyleFound then
        AActualName := AInfo.SystemFontName;
    end
    else
      AActualName := dxFontFamilyInfos.GetFontFamily(AFontName);
  end
  else
  begin
    AActualName := dxFontFamilyInfos.GetFontFamily(AFontName);
    AWidthFactor := AFont.WidthToHeightFactor;
  end;

  if not AAdditionalStyleFound and dxFontFamilyInfos.ContainsBoldStyle(AActualStyle) or
    (AActualStyle <> '') and (AFontDescriptor <> nil) and
    (TdxFontFileFlags(Integer(AFontDescriptor.Flags) and Integer(ffForceBold)) = ffForceBold) then
    AFontWeight := 700;

  Result.Name := AActualName;
  Result.Weight := AFontWeight;
  Result.WidthFactor := AWidthFactor;
  if AAdditionalStyleFound then
    Result.IsItalic := False
  else
    Result.IsItalic := dxFontFamilyInfos.ContainsItalicStyle(AActualStyle);
end;

function TdxPDFFontInMemoryRegistrator.PerformRegister: string;
begin
  if CreateHandle then
    Result:= Font.RegistrationName
  else
    Result := '';
end;

procedure TdxPDFFontInMemoryRegistrator.Unregister;
begin
  if HandleAllocated then
    if not RemoveFontMemResourceEx(FFontHandle) then
      TdxPDFUtils.RaiseTestException('TdxPDFFontInMemoryRegistrator.Unregister')
    else
      FFontHandle := 0;
end;

function TdxPDFFontInMemoryRegistrator.ShouldSubstituteFont(const ANormalizedFamilyName: string): Boolean;
begin
  Result := True;
end;

function TdxPDFFontInMemoryRegistrator.CreateHandle: Boolean;
var
  ACount: DWORD;
begin
  Result := Length(FFontFileData) > 0;
  if Result then
  begin
    FFontHandle := AddFontMemResourceEx(FFontFileData, Length(FFontFileData), nil, @ACount);
    Result := HandleAllocated;
  end;
end;

function TdxPDFFontInMemoryRegistrator.HandleAllocated: Boolean;
begin
  Result:= FFontHandle <> 0;
end;

{ TdxPDFOpenTypeFontCreator }

class function TdxPDFOpenTypeFontCreator.GetFontFileData(const AFont: IdxPDFFont): TBytes;
var
  AFile: TdxFontFile;
begin
  if Length(AFont.GetCompactFontFileData) > 0 then
  begin
    AFile := TdxFontFileHelper.CreateOpenTypeFile(AFont);
    try
      Result := AFile.GetData;
    finally
      AFile.Free;
    end;
  end;
end;

{ TdxPDFTrueTypeFontRegistrator }

constructor TdxPDFTrueTypeFontRegistrator.Create(const AFont: IdxPDFFont);
begin
  inherited Create(AFont, AFont.TrueTypeFontFileData);
end;

function TdxPDFTrueTypeFontRegistrator.CreateSubstituteFontData: TdxPDFFontRegistrationData;
var
  AName: string;
  AFontParameters: TdxPDFFontRegistratorParameters;
  APitchAndFamily: Byte;
begin
  AFontParameters := GetSubstituteFontParameters(Font.Instance as TdxPDFCustomFont);
  AName := AFontParameters.Name;
  APitchAndFamily := Font.PitchAndFamily;
  if (AName = 'LucidaConsole') or (AName = 'Lucida Console') then
  begin
    APitchAndFamily := APitchAndFamily and not VARIABLE_PITCH;
    APitchAndFamily := APitchAndFamily or FIXED_PITCH;
  end;
  Result := TdxPDFFontRegistrationData.Create(AName, AFontParameters.WidthFactor, AFontParameters.Weight,
    AFontParameters.IsItalic, APitchAndFamily, False, nil, False);
end;

function TdxPDFTrueTypeFontRegistrator.ShouldSubstituteFont(const ANormalizedFamilyName: string): Boolean;
begin
  Result := ANormalizedFamilyName <> 'symbol';
end;

{ TdxPDFOpenTypeFontRegistrator }

constructor TdxPDFOpenTypeFontRegistrator.Create(const AFont: IdxPDFFont);
begin
  inherited Create(AFont, TdxPDFOpenTypeFontCreator.GetFontFileData(AFont));
end;

function TdxPDFOpenTypeFontRegistrator.CreateSubstituteFontData: TdxPDFFontRegistrationData;
var
  AParameters: TdxPDFFontRegistratorParameters;
begin
  AParameters := GetSubstituteFontParameters(Font.Instance as TdxPDFCustomFont);
  Result := TdxPDFFontRegistrationData.Create(AParameters.Name, AParameters.WidthFactor, AParameters.Weight, AParameters.IsItalic,
    Font.PitchAndFamily, False, nil, False);
end;

{ TdxPDFType1FontRegistrator }

constructor TdxPDFType1FontRegistrator.Create(const AFont: IdxPDFFont; const AFontFolderName: string);
var
  AFileName: string;
begin
  inherited Create(AFont);
  AFileName := AFontFolderName + dxGenerateID;
  FPfmFileName := AFileName + '.pfm';
  FPfbFileName := AFileName + '.pfb';
  FFontResourceName := FPfmFileName + '|' + FPfbFileName;
  FFontName := TdxPDFType1FontFileCreator.CreateType1FontFiles(AFont, FPfmFileName, FPfbFileName);
end;

destructor TdxPDFType1FontRegistrator.Destroy;
begin
  Unregister;
  DeleteFontFile(FPfmFileName);
  DeleteFontFile(FPfbFileName);
  inherited Destroy;
end;

class procedure TdxPDFType1FontRegistrator.DeleteFontFile(const AFileName: string);
begin
  try
    DeleteFile(PWideChar(AFileName));
  except
  end;
end;

procedure TdxPDFType1FontRegistrator.Unregister;
begin
  RemoveFontResourceEx(PWideChar(FFontResourceName), FR_PRIVATE, nil);
end;

function TdxPDFType1FontRegistrator.PerformRegister: string;
begin
  if AddFontResourceEx(PWideChar(FFontResourceName), FR_PRIVATE, nil) = 0 then
    Result := ''
  else
    Result := FFontName;
end;

{ TdxFontFileHelper }

class function TdxFontFileHelper.CreateOpenTypeFile(const AFont: IdxPDFFont): TdxFontFile;
var
  AFontData: TBytes;
  ACharSet: TDictionary<SmallInt, SmallInt>;
  ACMapTable: TdxFontFileCMapTable;
  AHmtxTable: TdxFontFileHmtxTable;
  AMaxpTable: TdxFontFileMaxpTable;
begin
  SetLength(AFontData, 0);
  Result := TdxFontFile.Create(AFontData, True);
  ACharSet := AFont.Charset;
  ACMapTable := TdxFontFileCMapTable.CreateFromCharset(ACharSet);
  AHmtxTable := TdxFontFileHmtxTable.Create(AFont.GlyphCount);
  AMaxpTable := TdxFontFileMaxpTable.Create(AFont.GlyphCount);
  Result.AddTable(TdxFontFileCFFTable.Create(AFont.GetCompactFontFileData));
  Result.AddTable(CreateOS2Table(AFont));
  Result.AddTable(ACMapTable);
  Result.AddTable(CreateHeadTable(AFont));
  Result.AddTable(CreateHheaTable(AFont));
  Result.AddTable(AHmtxTable);
  Result.AddTable(AMaxpTable);
  Result.AddTable(TdxFontFileNameTable.Create(ACMapTable, AFont.GetRegistrationName));
  Result.AddTable(CreatePostTable(AFont));
end;

class function TdxFontFileHelper.GetCFFData(const AData: TBytes): TBytes;
begin
  Result := TdxFontFile.GetCFFData(AData);
end;

class function TdxFontFileHelper.GetFontFileInfo(const AFont: IdxPDFFont; const AFontData: TBytes): TdxFontFileInfo;
var
  ACMapRecord: TdxFontFileCMapSegmentMappingRecord;
  AFile: TdxFontFile;
begin
  AFile := TdxFontFile.Create(AFontData);
  Result.Data := AFile.GetData;
  try
    ACMapRecord := PatchCMapTable(AFile, AFont, Result.Encoding);
    PatchNameTable(AFile, AFont.GetRegistrationName);
    if AFile.HheaTable <> nil then
      AFile.HheaTable.Validate;
    if (AFont.GetFontDescriptor <> nil) and (AFile.OS2Table <> nil) and (AFile.HeadTable <> nil) then
      PatchOS2Table(AFile, AFont.GetFontDescriptor);
    Result.GlyphMapping := nil;
    Result.GlyphRanges := TList<TdxFontFileCMapGlyphRange>.Create;
    if ACMapRecord <> nil then
    begin
      Result.GlyphRanges.AddRange(ACMapRecord.GlyphRanges);
      if (AFile.PostTable <> nil) and AFile.CMapTable.NeedWrite then
      begin
        Result.GlyphMapping := ACMapRecord.CreateGlyphMapping(AFile.PostTable.GlyphNames);
        Result.GlyphMapping.TrimExcess;
      end;
    end;
    Result.Data := AFile.GetData;
  finally
    AFile.Free;
  end;
end;

class function TdxFontFileHelper.GetValidCFFData(const AData: TBytes): TBytes;
begin
  Result := AData;
  if (Length(AData) <> 0) and (AData[0] <> 1) then
  begin
    Result := GetCFFData(AData);
    if Result = nil then
      Result := AData;
  end;
end;

class function TdxFontFileHelper.CalculateAverageGlyphWidth(AFont: IdxPDFFont): SmallInt;
var
  ASum, AWidth: Double;
  ACount: Integer;
  AWidths: TdxPDFDoubleList;
begin
  ASum := 0.0;
  ACount := 0;
  AWidths := AFont.CreateGlyphWidths;
  try
    for AWidth in AWidths do
      if AWidth <> 0 then
      begin
        ASum := ASum + AWidth;
        Inc(ACount);
      end;
    if ACount = 0 then
      Result := 0
    else
      Result := Ceil(ASum / ACount);
  finally
    AWidths.Free;
  end;
end;

class function TdxFontFileHelper.CreateHeadTable(AFont: IdxPDFFont): TdxFontFileHeadTable;
var
  AFontDescriptor: TdxPDFFontDescriptor;
  AAscent: Double;
  AFontBBox: TdxRectF;
  AData: TBytes;
begin
  SetLength(AData, 0);
  Result := TdxFontFileHeadTable.Create(AData);

  TdxFontFileHeadTableAccess(Result).FVersion := $00010000;
  TdxFontFileHeadTableAccess(Result).FFontRevision := 0;
  TdxFontFileHeadTableAccess(Result).FCheckSumAdjustment := 0;
  TdxFontFileHeadTableAccess(Result).FMagicNumber := $5F0F3CF5;
  TdxFontFileHeadTableAccess(Result).FFlags := TdxFontFileHeadTableFlags(0);
  TdxFontFileHeadTableAccess(Result).FCreated := 19;
  TdxFontFileHeadTableAccess(Result).FModified := TdxFontFileHeadTableAccess(Result).FCreated;
  AFontDescriptor := AFont.GetFontDescriptor;
  if AFontDescriptor <> nil then
  begin
    AAscent := AFontDescriptor.ActualAscent;
    TdxFontFileHeadTableAccess(Result).FUnitsPerEm := Trunc(AAscent + AFontDescriptor.ActualDescent);
    if TdxFontFileHeadTableAccess(Result).FUnitsPerEm < 0 then
      TdxFontFileHeadTableAccess(Result).FUnitsPerEm := Trunc(AAscent)
    else
      if TdxFontFileHeadTableAccess(Result).FUnitsPerEm = 0 then
        TdxFontFileHeadTableAccess(Result).FUnitsPerEm := 2048;
    AFontBBox := AFontDescriptor.FontBBox;
    if AFontBBox <> dxRectF(cxNullRect) then
    begin
      TdxFontFileHeadTableAccess(Result).FXMin := Trunc(AFontBBox.Left);
      TdxFontFileHeadTableAccess(Result).FYMin := Trunc(AFontBBox.Bottom);
      TdxFontFileHeadTableAccess(Result).FXMax := Trunc(AFontBBox.Right);
      TdxFontFileHeadTableAccess(Result).FYMax := Trunc(AFontBBox.Top);
    end;
  end;
  if TdxFontFileHeadTableAccess(Result).FXMin = TdxFontFileHeadTableAccess(Result).FXMax then
    TdxFontFileHeadTableAccess(Result).FXMax := Trunc(TdxFontFileHeadTableAccess(Result).FXMin + CalculateAverageGlyphWidth(AFont));
  TdxFontFileHeadTableAccess(Result).FLowestRecPPEM := 6;
  TdxFontFileHeadTableAccess(Result).Changed;
end;

class function TdxFontFileHelper.CreateHheaTable(AFont: IdxPDFFont): TdxFontFileHheaTable;
var
  AItalicAngle, AMinWidth, AMaxWidth, AWidth: Double;
  AFontDescriptor: TdxPDFFontDescriptor;
  AEm: SmallInt;
  AData: TBytes;
  AWidths: TdxPDFDoubleList;
begin
  SetLength(AData, 0);
  Result := TdxFontFileHheaTable.Create(AData);
  TdxFontFileHheaTableAccess(Result).FVersion := $10000;
  AFontDescriptor := AFont.GetFontDescriptor;
  if AFontDescriptor = nil then
  begin
    TdxFontFileHheaTableAccess(Result).FAscender := 0;
    TdxFontFileHheaTableAccess(Result).FDescender := 0;
    AItalicAngle := 0;
  end
  else
  begin
    TdxFontFileHheaTableAccess(Result).FAscender := Trunc(AFontDescriptor.ActualAscent);
    TdxFontFileHheaTableAccess(Result).FDescender := Trunc(AFontDescriptor.ActualDescent);
    AItalicAngle := Abs(AFontDescriptor.ItalicAngle);
  end;
  AEm := Trunc(TdxFontFileHheaTableAccess(Result).FAscender - TdxFontFileHheaTableAccess(Result).FDescender);
  TdxFontFileHheaTableAccess(Result).FLineGap := Trunc(1.2 * AEm);

  AMinWidth := MaxInt;
  AMaxWidth := 0;
  AWidths := AFont.CreateGlyphWidths;
  try
  for AWidth in AWidths do
    if AWidth <> 0 then
    begin
      AMinWidth := Min(AWidth, AMinWidth);
      AMaxWidth := Max(AWidth, AMaxWidth);
    end;
  finally
    AWidths.Free;
  end;
  TdxFontFileHheaTableAccess(Result).FAdvanceWidthMax := Trunc(AMaxWidth);

  TdxFontFileHheaTableAccess(Result).FMinLeftSideBearing := 0;
  TdxFontFileHheaTableAccess(Result).FMinRightSideBearing := 0;
  TdxFontFileHheaTableAccess(Result).FXMaxExtent := Trunc(AMinWidth);
  if AItalicAngle = 0 then
    TdxFontFileHheaTableAccess(Result).FCaretSlopeRise := 1
  else
    TdxFontFileHheaTableAccess(Result).FCaretSlopeRise := Trunc(AEm * Sin((90 - AItalicAngle) * PI / 180));
  TdxFontFileHheaTableAccess(Result).FCaretSlopeRun := Trunc(AEm * Sin(AItalicAngle * Pi / 180));
  TdxFontFileHheaTableAccess(Result).FMetricDataFormat := 0;

  TdxFontFileHheaTableAccess(Result).FNumberOfHMetrics := AFont.GlyphCount;
  TdxFontFileHheaTableAccess(Result).Changed;
end;

class function TdxFontFileHelper.CreateOS2Table(AFont: IdxPDFFont): TdxFontFileOS2Table;
const
  EmptyValue = $00000000;
var
  AFlags: TdxFontFileFlags;
  AIsBold: Boolean;
  AAscender, ADescender, AItalicAngle, AEm: Double;
  AFontDescriptor: TdxPDFFontDescriptor;
  AFirstChar, ALastChar, AWidth: Integer;
  AData: TBytes;
begin
  SetLength(AData, 0);
  Result := TdxFontFileOS2Table.Create(AData);

  TdxFontFileOS2TableAccess(Result).FVersion := TdxFontFileVersion.OpenType_1_5;
  TdxFontFileOS2TableAccess(Result).FAvgCharWidth := CalculateAverageGlyphWidth(AFont);
  AFontDescriptor := AFont.GetFontDescriptor;
  if AFontDescriptor = nil then
  begin
    AFlags := ffNone;
    AIsBold := False;
    TdxFontFileOS2TableAccess(Result).FWeightClass := TdxFontFileOS2Table.NormalFontWeight;
    TdxFontFileOS2TableAccess(Result).FWidthClass := wcMedium;
    AAscender := 0;
    ADescender := 0;
    AItalicAngle := 0;
    TdxFontFileOS2TableAccess(Result).FXHeight := 0;
    TdxFontFileOS2TableAccess(Result).FCapHeight := 0;
  end
  else
  begin
    AFlags := AFontDescriptor.Flags;
    AIsBold := (Integer(AFlags) and Integer(ffForceBold)) = Integer(ffForceBold);
    if AIsBold then
      TdxFontFileOS2TableAccess(Result).FWeightClass := TdxFontFileOS2Table.BoldFontWeight
    else
    begin
      TdxFontFileOS2TableAccess(Result).FWeightClass := SmallInt(AFontDescriptor.FontWeight);
      AIsBold := TdxFontFileOS2TableAccess(Result).FWeightClass = TdxFontFileOS2Table.BoldFontWeight;
    end;
    case AFontDescriptor.FontStretch of
      fsCondensed:
        TdxFontFileOS2TableAccess(Result).FWidthClass := wcCondensed;
      fsExpanded:
        TdxFontFileOS2TableAccess(Result).FWidthClass := wcExpanded;
      fsExtraCondensed:
        TdxFontFileOS2TableAccess(Result).FWidthClass := wcExtraCondensed;
      fsExtraExpanded:
        TdxFontFileOS2TableAccess(Result).FWidthClass := wcExtraExpanded;
      fsSemiCondensed:
        TdxFontFileOS2TableAccess(Result).FWidthClass := wcSemiCondensed;
      fsSemiExpanded:
        TdxFontFileOS2TableAccess(Result).FWidthClass := wcSemiExpanded;
      fsUltraCondensed:
        TdxFontFileOS2TableAccess(Result).FWidthClass := wcUltraCondensed;
      fsUltraExpanded:
        TdxFontFileOS2TableAccess(Result).FWidthClass := wcUltraExpanded;
      else
        TdxFontFileOS2TableAccess(Result).FWidthClass := wcMedium;
    end;
    AAscender := AFontDescriptor.ActualAscent;
    ADescender := AFontDescriptor.ActualDescent;
    AItalicAngle := Abs(AFontDescriptor.ItalicAngle);
    TdxFontFileOS2TableAccess(Result).FXHeight := AFontDescriptor.XHeight;
    TdxFontFileOS2TableAccess(Result).FCapHeight := AFontDescriptor.CapHeight;
  end;
  TdxFontFileOS2TableAccess(Result).FEmbeddingType := TdxFontFileOS2EmbeddingType.PreviewPrintEmbedding;
  AEm := AAscender - ADescender;
  TdxFontFileOS2TableAccess(Result).FSubscriptXSize := Trunc(AEm / 5);
  TdxFontFileOS2TableAccess(Result).FSubscriptYSize := TdxFontFileOS2TableAccess(Result).FSubscriptXSize;
  TdxFontFileOS2TableAccess(Result).FSubscriptXOffset := Trunc(AEm * Sin(AItalicAngle * PI / 180));
  TdxFontFileOS2TableAccess(Result).FSubscriptYOffset := 0;
  TdxFontFileOS2TableAccess(Result).FSuperscriptXSize := TdxFontFileOS2TableAccess(Result).FSubscriptXSize;
  TdxFontFileOS2TableAccess(Result).FSuperscriptYSize := TdxFontFileOS2TableAccess(Result).FSubscriptYSize;
  TdxFontFileOS2TableAccess(Result).FSuperscriptXOffset := TdxFontFileOS2TableAccess(Result).FSubscriptXOffset;
  TdxFontFileOS2TableAccess(Result).FSuperscriptYOffset := Trunc(AAscender * 4 / 5);
  TdxFontFileOS2TableAccess(Result).FStrikeoutSize := Trunc(AEm / 10);
  TdxFontFileOS2TableAccess(Result).FStrikeoutPosition := Trunc(AAscender / 2);
  TdxFontFileOS2TableAccess(Result).FFamilyClass := TdxFontFileOS2FamilyClass.ffcNoClassification;
  TdxFontFileOS2TableAccess(Result).FUnicodeRange1 := TdxFontFileUnicodeRange1(EmptyValue);
  TdxFontFileOS2TableAccess(Result).FUnicodeRange2 := TdxFontFileUnicodeRange2(EmptyValue);
  TdxFontFileOS2TableAccess(Result).FUnicodeRange3 := TdxFontFileUnicodeRange3(EmptyValue);
  TdxFontFileOS2TableAccess(Result).FUnicodeRange4 := TdxFontFileUnicodeRange4(EmptyValue);
  TdxFontFileOS2TableAccess(Result).FVendor := 'DX  ';
  TdxFontFileOS2TableAccess(Result).FSelection := TdxFontFileSelection.Empty;
  if (Integer(AFlags) and Integer(ffItalic)) = Integer(ffItalic) then
    TdxFontFileOS2TableAccess(Result).FSelection := TdxFontFileSelection(Integer(TdxFontFileOS2TableAccess(Result).FSelection) or Integer(TdxFontFileSelection.ITALIC));
  if AIsBold then
    TdxFontFileOS2TableAccess(Result).FSelection := TdxFontFileSelection(Integer(TdxFontFileOS2TableAccess(Result).FSelection) or Integer(TdxFontFileSelection.BOLD));

  AFirstChar := MaxInt;
  ALastChar := -MaxInt - 1;
  for AWidth in AFont.Charset.Keys do
  begin
    AFirstChar := Min(AWidth, AFirstChar);
    ALastChar := Max(AWidth, ALastChar);
  end;
  TdxFontFileOS2TableAccess(Result).FFirstCharIndex := AFirstChar;
  TdxFontFileOS2TableAccess(Result).FLastCharIndex := ALastChar;
  TdxFontFileOS2TableAccess(Result).FTypoAscender := Trunc(AAscender);
  TdxFontFileOS2TableAccess(Result).FTypoDescender := Trunc(ADescender);
  TdxFontFileOS2TableAccess(Result).FTypoLineGap := Trunc(1.2 * AEm);
  TdxFontFileOS2TableAccess(Result).FWinAscent := TdxFontFileOS2TableAccess(Result).FTypoAscender;
  TdxFontFileOS2TableAccess(Result).FWinDescent := Trunc(Abs(ADescender));
  TdxFontFileOS2TableAccess(Result).FCodePageRange1 := TdxFontFileCodePageRange1.Latin1;
  TdxFontFileOS2TableAccess(Result).FCodePageRange2 := TdxFontFileCodePageRange2(EmptyValue);
  TdxFontFileOS2TableAccess(Result).Changed;
end;

class function TdxFontFileHelper.CreatePostTable(AFont: IdxPDFFont): TdxFontFilePostTable;
var
  AData: TBytes;
  AProportion: Integer;
  ADescriptor: TdxPDFFontDescriptor;
  AFontBBox: TdxRectF;
begin
  SetLength(AData, 0);
  Result := TdxFontFilePostTable.Create(AData);

  ADescriptor := AFont.GetFontDescriptor;
  if ADescriptor = nil then
  begin
    TdxFontFilePostTableAccess(Result).FItalicAngle := 0;
    TdxFontFilePostTableAccess(Result).FUnderlinePosition := 0;
    AProportion := TdxFontFilePostTable.FontIsProportionallySpaced;
  end
  else
  begin
    TdxFontFilePostTableAccess(Result).FItalicAngle := ADescriptor.ItalicAngle;
    AFontBBox := ADescriptor.FontBBox;
    if AFontBBox = dxRectF(cxNullRect) then
      TdxFontFilePostTableAccess(Result).FUnderlinePosition := 0
    else
      TdxFontFilePostTableAccess(Result).FUnderlinePosition := Trunc(AFontBBox.Top / 2);
    if (Integer(ADescriptor.Flags) and Integer(ffFixedPitch)) = Integer(ffFixedPitch) then
      AProportion := TdxFontFilePostTable.FontIsMonospaced
    else
      AProportion := TdxFontFilePostTable.FontIsProportionallySpaced;
  end;
  TdxFontFilePostTableAccess(Result).DataStream.WriteInt(TdxFontFilePostTableAccess(Result).Version);
  TdxFontFilePostTableAccess(Result).DataStream.WriteFixed(TdxFontFilePostTableAccess(Result).FItalicAngle);
  TdxFontFilePostTableAccess(Result).DataStream.WriteShort(TdxFontFilePostTableAccess(Result).FUnderlinePosition);
  TdxFontFilePostTableAccess(Result).DataStream.WriteShort(Trunc(TdxFontFilePostTableAccess(Result).FUnderlinePosition / 5));
  TdxFontFilePostTableAccess(Result).DataStream.WriteInt(AProportion);
  TdxFontFilePostTableAccess(Result).DataStream.WriteInt(TdxFontFilePostTable.MinMemType42);
  TdxFontFilePostTableAccess(Result).DataStream.WriteInt(TdxFontFilePostTable.MaxMemType42);
  TdxFontFilePostTableAccess(Result).DataStream.WriteInt(TdxFontFilePostTable.MinMemType1);
  TdxFontFilePostTableAccess(Result).DataStream.WriteInt(TdxFontFilePostTable.MaxMemType1);
end;

class function TdxFontFileHelper.PatchCMapTable(AFile: TdxFontFile; const AFont: IdxPDFFont;
  var AEncoding: TSmallIntDynArray): TdxFontFileCMapSegmentMappingRecord;

  function IsSymbolicFont: Boolean;
  begin
    Result := (AFont.GetFontDescriptor <> nil) and ((Integer(AFont.GetFontDescriptor.Flags) and Integer(ffSymbolic)) =
      Integer(ffSymbolic));
  end;

var
  AEncodingID: TdxFontFileEncodingID;
  ASkipEncodingValidation: Boolean;
  ASimpleFontEncoding: TdxPDFSimpleFontEncoding;
begin
  if AFile.CMapTable = nil then
  begin
    if IsSymbolicFont then
      AEncodingID := TdxFontFileEncodingID.Undefined
    else
      AEncodingID := TdxFontFileEncodingID.UGL;
    Result := TdxFontFileCMapSegmentMappingRecord.CreateDefault(AEncodingID);
    AFile.AddTable(TdxFontFileCMapTable.Create(Result));
  end
  else
  begin
    ASkipEncodingValidation := False;
    if AFont is TdxPDFTrueTypeFont then
    begin
      ASkipEncodingValidation := False;
      ASimpleFontEncoding := (AFont as TdxPDFTrueTypeFont).Encoding as TdxPDFSimpleFontEncoding;
      if (ASimpleFontEncoding is TdxPDFStandartEncoding) and ((ASimpleFontEncoding.Differences = nil) or
        (ASimpleFontEncoding.Differences <> nil) and (ASimpleFontEncoding.Differences.Count = 0)) then
        AFile.CMapTable.PopulateEncoding(AEncoding);
    end;
    ASkipEncodingValidation := ASkipEncodingValidation or (AFont is TdxPDFCIDType2Font);
    Result := AFile.CMapTable.Validate(ASkipEncodingValidation, IsSymbolicFont);
  end;
end;

class procedure TdxFontFileHelper.PatchNameTable(AFile: TdxFontFile; const ABaseFontName: string);
begin
  if AFile.NameTable = nil then
    AFile.AddTable(TdxFontFileNameTable.Create(AFile.CMapTable, ABaseFontName))
  else
    AFile.NameTable.AddName(AFile.CMapTable, ABaseFontName);
end;

class procedure TdxFontFileHelper.PatchOS2Table(AFile: TdxFontFile; AFontDescriptor: TdxPDFFontDescriptor);
var
  AAscent, ADescent, AWinAscent, AWinDescent: SmallInt;
  AFactor: Double;
begin
  if AFile.HheaTable <> nil then
  begin
    AAscent := AFile.HheaTable.Ascender;
    if AAscent > AFile.OS2Table.WinAscent then
      AFile.OS2Table.WinAscent := AAscent;
    ADescent := -AFile.HheaTable.Descender;
    if ADescent > AFile.OS2Table.WinDescent then
      AFile.OS2Table.WinDescent := ADescent;
  end;
  if (AFile.HeadTable <> nil) and (AFontDescriptor <> nil) and not TdxPDFUtils.IsRectEmpty(AFontDescriptor.FontBBox) then
  begin
    AFactor := AFile.HeadTable.UnitsPerEm / (AFontDescriptor.FontBBox.Top - AFontDescriptor.FontBBox.Bottom);
    AWinAscent := Trunc(AFactor * AFontDescriptor.FontBBox.Top);
    if AWinAscent > AFile.OS2Table.WinAscent then
      AFile.OS2Table.WinAscent := AWinAscent;

    AWinDescent := Trunc(-AFactor * AFontDescriptor.FontBBox.Bottom);
    if AWinDescent > AFile.OS2Table.WinDescent then
      AFile.OS2Table.WinDescent := AWinDescent;
  end;
end;

{ TdxPDFFontDataFacade }

constructor TdxPDFFontDataFacade.Create(const AFontFamily: string; AFontStyle: TdxGPFontStyle;
  AFontMetrics: TdxFontFileFontMetrics);
var
  ANameBuilder: TStringBuilder;
  APostfix: string;
  C: Char;
begin
  inherited Create;
  FFontFamily := AFontFamily;
  FFontStyle := AFontStyle;
  FFontMetrics := AFontMetrics;
  ANameBuilder := TStringBuilder.Create;
  try
    for C in AFontFamily do
      if ((C <> ' ') and (C <> #13)) and (C <> #10) then
        ANameBuilder.Append(C);
    APostfix := '';
    if Bold then
      APostfix := APostfix + TdxPDFKeywords.Bold;
    if Italic then
      APostfix := APostfix + TdxPDFKeywords.Italic;
    if APostfix <> '' then
    begin
      ANameBuilder.Append(',');
      ANameBuilder.Append(APostfix);
    end;
    FFontName := ANameBuilder.ToString;
  finally
    ANameBuilder.Free;
  end;
end;

function TdxPDFFontDataFacade.GetFontFileBasedName: string;
begin
  Result := FFontName;
end;

function TdxPDFFontDataFacade.GetBold: Boolean;
begin
  Result := TdxPDFTextUtils.HasFlag(FFontStyle, TdxGPFontStyle.FontStyleBold)
end;

function TdxPDFFontDataFacade.GetItalic: Boolean;
begin
  Result := TdxPDFTextUtils.HasFlag(FFontStyle, TdxGPFontStyle.FontStyleItalic)
end;

function TdxPDFFontDataFacade.GetUnderline: Boolean;
begin
  Result := TdxPDFTextUtils.HasFlag(FFontStyle, TdxGPFontStyle.FontStyleUnderline);
end;

function TdxPDFFontDataFacade.GetStrikeout: Boolean;
begin
  Result := TdxPDFTextUtils.HasFlag(FFontStyle, TdxGPFontStyle.FontStyleStrikeout);
end;

function TdxPDFFontDataFacade.GetIsSymbolFont: Boolean;
begin
  Result := (FontFileBasedName = 'Symbol') or (FontFileBasedName = 'SymbolMT');
end;

{ TdxPDFFontFileDataFacade }

constructor TdxPDFFontFileDataFacade.Create(const AFontFamily: string; AFontStyle: TdxGPFontStyle;
  AFontFile: TdxFontFile; AEmulateBold: Boolean);
begin
  inherited Create(AFontFamily, AFontStyle, TdxFontFileFontMetrics.Create(AFontFile));
  FFontFile := AFontFile;
  FEmulateBold := AEmulateBold;

  FEmulateItalic := Italic and (AFontFile.PostTable <> nil) and (Abs(AFontFile.PostTable.ItalicAngle) < MinDouble);
  FFontFileBasedName := FontName;
  if (AFontFile.NameTable <> nil) and (AFontFile.NameTable.MacFamilyName <> '') and
    (AFontFile.NameTable.MacFamilyName <> AFontFile.NameTable.FamilyName) and (AFontFile.NameTable.PostScriptName <> '') then
    FFontFileBasedName := AFontFile.NameTable.PostScriptName;
end;

destructor TdxPDFFontFileDataFacade.Destroy;
begin
  FreeAndNil(FFontFile);
  inherited Destroy;
end;

function TdxPDFFontFileDataFacade.GetFontFileBasedName: string;
begin
  Result := FFontFileBasedName;
end;

function TdxPDFFontFileDataFacade.GetEmulateBold: Boolean;
begin
  Result := FEmulateBold;
end;

function TdxPDFFontFileDataFacade.GetEmulateItalic: Boolean;
begin
  Result := FEmulateItalic;
end;

function TdxPDFFontFileDataFacade.CreateFontDescriptorData: TdxPDFFontDescriptorData;
var
  APost: TdxFontFilePostTable;
  AMetrics: TdxFontFileFontMetrics;
  AFlags: TdxFontFileFlags;
  AOs2: TdxFontFileOS2Table;
  APanose: TdxFontFilePanose;
  ASerifStyle: TdxFontFilePanoseSerifStyle;
  ANumGlyphs: Integer;
  AMaxp: TdxFontFileMaxpTable;
  ALoca: TdxFontFileLocaTable;
begin
  APost := FFontFile.PostTable;
  AMetrics := Metrics;
  if IsSymbolFont then
    AFlags := TdxFontFileFlags.ffSymbolic
  else
    AFlags := TdxFontFileFlags.ffNonSymbolic;

  AOs2 := FFontFile.OS2Table;
  if AOs2 <> nil then
    APanose := AOs2.Panose;

  if APanose.Proportion = TdxFontFilePanoseProportion.pMonospaced then
    AFlags := TdxFontFileFlags(Integer(AFlags) or Integer(TdxFontFileFlags.ffFixedPitch));

  ASerifStyle := APanose.SerifStyle;
  if ((ASerifStyle <> TdxFontFilePanoseSerifStyle.ssNormalSans) and (ASerifStyle <> TdxFontFilePanoseSerifStyle.ssObtuseSans)) and
    (ASerifStyle <> TdxFontFilePanoseSerifStyle.ssPerpendicularSans) then
    AFlags := TdxFontFileFlags(Integer(AFlags) or Integer(TdxFontFileFlags.ffSerif));

  if APanose.FamilyKind = TdxFontFilePanoseFamilyKind.fkLatinHandWritten then
    AFlags := TdxFontFileFlags(Integer(AFlags) or Integer(TdxFontFileFlags.ffScript));

  if Italic then
    AFlags := TdxFontFileFlags(Integer(AFlags) or Integer(TdxFontFileFlags.ffItalic));

  AMaxp := FFontFile.MaxpTable;
  if AMaxp <> nil then
    ANumGlyphs := AMaxp.NumGlyphs
  else
  begin
    ALoca := FFontFile.LocaTable;
    if ALoca <> nil then
      ANumGlyphs := Max(0, Length(ALoca.GlyphOffsets) - 1)
    else
      ANumGlyphs := 0;
  end;
  Result := TdxPDFFontDescriptorData.Create(Metrics, AFlags, APost.ItalicAngle, Bold, ANumGlyphs);
end;

function TdxPDFFontFileDataFacade.CreateFontFileSubset(ASubset: TDictionary<Integer, string>): TBytes;
begin
  Result := FFontFile.CreateSubset(ASubset);
end;

function TdxPDFFontFileDataFacade.GetGlyphWidth(AGlyphIndex: Integer): Double;
begin
  Result := FFontFile.GetCharacterWidth(AGlyphIndex);
end;

{ TdxPDFCharacterSet }

constructor TdxPDFCharacterSet.Create;
begin
  inherited Create;
  FCurrentSet := TDictionary<Integer, string>.Create;
  FHasNewCharacters := True;
end;

destructor TdxPDFCharacterSet.Destroy;
begin
  FreeAndNil(FCurrentSet);
  inherited Destroy;
end;

procedure TdxPDFCharacterSet.AddSubset(AToUnicode: TDictionary<Integer, string>);
var
  ACode: Integer;
  APair: TPair<Integer, string>;
begin
  for APair in AToUnicode do
  begin
    ACode := APair.Key;
    if not FCurrentSet.ContainsKey(ACode) then
    begin
      FCurrentSet.Add(ACode, APair.Value);
      FHasNewCharacters := True;
    end;
  end;
end;

{ TdxPDFEditableFontObject }

constructor TdxPDFEditableFontObject.Create(AShouldUseTwoByteGlyphIndex: Boolean);
begin
  inherited Create;
  FCharSet := TdxPDFCharacterSet.Create;
  FShouldUseTwoByteGlyphIndex := AShouldUseTwoByteGlyphIndex;
end;

destructor TdxPDFEditableFontObject.Destroy;
begin
  FreeAndNil(FCharSet);
  inherited Destroy;
end;

procedure TdxPDFEditableFontObject.UpdateFont;
begin
  if FCharSet.HasNewCharacters then
  begin
    Font.CharacterMapping := TdxPDFCMapStreamParser.Parse(TdxPDFCustomFontAccess(Font).Repository,
      TdxPDFCharacterMapping.CreateCharacterMappingData(FCharSet.CurrentSet, 'DEVEXP'));
    UpdateFontFile(FCharSet.CurrentSet);
    FCharSet.HasNewCharacters := False;
  end;
end;

procedure TdxPDFEditableFontObject.AddSubset(ASubset: TDictionary<Integer, string>);
begin
  FCharSet.AddSubset(ASubset);
end;

{ TdxPDFEmbeddedEditableFontObject }

constructor TdxPDFEmbeddedEditableFontObject.Create(AFontDataFacade: TdxPDFFontDataFacade; AIsUnsupportedFont: Boolean);
begin
  inherited Create(True);
  FFontDataFacade := AFontDataFacade;
  FIsUnsupportedFont := AIsUnsupportedFont;
  Font := TdxPDFDeferredCIDType2Font.Create('DEVEXP+' + AFontDataFacade.FontName,
    TdxPDFFontDescriptor.Create(AFontDataFacade.CreateFontDescriptorData));
end;

destructor TdxPDFEmbeddedEditableFontObject.Destroy;
begin
  Font := nil;
  inherited Destroy;
end;

function TdxPDFEmbeddedEditableFontObject.GetFont: TdxPDFCustomFont;
begin
  Result := FFont;
end;

function TdxPDFEmbeddedEditableFontObject.IsUnsupportedFont: Boolean;
begin
  Result := FIsUnsupportedFont;
end;

procedure TdxPDFEmbeddedEditableFontObject.SetFont(const AValue: TdxPDFCustomFont);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FFont));
end;

procedure TdxPDFEmbeddedEditableFontObject.UpdateFontFile(ASubset: TDictionary<Integer, string>);
var
  AIndex: Integer;
begin
  FFont.FontFileData := FFontDataFacade.CreateFontFileSubset(ASubset);
  FFont.Widths := TDictionary<Integer, Double>.Create;
  for AIndex in ASubset.Keys do
    FFont.Widths.Add(AIndex, FFontDataFacade.GetGlyphWidth(AIndex));
end;

{ TdxPDFNotEmbeddedEditableFontObject }

constructor TdxPDFNotEmbeddedEditableFontObject.Create(AFontDataFacade: TdxPDFFontDataFacade);
var
  ABaseFont: string;
begin
  inherited Create(False);
  FPDFWinAnsiEncodingName := 'WinAnsiEncoding';
  ABaseFont := AFontDataFacade.FontFileBasedName;
  Font := TdxPDFDeferredTrueTypeFont.Create(ABaseFont,
    TdxPDFFontDescriptor.Create(AFontDataFacade.CreateFontDescriptorData),
    TdxPDFSimpleFontEncoding.CreateEncoding(ABaseFont, FPDFWinAnsiEncodingName));
end;

destructor TdxPDFNotEmbeddedEditableFontObject.Destroy;
begin
  Font := nil;
  inherited Destroy;
end;

function TdxPDFNotEmbeddedEditableFontObject.GetFont: TdxPDFCustomFont;
begin
  Result := FFont;
end;

function TdxPDFNotEmbeddedEditableFontObject.IsUnsupportedFont: Boolean;
begin
  Result := False;
end;

procedure TdxPDFNotEmbeddedEditableFontObject.SetFont(const AValue: TdxPDFCustomFont);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FFont));
end;

procedure TdxPDFNotEmbeddedEditableFontObject.AddGlyphToEncoding(const AGlyphName: string; AGlyphIndex: Byte; AWidth: Double);
begin
  (FFont.Encoding as TdxPDFSimpleFontEncoding).Differences.Add(AGlyphIndex, AGlyphName);
  FFont.Widths[AGlyphIndex] := AWidth;
end;

procedure TdxPDFNotEmbeddedEditableFontObject.UpdateFontFile(ASubset: TDictionary<Integer, string>);
begin
// do nothing
end;

{ TdxPDFEditableFontData }

constructor TdxPDFEditableFontData.Create(AFontObject: IdxPDFFontObject; AFontDataFacade: TdxPDFFontDataFacade;
  AMapper: TdxPDFGlyphMapper);
begin
  inherited Create(nil);
  FFontObject := AFontObject;
  FFontDataFacade := AFontDataFacade;
  FMapper := AMapper;
end;

function TdxPDFEditableFontData.CreateGlyphRun: TdxPDFGlyphRun;
begin
  Result := FMapper.CreateGlyphRun;
end;

function TdxPDFEditableFontData.GetTextWidth(const AText: string; AFontSize: Double;
  ATextState: TdxPdfInteractiveFormFieldTextState): Double;
var
  AFactor, ACharWidth: Double;
  ALastIndex, I: Integer;
  C: Char;
begin
  Result := 0;
  if (AFontSize > 0) and (AText <> '') then
  begin
    ALastIndex := Length(AText) - 1;
    AFactor := AFontSize * ATextState.HorizontalScaling / 100000;
    for I := 0 to ALastIndex do
    begin
      C := AText[I + 1];
      ACharWidth := GetCharacterWidth(C) * AFactor;
      if ACharWidth > 0 then
        Result := Result + ACharWidth + ATextState.CharacterSpacing;
      if (C = ' ') and (I < ALastIndex) then
        Result := Result + ATextState.WordSpacing;
    end;
  end;
end;

function TdxPDFEditableFontData.ProcessString(const AStr: string; AFlags: TdxPDFGlyphMappingFlags): TdxPDFGlyphRun;
var
  AGlyphMapping: TdxPDFGlyphMappingResult;
begin
  AGlyphMapping := FMapper.MapString(AStr, AFlags);
  try
    FFontObject.AddSubset(AGlyphMapping.Mapping);
    Result := AGlyphMapping.GlyphRun;
  finally
    AGlyphMapping.Mapping.Free;
  end;
end;

procedure TdxPDFEditableFontData.UpdateFont;
begin
  FFontObject.UpdateFont;
end;

procedure TdxPDFEditableFontData.UpdateFontFile;
begin
  FFontObject.UpdateFont;
end;

procedure TdxPDFEditableFontData.DestroySubClasses;
begin
  FreeAndNil(FMapper);
  FreeAndNil(FFontDataFacade);
  FFontObject := nil;
  inherited DestroySubClasses;
end;

function TdxPDFEditableFontData.GetBold: Boolean;
begin
  Result := HasFlag(TdxGPFontStyle.FontStyleBold);
end;

function TdxPDFEditableFontData.GetFont: TdxPDFCustomFont;
begin
  Result := FFontObject.GetFont;
end;

function TdxPDFEditableFontData.GetItalic: Boolean;
begin
  Result := HasFlag(TdxGPFontStyle.FontStyleItalic);
end;

function TdxPDFEditableFontData.GetMetrics: TdxFontFileFontMetrics;
begin
  Result := FFontDataFacade.Metrics;
end;

function TdxPDFEditableFontData.GetNeedEmulateBold: Boolean;
begin
  Result := FFontDataFacade.EmulateBold;
end;

function TdxPDFEditableFontData.GetNeedEmulateItalic: Boolean;
begin
  Result := FFontDataFacade.EmulateItalic;
end;

function TdxPDFEditableFontData.GetStrikeout: Boolean;
begin
  Result := HasFlag(TdxGPFontStyle.FontStyleStrikeout);
end;

function TdxPDFEditableFontData.GetUnderline: Boolean;
begin
  Result := HasFlag(TdxGPFontStyle.FontStyleUnderline);
end;

function TdxPDFEditableFontData.GetCharacterWidth(ACh: Char): Double;
begin
  Result := FFontDataFacade.GetGlyphWidth(FMapper.GetGlyphIndex(ACh));
end;

function TdxPDFEditableFontData.HasFlag(AFontStyle: TdxGPFontStyle): Boolean;
begin
  Result := (Integer(FFontStyle) and Integer(AFontStyle)) <> 0;
end;

{ TdxPDFEditableFontDataCache }

constructor TdxPDFEditableFontDataCache.Create(ACatalog: TdxPDFCatalog);
begin
  inherited Create;
  FCatalog := ACatalog;
  FFontCache := TObjectDictionary<Integer, TdxPDFEditableFontData>.Create([doOwnsValues]);
end;

destructor TdxPDFEditableFontDataCache.Destroy;
begin
  FreeAndNil(FFontCache);
  inherited Destroy;
end;

procedure TdxPDFEditableFontDataCache.Clear;
begin
  FFontCache.Clear;
end;

procedure TdxPDFEditableFontDataCache.UpdateFonts;
var
  AFontData: TdxPDFEditableFontData;
begin
  for AFontData in FFontCache.Values do
    AFontData.UpdateFont;
end;

function TdxPDFEditableFontDataCache.EmbedFont(const AFontFamily: string): Boolean;
begin
  Result := True;
end;

function TdxPDFEditableFontDataCache.TryGetEditableFontData(const AKey: TCacheKey;
  out AFontData: TdxPDFEditableFontData): Boolean;
begin
  Result := FFontCache.TryGetValue(AKey.GetHash, AFontData);
end;

procedure TdxPDFEditableFontDataCache.CacheEditableFontData(const AKey: TCacheKey;
  AFontData: TdxPDFEditableFontData);
begin
  FFontCache.AddOrSetValue(AKey.GetHash, AFontData);
end;

{ TdxPDFEditableFontDataCache.TCacheKey }

class function TdxPDFEditableFontDataCache.TCacheKey.Create(const AFontFamily: string;
  AStyle: TdxGPFontStyle): TCacheKey;
begin
  Result.FontFamily := AFontFamily;
  Result.Style := AStyle;
end;

function TdxPDFEditableFontDataCache.TCacheKey.GetHash: Integer;

  procedure AddToHash(var AHash: Integer; const AData; ADataSize: Integer);
  begin
    AHash := dxBobJenkinsHash(AData, ADataSize, AHash);
  end;

  procedure AddStringToHash(var AHash: Integer; const AData: AnsiString);
  begin
    AddToHash(AHash, AData[1], Length(AData));
  end;

begin
  Result := 0;
  AddStringToHash(Result, dxStringToAnsiString(FontFamily));
  AddToHash(Result, Style, SizeOf(Style));
end;

{ TdxPDFGDIEditableFontDataCache }

function TdxPDFGDIEditableFontDataCache.GetFontData(const AFontFamily: string;
  AFontStyle: TdxGPFontStyle; ACreateFont: Boolean): TdxPDFEditableFontData;
var
  AFont: TdxGPFont;
  AFontID: TCacheKey;
begin
  Result := nil;
  AFontID := TCacheKey.Create(AFontFamily, AFontStyle);
  if ACreateFont and not TryGetEditableFontData(AFontID, Result) then
  begin
    AFont := TdxGPFont.Create(AFontFamily, 9, AFontStyle);
    try
      Result := TdxPDFEditableFontDataFactory.Create(AFont);
      CacheEditableFontData(AFontID, Result);
    finally
      AFont.Free;
    end;
  end;
end;

function TdxPDFGDIEditableFontDataCache.SearchFontData(const AFontFamilyName: string;
  AFontStyle: TdxGPFontStyle): TdxPDFEditableFontData;
begin
  Result := GetFontData(AFontFamilyName, AFontStyle, dxFontFamilyInfos.Contains(AFontFamilyName));
end;

initialization
  dxgFontFamilyInfos := nil;

finalization
  FreeAndNil(dxgFontFamilyInfos);

end.



