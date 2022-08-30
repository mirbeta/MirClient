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

unit dxEncoding;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Windows, Generics.Defaults, Generics.Collections, Contnrs,
  dxCoreClasses, dxGenerics;

type
  { TdxEncoding }

  TdxEncoding = class
  strict private class var
    FEncodings: TdxList<TEncoding>;
    FCodePagesToEncoding: TDictionary<Cardinal, TEncoding>;
    FCodePageToDisplayName: TDictionary<Cardinal, string>;
    FWebNameToEncoding: TDictionary<string, TEncoding>;
    FCodePageToWebName: TDictionary<Cardinal, string>;
    FEmptyEncoding: TEncoding;
  {$IFNDEF DELPHIXE2}
    FANSIEncoding: TEncoding;
  {$ENDIF}
    class constructor Initialize;
  {$HINTS OFF}
    class destructor Finalize;
  {$HINTS ON}
    class procedure AddEncoding(ACodePage: Integer; const AWebName, ADisplayName: string); overload; static;
    class procedure AddEncoding(AEncoding: TEncoding; const AWebName, ADisplayName: string); overload; static;
    class procedure PopulateEncodings;
    // encodings
    class function GetANSI: TEncoding; static;
    class function GetASCII: TEncoding; static; inline;
    class function GetBigEndianUnicode: TEncoding; static; inline;
    class function GetUnicode: TEncoding; static; inline;
    class function GetUTF7: TEncoding; static; inline;
    class function GetUTF8: TEncoding; static; inline;
    class function GetDefault: TEncoding; static;
    class function GetEncodings: TArray<TEncoding>; static;
    class function GetEmpty: TEncoding; static;
  protected
    class function GetDisplayName(AEncoding: TEncoding): string; static;
    class function GetWebName(AEncoding: TEncoding): string; static;
  public
    class function CharsetFromCodePage(ACodePage: Integer): Integer;
    class function CodePageFromCharset(ACharset: Integer): Integer;
    class function DetectEncoding(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TEncoding; overload;
    class function DetectEncoding(AStream: TStream): TEncoding; overload;
    class function GetEncoding(ACodePage: Cardinal): TEncoding; overload;
    class function GetEncoding(const AWebName: string): TEncoding; overload;
    class function GetEncodingCodePage(AEncoding: TEncoding): Integer;

    class property Encodings: TArray<TEncoding> read GetEncodings;

    class property ANSI: TEncoding read GetANSI;
    class property ASCII: TEncoding read GetASCII;
    class property BigEndianUnicode: TEncoding read GetBigEndianUnicode;
    class property Default: TEncoding read GetDefault;
    class property Empty: TEncoding read GetEmpty;
    class property Unicode: TEncoding read GetUnicode;
    class property UTF7: TEncoding read GetUTF7;
    class property UTF8: TEncoding read GetUTF8;
  end;

  { TEncodingHelper }

  TEncodingHelper = class helper for TEncoding
  private
    function GetWebName: string;
    function GetDisplayName: string;
  public
    function CanBeLosslesslyEncoded(const AValue: string): Boolean;
    property DisplayName: string read GetDisplayName;
    property WebName: string read GetWebName;
  end;



implementation

uses
  Math;

type

  { TdxCharsetAndCodePageTranslator }

  TdxCharsetAndCodePageTranslator = class abstract
  strict private
    class var
      FInstance: TdxCharsetAndCodePageTranslator;
  {$HINTS OFF}
    class destructor Finalize;
  {$HINTS ON}
  strict private
    class function GetInstance: TdxCharsetAndCodePageTranslator; static;
    class function CreateInstance: TdxCharsetAndCodePageTranslator; static;
  public
    class procedure ClearInstance; static;
    function CodePageFromCharset(ACharset: Cardinal): Cardinal; virtual; abstract;
    function CharsetFromCodePage(ACodePage: Cardinal): Cardinal; virtual; abstract;

    class property Instance: TdxCharsetAndCodePageTranslator read GetInstance;
  end;

  { TdxFullTrustCharsetAndCodePageTranslator }

  TdxFullTrustCharsetAndCodePageTranslator = class(TdxCharsetAndCodePageTranslator)
  strict private
    class var
      FCharsetToCodePage: TDictionary<Cardinal, Cardinal>;
      FCodePageToCharset: TDictionary<Cardinal, Cardinal>;
    class constructor Initialize;
  {$HINTS OFF}
    class destructor Finalize;
  {$HINTS ON}
  strict private
    function CodePageFromCharsetCore(ACharset: Cardinal): Cardinal;
    function CharsetFromCodePageCore(ACodePage: Cardinal): Cardinal;
  public
    function CodePageFromCharset(ACharset: Cardinal): Cardinal; override;
    function CharsetFromCodePage(ACodePage: Cardinal): Cardinal; override;
  end;

  TdxCharsetInfoHelper = record helper for TCharsetInfo
  public
    class function Create(ACharset: Cardinal): TCharsetInfo; static;
  end;

  TdxDetectionResult = (
    Detecting,
    Positive,
    Negative
  );

  TdxEncodingDetectorLanguage = (
    ChineseSimplified,
    ChineseTraditional,
    Japanese,
    Korean,
    NonCjk
  );
  TdxEncodingDetectorLanguages = set of TdxEncodingDetectorLanguage;

const
  TdxEncodingDetectorLanguagesAll = [
    TdxEncodingDetectorLanguage.ChineseSimplified,
    TdxEncodingDetectorLanguage.ChineseTraditional,
    TdxEncodingDetectorLanguage.Japanese,
    TdxEncodingDetectorLanguage.Korean,
    TdxEncodingDetectorLanguage.NonCjk];

  TdxEncodingDetectorLanguagesChinese = [
    TdxEncodingDetectorLanguage.ChineseSimplified,
    TdxEncodingDetectorLanguage.ChineseTraditional];

  TdxEncodingDetectorLanguagesCjk = [
    TdxEncodingDetectorLanguage.ChineseSimplified,
    TdxEncodingDetectorLanguage.ChineseTraditional,
    TdxEncodingDetectorLanguage.Japanese,
    TdxEncodingDetectorLanguage.Korean];

  EAStateStart = 0;
  EAStateError = 1;
  EAStateItsMe = 2;

type
  TdxInternalEncodingDetector = class;

  { TdxEncodingDetectorState }

  TdxEncodingDetectorState = class abstract
  strict private
    FDetector: TdxInternalEncodingDetector;
  protected
    property Detector: TdxInternalEncodingDetector read FDetector;
  public
    constructor Create(ADetector: TdxInternalEncodingDetector);
    function AnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): Boolean; virtual; abstract;
    function CalculateResult: TEncoding; virtual; abstract;
  end;

  { TdxInitialEncodingDetectorState }

  TdxInitialEncodingDetectorState = class(TdxEncodingDetectorState)
  public
    function AnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): Boolean; override;
    function CalculateResult: TEncoding; override;
    function TryAnalyseUnicodeSignature(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TEncoding;
  end;

  { TdxPureAsciiEncodingDetectorState }

  TdxPureAsciiEncodingDetectorState = class(TdxEncodingDetectorState)
  public
    function AnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): Boolean; override;
    function CalculateResult: TEncoding; override;
  end;

  { TdxNormalEncodingDetectorState }

  TdxNormalEncodingDetectorState = class(TdxEncodingDetectorState)
  public
    function AnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): Boolean; override;
    function CalculateResult: TEncoding; override;
  end;

  { TdxFinalEncodingDetectorState }

  TdxFinalEncodingDetectorState = class(TdxEncodingDetectorState)
  strict private
    FResult: TEncoding;
  public
    constructor Create(ADetector: TdxInternalEncodingDetector; AResult: TEncoding);
    function AnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): Boolean; override;
    function CalculateResult: TEncoding; override;
  end;

  { TdxEncodingDetectorBase }

  TdxEncodingDetectorBase = class abstract
  public const
    ShortcutThreshold = 0.95;
  strict private
    FCurrentResult: TdxDetectionResult;
  protected
    function GetEncoding: TEncoding; virtual; abstract;
    function ForceAnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult; virtual; abstract;
    function AppendBytes(const ASource: TArray<Byte>; AFrom, ATo: Integer; ATarget: TList<Byte>): Integer;
    function IsUpperAsciiByte(ACurrentByte: Byte): Boolean;
    function IsNonEnglishLetterLowerAsciiByte(ACurrentByte: Byte): Boolean;

    property CurrentResult: TdxDetectionResult read FCurrentResult;
  public
    function GetConfidence: Single; virtual; abstract;
    function AnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult;

    property Encoding: TEncoding read GetEncoding;
  end;

  { TdxGroupDetector }

  TdxGroupDetector = class abstract(TdxEncodingDetectorBase)
  strict private
    FBestMatchDetectorIndex: Integer;
    FActiveDetectorCount: Integer;
    FDetectors: TdxList<TdxEncodingDetectorBase>;
    FIsDetectorDisabled: TArray<Boolean>;
  protected
    procedure CreateDetectors; virtual;
    function GetEncoding: TEncoding; override;
    function AnalyseDataCore(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult; virtual;
    procedure PopulateDetectors; virtual; abstract;

    property Detectors: TdxList<TdxEncodingDetectorBase> read FDetectors;
  public
    constructor Create;
    destructor Destroy; override;
    function GetConfidence: Single; override;
  end;

  { TdxSingleByteCharsetGroupDetector }

  TdxSingleByteCharsetGroupDetector = class(TdxGroupDetector)
  protected
    procedure PopulateDetectors; override;
    function ForceAnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult; override;
  public
    constructor Create;
    function FilterBuffer(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TArray<Byte>;
  end;

  { TdxSingleByteEncodingDetector }

  TdxSingleByteEncodingDetector = class abstract(TdxEncodingDetectorBase)
  public const
    SampleSize = 64;
    EnoughSequencesThreshold = 1024;
    PositiveShortcutThreshold = 0.95;
    NegativeShortcutThreshold = 0.05;
    SymbolCategoryOrder = 250;
    SequenceCategoryCount = 4;
    PositiveCategoryIndex = SequenceCategoryCount - 1;
  strict private
    FLastCharacterOrder: Byte;
    FTotalSequenceCount: Integer;
    FSequenceCounters: TArray<Integer>;
    FTotalCharCount: Integer;
    FFrequentCharCount: Integer;
  protected
    function GetPositiveRatio: Single; virtual; abstract;
    function GetCharacterToOrderMap: TArray<Byte>; virtual; abstract;
    function GetPrecedenceMatrix: TArray<Integer>; virtual; abstract;
    function CalculatePrecedenceMatrixIndex(AOrder: Byte): Integer; virtual;
    function GetPrecedenceValue(AIndex: Integer): Integer; virtual;
    function ForceAnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult; override;
    class function Unpack2Bits(I: Integer; const ABuffer: TArray<Integer>): Integer; static;

    property PositiveRatio: Single read GetPositiveRatio;
    property CharacterToOrderMap: TArray<Byte> read GetCharacterToOrderMap;
    property PrecedenceMatrix: TArray<Integer> read GetPrecedenceMatrix;
  public
    constructor Create;
    function GetConfidence: Single; override;
  end;

  { TdxLatin1EncodingDetector }

  TdxLatin1EncodingDetector = class(TdxEncodingDetectorBase)
  strict private const
  {$REGION 'class consts'}
    FrequentCategoryCount = 4;
    Udf = 0;
    Oth = 1;
    Asc = 2;
    Ass = 3;
    Acv = 4;
    Aco = 5;
    Asv = 6;
    Aso = 7;
    ClassCount = 8;
    Latin1_CharToClass: array[Byte] of Byte = (
      Oth, Oth, Oth, Oth, Oth, Oth, Oth, Oth,
      Oth, Oth, Oth, Oth, Oth, Oth, Oth, Oth,
      Oth, Oth, Oth, Oth, Oth, Oth, Oth, Oth,
      Oth, Oth, Oth, Oth, Oth, Oth, Oth, Oth,
      Oth, Oth, Oth, Oth, Oth, Oth, Oth, Oth,
      Oth, Oth, Oth, Oth, Oth, Oth, Oth, Oth,
      Oth, Oth, Oth, Oth, Oth, Oth, Oth, Oth,
      Oth, Oth, Oth, Oth, Oth, Oth, Oth, Oth,
      Oth, Asc, Asc, Asc, Asc, Asc, Asc, Asc,
      Asc, Asc, Asc, Asc, Asc, Asc, Asc, Asc,
      Asc, Asc, Asc, Asc, Asc, Asc, Asc, Asc,
      Asc, Asc, Asc, Oth, Oth, Oth, Oth, Oth,
      Oth, Ass, Ass, Ass, Ass, Ass, Ass, Ass,
      Ass, Ass, Ass, Ass, Ass, Ass, Ass, Ass,
      Ass, Ass, Ass, Ass, Ass, Ass, Ass, Ass,
      Ass, Ass, Ass, Oth, Oth, Oth, Oth, Oth,
      Oth, Udf, Oth, Aso, Oth, Oth, Oth, Oth,
      Oth, Oth, Aco, Oth, Aco, Udf, Aco, Udf,
      Udf, Oth, Oth, Oth, Oth, Oth, Oth, Oth,
      Oth, Oth, Aso, Oth, Aso, Udf, Aso, Aco,
      Oth, Oth, Oth, Oth, Oth, Oth, Oth, Oth,
      Oth, Oth, Oth, Oth, Oth, Oth, Oth, Oth,
      Oth, Oth, Oth, Oth, Oth, Oth, Oth, Oth,
      Oth, Oth, Oth, Oth, Oth, Oth, Oth, Oth,
      Acv, Acv, Acv, Acv, Acv, Acv, Aco, Aco,
      Acv, Acv, Acv, Acv, Acv, Acv, Acv, Acv,
      Aco, Aco, Acv, Acv, Acv, Acv, Acv, Oth,
      Acv, Acv, Acv, Acv, Acv, Aco, Aco, Aco,
      Asv, Asv, Asv, Asv, Asv, Asv, Aso, Aso,
      Asv, Asv, Asv, Asv, Asv, Asv, Asv, Asv,
      Aso, Aso, Asv, Asv, Asv, Asv, Asv, Oth,
      Asv, Asv, Asv, Asv, Asv, Aso, Aso, Aso);
    Model: array[0..63] of Byte = (
      {       Udf Oth Asc Ass Acv Aco Asv Aso  }
      { Udf }  0,  0,  0,  0,  0,  0,  0,  0,
      { Oth }  0,  3,  3,  3,  3,  3,  3,  3,
      { Asc }  0,  3,  3,  3,  3,  3,  3,  3,
      { Ass }  0,  3,  3,  3,  1,  1,  3,  3,
      { Acv }  0,  3,  3,  3,  1,  2,  1,  2,
      { Aco }  0,  3,  3,  3,  3,  3,  3,  3,
      { Asv }  0,  3,  1,  3,  1,  1,  1,  3,
      { Aso }  0,  3,  1,  3,  1,  1,  3,  3);
  {$ENDREGION}
  strict private
    FLastCharacterClass: Byte;
    FFrequencyCounters: TArray<Integer>;
  protected
    function GetEncoding: TEncoding; override;
    function ForceAnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult; override;
  public
    constructor Create;
    function GetConfidence: Single; override;
    function FilterBuffer(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TArray<Byte>;
  end;

  { TdxCyrillicEncodingDetector }

  TdxCyrillicEncodingDetector = class abstract(TdxSingleByteEncodingDetector)
  strict private class var
    FPrecedenceMatrix: TArray<Integer>;
    class constructor Initialize;
  protected
    function GetPositiveRatio: Single; override;
    function GetPrecedenceMatrix: TArray<Integer>; override;
  end;

  { TdxBulgarianEncodingDetector }

  TdxBulgarianEncodingDetector = class abstract(TdxSingleByteEncodingDetector)
  strict private class var
    FPrecedenceMatrix: TArray<Integer>;
    class constructor Initialize;
  protected
    function GetPositiveRatio: Single; override;
    function GetPrecedenceMatrix: TArray<Integer>; override;
  end;

  { TdxIbm855EncodingDetector }

  TdxIbm855EncodingDetector = class(TdxCyrillicEncodingDetector)
  strict private class var
    FCharToOrderMap: TArray<Byte>;
    class constructor Initialize;
  protected
    function GetCharacterToOrderMap: TArray<Byte>; override;
    function GetEncoding: TEncoding; override;
  end;

  { TdxIbm866EncodingDetector }

  TdxIbm866EncodingDetector = class(TdxCyrillicEncodingDetector)
  strict private class var
    FCharToOrderMap: TArray<Byte>;
    class constructor Initialize;
  protected
    function GetCharacterToOrderMap: TArray<Byte>; override;
    function GetEncoding: TEncoding; override;
  end;

  { TdxKoi8rEncodingDetector }

  TdxKoi8rEncodingDetector = class(TdxCyrillicEncodingDetector)
  strict private class var
    FCharToOrderMap: TArray<Byte>;
    class constructor Initialize;
  protected
    function GetCharacterToOrderMap: TArray<Byte>; override;
    function GetEncoding: TEncoding; override;
  end;

  { TdxLatin5EncodingDetector }

  TdxLatin5EncodingDetector = class(TdxCyrillicEncodingDetector)
  strict private class var
    FCharToOrderMap: TArray<Byte>;
    class constructor Initialize;
  protected
    function GetCharacterToOrderMap: TArray<Byte>; override;
    function GetEncoding: TEncoding; override;
  end;

  { TdxLatin5BulgarianEncodingDetector }

  TdxLatin5BulgarianEncodingDetector = class(TdxBulgarianEncodingDetector)
  strict private class var
    FCharToOrderMap: TArray<Byte>;
    class constructor Initialize;
  protected
    function GetCharacterToOrderMap: TArray<Byte>; override;
    function GetEncoding: TEncoding; override;
  end;

  { TdxWin1251EncodingDetector }

  TdxWin1251EncodingDetector = class(TdxCyrillicEncodingDetector)
  strict private class var
    FCharToOrderMap: TArray<Byte>;
    class constructor Initialize;
  protected
    function GetCharacterToOrderMap: TArray<Byte>; override;
    function GetEncoding: TEncoding; override;
  end;

  { TdxWin1251BulgarianEncodingDetector }

  TdxWin1251BulgarianEncodingDetector = class(TdxBulgarianEncodingDetector)
  strict private class var
    FCharToOrderMap: TArray<Byte>;
    class constructor Initialize;
  protected
    function GetCharacterToOrderMap: TArray<Byte>; override;
    function GetEncoding: TEncoding; override;
  end;

  { TdxMacCyrillicDetector }

  TdxMacCyrillicDetector = class(TdxCyrillicEncodingDetector)
  strict private class var
    FCharToOrderMap: TArray<Byte>;
    class constructor Initialize;
  protected
    function GetCharacterToOrderMap: TArray<Byte>; override;
    function GetEncoding: TEncoding; override;
  end;

  { TdxGreekEncodingDetector }

  TdxGreekEncodingDetector = class abstract(TdxSingleByteEncodingDetector)
  strict private class var
    FPrecedenceMatrix: TArray<Integer>;
    class constructor Initialize;
  protected
    function GetPositiveRatio: Single; override;
    function GetPrecedenceMatrix: TArray<Integer>; override;
  end;

  { TdxLatin7EncodingDetector }

  TdxLatin7EncodingDetector = class(TdxGreekEncodingDetector)
  strict private class var
    FCharToOrderMap: TArray<Byte>;
    class constructor Initialize;
  protected
    function GetCharacterToOrderMap: TArray<Byte>; override;
    function GetEncoding: TEncoding; override;
  end;

  { TdxWin1253EncodingDetector }

  TdxWin1253EncodingDetector = class(TdxGreekEncodingDetector)
  strict private class var
    FCharToOrderMap: TArray<Byte>;
    class constructor Initialize;
  protected
    function GetCharacterToOrderMap: TArray<Byte>; override;
    function GetEncoding: TEncoding; override;
  end;

  { TdxBomLessUnicodeGroupDetector }

  TdxBomLessUnicodeGroupDetector = class(TdxGroupDetector)
  protected
    procedure PopulateDetectors; override;
    function ForceAnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult; override;
  public
    constructor Create;
  end;

  TdxEAState = Integer;

  { TdxEncodingAnalyzer }

  TdxEncodingAnalyzer = class abstract
  strict private
    FCurrentState: TdxEAState;
    FCurrentCharLength: Integer;
  protected
    function GetCharLenTable: TArray<Integer>; virtual; abstract;
    function GetStateTable: TArray<Integer>; virtual; abstract;
    function GetClassTable: TArray<Integer>; virtual; abstract;
    class function Pack8Bits(A, B, C, D: Integer): Integer; static;
    class function Pack16Bits(A, B: Integer): Integer; static;

    property CharLenTable: TArray<Integer> read GetCharLenTable;
    property StateTable: TArray<Integer> read GetStateTable;
    property ClassTable: TArray<Integer> read GetClassTable;
  public
    function NextState(C: Byte): TdxEAState;
    class function Pack4Bits(A, B, C, D, E, F, G, H: Integer): Integer; static;
    class function Unpack4Bits(I: Integer; const ABuffer: TArray<Integer>): Integer; static;

    property CurrentCharLength: Integer read FCurrentCharLength;
  end;

  { TdxUtf8EncodingAnalyzer }

  TdxUtf8EncodingAnalyzer = class(TdxEncodingAnalyzer)
  strict private class var
    FClassTable: TArray<Integer>;
    FStateTable: TArray<Integer>;
    FCharLenTable: TArray<Integer>;
    class constructor Initialize;
  protected
    function GetClassTable: TArray<Integer>; override;
    function GetStateTable: TArray<Integer>; override;
    function GetCharLenTable: TArray<Integer>; override;
  end;

  { TdxUtf8EncodingDetector }

  TdxUtf8EncodingDetector = class(TdxEncodingDetectorBase)
  strict private
    FMultiByteCharCount: Integer;
    FEncodingAnalyzer: TdxEncodingAnalyzer;
  protected
    function GetEncoding: TEncoding; override;
    function ForceAnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult; override;
  public
    constructor Create;
    destructor Destroy; override;
    function GetConfidence: Single; override;
  end;

  { TdxUtf16EncodingDetector }

  TdxUtf16EncodingDetector = class abstract(TdxEncodingDetectorBase)
  public const
    MinimumDataThreshold = 20;
    EnoughDataThreshold  = 256;
  strict private
    FIndex: Integer;
    FOddLowerByteCount: Integer;
    FEvenLowerByteCount: Integer;
  protected
    function GetConfidenceCore(AOddLowerByteCount, AEvenLowerByteCount: Integer; AHalfByteCount: Single): Single; virtual; abstract;
    function ForceAnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult; override;
  public
    function GetConfidence: Single; override;
  end;

  { TdxUtf16LittleEndianEncodingDetector }

  TdxUtf16LittleEndianEncodingDetector = class(TdxUtf16EncodingDetector)
  protected
    function GetEncoding: TEncoding; override;
    function GetConfidenceCore(AOddLowerByteCount, AEvenLowerByteCount: Integer; AHalfByteCount: Single): Single; override;
  end;

  { TdxUtf16BigEndianEncodingDetector }

  TdxUtf16BigEndianEncodingDetector = class(TdxUtf16EncodingDetector)
  protected
    function GetEncoding: TEncoding; override;
    function GetConfidenceCore(AOddLowerByteCount, AEvenLowerByteCount: Integer; AHalfByteCount: Single): Single; override;
  end;

  { TdxCharacterDistributionAnalyzer }

  TdxCharacterDistributionAnalyzer = class abstract
  protected const
    EnoughDataThreshold = 1024;
  strict private
    FFrequentCharCount: Integer;
    FTotalCharCount: Integer;
  protected
    function GetCharToFreqOrder: TArray<Word>; virtual; abstract;
    function GetTypicalDistributionRatio: Single; virtual; abstract;
    function GetOrder(const AStr: TArray<Byte>; AOffset: Integer): Integer; virtual; abstract;

    property CharToFreqOrder: TArray<Word> read GetCharToFreqOrder;
    property TypicalDistributionRatio: Single read GetTypicalDistributionRatio;
  public
    procedure AnalyzeCharacter(const ABuffer: TArray<Byte>; AFrom, ALength: Integer);
    function GetConfidence(AAIsPreferredLanguage: Boolean): Single;
    function GotEnoughData: Boolean;
  end;

  { TdxEastAsianEncodingDetector }

  TdxEastAsianEncodingDetector = class abstract(TdxEncodingDetectorBase)
  strict private
    FEncodingAnalyzer: TdxEncodingAnalyzer;
    FDistributionAnalyzer: TdxCharacterDistributionAnalyzer;
    FIsPreferredLanguage: Boolean;
    FLastCharBuffer: TArray<Byte>;
  protected
    function CreateDistributionAnalyzer: TdxCharacterDistributionAnalyzer; virtual; abstract;
    function CreateEncodingAnalyzer: TdxEncodingAnalyzer; virtual; abstract;
    function ForceAnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult; override;
    function GotEnoughData: Boolean; virtual;
    procedure ContinueContextAnalysis(const ABuffer: TArray<Byte>; AFrom, ALength: Integer); virtual;
    function GetContextAnalyzerConfidence(AIsPreferredLanguage: Boolean): Single; virtual;
  public
    constructor Create(APreferredLanguage: Boolean);
    destructor Destroy; override;
    function GetConfidence: Single; override;
  end;

  { TdxMultiByteCharsetGroupDetector }

  TdxMultiByteCharsetGroupDetector = class(TdxGroupDetector)
  strict private
    FLanguageFilter: TdxEncodingDetectorLanguages;
    FKeepNext: Integer;
  protected
    procedure PopulateDetectors; override;
    function ForceAnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult; override;
  public
    constructor Create(const ALanguageFilter: TdxEncodingDetectorLanguages);
  end;

  { TdxBig5EncodingDetector }

  TdxBig5EncodingDetector = class(TdxEastAsianEncodingDetector)
  protected
    function GetEncoding: TEncoding; override;
    function CreateDistributionAnalyzer: TdxCharacterDistributionAnalyzer; override;
    function CreateEncodingAnalyzer: TdxEncodingAnalyzer; override;
  end;

  { TdxBig5CharDistributionAnalyzer }

  TdxBig5CharDistributionAnalyzer = class(TdxCharacterDistributionAnalyzer)
  strict private class var
    FCharToFreqOrder: TArray<Word>;
    class constructor Initialize;
  protected
    function GetOrder(const AStr: TArray<Byte>; AOffset: Integer): Integer; override;
    function GetCharToFreqOrder: TArray<Word>; override;
    function GetTypicalDistributionRatio: Single; override;
  end;

  { TdxBig5EncodingAnalyzer }

  TdxBig5EncodingAnalyzer = class(TdxEncodingAnalyzer)
  strict private class var
    FClassTable: TArray<Integer>;
    FStateTable: TArray<Integer>;
    FCharLenTable: TArray<Integer>;
    class constructor Initialize;
  protected
    function GetClassTable: TArray<Integer>; override;
    function GetStateTable: TArray<Integer>; override;
    function GetCharLenTable: TArray<Integer>; override;
  end;

  { TdxGB18030EncodingDetector }

  TdxGB18030EncodingDetector = class(TdxEastAsianEncodingDetector)
  protected
    function GetEncoding: TEncoding; override;
    function CreateDistributionAnalyzer: TdxCharacterDistributionAnalyzer; override;
    function CreateEncodingAnalyzer: TdxEncodingAnalyzer; override;
  end;

  { TdxGB18030CharDistributionAnalyzer }

  TdxGB18030CharDistributionAnalyzer = class(TdxCharacterDistributionAnalyzer)
  strict private class var
    FCharToFreqOrder: TArray<Word>;
    class constructor Initialize;
  protected
    function GetOrder(const AStr: TArray<Byte>; AOffset: Integer): Integer; override;
    function GetCharToFreqOrder: TArray<Word>; override;
    function GetTypicalDistributionRatio: Single; override;
  end;

  { TdxGB18030EncodingAnalyzer }

  TdxGB18030EncodingAnalyzer = class(TdxEncodingAnalyzer)
  strict private class var
    FClassTable: TArray<Integer>;
    FStateTable: TArray<Integer>;
    FCharLenTable: TArray<Integer>;
    class constructor Initialize;
  protected
    function GetClassTable: TArray<Integer>; override;
    function GetStateTable: TArray<Integer>; override;
    function GetCharLenTable: TArray<Integer>; override;
  end;

  { TdxEucKrEncodingDetector }

  TdxEucKrEncodingDetector = class(TdxEastAsianEncodingDetector)
  protected
    function GetEncoding: TEncoding; override;
    function CreateDistributionAnalyzer: TdxCharacterDistributionAnalyzer; override;
    function CreateEncodingAnalyzer: TdxEncodingAnalyzer; override;
  end;

  { TdxEucKrCharDistributionAnalyzer }

  TdxEucKrCharDistributionAnalyzer = class(TdxCharacterDistributionAnalyzer)
  strict private class var
    FCharToFreqOrder: TArray<Word>;
    class constructor Initialize;
  protected
    function GetOrder(const AStr: TArray<Byte>; AOffset: Integer): Integer; override;
    function GetCharToFreqOrder: TArray<Word>; override;
    function GetTypicalDistributionRatio: Single; override;
  end;

  { TdxEucKrEncodingAnalyzer }

  TdxEucKrEncodingAnalyzer = class(TdxEncodingAnalyzer)
  strict private class var
    FClassTable: TArray<Integer>;
    FStateTable: TArray<Integer>;
    FCharLenTable: TArray<Integer>;
    class constructor Initialize;
  protected
    function GetClassTable: TArray<Integer>; override;
    function GetStateTable: TArray<Integer>; override;
    function GetCharLenTable: TArray<Integer>; override;
  end;

  { TdxJapaneseContextAnalyzer }

  TdxJapaneseContextAnalyzer = class abstract
  strict private const
  {$REGION 'class consts'}
    CategoryCount = 6;
    EnoughSequenceCount = 100;
    MinSequenceCount = 4;
    MaxSequenceCount = 1000;
    Jp2CharContext: array[0..82, 0..82] of Byte = (
      (0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1),
      (2,4,0,4,0,3,0,4,0,3,4,4,4,2,4,3,3,4,3,2,3,3,4,2,3,3,3,2,4,1,4,3,3,1,5,4,3,4,3,4,3,5,3,0,3,5,4,2,0,3,1,0,3,3,0,3,3,0,1,1,0,4,3,0,3,3,0,4,0,2,0,3,5,5,5,5,4,0,4,1,0,3,4),
      (0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2),
      (0,4,0,5,0,5,0,4,0,4,5,4,4,3,5,3,5,1,5,3,4,3,4,4,3,4,3,3,4,3,5,4,4,3,5,5,3,5,5,5,3,5,5,3,4,5,5,3,1,3,2,0,3,4,0,4,2,0,4,2,1,5,3,2,3,5,0,4,0,2,0,5,4,4,5,4,5,0,4,0,0,4,4),
      (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (0,3,0,4,0,3,0,3,0,4,5,4,3,3,3,3,4,3,5,4,4,3,5,4,4,3,4,3,4,4,4,4,5,3,4,4,3,4,5,5,4,5,5,1,4,5,4,3,0,3,3,1,3,3,0,4,4,0,3,3,1,5,3,3,3,5,0,4,0,3,0,4,4,3,4,3,3,0,4,1,1,3,4),
      (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (0,4,0,3,0,3,0,4,0,3,4,4,3,2,2,1,2,1,3,1,3,3,3,3,3,4,3,1,3,3,5,3,3,0,4,3,0,5,4,3,3,5,4,4,3,4,4,5,0,1,2,0,1,2,0,2,2,0,1,0,0,5,2,2,1,4,0,3,0,1,0,4,4,3,5,4,3,0,2,1,0,4,3),
      (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (0,3,0,5,0,4,0,2,1,4,4,2,4,1,4,2,4,2,4,3,3,3,4,3,3,3,3,1,4,2,3,3,3,1,4,4,1,1,1,4,3,3,2,0,2,4,3,2,0,3,3,0,3,1,1,0,0,0,3,3,0,4,2,2,3,4,0,4,0,3,0,4,4,5,3,4,4,0,3,0,0,1,4),
      (1,4,0,4,0,4,0,4,0,3,5,4,4,3,4,3,5,4,3,3,4,3,5,4,4,4,4,3,4,2,4,3,3,1,5,4,3,2,4,5,4,5,5,4,4,5,4,4,0,3,2,2,3,3,0,4,3,1,3,2,1,4,3,3,4,5,0,3,0,2,0,4,5,5,4,5,4,0,4,0,0,5,4),
      (0,5,0,5,0,4,0,3,0,4,4,3,4,3,3,3,4,0,4,4,4,3,4,3,4,3,3,1,4,2,4,3,4,0,5,4,1,4,5,4,4,5,3,2,4,3,4,3,2,4,1,3,3,3,2,3,2,0,4,3,3,4,3,3,3,4,0,4,0,3,0,4,5,4,4,4,3,0,4,1,0,1,3),
      (0,3,1,4,0,3,0,2,0,3,4,4,3,1,4,2,3,3,4,3,4,3,4,3,4,4,3,2,3,1,5,4,4,1,4,4,3,5,4,4,3,5,5,4,3,4,4,3,1,2,3,1,2,2,0,3,2,0,3,1,0,5,3,3,3,4,3,3,3,3,4,4,4,4,5,4,2,0,3,3,2,4,3),
      (0,2,0,3,0,1,0,1,0,0,3,2,0,0,2,0,1,0,2,1,3,3,3,1,2,3,1,0,1,0,4,2,1,1,3,3,0,4,3,3,1,4,3,3,0,3,3,2,0,0,0,0,1,0,0,2,0,0,0,0,0,4,1,0,2,3,2,2,2,1,3,3,3,4,4,3,2,0,3,1,0,3,3),
      (0,4,0,4,0,3,0,3,0,4,4,4,3,3,3,3,3,3,4,3,4,2,4,3,4,3,3,2,4,3,4,5,4,1,4,5,3,5,4,5,3,5,4,0,3,5,5,3,1,3,3,2,2,3,0,3,4,1,3,3,2,4,3,3,3,4,0,4,0,3,0,4,5,4,4,5,3,0,4,1,0,3,4),
      (0,2,0,3,0,3,0,0,0,2,2,2,1,0,1,0,0,0,3,0,3,0,3,0,1,3,1,0,3,1,3,3,3,1,3,3,3,0,1,3,1,3,4,0,0,3,1,1,0,3,2,0,0,0,0,1,3,0,1,0,0,3,3,2,0,3,0,0,0,0,0,3,4,3,4,3,3,0,3,0,0,2,3),
      (2,3,0,3,0,2,0,1,0,3,3,4,3,1,3,1,1,1,3,1,4,3,4,3,3,3,0,0,3,1,5,4,3,1,4,3,2,5,5,4,4,4,4,3,3,4,4,4,0,2,1,1,3,2,0,1,2,0,0,1,0,4,1,3,3,3,0,3,0,1,0,4,4,4,5,5,3,0,2,0,0,4,4),
      (0,2,0,1,0,3,1,3,0,2,3,3,3,0,3,1,0,0,3,0,3,2,3,1,3,2,1,1,0,0,4,2,1,0,2,3,1,4,3,2,0,4,4,3,1,3,1,3,0,1,0,0,1,0,0,0,1,0,0,0,0,4,1,1,1,2,0,3,0,0,0,3,4,2,4,3,2,0,1,0,0,3,3),
      (0,1,0,4,0,5,0,4,0,2,4,4,2,3,3,2,3,3,5,3,3,3,4,3,4,2,3,0,4,3,3,3,4,1,4,3,2,1,5,5,3,4,5,1,3,5,4,2,0,3,3,0,1,3,0,4,2,0,1,3,1,4,3,3,3,3,0,3,0,1,0,3,4,4,4,5,5,0,3,0,1,4,5),
      (0,2,0,3,0,3,0,0,0,2,3,1,3,0,4,0,1,1,3,0,3,4,3,2,3,1,0,3,3,2,3,1,3,0,2,3,0,2,1,4,1,2,2,0,0,3,3,0,0,2,0,0,0,1,0,0,0,0,2,2,0,3,2,1,3,3,0,2,0,2,0,0,3,3,1,2,4,0,3,0,2,2,3),
      (2,4,0,5,0,4,0,4,0,2,4,4,4,3,4,3,3,3,1,2,4,3,4,3,4,4,5,0,3,3,3,3,2,0,4,3,1,4,3,4,1,4,4,3,3,4,4,3,1,2,3,0,4,2,0,4,1,0,3,3,0,4,3,3,3,4,0,4,0,2,0,3,5,3,4,5,2,0,3,0,0,4,5),
      (0,3,0,4,0,1,0,1,0,1,3,2,2,1,3,0,3,0,2,0,2,0,3,0,2,0,0,0,1,0,1,1,0,0,3,1,0,0,0,4,0,3,1,0,2,1,3,0,0,0,0,0,0,3,0,0,0,0,0,0,0,4,2,2,3,1,0,3,0,0,0,1,4,4,4,3,0,0,4,0,0,1,4),
      (1,4,1,5,0,3,0,3,0,4,5,4,4,3,5,3,3,4,4,3,4,1,3,3,3,3,2,1,4,1,5,4,3,1,4,4,3,5,4,4,3,5,4,3,3,4,4,4,0,3,3,1,2,3,0,3,1,0,3,3,0,5,4,4,4,4,4,4,3,3,5,4,4,3,3,5,4,0,3,2,0,4,4),
      (0,2,0,3,0,1,0,0,0,1,3,3,3,2,4,1,3,0,3,1,3,0,2,2,1,1,0,0,2,0,4,3,1,0,4,3,0,4,4,4,1,4,3,1,1,3,3,1,0,2,0,0,1,3,0,0,0,0,2,0,0,4,3,2,4,3,5,4,3,3,3,4,3,3,4,3,3,0,2,1,0,3,3),
      (0,2,0,4,0,3,0,2,0,2,5,5,3,4,4,4,4,1,4,3,3,0,4,3,4,3,1,3,3,2,4,3,0,3,4,3,0,3,4,4,2,4,4,0,4,5,3,3,2,2,1,1,1,2,0,1,5,0,3,3,2,4,3,3,3,4,0,3,0,2,0,4,4,3,5,5,0,0,3,0,2,3,3),
      (0,3,0,4,0,3,0,1,0,3,4,3,3,1,3,3,3,0,3,1,3,0,4,3,3,1,1,0,3,0,3,3,0,0,4,4,0,1,5,4,3,3,5,0,3,3,4,3,0,2,0,1,1,1,0,1,3,0,1,2,1,3,3,2,3,3,0,3,0,1,0,1,3,3,4,4,1,0,1,2,2,1,3),
      (0,1,0,4,0,4,0,3,0,1,3,3,3,2,3,1,1,0,3,0,3,3,4,3,2,4,2,0,1,0,4,3,2,0,4,3,0,5,3,3,2,4,4,4,3,3,3,4,0,1,3,0,0,1,0,0,1,0,0,0,0,4,2,3,3,3,0,3,0,0,0,4,4,4,5,3,2,0,3,3,0,3,5),
      (0,2,0,3,0,0,0,3,0,1,3,0,2,0,0,0,1,0,3,1,1,3,3,0,0,3,0,0,3,0,2,3,1,0,3,1,0,3,3,2,0,4,2,2,0,2,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,2,1,2,0,1,0,1,0,0,0,1,3,1,2,0,0,0,1,0,0,1,4),
      (0,3,0,3,0,5,0,1,0,2,4,3,1,3,3,2,1,1,5,2,1,0,5,1,2,0,0,0,3,3,2,2,3,2,4,3,0,0,3,3,1,3,3,0,2,5,3,4,0,3,3,0,1,2,0,2,2,0,3,2,0,2,2,3,3,3,0,2,0,1,0,3,4,4,2,5,4,0,3,0,0,3,5),
      (0,3,0,3,0,3,0,1,0,3,3,3,3,0,3,0,2,0,2,1,1,0,2,0,1,0,0,0,2,1,0,0,1,0,3,2,0,0,3,3,1,2,3,1,0,3,3,0,0,1,0,0,0,0,0,2,0,0,0,0,0,2,3,1,2,3,0,3,0,1,0,3,2,1,0,4,3,0,1,1,0,3,3),
      (0,4,0,5,0,3,0,3,0,4,5,5,4,3,5,3,4,3,5,3,3,2,5,3,4,4,4,3,4,3,4,5,5,3,4,4,3,4,4,5,4,4,4,3,4,5,5,4,2,3,4,2,3,4,0,3,3,1,4,3,2,4,3,3,5,5,0,3,0,3,0,5,5,5,5,4,4,0,4,0,1,4,4),
      (0,4,0,4,0,3,0,3,0,3,5,4,4,2,3,2,5,1,3,2,5,1,4,2,3,2,3,3,4,3,3,3,3,2,5,4,1,3,3,5,3,4,4,0,4,4,3,1,1,3,1,0,2,3,0,2,3,0,3,0,0,4,3,1,3,4,0,3,0,2,0,4,4,4,3,4,5,0,4,0,0,3,4),
      (0,3,0,3,0,3,1,2,0,3,4,4,3,3,3,0,2,2,4,3,3,1,3,3,3,1,1,0,3,1,4,3,2,3,4,4,2,4,4,4,3,4,4,3,2,4,4,3,1,3,3,1,3,3,0,4,1,0,2,2,1,4,3,2,3,3,5,4,3,3,5,4,4,3,3,0,4,0,3,2,2,4,4),
      (0,2,0,1,0,0,0,0,0,1,2,1,3,0,0,0,0,0,2,0,1,2,1,0,0,1,0,0,0,0,3,0,0,1,0,1,1,3,1,0,0,0,1,1,0,1,1,0,0,0,0,0,2,0,0,0,0,0,0,0,0,1,1,2,2,0,3,4,0,0,0,1,1,0,0,1,0,0,0,0,0,1,1),
      (0,1,0,0,0,1,0,0,0,0,4,0,4,1,4,0,3,0,4,0,3,0,4,0,3,0,3,0,4,1,5,1,4,0,0,3,0,5,0,5,2,0,1,0,0,0,2,1,4,0,1,3,0,0,3,0,0,3,1,1,4,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),
      (1,4,0,5,0,3,0,2,0,3,5,4,4,3,4,3,5,3,4,3,3,0,4,3,3,3,3,3,3,2,4,4,3,1,3,4,4,5,4,4,3,4,4,1,3,5,4,3,3,3,1,2,2,3,3,1,3,1,3,3,3,5,3,3,4,5,0,3,0,3,0,3,4,3,4,4,3,0,3,0,2,4,3),
      (0,1,0,4,0,0,0,0,0,1,4,0,4,1,4,2,4,0,3,0,1,0,1,0,0,0,0,0,2,0,3,1,1,1,0,3,0,0,0,1,2,1,0,0,1,1,1,1,0,1,0,0,0,1,0,0,3,0,0,0,0,3,2,0,2,2,0,1,0,0,0,2,3,2,3,3,0,0,0,0,2,1,0),
      (0,5,1,5,0,3,0,3,0,5,4,4,5,1,5,3,3,0,4,3,4,3,5,3,4,3,3,2,4,3,4,3,3,0,3,3,1,4,4,3,4,4,4,3,4,5,5,3,2,3,1,1,3,3,1,3,1,1,3,3,2,4,5,3,3,5,0,4,0,3,0,4,4,3,5,3,3,0,3,4,0,4,3),
      (0,5,0,5,0,3,0,2,0,4,4,3,5,2,4,3,3,3,4,4,4,3,5,3,5,3,3,1,4,0,4,3,3,0,3,3,0,4,4,4,4,5,4,3,3,5,5,3,2,3,1,2,3,2,0,1,0,0,3,2,2,4,4,3,1,5,0,4,0,3,0,4,3,1,3,2,1,0,3,3,0,3,3),
      (0,4,0,5,0,5,0,4,0,4,5,5,5,3,4,3,3,2,5,4,4,3,5,3,5,3,4,0,4,3,4,4,3,2,4,4,3,4,5,4,4,5,5,0,3,5,5,4,1,3,3,2,3,3,1,3,1,0,4,3,1,4,4,3,4,5,0,4,0,2,0,4,3,4,4,3,3,0,4,0,0,5,5),
      (0,4,0,4,0,5,0,1,1,3,3,4,4,3,4,1,3,0,5,1,3,0,3,1,3,1,1,0,3,0,3,3,4,0,4,3,0,4,4,4,3,4,4,0,3,5,4,1,0,3,0,0,2,3,0,3,1,0,3,1,0,3,2,1,3,5,0,3,0,1,0,3,2,3,3,4,4,0,2,2,0,4,4),
      (2,4,0,5,0,4,0,3,0,4,5,5,4,3,5,3,5,3,5,3,5,2,5,3,4,3,3,4,3,4,5,3,2,1,5,4,3,2,3,4,5,3,4,1,2,5,4,3,0,3,3,0,3,2,0,2,3,0,4,1,0,3,4,3,3,5,0,3,0,1,0,4,5,5,5,4,3,0,4,2,0,3,5),
      (0,5,0,4,0,4,0,2,0,5,4,3,4,3,4,3,3,3,4,3,4,2,5,3,5,3,4,1,4,3,4,4,4,0,3,5,0,4,4,4,4,5,3,1,3,4,5,3,3,3,3,3,3,3,0,2,2,0,3,3,2,4,3,3,3,5,3,4,1,3,3,5,3,2,0,0,0,0,4,3,1,3,3),
      (0,1,0,3,0,3,0,1,0,1,3,3,3,2,3,3,3,0,3,0,0,0,3,1,3,0,0,0,2,2,2,3,0,0,3,2,0,1,2,4,1,3,3,0,0,3,3,3,0,1,0,0,2,1,0,0,3,0,3,1,0,3,0,0,1,3,0,2,0,1,0,3,3,1,3,3,0,0,1,1,0,3,3),
      (0,2,0,3,0,2,1,4,0,2,2,3,1,1,3,1,1,0,2,0,3,1,2,3,1,3,0,0,1,0,4,3,2,3,3,3,1,4,2,3,3,3,3,1,0,3,1,4,0,1,1,0,1,2,0,1,1,0,1,1,0,3,1,3,2,2,0,1,0,0,0,2,3,3,3,1,0,0,0,0,0,2,3),
      (0,5,0,4,0,5,0,2,0,4,5,5,3,3,4,3,3,1,5,4,4,2,4,4,4,3,4,2,4,3,5,5,4,3,3,4,3,3,5,5,4,5,5,1,3,4,5,3,1,4,3,1,3,3,0,3,3,1,4,3,1,4,5,3,3,5,0,4,0,3,0,5,3,3,1,4,3,0,4,0,1,5,3),
      (0,5,0,5,0,4,0,2,0,4,4,3,4,3,3,3,3,3,5,4,4,4,4,4,4,5,3,3,5,2,4,4,4,3,4,4,3,3,4,4,5,5,3,3,4,3,4,3,3,4,3,3,3,3,1,2,2,1,4,3,3,5,4,4,3,4,0,4,0,3,0,4,4,4,4,4,1,0,4,2,0,2,4),
      (0,4,0,4,0,3,0,1,0,3,5,2,3,0,3,0,2,1,4,2,3,3,4,1,4,3,3,2,4,1,3,3,3,0,3,3,0,0,3,3,3,5,3,3,3,3,3,2,0,2,0,0,2,0,0,2,0,0,1,0,0,3,1,2,2,3,0,3,0,2,0,4,4,3,3,4,1,0,3,0,0,2,4),
      (0,0,0,4,0,0,0,0,0,0,1,0,1,0,2,0,0,0,0,0,1,0,2,0,1,0,0,0,0,0,3,1,3,0,3,2,0,0,0,1,0,3,2,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,4,0,2,0,0,0,0,0,0,2),
      (0,2,1,3,0,2,0,2,0,3,3,3,3,1,3,1,3,3,3,3,3,3,4,2,2,1,2,1,4,0,4,3,1,3,3,3,2,4,3,5,4,3,3,3,3,3,3,3,0,1,3,0,2,0,0,1,0,0,1,0,0,4,2,0,2,3,0,3,3,0,3,3,4,2,3,1,4,0,1,2,0,2,3),
      (0,3,0,3,0,1,0,3,0,2,3,3,3,0,3,1,2,0,3,3,2,3,3,2,3,2,3,1,3,0,4,3,2,0,3,3,1,4,3,3,2,3,4,3,1,3,3,1,1,0,1,1,0,1,0,1,0,1,0,0,0,4,1,1,0,3,0,3,1,0,2,3,3,3,3,3,1,0,0,2,0,3,3),
      (0,0,0,0,0,0,0,0,0,0,3,0,2,0,3,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,3,0,3,0,3,1,0,1,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,2,0,2,3,0,0,0,0,0,0,0,0,3),
      (0,2,0,3,1,3,0,3,0,2,3,3,3,1,3,1,3,1,3,1,3,3,3,1,3,0,2,3,1,1,4,3,3,2,3,3,1,2,2,4,1,3,3,0,1,4,2,3,0,1,3,0,3,0,0,1,3,0,2,0,0,3,3,2,1,3,0,3,0,2,0,3,4,4,4,3,1,0,3,0,0,3,3),
      (0,2,0,1,0,2,0,0,0,1,3,2,2,1,3,0,1,1,3,0,3,2,3,1,2,0,2,0,1,1,3,3,3,0,3,3,1,1,2,3,2,3,3,1,2,3,2,0,0,1,0,0,0,0,0,0,3,0,1,0,0,2,1,2,1,3,0,3,0,0,0,3,4,4,4,3,2,0,2,0,0,2,4),
      (0,0,0,1,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,2,2,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,3,1,0,0,0,0,0,0,0,3),
      (0,3,0,3,0,2,0,3,0,3,3,3,2,3,2,2,2,0,3,1,3,3,3,2,3,3,0,0,3,0,3,2,2,0,2,3,1,4,3,4,3,3,2,3,1,5,4,4,0,3,1,2,1,3,0,3,1,1,2,0,2,3,1,3,1,3,0,3,0,1,0,3,3,4,4,2,1,0,2,1,0,2,4),
      (0,1,0,3,0,1,0,2,0,1,4,2,5,1,4,0,2,0,2,1,3,1,4,0,2,1,0,0,2,1,4,1,1,0,3,3,0,5,1,3,2,3,3,1,0,3,2,3,0,1,0,0,0,0,0,0,1,0,0,0,0,4,0,1,0,3,0,2,0,1,0,3,3,3,4,3,3,0,0,0,0,2,3),
      (0,0,0,1,0,0,0,0,0,0,2,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,1,0,0,1,0,0,0,0,0,3),
      (0,1,0,3,0,4,0,3,0,2,4,3,1,0,3,2,2,1,3,1,2,2,3,1,1,1,2,1,3,0,1,2,0,1,3,2,1,3,0,5,5,1,0,0,1,3,2,1,0,3,0,0,1,0,0,0,0,0,3,4,0,1,1,1,3,2,0,2,0,1,0,2,3,3,1,2,3,0,1,0,1,0,4),
      (0,0,0,1,0,3,0,3,0,2,2,1,0,0,4,0,3,0,3,1,3,0,3,0,3,0,1,0,3,0,3,1,3,0,3,3,0,0,1,2,1,1,1,0,1,2,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,2,2,1,2,0,0,2,0,0,0,0,2,3,3,3,3,0,0,0,0,1,4),
      (0,0,0,3,0,3,0,0,0,0,3,1,1,0,3,0,1,0,2,0,1,0,0,0,0,0,0,0,1,0,3,0,2,0,2,3,0,0,2,2,3,1,2,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,2,0,0,0,0,2,3),
      (2,4,0,5,0,5,0,4,0,3,4,3,3,3,4,3,3,3,4,3,4,4,5,4,5,5,5,2,3,0,5,5,4,1,5,4,3,1,5,4,3,4,4,3,3,4,3,3,0,3,2,0,2,3,0,3,0,0,3,3,0,5,3,2,3,3,0,3,0,3,0,3,4,5,4,5,3,0,4,3,0,3,4),
      (0,3,0,3,0,3,0,3,0,3,3,4,3,2,3,2,3,0,4,3,3,3,3,3,3,3,3,0,3,2,4,3,3,1,3,4,3,4,4,4,3,4,4,3,2,4,4,1,0,2,0,0,1,1,0,2,0,0,3,1,0,5,3,2,1,3,0,3,0,1,2,4,3,2,4,3,3,0,3,2,0,4,4),
      (0,3,0,3,0,1,0,0,0,1,4,3,3,2,3,1,3,1,4,2,3,2,4,2,3,4,3,0,2,2,3,3,3,0,3,3,3,0,3,4,1,3,3,0,3,4,3,3,0,1,1,0,1,0,0,0,4,0,3,0,0,3,1,2,1,3,0,4,0,1,0,4,3,3,4,3,3,0,2,0,0,3,3),
      (0,3,0,4,0,1,0,3,0,3,4,3,3,0,3,3,3,1,3,1,3,3,4,3,3,3,0,0,3,1,5,3,3,1,3,3,2,5,4,3,3,4,5,3,2,5,3,4,0,1,0,0,0,0,0,2,0,0,1,1,0,4,2,2,1,3,0,3,0,2,0,4,4,3,5,3,2,0,1,1,0,3,4),
      (0,5,0,4,0,5,0,2,0,4,4,3,3,2,3,3,3,1,4,3,4,1,5,3,4,3,4,0,4,2,4,3,4,1,5,4,0,4,4,4,4,5,4,1,3,5,4,2,1,4,1,1,3,2,0,3,1,0,3,2,1,4,3,3,3,4,0,4,0,3,0,4,4,4,3,3,3,0,4,2,0,3,4),
      (1,4,0,4,0,3,0,1,0,3,3,3,1,1,3,3,2,2,3,3,1,0,3,2,2,1,2,0,3,1,2,1,2,0,3,2,0,2,2,3,3,4,3,0,3,3,1,2,0,1,1,3,1,2,0,0,3,0,1,1,0,3,2,2,3,3,0,3,0,0,0,2,3,3,4,3,3,0,1,0,0,1,4),
      (0,4,0,4,0,4,0,0,0,3,4,4,3,1,4,2,3,2,3,3,3,1,4,3,4,0,3,0,4,2,3,3,2,2,5,4,2,1,3,4,3,4,3,1,3,3,4,2,0,2,1,0,3,3,0,0,2,0,3,1,0,4,4,3,4,3,0,4,0,1,0,2,4,4,4,4,4,0,3,2,0,3,3),
      (0,0,0,1,0,4,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,3,2,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,2),
      (0,2,0,3,0,4,0,4,0,1,3,3,3,0,4,0,2,1,2,1,1,1,2,0,3,1,1,0,1,0,3,1,0,0,3,3,2,0,1,1,0,0,0,0,0,1,0,2,0,2,2,0,3,1,0,0,1,0,1,1,0,1,2,0,3,0,0,0,0,1,0,0,3,3,4,3,1,0,1,0,3,0,2),
      (0,0,0,3,0,5,0,0,0,0,1,0,2,0,3,1,0,1,3,0,0,0,2,0,0,0,1,0,0,0,1,1,0,0,4,0,0,0,2,3,0,1,4,1,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,1,0,0,0,0,0,0,0,2,0,0,3,0,0,0,0,0,3),
      (0,2,0,5,0,5,0,1,0,2,4,3,3,2,5,1,3,2,3,3,3,0,4,1,2,0,3,0,4,0,2,2,1,1,5,3,0,0,1,4,2,3,2,0,3,3,3,2,0,2,4,1,1,2,0,1,1,0,3,1,0,1,3,1,2,3,0,2,0,0,0,1,3,5,4,4,4,0,3,0,0,1,3),
      (0,4,0,5,0,4,0,4,0,4,5,4,3,3,4,3,3,3,4,3,4,4,5,3,4,5,4,2,4,2,3,4,3,1,4,4,1,3,5,4,4,5,5,4,4,5,5,5,2,3,3,1,4,3,1,3,3,0,3,3,1,4,3,4,4,4,0,3,0,4,0,3,3,4,4,5,0,0,4,3,0,4,5),
      (0,4,0,4,0,3,0,3,0,3,4,4,4,3,3,2,4,3,4,3,4,3,5,3,4,3,2,1,4,2,4,4,3,1,3,4,2,4,5,5,3,4,5,4,1,5,4,3,0,3,2,2,3,2,1,3,1,0,3,3,3,5,3,3,3,5,4,4,2,3,3,4,3,3,3,2,1,0,3,2,1,4,3),
      (0,4,0,5,0,4,0,3,0,3,5,5,3,2,4,3,4,0,5,4,4,1,4,4,4,3,3,3,4,3,5,5,2,3,3,4,1,2,5,5,3,5,5,2,3,5,5,4,0,3,2,0,3,3,1,1,5,1,4,1,0,4,3,2,3,5,0,4,0,3,0,5,4,3,4,3,0,0,4,1,0,4,4),
      (1,3,0,4,0,2,0,2,0,2,5,5,3,3,3,3,3,0,4,2,3,4,4,4,3,4,0,0,3,4,5,4,3,3,3,3,2,5,5,4,5,5,5,4,3,5,5,5,1,3,1,0,1,0,0,3,2,0,4,2,0,5,2,3,2,4,1,3,0,3,0,4,5,4,5,4,3,0,4,2,0,5,4),
      (0,3,0,4,0,5,0,3,0,3,4,4,3,2,3,2,3,3,3,3,3,2,4,3,3,2,2,0,3,3,3,3,3,1,3,3,3,0,4,4,3,4,4,1,1,4,4,2,0,3,1,0,1,1,0,4,1,0,2,3,1,3,3,1,3,4,0,3,0,1,0,3,1,3,0,0,1,0,2,0,0,4,4),
      (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (0,3,0,3,0,2,0,3,0,1,5,4,3,3,3,1,4,2,1,2,3,4,4,2,4,4,5,0,3,1,4,3,4,0,4,3,3,3,2,3,2,5,3,4,3,2,2,3,0,0,3,0,2,1,0,1,2,0,0,0,0,2,1,1,3,1,0,2,0,4,0,3,4,4,4,5,2,0,2,0,0,1,3),
      (0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,1,0,0,1,1,0,0,0,4,2,1,1,0,1,0,3,2,0,0,3,1,1,1,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,1,0,0,0,2,0,0,0,1,4,0,4,2,1,0,0,0,0,0,1),
      (0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,1,0,0,0,0,3,1,0,0,0,2,0,2,1,0,0,1,2,1,0,1,1,0,0,3,0,0,0,0,0,0,0,0,0,0,0,1,3,1,0,0,0,0,0,1,0,0,2,1,0,0,0,0,0,0,0,0,2),
      (0,4,0,4,0,4,0,3,0,4,4,3,4,2,4,3,2,0,4,4,4,3,5,3,5,3,3,2,4,2,4,3,4,3,1,4,0,2,3,4,4,4,3,3,3,4,4,4,3,4,1,3,4,3,2,1,2,1,3,3,3,4,4,3,3,5,0,4,0,3,0,4,3,3,3,2,1,0,3,0,0,3,3),
      (0,4,0,3,0,3,0,3,0,3,5,5,3,3,3,3,4,3,4,3,3,3,4,4,4,3,3,3,3,4,3,5,3,3,1,3,2,4,5,5,5,5,4,3,4,5,5,3,2,2,3,3,3,3,2,3,3,1,2,3,2,4,3,3,3,4,0,4,0,2,0,4,3,2,2,1,2,0,3,0,0,4,1));
  {$ENDREGION}
  strict private
    FTotalSequenceCount: Integer;
    FLastCharacterOrder: Integer;
    FAnalysisComplete: Boolean;
    FSequenceCounters: TArray<Integer>;
  protected
    function GetOrder(const AStr: TArray<Byte>; AOffset: Integer): Integer; virtual; abstract;
  public
    constructor Create;
    procedure AnalyzeCharacter(const ABuffer: TArray<Byte>; AFrom, ALength: Integer);
    function GetConfidence(AIsPreferredLanguage: Boolean): Single;
    function GotEnoughData: Boolean;
  end;

  { TdxJapaneseEncodingDetector }

  TdxJapaneseEncodingDetector = class abstract(TdxEastAsianEncodingDetector)
  strict private
    FContextAnalyzer: TdxJapaneseContextAnalyzer;
  protected
    procedure ContinueContextAnalysis(const ABuffer: TArray<Byte>; AFrom, ALength: Integer); override;
    function GetContextAnalyzerConfidence(AIsPreferredLanguage: Boolean): Single; override;
    function GotEnoughData: Boolean; override;
    function CreateContextAnalyzer: TdxJapaneseContextAnalyzer; virtual; abstract;
  public
    constructor Create(APreferredLanguage: Boolean);
    destructor Destroy; override;
  end;

  { TdxJapaneseCharDistributionAnalyzer }

  TdxJapaneseCharDistributionAnalyzer = class abstract(TdxCharacterDistributionAnalyzer)
  strict private class var
    FCharToFreqOrder: TArray<Word>;
    class constructor Initialize;
  protected
    function GetCharToFreqOrder: TArray<Word>; override;
    function GetTypicalDistributionRatio: Single; override;
  end;

  { TdxEucJpEncodingDetector }

  TdxEucJpEncodingDetector = class(TdxJapaneseEncodingDetector)
  protected
    function GetEncoding: TEncoding; override;
    function CreateDistributionAnalyzer: TdxCharacterDistributionAnalyzer; override;
    function CreateEncodingAnalyzer: TdxEncodingAnalyzer; override;
    function CreateContextAnalyzer: TdxJapaneseContextAnalyzer; override;
  end;

  { TdxEucJpCharDistributionAnalyzer }

  TdxEucJpCharDistributionAnalyzer = class(TdxJapaneseCharDistributionAnalyzer)
  protected
    function GetOrder(const AStr: TArray<Byte>; AOffset: Integer): Integer; override;
  end;

  { TdxEucJpContextAnalyzer }

  TdxEucJpContextAnalyzer = class(TdxJapaneseContextAnalyzer)
  protected
    function GetOrder(const AStr: TArray<Byte>; AOffset: Integer): Integer; override;
  end;

  { TdxEucJpEncodingAnalyzer }

  TdxEucJpEncodingAnalyzer = class(TdxEncodingAnalyzer)
  strict private class var
    FClassTable: TArray<Integer>;
    FStateTable: TArray<Integer>;
    FCharLenTable: TArray<Integer>;
    class constructor Initialize;
  protected
    function GetClassTable: TArray<Integer>; override;
    function GetStateTable: TArray<Integer>; override;
    function GetCharLenTable: TArray<Integer>; override;
  end;

  { TdxShiftedJisEncodingDetector }

  TdxShiftedJisEncodingDetector = class(TdxJapaneseEncodingDetector)
  protected
    function GetEncoding: TEncoding; override;
    function CreateDistributionAnalyzer: TdxCharacterDistributionAnalyzer; override;
    function CreateEncodingAnalyzer: TdxEncodingAnalyzer; override;
    function CreateContextAnalyzer: TdxJapaneseContextAnalyzer; override;
    procedure ContinueContextAnalysis(const ABuffer: TArray<Byte>; AFrom, ALength: Integer); override;
  end;

  { TdxShiftedJisCharDistributionAnalyzer }

  TdxShiftedJisCharDistributionAnalyzer = class(TdxJapaneseCharDistributionAnalyzer)
  protected
    function GetOrder(const AStr: TArray<Byte>; AOffset: Integer): Integer; override;
  end;

  { TdxShiftedJisContextAnalyzer }

  TdxShiftedJisContextAnalyzer = class(TdxJapaneseContextAnalyzer)
  protected
    function GetOrder(const AStr: TArray<Byte>; AOffset: Integer): Integer; override;
  end;

  { TdxShiftedJisEncodingAnalyzer }

  TdxShiftedJisEncodingAnalyzer = class(TdxEncodingAnalyzer)
  strict private class var
    FClassTable: TArray<Integer>;
    FStateTable: TArray<Integer>;
    FCharLenTable: TArray<Integer>;
    class constructor Initialize;
  protected
    function GetClassTable: TArray<Integer>; override;
    function GetStateTable: TArray<Integer>; override;
    function GetCharLenTable: TArray<Integer>; override;
  end;

  { TdxInternalEncodingDetector }

  TdxInternalEncodingDetector = class
  strict private
    FLanguageFilter: TdxEncodingDetectorLanguages;
    FUnicodeDetector: TdxBomLessUnicodeGroupDetector;
    FDetectors: TdxList<TdxEncodingDetectorBase>;
    FDetectorState: TdxEncodingDetectorState;
    FStates: TdxList<TdxEncodingDetectorState>;
    procedure SetDetectorState(const Value: TdxEncodingDetectorState);
  protected
    function CreateDetectors: TdxList<TdxEncodingDetectorBase>; virtual;

    property Detectors: TdxList<TdxEncodingDetectorBase> read FDetectors;
    property UnicodeDetector: TdxBomLessUnicodeGroupDetector read FUnicodeDetector;
    property DetectorState: TdxEncodingDetectorState read FDetectorState write SetDetectorState;
  public
    constructor Create; overload;
    constructor Create(const ALanguageFilter: TdxEncodingDetectorLanguages); overload;
    destructor Destroy; override;
    procedure BeginDetection;
    function AnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): Boolean;
    function EndDetection: TEncoding;
    function Detect(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TEncoding; overload;
    function Detect(const ABuffer: TArray<Byte>): TEncoding; overload;
    function DetectStreamEncodingCore(AStream: TStream; AMaxByteCount: Integer): TEncoding;
    function Detect(AStream: TStream; AMaxByteCount: Integer; AKeepPosition: Boolean): TEncoding; overload;
    function Detect(AStream: TStream; AMaxByteCount: Integer): TEncoding; overload;
    function Detect(AStream: TStream): TEncoding; overload;

    property LanguageFilter: TdxEncodingDetectorLanguages read FLanguageFilter;
  end;

  { TdxEmptyEncoding }

  TdxEmptyEncoding = class(TEncoding)
  strict protected
    function GetChars(Bytes: PByte; ByteCount: Integer; Chars: PWideChar; CharCount: Integer): Integer; override;
    function GetCharCount(Bytes: PByte; ByteCount: Integer): Integer; override;
    function GetBytes(Chars: PWideChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; override;
    function GetByteCount(Chars: PChar; CharCount: Integer): Integer; override;
  public
    constructor Create;
    function GetByteCount(const AChars: TArray<Char>; AIndex: Integer; ACount: Integer): Integer; overload;
    function GetMaxByteCount(ACharCount: Integer): Integer; override;
    function GetMaxCharCount(AByteCount: Integer): Integer; override;
    function GetPreamble: System.TArray<System.Byte>; override;
  end;


{ TEncodingHelper }

function TEncodingHelper.GetDisplayName: string;
begin
  Result := TdxEncoding.GetDisplayName(Self);
end;

function TEncodingHelper.GetWebName: string;
begin
  Result := TdxEncoding.GetWebName(Self);
end;

function TEncodingHelper.CanBeLosslesslyEncoded(const AValue: string): Boolean;
var
  ABytes: TBytes;
  ADecodedValue: string;
begin
  ABytes := GetBytes(AValue);
  ADecodedValue := GetString(ABytes, 0, Length(ABytes));
  Result := AValue = ADecodedValue;
end;

{ TdxEncoding }

class constructor TdxEncoding.Initialize;
begin
  FCodePagesToEncoding := TDictionary<Cardinal, TEncoding>.Create(140);
  FCodePageToDisplayName := TDictionary<Cardinal, string>.Create(140);
  FWebNameToEncoding := TDictionary<string, TEncoding>.Create(140, TdxIStringComparer.Ordinal);
  FCodePageToWebName := TDictionary<Cardinal, string>.Create(140);
  PopulateEncodings;
end;

{$HINTS OFF}
class destructor TdxEncoding.Finalize;
var
  AEncoding: TEncoding;
  I: Integer;
begin
  for I := 0 to FEncodings.Count - 1 do
  begin
    AEncoding := FEncodings[I];
    if not TEncoding.IsStandardEncoding(AEncoding) then
      AEncoding.Free;
  end;
  FEncodings.Free;
  FCodePagesToEncoding.Free;
  FCodePageToDisplayName.Free;
  FWebNameToEncoding.Free;
  FCodePageToWebName.Free;
  FEmptyEncoding.Free;
{$IFNDEF DELPHIXE2}
  FANSIEncoding.Free;
{$ENDIF}
end;
{$HINTS ON}

class procedure TdxEncoding.PopulateEncodings;
begin
  FEncodings := TdxList<TEncoding>.Create;
  FEncodings.Capacity := 140;
  AddEncoding(   37, 'IBM037', 'IBM EBCDIC (US-Canada)');
  AddEncoding(  437, 'IBM437', 'OEM United States');
  AddEncoding(  500, 'IBM500', 'IBM EBCDIC (International)');
  AddEncoding(  708, 'ASMO-708', 'Arabic (ASMO 708)');
  AddEncoding(  720, 'DOS-720', 'Arabic (DOS)');
  AddEncoding(  737, 'ibm737', 'Greek (DOS)');
  AddEncoding(  775, 'ibm775', 'Baltic (DOS)');
  AddEncoding(  850, 'ibm850', 'Western European (DOS)');
  AddEncoding(  852, 'ibm852', 'Central European (DOS)');
  AddEncoding(  855, 'IBM855', 'OEM Cyrillic');
  AddEncoding(  857, 'ibm857', 'Turkish (DOS)');
  AddEncoding(  858, 'IBM00858', 'OEM Multilingual Latin I');
  AddEncoding(  860, 'IBM860', 'Portuguese (DOS)');
  AddEncoding(  861, 'ibm861', 'Icelandic (DOS)');
  AddEncoding(  862, 'DOS-862', 'Hebrew (DOS)');
  AddEncoding(  863, 'IBM863', 'French Canadian (DOS)');
  AddEncoding(  864, 'IBM864', 'Arabic (864)');
  AddEncoding(  865, 'IBM865', 'Nordic (DOS)');
  AddEncoding(  866, 'cp866', 'Cyrillic (DOS)');
  AddEncoding(  869, 'ibm869', 'Greek, Modern (DOS)');
  AddEncoding(  870, 'IBM870', 'IBM EBCDIC (Multilingual Latin-2)');
  AddEncoding(  874, 'windows-874', 'Thai (Windows)');
  AddEncoding(  875, 'cp875', 'IBM EBCDIC (Greek Modern)');
  AddEncoding(  932, 'shift_jis', 'Japanese (Shift-JIS)');
  AddEncoding(  936, 'gb2312', 'Chinese Simplified (GB2312)');
  AddEncoding(  949, 'ks_c_5601-1987', 'Korean');
  AddEncoding(  950, 'big5', 'Chinese Traditional (Big5)');
  AddEncoding( 1026, 'IBM1026', 'IBM EBCDIC (Turkish Latin-5)');
  AddEncoding( 1047, 'IBM01047', 'IBM Latin-1');
  AddEncoding( 1140, 'IBM01140', 'IBM EBCDIC (US-Canada-Euro)');
  AddEncoding( 1141, 'IBM01141', 'IBM EBCDIC (Germany-Euro)');
  AddEncoding( 1142, 'IBM01142', 'IBM EBCDIC (Denmark-Norway-Euro)');
  AddEncoding( 1143, 'IBM01143', 'IBM EBCDIC (Finland-Sweden-Euro)');
  AddEncoding( 1144, 'IBM01144', 'IBM EBCDIC (Italy-Euro)');
  AddEncoding( 1145, 'IBM01145', 'IBM EBCDIC (Spain-Euro)');
  AddEncoding( 1146, 'IBM01146', 'IBM EBCDIC (UK-Euro)');
  AddEncoding( 1147, 'IBM01147', 'IBM EBCDIC (France-Euro)');
  AddEncoding( 1148, 'IBM01148', 'IBM EBCDIC (International-Euro)');
  AddEncoding( 1149, 'IBM01149', 'IBM EBCDIC (Icelandic-Euro)');
  AddEncoding( Unicode, 'utf-16', 'Unicode');
  AddEncoding( BigEndianUnicode, 'utf-16BE',                'Unicode (Big-Endian)');
  AddEncoding( 1250, 'windows-1250', 'Central European (Windows)');
  AddEncoding( 1251, 'windows-1251', 'Cyrillic (Windows)');
  AddEncoding( 1252, 'Windows-1252', 'Western European (Windows)');
  AddEncoding( 1253, 'windows-1253', 'Greek (Windows)');
  AddEncoding( 1254, 'windows-1254', 'Turkish (Windows)');
  AddEncoding( 1255, 'windows-1255', 'Hebrew (Windows)');
  AddEncoding( 1256, 'windows-1256', 'Arabic (Windows)');
  AddEncoding( 1257, 'windows-1257', 'Baltic (Windows)');
  AddEncoding( 1258, 'windows-1258', 'Vietnamese (Windows)');
  AddEncoding( 1361, 'Johab', 'Korean (Johab)');
  AddEncoding(10000, 'macintosh', 'Western European (Mac)');
  AddEncoding(10001, 'x-mac-japanese', 'Japanese (Mac)');
  AddEncoding(10002, 'x-mac-chinesetrad', 'Chinese Traditional (Mac)');
  AddEncoding(10003, 'x-mac-korean', 'Korean (Mac)');
  AddEncoding(10004, 'x-mac-arabic', 'Arabic (Mac)');
  AddEncoding(10005, 'x-mac-hebrew', 'Hebrew (Mac)');
  AddEncoding(10006, 'x-mac-greek', 'Greek (Mac)');
  AddEncoding(10007, 'x-mac-cyrillic', 'Cyrillic (Mac)');
  AddEncoding(10008, 'x-mac-chinesesimp', 'Chinese Simplified (Mac)');
  AddEncoding(10010, 'x-mac-romanian', 'Romanian (Mac)');
  AddEncoding(10017, 'x-mac-ukrainian', 'Ukrainian (Mac)');
  AddEncoding(10021, 'x-mac-thai', 'Thai (Mac)');
  AddEncoding(10029, 'x-mac-ce', 'Central European (Mac)');
  AddEncoding(10079, 'x-mac-icelandic', 'Icelandic (Mac)');
  AddEncoding(10081, 'x-mac-turkish', 'Turkish (Mac)');
  AddEncoding(10082, 'x-mac-croatian', 'Croatian (Mac)');
  AddEncoding(12000, 'utf-32', 'Unicode (UTF-32)');
  AddEncoding(12001, 'utf-32BE', 'Unicode (UTF-32 Big-Endian)');
  AddEncoding(20000, 'x-Chinese-CNS', 'Chinese Traditional (CNS)');
  AddEncoding(20001, 'x-cp20001', 'TCA Taiwan');
  AddEncoding(20002, 'x-Chinese-Eten', 'Chinese Traditional (Eten)');
  AddEncoding(20003, 'x-cp20003', 'IBM5550 Taiwan');
  AddEncoding(20004, 'x-cp20004', 'TeleText Taiwan');
  AddEncoding(20005, 'x-cp20005', 'Wang Taiwan');
  AddEncoding(20105, 'x-IA5', 'Western European (IA5)');
  AddEncoding(20106, 'x-IA5-German', 'German (IA5)');
  AddEncoding(20107, 'x-IA5-Swedish', 'Swedish (IA5)');
  AddEncoding(20108, 'x-IA5-Norwegian', 'Norwegian (IA5)');
  AddEncoding(20127, 'us-ascii', 'US-ASCII');
  AddEncoding(20261, 'x-cp20261', 'T.61');
  AddEncoding(20269, 'x-cp20269', 'ISO-6937');
  AddEncoding(20273, 'IBM273', 'IBM EBCDIC (Germany)');
  AddEncoding(20277, 'IBM277', 'IBM EBCDIC (Denmark-Norway)');
  AddEncoding(20278, 'IBM278', 'IBM EBCDIC (Finland-Sweden)');
  AddEncoding(20280, 'IBM280', 'IBM EBCDIC (Italy)');
  AddEncoding(20284, 'IBM284', 'IBM EBCDIC (Spain)');
  AddEncoding(20285, 'IBM285', 'IBM EBCDIC (UK)');
  AddEncoding(20290, 'IBM290', 'IBM EBCDIC (Japanese katakana)');
  AddEncoding(20297, 'IBM297', 'IBM EBCDIC (France)');
  AddEncoding(20420, 'IBM420', 'IBM EBCDIC (Arabic)');
  AddEncoding(20423, 'IBM423', 'IBM EBCDIC (Greek)');
  AddEncoding(20424, 'IBM424', 'IBM EBCDIC (Hebrew)');
  AddEncoding(20833, 'x-EBCDIC-KoreanExtended', 'IBM EBCDIC (Korean Extended)');
  AddEncoding(20838, 'IBM-Thai', 'IBM EBCDIC (Thai)');
  AddEncoding(20866, 'koi8-r', 'Cyrillic (KOI8-R)');
  AddEncoding(20871, 'IBM871', 'IBM EBCDIC (Icelandic)');
  AddEncoding(20880, 'IBM880', 'IBM EBCDIC (Cyrillic Russian)');
  AddEncoding(20905, 'IBM905', 'IBM EBCDIC (Turkish)');
  AddEncoding(20924, 'IBM00924', 'IBM Latin-1');
  AddEncoding(20932, 'EUC-JP', 'Japanese (JIS 0208-1990 and 0212-1990)');
  AddEncoding(20936, 'x-cp20936', 'Chinese Simplified (GB2312-80)');
  AddEncoding(20949, 'x-cp20949', 'Korean Wansung');
  AddEncoding(21025, 'cp1025', 'IBM EBCDIC (Cyrillic Serbian-Bulgarian)');
  AddEncoding(21866, 'koi8-u', 'Cyrillic (KOI8-U)');
  AddEncoding(28591, 'iso-8859-1', 'Western European (ISO)');
  AddEncoding(28592, 'iso-8859-2', 'Central European (ISO)');
  AddEncoding(28593, 'iso-8859-3', 'Latin 3 (ISO)');
  AddEncoding(28594, 'iso-8859-4', 'Baltic (ISO)');
  AddEncoding(28595, 'iso-8859-5', 'Cyrillic (ISO)');
  AddEncoding(28596, 'iso-8859-6', 'Arabic (ISO)');
  AddEncoding(28597, 'iso-8859-7', 'Greek (ISO)');
  AddEncoding(28598, 'iso-8859-8', 'Hebrew (ISO-Visual)');
  AddEncoding(28599, 'iso-8859-9', 'Turkish (ISO)');
  AddEncoding(28603, 'iso-8859-13', 'Estonian (ISO)');
  AddEncoding(28605, 'iso-8859-15', 'Latin 9 (ISO)');
  AddEncoding(29001, 'x-Europa', 'Europa');
  AddEncoding(38598, 'iso-8859-8-i', 'Hebrew (ISO-Logical)');
  AddEncoding(50220, 'iso-2022-jp', 'Japanese (JIS)');
  AddEncoding(50221, 'csISO2022JP', 'Japanese (JIS-Allow 1 byte Kana)');
  AddEncoding(50222, 'iso-2022-jp', 'Japanese (JIS-Allow 1 byte Kana - SO/SI)');
  AddEncoding(50225, 'iso-2022-kr', 'Korean (ISO)');
  AddEncoding(50227, 'x-cp50227', 'Chinese Simplified (ISO-2022)');
  AddEncoding(51932, 'euc-jp', 'Japanese (EUC)');
  AddEncoding(51936, 'EUC-CN', 'Chinese Simplified (EUC)');
  AddEncoding(51949, 'euc-kr', 'Korean (EUC)');
  AddEncoding(52936, 'hz-gb-2312', 'Chinese Simplified (HZ)');
  AddEncoding(54936, 'GB18030', 'Chinese Simplified (GB18030)');
  AddEncoding(57002, 'x-iscii-de', 'ISCII Devanagari');
  AddEncoding(57003, 'x-iscii-be', 'ISCII Bengali');
  AddEncoding(57004, 'x-iscii-ta', 'ISCII Tamil');
  AddEncoding(57005, 'x-iscii-te', 'ISCII Telugu');
  AddEncoding(57006, 'x-iscii-as', 'ISCII Assamese');
  AddEncoding(57007, 'x-iscii-or', 'ISCII Oriya');
  AddEncoding(57008, 'x-iscii-ka', 'ISCII Kannada');
  AddEncoding(57009, 'x-iscii-ma', 'ISCII Malayalam');
  AddEncoding(57010, 'x-iscii-gu', 'ISCII Gujarati');
  AddEncoding(57011, 'x-iscii-pa', 'ISCII Punjabi');
  AddEncoding(UTF7,  'utf-7', 'Unicode (UTF-7)');
  AddEncoding(UTF8,  'utf-8', 'Unicode (UTF-8)');
end;

class procedure TdxEncoding.AddEncoding(ACodePage: Integer; const AWebName, ADisplayName: string);
var
  AEncoding: TEncoding;
  ACPInfo: TCPInfo;
begin
  if not GetCPInfo(ACodePage, ACPInfo) then
    Exit;
  AEncoding := TEncoding.GetEncoding(ACodePage);
  AddEncoding(AEncoding, AWebName, ADisplayName);
end;

class procedure TdxEncoding.AddEncoding(AEncoding: TEncoding; const AWebName, ADisplayName: string);
var
  ACodePage: Cardinal;
begin
  FEncodings.Add(AEncoding);
  ACodePage := AEncoding.CodePage;
  FCodePagesToEncoding.Add(ACodePage, AEncoding);
  FCodePageToDisplayName.Add(ACodePage, ADisplayName);
  FWebNameToEncoding.AddOrSetValue(AWebName, AEncoding);
  FCodePageToWebName.Add(ACodePage, AWebName);
end;

class function TdxEncoding.GetASCII: TEncoding;
begin
  Result := TEncoding.ASCII;
end;

class function TdxEncoding.GetDefault: TEncoding;
begin
  Result := TEncoding.Default;
end;

class function TdxEncoding.GetBigEndianUnicode: TEncoding;
begin
  Result := TEncoding.BigEndianUnicode;
end;

class function TdxEncoding.GetUnicode: TEncoding;
begin
  Result := TEncoding.Unicode;
end;

class function TdxEncoding.GetUTF7: TEncoding;
begin
  Result := TEncoding.UTF7;
end;

class function TdxEncoding.GetUTF8: TEncoding;
begin
  Result := TEncoding.UTF8;
end;

{$IFNDEF DELPHIXE2}
function InterlockedCompareExchangePointer(var Destination: Pointer; Exchange: Pointer; Comparand: Pointer): Pointer;
asm
  XCHG    EAX,ECX
  LOCK CMPXCHG [ECX],EDX
end;

class function TdxEncoding.GetANSI: TEncoding;
var
  LEncoding: TEncoding;
begin
  if FANSIEncoding = nil then
  begin
    LEncoding := TMBCSEncoding.Create(GetACP, 0, 0);
    if InterlockedCompareExchangePointer(Pointer(FANSIEncoding), Pointer(LEncoding), nil) <> nil then
      LEncoding.Free;
  {$IFDEF AUTOREFCOUNT}
    FANSIEncoding.__ObjAddRef;
  {$ENDIF AUTOREFCOUNT}
  end;
  Result := FANSIEncoding;
end;
{$ELSE}
class function TdxEncoding.GetANSI: TEncoding;
begin
  Result := TEncoding.ANSI;
end;
{$ENDIF}

class function TdxEncoding.CharsetFromCodePage(ACodePage: Integer): Integer;
begin
  Result := TdxCharsetAndCodePageTranslator.Instance.CharsetFromCodePage(ACodePage);
end;

class function TdxEncoding.CodePageFromCharset(ACharset: Integer): Integer;
begin
  Result := TdxCharsetAndCodePageTranslator.Instance.CodePageFromCharset(ACharset);
end;

class function TdxEncoding.DetectEncoding(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TEncoding;
var
  AEncodingDetector: TdxInternalEncodingDetector;
begin
  AEncodingDetector := TdxInternalEncodingDetector.Create;
  try
    Result := AEncodingDetector.Detect(ABuffer, AFrom, ALength);
  finally
    AEncodingDetector.Free;
  end;
end;

class function TdxEncoding.DetectEncoding(AStream: TStream): TEncoding;
var
  AEncodingDetector: TdxInternalEncodingDetector;
begin
  AEncodingDetector := TdxInternalEncodingDetector.Create;
  try
    Result := AEncodingDetector.Detect(AStream);
  finally
    AEncodingDetector.Free;
  end;
end;

class function TdxEncoding.GetEncoding(ACodePage: Cardinal): TEncoding;
const
  SymbolCodePage = 42;
begin
  if ACodePage = SymbolCodePage then
    Exit(Empty);
  if ACodePage = CP_ACP then
    ACodePage := GetACP;
  if not FCodePagesToEncoding.TryGetValue(ACodePage, Result) then
    raise EInvalidArgument.Create('CodePage');
end;

class function TdxEncoding.GetEmpty: TEncoding;
begin
  if FEmptyEncoding = nil then
    FEmptyEncoding := TdxEmptyEncoding.Create;
  Result := FEmptyEncoding;
end;

class function TdxEncoding.GetEncoding(const AWebName: string): TEncoding;
var
  ACode: Integer;
  ACodePage: Cardinal;
begin
  if not FWebNameToEncoding.TryGetValue(AWebName, Result) then
  begin
    if Copy(AWebName, 1, 2) = 'cp' then // do not localize
    begin
      Val(Copy(AWebName, 3, Length(AWebName) - 2), ACodePage, ACode);
      if ACode = 0 then
        Result := GetEncoding(ACodePage)
      else
        Result := nil;
    end;
    Result := nil;
  end;
end;

class function TdxEncoding.GetEncodingCodePage(AEncoding: TEncoding): Integer;
begin
  Result := AEncoding.CodePage;
end;

class function TdxEncoding.GetEncodings: TArray<TEncoding>;
begin
  Result := FEncodings.ToArray;
end;

class function TdxEncoding.GetDisplayName(AEncoding: TEncoding): string;
begin
  if AEncoding = nil then
    raise EInvalidArgument.Create('WebName');
  if not FCodePageToDisplayName.TryGetValue(AEncoding.CodePage, Result) then
    Result := '';
end;

class function TdxEncoding.GetWebName(AEncoding: TEncoding): string;
begin
  if AEncoding = nil then
    raise EInvalidArgument.Create('WebName');
  if not FCodePageToWebName.TryGetValue(AEncoding.CodePage, Result) then
    Result := '';
end;

{ TdxCharsetAndCodePageTranslator }

{$HINTS OFF}
class destructor TdxCharsetAndCodePageTranslator.Finalize;
begin
  ClearInstance;
end;
{$HINTS ON}

class function TdxCharsetAndCodePageTranslator.GetInstance: TdxCharsetAndCodePageTranslator;
begin
  if FInstance = nil then
    FInstance := CreateInstance;
  Result := FInstance;
end;

class procedure TdxCharsetAndCodePageTranslator.ClearInstance;
begin
  FreeAndNil(FInstance);
end;

class function TdxCharsetAndCodePageTranslator.CreateInstance: TdxCharsetAndCodePageTranslator;
begin
    Result := TdxFullTrustCharsetAndCodePageTranslator.Create
end;

{ TdxFullTrustCharsetAndCodePageTranslator }

class constructor TdxFullTrustCharsetAndCodePageTranslator.Initialize;
begin
  FCharsetToCodePage := TDictionary<Cardinal, Cardinal>.Create;
  FCodePageToCharset := TDictionary<Cardinal, Cardinal>.Create;
end;

{$HINTS OFF}
class destructor TdxFullTrustCharsetAndCodePageTranslator.Finalize;
begin
  FreeAndNil(FCharsetToCodePage);
  FreeAndNil(FCodePageToCharset);
end;
{$HINTS ON}

function TdxFullTrustCharsetAndCodePageTranslator.CodePageFromCharsetCore(ACharset: Cardinal): Cardinal;
var
  ACharSetInfo: TCharsetInfo;
  ASrc: DWORD;
begin
  ASrc := ACharset;
  ACharSetInfo := TCharsetInfo.Create(0);
  if not TranslateCharsetInfo(ASrc, ACharSetInfo, TCI_SRCCHARSET) then
    Exit(TdxEncoding.Default.CodePage);
  Result := ACharSetInfo.ciACP;
end;

function TdxFullTrustCharsetAndCodePageTranslator.CharsetFromCodePageCore(ACodePage: Cardinal): Cardinal;
var
  ACharSetInfo: TCharsetInfo;
  ASrc: DWORD;
begin
  ASrc := ACodePage;
  ACharSetInfo := TCharsetInfo.Create(0);
  if not TranslateCharsetInfo(ASrc, ACharSetInfo, TCI_SRCCODEPAGE) then
    Exit(0);
  Result := ACharSetInfo.ciCharset;
end;

function TdxFullTrustCharsetAndCodePageTranslator.CodePageFromCharset(ACharset: Cardinal): Cardinal;
begin
  if not FCharsetToCodePage.TryGetValue(ACharset, Result) then
  begin
    Result := CodePageFromCharsetCore(ACharset);
    FCharsetToCodePage.AddOrSetValue(ACharset, Result);
  end;
end;

function TdxFullTrustCharsetAndCodePageTranslator.CharsetFromCodePage(ACodePage: Cardinal): Cardinal;
begin
  if not FCodePageToCharset.TryGetValue(ACodePage, Result) then
  begin
    Result := CharsetFromCodePageCore(ACodePage);
    FCodePageToCharset.AddOrSetValue(ACodePage, Result);
  end;
end;

{ TdxCharsetInfoHelper }

class function TdxCharsetInfoHelper.Create(ACharset: Cardinal): TCharsetInfo;
begin
  Result.ciCharset := ACharset;
  Result.ciACP := CP_ACP;
end;

{ TdxEncodingDetectorState }

constructor TdxEncodingDetectorState.Create(ADetector: TdxInternalEncodingDetector);
begin
  inherited Create;
  FDetector := ADetector;
end;

{ TdxInitialEncodingDetectorState }

function TdxInitialEncodingDetectorState.AnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): Boolean;
var
  AEncoding: TEncoding;
begin
  if ALength <= 0 then
    Exit(False);

  AEncoding := TryAnalyseUnicodeSignature(ABuffer, AFrom, ALength);
  if AEncoding <> nil then
  begin
    Detector.DetectorState := TdxFinalEncodingDetectorState.Create(Detector, AEncoding);
    Exit(True);
  end
  else
  begin
    Detector.DetectorState := TdxPureAsciiEncodingDetectorState.Create(Detector);
    Exit(Detector.AnalyseData(ABuffer, AFrom, ALength));
  end;
end;

function TdxInitialEncodingDetectorState.CalculateResult: TEncoding;
begin
  Result := nil;
end;

function TdxInitialEncodingDetectorState.TryAnalyseUnicodeSignature(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TEncoding;
begin
  if ALength <= 3 then
    Exit(nil);

  case ABuffer[AFrom] of
    $EF:
      if ($BB = ABuffer[AFrom + 1]) and ($BF = ABuffer[AFrom + 2]) then
        Exit(TdxEncoding.UTF8);
    $FE:
      if (($FF = ABuffer[AFrom + 1]) and (0 = ABuffer[AFrom + 2])) and (0 = ABuffer[AFrom + 3]) then
        Exit(nil)
      else
        if $FF = ABuffer[AFrom + 1] then
          Exit(TdxEncoding.GetEncoding(1201));
    $00:
      if ((0 = ABuffer[AFrom + 1]) and ($FE = ABuffer[AFrom + 2])) and ($FF = ABuffer[AFrom + 3]) then
        Exit(TdxEncoding.GetEncoding(12001))
      else
        if ((0 = ABuffer[AFrom + 1]) and ($FF = ABuffer[AFrom + 2])) and ($FE = ABuffer[AFrom + 3]) then
          Exit(nil);
    $FF:
      if (($FE = ABuffer[AFrom + 1]) and (0 = ABuffer[AFrom + 2])) and (0 = ABuffer[AFrom + 3]) then

      else
        if $FE = ABuffer[AFrom + 1] then
          Exit(TdxEncoding.Unicode);
    else
      Exit(nil);
  end;

  Result := nil;
end;

{ TdxPureAsciiEncodingDetectorState }

function TdxPureAsciiEncodingDetectorState.AnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): Boolean;
var
  ATo, I: Integer;
  AResult: TdxDetectionResult;
begin
  ATo := AFrom + ALength;
  for I := AFrom to ATo - 1 do
  begin
    if ((ABuffer[I] and $80) <> 0) and (ABuffer[I] <> $A0) then
    begin
      Detector.DetectorState := TdxNormalEncodingDetectorState.Create(Detector);
      Exit(Detector.AnalyseData(ABuffer, AFrom, ALength));
    end;
  end;
  AResult := Detector.UnicodeDetector.AnalyseData(ABuffer, AFrom, ALength);
  if AResult = TdxDetectionResult.Positive then
  begin
    Detector.DetectorState := TdxFinalEncodingDetectorState.Create(Detector, Detector.UnicodeDetector.Encoding);
    Result := True;
  end
  else
    Result := False;
end;

function TdxPureAsciiEncodingDetectorState.CalculateResult: TEncoding;
begin
  Result := nil;
end;

{ TdxNormalEncodingDetectorState }

function TdxNormalEncodingDetectorState.AnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): Boolean;
var
  ADetectors: TdxList<TdxEncodingDetectorBase>;
  ACount, I: Integer;
  AResult: TdxDetectionResult;
begin
  ADetectors := Detector.Detectors;
  ACount := ADetectors.Count;
  for I := 0 to ACount - 1 do
  begin
    AResult := ADetectors[I].AnalyseData(ABuffer, AFrom, ALength);
    if AResult = TdxDetectionResult.Positive then
    begin
      Detector.DetectorState := TdxFinalEncodingDetectorState.Create(Detector, ADetectors[I].Encoding);
      Exit(True);
    end;
  end;

  Result := False;
end;

function TdxNormalEncodingDetectorState.CalculateResult: TEncoding;
var
  ADetectors: TdxList<TdxEncodingDetectorBase>;
  AMaxConfidence, AConfidence, AMinimumConfidence: Single;
  ABestDetectorIndex, ACount, I: Integer;
begin
  ADetectors := Detector.Detectors;
  AMaxConfidence := 0.0;
  ABestDetectorIndex := 0;
  ACount := ADetectors.Count;
  for I := 0 to ACount - 1 do
  begin
    AConfidence := ADetectors[I].GetConfidence;
    if AConfidence > AMaxConfidence then
    begin
      AMaxConfidence := AConfidence;
      ABestDetectorIndex := I;
    end;
  end;

  AMinimumConfidence := 0.20;
  if AMaxConfidence > AMinimumConfidence then
    Detector.DetectorState := TdxFinalEncodingDetectorState.Create(Detector, ADetectors[ABestDetectorIndex].Encoding)
  else
    Detector.DetectorState := TdxFinalEncodingDetectorState.Create(Detector, nil);

  Result := Detector.DetectorState.CalculateResult;
end;

{ TdxFinalEncodingDetectorState }

constructor TdxFinalEncodingDetectorState.Create(ADetector: TdxInternalEncodingDetector; AResult: TEncoding);
begin
  inherited Create(ADetector);
  FResult := AResult;
end;

function TdxFinalEncodingDetectorState.AnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): Boolean;
begin
  Result := True;
end;

function TdxFinalEncodingDetectorState.CalculateResult: TEncoding;
begin
  Result := FResult;
end;

{ TdxEncodingDetectorBase }

function TdxEncodingDetectorBase.AnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult;
begin
  if CurrentResult <> TdxDetectionResult.Detecting then
    Exit(CurrentResult);
  if ALength <= 0 then
    Exit(CurrentResult);
  FCurrentResult := ForceAnalyseData(ABuffer, AFrom, ALength);
  Result := CurrentResult;
end;

function TdxEncodingDetectorBase.AppendBytes(const ASource: TArray<Byte>; AFrom, ATo: Integer;
  ATarget: TList<Byte>): Integer;
begin
  while AFrom < ATo do
  begin
    ATarget.Add(ASource[AFrom]);
    Inc(AFrom);
  end;
  Result := AFrom;
end;

function TdxEncodingDetectorBase.IsUpperAsciiByte(ACurrentByte: Byte): Boolean;
begin
  Result := (ACurrentByte and $80) <> 0;
end;

function TdxEncodingDetectorBase.IsNonEnglishLetterLowerAsciiByte(ACurrentByte: Byte): Boolean;
begin
  Result := not CharInSet(AnsiChar(ACurrentByte), ['A'..'Z', 'a'..'z']);
end;

{ TdxGroupDetector }

constructor TdxGroupDetector.Create;
begin
  inherited Create;
  FBestMatchDetectorIndex := -1;
end;

destructor TdxGroupDetector.Destroy;
begin
  FDetectors.Free;
  inherited Destroy;
end;

procedure TdxGroupDetector.CreateDetectors;
begin
  FDetectors := TdxObjectList<TdxEncodingDetectorBase>.Create;
  PopulateDetectors;
  SetLength(FIsDetectorDisabled, Detectors.Count);
  FActiveDetectorCount := Detectors.Count;
end;

function TdxGroupDetector.GetEncoding: TEncoding;
begin
  if FBestMatchDetectorIndex = -1 then
  begin
    GetConfidence;
    if FBestMatchDetectorIndex = -1 then
      Exit(Detectors[0].Encoding);
  end;
  Result := Detectors[FBestMatchDetectorIndex].Encoding;
end;

function TdxGroupDetector.GetConfidence: Single;
var
  AConfidence: Single;
  I: Integer;
begin
  case CurrentResult of
    TdxDetectionResult.Positive:
      Result := 0.99;
    TdxDetectionResult.Negative:
      Result := 0.01;
    else
      Result := 0.0;
      for I := 0 to Detectors.Count - 1 do
      begin
        if FIsDetectorDisabled[I] then
          Continue;
        AConfidence := Detectors[I].GetConfidence;
        if Result < AConfidence then
        begin
          Result := AConfidence;
          FBestMatchDetectorIndex := I;
        end;
      end;
  end;
end;

function TdxGroupDetector.AnalyseDataCore(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult;
var
  I: Integer;
  AResult: TdxDetectionResult;
begin
  if CurrentResult = TdxDetectionResult.Negative then
    Exit(CurrentResult);
  if ALength = 0 then
    Exit(CurrentResult);

  for I := 0 to Detectors.Count - 1 do
  begin
    if FIsDetectorDisabled[I] then
      Continue;
    AResult := Detectors[I].AnalyseData(ABuffer, AFrom, ALength);
    if AResult = TdxDetectionResult.Positive then
    begin
      FBestMatchDetectorIndex := I;
      Exit(TdxDetectionResult.Positive);
    end
    else
      if AResult = TdxDetectionResult.Negative then
      begin
        FIsDetectorDisabled[I] := True;
        Dec(FActiveDetectorCount);
        if FActiveDetectorCount <= 0 then
        begin
          Exit(TdxDetectionResult.Negative);
        end;
      end;
  end;
  Result := CurrentResult;
end;

{ TdxSingleByteCharsetGroupDetector }

constructor TdxSingleByteCharsetGroupDetector.Create;
begin
  inherited Create;
  CreateDetectors;
end;

procedure TdxSingleByteCharsetGroupDetector.PopulateDetectors;
begin
  Detectors.Add(TdxWin1251EncodingDetector.Create);
  Detectors.Add(TdxKoi8rEncodingDetector.Create);
  Detectors.Add(TdxLatin5EncodingDetector.Create);
  Detectors.Add(TdxMacCyrillicDetector.Create);
  Detectors.Add(TdxIbm866EncodingDetector.Create);
  Detectors.Add(TdxIbm855EncodingDetector.Create);
  Detectors.Add(TdxLatin7EncodingDetector.Create);
  Detectors.Add(TdxWin1253EncodingDetector.Create);
  Detectors.Add(TdxLatin5BulgarianEncodingDetector.Create);
  Detectors.Add(TdxWin1251BulgarianEncodingDetector.Create);
end;

function TdxSingleByteCharsetGroupDetector.ForceAnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult;
var
  AFilteredBuffer: TArray<Byte>;
begin
  AFilteredBuffer := FilterBuffer(ABuffer, AFrom, ALength);
  Result := AnalyseDataCore(AFilteredBuffer, 0, Length(AFilteredBuffer));
end;

function TdxSingleByteCharsetGroupDetector.FilterBuffer(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TArray<Byte>;
var
  ABytes: TList<Byte>;
  AMeetUpperAscii: Boolean;
  ATo, AIndex, I: Integer;
  ACurrentByte: Byte;
begin
  ABytes := TList<Byte>.Create;
  try
    AMeetUpperAscii := False;
    ATo := AFrom + ALength;

    AIndex := AFrom;
    for I := AFrom to ATo - 1 do
    begin
      ACurrentByte := ABuffer[I];
      if IsUpperAsciiByte(ACurrentByte) then
      begin
        AMeetUpperAscii := True;
      end
      else
        if IsNonEnglishLetterLowerAsciiByte(ACurrentByte) then
        begin
          if AMeetUpperAscii and (I > AIndex) then
          begin
            AIndex := 1 + AppendBytes(ABuffer, AIndex, I, ABytes);
            ABytes.Add($20);
            AMeetUpperAscii := False;
          end
          else
            AIndex := I + 1;
        end;
    end;
    if AMeetUpperAscii then
      AppendBytes(ABuffer, AIndex, ATo, ABytes);

    Result := ABytes.ToArray;
  finally
    ABytes.Free;
  end;
end;

{ TdxSingleByteEncodingDetector }

constructor TdxSingleByteEncodingDetector.Create;
begin
  Assert(CharacterToOrderMap <> nil);
  Assert(Length(CharacterToOrderMap) = 256);
  Assert(PrecedenceMatrix <> nil);
  Assert(Length(PrecedenceMatrix) = 256);

  FLastCharacterOrder := 255;
  SetLength(FSequenceCounters, SequenceCategoryCount);
end;

function TdxSingleByteEncodingDetector.CalculatePrecedenceMatrixIndex(AOrder: Byte): Integer;
begin
  Result := FLastCharacterOrder * SampleSize + AOrder;
end;

function TdxSingleByteEncodingDetector.GetPrecedenceValue(AIndex: Integer): Integer;
begin
  Result := Unpack2Bits(AIndex, PrecedenceMatrix);
end;

function TdxSingleByteEncodingDetector.GetConfidence: Single;
var
  ARatio: Single;
begin
  if FTotalSequenceCount > 0 then
  begin
    ARatio := 1.0 * FSequenceCounters[PositiveCategoryIndex] / FTotalSequenceCount / PositiveRatio;
    ARatio := ARatio * FFrequentCharCount / FTotalCharCount;
    if ARatio >= 1.00 then
      ARatio := 0.99;
    Exit(ARatio);
  end;
  Result := 0.01;
end;

function TdxSingleByteEncodingDetector.ForceAnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult;
var
  ATo, I: Integer;
  AOrder: Byte;
  AConfidence: Single;
begin
  ATo := AFrom + ALength;
  for I := AFrom to ATo - 1 do
  begin
    AOrder := CharacterToOrderMap[ABuffer[I]];
    if AOrder < SymbolCategoryOrder then
      Inc(FTotalCharCount);
    if AOrder < SampleSize then
    begin
      Inc(FFrequentCharCount);
      if FLastCharacterOrder < SampleSize then
      begin
        Inc(FTotalSequenceCount);
        Inc(FSequenceCounters[GetPrecedenceValue(CalculatePrecedenceMatrixIndex(AOrder))]);
      end;
    end;
    FLastCharacterOrder := AOrder;
  end;

  if CurrentResult = TdxDetectionResult.Detecting then
  begin
    if FTotalSequenceCount > EnoughSequencesThreshold then
    begin
      AConfidence := GetConfidence;
      if AConfidence > PositiveShortcutThreshold then
        Exit(TdxDetectionResult.Positive)
      else
        if AConfidence < NegativeShortcutThreshold then
          Exit(TdxDetectionResult.Negative);
    end;
  end;

  Result := CurrentResult;
end;

class function TdxSingleByteEncodingDetector.Unpack2Bits(I: Integer; const ABuffer: TArray<Integer>): Integer;
var
  AValue: Integer;
begin
  AValue := I;
  Result := (ABuffer[AValue shr 4] shr ((AValue and $F) shl 1)) and $00000003;
end;

{ TdxLatin1EncodingDetector }

constructor TdxLatin1EncodingDetector.Create;
begin
  SetLength(FFrequencyCounters, FrequentCategoryCount);
  FLastCharacterClass := Oth;
end;

function TdxLatin1EncodingDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.GetEncoding(1252);
end;

function TdxLatin1EncodingDetector.ForceAnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult;
var
  AFilteredBuffer: TArray<Byte>;
  AFilteredBufferLength, I: Integer;
  ACharClass, AFreq: Byte;
begin
  AFilteredBuffer := FilterBuffer(ABuffer, AFrom, ALength);
  AFilteredBufferLength := Length(AFilteredBuffer);

  for I := 0 to AFilteredBufferLength - 1 do
  begin
    ACharClass := Latin1_CharToClass[AFilteredBuffer[I]];
    AFreq := Model[FLastCharacterClass * ClassCount + ACharClass];
    if AFreq = 0 then
      Exit(TdxDetectionResult.Negative);
    Inc(FFrequencyCounters[AFreq]);
    FLastCharacterClass := ACharClass;
  end;

  Result := CurrentResult;
end;

function TdxLatin1EncodingDetector.GetConfidence: Single;
var
  ATotal, I: Integer;
begin
  if CurrentResult = TdxDetectionResult.Negative then
    Exit(0.01);

  ATotal := 0;
  for I := 0 to FrequentCategoryCount - 1 do
    Inc(ATotal, FFrequencyCounters[I]);

  if ATotal <= 0 then
    Result := 0.0
  else
  begin
    Result := FFrequencyCounters[3] * 1.0 / ATotal;
    Result := Result - FFrequencyCounters[1] * 20.0 / ATotal;
  end;

  if Result < 0.0 then
    Result := 0.0;

  Result := Result * 0.50;
end;

function TdxLatin1EncodingDetector.FilterBuffer(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TArray<Byte>;
var
  AResult: TList<Byte>;
  ATo, AIndex, I: Integer;
  ACurrentByte: Byte;
begin
  AResult := TList<Byte>.Create;
  try
    ATo := AFrom + ALength;
    AIndex := AFrom;
    I := AFrom;
    while I < ATo do
    begin
      ACurrentByte := ABuffer[I];
      if not IsUpperAsciiByte(ACurrentByte) and IsNonEnglishLetterLowerAsciiByte(ACurrentByte) then
      begin
        if I > AIndex then
        begin
          AIndex := 1 + AppendBytes(ABuffer, AIndex, I, AResult);
          AResult.Add($20);
        end
        else
          AIndex := I + 1;
      end;
      Inc(I);
    end;
    AppendBytes(ABuffer, AIndex, I, AResult);

    Result := AResult.ToArray;
  finally
    AResult.Free;
  end;
end;

{ TdxCyrillicEncodingDetector }

class constructor TdxCyrillicEncodingDetector.Initialize;
begin
  FPrecedenceMatrix := TArray<Integer>.Create(
             -4,  -71335977,         -1,-2102734849, -268435521,  788529091,      32768,          0,
     -268435777,  532676547,      32768,          0,        -69, 1879048131,    1081344,          0,
     -268435457, 1879048131,      33792,          0, 2112552959,  468335359,      32768,       4096,
    -1157632001,  715729343,      32768,       8192,         -1,  653933247,      32768,       8256,
    -1159204865,  367737599,      32768,          0,-1074069505,  717171455,      32768,          0,
      -17825793,  443362239,      16384,       8192,  901775359,  308942421,      16384,          0,
    -1230245889,  644164027,      32768,          0,-1157627905,  397028031,      16384,       8192,
     -536871237,  788512706,      32768,          0,-1315983362,  373564475,      16384,          0,
    -1610875341,  247180162,      16384,          0,-1947931077,  450702211,      32768,          0,
     -603980233,  171900546,          0,          0, 1051701247,  290727252,      16384,          0,
     1073721343,  289812219,          0,       8192, 2058350591,  497686907,        256,       8192,
      836403194,    1643532,      16384,          0, 1587215723,  374879874,      32768,          0,
     2107285499,  289548697,      16384,          0,-1293193218,  290722892,      32768,          0,
     1719578607,  290984260,      16384,       4096,-1902446185,  514485889,      16384,          0,
      900012031,   21235060,          0,          0,  555224054,          8,          0,          0,
    -1701136366, 1347716417,   67145729,       1024,  805569534, 1347420185, 1415907601,  336610641,
    -1431668038, 1632703829, 1431721301,  341070101,-1436882241, 1628710517,   72685909,  335810821,
    -1717916606,-2060883584,  425283925, 1090864473,  537810605, 1342439450,  139543633,  335544581,
      538617582, 1615855636,   68225125,    1048901,-1431590845,-1876338304,  357929301, 1096107349,
      553995246, 1614807157, 1162941781,  268453125, 1632282622,  541332581,      32768,          0,
      543769342, 1681916981, 1146164517,  339738901,  556402413, 1616052501,   72353045,  336658710,
     1516939623, 1147406976,  357913942, 1095844950, 1163216899, 1142179136,  273744133, 1077952529,
      604635837, 1614807077,  340792340,  268435717,  538182382, 1346437154, 1432688933,  341131542,
      689571501, 1346371588,   85202000,        261, 1521121345, 1075151936,   67457349,   16777220,
     1248248866,-2007662208,  425022806, 1091911765,  536938153,  546373717,  273750353,  339804437,
      537211558, 1073741828,    1115456,   67109121,  421858025, 1073742865,  336860180,  335544581,
     1248503386, 1700162112,  352671061, 1095057429,  537531049,  536870916,   84935696,        261,
             32,    4194306,          0,          0,  287965865, 1342177284,    1053716,          4,
      621019817, 1082197056,    1311760,        321,-1588189783, 1343488000,    1049872,        261,
      269484641,    4194320,    1049872,  268435713, 1162233857, 1074857024,   84214784, 1073741840,
             26, 1073741824,     327937,    1064964,          2, 1073741824,   22286613,    4457473,
              1, 1073741824,  335614277,   18104325,  268435796,          0,    1049600,   67108865);
end;

function TdxCyrillicEncodingDetector.GetPositiveRatio: Single;
begin
  Result := 0.976601;
end;

function TdxCyrillicEncodingDetector.GetPrecedenceMatrix: TArray<Integer>;
begin
  Result := FPrecedenceMatrix;
end;

{ TdxBulgarianEncodingDetector }

class constructor TdxBulgarianEncodingDetector.Initialize;
begin
  FPrecedenceMatrix := TArray<Integer>.Create(
             -4,   -1048581,         -1,-1497694977,       -265, 1291845619,   84934656, 1073741824,
             -1,  486539195,   72351748, 1073741825,        -85,  218103735,   68157440,          0,
           -261,  218103735,   68157440,          0, 1807745023,   71920383,    2097152, 1073742336,
    -1746157569,  210354159,    2113536, 1073742336,-1140883457,  147717119,    2097152, 1073742080,
     2143158271,  143141615,    2097152, 1073742336, -336330753,    9006751,   69206017, 1073742081,
    -1415593985,  160033791,   70254592, 1073742336,-1077938177,   71953135,    1048576, 1073742080,
    -1518338049,   71580381,    2162752, 1073742080, 1450896383,   72690142,    2097152, 1073742080,
     1585362943, 1216702975,    2097152, 1073742080, 2147483647,   76114938,    1048576, 1073742080,
    -1140867693,   74344049,   67108864,          0,       -107,   78376816,    2097152,          0,
     1707078654,    4937167,    2097152, 1073742080,       -337,  149614453,    1048576,          0,
     1438317567,   67372509,    1048576,        256,  590134271,    4211160,    1048576, 1073741824,
     1448695551,   71636378,          0, 1073742080,  383350527,       5561,          0,          0,
       17067006,       4294,          0, 1073741824, 1701706687,     263385,    1048576,          0,
    -1481916777,  138750309,          0,          0,  459831039,   67110024,    1048576, 1073742080,
      779657215, -368831075, 1381652906, 1157710145,  304520191,    4457881,    1048576, 1073742336,
       28432367,-1505491831, 1431999829,   83891601,-1430603705, 1784960609,-1505387867,    4805718,
        6390766,-1845231474,-1856415130,  151340374,  270918575,-1572585336, 1433028181,   83887429,
       18138094,-1845493606,  303732058,   67175489,-1788695809,-1845213095,  303469909,   84018498,
           2046,-1841299134, 1382701418,   88167509, 1343792126,-1577009015, 1382701461,   83985745,
        2122750,-1845493623, 1361728917,   67175745,      70575,-1593835323, 1184520533,   83952725,
     -445732793, 1448416784,-1505384855,    4609045, 1516936262, 1444305505, 1632217769,    4542549,
    -1499027114,   72724069,          0,          0,  291803882,-2130624359, 1377125637,   67371329,
     1722459138, 1448428885, 1633261225,    4544598,          3,  285212672,     349253,          4,
         262890,-1862270843, 1386874197, 1157694804,  279466398,-1862270636, 1364547157,   67175492,
        2130602,-1577058236,  286671189,        320,  269779625, 1342177416,  290805056,        260,
        1065705,-2147467132, 1359282437,        320, 1448765094, 1364478292, 1447122277,      21525,
       16783169,       4096,    1048576,          0, 1074282922, 1107296325,  286412872,         64,
      353915561, 1073742980, 1090846980,        320,   71320234, 1073741892,   18154820,        320,
     1767203858,   22282512,  273683813,          0,      16794, 1073741893,   17104896,          0,
              3,          0,          0,          0,        577,   16777220,  285480212,      65552,
     1431655441, 1090814480,  272647429,      16384,    1069056,  285212672, 1610650965,     283668,
              2,          0,          0,          0, 1431655745,   72697105,          0, 1073741824);
end;

function TdxBulgarianEncodingDetector.GetPositiveRatio: Single;
begin
  Result := 0.969392;
end;

function TdxBulgarianEncodingDetector.GetPrecedenceMatrix: TArray<Integer>;
begin
  Result := FPrecedenceMatrix;
end;

{ TdxIbm855EncodingDetector }

class constructor TdxIbm855EncodingDetector.Initialize;
begin
  FCharToOrderMap := TArray<Byte>.Create(
    255,255,255,255,255,255,255,255,255,255,254,255,255,254,255,255,
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
    253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,
    252,252,252,252,252,252,252,252,252,252,253,253,253,253,253,253,
    253,142,143,144,145,146,147,148,149,150,151,152, 74,153, 75,154,
    155,156,157,158,159,160,161,162,163,164,165,253,253,253,253,253,
    253, 71,172, 66,173, 65,174, 76,175, 64,176,177, 77, 72,178, 69,
     67,179, 78, 73,180,181, 79,182,183,184,185,253,253,253,253,253,
    191,192,193,194, 68,195,196,197,198,199,200,201,202,203,204,205,
    206,207,208,209,210,211,212,213,214,215,216,217, 27, 59, 54, 70,
      3, 37, 21, 44, 28, 58, 13, 41,  2, 48, 39, 53, 19, 46,218,219,
    220,221,222,223,224, 26, 55,  4, 42,225,226,227,228, 23, 60,229,
    230,231,232,233,234,235, 11, 36,236,237,238,239,240,241,242,243,
      8, 49, 12, 38,  5, 31,  1, 34, 15,244,245,246,247, 35, 16,248,
     43,  9, 45,  7, 32,  6, 40, 14, 52, 24, 56, 10, 33, 17, 61,249,
    250, 18, 62, 20, 51, 25, 57, 30, 47, 29, 63, 22, 50,251,252,255);
end;

function TdxIbm855EncodingDetector.GetCharacterToOrderMap: TArray<Byte>;
begin
  Result := FCharToOrderMap;
end;

function TdxIbm855EncodingDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.GetEncoding(855);
end;

{ TdxIbm866EncodingDetector }

class constructor TdxIbm866EncodingDetector.Initialize;
begin
  FCharToOrderMap := TArray<Byte>.Create(
    255,255,255,255,255,255,255,255,255,255,254,255,255,254,255,255,
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
    253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,
    252,252,252,252,252,252,252,252,252,252,253,253,253,253,253,253,
    253,142,143,144,145,146,147,148,149,150,151,152, 74,153, 75,154,
    155,156,157,158,159,160,161,162,163,164,165,253,253,253,253,253,
    253, 71,172, 66,173, 65,174, 76,175, 64,176,177, 77, 72,178, 69,
     67,179, 78, 73,180,181, 79,182,183,184,185,253,253,253,253,253,
     37, 44, 33, 46, 41, 48, 56, 51, 42, 60, 36, 49, 38, 31, 34, 35,
     45, 32, 40, 52, 53, 55, 58, 50, 57, 63, 70, 62, 61, 47, 59, 43,
      3, 21, 10, 19, 13,  2, 24, 20,  4, 23, 11,  8, 12,  5,  1, 15,
    191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,
    207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,
    223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,
      9,  7,  6, 14, 39, 26, 28, 22, 25, 29, 54, 18, 17, 30, 27, 16,
    239, 68,240,241,242,243,244,245,246,247,248,249,250,251,252,255);
end;

function TdxIbm866EncodingDetector.GetCharacterToOrderMap: TArray<Byte>;
begin
  Result := FCharToOrderMap;
end;

function TdxIbm866EncodingDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.GetEncoding(866);
end;

{ TdxKoi8rEncodingDetector }

class constructor TdxKoi8rEncodingDetector.Initialize;
begin
  FCharToOrderMap := TArray<Byte>.Create(
    255,255,255,255,255,255,255,255,255,255,254,255,255,254,255,255,
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
    253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,
    252,252,252,252,252,252,252,252,252,252,253,253,253,253,253,253,
    253,142,143,144,145,146,147,148,149,150,151,152, 74,153, 75,154,
    155,156,157,158,159,160,161,162,163,164,165,253,253,253,253,253,
    253, 71,172, 66,173, 65,174, 76,175, 64,176,177, 77, 72,178, 69,
     67,179, 78, 73,180,181, 79,182,183,184,185,253,253,253,253,253,
    191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,
    207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,
    223,224,225, 68,226,227,228,229,230,231,232,233,234,235,236,237,
    238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,
     27,  3, 21, 28, 13,  2, 39, 19, 26,  4, 23, 11,  8, 12,  5,  1,
     15, 16,  9,  7,  6, 14, 24, 10, 17, 18, 20, 25, 30, 29, 22, 54,
     59, 37, 44, 58, 41, 48, 53, 46, 55, 42, 60, 36, 49, 38, 31, 34,
     35, 43, 45, 32, 40, 52, 56, 33, 61, 62, 51, 57, 47, 63, 50, 70);
end;

function TdxKoi8rEncodingDetector.GetCharacterToOrderMap: TArray<Byte>;
begin
  Result := FCharToOrderMap;
end;

function TdxKoi8rEncodingDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.GetEncoding(20866);
end;

{ TdxLatin5EncodingDetector }

class constructor TdxLatin5EncodingDetector.Initialize;
begin
  FCharToOrderMap := TArray<Byte>.Create(
    255,255,255,255,255,255,255,255,255,255,254,255,255,254,255,255,
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
    253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,
    252,252,252,252,252,252,252,252,252,252,253,253,253,253,253,253,
    253,142,143,144,145,146,147,148,149,150,151,152, 74,153, 75,154,
    155,156,157,158,159,160,161,162,163,164,165,253,253,253,253,253,
    253, 71,172, 66,173, 65,174, 76,175, 64,176,177, 77, 72,178, 69,
     67,179, 78, 73,180,181, 79,182,183,184,185,253,253,253,253,253,
    191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,
    207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,
    223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,
     37, 44, 33, 46, 41, 48, 56, 51, 42, 60, 36, 49, 38, 31, 34, 35,
     45, 32, 40, 52, 53, 55, 58, 50, 57, 63, 70, 62, 61, 47, 59, 43,
      3, 21, 10, 19, 13,  2, 24, 20,  4, 23, 11,  8, 12,  5,  1, 15,
      9,  7,  6, 14, 39, 26, 28, 22, 25, 29, 54, 18, 17, 30, 27, 16,
    239, 68,240,241,242,243,244,245,246,247,248,249,250,251,252,255);
end;

function TdxLatin5EncodingDetector.GetCharacterToOrderMap: TArray<Byte>;
begin
  Result := FCharToOrderMap;
end;

function TdxLatin5EncodingDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.GetEncoding(28595);
end;

{ TdxLatin5BulgarianEncodingDetector }

class constructor TdxLatin5BulgarianEncodingDetector.Initialize;
begin
  FCharToOrderMap := TArray<Byte>.Create(
    255,255,255,255,255,255,255,255,255,255,254,255,255,254,255,255,
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
    253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,
    252,252,252,252,252,252,252,252,252,252,253,253,253,253,253,253,
    253, 77, 90, 99,100, 72,109,107,101, 79,185, 81,102, 76, 94, 82,
    110,186,108, 91, 74,119, 84, 96,111,187,115,253,253,253,253,253,
    253, 65, 69, 70, 66, 63, 68,112,103, 92,194,104, 95, 86, 87, 71,
    116,195, 85, 93, 97,113,196,197,198,199,200,253,253,253,253,253,
    194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,
    210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,
     81,226,227,228,229,230,105,231,232,233,234,235,236, 45,237,238,
     31, 32, 35, 43, 37, 44, 55, 47, 40, 59, 33, 46, 38, 36, 41, 30,
     39, 28, 34, 51, 48, 49, 53, 50, 54, 57, 61,239, 67,240, 60, 56,
      1, 18,  9, 20, 11,  3, 23, 15,  2, 26, 12, 10, 14,  6,  4, 13,
      7,  8,  5, 19, 29, 25, 22, 21, 27, 24, 17, 75, 52,241, 42, 16,
     62,242,243,244, 58,245, 98,246,247,248,249,250,251, 91,252,253);
end;

function TdxLatin5BulgarianEncodingDetector.GetCharacterToOrderMap: TArray<Byte>;
begin
  Result := FCharToOrderMap;
end;

function TdxLatin5BulgarianEncodingDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.GetEncoding(28595);
end;

{ TdxWin1251EncodingDetector }

class constructor TdxWin1251EncodingDetector.Initialize;
begin
  FCharToOrderMap := TArray<Byte>.Create(
    255,255,255,255,255,255,255,255,255,255,254,255,255,254,255,255,
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
    253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,
    252,252,252,252,252,252,252,252,252,252,253,253,253,253,253,253,
    253,142,143,144,145,146,147,148,149,150,151,152, 74,153, 75,154,
    155,156,157,158,159,160,161,162,163,164,165,253,253,253,253,253,
    253, 71,172, 66,173, 65,174, 76,175, 64,176,177, 77, 72,178, 69,
     67,179, 78, 73,180,181, 79,182,183,184,185,253,253,253,253,253,
    191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,
    207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,
    223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,
    239,240,241,242,243,244,245,246, 68,247,248,249,250,251,252,253,
     37, 44, 33, 46, 41, 48, 56, 51, 42, 60, 36, 49, 38, 31, 34, 35,
     45, 32, 40, 52, 53, 55, 58, 50, 57, 63, 70, 62, 61, 47, 59, 43,
      3, 21, 10, 19, 13,  2, 24, 20,  4, 23, 11,  8, 12,  5,  1, 15,
      9,  7,  6, 14, 39, 26, 28, 22, 25, 29, 54, 18, 17, 30, 27, 16);
end;

function TdxWin1251EncodingDetector.GetCharacterToOrderMap: TArray<Byte>;
begin
  Result := FCharToOrderMap;
end;

function TdxWin1251EncodingDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.GetEncoding(1251);
end;

{ TdxWin1251BulgarianEncodingDetector }

class constructor TdxWin1251BulgarianEncodingDetector.Initialize;
begin
  FCharToOrderMap := TArray<Byte>.Create(
    255,255,255,255,255,255,255,255,255,255,254,255,255,254,255,255,
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
    253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,
    252,252,252,252,252,252,252,252,252,252,253,253,253,253,253,253,
    253, 77, 90, 99,100, 72,109,107,101, 79,185, 81,102, 76, 94, 82,
    110,186,108, 91, 74,119, 84, 96,111,187,115,253,253,253,253,253,
    253, 65, 69, 70, 66, 63, 68,112,103, 92,194,104, 95, 86, 87, 71,
    116,195, 85, 93, 97,113,196,197,198,199,200,253,253,253,253,253,
    206,207,208,209,210,211,212,213,120,214,215,216,217,218,219,220,
    221, 78, 64, 83,121, 98,117,105,222,223,224,225,226,227,228,229,
     88,230,231,232,233,122, 89,106,234,235,236,237,238, 45,239,240,
     73, 80,118,114,241,242,243,244,245, 62, 58,246,247,248,249,250,
     31, 32, 35, 43, 37, 44, 55, 47, 40, 59, 33, 46, 38, 36, 41, 30,
     39, 28, 34, 51, 48, 49, 53, 50, 54, 57, 61,251, 67,252, 60, 56,
      1, 18,  9, 20, 11,  3, 23, 15,  2, 26, 12, 10, 14,  6,  4, 13,
      7,  8,  5, 19, 29, 25, 22, 21, 27, 24, 17, 75, 52,253, 42, 16);
end;

function TdxWin1251BulgarianEncodingDetector.GetCharacterToOrderMap: TArray<Byte>;
begin
  Result := FCharToOrderMap;
end;

function TdxWin1251BulgarianEncodingDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.GetEncoding(1251);
end;

{ TdxMacCyrillicDetector }

class constructor TdxMacCyrillicDetector.Initialize;
begin
  FCharToOrderMap := TArray<Byte>.Create(
    255,255,255,255,255,255,255,255,255,255,254,255,255,254,255,255,
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
    253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,
    252,252,252,252,252,252,252,252,252,252,253,253,253,253,253,253,
    253,142,143,144,145,146,147,148,149,150,151,152, 74,153, 75,154,
    155,156,157,158,159,160,161,162,163,164,165,253,253,253,253,253,
    253, 71,172, 66,173, 65,174, 76,175, 64,176,177, 77, 72,178, 69,
     67,179, 78, 73,180,181, 79,182,183,184,185,253,253,253,253,253,
     37, 44, 33, 46, 41, 48, 56, 51, 42, 60, 36, 49, 38, 31, 34, 35,
     45, 32, 40, 52, 53, 55, 58, 50, 57, 63, 70, 62, 61, 47, 59, 43,
    191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,
    207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,
    223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,
    239,240,241,242,243,244,245,246,247,248,249,250,251,252, 68, 16,
      3, 21, 10, 19, 13,  2, 24, 20,  4, 23, 11,  8, 12,  5,  1, 15,
      9,  7,  6, 14, 39, 26, 28, 22, 25, 29, 54, 18, 17, 30, 27,255);
end;

function TdxMacCyrillicDetector.GetCharacterToOrderMap: TArray<Byte>;
begin
  Result := FCharToOrderMap;
end;

function TdxMacCyrillicDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.GetEncoding(10007);
end;

{ TdxGreekEncodingDetector }

class constructor TdxGreekEncodingDetector.Initialize;
begin
  FPrecedenceMatrix := TArray<Integer>.Create(
              0,          0,          0,          0, -134218064, 1059901347,    2097667,          0,
     -810299396,   49492222,          2,      32768, -201326920, 1069535119,    2097666,          0,
           -264,  801046371,    2097664,          0, 1023407100, 1053818879,    2097682,      16384,
     -822022148,   16727293,        514,          0, -805515268,  264239338,          0,          0,
     -805650436,  738196477,          0,          0, -285003780,   14921983,          0,          0,
     -834912308,  262353148,    2097168,          0, -805621764,   50052351,          0,          0,
    -1191183428,  982449067,    2097154,          0,  821817392,  571263747,    2097664,          0,
              0,          0,        512,          0, 1023407100,  957337347,    2097155,          0,
     -810799108,  117126911,          0,          0,  822081776, 1057935107,    3146243,          0,
      822080508,  990825219,    3145730,          0,  956299888,  503893763,    2097154,          0,
     -822919220,  820245503,          0,          0, -821882932,   15937784,          0,          0,
      822079536,  571264770,    2097408,          0, -886882308,   16494830,          0,          0,
      809498672,  168602374,          0,          0,-1892470836,   15937785,          0,          0,
      956298232,  705594883,    2097152,          0,  876345392,  352865282,          0,          0,
     -888987652,   27996414,          0,          0,-2013065268,   10631855,          0,          0,
    -1912599556,    7549112,          0,          0,  598716416,  572785155,-1971050360,    4328074,
    -1929376824,    8593576,          0,          0,-1977447540,-2132803416,-2046656476,     540674,
    -1024542624,-1575744753,-1970788216,    4720682,-1979503668,-2134732646,-2147446748,     526354,
              0,          0,          0,          0,-1886909704,-2002608128,-1979275224,     524320,
    -1978921272,-2147475288,-2147311456,        128,   44706848,   18892802,-1970722648,     133738,
           4096,      16386,  104867976,        522,-1979576640,   10625088,-2147319776,     524288,
    -1929377048,     135208,          0,          0,-2147341176, 1073741826,-1988018144,     525330,
       33688204,-2136997746,  134647844,     262146,     786432,          2,    8530072,       1561,
        9072672,  704692738,          0,          0,      37400,-2147416064,   38447144,     657482,
    -2097151744,-2145255424,-2075754460,     262170,-2147483064,-2147417952,-2147319768,   34078720,
         197288, 1073872904,-2147336160,     262166,     133768,-2147475414, 1073840144,       1024,
       33554572,-2145386464,   67272736,          1,-2113927544,-2145386328,-2013100000,     526336,
        2662448,      32770,          0,          0,   16781312,          0,        256,    2129920,
       33557160, 1078984704,-2147385344,          0,  537034752,      16384,    4196360,       1034,
            256,          8,          0,   34619392,        136,         32,      81936,          0,
              0,          0,        256,   35667984,     393216,       2306,          0,          0,
              0,          0,          0,          0,          0,          0,          0,          0);
end;

function TdxGreekEncodingDetector.GetPositiveRatio: Single;
begin
  Result := 0.982851;
end;

function TdxGreekEncodingDetector.GetPrecedenceMatrix: TArray<Integer>;
begin
  Result := FPrecedenceMatrix;
end;

{ TdxLatin7EncodingDetector }

class constructor TdxLatin7EncodingDetector.Initialize;
begin
  FCharToOrderMap := TArray<Byte>.Create(
    255,255,255,255,255,255,255,255,255,255,254,255,255,254,255,255,
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
    253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,
    252,252,252,252,252,252,252,252,252,252,253,253,253,253,253,253,
    253, 82,100,104, 94, 98,101,116,102,111,187,117, 92, 88,113, 85,
     79,118,105, 83, 67,114,119, 95, 99,109,188,253,253,253,253,253,
    253, 72, 70, 80, 81, 60, 96, 93, 89, 68,120, 97, 77, 86, 69, 55,
     78,115, 65, 66, 58, 76,106,103, 87,107,112,253,253,253,253,253,
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
    +253,233, 90,253,253,253,253,253,253,253,253,253,253, 74,253,253,
    253,253,253,253,247,248, 61, 36, 46, 71, 73,253, 54,253,108,123,
    110, 31, 51, 43, 41, 34, 91, 40, 52, 47, 44, 53, 38, 49, 59, 39,
     35, 48,250, 37, 33, 45, 56, 50, 84, 57,120,121, 17, 18, 22, 15,
    124,  1, 29, 20, 21,  3, 32, 13, 25,  5, 11, 16, 10,  6, 30,  4,
      9,  8, 14,  7,  2, 12, 28, 23, 42, 24, 64, 75, 19, 26, 27,253);
end;

function TdxLatin7EncodingDetector.GetCharacterToOrderMap: TArray<Byte>;
begin
  Result := FCharToOrderMap;
end;

function TdxLatin7EncodingDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.GetEncoding(28597);
end;

{ TdxWin1253EncodingDetector }

class constructor TdxWin1253EncodingDetector.Initialize;
begin
  FCharToOrderMap := TArray<Byte>.Create(
    255,255,255,255,255,255,255,255,255,255,254,255,255,254,255,255,
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
    253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,253,
    252,252,252,252,252,252,252,252,252,252,253,253,253,253,253,253,
    253, 82,100,104, 94, 98,101,116,102,111,187,117, 92, 88,113, 85,
     79,118,105, 83, 67,114,119, 95, 99,109,188,253,253,253,253,253,
    253, 72, 70, 80, 81, 60, 96, 93, 89, 68,120, 97, 77, 86, 69, 55,
     78,115, 65, 66, 58, 76,106,103, 87,107,112,253,253,253,253,253,
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
    253,233, 61,253,253,253,253,253,253,253,253,253,253, 74,253,253,
    253,253,253,253,247,253,253, 36, 46, 71, 73,253, 54,253,108,123,
    110, 31, 51, 43, 41, 34, 91, 40, 52, 47, 44, 53, 38, 49, 59, 39,
     35, 48,250, 37, 33, 45, 56, 50, 84, 57,120,121, 17, 18, 22, 15,
    124,  1, 29, 20, 21,  3, 32, 13, 25,  5, 11, 16, 10,  6, 30,  4,
      9,  8, 14,  7,  2, 12, 28, 23, 42, 24, 64, 75, 19, 26, 27,253);
end;

function TdxWin1253EncodingDetector.GetCharacterToOrderMap: TArray<Byte>;
begin
  Result := FCharToOrderMap;
end;

function TdxWin1253EncodingDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.GetEncoding(1253);
end;

{ TdxBomLessUnicodeGroupDetector }

constructor TdxBomLessUnicodeGroupDetector.Create;
begin
  inherited Create;
  CreateDetectors;
end;

procedure TdxBomLessUnicodeGroupDetector.PopulateDetectors;
begin
  Detectors.Add(TdxUtf16LittleEndianEncodingDetector.Create);
  Detectors.Add(TdxUtf16BigEndianEncodingDetector.Create);
end;

function TdxBomLessUnicodeGroupDetector.ForceAnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult;
begin
  Result := AnalyseDataCore(ABuffer, AFrom, ALength);
end;

class function TdxEncodingAnalyzer.Pack8Bits(A, B, C, D: Integer): Integer;
begin
  Result := Pack16Bits((B shl 8) or A, (D shl 8) or C);
end;

class function TdxEncodingAnalyzer.Pack16Bits(A, B: Integer): Integer;
begin
  Result := (B shl 16) or A;
end;

{ TdxUtf8EncodingDetector }

constructor TdxUtf8EncodingDetector.Create;
begin
  FEncodingAnalyzer := TdxUtf8EncodingAnalyzer.Create;
end;

destructor TdxUtf8EncodingDetector.Destroy;
begin
  FEncodingAnalyzer.Free;
  inherited Destroy;
end;

function TdxUtf8EncodingDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.UTF8;
end;

function TdxUtf8EncodingDetector.GetConfidence: Single;
var
  AUnlike, AConfidenceFactor: Single;
  I: Integer;
begin
  AUnlike := 0.99;

  AConfidenceFactor := 0.5;
  if FMultiByteCharCount < 6 then
  begin
    for I := 0 to FMultiByteCharCount - 1 do
      AUnlike := AUnlike * AConfidenceFactor;
    Result := 1.0 - AUnlike;
  end
  else
    Result := 0.99;
end;

function TdxUtf8EncodingDetector.ForceAnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult;
var
  ATo, I: Integer;
  AEncodingAnalyzerState: TdxEAState;
begin
  ATo := AFrom + ALength;
  for I := AFrom to ATo - 1 do
  begin
    AEncodingAnalyzerState := FEncodingAnalyzer.NextState(ABuffer[I]);
    if AEncodingAnalyzerState = EAStateItsMe then
      Exit(TdxDetectionResult.Positive);
    if AEncodingAnalyzerState = EAStateStart then
    begin
      if FEncodingAnalyzer.CurrentCharLength >= 2 then
        Inc(FMultiByteCharCount);
    end;
  end;

  if CurrentResult = TdxDetectionResult.Detecting then
    if GetConfidence > ShortcutThreshold then
      Exit(TdxDetectionResult.Positive);
  Result := CurrentResult;
end;

{ TdxUtf16EncodingDetector }

function TdxUtf16EncodingDetector.GetConfidence: Single;
var
  AByteCount: Integer;
  AHalfByteCount: Single;
begin
  AByteCount := FIndex + 1;
  if (AByteCount mod 2) <> 0 then
    Inc(AByteCount);

  if AByteCount < MinimumDataThreshold then
    Exit(0.01);

  AHalfByteCount := AByteCount / 2.0;
  Result := GetConfidenceCore(FOddLowerByteCount, FEvenLowerByteCount, AHalfByteCount);
end;

function TdxUtf16EncodingDetector.ForceAnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult;
var
  ATo, I: Integer;
  AValue: Byte;
begin
  ATo := AFrom + ALength;
  I := AFrom;
  while I < ATo do
  begin
    AValue := ABuffer[I];
    if (FIndex mod 2) = 0 then
    begin
      if AValue <= 5 then
        Inc(FEvenLowerByteCount);
    end
    else
    begin
      if AValue <= 5 then
        Inc(FOddLowerByteCount);
    end;
    Inc(I);
    Inc(FIndex);
  end;

  if CurrentResult = TdxDetectionResult.Detecting then
    if GetConfidence > ShortcutThreshold then
      Exit(TdxDetectionResult.Positive);
  Result := CurrentResult;
end;

{ TdxUtf16LittleEndianEncodingDetector }

function TdxUtf16LittleEndianEncodingDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.Unicode;
end;

function TdxUtf16LittleEndianEncodingDetector.GetConfidenceCore(AOddLowerByteCount, AEvenLowerByteCount: Integer;
  AHalfByteCount: Single): Single;
begin
  if (AOddLowerByteCount / AHalfByteCount > 0.8) and (AEvenLowerByteCount / AHalfByteCount < 0.2) then
    Result := 0.9 + Min(0.1, 2 * AHalfByteCount * 0.1 / (EnoughDataThreshold - MinimumDataThreshold))
  else
    Result := 0.01;
end;

{ TdxUtf16BigEndianEncodingDetector }

function TdxUtf16BigEndianEncodingDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.GetEncoding(1201);
end;

function TdxUtf16BigEndianEncodingDetector.GetConfidenceCore(AOddLowerByteCount, AEvenLowerByteCount: Integer;
  AHalfByteCount: Single): Single;
begin
  if (AEvenLowerByteCount / AHalfByteCount > 0.8) and (AOddLowerByteCount / AHalfByteCount < 0.2) then
    Result := 0.9 + Min(0.1, 2 * AHalfByteCount * 0.1 / (EnoughDataThreshold - MinimumDataThreshold))
  else
    Result := 0.01;
end;

{ TdxEastAsianEncodingDetector }

constructor TdxEastAsianEncodingDetector.Create(APreferredLanguage: Boolean);
begin
  FIsPreferredLanguage := APreferredLanguage;
  SetLength(FLastCharBuffer, 2);
  FEncodingAnalyzer := CreateEncodingAnalyzer;
  FDistributionAnalyzer := CreateDistributionAnalyzer;
end;

destructor TdxEastAsianEncodingDetector.Destroy;
begin
  FDistributionAnalyzer.Free;
  FEncodingAnalyzer.Free;
  inherited Destroy;
end;

function TdxEastAsianEncodingDetector.GetConfidence: Single;
begin
  Result := Max(GetContextAnalyzerConfidence(FIsPreferredLanguage), FDistributionAnalyzer.GetConfidence(FIsPreferredLanguage));
end;

function TdxEastAsianEncodingDetector.ForceAnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult;
var
  ATo, I, ACharLen: Integer;
  AEncodingAnalyzerState: TdxEAState;
begin
  ATo := AFrom + ALength;
  for I := AFrom to ATo - 1 do
  begin
    AEncodingAnalyzerState := FEncodingAnalyzer.NextState(ABuffer[I]);
    if AEncodingAnalyzerState = EAStateItsMe then
      Exit(TdxDetectionResult.Positive);

    if AEncodingAnalyzerState = EAStateStart then
    begin
      ACharLen := FEncodingAnalyzer.CurrentCharLength;
      if I = 0 then
      begin
        FLastCharBuffer[1] := ABuffer[0];
        ContinueContextAnalysis(FLastCharBuffer, 0, ACharLen);
        FDistributionAnalyzer.AnalyzeCharacter(FLastCharBuffer, 0, ACharLen);
      end
      else
      begin
        ContinueContextAnalysis(ABuffer, I - 1, ACharLen);
        FDistributionAnalyzer.AnalyzeCharacter(ABuffer, I - 1, ACharLen);
      end;
    end;
  end;

  FLastCharBuffer[0] := ABuffer[ATo - 1];

  if GotEnoughData and (GetConfidence > ShortcutThreshold) then
    Exit(TdxDetectionResult.Positive);

  Result := TdxDetectionResult.Detecting;
end;

function TdxEastAsianEncodingDetector.GotEnoughData: Boolean;
begin
  Result := FDistributionAnalyzer.GotEnoughData;
end;

procedure TdxEastAsianEncodingDetector.ContinueContextAnalysis(const ABuffer: TArray<Byte>; AFrom, ALength: Integer);
begin
end;

function TdxEastAsianEncodingDetector.GetContextAnalyzerConfidence(AIsPreferredLanguage: Boolean): Single;
begin
  Result := 0.0;
end;

{ TdxMultiByteCharsetGroupDetector }

constructor TdxMultiByteCharsetGroupDetector.Create(const ALanguageFilter: TdxEncodingDetectorLanguages);
begin
  inherited Create;
  FLanguageFilter := ALanguageFilter;
  CreateDetectors;
end;

procedure TdxMultiByteCharsetGroupDetector.PopulateDetectors;
var
  APreferredLanguage: Boolean;
begin
  Detectors.Add(TdxUtf8EncodingDetector.Create);
  if TdxEncodingDetectorLanguage.Japanese in FLanguageFilter then
  begin
    APreferredLanguage := FLanguageFilter = [TdxEncodingDetectorLanguage.Japanese];
    Detectors.Add(TdxShiftedJisEncodingDetector.Create(APreferredLanguage));
    Detectors.Add(TdxEucJpEncodingDetector.Create(APreferredLanguage));
  end;

  if TdxEncodingDetectorLanguage.ChineseSimplified in FLanguageFilter then
    Detectors.Add(TdxGB18030EncodingDetector.Create(FLanguageFilter = [TdxEncodingDetectorLanguage.ChineseSimplified]));

  if TdxEncodingDetectorLanguage.Korean in FLanguageFilter then
    Detectors.Add(TdxEucKrEncodingDetector.Create(FLanguageFilter = [TdxEncodingDetectorLanguage.Korean]));

  if TdxEncodingDetectorLanguage.ChineseTraditional in FLanguageFilter then
    Detectors.Add(TdxBig5EncodingDetector.Create((FLanguageFilter = [TdxEncodingDetectorLanguage.ChineseTraditional])));
end;

function TdxMultiByteCharsetGroupDetector.ForceAnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TdxDetectionResult;
var
  AStart, AKeepNext, ATo, I: Integer;
begin
  AStart := AFrom;
  AKeepNext := FKeepNext;

  ATo := AFrom + ALength;
  for I := AFrom to ATo - 1 do
  begin
    if IsUpperAsciiByte(ABuffer[I]) then
    begin
      if AKeepNext = 0 then
        AStart := I;
      AKeepNext := 2;
    end
    else
      if AKeepNext <> 0 then
      begin
        Dec(AKeepNext);
        if AKeepNext = 0 then
        begin
          Result := AnalyseDataCore(ABuffer, AStart, I + 1 - AStart);
          if Result <> TdxDetectionResult.Detecting then
            Exit;
        end;
      end;
  end;

  if AKeepNext <> 0 then
  begin
    Result := AnalyseDataCore(ABuffer, AStart, ALength - AStart);
    if Result <> TdxDetectionResult.Detecting then
      Exit;
  end;
  FKeepNext:= AKeepNext;
  Result := CurrentResult;
end;

{ TdxCharacterDistributionAnalyzer }

procedure TdxCharacterDistributionAnalyzer.AnalyzeCharacter(const ABuffer: TArray<Byte>; AFrom, ALength: Integer);
var
  AOrder: Integer;
begin
  if (ALength = 2) then
    AOrder := GetOrder(ABuffer, AFrom)
  else
    AOrder := -1;

  if AOrder >= 0 then
  begin
    Inc(FTotalCharCount);
    if AOrder < Length(CharToFreqOrder) then
    begin
      if 512 > CharToFreqOrder[AOrder] then
        Inc(FFrequentCharCount);
    end;
  end;
end;

function TdxCharacterDistributionAnalyzer.GetConfidence(AAIsPreferredLanguage: Boolean): Single;
const
  SureNo = 0.01;
  SureYes = 0.99;
  MINIMUM_DATA_THRESHOLD = 4;
begin
  if (FTotalCharCount <= 0) or (not AAIsPreferredLanguage and (FFrequentCharCount <= MINIMUM_DATA_THRESHOLD)) then
    Exit(SureNo);

  if FTotalCharCount <> FFrequentCharCount then
  begin
    Result := FFrequentCharCount / ((FTotalCharCount - FFrequentCharCount) * TypicalDistributionRatio);

    if Result < SureYes then
      Exit;
  end;
  Result := SureYes;
end;

function TdxCharacterDistributionAnalyzer.GotEnoughData: Boolean;
begin
  Result := FTotalCharCount > EnoughDataThreshold;
end;

{ TdxBig5EncodingDetector }

function TdxBig5EncodingDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.GetEncoding(950);
end;

function TdxBig5EncodingDetector.CreateDistributionAnalyzer: TdxCharacterDistributionAnalyzer;
begin
  Result := TdxBig5CharDistributionAnalyzer.Create;
end;

function TdxBig5EncodingDetector.CreateEncodingAnalyzer: TdxEncodingAnalyzer;
begin
  Result := TdxBig5EncodingAnalyzer.Create;
end;

{ TdxBig5CharDistributionAnalyzer }

class constructor TdxBig5CharDistributionAnalyzer.Initialize;
begin
  FCharToFreqOrder := TArray<Word>.Create(
       1,1801,1506, 255,1431, 198,   9,  82,   6,5008, 177, 202,3681,1256,2821, 110,
    3814,  33,3274, 261,  76,  44,2114,  16,2946,2187,1176, 659,3971,  26,3451,2653,
    1198,3972,3350,4202, 410,2215, 302, 590, 361,1964,   8, 204,  58,4510,5009,1932,
      63,5010,5011, 317,1614,  75, 222, 159,4203,2417,1480,5012,3555,3091, 224,2822,
    3682,   3,  10,3973,1471,  29,2787,1135,2866,1940, 873, 130,3275,1123, 312,5013,
    4511,2052, 507, 252, 682,5014, 142,1915, 124, 206,2947,  34,3556,3204,  64, 604,
    5015,2501,1977,1978, 155,1991, 645, 641,1606,5016,3452, 337,  72, 406,5017,  80,
     630, 238,3205,1509, 263, 939,1092,2654, 756,1440,1094,3453, 449,  69,2987, 591,
     179,2096, 471, 115,2035,1844,  60,  50,2988, 134, 806,1869, 734,2036,3454, 180,
     995,1607, 156, 537,2907, 688,5018, 319,1305, 779,2145, 514,2379, 298,4512, 359,
    2502,  90,2716,1338, 663,  11, 906,1099,2553,  20,2441, 182, 532,1716,5019, 732,
    1376,4204,1311,1420,3206,  25,2317,1056, 113, 399, 382,1950, 242,3455,2474, 529,
    3276, 475,1447,3683,5020, 117,  21, 656, 810,1297,2300,2334,3557,5021, 126,4205,
     706, 456, 150, 613,4513,  71,1118,2037,4206, 145,3092,  85, 835, 486,2115,1246,
    1426, 428, 727,1285,1015, 800, 106, 623, 303,1281,5022,2128,2359, 347,3815, 221,
    3558,3135,5023,1956,1153,4207,  83, 296,1199,3093, 192, 624,  93,5024, 822,1898,
    2823,3136, 795,2065, 991,1554,1542,1592,  27,  43,2867, 859, 139,1456, 860,4514,
     437, 712,3974, 164,2397,3137, 695, 211,3037,2097, 195,3975,1608,3559,3560,3684,
    3976, 234, 811,2989,2098,3977,2233,1441,3561,1615,2380, 668,2077,1638, 305, 228,
    1664,4515, 467, 415,5025, 262,2099,1593, 239, 108, 300, 200,1033, 512,1247,2078,
    5026,5027,2176,3207,3685,2682, 593, 845,1062,3277,  88,1723,2038,3978,1951, 212,
     266, 152, 149, 468,1899,4208,4516,  77, 187,5028,3038,  37,   5,2990,5029,3979,
    5030,5031,  39,2524,4517,2908,3208,2079,  55, 148,  74,4518, 545, 483,1474,1029,
    1665, 217,1870,1531,3138,1104,2655,4209,  24, 172,3562, 900,3980,3563,3564,4519,
      32,1408,2824,1312, 329, 487,2360,2251,2717, 784,2683,   4,3039,3351,1427,1789,
     188, 109, 499,5032,3686,1717,1790, 888,1217,3040,4520,5033,3565,5034,3352,1520,
    3687,3981, 196,1034, 775,5035,5036, 929,1816, 249, 439,  38,5037,1063,5038, 794,
    3982,1435,2301,  46, 178,3278,2066,5039,2381,5040, 214,1709,4521, 804,  35, 707,
     324,3688,1601,2554, 140, 459,4210,5041,5042,1365, 839, 272, 978,2262,2580,3456,
    2129,1363,3689,1423, 697, 100,3094,  48,  70,1231, 495,3139,2196,5043,1294,5044,
    2080, 462, 586,1042,3279, 853, 256, 988, 185,2382,3457,1698, 434,1084,5045,3458,
     314,2625,2788,4522,2335,2336, 569,2285, 637,1817,2525, 757,1162,1879,1616,3459,
     287,1577,2116, 768,4523,1671,2868,3566,2526,1321,3816, 909,2418,5046,4211, 933,
    3817,4212,2053,2361,1222,4524, 765,2419,1322, 786,4525,5047,1920,1462,1677,2909,
    1699,5048,4526,1424,2442,3140,3690,2600,3353,1775,1941,3460,3983,4213, 309,1369,
    1130,2825, 364,2234,1653,1299,3984,3567,3985,3986,2656, 525,1085,3041, 902,2001,
    1475, 964,4527, 421,1845,1415,1057,2286, 940,1364,3141, 376,4528,4529,1381,   7,
    2527, 983,2383, 336,1710,2684,1846, 321,3461, 559,1131,3042,2752,1809,1132,1313,
     265,1481,1858,5049, 352,1203,2826,3280, 167,1089, 420,2827, 776, 792,1724,3568,
    4214,2443,3281,5050,4215,5051, 446, 229, 333,2753, 901,3818,1200,1557,4530,2657,
    1921, 395,2754,2685,3819,4216,1836, 125, 916,3209,2626,4531,5052,5053,3820,5054,
    5055,5056,4532,3142,3691,1133,2555,1757,3462,1510,2318,1409,3569,5057,2146, 438,
    2601,2910,2384,3354,1068, 958,3043, 461, 311,2869,2686,4217,1916,3210,4218,1979,
     383, 750,2755,2627,4219, 274, 539, 385,1278,1442,5058,1154,1965, 384, 561, 210,
      98,1295,2556,3570,5059,1711,2420,1482,3463,3987,2911,1257, 129,5060,3821, 642,
     523,2789,2790,2658,5061, 141,2235,1333,  68, 176, 441, 876, 907,4220, 603,2602,
     710, 171,3464, 404, 549,  18,3143,2398,1410,3692,1666,5062,3571,4533,2912,4534,
    5063,2991, 368,5064, 146, 366,  99, 871,3693,1543, 748, 807,1586,1185,  22,2263,
     379,3822,3211,5065,3212, 505,1942,2628,1992,1382,2319,5066, 380,2362, 218, 702,
    1818,1248,3465,3044,3572,3355,3282,5067,2992,3694, 930,3283,3823,5068,  59,5069,
     585, 601,4221, 497,3466,1112,1314,4535,1802,5070,1223,1472,2177,5071, 749,1837,
     690,1900,3824,1773,3988,1476, 429,1043,1791,2236,2117, 917,4222, 447,1086,1629,
    5072, 556,5073,5074,2021,1654, 844,1090, 105, 550, 966,1758,2828,1008,1783, 686,
    1095,5075,2287, 793,1602,5076,3573,2603,4536,4223,2948,2302,4537,3825, 980,2503,
     544, 353, 527,4538, 908,2687,2913,5077, 381,2629,1943,1348,5078,1341,1252, 560,
    3095,5079,3467,2870,5080,2054, 973, 886,2081, 143,4539,5081,5082, 157,3989, 496,
    4224,  57, 840, 540,2039,4540,4541,3468,2118,1445, 970,2264,1748,1966,2082,4225,
    3144,1234,1776,3284,2829,3695, 773,1206,2130,1066,2040,1326,3990,1738,1725,4226,
     279,3145,  51,1544,2604, 423,1578,2131,2067, 173,4542,1880,5083,5084,1583, 264,
     610,3696,4543,2444, 280, 154,5085,5086,5087,1739, 338,1282,3096, 693,2871,1411,
    1074,3826,2445,5088,4544,5089,5090,1240, 952,2399,5091,2914,1538,2688, 685,1483,
    4227,2475,1436, 953,4228,2055,4545, 671,2400,  79,4229,2446,3285, 608, 567,2689,
    3469,4230,4231,1691, 393,1261,1792,2401,5092,4546,5093,5094,5095,5096,1383,1672,
    3827,3213,1464, 522,1119, 661,1150, 216, 675,4547,3991,1432,3574, 609,4548,2690,
    2402,5097,5098,5099,4232,3045,   0,5100,2476, 315, 231,2447, 301,3356,4549,2385,
    5101, 233,4233,3697,1819,4550,4551,5102,  96,1777,1315,2083,5103, 257,5104,1810,
    3698,2718,1139,1820,4234,2022,1124,2164,2791,1778,2659,5105,3097, 363,1655,3214,
    5106,2993,5107,5108,5109,3992,1567,3993, 718, 103,3215, 849,1443, 341,3357,2949,
    1484,5110,1712, 127,  67, 339,4235,2403, 679,1412, 821,5111,5112, 834, 738, 351,
    2994,2147, 846, 235,1497,1881, 418,1993,3828,2719, 186,1100,2148,2756,3575,1545,
    1355,2950,2872,1377, 583,3994,4236,2581,2995,5113,1298,3699,1078,2557,3700,2363,
      78,3829,3830, 267,1289,2100,2002,1594,4237, 348, 369,1274,2197,2178,1838,4552,
    1821,2830,3701,2757,2288,2003,4553,2951,2758, 144,3358, 882,4554,3995,2759,3470,
    4555,2915,5114,4238,1726, 320,5115,3996,3046, 788,2996,5116,2831,1774,1327,2873,
    3997,2832,5117,1306,4556,2004,1700,3831,3576,2364,2660, 787,2023, 506, 824,3702,
     534, 323,4557,1044,3359,2024,1901, 946,3471,5118,1779,1500,1678,5119,1882,4558,
     165, 243,4559,3703,2528, 123, 683,4239, 764,4560,  36,3998,1793, 589,2916, 816,
     626,1667,3047,2237,1639,1555,1622,3832,3999,5120,4000,2874,1370,1228,1933, 891,
    2084,2917, 304,4240,5121, 292,2997,2720,3577, 691,2101,4241,1115,4561, 118, 662,
    5122, 611,1156, 854,2386,1316,2875,   2, 386, 515,2918,5123,5124,3286, 868,2238,
    1486, 855,2661, 785,2216,3048,5125,1040,3216,3578,5126,3146, 448,5127,1525,5128,
    2165,4562,5129,3833,5130,4242,2833,3579,3147, 503, 818,4001,3148,1568, 814, 676,
    1444, 306,1749,5131,3834,1416,1030, 197,1428, 805,2834,1501,4563,5132,5133,5134,
    1994,5135,4564,5136,5137,2198,  13,2792,3704,2998,3149,1229,1917,5138,3835,2132,
    5139,4243,4565,2404,3580,5140,2217,1511,1727,1120,5141,5142, 646,3836,2448, 307,
    5143,5144,1595,3217,5145,5146,5147,3705,1113,1356,4002,1465,2529,2530,5148, 519,
    5149, 128,2133,  92,2289,1980,5150,4003,1512, 342,3150,2199,5151,2793,2218,1981,
    3360,4244, 290,1656,1317, 789, 827,2365,5152,3837,4566, 562, 581,4004,5153, 401,
    4567,2252,  94,4568,5154,1399,2794,5155,1463,2025,4569,3218,1944,5156, 828,1105,
    4245,1262,1394,5157,4246, 605,4570,5158,1784,2876,5159,2835, 819,2102, 578,2200,
    2952,5160,1502, 436,3287,4247,3288,2836,4005,2919,3472,3473,5161,2721,2320,5162,
    5163,2337,2068,  23,4571, 193, 826,3838,2103, 699,1630,4248,3098, 390,1794,1064,
    3581,5164,1579,3099,3100,1400,5165,4249,1839,1640,2877,5166,4572,4573, 137,4250,
     598,3101,1967, 780, 104, 974,2953,5167, 278, 899, 253, 402, 572, 504, 493,1339,
    5168,4006,1275,4574,2582,2558,5169,3706,3049,3102,2253, 565,1334,2722, 863,  41,
    5170,5171,4575,5172,1657,2338,  19, 463,2760,4251, 606,5173,2999,3289,1087,2085,
    1323,2662,3000,5174,1631,1623,1750,4252,2691,5175,2878, 791,2723,2663,2339, 232,
    2421,5176,3001,1498,5177,2664,2630, 755,1366,3707,3290,3151,2026,1609, 119,1918,
    3474, 862,1026,4253,5178,4007,3839,4576,4008,4577,2265,1952,2477,5179,1125, 817,
    4254,4255,4009,1513,1766,2041,1487,4256,3050,3291,2837,3840,3152,5180,5181,1507,
    5182,2692, 733,  40,1632,1106,2879, 345,4257, 841,2531, 230,4578,3002,1847,3292,
    3475,5183,1263, 986,3476,5184, 735, 879, 254,1137, 857, 622,1300,1180,1388,1562,
    4010,4011,2954, 967,2761,2665,1349, 592,2134,1692,3361,3003,1995,4258,1679,4012,
    1902,2188,5185, 739,3708,2724,1296,1290,5186,4259,2201,2202,1922,1563,2605,2559,
    1871,2762,3004,5187, 435,5188, 343,1108, 596,  17,1751,4579,2239,3477,3709,5189,
    4580, 294,3582,2955,1693, 477, 979, 281,2042,3583, 643,2043,3710,2631,2795,2266,
    1031,2340,2135,2303,3584,4581, 367,1249,2560,5190,3585,5191,4582,1283,3362,2005,
     240,1762,3363,4583,4584, 836,1069,3153, 474,5192,2149,2532, 268,3586,5193,3219,
    1521,1284,5194,1658,1546,4260,5195,3587,3588,5196,4261,3364,2693,1685,4262, 961,
    1673,2632, 190,2006,2203,3841,4585,4586,5197, 570,2504,3711,1490,5198,4587,2633,
    3293,1957,4588, 584,1514, 396,1045,1945,5199,4589,1968,2449,5200,5201,4590,4013,
     619,5202,3154,3294, 215,2007,2796,2561,3220,4591,3221,4592, 763,4263,3842,4593,
    5203,5204,1958,1767,2956,3365,3712,1174, 452,1477,4594,3366,3155,5205,2838,1253,
    2387,2189,1091,2290,4264, 492,5206, 638,1169,1825,2136,1752,4014, 648, 926,1021,
    1324,4595, 520,4596, 997, 847,1007, 892,4597,3843,2267,1872,3713,2405,1785,4598,
    1953,2957,3103,3222,1728,4265,2044,3714,4599,2008,1701,3156,1551,  30,2268,4266,
    5207,2027,4600,3589,5208, 501,5209,4267, 594,3478,2166,1822,3590,3479,3591,3223,
     829,2839,4268,5210,1680,3157,1225,4269,5211,3295,4601,4270,3158,2341,5212,4602,
    4271,5213,4015,4016,5214,1848,2388,2606,3367,5215,4603, 374,4017, 652,4272,4273,
     375,1140, 798,5216,5217,5218,2366,4604,2269, 546,1659, 138,3051,2450,4605,5219,
    2254, 612,1849, 910, 796,3844,1740,1371, 825,3845,3846,5220,2920,2562,5221, 692,
     444,3052,2634, 801,4606,4274,5222,1491, 244,1053,3053,4275,4276, 340,5223,4018,
    1041,3005, 293,1168,  87,1357,5224,1539, 959,5225,2240, 721, 694,4277,3847, 219,
    1478, 644,1417,3368,2666,1413,1401,1335,1389,4019,5226,5227,3006,2367,3159,1826,
     730,1515, 184,2840,  66,4607,5228,1660,2958, 246,3369, 378,1457, 226,3480, 975,
    4020,2959,1264,3592, 674, 696,5229, 163,5230,1141,2422,2167, 713,3593,3370,4608,
    4021,5231,5232,1186,  15,5233,1079,1070,5234,1522,3224,3594, 276,1050,2725, 758,
    1126, 653,2960,3296,5235,2342, 889,3595,4022,3104,3007, 903,1250,4609,4023,3481,
    3596,1342,1681,1718, 766,3297, 286,  89,2961,3715,5236,1713,5237,2607,3371,3008,
    5238,2962,2219,3225,2880,5239,4610,2505,2533, 181, 387,1075,4024, 731,2190,3372,
    5240,3298, 310, 313,3482,2304, 770,4278,  54,3054, 189,4611,3105,3848,4025,5241,
    1230,1617,1850, 355,3597,4279,4612,3373, 111,4280,3716,1350,3160,3483,3055,4281,
    2150,3299,3598,5242,2797,4026,4027,3009, 722,2009,5243,1071, 247,1207,2343,2478,
    1378,4613,2010, 864,1437,1214,4614, 373,3849,1142,2220, 667,4615, 442,2763,2563,
    3850,4028,1969,4282,3300,1840, 837, 170,1107, 934,1336,1883,5244,5245,2119,4283,
    2841, 743,1569,5246,4616,4284, 582,2389,1418,3484,5247,1803,5248, 357,1395,1729,
    3717,3301,2423,1564,2241,5249,3106,3851,1633,4617,1114,2086,4285,1532,5250, 482,
    2451,4618,5251,5252,1492, 833,1466,5253,2726,3599,1641,2842,5254,1526,1272,3718,
    4286,1686,1795, 416,2564,1903,1954,1804,5255,3852,2798,3853,1159,2321,5256,2881,
    4619,1610,1584,3056,2424,2764, 443,3302,1163,3161,5257,5258,4029,5259,4287,2506,
    3057,4620,4030,3162,2104,1647,3600,2011,1873,4288,5260,4289, 431,3485,5261, 250,
      97,  81,4290,5262,1648,1851,1558, 160, 848,5263, 866, 740,1694,5264,2204,2843,
    3226,4291,4621,3719,1687, 950,2479, 426, 469,3227,3720,3721,4031,5265,5266,1188,
     424,1996, 861,3601,4292,3854,2205,2694, 168,1235,3602,4293,5267,2087,1674,4622,
    3374,3303, 220,2565,1009,5268,3855, 670,3010, 332,1208, 717,5269,5270,3603,2452,
    4032,3375,5271, 513,5272,1209,2882,3376,3163,4623,1080,5273,5274,5275,5276,2534,
    3722,3604, 815,1587,4033,4034,5277,3605,3486,3856,1254,4624,1328,3058,1390,4035,
    1741,4036,3857,4037,5278, 236,3858,2453,3304,5279,5280,3723,3859,1273,3860,4625,
    5281, 308,5282,4626, 245,4627,1852,2480,1307,2583, 430, 715,2137,2454,5283, 270,
     199,2883,4038,5284,3606,2727,1753, 761,1754, 725,1661,1841,4628,3487,3724,5285,
    5286, 587,  14,3305, 227,2608, 326, 480,2270, 943,2765,3607, 291, 650,1884,5287,
    1702,1226, 102,1547,  62,3488, 904,4629,3489,1164,4294,5288,5289,1224,1548,2766,
     391, 498,1493,5290,1386,1419,5291,2056,1177,4630, 813, 880,1081,2368, 566,1145,
    4631,2291,1001,1035,2566,2609,2242, 394,1286,5292,5293,2069,5294,  86,1494,1730,
    4039, 491,1588, 745, 897,2963, 843,3377,4040,2767,2884,3306,1768, 998,2221,2070,
     397,1827,1195,1970,3725,3011,3378, 284,5295,3861,2507,2138,2120,1904,5296,4041,
    2151,4042,4295,1036,3490,1905, 114,2567,4296, 209,1527,5297,5298,2964,2844,2635,
    2390,2728,3164, 812,2568,5299,3307,5300,1559, 737,1885,3726,1210, 885,  28,2695,
    3608,3862,5301,4297,1004,1780,4632,5302, 346,1982,2222,2696,4633,3863,1742, 797,
    1642,4043,1934,1072,1384,2152, 896,4044,3308,3727,3228,2885,3609,5303,2569,1959,
    4634,2455,1786,5304,5305,5306,4045,4298,1005,1308,3728,4299,2729,4635,4636,1528,
    2610, 161,1178,4300,1983, 987,4637,1101,4301, 631,4046,1157,3229,2425,1343,1241,
    1016,2243,2570, 372, 877,2344,2508,1160, 555,1935, 911,4047,5307, 466,1170, 169,
    1051,2921,2697,3729,2481,3012,1182,2012,2571,1251,2636,5308, 992,2345,3491,1540,
    2730,1201,2071,2406,1997,2482,5309,4638, 528,1923,2191,1503,1874,1570,2369,3379,
    3309,5310, 557,1073,5311,1828,3492,2088,2271,3165,3059,3107, 767,3108,2799,4639,
    1006,4302,4640,2346,1267,2179,3730,3230, 778,4048,3231,2731,1597,2667,5312,4641,
    5313,3493,5314,5315,5316,3310,2698,1433,3311, 131,  95,1504,4049, 723,4303,3166,
    1842,3610,2768,2192,4050,2028,2105,3731,5317,3013,4051,1218,5318,3380,3232,4052,
    4304,2584, 248,1634,3864, 912,5319,2845,3732,3060,3865, 654,  53,5320,3014,5321,
    1688,4642, 777,3494,1032,4053,1425,5322, 191, 820,2121,2846, 971,4643, 931,3233,
     135, 664, 783,3866,1998, 772,2922,1936,4054,3867,4644,2923,3234, 282,2732, 640,
    1372,3495,1127, 922, 325,3381,5323,5324, 711,2045,5325,5326,4055,2223,2800,1937,
    4056,3382,2224,2255,3868,2305,5327,4645,3869,1258,3312,4057,3235,2139,2965,4058,
    4059,5328,2225, 258,3236,4646, 101,1227,5329,3313,1755,5330,1391,3314,5331,2924,
    2057, 893,5332,5333,5334,1402,4305,2347,5335,5336,3237,3611,5337,5338, 878,1325,
    1781,2801,4647, 259,1385,2585, 744,1183,2272,4648,5339,4060,2509,5340, 684,1024,
    4306,5341, 472,3612,3496,1165,3315,4061,4062, 322,2153, 881, 455,1695,1152,1340,
     660, 554,2154,4649,1058,4650,4307, 830,1065,3383,4063,4651,1924,5342,1703,1919,
    5343, 932,2273, 122,5344,4652, 947, 677,5345,3870,2637, 297,1906,1925,2274,4653,
    2322,3316,5346,5347,4308,5348,4309,  84,4310, 112, 989,5349, 547,1059,4064, 701,
    3613,1019,5350,4311,5351,3497, 942, 639, 457,2306,2456, 993,2966, 407, 851, 494,
    4654,3384, 927,5352,1237,5353,2426,3385, 573,4312, 680, 921,2925,1279,1875, 285,
     790,1448,1984, 719,2168,5354,5355,4655,4065,4066,1649,5356,1541, 563,5357,1077,
    5358,3386,3061,3498, 511,3015,4067,4068,3733,4069,1268,2572,3387,3238,4656,4657,
    5359, 535,1048,1276,1189,2926,2029,3167,1438,1373,2847,2967,1134,2013,5360,4313,
    1238,2586,3109,1259,5361, 700,5362,2968,3168,3734,4314,5363,4315,1146,1876,1907,
    4658,2611,4070, 781,2427, 132,1589, 203, 147, 273,2802,2407, 898,1787,2155,4071,
    4072,5364,3871,2803,5365,5366,4659,4660,5367,3239,5368,1635,3872, 965,5369,1805,
    2699,1516,3614,1121,1082,1329,3317,4073,1449,3873,  65,1128,2848,2927,2769,1590,
    3874,5370,5371,  12,2668,  45, 976,2587,3169,4661, 517,2535,1013,1037,3240,5372,
    3875,2849,5373,3876,5374,3499,5375,2612, 614,1999,2323,3877,3110,2733,2638,5376,
    2588,4316, 599,1269,5377,1811,3735,5378,2700,3111, 759,1060, 489,1806,3388,3318,
    1358,5379,5380,2391,1387,1215,2639,2256, 490,5381,5382,4317,1759,2392,2348,5383,
    4662,3878,1908,4074,2640,1807,3241,4663,3500,3319,2770,2349, 874,5384,5385,3501,
    3736,1859,  91,2928,3737,3062,3879,4664,5386,3170,4075,2669,5387,3502,1202,1403,
    3880,2969,2536,1517,2510,4665,3503,2511,5388,4666,5389,2701,1886,1495,1731,4076,
    2370,4667,5390,2030,5391,5392,4077,2702,1216, 237,2589,4318,2324,4078,3881,4668,
    4669,2703,3615,3504, 445,4670,5393,5394,5395,5396,2771,  61,4079,3738,1823,4080,
    5397, 687,2046, 935, 925, 405,2670, 703,1096,1860,2734,4671,4081,1877,1367,2704,
    3389, 918,2106,1782,2483, 334,3320,1611,1093,4672, 564,3171,3505,3739,3390, 945,
    2641,2058,4673,5398,1926, 872,4319,5399,3506,2705,3112, 349,4320,3740,4082,4674,
    3882,4321,3741,2156,4083,4675,4676,4322,4677,2408,2047, 782,4084, 400, 251,4323,
    1624,5400,5401, 277,3742, 299,1265, 476,1191,3883,2122,4324,4325,1109, 205,5402,
    2590,1000,2157,3616,1861,5403,5404,5405,4678,5406,4679,2573, 107,2484,2158,4085,
    3507,3172,5407,1533, 541,1301, 158, 753,4326,2886,3617,5408,1696, 370,1088,4327,
    4680,3618, 579, 327, 440, 162,2244, 269,1938,1374,3508, 968,3063,  56,1396,3113,
    2107,3321,3391,5409,1927,2159,4681,3016,5410,3619,5411,5412,3743,4682,2485,5413,
    2804,5414,1650,4683,5415,2613,5416,5417,4086,2671,3392,1149,3393,4087,3884,4088,
    5418,1076,  49,5419, 951,3242,3322,3323, 450,2850, 920,5420,1812,2805,2371,4328,
    1909,1138,2372,3885,3509,5421,3243,4684,1910,1147,1518,2428,4685,3886,5422,4686,
    2393,2614, 260,1796,3244,5423,5424,3887,3324, 708,5425,3620,1704,5426,3621,1351,
    1618,3394,3017,1887, 944,4329,3395,4330,3064,3396,4331,5427,3744, 422, 413,1714,
    3325, 500,2059,2350,4332,2486,5428,1344,1911, 954,5429,1668,5430,5431,4089,2409,
    4333,3622,3888,4334,5432,2307,1318,2512,3114, 133,3115,2887,4687, 629,  31,2851,
    2706,3889,4688, 850, 949,4689,4090,2970,1732,2089,4335,1496,1853,5433,4091, 620,
    3245, 981,1242,3745,3397,1619,3746,1643,3326,2140,2457,1971,1719,3510,2169,5434,
    3246,5435,5436,3398,1829,5437,1277,4690,1565,2048,5438,1636,3623,3116,5439, 869,
    2852, 655,3890,3891,3117,4092,3018,3892,1310,3624,4691,5440,5441,5442,1733, 558,
    4692,3747, 335,1549,3065,1756,4336,3748,1946,3511,1830,1291,1192, 470,2735,2108,
    2806, 913,1054,4093,5443,1027,5444,3066,4094,4693, 982,2672,3399,3173,3512,3247,
    3248,1947,2807,5445, 571,4694,5446,1831,5447,3625,2591,1523,2429,5448,2090, 984,
    4695,3749,1960,5449,3750, 852, 923,2808,3513,3751, 969,1519, 999,2049,2325,1705,
    5450,3118, 615,1662, 151, 597,4095,2410,2326,1049, 275,4696,3752,4337, 568,3753,
    3626,2487,4338,3754,5451,2430,2275, 409,3249,5452,1566,2888,3514,1002, 769,2853,
     194,2091,3174,3755,2226,3327,4339, 628,1505,5453,5454,1763,2180,3019,4096, 521,
    1161,2592,1788,2206,2411,4697,4097,1625,4340,4341, 412,  42,3119, 464,5455,2642,
    4698,3400,1760,1571,2889,3515,2537,1219,2207,3893,2643,2141,2373,4699,4700,3328,
    1651,3401,3627,5456,5457,3628,2488,3516,5458,3756,5459,5460,2276,2092, 460,5461,
    4701,5462,3020, 962, 588,3629, 289,3250,2644,1116,  52,5463,3067,1797,5464,5465,
    5466,1467,5467,1598,1143,3757,4342,1985,1734,1067,4702,1280,3402, 465,4703,1572,
     510,5468,1928,2245,1813,1644,3630,5469,4704,3758,5470,5471,2673,1573,1534,5472,
    5473, 536,1808,1761,3517,3894,3175,2645,5474,5475,5476,4705,3518,2929,1912,2809,
    5477,3329,1122, 377,3251,5478, 360,5479,5480,4343,1529, 551,5481,2060,3759,1769,
    2431,5482,2930,4344,3330,3120,2327,2109,2031,4706,1404, 136,1468,1479, 672,1171,
    3252,2308, 271,3176,5483,2772,5484,2050, 678,2736, 865,1948,4707,5485,2014,4098,
    2971,5486,2737,2227,1397,3068,3760,4708,4709,1735,2931,3403,3631,5487,3895, 509,
    2854,2458,2890,3896,5488,5489,3177,3178,4710,4345,2538,4711,2309,1166,1010, 552,
     681,1888,5490,5491,2972,2973,4099,1287,1596,1862,3179, 358, 453, 736, 175, 478,
    1117, 905,1167,1097,5492,1854,1530,5493,1706,5494,2181,3519,2292,3761,3520,3632,
    4346,2093,4347,5495,3404,1193,2489,4348,1458,2193,2208,1863,1889,1421,3331,2932,
    3069,2182,3521, 595,2123,5496,4100,5497,5498,4349,1707,2646, 223,3762,1359, 751,
    3121, 183,3522,5499,2810,3021, 419,2374, 633, 704,3897,2394, 241,5500,5501,5502,
     838,3022,3763,2277,2773,2459,3898,1939,2051,4101,1309,3122,2246,1181,5503,1136,
    2209,3899,2375,1446,4350,2310,4712,5504,5505,4351,1055,2615, 484,3764,5506,4102,
     625,4352,2278,3405,1499,4353,4103,5507,4104,4354,3253,2279,2280,3523,5508,5509,
    2774, 808,2616,3765,3406,4105,4355,3123,2539, 526,3407,3900,4356, 955,5510,1620,
    4357,2647,2432,5511,1429,3766,1669,1832, 994, 928,5512,3633,1260,5513,5514,5515,
    1949,2293, 741,2933,1626,4358,2738,2460, 867,1184, 362,3408,1392,5516,5517,4106,
    4359,1770,1736,3254,2934,4713,4714,1929,2707,1459,1158,5518,3070,3409,2891,1292,
    1930,2513,2855,3767,1986,1187,2072,2015,2617,4360,5519,2574,2514,2170,3768,2490,
    3332,5520,3769,4715,5521,5522, 666,1003,3023,1022,3634,4361,5523,4716,1814,2257,
     574,3901,1603, 295,1535, 705,3902,4362, 283, 858, 417,5524,5525,3255,4717,4718,
    3071,1220,1890,1046,2281,2461,4107,1393,1599, 689,2575, 388,4363,5526,2491, 802,
    5527,2811,3903,2061,1405,2258,5528,4719,3904,2110,1052,1345,3256,1585,5529, 809,
    5530,5531,5532, 575,2739,3524, 956,1552,1469,1144,2328,5533,2329,1560,2462,3635,
    3257,4108, 616,2210,4364,3180,2183,2294,5534,1833,5535,3525,4720,5536,1319,3770,
    3771,1211,3636,1023,3258,1293,2812,5537,5538,5539,3905, 607,2311,3906, 762,2892,
    1439,4365,1360,4721,1485,3072,5540,4722,1038,4366,1450,2062,2648,4367,1379,4723,
    2593,5541,5542,4368,1352,1414,2330,2935,1172,5543,5544,3907,3908,4724,1798,1451,
    5545,5546,5547,5548,2936,4109,4110,2492,2351, 411,4111,4112,3637,3333,3124,4725,
    1561,2674,1452,4113,1375,5549,5550,  47,2974, 316,5551,1406,1591,2937,3181,5552,
    1025,2142,3125,3182, 354,2740, 884,2228,4369,2412, 508,3772, 726,3638, 996,2433,
    3639, 729,5553, 392,2194,1453,4114,4726,3773,5554,5555,2463,3640,2618,1675,2813,
     919,2352,2975,2353,1270,4727,4115,  73,5556,5557, 647,5558,3259,2856,2259,1550,
    1346,3024,5559,1332, 883,3526,5560,5561,5562,5563,3334,2775,5564,1212, 831,1347,
    4370,4728,2331,3909,1864,3073, 720,3910,4729,4730,3911,5565,4371,5566,5567,4731,
    5568,5569,1799,4732,3774,2619,4733,3641,1645,2376,4734,5570,2938, 669,2211,2675,
    2434,5571,2893,5572,5573,1028,3260,5574,4372,2413,5575,2260,1353,5576,5577,4735,
    3183, 518,5578,4116,5579,4373,1961,5580,2143,4374,5581,5582,3025,2354,2355,3912,
     516,1834,1454,4117,2708,4375,4736,2229,2620,1972,1129,3642,5583,2776,5584,2976,
    1422, 577,1470,3026,1524,3410,5585,5586, 432,4376,3074,3527,5587,2594,1455,2515,
    2230,1973,1175,5588,1020,2741,4118,3528,4737,5589,2742,5590,1743,1361,3075,3529,
    2649,4119,4377,4738,2295, 895, 924,4378,2171, 331,2247,3076, 166,1627,3077,1098,
    5591,1232,2894,2231,3411,4739, 657, 403,1196,2377, 542,3775,3412,1600,4379,3530,
    5592,4740,2777,3261, 576, 530,1362,4741,4742,2540,2676,3776,4120,5593, 842,3913,
    5594,2814,2032,1014,4121, 213,2709,3413, 665, 621,4380,5595,3777,2939,2435,5596,
    2436,3335,3643,3414,4743,4381,2541,4382,4744,3644,1682,4383,3531,1380,5597, 724,
    2282, 600,1670,5598,1337,1233,4745,3126,2248,5599,1621,4746,5600, 651,4384,5601,
    1612,4385,2621,5602,2857,5603,2743,2312,3078,5604, 716,2464,3079, 174,1255,2710,
    4122,3645, 548,1320,1398, 728,4123,1574,5605,1891,1197,3080,4124,5606,3081,3082,
    3778,3646,3779, 747,5607, 635,4386,4747,5608,5609,5610,4387,5611,5612,4748,5613,
    3415,4749,2437, 451,5614,3780,2542,2073,4388,2744,4389,4125,5615,1764,4750,5616,
    4390, 350,4751,2283,2395,2493,5617,4391,4126,2249,1434,4127, 488,4752, 458,4392,
    4128,3781, 771,1330,2396,3914,2576,3184,2160,2414,1553,2677,3185,4393,5618,2494,
    2895,2622,1720,2711,4394,3416,4753,5619,2543,4395,5620,3262,4396,2778,5621,2016,
    2745,5622,1155,1017,3782,3915,5623,3336,2313, 201,1865,4397,1430,5624,4129,5625,
    5626,5627,5628,5629,4398,1604,5630, 414,1866, 371,2595,4754,4755,3532,2017,3127,
    4756,1708, 960,4399, 887, 389,2172,1536,1663,1721,5631,2232,4130,2356,2940,1580,
    5632,5633,1744,4757,2544,4758,4759,5634,4760,5635,2074,5636,4761,3647,3417,2896,
    4400,5637,4401,2650,3418,2815, 673,2712,2465, 709,3533,4131,3648,4402,5638,1148,
     502, 634,5639,5640,1204,4762,3649,1575,4763,2623,3783,5641,3784,3128, 948,3263,
     121,1745,3916,1110,5642,4403,3083,2516,3027,4132,3785,1151,1771,3917,1488,4133,
    1987,5643,2438,3534,5644,5645,2094,5646,4404,3918,1213,1407,2816, 531,2746,2545,
    3264,1011,1537,4764,2779,4405,3129,1061,5647,3786,3787,1867,2897,5648,2018, 120,
    4406,4407,2063,3650,3265,2314,3919,2678,3419,1955,4765,4134,5649,3535,1047,2713,
    1266,5650,1368,4766,2858, 649,3420,3920,2546,2747,1102,2859,2679,5651,5652,2000,
    5653,1111,3651,2977,5654,2495,3921,3652,2817,1855,3421,3788,5655,5656,3422,2415,
    2898,3337,3266,3653,5657,2577,5658,3654,2818,4135,1460, 856,5659,3655,5660,2899,
    2978,5661,2900,3922,5662,4408, 632,2517, 875,3923,1697,3924,2296,5663,5664,4767,
    3028,1239, 580,4768,4409,5665, 914, 936,2075,1190,4136,1039,2124,5666,5667,5668,
    5669,3423,1473,5670,1354,4410,3925,4769,2173,3084,4137, 915,3338,4411,4412,3339,
    1605,1835,5671,2748, 398,3656,4413,3926,4138, 328,1913,2860,4139,3927,1331,4414,
    3029, 937,4415,5672,3657,4140,4141,3424,2161,4770,3425, 524, 742, 538,3085,1012,
    5673,5674,3928,2466,5675, 658,1103, 225,3929,5676,5677,4771,5678,4772,5679,3267,
    1243,5680,4142, 963,2250,4773,5681,2714,3658,3186,5682,5683,2596,2332,5684,4774,
    5685,5686,5687,3536, 957,3426,2547,2033,1931,2941,2467, 870,2019,3659,1746,2780,
    2781,2439,2468,5688,3930,5689,3789,3130,3790,3537,3427,3791,5690,1179,3086,5691,
    3187,2378,4416,3792,2548,3188,3131,2749,4143,5692,3428,1556,2549,2297, 977,2901,
    2034,4144,1205,3429,5693,1765,3430,3189,2125,1271, 714,1689,4775,3538,5694,2333,
    3931, 533,4417,3660,2184, 617,5695,2469,3340,3539,2315,5696,5697,3190,5698,5699,
    3932,1988, 618, 427,2651,3540,3431,5700,5701,1244,1690,5702,2819,4418,4776,5703,
    3541,4777,5704,2284,1576, 473,3661,4419,3432, 972,5705,3662,5706,3087,5707,5708,
    4778,4779,5709,3793,4145,4146,5710, 153,4780, 356,5711,1892,2902,4420,2144, 408,
     803,2357,5712,3933,5713,4421,1646,2578,2518,4781,4782,3934,5714,3935,4422,5715,
    2416,3433, 752,5716,5717,1962,3341,2979,5718, 746,3030,2470,4783,4423,3794, 698,
    4784,1893,4424,3663,2550,4785,3664,3936,5719,3191,3434,5720,1824,1302,4147,2715,
    3937,1974,4425,5721,4426,3192, 823,1303,1288,1236,2861,3542,4148,3435, 774,3938,
    5722,1581,4786,1304,2862,3939,4787,5723,2440,2162,1083,3268,4427,4149,4428, 344,
    1173, 288,2316, 454,1683,5724,5725,1461,4788,4150,2597,5726,5727,4789, 985, 894,
    5728,3436,3193,5729,1914,2942,3795,1989,5730,2111,1975,5731,4151,5732,2579,1194,
     425,5733,4790,3194,1245,3796,4429,5734,5735,2863,5736, 636,4791,1856,3940, 760,
    1800,5737,4430,2212,1508,4792,4152,1894,1684,2298,5738,5739,4793,4431,4432,2213,
     479,5740,5741, 832,5742,4153,2496,5743,2980,2497,3797, 990,3132, 627,1815,2652,
    4433,1582,4434,2126,2112,3543,4794,5744, 799,4435,3195,5745,4795,2113,1737,3031,
    1018, 543, 754,4436,3342,1676,4796,4797,4154,4798,1489,5746,3544,5747,2624,2903,
    4155,5748,5749,2981,5750,5751,5752,5753,3196,4799,4800,2185,1722,5754,3269,3270,
    1843,3665,1715, 481, 365,1976,1857,5755,5756,1963,2498,4801,5757,2127,3666,3271,
     433,1895,2064,2076,5758, 602,2750,5759,5760,5761,5762,5763,3032,1628,3437,5764,
    3197,4802,4156,2904,4803,2519,5765,2551,2782,5766,5767,5768,3343,4804,2905,5769,
    4805,5770,2864,4806,4807,1221,2982,4157,2520,5771,5772,5773,1868,1990,5774,5775,
    5776,1896,5777,5778,4808,1897,4158, 318,5779,2095,4159,4437,5780,5781, 485,5782,
     938,3941, 553,2680, 116,5783,3942,3667,5784,3545,2681,2783,3438,3344,2820,5785,
    3668,2943,4160,1747,2944,2983,5786,5787, 207,5788,4809,5789,4810,2521,5790,3033,
     890,3669,3943,5791,1878,3798,3439,5792,2186,2358,3440,1652,5793,5794,5795, 941,
    2299, 208,3546,4161,2020, 330,4438,3944,2906,2499,3799,4439,4811,5796,5797,5798);
end;

function TdxBig5CharDistributionAnalyzer.GetOrder(const AStr: TArray<Byte>; AOffset: Integer): Integer;
begin
  if AStr[AOffset] >= $a4 then
    if AStr[AOffset + 1] >= $a1 then
      Result := 157 * (AStr[AOffset] - $a4) + AStr[AOffset + 1] - $a1 + 63
    else
      Result := 157 * (AStr[AOffset] - $a4) + AStr[AOffset + 1] - $40
  else
    Result := -1;
end;

function TdxBig5CharDistributionAnalyzer.GetCharToFreqOrder: TArray<Word>;
begin
  Result := FCharToFreqOrder;
end;

function TdxBig5CharDistributionAnalyzer.GetTypicalDistributionRatio: Single;
begin
  Result := 0.75;
end;

{ TdxBig5EncodingAnalyzer }

class constructor TdxBig5EncodingAnalyzer.Initialize;
begin
  FClassTable := TArray<Integer>.Create(
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,0,0),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,0,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,1),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,3,3,3,3,3,3,3),
    Pack4Bits(3,3,3,3,3,3,3,3),
    Pack4Bits(3,3,3,3,3,3,3,3),
    Pack4Bits(3,3,3,3,3,3,3,3),
    Pack4Bits(3,3,3,3,3,3,3,3),
    Pack4Bits(3,3,3,3,3,3,3,3),
    Pack4Bits(3,3,3,3,3,3,3,3),
    Pack4Bits(3,3,3,3,3,3,3,3),
    Pack4Bits(3,3,3,3,3,3,3,3),
    Pack4Bits(3,3,3,3,3,3,3,3),
    Pack4Bits(3,3,3,3,3,3,3,3),
    Pack4Bits(3,3,3,3,3,3,3,0));
  FStateTable := TArray<Integer>.Create(
    Pack4Bits(EAStateError, EAStateStart, EAStateStart,            3, EAStateError, EAStateError, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateError),
    Pack4Bits(EAStateError, EAStateStart, EAStateStart, EAStateStart, EAStateStart, EAStateStart, EAStateStart, EAStateStart));
  FCharLenTable := TArray<Integer>.Create(0, 1, 1, 2, 0);
end;

function TdxBig5EncodingAnalyzer.GetClassTable: TArray<Integer>;
begin
  Result := FClassTable;
end;

function TdxBig5EncodingAnalyzer.GetStateTable: TArray<Integer>;
begin
  Result := FStateTable;
end;

function TdxBig5EncodingAnalyzer.GetCharLenTable: TArray<Integer>;
begin
  Result := FCharLenTable;
end;

{ TdxGB18030EncodingDetector }

function TdxGB18030EncodingDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.GetEncoding(54936);
end;

function TdxGB18030EncodingDetector.CreateDistributionAnalyzer: TdxCharacterDistributionAnalyzer;
begin
  Result := TdxGB18030CharDistributionAnalyzer.Create;
end;

function TdxGB18030EncodingDetector.CreateEncodingAnalyzer: TdxEncodingAnalyzer;
begin
  Result := TdxGB18030EncodingAnalyzer.Create;
end;

{ TdxGB18030CharDistributionAnalyzer }

class constructor TdxGB18030CharDistributionAnalyzer.Initialize;
begin
  FCharToFreqOrder := TArray<Word>.Create(
    1671, 749,1443,2364,3924,3807,2330,3921,1704,3463,2691,1511,1515, 572,3191,2205,
    2361, 224,2558, 479,1711, 963,3162, 440,4060,1905,2966,2947,3580,2647,3961,3842,
    2204, 869,4207, 970,2678,5626,2944,2956,1479,4048, 514,3595, 588,1346,2820,3409,
     249,4088,1746,1873,2047,1774, 581,1813, 358,1174,3590,1014,1561,4844,2245, 670,
    1636,3112, 889,1286, 953, 556,2327,3060,1290,3141, 613, 185,3477,1367, 850,3820,
    1715,2428,2642,2303,2732,3041,2562,2648,3566,3946,1349, 388,3098,2091,1360,3585,
     152,1687,1539, 738,1559,  59,1232,2925,2267,1388,1249,1741,1679,2960, 151,1566,
    1125,1352,4271, 924,4296, 385,3166,4459, 310,1245,2850,  70,3285,2729,3534,3575,
    2398,3298,3466,1960,2265, 217,3647, 864,1909,2084,4401,2773,1010,3269,5152, 853,
    3051,3121,1244,4251,1895, 364,1499,1540,2313,1180,3655,2268, 562, 715,2417,3061,
     544, 336,3768,2380,1752,4075, 950, 280,2425,4382, 183,2759,3272, 333,4297,2155,
    1688,2356,1444,1039,4540, 736,1177,3349,2443,2368,2144,2225, 565, 196,1482,3406,
     927,1335,4147, 692, 878,1311,1653,3911,3622,1378,4200,1840,2969,3149,2126,1816,
    2534,1546,2393,2760, 737,2494,  13, 447, 245,2747,  38,2765,2129,2589,1079, 606,
     360, 471,3755,2890, 404, 848, 699,1785,1236, 370,2221,1023,3746,2074,2026,2023,
    2388,1581,2119, 812,1141,3091,2536,1519, 804,2053, 406,1596,1090, 784, 548,4414,
    1806,2264,2936,1100, 343,4114,5096, 622,3358, 743,3668,1510,1626,5020,3567,2513,
    3195,4115,5627,2489,2991,  24,2065,2697,1087,2719,  48,1634, 315,  68, 985,2052,
     198,2239,1347,1107,1439, 597,2366,2172, 871,3307, 919,2487,2790,1867, 236,2570,
    1413,3794, 906,3365,3381,1701,1982,1818,1524,2924,1205, 616,2586,2072,2004, 575,
     253,3099,  32,1365,1182, 197,1714,2454,1201, 554,3388,3224,2748, 756,2587, 250,
    2567,1507,1517,3529,1922,2761,2337,3416,1961,1677,2452,2238,3153, 615, 911,1506,
    1474,2495,1265,1906,2749,3756,3280,2161, 898,2714,1759,3450,2243,2444, 563,  26,
    3286,2266,3769,3344,2707,3677, 611,1402, 531,1028,2871,4548,1375, 261,2948, 835,
    1190,4134, 353, 840,2684,1900,3082,1435,2109,1207,1674, 329,1872,2781,4055,2686,
    2104, 608,3318,2423,2957,2768,1108,3739,3512,3271,3985,2203,1771,3520,1418,2054,
    1681,1153, 225,1627,2929, 162,2050,2511,3687,1954, 124,1859,2431,1684,3032,2894,
     585,4805,3969,2869,2704,2088,2032,2095,3656,2635,4362,2209, 256, 518,2042,2105,
    3777,3657, 643,2298,1148,1779, 190, 989,3544, 414,  11,2135,2063,2979,1471, 403,
    3678, 126, 770,1563, 671,2499,3216,2877, 600,1179, 307,2805,4937,1268,1297,2694,
     252,4032,1448,1494,1331,1394, 127,2256, 222,1647,1035,1481,3056,1915,1048, 873,
    3651, 210,  33,1608,2516, 200,1520, 415, 102,   0,3389,1287, 817,  91,3299,2940,
     836,1814, 549,2197,1396,1669,2987,3582,2297,2848,4528,1070, 687,  20,1819, 121,
    1552,1364,1461,1968,2617,3540,2824,2083, 177, 948,4938,2291, 110,4549,2066, 648,
    3359,1755,2110,2114,4642,4845,1693,3937,3308,1257,1869,2123, 208,1804,3159,2992,
    2531,2549,3361,2418,1350,2347,2800,2568,1291,2036,2680,  72, 842,1990, 212,1233,
    1154,1586,  75,2027,3410,4900,1823,1337,2710,2676, 728,2810,1522,3026,4995, 157,
     755,1050,4022, 710, 785,1936,2194,2085,1406,2777,2400, 150,1250,4049,1206, 807,
    1910, 534, 529,3309,1721,1660, 274,  39,2827, 661,2670,1578, 925,3248,3815,1094,
    4278,4901,4252,  41,1150,3747,2572,2227,4501,3658,4902,3813,3357,3617,2884,2258,
     887, 538,4187,3199,1294,2439,3042,2329,2343,2497,1255, 107, 543,1527, 521,3478,
    3568, 194,5062,  15, 961,3870,1241,1192,2664,  66,5215,3260,2111,1295,1127,2152,
    3805,4135, 901,1164,1976, 398,1278, 530,1460, 748, 904,1054,1966,1426,  53,2909,
     509, 523,2279,1534, 536,1019, 239,1685, 460,2353, 673,1065,2401,3600,4298,2272,
    1272,2363, 284,1753,3679,4064,1695,  81, 815,2677,2757,2731,1386, 859, 500,4221,
    2190,2566, 757,1006,2519,2068,1166,1455, 337,2654,3203,1863,1682,1914,3025,1252,
    1409,1366, 847, 714,2834,2038,3209, 964,2970,1901, 885,2553,1078,1756,3049, 301,
    1572,3326, 688,2130,1996,2429,1805,1648,2930,3421,2750,3652,3088, 262,1158,1254,
     389,1641,1812, 526,1719, 923,2073,1073,1902, 468, 489,4625,1140, 857,2375,3070,
    3319,2863, 380, 116,1328,2693,1161,2244, 273,1212,1884,2769,3011,1775,1142, 461,
    3066,1200,2147,2212, 790, 702,2695,4222,1601,1058, 434,2338,5153,3640,  67,2360,
    4099,2502, 618,3472,1329, 416,1132, 830,2782,1807,2653,3211,3510,1662, 192,2124,
     296,3979,1739,1611,3684,  23, 118, 324, 446,1239,1225, 293,2520,3814,3795,2535,
    3116,  17,1074, 467,2692,2201, 387,2922,  45,1326,3055,1645,3659,2817, 958, 243,
    1903,2320,1339,2825,1784,3289, 356, 576, 865,2315,2381,3377,3916,1088,3122,1713,
    1655, 935, 628,4689,1034,1327, 441, 800, 720, 894,1979,2183,1528,5289,2702,1071,
    4046,3572,2399,1571,3281,  79, 761,1103, 327, 134, 758,1899,1371,1615, 879, 442,
     215,2605,2579, 173,2048,2485,1057,2975,3317,1097,2253,3801,4263,1403,1650,2946,
     814,4968,3487,1548,2644,1567,1285,   2, 295,2636,  97, 946,3576, 832, 141,4257,
    3273, 760,3821,3521,3156,2607, 949,1024,1733,1516,1803,1920,2125,2283,2665,3180,
    1501,2064,3560,2171,1592, 803,3518,1416, 732,3897,4258,1363,1362,2458, 119,1427,
     602,1525,2608,1605,1639,3175, 694,3064,  10, 465,  76,2000,4846,4208, 444,3781,
    1619,3353,2206,1273,3796, 740,2483, 320,1723,2377,3660,2619,1359,1137,1762,1724,
    2345,2842,1850,1862, 912, 821,1866, 612,2625,1735,2573,3369,1093, 844,  89, 937,
     930,1424,3564,2413,2972,1004,3046,3019,2011, 711,3171,1452,4178, 428, 801,1943,
     432, 445,2811, 206,4136,1472, 730, 349,  73, 397,2802,2547, 998,1637,1167, 789,
     396,3217, 154,1218, 716,1120,1780,2819,4826,1931,3334,3762,2139,1215,2627, 552,
    3664,3628,3232,1405,2383,3111,1356,2652,3577,3320,3101,1703, 640,1045,1370,1246,
    4996, 371,1575,2436,1621,2210, 984,4033,1734,2638,  16,4529, 663,2755,3255,1451,
    3917,2257,1253,1955,2234,1263,2951, 214,1229, 617, 485, 359,1831,1969, 473,2310,
     750,2058, 165,  80,2864,2419, 361,4344,2416,2479,1134, 796,3726,1266,2943, 860,
    2715, 938, 390,2734,1313,1384, 248, 202, 877,1064,2854, 522,3907, 279,1602, 297,
    2357, 395,3740, 137,2075, 944,4089,2584,1267,3802,  62,1533,2285, 178, 176, 780,
    2440, 201,3707, 590, 478,1560,4354,2117,1075,  30,  74,4643,4004,1635,1441,2745,
     776,2596, 238,1077,1692,1912,2844, 605, 499,1742,3947, 241,3053, 980,1749, 936,
    2640,4511,2582, 515,1543,2162,5322,2892,2993, 890,2148,1924, 665,1827,3581,1032,
     968,3163, 339,1044,1896, 270, 583,1791,1720,4367,1194,3488,3669,  43,2523,1657,
     163,2167, 290,1209,1622,3378, 550, 634,2508,2510, 695,2634,2384,2512,1476,1414,
     220,1469,2341,2138,2852,3183,2900,4939,2865,3502,1211,3680, 854,3227,1299,2976,
    3172, 186,2998,1459, 443,1067,3251,1495, 321,1932,3054, 909, 753,1410,1828, 436,
    2441,1119,1587,3164,2186,1258, 227, 231,1425,1890,3200,3942, 247, 959, 725,5254,
    2741, 577,2158,2079, 929, 120, 174, 838,2813, 591,1115, 417,2024,  40,3240,1536,
    1037, 291,4151,2354, 632,1298,2406,2500,3535,1825,1846,3451, 205,1171, 345,4238,
      18,1163, 811, 685,2208,1217, 425,1312,1508,1175,4308,2552,1033, 587,1381,3059,
    2984,3482, 340,1316,4023,3972, 792,3176, 519, 777,4690, 918, 933,4130,2981,3741,
      90,3360,2911,2200,5184,4550, 609,3079,2030, 272,3379,2736, 363,3881,1130,1447,
     286, 779, 357,1169,3350,3137,1630,1220,2687,2391, 747,1277,3688,2618,2682,2601,
    1156,3196,5290,4034,3102,1689,3596,3128, 874, 219,2783, 798, 508,1843,2461, 269,
    1658,1776,1392,1913,2983,3287,2866,2159,2372, 829,4076,  46,4253,2873,1889,1894,
     915,1834,1631,2181,2318, 298, 664,2818,3555,2735, 954,3228,3117, 527,3511,2173,
     681,2712,3033,2247,2346,3467,1652, 155,2164,3382, 113,1994, 450, 899, 494, 994,
    1237,2958,1875,2336,1926,3727, 545,1577,1550, 633,3473, 204,1305,3072,2410,1956,
    2471, 707,2134, 841,2195,2196,2663,3843,1026,4940, 990,3252,4997, 368,1092, 437,
    3212,3258,1933,1829, 675,2977,2893, 412, 943,3723,4644,3294,3283,2230,2373,5154,
    2389,2241,2661,2323,1404,2524, 593, 787, 677,3008,1275,2059, 438,2709,2609,2240,
    2269,2246,1446,  36,1568,1373,3892,1574,2301,1456,3962, 693,2276,5216,2035,1143,
    2720,1919,1797,1811,2763,4137,2597,1830,1699,1488,1198,2090, 424,1694, 312,3634,
    3390,4179,3335,2252,1214, 561,1059,3243,2295,2561, 975,5155,2321,2751,3772, 472,
    1537,3282,3398,1047,2077,2348,2878,1323,3340,3076, 690,2906,  51, 369, 170,3541,
    1060,2187,2688,3670,2541,1083,1683, 928,3918, 459, 109,4427, 599,3744,4286, 143,
    2101,2730,2490,  82,1588,3036,2121, 281,1860, 477,4035,1238,2812,3020,2716,3312,
    1530,2188,2055,1317, 843, 636,1808,1173,3495, 649, 181,1002, 147,3641,1159,2414,
    3750,2289,2795, 813,3123,2610,1136,4368,   5,3391,4541,2174, 420, 429,1728, 754,
    1228,2115,2219, 347,2223,2733, 735,1518,3003,2355,3134,1764,3948,3329,1888,2424,
    1001,1234,1972,3321,3363,1672,1021,1450,1584, 226, 765, 655,2526,3404,3244,2302,
    3665, 731, 594,2184, 319,1576, 621, 658,2656,4299,2099,3864,1279,2071,2598,2739,
     795,3086,3699,3908,1707,2352,2402,1382,3136,2475,1465,4847,3496,3865,1085,3004,
    2591,1084, 213,2287,1963,3565,2250, 822, 793,4574,3187,1772,1789,3050, 595,1484,
    1959,2770,1080,2650, 456, 422,2996, 940,3322,4328,4345,3092,2742, 965,2784, 739,
    4124, 952,1358,2498,2949,2565, 332,2698,2378, 660,2260,2473,4194,3856,2919, 535,
    1260,2651,1208,1428,1300,1949,1303,2942, 433,2455,2450,1251,1946, 614,1269, 641,
    1306,1810,2737,3078,2912, 564,2365,1419,1415,1497,4460,2367,2185,1379,3005,1307,
    3218,2175,1897,3063, 682,1157,4040,4005,1712,1160,1941,1399, 394, 402,2952,1573,
    1151,2986,2404, 862, 299,2033,1489,3006, 346, 171,2886,3401,1726,2932, 168,2533,
      47,2507,1030,3735,1145,3370,1395,1318,1579,3609,4560,2857,4116,1457,2529,1965,
     504,1036,2690,2988,2405, 745,5871, 849,2397,2056,3081, 863,2359,3857,2096,  99,
    1397,1769,2300,4428,1643,3455,1978,1757,3718,1440,  35,4879,3742,1296,4228,2280,
     160,5063,1599,2013, 166, 520,3479,1646,3345,3012, 490,1937,1545,1264,2182,2505,
    1096,1188,1369,1436,2421,1667,2792,2460,1270,2122, 727,3167,2143, 806,1706,1012,
    1800,3037, 960,2218,1882, 805, 139,2456,1139,1521, 851,1052,3093,3089, 342,2039,
     744,5097,1468,1502,1585,2087, 223, 939, 326,2140,2577, 892,2481,1623,4077, 982,
    3708, 135,2131,  87,2503,3114,2326,1106, 876,1616, 547,2997,2831,2093,3441,4530,
    4314,   9,3256,4229,4148, 659,1462,1986,1710,2046,2913,2231,4090,4880,5255,3392,
    3274,1368,3689,4645,1477, 705,3384,3635,1068,1529,2941,1458,3782,1509, 100,1656,
    2548, 718,2339, 408,1590,2780,3548,1838,4117,3719,1345,3530, 717,3442,2778,3220,
    2898,1892,4590,3614,3371,2043,1998,1224,3483, 891, 635, 584,2559,3355, 733,1766,
    1729,1172,3789,1891,2307, 781,2982,2271,1957,1580,5773,2633,2005,4195,3097,1535,
    3213,1189,1934,5693,3262, 586,3118,1324,1598, 517,1564,2217,1868,1893,4445,3728,
    2703,3139,1526,1787,1992,3882,2875,1549,1199,1056,2224,1904,2711,5098,4287, 338,
    1993,3129,3489,2689,1809,2815,1997, 957,1855,3898,2550,3275,3057,1105,1319, 627,
    1505,1911,1883,3526, 698,3629,3456,1833,1431, 746,  77,1261,2017,2296,1977,1885,
     125,1334,1600, 525,1798,1109,2222,1470,1945, 559,2236,1186,3443,2476,1929,1411,
    2411,3135,1777,3372,2621,1841,1613,3229, 668,1430,1839,2643,2916, 195,1989,2671,
    2358,1387, 629,3205,2293,5256,4439, 123,1310, 888,1879,4300,3021,3605,1003,1162,
    3192,2910,2010, 140,2395,2859,  55,1082,2012,2901, 662, 419,2081,1438, 680,2774,
    4654,3912,1620,1731,1625,5035,4065,2328, 512,1344, 802,5443,2163,2311,2537, 524,
    3399,  98,1155,2103,1918,2606,3925,2816,1393,2465,1504,3773,2177,3963,1478,4346,
     180,1113,4655,3461,2028,1698, 833,2696,1235,1322,1594,4408,3623,3013,3225,2040,
    3022, 541,2881, 607,3632,2029,1665,1219, 639,1385,1686,1099,2803,3231,1938,3188,
    2858, 427, 676,2772,1168,2025, 454,3253,2486,3556, 230,1950, 580, 791,1991,1280,
    1086,1974,2034, 630, 257,3338,2788,4903,1017,  86,4790, 966,2789,1995,1696,1131,
     259,3095,4188,1308, 179,1463,5257, 289,4107,1248,  42,3413,1725,2288, 896,1947,
     774,4474,4254, 604,3430,4264, 392,2514,2588, 452, 237,1408,3018, 988,4531,1970,
    3034,3310, 540,2370,1562,1288,2990, 502,4765,1147,   4,1853,2708, 207, 294,2814,
    4078,2902,2509, 684,  34,3105,3532,2551, 644, 709,2801,2344, 573,1727,3573,3557,
    2021,1081,3100,4315,2100,3681, 199,2263,1837,2385, 146,3484,1195,2776,3949, 997,
    1939,3973,1008,1091,1202,1962,1847,1149,4209,5444,1076, 493, 117,5400,2521, 972,
    1490,2934,1796,4542,2374,1512,2933,2657, 413,2888,1135,2762,2314,2156,1355,2369,
     766,2007,2527,2170,3124,2491,2593,2632,4757,2437, 234,3125,3591,1898,1750,1376,
    1942,3468,3138, 570,2127,2145,3276,4131, 962, 132,1445,4196,  19, 941,3624,3480,
    3366,1973,1374,4461,3431,2629, 283,2415,2275, 808,2887,3620,2112,2563,1353,3610,
     955,1089,3103,1053,  96,  88,4097, 823,3808,1583, 399, 292,4091,3313, 421,1128,
     642,4006, 903,2539,1877,2082, 596,  29,4066,1790, 722,2157, 130, 995,1569, 769,
    1485, 464, 513,2213, 288,1923,1101,2453,4316, 133, 486,2445,  50, 625, 487,2207,
      57, 423, 481,2962, 159,3729,1558, 491, 303, 482, 501, 240,2837, 112,3648,2392,
    1783, 362,   8,3433,3422, 610,2793,3277,1390,1284,1654,  21,3823, 734, 367, 623,
     193, 287, 374,1009,1483, 816, 476, 313,2255,2340,1262,2150,2899,1146,2581, 782,
    2116,1659,2018,1880, 255,3586,3314,1110,2867,2137,2564, 986,2767,5185,2006, 650,
     158, 926, 762, 881,3157,2717,2362,3587, 306,3690,3245,1542,3077,2427,1691,2478,
    2118,2985,3490,2438, 539,2305, 983, 129,1754, 355,4201,2386, 827,2923, 104,1773,
    2838,2771, 411,2905,3919, 376, 767, 122,1114, 828,2422,1817,3506, 266,3460,1007,
    1609,4998, 945,2612,4429,2274, 726,1247,1964,2914,2199,2070,4002,4108, 657,3323,
    1422, 579, 455,2764,4737,1222,2895,1670, 824,1223,1487,2525, 558, 861,3080, 598,
    2659,2515,1967, 752,2583,2376,2214,4180, 977, 704,2464,4999,2622,4109,1210,2961,
     819,1541, 142,2284,  44, 418, 457,1126,3730,4347,4626,1644,1876,3671,1864, 302,
    1063,5694, 624, 723,1984,3745,1314,1676,2488,1610,1449,3558,3569,2166,2098, 409,
    1011,2325,3704,2306, 818,1732,1383,1824,1844,3757, 999,2705,3497,1216,1423,2683,
    2426,2954,2501,2726,2229,1475,2554,5064,1971,1794,1666,2014,1343, 783, 724, 191,
    2434,1354,2220,5065,1763,2752,2472,4152, 131, 175,2885,3434,  92,1466,4920,2616,
    3871,3872,3866, 128,1551,1632, 669,1854,3682,4691,4125,1230, 188,2973,3290,1302,
    1213, 560,3266, 917, 763,3909,3249,1760, 868,1958, 764,1782,2097, 145,2277,3774,
    4462,  64,1491,3062, 971,2132,3606,2442, 221,1226,1617, 218, 323,1185,3207,3147,
     571, 619,1473,1005,1744,2281, 449,1887,2396,3685, 275, 375,3816,1743,3844,3731,
     845,1983,2350,4210,1377, 773, 967,3499,3052,3743,2725,4007,1697,1022,3943,1464,
    3264,2855,2722,1952,1029,2839,2467,  84,4383,2215, 820,1391,2015,2448,3672, 377,
    1948,2168, 797,2545,3536,2578,2645,  94,2874,1678, 405,1259,3071, 771, 546,1315,
     470,1243,3083, 895,2468, 981, 969,2037, 846,4181, 653,1276,2928,  14,2594, 557,
    3007,2474, 156, 902,1338,1740,2574, 537,2518, 973,2282,2216,2433,1928, 138,2903,
    1293,2631,1612, 646,3457, 839,2935, 111, 496,2191,2847, 589,3186, 149,3994,2060,
    4031,2641,4067,3145,1870,  37,3597,2136,1025,2051,3009,3383,3549,1121,1016,3261,
    1301, 251,2446,2599,2153, 872,3246, 637, 334,3705, 831, 884, 921,3065,3140,4092,
    2198,1944, 246,2964, 108,2045,1152,1921,2308,1031, 203,3173,4170,1907,3890, 810,
    1401,2003,1690, 506, 647,1242,2828,1761,1649,3208,2249,1589,3709,2931,5156,1708,
     498, 666,2613, 834,3817,1231, 184,2851,1124, 883,3197,2261,3710,1765,1553,2658,
    1178,2639,2351,  93,1193, 942,2538,2141,4402, 235,1821, 870,1591,2192,1709,1871,
    3341,1618,4126,2595,2334, 603, 651,  69, 701, 268,2662,3411,2555,1380,1606, 503,
     448, 254,2371,2646, 574,1187,2309,1770, 322,2235,1292,1801, 305, 566,1133, 229,
    2067,2057, 706, 167, 483,2002,2672,3295,1820,3561,3067, 316, 378,2746,3452,1112,
     136,1981, 507,1651,2917,1117, 285,4591, 182,2580,3522,1304, 335,3303,1835,2504,
    1795,1792,2248, 674,1018,2106,2449,1857,2292,2845, 976,3047,1781,2600,2727,1389,
    1281,  52,3152, 153, 265,3950, 672,3485,3951,4463, 430,1183, 365, 278,2169,  27,
    1407,1336,2304, 209,1340,1730,2202,1852,2403,2883, 979,1737,1062, 631,2829,2542,
    3876,2592, 825,2086,2226,3048,3625, 352,1417,3724, 542, 991, 431,1351,3938,1861,
    2294, 826,1361,2927,3142,3503,1738, 463,2462,2723, 582,1916,1595,2808, 400,3845,
    3891,2868,3621,2254,  58,2492,1123, 910,2160,2614,1372,1603,1196,1072,3385,1700,
    3267,1980, 696, 480,2430, 920, 799,1570,2920,1951,2041,4047,2540,1321,4223,2469,
    3562,2228,1271,2602, 401,2833,3351,2575,5157, 907,2312,1256, 410, 263,3507,1582,
     996, 678,1849,2316,1480, 908,3545,2237, 703,2322, 667,1826,2849,1531,2604,2999,
    2407,3146,2151,2630,1786,3711, 469,3542, 497,3899,2409, 858, 837,4446,3393,1274,
     786, 620,1845,2001,3311, 484, 308,3367,1204,1815,3691,2332,1532,2557,1842,2020,
    2724,1927,2333,4440, 567,  22,1673,2728,4475,1987,1858,1144,1597, 101,1832,3601,
      12, 974,3783,4391, 951,1412,   1,3720, 453,4608,4041, 528,1041,1027,3230,2628,
    1129, 875,1051,3291,1203,2262,1069,2860,2799,2149,2615,3278, 144,1758,3040,  31,
     475,1680, 366,2685,3184, 311,1642,4008,2466,5036,1593,1493,2809, 216,1420,1668,
     233, 304,2128,3284, 232,1429,1768,1040,2008,3407,2740,2967,2543, 242,2133, 778,
    1565,2022,2620, 505,2189,2756,1098,2273, 372,1614, 708, 553,2846,2094,2278, 169,
    3626,2835,4161, 228,2674,3165, 809,1454,1309, 466,1705,1095, 900,3423, 880,2667,
    3751,5258,2317,3109,2571,4317,2766,1503,1342, 866,4447,1118,  63,2076, 314,1881,
    1348,1061, 172, 978,3515,1747, 532, 511,3970,   6, 601, 905,2699,3300,1751, 276,
    1467,3725,2668,  65,4239,2544,2779,2556,1604, 578,2451,1802, 992,2331,2624,1320,
    3446, 713,1513,1013, 103,2786,2447,1661, 886,1702, 916, 654,3574,2031,1556, 751,
    2178,2821,2179,1498,1538,2176, 271, 914,2251,2080,1325, 638,1953,2937,3877,2432,
    2754,  95,3265,1716, 260,1227,4083, 775, 106,1357,3254, 426,1607, 555,2480, 772,
    1985, 244,2546, 474, 495,1046,2611,1851,2061,  71,2089,1675,2590, 742,3758,2843,
    3222,1433, 267,2180,2576,2826,2233,2092,3913,2435, 956,1745,3075, 856,2113,1116,
     451,   3,1988,2896,1398, 993,2463,1878,2049,1341,2718,2721,2870,2108, 712,2904,
    4363,2753,2324, 277,2872,2349,2649, 384, 987, 435, 691,3000, 922, 164,3939, 652,
    1500,1184,4153,2482,3373,2165,4848,2335,3775,3508,3154,2806,2830,1554,2102,1664,
    2530,1434,2408, 893,1547,2623,3447,2832,2242,2532,3169,2856,3223,2078,  49,3770,
    3469, 462, 318, 656,2259,3250,3069, 679,1629,2758, 344,1138,1104,3120,1836,1283,
    3115,2154,1437,4448, 934, 759,1999, 794,2862,1038, 533,2560,1722,2342, 855,2626,
    1197,1663,4476,3127,  85,4240,2528,  25,1111,1181,3673, 407,3470,4561,2679,2713,
     768,1925,2841,3986,1544,1165, 932, 373,1240,2146,1930,2673, 721,4766, 354,4333,
     391,2963, 187,  61,3364,1442,1102, 330,1940,1767, 341,3809,4118, 393,2496,2062,
    2211, 105, 331, 300, 439, 913,1332, 626, 379,3304,1557, 328, 689,3952, 309,1555,
     931, 317,2517,3027, 325, 569, 686,2107,3084,  60,1042,1333,2794, 264,3177,4014,
    1628, 258,3712,   7,4464,1176,1043,1778, 683, 114,1975,  78,1492, 383,1886, 510,
     386, 645,5291,2891,2069,3305,4138,3867,2939,2603,2493,1935,1066,1848,3588,1015,
    1282,1289,4609, 697,1453,3044,2666,3611,1856,2412,  54, 719,1330, 568,3778,2459,
    1748, 788, 492, 551,1191,1000, 488,3394,3763, 282,1799, 348,2016,1523,3155,2390,
    1049, 382,2019,1788,1170, 729,2968,3523, 897,3926,2785,2938,3292, 350,2319,3238,
    1718,1717,2655,3453,3143,4465, 161,2889,2980,2009,1421,  56,1908,1640,2387,2232,
    1917,1874,2477,4921, 148,  83,3438, 592,4245,2882,1822,1055, 741, 115,1496,1624,
     381,1638,4592,1020, 516,3214, 458, 947,4575,1432, 211,1514,2926,1865,2142, 189,
     852,1221,1400,1486, 882,2299,4036, 351,  28,1122, 700,6479,6480,6481,6482,6483);
end;

function TdxGB18030CharDistributionAnalyzer.GetOrder(const AStr: TArray<Byte>; AOffset: Integer): Integer;
begin
  if (AStr[AOffset] >= $b0) and (AStr[AOffset + 1] >= $a1) then
    Result := 94 * (AStr[AOffset] - $b0) + AStr[AOffset + 1] - $a1
  else
    Result := -1;
end;

function TdxGB18030CharDistributionAnalyzer.GetCharToFreqOrder: TArray<Word>;
begin
  Result := FCharToFreqOrder;
end;

function TdxGB18030CharDistributionAnalyzer.GetTypicalDistributionRatio: Single;
begin
  Result := 0.9;
end;

{ TdxGB18030EncodingAnalyzer }

class constructor TdxGB18030EncodingAnalyzer.Initialize;
begin
  FClassTable := TArray<Integer>.Create(
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,0,0),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,0,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(3,3,3,3,3,3,3,3),
    Pack4Bits(3,3,1,1,1,1,1,1),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,4),
    Pack4Bits(5,6,6,6,6,6,6,6),
    Pack4Bits(6,6,6,6,6,6,6,6),
    Pack4Bits(6,6,6,6,6,6,6,6),
    Pack4Bits(6,6,6,6,6,6,6,6),
    Pack4Bits(6,6,6,6,6,6,6,6),
    Pack4Bits(6,6,6,6,6,6,6,6),
    Pack4Bits(6,6,6,6,6,6,6,6),
    Pack4Bits(6,6,6,6,6,6,6,6),
    Pack4Bits(6,6,6,6,6,6,6,6),
    Pack4Bits(6,6,6,6,6,6,6,6),
    Pack4Bits(6,6,6,6,6,6,6,6),
    Pack4Bits(6,6,6,6,6,6,6,6),
    Pack4Bits(6,6,6,6,6,6,6,6),
    Pack4Bits(6,6,6,6,6,6,6,6),
    Pack4Bits(6,6,6,6,6,6,6,6),
    Pack4Bits(6,6,6,6,6,6,6,0));
  FStateTable := TArray<Integer>.Create(
    Pack4Bits(EAStateError, EAStateStart, EAStateStart, EAStateStart, EAStateStart, EAStateStart,            3, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateItsMe, EAStateItsMe),
    Pack4Bits(EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateError, EAStateError, EAStateStart),
    Pack4Bits(           4, EAStateError, EAStateStart, EAStateStart, EAStateError, EAStateError, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError,            5, EAStateError, EAStateError, EAStateError, EAStateItsMe, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateStart, EAStateStart, EAStateStart, EAStateStart, EAStateStart, EAStateStart));
  FCharLenTable := TArray<Integer>.Create(0, 1, 1, 1, 1, 1, 2);
end;

function TdxGB18030EncodingAnalyzer.GetClassTable: TArray<Integer>;
begin
  Result := FClassTable;
end;

function TdxGB18030EncodingAnalyzer.GetStateTable: TArray<Integer>;
begin
  Result := FStateTable;
end;

function TdxGB18030EncodingAnalyzer.GetCharLenTable: TArray<Integer>;
begin
  Result := FCharLenTable;
end;

{ TdxEucKrEncodingDetector }

function TdxEucKrEncodingDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.GetEncoding(51949);
end;

function TdxEucKrEncodingDetector.CreateDistributionAnalyzer: TdxCharacterDistributionAnalyzer;
begin
  Result := TdxEucKrCharDistributionAnalyzer.Create;
end;

function TdxEucKrEncodingDetector.CreateEncodingAnalyzer: TdxEncodingAnalyzer;
begin
  Result := TdxEucKrEncodingAnalyzer.Create;
end;

{ TdxEucKrCharDistributionAnalyzer }

class constructor TdxEucKrCharDistributionAnalyzer.Initialize;
begin
  FCharToFreqOrder := TArray<Word>.Create(
      13, 130, 120,1396, 481,1719,1720, 328, 609, 212,1721, 707, 400, 299,1722,  87,
    1397,1723, 104, 536,1117,1203,1724,1267, 685,1268, 508,1725,1726,1727,1728,1398,
    1399,1729,1730,1731, 141, 621, 326,1057, 368,1732, 267, 488,  20,1733,1269,1734,
     945,1400,1735,  47, 904,1270,1736,1737, 773, 248,1738, 409, 313, 786, 429,1739,
     116, 987, 813,1401, 683,  75,1204, 145,1740,1741,1742,1743,  16, 847, 667, 622,
     708,1744,1745,1746, 966, 787, 304, 129,1747,  60, 820, 123, 676,1748,1749,1750,
    1751, 617,1752, 626,1753,1754,1755,1756, 653,1757,1758,1759,1760,1761,1762, 856,
     344,1763,1764,1765,1766,  89, 401, 418, 806, 905, 848,1767,1768,1769, 946,1205,
     709,1770,1118,1771, 241,1772,1773,1774,1271,1775, 569,1776, 999,1777,1778,1779,
    1780, 337, 751,1058,  28, 628, 254,1781, 177, 906, 270, 349, 891,1079,1782,  19,
    1783, 379,1784, 315,1785, 629, 754,1402, 559,1786, 636, 203,1206,1787, 710, 567,
    1788, 935, 814,1789,1790,1207, 766, 528,1791,1792,1208,1793,1794,1795,1796,1797,
    1403,1798,1799, 533,1059,1404,1405,1156,1406, 936, 884,1080,1800, 351,1801,1802,
    1803,1804,1805, 801,1806,1807,1808,1119,1809,1157, 714, 474,1407,1810, 298, 899,
     885,1811,1120, 802,1158,1812, 892,1813,1814,1408, 659,1815,1816,1121,1817,1818,
    1819,1820,1821,1822, 319,1823, 594, 545,1824, 815, 937,1209,1825,1826, 573,1409,
    1022,1827,1210,1828,1829,1830,1831,1832,1833, 556, 722, 807,1122,1060,1834, 697,
    1835, 900, 557, 715,1836,1410, 540,1411, 752,1159, 294, 597,1211, 976, 803, 770,
    1412,1837,1838,  39, 794,1413, 358,1839, 371, 925,1840, 453, 661, 788, 531, 723,
     544,1023,1081, 869,  91,1841, 392, 430, 790, 602,1414, 677,1082, 457,1415,1416,
    1842,1843, 475, 327,1024,1417, 795, 121,1844, 733, 403,1418,1845,1846,1847, 300,
     119, 711,1212, 627,1848,1272, 207,1849,1850, 796,1213, 382,1851, 519,1852,1083,
     893,1853,1854,1855, 367, 809, 487, 671,1856, 663,1857,1858, 956, 471, 306, 857,
    1859,1860,1160,1084,1861,1862,1863,1864,1865,1061,1866,1867,1868,1869,1870,1871,
     282,  96, 574,1872, 502,1085,1873,1214,1874, 907,1875,1876, 827, 977,1419,1420,
    1421, 268,1877,1422,1878,1879,1880, 308,1881,   2, 537,1882,1883,1215,1884,1885,
     127, 791,1886,1273,1423,1887,  34, 336, 404, 643,1888, 571, 654, 894, 840,1889,
       0, 886,1274, 122, 575, 260, 908, 938,1890,1275, 410, 316,1891,1892, 100,1893,
    1894,1123,  48,1161,1124,1025,1895, 633, 901,1276,1896,1897, 115, 816,1898, 317,
    1899, 694,1900, 909, 734,1424, 572, 866,1425, 691,  85, 524,1010, 543, 394, 841,
    1901,1902,1903,1026,1904,1905,1906,1907,1908,1909,  30, 451, 651, 988, 310,1910,
    1911,1426, 810,1216,  93,1912,1913,1277,1217,1914, 858, 759,  45,  58, 181, 610,
     269,1915,1916, 131,1062, 551, 443,1000, 821,1427, 957, 895,1086,1917,1918, 375,
    1919, 359,1920, 687,1921, 822,1922, 293,1923,1924,  40, 662, 118, 692,  29, 939,
     887, 640, 482, 174,1925,  69,1162, 728,1428, 910,1926,1278,1218,1279, 386, 870,
     217, 854,1163, 823,1927,1928,1929,1930, 834,1931,  78,1932, 859,1933,1063,1934,
    1935,1936,1937, 438,1164, 208, 595,1938,1939,1940,1941,1219,1125,1942, 280, 888,
    1429,1430,1220,1431,1943,1944,1945,1946,1947,1280, 150, 510,1432,1948,1949,1950,
    1951,1952,1953,1954,1011,1087,1955,1433,1043,1956, 881,1957, 614, 958,1064,1065,
    1221,1958, 638,1001, 860, 967, 896,1434, 989, 492, 553,1281,1165,1959,1282,1002,
    1283,1222,1960,1961,1962,1963,  36, 383, 228, 753, 247, 454,1964, 876, 678,1965,
    1966,1284, 126, 464, 490, 835, 136, 672, 529, 940,1088,1435, 473,1967,1968, 467,
      50, 390, 227, 587, 279, 378, 598, 792, 968, 240, 151, 160, 849, 882,1126,1285,
     639,1044, 133, 140, 288, 360, 811, 563,1027, 561, 142, 523,1969,1970,1971,   7,
     103, 296, 439, 407, 506, 634, 990,1972,1973,1974,1975, 645,1976,1977,1978,1979,
    1980,1981, 236,1982,1436,1983,1984,1089, 192, 828, 618, 518,1166, 333,1127,1985,
     818,1223,1986,1987,1988,1989,1990,1991,1992,1993, 342,1128,1286, 746, 842,1994,
    1995, 560, 223,1287,  98,   8, 189, 650, 978,1288,1996,1437,1997,  17, 345, 250,
     423, 277, 234, 512, 226,  97, 289,  42, 167,1998, 201,1999,2000, 843, 836, 824,
     532, 338, 783,1090, 182, 576, 436,1438,1439, 527, 500,2001, 947, 889,2002,2003,
    2004,2005, 262, 600, 314, 447,2006, 547,2007, 693, 738,1129,2008,  71,1440, 745,
     619, 688,2009, 829,2010,2011, 147,2012,  33, 948,2013,2014,  74, 224,2015,  61,
     191, 918, 399, 637,2016,1028,1130, 257, 902,2017,2018,2019,2020,2021,2022,2023,
    2024,2025,2026, 837,2027,2028,2029,2030, 179, 874, 591,  52, 724, 246,2031,2032,
    2033,2034,1167, 969,2035,1289, 630, 605, 911,1091,1168,2036,2037,2038,1441, 912,
    2039, 623,2040,2041, 253,1169,1290,2042,1442, 146, 620, 611, 577, 433,2043,1224,
     719,1170, 959, 440, 437, 534,  84, 388, 480,1131, 159, 220, 198, 679,2044,1012,
     819,1066,1443, 113,1225, 194, 318,1003,1029,2045,2046,2047,2048,1067,2049,2050,
    2051,2052,2053,  59, 913, 112,2054, 632,2055, 455, 144, 739,1291,2056, 273, 681,
     499,2057, 448,2058,2059, 760,2060,2061, 970, 384, 169, 245,1132,2062,2063, 414,
    1444,2064,2065,  41, 235,2066, 157, 252, 877, 568, 919, 789, 580,2067, 725,2068,
    2069,1292,2070,2071,1445,2072,1446,2073,2074,  55, 588,  66,1447, 271,1092,2075,
    1226,2076, 960,1013, 372,2077,2078,2079,2080,2081,1293,2082,2083,2084,2085, 850,
    2086,2087,2088,2089,2090, 186,2091,1068, 180,2092,2093,2094, 109,1227, 522, 606,
    2095, 867,1448,1093, 991,1171, 926, 353,1133,2096, 581,2097,2098,2099,1294,1449,
    1450,2100, 596,1172,1014,1228,2101,1451,1295,1173,1229,2102,2103,1296,1134,1452,
     949,1135,2104,2105,1094,1453,1454,1455,2106,1095,2107,2108,2109,2110,2111,2112,
    2113,2114,2115,2116,2117, 804,2118,2119,1230,1231, 805,1456, 405,1136,2120,2121,
    2122,2123,2124, 720, 701,1297, 992,1457, 927,1004,2125,2126,2127,2128,2129,2130,
      22, 417,2131, 303,2132, 385,2133, 971, 520, 513,2134,1174,  73,1096, 231, 274,
     962,1458, 673,2135,1459,2136, 152,1137,2137,2138,2139,2140,1005,1138,1460,1139,
    2141,2142,2143,2144,  11, 374, 844,2145, 154,1232,  46,1461,2146, 838, 830, 721,
    1233, 106,2147,  90, 428, 462, 578, 566,1175, 352,2148,2149, 538,1234, 124,1298,
    2150,1462, 761, 565,2151, 686,2152, 649,2153,  72, 173,2154, 460, 415,2155,1463,
    2156,1235, 305,2157,2158,2159,2160,2161,2162, 579,2163,2164,2165,2166,2167, 747,
    2168,2169,2170,2171,1464, 669,2172,2173,2174,2175,2176,1465,2177,  23, 530, 285,
    2178, 335, 729,2179, 397,2180,2181,2182,1030,2183,2184, 698,2185,2186, 325,2187,
    2188, 369,2189, 799,1097,1015, 348,2190,1069, 680,2191, 851,1466,2192,2193,  10,
    2194, 613, 424,2195, 979, 108, 449, 589,  27, 172,  81,1031,  80, 774, 281, 350,
    1032, 525, 301, 582,1176,2196, 674,1045,2197,2198,1467, 730, 762,2199,2200,2201,
    2202,1468,2203, 993,2204,2205, 266,1070, 963,1140,2206,2207,2208, 664,1098, 972,
    2209,2210,2211,1177,1469,1470, 871,2212,2213,2214,2215,2216,1471,2217,2218,2219,
    2220,2221,2222,2223,2224,2225,2226,2227,1472,1236,2228,2229,2230,2231,2232,2233,
    2234,2235,1299,2236,2237, 200,2238, 477, 373,2239,2240, 731, 825, 777,2241,2242,
    2243, 521, 486, 548,2244,2245,2246,1473,1300,  53, 549, 137, 875,  76, 158,2247,
    1301,1474, 469, 396,1016, 278, 712,2248, 321, 442, 503, 767, 744, 941,1237,1178,
    1475,2249,  82, 178,1141,1179, 973,2250,1302,2251, 297,2252,2253, 570,2254,2255,
    2256,  18, 450, 206,2257, 290, 292,1142,2258, 511, 162,  99, 346, 164, 735,2259,
    1476,1477,   4, 554, 343, 798,1099,2260,1100,2261,  43, 171,1303, 139, 215,2262,
    2263, 717, 775,2264,1033, 322, 216,2265, 831,2266, 149,2267,1304,2268,2269, 702,
    1238, 135, 845, 347, 309,2270, 484,2271, 878, 655, 238,1006,1478,2272,  67,2273,
     295,2274,2275, 461,2276, 478, 942, 412,2277,1034,2278,2279,2280, 265,2281, 541,
    2282,2283,2284,2285,2286,  70, 852,1071,2287,2288,2289,2290,  21,  56, 509, 117,
     432,2291,2292, 331, 980, 552,1101, 148, 284, 105, 393,1180,1239, 755,2293, 187,
    2294,1046,1479,2295, 340,2296,  63,1047, 230,2297,2298,1305, 763,1306, 101, 800,
     808, 494,2299,2300,2301, 903,2302,  37,1072,  14,   5,2303,  79, 675,2304, 312,
    2305,2306,2307,2308,2309,1480,   6,1307,2310,2311,2312,   1, 470,  35,  24, 229,
    2313, 695, 210,  86, 778,  15, 784, 592, 779,  32,  77, 855, 964,2314, 259,2315,
     501, 380,2316,2317,  83, 981, 153, 689,1308,1481,1482,1483,2318,2319, 716,1484,
    2320,2321,2322,2323,2324,2325,1485,2326,2327, 128,  57,  68, 261,1048, 211, 170,
    1240,  31,2328,  51, 435, 742,2329,2330,2331, 635,2332, 264, 456,2333,2334,2335,
     425,2336,1486, 143, 507, 263, 943,2337, 363, 920,1487, 256,1488,1102, 243, 601,
    1489,2338,2339,2340,2341,2342,2343,2344, 861,2345,2346,2347,2348,2349,2350, 395,
    2351,1490,1491,  62, 535, 166, 225,2352,2353, 668, 419,1241, 138, 604, 928,2354,
    1181,2355,1492,1493,2356,2357,2358,1143,2359, 696,2360, 387, 307,1309, 682, 476,
    2361,2362, 332,  12, 222, 156,2363, 232,2364, 641, 276, 656, 517,1494,1495,1035,
     416, 736,1496,2365,1017, 586,2366,2367,2368,1497,2369, 242,2370,2371,2372,1498,
    2373, 965, 713,2374,2375,2376,2377, 740, 982,1499, 944,1500,1007,2378,2379,1310,
    1501,2380,2381,2382, 785, 329,2383,2384,1502,2385,2386,2387, 932,2388,1503,2389,
    2390,2391,2392,1242,2393,2394,2395,2396,2397, 994, 950,2398,2399,2400,2401,1504,
    1311,2402,2403,2404,2405,1049, 749,2406,2407, 853, 718,1144,1312,2408,1182,1505,
    2409,2410, 255, 516, 479, 564, 550, 214,1506,1507,1313, 413, 239, 444, 339,1145,
    1036,1508,1509,1314,1037,1510,1315,2411,1511,2412,2413,2414, 176, 703, 497, 624,
     593, 921, 302,2415, 341, 165,1103,1512,2416,1513,2417,2418,2419, 376,2420, 700,
    2421,2422,2423, 258, 768,1316,2424,1183,2425, 995, 608,2426,2427,2428,2429, 221,
    2430,2431,2432,2433,2434,2435,2436,2437, 195, 323, 726, 188, 897, 983,1317, 377,
     644,1050, 879,2438, 452,2439,2440,2441,2442,2443,2444, 914,2445,2446,2447,2448,
     915, 489,2449,1514,1184,2450,2451, 515,  64, 427, 495,2452, 583,2453, 483, 485,
    1038, 562, 213,1515, 748, 666,2454,2455,2456,2457, 334,2458, 780, 996,1008, 705,
    1243,2459,2460,2461,2462,2463, 114,2464, 493,1146, 366, 163,1516, 961,1104,2465,
     291,2466,1318,1105,2467,1517, 365,2468, 355, 951,1244,2469,1319,2470, 631,2471,
    2472, 218,1320, 364, 320, 756,1518,1519,1321,1520,1322,2473,2474,2475,2476, 997,
    2477,2478,2479,2480, 665,1185,2481, 916,1521,2482,2483,2484, 584, 684,2485,2486,
     797,2487,1051,1186,2488,2489,2490,1522,2491,2492, 370,2493,1039,1187,  65,2494,
     434, 205, 463,1188,2495, 125, 812, 391, 402, 826, 699, 286, 398, 155, 781, 771,
     585,2496, 590, 505,1073,2497, 599, 244, 219, 917,1018, 952, 646,1523,2498,1323,
    2499,2500,  49, 984, 354, 741,2501, 625,2502,1324,2503,1019, 190, 357, 757, 491,
      95, 782, 868,2504,2505,2506,2507,2508,2509, 134,1524,1074, 422,1525, 898,2510,
     161,2511,2512,2513,2514, 769,2515,1526,2516,2517, 411,1325,2518, 472,1527,2519,
    2520,2521,2522,2523,2524, 985,2525,2526,2527,2528,2529,2530, 764,2531,1245,2532,
    2533,  25, 204, 311,2534, 496,2535,1052,2536,2537,2538,2539,2540,2541,2542, 199,
     704, 504, 468, 758, 657,1528, 196,  44, 839,1246, 272, 750,2543, 765, 862,2544,
    2545,1326,2546, 132, 615, 933,2547, 732,2548,2549,2550,1189,1529,2551, 283,1247,
    1053, 607, 929,2552,2553,2554, 930, 183, 872, 616,1040,1147,2555,1148,1020, 441,
     249,1075,2556,2557,2558, 466, 743,2559,2560,2561,  92, 514, 426, 420, 526,2562,
    2563,2564,2565,2566,2567,2568, 185,2569,2570,2571,2572, 776,1530, 658,2573, 362,
    2574, 361, 922,1076, 793,2575,2576,2577,2578,2579,2580,1531, 251,2581,2582,2583,
    2584,1532,  54, 612, 237,1327,2585,2586, 275, 408, 647, 111,2587,1533,1106, 465,
       3, 458,   9,  38,2588, 107, 110, 890, 209,  26, 737, 498,2589,1534,2590, 431,
     202,  88,1535, 356, 287,1107, 660,1149,2591, 381,1536, 986,1150, 445,1248,1151,
     974,2592,2593, 846,2594, 446, 953, 184,1249,1250, 727,2595, 923, 193, 883,2596,
    2597,2598, 102, 324, 539, 817,2599, 421,1041,2600, 832,2601,  94, 175, 197, 406,
    2602, 459,2603,2604,2605,2606,2607, 330, 555,2608,2609,2610, 706,1108, 389,2611,
    2612,2613,2614, 233,2615, 833, 558, 931, 954,1251,2616,2617,1537, 546,2618,2619,
    1009,2620,2621,2622,1538, 690,1328,2623, 955,2624,1539,2625,2626, 772,2627,2628,
    2629,2630,2631, 924, 648, 863, 603,2632,2633, 934,1540, 864, 865,2634, 642,1042,
     670,1190,2635,2636,2637,2638, 168,2639, 652, 873, 542,1054,1541,2640,2641,2642);
end;

function TdxEucKrCharDistributionAnalyzer.GetOrder(const AStr: TArray<Byte>; AOffset: Integer): Integer;
begin
  if AStr[AOffset] >= $b0 then
    Result := 94 * (AStr[AOffset] - $b0) + AStr[AOffset + 1] - $a1
  else
    Result := -1;
end;

function TdxEucKrCharDistributionAnalyzer.GetCharToFreqOrder: TArray<Word>;
begin
  Result := FCharToFreqOrder;
end;

function TdxEucKrCharDistributionAnalyzer.GetTypicalDistributionRatio: Single;
begin
  Result := 6.0;
end;

{ TdxEucKrEncodingAnalyzer }

class constructor TdxEucKrEncodingAnalyzer.Initialize;
begin
  FClassTable := TArray<Integer>.Create(
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,0,0),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,0,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(0,0,0,0,0,0,0,0),
    Pack4Bits(0,0,0,0,0,0,0,0),
    Pack4Bits(0,0,0,0,0,0,0,0),
    Pack4Bits(0,0,0,0,0,0,0,0),
    Pack4Bits(0,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,3,3,3),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,3,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,0));
  FStateTable := TArray<Integer>.Create(
    Pack4Bits(EAStateError, EAStateStart,            3, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError),
    Pack4Bits(EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateError, EAStateError, EAStateStart, EAStateStart));
  FCharLenTable := TArray<Integer>.Create(0, 1, 2, 0);
end;

function TdxEucKrEncodingAnalyzer.GetClassTable: TArray<Integer>;
begin
  Result := FClassTable;
end;

function TdxEucKrEncodingAnalyzer.GetStateTable: TArray<Integer>;
begin
  Result := FStateTable;
end;

function TdxEucKrEncodingAnalyzer.GetCharLenTable: TArray<Integer>;
begin
  Result := FCharLenTable;
end;

{ TdxJapaneseEncodingDetector }

constructor TdxJapaneseEncodingDetector.Create(APreferredLanguage: Boolean);
begin
  inherited Create(APreferredLanguage);
  FContextAnalyzer := CreateContextAnalyzer;
end;

destructor TdxJapaneseEncodingDetector.Destroy;
begin
  FContextAnalyzer.Free;
  inherited Destroy;
end;

procedure TdxJapaneseEncodingDetector.ContinueContextAnalysis(const ABuffer: TArray<Byte>; AFrom, ALength: Integer);
begin
  FContextAnalyzer.AnalyzeCharacter(ABuffer, AFrom, ALength);
end;

function TdxJapaneseEncodingDetector.GetContextAnalyzerConfidence(AIsPreferredLanguage: Boolean): Single;
begin
  Result := FContextAnalyzer.GetConfidence(AIsPreferredLanguage);
end;

function TdxJapaneseEncodingDetector.GotEnoughData: Boolean;
begin
  Result := FContextAnalyzer.GotEnoughData;
end;

{ TdxJapaneseCharDistributionAnalyzer }

class constructor TdxJapaneseCharDistributionAnalyzer.Initialize;
begin
  FCharToFreqOrder := TArray<Word>.Create(
      40,   1,   6, 182, 152, 180, 295,2127, 285, 381,3295,4304,3068,4606,3165,3510,
    3511,1822,2785,4607,1193,2226,5070,4608, 171,2996,1247,  18, 179,5071, 856,1661,
    1262,5072, 619, 127,3431,3512,3230,1899,1700, 232, 228,1294,1298, 284, 283,2041,
    2042,1061,1062,  48,  49,  44,  45, 433, 434,1040,1041, 996, 787,2997,1255,4305,
    2108,4609,1684,1648,5073,5074,5075,5076,5077,5078,3687,5079,4610,5080,3927,3928,
    5081,3296,3432, 290,2285,1471,2187,5082,2580,2825,1303,2140,1739,1445,2691,3375,
    1691,3297,4306,4307,4611, 452,3376,1182,2713,3688,3069,4308,5083,5084,5085,5086,
    5087,5088,5089,5090,5091,5092,5093,5094,5095,5096,5097,5098,5099,5100,5101,5102,
    5103,5104,5105,5106,5107,5108,5109,5110,5111,5112,4097,5113,5114,5115,5116,5117,
    5118,5119,5120,5121,5122,5123,5124,5125,5126,5127,5128,5129,5130,5131,5132,5133,
    5134,5135,5136,5137,5138,5139,5140,5141,5142,5143,5144,5145,5146,5147,5148,5149,
    5150,5151,5152,4612,5153,5154,5155,5156,5157,5158,5159,5160,5161,5162,5163,5164,
    5165,5166,5167,5168,5169,5170,5171,5172,5173,5174,5175,1472, 598, 618, 820,1205,
    1309,1412,1858,1307,1692,5176,5177,5178,5179,5180,5181,5182,1142,1452,1234,1172,
    1875,2043,2149,1793,1382,2973, 925,2404,1067,1241, 960,1377,2935,1491, 919,1217,
    1865,2030,1406,1499,2749,4098,5183,5184,5185,5186,5187,5188,2561,4099,3117,1804,
    2049,3689,4309,3513,1663,5189,3166,3118,3298,1587,1561,3433,5190,3119,1625,2998,
    3299,4613,1766,3690,2786,4614,5191,5192,5193,5194,2161,  26,3377,   2,3929,  20,
    3691,  47,4100,  50,  17,  16,  35, 268,  27, 243,  42, 155,  24, 154,  29, 184,
       4,  91,  14,  92,  53, 396,  33, 289,   9,  37,  64, 620,  21,  39, 321,   5,
      12,  11,  52,  13,   3, 208, 138,   0,   7,  60, 526, 141, 151,1069, 181, 275,
    1591,  83, 132,1475, 126, 331, 829,  15,  69, 160,  59,  22, 157,  55,1079, 312,
     109,  38,  23,  25,  10,  19,  79,5195,  61, 382,1124,   8,  30,5196,5197,5198,
    5199,5200,5201,5202,5203,5204,5205,5206,  89,  62,  74,  34,2416, 112, 139, 196,
     271, 149,  84, 607, 131, 765,  46,  88, 153, 683,  76, 874, 101, 258,  57,  80,
      32, 364, 121,1508, 169,1547,  68, 235, 145,2999,  41, 360,3027,  70,  63,  31,
      43, 259, 262,1383,  99, 533, 194,  66,  93, 846, 217, 192,  56, 106,  58, 565,
     280, 272, 311, 256, 146,  82, 308,  71, 100, 128, 214, 655, 110, 261, 104,1140,
      54,  51,  36,  87,  67,3070, 185,2618,2936,2020,  28,1066,2390,2059,5207,5208,
    5209,5210,5211,5212,5213,5214,5215,5216,4615,5217,5218,5219,5220,5221,5222,5223,
    5224,5225,5226,5227,5228,5229,5230,5231,5232,5233,5234,5235,5236,3514,5237,5238,
    5239,5240,5241,5242,5243,5244,2297,2031,4616,4310,3692,5245,3071,5246,3598,5247,
    4617,3231,3515,5248,4101,4311,4618,3808,4312,4102,5249,4103,4104,3599,5250,5251,
    5252,5253,5254,5255,5256,5257,5258,5259,5260,5261,5262,5263,5264,5265,5266,5267,
    5268,5269,5270,5271,5272,5273,5274,5275,5276,5277,5278,5279,5280,5281,5282,5283,
    5284,5285,5286,5287,5288,5289,5290,5291,5292,5293,5294,5295,5296,5297,5298,5299,
    5300,5301,5302,5303,5304,5305,5306,5307,5308,5309,5310,5311,5312,5313,5314,5315,
    5316,5317,5318,5319,5320,5321,5322,5323,5324,5325,5326,5327,5328,5329,5330,5331,
    5332,5333,5334,5335,5336,5337,5338,5339,5340,5341,5342,5343,5344,5345,5346,5347,
    5348,5349,5350,5351,5352,5353,5354,5355,5356,5357,5358,5359,5360,5361,5362,5363,
    5364,5365,5366,5367,5368,5369,5370,5371,5372,5373,5374,5375,5376,5377,5378,5379,
    5380,5381, 363, 642,2787,2878,2788,2789,2316,3232,2317,3434,2011, 165,1942,3930,
    3931,3932,3933,5382,4619,5383,4620,5384,5385,5386,5387,5388,5389,5390,5391,5392,
    5393,5394,5395,5396,5397,5398,5399,5400,5401,5402,5403,5404,5405,5406,5407,5408,
    5409,5410,5411,5412,5413,5414,5415,5416,5417,5418,5419,5420,5421,5422,5423,5424,
    5425,5426,5427,5428,5429,5430,5431,5432,5433,5434,5435,5436,5437,5438,5439,5440,
    5441,5442,5443,5444,5445,5446,5447,5448,5449,5450,5451,5452,5453,5454,5455,5456,
    5457,5458,5459,5460,5461,5462,5463,5464,5465,5466,5467,5468,5469,5470,5471,5472,
    5473,5474,5475,5476,5477,5478,5479,5480,5481,5482,5483,5484,5485,5486,5487,5488,
    5489,5490,5491,5492,5493,5494,5495,5496,5497,5498,5499,5500,5501,5502,5503,5504,
    5505,5506,5507,5508,5509,5510,5511,5512,5513,5514,5515,5516,5517,5518,5519,5520,
    5521,5522,5523,5524,5525,5526,5527,5528,5529,5530,5531,5532,5533,5534,5535,5536,
    5537,5538,5539,5540,5541,5542,5543,5544,5545,5546,5547,5548,5549,5550,5551,5552,
    5553,5554,5555,5556,5557,5558,5559,5560,5561,5562,5563,5564,5565,5566,5567,5568,
    5569,5570,5571,5572,5573,5574,5575,5576,5577,5578,5579,5580,5581,5582,5583,5584,
    5585,5586,5587,5588,5589,5590,5591,5592,5593,5594,5595,5596,5597,5598,5599,5600,
    5601,5602,5603,5604,5605,5606,5607,5608,5609,5610,5611,5612,5613,5614,5615,5616,
    5617,5618,5619,5620,5621,5622,5623,5624,5625,5626,5627,5628,5629,5630,5631,5632,
    5633,5634,5635,5636,5637,5638,5639,5640,5641,5642,5643,5644,5645,5646,5647,5648,
    5649,5650,5651,5652,5653,5654,5655,5656,5657,5658,5659,5660,5661,5662,5663,5664,
    5665,5666,5667,5668,5669,5670,5671,5672,5673,5674,5675,5676,5677,5678,5679,5680,
    5681,5682,5683,5684,5685,5686,5687,5688,5689,5690,5691,5692,5693,5694,5695,5696,
    5697,5698,5699,5700,5701,5702,5703,5704,5705,5706,5707,5708,5709,5710,5711,5712,
    5713,5714,5715,5716,5717,5718,5719,5720,5721,5722,5723,5724,5725,5726,5727,5728,
    5729,5730,5731,5732,5733,5734,5735,5736,5737,5738,5739,5740,5741,5742,5743,5744,
    5745,5746,5747,5748,5749,5750,5751,5752,5753,5754,5755,5756,5757,5758,5759,5760,
    5761,5762,5763,5764,5765,5766,5767,5768,5769,5770,5771,5772,5773,5774,5775,5776,
    5777,5778,5779,5780,5781,5782,5783,5784,5785,5786,5787,5788,5789,5790,5791,5792,
    5793,5794,5795,5796,5797,5798,5799,5800,5801,5802,5803,5804,5805,5806,5807,5808,
    5809,5810,5811,5812,5813,5814,5815,5816,5817,5818,5819,5820,5821,5822,5823,5824,
    5825,5826,5827,5828,5829,5830,5831,5832,5833,5834,5835,5836,5837,5838,5839,5840,
    5841,5842,5843,5844,5845,5846,5847,5848,5849,5850,5851,5852,5853,5854,5855,5856,
    5857,5858,5859,5860,5861,5862,5863,5864,5865,5866,5867,5868,5869,5870,5871,5872,
    5873,5874,5875,5876,5877,5878,5879,5880,5881,5882,5883,5884,5885,5886,5887,5888,
    5889,5890,5891,5892,5893,5894,5895,5896,5897,5898,5899,5900,5901,5902,5903,5904,
    5905,5906,5907,5908,5909,5910,5911,5912,5913,5914,5915,5916,5917,5918,5919,5920,
    5921,5922,5923,5924,5925,5926,5927,5928,5929,5930,5931,5932,5933,5934,5935,5936,
    5937,5938,5939,5940,5941,5942,5943,5944,5945,5946,5947,5948,5949,5950,5951,5952,
    5953,5954,5955,5956,5957,5958,5959,5960,5961,5962,5963,5964,5965,5966,5967,5968,
    5969,5970,5971,5972,5973,5974,5975,5976,5977,5978,5979,5980,5981,5982,5983,5984,
    5985,5986,5987,5988,5989,5990,5991,5992,5993,5994,5995,5996,5997,5998,5999,6000,
    6001,6002,6003,6004,6005,6006,6007,6008,6009,6010,6011,6012,6013,6014,6015,6016,
    6017,6018,6019,6020,6021,6022,6023,6024,6025,6026,6027,6028,6029,6030,6031,6032,
    6033,6034,6035,6036,6037,6038,6039,6040,6041,6042,6043,6044,6045,6046,6047,6048,
    6049,6050,6051,6052,6053,6054,6055,6056,6057,6058,6059,6060,6061,6062,6063,6064,
    6065,6066,6067,6068,6069,6070,6071,6072,6073,6074,6075,6076,6077,6078,6079,6080,
    6081,6082,6083,6084,6085,6086,6087,6088,6089,6090,6091,6092,6093,6094,6095,6096,
    6097,6098,6099,6100,6101,6102,6103,6104,6105,6106,6107,6108,6109,6110,6111,6112,
    6113,6114,2044,2060,4621, 997,1235, 473,1186,4622, 920,3378,6115,6116, 379,1108,
    4313,2657,2735,3934,6117,3809, 636,3233, 573,1026,3693,3435,2974,3300,2298,4105,
     854,2937,2463, 393,2581,2417, 539, 752,1280,2750,2480, 140,1161, 440, 708,1569,
     665,2497,1746,1291,1523,3000, 164,1603, 847,1331, 537,1997, 486, 508,1693,2418,
    1970,2227, 878,1220, 299,1030, 969, 652,2751, 624,1137,3301,2619,  65,3302,2045,
    1761,1859,3120,1930,3694,3516, 663,1767, 852, 835,3695, 269, 767,2826,2339,1305,
     896,1150, 770,1616,6118, 506,1502,2075,1012,2519, 775,2520,2975,2340,2938,4314,
    3028,2086,1224,1943,2286,6119,3072,4315,2240,1273,1987,3935,1557, 175, 597, 985,
    3517,2419,2521,1416,3029, 585, 938,1931,1007,1052,1932,1685,6120,3379,4316,4623,
     804, 599,3121,1333,2128,2539,1159,1554,2032,3810, 687,2033,2904, 952, 675,1467,
    3436,6121,2241,1096,1786,2440,1543,1924, 980,1813,2228, 781,2692,1879, 728,1918,
    3696,4624, 548,1950,4625,1809,1088,1356,3303,2522,1944, 502, 972, 373, 513,2827,
     586,2377,2391,1003,1976,1631,6122,2464,1084, 648,1776,4626,2141, 324, 962,2012,
    2177,2076,1384, 742,2178,1448,1173,1810, 222, 102, 301, 445, 125,2420, 662,2498,
     277, 200,1476,1165,1068, 224,2562,1378,1446, 450,1880, 659, 791, 582,4627,2939,
    3936,1516,1274, 555,2099,3697,1020,1389,1526,3380,1762,1723,1787,2229, 412,2114,
    1900,2392,3518, 512,2597, 427,1925,2341,3122,1653,1686,2465,2499, 697, 330, 273,
     380,2162, 951, 832, 780, 991,1301,3073, 965,2270,3519, 668,2523,2636,1286, 535,
    1407, 518, 671, 957,2658,2378, 267, 611,2197,3030,6123, 248,2299, 967,1799,2356,
     850,1418,3437,1876,1256,1480,2828,1718,6124,6125,1755,1664,2405,6126,4628,2879,
    2829, 499,2179, 676,4629, 557,2329,2214,2090, 325,3234, 464, 811,3001, 992,2342,
    2481,1232,1469, 303,2242, 466,1070,2163, 603,1777,2091,4630,2752,4631,2714, 322,
    2659,1964,1768, 481,2188,1463,2330,2857,3600,2092,3031,2421,4632,2318,2070,1849,
    2598,4633,1302,2254,1668,1701,2422,3811,2905,3032,3123,2046,4106,1763,1694,4634,
    1604, 943,1724,1454, 917, 868,2215,1169,2940, 552,1145,1800,1228,1823,1955, 316,
    1080,2510, 361,1807,2830,4107,2660,3381,1346,1423,1134,4108,6127, 541,1263,1229,
    1148,2540, 545, 465,1833,2880,3438,1901,3074,2482, 816,3937, 713,1788,2500, 122,
    1575, 195,1451,2501,1111,6128, 859, 374,1225,2243,2483,4317, 390,1033,3439,3075,
    2524,1687, 266, 793,1440,2599, 946, 779, 802, 507, 897,1081, 528,2189,1292, 711,
    1866,1725,1167,1640, 753, 398,2661,1053, 246, 348,4318, 137,1024,3440,1600,2077,
    2129, 825,4319, 698, 238, 521, 187,2300,1157,2423,1641,1605,1464,1610,1097,2541,
    1260,1436, 759,2255,1814,2150, 705,3235, 409,2563,3304, 561,3033,2005,2564, 726,
    1956,2343,3698,4109, 949,3812,3813,3520,1669, 653,1379,2525, 881,2198, 632,2256,
    1027, 778,1074, 733,1957, 514,1481,2466, 554,2180, 702,3938,1606,1017,1398,6129,
    1380,3521, 921, 993,1313, 594, 449,1489,1617,1166, 768,1426,1360, 495,1794,3601,
    1177,3602,1170,4320,2344, 476, 425,3167,4635,3168,1424, 401,2662,1171,3382,1998,
    1089,4110, 477,3169, 474,6130,1909, 596,2831,1842, 494, 693,1051,1028,1207,3076,
     606,2115, 727,2790,1473,1115, 743,3522, 630, 805,1532,4321,2021, 366,1057, 838,
     684,1114,2142,4322,2050,1492,1892,1808,2271,3814,2424,1971,1447,1373,3305,1090,
    1536,3939,3523,3306,1455,2199, 336, 369,2331,1035, 584,2393, 902, 718,2600,6131,
    2753, 463,2151,1149,1611,2467, 715,1308,3124,1268, 343,1413,3236,1517,1347,2663,
    2093,3940,2022,1131,1553,2100,2941,1427,3441,2942,1323,2484,6132,1980, 872,2368,
    2441,2943, 320,2369,2116,1082, 679,1933,3941,2791,3815, 625,1143,2023, 422,2200,
    3816,6133, 730,1695, 356,2257,1626,2301,2858,2637,1627,1778, 937, 883,2906,2693,
    3002,1769,1086, 400,1063,1325,3307,2792,4111,3077, 456,2345,1046, 747,6134,1524,
     884,1094,3383,1474,2164,1059, 974,1688,2181,2258,1047, 345,1665,1187, 358, 875,
    3170, 305, 660,3524,2190,1334,1135,3171,1540,1649,2542,1527, 927, 968,2793, 885,
    1972,1850, 482, 500,2638,1218,1109,1085,2543,1654,2034, 876,  78,2287,1482,1277,
     861,1675,1083,1779, 724,2754, 454, 397,1132,1612,2332, 893, 672,1237, 257,2259,
    2370, 135,3384, 337,2244, 547, 352, 340, 709,2485,1400, 788,1138,2511, 540, 772,
    1682,2260,2272,2544,2013,1843,1902,4636,1999,1562,2288,4637,2201,1403,1533, 407,
     576,3308,1254,2071, 978,3385, 170, 136,1201,3125,2664,3172,2394, 213, 912, 873,
    3603,1713,2202, 699,3604,3699, 813,3442, 493, 531,1054, 468,2907,1483, 304, 281,
    4112,1726,1252,2094, 339,2319,2130,2639, 756,1563,2944, 748, 571,2976,1588,2425,
    2715,1851,1460,2426,1528,1392,1973,3237, 288,3309, 685,3386, 296, 892,2716,2216,
    1570,2245, 722,1747,2217, 905,3238,1103,6135,1893,1441,1965, 251,1805,2371,3700,
    2601,1919,1078,  75,2182,1509,1592,1270,2640,4638,2152,6136,3310,3817, 524, 706,
    1075, 292,3818,1756,2602, 317,  98,3173,3605,3525,1844,2218,3819,2502, 814, 567,
     385,2908,1534,6137, 534,1642,3239, 797,6138,1670,1529, 953,4323, 188,1071, 538,
     178, 729,3240,2109,1226,1374,2000,2357,2977, 731,2468,1116,2014,2051,6139,1261,
    1593, 803,2859,2736,3443, 556, 682, 823,1541,6140,1369,2289,1706,2794, 845, 462,
    2603,2665,1361, 387, 162,2358,1740, 739,1770,1720,1304,1401,3241,1049, 627,1571,
    2427,3526,1877,3942,1852,1500, 431,1910,1503, 677, 297,2795, 286,1433,1038,1198,
    2290,1133,1596,4113,4639,2469,1510,1484,3943,6141,2442, 108, 712,4640,2372, 866,
    3701,2755,3242,1348, 834,1945,1408,3527,2395,3243,1811, 824, 994,1179,2110,1548,
    1453, 790,3003, 690,4324,4325,2832,2909,3820,1860,3821, 225,1748, 310, 346,1780,
    2470, 821,1993,2717,2796, 828, 877,3528,2860,2471,1702,2165,2910,2486,1789, 453,
     359,2291,1676,  73,1164,1461,1127,3311, 421, 604, 314,1037, 589, 116,2487, 737,
     837,1180, 111, 244, 735,6142,2261,1861,1362, 986, 523, 418, 581,2666,3822, 103,
     855, 503,1414,1867,2488,1091, 657,1597, 979, 605,1316,4641,1021,2443,2078,2001,
    1209,  96, 587,2166,1032, 260,1072,2153, 173,  94, 226,3244, 819,2006,4642,4114,
    2203, 231,1744, 782,  97,2667, 786,3387, 887, 391, 442,2219,4326,1425,6143,2694,
     633,1544,1202, 483,2015, 592,2052,1958,2472,1655, 419, 129,4327,3444,3312,1714,
    1257,3078,4328,1518,1098, 865,1310,1019,1885,1512,1734, 469,2444, 148, 773, 436,
    1815,1868,1128,1055,4329,1245,2756,3445,2154,1934,1039,4643, 579,1238, 932,2320,
     353, 205, 801, 115,2428, 944,2321,1881, 399,2565,1211, 678, 766,3944, 335,2101,
    1459,1781,1402,3945,2737,2131,1010, 844, 981,1326,1013, 550,1816,1545,2620,1335,
    1008, 371,2881, 936,1419,1613,3529,1456,1395,2273,1834,2604,1317,2738,2503, 416,
    1643,4330, 806,1126, 229, 591,3946,1314,1981,1576,1837,1666, 347,1790, 977,3313,
     764,2861,1853, 688,2429,1920,1462,  77, 595, 415,2002,3034, 798,1192,4115,6144,
    2978,4331,3035,2695,2582,2072,2566, 430,2430,1727, 842,1396,3947,3702, 613, 377,
     278, 236,1417,3388,3314,3174, 757,1869, 107,3530,6145,1194, 623,2262, 207,1253,
    2167,3446,3948, 492,1117,1935, 536,1838,2757,1246,4332, 696,2095,2406,1393,1572,
    3175,1782, 583, 190, 253,1390,2230, 830,3126,3389, 934,3245,1703,1749,2979,1870,
    2545,1656,2204, 869,2346,4116,3176,1817, 496,1764,4644, 942,1504, 404,1903,1122,
    1580,3606,2945,1022, 515, 372,1735, 955,2431,3036,6146,2797,1110,2302,2798, 617,
    6147, 441, 762,1771,3447,3607,3608,1904, 840,3037,  86, 939,1385, 572,1370,2445,
    1336, 114,3703, 898, 294, 203,3315, 703,1583,2274, 429, 961,4333,1854,1951,3390,
    2373,3704,4334,1318,1381, 966,1911,2322,1006,1155, 309, 989, 458,2718,1795,1372,
    1203, 252,1689,1363,3177, 517,1936, 168,1490, 562, 193,3823,1042,4117,1835, 551,
     470,4645, 395, 489,3448,1871,1465,2583,2641, 417,1493, 279,1295, 511,1236,1119,
      72,1231,1982,1812,3004, 871,1564, 984,3449,1667,2696,2096,4646,2347,2833,1673,
    3609, 695,3246,2668, 807,1183,4647, 890, 388,2333,1801,1457,2911,1765,1477,1031,
    3316,3317,1278,3391,2799,2292,2526, 163,3450,4335,2669,1404,1802,6148,2323,2407,
    1584,1728,1494,1824,1269, 298, 909,3318,1034,1632, 375, 776,1683,2061, 291, 210,
    1123, 809,1249,1002,2642,3038, 206,1011,2132, 144, 975, 882,1565, 342, 667, 754,
    1442,2143,1299,2303,2062, 447, 626,2205,1221,2739,2912,1144,1214,2206,2584, 760,
    1715, 614, 950,1281,2670,2621, 810, 577,1287,2546,4648, 242,2168, 250,2643, 691,
     123,2644, 647, 313,1029, 689,1357,2946,1650, 216, 771,1339,1306, 808,2063, 549,
     913,1371,2913,2914,6149,1466,1092,1174,1196,1311,2605,2396,1783,1796,3079, 406,
    2671,2117,3949,4649, 487,1825,2220,6150,2915, 448,2348,1073,6151,2397,1707, 130,
     900,1598, 329, 176,1959,2527,1620,6152,2275,4336,3319,1983,2191,3705,3610,2155,
    3706,1912,1513,1614,6153,1988, 646, 392,2304,1589,3320,3039,1826,1239,1352,1340,
    2916, 505,2567,1709,1437,2408,2547, 906,6154,2672, 384,1458,1594,1100,1329, 710,
     423,3531,2064,2231,2622,1989,2673,1087,1882, 333, 841,3005,1296,2882,2379, 580,
    1937,1827,1293,2585, 601, 574, 249,1772,4118,2079,1120, 645, 901,1176,1690, 795,
    2207, 478,1434, 516,1190,1530, 761,2080, 930,1264, 355, 435,1552, 644,1791, 987,
     220,1364,1163,1121,1538, 306,2169,1327,1222, 546,2645, 218, 241, 610,1704,3321,
    1984,1839,1966,2528, 451,6155,2586,3707,2568, 907,3178, 254,2947, 186,1845,4650,
     745, 432,1757, 428,1633, 888,2246,2221,2489,3611,2118,1258,1265, 956,3127,1784,
    4337,2490, 319, 510, 119, 457,3612, 274,2035,2007,4651,1409,3128, 970,2758, 590,
    2800, 661,2247,4652,2008,3950,1420,1549,3080,3322,3951,1651,1375,2111, 485,2491,
    1429,1156,6156,2548,2183,1495, 831,1840,2529,2446, 501,1657, 307,1894,3247,1341,
     666, 899,2156,1539,2549,1559, 886, 349,2208,3081,2305,1736,3824,2170,2759,1014,
    1913,1386, 542,1397,2948, 490, 368, 716, 362, 159, 282,2569,1129,1658,1288,1750,
    2674, 276, 649,2016, 751,1496, 658,1818,1284,1862,2209,2087,2512,3451, 622,2834,
     376, 117,1060,2053,1208,1721,1101,1443, 247,1250,3179,1792,3952,2760,2398,3953,
    6157,2144,3708, 446,2432,1151,2570,3452,2447,2761,2835,1210,2448,3082, 424,2222,
    1251,2449,2119,2836, 504,1581,4338, 602, 817, 857,3825,2349,2306, 357,3826,1470,
    1883,2883, 255, 958, 929,2917,3248, 302,4653,1050,1271,1751,2307,1952,1430,2697,
    2719,2359, 354,3180, 777, 158,2036,4339,1659,4340,4654,2308,2949,2248,1146,2232,
    3532,2720,1696,2623,3827,6158,3129,1550,2698,1485,1297,1428, 637, 931,2721,2145,
     914,2550,2587,  81,2450, 612, 827,2646,1242,4655,1118,2884, 472,1855,3181,3533,
    3534, 569,1353,2699,1244,1758,2588,4119,2009,2762,2171,3709,1312,1531,6159,1152,
    1938, 134,1830, 471,3710,2276,1112,1535,3323,3453,3535, 982,1337,2950, 488, 826,
     674,1058,1628,4120,2017, 522,2399, 211, 568,1367,3454, 350, 293,1872,1139,3249,
    1399,1946,3006,1300,2360,3324, 588, 736,6160,2606, 744, 669,3536,3828,6161,1358,
     199, 723, 848, 933, 851,1939,1505,1514,1338,1618,1831,4656,1634,3613, 443,2740,
    3829, 717,1947, 491,1914,6162,2551,1542,4121,1025,6163,1099,1223, 198,3040,2722,
     370, 410,1905,2589, 998,1248,3182,2380, 519,1449,4122,1710, 947, 928,1153,4341,
    2277, 344,2624,1511, 615, 105, 161,1212,1076,1960,3130,2054,1926,1175,1906,2473,
     414,1873,2801,6164,2309, 315,1319,3325, 318,2018,2146,2157, 963, 631, 223,4342,
    4343,2675, 479,3711,1197,2625,3712,2676,2361,6165,4344,4123,6166,2451,3183,1886,
    2184,1674,1330,1711,1635,1506, 799, 219,3250,3083,3954,1677,3713,3326,2081,3614,
    1652,2073,4657,1147,3041,1752, 643,1961, 147,1974,3955,6167,1716,2037, 918,3007,
    1994, 120,1537, 118, 609,3184,4345, 740,3455,1219, 332,1615,3830,6168,1621,2980,
    1582, 783, 212, 553,2350,3714,1349,2433,2082,4124, 889,6169,2310,1275,1410, 973,
     166,1320,3456,1797,1215,3185,2885,1846,2590,2763,4658, 629, 822,3008, 763, 940,
    1990,2862, 439,2409,1566,1240,1622, 926,1282,1907,2764, 654,2210,1607, 327,1130,
    3956,1678,1623,6170,2434,2192, 686, 608,3831,3715, 903,3957,3042,6171,2741,1522,
    1915,1105,1555,2552,1359, 323,3251,4346,3457, 738,1354,2553,2311,2334,1828,2003,
    3832,1753,2351,1227,6172,1887,4125,1478,6173,2410,1874,1712,1847, 520,1204,2607,
     264,4659, 836,2677,2102, 600,4660,3833,2278,3084,6174,4347,3615,1342, 640, 532,
     543,2608,1888,2400,2591,1009,4348,1497, 341,1737,3616,2723,1394, 529,3252,1321,
     983,4661,1515,2120, 971,2592, 924, 287,1662,3186,4349,2700,4350,1519, 908,1948,
    2452, 156, 796,1629,1486,2223,2055, 694,4126,1259,1036,3392,1213,2249,2742,1889,
    1230,3958,1015, 910, 408, 559,3617,4662, 746, 725, 935,4663,3959,3009,1289, 563,
     867,4664,3960,1567,2981,2038,2626, 988,2263,2381,4351, 143,2374, 704,1895,6175,
    1188,3716,2088, 673,3085,2362,4352, 484,1608,1921,2765,2918, 215, 904,3618,3537,
     894, 509, 976,3043,2701,3961,4353,2837,2982, 498,6176,6177,1102,3538,1332,3393,
    1487,1636,1637, 233, 245,3962, 383, 650, 995,3044, 460,1520,1206,2352, 749,3327,
     530, 700, 389,1438,1560,1773,3963,2264, 719,2951,2724,3834, 870,1832,1644,1000,
     839,2474,3717, 197,1630,3394, 365,2886,3964,1285,2133, 734, 922, 818,1106, 732,
     480,2083,1774,3458, 923,2279,1350, 221,3086,  85,2233,2234,3835,1585,3010,2147,
    1387,1705,2382,1619,2475, 133, 239,2802,1991,1016,2084,2383, 411,2838,1113, 651,
    1985,1160,3328, 990,1863,3087,1048,1276,2647, 265,2627,1599,3253,2056, 150, 638,
    2019, 656, 853, 326,1479, 680,1439,4354,1001,1759, 413,3459,3395,2492,1431, 459,
    4355,1125,3329,2265,1953,1450,2065,2863, 849, 351,2678,3131,3254,3255,1104,1577,
     227,1351,1645,2453,2193,1421,2887, 812,2121, 634,  95,2435, 201,2312,4665,1646,
    1671,2743,1601,2554,2702,2648,2280,1315,1366,2089,3132,1573,3718,3965,1729,1189,
     328,2679,1077,1940,1136, 558,1283, 964,1195, 621,2074,1199,1743,3460,3619,1896,
    1916,1890,3836,2952,1154,2112,1064, 862, 378,3011,2066,2113,2803,1568,2839,6178,
    3088,2919,1941,1660,2004,1992,2194, 142, 707,1590,1708,1624,1922,1023,1836,1233,
    1004,2313, 789, 741,3620,6179,1609,2411,1200,4127,3719,3720,4666,2057,3721, 593,
    2840, 367,2920,1878,6180,3461,1521, 628,1168, 692,2211,2649, 300, 720,2067,2571,
    2953,3396, 959,2504,3966,3539,3462,1977, 701,6181, 954,1043, 800, 681, 183,3722,
    1803,1730,3540,4128,2103, 815,2314, 174, 467, 230,2454,1093,2134, 755,3541,3397,
    1141,1162,6182,1738,2039, 270,3256,2513,1005,1647,2185,3837, 858,1679,1897,1719,
    2954,2324,1806, 402, 670, 167,4129,1498,2158,2104, 750,6183, 915, 189,1680,1551,
     455,4356,1501,2455, 405,1095,2955, 338,1586,1266,1819, 570, 641,1324, 237,1556,
    2650,1388,3723,6184,1368,2384,1343,1978,3089,2436, 879,3724, 792,1191, 758,3012,
    1411,2135,1322,4357, 240,4667,1848,3725,1574,6185, 420,3045,1546,1391, 714,4358,
    1967, 941,1864, 863, 664, 426, 560,1731,2680,1785,2864,1949,2363, 403,3330,1415,
    1279,2136,1697,2335, 204, 721,2097,3838,  90,6186,2085,2505, 191,3967, 124,2148,
    1376,1798,1178,1107,1898,1405, 860,4359,1243,1272,2375,2983,1558,2456,1638, 113,
    3621, 578,1923,2609, 880, 386,4130, 784,2186,2266,1422,2956,2172,1722, 497, 263,
    2514,1267,2412,2610, 177,2703,3542, 774,1927,1344, 616,1432,1595,1018, 172,4360,
    2325, 911,4361, 438,1468,3622, 794,3968,2024,2173,1681,1829,2957, 945, 895,3090,
     575,2212,2476, 475,2401,2681, 785,2744,1745,2293,2555,1975,3133,2865, 394,4668,
    3839, 635,4131, 639, 202,1507,2195,2766,1345,1435,2572,3726,1908,1184,1181,2457,
    3727,3134,4362, 843,2611, 437, 916,4669, 234, 769,1884,3046,3047,3623, 833,6187,
    1639,2250,2402,1355,1185,2010,2047, 999, 525,1732,1290,1488,2612, 948,1578,3728,
    2413,2477,1216,2725,2159, 334,3840,1328,3624,2921,1525,4132, 564,1056, 891,4363,
    1444,1698,2385,2251,3729,1365,2281,2235,1717,6188, 864,3841,2515, 444, 527,2767,
    2922,3625, 544, 461,6189, 566, 209,2437,3398,2098,1065,2068,3331,3626,3257,2137);
end;

function TdxJapaneseCharDistributionAnalyzer.GetCharToFreqOrder: TArray<Word>;
begin
  Result := FCharToFreqOrder;
end;

function TdxJapaneseCharDistributionAnalyzer.GetTypicalDistributionRatio: Single;
begin
  Result := 3.0;
end;

{ TdxJapaneseContextAnalyzer }

constructor TdxJapaneseContextAnalyzer.Create;
begin
  inherited Create;
  SetLength(FSequenceCounters, CategoryCount);
end;

procedure TdxJapaneseContextAnalyzer.AnalyzeCharacter(const ABuffer: TArray<Byte>; AFrom, ALength: Integer);
var
  AOrder: Integer;
begin
  if FTotalSequenceCount > MaxSequenceCount then
    FAnalysisComplete := True;
  if FAnalysisComplete then
    Exit;
  if ALength = 2 then
    AOrder := GetOrder(ABuffer, AFrom)
  else
    AOrder := -1;
  if (AOrder <> -1) and (FLastCharacterOrder <> -1) then
  begin
    Inc(FTotalSequenceCount);
    Inc(FSequenceCounters[Jp2CharContext[FLastCharacterOrder, AOrder]]);
  end;
  FLastCharacterOrder := AOrder;
end;

function TdxJapaneseContextAnalyzer.GetConfidence(AIsPreferredLanguage: Boolean): Single;
begin
  if (AIsPreferredLanguage) or (FTotalSequenceCount > MinSequenceCount) then
    Result := (FTotalSequenceCount - FSequenceCounters[0]) / FTotalSequenceCount
  else
    Result := -1.0;
end;

function TdxJapaneseContextAnalyzer.GotEnoughData: Boolean;
begin
  Result := FTotalSequenceCount > EnoughSequenceCount;
end;

{ TdxEucJpEncodingDetector }

function TdxEucJpEncodingDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.GetEncoding(51932);
end;

function TdxEucJpEncodingDetector.CreateDistributionAnalyzer: TdxCharacterDistributionAnalyzer;
begin
  Result := TdxEucJpCharDistributionAnalyzer.Create;
end;

function TdxEucJpEncodingDetector.CreateEncodingAnalyzer: TdxEncodingAnalyzer;
begin
  Result := TdxEucJpEncodingAnalyzer.Create;
end;

function TdxEucJpEncodingDetector.CreateContextAnalyzer: TdxJapaneseContextAnalyzer;
begin
  Result := TdxEucJpContextAnalyzer.Create;
end;

{ TdxEucJpCharDistributionAnalyzer }

function TdxEucJpCharDistributionAnalyzer.GetOrder(const AStr: TArray<Byte>; AOffset: Integer): Integer;
begin
  if AStr[AOffset] >= $a0 then
    Result := 94 * (AStr[AOffset] - $a1) + AStr[AOffset + 1] - $a1
  else
    Result := -1;
end;

{ TdxEucJpContextAnalyzer }

function TdxEucJpContextAnalyzer.GetOrder(const AStr: TArray<Byte>; AOffset: Integer): Integer;
begin
  if ((AStr[AOffset] = $A4) and (AStr[AOffset + 1] >= $a1)) and (AStr[AOffset + 1] <= $f3) then
    Result := AStr[AOffset + 1] - $a1
  else
    Result := -1;
end;

{ TdxEucJpEncodingAnalyzer }

class constructor TdxEucJpEncodingAnalyzer.Initialize;
begin
  FClassTable := TArray<Integer>.Create(
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,4,4,4,5,5),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,5,4,4,4,4),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(5,5,5,5,5,5,5,5),
    Pack4Bits(5,5,5,5,5,5,1,3),
    Pack4Bits(5,5,5,5,5,5,5,5),
    Pack4Bits(5,5,5,5,5,5,5,5),
    Pack4Bits(5,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(0,0,0,0,0,0,0,0),
    Pack4Bits(0,0,0,0,0,0,0,0),
    Pack4Bits(0,0,0,0,0,0,0,0),
    Pack4Bits(0,0,0,0,0,0,0,5));
  FStateTable := TArray<Integer>.Create(
    Pack4Bits(           3,            4,            3,            5, EAStateStart, EAStateError, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateError, EAStateError, EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateItsMe),
    Pack4Bits(EAStateItsMe, EAStateItsMe, EAStateStart, EAStateError, EAStateStart, EAStateError, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateStart, EAStateError, EAStateError, EAStateError,            3, EAStateError),
    Pack4Bits(           3, EAStateError, EAStateError, EAStateError, EAStateStart, EAStateStart, EAStateStart, EAStateStart));
  FCharLenTable := TArray<Integer>.Create(2, 2, 2, 3, 1, 0);
end;

function TdxEucJpEncodingAnalyzer.GetClassTable: TArray<Integer>;
begin
  Result := FClassTable;
end;

function TdxEucJpEncodingAnalyzer.GetStateTable: TArray<Integer>;
begin
  Result := FStateTable;
end;

function TdxEucJpEncodingAnalyzer.GetCharLenTable: TArray<Integer>;
begin
  Result := FCharLenTable;
end;

{ TdxShiftedJisEncodingDetector }

function TdxShiftedJisEncodingDetector.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.GetEncoding(932);
end;

function TdxShiftedJisEncodingDetector.CreateDistributionAnalyzer: TdxCharacterDistributionAnalyzer;
begin
  Result := TdxShiftedJisCharDistributionAnalyzer.Create;
end;

function TdxShiftedJisEncodingDetector.CreateEncodingAnalyzer: TdxEncodingAnalyzer;
begin
  Result := TdxShiftedJisEncodingAnalyzer.Create;
end;

function TdxShiftedJisEncodingDetector.CreateContextAnalyzer: TdxJapaneseContextAnalyzer;
begin
  Result := TdxShiftedJisContextAnalyzer.Create;
end;

procedure TdxShiftedJisEncodingDetector.ContinueContextAnalysis(const ABuffer: TArray<Byte>; AFrom, ALength: Integer);
begin
  inherited ContinueContextAnalysis(ABuffer, AFrom + 2 - ALength, ALength);
end;

{ TdxShiftedJisCharDistributionAnalyzer }

function TdxShiftedJisCharDistributionAnalyzer.GetOrder(const AStr: TArray<Byte>; AOffset: Integer): Integer;
begin
  if (AStr[AOffset] >= $81) and (AStr[AOffset] <= $09) then
    Result := 188 * (AStr[AOffset] - $81)
  else
    if (AStr[AOffset] >= $e0) and (AStr[AOffset] <= $0e) then
      Result := 188 * (AStr[AOffset] - $e0 + 31)
    else
      Exit(-1);

  Inc(Result, AStr[AOffset + 1] - $40);
  if AStr[AOffset + 1] > $07 then
    Dec(Result);
end;

{ TdxShiftedJisContextAnalyzer }

function TdxShiftedJisContextAnalyzer.GetOrder(const AStr: TArray<Byte>; AOffset: Integer): Integer;
begin
  if ((AStr[AOffset] = $82) and (AStr[AOffset + 1] >= $09)) and (AStr[AOffset + 1] <= $f1) then
    Result := AStr[AOffset + 1] - $09
  else
    Result := -1;
end;

{ TdxShiftedJisEncodingAnalyzer }

class constructor TdxShiftedJisEncodingAnalyzer.Initialize;
begin
  FClassTable := TArray<Integer>.Create(
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,0,0),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,0,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,1),
    Pack4Bits(3,3,3,3,3,3,3,3),
    Pack4Bits(3,3,3,3,3,3,3,3),
    Pack4Bits(3,3,3,3,3,3,3,3),
    Pack4Bits(3,3,3,3,3,3,3,3),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(2,2,2,2,2,2,2,2),
    Pack4Bits(3,3,3,3,3,3,3,3),
    Pack4Bits(3,3,3,3,3,4,4,4),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,4,4,0,0,0));
  FStateTable := TArray<Integer>.Create(
    Pack4Bits(EAStateError, EAStateStart, EAStateStart,            3, EAStateError, EAStateError, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateError, EAStateError, EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateItsMe),
    Pack4Bits(EAStateItsMe, EAStateItsMe, EAStateError, EAStateError, EAStateStart, EAStateStart, EAStateStart, EAStateStart));
  FCharLenTable := TArray<Integer>.Create(0, 1, 1, 2, 0, 0);
end;

function TdxShiftedJisEncodingAnalyzer.GetClassTable: TArray<Integer>;
begin
  Result := FClassTable;
end;

function TdxShiftedJisEncodingAnalyzer.GetStateTable: TArray<Integer>;
begin
  Result := FStateTable;
end;

function TdxShiftedJisEncodingAnalyzer.GetCharLenTable: TArray<Integer>;
begin
  Result := FCharLenTable;
end;

{ TdxEncodingAnalyzer }

function TdxEncodingAnalyzer.NextState(C: Byte): TdxEAState;
var
  AByteCls: Integer;
begin
  AByteCls := Unpack4Bits(C, ClassTable);
  if FCurrentState = EAStateStart then
    FCurrentCharLength := CharLenTable[AByteCls];
  FCurrentState := TdxEAState(Unpack4Bits(Integer(FCurrentState) * Length(CharLenTable) + AByteCls, StateTable));
  Result := FCurrentState;
end;

class function TdxEncodingAnalyzer.Unpack4Bits(I: Integer; const ABuffer: TArray<Integer>): Integer;
var
  AValue: Integer;
begin
  AValue := I;
  Result := (ABuffer[AValue shr 3] shr ((AValue and 7) shl 2)) and $0000000F;
end;

class function TdxEncodingAnalyzer.Pack4Bits(A, B, C, D, E, F, G, H: Integer): Integer;
begin
  Assert(A < 16);
  Assert(B < 16);
  Assert(C < 16);
  Assert(D < 16);
  Assert(E < 16);
  Assert(F < 16);
  Assert(G < 16);
  Assert(H < 16);

  Result := Pack8Bits((B shl 4) or A, (D shl 4) or C, (F shl 4) or E, (H shl 4) or G);
end;

{ TdxUtf8EncodingAnalyzer }

class constructor TdxUtf8EncodingAnalyzer.Initialize;
begin
  FClassTable := TArray<Integer>.Create(
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,0,0),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,0,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(1,1,1,1,1,1,1,1),
    Pack4Bits(2,2,2,2,3,3,3,3),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(4,4,4,4,4,4,4,4),
    Pack4Bits(5,5,5,5,5,5,5,5),
    Pack4Bits(5,5,5,5,5,5,5,5),
    Pack4Bits(5,5,5,5,5,5,5,5),
    Pack4Bits(5,5,5,5,5,5,5,5),
    Pack4Bits(0,0,6,6,6,6,6,6),
    Pack4Bits(6,6,6,6,6,6,6,6),
    Pack4Bits(6,6,6,6,6,6,6,6),
    Pack4Bits(6,6,6,6,6,6,6,6),
    Pack4Bits(7,8,8,8,8,8,8,8),
    Pack4Bits(8,8,8,8,8,9,8,8),
    Pack4Bits(10,11,11,11,11,11,11,11),
    Pack4Bits(12,13,13,13,14,15,0,0));
  FStateTable := TArray<Integer>.Create(
    Pack4Bits(EAStateError, EAStateStart, EAStateError, EAStateError, EAStateError, EAStateError,           12,           10),
    Pack4Bits(           9,           11,            8,            7,            6,            5,            4,            3),
    Pack4Bits(EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError),
    Pack4Bits(EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateItsMe),
    Pack4Bits(EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateItsMe, EAStateItsMe),
    Pack4Bits(EAStateError, EAStateError,            5,            5,            5,            5, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateError,            5,            5,            5, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError,            7,            7,            7,            7, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateError, EAStateError,            7,            7, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError,            9,            9,            9,            9, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateError, EAStateError, EAStateError,            9, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError,           12,           12,           12,           12, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateError, EAStateError, EAStateError,           12, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError,           12,           12,           12, EAStateError, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateStart, EAStateStart, EAStateStart, EAStateStart, EAStateError, EAStateError),
    Pack4Bits(EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError, EAStateError));
  FCharLenTable := TArray<Integer>.Create(0, 1, 0, 0, 0, 0, 2, 3, 3, 3, 4, 4, 5, 5, 6, 6);
end;

function TdxUtf8EncodingAnalyzer.GetClassTable: TArray<Integer>;
begin
  Result := FClassTable;
end;

function TdxUtf8EncodingAnalyzer.GetStateTable: TArray<Integer>;
begin
  Result := FStateTable;
end;

function TdxUtf8EncodingAnalyzer.GetCharLenTable: TArray<Integer>;
begin
  Result := FCharLenTable;
end;

{ TdxInternalEncodingDetector }

constructor TdxInternalEncodingDetector.Create(const ALanguageFilter: TdxEncodingDetectorLanguages);
begin
  inherited Create;
  FLanguageFilter := ALanguageFilter;
  FStates := TdxObjectList<TdxEncodingDetectorState>.Create;
end;

constructor TdxInternalEncodingDetector.Create;
begin
  Create(TdxEncodingDetectorLanguagesAll);
end;

destructor TdxInternalEncodingDetector.Destroy;
begin
  FStates.Free;
  FDetectors.Free;
  inherited Destroy;
end;

function TdxInternalEncodingDetector.CreateDetectors: TdxList<TdxEncodingDetectorBase>;
begin
  Result := TdxObjectList<TdxEncodingDetectorBase>.Create;
  Result.Add(TdxMultiByteCharsetGroupDetector.Create(LanguageFilter));
  Result.Add(TdxSingleByteCharsetGroupDetector.Create);
  Result.Add(TdxLatin1EncodingDetector.Create);
  Result.Add(FUnicodeDetector);
end;

procedure TdxInternalEncodingDetector.BeginDetection;
begin
  FUnicodeDetector := TdxBomLessUnicodeGroupDetector.Create;
  FDetectors.Free;
  FDetectors := CreateDetectors;
  DetectorState := TdxInitialEncodingDetectorState.Create(Self);
end;

function TdxInternalEncodingDetector.AnalyseData(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): Boolean;
begin
  Result := DetectorState.AnalyseData(ABuffer, AFrom, ALength);
end;

function TdxInternalEncodingDetector.EndDetection: TEncoding;
begin
  Result := DetectorState.CalculateResult;
end;

procedure TdxInternalEncodingDetector.SetDetectorState(const Value: TdxEncodingDetectorState);
begin
  if FStates.IndexOf(Value) < 0 then
    FStates.Add(Value);
  FDetectorState := Value;
end;

function TdxInternalEncodingDetector.Detect(const ABuffer: TArray<Byte>; AFrom, ALength: Integer): TEncoding;
begin
  BeginDetection;
  AnalyseData(ABuffer, AFrom, ALength);
  Result := EndDetection;
end;

function TdxInternalEncodingDetector.Detect(const ABuffer: TArray<Byte>): TEncoding;
begin
  Result := Detect(ABuffer, 0, Length(ABuffer));
end;

function TdxInternalEncodingDetector.DetectStreamEncodingCore(AStream: TStream; AMaxByteCount: Integer): TEncoding;
var
  ABuffer: TArray<Byte>;
  AByteCount: Integer;
begin
  SetLength(ABuffer, Min(1024, AMaxByteCount));
  BeginDetection;
  while AMaxByteCount > 0 do
  begin
    AByteCount := Min(AMaxByteCount, Length(ABuffer));
    AStream.ReadBuffer(ABuffer[0], AByteCount);
    if AnalyseData(ABuffer, 0, AByteCount) then
      Break;
    Dec(AMaxByteCount, AByteCount);
  end;
  Result := EndDetection;
end;

function TdxInternalEncodingDetector.Detect(AStream: TStream; AMaxByteCount: Integer; AKeepPosition: Boolean): TEncoding;
var
  APosition: Int64;
  AActualMaxByteCount: Integer;
begin
  APosition := 0;
  if AKeepPosition then
    APosition := AStream.Position;
  try
    AActualMaxByteCount := Min(Integer((AStream.Size - AStream.Position)), AMaxByteCount);
    if AActualMaxByteCount <= 0 then
      Exit(nil);

    Exit(DetectStreamEncodingCore(AStream, AActualMaxByteCount));
  finally
    if AKeepPosition then
      AStream.Seek(APosition, TSeekOrigin.soBeginning);
  end;
end;

function TdxInternalEncodingDetector.Detect(AStream: TStream; AMaxByteCount: Integer): TEncoding;
begin
  Result := Detect(AStream, AMaxByteCount, True);
end;

function TdxInternalEncodingDetector.Detect(AStream: TStream): TEncoding;
begin
  Result := Detect(AStream, 4096, True);
end;

{ TdxEmptyEncoding }

constructor TdxEmptyEncoding.Create;
begin
  inherited Create;
  FIsSingleByte := True;
end;

function TdxEmptyEncoding.GetByteCount(const AChars: TArray<Char>; AIndex: Integer; ACount: Integer): Integer;
begin
  Result := ACount;
end;

function TdxEmptyEncoding.GetByteCount(Chars: PChar; CharCount: Integer): Integer;
begin
  Result := CharCount;
end;

function TdxEmptyEncoding.GetBytes(Chars: PWideChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer;
var
  AMaxIndex: Integer;
  ACh: Char;
  ACharIndex, AByteIndex: Integer;
begin
  ACharIndex := 0;
  AByteIndex := 0;
  AMaxIndex := CharCount;
  while ACharIndex < AMaxIndex do
  begin
    ACh := Chars[ACharIndex];
    Inc(ACharIndex);
    if ACh >= #$0100 then
      ACh := '?';
    Bytes[AByteIndex] := Byte(ACh);
    Inc(AByteIndex);
  end;
  Result := CharCount;
end;

function TdxEmptyEncoding.GetCharCount(Bytes: PByte; ByteCount: Integer): Integer;
begin
  Result := ByteCount;
end;

function TdxEmptyEncoding.GetChars(Bytes: PByte; ByteCount: Integer; Chars: PWideChar; CharCount: Integer): Integer;
var
  AMaxIndex: Integer;
  AByteIndex, ACharIndex: Integer;
begin
  AMaxIndex := ByteCount;
  AByteIndex := 0;
  ACharIndex := 0;
  while AByteIndex < AMaxIndex do
  begin
    Chars[ACharIndex] := Char(Bytes[AByteIndex]);
    Inc(ACharIndex);
    Inc(AByteIndex);
  end;
  Result := ByteCount;
end;

function TdxEmptyEncoding.GetMaxByteCount(ACharCount: Integer): Integer;
begin
  Result := ACharCount;
end;

function TdxEmptyEncoding.GetMaxCharCount(AByteCount: Integer): Integer;
begin
  Result := AByteCount;
end;

function TdxEmptyEncoding.GetPreamble: System.TArray<System.Byte>;
begin
  Result := nil;
end;

end.
