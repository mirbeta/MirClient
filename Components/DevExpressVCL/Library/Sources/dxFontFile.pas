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

unit dxFontFile;

{$I cxVer.inc}

interface

uses
  Windows, Classes, SysUtils, Types, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses, cxGeometry,
  cxVariants, dxPDFBase;

type
  TdxFontFile = class;

  TdxFontFileEncodingID = (Undefined = 0, UGL = 1, Big5 = 3);
  TdxFontFileLanguageID = (English, EnglishUnitedStates = $0409);
  TdxFontFileNameID = (Copyright = 0, FontFamily = 1, FontSubfamily = 2, UniqueFontId = 3, FullFontName = 4,
   Version = 5, PostscriptName = 6, Trademark = 7);
  TdxFontFilePlatformID = (AppleUnicode, Macintosh, ISO, Microsoft);

  TdxFontFileCMapFormatID = (ByteEncoding = 0, HighByteMappingThrough = 2, SegmentMapping = 4, TrimmedMapping = 6,
		MixedCoverage = 8, TrimmedArray = 10, SegmentedCoverage = 12, ManyToOneRangeMapping = 13,
    UnicodeVariationSequences = 14);

	TdxFontFileHeadTableFlags = (BaselineForFontAt0 = $0001, LeftSideBearingPointAt0 = $0002,
    InstructionsMayDependOnPointSize = $0004, ForcePPEMToIntegerValues = $0008, InstructionsMayAlterAdvanceWidth = $0008,
    FontDataIsLossless = $0800, ProduceCompatibleMetrics = $1000, OptimizedForClearType = $2000, LastResort = $4000);

	TdxFontFileHeadTableMacStyle = (emsBold = $01, emsItalic = $02, Underline = $04, Outline = $08,
    Shadow = $10, Condensed = $20, Extended = $40);

	TdxFontFileDirectionHint = (FullyMixedDirectionalGlyphs = 0, OnlyStronglyLeftToRight = 1,
    OnlyStronglyLeftToRightButAlsoContainsNeutrals = 2, OnlyStronglyRightToLeft = -1,
    OnlyStronglyRightToLeftButAlsoContainsNeutrals = -2);

	TdxFontFileIndexToLocFormat = (Short, Long);

  TdxFontFileVersion = (TrueType_1_5, TrueType_1_66, OpenType_1_2, OpenType_1_4, OpenType_1_5);

  TdxFontFileOS2EmbeddingType = (InstallableEmbedding = 0, RestrictedLicense = $002, PreviewPrintEmbedding = $004,
    EditableEmbedding = $008, NoSubSetting = $100, BitmapEmbeddingOnly = $200);
  TdxFontFileOS2WidthClass = (wcUltraCondensed, wcExtraCondensed, wcCondensed, wcSemiCondensed, wcMedium, wcSemiExpanded,
    wcExpanded, wcExtraExpanded, wcUltraExpanded);

  TdxFontFileFlags = (ffNone = $00000, ffFixedPitch = $00001, ffSerif = $00002, ffSymbolic = $00004, ffScript = $00008,
    ffNonSymbolic = $00020, ffItalic = $00040, ffAllCap = $10000, ffSmallCap = $20000, ffForceBold = $40000);
  TdxFontFileStretch = (fsUltraCondensed, fsExtraCondensed, fsCondensed, fsSemiCondensed, fsNormal, fsSemiExpanded,
    fsExpanded, fsExtraExpanded, fsUltraExpanded);

  TdxFontFileOS2FamilyClass = (ffcNoClassification = $000, ffcOldStyleSerifNoClassification = $100,
    ffcOldStyleSerifIBMRoundedLegibility = $101, ffcOldStyleSerifGaralde = $102, ffcOldStyleSerifVenetian = $103,
    ffcOldStyleSerifModifiedVenetian = $104, ffcOldStyleSerifDutchModern = $105, ffcOldStyleSerifDutchTraditional = $106,
    ffcOldStyleSerifContemporary = $107, ffcOldStyleSerifCalligraphic = $108, ffcOldStyleSerifMiscellaneous = $10f,
    ffcTransitionalSerifNoClassification = $200, ffcTransitionalSerifDirectLine = $201, ffcTransitionalSerifScript = $202,
    ffcTransitionalSerifMiscellaneous = $20f, ffcModernSerifNoClassification = $300, ffcModernSerifItalian = $301,
    ffcModernSerifScript = $302, ffcModernSerifMiscellaneous = $30f, ffcClarendonSerifNoClassification = $400,
    ffcClarendonSerifClarendon = $401, ffcClarendonSerifModern = $402, ffcClarendonSerifTraditional = $403,
    ffcClarendonSerifNewspaper = $404, ffcClarendonSerifStubSerif = $405, ffcClarendonSerifMonotone = $406,
    ffcClarendonSerifTypewriter = $407, ClarendonSerifMiscellaneous = $40f, ffcSlabSerifNoClassification = $500,
    ffcSlabSerifMonotone = $501, ffcSlabSerifHumanist = $502, ffcSlabSerifGeometric = $503, ffcSlabSerifSwiss = $504,
    ffcSlabSerifTypewriter = $505, ffcSlabSerifMiscellaneous = $50f, ffcFreeFormSerifNoClassification = $700,
    ffcFreeFormSerifModern = $701, ffcFreeFormSerifMiscellaneous = $70f, ffcSansSerifNoClassification = $800,
    ffcSansSerifIBMNewGrotesqueGothic = $801, ffcSansSerifHumanist = $802, ffcSansSerifLowXRoundGeometric = $803,
    ffcSansSerifHighXRoundGeometric = $804, ffcSansSerifNeoGrotesqueGothic = $805,
    ffcSansSerifModifiedNeoGrotesqueGothic = $806, ffcSansSerifTypewriterGothic = $809, ffcSansSerifMatrix = $80a,
    ffcSansSerifMiscellaneous = $80f, ffcOrnamentalNoClassification = $900, ffcOrnamentalEngraver = $901,
    ffcOrnamentalBlackLetter = $902, OrnamentalDecorative = $903, OrnamentalThreeDimensional = $904,
    ffcOrnamentalMiscellaneous = $90f, ffcScriptNoClassification = $a00, ffcScriptUncial = $a01,
    ffcScriptBrushJoined = $a02, ffcScriptFormalJoined = $a03, ffcScriptMonotoneJoined = $a04, ffcScriptCalligraphic = $a05,
    ScriptBrushUnjoined = $a06, ffcScriptFormalUnjoined = $a07, ffcScriptMonotoneUnjoined = $a08, ffcScriptMiscellaneous = $a0f,
    ffcSymbolicNoClassification = $c00, ffcSymbolicMixedSerif = $c03, ffcSymbolicOldstyleSerif = $c06,
    ffcSymbolicNeoGrotesqueSansSerif = $c07, ffcSymbolicMiscellaneous = $c0f);

  TdxFontFileUnicodeRange1 = (BasicLatin = $00000001, Latin1Supplement = $00000002, LatinExtendedA = $00000004,
    LatinExtendedB = $00000008, IPAExtensions = $00000010, SpacingModifiersLetters = $00000020,
    CombiningDiacriticalMarks = $00000040, GreekAndCoptic = $00000080, Coptic = $00000100, Cyrillic = $00000200,
    Armenian = $00000400, Hebrew = $00000800, Vai = $00001000, Arabic = $00002000, NKo = $00004000, Devanagari = $00008000,
    Bengali = $00010000, Gurmukhi = $00020000, Gujarati = $00040000, Oriya = $00080000, Tamil = $00100000,
    Telugu = $00200000, Kannada = $00400000, Malayalam = $00800000, Thai = $01000000, Lao = $02000000, Georgian = $04000000,
    Balinese = $08000000, HangulJamo = $10000000, LatinExtendedAdditional = $20000000, GreekExtended = $40000000,
    GeneralPunctuation = $7FFFFFFF);

  TdxFontFileUnicodeRange2 = (SuperscriptsAndSubscripts = $00000001, CurrencySymbols = $00000002,
    CombiningDiacriticalMarksForSymbols = $00000004, LetterlikeSymbols = $00000008, NumberForms = $00000010,
    Arrows = $00000020, MathematicalOperators = $00000040, MiscellaneousTechnical = $00000080, ControlPictures = $00000100,
    OpticalCharacterRecognition = $00000200, EnclosedAlphanumerics = $00000400, BoxDrawing = $00000800,
    BlockElements = $00001000, GeometricShapes = $00002000, MiscellaneousSymbols = $00004000, Dingbats = $00008000,
    CJKSymbolsAndPunctuation = $00010000, Hiragana = $00020000, Katakana = $00040000, Bopomofo = $00080000,
    HangulCompatibilityJamo = $00100000, PhagsPa = $00200000, EnclosedCJKLettersAndMonths = $00400000,
    CJKCompatibility = $00800000, HangulSyllables = $01000000, NonPlane0 = $02000000, Phoenician = $04000000,
    CJKUnifiedIdeographs = $08000000, PrivateUseAreaPlane0 = $10000000, CJKStrokes = $20000000,
    AlphabeticPresentationForms = $40000000, ArabicPresentationFormsA = $7FFFFFFF);

  TdxFontFileUnicodeRange3 = (CombiningHalfMarks = $00000001, VerticalForms = $00000002,
    SmallFormsVariants = $00000004, ArabicPresentationFormsB = $00000008, HalfWidthAndFullWidthForms = $00000010,
    Specials = $00000020, Tibetan = $00000040, Syriac = $00000080, Thaana = $00000100, Sinhala = $00000200,
    Myanmar = $00000400, Ethiopic = $00000800, Cherokee = $00001000, UnifiedCanadianAboriginalSyllabics = $00002000,
    Ogham = $00004000, Runic = $00008000, Khmer = $00010000, Mongolian = $00020000, BraillePatterns = $00040000,
    YiSyllables = $00080000, Tagalog = $00100000, OldItalic = $00200000, Gothic = $00400000, Deseret = $00800000,
    MusicalSymbols = $01000000, MathematicalAlphanumericSymbols = $02000000, PrivateUsePlane15_16 = $04000000,
    VariationSelectors = $08000000, Tags = $10000000, Limbu = $20000000, TaiLe = $40000000, NewTaiLe = $7FFFFFFF);

  TdxFontFileUnicodeRange4 = (Buginese = $00000001, Glagolitic = $00000002, Tifinagh = $00000004,
    YijingHexagramSymbols = $00000008, SylotiNagri = $00000010, LinearBSyllabary = $00000020,
    AncientGreekNumbers = $00000040, Ugaritic = $00000080, OldPersian = $00000100, Shavian = $00000200,
    Osmanya = $00000400, CypriotSyllabary = $00000800, Kharoshthi = $00001000, TaiXuanJingSymbols = $00002000,
    Cuneiform = $00004000, CountingRodNumerals = $00008000, Sundanese = $00010000, Lepcha = $00020000,
    OlChiki = $00040000, Saurashtra = $00080000, KayahLi = $00100000, Rejang = $00200000, Cham = $00400000,
    AncientSymbols = $00800000, PhaistosDisc = $01000000, Carian = $02000000, DominoTiles = $04000000);

  TdxFontFileSelection = (Empty = $000, ITALIC = $001, UNDERSCORE = $002, NEGATIVE = $004, OUTLINED = $008,
    STRIKEOUT = $010, BOLD = $020, REGULAR = $040, USE_TYPO_METRICS = $080, WWS = $100, OBLIQUE = $200);

  TdxFontFileCodePageRange1 = (Latin1 = $00000001, Latin2EasternEurope = $00000002, r1Cyrillic = $00000004,
    Greek = $00000008, Turkish = $00000010, r1Hebrew = $00000020, r1Arabic = $00000040, WindowsBaltic = $00000080,
    Vietnamese = $00000100, r1Thai = $00010000, JISJapan = $00020000, ChineseSimplified = $00040000,
    KoreanWansung = $00080000, ChineseTraditional = $00100000, KoreanJohab = $00200000, MacintoshCharacterSet = $20000000,
    OEMCharacterSet = $40000000, SymbolCharacterSet = $7FFFFFFF);

  TdxFontFileCodePageRange2 = (IBMGreek = $00010000, MSDOSRussian = $00020000, MSDOSNordic = $00040000,
    r2Arabic = $00080000, MSDOSCanadianFrench = $00100000, r2Hebrew = $00200000, MSDOSIcelandic = $00400000,
    MSDOSPortuguese = $00800000, IBMTurkish = $01000000, IBMCyrillic = $02000000, Latin2 = $04000000,
    MSDOSBaltic = $08000000, GreekFormer437G = $10000000, ArabicASMO708 = $20000000, WELatin1 = $40000000, US = $7FFFFFFF);

  TdxFontFilePanoseFamilyKind = (fkAny = 0, fkNoFit = 1, fkLatinText = 2, fkLatinHandWritten = 3, fkLatinDecorative = 4,
    fkLatinSymbol = 5);
  TdxFontFilePanoseSerifStyle = (ssAny = 0, ssNoFit = 1, ssCove = 2, ssObtuseCove = 3, ssSquareCove = 4,
    ssObtuseSquareCove = 5, ssSquare = 6, ssThin = 7, ssOval = 8, ssExaggerated = 9, ssTriangle = 10, ssNormalSans = 11,
    ssObtuseSans = 12, ssPerpendicularSans = 13, ssFlared = 14, ssRounded = 15);
  TdxFontFilePanoseWeight = (wAny = 0, wNoFit = 1, wVeryLight = 2, wLight = 3, wThin = 4, wBook = 5, wMedium = 6,
    wDemi = 7, wBold = 8, wHeavy = 9, wBlack = 10, wExtraBlack = 11);
  TdxFontFilePanoseProportion = (pAny = 0, pNoFit = 1, pOldStyle = 2, pModern = 3, pEvenWidth = 4, pExtended = 5,
    pCondensed = 6, pVeryExtended = 7, pVeryCondensed = 8, pMonospaced = 9);
  TdxFontFilePanoseContrast = (cAny = 0, cNoFit = 1, cNone = 2, cVeryLow = 3, cLow = 4, cMediumLow = 5, cMedium = 6,
    cMediumHigh = 7, cHigh = 8, cVeryHigh = 9);
  TdxFontFilePanoseStrokeVariation = (svAny = 0, svNoFit = 1, svNoVariation = 2, svGradualDiagonal = 3,
    svGradualTransitional = 4, svGradualVertical = 5, svGradualHorizontal = 6, svRapidVertical = 7,
    svRapidHorizontal = 8, svInstantVertical = 9, svInstantHorizontal = 10);
  TdxFontFilePanoseArmStyle = (asAny = 0, asNoFit = 1, asStraightArmsHorizontal = 2, asStraightArmsWedge = 3,
    asStraightArmsVertical = 4, asStraightArmsSingleSerif = 5, asStraightArmsDoubleSerif = 6,
    asNonStraightHorizontal = 7, asNonStraightWedge = 8, asNonStraightVertical = 9, asNonStraightSingleSerif = 10,
    asNonStraightDoubleSerif = 11);
  TdxFontFilePanoseLetterForm = (lfAny = 0, lfNoFit = 1, lfNormalContact = 2, lfNormalWeighted = 3, lfNormalBoxed = 4,
    lfNormalFlattened = 5, lfNormalRounded = 6, lfNormalOffCenter = 7, lfNormalSquare = 8, lfObliqueContact = 9,
    lfObliqueWeighted = 10, lfObliqueBoxed = 11, lfObliqueFlattened = 12, lfObliqueRounded = 13, lfObliqueOffCenter = 14,
    lfObliqueSquare = 15);
  TdxFontFilePanoseMidline = (mAny = 0, mNoFit = 1, mStandardTrimmer = 2, mStandardPointed = 3, mStandardSerifed = 4,
    mHighTrimmed = 5, mHighPointed = 6, mHighSerifed = 7, mConstantTrimmed = 8, mConstantPointed = 9,
    mConstantSerifed = 10, mLowTrimmed = 11, mLowPointed = 12, mLowSerifed = 13);
  TdxFontFilePanoseXHeight = (xhAny = 0, xhNoFit = 1, xhConstantSmall = 2, xhConstantStandard = 3, xhConstantLarge = 4,
    xhDuckingSmall = 5, xhDuckingStandard = 6, xhDuckingLarge = 7);

  TdxFontFileStream = class(TdxPDFBytesStream);

	TdxFontFileCMapGlyphRange = record
		EndValue: SmallInt;
		StartValue: SmallInt;
  end;

	TdxFontFileCMapRow = record
    EndCode: SmallInt;
    StartCode: SmallInt;
    IdDelta: SmallInt;
    IdRangeOffset: SmallInt;
  end;

  TdxFontFilePanose = record
  public
    ArmStyle: TdxFontFilePanoseArmStyle;
    Contrast: TdxFontFilePanoseContrast;
    FamilyKind: TdxFontFilePanoseFamilyKind;
    LetterForm: TdxFontFilePanoseLetterform;
    Midline: TdxFontFilePanoseMidline;
    Proportion: TdxFontFilePanoseProportion;
    SerifStyle: TdxFontFilePanoseSerifStyle;
    StrokeVariation: TdxFontFilePanoseStrokeVariation;
    Weight: TdxFontFilePanoseWeight;
    XHeight: TdxFontFilePanoseXHeight;

    class function Create(AStream: TdxFontFileStream): TdxFontFilePanose; static;
    function IsDefault: Boolean;
    procedure Write(AStream: TdxFontFileStream);
  end;

  { TdxType1FontGlyphMapping }

  TdxType1FontGlyphMapping = class
  strict private
    FEncoding: TDictionary<Byte, string>;
    FMapping: TDictionary<string, SmallInt>;
  public
    constructor Create; overload;
    constructor Create(AMapping: TDictionary<string, SmallInt>; AEncoding: TDictionary<Byte, string>); overload;
    destructor Destroy; override;

    property Encoding: TDictionary<Byte, string> read FEncoding;
    property Mapping: TDictionary<string, SmallInt> read FMapping;
  end;

  { TdxGlyphNames }

  TdxGlyphNames = class
  public const
  {$REGION 'public const'}
    _notdef = '.notdef';
    _001_000 = '_001_000';
    _001_001 = '_001_001';
    _001_002 = '_001_002';
    _001_003 = '_001_003';
    A = 'A';
    AE = 'AE';
    AEacute = 'AEacute';
    AEsmall = 'AEsmall';
    Aacute = 'Aacute';
    Aacutesmall = 'Aacutesmall';
    Abreve = 'Abreve';
    Acircumflex = 'Acircumflex';
    Acircumflexsmall = 'Acircumflexsmall';
    Acutesmall = 'Acutesmall';
    Adieresis = 'Adieresis';
    Adieresissmall = 'Adieresissmall';
    Agrave = 'Agrave';
    Agravesmall = 'Agravesmall';
    Alpha = 'Alpha';
    Alphatonos = 'Alphatonos';
    Amacron = 'Amacron';
    Aogonek = 'Aogonek';
    Aring = 'Aring';
    Aringacute = 'Aringacute';
    Aringsmall = 'Aringsmall';
    Asmall = 'Asmall';
    Atilde = 'Atilde';
    Atildesmall = 'Atildesmall';
    B = 'B';
    Beta = 'Beta';
    Black = 'Black';
    Bold = 'Bold';
    Book = 'Book';
    Brevesmall = 'Brevesmall';
    Bsmall = 'Bsmall';
    C = 'C';
    Cacute = 'Cacute';
    Caronsmall = 'Caronsmall';
    Ccaron = 'Ccaron';
    Ccedilla = 'Ccedilla';
    Ccedillasmall = 'Ccedillasmall';
    Ccircumflex = 'Ccircumflex';
    Cdot = 'Cdot';
    Cedillasmall = 'Cedillasmall';
    Chi = 'Chi';
    Circumflexsmall = 'Circumflexsmall';
    Csmall = 'Csmall';
    D = 'D';
    Dcaron = 'Dcaron';
    Dcroat = 'Dcroat';
    Delta = 'Delta';
    Dieresissmall = 'Dieresissmall';
    Dotaccentsmall = 'Dotaccentsmall';
    Dsmall = 'Dsmall';
    E = 'E';
    Eacute = 'Eacute';
    Eacutesmall = 'Eacutesmall';
    Ebreve = 'Ebreve';
    Ecaron = 'Ecaron';
    Ecircumflex = 'Ecircumflex';
    Ecircumflexsmall = 'Ecircumflexsmall';
    Edieresis = 'Edieresis';
    Edieresissmall = 'Edieresissmall';
    Edot = 'Edot';
    Edotaccent = 'Edotaccent';
    Egrave = 'Egrave';
    Egravesmall = 'Egravesmall';
    Emacron = 'Emacron';
    Eng = 'Eng';
    Eogonek = 'Eogonek';
    Epsilon = 'Epsilon';
    Epsilontonos = 'Epsilontonos';
    Esmall = 'Esmall';
    Eta = 'Eta';
    Etatonos = 'Etatonos';
    Eth = 'Eth';
    Ethsmall = 'Ethsmall';
    Euro = 'Euro';
    F = 'F';
    Fsmall = 'Fsmall';
    G = 'G';
    Gamma = 'Gamma';
    Gbreve = 'Gbreve';
    Gcedilla = 'Gcedilla';
    Gcircumflex = 'Gcircumflex';
    Gcommaaccent = 'Gcommaaccent';
    Gdot = 'Gdot';
    Gravesmall = 'Gravesmall';
    Gsmall = 'Gsmall';
    H = 'H';
    H18533 = 'H18533';
    H18543 = 'H18543';
    H18551 = 'H18551';
    H22073 = 'H22073';
    Hbar = 'Hbar';
    Hcircumflex = 'Hcircumflex';
    Hsmall = 'Hsmall';
    Hungarumlautsmall = 'Hungarumlautsmall';
    I = 'I';
    IJ = 'IJ';
    Iacute = 'Iacute';
    Iacutesmall = 'Iacutesmall';
    Ibreve = 'Ibreve';
    Icircumflex = 'Icircumflex';
    Icircumflexsmall = 'Icircumflexsmall';
    Idieresis = 'Idieresis';
    Idieresissmall = 'Idieresissmall';
    Idot = 'Idot';
    Idotaccent = 'Idotaccent';
    Ifraktur = 'Ifraktur';
    Igrave = 'Igrave';
    Igravesmall = 'Igravesmall';
    Imacron = 'Imacron';
    Iogonek = 'Iogonek';
    Iota = 'Iota';
    Iotadieresis = 'Iotadieresis';
    Iotatonos = 'Iotatonos';
    Ismall = 'Ismall';
    Itilde = 'Itilde';
    J = 'J';
    Jcircumflex = 'Jcircumflex';
    Jsmall = 'Jsmall';
    K = 'K';
    Kappa = 'Kappa';
    Kcedilla = 'Kcedilla';
    Kcommaaccent = 'Kcommaaccent';
    Ksmall = 'Ksmall';
    L = 'L';
    Lacute = 'Lacute';
    Lambda = 'Lambda';
    Lcaron = 'Lcaron';
    Lcedilla = 'Lcedilla';
    Lcommaaccent = 'Lcommaaccent';
    Ldot = 'Ldot';
    Light = 'Light';
    Lslash = 'Lslash';
    Lslashsmall = 'Lslashsmall';
    Lsmall = 'Lsmall';
    M = 'M';
    Macronsmall = 'Macronsmall';
    Medium = 'Medium';
    Msmall = 'Msmall';
    Mu = 'Mu';
    N = 'N';
    Nacute = 'Nacute';
    Ncaron = 'Ncaron';
    Ncedilla = 'Ncedilla';
    Ncommaaccent = 'Ncommaaccent';
    Nsmall = 'Nsmall';
    Ntilde = 'Ntilde';
    Ntildesmall = 'Ntildesmall';
    Nu = 'Nu';
    O = 'O';
    OE = 'OE';
    OEsmall = 'OEsmall';
    Oacute = 'Oacute';
    Oacutesmall = 'Oacutesmall';
    Obreve = 'Obreve';
    Ocircumflex = 'Ocircumflex';
    Ocircumflexsmall = 'Ocircumflexsmall';
    Odblacute = 'Odblacute';
    Odieresis = 'Odieresis';
    Odieresissmall = 'Odieresissmall';
    Ogoneksmall = 'Ogoneksmall';
    Ograve = 'Ograve';
    Ogravesmall = 'Ogravesmall';
    Ohm = 'Ohm';
    Ohungarumlaut = 'Ohungarumlaut';
    Omacron = 'Omacron';
    Omega = 'Omega';
    Omegatonos = 'Omegatonos';
    Omicron = 'Omicron';
    Omicrontonos = 'Omicrontonos';
    Oslash = 'Oslash';
    Oslashacute = 'Oslashacute';
    Oslashsmall = 'Oslashsmall';
    Osmall = 'Osmall';
    Otilde = 'Otilde';
    Otildesmall = 'Otildesmall';
    P = 'P';
    Phi = 'Phi';
    Pi = 'Pi';
    Psi = 'Psi';
    Psmall = 'Psmall';
    Q = 'Q';
    Qsmall = 'Qsmall';
    R = 'R';
    Racute = 'Racute';
    Rcaron = 'Rcaron';
    Rcedilla = 'Rcedilla';
    Rcommaaccent = 'Rcommaaccent';
    Regular = 'Regular';
    Rfraktur = 'Rfraktur';
    Rho = 'Rho';
    Ringsmall = 'Ringsmall';
    Roman = 'Roman';
    Rsmall = 'Rsmall';
    S = 'S';
    SF010000 = 'SF010000';
    SF020000 = 'SF020000';
    SF030000 = 'SF030000';
    SF040000 = 'SF040000';
    SF050000 = 'SF050000';
    SF060000 = 'SF060000';
    SF070000 = 'SF070000';
    SF080000 = 'SF080000';
    SF090000 = 'SF090000';
    SF100000 = 'SF100000';
    SF110000 = 'SF110000';
    SF190000 = 'SF190000';
    SF200000 = 'SF200000';
    SF210000 = 'SF210000';
    SF220000 = 'SF220000';
    SF230000 = 'SF230000';
    SF240000 = 'SF240000';
    SF250000 = 'SF250000';
    SF260000 = 'SF260000';
    SF270000 = 'SF270000';
    SF280000 = 'SF280000';
    SF360000 = 'SF360000';
    SF370000 = 'SF370000';
    SF380000 = 'SF380000';
    SF390000 = 'SF390000';
    SF400000 = 'SF400000';
    SF410000 = 'SF410000';
    SF420000 = 'SF420000';
    SF430000 = 'SF430000';
    SF440000 = 'SF440000';
    SF450000 = 'SF450000';
    SF460000 = 'SF460000';
    SF470000 = 'SF470000';
    SF480000 = 'SF480000';
    SF490000 = 'SF490000';
    SF500000 = 'SF500000';
    SF510000 = 'SF510000';
    SF520000 = 'SF520000';
    SF530000 = 'SF530000';
    SF540000 = 'SF540000';
    Sacute = 'Sacute';
    Scaron = 'Scaron';
    Scaronsmall = 'Scaronsmall';
    Scedilla = 'Scedilla';
    Scircumflex = 'Scircumflex';
    Scommaaccent = 'Scommaaccent';
    Semibold = 'Semibold';
    Sigma = 'Sigma';
    Ssmall = 'Ssmall';
    Space = ' ';
    T = 'T';
    Tau = 'Tau';
    Tbar = 'Tbar';
    Tcaron = 'Tcaron';
    Tcedilla = 'Tcedilla';
    Tcommaaccent = 'Tcommaaccent';
    Theta = 'Theta';
    Thorn = 'Thorn';
    Thornsmall = 'Thornsmall';
    Tildesmall = 'Tildesmall';
    Tsmall = 'Tsmall';
    U = 'U';
    Uacute = 'Uacute';
    Uacutesmall = 'Uacutesmall';
    Ubreve = 'Ubreve';
    Ucircumflex = 'Ucircumflex';
    Ucircumflexsmall = 'Ucircumflexsmall';
    Udblacute = 'Udblacute';
    Udieresis = 'Udieresis';
    Udieresissmall = 'Udieresissmall';
    Ugrave = 'Ugrave';
    Ugravesmall = 'Ugravesmall';
    Uhungarumlaut = 'Uhungarumlaut';
    Umacron = 'Umacron';
    Uogonek = 'Uogonek';
    Upsilon = 'Upsilon';
    Upsilon1 = 'Upsilon1';
    Upsilondieresis = 'Upsilondieresis';
    Upsilontonos = 'Upsilontonos';
    Uring = 'Uring';
    Usmall = 'Usmall';
    Utilde = 'Utilde';
    V = 'V';
    Vsmall = 'Vsmall';
    W = 'W';
    Wacute = 'Wacute';
    Wcircumflex = 'Wcircumflex';
    Wdieresis = 'Wdieresis';
    Wgrave = 'Wgrave';
    Wsmall = 'Wsmall';
    X = 'X';
    Xi = 'Xi';
    Xsmall = 'Xsmall';
    Y = 'Y';
    Yacute = 'Yacute';
    Yacutesmall = 'Yacutesmall';
    Ycircumflex = 'Ycircumflex';
    Ydieresis = 'Ydieresis';
    Ydieresissmall = 'Ydieresissmall';
    Ygrave = 'Ygrave';
    Ysmall = 'Ysmall';
    Z = 'Z';
    Zacute = 'Zacute';
    Zcaron = 'Zcaron';
    Zcaronsmall = 'Zcaronsmall';
    Zdot = 'Zdot';
    Zdotaccent = 'Zdotaccent';
    Zeta = 'Zeta';
    Zsmall = 'Zsmall';
    LowerA = 'a';
    LowerA1 = 'a1';
    LowerA10 = 'a10';
    LowerA100 = 'a100';
    LowerA101 = 'a101';
    LowerA102 = 'a102';
    LowerA103 = 'a103';
    LowerA104 = 'a104';
    LowerA105 = 'a105';
    LowerA106 = 'a106';
    LowerA107 = 'a107';
    LowerA108 = 'a108';
    LowerA109 = 'a109';
    LowerA11 = 'a11';
    LowerA110 = 'a110';
    LowerA111 = 'a111';
    LowerA112 = 'a112';
    LowerA117 = 'a117';
    LowerA118 = 'a118';
    LowerA119 = 'a119';
    LowerA12 = 'a12';
    LowerA120 = 'a120';
    LowerA121 = 'a121';
    LowerA122 = 'a122';
    LowerA123 = 'a123';
    LowerA124 = 'a124';
    LowerA125 = 'a125';
    LowerA126 = 'a126';
    LowerA127 = 'a127';
    LowerA128 = 'a128';
    LowerA129 = 'a129';
    LowerA13 = 'a13';
    LowerA130 = 'a130';
    LowerA131 = 'a131';
    LowerA132 = 'a132';
    LowerA133 = 'a133';
    LowerA134 = 'a134';
    LowerA135 = 'a135';
    LowerA136 = 'a136';
    LowerA137 = 'a137';
    LowerA138 = 'a138';
    LowerA139 = 'a139';
    LowerA14 = 'a14';
    LowerA140 = 'a140';
    LowerA141 = 'a141';
    LowerA142 = 'a142';
    LowerA143 = 'a143';
    LowerA144 = 'a144';
    LowerA145 = 'a145';
    LowerA146 = 'a146';
    LowerA147 = 'a147';
    LowerA148 = 'a148';
    LowerA149 = 'a149';
    LowerA15 = 'a15';
    LowerA150 = 'a150';
    LowerA151 = 'a151';
    LowerA152 = 'a152';
    LowerA153 = 'a153';
    LowerA154 = 'a154';
    LowerA155 = 'a155';
    LowerA156 = 'a156';
    LowerA157 = 'a157';
    LowerA158 = 'a158';
    LowerA159 = 'a159';
    LowerA16 = 'a16';
    LowerA160 = 'a160';
    LowerA161 = 'a161';
    LowerA162 = 'a162';
    LowerA163 = 'a163';
    LowerA164 = 'a164';
    LowerA165 = 'a165';
    LowerA166 = 'a166';
    LowerA167 = 'a167';
    LowerA168 = 'a168';
    LowerA169 = 'a169';
    LowerA17 = 'a17';
    LowerA170 = 'a170';
    LowerA171 = 'a171';
    LowerA172 = 'a172';
    LowerA173 = 'a173';
    LowerA174 = 'a174';
    LowerA175 = 'a175';
    LowerA176 = 'a176';
    LowerA177 = 'a177';
    LowerA178 = 'a178';
    LowerA179 = 'a179';
    LowerA18 = 'a18';
    LowerA180 = 'a180';
    LowerA181 = 'a181';
    LowerA182 = 'a182';
    LowerA183 = 'a183';
    LowerA184 = 'a184';
    LowerA185 = 'a185';
    LowerA186 = 'a186';
    LowerA187 = 'a187';
    LowerA188 = 'a188';
    LowerA189 = 'a189';
    LowerA19 = 'a19';
    LowerA190 = 'a190';
    LowerA191 = 'a191';
    LowerA192 = 'a192';
    LowerA193 = 'a193';
    LowerA194 = 'a194';
    LowerA195 = 'a195';
    LowerA196 = 'a196';
    LowerA197 = 'a197';
    LowerA198 = 'a198';
    LowerA199 = 'a199';
    LowerA2 = 'a2';
    LowerA20 = 'a20';
    LowerA200 = 'a200';
    LowerA201 = 'a201';
    LowerA202 = 'a202';
    LowerA203 = 'a203';
    LowerA204 = 'a204';
    LowerA205 = 'a205';
    LowerA206 = 'a206';
    LowerA21 = 'a21';
    LowerA22 = 'a22';
    LowerA23 = 'a23';
    LowerA24 = 'a24';
    LowerA25 = 'a25';
    LowerA26 = 'a26';
    LowerA27 = 'a27';
    LowerA28 = 'a28';
    LowerA29 = 'a29';
    LowerA3 = 'a3';
    LowerA30 = 'a30';
    LowerA31 = 'a31';
    LowerA32 = 'a32';
    LowerA33 = 'a33';
    LowerA34 = 'a34';
    LowerA35 = 'a35';
    LowerA36 = 'a36';
    LowerA37 = 'a37';
    LowerA38 = 'a38';
    LowerA39 = 'a39';
    LowerA4 = 'a4';
    LowerA40 = 'a40';
    LowerA41 = 'a41';
    LowerA42 = 'a42';
    LowerA43 = 'a43';
    LowerA44 = 'a44';
    LowerA45 = 'a45';
    LowerA46 = 'a46';
    LowerA47 = 'a47';
    LowerA48 = 'a48';
    LowerA49 = 'a49';
    LowerA5 = 'a5';
    LowerA50 = 'a50';
    LowerA51 = 'a51';
    LowerA52 = 'a52';
    LowerA53 = 'a53';
    LowerA54 = 'a54';
    LowerA55 = 'a55';
    LowerA56 = 'a56';
    LowerA57 = 'a57';
    LowerA58 = 'a58';
    LowerA59 = 'a59';
    LowerA6 = 'a6';
    LowerA60 = 'a60';
    LowerA61 = 'a61';
    LowerA62 = 'a62';
    LowerA63 = 'a63';
    LowerA64 = 'a64';
    LowerA65 = 'a65';
    LowerA66 = 'a66';
    LowerA67 = 'a67';
    LowerA68 = 'a68';
    LowerA69 = 'a69';
    LowerA7 = 'a7';
    LowerA70 = 'a70';
    LowerA71 = 'a71';
    LowerA72 = 'a72';
    LowerA73 = 'a73';
    LowerA74 = 'a74';
    LowerA75 = 'a75';
    LowerA76 = 'a76';
    LowerA77 = 'a77';
    LowerA78 = 'a78';
    LowerA79 = 'a79';
    LowerA8 = 'a8';
    LowerA81 = 'a81';
    LowerA82 = 'a82';
    LowerA83 = 'a83';
    LowerA84 = 'a84';
    LowerA85 = 'a85';
    LowerA86 = 'a86';
    LowerA87 = 'a87';
    LowerA88 = 'a88';
    LowerA89 = 'a89';
    LowerA9 = 'a9';
    LowerA90 = 'a90';
    LowerA91 = 'a91';
    LowerA92 = 'a92';
    LowerA93 = 'a93';
    LowerA94 = 'a94';
    LowerA95 = 'a95';
    LowerA96 = 'a96';
    LowerA97 = 'a97';
    LowerA98 = 'a98';
    LowerA99 = 'a99';
    LowerAacute = 'aacute';
    LowerAbreve = 'abreve';
    LowerAcircumflex = 'acircumflex';
    LowerAcute = 'acute';
    LowerAdieresis = 'adieresis';
    LowerAe = 'ae';
    LowerAeacute = 'aeacute';
    LowerAfii00208 = 'afii00208';
    LowerAfii08941 = 'afii08941';
    LowerAfii10017 = 'afii10017';
    LowerAfii10018 = 'afii10018';
    LowerAfii10019 = 'afii10019';
    LowerAfii10020 = 'afii10020';
    LowerAfii10021 = 'afii10021';
    LowerAfii10022 = 'afii10022';
    LowerAfii10023 = 'afii10023';
    LowerAfii10024 = 'afii10024';
    LowerAfii10025 = 'afii10025';
    LowerAfii10026 = 'afii10026';
    LowerAfii10027 = 'afii10027';
    LowerAfii10028 = 'afii10028';
    LowerAfii10029 = 'afii10029';
    LowerAfii10030 = 'afii10030';
    LowerAfii10031 = 'afii10031';
    LowerAfii10032 = 'afii10032';
    LowerAfii10033 = 'afii10033';
    LowerAfii10034 = 'afii10034';
    LowerAfii10035 = 'afii10035';
    LowerAfii10036 = 'afii10036';
    LowerAfii10037 = 'afii10037';
    LowerAfii10038 = 'afii10038';
    LowerAfii10039 = 'afii10039';
    LowerAfii10040 = 'afii10040';
    LowerAfii10041 = 'afii10041';
    LowerAfii10042 = 'afii10042';
    LowerAfii10043 = 'afii10043';
    LowerAfii10044 = 'afii10044';
    LowerAfii10045 = 'afii10045';
    LowerAfii10046 = 'afii10046';
    LowerAfii10047 = 'afii10047';
    LowerAfii10048 = 'afii10048';
    LowerAfii10049 = 'afii10049';
    LowerAfii10050 = 'afii10050';
    LowerAfii10051 = 'afii10051';
    LowerAfii10052 = 'afii10052';
    LowerAfii10053 = 'afii10053';
    LowerAfii10054 = 'afii10054';
    LowerAfii10055 = 'afii10055';
    LowerAfii10056 = 'afii10056';
    LowerAfii10057 = 'afii10057';
    LowerAfii10058 = 'afii10058';
    LowerAfii10059 = 'afii10059';
    LowerAfii10060 = 'afii10060';
    LowerAfii10061 = 'afii10061';
    LowerAfii10062 = 'afii10062';
    LowerAfii10065 = 'afii10065';
    LowerAfii10066 = 'afii10066';
    LowerAfii10067 = 'afii10067';
    LowerAfii10068 = 'afii10068';
    LowerAfii10069 = 'afii10069';
    LowerAfii10070 = 'afii10070';
    LowerAfii10071 = 'afii10071';
    LowerAfii10072 = 'afii10072';
    LowerAfii10073 = 'afii10073';
    LowerAfii10074 = 'afii10074';
    LowerAfii10075 = 'afii10075';
    LowerAfii10076 = 'afii10076';
    LowerAfii10077 = 'afii10077';
    LowerAfii10078 = 'afii10078';
    LowerAfii10079 = 'afii10079';
    LowerAfii10080 = 'afii10080';
    LowerAfii10081 = 'afii10081';
    LowerAfii10082 = 'afii10082';
    LowerAfii10083 = 'afii10083';
    LowerAfii10084 = 'afii10084';
    LowerAfii10085 = 'afii10085';
    LowerAfii10086 = 'afii10086';
    LowerAfii10087 = 'afii10087';
    LowerAfii10088 = 'afii10088';
    LowerAfii10089 = 'afii10089';
    LowerAfii10090 = 'afii10090';
    LowerAfii10091 = 'afii10091';
    LowerAfii10092 = 'afii10092';
    LowerAfii10093 = 'afii10093';
    LowerAfii10094 = 'afii10094';
    LowerAfii10095 = 'afii10095';
    LowerAfii10096 = 'afii10096';
    LowerAfii10097 = 'afii10097';
    LowerAfii10098 = 'afii10098';
    LowerAfii10099 = 'afii10099';
    LowerAfii10100 = 'afii10100';
    LowerAfii10101 = 'afii10101';
    LowerAfii10102 = 'afii10102';
    LowerAfii10103 = 'afii10103';
    LowerAfii10104 = 'afii10104';
    LowerAfii10105 = 'afii10105';
    LowerAfii10106 = 'afii10106';
    LowerAfii10107 = 'afii10107';
    LowerAfii10108 = 'afii10108';
    LowerAfii10109 = 'afii10109';
    LowerAfii10110 = 'afii10110';
    LowerAfii10145 = 'afii10145';
    LowerAfii10193 = 'afii10193';
    LowerAfii61248 = 'afii61248';
    LowerAfii61289 = 'afii61289';
    LowerAfii61352 = 'afii61352';
    LowerAgrave = 'agrave';
    LowerAleph = 'aleph';
    LowerAlpha = 'alpha';
    LowerAlphatonos = 'alphatonos';
    LowerAmacron = 'amacron';
    LowerAmpersand = 'ampersand';
    LowerAmpersandsmall = 'ampersandsmall';
    LowerAngle = 'angle';
    LowerAngleleft = 'angleleft';
    LowerAngleright = 'angleright';
    LowerAnoteleia = 'anoteleia';
    LowerAogonek = 'aogonek';
    LowerApple = 'apple';
    LowerApplelogo = 'applelogo';
    LowerApproxequal = 'approxequal';
    LowerAring = 'aring';
    LowerAringacute = 'aringacute';
    LowerArrowboth = 'arrowboth';
    LowerArrowdblboth = 'arrowdblboth';
    LowerArrowdbldown = 'arrowdbldown';
    LowerArrowdblleft = 'arrowdblleft';
    LowerArrowdblright = 'arrowdblright';
    LowerArrowdblup = 'arrowdblup';
    LowerArrowdown = 'arrowdown';
    LowerArrowhorizex = 'arrowhorizex';
    LowerArrowleft = 'arrowleft';
    LowerArrowright = 'arrowright';
    LowerArrowup = 'arrowup';
    LowerArrowupdn = 'arrowupdn';
    LowerArrowupdnbse = 'arrowupdnbse';
    LowerArrowvertex = 'arrowvertex';
    LowerAsciicircum = 'asciicircum';
    LowerAsciitilde = 'asciitilde';
    LowerAsterisk = 'asterisk';
    LowerAsteriskmath = 'asteriskmath';
    LowerAsuperior = 'asuperior';
    LowerAt = 'at';
    LowerAtilde = 'atilde';
    LowerB = 'b';
    LowerBackslash = 'backslash';
    LowerBar = 'bar';
    LowerBeta = 'beta';
    LowerBlock = 'block';
    LowerBraceex = 'braceex';
    LowerBraceleft = 'braceleft';
    LowerBraceleftbt = 'braceleftbt';
    LowerBraceleftmid = 'braceleftmid';
    LowerBracelefttp = 'bracelefttp';
    LowerBraceright = 'braceright';
    LowerBracerightbt = 'bracerightbt';
    LowerBracerightmid = 'bracerightmid';
    LowerBracerighttp = 'bracerighttp';
    LowerBracketleft = 'bracketleft';
    LowerBracketleftbt = 'bracketleftbt';
    LowerBracketleftex = 'bracketleftex';
    LowerBracketlefttp = 'bracketlefttp';
    LowerBracketright = 'bracketright';
    LowerBracketrightbt = 'bracketrightbt';
    LowerBracketrightex = 'bracketrightex';
    LowerBracketrighttp = 'bracketrighttp';
    LowerBreve = 'breve';
    LowerBrokenbar = 'brokenbar';
    LowerBsuperior = 'bsuperior';
    LowerBullet = 'bullet';
    LowerC = 'c';
    LowerCacute = 'cacute';
    LowerCaron = 'caron';
    LowerCarriagereturn = 'carriagereturn';
    LowerCcaron = 'ccaron';
    LowerCcedilla = 'ccedilla';
    LowerCcircumflex = 'ccircumflex';
    LowerCdot = 'cdot';
    LowerCedilla = 'cedilla';
    LowerCent = 'cent';
    LowerCentinferior = 'centinferior';
    LowerCentoldstyle = 'centoldstyle';
    LowerCentsuperior = 'centsuperior';
    LowerChi = 'chi';
    LowerCircle = 'circle';
    LowerCirclemultiply = 'circlemultiply';
    LowerCircleplus = 'circleplus';
    LowerCircumflex = 'circumflex';
    LowerClub = 'club';
    LowerColon = 'colon';
    LowerColonmonetary = 'colonmonetary';
    LowerComma = 'comma';
    LowerCommaaccent = 'commaaccent';
    LowerCommainferior = 'commainferior';
    LowerCommasuperior = 'commasuperior';
    LowerCongruent = 'congruent';
    LowerCopyright = 'copyright';
    LowerCopyrightsans = 'copyrightsans';
    LowerCopyrightserif = 'copyrightserif';
    LowerCurrency = 'currency';
    LowerD = 'd';
    LowerDagger = 'dagger';
    LowerDaggerdbl = 'daggerdbl';
    LowerDcaron = 'dcaron';
    LowerDcroat = 'dcroat';
    LowerDegree = 'degree';
    LowerDelta = 'delta';
    LowerDiamond = 'diamond';
    LowerDieresis = 'dieresis';
    LowerDieresistonos = 'dieresistonos';
    LowerDivide = 'divide';
    LowerDkshade = 'dkshade';
    LowerDnblock = 'dnblock';
    LowerDollar = 'dollar';
    LowerDollarinferior = 'dollarinferior';
    LowerDollaroldstyle = 'dollaroldstyle';
    LowerDollarsuperior = 'dollarsuperior';
    LowerDotaccent = 'dotaccent';
    LowerDotlessi = 'dotlessi';
    LowerDotmath = 'dotmath';
    LowerDsuperior = 'dsuperior';
    LowerE = 'e';
    LowerEacute = 'eacute';
    LowerEbreve = 'ebreve';
    LowerEcaron = 'ecaron';
    LowerEcircumflex = 'ecircumflex';
    LowerEdieresis = 'edieresis';
    LowerEdot = 'edot';
    LowerEdotaccent = 'edotaccent';
    LowerEgrave = 'egrave';
    LowerEight = 'eight';
    LowerEightinferior = 'eightinferior';
    LowerEightoldstyle = 'eightoldstyle';
    LowerEightsuperior = 'eightsuperior';
    LowerElement = 'element';
    LowerEllipsis = 'ellipsis';
    LowerEmacron = 'emacron';
    LowerEmdash = 'emdash';
    LowerEmptyset = 'emptyset';
    LowerEndash = 'endash';
    LowerEng = 'eng';
    LowerEogonek = 'eogonek';
    LowerEpsilon = 'epsilon';
    LowerEpsilontonos = 'epsilontonos';
    LowerEqual = 'equal';
    LowerEquivalence = 'equivalence';
    LowerEstimated = 'estimated';
    LowerEsuperior = 'esuperior';
    LowerEta = 'eta';
    LowerEtatonos = 'etatonos';
    LowerEth = 'eth';
    LowerExclam = 'exclam';
    LowerExclamdbl = 'exclamdbl';
    LowerExclamdown = 'exclamdown';
    LowerExclamdownsmall = 'exclamdownsmall';
    LowerExclamsmall = 'exclamsmall';
    LowerExistential = 'existential';
    LowerF = 'f';
    LowerFemale = 'female';
    LowerFf = 'ff';
    LowerFfi = 'ffi';
    LowerFfl = 'ffl';
    LowerFi = 'fi';
    LowerFiguredash = 'figuredash';
    LowerFilledbox = 'filledbox';
    LowerFilledrect = 'filledrect';
    LowerFive = 'five';
    LowerFiveeighths = 'fiveeighths';
    LowerFiveinferior = 'fiveinferior';
    LowerFiveoldstyle = 'fiveoldstyle';
    LowerFivesuperior = 'fivesuperior';
    LowerFl = 'fl';
    LowerFlorin = 'florin';
    LowerFour = 'four';
    LowerFourinferior = 'fourinferior';
    LowerFouroldstyle = 'fouroldstyle';
    LowerFoursuperior = 'foursuperior';
    LowerFraction = 'fraction';
    LowerFranc = 'franc';
    LowerG = 'g';
    LowerGamma = 'gamma';
    LowerGbreve = 'gbreve';
    LowerGcedilla = 'gcedilla';
    LowerGcircumflex = 'gcircumflex';
    LowerGcommaaccent = 'gcommaaccent';
    LowerGdot = 'gdot';
    LowerGermandbls = 'germandbls';
    LowerGradient = 'gradient';
    LowerGrave = 'grave';
    LowerGreater = 'greater';
    LowerGreaterequal = 'greaterequal';
    LowerGuillemotleft = 'guillemotleft';
    LowerGuillemotright = 'guillemotright';
    LowerGuilsinglleft = 'guilsinglleft';
    LowerGuilsinglright = 'guilsinglright';
    LowerH = 'h';
    LowerHbar = 'hbar';
    LowerHcircumflex = 'hcircumflex';
    LowerHeart = 'heart';
    LowerHouse = 'house';
    LowerHungarumlaut = 'hungarumlaut';
    LowerHyphen = 'hyphen';
    LowerHypheninferior = 'hypheninferior';
    LowerHyphensuperior = 'hyphensuperior';
    LowerI = 'i';
    LowerIacute = 'iacute';
    LowerIbreve = 'ibreve';
    LowerIcircumflex = 'icircumflex';
    LowerIdieresis = 'idieresis';
    LowerIgrave = 'igrave';
    LowerIj = 'ij';
    LowerImacron = 'imacron';
    LowerIncrement = 'increment';
    LowerInfinity = 'infinity';
    LowerIntegral = 'integral';
    LowerIntegralbt = 'integralbt';
    LowerIntegralex = 'integralex';
    LowerIntegraltp = 'integraltp';
    LowerIntersection = 'intersection';
    LowerInvbullet = 'invbullet';
    LowerInvcircle = 'invcircle';
    LowerInvsmileface = 'invsmileface';
    LowerIogonek = 'iogonek';
    LowerIota = 'iota';
    LowerIotadieresis = 'iotadieresis';
    LowerIotadieresistonos = 'iotadieresistonos';
    LowerIotatonos = 'iotatonos';
    LowerIsuperior = 'isuperior';
    LowerItilde = 'itilde';
    LowerJ = 'j';
    LowerJcircumflex = 'jcircumflex';
    LowerK = 'k';
    LowerKappa = 'kappa';
    LowerKcedilla = 'kcedilla';
    LowerKcommaaccent = 'kcommaaccent';
    LowerKgreenlandic = 'kgreenlandic';
    LowerL = 'l';
    LowerLacute = 'lacute';
    LowerLambda = 'lambda';
    LowerLcaron = 'lcaron';
    LowerLcedilla = 'lcedilla';
    LowerLcommaaccent = 'lcommaaccent';
    LowerLdot = 'ldot';
    LowerLess = 'less';
    LowerLessequal = 'lessequal';
    LowerLfblock = 'lfblock';
    LowerLogicaland = 'logicaland';
    LowerLogicalnot = 'logicalnot';
    LowerLogicalor = 'logicalor';
    LowerLongs = 'longs';
    LowerLozenge = 'lozenge';
    LowerLslash = 'lslash';
    LowerLsuperior = 'lsuperior';
    LowerLtshade = 'ltshade';
    LowerM = 'm';
    LowerMacron = 'macron';
    LowerMale = 'male';
    LowerMinus = 'minus';
    LowerMinute = 'minute';
    LowerMsuperior = 'msuperior';
    LowerMu = 'mu';
    LowerMultiply = 'multiply';
    LowerMusicalnote = 'musicalnote';
    LowerMusicalnotedbl = 'musicalnotedbl';
    LowerN = 'n';
    LowerNacute = 'nacute';
    LowerNapostrophe = 'napostrophe';
    LowerNbspace = 'nbspace';
    LowerNcaron = 'ncaron';
    LowerNcedilla = 'ncedilla';
    LowerNcommaaccent = 'ncommaaccent';
    LowerNine = 'nine';
    LowerNineinferior = 'nineinferior';
    LowerNineoldstyle = 'nineoldstyle';
    LowerNinesuperior = 'ninesuperior';
    LowerNonbreakingspace = 'nonbreakingspace';
    LowerNonmarkingreturn = 'nonmarkingreturn';
    LowerNotelement = 'notelement';
    LowerNotequal = 'notequal';
    LowerNotsubset = 'notsubset';
    LowerNsuperior = 'nsuperior';
    LowerNtilde = 'ntilde';
    LowerNu = 'nu';
    Null = 'null';
    LowerNumbersign = 'numbersign';
    LowerO = 'o';
    LowerOacute = 'oacute';
    LowerObreve = 'obreve';
    LowerOcircumflex = 'ocircumflex';
    LowerOdblacute = 'odblacute';
    LowerOdieresis = 'odieresis';
    LowerOe = 'oe';
    LowerOgonek = 'ogonek';
    LowerOgrave = 'ograve';
    LowerOhungarumlaut = 'ohungarumlaut';
    LowerOmacron = 'omacron';
    LowerOmega = 'omega';
    LowerOmega1 = 'omega1';
    LowerOmegatonos = 'omegatonos';
    LowerOmicron = 'omicron';
    LowerOmicrontonos = 'omicrontonos';
    LowerOne = 'one';
    LowerOnedotenleader = 'onedotenleader';
    LowerOneeighth = 'oneeighth';
    LowerOnefitted = 'onefitted';
    LowerOnehalf = 'onehalf';
    LowerOneinferior = 'oneinferior';
    LowerOneoldstyle = 'oneoldstyle';
    LowerOnequarter = 'onequarter';
    LowerOnesuperior = 'onesuperior';
    LowerOnethird = 'onethird';
    LowerOpenbullet = 'openbullet';
    LowerOrdfeminine = 'ordfeminine';
    LowerOrdmasculine = 'ordmasculine';
    LowerOrthogonal = 'orthogonal';
    LowerOslash = 'oslash';
    LowerOslashacute = 'oslashacute';
    LowerOsuperior = 'osuperior';
    LowerOtilde = 'otilde';
    LowerP = 'p';
    LowerParagraph = 'paragraph';
    LowerParenleft = 'parenleft';
    LowerParenleftbt = 'parenleftbt';
    LowerParenleftex = 'parenleftex';
    LowerParenleftinferior = 'parenleftinferior';
    LowerParenleftsuperior = 'parenleftsuperior';
    LowerParenlefttp = 'parenlefttp';
    LowerParenright = 'parenright';
    LowerParenrightbt = 'parenrightbt';
    LowerParenrightex = 'parenrightex';
    LowerParenrightinferior = 'parenrightinferior';
    LowerParenrightsuperior = 'parenrightsuperior';
    LowerParenrighttp = 'parenrighttp';
    LowerPartialdiff = 'partialdiff';
    LowerPercent = 'percent';
    LowerPeriod = 'period';
    LowerPeriodcentered = 'periodcentered';
    LowerPeriodinferior = 'periodinferior';
    LowerPeriodsuperior = 'periodsuperior';
    LowerPerpendicular = 'perpendicular';
    LowerPerthousand = 'perthousand';
    LowerPeseta = 'peseta';
    LowerPhi = 'phi';
    LowerPhi1 = 'phi1';
    LowerPi = 'pi';
    LowerPlus = 'plus';
    LowerPlusminus = 'plusminus';
    LowerProduct = 'product';
    LowerPropersubset = 'propersubset';
    LowerPropersuperset = 'propersuperset';
    LowerProportional = 'proportional';
    LowerPsi = 'psi';
    LowerQ = 'q';
    LowerQuestion = 'question';
    LowerQuestiondown = 'questiondown';
    LowerQuestiondownsmall = 'questiondownsmall';
    LowerQuestionsmall = 'questionsmall';
    LowerQuotedbl = 'quotedbl';
    LowerQuotedblbase = 'quotedblbase';
    LowerQuotedblleft = 'quotedblleft';
    LowerQuotedblright = 'quotedblright';
    LowerQuoteleft = 'quoteleft';
    LowerQuotereversed = 'quotereversed';
    LowerQuoteright = 'quoteright';
    LowerQuotesinglbase = 'quotesinglbase';
    LowerQuotesingle = 'quotesingle';
    LowerR = 'r';
    LowerRacute = 'racute';
    LowerRadical = 'radical';
    LowerRadicalex = 'radicalex';
    LowerRcaron = 'rcaron';
    LowerRcedilla = 'rcedilla';
    LowerRcommaaccent = 'rcommaaccent';
    LowerReflexsubset = 'reflexsubset';
    LowerReflexsuperset = 'reflexsuperset';
    LowerRegistered = 'registered';
    LowerRegistersans = 'registersans';
    LowerRegisterserif = 'registerserif';
    LowerRevlogicalnot = 'revlogicalnot';
    LowerRho = 'rho';
    LowerRing = 'ring';
    LowerRsuperior = 'rsuperior';
    LowerRtblock = 'rtblock';
    LowerRupiah = 'rupiah';
    LowerS = 's';
    LowerSacute = 'sacute';
    LowerScaron = 'scaron';
    LowerScedilla = 'scedilla';
    LowerScircumflex = 'scircumflex';
    LowerScommaaccent = 'scommaaccent';
    LowerSecond = 'second';
    LowerSection = 'section';
    LowerSemicolon = 'semicolon';
    LowerSeven = 'seven';
    LowerSeveneighths = 'seveneighths';
    LowerSeveninferior = 'seveninferior';
    LowerSevenoldstyle = 'sevenoldstyle';
    LowerSevensuperior = 'sevensuperior';
    LowerSfthyphen = 'sfthyphen';
    LowerShade = 'shade';
    LowerSigma = 'sigma';
    LowerSigma1 = 'sigma1';
    LowerSimilar = 'similar';
    LowerSix = 'six';
    LowerSixinferior = 'sixinferior';
    LowerSixoldstyle = 'sixoldstyle';
    LowerSixsuperior = 'sixsuperior';
    LowerSlash = 'slash';
    LowerSmileface = 'smileface';
    LowerSpace = 'space';
    LowerSpade = 'spade';
    LowerSsuperior = 'ssuperior';
    LowerSterling = 'sterling';
    LowerSuchthat = 'suchthat';
    LowerSummation = 'summation';
    LowerSun = 'sun';
    LowerT = 't';
    LowerTau = 'tau';
    LowerTbar = 'tbar';
    LowerTcaron = 'tcaron';
    LowerTcedilla = 'tcedilla';
    LowerTcommaaccent = 'tcommaaccent';
    LowerTherefore = 'therefore';
    LowerTheta = 'theta';
    LowerTheta1 = 'theta1';
    LowerThorn = 'thorn';
    LowerThree = 'three';
    LowerThreeeighths = 'threeeighths';
    LowerThreeinferior = 'threeinferior';
    LowerThreeoldstyle = 'threeoldstyle';
    LowerThreequarters = 'threequarters';
    LowerThreequartersemdash = 'threequartersemdash';
    LowerThreesuperior = 'threesuperior';
    LowerTilde = 'tilde';
    LowerTonos = 'tonos';
    LowerTrademark = 'trademark';
    LowerTrademarksans = 'trademarksans';
    LowerTrademarkserif = 'trademarkserif';
    LowerTriagdn = 'triagdn';
    LowerTriaglf = 'triaglf';
    LowerTriagrt = 'triagrt';
    LowerTriagup = 'triagup';
    LowerTsuperior = 'tsuperior';
    LowerTwo = 'two';
    LowerTwodotenleader = 'twodotenleader';
    LowerTwoinferior = 'twoinferior';
    LowerTwooldstyle = 'twooldstyle';
    LowerTwosuperior = 'twosuperior';
    LowerTwothirds = 'twothirds';
    LowerU = 'u';
    LowerUacute = 'uacute';
    LowerUbreve = 'ubreve';
    LowerUcircumflex = 'ucircumflex';
    LowerUdblacute = 'udblacute';
    LowerUdieresis = 'udieresis';
    LowerUgrave = 'ugrave';
    LowerUhungarumlaut = 'uhungarumlaut';
    LowerUmacron = 'umacron';
    LowerUnderscore = 'underscore';
    LowerUnderscoredbl = 'underscoredbl';
    LowerUnion = 'union';
    LowerUniversal = 'universal';
    LowerUogonek = 'uogonek';
    LowerUpblock = 'upblock';
    LowerUpsilon = 'upsilon';
    LowerUpsilondieresis = 'upsilondieresis';
    LowerUpsilondieresistonos = 'upsilondieresistonos';
    LowerUpsilontonos = 'upsilontonos';
    LowerUring = 'uring';
    LowerUtilde = 'utilde';
    LowerV = 'v';
    LowerW = 'w';
    LowerWacute = 'wacute';
    LowerWcircumflex = 'wcircumflex';
    LowerWdieresis = 'wdieresis';
    LowerWeierstrass = 'weierstrass';
    LowerWgrave = 'wgrave';
    LowerX = 'x';
    LowerXi = 'xi';
    LowerY = 'y';
    LowerYacute = 'yacute';
    LowerYcircumflex = 'ycircumflex';
    LowerYdieresis = 'ydieresis';
    LowerYen = 'yen';
    LowerYgrave = 'ygrave';
    LowerZ = 'z';
    LowerZacute = 'zacute';
    LowerZcaron = 'zcaron';
    LowerZdot = 'zdot';
    LowerZdotaccent = 'zdotaccent';
    LowerZero = 'zero';
    LowerZeroinferior = 'zeroinferior';
    LowerZerooldstyle = 'zerooldstyle';
    LowerZerosuperior = 'zerosuperior';
    LowerZeta = 'zeta';
  {$ENDREGION}
  end;

  { TdxFontFileCustomEncoding }

  TdxFontFileCustomEncoding = class
  strict private
    FDictionary: TDictionary<Byte, string>;
  protected
    procedure Initialize; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    property Dictionary: TDictionary<Byte, string> read FDictionary;
  end;

  TdxFontFileCustomEncodingClass = class of TdxFontFileCustomEncoding;

  { TdxFontFileUnicodeConverter }

  TdxFontFileUnicodeConverter = class
  strict private
    FAdobeGlyphNames: TdxPDFWordDictionary;
    FGlyphCodes: TdxPDFWordDictionary;
    function InternalFindCode(ACode: Word; AEncoding: TdxFontFileCustomEncoding;
      AGlyphCodes: TdxPDFWordDictionary; out AResult: Word): Boolean;
    procedure Initialize;
    procedure InitializeAdobeGlyphNames;
    procedure InitializeAdobeGlyphNames1;
    procedure InitializeAdobeGlyphNames2;
    procedure InitializeAdobeGlyphNames3;
    procedure InitializeAdobeGlyphNames4;
    procedure InitializeGlyphCodes;
  protected
    property GlyphCodes: TdxPDFWordDictionary read FGlyphCodes;
  public
    constructor Create;
    destructor Destroy; override;

    function FindCode(const AName: string; out ACode: Word): Boolean; overload;
    function FindCode(AEncoding: TdxFontFileCustomEncoding; ACode: Word; out AResult: Word): Boolean; overload;
    function FindCode(AEncoding: TdxFontFileCustomEncoding; AGlyphCodes: TdxPDFWordDictionary; ACode: Word; out AResult: Word): Boolean; overload;
  end;

  { TdxFontFileMacRomanEncoding }

  TdxFontFileMacRomanEncoding = class(TdxFontFileCustomEncoding)
  protected
    procedure Initialize; override;
  end;

  { TdxFontFileStandardEncoding }

  TdxFontFileStandardEncoding = class(TdxFontFileCustomEncoding)
  protected
    procedure Initialize; override;
  end;

  { TdxFontFileWinAnsiEncoding }

  TdxFontFileWinAnsiEncoding = class(TdxFontFileCustomEncoding)
  protected
    procedure Initialize; override;
  end;

  { TdxFontFileSymbolEncoding }

  TdxFontFileSymbolEncoding = class(TdxFontFileCustomEncoding)
  protected
    procedure Initialize; override;
  end;

  { TdxFontFileZapfDingbatsEncoding }

  TdxFontFileZapfDingbatsEncoding = class(TdxFontFileCustomEncoding)
  protected
    procedure Initialize; override;
  end;

  { TdxFontFileBinaryTable }

  TdxFontFileBinaryTable = class
  strict private
    FDataStream: TdxFontFileStream;
    FName: string;
    FNeedWrite: Boolean;
    function GetTableData: TBytes;
    function GetDataSize: Integer;
    procedure CalculateCheckSum(var ACheckSum: Integer);
    procedure RecreateStream;
  protected
    procedure DoApplyChanges; virtual;

    function Write(AStream: TdxFontFileStream; AOffset: Integer): Integer;
    procedure ApplyChanges;
    procedure Changed(AIsChanged: Boolean = True);

    property DataStream: TdxFontFileStream read FDataStream;
    property Name: string read FName write FName;
  public
    constructor Create; overload;
    constructor Create(const AData: TBytes); overload; virtual;
    destructor Destroy; override;

    class function Tag: string; virtual;
    function AlignedTableData: TBytes;

    property Data: TBytes read GetTableData;
    property DataSize: Integer read GetDataSize;
    property NeedWrite: Boolean read FNeedWrite;
  end;

  { TdxFontFileLocaTable }

  TdxFontFileLocaTable = class(TdxFontFileBinaryTable)
  strict private
    FIsShortFormat: Boolean;
    FGlyphOffsets: TIntegerDynArray;
    procedure SetGlyphOffsets(const AValue: TIntegerDynArray);
  protected
    procedure DoApplyChanges; override;
  public
    class function Tag: string; override;
    procedure ReadOffsets(AFontFile: TdxFontFile);

    property GlyphOffsets: TIntegerDynArray read FGlyphOffsets write SetGlyphOffsets;
  end;

  { TdxFontFileCFFTable }

  TdxFontFileCFFTable = class(TdxFontFileBinaryTable)
  public
    class function Tag: string; override;
  end;

  { TdxFontFilePostTable }

  TdxFontFilePostTable = class(TdxFontFileBinaryTable)
  strict private const
    StandardMacCharacterSet: array[0.. 257] of string = (TdxGlyphNames._notdef, TdxGlyphNames.Null,
      TdxGlyphNames.LowerNonmarkingreturn, TdxGlyphNames.LowerSpace, TdxGlyphNames.LowerExclam,
      TdxGlyphNames.LowerQuotedbl, TdxGlyphNames.LowerNumbersign, TdxGlyphNames.LowerDollar,
      TdxGlyphNames.LowerPercent, TdxGlyphNames.LowerAmpersand, TdxGlyphNames.LowerQuotesingle,
      TdxGlyphNames.LowerParenleft, TdxGlyphNames.LowerParenright, TdxGlyphNames.LowerAsterisk,
      TdxGlyphNames.LowerPlus, TdxGlyphNames.LowerComma, TdxGlyphNames.LowerHyphen, TdxGlyphNames.LowerPeriod,
      TdxGlyphNames.LowerSlash, TdxGlyphNames.LowerZero, TdxGlyphNames.LowerOne, TdxGlyphNames.LowerTwo,
      TdxGlyphNames.LowerThree, TdxGlyphNames.LowerFour, TdxGlyphNames.LowerFive, TdxGlyphNames.LowerSix,
      TdxGlyphNames.LowerSeven, TdxGlyphNames.LowerEight, TdxGlyphNames.LowerNine,
      TdxGlyphNames.LowerColon, TdxGlyphNames.LowerSemicolon, TdxGlyphNames.LowerLess,
      TdxGlyphNames.LowerEqual, TdxGlyphNames.LowerGreater, TdxGlyphNames.LowerQuestion,
      TdxGlyphNames.LowerAt, TdxGlyphNames.A, TdxGlyphNames.B,
      TdxGlyphNames.C, TdxGlyphNames.D, TdxGlyphNames.E, TdxGlyphNames.F,
      TdxGlyphNames.G, TdxGlyphNames.H, TdxGlyphNames.I, TdxGlyphNames.J,
      TdxGlyphNames.K, TdxGlyphNames.L, TdxGlyphNames.M, TdxGlyphNames.N,
      TdxGlyphNames.O, TdxGlyphNames.P, TdxGlyphNames.Q, TdxGlyphNames.R,
      TdxGlyphNames.S, TdxGlyphNames.T, TdxGlyphNames.U, TdxGlyphNames.V,
      TdxGlyphNames.W, TdxGlyphNames.X, TdxGlyphNames.Y, TdxGlyphNames.Z,
      TdxGlyphNames.LowerBracketleft, TdxGlyphNames.LowerBackslash, TdxGlyphNames.LowerBracketright,
      TdxGlyphNames.LowerAsciicircum, TdxGlyphNames.LowerUnderscore, TdxGlyphNames.LowerGrave,
      TdxGlyphNames.LowerA, TdxGlyphNames.LowerB, TdxGlyphNames.LowerC,
      TdxGlyphNames.LowerD, TdxGlyphNames.LowerE, TdxGlyphNames.LowerF,
      TdxGlyphNames.LowerG, TdxGlyphNames.LowerH, TdxGlyphNames.LowerI,
      TdxGlyphNames.LowerJ, TdxGlyphNames.LowerK, TdxGlyphNames.LowerL,
      TdxGlyphNames.LowerM, TdxGlyphNames.LowerN, TdxGlyphNames.LowerO,
      TdxGlyphNames.LowerP, TdxGlyphNames.LowerQ, TdxGlyphNames.LowerR,
      TdxGlyphNames.LowerS, TdxGlyphNames.LowerT, TdxGlyphNames.LowerU,
      TdxGlyphNames.LowerV, TdxGlyphNames.LowerW, TdxGlyphNames.LowerX,
      TdxGlyphNames.LowerY, TdxGlyphNames.LowerZ, TdxGlyphNames.LowerBraceleft,
      TdxGlyphNames.LowerBar, TdxGlyphNames.LowerBraceright, TdxGlyphNames.LowerAsciitilde,
      TdxGlyphNames.Adieresis, TdxGlyphNames.Aring, TdxGlyphNames.Ccedilla,
      TdxGlyphNames.Eacute, TdxGlyphNames.Ntilde, TdxGlyphNames.Odieresis,
      TdxGlyphNames.Udieresis, TdxGlyphNames.LowerAacute, TdxGlyphNames.LowerAgrave,
      TdxGlyphNames.LowerAcircumflex, TdxGlyphNames.LowerAdieresis, TdxGlyphNames.LowerAtilde,
      TdxGlyphNames.LowerAring, TdxGlyphNames.LowerCcedilla, TdxGlyphNames.LowerEacute,
      TdxGlyphNames.LowerEgrave, TdxGlyphNames.LowerEcircumflex, TdxGlyphNames.LowerEdieresis,
      TdxGlyphNames.LowerIacute, TdxGlyphNames.LowerIgrave, TdxGlyphNames.LowerIcircumflex,
      TdxGlyphNames.LowerIdieresis, TdxGlyphNames.LowerNtilde, TdxGlyphNames.LowerOacute,
      TdxGlyphNames.LowerOgrave, TdxGlyphNames.LowerOcircumflex, TdxGlyphNames.LowerOdieresis,
      TdxGlyphNames.LowerOtilde, TdxGlyphNames.LowerUacute, TdxGlyphNames.LowerUgrave,
      TdxGlyphNames.LowerUcircumflex, TdxGlyphNames.LowerUdieresis, TdxGlyphNames.LowerDagger,
      TdxGlyphNames.LowerDegree, TdxGlyphNames.LowerCent, TdxGlyphNames.LowerSterling,
      TdxGlyphNames.LowerSection, TdxGlyphNames.LowerBullet, TdxGlyphNames.LowerParagraph,
      TdxGlyphNames.LowerGermandbls, TdxGlyphNames.LowerRegistered, TdxGlyphNames.LowerCopyright,
      TdxGlyphNames.LowerTrademark, TdxGlyphNames.LowerAcute, TdxGlyphNames.LowerDieresis,
      TdxGlyphNames.LowerNotequal, TdxGlyphNames.AE, TdxGlyphNames.Oslash,
      TdxGlyphNames.LowerInfinity, TdxGlyphNames.LowerPlusminus, TdxGlyphNames.LowerLessequal,
      TdxGlyphNames.LowerGreaterequal, TdxGlyphNames.LowerYen, TdxGlyphNames.LowerMu,
      TdxGlyphNames.LowerPartialdiff, TdxGlyphNames.LowerSummation, TdxGlyphNames.LowerProduct,
      TdxGlyphNames.LowerPi, TdxGlyphNames.LowerIntegral, TdxGlyphNames.LowerOrdfeminine,
      TdxGlyphNames.LowerOrdmasculine, TdxGlyphNames.Omega, TdxGlyphNames.LowerAe,
      TdxGlyphNames.LowerOslash, TdxGlyphNames.LowerQuestiondown, TdxGlyphNames.LowerExclamdown,
      TdxGlyphNames.LowerLogicalnot, TdxGlyphNames.LowerRadical, TdxGlyphNames.LowerFlorin,
      TdxGlyphNames.LowerApproxequal, TdxGlyphNames.LowerIncrement, TdxGlyphNames.LowerGuillemotleft,
      TdxGlyphNames.LowerGuillemotright, TdxGlyphNames.LowerEllipsis, TdxGlyphNames.LowerNonbreakingspace,
      TdxGlyphNames.Agrave, TdxGlyphNames.Atilde, TdxGlyphNames.Otilde,
      TdxGlyphNames.OE, TdxGlyphNames.LowerOe, TdxGlyphNames.LowerEndash,
      TdxGlyphNames.LowerEmdash, TdxGlyphNames.LowerQuotedblleft, TdxGlyphNames.LowerQuotedblright,
      TdxGlyphNames.LowerQuoteleft, TdxGlyphNames.LowerQuoteright, TdxGlyphNames.LowerDivide,
      TdxGlyphNames.LowerLozenge, TdxGlyphNames.LowerYdieresis, TdxGlyphNames.Ydieresis,
      TdxGlyphNames.LowerFraction, TdxGlyphNames.LowerCurrency, TdxGlyphNames.LowerGuilsinglleft,
      TdxGlyphNames.LowerGuilsinglright, TdxGlyphNames.LowerFi, TdxGlyphNames.LowerFl,
      TdxGlyphNames.LowerDaggerdbl, TdxGlyphNames.LowerPeriodcentered, TdxGlyphNames.LowerQuotesinglbase,
      TdxGlyphNames.LowerQuotedblbase, TdxGlyphNames.LowerPerthousand, TdxGlyphNames.Acircumflex,
      TdxGlyphNames.Ecircumflex, TdxGlyphNames.Aacute, TdxGlyphNames.Edieresis,
      TdxGlyphNames.Egrave, TdxGlyphNames.Iacute, TdxGlyphNames.Icircumflex,
      TdxGlyphNames.Idieresis, TdxGlyphNames.Igrave, TdxGlyphNames.Oacute,
      TdxGlyphNames.Ocircumflex, TdxGlyphNames.LowerApple, TdxGlyphNames.Ograve,
      TdxGlyphNames.Uacute, TdxGlyphNames.Ucircumflex, TdxGlyphNames.Ugrave,
      TdxGlyphNames.LowerDotlessi, TdxGlyphNames.LowerCircumflex, TdxGlyphNames.LowerTilde,
      TdxGlyphNames.LowerMacron, TdxGlyphNames.LowerBreve, TdxGlyphNames.LowerDotaccent,
      TdxGlyphNames.LowerRing, TdxGlyphNames.LowerCedilla, TdxGlyphNames.LowerHungarumlaut,
      TdxGlyphNames.LowerOgonek, TdxGlyphNames.LowerCaron, TdxGlyphNames.Lslash,
      TdxGlyphNames.LowerLslash, TdxGlyphNames.Scaron, TdxGlyphNames.LowerScaron,
      TdxGlyphNames.Zcaron, TdxGlyphNames.LowerZcaron, TdxGlyphNames.LowerBrokenbar,
      TdxGlyphNames.Eth, TdxGlyphNames.LowerEth, TdxGlyphNames.Yacute,
      TdxGlyphNames.LowerYacute, TdxGlyphNames.Thorn, TdxGlyphNames.LowerThorn,
      TdxGlyphNames.LowerMinus, TdxGlyphNames.LowerMultiply, TdxGlyphNames.LowerOnesuperior,
      TdxGlyphNames.LowerTwosuperior, TdxGlyphNames.LowerThreesuperior, TdxGlyphNames.LowerOnehalf,
      TdxGlyphNames.LowerOnequarter, TdxGlyphNames.LowerThreequarters, TdxGlyphNames.LowerFranc,
      TdxGlyphNames.Gbreve, TdxGlyphNames.LowerGbreve, TdxGlyphNames.Idotaccent,
      TdxGlyphNames.Scedilla, TdxGlyphNames.LowerScedilla, TdxGlyphNames.Cacute,
      TdxGlyphNames.LowerCacute, TdxGlyphNames.Ccaron, TdxGlyphNames.LowerCcaron,
      TdxGlyphNames.LowerDcroat);
  protected
    FItalicAngle: Single;
    FUnderlinePosition: SmallInt;
    FGlyphNames: TStringList;
  public const
    FontIsMonospaced = 1;
    FontIsProportionallySpaced = 0;
    MaxMemType1 = 0;
    MaxMemType42 = 0;
    MinMemType1 = 0;
    MinMemType42 = 0;
    HeaderSize = 32;
    Version = $00030000;
  public
    constructor Create(const AData: TBytes); override;
    destructor Destroy; override;
    class function Tag: string; override;

    property ItalicAngle: Single read FItalicAngle;
    property GlyphNames: TStringList read FGlyphNames;
    property UnderlinePosition: SmallInt read FUnderlinePosition;
  end;

  { TdxFontFileOS2Table }

  TdxFontFileOS2Table = class(TdxFontFileBinaryTable)
  strict private const
    BreakChar = 32;
    DefaultChar = 0;
    MaxContext = 0;
  strict private
    function GetIsSymbolic: Boolean;
    function GetUseTypoMetrics: Boolean;
    procedure SetWinAscent(const AValue: SmallInt);
    procedure SetWinDescent(const AValue: SmallInt);
  protected
    FAvgCharWidth: SmallInt;
    FBreakChar: SmallInt;
    FCapHeight: SmallInt;
    FCodePageRange1: TdxFontFileCodePageRange1;
    FCodePageRange2: TdxFontFileCodePageRange2;
    FDefaultChar: SmallInt;
    FEmbeddingType: TdxFontFileOS2EmbeddingType;
    FFamilyClass: TdxFontFileOS2FamilyClass;
    FFirstCharIndex: Integer;
    FLastCharIndex: Integer;
    FMaxContext: SmallInt;
    FPanose: TdxFontFilePanose;
    FSelection: TdxFontFileSelection;
    FStrikeoutPosition: SmallInt;
    FStrikeoutSize: SmallInt;
    FSubscriptXOffset: SmallInt;
    FSubscriptXSize: SmallInt;
    FSubscriptYOffset: SmallInt;
    FSubscriptYSize: SmallInt;
    FSuperscriptXOffset: SmallInt;
    FSuperscriptXSize: SmallInt;
    FSuperscriptYOffset: SmallInt;
    FSuperscriptYSize: SmallInt;
    FTypoAscender: SmallInt;
    FTypoDescender: SmallInt;
    FTypoLineGap: SmallInt;
    FUnicodeRange1: TdxFontFileUnicodeRange1;
    FUnicodeRange2: TdxFontFileUnicodeRange2;
    FUnicodeRange3: TdxFontFileUnicodeRange3;
    FUnicodeRange4: TdxFontFileUnicodeRange4;
    FVendor: string;
    FVersion: TdxFontFileVersion;
    FWeightClass: SmallInt;
    FWidthClass: TdxFontFileOS2WidthClass;
    FWinAscent: SmallInt;
    FWinDescent: SmallInt;
    FXHeight: SmallInt;

    procedure DoApplyChanges; override;
  public const
    BoldFontWeight = 700;
    NormalFontWeight = 400;
  public
    constructor Create(const AData: TBytes); overload; override;

    class function Tag: string; override;

    property IsSymbolic: Boolean read GetIsSymbolic;
    property WinAscent: SmallInt read FWinAscent write SetWinAscent;
    property WinDescent: SmallInt read FWinDescent write SetWinDescent;
    property Panose: TdxFontFilePanose read FPanose;
    property TypoAscender: SmallInt read FTypoAscender;
    property TypoDescender: SmallInt read FTypoDescender;
    property TypoLineGap: SmallInt read FTypoLineGap;
    property UseTypoMetrics: Boolean read GetUseTypoMetrics;
  end;

  { TdxFontFileMaxpTable }

  TdxFontFileMaxpTable = class(TdxFontFileBinaryTable)
  strict private const
    NumGlyphsOffset = 4;
  strict private
    function GetNumGlyphs: Integer;
    procedure SetNumGlyphs(const AValue: Integer);
  public
    constructor Create(AGlyphCount: Integer);
    class function Tag: string; override;

    property NumGlyphs: Integer read GetNumGlyphs write SetNumGlyphs;
  end;

  { TdxFontFileKernTable }

  TdxFontFileKernTable = class(TdxFontFileBinaryTable)
  strict private
    FKerning: TDictionary<Integer, SmallInt>;
  public
    constructor Create(const AData: TBytes); override;
    destructor Destroy; override;
    class function Tag: string; override;

    function GetKerning(AGlyphIndex1: Integer; AGlyphIndex2: Integer): SmallInt;
  end;

  { TdxFontFileHmtxTable }

  TdxFontFileHmtxTable = class(TdxFontFileBinaryTable)
  strict private
    FAdvanceWidths: TSmallIntDynArray;
  public
    constructor Create(AGlyphCount: Integer);

    class function Tag: string; override;
    function FillAdvanceWidths(AHMetricCount, AGlyphCount: Integer): TSmallIntDynArray;

    property AdvanceWidths: TSmallIntDynArray read FAdvanceWidths write FAdvanceWidths;
  end;

  { TdxFontFileHheaTable }

  TdxFontFileHheaTable = class(TdxFontFileBinaryTable)
  protected
    FAdvanceWidthMax: SmallInt;
    FAscender: SmallInt;
    FCaretSlopeRise: SmallInt;
    FCaretSlopeRun: SmallInt;
    FDescender: SmallInt;
    FLineGap: SmallInt;
    FMetricDataFormat: SmallInt;
    FMinLeftSideBearing: SmallInt;
    FMinRightSideBearing: SmallInt;
    FNumberOfHMetrics: Integer;
    FVersion: Integer;
    FXMaxExtent: SmallInt;

    procedure DoApplyChanges; override;
  public
    constructor Create(const AData: TBytes); override;
    class function Tag: string; override;

    procedure Validate;

    property AdvanceWidthMax: SmallInt read FAdvanceWidthMax;
    property Ascender: SmallInt read FAscender;
    property CaretSlopeRise: SmallInt read FCaretSlopeRise;
    property CaretSlopeRun: SmallInt read FCaretSlopeRun;
    property Descender: SmallInt read FDescender;
    property LineGap: SmallInt read FLineGap;
    property MetricDataFormat: SmallInt read FMetricDataFormat;
    property MinLeftSideBearing: SmallInt read FMinLeftSideBearing;
    property MinRightSideBearing: SmallInt read FMinRightSideBearing;
    property NumberOfHMetrics: Integer read FNumberOfHMetrics;
    property Version: Integer read FVersion;
    property XMaxExtent: SmallInt read FXMaxExtent;
  end;

  { TdxFontFileHeadTable }

  TdxFontFileHeadTable = class(TdxFontFileBinaryTable)
  strict private const
    DefaultUnitsPerEm = 2048;
  protected
    FCheckSumAdjustment: Integer;
    FCreated: Int64;
    FFlags: TdxFontFileHeadTableFlags;
    FFontDirectionHint: TdxFontFileDirectionHint;
    FFontRevision: Integer;
    FGlyphDataFormat: SmallInt;
    FIndexToLocFormat: TdxFontFileIndexToLocFormat;
    FLowestRecPPEM: SmallInt;
    FMacStyle: TdxFontFileHeadTableMacStyle;
    FMagicNumber: Integer;
    FModified: Int64;
    FUnitsPerEm: SmallInt;
    FVersion: Integer;
    FXMax: SmallInt;
    FXMin: SmallInt;
    FYMax: SmallInt;
    FYMin: SmallInt;

    procedure DoApplyChanges; override;
  public
    constructor Create(const AData: TBytes); override;
    class function Tag: string; override;

    property CheckSumAdjustment: Integer read FCheckSumAdjustment;
    property Created: Int64 read FCreated;
    property Flags: TdxFontFileHeadTableFlags read FFlags;
    property FontDirectionHint: TdxFontFileDirectionHint read FFontDirectionHint;
    property FontRevision: Integer read FFontRevision;
    property GlyphDataFormat: SmallInt read FGlyphDataFormat;
    property IndexToLocFormat: TdxFontFileIndexToLocFormat read FIndexToLocFormat;
    property LowestRecPPEM: SmallInt read FLowestRecPPEM;
    property MacStyle: TdxFontFileHeadTableMacStyle read FMacStyle;
    property MagicNumber: Integer read FMagicNumber;
    property Modified: Int64 read FModified;
    property UnitsPerEm: SmallInt read FUnitsPerEm;
    property Version: Integer read FVersion;
    property XMax: SmallInt read FXMax;
    property XMin: SmallInt read FXMin;
    property YMax: SmallInt read FYMax;
    property YMin: SmallInt read FYMin;
  end;

  { TdxFontFileCMapCustomFormatRecord }

  TdxFontFileCMapCustomFormatRecord = class
  private const
    MissingGlyphIndex = 0;
  strict private
    FEncodingId: TdxFontFileEncodingID;
    FLanguage: Integer;
    FPlatformId: TdxFontFilePlatformID;
  protected
    function GetFormat: TdxFontFileCMapFormatID; virtual; abstract;
    function GetSize: Integer; virtual; abstract;
    procedure UpdateEncoding(var AEncoding: TSmallIntDynArray); virtual;
    procedure Write(AStream: TdxFontFileStream); virtual;

    class procedure UpdateEncodingValue(const AEncoding: TSmallIntDynArray; AIndex, AValue: SmallInt);

    property Language: Integer read FLanguage write FLanguage;
    property Format: TdxFontFileCMapFormatID read GetFormat;
  public const
    NotdefGlyphIndex = 0;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID); overload;
    constructor Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); overload; virtual;

    function MapCode(ACharacter: Char): Integer; virtual;

    property EncodingId: TdxFontFileEncodingID read FEncodingId;
    property PlatformId: TdxFontFilePlatformID read FPlatformId;
    property Size: Integer read GetSize;
  end;

  { TdxFontFileCMapShortFormatRecord }

  TdxFontFileCMapShortFormatRecord = class(TdxFontFileCMapCustomFormatRecord)
  private const
    HeaderLength = 6;
  strict private
    FBodyLength: Integer;
  protected
    function GetSize: Integer; override;
    procedure Write(ATableStream: TdxFontFileStream); override;

    property BodyLength: Integer read FBodyLength;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; ALanguage: SmallInt); overload; virtual;
    constructor Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); overload; override;
  end;

  { TdxFontFileCMapTrimmedMappingRecord }

  TdxFontFileCMapTrimmedMappingRecord = class(TdxFontFileCMapShortFormatRecord)
  strict private
    FFirstCode: SmallInt;
    FEntryCount: SmallInt;
    FGlyphIdArray: TSmallIntDynArray;
  protected
    function GetFormat: TdxFontFileCMapFormatID; override;
    function GetSize: Integer; override;
    procedure UpdateEncoding(var AEncoding: TSmallIntDynArray); override;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); override;

    function MapCode(ACharacter: Char): Integer; override;
    procedure Write(ATableStream: TdxFontFileStream); override;

    property EntryCount: SmallInt read FEntryCount;
    property FirstCode: SmallInt read FFirstCode;
    property GlyphIdArray: TSmallIntDynArray read FGlyphIdArray;
  end;

  { TdxFontFileCMapByteEncodingRecord }

  TdxFontFileCMapByteEncodingRecord = class(TdxFontFileCMapShortFormatRecord)
  strict private
    FGlyphIdArray: TBytes;
  protected
    function GetFormat: TdxFontFileCMapFormatID; override;
    procedure UpdateEncoding(var AEncoding: TSmallIntDynArray); override;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); override;

    function MapCode(ACharacter: Char): Integer; override;
    procedure Write(ATableStream: TdxFontFileStream); override;

    property GlyphIdArray: TBytes read FGlyphIdArray;
  end;

  { TdxFontFileCMapHighByteMappingThroughSubHeader }

  TdxFontFileCMapHighByteMappingThroughSubHeader = class
  strict private
    FEntryCount: SmallInt;
    FFirstCode: SmallInt;
    FGlyphOffset: Integer;
    FIdDelta: SmallInt;
    FIdRangeOffset: SmallInt;
  public
    constructor Create(AFirstCode, AEntryCount, AIdDelta, AIdRangeOffset: SmallInt; AGlyphOffset: Integer);
    function CalcGlyphIndexArraySize(AOffset: Integer): Integer;

    property EntryCount: SmallInt read FEntryCount;
    property FirstCode: SmallInt read FFirstCode;
    property GlyphOffset: Integer read FGlyphOffset;
    property IdDelta: SmallInt read FIdDelta;
    property IdRangeOffset: SmallInt read FIdRangeOffset;
  end;

  { TdxFontFileCMapHighByteMappingThroughRecord }

  TdxFontFileCMapHighByteMappingThroughRecord = class(TdxFontFileCMapShortFormatRecord)
  strict private const
    SubHeaderKeysLength = 512;
    SubHeaderLength = 8;
  strict private
    FGlyphIndexArray: TSmallIntDynArray;
    FSubHeaderKeys: TSmallIntDynArray;
    FSubHeaders: TArray<TdxFontFileCMapHighByteMappingThroughSubHeader>;
  protected
    function GetFormat: TdxFontFileCMapFormatID; override;
    function GetSize: Integer; override;
    procedure UpdateEncoding(var AEncoding: TSmallIntDynArray); override;

    function ReadSubHeader(AStream: TdxFontFileStream; AEndOfSubheadersPosition: Integer): TdxFontFileCMapHighByteMappingThroughSubHeader;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); override;
    destructor Destroy; override;

    function MapCode(ACharacter: Char): Integer; override;
    procedure Write(ATableStream: TdxFontFileStream); override;

    property GlyphIndexArray: TSmallIntDynArray read FGlyphIndexArray;
    property SubHeaderKeys: TSmallIntDynArray read FSubHeaderKeys;
    property SubHeaders: TArray<TdxFontFileCMapHighByteMappingThroughSubHeader> read FSubHeaders;
  end;

  { TdxFontFileCMapSegmentMappingRecord }

  TdxFontFileCMapSegmentMappingRecord = class(TdxFontFileCMapShortFormatRecord)
  strict private const
    FinalCode = -1;
    FinalDelta = 1;
    UndefinedEncodingMicrosoftOffset = $F000;
  strict private type
    TMap = record
      CharCode: Char;
      GID: SmallInt;
      class function Create(ACharCode: Char; AGID: SmallInt): TMap; static;
    end;
  strict private
    FEndCode: TSmallIntDynArray;
    FGlyphIdArray: TSmallIntDynArray;
    FGlyphRanges: TList<TdxFontFileCMapGlyphRange>;
    FIdDelta: TSmallIntDynArray;
    FIdRangeOffset: TSmallIntDynArray;
    FSegCount: Integer;
    FSegmentOffsets: TIntegerDynArray;
    FStartCode: TSmallIntDynArray;
    function GetSegmentsLength: Integer;
    function IsUndefinedEncoding: Boolean;
    function ReadSegmentsArray(ACMapStream: TdxFontFileStream): TSmallIntDynArray;
  protected
    function GetFormat: TdxFontFileCMapFormatID; override;
    function GetSize: Integer; override;
    procedure UpdateEncoding(var AEncoding: TSmallIntDynArray); override;

    property SegmentsLength: Integer read GetSegmentsLength;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); override;
    constructor CreateDefault(AEncodingID: TdxFontFileEncodingID);
    constructor CreateFromCharset(ACharset: TDictionary<SmallInt, SmallInt>);
    constructor CreateFromTrimmedMapping(AEncodingID: TdxFontFileEncodingID; AFormatEntry: TdxFontFileCMapTrimmedMappingRecord);
    constructor CreateFromByteEncoding(AEncodingID: TdxFontFileEncodingID; AFormatEntry: TdxFontFileCMapByteEncodingRecord);
    constructor CreateFromSegmentMapping(AEncodingID: TdxFontFileEncodingID; AFormatEntry: TdxFontFileCMapSegmentMappingRecord);
    destructor Destroy; override;

    function MapCode(ACharacter: Char): Integer; override;
    procedure Write(ATableStream: TdxFontFileStream); override;

    function CreateGlyphMapping(AGlyphNames: TStringList): TdxPDFWordDictionary;
    function GetGlyphRanges: TList<TdxFontFileCMapGlyphRange>;
    function Validate: Boolean;

    property EndCode: TSmallIntDynArray read FEndCode;
    property GlyphIdArray: TSmallIntDynArray read FGlyphIdArray;
    property GlyphRanges: TList<TdxFontFileCMapGlyphRange> read GetGlyphRanges;
    property IdDelta: TSmallIntDynArray read FIdDelta;
    property IdRangeOffset: TSmallIntDynArray read FIdRangeOffset;
    property SegCount: Integer read FSegCount;
    property StartCode: TSmallIntDynArray read FStartCode;
  end;

  { TdxFontFileCMapLongRecord }

  TdxFontFileCMapLongRecord = class(TdxFontFileCMapCustomFormatRecord)
  protected
    procedure Write(ATableStream: TdxFontFileStream); override;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); override;
  end;

  { TdxFontFileCMapGroup }

  TdxFontFileCMapGroup = record
  strict private
    FEndCharCode: Integer;
    FGlyphID: Integer;
    FStartCharCode: Integer;
  public
    class function Create(AStartCharCode, AEndCharCode, AGlyphID: Integer): TdxFontFileCMapGroup; static;
    class function ReadGroups(AStream: TdxFontFileStream; AGroupsCount: Integer): TArray<TdxFontFileCMapGroup>; static;
    class procedure WriteGroups(const AGroups: TArray<TdxFontFileCMapGroup>; ATableStream: TdxFontFileStream); static;

    property EndCharCode: Integer read FEndCharCode;
    property GlyphID: Integer read FGlyphID;
    property StartCharCode: Integer read FStartCharCode;
  end;

  { TdxFontFileCMapRangeMappingRecord }

  TdxFontFileCMapRangeMappingRecord = class(TdxFontFileCMapLongRecord)
  strict private const
    HeaderLength = 16;
  strict private
    FGroups: TArray<TdxFontFileCMapGroup>;
  protected
    function GetSize: Integer; override;
    procedure Write(ATableStream: TdxFontFileStream); override;

    property Groups: TArray<TdxFontFileCMapGroup> read FGroups;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); override;
  end;

  { TdxFontFileCMapSegmentedCoverageRecord }

  TdxFontFileCMapSegmentedCoverageRecord = class(TdxFontFileCMapRangeMappingRecord)
  protected
    function GetFormat: TdxFontFileCMapFormatID; override;
  end;

  { TdxFontFileCMapManyToOneRangeMappingRecord }

  TdxFontFileCMapManyToOneRangeMappingRecord = class(TdxFontFileCMapRangeMappingRecord)
  protected
    function GetFormat: TdxFontFileCMapFormatID; override;
  end;

  { TdxFontFileCMapMixedCoverageRecord }

  TdxFontFileCMapMixedCoverageRecord = class(TdxFontFileCMapLongRecord)
  strict private const
    HeaderLength = 8208;
  strict private
    FIs32: TBytes;
    FGroups: TArray<TdxFontFileCMapGroup>;
  protected
    function GetFormat: TdxFontFileCMapFormatID; override;
    function GetSize: Integer; override;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); override;
    procedure Write(ATableStream: TdxFontFileStream); override;

    property Is32: TBytes read FIs32;
    property Groups: TArray<TdxFontFileCMapGroup> read FGroups;
  end;

  { TdxFontFileCMapTrimmedArrayRecord }

  TdxFontFileCMapTrimmedArrayRecord = class(TdxFontFileCMapLongRecord)
  strict private const
    HeaderLength = 20;
  strict private
    FCharacterCount: Integer;
    FGlyphs: TSmallIntDynArray;
    FStartCharacterCode: Integer;
  protected
    function GetFormat: TdxFontFileCMapFormatID; override;
    function GetSize: Integer; override;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); override;
    procedure Write(ATableStream: TdxFontFileStream); override;

    property CharacterCount: Integer read FCharacterCount;
    property Glyphs: TSmallIntDynArray read FGlyphs;
    property StartCharacterCode: Integer read FStartCharacterCode;
  end;

  { TdxDefaultUVSTable }

  TdxDefaultUVSTable = class
  strict private
    FAdditionalCount: Byte;
    FStartUnicodeValue: Integer;
  public
    constructor Create(AStartUnicodeValue: Integer; AAdditionalCount: Byte);
    procedure Write(ATableStream: TdxFontFileStream);

    property AdditionalCount: Byte read FAdditionalCount;
    property StartUnicodeValue: Integer read FStartUnicodeValue;
  end;

  { TdxNonDefaultUVSTable }

  TdxNonDefaultUVSTable = class
  strict private
    FGlyphId: SmallInt;
    FUnicodeValue: Integer;
  public
    constructor Create(AUnicodeValue: Integer; AGlyphId: SmallInt);
    procedure Write(ATableStream: TdxFontFileStream);

    property GlyphId: SmallInt read FGlyphId;
    property UnicodeValue: Integer read FUnicodeValue;
  end;

  { TdxFontFileCMapUnicodeVariationSelectorRecord }

  TdxFontFileCMapUnicodeVariationSelectorRecord = class
  strict private
    FDefaultUVSTables: TArray<TdxDefaultUVSTable>;
    FNonDefaultUVSTables: TArray<TdxNonDefaultUVSTable>;
    FVarSelector: Integer;
  public
    constructor Create(AVarSelector: Integer; const ADefaultUVSTables: TArray<TdxDefaultUVSTable>;
      const ANonDefaultUVSTables: TArray<TdxNonDefaultUVSTable>);
    destructor Destroy; override;

    function Write(ATableStream: TdxFontFileStream; AOffset: Integer): Integer;

    property DefaultUVSTables: TArray<TdxDefaultUVSTable> read FDefaultUVSTables;
    property NonDefaultUVSTables: TArray<TdxNonDefaultUVSTable> read FNonDefaultUVSTables;
    property VarSelector: Integer read FVarSelector;
  end;

  { TdxFontFileCMapUnicodeVariationSequenciesRecord }

  TdxFontFileCMapUnicodeVariationSequenciesRecord = class(TdxFontFileCMapCustomFormatRecord)
  strict private const
    DefaultUVSTableSize = 4;
    HeaderLength = 10;
    NonDefaultUVSTableSize = 5;
    VariationSelectorRecordSize = 11;
  strict private
    FDefaultUVSTableSize: Integer;
    FHeaderLength: Integer;
    FNonDefaultUVSTableSize: Integer;
    FVariationSelectorRecords: TArray<TdxFontFileCMapUnicodeVariationSelectorRecord>;
    FVariationSelectorRecordSize: Integer;

    class function GetInt24(const AArray: TBytes): Integer; static;
  protected
    function GetFormat: TdxFontFileCMapFormatID; override;
    function GetSize: Integer; override;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); override;
    destructor Destroy; override;

    procedure Write(ATableStream: TdxFontFileStream); override;

    property VariationSelectorRecords: TArray<TdxFontFileCMapUnicodeVariationSelectorRecord> read FVariationSelectorRecords;
  end;

  { TdxFontFileCMapTable }

  TdxFontFileCMapTable = class(TdxFontFileBinaryTable)
  strict private type
    TChooseTableFunc = reference to function(ATable: TdxFontFileCMapCustomFormatRecord): Boolean;
  strict private
    FCMapTables: TObjectList<TdxFontFileCMapCustomFormatRecord>;
    FMappedGlyphCache: TDictionary<Integer, Integer>;
    FVersion: SmallInt;
    function CreateRecord(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID;
      AFormat: TdxFontFileCMapFormatID; AStream: TdxFontFileStream): TdxFontFileCMapCustomFormatRecord;
    procedure UpdateEncoding(var AEncoding: TSmallIntDynArray; AChooseTableFunc: TChooseTableFunc);
  protected
    procedure DoApplyChanges; override;
  public
    constructor Create(const AData: TBytes); overload; override;
    constructor Create(ASegmentMappingFormatEntry: TdxFontFileCMapSegmentMappingRecord); overload;
    constructor CreateFromCharset(ACharset: TDictionary<SmallInt, Smallint>); overload;
    destructor Destroy; override;

    class function Tag: string; override;

    function Validate(ASkipEncodingValidation: Boolean; AIsSymbolic: Boolean): TdxFontFileCMapSegmentMappingRecord;
    function MapCodes(const AStr: string): TIntegerDynArray;
    function MapCode(ACharacter: Char): Integer;
    procedure PopulateEncoding(var AEncoding: TSmallIntDynArray);

    property CMapTables: TObjectList<TdxFontFileCMapCustomFormatRecord> read FCMapTables;
  end;

  { TdxFontFileNameRecord }

  TdxFontFileNameRecord = record
  strict private
    FEncodingID: TdxFontFileEncodingID;
    FLanguageID: TdxFontFileLanguageID;
    FName: string;
    FNameBytes: TBytes;
    FNameID: TdxFontFileNameID;
    FPlatformID: TdxFontFilePlatformID;
  public
    class function Create(APlatformID: TdxFontFilePlatformID; ALanguageID: TdxFontFileLanguageID; ANameID: TdxFontFileNameID;
      AEncodingID: TdxFontFileEncodingID; const ANameBytes: TBytes): TdxFontFileNameRecord; overload; static;
    class function Create(AStream: TdxFontFileStream; ADataOffset: Integer): TdxFontFileNameRecord; overload; static;

    property EncodingID: TdxFontFileEncodingID read FEncodingID;
    property LanguageID: TdxFontFileLanguageID read FLanguageID;
    property Name: string read FName;
    property NameBytes: TBytes read FNameBytes;
    property NameID: TdxFontFileNameID read FNameID;
    property PlatformID: TdxFontFilePlatformID read FPlatformID;
  end;

  { TdxFontFileNameTable }

  TdxFontFileNameTable = class(TdxFontFileBinaryTable)
  strict private const
    MaxNameLength = 31;
    NameFontSubfamily = 'Regular';
    NameVersion = '0.0';
  strict private
    FFamilyName: string;
    FMacFamilyName: string;
    FNamingTable: TList<TdxFontFileNameRecord>;
    FPostScriptName: string;
    function GetFamilyName: string;
    function GetMacFamilyName: string;
    function GetPostScriptName: string;
  protected
    procedure DoApplyChanges; override;
  public
    constructor Create(const AData: TBytes); overload; override;
    constructor Create(ACMapEntry: TdxFontFileCMapTable; const AFontName: string); overload;
    destructor Destroy; override;

    class function Tag: string; override;
    function FindName(APlatform: TdxFontFilePlatformID; AEncoding: TdxFontFileEncodingID;
      ALanguage: TdxFontFileLanguageID; AId: TdxFontFileNameID): string;
    procedure AddName(ACMapEntry: TdxFontFileCMapTable; const AFontName: string);

    property FamilyName: string read GetFamilyName;
    property MacFamilyName: string read GetMacFamilyName;
    property PostScriptName: string read GetPostScriptName;
  end;

  { TdxFontFileGlyphDescription }

  TdxFontFileGlyphDescription = record
  strict private const
    ARG_1_AND_2_ARE_WORDS = 1;
    MORE_COMPONENTS = 32;
    WE_HAVE_A_SCALE = 8;
    WE_HAVE_A_TWO_BY_TWO = 128;
    WE_HAVE_AN_X_AND_Y_SCALE = 64;
  strict private
    function GetSize: Integer;
  public const
    HeaderSize = 10;
  public
    Data: TBytes;
    GlyphIndexList: TIntegerDynArray;
    NumberOfContours: SmallInt;

    class function Create(AStream: TdxFontFileStream; AGlyphDataSize: Integer): TdxFontFileGlyphDescription; static;

    function IsEmpty: Boolean;
    procedure Write(AStream: TdxFontFileStream);
    property Size: Integer read GetSize;
  end;

  TdxFontFileSubsetGlyph = record
    Index: Integer;
    Description: TdxFontFileGlyphDescription;
  end;

  { TdxFontFileGlyphTable }

  TdxFontFileGlyphTable = class(TdxFontFileBinaryTable)
  strict private
    FGlyphs: TDictionary<Integer, TdxFontFileGlyphDescription>;
    FGlyphOffsets: TIntegerDynArray;
    FSubsetGlyphs: TList<TdxFontFileSubsetGlyph>;
    function Pad4(AValue: Integer): Integer;
    procedure SortSubsetGlyphs;
  protected
    procedure DoApplyChanges; override;
  public
    constructor Create(const AData: TBytes); override;
    destructor Destroy; override;

    class function Tag: string; override;
    function CalculateOffsets(AGlyphCount: Integer): TIntegerDynArray;
    procedure CreateSubset(AFontFile: TdxFontFile; AMapping: TDictionary<Integer, string>);
    procedure ReadGlyphs(AFontFile: TdxFontFile);
  end;

  { TdxFontFile }

  TdxFontFile = class
  strict private const
    OpenTypeVersion: array[0.. 3] of Byte = ($4F, $54, $54, $4F);
    TrueTypeFontToFactor = 1000 / 2048;
    TrueTypeVersion: array[0.. 3] of Byte = ($0, $1, $0, $0);
    TableDirectoryOffset = 12;
  strict private
    FCMap: TdxFontFileCMapTable;
    FHhea: TdxFontFileHheaTable;
    FHmtx: TdxFontFileHmtxTable;
    FInitalFontSize: Int64;
    FKern: TdxFontFileKernTable;
    FTableDictionary: TObjectDictionary<string, TdxFontFileBinaryTable>;
    FTTFToFontFileFactor: Single;
    FVersion: TBytes;

    function GetHeadTable: TdxFontFileHeadTable;
    function GetMaxpTable: TdxFontFileMaxpTable;
    function GetOS2Table: TdxFontFileOS2Table;
    function GetPostTable: TdxFontFilePostTable;
    function GetNameTable: TdxFontFileNameTable;
    function GetLocaTable: TdxFontFileLocaTable;
    function GetGlyphTable: TdxFontFileGlyphTable;
    function GetHheaTable: TdxFontFileHheaTable;
    function GetCMapTable: TdxFontFileCMapTable;
    function GetKernTable: TdxFontFileKernTable;
    function GetHmtxTable: TdxFontFileHmtxTable;

    function CreateTable(const ATag: string; const AArray: TBytes): TdxFontFileBinaryTable;
    function InternalGetData(ATablesToWrite: TStringList): TBytes;
    procedure ReadTables(AStream: TdxFontFileStream);
  protected
    property Table: TObjectDictionary<string, TdxFontFileBinaryTable> read FTableDictionary;

    property GlyphTable: TdxFontFileGlyphTable read GetGlyphTable;
    property HmtxTable: TdxFontFileHmtxTable read GetHmtxTable;
    property InitalFontSize: Int64 read FInitalFontSize;
  public
    constructor Create(AStream: TdxFontFileStream); overload;
    constructor Create(const AData: TBytes; AIsOpenType: Boolean = False); overload;
    destructor Destroy; override;

    class function GetCFFData(const AFontFileData: TBytes): TBytes; static;
    class function IsEqual(AFontFile1, AFontFile2: TdxFontFile): Boolean; static;
    function CreateSubset(AMapping: TDictionary<Integer, string>): TBytes;
    function GetCharacterWidth(AGlyphIndex: Integer): Single;
    function GetData: TBytes; overload;
    function IsTrueTypeFont: Boolean;
    procedure AddTable(ATable: TdxFontFileBinaryTable);

    property CMapTable: TdxFontFileCMapTable read GetCMapTable;
    property HeadTable: TdxFontFileHeadTable read GetHeadTable;
    property HheaTable: TdxFontFileHheaTable read GetHheaTable;
    property KernTable: TdxFontFileKernTable read GetKernTable;
    property LocaTable: TdxFontFileLocaTable read GetLocaTable;
    property MaxpTable: TdxFontFileMaxpTable read GetMaxpTable;
    property NameTable: TdxFontFileNameTable read GetNameTable;
    property OS2Table: TdxFontFileOS2Table read GetOS2Table;
    property PostTable: TdxFontFilePostTable read GetPostTable;
  end;

  { TdxFontFileFontMetrics }

  TdxFontFileFontMetrics = record
  strict private
    FAscent: Double;
    FDescent: Double;
    FLineSpacing: Double;
    FEmBBox: TdxRectF;
    function GetEmAscent: Double;
    function GetEmDescent: Double;
  public
    class function Create(AFontFile: TdxFontFile): TdxFontFileFontMetrics; overload; static;
    class function Create(AAscent, ADescent, ALineSpacing, AUnitsPerEm: Double): TdxFontFileFontMetrics; overload; static;

    function GetAscent(AFontSize: Double): Double;
    function GetDescent(AFontSize: Double): Double;
    function GetLineSpacing(AFontSize: Double): Double;

    property EmAscent: Double read GetEmAscent;
    property EmDescent: Double read GetEmDescent;
    property EmBBox: TdxRectF read FEmBBox;
  end;

function dxFontFileUnicodeConverter: TdxFontFileUnicodeConverter;

implementation

uses
  Math, dxTypeHelpers, dxPDFTypes, dxPDFUtils;

type
  TEncodingPair = record
    Count: SmallInt;
    Code: SmallInt;
  end;

  TdxFontFileTableClass = class of TdxFontFileBinaryTable;

var
  dxgFontFileUnicodeConverter: TdxFontFileUnicodeConverter;
  dxgFontFileSupportedTables: TDictionary<string, TdxFontFileTableClass>;

function dxFontFileUnicodeConverter: TdxFontFileUnicodeConverter;
begin
  Result := dxgFontFileUnicodeConverter;
end;

{ TdxFontFilePanose }

class function TdxFontFilePanose.Create(AStream: TdxFontFileStream): TdxFontFilePanose;
var
  AData: TBytes;
begin
  AData := AStream.ReadArray(10);
  Result.FamilyKind := TdxFontFilePanoseFamilyKind(AData[0]);
  Result.SerifStyle := TdxFontFilePanoseSerifStyle(AData[1]);
  Result.Weight := TdxFontFilePanoseWeight(AData[2]);
  Result.Proportion := TdxFontFilePanoseProportion(AData[3]);
  Result.Contrast := TdxFontFilePanoseContrast(AData[4]);
  Result.StrokeVariation := TdxFontFilePanoseStrokeVariation(AData[5]);
  Result.ArmStyle := TdxFontFilePanoseArmStyle(AData[6]);
  Result.LetterForm := TdxFontFilePanoseLetterform(AData[7]);
  Result.Midline := TdxFontFilePanoseMidline(AData[8]);
  Result.XHeight := TdxFontFilePanoseXHeight(AData[9]);
end;

function TdxFontFilePanose.IsDefault: Boolean;
begin
  Result := (FamilyKind = fkAny) and (SerifStyle = ssAny) and (Weight = wAny) and (Proportion = pAny)
    and (Contrast = cAny) and (StrokeVariation = svAny) and (ArmStyle = asAny) and (LetterForm = lfAny)
    and (Midline = mAny) and (XHeight = xhAny);
end;

procedure TdxFontFilePanose.Write(AStream: TdxFontFileStream);
var
  AData: TBytes;
begin
  SetLength(AData, 10);
  AData[0] := Byte(FamilyKind);
  AData[1] := Byte(SerifStyle);
  AData[2] := Byte(Weight);
  AData[3] := Byte(Proportion);
  AData[4] := Byte(Contrast);
  AData[5] := Byte(StrokeVariation);
  AData[6] := Byte(ArmStyle);
  AData[7] := Byte(LetterForm);
  AData[8] := Byte(Midline);
  AData[9] := Byte(XHeight);
  AStream.WriteArray(AData);
end;

{ TdxType1FontGlyphMapping }

constructor TdxType1FontGlyphMapping.Create;
begin
  inherited Create;
end;

constructor TdxType1FontGlyphMapping.Create(AMapping: TDictionary<string, SmallInt>;
  AEncoding: TDictionary<Byte, string>);
var
  AMappingPair: TPair<string, SmallInt>;
  AEncodingPair: TPair<Byte, string>;
begin
  Create;
  if  AMapping <> nil then
  begin
    FMapping := TDictionary<string, SmallInt>.Create;
    for AMappingPair in AMapping do
      FMapping.Add(AMappingPair.Key, AMappingPair.Value);
  end;
  if AEncoding <> nil then
  begin
    FEncoding := TDictionary<Byte, string>.Create;
    for AEncodingPair in AEncoding do
      FEncoding.Add(AEncodingPair.Key, AEncodingPair.Value);
  end;
end;

destructor TdxType1FontGlyphMapping.Destroy;
begin
  FreeAndNil(FMapping);
  FreeAndNil(FEncoding);
  inherited Destroy;
end;

{ TdxFontFileCustomEncoding }

constructor TdxFontFileCustomEncoding.Create;
begin
  inherited Create;
  FDictionary := TDictionary<Byte, string>.Create;
  Initialize;
end;

destructor TdxFontFileCustomEncoding.Destroy;
begin
  FreeAndNil(FDictionary);
  inherited Destroy;
end;

procedure TdxFontFileCustomEncoding.Initialize;
begin
// do nothing;
end;

{ TdxFontFileUnicodeConverter }

constructor TdxFontFileUnicodeConverter.Create;
begin
  inherited Create;
  FGlyphCodes := TdxPDFWordDictionary.Create;
  FAdobeGlyphNames := TdxPDFWordDictionary.Create;
  Initialize;
end;

destructor TdxFontFileUnicodeConverter.Destroy;
begin
  FreeAndNil(FAdobeGlyphNames);
  FreeAndNil(FGlyphCodes);
  inherited Destroy;
end;

function TdxFontFileUnicodeConverter.FindCode(const AName: string; out ACode: Word): Boolean;
begin
  Result := FGlyphCodes.TryGetValue(AName, ACode);
  if not Result then
    Result := FAdobeGlyphNames.TryGetValue(AName, ACode);
end;

function TdxFontFileUnicodeConverter.FindCode(AEncoding: TdxFontFileCustomEncoding; ACode: Word; out AResult: Word): Boolean;
begin
  Result := InternalFindCode(ACode, AEncoding, FGlyphCodes, AResult);
end;

function TdxFontFileUnicodeConverter.FindCode(AEncoding: TdxFontFileCustomEncoding; AGlyphCodes: TdxPDFWordDictionary;
  ACode: Word; out AResult: Word): Boolean;
begin
  Result := InternalFindCode(ACode, AEncoding, AGlyphCodes, AResult);
end;

function TdxFontFileUnicodeConverter.InternalFindCode(ACode: Word; AEncoding: TdxFontFileCustomEncoding;
  AGlyphCodes: TdxPDFWordDictionary; out AResult: Word): Boolean;
var
  AName: string;
begin
  if not AEncoding.Dictionary.TryGetValue(ACode, AName) then
    AName := TdxGlyphNames._notdef;
  Result := AGlyphCodes.TryGetValue(AName, AResult);
  if not Result and (AName = TdxGlyphNames.Zdotaccent) then
  begin
      Result := AGlyphCodes.TryGetValue(TdxGlyphNames.Zdot, AResult);
      if not Result and (AName = TdxGlyphNames.LowerZdotaccent) then
      begin
        Result := AGlyphCodes.TryGetValue(TdxGlyphNames.Zdot, AResult);
        if not Result then
          AResult := ACode;
      end;
  end;
end;

procedure TdxFontFileUnicodeConverter.Initialize;
begin
  InitializeGlyphCodes;
  InitializeAdobeGlyphNames;
end;

procedure TdxFontFileUnicodeConverter.InitializeAdobeGlyphNames;
begin
  InitializeAdobeGlyphNames1;
  InitializeAdobeGlyphNames2;
  InitializeAdobeGlyphNames3;
  InitializeAdobeGlyphNames4;
  FAdobeGlyphNames.TrimExcess;
end;

procedure TdxFontFileUnicodeConverter.InitializeAdobeGlyphNames1;
begin
  FAdobeGlyphNames.Add('a', 97);
  FAdobeGlyphNames.Add('A', 65);
  FAdobeGlyphNames.Add('aabengali', 2438);
  FAdobeGlyphNames.Add('aacute', 225);
  FAdobeGlyphNames.Add('Aacute', 193);
  FAdobeGlyphNames.Add('Aacutesmall', 63457);
  FAdobeGlyphNames.Add('aadeva', 2310);
  FAdobeGlyphNames.Add('aagujarati', 2694);
  FAdobeGlyphNames.Add('aagurmukhi', 2566);
  FAdobeGlyphNames.Add('aamatragurmukhi', 2622);
  FAdobeGlyphNames.Add('aarusquare', 13059);
  FAdobeGlyphNames.Add('aavowelsignbengali', 2494);
  FAdobeGlyphNames.Add('aavowelsigndeva', 2366);
  FAdobeGlyphNames.Add('aavowelsigngujarati', 2750);
  FAdobeGlyphNames.Add('abbreviationmarkarmenian', 1375);
  FAdobeGlyphNames.Add('abbreviationsigndeva', 2416);
  FAdobeGlyphNames.Add('abengali', 2437);
  FAdobeGlyphNames.Add('abopomofo', 12570);
  FAdobeGlyphNames.Add('abreve', 259);
  FAdobeGlyphNames.Add('Abreve', 258);
  FAdobeGlyphNames.Add('abreveacute', 7855);
  FAdobeGlyphNames.Add('Abreveacute', 7854);
  FAdobeGlyphNames.Add('abrevecyrillic', 1233);
  FAdobeGlyphNames.Add('Abrevecyrillic', 1232);
  FAdobeGlyphNames.Add('abrevedotbelow', 7863);
  FAdobeGlyphNames.Add('Abrevedotbelow', 7862);
  FAdobeGlyphNames.Add('abrevegrave', 7857);
  FAdobeGlyphNames.Add('Abrevegrave', 7856);
  FAdobeGlyphNames.Add('abrevehookabove', 7859);
  FAdobeGlyphNames.Add('Abrevehookabove', 7858);
  FAdobeGlyphNames.Add('abrevetilde', 7861);
  FAdobeGlyphNames.Add('Abrevetilde', 7860);
  FAdobeGlyphNames.Add('acaron', 462);
  FAdobeGlyphNames.Add('Acaron', 461);
  FAdobeGlyphNames.Add('acircle', 9424);
  FAdobeGlyphNames.Add('Acircle', 9398);
  FAdobeGlyphNames.Add('acircumflex', 226);
  FAdobeGlyphNames.Add('Acircumflex', 194);
  FAdobeGlyphNames.Add('acircumflexacute', 7845);
  FAdobeGlyphNames.Add('Acircumflexacute', 7844);
  FAdobeGlyphNames.Add('acircumflexdotbelow', 7853);
  FAdobeGlyphNames.Add('Acircumflexdotbelow', 7852);
  FAdobeGlyphNames.Add('acircumflexgrave', 7847);
  FAdobeGlyphNames.Add('Acircumflexgrave', 7846);
  FAdobeGlyphNames.Add('acircumflexhookabove', 7849);
  FAdobeGlyphNames.Add('Acircumflexhookabove', 7848);
  FAdobeGlyphNames.Add('Acircumflexsmall', 63458);
  FAdobeGlyphNames.Add('acircumflextilde', 7851);
  FAdobeGlyphNames.Add('Acircumflextilde', 7850);
  FAdobeGlyphNames.Add('acute', 180);
  FAdobeGlyphNames.Add('Acute', 63177);
  FAdobeGlyphNames.Add('acutebelowcmb', 791);
  FAdobeGlyphNames.Add('acutecmb', 769);
  FAdobeGlyphNames.Add('acutecomb', 769);
  FAdobeGlyphNames.Add('acutedeva', 2388);
  FAdobeGlyphNames.Add('acutelowmod', 719);
  FAdobeGlyphNames.Add('Acutesmall', 63412);
  FAdobeGlyphNames.Add('acutetonecmb', 833);
  FAdobeGlyphNames.Add('acyrillic', 1072);
  FAdobeGlyphNames.Add('Acyrillic', 1040);
  FAdobeGlyphNames.Add('adblgrave', 513);
  FAdobeGlyphNames.Add('Adblgrave', 512);
  FAdobeGlyphNames.Add('addakgurmukhi', 2673);
  FAdobeGlyphNames.Add('adeva', 2309);
  FAdobeGlyphNames.Add('adieresis', 228);
  FAdobeGlyphNames.Add('Adieresis', 196);
  FAdobeGlyphNames.Add('adieresiscyrillic', 1235);
  FAdobeGlyphNames.Add('Adieresiscyrillic', 1234);
  FAdobeGlyphNames.Add('adieresismacron', 479);
  FAdobeGlyphNames.Add('Adieresismacron', 478);
  FAdobeGlyphNames.Add('Adieresissmall', 63460);
  FAdobeGlyphNames.Add('adotbelow', 7841);
  FAdobeGlyphNames.Add('Adotbelow', 7840);
  FAdobeGlyphNames.Add('adotmacron', 481);
  FAdobeGlyphNames.Add('Adotmacron', 480);
  FAdobeGlyphNames.Add('ae', 230);
  FAdobeGlyphNames.Add('AE', 198);
  FAdobeGlyphNames.Add('aeacute', 509);
  FAdobeGlyphNames.Add('AEacute', 508);
  FAdobeGlyphNames.Add('aekorean', 12624);
  FAdobeGlyphNames.Add('aemacron', 483);
  FAdobeGlyphNames.Add('AEmacron', 482);
  FAdobeGlyphNames.Add('AEsmall', 63462);
  FAdobeGlyphNames.Add('afii00208', 8213);
  FAdobeGlyphNames.Add('afii08941', 8356);
  FAdobeGlyphNames.Add('afii10017', 1040);
  FAdobeGlyphNames.Add('afii10018', 1041);
  FAdobeGlyphNames.Add('afii10019', 1042);
  FAdobeGlyphNames.Add('afii10020', 1043);
  FAdobeGlyphNames.Add('afii10021', 1044);
  FAdobeGlyphNames.Add('afii10022', 1045);
  FAdobeGlyphNames.Add('afii10023', 1025);
  FAdobeGlyphNames.Add('afii10024', 1046);
  FAdobeGlyphNames.Add('afii10025', 1047);
  FAdobeGlyphNames.Add('afii10026', 1048);
  FAdobeGlyphNames.Add('afii10027', 1049);
  FAdobeGlyphNames.Add('afii10028', 1050);
  FAdobeGlyphNames.Add('afii10029', 1051);
  FAdobeGlyphNames.Add('afii10030', 1052);
  FAdobeGlyphNames.Add('afii10031', 1053);
  FAdobeGlyphNames.Add('afii10032', 1054);
  FAdobeGlyphNames.Add('afii10033', 1055);
  FAdobeGlyphNames.Add('afii10034', 1056);
  FAdobeGlyphNames.Add('afii10035', 1057);
  FAdobeGlyphNames.Add('afii10036', 1058);
  FAdobeGlyphNames.Add('afii10037', 1059);
  FAdobeGlyphNames.Add('afii10038', 1060);
  FAdobeGlyphNames.Add('afii10039', 1061);
  FAdobeGlyphNames.Add('afii10040', 1062);
  FAdobeGlyphNames.Add('afii10041', 1063);
  FAdobeGlyphNames.Add('afii10042', 1064);
  FAdobeGlyphNames.Add('afii10043', 1065);
  FAdobeGlyphNames.Add('afii10044', 1066);
  FAdobeGlyphNames.Add('afii10045', 1067);
  FAdobeGlyphNames.Add('afii10046', 1068);
  FAdobeGlyphNames.Add('afii10047', 1069);
  FAdobeGlyphNames.Add('afii10048', 1070);
  FAdobeGlyphNames.Add('afii10049', 1071);
  FAdobeGlyphNames.Add('afii10050', 1168);
  FAdobeGlyphNames.Add('afii10051', 1026);
  FAdobeGlyphNames.Add('afii10052', 1027);
  FAdobeGlyphNames.Add('afii10053', 1028);
  FAdobeGlyphNames.Add('afii10054', 1029);
  FAdobeGlyphNames.Add('afii10055', 1030);
  FAdobeGlyphNames.Add('afii10056', 1031);
  FAdobeGlyphNames.Add('afii10057', 1032);
  FAdobeGlyphNames.Add('afii10058', 1033);
  FAdobeGlyphNames.Add('afii10059', 1034);
  FAdobeGlyphNames.Add('afii10060', 1035);
  FAdobeGlyphNames.Add('afii10061', 1036);
  FAdobeGlyphNames.Add('afii10062', 1038);
  FAdobeGlyphNames.Add('afii10063', 63172);
  FAdobeGlyphNames.Add('afii10064', 63173);
  FAdobeGlyphNames.Add('afii10065', 1072);
  FAdobeGlyphNames.Add('afii10066', 1073);
  FAdobeGlyphNames.Add('afii10067', 1074);
  FAdobeGlyphNames.Add('afii10068', 1075);
  FAdobeGlyphNames.Add('afii10069', 1076);
  FAdobeGlyphNames.Add('afii10070', 1077);
  FAdobeGlyphNames.Add('afii10071', 1105);
  FAdobeGlyphNames.Add('afii10072', 1078);
  FAdobeGlyphNames.Add('afii10073', 1079);
  FAdobeGlyphNames.Add('afii10074', 1080);
  FAdobeGlyphNames.Add('afii10075', 1081);
  FAdobeGlyphNames.Add('afii10076', 1082);
  FAdobeGlyphNames.Add('afii10077', 1083);
  FAdobeGlyphNames.Add('afii10078', 1084);
  FAdobeGlyphNames.Add('afii10079', 1085);
  FAdobeGlyphNames.Add('afii10080', 1086);
  FAdobeGlyphNames.Add('afii10081', 1087);
  FAdobeGlyphNames.Add('afii10082', 1088);
  FAdobeGlyphNames.Add('afii10083', 1089);
  FAdobeGlyphNames.Add('afii10084', 1090);
  FAdobeGlyphNames.Add('afii10085', 1091);
  FAdobeGlyphNames.Add('afii10086', 1092);
  FAdobeGlyphNames.Add('afii10087', 1093);
  FAdobeGlyphNames.Add('afii10088', 1094);
  FAdobeGlyphNames.Add('afii10089', 1095);
  FAdobeGlyphNames.Add('afii10090', 1096);
  FAdobeGlyphNames.Add('afii10091', 1097);
  FAdobeGlyphNames.Add('afii10092', 1098);
  FAdobeGlyphNames.Add('afii10093', 1099);
  FAdobeGlyphNames.Add('afii10094', 1100);
  FAdobeGlyphNames.Add('afii10095', 1101);
  FAdobeGlyphNames.Add('afii10096', 1102);
  FAdobeGlyphNames.Add('afii10097', 1103);
  FAdobeGlyphNames.Add('afii10098', 1169);
  FAdobeGlyphNames.Add('afii10099', 1106);
  FAdobeGlyphNames.Add('afii10100', 1107);
  FAdobeGlyphNames.Add('afii10101', 1108);
  FAdobeGlyphNames.Add('afii10102', 1109);
  FAdobeGlyphNames.Add('afii10103', 1110);
  FAdobeGlyphNames.Add('afii10104', 1111);
  FAdobeGlyphNames.Add('afii10105', 1112);
  FAdobeGlyphNames.Add('afii10106', 1113);
  FAdobeGlyphNames.Add('afii10107', 1114);
  FAdobeGlyphNames.Add('afii10108', 1115);
  FAdobeGlyphNames.Add('afii10109', 1116);
  FAdobeGlyphNames.Add('afii10110', 1118);
  FAdobeGlyphNames.Add('afii10145', 1039);
  FAdobeGlyphNames.Add('afii10146', 1122);
  FAdobeGlyphNames.Add('afii10147', 1138);
  FAdobeGlyphNames.Add('afii10148', 1140);
  FAdobeGlyphNames.Add('afii10192', 63174);
  FAdobeGlyphNames.Add('afii10193', 1119);
  FAdobeGlyphNames.Add('afii10194', 1123);
  FAdobeGlyphNames.Add('afii10195', 1139);
  FAdobeGlyphNames.Add('afii10196', 1141);
  FAdobeGlyphNames.Add('afii10831', 63175);
  FAdobeGlyphNames.Add('afii10832', 63176);
  FAdobeGlyphNames.Add('afii10846', 1241);
  FAdobeGlyphNames.Add('afii299', 8206);
  FAdobeGlyphNames.Add('afii300', 8207);
  FAdobeGlyphNames.Add('afii301', 8205);
  FAdobeGlyphNames.Add('afii57381', 1642);
  FAdobeGlyphNames.Add('afii57388', 1548);
  FAdobeGlyphNames.Add('afii57392', 1632);
  FAdobeGlyphNames.Add('afii57393', 1633);
  FAdobeGlyphNames.Add('afii57394', 1634);
  FAdobeGlyphNames.Add('afii57395', 1635);
  FAdobeGlyphNames.Add('afii57396', 1636);
  FAdobeGlyphNames.Add('afii57397', 1637);
  FAdobeGlyphNames.Add('afii57398', 1638);
  FAdobeGlyphNames.Add('afii57399', 1639);
  FAdobeGlyphNames.Add('afii57400', 1640);
  FAdobeGlyphNames.Add('afii57401', 1641);
  FAdobeGlyphNames.Add('afii57403', 1563);
  FAdobeGlyphNames.Add('afii57407', 1567);
  FAdobeGlyphNames.Add('afii57409', 1569);
  FAdobeGlyphNames.Add('afii57410', 1570);
  FAdobeGlyphNames.Add('afii57411', 1571);
  FAdobeGlyphNames.Add('afii57412', 1572);
  FAdobeGlyphNames.Add('afii57413', 1573);
  FAdobeGlyphNames.Add('afii57414', 1574);
  FAdobeGlyphNames.Add('afii57415', 1575);
  FAdobeGlyphNames.Add('afii57416', 1576);
  FAdobeGlyphNames.Add('afii57417', 1577);
  FAdobeGlyphNames.Add('afii57418', 1578);
  FAdobeGlyphNames.Add('afii57419', 1579);
  FAdobeGlyphNames.Add('afii57420', 1580);
  FAdobeGlyphNames.Add('afii57421', 1581);
  FAdobeGlyphNames.Add('afii57422', 1582);
  FAdobeGlyphNames.Add('afii57423', 1583);
  FAdobeGlyphNames.Add('afii57424', 1584);
  FAdobeGlyphNames.Add('afii57425', 1585);
  FAdobeGlyphNames.Add('afii57426', 1586);
  FAdobeGlyphNames.Add('afii57427', 1587);
  FAdobeGlyphNames.Add('afii57428', 1588);
  FAdobeGlyphNames.Add('afii57429', 1589);
  FAdobeGlyphNames.Add('afii57430', 1590);
  FAdobeGlyphNames.Add('afii57431', 1591);
  FAdobeGlyphNames.Add('afii57432', 1592);
  FAdobeGlyphNames.Add('afii57433', 1593);
  FAdobeGlyphNames.Add('afii57434', 1594);
  FAdobeGlyphNames.Add('afii57440', 1600);
  FAdobeGlyphNames.Add('afii57441', 1601);
  FAdobeGlyphNames.Add('afii57442', 1602);
  FAdobeGlyphNames.Add('afii57443', 1603);
  FAdobeGlyphNames.Add('afii57444', 1604);
  FAdobeGlyphNames.Add('afii57445', 1605);
  FAdobeGlyphNames.Add('afii57446', 1606);
  FAdobeGlyphNames.Add('afii57448', 1608);
  FAdobeGlyphNames.Add('afii57449', 1609);
  FAdobeGlyphNames.Add('afii57450', 1610);
  FAdobeGlyphNames.Add('afii57451', 1611);
  FAdobeGlyphNames.Add('afii57452', 1612);
  FAdobeGlyphNames.Add('afii57453', 1613);
  FAdobeGlyphNames.Add('afii57454', 1614);
  FAdobeGlyphNames.Add('afii57455', 1615);
  FAdobeGlyphNames.Add('afii57456',616);
  FAdobeGlyphNames.Add('afii57457', 1617);
  FAdobeGlyphNames.Add('afii57458', 1618);
  FAdobeGlyphNames.Add('afii57470', 1607);
  FAdobeGlyphNames.Add('afii57505', 1700);
  FAdobeGlyphNames.Add('afii57506', 1662);
  FAdobeGlyphNames.Add('afii57507', 1670);
  FAdobeGlyphNames.Add('afii57508', 1688);
  FAdobeGlyphNames.Add('afii57509', 1711);
  FAdobeGlyphNames.Add('afii57511', 1657);
  FAdobeGlyphNames.Add('afii57512', 1672);
  FAdobeGlyphNames.Add('afii57513', 1681);
  FAdobeGlyphNames.Add('afii57514', 1722);
  FAdobeGlyphNames.Add('afii57519', 1746);
  FAdobeGlyphNames.Add('afii57534', 1749);
  FAdobeGlyphNames.Add('afii57636', 8362);
  FAdobeGlyphNames.Add('afii57645', 1470);
  FAdobeGlyphNames.Add('afii57658', 1475);
  FAdobeGlyphNames.Add('afii57664', 1488);
  FAdobeGlyphNames.Add('afii57665', 1489);
  FAdobeGlyphNames.Add('afii57666', 1490);
  FAdobeGlyphNames.Add('afii57667', 1491);
  FAdobeGlyphNames.Add('afii57668', 1492);
  FAdobeGlyphNames.Add('afii57669', 1493);
  FAdobeGlyphNames.Add('afii57670', 1494);
  FAdobeGlyphNames.Add('afii57671', 1495);
  FAdobeGlyphNames.Add('afii57672', 1496);
  FAdobeGlyphNames.Add('afii57673', 1497);
  FAdobeGlyphNames.Add('afii57674', 1498);
  FAdobeGlyphNames.Add('afii57675', 1499);
  FAdobeGlyphNames.Add('afii57676', 1500);
  FAdobeGlyphNames.Add('afii57677', 1501);
  FAdobeGlyphNames.Add('afii57678', 1502);
  FAdobeGlyphNames.Add('afii57679', 1503);
  FAdobeGlyphNames.Add('afii57680', 1504);
  FAdobeGlyphNames.Add('afii57681', 1505);
  FAdobeGlyphNames.Add('afii57682', 1506);
  FAdobeGlyphNames.Add('afii57683', 1507);
  FAdobeGlyphNames.Add('afii57684', 1508);
  FAdobeGlyphNames.Add('afii57685', 1509);
  FAdobeGlyphNames.Add('afii57686', 1510);
  FAdobeGlyphNames.Add('afii57687', 1511);
  FAdobeGlyphNames.Add('afii57688', 1512);
  FAdobeGlyphNames.Add('afii57689', 1513);
  FAdobeGlyphNames.Add('afii57690', 1514);
  FAdobeGlyphNames.Add('afii57694', 64298);
  FAdobeGlyphNames.Add('afii57695', 64299);
  FAdobeGlyphNames.Add('afii57700', 64331);
  FAdobeGlyphNames.Add('afii57705', 64287);
  FAdobeGlyphNames.Add('afii57716', 1520);
  FAdobeGlyphNames.Add('afii57717', 1521);
  FAdobeGlyphNames.Add('afii57718', 1522);
  FAdobeGlyphNames.Add('afii57723', 64309);
  FAdobeGlyphNames.Add('afii57793', 1460);
  FAdobeGlyphNames.Add('afii57794', 1461);
  FAdobeGlyphNames.Add('afii57795', 1462);
  FAdobeGlyphNames.Add('afii57796', 1467);
  FAdobeGlyphNames.Add('afii57797', 1464);
  FAdobeGlyphNames.Add('afii57798', 1463);
  FAdobeGlyphNames.Add('afii57799', 1456);
  FAdobeGlyphNames.Add('afii57800', 1458);
  FAdobeGlyphNames.Add('afii57801', 1457);
  FAdobeGlyphNames.Add('afii57802', 1459);
  FAdobeGlyphNames.Add('afii57803', 1474);
  FAdobeGlyphNames.Add('afii57804', 1473);
  FAdobeGlyphNames.Add('afii57806', 1465);
  FAdobeGlyphNames.Add('afii57807', 1468);
  FAdobeGlyphNames.Add('afii57839', 1469);
  FAdobeGlyphNames.Add('afii57841', 1471);
  FAdobeGlyphNames.Add('afii57842', 1472);
  FAdobeGlyphNames.Add('afii57929', 700);
  FAdobeGlyphNames.Add('afii61248', 8453);
  FAdobeGlyphNames.Add('afii61289', 8467);
  FAdobeGlyphNames.Add('afii61352', 8470);
  FAdobeGlyphNames.Add('afii61573', 8236);
  FAdobeGlyphNames.Add('afii61574', 8237);
  FAdobeGlyphNames.Add('afii61575', 8238);
  FAdobeGlyphNames.Add('afii61664', 8204);
  FAdobeGlyphNames.Add('afii63167', 1645);
  FAdobeGlyphNames.Add('afii64937', 701);
  FAdobeGlyphNames.Add('agrave', 224);
  FAdobeGlyphNames.Add('Agrave', 192);
  FAdobeGlyphNames.Add('Agravesmall', 63456);
  FAdobeGlyphNames.Add('agujarati', 2693);
  FAdobeGlyphNames.Add('agurmukhi', 2565);
  FAdobeGlyphNames.Add('ahiragana', 12354);
  FAdobeGlyphNames.Add('ahookabove', 7843);
  FAdobeGlyphNames.Add('Ahookabove', 7842);
  FAdobeGlyphNames.Add('aibengali', 2448);
  FAdobeGlyphNames.Add('aibopomofo', 12574);
  FAdobeGlyphNames.Add('aideva', 2320);
  FAdobeGlyphNames.Add('aiecyrillic', 1237);
  FAdobeGlyphNames.Add('Aiecyrillic', 1236);
  FAdobeGlyphNames.Add('aigujarati', 2704);
  FAdobeGlyphNames.Add('aigurmukhi', 2576);
  FAdobeGlyphNames.Add('aimatragurmukhi', 2632);
  FAdobeGlyphNames.Add('ainarabic', 1593);
  FAdobeGlyphNames.Add('ainfinalarabic', 65226);
  FAdobeGlyphNames.Add('aininitialarabic', 65227);
  FAdobeGlyphNames.Add('ainmedialarabic', 65228);
  FAdobeGlyphNames.Add('ainvertedbreve', 515);
  FAdobeGlyphNames.Add('Ainvertedbreve', 514);
  FAdobeGlyphNames.Add('aivowelsignbengali', 2504);
  FAdobeGlyphNames.Add('aivowelsigndeva', 2376);
  FAdobeGlyphNames.Add('aivowelsigngujarati', 2760);
  FAdobeGlyphNames.Add('akatakana', 12450);
  FAdobeGlyphNames.Add('akatakanahalfwidth', 65393);
  FAdobeGlyphNames.Add('akorean', 12623);
  FAdobeGlyphNames.Add('alef', 1488);
  FAdobeGlyphNames.Add('alefarabic', 1575);
  FAdobeGlyphNames.Add('alefdageshhebrew', 64304);
  FAdobeGlyphNames.Add('aleffinalarabic', 65166);
  FAdobeGlyphNames.Add('alefhamzaabovearabic', 1571);
  FAdobeGlyphNames.Add('alefhamzaabovefinalarabic', 65156);
  FAdobeGlyphNames.Add('alefhamzabelowarabic', 1573);
  FAdobeGlyphNames.Add('alefhamzabelowfinalarabic', 65160);
  FAdobeGlyphNames.Add('alefhebrew', 1488);
  FAdobeGlyphNames.Add('aleflamedhebrew', 64335);
  FAdobeGlyphNames.Add('alefmaddaabovearabic', 1570);
  FAdobeGlyphNames.Add('alefmaddaabovefinalarabic', 65154);
  FAdobeGlyphNames.Add('alefmaksuraarabic', 1609);
  FAdobeGlyphNames.Add('alefmaksurafinalarabic', 65264);
  FAdobeGlyphNames.Add('alefmaksurainitialarabic', 65267);
  FAdobeGlyphNames.Add('alefmaksuramedialarabic', 65268);
  FAdobeGlyphNames.Add('alefpatahhebrew', 64302);
  FAdobeGlyphNames.Add('alefqamatshebrew', 64303);
  FAdobeGlyphNames.Add('aleph', 8501);
  FAdobeGlyphNames.Add('allequal', 8780);
  FAdobeGlyphNames.Add('alpha', 945);
  FAdobeGlyphNames.Add('Alpha', 913);
  FAdobeGlyphNames.Add('alphatonos', 940);
  FAdobeGlyphNames.Add('Alphatonos', 902);
  FAdobeGlyphNames.Add('amacron', 257);
  FAdobeGlyphNames.Add('Amacron', 256);
  FAdobeGlyphNames.Add('amonospace', 65345);
  FAdobeGlyphNames.Add('Amonospace', 65313);
  FAdobeGlyphNames.Add('ampersand', 38);
  FAdobeGlyphNames.Add('ampersandmonospace', 65286);
  FAdobeGlyphNames.Add('ampersandsmall', 63270);
  FAdobeGlyphNames.Add('amsquare', 13250);
  FAdobeGlyphNames.Add('anbopomofo', 12578);
  FAdobeGlyphNames.Add('angbopomofo', 12580);
  FAdobeGlyphNames.Add('angkhankhuthai', 3674);
  FAdobeGlyphNames.Add('angle', 8736);
  FAdobeGlyphNames.Add('anglebracketleft', 12296);
  FAdobeGlyphNames.Add('anglebracketleftvertical', 65087);
  FAdobeGlyphNames.Add('anglebracketright', 12297);
  FAdobeGlyphNames.Add('anglebracketrightvertical', 65088);
  FAdobeGlyphNames.Add('angleleft', 9001);
  FAdobeGlyphNames.Add('angleright', 9002);
  FAdobeGlyphNames.Add('angstrom', 8491);
  FAdobeGlyphNames.Add('anoteleia', 903);
  FAdobeGlyphNames.Add('anudattadeva', 2386);
  FAdobeGlyphNames.Add('anusvarabengali', 2434);
  FAdobeGlyphNames.Add('anusvaradeva', 2306);
  FAdobeGlyphNames.Add('anusvaragujarati', 2690);
  FAdobeGlyphNames.Add('aogonek', 261);
  FAdobeGlyphNames.Add('Aogonek', 260);
  FAdobeGlyphNames.Add('apaatosquare', 13056);
  FAdobeGlyphNames.Add('aparen', 9372);
  FAdobeGlyphNames.Add('apostrophearmenian', 1370);
  FAdobeGlyphNames.Add('apostrophemod', 700);
  FAdobeGlyphNames.Add('apple', 63743);
  FAdobeGlyphNames.Add('approaches', 8784);
  FAdobeGlyphNames.Add('approxequal', 8776);
  FAdobeGlyphNames.Add('approxequalorimage', 8786);
  FAdobeGlyphNames.Add('approximatelyequal', 8773);
  FAdobeGlyphNames.Add('araeaekorean', 12686);
  FAdobeGlyphNames.Add('araeakorean', 12685);
  FAdobeGlyphNames.Add('arc', 8978);
  FAdobeGlyphNames.Add('arighthalfring', 7834);
  FAdobeGlyphNames.Add('aring', 229);
  FAdobeGlyphNames.Add('Aring', 197);
  FAdobeGlyphNames.Add('aringacute', 507);
  FAdobeGlyphNames.Add('Aringacute', 506);
  FAdobeGlyphNames.Add('aringbelow', 7681);
  FAdobeGlyphNames.Add('Aringbelow', 7680);
  FAdobeGlyphNames.Add('Aringsmall', 63461);
  FAdobeGlyphNames.Add('arrowboth', 8596);
  FAdobeGlyphNames.Add('arrowdashdown', 8675);
  FAdobeGlyphNames.Add('arrowdashleft', 8672);
  FAdobeGlyphNames.Add('arrowdashright', 8674);
  FAdobeGlyphNames.Add('arrowdashup', 8673);
  FAdobeGlyphNames.Add('arrowdblboth', 8660);
  FAdobeGlyphNames.Add('arrowdbldown', 8659);
  FAdobeGlyphNames.Add('arrowdblleft', 8656);
  FAdobeGlyphNames.Add('arrowdblright', 8658);
  FAdobeGlyphNames.Add('arrowdblup', 8657);
  FAdobeGlyphNames.Add('arrowdown', 8595);
  FAdobeGlyphNames.Add('arrowdownleft', 8601);
  FAdobeGlyphNames.Add('arrowdownright', 8600);
  FAdobeGlyphNames.Add('arrowdownwhite', 8681);
  FAdobeGlyphNames.Add('arrowheaddownmod', 709);
  FAdobeGlyphNames.Add('arrowheadleftmod', 706);
  FAdobeGlyphNames.Add('arrowheadrightmod', 707);
  FAdobeGlyphNames.Add('arrowheadupmod', 708);
  FAdobeGlyphNames.Add('arrowhorizex', 63719);
  FAdobeGlyphNames.Add('arrowleft', 8592);
  FAdobeGlyphNames.Add('arrowleftdbl', 8656);
  FAdobeGlyphNames.Add('arrowleftdblstroke', 8653);
  FAdobeGlyphNames.Add('arrowleftoverright', 8646);
  FAdobeGlyphNames.Add('arrowleftwhite', 8678);
  FAdobeGlyphNames.Add('arrowright', 8594);
  FAdobeGlyphNames.Add('arrowrightdblstroke', 8655);
  FAdobeGlyphNames.Add('arrowrightheavy', 10142);
  FAdobeGlyphNames.Add('arrowrightoverleft', 8644);
  FAdobeGlyphNames.Add('arrowrightwhite', 8680);
  FAdobeGlyphNames.Add('arrowtableft', 8676);
  FAdobeGlyphNames.Add('arrowtabright', 8677);
  FAdobeGlyphNames.Add('arrowup', 8593);
  FAdobeGlyphNames.Add('arrowupdn', 8597);
  FAdobeGlyphNames.Add('arrowupdnbse', 8616);
  FAdobeGlyphNames.Add('arrowupdownbase', 8616);
  FAdobeGlyphNames.Add('arrowupleft', 8598);
  FAdobeGlyphNames.Add('arrowupleftofdown', 8645);
  FAdobeGlyphNames.Add('arrowupright', 8599);
  FAdobeGlyphNames.Add('arrowupwhite', 8679);
  FAdobeGlyphNames.Add('arrowvertex', 63718);
  FAdobeGlyphNames.Add('asciicircum', 94);
  FAdobeGlyphNames.Add('asciicircummonospace', 65342);
  FAdobeGlyphNames.Add('asciitilde', 126);
  FAdobeGlyphNames.Add('asciitildemonospace', 65374);
  FAdobeGlyphNames.Add('ascript', 593);
  FAdobeGlyphNames.Add('ascriptturned', 594);
  FAdobeGlyphNames.Add('Asmall', 63329);
  FAdobeGlyphNames.Add('asmallhiragana', 12353);
  FAdobeGlyphNames.Add('asmallkatakana', 12449);
  FAdobeGlyphNames.Add('asmallkatakanahalfwidth', 65383);
  FAdobeGlyphNames.Add('asterisk', 42);
  FAdobeGlyphNames.Add('asteriskaltonearabic', 1645);
  FAdobeGlyphNames.Add('asteriskarabic', 1645);
  FAdobeGlyphNames.Add('asteriskmath', 8727);
  FAdobeGlyphNames.Add('asteriskmonospace', 65290);
  FAdobeGlyphNames.Add('asterisksmall', 65121);
  FAdobeGlyphNames.Add('asterism', 8258);
  FAdobeGlyphNames.Add('asuperior', 63209);
  FAdobeGlyphNames.Add('asymptoticallyequal', 8771);
  FAdobeGlyphNames.Add('at', 64);
  FAdobeGlyphNames.Add('atilde', 227);
  FAdobeGlyphNames.Add('Atilde', 195);
  FAdobeGlyphNames.Add('Atildesmall', 63459);
  FAdobeGlyphNames.Add('atmonospace', 65312);
  FAdobeGlyphNames.Add('atsmall', 65131);
  FAdobeGlyphNames.Add('aturned', 592);
  FAdobeGlyphNames.Add('aubengali', 2452);
  FAdobeGlyphNames.Add('aubopomofo', 12576);
  FAdobeGlyphNames.Add('audeva', 2324);
  FAdobeGlyphNames.Add('augujarati', 2708);
  FAdobeGlyphNames.Add('augurmukhi', 2580);
  FAdobeGlyphNames.Add('aulengthmarkbengali', 2519);
  FAdobeGlyphNames.Add('aumatragurmukhi', 2636);
  FAdobeGlyphNames.Add('auvowelsignbengali', 2508);
  FAdobeGlyphNames.Add('auvowelsigndeva', 2380);
  FAdobeGlyphNames.Add('auvowelsigngujarati', 2764);
  FAdobeGlyphNames.Add('avagrahadeva', 2365);
  FAdobeGlyphNames.Add('aybarmenian', 1377);
  FAdobeGlyphNames.Add('Aybarmenian', 1329);
  FAdobeGlyphNames.Add('ayin', 1506);
  FAdobeGlyphNames.Add('ayinaltonehebrew', 64288);
  FAdobeGlyphNames.Add('ayinhebrew', 1506);
  FAdobeGlyphNames.Add('b', 98);
  FAdobeGlyphNames.Add('B', 66);
  FAdobeGlyphNames.Add('babengali', 2476);
  FAdobeGlyphNames.Add('backslash', 92);
  FAdobeGlyphNames.Add('backslashmonospace', 65340);
  FAdobeGlyphNames.Add('badeva', 2348);
  FAdobeGlyphNames.Add('bagujarati', 2732);
  FAdobeGlyphNames.Add('bagurmukhi', 2604);
  FAdobeGlyphNames.Add('bahiragana', 12400);
  FAdobeGlyphNames.Add('bahtthai', 3647);
  FAdobeGlyphNames.Add('bakatakana', 12496);
  FAdobeGlyphNames.Add('bar', 124);
  FAdobeGlyphNames.Add('barmonospace', 65372);
  FAdobeGlyphNames.Add('bbopomofo', 12549);
  FAdobeGlyphNames.Add('bcircle', 9425);
  FAdobeGlyphNames.Add('Bcircle', 9399);
  FAdobeGlyphNames.Add('bdotaccent', 7683);
  FAdobeGlyphNames.Add('Bdotaccent', 7682);
  FAdobeGlyphNames.Add('bdotbelow', 7685);
  FAdobeGlyphNames.Add('Bdotbelow', 7684);
  FAdobeGlyphNames.Add('beamedsixteenthnotes', 9836);
  FAdobeGlyphNames.Add('because', 8757);
  FAdobeGlyphNames.Add('becyrillic', 1073);
  FAdobeGlyphNames.Add('Becyrillic', 1041);
  FAdobeGlyphNames.Add('beharabic', 1576);
  FAdobeGlyphNames.Add('behfinalarabic', 65168);
  FAdobeGlyphNames.Add('behinitialarabic', 65169);
  FAdobeGlyphNames.Add('behiragana', 12409);
  FAdobeGlyphNames.Add('behmedialarabic', 65170);
  FAdobeGlyphNames.Add('behmeeminitialarabic', 64671);
  FAdobeGlyphNames.Add('behmeemisolatedarabic', 64520);
  FAdobeGlyphNames.Add('behnoonfinalarabic', 64621);
  FAdobeGlyphNames.Add('bekatakana', 12505);
  FAdobeGlyphNames.Add('benarmenian', 1378);
  FAdobeGlyphNames.Add('Benarmenian', 1330);
  FAdobeGlyphNames.Add('bet', 1489);
  FAdobeGlyphNames.Add('beta', 946);
  FAdobeGlyphNames.Add('Beta', 914);
  FAdobeGlyphNames.Add('betasymbolgreek', 976);
  FAdobeGlyphNames.Add('betdagesh', 64305);
  FAdobeGlyphNames.Add('betdageshhebrew', 64305);
  FAdobeGlyphNames.Add('bethebrew', 1489);
  FAdobeGlyphNames.Add('betrafehebrew', 64332);
  FAdobeGlyphNames.Add('bhabengali', 2477);
  FAdobeGlyphNames.Add('bhadeva', 2349);
  FAdobeGlyphNames.Add('bhagujarati', 2733);
  FAdobeGlyphNames.Add('bhagurmukhi', 2605);
  FAdobeGlyphNames.Add('bhook', 595);
  FAdobeGlyphNames.Add('Bhook', 385);
  FAdobeGlyphNames.Add('bihiragana', 12403);
  FAdobeGlyphNames.Add('bikatakana', 12499);
  FAdobeGlyphNames.Add('bilabialclick', 664);
  FAdobeGlyphNames.Add('bindigurmukhi', 2562);
  FAdobeGlyphNames.Add('birusquare', 13105);
  FAdobeGlyphNames.Add('blackcircle', 9679);
  FAdobeGlyphNames.Add('blackdiamond', 9670);
  FAdobeGlyphNames.Add('blackdownpointingtriangle', 9660);
  FAdobeGlyphNames.Add('blackleftpointingpointer', 9668);
  FAdobeGlyphNames.Add('blackleftpointingtriangle', 9664);
  FAdobeGlyphNames.Add('blacklenticularbracketleft', 12304);
  FAdobeGlyphNames.Add('blacklenticularbracketleftvertical', 65083);
  FAdobeGlyphNames.Add('blacklenticularbracketright', 12305);
  FAdobeGlyphNames.Add('blacklenticularbracketrightvertical', 65084);
  FAdobeGlyphNames.Add('blacklowerlefttriangle', 9699);
  FAdobeGlyphNames.Add('blacklowerrighttriangle', 9698);
  FAdobeGlyphNames.Add('blackrectangle', 9644);
  FAdobeGlyphNames.Add('blackrightpointingpointer', 9658);
  FAdobeGlyphNames.Add('blackrightpointingtriangle', 9654);
  FAdobeGlyphNames.Add('blacksmallsquare', 9642);
  FAdobeGlyphNames.Add('blacksmilingface', 9787);
  FAdobeGlyphNames.Add('blacksquare', 9632);
  FAdobeGlyphNames.Add('blackstar', 9733);
  FAdobeGlyphNames.Add('blackupperlefttriangle', 9700);
  FAdobeGlyphNames.Add('blackupperrighttriangle', 9701);
  FAdobeGlyphNames.Add('blackuppointingsmalltriangle', 9652);
  FAdobeGlyphNames.Add('blackuppointingtriangle', 9650);
  FAdobeGlyphNames.Add('blank', 9251);
  FAdobeGlyphNames.Add('blinebelow', 7687);
  FAdobeGlyphNames.Add('Blinebelow', 7686);
  FAdobeGlyphNames.Add('block', 9608);
  FAdobeGlyphNames.Add('bmonospace', 65346);
  FAdobeGlyphNames.Add('Bmonospace', 65314);
  FAdobeGlyphNames.Add('bobaimaithai', 3610);
  FAdobeGlyphNames.Add('bohiragana', 12412);
  FAdobeGlyphNames.Add('bokatakana', 12508);
  FAdobeGlyphNames.Add('bparen', 9373);
  FAdobeGlyphNames.Add('bqsquare', 13251);
  FAdobeGlyphNames.Add('braceex', 63732);
  FAdobeGlyphNames.Add('braceleft', 123);
  FAdobeGlyphNames.Add('braceleftbt', 63731);
  FAdobeGlyphNames.Add('braceleftmid', 63730);
  FAdobeGlyphNames.Add('braceleftmonospace', 65371);
  FAdobeGlyphNames.Add('braceleftsmall', 65115);
  FAdobeGlyphNames.Add('bracelefttp', 63729);
  FAdobeGlyphNames.Add('braceleftvertical', 65079);
  FAdobeGlyphNames.Add('braceright', 125);
  FAdobeGlyphNames.Add('bracerightbt', 63742);
  FAdobeGlyphNames.Add('bracerightmid', 63741);
  FAdobeGlyphNames.Add('bracerightmonospace', 65373);
  FAdobeGlyphNames.Add('bracerightsmall', 65116);
  FAdobeGlyphNames.Add('bracerighttp', 63740);
  FAdobeGlyphNames.Add('bracerightvertical', 65080);
  FAdobeGlyphNames.Add('bracketleft', 91);
  FAdobeGlyphNames.Add('bracketleftbt', 63728);
  FAdobeGlyphNames.Add('bracketleftex', 63727);
  FAdobeGlyphNames.Add('bracketleftmonospace', 65339);
  FAdobeGlyphNames.Add('bracketlefttp', 63726);
  FAdobeGlyphNames.Add('bracketright', 93);
  FAdobeGlyphNames.Add('bracketrightbt', 63739);
  FAdobeGlyphNames.Add('bracketrightex', 63738);
  FAdobeGlyphNames.Add('bracketrightmonospace', 65341);
  FAdobeGlyphNames.Add('bracketrighttp', 63737);
  FAdobeGlyphNames.Add('breve', 728);
  FAdobeGlyphNames.Add('brevebelowcmb', 814);
  FAdobeGlyphNames.Add('brevecmb', 774);
  FAdobeGlyphNames.Add('breveinvertedbelowcmb', 815);
  FAdobeGlyphNames.Add('breveinvertedcmb', 785);
  FAdobeGlyphNames.Add('breveinverteddoublecmb', 865);
  FAdobeGlyphNames.Add('Brevesmall', 63220);
  FAdobeGlyphNames.Add('bridgebelowcmb', 810);
  FAdobeGlyphNames.Add('bridgeinvertedbelowcmb', 826);
  FAdobeGlyphNames.Add('brokenbar', 166);
  FAdobeGlyphNames.Add('Bsmall', 63330);
  FAdobeGlyphNames.Add('bstroke', 384);
  FAdobeGlyphNames.Add('bsuperior', 63210);
  FAdobeGlyphNames.Add('btopbar', 387);
  FAdobeGlyphNames.Add('Btopbar', 386);
  FAdobeGlyphNames.Add('buhiragana', 12406);
  FAdobeGlyphNames.Add('bukatakana', 12502);
  FAdobeGlyphNames.Add('bullet', 8226);
  FAdobeGlyphNames.Add('bulletinverse', 9688);
  FAdobeGlyphNames.Add('bulletoperator', 8729);
  FAdobeGlyphNames.Add('bullseye', 9678);
  FAdobeGlyphNames.Add('c', 99);
  FAdobeGlyphNames.Add('C', 67);
  FAdobeGlyphNames.Add('caarmenian', 1390);
  FAdobeGlyphNames.Add('Caarmenian', 1342);
  FAdobeGlyphNames.Add('cabengali', 2458);
  FAdobeGlyphNames.Add('cacute', 263);
  FAdobeGlyphNames.Add('Cacute', 262);
  FAdobeGlyphNames.Add('cadeva', 2330);
  FAdobeGlyphNames.Add('cagujarati', 2714);
  FAdobeGlyphNames.Add('cagurmukhi', 2586);
  FAdobeGlyphNames.Add('calsquare', 13192);
  FAdobeGlyphNames.Add('candrabindubengali', 2433);
  FAdobeGlyphNames.Add('candrabinducmb', 784);
  FAdobeGlyphNames.Add('candrabindudeva', 2305);
  FAdobeGlyphNames.Add('candrabindugujarati', 2689);
  FAdobeGlyphNames.Add('capslock', 8682);
  FAdobeGlyphNames.Add('careof', 8453);
  FAdobeGlyphNames.Add('caron', 711);
  FAdobeGlyphNames.Add('Caron', 63178);
  FAdobeGlyphNames.Add('caronbelowcmb', 812);
  FAdobeGlyphNames.Add('caroncmb', 780);
  FAdobeGlyphNames.Add('Caronsmall', 63221);
  FAdobeGlyphNames.Add('carriagereturn', 8629);
  FAdobeGlyphNames.Add('cbopomofo', 12568);
  FAdobeGlyphNames.Add('ccaron', 269);
  FAdobeGlyphNames.Add('Ccaron', 268);
  FAdobeGlyphNames.Add('ccedilla', 231);
  FAdobeGlyphNames.Add('Ccedilla', 199);
  FAdobeGlyphNames.Add('ccedillaacute', 7689);
  FAdobeGlyphNames.Add('Ccedillaacute', 7688);
  FAdobeGlyphNames.Add('Ccedillasmall', 63463);
  FAdobeGlyphNames.Add('ccircle', 9426);
  FAdobeGlyphNames.Add('Ccircle', 9400);
  FAdobeGlyphNames.Add('ccircumflex', 265);
  FAdobeGlyphNames.Add('Ccircumflex', 264);
  FAdobeGlyphNames.Add('ccurl', 597);
  FAdobeGlyphNames.Add('cdot', 267);
  FAdobeGlyphNames.Add('Cdot', 266);
  FAdobeGlyphNames.Add('cdotaccent', 267);
  FAdobeGlyphNames.Add('Cdotaccent', 266);
  FAdobeGlyphNames.Add('cdsquare', 13253);
  FAdobeGlyphNames.Add('cedilla', 184);
  FAdobeGlyphNames.Add('cedillacmb', 807);
  FAdobeGlyphNames.Add('Cedillasmall', 63416);
  FAdobeGlyphNames.Add('cent', 162);
  FAdobeGlyphNames.Add('centigrade', 8451);
  FAdobeGlyphNames.Add('centinferior', 63199);
  FAdobeGlyphNames.Add('centmonospace', 65504);
  FAdobeGlyphNames.Add('centoldstyle', 63394);
  FAdobeGlyphNames.Add('centsuperior', 63200);
  FAdobeGlyphNames.Add('chaarmenian', 1401);
  FAdobeGlyphNames.Add('Chaarmenian', 1353);
  FAdobeGlyphNames.Add('chabengali', 2459);
  FAdobeGlyphNames.Add('chadeva', 2331);
  FAdobeGlyphNames.Add('chagujarati', 2715);
  FAdobeGlyphNames.Add('chagurmukhi', 2587);
  FAdobeGlyphNames.Add('chbopomofo', 12564);
  FAdobeGlyphNames.Add('cheabkhasiancyrillic', 1213);
  FAdobeGlyphNames.Add('Cheabkhasiancyrillic', 1212);
  FAdobeGlyphNames.Add('checkmark', 10003);
  FAdobeGlyphNames.Add('checyrillic', 1095);
  FAdobeGlyphNames.Add('Checyrillic', 1063);
  FAdobeGlyphNames.Add('chedescenderabkhasiancyrillic', 1215);
  FAdobeGlyphNames.Add('Chedescenderabkhasiancyrillic', 1214);
  FAdobeGlyphNames.Add('chedescendercyrillic', 1207);
  FAdobeGlyphNames.Add('Chedescendercyrillic', 1206);
  FAdobeGlyphNames.Add('chedieresiscyrillic', 1269);
  FAdobeGlyphNames.Add('Chedieresiscyrillic', 1268);
  FAdobeGlyphNames.Add('cheharmenian', 1395);
  FAdobeGlyphNames.Add('Cheharmenian', 1347);
  FAdobeGlyphNames.Add('chekhakassiancyrillic', 1228);
  FAdobeGlyphNames.Add('Chekhakassiancyrillic', 1227);
  FAdobeGlyphNames.Add('cheverticalstrokecyrillic', 1209);
  FAdobeGlyphNames.Add('Cheverticalstrokecyrillic', 1208);
  FAdobeGlyphNames.Add('chi', 967);
  FAdobeGlyphNames.Add('Chi', 935);
  FAdobeGlyphNames.Add('chieuchacirclekorean', 12919);
  FAdobeGlyphNames.Add('chieuchaparenkorean', 12823);
  FAdobeGlyphNames.Add('chieuchcirclekorean', 12905);
  FAdobeGlyphNames.Add('chieuchkorean', 12618);
  FAdobeGlyphNames.Add('chieuchparenkorean', 12809);
  FAdobeGlyphNames.Add('chochangthai', 3594);
  FAdobeGlyphNames.Add('chochanthai', 3592);
  FAdobeGlyphNames.Add('chochingthai', 3593);
  FAdobeGlyphNames.Add('chochoethai', 3596);
  FAdobeGlyphNames.Add('chook', 392);
  FAdobeGlyphNames.Add('Chook', 391);
  FAdobeGlyphNames.Add('cieucacirclekorean', 12918);
  FAdobeGlyphNames.Add('cieucaparenkorean', 12822);
  FAdobeGlyphNames.Add('cieuccirclekorean', 12904);
  FAdobeGlyphNames.Add('cieuckorean', 12616);
  FAdobeGlyphNames.Add('cieucparenkorean', 12808);
  FAdobeGlyphNames.Add('cieucuparenkorean', 12828);
  FAdobeGlyphNames.Add('circle', 9675);
  FAdobeGlyphNames.Add('circlemultiply', 8855);
  FAdobeGlyphNames.Add('circleot', 8857);
  FAdobeGlyphNames.Add('circleplus', 8853);
  FAdobeGlyphNames.Add('circlepostalmark', 12342);
  FAdobeGlyphNames.Add('circlewithlefthalfblack', 9680);
  FAdobeGlyphNames.Add('circlewithrighthalfblack', 9681);
  FAdobeGlyphNames.Add('circumflex', 710);
  FAdobeGlyphNames.Add('circumflexbelowcmb', 813);
  FAdobeGlyphNames.Add('circumflexcmb', 770);
  FAdobeGlyphNames.Add('Circumflexsmall', 63222);
  FAdobeGlyphNames.Add('clear', 8999);
  FAdobeGlyphNames.Add('clickalveolar', 450);
  FAdobeGlyphNames.Add('clickdental', 448);
  FAdobeGlyphNames.Add('clicklateral', 449);
  FAdobeGlyphNames.Add('clickretroflex', 451);
  FAdobeGlyphNames.Add('club', 9827);
  FAdobeGlyphNames.Add('clubsuitblack', 9827);
  FAdobeGlyphNames.Add('clubsuitwhite', 9831);
  FAdobeGlyphNames.Add('cmcubedsquare', 13220);
  FAdobeGlyphNames.Add('cmonospace', 65347);
  FAdobeGlyphNames.Add('Cmonospace', 65315);
  FAdobeGlyphNames.Add('cmsquaredsquare', 13216);
  FAdobeGlyphNames.Add('coarmenian', 1409);
  FAdobeGlyphNames.Add('Coarmenian', 1361);
  FAdobeGlyphNames.Add('colon', 58);
  FAdobeGlyphNames.Add('colonmonetary', 8353);
  FAdobeGlyphNames.Add('colonmonospace', 65306);
  FAdobeGlyphNames.Add('colonsign', 8353);
  FAdobeGlyphNames.Add('colonsmall', 65109);
  FAdobeGlyphNames.Add('colontriangularhalfmod', 721);
  FAdobeGlyphNames.Add('colontriangularmod', 720);
  FAdobeGlyphNames.Add('comma', 44);
  FAdobeGlyphNames.Add('commaabovecmb', 787);
  FAdobeGlyphNames.Add('commaaboverightcmb', 789);
  FAdobeGlyphNames.Add('commaaccent', 63171);
  FAdobeGlyphNames.Add('commaarabic', 1548);
  FAdobeGlyphNames.Add('commaarmenian', 1373);
  FAdobeGlyphNames.Add('commainferior', 63201);
  FAdobeGlyphNames.Add('commamonospace', 65292);
  FAdobeGlyphNames.Add('commareversedabovecmb', 788);
  FAdobeGlyphNames.Add('commareversedmod', 701);
  FAdobeGlyphNames.Add('commasmall', 65104);
  FAdobeGlyphNames.Add('commasuperior', 63202);
  FAdobeGlyphNames.Add('commaturnedabovecmb', 786);
  FAdobeGlyphNames.Add('commaturnedmod', 699);
  FAdobeGlyphNames.Add('compass', 9788);
  FAdobeGlyphNames.Add('congruent', 8773);
  FAdobeGlyphNames.Add('contourintegral', 8750);
  FAdobeGlyphNames.Add('control', 8963);
  FAdobeGlyphNames.Add('controlACK', 6);
  FAdobeGlyphNames.Add('controlBEL', 7);
  FAdobeGlyphNames.Add('controlBS', 8);
  FAdobeGlyphNames.Add('controlCAN', 24);
  FAdobeGlyphNames.Add('controlCR', 13);
  FAdobeGlyphNames.Add('controlDC1', 17);
  FAdobeGlyphNames.Add('controlDC2', 18);
  FAdobeGlyphNames.Add('controlDC3', 19);
  FAdobeGlyphNames.Add('controlDC4', 20);
  FAdobeGlyphNames.Add('controlDEL', 127);
  FAdobeGlyphNames.Add('controlDLE', 16);
  FAdobeGlyphNames.Add('controlEM', 25);
  FAdobeGlyphNames.Add('controlENQ', 5);
  FAdobeGlyphNames.Add('controlEOT', 4);
  FAdobeGlyphNames.Add('controlESC', 27);
  FAdobeGlyphNames.Add('controlETB', 23);
  FAdobeGlyphNames.Add('controlETX', 3);
  FAdobeGlyphNames.Add('controlFF', 12);
  FAdobeGlyphNames.Add('controlFS', 28);
  FAdobeGlyphNames.Add('controlGS', 29);
  FAdobeGlyphNames.Add('controlHT', 9);
  FAdobeGlyphNames.Add('controlLF', 10);
  FAdobeGlyphNames.Add('controlNAK', 21);
  FAdobeGlyphNames.Add('controlRS', 30);
  FAdobeGlyphNames.Add('controlSI', 15);
  FAdobeGlyphNames.Add('controlSO', 14);
  FAdobeGlyphNames.Add('controlSOT', 2);
  FAdobeGlyphNames.Add('controlSTX', 1);
  FAdobeGlyphNames.Add('controlSUB', 26);
  FAdobeGlyphNames.Add('controlSYN', 22);
  FAdobeGlyphNames.Add('controlUS', 31);
  FAdobeGlyphNames.Add('controlVT', 11);
  FAdobeGlyphNames.Add('copyright', 169);
  FAdobeGlyphNames.Add('copyrightsans', 63721);
  FAdobeGlyphNames.Add('copyrightserif', 63193);
  FAdobeGlyphNames.Add('cornerbracketleft', 12300);
  FAdobeGlyphNames.Add('cornerbracketlefthalfwidth', 65378);
  FAdobeGlyphNames.Add('cornerbracketleftvertical', 65089);
  FAdobeGlyphNames.Add('cornerbracketright', 12301);
  FAdobeGlyphNames.Add('cornerbracketrighthalfwidth', 65379);
  FAdobeGlyphNames.Add('cornerbracketrightvertical', 65090);
  FAdobeGlyphNames.Add('corporationsquare', 13183);
  FAdobeGlyphNames.Add('cosquare', 13255);
  FAdobeGlyphNames.Add('coverkgsquare', 13254);
  FAdobeGlyphNames.Add('cparen', 9374);
  FAdobeGlyphNames.Add('cruzeiro', 8354);
  FAdobeGlyphNames.Add('Csmall', 63331);
  FAdobeGlyphNames.Add('cstretched', 663);
  FAdobeGlyphNames.Add('curlyand', 8911);
  FAdobeGlyphNames.Add('curlyor', 8910);
  FAdobeGlyphNames.Add('currency', 164);
  FAdobeGlyphNames.Add('cyrbreve', 63188);
  FAdobeGlyphNames.Add('cyrBreve', 63185);
  FAdobeGlyphNames.Add('cyrflex', 63189);
  FAdobeGlyphNames.Add('cyrFlex', 63186);
  FAdobeGlyphNames.Add('d', 100);
  FAdobeGlyphNames.Add('D', 68);
  FAdobeGlyphNames.Add('daarmenian', 1380);
  FAdobeGlyphNames.Add('Daarmenian', 1332);
  FAdobeGlyphNames.Add('dabengali', 2470);
  FAdobeGlyphNames.Add('dadarabic', 1590);
  FAdobeGlyphNames.Add('dadeva', 2342);
  FAdobeGlyphNames.Add('dadfinalarabic', 65214);
  FAdobeGlyphNames.Add('dadinitialarabic', 65215);
  FAdobeGlyphNames.Add('dadmedialarabic', 65216);
  FAdobeGlyphNames.Add('Dafrican', 393);
  FAdobeGlyphNames.Add('dagesh', 1468);
  FAdobeGlyphNames.Add('dageshhebrew', 1468);
  FAdobeGlyphNames.Add('dagger', 8224);
  FAdobeGlyphNames.Add('daggerdbl', 8225);
  FAdobeGlyphNames.Add('dagujarati', 2726);
  FAdobeGlyphNames.Add('dagurmukhi', 2598);
  FAdobeGlyphNames.Add('dahiragana', 12384);
  FAdobeGlyphNames.Add('dakatakana', 12480);
  FAdobeGlyphNames.Add('dalarabic', 1583);
  FAdobeGlyphNames.Add('dalet', 1491);
  FAdobeGlyphNames.Add('daletdagesh', 64307);
  FAdobeGlyphNames.Add('daletdageshhebrew', 64307);
  FAdobeGlyphNames.Add('dalethatafpatah', 1491);
  FAdobeGlyphNames.Add('dalethatafpatahhebrew', 1491);
  FAdobeGlyphNames.Add('dalethatafsegol', 1491);
  FAdobeGlyphNames.Add('dalethatafsegolhebrew', 1491);
  FAdobeGlyphNames.Add('dalethebrew', 1491);
  FAdobeGlyphNames.Add('dalethiriq', 1491);
  FAdobeGlyphNames.Add('dalethiriqhebrew', 1491);
  FAdobeGlyphNames.Add('daletholam', 1491);
  FAdobeGlyphNames.Add('daletholamhebrew', 1491);
  FAdobeGlyphNames.Add('daletpatah', 1491);
  FAdobeGlyphNames.Add('daletpatahhebrew', 1491);
  FAdobeGlyphNames.Add('daletqamats', 1491);
  FAdobeGlyphNames.Add('daletqamatshebrew', 1491);
  FAdobeGlyphNames.Add('daletqubuts', 1491);
  FAdobeGlyphNames.Add('daletqubutshebrew', 1491);
  FAdobeGlyphNames.Add('daletsegol', 1491);
  FAdobeGlyphNames.Add('daletsegolhebrew', 1491);
  FAdobeGlyphNames.Add('daletsheva', 1491);
  FAdobeGlyphNames.Add('daletshevahebrew', 1491);
  FAdobeGlyphNames.Add('dalettsere', 1491);
  FAdobeGlyphNames.Add('dalettserehebrew', 1491);
  FAdobeGlyphNames.Add('dalfinalarabic', 65194);
  FAdobeGlyphNames.Add('dammaarabic', 1615);
  FAdobeGlyphNames.Add('dammalowarabic', 1615);
  FAdobeGlyphNames.Add('dammatanaltonearabic', 1612);
  FAdobeGlyphNames.Add('dammatanarabic', 1612);
  FAdobeGlyphNames.Add('danda', 2404);
  FAdobeGlyphNames.Add('dargahebrew', 1447);
  FAdobeGlyphNames.Add('dargalefthebrew', 1447);
  FAdobeGlyphNames.Add('dasiapneumatacyrilliccmb', 1157);
  FAdobeGlyphNames.Add('dblanglebracketleft', 12298);
  FAdobeGlyphNames.Add('dblanglebracketleftvertical', 65085);
  FAdobeGlyphNames.Add('dblanglebracketright', 12299);
  FAdobeGlyphNames.Add('dblanglebracketrightvertical', 65086);
  FAdobeGlyphNames.Add('dblarchinvertedbelowcmb', 811);
  FAdobeGlyphNames.Add('dblarrowleft', 8660);
  FAdobeGlyphNames.Add('dblarrowright', 8658);
  FAdobeGlyphNames.Add('dbldanda', 2405);
  FAdobeGlyphNames.Add('dblgrave', 63190);
  FAdobeGlyphNames.Add('dblGrave', 63187);
  FAdobeGlyphNames.Add('dblgravecmb', 783);
  FAdobeGlyphNames.Add('dblintegral', 8748);
  FAdobeGlyphNames.Add('dbllowline', 8215);
  FAdobeGlyphNames.Add('dbllowlinecmb', 819);
  FAdobeGlyphNames.Add('dbloverlinecmb', 831);
  FAdobeGlyphNames.Add('dblprimemod', 698);
  FAdobeGlyphNames.Add('dblverticalbar', 8214);
  FAdobeGlyphNames.Add('dblverticallineabovecmb', 782);
  FAdobeGlyphNames.Add('dbopomofo', 12553);
  FAdobeGlyphNames.Add('dbsquare', 13256);
  FAdobeGlyphNames.Add('dcaron', 271);
  FAdobeGlyphNames.Add('Dcaron', 270);
  FAdobeGlyphNames.Add('dcedilla', 7697);
  FAdobeGlyphNames.Add('Dcedilla', 7696);
  FAdobeGlyphNames.Add('dcircle', 9427);
  FAdobeGlyphNames.Add('Dcircle', 9401);
  FAdobeGlyphNames.Add('dcircumflexbelow', 7699);
  FAdobeGlyphNames.Add('Dcircumflexbelow', 7698);
  FAdobeGlyphNames.Add('dcroat', 273);
  FAdobeGlyphNames.Add('Dcroat', 272);
  FAdobeGlyphNames.Add('ddabengali', 2465);
  FAdobeGlyphNames.Add('ddadeva', 2337);
  FAdobeGlyphNames.Add('ddagujarati', 2721);
  FAdobeGlyphNames.Add('ddagurmukhi', 2593);
  FAdobeGlyphNames.Add('ddalarabic', 1672);
  FAdobeGlyphNames.Add('ddalfinalarabic', 64393);
  FAdobeGlyphNames.Add('dddhadeva', 2396);
  FAdobeGlyphNames.Add('ddhabengali', 2466);
  FAdobeGlyphNames.Add('ddhadeva', 2338);
  FAdobeGlyphNames.Add('ddhagujarati', 2722);
  FAdobeGlyphNames.Add('ddhagurmukhi', 2594);
  FAdobeGlyphNames.Add('ddotaccent', 7691);
  FAdobeGlyphNames.Add('Ddotaccent', 7690);
  FAdobeGlyphNames.Add('ddotbelow', 7693);
  FAdobeGlyphNames.Add('Ddotbelow', 7692);
  FAdobeGlyphNames.Add('decimalseparatorarabic', 1643);
  FAdobeGlyphNames.Add('decimalseparatorpersian', 1643);
  FAdobeGlyphNames.Add('decyrillic', 1076);
  FAdobeGlyphNames.Add('Decyrillic', 1044);
  FAdobeGlyphNames.Add('degree', 176);
  FAdobeGlyphNames.Add('dehihebrew', 1453);
  FAdobeGlyphNames.Add('dehiragana', 12391);
  FAdobeGlyphNames.Add('deicoptic', 1007);
  FAdobeGlyphNames.Add('Deicoptic', 1006);
  FAdobeGlyphNames.Add('dekatakana', 12487);
  FAdobeGlyphNames.Add('deleteleft', 9003);
  FAdobeGlyphNames.Add('deleteright', 8998);
  FAdobeGlyphNames.Add('delta', 948);
  FAdobeGlyphNames.Add('Delta', 8710);
  FAdobeGlyphNames.Add('Deltagreek', 916);
  FAdobeGlyphNames.Add('deltaturned', 397);
  FAdobeGlyphNames.Add('denominatorminusonenumeratorbengali', 2552);
  FAdobeGlyphNames.Add('dezh', 676);
  FAdobeGlyphNames.Add('dhabengali', 2471);
  FAdobeGlyphNames.Add('dhadeva', 2343);
  FAdobeGlyphNames.Add('dhagujarati', 2727);
  FAdobeGlyphNames.Add('dhagurmukhi', 2599);
  FAdobeGlyphNames.Add('dhook', 599);
  FAdobeGlyphNames.Add('Dhook', 394);
  FAdobeGlyphNames.Add('dialytikatonos', 901);
  FAdobeGlyphNames.Add('dialytikatonoscmb', 836);
  FAdobeGlyphNames.Add('diamond', 9830);
  FAdobeGlyphNames.Add('diamondsuitwhite', 9826);
  FAdobeGlyphNames.Add('dieresis', 168);
  FAdobeGlyphNames.Add('Dieresis', 63179);
  FAdobeGlyphNames.Add('dieresisacute', 63191);
  FAdobeGlyphNames.Add('DieresisAcute', 63180);
  FAdobeGlyphNames.Add('dieresisbelowcmb', 804);
  FAdobeGlyphNames.Add('dieresiscmb', 776);
  FAdobeGlyphNames.Add('dieresisgrave', 63192);
  FAdobeGlyphNames.Add('DieresisGrave', 63181);
  FAdobeGlyphNames.Add('Dieresissmall', 63400);
  FAdobeGlyphNames.Add('dieresistonos', 901);
  FAdobeGlyphNames.Add('Digammagreek', 988);
  FAdobeGlyphNames.Add('dihiragana', 12386);
  FAdobeGlyphNames.Add('dikatakana', 12482);
  FAdobeGlyphNames.Add('dittomark', 12291);
  FAdobeGlyphNames.Add('divide', 247);
  FAdobeGlyphNames.Add('divides', 8739);
  FAdobeGlyphNames.Add('divisionslash', 8725);
  FAdobeGlyphNames.Add('djecyrillic', 1106);
  FAdobeGlyphNames.Add('Djecyrillic', 1026);
  FAdobeGlyphNames.Add('dkshade', 9619);
  FAdobeGlyphNames.Add('dlinebelow', 7695);
  FAdobeGlyphNames.Add('Dlinebelow', 7694);
  FAdobeGlyphNames.Add('dlsquare', 13207);
  FAdobeGlyphNames.Add('dmacron', 273);
  FAdobeGlyphNames.Add('dmonospace', 65348);
  FAdobeGlyphNames.Add('Dmonospace', 65316);
  FAdobeGlyphNames.Add('dnblock', 9604);
  FAdobeGlyphNames.Add('dochadathai', 3598);
  FAdobeGlyphNames.Add('dodekthai', 3604);
  FAdobeGlyphNames.Add('dohiragana', 12393);
  FAdobeGlyphNames.Add('dokatakana', 12489);
  FAdobeGlyphNames.Add('dollar', 36);
  FAdobeGlyphNames.Add('dollarinferior', 63203);
  FAdobeGlyphNames.Add('dollarmonospace', 65284);
  FAdobeGlyphNames.Add('dollaroldstyle', 63268);
  FAdobeGlyphNames.Add('dollarsmall', 65129);
  FAdobeGlyphNames.Add('dollarsuperior', 63204);
  FAdobeGlyphNames.Add('dong', 8363);
  FAdobeGlyphNames.Add('dorusquare', 13094);
  FAdobeGlyphNames.Add('dotaccent', 729);
  FAdobeGlyphNames.Add('dotaccentcmb', 775);
  FAdobeGlyphNames.Add('Dotaccentsmall', 63223);
  FAdobeGlyphNames.Add('dotbelowcmb', 803);
  FAdobeGlyphNames.Add('dotbelowcomb', 803);
  FAdobeGlyphNames.Add('dotkatakana', 12539);
  FAdobeGlyphNames.Add('dotlessi', 305);
  FAdobeGlyphNames.Add('dotlessj', 63166);
  FAdobeGlyphNames.Add('dotlessjstrokehook', 644);
  FAdobeGlyphNames.Add('dotmath', 8901);
  FAdobeGlyphNames.Add('dottedcircle', 9676);
  FAdobeGlyphNames.Add('doubleyodpatah', 64287);
  FAdobeGlyphNames.Add('doubleyodpatahhebrew', 64287);
  FAdobeGlyphNames.Add('downtackbelowcmb', 798);
  FAdobeGlyphNames.Add('downtackmod', 725);
  FAdobeGlyphNames.Add('dparen', 9375);
  FAdobeGlyphNames.Add('Dslash', 272);
  FAdobeGlyphNames.Add('Dsmall', 63332);
  FAdobeGlyphNames.Add('dsuperior', 63211);
  FAdobeGlyphNames.Add('dtail', 598);
  FAdobeGlyphNames.Add('dtopbar', 396);
  FAdobeGlyphNames.Add('Dtopbar', 395);
  FAdobeGlyphNames.Add('duhiragana', 12389);
  FAdobeGlyphNames.Add('dukatakana', 12485);
  FAdobeGlyphNames.Add('dz', 499);
  FAdobeGlyphNames.Add('Dz', 498);
  FAdobeGlyphNames.Add('DZ', 497);
  FAdobeGlyphNames.Add('dzaltone', 675);
  FAdobeGlyphNames.Add('dzcaron', 454);
  FAdobeGlyphNames.Add('Dzcaron', 453);
  FAdobeGlyphNames.Add('DZcaron', 452);
  FAdobeGlyphNames.Add('dzcurl', 677);
  FAdobeGlyphNames.Add('dzeabkhasiancyrillic', 1249);
  FAdobeGlyphNames.Add('Dzeabkhasiancyrillic', 1248);
  FAdobeGlyphNames.Add('dzecyrillic', 1109);
  FAdobeGlyphNames.Add('Dzecyrillic', 1029);
  FAdobeGlyphNames.Add('dzhecyrillic', 1119);
  FAdobeGlyphNames.Add('Dzhecyrillic', 1039);
  FAdobeGlyphNames.Add('e', 101);
  FAdobeGlyphNames.Add('E', 69);
  FAdobeGlyphNames.Add('eacute', 233);
  FAdobeGlyphNames.Add('Eacute', 201);
  FAdobeGlyphNames.Add('Eacutesmall', 63465);
  FAdobeGlyphNames.Add('earth', 9793);
  FAdobeGlyphNames.Add('ebengali', 2447);
  FAdobeGlyphNames.Add('ebopomofo', 12572);
  FAdobeGlyphNames.Add('ebreve', 277);
  FAdobeGlyphNames.Add('Ebreve', 276);
  FAdobeGlyphNames.Add('ecandradeva', 2317);
  FAdobeGlyphNames.Add('ecandragujarati', 2701);
  FAdobeGlyphNames.Add('ecandravowelsigndeva', 2373);
  FAdobeGlyphNames.Add('ecandravowelsigngujarati', 2757);
  FAdobeGlyphNames.Add('ecaron', 283);
  FAdobeGlyphNames.Add('Ecaron', 282);
  FAdobeGlyphNames.Add('ecedillabreve', 7709);
  FAdobeGlyphNames.Add('Ecedillabreve', 7708);
  FAdobeGlyphNames.Add('echarmenian', 1381);
  FAdobeGlyphNames.Add('Echarmenian', 1333);
  FAdobeGlyphNames.Add('echyiwnarmenian', 1415);
  FAdobeGlyphNames.Add('ecircle', 9428);
  FAdobeGlyphNames.Add('Ecircle', 9402);
  FAdobeGlyphNames.Add('ecircumflex', 234);
  FAdobeGlyphNames.Add('Ecircumflex', 202);
  FAdobeGlyphNames.Add('ecircumflexacute', 7871);
  FAdobeGlyphNames.Add('Ecircumflexacute', 7870);
  FAdobeGlyphNames.Add('ecircumflexbelow', 7705);
  FAdobeGlyphNames.Add('Ecircumflexbelow', 7704);
  FAdobeGlyphNames.Add('ecircumflexdotbelow', 7879);
  FAdobeGlyphNames.Add('Ecircumflexdotbelow', 7878);
  FAdobeGlyphNames.Add('ecircumflexgrave', 7873);
  FAdobeGlyphNames.Add('Ecircumflexgrave', 7872);
  FAdobeGlyphNames.Add('ecircumflexhookabove', 7875);
  FAdobeGlyphNames.Add('Ecircumflexhookabove', 7874);
  FAdobeGlyphNames.Add('Ecircumflexsmall', 63466);
  FAdobeGlyphNames.Add('ecircumflextilde', 7877);
  FAdobeGlyphNames.Add('Ecircumflextilde', 7876);
  FAdobeGlyphNames.Add('ecyrillic', 1108);
  FAdobeGlyphNames.Add('Ecyrillic', 1028);
  FAdobeGlyphNames.Add('edblgrave', 517);
  FAdobeGlyphNames.Add('Edblgrave', 516);
  FAdobeGlyphNames.Add('edeva', 2319);
  FAdobeGlyphNames.Add('edieresis', 235);
  FAdobeGlyphNames.Add('Edieresis', 203);
  FAdobeGlyphNames.Add('Edieresissmall', 63467);
  FAdobeGlyphNames.Add('edot', 279);
  FAdobeGlyphNames.Add('Edot', 278);
  FAdobeGlyphNames.Add('edotaccent', 279);
  FAdobeGlyphNames.Add('Edotaccent', 278);
  FAdobeGlyphNames.Add('edotbelow', 7865);
  FAdobeGlyphNames.Add('Edotbelow', 7864);
  FAdobeGlyphNames.Add('eegurmukhi', 2575);
  FAdobeGlyphNames.Add('eematragurmukhi', 2631);
  FAdobeGlyphNames.Add('efcyrillic', 1092);
  FAdobeGlyphNames.Add('Efcyrillic', 1060);
  FAdobeGlyphNames.Add('egrave', 232);
  FAdobeGlyphNames.Add('Egrave', 200);
  FAdobeGlyphNames.Add('Egravesmall', 63464);
  FAdobeGlyphNames.Add('egujarati', 2703);
  FAdobeGlyphNames.Add('eharmenian', 1383);
  FAdobeGlyphNames.Add('Eharmenian', 1335);
  FAdobeGlyphNames.Add('ehbopomofo', 12573);
  FAdobeGlyphNames.Add('ehiragana', 12360);
  FAdobeGlyphNames.Add('ehookabove', 7867);
  FAdobeGlyphNames.Add('Ehookabove', 7866);
  FAdobeGlyphNames.Add('eibopomofo', 12575);
  FAdobeGlyphNames.Add('eight', 56);
  FAdobeGlyphNames.Add('eightarabic', 1640);
  FAdobeGlyphNames.Add('eightbengali', 2542);
  FAdobeGlyphNames.Add('eightcircle', 9319);
  FAdobeGlyphNames.Add('eightcircleinversesansserif', 10129);
  FAdobeGlyphNames.Add('eightdeva', 2414);
  FAdobeGlyphNames.Add('eighteencircle', 9329);
  FAdobeGlyphNames.Add('eighteenparen', 9349);
  FAdobeGlyphNames.Add('eighteenperiod', 9369);
  FAdobeGlyphNames.Add('eightgujarati', 2798);
  FAdobeGlyphNames.Add('eightgurmukhi', 2670);
  FAdobeGlyphNames.Add('eighthackarabic', 1640);
  FAdobeGlyphNames.Add('eighthangzhou', 12328);
  FAdobeGlyphNames.Add('eighthnotebeamed', 9835);
  FAdobeGlyphNames.Add('eightideographicparen', 12839);
  FAdobeGlyphNames.Add('eightinferior', 8328);
  FAdobeGlyphNames.Add('eightmonospace', 65304);
  FAdobeGlyphNames.Add('eightoldstyle', 63288);
  FAdobeGlyphNames.Add('eightparen', 9339);
  FAdobeGlyphNames.Add('eightperiod', 9359);
  FAdobeGlyphNames.Add('eightpersian', 1784);
  FAdobeGlyphNames.Add('eightroman', 8567);
  FAdobeGlyphNames.Add('Eightroman', 8551);
  FAdobeGlyphNames.Add('eightsuperior', 8312);
  FAdobeGlyphNames.Add('eightthai', 3672);
  FAdobeGlyphNames.Add('einvertedbreve', 519);
  FAdobeGlyphNames.Add('Einvertedbreve', 518);
  FAdobeGlyphNames.Add('eiotifiedcyrillic', 1125);
  FAdobeGlyphNames.Add('Eiotifiedcyrillic', 1124);
  FAdobeGlyphNames.Add('ekatakana', 12456);
  FAdobeGlyphNames.Add('ekatakanahalfwidth', 65396);
  FAdobeGlyphNames.Add('ekonkargurmukhi', 2676);
  FAdobeGlyphNames.Add('ekorean', 12628);
  FAdobeGlyphNames.Add('elcyrillic', 1083);
  FAdobeGlyphNames.Add('Elcyrillic', 1051);
  FAdobeGlyphNames.Add('element', 8712);
  FAdobeGlyphNames.Add('elevencircle', 9322);
  FAdobeGlyphNames.Add('elevenparen', 9342);
  FAdobeGlyphNames.Add('elevenperiod', 9362);
  FAdobeGlyphNames.Add('elevenroman', 8570);
  FAdobeGlyphNames.Add('Elevenroman', 8554);
  FAdobeGlyphNames.Add('ellipsis', 8230);
  FAdobeGlyphNames.Add('ellipsisvertical', 8942);
  FAdobeGlyphNames.Add('emacron', 275);
  FAdobeGlyphNames.Add('Emacron', 274);
  FAdobeGlyphNames.Add('emacronacute', 7703);
  FAdobeGlyphNames.Add('Emacronacute', 7702);
  FAdobeGlyphNames.Add('emacrongrave', 7701);
  FAdobeGlyphNames.Add('Emacrongrave', 7700);
  FAdobeGlyphNames.Add('emcyrillic', 1084);
  FAdobeGlyphNames.Add('Emcyrillic', 1052);
  FAdobeGlyphNames.Add('emdash', 8212);
  FAdobeGlyphNames.Add('emdashvertical', 65073);
  FAdobeGlyphNames.Add('emonospace', 65349);
  FAdobeGlyphNames.Add('Emonospace', 65317);
  FAdobeGlyphNames.Add('emphasismarkarmenian', 1371);
  FAdobeGlyphNames.Add('emptyset', 8709);
  FAdobeGlyphNames.Add('enbopomofo', 12579);
  FAdobeGlyphNames.Add('encyrillic', 1085);
  FAdobeGlyphNames.Add('Encyrillic', 1053);
  FAdobeGlyphNames.Add('endash', 8211);
  FAdobeGlyphNames.Add('endashvertical', 65074);
  FAdobeGlyphNames.Add('endescendercyrillic', 1187);
  FAdobeGlyphNames.Add('Endescendercyrillic', 1186);
  FAdobeGlyphNames.Add('eng', 331);
  FAdobeGlyphNames.Add('Eng', 330);
  FAdobeGlyphNames.Add('engbopomofo', 12581);
  FAdobeGlyphNames.Add('enghecyrillic', 1189);
  FAdobeGlyphNames.Add('Enghecyrillic', 1188);
  FAdobeGlyphNames.Add('enhookcyrillic', 1224);
  FAdobeGlyphNames.Add('Enhookcyrillic', 1223);
  FAdobeGlyphNames.Add('enspace', 8194);
  FAdobeGlyphNames.Add('eogonek', 281);
  FAdobeGlyphNames.Add('Eogonek', 280);
  FAdobeGlyphNames.Add('eokorean', 12627);
  FAdobeGlyphNames.Add('eopen', 603);
  FAdobeGlyphNames.Add('Eopen', 400);
  FAdobeGlyphNames.Add('eopenclosed', 666);
  FAdobeGlyphNames.Add('eopenreversed', 604);
  FAdobeGlyphNames.Add('eopenreversedclosed', 606);
  FAdobeGlyphNames.Add('eopenreversedhook', 605);
  FAdobeGlyphNames.Add('eparen', 9376);
  FAdobeGlyphNames.Add('epsilon', 949);
  FAdobeGlyphNames.Add('Epsilon', 917);
  FAdobeGlyphNames.Add('epsilontonos', 941);
  FAdobeGlyphNames.Add('Epsilontonos', 904);
  FAdobeGlyphNames.Add('equal', 61);
  FAdobeGlyphNames.Add('equalmonospace', 65309);
  FAdobeGlyphNames.Add('equalsmall', 65126);
  FAdobeGlyphNames.Add('equalsuperior', 8316);
  FAdobeGlyphNames.Add('equivalence', 8801);
  FAdobeGlyphNames.Add('erbopomofo', 12582);
  FAdobeGlyphNames.Add('ercyrillic', 1088);
  FAdobeGlyphNames.Add('Ercyrillic', 1056);
  FAdobeGlyphNames.Add('ereversed', 600);
  FAdobeGlyphNames.Add('Ereversed', 398);
  FAdobeGlyphNames.Add('ereversedcyrillic', 1101);
  FAdobeGlyphNames.Add('Ereversedcyrillic', 1069);
  FAdobeGlyphNames.Add('escyrillic', 1089);
  FAdobeGlyphNames.Add('Escyrillic', 1057);
  FAdobeGlyphNames.Add('esdescendercyrillic', 1195);
  FAdobeGlyphNames.Add('Esdescendercyrillic', 1194);
  FAdobeGlyphNames.Add('esh', 643);
  FAdobeGlyphNames.Add('Esh', 425);
  FAdobeGlyphNames.Add('eshcurl', 646);
  FAdobeGlyphNames.Add('eshortdeva', 2318);
  FAdobeGlyphNames.Add('eshortvowelsigndeva', 2374);
  FAdobeGlyphNames.Add('eshreversedloop', 426);
  FAdobeGlyphNames.Add('eshsquatreversed', 645);
  FAdobeGlyphNames.Add('Esmall', 63333);
  FAdobeGlyphNames.Add('esmallhiragana', 12359);
  FAdobeGlyphNames.Add('esmallkatakana', 12455);
  FAdobeGlyphNames.Add('esmallkatakanahalfwidth', 65386);
  FAdobeGlyphNames.Add('estimated', 8494);
  FAdobeGlyphNames.Add('esuperior', 63212);
  FAdobeGlyphNames.Add('eta', 951);
  FAdobeGlyphNames.Add('Eta', 919);
  FAdobeGlyphNames.Add('etarmenian', 1384);
  FAdobeGlyphNames.Add('Etarmenian', 1336);
  FAdobeGlyphNames.Add('etatonos', 942);
  FAdobeGlyphNames.Add('Etatonos', 905);
  FAdobeGlyphNames.Add('eth', 240);
  FAdobeGlyphNames.Add('Eth', 208);
  FAdobeGlyphNames.Add('Ethsmall', 63472);
  FAdobeGlyphNames.Add('etilde', 7869);
  FAdobeGlyphNames.Add('Etilde', 7868);
  FAdobeGlyphNames.Add('etildebelow', 7707);
  FAdobeGlyphNames.Add('Etildebelow', 7706);
  FAdobeGlyphNames.Add('etnahtafoukhhebrew', 1425);
  FAdobeGlyphNames.Add('etnahtafoukhlefthebrew', 1425);
  FAdobeGlyphNames.Add('etnahtahebrew', 1425);
  FAdobeGlyphNames.Add('etnahtalefthebrew', 1425);
  FAdobeGlyphNames.Add('eturned', 477);
  FAdobeGlyphNames.Add('eukorean', 12641);
  FAdobeGlyphNames.Add('euro', 8364);
  FAdobeGlyphNames.Add('Euro', 8364);
  FAdobeGlyphNames.Add('evowelsignbengali', 2503);
  FAdobeGlyphNames.Add('evowelsigndeva', 2375);
  FAdobeGlyphNames.Add('evowelsigngujarati', 2759);
  FAdobeGlyphNames.Add('exclam', 33);
  FAdobeGlyphNames.Add('exclamarmenian', 1372);
  FAdobeGlyphNames.Add('exclamdbl', 8252);
  FAdobeGlyphNames.Add('exclamdown', 161);
  FAdobeGlyphNames.Add('exclamdownsmall', 63393);
  FAdobeGlyphNames.Add('exclammonospace', 65281);
  FAdobeGlyphNames.Add('exclamsmall', 63265);
  FAdobeGlyphNames.Add('existential', 8707);
  FAdobeGlyphNames.Add('ezh', 658);
  FAdobeGlyphNames.Add('Ezh', 439);
  FAdobeGlyphNames.Add('ezhcaron', 495);
  FAdobeGlyphNames.Add('Ezhcaron', 494);
  FAdobeGlyphNames.Add('ezhcurl', 659);
  FAdobeGlyphNames.Add('ezhreversed', 441);
  FAdobeGlyphNames.Add('Ezhreversed', 440);
  FAdobeGlyphNames.Add('ezhtail', 442);
  FAdobeGlyphNames.Add('f', 102);
  FAdobeGlyphNames.Add('F', 70);
  FAdobeGlyphNames.Add('fadeva', 2398);
  FAdobeGlyphNames.Add('fagurmukhi', 2654);
  FAdobeGlyphNames.Add('fahrenheit', 8457);
  FAdobeGlyphNames.Add('fathaarabic', 1614);
  FAdobeGlyphNames.Add('fathalowarabic', 1614);
  FAdobeGlyphNames.Add('fathatanarabic', 1611);
  FAdobeGlyphNames.Add('fbopomofo', 12552);
  FAdobeGlyphNames.Add('fcircle', 9429);
  FAdobeGlyphNames.Add('Fcircle', 9403);
  FAdobeGlyphNames.Add('fdotaccent', 7711);
  FAdobeGlyphNames.Add('Fdotaccent', 7710);
  FAdobeGlyphNames.Add('feharabic', 1601);
  FAdobeGlyphNames.Add('feharmenian', 1414);
  FAdobeGlyphNames.Add('Feharmenian', 1366);
  FAdobeGlyphNames.Add('fehfinalarabic', 65234);
  FAdobeGlyphNames.Add('fehinitialarabic', 65235);
  FAdobeGlyphNames.Add('fehmedialarabic', 65236);
  FAdobeGlyphNames.Add('feicoptic', 997);
  FAdobeGlyphNames.Add('Feicoptic', 996);
  FAdobeGlyphNames.Add('female', 9792);
  FAdobeGlyphNames.Add('ff', 64256);
  FAdobeGlyphNames.Add('ffi', 64259);
  FAdobeGlyphNames.Add('ffl', 64260);
  FAdobeGlyphNames.Add('Fhook', 401);
  FAdobeGlyphNames.Add('fi', 64257);
  FAdobeGlyphNames.Add('fifteencircle', 9326);
  FAdobeGlyphNames.Add('fifteenparen', 9346);
  FAdobeGlyphNames.Add('fifteenperiod', 9366);
  FAdobeGlyphNames.Add('figuredash', 8210);
  FAdobeGlyphNames.Add('filledbox', 9632);
  FAdobeGlyphNames.Add('filledrect', 9644);
  FAdobeGlyphNames.Add('finalkaf', 1498);
  FAdobeGlyphNames.Add('finalkafdagesh', 64314);
  FAdobeGlyphNames.Add('finalkafdageshhebrew', 64314);
  FAdobeGlyphNames.Add('finalkafhebrew', 1498);
  FAdobeGlyphNames.Add('finalkafqamats', 1498);
  FAdobeGlyphNames.Add('finalkafqamatshebrew', 1498);
  FAdobeGlyphNames.Add('finalkafsheva', 1498);
  FAdobeGlyphNames.Add('finalkafshevahebrew', 1498);
  FAdobeGlyphNames.Add('finalmem', 1501);
  FAdobeGlyphNames.Add('finalmemhebrew', 1501);
  FAdobeGlyphNames.Add('finalnun', 1503);
  FAdobeGlyphNames.Add('finalnunhebrew', 1503);
  FAdobeGlyphNames.Add('finalpe', 1507);
  FAdobeGlyphNames.Add('finalpehebrew', 1507);
  FAdobeGlyphNames.Add('finaltsadi', 1509);
  FAdobeGlyphNames.Add('finaltsadihebrew', 1509);
  FAdobeGlyphNames.Add('firsttonechinese', 713);
  FAdobeGlyphNames.Add('fisheye', 9673);
  FAdobeGlyphNames.Add('fitacyrillic', 1139);
  FAdobeGlyphNames.Add('Fitacyrillic', 1138);
  FAdobeGlyphNames.Add('five', 53);
  FAdobeGlyphNames.Add('fivearabic', 1637);
  FAdobeGlyphNames.Add('fivebengali', 2539);
  FAdobeGlyphNames.Add('fivecircle', 9316);
  FAdobeGlyphNames.Add('fivecircleinversesansserif', 10126);
  FAdobeGlyphNames.Add('fivedeva', 2411);
  FAdobeGlyphNames.Add('fiveeighths', 8541);
  FAdobeGlyphNames.Add('fivegujarati', 2795);
  FAdobeGlyphNames.Add('fivegurmukhi', 2667);
  FAdobeGlyphNames.Add('fivehackarabic', 1637);
  FAdobeGlyphNames.Add('fivehangzhou', 12325);
  FAdobeGlyphNames.Add('fiveideographicparen', 12836);
  FAdobeGlyphNames.Add('fiveinferior', 8325);
  FAdobeGlyphNames.Add('fivemonospace', 65301);
  FAdobeGlyphNames.Add('fiveoldstyle', 63285);
  FAdobeGlyphNames.Add('fiveparen', 9336);
  FAdobeGlyphNames.Add('fiveperiod', 9356);
  FAdobeGlyphNames.Add('fivepersian', 1781);
  FAdobeGlyphNames.Add('fiveroman', 8564);
  FAdobeGlyphNames.Add('Fiveroman', 8548);
  FAdobeGlyphNames.Add('fivesuperior', 8309);
  FAdobeGlyphNames.Add('fivethai', 3669);
  FAdobeGlyphNames.Add('fl', 64258);
  FAdobeGlyphNames.Add('florin', 402);
  FAdobeGlyphNames.Add('fmonospace', 65350);
  FAdobeGlyphNames.Add('Fmonospace', 65318);
  FAdobeGlyphNames.Add('fmsquare', 13209);
  FAdobeGlyphNames.Add('fofanthai', 3615);
  FAdobeGlyphNames.Add('fofathai', 3613);
  FAdobeGlyphNames.Add('fongmanthai', 3663);
  FAdobeGlyphNames.Add('forall', 8704);
  FAdobeGlyphNames.Add('four', 52);
  FAdobeGlyphNames.Add('fourarabic', 1636);
  FAdobeGlyphNames.Add('fourbengali', 2538);
  FAdobeGlyphNames.Add('fourcircle', 9315);
  FAdobeGlyphNames.Add('fourcircleinversesansserif', 10125);
  FAdobeGlyphNames.Add('fourdeva', 2410);
  FAdobeGlyphNames.Add('fourgujarati', 2794);
  FAdobeGlyphNames.Add('fourgurmukhi', 2666);
  FAdobeGlyphNames.Add('fourhackarabic', 1636);
  FAdobeGlyphNames.Add('fourhangzhou', 12324);
  FAdobeGlyphNames.Add('fourideographicparen', 12835);
  FAdobeGlyphNames.Add('fourinferior', 8324);
  FAdobeGlyphNames.Add('fourmonospace', 65300);
  FAdobeGlyphNames.Add('fournumeratorbengali', 2551);
  FAdobeGlyphNames.Add('fouroldstyle', 63284);
  FAdobeGlyphNames.Add('fourparen', 9335);
  FAdobeGlyphNames.Add('fourperiod', 9355);
  FAdobeGlyphNames.Add('fourpersian', 1780);
  FAdobeGlyphNames.Add('fourroman', 8563);
  FAdobeGlyphNames.Add('Fourroman', 8547);
  FAdobeGlyphNames.Add('foursuperior', 8308);
  FAdobeGlyphNames.Add('fourteencircle', 9325);
  FAdobeGlyphNames.Add('fourteenparen', 9345);
  FAdobeGlyphNames.Add('fourteenperiod', 9365);
  FAdobeGlyphNames.Add('fourthai', 3668);
  FAdobeGlyphNames.Add('fourthtonechinese', 715);
  FAdobeGlyphNames.Add('fparen', 9377);
  FAdobeGlyphNames.Add('fraction', 8260);
  FAdobeGlyphNames.Add('franc', 8355);
  FAdobeGlyphNames.Add('Fsmall', 63334);
  FAdobeGlyphNames.Add('g', 103);
  FAdobeGlyphNames.Add('G', 71);
  FAdobeGlyphNames.Add('gabengali', 2455);
  FAdobeGlyphNames.Add('gacute', 501);
  FAdobeGlyphNames.Add('Gacute', 500);
  FAdobeGlyphNames.Add('gadeva', 2327);
  FAdobeGlyphNames.Add('gafarabic', 1711);
  FAdobeGlyphNames.Add('gaffinalarabic', 64403);
  FAdobeGlyphNames.Add('gafinitialarabic', 64404);
  FAdobeGlyphNames.Add('gafmedialarabic', 64405);
  FAdobeGlyphNames.Add('gagujarati', 2711);
  FAdobeGlyphNames.Add('gagurmukhi', 2583);
  FAdobeGlyphNames.Add('gahiragana', 12364);
  FAdobeGlyphNames.Add('gakatakana', 12460);
  FAdobeGlyphNames.Add('gamma', 947);
  FAdobeGlyphNames.Add('Gamma', 915);
  FAdobeGlyphNames.Add('Gammaafrican', 404);
  FAdobeGlyphNames.Add('gammalatinsmall', 611);
  FAdobeGlyphNames.Add('gammasuperior', 736);
  FAdobeGlyphNames.Add('gangiacoptic', 1003);
  FAdobeGlyphNames.Add('Gangiacoptic', 1002);
  FAdobeGlyphNames.Add('gbopomofo', 12557);
  FAdobeGlyphNames.Add('gbreve', 287);
  FAdobeGlyphNames.Add('Gbreve', 286);
  FAdobeGlyphNames.Add('GBsquare', 13191);
  FAdobeGlyphNames.Add('gcaron', 487);
  FAdobeGlyphNames.Add('Gcaron', 486);
  FAdobeGlyphNames.Add('gcedilla', 291);
  FAdobeGlyphNames.Add('Gcedilla', 290);
  FAdobeGlyphNames.Add('gcircle', 9430);
  FAdobeGlyphNames.Add('Gcircle', 9404);
  FAdobeGlyphNames.Add('gcircumflex', 285);
  FAdobeGlyphNames.Add('Gcircumflex', 284);
  FAdobeGlyphNames.Add('gcommaaccent', 291);
  FAdobeGlyphNames.Add('Gcommaaccent', 290);
  FAdobeGlyphNames.Add('gdot', 289);
  FAdobeGlyphNames.Add('Gdot', 288);
  FAdobeGlyphNames.Add('gdotaccent', 289);
  FAdobeGlyphNames.Add('Gdotaccent', 288);
  FAdobeGlyphNames.Add('gecyrillic', 1075);
  FAdobeGlyphNames.Add('Gecyrillic', 1043);
  FAdobeGlyphNames.Add('gehiragana', 12370);
  FAdobeGlyphNames.Add('gekatakana', 12466);
  FAdobeGlyphNames.Add('geometricallyequal', 8785);
  FAdobeGlyphNames.Add('gereshaccenthebrew', 1436);
  FAdobeGlyphNames.Add('gereshhebrew', 1523);
  FAdobeGlyphNames.Add('gereshmuqdamhebrew', 1437);
  FAdobeGlyphNames.Add('germandbls', 223);
  FAdobeGlyphNames.Add('gershayimaccenthebrew', 1438);
  FAdobeGlyphNames.Add('gershayimhebrew', 1524);
  FAdobeGlyphNames.Add('getamark', 12307);
  FAdobeGlyphNames.Add('ghabengali', 2456);
  FAdobeGlyphNames.Add('ghadarmenian', 1394);
  FAdobeGlyphNames.Add('Ghadarmenian', 1346);
  FAdobeGlyphNames.Add('ghadeva', 2328);
  FAdobeGlyphNames.Add('ghagujarati', 2712);
  FAdobeGlyphNames.Add('ghagurmukhi', 2584);
  FAdobeGlyphNames.Add('ghainarabic', 1594);
  FAdobeGlyphNames.Add('ghainfinalarabic', 65230);
  FAdobeGlyphNames.Add('ghaininitialarabic', 65231);
  FAdobeGlyphNames.Add('ghainmedialarabic', 65232);
  FAdobeGlyphNames.Add('ghemiddlehookcyrillic', 1173);
  FAdobeGlyphNames.Add('Ghemiddlehookcyrillic', 1172);
  FAdobeGlyphNames.Add('ghestrokecyrillic', 1171);
  FAdobeGlyphNames.Add('Ghestrokecyrillic', 1170);
  FAdobeGlyphNames.Add('gheupturncyrillic', 1169);
  FAdobeGlyphNames.Add('Gheupturncyrillic', 1168);
  FAdobeGlyphNames.Add('ghhadeva', 2394);
  FAdobeGlyphNames.Add('ghhagurmukhi', 2650);
  FAdobeGlyphNames.Add('ghook', 608);
  FAdobeGlyphNames.Add('Ghook', 403);
  FAdobeGlyphNames.Add('ghzsquare', 13203);
  FAdobeGlyphNames.Add('gihiragana', 12366);
  FAdobeGlyphNames.Add('gikatakana', 12462);
  FAdobeGlyphNames.Add('gimarmenian', 1379);
  FAdobeGlyphNames.Add('Gimarmenian', 1331);
  FAdobeGlyphNames.Add('gimel', 1490);
  FAdobeGlyphNames.Add('gimeldagesh', 64306);
  FAdobeGlyphNames.Add('gimeldageshhebrew', 64306);
  FAdobeGlyphNames.Add('gimelhebrew', 1490);
  FAdobeGlyphNames.Add('gjecyrillic', 1107);
  FAdobeGlyphNames.Add('Gjecyrillic', 1027);
  FAdobeGlyphNames.Add('glottalinvertedstroke', 446);
  FAdobeGlyphNames.Add('glottalstop', 660);
  FAdobeGlyphNames.Add('glottalstopinverted', 662);
  FAdobeGlyphNames.Add('glottalstopmod', 704);
  FAdobeGlyphNames.Add('glottalstopreversed', 661);
  FAdobeGlyphNames.Add('glottalstopreversedmod', 705);
  FAdobeGlyphNames.Add('glottalstopreversedsuperior', 740);
  FAdobeGlyphNames.Add('glottalstopstroke', 673);
  FAdobeGlyphNames.Add('glottalstopstrokereversed', 674);
  FAdobeGlyphNames.Add('gmacron', 7713);
  FAdobeGlyphNames.Add('Gmacron', 7712);
  FAdobeGlyphNames.Add('gmonospace', 65351);
  FAdobeGlyphNames.Add('Gmonospace', 65319);
  FAdobeGlyphNames.Add('gohiragana', 12372);
  FAdobeGlyphNames.Add('gokatakana', 12468);
  FAdobeGlyphNames.Add('gparen', 9378);
  FAdobeGlyphNames.Add('gpasquare', 13228);
  FAdobeGlyphNames.Add('gradient', 8711);
  FAdobeGlyphNames.Add('grave', 96);
  FAdobeGlyphNames.Add('Grave', 63182);
  FAdobeGlyphNames.Add('gravebelowcmb', 790);
  FAdobeGlyphNames.Add('gravecmb', 768);
  FAdobeGlyphNames.Add('gravecomb', 768);
  FAdobeGlyphNames.Add('gravedeva', 2387);
  FAdobeGlyphNames.Add('gravelowmod', 718);
  FAdobeGlyphNames.Add('gravemonospace', 65344);
  FAdobeGlyphNames.Add('Gravesmall', 63328);
  FAdobeGlyphNames.Add('gravetonecmb', 832);
  FAdobeGlyphNames.Add('greater', 62);
  FAdobeGlyphNames.Add('greaterequal', 8805);
  FAdobeGlyphNames.Add('greaterequalorless', 8923);
  FAdobeGlyphNames.Add('greatermonospace', 65310);
  FAdobeGlyphNames.Add('greaterorequivalent', 8819);
  FAdobeGlyphNames.Add('greaterorless', 8823);
  FAdobeGlyphNames.Add('greateroverequal', 8807);
  FAdobeGlyphNames.Add('greatersmall', 65125);
  FAdobeGlyphNames.Add('gscript', 609);
  FAdobeGlyphNames.Add('Gsmall', 63335);
  FAdobeGlyphNames.Add('Gsmallhook', 667);
  FAdobeGlyphNames.Add('gstroke', 485);
  FAdobeGlyphNames.Add('Gstroke', 484);
  FAdobeGlyphNames.Add('guhiragana', 12368);
  FAdobeGlyphNames.Add('guillemotleft', 171);
  FAdobeGlyphNames.Add('guillemotright', 187);
  FAdobeGlyphNames.Add('guilsinglleft', 8249);
  FAdobeGlyphNames.Add('guilsinglright', 8250);
  FAdobeGlyphNames.Add('gukatakana', 12464);
  FAdobeGlyphNames.Add('guramusquare', 13080);
  FAdobeGlyphNames.Add('gysquare', 13257);
  FAdobeGlyphNames.Add('h', 104);
  FAdobeGlyphNames.Add('H', 72);
  FAdobeGlyphNames.Add('H18533', 9679);
  FAdobeGlyphNames.Add('H18543', 9642);
  FAdobeGlyphNames.Add('H18551', 9643);
  FAdobeGlyphNames.Add('H22073', 9633);
  FAdobeGlyphNames.Add('haabkhasiancyrillic', 1193);
  FAdobeGlyphNames.Add('Haabkhasiancyrillic', 1192);
  FAdobeGlyphNames.Add('haaltonearabic', 1729);
  FAdobeGlyphNames.Add('habengali', 2489);
  FAdobeGlyphNames.Add('hadescendercyrillic', 1203);
  FAdobeGlyphNames.Add('Hadescendercyrillic', 1202);
  FAdobeGlyphNames.Add('hadeva', 2361);
  FAdobeGlyphNames.Add('hagujarati', 2745);
  FAdobeGlyphNames.Add('hagurmukhi', 2617);
  FAdobeGlyphNames.Add('haharabic', 1581);
  FAdobeGlyphNames.Add('hahfinalarabic', 65186);
  FAdobeGlyphNames.Add('hahinitialarabic', 65187);
  FAdobeGlyphNames.Add('hahiragana', 12399);
  FAdobeGlyphNames.Add('hahmedialarabic', 65188);
  FAdobeGlyphNames.Add('haitusquare', 13098);
  FAdobeGlyphNames.Add('hakatakana', 12495);
  FAdobeGlyphNames.Add('hakatakanahalfwidth', 65418);
  FAdobeGlyphNames.Add('halantgurmukhi', 2637);
  FAdobeGlyphNames.Add('hamzaarabic', 1569);
  FAdobeGlyphNames.Add('hamzadammaarabic', 1569);
  FAdobeGlyphNames.Add('hamzadammatanarabic', 1569);
  FAdobeGlyphNames.Add('hamzafathaarabic', 1569);
  FAdobeGlyphNames.Add('hamzafathatanarabic', 1569);
  FAdobeGlyphNames.Add('hamzalowarabic', 1569);
  FAdobeGlyphNames.Add('hamzalowkasraarabic', 1569);
  FAdobeGlyphNames.Add('hamzalowkasratanarabic', 1569);
  FAdobeGlyphNames.Add('hamzasukunarabic', 1569);
  FAdobeGlyphNames.Add('hangulfiller', 12644);
  FAdobeGlyphNames.Add('hardsigncyrillic', 1098);
  FAdobeGlyphNames.Add('Hardsigncyrillic', 1066);
  FAdobeGlyphNames.Add('harpoonleftbarbup', 8636);
  FAdobeGlyphNames.Add('harpoonrightbarbup', 8640);
  FAdobeGlyphNames.Add('hasquare', 13258);
  FAdobeGlyphNames.Add('hatafpatah', 1458);
  FAdobeGlyphNames.Add('hatafpatah16', 1458);
  FAdobeGlyphNames.Add('hatafpatah23', 1458);
  FAdobeGlyphNames.Add('hatafpatah2f', 1458);
  FAdobeGlyphNames.Add('hatafpatahhebrew', 1458);
  FAdobeGlyphNames.Add('hatafpatahnarrowhebrew', 1458);
  FAdobeGlyphNames.Add('hatafpatahquarterhebrew', 1458);
  FAdobeGlyphNames.Add('hatafpatahwidehebrew', 1458);
  FAdobeGlyphNames.Add('hatafqamats', 1459);
  FAdobeGlyphNames.Add('hatafqamats1b', 1459);
  FAdobeGlyphNames.Add('hatafqamats28', 1459);
  FAdobeGlyphNames.Add('hatafqamats34', 1459);
  FAdobeGlyphNames.Add('hatafqamatshebrew', 1459);
  FAdobeGlyphNames.Add('hatafqamatsnarrowhebrew', 1459);
  FAdobeGlyphNames.Add('hatafqamatsquarterhebrew', 1459);
  FAdobeGlyphNames.Add('hatafqamatswidehebrew', 1459);
  FAdobeGlyphNames.Add('hatafsegol', 1457);
  FAdobeGlyphNames.Add('hatafsegol17', 1457);
  FAdobeGlyphNames.Add('hatafsegol24', 1457);
  FAdobeGlyphNames.Add('hatafsegol30', 1457);
  FAdobeGlyphNames.Add('hatafsegolhebrew', 1457);
  FAdobeGlyphNames.Add('hatafsegolnarrowhebrew', 1457);
  FAdobeGlyphNames.Add('hatafsegolquarterhebrew', 1457);
  FAdobeGlyphNames.Add('hatafsegolwidehebrew', 1457);
  FAdobeGlyphNames.Add('hbar', 295);
  FAdobeGlyphNames.Add('Hbar', 294);
  FAdobeGlyphNames.Add('hbopomofo', 12559);
  FAdobeGlyphNames.Add('hbrevebelow', 7723);
  FAdobeGlyphNames.Add('Hbrevebelow', 7722);
  FAdobeGlyphNames.Add('hcedilla', 7721);
  FAdobeGlyphNames.Add('Hcedilla', 7720);
  FAdobeGlyphNames.Add('hcircle', 9431);
  FAdobeGlyphNames.Add('Hcircle', 9405);
  FAdobeGlyphNames.Add('hcircumflex', 293);
  FAdobeGlyphNames.Add('Hcircumflex', 292);
  FAdobeGlyphNames.Add('hdieresis', 7719);
  FAdobeGlyphNames.Add('Hdieresis', 7718);
  FAdobeGlyphNames.Add('hdotaccent', 7715);
  FAdobeGlyphNames.Add('Hdotaccent', 7714);
  FAdobeGlyphNames.Add('hdotbelow', 7717);
  FAdobeGlyphNames.Add('Hdotbelow', 7716);
  FAdobeGlyphNames.Add('he', 1492);
  FAdobeGlyphNames.Add('heart', 9829);
  FAdobeGlyphNames.Add('heartsuitblack', 9829);
  FAdobeGlyphNames.Add('heartsuitwhite', 9825);
  FAdobeGlyphNames.Add('hedagesh', 64308);
  FAdobeGlyphNames.Add('hedageshhebrew', 64308);
  FAdobeGlyphNames.Add('hehaltonearabic', 1729);
  FAdobeGlyphNames.Add('heharabic', 1607);
  FAdobeGlyphNames.Add('hehebrew', 1492);
  FAdobeGlyphNames.Add('hehfinalaltonearabic', 64423);
  FAdobeGlyphNames.Add('hehfinalalttwoarabic', 65258);
  FAdobeGlyphNames.Add('hehfinalarabic', 65258);
  FAdobeGlyphNames.Add('hehhamzaabovefinalarabic', 64421);
  FAdobeGlyphNames.Add('hehhamzaaboveisolatedarabic', 64420);
  FAdobeGlyphNames.Add('hehinitialaltonearabic', 64424);
  FAdobeGlyphNames.Add('hehinitialarabic', 65259);
  FAdobeGlyphNames.Add('hehiragana', 12408);
  FAdobeGlyphNames.Add('hehmedialaltonearabic', 64425);
  FAdobeGlyphNames.Add('hehmedialarabic', 65260);
  FAdobeGlyphNames.Add('heiseierasquare', 13179);
  FAdobeGlyphNames.Add('hekatakana', 12504);
  FAdobeGlyphNames.Add('hekatakanahalfwidth', 65421);
  FAdobeGlyphNames.Add('hekutaarusquare', 13110);
  FAdobeGlyphNames.Add('henghook', 615);
  FAdobeGlyphNames.Add('herutusquare', 13113);
  FAdobeGlyphNames.Add('het', 1495);
  FAdobeGlyphNames.Add('hethebrew', 1495);
  FAdobeGlyphNames.Add('hhook', 614);
  FAdobeGlyphNames.Add('hhooksuperior', 689);
  FAdobeGlyphNames.Add('hieuhacirclekorean', 12923);
  FAdobeGlyphNames.Add('hieuhaparenkorean', 12827);
  FAdobeGlyphNames.Add('hieuhcirclekorean', 12909);
  FAdobeGlyphNames.Add('hieuhkorean', 12622);
  FAdobeGlyphNames.Add('hieuhparenkorean', 12813);
  FAdobeGlyphNames.Add('hihiragana', 12402);
  FAdobeGlyphNames.Add('hikatakana', 12498);
  FAdobeGlyphNames.Add('hikatakanahalfwidth', 65419);
  FAdobeGlyphNames.Add('hiriq', 1460);
  FAdobeGlyphNames.Add('hiriq14', 1460);
  FAdobeGlyphNames.Add('hiriq21', 1460);
  FAdobeGlyphNames.Add('hiriq2d', 1460);
  FAdobeGlyphNames.Add('hiriqhebrew', 1460);
  FAdobeGlyphNames.Add('hiriqnarrowhebrew', 1460);
  FAdobeGlyphNames.Add('hiriqquarterhebrew', 1460);
  FAdobeGlyphNames.Add('hiriqwidehebrew', 1460);
  FAdobeGlyphNames.Add('hlinebelow', 7830);
  FAdobeGlyphNames.Add('hmonospace', 65352);
  FAdobeGlyphNames.Add('Hmonospace', 65320);
  FAdobeGlyphNames.Add('hoarmenian', 1392);
  FAdobeGlyphNames.Add('Hoarmenian', 1344);
  FAdobeGlyphNames.Add('hohipthai', 3627);
  FAdobeGlyphNames.Add('hohiragana', 12411);
  FAdobeGlyphNames.Add('hokatakana', 12507);
  FAdobeGlyphNames.Add('hokatakanahalfwidth', 65422);
  FAdobeGlyphNames.Add('holam', 1465);
  FAdobeGlyphNames.Add('holam19', 1465);
  FAdobeGlyphNames.Add('holam26', 1465);
  FAdobeGlyphNames.Add('holam32', 1465);
  FAdobeGlyphNames.Add('holamhebrew', 1465);
  FAdobeGlyphNames.Add('holamnarrowhebrew', 1465);
  FAdobeGlyphNames.Add('holamquarterhebrew', 1465);
  FAdobeGlyphNames.Add('holamwidehebrew', 1465);
  FAdobeGlyphNames.Add('honokhukthai', 3630);
  FAdobeGlyphNames.Add('hookabovecomb', 777);
  FAdobeGlyphNames.Add('hookcmb', 777);
  FAdobeGlyphNames.Add('hookpalatalizedbelowcmb', 801);
  FAdobeGlyphNames.Add('hookretroflexbelowcmb', 802);
  FAdobeGlyphNames.Add('hoonsquare', 13122);
  FAdobeGlyphNames.Add('horicoptic', 1001);
  FAdobeGlyphNames.Add('Horicoptic', 1000);
  FAdobeGlyphNames.Add('horizontalbar', 8213);
  FAdobeGlyphNames.Add('horncmb', 795);
  FAdobeGlyphNames.Add('hotsprings', 9832);
  FAdobeGlyphNames.Add('house', 8962);
  FAdobeGlyphNames.Add('hparen', 9379);
  FAdobeGlyphNames.Add('HPsquare', 13259);
  FAdobeGlyphNames.Add('Hsmall', 63336);
  FAdobeGlyphNames.Add('hsuperior', 688);
  FAdobeGlyphNames.Add('hturned', 613);
  FAdobeGlyphNames.Add('huhiragana', 12405);
  FAdobeGlyphNames.Add('huiitosquare', 13107);
  FAdobeGlyphNames.Add('hukatakana', 12501);
  FAdobeGlyphNames.Add('hukatakanahalfwidth', 65420);
  FAdobeGlyphNames.Add('hungarumlaut', 733);
  FAdobeGlyphNames.Add('Hungarumlaut', 63183);
  FAdobeGlyphNames.Add('hungarumlautcmb', 779);
  FAdobeGlyphNames.Add('Hungarumlautsmall', 63224);
  FAdobeGlyphNames.Add('hv', 405);
  FAdobeGlyphNames.Add('hyphen', 45);
  FAdobeGlyphNames.Add('hypheninferior', 63205);
  FAdobeGlyphNames.Add('hyphenmonospace', 65293);
  FAdobeGlyphNames.Add('hyphensmall', 65123);
  FAdobeGlyphNames.Add('hyphensuperior', 63206);
  FAdobeGlyphNames.Add('hyphentwo', 8208);
  FAdobeGlyphNames.Add('Hzsquare', 13200);
  FAdobeGlyphNames.Add('i', 105);
  FAdobeGlyphNames.Add('I', 73);
  FAdobeGlyphNames.Add('iacute', 237);
  FAdobeGlyphNames.Add('Iacute', 205);
  FAdobeGlyphNames.Add('Iacutesmall', 63469);
  FAdobeGlyphNames.Add('iacyrillic', 1103);
  FAdobeGlyphNames.Add('IAcyrillic', 1071);
  FAdobeGlyphNames.Add('ibengali', 2439);
  FAdobeGlyphNames.Add('ibopomofo', 12583);
  FAdobeGlyphNames.Add('ibreve', 301);
  FAdobeGlyphNames.Add('Ibreve', 300);
  FAdobeGlyphNames.Add('icaron', 464);
end;

procedure TdxFontFileUnicodeConverter.InitializeAdobeGlyphNames2;
begin
  FAdobeGlyphNames.Add('Icaron', 463);
  FAdobeGlyphNames.Add('icircle', 9432);
  FAdobeGlyphNames.Add('Icircle', 9406);
  FAdobeGlyphNames.Add('icircumflex', 238);
  FAdobeGlyphNames.Add('Icircumflex', 206);
  FAdobeGlyphNames.Add('Icircumflexsmall', 63470);
  FAdobeGlyphNames.Add('icyrillic', 1110);
  FAdobeGlyphNames.Add('Icyrillic', 1030);
  FAdobeGlyphNames.Add('idblgrave', 521);
  FAdobeGlyphNames.Add('Idblgrave', 520);
  FAdobeGlyphNames.Add('ideographearthcircle', 12943);
  FAdobeGlyphNames.Add('ideographfirecircle', 12939);
  FAdobeGlyphNames.Add('ideographicallianceparen', 12863);
  FAdobeGlyphNames.Add('ideographiccallparen', 12858);
  FAdobeGlyphNames.Add('ideographiccentrecircle', 12965);
  FAdobeGlyphNames.Add('ideographicclose', 12294);
  FAdobeGlyphNames.Add('ideographiccomma', 12289);
  FAdobeGlyphNames.Add('ideographiccommaleft', 65380);
  FAdobeGlyphNames.Add('ideographiccongratulationparen', 12855);
  FAdobeGlyphNames.Add('ideographiccorrectcircle', 12963);
  FAdobeGlyphNames.Add('ideographicearthparen', 12847);
  FAdobeGlyphNames.Add('ideographicenterpriseparen', 12861);
  FAdobeGlyphNames.Add('ideographicexcellentcircle', 12957);
  FAdobeGlyphNames.Add('ideographicfestivalparen', 12864);
  FAdobeGlyphNames.Add('ideographicfinancialcircle', 12950);
  FAdobeGlyphNames.Add('ideographicfinancialparen', 12854);
  FAdobeGlyphNames.Add('ideographicfireparen', 12843);
  FAdobeGlyphNames.Add('ideographichaveparen', 12850);
  FAdobeGlyphNames.Add('ideographichighcircle', 12964);
  FAdobeGlyphNames.Add('ideographiciterationmark', 12293);
  FAdobeGlyphNames.Add('ideographiclaborcircle', 12952);
  FAdobeGlyphNames.Add('ideographiclaborparen', 12856);
  FAdobeGlyphNames.Add('ideographicleftcircle', 12967);
  FAdobeGlyphNames.Add('ideographiclowcircle', 12966);
  FAdobeGlyphNames.Add('ideographicmedicinecircle', 12969);
  FAdobeGlyphNames.Add('ideographicmetalparen', 12846);
  FAdobeGlyphNames.Add('ideographicmoonparen', 12842);
  FAdobeGlyphNames.Add('ideographicnameparen', 12852);
  FAdobeGlyphNames.Add('ideographicperiod', 12290);
  FAdobeGlyphNames.Add('ideographicprintcircle', 12958);
  FAdobeGlyphNames.Add('ideographicreachparen', 12867);
  FAdobeGlyphNames.Add('ideographicrepresentparen', 12857);
  FAdobeGlyphNames.Add('ideographicresourceparen', 12862);
  FAdobeGlyphNames.Add('ideographicrightcircle', 12968);
  FAdobeGlyphNames.Add('ideographicsecretcircle', 12953);
  FAdobeGlyphNames.Add('ideographicselfparen', 12866);
  FAdobeGlyphNames.Add('ideographicsocietyparen', 12851);
  FAdobeGlyphNames.Add('ideographicspace', 12288);
  FAdobeGlyphNames.Add('ideographicspecialparen', 12853);
  FAdobeGlyphNames.Add('ideographicstockparen', 12849);
  FAdobeGlyphNames.Add('ideographicstudyparen', 12859);
  FAdobeGlyphNames.Add('ideographicsunparen', 12848);
  FAdobeGlyphNames.Add('ideographicsuperviseparen', 12860);
  FAdobeGlyphNames.Add('ideographicwaterparen', 12844);
  FAdobeGlyphNames.Add('ideographicwoodparen', 12845);
  FAdobeGlyphNames.Add('ideographiczero', 12295);
  FAdobeGlyphNames.Add('ideographmetalcircle', 12942);
  FAdobeGlyphNames.Add('ideographmooncircle', 12938);
  FAdobeGlyphNames.Add('ideographnamecircle', 12948);
  FAdobeGlyphNames.Add('ideographsuncircle', 12944);
  FAdobeGlyphNames.Add('ideographwatercircle', 12940);
  FAdobeGlyphNames.Add('ideographwoodcircle', 12941);
  FAdobeGlyphNames.Add('ideva', 2311);
  FAdobeGlyphNames.Add('idieresis', 239);
  FAdobeGlyphNames.Add('Idieresis', 207);
  FAdobeGlyphNames.Add('idieresisacute', 7727);
  FAdobeGlyphNames.Add('Idieresisacute', 7726);
  FAdobeGlyphNames.Add('idieresiscyrillic', 1253);
  FAdobeGlyphNames.Add('Idieresiscyrillic', 1252);
  FAdobeGlyphNames.Add('Idieresissmall', 63471);
  FAdobeGlyphNames.Add('Idot', 304);
  FAdobeGlyphNames.Add('Idotaccent', 304);
  FAdobeGlyphNames.Add('idotbelow', 7883);
  FAdobeGlyphNames.Add('Idotbelow', 7882);
  FAdobeGlyphNames.Add('iebrevecyrillic', 1239);
  FAdobeGlyphNames.Add('Iebrevecyrillic', 1238);
  FAdobeGlyphNames.Add('iecyrillic', 1077);
  FAdobeGlyphNames.Add('Iecyrillic', 1045);
  FAdobeGlyphNames.Add('ieungacirclekorean', 12917);
  FAdobeGlyphNames.Add('ieungaparenkorean', 12821);
  FAdobeGlyphNames.Add('ieungcirclekorean', 12903);
  FAdobeGlyphNames.Add('ieungkorean', 12615);
  FAdobeGlyphNames.Add('ieungparenkorean', 12807);
  FAdobeGlyphNames.Add('Ifraktur', 8465);
  FAdobeGlyphNames.Add('igrave', 236);
  FAdobeGlyphNames.Add('Igrave', 204);
  FAdobeGlyphNames.Add('Igravesmall', 63468);
  FAdobeGlyphNames.Add('igujarati', 2695);
  FAdobeGlyphNames.Add('igurmukhi', 2567);
  FAdobeGlyphNames.Add('ihiragana', 12356);
  FAdobeGlyphNames.Add('ihookabove', 7881);
  FAdobeGlyphNames.Add('Ihookabove', 7880);
  FAdobeGlyphNames.Add('iibengali', 2440);
  FAdobeGlyphNames.Add('iicyrillic', 1080);
  FAdobeGlyphNames.Add('Iicyrillic', 1048);
  FAdobeGlyphNames.Add('iideva', 2312);
  FAdobeGlyphNames.Add('iigujarati', 2696);
  FAdobeGlyphNames.Add('iigurmukhi', 2568);
  FAdobeGlyphNames.Add('iimatragurmukhi', 2624);
  FAdobeGlyphNames.Add('iinvertedbreve', 523);
  FAdobeGlyphNames.Add('Iinvertedbreve', 522);
  FAdobeGlyphNames.Add('iishortcyrillic', 1081);
  FAdobeGlyphNames.Add('Iishortcyrillic', 1049);
  FAdobeGlyphNames.Add('iivowelsignbengali', 2496);
  FAdobeGlyphNames.Add('iivowelsigndeva', 2368);
  FAdobeGlyphNames.Add('iivowelsigngujarati', 2752);
  FAdobeGlyphNames.Add('ij', 307);
  FAdobeGlyphNames.Add('IJ', 306);
  FAdobeGlyphNames.Add('ikatakana', 12452);
  FAdobeGlyphNames.Add('ikatakanahalfwidth', 65394);
  FAdobeGlyphNames.Add('ikorean', 12643);
  FAdobeGlyphNames.Add('ilde', 732);
  FAdobeGlyphNames.Add('iluyhebrew', 1452);
  FAdobeGlyphNames.Add('imacron', 299);
  FAdobeGlyphNames.Add('Imacron', 298);
  FAdobeGlyphNames.Add('imacroncyrillic', 1251);
  FAdobeGlyphNames.Add('Imacroncyrillic', 1250);
  FAdobeGlyphNames.Add('imageorapproximatelyequal', 8787);
  FAdobeGlyphNames.Add('imatragurmukhi', 2623);
  FAdobeGlyphNames.Add('imonospace', 65353);
  FAdobeGlyphNames.Add('Imonospace', 65321);
  FAdobeGlyphNames.Add('increment', 8710);
  FAdobeGlyphNames.Add('infinity', 8734);
  FAdobeGlyphNames.Add('iniarmenian', 1387);
  FAdobeGlyphNames.Add('Iniarmenian', 1339);
  FAdobeGlyphNames.Add('integral', 8747);
  FAdobeGlyphNames.Add('integralbottom', 8993);
  FAdobeGlyphNames.Add('integralbt', 8993);
  FAdobeGlyphNames.Add('integralex', 63733);
  FAdobeGlyphNames.Add('integraltop', 8992);
  FAdobeGlyphNames.Add('integraltp', 8992);
  FAdobeGlyphNames.Add('intersection', 8745);
  FAdobeGlyphNames.Add('intisquare', 13061);
  FAdobeGlyphNames.Add('invbullet', 9688);
  FAdobeGlyphNames.Add('invcircle', 9689);
  FAdobeGlyphNames.Add('invsmileface', 9787);
  FAdobeGlyphNames.Add('iocyrillic', 1105);
  FAdobeGlyphNames.Add('Iocyrillic', 1025);
  FAdobeGlyphNames.Add('iogonek', 303);
  FAdobeGlyphNames.Add('Iogonek', 302);
  FAdobeGlyphNames.Add('iota', 953);
  FAdobeGlyphNames.Add('Iota', 921);
  FAdobeGlyphNames.Add('Iotaafrican', 406);
  FAdobeGlyphNames.Add('iotadieresis', 970);
  FAdobeGlyphNames.Add('Iotadieresis', 938);
  FAdobeGlyphNames.Add('iotadieresistonos', 912);
  FAdobeGlyphNames.Add('iotalatin', 617);
  FAdobeGlyphNames.Add('iotatonos', 943);
  FAdobeGlyphNames.Add('Iotatonos', 906);
  FAdobeGlyphNames.Add('iparen', 9380);
  FAdobeGlyphNames.Add('irigurmukhi', 2674);
  FAdobeGlyphNames.Add('Ismall', 63337);
  FAdobeGlyphNames.Add('ismallhiragana', 12355);
  FAdobeGlyphNames.Add('ismallkatakana', 12451);
  FAdobeGlyphNames.Add('ismallkatakanahalfwidth', 65384);
  FAdobeGlyphNames.Add('issharbengali', 2554);
  FAdobeGlyphNames.Add('istroke', 616);
  FAdobeGlyphNames.Add('Istroke', 407);
  FAdobeGlyphNames.Add('isuperior', 63213);
  FAdobeGlyphNames.Add('iterationhiragana', 12445);
  FAdobeGlyphNames.Add('iterationkatakana', 12541);
  FAdobeGlyphNames.Add('itilde', 297);
  FAdobeGlyphNames.Add('Itilde', 296);
  FAdobeGlyphNames.Add('itildebelow', 7725);
  FAdobeGlyphNames.Add('Itildebelow', 7724);
  FAdobeGlyphNames.Add('iubopomofo', 12585);
  FAdobeGlyphNames.Add('iucyrillic', 1102);
  FAdobeGlyphNames.Add('IUcyrillic', 1070);
  FAdobeGlyphNames.Add('ivowelsignbengali', 2495);
  FAdobeGlyphNames.Add('ivowelsigndeva', 2367);
  FAdobeGlyphNames.Add('ivowelsigngujarati', 2751);
  FAdobeGlyphNames.Add('izhitsacyrillic', 1141);
  FAdobeGlyphNames.Add('Izhitsacyrillic', 1140);
  FAdobeGlyphNames.Add('izhitsadblgravecyrillic', 1143);
  FAdobeGlyphNames.Add('Izhitsadblgravecyrillic', 1142);
  FAdobeGlyphNames.Add('j', 106);
  FAdobeGlyphNames.Add('J', 74);
  FAdobeGlyphNames.Add('jaarmenian', 1393);
  FAdobeGlyphNames.Add('Jaarmenian', 1345);
  FAdobeGlyphNames.Add('jabengali', 2460);
  FAdobeGlyphNames.Add('jadeva', 2332);
  FAdobeGlyphNames.Add('jagujarati', 2716);
  FAdobeGlyphNames.Add('jagurmukhi', 2588);
  FAdobeGlyphNames.Add('jbopomofo', 12560);
  FAdobeGlyphNames.Add('jcaron', 496);
  FAdobeGlyphNames.Add('jcircle', 9433);
  FAdobeGlyphNames.Add('Jcircle', 9407);
  FAdobeGlyphNames.Add('jcircumflex', 309);
  FAdobeGlyphNames.Add('Jcircumflex', 308);
  FAdobeGlyphNames.Add('jcrossedtail', 669);
  FAdobeGlyphNames.Add('jdotlessstroke', 607);
  FAdobeGlyphNames.Add('jecyrillic', 1112);
  FAdobeGlyphNames.Add('Jecyrillic', 1032);
  FAdobeGlyphNames.Add('jeemarabic', 1580);
  FAdobeGlyphNames.Add('jeemfinalarabic', 65182);
  FAdobeGlyphNames.Add('jeeminitialarabic', 65183);
  FAdobeGlyphNames.Add('jeemmedialarabic', 65184);
  FAdobeGlyphNames.Add('jeharabic', 1688);
  FAdobeGlyphNames.Add('jehfinalarabic', 64395);
  FAdobeGlyphNames.Add('jhabengali', 2461);
  FAdobeGlyphNames.Add('jhadeva', 2333);
  FAdobeGlyphNames.Add('jhagujarati', 2717);
  FAdobeGlyphNames.Add('jhagurmukhi', 2589);
  FAdobeGlyphNames.Add('jheharmenian', 1403);
  FAdobeGlyphNames.Add('Jheharmenian', 1355);
  FAdobeGlyphNames.Add('jis', 12292);
  FAdobeGlyphNames.Add('jmonospace', 65354);
  FAdobeGlyphNames.Add('Jmonospace', 65322);
  FAdobeGlyphNames.Add('jparen', 9381);
  FAdobeGlyphNames.Add('Jsmall', 63338);
  FAdobeGlyphNames.Add('jsuperior', 690);
  FAdobeGlyphNames.Add('k', 107);
  FAdobeGlyphNames.Add('K', 75);
  FAdobeGlyphNames.Add('kabashkircyrillic', 1185);
  FAdobeGlyphNames.Add('Kabashkircyrillic', 1184);
  FAdobeGlyphNames.Add('kabengali', 2453);
  FAdobeGlyphNames.Add('kacute', 7729);
  FAdobeGlyphNames.Add('Kacute', 7728);
  FAdobeGlyphNames.Add('kacyrillic', 1082);
  FAdobeGlyphNames.Add('Kacyrillic', 1050);
  FAdobeGlyphNames.Add('kadescendercyrillic', 1179);
  FAdobeGlyphNames.Add('Kadescendercyrillic', 1178);
  FAdobeGlyphNames.Add('kadeva', 2325);
  FAdobeGlyphNames.Add('kaf', 1499);
  FAdobeGlyphNames.Add('kafarabic', 1603);
  FAdobeGlyphNames.Add('kafdagesh', 64315);
  FAdobeGlyphNames.Add('kafdageshhebrew', 64315);
  FAdobeGlyphNames.Add('kaffinalarabic', 65242);
  FAdobeGlyphNames.Add('kafhebrew', 1499);
  FAdobeGlyphNames.Add('kafinitialarabic', 65243);
  FAdobeGlyphNames.Add('kafmedialarabic', 65244);
  FAdobeGlyphNames.Add('kafrafehebrew', 64333);
  FAdobeGlyphNames.Add('kagujarati', 2709);
  FAdobeGlyphNames.Add('kagurmukhi', 2581);
  FAdobeGlyphNames.Add('kahiragana', 12363);
  FAdobeGlyphNames.Add('kahookcyrillic', 1220);
  FAdobeGlyphNames.Add('Kahookcyrillic', 1219);
  FAdobeGlyphNames.Add('kakatakana', 12459);
  FAdobeGlyphNames.Add('kakatakanahalfwidth', 65398);
  FAdobeGlyphNames.Add('kappa', 954);
  FAdobeGlyphNames.Add('Kappa', 922);
  FAdobeGlyphNames.Add('kappasymbolgreek', 1008);
  FAdobeGlyphNames.Add('kapyeounmieumkorean', 12657);
  FAdobeGlyphNames.Add('kapyeounphieuphkorean', 12676);
  FAdobeGlyphNames.Add('kapyeounpieupkorean', 12664);
  FAdobeGlyphNames.Add('kapyeounssangpieupkorean', 12665);
  FAdobeGlyphNames.Add('karoriisquare', 13069);
  FAdobeGlyphNames.Add('kashidaautoarabic', 1600);
  FAdobeGlyphNames.Add('kashidaautonosidebearingarabic', 1600);
  FAdobeGlyphNames.Add('kasmallkatakana', 12533);
  FAdobeGlyphNames.Add('kasquare', 13188);
  FAdobeGlyphNames.Add('kasraarabic', 1616);
  FAdobeGlyphNames.Add('kasratanarabic', 1613);
  FAdobeGlyphNames.Add('kastrokecyrillic', 1183);
  FAdobeGlyphNames.Add('Kastrokecyrillic', 1182);
  FAdobeGlyphNames.Add('katahiraprolongmarkhalfwidth', 65392);
  FAdobeGlyphNames.Add('kaverticalstrokecyrillic', 1181);
  FAdobeGlyphNames.Add('Kaverticalstrokecyrillic', 1180);
  FAdobeGlyphNames.Add('kbopomofo', 12558);
  FAdobeGlyphNames.Add('KBsquare', 13189);
  FAdobeGlyphNames.Add('kcalsquare', 13193);
  FAdobeGlyphNames.Add('kcaron', 489);
  FAdobeGlyphNames.Add('Kcaron', 488);
  FAdobeGlyphNames.Add('kcedilla', 311);
  FAdobeGlyphNames.Add('Kcedilla', 310);
  FAdobeGlyphNames.Add('kcircle', 9434);
  FAdobeGlyphNames.Add('Kcircle', 9408);
  FAdobeGlyphNames.Add('kcommaaccent', 311);
  FAdobeGlyphNames.Add('Kcommaaccent', 310);
  FAdobeGlyphNames.Add('kdotbelow', 7731);
  FAdobeGlyphNames.Add('Kdotbelow', 7730);
  FAdobeGlyphNames.Add('keharmenian', 1412);
  FAdobeGlyphNames.Add('Keharmenian', 1364);
  FAdobeGlyphNames.Add('kehiragana', 12369);
  FAdobeGlyphNames.Add('kekatakana', 12465);
  FAdobeGlyphNames.Add('kekatakanahalfwidth', 65401);
  FAdobeGlyphNames.Add('kenarmenian', 1391);
  FAdobeGlyphNames.Add('Kenarmenian', 1343);
  FAdobeGlyphNames.Add('kesmallkatakana', 12534);
  FAdobeGlyphNames.Add('kgreenlandic', 312);
  FAdobeGlyphNames.Add('khabengali', 2454);
  FAdobeGlyphNames.Add('khacyrillic', 1093);
  FAdobeGlyphNames.Add('Khacyrillic', 1061);
  FAdobeGlyphNames.Add('khadeva', 2326);
  FAdobeGlyphNames.Add('khagujarati', 2710);
  FAdobeGlyphNames.Add('khagurmukhi', 2582);
  FAdobeGlyphNames.Add('khaharabic', 1582);
  FAdobeGlyphNames.Add('khahfinalarabic', 65190);
  FAdobeGlyphNames.Add('khahinitialarabic', 65191);
  FAdobeGlyphNames.Add('khahmedialarabic', 65192);
  FAdobeGlyphNames.Add('kheicoptic', 999);
  FAdobeGlyphNames.Add('Kheicoptic', 998);
  FAdobeGlyphNames.Add('khhadeva', 2393);
  FAdobeGlyphNames.Add('khhagurmukhi', 2649);
  FAdobeGlyphNames.Add('khieukhacirclekorean', 12920);
  FAdobeGlyphNames.Add('khieukhaparenkorean', 12824);
  FAdobeGlyphNames.Add('khieukhcirclekorean', 12906);
  FAdobeGlyphNames.Add('khieukhkorean', 12619);
  FAdobeGlyphNames.Add('khieukhparenkorean', 12810);
  FAdobeGlyphNames.Add('khokhaithai', 3586);
  FAdobeGlyphNames.Add('khokhonthai', 3589);
  FAdobeGlyphNames.Add('khokhuatthai', 3587);
  FAdobeGlyphNames.Add('khokhwaithai', 3588);
  FAdobeGlyphNames.Add('khomutthai', 3675);
  FAdobeGlyphNames.Add('khook', 409);
  FAdobeGlyphNames.Add('Khook', 408);
  FAdobeGlyphNames.Add('khorakhangthai', 3590);
  FAdobeGlyphNames.Add('khzsquare', 13201);
  FAdobeGlyphNames.Add('kihiragana', 12365);
  FAdobeGlyphNames.Add('kikatakana', 12461);
  FAdobeGlyphNames.Add('kikatakanahalfwidth', 65399);
  FAdobeGlyphNames.Add('kiroguramusquare', 13077);
  FAdobeGlyphNames.Add('kiromeetorusquare', 13078);
  FAdobeGlyphNames.Add('kirosquare', 13076);
  FAdobeGlyphNames.Add('kiyeokacirclekorean', 12910);
  FAdobeGlyphNames.Add('kiyeokaparenkorean', 12814);
  FAdobeGlyphNames.Add('kiyeokcirclekorean', 12896);
  FAdobeGlyphNames.Add('kiyeokkorean', 12593);
  FAdobeGlyphNames.Add('kiyeokparenkorean', 12800);
  FAdobeGlyphNames.Add('kiyeoksioskorean', 12595);
  FAdobeGlyphNames.Add('kjecyrillic', 1116);
  FAdobeGlyphNames.Add('Kjecyrillic', 1036);
  FAdobeGlyphNames.Add('KKsquare', 13261);
  FAdobeGlyphNames.Add('klinebelow', 7733);
  FAdobeGlyphNames.Add('Klinebelow', 7732);
  FAdobeGlyphNames.Add('klsquare', 13208);
  FAdobeGlyphNames.Add('kmcubedsquare', 13222);
  FAdobeGlyphNames.Add('kmonospace', 65355);
  FAdobeGlyphNames.Add('Kmonospace', 65323);
  FAdobeGlyphNames.Add('kmsquaredsquare', 13218);
  FAdobeGlyphNames.Add('kohiragana', 12371);
  FAdobeGlyphNames.Add('kohmsquare', 13248);
  FAdobeGlyphNames.Add('kokaithai', 3585);
  FAdobeGlyphNames.Add('kokatakana', 12467);
  FAdobeGlyphNames.Add('kokatakanahalfwidth', 65402);
  FAdobeGlyphNames.Add('kooposquare', 13086);
  FAdobeGlyphNames.Add('koppacyrillic', 1153);
  FAdobeGlyphNames.Add('Koppacyrillic', 1152);
  FAdobeGlyphNames.Add('Koppagreek', 990);
  FAdobeGlyphNames.Add('koreanstandardsymbol', 12927);
  FAdobeGlyphNames.Add('koroniscmb', 835);
  FAdobeGlyphNames.Add('kparen', 9382);
  FAdobeGlyphNames.Add('kpasquare', 13226);
  FAdobeGlyphNames.Add('ksicyrillic', 1135);
  FAdobeGlyphNames.Add('Ksicyrillic', 1134);
  FAdobeGlyphNames.Add('Ksmall', 63339);
  FAdobeGlyphNames.Add('ktsquare', 13263);
  FAdobeGlyphNames.Add('kturned', 670);
  FAdobeGlyphNames.Add('kuhiragana', 12367);
  FAdobeGlyphNames.Add('kukatakana', 12463);
  FAdobeGlyphNames.Add('kukatakanahalfwidth', 65400);
  FAdobeGlyphNames.Add('kvsquare', 13240);
  FAdobeGlyphNames.Add('kwsquare', 13246);
  FAdobeGlyphNames.Add('l', 108);
  FAdobeGlyphNames.Add('L', 76);
  FAdobeGlyphNames.Add('labengali', 2482);
  FAdobeGlyphNames.Add('lacute', 314);
  FAdobeGlyphNames.Add('Lacute', 313);
  FAdobeGlyphNames.Add('ladeva', 2354);
  FAdobeGlyphNames.Add('lagujarati', 2738);
  FAdobeGlyphNames.Add('lagurmukhi', 2610);
  FAdobeGlyphNames.Add('lakkhangyaothai', 3653);
  FAdobeGlyphNames.Add('lamaleffinalarabic', 65276);
  FAdobeGlyphNames.Add('lamalefhamzaabovefinalarabic', 65272);
  FAdobeGlyphNames.Add('lamalefhamzaaboveisolatedarabic', 65271);
  FAdobeGlyphNames.Add('lamalefhamzabelowfinalarabic', 65274);
  FAdobeGlyphNames.Add('lamalefhamzabelowisolatedarabic', 65273);
  FAdobeGlyphNames.Add('lamalefisolatedarabic', 65275);
  FAdobeGlyphNames.Add('lamalefmaddaabovefinalarabic', 65270);
  FAdobeGlyphNames.Add('lamalefmaddaaboveisolatedarabic', 65269);
  FAdobeGlyphNames.Add('lamarabic', 1604);
  FAdobeGlyphNames.Add('lambda', 955);
  FAdobeGlyphNames.Add('Lambda', 923);
  FAdobeGlyphNames.Add('lambdastroke', 411);
  FAdobeGlyphNames.Add('lamed', 1500);
  FAdobeGlyphNames.Add('lameddagesh', 64316);
  FAdobeGlyphNames.Add('lameddageshhebrew', 64316);
  FAdobeGlyphNames.Add('lamedhebrew', 1500);
  FAdobeGlyphNames.Add('lamedholam', 1500);
  FAdobeGlyphNames.Add('lamedholamdagesh', 1500);
  FAdobeGlyphNames.Add('lamedholamdageshhebrew', 1500);
  FAdobeGlyphNames.Add('lamedholamhebrew', 1500);
  FAdobeGlyphNames.Add('lamfinalarabic', 65246);
  FAdobeGlyphNames.Add('lamhahinitialarabic', 64714);
  FAdobeGlyphNames.Add('laminitialarabic', 65247);
  FAdobeGlyphNames.Add('lamjeeminitialarabic', 64713);
  FAdobeGlyphNames.Add('lamkhahinitialarabic', 64715);
  FAdobeGlyphNames.Add('lamlamhehisolatedarabic', 65010);
  FAdobeGlyphNames.Add('lammedialarabic', 65248);
  FAdobeGlyphNames.Add('lammeemhahinitialarabic', 64904);
  FAdobeGlyphNames.Add('lammeeminitialarabic', 64716);
  FAdobeGlyphNames.Add('lammeemjeeminitialarabic', 65247);
  FAdobeGlyphNames.Add('lammeemkhahinitialarabic', 65247);
  FAdobeGlyphNames.Add('largecircle', 9711);
  FAdobeGlyphNames.Add('lbar', 410);
  FAdobeGlyphNames.Add('lbelt', 620);
  FAdobeGlyphNames.Add('lbopomofo', 12556);
  FAdobeGlyphNames.Add('lcaron', 318);
  FAdobeGlyphNames.Add('Lcaron', 317);
  FAdobeGlyphNames.Add('lcedilla', 316);
  FAdobeGlyphNames.Add('Lcedilla', 315);
  FAdobeGlyphNames.Add('lcircle', 9435);
  FAdobeGlyphNames.Add('Lcircle', 9409);
  FAdobeGlyphNames.Add('lcircumflexbelow', 7741);
  FAdobeGlyphNames.Add('Lcircumflexbelow', 7740);
  FAdobeGlyphNames.Add('lcommaaccent', 316);
  FAdobeGlyphNames.Add('Lcommaaccent', 315);
  FAdobeGlyphNames.Add('ldot', 320);
  FAdobeGlyphNames.Add('Ldot', 319);
  FAdobeGlyphNames.Add('ldotaccent', 320);
  FAdobeGlyphNames.Add('Ldotaccent', 319);
  FAdobeGlyphNames.Add('ldotbelow', 7735);
  FAdobeGlyphNames.Add('Ldotbelow', 7734);
  FAdobeGlyphNames.Add('ldotbelowmacron', 7737);
  FAdobeGlyphNames.Add('Ldotbelowmacron', 7736);
  FAdobeGlyphNames.Add('leftangleabovecmb', 794);
  FAdobeGlyphNames.Add('lefttackbelowcmb', 792);
  FAdobeGlyphNames.Add('less', 60);
  FAdobeGlyphNames.Add('lessequal', 8804);
  FAdobeGlyphNames.Add('lessequalorgreater', 8922);
  FAdobeGlyphNames.Add('lessmonospace', 65308);
  FAdobeGlyphNames.Add('lessorequivalent', 8818);
  FAdobeGlyphNames.Add('lessorgreater', 8822);
  FAdobeGlyphNames.Add('lessoverequal', 8806);
  FAdobeGlyphNames.Add('lesssmall', 65124);
  FAdobeGlyphNames.Add('lezh', 622);
  FAdobeGlyphNames.Add('lfblock', 9612);
  FAdobeGlyphNames.Add('lhookretroflex', 621);
  FAdobeGlyphNames.Add('lira', 8356);
  FAdobeGlyphNames.Add('liwnarmenian', 1388);
  FAdobeGlyphNames.Add('Liwnarmenian', 1340);
  FAdobeGlyphNames.Add('lj', 457);
  FAdobeGlyphNames.Add('Lj', 456);
  FAdobeGlyphNames.Add('LJ', 455);
  FAdobeGlyphNames.Add('ljecyrillic', 1113);
  FAdobeGlyphNames.Add('Ljecyrillic', 1033);
  FAdobeGlyphNames.Add('ll', 63168);
  FAdobeGlyphNames.Add('LL', 63167);
  FAdobeGlyphNames.Add('lladeva', 2355);
  FAdobeGlyphNames.Add('llagujarati', 2739);
  FAdobeGlyphNames.Add('llinebelow', 7739);
  FAdobeGlyphNames.Add('Llinebelow', 7738);
  FAdobeGlyphNames.Add('llladeva', 2356);
  FAdobeGlyphNames.Add('llvocalicbengali', 2529);
  FAdobeGlyphNames.Add('llvocalicdeva', 2401);
  FAdobeGlyphNames.Add('llvocalicvowelsignbengali', 2531);
  FAdobeGlyphNames.Add('llvocalicvowelsigndeva', 2403);
  FAdobeGlyphNames.Add('lmiddletilde', 619);
  FAdobeGlyphNames.Add('lmonospace', 65356);
  FAdobeGlyphNames.Add('Lmonospace', 65324);
  FAdobeGlyphNames.Add('lmsquare', 13264);
  FAdobeGlyphNames.Add('lochulathai', 3628);
  FAdobeGlyphNames.Add('logicaland', 8743);
  FAdobeGlyphNames.Add('logicalnot', 172);
  FAdobeGlyphNames.Add('logicalnotreversed', 8976);
  FAdobeGlyphNames.Add('logicalor', 8744);
  FAdobeGlyphNames.Add('lolingthai', 3621);
  FAdobeGlyphNames.Add('longs', 383);
  FAdobeGlyphNames.Add('lowlinecenterline', 65102);
  FAdobeGlyphNames.Add('lowlinecmb', 818);
  FAdobeGlyphNames.Add('lowlinedashed', 65101);
  FAdobeGlyphNames.Add('lozenge', 9674);
  FAdobeGlyphNames.Add('lparen', 9383);
  FAdobeGlyphNames.Add('lslash', 322);
  FAdobeGlyphNames.Add('Lslash', 321);
  FAdobeGlyphNames.Add('Lslashsmall', 63225);
  FAdobeGlyphNames.Add('Lsmall', 63340);
  FAdobeGlyphNames.Add('lsquare', 8467);
  FAdobeGlyphNames.Add('lsuperior', 63214);
  FAdobeGlyphNames.Add('ltshade', 9617);
  FAdobeGlyphNames.Add('luthai', 3622);
  FAdobeGlyphNames.Add('lvocalicbengali', 2444);
  FAdobeGlyphNames.Add('lvocalicdeva', 2316);
  FAdobeGlyphNames.Add('lvocalicvowelsignbengali', 2530);
  FAdobeGlyphNames.Add('lvocalicvowelsigndeva', 2402);
  FAdobeGlyphNames.Add('lxsquare', 13267);
  FAdobeGlyphNames.Add('m', 109);
  FAdobeGlyphNames.Add('M', 77);
  FAdobeGlyphNames.Add('mabengali', 2478);
  FAdobeGlyphNames.Add('macron', 175);
  FAdobeGlyphNames.Add('Macron', 63184);
  FAdobeGlyphNames.Add('macronbelowcmb', 817);
  FAdobeGlyphNames.Add('macroncmb', 772);
  FAdobeGlyphNames.Add('macronlowmod', 717);
  FAdobeGlyphNames.Add('macronmonospace', 65507);
  FAdobeGlyphNames.Add('Macronsmall', 63407);
  FAdobeGlyphNames.Add('macute', 7743);
  FAdobeGlyphNames.Add('Macute', 7742);
  FAdobeGlyphNames.Add('madeva', 2350);
  FAdobeGlyphNames.Add('magujarati', 2734);
  FAdobeGlyphNames.Add('magurmukhi', 2606);
  FAdobeGlyphNames.Add('mahapakhhebrew', 1444);
  FAdobeGlyphNames.Add('mahapakhlefthebrew', 1444);
  FAdobeGlyphNames.Add('mahiragana', 12414);
  FAdobeGlyphNames.Add('maichattawalowleftthai', 63637);
  FAdobeGlyphNames.Add('maichattawalowrightthai', 63636);
  FAdobeGlyphNames.Add('maichattawathai', 3659);
  FAdobeGlyphNames.Add('maichattawaupperleftthai', 63635);
  FAdobeGlyphNames.Add('maieklowleftthai', 63628);
  FAdobeGlyphNames.Add('maieklowrightthai', 63627);
  FAdobeGlyphNames.Add('maiekthai', 3656);
  FAdobeGlyphNames.Add('maiekupperleftthai', 63626);
  FAdobeGlyphNames.Add('maihanakatleftthai', 63620);
  FAdobeGlyphNames.Add('maihanakatthai', 3633);
  FAdobeGlyphNames.Add('maitaikhuleftthai', 63625);
  FAdobeGlyphNames.Add('maitaikhuthai', 3655);
  FAdobeGlyphNames.Add('maitholowleftthai', 63631);
  FAdobeGlyphNames.Add('maitholowrightthai', 63630);
  FAdobeGlyphNames.Add('maithothai', 3657);
  FAdobeGlyphNames.Add('maithoupperleftthai', 63629);
  FAdobeGlyphNames.Add('maitrilowleftthai', 63634);
  FAdobeGlyphNames.Add('maitrilowrightthai', 63633);
  FAdobeGlyphNames.Add('maitrithai', 3658);
  FAdobeGlyphNames.Add('maitriupperleftthai', 63632);
  FAdobeGlyphNames.Add('maiyamokthai', 3654);
  FAdobeGlyphNames.Add('makatakana', 12510);
  FAdobeGlyphNames.Add('makatakanahalfwidth', 65423);
  FAdobeGlyphNames.Add('male', 9794);
  FAdobeGlyphNames.Add('mansyonsquare', 13127);
  FAdobeGlyphNames.Add('maqafhebrew', 1470);
  FAdobeGlyphNames.Add('mars', 9794);
  FAdobeGlyphNames.Add('masoracirclehebrew', 1455);
  FAdobeGlyphNames.Add('masquare', 13187);
  FAdobeGlyphNames.Add('mbopomofo', 12551);
  FAdobeGlyphNames.Add('mbsquare', 13268);
  FAdobeGlyphNames.Add('MBsquare', 13190);
  FAdobeGlyphNames.Add('mcircle', 9436);
  FAdobeGlyphNames.Add('Mcircle', 9410);
  FAdobeGlyphNames.Add('mcubedsquare', 13221);
  FAdobeGlyphNames.Add('mdotaccent', 7745);
  FAdobeGlyphNames.Add('Mdotaccent', 7744);
  FAdobeGlyphNames.Add('mdotbelow', 7747);
  FAdobeGlyphNames.Add('Mdotbelow', 7746);
  FAdobeGlyphNames.Add('meemarabic', 1605);
  FAdobeGlyphNames.Add('meemfinalarabic', 65250);
  FAdobeGlyphNames.Add('meeminitialarabic', 65251);
  FAdobeGlyphNames.Add('meemmedialarabic', 65252);
  FAdobeGlyphNames.Add('meemmeeminitialarabic', 64721);
  FAdobeGlyphNames.Add('meemmeemisolatedarabic', 64584);
  FAdobeGlyphNames.Add('meetorusquare', 13133);
  FAdobeGlyphNames.Add('mehiragana', 12417);
  FAdobeGlyphNames.Add('meizierasquare', 13182);
  FAdobeGlyphNames.Add('mekatakana', 12513);
  FAdobeGlyphNames.Add('mekatakanahalfwidth', 65426);
  FAdobeGlyphNames.Add('mem', 1502);
  FAdobeGlyphNames.Add('memdagesh', 64318);
  FAdobeGlyphNames.Add('memdageshhebrew', 64318);
  FAdobeGlyphNames.Add('memhebrew', 1502);
  FAdobeGlyphNames.Add('menarmenian', 1396);
  FAdobeGlyphNames.Add('Menarmenian', 1348);
  FAdobeGlyphNames.Add('merkhahebrew', 1445);
  FAdobeGlyphNames.Add('merkhakefulahebrew', 1446);
  FAdobeGlyphNames.Add('merkhakefulalefthebrew', 1446);
  FAdobeGlyphNames.Add('merkhalefthebrew', 1445);
  FAdobeGlyphNames.Add('mhook', 625);
  FAdobeGlyphNames.Add('mhzsquare', 13202);
  FAdobeGlyphNames.Add('middledotkatakanahalfwidth', 65381);
  FAdobeGlyphNames.Add('middot', 183);
  FAdobeGlyphNames.Add('mieumacirclekorean', 12914);
  FAdobeGlyphNames.Add('mieumaparenkorean', 12818);
  FAdobeGlyphNames.Add('mieumcirclekorean', 12900);
  FAdobeGlyphNames.Add('mieumkorean', 12609);
  FAdobeGlyphNames.Add('mieumpansioskorean', 12656);
  FAdobeGlyphNames.Add('mieumparenkorean', 12804);
  FAdobeGlyphNames.Add('mieumpieupkorean', 12654);
  FAdobeGlyphNames.Add('mieumsioskorean', 12655);
  FAdobeGlyphNames.Add('mihiragana', 12415);
  FAdobeGlyphNames.Add('mikatakana', 12511);
  FAdobeGlyphNames.Add('mikatakanahalfwidth', 65424);
  FAdobeGlyphNames.Add('minus', 8722);
  FAdobeGlyphNames.Add('minusbelowcmb', 800);
  FAdobeGlyphNames.Add('minuscircle', 8854);
  FAdobeGlyphNames.Add('minusmod', 727);
  FAdobeGlyphNames.Add('minusplus', 8723);
  FAdobeGlyphNames.Add('minute', 8242);
  FAdobeGlyphNames.Add('miribaarusquare', 13130);
  FAdobeGlyphNames.Add('mirisquare', 13129);
  FAdobeGlyphNames.Add('mlonglegturned', 624);
  FAdobeGlyphNames.Add('mlsquare', 13206);
end;

procedure TdxFontFileUnicodeConverter.InitializeAdobeGlyphNames3;
begin
  FAdobeGlyphNames.Add('mmcubedsquare', 13219);
  FAdobeGlyphNames.Add('mmonospace', 65357);
  FAdobeGlyphNames.Add('Mmonospace', 65325);
  FAdobeGlyphNames.Add('mmsquaredsquare', 13215);
  FAdobeGlyphNames.Add('mohiragana', 12418);
  FAdobeGlyphNames.Add('mohmsquare', 13249);
  FAdobeGlyphNames.Add('mokatakana', 12514);
  FAdobeGlyphNames.Add('mokatakanahalfwidth', 65427);
  FAdobeGlyphNames.Add('molsquare', 13270);
  FAdobeGlyphNames.Add('momathai', 3617);
  FAdobeGlyphNames.Add('moverssquare', 13223);
  FAdobeGlyphNames.Add('moverssquaredsquare', 13224);
  FAdobeGlyphNames.Add('mparen', 9384);
  FAdobeGlyphNames.Add('mpasquare', 13227);
  FAdobeGlyphNames.Add('Msmall', 63341);
  FAdobeGlyphNames.Add('mssquare', 13235);
  FAdobeGlyphNames.Add('msuperior', 63215);
  FAdobeGlyphNames.Add('mturned', 623);
  FAdobeGlyphNames.Add('Mturned', 412);
  FAdobeGlyphNames.Add('mu', 181);
  FAdobeGlyphNames.Add('Mu', 924);
  FAdobeGlyphNames.Add('mu1', 181);
  FAdobeGlyphNames.Add('muasquare', 13186);
  FAdobeGlyphNames.Add('muchgreater', 8811);
  FAdobeGlyphNames.Add('muchless', 8810);
  FAdobeGlyphNames.Add('mufsquare', 13196);
  FAdobeGlyphNames.Add('mugreek', 956);
  FAdobeGlyphNames.Add('mugsquare', 13197);
  FAdobeGlyphNames.Add('muhiragana', 12416);
  FAdobeGlyphNames.Add('mukatakana', 12512);
  FAdobeGlyphNames.Add('mukatakanahalfwidth', 65425);
  FAdobeGlyphNames.Add('mulsquare', 13205);
  FAdobeGlyphNames.Add('multiply', 215);
  FAdobeGlyphNames.Add('mumsquare', 13211);
  FAdobeGlyphNames.Add('munahhebrew', 1443);
  FAdobeGlyphNames.Add('munahlefthebrew', 1443);
  FAdobeGlyphNames.Add('musicalnote', 9834);
  FAdobeGlyphNames.Add('musicalnotedbl', 9835);
  FAdobeGlyphNames.Add('musicflatsign', 9837);
  FAdobeGlyphNames.Add('musicsharpsign', 9839);
  FAdobeGlyphNames.Add('mussquare', 13234);
  FAdobeGlyphNames.Add('muvsquare', 13238);
  FAdobeGlyphNames.Add('muwsquare', 13244);
  FAdobeGlyphNames.Add('mvmegasquare', 13241);
  FAdobeGlyphNames.Add('mvsquare', 13239);
  FAdobeGlyphNames.Add('mwmegasquare', 13247);
  FAdobeGlyphNames.Add('mwsquare', 13245);
  FAdobeGlyphNames.Add('n', 110);
  FAdobeGlyphNames.Add('N', 78);
  FAdobeGlyphNames.Add('nabengali', 2472);
  FAdobeGlyphNames.Add('nabla', 8711);
  FAdobeGlyphNames.Add('nacute', 324);
  FAdobeGlyphNames.Add('Nacute', 323);
  FAdobeGlyphNames.Add('nadeva', 2344);
  FAdobeGlyphNames.Add('nagujarati', 2728);
  FAdobeGlyphNames.Add('nagurmukhi', 2600);
  FAdobeGlyphNames.Add('nahiragana', 12394);
  FAdobeGlyphNames.Add('nakatakana', 12490);
  FAdobeGlyphNames.Add('nakatakanahalfwidth', 65413);
  FAdobeGlyphNames.Add('napostrophe', 329);
  FAdobeGlyphNames.Add('nasquare', 13185);
  FAdobeGlyphNames.Add('nbopomofo', 12555);
  FAdobeGlyphNames.Add('nbspace', 160);
  FAdobeGlyphNames.Add('ncaron', 328);
  FAdobeGlyphNames.Add('Ncaron', 327);
  FAdobeGlyphNames.Add('ncedilla', 326);
  FAdobeGlyphNames.Add('Ncedilla', 325);
  FAdobeGlyphNames.Add('ncircle', 9437);
  FAdobeGlyphNames.Add('Ncircle', 9411);
  FAdobeGlyphNames.Add('ncircumflexbelow', 7755);
  FAdobeGlyphNames.Add('Ncircumflexbelow', 7754);
  FAdobeGlyphNames.Add('ncommaaccent', 326);
  FAdobeGlyphNames.Add('Ncommaaccent', 325);
  FAdobeGlyphNames.Add('ndotaccent', 7749);
  FAdobeGlyphNames.Add('Ndotaccent', 7748);
  FAdobeGlyphNames.Add('ndotbelow', 7751);
  FAdobeGlyphNames.Add('Ndotbelow', 7750);
  FAdobeGlyphNames.Add('nehiragana', 12397);
  FAdobeGlyphNames.Add('nekatakana', 12493);
  FAdobeGlyphNames.Add('nekatakanahalfwidth', 65416);
  FAdobeGlyphNames.Add('newsheqelsign', 8362);
  FAdobeGlyphNames.Add('nfsquare', 13195);
  FAdobeGlyphNames.Add('ngabengali', 2457);
  FAdobeGlyphNames.Add('ngadeva', 2329);
  FAdobeGlyphNames.Add('ngagujarati', 2713);
  FAdobeGlyphNames.Add('ngagurmukhi', 2585);
  FAdobeGlyphNames.Add('ngonguthai', 3591);
  FAdobeGlyphNames.Add('nhiragana', 12435);
  FAdobeGlyphNames.Add('nhookleft', 626);
  FAdobeGlyphNames.Add('Nhookleft', 413);
  FAdobeGlyphNames.Add('nhookretroflex', 627);
  FAdobeGlyphNames.Add('nieunacirclekorean', 12911);
  FAdobeGlyphNames.Add('nieunaparenkorean', 12815);
  FAdobeGlyphNames.Add('nieuncieuckorean', 12597);
  FAdobeGlyphNames.Add('nieuncirclekorean', 12897);
  FAdobeGlyphNames.Add('nieunhieuhkorean', 12598);
  FAdobeGlyphNames.Add('nieunkorean', 12596);
  FAdobeGlyphNames.Add('nieunpansioskorean', 12648);
  FAdobeGlyphNames.Add('nieunparenkorean', 12801);
  FAdobeGlyphNames.Add('nieunsioskorean', 12647);
  FAdobeGlyphNames.Add('nieuntikeutkorean', 12646);
  FAdobeGlyphNames.Add('nihiragana', 12395);
  FAdobeGlyphNames.Add('nikatakana', 12491);
  FAdobeGlyphNames.Add('nikatakanahalfwidth', 65414);
  FAdobeGlyphNames.Add('nikhahitleftthai', 63641);
  FAdobeGlyphNames.Add('nikhahitthai', 3661);
  FAdobeGlyphNames.Add('nine', 57);
  FAdobeGlyphNames.Add('ninearabic', 1641);
  FAdobeGlyphNames.Add('ninebengali', 2543);
  FAdobeGlyphNames.Add('ninecircle', 9320);
  FAdobeGlyphNames.Add('ninecircleinversesansserif', 10130);
  FAdobeGlyphNames.Add('ninedeva', 2415);
  FAdobeGlyphNames.Add('ninegujarati', 2799);
  FAdobeGlyphNames.Add('ninegurmukhi', 2671);
  FAdobeGlyphNames.Add('ninehackarabic', 1641);
  FAdobeGlyphNames.Add('ninehangzhou', 12329);
  FAdobeGlyphNames.Add('nineideographicparen', 12840);
  FAdobeGlyphNames.Add('nineinferior', 8329);
  FAdobeGlyphNames.Add('ninemonospace', 65305);
  FAdobeGlyphNames.Add('nineoldstyle', 63289);
  FAdobeGlyphNames.Add('nineparen', 9340);
  FAdobeGlyphNames.Add('nineperiod', 9360);
  FAdobeGlyphNames.Add('ninepersian', 1785);
  FAdobeGlyphNames.Add('nineroman', 8568);
  FAdobeGlyphNames.Add('Nineroman', 8552);
  FAdobeGlyphNames.Add('ninesuperior', 8313);
  FAdobeGlyphNames.Add('nineteencircle', 9330);
  FAdobeGlyphNames.Add('nineteenparen', 9350);
  FAdobeGlyphNames.Add('nineteenperiod', 9370);
  FAdobeGlyphNames.Add('ninethai', 3673);
  FAdobeGlyphNames.Add('nj', 460);
  FAdobeGlyphNames.Add('Nj', 459);
  FAdobeGlyphNames.Add('NJ', 458);
  FAdobeGlyphNames.Add('njecyrillic', 1114);
  FAdobeGlyphNames.Add('Njecyrillic', 1034);
  FAdobeGlyphNames.Add('nkatakana', 12531);
  FAdobeGlyphNames.Add('nkatakanahalfwidth', 65437);
  FAdobeGlyphNames.Add('nlegrightlong', 414);
  FAdobeGlyphNames.Add('nlinebelow', 7753);
  FAdobeGlyphNames.Add('Nlinebelow', 7752);
  FAdobeGlyphNames.Add('nmonospace', 65358);
  FAdobeGlyphNames.Add('Nmonospace', 65326);
  FAdobeGlyphNames.Add('nmsquare', 13210);
  FAdobeGlyphNames.Add('nnabengali', 2467);
  FAdobeGlyphNames.Add('nnadeva', 2339);
  FAdobeGlyphNames.Add('nnagujarati', 2723);
  FAdobeGlyphNames.Add('nnagurmukhi', 2595);
  FAdobeGlyphNames.Add('nnnadeva', 2345);
  FAdobeGlyphNames.Add('nohiragana', 12398);
  FAdobeGlyphNames.Add('nokatakana', 12494);
  FAdobeGlyphNames.Add('nokatakanahalfwidth', 65417);
  FAdobeGlyphNames.Add('nonbreakingspace', 160);
  FAdobeGlyphNames.Add('nonenthai', 3603);
  FAdobeGlyphNames.Add('nonuthai', 3609);
  FAdobeGlyphNames.Add('noonarabic', 1606);
  FAdobeGlyphNames.Add('noonfinalarabic', 65254);
  FAdobeGlyphNames.Add('noonghunnaarabic', 1722);
  FAdobeGlyphNames.Add('noonghunnafinalarabic', 64415);
  FAdobeGlyphNames.Add('noonhehinitialarabic', 65255);
  FAdobeGlyphNames.Add('nooninitialarabic', 65255);
  FAdobeGlyphNames.Add('noonjeeminitialarabic', 64722);
  FAdobeGlyphNames.Add('noonjeemisolatedarabic', 64587);
  FAdobeGlyphNames.Add('noonmedialarabic', 65256);
  FAdobeGlyphNames.Add('noonmeeminitialarabic', 64725);
  FAdobeGlyphNames.Add('noonmeemisolatedarabic', 64590);
  FAdobeGlyphNames.Add('noonnoonfinalarabic', 64653);
  FAdobeGlyphNames.Add('notcontains', 8716);
  FAdobeGlyphNames.Add('notelement', 8713);
  FAdobeGlyphNames.Add('notelementof', 8713);
  FAdobeGlyphNames.Add('notequal', 8800);
  FAdobeGlyphNames.Add('notgreater', 8815);
  FAdobeGlyphNames.Add('notgreaternorequal', 8817);
  FAdobeGlyphNames.Add('notgreaternorless', 8825);
  FAdobeGlyphNames.Add('notidentical', 8802);
  FAdobeGlyphNames.Add('notless', 8814);
  FAdobeGlyphNames.Add('notlessnorequal', 8816);
  FAdobeGlyphNames.Add('notparallel', 8742);
  FAdobeGlyphNames.Add('notprecedes', 8832);
  FAdobeGlyphNames.Add('notsubset', 8836);
  FAdobeGlyphNames.Add('notsucceeds', 8833);
  FAdobeGlyphNames.Add('notsuperset', 8837);
  FAdobeGlyphNames.Add('nowarmenian', 1398);
  FAdobeGlyphNames.Add('Nowarmenian', 1350);
  FAdobeGlyphNames.Add('nparen', 9385);
  FAdobeGlyphNames.Add('Nsmall', 63342);
  FAdobeGlyphNames.Add('nssquare', 13233);
  FAdobeGlyphNames.Add('nsuperior', 8319);
  FAdobeGlyphNames.Add('ntilde', 241);
  FAdobeGlyphNames.Add('Ntilde', 209);
  FAdobeGlyphNames.Add('Ntildesmall', 63473);
  FAdobeGlyphNames.Add('nu', 957);
  FAdobeGlyphNames.Add('Nu', 925);
  FAdobeGlyphNames.Add('nuhiragana', 12396);
  FAdobeGlyphNames.Add('nukatakana', 12492);
  FAdobeGlyphNames.Add('nukatakanahalfwidth', 65415);
  FAdobeGlyphNames.Add('nuktabengali', 2492);
  FAdobeGlyphNames.Add('nuktadeva', 2364);
  FAdobeGlyphNames.Add('nuktagujarati', 2748);
  FAdobeGlyphNames.Add('nuktagurmukhi', 2620);
  FAdobeGlyphNames.Add('numbersign', 35);
  FAdobeGlyphNames.Add('numbersignmonospace', 65283);
  FAdobeGlyphNames.Add('numbersignsmall', 65119);
  FAdobeGlyphNames.Add('numeralsigngreek', 884);
  FAdobeGlyphNames.Add('numeralsignlowergreek', 885);
  FAdobeGlyphNames.Add('numero', 8470);
  FAdobeGlyphNames.Add('nun', 1504);
  FAdobeGlyphNames.Add('nundagesh', 64320);
  FAdobeGlyphNames.Add('nundageshhebrew', 64320);
  FAdobeGlyphNames.Add('nunhebrew', 1504);
  FAdobeGlyphNames.Add('nvsquare', 13237);
  FAdobeGlyphNames.Add('nwsquare', 13243);
  FAdobeGlyphNames.Add('nyabengali', 2462);
  FAdobeGlyphNames.Add('nyadeva', 2334);
  FAdobeGlyphNames.Add('nyagujarati', 2718);
  FAdobeGlyphNames.Add('nyagurmukhi', 2590);
  FAdobeGlyphNames.Add('o', 111);
  FAdobeGlyphNames.Add('O', 79);
  FAdobeGlyphNames.Add('oacute', 243);
  FAdobeGlyphNames.Add('Oacute', 211);
  FAdobeGlyphNames.Add('Oacutesmall', 63475);
  FAdobeGlyphNames.Add('oangthai', 3629);
  FAdobeGlyphNames.Add('obarred', 629);
  FAdobeGlyphNames.Add('obarredcyrillic', 1257);
  FAdobeGlyphNames.Add('Obarredcyrillic', 1256);
  FAdobeGlyphNames.Add('obarreddieresiscyrillic', 1259);
  FAdobeGlyphNames.Add('Obarreddieresiscyrillic', 1258);
  FAdobeGlyphNames.Add('obengali', 2451);
  FAdobeGlyphNames.Add('obopomofo', 12571);
  FAdobeGlyphNames.Add('obreve', 335);
  FAdobeGlyphNames.Add('Obreve', 334);
  FAdobeGlyphNames.Add('ocandradeva', 2321);
  FAdobeGlyphNames.Add('ocandragujarati', 2705);
  FAdobeGlyphNames.Add('ocandravowelsigndeva', 2377);
  FAdobeGlyphNames.Add('ocandravowelsigngujarati', 2761);
  FAdobeGlyphNames.Add('ocaron', 466);
  FAdobeGlyphNames.Add('Ocaron', 465);
  FAdobeGlyphNames.Add('Ocenteredtilde', 415);
  FAdobeGlyphNames.Add('ocircle', 9438);
  FAdobeGlyphNames.Add('Ocircle', 9412);
  FAdobeGlyphNames.Add('ocircumflex', 244);
  FAdobeGlyphNames.Add('Ocircumflex', 212);
  FAdobeGlyphNames.Add('ocircumflexacute', 7889);
  FAdobeGlyphNames.Add('Ocircumflexacute', 7888);
  FAdobeGlyphNames.Add('ocircumflexdotbelow', 7897);
  FAdobeGlyphNames.Add('Ocircumflexdotbelow', 7896);
  FAdobeGlyphNames.Add('ocircumflexgrave', 7891);
  FAdobeGlyphNames.Add('Ocircumflexgrave', 7890);
  FAdobeGlyphNames.Add('ocircumflexhookabove', 7893);
  FAdobeGlyphNames.Add('Ocircumflexhookabove', 7892);
  FAdobeGlyphNames.Add('Ocircumflexsmall', 63476);
  FAdobeGlyphNames.Add('ocircumflextilde', 7895);
  FAdobeGlyphNames.Add('Ocircumflextilde', 7894);
  FAdobeGlyphNames.Add('ocyrillic', 1086);
  FAdobeGlyphNames.Add('Ocyrillic', 1054);
  FAdobeGlyphNames.Add('odblacute', 337);
  FAdobeGlyphNames.Add('Odblacute', 336);
  FAdobeGlyphNames.Add('odblgrave', 525);
  FAdobeGlyphNames.Add('Odblgrave', 524);
  FAdobeGlyphNames.Add('odeva', 2323);
  FAdobeGlyphNames.Add('odieresis', 246);
  FAdobeGlyphNames.Add('Odieresis', 214);
  FAdobeGlyphNames.Add('odieresiscyrillic', 1255);
  FAdobeGlyphNames.Add('Odieresiscyrillic', 1254);
  FAdobeGlyphNames.Add('Odieresissmall', 63478);
  FAdobeGlyphNames.Add('odotbelow', 7885);
  FAdobeGlyphNames.Add('Odotbelow', 7884);
  FAdobeGlyphNames.Add('oe', 339);
  FAdobeGlyphNames.Add('OE', 338);
  FAdobeGlyphNames.Add('oekorean', 12634);
  FAdobeGlyphNames.Add('OEsmall', 63226);
  FAdobeGlyphNames.Add('ogonek', 731);
  FAdobeGlyphNames.Add('ogonekcmb', 808);
  FAdobeGlyphNames.Add('Ogoneksmall', 63227);
  FAdobeGlyphNames.Add('ograve', 242);
  FAdobeGlyphNames.Add('Ograve', 210);
  FAdobeGlyphNames.Add('Ogravesmall', 63474);
  FAdobeGlyphNames.Add('ogujarati', 2707);
  FAdobeGlyphNames.Add('oharmenian', 1413);
  FAdobeGlyphNames.Add('Oharmenian', 1365);
  FAdobeGlyphNames.Add('ohiragana', 12362);
  FAdobeGlyphNames.Add('Ohm', 8486);
  FAdobeGlyphNames.Add('ohookabove', 7887);
  FAdobeGlyphNames.Add('Ohookabove', 7886);
  FAdobeGlyphNames.Add('ohorn', 417);
  FAdobeGlyphNames.Add('Ohorn', 416);
  FAdobeGlyphNames.Add('ohornacute', 7899);
  FAdobeGlyphNames.Add('Ohornacute', 7898);
  FAdobeGlyphNames.Add('ohorndotbelow', 7907);
  FAdobeGlyphNames.Add('Ohorndotbelow', 7906);
  FAdobeGlyphNames.Add('ohorngrave', 7901);
  FAdobeGlyphNames.Add('Ohorngrave', 7900);
  FAdobeGlyphNames.Add('ohornhookabove', 7903);
  FAdobeGlyphNames.Add('Ohornhookabove', 7902);
  FAdobeGlyphNames.Add('ohorntilde', 7905);
  FAdobeGlyphNames.Add('Ohorntilde', 7904);
  FAdobeGlyphNames.Add('ohungarumlaut', 337);
  FAdobeGlyphNames.Add('Ohungarumlaut', 336);
  FAdobeGlyphNames.Add('oi', 419);
  FAdobeGlyphNames.Add('Oi', 418);
  FAdobeGlyphNames.Add('oinvertedbreve', 527);
  FAdobeGlyphNames.Add('Oinvertedbreve', 526);
  FAdobeGlyphNames.Add('okatakana', 12458);
  FAdobeGlyphNames.Add('okatakanahalfwidth', 65397);
  FAdobeGlyphNames.Add('okorean', 12631);
  FAdobeGlyphNames.Add('olehebrew', 1451);
  FAdobeGlyphNames.Add('omacron', 333);
  FAdobeGlyphNames.Add('Omacron', 332);
  FAdobeGlyphNames.Add('omacronacute', 7763);
  FAdobeGlyphNames.Add('Omacronacute', 7762);
  FAdobeGlyphNames.Add('omacrongrave', 7761);
  FAdobeGlyphNames.Add('Omacrongrave', 7760);
  FAdobeGlyphNames.Add('omdeva', 2384);
  FAdobeGlyphNames.Add('omega', 969);
  FAdobeGlyphNames.Add('Omega', 8486);
  FAdobeGlyphNames.Add('omega1', 982);
  FAdobeGlyphNames.Add('omegacyrillic', 1121);
  FAdobeGlyphNames.Add('Omegacyrillic', 1120);
  FAdobeGlyphNames.Add('Omegagreek', 937);
  FAdobeGlyphNames.Add('omegalatinclosed', 631);
  FAdobeGlyphNames.Add('omegaroundcyrillic', 1147);
  FAdobeGlyphNames.Add('Omegaroundcyrillic', 1146);
  FAdobeGlyphNames.Add('omegatitlocyrillic', 1149);
  FAdobeGlyphNames.Add('Omegatitlocyrillic', 1148);
  FAdobeGlyphNames.Add('omegatonos', 974);
  FAdobeGlyphNames.Add('Omegatonos', 911);
  FAdobeGlyphNames.Add('omgujarati', 2768);
  FAdobeGlyphNames.Add('omicron', 959);
  FAdobeGlyphNames.Add('Omicron', 927);
  FAdobeGlyphNames.Add('omicrontonos', 972);
  FAdobeGlyphNames.Add('Omicrontonos', 908);
  FAdobeGlyphNames.Add('omonospace', 65359);
  FAdobeGlyphNames.Add('Omonospace', 65327);
  FAdobeGlyphNames.Add('one', 49);
  FAdobeGlyphNames.Add('onearabic', 1633);
  FAdobeGlyphNames.Add('onebengali', 2535);
  FAdobeGlyphNames.Add('onecircle', 9312);
  FAdobeGlyphNames.Add('onecircleinversesansserif', 10122);
  FAdobeGlyphNames.Add('onedeva', 2407);
  FAdobeGlyphNames.Add('onedotenleader', 8228);
  FAdobeGlyphNames.Add('oneeighth', 8539);
  FAdobeGlyphNames.Add('onefitted', 63196);
  FAdobeGlyphNames.Add('onegujarati', 2791);
  FAdobeGlyphNames.Add('onegurmukhi', 2663);
  FAdobeGlyphNames.Add('onehackarabic', 1633);
  FAdobeGlyphNames.Add('onehalf', 189);
  FAdobeGlyphNames.Add('onehangzhou', 12321);
  FAdobeGlyphNames.Add('oneideographicparen', 12832);
  FAdobeGlyphNames.Add('oneinferior', 8321);
  FAdobeGlyphNames.Add('onemonospace', 65297);
  FAdobeGlyphNames.Add('onenumeratorbengali', 2548);
  FAdobeGlyphNames.Add('oneoldstyle', 63281);
  FAdobeGlyphNames.Add('oneparen', 9332);
  FAdobeGlyphNames.Add('oneperiod', 9352);
  FAdobeGlyphNames.Add('onepersian', 1777);
  FAdobeGlyphNames.Add('onequarter', 188);
  FAdobeGlyphNames.Add('oneroman', 8560);
  FAdobeGlyphNames.Add('Oneroman', 8544);
  FAdobeGlyphNames.Add('onesuperior', 185);
  FAdobeGlyphNames.Add('onethai', 3665);
  FAdobeGlyphNames.Add('onethird', 8531);
  FAdobeGlyphNames.Add('oogonek', 491);
  FAdobeGlyphNames.Add('Oogonek', 490);
  FAdobeGlyphNames.Add('oogonekmacron', 493);
  FAdobeGlyphNames.Add('Oogonekmacron', 492);
  FAdobeGlyphNames.Add('oogurmukhi', 2579);
  FAdobeGlyphNames.Add('oomatragurmukhi', 2635);
  FAdobeGlyphNames.Add('oopen', 596);
  FAdobeGlyphNames.Add('Oopen', 390);
  FAdobeGlyphNames.Add('oparen', 9386);
  FAdobeGlyphNames.Add('openbullet', 9702);
  FAdobeGlyphNames.Add('option', 8997);
  FAdobeGlyphNames.Add('ordfeminine', 170);
  FAdobeGlyphNames.Add('ordmasculine', 186);
  FAdobeGlyphNames.Add('orthogonal', 8735);
  FAdobeGlyphNames.Add('oshortdeva', 2322);
  FAdobeGlyphNames.Add('oshortvowelsigndeva', 2378);
  FAdobeGlyphNames.Add('oslash', 248);
  FAdobeGlyphNames.Add('Oslash', 216);
  FAdobeGlyphNames.Add('oslashacute', 511);
  FAdobeGlyphNames.Add('Oslashacute', 510);
  FAdobeGlyphNames.Add('Oslashsmall', 63480);
  FAdobeGlyphNames.Add('Osmall', 63343);
  FAdobeGlyphNames.Add('osmallhiragana', 12361);
  FAdobeGlyphNames.Add('osmallkatakana', 12457);
  FAdobeGlyphNames.Add('osmallkatakanahalfwidth', 65387);
  FAdobeGlyphNames.Add('ostrokeacute', 511);
  FAdobeGlyphNames.Add('Ostrokeacute', 510);
  FAdobeGlyphNames.Add('osuperior', 63216);
  FAdobeGlyphNames.Add('otcyrillic', 1151);
  FAdobeGlyphNames.Add('Otcyrillic', 1150);
  FAdobeGlyphNames.Add('otilde', 245);
  FAdobeGlyphNames.Add('Otilde', 213);
  FAdobeGlyphNames.Add('otildeacute', 7757);
  FAdobeGlyphNames.Add('Otildeacute', 7756);
  FAdobeGlyphNames.Add('otildedieresis', 7759);
  FAdobeGlyphNames.Add('Otildedieresis', 7758);
  FAdobeGlyphNames.Add('Otildesmall', 63477);
  FAdobeGlyphNames.Add('oubopomofo', 12577);
  FAdobeGlyphNames.Add('overline', 8254);
  FAdobeGlyphNames.Add('overlinecenterline', 65098);
  FAdobeGlyphNames.Add('overlinecmb', 773);
  FAdobeGlyphNames.Add('overlinedashed', 65097);
  FAdobeGlyphNames.Add('overlinedblwavy', 65100);
  FAdobeGlyphNames.Add('overlinewavy', 65099);
  FAdobeGlyphNames.Add('overscore', 175);
  FAdobeGlyphNames.Add('ovowelsignbengali', 2507);
  FAdobeGlyphNames.Add('ovowelsigndeva', 2379);
  FAdobeGlyphNames.Add('ovowelsigngujarati', 2763);
  FAdobeGlyphNames.Add('p', 112);
  FAdobeGlyphNames.Add('P', 80);
  FAdobeGlyphNames.Add('paampssquare', 13184);
  FAdobeGlyphNames.Add('paasentosquare', 13099);
  FAdobeGlyphNames.Add('pabengali', 2474);
  FAdobeGlyphNames.Add('pacute', 7765);
  FAdobeGlyphNames.Add('Pacute', 7764);
  FAdobeGlyphNames.Add('padeva', 2346);
  FAdobeGlyphNames.Add('pagedown', 8671);
  FAdobeGlyphNames.Add('pageup', 8670);
  FAdobeGlyphNames.Add('pagujarati', 2730);
  FAdobeGlyphNames.Add('pagurmukhi', 2602);
  FAdobeGlyphNames.Add('pahiragana', 12401);
  FAdobeGlyphNames.Add('paiyannoithai', 3631);
  FAdobeGlyphNames.Add('pakatakana', 12497);
  FAdobeGlyphNames.Add('palatalizationcyrilliccmb', 1156);
  FAdobeGlyphNames.Add('palochkacyrillic', 1216);
  FAdobeGlyphNames.Add('pansioskorean', 12671);
  FAdobeGlyphNames.Add('paragraph', 182);
  FAdobeGlyphNames.Add('parallel', 8741);
  FAdobeGlyphNames.Add('parenleft', 40);
  FAdobeGlyphNames.Add('parenleftaltonearabic', 64830);
  FAdobeGlyphNames.Add('parenleftbt', 63725);
  FAdobeGlyphNames.Add('parenleftex', 63724);
  FAdobeGlyphNames.Add('parenleftinferior', 8333);
  FAdobeGlyphNames.Add('parenleftmonospace', 65288);
  FAdobeGlyphNames.Add('parenleftsmall', 65113);
  FAdobeGlyphNames.Add('parenleftsuperior', 8317);
  FAdobeGlyphNames.Add('parenlefttp', 63723);
  FAdobeGlyphNames.Add('parenleftvertical', 65077);
  FAdobeGlyphNames.Add('parenright', 41);
  FAdobeGlyphNames.Add('parenrightaltonearabic', 64831);
  FAdobeGlyphNames.Add('parenrightbt', 63736);
  FAdobeGlyphNames.Add('parenrightex', 63735);
  FAdobeGlyphNames.Add('parenrightinferior', 8334);
  FAdobeGlyphNames.Add('parenrightmonospace', 65289);
  FAdobeGlyphNames.Add('parenrightsmall', 65114);
  FAdobeGlyphNames.Add('parenrightsuperior', 8318);
  FAdobeGlyphNames.Add('parenrighttp', 63734);
  FAdobeGlyphNames.Add('parenrightvertical', 65078);
  FAdobeGlyphNames.Add('partialdiff', 8706);
  FAdobeGlyphNames.Add('paseqhebrew', 1472);
  FAdobeGlyphNames.Add('pashtahebrew', 1433);
  FAdobeGlyphNames.Add('pasquare', 13225);
  FAdobeGlyphNames.Add('patah', 1463);
  FAdobeGlyphNames.Add('patah11', 1463);
  FAdobeGlyphNames.Add('patah1d', 1463);
  FAdobeGlyphNames.Add('patah2a', 1463);
  FAdobeGlyphNames.Add('patahhebrew', 1463);
  FAdobeGlyphNames.Add('patahnarrowhebrew', 1463);
  FAdobeGlyphNames.Add('patahquarterhebrew', 1463);
  FAdobeGlyphNames.Add('patahwidehebrew', 1463);
  FAdobeGlyphNames.Add('pazerhebrew', 1441);
  FAdobeGlyphNames.Add('pbopomofo', 12550);
  FAdobeGlyphNames.Add('pcircle', 9439);
  FAdobeGlyphNames.Add('Pcircle', 9413);
  FAdobeGlyphNames.Add('pdotaccent', 7767);
  FAdobeGlyphNames.Add('Pdotaccent', 7766);
  FAdobeGlyphNames.Add('pe', 1508);
  FAdobeGlyphNames.Add('pecyrillic', 1087);
  FAdobeGlyphNames.Add('Pecyrillic', 1055);
  FAdobeGlyphNames.Add('pedagesh', 64324);
  FAdobeGlyphNames.Add('pedageshhebrew', 64324);
  FAdobeGlyphNames.Add('peezisquare', 13115);
  FAdobeGlyphNames.Add('pefinaldageshhebrew', 64323);
  FAdobeGlyphNames.Add('peharabic', 1662);
  FAdobeGlyphNames.Add('peharmenian', 1402);
  FAdobeGlyphNames.Add('Peharmenian', 1354);
  FAdobeGlyphNames.Add('pehebrew', 1508);
  FAdobeGlyphNames.Add('pehfinalarabic', 64343);
  FAdobeGlyphNames.Add('pehinitialarabic', 64344);
  FAdobeGlyphNames.Add('pehiragana', 12410);
  FAdobeGlyphNames.Add('pehmedialarabic', 64345);
  FAdobeGlyphNames.Add('pekatakana', 12506);
  FAdobeGlyphNames.Add('pemiddlehookcyrillic', 1191);
  FAdobeGlyphNames.Add('Pemiddlehookcyrillic', 1190);
  FAdobeGlyphNames.Add('perafehebrew', 64334);
  FAdobeGlyphNames.Add('percent', 37);
  FAdobeGlyphNames.Add('percentarabic', 1642);
  FAdobeGlyphNames.Add('percentmonospace', 65285);
  FAdobeGlyphNames.Add('percentsmall', 65130);
  FAdobeGlyphNames.Add('period', 46);
  FAdobeGlyphNames.Add('periodarmenian', 1417);
  FAdobeGlyphNames.Add('periodcentered', 183);
  FAdobeGlyphNames.Add('periodhalfwidth', 65377);
  FAdobeGlyphNames.Add('periodinferior', 63207);
  FAdobeGlyphNames.Add('periodmonospace', 65294);
  FAdobeGlyphNames.Add('periodsmall', 65106);
  FAdobeGlyphNames.Add('periodsuperior', 63208);
  FAdobeGlyphNames.Add('perispomenigreekcmb', 834);
  FAdobeGlyphNames.Add('perpendicular', 8869);
  FAdobeGlyphNames.Add('perthousand', 8240);
  FAdobeGlyphNames.Add('peseta', 8359);
  FAdobeGlyphNames.Add('pfsquare', 13194);
  FAdobeGlyphNames.Add('phabengali', 2475);
  FAdobeGlyphNames.Add('phadeva', 2347);
  FAdobeGlyphNames.Add('phagujarati', 2731);
  FAdobeGlyphNames.Add('phagurmukhi', 2603);
  FAdobeGlyphNames.Add('phi', 966);
  FAdobeGlyphNames.Add('Phi', 934);
  FAdobeGlyphNames.Add('phi1', 981);
  FAdobeGlyphNames.Add('phieuphacirclekorean', 12922);
  FAdobeGlyphNames.Add('phieuphaparenkorean', 12826);
  FAdobeGlyphNames.Add('phieuphcirclekorean', 12908);
  FAdobeGlyphNames.Add('phieuphkorean', 12621);
  FAdobeGlyphNames.Add('phieuphparenkorean', 12812);
  FAdobeGlyphNames.Add('philatin', 632);
  FAdobeGlyphNames.Add('phinthuthai', 3642);
  FAdobeGlyphNames.Add('phisymbolgreek', 981);
  FAdobeGlyphNames.Add('phook', 421);
  FAdobeGlyphNames.Add('Phook', 420);
  FAdobeGlyphNames.Add('phophanthai', 3614);
  FAdobeGlyphNames.Add('phophungthai', 3612);
  FAdobeGlyphNames.Add('phosamphaothai', 3616);
  FAdobeGlyphNames.Add('pi', 960);
  FAdobeGlyphNames.Add('Pi', 928);
  FAdobeGlyphNames.Add('pieupacirclekorean', 12915);
  FAdobeGlyphNames.Add('pieupaparenkorean', 12819);
  FAdobeGlyphNames.Add('pieupcieuckorean', 12662);
  FAdobeGlyphNames.Add('pieupcirclekorean', 12901);
  FAdobeGlyphNames.Add('pieupkiyeokkorean', 12658);
  FAdobeGlyphNames.Add('pieupkorean', 12610);
  FAdobeGlyphNames.Add('pieupparenkorean', 12805);
  FAdobeGlyphNames.Add('pieupsioskiyeokkorean', 12660);
  FAdobeGlyphNames.Add('pieupsioskorean', 12612);
  FAdobeGlyphNames.Add('pieupsiostikeutkorean', 12661);
  FAdobeGlyphNames.Add('pieupthieuthkorean', 12663);
  FAdobeGlyphNames.Add('pieuptikeutkorean', 12659);
  FAdobeGlyphNames.Add('pihiragana', 12404);
  FAdobeGlyphNames.Add('pikatakana', 12500);
  FAdobeGlyphNames.Add('pisymbolgreek', 982);
  FAdobeGlyphNames.Add('piwrarmenian', 1411);
  FAdobeGlyphNames.Add('Piwrarmenian', 1363);
  FAdobeGlyphNames.Add('plus', 43);
  FAdobeGlyphNames.Add('plusbelowcmb', 799);
  FAdobeGlyphNames.Add('pluscircle', 8853);
  FAdobeGlyphNames.Add('plusminus', 177);
  FAdobeGlyphNames.Add('plusmod', 726);
  FAdobeGlyphNames.Add('plusmonospace', 65291);
  FAdobeGlyphNames.Add('plussmall', 65122);
  FAdobeGlyphNames.Add('plussuperior', 8314);
  FAdobeGlyphNames.Add('pmonospace', 65360);
  FAdobeGlyphNames.Add('Pmonospace', 65328);
  FAdobeGlyphNames.Add('pmsquare', 13272);
  FAdobeGlyphNames.Add('pohiragana', 12413);
  FAdobeGlyphNames.Add('pointingindexdownwhite', 9759);
  FAdobeGlyphNames.Add('pointingindexleftwhite', 9756);
  FAdobeGlyphNames.Add('pointingindexrightwhite', 9758);
  FAdobeGlyphNames.Add('pointingindexupwhite', 9757);
  FAdobeGlyphNames.Add('pokatakana', 12509);
  FAdobeGlyphNames.Add('poplathai', 3611);
  FAdobeGlyphNames.Add('postalmark', 12306);
  FAdobeGlyphNames.Add('postalmarkface', 12320);
  FAdobeGlyphNames.Add('pparen', 9387);
  FAdobeGlyphNames.Add('precedes', 8826);
  FAdobeGlyphNames.Add('prescription', 8478);
  FAdobeGlyphNames.Add('primemod', 697);
  FAdobeGlyphNames.Add('primereversed', 8245);
  FAdobeGlyphNames.Add('product', 8719);
  FAdobeGlyphNames.Add('projective', 8965);
  FAdobeGlyphNames.Add('prolongedkana', 12540);
  FAdobeGlyphNames.Add('propellor', 8984);
  FAdobeGlyphNames.Add('propersubset', 8834);
  FAdobeGlyphNames.Add('propersuperset', 8835);
  FAdobeGlyphNames.Add('proportion', 8759);
  FAdobeGlyphNames.Add('proportional', 8733);
  FAdobeGlyphNames.Add('psi', 968);
  FAdobeGlyphNames.Add('Psi', 936);
  FAdobeGlyphNames.Add('psicyrillic', 1137);
  FAdobeGlyphNames.Add('Psicyrillic', 1136);
  FAdobeGlyphNames.Add('psilipneumatacyrilliccmb', 1158);
  FAdobeGlyphNames.Add('Psmall', 63344);
  FAdobeGlyphNames.Add('pssquare', 13232);
  FAdobeGlyphNames.Add('puhiragana', 12407);
  FAdobeGlyphNames.Add('pukatakana', 12503);
  FAdobeGlyphNames.Add('pvsquare', 13236);
  FAdobeGlyphNames.Add('pwsquare', 13242);
  FAdobeGlyphNames.Add('q', 113);
  FAdobeGlyphNames.Add('Q', 81);
  FAdobeGlyphNames.Add('qadeva', 2392);
  FAdobeGlyphNames.Add('qadmahebrew', 1448);
  FAdobeGlyphNames.Add('qafarabic', 1602);
  FAdobeGlyphNames.Add('qaffinalarabic', 65238);
  FAdobeGlyphNames.Add('qafinitialarabic', 65239);
  FAdobeGlyphNames.Add('qafmedialarabic', 65240);
  FAdobeGlyphNames.Add('qamats', 1464);
  FAdobeGlyphNames.Add('qamats10', 1464);
  FAdobeGlyphNames.Add('qamats1a', 1464);
  FAdobeGlyphNames.Add('qamats1c', 1464);
  FAdobeGlyphNames.Add('qamats27', 1464);
  FAdobeGlyphNames.Add('qamats29', 1464);
  FAdobeGlyphNames.Add('qamats33', 1464);
  FAdobeGlyphNames.Add('qamatsde', 1464);
  FAdobeGlyphNames.Add('qamatshebrew', 1464);
  FAdobeGlyphNames.Add('qamatsnarrowhebrew', 1464);
  FAdobeGlyphNames.Add('qamatsqatanhebrew', 1464);
  FAdobeGlyphNames.Add('qamatsqatannarrowhebrew', 1464);
  FAdobeGlyphNames.Add('qamatsqatanquarterhebrew', 1464);
  FAdobeGlyphNames.Add('qamatsqatanwidehebrew', 1464);
  FAdobeGlyphNames.Add('qamatsquarterhebrew', 1464);
  FAdobeGlyphNames.Add('qamatswidehebrew', 1464);
  FAdobeGlyphNames.Add('qarneyparahebrew', 1439);
  FAdobeGlyphNames.Add('qbopomofo', 12561);
  FAdobeGlyphNames.Add('qcircle', 9440);
  FAdobeGlyphNames.Add('Qcircle', 9414);
  FAdobeGlyphNames.Add('qhook', 672);
  FAdobeGlyphNames.Add('qmonospace', 65361);
  FAdobeGlyphNames.Add('Qmonospace', 65329);
  FAdobeGlyphNames.Add('qof', 1511);
  FAdobeGlyphNames.Add('qofdagesh', 64327);
  FAdobeGlyphNames.Add('qofdageshhebrew', 64327);
  FAdobeGlyphNames.Add('qofhatafpatah', 1511);
  FAdobeGlyphNames.Add('qofhatafpatahhebrew', 1511);
  FAdobeGlyphNames.Add('qofhatafsegol', 1511);
  FAdobeGlyphNames.Add('qofhatafsegolhebrew', 1511);
  FAdobeGlyphNames.Add('qofhebrew', 1511);
  FAdobeGlyphNames.Add('qofhiriq', 1511);
  FAdobeGlyphNames.Add('qofhiriqhebrew', 1511);
  FAdobeGlyphNames.Add('qofholam', 1511);
  FAdobeGlyphNames.Add('qofholamhebrew', 1511);
  FAdobeGlyphNames.Add('qofpatah', 1511);
  FAdobeGlyphNames.Add('qofpatahhebrew', 1511);
  FAdobeGlyphNames.Add('qofqamats', 1511);
  FAdobeGlyphNames.Add('qofqamatshebrew', 1511);
  FAdobeGlyphNames.Add('qofqubuts', 1511);
  FAdobeGlyphNames.Add('qofqubutshebrew', 1511);
  FAdobeGlyphNames.Add('qofsegol', 1511);
  FAdobeGlyphNames.Add('qofsegolhebrew', 1511);
  FAdobeGlyphNames.Add('qofsheva', 1511);
  FAdobeGlyphNames.Add('qofshevahebrew', 1511);
  FAdobeGlyphNames.Add('qoftsere', 1511);
  FAdobeGlyphNames.Add('qoftserehebrew', 1511);
  FAdobeGlyphNames.Add('qparen', 9388);
  FAdobeGlyphNames.Add('Qsmall', 63345);
  FAdobeGlyphNames.Add('quarternote', 9833);
  FAdobeGlyphNames.Add('qubuts', 1467);
  FAdobeGlyphNames.Add('qubuts18', 1467);
  FAdobeGlyphNames.Add('qubuts25', 1467);
  FAdobeGlyphNames.Add('qubuts31', 1467);
  FAdobeGlyphNames.Add('qubutshebrew', 1467);
  FAdobeGlyphNames.Add('qubutsnarrowhebrew', 1467);
  FAdobeGlyphNames.Add('qubutsquarterhebrew', 1467);
  FAdobeGlyphNames.Add('qubutswidehebrew', 1467);
  FAdobeGlyphNames.Add('question', 63);
  FAdobeGlyphNames.Add('questionarabic', 1567);
  FAdobeGlyphNames.Add('questionarmenian', 1374);
  FAdobeGlyphNames.Add('questiondown', 191);
  FAdobeGlyphNames.Add('questiondownsmall', 63423);
  FAdobeGlyphNames.Add('questiongreek', 894);
  FAdobeGlyphNames.Add('questionmonospace', 65311);
  FAdobeGlyphNames.Add('questionsmall', 63295);
  FAdobeGlyphNames.Add('quotedbl', 34);
  FAdobeGlyphNames.Add('quotedblbase', 8222);
  FAdobeGlyphNames.Add('quotedblleft', 8220);
  FAdobeGlyphNames.Add('quotedblmonospace', 65282);
  FAdobeGlyphNames.Add('quotedblprime', 12318);
  FAdobeGlyphNames.Add('quotedblprimereversed', 12317);
  FAdobeGlyphNames.Add('quotedblright', 8221);
  FAdobeGlyphNames.Add('quoteleft', 8216);
  FAdobeGlyphNames.Add('quoteleftreversed', 8219);
  FAdobeGlyphNames.Add('quotereversed', 8219);
  FAdobeGlyphNames.Add('quoteright', 8217);
  FAdobeGlyphNames.Add('quoterightn', 329);
  FAdobeGlyphNames.Add('quotesinglbase', 8218);
  FAdobeGlyphNames.Add('quotesingle', 39);
  FAdobeGlyphNames.Add('quotesinglemonospace', 65287);
  FAdobeGlyphNames.Add('r', 114);
  FAdobeGlyphNames.Add('R', 82);
  FAdobeGlyphNames.Add('raarmenian', 1404);
  FAdobeGlyphNames.Add('Raarmenian', 1356);
  FAdobeGlyphNames.Add('rabengali', 2480);
  FAdobeGlyphNames.Add('racute', 341);
  FAdobeGlyphNames.Add('Racute', 340);
  FAdobeGlyphNames.Add('radeva', 2352);
  FAdobeGlyphNames.Add('radical', 8730);
  FAdobeGlyphNames.Add('radicalex', 63717);
  FAdobeGlyphNames.Add('radoverssquare', 13230);
  FAdobeGlyphNames.Add('radoverssquaredsquare', 13231);
  FAdobeGlyphNames.Add('radsquare', 13229);
  FAdobeGlyphNames.Add('rafe', 1471);
  FAdobeGlyphNames.Add('rafehebrew', 1471);
  FAdobeGlyphNames.Add('ragujarati', 2736);
  FAdobeGlyphNames.Add('ragurmukhi', 2608);
  FAdobeGlyphNames.Add('rahiragana', 12425);
  FAdobeGlyphNames.Add('rakatakana', 12521);
  FAdobeGlyphNames.Add('rakatakanahalfwidth', 65431);
  FAdobeGlyphNames.Add('ralowerdiagonalbengali', 2545);
  FAdobeGlyphNames.Add('ramiddlediagonalbengali', 2544);
  FAdobeGlyphNames.Add('ramshorn', 612);
  FAdobeGlyphNames.Add('ratio', 8758);
  FAdobeGlyphNames.Add('rbopomofo', 12566);
  FAdobeGlyphNames.Add('rcaron', 345);
  FAdobeGlyphNames.Add('Rcaron', 344);
  FAdobeGlyphNames.Add('rcedilla', 343);
  FAdobeGlyphNames.Add('Rcedilla', 342);
  FAdobeGlyphNames.Add('rcircle', 9441);
  FAdobeGlyphNames.Add('Rcircle', 9415);
  FAdobeGlyphNames.Add('rcommaaccent', 343);
  FAdobeGlyphNames.Add('Rcommaaccent', 342);
  FAdobeGlyphNames.Add('rdblgrave', 529);
  FAdobeGlyphNames.Add('Rdblgrave', 528);
  FAdobeGlyphNames.Add('rdotaccent', 7769);
  FAdobeGlyphNames.Add('Rdotaccent', 7768);
  FAdobeGlyphNames.Add('rdotbelow', 7771);
  FAdobeGlyphNames.Add('Rdotbelow', 7770);
  FAdobeGlyphNames.Add('rdotbelowmacron', 7773);
  FAdobeGlyphNames.Add('Rdotbelowmacron', 7772);
  FAdobeGlyphNames.Add('referencemark', 8251);
  FAdobeGlyphNames.Add('reflexsubset', 8838);
  FAdobeGlyphNames.Add('reflexsuperset', 8839);
  FAdobeGlyphNames.Add('registered', 174);
  FAdobeGlyphNames.Add('registersans', 63720);
  FAdobeGlyphNames.Add('registerserif', 63194);
  FAdobeGlyphNames.Add('reharabic', 1585);
  FAdobeGlyphNames.Add('reharmenian', 1408);
  FAdobeGlyphNames.Add('Reharmenian', 1360);
  FAdobeGlyphNames.Add('rehfinalarabic', 65198);
  FAdobeGlyphNames.Add('rehiragana', 12428);
  FAdobeGlyphNames.Add('rehyehaleflamarabic', 1585);
  FAdobeGlyphNames.Add('rekatakana', 12524);
  FAdobeGlyphNames.Add('rekatakanahalfwidth', 65434);
  FAdobeGlyphNames.Add('resh', 1512);
  FAdobeGlyphNames.Add('reshdageshhebrew', 64328);
  FAdobeGlyphNames.Add('reshhatafpatah', 1512);
  FAdobeGlyphNames.Add('reshhatafpatahhebrew', 1512);
  FAdobeGlyphNames.Add('reshhatafsegol', 1512);
  FAdobeGlyphNames.Add('reshhatafsegolhebrew', 1512);
  FAdobeGlyphNames.Add('reshhebrew', 1512);
  FAdobeGlyphNames.Add('reshhiriq', 1512);
  FAdobeGlyphNames.Add('reshhiriqhebrew', 1512);
  FAdobeGlyphNames.Add('reshholam', 1512);
  FAdobeGlyphNames.Add('reshholamhebrew', 1512);
  FAdobeGlyphNames.Add('reshpatah', 1512);
  FAdobeGlyphNames.Add('reshpatahhebrew', 1512);
  FAdobeGlyphNames.Add('reshqamats', 1512);
  FAdobeGlyphNames.Add('reshqamatshebrew', 1512);
  FAdobeGlyphNames.Add('reshqubuts', 1512);
  FAdobeGlyphNames.Add('reshqubutshebrew', 1512);
  FAdobeGlyphNames.Add('reshsegol', 1512);
  FAdobeGlyphNames.Add('reshsegolhebrew', 1512);
  FAdobeGlyphNames.Add('reshsheva', 1512);
  FAdobeGlyphNames.Add('reshshevahebrew', 1512);
  FAdobeGlyphNames.Add('reshtsere', 1512);
  FAdobeGlyphNames.Add('reshtserehebrew', 1512);
  FAdobeGlyphNames.Add('reversedtilde', 8765);
  FAdobeGlyphNames.Add('reviahebrew', 1431);
  FAdobeGlyphNames.Add('reviamugrashhebrew', 1431);
  FAdobeGlyphNames.Add('revlogicalnot', 8976);
  FAdobeGlyphNames.Add('rfishhook', 638);
  FAdobeGlyphNames.Add('rfishhookreversed', 639);
  FAdobeGlyphNames.Add('Rfraktur', 8476);
  FAdobeGlyphNames.Add('rhabengali', 2525);
  FAdobeGlyphNames.Add('rhadeva', 2397);
  FAdobeGlyphNames.Add('rho', 961);
  FAdobeGlyphNames.Add('Rho', 929);
  FAdobeGlyphNames.Add('rhook', 637);
  FAdobeGlyphNames.Add('rhookturned', 635);
  FAdobeGlyphNames.Add('rhookturnedsuperior', 693);
  FAdobeGlyphNames.Add('rhosymbolgreek', 1009);
  FAdobeGlyphNames.Add('rhotichookmod', 734);
  FAdobeGlyphNames.Add('rieulacirclekorean', 12913);
  FAdobeGlyphNames.Add('rieulaparenkorean', 12817);
  FAdobeGlyphNames.Add('rieulcirclekorean', 12899);
  FAdobeGlyphNames.Add('rieulhieuhkorean', 12608);
  FAdobeGlyphNames.Add('rieulkiyeokkorean', 12602);
  FAdobeGlyphNames.Add('rieulkiyeoksioskorean', 12649);
  FAdobeGlyphNames.Add('rieulkorean', 12601);
  FAdobeGlyphNames.Add('rieulmieumkorean', 12603);
  FAdobeGlyphNames.Add('rieulpansioskorean', 12652);
  FAdobeGlyphNames.Add('rieulparenkorean', 12803);
  FAdobeGlyphNames.Add('rieulphieuphkorean', 12607);
  FAdobeGlyphNames.Add('rieulpieupkorean', 12604);
  FAdobeGlyphNames.Add('rieulpieupsioskorean', 12651);
  FAdobeGlyphNames.Add('rieulsioskorean', 12605);
  FAdobeGlyphNames.Add('rieulthieuthkorean', 12606);
  FAdobeGlyphNames.Add('rieultikeutkorean', 12650);
  FAdobeGlyphNames.Add('rieulyeorinhieuhkorean', 12653);
  FAdobeGlyphNames.Add('rightangle', 8735);
  FAdobeGlyphNames.Add('righttackbelowcmb', 793);
  FAdobeGlyphNames.Add('righttriangle', 8895);
  FAdobeGlyphNames.Add('rihiragana', 12426);
  FAdobeGlyphNames.Add('rikatakana', 12522);
  FAdobeGlyphNames.Add('rikatakanahalfwidth', 65432);
  FAdobeGlyphNames.Add('ring', 730);
  FAdobeGlyphNames.Add('ringbelowcmb', 805);
  FAdobeGlyphNames.Add('ringcmb', 778);
  FAdobeGlyphNames.Add('ringhalfleft', 703);
  FAdobeGlyphNames.Add('ringhalfleftarmenian', 1369);
  FAdobeGlyphNames.Add('ringhalfleftbelowcmb', 796);
  FAdobeGlyphNames.Add('ringhalfleftcentered', 723);
  FAdobeGlyphNames.Add('ringhalfright', 702);
  FAdobeGlyphNames.Add('ringhalfrightbelowcmb', 825);
  FAdobeGlyphNames.Add('ringhalfrightcentered', 722);
  FAdobeGlyphNames.Add('Ringsmall', 63228);
  FAdobeGlyphNames.Add('rinvertedbreve', 531);
  FAdobeGlyphNames.Add('Rinvertedbreve', 530);
  FAdobeGlyphNames.Add('rittorusquare', 13137);
  FAdobeGlyphNames.Add('rlinebelow', 7775);
  FAdobeGlyphNames.Add('Rlinebelow', 7774);
  FAdobeGlyphNames.Add('rlongleg', 636);
  FAdobeGlyphNames.Add('rlonglegturned', 634);
  FAdobeGlyphNames.Add('rmonospace', 65362);
  FAdobeGlyphNames.Add('Rmonospace', 65330);
  FAdobeGlyphNames.Add('rohiragana', 12429);
  FAdobeGlyphNames.Add('rokatakana', 12525);
  FAdobeGlyphNames.Add('rokatakanahalfwidth', 65435);
  FAdobeGlyphNames.Add('roruathai', 3619);
  FAdobeGlyphNames.Add('rparen', 9389);
  FAdobeGlyphNames.Add('rrabengali', 2524);
  FAdobeGlyphNames.Add('rradeva', 2353);
  FAdobeGlyphNames.Add('rragurmukhi', 2652);
  FAdobeGlyphNames.Add('rreharabic', 1681);
  FAdobeGlyphNames.Add('rrehfinalarabic', 64397);
  FAdobeGlyphNames.Add('rrvocalicbengali', 2528);
  FAdobeGlyphNames.Add('rrvocalicdeva', 2400);
  FAdobeGlyphNames.Add('rrvocalicgujarati', 2784);
  FAdobeGlyphNames.Add('rrvocalicvowelsignbengali', 2500);
  FAdobeGlyphNames.Add('rrvocalicvowelsigndeva', 2372);
  FAdobeGlyphNames.Add('rrvocalicvowelsigngujarati', 2756);
  FAdobeGlyphNames.Add('Rsmall', 63346);
  FAdobeGlyphNames.Add('Rsmallinverted', 641);
  FAdobeGlyphNames.Add('Rsmallinvertedsuperior', 694);
  FAdobeGlyphNames.Add('rsuperior', 63217);
  FAdobeGlyphNames.Add('rtblock', 9616);
  FAdobeGlyphNames.Add('rturned', 633);
  FAdobeGlyphNames.Add('rturnedsuperior', 692);
  FAdobeGlyphNames.Add('ruhiragana', 12427);
  FAdobeGlyphNames.Add('rukatakana', 12523);
  FAdobeGlyphNames.Add('rukatakanahalfwidth', 65433);
  FAdobeGlyphNames.Add('rupeemarkbengali', 2546);
  FAdobeGlyphNames.Add('rupeesignbengali', 2547);
  FAdobeGlyphNames.Add('rupiah', 63197);
  FAdobeGlyphNames.Add('ruthai', 3620);
  FAdobeGlyphNames.Add('rvocalicbengali', 2443);
  FAdobeGlyphNames.Add('rvocalicdeva', 2315);
  FAdobeGlyphNames.Add('rvocalicgujarati', 2699);
  FAdobeGlyphNames.Add('rvocalicvowelsignbengali', 2499);
  FAdobeGlyphNames.Add('rvocalicvowelsigndeva', 2371);
  FAdobeGlyphNames.Add('rvocalicvowelsigngujarati', 2755);
  FAdobeGlyphNames.Add('s', 115);
  FAdobeGlyphNames.Add('S', 83);
  FAdobeGlyphNames.Add('sabengali', 2488);
  FAdobeGlyphNames.Add('sacute', 347);
  FAdobeGlyphNames.Add('Sacute', 346);
  FAdobeGlyphNames.Add('sacutedotaccent', 7781);
  FAdobeGlyphNames.Add('Sacutedotaccent', 7780);
  FAdobeGlyphNames.Add('sadarabic', 1589);
  FAdobeGlyphNames.Add('sadeva', 2360);
  FAdobeGlyphNames.Add('sadfinalarabic', 65210);
  FAdobeGlyphNames.Add('sadinitialarabic', 65211);
  FAdobeGlyphNames.Add('sadmedialarabic', 65212);
  FAdobeGlyphNames.Add('sagujarati', 2744);
  FAdobeGlyphNames.Add('sagurmukhi', 2616);
  FAdobeGlyphNames.Add('sahiragana', 12373);
  FAdobeGlyphNames.Add('sakatakana', 12469);
  FAdobeGlyphNames.Add('sakatakanahalfwidth', 65403);
  FAdobeGlyphNames.Add('sallallahoualayhewasallamarabic', 65018);
  FAdobeGlyphNames.Add('samekh', 1505);
  FAdobeGlyphNames.Add('samekhdagesh', 64321);
  FAdobeGlyphNames.Add('samekhdageshhebrew', 64321);
  FAdobeGlyphNames.Add('samekhhebrew', 1505);
  FAdobeGlyphNames.Add('Sampigreek', 992);
  FAdobeGlyphNames.Add('saraaathai', 3634);
  FAdobeGlyphNames.Add('saraaethai', 3649);
  FAdobeGlyphNames.Add('saraaimaimalaithai', 3652);
  FAdobeGlyphNames.Add('saraaimaimuanthai', 3651);
  FAdobeGlyphNames.Add('saraamthai', 3635);
  FAdobeGlyphNames.Add('saraathai', 3632);
  FAdobeGlyphNames.Add('saraethai', 3648);
  FAdobeGlyphNames.Add('saraiileftthai', 63622);
  FAdobeGlyphNames.Add('saraiithai', 3637);
  FAdobeGlyphNames.Add('saraileftthai', 63621);
  FAdobeGlyphNames.Add('saraithai', 3636);
  FAdobeGlyphNames.Add('saraothai', 3650);
  FAdobeGlyphNames.Add('saraueeleftthai', 63624);
  FAdobeGlyphNames.Add('saraueethai', 3639);
  FAdobeGlyphNames.Add('saraueleftthai', 63623);
  FAdobeGlyphNames.Add('sarauethai', 3638);
  FAdobeGlyphNames.Add('sarauthai', 3640);
  FAdobeGlyphNames.Add('sarauuthai', 3641);
  FAdobeGlyphNames.Add('sbopomofo', 12569);
  FAdobeGlyphNames.Add('scaron', 353);
  FAdobeGlyphNames.Add('Scaron', 352);
  FAdobeGlyphNames.Add('scarondotaccent', 7783);
  FAdobeGlyphNames.Add('Scarondotaccent', 7782);
  FAdobeGlyphNames.Add('Scaronsmall', 63229);
  FAdobeGlyphNames.Add('scedilla', 351);
  FAdobeGlyphNames.Add('Scedilla', 350);
  FAdobeGlyphNames.Add('schwa', 601);
  FAdobeGlyphNames.Add('Schwa', 399);
  FAdobeGlyphNames.Add('schwacyrillic', 1241);
  FAdobeGlyphNames.Add('Schwacyrillic', 1240);
  FAdobeGlyphNames.Add('schwadieresiscyrillic', 1243);
  FAdobeGlyphNames.Add('Schwadieresiscyrillic', 1242);
  FAdobeGlyphNames.Add('schwahook', 602);
  FAdobeGlyphNames.Add('scircle', 9442);
  FAdobeGlyphNames.Add('Scircle', 9416);
  FAdobeGlyphNames.Add('scircumflex', 349);
  FAdobeGlyphNames.Add('Scircumflex', 348);
  FAdobeGlyphNames.Add('scommaaccent', 537);
  FAdobeGlyphNames.Add('Scommaaccent', 536);
  FAdobeGlyphNames.Add('sdotaccent', 7777);
  FAdobeGlyphNames.Add('Sdotaccent', 7776);
  FAdobeGlyphNames.Add('sdotbelow', 7779);
  FAdobeGlyphNames.Add('Sdotbelow', 7778);
  FAdobeGlyphNames.Add('sdotbelowdotaccent', 7785);
  FAdobeGlyphNames.Add('Sdotbelowdotaccent', 7784);
  FAdobeGlyphNames.Add('seagullbelowcmb', 828);
  FAdobeGlyphNames.Add('second', 8243);
  FAdobeGlyphNames.Add('secondtonechinese', 714);
  FAdobeGlyphNames.Add('section', 167);
  FAdobeGlyphNames.Add('seenarabic', 1587);
  FAdobeGlyphNames.Add('seenfinalarabic', 65202);
  FAdobeGlyphNames.Add('seeninitialarabic', 65203);
  FAdobeGlyphNames.Add('seenmedialarabic', 65204);
  FAdobeGlyphNames.Add('segol', 1462);
  FAdobeGlyphNames.Add('segol13', 1462);
  FAdobeGlyphNames.Add('segol1f', 1462);
  FAdobeGlyphNames.Add('segol2c', 1462);
  FAdobeGlyphNames.Add('segolhebrew', 1462);
  FAdobeGlyphNames.Add('segolnarrowhebrew', 1462);
  FAdobeGlyphNames.Add('segolquarterhebrew', 1462);
  FAdobeGlyphNames.Add('segoltahebrew', 1426);
  FAdobeGlyphNames.Add('segolwidehebrew', 1462);
  FAdobeGlyphNames.Add('seharmenian', 1405);
  FAdobeGlyphNames.Add('Seharmenian', 1357);
  FAdobeGlyphNames.Add('sehiragana', 12379);
  FAdobeGlyphNames.Add('sekatakana', 12475);
  FAdobeGlyphNames.Add('sekatakanahalfwidth', 65406);
  FAdobeGlyphNames.Add('semicolon', 59);
  FAdobeGlyphNames.Add('semicolonarabic', 1563);
  FAdobeGlyphNames.Add('semicolonmonospace', 65307);
  FAdobeGlyphNames.Add('semicolonsmall', 65108);
  FAdobeGlyphNames.Add('semivoicedmarkkana', 12444);
  FAdobeGlyphNames.Add('semivoicedmarkkanahalfwidth', 65439);
  FAdobeGlyphNames.Add('sentisquare', 13090);
  FAdobeGlyphNames.Add('sentosquare', 13091);
  FAdobeGlyphNames.Add('seven', 55);
  FAdobeGlyphNames.Add('sevenarabic', 1639);
  FAdobeGlyphNames.Add('sevenbengali', 2541);
  FAdobeGlyphNames.Add('sevencircle', 9318);
  FAdobeGlyphNames.Add('sevencircleinversesansserif', 10128);
  FAdobeGlyphNames.Add('sevendeva', 2413);
  FAdobeGlyphNames.Add('seveneighths', 8542);
  FAdobeGlyphNames.Add('sevengujarati', 2797);
  FAdobeGlyphNames.Add('sevengurmukhi', 2669);
  FAdobeGlyphNames.Add('sevenhackarabic', 1639);
  FAdobeGlyphNames.Add('sevenhangzhou', 12327);
  FAdobeGlyphNames.Add('sevenideographicparen', 12838);
  FAdobeGlyphNames.Add('seveninferior', 8327);
  FAdobeGlyphNames.Add('sevenmonospace', 65303);
  FAdobeGlyphNames.Add('sevenoldstyle', 63287);
  FAdobeGlyphNames.Add('sevenparen', 9338);
  FAdobeGlyphNames.Add('sevenperiod', 9358);
  FAdobeGlyphNames.Add('sevenpersian', 1783);
  FAdobeGlyphNames.Add('sevenroman', 8566);
  FAdobeGlyphNames.Add('Sevenroman', 8550);
  FAdobeGlyphNames.Add('sevensuperior', 8311);
  FAdobeGlyphNames.Add('seventeencircle', 9328);
  FAdobeGlyphNames.Add('seventeenparen', 9348);
  FAdobeGlyphNames.Add('seventeenperiod', 9368);
  FAdobeGlyphNames.Add('seventhai', 3671);
  FAdobeGlyphNames.Add('SF010000', 9484);
  FAdobeGlyphNames.Add('SF020000', 9492);
  FAdobeGlyphNames.Add('SF030000', 9488);
  FAdobeGlyphNames.Add('SF040000', 9496);
  FAdobeGlyphNames.Add('SF050000', 9532);
  FAdobeGlyphNames.Add('SF060000', 9516);
  FAdobeGlyphNames.Add('SF070000', 9524);
  FAdobeGlyphNames.Add('SF080000', 9500);
  FAdobeGlyphNames.Add('SF090000', 9508);
  FAdobeGlyphNames.Add('SF100000', 9472);
  FAdobeGlyphNames.Add('SF110000', 9474);
  FAdobeGlyphNames.Add('SF190000', 9569);
  FAdobeGlyphNames.Add('SF200000', 9570);
  FAdobeGlyphNames.Add('SF210000', 9558);
  FAdobeGlyphNames.Add('SF220000', 9557);
  FAdobeGlyphNames.Add('SF230000', 9571);
  FAdobeGlyphNames.Add('SF240000', 9553);
  FAdobeGlyphNames.Add('SF250000', 9559);
  FAdobeGlyphNames.Add('SF260000', 9565);
  FAdobeGlyphNames.Add('SF270000', 9564);
  FAdobeGlyphNames.Add('SF280000', 9563);
  FAdobeGlyphNames.Add('SF360000', 9566);
  FAdobeGlyphNames.Add('SF370000', 9567);
  FAdobeGlyphNames.Add('SF380000', 9562);
  FAdobeGlyphNames.Add('SF390000', 9556);
  FAdobeGlyphNames.Add('SF400000', 9577);
  FAdobeGlyphNames.Add('SF410000', 9574);
  FAdobeGlyphNames.Add('SF420000', 9568);
  FAdobeGlyphNames.Add('SF430000', 9552);
  FAdobeGlyphNames.Add('SF440000', 9580);
  FAdobeGlyphNames.Add('SF450000', 9575);
  FAdobeGlyphNames.Add('SF460000', 9576);
  FAdobeGlyphNames.Add('SF470000', 9572);
  FAdobeGlyphNames.Add('SF480000', 9573);
  FAdobeGlyphNames.Add('SF490000', 9561);
  FAdobeGlyphNames.Add('SF500000', 9560);
  FAdobeGlyphNames.Add('SF510000', 9554);
  FAdobeGlyphNames.Add('SF520000', 9555);
  FAdobeGlyphNames.Add('SF530000', 9579);
  FAdobeGlyphNames.Add('SF540000', 9578);
  FAdobeGlyphNames.Add('sfthyphen', 173);
  FAdobeGlyphNames.Add('shaarmenian', 1399);
  FAdobeGlyphNames.Add('Shaarmenian', 1351);
  FAdobeGlyphNames.Add('shabengali', 2486);
  FAdobeGlyphNames.Add('shacyrillic', 1096);
  FAdobeGlyphNames.Add('Shacyrillic', 1064);
  FAdobeGlyphNames.Add('shaddaarabic', 1617);
  FAdobeGlyphNames.Add('shaddadammaarabic', 64609);
  FAdobeGlyphNames.Add('shaddadammatanarabic', 64606);
  FAdobeGlyphNames.Add('shaddafathaarabic', 64608);
  FAdobeGlyphNames.Add('shaddafathatanarabic', 1617);
  FAdobeGlyphNames.Add('shaddakasraarabic', 64610);
  FAdobeGlyphNames.Add('shaddakasratanarabic', 64607);
  FAdobeGlyphNames.Add('shade', 9618);
  FAdobeGlyphNames.Add('shadedark', 9619);
  FAdobeGlyphNames.Add('shadelight', 9617);
  FAdobeGlyphNames.Add('shademedium', 9618);
  FAdobeGlyphNames.Add('shadeva', 2358);
  FAdobeGlyphNames.Add('shagujarati', 2742);
  FAdobeGlyphNames.Add('shagurmukhi', 2614);
  FAdobeGlyphNames.Add('shalshelethebrew', 1427);
  FAdobeGlyphNames.Add('shbopomofo', 12565);
  FAdobeGlyphNames.Add('shchacyrillic', 1097);
  FAdobeGlyphNames.Add('Shchacyrillic', 1065);
  FAdobeGlyphNames.Add('sheenarabic', 1588);
  FAdobeGlyphNames.Add('sheenfinalarabic', 65206);
  FAdobeGlyphNames.Add('sheeninitialarabic', 65207);
  FAdobeGlyphNames.Add('sheenmedialarabic', 65208);
  FAdobeGlyphNames.Add('sheicoptic', 995);
  FAdobeGlyphNames.Add('Sheicoptic', 994);
  FAdobeGlyphNames.Add('sheqel', 8362);
  FAdobeGlyphNames.Add('sheqelhebrew', 8362);
  FAdobeGlyphNames.Add('sheva', 1456);
  FAdobeGlyphNames.Add('sheva115', 1456);
  FAdobeGlyphNames.Add('sheva15', 1456);
  FAdobeGlyphNames.Add('sheva22', 1456);
  FAdobeGlyphNames.Add('sheva2e', 1456);
  FAdobeGlyphNames.Add('shevahebrew', 1456);
  FAdobeGlyphNames.Add('shevanarrowhebrew', 1456);
  FAdobeGlyphNames.Add('shevaquarterhebrew', 1456);
  FAdobeGlyphNames.Add('shevawidehebrew', 1456);
  FAdobeGlyphNames.Add('shhacyrillic', 1211);
  FAdobeGlyphNames.Add('Shhacyrillic', 1210);
  FAdobeGlyphNames.Add('shimacoptic', 1005);
  FAdobeGlyphNames.Add('Shimacoptic', 1004);
  FAdobeGlyphNames.Add('shin', 1513);
  FAdobeGlyphNames.Add('shindagesh', 64329);
  FAdobeGlyphNames.Add('shindageshhebrew', 64329);
  FAdobeGlyphNames.Add('shindageshshindot', 64300);
  FAdobeGlyphNames.Add('shindageshshindothebrew', 64300);
  FAdobeGlyphNames.Add('shindageshsindot', 64301);
  FAdobeGlyphNames.Add('shindageshsindothebrew', 64301);
  FAdobeGlyphNames.Add('shindothebrew', 1473);
  FAdobeGlyphNames.Add('shinhebrew', 1513);
  FAdobeGlyphNames.Add('shinshindot', 64298);
  FAdobeGlyphNames.Add('shinshindothebrew', 64298);
  FAdobeGlyphNames.Add('shinsindot', 64299);
  FAdobeGlyphNames.Add('shinsindothebrew', 64299);
  FAdobeGlyphNames.Add('shook', 642);
  FAdobeGlyphNames.Add('sigma', 963);
  FAdobeGlyphNames.Add('Sigma', 931);
  FAdobeGlyphNames.Add('sigma1', 962);
  FAdobeGlyphNames.Add('sigmafinal', 962);
  FAdobeGlyphNames.Add('sigmalunatesymbolgreek', 1010);
  FAdobeGlyphNames.Add('sihiragana', 12375);
  FAdobeGlyphNames.Add('sikatakana', 12471);
  FAdobeGlyphNames.Add('sikatakanahalfwidth', 65404);
  FAdobeGlyphNames.Add('siluqhebrew', 1469);
  FAdobeGlyphNames.Add('siluqlefthebrew', 1469);
  FAdobeGlyphNames.Add('similar', 8764);
  FAdobeGlyphNames.Add('sindothebrew', 1474);
  FAdobeGlyphNames.Add('siosacirclekorean', 12916);
  FAdobeGlyphNames.Add('siosaparenkorean', 12820);
  FAdobeGlyphNames.Add('sioscieuckorean', 12670);
  FAdobeGlyphNames.Add('sioscirclekorean', 12902);
  FAdobeGlyphNames.Add('sioskiyeokkorean', 12666);
  FAdobeGlyphNames.Add('sioskorean', 12613);
  FAdobeGlyphNames.Add('siosnieunkorean', 12667);
  FAdobeGlyphNames.Add('siosparenkorean', 12806);
  FAdobeGlyphNames.Add('siospieupkorean', 12669);
  FAdobeGlyphNames.Add('siostikeutkorean', 12668);
  FAdobeGlyphNames.Add('six', 54);
  FAdobeGlyphNames.Add('sixarabic', 1638);
  FAdobeGlyphNames.Add('sixbengali', 2540);
  FAdobeGlyphNames.Add('sixcircle', 9317);
  FAdobeGlyphNames.Add('sixcircleinversesansserif', 10127);
  FAdobeGlyphNames.Add('sixdeva', 2412);
  FAdobeGlyphNames.Add('sixgujarati', 2796);
  FAdobeGlyphNames.Add('sixgurmukhi', 2668);
  FAdobeGlyphNames.Add('sixhackarabic', 1638);
  FAdobeGlyphNames.Add('sixhangzhou', 12326);
  FAdobeGlyphNames.Add('sixideographicparen', 12837);
  FAdobeGlyphNames.Add('sixinferior', 8326);
  FAdobeGlyphNames.Add('sixmonospace', 65302);
  FAdobeGlyphNames.Add('sixoldstyle', 63286);
  FAdobeGlyphNames.Add('sixparen', 9337);
  FAdobeGlyphNames.Add('sixperiod', 9357);
  FAdobeGlyphNames.Add('sixpersian', 1782);
  FAdobeGlyphNames.Add('sixroman', 8565);
  FAdobeGlyphNames.Add('Sixroman', 8549);
  FAdobeGlyphNames.Add('sixsuperior', 8310);
  FAdobeGlyphNames.Add('sixteencircle', 9327);
  FAdobeGlyphNames.Add('sixteencurrencydenominatorbengali', 2553);
  FAdobeGlyphNames.Add('sixteenparen', 9347);
  FAdobeGlyphNames.Add('sixteenperiod', 9367);
  FAdobeGlyphNames.Add('sixthai', 3670);
  FAdobeGlyphNames.Add('slash', 47);
  FAdobeGlyphNames.Add('slashmonospace', 65295);
  FAdobeGlyphNames.Add('slong', 383);
  FAdobeGlyphNames.Add('slongdotaccent', 7835);
  FAdobeGlyphNames.Add('smileface', 9786);
  FAdobeGlyphNames.Add('smonospace', 65363);
  FAdobeGlyphNames.Add('Smonospace', 65331);
  FAdobeGlyphNames.Add('sofpasuqhebrew', 1475);
  FAdobeGlyphNames.Add('softhyphen', 173);
  FAdobeGlyphNames.Add('softsigncyrillic', 1100);
  FAdobeGlyphNames.Add('Softsigncyrillic', 1068);
  FAdobeGlyphNames.Add('sohiragana', 12381);
  FAdobeGlyphNames.Add('sokatakana', 12477);
  FAdobeGlyphNames.Add('sokatakanahalfwidth', 65407);
  FAdobeGlyphNames.Add('soliduslongoverlaycmb', 824);
  FAdobeGlyphNames.Add('solidusshortoverlaycmb', 823);
  FAdobeGlyphNames.Add('sorusithai', 3625);
  FAdobeGlyphNames.Add('sosalathai', 3624);
  FAdobeGlyphNames.Add('sosothai', 3595);
  FAdobeGlyphNames.Add('sosuathai', 3626);
  FAdobeGlyphNames.Add('space', 32);
  FAdobeGlyphNames.Add('spacehackarabic', 32);
  FAdobeGlyphNames.Add('spade', 9824);
  FAdobeGlyphNames.Add('spadesuitblack', 9824);
  FAdobeGlyphNames.Add('spadesuitwhite', 9828);
  FAdobeGlyphNames.Add('sparen', 9390);
  FAdobeGlyphNames.Add('squarebelowcmb', 827);
  FAdobeGlyphNames.Add('squarecc', 13252);
  FAdobeGlyphNames.Add('squarecm', 13213);
  FAdobeGlyphNames.Add('squarediagonalcrosshatchfill', 9641);
  FAdobeGlyphNames.Add('squarehorizontalfill', 9636);
  FAdobeGlyphNames.Add('squarekg', 13199);
  FAdobeGlyphNames.Add('squarekm', 13214);
  FAdobeGlyphNames.Add('squarekmcapital', 13262);
  FAdobeGlyphNames.Add('squareln', 13265);
  FAdobeGlyphNames.Add('squarelog', 13266);
  FAdobeGlyphNames.Add('squaremg', 13198);
  FAdobeGlyphNames.Add('squaremil', 13269);
  FAdobeGlyphNames.Add('squaremm', 13212);
  FAdobeGlyphNames.Add('squaremsquared', 13217);
  FAdobeGlyphNames.Add('squareorthogonalcrosshatchfill', 9638);
  FAdobeGlyphNames.Add('squareupperlefttolowerrightfill', 9639);
  FAdobeGlyphNames.Add('squareupperrighttolowerleftfill', 9640);
  FAdobeGlyphNames.Add('squareverticalfill', 9637);
  FAdobeGlyphNames.Add('squarewhitewithsmallblack', 9635);
  FAdobeGlyphNames.Add('srsquare', 13275);
  FAdobeGlyphNames.Add('ssabengali', 2487);
  FAdobeGlyphNames.Add('ssadeva', 2359);
  FAdobeGlyphNames.Add('ssagujarati', 2743);
  FAdobeGlyphNames.Add('ssangcieuckorean', 12617);
  FAdobeGlyphNames.Add('ssanghieuhkorean', 12677);
  FAdobeGlyphNames.Add('ssangieungkorean', 12672);
  FAdobeGlyphNames.Add('ssangkiyeokkorean', 12594);
  FAdobeGlyphNames.Add('ssangnieunkorean', 12645);
  FAdobeGlyphNames.Add('ssangpieupkorean', 12611);
  FAdobeGlyphNames.Add('ssangsioskorean', 12614);
  FAdobeGlyphNames.Add('ssangtikeutkorean', 12600);
  FAdobeGlyphNames.Add('Ssmall', 63347);
  FAdobeGlyphNames.Add('ssuperior', 63218);
  FAdobeGlyphNames.Add('sterling', 163);
  FAdobeGlyphNames.Add('sterlingmonospace', 65505);
  FAdobeGlyphNames.Add('Stigmagreek', 986);
  FAdobeGlyphNames.Add('strokelongoverlaycmb', 822);
  FAdobeGlyphNames.Add('strokeshortoverlaycmb', 821);
  FAdobeGlyphNames.Add('subset', 8834);
  FAdobeGlyphNames.Add('subsetnotequal', 8842);
  FAdobeGlyphNames.Add('subsetorequal', 8838);
  FAdobeGlyphNames.Add('succeeds', 8827);
  FAdobeGlyphNames.Add('suchthat', 8715);
  FAdobeGlyphNames.Add('suhiragana', 12377);
  FAdobeGlyphNames.Add('sukatakana', 12473);
  FAdobeGlyphNames.Add('sukatakanahalfwidth', 65405);
  FAdobeGlyphNames.Add('sukunarabic', 1618);
  FAdobeGlyphNames.Add('summation', 8721);
  FAdobeGlyphNames.Add('sun', 9788);
  FAdobeGlyphNames.Add('superset', 8835);
  FAdobeGlyphNames.Add('supersetnotequal', 8843);
  FAdobeGlyphNames.Add('supersetorequal', 8839);
  FAdobeGlyphNames.Add('svsquare', 13276);
  FAdobeGlyphNames.Add('syouwaerasquare', 13180);
  FAdobeGlyphNames.Add('t', 116);
  FAdobeGlyphNames.Add('T', 84);
  FAdobeGlyphNames.Add('tabengali', 2468);
  FAdobeGlyphNames.Add('tackdown', 8868);
  FAdobeGlyphNames.Add('tackleft', 8867);
  FAdobeGlyphNames.Add('tadeva', 2340);
  FAdobeGlyphNames.Add('tagujarati', 2724);
  FAdobeGlyphNames.Add('tagurmukhi', 2596);
  FAdobeGlyphNames.Add('taharabic', 1591);
  FAdobeGlyphNames.Add('tahfinalarabic', 65218);
  FAdobeGlyphNames.Add('tahinitialarabic', 65219);
  FAdobeGlyphNames.Add('tahiragana', 12383);
  FAdobeGlyphNames.Add('tahmedialarabic', 65220);
  FAdobeGlyphNames.Add('taisyouerasquare', 13181);
  FAdobeGlyphNames.Add('takatakana', 12479);
  FAdobeGlyphNames.Add('takatakanahalfwidth', 65408);
  FAdobeGlyphNames.Add('tatweelarabic', 1600);
  FAdobeGlyphNames.Add('tau', 964);
  FAdobeGlyphNames.Add('Tau', 932);
  FAdobeGlyphNames.Add('tav', 1514);
  FAdobeGlyphNames.Add('tavdages', 64330);
  FAdobeGlyphNames.Add('tavdagesh', 64330);
  FAdobeGlyphNames.Add('tavdageshhebrew', 64330);
  FAdobeGlyphNames.Add('tavhebrew', 1514);
  FAdobeGlyphNames.Add('tbar', 359);
  FAdobeGlyphNames.Add('Tbar', 358);
  FAdobeGlyphNames.Add('tbopomofo', 12554);
  FAdobeGlyphNames.Add('tcaron', 357);
  FAdobeGlyphNames.Add('Tcaron', 356);
  FAdobeGlyphNames.Add('tccurl', 680);
  FAdobeGlyphNames.Add('tcedilla', 355);
  FAdobeGlyphNames.Add('Tcedilla', 354);
  FAdobeGlyphNames.Add('tcheharabic', 1670);
  FAdobeGlyphNames.Add('tchehfinalarabic', 64379);
  FAdobeGlyphNames.Add('tchehinitialarabic', 64380);
  FAdobeGlyphNames.Add('tchehmedialarabic', 64381);
  FAdobeGlyphNames.Add('tchehmeeminitialarabic', 64380);
  FAdobeGlyphNames.Add('tcircle', 9443);
  FAdobeGlyphNames.Add('Tcircle', 9417);
  FAdobeGlyphNames.Add('tcircumflexbelow', 7793);
  FAdobeGlyphNames.Add('Tcircumflexbelow', 7792);
  FAdobeGlyphNames.Add('tcommaaccent', 355);
  FAdobeGlyphNames.Add('Tcommaaccent', 354);
  FAdobeGlyphNames.Add('tdieresis', 7831);
  FAdobeGlyphNames.Add('tdotaccent', 7787);
  FAdobeGlyphNames.Add('Tdotaccent', 7786);
  FAdobeGlyphNames.Add('tdotbelow', 7789);
  FAdobeGlyphNames.Add('Tdotbelow', 7788);
  FAdobeGlyphNames.Add('tecyrillic', 1090);
  FAdobeGlyphNames.Add('Tecyrillic', 1058);
  FAdobeGlyphNames.Add('tedescendercyrillic', 1197);
  FAdobeGlyphNames.Add('Tedescendercyrillic', 1196);
  FAdobeGlyphNames.Add('teharabic', 1578);
  FAdobeGlyphNames.Add('tehfinalarabic', 65174);
  FAdobeGlyphNames.Add('tehhahinitialarabic', 64674);
  FAdobeGlyphNames.Add('tehhahisolatedarabic', 64524);
  FAdobeGlyphNames.Add('tehinitialarabic', 65175);
  FAdobeGlyphNames.Add('tehiragana', 12390);
  FAdobeGlyphNames.Add('tehjeeminitialarabic', 64673);
  FAdobeGlyphNames.Add('tehjeemisolatedarabic', 64523);
  FAdobeGlyphNames.Add('tehmarbutaarabic', 1577);
  FAdobeGlyphNames.Add('tehmarbutafinalarabic', 65172);
  FAdobeGlyphNames.Add('tehmedialarabic', 65176);
  FAdobeGlyphNames.Add('tehmeeminitialarabic', 64676);
  FAdobeGlyphNames.Add('tehmeemisolatedarabic', 64526);
  FAdobeGlyphNames.Add('tehnoonfinalarabic', 64627);
  FAdobeGlyphNames.Add('tekatakana', 12486);
  FAdobeGlyphNames.Add('tekatakanahalfwidth', 65411);
  FAdobeGlyphNames.Add('telephone', 8481);
  FAdobeGlyphNames.Add('telephoneblack', 9742);
  FAdobeGlyphNames.Add('telishagedolahebrew', 1440);
  FAdobeGlyphNames.Add('telishaqetanahebrew', 1449);
  FAdobeGlyphNames.Add('tencircle', 9321);
  FAdobeGlyphNames.Add('tenideographicparen', 12841);
  FAdobeGlyphNames.Add('tenparen', 9341);
  FAdobeGlyphNames.Add('tenperiod', 9361);
  FAdobeGlyphNames.Add('tenroman', 8569);
  FAdobeGlyphNames.Add('Tenroman', 8553);
  FAdobeGlyphNames.Add('tesh', 679);
  FAdobeGlyphNames.Add('tet', 1496);
  FAdobeGlyphNames.Add('tetdagesh', 64312);
  FAdobeGlyphNames.Add('tetdageshhebrew', 64312);
  FAdobeGlyphNames.Add('tethebrew', 1496);
  FAdobeGlyphNames.Add('tetsecyrillic', 1205);
  FAdobeGlyphNames.Add('Tetsecyrillic', 1204);
  FAdobeGlyphNames.Add('tevirhebrew', 1435);
  FAdobeGlyphNames.Add('tevirlefthebrew', 1435);
  FAdobeGlyphNames.Add('thabengali', 2469);
  FAdobeGlyphNames.Add('thadeva', 2341);
  FAdobeGlyphNames.Add('thagujarati', 2725);
  FAdobeGlyphNames.Add('thagurmukhi', 2597);
  FAdobeGlyphNames.Add('thalarabic', 1584);
  FAdobeGlyphNames.Add('thalfinalarabic', 65196);
  FAdobeGlyphNames.Add('thanthakhatlowleftthai', 63640);
  FAdobeGlyphNames.Add('thanthakhatlowrightthai', 63639);
  FAdobeGlyphNames.Add('thanthakhatthai', 3660);
  FAdobeGlyphNames.Add('thanthakhatupperleftthai', 63638);
  FAdobeGlyphNames.Add('theharabic', 1579);
  FAdobeGlyphNames.Add('thehfinalarabic', 65178);
  FAdobeGlyphNames.Add('thehinitialarabic', 65179);
  FAdobeGlyphNames.Add('thehmedialarabic', 65180);
  FAdobeGlyphNames.Add('thereexists', 8707);
  FAdobeGlyphNames.Add('therefore', 8756);
  FAdobeGlyphNames.Add('theta', 952);
  FAdobeGlyphNames.Add('Theta', 920);
  FAdobeGlyphNames.Add('theta1', 977);
end;

procedure TdxFontFileUnicodeConverter.InitializeAdobeGlyphNames4;
begin
  FAdobeGlyphNames.Add('thetasymbolgreek', 977);
  FAdobeGlyphNames.Add('thieuthacirclekorean', 12921);
  FAdobeGlyphNames.Add('thieuthaparenkorean', 12825);
  FAdobeGlyphNames.Add('thieuthcirclekorean', 12907);
  FAdobeGlyphNames.Add('thieuthkorean', 12620);
  FAdobeGlyphNames.Add('thieuthparenkorean', 12811);
  FAdobeGlyphNames.Add('thirteencircle', 9324);
  FAdobeGlyphNames.Add('thirteenparen', 9344);
  FAdobeGlyphNames.Add('thirteenperiod', 9364);
  FAdobeGlyphNames.Add('thonangmonthothai', 3601);
  FAdobeGlyphNames.Add('thook', 429);
  FAdobeGlyphNames.Add('Thook', 428);
  FAdobeGlyphNames.Add('thophuthaothai', 3602);
  FAdobeGlyphNames.Add('thorn', 254);
  FAdobeGlyphNames.Add('Thorn', 222);
  FAdobeGlyphNames.Add('Thornsmall', 63486);
  FAdobeGlyphNames.Add('thothahanthai', 3607);
  FAdobeGlyphNames.Add('thothanthai', 3600);
  FAdobeGlyphNames.Add('thothongthai', 3608);
  FAdobeGlyphNames.Add('thothungthai', 3606);
  FAdobeGlyphNames.Add('thousandcyrillic', 1154);
  FAdobeGlyphNames.Add('thousandsseparatorarabic', 1644);
  FAdobeGlyphNames.Add('thousandsseparatorpersian', 1644);
  FAdobeGlyphNames.Add('three', 51);
  FAdobeGlyphNames.Add('threearabic', 1635);
  FAdobeGlyphNames.Add('threebengali', 2537);
  FAdobeGlyphNames.Add('threecircle', 9314);
  FAdobeGlyphNames.Add('threecircleinversesansserif', 10124);
  FAdobeGlyphNames.Add('threedeva', 2409);
  FAdobeGlyphNames.Add('threeeighths', 8540);
  FAdobeGlyphNames.Add('threegujarati', 2793);
  FAdobeGlyphNames.Add('threegurmukhi', 2665);
  FAdobeGlyphNames.Add('threehackarabic', 1635);
  FAdobeGlyphNames.Add('threehangzhou', 12323);
  FAdobeGlyphNames.Add('threeideographicparen', 12834);
  FAdobeGlyphNames.Add('threeinferior', 8323);
  FAdobeGlyphNames.Add('threemonospace', 65299);
  FAdobeGlyphNames.Add('threenumeratorbengali', 2550);
  FAdobeGlyphNames.Add('threeoldstyle', 63283);
  FAdobeGlyphNames.Add('threeparen', 9334);
  FAdobeGlyphNames.Add('threeperiod', 9354);
  FAdobeGlyphNames.Add('threepersian', 1779);
  FAdobeGlyphNames.Add('threequarters', 190);
  FAdobeGlyphNames.Add('threequartersemdash', 63198);
  FAdobeGlyphNames.Add('threeroman', 8562);
  FAdobeGlyphNames.Add('Threeroman', 8546);
  FAdobeGlyphNames.Add('threesuperior', 179);
  FAdobeGlyphNames.Add('threethai', 3667);
  FAdobeGlyphNames.Add('thzsquare', 13204);
  FAdobeGlyphNames.Add('tihiragana', 12385);
  FAdobeGlyphNames.Add('tikatakana', 12481);
  FAdobeGlyphNames.Add('tikatakanahalfwidth', 65409);
  FAdobeGlyphNames.Add('tikeutacirclekorean', 12912);
  FAdobeGlyphNames.Add('tikeutaparenkorean', 12816);
  FAdobeGlyphNames.Add('tikeutcirclekorean', 12898);
  FAdobeGlyphNames.Add('tikeutkorean', 12599);
  FAdobeGlyphNames.Add('tikeutparenkorean', 12802);
  FAdobeGlyphNames.Add('tilde', 732);
  FAdobeGlyphNames.Add('tildebelowcmb', 816);
  FAdobeGlyphNames.Add('tildecmb', 771);
  FAdobeGlyphNames.Add('tildecomb', 771);
  FAdobeGlyphNames.Add('tildedoublecmb', 864);
  FAdobeGlyphNames.Add('tildeoperator', 8764);
  FAdobeGlyphNames.Add('tildeoverlaycmb', 820);
  FAdobeGlyphNames.Add('Tildesmall', 63230);
  FAdobeGlyphNames.Add('tildeverticalcmb', 830);
  FAdobeGlyphNames.Add('timescircle', 8855);
  FAdobeGlyphNames.Add('tipehahebrew', 1430);
  FAdobeGlyphNames.Add('tipehalefthebrew', 1430);
  FAdobeGlyphNames.Add('tippigurmukhi', 2672);
  FAdobeGlyphNames.Add('titlocyrilliccmb', 1155);
  FAdobeGlyphNames.Add('tiwnarmenian', 1407);
  FAdobeGlyphNames.Add('Tiwnarmenian', 1359);
  FAdobeGlyphNames.Add('tlinebelow', 7791);
  FAdobeGlyphNames.Add('Tlinebelow', 7790);
  FAdobeGlyphNames.Add('tmonospace', 65364);
  FAdobeGlyphNames.Add('Tmonospace', 65332);
  FAdobeGlyphNames.Add('toarmenian', 1385);
  FAdobeGlyphNames.Add('Toarmenian', 1337);
  FAdobeGlyphNames.Add('tohiragana', 12392);
  FAdobeGlyphNames.Add('tokatakana', 12488);
  FAdobeGlyphNames.Add('tokatakanahalfwidth', 65412);
  FAdobeGlyphNames.Add('tonebarextrahighmod', 741);
  FAdobeGlyphNames.Add('tonebarextralowmod', 745);
  FAdobeGlyphNames.Add('tonebarhighmod', 742);
  FAdobeGlyphNames.Add('tonebarlowmod', 744);
  FAdobeGlyphNames.Add('tonebarmidmod', 743);
  FAdobeGlyphNames.Add('tonefive', 445);
  FAdobeGlyphNames.Add('Tonefive', 444);
  FAdobeGlyphNames.Add('tonesix', 389);
  FAdobeGlyphNames.Add('Tonesix', 388);
  FAdobeGlyphNames.Add('tonetwo', 424);
  FAdobeGlyphNames.Add('Tonetwo', 423);
  FAdobeGlyphNames.Add('tonos', 900);
  FAdobeGlyphNames.Add('tonsquare', 13095);
  FAdobeGlyphNames.Add('topatakthai', 3599);
  FAdobeGlyphNames.Add('tortoiseshellbracketleft', 12308);
  FAdobeGlyphNames.Add('tortoiseshellbracketleftsmall', 65117);
  FAdobeGlyphNames.Add('tortoiseshellbracketleftvertical', 65081);
  FAdobeGlyphNames.Add('tortoiseshellbracketright', 12309);
  FAdobeGlyphNames.Add('tortoiseshellbracketrightsmall', 65118);
  FAdobeGlyphNames.Add('tortoiseshellbracketrightvertical', 65082);
  FAdobeGlyphNames.Add('totaothai', 3605);
  FAdobeGlyphNames.Add('tpalatalhook', 427);
  FAdobeGlyphNames.Add('tparen', 9391);
  FAdobeGlyphNames.Add('trademark', 8482);
  FAdobeGlyphNames.Add('trademarksans', 63722);
  FAdobeGlyphNames.Add('trademarkserif', 63195);
  FAdobeGlyphNames.Add('tretroflexhook', 648);
  FAdobeGlyphNames.Add('Tretroflexhook', 430);
  FAdobeGlyphNames.Add('triagdn', 9660);
  FAdobeGlyphNames.Add('triaglf', 9668);
  FAdobeGlyphNames.Add('triagrt', 9658);
  FAdobeGlyphNames.Add('triagup', 9650);
  FAdobeGlyphNames.Add('ts', 678);
  FAdobeGlyphNames.Add('tsadi', 1510);
  FAdobeGlyphNames.Add('tsadidagesh', 64326);
  FAdobeGlyphNames.Add('tsadidageshhebrew', 64326);
  FAdobeGlyphNames.Add('tsadihebrew', 1510);
  FAdobeGlyphNames.Add('tsecyrillic', 1094);
  FAdobeGlyphNames.Add('Tsecyrillic', 1062);
  FAdobeGlyphNames.Add('tsere', 1461);
  FAdobeGlyphNames.Add('tsere12', 1461);
  FAdobeGlyphNames.Add('tsere1e', 1461);
  FAdobeGlyphNames.Add('tsere2b', 1461);
  FAdobeGlyphNames.Add('tserehebrew', 1461);
  FAdobeGlyphNames.Add('tserenarrowhebrew', 1461);
  FAdobeGlyphNames.Add('tserequarterhebrew', 1461);
  FAdobeGlyphNames.Add('tserewidehebrew', 1461);
  FAdobeGlyphNames.Add('tshecyrillic', 1115);
  FAdobeGlyphNames.Add('Tshecyrillic', 1035);
  FAdobeGlyphNames.Add('Tsmall', 63348);
  FAdobeGlyphNames.Add('tsuperior', 63219);
  FAdobeGlyphNames.Add('ttabengali', 2463);
  FAdobeGlyphNames.Add('ttadeva', 2335);
  FAdobeGlyphNames.Add('ttagujarati', 2719);
  FAdobeGlyphNames.Add('ttagurmukhi', 2591);
  FAdobeGlyphNames.Add('tteharabic', 1657);
  FAdobeGlyphNames.Add('ttehfinalarabic', 64359);
  FAdobeGlyphNames.Add('ttehinitialarabic', 64360);
  FAdobeGlyphNames.Add('ttehmedialarabic', 64361);
  FAdobeGlyphNames.Add('tthabengali', 2464);
  FAdobeGlyphNames.Add('tthadeva', 2336);
  FAdobeGlyphNames.Add('tthagujarati', 2720);
  FAdobeGlyphNames.Add('tthagurmukhi', 2592);
  FAdobeGlyphNames.Add('tturned', 647);
  FAdobeGlyphNames.Add('tuhiragana', 12388);
  FAdobeGlyphNames.Add('tukatakana', 12484);
  FAdobeGlyphNames.Add('tukatakanahalfwidth', 65410);
  FAdobeGlyphNames.Add('tusmallhiragana', 12387);
  FAdobeGlyphNames.Add('tusmallkatakana', 12483);
  FAdobeGlyphNames.Add('tusmallkatakanahalfwidth', 65391);
  FAdobeGlyphNames.Add('twelvecircle', 9323);
  FAdobeGlyphNames.Add('twelveparen', 9343);
  FAdobeGlyphNames.Add('twelveperiod', 9363);
  FAdobeGlyphNames.Add('twelveroman', 8571);
  FAdobeGlyphNames.Add('Twelveroman', 8555);
  FAdobeGlyphNames.Add('twentycircle', 9331);
  FAdobeGlyphNames.Add('twentyhangzhou', 21316);
  FAdobeGlyphNames.Add('twentyparen', 9351);
  FAdobeGlyphNames.Add('twentyperiod', 9371);
  FAdobeGlyphNames.Add('two', 50);
  FAdobeGlyphNames.Add('twoarabic', 1634);
  FAdobeGlyphNames.Add('twobengali', 2536);
  FAdobeGlyphNames.Add('twocircle', 9313);
  FAdobeGlyphNames.Add('twocircleinversesansserif', 10123);
  FAdobeGlyphNames.Add('twodeva', 2408);
  FAdobeGlyphNames.Add('twodotenleader', 8229);
  FAdobeGlyphNames.Add('twodotleader', 8229);
  FAdobeGlyphNames.Add('twodotleadervertical', 65072);
  FAdobeGlyphNames.Add('twogujarati', 2792);
  FAdobeGlyphNames.Add('twogurmukhi', 2664);
  FAdobeGlyphNames.Add('twohackarabic', 1634);
  FAdobeGlyphNames.Add('twohangzhou', 12322);
  FAdobeGlyphNames.Add('twoideographicparen', 12833);
  FAdobeGlyphNames.Add('twoinferior', 8322);
  FAdobeGlyphNames.Add('twomonospace', 65298);
  FAdobeGlyphNames.Add('twonumeratorbengali', 2549);
  FAdobeGlyphNames.Add('twooldstyle', 63282);
  FAdobeGlyphNames.Add('twoparen', 9333);
  FAdobeGlyphNames.Add('twoperiod', 9353);
  FAdobeGlyphNames.Add('twopersian', 1778);
  FAdobeGlyphNames.Add('tworoman', 8561);
  FAdobeGlyphNames.Add('Tworoman', 8545);
  FAdobeGlyphNames.Add('twostroke', 443);
  FAdobeGlyphNames.Add('twosuperior', 178);
  FAdobeGlyphNames.Add('twothai', 3666);
  FAdobeGlyphNames.Add('twothirds', 8532);
  FAdobeGlyphNames.Add('u', 117);
  FAdobeGlyphNames.Add('U', 85);
  FAdobeGlyphNames.Add('uacute', 250);
  FAdobeGlyphNames.Add('Uacute', 218);
  FAdobeGlyphNames.Add('Uacutesmall', 63482);
  FAdobeGlyphNames.Add('ubar', 649);
  FAdobeGlyphNames.Add('ubengali', 2441);
  FAdobeGlyphNames.Add('ubopomofo', 12584);
  FAdobeGlyphNames.Add('ubreve', 365);
  FAdobeGlyphNames.Add('Ubreve', 364);
  FAdobeGlyphNames.Add('ucaron', 468);
  FAdobeGlyphNames.Add('Ucaron', 467);
  FAdobeGlyphNames.Add('ucircle', 9444);
  FAdobeGlyphNames.Add('Ucircle', 9418);
  FAdobeGlyphNames.Add('ucircumflex', 251);
  FAdobeGlyphNames.Add('Ucircumflex', 219);
  FAdobeGlyphNames.Add('ucircumflexbelow', 7799);
  FAdobeGlyphNames.Add('Ucircumflexbelow', 7798);
  FAdobeGlyphNames.Add('Ucircumflexsmall', 63483);
  FAdobeGlyphNames.Add('ucyrillic', 1091);
  FAdobeGlyphNames.Add('Ucyrillic', 1059);
  FAdobeGlyphNames.Add('udattadeva', 2385);
  FAdobeGlyphNames.Add('udblacute', 369);
  FAdobeGlyphNames.Add('Udblacute', 368);
  FAdobeGlyphNames.Add('udblgrave', 533);
  FAdobeGlyphNames.Add('Udblgrave', 532);
  FAdobeGlyphNames.Add('udeva', 2313);
  FAdobeGlyphNames.Add('udieresis', 252);
  FAdobeGlyphNames.Add('Udieresis', 220);
  FAdobeGlyphNames.Add('udieresisacute', 472);
  FAdobeGlyphNames.Add('Udieresisacute', 471);
  FAdobeGlyphNames.Add('udieresisbelow', 7795);
  FAdobeGlyphNames.Add('Udieresisbelow', 7794);
  FAdobeGlyphNames.Add('udieresiscaron', 474);
  FAdobeGlyphNames.Add('Udieresiscaron', 473);
  FAdobeGlyphNames.Add('udieresiscyrillic', 1265);
  FAdobeGlyphNames.Add('Udieresiscyrillic', 1264);
  FAdobeGlyphNames.Add('udieresisgrave', 476);
  FAdobeGlyphNames.Add('Udieresisgrave', 475);
  FAdobeGlyphNames.Add('udieresismacron', 470);
  FAdobeGlyphNames.Add('Udieresismacron', 469);
  FAdobeGlyphNames.Add('Udieresissmall', 63484);
  FAdobeGlyphNames.Add('udotbelow', 7909);
  FAdobeGlyphNames.Add('Udotbelow', 7908);
  FAdobeGlyphNames.Add('ugrave', 249);
  FAdobeGlyphNames.Add('Ugrave', 217);
  FAdobeGlyphNames.Add('Ugravesmall', 63481);
  FAdobeGlyphNames.Add('ugujarati', 2697);
  FAdobeGlyphNames.Add('ugurmukhi', 2569);
  FAdobeGlyphNames.Add('uhiragana', 12358);
  FAdobeGlyphNames.Add('uhookabove', 7911);
  FAdobeGlyphNames.Add('Uhookabove', 7910);
  FAdobeGlyphNames.Add('uhorn', 432);
  FAdobeGlyphNames.Add('Uhorn', 431);
  FAdobeGlyphNames.Add('uhornacute', 7913);
  FAdobeGlyphNames.Add('Uhornacute', 7912);
  FAdobeGlyphNames.Add('uhorndotbelow', 7921);
  FAdobeGlyphNames.Add('Uhorndotbelow', 7920);
  FAdobeGlyphNames.Add('uhorngrave', 7915);
  FAdobeGlyphNames.Add('Uhorngrave', 7914);
  FAdobeGlyphNames.Add('uhornhookabove', 7917);
  FAdobeGlyphNames.Add('Uhornhookabove', 7916);
  FAdobeGlyphNames.Add('uhorntilde', 7919);
  FAdobeGlyphNames.Add('Uhorntilde', 7918);
  FAdobeGlyphNames.Add('uhungarumlaut', 369);
  FAdobeGlyphNames.Add('Uhungarumlaut', 368);
  FAdobeGlyphNames.Add('uhungarumlautcyrillic', 1267);
  FAdobeGlyphNames.Add('Uhungarumlautcyrillic', 1266);
  FAdobeGlyphNames.Add('uinvertedbreve', 535);
  FAdobeGlyphNames.Add('Uinvertedbreve', 534);
  FAdobeGlyphNames.Add('ukatakana', 12454);
  FAdobeGlyphNames.Add('ukatakanahalfwidth', 65395);
  FAdobeGlyphNames.Add('ukcyrillic', 1145);
  FAdobeGlyphNames.Add('Ukcyrillic', 1144);
  FAdobeGlyphNames.Add('ukorean', 12636);
  FAdobeGlyphNames.Add('umacron', 363);
  FAdobeGlyphNames.Add('Umacron', 362);
  FAdobeGlyphNames.Add('umacroncyrillic', 1263);
  FAdobeGlyphNames.Add('Umacroncyrillic', 1262);
  FAdobeGlyphNames.Add('umacrondieresis', 7803);
  FAdobeGlyphNames.Add('Umacrondieresis', 7802);
  FAdobeGlyphNames.Add('umatragurmukhi', 2625);
  FAdobeGlyphNames.Add('umonospace', 65365);
  FAdobeGlyphNames.Add('Umonospace', 65333);
  FAdobeGlyphNames.Add('underscore', 95);
  FAdobeGlyphNames.Add('underscoredbl', 8215);
  FAdobeGlyphNames.Add('underscoremonospace', 65343);
  FAdobeGlyphNames.Add('underscorevertical', 65075);
  FAdobeGlyphNames.Add('underscorewavy', 65103);
  FAdobeGlyphNames.Add('union', 8746);
  FAdobeGlyphNames.Add('universal', 8704);
  FAdobeGlyphNames.Add('uogonek', 371);
  FAdobeGlyphNames.Add('Uogonek', 370);
  FAdobeGlyphNames.Add('uparen', 9392);
  FAdobeGlyphNames.Add('upblock', 9600);
  FAdobeGlyphNames.Add('upperdothebrew', 1476);
  FAdobeGlyphNames.Add('upsilon', 965);
  FAdobeGlyphNames.Add('Upsilon', 933);
  FAdobeGlyphNames.Add('Upsilon1', 978);
  FAdobeGlyphNames.Add('Upsilonacutehooksymbolgreek', 979);
  FAdobeGlyphNames.Add('Upsilonafrican', 433);
  FAdobeGlyphNames.Add('upsilondieresis', 971);
  FAdobeGlyphNames.Add('Upsilondieresis', 939);
  FAdobeGlyphNames.Add('Upsilondieresishooksymbolgreek', 980);
  FAdobeGlyphNames.Add('upsilondieresistonos', 944);
  FAdobeGlyphNames.Add('Upsilonhooksymbol', 978);
  FAdobeGlyphNames.Add('upsilonlatin', 650);
  FAdobeGlyphNames.Add('upsilontonos', 973);
  FAdobeGlyphNames.Add('Upsilontonos', 910);
  FAdobeGlyphNames.Add('uptackbelowcmb', 797);
  FAdobeGlyphNames.Add('uptackmod', 724);
  FAdobeGlyphNames.Add('uragurmukhi', 2675);
  FAdobeGlyphNames.Add('uring', 367);
  FAdobeGlyphNames.Add('Uring', 366);
  FAdobeGlyphNames.Add('ushortcyrillic', 1118);
  FAdobeGlyphNames.Add('Ushortcyrillic', 1038);
  FAdobeGlyphNames.Add('Usmall', 63349);
  FAdobeGlyphNames.Add('usmallhiragana', 12357);
  FAdobeGlyphNames.Add('usmallkatakana', 12453);
  FAdobeGlyphNames.Add('usmallkatakanahalfwidth', 65385);
  FAdobeGlyphNames.Add('ustraightcyrillic', 1199);
  FAdobeGlyphNames.Add('Ustraightcyrillic', 1198);
  FAdobeGlyphNames.Add('ustraightstrokecyrillic', 1201);
  FAdobeGlyphNames.Add('Ustraightstrokecyrillic', 1200);
  FAdobeGlyphNames.Add('utilde', 361);
  FAdobeGlyphNames.Add('Utilde', 360);
  FAdobeGlyphNames.Add('utildeacute', 7801);
  FAdobeGlyphNames.Add('Utildeacute', 7800);
  FAdobeGlyphNames.Add('utildebelow', 7797);
  FAdobeGlyphNames.Add('Utildebelow', 7796);
  FAdobeGlyphNames.Add('uubengali', 2442);
  FAdobeGlyphNames.Add('uudeva', 2314);
  FAdobeGlyphNames.Add('uugujarati', 2698);
  FAdobeGlyphNames.Add('uugurmukhi', 2570);
  FAdobeGlyphNames.Add('uumatragurmukhi', 2626);
  FAdobeGlyphNames.Add('uuvowelsignbengali', 2498);
  FAdobeGlyphNames.Add('uuvowelsigndeva', 2370);
  FAdobeGlyphNames.Add('uuvowelsigngujarati', 2754);
  FAdobeGlyphNames.Add('uvowelsignbengali', 2497);
  FAdobeGlyphNames.Add('uvowelsigndeva', 2369);
  FAdobeGlyphNames.Add('uvowelsigngujarati', 2753);
  FAdobeGlyphNames.Add('v', 118);
  FAdobeGlyphNames.Add('V', 86);
  FAdobeGlyphNames.Add('vadeva', 2357);
  FAdobeGlyphNames.Add('vagujarati', 2741);
  FAdobeGlyphNames.Add('vagurmukhi', 2613);
  FAdobeGlyphNames.Add('vakatakana', 12535);
  FAdobeGlyphNames.Add('vav', 1493);
  FAdobeGlyphNames.Add('vavdagesh', 64309);
  FAdobeGlyphNames.Add('vavdagesh65', 64309);
  FAdobeGlyphNames.Add('vavdageshhebrew', 64309);
  FAdobeGlyphNames.Add('vavhebrew', 1493);
  FAdobeGlyphNames.Add('vavholam', 64331);
  FAdobeGlyphNames.Add('vavholamhebrew', 64331);
  FAdobeGlyphNames.Add('vavvavhebrew', 1520);
  FAdobeGlyphNames.Add('vavyodhebrew', 1521);
  FAdobeGlyphNames.Add('vcircle', 9445);
  FAdobeGlyphNames.Add('Vcircle', 9419);
  FAdobeGlyphNames.Add('vdotbelow', 7807);
  FAdobeGlyphNames.Add('Vdotbelow', 7806);
  FAdobeGlyphNames.Add('vecyrillic', 1074);
  FAdobeGlyphNames.Add('Vecyrillic', 1042);
  FAdobeGlyphNames.Add('veharabic', 1700);
  FAdobeGlyphNames.Add('vehfinalarabic', 64363);
  FAdobeGlyphNames.Add('vehinitialarabic', 64364);
  FAdobeGlyphNames.Add('vehmedialarabic', 64365);
  FAdobeGlyphNames.Add('vekatakana', 12537);
  FAdobeGlyphNames.Add('venus', 9792);
  FAdobeGlyphNames.Add('verticalbar', 124);
  FAdobeGlyphNames.Add('verticallineabovecmb', 781);
  FAdobeGlyphNames.Add('verticallinebelowcmb', 809);
  FAdobeGlyphNames.Add('verticallinelowmod', 716);
  FAdobeGlyphNames.Add('verticallinemod', 712);
  FAdobeGlyphNames.Add('vewarmenian', 1406);
  FAdobeGlyphNames.Add('Vewarmenian', 1358);
  FAdobeGlyphNames.Add('vhook', 651);
  FAdobeGlyphNames.Add('Vhook', 434);
  FAdobeGlyphNames.Add('vikatakana', 12536);
  FAdobeGlyphNames.Add('viramabengali', 2509);
  FAdobeGlyphNames.Add('viramadeva', 2381);
  FAdobeGlyphNames.Add('viramagujarati', 2765);
  FAdobeGlyphNames.Add('visargabengali', 2435);
  FAdobeGlyphNames.Add('visargadeva', 2307);
  FAdobeGlyphNames.Add('visargagujarati', 2691);
  FAdobeGlyphNames.Add('vmonospace', 65366);
  FAdobeGlyphNames.Add('Vmonospace', 65334);
  FAdobeGlyphNames.Add('voarmenian', 1400);
  FAdobeGlyphNames.Add('Voarmenian', 1352);
  FAdobeGlyphNames.Add('voicediterationhiragana', 12446);
  FAdobeGlyphNames.Add('voicediterationkatakana', 12542);
  FAdobeGlyphNames.Add('voicedmarkkana', 12443);
  FAdobeGlyphNames.Add('voicedmarkkanahalfwidth', 65438);
  FAdobeGlyphNames.Add('vokatakana', 12538);
  FAdobeGlyphNames.Add('vparen', 9393);
  FAdobeGlyphNames.Add('Vsmall', 63350);
  FAdobeGlyphNames.Add('vtilde', 7805);
  FAdobeGlyphNames.Add('Vtilde', 7804);
  FAdobeGlyphNames.Add('vturned', 652);
  FAdobeGlyphNames.Add('vuhiragana', 12436);
  FAdobeGlyphNames.Add('vukatakana', 12532);
  FAdobeGlyphNames.Add('w', 119);
  FAdobeGlyphNames.Add('W', 87);
  FAdobeGlyphNames.Add('wacute', 7811);
  FAdobeGlyphNames.Add('Wacute', 7810);
  FAdobeGlyphNames.Add('waekorean', 12633);
  FAdobeGlyphNames.Add('wahiragana', 12431);
  FAdobeGlyphNames.Add('wakatakana', 12527);
  FAdobeGlyphNames.Add('wakatakanahalfwidth', 65436);
  FAdobeGlyphNames.Add('wakorean', 12632);
  FAdobeGlyphNames.Add('wasmallhiragana', 12430);
  FAdobeGlyphNames.Add('wasmallkatakana', 12526);
  FAdobeGlyphNames.Add('wattosquare', 13143);
  FAdobeGlyphNames.Add('wavedash', 12316);
  FAdobeGlyphNames.Add('wavyunderscorevertical', 65076);
  FAdobeGlyphNames.Add('wawarabic', 1608);
  FAdobeGlyphNames.Add('wawfinalarabic', 65262);
  FAdobeGlyphNames.Add('wawhamzaabovearabic', 1572);
  FAdobeGlyphNames.Add('wawhamzaabovefinalarabic', 65158);
  FAdobeGlyphNames.Add('wbsquare', 13277);
  FAdobeGlyphNames.Add('wcircle', 9446);
  FAdobeGlyphNames.Add('Wcircle', 9420);
  FAdobeGlyphNames.Add('wcircumflex', 373);
  FAdobeGlyphNames.Add('Wcircumflex', 372);
  FAdobeGlyphNames.Add('wdieresis', 7813);
  FAdobeGlyphNames.Add('Wdieresis', 7812);
  FAdobeGlyphNames.Add('wdotaccent', 7815);
  FAdobeGlyphNames.Add('Wdotaccent', 7814);
  FAdobeGlyphNames.Add('wdotbelow', 7817);
  FAdobeGlyphNames.Add('Wdotbelow', 7816);
  FAdobeGlyphNames.Add('wehiragana', 12433);
  FAdobeGlyphNames.Add('weierstrass', 8472);
  FAdobeGlyphNames.Add('wekatakana', 12529);
  FAdobeGlyphNames.Add('wekorean', 12638);
  FAdobeGlyphNames.Add('weokorean', 12637);
  FAdobeGlyphNames.Add('wgrave', 7809);
  FAdobeGlyphNames.Add('Wgrave', 7808);
  FAdobeGlyphNames.Add('whitebullet', 9702);
  FAdobeGlyphNames.Add('whitecircle', 9675);
  FAdobeGlyphNames.Add('whitecircleinverse', 9689);
  FAdobeGlyphNames.Add('whitecornerbracketleft', 12302);
  FAdobeGlyphNames.Add('whitecornerbracketleftvertical', 65091);
  FAdobeGlyphNames.Add('whitecornerbracketright', 12303);
  FAdobeGlyphNames.Add('whitecornerbracketrightvertical', 65092);
  FAdobeGlyphNames.Add('whitediamond', 9671);
  FAdobeGlyphNames.Add('whitediamondcontainingblacksmalldiamond', 9672);
  FAdobeGlyphNames.Add('whitedownpointingsmalltriangle', 9663);
  FAdobeGlyphNames.Add('whitedownpointingtriangle', 9661);
  FAdobeGlyphNames.Add('whiteleftpointingsmalltriangle', 9667);
  FAdobeGlyphNames.Add('whiteleftpointingtriangle', 9665);
  FAdobeGlyphNames.Add('whitelenticularbracketleft', 12310);
  FAdobeGlyphNames.Add('whitelenticularbracketright', 12311);
  FAdobeGlyphNames.Add('whiterightpointingsmalltriangle', 9657);
  FAdobeGlyphNames.Add('whiterightpointingtriangle', 9655);
  FAdobeGlyphNames.Add('whitesmallsquare', 9643);
  FAdobeGlyphNames.Add('whitesmilingface', 9786);
  FAdobeGlyphNames.Add('whitesquare', 9633);
  FAdobeGlyphNames.Add('whitestar', 9734);
  FAdobeGlyphNames.Add('whitetelephone', 9743);
  FAdobeGlyphNames.Add('whitetortoiseshellbracketleft', 12312);
  FAdobeGlyphNames.Add('whitetortoiseshellbracketright', 12313);
  FAdobeGlyphNames.Add('whiteuppointingsmalltriangle', 9653);
  FAdobeGlyphNames.Add('whiteuppointingtriangle', 9651);
  FAdobeGlyphNames.Add('wihiragana', 12432);
  FAdobeGlyphNames.Add('wikatakana', 12528);
  FAdobeGlyphNames.Add('wikorean', 12639);
  FAdobeGlyphNames.Add('wmonospace', 65367);
  FAdobeGlyphNames.Add('Wmonospace', 65335);
  FAdobeGlyphNames.Add('wohiragana', 12434);
  FAdobeGlyphNames.Add('wokatakana', 12530);
  FAdobeGlyphNames.Add('wokatakanahalfwidth', 65382);
  FAdobeGlyphNames.Add('won', 8361);
  FAdobeGlyphNames.Add('wonmonospace', 65510);
  FAdobeGlyphNames.Add('wowaenthai', 3623);
  FAdobeGlyphNames.Add('wparen', 9394);
  FAdobeGlyphNames.Add('wring', 7832);
  FAdobeGlyphNames.Add('Wsmall', 63351);
  FAdobeGlyphNames.Add('wsuperior', 695);
  FAdobeGlyphNames.Add('wturned', 653);
  FAdobeGlyphNames.Add('wynn', 447);
  FAdobeGlyphNames.Add('x', 120);
  FAdobeGlyphNames.Add('X', 88);
  FAdobeGlyphNames.Add('xabovecmb', 829);
  FAdobeGlyphNames.Add('xbopomofo', 12562);
  FAdobeGlyphNames.Add('xcircle', 9447);
  FAdobeGlyphNames.Add('Xcircle', 9421);
  FAdobeGlyphNames.Add('xdieresis', 7821);
  FAdobeGlyphNames.Add('Xdieresis', 7820);
  FAdobeGlyphNames.Add('xdotaccent', 7819);
  FAdobeGlyphNames.Add('Xdotaccent', 7818);
  FAdobeGlyphNames.Add('xeharmenian', 1389);
  FAdobeGlyphNames.Add('Xeharmenian', 1341);
  FAdobeGlyphNames.Add('xi', 958);
  FAdobeGlyphNames.Add('Xi', 926);
  FAdobeGlyphNames.Add('xmonospace', 65368);
  FAdobeGlyphNames.Add('Xmonospace', 65336);
  FAdobeGlyphNames.Add('xparen', 9395);
  FAdobeGlyphNames.Add('Xsmall', 63352);
  FAdobeGlyphNames.Add('xsuperior', 739);
  FAdobeGlyphNames.Add('y', 121);
  FAdobeGlyphNames.Add('Y', 89);
  FAdobeGlyphNames.Add('yaadosquare', 13134);
  FAdobeGlyphNames.Add('yabengali', 2479);
  FAdobeGlyphNames.Add('yacute', 253);
  FAdobeGlyphNames.Add('Yacute', 221);
  FAdobeGlyphNames.Add('Yacutesmall', 63485);
  FAdobeGlyphNames.Add('yadeva', 2351);
  FAdobeGlyphNames.Add('yaekorean', 12626);
  FAdobeGlyphNames.Add('yagujarati', 2735);
  FAdobeGlyphNames.Add('yagurmukhi', 2607);
  FAdobeGlyphNames.Add('yahiragana', 12420);
  FAdobeGlyphNames.Add('yakatakana', 12516);
  FAdobeGlyphNames.Add('yakatakanahalfwidth', 65428);
  FAdobeGlyphNames.Add('yakorean', 12625);
  FAdobeGlyphNames.Add('yamakkanthai', 3662);
  FAdobeGlyphNames.Add('yasmallhiragana', 12419);
  FAdobeGlyphNames.Add('yasmallkatakana', 12515);
  FAdobeGlyphNames.Add('yasmallkatakanahalfwidth', 65388);
  FAdobeGlyphNames.Add('yatcyrillic', 1123);
  FAdobeGlyphNames.Add('Yatcyrillic', 1122);
  FAdobeGlyphNames.Add('ycircle', 9448);
  FAdobeGlyphNames.Add('Ycircle', 9422);
  FAdobeGlyphNames.Add('ycircumflex', 375);
  FAdobeGlyphNames.Add('Ycircumflex', 374);
  FAdobeGlyphNames.Add('ydieresis', 255);
  FAdobeGlyphNames.Add('Ydieresis', 376);
  FAdobeGlyphNames.Add('Ydieresissmall', 63487);
  FAdobeGlyphNames.Add('ydotaccent', 7823);
  FAdobeGlyphNames.Add('Ydotaccent', 7822);
  FAdobeGlyphNames.Add('ydotbelow', 7925);
  FAdobeGlyphNames.Add('Ydotbelow', 7924);
  FAdobeGlyphNames.Add('yeharabic', 1610);
  FAdobeGlyphNames.Add('yehbarreearabic', 1746);
  FAdobeGlyphNames.Add('yehbarreefinalarabic', 64431);
  FAdobeGlyphNames.Add('yehfinalarabic', 65266);
  FAdobeGlyphNames.Add('yehhamzaabovearabic', 1574);
  FAdobeGlyphNames.Add('yehhamzaabovefinalarabic', 65162);
  FAdobeGlyphNames.Add('yehhamzaaboveinitialarabic', 65163);
  FAdobeGlyphNames.Add('yehhamzaabovemedialarabic', 65164);
  FAdobeGlyphNames.Add('yehinitialarabic', 65267);
  FAdobeGlyphNames.Add('yehmedialarabic', 65268);
  FAdobeGlyphNames.Add('yehmeeminitialarabic', 64733);
  FAdobeGlyphNames.Add('yehmeemisolatedarabic', 64600);
  FAdobeGlyphNames.Add('yehnoonfinalarabic', 64660);
  FAdobeGlyphNames.Add('yehthreedotsbelowarabic', 1745);
  FAdobeGlyphNames.Add('yekorean', 12630);
  FAdobeGlyphNames.Add('yen', 165);
  FAdobeGlyphNames.Add('yenmonospace', 65509);
  FAdobeGlyphNames.Add('yeokorean', 12629);
  FAdobeGlyphNames.Add('yeorinhieuhkorean', 12678);
  FAdobeGlyphNames.Add('yerahbenyomohebrew', 1450);
  FAdobeGlyphNames.Add('yerahbenyomolefthebrew', 1450);
  FAdobeGlyphNames.Add('yericyrillic', 1099);
  FAdobeGlyphNames.Add('Yericyrillic', 1067);
  FAdobeGlyphNames.Add('yerudieresiscyrillic', 1273);
  FAdobeGlyphNames.Add('Yerudieresiscyrillic', 1272);
  FAdobeGlyphNames.Add('yesieungkorean', 12673);
  FAdobeGlyphNames.Add('yesieungpansioskorean', 12675);
  FAdobeGlyphNames.Add('yesieungsioskorean', 12674);
  FAdobeGlyphNames.Add('yetivhebrew', 1434);
  FAdobeGlyphNames.Add('ygrave', 7923);
  FAdobeGlyphNames.Add('Ygrave', 7922);
  FAdobeGlyphNames.Add('yhook', 436);
  FAdobeGlyphNames.Add('Yhook', 435);
  FAdobeGlyphNames.Add('yhookabove', 7927);
  FAdobeGlyphNames.Add('Yhookabove', 7926);
  FAdobeGlyphNames.Add('yiarmenian', 1397);
  FAdobeGlyphNames.Add('Yiarmenian', 1349);
  FAdobeGlyphNames.Add('yicyrillic', 1111);
  FAdobeGlyphNames.Add('Yicyrillic', 1031);
  FAdobeGlyphNames.Add('yikorean', 12642);
  FAdobeGlyphNames.Add('yinyang', 9775);
  FAdobeGlyphNames.Add('yiwnarmenian', 1410);
  FAdobeGlyphNames.Add('Yiwnarmenian', 1362);
  FAdobeGlyphNames.Add('ymonospace', 65369);
  FAdobeGlyphNames.Add('Ymonospace', 65337);
  FAdobeGlyphNames.Add('yod', 1497);
  FAdobeGlyphNames.Add('yoddagesh', 64313);
  FAdobeGlyphNames.Add('yoddageshhebrew', 64313);
  FAdobeGlyphNames.Add('yodhebrew', 1497);
  FAdobeGlyphNames.Add('yodyodhebrew', 1522);
  FAdobeGlyphNames.Add('yodyodpatahhebrew', 64287);
  FAdobeGlyphNames.Add('yohiragana', 12424);
  FAdobeGlyphNames.Add('yoikorean', 12681);
  FAdobeGlyphNames.Add('yokatakana', 12520);
  FAdobeGlyphNames.Add('yokatakanahalfwidth', 65430);
  FAdobeGlyphNames.Add('yokorean', 12635);
  FAdobeGlyphNames.Add('yosmallhiragana', 12423);
  FAdobeGlyphNames.Add('yosmallkatakana', 12519);
  FAdobeGlyphNames.Add('yosmallkatakanahalfwidth', 65390);
  FAdobeGlyphNames.Add('yotgreek', 1011);
  FAdobeGlyphNames.Add('yoyaekorean', 12680);
  FAdobeGlyphNames.Add('yoyakorean', 12679);
  FAdobeGlyphNames.Add('yoyakthai', 3618);
  FAdobeGlyphNames.Add('yoyingthai', 3597);
  FAdobeGlyphNames.Add('yparen', 9396);
  FAdobeGlyphNames.Add('ypogegrammeni', 890);
  FAdobeGlyphNames.Add('ypogegrammenigreekcmb', 837);
  FAdobeGlyphNames.Add('yr', 422);
  FAdobeGlyphNames.Add('yring', 7833);
  FAdobeGlyphNames.Add('Ysmall', 63353);
  FAdobeGlyphNames.Add('ysuperior', 696);
  FAdobeGlyphNames.Add('ytilde', 7929);
  FAdobeGlyphNames.Add('Ytilde', 7928);
  FAdobeGlyphNames.Add('yturned', 654);
  FAdobeGlyphNames.Add('yuhiragana', 12422);
  FAdobeGlyphNames.Add('yuikorean', 12684);
  FAdobeGlyphNames.Add('yukatakana', 12518);
  FAdobeGlyphNames.Add('yukatakanahalfwidth', 65429);
  FAdobeGlyphNames.Add('yukorean', 12640);
  FAdobeGlyphNames.Add('yusbigcyrillic', 1131);
  FAdobeGlyphNames.Add('Yusbigcyrillic', 1130);
  FAdobeGlyphNames.Add('yusbigiotifiedcyrillic', 1133);
  FAdobeGlyphNames.Add('Yusbigiotifiedcyrillic', 1132);
  FAdobeGlyphNames.Add('yuslittlecyrillic', 1127);
  FAdobeGlyphNames.Add('Yuslittlecyrillic', 1126);
  FAdobeGlyphNames.Add('yuslittleiotifiedcyrillic', 1129);
  FAdobeGlyphNames.Add('Yuslittleiotifiedcyrillic', 1128);
  FAdobeGlyphNames.Add('yusmallhiragana', 12421);
  FAdobeGlyphNames.Add('yusmallkatakana', 12517);
  FAdobeGlyphNames.Add('yusmallkatakanahalfwidth', 65389);
  FAdobeGlyphNames.Add('yuyekorean', 12683);
  FAdobeGlyphNames.Add('yuyeokorean', 12682);
  FAdobeGlyphNames.Add('yyabengali', 2527);
  FAdobeGlyphNames.Add('yyadeva', 2399);
  FAdobeGlyphNames.Add('z', 122);
  FAdobeGlyphNames.Add('Z', 90);
  FAdobeGlyphNames.Add('zaarmenian', 1382);
  FAdobeGlyphNames.Add('Zaarmenian', 1334);
  FAdobeGlyphNames.Add('zacute', 378);
  FAdobeGlyphNames.Add('Zacute', 377);
  FAdobeGlyphNames.Add('zadeva', 2395);
  FAdobeGlyphNames.Add('zagurmukhi', 2651);
  FAdobeGlyphNames.Add('zaharabic', 1592);
  FAdobeGlyphNames.Add('zahfinalarabic', 65222);
  FAdobeGlyphNames.Add('zahinitialarabic', 65223);
  FAdobeGlyphNames.Add('zahiragana', 12374);
  FAdobeGlyphNames.Add('zahmedialarabic', 65224);
  FAdobeGlyphNames.Add('zainarabic', 1586);
  FAdobeGlyphNames.Add('zainfinalarabic', 65200);
  FAdobeGlyphNames.Add('zakatakana', 12470);
  FAdobeGlyphNames.Add('zaqefgadolhebrew', 1429);
  FAdobeGlyphNames.Add('zaqefqatanhebrew', 1428);
  FAdobeGlyphNames.Add('zarqahebrew', 1432);
  FAdobeGlyphNames.Add('zayin', 1494);
  FAdobeGlyphNames.Add('zayindagesh', 64310);
  FAdobeGlyphNames.Add('zayindageshhebrew', 64310);
  FAdobeGlyphNames.Add('zayinhebrew', 1494);
  FAdobeGlyphNames.Add('zbopomofo', 12567);
  FAdobeGlyphNames.Add('zcaron', 382);
  FAdobeGlyphNames.Add('Zcaron', 381);
  FAdobeGlyphNames.Add('Zcaronsmall', 63231);
  FAdobeGlyphNames.Add('zcircle', 9449);
  FAdobeGlyphNames.Add('Zcircle', 9423);
  FAdobeGlyphNames.Add('zcircumflex', 7825);
  FAdobeGlyphNames.Add('Zcircumflex', 7824);
  FAdobeGlyphNames.Add('zcurl', 657);
  FAdobeGlyphNames.Add('zdot', 380);
  FAdobeGlyphNames.Add('Zdot', 379);
  FAdobeGlyphNames.Add('zdotaccent', 380);
  FAdobeGlyphNames.Add('Zdotaccent', 379);
  FAdobeGlyphNames.Add('zdotbelow', 7827);
  FAdobeGlyphNames.Add('Zdotbelow', 7826);
  FAdobeGlyphNames.Add('zecyrillic', 1079);
  FAdobeGlyphNames.Add('Zecyrillic', 1047);
  FAdobeGlyphNames.Add('zedescendercyrillic', 1177);
  FAdobeGlyphNames.Add('Zedescendercyrillic', 1176);
  FAdobeGlyphNames.Add('zedieresiscyrillic', 1247);
  FAdobeGlyphNames.Add('Zedieresiscyrillic', 1246);
  FAdobeGlyphNames.Add('zehiragana', 12380);
  FAdobeGlyphNames.Add('zekatakana', 12476);
  FAdobeGlyphNames.Add('zero', 48);
  FAdobeGlyphNames.Add('zeroarabic', 1632);
  FAdobeGlyphNames.Add('zerobengali', 2534);
  FAdobeGlyphNames.Add('zerodeva', 2406);
  FAdobeGlyphNames.Add('zerogujarati', 2790);
  FAdobeGlyphNames.Add('zerogurmukhi', 2662);
  FAdobeGlyphNames.Add('zerohackarabic', 1632);
  FAdobeGlyphNames.Add('zeroinferior', 8320);
  FAdobeGlyphNames.Add('zeromonospace', 65296);
  FAdobeGlyphNames.Add('zerooldstyle', 63280);
  FAdobeGlyphNames.Add('zeropersian', 1776);
  FAdobeGlyphNames.Add('zerosuperior', 8304);
  FAdobeGlyphNames.Add('zerothai', 3664);
  FAdobeGlyphNames.Add('zerowidthjoiner', 65279);
  FAdobeGlyphNames.Add('zerowidthnonjoiner', 8204);
  FAdobeGlyphNames.Add('zerowidthspace', 8203);
  FAdobeGlyphNames.Add('zeta', 950);
  FAdobeGlyphNames.Add('Zeta', 918);
  FAdobeGlyphNames.Add('zhbopomofo', 12563);
  FAdobeGlyphNames.Add('zhearmenian', 1386);
  FAdobeGlyphNames.Add('Zhearmenian', 1338);
  FAdobeGlyphNames.Add('zhebrevecyrillic', 1218);
  FAdobeGlyphNames.Add('Zhebrevecyrillic', 1217);
  FAdobeGlyphNames.Add('zhecyrillic', 1078);
  FAdobeGlyphNames.Add('Zhecyrillic', 1046);
  FAdobeGlyphNames.Add('zhedescendercyrillic', 1175);
  FAdobeGlyphNames.Add('Zhedescendercyrillic', 1174);
  FAdobeGlyphNames.Add('zhedieresiscyrillic', 1245);
  FAdobeGlyphNames.Add('Zhedieresiscyrillic', 1244);
  FAdobeGlyphNames.Add('zihiragana', 12376);
  FAdobeGlyphNames.Add('zikatakana', 12472);
  FAdobeGlyphNames.Add('zinorhebrew', 1454);
  FAdobeGlyphNames.Add('zlinebelow', 7829);
  FAdobeGlyphNames.Add('Zlinebelow', 7828);
  FAdobeGlyphNames.Add('zmonospace', 65370);
  FAdobeGlyphNames.Add('Zmonospace', 65338);
  FAdobeGlyphNames.Add('zohiragana', 12382);
  FAdobeGlyphNames.Add('zokatakana', 12478);
  FAdobeGlyphNames.Add('zparen', 9397);
  FAdobeGlyphNames.Add('zretroflexhook', 656);
  FAdobeGlyphNames.Add('Zsmall', 63354);
  FAdobeGlyphNames.Add('zstroke', 438);
  FAdobeGlyphNames.Add('Zstroke', 437);
  FAdobeGlyphNames.Add('zuhiragana', 12378);
  FAdobeGlyphNames.Add('zukatakana', 12474);
end;

procedure TdxFontFileUnicodeConverter.InitializeGlyphCodes;
begin
  FGlyphCodes.Add(TdxGlyphNames.A, $41);
  FGlyphCodes.Add(TdxGlyphNames.AE, $c6);
  FGlyphCodes.Add(TdxGlyphNames.AEacute, $1fc);
  FGlyphCodes.Add(TdxGlyphNames.Aacute, $c1);
  FGlyphCodes.Add(TdxGlyphNames.Abreve, $102);
  FGlyphCodes.Add(TdxGlyphNames.Acircumflex, $c2);
  FGlyphCodes.Add(TdxGlyphNames.Adieresis, $c4);
  FGlyphCodes.Add(TdxGlyphNames.Agrave, $c0);
  FGlyphCodes.Add(TdxGlyphNames.Alpha, $391);
  FGlyphCodes.Add(TdxGlyphNames.Alphatonos, $386);
  FGlyphCodes.Add(TdxGlyphNames.Amacron, $100);
  FGlyphCodes.Add(TdxGlyphNames.Aogonek, $104);
  FGlyphCodes.Add(TdxGlyphNames.Aring, $c5);
  FGlyphCodes.Add(TdxGlyphNames.Aringacute, $1fa);
  FGlyphCodes.Add(TdxGlyphNames.Atilde, $c3);
  FGlyphCodes.Add(TdxGlyphNames.B, $42);
  FGlyphCodes.Add(TdxGlyphNames.Beta, $392);
  FGlyphCodes.Add(TdxGlyphNames.C, $43);
  FGlyphCodes.Add(TdxGlyphNames.Cacute, $106);
  FGlyphCodes.Add(TdxGlyphNames.Ccaron, $10c);
  FGlyphCodes.Add(TdxGlyphNames.Ccedilla, $c7);
  FGlyphCodes.Add(TdxGlyphNames.Ccircumflex, $108);
  FGlyphCodes.Add(TdxGlyphNames.Cdot, $10a);
  FGlyphCodes.Add(TdxGlyphNames.Chi, $3a7);
  FGlyphCodes.Add(TdxGlyphNames.D, $44);
  FGlyphCodes.Add(TdxGlyphNames.Dcaron, $10e);
  FGlyphCodes.Add(TdxGlyphNames.Dcroat, $110);
  FGlyphCodes.Add(TdxGlyphNames.Delta, $394);
  FGlyphCodes.Add(TdxGlyphNames.E, $45);
  FGlyphCodes.Add(TdxGlyphNames.Ebreve, $114);
  FGlyphCodes.Add(TdxGlyphNames.Ecaron, $11a);
  FGlyphCodes.Add(TdxGlyphNames.Emacron, $112);
  FGlyphCodes.Add(TdxGlyphNames.Eacute, $c9);
  FGlyphCodes.Add(TdxGlyphNames.Ecircumflex, $ca);
  FGlyphCodes.Add(TdxGlyphNames.Edieresis, $cb);
  FGlyphCodes.Add(TdxGlyphNames.Edot, $116);
  FGlyphCodes.Add(TdxGlyphNames.Egrave, $c8);
  FGlyphCodes.Add(TdxGlyphNames.Eng, $14a);
  FGlyphCodes.Add(TdxGlyphNames.Eogonek, $118);
  FGlyphCodes.Add(TdxGlyphNames.Epsilon, $395);
  FGlyphCodes.Add(TdxGlyphNames.Epsilontonos, $388);
  FGlyphCodes.Add(TdxGlyphNames.Eta, $397);
  FGlyphCodes.Add(TdxGlyphNames.Etatonos, $389);
  FGlyphCodes.Add(TdxGlyphNames.Eth, $d0);
  FGlyphCodes.Add(TdxGlyphNames.Euro, $20ac);
  FGlyphCodes.Add(TdxGlyphNames.F, $46);
  FGlyphCodes.Add(TdxGlyphNames.G, $47);
  FGlyphCodes.Add(TdxGlyphNames.Gamma, $393);
  FGlyphCodes.Add(TdxGlyphNames.Gbreve, $11e);
  FGlyphCodes.Add(TdxGlyphNames.Gcedilla, $122);
  FGlyphCodes.Add(TdxGlyphNames.Gcircumflex, $11c);
  FGlyphCodes.Add(TdxGlyphNames.Gdot, $120);
  FGlyphCodes.Add(TdxGlyphNames.H, $48);
  FGlyphCodes.Add(TdxGlyphNames.H18533, $25cf);
  FGlyphCodes.Add(TdxGlyphNames.H18543, $25aa);
  FGlyphCodes.Add(TdxGlyphNames.H18551, $25ab);
  FGlyphCodes.Add(TdxGlyphNames.H22073, $25a1);
  FGlyphCodes.Add(TdxGlyphNames.Hbar, $126);
  FGlyphCodes.Add(TdxGlyphNames.Hcircumflex, $124);
  FGlyphCodes.Add(TdxGlyphNames.I, $49);
  FGlyphCodes.Add(TdxGlyphNames.IJ, $132);
  FGlyphCodes.Add(TdxGlyphNames.Iacute, $cd);
  FGlyphCodes.Add(TdxGlyphNames.Ibreve, $12c);
  FGlyphCodes.Add(TdxGlyphNames.Icircumflex, $ce);
  FGlyphCodes.Add(TdxGlyphNames.Idieresis, $cf);
  FGlyphCodes.Add(TdxGlyphNames.Idot, $130);
  FGlyphCodes.Add(TdxGlyphNames.Idotaccent, $130);
  FGlyphCodes.Add(TdxGlyphNames.Ifraktur, $2111);
  FGlyphCodes.Add(TdxGlyphNames.Igrave, $cc);
  FGlyphCodes.Add(TdxGlyphNames.Imacron, $12a);
  FGlyphCodes.Add(TdxGlyphNames.Iogonek, $12e);
  FGlyphCodes.Add(TdxGlyphNames.Iota, $399);
  FGlyphCodes.Add(TdxGlyphNames.Iotadieresis, $3aa);
  FGlyphCodes.Add(TdxGlyphNames.Iotatonos, $38a);
  FGlyphCodes.Add(TdxGlyphNames.Itilde, $128);
  FGlyphCodes.Add(TdxGlyphNames.J, $4a);
  FGlyphCodes.Add(TdxGlyphNames.Jcircumflex, $134);
  FGlyphCodes.Add(TdxGlyphNames.K, $4b);
  FGlyphCodes.Add(TdxGlyphNames.Kappa, $39a);
  FGlyphCodes.Add(TdxGlyphNames.Kcedilla, $136);
  FGlyphCodes.Add(TdxGlyphNames.L, $4c);
  FGlyphCodes.Add(TdxGlyphNames.Lacute, $139);
  FGlyphCodes.Add(TdxGlyphNames.Lambda, $39b);
  FGlyphCodes.Add(TdxGlyphNames.Lcaron, $13d);
  FGlyphCodes.Add(TdxGlyphNames.Lcedilla, $13b);
  FGlyphCodes.Add(TdxGlyphNames.Ldot, $13f);
  FGlyphCodes.Add(TdxGlyphNames.Lslash, $141);
  FGlyphCodes.Add(TdxGlyphNames.M, $4d);
  FGlyphCodes.Add(TdxGlyphNames.Mu, $39c);
  FGlyphCodes.Add(TdxGlyphNames.N, $4e);
  FGlyphCodes.Add(TdxGlyphNames.Nacute, $143);
  FGlyphCodes.Add(TdxGlyphNames.Ncaron, $147);
  FGlyphCodes.Add(TdxGlyphNames.Ncedilla, $145);
  FGlyphCodes.Add(TdxGlyphNames.Ntilde, $d1);
  FGlyphCodes.Add(TdxGlyphNames.Nu, $39d);
  FGlyphCodes.Add(TdxGlyphNames.O, $4f);
  FGlyphCodes.Add(TdxGlyphNames.OE, $152);
  FGlyphCodes.Add(TdxGlyphNames.Oacute, $d3);
  FGlyphCodes.Add(TdxGlyphNames.Obreve, $14e);
  FGlyphCodes.Add(TdxGlyphNames.Ocircumflex, $d4);
  FGlyphCodes.Add(TdxGlyphNames.Odblacute, $150);
  FGlyphCodes.Add(TdxGlyphNames.Odieresis, $d6);
  FGlyphCodes.Add(TdxGlyphNames.Ograve, $d2);
  FGlyphCodes.Add(TdxGlyphNames.Ohm, $2126);
  FGlyphCodes.Add(TdxGlyphNames.Omacron, $14c);
  FGlyphCodes.Add(TdxGlyphNames.Omega, $3a9);
  FGlyphCodes.Add(TdxGlyphNames.Omegatonos, $38f);
  FGlyphCodes.Add(TdxGlyphNames.Omicron, $39f);
  FGlyphCodes.Add(TdxGlyphNames.Omicrontonos, $38c);
  FGlyphCodes.Add(TdxGlyphNames.Oslash, $d8);
  FGlyphCodes.Add(TdxGlyphNames.Oslashacute, $1fe);
  FGlyphCodes.Add(TdxGlyphNames.Otilde, $d5);
  FGlyphCodes.Add(TdxGlyphNames.P, $50);
  FGlyphCodes.Add(TdxGlyphNames.Phi, $3a6);
  FGlyphCodes.Add(TdxGlyphNames.Pi, $3a0);
  FGlyphCodes.Add(TdxGlyphNames.Psi, $3a8);
  FGlyphCodes.Add(TdxGlyphNames.Q, $51);
  FGlyphCodes.Add(TdxGlyphNames.R, $52);
  FGlyphCodes.Add(TdxGlyphNames.Racute, $154);
  FGlyphCodes.Add(TdxGlyphNames.Rcaron, $158);
  FGlyphCodes.Add(TdxGlyphNames.Rcedilla, $156);
  FGlyphCodes.Add(TdxGlyphNames.Rfraktur, $211c);
  FGlyphCodes.Add(TdxGlyphNames.Rho, $3a1);
  FGlyphCodes.Add(TdxGlyphNames.S, $53);
  FGlyphCodes.Add(TdxGlyphNames.SF010000, $250c);
  FGlyphCodes.Add(TdxGlyphNames.SF020000, $2514);
  FGlyphCodes.Add(TdxGlyphNames.SF030000, $2510);
  FGlyphCodes.Add(TdxGlyphNames.SF040000, $2518);
  FGlyphCodes.Add(TdxGlyphNames.SF050000, $253c);
  FGlyphCodes.Add(TdxGlyphNames.SF060000, $252c);
  FGlyphCodes.Add(TdxGlyphNames.SF070000, $2534);
  FGlyphCodes.Add(TdxGlyphNames.SF080000, $251c);
  FGlyphCodes.Add(TdxGlyphNames.SF090000, $2524);
  FGlyphCodes.Add(TdxGlyphNames.SF100000, $2500);
  FGlyphCodes.Add(TdxGlyphNames.SF110000, $2502);
  FGlyphCodes.Add(TdxGlyphNames.SF190000, $2561);
  FGlyphCodes.Add(TdxGlyphNames.SF200000, $2562);
  FGlyphCodes.Add(TdxGlyphNames.SF210000, $2556);
  FGlyphCodes.Add(TdxGlyphNames.SF220000, $2555);
  FGlyphCodes.Add(TdxGlyphNames.SF230000, $2563);
  FGlyphCodes.Add(TdxGlyphNames.SF240000, $2551);
  FGlyphCodes.Add(TdxGlyphNames.SF250000, $2557);
  FGlyphCodes.Add(TdxGlyphNames.SF260000, $255d);
  FGlyphCodes.Add(TdxGlyphNames.SF270000, $255c);
  FGlyphCodes.Add(TdxGlyphNames.SF280000, $255b);
  FGlyphCodes.Add(TdxGlyphNames.SF360000, $255e);
  FGlyphCodes.Add(TdxGlyphNames.SF370000, $255f);
  FGlyphCodes.Add(TdxGlyphNames.SF380000, $255a);
  FGlyphCodes.Add(TdxGlyphNames.SF390000, $2554);
  FGlyphCodes.Add(TdxGlyphNames.SF400000, $2569);
  FGlyphCodes.Add(TdxGlyphNames.SF410000, $2566);
  FGlyphCodes.Add(TdxGlyphNames.SF420000, $2560);
  FGlyphCodes.Add(TdxGlyphNames.SF430000, $2550);
  FGlyphCodes.Add(TdxGlyphNames.SF440000, $256c);
  FGlyphCodes.Add(TdxGlyphNames.SF450000, $2567);
  FGlyphCodes.Add(TdxGlyphNames.SF460000, $2568);
  FGlyphCodes.Add(TdxGlyphNames.SF470000, $2564);
  FGlyphCodes.Add(TdxGlyphNames.SF480000, $2565);
  FGlyphCodes.Add(TdxGlyphNames.SF490000, $2559);
  FGlyphCodes.Add(TdxGlyphNames.SF500000, $2558);
  FGlyphCodes.Add(TdxGlyphNames.SF510000, $2552);
  FGlyphCodes.Add(TdxGlyphNames.SF520000, $2553);
  FGlyphCodes.Add(TdxGlyphNames.SF530000, $256b);
  FGlyphCodes.Add(TdxGlyphNames.SF540000, $256a);
  FGlyphCodes.Add(TdxGlyphNames.Sacute, $15a);
  FGlyphCodes.Add(TdxGlyphNames.Scaron, $160);
  FGlyphCodes.Add(TdxGlyphNames.Scedilla, $15e);
  FGlyphCodes.Add(TdxGlyphNames.Scircumflex, $15c);
  FGlyphCodes.Add(TdxGlyphNames.Sigma, $3a3);
  FGlyphCodes.Add(TdxGlyphNames.T, $54);
  FGlyphCodes.Add(TdxGlyphNames.Tau, $3a4);
  FGlyphCodes.Add(TdxGlyphNames.Tbar, $166);
  FGlyphCodes.Add(TdxGlyphNames.Tcaron, $164);
  FGlyphCodes.Add(TdxGlyphNames.Tcedilla, $162);
  FGlyphCodes.Add(TdxGlyphNames.Theta, $398);
  FGlyphCodes.Add(TdxGlyphNames.Thorn, $de);
  FGlyphCodes.Add(TdxGlyphNames.U, $55);
  FGlyphCodes.Add(TdxGlyphNames.Uacute, $da);
  FGlyphCodes.Add(TdxGlyphNames.Ubreve, $16c);
  FGlyphCodes.Add(TdxGlyphNames.Ucircumflex, $db);
  FGlyphCodes.Add(TdxGlyphNames.Udblacute, $170);
  FGlyphCodes.Add(TdxGlyphNames.Udieresis, $dc);
  FGlyphCodes.Add(TdxGlyphNames.Ugrave, $d9);
  FGlyphCodes.Add(TdxGlyphNames.Umacron, $16a);
  FGlyphCodes.Add(TdxGlyphNames.Uogonek, $172);
  FGlyphCodes.Add(TdxGlyphNames.Upsilon, $3a5);
  FGlyphCodes.Add(TdxGlyphNames.Upsilon1, $3d2);
  FGlyphCodes.Add(TdxGlyphNames.Upsilondieresis, $3ab);
  FGlyphCodes.Add(TdxGlyphNames.Upsilontonos, $38e);
  FGlyphCodes.Add(TdxGlyphNames.Uring, $16e);
  FGlyphCodes.Add(TdxGlyphNames.Utilde, $168);
  FGlyphCodes.Add(TdxGlyphNames.V, $56);
  FGlyphCodes.Add(TdxGlyphNames.W, $57);
  FGlyphCodes.Add(TdxGlyphNames.Wacute, $1e82);
  FGlyphCodes.Add(TdxGlyphNames.Wcircumflex, $174);
  FGlyphCodes.Add(TdxGlyphNames.Wdieresis, $1e84);
  FGlyphCodes.Add(TdxGlyphNames.Wgrave, $1e80);
  FGlyphCodes.Add(TdxGlyphNames.X, $58);
  FGlyphCodes.Add(TdxGlyphNames.Xi, $39e);
  FGlyphCodes.Add(TdxGlyphNames.Y, $59);
  FGlyphCodes.Add(TdxGlyphNames.Yacute, $dd);
  FGlyphCodes.Add(TdxGlyphNames.Ycircumflex, $176);
  FGlyphCodes.Add(TdxGlyphNames.Ydieresis, $178);
  FGlyphCodes.Add(TdxGlyphNames.Ygrave, $1ef2);
  FGlyphCodes.Add(TdxGlyphNames.Z, $5a);
  FGlyphCodes.Add(TdxGlyphNames.Zacute, $179);
  FGlyphCodes.Add(TdxGlyphNames.Zcaron, $17d);
  FGlyphCodes.Add(TdxGlyphNames.Zdot, $17b);
  FGlyphCodes.Add(TdxGlyphNames.Zeta, $396);
  FGlyphCodes.Add(TdxGlyphNames.LowerA, $61);
  FGlyphCodes.Add(TdxGlyphNames.LowerA1, $2701);
  FGlyphCodes.Add(TdxGlyphNames.LowerA10, $2721);
  FGlyphCodes.Add(TdxGlyphNames.LowerA100, $275e);
  FGlyphCodes.Add(TdxGlyphNames.LowerA101, $2761);
  FGlyphCodes.Add(TdxGlyphNames.LowerA102, $2762);
  FGlyphCodes.Add(TdxGlyphNames.LowerA103, $2763);
  FGlyphCodes.Add(TdxGlyphNames.LowerA104, $2764);
  FGlyphCodes.Add(TdxGlyphNames.LowerA105, $2710);
  FGlyphCodes.Add(TdxGlyphNames.LowerA106, $2765);
  FGlyphCodes.Add(TdxGlyphNames.LowerA107, $2766);
  FGlyphCodes.Add(TdxGlyphNames.LowerA108, $2767);
  FGlyphCodes.Add(TdxGlyphNames.LowerA109, $2660);
  FGlyphCodes.Add(TdxGlyphNames.LowerA11, $261b);
  FGlyphCodes.Add(TdxGlyphNames.LowerA110, $2665);
  FGlyphCodes.Add(TdxGlyphNames.LowerA111, $2666);
  FGlyphCodes.Add(TdxGlyphNames.LowerA112, $2663);
  FGlyphCodes.Add(TdxGlyphNames.LowerA117, $2709);
  FGlyphCodes.Add(TdxGlyphNames.LowerA118, $2708);
  FGlyphCodes.Add(TdxGlyphNames.LowerA119, $2707);
  FGlyphCodes.Add(TdxGlyphNames.LowerA12, $261e);
  FGlyphCodes.Add(TdxGlyphNames.LowerA120, $2460);
  FGlyphCodes.Add(TdxGlyphNames.LowerA121, $2461);
  FGlyphCodes.Add(TdxGlyphNames.LowerA122, $2462);
  FGlyphCodes.Add(TdxGlyphNames.LowerA123, $2463);
  FGlyphCodes.Add(TdxGlyphNames.LowerA124, $2464);
  FGlyphCodes.Add(TdxGlyphNames.LowerA125, $2465);
  FGlyphCodes.Add(TdxGlyphNames.LowerA126, $2466);
  FGlyphCodes.Add(TdxGlyphNames.LowerA127, $2467);
  FGlyphCodes.Add(TdxGlyphNames.LowerA128, $2468);
  FGlyphCodes.Add(TdxGlyphNames.LowerA129, $2469);
  FGlyphCodes.Add(TdxGlyphNames.LowerA13, $270c);
  FGlyphCodes.Add(TdxGlyphNames.LowerA130, $2776);
  FGlyphCodes.Add(TdxGlyphNames.LowerA131, $2777);
  FGlyphCodes.Add(TdxGlyphNames.LowerA132, $2778);
  FGlyphCodes.Add(TdxGlyphNames.LowerA133, $2779);
  FGlyphCodes.Add(TdxGlyphNames.LowerA134, $277a);
  FGlyphCodes.Add(TdxGlyphNames.LowerA135, $277b);
  FGlyphCodes.Add(TdxGlyphNames.LowerA136, $277c);
  FGlyphCodes.Add(TdxGlyphNames.LowerA137, $277d);
  FGlyphCodes.Add(TdxGlyphNames.LowerA138, $277e);
  FGlyphCodes.Add(TdxGlyphNames.LowerA139, $277f);
  FGlyphCodes.Add(TdxGlyphNames.LowerA14, $270d);
  FGlyphCodes.Add(TdxGlyphNames.LowerA140, $2780);
  FGlyphCodes.Add(TdxGlyphNames.LowerA141, $2781);
  FGlyphCodes.Add(TdxGlyphNames.LowerA142, $2782);
  FGlyphCodes.Add(TdxGlyphNames.LowerA143, $2783);
  FGlyphCodes.Add(TdxGlyphNames.LowerA144, $2784);
  FGlyphCodes.Add(TdxGlyphNames.LowerA145, $2785);
  FGlyphCodes.Add(TdxGlyphNames.LowerA146, $2786);
  FGlyphCodes.Add(TdxGlyphNames.LowerA147, $2787);
  FGlyphCodes.Add(TdxGlyphNames.LowerA148, $2788);
  FGlyphCodes.Add(TdxGlyphNames.LowerA149, $2789);
  FGlyphCodes.Add(TdxGlyphNames.LowerA15, $270e);
  FGlyphCodes.Add(TdxGlyphNames.LowerA150, $278a);
  FGlyphCodes.Add(TdxGlyphNames.LowerA151, $278b);
  FGlyphCodes.Add(TdxGlyphNames.LowerA152, $278c);
  FGlyphCodes.Add(TdxGlyphNames.LowerA153, $278d);
  FGlyphCodes.Add(TdxGlyphNames.LowerA154, $278e);
  FGlyphCodes.Add(TdxGlyphNames.LowerA155, $278f);
  FGlyphCodes.Add(TdxGlyphNames.LowerA156, $2790);
  FGlyphCodes.Add(TdxGlyphNames.LowerA157, $2791);
  FGlyphCodes.Add(TdxGlyphNames.LowerA158, $2792);
  FGlyphCodes.Add(TdxGlyphNames.LowerA159, $2793);
  FGlyphCodes.Add(TdxGlyphNames.LowerA16, $270f);
  FGlyphCodes.Add(TdxGlyphNames.LowerA160, $2794);
  FGlyphCodes.Add(TdxGlyphNames.LowerA161, $2192);
  FGlyphCodes.Add(TdxGlyphNames.LowerA162, $27a3);
  FGlyphCodes.Add(TdxGlyphNames.LowerA163, $2194);
  FGlyphCodes.Add(TdxGlyphNames.LowerA164, $2195);
  FGlyphCodes.Add(TdxGlyphNames.LowerA165, $2799);
  FGlyphCodes.Add(TdxGlyphNames.LowerA166, $279b);
  FGlyphCodes.Add(TdxGlyphNames.LowerA167, $279c);
  FGlyphCodes.Add(TdxGlyphNames.LowerA168, $279d);
  FGlyphCodes.Add(TdxGlyphNames.LowerA169, $279e);
  FGlyphCodes.Add(TdxGlyphNames.LowerA17, $2711);
  FGlyphCodes.Add(TdxGlyphNames.LowerA170, $279f);
  FGlyphCodes.Add(TdxGlyphNames.LowerA171, $27a0);
  FGlyphCodes.Add(TdxGlyphNames.LowerA172, $27a1);
  FGlyphCodes.Add(TdxGlyphNames.LowerA173, $27a2);
  FGlyphCodes.Add(TdxGlyphNames.LowerA174, $27a4);
  FGlyphCodes.Add(TdxGlyphNames.LowerA175, $27a5);
  FGlyphCodes.Add(TdxGlyphNames.LowerA176, $27a6);
  FGlyphCodes.Add(TdxGlyphNames.LowerA177, $27a7);
  FGlyphCodes.Add(TdxGlyphNames.LowerA178, $27a8);
  FGlyphCodes.Add(TdxGlyphNames.LowerA179, $27a9);
  FGlyphCodes.Add(TdxGlyphNames.LowerA18, $2712);
  FGlyphCodes.Add(TdxGlyphNames.LowerA180, $27ab);
  FGlyphCodes.Add(TdxGlyphNames.LowerA181, $27ad);
  FGlyphCodes.Add(TdxGlyphNames.LowerA182, $27af);
  FGlyphCodes.Add(TdxGlyphNames.LowerA183, $27b2);
  FGlyphCodes.Add(TdxGlyphNames.LowerA184, $27b3);
  FGlyphCodes.Add(TdxGlyphNames.LowerA185, $27b5);
  FGlyphCodes.Add(TdxGlyphNames.LowerA186, $27b8);
  FGlyphCodes.Add(TdxGlyphNames.LowerA187, $27ba);
  FGlyphCodes.Add(TdxGlyphNames.LowerA188, $27bb);
  FGlyphCodes.Add(TdxGlyphNames.LowerA189, $27bc);
  FGlyphCodes.Add(TdxGlyphNames.LowerA19, $2713);
  FGlyphCodes.Add(TdxGlyphNames.LowerA190, $27bd);
  FGlyphCodes.Add(TdxGlyphNames.LowerA191, $27be);
  FGlyphCodes.Add(TdxGlyphNames.LowerA192, $279a);
  FGlyphCodes.Add(TdxGlyphNames.LowerA193, $27aa);
  FGlyphCodes.Add(TdxGlyphNames.LowerA194, $27b6);
  FGlyphCodes.Add(TdxGlyphNames.LowerA195, $27b9);
  FGlyphCodes.Add(TdxGlyphNames.LowerA196, $2798);
  FGlyphCodes.Add(TdxGlyphNames.LowerA197, $27b4);
  FGlyphCodes.Add(TdxGlyphNames.LowerA198, $27b7);
  FGlyphCodes.Add(TdxGlyphNames.LowerA199, $27ac);
  FGlyphCodes.Add(TdxGlyphNames.LowerA2, $2702);
  FGlyphCodes.Add(TdxGlyphNames.LowerA20, $2714);
  FGlyphCodes.Add(TdxGlyphNames.LowerA200, $27ae);
  FGlyphCodes.Add(TdxGlyphNames.LowerA201, $27b1);
  FGlyphCodes.Add(TdxGlyphNames.LowerA202, $2703);
  FGlyphCodes.Add(TdxGlyphNames.LowerA203, $2750);
  FGlyphCodes.Add(TdxGlyphNames.LowerA204, $2752);
  FGlyphCodes.Add(TdxGlyphNames.LowerA205, $f8dd);
  FGlyphCodes.Add(TdxGlyphNames.LowerA206, $f8df);
  FGlyphCodes.Add(TdxGlyphNames.LowerA21, $2715);
  FGlyphCodes.Add(TdxGlyphNames.LowerA22, $2716);
  FGlyphCodes.Add(TdxGlyphNames.LowerA23, $2717);
  FGlyphCodes.Add(TdxGlyphNames.LowerA24, $2718);
  FGlyphCodes.Add(TdxGlyphNames.LowerA25, $2719);
  FGlyphCodes.Add(TdxGlyphNames.LowerA26, $271a);
  FGlyphCodes.Add(TdxGlyphNames.LowerA27, $271b);
  FGlyphCodes.Add(TdxGlyphNames.LowerA28, $271c);
  FGlyphCodes.Add(TdxGlyphNames.LowerA29, $2722);
  FGlyphCodes.Add(TdxGlyphNames.LowerA3, $2704);
  FGlyphCodes.Add(TdxGlyphNames.LowerA30, $2723);
  FGlyphCodes.Add(TdxGlyphNames.LowerA31, $2724);
  FGlyphCodes.Add(TdxGlyphNames.LowerA32, $2725);
  FGlyphCodes.Add(TdxGlyphNames.LowerA33, $2726);
  FGlyphCodes.Add(TdxGlyphNames.LowerA34, $2727);
  FGlyphCodes.Add(TdxGlyphNames.LowerA35, $2605);
  FGlyphCodes.Add(TdxGlyphNames.LowerA36, $2729);
  FGlyphCodes.Add(TdxGlyphNames.LowerA37, $272a);
  FGlyphCodes.Add(TdxGlyphNames.LowerA38, $272b);
  FGlyphCodes.Add(TdxGlyphNames.LowerA39, $272c);
  FGlyphCodes.Add(TdxGlyphNames.LowerA4, $260e);
  FGlyphCodes.Add(TdxGlyphNames.LowerA40, $272d);
  FGlyphCodes.Add(TdxGlyphNames.LowerA41, $272e);
  FGlyphCodes.Add(TdxGlyphNames.LowerA42, $272f);
  FGlyphCodes.Add(TdxGlyphNames.LowerA43, $2730);
  FGlyphCodes.Add(TdxGlyphNames.LowerA44, $2731);
  FGlyphCodes.Add(TdxGlyphNames.LowerA45, $2732);
  FGlyphCodes.Add(TdxGlyphNames.LowerA46, $2733);
  FGlyphCodes.Add(TdxGlyphNames.LowerA47, $2734);
  FGlyphCodes.Add(TdxGlyphNames.LowerA48, $2735);
  FGlyphCodes.Add(TdxGlyphNames.LowerA49, $2736);
  FGlyphCodes.Add(TdxGlyphNames.LowerA5, $2706);
  FGlyphCodes.Add(TdxGlyphNames.LowerA50, $2737);
  FGlyphCodes.Add(TdxGlyphNames.LowerA51, $2738);
  FGlyphCodes.Add(TdxGlyphNames.LowerA52, $2739);
  FGlyphCodes.Add(TdxGlyphNames.LowerA53, $273a);
  FGlyphCodes.Add(TdxGlyphNames.LowerA54, $273b);
  FGlyphCodes.Add(TdxGlyphNames.LowerA55, $273c);
  FGlyphCodes.Add(TdxGlyphNames.LowerA56, $273d);
  FGlyphCodes.Add(TdxGlyphNames.LowerA57, $273e);
  FGlyphCodes.Add(TdxGlyphNames.LowerA58, $273f);
  FGlyphCodes.Add(TdxGlyphNames.LowerA59, $2740);
  FGlyphCodes.Add(TdxGlyphNames.LowerA6, $271d);
  FGlyphCodes.Add(TdxGlyphNames.LowerA60, $2741);
  FGlyphCodes.Add(TdxGlyphNames.LowerA61, $2742);
  FGlyphCodes.Add(TdxGlyphNames.LowerA62, $2743);
  FGlyphCodes.Add(TdxGlyphNames.LowerA63, $2744);
  FGlyphCodes.Add(TdxGlyphNames.LowerA64, $2745);
  FGlyphCodes.Add(TdxGlyphNames.LowerA65, $2746);
  FGlyphCodes.Add(TdxGlyphNames.LowerA66, $2747);
  FGlyphCodes.Add(TdxGlyphNames.LowerA67, $2748);
  FGlyphCodes.Add(TdxGlyphNames.LowerA68, $2749);
  FGlyphCodes.Add(TdxGlyphNames.LowerA69, $274a);
  FGlyphCodes.Add(TdxGlyphNames.LowerA7, $271e);
  FGlyphCodes.Add(TdxGlyphNames.LowerA70, $274b);
  FGlyphCodes.Add(TdxGlyphNames.LowerA71, $25cf);
  FGlyphCodes.Add(TdxGlyphNames.LowerA72, $274d);
  FGlyphCodes.Add(TdxGlyphNames.LowerA73, $25a0);
  FGlyphCodes.Add(TdxGlyphNames.LowerA74, $274f);
  FGlyphCodes.Add(TdxGlyphNames.LowerA75, $2751);
  FGlyphCodes.Add(TdxGlyphNames.LowerA76, $25b2);
  FGlyphCodes.Add(TdxGlyphNames.LowerA77, $25bc);
  FGlyphCodes.Add(TdxGlyphNames.LowerA78, $25c6);
  FGlyphCodes.Add(TdxGlyphNames.LowerA79, $2756);
  FGlyphCodes.Add(TdxGlyphNames.LowerA8, $271f);
  FGlyphCodes.Add(TdxGlyphNames.LowerA81, $25d7);
  FGlyphCodes.Add(TdxGlyphNames.LowerA82, $2758);
  FGlyphCodes.Add(TdxGlyphNames.LowerA83, $2759);
  FGlyphCodes.Add(TdxGlyphNames.LowerA84, $275a);
  FGlyphCodes.Add(TdxGlyphNames.LowerA85, $f8de);
  FGlyphCodes.Add(TdxGlyphNames.LowerA86, $f8e0);
  FGlyphCodes.Add(TdxGlyphNames.LowerA87, $f8e1);
  FGlyphCodes.Add(TdxGlyphNames.LowerA88, $f8e2);
  FGlyphCodes.Add(TdxGlyphNames.LowerA89, $f8d7);
  FGlyphCodes.Add(TdxGlyphNames.LowerA9, $2720);
  FGlyphCodes.Add(TdxGlyphNames.LowerA90, $f8d8);
  FGlyphCodes.Add(TdxGlyphNames.LowerA91, $f8db);
  FGlyphCodes.Add(TdxGlyphNames.LowerA92, $f8dc);
  FGlyphCodes.Add(TdxGlyphNames.LowerA93, $f8d9);
  FGlyphCodes.Add(TdxGlyphNames.LowerA94, $f8da);
  FGlyphCodes.Add(TdxGlyphNames.LowerA95, $f8e3);
  FGlyphCodes.Add(TdxGlyphNames.LowerA96, $f8e4);
  FGlyphCodes.Add(TdxGlyphNames.LowerA97, $275b);
  FGlyphCodes.Add(TdxGlyphNames.LowerA98, $275c);
  FGlyphCodes.Add(TdxGlyphNames.LowerA99, $275d);
  FGlyphCodes.Add(TdxGlyphNames.LowerAacute, $e1);
  FGlyphCodes.Add(TdxGlyphNames.LowerAbreve, $103);
  FGlyphCodes.Add(TdxGlyphNames.LowerAcircumflex, $e2);
  FGlyphCodes.Add(TdxGlyphNames.LowerAcute, $b4);
  FGlyphCodes.Add(TdxGlyphNames.LowerAdieresis, $e4);
  FGlyphCodes.Add(TdxGlyphNames.LowerAe, $e6);
  FGlyphCodes.Add(TdxGlyphNames.LowerAeacute, $1fd);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii00208, $2015);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii08941, $20a4);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10017, $410);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10018, $411);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10019, $412);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10020, $413);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10021, $414);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10022, $415);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10023, $401);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10024, $416);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10025, $417);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10026, $418);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10027, $419);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10028, $41a);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10029, $41b);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10030, $41c);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10031, $41d);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10032, $41e);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10033, $41f);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10034, $420);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10035, $421);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10036, $422);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10037, $423);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10038, $424);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10039, $425);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10040, $426);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10041, $427);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10042, $428);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10043, $429);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10044, $42a);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10045, $42b);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10046, $42c);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10047, $42d);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10048, $42e);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10049, $42f);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10050, $490);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10051, $402);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10052, $403);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10053, $404);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10054, $405);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10055, $406);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10056, $407);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10057, $408);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10058, $409);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10059, $40a);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10060, $40b);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10061, $40c);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10062, $40e);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10065, $430);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10066, $431);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10067, $432);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10068, $433);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10069, $434);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10070, $435);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10071, $451);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10072, $436);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10073, $437);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10074, $438);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10075, $439);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10076, $43a);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10077, $43b);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10078, $43c);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10079, $43d);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10080, $43e);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10081, $43f);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10082, $440);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10083, $441);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10084, $442);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10085, $443);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10086, $444);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10087, $445);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10088, $446);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10089, $447);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10090, $448);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10091, $449);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10092, $44a);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10093, $44b);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10094, $44c);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10095, $44d);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10096, $44e);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10097, $44f);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10098, $491);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10099, $452);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10100, $453);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10101, $454);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10102, $455);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10103, $456);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10104, $457);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10105, $458);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10106, $459);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10107, $45a);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10108, $45b);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10109, $45c);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10110, $45e);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10145, $40f);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii10193, $45f);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii61248, $2105);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii61289, $2113);
  FGlyphCodes.Add(TdxGlyphNames.LowerAfii61352, $2116);
  FGlyphCodes.Add(TdxGlyphNames.LowerAgrave, $e0);
  FGlyphCodes.Add(TdxGlyphNames.LowerAleph, $2135);
  FGlyphCodes.Add(TdxGlyphNames.LowerAlpha, $3b1);
  FGlyphCodes.Add(TdxGlyphNames.LowerAlphatonos, $3ac);
  FGlyphCodes.Add(TdxGlyphNames.LowerAmacron, $101);
  FGlyphCodes.Add(TdxGlyphNames.LowerAmpersand, $26);
  FGlyphCodes.Add(TdxGlyphNames.LowerAngle, $2220);
  FGlyphCodes.Add(TdxGlyphNames.LowerAngleleft, $2329);
  FGlyphCodes.Add(TdxGlyphNames.LowerAngleright, $232a);
  FGlyphCodes.Add(TdxGlyphNames.LowerAnoteleia, $387);
  FGlyphCodes.Add(TdxGlyphNames.LowerAogonek, $105);
  FGlyphCodes.Add(TdxGlyphNames.LowerApplelogo, $f000);
  FGlyphCodes.Add(TdxGlyphNames.LowerApproxequal, $2248);
  FGlyphCodes.Add(TdxGlyphNames.LowerAring, $e5);
  FGlyphCodes.Add(TdxGlyphNames.LowerAringacute, $1fb);
  FGlyphCodes.Add(TdxGlyphNames.LowerArrowboth, $2194);
  FGlyphCodes.Add(TdxGlyphNames.LowerArrowdblboth, $21d4);
  FGlyphCodes.Add(TdxGlyphNames.LowerArrowdbldown, $21d3);
  FGlyphCodes.Add(TdxGlyphNames.LowerArrowdblleft, $21d0);
  FGlyphCodes.Add(TdxGlyphNames.LowerArrowdblright, $21d2);
  FGlyphCodes.Add(TdxGlyphNames.LowerArrowdblup, $21d1);
  FGlyphCodes.Add(TdxGlyphNames.LowerArrowdown, $2193);
  FGlyphCodes.Add(TdxGlyphNames.LowerArrowhorizex, $f8e7);
  FGlyphCodes.Add(TdxGlyphNames.LowerArrowleft, $2190);
  FGlyphCodes.Add(TdxGlyphNames.LowerArrowright, $2192);
  FGlyphCodes.Add(TdxGlyphNames.LowerArrowup, $2191);
  FGlyphCodes.Add(TdxGlyphNames.LowerArrowupdn, $2195);
  FGlyphCodes.Add(TdxGlyphNames.LowerArrowupdnbse, $21a8);
  FGlyphCodes.Add(TdxGlyphNames.LowerArrowvertex, $f8e6);
  FGlyphCodes.Add(TdxGlyphNames.LowerAsciicircum, $5e);
  FGlyphCodes.Add(TdxGlyphNames.LowerAsciitilde, $7e);
  FGlyphCodes.Add(TdxGlyphNames.LowerAsterisk, $2a);
  FGlyphCodes.Add(TdxGlyphNames.LowerAsteriskmath, $2217);
  FGlyphCodes.Add(TdxGlyphNames.LowerAt, $40);
  FGlyphCodes.Add(TdxGlyphNames.LowerAtilde, $e3);
  FGlyphCodes.Add(TdxGlyphNames.LowerB, $62);
  FGlyphCodes.Add(TdxGlyphNames.LowerBackslash, $5c);
  FGlyphCodes.Add(TdxGlyphNames.LowerBar, $7c);
  FGlyphCodes.Add(TdxGlyphNames.LowerBeta, $3b2);
  FGlyphCodes.Add(TdxGlyphNames.LowerBlock, $2588);
  FGlyphCodes.Add(TdxGlyphNames.LowerBraceex, $f8f4);
  FGlyphCodes.Add(TdxGlyphNames.LowerBraceleft, $7b);
  FGlyphCodes.Add(TdxGlyphNames.LowerBraceleftbt, $f8f1);
  FGlyphCodes.Add(TdxGlyphNames.LowerBraceleftmid, $f8f2);
  FGlyphCodes.Add(TdxGlyphNames.LowerBracelefttp, $f8f1);
  FGlyphCodes.Add(TdxGlyphNames.LowerBracerightbt, $f8fe);
  FGlyphCodes.Add(TdxGlyphNames.LowerBracerightmid, $f8fd);
  FGlyphCodes.Add(TdxGlyphNames.LowerBracerighttp, $f8fc);
  FGlyphCodes.Add(TdxGlyphNames.LowerBraceright, $7d);
  FGlyphCodes.Add(TdxGlyphNames.LowerBracketleft, $5b);
  FGlyphCodes.Add(TdxGlyphNames.LowerBracketleftbt, $f8f0);
  FGlyphCodes.Add(TdxGlyphNames.LowerBracketleftex, $f8ef);
  FGlyphCodes.Add(TdxGlyphNames.LowerBracketlefttp, $f8ee);
  FGlyphCodes.Add(TdxGlyphNames.LowerBracketright, $5d);
  FGlyphCodes.Add(TdxGlyphNames.LowerBracketrightbt, $f8fb);
  FGlyphCodes.Add(TdxGlyphNames.LowerBracketrightex, $f8fa);
  FGlyphCodes.Add(TdxGlyphNames.LowerBracketrighttp, $f8f9);
  FGlyphCodes.Add(TdxGlyphNames.LowerBreve, $2d8);
  FGlyphCodes.Add(TdxGlyphNames.LowerBrokenbar, $a6);
  FGlyphCodes.Add(TdxGlyphNames.LowerBullet, $2022);
  FGlyphCodes.Add(TdxGlyphNames.LowerC, $63);
  FGlyphCodes.Add(TdxGlyphNames.LowerCacute, $107);
  FGlyphCodes.Add(TdxGlyphNames.LowerCaron, $2c7);
  FGlyphCodes.Add(TdxGlyphNames.LowerCarriagereturn, $21b5);
  FGlyphCodes.Add(TdxGlyphNames.LowerCcaron, $10d);
  FGlyphCodes.Add(TdxGlyphNames.LowerCcedilla, $e7);
  FGlyphCodes.Add(TdxGlyphNames.LowerCcircumflex, $109);
  FGlyphCodes.Add(TdxGlyphNames.LowerCdot, $10b);
  FGlyphCodes.Add(TdxGlyphNames.LowerCedilla, $b8);
  FGlyphCodes.Add(TdxGlyphNames.LowerCent, $a2);
  FGlyphCodes.Add(TdxGlyphNames.LowerChi, $3c7);
  FGlyphCodes.Add(TdxGlyphNames.LowerCircle, $25cb);
  FGlyphCodes.Add(TdxGlyphNames.LowerCirclemultiply, $2297);
  FGlyphCodes.Add(TdxGlyphNames.LowerCircleplus, $2295);
  FGlyphCodes.Add(TdxGlyphNames.LowerCircumflex, $2c6);
  FGlyphCodes.Add(TdxGlyphNames.LowerClub, $2663);
  FGlyphCodes.Add(TdxGlyphNames.LowerColon, $3a);
  FGlyphCodes.Add(TdxGlyphNames.LowerComma, $2c);
  FGlyphCodes.Add(TdxGlyphNames.LowerCongruent, $2245);
  FGlyphCodes.Add(TdxGlyphNames.LowerCopyright, $a9);
  FGlyphCodes.Add(TdxGlyphNames.LowerCopyrightsans, $f8e9);
  FGlyphCodes.Add(TdxGlyphNames.LowerCopyrightserif, $f6d9);
  FGlyphCodes.Add(TdxGlyphNames.LowerCurrency, $a4);
  FGlyphCodes.Add(TdxGlyphNames.LowerD, $64);
  FGlyphCodes.Add(TdxGlyphNames.LowerDagger, $2020);
  FGlyphCodes.Add(TdxGlyphNames.LowerDaggerdbl, $2021);
  FGlyphCodes.Add(TdxGlyphNames.LowerDcaron, $10f);
  FGlyphCodes.Add(TdxGlyphNames.LowerDcroat, $111);
  FGlyphCodes.Add(TdxGlyphNames.LowerDegree, $b0);
  FGlyphCodes.Add(TdxGlyphNames.LowerDelta, $3b4);
  FGlyphCodes.Add(TdxGlyphNames.LowerDiamond, $2666);
  FGlyphCodes.Add(TdxGlyphNames.LowerDieresis, $a8);
  FGlyphCodes.Add(TdxGlyphNames.LowerDieresistonos, $385);
  FGlyphCodes.Add(TdxGlyphNames.LowerDivide, $f7);
  FGlyphCodes.Add(TdxGlyphNames.LowerDkshade, $2593);
  FGlyphCodes.Add(TdxGlyphNames.LowerDnblock, $2584);
  FGlyphCodes.Add(TdxGlyphNames.LowerDollar, $24);
  FGlyphCodes.Add(TdxGlyphNames.LowerDotaccent, $2d9);
  FGlyphCodes.Add(TdxGlyphNames.LowerDotlessi, $131);
  FGlyphCodes.Add(TdxGlyphNames.LowerDotmath, $22c5);
  FGlyphCodes.Add(TdxGlyphNames.LowerE, $65);
  FGlyphCodes.Add(TdxGlyphNames.LowerEacute, $e9);
  FGlyphCodes.Add(TdxGlyphNames.LowerEbreve, $115);
  FGlyphCodes.Add(TdxGlyphNames.LowerEcaron, $11b);
  FGlyphCodes.Add(TdxGlyphNames.LowerEcircumflex, $ea);
  FGlyphCodes.Add(TdxGlyphNames.LowerEdieresis, $eb);
  FGlyphCodes.Add(TdxGlyphNames.LowerEdot, $117);
  FGlyphCodes.Add(TdxGlyphNames.LowerEgrave, $e8);
  FGlyphCodes.Add(TdxGlyphNames.LowerEight, $38);
  FGlyphCodes.Add(TdxGlyphNames.LowerElement, $2208);
  FGlyphCodes.Add(TdxGlyphNames.LowerEllipsis, $2026);
  FGlyphCodes.Add(TdxGlyphNames.LowerEmacron, $113);
  FGlyphCodes.Add(TdxGlyphNames.LowerEmdash, $2014);
  FGlyphCodes.Add(TdxGlyphNames.LowerEmptyset, $2205);
  FGlyphCodes.Add(TdxGlyphNames.LowerEndash, $2013);
  FGlyphCodes.Add(TdxGlyphNames.LowerEng, $14b);
  FGlyphCodes.Add(TdxGlyphNames.LowerEqual, $3d);
  FGlyphCodes.Add(TdxGlyphNames.LowerEogonek, $119);
  FGlyphCodes.Add(TdxGlyphNames.LowerEpsilon, $3b5);
  FGlyphCodes.Add(TdxGlyphNames.LowerEpsilontonos, $3ad);
  FGlyphCodes.Add(TdxGlyphNames.LowerEquivalence, $2261);
  FGlyphCodes.Add(TdxGlyphNames.LowerEstimated, $212e);
  FGlyphCodes.Add(TdxGlyphNames.LowerEta, $3b7);
  FGlyphCodes.Add(TdxGlyphNames.LowerEtatonos, $3ae);
  FGlyphCodes.Add(TdxGlyphNames.LowerEth, $f0);
  FGlyphCodes.Add(TdxGlyphNames.LowerExclam, $21);
  FGlyphCodes.Add(TdxGlyphNames.LowerExclamdbl, $203c);
  FGlyphCodes.Add(TdxGlyphNames.LowerExclamdown, $a1);
  FGlyphCodes.Add(TdxGlyphNames.LowerExistential, $2203);
  FGlyphCodes.Add(TdxGlyphNames.LowerF, $66);
  FGlyphCodes.Add(TdxGlyphNames.LowerFemale, $2640);
  FGlyphCodes.Add(TdxGlyphNames.LowerFi, $fb01);
  FGlyphCodes.Add(TdxGlyphNames.LowerFilledbox, $25a0);
  FGlyphCodes.Add(TdxGlyphNames.LowerFilledrect, $25ac);
  FGlyphCodes.Add(TdxGlyphNames.LowerFive, $35);
  FGlyphCodes.Add(TdxGlyphNames.LowerFiveeighths, $215d);
  FGlyphCodes.Add(TdxGlyphNames.LowerFl, $fb02);
  FGlyphCodes.Add(TdxGlyphNames.LowerFlorin, $192);
  FGlyphCodes.Add(TdxGlyphNames.LowerFour, $34);
  FGlyphCodes.Add(TdxGlyphNames.LowerFraction, $2044);
  FGlyphCodes.Add(TdxGlyphNames.LowerFranc, $20a3);
  FGlyphCodes.Add(TdxGlyphNames.LowerG, $67);
  FGlyphCodes.Add(TdxGlyphNames.LowerGamma, $3b3);
  FGlyphCodes.Add(TdxGlyphNames.LowerGbreve, $11f);
  FGlyphCodes.Add(TdxGlyphNames.LowerGcedilla, $123);
  FGlyphCodes.Add(TdxGlyphNames.LowerGcircumflex, $11d);
  FGlyphCodes.Add(TdxGlyphNames.LowerGdot, $0121);
  FGlyphCodes.Add(TdxGlyphNames.LowerGermandbls, $df);
  FGlyphCodes.Add(TdxGlyphNames.LowerGradient, $2207);
  FGlyphCodes.Add(TdxGlyphNames.LowerGrave, $60);
  FGlyphCodes.Add(TdxGlyphNames.LowerGreater, $3e);
  FGlyphCodes.Add(TdxGlyphNames.LowerGreaterequal, $2265);
  FGlyphCodes.Add(TdxGlyphNames.LowerGuillemotleft, $ab);
  FGlyphCodes.Add(TdxGlyphNames.LowerGuillemotright, $bb);
  FGlyphCodes.Add(TdxGlyphNames.LowerGuilsinglleft, $2039);
  FGlyphCodes.Add(TdxGlyphNames.LowerGuilsinglright, $203a);
  FGlyphCodes.Add(TdxGlyphNames.LowerH, $68);
  FGlyphCodes.Add(TdxGlyphNames.LowerHbar, $127);
  FGlyphCodes.Add(TdxGlyphNames.LowerHcircumflex, $125);
  FGlyphCodes.Add(TdxGlyphNames.LowerHeart, $2665);
  FGlyphCodes.Add(TdxGlyphNames.LowerHouse, $2302);
  FGlyphCodes.Add(TdxGlyphNames.LowerHungarumlaut, $2dd);
  FGlyphCodes.Add(TdxGlyphNames.LowerHyphen, $2d);
  FGlyphCodes.Add(TdxGlyphNames.LowerI, $69);
  FGlyphCodes.Add(TdxGlyphNames.LowerIacute, $ed);
  FGlyphCodes.Add(TdxGlyphNames.LowerIbreve, $12d);
  FGlyphCodes.Add(TdxGlyphNames.LowerIcircumflex, $ee);
  FGlyphCodes.Add(TdxGlyphNames.LowerIdieresis, $ef);
  FGlyphCodes.Add(TdxGlyphNames.LowerIgrave, $ec);
  FGlyphCodes.Add(TdxGlyphNames.LowerIj, $133);
  FGlyphCodes.Add(TdxGlyphNames.LowerImacron, $12b);
  FGlyphCodes.Add(TdxGlyphNames.LowerIncrement, $2206);
  FGlyphCodes.Add(TdxGlyphNames.LowerInfinity, $221e);
  FGlyphCodes.Add(TdxGlyphNames.LowerIntegral, $222b);
  FGlyphCodes.Add(TdxGlyphNames.LowerIntegralbt, $2321);
  FGlyphCodes.Add(TdxGlyphNames.LowerIntegralex, $f8f5);
  FGlyphCodes.Add(TdxGlyphNames.LowerIntegraltp, $2320);
  FGlyphCodes.Add(TdxGlyphNames.LowerIntersection, $2229);
  FGlyphCodes.Add(TdxGlyphNames.LowerInvbullet, $25d8);
  FGlyphCodes.Add(TdxGlyphNames.LowerInvcircle, $25d9);
  FGlyphCodes.Add(TdxGlyphNames.LowerInvsmileface, $263b);
  FGlyphCodes.Add(TdxGlyphNames.LowerIogonek, $12f);
  FGlyphCodes.Add(TdxGlyphNames.LowerIota, $3b9);
  FGlyphCodes.Add(TdxGlyphNames.LowerIotadieresis, $3ca);
  FGlyphCodes.Add(TdxGlyphNames.LowerIotadieresistonos, $390);
  FGlyphCodes.Add(TdxGlyphNames.LowerIotatonos, $3af);
  FGlyphCodes.Add(TdxGlyphNames.LowerItilde, $129);
  FGlyphCodes.Add(TdxGlyphNames.LowerJ, $6a);
  FGlyphCodes.Add(TdxGlyphNames.LowerJcircumflex, $135);
  FGlyphCodes.Add(TdxGlyphNames.LowerK, $6b);
  FGlyphCodes.Add(TdxGlyphNames.LowerKappa, $3ba);
  FGlyphCodes.Add(TdxGlyphNames.LowerKcedilla, $137);
  FGlyphCodes.Add(TdxGlyphNames.LowerKgreenlandic, $138);
  FGlyphCodes.Add(TdxGlyphNames.LowerL, $6c);
  FGlyphCodes.Add(TdxGlyphNames.LowerLacute, $13a);
  FGlyphCodes.Add(TdxGlyphNames.LowerLambda, $3bb);
  FGlyphCodes.Add(TdxGlyphNames.LowerLcaron, $13e);
  FGlyphCodes.Add(TdxGlyphNames.LowerLcedilla, $13c);
  FGlyphCodes.Add(TdxGlyphNames.LowerLdot, $140);
  FGlyphCodes.Add(TdxGlyphNames.LowerLess, $3c);
  FGlyphCodes.Add(TdxGlyphNames.LowerLessequal, $2264);
  FGlyphCodes.Add(TdxGlyphNames.LowerLfblock, $258c);
  FGlyphCodes.Add(TdxGlyphNames.LowerLogicaland, $2227);
  FGlyphCodes.Add(TdxGlyphNames.LowerLogicalnot, $ac);
  FGlyphCodes.Add(TdxGlyphNames.LowerLogicalor, $2228);
  FGlyphCodes.Add(TdxGlyphNames.LowerLongs, $17f);
  FGlyphCodes.Add(TdxGlyphNames.LowerLozenge, $25ca);
  FGlyphCodes.Add(TdxGlyphNames.LowerLslash, $142);
  FGlyphCodes.Add(TdxGlyphNames.LowerLtshade, $2591);
  FGlyphCodes.Add(TdxGlyphNames.LowerM, $6d);
  FGlyphCodes.Add(TdxGlyphNames.LowerMacron, $af);
  FGlyphCodes.Add(TdxGlyphNames.LowerMale, $2642);
  FGlyphCodes.Add(TdxGlyphNames.LowerMinus, $2212);
  FGlyphCodes.Add(TdxGlyphNames.LowerMinute, $2032);
  FGlyphCodes.Add(TdxGlyphNames.LowerMu, $b5);
  FGlyphCodes.Add(TdxGlyphNames.LowerMultiply, $d7);
  FGlyphCodes.Add(TdxGlyphNames.LowerMusicalnote, $266a);
  FGlyphCodes.Add(TdxGlyphNames.LowerMusicalnotedbl, $266b);
  FGlyphCodes.Add(TdxGlyphNames.LowerN, $6e);
  FGlyphCodes.Add(TdxGlyphNames.LowerNacute, $144);
  FGlyphCodes.Add(TdxGlyphNames.LowerNapostrophe, $149);
  FGlyphCodes.Add(TdxGlyphNames.LowerNbspace, $a0);
  FGlyphCodes.Add(TdxGlyphNames.LowerNcaron, $148);
  FGlyphCodes.Add(TdxGlyphNames.LowerNcedilla, $146);
  FGlyphCodes.Add(TdxGlyphNames.LowerNine, $39);
  FGlyphCodes.Add(TdxGlyphNames.LowerNotelement, $2209);
  FGlyphCodes.Add(TdxGlyphNames.LowerNotequal, $2260);
  FGlyphCodes.Add(TdxGlyphNames.LowerNotsubset, $2284);
  FGlyphCodes.Add(TdxGlyphNames.LowerNsuperior, $207f);
  FGlyphCodes.Add(TdxGlyphNames.LowerNtilde, $f1);
  FGlyphCodes.Add(TdxGlyphNames.LowerNu, $3bd);
  FGlyphCodes.Add(TdxGlyphNames.LowerNumbersign, $23);
  FGlyphCodes.Add(TdxGlyphNames.LowerO, $6f);
  FGlyphCodes.Add(TdxGlyphNames.LowerOacute, $f3);
  FGlyphCodes.Add(TdxGlyphNames.LowerObreve, $14f);
  FGlyphCodes.Add(TdxGlyphNames.LowerOcircumflex, $f4);
  FGlyphCodes.Add(TdxGlyphNames.LowerOdblacute, $151);
  FGlyphCodes.Add(TdxGlyphNames.LowerOdieresis, $f6);
  FGlyphCodes.Add(TdxGlyphNames.LowerOe, $153);
  FGlyphCodes.Add(TdxGlyphNames.LowerOgonek, $2db);
  FGlyphCodes.Add(TdxGlyphNames.LowerOgrave, $f2);
  FGlyphCodes.Add(TdxGlyphNames.LowerOmacron, $14d);
  FGlyphCodes.Add(TdxGlyphNames.LowerOmega, $3c9);
  FGlyphCodes.Add(TdxGlyphNames.LowerOmega1, $3d6);
  FGlyphCodes.Add(TdxGlyphNames.LowerOmegatonos, $3ce);
  FGlyphCodes.Add(TdxGlyphNames.LowerOmicron, $3bf);
  FGlyphCodes.Add(TdxGlyphNames.LowerOmicrontonos, $3cc);
  FGlyphCodes.Add(TdxGlyphNames.LowerOne, $31);
  FGlyphCodes.Add(TdxGlyphNames.LowerOneeighth, $215b);
  FGlyphCodes.Add(TdxGlyphNames.LowerOnehalf, $bd);
  FGlyphCodes.Add(TdxGlyphNames.LowerOnequarter, $bc);
  FGlyphCodes.Add(TdxGlyphNames.LowerOnesuperior, $b9);
  FGlyphCodes.Add(TdxGlyphNames.LowerOpenbullet, $25e6);
  FGlyphCodes.Add(TdxGlyphNames.LowerOrdfeminine, $aa);
  FGlyphCodes.Add(TdxGlyphNames.LowerOrdmasculine, $ba);
  FGlyphCodes.Add(TdxGlyphNames.LowerOrthogonal, $221f);
  FGlyphCodes.Add(TdxGlyphNames.LowerOslash, $f8);
  FGlyphCodes.Add(TdxGlyphNames.LowerOslashacute, $1ff);
  FGlyphCodes.Add(TdxGlyphNames.LowerOtilde, $f5);
  FGlyphCodes.Add(TdxGlyphNames.LowerP, $70);
  FGlyphCodes.Add(TdxGlyphNames.LowerParagraph, $b6);
  FGlyphCodes.Add(TdxGlyphNames.LowerParenleft, $28);
  FGlyphCodes.Add(TdxGlyphNames.LowerParenleftbt, $f8ed);
  FGlyphCodes.Add(TdxGlyphNames.LowerParenleftex, $f8ec);
  FGlyphCodes.Add(TdxGlyphNames.LowerParenlefttp, $f8eb);
  FGlyphCodes.Add(TdxGlyphNames.LowerParenright, $29);
  FGlyphCodes.Add(TdxGlyphNames.LowerParenrightbt, $f8f8);
  FGlyphCodes.Add(TdxGlyphNames.LowerParenrightex, $f8f7);
  FGlyphCodes.Add(TdxGlyphNames.LowerParenrighttp, $f8f6);
  FGlyphCodes.Add(TdxGlyphNames.LowerPartialdiff, $2202);
  FGlyphCodes.Add(TdxGlyphNames.LowerPercent, $25);
  FGlyphCodes.Add(TdxGlyphNames.LowerPeriod, $2e);
  FGlyphCodes.Add(TdxGlyphNames.LowerPeriodcentered, $b7);
  FGlyphCodes.Add(TdxGlyphNames.LowerPerpendicular, $22a5);
  FGlyphCodes.Add(TdxGlyphNames.LowerPerthousand, $2030);
  FGlyphCodes.Add(TdxGlyphNames.LowerPeseta, $20a7);
  FGlyphCodes.Add(TdxGlyphNames.LowerPhi, $3c6);
  FGlyphCodes.Add(TdxGlyphNames.LowerPhi1, $3d5);
  FGlyphCodes.Add(TdxGlyphNames.LowerPi, $3c0);
  FGlyphCodes.Add(TdxGlyphNames.LowerPlus, $2b);
  FGlyphCodes.Add(TdxGlyphNames.LowerPlusminus, $b1);
  FGlyphCodes.Add(TdxGlyphNames.LowerProduct, $220f);
  FGlyphCodes.Add(TdxGlyphNames.LowerPropersubset, $2282);
  FGlyphCodes.Add(TdxGlyphNames.LowerPropersuperset, $2283);
  FGlyphCodes.Add(TdxGlyphNames.LowerProportional, $221d);
  FGlyphCodes.Add(TdxGlyphNames.LowerPsi, $3c8);
  FGlyphCodes.Add(TdxGlyphNames.LowerQ, $71);
  FGlyphCodes.Add(TdxGlyphNames.LowerQuestion, $3f);
  FGlyphCodes.Add(TdxGlyphNames.LowerQuestiondown, $bf);
  FGlyphCodes.Add(TdxGlyphNames.LowerQuotedbl, $22);
  FGlyphCodes.Add(TdxGlyphNames.LowerQuotedblbase, $201e);
  FGlyphCodes.Add(TdxGlyphNames.LowerQuotedblleft, $201c);
  FGlyphCodes.Add(TdxGlyphNames.LowerQuotedblright, $201d);
  FGlyphCodes.Add(TdxGlyphNames.LowerQuoteleft, $2018);
  FGlyphCodes.Add(TdxGlyphNames.LowerQuotereversed, $201b);
  FGlyphCodes.Add(TdxGlyphNames.LowerQuoteright, $2019);
  FGlyphCodes.Add(TdxGlyphNames.LowerQuotesinglbase, $201a);
  FGlyphCodes.Add(TdxGlyphNames.LowerQuotesingle, $27);
  FGlyphCodes.Add(TdxGlyphNames.LowerR, $72);
  FGlyphCodes.Add(TdxGlyphNames.LowerRacute, $155);
  FGlyphCodes.Add(TdxGlyphNames.LowerRadical, $221a);
  FGlyphCodes.Add(TdxGlyphNames.LowerRadicalex, $203e);
  FGlyphCodes.Add(TdxGlyphNames.LowerRcaron, $159);
  FGlyphCodes.Add(TdxGlyphNames.LowerRcedilla, $157);
  FGlyphCodes.Add(TdxGlyphNames.LowerReflexsubset, $2286);
  FGlyphCodes.Add(TdxGlyphNames.LowerReflexsuperset, $2287);
  FGlyphCodes.Add(TdxGlyphNames.LowerRegistered, $ae);
  FGlyphCodes.Add(TdxGlyphNames.LowerRegistersans, $f8e8);
  FGlyphCodes.Add(TdxGlyphNames.LowerRegisterserif, $f6da);
  FGlyphCodes.Add(TdxGlyphNames.LowerRevlogicalnot, $2310);
  FGlyphCodes.Add(TdxGlyphNames.LowerRho, $3c1);
  FGlyphCodes.Add(TdxGlyphNames.LowerRing, $2da);
  FGlyphCodes.Add(TdxGlyphNames.LowerRtblock, $2590);
  FGlyphCodes.Add(TdxGlyphNames.LowerS, $73);
  FGlyphCodes.Add(TdxGlyphNames.LowerSacute, $15b);
  FGlyphCodes.Add(TdxGlyphNames.LowerScaron, $161);
  FGlyphCodes.Add(TdxGlyphNames.LowerScedilla, $15f);
  FGlyphCodes.Add(TdxGlyphNames.LowerScircumflex, $15d);
  FGlyphCodes.Add(TdxGlyphNames.LowerSecond, $2033);
  FGlyphCodes.Add(TdxGlyphNames.LowerSection, $a7);
  FGlyphCodes.Add(TdxGlyphNames.LowerSemicolon, $3b);
  FGlyphCodes.Add(TdxGlyphNames.LowerSeven, $37);
  FGlyphCodes.Add(TdxGlyphNames.LowerSeveneighths, $215e);
  FGlyphCodes.Add(TdxGlyphNames.LowerSfthyphen, $ad);
  FGlyphCodes.Add(TdxGlyphNames.LowerShade, $2592);
  FGlyphCodes.Add(TdxGlyphNames.LowerSigma, $3c3);
  FGlyphCodes.Add(TdxGlyphNames.LowerSigma1, $3c2);
  FGlyphCodes.Add(TdxGlyphNames.LowerSimilar, $223c);
  FGlyphCodes.Add(TdxGlyphNames.LowerSix, $36);
  FGlyphCodes.Add(TdxGlyphNames.LowerSlash, $2f);
  FGlyphCodes.Add(TdxGlyphNames.LowerSmileface, $263a);
  FGlyphCodes.Add(TdxGlyphNames.LowerSpace, $20);
  FGlyphCodes.Add(TdxGlyphNames.LowerSpade, $2660);
  FGlyphCodes.Add(TdxGlyphNames.LowerSterling, $a3);
  FGlyphCodes.Add(TdxGlyphNames.LowerSuchthat, $220b);
  FGlyphCodes.Add(TdxGlyphNames.LowerSummation, $2211);
  FGlyphCodes.Add(TdxGlyphNames.LowerSun, $263c);
  FGlyphCodes.Add(TdxGlyphNames.LowerT, $74);
  FGlyphCodes.Add(TdxGlyphNames.LowerTau, $3c4);
  FGlyphCodes.Add(TdxGlyphNames.LowerTbar, $167);
  FGlyphCodes.Add(TdxGlyphNames.LowerTcaron, $165);
  FGlyphCodes.Add(TdxGlyphNames.LowerTcedilla, $163);
  FGlyphCodes.Add(TdxGlyphNames.LowerTherefore, $2234);
  FGlyphCodes.Add(TdxGlyphNames.LowerTheta, $3b8);
  FGlyphCodes.Add(TdxGlyphNames.LowerTheta1, $3d1);
  FGlyphCodes.Add(TdxGlyphNames.LowerThorn, $fe);
  FGlyphCodes.Add(TdxGlyphNames.LowerThree, $33);
  FGlyphCodes.Add(TdxGlyphNames.LowerThreeeighths, $215c);
  FGlyphCodes.Add(TdxGlyphNames.LowerThreequarters, $be);
  FGlyphCodes.Add(TdxGlyphNames.LowerThreesuperior, $b3);
  FGlyphCodes.Add(TdxGlyphNames.LowerTilde, $2dc);
  FGlyphCodes.Add(TdxGlyphNames.LowerTonos, $384);
  FGlyphCodes.Add(TdxGlyphNames.LowerTrademark, $2122);
  FGlyphCodes.Add(TdxGlyphNames.LowerTrademarksans, $f8ea);
  FGlyphCodes.Add(TdxGlyphNames.LowerTrademarkserif, $f6db);
  FGlyphCodes.Add(TdxGlyphNames.LowerTriagdn, $25bc);
  FGlyphCodes.Add(TdxGlyphNames.LowerTriaglf, $25c4);
  FGlyphCodes.Add(TdxGlyphNames.LowerTriagrt, $25ba);
  FGlyphCodes.Add(TdxGlyphNames.LowerTriagup, $25b2);
  FGlyphCodes.Add(TdxGlyphNames.LowerTwo, $32);
  FGlyphCodes.Add(TdxGlyphNames.LowerTwosuperior, $b2);
  FGlyphCodes.Add(TdxGlyphNames.LowerU, $75);
  FGlyphCodes.Add(TdxGlyphNames.LowerUacute, $fa);
  FGlyphCodes.Add(TdxGlyphNames.LowerUbreve, $16d);
  FGlyphCodes.Add(TdxGlyphNames.LowerUcircumflex, $fb);
  FGlyphCodes.Add(TdxGlyphNames.LowerUdblacute, $171);
  FGlyphCodes.Add(TdxGlyphNames.LowerUdieresis, $fc);
  FGlyphCodes.Add(TdxGlyphNames.LowerUgrave, $f9);
  FGlyphCodes.Add(TdxGlyphNames.LowerUmacron, $16b);
  FGlyphCodes.Add(TdxGlyphNames.LowerUnderscore, $5f);
  FGlyphCodes.Add(TdxGlyphNames.LowerUnion, $222a);
  FGlyphCodes.Add(TdxGlyphNames.LowerUniversal, $2200);
  FGlyphCodes.Add(TdxGlyphNames.LowerUnderscoredbl, $2017);
  FGlyphCodes.Add(TdxGlyphNames.LowerUogonek, $173);
  FGlyphCodes.Add(TdxGlyphNames.LowerUpblock, $2580);
  FGlyphCodes.Add(TdxGlyphNames.LowerUpsilon, $3c5);
  FGlyphCodes.Add(TdxGlyphNames.LowerUpsilondieresis, $3cb);
  FGlyphCodes.Add(TdxGlyphNames.LowerUpsilondieresistonos, $3b0);
  FGlyphCodes.Add(TdxGlyphNames.LowerUpsilontonos, $3cd);
  FGlyphCodes.Add(TdxGlyphNames.LowerUring, $16f);
  FGlyphCodes.Add(TdxGlyphNames.LowerUtilde, $169);
  FGlyphCodes.Add(TdxGlyphNames.LowerV, $76);
  FGlyphCodes.Add(TdxGlyphNames.LowerW, $77);
  FGlyphCodes.Add(TdxGlyphNames.LowerWacute, $1e83);
  FGlyphCodes.Add(TdxGlyphNames.LowerWcircumflex, $175);
  FGlyphCodes.Add(TdxGlyphNames.LowerWdieresis, $1e85);
  FGlyphCodes.Add(TdxGlyphNames.LowerWeierstrass, $2118);
  FGlyphCodes.Add(TdxGlyphNames.LowerWgrave, $1e81);
  FGlyphCodes.Add(TdxGlyphNames.LowerX, $78);
  FGlyphCodes.Add(TdxGlyphNames.LowerXi, $3be);
  FGlyphCodes.Add(TdxGlyphNames.LowerY, $79);
  FGlyphCodes.Add(TdxGlyphNames.LowerYacute, $fd);
  FGlyphCodes.Add(TdxGlyphNames.LowerYcircumflex, $177);
  FGlyphCodes.Add(TdxGlyphNames.LowerYdieresis, $ff);
  FGlyphCodes.Add(TdxGlyphNames.LowerYen, $a5);
  FGlyphCodes.Add(TdxGlyphNames.LowerYgrave, $1ef3);
  FGlyphCodes.Add(TdxGlyphNames.LowerZ, $7a);
  FGlyphCodes.Add(TdxGlyphNames.LowerZacute, $17a);
  FGlyphCodes.Add(TdxGlyphNames.LowerZcaron, $17e);
  FGlyphCodes.Add(TdxGlyphNames.LowerZdot, $17c);
  FGlyphCodes.Add(TdxGlyphNames.LowerZero, $30);
  FGlyphCodes.Add(TdxGlyphNames.LowerZeta, $03b6);
  FGlyphCodes.TrimExcess;
end;

{ TdxFontFileMacRomanEncoding }

procedure TdxFontFileMacRomanEncoding.Initialize;
begin
  Dictionary.Add(65, TdxGlyphNames.A);
  Dictionary.Add(174, TdxGlyphNames.AE);
  Dictionary.Add(231, TdxGlyphNames.Aacute);
  Dictionary.Add(229, TdxGlyphNames.Acircumflex);
  Dictionary.Add(128, TdxGlyphNames.Adieresis);
  Dictionary.Add(203, TdxGlyphNames.Agrave);
  Dictionary.Add(129, TdxGlyphNames.Aring);
  Dictionary.Add(204, TdxGlyphNames.Atilde);
  Dictionary.Add(66, TdxGlyphNames.B);
  Dictionary.Add(67, TdxGlyphNames.C);
  Dictionary.Add(130, TdxGlyphNames.Ccedilla);
  Dictionary.Add(68, TdxGlyphNames.D);
  Dictionary.Add(69, TdxGlyphNames.E);
  Dictionary.Add(131, TdxGlyphNames.Eacute);
  Dictionary.Add(230, TdxGlyphNames.Ecircumflex);
  Dictionary.Add(232, TdxGlyphNames.Edieresis);
  Dictionary.Add(233, TdxGlyphNames.Egrave);
  Dictionary.Add(70, TdxGlyphNames.F);
  Dictionary.Add(71, TdxGlyphNames.G);
  Dictionary.Add(72, TdxGlyphNames.H);
  Dictionary.Add(73, TdxGlyphNames.I);
  Dictionary.Add(234, TdxGlyphNames.Iacute);
  Dictionary.Add(235, TdxGlyphNames.Icircumflex);
  Dictionary.Add(236, TdxGlyphNames.Idieresis);
  Dictionary.Add(237, TdxGlyphNames.Igrave);
  Dictionary.Add(74, TdxGlyphNames.J);
  Dictionary.Add(75, TdxGlyphNames.K);
  Dictionary.Add(76, TdxGlyphNames.L);
  Dictionary.Add(77, TdxGlyphNames.M);
  Dictionary.Add(78, TdxGlyphNames.N);
  Dictionary.Add(132, TdxGlyphNames.Ntilde);
  Dictionary.Add(79, TdxGlyphNames.O);
  Dictionary.Add(206, TdxGlyphNames.OE);
  Dictionary.Add(238, TdxGlyphNames.Oacute);
  Dictionary.Add(239, TdxGlyphNames.Ocircumflex);
  Dictionary.Add(133, TdxGlyphNames.Odieresis);
  Dictionary.Add(241, TdxGlyphNames.Ograve);
  Dictionary.Add(175, TdxGlyphNames.Oslash);
  Dictionary.Add(205, TdxGlyphNames.Otilde);
  Dictionary.Add(80, TdxGlyphNames.P);
  Dictionary.Add(81, TdxGlyphNames.Q);
  Dictionary.Add(82, TdxGlyphNames.R);
  Dictionary.Add(83, TdxGlyphNames.S);
  Dictionary.Add(84, TdxGlyphNames.T);
  Dictionary.Add(85, TdxGlyphNames.U);
  Dictionary.Add(242, TdxGlyphNames.Uacute);
  Dictionary.Add(243, TdxGlyphNames.Ucircumflex);
  Dictionary.Add(134, TdxGlyphNames.Udieresis);
  Dictionary.Add(244, TdxGlyphNames.Ugrave);
  Dictionary.Add(86, TdxGlyphNames.V);
  Dictionary.Add(87, TdxGlyphNames.W);
  Dictionary.Add(88, TdxGlyphNames.X);
  Dictionary.Add(89, TdxGlyphNames.Y);
  Dictionary.Add(217, TdxGlyphNames.Ydieresis);
  Dictionary.Add(90, TdxGlyphNames.Z);
  Dictionary.Add(97, TdxGlyphNames.LowerA);
  Dictionary.Add(135, TdxGlyphNames.LowerAacute);
  Dictionary.Add(137, TdxGlyphNames.LowerAcircumflex);
  Dictionary.Add(171, TdxGlyphNames.LowerAcute);
  Dictionary.Add(138, TdxGlyphNames.LowerAdieresis);
  Dictionary.Add(190, TdxGlyphNames.LowerAe);
  Dictionary.Add(136, TdxGlyphNames.LowerAgrave);
  Dictionary.Add(38, TdxGlyphNames.LowerAmpersand);
  Dictionary.Add(140, TdxGlyphNames.LowerAring);
  Dictionary.Add(94, TdxGlyphNames.LowerAsciicircum);
  Dictionary.Add(126, TdxGlyphNames.LowerAsciitilde);
  Dictionary.Add(42, TdxGlyphNames.LowerAsterisk);
  Dictionary.Add(64, TdxGlyphNames.LowerAt);
  Dictionary.Add(139, TdxGlyphNames.LowerAtilde);
  Dictionary.Add(98, TdxGlyphNames.LowerB);
  Dictionary.Add(92, TdxGlyphNames.LowerBackslash);
  Dictionary.Add(124, TdxGlyphNames.LowerBar);
  Dictionary.Add(123, TdxGlyphNames.LowerBraceleft);
  Dictionary.Add(125, TdxGlyphNames.LowerBraceright);
  Dictionary.Add(91, TdxGlyphNames.LowerBracketleft);
  Dictionary.Add(93, TdxGlyphNames.LowerBracketright);
  Dictionary.Add(249, TdxGlyphNames.LowerBreve);
  Dictionary.Add(165, TdxGlyphNames.LowerBullet);
  Dictionary.Add(99, TdxGlyphNames.LowerC);
  Dictionary.Add(255, TdxGlyphNames.LowerCaron);
  Dictionary.Add(141, TdxGlyphNames.LowerCcedilla);
  Dictionary.Add(252, TdxGlyphNames.LowerCedilla);
  Dictionary.Add(162, TdxGlyphNames.LowerCent);
  Dictionary.Add(246, TdxGlyphNames.LowerCircumflex);
  Dictionary.Add(58, TdxGlyphNames.LowerColon);
  Dictionary.Add(44, TdxGlyphNames.LowerComma);
  Dictionary.Add(169, TdxGlyphNames.LowerCopyright);
  Dictionary.Add(219, TdxGlyphNames.LowerCurrency);
  Dictionary.Add(100, TdxGlyphNames.LowerD);
  Dictionary.Add(160, TdxGlyphNames.LowerDagger);
  Dictionary.Add(224, TdxGlyphNames.LowerDaggerdbl);
  Dictionary.Add(161, TdxGlyphNames.LowerDegree);
  Dictionary.Add(172, TdxGlyphNames.LowerDieresis);
  Dictionary.Add(214, TdxGlyphNames.LowerDivide);
  Dictionary.Add(36, TdxGlyphNames.LowerDollar);
  Dictionary.Add(250, TdxGlyphNames.LowerDotaccent);
  Dictionary.Add(245, TdxGlyphNames.LowerDotlessi);
  Dictionary.Add(101, TdxGlyphNames.LowerE);
  Dictionary.Add(142, TdxGlyphNames.LowerEacute);
  Dictionary.Add(144, TdxGlyphNames.LowerEcircumflex);
  Dictionary.Add(145, TdxGlyphNames.LowerEdieresis);
  Dictionary.Add(143, TdxGlyphNames.LowerEgrave);
  Dictionary.Add(56, TdxGlyphNames.LowerEight);
  Dictionary.Add(201, TdxGlyphNames.LowerEllipsis);
  Dictionary.Add(209, TdxGlyphNames.LowerEmdash);
  Dictionary.Add(208, TdxGlyphNames.LowerEndash);
  Dictionary.Add(61, TdxGlyphNames.LowerEqual);
  Dictionary.Add(33, TdxGlyphNames.LowerExclam);
  Dictionary.Add(193, TdxGlyphNames.LowerExclamdown);
  Dictionary.Add(102, TdxGlyphNames.LowerF);
  Dictionary.Add(222, TdxGlyphNames.LowerFi);
  Dictionary.Add(53, TdxGlyphNames.LowerFive);
  Dictionary.Add(223, TdxGlyphNames.LowerFl);
  Dictionary.Add(196, TdxGlyphNames.LowerFlorin);
  Dictionary.Add(52, TdxGlyphNames.LowerFour);
  Dictionary.Add(218, TdxGlyphNames.LowerFraction);
  Dictionary.Add(103, TdxGlyphNames.LowerG);
  Dictionary.Add(167, TdxGlyphNames.LowerGermandbls);
  Dictionary.Add(96, TdxGlyphNames.LowerGrave);
  Dictionary.Add(62, TdxGlyphNames.LowerGreater);
  Dictionary.Add(199, TdxGlyphNames.LowerGuillemotleft);
  Dictionary.Add(200, TdxGlyphNames.LowerGuillemotright);
  Dictionary.Add(220, TdxGlyphNames.LowerGuilsinglleft);
  Dictionary.Add(221, TdxGlyphNames.LowerGuilsinglright);
  Dictionary.Add(104, TdxGlyphNames.LowerH);
  Dictionary.Add(253, TdxGlyphNames.LowerHungarumlaut);
  Dictionary.Add(45, TdxGlyphNames.LowerHyphen);
  Dictionary.Add(105, TdxGlyphNames.LowerI);
  Dictionary.Add(146, TdxGlyphNames.LowerIacute);
  Dictionary.Add(148, TdxGlyphNames.LowerIcircumflex);
  Dictionary.Add(149, TdxGlyphNames.LowerIdieresis);
  Dictionary.Add(147, TdxGlyphNames.LowerIgrave);
  Dictionary.Add(106, TdxGlyphNames.LowerJ);
  Dictionary.Add(107, TdxGlyphNames.LowerK);
  Dictionary.Add(108, TdxGlyphNames.LowerL);
  Dictionary.Add(60, TdxGlyphNames.LowerLess);
  Dictionary.Add(194, TdxGlyphNames.LowerLogicalnot);
  Dictionary.Add(109, TdxGlyphNames.LowerM);
  Dictionary.Add(248, TdxGlyphNames.LowerMacron);
  Dictionary.Add(181, TdxGlyphNames.LowerMu);
  Dictionary.Add(110, TdxGlyphNames.LowerN);
  Dictionary.Add(57, TdxGlyphNames.LowerNine);
  Dictionary.Add(150, TdxGlyphNames.LowerNtilde);
  Dictionary.Add(35, TdxGlyphNames.LowerNumbersign);
  Dictionary.Add(111, TdxGlyphNames.LowerO);
  Dictionary.Add(151, TdxGlyphNames.LowerOacute);
  Dictionary.Add(153, TdxGlyphNames.LowerOcircumflex);
  Dictionary.Add(154, TdxGlyphNames.LowerOdieresis);
  Dictionary.Add(207, TdxGlyphNames.LowerOe);
  Dictionary.Add(254, TdxGlyphNames.LowerOgonek);
  Dictionary.Add(152, TdxGlyphNames.LowerOgrave);
  Dictionary.Add(49, TdxGlyphNames.LowerOne);
  Dictionary.Add(187, TdxGlyphNames.LowerOrdfeminine);
  Dictionary.Add(188, TdxGlyphNames.LowerOrdmasculine);
  Dictionary.Add(191, TdxGlyphNames.LowerOslash);
  Dictionary.Add(155, TdxGlyphNames.LowerOtilde);
  Dictionary.Add(112, TdxGlyphNames.LowerP);
  Dictionary.Add(166, TdxGlyphNames.LowerParagraph);
  Dictionary.Add(40, TdxGlyphNames.LowerParenleft);
  Dictionary.Add(41, TdxGlyphNames.LowerParenright);
  Dictionary.Add(37, TdxGlyphNames.LowerPercent);
  Dictionary.Add(46, TdxGlyphNames.LowerPeriod);
  Dictionary.Add(225, TdxGlyphNames.LowerPeriodcentered);
  Dictionary.Add(228, TdxGlyphNames.LowerPerthousand);
  Dictionary.Add(43, TdxGlyphNames.LowerPlus);
  Dictionary.Add(177, TdxGlyphNames.LowerPlusminus);
  Dictionary.Add(113, TdxGlyphNames.LowerQ);
  Dictionary.Add(63, TdxGlyphNames.LowerQuestion);
  Dictionary.Add(192, TdxGlyphNames.LowerQuestiondown);
  Dictionary.Add(34, TdxGlyphNames.LowerQuotedbl);
  Dictionary.Add(227, TdxGlyphNames.LowerQuotedblbase);
  Dictionary.Add(210, TdxGlyphNames.LowerQuotedblleft);
  Dictionary.Add(211, TdxGlyphNames.LowerQuotedblright);
  Dictionary.Add(212, TdxGlyphNames.LowerQuoteleft);
  Dictionary.Add(213, TdxGlyphNames.LowerQuoteright);
  Dictionary.Add(226, TdxGlyphNames.LowerQuotesinglbase);
  Dictionary.Add(39, TdxGlyphNames.LowerQuotesingle);
  Dictionary.Add(114, TdxGlyphNames.LowerR);
  Dictionary.Add(168, TdxGlyphNames.LowerRegistered);
  Dictionary.Add(251, TdxGlyphNames.LowerRing);
  Dictionary.Add(115, TdxGlyphNames.LowerS);
  Dictionary.Add(164, TdxGlyphNames.LowerSection);
  Dictionary.Add(59, TdxGlyphNames.LowerSemicolon);
  Dictionary.Add(55, TdxGlyphNames.LowerSeven);
  Dictionary.Add(54, TdxGlyphNames.LowerSix);
  Dictionary.Add(47, TdxGlyphNames.LowerSlash);
  Dictionary.Add(32, TdxGlyphNames.LowerSpace);
  Dictionary.Add(163, TdxGlyphNames.LowerSterling);
  Dictionary.Add(116, TdxGlyphNames.LowerT);
  Dictionary.Add(51, TdxGlyphNames.LowerThree);
  Dictionary.Add(247, TdxGlyphNames.LowerTilde);
  Dictionary.Add(170, TdxGlyphNames.LowerTrademark);
  Dictionary.Add(50, TdxGlyphNames.LowerTwo);
  Dictionary.Add(117, TdxGlyphNames.LowerU);
  Dictionary.Add(156, TdxGlyphNames.LowerUacute);
  Dictionary.Add(158, TdxGlyphNames.LowerUcircumflex);
  Dictionary.Add(159, TdxGlyphNames.LowerUdieresis);
  Dictionary.Add(157, TdxGlyphNames.LowerUgrave);
  Dictionary.Add(95, TdxGlyphNames.LowerUnderscore);
  Dictionary.Add(118, TdxGlyphNames.LowerV);
  Dictionary.Add(119, TdxGlyphNames.LowerW);
  Dictionary.Add(120, TdxGlyphNames.LowerX);
  Dictionary.Add(121, TdxGlyphNames.LowerY);
  Dictionary.Add(216, TdxGlyphNames.LowerYdieresis);
  Dictionary.Add(180, TdxGlyphNames.LowerYen);
  Dictionary.Add(122, TdxGlyphNames.LowerZ);
  Dictionary.Add(48, TdxGlyphNames.LowerZero);
  Dictionary.TrimExcess;
end;

{ TdxFontFileStandardEncoding }

procedure TdxFontFileStandardEncoding.Initialize;
begin
  Dictionary.Add(32, 'space');
  Dictionary.Add(33, 'exclam');
  Dictionary.Add(34, 'quotedbl');
  Dictionary.Add(35, 'numbersign');
  Dictionary.Add(36, 'dollar');
  Dictionary.Add(37, 'percent');
  Dictionary.Add(38, 'ampersand');
  Dictionary.Add(39, 'quoteright');
  Dictionary.Add(40, 'parenleft');
  Dictionary.Add(41, 'parenright');
  Dictionary.Add(42, 'asterisk');
  Dictionary.Add(43, 'plus');
  Dictionary.Add(44, 'comma');
  Dictionary.Add(45, 'hyphen');
  Dictionary.Add(46, 'period');
  Dictionary.Add(47, 'slash');
  Dictionary.Add(48, 'zero');
  Dictionary.Add(49, 'one');
  Dictionary.Add(50, 'two');
  Dictionary.Add(51, 'three');
  Dictionary.Add(52, 'four');
  Dictionary.Add(53, 'five');
  Dictionary.Add(54, 'six');
  Dictionary.Add(55, 'seven');
  Dictionary.Add(56, 'eight');
  Dictionary.Add(57, 'nine');
  Dictionary.Add(58, 'colon');
  Dictionary.Add(59, 'semicolon');
  Dictionary.Add(60, 'less');
  Dictionary.Add(61, 'equal');
  Dictionary.Add(62, 'greater');
  Dictionary.Add(63, 'question');
  Dictionary.Add(64, 'at');
  Dictionary.Add(65, 'A');
  Dictionary.Add(66, 'B');
  Dictionary.Add(67, 'C');
  Dictionary.Add(68, 'D');
  Dictionary.Add(69, 'E');
  Dictionary.Add(70, 'F');
  Dictionary.Add(71, 'G');
  Dictionary.Add(72, 'H');
  Dictionary.Add(73, 'I');
  Dictionary.Add(74, 'J');
  Dictionary.Add(75, 'K');
  Dictionary.Add(76, 'L');
  Dictionary.Add(77, 'M');
  Dictionary.Add(78, 'N');
  Dictionary.Add(79, 'O');
  Dictionary.Add(80, 'P');
  Dictionary.Add(81, 'Q');
  Dictionary.Add(82, 'R');
  Dictionary.Add(83, 'S');
  Dictionary.Add(84, 'T');
  Dictionary.Add(85, 'U');
  Dictionary.Add(86, 'V');
  Dictionary.Add(87, 'W');
  Dictionary.Add(88, 'X');
  Dictionary.Add(89, 'Y');
  Dictionary.Add(90, 'Z');
  Dictionary.Add(91, 'bracketleft');
  Dictionary.Add(92, 'backslash');
  Dictionary.Add(93, 'bracketright');
  Dictionary.Add(94, 'asciicircum');
  Dictionary.Add(95, 'underscore');
  Dictionary.Add(96, 'quoteleft');
  Dictionary.Add(97, 'a');
  Dictionary.Add(98, 'b');
  Dictionary.Add(99, 'c');
  Dictionary.Add(100, 'd');
  Dictionary.Add(101, 'e');
  Dictionary.Add(102, 'f');
  Dictionary.Add(103, 'g');
  Dictionary.Add(104, 'h');
  Dictionary.Add(105, 'i');
  Dictionary.Add(106, 'j');
  Dictionary.Add(107, 'k');
  Dictionary.Add(108, 'l');
  Dictionary.Add(109, 'm');
  Dictionary.Add(110, 'n');
  Dictionary.Add(111, 'o');
  Dictionary.Add(112, 'p');
  Dictionary.Add(113, 'q');
  Dictionary.Add(114, 'r');
  Dictionary.Add(115, 's');
  Dictionary.Add(116, 't');
  Dictionary.Add(117, 'u');
  Dictionary.Add(118, 'v');
  Dictionary.Add(119, 'w');
  Dictionary.Add(120, 'x');
  Dictionary.Add(121, 'y');
  Dictionary.Add(122, 'z');
  Dictionary.Add(123, 'braceleft');
  Dictionary.Add(124, 'bar');
  Dictionary.Add(125, 'braceright');
  Dictionary.Add(126, 'asciitilde');
  Dictionary.Add(161, 'exclamdown');
  Dictionary.Add(162, 'cent');
  Dictionary.Add(163, 'sterling');
  Dictionary.Add(164, 'fraction');
  Dictionary.Add(165, 'yen');
  Dictionary.Add(166, 'florin');
  Dictionary.Add(167, 'section');
  Dictionary.Add(168, 'currency');
  Dictionary.Add(169, 'quotesingle');
  Dictionary.Add(170, 'quotedblleft');
  Dictionary.Add(171, 'guillemotleft');
  Dictionary.Add(172, 'guilsinglleft');
  Dictionary.Add(173, 'guilsinglright');
  Dictionary.Add(174, 'fi');
  Dictionary.Add(175, 'fl');
  Dictionary.Add(177, 'endash');
  Dictionary.Add(178, 'dagger');
  Dictionary.Add(179, 'daggerdbl');
  Dictionary.Add(180, 'periodcentered');
  Dictionary.Add(182, 'paragraph');
  Dictionary.Add(183, 'bullet');
  Dictionary.Add(184, 'quotesinglbase');
  Dictionary.Add(185, 'quotedblbase');
  Dictionary.Add(186, 'quotedblright');
  Dictionary.Add(187, 'guillemotright');
  Dictionary.Add(188, 'ellipsis');
  Dictionary.Add(189, 'perthousand');
  Dictionary.Add(191, 'questiondown');
  Dictionary.Add(193, 'grave');
  Dictionary.Add(194, 'acute');
  Dictionary.Add(195, 'circumflex');
  Dictionary.Add(196, 'tilde');
  Dictionary.Add(197, 'macron');
  Dictionary.Add(198, 'breve');
  Dictionary.Add(199, 'dotaccent');
  Dictionary.Add(200, 'dieresis');
  Dictionary.Add(202, 'ring');
  Dictionary.Add(203, 'cedilla');
  Dictionary.Add(205, 'hungarumlaut');
  Dictionary.Add(206, 'ogonek');
  Dictionary.Add(207, 'caron');
  Dictionary.Add(208, 'emdash');
  Dictionary.Add(225, 'AE');
  Dictionary.Add(227, 'ordfeminine');
  Dictionary.Add(232, 'Lslash');
  Dictionary.Add(233, 'Oslash');
  Dictionary.Add(234, 'OE');
  Dictionary.Add(235, 'ordmasculine');
  Dictionary.Add(241, 'ae');
  Dictionary.Add(245, 'dotlessi');
  Dictionary.Add(248, 'lslash');
  Dictionary.Add(249, 'oslash');
  Dictionary.Add(250, 'oe');
  Dictionary.Add(251, 'germandbls');
  Dictionary.TrimExcess;
end;

{ TdxFontFileWinAnsiEncoding }

procedure TdxFontFileWinAnsiEncoding.Initialize;
begin
  Dictionary.Add(65, TdxGlyphNames.A);
  Dictionary.Add(198, TdxGlyphNames.AE);
  Dictionary.Add(193, TdxGlyphNames.Aacute);
  Dictionary.Add(194, TdxGlyphNames.Acircumflex);
  Dictionary.Add(196, TdxGlyphNames.Adieresis);
  Dictionary.Add(192, TdxGlyphNames.Agrave);
  Dictionary.Add(197, TdxGlyphNames.Aring);
  Dictionary.Add(195, TdxGlyphNames.Atilde);
  Dictionary.Add(66, TdxGlyphNames.B);
  Dictionary.Add(67, TdxGlyphNames.C);
  Dictionary.Add(199, TdxGlyphNames.Ccedilla);
  Dictionary.Add(68, TdxGlyphNames.D);
  Dictionary.Add(69, TdxGlyphNames.E);
  Dictionary.Add(201, TdxGlyphNames.Eacute);
  Dictionary.Add(202, TdxGlyphNames.Ecircumflex);
  Dictionary.Add(203, TdxGlyphNames.Edieresis);
  Dictionary.Add(200, TdxGlyphNames.Egrave);
  Dictionary.Add(208, TdxGlyphNames.Eth);
  Dictionary.Add(128, TdxGlyphNames.Euro);
  Dictionary.Add(70, TdxGlyphNames.F);
  Dictionary.Add(71, TdxGlyphNames.G);
  Dictionary.Add(72, TdxGlyphNames.H);
  Dictionary.Add(73, TdxGlyphNames.I);
  Dictionary.Add(205, TdxGlyphNames.Iacute);
  Dictionary.Add(206, TdxGlyphNames.Icircumflex);
  Dictionary.Add(207, TdxGlyphNames.Idieresis);
  Dictionary.Add(204, TdxGlyphNames.Igrave);
  Dictionary.Add(74, TdxGlyphNames.J);
  Dictionary.Add(75, TdxGlyphNames.K);
  Dictionary.Add(76, TdxGlyphNames.L);
  Dictionary.Add(77, TdxGlyphNames.M);
  Dictionary.Add(78, TdxGlyphNames.N);
  Dictionary.Add(209, TdxGlyphNames.Ntilde);
  Dictionary.Add(79, TdxGlyphNames.O);
  Dictionary.Add(140, TdxGlyphNames.OE);
  Dictionary.Add(211, TdxGlyphNames.Oacute);
  Dictionary.Add(212, TdxGlyphNames.Ocircumflex);
  Dictionary.Add(214, TdxGlyphNames.Odieresis);
  Dictionary.Add(210, TdxGlyphNames.Ograve);
  Dictionary.Add(216, TdxGlyphNames.Oslash);
  Dictionary.Add(213, TdxGlyphNames.Otilde);
  Dictionary.Add(80, TdxGlyphNames.P);
  Dictionary.Add(81, TdxGlyphNames.Q);
  Dictionary.Add(82, TdxGlyphNames.R);
  Dictionary.Add(83, TdxGlyphNames.S);
  Dictionary.Add(138, TdxGlyphNames.Scaron);
  Dictionary.Add(84, TdxGlyphNames.T);
  Dictionary.Add(222, TdxGlyphNames.Thorn);
  Dictionary.Add(85, TdxGlyphNames.U);
  Dictionary.Add(218, TdxGlyphNames.Uacute);
  Dictionary.Add(219, TdxGlyphNames.Ucircumflex);
  Dictionary.Add(220, TdxGlyphNames.Udieresis);
  Dictionary.Add(217, TdxGlyphNames.Ugrave);
  Dictionary.Add(86, TdxGlyphNames.V);
  Dictionary.Add(87, TdxGlyphNames.W);
  Dictionary.Add(88, TdxGlyphNames.X);
  Dictionary.Add(89, TdxGlyphNames.Y);
  Dictionary.Add(221, TdxGlyphNames.Yacute);
  Dictionary.Add(159, TdxGlyphNames.Ydieresis);
  Dictionary.Add(90, TdxGlyphNames.Z);
  Dictionary.Add(142, TdxGlyphNames.Zcaron);
  Dictionary.Add(97, TdxGlyphNames.LowerA);
  Dictionary.Add(225, TdxGlyphNames.LowerAacute);
  Dictionary.Add(226, TdxGlyphNames.LowerAcircumflex);
  Dictionary.Add(180, TdxGlyphNames.LowerAcute);
  Dictionary.Add(228, TdxGlyphNames.LowerAdieresis);
  Dictionary.Add(230, TdxGlyphNames.LowerAe);
  Dictionary.Add(224, TdxGlyphNames.LowerAgrave);
  Dictionary.Add(38, TdxGlyphNames.LowerAmpersand);
  Dictionary.Add(229, TdxGlyphNames.LowerAring);
  Dictionary.Add(94, TdxGlyphNames.LowerAsciicircum);
  Dictionary.Add(126, TdxGlyphNames.LowerAsciitilde);
  Dictionary.Add(42, TdxGlyphNames.LowerAsterisk);
  Dictionary.Add(64, TdxGlyphNames.LowerAt);
  Dictionary.Add(227, TdxGlyphNames.LowerAtilde);
  Dictionary.Add(98, TdxGlyphNames.LowerB);
  Dictionary.Add(92, TdxGlyphNames.LowerBackslash);
  Dictionary.Add(124, TdxGlyphNames.LowerBar);
  Dictionary.Add(123, TdxGlyphNames.LowerBraceleft);
  Dictionary.Add(125, TdxGlyphNames.LowerBraceright);
  Dictionary.Add(91, TdxGlyphNames.LowerBracketleft);
  Dictionary.Add(93, TdxGlyphNames.LowerBracketright);
  Dictionary.Add(166, TdxGlyphNames.LowerBrokenbar);
  Dictionary.Add(127, TdxGlyphNames.LowerBullet);
  Dictionary.Add(149, TdxGlyphNames.LowerBullet);
  Dictionary.Add(99, TdxGlyphNames.LowerC);
  Dictionary.Add(231, TdxGlyphNames.LowerCcedilla);
  Dictionary.Add(184, TdxGlyphNames.LowerCedilla);
  Dictionary.Add(162, TdxGlyphNames.LowerCent);
  Dictionary.Add(136, TdxGlyphNames.LowerCircumflex);
  Dictionary.Add(58, TdxGlyphNames.LowerColon);
  Dictionary.Add(44, TdxGlyphNames.LowerComma);
  Dictionary.Add(169, TdxGlyphNames.LowerCopyright);
  Dictionary.Add(164, TdxGlyphNames.LowerCurrency);
  Dictionary.Add(100, TdxGlyphNames.LowerD);
  Dictionary.Add(134, TdxGlyphNames.LowerDagger);
  Dictionary.Add(135, TdxGlyphNames.LowerDaggerdbl);
  Dictionary.Add(176, TdxGlyphNames.LowerDegree);
  Dictionary.Add(168, TdxGlyphNames.LowerDieresis);
  Dictionary.Add(247, TdxGlyphNames.LowerDivide);
  Dictionary.Add(36, TdxGlyphNames.LowerDollar);
  Dictionary.Add(101, TdxGlyphNames.LowerE);
  Dictionary.Add(233, TdxGlyphNames.LowerEacute);
  Dictionary.Add(234, TdxGlyphNames.LowerEcircumflex);
  Dictionary.Add(235, TdxGlyphNames.LowerEdieresis);
  Dictionary.Add(232, TdxGlyphNames.LowerEgrave);
  Dictionary.Add(56, TdxGlyphNames.LowerEight);
  Dictionary.Add(133, TdxGlyphNames.LowerEllipsis);
  Dictionary.Add(151, TdxGlyphNames.LowerEmdash);
  Dictionary.Add(150, TdxGlyphNames.LowerEndash);
  Dictionary.Add(61, TdxGlyphNames.LowerEqual);
  Dictionary.Add(240, TdxGlyphNames.LowerEth);
  Dictionary.Add(33, TdxGlyphNames.LowerExclam);
  Dictionary.Add(161, TdxGlyphNames.LowerExclamdown);
  Dictionary.Add(102, TdxGlyphNames.LowerF);
  Dictionary.Add(53, TdxGlyphNames.LowerFive);
  Dictionary.Add(131, TdxGlyphNames.LowerFlorin);
  Dictionary.Add(52, TdxGlyphNames.LowerFour);
  Dictionary.Add(103, TdxGlyphNames.LowerG);
  Dictionary.Add(223, TdxGlyphNames.LowerGermandbls);
  Dictionary.Add(96, TdxGlyphNames.LowerGrave);
  Dictionary.Add(62, TdxGlyphNames.LowerGreater);
  Dictionary.Add(171, TdxGlyphNames.LowerGuillemotleft);
  Dictionary.Add(187, TdxGlyphNames.LowerGuillemotright);
  Dictionary.Add(139, TdxGlyphNames.LowerGuilsinglleft);
  Dictionary.Add(155, TdxGlyphNames.LowerGuilsinglright);
  Dictionary.Add(104, TdxGlyphNames.LowerH);
  Dictionary.Add(45, TdxGlyphNames.LowerHyphen);
  Dictionary.Add(105, TdxGlyphNames.LowerI);
  Dictionary.Add(237, TdxGlyphNames.LowerIacute);
  Dictionary.Add(238, TdxGlyphNames.LowerIcircumflex);
  Dictionary.Add(239, TdxGlyphNames.LowerIdieresis);
  Dictionary.Add(236, TdxGlyphNames.LowerIgrave);
  Dictionary.Add(106, TdxGlyphNames.LowerJ);
  Dictionary.Add(107, TdxGlyphNames.LowerK);
  Dictionary.Add(108, TdxGlyphNames.LowerL);
  Dictionary.Add(60, TdxGlyphNames.LowerLess);
  Dictionary.Add(172, TdxGlyphNames.LowerLogicalnot);
  Dictionary.Add(109, TdxGlyphNames.LowerM);
  Dictionary.Add(175, TdxGlyphNames.LowerMacron);
  Dictionary.Add(181, TdxGlyphNames.LowerMu);
  Dictionary.Add(215, TdxGlyphNames.LowerMultiply);
  Dictionary.Add(110, TdxGlyphNames.LowerN);
  Dictionary.Add(57, TdxGlyphNames.LowerNine);
  Dictionary.Add(241, TdxGlyphNames.LowerNtilde);
  Dictionary.Add(35, TdxGlyphNames.LowerNumbersign);
  Dictionary.Add(111, TdxGlyphNames.LowerO);
  Dictionary.Add(243, TdxGlyphNames.LowerOacute);
  Dictionary.Add(244, TdxGlyphNames.LowerOcircumflex);
  Dictionary.Add(246, TdxGlyphNames.LowerOdieresis);
  Dictionary.Add(156, TdxGlyphNames.LowerOe);
  Dictionary.Add(242, TdxGlyphNames.LowerOgrave);
  Dictionary.Add(49, TdxGlyphNames.LowerOne);
  Dictionary.Add(189, TdxGlyphNames.LowerOnehalf);
  Dictionary.Add(188, TdxGlyphNames.LowerOnequarter);
  Dictionary.Add(185, TdxGlyphNames.LowerOnesuperior);
  Dictionary.Add(170, TdxGlyphNames.LowerOrdfeminine);
  Dictionary.Add(186, TdxGlyphNames.LowerOrdmasculine);
  Dictionary.Add(248, TdxGlyphNames.LowerOslash);
  Dictionary.Add(245, TdxGlyphNames.LowerOtilde);
  Dictionary.Add(112, TdxGlyphNames.LowerP);
  Dictionary.Add(182, TdxGlyphNames.LowerParagraph);
  Dictionary.Add(40, TdxGlyphNames.LowerParenleft);
  Dictionary.Add(41, TdxGlyphNames.LowerParenright);
  Dictionary.Add(37, TdxGlyphNames.LowerPercent);
  Dictionary.Add(46, TdxGlyphNames.LowerPeriod);
  Dictionary.Add(183, TdxGlyphNames.LowerPeriodcentered);
  Dictionary.Add(137, TdxGlyphNames.LowerPerthousand);
  Dictionary.Add(43, TdxGlyphNames.LowerPlus);
  Dictionary.Add(177, TdxGlyphNames.LowerPlusminus);
  Dictionary.Add(113, TdxGlyphNames.LowerQ);
  Dictionary.Add(63, TdxGlyphNames.LowerQuestion);
  Dictionary.Add(191, TdxGlyphNames.LowerQuestiondown);
  Dictionary.Add(34, TdxGlyphNames.LowerQuotedbl);
  Dictionary.Add(132, TdxGlyphNames.LowerQuotedblbase);
  Dictionary.Add(147, TdxGlyphNames.LowerQuotedblleft);
  Dictionary.Add(148, TdxGlyphNames.LowerQuotedblright);
  Dictionary.Add(145, TdxGlyphNames.LowerQuoteleft);
  Dictionary.Add(146, TdxGlyphNames.LowerQuoteright);
  Dictionary.Add(130, TdxGlyphNames.LowerQuotesinglbase);
  Dictionary.Add(39, TdxGlyphNames.LowerQuotesingle);
  Dictionary.Add(114, TdxGlyphNames.LowerR);
  Dictionary.Add(174, TdxGlyphNames.LowerRegistered);
  Dictionary.Add(115, TdxGlyphNames.LowerS);
  Dictionary.Add(154, TdxGlyphNames.LowerScaron);
  Dictionary.Add(167, TdxGlyphNames.LowerSection);
  Dictionary.Add(59, TdxGlyphNames.LowerSemicolon);
  Dictionary.Add(55, TdxGlyphNames.LowerSeven);
  Dictionary.Add(54, TdxGlyphNames.LowerSix);
  Dictionary.Add(47, TdxGlyphNames.LowerSlash);
  Dictionary.Add(10, TdxGlyphNames.LowerSpace);
  Dictionary.Add(13, TdxGlyphNames.LowerSpace);
  Dictionary.Add(32, TdxGlyphNames.LowerSpace);
  Dictionary.Add(160, TdxGlyphNames.LowerSpace);
  Dictionary.Add(163, TdxGlyphNames.LowerSterling);
  Dictionary.Add(116, TdxGlyphNames.LowerT);
  Dictionary.Add(254, TdxGlyphNames.LowerThorn);
  Dictionary.Add(51, TdxGlyphNames.LowerThree);
  Dictionary.Add(190, TdxGlyphNames.LowerThreequarters);
  Dictionary.Add(179, TdxGlyphNames.LowerThreesuperior);
  Dictionary.Add(152, TdxGlyphNames.LowerTilde);
  Dictionary.Add(153, TdxGlyphNames.LowerTrademark);
  Dictionary.Add(50, TdxGlyphNames.LowerTwo);
  Dictionary.Add(178, TdxGlyphNames.LowerTwosuperior);
  Dictionary.Add(117, TdxGlyphNames.LowerU);
  Dictionary.Add(250, TdxGlyphNames.LowerUacute);
  Dictionary.Add(251, TdxGlyphNames.LowerUcircumflex);
  Dictionary.Add(252, TdxGlyphNames.LowerUdieresis);
  Dictionary.Add(249, TdxGlyphNames.LowerUgrave);
  Dictionary.Add(95, TdxGlyphNames.LowerUnderscore);
  Dictionary.Add(118, TdxGlyphNames.LowerV);
  Dictionary.Add(119, TdxGlyphNames.LowerW);
  Dictionary.Add(120, TdxGlyphNames.LowerX);
  Dictionary.Add(121, TdxGlyphNames.LowerY);
  Dictionary.Add(253, TdxGlyphNames.LowerYacute);
  Dictionary.Add(255, TdxGlyphNames.LowerYdieresis);
  Dictionary.Add(165, TdxGlyphNames.LowerYen);
  Dictionary.Add(122, TdxGlyphNames.LowerZ);
  Dictionary.Add(158, TdxGlyphNames.LowerZcaron);
  Dictionary.Add(48, TdxGlyphNames.LowerZero);
  Dictionary.TrimExcess;
end;

{ TdxFontFileSymbolEncoding }

procedure TdxFontFileSymbolEncoding.Initialize;
begin
  Dictionary.Add(65, TdxGlyphNames.Alpha);
  Dictionary.Add(66, TdxGlyphNames.Beta);
  Dictionary.Add(67, TdxGlyphNames.Chi);
  Dictionary.Add(68, TdxGlyphNames.Delta);
  Dictionary.Add(69, TdxGlyphNames.Epsilon);
  Dictionary.Add(72, TdxGlyphNames.Eta);
  Dictionary.Add(160, TdxGlyphNames.Euro);
  Dictionary.Add(71, TdxGlyphNames.Gamma);
  Dictionary.Add(193, TdxGlyphNames.Ifraktur);
  Dictionary.Add(73, TdxGlyphNames.Iota);
  Dictionary.Add(75, TdxGlyphNames.Kappa);
  Dictionary.Add(76, TdxGlyphNames.Lambda);
  Dictionary.Add(77, TdxGlyphNames.Mu);
  Dictionary.Add(78, TdxGlyphNames.Nu);
  Dictionary.Add(87, TdxGlyphNames.Omega);
  Dictionary.Add(79, TdxGlyphNames.Omicron);
  Dictionary.Add(70, TdxGlyphNames.Phi);
  Dictionary.Add(80, TdxGlyphNames.Pi);
  Dictionary.Add(89, TdxGlyphNames.Psi);
  Dictionary.Add(194, TdxGlyphNames.Rfraktur);
  Dictionary.Add(82, TdxGlyphNames.Rho);
  Dictionary.Add(83, TdxGlyphNames.Sigma);
  Dictionary.Add(84, TdxGlyphNames.Tau);
  Dictionary.Add(81, TdxGlyphNames.Theta);
  Dictionary.Add(85, TdxGlyphNames.Upsilon);
  Dictionary.Add(161, TdxGlyphNames.Upsilon1);
  Dictionary.Add(88, TdxGlyphNames.Xi);
  Dictionary.Add(90, TdxGlyphNames.Zeta);
  Dictionary.Add(192, TdxGlyphNames.LowerAleph);
  Dictionary.Add(97, TdxGlyphNames.LowerAlpha);
  Dictionary.Add(38, TdxGlyphNames.LowerAmpersand);
  Dictionary.Add(208, TdxGlyphNames.LowerAngle);
  Dictionary.Add(225, TdxGlyphNames.LowerAngleleft);
  Dictionary.Add(241, TdxGlyphNames.LowerAngleright);
  Dictionary.Add(187, TdxGlyphNames.LowerApproxequal);
  Dictionary.Add(171, TdxGlyphNames.LowerArrowboth);
  Dictionary.Add(219, TdxGlyphNames.LowerArrowdblboth);
  Dictionary.Add(223, TdxGlyphNames.LowerArrowdbldown);
  Dictionary.Add(220, TdxGlyphNames.LowerArrowdblleft);
  Dictionary.Add(222, TdxGlyphNames.LowerArrowdblright);
  Dictionary.Add(221, TdxGlyphNames.LowerArrowdblup);
  Dictionary.Add(175, TdxGlyphNames.LowerArrowdown);
  Dictionary.Add(190, TdxGlyphNames.LowerArrowhorizex);
  Dictionary.Add(172, TdxGlyphNames.LowerArrowleft);
  Dictionary.Add(174, TdxGlyphNames.LowerArrowright);
  Dictionary.Add(173, TdxGlyphNames.LowerArrowup);
  Dictionary.Add(189, TdxGlyphNames.LowerArrowvertex);
  Dictionary.Add(42, TdxGlyphNames.LowerAsteriskmath);
  Dictionary.Add(124, TdxGlyphNames.LowerBar);
  Dictionary.Add(98, TdxGlyphNames.LowerBeta);
  Dictionary.Add(123, TdxGlyphNames.LowerBraceleft);
  Dictionary.Add(125, TdxGlyphNames.LowerBraceright);
  Dictionary.Add(236, TdxGlyphNames.LowerBracelefttp);
  Dictionary.Add(237, TdxGlyphNames.LowerBraceleftmid);
  Dictionary.Add(238, TdxGlyphNames.LowerBraceleftbt);
  Dictionary.Add(252, TdxGlyphNames.LowerBracerighttp);
  Dictionary.Add(253, TdxGlyphNames.LowerBracerightmid);
  Dictionary.Add(254, TdxGlyphNames.LowerBracerightbt);
  Dictionary.Add(239, TdxGlyphNames.LowerBraceex);
  Dictionary.Add(91, TdxGlyphNames.LowerBracketleft);
  Dictionary.Add(93, TdxGlyphNames.LowerBracketright);
  Dictionary.Add(233, TdxGlyphNames.LowerBracketlefttp);
  Dictionary.Add(234, TdxGlyphNames.LowerBracketleftex);
  Dictionary.Add(235, TdxGlyphNames.LowerBracketleftbt);
  Dictionary.Add(249, TdxGlyphNames.LowerBracketrighttp);
  Dictionary.Add(250, TdxGlyphNames.LowerBracketrightex);
  Dictionary.Add(251, TdxGlyphNames.LowerBracketrightbt);
  Dictionary.Add(183, TdxGlyphNames.LowerBullet);
  Dictionary.Add(191, TdxGlyphNames.LowerCarriagereturn);
  Dictionary.Add(99, TdxGlyphNames.LowerChi);
  Dictionary.Add(196, TdxGlyphNames.LowerCirclemultiply);
  Dictionary.Add(197, TdxGlyphNames.LowerCircleplus);
  Dictionary.Add(167, TdxGlyphNames.LowerClub);
  Dictionary.Add(58, TdxGlyphNames.LowerColon);
  Dictionary.Add(44, TdxGlyphNames.LowerComma);
  Dictionary.Add(64, TdxGlyphNames.LowerCongruent);
  Dictionary.Add(227, TdxGlyphNames.LowerCopyrightsans);
  Dictionary.Add(211, TdxGlyphNames.LowerCopyrightserif);
  Dictionary.Add(176, TdxGlyphNames.LowerDegree);
  Dictionary.Add(100, TdxGlyphNames.LowerDelta);
  Dictionary.Add(168, TdxGlyphNames.LowerDiamond);
  Dictionary.Add(184, TdxGlyphNames.LowerDivide);
  Dictionary.Add(215, TdxGlyphNames.LowerDotmath);
  Dictionary.Add(56, TdxGlyphNames.LowerEight);
  Dictionary.Add(206, TdxGlyphNames.LowerElement);
  Dictionary.Add(188, TdxGlyphNames.LowerEllipsis);
  Dictionary.Add(198, TdxGlyphNames.LowerEmptyset);
  Dictionary.Add(101, TdxGlyphNames.LowerEpsilon);
  Dictionary.Add(61, TdxGlyphNames.LowerEqual);
  Dictionary.Add(186, TdxGlyphNames.LowerEquivalence);
  Dictionary.Add(104, TdxGlyphNames.LowerEta);
  Dictionary.Add(33, TdxGlyphNames.LowerExclam);
  Dictionary.Add(36, TdxGlyphNames.LowerExistential);
  Dictionary.Add(53, TdxGlyphNames.LowerFive);
  Dictionary.Add(166, TdxGlyphNames.LowerFlorin);
  Dictionary.Add(52, TdxGlyphNames.LowerFour);
  Dictionary.Add(164, TdxGlyphNames.LowerFraction);
  Dictionary.Add(103, TdxGlyphNames.LowerGamma);
  Dictionary.Add(209, TdxGlyphNames.LowerGradient);
  Dictionary.Add(62, TdxGlyphNames.LowerGreater);
  Dictionary.Add(179, TdxGlyphNames.LowerGreaterequal);
  Dictionary.Add(169, TdxGlyphNames.LowerHeart);
  Dictionary.Add(165, TdxGlyphNames.LowerInfinity);
  Dictionary.Add(242, TdxGlyphNames.LowerIntegral);
  Dictionary.Add(243, TdxGlyphNames.LowerIntegraltp);
  Dictionary.Add(244, TdxGlyphNames.LowerIntegralex);
  Dictionary.Add(245, TdxGlyphNames.LowerIntegralbt);
  Dictionary.Add(199, TdxGlyphNames.LowerIntersection);
  Dictionary.Add(105, TdxGlyphNames.LowerIota);
  Dictionary.Add(107, TdxGlyphNames.LowerKappa);
  Dictionary.Add(108, TdxGlyphNames.LowerLambda);
  Dictionary.Add(60, TdxGlyphNames.LowerLess);
  Dictionary.Add(163, TdxGlyphNames.LowerLessequal);
  Dictionary.Add(217, TdxGlyphNames.LowerLogicaland);
  Dictionary.Add(216, TdxGlyphNames.LowerLogicalnot);
  Dictionary.Add(218, TdxGlyphNames.LowerLogicalor);
  Dictionary.Add(224, TdxGlyphNames.LowerLozenge);
  Dictionary.Add(45, TdxGlyphNames.LowerMinus);
  Dictionary.Add(162, TdxGlyphNames.LowerMinute);
  Dictionary.Add(109, TdxGlyphNames.LowerMu);
  Dictionary.Add(180, TdxGlyphNames.LowerMultiply);
  Dictionary.Add(57, TdxGlyphNames.LowerNine);
  Dictionary.Add(207, TdxGlyphNames.LowerNotelement);
  Dictionary.Add(185, TdxGlyphNames.LowerNotequal);
  Dictionary.Add(203, TdxGlyphNames.LowerNotsubset);
  Dictionary.Add(110, TdxGlyphNames.LowerNu);
  Dictionary.Add(35, TdxGlyphNames.LowerNumbersign);
  Dictionary.Add(119, TdxGlyphNames.LowerOmega);
  Dictionary.Add(118, TdxGlyphNames.LowerOmega1);
  Dictionary.Add(111, TdxGlyphNames.LowerOmicron);
  Dictionary.Add(49, TdxGlyphNames.LowerOne);
  Dictionary.Add(40, TdxGlyphNames.LowerParenleft);
  Dictionary.Add(41, TdxGlyphNames.LowerParenright);
  Dictionary.Add(230, TdxGlyphNames.LowerParenlefttp);
  Dictionary.Add(231, TdxGlyphNames.LowerParenleftex);
  Dictionary.Add(232, TdxGlyphNames.LowerParenleftbt);
  Dictionary.Add(246, TdxGlyphNames.LowerParenrighttp);
  Dictionary.Add(247, TdxGlyphNames.LowerParenrightex);
  Dictionary.Add(248, TdxGlyphNames.LowerParenrightbt);
  Dictionary.Add(182, TdxGlyphNames.LowerPartialdiff);
  Dictionary.Add(37, TdxGlyphNames.LowerPercent);
  Dictionary.Add(46, TdxGlyphNames.LowerPeriod);
  Dictionary.Add(94, TdxGlyphNames.LowerPerpendicular);
  Dictionary.Add(102, TdxGlyphNames.LowerPhi);
  Dictionary.Add(106, TdxGlyphNames.LowerPhi1);
  Dictionary.Add(112, TdxGlyphNames.LowerPi);
  Dictionary.Add(43, TdxGlyphNames.LowerPlus);
  Dictionary.Add(177, TdxGlyphNames.LowerPlusminus);
  Dictionary.Add(213, TdxGlyphNames.LowerProduct);
  Dictionary.Add(204, TdxGlyphNames.LowerPropersubset);
  Dictionary.Add(201, TdxGlyphNames.LowerPropersuperset);
  Dictionary.Add(181, TdxGlyphNames.LowerProportional);
  Dictionary.Add(121, TdxGlyphNames.LowerPsi);
  Dictionary.Add(63, TdxGlyphNames.LowerQuestion);
  Dictionary.Add(214, TdxGlyphNames.LowerRadical);
  Dictionary.Add(96, TdxGlyphNames.LowerRadicalex);
  Dictionary.Add(205, TdxGlyphNames.LowerReflexsubset);
  Dictionary.Add(202, TdxGlyphNames.LowerReflexsuperset);
  Dictionary.Add(226, TdxGlyphNames.LowerRegistersans);
  Dictionary.Add(210, TdxGlyphNames.LowerRegisterserif);
  Dictionary.Add(114, TdxGlyphNames.LowerRho);
  Dictionary.Add(178, TdxGlyphNames.LowerSecond);
  Dictionary.Add(59, TdxGlyphNames.LowerSemicolon);
  Dictionary.Add(55, TdxGlyphNames.LowerSeven);
  Dictionary.Add(115, TdxGlyphNames.LowerSigma);
  Dictionary.Add(86, TdxGlyphNames.LowerSigma1);
  Dictionary.Add(126, TdxGlyphNames.LowerSimilar);
  Dictionary.Add(54, TdxGlyphNames.LowerSix);
  Dictionary.Add(47, TdxGlyphNames.LowerSlash);
  Dictionary.Add(32, TdxGlyphNames.LowerSpace);
  Dictionary.Add(170, TdxGlyphNames.LowerSpade);
  Dictionary.Add(39, TdxGlyphNames.LowerSuchthat);
  Dictionary.Add(229, TdxGlyphNames.LowerSummation);
  Dictionary.Add(116, TdxGlyphNames.LowerTau);
  Dictionary.Add(92, TdxGlyphNames.LowerTherefore);
  Dictionary.Add(113, TdxGlyphNames.LowerTheta);
  Dictionary.Add(74, TdxGlyphNames.LowerTheta1);
  Dictionary.Add(51, TdxGlyphNames.LowerThree);
  Dictionary.Add(228, TdxGlyphNames.LowerTrademarksans);
  Dictionary.Add(212, TdxGlyphNames.LowerTrademarkserif);
  Dictionary.Add(50, TdxGlyphNames.LowerTwo);
  Dictionary.Add(95, TdxGlyphNames.LowerUnderscore);
  Dictionary.Add(200, TdxGlyphNames.LowerUnion);
  Dictionary.Add(34, TdxGlyphNames.LowerUniversal);
  Dictionary.Add(117, TdxGlyphNames.LowerUpsilon);
  Dictionary.Add(195, TdxGlyphNames.LowerWeierstrass);
  Dictionary.Add(120, TdxGlyphNames.LowerXi);
  Dictionary.Add(48, TdxGlyphNames.LowerZero);
  Dictionary.Add(122, TdxGlyphNames.LowerZeta);
  Dictionary.TrimExcess;
end;

{ TdxFontFileZapfDingbatsEncoding }

procedure TdxFontFileZapfDingbatsEncoding.Initialize;
begin
  Dictionary.Add(32, TdxGlyphNames.LowerSpace);
  Dictionary.Add(33, TdxGlyphNames.LowerA1);
  Dictionary.Add(34, TdxGlyphNames.LowerA2);
  Dictionary.Add(35, TdxGlyphNames.LowerA202);
  Dictionary.Add(36, TdxGlyphNames.LowerA3);
  Dictionary.Add(37, TdxGlyphNames.LowerA4);
  Dictionary.Add(38, TdxGlyphNames.LowerA5);
  Dictionary.Add(39, TdxGlyphNames.LowerA119);
  Dictionary.Add(40, TdxGlyphNames.LowerA118);
  Dictionary.Add(41, TdxGlyphNames.LowerA117);
  Dictionary.Add(42, TdxGlyphNames.LowerA11);
  Dictionary.Add(43, TdxGlyphNames.LowerA12);
  Dictionary.Add(44, TdxGlyphNames.LowerA13);
  Dictionary.Add(45, TdxGlyphNames.LowerA14);
  Dictionary.Add(46, TdxGlyphNames.LowerA15);
  Dictionary.Add(47, TdxGlyphNames.LowerA16);
  Dictionary.Add(48, TdxGlyphNames.LowerA105);
  Dictionary.Add(49, TdxGlyphNames.LowerA17);
  Dictionary.Add(50, TdxGlyphNames.LowerA18);
  Dictionary.Add(51, TdxGlyphNames.LowerA19);
  Dictionary.Add(52, TdxGlyphNames.LowerA20);
  Dictionary.Add(53, TdxGlyphNames.LowerA21);
  Dictionary.Add(54, TdxGlyphNames.LowerA22);
  Dictionary.Add(55, TdxGlyphNames.LowerA23);
  Dictionary.Add(56, TdxGlyphNames.LowerA24);
  Dictionary.Add(57, TdxGlyphNames.LowerA25);
  Dictionary.Add(58, TdxGlyphNames.LowerA26);
  Dictionary.Add(59, TdxGlyphNames.LowerA27);
  Dictionary.Add(60, TdxGlyphNames.LowerA28);
  Dictionary.Add(61, TdxGlyphNames.LowerA6);
  Dictionary.Add(62, TdxGlyphNames.LowerA7);
  Dictionary.Add(63, TdxGlyphNames.LowerA8);
  Dictionary.Add(64, TdxGlyphNames.LowerA9);
  Dictionary.Add(65, TdxGlyphNames.LowerA10);
  Dictionary.Add(66, TdxGlyphNames.LowerA29);
  Dictionary.Add(67, TdxGlyphNames.LowerA30);
  Dictionary.Add(68, TdxGlyphNames.LowerA31);
  Dictionary.Add(69, TdxGlyphNames.LowerA32);
  Dictionary.Add(70, TdxGlyphNames.LowerA33);
  Dictionary.Add(71, TdxGlyphNames.LowerA34);
  Dictionary.Add(72, TdxGlyphNames.LowerA35);
  Dictionary.Add(73, TdxGlyphNames.LowerA36);
  Dictionary.Add(74, TdxGlyphNames.LowerA37);
  Dictionary.Add(75, TdxGlyphNames.LowerA38);
  Dictionary.Add(76, TdxGlyphNames.LowerA39);
  Dictionary.Add(77, TdxGlyphNames.LowerA40);
  Dictionary.Add(78, TdxGlyphNames.LowerA41);
  Dictionary.Add(79, TdxGlyphNames.LowerA42);
  Dictionary.Add(80, TdxGlyphNames.LowerA43);
  Dictionary.Add(81, TdxGlyphNames.LowerA44);
  Dictionary.Add(82, TdxGlyphNames.LowerA45);
  Dictionary.Add(83, TdxGlyphNames.LowerA46);
  Dictionary.Add(84, TdxGlyphNames.LowerA47);
  Dictionary.Add(85, TdxGlyphNames.LowerA48);
  Dictionary.Add(86, TdxGlyphNames.LowerA49);
  Dictionary.Add(87, TdxGlyphNames.LowerA50);
  Dictionary.Add(88, TdxGlyphNames.LowerA51);
  Dictionary.Add(89, TdxGlyphNames.LowerA52);
  Dictionary.Add(90, TdxGlyphNames.LowerA53);
  Dictionary.Add(91, TdxGlyphNames.LowerA54);
  Dictionary.Add(92, TdxGlyphNames.LowerA55);
  Dictionary.Add(93, TdxGlyphNames.LowerA56);
  Dictionary.Add(94, TdxGlyphNames.LowerA57);
  Dictionary.Add(95, TdxGlyphNames.LowerA58);
  Dictionary.Add(96, TdxGlyphNames.LowerA59);
  Dictionary.Add(97, TdxGlyphNames.LowerA60);
  Dictionary.Add(98, TdxGlyphNames.LowerA61);
  Dictionary.Add(99, TdxGlyphNames.LowerA62);
  Dictionary.Add(100, TdxGlyphNames.LowerA63);
  Dictionary.Add(101, TdxGlyphNames.LowerA64);
  Dictionary.Add(102, TdxGlyphNames.LowerA65);
  Dictionary.Add(103, TdxGlyphNames.LowerA66);
  Dictionary.Add(104, TdxGlyphNames.LowerA67);
  Dictionary.Add(105, TdxGlyphNames.LowerA68);
  Dictionary.Add(106, TdxGlyphNames.LowerA69);
  Dictionary.Add(107, TdxGlyphNames.LowerA70);
  Dictionary.Add(108, TdxGlyphNames.LowerA71);
  Dictionary.Add(109, TdxGlyphNames.LowerA72);
  Dictionary.Add(110, TdxGlyphNames.LowerA73);
  Dictionary.Add(111, TdxGlyphNames.LowerA74);
  Dictionary.Add(112, TdxGlyphNames.LowerA203);
  Dictionary.Add(113, TdxGlyphNames.LowerA75);
  Dictionary.Add(114, TdxGlyphNames.LowerA204);
  Dictionary.Add(115, TdxGlyphNames.LowerA76);
  Dictionary.Add(116, TdxGlyphNames.LowerA77);
  Dictionary.Add(117, TdxGlyphNames.LowerA78);
  Dictionary.Add(118, TdxGlyphNames.LowerA79);
  Dictionary.Add(119, TdxGlyphNames.LowerA81);
  Dictionary.Add(120, TdxGlyphNames.LowerA82);
  Dictionary.Add(121, TdxGlyphNames.LowerA83);
  Dictionary.Add(122, TdxGlyphNames.LowerA84);
  Dictionary.Add(123, TdxGlyphNames.LowerA97);
  Dictionary.Add(124, TdxGlyphNames.LowerA98);
  Dictionary.Add(125, TdxGlyphNames.LowerA99);
  Dictionary.Add(126, TdxGlyphNames.LowerA100);
  Dictionary.Add(161, TdxGlyphNames.LowerA101);
  Dictionary.Add(162, TdxGlyphNames.LowerA102);
  Dictionary.Add(163, TdxGlyphNames.LowerA103);
  Dictionary.Add(164, TdxGlyphNames.LowerA104);
  Dictionary.Add(165, TdxGlyphNames.LowerA106);
  Dictionary.Add(166, TdxGlyphNames.LowerA107);
  Dictionary.Add(167, TdxGlyphNames.LowerA108);
  Dictionary.Add(168, TdxGlyphNames.LowerA112);
  Dictionary.Add(169, TdxGlyphNames.LowerA111);
  Dictionary.Add(170, TdxGlyphNames.LowerA110);
  Dictionary.Add(171, TdxGlyphNames.LowerA109);
  Dictionary.Add(172, TdxGlyphNames.LowerA120);
  Dictionary.Add(173, TdxGlyphNames.LowerA121);
  Dictionary.Add(174, TdxGlyphNames.LowerA122);
  Dictionary.Add(175, TdxGlyphNames.LowerA123);
  Dictionary.Add(176, TdxGlyphNames.LowerA124);
  Dictionary.Add(177, TdxGlyphNames.LowerA125);
  Dictionary.Add(178, TdxGlyphNames.LowerA126);
  Dictionary.Add(179, TdxGlyphNames.LowerA127);
  Dictionary.Add(180, TdxGlyphNames.LowerA128);
  Dictionary.Add(181, TdxGlyphNames.LowerA129);
  Dictionary.Add(182, TdxGlyphNames.LowerA130);
  Dictionary.Add(183, TdxGlyphNames.LowerA131);
  Dictionary.Add(184, TdxGlyphNames.LowerA132);
  Dictionary.Add(185, TdxGlyphNames.LowerA133);
  Dictionary.Add(186, TdxGlyphNames.LowerA134);
  Dictionary.Add(187, TdxGlyphNames.LowerA135);
  Dictionary.Add(188, TdxGlyphNames.LowerA136);
  Dictionary.Add(189, TdxGlyphNames.LowerA137);
  Dictionary.Add(190, TdxGlyphNames.LowerA138);
  Dictionary.Add(191, TdxGlyphNames.LowerA139);
  Dictionary.Add(192, TdxGlyphNames.LowerA140);
  Dictionary.Add(193, TdxGlyphNames.LowerA141);
  Dictionary.Add(194, TdxGlyphNames.LowerA142);
  Dictionary.Add(195, TdxGlyphNames.LowerA143);
  Dictionary.Add(196, TdxGlyphNames.LowerA144);
  Dictionary.Add(197, TdxGlyphNames.LowerA145);
  Dictionary.Add(198, TdxGlyphNames.LowerA146);
  Dictionary.Add(199, TdxGlyphNames.LowerA147);
  Dictionary.Add(200, TdxGlyphNames.LowerA148);
  Dictionary.Add(201, TdxGlyphNames.LowerA149);
  Dictionary.Add(202, TdxGlyphNames.LowerA150);
  Dictionary.Add(203, TdxGlyphNames.LowerA151);
  Dictionary.Add(204, TdxGlyphNames.LowerA152);
  Dictionary.Add(205, TdxGlyphNames.LowerA153);
  Dictionary.Add(206, TdxGlyphNames.LowerA154);
  Dictionary.Add(207, TdxGlyphNames.LowerA155);
  Dictionary.Add(208, TdxGlyphNames.LowerA156);
  Dictionary.Add(209, TdxGlyphNames.LowerA157);
  Dictionary.Add(210, TdxGlyphNames.LowerA158);
  Dictionary.Add(211, TdxGlyphNames.LowerA159);
  Dictionary.Add(212, TdxGlyphNames.LowerA160);
  Dictionary.Add(213, TdxGlyphNames.LowerA161);
  Dictionary.Add(214, TdxGlyphNames.LowerA163);
  Dictionary.Add(215, TdxGlyphNames.LowerA164);
  Dictionary.Add(216, TdxGlyphNames.LowerA196);
  Dictionary.Add(217, TdxGlyphNames.LowerA165);
  Dictionary.Add(218, TdxGlyphNames.LowerA192);
  Dictionary.Add(219, TdxGlyphNames.LowerA166);
  Dictionary.Add(220, TdxGlyphNames.LowerA167);
  Dictionary.Add(221, TdxGlyphNames.LowerA168);
  Dictionary.Add(222, TdxGlyphNames.LowerA169);
  Dictionary.Add(223, TdxGlyphNames.LowerA170);
  Dictionary.Add(224, TdxGlyphNames.LowerA171);
  Dictionary.Add(225, TdxGlyphNames.LowerA172);
  Dictionary.Add(226, TdxGlyphNames.LowerA173);
  Dictionary.Add(227, TdxGlyphNames.LowerA162);
  Dictionary.Add(228, TdxGlyphNames.LowerA174);
  Dictionary.Add(229, TdxGlyphNames.LowerA175);
  Dictionary.Add(230, TdxGlyphNames.LowerA176);
  Dictionary.Add(231, TdxGlyphNames.LowerA177);
  Dictionary.Add(232, TdxGlyphNames.LowerA178);
  Dictionary.Add(233, TdxGlyphNames.LowerA179);
  Dictionary.Add(234, TdxGlyphNames.LowerA193);
  Dictionary.Add(235, TdxGlyphNames.LowerA180);
  Dictionary.Add(236, TdxGlyphNames.LowerA199);
  Dictionary.Add(237, TdxGlyphNames.LowerA181);
  Dictionary.Add(238, TdxGlyphNames.LowerA200);
  Dictionary.Add(239, TdxGlyphNames.LowerA182);
  Dictionary.Add(240, TdxGlyphNames.LowerA201);
  Dictionary.Add(241, TdxGlyphNames.LowerA183);
  Dictionary.Add(242, TdxGlyphNames.LowerA184);
  Dictionary.Add(243, TdxGlyphNames.LowerA197);
  Dictionary.Add(244, TdxGlyphNames.LowerA185);
  Dictionary.Add(245, TdxGlyphNames.LowerA194);
  Dictionary.Add(246, TdxGlyphNames.LowerA198);
  Dictionary.Add(247, TdxGlyphNames.LowerA186);
  Dictionary.Add(249, TdxGlyphNames.LowerA195);
  Dictionary.Add(250, TdxGlyphNames.LowerA187);
  Dictionary.Add(251, TdxGlyphNames.LowerA188);
  Dictionary.Add(252, TdxGlyphNames.LowerA189);
  Dictionary.Add(253, TdxGlyphNames.LowerA190);
  Dictionary.Add(254, TdxGlyphNames.LowerA191);
  Dictionary.TrimExcess;
end;


procedure TdxFontFileCMapSegmentMappingRecord.Write(ATableStream: TdxFontFileStream);
var
  ADoubleSegCount, ASearchRange: SmallInt;
begin
  inherited Write(ATableStream);
  ADoubleSegCount := SmallInt((FSegCount * 2));
  ATableStream.WriteShort(ADoubleSegCount);
  ASearchRange := SmallInt(Trunc(2 * Power(2, Floor(Log2(FSegCount)))));
  ATableStream.WriteShort(ASearchRange);
  ATableStream.WriteShort(SmallInt(Floor(Log2(FSegCount))));
  ATableStream.WriteShort(SmallInt((ADoubleSegCount - ASearchRange)));
  ATableStream.WriteShortArray(FEndCode);
  ATableStream.WriteShort(0);
  ATableStream.WriteShortArray(FStartCode);
  ATableStream.WriteShortArray(FIdDelta);
  ATableStream.WriteShortArray(FIdRangeOffset);
  ATableStream.WriteShortArray(FGlyphIdArray);
end;

{ TdxFontFileCMapLongRecord }

constructor TdxFontFileCMapLongRecord.Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream);
begin
  inherited Create(APlatformId, AEncodingId);
  AStream.ReadShort;
  AStream.ReadInt;
  Language := AStream.ReadInt;
end;

procedure TdxFontFileCMapLongRecord.Write(ATableStream: TdxFontFileStream);
begin
  inherited Write(ATableStream);
  ATableStream.WriteShort(0);
  ATableStream.WriteInt(Size);
  ATableStream.WriteInt(Language);
end;

{ TdxFontFileCMapGroup }

class function TdxFontFileCMapGroup.Create(AStartCharCode, AEndCharCode, AGlyphID: Integer): TdxFontFileCMapGroup;
begin
  Result.FStartCharCode := AStartCharCode;
  Result.FEndCharCode := AEndCharCode;
  Result.FGlyphID := AGlyphID;
end;

class function TdxFontFileCMapGroup.ReadGroups(AStream: TdxFontFileStream; AGroupsCount: Integer): TArray<TdxFontFileCMapGroup>;
var
  I, AStartCharCode, AEndCharCode, AGlyphID: Integer;
begin
  SetLength(Result, AGroupsCount);
  for I := 0 to AGroupsCount - 1 do
  begin
    AStartCharCode := AStream.ReadInt;
    AEndCharCode := AStream.ReadInt;
    AGlyphID := AStream.ReadInt;
    Result[I] := TdxFontFileCMapGroup.Create(AStartCharCode, AEndCharCode, AGlyphID);
  end;
end;

class procedure TdxFontFileCMapGroup.WriteGroups(const AGroups: TArray<TdxFontFileCMapGroup>; ATableStream: TdxFontFileStream);
var
  AGroup: TdxFontFileCMapGroup;
begin
  for AGroup in AGroups do
  begin
    ATableStream.WriteInt(AGroup.startCharCode);
    ATableStream.WriteInt(AGroup.endCharCode);
    ATableStream.WriteInt(AGroup.glyphID);
  end;
end;


constructor TdxFontFileCMapSegmentMappingRecord.CreateFromSegmentMapping(AEncodingID: TdxFontFileEncodingID;
  AFormatEntry: TdxFontFileCMapSegmentMappingRecord);
begin
  inherited Create(TdxFontFilePlatformID.Microsoft, AEncodingID, AFormatEntry.Language);
  FSegCount := AFormatEntry.SegCount;
  FEndCode := AFormatEntry.EndCode;
  FStartCode := AFormatEntry.StartCode;
  FIdDelta := AFormatEntry.IdDelta;
  FIdRangeOffset := AFormatEntry.IdRangeOffset;
  FGlyphIdArray := AFormatEntry.GlyphIdArray;
end;

constructor TdxFontFileCMapSegmentMappingRecord.CreateFromByteEncoding(AEncodingID: TdxFontFileEncodingID;
  AFormatEntry: TdxFontFileCMapByteEncodingRecord);
var
  AGlyphIndexes: TList<TEncodingPair>;
  AGlyphInfo: TEncodingPair;

  AGlyphArray: TBytes;
  I, AGlyphIndex, AGlyphCode: SmallInt;
  AGlyphName: string;
  AActualGlyphCode: Word;
  ALength, AIndex: Integer;
  APair: TEncodingPair;

  AComparison: TComparison<TEncodingPair>;
  AMacRomanEncoding: TdxFontFileMacRomanEncoding;
begin
  inherited Create(TdxFontFilePlatformID.Microsoft, AEncodingID, AFormatEntry.Language);
  AGlyphArray := AFormatEntry.GlyphIdArray;

  AGlyphIndexes := TList<TEncodingPair>.Create;

  if AEncodingID = TdxFontFileEncodingID.Undefined then
    for I := 0 to Length(AGlyphArray) - 1 do
    begin
      AGlyphIndex := AGlyphArray[I];
      if AGlyphIndex <> 0 then
      begin
        AGlyphInfo.Count := SmallInt(UndefinedEncodingMicrosoftOffset + I);
        AGlyphInfo.Code := AGlyphIndex;
        AGlyphIndexes.Add(AGlyphInfo);
      end;
    end
  else
    if AFormatEntry.PlatformId = TdxFontFilePlatformID.Macintosh then
    begin
      AMacRomanEncoding := TdxFontFileMacRomanEncoding.Create;
      try
        for I := 0 to Length(AGlyphArray) - 1 do
        begin
          AGlyphIndex := AGlyphArray[I];
          if AGlyphIndex <> 0 then
          begin
            if AMacRomanEncoding.Dictionary.TryGetValue(Byte(I), AGlyphName) and
              dxFontFileUnicodeConverter.FindCode(AGlyphName, AActualGlyphCode) then
            begin
              AGlyphInfo.Count := SmallInt(AActualGlyphCode);
              AGlyphInfo.Code := AGlyphIndex;
              AGlyphIndexes.Add(AGlyphInfo);
            end;
          end;
        end;
      finally
        AMacRomanEncoding.Free;
      end;
    end
    else
      for I := 0 to Length(AGlyphArray) - 1 do
      begin
        AGlyphIndex := AGlyphArray[I];
        if AGlyphIndex <> 0 then
        begin
          AGlyphInfo.Count := I;
          AGlyphInfo.Code := AGlyphIndex;
          AGlyphIndexes.Add(AGlyphInfo);
        end;
      end;
  ALength := AGlyphIndexes.Count;
  FSegCount := ALength + 1;
  SetLength(FStartCode, FSegCount);
  SetLength(FEndCode, FSegCount);
  SetLength(FIdDelta, FSegCount);
  SetLength(FIdRangeOffset, FSegCount);
  AIndex := 0;

  AComparison :=
    function(const Left, Right: TEncodingPair): Integer
    begin
      Result := Left.Count-Right.Count;
    end;

  AGlyphIndexes.Sort(TComparer<TEncodingPair>.Construct(AComparison));


  for APair in AGlyphIndexes do
  begin
    AGlyphCode := APair.Count;
    FStartCode[AIndex] := AGlyphCode;
    FEndCode[AIndex] := AGlyphCode;
    FIdDelta[AIndex] := SmallInt((APair.Code - AGlyphCode));
    Inc(AIndex);
  end;
  AGlyphIndexes.Free;
  FStartCode[ALength] := FinalCode;
  FEndCode[ALength] := FinalCode;
  FIdDelta[ALength] := FinalDelta;
  SetLength(FGlyphIdArray, 0);
end;

constructor TdxFontFileCMapSegmentMappingRecord.CreateFromTrimmedMapping(AEncodingID: TdxFontFileEncodingID;
  AFormatEntry: TdxFontFileCMapTrimmedMappingRecord);
var
  AFirstCode: SmallInt;
begin
  inherited Create(TdxFontFilePlatformID.Microsoft, AEncodingID, AFormatEntry.Language);
  FSegCount := 2;
  AFirstCode := AFormatEntry.FirstCode;
  if (AEncodingID = TdxFontFileEncodingID.Undefined) and (AFirstCode + AFormatEntry.EntryCount < 4096) then
    AFirstCode := SmallInt(AFirstCode + UndefinedEncodingMicrosoftOffset);

  SetLength(FEndCode, 2);
  FEndCode[0] := SmallInt(AFirstCode + AFormatEntry.EntryCount - 1);
  FEndCode[1] := FinalCode;

  SetLength(FStartCode, 2);
  FStartCode[0] := AFirstCode;
  FStartCode[1] := FinalCode;

  SetLength(FIdDelta, 2);
  FIdDelta[0] := 0;
  FIdDelta[1] := FinalDelta;

  SetLength(FIdRangeOffset, 2);
  FIdRangeOffset[0] := 4;
  FIdRangeOffset[1] := 0;

  FGlyphIdArray := AFormatEntry.GlyphIdArray;
end;

constructor TdxFontFileCMapSegmentMappingRecord.CreateDefault(AEncodingID: TdxFontFileEncodingID);
begin
  Create(TdxFontFilePlatformID.Microsoft, AEncodingID, 0);
  FSegCount := 2;
  SetLength(FEndCode, 2);
  FEndCode[0] := 0;
  FEndCode[1] := FinalCode;

  SetLength(FStartCode, 2);
  FStartCode[0] := 0;
  FStartCode[1] := FinalCode;

  SetLength(FIdDelta, 2);
  FIdDelta[0] := 0;
  FIdDelta[1] := FinalDelta;

  SetLength(FIdRangeOffset, 2);
  FIdRangeOffset[0] := 4;
  FIdRangeOffset[1] := 0;

  SetLength(FGlyphIdArray, 1);
  FGlyphIdArray[0] := 0
end;

destructor TdxFontFileCMapSegmentMappingRecord.Destroy;
begin
  FreeAndNil(FGlyphRanges);
  inherited Destroy;
end;

function TdxFontFileCMapSegmentMappingRecord.GetSegmentsLength: Integer;
begin
  Result := 10 + FSegCount * 8;
end;

function TdxFontFileCMapSegmentMappingRecord.IsUndefinedEncoding: Boolean;
begin
  Result := (PlatformId = TdxFontFilePlatformID.Microsoft) and (EncodingId = TdxFontFileEncodingID.Undefined);
end;

function TdxFontFileCMapSegmentMappingRecord.GetFormat: TdxFontFileCMapFormatID;
begin
  Result := TdxFontFileCMapFormatID.SegmentMapping;
end;

function TdxFontFileCMapSegmentMappingRecord.GetSize: Integer;
begin
  Result := HeaderLength + SegmentsLength + Length(FGlyphIdArray) * 2;
end;

procedure TdxFontFileCMapSegmentMappingRecord.UpdateEncoding(var AEncoding: TSmallIntDynArray);
var
  AIsUndefinedEncoding: Boolean;
  AGlyphIdArrayLength, I, AIndex, APosition: Integer;
  AStart, AEnd, AOffset: Word;
  ADelta, AGlyphIndex: SmallInt;
begin
  AIsUndefinedEncoding := IsUndefinedEncoding;
  AGlyphIdArrayLength := Length(FGlyphIdArray);
  for I := 0 to FSegCount - 1 do
  begin
    AStart := FStartCode[I];
    ADelta := FIdDelta[I];
    AEnd := FEndCode[I];
    if AStart >= 256 then
    begin
      if AIsUndefinedEncoding and (AStart >= UndefinedEncodingMicrosoftOffset) then
      begin
        Dec(AStart, UndefinedEncodingMicrosoftOffset);
        Dec(AEnd, UndefinedEncodingMicrosoftOffset);
        ADelta := ADelta + UndefinedEncodingMicrosoftOffset;
      end;
      if AStart > 256 then
        Break;
    end;
    if AEnd >= 256 then
      AEnd := 255;
    AOffset := FIdRangeOffset[I];
    if AOffset = 0 then
    begin
      AGlyphIndex := AStart + ADelta;
      for AIndex := AStart to AEnd do
      begin
        UpdateEncodingValue(AEncoding, AIndex, AGlyphIndex);
        Inc(AGlyphIndex);
      end;
    end
    else
    begin
      APosition := AOffset div 2 - FSegCount + I;
      for AIndex := AStart to AEnd do
      begin
        if (APosition >= 0) and (APosition < AGlyphIdArrayLength) then
          UpdateEncodingValue(AEncoding, AIndex, FGlyphIdArray[APosition]);
        Inc(APosition);
      end;
    end;
  end;
end;

function TdxFontFileCMapSegmentMappingRecord.MapCode(ACharacter: Char): Integer;
var
  AGlyphCount, AIdRangeCount, I, APosition: Integer;
  AStart, AOffset: SmallInt;
  AGlyphIndex: Word;
begin
  AGlyphCount := Length(FGlyphIdArray);
  AIdRangeCount := Length(FIdRangeOffset);
  for I := 0 to FSegCount - 1 do
  begin
    AStart := FStartCode[I];
    if (FEndCode[I] >= SmallInt(ACharacter)) and (AStart <= SmallInt(ACharacter)) then
    begin
      AOffset := FIdRangeOffset[I];
      if AOffset <> 0 then
      begin
        APosition := FIdRangeOffset[I] div 2 + SmallInt(ACharacter) - AStart + I - AIdRangeCount;
        if APosition < AGlyphCount then
        begin
          AGlyphIndex := Word(FGlyphIdArray[APosition]);
          if AGlyphIndex = MissingGlyphIndex then
            Exit(AGlyphIndex)
          else
            Exit((AGlyphIndex + FIdDelta[I]) mod 65536);
        end;
      end
      else
        if SmallInt(ACharacter) = MissingGlyphIndex then
          Exit(SmallInt(ACharacter))
        else
          Exit((SmallInt(ACharacter) + FIdDelta[I]) mod 65536);
    end;
  end;
  Result := MissingGlyphIndex;
end;

function TdxFontFileCMapSegmentMappingRecord.CreateGlyphMapping(AGlyphNames: TStringList): TdxPDFWordDictionary;
var
  AGlyphNamesCount, AGlyphIdCount, I, AOffset, AStartIndex, AIdIndex: Integer;
  AStart, AEnd, ARangeOffset, ACode, AGlyphId: SmallInt;
begin
  Result := TdxPDFWordDictionary.Create;
  if (AGlyphNames = nil) or (AGlyphNames.Count = 0) then
    Exit;
  AGlyphNamesCount := AGlyphNames.Count;
  AGlyphIdCount := Length(FGlyphIdArray);

  AOffset := FSegCount;
  for I := 0 to FSegCount - 1 do
  begin
    AStart := FStartCode[I];
    AEnd := FEndCode[I];
    if AStart <> -1 then
    begin
      ARangeOffset := FIdRangeOffset[I];
      if ARangeOffset > 0 then
      begin
        AStartIndex := ARangeOffset div 2 - AStart - AOffset;
        AIdIndex := AStart + AStartIndex;

        ACode := AStart;
        while (ACode <= AEnd) and (AIdIndex < AGlyphIdCount) do
        begin
          AGlyphId := FGlyphIdArray[AIdIndex];
          if (AGlyphId < 0) or (AGlyphId >= AGlyphNamesCount) then
            Exit(nil);
          Result.AddOrSetValue(AGlyphNames[AGlyphId], Word(ACode));
          Inc(ACode);
          Inc(AIdIndex);
        end;
      end
      else
      begin
        AGlyphId := AStart + FIdDelta[I];

        ACode := AStart;
        while ACode <= AEnd do
        begin
          if AGlyphId >= AGlyphNamesCount then
          begin
            Result.Free;
            Exit(nil);
          end;
          Result.AddOrSetValue(AGlyphNames[AGlyphId], ACode);
          Inc(ACode);
          Inc(AGlyphId);
        end;
      end;
    end;
    Dec(AOffset);
  end;
end;

function TdxFontFileCMapSegmentMappingRecord.GetGlyphRanges: TList<TdxFontFileCMapGlyphRange>;
var
  I: Integer;
  AEnd: SmallInt;
  EA5697E9668D44678735E263F6731488: TdxFontFileCMapGlyphRange;
begin
  if FGlyphRanges = nil then
  begin
    FGlyphRanges := TList<TdxFontFileCMapGlyphRange>.Create;
    for I := 0 to FSegCount - 1 do
    begin
      AEnd := FEndCode[I];
      if AEnd = -1 then
        Break;
      EA5697E9668D44678735E263F6731488.StartValue := FStartCode[I];
      EA5697E9668D44678735E263F6731488.EndValue := AEnd;
      FGlyphRanges.Add(EA5697E9668D44678735E263F6731488);
    end;
  end;
  Result := FGlyphRanges;
end;

function TdxFontFileCMapSegmentMappingRecord.Validate: Boolean;
var
  AComparison: TComparison<TdxFontFileCMapRow>;

  AMaxIndex, AIndex, I: Integer;
  APrevious, AValue: Word;
  AList: TList<TdxFontFileCMapRow>;
  ARow: TdxFontFileCMapRow;
begin
  if FSegCount <= 0 then
    Exit(False);
  AMaxIndex := FSegCount - 1;
  APrevious := Word(FEndCode[0]);

  AIndex := 1;
  while AIndex < AMaxIndex do
  begin
    AValue := Word(FEndCode[AIndex]);
    if AValue < APrevious then
    begin
      AList := TList<TdxFontFileCMapRow>.Create;
      try
        AList.Capacity := AMaxIndex;
        for I := 0 to AMaxIndex - 1 do
        begin
          ARow.EndCode := FEndCode[I];
          ARow.StartCode := FStartCode[I];
          ARow.IdDelta := FIdDelta[I];
          ARow.IdRangeOffset := FIdRangeOffset[I];
          AList.Add(ARow);
        end;
        AComparison :=
          function(const ALeft, ARigth: TdxFontFileCMapRow): Integer
          var
            AResult: Integer;
          begin
          AResult := Word(ALeft.EndCode) - Word(ARigth.EndCode);
          if AResult = 0 then
          begin
            AResult := Word(ALeft.StartCode) - Word(ARigth.StartCode);
            if AResult = 0 then
              AResult := Word(ALeft.IdDelta) - Word(ARigth.IdDelta);
          end;
          Result := AResult;
        end;

        AList.Sort(TComparer<TdxFontFileCMapRow>.Construct(AComparison));
        for I := 0 to AMaxIndex - 1 do
        begin
          ARow := AList[I];
          FEndCode[I] := ARow.EndCode;
          FStartCode[I] := ARow.StartCode;
          FIdDelta[I] := ARow.IdDelta;
          FIdRangeOffset[I] := ARow.IdRangeOffset;
        end;
      finally
        AList.Free;
      end;
      Exit(True);
    end;
    APrevious := AValue;
    Inc(AIndex);
  end;
  Result := False;
end;

function TdxFontFileCMapSegmentMappingRecord.ReadSegmentsArray(ACMapStream: TdxFontFileStream): TSmallIntDynArray;
var
  AResult: TSmallIntDynArray;
  I: Integer;
begin
  SetLength(AResult, FSegCount);
  for I := 0 to FSegCount - 1 do
    AResult[I] := ACMapStream.ReadShort;
  Result := AResult;
end;

class function TdxFontFileCMapSegmentMappingRecord.TMap.Create(ACharCode: Char; AGID: SmallInt): TMap;
begin
  Result.CharCode := ACharCode;
  Result.GID := AGID;
end;


{ TdxFontFileCMapRangeMappingRecord }

constructor TdxFontFileCMapRangeMappingRecord.Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream);
begin
  inherited Create(APlatformId, AEncodingId, AStream);
  FGroups := TdxFontFileCMapGroup.ReadGroups(AStream, AStream.ReadInt);
end;

procedure TdxFontFileCMapRangeMappingRecord.Write(ATableStream: TdxFontFileStream);
begin
  inherited Write(ATableStream);
  ATableStream.WriteInt(Length(FGroups));
  TdxFontFileCMapGroup.WriteGroups(FGroups, ATableStream);
end;

function TdxFontFileCMapRangeMappingRecord.GetSize: Integer;
begin
  Result := HeaderLength + Length(FGroups) * 12;
end;

{ TdxFontFileCMapSegmentedCoverageRecord }

function TdxFontFileCMapSegmentedCoverageRecord.GetFormat: TdxFontFileCMapFormatID;
begin
  Result := TdxFontFileCMapFormatID.SegmentedCoverage;
end;

{ TdxFontFileCMapManyToOneRangeMappingRecord }

function TdxFontFileCMapManyToOneRangeMappingRecord.GetFormat: TdxFontFileCMapFormatID;
begin
  Result := TdxFontFileCMapFormatID.ManyToOneRangeMapping;
end;

{ TdxFontFileCMapMixedCoverageRecord }

constructor TdxFontFileCMapMixedCoverageRecord.Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream);
begin
  inherited Create(APlatformId, AEncodingId, AStream);
  FIs32 := AStream.ReadArray(8192);
  FGroups := TdxFontFileCMapGroup.ReadGroups(AStream, AStream.ReadInt);
end;

procedure TdxFontFileCMapMixedCoverageRecord.Write(ATableStream: TdxFontFileStream);
begin
  inherited Write(ATableStream);
  ATableStream.WriteArray(FIs32);
  ATableStream.WriteInt(Length(FGroups));
  TdxFontFileCMapGroup.WriteGroups(FGroups, ATableStream);
end;

function TdxFontFileCMapMixedCoverageRecord.GetFormat: TdxFontFileCMapFormatID;
begin
  Result := TdxFontFileCMapFormatID.MixedCoverage;
end;

function TdxFontFileCMapMixedCoverageRecord.GetSize: Integer;
begin
  Result := HeaderLength + Length(FGroups) * 12;
end;

{ TdxFontFileCMapTrimmedArrayRecord }

constructor TdxFontFileCMapTrimmedArrayRecord.Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream);
begin
  inherited Create(APlatformId, AEncodingId, AStream);
  FStartCharacterCode := AStream.ReadInt;
  FCharacterCount := AStream.ReadInt;
  FGlyphs := AStream.ReadShortArray(FCharacterCount);
end;

procedure TdxFontFileCMapTrimmedArrayRecord.Write(ATableStream: TdxFontFileStream);
begin
  inherited Write(ATableStream);
  ATableStream.WriteInt(FStartCharacterCode);
  ATableStream.WriteInt(FCharacterCount);
  ATableStream.WriteShortArray(FGlyphs);
end;

function TdxFontFileCMapTrimmedArrayRecord.GetFormat: TdxFontFileCMapFormatID;
begin
  Result := TdxFontFileCMapFormatID.TrimmedArray;
end;

function TdxFontFileCMapTrimmedArrayRecord.GetSize: Integer;
begin
  Result := HeaderLength + Length(FGlyphs) * 2;
end;

{ TdxDefaultUVSTable }

constructor TdxDefaultUVSTable.Create(AStartUnicodeValue: Integer; AAdditionalCount: Byte);
begin
  inherited Create;
  FStartUnicodeValue := AStartUnicodeValue;
  FAdditionalCount := AAdditionalCount;
end;

procedure TdxDefaultUVSTable.Write(ATableStream: TdxFontFileStream);
var
  AStartUnicodeValueBytes: TBytes;
begin
  SetLength(AStartUnicodeValueBytes, 3);
  AStartUnicodeValueBytes[2] := Byte((FStartUnicodeValue and $FF));
  AStartUnicodeValueBytes[1] := Byte(((FStartUnicodeValue and $FF00) shr 8));
  AStartUnicodeValueBytes[0] := Byte(((FStartUnicodeValue and $FF0000) shr 16));
  ATableStream.WriteArray(AStartUnicodeValueBytes);
  ATableStream.WriteByte(FAdditionalCount);
end;

{ TdxNonDefaultUVSTable }

constructor TdxNonDefaultUVSTable.Create(AUnicodeValue: Integer; AGlyphId: SmallInt);
begin
  inherited Create;
  FUnicodeValue := AUnicodeValue;
  FGlyphId := AGlyphId;
end;

procedure TdxNonDefaultUVSTable.Write(ATableStream: TdxFontFileStream);
var
  AUnicodeValueBytes: TBytes;
begin
  SetLength(AUnicodeValueBytes, 3);
  AUnicodeValueBytes[2] := Byte((FUnicodeValue and $FF));
  AUnicodeValueBytes[1] := Byte(((FUnicodeValue and $FF00) shr 8));
  AUnicodeValueBytes[0] := Byte(((FUnicodeValue and $FF0000) shr 16));
  ATableStream.WriteArray(AUnicodeValueBytes);
  ATableStream.WriteShort(FGlyphId);
end;

{ TdxFontFileCMapUnicodeVariationSelectorRecord }

constructor TdxFontFileCMapUnicodeVariationSelectorRecord.Create(AVarSelector: Integer;
  const ADefaultUVSTables: TArray<TdxDefaultUVSTable>; const ANonDefaultUVSTables: TArray<TdxNonDefaultUVSTable>);
begin
  inherited Create;
  FVarSelector := AVarSelector;
  FDefaultUVSTables := ADefaultUVSTables;
  FNonDefaultUVSTables := ANonDefaultUVSTables;
end;

destructor TdxFontFileCMapUnicodeVariationSelectorRecord.Destroy;

  procedure DestroyDefaultUVSTables;
  var
    I: Integer;
  begin
    for I := 0 to Length(FDefaultUVSTables) - 1 do
      FDefaultUVSTables[I].Free;
  end;

  procedure DestroyNonDefaultUVSTables;
  var
    I: Integer;
  begin
    for I := 0 to Length(FNonDefaultUVSTables) - 1 do
      FNonDefaultUVSTables[I].Free;
  end;

begin
  DestroyDefaultUVSTables;
  DestroyNonDefaultUVSTables;
  inherited Destroy;
end;


{ TdxFontFileCMapByteEncodingRecord }

constructor TdxFontFileCMapByteEncodingRecord.Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream);
begin
  inherited Create(APlatformId, AEncodingId, AStream);
  FGlyphIdArray := AStream.ReadArray(BodyLength);
end;

function TdxFontFileCMapByteEncodingRecord.MapCode(ACharacter: Char): Integer;
begin
  if (Integer(ACharacter) > 255) or (Integer(ACharacter) >= Length(FGlyphIdArray)) then
    Result := MissingGlyphIndex
  else
    Result := SmallInt(FGlyphIdArray[Integer(ACharacter)]);
end;

procedure TdxFontFileCMapByteEncodingRecord.Write(ATableStream: TdxFontFileStream);
begin
  inherited Write(ATableStream);
  ATableStream.WriteArray(FGlyphIdArray);
end;

function TdxFontFileCMapByteEncodingRecord.GetFormat: TdxFontFileCMapFormatID;
begin
  Result := TdxFontFileCMapFormatID.ByteEncoding;
end;

procedure TdxFontFileCMapByteEncodingRecord.UpdateEncoding(var AEncoding: TSmallIntDynArray);
var
  ALength, I: Integer;
begin
  ALength := Math.Min(Length(AEncoding), Length(FGlyphIdArray));
  for I := 0 to ALength - 1 do
    UpdateEncodingValue(AEncoding, I, FGlyphIdArray[I]);
end;

{ TdxFontFileCMapHighByteMappingThroughSubHeader }

constructor TdxFontFileCMapHighByteMappingThroughSubHeader.Create(AFirstCode, AEntryCount, AIdDelta, AIdRangeOffset: SmallInt;
  AGlyphOffset: Integer);
begin
  inherited Create;
  FFirstCode := AFirstCode;
  FEntryCount := AEntryCount;
  FIdDelta := AIdDelta;
  FIdRangeOffset := AIdRangeOffset;
  FGlyphOffset := AGlyphOffset;
end;

function TdxFontFileCMapHighByteMappingThroughSubHeader.CalcGlyphIndexArraySize(AOffset: Integer): Integer;
begin
  Result := (FIdRangeOffset + FEntryCount * 2 - AOffset) div 2;
end;

{ TdxFontFileCMapHighByteMappingThroughRecord }

constructor TdxFontFileCMapHighByteMappingThroughRecord.Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream);
var
  ASubHeaderIndexes: TDictionary<Integer, Integer>;
  AKey, ASubHeaderCount, AEndOfSubheadersPosition, AOffset, AGlyphIndexArrayCount, I: Integer;
  AFirstSubheader, ASubHeader: TdxFontFileCMapHighByteMappingThroughSubHeader;
begin
  inherited Create(APlatformId, AEncodingId, AStream);

  FSubHeaderKeys := AStream.ReadShortArray(256);
  ASubHeaderIndexes := TDictionary<Integer, Integer>.Create;
  try
    for AKey in FSubHeaderKeys do
      if not ASubHeaderIndexes.ContainsKey(AKey) then
        ASubHeaderIndexes.Add(AKey, AKey);
    ASubHeaderCount := ASubHeaderIndexes.Count;
  finally
    ASubHeaderIndexes.Free;
  end;

  SetLength(FSubHeaders, ASubHeaderCount);
  AEndOfSubheadersPosition := Integer(AStream.Position) + ASubHeaderCount * 8;
  AOffset := ASubHeaderCount * 8 - 6;
  AFirstSubheader := ReadSubHeader(AStream, AEndOfSubheadersPosition);
  AGlyphIndexArrayCount := AFirstSubheader.CalcGlyphIndexArraySize(AOffset);
  FSubHeaders[0] := AFirstSubheader;
  Dec(AOffset, 8);
  for I  := 1 to ASubHeaderCount - 1 do
  begin
    ASubHeader := ReadSubHeader(AStream, AEndOfSubheadersPosition);
    FSubHeaders[I] := ASubHeader;
    AGlyphIndexArrayCount := Max(AGlyphIndexArrayCount, ASubHeader.CalcGlyphIndexArraySize(AOffset));
    Dec(AOffset, 8);
  end;
  FGlyphIndexArray := AStream.ReadShortArray(AGlyphIndexArrayCount);
end;

destructor TdxFontFileCMapHighByteMappingThroughRecord.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FSubHeaders) - 1 do
    FSubHeaders[I].Free;
  inherited Destroy;
end;

function TdxFontFileCMapHighByteMappingThroughRecord.GetFormat: TdxFontFileCMapFormatID;
begin
  Result := TdxFontFileCMapFormatID.HighByteMappingThrough;
end;

function TdxFontFileCMapHighByteMappingThroughRecord.GetSize: Integer;
begin
  Result := HeaderLength + SubHeaderKeysLength + SubHeaderLength * Length(FSubHeaders) + Length(FGlyphIndexArray) * 2;
end;

procedure TdxFontFileCMapHighByteMappingThroughRecord.UpdateEncoding(var AEncoding: TSmallIntDynArray);
var
  AIndex: SmallInt;
begin
  for AIndex := 0 to 256 - 1 do
    if FSubHeaderKeys[AIndex] = 0 then
      UpdateEncodingValue(AEncoding, AIndex, FGlyphIndexArray[AIndex]);
end;

function TdxFontFileCMapHighByteMappingThroughRecord.ReadSubHeader(AStream: TdxFontFileStream;
  AEndOfSubheadersPosition: Integer): TdxFontFileCMapHighByteMappingThroughSubHeader;
var
  AFirstCode, AEntryCount, AIdDelta, AIdRangeOffset: SmallInt;
  APos: Integer;
begin
  AFirstCode := AStream.ReadShort;
  AEntryCount := AStream.ReadShort;
  AIdDelta := AStream.ReadShort;
  APos := Integer(AStream.Position);
  AIdRangeOffset := AStream.ReadShort;
  Result := TdxFontFileCMapHighByteMappingThroughSubHeader.Create(AFirstCode, AEntryCount, AIdDelta, AIdRangeOffset, (AIdRangeOffset - (AEndOfSubheadersPosition - APos)) div 2);
end;

procedure TdxFontFileCMapHighByteMappingThroughRecord.Write(ATableStream: TdxFontFileStream);
var
  ASubHeader: TdxFontFileCMapHighByteMappingThroughSubHeader;
begin
  inherited Write(ATableStream);
  ATableStream.WriteShortArray(FSubHeaderKeys);
  for ASubHeader in FSubHeaders do
  begin
    ATableStream.WriteShort(ASubHeader.FirstCode);
    ATableStream.WriteShort(ASubHeader.EntryCount);
    ATableStream.WriteShort(ASubHeader.IdDelta);
    ATableStream.WriteShort(ASubHeader.IdRangeOffset);
  end;
  ATableStream.WriteShortArray(FGlyphIndexArray);
end;

function TdxFontFileCMapHighByteMappingThroughRecord.MapCode(ACharacter: Char): Integer;
var
  AHigh, ALow: Byte;
  ASubheaderIndex, AIndex, P: Integer;
  AGlyph: Word;
  ASubHeader: TdxFontFileCMapHighByteMappingThroughSubHeader;
  AFirstCode: SmallInt;
begin
  AHigh := Byte(ACharacter) shr 8;
  ALow := Byte(ACharacter) and $FF;

  ASubheaderIndex := FSubHeaderKeys[AHigh] div 8;
  if ASubheaderIndex = 0 then
  begin
    AGlyph := Word(FGlyphIndexArray[ALow]);
    if ((FSubHeaders <> nil) and (Length(FSubHeaders) > 0)) and (AGlyph <> MissingGlyphIndex) then
      Exit((AGlyph + FSubHeaders[0].IdDelta) mod 65536)
    else
      Exit(AGlyph);
  end;
  if ASubheaderIndex > Length(FSubHeaders) then
    Exit(MissingGlyphIndex);
  ASubHeader := FSubHeaders[ASubheaderIndex];
  AFirstCode := ASubHeader.FirstCode;
  if (AFirstCode > ALow) or (ALow >= AFirstCode + ASubHeader.EntryCount) then
    Exit(MissingGlyphIndex);
  AIndex := ASubHeader.GlyphOffset + ALow;
  if AIndex > Length(FGlyphIndexArray) then
    Exit(MissingGlyphIndex);
  P := Word(FGlyphIndexArray[AIndex]);
  if P <> MissingGlyphIndex then
    Inc(P, ASubHeader.IdDelta);
  Result := P mod 65536;
end;

{ TdxFontFileCMapSegmentMappingRecord }

constructor TdxFontFileCMapSegmentMappingRecord.Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream);
var
  I, ASeg, AGlyphIdArrayLength: Integer;
begin
  inherited Create(APlatformId, AEncodingId, AStream);
  FSegCount := AStream.ReadShort div 2;
  AStream.ReadShort;
  AStream.ReadShort;
  AStream.ReadShort;
  FEndCode := ReadSegmentsArray(AStream);
  AStream.ReadShort;
  FStartCode := ReadSegmentsArray(AStream);
  FIdDelta := ReadSegmentsArray(AStream);
  FIdRangeOffset := ReadSegmentsArray(AStream);
  AGlyphIdArrayLength := (BodyLength - SegmentsLength) div 2;
  if AGlyphIdArrayLength < 0 then
    AGlyphIdArrayLength := 0;
  SetLength(FGlyphIdArray, AGlyphIdArrayLength);
  for I := 0 to AGlyphIdArrayLength - 1 do
    FGlyphIdArray[I] := AStream.ReadShort;
  SetLength(FSegmentOffsets, FSegCount);
  ASeg := 1;
  for I := 0 to FSegCount - 1 do
  begin
    FSegmentOffsets[I] := (FIdRangeOffset[I] - (FSegCount - ASeg) - 2) div 2;
    Inc(ASeg);
  end;
end;

constructor TdxFontFileCMapSegmentMappingRecord.CreateFromCharset(ACharset: TDictionary<SmallInt, SmallInt>);
var
  I, ALength, AIndex: Integer;
  AComparison: TComparison<TMap>;
  ACharCode: Char;
  APair: TMap;
  AGID: SmallInt;
  AEncodingUnitKey: Char;
  AEncodingUnitValue: Byte;
  AStandardEncodingUnicodeToSID: TDictionary<Char, Byte>;
  AMapping: TList<TMap>;
begin
  inherited Create(TdxFontFilePlatformID.Microsoft, TdxFontFileEncodingID.UGL, 0);

  AMapping := TList<TMap>.Create;
  AStandardEncodingUnicodeToSID := TDictionary<Char, Byte>.Create;
  try
    AStandardEncodingUnicodeToSID.Add(Char(32), 1);
    AStandardEncodingUnicodeToSID.Add(Char(33), 2);
    AStandardEncodingUnicodeToSID.Add(Char(34), 3);
    AStandardEncodingUnicodeToSID.Add(Char(35), 4);
    AStandardEncodingUnicodeToSID.Add(Char(36), 5);
    AStandardEncodingUnicodeToSID.Add(Char(37), 6);
    AStandardEncodingUnicodeToSID.Add(Char(38), 7);
    AStandardEncodingUnicodeToSID.Add(Char(8217), 8);
    AStandardEncodingUnicodeToSID.Add(Char(40), 9);
    AStandardEncodingUnicodeToSID.Add(Char(41), 10);
    AStandardEncodingUnicodeToSID.Add(Char(42), 11);
    AStandardEncodingUnicodeToSID.Add(Char(43), 12);
    AStandardEncodingUnicodeToSID.Add(Char(44), 13);
    AStandardEncodingUnicodeToSID.Add(Char(45), 14);
    AStandardEncodingUnicodeToSID.Add(Char(46), 15);
    AStandardEncodingUnicodeToSID.Add(Char(47), 16);
    AStandardEncodingUnicodeToSID.Add(Char(48), 17);
    AStandardEncodingUnicodeToSID.Add(Char(49), 18);
    AStandardEncodingUnicodeToSID.Add(Char(50), 19);
    AStandardEncodingUnicodeToSID.Add(Char(51), 20);
    AStandardEncodingUnicodeToSID.Add(Char(52), 21);
    AStandardEncodingUnicodeToSID.Add(Char(53), 22);
    AStandardEncodingUnicodeToSID.Add(Char(54), 23);
    AStandardEncodingUnicodeToSID.Add(Char(55), 24);
    AStandardEncodingUnicodeToSID.Add(Char(56), 25);
    AStandardEncodingUnicodeToSID.Add(Char(57), 26);
    AStandardEncodingUnicodeToSID.Add(Char(58), 27);
    AStandardEncodingUnicodeToSID.Add(Char(59), 28);
    AStandardEncodingUnicodeToSID.Add(Char(60), 29);
    AStandardEncodingUnicodeToSID.Add(Char(61), 30);
    AStandardEncodingUnicodeToSID.Add(Char(62), 31);
    AStandardEncodingUnicodeToSID.Add(Char(63), 32);
    AStandardEncodingUnicodeToSID.Add(Char(64), 33);
    AStandardEncodingUnicodeToSID.Add(Char(65), 34);
    AStandardEncodingUnicodeToSID.Add(Char(66), 35);
    AStandardEncodingUnicodeToSID.Add(Char(67), 36);
    AStandardEncodingUnicodeToSID.Add(Char(68), 37);
    AStandardEncodingUnicodeToSID.Add(Char(69), 38);
    AStandardEncodingUnicodeToSID.Add(Char(70), 39);
    AStandardEncodingUnicodeToSID.Add(Char(71), 40);
    AStandardEncodingUnicodeToSID.Add(Char(72), 41);
    AStandardEncodingUnicodeToSID.Add(Char(73), 42);
    AStandardEncodingUnicodeToSID.Add(Char(74), 43);
    AStandardEncodingUnicodeToSID.Add(Char(75), 44);
    AStandardEncodingUnicodeToSID.Add(Char(76), 45);
    AStandardEncodingUnicodeToSID.Add(Char(77), 46);
    AStandardEncodingUnicodeToSID.Add(Char(78), 47);
    AStandardEncodingUnicodeToSID.Add(Char(79), 48);
    AStandardEncodingUnicodeToSID.Add(Char(80), 49);
    AStandardEncodingUnicodeToSID.Add(Char(81), 50);
    AStandardEncodingUnicodeToSID.Add(Char(82), 51);
    AStandardEncodingUnicodeToSID.Add(Char(83), 52);
    AStandardEncodingUnicodeToSID.Add(Char(84), 53);
    AStandardEncodingUnicodeToSID.Add(Char(85), 54);
    AStandardEncodingUnicodeToSID.Add(Char(86), 55);
    AStandardEncodingUnicodeToSID.Add(Char(87), 56);
    AStandardEncodingUnicodeToSID.Add(Char(88), 57);
    AStandardEncodingUnicodeToSID.Add(Char(89), 58);
    AStandardEncodingUnicodeToSID.Add(Char(90), 59);
    AStandardEncodingUnicodeToSID.Add(Char(91), 60);
    AStandardEncodingUnicodeToSID.Add(Char(92), 61);
    AStandardEncodingUnicodeToSID.Add(Char(93), 62);
    AStandardEncodingUnicodeToSID.Add(Char(94), 63);
    AStandardEncodingUnicodeToSID.Add(Char(95), 64);
    AStandardEncodingUnicodeToSID.Add(Char(8216), 65);
    AStandardEncodingUnicodeToSID.Add(Char(97), 66);
    AStandardEncodingUnicodeToSID.Add(Char(98), 67);
    AStandardEncodingUnicodeToSID.Add(Char(99), 68);
    AStandardEncodingUnicodeToSID.Add(Char(100), 69);
    AStandardEncodingUnicodeToSID.Add(Char(101), 70);
    AStandardEncodingUnicodeToSID.Add(Char(102), 71);
    AStandardEncodingUnicodeToSID.Add(Char(103), 72);
    AStandardEncodingUnicodeToSID.Add(Char(104), 73);
    AStandardEncodingUnicodeToSID.Add(Char(105), 74);
    AStandardEncodingUnicodeToSID.Add(Char(106), 75);
    AStandardEncodingUnicodeToSID.Add(Char(107), 76);
    AStandardEncodingUnicodeToSID.Add(Char(108), 77);
    AStandardEncodingUnicodeToSID.Add(Char(109), 78);
    AStandardEncodingUnicodeToSID.Add(Char(110), 79);
    AStandardEncodingUnicodeToSID.Add(Char(111), 80);
    AStandardEncodingUnicodeToSID.Add(Char(112), 81);
    AStandardEncodingUnicodeToSID.Add(Char(113), 82);
    AStandardEncodingUnicodeToSID.Add(Char(114), 83);
    AStandardEncodingUnicodeToSID.Add(Char(115), 84);
    AStandardEncodingUnicodeToSID.Add(Char(116), 85);
    AStandardEncodingUnicodeToSID.Add(Char(117), 86);
    AStandardEncodingUnicodeToSID.Add(Char(118), 87);
    AStandardEncodingUnicodeToSID.Add(Char(119), 88);
    AStandardEncodingUnicodeToSID.Add(Char(120), 89);
    AStandardEncodingUnicodeToSID.Add(Char(121), 90);
    AStandardEncodingUnicodeToSID.Add(Char(122), 91);
    AStandardEncodingUnicodeToSID.Add(Char(123), 92);
    AStandardEncodingUnicodeToSID.Add(Char(124), 93);
    AStandardEncodingUnicodeToSID.Add(Char(125), 94);
    AStandardEncodingUnicodeToSID.Add(Char(126), 95);
    AStandardEncodingUnicodeToSID.Add(Char(161), 96);
    AStandardEncodingUnicodeToSID.Add(Char(162), 97);
    AStandardEncodingUnicodeToSID.Add(Char(163), 98);
    AStandardEncodingUnicodeToSID.Add(Char(8260), 99);
    AStandardEncodingUnicodeToSID.Add(Char(165), 100);
    AStandardEncodingUnicodeToSID.Add(Char(402), 101);
    AStandardEncodingUnicodeToSID.Add(Char(167), 102);
    AStandardEncodingUnicodeToSID.Add(Char(164), 103);
    AStandardEncodingUnicodeToSID.Add(Char(39), 104);
    AStandardEncodingUnicodeToSID.Add(Char(8220), 105);
    AStandardEncodingUnicodeToSID.Add(Char(171), 106);
    AStandardEncodingUnicodeToSID.Add(Char(8249), 107);
    AStandardEncodingUnicodeToSID.Add(Char(8250), 108);
    AStandardEncodingUnicodeToSID.Add(Char(64257), 109);
    AStandardEncodingUnicodeToSID.Add(Char(64258), 110);
    AStandardEncodingUnicodeToSID.Add(Char(8211), 111);
    AStandardEncodingUnicodeToSID.Add(Char(8224), 112);
    AStandardEncodingUnicodeToSID.Add(Char(8225), 113);
    AStandardEncodingUnicodeToSID.Add(Char(183), 114);
    AStandardEncodingUnicodeToSID.Add(Char(182), 115);
    AStandardEncodingUnicodeToSID.Add(Char(8226), 116);
    AStandardEncodingUnicodeToSID.Add(Char(8218), 117);
    AStandardEncodingUnicodeToSID.Add(Char(8222), 118);
    AStandardEncodingUnicodeToSID.Add(Char(8221), 119);
    AStandardEncodingUnicodeToSID.Add(Char(187), 120);
    AStandardEncodingUnicodeToSID.Add(Char(8230), 121);
    AStandardEncodingUnicodeToSID.Add(Char(8240), 122);
    AStandardEncodingUnicodeToSID.Add(Char(191), 123);
    AStandardEncodingUnicodeToSID.Add(Char(96), 124);
    AStandardEncodingUnicodeToSID.Add(Char(180), 125);
    AStandardEncodingUnicodeToSID.Add(Char(710), 126);
    AStandardEncodingUnicodeToSID.Add(Char(732), 127);
    AStandardEncodingUnicodeToSID.Add(Char(175), 128);
    AStandardEncodingUnicodeToSID.Add(Char(728), 129);
    AStandardEncodingUnicodeToSID.Add(Char(729), 130);
    AStandardEncodingUnicodeToSID.Add(Char(168), 131);
    AStandardEncodingUnicodeToSID.Add(Char(730), 132);
    AStandardEncodingUnicodeToSID.Add(Char(184), 133);
    AStandardEncodingUnicodeToSID.Add(Char(733), 134);
    AStandardEncodingUnicodeToSID.Add(Char(731), 135);
    AStandardEncodingUnicodeToSID.Add(Char(711), 136);
    AStandardEncodingUnicodeToSID.Add(Char(8212), 137);
    AStandardEncodingUnicodeToSID.Add(Char(198), 138);
    AStandardEncodingUnicodeToSID.Add(Char(170), 139);
    AStandardEncodingUnicodeToSID.Add(Char(321), 140);
    AStandardEncodingUnicodeToSID.Add(Char(216), 141);
    AStandardEncodingUnicodeToSID.Add(Char(338), 142);
    AStandardEncodingUnicodeToSID.Add(Char(186), 143);
    AStandardEncodingUnicodeToSID.Add(Char(230), 144);
    AStandardEncodingUnicodeToSID.Add(Char(305), 145);
    AStandardEncodingUnicodeToSID.Add(Char(322), 146);
    AStandardEncodingUnicodeToSID.Add(Char(248), 147);
    AStandardEncodingUnicodeToSID.Add(Char(339), 148);
    AStandardEncodingUnicodeToSID.Add(Char(223), 149);
    for AEncodingUnitKey in AStandardEncodingUnicodeToSID.Keys do
      if AStandardEncodingUnicodeToSID.TryGetValue(AEncodingUnitKey, AEncodingUnitValue) then
        if ACharset.TryGetValue(AEncodingUnitValue, AGID) then
          AMapping.Add(TMap.Create(AEncodingUnitKey, AGID));

    AComparison :=
      function(const Left, Right: TMap): Integer
      begin
        Result := Ord(Left.CharCode) - Ord(Right.CharCode);
      end;
    AMapping.Sort(TComparer<TMap>.Construct(AComparison));
  finally
    AStandardEncodingUnicodeToSID.Free;
  end;
  ALength := Max(AMapping.Count, 1);
  FSegCount := ALength + 1;
  SetLength(FStartCode, FSegCount);
  SetLength(FEndCode, FSegCount);
  SetLength(FIdDelta, FSegCount);
  SetLength(FIdRangeOffset, FSegCount);
  AIndex := 0;
  for I := 0 to AMapping.Count - 1 do
  begin
    APair := AMapping[I];
    ACharCode := APair.CharCode;
    FStartCode[AIndex] := SmallInt(ACharCode);
    FEndCode[AIndex] := SmallInt(ACharCode);
    FIdDelta[AIndex] := SmallInt(APair.GID - SmallInt(ACharCode));
    Inc(AIndex);
  end;
  FStartCode[ALength] := FinalCode;
  FEndCode[ALength] := FinalCode;
  FIdDelta[ALength] := FinalDelta;
  SetLength(FGlyphIdArray, 0);
  AMapping.Free;
end;


{ TdxFontFileCMapCustomFormatRecord }

constructor TdxFontFileCMapCustomFormatRecord.Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID);
begin
  inherited Create;
  FPlatformId := APlatformId;
  FEncodingId := AEncodingId;
end;

constructor TdxFontFileCMapCustomFormatRecord.Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID;
  AStream: TdxFontFileStream);
begin
  Create(APlatformId, AEncodingId);
end;

function TdxFontFileCMapCustomFormatRecord.MapCode(ACharacter: Char): Integer;
begin
  Result := Integer(ACharacter);
end;

procedure TdxFontFileCMapCustomFormatRecord.UpdateEncoding(var AEncoding: TSmallIntDynArray);
begin
  TdxPDFUtils.RaiseTestException;
end;

procedure TdxFontFileCMapCustomFormatRecord.Write(AStream: TdxFontFileStream);
begin
  AStream.WriteShort(SmallInt(Format));
end;

class procedure TdxFontFileCMapCustomFormatRecord.UpdateEncodingValue(const AEncoding: TSmallIntDynArray;
  AIndex, AValue: SmallInt);
var
  AExistingValue: SmallInt;
begin
  if AValue <> 0 then
  begin
    AExistingValue := AEncoding[AIndex];
    if AExistingValue = 0 then
      AEncoding[AIndex] := AValue;
  end;
end;

{ TdxFontFileCMapShortFormatRecord }

constructor TdxFontFileCMapShortFormatRecord.Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID;
  ALanguage: SmallInt);
begin
  Create(APlatformId, AEncodingId);
end;

constructor TdxFontFileCMapShortFormatRecord.Create(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID;
  AStream: TdxFontFileStream);
begin
  inherited Create(APlatformId, AEncodingId, AStream);
  FBodyLength := Max(AStream.ReadUshort - HeaderLength, 0);
  Language := SmallInt(AStream.ReadShort);
end;

procedure TdxFontFileCMapShortFormatRecord.Write(ATableStream: TdxFontFileStream);
begin
  inherited Write(ATableStream);
  ATableStream.WriteShort(SmallInt(Size));
  ATableStream.WriteShort(Language);
end;

function TdxFontFileCMapShortFormatRecord.GetSize: Integer;
begin
  Result := HeaderLength + FBodyLength;
end;

{ TdxFontFileCMapTrimmedMappingRecord }

constructor TdxFontFileCMapTrimmedMappingRecord.Create(APlatformId: TdxFontFilePlatformID;
  AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream);
begin
  inherited Create(APlatformId, AEncodingId, AStream);
  FFirstCode := AStream.ReadShort;
  FEntryCount := AStream.ReadShort;
  FGlyphIdArray := AStream.ReadShortArray(FEntryCount);
end;

function TdxFontFileCMapTrimmedMappingRecord.MapCode(ACharacter: Char): Integer;
var
  ACode: Integer;
begin
  ACode := Integer(ACharacter) - FFirstCode;
  if (ACode >= 0) and (ACode < FEntryCount) then
    Result := Word(FGlyphIdArray[ACode])
  else
    Result := MissingGlyphIndex;
end;

procedure TdxFontFileCMapTrimmedMappingRecord.Write(ATableStream: TdxFontFileStream);
begin
  inherited Write(ATableStream);
  ATableStream.WriteShort(FFirstCode);
  ATableStream.WriteShort(FEntryCount);
  ATableStream.WriteShortArray(FGlyphIdArray);
end;

function TdxFontFileCMapTrimmedMappingRecord.GetFormat: TdxFontFileCMapFormatID;
begin
  Result := TdxFontFileCMapFormatID.TrimmedMapping;
end;

function TdxFontFileCMapTrimmedMappingRecord.GetSize: Integer;
begin
  Result := HeaderLength + 4 + Length(FGlyphIdArray) * 2;
end;

procedure TdxFontFileCMapTrimmedMappingRecord.UpdateEncoding(var AEncoding: TSmallIntDynArray);
var
  I, ACode: SmallInt;
begin
  ACode := FFirstCode;
  for I := 0 to FEntryCount - 1 do
  begin
    if ACode >= 256 then
      Break;
    UpdateEncodingValue(AEncoding, ACode, FGlyphIdArray[I]);
    Inc(ACode);
  end;
end;


function TdxFontFileCMapUnicodeVariationSelectorRecord.Write(ATableStream: TdxFontFileStream; AOffset: Integer): Integer;
var
  ADefaultUVSTableSize, ANonDefaultUVSTableSize: Integer;
  AVarSelectorBytes: TBytes;
begin
  ADefaultUVSTableSize := 4;
  ANonDefaultUVSTableSize := 5;
  SetLength(AVarSelectorBytes, 3);
  AVarSelectorBytes[2] := Byte((FVarSelector and $FF));
  AVarSelectorBytes[1] := Byte(((FVarSelector and $FF00) shr 8));
  AVarSelectorBytes[0] := Byte(((FVarSelector and $FF0000) shr 16));
  ATableStream.WriteArray(AVarSelectorBytes);
  if FDefaultUVSTables = nil then
    ATableStream.WriteInt(0)
  else
  begin
    ATableStream.WriteInt(AOffset);
    Inc(AOffset, (ADefaultUVSTableSize * Length(FDefaultUVSTables)) + 4);
  end;
  if FNonDefaultUVSTables = nil then
    ATableStream.WriteInt(0)
  else
  begin
    ATableStream.WriteInt(AOffset);
    Inc(AOffset, (ANonDefaultUVSTableSize * Length(FNonDefaultUVSTables) + 4));
  end;
  Result := AOffset;
end;

{ TdxFontFileCMapUnicodeVariationSequenciesRecord }

constructor TdxFontFileCMapUnicodeVariationSequenciesRecord.Create(APlatformId: TdxFontFilePlatformID;
  AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream);
var
  AStartPosition, APos: Int64;
  AVariationSelectorRecordsCount, I, AVarSelector, ADefaultUVSOffset: Integer;
  ANonDefaultUVSOffset, ANumUnicodeValueRanges, J, ANumUVSMappings: Integer;
  ADefaultUVSTables: TArray<TdxDefaultUVSTable>;
  ANonDefaultUVSTables: TArray<TdxNonDefaultUVSTable>;
begin
  inherited Create(APlatformId, AEncodingId);
  AStartPosition := AStream.Position - 2;
  AStream.ReadInt;
  AVariationSelectorRecordsCount := AStream.ReadInt;
  SetLength(FVariationSelectorRecords, AVariationSelectorRecordsCount);
  for I := 0 to AVariationSelectorRecordsCount - 1 do
  begin
    AVarSelector := GetInt24(AStream.ReadArray(3));
    ADefaultUVSOffset := AStream.ReadInt;
    ANonDefaultUVSOffset := AStream.ReadInt;
    APos := AStream.Position;
    ADefaultUVSTables := nil;
    ANonDefaultUVSTables := nil;
    if ADefaultUVSOffset <> 0 then
    begin
      AStream.Position := AStartPosition + ADefaultUVSOffset;
      ANumUnicodeValueRanges := AStream.ReadInt;
      SetLength(ADefaultUVSTables, ANumUnicodeValueRanges);
      for J := 0 to ANumUnicodeValueRanges - 1 do
        ADefaultUVSTables[J] := TdxDefaultUVSTable.Create(GetInt24(AStream.ReadArray(3)), AStream.ReadByte);
    end;
    if ANonDefaultUVSOffset <> 0 then
    begin
      AStream.Position := AStartPosition + ANonDefaultUVSOffset;
      ANumUVSMappings := AStream.ReadInt;
      SetLength(ANonDefaultUVSTables, ANumUVSMappings);
      for J := 0 to ANumUVSMappings - 1 do
        ANonDefaultUVSTables[J] := TdxNonDefaultUVSTable.Create(GetInt24(AStream.ReadArray(3)), AStream.ReadShort);
    end;
    AStream.Position := APos;
    FVariationSelectorRecords[I] := TdxFontFileCMapUnicodeVariationSelectorRecord.Create(AVarSelector, ADefaultUVSTables, ANonDefaultUVSTables);
  end;
  AStream.Position := AStartPosition + Size;
end;

destructor TdxFontFileCMapUnicodeVariationSequenciesRecord.Destroy;
var
  EA5697E9668D44678735E263F6731488: Integer;
begin
  for EA5697E9668D44678735E263F6731488 := 0 to Length(FVariationSelectorRecords) - 1do
    FVariationSelectorRecords[EA5697E9668D44678735E263F6731488].Free;
  inherited Destroy;
end;

class function TdxFontFileCMapUnicodeVariationSequenciesRecord.GetInt24(const AArray: TBytes): Integer;
begin
  if (AArray <> nil) and (Length(AArray) = 3) then
    Result := (AArray[0] shl 16) + (AArray[1] shl 8) + AArray[2]
  else
    Result := 0;
end;

procedure TdxFontFileCMapUnicodeVariationSequenciesRecord.Write(ATableStream: TdxFontFileStream);
var
  AOffset: Integer;
  AVarSelectorRecord: TdxFontFileCMapUnicodeVariationSelectorRecord;
  ADTables: TArray<TdxDefaultUVSTable>;
  ADTable: TdxDefaultUVSTable;
  ANdTables: TArray<TdxNonDefaultUVSTable>;
  ANdTable: TdxNonDefaultUVSTable;
begin
  inherited Write(ATableStream);
  ATableStream.WriteInt(Size);
  ATableStream.WriteInt(Length(FVariationSelectorRecords));
  AOffset := FHeaderLength + Length(FVariationSelectorRecords) * FVariationSelectorRecordSize;
  for AVarSelectorRecord in FVariationSelectorRecords do
    Inc(AOffset, AVarSelectorRecord.Write(ATableStream, AOffset));
  for AVarSelectorRecord in FVariationSelectorRecords do
  begin
    ADTables := AVarSelectorRecord.DefaultUVSTables;
    if ADTables <> nil then
    begin
      ATableStream.WriteInt(Length(ADTables));
      for ADTable in ADTables do
        ADTable.Write(ATableStream);
    end;
    ANdTables := AVarSelectorRecord.NonDefaultUVSTables;
    if ANdTables <> nil then
    begin
      ATableStream.WriteInt(Length(ANdTables));
      for ANdTable in ANdTables do
        ANdTable.Write(ATableStream);
    end;
  end;
end;

function TdxFontFileCMapUnicodeVariationSequenciesRecord.GetFormat: TdxFontFileCMapFormatID;
begin
  Result := TdxFontFileCMapFormatID.UnicodeVariationSequences;
end;

function TdxFontFileCMapUnicodeVariationSequenciesRecord.GetSize: Integer;
var
  ALength: Integer;
  AVariationSelectorRecord: TdxFontFileCMapUnicodeVariationSelectorRecord;
  ADTables: TArray<TdxDefaultUVSTable>;
  ANdTables: TArray<TdxNonDefaultUVSTable>;
begin
  ALength := FHeaderLength + Length(FVariationSelectorRecords) * FVariationSelectorRecordSize;
  for AVariationSelectorRecord in FVariationSelectorRecords do
  begin
    ADTables := AVariationSelectorRecord.DefaultUVSTables;
    if ADTables <> nil then
      Inc(ALength, (4 + Length(ADTables) * FDefaultUVSTableSize));
    ANdTables := AVariationSelectorRecord.NonDefaultUVSTables;
    if ANdTables <> nil then
      Inc(ALength, (4 + Length(ANdTables) * FNonDefaultUVSTableSize));
  end;
  Result := ALength;
end;

{ TdxFontFileCMapTable }

constructor TdxFontFileCMapTable.Create(ASegmentMappingFormatEntry: TdxFontFileCMapSegmentMappingRecord);
begin
  inherited Create(nil);
  FCMapTables := TObjectList<TdxFontFileCMapCustomFormatRecord>.Create;
  FMappedGlyphCache := TDictionary<Integer, Integer>.Create;

  FCMapTables.Add(ASegmentMappingFormatEntry);
  Changed;
end;

constructor TdxFontFileCMapTable.CreateFromCharset(ACharset: TDictionary<SmallInt, Smallint>);
var
  ACMapEntry: TdxFontFileCMapSegmentMappingRecord;
begin
  ACMapEntry := TdxFontFileCMapSegmentMappingRecord.CreateFromCharset(ACharset);
  Create(ACMapEntry);
end;

constructor TdxFontFileCMapTable.Create(const AData: TBytes);
var
  ANumberOfEncodingTables: SmallInt;
  I, AOffset: Integer;
  APlatformId: TdxFontFilePlatformID;
  AEncodingId: TdxFontFileEncodingID;
  APosition: Int64;
begin
  inherited Create(AData);
  FCMapTables := TObjectList<TdxFontFileCMapCustomFormatRecord>.Create;
  FMappedGlyphCache := TDictionary<Integer, Integer>.Create;

  FVersion := DataStream.ReadShort;
  ANumberOfEncodingTables := DataStream.ReadShort;
  FCMapTables.Capacity := ANumberOfEncodingTables;
  for I := 0 to ANumberOfEncodingTables - 1 do
  begin
    APlatformId := TdxFontFilePlatformID(DataStream.ReadShort);
    AEncodingId := TdxFontFileEncodingID(DataStream.ReadShort);
    AOffset := DataStream.ReadInt;
    APosition := DataStream.Position;
    DataStream.Position := AOffset;
    FCMapTables.Add(CreateRecord(APlatformId, AEncodingId, TdxFontFileCMapFormatID(DataStream.ReadShort), DataStream));
    DataStream.Position := APosition;
  end;
end;

destructor TdxFontFileCMapTable.Destroy;
begin
  FreeAndNil(FMappedGlyphCache);
  FreeAndNil(FCMapTables);
  inherited Destroy;
end;

class function TdxFontFileCMapTable.Tag: string;
begin
  Result := 'cmap';
end;


procedure TdxFontFileHheaTable.DoApplyChanges;
begin
  inherited DoApplyChanges;
  DataStream.WriteInt(FVersion);
  DataStream.WriteShort(FAscender);
  DataStream.WriteShort(FDescender);
  DataStream.WriteShort(FLineGap);
  DataStream.WriteShort(FAdvanceWidthMax);
  DataStream.WriteShort(FMinLeftSideBearing);
  DataStream.WriteShort(FMinRightSideBearing);
  DataStream.WriteShort(FXMaxExtent);
  DataStream.WriteShort(FCaretSlopeRise);
  DataStream.WriteShort(FCaretSlopeRun);
  DataStream.WriteShort(0);
  DataStream.WriteShort(0);
  DataStream.WriteShort(0);
  DataStream.WriteShort(0);
  DataStream.WriteShort(0);
  DataStream.WriteShort(FMetricDataFormat);
  DataStream.WriteShort(SmallInt(FNumberOfHMetrics));
end;

{ TdxFontFileHeadTable }

constructor TdxFontFileHeadTable.Create(const AData: TBytes);
var
  AValue: SmallInt;
begin
  inherited Create(AData);
  if Length(AData) > 0 then
  begin
    FUnitsPerEm := DefaultUnitsPerEm;
    FVersion := DataStream.ReadInt;
    FFontRevision := DataStream.ReadInt;
    FCheckSumAdjustment := DataStream.ReadInt;
    FMagicNumber := DataStream.ReadInt;
    FFlags := TdxFontFileHeadTableFlags(DataStream.ReadShort);
    FUnitsPerEm := DataStream.ReadShort;
    FCreated := DataStream.ReadLong;
    FModified := DataStream.ReadLong;
    FXMin := DataStream.ReadShort;
    FYMin := DataStream.ReadShort;
    FXMax := DataStream.ReadShort;
    FYMax := DataStream.ReadShort;
    FMacStyle := TdxFontFileHeadTableMacStyle(DataStream.ReadShort);
    FLowestRecPPEM := DataStream.ReadShort;
    FFontDirectionHint := TdxFontFileDirectionHint(DataStream.ReadShort);

    AValue := DataStream.ReadShort;
    if AValue = 0 then
      FIndexToLocFormat := TdxFontFileIndexToLocFormat.Short
    else
      FIndexToLocFormat := TdxFontFileIndexToLocFormat.Long;
    FGlyphDataFormat := DataStream.ReadShort;
  end;
end;

class function TdxFontFileHeadTable.Tag: string;
const
  EA5697E9668D44678735E263F6731488 = 'head';
begin
  Result := EA5697E9668D44678735E263F6731488;
end;

procedure TdxFontFileHeadTable.DoApplyChanges;
begin
  inherited DoApplyChanges;
  DataStream.WriteInt(FVersion);
  DataStream.WriteInt(FFontRevision);
  DataStream.WriteInt(FCheckSumAdjustment);
  DataStream.WriteInt(FMagicNumber);
  DataStream.WriteShort(SmallInt(FFlags));
  DataStream.WriteShort(FUnitsPerEm);
  DataStream.WriteLong(FCreated);
  DataStream.WriteLong(FModified);
  DataStream.WriteShort(FXMin);
  DataStream.WriteShort(FYMin);
  DataStream.WriteShort(FXMax);
  DataStream.WriteShort(FYMax);
  DataStream.WriteShort(SmallInt(FMacStyle));
  DataStream.WriteShort(FLowestRecPPEM);
  DataStream.WriteShort(SmallInt(FFontDirectionHint));
  DataStream.WriteShort(SmallInt(FIndexToLocFormat));
  DataStream.WriteShort(FGlyphDataFormat);
end;


{ TdxFontFileHmtxTable }

constructor TdxFontFileHmtxTable.Create(AGlyphCount: Integer);
var
  I: Integer;
  EA5697E9668D44678735E263F6731488: TBytes;
begin
  SetLength(EA5697E9668D44678735E263F6731488, 0);
  inherited Create(EA5697E9668D44678735E263F6731488);
  for I := 0 to AGlyphCount * 4 - 1 do
    DataStream.WriteByte(0);
end;

class function TdxFontFileHmtxTable.Tag: string;
begin
  Result := 'hmtx';
end;

function TdxFontFileHmtxTable.FillAdvanceWidths(AHMetricCount, AGlyphCount: Integer): TSmallIntDynArray;
var
  ASize, I: Integer;
  ALastAdvanceWidth: SmallInt;
begin
  DataStream.Position := 0;
  ASize := Max(AHMetricCount, AGlyphCount);
  SetLength(FAdvanceWidths, ASize);
  if DataSize >= AHMetricCount * 4 then
  begin
    for I := 0 to AHMetricCount - 1 do
    begin
      FAdvanceWidths[I] := DataStream.ReadShort;
      DataStream.ReadShort;
    end;
    ALastAdvanceWidth := FAdvanceWidths[AHMetricCount - 1];
    for I := AHMetricCount to ASize - 1 do
      FAdvanceWidths[I] := ALastAdvanceWidth;
  end;
  Result := FAdvanceWidths;
end;

{ TdxFontFileHheaTable }

constructor TdxFontFileHheaTable.Create(const AData: TBytes);
begin
  inherited Create(AData);
  if Length(AData) > 0 then
  begin
    FVersion := DataStream.ReadInt;
    FAscender := DataStream.ReadShort;
    FDescender := DataStream.ReadShort;
    FLineGap := DataStream.ReadShort;
    FAdvanceWidthMax := DataStream.ReadShort;
    FMinLeftSideBearing := DataStream.ReadShort;
    FMinRightSideBearing := DataStream.ReadShort;
    FXMaxExtent := DataStream.ReadShort;
    FCaretSlopeRise := DataStream.ReadShort;
    FCaretSlopeRun := DataStream.ReadShort;
    DataStream.ReadShort;
    DataStream.ReadShort;
    DataStream.ReadShort;
    DataStream.ReadShort;
    DataStream.ReadShort;
    FMetricDataFormat := DataStream.ReadShort;
    FNumberOfHMetrics := DataStream.ReadUshort;
  end;
end;

class function TdxFontFileHheaTable.Tag: string;
begin
  Result := 'hhea';
end;

procedure TdxFontFileHheaTable.Validate;
begin
  if (FAscender = 0) and (FDescender = 0) then
  begin
    FAscender := 1;
    FDescender := -1;
    Changed;
  end;
end;


function TdxFontFileCMapTable.Validate(ASkipEncodingValidation: Boolean; AIsSymbolic: Boolean): TdxFontFileCMapSegmentMappingRecord;
var
  AEncodingID: TdxFontFileEncodingID;
  ASegmentMappingFormatEntry, AUncompatibleSegmentMappingFormatEntry: TdxFontFileCMapSegmentMappingRecord;
  AActualTrimmedMappingFormatEntry: TdxFontFileCMapTrimmedMappingRecord;
  AActualByteEncodingFormatEntry: TdxFontFileCMapByteEncodingRecord;
  AFormatEntry: TdxFontFileCMapCustomFormatRecord;
begin
  if AIsSymbolic then
    AEncodingID := TdxFontFileEncodingID.Undefined
  else
    AEncodingID := TdxFontFileEncodingID.UGL;

  AActualTrimmedMappingFormatEntry := nil;
  AActualByteEncodingFormatEntry := nil;
  AUncompatibleSegmentMappingFormatEntry := nil;

  for AFormatEntry in FCMapTables do
  begin
    if AFormatEntry is TdxFontFileCMapSegmentMappingRecord then
    begin
      ASegmentMappingFormatEntry := AFormatEntry as TdxFontFileCMapSegmentMappingRecord;
      if (ASegmentMappingFormatEntry <> nil) then
      begin
        if ASegmentMappingFormatEntry.PlatformId = TdxFontFilePlatformID.Microsoft then
          if ((ASkipEncodingValidation) or (ASegmentMappingFormatEntry.EncodingId = AEncodingID)) then
          begin
            Changed(ASegmentMappingFormatEntry.Validate);
            Exit(ASegmentMappingFormatEntry);
          end;
        AUncompatibleSegmentMappingFormatEntry := ASegmentMappingFormatEntry;
      end;
    end;
    if AFormatEntry is TdxFontFileCMapTrimmedMappingRecord then
      AActualTrimmedMappingFormatEntry := TdxFontFileCMapTrimmedMappingRecord(AFormatEntry);

    if AFormatEntry is TdxFontFileCMapByteEncodingRecord then
      AActualByteEncodingFormatEntry := TdxFontFileCMapByteEncodingRecord(AFormatEntry);
  end;

  if AActualTrimmedMappingFormatEntry <> nil then
    ASegmentMappingFormatEntry := TdxFontFileCMapSegmentMappingRecord.CreateFromTrimmedMapping(AEncodingID, AActualTrimmedMappingFormatEntry)
  else
    if AActualByteEncodingFormatEntry <> nil then
      ASegmentMappingFormatEntry := TdxFontFileCMapSegmentMappingRecord.CreateFromByteEncoding(AEncodingID, AActualByteEncodingFormatEntry)
    else
      if AUncompatibleSegmentMappingFormatEntry <> nil then
        ASegmentMappingFormatEntry := TdxFontFileCMapSegmentMappingRecord.CreateFromSegmentMapping(AEncodingID, AUncompatibleSegmentMappingFormatEntry)
      else
        ASegmentMappingFormatEntry := TdxFontFileCMapSegmentMappingRecord.CreateDefault(AEncodingID);
  FCMapTables.Clear;
  FCMapTables.Add(ASegmentMappingFormatEntry);
  Changed;
  Result := ASegmentMappingFormatEntry;
end;

function TdxFontFileCMapTable.MapCodes(const AStr: string): TIntegerDynArray;
var
  ACount, I: Integer;
  ACodes: TIntegerDynArray;
begin
  if AStr = '' then
    Exit;
  ACount := Length(AStr);
  SetLength(ACodes, ACount);
  for I := 0 to ACount - 1 do
    ACodes[I] := MapCode(AStr[I]);
  Result := ACodes;
end;

function TdxFontFileCMapTable.MapCode(ACharacter: Char): Integer;
var
  AGlyph: Integer;
  ACMap: TdxFontFileCMapCustomFormatRecord;
begin
  AGlyph := 0;
  if FMappedGlyphCache.TryGetValue(Integer(ACharacter), AGlyph) then
    Exit(AGlyph);
  for ACMap in FCMapTables do
  begin
    AGlyph := ACMap.MapCode(ACharacter);
    if AGlyph <> TdxFontFileCMapCustomFormatRecord.MissingGlyphIndex then
      Break;
  end;
  FMappedGlyphCache.Add(Integer(ACharacter), AGlyph);
  Result := AGlyph;
end;

procedure TdxFontFileCMapTable.PopulateEncoding(var AEncoding: TSmallIntDynArray);
begin
  SetLength(AEncoding, 256);
  UpdateEncoding(AEncoding,
    function(ATable: TdxFontFileCMapCustomFormatRecord): Boolean
    begin
      Result := (ATable.EncodingId = TdxFontFileEncodingID.Undefined) and (ATable is TdxFontFileCMapSegmentMappingRecord);
    end);

  UpdateEncoding(AEncoding,
    function(ATable: TdxFontFileCMapCustomFormatRecord): Boolean
    begin
      Result := (ATable.EncodingId = TdxFontFileEncodingID.Undefined) and not (ATable is TdxFontFileCMapSegmentMappingRecord);
    end);

  UpdateEncoding(AEncoding,
    function(ATable: TdxFontFileCMapCustomFormatRecord): Boolean
    begin
      Result := ATable.EncodingId = TdxFontFileEncodingID.Undefined;
    end);
end;

procedure TdxFontFileCMapTable.DoApplyChanges;
var
  AOffset: Integer;
  ACMapTableCount: SmallInt;
  ACMapRecord: TdxFontFileCMapCustomFormatRecord;
begin
  inherited DoApplyChanges;
  DataStream.WriteShort(FVersion);
  ACMapTableCount := SmallInt(FCMapTables.Count);
  DataStream.WriteShort(ACMapTableCount);
  AOffset := 4 + ACMapTableCount * 8;
  for ACMapRecord in FCMapTables do
  begin
    DataStream.WriteShort(SmallInt(ACMapRecord.PlatformId));
    DataStream.WriteShort(SmallInt(ACMapRecord.EncodingId));
    DataStream.WriteInt(AOffset);
    Inc(AOffset, ACMapRecord.Size);
  end;
  for ACMapRecord in FCMapTables do
    ACMapRecord.Write(DataStream);
end;

function TdxFontFileCMapTable.CreateRecord(APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID;
  AFormat: TdxFontFileCMapFormatID; AStream: TdxFontFileStream): TdxFontFileCMapCustomFormatRecord;
begin
  Result := nil;
  case AFormat of
    TdxFontFileCMapFormatID.ByteEncoding:
      Result := TdxFontFileCMapByteEncodingRecord.Create(APlatformId, AEncodingId, AStream);
    TdxFontFileCMapFormatID.HighByteMappingThrough:
      Result := TdxFontFileCMapHighByteMappingThroughRecord.Create(APlatformId, AEncodingId, AStream);
    TdxFontFileCMapFormatID.SegmentMapping:
      Result := TdxFontFileCMapSegmentMappingRecord.Create(APlatformId, AEncodingId, AStream);
    TdxFontFileCMapFormatID.TrimmedMapping:
      Result := TdxFontFileCMapTrimmedMappingRecord.Create(APlatformId, AEncodingId, AStream);
    TdxFontFileCMapFormatID.MixedCoverage:
      Result := TdxFontFileCMapMixedCoverageRecord.Create(APlatformId, AEncodingId, AStream);
    TdxFontFileCMapFormatID.TrimmedArray:
      Result := TdxFontFileCMapTrimmedArrayRecord.Create(APlatformId, AEncodingId, AStream);
    TdxFontFileCMapFormatID.SegmentedCoverage:
      Result := TdxFontFileCMapSegmentedCoverageRecord.Create(APlatformId, AEncodingId, AStream);
    TdxFontFileCMapFormatID.ManyToOneRangeMapping:
      Result := TdxFontFileCMapManyToOneRangeMappingRecord.Create(APlatformId, AEncodingId, AStream);
    TdxFontFileCMapFormatID.UnicodeVariationSequences:
      Result := TdxFontFileCMapUnicodeVariationSequenciesRecord.Create(APlatformId, AEncodingId, AStream);
  else
    TdxPDFUtils.RaiseTestException('Error creating CMap record');
  end;
end;

procedure TdxFontFileCMapTable.UpdateEncoding(var AEncoding: TSmallIntDynArray; AChooseTableFunc: TChooseTableFunc);
var
  ATable: TdxFontFileCMapCustomFormatRecord;
begin
  for ATable in FCMapTables do
    if AChooseTableFunc(ATable) then
      ATable.UpdateEncoding(AEncoding);
end;

{ TdxFontFileNameRecord }

class function TdxFontFileNameRecord.Create(AStream: TdxFontFileStream; ADataOffset: Integer): TdxFontFileNameRecord;
var
  ALength, AOffset: Integer;
  APosition, ASavedPosition: Int64;
begin
  Result.FPlatformID := TdxFontFilePlatformID(AStream.ReadUshort);
  Result.FEncodingID := TdxFontFileEncodingID(AStream.ReadUshort);
  Result.FLanguageID := TdxFontFileLanguageID(AStream.ReadUshort);
  Result.FNameID := TdxFontFileNameID(AStream.ReadUshort);
  ALength := AStream.ReadUshort;
  AOffset := AStream.ReadUshort;
  APosition := ADataOffset + AOffset;
  if APosition + ALength <= AStream.Size then
  begin
    ASavedPosition := AStream.Position;
    AStream.Position := APosition;
    try
      Result.FNameBytes := AStream.ReadArray(ALength);
      if Result.PlatformID = TdxFontFilePlatformID.Microsoft then
        Result.FName := TdxPDFUtils.ConvertToStr(Result.FNameBytes, Length(Result.FNameBytes))
      else
        Result.FName := TdxPDFUtils.ConvertToASCIIString(Result.FNameBytes);
    finally
      AStream.Position := ASavedPosition;
    end;
  end;
end;

class function TdxFontFileNameRecord.Create(APlatformID: TdxFontFilePlatformID; ALanguageID: TdxFontFileLanguageID;
  ANameID: TdxFontFileNameID; AEncodingID: TdxFontFileEncodingID; const ANameBytes: TBytes): TdxFontFileNameRecord;
begin
  Result.FPlatformID := APlatformID;
  Result.FLanguageID := ALanguageID;
  Result.FNameID := ANameID;
  Result.FEncodingID := AEncodingID;
  Result.FNameBytes := ANameBytes;
  if APlatformID = TdxFontFilePlatformID.Microsoft then
    Result.FName := TdxpDFUtils.ConvertToStr(Result.FNameBytes, Length(Result.FNameBytes))
  else
    Result.FName := TdxPDFUtils.ConvertToASCIIString(Result.FNameBytes);
end;

{ TdxFontFileNameTable }

constructor TdxFontFileNameTable.Create(const AData: TBytes);
var
  I: Integer;
  ACount, AOffset: SmallInt;
begin
  inherited Create(AData);
  FNamingTable := TList<TdxFontFileNameRecord>.Create;
  FFamilyName := '';
  FMacFamilyName := '';
  FPostScriptName := '';
  if DataSize > 6 then
  begin
    DataStream.ReadShort;
    ACount := DataStream.ReadShort;
    AOffset := DataStream.ReadShort;
    for I := 0 to ACount - 1 do
      FNamingTable.Add(TdxFontFileNameRecord.Create(DataStream, AOffset));
  end;
end;

constructor TdxFontFileNameTable.Create(ACMapEntry: TdxFontFileCMapTable; const AFontName: string);
begin
  Create(nil);
  AddName(ACMapEntry, AFontName);
end;

destructor TdxFontFileNameTable.Destroy;
begin
  FreeAndNil(FNamingTable);
  inherited Destroy;
end;

function TdxFontFileNameTable.GetFamilyName: string;
begin
  if FFamilyName = '' then
    FFamilyName := FindName(TdxFontFilePlatformID.Microsoft, TdxFontFileEncodingID.UGL,
      TdxFontFileLanguageID.EnglishUnitedStates, TdxFontFileNameID.FontFamily);
  Result := FFamilyName;
end;

function TdxFontFileNameTable.GetMacFamilyName: string;
begin
  if FMacFamilyName = '' then
    FMacFamilyName := FindName(TdxFontFilePlatformID.Macintosh, TdxFontFileEncodingID.Undefined,
      TdxFontFileLanguageID.English, TdxFontFileNameID.FontFamily);
  Result := FMacFamilyName;
end;

function TdxFontFileNameTable.GetPostScriptName: string;
begin
  if FPostScriptName = '' then
    FPostScriptName := FindName(TdxFontFilePlatformID.Macintosh, TdxFontFileEncodingID.Undefined,
      TdxFontFileLanguageID.English, TdxFontFileNameID.PostscriptName);
  Result := FPostScriptName;
end;

class function TdxFontFileNameTable.Tag: string;
begin
  Result := 'name';
end;

function TdxFontFileNameTable.FindName(APlatform: TdxFontFilePlatformID;
  AEncoding: TdxFontFileEncodingID; ALanguage: TdxFontFileLanguageID; AId: TdxFontFileNameID): string;
var
  ARecord: TdxFontFileNameRecord;
begin
  for ARecord in FNamingTable do
    if (((ARecord.PlatformID = APlatform) and (ARecord.EncodingID = AEncoding)) and (ARecord.LanguageID = ALanguage)) and (ARecord.NameID = AId) then
      Exit(ARecord.Name);
  Result := '';
end;

procedure TdxFontFileNameTable.AddName(ACMapEntry: TdxFontFileCMapTable; const AFontName: string);
var
  ANamesDictionary: TDictionary<TdxFontFileNameID, TBytes>;
  AEncoding: TEncoding;
  AName: string;
  AFontNameBytes: TBytes;
  ACMapRecord: TdxFontFileCMapCustomFormatRecord;
  APlatformId: TdxFontFilePlatformID;
  AEncodingId: TdxFontFileEncodingID;
  ALanguageId: TdxFontFileLanguageID;
  AEntry: TPair<TdxFontFileNameID, TBytes>;
begin
  if Length(AFontName) > MaxNameLength then
    AName := Copy(AFontName, 1, MaxNameLength)
  else
    AName := AFontName;

  ANamesDictionary := TDictionary<TdxFontFileNameID, TBytes>.Create;
  try
    AEncoding := TEncoding.BigEndianUnicode;
    AFontNameBytes := AEncoding.GetBytes(AName);
    ANamesDictionary.Add(TdxFontFileNameID.FontFamily, AFontNameBytes);
    ANamesDictionary.Add(TdxFontFileNameID.FontSubfamily, AEncoding.GetBytes(NameFontSubfamily));
    ANamesDictionary.Add(TdxFontFileNameID.FullFontName, AFontNameBytes);
    ANamesDictionary.Add(TdxFontFileNameID.UniqueFontId, AFontNameBytes);
    ANamesDictionary.Add(TdxFontFileNameID.Version, AEncoding.GetBytes(NameVersion));
    ANamesDictionary.Add(TdxFontFileNameID.PostscriptName, AFontNameBytes);
    FNamingTable.Clear;
    for ACMapRecord in ACMapEntry.CMapTables do
    begin
      APlatformId := ACMapRecord.PlatformId;
      AEncodingId := ACMapRecord.EncodingId;
      if APlatformId = TdxFontFilePlatformID.Microsoft then
        ALanguageId := TdxFontFileLanguageID.EnglishUnitedStates
      else
        ALanguageId := TdxFontFileLanguageID.English;
      for AEntry in ANamesDictionary do
        FNamingTable.Add(TdxFontFileNameRecord.Create(APlatformId, ALanguageId, AEntry.Key, AEncodingId, AEntry.Value));
    end;
  finally
    ANamesDictionary.Free;
  end;
  Changed;
end;

procedure TdxFontFileNameTable.DoApplyChanges;
var
  I: Integer;
  ANameBytes: TBytes;
  ANameRecord: TdxFontFileNameRecord;
  ANameOffset, ANameLength: SmallInt;
begin
  inherited DoApplyChanges;
  DataStream.WriteShort(0);
  DataStream.WriteShort(SmallInt(FNamingTable.Count));
  DataStream.WriteShort(SmallInt((6 + FNamingTable.Count * 12)));
  ANameOffset := 0;
  for I := 0 to FNamingTable.Count - 1 do
  begin
    ANameRecord := FNamingTable[I];
    DataStream.WriteShort(SmallInt(ANameRecord.PlatformID));
    DataStream.WriteShort(SmallInt(ANameRecord.EncodingID));
    DataStream.WriteShort(SmallInt(ANameRecord.LanguageID));
    DataStream.WriteShort(SmallInt(ANameRecord.NameID));
    ANameBytes := ANameRecord.NameBytes;
    ANameLength := SmallInt((Length(ANameRecord.NameBytes)));
    DataStream.WriteShort(ANameLength);
    DataStream.WriteShort(ANameOffset);
    Inc(ANameOffset, ANameLength);
  end;
  for I := 0 to FNamingTable.Count - 1 do
  begin
    ANameRecord := FNamingTable[I];
    if ANameRecord.NameBytes <> nil then
      DataStream.WriteArray(ANameRecord.NameBytes);
  end;
end;


{ TdxFontFileKernTable }

constructor TdxFontFileKernTable.Create(const AData: TBytes);
var
  ATablesCount, I, AFormat, APairCount, J, APair: Integer;
  ATableStart, ALastTableLength: Int64;
begin
  inherited Create(AData);
  FKerning := TDictionary<Integer, SmallInt>.Create;

  DataStream.ReadUshort;
  ATablesCount := DataStream.ReadUshort;
  ATableStart := DataStream.Position;
  ALastTableLength := 0;
  for I := 0 to ATablesCount - 1 do
  begin
    Inc(ATableStart, ALastTableLength);
    DataStream.Position := ATableStart + 2;
    ALastTableLength := DataStream.ReadUshort;
    AFormat := DataStream.ReadUshort and $FFF7;
    if AFormat = 1 then
    begin
      APairCount := DataStream.ReadUshort;
      DataStream.ReadArray(6);
      for J := 0 to APairCount - 1 do
      begin
        APair := DataStream.ReadInt;
        if not FKerning.ContainsKey(APair) then
          FKerning.Add(APair, DataStream.ReadShort);
      end;
    end;
  end;
end;

destructor TdxFontFileKernTable.Destroy;
begin
  FreeAndNil(FKerning);
  inherited Destroy;
end;

class function TdxFontFileKernTable.Tag: string;
begin
  Result := 'kern';
end;

function TdxFontFileKernTable.GetKerning(AGlyphIndex1: Integer; AGlyphIndex2: Integer): SmallInt;
begin
  if not FKerning.TryGetValue((AGlyphIndex1 shl 16) + AGlyphIndex2, Result) then
    Result := 0;
end;


{ TdxFontFilePostTable }

constructor TdxFontFilePostTable.Create(const AData: TBytes);
var
  AVersion, AGlyphCount, I, ALength: Integer;
  AGlyphNamePosition, APosition: Int64;
  AValue: SmallInt;
begin
  inherited Create(AData);
  FGlyphNames := TStringList.Create;
  if Length(AData) > 0 then
  begin
    AVersion := DataStream.ReadInt;
    FItalicAngle := DataStream.ReadFixed;
    if (AVersion = $20000) and (DataSize > HeaderSize) then
    begin
      DataStream.Position := HeaderSize;
      AGlyphCount := DataStream.ReadUshort;
      FGlyphNames.Capacity := AGlyphCount;
      AGlyphNamePosition := DataStream.Position + AGlyphCount * 2;
      for I := 0 to AGlyphCount - 1 do
      begin
        AValue := DataStream.ReadShort;
        if AValue < 258 then
          FGlyphNames.Add(StandardMacCharacterSet[AValue])
        else
        begin
          APosition := DataStream.Position;
          DataStream.Position := AGlyphNamePosition;
          ALength := DataStream.ReadByte;
          FGlyphNames.Add(DataStream.ReadString(ALength));
          AGlyphNamePosition := DataStream.Position;
          DataStream.Position := APosition;
        end;
      end;
    end;
  end;
end;

destructor TdxFontFilePostTable.Destroy;
begin
  FreeAndNil(FGlyphNames);
  inherited Destroy;
end;

class function TdxFontFilePostTable.Tag: string;
begin
  Result := 'post';
end;

{ TdxFontFileOS2Table }

constructor TdxFontFileOS2Table.Create(const AData: TBytes);
const
  EmptyValue = $00000000;
begin
  inherited Create(AData);
  if Length(AData) > 0 then
  begin
    FVersion := TdxFontFileVersion(DataStream.ReadShort);
    FAvgCharWidth := DataStream.ReadShort;
    FWeightClass := DataStream.ReadShort;
    FWidthClass := TdxFontFileOS2WidthClass(DataStream.ReadShort);
    FEmbeddingType := TdxFontFileOS2EmbeddingType(DataStream.ReadShort);
    FSubscriptXSize := DataStream.ReadShort;
    FSubscriptYSize := DataStream.ReadShort;
    FSubscriptXOffset := DataStream.ReadShort;
    FSubscriptYOffset := DataStream.ReadShort;
    FSuperscriptXSize := DataStream.ReadShort;
    FSuperscriptYSize := DataStream.ReadShort;
    FSuperscriptXOffset := DataStream.ReadShort;
    FSuperscriptYOffset := DataStream.ReadShort;
    FStrikeoutSize := DataStream.ReadShort;
    FStrikeoutPosition := DataStream.ReadShort;
    FFamilyClass := TdxFontFileOS2FamilyClass(DataStream.ReadShort);
    FPanose := TdxFontFilePanose.Create(DataStream);
    FUnicodeRange1 := TdxFontFileUnicodeRange1(DataStream.ReadInt);
    FUnicodeRange2 := TdxFontFileUnicodeRange2(DataStream.ReadInt);
    FUnicodeRange3 := TdxFontFileUnicodeRange3(DataStream.ReadInt);
    FUnicodeRange4 := TdxFontFileUnicodeRange4(DataStream.ReadInt);
    FVendor := DataStream.ReadString(4);
    FSelection := TdxFontFileSelection(DataStream.ReadShort);
    FFirstCharIndex := DataStream.ReadUshort;
    FLastCharIndex := DataStream.ReadUshort;
    FTypoAscender := DataStream.ReadShort;
    FTypoDescender := DataStream.ReadShort;
    FTypoLineGap := DataStream.ReadShort;
    FWinAscent := DataStream.ReadShort;
    FWinDescent := DataStream.ReadShort;
    if FVersion > TdxFontFileVersion.TrueType_1_5 then
    begin
      FCodePageRange1 := TdxFontFileCodePageRange1(DataStream.ReadInt);
      FCodePageRange2 := TdxFontFileCodePageRange2(DataStream.ReadInt);
    end
    else
    begin
      FCodePageRange1 := TdxFontFileCodePageRange1(EmptyValue);
      FCodePageRange2 := TdxFontFileCodePageRange2(EmptyValue);
    end;
  end;
end;

class function TdxFontFileOS2Table.Tag: string;
begin
  Result := 'OS/2';
end;

procedure TdxFontFileOS2Table.DoApplyChanges;
begin
  inherited DoApplyChanges;
  DataStream.WriteShort(SmallInt(FVersion));
  DataStream.WriteShort(FAvgCharWidth);
  DataStream.WriteShort(FWeightClass);
  DataStream.WriteShort(SmallInt(FWidthClass));
  DataStream.WriteShort(SmallInt(FEmbeddingType));
  DataStream.WriteShort(FSubscriptXSize);
  DataStream.WriteShort(FSubscriptYSize);
  DataStream.WriteShort(FSubscriptXOffset);
  DataStream.WriteShort(FSubscriptYOffset);
  DataStream.WriteShort(FSuperscriptXSize);
  DataStream.WriteShort(FSuperscriptYSize);
  DataStream.WriteShort(FSuperscriptXOffset);
  DataStream.WriteShort(FSuperscriptYOffset);
  DataStream.WriteShort(FStrikeoutSize);
  DataStream.WriteShort(FStrikeoutPosition);
  DataStream.WriteShort(SmallInt(FFamilyClass));
  FPanose.Write(DataStream);
  DataStream.WriteInt(Integer(FUnicodeRange1));
  DataStream.WriteInt(Integer(FUnicodeRange2));
  DataStream.WriteInt(Integer(FUnicodeRange3));
  DataStream.WriteInt(Integer(FUnicodeRange4));
  DataStream.WriteString(FVendor);
  DataStream.WriteShort(SmallInt(FSelection));
  DataStream.WriteShort(SmallInt(FFirstCharIndex));
  DataStream.WriteShort(SmallInt(FLastCharIndex));
  DataStream.WriteShort(FTypoAscender);
  DataStream.WriteShort(FTypoDescender);
  DataStream.WriteShort(FTypoLineGap);
  DataStream.WriteShort(FWinAscent);
  DataStream.WriteShort(FWinDescent);
  if FVersion >= TdxFontFileVersion.TrueType_1_66 then
  begin
    DataStream.WriteInt(Integer(FCodePageRange1));
    DataStream.WriteInt(Integer(FCodePageRange2));
    if FVersion >= TdxFontFileVersion.OpenType_1_2 then
    begin
      DataStream.WriteShort(FXHeight);
      DataStream.WriteShort(FCapHeight);
      DataStream.WriteShort(FDefaultChar);
      DataStream.WriteShort(FBreakChar);
      DataStream.WriteShort(FMaxContext);
    end;
  end;
end;

function TdxFontFileOS2Table.GetIsSymbolic: Boolean;
begin
  Result := (Integer(FCodePageRange1) and Integer(TdxFontFileCodePageRange1.SymbolCharacterSet)) <> 0;
end;

function TdxFontFileOS2Table.GetUseTypoMetrics: Boolean;
begin
  Result := (Integer(FSelection) and Integer(TdxFontFileSelection.USE_TYPO_METRICS)) <> 0;
end;

procedure TdxFontFileOS2Table.SetWinAscent(const AValue: SmallInt);
begin
  FWinAscent := AValue;
  Changed;
end;

procedure TdxFontFileOS2Table.SetWinDescent(const AValue: SmallInt);
begin
  FWinDescent := AValue;
  Changed;
end;

{ TdxFontFileMaxpTable }

constructor TdxFontFileMaxpTable.Create(AGlyphCount: Integer);
var
  AData: TBytes;
begin
  SetLength(AData, 0);
  inherited Create(AData);
  DataStream.WriteInt($00005000);
  DataStream.WriteShort(AGlyphCount);
end;

class function TdxFontFileMaxpTable.Tag: string;
begin
  Result := 'maxp';
end;

function TdxFontFileMaxpTable.GetNumGlyphs: Integer;
begin
  DataStream.Position := NumGlyphsOffset;
  Result := DataStream.ReadUshort;
end;

procedure TdxFontFileMaxpTable.SetNumGlyphs(const AValue: Integer);
begin
  DataStream.Position := NumGlyphsOffset;
  DataStream.WriteShort(SmallInt(AValue));
end;


{ TdxFontFileBinaryTable }

constructor TdxFontFileBinaryTable.Create;
begin
  inherited Create;
  FDataStream := TdxFontFileStream.Create;
end;

constructor TdxFontFileBinaryTable.Create(const AData: TBytes);
begin
  Create;
  FDataStream.WriteArray(AData);
  FDataStream.Position := 0;
  Name := Tag;
end;

destructor TdxFontFileBinaryTable.Destroy;
begin
  FreeAndNil(FDataStream);
  inherited Destroy;
end;

class function TdxFontFileBinaryTable.Tag: string;
begin
  Result := '';
end;

function TdxFontFileBinaryTable.AlignedTableData: TBytes;
begin
  Result := FDataStream.ToAlignedArray;
end;

function TdxFontFileBinaryTable.GetDataSize: Integer;
begin
  Result := FDataStream.Size;
end;

function TdxFontFileBinaryTable.GetTableData: TBytes;
begin
  Result := FDataStream.Data;
end;

procedure TdxFontFileBinaryTable.CalculateCheckSum(var ACheckSum: Integer);
var
  I, AIndex, AElement, ACount: Integer;
  AData: TBytes;
begin
  AData := AlignedTableData;
  AIndex := 0;
  ACheckSum := 0;
  ACount := Length(AData) div 4;
  for I := 0 to ACount - 1 do
  begin
    AElement := AData[AIndex] shl 24;
    AElement := AElement + AData[AIndex + 1] shl 16;
    AElement := AElement + AData[AIndex + 2] shl 8;
    AElement := AElement + AData[AIndex + 3];
    Inc(ACheckSum, AElement);
    Inc(AIndex, 4);
  end;
end;

function TdxFontFileBinaryTable.Write(AStream: TdxFontFileStream; AOffset: Integer): Integer;
var
  ALength, AFactor, AAdditionalLength, ACheckSum: Integer;
begin
  ApplyChanges;
  ALength := DataSize;
  if ALength = 0 then
  begin
    DataStream.WriteInt(0);
    ALength := 4;
  end;
  AFactor := ALength mod 4;
  AAdditionalLength := 0;
  if AFactor <> 0 then
  begin
    AAdditionalLength := 4 - AFactor;
    DataStream.Position := ALength;
    DataStream.WriteEmptyArray(AAdditionalLength);
  end;
  AStream.WriteString(Name);
  CalculateCheckSum(ACheckSum);
  AStream.WriteInt(ACheckSum);
  AStream.WriteInt(AOffset);
  AStream.WriteInt(ALength);
  Result := ALength + AAdditionalLength;
end;

procedure TdxFontFileBinaryTable.DoApplyChanges;
begin
// do nothing
end;

procedure TdxFontFileBinaryTable.ApplyChanges;
begin
  if FNeedWrite then
  begin
    RecreateStream;
    DoApplyChanges;
  end;
end;

procedure TdxFontFileBinaryTable.Changed(AIsChanged: Boolean = True);
begin
  FNeedWrite := AIsChanged;
end;

procedure TdxFontFileBinaryTable.RecreateStream;
begin
  FDataStream.Free;
  FDataStream := TdxFontFileStream.Create;
end;

{ TdxFontFileLocaTable }

class function TdxFontFileLocaTable.Tag: string;
begin
  Result := 'loca';
end;

procedure TdxFontFileLocaTable.SetGlyphOffsets(const AValue: TIntegerDynArray);
begin
  FGlyphOffsets := AValue;
  Changed;
end;

procedure TdxFontFileLocaTable.ReadOffsets(AFontFile: TdxFontFile);
var
  AOffsetCount, AMaxpCount, I: Integer;
begin
  FIsShortFormat := (AFontFile.HeadTable <> nil) and (AFontFile.HeadTable.IndexToLocFormat = TdxFontFileIndexToLocFormat.Short);
  AOffsetCount := Length(Data) div IfThen(FIsShortFormat, 2, 4);
  if AOffsetCount > 1 then
  begin
    if AFontFile.MaxpTable <> nil then
    begin
      AMaxpCount := AFontFile.MaxpTable.NumGlyphs + 1;
      if AOffsetCount > AMaxpCount then
        AOffsetCount := AMaxpCount
      else
        if AOffsetCount < AMaxpCount then
          AFontFile.MaxpTable.NumGlyphs := AOffsetCount - 1;
    end;
    DataStream.Position := 0;
    SetLength(FGlyphOffsets, AOffsetCount);
    if FIsShortFormat then
      for I := 0 to AOffsetCount - 1 do
        FGlyphOffsets[I] := DataStream.ReadUshort * 2
    else
      for I := 0 to AOffsetCount - 1 do
        FGlyphOffsets[I] := DataStream.ReadInt;
  end
  else
    SetLength(FGlyphOffsets, 0);
end;

procedure TdxFontFileLocaTable.DoApplyChanges;
var
  ACount, I: Integer;
begin
  inherited DoApplyChanges;
  ACount := Length(FGlyphOffsets);
  if FIsShortFormat then
    for I := 0 to ACount - 1 do
      DataStream.WriteShort(FGlyphOffsets[I] div 2)
  else
    for I := 0 to ACount - 1 do
      DataStream.WriteInt(FGlyphOffsets[I]);
end;

{ TdxFontFileCFFTable }

class function TdxFontFileCFFTable.Tag: string;
begin
  Result := 'CFF ';
end;


{ TdxFontFileGlyphDescription }

class function TdxFontFileGlyphDescription.Create(AStream: TdxFontFileStream;
  AGlyphDataSize: Integer): TdxFontFileGlyphDescription;
var
  AGlyphStart: Int64;
  AFlags: Word;
begin
  SetLength(Result.GlyphIndexList, 0);

  AGlyphStart := AStream.Position;
  Result.NumberOfContours := AStream.ReadShort;
  Result.Data := AStream.ReadArray(AGlyphDataSize - 2);
  if Result.NumberOfContours < 0 then
  begin
    AStream.Position := AGlyphStart + HeaderSize;
    repeat
      AFlags := Word(AStream.ReadUshort);
      TdxPDFUtils.AddValue(AStream.ReadUshort, Result.GlyphIndexList);
      if (AFlags and ARG_1_AND_2_ARE_WORDS) <> 0 then
        AStream.Position := AStream.Position + 4
      else
        AStream.Position := AStream.Position + 2;
      if (AFlags and WE_HAVE_A_SCALE) <> 0 then
        AStream.Position := AStream.Position + 2
      else
        if (AFlags and WE_HAVE_AN_X_AND_Y_SCALE) <> 0 then
          AStream.Position := AStream.Position + 4
        else
          if (AFlags and WE_HAVE_A_TWO_BY_TWO) <> 0 then
            AStream.Position := AStream.Position + 8;
    until not ((AFlags and MORE_COMPONENTS) <> 0);
  end;
end;

function TdxFontFileGlyphDescription.GetSize: Integer;
begin
  Result := Length(Data) + 2;
end;

function TdxFontFileGlyphDescription.IsEmpty: Boolean;
begin
  Result := NumberOfContours = 0;
end;

procedure TdxFontFileGlyphDescription.Write(AStream: TdxFontFileStream);
begin
  AStream.WriteShort(NumberOfContours);
  AStream.WriteArray(Data);
end;

{ TdxFontFileGlyphTable }

constructor TdxFontFileGlyphTable.Create(const AData: TBytes);
begin
  inherited Create(AData);
  FGlyphs := TDictionary<Integer, TdxFontFileGlyphDescription>.Create;
  FSubsetGlyphs := nil;
end;

destructor TdxFontFileGlyphTable.Destroy;
begin
  FreeAndNil(FSubsetGlyphs);
  FreeAndNil(FGlyphs);
  inherited Destroy;
end;

class function TdxFontFileGlyphTable.Tag: string;
begin
  Result := 'glyf';
end;

function TdxFontFileGlyphTable.Pad4(AValue: Integer): Integer;
var
  ADiff: Integer;
begin
  ADiff := AValue mod 4;
  Result := IfThen(ADiff = 0, AValue, AValue + 4 - AValue mod 4);
end;

procedure TdxFontFileGlyphTable.SortSubsetGlyphs;
var
  AComparison: TComparison<TdxFontFileSubsetGlyph>;
begin
  AComparison :=
    function(const Left, Right: TdxFontFileSubsetGlyph): Integer
    begin
      Result := Left.Index - Right.Index;
    end;

  FSubsetGlyphs.Sort(TComparer<TdxFontFileSubsetGlyph>.Construct(AComparison));
end;

procedure TdxFontFileGlyphTable.CreateSubset(AFontFile: TdxFontFile; AMapping: TDictionary<Integer, string>);

  function IndexExists(AIndex: integer): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to FSubsetGlyphs.Count - 1 do
    begin
      Result := FSubsetGlyphs[I].Index = AIndex;
      if Result then
        Break;
    end;
  end;

var
  AGlyph: TdxFontFileGlyphDescription;
  AGlyphIndex, AIndex: Integer;
  ALoca: TdxFontFileLocaTable;
  ASubsetGlyph, ATempSubsetGlyph: TdxFontFileSubsetGlyph;
begin
  if FSubsetGlyphs = nil then
    FSubsetGlyphs := TList<TdxFontFileSubsetGlyph>.Create;
  FSubsetGlyphs.Clear;

  for AGlyphIndex in AMapping.Keys do
    if FGlyphs.TryGetValue(AGlyphIndex, AGlyph) then
    begin
      for AIndex in AGlyph.GlyphIndexList do
        if not IndexExists(AIndex) and FGlyphs.ContainsKey(AIndex) then
        begin
          ASubsetGlyph.Index := AIndex;
          ASubsetGlyph.Description := FGlyphs[AIndex];
          FSubsetGlyphs.Add(ASubsetGlyph);
        end;
      ATempSubsetGlyph.Index := AGlyphIndex;
      ATempSubsetGlyph.Description := AGlyph;
      FSubsetGlyphs.Add(ATempSubsetGlyph);
    end;

  SortSubsetGlyphs;

  FGlyphOffsets := CalculateOffsets(Length(FGlyphOffsets) - 1);

  ALoca := AFontFile.LocaTable;
  if ALoca <> nil then
    ALoca.GlyphOffsets := FGlyphOffsets;
  Changed;
end;

procedure TdxFontFileGlyphTable.ReadGlyphs(AFontFile: TdxFontFile);
var
  AStreamLength, ACount, ANextOffset, ANextOffsetIndex, AOffset, ACurrentOffset, ASize, AGlyphOffset, AIndex, I, ANextGlyphOffset: Integer;
  AOffsets: TList<Integer>;
  AGlyphDescriptions: TDictionary<Integer, TdxFontFileGlyphDescription>;
  AGlyph: TdxFontFileGlyphDescription;
  AKey: Integer;
  ATemp: TdxFontFileSubsetGlyph;
  AValue: TdxFontFileGlyphDescription;
begin
  FGlyphs.Clear;
  if AFontFile.LocaTable <> nil then
  begin
    DataStream.Position := 0;
    AStreamLength := DataSize;
    FGlyphOffsets := AFontFile.LocaTable.GlyphOffsets;
    ACount := Length(FGlyphOffsets) - 1;

    AOffsets := TList<Integer>.Create;
    AOffsets.AddRange(FGlyphOffsets);
    AOffsets.Sort;
    try
      AGlyphDescriptions := TDictionary<Integer, TdxFontFileGlyphDescription>.Create;
      try
        ANextOffset := AOffsets[0];
        if AOffsets[0] <> FGlyphOffsets[0] then
          Changed;

        ANextOffsetIndex := 1;
        for I := 0 to ACount - 1 do
        begin
          AOffset := ANextOffset;
          ACurrentOffset := FGlyphOffsets[ANextOffsetIndex];
          ANextOffset := Min(AOffsets[ANextOffsetIndex], AStreamLength);
          Inc(ANextOffsetIndex);
          if ANextOffset <> ACurrentOffset then
            Changed;
          ASize := ANextOffset - AOffset;
          if ASize >= TdxFontFileGlyphDescription.HeaderSize then
          begin
            DataStream.Position := AOffset;
            AGlyph := TdxFontFileGlyphDescription.Create(DataStream, ASize);
            if AGlyph.IsEmpty then
              Changed
            else
              AGlyphDescriptions.Add(AOffset, AGlyph);
          end
          else
            if ASize <> 0 then
            begin
              ANextOffset := AOffset;
              Changed;
            end;
        end;

        AGlyphOffset := FGlyphOffsets[0];

        AIndex := 0;
        for I := 1 to ACount do
        begin
          ANextGlyphOffset := FGlyphOffsets[I];
          if (ANextGlyphOffset - AGlyphOffset <> 0) and AGlyphDescriptions.TryGetValue(FGlyphOffsets[AIndex], AGlyph) then
            FGlyphs.Add(AIndex, AGlyph);
          AGlyphOffset := ANextGlyphOffset;
          Inc(AIndex);
        end;
      finally
        AGlyphDescriptions.Free;
      end;
    finally
      AOffsets.Free;
    end;

    FSubsetGlyphs := TList<TdxFontFileSubsetGlyph>.Create;
    for AKey in FGlyphs.Keys do
      if FGlyphs.TryGetValue(AKey, AValue) then
      begin
        ATemp.Index := AKey;
        ATemp.Description := AValue;
        FSubsetGlyphs.Add(ATemp);
      end;

    SortSubsetGlyphs;

    FGlyphOffsets := CalculateOffsets(ACount);
    if NeedWrite then
    begin
      AFontFile.LocaTable.GlyphOffsets := FGlyphOffsets;
      if AFontFile.MaxpTable <> nil then
        AFontFile.MaxpTable.NumGlyphs := SmallInt(ACount);
      Changed;
    end;
  end;
end;

function TdxFontFileGlyphTable.CalculateOffsets(AGlyphCount: Integer): TIntegerDynArray;
var
  I, AIndex, AOffset, AOffsetIndex, AGlyphIndex: Integer;
  ASubsetGlyph: TdxFontFileSubsetGlyph;
begin
  SetLength(Result, AGlyphCount + 1);
  AOffset := 0;
  AOffsetIndex := 0;
  for AIndex := 0 to FSubsetGlyphs.Count - 1 do
  begin
    ASubsetGlyph := FSubsetGlyphs[AIndex];
    AGlyphIndex := ASubsetGlyph.Index;
    for I := AOffsetIndex to AGlyphIndex do
    begin
      Result[I] := AOffset;
      Inc(AOffsetIndex);
    end;
    Inc(AOffset, Pad4(ASubsetGlyph.Description.Size));
  end;
  for I := AOffsetIndex to AGlyphCount do
    Result[I] := AOffset;
end;


procedure TdxFontFileGlyphTable.DoApplyChanges;
var
  I, APos, AEnd: Integer;
  ASubsetGlyph: TdxFontFileSubsetGlyph;
begin
  inherited DoApplyChanges;
  for ASubsetGlyph in FSubsetGlyphs do
  begin
    DataStream.Position := FGlyphOffsets[ASubsetGlyph.Index];
    ASubsetGlyph.Description.Write(DataStream);
    APos := Integer(DataStream.Position);
    AEnd := Pad4(APos);
    for I := APos to AEnd - 1 do
      DataStream.WriteByte(0);
  end;
end;

{ TdxFontFile }

constructor TdxFontFile.Create(AStream: TdxFontFileStream);
begin
  Create(AStream.Data);
end;

constructor TdxFontFile.Create(const AData: TBytes; AIsOpenType: Boolean = False);
var
  AStream: TdxFontFileStream;
begin
  inherited Create;
  SetLength(FVersion, 4);
  if AIsOpenType then
  begin
    FVersion[0] := OpenTypeVersion[0];
    FVersion[1] := OpenTypeVersion[1];
    FVersion[2] := OpenTypeVersion[2];
    FVersion[3] := OpenTypeVersion[3];
  end
  else
  begin
    FVersion[0] := TrueTypeVersion[0];
    FVersion[1] := TrueTypeVersion[1];
    FVersion[2] := TrueTypeVersion[2];
    FVersion[3] := TrueTypeVersion[3];
  end;
  FTableDictionary := TObjectDictionary<string, TdxFontFileBinaryTable>.Create([doOwnsValues]);
  if Length(AData) > 0 then
  begin
    AStream := TdxFontFileStream.Create(AData);
    try
      ReadTables(AStream);
    finally
      AStream.Free;
    end;
  end;
end;

destructor TdxFontFile.Destroy;
begin
  FreeAndNil(FTableDictionary);
  inherited Destroy;
end;

class function TdxFontFile.GetCFFData(const AFontFileData: TBytes): TBytes;
var
  ALength: Integer;
  AFile: TdxFontFile;
  ACFF: TdxFontFileBinaryTable;
begin
  Result := nil;
  ALength := Length(AFontFileData);
  if (ALength >= Length(OpenTypeVersion)) and (AFontFileData[0] = OpenTypeVersion[0]) and
    (AFontFileData[1] = OpenTypeVersion[1]) and (AFontFileData[2] = OpenTypeVersion[2]) and
    (AFontFileData[3] = OpenTypeVersion[3]) then
  begin
    AFile := TdxFontFile.Create(AFontFileData, True);
    try
      if (AFile <> nil) and AFile.Table.TryGetValue(TdxFontFileCFFTable.Tag, ACFF) then
        Result := ACFF.Data;
    finally
      AFile.Free;
    end;
  end;
end;

class function TdxFontFile.IsEqual(AFontFile1, AFontFile2: TdxFontFile): Boolean;
begin
  if (AFontFile1 = nil) and (AFontFile2 = nil) then
    Exit(False);
  if ((AFontFile1 = nil) and (AFontFile2 <> nil)) or ((AFontFile1 <> nil) and (AFontFile2 = nil)) then
    Exit(False);
  Result := AFontFile1.InitalFontSize = AFontFile2.InitalFontSize;
end;

function TdxFontFile.CreateSubset(AMapping: TDictionary<Integer, string>): TBytes;
var
  AList: TStringList;
begin
  SetLength(Result, 0);
  if GlyphTable <> nil then
    GlyphTable.CreateSubset(Self, AMapping);
  AList := TStringList.Create;
  try
    AList.Add(TdxFontFileHeadTable.Tag);
    AList.Add(TdxFontFileHheaTable.Tag);
    AList.Add(TdxFontFileMaxpTable.Tag);
    AList.Add(TdxFontFileHmtxTable.Tag);
    AList.Add(TdxFontFileGlyphTable.Tag);
    AList.Add(TdxFontFileLocaTable.Tag);
    AList.Add('cvt ');
    AList.Add('fpgm');
    AList.Add('prep');
    Result := InternalGetData(AList);
  finally
    AList.Free;
  end;
end;

function TdxFontFile.GetCharacterWidth(AGlyphIndex: Integer): Single;
var
  AGlyphCount: Integer;
begin
  if HmtxTable = nil then
    Exit(0);
  if HmtxTable.AdvanceWidths = nil then
  begin
    if HheaTable = nil then
      Exit(0);
    if MaxpTable = nil then
      AGlyphCount := 0
    else
      AGlyphCount := MaxpTable.NumGlyphs;
    HmtxTable.AdvanceWidths := HmtxTable.FillAdvanceWidths(HheaTable.NumberOfHMetrics, AGlyphCount);
  end;
  if AGlyphIndex < Length(HmtxTable.AdvanceWidths) then
    Result := HmtxTable.AdvanceWidths[AGlyphIndex] * FTTFToFontFileFactor
  else
    Result := 0;
end;

function TdxFontFile.GetData: TBytes;
var
  AKey: string;
  AList: TStringList;
begin
  AList := TStringList.Create;
  try
    for AKey in Table.Keys do
      AList.Add(AKey);
    Result := InternalGetData(AList);
  finally
    AList.Free;
  end;
end;

function TdxFontFile.IsTrueTypeFont: Boolean;
begin
  Result := GlyphTable <> nil;
end;

procedure TdxFontFile.AddTable(ATable: TdxFontFileBinaryTable);
begin
  FTableDictionary.Add(ATable.Name, ATable);
end;

function TdxFontFile.GetHeadTable: TdxFontFileHeadTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if Table.TryGetValue(TdxFontFileHeadTable.Tag, AValue) then
    Result := AValue as TdxFontFileHeadTable
  else
    Result := nil;
end;

function TdxFontFile.GetMaxpTable: TdxFontFileMaxpTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if Table.TryGetValue(TdxFontFileMaxpTable.Tag, AValue) then
    Result := AValue as TdxFontFileMaxpTable
  else
    Result := nil;
end;

function TdxFontFile.GetOS2Table: TdxFontFileOS2Table;
var
  AValue: TdxFontFileBinaryTable;
begin
  if Table.TryGetValue(TdxFontFileOS2Table.Tag, AValue) then
    Result := AValue as TdxFontFileOS2Table
  else
    Result := nil;
end;

function TdxFontFile.GetPostTable: TdxFontFilePostTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if Table.TryGetValue(TdxFontFilePostTable.Tag, AValue) then
    Result := AValue as TdxFontFilePostTable
  else
    Result := nil;
end;

function TdxFontFile.GetNameTable: TdxFontFileNameTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if Table.TryGetValue(TdxFontFileNameTable.Tag, AValue) then
    Result := AValue as TdxFontFileNameTable
  else
    Result := nil;
end;

function TdxFontFile.GetLocaTable: TdxFontFileLocaTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if Table.TryGetValue(TdxFontFileLocaTable.Tag, AValue) then
    Result := AValue as TdxFontFileLocaTable
  else
    Result := nil;
end;

function TdxFontFile.GetGlyphTable: TdxFontFileGlyphTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if Table.TryGetValue(TdxFontFileGlyphTable.Tag, AValue) then
    Result := AValue as TdxFontFileGlyphTable
  else
    Result := nil;
end;

function TdxFontFile.GetHheaTable: TdxFontFileHheaTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if FHhea = nil then
    if Table.TryGetValue(TdxFontFileHheaTable.Tag, AValue) then
      FHhea := AValue as TdxFontFileHheaTable
    else
      FHhea := nil;
  Result := FHhea;
end;

function TdxFontFile.GetCMapTable: TdxFontFileCMapTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if FCMap = nil then
    if Table.TryGetValue(TdxFontFileCMapTable.Tag, AValue) then
      FCMap := AValue as TdxFontFileCMapTable
    else
      FCMap := nil;
  Result := FCMap;
end;

function TdxFontFile.GetKernTable: TdxFontFileKernTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if FKern = nil then
    if Table.TryGetValue(TdxFontFileKernTable.Tag, AValue) then
      FKern := AValue as TdxFontFileKernTable
    else
      FKern := nil;
  Result := FKern;
end;

function TdxFontFile.GetHmtxTable: TdxFontFileHmtxTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if FHmtx = nil then
    if Table.TryGetValue(TdxFontFileHmtxTable.Tag, AValue) then
      FHmtx := AValue as TdxFontFileHmtxTable
    else
      FHmtx := nil;
  Result := FHmtx;
end;

procedure TdxFontFile.ReadTables(AStream: TdxFontFileStream);
var
  AStartOffset, AStreamLength, AOffset, ACurrentPosition: Int64;
  ANumTables, I: SmallInt;
  ATag: string;
  ALength: Integer;
begin
  FInitalFontSize := AStream.Size;
  AStartOffset := AStream.Position;
  AStream.ReadInt;
  ANumTables := AStream.ReadShort;
  AStream.ReadShort;
  AStream.ReadShort;
  AStream.Position := AStartOffset + TableDirectoryOffset;
  AStreamLength := AStream.Size;
  for I := 0 to ANumTables - 1 do
  begin
    ATag := AStream.ReadString(4);
    AStream.ReadInt;
    AOffset := AStream.ReadInt;
    ALength := AStream.ReadInt;
    if (ATag = TdxFontFileGlyphTable.Tag) and (ALength = 0) then
      ALength := AStreamLength - AOffset;
    if (AOffset > 0) and (ALength > 0) and (AOffset + ALength <= AStreamLength) and (ATag <> 'EBLC') then
    begin
      ACurrentPosition := AStream.Position;
      AStream.Position := AOffset;
      AddTable(CreateTable(ATag, AStream.ReadArray(ALength)));
      AStream.Position := ACurrentPosition;
    end;
  end;
  if HeadTable <> nil then
    FTTFToFontFileFactor := 1000 / HeadTable.UnitsPerEm;
  if LocaTable <> nil then
    LocaTable.ReadOffsets(Self);
  if GlyphTable <> nil then
    GlyphTable.ReadGlyphs(Self);
end;

function TdxFontFile.CreateTable(const ATag: string; const AArray: TBytes): TdxFontFileBinaryTable;
var
  AClass: TdxFontFileTableClass;
begin
  if (Length(AArray) = 0) or not dxgFontFileSupportedTables.TryGetValue(ATag, AClass) then
    AClass := TdxFontFileBinaryTable;
  Result := AClass.Create(AArray);
  Result.Name := ATag;
end;

function TdxFontFile.InternalGetData(ATablesToWrite: TStringList): TBytes;
var
  I, ANumTables, ASearchRange: SmallInt;
  AEntry: TPair<string, TdxFontFileBinaryTable>;
  AEntryOffset: Integer;
  AStream: TdxFontFileStream;
  ACurrentTable: TdxFontFileBinaryTable;
begin
  ATablesToWrite.Sort;
  ANumTables := 0;
  for AEntry in FTableDictionary do
    if ATablesToWrite.IndexOf(AEntry.Key) <> -1 then
      Inc(ANumTables);

  AEntryOffset := TableDirectoryOffset + ANumTables * 16;
  AStream := TdxFontFileStream.Create;
  try
    AStream.WriteArray(FVersion);
    AStream.WriteShort(ANumTables);
    ASearchRange := Trunc(Power(2, Floor(Log2(ANumTables))));
    AStream.WriteShort(ASearchRange * 16);
    AStream.WriteShort(Trunc(Log2(ANumTables)));
    AStream.WriteShort(SmallInt((ANumTables * 16 - ASearchRange * 16)));

    for I := 0 to ATablesToWrite.Count - 1 do
      if FTableDictionary.TryGetValue(ATablesToWrite[I], ACurrentTable) then
        Inc(AEntryOffset, ACurrentTable.Write(AStream, AEntryOffset));

    for I := 0 to ATablesToWrite.Count - 1 do
      if FTableDictionary.TryGetValue(ATablesToWrite[I], ACurrentTable) then
        AStream.WriteArray(ACurrentTable.AlignedTableData);

    AStream.Position := 0;
    Result := AStream.ReadArray(Integer(AStream.Size));
  finally
    AStream.Free;
  end;
end;

{ TdxFontFileFontMetrics }

class function TdxFontFileFontMetrics.Create(AFontFile: TdxFontFile): TdxFontFileFontMetrics;
var
  AUnitsPerEm, AFactor: Double;
  AHead: TdxFontFileHeadTable;
  AOs2: TdxFontFileOS2Table;
  AHhea: TdxFontFileHheaTable;
begin
  AHead := AFontFile.HeadTable;
  if AHead = nil then
  begin
    AUnitsPerEm := 2048;
    Result.FEmBBox := TdxRectF.CreateSize(0, 0, 0, 0);
  end
  else
  begin
    AUnitsPerEm := AHead.UnitsPerEm;
    AFactor := 1000 / AUnitsPerEm;
    Result.FEmBBox := TdxRectF.CreateSize(Round(AHead.XMin * AFactor), Round(AHead.YMin * AFactor),
      Round(AHead.XMax * AFactor), Round(AHead.YMax * AFactor));
  end;
  AOs2 := AFontFile.OS2Table;
  AHhea := AFontFile.HheaTable;
  if AOs2 <> nil then
  begin
    if AOs2.UseTypoMetrics then
    begin
      Result.FAscent := AOs2.TypoAscender / AUnitsPerEm;
      Result.FDescent := -AOs2.TypoDescender / AUnitsPerEm;
      Result.FLineSpacing := Result.FAscent + Result.FDescent + AOs2.TypoLineGap / AUnitsPerEm;
    end
    else
    begin
      Result.FAscent := AOs2.WinAscent / AUnitsPerEm;
      Result.FDescent := AOs2.WinDescent / AUnitsPerEm;
      Result.FLineSpacing := Result.FAscent + Result.FDescent;
      if AHhea <> nil then
        Result.FLineSpacing := Result.FLineSpacing + TdxPDFUtils.Max(0,
          (AHhea.LineGap - ((AOs2.WinAscent + AOs2.WinDescent) - (AHhea.Ascender - AHhea.Descender))) / AUnitsPerEm);
    end;
  end
  else
    if AHhea <> nil then
    begin
      Result.FAscent := AHhea.Ascender / AUnitsPerEm;
      Result.FDescent := -AHhea.Descender / AUnitsPerEm;
      Result.FLineSpacing := Result.FAscent + Result.FDescent + AHhea.LineGap / AUnitsPerEm;
    end;
end;

class function TdxFontFileFontMetrics.Create(AAscent, ADescent, ALineSpacing, AUnitsPerEm: Double): TdxFontFileFontMetrics;
begin
  Result.FAscent := AAscent / AUnitsPerEm;
  Result.FDescent := ADescent / AUnitsPerEm;
  Result.FLineSpacing := ALineSpacing / AUnitsPerEm;
  Result.FEmBBox := TdxRectF.CreateSize(0, 0, 0, 0);
end;

function TdxFontFileFontMetrics.GetEmAscent: Double;
begin
  Result := FAscent * 1000;
end;

function TdxFontFileFontMetrics.GetEmDescent: Double;
begin
  Result := FDescent * 1000;
end;

function TdxFontFileFontMetrics.GetAscent(AFontSize: Double): Double;
begin
  Result := FAscent * AFontSize;
end;

function TdxFontFileFontMetrics.GetDescent(AFontSize: Double): Double;
begin
  Result := FDescent * AFontSize;
end;

function TdxFontFileFontMetrics.GetLineSpacing(AFontSize: Double): Double;
begin
  Result := FLineSpacing * AFontSize;
end;

initialization
  dxgFontFileUnicodeConverter := TdxFontFileUnicodeConverter.Create;
  dxgFontFileSupportedTables := TDictionary<string, TdxFontFileTableClass>.Create;

  dxgFontFileSupportedTables.Add(TdxFontFileCMapTable.Tag, TdxFontFileCMapTable);
  dxgFontFileSupportedTables.Add(TdxFontFileCFFTable.Tag, TdxFontFileCFFTable);
  dxgFontFileSupportedTables.Add(TdxFontFileGlyphTable.Tag, TdxFontFileGlyphTable);
  dxgFontFileSupportedTables.Add(TdxFontFileHeadTable.Tag, TdxFontFileHeadTable);
  dxgFontFileSupportedTables.Add(TdxFontFileHheaTable.Tag, TdxFontFileHheaTable);
  dxgFontFileSupportedTables.Add(TdxFontFileHmtxTable.Tag, TdxFontFileHmtxTable);
  dxgFontFileSupportedTables.Add(TdxFontFileLocaTable.Tag, TdxFontFileLocaTable);
  dxgFontFileSupportedTables.Add(TdxFontFileKernTable.Tag, TdxFontFileKernTable);
  dxgFontFileSupportedTables.Add(TdxFontFileMaxpTable.Tag, TdxFontFileMaxpTable);
  dxgFontFileSupportedTables.Add(TdxFontFileNameTable.Tag, TdxFontFileNameTable);
  dxgFontFileSupportedTables.Add(TdxFontFileOS2Table.Tag, TdxFontFileOS2Table);
  dxgFontFileSupportedTables.Add(TdxFontFilePostTable.Tag, TdxFontFilePostTable);

finalization
  FreeAndNil(dxgFontFileUnicodeConverter);
  FreeAndNil(dxgFontFileSupportedTables);

end.

