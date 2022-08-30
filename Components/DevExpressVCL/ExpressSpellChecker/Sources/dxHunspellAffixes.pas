{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpellChecker                                      }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPELLCHECKER AND ALL           }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxHunspellAffixes;

{$I cxVer.inc}

interface

uses
  StrUtils, Math, SysUtils, Classes, cxClasses, dxHunspellUtils, dxHunspellWords,
  dxHunspellTypes, dxCore;

const
  dxAffixTableSize = $10000;
  dxMaxFlagCount = $10000;
  dxPhoneHashSize = $10000;
  dxMaxPhoneticsLength = 256;

type
  TdxAffixReaderList = class;

  TdxAffixOption = (aoPrefixSuffixUnion, aoAffixFlagAliasTable, aoAffixMorphologyTable,
    aoCompoundMiddle, aoCompoundEnd, aoInCompoundOnly, aoNeedAffix, aoCompoundForbid,
    aoCompoundPermit, aoCircumfix);
  TdxAffixOptions = set of TdxAffixOption;

  TdxExistentAffixFlags = array[0..dxMaxFlagCount - 1] of Byte;

  TdxStringData = record
    Data: PWideChar;
    Length: Integer;
  end;

  TdxReplaceTableItem = packed record
    Text: PWideChar;
    Replacement: PWideChar;
  end;
  PdxReplaceArray = ^TdxReplaceArray;
  TdxReplaceArray = array[0..0] of TdxReplaceTableItem;

  TdxMapTableItem = packed record
    CharacterSet: PWideChar;
    Length: Integer;
  end;
  PdxMapArray = ^TdxMapArray;
  TdxMapArray = array[0..0] of TdxMapTableItem;

  TdxPhoneHashArray = array[0..dxPhoneHashSize - 1] of Integer;

  PdxGuessWord = ^TdxGuessWord;
  TdxGuessWord = packed record
    Word: PWideChar;
    Orig: PWideChar;
    Allow: Boolean;
  end;
  PdxGuessWordArray = ^TdxGuessWordArray;
  TdxGuessWordArray = array[0..0] of TdxGuessWord;

  TdxCompoundForbidPattern = record
    Pattern: PWideChar;
    Pattern2: PWideChar;
    Pattern3: PWideChar;
    Condition: Word;
    Condition2: Word;
  end;
  PdxCompoundForbidPatternArray = ^TdxCompoundForbidPatternArray;
  TdxCompoundForbidPatternArray = array[0..0] of TdxCompoundForbidPattern;

  TdxHunspellAffixManager = class;
  TdxAffix = class;
  TdxSuffix = class;

  TdxAffixType = (atPrefix, atSuffix);

  TdxCompoundWordPart = (cwpNone, cwpFirst, cwpLast, cwpOther);

  TAffixTable = array [0..dxAffixTableSize - 1] of TdxAffix;

  TWordFormArray = array [0..dxMaxWordLength + 4 - 1] of WideChar;

  TAffixManagerFlagArray = array[0..dxMaxFlagCount - 1] of Boolean;

  PdxAffixItem = ^TdxAffixItem;
  TdxAffixItem = packed record
    StripString: PWideChar;
    StripStringLength: Byte;
    AppendString: PWideChar;
    AppendStringLength: Byte;
    ConditionLength: Shortint;
    CompatibleFlags: TdxHunspellFlags;
    MorphologicalDescription: PWideChar;
    Condition: PWideChar;
  end;
  PdxAffixItemArray = ^TdxAffixItemArray;
  TdxAffixItemArray = array[0..0] of TdxAffixItem;

  PdxCompoundFlag = ^TdxCompoundFlag;
  TdxCompoundFlag = record
    Pattern: PdxAffixFlagsData;
    Count: Integer;
    Length: Integer;
  end;
  PdxCompoundFlagArray = ^TdxCompoundFlagArray;
  TdxCompoundFlagArray = array[0..0] of TdxCompoundFlag;

  TdxCompoundWordRules = array[0..dxMaxWordLength] of TdxHunspellWordStem;

  { TdxCustomHunspellDataTable }

  TdxCustomHunspellDataTable = class
  private
    FCurrentDataIndex: Integer;
    FDataIdentifier: PWideChar;
    FCachedIdentifier: PWideChar;
    FIdentifierLength: Integer;
    FSize: Integer;
    procedure FreeData;
  protected
    FData: Pointer;
    function AddDataPiece(const APiece: PWideChar; ADataIndex,
      APieceIndex: Integer): Boolean; virtual;
    procedure AfterReadData; virtual;
    function IsAllPiecesFound(APieceIndex: Integer): Boolean; virtual;
    function CheckDataType(const APiece: PWideChar): Boolean;
    procedure FreeDataItems; virtual; abstract;
    function GetCount: Integer; virtual;
    function GetDataIdentifier: PWideChar; virtual; abstract;
    function GetDataItemSize: Integer; virtual; abstract;
    function GetPieceQuantity: Integer; virtual; abstract;
    procedure InitializeItem(Index: Integer); virtual;
    function ReadDataHeader(ALine: PWideChar): Boolean; virtual;
    function ParseHeader(ALine: PWideChar; out ATableSize: Integer): Boolean; virtual;
    function ParseData(AAffixReader: TdxHunspellReader): Boolean;
  public
    constructor Create; overload;
    constructor Create(ADataIdentifier: PWideChar); overload;
    destructor Destroy; override;
    procedure AllocateData(ASize: Integer);
    function ReadData(ALine: PWideChar; AAffixFileReader: TdxHunspellReader): Boolean;
    property Count: Integer read GetCount;
  end;

  { TdxSpellCheckerDataLinkedTable }

  TdxHunspellDataLinkedTable = class(TdxCustomHunspellDataTable)
  private
    FAffixManager: TdxHunspellAffixManager;
    function GetWordStemManager: TdxHunspellWordStemManager; inline;
  public
    constructor Create(AAffixManager: TdxHunspellAffixManager);
    property AffixManager: TdxHunspellAffixManager read FAffixManager;
    property WordStemManager: TdxHunspellWordStemManager read GetWordStemManager;
  end;

  { TdxAffixItemTable }

  TdxAffixItemTable = class(TdxHunspellDataLinkedTable)
  private
    FAffixesIsCreated: Boolean;
    FDecodedFlag: Word;
    FFlag: PWideChar;
    FPrefixSuffixUnion: Boolean;
    procedure AddLineRemainderToMorphologicDescription(ALine: PWideChar; ADataIndex: Integer);
    function CheckFlag(ALine: PWideChar): Boolean;
    function ConditionLength(ALine: PWideChar): Integer;
    function EncodeAffixCondition(ALine: PWideChar; ADataIndex: Integer): Boolean;
    function HasAffixFlagAliases: Boolean;
    function HasAffixMorphologicAliases: Boolean;
    function FlagIsNil: Boolean;
    procedure FreeIfZero(var AString: TdxStringData);
    function GetData: PdxAffixItemArray; inline;
    function GetItem(Index: Integer): TdxAffixItem; inline;
    function GetOptions(const AItem: TdxAffixItem): TdxAffixOptions;
    function IsReverseWritingDirection: Boolean;
    procedure ParseAppendString(const ALine: PWideChar; ADataIndex: Integer);
    function ParseAppendStringWithAffixes(const ALine: PWideChar; ADataIndex: Integer): Boolean;
    function ParseCompatibleFlags(const ALine: PWideChar; ADataIndex: Integer): Boolean;
    function ParseCondition(ALine: PWideChar; ADataIndex: Integer): Boolean;
    function ParseFlag(ALine: PWideChar): Boolean;
    function ParseMorphologicDescription(ALine: PWideChar; ADataIndex: Integer): Boolean;
    procedure ParsePrefixSuffixUnion(ALine: PWideChar);
    procedure ParseStripString(const ALine: PWideChar; ADataIndex: Integer);
    function ParseTableSize(ALine: PWideChar; out ATableSize: Integer): Boolean;
    procedure ProcessReverseString(ALine: PWideChar);
    procedure RegisterCompatibleFlags(ADataIndex: Integer);
    procedure ReverseCondition(ACondition: PWideChar);
  protected
    function AddDataPiece(const APiece: PWideChar; ADataIndex,
      APieceIndex: Integer): Boolean; override;
    function IsAllPiecesFound(APieceIndex: Integer): Boolean; override;
    function IsComplexPrefixes: Boolean;
    procedure BuildAffixes; virtual;
    function ConditionContainsDuplicateInformation(AStripString: PWideChar; AStripStringLen: Integer;
      const ACondition: PWideChar; out Contains: Boolean): Boolean; virtual; abstract;
    function GetDataIdentifier: PWideChar; override;
    function GetDataItemSize: Integer; override;
    function GetPieceQuantity: Integer; override;
    procedure FreeDataItems; override;
    function IsPrefix: Boolean; virtual; abstract;
    function ParseHeader(ALine: PWideChar; out ATableSize: Integer): Boolean; override;

    property Data: PdxAffixItemArray read GetData;
    property Items[Index: Integer]: TdxAffixItem read GetItem; default;
  public
    destructor Destroy; override;
    property Flag: PWideChar read FFlag;
  end;

  TdxAffixItemTableClass = class of TdxAffixItemTable;

  { TdxPrefixItemTable }

  TdxPrefixItemTable = class(TdxAffixItemTable)
  protected
    function ConditionContainsDuplicateInformation(AStripString: PWideChar; AStripStringLen: Integer;
      const ACondition: PWideChar; out AContains: Boolean): Boolean; override;
    function IsPrefix: Boolean; override;
  public
    procedure BuildAffixes; override;
  end;

  { TdxSuffixItemTable }

  TdxSuffixItemTable = class(TdxAffixItemTable)
  protected
    function ConditionContainsDuplicateInformation(AStripString: PWideChar;
      AStripStringLength: Integer; const ACondition: PWideChar; out Contains: Boolean): Boolean; override;
    function IsPrefix: Boolean; override;
  public
    procedure BuildAffixes; override;
  end;

  { TdxBreakTable }

  TdxBreakTable = class(TdxCustomHunspellDataTable)
  private
    procedure FillDefaultData;
    function GetData: PdxPWideCharArray; inline;
    function GetItem(Index: Integer): PWideChar; inline;
  protected
    function AddDataPiece(const APiece: PWideChar; ADataIndex,
      APieceIndex: Integer): Boolean; override;
    procedure FreeDataItems; override;
    function GetDataIdentifier: PWideChar; override;
    function GetDataItemSize: Integer; override;
    function GetPieceQuantity: Integer; override;
  public
    constructor Create;
    property Data: PdxPWideCharArray read GetData;
    property Items[Index: Integer]: PWideChar read GetItem; default;
  end;

  { TdxCheckCompoundPatternTable }

  TdxCheckCompoundPatternTable = class(TdxHunspellDataLinkedTable)
  private
    FIsSimplified: Boolean;
    procedure ExtractCondition(APattern: PWideChar; var ACondition: Word);
    function GetData: PdxCompoundForbidPatternArray; inline;
    function GetItem(Index: Integer): TdxCompoundForbidPattern; inline;
  protected
    function AddDataPiece(const APiece: PWideChar; ADataIndex,
      APieceIndex: Integer): Boolean; override;
    procedure FreeDataItems; override;
    function GetDataIdentifier: PWideChar; override;
    function GetDataItemSize: Integer; override;
    function GetPieceQuantity: Integer; override;
    procedure InitializeItem(Index: Integer); override;
  public
    property Data: PdxCompoundForbidPatternArray read GetData;
    property IsSimplified: Boolean read FIsSimplified;
    property Items[Index: Integer]: TdxCompoundForbidPattern read GetItem; default;
  end;

  { TdxCompoundRuleTable}

  TdxCompoundRuleTable  = class(TdxHunspellDataLinkedTable)
  private
    function GetData: PdxCompoundFlagArray; inline;
    function GetItem(Index: Integer): TdxCompoundFlag; inline;
    function ParseFlags(APiece: PWideChar; AIndex: Integer): Boolean;
  protected
    function AddDataPiece(const APiece: PWideChar; ADataIndex,
      APieceIndex: Integer): Boolean; override;
    procedure FreeDataItems; override;
    function GetDataIdentifier: PWideChar; override;
    function GetDataItemSize: Integer; override;
    function GetPieceQuantity: Integer; override;
    procedure InitializeItem(Index: Integer); override;
  public
    function IsPartOfCompoundWordFlag(AFlag: PWideChar): Boolean; overload;
    function IsPartOfCompoundWordFlag(AFlag: Word): Boolean; overload;
    property Data: PdxCompoundFlagArray read GetData;
    property Items[Index: Integer]: TdxCompoundFlag read GetItem; default;
  end;

  { TdxMapTable }

  TdxMapTable = class(TdxCustomHunspellDataTable)
  private
    function GetData: PdxMapArray; inline;
    function GetItem(Index: Integer): TdxMapTableItem; inline;
  protected
    function AddDataPiece(const APiece: PWideChar; ADataIndex,
      APieceIndex: Integer): Boolean; override;
    procedure FreeDataItems; override;
    function GetDataIdentifier: PWideChar; override;
    function GetDataItemSize: Integer; override;
    function GetPieceQuantity: Integer; override;
    procedure InitializeItem(Index: Integer); override;
  public
    property Data: PdxMapArray read GetData;
    property Items[Index: Integer]: TdxMapTableItem read GetItem; default;
  end;

  { TdxPhoneTable }

  TdxPhoneTable = class(TdxCustomHunspellDataTable)
  private
    FCount: Integer;
    FRules: PdxPWideCharArray;
    FHash: TdxPhoneHashArray;
    procedure InitializeHash;
  protected
    function AddDataPiece(const APiece: PWideChar; ADataIndex,
      APieceIndex: Integer): Boolean; override;
    procedure AfterReadData; override;
    procedure FreeDataItems; override;
    procedure FreeRules;
    function GetCount: Integer; override;
    function GetDataItemSize: Integer; override;
    function GetDataIdentifier: PWideChar; override;
    function GetPieceQuantity: Integer; override;
    function ReadDataHeader(ALine: PWideChar): Boolean; override;

    property Rules: PdxPWideCharArray read FRules;
  public
    destructor Destroy; override;
    function Phonetic(const AWord: PWideChar; ADest: PWideChar; ALength: Integer): Integer;
  end;

  { TdxReplaceTable }

  TdxReplaceTable = class(TdxCustomHunspellDataTable)
  private
    function GetData: PdxReplaceArray; inline;
    function GetItem(Index: Integer): TdxReplaceTableItem; inline;
  protected
    function AddDataPiece(const APiece: PWideChar; ADataIndex,
      APieceIndex: Integer): Boolean; override;
    procedure FreeDataItems; override;
    function GetDataIdentifier: PWideChar; override;
    function GetDataItemSize: Integer; override;
    function GetPieceQuantity: Integer; override;
    procedure InitializeItem(Index: Integer); override;
  public
    property Data: PdxReplaceArray read GetData;
    property Items[Index: Integer]: TdxReplaceTableItem read GetItem; default;
  end;

  { TdxConvertTable }

  TdxConvertTable = class(TdxReplaceTable)
  private
    function FindNear(const AWord: PWideChar): Integer;
    function Match(const AWord: PWideChar; AIndex: Integer): Integer;
    procedure Sort(AIndex: Integer);
  protected
    function AddDataPiece(const APiece: PWideChar; ADataIndex,
      APieceIndex: Integer): Boolean; override;
  public
    function Convert(const AWord: PWideChar; ADest: PWideChar): Boolean;
  end;

  { TdxAffixFlagAliasesTable }

  TdxAffixFlagAliasesTable = class(TdxHunspellDataLinkedTable)
  protected
    function AddDataPiece(const APiece: PWideChar; ADataIndex,
      APieceIndex: Integer): Boolean; override;
    procedure FreeDataItems; override;
    function GetDataIdentifier: PWideChar; override;
    function GetDataItemSize: Integer; override;
    function GetPieceQuantity: Integer; override;
  end;

  { TdxMorphologicAliasesTable }

  TdxMorphologicAliasesTable = class(TdxHunspellDataLinkedTable)
  protected
    function AddDataPiece(const APiece: PWideChar; ADataIndex,
      APieceIndex: Integer): Boolean; override;
    procedure FreeDataItems; override;
    function GetCount: Integer; override;
    function GetDataIdentifier: PWideChar; override;
    function GetDataItemSize: Integer; override;
    function GetPieceQuantity: Integer; override;
    function ReadDataHeader(ALine: PWideChar): Boolean; override;
  end;

  { TdxAffix }

  TdxAffix = class
  protected
    FAffixManager: TdxHunspellAffixManager;
    FAppendString: PWideChar;
    FStripString: PWideChar;
    FAppendStringLength: Byte;
    FStripStringLength: Byte;
    FConditionLength: ShortInt;
    FOptions: TdxAffixOptions;
    FFlag: Word;
    FMorphologicalDescription: PWideChar;
    FCompatibleFlags: TdxHunspellFlags;
    FCondition: PWideChar;
    FNext: TdxAffix;
    FNextSimilar: TdxAffix;
    FNextDifferent: TdxAffix;
    FNextByFlag: TdxAffix;
    function GetCircumfix: Boolean; inline;
    function GetCompoundEnd: Boolean; inline;
    function GetCompoundForbid: Boolean; inline;
    function GetCompoundMiddle: Boolean; inline;
    function GetCompoundPermit: Boolean; inline;
    function GetInCompoundOnly: Boolean; inline;
    function GetNeedAffix: Boolean; inline;
    function GetPrefixSuffixUnion: Boolean; inline;
  protected
    function CanUseInCompoundWordPart(ACompoundWordPart: TdxCompoundWordPart): Boolean; virtual; abstract;
    function GetAppendString: PWideChar; virtual;
    function IsCompatibleWithFlag(AFlag: Word; CompatibleIfNull: Boolean = False): Boolean;
    function IsPrefixSuffixUnion(const AOptions: TdxAffixOptions): Boolean;
    function IsWithoutAffixWordLengthCorrect(ALengthWithoutAffix: Integer): Boolean;
    procedure SearchConditionGroupEnd(var AConditionCursor: PWideChar);
  public
    constructor Create(AAffixManager: TdxHunspellAffixManager;
      AAffixItem: PdxAffixItem; AFlag: Word; const AOptions: TdxAffixOptions);
    destructor Destroy; override;
    function CanUseInSimpleWordsOrIsFogemorpheme(ACompoundWordPart: TdxCompoundWordPart): Boolean;
    function CanUseInTheMiddleOfCompoundWords(ACompoundWordPart: TdxCompoundWordPart): Boolean;

    //options
    property Circumfix: Boolean read GetCircumfix;
    property CompoundEnd: Boolean read GetCompoundEnd;
    property CompoundForbid: Boolean read GetCompoundForbid;
    property CompoundMiddle: Boolean read GetCompoundMiddle;
    property CompoundPermit: Boolean read GetCompoundPermit;
    property InCompoundOnly: Boolean read GetInCompoundOnly;
    property NeedAffix: Boolean read GetNeedAffix;
    property PrefixSuffixUnion: Boolean read GetPrefixSuffixUnion;

    property AffixManager: TdxHunspellAffixManager read FAffixManager;
    property AppendString: PWideChar read GetAppendString;
    property AppendStringLength: Byte read FAppendStringLength write FAppendStringLength;
    property CompatibleFlags: TdxHunspellFlags read FCompatibleFlags write FCompatibleFlags;
    property Flag: Word read FFlag;
    property MorphologicalDescription: PWideChar read FMorphologicalDescription;
    property Options: TdxAffixOptions read FOptions write FOptions;

    property Next: TdxAffix read FNext write FNext;
    property NextSimilar: TdxAffix read FNextSimilar write FNextSimilar;
    property NextDifferent: TdxAffix read FNextDifferent write FNextDifferent;
    property NextByFlag: TdxAffix read FNextByFlag write FNextByFlag;
  end;

  { TdxPrefix }

  TdxPrefix = class(TdxAffix)
  protected
    function CanUseInCompoundWordPart(ACompoundWordPart: TdxCompoundWordPart): Boolean; override;
  public
    function CheckWord(const AWord: PWideChar; AWordLength: Integer; var ATakenSuffix: TdxSuffix;
      ACompoundWordPart: TdxCompoundWordPart; const ACompoundFlag: Word = dxNullFlag): TdxHunspellWordStem;
    function GetWordWithAffix(const AWord: PWideChar; AWordLength: Integer): PWideChar;
    function MakeInitialWordFormAndProcessTwoSuffixCheck(const AWord: PWideChar; AWordLength: Integer;
      var ATakenSuffix: TdxSuffix; ACompoundWordPart: TdxCompoundWordPart;
      const ACompoundFlag: Word = dxNullFlag): TdxHunspellWordStem;
    function IsWordSuitableForCondition(AWord: PWideChar): Boolean;
  end;

  { TdxSuffix }

  TdxSuffix = class(TdxAffix)
  private
    FReverseAppendString: PWideChar;
  protected
    function CanUseInCompoundWordPart(ACompoundWordPart: TdxCompoundWordPart): Boolean; override;
    function GetAppendString: PWideChar; override;

    property ReverseAppendString: PWideChar read FAppendString;
  public
    constructor Create(AAffixManager: TdxHunspellAffixManager;
      AAffixItem: PdxAffixItem; AFlag: Word; const AOptions: TdxAffixOptions);
    destructor Destroy; override;
    function CheckWord(const AWord: PWideChar; AWordLength: Integer;
      const AOptions: TdxAffixOptions; APrefix: TdxPrefix;
      const ACompatibleFlag: Word = dxNullFlag; const ACompoundFlag: Word = dxNullFlag;
      const AIncompatibleFlag: Word = dxNullFlag): TdxHunspellWordStem;
    function MakeInitialWordFormAndProcessSuffixCheck(const AWord: PWideChar;
      AWordLength: Integer; const AOptions: TdxAffixOptions; APrefix: TdxPrefix;
      var ATakenSuffix: TdxSuffix; const ACompoundFlag: Word = dxNullFlag): TdxHunspellWordStem;
    function GetWordWithAffix(const AWord: PWideChar; AWordLength: Integer): PWideChar;
    function IsWordSuitableForCondition(AWordEnd, AWordBegin: PWideChar): Boolean;
  end;

  TdxSuffixInfo = record
    Flag: Word;
    AppendString: PWideChar;
  end;

  TdxBackTrackingItem = record
    MetacharacterPosition: SmallInt;
    WordsIndex: SmallInt;
    MatchedWordCount: Integer;
  end;
  TdxBackTrackingTable = array [0..dxMaxWordLength - 1] of TdxBackTrackingItem;

  { TdxHunspellAffixManager }

  TdxHunspellAffixManager = class
  private
    FBreakTable: TdxBreakTable;
    FCheckCompoundCase: Boolean;
    FCheckCompoundDup: Boolean;
    FCheckCompoundPatternTable: TdxCheckCompoundPatternTable;
    FCheckCompoundRep: Boolean;
    FCheckCompoundTriple: Boolean;
    FCheckSharps: Boolean;
    FCircumfix: Word;
    FCompatibleFlags: TAffixManagerFlagArray;
    FCompatibleFlagsExist: Boolean;
    FCompoundBegin: Word;
    FCompoundEnd: Word;
    FCompoundFlag: Word;
    FCompoundForbid: Word;
    FCompoundMiddle: Word;
    FCompoundPartMin: Integer;
    FCompoundPermit: Word;
    FCompoundRoot: Word;
    FCompoundRuleTable: TdxCompoundRuleTable;
    FCompoundVowels: PWideChar;
    FCompoundWordMax: Integer;
    FExistentAffixFlags: TdxExistentAffixFlags;
    FFullStrip: Boolean;
    FInputConvertTable: TdxConvertTable;
    FIsHungarian: Boolean;
    FKeepCase: Word;
    FKeyboardString: PWideChar;
    FLemmaPresent: Word;
    FMapTable: TdxMapTable;
    FMaxCompoundSyllable: Integer;
    FMaxNgramSuggestionCount: Integer;
    FNeedAffix: Word;
    FNoSplitSuggestions: Boolean;
    FNoSuggest: Word;
    FInCompoundOnly: Word;
    FOutputConvertTable: TdxConvertTable;
    FPhoneTable: TdxPhoneTable;
    FPrefixTableArrangedByAppendString: TAffixTable;
    FPrefixTableIndexedByFlag: TAffixTable;
    FReplaceTable: TdxReplaceTable;
    FSavedSuffixInfo: TdxSuffixInfo;
    FSimplifiedTriple: Boolean;
    FSubstandard: Word;
    FSuffixTableArrangedByAppendString: TAffixTable;
    FSuffixTableIndexedByFlag: TAffixTable;
    FSuggestionsWithDots: Boolean;
    FSyllableNum: PWideChar;
    FTryChars: PWideChar;
    FVersion: PWideChar;
    FWordStemManager: TdxHunspellWordStemManager;
    FWordChars: PWideChar;
    function ParseAffix(ALine: PWideChar; AAffixFileReader: TdxHunspellReader; AAffixType: TdxAffixType): Boolean;
    function ParseCompoundSyllable(ALine: PWideChar; AAffixFileReader: TdxHunspellReader): Boolean;
    function ParseRep(ALine: PWideChar; AAffixFileReader: TdxHunspellReader): Boolean;

    function AreAffixesBothCircumfixesOrNot(APrefix, ASuffix: TdxAffix): Boolean;
    function CanBeFirstPartInCompoundWord(AWordStem: TdxHunspellWordStem; AWordPartIndex: Cardinal): Boolean;
    procedure CheckHungarianSyllableCount(AWord: PWideChar; AWordStem: TdxHunspellWordStem;
      var ASyllableCount: SmallInt);
    procedure CheckWordsCompatibilityWithCompoundRule(const AWords: PdxWordStemTable;
      var ABackTrackingTable: TdxBackTrackingTable; AWordCount: SmallInt;
      ACompoundRuleTableIndex: Integer; var AFlagIndex, AWordIndex, ABackTrackingIndex: SmallInt;
      var AIsWordStemCompatibleWithCompoundRule,
      AIsAllWordsCompatibleWithCompoundRule: Boolean);
    function CompoundPartAffixCheck(const AInitialWordForm: TWordFormArray; const AInitialWordJoint: Integer;
      var AWordStem: TdxHunspellWordStem; const AWordPartIndex: SmallInt; const ACompoundWordPart: TdxCompoundWordPart;
      var ATakenPrefix: TdxPrefix; var ATakenSuffix: TdxSuffix): Boolean;
    procedure CorrectBackTrackingIndex(var ABackTrackingTable: TdxBackTrackingTable;
      var AFlagIndex, AWordIndex, ABackTrackingIndex: SmallInt);
    procedure CompoundRootCheck(AWordStem: TdxHunspellWordStem; var AWordPartIndex: SmallInt);
    function EndOfCompoundRulePattern(ACompoundRuleTableIndex: Integer; AFlagIndex: Integer): Boolean;
    function FindSuitableCheckCompoundPatternTableIndex(var ATableIndex: Integer; AWordSecondPart: PWideChar): Boolean;
    function GetIsAllWordsCompatibleWithCompoundRule(const AWords: PdxWordStemTable;
      var AWordIndex: SmallInt; AWordIndexMax: Integer; AFlag: Word): Boolean;
    function IsWordStemCompatibleWithCompoundRule(AWordStem: TdxHunspellWordStem): Boolean;
    procedure GetWordStemSuitableForAffixAndCompoundRules(var AWordStem: TdxHunspellWordStem;
      AWordPartIndex: SmallInt; ATableIndex: Integer; var AWords: PdxWordStemTable;
      AWordIndex: SmallInt; ACompoundRuleWords: PdxWordStemTable; AIsFirstWordPart: Boolean);
    procedure GenerateCompoundFirstPartWithAffixes(var AWordStem: TdxHunspellWordStem;
      const AInitialWordForm: TWordFormArray; AInitialWordJoint: Integer;
      var ATakenPrefix: TdxPrefix; var ATakenSuffix: TdxSuffix; ACompoundWordPart: TdxCompoundWordPart);
    function IsAffixesForbidden(ATakenPrefix: TdxPrefix; ATakenSuffix: TdxSuffix): Boolean;
    function IsAffixesIncompatibleWithCompoundParts(
      ATakenPrefix: TdxPrefix; ATakenSuffix: TdxSuffix;
      AIsAffixChecked: Boolean; AWordPartIndex: SmallInt): Boolean;
    function IsWordPartSuitsCheckCompoundPatternTable(AWordStem: TdxHunspellWordStem; ATableIndex: Integer; AIsFirstWordPart: Boolean): Boolean;
    function IsWordPartSuitsCompoundFlags(AWordStem: TdxHunspellWordStem; AWordPartIndex: SmallInt; AIsFirstWordPart: Boolean; AWords: PdxWordStemTable): Boolean;
    function IsSyllablesAndCompoundPartsWithinLimits(AWordPartIndex, ASyllableCount: SmallInt): Boolean;
    function IsTripleLettersExistAndForbid(AWord: PWideChar; AJoint: Cardinal): Boolean;
    procedure MakeWordWithCheckCompoundPatternTable(const AWord: PWideChar;
      AInitialWordForm: PWideChar; ACompoundWordJoint: Integer;
      var AInitialWordJoint, AWordLength: Integer; ATableIndex: Integer);

    function CheckHungarianFlagsCompatibility(AWordStem: TdxHunspellWordStem;
      AIsHungarianMoveRule: Boolean): Boolean;
    function CheckHungarianFlagsCompatibilityWithSuffix(var AWordStem: TdxHunspellWordStem; const AInitialWordForm: TWordFormArray;
      ACompoundWordJoint: Integer; AIsHungarianMoveRule: Boolean;
      var ATakenPrefix: TdxPrefix; var ATakenSuffix: TdxSuffix): Boolean;

    procedure BuildAffixTree(AAffix: TdxAffix; var AAffixTableArrangedByAppendString: TAffixTable;
      var AAffixTableIndexedByFlag: TAffixTable);
    function ProcessSuffixOrPrefixCheck(const AWord: PWideChar; AWordLength: Integer;
      ACompoundFlag: Word; ACompoundWordPart: TdxCompoundWordPart; var
      AWordStem: TdxHunspellWordStem; var ATakenPrefix: TdxPrefix; var ATakenSuffix: TdxSuffix): Boolean;
    procedure LinkAffixesIntoSubsets(const AAffixTableArrangedByAppendString: TAffixTable);
    function OrderAffixTree(AAffix, ANextAffix: TdxAffix): TdxAffix;
    procedure AffixTreeToList(var ATable: TAffixTable);
    function GetAffixFlagMode: TdxAffixFlagMode;
    function GetCodePage: Cardinal;
    function GetComplexPrefixes: Boolean;
    function GetForbiddenWordFlag: Word;
    function GetIgnoredChars: PWideChar;
    function GetIsCompoundWordsAvailable: Boolean;
    function GetLanguage: Integer;
    procedure ProcessPrefixes(const ARoot: PWideChar; ARootLength: Integer;
      const AFlags: TdxHunspellFlags; AWords: PdxGuessWordArray; AWordCount:Integer;
      var AIndex: Integer; ABad: PWideChar; ABadLength: Integer);
    procedure ProcessSuffixes(const ARoot: PWideChar; ARootLength: Integer;
      const AFlags: TdxHunspellFlags; var AIndex: Integer; AWords: PdxGuessWordArray; AWordCount: Integer;
      ABad: PWideChar; ABadLength: Integer; APhonetic: PWideChar);
    procedure SetComplexPrefixes(Value: Boolean);
    procedure SetIgnoredChars(Value: PWideChar);
    procedure SetLanguage(Value: Integer);
  protected
    function DecodeFlag(const AFlag: PWideChar): Word;
    function EncodeFlag(AFlag: Word): PWideChar;
    function GetReaders: TdxAffixReaderList; virtual;
    function GetInitialReaders: TdxAffixReaderList; virtual;
    procedure InitCodePage(AStream: TStream);
    function IsAffixKeyUnique(AFlag: Word; IsPrefix: Boolean): Boolean;
    function ParseGrammar(AStream: TStream): Integer; virtual;
    procedure SetAffixKeyExist(AFlag: Word; IsPrefix: Boolean);
    procedure UpdateAffixesOptions(ATable: TAffixTable);
    procedure UpdateAffixOptions(AAffix: TdxAffix);

    property FullStrip: Boolean read FFullStrip;
    property WordStemManager: TdxHunspellWordStemManager read FWordStemManager;
  public
    constructor Create(AWordStemManager: TdxHunspellWordStemManager);
    destructor Destroy; override;
    procedure UpdateWordStemOptions;
    function AffixCheck(const AWord: PWideChar; AWordLength: Integer; var ATakenPrefix: TdxPrefix;
      var ATakenSuffix: TdxSuffix; const ACompoundFlag: Word = 0;
      ACompoundWordPart: TdxCompoundWordPart = cwpNone): TdxHunspellWordStem;
    function ProcessPrefixCheck(const AWord: PWideChar; AWordLength: Integer;
      var ATakenPrefix: TdxPrefix; var ATakenSuffix: TdxSuffix;
      ACompoundWordPart: TdxCompoundWordPart; const ACompoundFlag: Word = dxNullFlag): TdxHunspellWordStem;
    function IsForbiddenOrNoSuggestWord(AWordStem: TdxHunspellWordStem; ASuggest: Boolean): Boolean;
    function IsLeadingSubset(ASubset: PWideChar; ASet: PWideChar): Boolean; inline;
    function IsTrailingReversedSubset(AReversedSubset: PWideChar; AEndOfSet: PWideChar; ASetLength: Integer): Boolean;
    function ProcessPrefixWithSuffixesCheck(const AWord: PWideChar; AWordLength: Integer;
      var ATakenPrefix: TdxPrefix; var ATakenSuffix: TdxSuffix; ACompoundWordPart: TdxCompoundWordPart;
      const ACompoundFlag: Word = dxNullFlag): TdxHunspellWordStem;
    function ProcessSuffixCheck(const AWord: PWideChar; AWordLength: Integer;
      const AOptions: TdxAffixOptions; APrefix: TdxPrefix; var ATakenSuffix: TdxSuffix;
      const ACompatibleFlag: Word = dxNullFlag;
      const ACompoundFlag: Word = dxNullFlag;
      ACompoundWordPart: TdxCompoundWordPart = cwpNone): TdxHunspellWordStem;
    function ProcessTwoSuffixCheck(const AWord: PWideChar; AWordLength: Integer;
      const AOptions: TdxAffixOptions; APrefix: TdxPrefix; var ATakenSuffix: TdxSuffix;
      const ACompoundFlag: Word = dxNullFlag): TdxHunspellWordStem;
    function GetSyllableCount(const AWord: PWideChar; AWordLength: Integer): SmallInt;
    function IsSimpleWordWithReplacedCharacters(const AWord: PWideChar;
      AWordLength: Integer; var ATakenPrefix: TdxPrefix; var ATakenSuffix: TdxSuffix): Boolean;
    function IsForbiddenByCheckCompoundPattern(const AWord: PWideChar; AJoint: Integer;
      AFirstPartWordStem, ASecondPartWordStem: TdxHunspellWordStem): Boolean;
    function IsWordPartsSuitCompoundRule(var AWords: PdxWordStemTable; AWordCount: SmallInt;
      AWordStem: TdxHunspellWordStem; ADefaultWords: PdxWordStemTable; AIsFirstWordPart: Boolean): Boolean;
    function IsJointUpperCaseExistAndForbid(const AWord: PWideChar; AJoint: Integer): Boolean;
    function CompoundWordWithReplacedCharactersExists(const AWord: PWideChar;
      AWordLength: Integer; var ATakenPrefix: TdxPrefix; var ATakenSuffix: TdxSuffix): Boolean;
    procedure GetCompoundWordJointRange(var ACompoundWordJointMin, ACompoundWordJointMax: Integer; const AWord: PWideChar; AWordLength: Integer);
    function CompoundCheck(const AWord: PWideChar; AWordPartIndex, ASyllableCount,
      AWordPartMaxCount, AWordIndex: SmallInt; AWords: PdxWordStemTable = nil;
      AIsHungarianMoveRule: Boolean = False; ASuggest: Boolean = False): TdxHunspellWordStem;
    procedure Load(AStream: TStream);
    function Lookup(const AWord: PWideChar): TdxHunspellWordStem;
    procedure SetCompatibleFlags(AIndex: Integer; AValue: Boolean);
    function ExpandWordStem(AWords: PdxGuessWordArray; AWordCount: Integer;
      const ARoot: PWideChar; ARootLength: Integer; const AFlags: TdxHunspellFlags;
      ABad: PWideChar; ABadLength: Integer; APhonetic: PWideChar): Integer;

    property AffixFlagMode: TdxAffixFlagMode read GetAffixFlagMode;
    property BreakTable: TdxBreakTable read FBreakTable;
    property CheckSharps: Boolean read FCheckSharps;
    property CodePage: Cardinal read GetCodePage;
    property CompatibleFlagsExist: Boolean read FCompatibleFlagsExist write FCompatibleFlagsExist;
    property ComplexPrefixes: Boolean read GetComplexPrefixes write SetComplexPrefixes;
    property CompoundFlag: Word read FCompoundFlag;
    property ForbiddenWordFlag: Word read GetForbiddenWordFlag;
    property IgnoredChars: PWideChar read GetIgnoredChars write SetIgnoredChars;
    property InputConvertTable: TdxConvertTable read FInputConvertTable;
    property IsCompoundWordsAvailable: Boolean read GetIsCompoundWordsAvailable;
    property IsHungarian: Boolean read FIsHungarian;
    property KeepCase: Word read FKeepCase;
    property KeyboardString: PWideChar read FKeyboardString;
    property Language: Integer read GetLanguage write SetLanguage;
    property MapTable: TdxMapTable read FMapTable;
    property MaxNgramSuggestionCount: Integer read FMaxNgramSuggestionCount;
    property NeedAffix: Word read FNeedAffix;
    property OutputConvertTable: TdxConvertTable read FOutputConvertTable;
    property InCompoundOnly: Word read FInCompoundOnly;
    property PhoneTable: TdxPhoneTable read FPhoneTable;
    property ReplaceTable: TdxReplaceTable read FReplaceTable;
    property SuggestionsWithDots: Boolean read FSuggestionsWithDots;
    property TryChars: PWideChar read FTryChars;
    property WordChars: PWideChar read FWordChars;
  end;

  { TdxAffixFileAttributeReader }

  TdxAffixFileAttributeReader = class
  private
    FAffixManager: TdxHunspellAffixManager;
    function GetWordStemManager: TdxHunspellWordStemManager;
  protected
    function GetDataIdentifier: PWideChar; virtual; abstract;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; virtual; abstract;
    function ParseFlag(ALine: PWideChar; var ADecodedFlag: Word; AReader: TdxHunspellReader): Boolean;
    function ParseNumber(ALine: PWideChar; out ANumber: Integer): Boolean;
    function Repeating: Boolean; virtual;

    property AffixManager: TdxHunspellAffixManager read FAffixManager;
    property WordStemManager: TdxHunspellWordStemManager read GetWordStemManager;
  public
    constructor Create(AAffixManager: TdxHunspellAffixManager);
  end;

  TdxAffixFileAttributeReaderClass = class of TdxAffixFileAttributeReader;

  { TdxAffixReaderList }

  TdxAffixReaderList = class(TcxObjectList)
  private
    FAffixManager: TdxHunspellAffixManager;
    function GetItem(Index: Integer): TdxAffixFileAttributeReader;
  public
    constructor Create(AAffixManager: TdxHunspellAffixManager);
    procedure Add(AReaderClass: TdxAffixFileAttributeReaderClass);
    procedure ProcessLine(ALine: PWideChar; AReader: TdxHunspellReader);

    property Items[Index: Integer]: TdxAffixFileAttributeReader read GetItem; default;
  end;

implementation

uses
  dxSpellCheckerUtils, Windows;

const
  dxExistentSuffixFlag   = 1 shl 0;
  dxExistentPrefixFlag   = 1 shl 1;

  dxMaxLineLength        = 8192;
  dxDefaultCompoundPartLength = 3;

  dxCompoundRuleOneWordFlag = '?';
  dxCompoundRuleSeveralWordsFlag = '*';

  dxHungarianFlagC = 'c';
  dxHungarianFlagF = 'F';
  dxHungarianFlagG = 'G';
  dxHungarianFlagH = 'H';
  dxHungarianFlagI = 'I';
  dxHungarianFlagJ = 'J';
  dxHungarianFlagX = 'x';
  dxHungarianFlagPercent = '%';

type

  { FileReaders }

  TTryReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TKeyReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TFlagReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TSetReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TComplexPrefixesReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TCompoundFlagReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TCompoundBeginReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TCompoundEndReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TCompoundMiddleReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TCompoundWordMaxReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TCompoundRootReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TCompoundPermitFlagReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TCompoundForbidFlagReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TCheckCompoundDupReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TCheckCompoundRepReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TCheckCompoundTripleReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TSimplifiedTripleReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TCheckCompoundCaseReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TNoSuggestReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TForbiddenWordReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TLemmaPresentReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TCircumfixReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TInCompoundOnlyReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TPseudoRootReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TNeedAffixReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TCompoundMinReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TCompoundSyllableReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TSyllableNumReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TWordCharsReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TIgnoreReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TRepReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
    function Repeating: Boolean; override;
  end;

  TIConvReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TOConvReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TCheckCompoundPatternReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
    function Repeating: Boolean; override;
  end;

  TCompoundRuleReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
    function Repeating: Boolean; override;
  end;

  TMapReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
    function Repeating: Boolean; override;
  end;

  TPhoneReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TBreakReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
    function Repeating: Boolean; override;
  end;

  TLangReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TVersionReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TNoSplitSuggestionsReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TNgramSuggestionReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TFullStripReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TSuggestionsWithDotsReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TKeepCaseReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TSubStandardReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TCheckSharpsReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
  end;

  TPrefixReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
    function Repeating: Boolean; override;
  end;

  TSuffixReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
    function Repeating: Boolean; override;
  end;

  TAffixFlagAliasReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
    function Repeating: Boolean; override;
  end;

  TMorphologicAliasReader = class(TdxAffixFileAttributeReader)
  protected
    function GetDataIdentifier: PWideChar; override;
    function Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean; override;
    function Repeating: Boolean; override;
  end;

{ TdxHunspellAffixManager }

constructor TdxHunspellAffixManager.Create(AWordStemManager: TdxHunspellWordStemManager);
const
  ICONV: PWideChar = diIConv;
  OCONV: PWideChar = diOConv;
var
  I: Integer;
begin
  inherited Create;
  FWordStemManager := AWordStemManager;
  FCompoundWordMax := -1;
  FCompoundPartMin := -1;
  FMaxNgramSuggestionCount := -1;
  FBreakTable := TdxBreakTable.Create;
  FCheckCompoundPatternTable := TdxCheckCompoundPatternTable.Create(Self);
  FCompoundRuleTable := TdxCompoundRuleTable.Create(Self);
  FInputConvertTable := TdxConvertTable.Create(ICONV);
  FOutputConvertTable := TdxConvertTable.Create(OCONV);
  FMapTable := TdxMapTable.Create;
  FReplaceTable := TdxReplaceTable.Create;
  FPhoneTable := TdxPhoneTable.Create;
  for I := 0 to dxAffixTableSize - 1 do
  begin
    FPrefixTableArrangedByAppendString[I] := nil;
    FSuffixTableArrangedByAppendString[I] := nil;
    FPrefixTableIndexedByFlag[I] := nil;
    FSuffixTableIndexedByFlag[I] := nil;
  end;
  for I := 0 to dxMaxFlagCount - 1 do
    FCompatibleFlags[I] := False;
end;

destructor TdxHunspellAffixManager.Destroy;
var
  I: Integer;
  AAffix, ANextAffix: TdxAffix;
begin
  for I := 0 to dxAffixTableSize - 1 do
  begin
    FPrefixTableIndexedByFlag[I] := nil;
    FSuffixTableIndexedByFlag[I] := nil;
    AAffix := TdxPrefix(FPrefixTableArrangedByAppendString[I]);
    while AAffix <> nil do
    begin
      ANextAffix := AAffix.Next;
      AAffix.Free;
      AAffix := ANextAffix;
    end;
    AAffix := TdxSuffix(FSuffixTableArrangedByAppendString[I]);
    while AAffix <> nil do
    begin
      ANextAffix := AAffix.Next;
      AAffix.Free;
      AAffix := ANextAffix;
    end;
    FSuffixTableArrangedByAppendString[I] := nil;
  end;
  FreeAndNil(FMapTable);
  FreeAndNil(FBreakTable);
  FreeAndNil(FReplaceTable);
  FreeAndNil(FInputConvertTable);
  FreeAndNil(FOutputConvertTable);
  FreeAndNil(FCompoundRuleTable);
  FreeAndNil(FCheckCompoundPatternTable);
  FreeAndNil(FPhoneTable);
  FWordStemManager := nil;
  StrDispose(FTryChars);
  StrDispose(FCompoundVowels);
  StrDispose(FSyllableNum);
  StrDispose(FVersion);
  StrDispose(FKeyboardString);
  StrDispose(FWordChars);
  inherited Destroy;
end;

function TdxHunspellAffixManager.ParseGrammar(AStream: TStream): Integer;
var
  ALineBuffer: TdxLineBuffer;
  ALine: PWideChar;
  AAffixReader: TdxHunspellReader;
  AReaders: TdxAffixReaderList;
begin
  Result := 0;
  InitCodePage(AStream);
  AReaders := GetReaders;
  try
    AAffixReader := TdxHunspellReader.Create(AStream, CodePage);
    try
      ALine := @ALineBuffer;
      FillChar(FExistentAffixFlags, SizeOf(FExistentAffixFlags), 0);
      while AAffixReader.GetNextLine(ALineBuffer, SizeOf(ALineBuffer)) do
        AReaders.ProcessLine(ALine, AAffixReader);
    finally
      AAffixReader.Free;
    end;
  finally
    AReaders.Free;
  end;
end;

procedure TdxHunspellAffixManager.BuildAffixTree(AAffix: TdxAffix;
  var AAffixTableArrangedByAppendString: TAffixTable; var AAffixTableIndexedByFlag: TAffixTable);
var
  ACurrentItem, APreviousAffix: TdxAffix;
  AFirstChar: Word;
begin
  ACurrentItem := AAffixTableIndexedByFlag[AAffix.Flag];
  AAffix.NextByFlag := ACurrentItem;
  AAffixTableIndexedByFlag[AAffix.Flag] := AAffix;
  if AAffix.AppendStringLength = 0 then
  begin
    ACurrentItem := AAffixTableArrangedByAppendString[0];
    AAffix.Next := ACurrentItem;
    AAffixTableArrangedByAppendString[0] := AAffix;
    Exit;
  end;
  AAffix.NextSimilar := nil;
  AAffix.NextDifferent := nil;
  AFirstChar := Ord(AAffix.AppendString^);
  ACurrentItem := AAffixTableArrangedByAppendString[AFirstChar];
  if ACurrentItem = nil then
  begin
    AAffixTableArrangedByAppendString[AFirstChar] := AAffix;
    Exit;
  end;
  repeat
    APreviousAffix := ACurrentItem;
    if StrComp(AAffix.AppendString, ACurrentItem.AppendString) <= 0 then
    begin
      ACurrentItem := ACurrentItem.NextSimilar;
      if ACurrentItem = nil then
        APreviousAffix.NextSimilar := AAffix;
    end
    else
    begin
      ACurrentItem := ACurrentItem.NextDifferent;
      if ACurrentItem = nil then
        APreviousAffix.NextDifferent := AAffix;
    end;
  until ACurrentItem = nil;
end;

function TdxHunspellAffixManager.ProcessSuffixOrPrefixCheck(const AWord: PWideChar;
  AWordLength: Integer; ACompoundFlag: Word; ACompoundWordPart: TdxCompoundWordPart;
  var AWordStem: TdxHunspellWordStem; var ATakenPrefix: TdxPrefix;
  var ATakenSuffix: TdxSuffix): Boolean;
begin
  AWordStem := ProcessSuffixCheck(AWord, AWordLength, [], nil, ATakenSuffix,
    dxNullFlag, ACompoundFlag, ACompoundWordPart);
  Result := AWordStem <> nil;
  if not Result then
  begin
    AWordStem := ProcessPrefixCheck(AWord, AWordLength, ATakenPrefix,
      ATakenSuffix, ACompoundWordPart, ACompoundFlag);
    Result := AWordStem <> nil;
  end;
end;

procedure TdxHunspellAffixManager.AffixTreeToList(var ATable: TAffixTable);
var
  I: Integer;
begin
  for I := 1 to dxAffixTableSize - 1 do
    ATable[I] := OrderAffixTree(ATable[I], nil);
end;

function TdxHunspellAffixManager.GetAffixFlagMode: TdxAffixFlagMode;
begin
  Result := FWordStemManager.AffixFlagMode;
end;

function TdxHunspellAffixManager.GetCodePage: Cardinal;
begin
  Result := FWordStemManager.CodePage;
end;

function TdxHunspellAffixManager.GetComplexPrefixes: Boolean;
begin
  Result := FWordStemManager.ComplexPrefixes;
end;

function TdxHunspellAffixManager.GetForbiddenWordFlag: Word;
begin
  Result := FWordStemManager.ForbiddenWordFlag;
end;

function TdxHunspellAffixManager.GetIgnoredChars: PWideChar;
begin
  Result := FWordStemManager.IgnoredChars;
end;

function TdxHunspellAffixManager.OrderAffixTree(AAffix, ANextAffix: TdxAffix): TdxAffix;
begin
  if AAffix <> nil then
  begin
    ANextAffix := OrderAffixTree(AAffix.NextDifferent, ANextAffix);
    AAffix.Next := ANextAffix;
    ANextAffix := OrderAffixTree(AAffix.NextSimilar, AAffix);
  end;
  Result := ANextAffix;
end;

procedure TdxHunspellAffixManager.LinkAffixesIntoSubsets(const AAffixTableArrangedByAppendString: TAffixTable);
var
  AAffix, ANextAffix, ALastAffixInChain: TdxAffix;
  I: Integer;
begin
  for I := 1 to dxAffixTableSize - 1 do
  begin
    AAffix := AAffixTableArrangedByAppendString[I];
    while AAffix <> nil do
    begin
      ANextAffix := AAffix.Next;
      while ANextAffix <> nil do
      begin
        if not IsLeadingSubset(AAffix.AppendString, ANextAffix.AppendString) then
          Break;
        ANextAffix := ANextAffix.Next;
      end;
      AAffix.NextDifferent := ANextAffix;
      AAffix.NextSimilar := nil;
      if (AAffix.Next <> nil) and IsLeadingSubset(AAffix.AppendString, AAffix.Next.AppendString) then
        AAffix.NextSimilar := AAffix.Next;
      AAffix := AAffix.Next;
    end;
    AAffix := AAffixTableArrangedByAppendString[I];
    while AAffix <> nil do
    begin
      ANextAffix := AAffix.Next;
      ALastAffixInChain := nil;
      while ANextAffix <> nil do
      begin
        if not IsLeadingSubset(AAffix.AppendString, ANextAffix.AppendString) then
          Break;
        ALastAffixInChain := ANextAffix;
        ANextAffix := ANextAffix.Next;
      end;
      if ALastAffixInChain <> nil then
        ALastAffixInChain.NextDifferent := nil;
      AAffix := AAffix.Next;
    end;
  end;
end;

function TdxHunspellAffixManager.IsForbiddenOrNoSuggestWord(
  AWordStem: TdxHunspellWordStem; ASuggest: Boolean): Boolean;
begin
  Result := (AWordStem <> nil) and
    (AWordStem.Forbidden or (ASuggest and AWordStem.NoSuggest));
end;

function TdxHunspellAffixManager.IsLeadingSubset(ASubset: PWideChar; ASet: PWideChar): Boolean;
begin
  while ((ASubset^ = ASet^) or (ASubset^ = '.')) and (ASubset^ <> #0) do
  begin
    Inc(ASubset);
    Inc(ASet);
  end;
  Result := ASubset^ = #0;
end;

function TdxHunspellAffixManager.IsTrailingReversedSubset(AReversedSubset: PWideChar;
  AEndOfSet: PWideChar; ASetLength: Integer): Boolean;
begin
  while (ASetLength > 0) and (AReversedSubset^ <> #0) and
    ((AReversedSubset^ = AEndOfSet^) or (AReversedSubset^ = '.')) do
  begin
    Inc(AReversedSubset);
    Dec(AEndOfSet);
    Dec(ASetLength);
  end;
  Result := AReversedSubset^ = #0;
end;

function TdxHunspellAffixManager.ProcessPrefixCheck(const AWord: PWideChar; AWordLength: Integer;
  var ATakenPrefix: TdxPrefix; var ATakenSuffix: TdxSuffix; ACompoundWordPart: TdxCompoundWordPart;
  const ACompoundFlag: Word = dxNullFlag): TdxHunspellWordStem;
var
  APrefix: TdxPrefix;
begin
  Result := nil;
  ATakenPrefix := nil;
  FSavedSuffixInfo.AppendString := nil;
  APrefix := TdxPrefix(FPrefixTableArrangedByAppendString[0]);
  while (APrefix <> nil) and (Result = nil) do
  begin
    if APrefix.CanUseInSimpleWordsOrIsFogemorpheme(ACompoundWordPart) and
      APrefix.CanUseInTheMiddleOfCompoundWords(ACompoundWordPart) then
    begin
      Result := APrefix.CheckWord(AWord, AWordLength, ATakenSuffix, ACompoundWordPart, ACompoundFlag);
      if Result <> nil then
        ATakenPrefix := APrefix;
    end;
    APrefix := TdxPrefix(APrefix.Next);
  end;
  if Result = nil then
  begin
    APrefix := TdxPrefix(FPrefixTableArrangedByAppendString[PWord(AWord)^]);
    while (APrefix <> nil) and (Result = nil) do
    begin
      if IsLeadingSubset(APrefix.AppendString, AWord) then
      begin
        if APrefix.CanUseInSimpleWordsOrIsFogemorpheme(ACompoundWordPart) and
          APrefix.CanUseInTheMiddleOfCompoundWords(ACompoundWordPart) then
        begin
          Result := APrefix.CheckWord(AWord, AWordLength, ATakenSuffix, ACompoundWordPart, ACompoundFlag);
          if Result <> nil then
            ATakenPrefix := APrefix;
        end;
        APrefix := TdxPrefix(APrefix.NextSimilar);
      end
      else
        APrefix := TdxPrefix(APrefix.NextDifferent);
    end;
  end;
end;

function TdxHunspellAffixManager.ProcessPrefixWithSuffixesCheck(const AWord: PWideChar;
  AWordLength: Integer; var ATakenPrefix: TdxPrefix; var ATakenSuffix: TdxSuffix;
  ACompoundWordPart: TdxCompoundWordPart; const ACompoundFlag: Word = dxNullFlag): TdxHunspellWordStem;
var
  APrefix: TdxPrefix;
begin
  Result := nil;

  ATakenPrefix := nil;
  FSavedSuffixInfo.AppendString := nil;

  APrefix := TdxPrefix(FPrefixTableArrangedByAppendString[0]);
  while (APrefix <> nil) and (Result = nil) do
  begin
    Result := APrefix.MakeInitialWordFormAndProcessTwoSuffixCheck(AWord,
      AWordLength, ATakenSuffix, ACompoundWordPart, ACompoundFlag);
    APrefix := TdxPrefix(APrefix.Next);
  end;

  if Result = nil then
  begin
    APrefix := TdxPrefix(FPrefixTableArrangedByAppendString[PWord(AWord)^]);

    while (APrefix <> nil) and (Result = nil) do
    begin
      if IsLeadingSubset(APrefix.AppendString, AWord) then
      begin
        Result := APrefix.MakeInitialWordFormAndProcessTwoSuffixCheck(AWord,
          AWordLength, ATakenSuffix, ACompoundWordPart, ACompoundFlag);
        if Result <> nil then
          ATakenPrefix := APrefix;
        APrefix := TdxPrefix(APrefix.NextSimilar);
      end
      else
        APrefix := TdxPrefix(APrefix.NextDifferent);
    end;
  end;
end;

function TdxHunspellAffixManager.IsSimpleWordWithReplacedCharacters(
  const AWord: PWideChar; AWordLength: Integer; var ATakenPrefix: TdxPrefix;
  var ATakenSuffix: TdxSuffix): Boolean;
var
  AChangedWord: array [0..dxMaxLineLength - 1] of WideChar;
  AWordCursor: PWideChar;
  AReplaceLength, AInitialLength, I: Integer;
  ATemp: PWideChar;
begin
  Result := False;
  if (AWordLength < 2) or (FReplaceTable.Count = 0) or not FCheckCompoundRep then
    Exit;

  for I := 0 to FReplaceTable.Count - 1 do
  begin
    AWordCursor := AWord;
    AReplaceLength := StrLen(FReplaceTable[I].Replacement);
    AInitialLength := StrLen(FReplaceTable[I].Text);
    AWordCursor := StrPos(AWordCursor, FReplaceTable[I].Text);
    while AWordCursor <> nil do
    begin
      StrCopy(AChangedWord, AWord);
      if AWordCursor - AWord + AReplaceLength + Integer(StrLen(AWordCursor + AInitialLength)) > dxMaxLineLength then
        Break;

      ATemp := AChangedWord;
      ATemp := ATemp + (AWordCursor - AWord);
      StrCopy(ATemp, FReplaceTable[I].Replacement);
      StrCopy(ATemp + AReplaceLength,
        AWordCursor + AInitialLength);
      if CompoundWordWithReplacedCharactersExists(AChangedWord, StrLen(AChangedWord), ATakenPrefix, ATakenSuffix) then
        Exit(True);

      Inc(AWordCursor);
      AWordCursor := StrPos(AWordCursor, FReplaceTable[I].Text);
    end;
  end;
end;

function TdxHunspellAffixManager.IsForbiddenByCheckCompoundPattern(const AWord: PWideChar;
  AJoint: Integer; AFirstPartWordStem, ASecondPartWordStem: TdxHunspellWordStem): Boolean;
var
  ALength, I: Integer;
begin
  Result := False;
  for I := 0 to FCheckCompoundPatternTable.Count - 1 do
  begin
    ALength := StrLen(FCheckCompoundPatternTable[I].Pattern);
    if IsLeadingSubset(FCheckCompoundPatternTable[I].Pattern2, AWord + AJoint) and
      ((AFirstPartWordStem = nil) or (FCheckCompoundPatternTable[I].Condition = 0) or
      AFirstPartWordStem.IsCompatibleWithFlag(FCheckCompoundPatternTable[I].Condition)) and
      ((ASecondPartWordStem = nil) or (FCheckCompoundPatternTable[I].Condition2 = 0) or
      ASecondPartWordStem.IsCompatibleWithFlag(FCheckCompoundPatternTable[I].Condition2)) and
      (ALength <> 0) and (AJoint > ALength) and
      (StrLComp(AWord + AJoint - ALength, FCheckCompoundPatternTable[I].Pattern, ALength) = 0) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TdxHunspellAffixManager.IsJointUpperCaseExistAndForbid(const AWord: PWideChar; AJoint: Integer): Boolean;
var
  A, B: WideChar;
begin
  Result := False;
  begin
    A := (AWord + AJoint - 1)^;
    B := (AWord + AJoint)^;
    if (IsUpCase(A) or IsUpCase(B)) and (A <> '-') and (B <> '-') then
      Result := FCheckCompoundCase;
  end;
end;

function TdxHunspellAffixManager.IsWordPartsSuitCompoundRule(var AWords: PdxWordStemTable;
  AWordCount: SmallInt; AWordStem: TdxHunspellWordStem;
  ADefaultWords: PdxWordStemTable; AIsFirstWordPart: Boolean): Boolean;
var
  ABackTrackingTable: TdxBackTrackingTable;
  ABackTrackingIndex, AFlagIndex, AWordIndex: SmallInt;
  I: Integer;
  AIsWordStemCompatibleWithCompoundRule, AIsAllWordsCompatibleWithCompoundRule,
    AIsWordsInitialized: Boolean;

  function IsFullCompatibility: Boolean;
  begin
    Result := AIsWordStemCompatibleWithCompoundRule and AIsAllWordsCompatibleWithCompoundRule;
  end;

begin
  Result := False;
  if (AWordStem.AffixFlags.Length = 0) or ((AWords = nil) and (ADefaultWords = nil)) then
    Exit;

  AIsWordStemCompatibleWithCompoundRule := IsWordStemCompatibleWithCompoundRule(AWordStem);
  if AIsWordStemCompatibleWithCompoundRule then
  begin
    AIsWordsInitialized := False;
    if AWords = nil then
    begin
      AIsWordsInitialized := True;
      AWords := ADefaultWords;
    end;
    AWords[AWordCount] := AWordStem;

    ABackTrackingIndex := 0;
    I := 0;
    while (I < FCompoundRuleTable.Count) and not Result do
    begin
      AFlagIndex := 0;
      AWordIndex := 0;
      AIsWordStemCompatibleWithCompoundRule := True;
      AIsAllWordsCompatibleWithCompoundRule := True;
      repeat
        CheckWordsCompatibilityWithCompoundRule(AWords, ABackTrackingTable,
          AWordCount, I, AFlagIndex, AWordIndex, ABackTrackingIndex,
          AIsWordStemCompatibleWithCompoundRule,
          AIsAllWordsCompatibleWithCompoundRule);
        if IsFullCompatibility then
        begin
          Result := EndOfCompoundRulePattern(I, AFlagIndex);
          if Result then
            Break;
        end;
        if ABackTrackingIndex > 0 then
        begin
          AIsWordStemCompatibleWithCompoundRule := True;
          CorrectBackTrackingIndex(ABackTrackingTable, AFlagIndex, AWordIndex,
            ABackTrackingIndex);
        end;
      until ABackTrackingIndex = 0;
      if IsFullCompatibility and
        ((AIsFirstWordPart or (FCompoundRuleTable[I].Length <= AFlagIndex)) or
        EndOfCompoundRulePattern(I, AFlagIndex)) then
        Result := True;
      Inc(I);
    end;
    if not Result then
    begin
      AWords[AWordCount] := nil;
      if AIsWordsInitialized then
        AWords := nil;
    end;
  end;
end;

function TdxHunspellAffixManager.CompoundWordWithReplacedCharactersExists(
  const AWord: PWideChar; AWordLength: Integer; var ATakenPrefix: TdxPrefix; var ATakenSuffix: TdxSuffix): Boolean;
begin
  Result := True;
  if Lookup(AWord) = nil then
    if AffixCheck(AWord, AWordLength, ATakenPrefix, ATakenSuffix) = nil then
      Result := False;
end;

function TdxHunspellAffixManager.GetSyllableCount(const AWord: PWideChar; AWordLength: Integer): SmallInt;
var
  I: Integer;
begin
  Result := 0;
  if FMaxCompoundSyllable = 0 then
    Exit;
  for I := 0 to AWordLength - 1 do
    if StrScan(FCompoundVowels, (AWord + I)^) <> nil then
      Inc(Result);
end;

procedure TdxHunspellAffixManager.GetCompoundWordJointRange(var ACompoundWordJointMin,
  ACompoundWordJointMax: Integer; const AWord: PWideChar; AWordLength: Integer);
begin
  ACompoundWordJointMin := FCompoundPartMin;
  ACompoundWordJointMax := AWordLength - FCompoundPartMin + 1;
end;

function TdxHunspellAffixManager.CompoundCheck(const AWord: PWideChar;
  AWordPartIndex, ASyllableCount, AWordPartMaxCount, AWordIndex: SmallInt; AWords: PdxWordStemTable = nil;
  AIsHungarianMoveRule: Boolean = False; ASuggest: Boolean = False): TdxHunspellWordStem;
var
  ACompoundWordJoint, AWordLength, ACompoundWordJointMin, ACompoundWordJointMax,
  ATableIndex, AInitialWordJoint: Integer;
  AIsAffixChecked: Boolean;
  ADoubleCharacter, ACheckWordWithoutTripleRemoving: Boolean;
  ASyllableCountStore, ASyllableCountStore2, AWordPartIndexStore, AWordPartIndexStore2: SmallInt;
  AWordStem, AFirstPartWordStem: TdxHunspellWordStem;
  ACompoundRuleWords: TdxCompoundWordRules;
  AInitialWordForm: TWordFormArray;
  AJointCharacter: WideChar;
  ACompoundWordPart: TdxCompoundWordPart;
  ATakenPrefix: TdxPrefix;
  ATakenSuffix: TdxSuffix;

  function CheckSimpleWordWithReplacedCharacters: TdxHunspellWordStem;
  begin
    if IsSimpleWordWithReplacedCharacters(AWord, AWordLength, ATakenPrefix, ATakenSuffix) then
      Result := nil
    else
      Result := AFirstPartWordStem;
  end;

  procedure HungarianSyllableProcess(var ASyllableCount, AWordPartIndex: SmallInt);
  begin
    if IsHungarian then
    begin
      Inc(ASyllableCount, GetSyllableCount(AInitialWordForm, AInitialWordJoint));
      if (ATakenPrefix <> nil) and (GetSyllableCount(ATakenPrefix.AppendString, StrLen(ATakenPrefix.AppendString)) > 1) then
        Inc(AWordPartIndex);
    end;
  end;

  procedure HungarianDecreaseSyllableCount(var ASyllableCount: SmallInt);
  begin
    if (AWordStem <> nil) and IsHungarian and
      AWordStem.IsCompatibleWithFlag(Ord(dxHungarianFlagI)) and
      not AWordStem.IsCompatibleWithFlag(Ord(dxHungarianFlagJ)) then
      Dec(ASyllableCount);
  end;

begin
  Result := nil;
  AWordLength := StrLen(AWord);
  if IsSimpleWordWithReplacedCharacters(AWord, AWordLength, ATakenPrefix, ATakenSuffix) then
    Exit;
  AWordStem := nil;
  ADoubleCharacter := False;
  ACheckWordWithoutTripleRemoving := False;
  ATableIndex := 0;
  GetCompoundWordJointRange(ACompoundWordJointMin, ACompoundWordJointMax, AWord, AWordLength);
  StrCopy(AInitialWordForm, AWord);
  ACompoundWordJoint := ACompoundWordJointMin;
  while ACompoundWordJoint < ACompoundWordJointMax do
  begin
    AWordLength := StrLen(AWord);
    GetCompoundWordJointRange(ACompoundWordJointMin, ACompoundWordJointMax, AWord, AWordLength);
    StrCopy(AInitialWordForm, AWord);
    ASyllableCountStore := ASyllableCount;
    AWordPartIndexStore := AWordPartIndex;
    AIsAffixChecked := False;
    repeat
      AInitialWordJoint := ACompoundWordJoint;
      if ATableIndex > 0 then
      begin
        if not FindSuitableCheckCompoundPatternTableIndex(ATableIndex,
          AWord + ACompoundWordJoint) then
          Break;
        MakeWordWithCheckCompoundPatternTable(AWord, AInitialWordForm,
          ACompoundWordJoint, AInitialWordJoint, AWordLength, ATableIndex - 1);
        GetCompoundWordJointRange(ACompoundWordJointMin, ACompoundWordJointMax,
          AInitialWordForm, AWordLength);
      end;
      AJointCharacter := AInitialWordForm[AInitialWordJoint];
      AInitialWordForm[AInitialWordJoint] := #0;
      ATakenSuffix := nil;
      ATakenPrefix := nil;
      AWordStem := Lookup(AInitialWordForm);
      if not AIsHungarianMoveRule then
        GetWordStemSuitableForAffixAndCompoundRules(AWordStem, AWordPartIndex,
          ATableIndex, AWords, AWordIndex, @ACompoundRuleWords, True);
      if AWordStem = nil then
      begin
        ACompoundWordPart := cwpFirst;
        if AIsHungarianMoveRule then
          ACompoundWordPart := cwpOther;
        GenerateCompoundFirstPartWithAffixes(AWordStem, AInitialWordForm,
          AInitialWordJoint, ATakenPrefix, ATakenSuffix, ACompoundWordPart);
        AIsAffixChecked := CompoundPartAffixCheck(AInitialWordForm, AInitialWordJoint,
          AWordStem, AWordPartIndex, ACompoundWordPart, ATakenPrefix, ATakenSuffix);
      end
      else
        if AWordStem.NeedAffix or IsForbiddenOrNoSuggestWord(AWordStem, ASuggest) then
        begin
          AInitialWordForm[AInitialWordJoint] := AJointCharacter;
          Continue;
        end;
      if (AWordStem <> nil) and not AIsHungarianMoveRule and
        (IsAffixesForbidden(ATakenPrefix, ATakenSuffix) or
        IsAffixesIncompatibleWithCompoundParts(ATakenPrefix,
        ATakenSuffix, AIsAffixChecked, AWordPartIndex)) then
        AWordStem := nil;
      if IsForbiddenOrNoSuggestWord(AWordStem, ASuggest) then
      begin
        Result := nil;
        Exit;
      end;
      CompoundRootCheck(AWordStem, AWordPartIndex);
      if ((AWordStem <> nil) and
          (AIsAffixChecked or ((AWords <> nil) and (AWords[AWordIndex] <> nil)) or
            CanBeFirstPartInCompoundWord(AWordStem, AWordPartIndexStore) or
            CheckHungarianFlagsCompatibility(AWordStem, AIsHungarianMoveRule)) and
          IsWordPartSuitsCheckCompoundPatternTable(AWordStem, ATableIndex, True) and
          not ((ATableIndex = 0) and (AWords = nil) and
          (IsTripleLettersExistAndForbid(AWord, AInitialWordJoint) or
            IsJointUpperCaseExistAndForbid(AWord, AInitialWordJoint)))) or
         CheckHungarianFlagsCompatibilityWithSuffix(AWordStem, AInitialWordForm, ACompoundWordJoint,
           AIsHungarianMoveRule, ATakenPrefix, ATakenSuffix) then
      begin
        HungarianSyllableProcess(ASyllableCount, AWordPartIndex);
        AFirstPartWordStem := AWordStem;
        AInitialWordForm[AInitialWordJoint] := AJointCharacter;
        repeat
          if FSimplifiedTriple then
          begin
            if ADoubleCharacter then
            begin
              ACheckWordWithoutTripleRemoving := True;
              Dec(AInitialWordJoint);
            end
            else
              if (AInitialWordJoint > 2) and
                ((AWord + AInitialWordJoint - 1)^ = (AWord + AInitialWordJoint - 2)^) then
                ADoubleCharacter := True;
          end;
          AWordStem := Lookup(AInitialWordForm + AInitialWordJoint);
          GetWordStemSuitableForAffixAndCompoundRules(AWordStem, 0,
            ATableIndex, AWords, AWordIndex + 1, nil, False);
          if (AWordStem <> nil) and (AWords <> nil) and (AWords[AWordIndex + 1] <> nil) then
          begin
            Result := AFirstPartWordStem;
            Exit;
          end;
          if IsForbiddenOrNoSuggestWord(AWordStem, ASuggest) then
          begin
            Result := nil;
            Exit;
          end;
          ASyllableCountStore2 := ASyllableCount;
          AWordPartIndexStore2 := AWordPartIndex;
          HungarianDecreaseSyllableCount(ASyllableCount);
          CompoundRootCheck(AWordStem, AWordPartIndex);
          if (AWordStem <> nil) and
             (AWordStem.Compound or AWordStem.CompoundEnd) and
              IsSyllablesAndCompoundPartsWithinLimits(AWordPartIndex + 1,
                ASyllableCount + GetSyllableCount(AWordStem.WordStem,
                AWordStem.WordStemLength)) and
              ((ATableIndex <> 0) or
              not IsForbiddenByCheckCompoundPattern(AWord, ACompoundWordJoint, AFirstPartWordStem, AWordStem)) and
              (not FCheckCompoundDup or (AWordStem <> AFirstPartWordStem)) and
              IsWordPartSuitsCheckCompoundPatternTable(AWordStem, ATableIndex, False) then
          begin
            Result := AFirstPartWordStem;
            Exit;
          end;
          ASyllableCount := ASyllableCountStore2;
          AWordPartIndex := AWordPartIndexStore2;
          ATakenSuffix := nil;
          FSavedSuffixInfo.Flag := dxNullFlag;
          if FCompoundFlag <> 0 then
            AWordStem := AffixCheck(AWord + AInitialWordJoint, StrLen(AWord + AInitialWordJoint),
              ATakenPrefix, ATakenSuffix, FCompoundFlag, cwpLast)
          else
            AWordStem := nil;
          if (AWordStem = nil) and (FCompoundEnd <> 0) then
          begin
            ATakenSuffix := nil;
            ATakenPrefix := nil;
            AWordStem := AffixCheck(AWord + AInitialWordJoint, StrLen(AWord + AInitialWordJoint),
              ATakenPrefix, ATakenSuffix, FCompoundEnd, cwpLast);
          end;
          if (AWordStem = nil) and (FCompoundRuleTable.Count <> 0) and (AWords <> nil) then
          begin
            AWordStem := AffixCheck(AWord + AInitialWordJoint, StrLen(AWord + AInitialWordJoint),
              ATakenPrefix, ATakenSuffix, 0, cwpLast);
            if (AWordStem <> nil) and IsWordPartsSuitCompoundRule(AWords, AWordIndex + 1, AWordStem, nil, True) then
            begin
              Result := AFirstPartWordStem;
              Exit;
            end;
            AWordStem := nil;
          end;
          if (AWordStem <> nil) and ((ATableIndex <> 0) and
            not AWordStem.IsCompatibleWithFlag(FCheckCompoundPatternTable[ATableIndex - 1].Condition2) or
            (ATableIndex = 0) and
            IsForbiddenByCheckCompoundPattern(AWord, AInitialWordJoint, AFirstPartWordStem, AWordStem) or
            IsAffixesForbidden(ATakenPrefix, ATakenSuffix))  then
            AWordStem := nil;
          if IsForbiddenOrNoSuggestWord(AWordStem, ASuggest) then
          begin
            Result := nil;
            Exit;
          end;
          if IsHungarian then
          begin
            CheckHungarianSyllableCount(AWord + AInitialWordJoint, AWordStem, ASyllableCount);
            if (ATakenPrefix <> nil) and
              (GetSyllableCount(ATakenPrefix.AppendString, StrLen(ATakenPrefix.AppendString)) > 1) then
              Inc(AWordPartIndex);
          end;
          CompoundRootCheck(AWordStem, AWordPartIndex);
          if (AWordStem <> nil) and
            IsSyllablesAndCompoundPartsWithinLimits(AWordPartIndex + 1, ASyllableCount) and
            (not FCheckCompoundDup or (AWordStem <> AFirstPartWordStem)) then
            begin
              Result := AFirstPartWordStem;
              Exit;
            end;
          ASyllableCount := ASyllableCountStore2;
          AWordPartIndex := AWordPartIndexStore2;
          if AWordPartIndex < AWordPartMaxCount then
          begin
            AWordStem := CompoundCheck(AInitialWordForm + AInitialWordJoint, AWordPartIndex + 1,
              ASyllableCount, AWordPartMaxCount, AWordIndex + 1, AWords, False, ASuggest);
            if (AWordStem <> nil) and (
               not ((ATableIndex = 0) xor IsForbiddenByCheckCompoundPattern(AWord,
                 AInitialWordJoint, AFirstPartWordStem, AWordStem))) then
              AWordStem := nil;
          end
          else
            AWordStem := nil;
          if AWordStem <> nil then
          begin
            Result := AFirstPartWordStem;
            Exit;
          end;
        until not ADoubleCharacter or ACheckWordWithoutTripleRemoving;
        if ACheckWordWithoutTripleRemoving then
        begin
          Inc(ACompoundWordJoint);
          ACheckWordWithoutTripleRemoving := False;
          ADoubleCharacter := False;
        end;
      end;
      Inc(ATableIndex);
    until not (FCheckCompoundPatternTable.IsSimplified and (ATableIndex <= FCheckCompoundPatternTable.Count));
    ATableIndex := 0;
    AWordPartIndex := AWordPartIndexStore;
    ASyllableCount := ASyllableCountStore;
    Inc(ACompoundWordJoint);
  end;
end;

function TdxHunspellAffixManager.ProcessSuffixCheck(const AWord: PWideChar; AWordLength: Integer;
  const AOptions: TdxAffixOptions; APrefix: TdxPrefix; var ATakenSuffix: TdxSuffix;
  const ACompatibleFlag: Word = dxNullFlag; const ACompoundFlag: Word = dxNullFlag;
  ACompoundWordPart: TdxCompoundWordPart = cwpNone): TdxHunspellWordStem;
var
  ASuffix: TdxSuffix;
  AIncompatibleFlag: Word;
begin
  Result := nil;
  AIncompatibleFlag := IfThen(ACompoundWordPart <> cwpNone, dxNullFlag, FInCompoundOnly);
  ASuffix := TdxSuffix(FSuffixTableArrangedByAppendString[0]);
  while (ASuffix <> nil) and (Result = nil) do
  begin
    if (ACompatibleFlag = 0) or (ASuffix.CompatibleFlags <> nil) then
    begin
      if ASuffix.CanUseInTheMiddleOfCompoundWords(ACompoundWordPart) and
        AreAffixesBothCircumfixesOrNot(APrefix, ASuffix) and
        ASuffix.CanUseInSimpleWordsOrIsFogemorpheme(ACompoundWordPart) and
        ((ACompatibleFlag <> dxNullFlag) or not ASuffix.NeedAffix or
        (APrefix <> nil) and not APrefix.NeedAffix) then
      begin
        Result := ASuffix.CheckWord(AWord, AWordLength, AOptions, APrefix,
          ACompatibleFlag, ACompoundFlag, AIncompatibleFlag);
        if Result <> nil then
          ATakenSuffix := ASuffix;
      end;
    end;
    ASuffix := TdxSuffix(ASuffix.Next);
  end;
  if Result = nil then
  begin
    ASuffix := TdxSuffix(FSuffixTableArrangedByAppendString[Ord((AWord + AWordLength - 1)^)]);
    while (ASuffix <> nil) and (Result = nil) do
    begin
      if IsTrailingReversedSubset(ASuffix.AppendString, AWord + AWordLength - 1, AWordLength) then
      begin
        if ASuffix.CanUseInTheMiddleOfCompoundWords(ACompoundWordPart) and
          AreAffixesBothCircumfixesOrNot(APrefix, ASuffix) and
          ASuffix.CanUseInSimpleWordsOrIsFogemorpheme(ACompoundWordPart) and
          ((ACompatibleFlag <> 0) or not ASuffix.NeedAffix or
          (APrefix <> nil) and not APrefix.NeedAffix) then
        begin
          Result := ASuffix.CheckWord(AWord, AWordLength, AOptions, APrefix,
            ACompatibleFlag, ACompoundFlag, AIncompatibleFlag);
          if Result <> nil then
          begin
            ATakenSuffix := ASuffix;
            FSavedSuffixInfo.Flag := ASuffix.Flag;
            if ASuffix.CompatibleFlags = nil then
              FSavedSuffixInfo.AppendString := ASuffix.AppendString;
          end;
        end;
        ASuffix := TdxSuffix(ASuffix.NextSimilar);
      end
      else
        ASuffix := TdxSuffix(ASuffix.NextDifferent);
    end;
  end;
end;

function TdxHunspellAffixManager.ProcessTwoSuffixCheck(const AWord: PWideChar;
  AWordLength: Integer; const AOptions: TdxAffixOptions; APrefix: TdxPrefix;
  var ATakenSuffix: TdxSuffix; const ACompoundFlag: Word = dxNullFlag): TdxHunspellWordStem;
var
  ASuffix: TdxSuffix;
begin
  Result := nil;
  ASuffix := TdxSuffix(FSuffixTableArrangedByAppendString[0]);
  while (ASuffix <> nil) and (Result = nil) do
  begin
    if FCompatibleFlags[ASuffix.Flag] then
      Result := ASuffix.MakeInitialWordFormAndProcessSuffixCheck(AWord,
        AWordLength, AOptions, APrefix, ATakenSuffix, ACompoundFlag);
    ASuffix := TdxSuffix(ASuffix.Next);
  end;
  if Result = nil then
  begin
    ASuffix := TdxSuffix(FSuffixTableArrangedByAppendString[Ord((AWord + AWordLength - 1)^)]);
    while (ASuffix <> nil) and (Result = nil) do
    begin
      if IsTrailingReversedSubset(ASuffix.AppendString, AWord + AWordLength - 1, AWordLength) then
      begin
        if FCompatibleFlags[ASuffix.Flag] then
        begin
          Result := ASuffix.MakeInitialWordFormAndProcessSuffixCheck(AWord,
            AWordLength, AOptions, APrefix, ATakenSuffix, ACompoundFlag);
          if Result <> nil then
          begin
            FSavedSuffixInfo.Flag := ASuffix.Flag;
            if ASuffix.CompatibleFlags = nil then
              FSavedSuffixInfo.AppendString := ASuffix.AppendString;
          end;
        end;
        ASuffix := TdxSuffix(ASuffix.NextSimilar);
      end
      else
        ASuffix := TdxSuffix(ASuffix.NextDifferent);
    end;
  end;
end;

function TdxHunspellAffixManager.AffixCheck(const AWord: PWideChar; AWordLength: Integer;
  var ATakenPrefix: TdxPrefix; var ATakenSuffix: TdxSuffix; const ACompoundFlag: Word = 0;
      ACompoundWordPart: TdxCompoundWordPart = cwpNone): TdxHunspellWordStem;
begin
  Result := ProcessPrefixCheck(AWord, AWordLength, ATakenPrefix, ATakenSuffix,
    ACompoundWordPart, ACompoundFlag);
  if Result = nil then
  begin
    Result := ProcessSuffixCheck(AWord, AWordLength, [], nil, ATakenSuffix,
      dxNullFlag, ACompoundFlag, ACompoundWordPart);
    if FCompatibleFlagsExist then
    begin
      ATakenSuffix := nil;
      ATakenPrefix := nil;
      if Result = nil then
      begin
        Result := ProcessTwoSuffixCheck(AWord, AWordLength, [], nil, ATakenSuffix, ACompoundFlag);
        if Result = nil then
          Result := ProcessPrefixWithSuffixesCheck(AWord, AWordLength,
            ATakenPrefix, ATakenSuffix, cwpNone, ACompoundFlag);
      end;
    end;
  end;
end;

procedure TdxHunspellAffixManager.SetCompatibleFlags(AIndex: Integer; AValue: Boolean);
begin
  FCompatibleFlags[AIndex] := AValue;
end;

procedure SetupGuessByIndex(AWords: PdxGuessWordArray; AIndex: Integer; AWord, AOrig: PWideChar;
  AAllow: Boolean);
var
  AItem: PdxGuessWord;
begin
  AItem := @AWords[AIndex];
  AItem^.Word := AWord;
  AItem^.Orig := AOrig;
  AItem^.Allow := AAllow;
end;

function TdxHunspellAffixManager.ExpandWordStem(AWords: PdxGuessWordArray;
  AWordCount: Integer; const ARoot: PWideChar; ARootLength: Integer;
  const AFlags: TdxHunspellFlags; ABad: PWideChar; ABadLength: Integer;
  APhonetic: PWideChar): Integer;
var
  AIndex: Integer;
  I, J, K: Integer;
  APrefix: TdxPrefix;
  ANewWord, AWord: PWideChar;
begin
  AIndex := 0;
  if (AIndex < AWordCount) and not ((NeedAffix <> 0) and AFlags.ContainsFlag(NeedAffix) or
    (InCompoundOnly <> 0) and AFlags.ContainsFlag(InCompoundOnly)) then
  begin
    SetupGuessByIndex(AWords, AIndex, StrNew(ARoot), nil, False);
    Inc(AIndex);
    if (APhonetic <> nil) and (AIndex < AWordCount) then
    begin
      SetupGuessByIndex(AWords, AIndex, StrNew(APhonetic), StrNew(ARoot), False);
      Inc(AIndex);
    end;
  end;
  ProcessSuffixes(ARoot, ARootLength, AFlags, AIndex, AWords, AWordCount, ABad, ABadLength, APhonetic);
  K := AIndex;
  for I := 1 to K - 1 do
    if AWords[I].Allow then
    begin
      for J := 0 to AFlags.Length - 1 do
      begin
        APrefix := TdxPrefix(FPrefixTableIndexedByFlag[AFlags[J]]);
        while APrefix <> nil do
        begin
          if (APrefix.Flag = AFlags[J]) and APrefix.PrefixSuffixUnion and
            ((APrefix.AppendStringLength = 0) or ((ABadLength > APrefix.AppendStringLength) and
            (StrLComp(APrefix.AppendString, ABad, APrefix.AppendStringLength) = 0))) then
          begin
            AWord := AWords[I].Word;
            ANewWord := APrefix.GetWordWithAffix(AWord, StrLen(AWord));
            if ANewWord <> nil then
            begin
              if AIndex < AWordCount then
              begin
                SetupGuessByIndex(AWords, AIndex, ANewWord, nil, APrefix.PrefixSuffixUnion);
                Inc(AIndex);
              end
              else
                StrDispose(ANewWord);
            end;
          end;
          APrefix := TdxPrefix(APrefix.NextByFlag);
        end;
      end;
    end;
  ProcessPrefixes(ARoot, ARootLength, AFlags, AWords, AWordCount, AIndex, ABad, ABadLength);
  Result := AIndex;
end;

procedure TdxHunspellAffixManager.ProcessSuffixes(const ARoot: PWideChar; ARootLength: Integer;
  const AFlags: TdxHunspellFlags; var AIndex: Integer; AWords: PdxGuessWordArray; AWordCount: Integer;
  ABad: PWideChar; ABadLength: Integer; APhonetic: PWideChar);
var
  I: Integer;
  ASuffix: TdxSuffix;
  ATemp: TdxLineBuffer;
  ANewWord: PWideChar;
begin
  for I := 0 to AFlags.Length - 1 do
  begin
    ASuffix := TdxSuffix(FSuffixTableIndexedByFlag[AFlags[I]]);
    while ASuffix <> nil do
    begin
      if (ASuffix.Flag = AFlags[I]) and ((ASuffix.AppendStringLength = 0) or
        ((ABadLength > ASuffix.AppendStringLength) and
        StrEquals(ASuffix.ReverseAppendString, ABad + ABadLength - ASuffix.AppendStringLength))) and
        not (ASuffix.NeedAffix or ASuffix.Circumfix or ASuffix.InCompoundOnly) then
      begin
        ANewWord := ASuffix.GetWordWithAffix(ARoot, ARootLength);
        if ANewWord <> nil then
        begin
          if AIndex < AWordCount then
          begin
            SetupGuessByIndex(AWords, AIndex, ANewWord, nil, ASuffix.PrefixSuffixUnion);
            Inc(AIndex);
            if (APhonetic <> nil) and (AIndex < AWordCount) then
            begin
              StrCopy(ATemp, APhonetic);
              StrCat(ATemp, ASuffix.AppendString);
              StrReverse(ATemp + StrLen(APhonetic));
              SetupGuessByIndex(AWords, AIndex, StrNew(ATemp), StrNew(ANewWord), False);
              Inc(AIndex);
            end;
          end
          else
            StrDispose(ANewWord);
        end;
      end;
      ASuffix := TdxSuffix(ASuffix.NextByFlag);
    end;
  end;
end;

procedure TdxHunspellAffixManager.ProcessPrefixes(const ARoot: PWideChar; ARootLength: Integer;
  const AFlags: TdxHunspellFlags; AWords: PdxGuessWordArray; AWordCount:Integer;
  var AIndex: Integer; ABad: PWideChar; ABadLength: Integer);
var
  I: Integer;
  APrefix: TdxPrefix;
  ANewWord: PWideChar;
begin
  for I := 0 to AFlags.Length - 1 do
  begin
    APrefix := TdxPrefix(FPrefixTableIndexedByFlag[AFlags[I]]);
    while APrefix <> nil do
    begin
      if (APrefix.Flag = AFlags[I]) and ((APrefix.AppendStringLength = 0) or
        ((ABadLength > APrefix.AppendStringLength) and
        (StrLComp(APrefix.AppendString, ABad, APrefix.AppendStringLength) = 0))) and
        not (APrefix.NeedAffix or APrefix.Circumfix or APrefix.InCompoundOnly) then
      begin
        ANewWord := APrefix.GetWordWithAffix(ARoot, ARootLength);
        if ANewWord <> nil then
        begin
          if AIndex < AWordCount then
          begin
            SetupGuessByIndex(AWords, AIndex, ANewWord, nil, APrefix.PrefixSuffixUnion);
            Inc(AIndex);
          end
          else
            StrDispose(ANewWord);
        end;
      end;
      APrefix := TdxPrefix(APrefix.NextByFlag);
    end;
  end;
end;

function TdxHunspellAffixManager.IsAffixKeyUnique(AFlag: Word; IsPrefix: Boolean): Boolean;
begin
  Result := (IsPrefix and (FExistentAffixFlags[AFlag] and dxExistentPrefixFlag = 0)) or
    (FExistentAffixFlags[AFlag] and dxExistentSuffixFlag = 0);
end;

procedure TdxHunspellAffixManager.SetAffixKeyExist(AFlag: Word;
  IsPrefix: Boolean);
begin
  if IsPrefix then
    FExistentAffixFlags[AFlag] := FExistentAffixFlags[AFlag] or dxExistentPrefixFlag
  else
    FExistentAffixFlags[AFlag] := FExistentAffixFlags[AFlag] or dxExistentSuffixFlag;
end;

procedure TdxHunspellAffixManager.UpdateAffixesOptions(ATable: TAffixTable);
var
  ACurrentAffix: TdxAffix;
  I: Integer;
begin
  for I := 0 to dxAffixTableSize - 1 do
  begin
    ACurrentAffix := ATable[I];
    while ACurrentAffix <> nil do
    begin
      UpdateAffixOptions(ACurrentAffix);
      ACurrentAffix := ACurrentAffix.NextByFlag;
    end;
  end;
end;

procedure TdxHunspellAffixManager.UpdateAffixOptions(AAffix: TdxAffix);
var
  AAffixOptions: TdxAffixOptions;
begin
  AAffixOptions := AAffix.Options;
  if AAffix.IsCompatibleWithFlag(FCompoundMiddle) then
    Include(AAffixOptions, aoCompoundMiddle);
  if AAffix.IsCompatibleWithFlag(FCompoundEnd) then
    Include(AAffixOptions, aoCompoundEnd);
  if AAffix.IsCompatibleWithFlag(FInCompoundOnly) then
    Include(AAffixOptions, aoInCompoundOnly);
  if AAffix.IsCompatibleWithFlag(FNeedAffix) then
    Include(AAffixOptions, aoNeedAffix);
  if AAffix.IsCompatibleWithFlag(FCompoundForbid) then
    Include(AAffixOptions, aoCompoundForbid);
  if AAffix.IsCompatibleWithFlag(FCompoundPermit) then
    Include(AAffixOptions, aoCompoundPermit);
  if AAffix.IsCompatibleWithFlag(FCircumfix) then
    Include(AAffixOptions, aoCircumfix);
  AAffix.Options := AAffixOptions;
end;

procedure TdxHunspellAffixManager.UpdateWordStemOptions;
var
  ATableIndex: Integer;
  AWordStem: TdxHunspellWordStem;
  AWordStemOptions: TdxWordStemOptions;
begin
  AWordStem := FWordStemManager.GetFirstItem(ATableIndex);
  while AWordStem <> nil do
  begin
    AWordStemOptions := [];
    if AWordStem.IsCompatibleWithFlag(FNeedAffix) then
      Include(AWordStemOptions, wsoNeedAffix);
    if AWordStem.IsCompatibleWithFlag(FNoSuggest) then
      Include(AWordStemOptions, wsoNoSuggest);
    if AWordStem.IsCompatibleWithFlag(FCompoundFlag) then
      Include(AWordStemOptions, wsoCompound);
    if AWordStem.IsCompatibleWithFlag(FCompoundBegin) then
      Include(AWordStemOptions, wsoCompoundBegin);
    if AWordStem.IsCompatibleWithFlag(FCompoundMiddle) then
      Include(AWordStemOptions, wsoCompoundMiddle);
    if AWordStem.IsCompatibleWithFlag(FCompoundEnd) then
      Include(AWordStemOptions, wsoCompoundEnd);
    if AWordStem.IsCompatibleWithFlag(FCompoundRoot) then
      Include(AWordStemOptions, wsoCompoundRoot);
    if AWordStem.IsCompatibleWithFlag(FInCompoundOnly) then
      Include(AWordStemOptions, wsoInCompoundOnly);
    if AWordStem.IsCompatibleWithFlag(FKeepCase) then
      Include(AWordStemOptions, wsoKeepCase);
    AWordStem.UpdateOptions(AWordStemOptions);
    AWordStem := FWordStemManager.GetNextItem(ATableIndex, AWordStem);
  end;
end;

procedure TdxHunspellAffixManager.Load(AStream: TStream);
begin
  ParseGrammar(AStream);
  UpdateAffixesOptions(FPrefixTableIndexedByFlag);
  UpdateAffixesOptions(FSuffixTableIndexedByFlag);
  AffixTreeToList(FPrefixTableArrangedByAppendString);
  AffixTreeToList(FSuffixTableArrangedByAppendString);
  LinkAffixesIntoSubsets(FPrefixTableArrangedByAppendString);
  LinkAffixesIntoSubsets(FSuffixTableArrangedByAppendString);
  if FKeyboardString = nil then
    FKeyboardString := StrNew(dxDefaultHunspellKeyboardString);
  if FCompoundPartMin = -1 then
    FCompoundPartMin := dxDefaultCompoundPartLength;
end;

function TdxHunspellAffixManager.Lookup(const AWord: PWideChar): TdxHunspellWordStem;
begin
  Result := FWordStemManager.Lookup(AWord);
end;

function TdxHunspellAffixManager.ParseCompoundSyllable(ALine: PWideChar;
  AAffixFileReader: TdxHunspellReader): Boolean;
const
  aeiouAEIOU: PWideChar = 'aeiouAEIOU';
var
  ALineCursor, APiece: PWideChar;
  I, APartCount: Integer;
begin
  Result := True;
  ALineCursor := ALine;
  APiece := StrSeparate(@ALineCursor, #0);
  APartCount := 0;
  I := 0;
  while APiece <> nil do
  begin
    if APiece^ <> #0 then
    begin
      case I of
        0: Inc(APartCount);
        1: begin
             FMaxCompoundSyllable := StrInt(APiece);
             Inc(APartCount);
           end;
        2: begin
             FCompoundVowels := StrNew(APiece);
             Inc(APartCount);
           end;
      end;
      Inc(I);
    end;
    APiece := StrSeparate(@ALineCursor, #0);
  end;
  if APartCount < 2 then
  begin
    Result := False;
    Exit;
  end;
  if APartCount = 2 then
    FCompoundVowels := StrNew(aeiouAEIOU);
end;

function TdxHunspellAffixManager.ParseRep(ALine: PWideChar; AAffixFileReader: TdxHunspellReader): Boolean;
begin
  Result := FReplaceTable.ReadData(ALine, AAffixFileReader);
end;

function TdxHunspellAffixManager.AreAffixesBothCircumfixesOrNot(APrefix, ASuffix: TdxAffix): Boolean;
begin
  Result := (FCircumfix = dxNullFlag) or
    not (((APrefix <> nil) and APrefix.Circumfix) xor ASuffix.Circumfix);
end;

function TdxHunspellAffixManager.CanBeFirstPartInCompoundWord(
  AWordStem: TdxHunspellWordStem; AWordPartIndex: Cardinal): Boolean;
begin
  Result := AWordStem.Compound or
    ((AWordPartIndex = 0) and AWordStem.CompoundBegin) or
    ((AWordPartIndex > 0) and AWordStem.CompoundMiddle);
end;

procedure TdxHunspellAffixManager.CheckHungarianSyllableCount(AWord: PWideChar;
  AWordStem: TdxHunspellWordStem; var ASyllableCount: SmallInt);
begin
  Inc(ASyllableCount, GetSyllableCount(AWord, StrLen(AWord)));
  if FSavedSuffixInfo.AppendString <> nil then
    Dec(ASyllableCount, GetSyllableCount(FSavedSuffixInfo.AppendString, StrLen(FSavedSuffixInfo.AppendString)));
  if FSyllableNum <> nil then
    case Chr(FSavedSuffixInfo.Flag) of
      dxHungarianFlagC: Inc(ASyllableCount, 2);
      dxHungarianFlagJ: Inc(ASyllableCount, 1);
      dxHungarianFlagI: if AWordStem.IsCompatibleWithFlag(Ord(dxHungarianFlagJ)) then
                          Inc(ASyllableCount, 1);
    end;
end;

procedure TdxHunspellAffixManager.CheckWordsCompatibilityWithCompoundRule(
  const AWords: PdxWordStemTable; var ABackTrackingTable: TdxBackTrackingTable;
  AWordCount: SmallInt; ACompoundRuleTableIndex: Integer; var AFlagIndex,
  AWordIndex, ABackTrackingIndex: SmallInt;
  var AIsWordStemCompatibleWithCompoundRule,
  AIsAllWordsCompatibleWithCompoundRule: Boolean);
var
  AMaxWordIndex: SmallInt;
  AMetacharacterIndex: Integer;
  AFlag: Word;
begin
  while (AFlagIndex < FCompoundRuleTable[ACompoundRuleTableIndex].Length) and (AWordIndex <= AWordCount) do
  begin
    AMetacharacterIndex := AFlagIndex + 1;
    AFlag := 0;
    if AMetacharacterIndex < FCompoundRuleTable[ACompoundRuleTableIndex].Length then
      AFlag := FCompoundRuleTable[ACompoundRuleTableIndex].Pattern[AMetacharacterIndex];
    if (AMetacharacterIndex < FCompoundRuleTable[ACompoundRuleTableIndex].Length) and
      FCompoundRuleTable.IsPartOfCompoundWordFlag(AFlag) then
    begin

      AMaxWordIndex := IfThen(AFlag = Ord(dxCompoundRuleOneWordFlag), AWordIndex, AWordCount);

      Inc(AFlagIndex, 2);
      ABackTrackingTable[ABackTrackingIndex].MetacharacterPosition := AFlagIndex;
      ABackTrackingTable[ABackTrackingIndex].WordsIndex := AWordIndex;
      AIsAllWordsCompatibleWithCompoundRule :=
        GetIsAllWordsCompatibleWithCompoundRule(AWords, AWordIndex,
          AMaxWordIndex, FCompoundRuleTable[ACompoundRuleTableIndex].Pattern[AFlagIndex - 2]);
      if AWordIndex <= AWordCount then
        AIsAllWordsCompatibleWithCompoundRule := False;
      ABackTrackingTable[ABackTrackingIndex].MatchedWordCount :=
        AWordIndex - ABackTrackingTable[ABackTrackingIndex].WordsIndex;
      if ABackTrackingTable[ABackTrackingIndex].MatchedWordCount > 0 then
        Inc(ABackTrackingIndex);
      if AIsAllWordsCompatibleWithCompoundRule then
        Break;
    end
    else
    begin
      AIsAllWordsCompatibleWithCompoundRule := True;
      if (AWords[AWordIndex] = nil) or
        not AWords[AWordIndex].IsCompatibleWithFlag(FCompoundRuleTable[ACompoundRuleTableIndex].Pattern[AFlagIndex]) then
      begin
        AIsWordStemCompatibleWithCompoundRule := False;
        Break;
      end;
      Inc(AFlagIndex);
      Inc(AWordIndex);
      if (FCompoundRuleTable[ACompoundRuleTableIndex].Length = AFlagIndex) and not(AWordIndex > AWordCount) then
        AIsWordStemCompatibleWithCompoundRule := False;
    end;
  end;
end;

function TdxHunspellAffixManager.CompoundPartAffixCheck(
  const AInitialWordForm: TWordFormArray; const AInitialWordJoint: Integer;
  var AWordStem: TdxHunspellWordStem; const AWordPartIndex: SmallInt;
  const ACompoundWordPart: TdxCompoundWordPart; var ATakenPrefix: TdxPrefix;
  var ATakenSuffix: TdxSuffix): Boolean;
begin
  Result := (AWordStem <> nil) or
    (((AWordPartIndex = 0) and (FCompoundBegin <> 0) and
      ProcessSuffixOrPrefixCheck(AInitialWordForm, AInitialWordJoint,
        FCompoundBegin, ACompoundWordPart, AWordStem, ATakenPrefix, ATakenSuffix)) or
    ((AWordPartIndex > 0) and (FCompoundMiddle <> 0) and
      ProcessSuffixOrPrefixCheck(AInitialWordForm, AInitialWordJoint,
        FCompoundMiddle, ACompoundWordPart, AWordStem, ATakenPrefix, ATakenSuffix)));
end;

procedure TdxHunspellAffixManager.CorrectBackTrackingIndex(
  var ABackTrackingTable: TdxBackTrackingTable; var AFlagIndex, AWordIndex,
  ABackTrackingIndex: SmallInt);
begin
  repeat
    Dec(ABackTrackingTable[ABackTrackingIndex - 1].MatchedWordCount);
    AFlagIndex := ABackTrackingTable[ABackTrackingIndex - 1].MetacharacterPosition;
    AWordIndex := ABackTrackingTable[ABackTrackingIndex - 1].WordsIndex +
      ABackTrackingTable[ABackTrackingIndex - 1].MatchedWordCount;
    Dec(ABackTrackingIndex);
  until not((ABackTrackingTable[ABackTrackingIndex].MatchedWordCount < 0) and
    (ABackTrackingIndex > 0));
  if not(ABackTrackingTable[ABackTrackingIndex].MatchedWordCount < 0) then
    Inc(ABackTrackingIndex);
end;

procedure TdxHunspellAffixManager.CompoundRootCheck(
  AWordStem: TdxHunspellWordStem; var AWordPartIndex: SmallInt);
begin
  if (AWordStem <> nil) and AWordStem.CompoundRoot then
    Inc(AWordPartIndex);
end;

function TdxHunspellAffixManager.EndOfCompoundRulePattern(ACompoundRuleTableIndex: Integer;
  AFlagIndex: Integer): Boolean;
begin
  while (AFlagIndex + 1 < FCompoundRuleTable[ACompoundRuleTableIndex].Length) and
      FCompoundRuleTable.IsPartOfCompoundWordFlag(FCompoundRuleTable[ACompoundRuleTableIndex].Pattern[AFlagIndex + 1]) do
    Inc(AFlagIndex, 2);
  Result := FCompoundRuleTable[ACompoundRuleTableIndex].Length <= AFlagIndex;
end;

function TdxHunspellAffixManager.FindSuitableCheckCompoundPatternTableIndex(
  var ATableIndex: Integer; AWordSecondPart: PWideChar): Boolean;
begin
  while (ATableIndex <= FCheckCompoundPatternTable.Count) and
        ((FCheckCompoundPatternTable[ATableIndex - 1].Pattern3 = nil) or
        (StrLComp(AWordSecondPart, FCheckCompoundPatternTable[ATableIndex - 1].Pattern3,
        StrLen(FCheckCompoundPatternTable[ATableIndex - 1].Pattern3)) <> 0)) do
    Inc(ATableIndex);
  Result := ATableIndex <= FCheckCompoundPatternTable.Count;
end;

function TdxHunspellAffixManager.GetIsAllWordsCompatibleWithCompoundRule(
  const AWords: PdxWordStemTable; var AWordIndex: SmallInt; AWordIndexMax: Integer;
  AFlag: Word): Boolean;
begin
  Result := True;
  while (AWordIndex <= AWordIndexMax) and Result do
  begin
    Result := AWords[AWordIndex].IsCompatibleWithFlag(AFlag);
    if Result then
      Inc(AWordIndex);
  end;
end;

function TdxHunspellAffixManager.IsWordStemCompatibleWithCompoundRule(
  AWordStem: TdxHunspellWordStem): Boolean;
var
  I, J: Integer;
  AFlag: Word;
begin
  Result := False;
  for I := 0 to FCompoundRuleTable.Count - 1 do
    for J := 0 to FCompoundRuleTable[I].Length - 1 do
    begin
      AFlag := FCompoundRuleTable[I].Pattern[J];
      if not FCompoundRuleTable.IsPartOfCompoundWordFlag(AFlag) and
        AWordStem.IsCompatibleWithFlag(AFlag) then
      begin
        Result := True;
        Exit;
      end;
    end;
end;

procedure TdxHunspellAffixManager.GetWordStemSuitableForAffixAndCompoundRules(
  var AWordStem: TdxHunspellWordStem;
  AWordPartIndex: SmallInt; ATableIndex: Integer; var AWords: PdxWordStemTable;
  AWordIndex: SmallInt; ACompoundRuleWords: PdxWordStemTable; AIsFirstWordPart: Boolean);
begin
  while (AWordStem <> nil) and
    (AWordStem.NeedAffix or
    not ((IsWordPartSuitsCompoundFlags(AWordStem, AWordPartIndex, AIsFirstWordPart, AWords) or
    ((AWords <> nil) or (AWordPartIndex = 0)) and
    IsWordPartsSuitCompoundRule(AWords, AWordIndex, AWordStem, ACompoundRuleWords, AIsFirstWordPart)) and
    IsWordPartSuitsCheckCompoundPatternTable(AWordStem, ATableIndex, AIsFirstWordPart))) do
    AWordStem := AWordStem.NextHomonym;
end;

procedure TdxHunspellAffixManager.GenerateCompoundFirstPartWithAffixes(
  var AWordStem: TdxHunspellWordStem; const AInitialWordForm: TWordFormArray;
  AInitialWordJoint: Integer; var ATakenPrefix: TdxPrefix; var ATakenSuffix: TdxSuffix;
  ACompoundWordPart: TdxCompoundWordPart);
begin
  if (FCompoundFlag <> 0) then
  begin
    AWordStem := ProcessPrefixCheck(AInitialWordForm, AInitialWordJoint,
      ATakenPrefix, ATakenSuffix, ACompoundWordPart, FCompoundFlag);
    if AWordStem = nil then
    begin
      AWordStem := ProcessSuffixCheck(AInitialWordForm, AInitialWordJoint,
        [], nil, ATakenSuffix, dxNullFlag, FCompoundFlag, ACompoundWordPart);
      if (AWordStem <> nil) and (ACompoundWordPart = cwpFirst){not AIsHungarianMoveRule} and
        (ATakenSuffix.CompoundForbid or ATakenSuffix.CompoundEnd) then
        AWordStem := nil;
    end;
  end;
end;

function TdxHunspellAffixManager.IsAffixesForbidden(ATakenPrefix: TdxPrefix;
  ATakenSuffix: TdxSuffix): Boolean;
begin
  Result := (ATakenPrefix <> nil) and ATakenPrefix.CompoundForbid or
            (ATakenSuffix <> nil) and ATakenSuffix.CompoundForbid;
end;

function TdxHunspellAffixManager.IsAffixesIncompatibleWithCompoundParts(
  ATakenPrefix: TdxPrefix; ATakenSuffix: TdxSuffix; AIsAffixChecked: Boolean;
  AWordPartIndex: SmallInt): Boolean;
begin
  Result := not AIsAffixChecked and (FCompoundEnd <> 0) and
    (((ATakenPrefix <> nil) and ATakenPrefix.CompoundEnd) or
    ((ATakenSuffix <> nil) and ATakenSuffix.CompoundEnd)) or
    (AWordPartIndex = 0) and (FCompoundMiddle <> 0) and
    (((ATakenPrefix <> nil) and ATakenPrefix.CompoundMiddle) or
    ((ATakenSuffix <> nil) and ATakenSuffix.CompoundMiddle));
end;

function TdxHunspellAffixManager.IsWordPartSuitsCheckCompoundPatternTable(
  AWordStem: TdxHunspellWordStem; ATableIndex: Integer; AIsFirstWordPart: Boolean): Boolean;
var
  AFlag: Word;
begin
  Result := ATableIndex = 0;
  if not Result then
  begin
    if AIsFirstWordPart then
      AFlag := FCheckCompoundPatternTable[ATableIndex - 1].Condition
    else
      AFlag := FCheckCompoundPatternTable[ATableIndex - 1].Condition2;
    Result := AWordStem.IsCompatibleWithFlag(AFlag, True);
  end;
end;

function TdxHunspellAffixManager.IsWordPartSuitsCompoundFlags(AWordStem: TdxHunspellWordStem;
  AWordPartIndex: SmallInt; AIsFirstWordPart: Boolean; AWords: PdxWordStemTable): Boolean;
begin
  Result := (AWords = nil) and AWordStem.Compound;
  if not Result then
  begin
    if AIsFirstWordPart then
      Result := ((AWordPartIndex = 0) and AWordStem.CompoundBegin) or
        ((AWordPartIndex <> 0) and (AWords = nil) and AWordStem.CompoundMiddle)
    else
      Result := (AWords = nil) and AWordStem.CompoundEnd;
  end;
end;

function TdxHunspellAffixManager.IsSyllablesAndCompoundPartsWithinLimits(
  AWordPartIndex, ASyllableCount: SmallInt): Boolean;
begin
  Result :=
    ((FCompoundWordMax = -1) or (AWordPartIndex < FCompoundWordMax)) or
    ((FMaxCompoundSyllable <> 0) and (ASyllableCount <= FMaxCompoundSyllable));
end;

function TdxHunspellAffixManager.IsTripleLettersExistAndForbid(AWord: PWideChar; AJoint: Cardinal): Boolean;
begin
  Result := FCheckCompoundTriple and
    (AWord[AJoint - 1] = AWord[AJoint]) and (
    ((AJoint > 1) and (AWord[AJoint - 1] = AWord[AJoint - 2])) or
    ((AJoint + 1 < StrLen(AWord)) and (AWord[AJoint - 1] = AWord[AJoint + 1])));
end;

procedure TdxHunspellAffixManager.MakeWordWithCheckCompoundPatternTable(const AWord: PWideChar;
  AInitialWordForm: PWideChar; ACompoundWordJoint: Integer;
  var AInitialWordJoint, AWordLength: Integer; ATableIndex: Integer);
begin
  StrCopy(AInitialWordForm + ACompoundWordJoint, FCheckCompoundPatternTable[ATableIndex].Pattern);
  Inc(AInitialWordJoint, StrLen(FCheckCompoundPatternTable[ATableIndex].Pattern));
  StrCopy(AInitialWordForm + AInitialWordJoint, FCheckCompoundPatternTable[ATableIndex].Pattern2);
  StrCopy(AInitialWordForm + AInitialWordJoint + StrLen(FCheckCompoundPatternTable[ATableIndex].Pattern2),
    AWord + ACompoundWordJoint + StrLen(FCheckCompoundPatternTable[ATableIndex].Pattern3));
  Inc(AWordLength, StrLen(FCheckCompoundPatternTable[ATableIndex].Pattern) +
    StrLen(FCheckCompoundPatternTable[ATableIndex].Pattern2) -
    StrLen(FCheckCompoundPatternTable[ATableIndex].Pattern3));
end;

function TdxHunspellAffixManager.CheckHungarianFlagsCompatibility(
  AWordStem: TdxHunspellWordStem; AIsHungarianMoveRule: Boolean): Boolean;
begin
  Result := IsHungarian and AIsHungarianMoveRule and (
    AWordStem.IsCompatibleWithFlag(Ord(dxHungarianFlagF)) or
    AWordStem.IsCompatibleWithFlag(Ord(dxHungarianFlagG)) or
    AWordStem.IsCompatibleWithFlag(Ord(dxHungarianFlagH)));
end;

function TdxHunspellAffixManager.CheckHungarianFlagsCompatibilityWithSuffix(var AWordStem: TdxHunspellWordStem;
  const AInitialWordForm: TWordFormArray; ACompoundWordJoint: Integer;
  AIsHungarianMoveRule: Boolean; var ATakenPrefix: TdxPrefix; var ATakenSuffix: TdxSuffix): Boolean;

  function DoAffixCheck: Boolean;
  begin
    AWordStem := AffixCheck(AInitialWordForm, ACompoundWordJoint, ATakenPrefix, ATakenSuffix);
    Result := AWordStem <> nil;
  end;

begin
  Result := (AWordStem = nil) and IsHungarian and AIsHungarianMoveRule and
    DoAffixCheck and ((ATakenSuffix <> nil) and
    (ATakenSuffix.IsCompatibleWithFlag(Ord(dxHungarianFlagX)) or
    ATakenSuffix.IsCompatibleWithFlag(Ord(dxHungarianFlagPercent))));
end;

function TdxHunspellAffixManager.ParseAffix(ALine: PWideChar;
  AAffixFileReader: TdxHunspellReader; AAffixType: TdxAffixType): Boolean;
const
  AffixItemTableClass: array[TdxAffixType] of TdxAffixItemTableClass =
    (TdxPrefixItemTable, TdxSuffixItemTable);
var
  AAffixItem: TdxAffixItemTable;
begin
  AAffixItem := AffixItemTableClass[AAffixType].Create(Self);
  try
    Result := AAffixItem.ReadData(ALine, AAffixFileReader);
    if Result then
      AAffixItem.BuildAffixes;
  finally
    AAffixItem.Free;
  end;
end;

function TdxHunspellAffixManager.GetIsCompoundWordsAvailable: Boolean;
begin
  Result := (FCompoundFlag <> dxNullFlag) or (FCompoundBegin <> dxNullFlag) or
    (FCompoundRuleTable.Count > 0);
end;

function TdxHunspellAffixManager.GetLanguage: Integer;
begin
  Result := FWordStemManager.Language;
end;

function TdxHunspellAffixManager.DecodeFlag(const AFlag: PWideChar): Word;
begin
  case AffixFlagMode of
    afmTwoCharacters:
      Result := Ord(AFlag^) shl 8 + Ord((AFlag + 1)^);
    afmNumber:
      Result := StrInt(AFlag);
  else
    Result := Ord(AFlag^);
  end;
end;

function TdxHunspellAffixManager.EncodeFlag(AFlag: Word): PWideChar;
var
  AEncodedFlag: array [0..9] of Word;
begin
  if AFlag = 0 then
    Result := nil
  else
    if AffixFlagMode = afmNumber then
      Result := IntStr(AFlag)
    else
    begin
      if AffixFlagMode = afmTwoCharacters then
      begin
        AEncodedFlag[0] := WordRec(AFlag).Hi;
        AEncodedFlag[1] := WordRec(AFlag).Lo;
        AEncodedFlag[2] := 0;
      end
      else
      begin
        AEncodedFlag[0] := AFlag;
        AEncodedFlag[1] := 0;
      end;
      Result := StrNew(PWideChar(@AEncodedFlag));
    end;
end;

function TdxHunspellAffixManager.GetReaders: TdxAffixReaderList;
begin
  Result := TdxAffixReaderList.Create(Self);
  Result.Add(TPrefixReader);
  Result.Add(TSuffixReader);
  Result.Add(TRepReader);
  Result.Add(TTryReader);
  Result.Add(TKeyReader);
  Result.Add(TComplexPrefixesReader);
  Result.Add(TCompoundFlagReader);
  Result.Add(TCompoundBeginReader);
  Result.Add(TCompoundEndReader);
  Result.Add(TCompoundMiddleReader);
  Result.Add(TCompoundWordMaxReader);
  Result.Add(TCompoundRootReader);
  Result.Add(TCompoundPermitFlagReader);
  Result.Add(TCompoundForbidFlagReader);
  Result.Add(TCheckCompoundDupReader);
  Result.Add(TCheckCompoundRepReader);
  Result.Add(TCheckCompoundTripleReader);
  Result.Add(TSimplifiedTripleReader);
  Result.Add(TCheckCompoundCaseReader);
  Result.Add(TNoSuggestReader);
  Result.Add(TForbiddenWordReader);
  Result.Add(TLemmaPresentReader);
  Result.Add(TCircumfixReader);
  Result.Add(TInCompoundOnlyReader);
  Result.Add(TPseudoRootReader);
  Result.Add(TNeedAffixReader);
  Result.Add(TCompoundMinReader);
  Result.Add(TCompoundSyllableReader);
  Result.Add(TSyllableNumReader);
  Result.Add(TWordCharsReader);
  Result.Add(TIgnoreReader);
  Result.Add(TIConvReader);
  Result.Add(TOConvReader);
  Result.Add(TCheckCompoundPatternReader);
  Result.Add(TCompoundRuleReader);
  Result.Add(TMapReader);
  Result.Add(TBreakReader);
  Result.Add(TLangReader);
  Result.Add(TVersionReader);
  Result.Add(TNoSplitSuggestionsReader);
  Result.Add(TNgramSuggestionReader);
  Result.Add(TFullStripReader);
  Result.Add(TSuggestionsWithDotsReader);
  Result.Add(TKeepCaseReader);
  Result.Add(TSubStandardReader);
  Result.Add(TCheckSharpsReader);
  Result.Add(TPhoneReader);
  Result.Add(TAffixFlagAliasReader);
  Result.Add(TMorphologicAliasReader);
end;

function TdxHunspellAffixManager.GetInitialReaders: TdxAffixReaderList;
begin
  Result := TdxAffixReaderList.Create(Self);
  Result.Add(TFlagReader);
  Result.Add(TSetReader);
end;

procedure TdxHunspellAffixManager.InitCodePage(AStream: TStream);
var
  ALine: PWideChar;
  AAffixReader: TdxHunspellReader;
  ALineBuffer: TdxLineBuffer;
  AReaders: TdxAffixReaderList;
begin
  FWordStemManager.CodePage := dxDefaultHunspellCodePage; //ISO 8859-1 Latin I;
  AReaders := GetInitialReaders;
  try
    AAffixReader := TdxHunspellReader.Create(AStream, CodePage);
    try
      ALine := @ALineBuffer;
      while AAffixReader.GetNextLine(ALineBuffer, SizeOf(ALineBuffer)) do
        AReaders.ProcessLine(ALine, AAffixReader);
    finally
      AAffixReader.Free;
    end;
  finally
    AReaders.Free;
  end;
end;

procedure TdxHunspellAffixManager.SetComplexPrefixes(Value: Boolean);
begin
  FWordStemManager.ComplexPrefixes := Value;
end;

procedure TdxHunspellAffixManager.SetIgnoredChars(Value: PWideChar);
begin
  FWordStemManager.IgnoredChars := Value;
end;

procedure TdxHunspellAffixManager.SetLanguage(Value: Integer);
begin
  FWordStemManager.Language := Value;
  FIsHungarian := Language = LanguageHungarian;
end;

{ TdxAffix }

constructor TdxAffix.Create(AAffixManager: TdxHunspellAffixManager;
  AAffixItem: PdxAffixItem; AFlag: Word; const AOptions: TdxAffixOptions);
begin
  inherited Create;
  FOptions := AOptions;
  FAffixManager := AAffixManager;
  FFlag := AFlag;
  FStripString := AAffixItem.StripString;
  FAppendString := AAffixItem.AppendString;
  FStripStringLength := AAffixItem.StripStringLength;
  FAppendStringLength := AAffixItem.AppendStringLength;
  FCondition := AAffixItem.Condition;
  FConditionLength := AAffixItem.ConditionLength;
  FMorphologicalDescription := AAffixItem.MorphologicalDescription;
  FCompatibleFlags := AAffixItem.CompatibleFlags;
end;

destructor TdxAffix.Destroy;
begin
  StrFreeAndNil(FAppendString);
  StrFreeAndNil(FStripString);
  StrFreeAndNil(FCondition);
  if (FMorphologicalDescription <> nil) and not (aoAffixMorphologyTable in FOptions) then
    StrDispose(FMorphologicalDescription);
  FreeAndNil(FCompatibleFlags);
  inherited Destroy;
end;

function TdxAffix.CanUseInSimpleWordsOrIsFogemorpheme(ACompoundWordPart: TdxCompoundWordPart): Boolean;
begin
  Result := (ACompoundWordPart <> cwpNone) or not InCompoundOnly;
end;

function TdxAffix.CanUseInTheMiddleOfCompoundWords(ACompoundWordPart: TdxCompoundWordPart): Boolean;
begin
  Result := CanUseInCompoundWordPart(ACompoundWordPart) or CompoundPermit;
end;

function TdxAffix.GetAppendString: PWideChar;
begin
  Result := FAppendString;
end;

function TdxAffix.IsCompatibleWithFlag(AFlag: Word; CompatibleIfNull: Boolean = False): Boolean;
begin
  if AFlag <> dxNullFlag then
    Result := (CompatibleFlags <> nil) and CompatibleFlags.ContainsFlag(AFlag)
  else
    Result := CompatibleIfNull;
end;

function TdxAffix.IsPrefixSuffixUnion(const AOptions: TdxAffixOptions): Boolean;
begin
  Result := aoPrefixSuffixUnion in AOptions;
end;

function TdxAffix.IsWithoutAffixWordLengthCorrect(ALengthWithoutAffix: Integer): Boolean;
begin
  Result := ((ALengthWithoutAffix > 0) or
    (ALengthWithoutAffix = 0) and AffixManager.FullStrip) and
    (ALengthWithoutAffix + FStripStringLength >= FConditionLength);
end;

procedure TdxAffix.SearchConditionGroupEnd(var AConditionCursor: PWideChar);
begin
  while (AConditionCursor^ <> #0) and (AConditionCursor^ <> ']') do
    Inc(AConditionCursor);
end;

function TdxAffix.GetCircumfix: Boolean;
begin
  Result := aoCircumfix in FOptions;
end;

function TdxAffix.GetCompoundEnd: Boolean;
begin
  Result := aoCompoundEnd in FOptions;
end;

function TdxAffix.GetCompoundForbid: Boolean;
begin
  Result := aoCompoundForbid in FOptions;
end;

function TdxAffix.GetCompoundMiddle: Boolean;
begin
  Result := aoCompoundMiddle in FOptions;
end;

function TdxAffix.GetCompoundPermit: Boolean;
begin
  Result := aoCompoundPermit in FOptions;
end;

function TdxAffix.GetInCompoundOnly: Boolean;
begin
  Result := aoInCompoundOnly in FOptions;
end;

function TdxAffix.GetNeedAffix: Boolean;
begin
  Result := aoNeedAffix in FOptions;
end;

function TdxAffix.GetPrefixSuffixUnion: Boolean;
begin
  Result := IsPrefixSuffixUnion(FOptions);
end;

{ TdxPrefix }

function TdxPrefix.CanUseInCompoundWordPart(ACompoundWordPart: TdxCompoundWordPart): Boolean;
begin
  Result := ACompoundWordPart <> cwpLast;
end;

function TdxPrefix.CheckWord(const AWord: PWideChar; AWordLength: Integer; var ATakenSuffix: TdxSuffix;
  ACompoundWordPart: TdxCompoundWordPart; const ACompoundFlag: Word = dxNullFlag): TdxHunspellWordStem;
var
  AInitialWordFormLength: Integer;
  AWordStem: TdxHunspellWordStem;
  AInitialWordForm: TdxLineBuffer;
begin
  Result := nil;
  AInitialWordFormLength := AWordLength - AppendStringLength;
  if IsWithoutAffixWordLengthCorrect(AInitialWordFormLength) then
  begin
    if FStripStringLength <> 0 then
      StrCopy(AInitialWordForm, FStripString);
    StrCopy(AInitialWordForm + FStripStringLength, AWord + AppendStringLength);
    if IsWordSuitableForCondition(AInitialWordForm) then
    begin
      Inc(AInitialWordFormLength, FStripStringLength);
      AWordStem := AffixManager.Lookup(AInitialWordForm);
      while (AWordStem <> nil) and (Result = nil) do
      begin
        if not NeedAffix and AWordStem.IsCompatibleWithFlag(FFlag) and
           (AWordStem.IsCompatibleWithFlag(ACompoundFlag, True) or
           IsCompatibleWithFlag(ACompoundFlag)) then
          Result := AWordStem;
        AWordStem := AWordStem.NextHomonym;
      end;
      if (Result = nil) and PrefixSuffixUnion then
        Result := AffixManager.ProcessSuffixCheck(AInitialWordForm,
          AInitialWordFormLength, [aoPrefixSuffixUnion], Self, ATakenSuffix,
          dxNullFlag, ACompoundFlag, ACompoundWordPart);
    end;
  end;
end;

function TdxPrefix.GetWordWithAffix(const AWord: PWideChar; AWordLength: Integer): PWideChar;
var
  ATempWord: TdxLineBuffer;
  AWordCursor: PWideChar;
begin
  Result := nil;
  if ((AWordLength > FStripStringLength) or ((AWordLength = 0) and AffixManager.FullStrip)) and
    (AWordLength >= FConditionLength) and IsWordSuitableForCondition(AWord) and
    ((FStripStringLength = 0) or (StrLComp(AWord, FStripString, FStripStringLength) = 0)) and
    (dxMaxWordLength > AWordLength + FAppendStringLength - FStripStringLength) then
  begin
    AWordCursor := ATempWord;
    if FAppendStringLength <> 0 then
    begin
      StrCopy(ATempWord, FAppendString);
      Inc(AWordCursor, FAppendStringLength);
    end;
    StrCopy(AWordCursor, AWord + FStripStringLength);
    Result := StrNew(ATempWord);
  end;
end;

function TdxPrefix.MakeInitialWordFormAndProcessTwoSuffixCheck(const AWord: PWideChar;
  AWordLength: Integer; var ATakenSuffix: TdxSuffix;
  ACompoundWordPart: TdxCompoundWordPart;
  const ACompoundFlag: Word = dxNullFlag): TdxHunspellWordStem;
var
  AInitialWordFormLength: Integer;
  AInitialWordForm: TdxLineBuffer;
begin
  Result := nil;
  AInitialWordFormLength := AWordLength - AppendStringLength;
  if IsWithoutAffixWordLengthCorrect(AInitialWordFormLength) then
  begin
    if FStripStringLength <> 0 then
      StrCopy(AInitialWordForm, FStripString);
    StrCopy(AInitialWordForm + FStripStringLength, AWord + AppendStringLength);
    if IsWordSuitableForCondition(AInitialWordForm) then
    begin
      Inc(AInitialWordFormLength, FStripStringLength);
      if PrefixSuffixUnion and (ACompoundWordPart <> cwpFirst) then
        Result := AffixManager.ProcessTwoSuffixCheck(AInitialWordForm, AInitialWordFormLength,
          [aoPrefixSuffixUnion], Self, ATakenSuffix, ACompoundFlag);
    end;
  end;
end;

function TdxPrefix.IsWordSuitableForCondition(AWord: PWideChar): Boolean;
var
  AWordCursor: PWideChar;
  ANegative: Boolean;
  AIsGroup: Boolean;
  AConditionCursor: PWideChar;
begin
  if FConditionLength = 0 then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
  AWordCursor := nil;
  ANegative := False;
  AIsGroup := False;
  AConditionCursor := FCondition;
  while AConditionCursor^ <> #0 do
  begin
    case AConditionCursor^ of
      '[':
        begin
          ANegative := False;
          AIsGroup := False;
          Inc(AConditionCursor);
          AWordCursor := AWord;
        end;
      '^':
        begin
          ANegative := True;
          Inc(AConditionCursor);
        end;
      ']':
        begin
          if ANegative = AIsGroup then
            Exit;
          AWordCursor := nil;
          Inc(AConditionCursor);
          if not AIsGroup then
            Inc(AWord);
          if (AWord^ = #0) and (AConditionCursor^ <> #0) then
            Exit;
        end;
      '.':
        if AWordCursor = nil then
        begin
          Inc(AConditionCursor);
          Inc(AWord);
          if (AWord^ = #0) and (AConditionCursor^ <> #0) then
            Exit;
        end;
    else
      if AWord^ = AConditionCursor^ then
      begin
        Inc(AWord);
        Inc(AConditionCursor);
        if AWordCursor <> nil then
        begin
          AIsGroup := True;
          SearchConditionGroupEnd(AConditionCursor);
        end;
      end
      else
        if AWordCursor <> nil then
          Inc(AConditionCursor)
        else
          Exit;
    end;
  end;
  Result := True;
end;

{ TdxSuffix }

constructor TdxSuffix.Create(AAffixManager: TdxHunspellAffixManager;
  AAffixItem: PdxAffixItem; AFlag: Word; const AOptions: TdxAffixOptions);
begin
  inherited Create(AAffixManager, AAffixItem, AFlag, AOptions);
  FReverseAppendString := StrCopyReverse(FAppendString);
end;

destructor TdxSuffix.Destroy;
begin
  StrDispose(FReverseAppendString);
  inherited Destroy;
end;

function TdxSuffix.CanUseInCompoundWordPart(ACompoundWordPart: TdxCompoundWordPart): Boolean;
begin
  Result := ACompoundWordPart <> cwpFirst;
end;

function TdxSuffix.GetWordWithAffix(const AWord: PWideChar; AWordLength: Integer): PWideChar;
var
  ATempWord: TdxLineBuffer;
begin
  Result := nil;
  if ((AWordLength > FStripStringLength) or (AWordLength = 0) and (AffixManager.FullStrip)) and
    (AWordLength >= FConditionLength) and IsWordSuitableForCondition(AWord + AWordLength, AWord) and
    ((FStripStringLength = 0) or StrEquals(AWord + AWordLength - FStripStringLength, FStripString)) and
    (dxMaxWordLength > AWordLength + AppendStringLength - FStripStringLength) then
  begin
    Move(AWord^, ATempWord, (AWordLength + 1) * SizeOf(WideChar));
    if AppendStringLength <> 0 then
      StrCopy(ATempWord + AWordLength - FStripStringLength, FAppendString)
    else
      (ATempWord + AWordLength - FStripStringLength)^ := #0;
    Result := StrNew(ATempWord);
  end;
end;

function TdxSuffix.CheckWord(const AWord: PWideChar; AWordLength: Integer;
  const AOptions: TdxAffixOptions; APrefix: TdxPrefix;
  const ACompatibleFlag: Word = dxNullFlag; const ACompoundFlag: Word = dxNullFlag;
  const AIncompatibleFlag: Word = dxNullFlag): TdxHunspellWordStem;

  function IsIncompatibleWithFlag(AWordStem: TdxHunspellWordStem; AFlag: Word): Boolean;
  begin
    Result := (AFlag = 0) or not AWordStem.IsCompatibleWithFlag(AFlag);
  end;

var
  AInitialWordFormLength: Integer;
  AWordStem: TdxHunspellWordStem;
  AInitialWordFormEnd: PWideChar;
  AInitialWordForm: TdxLineBuffer;
begin
  Result := nil;
  if IsPrefixSuffixUnion(AOptions) and not PrefixSuffixUnion then
    Exit;
  AInitialWordFormLength := AWordLength - AppendStringLength;
  if IsWithoutAffixWordLengthCorrect(AInitialWordFormLength) then
  begin
    StrCopy(AInitialWordForm, AWord);
    AInitialWordFormEnd := AInitialWordForm + AInitialWordFormLength;
    if FStripStringLength <> 0 then
    begin
      StrCopy(AInitialWordFormEnd, FStripString);
      Inc(AInitialWordFormLength, FStripStringLength);
      AInitialWordFormEnd := AInitialWordForm + AInitialWordFormLength;
    end
    else
      AInitialWordFormEnd^ := #0;
    if IsWordSuitableForCondition(AInitialWordFormEnd, AInitialWordForm) then
    begin
      AWordStem := AffixManager.Lookup(AInitialWordForm);
      if AWordStem <> nil then
      begin
        repeat
          if (AWordStem.IsCompatibleWithFlag(FFlag) or
            (APrefix <> nil) and APrefix.IsCompatibleWithFlag(FFlag)) and
            (not IsPrefixSuffixUnion(AOptions) or
            AWordStem.IsCompatibleWithFlag(APrefix.Flag) or
            IsCompatibleWithFlag(APrefix.Flag)) and
            IsCompatibleWithFlag(ACompatibleFlag, True) and
            IsIncompatibleWithFlag(AWordStem, AIncompatibleFlag) and
            ((ACompoundFlag = 0) or (AWordStem.IsCompatibleWithFlag(ACompoundFlag, True) or
            IsCompatibleWithFlag(ACompoundFlag))) then
            Result := AWordStem;
          AWordStem := AWordStem.NextHomonym;
        until (AWordStem = nil) or (Result <> nil);
      end;
    end;
  end;
end;

function TdxSuffix.MakeInitialWordFormAndProcessSuffixCheck(const AWord: PWideChar;
  AWordLength: Integer; const AOptions: TdxAffixOptions; APrefix: TdxPrefix;
  var ATakenSuffix: TdxSuffix; const ACompoundFlag: Word = 0): TdxHunspellWordStem;
var
  AInitialWordFormLength: Integer;
  AInitialWordFormEnd: PWideChar;
  AInitialWordForm: TdxLineBuffer;
begin
  Result := nil;
  if IsPrefixSuffixUnion(AOptions) and not PrefixSuffixUnion then
    Exit;
  AInitialWordFormLength := AWordLength - AppendStringLength;
  if IsWithoutAffixWordLengthCorrect(AInitialWordFormLength) then
  begin
    StrCopy(AInitialWordForm, AWord);
    AInitialWordFormEnd := AInitialWordForm + AInitialWordFormLength;
    if FStripStringLength <> 0 then
    begin
      StrCopy(AInitialWordFormEnd, FStripString);
      Inc(AInitialWordFormLength, FStripStringLength);
      AInitialWordFormEnd := AInitialWordForm + AInitialWordFormLength;
    end
    else
      AInitialWordFormEnd^ := #0;
    if IsWordSuitableForCondition(AInitialWordFormEnd, AInitialWordForm) then
    begin
      if APrefix <> nil then
      begin
        if IsCompatibleWithFlag(APrefix.Flag) then
          Result := AffixManager.ProcessSuffixCheck(AInitialWordForm,
            AInitialWordFormLength, [], nil, ATakenSuffix, FFlag,
            ACompoundFlag)
        else
          Result := AffixManager.ProcessSuffixCheck(AInitialWordForm,
            AInitialWordFormLength, AOptions, APrefix, ATakenSuffix,
            FFlag, ACompoundFlag);
      end
      else
        Result := AffixManager.ProcessSuffixCheck(AInitialWordForm,
          AInitialWordFormLength, [], nil, ATakenSuffix, FFlag,
          ACompoundFlag);
    end;
  end;
end;

function TdxSuffix.GetAppendString: PWideChar;
begin
  Result := FReverseAppendString;
end;

function TdxSuffix.IsWordSuitableForCondition(AWordEnd, AWordBegin: PWideChar): Boolean;
var
  AConditionCursor, AWordCursor: PWideChar;
  ANegative, AIsGroup: Boolean;
  I: Integer;
begin
  Result := FConditionLength = 0;
  if Result then
    Exit;
  AWordCursor := nil;
  ANegative := False;
  AIsGroup := False;
  AConditionCursor := FCondition;
  Dec(AWordEnd);
  I := 1;
  while AConditionCursor^ <> #0 do
  begin
    case AConditionCursor^ of
      '[':
        begin
          Inc(AConditionCursor);
          AWordCursor := AWordEnd;
        end;
      '^':
        begin
          Inc(AConditionCursor);
          ANegative := True;
        end;
      ']':
        begin
          if not ANegative and not AIsGroup then
            Exit;
          Inc(I);
          if not AIsGroup then
            Dec(AWordEnd);
          AWordCursor := nil;
          ANegative := False;
          AIsGroup := False;
          Inc(AConditionCursor);
          if (AWordEnd < AWordBegin) and (AConditionCursor^ <> #0) then
            Exit;
        end;
      '.':
        if AWordCursor = nil then
        begin
          Inc(AConditionCursor);
          Dec(AWordEnd);
          if AWordEnd < AWordBegin then
          begin
            if AConditionCursor^ = #0 then
              Result := True;
            Exit;
          end;
        end;
    else
      if AWordEnd^ = AConditionCursor^ then
      begin
        Inc(AConditionCursor);
        if AWordCursor <> nil then
        begin
          if ANegative then
            Exit
          else
            if I = FConditionLength then
            begin
              Result := True;
              Exit;
            end;
          AIsGroup := True;
          SearchConditionGroupEnd(AConditionCursor);
          Dec(AWordEnd);
        end;
        if AWordCursor = nil then
        begin
          Inc(I);
          Dec(AWordEnd);
        end;
        if (AWordEnd < AWordBegin) and (AConditionCursor^ <> #0) and (AConditionCursor^ <> ']') then
          Exit;
      end
      else
        if AWordCursor <> nil then
          Inc(AConditionCursor)
        else
          Exit;
    end;
  end;
  Result := True;
end;

{ TdxAffixDataTable }

constructor TdxCustomHunspellDataTable.Create;
begin
  // do nothing
end;


{ TdxCustomHunspellDataTable }

constructor TdxCustomHunspellDataTable.Create(ADataIdentifier: PWideChar);
begin
  inherited Create;
  FDataIdentifier := ADataIdentifier;
end;

destructor TdxCustomHunspellDataTable.Destroy;
begin
  FreeData;
  inherited Destroy;
end;

procedure TdxCustomHunspellDataTable.AllocateData(ASize: Integer);
begin
  FreeData;
  FSize := ASize;
  FData := AllocMem(GetDataItemSize * ASize);
end;

function TdxCustomHunspellDataTable.ReadData(ALine: PWideChar; AAffixFileReader: TdxHunspellReader): Boolean;
begin
  Result := ReadDataHeader(ALine);
  if Result then
    Result := ParseData(AAffixFileReader);
  AfterReadData;
  if not Result then
    FreeData;
end;

function TdxCustomHunspellDataTable.AddDataPiece(const APiece: PWideChar; ADataIndex,
  APieceIndex: Integer): Boolean;
begin
  if APieceIndex = 0 then
  begin
    Result := CheckDataType(APiece);
    if Result then
    begin
      FCurrentDataIndex := ADataIndex;
      InitializeItem(ADataIndex);
    end;
  end
  else
    Result := FCurrentDataIndex = ADataIndex;
end;

procedure TdxCustomHunspellDataTable.AfterReadData;
begin
  //do nothing
end;

function TdxCustomHunspellDataTable.IsAllPiecesFound(APieceIndex: Integer): Boolean;
begin
  Result := APieceIndex >= GetPieceQuantity;
end;

function TdxCustomHunspellDataTable.CheckDataType(const APiece: PWideChar): Boolean;
begin
  if FCachedIdentifier = nil then
  begin
    if FDataIdentifier = nil then
      FCachedIdentifier := GetDataIdentifier
    else
      FCachedIdentifier := FDataIdentifier;
    FIdentifierLength := StrLen(FCachedIdentifier);
  end;
  Result := StrLComp(APiece, FCachedIdentifier, FIdentifierLength) = 0;
end;

function TdxCustomHunspellDataTable.GetCount: Integer;
begin
  Result := FSize;
end;

procedure TdxCustomHunspellDataTable.InitializeItem(Index: Integer);
begin
  // do nothing
end;

function TdxCustomHunspellDataTable.ReadDataHeader(ALine: PWideChar): Boolean;
var
  ATableSize: Integer;
begin
  Result := ParseHeader(ALine, ATableSize);
  if Result then
    AllocateData(ATableSize);
end;

function TdxCustomHunspellDataTable.ParseHeader(ALine: PWideChar; out ATableSize: Integer): Boolean;
var
  APiece: PWideChar;
begin
  ATableSize := 0;
  APiece := nil;
  Result := GetSecondPartOfString(ALine, APiece);
  if Result then
  begin
    ATableSize := StrInt(APiece);
    StrDispose(APiece);
  end;
end;

function TdxCustomHunspellDataTable.ParseData(AAffixReader: TdxHunspellReader): Boolean;
var
  ALineBuffer: TdxLineBuffer;
  ADataIndex, APieceIndex: Integer;
  ALineStart, ALineCursor, APiece: PWideChar;
begin
  Result := True;
  ALineStart := @ALineBuffer;
  for ADataIndex := 0 to Count - 1 do
  begin
    AAffixReader.GetNextLine(ALineBuffer, SizeOf(ALineBuffer));
    RemoveCRLF(ALineStart);
    ALineCursor := ALineStart;
    APieceIndex := 0;
    repeat
      APiece := StrSeparate(@ALineCursor, #0);
      if (APiece <> nil) and (APiece^ <> #0) then
      begin
        Result := AddDataPiece(APiece, ADataIndex, APieceIndex);
        Inc(APieceIndex);
      end;
    until (APiece = nil) or not Result;
    if not IsAllPiecesFound(APieceIndex) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TdxCustomHunspellDataTable.FreeData;
begin
  if FData <> nil then
  begin
    FreeDataItems;
    FreeMem(FData, GetDataItemSize * FSize);
    FData := nil;
  end;
  FSize := 0;
end;

{ TdxAffixDataLinkedTable }

constructor TdxHunspellDataLinkedTable.Create(AAffixManager: TdxHunspellAffixManager);
begin
  inherited Create;
  FAffixManager := AAffixManager;
end;

function TdxHunspellDataLinkedTable.GetWordStemManager: TdxHunspellWordStemManager;
begin
  Result := AffixManager.WordStemManager;
end;

{ TdxAffixItemTable }

destructor TdxAffixItemTable.Destroy;
begin
  StrDispose(FFlag);
  inherited Destroy;
end;

function TdxAffixItemTable.AddDataPiece(const APiece: PWideChar; ADataIndex,
  APieceIndex: Integer): Boolean;
begin
  Result := inherited AddDataPiece(APiece, ADataIndex, APieceIndex);
  if Result then
    case APieceIndex of
      1: Result := CheckFlag(APiece);
      2: ParseStripString(APiece, ADataIndex);
      3: Result := ParseAppendStringWithAffixes(APiece, ADataIndex);
      4: Result := ParseCondition(APiece, ADataIndex);
      5: Result := ParseMorphologicDescription(APiece, ADataIndex);
      6: AddLineRemainderToMorphologicDescription(APiece, ADataIndex);
    end;
end;

function TdxAffixItemTable.IsAllPiecesFound(APieceIndex: Integer): Boolean;
begin
  Result := True;
end;

function TdxAffixItemTable.IsComplexPrefixes: Boolean;
begin
  Result := AffixManager.ComplexPrefixes;
end;

procedure TdxAffixItemTable.BuildAffixes;
begin
  FAffixesIsCreated := True;
end;

function TdxAffixItemTable.GetDataItemSize: Integer;
begin
  Result := SizeOf(TdxAffixItem);
end;

function TdxAffixItemTable.GetDataIdentifier: PWideChar;
begin
  if IsPrefix then
    Result := 'PFX'
  else
    Result := 'SFX';
end;

function TdxAffixItemTable.GetPieceQuantity: Integer;
begin
  Result := 7;
end;

procedure TdxAffixItemTable.FreeDataItems;
var
  I: Integer;
begin
  if not FAffixesIsCreated then
    for I := 0 to Count - 1 do
    begin
      StrDispose(Data[I].StripString);
      StrDispose(Data[I].AppendString);
      if not HasAffixMorphologicAliases then
        StrDispose(Data[I].MorphologicalDescription);
      StrDispose(Data[I].Condition);
      FreeAndNil(Data[I].CompatibleFlags);
    end;
end;

function TdxAffixItemTable.ParseHeader(ALine: PWideChar; out ATableSize: Integer): Boolean;
var
  I: Integer;
  APiece: PWideChar;
begin
  Result := True;
  APiece := StrSeparate(@ALine, #0);
  I := 0;
  while (APiece <> nil) and Result and (I < 4) do
  begin
    if APiece^ <> #0 then
    begin
      case I of
        1: Result := ParseFlag(APiece);
        2: ParsePrefixSuffixUnion(APiece);
        3: Result := ParseTableSize(APiece, ATableSize);
      end;
      Inc(I);
    end;
    APiece := StrSeparate(@ALine, #0);
  end;
end;

function TdxAffixItemTable.GetData: PdxAffixItemArray;
begin
  Result := PdxAffixItemArray(FData);
end;

function TdxAffixItemTable.GetItem(Index: Integer): TdxAffixItem;
begin
  Result := Data[Index];
end;

procedure TdxAffixItemTable.AddLineRemainderToMorphologicDescription(
  ALine: PWideChar; ADataIndex: Integer);
var
  ANewDescription: PWideChar;
  ALength: Integer;
begin
  if not HasAffixMorphologicAliases then
  begin
    ALength := StrLen(Data[ADataIndex].MorphologicalDescription) + StrLen(ALine) + 2;
    ANewDescription := StrAlloc(ALength);
    StrCopy(ANewDescription, Data[ADataIndex].MorphologicalDescription);
    ALength := StrLen(ANewDescription);
    StrCopy(ANewDescription + ALength, ' ');
    StrCopy(ANewDescription + ALength + 1, ALine);
    StrDispose(Data[ADataIndex].MorphologicalDescription);
    Data[ADataIndex].MorphologicalDescription := ANewDescription;
  end;
end;

function TdxAffixItemTable.CheckFlag(ALine: PWideChar): Boolean;
begin
  Result := (AffixManager.DecodeFlag(ALine) = FDecodedFlag) or FlagIsNil;
end;

procedure TdxSuffixItemTable.BuildAffixes;
var
  I: Integer;
begin
  inherited BuildAffixes;
  for I := 0 to Count - 1 do
    AffixManager.BuildAffixTree(TdxSuffix.Create(AffixManager, @Data[I],
      FDecodedFlag, GetOptions(Data[I])),
      AffixManager.FSuffixTableArrangedByAppendString, AffixManager.FSuffixTableIndexedByFlag);
end;

function TdxSuffixItemTable.ConditionContainsDuplicateInformation(
  AStripString: PWideChar; AStripStringLength: Integer; const ACondition: PWideChar;
  out Contains: Boolean): Boolean;
var
  AConditionLength, I, J: Integer;
  ANegative, AIsCharactersAgreed: Boolean;
begin
  Result := True;
  Contains := False;
  AConditionLength := StrLen(ACondition);
  if (AStripStringLength >= AConditionLength) and StrEquals(AStripString + AStripStringLength - AConditionLength, ACondition) then
    Contains := True;
  if not Contains then
  begin
    I := AStripStringLength - 1;
    J := AConditionLength - 1;
    while (I >= 0) and (J >= 0) and Result  do
    begin
      if (ACondition + J)^ <> ']' then
      begin
        Result := (ACondition + J)^ = (AStripString + I)^;
      end
      else
      begin
        AIsCharactersAgreed := False;
        repeat
          Dec(J);
          if (AStripString + I)^ = (ACondition + J)^ then
            AIsCharactersAgreed := True;
        until not((J > 0) and ((ACondition + J)^ <> '['));
        Result := not((J = 0) and ((ACondition + J)^ <> '['));
        if Result then
        begin
          ANegative := (ACondition + J + 1)^ = '^';
          Result := ANegative xor AIsCharactersAgreed;
        end;
      end;
      Dec(I);
      Dec(J);
    end;
    if Result and (J < 0) then
      Contains := True;
  end;
end;

function TdxAffixItemTable.ConditionLength(ALine: PWideChar): Integer;
var
  AInGroup: Boolean;
begin
  Result := 0;
  AInGroup := False;
  while ALine^ <> #0 do
  begin
    if ALine^ = '[' then
    begin
      AInGroup := True;
      Inc(Result);
    end
    else
      if ALine^ = ']' then
        AInGroup := False
      else
        if not AInGroup then
          Inc(Result);
    Inc(ALine);
  end;
end;

function TdxAffixItemTable.EncodeAffixCondition(ALine: PWideChar; ADataIndex: Integer): Boolean;
begin
  Result := True;
  if not StrEquals(ALine, '.') then
  begin
    Data[ADataIndex].Condition := StrNew(ALine);
    Data[ADataIndex].ConditionLength := ConditionLength(ALine);
  end
  else
  begin
    Data[ADataIndex].ConditionLength := 0;
    Data[ADataIndex].Condition := nil;
  end;
end;

function TdxAffixItemTable.HasAffixFlagAliases: Boolean;
begin
  Result := WordStemManager.HasAffixFlagAliases;
end;

function TdxAffixItemTable.HasAffixMorphologicAliases: Boolean;
begin
  Result := WordStemManager.HasAffixMorphologicAliases;
end;

function TdxAffixItemTable.FlagIsNil: Boolean;
var
  AEncodedFlag: PWideChar;
begin
  AEncodedFlag := AffixManager.EncodeFlag(FDecodedFlag);
  try
    Result := AEncodedFlag = nil;
  finally
    StrDispose(AEncodedFlag);
  end;
end;

procedure TdxAffixItemTable.FreeIfZero(var AString: TdxStringData);
const
  Empty: PWideChar = '';
begin
  if StrEquals(AString.Data, '0') then
  begin
    StrDispose(AString.Data);
    AString.Data := StrNew(Empty);
    AString.Length := 0;
  end;
end;

function TdxAffixItemTable.GetOptions(const AItem: TdxAffixItem): TdxAffixOptions;
begin
  Result := [];
  if FPrefixSuffixUnion then
    Include(Result, aoPrefixSuffixUnion);
  if HasAffixFlagAliases then
    Include(Result, aoAffixFlagAliasTable);
  if WordStemManager.HasAffixMorphologicAliases then
    Include(Result, aoAffixMorphologyTable);
end;

function TdxAffixItemTable.IsReverseWritingDirection: Boolean;
begin
  Result := AffixManager.ComplexPrefixes;
end;

procedure TdxAffixItemTable.ParseAppendString(const ALine: PWideChar; ADataIndex: Integer);
begin
  RemoveIgnoredChars(ALine, AffixManager.IgnoredChars);
  ProcessReverseString(ALine);
  Data[ADataIndex].AppendString := StrNew(ALine);
end;

function TdxAffixItemTable.ParseAppendStringWithAffixes(const ALine: PWideChar; ADataIndex: Integer): Boolean;
var
  ACompatibleFlagCursor: PWideChar;
  ATempStringData: TdxStringData;
begin
  Result := True;
  ACompatibleFlagCursor := StrScan(ALine, '/');
  if ACompatibleFlagCursor <> nil then
  begin
    ACompatibleFlagCursor^ := #0;

    ParseAppendString(ALine, ADataIndex);

    Result := ParseCompatibleFlags(ACompatibleFlagCursor + 1, ADataIndex);

    ACompatibleFlagCursor^ := '/';

    if Result then
      RegisterCompatibleFlags(ADataIndex);
  end
  else
    ParseAppendString(ALine, ADataIndex);

  Data[ADataIndex].AppendStringLength := StrLen(Data[ADataIndex].AppendString);
  if Result then
  begin
    ATempStringData.Data := Data[ADataIndex].AppendString;
    ATempStringData.Length := Data[ADataIndex].AppendStringLength;
    FreeIfZero(ATempStringData);
    Data[ADataIndex].AppendString := ATempStringData.Data;
    Data[ADataIndex].AppendStringLength := ATempStringData.Length;
  end;
end;

function TdxAffixItemTable.ParseCompatibleFlags(const ALine: PWideChar; ADataIndex: Integer): Boolean;
begin
   Result := True;
   Data[ADataIndex].CompatibleFlags := TdxHunspellFlags.Create(AffixManager.AffixFlagMode);
   if HasAffixFlagAliases then
     Result := Data[ADataIndex].CompatibleFlags.InitializeByAlias(WordStemManager, ALine)
   else
   begin
     Data[ADataIndex].CompatibleFlags.Decode(WordStemManager, ALine);
     Data[ADataIndex].CompatibleFlags.Sort;
   end;
end;

function TdxAffixItemTable.ParseCondition(ALine: PWideChar; ADataIndex: Integer): Boolean;
var
  AContains: Boolean;
begin
  Result := True;
  ProcessReverseString(ALine);
  if IsReverseWritingDirection then
    ReverseCondition(ALine);
  if (Data[ADataIndex].StripStringLength <> 0) and not StrEquals(ALine, '.') then
  begin
    Result := ConditionContainsDuplicateInformation(Data[ADataIndex].StripString,
      Data[ADataIndex].StripStringLength, ALine, AContains);
    if Result and AContains then
    begin
      ALine^ := '.';
      (ALine + 1)^ := #0;
    end;
  end;
  if Result then
  begin
    if not IsPrefix then
    begin
      StrReverse(ALine);
      ReverseCondition(ALine);
    end;
    Result := EncodeAffixCondition(ALine, ADataIndex);
  end;
end;

function TdxAffixItemTable.ParseFlag(ALine: PWideChar): Boolean;
begin
  FDecodedFlag := AffixManager.DecodeFlag(ALine);
  Result := AffixManager.IsAffixKeyUnique(FDecodedFlag, IsPrefix);
  if Result then
  begin
    AffixManager.SetAffixKeyExist(FDecodedFlag, IsPrefix);
    FFlag := StrNew(ALine);
  end;
end;

function TdxAffixItemTable.ParseMorphologicDescription(ALine: PWideChar; ADataIndex: Integer): Boolean;
begin
  Result := True;
  if HasAffixMorphologicAliases then
    Data[ADataIndex].MorphologicalDescription := WordStemManager.GetAffixMorphologyByAlias(StrInt(ALine))
  else
  begin
    ProcessReverseString(ALine);
    Data[ADataIndex].MorphologicalDescription := StrNew(ALine);
    Result := Data[ADataIndex].MorphologicalDescription <> nil;
  end;
end;

procedure TdxAffixItemTable.ParsePrefixSuffixUnion(ALine: PWideChar);
begin
  FPrefixSuffixUnion := ALine^ = 'Y';
end;

procedure TdxAffixItemTable.ParseStripString(const ALine: PWideChar; ADataIndex: Integer);
var
  ATempStringData: TdxStringData;
begin
  ProcessReverseString(ALine);
  ATempStringData.Data := StrNew(ALine);
  ATempStringData.Length := StrLen(ATempStringData.Data);
  FreeIfZero(ATempStringData);
  Data[ADataIndex].StripString := ATempStringData.Data;
  Data[ADataIndex].StripStringLength := ATempStringData.Length;
end;

function TdxAffixItemTable.ParseTableSize(ALine: PWideChar; out ATableSize: Integer): Boolean;
begin
  ATableSize := StrInt(ALine);
  Result := (ATableSize <> 0) or FlagIsNil;
end;

procedure TdxAffixItemTable.ProcessReverseString(ALine: PWideChar);
begin
  if IsReverseWritingDirection then
    StrReverse(ALine);
end;

procedure TdxAffixItemTable.RegisterCompatibleFlags(ADataIndex: Integer);
var
  I: Integer;
begin
  AffixManager.CompatibleFlagsExist := True;
  for I := 0 to Data[ADataIndex].CompatibleFlags.Length - 1 do
    AffixManager.SetCompatibleFlags(Data[ADataIndex].CompatibleFlags[I], True);
end;

procedure TdxAffixItemTable.ReverseCondition(ACondition: PWideChar);
var
  ANegative: Boolean;
  AConditionCursor: PWideChar;
begin
  ANegative := False;
  AConditionCursor := ACondition + StrLen(ACondition) - 1;
  while AConditionCursor >= ACondition do
  begin
    case AConditionCursor^ of
      '[': begin
             if ANegative then
               (AConditionCursor + 1)^ := '['
             else
               AConditionCursor^ := ']';
           end;
      ']': begin
             AConditionCursor^ := '[';
             if ANegative then
               (AConditionCursor + 1)^ := '^';
             ANegative := False;
           end;
      '^': begin
             if (AConditionCursor + 1)^ = ']' then
               ANegative := True
             else
               (AConditionCursor + 1)^ := AConditionCursor^;
           end;
    else
      if ANegative then
        (AConditionCursor + 1)^ := AConditionCursor^;
    end;
    Dec(AConditionCursor);
  end;
end;

{ TdxPrefixItemTable }

procedure TdxPrefixItemTable.BuildAffixes;
var
  I: Integer;
begin
  inherited BuildAffixes;
  for I := 0 to Count - 1 do
    AffixManager.BuildAffixTree(TdxPrefix.Create(AffixManager, @Data[I],
      FDecodedFlag, GetOptions(Data[I])),
      AffixManager.FPrefixTableArrangedByAppendString, AffixManager.FPrefixTableIndexedByFlag);
end;

function TdxPrefixItemTable.ConditionContainsDuplicateInformation(
  AStripString: PWideChar; AStripStringLen: Integer; const ACondition: PWideChar;
  out AContains: Boolean): Boolean;
var
  AConditionLength, I, J: Integer;
  ANegative, AIsCharactersAgreed: Boolean;
begin
  Result := True;
  AConditionLength := StrLen(ACondition);
  AContains := StrEquals(AStripString, ACondition);
  if not AContains then
  begin
    I := 0;
    J := 0;
    while (I < AStripStringLen) and (J < AConditionLength) and Result do
    begin
      if (ACondition + J)^ <> '[' then
        Result := (ACondition + J)^ = (AStripString + I)^
      else
      begin
        ANegative := (ACondition + J + 1)^ = '^';
        AIsCharactersAgreed := False;
        repeat
          Inc(J);
          if (AStripString + I)^ = (ACondition + J)^ then
            AIsCharactersAgreed := True;
        until not((J < AConditionLength - 1) and ((ACondition + J)^ <> ']'));
        Result := not ((J = AConditionLength - 1) and ((ACondition + J)^ <> ']'));
        if Result then
          Result := ANegative xor AIsCharactersAgreed;
      end;
      Inc(I);
      Inc(J);
    end;
    if Result and (J >= AConditionLength) then
      AContains := True;
  end;
end;

function TdxPrefixItemTable.IsPrefix: Boolean;
begin
  Result := not IsComplexPrefixes;
end;

{ TdxSuffixItemTable }

function TdxSuffixItemTable.IsPrefix: Boolean;
begin
  Result := IsComplexPrefixes;
end;

{ TdxBreakTable }

constructor TdxBreakTable.Create;
begin
  inherited Create;
  FillDefaultData;
end;

procedure TdxBreakTable.FillDefaultData;
var
  I: Integer;
begin
  I := 3; //hack range-checking
  AllocateData(I);
  Data[I - 3] := StrNew('-');
  Data[I - 2] := StrNew('^-');
  Data[I - 1] := StrNew('-$');
end;

function TdxBreakTable.AddDataPiece(const APiece: PWideChar; ADataIndex,
  APieceIndex: Integer): Boolean;
begin
  Result := inherited AddDataPiece(APiece, ADataIndex, APieceIndex);
  if Result and (APieceIndex = 1) then
    Data[ADataIndex] := StrNew(APiece);
end;

procedure TdxBreakTable.FreeDataItems;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    StrFreeAndNil(Data[I]);
end;

function TdxBreakTable.GetDataIdentifier: PWideChar;
begin
  Result := 'BREAK';
end;

function TdxBreakTable.GetDataItemSize: Integer;
begin
  Result := SizeOf(PWideChar);
end;

function TdxBreakTable.GetPieceQuantity: Integer;
begin
  Result := 2;
end;

function TdxBreakTable.GetData: PdxPWideCharArray;
begin
  Result := PdxPWideCharArray(FData);
end;

function TdxBreakTable.GetItem(Index: Integer): PWideChar;
begin
  Result := Data[Index];
end;

{ TdxCheckCompoundPatternTable }

function TdxCheckCompoundPatternTable.AddDataPiece(const APiece: PWideChar;
  ADataIndex, APieceIndex: Integer): Boolean;
begin
  Result := inherited AddDataPiece(APiece, ADataIndex, APieceIndex);
  if Result then
    case APieceIndex of
      1: begin
           Data[ADataIndex].Pattern := StrNew(APiece);
           ExtractCondition(Data[ADataIndex].Pattern, Data[ADataIndex].Condition);
         end;
      2: begin
           Data[ADataIndex].Pattern2 := StrNew(APiece);
           ExtractCondition(Data[ADataIndex].Pattern2, Data[ADataIndex].Condition2);
         end;
      3: begin
           Data[ADataIndex].Pattern3 := StrNew(APiece);
           FIsSimplified := True;
         end;
    end;
end;

procedure TdxCheckCompoundPatternTable.FreeDataItems;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    StrDispose(Data[I].Pattern);
    StrDispose(Data[I].Pattern2);
    StrDispose(Data[I].Pattern3);
  end;
end;

function TdxCheckCompoundPatternTable.GetDataIdentifier: PWideChar;
begin
  Result := diCheckCompoundPattern;
end;

function TdxCheckCompoundPatternTable.GetDataItemSize: Integer;
begin
  Result := SizeOf(TdxCompoundForbidPattern);
end;

function TdxCheckCompoundPatternTable.GetPieceQuantity: Integer;
begin
  Result := 3;
end;

procedure TdxCheckCompoundPatternTable.InitializeItem(Index: Integer);
begin
  Data[Index].Pattern := nil;
  Data[Index].Pattern2 := nil;
  Data[Index].Pattern3 := nil;
  Data[Index].Condition := dxNullFlag;
  Data[Index].Condition2 := dxNullFlag;
end;

procedure TdxCheckCompoundPatternTable.ExtractCondition(APattern: PWideChar;
  var ACondition: Word);
var
  ADelimiter: PWideChar;
begin
  ADelimiter := StrScan(APattern, '/');
  if ADelimiter <> nil then
  begin
    ADelimiter^ := #0;
    ACondition := AffixManager.DecodeFlag(ADelimiter + 1);
  end;
end;

function TdxCheckCompoundPatternTable.GetData: PdxCompoundForbidPatternArray;
begin
  Result := PdxCompoundForbidPatternArray(FData);
end;

function TdxCheckCompoundPatternTable.GetItem(Index: Integer): TdxCompoundForbidPattern;
begin
  Result := Data[Index];
end;

{ TdxCompoundRuleTable }

function TdxCompoundRuleTable.IsPartOfCompoundWordFlag(AFlag: PWideChar): Boolean;
begin
  Result := IsPartOfCompoundWordFlag(Ord(AFlag^));
end;

function TdxCompoundRuleTable.IsPartOfCompoundWordFlag(AFlag: Word): Boolean;
begin
  Result := (AFlag = Ord(dxCompoundRuleSeveralWordsFlag)) or
    (AFlag = Ord(dxCompoundRuleOneWordFlag));
end;

function TdxCompoundRuleTable.AddDataPiece(const APiece: PWideChar; ADataIndex,
  APieceIndex: Integer): Boolean;
begin
  Result := inherited AddDataPiece(APiece, ADataIndex, APieceIndex);
  if Result and (APieceIndex = 1) then
    Result := ParseFlags(APiece, ADataIndex);
end;

procedure TdxCompoundRuleTable.FreeDataItems;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FreeMem(Data[I].Pattern, Data[I].Count * SizeOf(Word));
end;

function TdxCompoundRuleTable.GetDataIdentifier: PWideChar;
begin
  Result := diCompoundRule;
end;

function TdxCompoundRuleTable.GetDataItemSize: Integer;
begin
  Result := SizeOf(TdxCompoundFlag);
end;

function TdxCompoundRuleTable.GetPieceQuantity: Integer;
begin
  Result := 2;
end;

procedure TdxCompoundRuleTable.InitializeItem(Index: Integer);
begin
  Data[Index].Pattern := nil;
end;

function TdxCompoundRuleTable.GetData: PdxCompoundFlagArray;
begin
  Result := PdxCompoundFlagArray(FData);
end;

function TdxCompoundRuleTable.GetItem(Index: Integer): TdxCompoundFlag;
begin
  Result := Data[Index];
end;

function TdxCompoundRuleTable.ParseFlags(APiece: PWideChar; AIndex: Integer): Boolean;
var
  ABracketPosition: PWideChar;
  AIsEndOfLine: Boolean;
  AItem: PdxCompoundFlag;

  procedure AllocPattern(AItem: PdxCompoundFlag; ASize: Integer);
  begin
    AItem^.Count := ASize;
    AItem^.Pattern := AllocMem(AItem^.Count * SizeOf(Word));
  end;

  procedure ReadFlags(APiece: PWideChar; AItem: PdxCompoundFlag);
  var
    AFlags: TdxHunspellFlags;
    I: Integer;
  begin
    AFlags := TdxHunspellFlags.Create(AffixManager.AffixFlagMode);
    try
      AFlags.Decode(WordStemManager, APiece);
      for I := 0 to AFlags.Length - 1 do
      begin
        AItem^.Pattern[AItem^.Length] := AFlags[I];
        Inc(AItem^.Length);
      end;
    finally
      AFlags.Free;
    end;
  end;

begin
  AItem := @Data[AIndex];
  AllocPattern(AItem, StrLen(APiece));
  if StrScan(APiece, '(') <> nil then
  begin
    AIsEndOfLine := False;
    repeat
      ABracketPosition := APiece + 1;
      while (ABracketPosition^ <> '(') and (ABracketPosition^ <> ')') and (ABracketPosition^ <> #0) do
        Inc(ABracketPosition);
      if ABracketPosition^ = #0 then
        AIsEndOfLine := True
      else
        ABracketPosition^ := #0;
      if APiece^ = '(' then
        Inc(APiece);
      if IsPartOfCompoundWordFlag(APiece) then
      begin
        AItem^.Pattern[AItem^.Length] := Ord(APiece^);
        Inc(AItem^.Length);
      end
      else
        if APiece^ <> #0 then
          ReadFlags(APiece, AItem);
      APiece := ABracketPosition + 1;
    until AIsEndOfLine or (APiece^ = #0);
  end
  else
    ReadFlags(APiece, AItem);
  Result := AItem^.Length <> 0;
end;

{ TdxMapTable }

function TdxMapTable.AddDataPiece(const APiece: PWideChar; ADataIndex,
  APieceIndex: Integer): Boolean;
begin
  Result := inherited AddDataPiece(APiece, ADataIndex, APieceIndex);
  if Result and (APieceIndex = 1) then
  begin
    Data[ADataIndex].CharacterSet := StrNew(APiece);
    Data[ADataIndex].Length := StrLen(Data[ADataIndex].CharacterSet);
  end;
end;

procedure TdxMapTable.FreeDataItems;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    StrDispose(Data[I].CharacterSet);
end;

function TdxMapTable.GetDataIdentifier: PWideChar;
begin
  Result := diMap;
end;

function TdxMapTable.GetDataItemSize: Integer;
begin
  Result := SizeOf(TdxMapTableItem);
end;

function TdxMapTable.GetPieceQuantity: Integer;
begin
  Result := 2;
end;

procedure TdxMapTable.InitializeItem(Index: Integer);
begin
  Data[Index].CharacterSet := nil;
  Data[Index].Length := 0;
end;

function TdxMapTable.GetData: PdxMapArray;
begin
  Result := PdxMapArray(FData);
end;

function TdxMapTable.GetItem(Index: Integer): TdxMapTableItem;
begin
  Result := Data[Index];
end;

{ TdxPhoneTable }

destructor TdxPhoneTable.Destroy;
begin
  FreeRules;
  inherited Destroy;
end;

function TdxPhoneTable.Phonetic(const AWord: PWideChar; ADest: PWideChar; ALength: Integer): Integer;
const
  SpecialSymbols: PWideChar = '(-<^$';
var
  ACharIndex, J, AFoundLetters, ARuleIndex, APriority, z, k0, n0, p0, z0: Integer;
  AChar, C0: WideChar;
  S: PWideChar;
  ATempWord: array[0..dxMaxPhoneticsLength] of WideChar;
begin
  Result := 0;
  AFoundLetters := 0;
  p0 := -333;
  if ALength = -1 then
    ALength := StrLen(AWord);
  if ALength > dxMaxPhoneticsLength then
    Exit;
  StrCopy(ATempWord, AWord);

  ACharIndex := 0;
  J := 0;
  z := 0;
  AChar := ATempWord[ACharIndex];
  while AChar <> #0 do
  begin
    ARuleIndex := FHash[Ord(AChar)];
    z0 := 0;

    if ARuleIndex >= 0 then
    begin
      while Rules[ARuleIndex]^ = AChar do
      begin
        AFoundLetters := 1;
        APriority := 5;
        S := Rules[ARuleIndex];
        Inc(S);

        while (S^ <> #0) and (ATempWord[ACharIndex + AFoundLetters] = S^) and
          not IsDigit(S^) and (StrScan(PWideChar(SpecialSymbols), S^) = nil) do
        begin
          Inc(AFoundLetters);
          Inc(S);
        end;
        if S^ = '(' then
          if dxWideIsAlpha(ATempWord[ACharIndex + AFoundLetters]) and (StrScan(S + 1, ATempWord[ACharIndex + AFoundLetters]) <> nil) then
          begin
            Inc(AFoundLetters);
            while S^ <> ')' do
              Inc(S);
            Inc(S);
          end;
        p0 := Ord(S^);
        k0 := AFoundLetters;
        while (S^ = '-') and (AFoundLetters > 1) do
        begin
          Dec(AFoundLetters);
          Inc(S);
        end;
        if S^ = '<' then
          Inc(S);
        if IsDigit(S^) then
        begin
          APriority := Ord(S^) - Ord('0');
          Inc(S);
        end;
        if (S^ = '^') and ((S + 1)^ = '^') then
          Inc(S);

        if (S^ = #0) or ((S^ = '^') and ((ACharIndex = 0) or not dxWideIsAlpha(ATempWord[ACharIndex - 1]) and
          ((S + 1)^ <> '$') or not dxWideIsAlpha(ATempWord[ACharIndex + k0]))) or
          ((S^ = '$') and (ACharIndex > 0) and dxWideIsAlpha(ATempWord[ACharIndex - 1]) and
          not dxWideIsAlpha(ATempWord[ACharIndex + k0])) then
        begin
          C0 := ATempWord[ACharIndex + AFoundLetters - 1];
          n0 := FHash[Ord(C0)];

          if (AFoundLetters > 1) and (n0 >= 0) and (p0 <> Ord('-')) and (ATempWord[ACharIndex + AFoundLetters] <> #0) then
          begin
            while Rules[n0]^ = C0 do
            begin
              k0 := AFoundLetters;
              p0 := 5;
              S := Rules[n0];
              Inc(S);
              while (S^ <> #0) and (ATempWord[ACharIndex + k0] = S^) and not IsDigit(S^) and
                (StrScan(PWideChar(SpecialSymbols), S^) <> nil) do
              begin
                Inc(k0);
                Inc(S);
              end;
              if S^ = '(' then
                if dxWideIsAlpha(ATempWord[ACharIndex + k0]) and (StrScan(S + 1, ATempWord[ACharIndex + k0]) <> nil) then
                begin
                  Inc(k0);
                  while (S^ <> ')') and (S^ <> #0) do
                    Inc(S);
                  if S^ = ')' then
                    Inc(S);
                end;
              while S^ = '-' do
                Inc(S);
              if S^ = '<' then
                Inc(S);
              if IsDigit(S^) then
              begin
                p0 := Ord(S^) - Ord('0');
                Inc(S);
              end;

              if (S^ = #0) or (S^ = '$') and not dxWideIsAlpha(ATempWord[ACharIndex + k0]) then
              begin
                if (k0 = AFoundLetters) or (p0 < APriority) then
                begin
                  Inc(n0, 2);
                  Continue;
                end;
                Break;
              end;
              Inc(n0, 2);
            end;

            if (p0 >= APriority) and (Rules[n0]^ = C0) then
            begin
              Inc(ARuleIndex, 2);
              Continue;
            end;
          end;

          S := Rules[ARuleIndex + 1];
          p0 := 0;
          if (Rules[ARuleIndex]^ <> #0) and (StrScan(Rules[ARuleIndex] + 1, '<') <> nil) then
            p0 := 1;
          if (p0 = 1) and (z = 0) then
          begin
            if (J > 0) and (S^ <> #0) and ((ADest[J - 1] = AChar) or (ADest[J - 1] = S^)) then
              Dec(J);
            z0 := 1;
            z := 1;
            k0 := 0;
            while (S^ <> #0) and (ATempWord[ACharIndex + k0] <> #0) do
            begin
              ATempWord[ACharIndex + k0] := S^;
              Inc(k0);
              Inc(S);
            end;
            if AFoundLetters > k0 then
              StrCopy(PWideChar(@ATempWord[ACharIndex + k0]), PWideChar(@ATempWord[ACharIndex + AFoundLetters]));
            AChar := ATempWord[ACharIndex];
          end
          else
          begin
            Inc(ACharIndex, AFoundLetters - 1);
            z := 0;
            while (S^ <> #0) and ((S + 1)^ <> #0) and (J < ALength) do
            begin
              if (J = 0) or (ADest[J - 1] <> S^) then
              begin
                ADest[J] := S^;
                Inc(J);
              end;
              Inc(S);
            end;
            AChar := S^;
            if (Rules[ARuleIndex]^ <> #0) and (StrPos(Rules[ARuleIndex] + 1, '^^') <> nil) then
            begin
              if AChar <> #0 then
              begin
                ADest[J] := AChar;
                Inc(J);
              end;
              StrCopy(PWideChar(@ATempWord[0]), PWideChar(@ATempWord[ACharIndex + 1]));
              ACharIndex := 0;
              z0 := 1;
            end;
          end;
          Break;
        end;
        Inc(ARuleIndex, 2);
      end;
    end;
    if z0 = 0 then
    begin
      if (AFoundLetters <> 0) and (p0 = 0) and (J < ALength) and (AChar <> #0) then
      begin
        ADest[J] := AChar;
        Inc(J);
      end;
      Inc(ACharIndex);
      z := 0;
      AFoundLetters := 0;
    end;
    AChar := ATempWord[ACharIndex];
  end;
  ADest[J] := #0;
  Result := J;
end;

function TdxPhoneTable.AddDataPiece(const APiece: PWideChar; ADataIndex,
  APieceIndex: Integer): Boolean;
begin
  Result := inherited AddDataPiece(APiece, ADataIndex, APieceIndex);
  if Result then
    case APieceIndex of
      1: Rules[2 * ADataIndex] := StrReplace(StrNew(APiece), '_', '');
      2: Rules[2 * ADataIndex + 1] := StrReplace(StrNew(APiece), '_', '');
    end;
end;

procedure TdxPhoneTable.AfterReadData;
begin
  InitializeHash;
end;

procedure TdxPhoneTable.FreeDataItems;
begin
  // do nothing
end;

procedure TdxPhoneTable.FreeRules;
var
  I: Integer;
begin
  if FRules = nil then Exit;
  for I := 0 to Count - 1 do
  begin
    StrDispose(Rules[2 * I]);
    StrDispose(Rules[2 * I + 1]);
  end;
  FreeMem(FRules, 2 * Count * SizeOf(PWideChar));
end;

function TdxPhoneTable.GetCount: Integer;
begin
  Result := FCount;
end;

function TdxPhoneTable.GetDataItemSize: Integer;
begin
  Result := 0;
end;

function TdxPhoneTable.GetDataIdentifier: PWideChar;
begin
  Result := diPhone;
end;

function TdxPhoneTable.GetPieceQuantity: Integer;
begin
  Result := 2;
end;

function TdxPhoneTable.ReadDataHeader(ALine: PWideChar): Boolean;
var
  ACount: Integer;
begin
  Result := ParseHeader(ALine, ACount);
  if Result then
  begin
    FCount := ACount;
    FRules := AllocMem(2 * ACount * SizeOf(PWideChar));
  end;
end;

procedure TdxPhoneTable.InitializeHash;
var
  I, ACharCode: Integer;
begin
  for I := 0 to dxPhoneHashSize - 1 do
    FHash[I] := -1;
  for I := 0 to Count - 1 do
  begin
    ACharCode := Ord(Rules[I * 2]^);
    if FHash[ACharCode] < 0 then
      FHash[ACharCode] := I * 2;
  end;
end;

{ TdxReplaceTable }

function TdxReplaceTable.AddDataPiece(const APiece: PWideChar; ADataIndex,
  APieceIndex: Integer): Boolean;
begin
  Result := inherited AddDataPiece(APiece, ADataIndex, APieceIndex);
  if Result then
    case APieceIndex of
      1: Data[ADataIndex].Text := StrReplace(StrNew(APiece), '_', ' ');
      2: Data[ADataIndex].Replacement := StrReplace(StrNew(APiece), '_', ' ');
    end;
end;

procedure TdxReplaceTable.FreeDataItems;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    StrDispose(Data[I].Text);
    StrDispose(Data[I].Replacement);
  end;
end;

function TdxReplaceTable.GetDataIdentifier: PWideChar;
begin
  Result := diRep;
end;

function TdxReplaceTable.GetDataItemSize: Integer;
begin
  Result := SizeOf(TdxReplaceTableItem);
end;

function TdxReplaceTable.GetPieceQuantity: Integer;
begin
  Result := 3;
end;

procedure TdxReplaceTable.InitializeItem(Index: Integer);
begin
  Data[Index].Text := nil;
  Data[Index].Replacement := nil;
end;

function TdxReplaceTable.GetData: PdxReplaceArray;
begin
  Result := PdxReplaceArray(FData);
end;

function TdxReplaceTable.GetItem(Index: Integer): TdxReplaceTableItem;
begin
  Result := Data[Index];
end;

{ TdxConvertTable }

function TdxConvertTable.Convert(const AWord: PWideChar; ADest: PWideChar): Boolean;
var
  ACount, I, AIndex, AMatchLen, AWordLen: Cardinal;
  AReplacement: PWideChar;
begin
  Result := False;
  if Data = nil then
    Exit;
  ACount := 0;
  AWordLen := StrLen(AWord);
  I := 0;
  while I < AWordLen do
  begin
    AIndex := FindNear(AWord + I);
    AMatchLen := Match(AWord + I, AIndex);
    if AMatchLen <> 0 then
    begin
      AReplacement := Data[AIndex].Replacement;
      StrCopy(ADest + ACount, AReplacement);
      Inc(ACount, StrLen(AReplacement));
      Inc(I, AMatchLen - 1);
      Result := True;
    end
    else
    begin
      (ADest + ACount)^ := (AWord + I)^;
      Inc(ACount);
    end;
    Inc(I);
  end;
  (ADest + ACount)^ := #0;
end;

function TdxConvertTable.AddDataPiece(const APiece: PWideChar; ADataIndex,
  APieceIndex: Integer): Boolean;
begin
  Result := inherited AddDataPiece(APiece, ADataIndex, APieceIndex);
  if Result and (APieceIndex = 2) then
    Sort(ADataIndex);
end;

function TdxConvertTable.Match(const AWord: PWideChar; AIndex: Integer): Integer;
var
  AItem: TdxReplaceTableItem;
  ALength: Integer;
begin
  Result := 0;
  AItem := Data[AIndex];
  ALength := StrLen(AItem.Text);
  if StrLComp(AWord, AItem.Text, ALength) = 0 then
    Result := ALength;
end;

function TdxConvertTable.FindNear(const AWord: PWideChar): Integer;
var
  ALeft, ARight, APivot, ACompare: Integer;
begin
  ALeft := 0;
  ARight := Count;
  while ARight - ALeft > 1 do
  begin
    APivot := (ALeft + ARight) div 2;
    ACompare := StrComp(AWord, Data[APivot].Text);
    if ACompare <= 0 then
    begin
      ARight := APivot;
      if ACompare = 0 then
        ALeft := APivot;
    end
    else
      ALeft := APivot;
  end;
  Result := ALeft;
end;

procedure TdxConvertTable.Sort(AIndex: Integer);
var
  I: Integer;
  AItem: TdxReplaceTableItem;
begin
  for I := AIndex downto 1 do
  begin
    AItem := Data[I];
    if StrComp(AItem.Text, Data[I - 1].Text) < 0 then
    begin
      Data[I] := Data[I - 1];
      Data[I - 1] := AItem;
    end
    else
      Break;
  end;
end;

{ TdxAffixFlagAliasesTable }

function TdxAffixFlagAliasesTable.AddDataPiece(const APiece: PWideChar;
  ADataIndex, APieceIndex: Integer): Boolean;
var
  AAffixFlags: TdxHunspellFlags;
begin
  Result := inherited AddDataPiece(APiece, ADataIndex, APieceIndex);
  if Result and (APieceIndex = 1) then
  begin
    AAffixFlags := TdxHunspellFlags.Create(AffixManager.AffixFlagMode);
    AAffixFlags.Decode(WordStemManager, APiece);
    AAffixFlags.Sort;
    WordStemManager.AffixFlagAliases.Add(AAffixFlags);
  end;
end;

procedure TdxAffixFlagAliasesTable.FreeDataItems;
begin
  // do nothing
end;

function TdxAffixFlagAliasesTable.GetDataIdentifier: PWideChar;
begin
  Result := diAF;
end;

function TdxAffixFlagAliasesTable.GetDataItemSize: Integer;
begin
  Result := 0;
end;

function TdxAffixFlagAliasesTable.GetPieceQuantity: Integer;
begin
  Result := 2;
end;

{ TdxMorphologicAliasesTable }

function TdxMorphologicAliasesTable.AddDataPiece(const APiece: PWideChar;
  ADataIndex, APieceIndex: Integer): Boolean;
begin
  Result := inherited AddDataPiece(APiece, ADataIndex, APieceIndex);
  if Result and (APieceIndex = 1) then
  begin
    WordStemManager.AffixMorphologicAliases[ADataIndex] := StrNew(APiece);
    if AffixManager.ComplexPrefixes then
      StrReverse(WordStemManager.AffixMorphologicAliases[ADataIndex]);
  end;
end;

procedure TdxMorphologicAliasesTable.FreeDataItems;
begin
  // do nothing
end;

function TdxMorphologicAliasesTable.GetCount: Integer;
begin
  Result := WordStemManager.AffixMorphologicAliasesCount;
end;

function TdxMorphologicAliasesTable.GetDataIdentifier: PWideChar;
begin
  Result := diAM;
end;

function TdxMorphologicAliasesTable.GetDataItemSize: Integer;
begin
  Result := 0;
end;

function TdxMorphologicAliasesTable.GetPieceQuantity: Integer;
begin
  Result := 2;
end;

function TdxMorphologicAliasesTable.ReadDataHeader(ALine: PWideChar): Boolean;
var
  ATableSize: Integer;
begin
  Result := ParseHeader(ALine, ATableSize);
  if Result then
    WordStemManager.AllocateAffixMorphologicAliases(ATableSize);
end;

{ TAffixFileReader }

constructor TdxAffixFileAttributeReader.Create(AAffixManager: TdxHunspellAffixManager);
begin
  FAffixManager := AAffixManager;
end;

function TdxAffixFileAttributeReader.ParseFlag(ALine: PWideChar; var ADecodedFlag: Word;
  AReader: TdxHunspellReader): Boolean;
var
  AFlag: PWideChar;
begin
  Result := True;
  AFlag := nil;
  if (ADecodedFlag <> dxNullFlag) and not(ADecodedFlag >= dxDefaultFlags) then
  begin
    Result := False;
    Exit;
  end;
  if not GetSecondPartOfString(ALine, AFlag) then
  begin
    Result := False;
    Exit;
  end;
  ADecodedFlag := AffixManager.DecodeFlag(AFlag);
  StrDispose(AFlag);
end;

function TdxAffixFileAttributeReader.ParseNumber(ALine: PWideChar; out ANumber: Integer): Boolean;
var
  ASecondPart: PWideChar;
begin
  Result := True;
  if ANumber <> - 1 then
  begin
    Result := False;
    Exit;
  end;
  ASecondPart := nil;
  try
    if GetSecondPartOfString(ALine, ASecondPart) then
      ANumber := StrInt(ASecondPart)
    else
      Result := False;
  finally
    StrDispose(ASecondPart);
  end;
end;

function TdxAffixFileAttributeReader.Repeating: Boolean;
begin
  Result := False;
end;

function TdxAffixFileAttributeReader.GetWordStemManager: TdxHunspellWordStemManager;
begin
  Result := AffixManager.WordStemManager;
end;

{ TTryReader }

function TTryReader.GetDataIdentifier: PWideChar;
begin
  Result := diTry;
end;

function TTryReader.Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean;
begin
  Result := GetSecondPartOfString(ALine, AffixManager.FTryChars);
end;

{ TKeyReader }

function TKeyReader.GetDataIdentifier: PWideChar;
begin
  Result := diKey;
end;

function TKeyReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := GetSecondPartOfString(ALine, AffixManager.FKeyboardString);
end;

{ TFlagReader }

function TFlagReader.GetDataIdentifier: PWideChar;
begin
  Result := diFlag;
end;

function TFlagReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
const
  UTF8CodePage = 65001;
begin
  Result := True;
  if StrPos(ALine, 'long') <> nil then
    WordStemManager.AffixFlagMode := afmTwoCharacters;
  if StrPos(ALine, 'num') <> nil then
    WordStemManager.AffixFlagMode := afmNumber;
  if StrPos(ALine, 'UTF-8') <> nil then
  begin
    WordStemManager.AffixFlagMode := afmUnicode;
    WordStemManager.CodePage := UTF8CodePage;
  end;
end;

{ TSetReader }

function TSetReader.GetDataIdentifier: PWideChar;
begin
  Result := diSet;
end;

function TSetReader.Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean;
var
  AEncoding: PWideChar;
begin
  AEncoding := nil;
  try
    Result := GetSecondPartOfString(ALine, AEncoding);
    WordStemManager.CodePage := GetCodePageByName(AEncoding);
  finally
    StrDispose(AEncoding);
  end;
end;

{ TComplexPrefixesReader }

function TComplexPrefixesReader.GetDataIdentifier: PWideChar;
begin
  Result := diComplexPrefixes;
end;

function TComplexPrefixesReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := True;
  AffixManager.ComplexPrefixes := True;
end;

{ TCompoundFlagReader }

function TCompoundFlagReader.GetDataIdentifier: PWideChar;
begin
  Result := diCompoundFlag;
end;

function TCompoundFlagReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := ParseFlag(ALine, AffixManager.FCompoundFlag, AReader);
end;

{ TCompoundBeginReader }

function TCompoundBeginReader.GetDataIdentifier: PWideChar;
begin
  Result := diCompoundBegin;
end;

function TCompoundBeginReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  if AffixManager.ComplexPrefixes then
    Result := ParseFlag(ALine, AffixManager.FCompoundEnd, AReader)
  else
    Result := ParseFlag(ALine, AffixManager.FCompoundBegin, AReader);
end;

{ TCompoundEndReader }

function TCompoundEndReader.GetDataIdentifier: PWideChar;
begin
  Result := diCompoundEnd;
end;

function TCompoundEndReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  if AffixManager.ComplexPrefixes then
    Result := ParseFlag(ALine, AffixManager.FCompoundBegin, AReader)
  else
    Result := ParseFlag(ALine, AffixManager.FCompoundEnd, AReader);
end;

{ TCompoundMiddleReader }

function TCompoundMiddleReader.GetDataIdentifier: PWideChar;
begin
  Result := diCompoundMiddle;
end;

function TCompoundMiddleReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := ParseFlag(ALine, AffixManager.FCompoundMiddle, AReader);
end;

{ TCompoundWordMaxReader }

function TCompoundWordMaxReader.GetDataIdentifier: PWideChar;
begin
  Result := diCompoundWordMax;
end;

function TCompoundWordMaxReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := ParseNumber(ALine, AffixManager.FCompoundWordMax);
end;

{ TCompoundRootReader }

function TCompoundRootReader.GetDataIdentifier: PWideChar;
begin
  Result := diCompoundRoot;
end;

function TCompoundRootReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := ParseFlag(ALine, AffixManager.FCompoundRoot, AReader);
end;

{ TCompoundPermitFlagReader }

function TCompoundPermitFlagReader.GetDataIdentifier: PWideChar;
begin
  Result := diCompoundPermitFlag;
end;

function TCompoundPermitFlagReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := ParseFlag(ALine, AffixManager.FCompoundPermit, AReader);
end;

{ TCompoundForbidFlagReader }

function TCompoundForbidFlagReader.GetDataIdentifier: PWideChar;
begin
  Result := diCompoundForbidFlag;
end;

function TCompoundForbidFlagReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := ParseFlag(ALine, AffixManager.FCompoundForbid, AReader);
end;

{ TCheckCompoundDupReader }

function TCheckCompoundDupReader.GetDataIdentifier: PWideChar;
begin
  Result := diCheckCompoundDup;
end;

function TCheckCompoundDupReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := True;
  AffixManager.FCheckCompoundDup := True;
end;

{ TCheckCompoundRepReader }

function TCheckCompoundRepReader.GetDataIdentifier: PWideChar;
begin
  Result := diCheckCompoundRep;
end;

function TCheckCompoundRepReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := True;
  AffixManager.FCheckCompoundRep := True;
end;

{ TCheckCompoundTripleReader }

function TCheckCompoundTripleReader.GetDataIdentifier: PWideChar;
begin
  Result := diCheckCompoundTriple;
end;

function TCheckCompoundTripleReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := True;
  AffixManager.FCheckCompoundTriple := True;
end;

{ TSimplifiedTripleReader }

function TSimplifiedTripleReader.GetDataIdentifier: PWideChar;
begin
  Result := diSimplifiedTriple;
end;

function TSimplifiedTripleReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := True;
  AffixManager.FSimplifiedTriple := True;
end;

{ TCheckCompoundCaseReader }

function TCheckCompoundCaseReader.GetDataIdentifier: PWideChar;
begin
  Result := diCheckCompoundCase;
end;

function TCheckCompoundCaseReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := True;
  AffixManager.FCheckCompoundCase := True;
end;

{ TNoSuggestReader }

function TNoSuggestReader.GetDataIdentifier: PWideChar;
begin
  Result := diNoSuggest;
end;

function TNoSuggestReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := ParseFlag(ALine, AffixManager.FNoSuggest, AReader);
end;

{ TForbiddenWordReader }

function TForbiddenWordReader.GetDataIdentifier: PWideChar;
begin
  Result := diForbiddenWord;
end;

function TForbiddenWordReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
var
  AFlag: Word;
begin
  AFlag := AffixManager.ForbiddenWordFlag;
  Result := ParseFlag(ALine, AFlag, AReader);
  if Result then
    WordStemManager.ForbiddenWordFlag := AFlag;
end;

{ TLemmaPresentReader }

function TLemmaPresentReader.GetDataIdentifier: PWideChar;
begin
  Result := diLemmaPresent;
end;

function TLemmaPresentReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := ParseFlag(ALine, AffixManager.FLemmaPresent, AReader);
end;

{ TCircumfixReader }

function TCircumfixReader.GetDataIdentifier: PWideChar;
begin
  Result := diCircumfix;
end;

function TCircumfixReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := ParseFlag(ALine, AffixManager.FCircumfix, AReader);
end;

{ TInCompoundOnlyReader }

function TInCompoundOnlyReader.GetDataIdentifier: PWideChar;
begin
  Result := diInCompoundOnly;
end;

function TInCompoundOnlyReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := ParseFlag(ALine, AffixManager.FInCompoundOnly, AReader);
end;

{ TPseudoRootReader }

function TPseudoRootReader.GetDataIdentifier: PWideChar;
begin
  Result := diPseudoRoot;
end;

function TPseudoRootReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := ParseFlag(ALine, AffixManager.FNeedAffix, AReader)
end;

{ TNeedAffixReader }

function TNeedAffixReader.GetDataIdentifier: PWideChar;
begin
  Result := diNeedAffix;
end;

function TNeedAffixReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := ParseFlag(ALine, AffixManager.FNeedAffix, AReader);
end;

{ TCompoundMinReader }

function TCompoundMinReader.GetDataIdentifier: PWideChar;
begin
  Result := diCompoundMin;
end;

function TCompoundMinReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := ParseNumber(ALine, AffixManager.FCompoundPartMin);
  if AffixManager.FCompoundPartMin < 1 then
    AffixManager.FCompoundPartMin := 1;
end;

{ TCompoundSyllableReader }

function TCompoundSyllableReader.GetDataIdentifier: PWideChar;
begin
  Result := diCompoundSyllable;
end;

function TCompoundSyllableReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := AffixManager.ParseCompoundSyllable(ALine, AReader);
end;

{ TSyllableNumReader }

function TSyllableNumReader.GetDataIdentifier: PWideChar;
begin
  Result := diSyllableNum;
end;

function TSyllableNumReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := GetSecondPartOfString(ALine, AffixManager.FSyllableNum);
end;

{ TWordCharsReader }

function TWordCharsReader.GetDataIdentifier: PWideChar;
begin
  Result := diWordChars;
end;

function TWordCharsReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := GetSecondPartOfString(ALine, AffixManager.FWordChars);
end;

{ TIgnoreReader }

function TIgnoreReader.GetDataIdentifier: PWideChar;
begin
  Result := diIgnore;
end;

function TIgnoreReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
var
  AIgnoredChars: PWideChar;
begin
  AIgnoredChars := AffixManager.IgnoredChars;
  Result := GetSecondPartOfString(ALine, AIgnoredChars);
  if Result then
    AffixManager.IgnoredChars := AIgnoredChars;
end;

{ TRepReader }

function TRepReader.GetDataIdentifier: PWideChar;
begin
  Result := diRep;
end;

function TRepReader.Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean;
begin
  Result := AffixManager.ParseRep(ALine, AReader);
end;

function TRepReader.Repeating: Boolean;
begin
  Result := True;
end;

{ TIConvReader }

function TIConvReader.GetDataIdentifier: PWideChar;
begin
  Result := diIConv;
end;

function TIConvReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := AffixManager.InputConvertTable.ReadData(ALine, AReader);
end;

{ TOConvReader }

function TOConvReader.GetDataIdentifier: PWideChar;
begin
  Result := diOConv;
end;

function TOConvReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := AffixManager.OutputConvertTable.ReadData(ALine, AReader);
end;

{ TCheckCompoundPatternReader }

function TCheckCompoundPatternReader.GetDataIdentifier: PWideChar;
begin
  Result := diCheckCompoundPattern;
end;

function TCheckCompoundPatternReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := AffixManager.FCheckCompoundPatternTable.ReadData(ALine, AReader);
end;

function TCheckCompoundPatternReader.Repeating: Boolean;
begin
  Result := True;
end;

{ TCompoundRuleReader }

function TCompoundRuleReader.GetDataIdentifier: PWideChar;
begin
  Result := diCompoundRule;
end;

function TCompoundRuleReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := AffixManager.FCompoundRuleTable.ReadData(ALine, AReader);
end;

function TCompoundRuleReader.Repeating: Boolean;
begin
  Result := True;
end;

{ TMapReader }

function TMapReader.GetDataIdentifier: PWideChar;
begin
  Result := diMap;
end;

function TMapReader.Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean;
begin
  Result := AffixManager.FMapTable.ReadData(ALine, AReader);
end;

function TMapReader.Repeating: Boolean;
begin
  Result := True;
end;

{ TPhoneReader }

function TPhoneReader.GetDataIdentifier: PWideChar;
begin
  Result := diPhone;
end;

function TPhoneReader.Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean;
begin
  Result := AffixManager.FPhoneTable.ReadData(ALine, AReader);
end;

{ TBreakReader }

function TBreakReader.GetDataIdentifier: PWideChar;
begin
  Result := diBreak;
end;

function TBreakReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := AffixManager.FBreakTable.ReadData(ALine, AReader);
end;

function TBreakReader.Repeating: Boolean;
begin
  Result := True;
end;

{ TLangReader }

function TLangReader.GetDataIdentifier: PWideChar;
begin
  Result := diLang;
end;

function TLangReader.Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean;
var
  ALanguage: PWideChar;
  ALanguageID: Integer;
begin
  ALanguage := nil;
  try
    Result := GetSecondPartOfString(ALine, ALanguage);
    if Result then
    begin
      ALanguageID := GetLanguageID(ALanguage);
      if ALanguageID <> LanguageNone then
        AffixManager.Language := ALanguageID;
    end;
  finally
    StrDispose(ALanguage);
  end;
end;

{ TVersionReader }

function TVersionReader.GetDataIdentifier: PWideChar;
begin
  Result := diVersion;
end;

function TVersionReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := True;
  ALine := ALine + 7;
  while (ALine^ = ' ') or (ALine^ = #9) do
    Inc(ALine);
  if ALine^ <> #0 then
    AffixManager.FVersion := StrNew(ALine)
  else
    AffixManager.FVersion := nil;
end;

{ TNoSplitSuggestionsReader }

function TNoSplitSuggestionsReader.GetDataIdentifier: PWideChar;
begin
  Result := diNoSplitSuggestions;
end;

function TNoSplitSuggestionsReader.Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean;
begin
  Result := True;
  AffixManager.FNoSplitSuggestions := True;
end;

{ TNgramSuggestionReader }

function TNgramSuggestionReader.GetDataIdentifier: PWideChar;
begin
  Result := diMaxNgramSuggestions;
end;

function TNgramSuggestionReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := ParseNumber(ALine, AffixManager.FMaxNgramSuggestionCount);
end;

{ TFullStripReader }

function TFullStripReader.GetDataIdentifier: PWideChar;
begin
  Result := diFullStrip;
end;

function TFullStripReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := True;
  AffixManager.FFullStrip := True;
end;

{ TSuggestionsWithDotsReader }

function TSuggestionsWithDotsReader.GetDataIdentifier: PWideChar;
begin
  Result := diSuggestionsWithDots;
end;

function TSuggestionsWithDotsReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := True;
  AffixManager.FSuggestionsWithDots := True;
end;

{ TKeepCaseReader }

function TKeepCaseReader.GetDataIdentifier: PWideChar;
begin
  Result := diKeepCase;
end;

function TKeepCaseReader.Process(ALine: PWideChar; AReader: TdxHunspellReader): Boolean;
begin
  Result := ParseFlag(ALine, AffixManager.FKeepCase, AReader);
end;

{ TSubStandardReader }

function TSubStandardReader.GetDataIdentifier: PWideChar;
begin
  Result := diSubStandard;
end;

function TSubStandardReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := ParseFlag(ALine, AffixManager.FSubstandard, AReader);
end;

{ TCheckSharpsReader }

function TCheckSharpsReader.GetDataIdentifier: PWideChar;
begin
  Result := diCheckSharps;
end;

function TCheckSharpsReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
begin
  Result := True;
  AffixManager.FCheckSharps := True;
end;

{ TPrefixReader }

function TPrefixReader.GetDataIdentifier: PWideChar;
begin
  Result := diPrefix;
end;

function TPrefixReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
var
  AAffixType: TdxAffixType;
begin
  AAffixType := atPrefix;
  if AffixManager.ComplexPrefixes then
    AAffixType := atSuffix;
  Result := AffixManager.ParseAffix(ALine, AReader, AAffixType);
end;

function TPrefixReader.Repeating: Boolean;
begin
  Result := True;
end;

{ TSuffixReader }

function TSuffixReader.GetDataIdentifier: PWideChar;
begin
  Result := diSuffix;
end;

function TSuffixReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
var
  AAffixType: TdxAffixType;
begin
  AAffixType := atSuffix;
  if AffixManager.ComplexPrefixes then
    AAffixType := atPrefix;
  Result := AffixManager.ParseAffix(ALine, AReader, AAffixType);
end;

function TSuffixReader.Repeating: Boolean;
begin
  Result := True;
end;

{ TAffixFlagAliasReader }

function TAffixFlagAliasReader.GetDataIdentifier: PWideChar;
begin
  Result := diAf;
end;

function TAffixFlagAliasReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
var
  ATable: TdxAffixFlagAliasesTable;
begin
  Result := False;
  if WordStemManager.AffixFlagAliases.Count = 0 then
  begin
    ATable := TdxAffixFlagAliasesTable.Create(AffixManager);
    try
      Result := ATable.ReadData(ALine, AReader);
    finally
      ATable.Free;
    end;
  end;
end;

function TAffixFlagAliasReader.Repeating: Boolean;
begin
  Result := True;
end;

{ TMorphologicAliasReader }

function TMorphologicAliasReader.GetDataIdentifier: PWideChar;
begin
  Result := diAm;
end;

function TMorphologicAliasReader.Process(ALine: PWideChar;
  AReader: TdxHunspellReader): Boolean;
var
  ATable: TdxMorphologicAliasesTable;
begin
  Result := False;
  if WordStemManager.AffixMorphologicAliasesCount = 0 then
  begin
    ATable := TdxMorphologicAliasesTable.Create(AffixManager);
    try
      Result := ATable.ReadData(ALine, AReader);
    finally
      ATable.Free;
    end;
  end;
end;

function TMorphologicAliasReader.Repeating: Boolean;
begin
  Result := True;
end;

{ TdxAffixReaderList }

constructor TdxAffixReaderList.Create(AAffixManager: TdxHunspellAffixManager);
begin
  inherited Create;
  FAffixManager := AAffixManager;
end;

procedure TdxAffixReaderList.Add(AReaderClass: TdxAffixFileAttributeReaderClass);
begin
  inherited Add(AReaderClass.Create(FAffixManager));
end;

function TdxAffixReaderList.GetItem(Index: Integer): TdxAffixFileAttributeReader;
begin
  Result := TdxAffixFileAttributeReader(inherited Items[Index]);
end;

procedure TdxAffixReaderList.ProcessLine(ALine: PWideChar; AReader: TdxHunspellReader);
var
  AIndex: Integer;
  AItem: TdxAffixFileAttributeReader;
  ADataIdentifier: PWideChar;
begin
  RemoveCRLF(ALine);
  if ALine^ = #0 then
    Exit;
  AIndex := 0;
  while AIndex < Count do
  begin
    AItem := Items[AIndex];
    ADataIdentifier := AItem.GetDataIdentifier;
    if StrLComp(ALine, ADataIdentifier, StrLen(ADataIdentifier)) = 0 then
    begin
      if not AItem.Process(ALine, AReader) then
        HunspellError(Format('Error: attribute %s has wrong data',
          [dxWideStringToAnsiString(AItem.GetDataIdentifier)]), AReader.LineIndex);
      if not AItem.Repeating then
      begin
        Delete(AIndex);
        AItem.Free;
        Continue;
      end;
    end;
    Inc(AIndex);
  end;
end;

end.
