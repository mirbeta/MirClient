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

unit dxRichEdit.Api.FindAndReplace;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, RegularExpressions, Generics.Defaults, Generics.Collections,
  dxCoreClasses, dxGenerics,

  dxRichEdit.NativeApi,
  dxRichEdit.Api.NativeDocument,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.FindAndReplace,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable;

type
  TdxTextSearchProvider = class;

  { TdxRegexSearchInfo }

  TdxRegexSearchInfo = (
    StartExtended,
    EndExtended
  );

  TdxRegexSearchInfos = set of TdxRegexSearchInfo;

  { TdxSearchResult }

  TdxSearchResult = class
  strict private
    FValue: TdxRunInfo;
  protected
    function GetSuccess: Boolean;
  public
    constructor Create(AValue: TdxRunInfo); overload;
    procedure Clear; virtual;

    property Value: TdxRunInfo read FValue;
    property Success: Boolean read GetSuccess;
  end;

  { TdxSearchStrategy }

  TdxSearchStrategy = class abstract
  public
    function Match(const APattern: string; AOptions: TdxSearchOptions; const AStart, AEnd: TdxDocumentLogPosition): TdxSearchResult; virtual; abstract;
  end;

  { TdxBufferedDocumentCharacterIterator }

  TdxBufferedDocumentCharacterIterator = class abstract
  strict private
    FOffset: Integer;
    FBuffer: TStringBuilder;
    FStartPosition: TdxDocumentModelPosition;
    FEndPosition: TdxDocumentModelPosition;
    function GetPieceTable: TdxPieceTable;
    function GetBufferLength: Integer;
  private
    function GetCharacters(Index: Integer): Char;
  protected
    function GetCurrentPosition: TdxDocumentModelPosition; virtual; abstract;
    procedure SetCurrentPosition(const Value: TdxDocumentModelPosition); virtual; abstract;
    procedure EnsureStartPositionIsVisible; virtual;
    function IsRunVisible(ARunIndex: TdxRunIndex): Boolean;
    function GetPositionForward(const APosition: TdxDocumentModelPosition; AOffset: Integer): TdxDocumentModelPosition;
    function GetPositionBackward(const APosition: TdxDocumentModelPosition; AOffset: Integer): TdxDocumentModelPosition;
    function GetRunText(ARun: TdxTextRunBase): string;
    procedure AppendBufferCore(AMinLength: Integer); virtual; abstract;
    procedure FlushBuffer; virtual; abstract;
    function GetCharacterIndex(AIndex: Integer): Integer; virtual; abstract;

    property Buffer: TStringBuilder read FBuffer;
    property Offset: Integer read FOffset write FOffset;
  public
    constructor Create(const AStartPosition: TdxDocumentModelPosition);
    destructor Destroy; override;
    procedure AppendBuffer(AMinLength: Integer); virtual;
    procedure ShiftBuffer(ADelta: Integer); virtual;
    function GetPosition(const APosition: TdxDocumentModelPosition; AOffset: Integer): TdxDocumentModelPosition; overload; virtual; abstract;
    function GetPosition(AOffset: Integer): TdxDocumentModelPosition; overload; virtual;
    function Compare(AOffset: Integer; ACh: Char; AIgnoreCase: Boolean): Boolean; overload;
    function Compare(ACh1: Char; ACh2: Char; AIgnoreCase: Boolean): Boolean; overload;

    function IsCharacterExist(AIndex: Integer): Boolean; virtual; abstract;

    property StartPosition: TdxDocumentModelPosition read FStartPosition;
    property EndPosition: TdxDocumentModelPosition read FEndPosition;
    property PieceTable: TdxPieceTable read GetPieceTable;
    property CurrentPosition: TdxDocumentModelPosition read GetCurrentPosition write SetCurrentPosition;
    property BufferLength: Integer read GetBufferLength;
    property Characters[Index: Integer]: Char read GetCharacters; default;
  end;

  { TdxBufferedDocumentCharacterIteratorForward }

  TdxBufferedDocumentCharacterIteratorForward = class(TdxBufferedDocumentCharacterIterator)
  protected
    function GetCurrentPosition: TdxDocumentModelPosition; override;
    procedure SetCurrentPosition(const Value: TdxDocumentModelPosition); override;
    function GetCharacterIndex(AIndex: Integer): Integer; override;
    procedure AppendBufferCore(AMinLength: Integer); override;
    procedure FlushBuffer; override;
  public
    function GetPosition(const APosition: TdxDocumentModelPosition; AOffset: Integer): TdxDocumentModelPosition; override;
    function IsCharacterExist(AIndex: Integer): Boolean; override;
  end;

  { TdxBufferedDocumentCharacterIteratorBackward }

  TdxBufferedDocumentCharacterIteratorBackward = class(TdxBufferedDocumentCharacterIterator)
  protected
    function GetCurrentPosition: TdxDocumentModelPosition; override;
    procedure SetCurrentPosition(const Value: TdxDocumentModelPosition); override;
    function GetCharacterIndex(AIndex: Integer): Integer; override;
    procedure AppendBufferCore(AMinLength: Integer); override;
    procedure FlushBuffer; override;
  public
    function GetPosition(const APosition: TdxDocumentModelPosition; AOffset: Integer): TdxDocumentModelPosition; override;
    function IsCharacterExist(AIndex: Integer): Boolean; override;
  end;

  { TdxRegexSearchResult }

  TdxRegexSearchResult = class(TdxSearchResult)
  strict private
    FMatchInfo: TdxBufferedRegexSearchResult;
  public
    constructor Create(AResult: TdxRunInfo; AMatchInfo: TdxBufferedRegexSearchResult);
    procedure Clear; override;

    property MatchInfo: TdxBufferedRegexSearchResult read FMatchInfo;
  end;

  { IdxTextSearchStateBase }

  IdxTextSearchStateBase = interface
    ['{77B4195C-6D8D-425E-B5DB-1C9A8ED9EC02}']
    function GetSearchScope: TdxSearchScope;
    function GetShouldSearch: Boolean;
    function GetType: TdxSearchState;
    function FindNext(AStartPosition: TdxDocumentLogPosition): TdxSearchResult;
    function GetNextStateType: TdxSearchState;

    property SearchScope: TdxSearchScope read GetSearchScope;
    property &Type: TdxSearchState read GetType;
    property ShouldSearch: Boolean read GetShouldSearch;
  end;

  { TdxTextSearchStateBase }

  TdxTextSearchStateBase = class abstract(TInterfacedObject, IdxTextSearchStateBase)
  strict private
    FSearchProvider: TdxTextSearchProvider;
    function GetDocumentModel: TdxDocumentModel;
    function GetSearchParameters: TdxSearchParameters;
    function GetSearchContext: TdxSearchContext;
    function GetPieceTable: TdxPieceTable;
    function GetSearchLimitOffset: Integer;
  protected
    function GetSearchScope: TdxSearchScope; virtual; abstract;
    function GetType: TdxSearchState; virtual; abstract;
    function GetShouldSearch: Boolean; virtual; abstract;
    function CanSearch(AStartPosition: TdxDocumentLogPosition): Boolean; virtual;
    function DoSearch(AStart, AEnd: TdxDocumentLogPosition): TdxSearchResult; overload;
    function DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult; overload; virtual; abstract;
    function CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean; virtual; abstract;

    property SearchProvider: TdxTextSearchProvider read FSearchProvider;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property SearchParameters: TdxSearchParameters read GetSearchParameters;
    property SearchContext: TdxSearchContext read GetSearchContext;
    property PieceTable: TdxPieceTable read GetPieceTable;
    property SearchLimitOffset: Integer read GetSearchLimitOffset;
  public
    constructor Create(ASearchProvider: TdxTextSearchProvider);
    function FindNext(AStartPosition: TdxDocumentLogPosition): TdxSearchResult; virtual;
    function GetNextStateType: TdxSearchState; virtual; abstract;

    property SearchScope: TdxSearchScope read GetSearchScope;
    property &Type: TdxSearchState read GetType;
    property ShouldSearch: Boolean read GetShouldSearch;
  end;

  { TdxFindStartState }

  TdxFindStartState = class(TdxTextSearchStateBase)
  protected
    function GetSearchScope: TdxSearchScope; override;
    function GetType: TdxSearchState; override;
    function GetShouldSearch: Boolean; override;
    function DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult; override;
    function CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean; override;
  public
    function FindNext(AStartPosition: TdxDocumentLogPosition): TdxSearchResult; override;
    function GetNextStateType: TdxSearchState; override;
  end;

  { TdxFindInSelectionForwardState }

  TdxFindInSelectionForwardState = class(TdxTextSearchStateBase)
  protected
    function GetSearchScope: TdxSearchScope; override;
    function GetType: TdxSearchState; override;
    function GetShouldSearch: Boolean; override;
    function DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult; override;
    function CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean; override;
  public
    function GetNextStateType: TdxSearchState; override;
  end;

  { TdxFindAfterCaretForwardState }

  TdxFindAfterCaretForwardState = class(TdxTextSearchStateBase)
  protected
    function GetSearchScope: TdxSearchScope; override;
    function GetType: TdxSearchState; override;
    function GetShouldSearch: Boolean; override;
    function DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult; override;
    function CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean; override;
  public
    function GetNextStateType: TdxSearchState; override;
  end;

  { TdxFindBeforeCaretForwardState }

  TdxFindBeforeCaretForwardState = class(TdxTextSearchStateBase)
  protected
    function GetSearchScope: TdxSearchScope; override;
    function GetType: TdxSearchState; override;
    function GetShouldSearch: Boolean; override;
    function DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult; override;
    function CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean; override;
  public
    function GetNextStateType: TdxSearchState; override;
  end;

  { TdxFindAfterSelectionForwardState }

  TdxFindAfterSelectionForwardState = class(TdxTextSearchStateBase)
  protected
    function GetSearchScope: TdxSearchScope; override;
    function GetType: TdxSearchState; override;
    function GetShouldSearch: Boolean; override;
    function DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult; override;
    function CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean; override;
  public
    function GetNextStateType: TdxSearchState; override;
  end;

  { TdxFindBeforeSelectionForwardState }

  TdxFindBeforeSelectionForwardState = class(TdxTextSearchStateBase)
  protected
    function GetSearchScope: TdxSearchScope; override;
    function GetType: TdxSearchState; override;
    function GetShouldSearch: Boolean; override;
    function DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult; override;
    function CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean; override;
  public
    function GetNextStateType: TdxSearchState; override;
  end;

  { TdxFindFinishState }

  TdxFindFinishState = class(TdxTextSearchStateBase)
  protected
    function GetSearchScope: TdxSearchScope; override;
    function GetType: TdxSearchState; override;
    function GetShouldSearch: Boolean; override;
    function DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult; override;
    function CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean; override;
  public
    function GetNextStateType: TdxSearchState; override;
  end;

  { TdxFindInSelectionBackwardState }

  TdxFindInSelectionBackwardState = class(TdxTextSearchStateBase)
  protected
    function GetSearchScope: TdxSearchScope; override;
    function GetType: TdxSearchState; override;
    function GetShouldSearch: Boolean; override;
    function DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult; override;
    function CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean; override;
  public
    function GetNextStateType: TdxSearchState; override;
  end;

  { TdxFindAfterCaretBackwardState }

  TdxFindAfterCaretBackwardState = class(TdxTextSearchStateBase)
  protected
    function GetSearchScope: TdxSearchScope; override;
    function GetType: TdxSearchState; override;
    function GetShouldSearch: Boolean; override;
    function DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult; override;
    function CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean; override;
  public
    function GetNextStateType: TdxSearchState; override;
  end;

  { TdxFindBeforeCaretBackwardState }

  TdxFindBeforeCaretBackwardState = class(TdxTextSearchStateBase)
  protected
    function GetSearchScope: TdxSearchScope; override;
    function GetType: TdxSearchState; override;
    function GetShouldSearch: Boolean; override;
    function DoSearch(AEndPosition: TdxDocumentLogPosition): TdxSearchResult; override;
    function CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean; override;
  public
    function GetNextStateType: TdxSearchState; override;
  end;

  { TdxFindAfterSelectionBackwardState }

  TdxFindAfterSelectionBackwardState = class(TdxTextSearchStateBase)
  protected
    function GetSearchScope: TdxSearchScope; override;
    function GetType: TdxSearchState; override;
    function GetShouldSearch: Boolean; override;
    function DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult; override;
    function CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean; override;
  public
    function GetNextStateType: TdxSearchState; override;
  end;

  { TdxFindBeforeSelectionBackwardState }

  TdxFindBeforeSelectionBackwardState = class(TdxTextSearchStateBase)
  protected
    function GetSearchScope: TdxSearchScope; override;
    function GetType: TdxSearchState; override;
    function GetShouldSearch: Boolean; override;
    function DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult; override;
    function CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean; override;
  public
    function GetNextStateType: TdxSearchState; override;
  end;

  { TdxTextSearchProvider }

  TdxTextSearchProvider = class abstract
  strict private
    FPieceTable: TdxPieceTable;
    FState: IdxTextSearchStateBase;
    FParameters: TdxSearchParameters;
    FContext: TdxSearchContext;
  protected
    function FindRunInfo(AStart, AEnd: TdxDocumentLogPosition): TdxSearchResult;
    function CreateSearchStrategy: TdxSearchStrategy; virtual;
    function GetStartAtPosition(ARunInfo: TdxRunInfo): TdxDocumentLogPosition; virtual; abstract;
    function CreateStringSearch: TdxSearchStrategy; virtual; abstract;
    function CreateRegexSearch: TdxSearchStrategy; virtual; abstract;

    property Parameters: TdxSearchParameters read FParameters;
    property Context: TdxSearchContext read FContext;
  public
    constructor Create(APieceTable: TdxPieceTable; AParameters: TdxSearchParameters; AContext: TdxSearchContext);
    function FindNextPosition: TdxRunInfo; virtual;
    procedure ChangeState(ASearchType: TdxSearchState);
    procedure SetState(ASearchType: TdxSearchState); virtual; abstract;

    property State: IdxTextSearchStateBase read FState write FState;
    property PieceTable: TdxPieceTable read FPieceTable;
  end;

  { TdxTextSearchForwardProvider }

  TdxTextSearchForwardProvider = class(TdxTextSearchProvider)
  protected
    function CreateStringSearch: TdxSearchStrategy; override;
    function CreateRegexSearch: TdxSearchStrategy; override;
    function GetStartAtPosition(ARunInfo: TdxRunInfo): TdxDocumentLogPosition; override;
    function GetNextVisiblePosition(const APosition: TdxDocumentModelPosition): TdxDocumentLogPosition; overload;
    function GetNextVisiblePosition(APosition: TdxDocumentLogPosition): TdxDocumentLogPosition; overload;
  public
    procedure SetState(ASearchType: TdxSearchState); override;
  end;

  { TdxTextSearchBackwardProvider }

  TdxTextSearchBackwardProvider = class(TdxTextSearchProvider)
  protected
    function CreateStringSearch: TdxSearchStrategy; override;
    function CreateRegexSearch: TdxSearchStrategy; override;
    function GetStartAtPosition(ARunInfo: TdxRunInfo): TdxDocumentLogPosition; override;
    function GetPrevVisiblePosition(const APosition: TdxDocumentModelPosition): TdxDocumentLogPosition; overload;
    function GetPrevVisiblePosition(APosition: TdxDocumentLogPosition): TdxDocumentLogPosition; overload;
  public
    procedure SetState(ASearchType: TdxSearchState); override;
  end;

  { TdxDocumentCharacterIterator }

  TdxDocumentCharacterIterator = class abstract
  strict private
    FCurrentPosition: TdxNullableValue<TdxDocumentModelPosition>;
    FStartPosition: TdxDocumentLogPosition;
    FEndPosition: TdxDocumentLogPosition;
    FPieceTable: TdxPieceTable;
    FCachedRunIndex: TdxRunIndex;
    FIsCachedRunVisible: Boolean;
    function GetCurrentPosition: TdxDocumentModelPosition;
    function GetCurrentRun: TdxTextRunBase;
    procedure ResetCurrentPosition;
  protected
    procedure InitializeCurrentPosition; virtual; abstract;
    procedure SetInterval(AStart, AEnd: TdxDocumentLogPosition); virtual;
    procedure SetCurrentPosition(const APos: TdxDocumentModelPosition); virtual;
    procedure InvalidateCurrentPosition; virtual;
    function GetModelPosition(AOffset: Integer): TdxDocumentModelPosition; overload;
    function GetModelPosition(const APos: TdxDocumentModelPosition; AOffset: Integer): TdxDocumentModelPosition; overload;
    function GetCharacterByPosition(const APos: TdxDocumentModelPosition): Char;
    function IsVisible(const APos: TdxDocumentModelPosition): Boolean; virtual;
    procedure ShiftModelPosition(var APos: TdxDocumentModelPosition; AOffset: Integer);
    procedure MoveToVisiblePosition(var AResult: TdxDocumentModelPosition); virtual; abstract;

    property CurrentRun: TdxTextRunBase read GetCurrentRun;
  public
    constructor Create(APieceTable: TdxPieceTable; AStart, AEnd: TdxDocumentLogPosition); overload;
    constructor Create(APieceTable: TdxPieceTable); overload;
    function CanMoveOnOffset(AOffset: Integer): Boolean; virtual; abstract;
    function GetCharacter: Char; overload; virtual;
    function GetCharacter(AOffset: Integer): Char; overload; virtual;
    function GetCharacter(AOffset: Integer; out APos: TdxDocumentLogPosition): Char; overload; virtual;
    function HasNextChar: Boolean; virtual;
    procedure ShiftCurrentPosition(AOffset: Integer); virtual;
    procedure MoveNext; virtual;

    property StartPosition: TdxDocumentLogPosition read FStartPosition write FStartPosition;
    property EndPosition: TdxDocumentLogPosition read FEndPosition write FEndPosition;
    property CurrentPosition: TdxDocumentModelPosition read GetCurrentPosition write SetCurrentPosition;
    property PieceTable: TdxPieceTable read FPieceTable;
  end;

  { TdxDocumentCharacterIteratorForward }

  TdxDocumentCharacterIteratorForward = class(TdxDocumentCharacterIterator)
  protected
    procedure InitializeCurrentPosition; override;
    procedure MoveToVisiblePosition(var APos: TdxDocumentModelPosition); override;
  public
    function CanMoveOnOffset(AOffset: Integer): Boolean; override;
  end;

  { TdxBMTextSearchBase }

  TdxBMTextSearchBase = class abstract
  private const
    UnicodeAlphabetSize = 65536;
  strict private
    FBadCharacterShiftTable: TArray<Integer>;
    FGoodSuffixShiftTable: TArray<Integer>;
    FOriginalPattern: string;
    FPattern: string;
    FCaseSensitive: Boolean;
    FPieceTable: TdxPieceTable;
  protected
    procedure InitializeTables; virtual;
    function CalculateWorkPattern: string; virtual;
    function GetCharacter(AIterator: TdxDocumentCharacterIterator; AOffset: Integer): Char;
    function CreateUnicodeAlphabetTable(AInitialValue: Integer): TArray<Integer>;
    procedure PopulateBadCharacterShiftTable(const APattern: string; const ATable: TArray<Integer>);
    function CreateBadCharacterShiftTable: TArray<Integer>;
    function CreateSuffixTable(const APattern: string): TArray<Integer>;
    function CreateGoodSuffixShiftTable: TArray<Integer>;

    property Pattern: string read FPattern write FPattern;
    property BadCharacterShiftTable: TArray<Integer> read FBadCharacterShiftTable;
    property GoodSuffixShiftTable: TArray<Integer> read FGoodSuffixShiftTable;
  public
    constructor Create(APieceTable: TdxPieceTable; const APattern: string; ACaseSensitive: Boolean);
    function Search(const AStart, AEnd: TdxDocumentLogPosition): TdxNullableValue<TdxDocumentModelPosition>; virtual; abstract;

    property PieceTable: TdxPieceTable read FPieceTable;
    property OriginalPattern: string read FOriginalPattern;
    property CaseSensitive: Boolean read FCaseSensitive;
  end;

 { TdxBMTextSearchForward }

  TdxBMTextSearchForward = class(TdxBMTextSearchBase)
  public
    function Search(const AStart, AEnd: TdxDocumentLogPosition): TdxNullableValue<TdxDocumentModelPosition>; override;
  end;

  { TdxBMTextSearchBackward }

  TdxBMTextSearchBackward = class(TdxBMTextSearchBase)
  protected
    function CalculateWorkPattern: string; override;
  public
    function Search(const AStart, AEnd: TdxDocumentLogPosition): TdxNullableValue<TdxDocumentModelPosition>; override;
  end;

  { TdxSearchIntervalCalculator }

  TdxSearchIntervalCalculator = class abstract
  public
    function CalculateStartPosition(const AStart, AEnd: TdxDocumentLogPosition;
      APrevResult: TdxSearchResult): TdxDocumentLogPosition; virtual; abstract;
    function CalculateEndPosition(const AStart, AEnd: TdxDocumentLogPosition;
      APrevResult: TdxSearchResult): TdxDocumentLogPosition; virtual; abstract;
  end;

  { TdxSearchIntervalCalculatorForward }

  TdxSearchIntervalCalculatorForward = class(TdxSearchIntervalCalculator)
  public
    function CalculateStartPosition(const AStart, AEnd: TdxDocumentLogPosition;
      APrevResult: TdxSearchResult): TdxDocumentLogPosition; override;
    function CalculateEndPosition(const AStart, AEnd: TdxDocumentLogPosition;
      APrevResult: TdxSearchResult): TdxDocumentLogPosition; override;
  end;

  { TdxSearchIntervalCalculatorBackward }

  TdxSearchIntervalCalculatorBackward = class(TdxSearchIntervalCalculator)
  public
    function CalculateStartPosition(const AStart, AEnd: TdxDocumentLogPosition;
      APrevResult: TdxSearchResult): TdxDocumentLogPosition; override;
    function CalculateEndPosition(const AStart, AEnd: TdxDocumentLogPosition;
      APrevResult: TdxSearchResult): TdxDocumentLogPosition; override;
  end;

  { TdxBufferedRegexSearchResultCollection }

  TdxBufferedRegexSearchResultCollection = class(TdxList<TdxBufferedRegexSearchResult>);

  { TdxBufferedRegexSearchResultTable }

  TdxBufferedRegexSearchResultTable = class(TDictionary<Integer, TdxBufferedRegexSearchResult>);

  { TdxSearchConditions }

  TdxSearchConditions = record
  strict private
    FStartIndex: Integer;
    FLength: Integer;
    FInput: string;
  public
    constructor Create(const AInput: string); overload;
    constructor Create(const AInput: string; AStartIndex: Integer; ALength: Integer); overload;

    property Input: string read FInput write FInput;
    property StartIndex: Integer read FStartIndex write FStartIndex;
    property Length: Integer read FLength write FLength;
  end;

  { TdxBufferedRegexSearchBase }

  TdxBufferedRegexSearchBase = class abstract
  strict private
    FBufferSize: Integer;
    FBufferShiftSize: Integer;
    FBuffer: TStringBuilder;
    FBufferOffset: Integer;
    FSearchInfo: TdxRegexSearchInfos;
    FRegex: TRegex;
    FBufferStartPosition: TdxDocumentModelPosition;
    FCurrentInputPosition: TdxDocumentModelPosition;
    FEndPosition: TdxDocumentLogPosition;
    FStartPosition: TdxDocumentLogPosition;
    FPieceTable: TdxPieceTable;
  protected
    procedure InitializeSearchInterval(AStart, AEnd: TdxDocumentLogPosition); virtual;
    function CreateIterator(const APosition: TdxDocumentModelPosition): TdxBufferedDocumentCharacterIterator; virtual; abstract;
    function CanMoveNext: Boolean; virtual; abstract;
    function CreateResult(const AMatch: TMatch): TdxBufferedRegexSearchResult; overload;
    function CreateResult(AResults: TdxBufferedRegexSearchResultCollection): TdxBufferedRegexSearchResultCollection; overload;
    function CalculateBufferShiftSize(AResult: TdxBufferedRegexSearchResult): Integer; virtual; abstract;
    function CalculateStartPosition: TdxDocumentModelPosition; virtual; abstract;
    function ProcessBuffer(AIterator: TdxBufferedDocumentCharacterIterator; const ARegEx: TRegex; AResults: TdxBufferedRegexSearchResultCollection): Boolean; virtual;
    procedure ShiftBuffer(AIterator: TdxBufferedDocumentCharacterIterator; ADelta: Integer); virtual;
    function CalculateMatchCore(const ARegEx: TRegex; const AConditions: TdxSearchConditions): TMatch; virtual; abstract;
    function IsMatchInTheEndOfBuffer(const AMatch: TMatch): Boolean;
    function IsMatchInTheStartOfBuffer(const AMatch: TMatch): Boolean;
    function IsMatchGreatTheEndOfBuffer(const AMatch: TMatch): Boolean;
    function ShouldRecalcMatchAtStartOfBuffer(var AConditions: TdxSearchConditions): Boolean; virtual;
    function ShouldRecalcMatchAtEndOfBuffer(var AConditions: TdxSearchConditions): Boolean; virtual;
    procedure CalculateSearchConditions(const APrevMatch: TMatch; var AConditions: TdxSearchConditions); virtual; abstract;
    procedure AppendBuffer(AIterator: TdxBufferedDocumentCharacterIterator); virtual; abstract;
    function CalculateMatch(AIterator: TdxBufferedDocumentCharacterIterator; const ARegEx: TRegex; var AConditions: TdxSearchConditions): TMatch; virtual; abstract;
    function GetMatchAbsolutePosition(const AMatch: TMatch): Integer; virtual; abstract;

    property SearchInfo: TdxRegexSearchInfos read FSearchInfo write FSearchInfo;
    property Regex: TRegex read FRegex;
    property EndPosition: TdxDocumentLogPosition read FEndPosition;
    property StartPosition: TdxDocumentLogPosition read FStartPosition;
  public
    constructor Create(APieceTable: TdxPieceTable; const ARegex: TRegex); overload;
    constructor Create(APieceTable: TdxPieceTable; const ARegex: TRegex; AMaxGuaranteedSearchResultLength: Integer); overload;
    constructor Create(APieceTable: TdxPieceTable; const ARegex: TRegex; ABufferSize, ABufferShiftSize: Integer); overload;
    destructor Destroy; override;
    function Match(AStart, AEnd: TdxDocumentLogPosition): TdxBufferedRegexSearchResult;
    procedure Initialize(AStart, AEnd: TdxDocumentLogPosition; const ACurrentPosition: TdxDocumentModelPosition);
    function Matches(AStart, AEnd: TdxDocumentLogPosition): TdxBufferedRegexSearchResultCollection;

    property PieceTable: TdxPieceTable read FPieceTable;
    property BufferShiftSize: Integer read FBufferShiftSize;
    property BufferSize: Integer read FBufferSize;
    property Buffer: TStringBuilder read FBuffer;
    property BufferStartPosition: TdxDocumentModelPosition read FBufferStartPosition write FBufferStartPosition;
    property BufferInputPosition: TdxDocumentModelPosition read FCurrentInputPosition write FCurrentInputPosition;
    property BufferOffset: Integer read FBufferOffset write FBufferOffset;
  end;

  { TdxBufferedRegexSearchForward }

  TdxBufferedRegexSearchForward = class(TdxBufferedRegexSearchBase)
  protected
    function CalculateMatchCore(const ARegEx: TRegex; const AConditions: TdxSearchConditions): TMatch; override;
    procedure CalculateSearchConditions(const APrevMatch: TMatch; var AConditions: TdxSearchConditions); override;
    procedure AppendBuffer(AIterator: TdxBufferedDocumentCharacterIterator); override;
    procedure ShiftBuffer(AIterator: TdxBufferedDocumentCharacterIterator; ADelta: Integer); override;
    function CalculateMatch(AIterator: TdxBufferedDocumentCharacterIterator; const ARegEx: TRegex;
      var AConditions: TdxSearchConditions): TMatch; override;
    function GetMatchAbsolutePosition(const AMatch: TMatch): Integer; override;
    function CreateIterator(const APosition: TdxDocumentModelPosition): TdxBufferedDocumentCharacterIterator; override;
    function CalculateStartPosition: TdxDocumentModelPosition; override;
    function CanMoveNext: Boolean; override;
    function CalculateBufferShiftSize(AResult: TdxBufferedRegexSearchResult): Integer; override;
  end;

  { TdxBufferedRegexSearchBackward }

  TdxBufferedRegexSearchBackward = class(TdxBufferedRegexSearchBase)
  protected
    procedure CalculateSearchConditions(const APrevMatch: TMatch; var AConditions: TdxSearchConditions); override;
    procedure AppendBuffer(AIterator: TdxBufferedDocumentCharacterIterator); override;
    function CalculateMatch(AIterator: TdxBufferedDocumentCharacterIterator; const ARegEx: TRegex;
      var AConditions: TdxSearchConditions): TMatch; override;
    function GetMatchAbsolutePosition(const AMatch: TMatch): Integer; override;
    function CreateIterator(const APosition: TdxDocumentModelPosition): TdxBufferedDocumentCharacterIterator; override;
    function CalculateStartPosition: TdxDocumentModelPosition; override;
    function CanMoveNext: Boolean; override;
    function CalculateBufferShiftSize(AResult: TdxBufferedRegexSearchResult): Integer; override;
    function CalculateMatchCore(const ARegEx: TRegEx; const AConditions: TdxSearchConditions): TMatch; override;
  end;

  { TdxTextSearchStrategyBase }

  TdxTextSearchStrategyBase<T: class> = class abstract(TdxSearchStrategy)
  strict private
    FPieceTable: TdxPieceTable;
  protected
    function PreparePattern(const APattern: string): string; virtual;
    function MatchCore(ASearcher: T; const AStart, AEnd: TdxDocumentLogPosition): TdxSearchResult; virtual; abstract;
    function CreateIntervalCalculator: TdxSearchIntervalCalculator; virtual; abstract;
    function CreateSearcher(const APattern: string; AOptions: TdxSearchOptions): T; virtual; abstract;
    function IsWordWhole(ARunInfo: TdxRunInfo): Boolean; virtual;
    function GetPrevCharacter(const AStart: TdxDocumentModelPosition): Char; virtual;
    function GetNextCharacter(const AEnd: TdxDocumentModelPosition): Char; virtual;
  public
    constructor Create(APieceTable: TdxPieceTable);
    function Match(const APattern: string; AOptions: TdxSearchOptions; const AStart, AEnd: TdxDocumentLogPosition): TdxSearchResult; override;

    property PieceTable: TdxPieceTable read FPieceTable;
  end;

  { TdxTextSearchByStringStrategy }

  TdxTextSearchByStringStrategy = class abstract(TdxTextSearchStrategyBase<TdxBMTextSearchBase>)
  protected
    function MatchCore(ASearcher: TdxBMTextSearchBase; const AStart, AEnd: TdxDocumentLogPosition): TdxSearchResult; override;
  end;

  { TdxTextSearchByStringForwardStrategy }

  TdxTextSearchByStringForwardStrategy = class(TdxTextSearchByStringStrategy)
  protected
    function CreateIntervalCalculator: TdxSearchIntervalCalculator; override;
    function CreateSearcher(const APattern: string; AOptions: TdxSearchOptions): TdxBMTextSearchBase; override;
  end;

  { TdxTextSearchByStringBackwardStrategy }

  TdxTextSearchByStringBackwardStrategy = class(TdxTextSearchByStringStrategy)
  protected
    function CreateIntervalCalculator: TdxSearchIntervalCalculator; override;
    function CreateSearcher(const APattern: string; AOptions: TdxSearchOptions): TdxBMTextSearchBase; override;
  end;

  { TdxTextSearchByRegexStrategy }

  TdxTextSearchByRegexStrategy = class abstract(TdxTextSearchStrategyBase<TdxBufferedRegexSearchBase>)
  protected
    function MatchCore(ASearcher: TdxBufferedRegexSearchBase; const AStart, AEnd: TdxDocumentLogPosition): TdxSearchResult; override;
  end;

  { TdxTextSearchByRegexForwardStrategy }

  TdxTextSearchByRegexForwardStrategy = class(TdxTextSearchByRegexStrategy)
  protected
    function CreateIntervalCalculator: TdxSearchIntervalCalculator; override;
    function CreateSearcher(const APattern: string; AOptions: TdxSearchOptions): TdxBufferedRegexSearchBase; override;
  end;

  { TdxTextSearchByRegexBackwardStrategy }

  TdxTextSearchByRegexBackwardStrategy = class(TdxTextSearchByRegexStrategy)
  protected
    function CreateIntervalCalculator: TdxSearchIntervalCalculator; override;
    function CreateSearcher(const APattern: string; AOptions: TdxSearchOptions): TdxBufferedRegexSearchBase; override;
  end;

  EIncorrectRegularExpressionException = class(Exception);

  { TdxNativeSearchResultBase }

  TdxNativeSearchResultBase = class(TInterfacedObject, IdxRichEditSearchResult)
  strict private
    FCurrentResult: IdxRichEditDocumentRange;
    FDocument: TdxNativeSubDocument;
    FEndPosition: TdxDocumentLogPosition;
    FFindText: string;
    FOptions: TdxRichEditSearchOptions;
    FRange: IdxRichEditDocumentRange;
    FStartPosition: TdxDocumentLogPosition;
    FTextSearch: TdxSearchStrategy;
    function GetCurrentResult: IdxRichEditDocumentRange;
  protected
    procedure SetInitialState;
    function CreateDocumentRange(ARangeInfo: TdxRunInfo): IdxRichEditDocumentRange; virtual;

    function CreateTextSearch: TdxSearchStrategy; virtual; abstract;
    procedure ChangeSearchInterval(ARangeInfo: TdxRunInfo); virtual; abstract;
    procedure ChangeSearchIntervalAfterReplace(ADeltaLength: Integer); virtual; abstract;
    procedure UpdateSearchInterval; virtual; abstract;

    property StartPosition: TdxDocumentLogPosition read FStartPosition write FStartPosition;
    property EndPosition: TdxDocumentLogPosition read FEndPosition write FEndPosition;
    property Range: IdxRichEditDocumentRange read FRange;
    property FindText: string read FFindText;
    property Document: TdxNativeSubDocument read FDocument;
  public
    constructor Create(ADocument: TdxNativeSubDocument;
      const AFindText: string; ASearchOptions: TdxRichEditSearchOptions;
      const ARange: IdxRichEditDocumentRange);
    destructor Destroy; override;

    function FindNext: Boolean; virtual;
    procedure Replace(const AReplaceWith: string); virtual;
    procedure Reset;

    property CurrentResult: IdxRichEditDocumentRange read FCurrentResult;
  end;

  { TdxNativeSearchResultForward }

  TdxNativeSearchResultForward = class(TdxNativeSearchResultBase)
  protected
    function CreateTextSearch: TdxSearchStrategy; override;
    procedure ChangeSearchInterval(ARangeInfo: TdxRunInfo); override;
    procedure ChangeSearchIntervalAfterReplace(ADeltaLength: Integer); override;
    procedure UpdateSearchInterval; override;
  end;

  { TdxNativeSearchResultBackward }

  TdxNativeSearchResultBackward = class(TdxNativeSearchResultBase)
  protected
    function CreateTextSearch: TdxSearchStrategy; override;
    procedure ChangeSearchInterval(ARangeInfo: TdxRunInfo); override;
    procedure ChangeSearchIntervalAfterReplace(ADeltaLength: Integer); override;
    procedure UpdateSearchInterval; override;
  end;

  TdxNativeMatch = class;

  { TdxNativeGroup }

  TdxNativeGroup = class(TInterfacedObject, IdxRichEditRegexSearchGroup)
  strict private
    FDocument: TdxNativeSubDocument;
    FPosition: TdxDocumentLogPosition;
    FGroup: TGroup;
    FRange: IdxRichEditDocumentRange;
    function GetLength: Integer;
    function GetPosition: IdxRichEditDocumentPosition;
    function GetValue: string;
  protected
    property Document: TdxNativeSubDocument read FDocument;
    property LogPosition: TdxDocumentLogPosition read FPosition;
  public
    constructor Create(ADocument: TdxNativeSubDocument;
      APosition: TdxDocumentLogPosition; const AGroup: TGroup);
    function GetRange: IdxRichEditDocumentRange;
    function ToString: string; override;

    property Position: IdxRichEditDocumentPosition read GetPosition;
    property Length: Integer read GetLength;
    property Value: string read GetValue;
  end;

  { TdxNativeGroupCollection }

  TdxNativeGroupCollection = class(TInterfacedObject, IdxRichEditRegexSearchGroupCollection)
  strict private
    FMatch: TdxNativeMatch;
    function GetCount: Integer;
    function GetItem(Index: Integer): IdxRichEditRegexSearchGroup;
  public
    constructor Create(const AMatch: TdxNativeMatch);
    property Items[Index: Integer]: IdxRichEditRegexSearchGroup read GetItem; default;
    property Count: Integer read GetCount;
  end;

  { TdxNativeMatch }

  TdxNativeMatch = class(TInterfacedObject, IdxRichEditRegexSearchMatch)
  strict private
    FDocument: TdxNativeSubDocument;
    FGroups: IdxRichEditRegexSearchGroupCollection;
    FLogPosition: TdxDocumentLogPosition;
    FMatch: TMatch;
    FOffset: TdxDocumentModelPosition;
    FRange: IdxRichEditDocumentRange;
    function GetLength: Integer;
    function GetPosition: IdxRichEditDocumentPosition;
    function GetRange: IdxRichEditDocumentRange;
    function GetValue: string;
  protected
    function CreateGroup(const AGroup: TGroup): IdxRichEditRegexSearchGroup; virtual;
    function GetGroups: IdxRichEditRegexSearchGroupCollection;

    property Length: Integer read GetLength;
    property Match: TMatch read FMatch;
    property Position: IdxRichEditDocumentPosition read GetPosition;
  public
    constructor Create(ADocument: TdxNativeSubDocument;
      APosition: TdxDocumentLogPosition; const AOffset: TdxDocumentModelPosition; const AMatch: TMatch);

    function Result(const AReplacement: string): string;
  end;

  { TdxNativeRegexSearch }

  TdxNativeRegexSearch = class
  strict private
    FDocument: TdxNativeSubDocument;
    FRegex: TRegEx;
    FMaxGuaranteedSearchResultLength: Integer;
  protected
    function Match(const ARange: IdxRichEditDocumentRange): IdxRichEditRegexSearchMatch; virtual;
    function Matches(const ARange: IdxRichEditDocumentRange): TArray<IdxRichEditDocumentRange>; virtual;
    function NextMatch(const AMatch: IdxRichEditRegexSearchMatch;
      const ARange: IdxRichEditDocumentRange): IdxRichEditRegexSearchMatch; virtual;

    class function CreatePosition(const APos: TdxDocumentModelPosition; AOffset: Integer): TdxDocumentLogPosition; virtual;
    function CreateMatch(const AOffset: TdxDocumentModelPosition; const AMatch: TMatch): IdxRichEditRegexSearchMatch; virtual;
    function CreateRegexSearch: TdxBufferedRegexSearchForward; virtual;

    property Document: TdxNativeSubDocument read FDocument;
    property Regex: TRegEx read FRegex;
    property MaxGuaranteedSearchResultLength: Integer read FMaxGuaranteedSearchResultLength;
  public
    constructor Create(ADocument: TdxNativeSubDocument;
      const ARegex: TRegEx; AMaxGuaranteedSearchResultLength: Integer);
  end;

  { TdxNativeRegexSearchResult }

  TdxNativeRegexSearchResult = class abstract(TInterfacedObject, IdxRichEditRegexSearchResult)
  strict private
    FRegexSearch: TdxNativeRegexSearch;
    FDocument: TdxNativeSubDocument;
    FRange: IdxRichEditDocumentRange;
    FMaxGuaranteedSearchResultLength: Integer;
    FMatch: IdxRichEditRegexSearchMatch;
    function GetCurrentResult: IdxRichEditDocumentRange;
    function GetMatch: IdxRichEditRegexSearchMatch;
  protected
    function CreateRegexSearch(ADocument: TdxNativeSubDocument;
      const ARegex: TRegEx): TdxNativeRegexSearch; virtual;

    property MaxGuaranteedSearchResultLength: Integer read FMaxGuaranteedSearchResultLength;
  public
    constructor Create(ADocument: TdxNativeSubDocument;
      const ARegex: TRegEx; const ARange: IdxRichEditDocumentRange; AMaxGuaranteedSearchResultLength: Integer);
    destructor Destroy; override;
    procedure Reset;
    function FindNext: Boolean;
    procedure Replace(const AReplaceWith: string);

    property Match: IdxRichEditRegexSearchMatch read GetMatch;
    property CurrentResult: IdxRichEditDocumentRange read GetCurrentResult;
  end;

implementation

uses
  RTLConsts, Contnrs, RegularExpressionsCore, Math, Character, Dialogs,
  dxRichEdit.Utils.Exceptions,
  dxCharacters,
  dxStringHelper,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.ParagraphRange;

type
{$IFNDEF DELPHI102TOKYO}
  TStringBuilderHelper = class helper for TStringBuilder
    function Insert(AIndex: Integer; const AValue: TCharArray; AStartIndex, ACharCount: Integer): TStringBuilder; overload;
  end;
{$ENDIF}

  TdxMathHelper = record helper for TMatch
    function Index: Integer;
{$IFNDEF DELPHIBERLIN}
    function Length: Integer;
{$ENDIF}
  end;

{$IFNDEF DELPHIBERLIN}

  TdxGroupHelper = record helper for TGroup
    function InternalIndex: Integer;
    function InternalLength: Integer;
  end;

  TRegExHelper = record helper for TRegex
    procedure SetState(AState: TPerlRegExState);
  end;
{$ENDIF}

{$IFNDEF DELPHI102TOKYO}
{ TStringBuilderHelper }

function TStringBuilderHelper.Insert(AIndex: Integer; const AValue: TCharArray; AStartIndex,
  ACharCount: Integer): TStringBuilder;
begin
  if AIndex - 1 >= Length then
    raise ERangeError.CreateResFmt(@SListIndexError, [AIndex])
  else if AIndex < 0 then
    raise ERangeError.CreateResFmt(@SListIndexError, [AIndex]);
  if AStartIndex < 0 then
    raise ERangeError.CreateResFmt(@SParamIsNegative, ['AStartIndex']); // DO NOT LOCALIZE
  if ACharCount < 0 then
    raise ERangeError.CreateResFmt(@SParamIsNegative, ['ACharCount']); // DO NOT LOCALIZE
  if AStartIndex + ACharCount > System.Length(AValue) then
    raise ERangeError.CreateResFmt(@SInputBufferExceed,
      ['StartIndex', AStartIndex, 'ACharCount', ACharCount]);
  Length := Length + ACharCount;
  Move(FData[AIndex], FData[AIndex + ACharCount], (Length - ACharCount - AIndex) * SizeOf(Char));
  Move(AValue[AStartIndex], FData[AIndex], ACharCount * SizeOf(Char));
  Result := Self;
end;
{$ENDIF}

{ TdxMathHelper }

function TdxMathHelper.Index: Integer;
begin
{$IFNDEF DELPHIBERLIN}
  Result := Self.FGroup.InternalIndex;
{$ELSE}
  Result := inherited Index - 1;
{$ENDIF}
end;

{$IFNDEF DELPHIBERLIN}
function TdxMathHelper.Length: Integer;
begin
  Result := Self.FGroup.InternalLength;
end;

{ TdxGroupHelper }

function TdxGroupHelper.InternalIndex: Integer;
begin
  if Self.FIndex > System.Length(Self.FValue) then
    Result := Self.FIndex
  else
    Result := Self.Index - 1;
end;

function TdxGroupHelper.InternalLength: Integer;
begin
  if Self.FIndex > System.Length(Self.FValue) then
    Result := 0
  else
    Result := Self.Length;
end;

{ TRegExHelper }

procedure TRegExHelper.SetState(AState: TPerlRegExState);
begin
  Self.FRegEx.State := AState;
end;
{$ENDIF}

{ TdxSearchResult }

constructor TdxSearchResult.Create(AValue: TdxRunInfo);
begin
  inherited Create;
  FValue := AValue;
end;

procedure TdxSearchResult.Clear;
begin
  FreeAndNil(FValue);
end;

function TdxSearchResult.GetSuccess: Boolean;
begin
  Result := FValue <> nil;
end;

{ TdxBufferedDocumentCharacterIterator }

constructor TdxBufferedDocumentCharacterIterator.Create(const AStartPosition: TdxDocumentModelPosition);
begin
  inherited Create;
  FStartPosition := AStartPosition;
  FEndPosition := AStartPosition;
  FBuffer := TStringBuilder.Create;
end;

destructor TdxBufferedDocumentCharacterIterator.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

function TdxBufferedDocumentCharacterIterator.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(StartPosition.PieceTable);
end;

function TdxBufferedDocumentCharacterIterator.GetBufferLength: Integer;
begin
  Result := Buffer.Length - Offset;
end;

function TdxBufferedDocumentCharacterIterator.GetCharacters(Index: Integer): Char;
begin
  Result := Buffer[GetCharacterIndex(index)];
end;

procedure TdxBufferedDocumentCharacterIterator.AppendBuffer(AMinLength: Integer);
var
  ADelta: Integer;
begin
  ADelta := AMinLength - (FBuffer.Length - FOffset);
  if ADelta <= 0 then
    Exit;

  if Buffer.Length > 0 then
    FlushBuffer;
  AppendBufferCore(AMinLength);
  EnsureStartPositionIsVisible;
end;

procedure TdxBufferedDocumentCharacterIterator.EnsureStartPositionIsVisible;
begin
  if not IsRunVisible(StartPosition.RunIndex) then
    StartPosition.CopyFrom(GetPositionBackward(EndPosition, Buffer.Length));
end;

procedure TdxBufferedDocumentCharacterIterator.ShiftBuffer(ADelta: Integer);
begin
  Offset := Offset + ADelta;
  CurrentPosition := GetPosition(ADelta);
end;

function TdxBufferedDocumentCharacterIterator.IsRunVisible(ARunIndex: TdxRunIndex): Boolean;
begin
  Result := PieceTable.VisibleTextFilter.IsRunVisible(ARunIndex);
end;

function TdxBufferedDocumentCharacterIterator.GetPosition(AOffset: Integer): TdxDocumentModelPosition;
begin
  Result := GetPosition(CurrentPosition, AOffset);
end;

function TdxBufferedDocumentCharacterIterator.GetPositionForward(const APosition: TdxDocumentModelPosition; AOffset: Integer): TdxDocumentModelPosition;
var
  ARuns: TdxTextRunCollection;
  ARunIndex, ALastRunIndex: TdxRunIndex;
  APositionOffset, ATotalOffset: Integer;
  APos: TdxDocumentLogPosition;
  ARun: TdxTextRunBase;
begin
  ARuns := PieceTable.Runs;
  ARunIndex := APosition.RunIndex;
  APositionOffset := ARuns[ARunIndex].Length - APosition.RunOffset;
  ATotalOffset := 0;
  if IsRunVisible(APosition.RunIndex) then
  begin
    if AOffset < APositionOffset then
    begin
      Result := APosition;
      Result.LogPosition := Result.LogPosition + AOffset;
      Exit;
    end;
    ATotalOffset := APositionOffset;
  end;

  ALastRunIndex := TdxRunIndex(ARuns.Count - 1);
  APos := APosition.RunStartLogPosition + ARuns[ARunIndex].Length;

  while True do
  begin
    Inc(ARunIndex);
    if ARunIndex > ALastRunIndex then
      Break;

    ARun := ARuns[ARunIndex];
    if IsRunVisible(ARunIndex) then
    begin
      if AOffset < (ATotalOffset + ARun.Length) then
      begin
        Result := TdxDocumentModelPosition.Create(PieceTable);
        Result.LogPosition := APos + (AOffset - ATotalOffset);
        Result.RunStartLogPosition := APos;
        Result.RunIndex := ARunIndex;
        Result.ParagraphIndex := ARun.Paragraph.Index;
        Exit;
      end;
      Inc(ATotalOffset, ARun.Length);
    end;
    Inc(APos, ARun.Length);
  end;
  TdxRichEditExceptions.ThrowInternalException;
end;

function TdxBufferedDocumentCharacterIterator.GetPositionBackward(const APosition: TdxDocumentModelPosition;
  AOffset: Integer): TdxDocumentModelPosition;
var
  ATotalOffset: Integer;
  APos: TdxDocumentLogPosition;
  ARuns: TdxTextRunCollection;
  ARunIndex: TdxRunIndex;
  ARun: TdxTextRunBase;
begin
  ATotalOffset := 0;
  if IsRunVisible(APosition.RunIndex) then
  begin
    if AOffset <= APosition.RunOffset then
    begin
      Result := APosition;
      Result.LogPosition := Result.LogPosition - AOffset;
      Exit;
    end;
    ATotalOffset := APosition.RunOffset;
  end;

  APos := APosition.RunStartLogPosition;
  ARuns := PieceTable.Runs;
  ARunIndex := APosition.RunIndex;

  while True do
  begin
    Dec(ARunIndex);
    if ARunIndex < 0 then
      Break;

    ARun := ARuns[ARunIndex];
    if IsRunVisible(ARunIndex) then
    begin
      if AOffset <= (ATotalOffset + ARun.Length) then
      begin
        Result := TdxDocumentModelPosition.Create(PieceTable);
        Result.LogPosition := APos - (AOffset - ATotalOffset);
        Result.RunStartLogPosition := APos - ARun.Length;
        Result.RunIndex := ARunIndex;
        Result.ParagraphIndex := ARun.Paragraph.Index;
        Exit;
      end;
      Inc(ATotalOffset, ARun.Length);
    end;
    Dec(APos, ARun.Length);
  end;
  TdxRichEditExceptions.ThrowInternalException;
end;

function TdxBufferedDocumentCharacterIterator.GetRunText(ARun: TdxTextRunBase): string;
begin
  if ARun is TdxParagraphRun then
    Result := #13
  else
    Result := ARun.GetNonEmptyText(PieceTable.TextBuffer);
end;

function TdxBufferedDocumentCharacterIterator.Compare(AOffset: Integer; ACh: Char; AIgnoreCase: Boolean): Boolean;
var
  ADocumentChar: Char;
  APos: TdxDocumentModelPosition;
begin
  ADocumentChar := Self[AOffset];
  if Compare(ADocumentChar, ACh, AIgnoreCase) then
  begin
    if (ADocumentChar = TdxCharacters.ObjectMark) or (ADocumentChar = TdxCharacters.FloatingObjectMark) then
    begin
      APos := GetPosition(AOffset);
      if not (PieceTable.Runs[APos.RunIndex] is TdxTextRun) then
        Exit(False);
    end;
    Result := True;
  end
  else
    Result := False;
end;

function TdxBufferedDocumentCharacterIterator.Compare(ACh1: Char; ACh2: Char; AIgnoreCase: Boolean): Boolean;
begin
  if AIgnoreCase then
  {$IFDEF DELPHIXE4}
    Result := ACh1.ToLower = ACh2.ToLower
  {$ELSE}
    Result := TCharacter.ToLower(ACh1) = TCharacter.ToLower(ACh2)
  {$ENDIF}
  else
    Result := ACh1 = ACh2;
end;


{ TdxBufferedDocumentCharacterIteratorForward }

function TdxBufferedDocumentCharacterIteratorForward.GetCurrentPosition: TdxDocumentModelPosition;
begin
  Result := StartPosition;
end;

function TdxBufferedDocumentCharacterIteratorForward.GetCharacterIndex(AIndex: Integer): Integer;
begin
  Result := AIndex + Offset;
end;

procedure TdxBufferedDocumentCharacterIteratorForward.AppendBufferCore(AMinLength: Integer);
var
  ALastRunIndex: TdxRunIndex;
  ARuns: TdxTextRunCollection;
  ARun: TdxTextRunBase;
begin
  ALastRunIndex := TdxRunIndex(PieceTable.Runs.Count - 1);
  ARuns := PieceTable.Runs;
  while (Buffer.Length < AMinLength) and (EndPosition.RunIndex < ALastRunIndex) do
  begin
    ARun := ARuns[EndPosition.RunIndex];
    if IsRunVisible(EndPosition.RunIndex) then
    begin
      if EndPosition.RunOffset > 0 then
        Buffer.Append(TdxStringHelper.Substring(GetRunText(ARun), EndPosition.RunOffset))
      else
        Buffer.Append(GetRunText(ARun));
    end;
    EndPosition.LogPosition := EndPosition.LogPosition + ARun.Length - EndPosition.RunOffset;
    EndPosition.RunStartLogPosition := EndPosition.LogPosition;
    EndPosition.RunIndex := EndPosition.RunIndex + 1;
    EndPosition.ParagraphIndex := ARuns[EndPosition.RunIndex].Paragraph.Index;
  end;
end;

procedure TdxBufferedDocumentCharacterIteratorForward.FlushBuffer;
var
  ANewValue: string;
begin
  ANewValue := Buffer.ToString(Offset, Buffer.Length - Offset);
  Buffer.Length := 0;
  Buffer.Append(ANewValue);
  Offset := 0;
end;

function TdxBufferedDocumentCharacterIteratorForward.IsCharacterExist(AIndex: Integer): Boolean;
begin
  Result := GetCharacterIndex(AIndex) < Buffer.Length;
end;

procedure TdxBufferedDocumentCharacterIteratorForward.SetCurrentPosition(const Value: TdxDocumentModelPosition);
begin
  StartPosition.CopyFrom(Value);
end;

function TdxBufferedDocumentCharacterIteratorForward.GetPosition(const APosition: TdxDocumentModelPosition;
  AOffset: Integer): TdxDocumentModelPosition;
begin
  Result := GetPositionForward(APosition, AOffset);
end;

{ TdxBufferedDocumentCharacterIteratorBackward }

function TdxBufferedDocumentCharacterIteratorBackward.GetCurrentPosition: TdxDocumentModelPosition;
begin
  Result := EndPosition;
end;

function TdxBufferedDocumentCharacterIteratorBackward.GetCharacterIndex(AIndex: Integer): Integer;
begin
  Result := (Buffer.Length - 1) - (AIndex + Offset);
end;

procedure TdxBufferedDocumentCharacterIteratorBackward.AppendBufferCore(AMinLength: Integer);
var
  ARuns: TdxTextRunCollection;
  ALogPosition: TdxDocumentLogPosition;
  ARun: TdxTextRunBase;
  ARunIndex: TdxRunIndex;
begin
  ARuns := PieceTable.Runs;
  ALogPosition := StartPosition.LogPosition;
  if StartPosition.RunOffset > 0 then
  begin
    if IsRunVisible(StartPosition.RunIndex) then
    begin
      ARun := ARuns[StartPosition.RunIndex];
      Buffer.Insert(0, TdxStringHelper.Substring(GetRunText(ARun), 0, StartPosition.RunOffset));
    end;
    ALogPosition := StartPosition.RunStartLogPosition;
  end;
  ARunIndex := StartPosition.RunIndex;
  while (Buffer.Length < AMinLength) and (ARunIndex > 0) do
  begin
    Dec(ARunIndex);
    if IsRunVisible(ARunIndex) then
      Buffer.Insert(0, ARuns[ARunIndex].GetNonEmptyText(PieceTable.TextBuffer));
    Dec(ALogPosition, ARuns[ARunIndex].Length);
  end;
  StartPosition.LogPosition := ALogPosition;
  StartPosition.RunStartLogPosition := ALogPosition;
  StartPosition.RunIndex := ARunIndex;
  StartPosition.ParagraphIndex := ARuns[ARunIndex].Paragraph.Index;
end;

procedure TdxBufferedDocumentCharacterIteratorBackward.FlushBuffer;
begin
  Buffer.Length := Buffer.Length - Offset;
  Offset := 0;
end;

function TdxBufferedDocumentCharacterIteratorBackward.IsCharacterExist(AIndex: Integer): Boolean;
begin
  Result := GetCharacterIndex(AIndex) >= 0;
end;

procedure TdxBufferedDocumentCharacterIteratorBackward.SetCurrentPosition(const Value: TdxDocumentModelPosition);
begin
  EndPosition.CopyFrom(Value);
end;

function TdxBufferedDocumentCharacterIteratorBackward.GetPosition(const APosition: TdxDocumentModelPosition; AOffset: Integer): TdxDocumentModelPosition;
begin
  Result := GetPositionBackward(APosition, AOffset);
end;

{ TdxRegexSearchResult }

constructor TdxRegexSearchResult.Create(AResult: TdxRunInfo; AMatchInfo: TdxBufferedRegexSearchResult);
begin
  inherited Create(AResult);
  FMatchInfo := AMatchInfo;
end;

procedure TdxRegexSearchResult.Clear;
begin
  inherited Clear;
  FreeAndNil(FMatchInfo);
end;

{ TdxTextSearchStateBase }

constructor TdxTextSearchStateBase.Create(ASearchProvider: TdxTextSearchProvider);
begin
  inherited Create;
  Assert(Assigned(ASearchProvider), 'SearchProvider');
  FSearchProvider := ASearchProvider;
end;

function TdxTextSearchStateBase.GetDocumentModel: TdxDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

function TdxTextSearchStateBase.GetSearchParameters: TdxSearchParameters;
begin
  Result := SearchProvider.Parameters;
end;

function TdxTextSearchStateBase.GetSearchContext: TdxSearchContext;
begin
  Result := SearchProvider.Context;
end;

function TdxTextSearchStateBase.GetPieceTable: TdxPieceTable;
begin
  Result := SearchProvider.PieceTable;
end;

function TdxTextSearchStateBase.GetSearchLimitOffset: Integer;
begin
  if not SearchParameters.UseRegularExpression then
    Result := Length(SearchParameters.SearchString) - 1
  else
    Result := 0;
end;

function TdxTextSearchStateBase.FindNext(AStartPosition: TdxDocumentLogPosition): TdxSearchResult;
begin
  if not (ShouldSearch and CanSearch(AStartPosition)) then
    Result := TdxSearchResult.Create
  else
    Result := DoSearch(AStartPosition);
end;

function TdxTextSearchStateBase.DoSearch(AStart, AEnd: TdxDocumentLogPosition): TdxSearchResult;
begin
  Result := SearchProvider.FindRunInfo(AStart, AEnd);
end;

function TdxTextSearchStateBase.CanSearch(AStartPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := SearchContext.StartOfIntervalSearch or CanSearchFromPosition(AStartPosition);
end;

{ TdxFindStartState }

function TdxFindStartState.GetSearchScope: TdxSearchScope;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := TdxSearchScope.None;
end;

function TdxFindStartState.GetType: TdxSearchState;
begin
  Result := TdxSearchState.FindStart;
end;

function TdxFindStartState.GetShouldSearch: Boolean;
begin
  Result := True;
end;

function TdxFindStartState.FindNext(AStartPosition: TdxDocumentLogPosition): TdxSearchResult;
begin
  if SearchParameters.FindInSelection then
    SearchProvider.ChangeState(TdxSearchState.FindInSelection)
  else
    SearchProvider.ChangeState(TdxSearchState.FindAfterCaret);

  Result := SearchProvider.State.FindNext(AStartPosition);
end;

function TdxFindStartState.GetNextStateType: TdxSearchState;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := Default(TdxSearchState);
end;

function TdxFindStartState.DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := nil;
end;

function TdxFindStartState.CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := True;
end;

{ TdxFindInSelectionForwardState }

function TdxFindInSelectionForwardState.GetSearchScope: TdxSearchScope;
begin
  Result := TdxSearchScope.SelectedText;
end;

function TdxFindInSelectionForwardState.GetType: TdxSearchState;
begin
  Result := TdxSearchState.FindInSelection;
end;

function TdxFindInSelectionForwardState.GetShouldSearch: Boolean;
begin
  Result := SearchContext.EndSelection > SearchContext.StartSelection;
end;

function TdxFindInSelectionForwardState.GetNextStateType: TdxSearchState;
begin
  Result := TdxSearchState.FindAfterSelection;
end;

function TdxFindInSelectionForwardState.DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult;
begin
  if SearchContext.StartOfIntervalSearch then
    Result := DoSearch(SearchContext.StartSelection, SearchContext.EndSelection)
  else
    Result := DoSearch(AStartPosition, SearchContext.EndSelection);
end;

function TdxFindInSelectionForwardState.CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := AStartPosition < SearchContext.EndSelection;
end;

{ TdxFindAfterCaretForwardState }

function TdxFindAfterCaretForwardState.GetSearchScope: TdxSearchScope;
begin
  Result := TdxSearchScope.BelowSelectedText;
end;

function TdxFindAfterCaretForwardState.GetType: TdxSearchState;
begin
  Result := TdxSearchState.FindAfterCaret;
end;

function TdxFindAfterCaretForwardState.GetShouldSearch: Boolean;
begin
  Result := SearchContext.EndSelection < PieceTable.DocumentEndLogPosition;
end;

function TdxFindAfterCaretForwardState.GetNextStateType: TdxSearchState;
begin
  if SearchContext.EndSelection = PieceTable.DocumentStartLogPosition then
    Result := TdxSearchState.FindFinish
  else
    Result := TdxSearchState.FindBeforeCaret;
end;

function TdxFindAfterCaretForwardState.DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult;
begin
  if SearchContext.StartOfIntervalSearch then
    Result := DoSearch(SearchContext.EndSelection, PieceTable.DocumentEndLogPosition)
  else
    Result := DoSearch(AStartPosition, PieceTable.DocumentEndLogPosition);
end;

function TdxFindAfterCaretForwardState.CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := AStartPosition < PieceTable.DocumentEndLogPosition;
end;

{ TdxFindBeforeCaretForwardState }

function TdxFindBeforeCaretForwardState.GetSearchScope: TdxSearchScope;
begin
  Result := TdxSearchScope.AboveSelectedText;
end;

function TdxFindBeforeCaretForwardState.GetType: TdxSearchState;
begin
  Result := TdxSearchState.FindBeforeCaret;
end;

function TdxFindBeforeCaretForwardState.GetShouldSearch: Boolean;
begin
  Result := SearchContext.EndSelection > PieceTable.DocumentStartLogPosition;
end;

function TdxFindBeforeCaretForwardState.GetNextStateType: TdxSearchState;
begin
  Result := TdxSearchState.FindFinish;
end;

function TdxFindBeforeCaretForwardState.DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult;
var
  AEndPosition: TdxDocumentLogPosition;
begin
  AEndPosition := Min(SearchContext.EndSelection + SearchLimitOffset, PieceTable.DocumentEndLogPosition);
  if SearchContext.StartOfIntervalSearch then
    Result := DoSearch(PieceTable.DocumentStartLogPosition, AEndPosition)
  else
    Result := DoSearch(AStartPosition, AEndPosition);
end;

function TdxFindBeforeCaretForwardState.CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := AStartPosition < SearchContext.EndSelection;
end;

{ TdxFindAfterSelectionForwardState }

function TdxFindAfterSelectionForwardState.GetSearchScope: TdxSearchScope;
begin
  Result := TdxSearchScope.BelowSelectedText;
end;

function TdxFindAfterSelectionForwardState.GetType: TdxSearchState;
begin
  Result := TdxSearchState.FindAfterSelection;
end;

function TdxFindAfterSelectionForwardState.GetShouldSearch: Boolean;
begin
  Result := SearchContext.EndSelection < PieceTable.DocumentEndLogPosition;
end;

function TdxFindAfterSelectionForwardState.GetNextStateType: TdxSearchState;
begin
  if SearchContext.StartSelection = PieceTable.DocumentStartLogPosition then
    Result := TdxSearchState.FindFinish
  else
    Result := TdxSearchState.FindBeforeSelection;
end;

function TdxFindAfterSelectionForwardState.DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult;
var
  AStartPos: TdxDocumentLogPosition;
begin
  if SearchContext.StartOfIntervalSearch then
  begin
    AStartPos := Max(PieceTable.DocumentStartLogPosition, SearchContext.EndSelection - SearchLimitOffset);
    Result := DoSearch(AStartPos, PieceTable.DocumentEndLogPosition);
  end
  else
    Result := DoSearch(AStartPosition, PieceTable.DocumentEndLogPosition);
end;

function TdxFindAfterSelectionForwardState.CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := AStartPosition <= PieceTable.DocumentEndLogPosition;
end;

{ TdxFindBeforeSelectionForwardState }

function TdxFindBeforeSelectionForwardState.GetSearchScope: TdxSearchScope;
begin
  Result := TdxSearchScope.AboveSelectedText;
end;

function TdxFindBeforeSelectionForwardState.GetType: TdxSearchState;
begin
  Result := TdxSearchState.FindBeforeSelection;
end;

function TdxFindBeforeSelectionForwardState.GetShouldSearch: Boolean;
begin
  Result := SearchContext.StartSelection > PieceTable.DocumentStartLogPosition;
end;

function TdxFindBeforeSelectionForwardState.GetNextStateType: TdxSearchState;
begin
  Result := TdxSearchState.FindFinish;
end;

function TdxFindBeforeSelectionForwardState.DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult;
var
  AEndPosition: TdxDocumentLogPosition;
begin
  AEndPosition := Min(SearchContext.StartSelection + SearchLimitOffset, PieceTable.DocumentEndLogPosition);
  if SearchContext.StartOfIntervalSearch then
    Result := DoSearch(PieceTable.DocumentStartLogPosition, AEndPosition)
  else
    Result := DoSearch(AStartPosition, AEndPosition);
end;

function TdxFindBeforeSelectionForwardState.CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := AStartPosition < SearchContext.StartSelection;
end;

{ TdxFindFinishState }

function TdxFindFinishState.GetSearchScope: TdxSearchScope;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := Default(TdxSearchScope);
end;

function TdxFindFinishState.GetType: TdxSearchState;
begin
  Result := TdxSearchState.FindFinish;
end;

function TdxFindFinishState.GetShouldSearch: Boolean;
begin
  Result := False;
end;

function TdxFindFinishState.GetNextStateType: TdxSearchState;
begin
  Result := TdxSearchState.FindStart;
end;

function TdxFindFinishState.DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := nil;
end;

function TdxFindFinishState.CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := False;
end;

{ TdxFindInSelectionBackwardState }

function TdxFindInSelectionBackwardState.GetSearchScope: TdxSearchScope;
begin
  Result := TdxSearchScope.SelectedText;
end;

function TdxFindInSelectionBackwardState.GetType: TdxSearchState;
begin
  Result := TdxSearchState.FindInSelection;
end;

function TdxFindInSelectionBackwardState.GetShouldSearch: Boolean;
begin
  Result := SearchContext.EndSelection > SearchContext.StartSelection;
end;

function TdxFindInSelectionBackwardState.GetNextStateType: TdxSearchState;
begin
  Result := TdxSearchState.FindAfterSelection;
end;

function TdxFindInSelectionBackwardState.DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult;
begin
  if SearchContext.StartOfIntervalSearch then
    Result := DoSearch(SearchContext.StartSelection, SearchContext.EndSelection)
  else
    Result := DoSearch(SearchContext.StartSelection, AStartPosition);
end;

function TdxFindInSelectionBackwardState.CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := AStartPosition > SearchContext.StartSelection;
end;

{ TdxFindAfterCaretBackwardState }

function TdxFindAfterCaretBackwardState.GetSearchScope: TdxSearchScope;
begin
  Result := TdxSearchScope.AboveSelectedText;
end;

function TdxFindAfterCaretBackwardState.GetType: TdxSearchState;
begin
  Result := TdxSearchState.FindAfterCaret;
end;

function TdxFindAfterCaretBackwardState.GetShouldSearch: Boolean;
begin
  Result := SearchContext.StartSelection > PieceTable.DocumentStartLogPosition;
end;

function TdxFindAfterCaretBackwardState.GetNextStateType: TdxSearchState;
begin
  if SearchContext.StartSelection = PieceTable.DocumentEndLogPosition then
    Result := TdxSearchState.FindFinish
  else
    Result := TdxSearchState.FindBeforeCaret;
end;

function TdxFindAfterCaretBackwardState.DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult;
begin
  if SearchContext.StartOfIntervalSearch then
    Result := DoSearch(PieceTable.DocumentStartLogPosition, SearchContext.StartSelection)
  else
    Result := DoSearch(PieceTable.DocumentStartLogPosition, AStartPosition);
end;

function TdxFindAfterCaretBackwardState.CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := AStartPosition >= PieceTable.DocumentStartLogPosition;
end;

{ TdxFindBeforeCaretBackwardState }

function TdxFindBeforeCaretBackwardState.GetSearchScope: TdxSearchScope;
begin
  Result := TdxSearchScope.BelowSelectedText;
end;

function TdxFindBeforeCaretBackwardState.GetType: TdxSearchState;
begin
  Result := TdxSearchState.FindBeforeCaret;
end;

function TdxFindBeforeCaretBackwardState.GetShouldSearch: Boolean;
begin
  Result := SearchContext.StartSelection < PieceTable.DocumentEndLogPosition;
end;

function TdxFindBeforeCaretBackwardState.GetNextStateType: TdxSearchState;
begin
  Result := TdxSearchState.FindFinish;
end;

function TdxFindBeforeCaretBackwardState.DoSearch(AEndPosition: TdxDocumentLogPosition): TdxSearchResult;
var
  AStartPos: TdxDocumentLogPosition;
begin
  AStartPos := Max(SearchContext.StartSelection - SearchLimitOffset, PieceTable.DocumentStartLogPosition);
  if SearchContext.StartOfIntervalSearch then
    Result := DoSearch(AStartPos, PieceTable.DocumentEndLogPosition)
  else
    Result := DoSearch(AStartPos, AEndPosition);
end;

function TdxFindBeforeCaretBackwardState.CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := AStartPosition >= SearchContext.StartSelection;
end;

{ TdxFindAfterSelectionBackwardState }

function TdxFindAfterSelectionBackwardState.GetSearchScope: TdxSearchScope;
begin
  Result := TdxSearchScope.AboveSelectedText;
end;

function TdxFindAfterSelectionBackwardState.GetType: TdxSearchState;
begin
  Result := TdxSearchState.FindAfterSelection;
end;

function TdxFindAfterSelectionBackwardState.GetShouldSearch: Boolean;
begin
  Result := SearchContext.StartSelection > PieceTable.DocumentStartLogPosition;
end;

function TdxFindAfterSelectionBackwardState.GetNextStateType: TdxSearchState;
begin
  if SearchContext.EndSelection = PieceTable.DocumentEndLogPosition then
    Result := TdxSearchState.FindFinish
  else
    Result := TdxSearchState.FindBeforeSelection;
end;

function TdxFindAfterSelectionBackwardState.DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult;
var
  AEndPos: TdxDocumentLogPosition;
begin
  if SearchContext.StartOfIntervalSearch then
  begin
    AEndPos := Min(SearchContext.StartSelection + SearchLimitOffset, PieceTable.DocumentEndLogPosition);
    Result := DoSearch(PieceTable.DocumentStartLogPosition, AEndPos)
  end
  else
    Result := DoSearch(PieceTable.DocumentStartLogPosition, AStartPosition);
end;

function TdxFindAfterSelectionBackwardState.CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := AStartPosition >= PieceTable.DocumentStartLogPosition;
end;

{ TdxFindBeforeSelectionBackwardState }

function TdxFindBeforeSelectionBackwardState.GetSearchScope: TdxSearchScope;
begin
  Result := TdxSearchScope.BelowSelectedText;
end;

function TdxFindBeforeSelectionBackwardState.GetType: TdxSearchState;
begin
  Result := TdxSearchState.FindBeforeSelection;
end;

function TdxFindBeforeSelectionBackwardState.GetShouldSearch: Boolean;
begin
  Result := SearchContext.EndSelection < PieceTable.DocumentEndLogPosition;
end;

function TdxFindBeforeSelectionBackwardState.GetNextStateType: TdxSearchState;
begin
  Result := TdxSearchState.FindFinish;
end;

function TdxFindBeforeSelectionBackwardState.DoSearch(AStartPosition: TdxDocumentLogPosition): TdxSearchResult;
var
  AStartPos: TdxDocumentLogPosition;
begin
  AStartPos := Max(PieceTable.DocumentStartLogPosition, SearchContext.EndSelection - SearchLimitOffset);
  if SearchContext.StartOfIntervalSearch then
    Result := DoSearch(AStartPos, PieceTable.DocumentEndLogPosition)
  else
    Result := DoSearch(AStartPos, AStartPosition);
end;

function TdxFindBeforeSelectionBackwardState.CanSearchFromPosition(AStartPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := AStartPosition >= SearchContext.EndSelection;
end;

{ TdxTextSearchProvider }

constructor TdxTextSearchProvider.Create(APieceTable: TdxPieceTable; AParameters: TdxSearchParameters; AContext: TdxSearchContext);
begin
  inherited Create;
  Assert(Assigned(APieceTable), 'pieceTable');
  Assert(Assigned(AParameters), 'parameters');
  Assert(Assigned(AContext), 'context');
  FPieceTable := APieceTable;
  FParameters := AParameters;
  FContext := AContext;

  SetState(AContext.SearchState);
end;

function TdxTextSearchProvider.FindNextPosition: TdxRunInfo;
var
  ASearchResult: TdxSearchResult;
  AState: IdxTextSearchStateBase;
begin
  if Context.SearchState = TdxSearchState.FindFinish then
    Exit(nil);

  AState := State;
  ASearchResult := AState.FindNext(Context.StartAt);
  try
    if ASearchResult.Success then
    begin
      Context.StartOfIntervalSearch := False;
      Context.StartAt := GetStartAtPosition(ASearchResult.Value);
      Context.MatchCount := Context.MatchCount + 1;
      if ASearchResult is TdxRegexSearchResult then
        Context.MatchInfo := TdxRegexSearchResult(ASearchResult).MatchInfo
      else
        Context.MatchInfo := nil;
    end
    else
    begin
      Context.StartOfIntervalSearch := True;
      Context.SearchState := State.GetNextStateType;
    end;
    Context.SearchScope := State.SearchScope;
    Result := ASearchResult.Value;
  finally
    ASearchResult.Free;
  end;
end;

function TdxTextSearchProvider.FindRunInfo(AStart, AEnd: TdxDocumentLogPosition): TdxSearchResult;
var
  ASearchStrategy: TdxSearchStrategy;
begin
  ASearchStrategy := CreateSearchStrategy;
  try
    Result := ASearchStrategy.Match(Parameters.SearchString, Parameters.SearchOptions, AStart, AEnd);
  finally
    ASearchStrategy.Free;
  end;
end;

function TdxTextSearchProvider.CreateSearchStrategy: TdxSearchStrategy;
begin
  if Parameters.UseRegularExpression then
    Result := CreateRegexSearch
  else
    Result := CreateStringSearch;
end;

procedure TdxTextSearchProvider.ChangeState(ASearchType: TdxSearchState);
begin
  SetState(ASearchType);
  Context.SearchState := ASearchType;
end;

{ TdxTextSearchForwardProvider }

procedure TdxTextSearchForwardProvider.SetState(ASearchType: TdxSearchState);
begin
  case ASearchType of
    TdxSearchState.FindStart:
      State := TdxFindStartState.Create(Self);
    TdxSearchState.FindInSelection:
      State := TdxFindInSelectionForwardState.Create(Self);
    TdxSearchState.FindAfterCaret:
      State := TdxFindAfterCaretForwardState.Create(Self);
    TdxSearchState.FindBeforeCaret:
      State := TdxFindBeforeCaretForwardState.Create(Self);
    TdxSearchState.FindAfterSelection:
      State := TdxFindAfterSelectionForwardState.Create(Self);
    TdxSearchState.FindBeforeSelection:
      State := TdxFindBeforeSelectionForwardState.Create(Self);
    TdxSearchState.FindFinish:
      State := TdxFindFinishState.Create(Self);
    else
      TdxRichEditExceptions.ThrowInternalException;
  end;
end;

function TdxTextSearchForwardProvider.CreateStringSearch: TdxSearchStrategy;
begin
  Result := TdxTextSearchByStringForwardStrategy.Create(PieceTable);
end;

function TdxTextSearchForwardProvider.CreateRegexSearch: TdxSearchStrategy;
begin
  Result := TdxTextSearchByRegexForwardStrategy.Create(PieceTable);
end;

function TdxTextSearchForwardProvider.GetStartAtPosition(ARunInfo: TdxRunInfo): TdxDocumentLogPosition;
var
  AOffset: Integer;
begin
  if Context.Action = TdxSearchAction.Find then
  begin
    if Parameters.UseRegularExpression then
    begin
      if ARunInfo.Start < ARunInfo.&End then
        Result := ARunInfo.&End.LogPosition
      else
        Result := GetNextVisiblePosition(ARunInfo.&End);
    end
    else
      Result := GetNextVisiblePosition(ARunInfo.Start);
  end
  else
  begin
    if Parameters.UseRegularExpression and (Context.MatchInfo <> nil) then
      AOffset := Length(Context.MatchInfo.Match.Result(Parameters.ReplaceString))
    else
      AOffset := Length(Parameters.ReplaceString);
    if (AOffset = 0) and (ARunInfo.&End = ARunInfo.Start) then
      AOffset := 1;

    Result := ARunInfo.Start.LogPosition + AOffset;
  end;
end;

function TdxTextSearchForwardProvider.GetNextVisiblePosition(
  const APosition: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := GetNextVisiblePosition(APosition.LogPosition);
end;

function TdxTextSearchForwardProvider.GetNextVisiblePosition(APosition: TdxDocumentLogPosition): TdxDocumentLogPosition;
var
  ANextPosition: TdxDocumentLogPosition;
  AModelPosition: TdxDocumentModelPosition;
begin
  ANextPosition := APosition + 1;
  if ANextPosition > PieceTable.DocumentEndLogPosition then
    Exit(ANextPosition);

  AModelPosition := TdxPositionConverter.ToDocumentModelPosition(PieceTable, ANextPosition);
  if PieceTable.VisibleTextFilter.IsRunVisible(AModelPosition.RunIndex) then
    Result := ANextPosition
  else
    Result := PieceTable.VisibleTextFilter.GetNextVisibleLogPosition(AModelPosition, False);
end;

{ TdxTextSearchBackwardProvider }

procedure TdxTextSearchBackwardProvider.SetState(ASearchType: TdxSearchState);
begin
  case ASearchType of
    TdxSearchState.FindStart:
      State := TdxFindStartState.Create(Self);
    TdxSearchState.FindInSelection:
      State := TdxFindInSelectionBackwardState.Create(Self);
    TdxSearchState.FindAfterCaret:
      State := TdxFindAfterCaretBackwardState.Create(Self);
    TdxSearchState.FindBeforeCaret:
      State := TdxFindBeforeCaretBackwardState.Create(Self);
    TdxSearchState.FindAfterSelection:
      State := TdxFindAfterSelectionBackwardState.Create(Self);
    TdxSearchState.FindBeforeSelection:
      State := TdxFindBeforeSelectionBackwardState.Create(Self);
    TdxSearchState.FindFinish:
      State := TdxFindFinishState.Create(Self);
    else
      TdxRichEditExceptions.ThrowInternalException;
  end;
end;

function TdxTextSearchBackwardProvider.CreateStringSearch: TdxSearchStrategy;
begin
  Result := TdxTextSearchByStringBackwardStrategy.Create(PieceTable);
end;

function TdxTextSearchBackwardProvider.CreateRegexSearch: TdxSearchStrategy;
begin
  Result := TdxTextSearchByRegexBackwardStrategy.Create(PieceTable);
end;

function TdxTextSearchBackwardProvider.GetStartAtPosition(ARunInfo: TdxRunInfo): TdxDocumentLogPosition;
begin
  if Context.Action = TdxSearchAction.Find then
  begin
    if Parameters.UseRegularExpression then
    begin
      if ARunInfo.Start < ARunInfo.&End then
        Result := ARunInfo.Start.LogPosition
      else
        Result := GetPrevVisiblePosition(ARunInfo.Start);
    end
    else
      Result := GetPrevVisiblePosition(ARunInfo.&End);
  end
  else
    Result := GetPrevVisiblePosition(ARunInfo.Start);
end;

function TdxTextSearchBackwardProvider.GetPrevVisiblePosition(
  const APosition: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := GetPrevVisiblePosition(APosition.LogPosition);
end;

function TdxTextSearchBackwardProvider.GetPrevVisiblePosition(APosition: TdxDocumentLogPosition): TdxDocumentLogPosition;
var
  APrevPosition: TdxDocumentLogPosition;
  AModelPosition: TdxDocumentModelPosition;
begin
  APrevPosition := APosition - 1;
  if APrevPosition < PieceTable.DocumentStartLogPosition then
    Exit(APosition);

  AModelPosition := TdxPositionConverter.ToDocumentModelPosition(PieceTable, APrevPosition);
  if PieceTable.VisibleTextFilter.IsRunVisible(AModelPosition.RunIndex) then
    Result := APrevPosition
  else
    Result := PieceTable.VisibleTextFilter.GetPrevVisibleLogPosition(AModelPosition, True);
end;

{ TdxDocumentCharacterIterator }

constructor TdxDocumentCharacterIterator.Create(APieceTable: TdxPieceTable);
begin
  inherited Create;
  FCachedRunIndex := TdxRunIndex(-1);
  Assert(Assigned(APieceTable), 'pieceTable');
  FPieceTable := APieceTable;
end;

constructor TdxDocumentCharacterIterator.Create(APieceTable: TdxPieceTable; AStart, AEnd: TdxDocumentLogPosition);
begin
  Create(APieceTable);
  SetInterval(AStart, AEnd);
end;

function TdxDocumentCharacterIterator.GetCurrentPosition: TdxDocumentModelPosition;
begin
  if FCurrentPosition.IsNull then
    InitializeCurrentPosition;
  Result := FCurrentPosition.Value;
end;

function TdxDocumentCharacterIterator.GetCurrentRun: TdxTextRunBase;
begin
  Result := PieceTable.Runs[CurrentPosition.RunIndex];
end;

procedure TdxDocumentCharacterIterator.SetInterval(AStart, AEnd: TdxDocumentLogPosition);
begin
  if AStart < 0 then
    TdxRichEditExceptions.ThrowArgumentException('startPosition', AStart);
  if (AEnd < AStart) or (AEnd > PieceTable.DocumentEndLogPosition) then
    TdxRichEditExceptions.ThrowArgumentException('endPosition', AEnd);
  FStartPosition := AStart;
  FEndPosition := AEnd;
end;

procedure TdxDocumentCharacterIterator.SetCurrentPosition(const APos: TdxDocumentModelPosition);
begin
  FCurrentPosition := APos;
end;

procedure TdxDocumentCharacterIterator.InvalidateCurrentPosition;
begin
  ResetCurrentPosition;
end;

function TdxDocumentCharacterIterator.GetCharacter: Char;
begin
  Result := GetCharacterByPosition(CurrentPosition);
end;

function TdxDocumentCharacterIterator.GetCharacter(AOffset: Integer): Char;
var
  ACharPosition: TdxDocumentModelPosition;
begin
  ACharPosition := GetModelPosition(AOffset);
  Result := GetCharacterByPosition(ACharPosition);
end;

function TdxDocumentCharacterIterator.GetCharacter(AOffset: Integer; out APos: TdxDocumentLogPosition): Char;
var
  ACharPosition: TdxDocumentModelPosition;
begin
  ACharPosition := GetModelPosition(AOffset);
  APos := ACharPosition.LogPosition;
  Result := GetCharacterByPosition(ACharPosition);
end;

function TdxDocumentCharacterIterator.GetModelPosition(AOffset: Integer): TdxDocumentModelPosition;
begin
  Result := GetModelPosition(CurrentPosition, AOffset);
end;

function TdxDocumentCharacterIterator.GetModelPosition(const APos: TdxDocumentModelPosition; AOffset: Integer): TdxDocumentModelPosition;
begin
  Result := APos;
  ShiftModelPosition(Result, AOffset);
end;

function TdxDocumentCharacterIterator.GetCharacterByPosition(const APos: TdxDocumentModelPosition): Char;
var
  ARunText: string;
  AOffset: Integer;
begin
  ARunText := PieceTable.Runs[APos.RunIndex].GetNonEmptyText(PieceTable.TextBuffer);
  AOffset := APos.LogPosition - APos.RunStartLogPosition;
  if (AOffset < 0) or (AOffset >= Length(ARunText)) then
    Result := #0
  else
    Result := ARunText[AOffset];
end;

function TdxDocumentCharacterIterator.IsVisible(const APos: TdxDocumentModelPosition): Boolean;
var
  ARunIndex: TdxRunIndex;
begin
  ARunIndex := APos.RunIndex;
  if ARunIndex <> FCachedRunIndex then
  begin
    FCachedRunIndex := ARunIndex;
    FIsCachedRunVisible := FPieceTable.VisibleTextFilter.IsRunVisible(ARunIndex);
  end;
  Result := FIsCachedRunVisible;
end;

function TdxDocumentCharacterIterator.HasNextChar: Boolean;
begin
  Result := CanMoveOnOffset(0);
end;

procedure TdxDocumentCharacterIterator.ShiftCurrentPosition(AOffset: Integer);
var
  APos: TdxDocumentModelPosition;
begin
  APos := CurrentPosition;
  ShiftModelPosition(APos, AOffset);
  CurrentPosition := APos;
end;

procedure TdxDocumentCharacterIterator.MoveNext;
var
  APos: TdxDocumentModelPosition;
begin
  APos := CurrentPosition;
  ShiftModelPosition(APos, 1);
  CurrentPosition := APos;
end;

procedure TdxDocumentCharacterIterator.ResetCurrentPosition;
begin
  FCurrentPosition.Reset;
end;

procedure TdxDocumentCharacterIterator.ShiftModelPosition(var APos: TdxDocumentModelPosition; AOffset: Integer);
var
  ACount, I: Integer;
begin
  ACount := Abs(AOffset);
  for I := 0 to ACount - 1 do
    MoveToVisiblePosition(APos);
end;

{ TdxDocumentCharacterIteratorForward }

procedure TdxDocumentCharacterIteratorForward.InitializeCurrentPosition;
begin
  CurrentPosition := TdxPositionConverter.ToDocumentModelPosition(PieceTable, StartPosition);
end;

function TdxDocumentCharacterIteratorForward.CanMoveOnOffset(AOffset: Integer): Boolean;
begin
  Result := (CurrentPosition.LogPosition + AOffset) <= EndPosition;
end;

procedure TdxDocumentCharacterIteratorForward.MoveToVisiblePosition(var APos: TdxDocumentModelPosition);
begin
  repeat
    if APos.LogPosition < PieceTable.DocumentEndLogPosition then
      TdxDocumentModelPosition.MoveForwardCore(APos)
    else
      APos.LogPosition := APos.LogPosition + 1;
  until IsVisible(APos);
end;

{ TdxBMTextSearchBase }

constructor TdxBMTextSearchBase.Create(APieceTable: TdxPieceTable; const APattern: string; ACaseSensitive: Boolean);
begin
  inherited Create;
  Assert(Assigned(APieceTable), 'pieceTable');
  FPieceTable := APieceTable;
  FCaseSensitive := ACaseSensitive;
  FOriginalPattern := APattern;
  InitializeTables;
end;

procedure TdxBMTextSearchBase.InitializeTables;
begin
  FPattern := CalculateWorkPattern;
  FBadCharacterShiftTable := CreateBadCharacterShiftTable;
  FGoodSuffixShiftTable := CreateGoodSuffixShiftTable;
end;

function TdxBMTextSearchBase.CalculateWorkPattern: string;
begin
  Result := OriginalPattern;
end;

function TdxBMTextSearchBase.GetCharacter(AIterator: TdxDocumentCharacterIterator; AOffset: Integer): Char;
begin
  Result := AIterator.GetCharacter(AOffset);
  if not CaseSensitive then
    Result := {$IFDEF DELPHIXE4}Result.ToLower{$ELSE}TCharacter.ToLower(Result){$ENDIF};
end;

function TdxBMTextSearchBase.CreateUnicodeAlphabetTable(AInitialValue: Integer): TArray<Integer>;
var
  I: Integer;
begin
  SetLength(Result, UnicodeAlphabetSize);
  for I := 0 to UnicodeAlphabetSize - 1 do
    Result[I] := AInitialValue;
end;

procedure TdxBMTextSearchBase.PopulateBadCharacterShiftTable(const APattern: string; const ATable: TArray<Integer>);
var
  APatternLength, I, AOffset: Integer;
  ACh: Char;
begin
  APatternLength := Length(APattern);
  for I := 0 to APatternLength - 1 - 1 do
  begin
    ACh := APattern[I + 1];
    AOffset := APatternLength - I - 1;
    ATable[Integer(ACh)] := AOffset;
  end;
end;

function TdxBMTextSearchBase.CreateBadCharacterShiftTable: TArray<Integer>;
begin
  Result := CreateUnicodeAlphabetTable(Length(Pattern));
  if CaseSensitive then
    PopulateBadCharacterShiftTable(Pattern, Result)
  else
  begin
    PopulateBadCharacterShiftTable({$IFDEF DELPHIXE4}Pattern.ToLower{$ELSE}TCharacter.ToLower(Pattern){$ENDIF}, Result);
    PopulateBadCharacterShiftTable({$IFDEF DELPHIXE4}Pattern.ToUpper{$ELSE}TCharacter.ToUpper(Pattern){$ENDIF}, Result);
  end;
end;

function TdxBMTextSearchBase.CreateSuffixTable(const APattern: string): TArray<Integer>;
var
  APatternLength, G, F, I: Integer;
  ALPattern: string;
begin
  APatternLength := Length(APattern);
  SetLength(Result, APatternLength);
  if not CaseSensitive then
    ALPattern := {$IFDEF DELPHIXE4}APattern.ToLower{$ELSE}TCharacter.ToLower(APattern){$ENDIF}
  else
    ALPattern := APattern;
  Result[APatternLength - 1] := APatternLength;
  G := APatternLength - 1;
  F := 0;
  for I := APatternLength - 2 downto 0 do
  begin
    if (I > G) and (Result[I + APatternLength - 1 - F] < I - G) then
      Result[I] := Result[I + APatternLength - 1 - F]
    else
    begin
      if I < G then
        G := I;
      F := I;
      while (G >= 0) and (ALPattern[G + 1] = ALPattern[G + APatternLength - 1 - F + 1]) do
        Dec(G);
      Result[I] := F - G;
    end;
  end;
end;

function TdxBMTextSearchBase.CreateGoodSuffixShiftTable: TArray<Integer>;
var
  APatternLength, I , J: Integer;
  ASuffixes: TArray<Integer>;
begin
  APatternLength := Length(Pattern);
  SetLength(Result, APatternLength);
  for I := 0 to APatternLength - 1 do
    Result[I] := APatternLength;
  ASuffixes := CreateSuffixTable(Pattern);
  for I := APatternLength - 1 downto 0 do
  begin
    if ASuffixes[I] = I + 1 then
    begin
      for J := 0 to APatternLength - I - 1 - 1 do
      begin
        if Result[J] = APatternLength then
          Result[J] := APatternLength - I - 1;
      end;
    end;
  end;
  for I := 0 to APatternLength - 1 - 1 do
    Result[APatternLength - 1 - ASuffixes[I]] := APatternLength - I - 1;
end;

{ TdxBMTextSearchForward }

function TdxBMTextSearchForward.Search(const AStart, AEnd: TdxDocumentLogPosition): TdxNullableValue<TdxDocumentModelPosition>;
var
  APosition: TdxDocumentModelPosition;
  AIterator: TdxBufferedDocumentCharacterIterator;
  APatternLength, AShift, U, AOffset, V, ATurboShift, ABadCharShift: Integer;
begin
  APosition := TdxPositionConverter.ToDocumentModelPosition(PieceTable, AStart);
  AIterator := TdxBufferedDocumentCharacterIteratorForward.Create(APosition);
  try
    APatternLength := Length(Pattern);
    AShift := APatternLength;
    U := 0;
    while AIterator.CurrentPosition.LogPosition < AEnd do
    begin

      AIterator.AppendBuffer(APatternLength);
      AOffset := APatternLength - 1;
      if not AIterator.IsCharacterExist(AOffset) then
        Break;

      while (AOffset >= 0) and AIterator.Compare(AOffset, Pattern[AOffset + 1], not CaseSensitive) do
      begin
        Dec(AOffset);
        if (U <> 0) and (AOffset = APatternLength - 1 - AShift) then
          Dec(AOffset, U);
      end;
      if AOffset < 0 then
      begin
        if AIterator.GetPosition(Length(Pattern)).LogPosition <= AEnd then
          Exit(AIterator.StartPosition)
        else
          Exit(TdxNullableValue<TdxDocumentModelPosition>.Null);
      end
      else
      begin
        V := APatternLength - 1 - AOffset;
        ATurboShift := U - V;
        ABadCharShift := BadCharacterShiftTable[Integer(AIterator[AOffset])] - APatternLength + 1 + AOffset;
        AShift := Math.Max(ATurboShift, ABadCharShift);
        AShift := Math.Max(AShift, GoodSuffixShiftTable[AOffset]);
        if AShift = GoodSuffixShiftTable[AOffset] then
          U := Math.Min(APatternLength - AShift, V)
        else
        begin
          if ATurboShift < ABadCharShift then
            AShift := Math.Max(AShift, U + 1);
          U := 0;
        end;
      end;
      AIterator.ShiftBuffer(AShift);
    end;
    Result := TdxNullableValue<TdxDocumentModelPosition>.Null;
  finally
    AIterator.Free;
  end;
end;

{ TdxBMTextSearchBackward }

function TdxBMTextSearchBackward.CalculateWorkPattern: string;
var
  APattern: string;
  ASb: TStringBuilder;
  I: Integer;
begin
  APattern := inherited CalculateWorkPattern;
  ASb := TStringBuilder.Create;
  try
    for I := Length(APattern) downto 1 do
      ASb.Append(APattern[I]);
    Result := ASb.ToString;
  finally
    ASb.Free;
  end;
end;

function TdxBMTextSearchBackward.Search(const AStart, AEnd: TdxDocumentLogPosition): TdxNullableValue<TdxDocumentModelPosition>;
var
  APosition: TdxDocumentModelPosition;
  AIterator: TdxBufferedDocumentCharacterIterator;
  APatternLength, AShift, U, AOffset, V, ATurboShift, ABadCharShift: Integer;
begin
  APosition := TdxPositionConverter.ToDocumentModelPosition(PieceTable, AEnd);
  AIterator := TdxBufferedDocumentCharacterIteratorBackward.Create(APosition);
  try
    APatternLength := Length(Pattern);
    AShift := APatternLength;
    U := 0;

    while AIterator.CurrentPosition.LogPosition > AStart do
    begin
      AIterator.AppendBuffer(APatternLength);
      AOffset := APatternLength - 1;
      if not AIterator.IsCharacterExist(AOffset) then
        Break;

      while (AOffset >= 0) and AIterator.Compare(AOffset, Pattern[AOffset + 1], not CaseSensitive) do
      begin
        Dec(AOffset);
        if (U <> 0) and (AOffset = AShift) then
          Dec(AOffset, U);
      end;
      if AOffset < 0 then
      begin
        Result := AIterator.GetPosition(APatternLength);
        if Result.Value.LogPosition >= AStart then
          Exit
        else
          Exit(TdxNullableValue<TdxDocumentModelPosition>.Null);
      end
      else
      begin
        V := APatternLength - 1 - AOffset;
        ATurboShift := U - V;
        ABadCharShift := BadCharacterShiftTable[Integer(AIterator[AOffset])] - APatternLength + 1 + AOffset;
        AShift := Math.Max(ATurboShift, ABadCharShift);
        AShift := Math.Max(AShift, GoodSuffixShiftTable[AOffset]);
        if AShift = GoodSuffixShiftTable[AOffset] then
          U := Math.Min(APatternLength - AShift, V)
        else
        begin
          if ATurboShift < ABadCharShift then
            AShift := Math.Max(AShift, U + 1);
          U := 0;
        end;
      end;
      AIterator.ShiftBuffer(AShift);
    end;
    Result := TdxNullableValue<TdxDocumentModelPosition>.Null;
  finally
    AIterator.Free;
  end;
end;

{ TdxSearchIntervalCalculatorForward }

function TdxSearchIntervalCalculatorForward.CalculateStartPosition(const AStart, AEnd: TdxDocumentLogPosition;
  APrevResult: TdxSearchResult): TdxDocumentLogPosition;
begin
  if APrevResult <> nil then
    Result := TdxDocumentModelPosition.MoveForward(APrevResult.Value.Start).LogPosition
  else
    Result := AStart;
end;

function TdxSearchIntervalCalculatorForward.CalculateEndPosition(const AStart, AEnd: TdxDocumentLogPosition;
  APrevResult: TdxSearchResult): TdxDocumentLogPosition;
begin
  Result := AEnd;
end;

{ TdxSearchIntervalCalculatorBackward }

function TdxSearchIntervalCalculatorBackward.CalculateStartPosition(const AStart, AEnd: TdxDocumentLogPosition;
  APrevResult: TdxSearchResult): TdxDocumentLogPosition;
begin
  Result := AStart;
end;

function TdxSearchIntervalCalculatorBackward.CalculateEndPosition(const AStart, AEnd: TdxDocumentLogPosition;
  APrevResult: TdxSearchResult): TdxDocumentLogPosition;
begin
  if APrevResult <> nil then
    Result := TdxDocumentModelPosition.MoveBackward(APrevResult.Value.&End).LogPosition
  else
    Result := AEnd;
end;

{ TdxSearchConditions }

constructor TdxSearchConditions.Create(const AInput: string; AStartIndex: Integer; ALength: Integer);
begin
  Create(AInput);
  FStartIndex := AStartIndex;
  FLength := ALength;
end;

constructor TdxSearchConditions.Create(const AInput: string);
begin
  FInput := AInput;
end;

{ TdxBufferedRegexSearchBase }

constructor TdxBufferedRegexSearchBase.Create(APieceTable: TdxPieceTable; const ARegex: TRegex; ABufferSize, ABufferShiftSize: Integer);
begin
  inherited Create;
  FPieceTable := APieceTable;
  FRegex := ARegex;
  FBufferSize := ABufferSize;
  FBufferShiftSize := ABufferShiftSize;
  FBuffer := TStringBuilder.Create(ABufferSize);
end;

constructor TdxBufferedRegexSearchBase.Create(APieceTable: TdxPieceTable; const ARegex: TRegex; AMaxGuaranteedSearchResultLength: Integer);
begin
  Create(APieceTable, ARegex, 2 * AMaxGuaranteedSearchResultLength, AMaxGuaranteedSearchResultLength);
end;

constructor TdxBufferedRegexSearchBase.Create(APieceTable: TdxPieceTable; const ARegex: TRegex);
begin
  Create(APieceTable, ARegex, 128);
end;

function TdxBufferedRegexSearchBase.Match(AStart, AEnd: TdxDocumentLogPosition): TdxBufferedRegexSearchResult;
var
  APosition: TdxDocumentModelPosition;
  AIterator: TdxBufferedDocumentCharacterIterator;
  AConditions: TdxSearchConditions;
  AMatch: TMatch;
begin
  InitializeSearchInterval(AStart, AEnd);
  APosition := CalculateStartPosition;
  Initialize(AStart, AEnd, APosition);
  AIterator := CreateIterator(APosition);
  try
    while True do
    begin
      AppendBuffer(AIterator);
      if FBuffer.Length = 0 then
        Break;
      AConditions := TdxSearchConditions.Create(Buffer.ToString, 0, Buffer.Length);
      AMatch := CalculateMatch(AIterator, Regex, AConditions);
      if AMatch.Success then
        Exit(CreateResult(AMatch));
      ShiftBuffer(AIterator, BufferShiftSize);
    end;
  finally
    AIterator.Free;
  end;
  Result := nil;
end;

procedure TdxBufferedRegexSearchBase.Initialize(AStart, AEnd: TdxDocumentLogPosition; const ACurrentPosition: TdxDocumentModelPosition);
begin
  FBufferStartPosition := ACurrentPosition;
  FCurrentInputPosition := ACurrentPosition;
  FBuffer.Length := 0;
  FBufferOffset := 0;
end;

procedure TdxBufferedRegexSearchBase.InitializeSearchInterval(AStart, AEnd: TdxDocumentLogPosition);
var
  APrevVisiblePosition, ANextVisiblePosition: TdxDocumentLogPosition;
begin
  FSearchInfo := [];
  if AStart > PieceTable.DocumentStartLogPosition then
  begin
    APrevVisiblePosition := PieceTable.VisibleTextFilter.GetPrevVisibleLogPosition(AStart, True);
    FStartPosition := Max(PieceTable.DocumentStartLogPosition, APrevVisiblePosition);
    Include(FSearchInfo, TdxRegexSearchInfo.StartExtended);
  end
  else
    FStartPosition := AStart;

  if AEnd < PieceTable.DocumentEndLogPosition then
  begin
    ANextVisiblePosition := PieceTable.VisibleTextFilter.GetNextVisibleLogPosition(AEnd, True);
    FEndPosition := Min(PieceTable.DocumentEndLogPosition, ANextVisiblePosition);
    Include(FSearchInfo, TdxRegexSearchInfo.EndExtended);
  end
  else
    FEndPosition := AEnd;
end;

function TdxBufferedRegexSearchBase.CreateResult(const AMatch: TMatch): TdxBufferedRegexSearchResult;
begin
  Result := TdxBufferedRegexSearchResult.Create(FRegex, AMatch, BufferStartPosition);
end;

function TdxBufferedRegexSearchBase.Matches(AStart, AEnd: TdxDocumentLogPosition): TdxBufferedRegexSearchResultCollection;
var
  APosition: TdxDocumentModelPosition;
  AIterator: TdxBufferedDocumentCharacterIterator;
  AResults: TdxBufferedRegexSearchResultCollection;
  AIsSuccess: Boolean;
  AShiftSize, ALastIndex: Integer;
begin
  InitializeSearchInterval(AStart, AEnd);
  APosition := CalculateStartPosition;
  Initialize(AStart, AEnd, APosition);
  AIterator := CreateIterator(APosition);
  AResults := TdxBufferedRegexSearchResultCollection.Create;
  try
    while True do
    begin
      AppendBuffer(AIterator);
      AIsSuccess := ProcessBuffer(AIterator, Regex, AResults);
      if FBuffer.Length < BufferSize then
        Exit(CreateResult(AResults));

      AShiftSize := 0;
      if AIsSuccess then
      begin
        ALastIndex := AResults.Count - 1;
        AShiftSize := CalculateBufferShiftSize(AResults[ALastIndex]);
      end;
      AShiftSize := Max(AShiftSize, BufferShiftSize);
      ShiftBuffer(AIterator, AShiftSize);
    end;
  finally
    AIterator.Free;
    AResults.Free;
  end;
end;

function TdxBufferedRegexSearchBase.ProcessBuffer(AIterator: TdxBufferedDocumentCharacterIterator; const ARegEx: TRegex;
  AResults: TdxBufferedRegexSearchResultCollection): Boolean;
var
  AConditions: TdxSearchConditions;
  AMatch: TMatch;
  ASearchResult, ALastResult: TdxBufferedRegexSearchResult;
  AAbsolutePosition: Integer;
begin
  AConditions := TdxSearchConditions.Create(Buffer.ToString, 0, Buffer.Length);
  Result := False;
  while True do
  begin
    AMatch := CalculateMatch(AIterator, ARegEx, AConditions);
    if not AMatch.Success then
      Break;

    ASearchResult := CreateResult(AMatch);
    AAbsolutePosition := GetMatchAbsolutePosition(AMatch);
    if AResults.Count > 0 then
      ALastResult := AResults[AResults.Count - 1]
    else
      ALastResult := nil;
    if (ALastResult = nil) or (AAbsolutePosition > ALastResult.AbsolutePosition) then
    begin
      ASearchResult.AbsolutePosition := AAbsolutePosition;
      AResults.Add(ASearchResult);
      Result := True;
    end;
    CalculateSearchConditions(AMatch, AConditions);
  end;
end;

function TdxBufferedRegexSearchBase.CreateResult(AResults: TdxBufferedRegexSearchResultCollection): TdxBufferedRegexSearchResultCollection;
var
  ACount, I: Integer;
begin
  Result := TdxBufferedRegexSearchResultCollection.Create;
  ACount := AResults.Count;
  for I := 0 to ACount - 1 do
    Result.Add(AResults[I]);
end;

destructor TdxBufferedRegexSearchBase.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

procedure TdxBufferedRegexSearchBase.ShiftBuffer(AIterator: TdxBufferedDocumentCharacterIterator; ADelta: Integer);
var
  AShiftSize: Integer;
begin
  AShiftSize := Min(ADelta, Buffer.Length);
  FBufferOffset := FBufferOffset + AShiftSize;
  Buffer.Length := Buffer.Length - AShiftSize;
end;

function TdxBufferedRegexSearchBase.IsMatchGreatTheEndOfBuffer(const AMatch: TMatch): Boolean;
begin
  Result := AMatch.Index + AMatch.Length > Buffer.Length;
end;

function TdxBufferedRegexSearchBase.IsMatchInTheEndOfBuffer(const AMatch: TMatch): Boolean;
begin
  Result := AMatch.Index + AMatch.Length = Buffer.Length;
end;

function TdxBufferedRegexSearchBase.IsMatchInTheStartOfBuffer(const AMatch: TMatch): Boolean;
begin
  Result := AMatch.Index = 0;
end;

function TdxBufferedRegexSearchBase.ShouldRecalcMatchAtStartOfBuffer(var AConditions: TdxSearchConditions): Boolean;
begin
  if TdxRegexSearchInfo.StartExtended in SearchInfo then
  begin
    Assert(AConditions.StartIndex = 0);
    AConditions.StartIndex := AConditions.StartIndex + 1;
    AConditions.Length := AConditions.Length - 1;
    Result := True;
  end
  else
    Result := False;
end;

function TdxBufferedRegexSearchBase.ShouldRecalcMatchAtEndOfBuffer(var AConditions: TdxSearchConditions): Boolean;
begin
  if TdxRegexSearchInfo.EndExtended in SearchInfo then
  begin
    AConditions.Length := AConditions.Length - 1;
    Result := True;
  end
  else
    Result := False;
end;

{ TdxBufferedRegexSearchForward }

function TdxBufferedRegexSearchForward.CalculateMatchCore(const ARegEx: TRegex; const AConditions: TdxSearchConditions): TMatch;
var
  AInput: string;
begin
  AInput := TdxStringHelper.Substring(AConditions.Input, 0, AConditions.StartIndex + AConditions.Length);
  Result := ARegEx.Match(AInput, AConditions.StartIndex + 1);
end;

procedure TdxBufferedRegexSearchForward.CalculateSearchConditions(const APrevMatch: TMatch; var AConditions: TdxSearchConditions);
var
  ANewIndex: Integer;
begin
  ANewIndex := APrevMatch.Index + IfThen(APrevMatch.Length > 0, APrevMatch.Length, 1);
  AConditions.Length := AConditions.Length - (ANewIndex - AConditions.StartIndex);
  AConditions.StartIndex := ANewIndex;
end;

procedure TdxBufferedRegexSearchForward.AppendBuffer(AIterator: TdxBufferedDocumentCharacterIterator);
var
  ADelta, ACount, ACharIndex: Integer;
begin
  ADelta := BufferSize - Buffer.Length;
  AIterator.AppendBuffer(ADelta);
  ACount := Min(ADelta, AIterator.BufferLength);
  ACharIndex := 0;
  while (ACharIndex < ACount) and CanMoveNext do
  begin
    Buffer.Append(AIterator[ACharIndex]);
    BufferInputPosition := AIterator.GetPosition(BufferInputPosition, 1);
    Inc(ACharIndex);
  end;
  AIterator.ShiftBuffer(ACharIndex);
end;

procedure TdxBufferedRegexSearchForward.ShiftBuffer(AIterator: TdxBufferedDocumentCharacterIterator; ADelta: Integer);
var
  ACount, ASourceIndex, ATargetIndex, AShiftSize: Integer;
begin
  ACount := Buffer.Length;
  ATargetIndex := 0;
  for ASourceIndex := ADelta to ACount - 1 do
  begin
    Buffer[ATargetIndex] := Buffer[ASourceIndex];
    Inc(ATargetIndex);
  end;
  AShiftSize := Min(ADelta, ACount);
  BufferStartPosition := AIterator.GetPosition(BufferStartPosition, AShiftSize);
  inherited ShiftBuffer(AIterator, ADelta);
end;

function TdxBufferedRegexSearchForward.CalculateMatch(AIterator: TdxBufferedDocumentCharacterIterator; const ARegEx: TRegex;
  var AConditions: TdxSearchConditions): TMatch;
var
  AResult: TMatch;
begin
  if AConditions.Length <= 0 then
    Exit(Default(TMatch));
  while True do
  begin
    AResult := CalculateMatchCore(ARegEx, AConditions);
    if AResult.Success then
    begin
      if IsMatchInTheStartOfBuffer(AResult) then
      begin
        if BufferOffset > 0 then
        begin
          CalculateSearchConditions(AResult, AConditions);
          Continue;
        end
        else
          if ShouldRecalcMatchAtStartOfBuffer(AConditions) then
            Continue;
      end;
      if IsMatchInTheEndOfBuffer(AResult) then
      begin
        if CanMoveNext then
          Exit(Default(TMatch))
        else
          if ShouldRecalcMatchAtEndOfBuffer(AConditions) then
            Continue;
      end
      else
        if IsMatchGreatTheEndOfBuffer(AResult) then
          Exit(Default(TMatch));
      Exit(AResult);
    end
    else
      Exit(AResult);
  end;
end;

function TdxBufferedRegexSearchForward.GetMatchAbsolutePosition(const AMatch: TMatch): Integer;
begin
  Result := AMatch.Index + BufferOffset;
end;

function TdxBufferedRegexSearchForward.CreateIterator(const APosition: TdxDocumentModelPosition): TdxBufferedDocumentCharacterIterator;
begin
  Result := TdxBufferedDocumentCharacterIteratorForward.Create(APosition);
end;

function TdxBufferedRegexSearchForward.CalculateStartPosition: TdxDocumentModelPosition;
begin
  Result := TdxPositionConverter.ToDocumentModelPosition(PieceTable, StartPosition);
end;

function TdxBufferedRegexSearchForward.CanMoveNext: Boolean;
begin
  Result := BufferInputPosition.LogPosition < EndPosition;
end;

function TdxBufferedRegexSearchForward.CalculateBufferShiftSize(AResult: TdxBufferedRegexSearchResult): Integer;
begin
  Result := AResult.Match.Index + AResult.Match.Length;
end;

{ TdxBufferedRegexSearchBackward }

procedure TdxBufferedRegexSearchBackward.CalculateSearchConditions(const APrevMatch: TMatch; var AConditions: TdxSearchConditions);
begin
  AConditions.Length := APrevMatch.Index - AConditions.StartIndex;
end;

procedure TdxBufferedRegexSearchBackward.AppendBuffer(AIterator: TdxBufferedDocumentCharacterIterator);
var
  ADelta, ACount, AIndex, ACharIndex, AStartIndex, AAppendCharsCount: Integer;
  ACharBuffer: TArray<Char>;
begin
  ADelta := BufferSize - Buffer.Length;
  AIterator.AppendBuffer(ADelta);
  ACount := Min(ADelta, AIterator.BufferLength);
  SetLength(ACharBuffer, ACount);
  AIndex := 0;

  ACharIndex := ACount - 1;
  while (ACharIndex >= 0) and CanMoveNext do
  begin
    ACharBuffer[ACharIndex] := AIterator[AIndex];
    BufferInputPosition := AIterator.GetPosition(BufferInputPosition, 1);
    Inc(AIndex);
    Dec(ACharIndex);
  end;

  AStartIndex := ACharIndex + 1;
  AAppendCharsCount := ACount - AStartIndex;
  AIterator.ShiftBuffer(AAppendCharsCount);
  BufferStartPosition := AIterator.GetPosition(BufferStartPosition, AIndex);
  Buffer.Insert(0, ACharBuffer, AStartIndex, AAppendCharsCount);
end;

function TdxBufferedRegexSearchBackward.CalculateMatch(AIterator: TdxBufferedDocumentCharacterIterator; const ARegEx: TRegex;
  var AConditions: TdxSearchConditions): TMatch;
var
  AResult: TMatch;
begin
  if AConditions.Length <= 0 then
    Exit(Default(TMatch));
  while True do
  begin
    AResult := CalculateMatchCore(ARegEx, AConditions);
    if AResult.Success then
    begin
      if IsMatchInTheStartOfBuffer(AResult) then
      begin
        if CanMoveNext then
          Exit(Default(TMatch))
        else
          if ShouldRecalcMatchAtStartOfBuffer(AConditions) then
            Continue;
      end;
      if IsMatchInTheEndOfBuffer(AResult) then
      begin
        if BufferOffset > 0 then
        begin
          CalculateSearchConditions(AResult, AConditions);
          Continue;
        end
        else
          if ShouldRecalcMatchAtEndOfBuffer(AConditions) then
            Continue;
      end;
      Exit(AResult);
    end
    else
      Exit(AResult);
  end;
end;

function TdxBufferedRegexSearchBackward.CalculateMatchCore(const ARegEx: TRegEx;
  const AConditions: TdxSearchConditions): TMatch;
var
  AMatch: TMatch;
begin
  Result := ARegEx.Match(AConditions.Input, AConditions.StartIndex + 1, AConditions.Length - 1);
  while Result.Success do
  begin
    AMatch := Result.NextMatch;
    if not AMatch.Success then
      Exit;
    Result := AMatch;
  end;
end;

function TdxBufferedRegexSearchBackward.GetMatchAbsolutePosition(const AMatch: TMatch): Integer;
begin
  Result := (BufferOffset + Buffer.Length) - AMatch.Index;
end;

function TdxBufferedRegexSearchBackward.CreateIterator(const APosition: TdxDocumentModelPosition): TdxBufferedDocumentCharacterIterator;
begin
  Result := TdxBufferedDocumentCharacterIteratorBackward.Create(APosition);
end;

function TdxBufferedRegexSearchBackward.CalculateStartPosition: TdxDocumentModelPosition;
begin
  Result := TdxPositionConverter.ToDocumentModelPosition(PieceTable, EndPosition);
end;

function TdxBufferedRegexSearchBackward.CanMoveNext: Boolean;
begin
  Result := BufferInputPosition.LogPosition > StartPosition;
end;

function TdxBufferedRegexSearchBackward.CalculateBufferShiftSize(AResult: TdxBufferedRegexSearchResult): Integer;
begin
  Result := Buffer.Length - AResult.Match.Index;
end;

{ TdxTextSearchStrategyBase }

constructor TdxTextSearchStrategyBase<T>.Create(APieceTable: TdxPieceTable);
begin
  inherited Create;
  Assert(Assigned(APieceTable), 'pieceTable');
  FPieceTable := APieceTable;
end;

function TdxTextSearchStrategyBase<T>.Match(const APattern: string; AOptions: TdxSearchOptions;
  const AStart, AEnd: TdxDocumentLogPosition): TdxSearchResult;
var
  AWholeWord: Boolean;
  ASearcher: T;
  ACalculator: TdxSearchIntervalCalculator;
  AResult: TdxSearchResult;
begin
  if APattern = '' then
    raise Exception.Create('pattern');

  AWholeWord := TdxSearchOption.WholeWord in AOptions;
  Result := nil;
  ASearcher := CreateSearcher(PreparePattern(APattern), AOptions);
  ACalculator := CreateIntervalCalculator;
  try
    while True do
    begin
      AResult := MatchCore(ASearcher, ACalculator.CalculateStartPosition(AStart, AEnd, Result),
        ACalculator.CalculateEndPosition(AStart, AEnd, Result));
      if Result <> nil  then
      begin
        Result.Clear;
        Result.Free;
      end;
      Result := AResult;
      if (not Result.Success) or (not AWholeWord) or (IsWordWhole(Result.Value)) then
        Break;
    end;
  finally
    ASearcher.Free;
    ACalculator.Free;
  end;
end;

function TdxTextSearchStrategyBase<T>.PreparePattern(const APattern: string): string;
begin
  Result := StringReplace(APattern, #10#13, #13, [rfReplaceAll]);
end;

function TdxTextSearchStrategyBase<T>.IsWordWhole(ARunInfo: TdxRunInfo): Boolean;
var
  APrevChar, ANextChar: Char;
begin
  APrevChar := GetPrevCharacter(ARunInfo.Start);
  if (APrevChar <> #0) and {$IFDEF DELPHIXE4}APrevChar.IsLetterOrDigit{$ELSE}TCharacter.IsLetterOrDigit(APrevChar){$ENDIF} then
    Exit(False);

  ANextChar := GetNextCharacter(ARunInfo.&End);
  if (ANextChar <> #0) and {$IFDEF DELPHIXE4}ANextChar.IsLetterOrDigit{$ELSE}TCharacter.IsLetterOrDigit(ANextChar){$ENDIF} then
    Result := False
  else
    Result := True;
end;

function TdxTextSearchStrategyBase<T>.GetPrevCharacter(const AStart: TdxDocumentModelPosition): Char;
var
  ARunIndex: TdxRunIndex;
  ARunText: string;
begin
  if AStart.LogPosition <= PieceTable.DocumentStartLogPosition then
    Exit(#0);

  if AStart.RunOffset > 0 then
    Exit(PieceTable.Runs[AStart.RunIndex].GetNonEmptyText(PieceTable.TextBuffer)[AStart.RunOffset - 1 + 1]);

  ARunIndex := AStart.RunIndex - 1;
  ARunText := PieceTable.Runs[ARunIndex].GetNonEmptyText(PieceTable.TextBuffer);
  Result := ARunText[Length(ARunText)];
end;

function TdxTextSearchStrategyBase<T>.GetNextCharacter(const AEnd: TdxDocumentModelPosition): Char;
begin
  if AEnd.LogPosition >= PieceTable.DocumentEndLogPosition then
    Result := #0
  else
    Result := PieceTable.Runs[AEnd.RunIndex].GetNonEmptyText(PieceTable.TextBuffer)[AEnd.RunOffset + 1];
end;

{ TdxTextSearchByStringStrategy }

function TdxTextSearchByStringStrategy.MatchCore(ASearcher: TdxBMTextSearchBase; const AStart, AEnd: TdxDocumentLogPosition): TdxSearchResult;
var
  AResult: TdxNullableValue<TdxDocumentModelPosition>;
  AIterator: TdxBufferedDocumentCharacterIteratorForward;
  ARunInfo: TdxRunInfo;
begin
  AResult := ASearcher.Search(AStart, AEnd);
  if AResult.IsNull then
    Exit(TdxSearchResult.Create);

  AIterator := TdxBufferedDocumentCharacterIteratorForward.Create(AResult.Value);
  try
    ARunInfo := TdxRunInfo.Create(PieceTable);
    ARunInfo.Start.CopyFrom(AResult.Value);
    ARunInfo.&End.CopyFrom(AIterator.GetPosition(Length(ASearcher.Pattern)));
    Result := TdxSearchResult.Create(ARunInfo);
  finally
    AIterator.Free;
  end;
end;

{ TdxTextSearchByStringForwardStrategy }

function TdxTextSearchByStringForwardStrategy.CreateSearcher(const APattern: string; AOptions: TdxSearchOptions): TdxBMTextSearchBase;
var
  ACaseSensitive: Boolean;
begin
  ACaseSensitive := TdxSearchOption.CaseSensitive in AOptions;
  Result := TdxBMTextSearchForward.Create(PieceTable, APattern, ACaseSensitive);
end;

function TdxTextSearchByStringForwardStrategy.CreateIntervalCalculator: TdxSearchIntervalCalculator;
begin
  Result := TdxSearchIntervalCalculatorForward.Create;
end;

{ TdxTextSearchByStringBackwardStrategy }

function TdxTextSearchByStringBackwardStrategy.CreateIntervalCalculator: TdxSearchIntervalCalculator;
begin
  Result := TdxSearchIntervalCalculatorBackward.Create;
end;

function TdxTextSearchByStringBackwardStrategy.CreateSearcher(const APattern: string; AOptions: TdxSearchOptions): TdxBMTextSearchBase;
var
  ACaseSensitive: Boolean;
begin
  ACaseSensitive := TdxSearchOption.CaseSensitive in AOptions;
  Result := TdxBMTextSearchBackward.Create(PieceTable, APattern, ACaseSensitive);
end;

 { TdxTextSearchByRegexStrategy }

function TdxTextSearchByRegexStrategy.MatchCore(ASearcher: TdxBufferedRegexSearchBase; const AStart, AEnd: TdxDocumentLogPosition): TdxSearchResult;
var
  AResult: TdxBufferedRegexSearchResult;
  AIterator: TdxBufferedDocumentCharacterIteratorForward;
  ARunInfo: TdxRunInfo;
begin
  AResult := ASearcher.Match(AStart, AEnd);
  if AResult = nil then
    Exit(TdxSearchResult.Create);

  AIterator := TdxBufferedDocumentCharacterIteratorForward.Create(AResult.Offset);
  try
    ARunInfo := TdxRunInfo.Create(PieceTable);
    ARunInfo.Start.CopyFrom(AIterator.GetPosition(AResult.Match.Index));
    ARunInfo.&End.CopyFrom(AIterator.GetPosition(ARunInfo.Start, AResult.Match.Length));
    Result := TdxRegexSearchResult.Create(ARunInfo, AResult);
  finally
    AIterator.Free;
  end;
end;

{ TdxTextSearchByRegexForwardStrategy }

function TdxTextSearchByRegexForwardStrategy.CreateIntervalCalculator: TdxSearchIntervalCalculator;
begin
  Result := TdxSearchIntervalCalculatorForward.Create;
end;

function TdxTextSearchByRegexForwardStrategy.CreateSearcher(const APattern: string; AOptions: TdxSearchOptions): TdxBufferedRegexSearchBase;
var
  AIgnoreCase: Boolean;
  ARegexOptions: TRegexOptions;
  ARegex: TRegex;
  ABufferSize: Integer;
begin
  AIgnoreCase := not (TdxSearchOption.CaseSensitive in AOptions);
  if AIgnoreCase then
    ARegexOptions := [roIgnoreCase, roMultiLine]
  else
    ARegexOptions := [roMultiLine];
  ARegex := TRegex.Create(APattern, ARegexOptions);
{$IFNDEF DELPHIBERLIN}
  ARegex.SetState([]);
{$ENDIF}
  ABufferSize := PieceTable.DocumentModel.SearchOptions.RegExResultMaxGuaranteedLength;
  Result := TdxBufferedRegexSearchForward.Create(PieceTable, ARegex, ABufferSize);
end;

{ TdxTextSearchByRegexBackwardStrategy }

function TdxTextSearchByRegexBackwardStrategy.CreateIntervalCalculator: TdxSearchIntervalCalculator;
begin
  Result := TdxSearchIntervalCalculatorBackward.Create;
end;

function TdxTextSearchByRegexBackwardStrategy.CreateSearcher(const APattern: string; AOptions: TdxSearchOptions): TdxBufferedRegexSearchBase;
var
  AIgnoreCase: Boolean;
  ARegexOptions: TRegexOptions;
  ARegex: TRegex;
  ABufferSize: Integer;
begin
  AIgnoreCase := not (TdxSearchOption.CaseSensitive in AOptions);
  if AIgnoreCase then
    ARegexOptions := [roIgnoreCase]
  else
    ARegexOptions := [roNone];
  ARegex := TRegex.Create(APattern, ARegexOptions);
{$IFNDEF DELPHIBERLIN}
  ARegex.SetState([]);
{$ENDIF}
  ABufferSize := PieceTable.DocumentModel.SearchOptions.RegExResultMaxGuaranteedLength;
  Result := TdxBufferedRegexSearchBackward.Create(PieceTable, ARegex, ABufferSize);
end;

{ TdxNativeSearchResultBase }

constructor TdxNativeSearchResultBase.Create(ADocument: TdxNativeSubDocument;
  const AFindText: string; ASearchOptions: TdxRichEditSearchOptions;
  const ARange: IdxRichEditDocumentRange);
begin
  inherited Create;
  FDocument := ADocument;
  FFindText := AFindText;
  FOptions := ASearchOptions;
  FRange := ARange;
  FTextSearch := CreateTextSearch;
  SetInitialState;
end;

destructor TdxNativeSearchResultBase.Destroy;
begin
  FreeAndNil(FTextSearch);
  inherited Destroy;
end;

function TdxNativeSearchResultBase.GetCurrentResult: IdxRichEditDocumentRange;
begin
  Result := FCurrentResult;
end;

procedure TdxNativeSearchResultBase.SetInitialState;
begin
  FCurrentResult := nil;
  FStartPosition := 0;
  FEndPosition := MaxInt;
end;

function TdxNativeSearchResultBase.FindNext: Boolean;
var
  ARunInfo: TdxRunInfo;
  AResult: TdxSearchResult;
begin
  UpdateSearchInterval;
  if FStartPosition > FEndPosition then
    Exit(False);
  AResult := FTextSearch.Match(FindText, FOptions, FStartPosition, FEndPosition);
  try
    ARunInfo := AResult.Value;
    try
      if ARunInfo = nil then
        Exit(False);
      FCurrentResult := CreateDocumentRange(ARunInfo);
      ChangeSearchInterval(ARunInfo);
      Result := True;
    finally
      ARunInfo.Free;
    end;
  finally
    AResult.Free;
  end;
end;

procedure TdxNativeSearchResultBase.Replace(const AReplaceWith: string);
var
  ADeltaLength: Integer;
begin
  if (CurrentResult = nil) or (CurrentResult.Length = 0) then
    Exit;
  ADeltaLength := Document.Replace(CurrentResult, AReplaceWith);
  ChangeSearchIntervalAfterReplace(ADeltaLength);
end;

function TdxNativeSearchResultBase.CreateDocumentRange(ARangeInfo: TdxRunInfo): IdxRichEditDocumentRange;
var
  APosition: IdxRichEditDocumentPosition;
begin
  APosition := TdxNativeDocumentPosition.Create(Document, ARangeInfo.Start);
  Result := FDocument.CreateRange(APosition, ARangeInfo.&End.LogPosition - ARangeInfo.Start.LogPosition);
end;

procedure TdxNativeSearchResultBase.Reset;
begin
  SetInitialState;
end;

{ TdxNativeSearchResultForward }

procedure TdxNativeSearchResultForward.UpdateSearchInterval;
begin
  StartPosition := Max(StartPosition, Range.Start.LogPosition);
  EndPosition := Min(Range.&End.LogPosition, Document.PieceTable.DocumentEndLogPosition);
end;

procedure TdxNativeSearchResultForward.ChangeSearchInterval(ARangeInfo: TdxRunInfo);
begin
  StartPosition := ARangeInfo.Start.LogPosition + 1;
end;

function TdxNativeSearchResultForward.CreateTextSearch: TdxSearchStrategy;
begin
  Result := TdxTextSearchByStringForwardStrategy.Create(Document.PieceTable);
end;

procedure TdxNativeSearchResultForward.ChangeSearchIntervalAfterReplace(ADeltaLength: Integer);
begin
  StartPosition := StartPosition + ADeltaLength;
end;

{ TdxNativeSearchResultBackward }

procedure TdxNativeSearchResultBackward.UpdateSearchInterval;
var
  AInitialEnd: TdxDocumentLogPosition;
begin
  StartPosition := Range.Start.LogPosition;
  AInitialEnd := Min(Range.&End.LogPosition, Document.PieceTable.DocumentEndLogPosition);
  EndPosition := Min(EndPosition, AInitialEnd);
end;

procedure TdxNativeSearchResultBackward.ChangeSearchInterval(ARangeInfo: TdxRunInfo);
begin
  EndPosition := ARangeInfo.&End.LogPosition - 1;
end;

function TdxNativeSearchResultBackward.CreateTextSearch: TdxSearchStrategy;
begin
  Result := TdxTextSearchByStringBackwardStrategy.Create(Document.PieceTable);
end;

procedure TdxNativeSearchResultBackward.ChangeSearchIntervalAfterReplace(ADeltaLength: Integer);
begin
  StartPosition := CurrentResult.Start.LogPosition - 1;
end;

{ TdxNativeGroup }

constructor TdxNativeGroup.Create(ADocument: TdxNativeSubDocument;
  APosition: TdxDocumentLogPosition; const AGroup: TGroup);
begin
  inherited Create;
  FDocument := ADocument;
  FPosition := APosition;
  FGroup := AGroup;
end;

function TdxNativeGroup.GetPosition: IdxRichEditDocumentPosition;
begin
  Result := Document.CreatePositionCore(LogPosition);
end;

function TdxNativeGroup.GetValue: string;
begin
  Result := FGroup.Value;
end;

function TdxNativeGroup.GetRange: IdxRichEditDocumentRange;
begin
  if FRange = nil then
    FRange := FDocument.CreateRange(Position, Length);
  Result := FRange;
end;

function TdxNativeGroup.GetLength: Integer;
begin
  Result := FGroup.Length;
end;

function TdxNativeGroup.ToString: string;
begin
  Result := Value;
end;

{ TdxNativeGroupCollection }

constructor TdxNativeGroupCollection.Create(const AMatch: TdxNativeMatch);
begin
  inherited Create;
  FMatch := AMatch;
end;

function TdxNativeGroupCollection.GetCount: Integer;
begin
  Result := FMatch.Match.Groups.Count;
end;

function TdxNativeGroupCollection.GetItem(
  Index: Integer): IdxRichEditRegexSearchGroup;
begin
  Result := FMatch.CreateGroup(FMatch.Match.Groups[Index]);
end;

{ TdxNativeMatch }

constructor TdxNativeMatch.Create(
  ADocument: TdxNativeSubDocument; APosition: TdxDocumentLogPosition;
  const AOffset: TdxDocumentModelPosition; const AMatch: TMatch);
begin
  inherited Create;
  FDocument := ADocument;
  FLogPosition := APosition;
  FOffset := AOffset;
  FMatch := AMatch;
  FGroups := TdxNativeGroupCollection.Create(Self);
end;

function TdxNativeMatch.CreateGroup(const AGroup: TGroup): IdxRichEditRegexSearchGroup;
var
  APosition: TdxDocumentLogPosition;
begin
  APosition := TdxNativeRegexSearch.CreatePosition(FOffset, AGroup.Index);
  Result := TdxNativeGroup.Create(FDocument, APosition, AGroup);
end;

function TdxNativeMatch.GetGroups: IdxRichEditRegexSearchGroupCollection;
begin
  Result := FGroups;
end;

function TdxNativeMatch.Result(const AReplacement: string): string;
begin
  Result := FMatch.Result(AReplacement);
end;

function TdxNativeMatch.GetLength: Integer;
begin
  Result := FMatch.Length;
end;

function TdxNativeMatch.GetPosition: IdxRichEditDocumentPosition;
begin
  Result := FDocument.CreatePositionCore(FLogPosition);
end;

function TdxNativeMatch.GetValue: string;
begin
  Result := FMatch.Value;
end;

function TdxNativeMatch.GetRange: IdxRichEditDocumentRange;
begin
  if FRange = nil then
    FRange := FDocument.CreateRange(Position, Length);
  Result := FRange;
end;

{ TdxNativeRegexSearch }

constructor TdxNativeRegexSearch.Create(ADocument: TdxNativeSubDocument;
  const ARegex: TRegEx; AMaxGuaranteedSearchResultLength: Integer);
begin
  inherited Create;
  FDocument := ADocument;
  FRegex := ARegex;
  FMaxGuaranteedSearchResultLength := AMaxGuaranteedSearchResultLength;
end;

function TdxNativeRegexSearch.Match(const ARange: IdxRichEditDocumentRange): IdxRichEditRegexSearchMatch;
var
  ARegexSearch: TdxBufferedRegexSearchForward;
  AStartPosition, AEndPosition: TdxDocumentLogPosition;
  AResult: TdxBufferedRegexSearchResult;
begin
  if ARange.Length = 0 then
    Exit(nil);
  ARegexSearch := CreateRegexSearch;
  try
    AStartPosition := ARange.Start.LogPosition;
    AEndPosition := Min(ARange.&End.LogPosition, Document.PieceTable.DocumentEndLogPosition);
    AResult := ARegexSearch.Match(AStartPosition, AEndPosition);
    try
      if AResult = nil then
        Exit(nil);
      Result := CreateMatch(AResult.Offset, AResult.Match);
    finally
      AResult.Free;
    end;
  finally
    ARegexSearch.Free;
  end;
end;

function TdxNativeRegexSearch.Matches(const ARange: IdxRichEditDocumentRange): TArray<IdxRichEditDocumentRange>;
var
  ARegexSearch: TdxBufferedRegexSearchForward;
  AStartPosition, AEndPosition: TdxDocumentLogPosition;
  AResults: TdxBufferedRegexSearchResultCollection;
  ARanges: TList<IdxRichEditDocumentRange>;
  ACount, I: Integer;
  AResult: TdxBufferedRegexSearchResult;
  AIterator: TdxBufferedDocumentCharacterIteratorForward;
  AStart, AEnd: TdxDocumentModelPosition;
begin
  ARegexSearch := CreateRegexSearch;
  try
    AStartPosition := ARange.Start.LogPosition;
    AEndPosition := Min(ARange.&End.LogPosition, Document.PieceTable.DocumentEndLogPosition);
    AResults := ARegexSearch.Matches(AStartPosition, AEndPosition);
    try
      ARanges := TList<IdxRichEditDocumentRange>.Create;
      try
        ACount := AResults.Count;
        for I := 0 to ACount - 1 do
        begin
          AResult := AResults[I];
          AIterator := TdxBufferedDocumentCharacterIteratorForward.Create(AResult.Offset);
          try
            AStart := AIterator.GetPosition(AResult.Match.Index);
            AEnd := AIterator.GetPosition(AStart, AResult.Match.Length);
            ARanges.Add(TdxNativeDocumentRange.Create(FDocument, AStart, AEnd));
          finally
            AIterator.Free;
          end;
        end;
        Result := ARanges.ToArray;
      finally
        ARanges.Free;
      end;
    finally
      AResults.Free;
    end;
  finally
    ARegexSearch.Free;
  end;
end;

function TdxNativeRegexSearch.NextMatch(const AMatch: IdxRichEditRegexSearchMatch;
  const ARange: IdxRichEditDocumentRange): IdxRichEditRegexSearchMatch;
var
  APrevResult, ANewRange: IdxRichEditDocumentRange;
  ALength: Integer;
begin
  APrevResult := AMatch.GetRange;
  ALength := ARange.&End.LogPosition - APrevResult.&End.LogPosition;
  ANewRange := Document.CreateRange(APrevResult.&End, ALength);
  Result := Match(ANewRange);
end;

function TdxNativeRegexSearch.CreateMatch(
  const AOffset: TdxDocumentModelPosition; const AMatch: TMatch): IdxRichEditRegexSearchMatch;
var
  APosition: TdxDocumentLogPosition;
begin
  APosition := CreatePosition(AOffset, AMatch.Index);
  Result := TdxNativeMatch.Create(FDocument, APosition, AOffset, AMatch);
end;

function TdxNativeRegexSearch.CreateRegexSearch: TdxBufferedRegexSearchForward;
begin
  Result := TdxBufferedRegexSearchForward.Create(Document.PieceTable,
    Regex, MaxGuaranteedSearchResultLength);
end;

class function TdxNativeRegexSearch.CreatePosition(
  const APos: TdxDocumentModelPosition; AOffset: Integer): TdxDocumentLogPosition;
var
  AIterator: TdxBufferedDocumentCharacterIteratorForward;
begin
  AIterator := TdxBufferedDocumentCharacterIteratorForward.Create(APos);
  try
    Result := AIterator.GetPosition(AOffset).LogPosition;
  finally
    AIterator.Free;
  end;
end;

{ TdxNativeRegexSearchResult }

constructor TdxNativeRegexSearchResult.Create(ADocument: TdxNativeSubDocument;
  const ARegex: TRegEx; const ARange: IdxRichEditDocumentRange; AMaxGuaranteedSearchResultLength: Integer);
begin
  inherited Create;
  FDocument := ADocument;
  FRange := ARange;
  FMaxGuaranteedSearchResultLength := AMaxGuaranteedSearchResultLength;
  FRegexSearch := CreateRegexSearch(ADocument, ARegex);
end;

destructor TdxNativeRegexSearchResult.Destroy;
begin
  FreeAndNil(FRegexSearch);
  inherited Destroy;
end;

function TdxNativeRegexSearchResult.CreateRegexSearch(
  ADocument: TdxNativeSubDocument; const ARegex: TRegEx): TdxNativeRegexSearch;
begin
  Result := TdxNativeRegexSearch.Create(ADocument, ARegex, MaxGuaranteedSearchResultLength)
end;

function TdxNativeRegexSearchResult.GetCurrentResult: IdxRichEditDocumentRange;
begin
  if FMatch = nil then
    Exit(nil);
  Result := FMatch.GetRange;
end;

function TdxNativeRegexSearchResult.GetMatch: IdxRichEditRegexSearchMatch;
begin
  Result := FMatch;
end;

procedure TdxNativeRegexSearchResult.Reset;
begin
  FMatch := nil;
end;

function TdxNativeRegexSearchResult.FindNext: Boolean;
var
  AResult: IdxRichEditRegexSearchMatch;
begin
  if (FMatch <> nil) then
    AResult := FRegexSearch.NextMatch(FMatch, FRange)
  else
    AResult := FRegexSearch.Match(FRange);
  if AResult = nil then
    Exit(False);
  FMatch := AResult;
  Result := True;
end;

procedure TdxNativeRegexSearchResult.Replace(const AReplaceWith: string);
var
  AResult: string;
begin
  if FMatch = nil then
    Exit;
  AResult := TdxNativeMatch(FMatch).Result(AReplaceWith);
  FDocument.Replace(CurrentResult, AResult);
end;

end.
