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

unit dxSpellChecker;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Messages, Classes, Graphics,
  Controls, Forms, StdCtrls, RichEdit, Menus, Generics.Defaults, Generics.Collections,
  dxCoreClasses, cxClasses, dxCore, cxControls, cxLookAndFeels,
  dxSpellCheckerCore, cxEdit, cxTextEdit, cxRichEdit, dxSpellCheckerRules,
  dxSpellCheckerAlgorithms, dxHunspellTypes, dxSpellCheckerTextParsers, dxSpellCheckerAdapters, SyncObjs;

const
  dxDefaultSuggestionsTimeout = 250;

type
  TdxCustomSpellChecker = class;
  TdxCustomSpellCheckerDictionary = class;
  TdxSpellCheckerAutoCorrectOptions = class;
  TdxSpellCheckerCheckAsYouTypeOptions = class;
  TdxSpellCheckerCustomAutoCorrectManager = class;
  TdxSpellCheckerSpellingOptions = class;
  TdxSpellCheckerSuggestionBuilder = class;
  TdxSpellCheckerSuggestionList = class;
  TdxSpellCheckerWordList = class;

  EdxSpellCheckerException = class(EdxException);

  PdxSpellCheckerWordStruct = ^TdxSpellCheckerWordStruct;
  TdxSpellCheckerWordStruct = packed record
    Size: Cardinal;
    NextMetaphone: Pointer;
    Next: Pointer;
    Length: Cardinal;
  end;

  { TdxSpellCheckerPersistent }

  TdxSpellCheckerPersistent = class(TPersistent)
  strict private
    FSpellChecker: TdxCustomSpellChecker;
  public
    constructor Create(ASpellChecker: TdxCustomSpellChecker); virtual;
    property SpellChecker: TdxCustomSpellChecker read FSpellChecker;
  end;

  { TdxSpellCheckerSuggestion }

  TdxSpellCheckerSuggestion = class
  private
    FDictionary: TdxCustomSpellCheckerDictionary;
    FDistance: Integer;
    FOrder: Integer;
    FWord: string;
  public
    constructor Create(const AWord: string; ADictionary: TdxCustomSpellCheckerDictionary; ADistance: Integer);

    property Dictionary: TdxCustomSpellCheckerDictionary read FDictionary;
    property Distance: Integer read FDistance;
    property Word: string read FWord write FWord;
  end;

  { TdxSpellCheckerSuggestionList }

  TdxSpellCheckerSuggestionList = class(TcxObjectList)
  strict private
    function GetItem(Index: Integer): TdxSpellCheckerSuggestion;
  public
    procedure Add(const AWord: string; ADictionary: TdxCustomSpellCheckerDictionary; ADistance: Integer);
    procedure Assign(ASuggestions: TdxSpellCheckerSuggestionList);
    procedure Delete(AIndex: Integer);
    procedure FixCapitalization(const AMask: string);
    procedure Insert(AIndex: Integer; const AWord: string; ADictionary: TdxCustomSpellCheckerDictionary; ADistance: Integer);
    procedure RemoveDuplicates;
    procedure SortByDistance;
    procedure SortByWord;
    procedure SaveToStrings(AStrings: TStrings);

    property Items[Index: Integer]: TdxSpellCheckerSuggestion read GetItem; default;
  end;

  { TdxSpellCheckerSuggestionCacheItem }

  TdxSpellCheckerSuggestionCacheItem = class
  private
    FRefCount: Integer;
    FSuggestions: TdxSpellCheckerSuggestionList;
    FWord: string;
  protected
    property RefCount: Integer read FRefCount;
    property Word: string read FWord;
  public
    constructor Create(const AWord: string; ASuggestions: TdxSpellCheckerSuggestionList);
    destructor Destroy; override;
    procedure GetSuggestions(ASuggestions: TdxSpellCheckerSuggestionList);
  end;

  { TdxSpellCheckerSuggestionCache }

  TdxSpellCheckerSuggestionCache = class(TcxObjectList)
  strict private
    FMaxCapacity: Integer;

    function GetItem(Index: Integer): TdxSpellCheckerSuggestionCacheItem;
    function GetRareItemIndex: Integer;
  protected
    function Find(const AWord: string): TdxSpellCheckerSuggestionCacheItem;
  public
    constructor Create(AMaxCapacity: Integer);
    procedure Add(const AWord: string; ASuggestions: TdxSpellCheckerSuggestionList);
    function GetSuggestions(const AWord: string; ASuggestions: TdxSpellCheckerSuggestionList): Boolean;
    procedure Reset;

    property Items[Index: Integer]: TdxSpellCheckerSuggestionCacheItem read GetItem; default;
  end;

  { TdxSpellCheckerSingleWordParser }

  TdxSpellCheckerSingleWordParser = class(TcxIUnknownObject, IdxSpellCheckerCheckModeHelper)
  strict private
    FSpellChecker: TdxCustomSpellChecker;
  protected
    property SpellChecker: TdxCustomSpellChecker read FSpellChecker;
  public
    constructor Create(ASpellChecker: TdxCustomSpellChecker); virtual;
    destructor Destroy; override;
    // IdxSpellCheckerCheckModeHelper
    function GetPrevWord: string; virtual;
    function WordExists(const AWord: string): Boolean; virtual;
  end;

  { TdxSpellCheckerTextParser }

  TdxSpellCheckerSentence = record
    MisspelledWordPositionFinish: IdxSpellCheckerPosition;
    MisspelledWordPositionInText: Integer;
    MisspelledWordPositionStart: IdxSpellCheckerPosition;
    TextPositionFinish: IdxSpellCheckerPosition;
    TextPositionStart: IdxSpellCheckerPosition;
    Text: string;
  end;

  { TdxSpellCheckerUndoItem }

  TdxSpellCheckerUndoItem = class
  strict private
    FMisspelledWord: string;
    FMisspelledWordFinish: IdxSpellCheckerPosition;
    FMisspelledWordStart: IdxSpellCheckerPosition;
    FPrevWord: string;
    FReplacement: string;
    FReplacementFinish: IdxSpellCheckerPosition;
  public
    constructor Create(const AMisspelledWord, AReplacement, APrevWord: string;
      const AMisspelledWordStart, AMisspelledWordFinish, AReplacementFinish: IdxSpellCheckerPosition);

    property MisspelledWord: string read FMisspelledWord;
    property MisspelledWordFinish: IdxSpellCheckerPosition read FMisspelledWordFinish;
    property MisspelledWordStart: IdxSpellCheckerPosition read FMisspelledWordStart;
    property PrevWord: string read FPrevWord;
    property Replacement: string read FReplacement;
    property ReplacementFinish: IdxSpellCheckerPosition read FReplacementFinish;
    property ReplacementStart: IdxSpellCheckerPosition read FMisspelledWordStart;
  end;

  { TdxSpellCheckerUndoManager }

  TdxSpellCheckerUndoManager = class(TPersistent)
  strict private
    FUndoList: TcxObjectList;

    function GetCount: Integer;
    function GetItem(Index: Integer): TdxSpellCheckerUndoItem;
    function GetLast: TdxSpellCheckerUndoItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const AMisspelledWord, AReplacement, APrevWord: string;
      const AMisspelledWordStart, AMisspelledWordFinish, AReplacementFinish: IdxSpellCheckerPosition);
    procedure UndoLast;

    property Count: Integer read GetCount;
    property Last: TdxSpellCheckerUndoItem read GetLast;
    property Items[Index: Integer]: TdxSpellCheckerUndoItem read GetItem; default;
  end;

  { TdxSpellCheckerAbstractCheckMode }

  TdxSpellCheckerAbstractCheckMode = class(TInterfacedPersistent, IdxSpellCheckerCheckModeHelper)
  strict private
    FMeaningfulCharacters: string;
    FPrevWord: string;
    FSpellChecker: TdxCustomSpellChecker;
  protected
    FController: IdxSpellCheckTextController;
    FIgnoreOnceList: IdxSpellCheckerIgnoreList;
    FLastCode: TdxSpellCheckerSpellingCode;
    FMisspelledWord: string;
    FMisspellingFinish: IdxSpellCheckerPosition;
    FMisspellingStart: IdxSpellCheckerPosition;
    FSpellingFinish: IdxSpellCheckerPosition;
    FSpellingStart: IdxSpellCheckerPosition;

    function CheckWord(const AWord: string): TdxSpellCheckerSpellingCode; virtual;
    function GetNextWord(out AWord: string): Boolean; virtual;
    function GetOwner: TPersistent; override;
    function InternalCheckWord(var AWord: string): TdxSpellCheckerSpellingCode; virtual;
    function InternalProcessWord(const AWord: string): Boolean; virtual;
    function IsNeedIgnoreWord(const AWord: string): Boolean; virtual;
    function IsValidWord(const AWord: string): Boolean; virtual;
    procedure UpdateMeaningfulCharacters; virtual;
    function ValidateSpellingBounds: Boolean; virtual;

    // IdxSpellCheckerCheckModeHelper
    function GetPrevWord: string;
    function WordExists(const AWord: string): Boolean;

    property IgnoreOnceList: IdxSpellCheckerIgnoreList read FIgnoreOnceList;
    property MeaningfulCharacters: string read FMeaningfulCharacters;
    property PrevWord: string read FPrevWord write FPrevWord;
  public
    constructor Create(AOwner: TdxCustomSpellChecker);
    destructor Destroy; override;
    function GetNextMisspelledWord: Boolean; virtual;
    procedure Skip; virtual;

    property Controller: IdxSpellCheckTextController read FController;
    property LastCode: TdxSpellCheckerSpellingCode read FLastCode;
    property MisspelledWord: string read FMisspelledWord;
    property SpellChecker: TdxCustomSpellChecker read FSpellChecker;
  end;

  { TdxSpellCheckerCustomCheckMode }

  TdxSpellCheckerCustomCheckMode = class(TdxSpellCheckerAbstractCheckMode)
  strict private
    FAdapter: IdxSpellCheckerAdapter;
  protected
    procedure CreateTextController; virtual;
    function InternalProcessWord(const AWord: string): Boolean; override;
    function IsNeedChangeWord(const AWord: string; out AReplacement: string): Boolean; virtual;
    function IsNeedDeleteWord(const AWord: string): Boolean; virtual;
    procedure ReplaceWord(var AStart, AFinish: IdxSpellCheckerPosition; const AWord, APrevWord: string);
    procedure RestoreSelection; virtual;
    procedure StoreSelection; virtual;
    procedure SelectMisspelledWord; virtual;
    procedure UpdateByDictionary(ADictionary: TdxCustomSpellCheckerDictionary); virtual;
    procedure UpdateTextInfo; virtual;

    property Adapter: IdxSpellCheckerAdapter read FAdapter;
  public
    constructor Create(AOwner: TdxCustomSpellChecker; AAdapter: IdxSpellCheckerAdapter); virtual;
    destructor Destroy; override;

    procedure Add; virtual;
    function CanUndo: Boolean; virtual;
    procedure Change(const AWord: string); virtual;
    procedure ChangeAll(const AWord: string); virtual;
    procedure Delete; virtual;
    procedure DeleteAll; virtual;
    function GetSuggestions(const AWord: string): TdxSpellCheckerSuggestionList; virtual;
    function GetNextMisspelledWord: Boolean; override;
    procedure Ignore; virtual;
    procedure IgnoreAll; virtual;
    procedure UndoLast; virtual;
  end;

  TdxSpellCheckerCustomCheckModeClass = class of TdxSpellCheckerCustomCheckMode;

  { TdxSpellCheckerDialogCheckMode }

  TdxSpellCheckerDialogCheckMode = class(TdxSpellCheckerCustomCheckMode)
  strict private
    FCheckedRange: Integer;
    FSaveHideSelection: Boolean;
    FSaveSelFinish: IdxSpellCheckerPosition;
    FSaveSelStart: IdxSpellCheckerPosition;
  protected
    FUndoManager: TdxSpellCheckerUndoManager;

    procedure CreateTextController; override;
    function IsCheckingSelectedText: Boolean; virtual;
    procedure RestoreSelection; override;
    procedure StoreSelection; override;
    function ValidateSpellingBounds: Boolean; override;
    procedure ValidateSpellingBoundsAfterUndo;

    property UndoManager: TdxSpellCheckerUndoManager read FUndoManager;
  public
    constructor Create(AOwner: TdxCustomSpellChecker; AAdapter: IdxSpellCheckerAdapter); override;
    destructor Destroy; override;

    function CanUndo: Boolean; override;
    procedure Change(const AWord: string); override;
    procedure Ignore; override;
    function ShowDialog: Integer; virtual;
    procedure UndoLast; override;
  end;

  { TdxSpellCheckerOutlookCheckMode }

  TdxSpellCheckerOutlookCheckMode = class(TdxSpellCheckerDialogCheckMode)
  public
    function ShowDialog: Integer; override;
  end;

  { TdxSpellCheckerWordCheckMode }

  TdxSpellCheckerWordCheckMode = class(TdxSpellCheckerDialogCheckMode)
  strict private
    function GetMisspelledSentence: TdxSpellCheckerSentence;
  public
    procedure ChangeSentence(const ASentence: string); virtual;
    function ShowDialog: Integer; override;

    property MisspelledSentence: TdxSpellCheckerSentence read GetMisspelledSentence;
  end;

  { TdxSpellCheckerWordList }

  TdxMetaphoneTable = array[Word] of PWideChar;
  PdxMetaphoneTable = ^TdxMetaphoneTable;

  TdxSpellCheckerWordList = class
  strict private
    FCount: Integer;
    FFindMostValidCapitalizationInDuplicates: Boolean;
    FLangID: Cardinal;
    FLock: TRTLCriticalSection;
    FMetaphone: TdxDoubleMetaphone;
    FMetaphoneTable: PdxMetaphoneTable;
    FTable: PdxPWideCharArray;
    FTableSize: Integer;
    FUpperCaseBuffer: array[0..256] of WideChar;
    FUseDoubleMetaphone: Boolean;

    function GetCodePage: Cardinal;
    function GetWordStruct(AWord: PWideChar): Pointer; inline;
    procedure InitMetaphoneTable;
    procedure SetUseDoubleMetaphone(AValue: Boolean);
    procedure UpdateMetaphoneInfo(ANewWord: PWideChar; ALength: Integer);
    procedure UpdateWordsMetaphone;
  protected
    function ElfHash(P: PWideChar; ALength: Integer): Integer;
    function FindWord(const S: string): PWideChar;
    function NewWord(S: PWideChar; ALength: Integer): PWideChar; inline;
    function AllocWord(ACharCount: Cardinal): PWideChar;
    procedure DisposeWord(AWord: PWideChar);
    function GetNextMetaphoneWord(AWord: PWideChar): Pointer; inline;
    function GetNextWord(AWord: PWideChar): Pointer; inline;
    procedure SetNextMetaphoneWord(AWord: PWideChar; AValue: Pointer); inline;
    procedure SetNextWord(AWord: PWideChar; AValue: Pointer); inline;
    function WordLength(AWord: PWideChar): Integer; inline;

    property CodePage: Cardinal  read GetCodePage;
  public
    constructor Create(ALangID: Cardinal; ATableSize: Integer);
    destructor Destroy; override;
    procedure Add(const S: string);
    procedure Clear;
    function Find(const S: string): Boolean;
    procedure LoadFromStrings(AStrings: TStrings);
    procedure PopulateMetaphoneSuggestions(ASuggestionBuilder: TdxSpellCheckerSuggestionBuilder; const AWord: string);
    procedure SaveToStrings(AStrings: TStrings);

    property Count: Integer read FCount;
    property FindMostValidCapitalizationInDuplicates: Boolean read FFindMostValidCapitalizationInDuplicates write FFindMostValidCapitalizationInDuplicates;
    property LangID: Cardinal read FLangID write FLangID;
    property UseDoubleMetaphone: Boolean read FUseDoubleMetaphone write SetUseDoubleMetaphone;
  end;

  { TdxSpellCheckerSuggestionBuilder }

  TdxSpellCheckerSuggestionBuilder = class
  strict private
    FAlphabet: string;
    FDictionary: TdxCustomSpellCheckerDictionary;
    FOriginalWord: string;
    FStartTicks: Cardinal;
    FSuggestions: TdxSpellCheckerSuggestionList;
    FWord: string;

    function GetTimeout: Cardinal;
    procedure SetAlphabet(const AValue: string);
    function GetMaxDistance: Integer;
    function GetSimilarity: TdxStringSimilarityCalculator;
  protected
    function CanAddToSuggestions(const ATestWord: string): Boolean; virtual;
    procedure CheckAddMetaphoneSuggestion(ATestWord, AUpperWord1: PWideChar;
      AUpperWordLen1: Integer; AUpperWord2: PWideChar; AUpperWordLen2: Integer);
    procedure PopulateCapitalizationSuggestions;
    procedure DoAddMetaphoneSuggestions; virtual;
    procedure DoAddSuggestions; virtual;
    function IsCaseSensitive: Boolean; virtual;
    function IsTimeOut: Boolean;
    function PrepareWord(const AWord: string): string; virtual;

    property Alphabet: string read FAlphabet write SetAlphabet;
    property Dictionary: TdxCustomSpellCheckerDictionary read FDictionary;
    property MaxDistance: Integer read GetMaxDistance;
    property OriginalWord: string read FOriginalWord;
    property Similarity: TdxStringSimilarityCalculator read GetSimilarity;
    property Suggestions: TdxSpellCheckerSuggestionList read FSuggestions write FSuggestions;
    property Timeout: Cardinal read GetTimeout;
    property Word: string read FWord write FWord;
  public
    constructor Create(ADictionary: TdxCustomSpellCheckerDictionary); virtual;
    procedure AddSuggestions(const AWord: string; ASuggestions: TdxSpellCheckerSuggestionList); virtual;
  end;

  { TdxNearMissStrategy }

  TdxNearMissStrategy = class(TdxSpellCheckerSuggestionBuilder)
  protected
    procedure CheckAddToSuggestions(const ATestWord: string);
    procedure CheckChangeOneLetter;
    procedure CheckDeleteLetter;
    procedure CheckDoubleTwoChars;
    procedure CheckInsertLetter;
    procedure CheckInsertSpace;
    procedure CheckMoveChar;
    procedure DoAddSuggestions; override;
    procedure InterchangeTwoLetters;
    procedure LongInterchangeTwoLetters;
  end;

  { TdxDictionaryLoadThread }

  TdxDictionaryLoadThread = class(TcxThread)
  private
    FDictionary: TdxCustomSpellCheckerDictionary;
    FFinished: Boolean;
  protected
    procedure Execute; override;

    property Dictionary: TdxCustomSpellCheckerDictionary read FDictionary;
  public
    constructor Create(ADictionary: TdxCustomSpellCheckerDictionary);
    function IsLoadComplete: Boolean;

    property Finished: Boolean read FFinished;
  end;

  { TdxCustomSpellCheckerDictionary }

  TdxSpellCheckerDictionaryLoadingEvent = procedure (Sender: TdxCustomSpellCheckerDictionary; var AHandled: Boolean) of object;
  TdxSpellCheckerDictionaryLoadedEvent = procedure (Sender: TdxCustomSpellCheckerDictionary) of object;
  TdxSpellCheckerDictionaryLoadMode = (dlmDefault, dlmDirectLoad, dlmThreadedLoad);

  TdxCustomSpellCheckerDictionary = class(TdxSpellCheckerPersistent)
  strict private
    FAlphabet: string;
    FCodePage: Cardinal;
    FEnabled: Boolean;
    FLanguage: DWORD;
    FLoadThread: TdxDictionaryLoadThread;
    FTerminated: Boolean;
    FTimeout: Cardinal;

    FOnLoading: TdxSpellCheckerDictionaryLoadingEvent;
    FOnLoaded: TdxSpellCheckerDictionaryLoadedEvent;

    function GetActive: Boolean;
    procedure SetEnabled(AValue: Boolean);
    procedure SetCodePage(AValue: Cardinal);
    procedure SetLanguage(const AValue: DWORD);
  protected
    FLoaded: Boolean;

    procedure AfterLoad; virtual;
    procedure BeforeLoad; virtual;
    function CanLoad: Boolean; virtual;
    function CanUseDoubleMetaphone: Boolean; virtual;
    procedure Cleanup; virtual;
    function CreateSuggestionBuilder: TdxSpellCheckerSuggestionBuilder; virtual;
    procedure DirectLoad; virtual;
    procedure DoActivate; virtual;
    function DoLoad: Boolean; virtual;
    procedure DoLoadedEvent; virtual;
    function DoLoadingEvent: Boolean; virtual;
    function DoUnload: Boolean; virtual;
    function ExecuteLoad: Boolean; virtual;
    procedure FreeContent; virtual;
    function GetActiveAlphabet: string; virtual;
    function GetDisplayName: string; virtual;
    procedure InitializeContent; virtual;
    function IsEnglishLanguage: Boolean;
    procedure LanguageChanged; virtual;
    procedure LoadingComplete; virtual;
    function LoadingTerminated: Boolean; inline;
    procedure LoadUsingThread; virtual;
    procedure PopulateCapitalizationSuggestions(ASuggestionBuilder: TdxSpellCheckerSuggestionBuilder; const AWord: string); virtual;
    procedure PopulateMetaphoneSuggestions(ASuggestionBuilder: TdxSpellCheckerSuggestionBuilder; const AWord: string); virtual;
    procedure Reload;
    procedure Terminate;
    procedure ThreadDone(Sender: TObject); virtual;
    procedure Update; virtual;
    procedure UpdateLoadedOnLoadEvent;
    procedure UpdateUsingMetaphone; virtual;
    procedure UpdateWordChars(var AWordChars: string); virtual;

    property LoadThread: TdxDictionaryLoadThread read FLoadThread;
    property Timeout: Cardinal read FTimeout write FTimeout;
  public
    constructor Create(ASpellChecker: TdxCustomSpellChecker); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Activate;
    function HasWord(const AWord: string): Boolean; virtual;

    procedure Clear;
    procedure Load(AMode: TdxSpellCheckerDictionaryLoadMode = dlmDefault);
    procedure Unload;

    property Active: Boolean read GetActive;
    property Alphabet: string read FAlphabet write FAlphabet;
    property CodePage: Cardinal read FCodePage write SetCodePage default CP_ACP;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Language: DWORD read FLanguage write SetLanguage default 0;
    property Loaded: Boolean read FLoaded;

    property OnLoaded: TdxSpellCheckerDictionaryLoadedEvent read FOnLoaded write FOnLoaded;
    property OnLoading: TdxSpellCheckerDictionaryLoadingEvent read FOnLoading write FOnLoading;
  end;

  TdxCustomSpellCheckerDictionaryClass = class of TdxCustomSpellCheckerDictionary;

  { TdxSpellCheckerDictionary }

  TdxSpellCheckerDictionary = class(TdxCustomSpellCheckerDictionary)
  strict private
    FCheckCapitalization: Boolean;
    FWords: TdxSpellCheckerWordList;

    function GetWordCount: Integer;
    procedure SetCheckCapitalization(AValue: Boolean);
  protected
    function CanUseDoubleMetaphone: Boolean; override;
    procedure Cleanup; override;
    function DoUnload: Boolean; override;
    procedure FreeContent; override;
    procedure InitializeContent; override;
    function IsCorrectCapitalization(const AWord, APattern: string): Boolean; virtual;
    procedure LanguageChanged; override;
    procedure PopulateCapitalizationSuggestions(ASuggestionBuilder: TdxSpellCheckerSuggestionBuilder; const AWord: string); override;
    procedure PopulateMetaphoneSuggestions(ASuggestionBuilder: TdxSpellCheckerSuggestionBuilder; const AWord: string); override;
    procedure UpdateUsingMetaphone; override;

    property Words: TdxSpellCheckerWordList read FWords;
  public
    procedure Assign(Source: TPersistent); override;
    function HasWord(const AWord: string): Boolean; override;

    property Active;
    property Alphabet;
    property CheckCapitalization: Boolean read FCheckCapitalization write SetCheckCapitalization default False;
    property CodePage;
    property Enabled;
    property Language;
    property Loaded;
    property WordCount: Integer read GetWordCount;
    property OnLoaded;
    property OnLoading;
  end;

  { TdxUserSpellCheckerDictionary }

  TdxUserSpellCheckerDictionaryOption = (udFileMustExist, udSaveOnUnload);
  TdxUserSpellCheckerDictionaryOptions = set of TdxUserSpellCheckerDictionaryOption;

  TdxUserSpellCheckerDictionary = class(TdxSpellCheckerDictionary)
  strict private
    FDictionaryPath: TFileName;
    FOptions: TdxUserSpellCheckerDictionaryOptions;
  protected
    function DoLoad: Boolean; override;
    function DoUnload: Boolean; override;
    function GetDisplayName: string; override;
  public
    constructor Create(ASpellChecker: TdxCustomSpellChecker); override;
    procedure Assign(Source: TPersistent); override;
    procedure AddWord(const AWord: string);
    procedure LoadFromStrings(AStrings: TStrings);
    procedure SaveToStrings(AStrings: TStrings);
  published
    property Alphabet;
    property CheckCapitalization;
    property CodePage;
    property Enabled;
    property DictionaryPath: TFileName read FDictionaryPath write FDictionaryPath;
    property Options: TdxUserSpellCheckerDictionaryOptions read FOptions write FOptions default [udSaveOnUnload];
    property OnLoaded;
    property OnLoading;
  end;

  { TdxSpellCheckerDictionaryItem }

  TdxSpellCheckerDictionaries = class;

  TdxSpellCheckerDictionaryItem = class(TCollectionItem)
  strict private
    FDictionaryType: TdxCustomSpellCheckerDictionary;
    FDictionaryTypeClass: TdxCustomSpellCheckerDictionaryClass;
    FDictionaryEvents: TNotifyEvent;

    function GetCollection: TdxSpellCheckerDictionaries;
    function GetDictionaryTypeClassName: string;
    procedure SetDictionaryType(AValue: TdxCustomSpellCheckerDictionary);
    procedure SetDictionaryTypeClassName(const AValue: string);
    procedure SetDictionaryTypeClass(AValue: TdxCustomSpellCheckerDictionaryClass);
  protected
    function GetDisplayName: string; override;
    procedure RecreateDictionaryType;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Collection: TdxSpellCheckerDictionaries read GetCollection;
    property DictionaryTypeClass: TdxCustomSpellCheckerDictionaryClass read FDictionaryTypeClass write SetDictionaryTypeClass;
  published
    property DictionaryTypeClassName: string read GetDictionaryTypeClassName write SetDictionaryTypeClassName;
    property DictionaryType: TdxCustomSpellCheckerDictionary read FDictionaryType write SetDictionaryType;
    property DictionaryEvents: TNotifyEvent read FDictionaryEvents write FDictionaryEvents;
  end;

  { TdxSpellCheckerDictionaries }

  TdxSpellCheckerDictionaries = class(TCollection)
  private
    FSpellChecker: TdxCustomSpellChecker;
    function GetItem(Index: Integer): TdxSpellCheckerDictionaryItem;
    procedure SetItem(Index: Integer; AValue: TdxSpellCheckerDictionaryItem);
  protected
    function GetOwner: TPersistent; override;

    property SpellChecker: TdxCustomSpellChecker read FSpellChecker;
  public
    constructor Create(ASpellChecker: TdxCustomSpellChecker);
    function Add: TdxSpellCheckerDictionaryItem;
    function GetNamePath: string; override;

    property Items[Index: Integer]: TdxSpellCheckerDictionaryItem read GetItem write SetItem; default;
  end;

  { TdxSpellCheckerAutoCorrectOptions }

  TdxSpellCheckerAutoCorrectOptionsChangedEvent = procedure (Sender: TdxSpellCheckerAutoCorrectOptions) of object;

  TdxSpellCheckerAutoCorrectOptions = class(TdxSpellCheckerCustomAutoCorrectOptions)
  strict private
    FOnChanged: TdxSpellCheckerAutoCorrectOptionsChangedEvent;

    function GetSpellChecker: TdxCustomSpellChecker;
  protected
    procedure DoActiveChanged; override;
    procedure DoChanged; override;
  public
    function GetReplacement(const AText: string; out AReplacement: string): Boolean; virtual;

    property SpellChecker: TdxCustomSpellChecker read GetSpellChecker;
    property OnChanged: TdxSpellCheckerAutoCorrectOptionsChangedEvent read FOnChanged write FOnChanged;
  end;

  { TdxSpellCheckerSpellingOptions }

  TdxSpellCheckerSpellingOptionsChangedEvent = procedure (Sender: TdxSpellCheckerSpellingOptions) of object;

  TdxSpellCheckerSpellingOptions = class(TdxSpellCheckerCustomOptions)
  strict private
    FIgnoreMixedCaseWords: Boolean;
    FIgnoreUpperCaseWords: Boolean;
    FIgnoreMarkupTags: Boolean;
    FIgnoreRepeatedWords: Boolean;
    FCheckSelectedTextFirst: Boolean;
    FIgnoreUrls: Boolean;
    FCheckFromCursorPos: Boolean;
    FIgnoreEmails: Boolean;
    FIgnoreWordsWithNumbers: Boolean;

    FOnChanged: TdxSpellCheckerSpellingOptionsChangedEvent;

    procedure SetCheckFromCursorPos(AValue: Boolean);
    procedure SetCheckSelectedTextFirst(AValue: Boolean);
    procedure SetIgnoreEmails(AValue: Boolean);
    procedure SetIgnoreMarkupTags(AValue: Boolean);
    procedure SetIgnoreMixedCaseWords(AValue: Boolean);
    procedure SetIgnoreRepeatedWords(AValue: Boolean);
    procedure SetIgnoreUpperCaseWords(AValue: Boolean);
    procedure SetIgnoreUrls(AValue: Boolean);
    procedure SetIgnoreWordsWithNumbers(AValue: Boolean);
  protected
    procedure DoChanged; override;
    procedure InitializeOptions; override;
    procedure PopulateRules(ARules: TdxSpellCheckerRules; AHelper: IdxSpellCheckerCheckModeHelper); virtual;

    property IgnoreMarkupTags: Boolean read FIgnoreMarkupTags write SetIgnoreMarkupTags default True;
  public
    procedure Assign(Source: TPersistent); override;

    property OnChanged: TdxSpellCheckerSpellingOptionsChangedEvent read FOnChanged write FOnChanged;
  published
    property CheckFromCursorPos: Boolean read FCheckFromCursorPos write SetCheckFromCursorPos default False;
    property CheckSelectedTextFirst: Boolean read FCheckSelectedTextFirst write SetCheckSelectedTextFirst default True;
    property IgnoreEmails: Boolean read FIgnoreEmails write SetIgnoreEmails default True;
    property IgnoreMixedCaseWords: Boolean read FIgnoreMixedCaseWords write SetIgnoreMixedCaseWords default True;
    property IgnoreRepeatedWords: Boolean read FIgnoreRepeatedWords write SetIgnoreRepeatedWords default False;
    property IgnoreUpperCaseWords: Boolean read FIgnoreUpperCaseWords write SetIgnoreUpperCaseWords default True;
    property IgnoreUrls: Boolean read FIgnoreUrls write SetIgnoreUrls default True;
    property IgnoreWordsWithNumbers: Boolean read FIgnoreWordsWithNumbers write SetIgnoreWordsWithNumbers default True;
  end;

  { TdxSpellCheckerCheckAsYouTypeOptions }

  TdxSpellCheckerCheckAsYouTypeOptionsChangedEvent = procedure(
    ASender: TdxSpellCheckerCheckAsYouTypeOptions) of object;

  TdxSpellCheckerCheckAsYouTypeOptions = class(TdxSpellCheckerCustomCheckAsYouTypeOptions)
  strict private
    FModifyControlPopupMenu: Boolean;
    FPopupMenu: TComponent;
    FOnChanged: TdxSpellCheckerCheckAsYouTypeOptionsChangedEvent;
    function GetSpellChecker: TdxCustomSpellChecker;
    procedure SetModifyControlPopupMenu(AValue: Boolean);
    procedure SetPopupMenu(AValue: TComponent);
  protected
    procedure DoActiveChanged; override;
    procedure DoChanged; override;
    procedure InitializeOptions; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); virtual;
  public
    procedure Assign(Source: TPersistent); override;

    property SpellChecker: TdxCustomSpellChecker read GetSpellChecker;
    property OnChanged: TdxSpellCheckerCheckAsYouTypeOptionsChangedEvent read FOnChanged write FOnChanged;
  published
    property ModifyControlPopupMenu: Boolean read FModifyControlPopupMenu write SetModifyControlPopupMenu default True;
    property PopupMenu: TComponent read FPopupMenu write SetPopupMenu;
  end;

  { TdxSpellCheckerCustomManager }

  TdxSpellCheckerCustomManager = class abstract(TdxSpellCheckerPersistent)
  strict private
    FCheckMode: TdxSpellCheckerCustomCheckMode;
    FContainer: TcxCustomEdit;

    function GetEdit: TWinControl;
    procedure SetEdit(const AValue: TWinControl);
  protected
    FAdapter: IdxSpellCheckerAdapter;

    function CreateAdapter(AControl: TWinControl; out AAdapter: IdxSpellCheckerAdapter): Boolean; virtual;
    procedure DoActiveChanged; virtual; abstract;
    procedure DoFinalizeController; virtual;
    procedure DoFinish(AControl: TWinControl); virtual; abstract;
    procedure DoOptionsChanged; virtual; abstract;
    function DoStart(AControl: TWinControl): Boolean; virtual; abstract;
    function GetActive: Boolean; virtual;
    function GetCheckModeClass: TdxSpellCheckerCustomCheckModeClass; virtual;
    procedure FinalizeController; virtual;
    procedure InitializeController(AControl: TWinControl); virtual;
    procedure ValidateAdapter; virtual;

    property Container: TcxCustomEdit read FContainer;
  public
    constructor Create(ASpellChecker: TdxCustomSpellChecker); override;

    procedure CheckFinish; virtual;
    procedure CheckStart(AControl: TWinControl); virtual;

    property Active: Boolean read GetActive;
    property Adapter: IdxSpellCheckerAdapter read FAdapter;
    property CheckMode: TdxSpellCheckerCustomCheckMode read FCheckMode;
    property Edit: TWinControl read GetEdit write SetEdit;
  end;

  { TdxSpellCheckerManager }

  TdxSpellCheckerManager = class(TdxSpellCheckerCustomManager);

  { IdxSpellCheckerAutoCorrectRule }

  IdxSpellCheckerAutoCorrectRule = interface(IdxSpellCheckerAutoCorrectCustomRule)
  ['{7BDFBC45-8824-4242-82F4-C1E81BF27313}']
    function GetActive: Boolean;
    function IsCheckWord(const ATextController: IdxSpellCheckTextController;
      var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean;
    function IsTerminating: Boolean;
    function IsWordDelimiter(AChar: WideChar): Boolean;

    property Active: Boolean read GetActive;
  end;

  { TdxSpellCheckerAutoCorrectCustomRule }

  TdxSpellCheckerAutoCorrectCustomRule = class(TInterfacedObject,
    IdxSpellCheckerAutoCorrectCustomRule,
    IdxSpellCheckerAutoCorrectRule)
  strict private
    FOptionsLink: TcxObjectLink;
    FWordDelimiters: string;

    function GetOptions: TdxSpellCheckerAutoCorrectOptions;
  protected
    function GetActive: Boolean; virtual;
    procedure InitializeDelimiters; virtual;
    procedure SetWordDelimiters(const AWordDelimiters: string);
  public
    constructor Create(AOptions: TdxSpellCheckerAutoCorrectOptions); virtual;
    destructor Destroy; override;

    procedure AfterCorrect; virtual;
    function IsCheckWord(const ATextController: IdxSpellCheckTextController;
      var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean; virtual; abstract;
    function IsTerminating: Boolean; virtual;
    function IsWordDelimiter(AChar: WideChar): Boolean;
    procedure Undo; virtual;

    property Active: Boolean read GetActive;
    property Options: TdxSpellCheckerAutoCorrectOptions read GetOptions;
    property WordDelimiters: string read FWordDelimiters;
  end;

  { TdxSpellCheckerCustomAutoCorrectManager }

  TdxSpellCheckerCustomAutoCorrectManager = class(TdxSpellCheckerCustomManager)
  strict private
    FLastKey: WideChar;

    function GetOptions: TdxSpellCheckerAutoCorrectOptions;
  protected
    function CreateAdapter(AControl: TWinControl; out AAdapter: IdxSpellCheckerAdapter): Boolean; override;
    function DoAutoCorrect(ARule: TdxSpellCheckerAutoCorrectCustomRule;
      var AWordRange: TdxSpellCheckerAutoCorrectWordRange): Boolean; virtual;
    procedure DoFinish(AControl: TWinControl); override;
    function DoStart(AControl: TWinControl): Boolean; override;
    function GetActive: Boolean; override;
    procedure Undo; virtual;

    property LastKey: WideChar read FLastKey;
  public
    procedure KeyPress(AKey: Char); virtual;

    property Options: TdxSpellCheckerAutoCorrectOptions read GetOptions;
  end;

  { TdxSpellCheckerCustomCheckAsYouTypeManager }

  TdxSpellCheckerCustomCheckAsYouTypeManager = class(TdxSpellCheckerCustomManager)
  strict private
    FManualCheckCount: Integer;

    function GetIsSpellCheckerReady: Boolean;
    function GetOptions: TdxSpellCheckerCheckAsYouTypeOptions;
  protected
    procedure DoFinish(AControl: TWinControl); override;
    function DoStart(AControl: TWinControl): Boolean; override;
    function GetActive: Boolean; override;
    procedure InnerShowPopupMenu(APopup: TComponent; const P: TPoint);
    procedure ManualCheckStateChanged; virtual;
    procedure StartManualSpelling(AAdapter: IdxSpellCheckerAdapter); virtual;
  public
    procedure BeginManualCheck; virtual;
    procedure EndManualCheck; virtual;

    procedure CheckFinish; override;
    procedure CheckStart(AControl: TWinControl); overload; override;
    procedure CheckStart; reintroduce; overload;
    procedure KeyDown(AKey: Word; Shift: TShiftState); virtual;
    procedure KeyUp(AKey: Word; Shift: TShiftState); virtual;
    procedure Refresh(AChangeType: TdxChangeType = ctMedium); virtual; abstract;

    procedure DrawMisspellings(AControl: TWinControl); virtual; abstract;
    procedure LayoutChanged(AControl: TWinControl); virtual; abstract;
    function QueryPopup(APopup: TComponent; const P: TPoint): Boolean; virtual; abstract;
    procedure SelectionChanged(AControl: TWinControl); virtual; abstract;
    procedure TextChanged(AControl: TWinControl); virtual; abstract;

    property IsSpellCheckerReady: Boolean read GetIsSpellCheckerReady;
    property ManualCheckCount: Integer read FManualCheckCount;
    property Options: TdxSpellCheckerCheckAsYouTypeOptions read GetOptions;
  end;

  { TdxSpellCheckerIgnoreList }

  TdxSpellCheckerIgnoreList = class(TInterfacedObject, IdxSpellCheckerIgnoreList)
  strict private
    FWordList: TdxSpellCheckerWordList;
  public
    constructor Create;
    destructor Destroy; override;

    // IdxSpellCheckerIgnoreList
    procedure Add(const AWord: string); overload;
    procedure Add(const AStart, AEnd: IdxSpellCheckerPosition; const AWord: string); overload;
    procedure Clear;
    function Contains(const AWord: string): Boolean; overload;
    function Contains(const AStart, AEnd: IdxSpellCheckerPosition; const AWord: string): Boolean; overload;
    procedure Remove(const AWord: string); overload;
    procedure Remove(const AStart, AEnd: IdxSpellCheckerPosition; const AWord: string); overload;
  end;

  { TdxSpellCheckerIgnoreOnceListsManager }

  TdxSpellCheckerIgnoreOnceListsManager = class(TComponent)
  strict private
    FMap: TDictionary<TControl, IdxSpellCheckerIgnoreList>;
  protected
    function CheckControl(AControl: TControl): TControl;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Get(AControl: TControl): IdxSpellCheckerIgnoreList;
    procedure Reset(AControl: TControl);

    procedure Register(AControl: TControl; const AList: IdxSpellCheckerIgnoreList);
    procedure Unregister(AControl: TControl);
    procedure UnregisterAll;
  end;

  { TdxCustomSpellChecker }

  TdxSpellCheckerSpellingFormType = (sftOutlook, sftWord);

  TdxSpellCheckerEvent = procedure (Sender: TdxCustomSpellChecker) of object;
  TdxSpellCheckerCheckWordEvent = procedure (Sender: TdxCustomSpellChecker;
    const AWord: WideString; out AValid: Boolean; var AHandled: Boolean) of object;
  TdxSpellCheckerAddWordEvent = procedure (AUserDictionary: TdxUserSpellCheckerDictionary;
    const AWord: WideString; var AHandled: Boolean) of object;
  TdxSpellCheckerAutoCorrectEvent = procedure (Sender: TdxCustomSpellChecker;
    ARule: TdxSpellCheckerAutoCorrectCustomRule; var AWordRange: TdxSpellCheckerAutoCorrectWordRange; var AAllow: Boolean) of object;
  TdxSpellCheckerEnabledDictionariesLoadedEvent = procedure (Sender: TdxCustomSpellChecker;
    const AWord: WideString; var AHandled: Boolean) of object;
  TdxSpellCheckerSpellingComplete = procedure (Sender: TdxCustomSpellChecker; var AHandled: Boolean) of object;
  TdxSpellCheckerCheckFinishEvent = procedure (Sender: TdxCustomSpellChecker; AControl: TWinControl) of object;
  TdxSpellCheckerCheckStartEvent = procedure (Sender: TdxCustomSpellChecker; AControl: TWinControl; var AAllow: Boolean) of object;
  TdxSpellCheckerCheckAsYouTypePopupEvent = procedure(ASender: TdxCustomSpellChecker; APopupMenu: TComponent; var AHandled: Boolean) of object;
  TdxSpellCheckerCheckControlInContainerEvent = procedure (Sender: TdxCustomSpellChecker;
    AControl: TWinControl; var AAllow: Boolean; var AContinue: Boolean) of object;
  TdxSpellCheckerGetSuggestionsEvent = procedure (Sender: TdxCustomSpellChecker;
    const AWord: WideString; ASuggestions: TdxSpellCheckerSuggestionList) of object;

  TdxCustomSpellChecker = class(TcxCustomComponent,
    IdxSkinSupport,
    IdxSpellChecker,
    IdxSpellChecker2,
    IdxSpellChecker3)
  strict private
    FAutoCorrectManager: TdxSpellCheckerCustomAutoCorrectManager;
    FAutoCorrectOptions: TdxSpellCheckerAutoCorrectOptions;
    FAutoLoadDictionaries: Boolean;
    FChangeList: TdxSpellCheckerReplacementList;
    FCheckAsYouTypeManager: TdxSpellCheckerCustomCheckAsYouTypeManager;
    FCheckAsYouTypeOptions: TdxSpellCheckerCheckAsYouTypeOptions;
    FCheckingContainer: Boolean;
    FCheckingLock: TCriticalSection;
    FCheckMode: TdxSpellCheckerCustomCheckMode;
    FCheckModeHelper: IdxSpellCheckerCheckModeHelper;
    FCreating: Boolean;
    FDeleteList: TdxSpellCheckerWordList;
    FDialogLookAndFeel: TcxLookAndFeel;
    FDictionaryItems: TdxSpellCheckerDictionaries;
    FIgnoreList: TdxSpellCheckerWordList;
    FIgnoreOnceLists: TdxSpellCheckerIgnoreOnceListsManager;
    FLastDialogResult: Integer;
    FMetaphoneDistance: Integer;
    FRules: TdxSpellCheckerRules;
    FSimilarity: TdxStringSimilarityCalculator;
    FSpellingFormType: TdxSpellCheckerSpellingFormType;
    FSpellingOptions: TdxSpellCheckerSpellingOptions;
    FSuggestionCache: TdxSpellCheckerSuggestionCache;
    FThreadedLoadPriority: TThreadPriority;
    FUseThreadedLoad: Boolean;

    FOnAddWord: TdxSpellCheckerAddWordEvent;
    FOnAfterCheck: TdxSpellCheckerEvent;
    FOnAutoCorrect: TdxSpellCheckerAutoCorrectEvent;
    FOnAutoCorrectFinish: TdxSpellCheckerCheckFinishEvent;
    FOnAutoCorrectStart: TdxSpellCheckerCheckStartEvent;
    FOnBeforeCheck: TdxSpellCheckerEvent;
    FOnCheckAsYouTypeFinish: TdxSpellCheckerCheckFinishEvent;
    FOnCheckAsYouTypePopup: TdxSpellCheckerCheckAsYouTypePopupEvent;
    FOnCheckAsYouTypeStart: TdxSpellCheckerCheckStartEvent;
    FOnCheckControlInContainer: TdxSpellCheckerCheckControlInContainerEvent;
    FOnCheckWord: TdxSpellCheckerCheckWordEvent;
    FOnEnabledDictionariesLoaded: TdxSpellCheckerEvent;
    FOnGetSuggestions: TdxSpellCheckerGetSuggestionsEvent;
    FOnSpellingComplete: TdxSpellCheckerSpellingComplete;

    FOnDialogCheckModeChanged: TdxMulticastMethod<TNotifyEvent>;
    FOnSpellingOptionsChanged: TdxMulticastMethod<TNotifyEvent>;

    procedure AutoCorrectOptionsChangedHandler(Sender: TObject);
    procedure CheckAsYouTypeOptionsChangedHandler(Sender: TObject);
    procedure SpellingOptionsChangedHandler(Sender: TObject);

    function GetActiveDictionary(Index: Integer): TdxCustomSpellCheckerDictionary;
    function GetActiveDictionaryCount: Integer;
    function GetCheckMode: TdxSpellCheckerCustomCheckMode;
    function GetDictionary(Index: Integer): TdxCustomSpellCheckerDictionary;
    function GetDictionaryCount: Integer;
    function GetEnabledDictionary(Index: Integer): TdxCustomSpellCheckerDictionary;
    function GetEnabledDictionaryCount: Integer;
    function GetUserDictionary(Index: Integer): TdxUserSpellCheckerDictionary;
    function GetUserDictionaryCount: Integer;
    procedure SetAutoCorrectOptions(AValue: TdxSpellCheckerAutoCorrectOptions);
    procedure SetCheckAsYouTypeOptions(AValue: TdxSpellCheckerCheckAsYouTypeOptions);
    procedure SetDialogLookAndFeel(AValue: TcxLookAndFeel);
    procedure SetDictionaryItems(AValue: TdxSpellCheckerDictionaries);
    procedure SetMetaphoneDistance(AValue: Integer);
    procedure SetSpellingOptions(AValue: TdxSpellCheckerSpellingOptions);
  protected
    FCheckGroupMode: Boolean;

    procedure AddToIgnoreList(const AWord: string); virtual;
    function CanCheckControlInContainer(AControl: TWinControl): Boolean; virtual;
    procedure CheckCallEnabledDictionariesLoaded;
    procedure CheckControlInContainer(AControl: TWinControl); virtual;
    function CreateAutoCorrectManager: TdxSpellCheckerCustomAutoCorrectManager; virtual;
    function CreateAutoCorrectOptions: TdxSpellCheckerAutoCorrectOptions; virtual;
    function CreateCheckAsYouTypeManager: TdxSpellCheckerCustomCheckAsYouTypeManager; virtual;
    function CreateCheckAsYouTypeOptions: TdxSpellCheckerCheckAsYouTypeOptions; virtual;
    function CreateSimilarity: TdxStringSimilarityCalculator; virtual;
    function CreateSingleWordParser: TdxSpellCheckerSingleWordParser; virtual;
    function CreateSpellingOptions: TdxSpellCheckerSpellingOptions; virtual;
    function DoAddWord(AUserDictionary: TdxUserSpellCheckerDictionary; const AWord: string): Boolean; virtual;
    procedure DoAfterCheck; virtual;
    function DoAutoCorrect(ARule: TdxSpellCheckerAutoCorrectCustomRule; var AWordRange: TdxSpellCheckerAutoCorrectWordRange): Boolean; virtual;
    procedure DoAutoCorrectFinish(AControl: TWinControl); virtual;
    function DoAutoCorrectStart(AControl: TWinControl): Boolean; virtual;
    procedure DoBeforeCheck; virtual;
    procedure DoCheckAsYouTypeFinish(AControl: TWinControl); virtual;
    function DoCheckAsYouTypePopup(APopup: TComponent): Boolean; virtual;
    function DoCheckAsYouTypeStart(AControl: TWinControl): Boolean; virtual;
    procedure DoCheckContainer(AContainer: TWinControl; ARecursive: Boolean); virtual;
    function DoCheckControlInContainer(AControl: TWinControl; var AContinue: Boolean): Boolean; virtual;
    function DoCheckWord(const AWord: string; var AValid: Boolean): Boolean; virtual;
    procedure DoDialogCheckModeChanged; virtual;
    procedure DoEnabledDictionariesLoaded; virtual;
    procedure DoGetSuggestions(const AWord: string; ASuggestions: TdxSpellCheckerSuggestionList); virtual;
    function DoSpellingComplete: Boolean; virtual;
    function GetDialogCheckModeClass: TdxSpellCheckerCustomCheckModeClass; virtual;
    function GetWordChars: string; virtual;
    procedure LoadDictionariesDirect(AIgnoreDisabled: Boolean = True); virtual;
    procedure LoadDictionariesUsingThread(AIgnoreDisabled: Boolean = True); virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SpellingComplete;
    procedure SpellingOptionsChanged(AChangeType: TdxChangeType = ctMedium); virtual;
    procedure UpdateByDictionary(ADictionary: TdxCustomSpellCheckerDictionary); virtual;

    procedure InternalCheck(AAdapter: IdxSpellCheckerAdapter); virtual;
    function InternalIsValidWord(const AWord: string): Boolean; overload; virtual;
    function InternalIsValidWord(const AWord: string; AHelper: IdxSpellCheckerCheckModeHelper): Boolean; overload;
    procedure ReleaseCheckModeHelper(AHelper: IdxSpellCheckerCheckModeHelper); virtual;
    procedure SetCheckModeHelper(AHelper: IdxSpellCheckerCheckModeHelper); virtual;

    // IdxSpellChecker
    procedure CheckFinish;
    procedure CheckStart(AControl: TWinControl);
    procedure DrawMisspellings(AControl: TWinControl);
    function IsSpellCheckerDialogControl(AWnd: THandle): Boolean;
    procedure KeyPress(AKey: Char);
    function QueryPopup(APopup: TComponent; const P: TPoint): Boolean;
    procedure LayoutChanged(AControl: TWinControl);
    procedure SelectionChanged(AControl: TWinControl);
    procedure TextChanged(AControl: TWinControl);
    procedure Undo;

    // IdxSpellChecker2
    procedure KeyDown(AKey: Word; Shift: TShiftState);
    procedure KeyUp(AKey: Word; Shift: TShiftState);

    // IdxSpellChecker3
    function AutoCorrect(AControl: TWinControl; const AController: IdxSpellCheckTextController;
      var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean;
    function GetAutoCorrectOptions: TdxSpellCheckerCustomAutoCorrectOptions;
    function GetCheckAsYouTypeOptions: TdxSpellCheckerCustomCheckAsYouTypeOptions;
    function CheckAsync(AControl: TWinControl; const AController: IdxSpellCheckTextController;
      const AFrom, ATo: IdxSpellCheckerPosition; AProc: IdxSpellChecker3CheckCallbackProc;
      const APrevWord: string = ''): Boolean;
    procedure IdxSpellChecker3.AddToDictionary = AddWordToUserDictionary;
    procedure IdxSpellChecker3.CheckControl = Check;
    procedure IdxSpellChecker3.Ignore = IgnoreOnce;
    function IdxSpellChecker3.CanAddToDictionary = HasEnabledUserDictionary;
    function IdxSpellChecker3.GetSuggestions = GetSuggestionsEx;
    function GetSuggestionsEx(const AWord: string): TArray<string>;
    function IsDialogCheckMode: Boolean;
    procedure IgnoreAll(AControl: TControl; const AWord: string);
    procedure IgnoreOnce(AControl: TControl; const AWord: string; const AStart, AEnd: IdxSpellCheckerPosition); overload;
    procedure IgnoreOnce(AList: IdxSpellCheckerIgnoreList; const AWord: string; const AStart, AEnd: IdxSpellCheckerPosition); overload;
    procedure RegisterIgnoreList(AControl: TControl; const AIgnoreList: IdxSpellCheckerIgnoreList);
    procedure UnregisterIgnoreList(AControl: TControl);
    procedure AddDialogCheckModeChangeHandler(AEvent: TNotifyEvent);
    procedure RemoveDialogCheckModeChangeHandler(AEvent: TNotifyEvent);
    procedure AddSpellingOptionsChangedHandler(AEvent: TNotifyEvent);
    procedure RemoveSpellingOptionsChangedHandler(AEvent: TNotifyEvent);

    property AutoCorrectManager: TdxSpellCheckerCustomAutoCorrectManager read FAutoCorrectManager;
    property ChangeList: TdxSpellCheckerReplacementList read FChangeList;
    property CheckAsYouTypeManager: TdxSpellCheckerCustomCheckAsYouTypeManager read FCheckAsYouTypeManager;
    property CheckingContainer: Boolean read FCheckingContainer;
    property CheckingLock: TCriticalSection read FCheckingLock;
    property DeleteList: TdxSpellCheckerWordList read FDeleteList;
    property IgnoreList: TdxSpellCheckerWordList read FIgnoreList;
    property IgnoreOnceLists: TdxSpellCheckerIgnoreOnceListsManager read FIgnoreOnceLists;
    property LastDialogResult: Integer read FLastDialogResult write FLastDialogResult;
    property Similarity: TdxStringSimilarityCalculator read FSimilarity;
    property SuggestionCache: TdxSpellCheckerSuggestionCache read FSuggestionCache;

    property OnAddWord: TdxSpellCheckerAddWordEvent read FOnAddWord write FOnAddWord;
    property OnAfterCheck: TdxSpellCheckerEvent read FOnAfterCheck write FOnAfterCheck;
    property OnAutoCorrect: TdxSpellCheckerAutoCorrectEvent read FOnAutoCorrect write FOnAutoCorrect;
    property OnAutoCorrectFinish: TdxSpellCheckerCheckFinishEvent read FOnAutoCorrectFinish write FOnAutoCorrectFinish;
    property OnAutoCorrectStart: TdxSpellCheckerCheckStartEvent read FOnAutoCorrectStart write FOnAutoCorrectStart;
    property OnBeforeCheck: TdxSpellCheckerEvent read FOnBeforeCheck write FOnBeforeCheck;
    property OnCheckAsYouTypeFinish: TdxSpellCheckerCheckFinishEvent read FOnCheckAsYouTypeFinish write FOnCheckAsYouTypeFinish;
    property OnCheckAsYouTypePopup: TdxSpellCheckerCheckAsYouTypePopupEvent read FOnCheckAsYouTypePopup write FOnCheckAsYouTypePopup;
    property OnCheckAsYouTypeStart: TdxSpellCheckerCheckStartEvent read FOnCheckAsYouTypeStart write FOnCheckAsYouTypeStart;
    property OnCheckControlInContainer: TdxSpellCheckerCheckControlInContainerEvent read FOnCheckControlInContainer write FOnCheckControlInContainer;
    property OnCheckWord: TdxSpellCheckerCheckWordEvent read FOnCheckWord write FOnCheckWord;
    property OnEnabledDictionariesLoaded: TdxSpellCheckerEvent read FOnEnabledDictionariesLoaded write FOnEnabledDictionariesLoaded;
    property OnGetSuggestions: TdxSpellCheckerGetSuggestionsEvent read FOnGetSuggestions write FOnGetSuggestions;
    property OnSpellingComplete: TdxSpellCheckerSpellingComplete read FOnSpellingComplete write FOnSpellingComplete;

    property OnDialogCheckModeChanged: TdxMulticastMethod<TNotifyEvent> read FOnDialogCheckModeChanged;
    property OnSpellingOptionsChanged: TdxMulticastMethod<TNotifyEvent> read FOnSpellingOptionsChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Check(var AText: AnsiString); overload;
    procedure Check(var AText: string); overload;
    procedure Check(var AText: WideString); overload;
    procedure Check(AEdit: TWinControl); overload;
    procedure CheckContainer(AContainer: TWinControl; ARecursive: Boolean);

    procedure AddWordToUserDictionary(const AWord: string);
    procedure ClearIgnoreOnceList(AEdit: TWinControl);
    function FindDictionaryByWord(const AWord: string): TdxCustomSpellCheckerDictionary;
    function FindFirstEnabledUserDictionary: TdxUserSpellCheckerDictionary;
    function GetSuggestions(const AWord: string): TdxSpellCheckerSuggestionList;
    function HasWordInDictionaries(const AWord: string): Boolean;
    function HasEnabledUserDictionary: Boolean;
    function IsValidWord(const AWord: string): Boolean; overload;
    function IsValidWord(const AWord: AnsiString): Boolean; overload;
    procedure PopulateLanguages(AList: TStrings);
    procedure ShowSpellingCompleteMessage; virtual;

    procedure LoadDictionaries(AIgnoreDisabled: Boolean = True);
    procedure UnloadDictionaries;

    function GetTwoWordsDistance(const AWord1, AWord2: string): Integer;

    property ActiveDictionaries[Index: Integer]: TdxCustomSpellCheckerDictionary read GetActiveDictionary;
    property ActiveDictionaryCount: Integer read GetActiveDictionaryCount;
    property AutoCorrectOptions: TdxSpellCheckerAutoCorrectOptions read FAutoCorrectOptions write SetAutoCorrectOptions;
    property AutoLoadDictionaries: Boolean read FAutoLoadDictionaries write FAutoLoadDictionaries default False;
    property CheckMode: TdxSpellCheckerCustomCheckMode read GetCheckMode;
    property CheckAsYouTypeOptions: TdxSpellCheckerCheckAsYouTypeOptions read FCheckAsYouTypeOptions write SetCheckAsYouTypeOptions;
    property DialogLookAndFeel: TcxLookAndFeel read FDialogLookAndFeel write SetDialogLookAndFeel;
    property Dictionaries[Index: Integer]: TdxCustomSpellCheckerDictionary read GetDictionary;
    property DictionaryCount: Integer read GetDictionaryCount;
    property DictionaryItems: TdxSpellCheckerDictionaries read FDictionaryItems write SetDictionaryItems;
    property EnabledDictionaries[Index: Integer]: TdxCustomSpellCheckerDictionary read GetEnabledDictionary;
    property EnabledDictionaryCount: Integer read GetEnabledDictionaryCount;
    property MetaphoneDistance: Integer read FMetaphoneDistance write SetMetaphoneDistance default 3;
    property Rules: TdxSpellCheckerRules read FRules;
    property SpellingFormType: TdxSpellCheckerSpellingFormType read FSpellingFormType write FSpellingFormType default sftOutlook;
    property SpellingOptions: TdxSpellCheckerSpellingOptions read FSpellingOptions write SetSpellingOptions;
    property ThreadedLoadPriority: TThreadPriority read FThreadedLoadPriority write FThreadedLoadPriority default tpLower;
    property UserDictionaries[Index: Integer]: TdxUserSpellCheckerDictionary read GetUserDictionary;
    property UserDictionaryCount: Integer read GetUserDictionaryCount;
    property UseThreadedLoad: Boolean read FUseThreadedLoad write FUseThreadedLoad default False;
  end;

  { TdxSpellChecker }

  TdxSpellChecker = class(TdxCustomSpellChecker)
  published
    property AutoCorrectOptions;
    property AutoLoadDictionaries;
    property CheckAsYouTypeOptions;
    property DialogLookAndFeel;
    property DictionaryItems;
    property MetaphoneDistance;
    property SpellingFormType;
    property SpellingOptions;
    property ThreadedLoadPriority;
    property UseThreadedLoad;

    property OnAddWord;
    property OnAfterCheck;
    property OnAutoCorrect;
    property OnAutoCorrectFinish;
    property OnAutoCorrectStart;
    property OnBeforeCheck;
    property OnCheckAsYouTypeFinish;
    property OnCheckAsYouTypePopup;
    property OnCheckAsYouTypeStart;
    property OnCheckControlInContainer;
    property OnCheckWord;
    property OnEnabledDictionariesLoaded;
    property OnGetSuggestions;
    property OnSpellingComplete;
  end;

function GetRegisteredDictionaryTypes: TcxRegisteredClasses;

implementation

uses
  Character, dxCharacters,
  AnsiStrings, ComCtrls, Dialogs, cxGeometry, dxOffice11, StrUtils, RTLConsts, dxHash, dxHashUtils, dxSpellCheckerStrs,
  dxSpellCheckerUtils, dxSpellCheckerDialogs, dxISpellDecompressor, dxSpellCheckerCheckAsYouType, dxHunspellDictionary,
  cxContainer, dxSpellCheckerAutoCorrect, dxSpellCheckerBaseForm, dxXMLDoc;

const
  dxBeforeSelectionChecked  =   1;
  dxSelectionChecked        =   2;
  dxAfterSelectionChecked   =   3;

var
  FRegisteredDictionaryTypes: TcxRegisteredClasses;

type
  TcxCustomTextEditPropertiesAccess = class(TcxCustomTextEditProperties);

function GetRegisteredDictionaryTypes: TcxRegisteredClasses;
begin
  if FRegisteredDictionaryTypes = nil then
  begin
    FRegisteredDictionaryTypes := TcxRegisteredClasses.Create;
    FRegisteredDictionaryTypes.Sorted := True;
  end;
  Result := FRegisteredDictionaryTypes;
end;

procedure FreeRegisteredDictionaryTypes;
begin
  FreeAndNil(FRegisteredDictionaryTypes);
end;

procedure SpellCheckerCreatingError(APointer: Pointer);
begin
  raise EdxSpellCheckerException.Create(cxGetResourceString(APointer));
end;

procedure SkipSpace(const S: string; var APos: Integer);
var
  ALen: Integer;
begin
  ALen := Length(S);
  while (APos < ALen) and TdxCharacters.IsWhiteSpace(S[APos]) do
    Inc(APos);
end;

function GetNextPart(const S: string; var APos: Integer; ADelimiter: WideChar; ACanBeSpace: Boolean = True): string;
var
  ALen: Integer;
begin
  ALen := Length(S);
  Result := '';
  while (APos < ALen) and not ((ADelimiter = S[APos]) or (ACanBeSpace and TdxCharacters.IsWhiteSpace(S[APos]))) do
  begin
    Result := Result + S[APos];
    Inc(APos);
  end;
  if ALen > APos then
    Inc(APos);
end;

{ TdxSpellCheckerCustomManager }

constructor TdxSpellCheckerCustomManager.Create(ASpellChecker: TdxCustomSpellChecker);
begin
  inherited Create(ASpellChecker);
  FContainer := nil;
end;

procedure TdxSpellCheckerCustomManager.CheckFinish;
begin
  Edit := nil;
end;

procedure TdxSpellCheckerCustomManager.CheckStart(AControl: TWinControl);
begin
  if Active then
  begin
    if TdxSpellCheckerAdapters.IsSupported(AControl) then
      Edit := AControl
    else
      Edit := nil;
  end;
end;

function TdxSpellCheckerCustomManager.CreateAdapter(AControl: TWinControl; out AAdapter: IdxSpellCheckerAdapter): Boolean;
begin
  Result := TdxSpellCheckerAdapters.CreateAdapter(AControl, AAdapter);
end;

procedure TdxSpellCheckerCustomManager.DoFinalizeController;
begin
end;

function TdxSpellCheckerCustomManager.GetActive: Boolean;
begin
  Result := False;
end;

function TdxSpellCheckerCustomManager.GetCheckModeClass: TdxSpellCheckerCustomCheckModeClass;
begin
  Result := TdxSpellCheckerCustomCheckMode;
end;

procedure TdxSpellCheckerCustomManager.FinalizeController;
begin
  if Assigned(CheckMode) then
  begin
    DoFinish(Container);
    DoFinalizeController;
    FreeAndNil(FCheckMode);
    FContainer := nil;
    FAdapter := nil;
  end;
end;

procedure TdxSpellCheckerCustomManager.InitializeController(AControl: TWinControl);

  function CanCheck(AControl: TControl): Boolean;
  begin
    Result := AControl = nil;
    if not Result then
    begin
      Result := not Supports(AControl, IdxDisabledSpellChecking);
      if Result then
        Result := CanCheck(AControl.Parent);
    end;
  end;

var
  AAdapter: IdxSpellCheckerAdapter;
begin
  if (AControl = nil) or not CanCheck(AControl) then
    FContainer := nil
  else
  begin
    FContainer := TcxCustomEdit(GetInnerControlContainer(AControl));
    if CreateAdapter(AControl, AAdapter) and DoStart(FContainer) then
    begin
      FAdapter := AAdapter;
      FCheckMode := GetCheckModeClass.Create(SpellChecker, Adapter);
    end;
  end;
end;

procedure TdxSpellCheckerCustomManager.ValidateAdapter;
begin
  if Adapter = nil then
    CheckStart(FindControl(GetFocus))
end;

function TdxSpellCheckerCustomManager.GetEdit: TWinControl;
begin
  if Adapter <> nil then
    Result := Adapter.Edit
  else
    Result := nil;
end;

procedure TdxSpellCheckerCustomManager.SetEdit(const AValue: TWinControl);
begin
  if Edit <> AValue then
  begin
    FinalizeController;
    InitializeController(AValue);
  end;
end;

{ TdxSpellCheckerAutoCorrectCustomRule }

constructor TdxSpellCheckerAutoCorrectCustomRule.Create(AOptions: TdxSpellCheckerAutoCorrectOptions);
begin
  inherited Create;
  FOptionsLink := cxAddObjectLink(AOptions);
  InitializeDelimiters;
end;

destructor TdxSpellCheckerAutoCorrectCustomRule.Destroy;
begin
  cxRemoveObjectLink(FOptionsLink);
  inherited Destroy;
end;

procedure TdxSpellCheckerAutoCorrectCustomRule.AfterCorrect;
begin
  // do nothing
end;

procedure TdxSpellCheckerAutoCorrectCustomRule.InitializeDelimiters;
begin
  SetWordDelimiters(#9#13#32'.,<>=!?:;''"()[]{}+|-/\+0123456789');
end;

function TdxSpellCheckerAutoCorrectCustomRule.IsTerminating: Boolean;
begin
  Result := True;
end;

function TdxSpellCheckerAutoCorrectCustomRule.IsWordDelimiter(AChar: WideChar): Boolean;
begin
  Result := WideCharPos(AChar, FWordDelimiters) > 0;
end;

procedure TdxSpellCheckerAutoCorrectCustomRule.Undo;
begin
  // do nothing
end;

function TdxSpellCheckerAutoCorrectCustomRule.GetActive: Boolean;
begin
  Result := False;
end;

function TdxSpellCheckerAutoCorrectCustomRule.GetOptions: TdxSpellCheckerAutoCorrectOptions;
begin
  Result := TdxSpellCheckerAutoCorrectOptions(FOptionsLink.Ref);
end;

procedure TdxSpellCheckerAutoCorrectCustomRule.SetWordDelimiters(const AWordDelimiters: string);
begin
  FWordDelimiters := AWordDelimiters;
end;

{ TdxSpellCheckerCustomAutoCorrectManager }

procedure TdxSpellCheckerCustomAutoCorrectManager.KeyPress(AKey: Char);
begin
  ValidateAdapter;
  FLastKey := WideChar(AKey);
end;

function TdxSpellCheckerCustomAutoCorrectManager.CreateAdapter(
  AControl: TWinControl; out AAdapter: IdxSpellCheckerAdapter): Boolean;
begin
  Result := inherited CreateAdapter(AControl, AAdapter) and Supports(AAdapter, IdxSpellCheckerAutoCorrectAdapter);
end;

function TdxSpellCheckerCustomAutoCorrectManager.DoAutoCorrect(
  ARule: TdxSpellCheckerAutoCorrectCustomRule; var AWordRange: TdxSpellCheckerAutoCorrectWordRange): Boolean;
begin
  Result := SpellChecker.DoAutoCorrect(ARule, AWordRange);
end;

procedure TdxSpellCheckerCustomAutoCorrectManager.DoFinish(AControl: TWinControl);
begin
  SpellChecker.DoAutoCorrectFinish(AControl);
end;

function TdxSpellCheckerCustomAutoCorrectManager.DoStart(AControl: TWinControl): Boolean;
begin
  Result := SpellChecker.DoAutoCorrectStart(AControl);
end;

function TdxSpellCheckerCustomAutoCorrectManager.GetActive: Boolean;
begin
  Result := Options.Active;
end;

procedure TdxSpellCheckerCustomAutoCorrectManager.Undo;
begin
end;

function TdxSpellCheckerCustomAutoCorrectManager.GetOptions: TdxSpellCheckerAutoCorrectOptions;
begin
  Result := SpellChecker.AutoCorrectOptions;
end;

{ TdxSpellCheckerCustomCheckAsYouTypeManager }

procedure TdxSpellCheckerCustomCheckAsYouTypeManager.BeginManualCheck;
begin
  if InterlockedIncrement(FManualCheckCount) = 1 then
    ManualCheckStateChanged;
end;

procedure TdxSpellCheckerCustomCheckAsYouTypeManager.EndManualCheck;
begin
  if InterlockedDecrement(FManualCheckCount) = 0 then
    ManualCheckStateChanged;
end;

procedure TdxSpellCheckerCustomCheckAsYouTypeManager.CheckFinish;
begin
  if ManualCheckCount = 0 then
    inherited CheckFinish;
end;

procedure TdxSpellCheckerCustomCheckAsYouTypeManager.CheckStart(AControl: TWinControl);
begin
  if ManualCheckCount = 0 then
    inherited CheckStart(AControl);
end;

procedure TdxSpellCheckerCustomCheckAsYouTypeManager.CheckStart;
begin
  CheckStart(FindControl(GetFocus));
end;

procedure TdxSpellCheckerCustomCheckAsYouTypeManager.KeyDown(AKey: Word; Shift: TShiftState);
begin
end;

procedure TdxSpellCheckerCustomCheckAsYouTypeManager.KeyUp(AKey: Word; Shift: TShiftState);
begin
end;

procedure TdxSpellCheckerCustomCheckAsYouTypeManager.InnerShowPopupMenu(APopup: TComponent; const P: TPoint);
begin
  if not SpellChecker.DoCheckAsYouTypePopup(APopup) then
    ShowPopupMenu(nil, APopup, P.X, P.Y);
end;

procedure TdxSpellCheckerCustomCheckAsYouTypeManager.ManualCheckStateChanged;
begin
  if ManualCheckCount = 0 then
    TThread.Queue(nil, CheckStart);
end;

function TdxSpellCheckerCustomCheckAsYouTypeManager.GetIsSpellCheckerReady: Boolean;
begin
  Result := SpellChecker.ActiveDictionaryCount > 0;
end;

function TdxSpellCheckerCustomCheckAsYouTypeManager.GetOptions: TdxSpellCheckerCheckAsYouTypeOptions;
begin
  Result := SpellChecker.CheckAsYouTypeOptions;
end;

procedure TdxSpellCheckerCustomCheckAsYouTypeManager.DoFinish(AControl: TWinControl);
begin
  SpellChecker.DoCheckAsYouTypeFinish(AControl);
end;

function TdxSpellCheckerCustomCheckAsYouTypeManager.DoStart(AControl: TWinControl): Boolean;
begin
  Result := SpellChecker.DoCheckAsYouTypeStart(AControl);
end;

function TdxSpellCheckerCustomCheckAsYouTypeManager.GetActive: Boolean;
begin
  Result := Options.Active;
end;

procedure TdxSpellCheckerCustomCheckAsYouTypeManager.StartManualSpelling(AAdapter: IdxSpellCheckerAdapter);
begin
  SpellChecker.InternalCheck(AAdapter);
end;

{ TdxSpellCheckerIgnoreList }

constructor TdxSpellCheckerIgnoreList.Create;
begin
  inherited Create;
  FWordList := TdxSpellCheckerWordList.Create(LANG_SYSTEM_DEFAULT, 257);
end;

destructor TdxSpellCheckerIgnoreList.Destroy;
begin
  FreeAndNil(FWordList);
  inherited Destroy;
end;

procedure TdxSpellCheckerIgnoreList.Add(const AWord: string);
begin
  FWordList.Add(AWord);
end;

procedure TdxSpellCheckerIgnoreList.Add(const AStart, AEnd: IdxSpellCheckerPosition; const AWord: string);
begin
  Add(AWord);
end;

procedure TdxSpellCheckerIgnoreList.Clear;
begin
  FWordList.Clear;
end;

function TdxSpellCheckerIgnoreList.Contains(const AWord: string): Boolean;
begin
  Result := FWordList.Find(AWord);
end;

function TdxSpellCheckerIgnoreList.Contains(const AStart, AEnd: IdxSpellCheckerPosition; const AWord: string): Boolean;
begin
  Result := Contains(AWord)
end;

procedure TdxSpellCheckerIgnoreList.Remove(const AStart, AEnd: IdxSpellCheckerPosition; const AWord: string);
begin
  Remove(AWord)
end;

procedure TdxSpellCheckerIgnoreList.Remove(const AWord: string);
begin
  raise EdxSpellCheckerException.Create('TdxSpellCheckerIgnoreList.Remove not implemented');
end;


{ TdxSpellCheckerAbstractCheckMode }

constructor TdxSpellCheckerAbstractCheckMode.Create(AOwner: TdxCustomSpellChecker);
begin
  inherited Create;
  FSpellChecker := AOwner;
end;

destructor TdxSpellCheckerAbstractCheckMode.Destroy;
begin
  SpellChecker.ReleaseCheckModeHelper(Self);
  inherited Destroy;
end;

function TdxSpellCheckerAbstractCheckMode.GetNextMisspelledWord: Boolean;
var
  AWord: string;
begin
  FLastCode := scNoError;
  FMisspelledWord := '';
  while GetNextWord(AWord) do
  begin
    if InternalProcessWord(AWord) then
      Continue;
    FLastCode := InternalCheckWord(AWord);
    if FLastCode <> scNoError then
      Exit(True);
    PrevWord := AWord;
    FMisspellingStart := FMisspellingFinish;
  end;
  Result := False;
end;

procedure TdxSpellCheckerAbstractCheckMode.Skip;
begin
  FMisspellingStart := FMisspellingFinish;
end;

function TdxSpellCheckerAbstractCheckMode.CheckWord(const AWord: string): TdxSpellCheckerSpellingCode;
begin
  if IsValidWord(AWord) then
    Result := scNoError
  else
  begin
    Result := SpellChecker.Rules.ErrorCode;
    if Result = scNoError then
      Result := scMisspelled;
  end;
  FMisspelledWord := IfThen(Result <> scNoError, AWord);
end;

function TdxSpellCheckerAbstractCheckMode.GetNextWord(out AWord: string): Boolean;
var
  ADelimiters: string;
  APrevStart: IdxSpellCheckerPosition;
begin
  repeat
    APrevStart := FMisspellingStart;
    FMisspellingFinish := FSpellingFinish;
    Result := Controller.GetNextWord(FMisspellingStart, FMisspellingFinish);
    if not Result then
      FMisspellingStart := FMisspellingFinish;
  until Result or not ValidateSpellingBounds;

  if Result then
  begin
    ADelimiters := Controller.GetWord(APrevStart, FMisspellingStart);
    if Length(ADelimiters) <> GetWideCharCount(' ', ADelimiters) then
      PrevWord := '';
    AWord := Controller.GetWord(FMisspellingStart, FMisspellingFinish);
  end;
end;

function TdxSpellCheckerAbstractCheckMode.GetOwner: TPersistent;
begin
  Result := FSpellChecker;
end;

function TdxSpellCheckerAbstractCheckMode.InternalCheckWord(var AWord: string): TdxSpellCheckerSpellingCode;
var
  AWordLength: Integer;
  AWordWithoutLastChar: string;
begin
  Result := CheckWord(AWord);
  AWordLength := Length(AWord);

  // Remove the last non-alphanumeric symbol (.')
  if (Result <> scNoError) and (AWordLength >= 2) then
  begin
    AWordWithoutLastChar := Copy(AWord, 1, AWordLength - 1);
    if (Pos(AWord[AWordLength], MeaningfulCharacters) > 0) and not WideIsAlphaNumeric(AWord[AWordLength]) then
    begin
      AWord := AWordWithoutLastChar;
      AWordLength := Length(AWord);
      FMisspellingFinish := FMisspellingFinish.MoveBackward;
      Result := CheckWord(AWord);
    end;
  end;

  // Skip all non-alphanumeric symbols
  if (Result = scMisspelled) and (AWordLength >= 1) then
  begin
    while (AWordLength > 0) and not WideIsAlphaNumeric(AWord[1]) do
    begin
      FMisspellingStart := FMisspellingStart;
      Dec(AWordLength);
      System.Delete(AWord, 1, 1);
    end;
    while (AWordLength > 0) and not WideIsAlphaNumeric(AWord[AWordLength]) do
    begin
      System.Delete(AWord, AWordLength, 1);
      FMisspellingFinish := FMisspellingFinish.MoveBackward;
      Dec(AWordLength);
    end;
    if AWordLength = 0 then
      Result := scNoError
    else
      Result := CheckWord(AWord);
  end;
end;

function TdxSpellCheckerAbstractCheckMode.InternalProcessWord(const AWord: string): Boolean;
begin
  Result := not Controller.IsRangeEditable(FMisspellingStart, FMisspellingFinish) or IsNeedIgnoreWord(AWord);
  if Result then
  begin
    FMisspelledWord := AWord;
    Skip;
  end;
end;

function TdxSpellCheckerAbstractCheckMode.IsNeedIgnoreWord(const AWord: string): Boolean;
var
  AActualWord: string;
  AWordLength: Integer;
begin
  AActualWord := AWord;
  AWordLength := Length(AActualWord);
  if AActualWord[AWordLength] = '.' then
    AActualWord := Copy(AWord, 1, AWordLength - 1);
  Result := SpellChecker.IgnoreList.Find(AWord) or (IgnoreOnceList <> nil) and
    IgnoreOnceList.Contains(FMisspellingStart, FMisspellingFinish, AWord);
end;

function TdxSpellCheckerAbstractCheckMode.IsValidWord(const AWord: string): Boolean;
begin
  Result := SpellChecker.InternalIsValidWord(AWord, Self);
end;

function TdxSpellCheckerAbstractCheckMode.ValidateSpellingBounds: Boolean;
begin
  Result := False;
end;

procedure TdxSpellCheckerAbstractCheckMode.UpdateMeaningfulCharacters;
var
  AIntf: IdxSpellCheckerMeaningfulCharacters;
begin
  FMeaningfulCharacters := SpellChecker.GetWordChars;
  if Supports(Controller, IdxSpellCheckerMeaningfulCharacters, AIntf) then
    AIntf.UpdateMeaningfulCharacters(FMeaningfulCharacters);
end;

function TdxSpellCheckerAbstractCheckMode.GetPrevWord: string;
begin
  Result := FPrevWord;
end;

function TdxSpellCheckerAbstractCheckMode.WordExists(const AWord: string): Boolean;
begin
  Result := SpellChecker.HasWordInDictionaries(AWord);
end;

{ TdxSpellCheckerCustomCheckMode }

constructor TdxSpellCheckerCustomCheckMode.Create(AOwner: TdxCustomSpellChecker; AAdapter: IdxSpellCheckerAdapter);
begin
  inherited Create(AOwner);
  FAdapter := AAdapter;
  FIgnoreOnceList := AOwner.IgnoreOnceLists.Get(AAdapter.Edit);
  StoreSelection;
  CreateTextController;
  FMisspellingFinish := FSpellingStart;
  FMisspellingStart := FSpellingStart;
end;

destructor TdxSpellCheckerCustomCheckMode.Destroy;
begin
  RestoreSelection;
  FIgnoreOnceList := nil;
  inherited Destroy;
end;

procedure TdxSpellCheckerCustomCheckMode.Add;
begin
  if SpellChecker.HasEnabledUserDictionary then
    SpellChecker.AddWordToUserDictionary(MisspelledWord);
end;

function TdxSpellCheckerCustomCheckMode.CanUndo: Boolean;
begin
  Result := False;
end;

procedure TdxSpellCheckerCustomCheckMode.Change(const AWord: string);
begin
  ReplaceWord(FMisspellingStart, FMisspellingFinish, AWord, AWord);
  ValidateSpellingBounds;
  FMisspellingStart := FMisspellingFinish;
end;

procedure TdxSpellCheckerCustomCheckMode.ChangeAll(const AWord: string);
begin
  SpellChecker.ChangeList.Add(MisspelledWord, AWord);
  Change(AWord);
end;

procedure TdxSpellCheckerCustomCheckMode.Delete;
begin
  FMisspellingStart := FMisspellingStart.MoveBackward;
  FMisspelledWord := Controller.GetWord(FMisspellingStart, FMisspellingFinish);
  ReplaceWord(FMisspellingStart, FMisspellingFinish, '', PrevWord);
  ValidateSpellingBounds;
  FMisspellingStart := FMisspellingFinish;
end;

procedure TdxSpellCheckerCustomCheckMode.DeleteAll;
begin
  SpellChecker.DeleteList.Add(MisspelledWord);
  Delete;
end;

function TdxSpellCheckerCustomCheckMode.GetNextMisspelledWord: Boolean;
begin
  Result := inherited GetNextMisspelledWord;
  if Result then
    SelectMisspelledWord;
end;

function TdxSpellCheckerCustomCheckMode.GetSuggestions(const AWord: string): TdxSpellCheckerSuggestionList;
begin
  Result := SpellChecker.GetSuggestions(AWord);
end;

function TdxSpellCheckerCustomCheckMode.InternalProcessWord(const AWord: string): Boolean;
var
  AReplacement: string;
begin
  Result := inherited InternalProcessWord(AWord);
  if not Result then
    if IsNeedChangeWord(AWord, AReplacement) then
    begin
      Change(AReplacement);
      PrevWord := AReplacement;
      Result := True;
    end
    else
      if IsNeedDeleteWord(AWord) then
      begin
        Delete;
        Result := True;
      end;
end;

procedure TdxSpellCheckerCustomCheckMode.Ignore;
begin
  SpellChecker.IgnoreOnce(IgnoreOnceList, MisspelledWord, FMisspellingStart, FMisspellingFinish);
  Skip;
end;

procedure TdxSpellCheckerCustomCheckMode.IgnoreAll;
begin
  SpellChecker.AddToIgnoreList(MisspelledWord);
  Skip;
end;

procedure TdxSpellCheckerCustomCheckMode.ReplaceWord(
  var AStart, AFinish: IdxSpellCheckerPosition; const AWord, APrevWord: string);
begin
  Adapter.Replace(AStart, AFinish, AWord, FSpellingStart, FSpellingFinish);
  PrevWord := APrevWord;
  UpdateTextInfo;
end;

procedure TdxSpellCheckerCustomCheckMode.RestoreSelection;
begin
end;

procedure TdxSpellCheckerCustomCheckMode.StoreSelection;
begin
end;

procedure TdxSpellCheckerCustomCheckMode.UndoLast;
begin
end;

procedure TdxSpellCheckerCustomCheckMode.CreateTextController;
begin
  if FController = nil then
    FController := Adapter.CreateController;
  Adapter.GetSpellingBounds(FSpellingStart, FSpellingFinish);
  UpdateMeaningfulCharacters;
  UpdateTextInfo;
end;

function TdxSpellCheckerCustomCheckMode.IsNeedChangeWord(const AWord: string; out AReplacement: string): Boolean;
var
  AItem: TdxSpellCheckerReplacement;
  ANeedChangeCapitalization: Boolean;
  AReplacementCapitalizationType: TdxCapitalizationType;
  AWordCapitalizationType: TdxCapitalizationType;
begin
  AItem := SpellChecker.ChangeList.FindReplacement(AWord);
  Result := Assigned(AItem);
  if Result then
  begin
    AReplacement := AItem.Replacement;
    AReplacementCapitalizationType := GetWordCapitalizationType(AReplacement);
    AWordCapitalizationType := GetWordCapitalizationType(AWord);
    ANeedChangeCapitalization := (AReplacementCapitalizationType = ctLower) and
      not (AWordCapitalizationType in [ctMixedCapitalized, ctMixed]) and
      (AReplacementCapitalizationType <> AWordCapitalizationType);
    if ANeedChangeCapitalization then
    begin
      case AWordCapitalizationType of
        ctCapitalized:
          AReplacement[1] := WideUpperCase(AReplacement[1])[1];
        ctUpper:
          AReplacement := WideUpperCase(AReplacement);
      end;
    end;
  end;
end;

function TdxSpellCheckerCustomCheckMode.IsNeedDeleteWord(const AWord: string): Boolean;
begin
  Result := SpellChecker.DeleteList.Find(AWord);
end;

procedure TdxSpellCheckerCustomCheckMode.SelectMisspelledWord;
begin
  Adapter.SetSelection(FMisspellingStart, FMisspellingFinish);
end;

procedure TdxSpellCheckerCustomCheckMode.UpdateByDictionary(ADictionary: TdxCustomSpellCheckerDictionary);
begin
end;

procedure TdxSpellCheckerCustomCheckMode.UpdateTextInfo;
begin
  Adapter.UpdateController(Controller);
  Controller.SetSpellingArea(FSpellingStart, FSpellingFinish);
end;

{ TdxSpellCheckerDialogCheckMode }

constructor TdxSpellCheckerDialogCheckMode.Create(AOwner: TdxCustomSpellChecker; AAdapter: IdxSpellCheckerAdapter);
begin
  inherited Create(AOwner, AAdapter);
  FUndoManager := TdxSpellCheckerUndoManager.Create;
  Adapter.HideSelection := False;
end;

destructor TdxSpellCheckerDialogCheckMode.Destroy;
begin
  FreeAndNil(FUndoManager);
  inherited Destroy;
end;

function TdxSpellCheckerDialogCheckMode.CanUndo: Boolean;
begin
  Result := UndoManager.Count > 0;
end;

procedure TdxSpellCheckerDialogCheckMode.Change(const AWord: string);
var
  AStart, AFinish: IdxSpellCheckerPosition;
begin
  AFinish := FMisspellingFinish;
  AStart := FMisspellingStart;
  inherited Change(AWord);
  UndoManager.Add(MisspelledWord, AWord, '', AStart, AFinish, FMisspellingFinish);
end;

procedure TdxSpellCheckerDialogCheckMode.CreateTextController;
begin
  inherited CreateTextController;

  FCheckedRange := 0;
  if not SpellChecker.CheckingContainer then
  begin
    if SpellChecker.SpellingOptions.CheckFromCursorPos or IsCheckingSelectedText then
      FSpellingStart := Controller.GetWordStartPosition(FSaveSelStart);
    if IsCheckingSelectedText then
    begin
      FSpellingFinish := Controller.GetWordFinishPosition(FSaveSelFinish);
      FCheckedRange := dxSelectionChecked;
    end;
  end;
  UpdateTextInfo;
end;

procedure TdxSpellCheckerDialogCheckMode.Ignore;
begin
  UndoManager.Add(MisspelledWord, MisspelledWord, PrevWord, FMisspellingStart, FMisspellingFinish, FMisspellingFinish);
  inherited Ignore;
end;

function TdxSpellCheckerDialogCheckMode.ShowDialog: Integer;
begin
  Adapter.Post;
  Result := 0;
end;

function TdxSpellCheckerDialogCheckMode.IsCheckingSelectedText: Boolean;
begin
  Result := SpellChecker.SpellingOptions.CheckSelectedTextFirst and (FSaveSelFinish.Compare(FSaveSelStart) > 0);
end;

procedure TdxSpellCheckerDialogCheckMode.RestoreSelection;
begin
  Adapter.HideSelection := FSaveHideSelection;
  Adapter.SetSelection(FSaveSelStart, FSaveSelFinish);
end;

procedure TdxSpellCheckerDialogCheckMode.UndoLast;
var
  ADelta: Integer;
  AItem: TdxSpellCheckerUndoItem;
  AMisspellingFinish: Integer;
  AMisspellingStart: Integer;
  AStart, AFinish: IdxSpellCheckerPosition;
begin
  AItem := UndoManager.Last;
  if CanUndo and (AItem <> nil) then
  begin
    AMisspellingFinish := FMisspellingFinish.ToInteger;
    AMisspellingStart := FMisspellingStart.ToInteger;

    AStart := AItem.ReplacementStart;
    AFinish := AItem.ReplacementFinish;
    ReplaceWord(AStart, AFinish, AItem.MisspelledWord, AItem.PrevWord);
    ADelta := AFinish.ToInteger - AItem.ReplacementFinish.ToInteger;

    FMisspellingFinish := Controller.CreatePosition(AMisspellingFinish + ADelta);
    FMisspellingStart := Controller.CreatePosition(AMisspellingStart + ADelta);

    SelectMisspelledWord;
    ValidateSpellingBoundsAfterUndo;
    UndoManager.UndoLast;
  end;
end;

procedure TdxSpellCheckerDialogCheckMode.StoreSelection;
begin
  FSaveHideSelection := Adapter.HideSelection;
  Adapter.GetSelection(FSaveSelStart, FSaveSelFinish);
  inherited StoreSelection;
end;

function TdxSpellCheckerDialogCheckMode.ValidateSpellingBounds: Boolean;

  function ShowCheckEntireDocumentConfirmation: Boolean;
  begin
    Result := MessageDlg(cxGetResourceString(@sdxSpellCheckerSelectionCheckIsFinished), mtInformation, [mbYes, mbNo], 0) = mrYes;
  end;

  procedure SetBeforeSelectionBounds(const ASpellingStart: IdxSpellCheckerPosition);
  begin
    PrevWord := '';
    FSpellingFinish := FSpellingStart;
    FSpellingStart := ASpellingStart;
    FMisspellingStart := ASpellingStart;
    FMisspellingFinish := FMisspellingStart;
    UpdateTextInfo;
  end;

var
  ASpellingStart, ASpellingEnd: IdxSpellCheckerPosition;
begin
  Result := False;
  if (SpellChecker.SpellingOptions.CheckFromCursorPos or IsCheckingSelectedText) and not SpellChecker.CheckingContainer then
  begin
    Adapter.GetSpellingBounds(ASpellingStart, ASpellingEnd);

    if IsCheckingSelectedText then
    begin
      if FMisspellingStart.Compare(FSpellingFinish) >= 0 then
      begin
        if (FCheckedRange = dxAfterSelectionChecked) and (FMisspellingStart.Compare(ASpellingStart) > 0) then
        begin
          SetBeforeSelectionBounds(ASpellingStart);
          FCheckedRange := dxBeforeSelectionChecked;
          Result := True;
        end;
        if (FCheckedRange = dxSelectionChecked) and ShowCheckEntireDocumentConfirmation then
        begin
          FCheckedRange := dxAfterSelectionChecked;
          FMisspellingStart := FSpellingFinish;
          FSpellingFinish := ASpellingEnd;
          UpdateTextInfo;
          Result := True;
        end;
      end;
    end
    else
      if (FSpellingStart.Compare(ASpellingStart) > 0) and
        ((FMisspellingStart.Compare(ASpellingEnd) >= 0) or (FMisspellingStart.Compare(FSpellingStart) < 0)) then
      begin
        SetBeforeSelectionBounds(ASpellingStart);
        Result := True;
      end;
  end;
end;

procedure TdxSpellCheckerDialogCheckMode.ValidateSpellingBoundsAfterUndo;
var
  ASpellingStart, ASpellingFinish: IdxSpellCheckerPosition;
begin
  Adapter.GetSpellingBounds(ASpellingStart, ASpellingFinish);
  if (FMisspellingStart.Compare(FSpellingFinish) >= 0) and (FSpellingFinish.Compare(ASpellingFinish) < 0) then
  begin
    FCheckedRange := dxAfterSelectionChecked;
    FSpellingStart := FSpellingFinish;
    FSpellingFinish := ASpellingFinish;
    UpdateTextInfo;
  end;
end;

{ TdxSpellCheckerOutlookCheckMode }

function TdxSpellCheckerOutlookCheckMode.ShowDialog: Integer;
begin
  Result := dxShowOutlookSpellingDialog(SpellChecker);
  inherited ShowDialog;
end;

{ TdxSpellCheckerWordCheckMode }

procedure TdxSpellCheckerWordCheckMode.ChangeSentence(const ASentence: string);
var
  AMisspelledSentence: TdxSpellCheckerSentence;
begin
  AMisspelledSentence := MisspelledSentence;
  FMisspellingFinish := AMisspelledSentence.TextPositionFinish;
  FMisspellingStart := AMisspelledSentence.TextPositionStart;
  ReplaceWord(FMisspellingStart, FMisspellingFinish, ASentence, '');
  UndoManager.Add(AMisspelledSentence.Text, ASentence, '',
    AMisspelledSentence.TextPositionStart, AMisspelledSentence.TextPositionFinish, FMisspellingFinish);
  UpdateTextInfo;
  FMisspellingStart := AMisspelledSentence.TextPositionStart;
end;

function TdxSpellCheckerWordCheckMode.ShowDialog: Integer;
begin
  Result := dxShowWordSpellingDialog(SpellChecker);
  inherited ShowDialog;
end;

function TdxSpellCheckerWordCheckMode.GetMisspelledSentence: TdxSpellCheckerSentence;
begin
  Result.TextPositionStart := Controller.GetSentenceStartPosition(FMisspellingStart);
  Result.TextPositionFinish := Controller.GetSentenceFinishPosition(FMisspellingStart);
  Result.Text := Controller.GetWord(Result.TextPositionStart, Result.TextPositionFinish);
  Result.MisspelledWordPositionStart := FMisspellingStart;
  Result.MisspelledWordPositionFinish := FMisspellingFinish;
  Result.MisspelledWordPositionInText := Length(Controller.GetWord(Result.TextPositionStart, FMisspellingStart));
end;

{ TdxSpellCheckerSuggestionBuilder }

constructor TdxSpellCheckerSuggestionBuilder.Create(ADictionary: TdxCustomSpellCheckerDictionary);
begin
  inherited Create;
  FDictionary := ADictionary;
  Alphabet := ADictionary.GetActiveAlphabet;
end;

procedure TdxSpellCheckerSuggestionBuilder.AddSuggestions(const AWord: string; ASuggestions: TdxSpellCheckerSuggestionList);
begin
  if (ASuggestions = nil) or (Length(AWord) = 0) then Exit;
  FSuggestions := ASuggestions;
  FOriginalWord := AWord;
  FWord := PrepareWord(AWord);
  try
    FStartTicks := GetTickCount;
    DoAddSuggestions;
    if (Suggestions.Count = 0) and Dictionary.CanUseDoubleMetaphone then
      DoAddMetaphoneSuggestions;
  except
    on EAbort do;
    else
      raise;
  end;
end;

function TdxSpellCheckerSuggestionBuilder.CanAddToSuggestions(const ATestWord: string): Boolean;
begin
  Result := Dictionary.HasWord(ATestWord);
  if IsTimeOut then
    Abort;
end;

procedure TdxSpellCheckerSuggestionBuilder.CheckAddMetaphoneSuggestion(
  ATestWord, AUpperWord1: PWideChar; AUpperWordLen1: Integer;
  AUpperWord2: PWideChar; AUpperWordLen2: Integer);
var
  ADistance: Integer;
begin
  ADistance := Similarity.GetDistance(AUpperWord1, AUpperWordLen1, AUpperWord2,
    AUpperWordLen2);
  if ADistance <= MaxDistance then
    Suggestions.Add(ATestWord, Dictionary, ADistance);
  if IsTimeOut then
    Abort;
end;

procedure TdxSpellCheckerSuggestionBuilder.PopulateCapitalizationSuggestions;
begin
  Dictionary.PopulateCapitalizationSuggestions(Self, Word);
end;

procedure TdxSpellCheckerSuggestionBuilder.DoAddMetaphoneSuggestions;
begin
  Dictionary.PopulateMetaphoneSuggestions(Self, Word);
end;

procedure TdxSpellCheckerSuggestionBuilder.DoAddSuggestions;
begin
  PopulateCapitalizationSuggestions;
end;

function TdxSpellCheckerSuggestionBuilder.IsCaseSensitive: Boolean;
begin
  Result := False;
end;

function TdxSpellCheckerSuggestionBuilder.IsTimeOut: Boolean;
begin
  Result := (Timeout > 0) and (GetTickCount >= Timeout + FStartTicks);
end;

function TdxSpellCheckerSuggestionBuilder.PrepareWord(const AWord: string): string;
begin
  Result := WideLowerCase(AWord);
end;

function TdxSpellCheckerSuggestionBuilder.GetMaxDistance: Integer;
begin
  Result := Dictionary.SpellChecker.MetaphoneDistance * 2;
end;

function TdxSpellCheckerSuggestionBuilder.GetSimilarity: TdxStringSimilarityCalculator;
begin
  Result := Dictionary.SpellChecker.Similarity;
end;

function TdxSpellCheckerSuggestionBuilder.GetTimeout: Cardinal;
begin
  Result := Dictionary.Timeout;
end;

procedure TdxSpellCheckerSuggestionBuilder.SetAlphabet(const AValue: string);
var
  ALower: string;
  I: Integer;
begin
  FAlphabet := '';
  if Length(AValue) > 0 then
  begin
    if IsCaseSensitive then
      FAlphabet := AValue
    else
    begin
      ALower := LowerCase(AValue);
      for I := 1 to Length(ALower) do
        if WideCharPos(ALower[I], FAlphabet) = 0 then
          FAlphabet := FAlphabet + ALower[I];
    end;
  end;
end;


{ TdxSpellCheckerUndoManager }

constructor TdxSpellCheckerUndoManager.Create;
begin
  inherited Create;
  FUndoList := TcxObjectList.Create;
end;

destructor TdxSpellCheckerUndoManager.Destroy;
begin
  FreeAndNil(FUndoList);
  inherited Destroy;
end;

procedure TdxSpellCheckerUndoManager.Add(const AMisspelledWord, AReplacement, APrevWord: string;
  const AMisspelledWordStart, AMisspelledWordFinish, AReplacementFinish: IdxSpellCheckerPosition);
begin
  FUndoList.Add(TdxSpellCheckerUndoItem.Create(AMisspelledWord,
    AReplacement, APrevWord, AMisspelledWordStart, AMisspelledWordFinish, AReplacementFinish));
end;

procedure TdxSpellCheckerUndoManager.UndoLast;
begin
  if Count > 0 then
  begin
    Last.Free;
    FUndoList.Delete(Count - 1);
  end;
end;

function TdxSpellCheckerUndoManager.GetCount: Integer;
begin
  Result := FUndoList.Count;
end;

function TdxSpellCheckerUndoManager.GetItem(Index: Integer): TdxSpellCheckerUndoItem;
begin
  Result := TdxSpellCheckerUndoItem(FUndoList[Index]);
end;

function TdxSpellCheckerUndoManager.GetLast: TdxSpellCheckerUndoItem;
begin
  if Count > 0 then
    Result := Items[Count - 1]
  else
    Result := nil;
end;

{ TdxNearMissStrategy }

procedure TdxNearMissStrategy.CheckAddToSuggestions(const ATestWord: string);
begin
  if CanAddToSuggestions(ATestWord) then
    if IsCaseSensitive then
      Suggestions.Add(ATestWord, Dictionary, 2)
    else
      Suggestions.Add(WidePatternCase(OriginalWord, ATestWord), Dictionary, 2);
end;

procedure TdxNearMissStrategy.CheckChangeOneLetter;
var
  ASymbol: WideChar;
  ATestWord: string;
  I, J: Integer;
begin
  if Length(Alphabet) = 0 then
    Exit;

  for I := 1 to Length(Word) do
    for J := 1 to Length(Alphabet) do
    begin
      ASymbol := Alphabet[J];
      if Word[I] <> ASymbol then
      begin
        ATestWord := Word;
        ATestWord[I] := ASymbol;
        CheckAddToSuggestions(ATestWord);
      end;
      if IsTimeOut then
        Exit;
    end;
end;

procedure TdxNearMissStrategy.CheckDeleteLetter;
var
  ADeletePos, ALen: Integer;
  ASymbol: WideChar;
  ATestWord: string;
begin
  ALen := Length(Word);
  if ALen < 2 then Exit;
  ADeletePos := ALen;
  repeat
    ATestWord := Word;
    ASymbol := ATestWord[ADeletePos];
    Delete(ATestWord, ADeletePos, 1);
    CheckAddToSuggestions(ATestWord);
    Dec(ADeletePos);
    while (ADeletePos >= 1) and (ASymbol = Word[ADeletePos]) do
      Dec(ADeletePos);
  until ADeletePos < 1;
end;

procedure TdxNearMissStrategy.CheckDoubleTwoChars;
var
  ALen: Integer;
  ATestWord: string;
  AWord: PWideChar;
  I: Integer;
begin
  ALen := Length(Word);
  if ALen < 4 then Exit;
  AWord := PWideChar(Word);
  for I := 0 to ALen - 3 do
    if PCardinal(@AWord[I])^ = PCardinal(@AWord[I + 2])^ then
    begin
      ATestWord := Word;
      Delete(ATestWord, I + 1, 2);
      CheckAddToSuggestions(PWideChar(ATestWord));
    end;
end;

procedure TdxNearMissStrategy.CheckInsertLetter;
var
  ASymbol: WideChar;
  ATestWord: string;
  I, AInsertPos, ALen: Integer;
begin
  if Length(Alphabet) = 0 then
    Exit;

  ALen := Length(Word);
  for I := 1 to Length(Alphabet) do
  begin
    ASymbol := Alphabet[I];
    AInsertPos := 1;
    repeat
      ATestWord := Word;
      while (AInsertPos <= ALen) and (ASymbol = ATestWord[AInsertPos]) do
        Inc(AInsertPos);
      Insert(ASymbol, ATestWord, AInsertPos);
      CheckAddToSuggestions(ATestWord);
      if IsTimeOut then
        Exit;
      if AInsertPos > ALen then
        Break
      else
        Inc(AInsertPos);
    until False;
  end;
end;

procedure TdxNearMissStrategy.CheckInsertSpace;
var
  ATestWord1, ATestWord2: string;
  I, ALen: Integer;
begin
  ALen := Length(Word);
  if ALen < 2 then
    Exit;

  for I := ALen - 1 downto 1 do
  begin
    ATestWord1 := Copy(Word, 1, I);
    ATestWord2 := Copy(Word, I + 1, ALen);
    if CanAddToSuggestions(ATestWord1) and  CanAddToSuggestions(ATestWord2) then
      Suggestions.Add(ATestWord1 + ' ' + ATestWord2, Dictionary, 2);
  end;
end;

procedure TdxNearMissStrategy.CheckMoveChar;
var
  AChar: WideChar;
  ALen: Integer;
  ATestWord: string;
  I, J: Integer;
begin
  ALen := Length(Word);
  if ALen < 2 then
    Exit;
  ATestWord := Word;
  for I := 1 to ALen do
  begin
    J := I + 1;
    while (J < ALen) and ((J - I) < 10) do
    begin
      AChar := ATestWord[J];
      ATestWord[J] := ATestWord[J + 1];
      ATestWord[J + 1] := AChar;
      if (J - I) >= 2 then
        CheckAddToSuggestions(ATestWord);
      Inc(J);
    end;
    ATestWord := Word;
  end;

  for I := ALen downto 1 do
  begin
    J := I - 1;
    while (J > 1) and ((I - J) < 10) do
    begin
      AChar := ATestWord[J];
      ATestWord[J] := ATestWord[J - 1];
      ATestWord[J - 1] := AChar;
      if (I - J) >=2 then
        CheckAddToSuggestions(ATestWord);
      Dec(J);
    end;
    ATestWord := Word;
  end;
end;

procedure TdxNearMissStrategy.DoAddSuggestions;
begin
  inherited DoAddSuggestions;
  InterchangeTwoLetters;
  LongInterchangeTwoLetters;
  CheckDeleteLetter;
  CheckInsertLetter;
  CheckMoveChar;
  CheckChangeOneLetter;
  CheckDoubleTwoChars;
  CheckInsertSpace;
end;

procedure TdxNearMissStrategy.InterchangeTwoLetters;
var
  ATestWord: string;
  I, ALen: Integer;
begin
  ALen := Length(Word);
  if ALen < 2 then Exit;
  for I := 1 to ALen - 1 do
  begin
    if Word[I] = Word[I + 1] then
      Continue;
    ATestWord := Word;
    ATestWord[I] := ATestWord[I + 1];
    ATestWord[I + 1] := Word[I];
    CheckAddToSuggestions(ATestWord);
  end;
end;

procedure TdxNearMissStrategy.LongInterchangeTwoLetters;
var
  AChar: WideChar;
  ALen, I, J: Integer;
  AWord: string;
begin
  ALen := Length(Word);
  if ALen < 3 then Exit;
  for I := 1 to ALen do
    for J := I + 2 to ALen do
    begin
      AWord := Word;
      AChar := AWord[I];
      AWord[I] := AWord[J];
      AWord[J] := AChar;
      CheckAddToSuggestions(AWord);
    end;
end;

{ TdxSpellCheckerWordList }

constructor TdxSpellCheckerWordList.Create(ALangID: Cardinal; ATableSize: Integer);
begin
  inherited Create;
  InitializeCriticalSectionAndSpinCount(FLock, 512);
  FLangID := ALangID;
  FCount := 0;
  FTableSize := ATableSize;
  FTable := AllocMem(FTableSize * SizeOf(TdxNativeInt));
end;

destructor TdxSpellCheckerWordList.Destroy;
begin
  Clear;
  FreeMem(FTable);
  UseDoubleMetaphone := False; //free memory
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

procedure TdxSpellCheckerWordList.Add(const S: string);
var
  AIndex, ASrcLen, AWordLen: Integer;
  AWord, ATemp, P: PWideChar;
  CT1, CT2: TdxCapitalizationType;
begin
  ASrcLen := Length(S);
  if ASrcLen = 0 then Exit;
  EnterCriticalSection(FLock);
  try
    P := Pointer(S);
    AIndex := ElfHash(P, ASrcLen) mod FTableSize;
    AWord := FTable^[AIndex];
    if AWord = nil then
      FTable^[AIndex] := NewWord(P, ASrcLen)
    else
    begin
      repeat
        AWordLen := WordLength(AWord);
        if (AWordLen = ASrcLen) and (CompareStringW(LangID, NORM_IGNORECASE, AWord, AWordLen, P, ASrcLen) = CSTR_EQUAL) then
        begin
          if FindMostValidCapitalizationInDuplicates then
          begin
            CT1 := GetWordCapitalizationType(AWord, AWordLen, LangID);
            if CT1 = ctLower then
              Exit;
            CT2 := GetWordCapitalizationType(P, AWordLen, LangID);
            if (CT1 <> CT2) and ((CT2 = ctLower) or ((CT2 = ctCapitalized) and (CT1 = ctUpper))) then
              Move(Pointer(P)^, Pointer(AWord)^, AWordLen * SizeOf(WideChar));
          end;
          Exit;
        end;
        ATemp := GetNextWord(AWord);
        if ATemp = nil then
        begin
          SetNextWord(AWord, NewWord(P, ASrcLen));
          Break;
        end;
        AWord := ATemp;
      until False;
    end;
    Inc(FCount);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TdxSpellCheckerWordList.Clear;
var
  AWord, ATemp: PWideChar;
  I: Integer;
begin
  if Count = 0 then
    Exit;

  EnterCriticalSection(FLock);
  try
    InitMetaphoneTable;
    for I := 0 to FTableSize - 1 do
    begin
      AWord := FTable^[I];
      while AWord <> nil do
      begin
        ATemp := AWord;
        AWord := GetNextWord(AWord);
        DisposeWord(ATemp);
      end;
      FTable^[I] := nil;
    end;
    FCount := 0;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TdxSpellCheckerWordList.Find(const S: string): Boolean;
begin
  Result := FindWord(S) <> nil;
end;

procedure TdxSpellCheckerWordList.LoadFromStrings(AStrings: TStrings);
var
  I: Integer;
begin
  if AStrings <> nil then
  begin
    Clear;
    for I := 0 to AStrings.Count - 1 do
      Add(AStrings[I]);
  end;
end;

procedure TdxSpellCheckerWordList.PopulateMetaphoneSuggestions(
  ASuggestionBuilder: TdxSpellCheckerSuggestionBuilder; const AWord: string);
var
  ABuffer: array[0..256] of WideChar;
  AKey: Word;
  ASrcLen, ATestLen: Integer;
  ATestWord: PWideChar;
begin
  ASrcLen := Length(AWord);
  if not UseDoubleMetaphone or (ASrcLen = 0) or (ASuggestionBuilder = nil) or Find(AWord) then
    Exit;

  //FUpperCaseBuffer contains an upper string here
  FMetaphone.DoubleMetaphone(@FUpperCaseBuffer, ASrcLen);
  AKey := FMetaphone.PrimaryKey;
  if AKey = 0 then Exit;
  ATestWord := FMetaphoneTable^[AKey];
  while ATestWord <> nil do
  begin
    ATestLen := WordLength(ATestWord);
    if Abs(ATestLen - ASrcLen) <= ASuggestionBuilder.MaxDistance then
    begin
      LCMapStringW(LangID, LCMAP_UPPERCASE, ATestWord, ATestLen, @ABuffer, SizeOf(ABuffer) div SizeOf(WideChar));
      ASuggestionBuilder.CheckAddMetaphoneSuggestion(ATestWord, @ABuffer, ATestLen, @FUpperCaseBuffer, ASrcLen);
    end;
    ATestWord := GetNextMetaphoneWord(ATestWord);
  end;
end;

procedure TdxSpellCheckerWordList.SaveToStrings(AStrings: TStrings);
var
  I: Integer;
  AWord: PWideChar;
begin
  if AStrings = nil then Exit;
  AStrings.BeginUpdate;
  try
    AStrings.Clear;
    for I := 0 to FTableSize - 1 do
    begin
      AWord := FTable^[I];
      while AWord <> nil do
      begin
        AStrings.Add(AWord);
        AWord := GetNextWord(AWord);
      end;
    end;
  finally
    AStrings.EndUpdate;
  end;
end;

function TdxSpellCheckerWordList.ElfHash(P: PWideChar; ALength: Integer): Integer;
begin
  Result := dxElfHash(P, ALength, @FUpperCaseBuffer, SizeOf(FUpperCaseBuffer) div SizeOf(WideChar));
end;

function TdxSpellCheckerWordList.FindWord(const S: string): PWideChar;
var
  AIndex, ASrcLen, AResultLen: Integer;
  P: PWideChar;
begin
  Result := nil;
  ASrcLen := Length(S);
  if ASrcLen = 0 then Exit;
  P := Pointer(S);
  AIndex := ElfHash(P, ASrcLen) mod FTableSize;
  Result := FTable^[AIndex];
  while Result <> nil do
  begin
    AResultLen := WordLength(Result);
    if (AResultLen = ASrcLen) and (CompareStringW(LangID, NORM_IGNORECASE, Result, AResultLen, P, ASrcLen) = CSTR_EQUAL) then
      Exit;
    Result := GetNextWord(Result);
  end;
end;

function TdxSpellCheckerWordList.NewWord(S: PWideChar; ALength: Integer): PWideChar;
begin
  Result := AllocWord(ALength);
  if UseDoubleMetaphone then
    UpdateMetaphoneInfo(Result, ALength);
  Inc(ALength); //include terminated null
  Move(S^, Result^, ALength * SizeOf(WideChar));
end;

function TdxSpellCheckerWordList.AllocWord(ACharCount: Cardinal): PWideChar;
var
  ASize: Integer;
begin
  ASize := (ACharCount + 1) * SizeOf(WideChar); //add terminated null
  Inc(ASize, SizeOf(TdxSpellCheckerWordStruct));
  GetMem(Result, ASize);
  PdxSpellCheckerWordStruct(Result)^.Size := ASize;
  PdxSpellCheckerWordStruct(Result)^.NextMetaphone := nil;
  PdxSpellCheckerWordStruct(Result)^.Next := nil;
  PdxSpellCheckerWordStruct(Result)^.Length := ACharCount;
  Result := ShiftPointer(Result, SizeOf(TdxSpellCheckerWordStruct));
end;

procedure TdxSpellCheckerWordList.DisposeWord(AWord: PWideChar);
begin
  if AWord <> nil then
    FreeMem(GetWordStruct(AWord));
end;

function TdxSpellCheckerWordList.GetNextMetaphoneWord(AWord: PWideChar): Pointer;
begin
  if AWord = nil then
    Result := nil
  else
    Result := PdxSpellCheckerWordStruct(GetWordStruct(AWord))^.NextMetaphone;
end;

function TdxSpellCheckerWordList.GetNextWord(AWord: PWideChar): Pointer;
begin
  if AWord = nil then
    Result := nil
  else
    Result := PdxSpellCheckerWordStruct(GetWordStruct(AWord))^.Next;
end;

procedure TdxSpellCheckerWordList.SetNextMetaphoneWord(AWord: PWideChar; AValue: Pointer);
begin
  if AWord <> nil then
    PdxSpellCheckerWordStruct(GetWordStruct(AWord))^.NextMetaphone := AValue;
end;

procedure TdxSpellCheckerWordList.SetNextWord(AWord: PWideChar; AValue: Pointer);
begin
  if AWord <> nil then
    PdxSpellCheckerWordStruct(GetWordStruct(AWord))^.Next := AValue;
end;

function TdxSpellCheckerWordList.WordLength(AWord: PWideChar): Integer;
begin
  if AWord = nil then
    Result := 0
  else
    Result := PdxSpellCheckerWordStruct(GetWordStruct(AWord))^.Length;
end;

function TdxSpellCheckerWordList.GetCodePage: Cardinal;
begin
  Result := LanguageToCodePage(FLangID);
end;

function TdxSpellCheckerWordList.GetWordStruct(AWord: PWideChar): Pointer;
begin
  Result := ShiftPointer(AWord, -SizeOf(TdxSpellCheckerWordStruct));
end;

procedure TdxSpellCheckerWordList.InitMetaphoneTable;
begin
  if not UseDoubleMetaphone then
    Exit;
  ZeroMemory(FMetaphoneTable, SizeOf(TdxMetaphoneTable));
end;

procedure TdxSpellCheckerWordList.SetUseDoubleMetaphone(AValue: Boolean);
begin
  if FUseDoubleMetaphone <> AValue then
  begin
    FUseDoubleMetaphone := AValue;
    if AValue then
    begin
      FMetaphoneTable := AllocMem(SizeOf(TdxMetaphoneTable));
      FMetaphone := TdxDoubleMetaphone.Create;
    end
    else
    begin
      FreeAndNil(FMetaphone);
      FreeMem(FMetaphoneTable);
      FMetaphoneTable := nil;
    end;
    UpdateWordsMetaphone;
  end;
end;

procedure TdxSpellCheckerWordList.UpdateMetaphoneInfo(ANewWord: PWideChar; ALength: Integer);
var
  AKey: Word;
  AWord, ATemp: PWideChar;
begin
  FMetaphone.DoubleMetaphone(@FUpperCaseBuffer, ALength);
  AKey := FMetaphone.PrimaryKey;
  if AKey = 0 then Exit;
  AWord := FMetaphoneTable^[AKey];
  if AWord = nil then
    FMetaphoneTable^[AKey] := ANewWord
  else
  begin
    repeat
      ATemp := GetNextMetaphoneWord(AWord);
      if ATemp = nil then
      begin
        SetNextMetaphoneWord(AWord, ANewWord);
        Break;
      end;
      AWord := ATemp;
    until False;
  end;
end;

procedure TdxSpellCheckerWordList.UpdateWordsMetaphone;
var
  I, AWordLen: Integer;
  AWord: PWideChar;
begin
  if Count = 0 then Exit;
  for I := 0 to FTableSize - 1 do
  begin
    AWord := FTable^[I];
    while AWord <> nil do
    begin
      if UseDoubleMetaphone then
      begin
        AWordLen := WordLength(AWord);
        LCMapStringW(LangID, LCMAP_UPPERCASE, AWord, AWordLen, @FUpperCaseBuffer,
          SizeOf(FUpperCaseBuffer) div SizeOf(WideChar));
        UpdateMetaphoneInfo(AWord, AWordLen);
      end
      else
        SetNextMetaphoneWord(AWord, nil);
      AWord := GetNextWord(AWord);
    end;
  end;
end;


{ TdxSpellCheckerUndoItem }

constructor TdxSpellCheckerUndoItem.Create(const AMisspelledWord, AReplacement, APrevWord: string;
  const AMisspelledWordStart, AMisspelledWordFinish, AReplacementFinish: IdxSpellCheckerPosition);
begin
  inherited Create;
  FPrevWord := APrevWord;
  FReplacement := AReplacement;
  FMisspelledWord := AMisspelledWord;
  FMisspelledWordStart := AMisspelledWordStart;
  FMisspelledWordFinish := AMisspelledWordFinish;
  FReplacementFinish := AReplacementFinish;
end;

{ TdxDictionaryLoadThread }

constructor TdxDictionaryLoadThread.Create(
  ADictionary: TdxCustomSpellCheckerDictionary);
begin
  inherited Create(False);
  FDictionary := ADictionary;
  Priority := Dictionary.SpellChecker.ThreadedLoadPriority;
  OnTerminate := Dictionary.ThreadDone;
end;

function TdxDictionaryLoadThread.IsLoadComplete: Boolean;
begin
  Result := not Terminated and Dictionary.Loaded;
end;

procedure TdxDictionaryLoadThread.Execute;
begin
  FFinished := False;
  try
    ResetException;
    try
      with Dictionary do
      begin
        try
          Synchronize(UpdateLoadedOnLoadEvent);
          if not Loaded then
            FLoaded := ExecuteLoad and not Terminated;
        except
          FLoaded := False;
          raise;
        end;
        if Loaded then
          Synchronize(LoadingComplete);
      end;
    except
      HandleException;
    end;
  finally
    FFinished := True;
  end;
end;


{ TdxSpellCheckerSingleWordParser }

constructor TdxSpellCheckerSingleWordParser.Create(ASpellChecker: TdxCustomSpellChecker);
begin
  inherited Create;
  FSpellChecker  := ASpellChecker;
end;

destructor TdxSpellCheckerSingleWordParser.Destroy;
begin
  FSpellChecker.ReleaseCheckModeHelper(Self);
  inherited Destroy;
end;

function TdxSpellCheckerSingleWordParser.GetPrevWord: string;
begin
  Result := '';
end;

function TdxSpellCheckerSingleWordParser.WordExists(const AWord: string): Boolean;
begin
  Result := SpellChecker.HasWordInDictionaries(AWord);
end;

{ TdxCustomSpellCheckerDictionary }

constructor TdxCustomSpellCheckerDictionary.Create(ASpellChecker: TdxCustomSpellChecker);
begin
  inherited Create(ASpellChecker);
  FEnabled := True;
  FCodePage := CP_ACP;
  InitializeContent;
  Language := GetSystemDefaultLangID;
  FTimeout := dxDefaultSuggestionsTimeout;
end;

destructor TdxCustomSpellCheckerDictionary.Destroy;
begin
  Terminate;
  FLoadThread.Free;    //don't use FreeAndNil
  FLoadThread := nil;
  Unload;
  FreeContent;
  inherited Destroy;
end;

procedure TdxCustomSpellCheckerDictionary.Assign(Source: TPersistent);
begin
  if Source is TdxCustomSpellCheckerDictionary then
  begin
    FAlphabet := TdxCustomSpellCheckerDictionary(Source).Alphabet;
    FCodePage := TdxCustomSpellCheckerDictionary(Source).CodePage;
    FEnabled := TdxCustomSpellCheckerDictionary(Source).Enabled;
    FLanguage := TdxCustomSpellCheckerDictionary(Source).Language;
    FTimeout := TdxCustomSpellCheckerDictionary(Source).Timeout;
  end;
end;

procedure TdxCustomSpellCheckerDictionary.Activate;
begin
  DoActivate;
end;

procedure TdxCustomSpellCheckerDictionary.Load(
  AMode: TdxSpellCheckerDictionaryLoadMode = dlmDefault);
begin
  case AMode of
    dlmDefault:
      begin
        if SpellChecker.UseThreadedLoad then
          LoadUsingThread
        else
          DirectLoad;
      end;
    dlmDirectLoad:
      DirectLoad;
    dlmThreadedLoad:
      LoadUsingThread;
  end;
end;

procedure TdxCustomSpellCheckerDictionary.Unload;
begin
  if LoadThread <> nil then
    LoadThread.Terminate;
  if Loaded then
    FLoaded := not DoUnload;
  if not Loaded then
    Cleanup;
  Update;
end;

procedure TdxCustomSpellCheckerDictionary.Clear;
begin
  Enabled := False;
  Unload;
end;

procedure TdxCustomSpellCheckerDictionary.AfterLoad;
begin
end;

procedure TdxCustomSpellCheckerDictionary.BeforeLoad;
begin
end;

function TdxCustomSpellCheckerDictionary.CanLoad: Boolean;
begin
  Result := not Loaded and ((FLoadThread = nil) or FLoadThread.Finished);
end;

function TdxCustomSpellCheckerDictionary.CanUseDoubleMetaphone: Boolean;
begin
  Result := False;
end;

procedure TdxCustomSpellCheckerDictionary.Cleanup;
begin
end;

function TdxCustomSpellCheckerDictionary.CreateSuggestionBuilder: TdxSpellCheckerSuggestionBuilder;
begin
  Result := TdxNearMissStrategy.Create(Self);
end;

procedure TdxCustomSpellCheckerDictionary.DirectLoad;
begin
  if not CanLoad then Exit;
  ShowHourglassCursor;
  try
    try
      FLoaded := DoLoadingEvent or ExecuteLoad;
    except
      FLoaded := False;
      raise;
    end;
    if Loaded then
      LoadingComplete;
  finally
    HideHourglassCursor;
  end;
end;

procedure TdxCustomSpellCheckerDictionary.DoActivate;
begin
  Load;
  Enabled := True;
end;

function TdxCustomSpellCheckerDictionary.DoLoad: Boolean;
begin
  Result := False;
end;

procedure TdxCustomSpellCheckerDictionary.DoLoadedEvent;
begin
  if Assigned(FOnLoaded) then
    FOnLoaded(Self);
end;

function TdxCustomSpellCheckerDictionary.DoLoadingEvent: Boolean;
begin
  Result := False;
  if Assigned(FOnLoading) then
    FOnLoading(Self, Result);
end;

function TdxCustomSpellCheckerDictionary.DoUnload: Boolean;
begin
  Result := True;
end;

function TdxCustomSpellCheckerDictionary.ExecuteLoad: Boolean;
begin
  BeforeLoad;
  try
    Result := DoLoad;
  finally
    AfterLoad;
  end;
end;

procedure TdxCustomSpellCheckerDictionary.FreeContent;
begin
end;

function TdxCustomSpellCheckerDictionary.GetActiveAlphabet: string;
begin
  if Alphabet = '' then
    Result := CreateDefaultAlphabet(CodePage)
  else
    Result := Alphabet;
end;

function TdxCustomSpellCheckerDictionary.GetDisplayName: string;
begin
  Result := GetRegisteredDictionaryTypes.GetDescriptionByClass(ClassType);
  if Result = '' then
    Result := ClassName;
end;

procedure TdxCustomSpellCheckerDictionary.InitializeContent;
begin
end;

function TdxCustomSpellCheckerDictionary.IsEnglishLanguage: Boolean;
begin
  Result := (Language and $3FF) = LANG_ENGLISH;
end;

procedure TdxCustomSpellCheckerDictionary.LanguageChanged;
begin
end;

procedure TdxCustomSpellCheckerDictionary.LoadingComplete;
begin
  DoLoadedEvent;
  Update;
end;

function TdxCustomSpellCheckerDictionary.LoadingTerminated: Boolean;
begin
  Result := FTerminated or (FLoadThread <> nil) and FLoadThread.Terminated;
end;

procedure TdxCustomSpellCheckerDictionary.LoadUsingThread;
begin
  if not CanLoad then Exit;
  FreeAndNil(FLoadThread);
  FLoadThread := TdxDictionaryLoadThread.Create(Self);
end;

procedure TdxCustomSpellCheckerDictionary.PopulateCapitalizationSuggestions(
  ASuggestionBuilder: TdxSpellCheckerSuggestionBuilder; const AWord: string);
begin
end;

procedure TdxCustomSpellCheckerDictionary.PopulateMetaphoneSuggestions(
  ASuggestionBuilder: TdxSpellCheckerSuggestionBuilder; const AWord: string);
begin
end;

procedure TdxCustomSpellCheckerDictionary.Reload;
begin
  if Loaded then
  begin
    Unload;
    Load;
    Update;
  end;
end;

procedure TdxCustomSpellCheckerDictionary.Terminate;
begin
  FTerminated := True;
end;

procedure TdxCustomSpellCheckerDictionary.ThreadDone(Sender: TObject);
begin
  if not LoadThread.IsLoadComplete then
    Cleanup
  else
    SpellChecker.CheckCallEnabledDictionariesLoaded;
end;

procedure TdxCustomSpellCheckerDictionary.Update;
begin
  SpellChecker.UpdateByDictionary(Self);
end;

procedure TdxCustomSpellCheckerDictionary.UpdateLoadedOnLoadEvent;
begin
  FLoaded := DoLoadingEvent;
end;

procedure TdxCustomSpellCheckerDictionary.UpdateUsingMetaphone;
begin
end;

procedure TdxCustomSpellCheckerDictionary.UpdateWordChars(var AWordChars: string);
begin
end;

function TdxCustomSpellCheckerDictionary.GetActive: Boolean;
begin
  Result := Loaded and Enabled;
end;

function TdxCustomSpellCheckerDictionary.HasWord(const AWord: string): Boolean;
begin
  Result := False;
end;

procedure TdxCustomSpellCheckerDictionary.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    if Loaded then
      Update;
  end;
end;

procedure TdxCustomSpellCheckerDictionary.SetCodePage(AValue: Cardinal);
begin
  if FCodePage <> AValue then
  begin
    FCodePage := AValue;
    Reload;
  end;
end;

procedure TdxCustomSpellCheckerDictionary.SetLanguage(const AValue: DWORD);
begin
  if FLanguage <> AValue then
  begin
    FLanguage := AValue;
    LanguageChanged;
  end;
end;

{ TdxSpellCheckerDictionary }

procedure TdxSpellCheckerDictionary.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxSpellCheckerDictionary then
    FCheckCapitalization := TdxSpellCheckerDictionary(Source).CheckCapitalization;
end;

function TdxSpellCheckerDictionary.HasWord(const AWord: string): Boolean;
var
  AWordInDictionary: PWideChar;
begin
  AWordInDictionary := FWords.FindWord(AWord);
  Result := (AWordInDictionary <> nil);
  if Result and CheckCapitalization then
    Result := IsCorrectCapitalization(AWord, AWordInDictionary);
end;

function TdxSpellCheckerDictionary.CanUseDoubleMetaphone: Boolean;
begin
  Result := Words.UseDoubleMetaphone;
end;

procedure TdxSpellCheckerDictionary.Cleanup;
begin
  Words.Clear;
end;

function TdxSpellCheckerDictionary.DoUnload: Boolean;
begin
  Words.Clear;
  Result := True;
end;

procedure TdxSpellCheckerDictionary.FreeContent;
begin
  FWords.Free;
end;

procedure TdxSpellCheckerDictionary.InitializeContent;
begin
  FWords := TdxSpellCheckerWordList.Create(CodePage, 269683);
end;

function TdxSpellCheckerDictionary.IsCorrectCapitalization(const AWord, APattern: string): Boolean;
begin
  case GetWordCapitalizationType(APattern) of
    ctLower:
      Result := GetWordCapitalizationType(AWord) in [ctLower,ctCapitalized, ctUpper];
    ctUpper:
      Result := GetWordCapitalizationType(AWord) = ctUpper;
    ctCapitalized:
      Result := GetWordCapitalizationType(AWord) in [ctCapitalized, ctUpper];
    ctMixed:
      Result := (AWord = APattern) or (GetWordCapitalizationType(AWord) = ctUpper);
  else
    Result := True;
  end;
end;

procedure TdxSpellCheckerDictionary.LanguageChanged;
begin
  Words.LangID := Language;
  UpdateUsingMetaphone;
end;

procedure TdxSpellCheckerDictionary.PopulateCapitalizationSuggestions(
  ASuggestionBuilder: TdxSpellCheckerSuggestionBuilder; const AWord: string);
var
  AWordInDictionary: PWideChar;
begin
  if not CheckCapitalization then
    Exit;
  AWordInDictionary := Words.FindWord(AWord);
  if AWordInDictionary <> nil then
    ASuggestionBuilder.Suggestions.Add(AWordInDictionary, Self, 1);
end;

procedure TdxSpellCheckerDictionary.PopulateMetaphoneSuggestions(
  ASuggestionBuilder: TdxSpellCheckerSuggestionBuilder; const AWord: string);
begin
  Words.PopulateMetaphoneSuggestions(ASuggestionBuilder, AWord);
end;

procedure TdxSpellCheckerDictionary.UpdateUsingMetaphone;
begin
  FWords.UseDoubleMetaphone := IsEnglishLanguage and
    (SpellChecker.MetaphoneDistance > 1);
end;

function TdxSpellCheckerDictionary.GetWordCount: Integer;
begin
  Result := FWords.Count;
end;

procedure TdxSpellCheckerDictionary.SetCheckCapitalization(AValue: Boolean);
begin
  if FCheckCapitalization <> AValue then
  begin
    FCheckCapitalization := AValue;
    Words.FindMostValidCapitalizationInDuplicates := AValue;
    Reload;
  end;
end;

{ TdxUserSpellCheckerDictionary }

constructor TdxUserSpellCheckerDictionary.Create(
  ASpellChecker: TdxCustomSpellChecker);
begin
  inherited Create(ASpellChecker);
  FOptions := [udSaveOnUnload];
end;

procedure TdxUserSpellCheckerDictionary.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxUserSpellCheckerDictionary then
  begin
    DictionaryPath := TdxUserSpellCheckerDictionary(Source).DictionaryPath;
    Options := TdxUserSpellCheckerDictionary(Source).Options;
  end;
end;

procedure TdxUserSpellCheckerDictionary.AddWord(const AWord: string);
begin
  Words.Add(AWord);
  Update;
end;

procedure TdxUserSpellCheckerDictionary.LoadFromStrings(AStrings: TStrings);
begin
  Words.LoadFromStrings(AStrings);
  Update;
end;

procedure TdxUserSpellCheckerDictionary.SaveToStrings(AStrings: TStrings);
begin
  if AStrings = nil then Exit;
  Words.SaveToStrings(AStrings);
  if AStrings is TStringList then
    TStringList(AStrings).Sort;
end;

function TdxUserSpellCheckerDictionary.DoLoad: Boolean;
var
  AStrings: TStrings;
begin
  Result := True;
  if not FileExists(DictionaryPath) then
    if not (udFileMustExist in Options) then Exit;
  AStrings := TStringList.Create;
  try
    AStrings.LoadFromFile(DictionaryPath);
    Words.LoadFromStrings(AStrings);
  finally
    AStrings.Free;
  end;
end;

function TdxUserSpellCheckerDictionary.DoUnload: Boolean;
var
  AStrings: TStrings;
begin
  Result := True;
  if udSaveOnUnload in Options then
  begin
    AStrings := TStringList.Create;
    try
      Words.SaveToStrings(AStrings);
      try
        AStrings.SaveToFile(DictionaryPath);
      except
        if not (csDestroying in SpellChecker.ComponentState) then
          raise;
      end;
    finally
      AStrings.Free;
    end;
  end;
end;

function TdxUserSpellCheckerDictionary.GetDisplayName: string;
var
  AFileName: TFileName;
begin
  Result := inherited GetDisplayName;
  AFileName := SysUtils.ExtractFileName(DictionaryPath);
  if AFileName <> '' then
    Result := Format('%s (%s)', [Result, AFileName]);
end;

{ TdxSpellCheckerDictionaryItem }

destructor TdxSpellCheckerDictionaryItem.Destroy;
begin
  FreeAndNil(FDictionaryType);
  inherited Destroy;
end;

procedure TdxSpellCheckerDictionaryItem.Assign(Source: TPersistent);
begin
  if Source is TdxSpellCheckerDictionaryItem then
  begin
    DictionaryTypeClassName := TdxSpellCheckerDictionaryItem(Source).DictionaryTypeClassName;
    DictionaryType := TdxSpellCheckerDictionaryItem(Source).DictionaryType;
  end;
end;

function TdxSpellCheckerDictionaryItem.GetDisplayName: string;
begin
  if FDictionaryType <> nil then
    Result := FDictionaryType.GetDisplayName
  else
    Result := inherited GetDisplayName;
end;

procedure TdxSpellCheckerDictionaryItem.RecreateDictionaryType;
begin
  FreeAndNil(FDictionaryType);
  if FDictionaryTypeClass <> nil then
    FDictionaryType := FDictionaryTypeClass.Create(Collection.SpellChecker);
end;

function TdxSpellCheckerDictionaryItem.GetCollection: TdxSpellCheckerDictionaries;
begin
  Result := TdxSpellCheckerDictionaries(inherited Collection);
end;

function TdxSpellCheckerDictionaryItem.GetDictionaryTypeClassName: string;
begin
  if FDictionaryType = nil then
    Result := ''
  else
    Result := FDictionaryType.ClassName;
end;

procedure TdxSpellCheckerDictionaryItem.SetDictionaryType(
  AValue: TdxCustomSpellCheckerDictionary);
begin
  if (FDictionaryType <> nil) and (AValue <> nil) then
    FDictionaryType.Assign(AValue);
end;

procedure TdxSpellCheckerDictionaryItem.SetDictionaryTypeClass(
  AValue: TdxCustomSpellCheckerDictionaryClass);
begin
  if FDictionaryTypeClass <> AValue then
  begin
    FDictionaryTypeClass := AValue;
    RecreateDictionaryType;
  end;
end;

procedure TdxSpellCheckerDictionaryItem.SetDictionaryTypeClassName(
  const AValue: string);
begin
  if not SameText(DictionaryTypeClassName, AValue) then
  begin
    with GetRegisteredDictionaryTypes do
      DictionaryTypeClass := TdxCustomSpellCheckerDictionaryClass(FindByClassName(AValue));
  end;
end;

{ TdxSpellCheckerDictionaries }

constructor TdxSpellCheckerDictionaries.Create(ASpellChecker: TdxCustomSpellChecker);
begin
  inherited Create(TdxSpellCheckerDictionaryItem);
  FSpellChecker := ASpellChecker;
end;

function TdxSpellCheckerDictionaries.Add: TdxSpellCheckerDictionaryItem;
begin
  Result := TdxSpellCheckerDictionaryItem(inherited Add);
end;

function TdxSpellCheckerDictionaries.GetNamePath: string;
var
  S: string;
begin
  S := SpellChecker.Name;
  if S = '' then
    S := SpellChecker.GetNamePath;
  Result := S + '.' + 'Items';
end;

function TdxSpellCheckerDictionaries.GetOwner: TPersistent;
begin
  Result := SpellChecker;
end;

function TdxSpellCheckerDictionaries.GetItem(
  Index: Integer): TdxSpellCheckerDictionaryItem;
begin
  Result := TdxSpellCheckerDictionaryItem(inherited Items[Index]);
end;

procedure TdxSpellCheckerDictionaries.SetItem(Index: Integer;
  AValue: TdxSpellCheckerDictionaryItem);
begin
  inherited Items[Index] := AValue;
end;


{ TdxSpellCheckerSuggestionCache }

constructor TdxSpellCheckerSuggestionCache.Create(AMaxCapacity: Integer);
begin
  inherited Create;
  FMaxCapacity := AMaxCapacity;
  Capacity := AMaxCapacity;
end;

procedure TdxSpellCheckerSuggestionCache.Add(const AWord: string; ASuggestions: TdxSpellCheckerSuggestionList);
var
  AItem: TdxSpellCheckerSuggestionCacheItem;
  AIndex: Integer;
begin
  if Count = FMaxCapacity then
  begin
    AIndex := GetRareItemIndex;
    with Items[AIndex] do
    begin
      FWord := AWord;
      FRefCount := 0;
      FSuggestions.Assign(ASuggestions);
    end;
  end
  else
  begin
    AItem := TdxSpellCheckerSuggestionCacheItem.Create(AWord, ASuggestions);
    inherited Add(AItem);
  end;
end;

function TdxSpellCheckerSuggestionCache.Find(const AWord: string): TdxSpellCheckerSuggestionCacheItem;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if WideCompareStr(Result.Word, AWord) = 0 then
      Exit;
  end;
  Result := nil;
end;

function TdxSpellCheckerSuggestionCache.GetItem(Index: Integer): TdxSpellCheckerSuggestionCacheItem;
begin
  Result := TdxSpellCheckerSuggestionCacheItem(inherited Items[Index]);
end;

function TdxSpellCheckerSuggestionCache.GetRareItemIndex: Integer;
var
  I, AValue: Integer;
begin
  Result := 0;
  AValue := MaxInt;
  for I := 0 to Count - 1 do
  begin
    if Items[I].RefCount < AValue then
    begin
      AValue := Items[I].RefCount;
      Result := I;
      if AValue = 0 then
        Exit;
    end;
  end;
end;

function TdxSpellCheckerSuggestionCache.GetSuggestions(const AWord: string; ASuggestions: TdxSpellCheckerSuggestionList): Boolean;
var
  AItem: TdxSpellCheckerSuggestionCacheItem;
begin
  AItem := Find(AWord);
  Result := AItem <> nil;
  if Result then
    AItem.GetSuggestions(ASuggestions);
end;

procedure TdxSpellCheckerSuggestionCache.Reset;
begin
  Clear;
end;

{ TdxSpellCheckerAutoCorrectOptions }

function TdxSpellCheckerAutoCorrectOptions.GetReplacement(const AText: string; out AReplacement: string): Boolean;

  function WordContainsOnlyAlpha(const S: string): Boolean;
  var
    I: Integer;
    ALen: Integer;
  begin
    ALen := Length(S);
    Result := ALen > 0;
    if Result then
      for I := 1 to ALen do
      begin
        Result := dxWideIsAlpha(S[I]);
        if not Result then
          Break;
      end;
  end;

var
  AItem: TdxSpellCheckerReplacement;
  ASuggestions: TdxSpellCheckerSuggestionList;
begin
  AItem := Replacements.FindReplacement(AText);
  Result := AItem <> nil;
  if Result then
    AReplacement := AItem.Replacement
  else
    if AutomaticallyUseSuggestions and WordContainsOnlyAlpha(AText) then
    begin
      ASuggestions := SpellChecker.GetSuggestions(AText);
      try
        Result := ASuggestions.Count = 1;
        if Result then
          AReplacement := ASuggestions.Items[0].Word;
      finally
        FreeAndNil(ASuggestions);
      end;
    end;
end;

procedure TdxSpellCheckerAutoCorrectOptions.DoActiveChanged;
begin
  SpellChecker.AutoCorrectManager.DoActiveChanged;
end;

procedure TdxSpellCheckerAutoCorrectOptions.DoChanged;
begin
  inherited DoChanged;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TdxSpellCheckerAutoCorrectOptions.GetSpellChecker: TdxCustomSpellChecker;
begin
  Result := TdxCustomSpellChecker(Owner);
end;


{ TdxSpellCheckerSuggestionCacheItem }

constructor TdxSpellCheckerSuggestionCacheItem.Create(const AWord: string; ASuggestions: TdxSpellCheckerSuggestionList);
begin
  inherited Create;
  FSuggestions := TdxSpellCheckerSuggestionList.Create;
  FSuggestions.Assign(ASuggestions);
  FWord := AWord;
end;

destructor TdxSpellCheckerSuggestionCacheItem.Destroy;
begin
  FreeAndNil(FSuggestions);
  inherited Destroy;
end;

procedure TdxSpellCheckerSuggestionCacheItem.GetSuggestions(ASuggestions: TdxSpellCheckerSuggestionList);
begin
  Inc(FRefCount);
  ASuggestions.Assign(FSuggestions);
end;

{ TdxSpellCheckerSpellingOptions }

procedure TdxSpellCheckerSpellingOptions.Assign(Source: TPersistent);
begin
  if Source is TdxSpellCheckerSpellingOptions then
  begin
    FIgnoreMixedCaseWords := TdxSpellCheckerSpellingOptions(Source).IgnoreMixedCaseWords;
    FIgnoreUpperCaseWords := TdxSpellCheckerSpellingOptions(Source).IgnoreUpperCaseWords;
    FIgnoreMarkupTags := TdxSpellCheckerSpellingOptions(Source).IgnoreMarkupTags;
    FIgnoreRepeatedWords := TdxSpellCheckerSpellingOptions(Source).IgnoreRepeatedWords;
    FCheckSelectedTextFirst := TdxSpellCheckerSpellingOptions(Source).CheckSelectedTextFirst;
    FIgnoreUrls := TdxSpellCheckerSpellingOptions(Source).IgnoreUrls;
    FCheckFromCursorPos := TdxSpellCheckerSpellingOptions(Source).CheckFromCursorPos;
    FIgnoreEmails := TdxSpellCheckerSpellingOptions(Source).IgnoreEmails;
    FIgnoreWordsWithNumbers := TdxSpellCheckerSpellingOptions(Source).IgnoreWordsWithNumbers;
  end;
end;

procedure TdxSpellCheckerSpellingOptions.DoChanged;
begin
  inherited DoChanged;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TdxSpellCheckerSpellingOptions.InitializeOptions;
begin
  FIgnoreMixedCaseWords := True;
  FIgnoreUpperCaseWords := True;
  FIgnoreMarkupTags := True;
  FIgnoreRepeatedWords := False;
  FCheckSelectedTextFirst := True;
  FIgnoreUrls := True;
  FCheckFromCursorPos := False;
  FIgnoreEmails := True;
  FIgnoreWordsWithNumbers := True;
end;

procedure TdxSpellCheckerSpellingOptions.PopulateRules(ARules: TdxSpellCheckerRules; AHelper: IdxSpellCheckerCheckModeHelper);
begin
  if AHelper = nil then
    Exit;
  if not IgnoreRepeatedWords then
    ARules.Add(TdxSpellCheckerRepeatedWordsRule.Create(AHelper));
  if IgnoreEmails then
    ARules.Add(TdxSpellCheckerIgnoreEmailRule.Create(AHelper));
  if IgnoreMixedCaseWords then
    ARules.Add(TdxSpellCheckerIgnoreMixedCaseWordsRule.Create(AHelper));
  if IgnoreUpperCaseWords then
    ARules.Add(TdxSpellCheckerIgnoreUpperCaseWordsRule.Create(AHelper));
  if IgnoreUrls then
    ARules.Add(TdxSpellCheckerIgnoreUrlRule.Create(AHelper));
  if IgnoreWordsWithNumbers then
    ARules.Add(TdxSpellCheckerIgnoreWordsWithNumbersRule.Create(AHelper));
  ARules.Add(TdxSpellCheckerWordExistsRule.Create(AHelper));
end;

procedure TdxSpellCheckerSpellingOptions.SetCheckFromCursorPos(AValue: Boolean);
begin
  if FCheckFromCursorPos <> AValue then
  begin
    FCheckFromCursorPos := AValue;
    Changed;
  end;
end;

procedure TdxSpellCheckerSpellingOptions.SetCheckSelectedTextFirst(AValue: Boolean);
begin
  if FCheckSelectedTextFirst <> AValue then
  begin
    FCheckSelectedTextFirst := AValue;
    Changed;
  end;
end;

procedure TdxSpellCheckerSpellingOptions.SetIgnoreEmails(AValue: Boolean);
begin
  if FIgnoreEmails <> AValue then
  begin
    FIgnoreEmails := AValue;
    Changed;
  end;
end;

procedure TdxSpellCheckerSpellingOptions.SetIgnoreMarkupTags(AValue: Boolean);
begin
  if FIgnoreMarkupTags <> AValue then
  begin
    FIgnoreMarkupTags := AValue;
    Changed;
  end;
end;

procedure TdxSpellCheckerSpellingOptions.SetIgnoreMixedCaseWords(AValue: Boolean);
begin
  if FIgnoreMixedCaseWords <> AValue then
  begin
    FIgnoreMixedCaseWords := AValue;
    Changed;
  end;
end;

procedure TdxSpellCheckerSpellingOptions.SetIgnoreRepeatedWords(AValue: Boolean);
begin
  if FIgnoreRepeatedWords <> AValue then
  begin
    FIgnoreRepeatedWords := AValue;
    Changed;
  end;
end;

procedure TdxSpellCheckerSpellingOptions.SetIgnoreUpperCaseWords(AValue: Boolean);
begin
  if FIgnoreUpperCaseWords <> AValue then
  begin
    FIgnoreUpperCaseWords := AValue;
    Changed;
  end;
end;

procedure TdxSpellCheckerSpellingOptions.SetIgnoreUrls(AValue: Boolean);
begin
  if FIgnoreUrls <> AValue then
  begin
    FIgnoreUrls := AValue;
    Changed;
  end;
end;

procedure TdxSpellCheckerSpellingOptions.SetIgnoreWordsWithNumbers(AValue: Boolean);
begin
  if FIgnoreWordsWithNumbers <> AValue then
  begin
    FIgnoreWordsWithNumbers := AValue;
    Changed;
  end;
end;

{ TdxSpellCheckerCheckAsYouTypeOptions }

procedure TdxSpellCheckerCheckAsYouTypeOptions.Assign(Source: TPersistent);
begin
  if Source is TdxSpellCheckerCheckAsYouTypeOptions then
  begin
    PopupMenu := TdxSpellCheckerCheckAsYouTypeOptions(Source).PopupMenu;
    FModifyControlPopupMenu := TdxSpellCheckerCheckAsYouTypeOptions(Source).ModifyControlPopupMenu;
  end;
  inherited Assign(Source);
end;

procedure TdxSpellCheckerCheckAsYouTypeOptions.DoActiveChanged;
begin
  SpellChecker.CheckAsYouTypeManager.DoActiveChanged;
end;

procedure TdxSpellCheckerCheckAsYouTypeOptions.DoChanged;
begin
  inherited DoChanged;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TdxSpellCheckerCheckAsYouTypeOptions.InitializeOptions;
begin
  inherited InitializeOptions;
  FModifyControlPopupMenu := True;
end;

procedure TdxSpellCheckerCheckAsYouTypeOptions.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  if (AComponent = FPopupMenu) and (AOperation = opRemove) then
    PopupMenu := nil;
end;

function TdxSpellCheckerCheckAsYouTypeOptions.GetSpellChecker: TdxCustomSpellChecker;
begin
  Result := TdxCustomSpellChecker(Owner);
end;

procedure TdxSpellCheckerCheckAsYouTypeOptions.SetModifyControlPopupMenu(AValue: Boolean);
begin
  if AValue <> FModifyControlPopupMenu then
  begin
    FModifyControlPopupMenu := AValue;
    Changed;
  end;
end;

procedure TdxSpellCheckerCheckAsYouTypeOptions.SetPopupMenu(AValue: TComponent);
begin
  if FPopupMenu <> AValue then
  begin
    if (FPopupMenu <> nil) and not (csDestroying in FPopupMenu.ComponentState) then
      FPopupMenu.RemoveFreeNotification(SpellChecker);
    FPopupMenu := AValue;
    if FPopupMenu <> nil then
      FPopupMenu.FreeNotification(SpellChecker);
    Changed;
  end;
end;

{ TdxSpellCheckerIgnoreOnceListsManager }

constructor TdxSpellCheckerIgnoreOnceListsManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMap := TDictionary<TControl, IdxSpellCheckerIgnoreList>.Create;
end;

destructor TdxSpellCheckerIgnoreOnceListsManager.Destroy;
begin
  UnregisterAll;
  FreeAndNil(FMap);
  inherited Destroy;
end;

function TdxSpellCheckerIgnoreOnceListsManager.Get(AControl: TControl): IdxSpellCheckerIgnoreList;
begin
  if AControl = nil then
    Exit(nil);

  AControl := CheckControl(AControl);
  if not FMap.TryGetValue(AControl, Result) then
  begin
    Register(AControl, TdxSpellCheckerIgnoreList.Create);
    Result := FMap[AControl];
  end;
end;

procedure TdxSpellCheckerIgnoreOnceListsManager.Reset(AControl: TControl);
var
  AList: IdxSpellCheckerIgnoreList;
begin
  if FMap.TryGetValue(CheckControl(AControl), AList) then
    AList.Clear;
end;

procedure TdxSpellCheckerIgnoreOnceListsManager.Register(AControl: TControl; const AList: IdxSpellCheckerIgnoreList);
begin
  AControl := CheckControl(AControl);
  FMap.Add(AControl, AList);
  AControl.FreeNotification(Self);
end;

procedure TdxSpellCheckerIgnoreOnceListsManager.Unregister(AControl: TControl);
begin
  AControl := CheckControl(AControl);
  AControl.RemoveFreeNotification(Self);
  if FMap <> nil then
    FMap.Remove(AControl);
end;

procedure TdxSpellCheckerIgnoreOnceListsManager.UnregisterAll;
var
  AKey: TControl;
  AMap: TDictionary<TControl, IdxSpellCheckerIgnoreList>;
begin
  AMap := nil;
  ExchangePointers(FMap, AMap);
  try
    for AKey in AMap.Keys do
      Unregister(AKey);
    AMap.Clear;
  finally
    ExchangePointers(FMap, AMap);
  end;
end;

function TdxSpellCheckerIgnoreOnceListsManager.CheckControl(AControl: TControl): TControl;
var
  AInnerControl: IcxContainerInnerControl;
begin
  if Supports(AControl, IcxContainerInnerControl, AInnerControl) then
    Result := AInnerControl.ControlContainer
  else
    Result := AControl;
end;

procedure TdxSpellCheckerIgnoreOnceListsManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    if AComponent is TControl then
      Unregister(TControl(AComponent));
  end;
end;


{ TdxSpellCheckerPersistent }

constructor TdxSpellCheckerPersistent.Create(ASpellChecker: TdxCustomSpellChecker);
begin
  inherited Create;
  FSpellChecker := ASpellChecker;
end;

{ TdxSpellCheckerSuggestion }

constructor TdxSpellCheckerSuggestion.Create(const AWord: string;
  ADictionary: TdxCustomSpellCheckerDictionary; ADistance: Integer);
begin
  inherited Create;
  FDictionary := ADictionary;
  FWord := AWord;
  FDistance := ADistance;
end;

{ TdxSpellCheckerSuggestionList }

procedure TdxSpellCheckerSuggestionList.Add(const AWord: string; ADictionary: TdxCustomSpellCheckerDictionary; ADistance: Integer);
var
  AItem: TdxSpellCheckerSuggestion;
begin
  AItem := TdxSpellCheckerSuggestion.Create(AWord, ADictionary, ADistance);
  AItem.FOrder := Count;
  inherited Add(AItem);
end;

procedure TdxSpellCheckerSuggestionList.Assign(
  ASuggestions: TdxSpellCheckerSuggestionList);
var
  I: Integer;
  AItem: TdxSpellCheckerSuggestion;
begin
  Clear;
  for I := 0 to ASuggestions.Count - 1 do
  begin
    AItem := ASuggestions[I];
    Add(AItem.Word, AItem.Dictionary, AItem.Distance);
  end;
end;

procedure TdxSpellCheckerSuggestionList.Delete(AIndex: Integer);
begin
  Items[AIndex].Free;
  inherited Delete(AIndex);
end;

procedure TdxSpellCheckerSuggestionList.FixCapitalization(const AMask: string);
var
  I: Integer;
begin
  if Length(AMask) = 0 then Exit;
  case GetWordCapitalizationType(AMask) of
//    ctLower:
//      for I := 0 to Count - 1 do
//        with Items[I] do
//          FWord := WideLowerCase(FWord);
    ctUpper:
      for I := 0 to Count - 1 do
        with Items[I] do
          FWord := WideUpperCase(FWord);
    ctCapitalized:
      for I := 0 to Count - 1 do
        with Items[I] do
          if Length(FWord) > 0 then
            FWord[1] := WideUpperCase(FWord[1])[1];
  end;
end;

procedure TdxSpellCheckerSuggestionList.Insert(AIndex: Integer;
  const AWord: string; ADictionary: TdxCustomSpellCheckerDictionary; ADistance: Integer);
var
  AItem: TdxSpellCheckerSuggestion;
begin
  AItem := TdxSpellCheckerSuggestion.Create(AWord, ADictionary, ADistance);
  AItem.FOrder := Count;
  inherited Insert(AIndex, AItem);
end;

procedure TdxSpellCheckerSuggestionList.RemoveDuplicates;
var
  I: Integer;
begin
  SortByWord;
  I := 1;
  while I < Count do
  begin
    while (I < Count) and WideSameStr(Items[I].Word, Items[I - 1].Word) do
      Delete(I);
    Inc(I);
  end;
  SortByDistance;
end;

procedure TdxSpellCheckerSuggestionList.SaveToStrings(AStrings: TStrings);
var
  I: Integer;
begin
  AStrings.BeginUpdate;
  try
    AStrings.Clear;
    for I := 0 to Count - 1 do
      AStrings.AddObject(Items[I].Word, Pointer(Items[I].Distance));
  finally
    AStrings.EndUpdate;
  end;
end;

function CompareSuggestionsByDistance(Item1, Item2: Pointer): Integer;
begin
  Result := TdxSpellCheckerSuggestion(Item1).Distance - TdxSpellCheckerSuggestion(Item2).Distance;
  if Result = 0 then
    Result := TdxSpellCheckerSuggestion(Item1).FOrder - TdxSpellCheckerSuggestion(Item2).FOrder;
end;

procedure TdxSpellCheckerSuggestionList.SortByDistance;
begin
  Sort(CompareSuggestionsByDistance);
end;

function CompareSuggestionsByWord(Item1, Item2: Pointer): Integer;
begin
  Result := WideCompareText(TdxSpellCheckerSuggestion(Item1).Word, TdxSpellCheckerSuggestion(Item2).Word);
  if Result = 0 then
    Result := TdxSpellCheckerSuggestion(Item1).FOrder - TdxSpellCheckerSuggestion(Item2).FOrder;
end;

procedure TdxSpellCheckerSuggestionList.SortByWord;
begin
  Sort(CompareSuggestionsByWord);
end;

function TdxSpellCheckerSuggestionList.GetItem(
  Index: Integer): TdxSpellCheckerSuggestion;
begin
  Result := TdxSpellCheckerSuggestion(inherited Items[Index]);
end;

{ TdxCustomSpellChecker }

function IsContainer(AComponent: TComponent): Boolean;
begin
  Result := (AComponent is TCustomForm) or (AComponent is TCustomFrame) or (AComponent is TDataModule);
end;

function SpellCheckerExists(AOwner: TComponent; AInstance: TdxCustomSpellChecker): Boolean;
var
  I: Integer;
begin
  for I := 0 to AOwner.ComponentCount - 1 do
  begin
    if (AOwner.Components[I] <> AInstance) and (AOwner.Components[I] is TdxCustomSpellChecker) then
      Exit(True);
  end;
  Result := False;
end;

constructor TdxCustomSpellChecker.Create(AOwner: TComponent);
var
  AComponent: TComponent;
  I: Integer;
begin
  FCreating := True;
  if IsContainer(AOwner) and SpellCheckerExists(AOwner, Self) then
    SpellCheckerCreatingError(@sdxSpellCheckerMoreThanOne);
  for I := 0 to Application.ComponentCount - 1 do
  begin
    AComponent := Application.Components[I];
    if IsContainer(AComponent) and SpellCheckerExists(AComponent, Self) then
      SpellCheckerCreatingError(@sdxSpellCheckerMoreThanOne);
  end;
  FThreadedLoadPriority := tpLower;
  FCreating := False;
  inherited Create(AOwner);
  FAutoLoadDictionaries := False;
  FSpellingFormType := sftOutlook;
  FMetaphoneDistance := 3;
  FCheckingLock := TCriticalSection.Create;
  FDictionaryItems := TdxSpellCheckerDictionaries.Create(Self);
  FSimilarity := CreateSimilarity;
  FRules := TdxSpellCheckerRules.Create;
  FSuggestionCache := TdxSpellCheckerSuggestionCache.Create(256);
  FDialogLookAndFeel := TcxLookAndFeel.Create(nil);

  FSpellingOptions := CreateSpellingOptions;
  FSpellingOptions.AddChangedHandler(SpellingOptionsChangedHandler);
  FAutoCorrectOptions := CreateAutoCorrectOptions;
  FAutoCorrectOptions.AddChangedHandler(AutoCorrectOptionsChangedHandler);
  FAutoCorrectManager := CreateAutoCorrectManager;
  FCheckAsYouTypeOptions := CreateCheckAsYouTypeOptions;
  FCheckAsYouTypeOptions.AddChangedHandler(CheckAsYouTypeOptionsChangedHandler);
  FCheckAsYouTypeManager := CreateCheckAsYouTypeManager;

  SpellingOptionsChanged(ctHard);
  FChangeList := TdxSpellCheckerReplacementList.Create;
  FDeleteList := TdxSpellCheckerWordList.Create(LANG_SYSTEM_DEFAULT, 257);
  FIgnoreList := TdxSpellCheckerWordList.Create(LANG_SYSTEM_DEFAULT, 257);
  FIgnoreOnceLists := TdxSpellCheckerIgnoreOnceListsManager.Create(nil);
  TdxSpellCheckerInstance.SetInstance(Self);
end;

destructor TdxCustomSpellChecker.Destroy;
begin
  if not FCreating then
  begin
    TdxSpellCheckerInstance.SetInstance(nil);
    CheckAsYouTypeOptions.Active := False;
    UnloadDictionaries;
    FreeAndNil(FDictionaryItems);
    FreeAndNil(FIgnoreOnceLists);
    FreeAndNil(FSuggestionCache);
    FreeAndNil(FChangeList);
    FreeAndNil(FDeleteList);
    FreeAndNil(FIgnoreList);
    FreeAndNil(FSpellingOptions);
    FreeAndNil(FRules);
    FreeAndNil(FSimilarity);
    FreeAndNil(FCheckingLock);
    FreeAndNil(FCheckAsYouTypeManager);
    FreeAndNil(FCheckAsYouTypeOptions);
    FreeAndNil(FAutoCorrectOptions);
    FreeAndNil(FAutoCorrectManager);
    FreeAndNil(FDialogLookAndFeel);
  end;
  inherited Destroy;
end;

procedure TdxCustomSpellChecker.Check(var AText: AnsiString);
var
  ATempValue: string;
begin
  ATempValue := dxAnsiStringToString(AText);
  Check(ATempValue);
  AText := dxStringToAnsiString(ATempValue);
end;

procedure TdxCustomSpellChecker.Check(var AText: string);
var
  AEditAdapter: IdxSpellCheckerAdapter;
begin
  AEditAdapter := TdxSpellCheckerTextAdapter.Create(AText);
  InternalCheck(AEditAdapter);
  AText := (AEditAdapter as TdxSpellCheckerTextAdapter).Text;
end;

procedure TdxCustomSpellChecker.Check(var AText: WideString);
var
  ATempValue: string;
begin
  ATempValue := AText;
  Check(ATempValue);
  AText := ATempValue;
end;

procedure TdxCustomSpellChecker.Check(AEdit: TWinControl);
var
  AAdapter: IdxSpellCheckerAdapter;
  AIntf: IcxContainerInnerControl;
begin
  ClearIgnoreOnceList(AEdit);
  if Supports(AEdit, IcxContainerInnerControl, AIntf) then
    Check(AIntf.ControlContainer)
  else
    if TdxSpellCheckerAdapters.CreateAdapter(AEdit, AAdapter) then
      InternalCheck(AAdapter);
end;

procedure TdxCustomSpellChecker.CheckContainer(AContainer: TWinControl; ARecursive: Boolean);
begin
  if AContainer = nil then
    Exit;

  FCheckingContainer := True;
  try
    FCheckGroupMode := True;
    LastDialogResult := mrOk;
    try
      DoCheckContainer(AContainer, ARecursive);
    finally
      FCheckGroupMode := False;
    end;
    if LastDialogResult <> mrCancel then
      SpellingComplete;
  finally
    FCheckingContainer := False;
  end;
end;

procedure TdxCustomSpellChecker.AddToIgnoreList(const AWord: string);
begin
  IgnoreList.Add(AWord);
  SpellingOptionsChanged(ctLight);
end;

procedure TdxCustomSpellChecker.AddWordToUserDictionary(const AWord: string);
var
  AUserDictionary: TdxUserSpellCheckerDictionary;
begin
  AUserDictionary := FindFirstEnabledUserDictionary;
  if AUserDictionary <> nil then
  begin
    if not DoAddWord(AUserDictionary, AWord) then
      AUserDictionary.AddWord(AWord);
    SpellingOptionsChanged(ctLight);
  end;
end;

procedure TdxCustomSpellChecker.ClearIgnoreOnceList(AEdit: TWinControl);
begin
  IgnoreOnceLists.Reset(AEdit);
  SpellingOptionsChanged(ctHard);
end;

function TdxCustomSpellChecker.CanCheckControlInContainer(AControl: TWinControl): Boolean;
begin
  Result := TdxSpellCheckerAdapters.IsSupported(AControl);
end;

function TdxCustomSpellChecker.FindDictionaryByWord(const AWord: string): TdxCustomSpellCheckerDictionary;
var
  ADictionary: TdxCustomSpellCheckerDictionary;
  I: Integer;
begin
  for I := 0 to ActiveDictionaryCount - 1 do
  begin
    ADictionary := ActiveDictionaries[I];
    if ADictionary.HasWord(AWord) then
      Exit(ADictionary);
  end;
  Result := nil;
end;

function TdxCustomSpellChecker.FindFirstEnabledUserDictionary: TdxUserSpellCheckerDictionary;
var
  I: Integer;
  ADictionary: TdxUserSpellCheckerDictionary;
begin
  Result := nil;
  for I := 0 to UserDictionaryCount - 1 do
  begin
    ADictionary := UserDictionaries[I];
    if ADictionary.Enabled then
    begin
      Result := ADictionary;
      Break;
    end;
  end;
end;

function TdxCustomSpellChecker.GetSuggestions(const AWord: string): TdxSpellCheckerSuggestionList;
var
  ADictionary: TdxCustomSpellCheckerDictionary;
  I: Integer;
begin
  Result := TdxSpellCheckerSuggestionList.Create;
  if not HasWordInDictionaries(AWord) then
  begin
    if not SuggestionCache.GetSuggestions(AWord, Result) then
    begin
      ShowHourglassCursor;
      try
        for I := 0 to ActiveDictionaryCount - 1 do
        begin
          ADictionary := ActiveDictionaries[I];
          with ADictionary.CreateSuggestionBuilder do
          try
            AddSuggestions(AWord, Result);
          finally
            Free;
          end;
        end;
        Result.RemoveDuplicates;
        SuggestionCache.Add(AWord, Result);
      finally
        HideHourglassCursor;
      end;
    end;
    DoGetSuggestions(AWord, Result);
  end;
end;

function TdxCustomSpellChecker.HasWordInDictionaries(const AWord: string): Boolean;
begin
  Result := FindDictionaryByWord(AWord) <> nil;
end;

function TdxCustomSpellChecker.HasEnabledUserDictionary: Boolean;
begin
  Result := FindFirstEnabledUserDictionary <> nil;
end;

function TdxCustomSpellChecker.IsValidWord(const AWord: string): Boolean;
var
  AHelper: TdxSpellCheckerSingleWordParser;
begin
  AHelper := CreateSingleWordParser;
  try
    Result := InternalIsValidWord(AWord, AHelper);
  finally
    AHelper.Free;
  end;
end;

function TdxCustomSpellChecker.IsValidWord(const AWord: AnsiString): Boolean;
begin
  Result := IsValidWord(dxAnsiStringToString(AWord));
end;

procedure TdxCustomSpellChecker.PopulateLanguages(AList: TStrings);
var
  I: Integer;
  ADictionary: TdxCustomSpellCheckerDictionary;
  ALanguageName: string;
  ALangID: Cardinal;
begin
  for I := 0 to DictionaryItems.Count - 1 do
  begin
    ADictionary := Dictionaries[I];
    if ADictionary is TdxUserSpellCheckerDictionary then
      Continue;
    if ADictionary.Language <> 0 then
      ALangID := ADictionary.Language
    else
      ALangID := dxLanguages.GetDefaultLanguageLCID;
    ALanguageName := dxLanguages.NameFromLocaleID[ALangID];
    if AList.IndexOf(ALanguageName) < 0 then
      AList.AddObject(ALanguageName, Pointer(ALangID));
  end;
end;

procedure TdxCustomSpellChecker.ShowSpellingCompleteMessage;
begin
  ShowMessage(cxGetResourceString(@sdxSpellCheckerSpellingComplete));
end;

procedure TdxCustomSpellChecker.LoadDictionaries(AIgnoreDisabled: Boolean = True);
begin
  if UseThreadedLoad then
    LoadDictionariesUsingThread(AIgnoreDisabled)
  else
    LoadDictionariesDirect(AIgnoreDisabled);
end;

procedure TdxCustomSpellChecker.UnloadDictionaries;
var
  I: Integer;
begin
  ShowHourglassCursor;
  try
    for I := 0 to DictionaryCount - 1 do
      Dictionaries[I].Terminate;
    for I := 0 to DictionaryCount - 1 do
      Dictionaries[I].Unload;
  finally
    HideHourglassCursor;
  end;
end;

function TdxCustomSpellChecker.GetTwoWordsDistance(const AWord1, AWord2: string): Integer;
begin
  Result := Similarity.GetDistance(
    PWideChar(WideUpperCase(AWord1)), Length(AWord1),
    PWideChar(WideUpperCase(AWord2)), Length(AWord2)) div 2;
end;

procedure TdxCustomSpellChecker.CheckCallEnabledDictionariesLoaded;
begin
  CheckAsYouTypeManager.Refresh(ctHard);
  if ActiveDictionaryCount = EnabledDictionaryCount then
    DoEnabledDictionariesLoaded;
end;

procedure TdxCustomSpellChecker.CheckControlInContainer(AControl: TWinControl);
begin
  Check(AControl)
end;

function TdxCustomSpellChecker.CreateAutoCorrectManager: TdxSpellCheckerCustomAutoCorrectManager;
begin
  Result := TdxSpellCheckerAutoCorrectManager.Create(Self);
end;

function TdxCustomSpellChecker.CreateAutoCorrectOptions: TdxSpellCheckerAutoCorrectOptions;
begin
  Result := TdxSpellCheckerAutoCorrectOptions.Create(Self);
end;

function TdxCustomSpellChecker.CreateCheckAsYouTypeManager: TdxSpellCheckerCustomCheckAsYouTypeManager;
begin
  Result := TdxSpellCheckerCheckAsYouTypeManager.Create(Self);
end;

function TdxCustomSpellChecker.CreateCheckAsYouTypeOptions: TdxSpellCheckerCheckAsYouTypeOptions;
begin
  Result := TdxSpellCheckerCheckAsYouTypeOptions.Create(Self);
end;

function TdxCustomSpellChecker.CreateSimilarity: TdxStringSimilarityCalculator;
begin
  Result := TdxStringSimilarityCalculator.Create;
end;

function TdxCustomSpellChecker.CreateSingleWordParser: TdxSpellCheckerSingleWordParser;
begin
  Result := TdxSpellCheckerSingleWordParser.Create(Self);
end;

function TdxCustomSpellChecker.CreateSpellingOptions: TdxSpellCheckerSpellingOptions;
begin
  Result := TdxSpellCheckerSpellingOptions.Create(Self);
end;

function TdxCustomSpellChecker.DoAddWord(AUserDictionary: TdxUserSpellCheckerDictionary; const AWord: string): Boolean;
begin
  Result := False;
  if Assigned(FOnAddWord) then
    FOnAddWord(AUserDictionary, AWord, Result);
end;

procedure TdxCustomSpellChecker.DoAfterCheck;
begin
  if Assigned(FOnAfterCheck) then
    FOnAfterCheck(Self);
end;

function TdxCustomSpellChecker.DoAutoCorrect(ARule: TdxSpellCheckerAutoCorrectCustomRule;
  var AWordRange: TdxSpellCheckerAutoCorrectWordRange): Boolean;
begin
  Result := True;
  if Assigned(FOnAutoCorrect) then
    FOnAutoCorrect(Self, ARule, AWordRange, Result);
end;

procedure TdxCustomSpellChecker.DoAutoCorrectFinish(AControl: TWinControl);
begin
  if Assigned(OnAutoCorrectFinish) then
    OnAutoCorrectFinish(Self, AControl);
end;

function TdxCustomSpellChecker.DoAutoCorrectStart(AControl: TWinControl): Boolean;
begin
  Result := Assigned(AControl);
  if Result and Assigned(OnAutoCorrectStart) then
    OnAutoCorrectStart(Self, AControl, Result);
end;

procedure TdxCustomSpellChecker.DoBeforeCheck;
begin
  if Assigned(FOnBeforeCheck) then
    FOnBeforeCheck(Self);
end;

procedure TdxCustomSpellChecker.DoCheckAsYouTypeFinish(AControl: TWinControl);
begin
  if Assigned(OnCheckAsYouTypeFinish) then
    OnCheckAsYouTypeFinish(Self, AControl);
end;

function TdxCustomSpellChecker.DoCheckAsYouTypePopup(APopup: TComponent): Boolean;
begin
  Result := False;
  if Assigned(FOnCheckAsYouTypePopup) then
    FOnCheckAsYouTypePopup(Self, APopup, Result);
end;

function TdxCustomSpellChecker.DoCheckAsYouTypeStart(AControl: TWinControl): Boolean;
begin
  Result := Assigned(AControl);
  if Result and Assigned(OnCheckAsYouTypeStart) then
    OnCheckAsYouTypeStart(Self, AControl, Result);
end;

procedure TdxCustomSpellChecker.DoCheckContainer(AContainer: TWinControl; ARecursive: Boolean);

  function StopCheck(AContinue: Boolean): Boolean;
  begin
    Result := not AContinue or (LastDialogResult <> mrOk);
    if Result then
      LastDialogResult := mrCancel;
  end;

var
  I: Integer;
  AControl: TWinControl;
  AContinue: Boolean;
  L: TList;
begin
  L := TList.Create;
  try
    AContainer.GetTabOrderList(L);
    for I := 0 to L.Count - 1 do
    begin
      AControl := TWinControl(L[I]);
      if not AControl.CanFocus or (not ARecursive and (AControl.Parent <> AContainer)) then
        Continue;
      AContinue := True;
      if CanCheckControlInContainer(AControl) then
      begin
        AControl := GetInnerControlContainer(AControl);
        if DoCheckControlInContainer(AControl, AContinue) then
          CheckControlInContainer(AControl);
        if StopCheck(AContinue) then
          Break;
      end;
    end;
  finally
    L.Free;
  end;
end;

function TdxCustomSpellChecker.DoCheckControlInContainer(AControl: TWinControl; var AContinue: Boolean): Boolean;
begin
  Result := True;
  if Assigned(FOnCheckControlInContainer) then
    FOnCheckControlInContainer(Self, AControl, Result, AContinue);
end;

function TdxCustomSpellChecker.DoCheckWord(const AWord: string; var AValid: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnCheckWord) then
    FOnCheckWord(Self, AWord, AValid, Result);
end;

procedure TdxCustomSpellChecker.DoEnabledDictionariesLoaded;
begin
  if Assigned(FOnEnabledDictionariesLoaded) then
    FOnEnabledDictionariesLoaded(Self);
end;

procedure TdxCustomSpellChecker.DoDialogCheckModeChanged;
begin
  if not OnDialogCheckModeChanged.Empty then
    OnDialogCheckModeChanged.Invoke(Self);
end;

procedure TdxCustomSpellChecker.DoGetSuggestions(const AWord: string; ASuggestions: TdxSpellCheckerSuggestionList);
begin
  if Assigned(FOnGetSuggestions) then
    FOnGetSuggestions(Self, AWord, ASuggestions);
end;

function TdxCustomSpellChecker.DoSpellingComplete: Boolean;
begin
  Result := False;
  if Assigned(FOnSpellingComplete) then
    FOnSpellingComplete(Self, Result);
end;

function TdxCustomSpellChecker.GetDialogCheckModeClass: TdxSpellCheckerCustomCheckModeClass;
begin
  if sftWord = SpellingFormType then
    Result := TdxSpellCheckerWordCheckMode
  else
    Result := TdxSpellCheckerOutlookCheckMode;
end;

procedure TdxCustomSpellChecker.LoadDictionariesDirect(AIgnoreDisabled: Boolean = True);
var
  ADictionary: TdxCustomSpellCheckerDictionary;
  I: Integer;
begin
  ShowHourglassCursor;
  try
    for I := 0 to DictionaryCount - 1 do
    begin
      ADictionary := Dictionaries[I];
      if ADictionary.Enabled or not AIgnoreDisabled then
        ADictionary.Load;
    end;
  finally
    HideHourglassCursor;
  end;
end;

procedure TdxCustomSpellChecker.LoadDictionariesUsingThread(AIgnoreDisabled: Boolean = True);
var
  ADictionary: TdxCustomSpellCheckerDictionary;
  I: Integer;
begin
  for I := 0 to DictionaryCount - 1 do
  begin
    ADictionary := Dictionaries[I];
    if ADictionary.Enabled or not AIgnoreDisabled then
      ADictionary.LoadUsingThread;
  end;
end;

procedure TdxCustomSpellChecker.Loaded;
begin
  inherited Loaded;
  if AutoLoadDictionaries and not (csDesigning in ComponentState) then
    LoadDictionaries;
end;

procedure TdxCustomSpellChecker.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if CheckAsYouTypeOptions <> nil then
    CheckAsYouTypeOptions.Notification(AComponent, Operation);
end;

procedure TdxCustomSpellChecker.SpellingOptionsChanged(AChangeType: TdxChangeType = ctMedium);
begin
  ReleaseCheckModeHelper(FCheckModeHelper);
  CheckAsYouTypeManager.Refresh(AChangeType);
  if not OnSpellingOptionsChanged.Empty then
    OnSpellingOptionsChanged.Invoke(Self);
end;

procedure TdxCustomSpellChecker.UpdateByDictionary(ADictionary: TdxCustomSpellCheckerDictionary);
begin
  if CheckMode <> nil then
    CheckMode.UpdateByDictionary(ADictionary);
  if SuggestionCache <> nil then
    SuggestionCache.Reset;
  SpellingOptionsChanged(ctHard);
end;

procedure TdxCustomSpellChecker.InternalCheck(AAdapter: IdxSpellCheckerAdapter);
begin
  DoBeforeCheck;
  try
    if ActiveDictionaryCount = 0 then
      raise EdxSpellCheckerException.Create(cxGetResourceString(@sdxSpellCheckerNoActiveDictionaries));

    CheckAsYouTypeManager.BeginManualCheck;
    try
      FCheckMode := GetDialogCheckModeClass.Create(Self, AAdapter);
      try
        DoDialogCheckModeChanged;
        if not CheckMode.GetNextMisspelledWord then
        begin
          SpellingComplete;
          LastDialogResult := mrOk;
        end
        else
        begin
          LastDialogResult := (CheckMode as TdxSpellCheckerDialogCheckMode).ShowDialog;
          if LastDialogResult = mrOk then
            SpellingComplete;
        end;
      finally
        FreeAndNil(FCheckMode);
        DoDialogCheckModeChanged;
      end;
    finally
      CheckAsYouTypeManager.EndManualCheck;
    end;
  finally
    DoAfterCheck;
  end;
end;

function TdxCustomSpellChecker.InternalIsValidWord(const AWord: string): Boolean;
begin
  CheckingLock.Enter;
  try
    Result := False;
    if not DoCheckWord(AWord, Result) then
      Result := Rules.IsValid(AWord);
  finally
    CheckingLock.Leave;
  end;
end;

function TdxCustomSpellChecker.InternalIsValidWord(const AWord: string; AHelper: IdxSpellCheckerCheckModeHelper): Boolean;
begin
  CheckingLock.Enter;
  try
    SetCheckModeHelper(AHelper);
    Result := InternalIsValidWord(AWord);
  finally
    CheckingLock.Leave;
  end;
end;

procedure TdxCustomSpellChecker.ReleaseCheckModeHelper(AHelper: IdxSpellCheckerCheckModeHelper);
begin
  CheckingLock.Enter;
  try
    if AHelper = FCheckModeHelper then
      SetCheckModeHelper(nil);
  finally
    CheckingLock.Leave;
  end;
end;

procedure TdxCustomSpellChecker.SetCheckModeHelper(AHelper: IdxSpellCheckerCheckModeHelper);
begin
  CheckingLock.Enter;
  try
    if FCheckModeHelper <> AHelper then
    begin
      Rules.Clear;
      FCheckModeHelper := AHelper;
      SpellingOptions.PopulateRules(Rules, AHelper);
    end;
  finally
    CheckingLock.Leave;
  end;
end;

// IdxSpellChecker
procedure TdxCustomSpellChecker.CheckFinish;
begin
  CheckAsYouTypeManager.CheckFinish;
  AutoCorrectManager.CheckFinish;
end;

procedure TdxCustomSpellChecker.CheckStart(AControl: TWinControl);
begin
  CheckAsYouTypeManager.CheckStart(AControl);
  AutoCorrectManager.CheckStart(AControl);
end;

procedure TdxCustomSpellChecker.DrawMisspellings(AControl: TWinControl);
begin
  CheckAsYouTypeManager.DrawMisspellings(AControl);
end;

function TdxCustomSpellChecker.IsSpellCheckerDialogControl(AWnd: THandle): Boolean;
begin
  Result := CheckAsYouTypeManager.Active and (CheckAsYouTypeManager.ManualCheckCount > 0);
end;

procedure TdxCustomSpellChecker.KeyPress(AKey: Char);
begin
  AutoCorrectManager.KeyPress(AKey);
end;

function TdxCustomSpellChecker.QueryPopup(APopup: TComponent; const P: TPoint): Boolean;
begin
  Result := CheckAsYouTypeManager.QueryPopup(APopup, P);
end;

procedure TdxCustomSpellChecker.LayoutChanged(AControl: TWinControl);
begin
  CheckAsYouTypeManager.LayoutChanged(AControl);
end;

procedure TdxCustomSpellChecker.SelectionChanged(AControl: TWinControl);
begin
  CheckAsYouTypeManager.SelectionChanged(AControl);
end;

procedure TdxCustomSpellChecker.TextChanged(AControl: TWinControl);
begin
  CheckAsYouTypeManager.TextChanged(AControl);
end;

procedure TdxCustomSpellChecker.Undo;
begin
  AutoCorrectManager.Undo;
end;

// IdxSpellChecker2
procedure TdxCustomSpellChecker.KeyDown(AKey: Word; Shift: TShiftState);
begin
  CheckAsYouTypeManager.KeyDown(AKey, Shift);
end;

procedure TdxCustomSpellChecker.KeyUp(AKey: Word; Shift: TShiftState);
begin
  CheckAsYouTypeManager.KeyUp(AKey, Shift);
end;

// IdxSpellChecker3
function TdxCustomSpellChecker.AutoCorrect(
  AControl: TWinControl; const AController: IdxSpellCheckTextController;
  var AInfo: TdxSpellCheckerAutoCorrectWordInfo): Boolean;
var
  AEngine: TdxSpellCheckerAutoCorrectEngine;
begin
  Result := False;
  if DoAutoCorrectStart(AControl) then
  try
    AEngine := TdxSpellCheckerAutoCorrectEngine.Create(Self, AController);
    try
      AEngine.Check(AInfo);
      Result := AInfo.Rule <> nil;
    finally
      AEngine.Free;
    end;
  finally
    DoAutoCorrectFinish(AControl);
  end;
end;

function TdxCustomSpellChecker.GetAutoCorrectOptions: TdxSpellCheckerCustomAutoCorrectOptions;
begin
  Result := AutoCorrectOptions;
end;

function TdxCustomSpellChecker.GetCheckAsYouTypeOptions: TdxSpellCheckerCustomCheckAsYouTypeOptions;
begin
  Result := CheckAsYouTypeOptions;
end;

function TdxCustomSpellChecker.CheckAsync(AControl: TWinControl; const AController: IdxSpellCheckTextController;
  const AFrom, ATo: IdxSpellCheckerPosition; AProc: IdxSpellChecker3CheckCallbackProc; const APrevWord: string = ''): Boolean;
const
  Map: array[TdxSpellCheckerSpellingCode] of TdxSpellingError = (seUnknown, seMisspelling, seRepeating);
var
  ACheckMode: TdxSpellCheckerAbstractCheckMode;
begin
  Result := False;
  if DoCheckAsYouTypeStart(AControl) then
  try
    ACheckMode := TdxSpellCheckerAbstractCheckMode.Create(Self);
    try
      ACheckMode.FController := AController;
      ACheckMode.FIgnoreOnceList := IgnoreOnceLists.Get(AControl);
      ACheckMode.FSpellingStart := AFrom;
      ACheckMode.FSpellingFinish := ATo;
      ACheckMode.FMisspellingStart := ACheckMode.FSpellingStart;
      ACheckMode.FMisspellingFinish := ACheckMode.FSpellingStart;
      ACheckMode.PrevWord := APrevWord;

      Result := True;
      while Result and ACheckMode.GetNextMisspelledWord do
      begin
        Result := AProc(TdxSpellingErrorInfo.Create(ACheckMode.MisspelledWord,
          ACheckMode.FMisspellingStart, ACheckMode.FMisspellingFinish, Map[ACheckMode.LastCode]));
        ACheckMode.Skip;
      end;
    finally
      ACheckMode.Free;
    end;
  finally
    DoCheckAsYouTypeFinish(AControl);
  end;
end;

function TdxCustomSpellChecker.GetSuggestionsEx(const AWord: string): TArray<string>;
var
  AList: TdxSpellCheckerSuggestionList;
  I: Integer;
begin
  AList := GetSuggestions(AWord);
  try
    SetLength(Result, AList.Count);
    for I := 0 to AList.Count - 1 do
      Result[I] := AList[I].Word;
  finally
    AList.Free;
  end;
end;

function TdxCustomSpellChecker.IsDialogCheckMode: Boolean;
begin
  Result := CheckMode is TdxSpellCheckerDialogCheckMode;
end;

procedure TdxCustomSpellChecker.IgnoreAll(AControl: TControl; const AWord: string);
begin
  AddToIgnoreList(AWord);
end;

procedure TdxCustomSpellChecker.IgnoreOnce(AControl: TControl;
  const AWord: string; const AStart, AEnd: IdxSpellCheckerPosition);
begin
  IgnoreOnce(IgnoreOnceLists.Get(AControl), AWord, AStart, AEnd);
end;

procedure TdxCustomSpellChecker.IgnoreOnce(AList: IdxSpellCheckerIgnoreList;
  const AWord: string; const AStart, AEnd: IdxSpellCheckerPosition);
begin
  if AList <> nil then
  begin
    AList.Add(AStart, AEnd, AWord);
    SpellingOptionsChanged(ctLight);
  end;
end;

procedure TdxCustomSpellChecker.RegisterIgnoreList(AControl: TControl; const AIgnoreList: IdxSpellCheckerIgnoreList);
begin
  IgnoreOnceLists.Register(AControl, AIgnoreList);
end;

procedure TdxCustomSpellChecker.UnregisterIgnoreList(AControl: TControl);
begin
  IgnoreOnceLists.Unregister(AControl);
end;

procedure TdxCustomSpellChecker.AddDialogCheckModeChangeHandler(AEvent: TNotifyEvent);
begin
  OnDialogCheckModeChanged.Add(AEvent);
end;

procedure TdxCustomSpellChecker.RemoveDialogCheckModeChangeHandler(AEvent: TNotifyEvent);
begin
  OnDialogCheckModeChanged.Remove(AEvent);
end;

procedure TdxCustomSpellChecker.AddSpellingOptionsChangedHandler(AEvent: TNotifyEvent);
begin
  OnSpellingOptionsChanged.Add(AEvent);
end;

procedure TdxCustomSpellChecker.RemoveSpellingOptionsChangedHandler(AEvent: TNotifyEvent);
begin
  OnSpellingOptionsChanged.Remove(AEvent);
end;

procedure TdxCustomSpellChecker.SpellingComplete;
begin
  if not FCheckGroupMode and not DoSpellingComplete then
    ShowSpellingCompleteMessage;
end;

procedure TdxCustomSpellChecker.AutoCorrectOptionsChangedHandler(Sender: TObject);
begin
  if AutoCorrectManager <> nil then
    AutoCorrectManager.DoOptionsChanged;
end;

procedure TdxCustomSpellChecker.CheckAsYouTypeOptionsChangedHandler(Sender: TObject);
begin
  if CheckAsYouTypeManager <> nil then
    CheckAsYouTypeManager.DoOptionsChanged;
end;

procedure TdxCustomSpellChecker.SpellingOptionsChangedHandler(Sender: TObject);
begin
  SpellingOptionsChanged(ctHard);
end;

function TdxCustomSpellChecker.GetActiveDictionary(Index: Integer): TdxCustomSpellCheckerDictionary;
var
  I, J: Integer;
  ADictionary: TdxCustomSpellCheckerDictionary;
begin
  Result := nil;
  J := 0;
  for I := 0 to DictionaryItems.Count - 1 do
  begin
    ADictionary := DictionaryItems[I].DictionaryType;
    if (ADictionary <> nil) and (ADictionary.Active) then
    begin
      if J = Index then
      begin
        Result := ADictionary;
        Exit;
      end;
      Inc(J);
    end;
  end;
end;

function TdxCustomSpellChecker.GetActiveDictionaryCount: Integer;
var
  I: Integer;
  ADictionary: TdxCustomSpellCheckerDictionary;
begin
  Result := 0;
  for I := 0 to DictionaryItems.Count - 1 do
  begin
    ADictionary := DictionaryItems[I].DictionaryType;
    if (ADictionary <> nil) and (ADictionary.Active) then
      Inc(Result);
  end;
end;

function TdxCustomSpellChecker.GetCheckMode: TdxSpellCheckerCustomCheckMode;
begin
  Result := FCheckMode;
  if (Result = nil) and Assigned(CheckAsYouTypeManager) and CheckAsYouTypeManager.Active then
    Result := CheckAsYouTypeManager.CheckMode;
end;

function TdxCustomSpellChecker.GetDictionary(
  Index: Integer): TdxCustomSpellCheckerDictionary;
var
  I, J: Integer;
  ADictionary: TdxCustomSpellCheckerDictionary;
begin
  Result := nil;
  J := 0;
  for I := 0 to DictionaryItems.Count - 1 do
  begin
    ADictionary := DictionaryItems[I].DictionaryType;
    if ADictionary <> nil then
    begin
      if J = Index then
      begin
        Result := ADictionary;
        Exit;
      end;
      Inc(J);
    end;
  end;
end;

function TdxCustomSpellChecker.GetDictionaryCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to DictionaryItems.Count - 1 do
    if DictionaryItems[I].DictionaryType <> nil then
      Inc(Result);
end;

function TdxCustomSpellChecker.GetEnabledDictionary(
  Index: Integer): TdxCustomSpellCheckerDictionary;
var
  I, J: Integer;
  ADictionary: TdxCustomSpellCheckerDictionary;
begin
  Result := nil;
  J := 0;
  for I := 0 to DictionaryItems.Count - 1 do
  begin
    ADictionary := DictionaryItems[I].DictionaryType;
    if (ADictionary <> nil) and (ADictionary.Enabled) then
    begin
      if J = Index then
      begin
        Result := DictionaryItems[I].DictionaryType;
        Exit;
      end;
      Inc(J);
    end;
  end;
end;

function TdxCustomSpellChecker.GetEnabledDictionaryCount: Integer;
var
  I: Integer;
  ADictionary: TdxCustomSpellCheckerDictionary;
begin
  Result := 0;
  for I := 0 to DictionaryItems.Count - 1 do
  begin
    ADictionary := DictionaryItems[I].DictionaryType;
    if (ADictionary <> nil) and (ADictionary.Enabled) then
      Inc(Result);
  end;
end;

function TdxCustomSpellChecker.GetUserDictionary(
  Index: Integer): TdxUserSpellCheckerDictionary;
var
  I, J: Integer;
begin
  Result := nil;
  J := 0;
  for I := 0 to DictionaryItems.Count - 1 do
  begin
    if DictionaryItems[I].DictionaryType is TdxUserSpellCheckerDictionary then
    begin
      if J = Index then
      begin
        Result := TdxUserSpellCheckerDictionary(DictionaryItems[I].DictionaryType);
        Exit;
      end;
      Inc(J);
    end;
  end;
end;

function TdxCustomSpellChecker.GetUserDictionaryCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to DictionaryItems.Count - 1 do
    if DictionaryItems[I].DictionaryType is TdxUserSpellCheckerDictionary then
      Inc(Result);
end;

function TdxCustomSpellChecker.GetWordChars: string;
var
  ADictionary: TdxCustomSpellCheckerDictionary;
  I: Integer;
begin
  Result := '''';
  for I := 0 to DictionaryItems.Count - 1 do
  begin
    ADictionary := DictionaryItems[I].DictionaryType;
    if (ADictionary <> nil) and ADictionary.Active then
      ADictionary.UpdateWordChars(Result);
  end;
end;

procedure TdxCustomSpellChecker.SetAutoCorrectOptions(AValue: TdxSpellCheckerAutoCorrectOptions);
begin
  FAutoCorrectOptions.Assign(AValue);
end;

procedure TdxCustomSpellChecker.SetCheckAsYouTypeOptions(
  AValue: TdxSpellCheckerCheckAsYouTypeOptions);
begin
  FCheckAsYouTypeOptions.Assign(AValue);
end;

procedure TdxCustomSpellChecker.SetDialogLookAndFeel(AValue: TcxLookAndFeel);
begin
  FDialogLookAndFeel.Assign(AValue);
end;

procedure TdxCustomSpellChecker.SetDictionaryItems(
  AValue: TdxSpellCheckerDictionaries);
begin
  FDictionaryItems.Assign(AValue);
end;

procedure TdxCustomSpellChecker.SetMetaphoneDistance(AValue: Integer);
var
  I: Integer;
begin
  if AValue < 1 then
    AValue := 1;
  if AValue > 5 then
    AValue := 5;
  if FMetaphoneDistance <> AValue then
  begin
    FMetaphoneDistance := AValue;
    for I := 0 to DictionaryCount - 1 do
      Dictionaries[I].UpdateUsingMetaphone;
  end;
end;

procedure TdxCustomSpellChecker.SetSpellingOptions(AValue: TdxSpellCheckerSpellingOptions);
begin
  FSpellingOptions.Assign(AValue);
end;


initialization
  GetRegisteredDictionaryTypes.Register(TdxUserSpellCheckerDictionary, cxGetResourceString(@sdxSpellCheckerUserDictionary));

finalization
  FreeRegisteredDictionaryTypes;
end.
