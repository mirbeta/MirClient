{ *************************************************************************** }
{ TMS Spell Check component                                                   }
{ for Delphi & C++Builder                                                     }
{                                                                             }
{ written by TMS Software                                                     }
{ copyright © 2015                                                            }
{ Email : info@tmssoftware.com                                                }
{ Web : http://www.tmssoftware.com                                            }
{                                                                             }
{ The source code is given as is. The author is not responsible               }
{ for any possible damage done due to the use of this code.                   }
{ The component can be freely used in any application. The complete           }
{ source code remains property of the author and may not be distributed,      }
{ published, given or sold in any form as such. No parts of the source        }
{ code can be included in any other component or application without          }
{ written authorization of the author.                                        }
{ *************************************************************************** }

unit TMSSpellCheck;

{$I TMSDEFS.INC}

interface

uses
  Classes, Types, DB, DBClient, SysUtils, Variants,
  SyncObjs, XMLDoc, XMLDom, XMLIntf, Generics.Collections,
  TMSSpellInfos, TMSSpellCheckUtil, TMSSpellSoundEx,
{$IFDEF FMXLIB}
  FMX.Forms, FMX.Controls, FMX.ExtCtrls, FMX.Types, FMX.TMSBaseControl, System.IOUtils
{$ENDIF}
{$IFNDEF FMXLIB}
  Forms, Controls, ExtCtrls, Windows
{$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 0; // Build nr.
  DATE_VER = 'Sep, 2015'; // Month version

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed : Dictionary handling issue with Spanish
  // v1.0.0.2 : Fixed : German language handling
  // v1.0.1.0 : New : Support for Delphi XE8 & C++Builder XE8 Prof, Ent. Architect added
  // v1.0.1.1 : Fixed : Memory leak with TAdvSpellCheckCorrectLinesDialog
  //          : Fixed : Warnings with 64bit compilation
  // v1.0.2.0 : New : RAD Studio 10 Seattle support 

type
  TSPObject = TObject;
  TCustomAdvSpellCheck = class;
  TAdvSuggestionContext = class;
  TAdvSpellCheckCallbackContext = class;
  TAdvSpellcheckRequestGroup = class;
  TSpellCheckLanguagePack = class;
  TAdvValidationContext = class;
  TAdvSpellEvent = (asplOnBeforeBeginRequest, aspOnRequestsProcessed,
    asplOnBeforeEndRequest, asplOnAfterBeginRequest, asplOnAfterEndRequest,
    asplOnRequestGroupResult, asplOnRequestResult,
    asplOnBeforeLoadIgnoreListText, asplOnAfterLoadIgnoreListText,
    asplOnBeforeAddToIgnoreWord, asplOnAfterAddToIgnoreWord,
    asplOnBeforeRemoveFromIgnoreWord, asplOnAfterRemoveFromignoreWord,
    asplOnBeforeSaveConfig, asplOnAfterSaveConfig, asplOnBeforeLoadConfig,
    asplOnAfterLoadConfig, asplOnBeforeSaveIgnoreList,
    asplOnAfterSaveIgnoreList, asplOnBeforeLoadIgnoreList,
    asplOnAfterLoadIgnoreList, asplOnBeforeSaveDB, asplOnAfterSaveDB,
    asplOnBeforeLoadDB, asplOnAfterLoadDB, asplOnBeforeAppendWordsToDictionary,
    asplOnAfterAppendWordsToDictionary, asplOnBeforeRefreshDictionary,
    asplOnAfterRefreshDictionary, asplOnBeforeOpen, asplOnAfterOpen,
    asplOnBeforeClose, asplOnAfterClose, asplOnBeforeSave, asplOnAfterLoad,
    asplOnBeforeLoad, asplOnAfterSave, asplOnBeforeCleanupDictionary,
    asplOnAfterCleanupDictionary, asplOnBeforeGetSuggestions,
    asplOnAfterGetSuggestions, asplOnAfterGetValidation,
    asplOnBeforeGetValidation);

  TAdvSpellEventsList = set of TAdvSpellEvent;

  TAdvSpellcheckSynchronizedEvent = class
  private
    FEventType: TAdvSpellEvent;
    FParent: TCustomAdvSpellCheck;
  public
    Parameters: array [0 .. 20] of Pointer;
    Event: procedure of Object;
    property EventType: TAdvSpellEvent read FEventType write FEventType;
    procedure ArrayOf(const Values: array of TSPObject);
    procedure Execute();
    procedure RunEvent();
    constructor Create(AParent: TCustomAdvSpellCheck);
    destructor Destroy(); override;
  end;

  TProcessRequestContext = class
  private
    FList: TList<TAdvSpellcheckRequestGroup>;
  protected
    function GetCount: integer;
    function GetItems(Index: integer): TAdvSpellcheckRequestGroup;
    procedure Add(Value: TAdvSpellcheckRequestGroup);
  public
    property Count: integer read GetCount;
    property Items[Index: integer]: TAdvSpellcheckRequestGroup
      read GetItems; default;
    constructor Create;
    destructor Destroy; override;
  end;

  TAdvWordLocation = (wlStart, wlMiddle, wlEnd);
  TAdvWordCorrections = (wlcStartWithCapitalWords, wlcCaseInsensitive,
    wlcAllCapitalWords);
  TAdvWordCorrection = set of TAdvWordCorrections;

  TAdvWordValidationResult = (wvrValidated, wvrNotValidated,
    wvrAttentionRequired);

  TAdvSpellStoreElement = (sseSpellcheckDB, sseIgnoreList);

  TAdvResultTypes = (rtValidation, rtSuggestions);

  TAdvSpellStoreElements = set of TAdvSpellStoreElement;

  TSpellCheckRequestEvent = procedure(Sender: TObject) of object;
  TSpellCheckRequestCallBack = procedure(Sender: TObject;
    CallBackContext: TAdvSpellCheckCallbackContext) of object;

  TSpellCheckGroupCallBack = procedure(Sender: TObject;
    CallBackContext: TAdvSpellcheckRequestGroup) of object;

  TSpellCheckBeforeWordEvent = procedure(Sender: TObject; var Word: string;
    var Handled: boolean) of object;

  TSpellCheckAfterWordEvent = procedure(Sender: TObject; var Word: string)
    of object;

  TSpellCheckProcessEvent = procedure(Sender: TObject;
    Context: TProcessRequestContext) of object;
  TSpellCheckBeforeLangWordEvent = procedure(Sender: TObject;
    Language: TSpellCheckLanguagePack; var Word: string; var Handled: boolean)
    of object;

  TSpellCheckAfterLangWordEvent = procedure(Sender: TObject;
    Language: TSpellCheckLanguagePack; var Word: string) of object;

  TSpellCheckWordBeforeVerifyEvent = procedure(Sender: TObject;
    Context: TAdvValidationContext; Handled: boolean) of object;

  TSpellCheckWordBeforeSuggestionsEvent = procedure(Sender: TObject;
    var SuggestionsContext: TAdvSuggestionContext; var Handled: boolean)
    of object;

  TSpellCheckWordAfterVerifyEvent = procedure(Sender: TObject;
    Context: TAdvValidationContext) of object;

  TSpellCheckWordAfterSuggestionsEvent = procedure(Sender: TObject;
    var SuggestionsContext: TAdvSuggestionContext) of object;

  TSpellCheckBeforeSaveEvent = procedure(Sender: TObject; var Stream: TStream;
    var Handled: boolean) of object;

  TSpellCheckAfterSaveEvent = procedure(Sender: TObject; var Stream: TStream)
    of object;

  TSpellCheckBeforeSaveEventText = procedure(Sender: TObject; var Words: String;
    var Handled: boolean) of object;

  TSpellCheckAfterSaveEventText = procedure(Sender: TObject; var Words: string)
    of object;

  TSpellCheckAfterEvent = procedure(Sender: TObject) of object;

  TSpellCheckBeforeEvent = procedure(Sender: TObject; var Handled: boolean)
    of object;

  TBeforeRequest = procedure(Sender: TObject;
    var Group: TAdvSpellcheckRequestGroup; var Handled: boolean) of object;

  TAfterRequest = procedure(Sender: TObject; Group: TAdvSpellcheckRequestGroup)
    of object;

  TRequestGroupResult = procedure(Sender: TObject;
    Group: TAdvSpellcheckRequestGroup; var Handled: boolean) of object;

  TRequestContextResult = procedure(Sender: TObject;
    Result: TAdvSpellCheckCallbackContext; var Handled: boolean) of object;

  TAdvValidationContext = class
  private
    FWordLocation: TAdvWordLocation;
    FValidationOptions: TAdvWordCorrection;
    FValidationResult: TAdvWordValidationResult;
    FWord: string;
    FBooleanResult: boolean;
  public
    property ValidationOptions: TAdvWordCorrection read FValidationOptions
      write FValidationOptions;
    property ValidationResult: TAdvWordValidationResult read FValidationResult
      write FValidationResult;
    property WordLocation: TAdvWordLocation read FWordLocation
      write FWordLocation;
    property Word: string read FWord write FWord;
    property BooleanResult: boolean read FBooleanResult write FBooleanResult;
    constructor Create;
    destructor Destroy; override;
  end;

  TAdvSuggestionContext = class
  private
    FSameCaseSuggestions: boolean;
    FWord: string;
    FStringResult: String;
  public
    property StringResult: String read FStringResult write FStringResult;
    property SameCaseSuggestions: boolean read FSameCaseSuggestions
      write FSameCaseSuggestions;
    property Word: string read FWord write FWord;
    constructor Create;
    destructor Destroy; override;
  end;

  TAdvSpellCheckCallbackContext = class
  private
    FRequestType: TAdvResultTypes;
    FOriginalRequest: String;
    FData: TSPObject;
    FBooleanResult: boolean;
    FStringResult: String;
    FWordLocation: TAdvWordLocation;
    FValidationOptions: TAdvWordCorrection;
    FValidationResult: TAdvWordValidationResult;
    FSameCaseSuggestions: boolean;
  public
    property SameCaseSuggestions: boolean read FSameCaseSuggestions
      write FSameCaseSuggestions;
    property ValidationOptions: TAdvWordCorrection read FValidationOptions
      write FValidationOptions;
    property ValidationResult: TAdvWordValidationResult read FValidationResult
      write FValidationResult;
    property WordLocation: TAdvWordLocation read FWordLocation
      write FWordLocation;

    property RequestType: TAdvResultTypes read FRequestType;
    property OriginalRequest: String read FOriginalRequest;
    property Data: TSPObject read FData;
    property BooleanResult: boolean read FBooleanResult;
    property StringResult: String read FStringResult;
    constructor Create;
    destructor Destroy; override;
  end;

  TAdvSpellCheckRequest = class
  private
    FRequestType: TAdvResultTypes;
    FOriginalRequest: String;
    FData: TSPObject;
    FCommandGroup: String;
    FWordLocation: TAdvWordLocation;
    FValidationOptions: TAdvWordCorrection;
    FValidationResult: TAdvWordValidationResult;
    FGroup: TAdvSpellcheckRequestGroup;
    FCallback: TSpellCheckRequestCallBack;
    FStringResult: string;
    FBooleanResult: boolean;
    FExecuted: boolean;
    FSameCaseSuggestions: boolean;
    procedure SetExcuted(Value: boolean);
    function GetExecuted: boolean;
  public
    property SameCaseSuggestions: boolean read FSameCaseSuggestions
      write FSameCaseSuggestions;
    property ValidationOptions: TAdvWordCorrection read FValidationOptions
      write FValidationOptions;
    property ValidationResult: TAdvWordValidationResult read FValidationResult
      write FValidationResult;
    property WordLocation: TAdvWordLocation read FWordLocation
      write FWordLocation;

    property Executed: boolean read GetExecuted write SetExcuted;
    property StringResult: string read FStringResult write FStringResult;
    property BooleanResult: boolean read FBooleanResult write FBooleanResult;
    property RequestType: TAdvResultTypes read FRequestType write FRequestType;
    property OriginalRequest: String read FOriginalRequest
      write FOriginalRequest;
    property Data: TSPObject read FData write FData;
    property CommandGroup: string read FCommandGroup write FCommandGroup;
    property Group: TAdvSpellcheckRequestGroup read FGroup write FGroup;
    property Callback: TSpellCheckRequestCallBack read FCallback
      write FCallback;
    constructor Create;
    destructor Destroy; override;
  end;

  TAdvSpellcheckRequestGroup = class
  private
    FList: TList<TAdvSpellCheckRequest>;
    FName: string;
    FLock: TCriticalSection;
    FValidationsCancelled: boolean;
    FSuggestionsCancelled: boolean;
    FCallback: TSpellCheckGroupCallBack;
  protected
    function GetCount: integer;
    function GetItem(Index: integer): TAdvSpellCheckRequest;
  public
    property Callback: TSpellCheckGroupCallBack read FCallback write FCallback;
    property Items[Index: integer]: TAdvSpellCheckRequest read GetItem; default;
    property Name: string read FName write FName;
    property Count: integer read GetCount;
    function SuggestionsCancelled: boolean;
    function ValidationsCancelled: boolean;
    procedure Add(Value: TAdvSpellCheckRequest);
    procedure Reset;
    procedure CancelSuggestions;
    procedure CancelValidations;
    procedure CancelAll;
    procedure Lock;
    procedure Release;
    procedure CleanupGroup(GroupName: string);
    procedure CleanupExecuted;
    procedure Cleanup;
    constructor Create;
    destructor Destroy; override;
  end;

  TSpellCheckWorkerThread = class(TThread)
  private
    FOwner: TCustomAdvSpellCheck;
    FStarted: boolean;
  protected
    procedure Execute; override;
  public
    property Started: boolean read FStarted write FStarted;
    constructor Create(Owner: TCustomAdvSpellCheck);
    destructor Destroy; override;
  end;

  TSpellCheckLanguagePack = class(TCollectionItem)
  private
    FAffixData: TStringList;
    FEnabled: boolean;
    FSpellInfo: TTMSSpellinfo;
    FLanguageCode: TTMSLangauages;
    FSourceFileName: TFileName;
    FDescription: String;
    FGuid: string;
    FWords: TStrings;
    FSoundexName: string;
    FAffixFileName: TFileName;
    FSoundexProcess: TTMSSoundexProcess;
    FOnActiveLanguageValidation: TSpellCheckBeforeLangWordEvent;
    FOnValidation: TSpellCheckBeforeLangWordEvent;
  protected
    procedure SetEnabled(Value: boolean);
    procedure SetLanguageCode(Value: TTMSLangauages);
    procedure SetSourceFileName(Value: TFileName);
    procedure SetWords(Value: TStrings);
    procedure SetSoundex(Name: string);
    procedure SetSoundexProcess(Value: TTMSSoundexProcess);
    procedure SetAffixFilename(Value: TFileName);
    procedure SetAffixData(Value: TStringList);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property AffixData: TStringList read FAffixData write SetAffixData;
    property SpellInfo: TTMSSpellinfo read FSpellInfo;
    property OnActiveLanguageValidation: TSpellCheckBeforeLangWordEvent
      read FOnActiveLanguageValidation write FOnActiveLanguageValidation;
    property OnValidation: TSpellCheckBeforeLangWordEvent read FOnValidation
      write FOnValidation;

    property AffixFileName: TFileName read FAffixFileName
      write SetAffixFilename;
    property SoundexProcess: TTMSSoundexProcess read FSoundexProcess
      write SetSoundexProcess;
    property SoundexName: string read FSoundexName write FSoundexName;
    property Words: TStrings read FWords write SetWords;
    property Guid: string read FGuid write FGuid;
    property Enabled: boolean read FEnabled write SetEnabled;
    property LanguageCode: TTMSLangauages read FLanguageCode
      write SetLanguageCode;
    property SourceFileName: TFileName read FSourceFileName
      write SetSourceFileName;
    property Description: String read FDescription write FDescription;
    procedure RenewGuid();
  end;

  TSpellCheckHighlight = class(TObject)
  private
    FSelStart,FSelLength: integer;
  public
    property SelStart: integer read FSelStart write FSelStart;
    property SelLength: integer read FSelLength write FSelLength;
  end;

  TSpellCheckLanguagePackCollection = class(TCollection)
  private
    FOwner: TCustomAdvSpellCheck;
  protected
    function GetOwner: TPersistent; override;
    function GetItems(Index: integer): TSpellCheckLanguagePack;
  public
    property Items[Index: integer]: TSpellCheckLanguagePack
      read GetItems; default;
    function GetIndexByName(Name: String): integer;
    function GetIndexByLangauge(Name: String): integer;
    function Add: TSpellCheckLanguagePack; reintroduce;
    function Insert(Index: integer): TSpellCheckLanguagePack; reintroduce;
    constructor Create(Owner: TCustomAdvSpellCheck);
    destructor Destroy; override;
  end;

  TCustomAdvSpellCheck = class(TComponent)
  private
    FMultiLanguageValidation: boolean;
    InternalWords: TStringList;
    FUpdateCount: integer;
    FAsyncEventsList: TAdvSpellEventsList;
    FRequestGroupResult: TRequestGroupResult;
    FSyncEvents: TAdvSpellcheckSynchronizedEvent;
    FRequestResult: TRequestContextResult;

    FBeforeBeginRequest, FBeforeEndRequest: TBeforeRequest;
    FAfterBeginRequest, FAfterEndRequest: TAfterRequest;
    FBeforeAddToIgnoreWord: TSpellCheckBeforeWordEvent;
    FAfterAddToIgnoreWord: TSpellCheckAfterWordEvent;

    FBeforeRemoveFromIgnoreWord: TSpellCheckBeforeWordEvent;
    FAfterRemoveFromignoreWord: TSpellCheckAfterWordEvent;
    FBeforeSaveConfig: TSpellCheckBeforeSaveEvent;
    FAfterSaveConfig: TSpellCheckAfterSaveEvent;
    FBeforeLoadConfig: TSpellCheckBeforeSaveEvent;
    FAfterLoadConfig: TSpellCheckAfterSaveEvent;
    FBeforeSaveIgnoreList: TSpellCheckBeforeSaveEvent;
    FAfterSaveIgnoreList: TSpellCheckAfterSaveEvent;
    FBeforeLoadIgnoreListText: TSpellCheckBeforeSaveEventText;
    FAfterLoadIgnoreListText: TSpellCheckAfterSaveEventText;
    FBeforeLoadIgnoreList: TSpellCheckBeforeSaveEvent;
    FAfterLoadIgnoreList: TSpellCheckAfterSaveEvent;
    FBeforeSaveDB: TSpellCheckBeforeSaveEvent;
    FAfterSaveDB: TSpellCheckAfterSaveEvent;
    FBeforeLoadDB: TSpellCheckBeforeSaveEvent;
    FAfterLoadDB: TSpellCheckAfterSaveEvent;
    FBeforeAppendWordsToDictionary: TSpellCheckBeforeLangWordEvent;
    FAfterAppendWordsToDictionary: TSpellCheckAfterLangWordEvent;
    FBeforeRefreshDictionary: TSpellCheckBeforeEvent;
    FAfterRefreshDictionary: TSpellCheckAfterEvent;
    FBeforeOpen: TSpellCheckBeforeEvent;
    FAfterOpen: TSpellCheckAfterEvent;
    FBeforeClose: TSpellCheckBeforeEvent;
    FAfterClose: TSpellCheckAfterEvent;
    FBeforeSave: TSpellCheckBeforeSaveEvent;
    FAfterLoad: TSpellCheckAfterSaveEvent;
    FBeforeLoad: TSpellCheckBeforeSaveEvent;
    FAfterSave: TSpellCheckAfterSaveEvent;
    FBeforeCleanupDictionary: TSpellCheckBeforeEvent;
    FAfterCleanupDictionary: TSpellCheckAfterEvent;
    FBeforeGetSuggestions: TSpellCheckWordBeforeSuggestionsEvent;
    FAfterGetSuggestions: TSpellCheckWordAfterSuggestionsEvent;
    FAfterGetValidation: TSpellCheckWordAfterVerifyEvent;
    FBeforeGetValidation: TSpellCheckWordBeforeVerifyEvent;
    FOnRequestsProcessed: TSpellCheckProcessEvent;
    FCurrentLangaugeSuggestions: boolean;
    FActiveDictionaryIndex: integer;
    FActiveDictionaryName: string;
    FSoundex: TTMSoundexManager;
    FUpdateRequired, FAutoupdate: boolean;
    FOriginalSettings: string;
    FTimer: TTimer;
    FCurrentGroup: TAdvSpellcheckRequestGroup;
    FRequestGroupsList: TList<TAdvSpellcheckRequestGroup>;
    FCallbackList: TList<TAdvSpellCheckRequest>;
    FRequestsList: TList<TAdvSpellCheckRequest>;
    FCallbackLock, FDatasetLock, FInputLock: TCriticalSection;
    FLanguages: TSpellCheckLanguagePackCollection;
    FDataset: TClientDataset;
    FStoreElements: TAdvSpellStoreElements;
    FIgnoreList: TStringList;
    FInMemoryDatabase: string;
    FDatabaseFileName: String;
    FActive: boolean;
    FTempActive: boolean;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    FProgressform: TForm;
    FWorkerThread: TSpellCheckWorkerThread;
    procedure InsertEmptyData;
    procedure DoAfterLoadIgnoreList(Words: String); overload;
    function DoBeforeLoadIgnoreList(Words: string): boolean; overload;
    procedure DoAfterLoadIgnoreList(Stream: TStream); overload;
    function DoBeforeLoadIgnoreList(Stream: TStream): boolean; overload;
    function DoBeforeBeginRequest(var Group
      : TAdvSpellcheckRequestGroup): boolean;
    procedure DoAfterbeginRequest(Group: TAdvSpellcheckRequestGroup);
    function DobeforeEndRequest(var Group: TAdvSpellcheckRequestGroup): boolean;
    procedure DoAfterEndRequest(Group: TAdvSpellcheckRequestGroup);
    function DoGroupCallbackRequestResults
      (Group: TAdvSpellcheckRequestGroup): boolean;
    function DoCallbackRequestResults(CallBackContext
      : TAdvSpellCheckCallbackContext): boolean;
    function DoBeforeAddIgnoreWord(var Word: string): boolean;
    procedure DoAfterAddIgnoreWord(Word: string);
    function DoBeforeRemoveIgnoreWord(var Word: string): boolean;
    procedure DoAfterRemoveignoreWord(Word: string);
    function DoBeforeSaveConfig(Stream: TStream): boolean;
    procedure DoAfterSaveConfig(Stream: TStream);
    function DoBeforeLoadConfig(Stream: TStream): boolean;
    procedure DoAfterLoadConfig(Stream: TStream);
    function DoBeforeSaveIgnoreList(Stream: TStream): boolean;
    procedure DoAfterSaveIgnoreList(Stream: TStream);
    function DoBeforeSaveDB(Stream: TStream): boolean;
    procedure DoAfterSaveDB(Stream: TStream);
    function DoBeforeLoadDB(Stream: TStream): boolean;
    procedure DoAfterLoadDB(Stream: TStream);
    function DoBeforeAppendWordsToDictionary(Language: TSpellCheckLanguagePack;
      var Words: string): boolean;
    procedure DoAfterAppendWordsToDictionary(Language: TSpellCheckLanguagePack;
      var Words: string);
    function DoBeforeRefreshDictionary(): boolean;
    procedure DoAfterRefreshDictionary();
    function DoBeforeOpen(): boolean;
    procedure DoAfterOpen;
    function DoBeforeClose: boolean;
    procedure DoAfterClose;
    function DoBeforeSave(Stream: TStream): boolean;
    procedure DoAfterSave(Stream: TStream);
    function DoBeforeLoad(Stream: TStream): boolean;
    procedure DoAfterLoad(Stream: TStream);
    function DoBeforeCleanupDictionary(): boolean;
    procedure DoAfterCleanupDictionary;
    function DoBeforeGetSuggestions(Context: TAdvSuggestionContext): boolean;
    procedure DoAfterGetSuggestions(Context: TAdvSuggestionContext);
    procedure DoAfterGetValidation(Words: string;
      Context: TAdvValidationContext);
    function DoBeforeGetValidation(var Words: string;
      Context: TAdvValidationContext): boolean;

    procedure Loaded; override;
    procedure OnTimer(Sender: TObject); virtual;
    function GetRequestGroup(Index: integer)
      : TAdvSpellcheckRequestGroup; virtual;
    function GetRequestGroupByName(Name: String)
      : TAdvSpellcheckRequestGroup; virtual;
    function GetRequestGroupIndex(Name: String): integer; virtual;
    procedure RemoveRequestGroup(Name: String); overload; virtual;
    procedure RemoveRequestGroup(Index: integer); overload; virtual;
    procedure CleanupRequestGroups();
    procedure DoRequestsProcessed(Context: TProcessRequestContext); virtual;
    procedure ExecEvents; virtual;

    procedure CreateTable; virtual;
    procedure SetActive(Value: boolean); virtual;
    procedure SetDatabasefileName(Value: String); virtual;
    function GetDatabasefileName: String; virtual;
    function CheckForActive: boolean; virtual;
    procedure SetIndexToWord; virtual;
    procedure SetIndexToSoundex(SingleLangauge: boolean); virtual;
    procedure RemoveFilter; virtual;
    procedure CreateDatasetIfNotExists; virtual;
    function GetInmemoryDatabaseName: string; virtual;
    procedure SetInMemoryDatabaseName(Value: String); virtual;
    procedure RequiredActiveState; virtual;
    procedure RequiredNonActiveState; virtual;
    procedure ExecCommand; virtual;
    procedure LockDataSet; virtual;
    procedure ReleaseDataset; virtual;
    procedure LockCallback; virtual;
    procedure ReleaseCallback; virtual;
    procedure LockInput; virtual;
    procedure ReleaseInput; virtual;
    function InternalGetValidate(Word: string; WordLocation: TAdvWordLocation;
      ValidationOptions: TAdvWordCorrection): TAdvWordValidationResult;
    function InternalGetSuggestions(Word: String;
      CurrentLanguageSuggestion: boolean; SameCapsSuggestion: boolean;
      MaxSuggestions: integer): string;
    function GetActiveDictionaryName: string;
    procedure SetActiveDictionaryName(Name: string);
    function CurrentSoundexProcess: TTMSSoundexProcess;
    procedure SetActiveDictionary(Value: integer);
    property RequestGroups[Index: integer]: TAdvSpellcheckRequestGroup
      read GetRequestGroup;
    property OnlyActiveLangaugeSuggestions: boolean
      read FCurrentLangaugeSuggestions write FCurrentLangaugeSuggestions;
    procedure Flash();
  public
    procedure BeginUpdate;
    function Updating: boolean; reintroduce;
    procedure EndUpdate;

    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    function GetVersionNr: integer; virtual;

    function CompareXMLConfig(S1, S2: string): boolean;

    function ReturnXMLConfig: String;
    function LoadXMLConfig: String;

    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(Filename: string);
    procedure SaveToFile(Filename: String);

    procedure SaveConfig(Filename: string); overload; virtual;
    procedure SaveConfig(Stream: TStream); overload; virtual;

    procedure LoadConfigFromString(Config: string);
    procedure LoadConfig(Filename: string); overload; virtual;
    procedure LoadConfig(Stream: TStream); overload; virtual;

    procedure SaveDB(Filename: string); overload; virtual;
    procedure SaveDB(Stream: TStream); overload; virtual;
    procedure LoadDB(Filename: string); overload; virtual;
    procedure LoadDB(Stream: TStream); overload; virtual;

    procedure SaveIgnoreList(Filename: String); overload; virtual;
    procedure SaveIgnoreList(Stream: TStream); overload; virtual;
    procedure LoadIgnoreList(Filename: String); overload; virtual;
    procedure LoadIgnoreList(Stream: TStream); overload; virtual;

    procedure CancelSuggestions(AnId: string);
    procedure CancelValidations(AnId: string);
    procedure CancelAllRequests(AnId: string);

    procedure BeginRequest(AnId: string;
      Callback: TSpellCheckGroupCallBack = nil);
    procedure AddSuggestionRequest(Word: string; Data: TSPObject;
      Callback: TSpellCheckRequestCallBack = nil;
      SameCapsSuggestion: boolean = true);
    procedure AddValidationRequest(Word: string; Data: TSPObject;
      Callback: TSpellCheckRequestCallBack;
      WordLocation: TAdvWordLocation = TAdvWordLocation.wlMiddle;
      ValidationOptions: TAdvWordCorrection =
      [TAdvWordCorrections.wlcStartWithCapitalWords,
      TAdvWordCorrections.wlcCaseInsensitive]);
    procedure EndRequest;

    procedure AddToDictionary(Language: TSpellCheckLanguagePack; Words: string);
    procedure RemoveFromDictionary(Language: TSpellCheckLanguagePack;
      Words: string);

    procedure AppendWordsList(Language: TSpellCheckLanguagePack; Words: string);
      overload; virtual;
    procedure AppendWordsList(Words: string); overload; virtual;
    procedure LoadWordsList(Language: TSpellCheckLanguagePack; Stream: TStream);
      overload; virtual;
    procedure AppendWordsList(Language: TSpellCheckLanguagePack;
      Stream: TStream); overload; virtual;
    procedure LoadWordsList(Language: TSpellCheckLanguagePack;
      Filename: string); overload; virtual;
    procedure AppendWordsListFromFile(Language: TSpellCheckLanguagePack;
      Filename: String); virtual;
    procedure AppendWords(Language: TSpellCheckLanguagePack; Words: String);
      overload; virtual;
    procedure OpenSettingsDialog; overload; virtual;

    // procedure SaveData; virtual;
    procedure RefreshDatabase; virtual;
    procedure Open; virtual;
    procedure Close; virtual;
    function Validate(Word: string;
      WordLocation: TAdvWordLocation = TAdvWordLocation.wlMiddle;
      ValidationOptions: TAdvWordCorrection =
      [TAdvWordCorrections.wlcCaseInsensitive,
      TAdvWordCorrections.wlcAllCapitalWords])
      : TAdvWordValidationResult; virtual;
    function Suggestions(Word: string; SameCapsSuggestion: boolean = true)
      : String; virtual;
    function FirstSuggestion(Word: string; SameCapsSuggestion: boolean = true)
      : string; virtual;

    function IndexInIgnoreList(Word: string): integer; virtual;
    function ExistsInIgnoreList(Word: string): boolean; virtual;
    procedure AddToIgnoreList(Word: string); virtual;
    procedure RemoveFromIgnoreList(Word: string); overload; virtual;
    procedure LoadIgnoreListWords(Words: String); overload; virtual;
    procedure CleanupIgnoreList();

    property AsyncEventsList: TAdvSpellEventsList read FAsyncEventsList
      write FAsyncEventsList;

    property OnRequestsProcessed: TSpellCheckProcessEvent
      read FOnRequestsProcessed write FOnRequestsProcessed;

    property OnRequestGroupResult: TRequestGroupResult read FRequestGroupResult
      write FRequestGroupResult;
    property OnRequestResult: TRequestContextResult read FRequestResult
      write FRequestResult;
    property OnBeforeBeginRequest: TBeforeRequest read FBeforeBeginRequest
      write FBeforeBeginRequest;
    property OnBeforeEndRequest: TBeforeRequest read FBeforeEndRequest
      write FBeforeEndRequest;
    property OnAfterBeginRequest: TAfterRequest read FAfterBeginRequest
      write FAfterBeginRequest;
    property OnAfterEndRequest: TAfterRequest read FAfterEndRequest
      write FAfterEndRequest;

    property MultiLanguageValidation: boolean read FMultiLanguageValidation
      write FMultiLanguageValidation;
    property OnBeforeLoadIgnoreListText: TSpellCheckBeforeSaveEventText
      read FBeforeLoadIgnoreListText write FBeforeLoadIgnoreListText;
    property OnAfterLoadIgnoreListText: TSpellCheckAfterSaveEventText
      read FAfterLoadIgnoreListText write FAfterLoadIgnoreListText;
    property OnBeforeAddToIgnoreWord: TSpellCheckBeforeWordEvent
      read FBeforeAddToIgnoreWord write FBeforeAddToIgnoreWord;
    property OnAfterAddToIgnoreWord: TSpellCheckAfterWordEvent
      read FAfterAddToIgnoreWord write FAfterAddToIgnoreWord;
    property OnBeforeRemoveFromIgnoreWord: TSpellCheckBeforeWordEvent
      read FBeforeRemoveFromIgnoreWord write FBeforeRemoveFromIgnoreWord;
    property OnAfterRemoveFromignoreWord: TSpellCheckAfterWordEvent
      read FAfterRemoveFromignoreWord write FAfterRemoveFromignoreWord;
    property OnBeforeSaveConfig: TSpellCheckBeforeSaveEvent
      read FBeforeSaveConfig write FBeforeSaveConfig;
    property OnAfterSaveConfig: TSpellCheckAfterSaveEvent read FAfterSaveConfig
      write FAfterSaveConfig;
    property OnBeforeLoadConfig: TSpellCheckBeforeSaveEvent
      read FBeforeLoadConfig write FBeforeLoadConfig;
    property OnAfterLoadConfig: TSpellCheckAfterSaveEvent read FAfterLoadConfig
      write FAfterLoadConfig;
    property OnBeforeSaveIgnoreList: TSpellCheckBeforeSaveEvent
      read FBeforeSaveIgnoreList write FBeforeSaveIgnoreList;
    property OnAfterSaveIgnoreList: TSpellCheckAfterSaveEvent
      read FAfterSaveIgnoreList write FAfterSaveIgnoreList;
    property OnBeforeLoadIgnoreList: TSpellCheckBeforeSaveEvent
      read FBeforeLoadIgnoreList write FBeforeLoadIgnoreList;
    property OnAfterLoadIgnoreList: TSpellCheckAfterSaveEvent
      read FAfterLoadIgnoreList write FAfterLoadIgnoreList;
    property OnBeforeSaveDB: TSpellCheckBeforeSaveEvent read FBeforeSaveDB
      write FBeforeSaveDB;
    property OnAfterSaveDB: TSpellCheckAfterSaveEvent read FAfterSaveDB
      write FAfterSaveDB;
    property OnBeforeLoadDB: TSpellCheckBeforeSaveEvent read FBeforeLoadDB
      write FBeforeLoadDB;
    property OnAfterLoadDB: TSpellCheckAfterSaveEvent read FAfterLoadDB
      write FAfterLoadDB;
    property OnBeforeAppendWordsToDictionary: TSpellCheckBeforeLangWordEvent
      read FBeforeAppendWordsToDictionary write FBeforeAppendWordsToDictionary;
    property OnAfterAppendWordsToDictionary: TSpellCheckAfterLangWordEvent
      read FAfterAppendWordsToDictionary write FAfterAppendWordsToDictionary;
    property OnBeforeRefreshDictionary: TSpellCheckBeforeEvent
      read FBeforeRefreshDictionary write FBeforeRefreshDictionary;
    property OnAfterRefreshDictionary: TSpellCheckAfterEvent
      read FAfterRefreshDictionary write FAfterRefreshDictionary;
    property OnBeforeOpen: TSpellCheckBeforeEvent read FBeforeOpen
      write FBeforeOpen;
    property OnAfterOpen: TSpellCheckAfterEvent read FAfterOpen
      write FAfterOpen;
    property OnBeforeClose: TSpellCheckBeforeEvent read FBeforeClose
      write FBeforeClose;
    property OnAfterClose: TSpellCheckAfterEvent read FAfterClose
      write FAfterClose;
    property OnBeforeSave: TSpellCheckBeforeSaveEvent read FBeforeSave
      write FBeforeSave;
    property OnAfterLoad: TSpellCheckAfterSaveEvent read FAfterLoad
      write FAfterLoad;
    property OnBeforeLoad: TSpellCheckBeforeSaveEvent read FBeforeLoad
      write FBeforeLoad;
    property OnAfterSave: TSpellCheckAfterSaveEvent read FAfterSave
      write FAfterSave;
    property OnBeforeCleanupDictionary: TSpellCheckBeforeEvent
      read FBeforeCleanupDictionary write FBeforeCleanupDictionary;
    property OnAfterCleanupDictionary: TSpellCheckAfterEvent
      read FAfterCleanupDictionary write FAfterCleanupDictionary;
    property OnBeforeGetSuggestions: TSpellCheckWordBeforeSuggestionsEvent
      read FBeforeGetSuggestions write FBeforeGetSuggestions;
    property OnAfterGetSuggestions: TSpellCheckWordAfterSuggestionsEvent
      read FAfterGetSuggestions write FAfterGetSuggestions;
    property OnAfterGetValidation: TSpellCheckWordAfterVerifyEvent
      read FAfterGetValidation write FAfterGetValidation;
    property OnBeforeGetValidation: TSpellCheckWordBeforeVerifyEvent
      read FBeforeGetValidation write FBeforeGetValidation;

    function ActiveLanguage: TSpellCheckLanguagePack;
    property ActiveLanguageIndex: integer read FActiveDictionaryIndex
      write SetActiveDictionary;
    property CurrentLangaugeSuggestions: boolean
      read FCurrentLangaugeSuggestions write FCurrentLangaugeSuggestions;
    property ActiveLanguageName: string read GetActiveDictionaryName
      write SetActiveDictionaryName;
    property SoundexManager: TTMSoundexManager read FSoundex write FSoundex;
    property AutoUpdate: boolean read FAutoupdate write FAutoupdate;
    property Languages: TSpellCheckLanguagePackCollection read FLanguages
      write FLanguages;
    property StoreElements: TAdvSpellStoreElements read FStoreElements
      write FStoreElements;

    property InMemoryDatabaseName: String read GetInmemoryDatabaseName
      write SetInMemoryDatabaseName;
    property DatabaseFilename: string read GetDatabasefileName
      write SetDatabasefileName;
    property Active: boolean read FActive write SetActive;
    property IgnoreList: TStringList read FIgnoreList write FIgnoreList;

    property Version: string read GetVersion write SetVersion;
  end;

  {$IFDEF FMXLIB}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF}
  TAdvSpellCheck = class(TCustomAdvSpellCheck)
  published
    property OnRequestsProcessed;

    property AutoUpdate;
    property Languages;
    property InMemoryDatabaseName;
    property DatabaseFilename;
    property StoreElements;
    property Active;
    property IgnoreList;
    property Version;

    property OnBeforeBeginRequest;
    property OnBeforeEndRequest;
    property OnAfterBeginRequest;
    property OnAfterEndRequest;

    property OnRequestGroupResult;
    property OnRequestResult;

    property MultiLanguageValidation;
    property OnBeforeLoadIgnoreListText;
    property OnAfterLoadIgnoreListText;
    property OnBeforeAddToIgnoreWord;
    property OnAfterAddToIgnoreWord;
    property OnBeforeRemoveFromIgnoreWord;
    property OnAfterRemoveFromignoreWord;
    property OnBeforeSaveConfig;
    property OnAfterSaveConfig;
    property OnBeforeLoadConfig;
    property OnAfterLoadConfig;
    property OnBeforeSaveIgnoreList;
    property OnAfterSaveIgnoreList;
    property OnBeforeLoadIgnoreList;
    property OnAfterLoadIgnoreList;
    property OnBeforeSaveDB;
    property OnAfterSaveDB;
    property OnBeforeLoadDB;
    property OnAfterLoadDB;
    property OnBeforeAppendWordsToDictionary;
    property OnAfterAppendWordsToDictionary;
    property OnBeforeRefreshDictionary;
    property OnAfterRefreshDictionary;
    property OnBeforeOpen;
    property OnAfterOpen;
    property OnBeforeClose;
    property OnAfterClose;
    property OnBeforeSave;
    property OnAfterLoad;
    property OnBeforeLoad;
    property OnAfterSave;
    property OnBeforeCleanupDictionary;
    property OnAfterCleanupDictionary;
    property OnBeforeGetSuggestions;
    property OnAfterGetSuggestions;
    property OnAfterGetValidation;
    property OnBeforeGetValidation;
  end;

resourcestring
   TMSSPELLERRMSG = 'No spell check engine assigned';

implementation

uses
{$IFNDEF FMXLIB}
  TMSSpellUpdateDictProgForm,
{$ENDIF}
  TMSSpellCheckConfForm;

function Hiword(L: DWORD): integer;
begin
  Result := L shr 16;
end;

function LoWord(L: DWORD): integer;
begin
  Result := L AND $FFFF;
end;

function MakeWord(b1, b2: integer): integer;
begin
  Result := b1 or b2 shl 8;
end;

function MakeLong(i1, i2: integer): integer;
begin
  Result := i1 or i2 shl 16;
end;

// ------------------------------------------------------------------------------

{ TAdvRequest }
procedure TAdvSpellCheckRequest.SetExcuted(Value: boolean);
begin
  Group.Lock;
  try
    FExecuted := Value;
  finally
    Group.Release;
  end;
end;

function TAdvSpellCheckRequest.GetExecuted: boolean;
begin
  Group.Lock;
  try
    Exit(FExecuted);
  finally
    Group.Release;
  end;
end;

constructor TAdvSpellCheckRequest.Create;
begin
  Inherited;
end;

destructor TAdvSpellCheckRequest.Destroy;
begin
  Inherited;
end;

// ------------------------------------------------------------------------------

{ TAdvRequestGroup }

procedure TAdvSpellcheckRequestGroup.Add(Value: TAdvSpellCheckRequest);
begin
  FList.Add(Value);
end;

procedure TAdvSpellcheckRequestGroup.Reset;
begin
  Lock;
  try
    FSuggestionsCancelled := false;
    FValidationsCancelled := false;
  finally
    Release;
  end;
end;

procedure TAdvSpellcheckRequestGroup.CancelSuggestions;
var
  I: integer;
begin
  Lock;
  try
    for I := 0 to FList.Count - 1 do
      if (Self[I].RequestType = rtSuggestions) then
        Items[I].Executed := true;
  finally
    Release;
  end;
end;

function TAdvSpellcheckRequestGroup.SuggestionsCancelled: boolean;
begin
  Lock;
  try
    Exit(FSuggestionsCancelled);
  finally
    Release;
  end;
end;

function TAdvSpellcheckRequestGroup.ValidationsCancelled: boolean;
begin
  Lock;
  try
    Exit(FValidationsCancelled);
  finally
    Release;
  end;
end;

procedure TAdvSpellcheckRequestGroup.CancelValidations;
var
  I: integer;
begin
  Lock;
  try
    for I := 0 to FList.Count - 1 do
      if (Self[I].RequestType = rtValidation) then
        Items[I].Executed := true;
  finally
    Release;
  end;
end;

procedure TAdvSpellcheckRequestGroup.CancelAll;
var
  I: integer;
begin
  Lock;
  try
    for I := 0 to FList.Count - 1 do
      Items[I].Executed := true;
  finally
    Release;
  end;
end;

procedure TAdvSpellcheckRequestGroup.Lock;
begin
  FLock.Enter;
end;

function TAdvSpellcheckRequestGroup.GetCount: integer;
begin
  Exit(FList.Count);
end;

function TAdvSpellcheckRequestGroup.GetItem(Index: integer)
  : TAdvSpellCheckRequest;
begin
  Exit(TAdvSpellCheckRequest(FList[index]));
end;

procedure TAdvSpellcheckRequestGroup.Release;
begin
  FLock.Leave;
end;

procedure TAdvSpellcheckRequestGroup.CleanupExecuted;
var
  I: integer;
  tl: TList<TAdvSpellCheckRequest>;
  r: TAdvSpellCheckRequest;
begin
  Lock;
  tl := TList<TAdvSpellCheckRequest>.Create;
  try
    for I := 0 to Count - 1 do
    begin
      r := Items[I];

      if r.Executed then
      begin
        r.Free;
        Continue;
      end;
      tl.Add(r)
    end;

    FList.Clear;
    for I := 0 to tl.Count - 1 do
      FList.Add(tl[I]);
  finally
    Release;
    tl.Free;
  end;
end;

procedure TAdvSpellcheckRequestGroup.CleanupGroup(GroupName: string);
var
  I: integer;
  tl: TList<TAdvSpellCheckRequest>;
  r: TAdvSpellCheckRequest;
begin
  Lock;
  tl := TList<TAdvSpellCheckRequest>.Create;
  try
    for I := 0 to Count - 1 do
    begin
      r := Items[I];
      if r.CommandGroup = GroupName then
      begin
        r.Free;
        Continue;
      end;
      if r.CommandGroup <> GroupName then
        tl.Add(r)
    end;
    FList.Clear;
    for I := 0 to tl.Count - 1 do
      FList.Add(tl[I]);
  finally
    Release;
    tl.Free;
  end;
end;

procedure TAdvSpellcheckRequestGroup.Cleanup;
var
  I: integer;
  r: TAdvSpellCheckRequest;
begin
  Lock;
  try
    for I := 0 to FList.Count - 1 do
    begin
      r := Items[I];
      r.Free;
    end;
    FList.Clear;
  finally
    Release;
  end;
end;

constructor TAdvSpellcheckRequestGroup.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FList := TList<TAdvSpellCheckRequest>.Create;
end;

destructor TAdvSpellcheckRequestGroup.Destroy;
begin
  try
    Lock;
    Cleanup;
  finally
    Release;
    FreeAndNil(FList);
    FreeAndNil(FLock);
    Inherited;
  end;
end;

// ------------------------------------------------------------------------------

{ TAdvSuggestionContext }
constructor TAdvSuggestionContext.Create;
begin
  Inherited;
end;

destructor TAdvSuggestionContext.Destroy;
begin
  Inherited;
end;

// ------------------------------------------------------------------------------

{ TAdvValidationContext }
constructor TAdvValidationContext.Create;
begin
  Inherited;
end;

destructor TAdvValidationContext.Destroy;
begin
  Inherited;
end;

// ------------------------------------------------------------------------------

{ TAdvCallbackContext }
constructor TAdvSpellCheckCallbackContext.Create;
begin
  inherited;
end;

destructor TAdvSpellCheckCallbackContext.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------

{ TSpellCheckWorkerThread }

procedure TSpellCheckWorkerThread.Execute;
begin
  Started := true;
  while not Terminated and not Application.Terminated do
  begin
    FOwner.ExecCommand;
    Sleep(10);
  end;
  Started := false;
end;

destructor TSpellCheckWorkerThread.Destroy;
begin
  inherited;
end;

constructor TSpellCheckWorkerThread.Create(Owner: TCustomAdvSpellCheck);
begin
  Inherited Create(true);
  FOwner := Owner;
  {$IFNDEF FMXLIB}
  Priority := tpLowest;
  {$ENDIF}
end;

// ------------------------------------------------------------------------------

{ TSpellCheckLanguagePackCollection }
function TSpellCheckLanguagePackCollection.GetItems(Index: integer)
  : TSpellCheckLanguagePack;
begin
  Exit(TSpellCheckLanguagePack(Inherited Items[Index]));
end;

function TSpellCheckLanguagePackCollection.GetOwner: TPersistent;
begin
  Exit(FOwner);
end;

function TSpellCheckLanguagePackCollection.GetIndexByName(Name: String)
  : integer;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    if LowerCase(name) = LowerCase(Self[I].Description) then
      Exit(I);

  Exit(-1);
end;

function TSpellCheckLanguagePackCollection.GetIndexByLangauge
  (Name: String): integer;
var
  I: integer;
  L: TTMSLangauages;
begin
  L := StringToLanguageEnum(Name);
  for I := 0 to Count - 1 do
    if Self[I].LanguageCode = L then
      Exit(I);
  Exit(-1);
end;

function TSpellCheckLanguagePackCollection.Add: TSpellCheckLanguagePack;
begin
  Exit(TSpellCheckLanguagePack(Inherited Add));
end;

function TSpellCheckLanguagePackCollection.Insert(Index: integer)
  : TSpellCheckLanguagePack;
begin
  Exit(TSpellCheckLanguagePack(Inherited Insert(Index)));
end;

constructor TSpellCheckLanguagePackCollection.Create
  (Owner: TCustomAdvSpellCheck);
begin
  Inherited Create(TSpellCheckLanguagePack);
  FOwner := Owner;
end;

destructor TSpellCheckLanguagePackCollection.Destroy;
begin
  Inherited;
end;

// ------------------------------------------------------------------------------

{ TSpellCheckLanguagePack }

procedure TSpellCheckLanguagePack.SetSourceFileName(Value: TFileName);
begin
  if FSourceFileName <> Value then
  begin
    RenewGuid();
    FSourceFileName := Value;
  end;
end;

procedure TSpellCheckLanguagePack.SetAffixData(Value: TStringList);
var
  str: TStringStream;
begin
  if Value.Text <> FAffixData.Text then
  begin
    RenewGuid;
    FAffixData.Text := Value.Text;
    if not FileExists(FixFileName(AffixFileName)) then
    begin
      str := TStringStream.Create(FAffixData.Text, TEncoding.Unicode);
      try
        SpellInfo.LoadFromStream(str);
      finally
        str.Free;
      end;
    end;
  end;
end;

procedure TSpellCheckLanguagePack.SetAffixFilename(Value: TFileName);
begin
  if FAffixFileName <> Value then
  begin
    if (Value <> '') and FileExists(FixFileName(Value)) then
    begin;
      SpellInfo.LoadFromFile(Value);
    end;
    RenewGuid;
    FAffixFileName := Value;
  end;
end;

procedure TSpellCheckLanguagePack.SetSoundexProcess(Value: TTMSSoundexProcess);
begin
  if FSoundexProcess <> Value then
  begin
    RenewGuid;
    FSoundexProcess := Value;
  end;
end;

procedure TSpellCheckLanguagePack.SetSoundex(Name: string);
begin
  if FSoundexName <> Name then
  begin
    RenewGuid;
    FSoundexName := Name;
  end;
end;

procedure TSpellCheckLanguagePack.SetWords(Value: TStrings);
begin
  if FWords.Text <> Value.Text then
  begin
    RenewGuid();
    FWords.Text := Value.Text;
  end;
end;

procedure TSpellCheckLanguagePack.RenewGuid();
begin
  FGuid := GuidToString(TGuid.NewGuid);
end;

procedure TSpellCheckLanguagePack.SetLanguageCode(Value: TTMSLangauages);
begin
  if FLanguageCode <> Value then
  begin
    RenewGuid();
    FLanguageCode := Value;
  end;
end;

constructor TSpellCheckLanguagePack.Create(Collection: TCollection);
begin
  inherited;
  FEnabled := true;
  FWords := TStringList.Create;
  FSoundexName := 'ENGLISH';
  FSpellInfo := TTMSSpellinfo.Create(Self);
  FAffixData := TStringList.Create;
end;

destructor TSpellCheckLanguagePack.Destroy;
begin
  FreeAndNil(FWords);
  FreeAndNil(FSpellInfo);
  FreeAndNil(FAffixData);
  Inherited;
end;

procedure TSpellCheckLanguagePack.SetEnabled(Value: boolean);
begin
  if FEnabled <> Value then
  begin
    RenewGuid;
    FEnabled := Value;
  end;
end;

// ------------------------------------------------------------------------------

{ TCustomAdvSpellCheck }
procedure TCustomAdvSpellCheck.ExecEvents;
var
  I: integer;
  rt: TAdvSpellCheckCallbackContext;
  C: integer;
  rc: TProcessRequestContext;
begin
  LockCallback;
  try
    try
      LockInput;
      for I := 0 to FRequestGroupsList.Count - 1 do
      begin
        if not DoGroupCallbackRequestResults(FRequestGroupsList[I]) then
          if @TAdvSpellcheckRequestGroup(FRequestGroupsList[I]).Callback <> nil
          then
          begin
            TAdvSpellcheckRequestGroup(FRequestGroupsList[I])
              .Callback(Self,
              TAdvSpellcheckRequestGroup(FRequestGroupsList[I]));
            for C := 0 to TAdvSpellcheckRequestGroup(FRequestGroupsList[I])
              .Count - 1 do
              TAdvSpellcheckRequestGroup(FRequestGroupsList[I])
                [C].Executed := true;
          end;
      end;
    finally
      ReleaseInput;
    end;

    for I := 0 to FCallbackList.Count - 1 do
    begin
      if TAdvSpellCheckRequest(FCallbackList[I]).Executed then
        Continue;
      if (TAdvSpellCheckRequest(FCallbackList[I]).RequestType = rtSuggestions)
        and TAdvSpellCheckRequest(FCallbackList[I]).Group.SuggestionsCancelled
      then
      begin
        TAdvSpellCheckRequest(FCallbackList[I]).Executed := true;
        Continue;
      end;
      if (TAdvSpellCheckRequest(FCallbackList[I]).RequestType = rtValidation)
        and TAdvSpellCheckRequest(FCallbackList[I]).Group.ValidationsCancelled
      then
      begin
        TAdvSpellCheckRequest(FCallbackList[I]).Executed := true;
        Continue;
      end;
      try
        begin
          rt := TAdvSpellCheckCallbackContext.Create;
          try
            rt.FOriginalRequest := TAdvSpellCheckRequest(FCallbackList[I])
              .OriginalRequest;
            rt.FData := TAdvSpellCheckRequest(FCallbackList[I]).Data;
            rt.FStringResult := TAdvSpellCheckRequest(FCallbackList[I])
              .StringResult;
            rt.FRequestType := TAdvSpellCheckRequest(FCallbackList[I])
              .RequestType;
            rt.FValidationResult := TAdvSpellCheckRequest(FCallbackList[I])
              .ValidationResult;
            rt.FWordLocation := TAdvSpellCheckRequest(FCallbackList[I])
              .WordLocation;
            rt.FBooleanResult := TAdvSpellCheckRequest(FCallbackList[I])
              .BooleanResult;
            rt.FOriginalRequest := TAdvSpellCheckRequest(FCallbackList[I])
              .OriginalRequest;

            rt.FValidationOptions := TAdvSpellCheckRequest(FCallbackList[I])
              .ValidationOptions;
            rt.FSameCaseSuggestions := TAdvSpellCheckRequest(FCallbackList[I])
              .SameCaseSuggestions;
            rt.WordLocation := TAdvSpellCheckRequest(FCallbackList[I])
              .WordLocation;
            // if (rt.RequestType = rtValidation) and Not rt.BooleanResult then
            if not DoCallbackRequestResults(rt) then
              if @TAdvSpellCheckRequest(FCallbackList[I]).Callback <> nil then
                TAdvSpellCheckRequest(FCallbackList[I]).Callback(Self, rt);
          finally
            TAdvSpellCheckRequest(FCallbackList[I]).Executed := true;
            rt.Free;
          end;
        end;
      except
        ;
      end;
    end;

    if @OnRequestsProcessed <> nil then
    begin
      rc := TProcessRequestContext.Create;
      try
        for I := 0 to FRequestGroupsList.Count - 1 do
          rc.Add(TAdvSpellcheckRequestGroup(FRequestGroupsList[I]));
        DoRequestsProcessed(rc);
      finally
        rc.Free;
      end;
    end;

    for I := 0 to FRequestGroupsList.Count - 1 do
    begin
      TAdvSpellcheckRequestGroup(FRequestGroupsList[I]).CleanupExecuted;
      if TAdvSpellcheckRequestGroup(FRequestGroupsList[I]).Count > 0 then;
    end;

    // if not mt then
    // TThread.Synchronize(FWorkerThread,ExecEvents);//FTimer.Enabled := false;
    FCallbackList.Clear;
  finally
    ReleaseCallback;
  end;
end;

/// //events handling

function TCustomAdvSpellCheck.DoBeforeBeginRequest
  (var Group: TAdvSpellcheckRequestGroup): boolean;
begin
  Result := false;
  if @FBeforeBeginRequest <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Group, @Result]);
    FSyncEvents.EventType := asplOnBeforeBeginRequest;
    FSyncEvents.RunEvent;
    // FBeforeBeginRequest(Self, Group, Result);
  end;
end;

procedure TCustomAdvSpellCheck.DoAfterbeginRequest
  (Group: TAdvSpellcheckRequestGroup);
begin
  if @FAfterBeginRequest <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Group]);
    FSyncEvents.EventType := asplOnAfterBeginRequest;
    FSyncEvents.RunEvent;
    // FAfterBeginRequest(Self, Group);
  end;
end;

function TCustomAdvSpellCheck.DobeforeEndRequest
  (var Group: TAdvSpellcheckRequestGroup): boolean;
begin
  Result := false;
  if @FBeforeEndRequest <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Group, @Result]);
    FSyncEvents.EventType := asplOnBeforeEndRequest;
    FSyncEvents.RunEvent;
    // FBeforeEndRequest(Self, Group, Result);
  end;
end;

procedure TCustomAdvSpellCheck.DoAfterEndRequest
  (Group: TAdvSpellcheckRequestGroup);
begin
  if @FAfterEndRequest <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Group]);
    FSyncEvents.EventType := asplOnAfterEndRequest;
    FSyncEvents.RunEvent;
    // FAfterEndRequest(Self, Group);
  end;
end;

function TCustomAdvSpellCheck.DoGroupCallbackRequestResults
  (Group: TAdvSpellcheckRequestGroup): boolean;
begin
  Result := false;
  if @FRequestGroupResult <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Group, @Result]);
    FSyncEvents.EventType := asplOnRequestGroupResult;
    FSyncEvents.RunEvent;
    // FRequestGroupResult(Self, Group, Result);
  end;
end;

function TCustomAdvSpellCheck.DoCallbackRequestResults(CallBackContext
  : TAdvSpellCheckCallbackContext): boolean;
begin
  Result := false;
  if @FRequestResult <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @CallBackContext, @Result]);
    FSyncEvents.EventType := asplOnRequestResult;
    FSyncEvents.RunEvent;
    // FRequestResult(Self, CallBackContext, Result);
  end;
end;

function TCustomAdvSpellCheck.DoBeforeAddIgnoreWord(var Word: string): boolean;
begin
  Result := false;
  if @FBeforeAddToIgnoreWord <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Word, @Result]);
    FSyncEvents.EventType := asplOnBeforeAddToIgnoreWord;
    FSyncEvents.RunEvent;
    // FBeforeAddToIgnoreWord(Self, Word, Result);
  end;
end;

procedure TCustomAdvSpellCheck.DoAfterAddIgnoreWord(Word: string);
begin
  if @FAfterAddToIgnoreWord <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Word]);
    FSyncEvents.EventType := asplOnAfterAddToIgnoreWord;
    FSyncEvents.RunEvent;
    // FAfterAddToIgnoreWord(Self, Word);
  end;
end;

function TCustomAdvSpellCheck.DoBeforeRemoveIgnoreWord
  (var Word: string): boolean;
begin
  Result := false;
  if @FBeforeRemoveFromIgnoreWord <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Word, @Result]);
    FSyncEvents.EventType := asplOnBeforeRemoveFromIgnoreWord;
    FSyncEvents.RunEvent;
    // FBeforeRemoveFromIgnoreWord(Self, Word, Result);
  end;
end;

procedure TCustomAdvSpellCheck.DoAfterRemoveignoreWord(Word: string);
begin
  if @FAfterRemoveFromignoreWord <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Word]);
    FSyncEvents.EventType := asplOnAfterRemoveFromignoreWord;
    FSyncEvents.RunEvent;
    // FAfterRemoveFromignoreWord(Self, Word);
  end;
end;

function TCustomAdvSpellCheck.DoBeforeSaveConfig(Stream: TStream): boolean;
begin
  Result := false;
  if @FBeforeSaveConfig <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Stream, @Result]);
    FSyncEvents.EventType := asplOnBeforeSaveConfig;
    FSyncEvents.RunEvent;
    // FBeforeSaveConfig(Self, Stream, Result);
  end;
end;

procedure TCustomAdvSpellCheck.DoAfterSaveConfig(Stream: TStream);
begin
  if @FAfterSaveConfig <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Stream]);
    FSyncEvents.EventType := asplOnAfterSaveConfig;
    FSyncEvents.RunEvent;
    // FAfterSaveConfig(Self, Stream);
  end;
end;

function TCustomAdvSpellCheck.DoBeforeLoadConfig(Stream: TStream): boolean;
begin
  Result := false;
  if @FBeforeLoadConfig <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Stream, @Result]);
    FSyncEvents.EventType := asplOnBeforeLoadConfig;
    FSyncEvents.RunEvent;
    // FBeforeLoadConfig(Self, Stream, Result);
  end;
end;

procedure TCustomAdvSpellCheck.DoAfterLoadConfig(Stream: TStream);
begin
  if @FAfterLoadConfig <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Stream]);
    FSyncEvents.EventType := asplOnAfterLoadConfig;
    FSyncEvents.RunEvent;
    // FAfterLoadConfig(Self, Stream);
  end;
end;

function TCustomAdvSpellCheck.DoBeforeSaveIgnoreList(Stream: TStream): boolean;
begin
  Result := false;
  if @FBeforeSaveIgnoreList <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Stream, @Result]);
    FSyncEvents.EventType := asplOnBeforeSaveIgnoreList;
    FSyncEvents.RunEvent;
    // FBeforeSaveIgnoreList(Self, Stream, Result);
  end;
end;

procedure TCustomAdvSpellCheck.DoAfterLoadIgnoreList(Stream: TStream);
begin
  if @FAfterLoadIgnoreList <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Stream]);
    FSyncEvents.EventType := asplOnAfterLoadIgnoreList;
    FSyncEvents.RunEvent;
  end;
  // FAfterLoadIgnoreList(Self, Stream);
end;

procedure TCustomAdvSpellCheck.InsertEmptyData;
begin
  { FDataset.Insert;
    try
    FDataset.FieldByName('CAPTION').AsString:='TMS';
    FDataset.FieldByName('ORGWORD').AsString:='TMS';
    FDataset.FieldByName('SOUNDEX').AsString:='0000';
    FDataset.FieldByName('LANG').AsInteger:=-1;
    finally
    FDataset.Post;
    end; }
end;

procedure TCustomAdvSpellCheck.DoAfterLoadIgnoreList(Words: String);
begin
  if @FAfterLoadIgnoreListText <> nil then
  begin
    // FAfterLoadIgnoreListText(Self, Words);
    FSyncEvents.ArrayOf([@Self, @Words]);
    FSyncEvents.EventType := asplOnAfterLoadIgnoreList;
    FSyncEvents.RunEvent;
  end;
end;

function TCustomAdvSpellCheck.DoBeforeLoadIgnoreList(Stream: TStream): boolean;
begin
  Result := false;
  if @FBeforeLoadIgnoreList <> nil then
  begin
    // FBeforeLoadIgnoreList(Self, Stream, Result);
    FSyncEvents.ArrayOf([@Self, @Stream, @Result]);
    FSyncEvents.EventType := asplOnBeforeLoadIgnoreList;
    FSyncEvents.RunEvent;
  end;
end;

function TCustomAdvSpellCheck.DoBeforeLoadIgnoreList(Words: string): boolean;
begin
  Result := false;
  if @FBeforeLoadIgnoreListText <> nil then
    FBeforeLoadIgnoreListText(Self, Words, Result);
end;

procedure TCustomAdvSpellCheck.DoAfterSaveIgnoreList(Stream: TStream);
begin
  if @FAfterSaveIgnoreList <> nil then
  begin
    // FAfterSaveIgnoreList(Self, Stream);
    FSyncEvents.ArrayOf([@Self, @Stream]);
    FSyncEvents.EventType := asplOnAfterSaveIgnoreList;
    FSyncEvents.RunEvent;
  end;
end;

function TCustomAdvSpellCheck.DoBeforeSaveDB(Stream: TStream): boolean;
begin
  Result := false;
  if @FBeforeSaveIgnoreList <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Stream, @Result]);
    FSyncEvents.EventType := asplOnBeforeSaveDB;
    FSyncEvents.RunEvent;
    // FBeforeSaveIgnoreList(Self, Stream, Result);
  end;
end;

procedure TCustomAdvSpellCheck.DoAfterSaveDB(Stream: TStream);
begin
  if @FAfterSaveDB <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Stream]);
    FSyncEvents.EventType := asplOnAfterSaveDB;
    FSyncEvents.RunEvent;
    // FAfterSaveDB(Self, Stream);
  end;
end;

function TCustomAdvSpellCheck.DoBeforeLoadDB(Stream: TStream): boolean;
begin
  Result := false;
  if @FBeforeLoadIgnoreList <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Stream, @Result]);
    FSyncEvents.EventType := asplOnBeforeLoadDB;
    FSyncEvents.RunEvent;
    // FBeforeLoadIgnoreList(Self, Stream, Result);
  end;
end;

procedure TCustomAdvSpellCheck.DoAfterLoadDB(Stream: TStream);
begin
  if @FAfterLoadDB <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Stream]);
    FSyncEvents.EventType := asplOnAfterLoadDB;
    FSyncEvents.RunEvent;
    // FAfterLoadDB(Self, Stream);
  end;
end;

function TCustomAdvSpellCheck.DoBeforeAppendWordsToDictionary
  (Language: TSpellCheckLanguagePack; var Words: string): boolean;
begin
  Result := false;
  if @FBeforeAppendWordsToDictionary <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Language, @Words, @Result]);
    FSyncEvents.EventType := asplOnBeforeAppendWordsToDictionary;
    FSyncEvents.RunEvent;
    // FBeforeAppendWordsToDictionary(Self, Language, Words, Result);
  end;
end;

procedure TCustomAdvSpellCheck.DoAfterAppendWordsToDictionary
  (Language: TSpellCheckLanguagePack; var Words: string);
begin
  if @FAfterAppendWordsToDictionary <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Language, @Words]);
    FSyncEvents.EventType := asplOnAfterAppendWordsToDictionary;
    FSyncEvents.RunEvent;
    // FAfterAppendWordsToDictionary(Self, Language, Words);
  end;
end;

function TCustomAdvSpellCheck.DoBeforeRefreshDictionary(): boolean;
begin
  Result := false;
  if @FBeforeRefreshDictionary <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Result]);
    FSyncEvents.EventType := asplOnBeforeRefreshDictionary;
    FSyncEvents.RunEvent;
    // FBeforeRefreshDictionary(Self, Result);
  end;
end;

procedure TCustomAdvSpellCheck.DoAfterRefreshDictionary();
begin
  if @FAfterRefreshDictionary <> nil then
  begin
    FSyncEvents.ArrayOf([@Self]);
    FSyncEvents.EventType := asplOnAfterRefreshDictionary;
    FSyncEvents.RunEvent;
    // FAfterRefreshDictionary(Self);
  end;
end;

function TCustomAdvSpellCheck.DoBeforeOpen(): boolean;
begin
  Result := false;
  if @FBeforeOpen <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Result]);
    FSyncEvents.EventType := asplOnBeforeOpen;
    FSyncEvents.RunEvent;
    // FBeforeOpen(Self, Result);
  end;
end;

procedure TCustomAdvSpellCheck.DoAfterOpen;
begin
  if @FAfterOpen <> nil then
  begin
    FSyncEvents.ArrayOf([@Self]);
    FSyncEvents.EventType := asplOnAfterOpen;
    FSyncEvents.RunEvent;
    // FAfterOpen(Self);
  end;
end;

function TCustomAdvSpellCheck.DoBeforeClose: boolean;
begin
  Result := false;
  if @FBeforeClose <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Result]);
    FSyncEvents.EventType := asplOnBeforeClose;
    FSyncEvents.RunEvent;
    // FBeforeClose(Self, Result);
  end;
end;

procedure TCustomAdvSpellCheck.DoAfterClose;
begin
  if @FAfterClose <> nil then
  begin
    FSyncEvents.ArrayOf([@Self]);
    FSyncEvents.EventType := asplOnBeforeClose;
    FSyncEvents.RunEvent;
    // FAfterClose(Self);
  end;
end;

function TCustomAdvSpellCheck.DoBeforeSave(Stream: TStream): boolean;
begin
  Result := false;
  if @FBeforeSave <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Stream, @Result]);
    FSyncEvents.EventType := asplOnBeforeSave;
    FSyncEvents.RunEvent;
    // FBeforeSave(Self, Stream, Result);
  end;
end;

procedure TCustomAdvSpellCheck.DoAfterSave(Stream: TStream);
begin
  if @FAfterSave <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Stream]);
    FSyncEvents.EventType := asplOnAfterSave;
    FSyncEvents.RunEvent;
    // FAfterSave(Self, Stream);
  end;
end;

function TCustomAdvSpellCheck.DoBeforeLoad(Stream: TStream): boolean;
begin
  Result := false;
  if @FBeforeLoad <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Stream, @Result]);
    FSyncEvents.EventType := asplOnBeforeLoad;
    FSyncEvents.RunEvent;
    // FBeforeLoad(Self, Stream, Result);
  end;
end;

procedure TCustomAdvSpellCheck.DoAfterLoad(Stream: TStream);
begin
  if @FAfterLoad <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Stream]);
    FSyncEvents.EventType := asplOnAfterLoad;
    FSyncEvents.RunEvent;
    // FAfterLoad(Self, Stream);
  end;
end;

function TCustomAdvSpellCheck.DoBeforeCleanupDictionary: boolean;
begin
  Result := false;
  if @FAfterCleanupDictionary <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Result]);
    FSyncEvents.EventType := asplOnBeforeCleanupDictionary;
    FSyncEvents.RunEvent;
    // FBeforeCleanupDictionary(Self, Result);
  end;
end;

procedure TCustomAdvSpellCheck.DoAfterCleanupDictionary;
begin
  if @FAfterCleanupDictionary <> nil then
  begin
    FSyncEvents.ArrayOf([@Self]);
    FSyncEvents.EventType := asplOnAfterCleanupDictionary;;
    FSyncEvents.RunEvent;
    // FAfterCleanupDictionary(Self);
  end;
end;

function TCustomAdvSpellCheck.DoBeforeGetSuggestions
  (Context: TAdvSuggestionContext): boolean;
begin
  Result := false;
  if @FBeforeGetSuggestions <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Context, @Result]);
    FSyncEvents.EventType := asplOnBeforeGetSuggestions;
    FSyncEvents.RunEvent;
    // FBeforeGetSuggestions(Self, Context, Result);
  end;
end;

procedure TCustomAdvSpellCheck.DoAfterGetSuggestions
  (Context: TAdvSuggestionContext);
begin
  if @FAfterGetSuggestions <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Context]);
    FSyncEvents.EventType := asplOnAfterGetSuggestions;
    FSyncEvents.RunEvent;
    // FAfterGetSuggestions(Self, Context);
  end;
end;

procedure TCustomAdvSpellCheck.DoAfterGetValidation(Words: string;
  Context: TAdvValidationContext);
begin
  if @FAfterGetValidation <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Context]);
    FSyncEvents.EventType := asplOnAfterGetValidation;
    FSyncEvents.RunEvent;
    // FAfterGetValidation(Self, Context);
  end;
end;

procedure TCustomAdvSpellCheck.DoRequestsProcessed
  (Context: TProcessRequestContext);
begin
  if @OnRequestsProcessed <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Context]);
    FSyncEvents.EventType := aspOnRequestsProcessed;
    FSyncEvents.RunEvent;
  end;
end;

function TCustomAdvSpellCheck.DoBeforeGetValidation(var Words: string;
  Context: TAdvValidationContext): boolean;
begin
  Result := false;
  if @FBeforeGetValidation <> nil then
  begin
    FSyncEvents.ArrayOf([@Self, @Context, @Result]);
    FSyncEvents.EventType := asplOnBeforeGetValidation;
    FSyncEvents.RunEvent;
    // FBeforeGetValidation(Self, Context, Result);
  end;
end;

///

procedure TCustomAdvSpellCheck.Loaded;
begin
  if not(csDesigning in ComponentState) and FTempActive then
    Open;
end;

procedure TCustomAdvSpellCheck.OnTimer(Sender: TObject);
begin
  ExecEvents;
end;

function TCustomAdvSpellCheck.GetRequestGroup(Index: integer)
  : TAdvSpellcheckRequestGroup;
begin
  Exit(TAdvSpellcheckRequestGroup(FRequestGroupsList[Index]));
end;

function TCustomAdvSpellCheck.GetRequestGroupByName(Name: String)
  : TAdvSpellcheckRequestGroup;
var
  I: integer;
begin
  I := GetRequestGroupIndex(Name);
  Result := nil;
  if I <> -1 then
    Exit(TAdvSpellcheckRequestGroup(FRequestGroupsList[I]));
end;

function TCustomAdvSpellCheck.GetRequestGroupIndex(Name: String): integer;
var
  I: integer;
begin
  Result := -1;
  for I := 0 to FRequestGroupsList.Count - 1 do
  begin
    if RequestGroups[I].Name = Name then
      Exit(I);
  end;
end;

function TCustomAdvSpellCheck.GetVersion: string;
var
  vn: integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' +
    IntToStr(Hi(LoWord(vn))) + '.' + IntToStr(Lo(LoWord(vn)));
end;

function TCustomAdvSpellCheck.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TCustomAdvSpellCheck.RemoveRequestGroup(Index: integer);
var
  g: TAdvSpellcheckRequestGroup;
begin
  g := TAdvSpellcheckRequestGroup(FRequestGroupsList[Index]);
  g.Free;

  FRequestGroupsList.Delete(Index);
end;

procedure TCustomAdvSpellCheck.RemoveRequestGroup(Name: String);
var
  I: integer;
  r: TAdvSpellcheckRequestGroup;
begin
  I := GetRequestGroupIndex(Name);
  if I <> -1 then
  begin
    r := RequestGroups[I];
    FRequestGroupsList.Remove(r);
    r.Free;
  end;
end;

procedure TCustomAdvSpellCheck.CleanupRequestGroups();
var
  I: integer;
  g: TAdvSpellcheckRequestGroup;

begin
  for I := 0 to FRequestGroupsList.Count - 1 do
  begin
    g := RequestGroups[I];
    g.Free;
  end;
  FRequestGroupsList.Clear;
end;

procedure TCustomAdvSpellCheck.CreateTable;
begin
  LockDataSet;
  FDataset.Active := false;
  try
    if DoBeforeCleanupDictionary then
      Exit;
    FDataset.IndexName := '';
    FDataset.IndexDefs.Clear;
    FDataset.FieldDefs.Clear;
    FDataset.FieldDefs.Add('ENABLED', ftBoolean, 0);
    FDataset.FieldDefs.Add('LANG', ftInteger, 0);
    FDataset.FieldDefs.Add('CAPTION', ftWideString, 40);
    FDataset.FieldDefs.Add('ORGWORD', ftWideString, 40);
    FDataset.FieldDefs.Add('SOUNDEX', ftWideString, 20);
    FDataset.FieldDefs.Add('FLAGS', ftWideString, 20);
    FDataset.CreateDataSet;
  finally
    if FActive then
      FDataset.Active := true;
    ReleaseDataset;
  end;
  DoAfterCleanupDictionary;
end;

procedure TCustomAdvSpellCheck.CreateDatasetIfNotExists;
var
  dbfile: string;
begin
  dbfile := FixFileName(DatabaseFilename + '.SPLX');

{$IFDEF FMXLIB}
{$IFDEF ANDROID}
  dbfile := DatabaseFilename + '.SPLX';
  dbfile := TPath.GetDocumentsPath + PathDelim + dbfile;
{$ENDIF}
{$IFDEF IOS}
{$ENDIF}
{$ENDIF}
  if FileExists(dbfile) then
  begin
    FActive := true;
    LoadFromFile(dbfile);
    FActive := false;
  end
  else
  begin
    CreateTable;
    if AutoUpdate then
      RefreshDatabase;
  end;
  if FDataset.IndexDefs.IndexOf('CAPTIONIND') = -1 then
    FDataset.IndexDefs.Add('CAPTIONIND', 'ENABLED;CAPTION;LANG', []);
  { if FDataset.IndexDefs.IndexOf('SOUNDEX') = -1 then
    FDataset.IndexDefs.Add('SOUNDEX', 'ENABLED;SOUNDEX', []); }
  if FDataset.IndexDefs.IndexOf('LANGSOUNDEX') = -1 then
    FDataset.IndexDefs.Add('LANGSOUNDEX', 'LANG;ENABLED;SOUNDEX', []);
  FDataset.Active := true;
  FOriginalSettings := ReturnXMLConfig;
end;

function TCustomAdvSpellCheck.GetInmemoryDatabaseName: string;
begin
  if FInMemoryDatabase = '' then
    Exit(FDatabaseFileName)
  else
    Exit(FInMemoryDatabase)
end;

procedure TCustomAdvSpellCheck.RequiredActiveState;
begin
  if not FActive then
    raise Exception.Create
      ('Cannot run this command because component is not in active state');
end;

procedure TCustomAdvSpellCheck.RequiredNonActiveState;
begin
  if FActive then
    raise Exception.Create
      ('Cannot run this command because component is in active state ');
end;

procedure TCustomAdvSpellCheck.SetInMemoryDatabaseName(Value: String);
begin
  RequiredNonActiveState;
  // FDataset.Filename := Value+'.outx';
end;

procedure TCustomAdvSpellCheck.SetVersion(const Value: string);
begin

end;

function TCustomAdvSpellCheck.CheckForActive: boolean;
begin
  Exit(FActive);
end;

procedure TCustomAdvSpellCheck.SetIndexToWord;
begin
  RequiredActiveState;
  // RemoveFilter;
  // FDataset.Active:=false;
  if FDataset.IndexName <> 'CAPTIONIND' then
    FDataset.IndexName := 'CAPTIONIND';
  FDataset.Active := true;
end;

procedure TCustomAdvSpellCheck.SetIndexToSoundex(SingleLangauge: boolean);
begin
  RequiredActiveState;
  if SingleLangauge then
  begin
    if FDataset.IndexName <> 'LANGSOUNDEX' then
      FDataset.IndexName := 'LANGSOUNDEX';
  end
  else
  begin
    if FDataset.IndexName <> 'SOUNDEX' then
      FDataset.IndexName := 'SOUNDEX';
  end;
end;

procedure TCustomAdvSpellCheck.RemoveFilter;
begin
  RequiredActiveState;
  { FDataset.Filtered := false;
    FDataset.Filter := ''; }
  FDataset.CancelRange;
end;

function TCustomAdvSpellCheck.GetDatabasefileName: String;
begin
  Exit(FDatabaseFileName);
end;

procedure TCustomAdvSpellCheck.SetDatabasefileName(Value: String);
begin
  // FDataset.SaveToFile(Value);
  FDatabaseFileName := Value;
end;

procedure TCustomAdvSpellCheck.ExecCommand;
var
  I: integer;
  C: integer;
  al: TList<TAdvSpellCheckRequest>;
begin
  if FRequestsList.Count = 0 then
    Exit;

  LockInput;
  try
    for I := 0 to FRequestsList.Count - 1 do
    begin
      TAdvSpellCheckRequest(FRequestsList[I])
        .Group.Add(TAdvSpellCheckRequest(FRequestsList[I]));
    end;
    FRequestsList.Clear;
  finally
    ReleaseInput;
  end;

  LockDataSet;
  al := TList<TAdvSpellCheckRequest>.Create;
  try
    for I := 0 to FRequestGroupsList.Count - 1 do
    begin
      for C := 0 to RequestGroups[I].Count - 1 do
      begin
        if RequestGroups[I].Items[C].Executed then
          Continue;
        if RequestGroups[I].ValidationsCancelled then
          break;
        if RequestGroups[I].Items[C].RequestType = rtValidation then
        begin
          RequestGroups[I].Items[C].ValidationResult :=
            InternalGetValidate(RequestGroups[I].Items[C].OriginalRequest,
            RequestGroups[I].Items[C].WordLocation,
            RequestGroups[I].Items[C].FValidationOptions);
          RequestGroups[I].Items[C].BooleanResult :=
            Not(RequestGroups[I].Items[C].ValidationResult
            in [TAdvWordValidationResult.wvrNotValidated,
            TAdvWordValidationResult.wvrAttentionRequired]);
          al.Add(RequestGroups[I].Items[C]);
        end;
      end;
      for C := 0 to RequestGroups[I].Count - 1 do
      begin
        if RequestGroups[I].Items[C].Executed then
          Continue;
        if RequestGroups[I].SuggestionsCancelled then
          break;
        if RequestGroups[I].Items[C].RequestType = rtSuggestions then
        begin
          RequestGroups[I].Items[C].StringResult :=
            InternalGetSuggestions(RequestGroups[I].Items[C].OriginalRequest,
            OnlyActiveLangaugeSuggestions,
            RequestGroups[I].Items[C].SameCaseSuggestions, 20);
          al.Add(RequestGroups[I].Items[C]);
        end;
      end;
    end;

    LockCallback;
    try
      for I := 0 to al.Count - 1 do
        FCallbackList.Add(al[I]);
    finally
      ReleaseCallback;
    end;

  finally
    al.Free;
    ReleaseDataset;
  end;

  if FCallbackList.Count > 0 then
  begin
    TThread.Synchronize(FWorkerThread, ExecEvents); // FTimer.Enabled := true;
  end;
end;

procedure TCustomAdvSpellCheck.LockDataSet;
begin
  FDatasetLock.Enter;
end;

procedure TCustomAdvSpellCheck.LockCallback;
begin
  FCallbackLock.Enter;
end;

procedure TCustomAdvSpellCheck.ReleaseCallback;
begin
  FCallbackLock.Leave;
end;

procedure TCustomAdvSpellCheck.ReleaseDataset;
begin
  FDatasetLock.Leave;
end;

procedure TCustomAdvSpellCheck.LockInput;
begin
  FInputLock.Enter;
end;

function TCustomAdvSpellCheck.InternalGetValidate(Word: string;
  WordLocation: TAdvWordLocation; ValidationOptions: TAdvWordCorrection)
  : TAdvWordValidationResult;
var
  cr: TAdvValidationContext;
  ls: string;
  cd, st: TStringList;
  I: integer;
  removedSuffix: string;
  function CheckListExists(Value: String): boolean;
  var
    L: integer;
  begin
    Result := false;
    for L := 0 to cd.Count - 1 do
    begin
      if cd[L] = Value then
        Exit(true);
    end;
  end;
  function AddToCheckedList(Value: string): boolean;
  begin
    Result := false;
    if Not CheckListExists(Value) then
    begin
      cd.Add(Value);
      Exit(true);
    end;
  end;
  function Find(L: TSpellCheckLanguagePack): boolean;
  var
    li: TTMSFlagData;
    E: integer;
  begin
    if not L.Enabled then
      Exit(false);
    Result := false;
    if FDataset.FindKey([true, UpperCase(L.SpellInfo.Encode('dictionary', Word),
      TLocaleOptions.loUserLocale), L.Index]) and
      (FDataset.FieldByName('ORGWORD').AsString = L.SpellInfo.Encode
      ('dictionary', Word)) then
    begin
      Word := L.SpellInfo.Encode('dictionary', Word);
      Exit(true);
    end;

    li := L.SpellInfo.FindFirst(Word, attSuffix);
    if li = nil then
      Exit(false);
    cd := TStringList.Create;
    try
      repeat
        if li.Flag.Name = 'I' then
          Word := Word;
        removedSuffix := li.RemoveAffix(L.SpellInfo.Encode('dictionary',Word));
        if AddToCheckedList(removedSuffix) and (li <> nil) and
          li.CompareSuffix(Word) then
          if FDataset.FindKey([true, UpperCase(L.SpellInfo.Encode('dictionary',
            removedSuffix), TLocaleOptions.loUserLocale), L.Index]) then
          begin
            // if Pos(li.Flag.Name, FDataset.FieldByName('FLAGS').AsString) <> 0 then
            while not FDataset.Eof and
              (FDataset.FieldByName('CAPTION').AsString = UpperCase
              (removedSuffix, TLocaleOptions.loUserLocale)) do
            begin
              ls := li.RemoveAffix(Word);
              if L.Index = FDataset.FieldByName('LANG').AsInteger then
              begin
                st := TStringList.Create;
                try
                  st.Text := L.SpellInfo.GetSuggestionsFor
                    (L.SpellInfo.Encode('dictionary', li.RemoveAffix(Word)),
                    FDataset.FieldByName('FLAGS').AsString);
                  for E := 0 to st.Count - 1 do
                    if UpperCase(st[E], TLocaleOptions.loUserLocale)
                      = UpperCase(L.SpellInfo.Encode('dictionary', Word),
                      TLocaleOptions.loUserLocale) then
                    begin
                      Word := L.SpellInfo.Encode('dictionary',
                        li.RemoveAffix(Word));
                      Exit(true);
                    end;
                finally
                  st.Free;
                end;
                // Exit(true);
              end;
              FDataset.Next;
            end;
          end;
        li := L.SpellInfo.FindNext;
      until (li = nil);
    finally
      cd.Free;
    end;
  end;

begin
  LockDataSet;
  Result := TAdvWordValidationResult.wvrNotValidated;
  try
    cr := TAdvValidationContext.Create;
    try
      cr.Word := Word;
      cr.WordLocation := WordLocation;
      cr.WordLocation := WordLocation;
      cr.ValidationOptions := ValidationOptions;
      cr.FValidationResult := Result;
      cr.FBooleanResult := false;
      if DoBeforeGetValidation(Word, cr) then
        Exit;
    finally
      Word := cr.Word;
      WordLocation := cr.WordLocation;
      WordLocation := cr.WordLocation;
      ValidationOptions := cr.ValidationOptions;
      Result := cr.FValidationResult;
      cr.BooleanResult := Result <> TAdvWordValidationResult.wvrNotValidated;
      cr.Free;
    end;

    if Length(Word) < 2 then
      Exit(TAdvWordValidationResult.wvrValidated);
    if SoundexManager.SoundexValidation(Word) in [0, 2] then
      Exit(TAdvWordValidationResult.wvrValidated);

    Word := Trim(Word);
    if Word = '' then
      Exit(TAdvWordValidationResult.wvrValidated);

    if ExistsInIgnoreList(Word) then
      Exit(TAdvWordValidationResult.wvrValidated);
    Word := Word;
    RemoveFilter;
    SetIndexToWord;
    if not(TAdvWordCorrections.wlcCaseInsensitive in ValidationOptions) then
    begin
      if FDataset.FindKey([true, UpperCase(Word, TLocaleOptions.loUserLocale)])
      then
        Exit(TAdvWordValidationResult.wvrValidated)
      else
      begin
        if Find(Languages[Self.ActiveLanguageIndex]) then
          Exit(TAdvWordValidationResult.wvrValidated);
        for I := 0 to Languages.Count - 1 do
          if I <> ActiveLanguageIndex then
            if Find(Languages[I]) then
              Exit(TAdvWordValidationResult.wvrValidated);
        Exit(TAdvWordValidationResult.wvrNotValidated);
      end;
    end
    else
    begin
      if not(FDataset.FindKey([true, UpperCase(Word,
        TLocaleOptions.loUserLocale)]) and
        (MultiLanguageValidation or (FDataset.FieldByName('LANG').AsInteger=ActiveLanguageIndex))
        and
        ((Word = FDataset.FieldByName('ORGWORD').AsString) or
        (Copy(Word, 1, 1) = UpperCase(Copy(Word, 1, 1),
        TLocaleOptions.loUserLocale)))) then
      begin
        Result := TAdvWordValidationResult.wvrNotValidated;
        while true do
        begin
          if Find(Languages[Self.ActiveLanguageIndex]) then
          begin
            if (Word = FDataset.FieldByName('ORGWORD').AsString) or
              (Copy(Word, 1, 1) = UpperCase(Copy(Word, 1, 1),
              TLocaleOptions.loUserLocale)) then
              Exit(TAdvWordValidationResult.wvrValidated);
            Result := TAdvWordValidationResult.wvrAttentionRequired;
            Break;
          end;
          if MultiLanguageValidation then
            for I := 0 to Languages.Count - 1 do
              if I <> ActiveLanguageIndex then
                if Find(Languages[I]) then
                begin
                  if (Word = FDataset.FieldByName('ORGWORD').AsString) or
                    (Copy(Word, 1, 1) = UpperCase(Copy(Word, 1, 1),
                    TLocaleOptions.loUserLocale)) then
                    Exit(TAdvWordValidationResult.wvrValidated);
                  Result := TAdvWordValidationResult.wvrAttentionRequired;
                  Continue;
                end;

          if Result = TAdvWordValidationResult.wvrNotValidated then
          begin
            if UpperCase(Word, TLocaleOptions.loUserLocale)
              = UpperCase(FDataset.FieldByName('ORGWORD').AsString,
              TLocaleOptions.loUserLocale) then
              break;
            Exit(TAdvWordValidationResult.wvrNotValidated);
          end
          else
            break;
        end;
      end;
      if not(FDataset.FieldByName('ORGWORD').AsString = Word) then
      begin
        while not FDataset.Eof do
        begin
          if FDataset.FieldByName('ORGWORD').AsString = Word then
            Exit(TAdvWordValidationResult.wvrValidated);
          if EditDistance(UpperCase(FDataset.FieldByName('ORGWORD').AsString,
            TLocaleOptions.loUserLocale),
            UpperCase(Word, TLocaleOptions.loUserLocale)) <> 0 then
          begin
            if Copy(Word, 1, 1) = UpperCase(Copy(Word, 1, 1),
              TLocaleOptions.loUserLocale) then
              Exit(TAdvWordValidationResult.wvrValidated)
            else
              Exit(TAdvWordValidationResult.wvrAttentionRequired);
          end;
          FDataset.Next;
        end;
      end;
      if (FDataset.FieldByName('ORGWORD').AsString = Word) and
        (MultiLanguageValidation or (FDataset.FieldByName('LANG').AsInteger = ActiveLanguageIndex)) then
         Exit(TAdvWordValidationResult.wvrValidated)
      else
        Exit(TAdvWordValidationResult.wvrNotValidated);
    end;
  finally
    ReleaseDataset;
    cr := TAdvValidationContext.Create;
    try
      cr.Word := Word;
      cr.WordLocation := WordLocation;
      cr.WordLocation := WordLocation;
      cr.ValidationOptions := ValidationOptions;
      cr.FValidationResult := Result;
      cr.BooleanResult := Result <> TAdvWordValidationResult.wvrNotValidated;
      DoAfterGetValidation(Word, cr);
    finally
      Word := cr.Word;
      Result := cr.FValidationResult;
      cr.BooleanResult := Result <> TAdvWordValidationResult.wvrNotValidated;
      cr.Free;
    end;
  end;
end;

function TCustomAdvSpellCheck.LoadXMLConfig: String;
begin; // not implemented
end;

function TCustomAdvSpellCheck.ReturnXMLConfig: String;
var
  xml: TXMLDocument;
  item, item2, item3: IXMLNode;
  I: integer;
  s: TStringList;
begin
  xml := TXMLDocument.Create(Self);
  xml.Active := true;
  try
    xml.DocumentElement := xml.CreateNode('tms', ntElement, '');
    { xml.DocumentElement.SetAttributeNS('xmlns', '',
      'http://www.tmssoftware.com/tmsspellcheck'); }

    item := xml.DocumentElement.AddChild('spellcheck');
    { item.SetAttributeNS('xmlns', '',
      'http://www.tmssoftware.com/tmsspellcheck/langauges'); }
    item2 := item.AddChild('items');
    { item2.SetAttributeNS('xmlns', '',
      'http://www.tmssoftware.com/tmsspellcheck/langauges/items'); }
    for I := 0 to FLanguages.Count - 1 do
    begin
      item3 := item2.AddChild('item');
      item3.AddChild('enabled').Text := BoolToStr(FLanguages[I].Enabled);
      item3.AddChild('code').Text :=
        LanguageEnumToString(FLanguages[I].FLanguageCode);
      item3.AddChild('filename').Text := FLanguages[I].SourceFileName;
      item3.AddChild('description').Text := FLanguages[I].FDescription;
      item3.AddChild('guid').Text := FLanguages[I].Guid;
      item3.AddChild('words').Text := FLanguages[I].FWords.Text;
      item3.AddChild('soundex').Text := FLanguages[I].SoundexName;
      item3.AddChild('affix').Text := FLanguages[I].AffixFileName;

      if FileExists(FLanguages[I].AffixFileName) then
      begin
        s := TStringList.Create;
        try
          s.LoadFromFile(FLanguages[I].AffixFileName);
          item3.AddChild('affixdata').Text := Base64Encode(s.Text);
          // tnodetype.ntCData;
          // item3.AddChild('affixdata').Text := s.Text;
        finally
          s.Free;
        end;
      end;

      case FLanguages[I].SoundexProcess of
        spStandard:
          item3.AddChild('process').Text := 'STANDARD';
        spGerman:
          item3.AddChild('process').Text := 'GERMAN';
        spFrench:
          item3.AddChild('process').Text := 'FRENCH';
        spSpanish:
          item3.AddChild('process').Text := 'SPANISH';
        spItalian:
          item3.AddChild('process').Text := 'ITALIAN';
        spDutch:
          item3.AddChild('process').Text := 'DUTCH';
      end;

    end;
  finally
    Result := xml.xml.Text;
    xml.Free;
  end;
end;

function TCustomAdvSpellCheck.CompareXMLConfig(S1, S2: string): boolean;
var
  xml1, xml2: TXMLDocument;
  item1, item2: IXMLNode;
  I: integer;
begin
  try
    if (S1 = '') or (S2 = '') then
      Exit(false);

    xml1 := TXMLDocument.Create(Self);
    xml2 := TXMLDocument.Create(Self);
    xml1.Active := true;
    xml2.Active := true;
    xml1.LoadFromXML(string(UTF8Encode(S1)));
    xml2.LoadFromXML(string(UTF8Encode(S2)));
    item1 := xml1.DocumentElement.ChildNodes[0].ChildNodes[0];
    item2 := xml2.DocumentElement.ChildNodes[0].ChildNodes[0];
    try
      if item1.ChildNodes.Count <> item2.ChildNodes.Count then
        Exit(false);
      for I := 0 to item1.ChildNodes.Count - 1 do
      begin
        if FindNode(item1.ChildNodes[I], 'guid').Text <>
          FindNode(item2.ChildNodes[I], 'guid').Text then
          Exit(false);
      end;
      Exit(true);
    finally
      xml1.Free;
      xml2.Free;
    end;
  except
    Exit(false);
  end;
end;

function TCustomAdvSpellCheck.GetActiveDictionaryName: string;
begin
  Exit(FActiveDictionaryName);
end;

procedure TCustomAdvSpellCheck.SetActiveDictionaryName(Name: string);
begin
  FActiveDictionaryName := Name;
  FActiveDictionaryIndex := -1;
  if Not(csDesigning in ComponentState) and FActive then
  begin
    FActiveDictionaryIndex := FLanguages.GetIndexByName(Name);
    if FActiveDictionaryIndex = -1 then
      FActiveDictionaryIndex := FLanguages.GetIndexByLangauge(Name);
    if FActiveDictionaryIndex = -1 then
      FActiveDictionaryIndex := 0;
    SoundexManager.Apply(Languages[FActiveDictionaryIndex].SoundexName);
  end;
end;

procedure TCustomAdvSpellCheck.SetActiveDictionary(Value: integer);
begin
  if Value < Languages.Count then
    SoundexManager.Apply(Languages[Value].SoundexName)
  else
    raise Exception.Create('Invalid language index');
  FActiveDictionaryIndex := Value;
end;

function TCustomAdvSpellCheck.CurrentSoundexProcess: TTMSSoundexProcess;
begin
  if (FActiveDictionaryIndex > -1) and (FActiveDictionaryIndex < FLanguages.Count)
  then
    Exit(FLanguages[FActiveDictionaryIndex].SoundexProcess);
  Exit(spStandard);
end;

function IntToBinByte(Value: Byte): string;
var
  I: integer;
begin
  SetLength(Result, 8);
  for I := 1 to 8 do
  begin
    if (Value shr (8 - I)) and 1 = 0 then
    begin
      Result[I] := '0'
    end
    else
    begin
      Result[I] := '1';
    end;
  end;
end;

function HammingDistance(Word1, Word2: string): Word;
var
  s, r1, r2: string;
  C, I: integer;
begin
  SetLength(r1, Length(Word1) * 16);
  SetLength(r2, Length(Word2) * 16);
  for I := 1 to Length(Word1) do
  begin
    s := IntToBinByte(PByteArray(@Word1[I])[0]);
    for C := 1 to 8 do
      r1[((I - 1) * 16) + C] := s[C];
    s := IntToBinByte(PByteArray(@Word1[I])[1]);
    for C := 1 to 8 do
      r1[(((I - 1) * 16) + 8) + C] := s[C];
  end;
  for I := 1 to Length(Word2) do
  begin
    s := IntToBinByte(PByteArray(@Word1[I])[0]);
    for C := 1 to 8 do
      r2[((I - 1) * 16) + C] := s[C];
    s := IntToBinByte(PByteArray(@Word1[I])[1]);
    for C := 1 to 8 do
      r2[(((I - 1) * 16) + 8) + C] := s[C];
  end;
  Exit(EditDistance(r1, r2));
end;

function TCustomAdvSpellCheck.InternalGetSuggestions(Word: String;
  CurrentLanguageSuggestion: boolean; SameCapsSuggestion: boolean;
  MaxSuggestions: integer): string;
var
  op, st: TStringList;
  start, endx: string;
  edis, sc, scs: integer;
  I: integer;
  fsc, falc: boolean;

  function ReturnString(Value: string; Add: integer): String;
  var
    v: integer;
  begin
    v := 0;
    if Value = '' then
      v := 0;
    if Value <> '' then
      v := StrToInt(Copy(Value, 1, 4) + Copy('0000', 1, 4 - Length(Value)));

    v := v + Add;
    if v < 0 then
      v := 0;
    { if v > 9990 then
      v := 999; }
    Exit(IntToStr(v)); // +));
  end;

  procedure GetCapsInfo;
  var
    S1, S2: string;
    I: integer;
  begin
    S1 := UpperCase(Word, TLocaleOptions.loUserLocale);
    S2 := LowerCase(Word, TLocaleOptions.loUserLocale);
    if S1[1] = Word[1] then
    begin
      fsc := true;
      falc := true;
      for I := 1 to Length(S2) do
      begin
        if (Word[I] = S2[I]) then
        begin
          falc := false;
          Exit;
        end;
      end;
    end;
  end;

  function CheckHaveUpper(Word: string): boolean;
  var
    S1, S2: string;
    I: integer;
  begin
    if Word = '' then
      Exit(false);
    S1 := LowerCase(Word, TLocaleOptions.loUserLocale);
    Result := false;
    if S1[1] <> Word[1] then
    begin
      for I := 1 to Length(S2) do
      begin
        if (Word[I] <> S2[I]) then
        begin
          Exit(true);
        end;
      end;
    end;
  end;

var
  pre, sx: string;
  rc: TAdvSuggestionContext;
  // mc: integer;
  sr: single;
  ast: TStringList;

  procedure AddOp(Value: string);
  var
    j: integer;
  begin
    //Value := Languages[FActiveDictionaryIndex].SpellInfo.Decode('utf8', Value);
    for j := 0 to op.Count - 1 do
      if Value = op[j] then
        Exit;
    op.Add(Value);
  end;

begin
  // Count := 0;
  RequiredActiveState;
  fsc := false;
  falc := false;

  rc := TAdvSuggestionContext.Create;
  try
    rc.FSameCaseSuggestions := SameCapsSuggestion;
    rc.FWord := Word;
    if DoBeforeGetSuggestions(rc) then
      Exit;
  finally
    Word := rc.Word;
    Result := rc.StringResult;
    SameCapsSuggestion := rc.SameCaseSuggestions;
    rc.Free;
  end;

  Word := Trim(Word);
  if Word = '' then
    Exit;

  GetCapsInfo;
  LockDataSet;

  st := TStringList.Create;
  st.Capacity := 100000;
  ast := TStringList.Create;;
  ast.Capacity := 100000;
  try
    SoundexManager.Apply(Languages[FActiveDictionaryIndex].SoundexName);

    Word := Languages[ActiveLanguageIndex].SpellInfo.Encode('dictionary', Word);
    SetIndexToSoundex(CurrentLanguageSuggestion);
    sx := SoundexManager.Soundex(CurrentSoundexProcess, Word);
    if sx = '' then
      Exit('');

    // si := Copy(sx, 2, 3);

    start := Copy(sx, 1, 1); // + }ReturnString(sx, -500);
    endx := Copy(sx, 1, 1) + '999'; // }ReturnString(sx, +500);

    if CurrentLanguageSuggestion then
      pre := 'LANG=' + IntToStr(FActiveDictionaryIndex) + ' and '
    else
      pre := '';
    { FDataset.Filter := pre + 'ENABLED=true and (SOUNDEX>=' + '''' + start + ''''
      + ' and SOUNDEX<=' + '''' + endx + '''' + ')';
      FDataset.Filtered := true; }
    FDataset.SetRangeStart;
    if CurrentLanguageSuggestion then
      FDataset.FieldByName('LANG').AsInteger := FActiveDictionaryIndex;
    FDataset.FieldByName('ENABLED').AsBoolean := true;
    FDataset.FieldByName('SOUNDEX').AsString := start;
    FDataset.SetRangeEnd;
    if CurrentLanguageSuggestion then
      FDataset.FieldByName('LANG').AsInteger := FActiveDictionaryIndex;
    FDataset.FieldByName('ENABLED').AsBoolean := true;
    FDataset.FieldByName('SOUNDEX').AsString := endx;
    FDataset.ApplyRange;

    edis := Round(Length(Word) * 0.60);

    if edis > 4 then
      edis := 4;
    if edis < 4 then
      edis := 4;
    FDataset.First;

    { while not FDataset.Eof do
      begin
      Count := Count + 1;
      // if  then
      ast.Text := FDataset.FieldByName('ORGWORD').AsString;{Languages[FActiveDictionaryIndex].SpellInfo.GetSuggestionsFor
      (FDataset.FieldByName('ORGWORD').AsString, FDataset.FieldByName('Flags')
      .AsString); }

    while not FDataset.Eof do
    begin
      // Count := Count + 1;
      // if FDataset.FieldByName('ORGWORD').AsString = 'Nederlands' then
      // Count := Count;

      sc := EditDistance(UpperCase(Word, TLocaleOptions.loUserLocale),
        UpperCase(FDataset.FieldByName('ORGWORD').AsString,
        TLocaleOptions.loUserLocale));
      { if Length(sx)>Length(FDataset.FieldByName('SOUNDEX').AsString) then begin
        mc:=Round(Length(FDataset.FieldByName('SOUNDEX').AsString) * 0.60);
        scs:=EditDistance(sx,FDataset.FieldByName('SOUNDEX').AsString);
        //        scs:=EditDistance(Copy(sx,1,Length(FDataset.FieldByName('SOUNDEX').AsString)),FDataset.FieldByName('SOUNDEX').AsString)
        end
        else begin
        scs:=EditDistance(FDataset.FieldByName('SOUNDEX').AsString,sx);
        mc:=Round(Length(sx) * 0.60);
        //        scs:=EditDistance(sx,Copy(FDataset.FieldByName('SOUNDEX').AsString,1,Length(sx)));
        end; }
      // scs:=EditDistance(sx,Copy(FDataset.FieldByName('SOUNDEX').AsString,1,Length(sx)));
      // mc:=Round(Length(sx) * 0.60);
      { if UpperCase(FDataset.FieldByName('ORGWORD').AsString)='TITEL' then
        mc:=mc; }
      // mc := Round(Length(sx) * 0.60);
      if (sc <= edis) then
      begin
        scs := EditDistance(FDataset.FieldByName('SOUNDEX').AsString, sx);
        // if (scs <= mc) then
        begin

          ast.Text := Languages[FActiveDictionaryIndex]
            .SpellInfo.GetSuggestionsFor(Languages[FActiveDictionaryIndex].SpellInfo.Decode('dictionary', FDataset.FieldByName('ORGWORD')
            .AsString), FDataset.FieldByName('Flags').AsString);

          // if FDataset.FieldByName('ORGWORD').AsString='web' then
          for I := 0 to ast.Count - 1 do
          begin
            sr := StringSimilarityRatio
              (UpperCase(Word, TLocaleOptions.loUserLocale),
              UpperCase(ast[I], TLocaleOptions.loUserLocale), true);
            if ast[I] = '' then
              Continue;
            if EditDistance(Word, ast[I]) > edis then
              Continue;
            st.Add(Char(EditDistance(ast[I], Word) + 32) +
              Char(EditDistance(ast[I], Word, true) + 32) + IntToStr(sc) +
              IntToStr(scs) + Char(2000 - Round(sr * 1000)) +
              Char(HammingDistance(UpperCase(Word, TLocaleOptions.loUserLocale),
              ast[I]) + 32) + ' ' + ast[I]);
          end;
        end;
        // Dec(edis);
        // if edis < 3 then
        // edis := 3;
      end;
      FDataset.Next;
      // if st.Count + 1 > MaxSuggestions then
      // break;
    end;
  finally
    st.Sort;
    op := TStringList.Create;
    try
      for I := 0 to st.Count - 1 do
      begin
        if SameCapsSuggestion and not CheckHaveUpper(Copy(st[I], 8, 255)) then
        begin
          if falc then
            AddOp(UpperCase(Copy(st[I], 8, 255), TLocaleOptions.loUserLocale))
          else if fsc then
            AddOp(UpperCase(Copy(st[I], 8, 1), TLocaleOptions.loUserLocale) +
              LowerCase(Copy(st[I], 9, 65535), TLocaleOptions.loUserLocale))
          else
            AddOp(Copy(st[I], 8, 255));
        end
        else
          AddOp(Copy(st[I], 8, 255));
        if op.Count > MaxSuggestions then
          break;
      end;
    finally
      Result := op.Text;
      op.Free;
    end;
    st.Free;
    ast.Free;
    ReleaseDataset;
  end;

  rc := TAdvSuggestionContext.Create;
  try
    rc.FSameCaseSuggestions := SameCapsSuggestion;
    rc.FWord := Word;
    rc.StringResult := Result;
    DoAfterGetSuggestions(rc);
  finally
    Word := rc.Word;
    Result := rc.StringResult;
    rc.Free;
  end;
end;

procedure TCustomAdvSpellCheck.ReleaseInput;
begin
  FInputLock.Leave;
end;

procedure TCustomAdvSpellCheck.SaveDB(Filename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FixFileName(Filename), fmCreate);
  try
    SaveDB(fs);
  finally
    fs.Free;
  end;
end;

procedure TCustomAdvSpellCheck.SaveDB(Stream: TStream);
begin
  LockDataSet;
  try
    if DoBeforeSaveDB(Stream) then
      Exit;
    FDataset.SaveToStream(Stream);
  finally
    ReleaseDataset;
  end;
  DoAfterSaveDB(Stream);
end;

procedure TCustomAdvSpellCheck.SaveIgnoreList(Filename: String);

var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FixFileName(Filename), fmCreate);
  try
    SaveIgnoreList(fs);
  finally
    fs.Free;
  end;
end;

procedure TCustomAdvSpellCheck.SaveIgnoreList(Stream: TStream);

begin
  LockDataSet;
  try
    if DoBeforeSaveIgnoreList(Stream) then
      Exit;
    IgnoreList.SaveToStream(Stream, TEncoding.Unicode);
  finally
    ReleaseDataset;
  end;
  DoAfterSaveIgnoreList(Stream);
end;

procedure TCustomAdvSpellCheck.LoadConfigFromString(Config: string);
var
  st: TStringStream;
begin
  st := TStringStream.Create(Config, TEncoding.Unicode);
  try
    st.Position := 0;
    LoadConfig(st);
  finally
    st.Free;
  end;
end;

procedure TCustomAdvSpellCheck.LoadConfig(Filename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FixFileName(Filename), fmOpenRead);
  try
    LoadConfig(fs);
  finally
    fs.Free;
  end;
end;

procedure TCustomAdvSpellCheck.LoadConfig(Stream: TStream);
var
  xml1: TXMLDocument;
  item1, item2: IXMLNode;
  C, I: integer;
  lp: TSpellCheckLanguagePack;
begin
  LockDataSet;
  try
    if DoBeforeLoadConfig(Stream) then
      Exit;
    SoundexManager.Reset;
    Stream.Position := 0;
    try
      xml1 := TXMLDocument.Create(Self);
      xml1.Active := true;
      Stream.Position := 0;
      xml1.LoadFromStream(Stream);
      try
        item1 := xml1.DocumentElement.ChildNodes[0].ChildNodes[0];
        FLanguages.Clear;
        for I := 0 to item1.ChildNodes.Count - 1 do
        begin
          if item1.ChildNodes[I].LocalName = 'item' then
          begin
            lp := FLanguages.Add;
            item2 := item1.ChildNodes[I];
            for C := 0 to item2.ChildNodes.Count - 1 do
            begin
              if item2.ChildNodes[C].LocalName = 'soundex' then
                lp.SoundexName := item2.ChildNodes[C].Text;
              if item2.ChildNodes[C].LocalName = 'affix' then
                lp.AffixFileName := item2.ChildNodes[C].Text;
              if item2.ChildNodes[C].LocalName = 'affixdata' then
              begin
                lp.AffixData.Text := Base64Decode(item2.ChildNodes[C].Text);
                lp.AffixData := lp.AffixData;
              end;
              if item2.ChildNodes[C].LocalName = 'process' then
              begin
                if item2.ChildNodes[C].Text = 'GERMAN' then
                  lp.SoundexProcess := spGerman
                else if item2.ChildNodes[C].Text = 'FRENCH' then
                  lp.SoundexProcess := spFrench
                else if item2.ChildNodes[C].Text = 'SPANISH' then
                  lp.SoundexProcess := spSpanish
                else if item2.ChildNodes[C].Text = 'ITALIAN' then
                  lp.SoundexProcess := spItalian
                else if item2.ChildNodes[C].Text = 'DUTCH' then
                  lp.SoundexProcess := spDutch
                else
                  lp.SoundexProcess := spStandard;
              end;
              if item2.ChildNodes[C].LocalName = 'code' then
                lp.LanguageCode := StringToLanguageEnum
                  (item2.ChildNodes[C].Text);
              if item2.ChildNodes[C].LocalName = 'filename' then
                lp.SourceFileName := item2.ChildNodes[C].Text;
              if item2.ChildNodes[C].LocalName = 'description' then
                lp.Description := item2.ChildNodes[C].Text;
              if item2.ChildNodes[C].LocalName = 'words' then
                lp.Words.Text := item2.ChildNodes[C].Text;
              if (item2.ChildNodes[C].LocalName = 'enabled') and
                (item2.ChildNodes[C].Text <> '') then
                lp.Enabled := StrToBool(item2.ChildNodes[C].Text);
              if item2.ChildNodes[I].LocalName = 'guid' then
                lp.Guid := item2.ChildNodes[C].Text;
            end;
          end;
        end;
      finally
        xml1.Free;
      end;
    except
      ;
    end;
    for I := 0 to Languages.Count - 1 do
      if Languages[I].Enabled then
        SoundexManager.AddSoundexCollection(Languages[I].SoundexName)
          .Enabled := true;
    SoundexManager.Apply;
    ActiveLanguageIndex := ActiveLanguageIndex;
  finally
    ReleaseDataset;
  end;
  DoAfterLoadConfig(Stream);
end;

procedure TCustomAdvSpellCheck.LoadDB(Filename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FixFileName(Filename), fmOpenRead);
  try
    LoadDB(fs);
  finally
    fs.Free;
  end;
end;

procedure TCustomAdvSpellCheck.LoadDB(Stream: TStream);
begin
  RequiredActiveState;
  if Stream.Size = 0 then
    CreateTable
  else
  begin
    LockDataSet;
    FDataset.Active := false;
    try
      Stream.Position := 0;
      if DoBeforeLoadDB(Stream) then
        Exit;
      FDataset.LoadFromStream(Stream);
    finally
      FDataset.Active := true;
      ReleaseDataset;
    end;
  end;
  DoAfterLoadDB(Stream);
end;

procedure TCustomAdvSpellCheck.LoadIgnoreList(Filename: String);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FixFileName(Filename), fmOpenRead);
  try
    LoadIgnoreList(fs);
  finally
    fs.Free;
  end;
end;

procedure TCustomAdvSpellCheck.LoadIgnoreList(Stream: TStream);
begin
  LockDataSet;
  try
    if DoBeforeLoadIgnoreList(Stream) then
      Exit;
    Stream.Position := 0;
    FIgnoreList.LoadFromStream(Stream, TEncoding.Unicode);
  finally
    ReleaseDataset;
  end;
  DoAfterLoadIgnoreList(Stream);
end;

procedure TCustomAdvSpellCheck.SaveToStream(Stream: TStream);
var
  Size: Int64;
  st: TMemoryStream;
begin
  RequiredActiveState;
  LockDataSet;
  st := TMemoryStream.Create;
  try
    st.Size := 0;
    SaveConfig(st);
    Size := st.Size;
    Stream.Write(Size, Sizeof(Int64));
    st.Position := 0;
    Stream.CopyFrom(st, st.Size);
    st.Size := 0;
    if sseIgnoreList in FStoreElements then
    begin
      SaveIgnoreList(st);
    end;
    st.Position := 0;
    Size := st.Size;
    Stream.Write(Size, Sizeof(Int64));
    Stream.CopyFrom(st, st.Size);
    st.Size := 0;
    if sseSpellcheckDB in FStoreElements then
      SaveDB(st);
    st.Position := 0;
    Size := st.Size;
    Stream.Write(Size, Sizeof(Int64));
    Stream.CopyFrom(st, st.Size);
    FOriginalSettings := ReturnXMLConfig;
  finally
    st.Free;
    ReleaseDataset;
  end;
end;

procedure TCustomAdvSpellCheck.LoadFromStream(Stream: TStream);
var
  Size: Int64;
  st: TStringStream;
  s: string;
begin
  RequiredActiveState;
  LockDataSet;
  st := TStringStream.Create;
  try
    st.Size := 0;
    Stream.Read(Size, Sizeof(Int64));
    st.CopyFrom(Stream, Size);
    s := st.DataString;
    FOriginalSettings := s;
    // if LoadConfiguration then
    LoadConfig(st);
    st.Size := 0;
    Stream.Read(Size, Sizeof(Int64));
    if Size > 0 then
    begin
      st.CopyFrom(Stream, Size);
      LoadIgnoreList(st);
    end;

    st.Size := 0;
    Stream.Read(Size, Sizeof(Int64));
    if Size <> 0 then
    begin
      st.CopyFrom(Stream, Size);
      LoadDB(st);
    end
    else
      CreateTable;
    // if not CompareXMLConfig(s, ReturnXMLConfig) then
    // RefreshDatabase()
    // else
  finally
    st.Free;
    ReleaseDataset;
  end;
end;

procedure TCustomAdvSpellCheck.LoadFromFile(Filename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FixFileName(Filename), fmOpenRead);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TCustomAdvSpellCheck.SaveToFile(Filename: String);
var
  fs: TFileStream;
begin
  FUpdateRequired := false;
  RequiredActiveState;
  fs := TFileStream.Create(FixFileName(Filename), fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TCustomAdvSpellCheck.SaveConfig(Filename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FixFileName(Filename), fmCreate);
  try
    SaveConfig(fs);
  finally
    fs.Free;
  end;
end;

procedure TCustomAdvSpellCheck.SaveConfig(Stream: TStream);
var
  st: TStringStream;
begin
  if DoBeforeSaveConfig(Stream) then
    Exit;
  st := TStringStream.Create(ReturnXMLConfig, TEncoding.UTF8);
  try
    st.WriteString(ReturnXMLConfig);
    st.Position := 0;
    Stream.CopyFrom(st, st.Size);
  finally
    st.Free;
  end;
  DoAfterSaveConfig(Stream);
end;

procedure TCustomAdvSpellCheck.CancelSuggestions(AnId: string);
var
  I: integer;
  g: TAdvSpellcheckRequestGroup;
begin
  g := GetRequestGroupByName(AnId);
  if g <> nil then
    g.CancelSuggestions;
  LockInput;
  try
    for I := 0 to FRequestsList.Count - 1 do
      if (TAdvSpellCheckRequest(FRequestsList[I]).Group = g) and
        (TAdvSpellCheckRequest(FRequestsList[I]).RequestType = rtValidation)
      then
        TAdvSpellCheckRequest(FRequestsList[I]).Executed := true;
  finally
    ReleaseInput;
  end;
end;

procedure TCustomAdvSpellCheck.CancelValidations(AnId: string);
var
  I: integer;
  g: TAdvSpellcheckRequestGroup;
begin
  g := GetRequestGroupByName(AnId);
  if g <> nil then
    g.CancelValidations;
  LockInput;
  try
    for I := 0 to FRequestsList.Count - 1 do
      if (TAdvSpellCheckRequest(FRequestsList[I]).Group = g) and
        (TAdvSpellCheckRequest(FRequestsList[I]).RequestType = rtValidation)
      then
        TAdvSpellCheckRequest(FRequestsList[I]).Executed := true;
  finally
    ReleaseInput;
  end;
end;

procedure TCustomAdvSpellCheck.CancelAllRequests(AnId: string);
var
  I: integer;
  g: TAdvSpellcheckRequestGroup;
begin
  g := GetRequestGroupByName(AnId);
  if g <> nil then
    g.CancelAll;
  LockInput;
  try
    for I := 0 to FRequestsList.Count - 1 do
      if TAdvSpellCheckRequest(FRequestsList[I]).Group = g then
        TAdvSpellCheckRequest(FRequestsList[I]).Executed := true;
  finally
    ReleaseInput;
  end;
end;

procedure TCustomAdvSpellCheck.BeginRequest(AnId: string;
  Callback: TSpellCheckGroupCallBack = nil);
begin
  LockInput;
  FCurrentGroup := GetRequestGroupByName(AnId);
  if DoBeforeBeginRequest(FCurrentGroup) then
    Exit;
  if FCurrentGroup = nil then
  begin
    FCurrentGroup := TAdvSpellcheckRequestGroup.Create;
    FCurrentGroup.Name := AnId;
    FRequestGroupsList.Add(FCurrentGroup);
  end;
  FCurrentGroup.Callback := Callback;
  DoAfterbeginRequest(FCurrentGroup);
end;

function TCustomAdvSpellCheck.ActiveLanguage: TSpellCheckLanguagePack;
begin
  Result := nil;

  if (Languages.Count > 0) and (ActiveLanguageIndex >= 0) and
    (ActiveLanguageIndex < Languages.Count) then
  begin
    Result := Languages[ActiveLanguageIndex];
  end;
end;

procedure TCustomAdvSpellCheck.AddSuggestionRequest(Word: string;
  Data: TSPObject; Callback: TSpellCheckRequestCallBack = nil;
  SameCapsSuggestion: boolean = true);
var
  rq: TAdvSpellCheckRequest;
begin
  Word := Trim(Word);
  if Word = '' then
    Exit;
  if FCurrentGroup = nil then
    raise Exception.Create('Please run Begin Request before run this command');
  rq := TAdvSpellCheckRequest.Create;
  rq.RequestType := TAdvResultTypes.rtSuggestions;
  rq.SameCaseSuggestions := SameCapsSuggestion;
  rq.OriginalRequest := Word;
  rq.Data := Data;
  rq.Callback := Callback;
  rq.Group := FCurrentGroup;
  FRequestsList.Add(rq);
end;

procedure TCustomAdvSpellCheck.AddValidationRequest(Word: string;
  Data: TSPObject; Callback: TSpellCheckRequestCallBack;
  WordLocation: TAdvWordLocation = TAdvWordLocation.wlMiddle;
  ValidationOptions: TAdvWordCorrection =
  [TAdvWordCorrections.wlcStartWithCapitalWords,
  TAdvWordCorrections.wlcCaseInsensitive]);
var
  rq: TAdvSpellCheckRequest;
begin
  Word := Trim(Word);
  if Word = '' then
    Exit;
  if FCurrentGroup = nil then
    raise Exception.Create('Please run Begin Request before run this command');
  rq := TAdvSpellCheckRequest.Create;
  rq.RequestType := TAdvResultTypes.rtValidation;
  rq.WordLocation := WordLocation;
  rq.ValidationOptions := ValidationOptions;
  rq.OriginalRequest := Word;
  rq.Data := Data;
  rq.Callback := Callback;
  rq.Group := FCurrentGroup;
  FRequestsList.Add(rq);
end;

procedure TCustomAdvSpellCheck.EndRequest;
begin
  if DobeforeEndRequest(FCurrentGroup) then
    Exit;
  FCurrentGroup := nil;
  ReleaseInput;
  if FWorkerThread.Suspended then
    FWorkerThread.Suspended := false;
  DoAfterEndRequest(FCurrentGroup);
end;

procedure TCustomAdvSpellCheck.AppendWordsList(Words: string);
begin
  AppendWordsList(nil, Words);
end;

procedure TCustomAdvSpellCheck.LoadWordsList(Language: TSpellCheckLanguagePack;
  Stream: TStream);
begin
  CreateTable;
  AppendWordsList(Language, Stream);
end;

procedure TCustomAdvSpellCheck.RemoveFromDictionary
  (Language: TSpellCheckLanguagePack; Words: string);
var
  I, C: integer;
  st, stout: TStringList;
  f: boolean;
begin
  RequiredActiveState;
  Words := Trim(Words);
  if Language = nil then
    Language := Languages[ActiveLanguageIndex];

  st := TStringList.Create;
  stout := TStringList.Create;
  try
    st.Text := Words;
    for C := 0 to st.Count - 1 do
    begin
      f := false;
      for I := 0 to Language.Words.Count - 1 do
      begin
        if Language.Words[I] = st[C] then
        begin
          FUpdateRequired := true;
          Language.Words.Delete(I);
          f := true;
          break;
        end;
      end;
      if f then
        stout.Add(st[C]);
    end;
    LockDataSet;
    try
      SetIndexToWord;
      for I := 0 to stout.Count - 1 do
        if FDataset.FindKey([true, UpperCase(stout[I],
          TLocaleOptions.loUserLocale), Language.Index]) then
          FDataset.Delete;
    finally
      ReleaseDataset;
    end;
  finally
    Words := stout.Text;
    st.Free;
    stout.Free;
  end;
  FUpdateRequired := true;
  // AppendWordsList(Language, Words);
end;

procedure TCustomAdvSpellCheck.AddToDictionary
  (Language: TSpellCheckLanguagePack; Words: string);
var
  I, C: integer;
  st, stout: TStringList;
  f: boolean;
begin
  RequiredActiveState;
  Words := Trim(Words);
  if Language = nil then
    Language := Languages[ActiveLanguageIndex];

  st := TStringList.Create;
  stout := TStringList.Create;
  try
    st.Text := Words;
    for C := 0 to st.Count - 1 do
    begin
      f := false;
      for I := 0 to Language.Words.Count - 1 do
      begin
        if Language.Words[I] = st[C] then
        begin
          f := true;
          break;
        end;
      end;
      if Not f then
        stout.Add(st[C]);
    end;
    for I := 0 to stout.Count - 1 do
      if Trim(stout[I]) <> '' then
        Language.Words.Add(Trim(stout[I]));
  finally
    Words := stout.Text;
    st.Free;
    stout.Free;
  end;
  if Words = '' then
    Exit;
  FUpdateRequired := true;
  AppendWordsList(Language, Words);
end;

procedure TCustomAdvSpellCheck.AppendWordsList
  (Language: TSpellCheckLanguagePack; Words: string);
var
  str: TStringStream;
begin
  str := TStringStream.Create('', TEncoding.Unicode);
  str.WriteString(Words);
  try
    AppendWordsList(Language, str);
  finally
    str.Free;
  end;
end;

procedure TCustomAdvSpellCheck.Flash();
var
  st: TStringList;
  I: integer;
  xw, aw: string;
  Language: TSpellCheckLanguagePack;
  cl: integer;
begin
  cl := -1;
  LockDataSet;
  st := InternalWords;
  try
    st.Sort;

    if (FProgressform <> nil) then
    begin
{$IFNDEF FMXLIB}
      TUpdateDictProgFrm(FProgressform).ProgressBar1.Max := st.Count;
      TUpdateDictProgFrm(FProgressform).Label1.Caption := 'Updating Dictionary';
      Application.ProcessMessages;
{$ENDIF}
    end;
    for I := 0 to st.Count - 1 do
    begin
      if (FProgressform <> nil) and (I mod 300 = 0) then
      begin
{$IFNDEF FMXLIB}
        TUpdateDictProgFrm(FProgressform).ProgressBar1.Position := I;
        Application.ProcessMessages;
{$ENDIF}
      end;
      aw := Trim(FixTextForDict(st[I]));
      xw := Trim(GetFlagsDict(st[I]));
      Language := Languages[GetLangIndex(st[I])];
      if cl <> Language.Index then
        SoundexManager.Apply(Language.SoundexName);
      cl := Language.Index;

      if aw = '' then
        Continue;
      FDataset.AppendRecord([Language.Enabled, Language.Index,
        UpperCase(aw, TLocaleOptions.loUserLocale), aw,
        SoundexManager.Soundex(Language.SoundexProcess, aw), xw]);

      { FDataset.Insert;
        FDataset.FieldByName('ENABLED').AsBoolean := Language.Enabled;
        FDataset.FieldByName('LANG').AsInteger := Language.Index;
        FDataset.FieldByName('CAPTION').AsString :=
        UpperCase(aw, TLocaleOptions.loUserLocale);
        FDataset.FieldByName('ORGWORD').AsString := aw; // ;
        FDataset.FieldByName('SOUNDEX').AsString :=
        SoundexManager.Soundex(Language.SoundexProcess, aw);
        FDataset.FieldByName('FLAGS').AsString := xw;

        FDataset.Post; }
      // if I>22000{16950} then
      // Exit;
    end;
  finally
    ReleaseDataset;
    InternalWords.Clear;
  end;
end;

procedure TCustomAdvSpellCheck.AppendWordsList
  (Language: TSpellCheckLanguagePack; Stream: TStream);
var
  st, st2, st3: TStringList;
  I: integer;
  xw, aw, w: string;
  C: integer;
begin
  st := InternalWords;
  st2 := TStringList.Create;
  st3 := TStringList.Create;
  try
    if Language = nil then
      Language := Languages[ActiveLanguageIndex];
    Stream.Position := 0;
    st2.LoadFromStream(Stream, TEncoding.Unicode);
    w := st2.Text;
    if DoBeforeAppendWordsToDictionary(Language, w) then
      Exit;
    st2.Text := w;
    for I := 0 to st2.Count - 1 do
    begin
      aw := Trim(FixTextForDict(st2[I]));
      xw := Trim(GetFlagsDict(st2[I]));
      st3.Text := Language.SpellInfo.GetPrefixSuggestionsFor(aw, xw);
      for C := 0 to st3.Count - 1 do
      begin
        if st3[C] = '' then
          Continue;
        st.Add(st3[C] + '%%' + Char(Language.Index + 32));
      end;
    end;
    if not Updating then
      Flash;
    DoAfterAppendWordsToDictionary(Language, w);
  finally
    st2.Free;
    st3.Free;
  end;
end;

procedure TCustomAdvSpellCheck.LoadWordsList(Language: TSpellCheckLanguagePack;
  Filename: string);
var
  f: TFileStream;
begin
  f := TFileStream.Create(FixFileName(Filename), fmOpenRead);
  try
    LoadWordsList(Language, f);
  finally
    f.Free;
  end;
end;

procedure TCustomAdvSpellCheck.OpenSettingsDialog;
{$IFNDEF FMXLIB}
var
  frm: TSPLCheckConfFrm;
{$ENDIF}

{$IFDEF FMXLIB}
var
  frm: TSpellConfigFrm;
{$ENDIF}

begin
{$IFNDEF FMXLIB}
  frm := TSPLCheckConfFrm.Create(nil);

{$ENDIF}
{$IFDEF FMXLIB}
  frm := TSpellConfigFrm.CreateNew(nil);
  frm.Init;
{$ENDIF}
  try
    frm.LoadData(TAdvSpellCheck(Self));
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TCustomAdvSpellCheck.AppendWords(Language: TSpellCheckLanguagePack;
  Words: String);
var
  f: TStringStream;
begin
  f := TStringStream.Create(Words);
  try
    AppendWordsList(Language, f);
  finally
    f.Free;
  end;
end;

procedure TCustomAdvSpellCheck.AppendWordsListFromFile
  (Language: TSpellCheckLanguagePack; Filename: String);
var
  f: TFileStream;
begin
  f := TFileStream.Create(FixFileName(Filename), fmOpenRead);
  try
    AppendWordsList(Language, f);
  finally
    f.Free;
  end;
end;

procedure TCustomAdvSpellCheck.RefreshDatabase;
var
  ol, I: integer;
{$IFNDEF FMXLIB}
  Ce, Te: integer;
{$ENDIF}
begin;
  if DoBeforeRefreshDictionary() then
    Exit;
  CreateTable;
  InsertEmptyData;
  FUpdateRequired := true;
  if FDataset.IndexDefs.IndexOf('CAPTIONIND') = -1 then
    FDataset.IndexDefs.Add('CAPTIONIND', 'ENABLED;CAPTION;LANG', []);
  if FDataset.IndexDefs.IndexOf('SOUNDEX') = -1 then
    FDataset.IndexDefs.Add('SOUNDEX', 'ENABLED;SOUNDEX', []);
  if FDataset.IndexDefs.IndexOf('LANGSOUNDEX') = -1 then
    FDataset.IndexDefs.Add('LANGSOUNDEX', 'LANG;ENABLED;SOUNDEX', []);

  ol := FActiveDictionaryIndex;

{$IFNDEF FMXLIB}
  FProgressform := TUpdateDictProgFrm.Create(nil);
{$ENDIF}
  try
    BeginUpdate;
    // if FProgressform <> nil then
    // FProgressform.show;
{$IFNDEF FMXLIB}
    Te := 1;
    Ce := 0;
    for I := 0 to FLanguages.Count - 1 do
      if FLanguages[I].Enabled then
        Inc(Ce);
{$ENDIF}
    for I := 0 to FLanguages.Count - 1 do
    begin
      ActiveLanguageIndex := I;
      if not FLanguages[I].Enabled then
        Continue;

{$IFNDEF FMXLIB}
      if FProgressform <> nil then
      begin
        TUpdateDictProgFrm(FProgressform).Label1.Caption :=
          FLanguages[I].Description + ' ' + LanguageEnumToString
          (FLanguages[I].FLanguageCode) + ' ' + IntToStr(Te) + ' of ' +
          IntToStr(Ce);
        Application.ProcessMessages;
      end;
{$ENDIF}
      if Languages[I].Words.Text <> '' then
      begin
        if not FProgressform.Visible then
          FProgressform.show;
        AppendWordsList(Languages[I], Languages[I].Words.Text);
      end;
      if FileExists(FixFileName(Languages[I].SourceFileName)) then
      begin
        {$IFNDEF FMXLIB}
        if not FProgressform.Visible then
          FProgressform.Show;
        {$ENDIF}
        AppendWordsListFromFile(Languages[I], Languages[I].SourceFileName);
      end;

{$IFNDEF FMXLIB}
      Inc(Te);
{$ENDIF}
    end;
  finally
    EndUpdate;
    ActiveLanguageIndex := ol;
    FreeAndNil(FProgressform);
  end;
  DoAfterRefreshDictionary;
end;

procedure TCustomAdvSpellCheck.Open;
begin
  RequiredNonActiveState;
  if Not FActive then
  begin
    if DoBeforeOpen then
      Exit;
    CreateDatasetIfNotExists;
    FDataset.Active := true;
    FActive := true;
  end;
  DoAfterOpen;
end;

procedure TCustomAdvSpellCheck.Close;
begin
  RequiredActiveState;
  if DoBeforeClose then
    Exit;
  FDataset.Active := false;
  FActive := false;
  DoAfterClose();
end;

function TCustomAdvSpellCheck.Validate(Word: string;
  WordLocation: TAdvWordLocation = TAdvWordLocation.wlMiddle;
  ValidationOptions: TAdvWordCorrection =
  [TAdvWordCorrections.wlcCaseInsensitive,
  TAdvWordCorrections.wlcAllCapitalWords]): TAdvWordValidationResult;
begin
  Exit(Self.InternalGetValidate(Word, WordLocation, ValidationOptions));
end;

function TCustomAdvSpellCheck.FirstSuggestion(Word: string;
  SameCapsSuggestion: boolean = true): string;
var
  sl: TStringList;
begin
  Result := '';
  sl := TStringList.Create;
  try
    sl.Text := InternalGetSuggestions(Word, OnlyActiveLangaugeSuggestions,
      SameCapsSuggestion, 20);
    if sl.Count > 0 then
      Result := sl.Strings[0];
  finally
    sl.Free;
  end;
end;

function TCustomAdvSpellCheck.Suggestions(Word: string;
  SameCapsSuggestion: boolean = true): String;
begin
  Exit(InternalGetSuggestions(Word, OnlyActiveLangaugeSuggestions,
    SameCapsSuggestion, 20));
end;

function TCustomAdvSpellCheck.IndexInIgnoreList(Word: string): integer;
var
  I: integer;
begin
  FUpdateRequired := true;
  Word := Word;
  Result := -1;
  for I := 0 to FIgnoreList.Count - 1 do
    if UpperCase(Word, TLocaleOptions.loUserLocale) = UpperCase(FIgnoreList[I],
      TLocaleOptions.loUserLocale) then
      Exit(I);
end;

function TCustomAdvSpellCheck.ExistsInIgnoreList(Word: string): boolean;
begin
  Exit(IndexInIgnoreList(Word) <> -1);
end;

procedure TCustomAdvSpellCheck.AddToIgnoreList(Word: string);
begin
  if DoBeforeAddIgnoreWord(Word) then
    Exit;
  RequiredActiveState;
  FUpdateRequired := true;
  LockDataSet;
  try
    if Not ExistsInIgnoreList(Word) then
      FIgnoreList.Add(Word);
    FUpdateRequired := true;
  finally
    ReleaseDataset;
  end;
  DoAfterAddIgnoreWord(Word);
end;

procedure TCustomAdvSpellCheck.RemoveFromIgnoreList(Word: string);
var
  I: integer;
begin
  if DoBeforeRemoveIgnoreWord(Word) then
    Exit;
  RequiredActiveState;
  FUpdateRequired := true;
  LockDataSet;
  try
    I := IndexInIgnoreList(Word);
    if I <> -1 then
    begin
      FIgnoreList.Delete(I);
      FUpdateRequired := true;
    end;
  finally
    ReleaseDataset
  end;
  DoAfterRemoveignoreWord(Word);
end;

procedure TCustomAdvSpellCheck.LoadIgnoreListWords(Words: String);
begin
  if DoBeforeLoadIgnoreList(Words) then
    Exit;
  // todo: fix this .
  RequiredActiveState;
  FUpdateRequired := true;
  LockDataSet;
  try
    Words := Words;
    FIgnoreList.Text := Words;
  finally
    ReleaseDataset;
  end;
  DoAfterLoadIgnoreList(Words);
end;

procedure TCustomAdvSpellCheck.CleanupIgnoreList();
begin
  RequiredActiveState;
  FUpdateRequired := true;
  FIgnoreList.Clear;
end;

procedure TCustomAdvSpellCheck.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

function TCustomAdvSpellCheck.Updating: boolean;
begin
  Exit(FUpdateCount <> 0);
end;

procedure TCustomAdvSpellCheck.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount < 0 then
    FUpdateCount := 0;
  if FUpdateCount = 0 then
    Flash;
end;

constructor TCustomAdvSpellCheck.Create(Owner: TComponent);
var
  spc: TSpellCheckLanguagePack;
begin
  Inherited Create(Owner);
  FSyncEvents := TAdvSpellcheckSynchronizedEvent.Create(Self);
  FLanguages := TSpellCheckLanguagePackCollection.Create(Self);
  FDataset := TClientDataset.Create(nil);
  FIgnoreList := TStringList.Create;
  FStoreElements := [sseSpellcheckDB, sseIgnoreList];
  FDatabaseFileName := 'TMSSPELLCHECK';
  FInMemoryDatabase := 'TMSSPELLCHECK';
  FDatasetLock := TCriticalSection.Create;
  FInputLock := TCriticalSection.Create;
  FRequestGroupsList := TList<TAdvSpellcheckRequestGroup>.Create;
  FRequestsList := TList<TAdvSpellCheckRequest>.Create;
  FCallbackList := TList<TAdvSpellCheckRequest>.Create;
  FCallbackLock := TCriticalSection.Create;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := false;
  FTimer.Interval := 50;
  FTimer.OnTimer := OnTimer;
  FWorkerThread := TSpellCheckWorkerThread.Create(Self);
  FAutoupdate := false;

  if not(csReading in ComponentState) and (csDesigning in ComponentState) then
  begin
    spc := Languages.Add;
    spc.SetLanguageCode(lcEnglish);
    spc.SetSourceFileName('English.lat');
    spc.SetAffixFilename('English.aff');
    spc.Description := 'English Dictionary';
  end;
  FSoundex := TTMSoundexManager.Create;
  FCurrentLangaugeSuggestions := true;
  InternalWords := TStringList.Create;
end;

destructor TCustomAdvSpellCheck.Destroy;
begin
  if FActive and (not(csDesigning in ComponentState) and AutoUpdate and
    (FUpdateRequired or not CompareXMLConfig(ReturnXMLConfig,
    FOriginalSettings))) then
    SaveToFile(DatabaseFilename + '.SPLX');
  FWorkerThread.Terminate;
  if FWorkerThread.Started then
    FWorkerThread.WaitFor;
  CleanupRequestGroups;
  FreeAndNil(InternalWords);
  FreeAndNil(FSyncEvents);
  FreeAndNil(FSoundex);
  FreeAndNil(FWorkerThread);
  FreeAndNil(FTimer);
  FreeAndNil(FCallbackLock);
  FreeAndNil(FCallbackList);
  FreeAndNil(FRequestsList);
  FreeAndNil(FRequestGroupsList);
  FreeAndNil(FDatasetLock);
  FreeAndNil(FInputLock);
  FreeAndNil(FLanguages);
  FreeAndNil(FIgnoreList);
  FreeAndNil(FDataset);
  Inherited;
end;

procedure TCustomAdvSpellCheck.SetActive(Value: boolean);
begin
  if csDesigning in ComponentState then
  begin
    FActive := Value;
    Exit;
  end;
  if csReading in ComponentState then
  begin
    FTempActive := Value;
    Exit;
  end;

  if not FActive and Value then
    Open;

  if FActive and Not Value then
    Close;
end;

// ------------------------------------------------------------------------------

{ TAdvSpellcheckSynchronizedEvent }
procedure TAdvSpellcheckSynchronizedEvent.ArrayOf(const Values
  : array of TSPObject);
var
  I: integer;
begin
  for I := 0 to High(Values) do
    Parameters[I] := Values[I];
end;

constructor TAdvSpellcheckSynchronizedEvent.Create
  (AParent: TCustomAdvSpellCheck);
begin
  Inherited Create;
  FParent := AParent;
end;

destructor TAdvSpellcheckSynchronizedEvent.Destroy();
begin
  Inherited;
end;

procedure TAdvSpellcheckSynchronizedEvent.RunEvent();
begin
  if EventType in FParent.AsyncEventsList then
    Execute
  else
    TThread.Synchronize(nil, Execute);
end;

procedure TAdvSpellcheckSynchronizedEvent.Execute();
begin
  try
    if Application.Terminated then
      Exit;
    case EventType of
      aspOnRequestsProcessed:
        if @FParent.OnRequestsProcessed <> nil then
          FParent.OnRequestsProcessed(TObject(Parameters[0]^),
            TProcessRequestContext(Parameters[1]^));
      asplOnBeforeBeginRequest:
        if @FParent.OnBeforeBeginRequest <> nil then
          FParent.FBeforeBeginRequest(TObject(Parameters[0]^),
            TAdvSpellcheckRequestGroup(Parameters[1]^),
            boolean(Parameters[2]^));
      asplOnBeforeEndRequest:
        if @FParent.OnBeforeEndRequest <> nil then
          FParent.OnBeforeEndRequest(TObject(Parameters[0]^),
            TAdvSpellcheckRequestGroup(Parameters[1]^),
            boolean(Parameters[2]^));
      asplOnAfterBeginRequest:
        if @FParent.OnAfterBeginRequest <> nil then
          FParent.OnAfterBeginRequest(TObject(Parameters[0]^),
            TAdvSpellcheckRequestGroup(Parameters[1]^));
      asplOnAfterEndRequest:
        if @FParent.OnAfterEndRequest <> nil then
          FParent.OnAfterEndRequest(TObject(Parameters[0]^),
            TAdvSpellcheckRequestGroup(Parameters[1]^));
      asplOnRequestGroupResult:
        if @FParent.OnRequestGroupResult <> nil then
          FParent.OnRequestGroupResult(TObject(Parameters[0]^),
            TAdvSpellcheckRequestGroup(Parameters[1]^),
            boolean(Parameters[2]^));
      asplOnRequestResult:
        if @FParent.OnRequestResult <> nil then
          FParent.OnRequestResult(TObject(Parameters[0]^),
            TAdvSpellCheckCallbackContext(Parameters[1]^),
            boolean(Parameters[2]^));
      asplOnBeforeLoadIgnoreListText:
        if @FParent.OnBeforeLoadIgnoreListText <> nil then
          FParent.OnBeforeLoadIgnoreListText(TObject(Parameters[0]^),
            String(Parameters[1]^), boolean(Parameters[2]^));
      asplOnAfterLoadIgnoreListText:
        if @FParent.OnAfterLoadIgnoreListText <> nil then
          FParent.OnAfterLoadIgnoreListText(TObject(Parameters[0]^),
            String(Parameters[1]^));
      asplOnBeforeAddToIgnoreWord:
        if @FParent.OnBeforeAddToIgnoreWord <> nil then
          FParent.OnBeforeAddToIgnoreWord(TObject(Parameters[0]^),
            String(Parameters[1]^), boolean(Parameters[2]^));
      asplOnAfterAddToIgnoreWord:
        if @FParent.OnAfterAddToIgnoreWord <> nil then
          FParent.OnAfterAddToIgnoreWord(TObject(Parameters[0]^),
            String(Parameters[1]^));
      asplOnBeforeRemoveFromIgnoreWord:
        if @FParent.OnBeforeRemoveFromIgnoreWord <> nil then
          FParent.OnBeforeRemoveFromIgnoreWord(TObject(Parameters[0]^),
            String(Parameters[1]^), boolean(Parameters[2]^));
      asplOnAfterRemoveFromignoreWord:
        if @FParent.OnAfterRemoveFromignoreWord <> nil then
          FParent.OnAfterRemoveFromignoreWord(TObject(Parameters[0]^),
            String(Parameters[1]^));
      asplOnBeforeSaveConfig:
        if @FParent.OnBeforeSaveConfig <> nil then
          FParent.OnBeforeSaveConfig(TObject(Parameters[0]^),
            TStream(Parameters[1]^), boolean(Parameters[2]^));
      asplOnAfterSaveConfig:
        if @FParent.OnAfterSaveConfig <> nil then
          FParent.OnAfterSaveConfig(TObject(Parameters[0]^),
            TStream(Parameters[1]^));
      asplOnBeforeLoadConfig:
        if @FParent.OnBeforeLoadConfig <> nil then
          FParent.OnBeforeLoadConfig(TObject(Parameters[0]^),
            TStream(Parameters[1]^), boolean(Parameters[2]^));
      asplOnAfterLoadConfig:
        if @FParent.OnAfterLoadConfig <> nil then
          FParent.OnAfterLoadConfig(TObject(Parameters[0]^),
            TStream(Parameters[1]^));
      asplOnBeforeSaveIgnoreList:
        if @FParent.OnBeforeSaveIgnoreList <> nil then
          FParent.OnBeforeSaveIgnoreList(TObject(Parameters[0]^),
            TStream(Parameters[1]^), boolean(Parameters[2]^));
      asplOnAfterSaveIgnoreList:
        if @FParent.OnAfterSaveIgnoreList <> nil then
          FParent.OnAfterSaveIgnoreList(TObject(Parameters[0]^),
            TStream(Parameters[1]^));
      asplOnBeforeLoadIgnoreList:
        if @FParent.OnBeforeLoadIgnoreList <> nil then
          FParent.OnBeforeLoadIgnoreList(TObject(Parameters[0]^),
            TStream(Parameters[1]^), boolean(Parameters[2]^));
      asplOnAfterLoadIgnoreList:
        if @FParent.OnAfterLoadIgnoreList <> nil then
          FParent.OnAfterLoadIgnoreList(TObject(Parameters[0]^),
            TStream(Parameters[1]^));
      asplOnBeforeSaveDB:
        if @FParent.OnBeforeSaveDB <> nil then
          FParent.OnBeforeSaveDB(TObject(Parameters[0]^),
            TStream(Parameters[1]^), boolean(Parameters[2]^));
      asplOnAfterSaveDB:
        if @FParent.OnAfterSaveDB <> nil then
          FParent.OnAfterSaveDB(TObject(Parameters[0]^),
            TStream(Parameters[1]^));
      asplOnBeforeLoadDB:
        if @FParent.OnBeforeLoadDB <> nil then
          FParent.OnBeforeLoadDB(TObject(Parameters[0]^),
            TStream(Parameters[1]^), boolean(Parameters[2]^));
      asplOnAfterLoadDB:
        if @FParent.OnAfterLoadDB <> nil then
          FParent.OnAfterLoadDB(TObject(Parameters[0]^),
            TStream(Parameters[1]^));
      asplOnBeforeAppendWordsToDictionary:
        if @FParent.OnBeforeAppendWordsToDictionary <> nil then
          FParent.OnBeforeAppendWordsToDictionary(TObject(Parameters[0]^),
            TSpellCheckLanguagePack(Parameters[1]^), string(Parameters[2]^),
            boolean(Parameters[3]^));
      asplOnAfterAppendWordsToDictionary:
        if @FParent.OnBeforeAppendWordsToDictionary <> nil then
          FParent.OnBeforeAppendWordsToDictionary(TObject(Parameters[0]^),
            TSpellCheckLanguagePack(Parameters[1]^), string(Parameters[2]^),
            boolean(Parameters[3]^));
      asplOnBeforeRefreshDictionary:
        if @FParent.OnBeforeRefreshDictionary <> nil then
          FParent.OnBeforeRefreshDictionary(TObject(Parameters[0]^),
            boolean(Parameters[1]^));
      asplOnAfterRefreshDictionary:
        if @FParent.OnAfterRefreshDictionary <> nil then
          FParent.OnAfterRefreshDictionary(TObject(Parameters[0]^));
      asplOnBeforeOpen:
        if @FParent.OnBeforeOpen <> nil then
          FParent.OnBeforeOpen(TObject(Parameters[0]^),
            boolean(Parameters[1]^));
      asplOnAfterOpen:
        if @FParent.OnAfterOpen <> nil then
          FParent.OnAfterOpen(TObject(Parameters[0]^));
      asplOnBeforeClose:
        if @FParent.OnBeforeClose <> nil then
          FParent.OnBeforeClose(TObject(Parameters[0]^),
            boolean(Parameters[2]^));
      asplOnAfterClose:
        if @FParent.OnAfterClose <> nil then
          FParent.OnAfterClose(TObject(Parameters[0]^));
      asplOnBeforeSave:
        if @FParent.OnBeforeSave <> nil then
          FParent.OnBeforeSave(TObject(Parameters[0]^), TStream(Parameters[1]^),
            boolean(Parameters[2]^));
      asplOnAfterLoad:
        if @FParent.OnAfterLoad <> nil then
          FParent.OnAfterLoad(TObject(Parameters[0]^), TStream(Parameters[1]^));
      asplOnBeforeLoad:
        if @FParent.OnBeforeLoad <> nil then
          FParent.OnBeforeLoad(TObject(Parameters[0]^), TStream(Parameters[1]^),
            boolean(Parameters[2]^));
      asplOnAfterSave:
        if @FParent.OnAfterSave <> nil then
          FParent.OnAfterSave(TObject(Parameters[0]^), TStream(Parameters[1]^));
      asplOnBeforeCleanupDictionary:
        if @FParent.OnBeforeCleanupDictionary <> nil then
          FParent.OnBeforeCleanupDictionary(TObject(Parameters[0]^),
            boolean(Parameters[1]^));
      asplOnAfterCleanupDictionary:
        if @FParent.OnAfterCleanupDictionary <> nil then
          FParent.OnAfterCleanupDictionary(TObject(Parameters[0]^));
      asplOnBeforeGetSuggestions:
        if @FParent.OnBeforeGetSuggestions <> nil then
          FParent.OnBeforeGetSuggestions(TObject(Parameters[0]^),
            TAdvSuggestionContext(Parameters[1]^), boolean(Parameters[2]^));
      asplOnAfterGetSuggestions:
        if @FParent.OnAfterGetSuggestions <> nil then
          FParent.OnAfterGetSuggestions(TObject(Parameters[0]^),
            TAdvSuggestionContext(Parameters[1]^));
      asplOnAfterGetValidation:
        if @FParent.OnAfterGetValidation <> nil then
          FParent.OnAfterGetValidation(TObject(Parameters[0]^),
            TAdvValidationContext(Parameters[1]^));
      asplOnBeforeGetValidation:
        if @FParent.OnBeforeGetValidation <> nil then
          FParent.OnBeforeGetValidation(TObject(Parameters[0]^),
            TAdvValidationContext(Parameters[1]^), boolean(Parameters[2]^));
    end;
  finally
  end;
end;

// ------------------------------------------------------------------------------

{ TProcessRequestContext }
function TProcessRequestContext.GetCount: integer;
begin
  Exit(FList.Count);
end;

procedure TProcessRequestContext.Add(Value: TAdvSpellcheckRequestGroup);
begin
  FList.Add(Value);
end;

function TProcessRequestContext.GetItems(Index: integer)
  : TAdvSpellcheckRequestGroup;
begin
  Exit(FList[Index]);
end;

constructor TProcessRequestContext.Create;
begin
  Inherited;
  FList := TList<TAdvSpellcheckRequestGroup>.Create;
end;

destructor TProcessRequestContext.Destroy;
begin
  FreeAndNil(FList);
  Inherited;
end;

{$IFNDEF FMXLIB}
{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}
{$ENDIF}


end.
